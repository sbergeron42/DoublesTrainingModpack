use std::cmp::Ordering;

use core::sync::atomic::{AtomicI32, AtomicU8, AtomicUsize, AtomicU32, Ordering as AtomicOrdering};

use skyline::nn::ui2d::ResColor;
use smash::app::{lua_bind::*, utility, BattleObjectModuleAccessor};
use smash::lib::lua_const::*;

use InputRecordState::*;
use PossessionState::*;

use crate::common::consts::{FighterId, HitstunPlayback, OnOff, RecordTrigger};
use crate::common::input::*;
use crate::common::offsets::OFFSET_SET_CPU_CONTROLS;
use crate::common::{button_config, is_training_mode};
use crate::common::{is_in_hitstun, is_in_shieldstun, try_get_module_accessor, MENU};
use crate::training::mash;
use crate::training::ui::notifications::{clear_notification, color_notification};
use crate::{error, warn};

use training_mod_sync::*;

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum InputRecordState {
    None = 0,
    Pause = 1,
    Record = 2,
    Playback = 3,
}

impl InputRecordState {
    fn from_u8(v: u8) -> Self {
        match v {
            1 => Pause,
            2 => Record,
            3 => Playback,
            _ => None,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum PossessionState {
    Player = 0,
    Cpu = 1,
    Lockout = 2,
    Standby = 3,
}

impl PossessionState {
    fn from_u8(v: u8) -> Self {
        match v {
            1 => Cpu,
            2 => Lockout,
            3 => Standby,
            _ => Player,
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub enum StartingStatus {
    Aerial, // FIGHTER_STATUS_KIND_ATTACK_AIR TODO: This shouldn't happen without starting input recording in the air - when would we want this?
    // Probably should lock input recordings to either the ground or the air
    Airdodge, // FIGHTER_STATUS_KIND_ESCAPE_AIR, FIGHTER_STATUS_KIND_ESCAPE_AIR_SLIDE
    // Other statuses cannot be used to hitstun cancel via damage_fly_attack_frame/damage_fly_escape_frame
    // Some statuses can leave shield earlier though, so we should check for this
    SpecialHi,  // Up B: FIGHTER_STATUS_KIND_SPECIAL_HI,
    Jump,       // FIGHTER_STATUS_KIND_JUMP_SQUAT
    DoubleJump, //FIGHTER_STATUS_KIND_JUMP_AERIAL
    Spotdodge,  // FIGHTER_STATUS_KIND_ESCAPE
    Roll,       // FIGHTER_STATUS_KIND_ESCAPE_F, FIGHTER_STATUS_KIND_ESCAPE_B
    Grab,       // FIGHTER_STATUS_KIND_CATCH
    Other,
}

pub const STICK_NEUTRAL: f32 = 0.2;
pub const STICK_CLAMP_MULTIPLIER: f32 = 1.0 / 120.0; // 120.0 = CLAMP_MAX
const FINAL_RECORD_MAX: usize = 600; // Maximum length for input recording sequences (capacity)
const TOTAL_SLOT_COUNT: usize = 5; // Total number of input recording slots
static INPUT_RECORD: AtomicU8 = AtomicU8::new(InputRecordState::None as u8);
static INPUT_RECORD_FRAME: AtomicUsize = AtomicUsize::new(0);
static POSSESSION: AtomicU8 = AtomicU8::new(PossessionState::Player as u8);
static LOCKOUT_FRAME: AtomicUsize = AtomicUsize::new(0);
static BUFFER_FRAME: AtomicUsize = AtomicUsize::new(0);
static RECORDED_LR: AtomicU32 = AtomicU32::new(0x3f800000); // 1.0f32.to_bits() — direction CPU faced during recording
static CURRENT_LR: AtomicU32 = AtomicU32::new(0x3f800000); // 1.0f32.to_bits() — direction CPU faces at playback start
static STARTING_STATUS: AtomicI32 = AtomicI32::new(0); // First non-wait status in recording (for hitstun cancel)
static CURRENT_RECORD_SLOT: AtomicUsize = AtomicUsize::new(0);
static CURRENT_PLAYBACK_SLOT: AtomicUsize = AtomicUsize::new(0);
static CURRENT_FRAME_LENGTH: AtomicUsize = AtomicUsize::new(60);
// Large arrays — keep as RwLock
pub static P1_FINAL_MAPPING: RwLock<[[MappedInputs; FINAL_RECORD_MAX]; TOTAL_SLOT_COUNT]> =
    RwLock::new([[{ MappedInputs::empty() }; FINAL_RECORD_MAX]; TOTAL_SLOT_COUNT]);
pub static P1_FRAME_LENGTH_MAPPING: RwLock<[usize; TOTAL_SLOT_COUNT]> =
    RwLock::new([60; TOTAL_SLOT_COUNT]);

// Helper functions for atomic enum/float access
fn load_input_record() -> InputRecordState {
    InputRecordState::from_u8(INPUT_RECORD.load(AtomicOrdering::Relaxed))
}
fn store_input_record(v: InputRecordState) {
    INPUT_RECORD.store(v as u8, AtomicOrdering::Relaxed);
}
fn load_possession() -> PossessionState {
    PossessionState::from_u8(POSSESSION.load(AtomicOrdering::Relaxed))
}
fn store_possession(v: PossessionState) {
    POSSESSION.store(v as u8, AtomicOrdering::Relaxed);
}
fn load_recorded_lr() -> f32 {
    f32::from_bits(RECORDED_LR.load(AtomicOrdering::Relaxed))
}
fn store_recorded_lr(v: f32) {
    RECORDED_LR.store(v.to_bits(), AtomicOrdering::Relaxed);
}
fn load_current_lr() -> f32 {
    f32::from_bits(CURRENT_LR.load(AtomicOrdering::Relaxed))
}
fn store_current_lr(v: f32) {
    CURRENT_LR.store(v.to_bits(), AtomicOrdering::Relaxed);
}
// pub static P1_STARTING_STATUSES: RwLock<[StartingStatus; TOTAL_SLOT_COUNT]> = RwLock::new([StartingStatus::Other; TOTAL_SLOT_COUNT]); // TODO! Not used currently

unsafe fn can_transition(module_accessor: *mut BattleObjectModuleAccessor) -> bool {
    let transition_term = into_transition_term(into_starting_status(STARTING_STATUS.load(AtomicOrdering::Relaxed)));
    WorkModule::is_enable_transition_term(module_accessor, transition_term)
}

unsafe fn should_mash_playback() {
    // Don't want to interrupt recording
    if is_recording() {
        return;
    }
    if !mash::is_playback_queued() {
        return;
    }
    // playback is queued, so we might want to begin this frame
    // if we're currently playing back, we don't want to interrupt (this may change for layered multislot playback, but for now this is fine)
    if is_playback() {
        return;
    }
    let mut should_playback = false;
    let cpu_module_accessor = try_get_module_accessor(FighterId::CPU)
        .expect("Could not get CPU module accessor in should_mash_playback");
    // depending on our current status, we want to wait for different timings to begin playback

    // TODO: This isn't the best way to write this I'm sure, want to rewrite

    if is_in_hitstun(&mut *cpu_module_accessor) {
        // if we're in hitstun and want to enter the frame we start hitstop for SDI, start if we're in any damage status instantly
        if read(&MENU).hitstun_playback == HitstunPlayback::INSTANT {
            should_playback = true;
        }
        // if we want to wait until we exit hitstop and begin flying away for shield art etc, start if we're not in hitstop
        if read(&MENU).hitstun_playback == HitstunPlayback::HITSTOP
            && !StopModule::is_stop(cpu_module_accessor)
        {
            should_playback = true;
        }
        // if we're in hitstun and want to wait till FAF to act, then we want to match our starting status to the correct transition term to see if we can hitstun cancel
        if read(&MENU).hitstun_playback == HitstunPlayback::HITSTUN
            && can_transition(cpu_module_accessor)
        {
            should_playback = true;
        }
    } else if is_in_shieldstun(&mut *cpu_module_accessor) {
        // TODO: Add instant shieldstun toggle for shield art out of electric hitstun? Idk that's so specific
        if can_transition(cpu_module_accessor) {
            should_playback = true;
        }
    } else if can_transition(cpu_module_accessor) {
        should_playback = true;
    }

    // how do we deal with buffering motion inputs out of shield? You can't complete them in one frame, but they can definitely be buffered during shield drop
    // probably need a separate standby setting for grounded, aerial, shield, where shield starts once you let go of shield, and aerial keeps you in the air?

    if should_playback {
        playback(Some(mash::queued_playback_slot()));
    }
}

// TODO: set up a better match later and make this into one func

fn into_starting_status(status: i32) -> StartingStatus {
    if status == *FIGHTER_STATUS_KIND_ATTACK_AIR {
        return StartingStatus::Aerial;
    } else if (*FIGHTER_STATUS_KIND_ESCAPE_AIR..*FIGHTER_STATUS_KIND_ESCAPE_AIR_SLIDE)
        .contains(&status)
    {
        return StartingStatus::Airdodge;
    } else if status == *FIGHTER_STATUS_KIND_SPECIAL_HI {
        return StartingStatus::SpecialHi;
    } else if status == *FIGHTER_STATUS_KIND_JUMP_SQUAT {
        return StartingStatus::Jump;
    } else if status == *FIGHTER_STATUS_KIND_JUMP_AERIAL {
        return StartingStatus::DoubleJump;
    } else if status == *FIGHTER_STATUS_KIND_ESCAPE {
        return StartingStatus::Spotdodge;
    } else if (*FIGHTER_STATUS_KIND_ESCAPE_F..*FIGHTER_STATUS_KIND_ESCAPE_B).contains(&status) {
        return StartingStatus::Roll;
    } else if status == *FIGHTER_STATUS_KIND_CATCH {
        return StartingStatus::Grab;
    }
    StartingStatus::Other
}

fn into_transition_term(starting_status: StartingStatus) -> i32 {
    match starting_status {
        StartingStatus::Aerial => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_ATTACK_AIR,
        StartingStatus::Airdodge => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_ESCAPE_AIR,
        StartingStatus::Other => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_SPECIAL_S, // placeholder - most likely don't want to use this in final build, and have a different set of checks
        StartingStatus::SpecialHi => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_SPECIAL_HI,
        StartingStatus::Jump => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_JUMP_SQUAT,
        StartingStatus::DoubleJump => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_JUMP_AERIAL,
        StartingStatus::Spotdodge => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_ESCAPE,
        StartingStatus::Roll => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_ESCAPE_F,
        StartingStatus::Grab => *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_CATCH,
    }
}

pub unsafe fn handle_recording() {
    if let Some(player_module_accessor) = try_get_module_accessor(FighterId::Player) {
        if let Some(cpu_module_accessor) = try_get_module_accessor(FighterId::CPU) {
            handle_recording_for_fighter(&mut *player_module_accessor);
            handle_recording_for_fighter(&mut *cpu_module_accessor);
        }
    }
}

unsafe fn handle_recording_for_fighter(module_accessor: &mut BattleObjectModuleAccessor) {
    // Allow this because sometimes we want to make sure our NNSDK doesn't have
    // an erroneous definition
    #[allow(clippy::unnecessary_cast)]
    let entry_id_int =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID) as i32;
    let fighter_kind = utility::get_kind(module_accessor);
    let fighter_is_nana = fighter_kind == *FIGHTER_KIND_NANA;

    CURRENT_RECORD_SLOT.store(
        read(&MENU).recording_slot.into_idx().unwrap_or(0),
        AtomicOrdering::Relaxed,
    );

    if entry_id_int == 0 && !fighter_is_nana {
        if button_config::combo_passes(button_config::ButtonCombo::InputPlayback) {
            playback(read(&MENU).playback_button_slots.get_random().into_idx());
        } else if read(&MENU).record_trigger.contains(&RecordTrigger::COMMAND)
            && button_config::combo_passes(button_config::ButtonCombo::InputRecord)
        {
            lockout_record();
        }
        let input_record = load_input_record();
        if input_record == None {
            clear_notification("Input Recording");
        }
        // Handle recording end
        let mut irf = INPUT_RECORD_FRAME.load(AtomicOrdering::Relaxed);
        if (input_record == Record || input_record == Playback)
            && irf >= CURRENT_FRAME_LENGTH.load(AtomicOrdering::Relaxed) - 1
        {
            store_possession(Player);
            if mash::is_playback_queued() {
                mash::reset();
            }

            // If we need to crop the recording for neutral input
            // INPUT_RECORD_FRAME must be > 0 to prevent bounding errors
            if input_record == Record
                && read(&MENU).recording_crop == OnOff::ON
                && irf > 0
            {
                while irf > 0 && is_input_neutral(irf - 1) {
                    // Discard frames at the end of the recording until the last frame with input
                    irf -= 1;
                }
                CURRENT_FRAME_LENGTH.store(irf, AtomicOrdering::Relaxed);
                let mut p1_frame_length_mapping = lock_write(&P1_FRAME_LENGTH_MAPPING);
                (*p1_frame_length_mapping)[CURRENT_RECORD_SLOT.load(AtomicOrdering::Relaxed)] = irf;
                drop(p1_frame_length_mapping);
            }

            irf = 0;

            if read(&MENU).playback_loop == OnOff::ON && input_record == Playback {
                let playback_slot = CURRENT_PLAYBACK_SLOT.load(AtomicOrdering::Relaxed);
                playback(Some(playback_slot));
            } else {
                store_input_record(None);
            }
        }
        INPUT_RECORD_FRAME.store(irf, AtomicOrdering::Relaxed);
    }

    // Handle Possession Coloring
    let possession = load_possession();
    if entry_id_int == 1 && possession == Lockout {
        clear_notification("Input Recording");
        color_notification(
            "Input Recording".to_string(),
            "Lockout".to_owned(),
            60,
            ResColor {
                r: 8,
                g: 8,
                b: 200,
                a: 255,
            },
        );
        set_color_rgb_2(
            module_accessor,
            0.0,
            0.0,
            1.0,
            *MODEL_COLOR_TYPE_COLOR_BLEND,
        );
    } else if entry_id_int == 1 && possession == Standby {
        clear_notification("Input Recording");
        color_notification(
            "Input Recording".to_string(),
            "Standby".to_owned(),
            60,
            ResColor {
                r: 200,
                g: 8,
                b: 200,
                a: 255,
            },
        );
        set_color_rgb_2(
            module_accessor,
            1.0,
            0.0,
            1.0,
            *MODEL_COLOR_TYPE_COLOR_BLEND,
        );
    } else if entry_id_int == 1 && possession == Cpu {
        clear_notification("Input Recording");
        color_notification(
            "Input Recording".to_string(),
            "Recording".to_owned(),
            60,
            ResColor {
                r: 200,
                g: 8,
                b: 8,
                a: 255,
            },
        );
        set_color_rgb_2(
            module_accessor,
            1.0,
            0.0,
            0.0,
            *MODEL_COLOR_TYPE_COLOR_BLEND,
        );
    } else if entry_id_int == 1 && possession == Player && load_input_record() == Playback {
        // Need to re-read INPUT_RECORD instead of using the local variable because we might have assigned to it early
        // Displays if the inputs from the current frame were a result of playback
        let input_record_frame = INPUT_RECORD_FRAME.load(AtomicOrdering::Relaxed);
        if input_record_frame == 0 || input_record_frame == 1 {
            // can be either, seems like a thread issue
            clear_notification("Input Recording");
            color_notification(
                "Input Recording".to_string(),
                "Playback".to_owned(),
                CURRENT_FRAME_LENGTH.load(AtomicOrdering::Relaxed) as u32,
                ResColor {
                    r: 0,
                    g: 0,
                    b: 0,
                    a: 255,
                },
            );
        }
    }
}

pub unsafe fn lockout_record() {
    let cpu_module_accessor = try_get_module_accessor(FighterId::CPU)
        .expect("Could not get CPU module accessor in lockout_record");
    let recording_duration = read(&MENU).recording_duration.into_frames();
    let current_record_slot = CURRENT_RECORD_SLOT.load(AtomicOrdering::Relaxed);
    let mut p1_final_mapping = lock_write(&P1_FINAL_MAPPING);
    (*p1_final_mapping)[current_record_slot] = [{ MappedInputs::empty() }; FINAL_RECORD_MAX];
    drop(p1_final_mapping);
    let mut p1_frame_length_mapping = lock_write(&P1_FRAME_LENGTH_MAPPING);
    (*p1_frame_length_mapping)[current_record_slot] = recording_duration;
    drop(p1_frame_length_mapping);
    CURRENT_FRAME_LENGTH.store(recording_duration, AtomicOrdering::Relaxed);
    store_input_record(Pause);
    INPUT_RECORD_FRAME.store(0, AtomicOrdering::Relaxed);
    store_possession(Lockout);
    LOCKOUT_FRAME.store(30, AtomicOrdering::Relaxed); // This needs to be this high or issues occur dropping shield - but does this cause problems when trying to record ledge?
    BUFFER_FRAME.store(0, AtomicOrdering::Relaxed);
    // Store the direction the CPU is facing when we initially record, so we can turn their inputs around if needed
    let lr = PostureModule::lr(cpu_module_accessor);
    store_recorded_lr(lr);
    store_current_lr(lr);
}

// Returns whether we did playback
pub unsafe fn playback(slot: Option<usize>) -> bool {
    if load_input_record() == Pause {
        warn!("Tried to playback during lockout!");
        return false;
    }
    if let Some(slot) = slot {
        let cpu_module_accessor = try_get_module_accessor(FighterId::CPU)
            .expect("Could not get CPU module accessor in playback");
        let frame_length = lock_read(&P1_FRAME_LENGTH_MAPPING)[slot];
        CURRENT_FRAME_LENGTH.store(frame_length, AtomicOrdering::Relaxed);
        CURRENT_PLAYBACK_SLOT.store(slot, AtomicOrdering::Relaxed);
        store_input_record(Playback);
        store_possession(Player);
        INPUT_RECORD_FRAME.store(0, AtomicOrdering::Relaxed);
        BUFFER_FRAME.store(0, AtomicOrdering::Relaxed);
        store_current_lr(PostureModule::lr(cpu_module_accessor));
        true
    } else {
        warn!("Tried to playback without a slot selected!");
        false
    }
}

pub unsafe fn playback_ledge(slot: Option<usize>) {
    let did_playback = playback(slot);
    if did_playback {
        let mut bf = 5usize; // So we can make sure the option is buffered and won't get ledge trumped if delay is 0
                              // drop down from ledge can't be buffered on the same frame as jump/attack/roll/ngu so we have to do this
                              // Need to buffer 1 less frame for non-lassos
        let cpu_module_accessor = try_get_module_accessor(FighterId::CPU)
            .expect("Could not get CPU module accessor in playback_ledge");
        let status_kind = StatusModule::status_kind(cpu_module_accessor);
        if status_kind == *FIGHTER_STATUS_KIND_CLIFF_CATCH {
            bf -= 1;
        }
        BUFFER_FRAME.store(bf, AtomicOrdering::Relaxed);
    }
}

pub unsafe fn stop_playback() {
    store_input_record(None);
    INPUT_RECORD_FRAME.store(0, AtomicOrdering::Relaxed);
    store_possession(Player);
}

pub unsafe fn is_input_neutral(input_frame: usize) -> bool {
    // Returns whether we should be done with standby this frame (if any significant controller input has been made)
    let current_record_slot = CURRENT_RECORD_SLOT.load(AtomicOrdering::Relaxed);
    let frame_input = lock_read(&P1_FINAL_MAPPING)[current_record_slot][input_frame];

    let clamped_lstick_x =
        ((frame_input.lstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    let clamped_lstick_y =
        ((frame_input.lstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    let clamped_rstick_x =
        ((frame_input.rstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    let clamped_rstick_y =
        ((frame_input.rstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);

    // No buttons pressed or just flick jump-- if they really did a stick jump, we'd have lstick movement as well
    let buttons_pressed =
        !(frame_input.buttons.is_empty() || frame_input.buttons == Buttons::FLICK_JUMP);
    let lstick_movement =
        clamped_lstick_x.abs() >= STICK_NEUTRAL || clamped_lstick_y.abs() >= STICK_NEUTRAL;
    let rstick_movement =
        clamped_rstick_x.abs() >= STICK_NEUTRAL || clamped_rstick_y.abs() >= STICK_NEUTRAL;
    !(lstick_movement || rstick_movement || buttons_pressed)
}

pub unsafe fn handle_final_input_mapping(player_idx: i32, out: *mut MappedInputs) {
    let possession = load_possession();
    if player_idx == 0 {
        // if player 1
        if load_input_record() == Record {
            let mut irf = INPUT_RECORD_FRAME.load(AtomicOrdering::Relaxed);
            // check for standby before starting action:
            if possession == Standby && !is_input_neutral(0) {
                // last input made us start an action, so start recording and end standby.
                irf += 1;
                INPUT_RECORD_FRAME.store(irf, AtomicOrdering::Relaxed);
                store_possession(Cpu);
            }

            if irf == 1 {
                // We're on the second frame of recording, grabbing the status should give us the status that resulted from the first frame of input
                // We'll want to save this status so that we use the correct TRANSITION TERM for hitstun cancelling out of damage fly
                let cpu_module_accessor = try_get_module_accessor(FighterId::CPU)
                    .expect("Could not get CPU module accessor in handle_final_input_mapping");
                STARTING_STATUS.store(
                    StatusModule::status_kind(cpu_module_accessor),
                    AtomicOrdering::Relaxed,
                );
            }
            let mut p1_final_mapping = lock_write(&P1_FINAL_MAPPING);
            let current_record_slot = CURRENT_RECORD_SLOT.load(AtomicOrdering::Relaxed);
            (*p1_final_mapping)[current_record_slot][irf] = *out;
            drop(p1_final_mapping);
            *out = MappedInputs::empty(); // don't control player while recording
        }
        // Don't allow for player input during Lockout
        if possession == Lockout {
            *out = MappedInputs::empty();
        }
    }
}

#[skyline::hook(offset = *OFFSET_SET_CPU_CONTROLS)] // After cpu controls are assigned from ai calls
unsafe fn set_cpu_controls(p_data: *mut *mut u8) {
    use crate::training::doubles;

    let controller_data = *p_data.add(1) as *mut ControlModuleInternal;

    call_original!(p_data);

    if !is_training_mode() {
        return;
    }

    // For human entries in team mode: override AI output with saved FIM-produced input.
    // inject_human_input uses the per-frame call counter to identify the entry.
    if doubles::is_team_mode() && doubles::inject_human_input(controller_data) {
        // Human entry — controller input injected. Skip input_record.
        return;
    }

    // Check if we need to begin playback this frame due to a mash toggle
    // TODO: Setup STARTING_STATUS based on current playback slot here

    // This check prevents out of shield if mash exiting is on
    if load_input_record() == None {
        should_mash_playback();
    }

    let Some(cpu_module_accessor) = try_get_module_accessor(FighterId::CPU) else {
        // Sometimes we can try to grab their module accessor before they are valid?
        return;
    };

    if load_input_record() == Pause {
        let lockout_frame = LOCKOUT_FRAME.load(AtomicOrdering::Relaxed);
        match lockout_frame.cmp(&0) {
            Ordering::Greater => LOCKOUT_FRAME.store(lockout_frame - 1, AtomicOrdering::Relaxed),
            Ordering::Equal => {
                store_input_record(Record);
                store_possession(Standby);
            }
            Ordering::Less => error!("LOCKOUT_FRAME OUT OF BOUNDS"),
        }
    }

    let input_record = load_input_record();
    if input_record == Record || input_record == Playback {
        // if we aren't facing the way we were when we initially recorded, we reverse horizontal inputs
        let mut x_input_multiplier = load_recorded_lr() * load_current_lr();
        // Don't flip Shulk's dial inputs
        let fighter_kind = utility::get_kind(&mut *cpu_module_accessor);
        if fighter_kind == *FIGHTER_KIND_SHULK {
            let circle_menu_flag = WorkModule::is_flag(
                &mut *cpu_module_accessor,
                *FIGHTER_SHULK_INSTANCE_WORK_ID_FLAG_SPECIAL_N_CIRCLE_MENU,
            );
            if circle_menu_flag {
                // While in dial, don't flip horizontal inputs
                x_input_multiplier = 1.0;
            }
            // If we have issues with the frame after the dial comes out, change condition to
            //  circle_menu_flag && FIGHTER_SHULK_INSTANCE_WORK_ID_INT_SPECIAL_N_DECIDE_INTERVAL_FRAME > 1
        }
        // Prevent us from falling off of the ledge in standby
        if StatusModule::status_kind(cpu_module_accessor) == *FIGHTER_STATUS_KIND_CLIFF_WAIT
            && is_standby()
            && WorkModule::get_int(
                cpu_module_accessor,
                *FIGHTER_STATUS_CLIFF_WORK_INT_CATCH_REST_TIME,
            ) < 50
        {
            WorkModule::set_int(
                cpu_module_accessor,
                200,
                *FIGHTER_STATUS_CLIFF_WORK_INT_CATCH_REST_TIME,
            );
        }

        let mut irf = INPUT_RECORD_FRAME.load(AtomicOrdering::Relaxed);
        let slot = if input_record == Record {
            CURRENT_RECORD_SLOT.load(AtomicOrdering::Relaxed)
        } else {
            CURRENT_PLAYBACK_SLOT.load(AtomicOrdering::Relaxed)
        };
        let p1_final_mapping = lock_read(&P1_FINAL_MAPPING);
        let mut saved_mapped_inputs = p1_final_mapping[slot][irf];
        drop(p1_final_mapping);
        let mut bf = BUFFER_FRAME.load(AtomicOrdering::Relaxed);
        if (0 < bf) && (bf <= 3) {
            // Our option is already buffered, now we need to 0 out inputs to make sure our future controls act like flicks/presses instead of holding the button
            saved_mapped_inputs = MappedInputs::empty();
        }

        (*controller_data).buttons = saved_mapped_inputs.buttons;
        (*controller_data).stick_x =
            x_input_multiplier * ((saved_mapped_inputs.lstick_x as f32) / (i8::MAX as f32));
        (*controller_data).stick_y = (saved_mapped_inputs.lstick_y as f32) / (i8::MAX as f32);
        // Clamp stick inputs for separate part of structure
        let mut clamped_lstick_x = x_input_multiplier
            * ((saved_mapped_inputs.lstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
        let mut clamped_lstick_y =
            ((saved_mapped_inputs.lstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
        clamped_lstick_x = if clamped_lstick_x.abs() >= STICK_NEUTRAL {
            clamped_lstick_x
        } else {
            0.0
        };
        clamped_lstick_y = if clamped_lstick_y.abs() >= STICK_NEUTRAL {
            clamped_lstick_y
        } else {
            0.0
        };
        (*controller_data).clamped_lstick_x = clamped_lstick_x;
        (*controller_data).clamped_lstick_y = clamped_lstick_y;

        // Keep counting frames, unless we're in standby waiting for an input, or are buffering an option
        // When buffering an option, we keep inputting the first frame of input during the buffer window
        if bf > 0 {
            bf -= 1;
        } else if irf < CURRENT_FRAME_LENGTH.load(AtomicOrdering::Relaxed) - 1
            && load_possession() != Standby
        {
            irf += 1;
        }
        BUFFER_FRAME.store(bf, AtomicOrdering::Relaxed);
        INPUT_RECORD_FRAME.store(irf, AtomicOrdering::Relaxed);
    }
}

pub fn is_playback() -> bool {
    let input_record = load_input_record();
    input_record == Record || input_record == Playback
}

pub fn is_recording() -> bool {
    load_input_record() == Record
}

pub unsafe fn is_standby() -> bool {
    let possession = load_possession();
    possession == Standby || possession == Lockout
}

extern "C" {
    // TODO: we should be using this from skyline
    #[link_name = "\u{1}_ZN3app8lua_bind31ModelModule__set_color_rgb_implEPNS_26BattleObjectModuleAccessorEfffNS_16MODEL_COLOR_TYPEE"]
    pub fn set_color_rgb_2(
        arg1: *mut BattleObjectModuleAccessor,
        arg2: f32,
        arg3: f32,
        arg4: f32,
        arg5: i32,
    );
}

pub fn init() {
    skyline::install_hooks!(set_cpu_controls);
}
