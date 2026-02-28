/// Doubles training support: allow CPU fighters to hit each other, and apply
/// native Smash control-scheme profiles to CPU slots controlled by humans.
///
/// # CPU↔CPU hitbox interaction
/// Each CPU gets its own unique hit-team equal to its entry ID so the game's
/// team-attack filter allows CPU→CPU hits.
///
/// # CPU control-scheme profiles — current status (Feb 2026)
///
/// ## What we know from FIM diagnostic logging
/// The Final-Input-Mapping (FIM) hook at OFFSET_FIM is called **only** for
/// Player 1's physical controller (npad=0).  Two player_idx values appear:
///   player_idx=0  npad=0  → P1's main game-action processing (every frame)
///   player_idx=1  npad=0  → same physical controller, secondary purpose
///
/// The second physical controller (npad=1, used by a human to control the CPU
/// fighter in "CPU Behavior = Control" mode) NEVER appears in the FIM hook.
/// Its inputs go through a completely separate pipeline — most likely the
/// set_cpu_controls hook (OFFSET_SET_CPU_CONTROLS).
///
/// ## Consequence
/// apply_cpu_control_scheme is NOT hooked into FIM any more; modifying the
/// mapping at player_idx=1 through FIM incorrectly changed P1's controls.
/// The correct hook point for CPU remapping is set_cpu_controls.  Use the
/// log_set_cpu_controls_call() diagnostic to understand what is available
/// there before implementing remapping.
///
/// ## Profile memory layout (per entry, relative to entry base)
/// * `+0x08`  u32     tag_len  — 0 means empty slot
/// * `+0x0C`  u16[]   tag      — UTF-16 profile name, length = tag_len
/// * `+0x24`  0x50B   ControllerMapping (bindings, tap-jump, sensitivity)
/// Pointer chain: .text + 0x5313510 → [0] → [0x58]->[*] → array[entry * 0xf7d8]
///
/// ## Profile ordering
/// Stored in creation order (entry 0 = oldest), NOT most-recently-used order
/// as shown in Options → Controls.  Profile N in the TUI = memory entry N-1.
///
/// ## Debug logging
/// Two outputs:
/// 1. On-screen toast (fires once per unique diagnostic event).
/// 2. Append-only file: `sd:/ultimate/TrainingModpack/doubles_debug.log`
use core::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, AtomicUsize, Ordering};

use skyline::hooks::{getRegionAddress, Region};
use smash::app::{self, lua_bind::*, BattleObjectModuleAccessor};
use smash::lib::lua_const::*;
use training_mod_sync::*;

use crate::common::input::{
    ButtonBitfield, Buttons, ControllerMapping, ControllerStyle, InputKind, MappedInputs,
};
use crate::common::{FIGHTER_MANAGER_ADDR, MENU, TRAINING_MENU_ADDR};
use crate::training::ui::notifications::notification;

// ---------------------------------------------------------------------------
// SD card debug log
// ---------------------------------------------------------------------------

const DOUBLES_DEBUG_LOG: &str = "sd:/ultimate/TrainingModpack/doubles_debug.log";

/// Appends `msg` with a timestamp to the persistent debug file on the SD card.
/// Errors are silently ignored so this is safe to call from any hook context.
fn debug_log(msg: &str) {
    use std::io::Write;
    // Monotonic frame counter as lightweight timestamp (no std::time on this platform).
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let tick = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(DOUBLES_DEBUG_LOG)
    {
        let _ = writeln!(f, "[{:06}] {}", tick, msg);
    }
}

// ---------------------------------------------------------------------------
// CPU↔CPU hit-team assignment
// ---------------------------------------------------------------------------

pub unsafe fn set_cpu_hit_team(module_accessor: &mut BattleObjectModuleAccessor) {
    let entry_id =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID);
    if entry_id > 0 {
        TeamModule::set_hit_team(module_accessor, entry_id);
    }
}

// ---------------------------------------------------------------------------
// Profile pointer-chain access helpers
// ---------------------------------------------------------------------------

/// Returns the base pointer for profile entry `entry` (0-indexed), or `None`
/// if any pointer in the chain is null/zero.
unsafe fn get_profile_base(entry: usize) -> Option<*mut u8> {
    let one = *((getRegionAddress(Region::Text) as *mut u8).add(0x5313510) as *const u64);
    if one == 0 {
        return None;
    }
    let two = *(one as *const u64);
    if two == 0 {
        return None;
    }
    let ptr_ptr = *((two + 0x58) as *const *const *const u64);
    if ptr_ptr.is_null() {
        return None;
    }
    let ptr = *ptr_ptr;
    if ptr.is_null() {
        return None;
    }
    let three = *ptr;
    if three == 0 {
        return None;
    }
    Some((three as *mut u8).add(entry * 0xf7d8))
}

/// Returns the UTF-16 display name for profile entry `entry`, or `None`.
unsafe fn get_profile_name(entry: usize) -> Option<String> {
    let base = get_profile_base(entry)?;
    let tag_len = *(base.add(0x8) as *const u32);
    if tag_len == 0 {
        return None;
    }
    let slice = core::slice::from_raw_parts(base.add(0xc) as *const u16, tag_len as usize);
    Some(String::from_utf16_lossy(slice))
}

/// Copies the 0x50-byte ControllerMapping at +0x24 from entry into `dest`.
/// Returns `false` if the chain is invalid or the slot is empty.
unsafe fn copy_profile_mapping(entry: usize, dest: *mut ControllerMapping) -> bool {
    let base = match get_profile_base(entry) {
        Some(b) => b,
        None => return false,
    };
    let tag_len = *(base.add(0x8) as *const u32);
    if tag_len == 0 {
        return false;
    }
    core::ptr::copy_nonoverlapping(
        base.add(0x24),
        dest as *mut u8,
        core::mem::size_of::<ControllerMapping>(),
    );
    true
}

// ---------------------------------------------------------------------------
// FIM diagnostic — log each unique (player_idx, npad_number) combination once
// ---------------------------------------------------------------------------
//
// This diagnostic confirmed that the FIM hook is only called for npad=0 (P1's
// physical controller).  npad=1 (CPU's controller in "Control" mode) never
// appears here.  Logging is kept active in case of new discoveries.

// Bitmask tracking which (player_idx, npad) combos have been logged.
// Bit index = player_idx * 8 + npad_number (assuming both <= 7).
static FIM_SEEN: AtomicU64 = AtomicU64::new(0);
static FIM_LOGGING_DONE: AtomicBool = AtomicBool::new(false);

/// Call from the FIM hook with the controller info for this call.
/// Logs each unique (player_idx, npad_number) combination once to file and
/// on-screen.  Subsequent calls for the same combo are no-ops (no spam).
pub fn log_fim_call(player_idx: i32, npad_number: u32, is_connected: bool) {
    if FIM_LOGGING_DONE.load(Ordering::Relaxed) {
        return;
    }
    let pidx = player_idx.clamp(0, 7) as u64;
    let npad = npad_number.clamp(0, 7) as u64;
    let bit = 1u64 << (pidx * 8 + npad);
    let prev = FIM_SEEN.fetch_or(bit, Ordering::Relaxed);
    if prev & bit != 0 {
        return;
    }
    let msg = format!(
        "FIM: player_idx={}  npad={}  connected={}",
        player_idx, npad_number, is_connected
    );
    debug_log(&msg);
    notification("Doubles FIM Debug".to_string(), msg, 300);
}

/// Resets diagnostic state so new combos are logged again.
#[allow(dead_code)]
pub fn reset_fim_diagnostic() {
    FIM_SEEN.store(0, Ordering::Relaxed);
    FIM_LOGGING_DONE.store(false, Ordering::Relaxed);
    debug_log("--- reset_fim_diagnostic ---");
}

// ---------------------------------------------------------------------------
// Change-detection state for profile notifications
// ---------------------------------------------------------------------------

static LAST_PROFILE: [AtomicUsize; 4] = [
    AtomicUsize::new(usize::MAX),
    AtomicUsize::new(usize::MAX),
    AtomicUsize::new(usize::MAX),
    AtomicUsize::new(usize::MAX),
];

// ---------------------------------------------------------------------------
// Vanilla CPU Behavior override
// ---------------------------------------------------------------------------

// Game value for "CPU Behavior = Control" (player controls the CPU fighter).
const CPU_BEHAVIOR_CONTROL: u32 = 8;

/// When a teammate slot is configured, write CPU Behavior = Control to the
/// vanilla pause menu every frame so P2's physical controller is routed to that
/// CPU fighter.  The user does not need to set this manually.
///
/// Offset 0xb4c inside PauseMenu = the CPU Behavior u32 field (confirmed via
/// Ghidra: FUN_7101bbacc0 case 4 writes to `lVar10 + 0xb4c`).
pub unsafe fn enforce_cpu_behavior_for_teammate() {
    let teammate = read(&MENU).teammate_slot.selected_index();
    if teammate == 0 {
        return; // No teammate configured — leave vanilla setting alone.
    }
    if TRAINING_MENU_ADDR.is_null() {
        return;
    }
    let field = (TRAINING_MENU_ADDR as *mut u8).add(0xb4c) as *mut u32;
    *field = CPU_BEHAVIOR_CONTROL;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Called every frame from Player 1's FIM hook (player_idx == 0).
/// Announces any CPU profile changes with a toast notification.
pub unsafe fn check_and_announce_profiles() {
    let menu = read(&MENU);
    let slots: [(usize, usize); 3] = [
        (1, menu.cpu1_profile.selected_index()),
        (2, menu.cpu2_profile.selected_index()),
        (3, menu.cpu3_profile.selected_index()),
    ];

    for (slot, profile_idx) in slots {
        let last = LAST_PROFILE[slot].load(Ordering::Relaxed);
        if profile_idx == last {
            continue;
        }
        LAST_PROFILE[slot].store(profile_idx, Ordering::Relaxed);

        let name = if profile_idx == 0 {
            "Default (no override)".to_string()
        } else {
            get_profile_name(profile_idx - 1)
                .unwrap_or_else(|| format!("Profile {} (empty/invalid)", profile_idx))
        };

        let log_msg = format!(
            "CPU {} Profile changed → idx={} name={}",
            slot, profile_idx, name
        );
        debug_log(&log_msg);
        notification(format!("CPU {} Profile", slot), name, 150);
    }
}

// One-shot flag: fires the first time apply_cpu_control_scheme copies a profile.
static APPLY_LOGGED: AtomicBool = AtomicBool::new(false);

// One-shot flag: fires once unconditionally from apply_tap_jump_override (diagnostic).
static TAP_JUMP_DIAG_LOGGED: AtomicBool = AtomicBool::new(false);
// One-shot flag: fires the first time apply_tap_jump_override suppresses a FLICK_JUMP.
static TAP_JUMP_LOGGED: AtomicBool = AtomicBool::new(false);

/// Apply the user-selected control-scheme profile to the ControllerMapping
/// buffer pointed to by `mappings`.
///
/// Called from the FIM hook before `original!()` for `player_idx > 0`.
/// The caller saves and restores the affected slot around the original call
/// so game state is not permanently mutated.
///
/// # Slot indexing
/// FIM accesses the mapping array as `param_1 + player_idx * sizeof(ControllerMapping)`.
/// We must copy to `mappings.add(cpu_slot)` (the player-indexed slot), NOT to
/// `mappings` (slot 0, which belongs to P1).  Copying to the wrong slot is what
/// caused C-stick Tilt to be ignored — FIM kept reading SmashAttack from slot 1.
pub unsafe fn apply_cpu_control_scheme(mappings: *mut ControllerMapping, cpu_slot: usize) {
    if cpu_slot < 1 || cpu_slot > 3 {
        return;
    }
    let profile_idx: usize = {
        let menu = read(&MENU);
        match cpu_slot {
            1 => menu.cpu1_profile.selected_index(),
            2 => menu.cpu2_profile.selected_index(),
            3 => menu.cpu3_profile.selected_index(),
            _ => return,
        }
    };
    if profile_idx == 0 {
        return;
    }
    let entry = profile_idx - 1;

    // Copy to the player-indexed slot so FIM sees the profile for this CPU.
    let dest = mappings.add(cpu_slot);

    let tapjump_before = (*dest).gc_tapjump as u8;
    let copy_ok = copy_profile_mapping(entry, dest);
    let tapjump_after = (*dest).gc_tapjump as u8;

    // Log once to confirm the function is running and the copy result.
    if !APPLY_LOGGED.swap(true, Ordering::Relaxed) {
        let msg = format!(
            "APPLY slot={} pidx={} entry={} copy={} tj_before={} tj_after={}",
            cpu_slot, profile_idx, entry, copy_ok as u8, tapjump_before, tapjump_after
        );
        debug_log(&msg);
        notification("Profile Apply".to_string(), msg, 300);
    }
}

/// Post-processes `out->buttons` after the original FIM call to enforce the
/// tap-jump setting from the user-selected profile.
///
/// Called after `original!()` in the FIM hook for `player_idx > 0`.
/// Necessary because FIM does not read `mappings->tapjump` when generating
/// the `FLICK_JUMP` flag — modifying the mapping in `apply_cpu_control_scheme`
/// has no effect on tap jump.  Clearing `FLICK_JUMP` here is the correct fix.
pub unsafe fn apply_tap_jump_override(
    out: *mut MappedInputs,
    cpu_slot: usize,
    controller_style: ControllerStyle,
) {
    if cpu_slot < 1 || cpu_slot > 3 {
        return;
    }
    let profile_idx: usize = {
        let menu = read(&MENU);
        match cpu_slot {
            1 => menu.cpu1_profile.selected_index(),
            2 => menu.cpu2_profile.selected_index(),
            3 => menu.cpu3_profile.selected_index(),
            _ => return,
        }
    };
    if profile_idx == 0 {
        return;
    }

    let style_num = controller_style as u32;
    let flickjump = (*out).buttons.contains(Buttons::FLICK_JUMP) as u8;

    let base = match get_profile_base(profile_idx - 1) {
        Some(b) => b,
        None => {
            if !TAP_JUMP_DIAG_LOGGED.swap(true, Ordering::Relaxed) {
                let msg = format!(
                    "TJ_DIAG slot={} pidx={} style={} fj={} EARLY:chain_fail",
                    cpu_slot, profile_idx, style_num, flickjump
                );
                debug_log(&msg);
                notification("TJ Diag".to_string(), msg, 300);
            }
            return;
        }
    };
    let tag_len = *(base.add(0x8) as *const u32);
    if tag_len == 0 {
        if !TAP_JUMP_DIAG_LOGGED.swap(true, Ordering::Relaxed) {
            let msg = format!(
                "TJ_DIAG slot={} pidx={} style={} fj={} EARLY:empty_slot",
                cpu_slot, profile_idx, style_num, flickjump
            );
            debug_log(&msg);
            notification("TJ Diag".to_string(), msg, 300);
        }
        return;
    }
    let mapping = &*(base.add(0x24) as *const ControllerMapping);
    let gc_tj = mapping.gc_tapjump as u8;
    let pro_tj = mapping.pro_tapjump as u8;
    let joy_tj = mapping.joy_tapjump as u8;

    // Unconditional one-shot diagnostic — logs regardless of tapjump value or button state.
    if !TAP_JUMP_DIAG_LOGGED.swap(true, Ordering::Relaxed) {
        let msg = format!(
            "TJ_DIAG slot={} pidx={} style={} fj={} gc={} pro={} joy={}",
            cpu_slot, profile_idx, style_num, flickjump, gc_tj, pro_tj, joy_tj
        );
        debug_log(&msg);
        notification("TJ Diag".to_string(), msg, 300);
    }

    let tapjump = match controller_style {
        ControllerStyle::GCController => mapping.gc_tapjump,
        ControllerStyle::DualJoycon
        | ControllerStyle::LeftJoycon
        | ControllerStyle::RightJoycon
        | ControllerStyle::Handheld => mapping.joy_tapjump,
        _ => mapping.pro_tapjump,
    };
    if !tapjump && (*out).buttons.contains(Buttons::FLICK_JUMP) {
        (*out).buttons.remove(Buttons::FLICK_JUMP);
        if !TAP_JUMP_LOGGED.swap(true, Ordering::Relaxed) {
            let msg = format!(
                "TAP_JUMP OFF: slot={} style={} profile={}",
                cpu_slot, style_num, profile_idx
            );
            debug_log(&msg);
            notification("Tap Jump".to_string(), msg, 300);
        }
    }
}

// ---------------------------------------------------------------------------
// Button remapping override
// ---------------------------------------------------------------------------
//
// The FIM function ignores the ControllerMapping pointer for button mapping,
// reading from internal state instead (same problem as tap-jump).  The fix is
// to post-process out->buttons after original!():
//   1. Read which physical buttons are pressed from ButtonBitfield.
//   2. Look up each button's InputKind in the selected profile's mapping.
//   3. Rebuild the remappable Buttons flags from the profile's InputKinds.
//   4. Replace only those flags in out->buttons; preserve everything else
//      (FLICK_JUMP, CSTICK_ON, GUARD_HOLD, ATTACK_RAW, SPECIAL_RAW, etc.).

/// Convert an InputKind enum value to its corresponding Buttons bitflag.
fn input_kind_to_buttons(kind: InputKind) -> Buttons {
    match kind {
        InputKind::Attack => Buttons::ATTACK,
        InputKind::Special => Buttons::SPECIAL,
        InputKind::Jump => Buttons::JUMP,
        InputKind::Guard => Buttons::GUARD,
        InputKind::Grab => Buttons::CATCH,
        InputKind::SmashAttack => Buttons::SMASH,
        InputKind::AppealHi => Buttons::APPEAL_HI,
        InputKind::AppealS => Buttons::APPEAL_SL,
        InputKind::AppealLw => Buttons::APPEAL_LW,
        InputKind::Unset => Buttons::empty(),
    }
}

// One-shot: fires the first time apply_button_remap_override is called for a
// slot with a profile active (confirms the function is wired in).
static BTN_REMAP_ACTIVE_LOGGED: AtomicBool = AtomicBool::new(false);
// One-shot: fires the first time the override actually changes out->buttons
// (confirms buttons are being remapped in practice).
static BTN_REMAP_CHANGE_LOGGED: AtomicBool = AtomicBool::new(false);

/// Post-processes `out->buttons` after the original FIM call to enforce the
/// button mapping from the user-selected profile.
///
/// Called after `original!()` in the FIM hook for `player_idx > 0`.
/// Necessary because FIM reads button mappings from internal state (not from
/// the `ControllerMapping` pointer passed as its first argument), just like
/// tap-jump.  The approach mirrors `apply_tap_jump_override`: read the desired
/// state from the profile, then rewrite the relevant flags.
///
/// Only InputKind-mappable flags (ATTACK, SPECIAL, JUMP, GUARD, CATCH, SMASH,
/// APPEAL_HI, APPEAL_LW, APPEAL_SL) are replaced.  All other flags —
/// FLICK_JUMP, CSTICK_ON, GUARD_HOLD, ATTACK_RAW, SPECIAL_RAW, etc. — are
/// preserved from the original FIM output.
pub unsafe fn apply_button_remap_override(
    out: *mut MappedInputs,
    cpu_slot: usize,
    controller_style: ControllerStyle,
    current_buttons: ButtonBitfield,
) {
    if cpu_slot < 1 || cpu_slot > 3 {
        return;
    }
    let profile_idx: usize = {
        let menu = read(&MENU);
        match cpu_slot {
            1 => menu.cpu1_profile.selected_index(),
            2 => menu.cpu2_profile.selected_index(),
            3 => menu.cpu3_profile.selected_index(),
            _ => return,
        }
    };
    if profile_idx == 0 {
        return;
    }

    let base = match get_profile_base(profile_idx - 1) {
        Some(b) => b,
        None => return,
    };
    let tag_len = *(base.add(0x8) as *const u32);
    if tag_len == 0 {
        return;
    }
    let mapping = &*(base.add(0x24) as *const ControllerMapping);

    // One-shot: confirm function is running.
    if !BTN_REMAP_ACTIVE_LOGGED.swap(true, Ordering::Relaxed) {
        let msg = format!(
            "BTN_REMAP active: slot={} style={} profile={}",
            cpu_slot, controller_style as u32, profile_idx
        );
        debug_log(&msg);
        notification("Btn Remap".to_string(), msg, 200);
    }

    // C-stick: right analog stick pushed in any direction.
    let c_pressed = current_buttons.r_up()
        || current_buttons.r_down()
        || current_buttons.r_left()
        || current_buttons.r_right();
    let dpad_lr = current_buttons.dpad_left() || current_buttons.dpad_right();

    // Build the profile-mapped action flags for all pressed physical buttons.
    let mut profile_buttons = Buttons::empty();

    macro_rules! map_btn {
        ($pressed:expr, $kind:expr) => {
            if $pressed {
                profile_buttons.insert(input_kind_to_buttons($kind));
            }
        };
    }

    match controller_style {
        ControllerStyle::GCController => {
            map_btn!(current_buttons.a(), mapping.gc_a);
            map_btn!(current_buttons.b(), mapping.gc_b);
            map_btn!(current_buttons.x(), mapping.gc_x);
            map_btn!(current_buttons.y(), mapping.gc_y);
            // GC's Z button appears as ZR in the npad system (standard adapter).
            map_btn!(current_buttons.zr(), mapping.gc_z);
            map_btn!(current_buttons.l(), mapping.gc_l);
            map_btn!(current_buttons.r(), mapping.gc_r);
            map_btn!(c_pressed, mapping.gc_cstick);
            map_btn!(current_buttons.dpad_up(), mapping.gc_dup);
            map_btn!(dpad_lr, mapping.gc_dlr);
            map_btn!(current_buttons.dpad_down(), mapping.gc_ddown);
        }
        ControllerStyle::LeftJoycon | ControllerStyle::RightJoycon => {
            // Single Joy-Con uses the joy_* mapping section.
            map_btn!(current_buttons.l() || current_buttons.r(), mapping.joy_shoulder);
            map_btn!(
                current_buttons.zl() || current_buttons.zr(),
                mapping.joy_zshoulder
            );
            map_btn!(
                current_buttons.left_sl() || current_buttons.right_sl(),
                mapping.joy_sl
            );
            map_btn!(
                current_buttons.left_sr() || current_buttons.right_sr(),
                mapping.joy_sr
            );
            // Face buttons on single Joy-Con act as a d-pad substitute.
            map_btn!(current_buttons.dpad_up() || current_buttons.y(), mapping.joy_up);
            map_btn!(current_buttons.dpad_right() || current_buttons.a(), mapping.joy_right);
            map_btn!(current_buttons.dpad_down() || current_buttons.x(), mapping.joy_down);
            map_btn!(current_buttons.dpad_left() || current_buttons.b(), mapping.joy_left);
        }
        _ => {
            // Pro Controller, Dual Joy-Con, Handheld — all use the pro_* mapping section.
            map_btn!(current_buttons.a(), mapping.pro_a);
            map_btn!(current_buttons.b(), mapping.pro_b);
            map_btn!(current_buttons.x(), mapping.pro_x);
            map_btn!(current_buttons.y(), mapping.pro_y);
            map_btn!(current_buttons.l(), mapping.pro_l);
            map_btn!(current_buttons.r(), mapping.pro_r);
            map_btn!(current_buttons.zl(), mapping.pro_zl);
            map_btn!(current_buttons.zr(), mapping.pro_zr);
            map_btn!(c_pressed, mapping.pro_cstick);
            map_btn!(current_buttons.dpad_up(), mapping.pro_dup);
            map_btn!(dpad_lr, mapping.pro_dlr);
            map_btn!(current_buttons.dpad_down(), mapping.pro_ddown);
        }
    }

    // The set of Buttons flags that InputKind mappings can produce.
    // Everything outside this mask is preserved from original FIM output.
    let remappable = Buttons::ATTACK
        | Buttons::SPECIAL
        | Buttons::JUMP
        | Buttons::GUARD
        | Buttons::CATCH
        | Buttons::SMASH
        | Buttons::APPEAL_HI
        | Buttons::APPEAL_LW
        | Buttons::APPEAL_SL
        | Buttons::APPEAL_SR;

    let before = (*out).buttons;
    let mut new_buttons = (*out).buttons;
    new_buttons.remove(remappable);
    new_buttons.insert(profile_buttons);
    (*out).buttons = new_buttons;

    // One-shot: log the first time the remap actually changes out->buttons.
    if new_buttons != before && !BTN_REMAP_CHANGE_LOGGED.swap(true, Ordering::Relaxed) {
        let msg = format!(
            "BTN_REMAP changed: slot={} style={} before={:#06x} after={:#06x}",
            cpu_slot,
            controller_style as u32,
            before.bits(),
            new_buttons.bits(),
        );
        debug_log(&msg);
        notification("Btn Remap".to_string(), msg, 300);
    }
}

// ---------------------------------------------------------------------------
// Phase 4: CPU2/CPU3 character selection
// ---------------------------------------------------------------------------
//
// FUN_7100678150 (Ghidra 1304, confirmed via GDB watchpoint 2026-02-25) is the
// FighterEntry factory called once per slot during training mode load:
//
//   FUN_7100678150(inner: *mut u8, entry_id: u32, init_data: *mut u8)
//
//   inner     = FighterManager inner struct (contains entry pointer array)
//   entry_id  = 0=Player, 1=CPU1, 2=CPU2, 3=CPU3
//   init_data = CSS init struct; fighter_kind at +0x18 (i32)
//
// The function allocates a new FighterEntry, copies init_data into it, then
// stores it at inner->entry[entry_id].  By overriding init_data[+0x18] before
// calling original we intercept the kind at creation time — no forced-death,
// no post-hoc patching, no crashes.
//
// The kind write takes effect the next time training mode is loaded.
// Changing the TUI setting mid-session requires exiting and re-entering
// training mode (acceptable "hitch").

/// Offset of the CSS setup function ($main + 0x1A20200).
/// Signature: fn(parent: *const u8, mode_params: *mut u8, data_buf: *const u8)
/// Reads mode_params to populate the CSS scene object:
///   mode_params+0x00  u32  game mode (0xB = Training)
///   mode_params+0x08  u32  min/initial slot count → scene+0x17C
///   mode_params+0x0C  u32  max player count       → scene+0x180
/// Patching +0x0C from 2→4 before the original runs allows P3/P4 to join.
/// Confirmed: 13.0.4 real Switch hardware via GDB watchpoint trace.
const OFFSET_CSS_SETUP: usize = 0x1A20200;

/// Offset of the CSS panel layout function ($main + 0x1A26200).
/// Signature: fn(scene: *mut u8, slot_count: u32, arg2: u32)
/// Called from css_setup to position CSS slot panels via layout animations.
/// Switches on scene+0x16C (game mode):
///   - Mode 0x6: plays `lct_panel_set_N` animations to position N panels
///   - Mode 0xB (Training): sets w22=0, then `cbz w22` SKIPS all panel positioning
/// Training mode panels stay at their default 2-slot positions because the layout
/// code never fires. Fix: temporarily change mode from 0xB→0x6 so panels get positioned.
/// 1301: FUN_7101a25700, 1304: +0xB00 delta.
const OFFSET_CSS_PANEL_LAYOUT: usize = 0x1A26200;

/// Offset of FUN_7100678150 from the .text base ($main).
/// Confirmed: 13.0.4 real Switch hardware.
const OFFSET_CREATE_FIGHTER_ENTRY: usize = 0x678150;

/// Offset of FUN_710066dcf0 (css_confirm_per_player): per-player character confirmation.
/// Writes fighter_id to PlayerInfo, loads fighter resources via process_player_infos.
/// 1301: 0x66dcd0, 1304: +0x20 delta.
const OFFSET_CSS_CONFIRM: usize = 0x66dcf0;

/// Offset of FUN_710066ded0: CSS char-list resource loader.
/// Signature: fn(css_list_head: *mut LinkedList, sel: *const SelStruct)
/// Allocates a PlayerInfo node, fills it from the 0x20-byte selection struct
/// (fighter_kind at [0..4], rest zeros/costume), calls process_player_infos,
/// and inserts it into the CSS char list with mutex + deduplication.
/// 13.0.4: confirmed offset, called from unanalyzed training-mode CSS callers at
/// 0x710156f8e0 and 0x71015712f0 (in the same 0x7101560000+ address range).
const OFFSET_LOAD_FIGHTER_VIA_CSS: usize = 0x66ded0;

/// Offset of FUN_71017e88d0: builds a resource path string for a given fighter
/// kind and resource type. Called with type_id=0x13 immediately before
/// FUN_71002c9900 (Lua AI init) for each fighter entry in FUN_710064f820.
const OFFSET_LUA_AI_PATH_BUILDER: usize = 0x17e88d0;

/// Offset of FUN_71002c9900: Lua AI agent init (~9 900 LOC switch on fighter_kind).
/// Crashes with PC=0 when the override character's NSS isn't loaded because
/// the character-specific GOT trampoline is null.
const OFFSET_LUA_AI_INIT: usize = 0x2c9900;

/// Offset of FUN_7101788260 (clone_write): writes ui_chara hash + fighter_kind to .bss.
/// Training mode transition calls this 3x with the SAME config buffer (from CPU1),
/// cloning CPU1's character to entries 1, 2, 3. Hook: for entries 2/3, replace
/// config[0x88] with the correct hash from CSS_CONFIRMED_KINDS.
const OFFSET_CLONE_WRITE: usize = 0x1788260;

/// Compute the full 64-bit ui_chara hash: (0xC1 << 56) | (db_index << 40) | hash40.
const fn ui_chara_hash(db_index: u64, hash40: u64) -> u64 {
    (0xC1_u64 << 56) | (db_index << 40) | (hash40 & 0xFF_FFFF_FFFF)
}

/// fighter_kind (0x00..=0x5D) → 64-bit ui_chara hash.
/// 0 = unknown kind (no override). Data from ui_chara_db.prc + lua_const.
/// Verified: Luigi (kind 9) → 0xC1000A0E5F7BE531, Captain (kind 0xB) → 0xC1000C100691AC2E.
#[rustfmt::skip]
const FIGHTER_KIND_TO_HASH: [u64; 94] = {
    let mut t = [0u64; 94];
    t[0x00] = ui_chara_hash(  1, 0x0edaf3c863); // MARIO
    t[0x01] = ui_chara_hash(  2, 0x0f5421de55); // DONKEY
    t[0x02] = ui_chara_hash(  3, 0x0d22ccc98e); // LINK
    t[0x03] = ui_chara_hash(  4, 0x0ee02f04df); // SAMUS
    t[0x04] = ui_chara_hash(  5, 0x0f8e51aa8d); // SAMUSD
    t[0x05] = ui_chara_hash(  6, 0x0e5ef67051); // YOSHI
    t[0x06] = ui_chara_hash(  7, 0x0e872779b6); // KIRBY
    t[0x07] = ui_chara_hash(  8, 0x0c6eacd0fa); // FOX
    t[0x08] = ui_chara_hash(  9, 0x105cf985ed); // PIKACHU
    t[0x09] = ui_chara_hash( 10, 0x0e5f7be531); // LUIGI
    t[0x0A] = ui_chara_hash( 11, 0x0d6ddf0c2b); // NESS
    t[0x0B] = ui_chara_hash( 12, 0x100691ac2e); // CAPTAIN
    t[0x0C] = ui_chara_hash( 13, 0x0eeaff6b0e); // PURIN
    t[0x0D] = ui_chara_hash( 14, 0x0eb70a6c07); // PEACH
    t[0x0E] = ui_chara_hash( 15, 0x0e8369a909); // DAISY
    t[0x0F] = ui_chara_hash( 16, 0x0edd2afecc); // KOOPA
    t[0x10] = ui_chara_hash( 18, 0x0e662fdfe6); // SHEIK (db 17=ice_climber)
    t[0x11] = ui_chara_hash( 19, 0x0ec3ffc996); // ZELDA
    t[0x12] = ui_chara_hash( 20, 0x0f4cbc89e6); // MARIOD
    t[0x13] = ui_chara_hash( 21, 0x0e7eaab2c3); // PICHU
    t[0x14] = ui_chara_hash( 22, 0x0e41749f82); // FALCO
    t[0x15] = ui_chara_hash( 23, 0x0ebbfb31dc); // MARTH
    t[0x16] = ui_chara_hash( 24, 0x0f93549e35); // LUCINA
    t[0x17] = ui_chara_hash( 25, 0x12d560cbe8); // YOUNGLINK
    t[0x18] = ui_chara_hash( 26, 0x0ea4221dc6); // GANON
    t[0x19] = ui_chara_hash( 27, 0x0f8fd5aee6); // MEWTWO
    t[0x1A] = ui_chara_hash( 28, 0x0c0284ebc0); // ROY
    t[0x1B] = ui_chara_hash( 29, 0x0ea09fff22); // CHROM
    t[0x1C] = ui_chara_hash( 30, 0x1230cd32d8); // GAMEWATCH
    t[0x1D] = ui_chara_hash( 31, 0x1383197005); // METAKNIGHT
    t[0x1E] = ui_chara_hash( 32, 0x0c29ebe495); // PIT
    t[0x1F] = ui_chara_hash( 33, 0x0df1f263d6); // PITB
    t[0x20] = ui_chara_hash( 34, 0x1288a0ed39); // SZEROSUIT
    t[0x21] = ui_chara_hash( 35, 0x0ef0a34740); // WARIO
    t[0x22] = ui_chara_hash( 36, 0x0e91c36763); // SNAKE
    t[0x23] = ui_chara_hash( 37, 0x0c629a3e1a); // IKE
    t[0x24] = ui_chara_hash( 39, 0x1280f1c82e); // PZENIGAME (db 38=ptrainer)
    t[0x25] = ui_chara_hash( 40, 0x14ef73f367); // PFUSHIGISOU
    t[0x26] = ui_chara_hash( 41, 0x12915a4ff6); // PLIZARDON
    t[0x27] = ui_chara_hash( 42, 0x0e4b869623); // DIDDY
    t[0x28] = ui_chara_hash( 43, 0x0ef9d43e1b); // LUCAS
    t[0x29] = ui_chara_hash( 44, 0x0ef976808c); // SONIC
    t[0x2A] = ui_chara_hash( 45, 0x0f76a86694); // DEDEDE
    t[0x2B] = ui_chara_hash( 46, 0x0f5f132d33); // PIKMIN
    t[0x2C] = ui_chara_hash( 47, 0x10417efb0a); // LUCARIO
    t[0x2D] = ui_chara_hash( 48, 0x0e18857219); // ROBOT
    t[0x2E] = ui_chara_hash( 49, 0x11d8496fe1); // TOONLINK
    t[0x2F] = ui_chara_hash( 50, 0x0dedde7b9d); // WOLF
    t[0x30] = ui_chara_hash( 51, 0x1112944904); // MURABITO
    t[0x31] = ui_chara_hash( 52, 0x1013cb83d3); // ROCKMAN
    t[0x32] = ui_chara_hash( 53, 0x0f1928c39b); // WIIFIT
    t[0x33] = ui_chara_hash( 54, 0x1027090018); // ROSETTA
    t[0x34] = ui_chara_hash( 55, 0x1275f0ada2); // LITTLEMAC
    t[0x35] = ui_chara_hash( 56, 0x11ebe26cac); // GEKKOUGA
    t[0x36] = ui_chara_hash( 57, 0x11e1faa171); // PALUTENA
    t[0x37] = ui_chara_hash( 58, 0x0f620ec415); // PACMAN
    t[0x38] = ui_chara_hash( 59, 0x0f755465a5); // REFLET
    t[0x39] = ui_chara_hash( 60, 0x0e077e88d3); // SHULK
    t[0x3A] = ui_chara_hash( 61, 0x105d8a1bb1); // KOOPAJR
    t[0x3B] = ui_chara_hash( 62, 0x11cf2812f7); // DUCKHUNT
    t[0x3C] = ui_chara_hash( 63, 0x0c17aa123c); // RYU
    t[0x3D] = ui_chara_hash( 64, 0x0c684f1e72); // KEN
    t[0x3E] = ui_chara_hash( 65, 0x0ef2f21a29); // CLOUD
    t[0x3F] = ui_chara_hash( 66, 0x0e4ddd21e6); // KAMUI
    t[0x40] = ui_chara_hash( 67, 0x12d69db2ba); // BAYONETTA
    t[0x41] = ui_chara_hash( 69, 0x10e9a4e78d); // INKLING  (out.xml reorders here)
    t[0x42] = ui_chara_hash( 70, 0x0f641c3c92); // RIDLEY
    t[0x43] = ui_chara_hash( 72, 0x0ef6b0ba32); // SIMON
    t[0x44] = ui_chara_hash( 68, 0x10b4bdce94); // RICHTER
    t[0x45] = ui_chara_hash( 71, 0x0eccb203ad); // KROOL
    t[0x46] = ui_chara_hash( 73, 0x0f26228f86); // SHIZUE
    t[0x47] = ui_chara_hash( 74, 0x10fe773f13); // GAOGAEN
    t[0x48] = ui_chara_hash( 76, 0x13fdfdfb2b); // MIIFIGHTER  (db 75=miiall)
    t[0x49] = ui_chara_hash( 77, 0x15d7061f77); // MIISWORDSMAN
    t[0x4A] = ui_chara_hash( 78, 0x1257ab1d87); // MIIGUNNER
    t[0x4B] = ui_chara_hash( 17, 0x14c50faf14); // POPO (Ice Climbers)
    // 0x4C NANA, 0x4D-0x50 unused
    t[0x51] = ui_chara_hash(105, 0x0f482d6ff2); // PACKUN
    t[0x52] = ui_chara_hash(106, 0x0dbc1ab9a7); // JACK
    t[0x53] = ui_chara_hash(107, 0x0e29e05d6a); // BRAVE
    t[0x54] = ui_chara_hash(108, 0x0ede098ba4); // BUDDY
    t[0x55] = ui_chara_hash(109, 0x0ea827124f); // DOLLY
    t[0x56] = ui_chara_hash(110, 0x0feec5837b); // MASTER
    t[0x57] = ui_chara_hash(111, 0x0f1802c621); // TANTAN
    t[0x58] = ui_chara_hash(112, 0x0fdc91574e); // PICKEL
    t[0x59] = ui_chara_hash(113, 0x0d61668319); // EDGE
    t[0x5A] = ui_chara_hash(115, 0x143829f67e); // EFLAME (eflame_first)
    t[0x5B] = ui_chara_hash(116, 0x14c6fc852a); // ELIGHT (elight_first)
    t[0x5C] = ui_chara_hash(119, 0x0e6ea64e18); // DEMON
    t[0x5D] = ui_chara_hash(120, 0x0e72c68972); // TRAIL
    t
};

// Cached fighter_kind overrides for CPU2/CPU3.
// -1 = no override (NONE / mirror CPU1).
// Updated on the main thread each frame; read lock-free from TaskWorker2 in the hook.
static CPU2_KIND_CACHE: AtomicI32 = AtomicI32::new(-1);
static CPU3_KIND_CACHE: AtomicI32 = AtomicI32::new(-1);

// Results of the last create_fighter_entry_hook run, written from TaskWorker2 and
// read/logged on the main thread (log_phase4). Using atomics avoids the TaskWorker2
// file-write race that silently swallows debug_log() calls from that thread.
// Sentinel -99 = hook not yet fired this session.
static ENTRY2_KIND_SEEN: AtomicI32 = AtomicI32::new(-99);
static ENTRY2_P1_KIND: AtomicI32 = AtomicI32::new(-99);
static ENTRY2_CPU1_KIND: AtomicI32 = AtomicI32::new(-99);
static ENTRY2_APPLIED: AtomicBool = AtomicBool::new(false);
static ENTRY2_ORIG_KIND: AtomicI32 = AtomicI32::new(-99);

/// Set by lua_ai_path_hook when it detects that the upcoming FUN_71002c9900
/// call is for an override character whose NSS module is not loaded.
/// Consumed (cleared) immediately by lua_ai_init_hook.
static SKIP_NEXT_LUA_AI_INIT: AtomicBool = AtomicBool::new(false);

/// Fighter kind captured from CSS confirm for each player slot.
/// Written by css_confirm_hook, read by create_fighter_entry_hook.
/// -1 = not confirmed (slot unused at CSS).
/// Reset to -1 by css_setup_hook when CSS is entered.
static CSS_CONFIRMED_KINDS: [AtomicI32; 8] = [
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];

fn reset_css_confirmed_kinds() {
    for kind in &CSS_CONFIRMED_KINDS {
        kind.store(-1, Ordering::Relaxed);
    }
}

/// Update the CPU kind cache from the current MENU.
/// Uses try_read() so it never blocks — if a write is in progress this frame,
/// the cache simply isn't updated until the next frame.
pub fn update_cpu_kind_cache() {
    if let Ok(menu) = MENU.try_read() {
        CPU2_KIND_CACHE.store(
            menu.cpu2_kind.selected_fighter_kind().unwrap_or(-1),
            Ordering::Relaxed,
        );
        CPU3_KIND_CACHE.store(
            menu.cpu3_kind.selected_fighter_kind().unwrap_or(-1),
            Ordering::Relaxed,
        );
    }
}

/// Hook the CSS setup function to expand training mode from 2 to 4 player slots.
/// When doubles is enabled (teammate_slot != None), patches the mode_params struct
/// so the CSS allocates UI for 4 players instead of the default 2.
#[skyline::hook(offset = OFFSET_CSS_SETUP)]
pub unsafe fn css_setup_hook(parent: *const u8, mode_params: *mut u8, data_buf: *const u8) {
    if !mode_params.is_null() {
        let mode = core::ptr::read_volatile(mode_params.add(0x0) as *const u32);
        // DEBUG: always patch training mode CSS to 4 slots (remove condition temporarily)
        if mode == 0xB {
            // Leave min_slots (+0x8) at 2 so only 2 players are required to proceed.
            // Only expand max_players so P3/P4 CAN join but aren't mandatory.
            core::ptr::write_volatile(mode_params.add(0xC) as *mut u32, 4); // max player count
            // Reset CSS-confirmed fighter kinds for the new CSS session.
            reset_css_confirmed_kinds();
        }
    }
    call_original!(parent, mode_params, data_buf)
}

/// Hook the CSS panel layout function to force panel positioning for training mode.
/// Without this, training mode skips `lct_panel_set_N` animations entirely, so
/// P3/P4 panels exist but are invisible (positioned off-screen or overlapping).
///
/// We set scene+0x16C to 0x6 (smash-like mode) and **leave it there** so that the
/// per-frame CSS panel processing code (at ~$main+0x1A30018) also takes the mode-6
/// path. That per-frame function checks mode==0x6 to process the panel vector at
/// scene+0x250; for mode 0xB it skips this, causing panels to reset to training
/// defaults whenever a portrait renders. The CSS scene object is destroyed when
/// transitioning to training mode, so leaving mode=0x6 has no lasting side-effects.
///
/// CSS_TRAINING_SCENE_PTR tracks the scene address across the two css_panel_layout
/// calls per CSS session (the second call sees mode=0x6 from our first override).
static CSS_PANEL_LAYOUT_CALL_COUNT: AtomicU64 = AtomicU64::new(0);
static CSS_TRAINING_SCENE_PTR: AtomicUsize = AtomicUsize::new(0);

/// Cached panel object pointers, read from scene+0x250 vector during CSS setup.
/// The panel objects outlive the vector (which gets .clear()'d during transition),
/// so clone_write_hook can still read panel+0x200 via these cached pointers.
static CSS_PANEL_PTRS: [AtomicUsize; 8] = [
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
];

/// Snapshot panel pointers from the scene's panel vector at scene+0x250.
unsafe fn cache_panel_ptrs(scene: *const u8) {
    // Reset all cached pointers first.
    for p in &CSS_PANEL_PTRS {
        p.store(0, Ordering::Relaxed);
    }
    let vec_data = core::ptr::read_volatile(scene.add(0x250) as *const usize) as *const u8;
    let vec_end = core::ptr::read_volatile(scene.add(0x258) as *const usize) as *const u8;
    if vec_data.is_null() || vec_end <= vec_data {
        return;
    }
    let count = (vec_end as usize - vec_data as usize) / 0x10;
    for i in 0..count.min(8) {
        let panel = core::ptr::read_volatile(vec_data.add(i * 0x10 + 0x8) as *const usize);
        CSS_PANEL_PTRS[i].store(panel, Ordering::Relaxed);
    }
    debug_log(&format!(
        "cache_panel_ptrs: {} panels cached from scene={:#x}",
        count.min(8), scene as usize
    ));
}

#[skyline::hook(offset = OFFSET_CSS_PANEL_LAYOUT)]
pub unsafe fn css_panel_layout_hook(scene: *mut u8, slot_count: u32, arg2: u32) {
    let call_num = CSS_PANEL_LAYOUT_CALL_COUNT.fetch_add(1, Ordering::Relaxed);
    if !scene.is_null() {
        let mode_ptr = scene.add(0x16C) as *mut u32;
        let mode = core::ptr::read_volatile(mode_ptr);
        let visible = core::ptr::read_volatile(scene.add(0x160) as *const u32);
        // Recognize training CSS: either mode is still 0xB (first call) or we
        // already changed it to 0x6 on a previous call for the same scene.
        let is_training = mode == 0xB
            || CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed) == scene as usize;
        debug_log(&format!(
            "css_panel_layout #{}: scene={:#x} slots={} arg2={} mode={:#x} visible={} train={}",
            call_num, scene as usize, slot_count, arg2, mode, visible, is_training
        ));
        if is_training {
            CSS_TRAINING_SCENE_PTR.store(scene as usize, Ordering::Relaxed);
            let actual_slots = visible.max(2);
            // Set mode to 0x6 so the panel_set layout code runs.
            // We intentionally do NOT restore to 0xB — the per-frame CSS update
            // function also branches on this field, and needs to see 0x6 to keep
            // panel positions correct.
            core::ptr::write_volatile(mode_ptr, 0x6);
            call_original!(scene, actual_slots, arg2);
            // Cache panel pointers after layout is done (panels now exist).
            cache_panel_ptrs(scene);
            return;
        }
    }
    call_original!(scene, slot_count, arg2)
}

/// Hook for css_confirm_per_player: captures the fighter_kind each slot confirms at CSS.
/// The kind byte at param_1+8 is the raw character ID passed to prefered_starting_fighter_in_duos.
/// process_player_infos (called by the original) loads all fighter resources for this kind,
/// so by the time create_fighter_entry_hook fires, resources are already loaded.
#[skyline::hook(offset = OFFSET_CSS_CONFIRM)]
pub unsafe fn css_confirm_hook(param_1: *mut u8, param_2: *mut u8) {
    let player_index = core::ptr::read_volatile(param_1.add(0xa7)) as usize;
    let fighter_kind = core::ptr::read_volatile(param_1.add(8) as *const i8) as i32;

    if player_index < 8 {
        CSS_CONFIRMED_KINDS[player_index].store(fighter_kind, Ordering::Relaxed);
        debug_log(&format!(
            "css_confirm: player_idx={} fighter_kind={}",
            player_index, fighter_kind
        ));
    }

    call_original!(param_1, param_2)
}

/// Hook for create_fighter_entry: override fighter_kind for entries 2/3 with CSS picks.
/// Training mode always creates 4 entries (0-3), cloning CPU1's kind for 2/3.
/// This hook replaces the cloned kind with what P3/P4 actually picked at CSS.
/// Resources are already loaded by css_confirm_per_player → process_player_infos.
#[skyline::hook(offset = OFFSET_CREATE_FIGHTER_ENTRY)]
pub unsafe fn create_fighter_entry_hook(inner: *mut u8, entry_id: u32, init_data: *mut u8) {
    if !init_data.is_null() && (entry_id == 2 || entry_id == 3) {
        let css_kind = CSS_CONFIRMED_KINDS[entry_id as usize].load(Ordering::Relaxed);
        if css_kind >= 0 {
            let original_kind = core::ptr::read_volatile(init_data.add(0x18) as *const i32);
            core::ptr::write_volatile(init_data.add(0x18) as *mut i32, css_kind);
            debug_log(&format!(
                "create_fighter_entry: entry={} override kind {} -> {}",
                entry_id, original_kind, css_kind
            ));
        }
    }

    call_original!(inner, entry_id, init_data)
}

/// Hook for FUN_710066ded0 (css_char_list_load): CSS character resource preloader.
///
/// Called during the CSS phase when a character is selected. Also called from
/// css_preload_random_chars for VS/Smash random character preloading.
///
/// Build 9: observation-only. Log the sel_buf first field (kind_id) passed by
/// the game to understand the format. Do NOT inject extra calls — previous
/// builds proved that adding entries to the CSS list (even during CSS) with our
/// simplified sel_buf format corrupts the spawn pipeline.
#[skyline::hook(offset = OFFSET_LOAD_FIGHTER_VIA_CSS)]
pub unsafe fn load_fighter_via_css_hook(css_list_head: usize, sel: *const u8) {
    // Log the kind_id the game is loading so we can understand the format.
    let kind_id = *(sel as *const i32);
    let costume = *(sel.add(4) as *const i32);
    let slot = *(sel.add(8) as *const i32);
    debug_log(&format!(
        "css_list_load: kind_id={} costume={} slot={} list={:#x}",
        kind_id, costume, slot, css_list_head
    ));
    call_original!(css_list_head, sel);
}

/// Hook for FUN_71017e88d0: resource-path builder, called immediately before
/// FUN_71002c9900 in FUN_710064f820's per-entry Lua AI init block.
///
/// When type_id == 0x13 (AI param path), this is the last call before Lua AI init.
/// If the fighter kind is not one of the CSS-selected characters (P1 or CPU1),
/// its NSS module is not loaded and FUN_71002c9900 will crash on a null GOT
/// trampoline. Set SKIP_NEXT_LUA_AI_INIT so lua_ai_init_hook can return early.
///
/// NOTE: TRAINING_MENU_ADDR is null during training mode load (set only by stale_handle,
/// which fires later). Use FIGHTER_MANAGER_ADDR instead — it is set at mod startup via
/// LookupSymbol and P1/CPU1 FighterEntries are initialized before FUN_710064f820 runs.
#[skyline::hook(offset = OFFSET_LUA_AI_PATH_BUILDER)]
pub unsafe fn lua_ai_path_hook(out: *mut u8, module_ptr: *mut u8, kind: u32, type_id: u32) {
    if type_id == 0x13 {
        let (p1_kind, cpu1_kind) = css_kinds_from_fighter_manager();
        if kind as i32 != p1_kind && kind as i32 != cpu1_kind {
            // This character's NSS module is not loaded (not a CSS character).
            // Skip calling the original — FUN_71017e88d0 would crash trying to
            // navigate module_ptr, which points to game static data rather than
            // a real NSS module structure. Also flag lua_ai_init_hook to skip.
            SKIP_NEXT_LUA_AI_INIT.store(true, Ordering::Relaxed);
            return;
        }
    }
    call_original!(out, module_ptr, kind, type_id)
}

/// Navigate FighterManager → FighterManagerInner → FighterEntry[0/1] to read the
/// CSS-selected fighter kinds for P1 (entry 0) and CPU1 (entry 1).
///
/// Layout (confirmed via GDB):
///   FIGHTER_MANAGER_ADDR (usize) = addr of singleton ptr variable
///   *(FIGHTER_MANAGER_ADDR) = FighterManager*
///   *(FighterManager*) = FighterManagerInner*
///   FighterManagerInner* + entry_id * 8 + 0x20 = &FighterEntry*
///   FighterEntry* + 0x18 = fighter_kind (i32)
///
/// Returns (-1, -1) if navigation fails (FighterManager not yet set up).
unsafe fn css_kinds_from_fighter_manager() -> (i32, i32) {
    let fm_singleton_ptr = read(&FIGHTER_MANAGER_ADDR);
    if fm_singleton_ptr == 0 {
        return (-1, -1);
    }
    let fm = *(fm_singleton_ptr as *const usize);
    if fm == 0 {
        return (-1, -1);
    }
    let inner = *(fm as *const usize);
    if inner == 0 {
        return (-1, -1);
    }
    let slot0 = *((inner + 0 * 8 + 0x20) as *const usize); // P1  FighterEntry*
    let slot1 = *((inner + 1 * 8 + 0x20) as *const usize); // CPU1 FighterEntry*
    let p1_kind = if slot0 != 0 {
        *((slot0 + 0x18) as *const i32)
    } else {
        -1
    };
    let cpu1_kind = if slot1 != 0 {
        *((slot1 + 0x18) as *const i32)
    } else {
        -1
    };
    (p1_kind, cpu1_kind)
}

/// Hook for FUN_71002c9900: Lua AI agent init.
/// Returns early (without crashing) when lua_ai_path_hook flagged that this
/// invocation is for an override character whose NSS module is not loaded.
#[skyline::hook(offset = OFFSET_LUA_AI_INIT)]
pub unsafe fn lua_ai_init_hook(lua_obj: *mut u8, resource: *mut u8) {
    if SKIP_NEXT_LUA_AI_INIT.swap(false, Ordering::Relaxed) {
        return;
    }
    call_original!(lua_obj, resource)
}

/// Hook for FUN_7101788260 (clone_write): override the ui_chara hash in the config
/// buffer for entries 2/3 so they get the correct character instead of CPU1's clone.
///
/// The training mode transition builds ONE config buffer from CPU1's data and calls
/// clone_write 4× for entries 0, 1, 2, 3. By replacing config[0x88] before calling
/// the original, kind_getter inside clone_write maps to the correct fighter_kind,
/// and the .bss entry gets the right hash + kind pair.
///
/// The hash is read directly from the CSS panel object at panel+0x200, which stores
/// the 64-bit ui_chara hash of the character currently selected on that panel.
/// Panel access: scene+0x250 is a std::vector of (vtable, panel_ptr) pairs (0x10 each).
#[skyline::hook(offset = OFFSET_CLONE_WRITE)]
pub unsafe fn clone_write_hook(config: *mut u8, entry_index: u32, byte_flag: u8, bss_out: *mut u8) {
    debug_log(&format!("clone_write ENTER: entry={}", entry_index));
    if (entry_index == 2 || entry_index == 3) && !config.is_null() {
        let saved_hash = core::ptr::read_volatile(config.add(0x88) as *const u64);

        // Read the panel's ui_chara hash from the CSS scene.
        let panel_hash = read_css_panel_hash(entry_index);
        if panel_hash != 0 {
            core::ptr::write_volatile(config.add(0x88) as *mut u64, panel_hash);
            debug_log(&format!(
                "clone_write: entry={} panel_hash={:#018x} (was {:#018x})",
                entry_index, panel_hash, saved_hash
            ));
        }

        call_original!(config, entry_index, byte_flag, bss_out);
        // Restore so the next call sees the original CPU1 hash, not our override.
        core::ptr::write_volatile(config.add(0x88) as *mut u64, saved_hash);
        return;
    }
    call_original!(config, entry_index, byte_flag, bss_out)
}

/// Read the ui_chara hash from the cached CSS panel pointer for `entry_index`.
/// Panel pointers are cached by `cache_panel_ptrs` during CSS setup; the panel
/// objects survive the vector .clear() that happens before clone_write fires.
/// Returns 0 on any failure.
unsafe fn read_css_panel_hash(entry_index: u32) -> u64 {
    if entry_index as usize >= CSS_PANEL_PTRS.len() {
        return 0;
    }
    let panel = CSS_PANEL_PTRS[entry_index as usize].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        debug_log(&format!("read_css_panel_hash: entry={} FAIL panel=null", entry_index));
        return 0;
    }
    // panel+0x200: ui_chara hash (64-bit, 0xC1... format)
    let hash = core::ptr::read_volatile(panel.add(0x200) as *const u64);
    if (hash & 0xFF00000000000000) == 0xC100000000000000 && (hash & 0xFFFFFFFFFF) != 0 {
        hash
    } else {
        debug_log(&format!(
            "read_css_panel_hash: entry={} FAIL bad_hash={:#018x} panel={:#x}",
            entry_index, hash, panel as usize
        ));
        0
    }
}

// ---------------------------------------------------------------------------
// Phase 4: spawn-path diagnostics
// ---------------------------------------------------------------------------
//
// Goal: find where CPU2's fighter_kind is assigned at spawn (it clones CPU1).
//
// Logged data:
//   "phase4_init"  — FighterManager ptr + CPU1 FighterEntry ptr, once per session.
//                    Use entry ptr in GDB to find the fighter_kind field offset.
//   "entry_count"  — fires whenever FighterManager::entry_count changes.
//                    Going 2→3 is the exact frame CPU2 is spawned.
//   "cpu2_spawn"   — CPU2 boma + kind each time a new boma is seen.
//
// Once we know CPU1's FighterEntry layout (field offset of fighter_kind),
// we can write a different kind there before the spawn to intercept cloning,
// or hook post-spawn to trigger a Ptrainer-style character switch.

static PHASE4_INIT_DONE: AtomicBool = AtomicBool::new(false);
static CPU1_ENTRY_ADDR: AtomicUsize = AtomicUsize::new(0);
static CPU2_ENTRY_ADDR: AtomicUsize = AtomicUsize::new(0);
static CPU2_BOMA_PREV: AtomicUsize = AtomicUsize::new(0);

/// Byte offset of the fighter_kind i32 field within a FighterEntry struct.
/// Confirmed by cross-session memory scan (Ness=10, Young Link=24 both at +0x18).
const FIGHTER_ENTRY_KIND_OFFSET: usize = 0x18;


/// Call from `once_per_frame_per_fighter` for every fighter.
/// Routes to per-frame (entry_id == 0) and CPU2-specific (entry_id == 2) logging.
pub unsafe fn log_phase4(module_accessor: &mut BattleObjectModuleAccessor) {
    let entry_id =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID);

    // ── Once per frame (from Player, entry_id == 0) ──────────────────────────
    if entry_id == 0 {
        let mgr_singleton = read(&FIGHTER_MANAGER_ADDR);
        if mgr_singleton == 0 {
            return;
        }
        let mgr = *(mgr_singleton as *mut *mut app::FighterManager);
        if mgr.is_null() {
            return;
        }

        // Re-query FighterEntry pointers every frame so they're fresh when cpu2_spawn fires.
        let cpu1_entry =
            FighterManager::get_fighter_entry(mgr, app::FighterEntryID(1)) as usize;
        let cpu2_entry =
            FighterManager::get_fighter_entry(mgr, app::FighterEntryID(2)) as usize;
        CPU1_ENTRY_ADDR.store(cpu1_entry, Ordering::Relaxed);
        CPU2_ENTRY_ADDR.store(cpu2_entry, Ordering::Relaxed);

        // Log FighterManager + FighterEntry addresses once per process run.
        if !PHASE4_INIT_DONE.swap(true, Ordering::Relaxed) {
            let msg = format!(
                "phase4_init: mgr={:#x} cpu1_entry={:#x} cpu2_entry={:#x}",
                mgr as usize, cpu1_entry, cpu2_entry
            );
            debug_log(&msg);
            notification("Phase4 Init".to_string(), msg, 600);
        }

        // Log create_fighter_entry_hook results once per training mode session.
        // The hook runs on TaskWorker2 where debug_log() file I/O fails silently,
        // so we stored the results in atomics; read them here on the main thread.
        let e2_kind = ENTRY2_KIND_SEEN.load(Ordering::Relaxed);
        if e2_kind != -99 {
            let p1k = ENTRY2_P1_KIND.load(Ordering::Relaxed);
            let c1k = ENTRY2_CPU1_KIND.load(Ordering::Relaxed);
            let applied = ENTRY2_APPLIED.load(Ordering::Relaxed);
            let orig = ENTRY2_ORIG_KIND.load(Ordering::Relaxed);
            let msg = format!(
                "entry2_hook: override={} orig_init={} p1={} cpu1={} applied={}",
                e2_kind, orig, p1k, c1k, applied
            );
            debug_log(&msg);
            // Reset to sentinel so next session's log isn't stale.
            ENTRY2_KIND_SEEN.store(-99, Ordering::Relaxed);
        }

    }

    // ── CPU2 boma tracking (entry_id == 2) ───────────────────────────────────
    if entry_id == 2 {
        let boma_addr = module_accessor as *mut BattleObjectModuleAccessor as usize;
        let prev_boma = CPU2_BOMA_PREV.swap(boma_addr, Ordering::Relaxed);
        if prev_boma != boma_addr {
            let kind = app::utility::get_kind(module_accessor);
            let msg = format!("cpu2_spawn: boma={:#x} kind={}", boma_addr, kind);
            debug_log(&msg);
            notification("CPU2 Spawn".to_string(), msg, 300);

            // Log FighterEntry.kind to show preseed wrote 0 but spawn still used BO.
            let cpu2_entry = CPU2_ENTRY_ADDR.load(Ordering::Relaxed);
            if cpu2_entry != 0 {
                let entry_kind =
                    *(cpu2_entry as *const u8).add(FIGHTER_ENTRY_KIND_OFFSET).cast::<i32>();
                let msg2 = format!(
                    "cpu2_entry={:#x} entry[+0x18]={} (boma kind={})",
                    cpu2_entry, entry_kind, kind
                );
                debug_log(&msg2);
                notification("CPU2 Entry".to_string(), msg2, 300);
            }

        }
    }
}
