/// Doubles training support:
///
/// - CPU↔CPU hitbox interaction: each CPU gets a unique hit-team equal to its
///   entry ID so the game's team-attack filter allows CPU→CPU hits.
/// - 4-slot CSS: hooks CSS setup/layout to show 4 character select panels in
///   training mode, allowing P3/P4 character selection.
/// - Clone-write override: intercepts the training-mode config cloning so
///   entries 2/3 get their CSS-selected character instead of CPU1's clone.
/// - Lua AI safety: skips AI init for override characters whose NSS modules
///   are not loaded, preventing null-pointer crashes.
/// - Clone-write also sets human/CPU status and controller bindings from CSS,
///   so CPU Behavior doesn't need to be forced.
use core::sync::atomic::{AtomicBool, AtomicI32, AtomicU32, AtomicU64, AtomicUsize, Ordering};

use smash::app::{lua_bind::*, BattleObjectModuleAccessor};
use smash::lib::lua_const::*;
use training_mod_sync::*;

use crate::common::{FIGHTER_MANAGER_ADDR, MENU};

// ---------------------------------------------------------------------------
// SD card debug log
// ---------------------------------------------------------------------------

const DOUBLES_DEBUG_LOG: &str = "sd:/ultimate/TrainingModpack/doubles_debug.log";

/// Appends `msg` with a timestamp to the persistent debug file on the SD card.
/// On the first call each session, the file is truncated (overwritten) so you
/// always get a fresh log without manual cleanup.
/// Errors are silently ignored so this is safe to call from any hook context.
pub fn debug_log(msg: &str) {
    use std::io::Write;
    // Monotonic frame counter as lightweight timestamp (no std::time on this platform).
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    static FIRST_CALL: AtomicBool = AtomicBool::new(true);
    let tick = COUNTER.fetch_add(1, Ordering::Relaxed);
    // First call: truncate. Subsequent calls: append.
    let truncate = FIRST_CALL.swap(false, Ordering::Relaxed);
    let mut opts = std::fs::OpenOptions::new();
    opts.create(true);
    if truncate {
        opts.write(true).truncate(true);
    } else {
        opts.append(true);
    }
    if let Ok(mut f) = opts.open(DOUBLES_DEBUG_LOG) {
        let _ = writeln!(f, "[{:06}] {}", tick, msg);
    }
}

// ---------------------------------------------------------------------------
// CPU↔CPU hit-team assignment
// ---------------------------------------------------------------------------

pub unsafe fn set_cpu_hit_team(module_accessor: &mut BattleObjectModuleAccessor) {
    let entry_id =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID);
    // Every fighter gets hit-team = entry_id so all 4 can hit each other.
    // Without this, fighters on the same team share a hit-team and their
    // hitboxes pass through each other.
    TeamModule::set_hit_team(module_accessor, entry_id);
    // Also set the general team affiliation (team_no) to a unique value.
    // The game checks team_no in addition to hit_team_no for some hit
    // interactions — without this, P3/P4 (cloned from CPU1, team_no=1)
    // cannot hit P2 (also team_no=1) even though hit_team differs.
    TeamModule::set_team(module_accessor, entry_id, false);
}

// ---------------------------------------------------------------------------
// Human controller input injection
// ---------------------------------------------------------------------------
//
// Training mode forces all non-P1 entries to CPU. For entries designated as
// human at CSS, we need to inject their hardware controller input.
//
// Key discovery: FIM fires for player_idx > 0 in training mode when
// config[0x78]=0 (human entry). For CPU entries (config[0x78]=1), FIM only
// fires for player_idx=0. We capture the game's native FIM output for
// human entries at player_idx > 0 (correct controller, correct profile).
// As fallback for entries that may not get native FIM calls (e.g. entries
// 2/3), we also call FIM's original during player_idx=0 with saved
// Controller pointers from CSS.
//
// Approach:
//   1. During CSS: FIM fires for all player_idx — save Controller ptrs per npad
//   2. During training FIM (player_idx=0): call FIM original for each human
//      entry's npad using the saved Controller ptr (fallback)
//   3. During training FIM (player_idx > 0): if human entry, capture native
//      FIM output (overwrites fallback with correct data)
//   4. In set_cpu_controls: for human entries, override AI output with saved
//      MappedInputs; for CPU entries, let AI output stand

use crate::common::input::{
    Buttons, ControlModuleInternal, Controller, ControllerMapping, ControllerStyle, InputKind,
    MappedInputs,
};

/// Per-frame call counter for set_cpu_controls. Incremented each call,
/// reset each frame from the FIM hook. The Nth call (1-indexed) corresponds
/// to entry N (set_cpu_controls fires for entries 1, 2, 3 in order).
static SET_CPU_CONTROLS_COUNTER: AtomicI32 = AtomicI32::new(0);

/// Called from FIM hook (player_idx==0) once per frame to reset the counter.
pub fn reset_cpu_controls_counter() {
    SET_CPU_CONTROLS_COUNTER.store(0, Ordering::Relaxed);
}

/// Tracks which hardware npad each human entry is assigned to.
/// Set during clone_write based on CSS panel data. -1 = not human or unassigned.
static HUMAN_ENTRY_NPAD: [AtomicI32; 4] = [
    AtomicI32::new(-1), // entry 0 = P1, handled by FIM directly
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];

/// Returns the assigned npad for a human entry, or -1 if not human/unassigned.
pub fn get_human_entry_npad(entry_id: i32) -> i32 {
    if entry_id >= 0 && (entry_id as usize) < HUMAN_ENTRY_NPAD.len() {
        HUMAN_ENTRY_NPAD[entry_id as usize].load(Ordering::Relaxed)
    } else {
        -1
    }
}

/// Update the npad for a human entry. Used for self-correction when the
/// fallback Controller lookup discovers the real npad differs from panel data.
pub fn set_human_entry_npad(entry_id: i32, npad: i32) {
    if entry_id >= 0 && (entry_id as usize) < HUMAN_ENTRY_NPAD.len() {
        HUMAN_ENTRY_NPAD[entry_id as usize].store(npad, Ordering::Relaxed);
    }
}

/// Tag/profile index per entry, saved during clone_write.
/// Used to read the real ControllerMapping from game memory for button remapping.
/// The tag index maps into the profile array: base + tag * 0xf7d8 + 0x24.
static HUMAN_ENTRY_TAG: [AtomicU32; 4] = [
    AtomicU32::new(0), AtomicU32::new(0),
    AtomicU32::new(0), AtomicU32::new(0),
];

/// Returns the tag/profile index for a given entry, as saved during clone_write.
pub fn get_entry_tag(entry_id: i32) -> u32 {
    if entry_id >= 0 && (entry_id as usize) < HUMAN_ENTRY_TAG.len() {
        HUMAN_ENTRY_TAG[entry_id as usize].load(Ordering::Relaxed)
    } else {
        0
    }
}

/// Read the ControllerMapping from the game's profile memory for a given tag index.
///
/// Pointer chain from disassembly of clone_write (13.0.4, offset 0x178845c):
///   p0 = *(text + 0x5313510)       // global ptr
///   p1 = *p0                       // profile manager object
///   flag = *(u8*)p1                // must be 0
///   p2 = *(p1 + 0x58)             // inner ptr
///   p3 = *p2                       // array container
///   array_base = *p3               // actual array base
///   entry = array_base + tag * 0xf7d8
///   ControllerMapping at entry + 0x24 (0x50 bytes)
///
/// Returns None if any pointer in the chain is null or flag is nonzero.
pub unsafe fn get_profile_mapping(tag: u32) -> Option<ControllerMapping> {
    use skyline::hooks::{getRegionAddress, Region};
    let text_base = getRegionAddress(Region::Text) as usize;
    let dat_addr = text_base + 0x5313510;
    let p0 = *(dat_addr as *const usize);

    // One-shot diagnostic: log every pointer in the chain
    static CHAIN_LOG_DONE: AtomicBool = AtomicBool::new(false);
    let should_log_chain = !CHAIN_LOG_DONE.swap(true, Ordering::Relaxed);

    if p0 == 0 {
        if should_log_chain { debug_log("PROFILE_CHAIN FAIL: p0=0"); }
        return None;
    }
    // Extra deref: p1 = *p0 (profile manager object)
    let p1 = *(p0 as *const usize);
    if p1 == 0 {
        if should_log_chain { debug_log(&format!("PROFILE_CHAIN FAIL: p0={:#x} *p0=0", p0)); }
        return None;
    }
    // Flag byte at p1 must be 0 (game skips profile lookup if nonzero)
    let flag = *(p1 as *const u8);
    if flag != 0 {
        if should_log_chain {
            debug_log(&format!("PROFILE_CHAIN FAIL: flag={} (p0={:#x} p1={:#x})", flag, p0, p1));
        }
        return None;
    }
    let p2 = *((p1 + 0x58) as *const usize);
    if p2 == 0 {
        if should_log_chain { debug_log(&format!("PROFILE_CHAIN FAIL: *(p1+0x58)=0 p1={:#x}", p1)); }
        return None;
    }
    let p3 = *(p2 as *const usize);
    if p3 == 0 {
        if should_log_chain { debug_log(&format!("PROFILE_CHAIN FAIL: *p2=0 p2={:#x}", p2)); }
        return None;
    }
    let array_base = *(p3 as *const usize);
    if array_base == 0 {
        if should_log_chain { debug_log(&format!("PROFILE_CHAIN FAIL: *p3=0 p3={:#x}", p3)); }
        return None;
    }

    if should_log_chain {
        debug_log(&format!(
            "PROFILE_CHAIN OK: p0={:#x} p1={:#x} p2={:#x} p3={:#x} base={:#x} tag={} entry={:#x}",
            p0, p1, p2, p3, array_base, tag,
            array_base + (tag as usize) * 0xf7d8
        ));
    }

    let entry_base = array_base + (tag as usize) * 0xf7d8;
    let mapping_ptr = (entry_base + 0x24) as *const ControllerMapping;
    let mapping = core::ptr::read_volatile(mapping_ptr);

    // One-shot diagnostic: log the profile data on first successful read per tag
    static PROFILE_LOG_DONE: [AtomicBool; 4] = [
        AtomicBool::new(false), AtomicBool::new(false),
        AtomicBool::new(false), AtomicBool::new(false),
    ];
    let log_slot = (tag as usize) % 4;
    if !PROFILE_LOG_DONE[log_slot].swap(true, Ordering::Relaxed) {
        debug_log(&format!(
            "PROFILE: tag={} gc_a={:?} gc_b={:?} gc_tapjump={} pro_a={:?} pro_b={:?} pro_tapjump={}",
            tag,
            mapping.gc_a, mapping.gc_b, mapping.gc_tapjump,
            mapping.pro_a, mapping.pro_b, mapping.pro_tapjump,
        ));
    }

    Some(mapping)
}

/// Saved Controller pointers per npad (0..7). Captured from FIM hook's
/// controller_struct.controller during CSS (when FIM fires for all player_idx).
/// These survive into training mode because Controller objects are persistent.
static CONTROLLER_PTRS: [AtomicUsize; 8] = [
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
];

/// Called from FIM hook (every call, including CSS) to save the Controller
/// pointer for a given npad. This builds our npad → Controller mapping.
pub fn save_controller_for_npad(npad: u32, controller_addr: usize) {
    if (npad as usize) < CONTROLLER_PTRS.len() {
        let old = CONTROLLER_PTRS[npad as usize].swap(controller_addr, Ordering::Relaxed);
        // One-shot diagnostic: log the first time each npad gets a Controller.
        if old == 0 && controller_addr != 0 {
            debug_log(&format!(
                "CTRL_SAVED: npad={} addr={:#x}",
                npad, controller_addr
            ));
        }
    }
}

/// Also save Controller by FIM player_idx. This lets us find a Controller
/// even when we don't know its npad (e.g., P3 during training mode when
/// the panel npad offset is unreliable).
static CONTROLLER_BY_PIDX: [AtomicUsize; 8] = [
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
];

/// Save Controller pointer by FIM player_idx.
pub fn save_controller_for_player_idx(player_idx: i32, controller_addr: usize) {
    if player_idx >= 0 && (player_idx as usize) < CONTROLLER_BY_PIDX.len() {
        CONTROLLER_BY_PIDX[player_idx as usize].store(controller_addr, Ordering::Relaxed);
    }
}

/// Returns the saved Controller pointer for a given npad, or 0 if not saved.
pub fn get_controller_for_npad(npad: i32) -> usize {
    if npad >= 0 && (npad as usize) < CONTROLLER_PTRS.len() {
        CONTROLLER_PTRS[npad as usize].load(Ordering::Relaxed)
    } else {
        0
    }
}

/// Fallback: find any saved Controller that isn't P1's (npad 0).
/// Used when CONTROLLER_PTRS[entry_npad] is 0, meaning the panel npad
/// was wrong or FIM never fired for that specific npad.
pub fn find_non_p1_controller() -> usize {
    let p1_addr = CONTROLLER_PTRS[0].load(Ordering::Relaxed);

    // First try CONTROLLER_BY_PIDX — CSS player_idx=1 is the second
    // connected controller, which is P3 in a 2-controller setup.
    for pidx in 1..CONTROLLER_BY_PIDX.len() {
        let addr = CONTROLLER_BY_PIDX[pidx].load(Ordering::Relaxed);
        if addr != 0 && addr != p1_addr {
            return addr;
        }
    }

    // Fallback: search CONTROLLER_PTRS for any non-P1 controller.
    for npad in 1..CONTROLLER_PTRS.len() {
        let addr = CONTROLLER_PTRS[npad].load(Ordering::Relaxed);
        if addr != 0 && addr != p1_addr {
            return addr;
        }
    }

    0
}

/// Saved ControllerMappings per npad (0..7). Indexed by hardware controller ID
/// so the mapping follows the physical controller regardless of which player_idx
/// or entry_id it's associated with. Captured from FIM hook during CSS (and
/// updated each FIM call) so the profile data is available during training mode.
static mut SAVED_CTRL_MAPPINGS: [[u8; 0x50]; 8] = [[0u8; 0x50]; 8];
static SAVED_CTRL_MAPPINGS_VALID: [AtomicBool; 8] = [
    AtomicBool::new(false), AtomicBool::new(false),
    AtomicBool::new(false), AtomicBool::new(false),
    AtomicBool::new(false), AtomicBool::new(false),
    AtomicBool::new(false), AtomicBool::new(false),
];

/// Save a ControllerMapping for a hardware npad from the FIM hook.
/// Called every FIM call (including CSS) so mappings are captured before
/// training mode potentially drops them for non-standard entries.
pub unsafe fn save_ctrl_mapping(npad: u32, mapping: *const ControllerMapping) {
    if (npad as usize) < 8 && !mapping.is_null() {
        core::ptr::copy_nonoverlapping(
            mapping as *const u8,
            SAVED_CTRL_MAPPINGS[npad as usize].as_mut_ptr(),
            0x50,
        );
        SAVED_CTRL_MAPPINGS_VALID[npad as usize].store(true, Ordering::Relaxed);
    }
}

/// Returns a pointer to the saved ControllerMapping for a given npad, or null
/// if not yet captured. Use the entry's npad (from get_human_entry_npad) to
/// look up the correct profile.
pub unsafe fn get_saved_ctrl_mapping(npad: i32) -> *const ControllerMapping {
    if npad >= 0 && (npad as usize) < 8
        && SAVED_CTRL_MAPPINGS_VALID[npad as usize].load(Ordering::Relaxed)
    {
        SAVED_CTRL_MAPPINGS[npad as usize].as_ptr() as *const ControllerMapping
    } else {
        core::ptr::null()
    }
}

/// Saved MappedInputs for each human entry, produced by calling FIM's
/// original function in the FIM hook for each human entry's controller.
static HUMAN_MAPPED_INPUTS: [RwLock<MappedInputs>; 4] = [
    RwLock::new(MappedInputs::empty()),
    RwLock::new(MappedInputs::empty()),
    RwLock::new(MappedInputs::empty()),
    RwLock::new(MappedInputs::empty()),
];

/// Tracks whether save_human_mapped_input was called this frame for each entry.
/// Distinguishes "player is idle (zero input)" from "FIM extra call didn't fire".
static HUMAN_INPUT_CAPTURED: [AtomicBool; 4] = [
    AtomicBool::new(false), AtomicBool::new(false),
    AtomicBool::new(false), AtomicBool::new(false),
];

/// Save a human entry's mapped input (called from FIM hook).
pub fn save_human_mapped_input(entry_id: i32, mapped: &MappedInputs) {
    if entry_id >= 0 && (entry_id as usize) < HUMAN_MAPPED_INPUTS.len() {
        assign(&HUMAN_MAPPED_INPUTS[entry_id as usize], *mapped);
        HUMAN_INPUT_CAPTURED[entry_id as usize].store(true, Ordering::Relaxed);
    }
}

/// Clear all saved mapped inputs (called at start of each frame from FIM hook).
pub fn clear_human_mapped_inputs() {
    for slot in &HUMAN_MAPPED_INPUTS {
        assign(slot, MappedInputs::empty());
    }
    for flag in &HUMAN_INPUT_CAPTURED {
        flag.store(false, Ordering::Relaxed);
    }
}

/// Called from set_cpu_controls AFTER call_original!.
/// Uses the per-frame call counter to identify the entry (Nth call = entry N).
/// For human entries: ALWAYS overrides AI output — injects saved FIM-produced
/// MappedInputs if captured, or zeros the CMI if not (so AI never autopilots).
/// For CPU entries: no change (AI output is kept).
/// Returns true if this is a human entry (caller should skip input_record).
pub unsafe fn inject_human_input(cmi: *mut ControlModuleInternal) -> bool {
    let entry_id = SET_CPU_CONTROLS_COUNTER.fetch_add(1, Ordering::Relaxed) + 1;

    if entry_id < 1 || entry_id > 3 {
        return false;
    }

    if !is_human_entry(entry_id) {
        return false;
    }

    use crate::training::input_record::{STICK_CLAMP_MULTIPLIER, STICK_NEUTRAL};

    if HUMAN_INPUT_CAPTURED[entry_id as usize].load(Ordering::Relaxed) {
        // FIM captured input this frame — inject it.
        let mapped = read(&HUMAN_MAPPED_INPUTS[entry_id as usize]);

        // Convert MappedInputs → CMI format (same conversion as input_record playback)
        (*cmi).buttons = mapped.buttons;
        (*cmi).stick_x = (mapped.lstick_x as f32) / (i8::MAX as f32);
        (*cmi).stick_y = (mapped.lstick_y as f32) / (i8::MAX as f32);

        let clamp_x = ((mapped.lstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
        let clamp_y = ((mapped.lstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
        (*cmi).clamped_lstick_x = if clamp_x.abs() >= STICK_NEUTRAL { clamp_x } else { 0.0 };
        (*cmi).clamped_lstick_y = if clamp_y.abs() >= STICK_NEUTRAL { clamp_y } else { 0.0 };

        let rclamp_x = ((mapped.rstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
        let rclamp_y = ((mapped.rstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
        (*cmi).clamped_rstick_x = if rclamp_x.abs() >= STICK_NEUTRAL { rclamp_x } else { 0.0 };
        (*cmi).clamped_rstick_y = if rclamp_y.abs() >= STICK_NEUTRAL { rclamp_y } else { 0.0 };
    } else {
        // No FIM capture this frame (e.g. Controller ptr not yet saved during startup).
        // Zero the CMI so the character stands still instead of AI autopiloting.
        (*cmi).buttons = Buttons::empty();
        (*cmi).stick_x = 0.0;
        (*cmi).stick_y = 0.0;
        (*cmi).clamped_lstick_x = 0.0;
        (*cmi).clamped_lstick_y = 0.0;
        (*cmi).clamped_rstick_x = 0.0;
        (*cmi).clamped_rstick_y = 0.0;
    }

    true
}

/// Capture the game's normal FIM output for human entries at player_idx > 0.
/// When the game fires FIM for these entries naturally (which happens when
/// config[0x78]=0, i.e. human), this overwrites any stale data from the
/// FIM extra calls (which may have used the wrong controller).
///
/// Called from the FIM hook right after original!() for player_idx > 0.
/// FIM's native output already has correct button mapping from internal state
/// (set up by clone_write via config[0x214]), so we do NOT apply
/// apply_profile_button_remap here — that would corrupt the correct output.
pub unsafe fn capture_native_fim_output(
    player_idx: i32,
    out: &MappedInputs,
    _controller_addr: usize,
) {
    if player_idx < 1 || player_idx > 3 {
        return;
    }
    if !is_human_entry(player_idx) {
        return;
    }

    let mut captured = *out;

    // Strip phantom FLICK_JUMP — same logic as FIM extra calls
    if captured.buttons.contains(Buttons::FLICK_JUMP) && captured.lstick_y < 56 {
        captured.buttons &= !Buttons::FLICK_JUMP;
    }

    // Diagnostic: log native FIM output periodically to compare with FIM extra call
    {
        static NATIVE_DIAG_CTR: AtomicU32 = AtomicU32::new(0);
        let ctr = NATIVE_DIAG_CTR.fetch_add(1, Ordering::Relaxed);
        if ctr < 10 || ctr % 120 == 0 {
            debug_log(&format!(
                "NATIVE_FIM: pidx={} btns={:#x} lstick=({},{})",
                player_idx, captured.buttons.bits(),
                captured.lstick_x, captured.lstick_y,
            ));
        }
    }

    save_human_mapped_input(player_idx, &captured);
}

// ---------------------------------------------------------------------------
// Profile button remapping for human entries
// ---------------------------------------------------------------------------

/// Converts an InputKind (from profile ControllerMapping) to the corresponding
/// Buttons flags. Includes _RAW variants where applicable so the game's
/// smash-input detection and hold logic work correctly.
fn input_kind_to_buttons(kind: InputKind) -> Buttons {
    match kind {
        InputKind::Attack => Buttons::ATTACK | Buttons::ATTACK_RAW,
        InputKind::Special => Buttons::SPECIAL | Buttons::SPECIAL_RAW | Buttons::SPECIAL_RAW2,
        InputKind::Jump => Buttons::JUMP,
        InputKind::Guard => Buttons::GUARD | Buttons::GUARD_HOLD,
        InputKind::Grab => Buttons::CATCH,
        InputKind::SmashAttack => Buttons::SMASH,
        InputKind::AppealHi => Buttons::APPEAL_HI,
        InputKind::AppealS => Buttons::APPEAL_SL,
        InputKind::AppealLw => Buttons::APPEAL_LW,
        InputKind::Unset => Buttons::empty(),
    }
}

/// Applies profile button remapping to FIM extra output for a human entry.
///
/// FIM's original produces default-mapped buttons (A→Attack, B→Special, etc.)
/// regardless of the player's profile. This function reads the raw hardware
/// button state from the Controller and rebuilds the logical button flags
/// using the entry's ControllerMapping from their profile.
///
/// Stick-derived flags (FLICK_JUMP, CSTICK_ON, JUMP_MINI) are preserved from
/// FIM's output since they don't depend on button remapping.
///
/// Also enforces the profile's tap-jump setting: if disabled, strips FLICK_JUMP.
pub unsafe fn apply_profile_button_remap(
    extra_out: &mut MappedInputs,
    controller: *const Controller,
    mapping: *const ControllerMapping,
) {
    if controller.is_null() || mapping.is_null() {
        return;
    }

    let raw = (*controller).current_buttons;
    let style = (*controller).style;
    let mapping = &*mapping;

    let mut buttons = Buttons::empty();
    let tapjump_enabled: bool;

    match style {
        ControllerStyle::GCController => {
            if raw.a() { buttons |= input_kind_to_buttons(mapping.gc_a); }
            if raw.b() { buttons |= input_kind_to_buttons(mapping.gc_b); }
            if raw.x() { buttons |= input_kind_to_buttons(mapping.gc_x); }
            if raw.y() { buttons |= input_kind_to_buttons(mapping.gc_y); }
            if raw.l() || raw.real_digital_l() {
                buttons |= input_kind_to_buttons(mapping.gc_l);
            }
            if raw.r() || raw.real_digital_r() {
                buttons |= input_kind_to_buttons(mapping.gc_r);
            }
            // GC Z button maps to ZR in the raw ButtonBitfield
            if raw.zl() || raw.zr() {
                buttons |= input_kind_to_buttons(mapping.gc_z);
            }
            if raw.dpad_up() { buttons |= input_kind_to_buttons(mapping.gc_dup); }
            if raw.dpad_down() { buttons |= input_kind_to_buttons(mapping.gc_ddown); }
            if raw.dpad_left() || raw.dpad_right() {
                buttons |= input_kind_to_buttons(mapping.gc_dlr);
            }
            tapjump_enabled = mapping.gc_tapjump;
        }
        _ => {
            // Pro Controller, Handheld, Dual Joycon, etc.
            if raw.a() { buttons |= input_kind_to_buttons(mapping.pro_a); }
            if raw.b() { buttons |= input_kind_to_buttons(mapping.pro_b); }
            if raw.x() { buttons |= input_kind_to_buttons(mapping.pro_x); }
            if raw.y() { buttons |= input_kind_to_buttons(mapping.pro_y); }
            if raw.l() { buttons |= input_kind_to_buttons(mapping.pro_l); }
            if raw.r() { buttons |= input_kind_to_buttons(mapping.pro_r); }
            if raw.zl() { buttons |= input_kind_to_buttons(mapping.pro_zl); }
            if raw.zr() { buttons |= input_kind_to_buttons(mapping.pro_zr); }
            if raw.dpad_up() { buttons |= input_kind_to_buttons(mapping.pro_dup); }
            if raw.dpad_down() { buttons |= input_kind_to_buttons(mapping.pro_ddown); }
            if raw.dpad_left() || raw.dpad_right() {
                buttons |= input_kind_to_buttons(mapping.pro_dlr);
            }
            tapjump_enabled = mapping.pro_tapjump;
        }
    }

    // Strip FLICK_JUMP if profile has tap-jump disabled
    if !tapjump_enabled {
        extra_out.buttons &= !Buttons::FLICK_JUMP;
    }

    // Preserve stick-derived flags from FIM's output (unaffected by button remap)
    let stick_flags = extra_out.buttons
        & (Buttons::FLICK_JUMP | Buttons::CSTICK_ON | Buttons::JUMP_MINI | Buttons::STOCK_SHARE);

    extra_out.buttons = buttons | stick_flags;
}

// ---------------------------------------------------------------------------
// BOMA address tracking (for diagnostic cross-referencing with p_data)
// ---------------------------------------------------------------------------

/// Saved BattleObjectModuleAccessor addresses per entry, for diagnostic
/// cross-referencing against set_cpu_controls p_data values.
static BOMA_FOR_ENTRY: [AtomicUsize; 4] = [
    AtomicUsize::new(0), AtomicUsize::new(0),
    AtomicUsize::new(0), AtomicUsize::new(0),
];

/// Called from once_per_frame_per_fighter to save each entry's module_accessor
/// address. This lets us match p_data values in set_cpu_controls against known
/// entry addresses.
pub unsafe fn track_boma_address(module_accessor: &mut BattleObjectModuleAccessor) {
    let entry_id =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID);
    if entry_id >= 0 && (entry_id as usize) < BOMA_FOR_ENTRY.len() {
        let addr = module_accessor as *mut _ as usize;
        let old = BOMA_FOR_ENTRY[entry_id as usize].swap(addr, Ordering::Relaxed);
        // Log once when addresses are first established.
        if old == 0 {
            debug_log(&format!(
                "BOMA: entry={} addr={:#x}",
                entry_id, addr
            ));
        }
    }
}

// ---------------------------------------------------------------------------
// Human/CPU entry tracking (set during clone_write, read by is_operation_cpu)
// ---------------------------------------------------------------------------

/// Tracks which entries were assigned as human at CSS.
/// Index = entry_id (0..3). Set by clone_write_hook, read by input_record's
/// set_cpu_controls hook to skip AI for human-controlled entries.
static CSS_ENTRY_IS_HUMAN: [AtomicBool; 4] = [
    AtomicBool::new(true),  // entry 0 (P1) always human
    AtomicBool::new(false), // entry 1
    AtomicBool::new(false), // entry 2
    AtomicBool::new(false), // entry 3
];

/// Reverse mapping: hardware npad → entry_id. Allows try_inject_human_input
/// to find the correct entry from the CMI's controller_index (which is the
/// hardware npad, not the entry_id). Index = npad (0..7), value = entry_id.
/// Set in clone_write_hook, -1 = unassigned.
static NPAD_TO_ENTRY: [AtomicI32; 8] = [
    AtomicI32::new(0),  // npad 0 → entry 0 (P1, default)
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];

/// Returns true if the given entry was set as human at CSS.
pub fn is_human_entry(entry_id: i32) -> bool {
    if entry_id >= 0 && (entry_id as usize) < CSS_ENTRY_IS_HUMAN.len() {
        CSS_ENTRY_IS_HUMAN[entry_id as usize].load(Ordering::Relaxed)
    } else {
        false
    }
}

// ---------------------------------------------------------------------------
// FIM npad tracking (hardware controller → entry mapping)
// ---------------------------------------------------------------------------

/// Tracks the hardware npad (controller index) that FIM associates with each
/// player_idx/entry. Updated every FIM call so it's available at clone_write
/// time. Index = player_idx (0..3), value = npad_number.
static FIM_NPAD_FOR_ENTRY: [AtomicI32; 4] = [
    AtomicI32::new(0),
    AtomicI32::new(-1),
    AtomicI32::new(-1),
    AtomicI32::new(-1),
];

/// Called from the FIM hook (BEFORE the is_training_mode check) to record
/// which hardware npad is being used for each player_idx. This runs during
/// CSS too, so the mapping is available when clone_write fires.
pub fn track_fim_npad(player_idx: i32, npad: u32) {
    if player_idx >= 0 && (player_idx as usize) < FIM_NPAD_FOR_ENTRY.len() {
        FIM_NPAD_FOR_ENTRY[player_idx as usize].store(npad as i32, Ordering::Relaxed);
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
/// config[0x88] with the correct hash read from CSS panel objects.
const OFFSET_CLONE_WRITE: usize = 0x1788260;

/// Offset of set_panel_type ($main + 0x1A028B0).
/// Signature: fn(panel: *mut u8, panel_type: i32)
/// Types: 0=human, 1=CPU, 2/3=disabled.
/// Properly transitions a CSS panel between states: updates is_cpu field (panel+0x1F8),
/// refreshes sub-objects (panel+0x5E8), triggers visual update (panel color/name),
/// and for type=0 (human) handles tag/profile setup via virtual call through panel+0x1B0.
/// Confirmed via GDB hardware watchpoint on panel+0x1F8 during manual CPU→human switch.
const OFFSET_SET_PANEL_TYPE: usize = 0x1A028B0;

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

/// Fighter kind derived from the CSS panel hash during clone_write.
/// For entries 2/3, the game's css_confirm doesn't properly track P3/P4's
/// selections for special characters (Pokemon Trainer returns 0 instead of
/// the correct kind). clone_write fires first and has access to the correct
/// panel hash, so we derive the fighter_kind here and use it as an override
/// in create_fighter_entry_hook.
/// -1 = not set (use CSS_CONFIRMED_KINDS as fallback).
static CLONE_WRITE_KINDS: [AtomicI32; 4] = [
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];

fn reset_css_confirmed_kinds() {
    for kind in &CSS_CONFIRMED_KINDS {
        kind.store(-1, Ordering::Relaxed);
    }
    for kind in &CLONE_WRITE_KINDS {
        kind.store(-1, Ordering::Relaxed);
    }
    for kind in &RANDOM_PICKED_KIND {
        kind.store(-1, Ordering::Relaxed);
    }
    // Reset RNG so it reseeds from new panel pointers on next CSS session.
    RNG_STATE.store(0, Ordering::Relaxed);
}

/// Extract the ui_chara db_index from a ui_chara hash.
/// Hash format: (0xC1 << 56) | (db_index << 40) | hash40("ui_chara_xxx")
/// Returns the db_index (typically 0-91), or -1 if the hash is invalid.
fn db_index_from_hash(hash: u64) -> i32 {
    if (hash & 0xFF00000000000000) != 0xC100000000000000 {
        return -1;
    }
    ((hash >> 40) & 0xFFFF) as i32
}

/// Derive the fighter_kind for character creation from a ui_chara db_index.
/// Returns -1 for most characters (css_confirm handles them correctly).
/// Returns -1 for special characters like Pokemon Trainer too — PT requires
/// special factory init_data beyond just the kind field, and writing PZENIGAME
/// into a cloned buffer crashes. PT support for P3/P4 needs a deeper approach.
fn fighter_kind_from_db_index(_db_index: i32) -> i32 {
    // TODO: Pokemon Trainer (db_index 38) can't be created by just setting
    // kind=PZENIGAME in cloned init_data — the factory needs PT-specific fields.
    // For now, return -1 and let css_confirm's value be used (shows as the clone
    // character's model, which is wrong but doesn't crash).
    -1
}

// ---------------------------------------------------------------------------
// Random character resolution for P3/P4
// ---------------------------------------------------------------------------
//
// The game resolves Random to a specific character for P1/P2 before css_confirm,
// but not for mod-added P3/P4 slots. We resolve it ourselves: detect Random via
// panel hash (db_index==0), pick from the valid roster, and override the kind in
// css_confirm so process_player_infos loads the correct resources.

/// Pre-picked random character kind, set by clone_write_hook (which fires first)
/// when it detects Random. css_confirm_hook reads this to override param_1+8
/// so process_player_infos loads the correct resources.
/// -1 = not Random / not picked.
static RANDOM_PICKED_KIND: [AtomicI32; 4] = [
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];

/// Construct a full ui_chara hash from db_index and hash40 lower 40 bits.
const fn ui_hash(db: u64, h40: u64) -> u64 {
    0xC100_0000_0000_0000 | (db << 40) | h40
}

/// All characters as (fighter_kind, ui_chara_hash) pairs.
/// fighter_kind = FIGHTER_KIND_* value from lua_const (NOT css_kind! They diverge after IC).
/// ui_hash db_index = portrait table position (shifted from out_xml by Nana/Mii insertions).
/// Each hash40 verified against out.xml dump from data.arc final patch.
/// Excludes: Random, Pokemon Trainer, Mii fighters, PT sub-fighters, bosses.
const VALID_RANDOM_POOL: &[(i32, u64)] = &[
    // fighter_kind 0-15 = css_kind 0-15 (identical)
    ( 0, ui_hash(  1, 0x0edaf3c863)), // Mario          FK=MARIO
    ( 1, ui_hash(  2, 0x0f5421de55)), // Donkey Kong    FK=DONKEY
    ( 2, ui_hash(  3, 0x0d22ccc98e)), // Link           FK=LINK
    ( 3, ui_hash(  4, 0x0ee02f04df)), // Samus          FK=SAMUS
    ( 4, ui_hash(  5, 0x0f8e51aa8d)), // Dark Samus     FK=SAMUSD
    ( 5, ui_hash(  6, 0x0e5ef67051)), // Yoshi          FK=YOSHI
    ( 6, ui_hash(  7, 0x0e872779b6)), // Kirby          FK=KIRBY
    ( 7, ui_hash(  8, 0x0c6eacd0fa)), // Fox            FK=FOX
    ( 8, ui_hash(  9, 0x105cf985ed)), // Pikachu        FK=PIKACHU
    ( 9, ui_hash( 10, 0x0e5f7be531)), // Luigi          FK=LUIGI
    (10, ui_hash( 11, 0x0d6ddf0c2b)), // Ness           FK=NESS
    (11, ui_hash( 12, 0x100691ac2e)), // Captain Falcon FK=CAPTAIN
    (12, ui_hash( 13, 0x0eeaff6b0e)), // Jigglypuff     FK=PURIN
    (13, ui_hash( 14, 0x0eb70a6c07)), // Peach          FK=PEACH
    (14, ui_hash( 15, 0x0e8369a909)), // Daisy          FK=DAISY
    (15, ui_hash( 16, 0x0edd2afecc)), // Bowser         FK=KOOPA
    // IC: css_kind=16, but FK=POPO=75
    (75, ui_hash( 17, 0x14c50faf14)), // Ice Climbers   FK=POPO(0x4B)
    // Post-IC: FK = css_kind - 1 (FIGHTER_KIND skips IC here, CSS doesn't)
    (16, ui_hash( 19, 0x0e662fdfe6)), // Sheik          FK=SHEIK(0x10)
    (17, ui_hash( 20, 0x0ec3ffc996)), // Zelda          FK=ZELDA(0x11)
    (18, ui_hash( 21, 0x0f4cbc89e6)), // Dr. Mario      FK=MARIOD(0x12)
    (19, ui_hash( 22, 0x0e7eaab2c3)), // Pichu          FK=PICHU(0x13)
    (20, ui_hash( 23, 0x0e41749f82)), // Falco          FK=FALCO(0x14)
    (21, ui_hash( 24, 0x0ebbfb31dc)), // Marth          FK=MARTH(0x15)
    (22, ui_hash( 25, 0x0f93549e35)), // Lucina         FK=LUCINA(0x16)
    (23, ui_hash( 26, 0x12d560cbe8)), // Young Link     FK=YOUNGLINK(0x17)
    (24, ui_hash( 27, 0x0ea4221dc6)), // Ganondorf      FK=GANON(0x18)
    (25, ui_hash( 28, 0x0f8fd5aee6)), // Mewtwo         FK=MEWTWO(0x19)
    (26, ui_hash( 29, 0x0c0284ebc0)), // Roy            FK=ROY(0x1A)
    (27, ui_hash( 30, 0x0ea09fff22)), // Chrom          FK=CHROM(0x1B)
    (28, ui_hash( 31, 0x1230cd32d8)), // Mr. Game&Watch FK=GAMEWATCH(0x1C)
    (29, ui_hash( 32, 0x1383197005)), // Meta Knight    FK=METAKNIGHT(0x1D)
    (30, ui_hash( 33, 0x0c29ebe495)), // Pit            FK=PIT(0x1E)
    (31, ui_hash( 34, 0x0df1f263d6)), // Dark Pit       FK=PITB(0x1F)
    (32, ui_hash( 35, 0x1288a0ed39)), // Zero Suit Samus FK=SZEROSUIT(0x20)
    (33, ui_hash( 36, 0x0ef0a34740)), // Wario          FK=WARIO(0x21)
    (34, ui_hash( 37, 0x0e91c36763)), // Snake          FK=SNAKE(0x22)
    (35, ui_hash( 38, 0x0c629a3e1a)), // Ike            FK=IKE(0x23)
    // skip: Pokemon Trainer + sub-fighters (FK 36-38 = PT subs)
    (39, ui_hash( 43, 0x0e4b869623)), // Diddy Kong     FK=DIDDY(0x27)
    (40, ui_hash( 44, 0x0ef9d43e1b)), // Lucas          FK=LUCAS(0x28)
    (41, ui_hash( 45, 0x0ef976808c)), // Sonic          FK=SONIC(0x29)
    (42, ui_hash( 46, 0x0f76a86694)), // King Dedede    FK=DEDEDE(0x2A)
    (43, ui_hash( 47, 0x0f5f132d33)), // Olimar         FK=PIKMIN(0x2B)
    (44, ui_hash( 48, 0x10417efb0a)), // Lucario        FK=LUCARIO(0x2C)
    (45, ui_hash( 49, 0x0e18857219)), // R.O.B.         FK=ROBOT(0x2D)
    (46, ui_hash( 50, 0x11d8496fe1)), // Toon Link      FK=TOONLINK(0x2E)
    (47, ui_hash( 51, 0x0dedde7b9d)), // Wolf           FK=WOLF(0x2F)
    (48, ui_hash( 52, 0x1112944904)), // Villager       FK=MURABITO(0x30)
    (49, ui_hash( 53, 0x1013cb83d3)), // Mega Man       FK=ROCKMAN(0x31)
    (50, ui_hash( 54, 0x0f1928c39b)), // Wii Fit Trainer FK=WIIFIT(0x32)
    (51, ui_hash( 55, 0x1027090018)), // Rosalina       FK=ROSETTA(0x33)
    (52, ui_hash( 56, 0x1275f0ada2)), // Little Mac     FK=LITTLEMAC(0x34)
    (53, ui_hash( 57, 0x11ebe26cac)), // Greninja       FK=GEKKOUGA(0x35)
    // skip: Mii fighters (FK 72-74, css_kind 55-57)
    // Post-Mii: FK = css_kind - 4
    (54, ui_hash( 61, 0x11e1faa171)), // Palutena       FK=PALUTENA(0x36)
    (55, ui_hash( 62, 0x0f620ec415)), // Pac-Man        FK=PACMAN(0x37)
    (56, ui_hash( 63, 0x0f755465a5)), // Robin          FK=REFLET(0x38)
    (57, ui_hash( 64, 0x0e077e88d3)), // Shulk          FK=SHULK(0x39)
    (58, ui_hash( 65, 0x105d8a1bb1)), // Bowser Jr.     FK=KOOPAJR(0x3A)
    (59, ui_hash( 66, 0x11cf2812f7)), // Duck Hunt      FK=DUCKHUNT(0x3B)
    (60, ui_hash( 67, 0x0c17aa123c)), // Ryu            FK=RYU(0x3C)
    (61, ui_hash( 68, 0x0c684f1e72)), // Ken            FK=KEN(0x3D)
    (62, ui_hash( 69, 0x0ef2f21a29)), // Cloud          FK=CLOUD(0x3E)
    (63, ui_hash( 70, 0x0e4ddd21e6)), // Corrin         FK=KAMUI(0x3F)
    (64, ui_hash( 71, 0x12d69db2ba)), // Bayonetta      FK=BAYONETTA(0x40)
    (65, ui_hash( 73, 0x10e9a4e78d)), // Inkling        FK=INKLING(0x41)
    (66, ui_hash( 74, 0x0f641c3c92)), // Ridley         FK=RIDLEY(0x42)
    (67, ui_hash( 76, 0x0ef6b0ba32)), // Simon          FK=SIMON(0x43)
    (68, ui_hash( 72, 0x10b4bdce94)), // Richter        FK=RICHTER(0x44)
    (69, ui_hash( 75, 0x0eccb203ad)), // King K. Rool   FK=KROOL(0x45)
    (70, ui_hash( 77, 0x0f26228f86)), // Isabelle       FK=SHIZUE(0x46)
    (71, ui_hash( 78, 0x10fe773f13)), // Incineroar     FK=GAOGAEN(0x47)
    // DLC: FK = css_kind + 5 (gap for Miis/Popo/Nana/Koopag/MiiEnemy in FIGHTER_KIND)
    (81, ui_hash(109, 0x0f482d6ff2)), // Piranha Plant  FK=PACKUN(0x51)
    (82, ui_hash(110, 0x0dbc1ab9a7)), // Joker          FK=JACK(0x52)
    (83, ui_hash(111, 0x0e29e05d6a)), // Hero           FK=BRAVE(0x53)
    (84, ui_hash(112, 0x0ede098ba4)), // Banjo&Kazooie  FK=BUDDY(0x54)
    (85, ui_hash(113, 0x0ea827124f)), // Terry          FK=DOLLY(0x55)
    (86, ui_hash(114, 0x0feec5837b)), // Byleth         FK=MASTER(0x56)
    (87, ui_hash(115, 0x0f1802c621)), // Min Min        FK=TANTAN(0x57)
    (88, ui_hash(116, 0x0fdc91574e)), // Steve          FK=PICKEL(0x58)
    (89, ui_hash(117, 0x0d61668319)), // Sephiroth      FK=EDGE(0x59)
    (90, ui_hash(119, 0x143829f67e)), // Pyra           FK=EFLAME(0x5A)
    (92, ui_hash(123, 0x0e6ea64e18)), // Kazuya         FK=DEMON(0x5C)
    (93, ui_hash(124, 0x0e72c68972)), // Sora           FK=TRAIL(0x5D)
];

/// Simple LCG state for random fighter selection.
static RNG_STATE: AtomicU64 = AtomicU64::new(0);

/// Pick a random (css_kind, ui_chara_hash) from VALID_RANDOM_POOL using LCG PRNG.
/// Seeded lazily from CSS panel pointer addresses (vary with ASLR/allocation)
/// so different CSS sessions produce different sequences.
fn pick_random_character() -> (i32, u64) {
    let mut state = RNG_STATE.load(Ordering::Relaxed);
    if state == 0 {
        // Seed from panel pointer addresses — these change every CSS session
        // due to heap allocation + ASLR, giving natural per-session variation.
        let p0 = CSS_PANEL_PTRS[0].load(Ordering::Relaxed) as u64;
        let p1 = CSS_PANEL_PTRS[1].load(Ordering::Relaxed) as u64;
        state = p0.wrapping_mul(2654435761) ^ p1.wrapping_mul(40503) ^ 0xDEAD_BEEF;
        if state == 0 {
            state = 0xCAFE_BABE;
        }
    }
    state = state
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1_442_695_040_888_963_407);
    RNG_STATE.store(state, Ordering::Relaxed);
    let index = ((state >> 33) as usize) % VALID_RANDOM_POOL.len();
    VALID_RANDOM_POOL[index]
}

/// Check if the CSS panel for `player_index` currently shows Random (db_index 0).
unsafe fn is_panel_random(player_index: usize) -> bool {
    if player_index >= CSS_PANEL_PTRS.len() {
        return false;
    }
    let panel = CSS_PANEL_PTRS[player_index].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        return false;
    }
    let hash = core::ptr::read_volatile(panel.add(0x200) as *const u64);
    db_index_from_hash(hash) == 0
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
static CSS_TRAINING_SCENE_PTR: AtomicUsize = AtomicUsize::new(0);

/// Cached panel object pointers, read from scene+0x250 vector during CSS setup.
/// The panel objects outlive the vector (which gets .clear()'d during transition),
/// so clone_write_hook can still read panel+0x200 via these cached pointers.
static CSS_PANEL_PTRS: [AtomicUsize; 8] = [
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
];

/// Saved CSS state for persistence across training ↔ CSS transitions.
/// Captured during clone_write (training transition), restored on CSS re-entry.
static SAVED_CSS_SLOT_COUNT: AtomicUsize = AtomicUsize::new(0);
static SAVED_CSS_HASH: [AtomicU64; 4] = [
    AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0),
];
static SAVED_CSS_IS_CPU: [AtomicI32; 4] = [
    AtomicI32::new(0), AtomicI32::new(0), AtomicI32::new(0), AtomicI32::new(0),
];
static SAVED_CSS_TAG: [AtomicI32; 4] = [
    AtomicI32::new(0), AtomicI32::new(0), AtomicI32::new(0), AtomicI32::new(0),
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
}

#[skyline::hook(offset = OFFSET_CSS_PANEL_LAYOUT)]
pub unsafe fn css_panel_layout_hook(scene: *mut u8, slot_count: u32, arg2: u32) {
    if !scene.is_null() {
        let mode_ptr = scene.add(0x16C) as *mut u32;
        let mode = core::ptr::read_volatile(mode_ptr);
        let visible = core::ptr::read_volatile(scene.add(0x160) as *const u32);
        // Recognize training CSS: either mode is still 0xB (first call) or we
        // already changed it to 0x6 on a previous call for the same scene.
        let is_training = mode == 0xB
            || CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed) == scene as usize;
        if is_training {
            CSS_TRAINING_SCENE_PTR.store(scene as usize, Ordering::Relaxed);
            // Use saved slot count from previous training session if available,
            // otherwise fall back to visible count (from connected controllers).
            let saved_slots = SAVED_CSS_SLOT_COUNT.load(Ordering::Relaxed) as u32;
            let actual_slots = visible.max(2).max(saved_slots);
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
///
/// For P3/P4 with Random selected: the game doesn't resolve Random for mod-added slots,
/// so css_confirm receives kind=0 (Mario). We detect Random via the CSS panel hash,
/// pick a truly random fighter_kind, and override param_1+8 before calling original.
/// This causes process_player_infos to load the random character's resources.
#[skyline::hook(offset = OFFSET_CSS_CONFIRM)]
pub unsafe fn css_confirm_hook(param_1: *mut u8, param_2: *mut u8) {
    let player_index = core::ptr::read_volatile(param_1.add(0xa7)) as usize;
    let mut fighter_kind = core::ptr::read_volatile(param_1.add(8) as *const i8) as i32;

    // Resolve Random for P3/P4: clone_write fires BEFORE css_confirm and pre-picks
    // the random character (storing kind in RANDOM_PICKED_KIND + writing hash to .bss).
    // Here we just read the pre-picked kind and override param_1+8 so
    // process_player_infos loads the correct character's resources.
    if player_index < 4 {
        let picked = RANDOM_PICKED_KIND[player_index].load(Ordering::Relaxed);
        if picked >= 0 {
            debug_log(&format!(
                "css_confirm: player_index={} RANDOM → using pre-picked kind={}",
                player_index, picked
            ));
            fighter_kind = picked;
            core::ptr::write_volatile(param_1.add(8) as *mut i8, picked as i8);
        }
    }

    debug_log(&format!(
        "css_confirm: player_index={} fighter_kind={} (0x{:x})",
        player_index, fighter_kind, fighter_kind
    ));

    if player_index < 8 {
        CSS_CONFIRMED_KINDS[player_index].store(fighter_kind, Ordering::Relaxed);
    }

    call_original!(param_1, param_2)
}

/// Hook for create_fighter_entry: override fighter_kind for entries 2/3 with CSS picks.
/// Training mode always creates 4 entries (0-3), cloning CPU1's kind for 2/3.
/// This hook replaces the cloned kind with what P3/P4 actually picked at CSS.
///
/// Priority for determining fighter_kind:
///   1. CLONE_WRITE_KINDS — derived from CSS panel hash in clone_write_hook.
///      Handles special chars (Pokemon Trainer) where css_confirm returns wrong data.
///   2. CSS_CONFIRMED_KINDS — from css_confirm_hook. Works for most characters.
///   3. No override — keep the cloned kind from CPU1.
#[skyline::hook(offset = OFFSET_CREATE_FIGHTER_ENTRY)]
pub unsafe fn create_fighter_entry_hook(inner: *mut u8, entry_id: u32, init_data: *mut u8) {
    if !init_data.is_null() && (entry_id == 2 || entry_id == 3) {
        let original_kind = core::ptr::read_volatile(init_data.add(0x18) as *const i32);

        // Prefer clone_write's derivation (from panel hash) for special characters.
        let cw_kind = CLONE_WRITE_KINDS[entry_id as usize].load(Ordering::Relaxed);
        let css_kind = CSS_CONFIRMED_KINDS[entry_id as usize].load(Ordering::Relaxed);

        let creation_kind = if cw_kind >= 0 {
            cw_kind // Pokemon Trainer etc. — clone_write derived the correct kind
        } else if css_kind >= 0 {
            css_kind // Normal characters — css_confirm is correct
        } else {
            -1
        };

        if creation_kind >= 0 {
            debug_log(&format!(
                "create_fighter_entry: entry={} original={} cw_kind={} css_kind={} → creation={}",
                entry_id, original_kind, cw_kind, css_kind, creation_kind
            ));
            core::ptr::write_volatile(init_data.add(0x18) as *mut i32, creation_kind);
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
    call_original!(css_list_head, sel);
}

/// Hook for FUN_71017e88d0: resource-path builder, called immediately before
/// FUN_71002c9900 in FUN_710064f820's per-entry Lua AI init block.
///
/// When type_id == 0x13 (AI param path), this is the last call before Lua AI init.
/// If the fighter kind is not one of the CSS-selected characters, its NSS module is
/// not loaded and FUN_71002c9900 will crash on a null GOT trampoline. We build a
/// whitelist from P1/CPU1 (via FighterManager) plus all CSS-confirmed kinds (P3/P4).
/// Pokemon Trainer selections expand to include all three Pokemon kinds.
///
/// NOTE: TRAINING_MENU_ADDR is null during training mode load (set only by stale_handle,
/// which fires later). Use FIGHTER_MANAGER_ADDR instead — it is set at mod startup via
/// LookupSymbol and P1/CPU1 FighterEntries are initialized before FUN_710064f820 runs.
#[skyline::hook(offset = OFFSET_LUA_AI_PATH_BUILDER)]
pub unsafe fn lua_ai_path_hook(out: *mut u8, module_ptr: *mut u8, kind: u32, type_id: u32) {
    if type_id == 0x13 {
        if !is_kind_resource_loaded(kind as i32) {
            SKIP_NEXT_LUA_AI_INIT.store(true, Ordering::Relaxed);
            return;
        }
    }
    call_original!(out, module_ptr, kind, type_id)
}

/// Check whether a fighter kind's resources are loaded (CSS-selected or derived).
/// Returns true if the kind matches any CSS-confirmed character or P1/CPU1,
/// including Pokemon Trainer's three sub-Pokemon kinds.
unsafe fn is_kind_resource_loaded(kind: i32) -> bool {
    // P1 and CPU1 from FighterManager (always loaded).
    let (p1_kind, cpu1_kind) = css_kinds_from_fighter_manager();
    if kind == p1_kind || kind == cpu1_kind {
        return true;
    }
    // Check all CSS-confirmed kinds (P3/P4 and any others).
    for slot_kind_atom in &CSS_CONFIRMED_KINDS {
        let slot_kind = slot_kind_atom.load(Ordering::Relaxed);
        if slot_kind < 0 {
            continue;
        }
        if kind == slot_kind {
            return true;
        }
        // Pokemon Trainer: CSS confirms kind=114 (PTRAINER), but the actual
        // fighters are Squirtle/Ivysaur/Charizard. process_player_infos loads
        // all three Pokemon's resources when PT is confirmed.
        if slot_kind == *FIGHTER_KIND_PTRAINER
            && (kind == *FIGHTER_KIND_PZENIGAME
                || kind == *FIGHTER_KIND_PFUSHIGISOU
                || kind == *FIGHTER_KIND_PLIZARDON)
        {
            return true;
        }
    }
    // Also check clone_write-derived kinds (for entries 2/3 where css_confirm
    // may have returned wrong data, e.g. Pokemon Trainer).
    for cw_kind_atom in &CLONE_WRITE_KINDS {
        let cw_kind = cw_kind_atom.load(Ordering::Relaxed);
        if cw_kind < 0 {
            continue;
        }
        if kind == cw_kind {
            return true;
        }
        // If clone_write derived PZENIGAME from a PT selection, also allow
        // the other two Pokemon kinds (Ivysaur, Charizard).
        if cw_kind == *FIGHTER_KIND_PZENIGAME
            && (kind == *FIGHTER_KIND_PFUSHIGISOU || kind == *FIGHTER_KIND_PLIZARDON)
        {
            return true;
        }
    }
    false
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

/// Hook for FUN_7101788260 (clone_write): override config fields for entries 2/3
/// so they get the correct character, player type, and controller binding instead
/// of CPU1's cloned values.
///
/// The training mode transition builds ONE config buffer from CPU1's data and calls
/// clone_write 4× for entries 0, 1, 2, 3. We override:
///   - config[0x88] (ui_chara hash)  — from CSS panel+0x200
///   - config[0x78] (player type)    — 0=human, 1=CPU; from CSS panel+0x1F8
///   - config[0x7C] (npad/controller)— hardware npad for human, -1 for CPU
///
/// Panel access: scene+0x250 is a std::vector of (vtable, panel_ptr) pairs (0x10 each).
#[skyline::hook(offset = OFFSET_CLONE_WRITE)]
pub unsafe fn clone_write_hook(config: *mut u8, entry_index: u32, byte_flag: u8, bss_out: *mut u8) {
    // On the first clone_write call (entry 0), snapshot all panel state so we
    // can restore it when the user returns to CSS from training mode.
    if entry_index == 0 {
        save_css_state_for_reentry();
        // Reset npad→entry mapping. Entry 0 always uses npad 0.
        for i in 0..8 {
            NPAD_TO_ENTRY[i].store(-1, Ordering::Relaxed);
        }
        NPAD_TO_ENTRY[0].store(0, Ordering::Relaxed);
        // Reset human entry npad tracking.
        for slot in &HUMAN_ENTRY_NPAD {
            slot.store(-1, Ordering::Relaxed);
        }
        // Reset tag/profile tracking.
        for slot in &HUMAN_ENTRY_TAG {
            slot.store(0, Ordering::Relaxed);
        }
    }

    // Track human/CPU status for entry 1 from config[0x78].
    // Entry 0 is always human (default). Entries 2/3 are tracked inside
    // their override block below (config has CPU1's data here, not theirs).
    if entry_index == 1 && !config.is_null() {
        let config_type = core::ptr::read_volatile(config.add(0x78) as *const i32);
        let is_entry1_human = config_type == 0;
        CSS_ENTRY_IS_HUMAN[1].store(is_entry1_human, Ordering::Relaxed);
        // Save tag/profile index for entry 1 — the game populates config[0x214] correctly.
        let entry1_tag = core::ptr::read_volatile(config.add(0x214) as *const u32);
        HUMAN_ENTRY_TAG[1].store(entry1_tag, Ordering::Relaxed);

        if is_entry1_human {
            // config[0x7C] is correct for entry 1 — the game populates P2's
            // npad properly (unlike entries 2/3 which are cloned from CPU1).
            let entry1_npad = core::ptr::read_volatile(config.add(0x7C) as *const i32);
            HUMAN_ENTRY_NPAD[1].store(entry1_npad, Ordering::Relaxed);
            if entry1_npad >= 0 && entry1_npad < 8 {
                NPAD_TO_ENTRY[entry1_npad as usize].store(1, Ordering::Relaxed);
            }
            debug_log(&format!(
                "clone_write: entry=1 HUMAN npad={} (from config[0x7C])",
                entry1_npad
            ));
        } else {
            // CPU P2: force npad to -1 so its CMI controller_index won't
            // collide with a human entry that uses the same hardware npad.
            HUMAN_ENTRY_NPAD[1].store(-1, Ordering::Relaxed);
            core::ptr::write_volatile(config.add(0x7C) as *mut i32, -1);
        }
    }

    // Diagnostic: log P2's native hash so we can see what db the game uses
    if entry_index == 1 && !config.is_null() {
        let native_hash = core::ptr::read_volatile(config.add(0x88) as *const u64);
        let native_db = (native_hash >> 40) & 0xFFFF;
        debug_log(&format!(
            "clone_write: entry=1 NATIVE hash={:#018x} db={}",
            native_hash, native_db
        ));
    }

    if (entry_index == 2 || entry_index == 3) && !config.is_null() {
        // Save original values so subsequent calls see CPU1's data.
        let saved_hash = core::ptr::read_volatile(config.add(0x88) as *const u64);
        let saved_type = core::ptr::read_volatile(config.add(0x78) as *const i32);
        let saved_npad = core::ptr::read_volatile(config.add(0x7C) as *const i32);

        // Read CSS panel hash to determine the selected character.
        let panel_hash = read_css_panel_hash(entry_index);
        let db_idx = if panel_hash != 0 { db_index_from_hash(panel_hash) } else { -1 };

        // Random (db_index 0): pick a random character and write its hash to config
        // so the HUD portrait loads correctly. Also store the kind for css_confirm.
        let is_random = db_idx == 0;
        if is_random {
            let (kind, hash) = pick_random_character();
            RANDOM_PICKED_KIND[entry_index as usize].store(kind, Ordering::Relaxed);
            core::ptr::write_volatile(config.add(0x88) as *mut u64, hash);
            let written_db = (hash >> 40) & 0xFFFF;
            debug_log(&format!(
                "clone_write: entry={} RANDOM(v5) → fk={} db={} hash={:#018x}",
                entry_index, kind, written_db, hash
            ));
        } else if panel_hash != 0 {
            // Normal character: override hash from CSS panel.
            core::ptr::write_volatile(config.add(0x88) as *mut u64, panel_hash);
            // Derive fighter_kind from the hash for special characters where
            // css_confirm returns the wrong value (e.g. Pokemon Trainer → 0).
            let derived_kind = fighter_kind_from_db_index(db_idx);
            if derived_kind >= 0 {
                CLONE_WRITE_KINDS[entry_index as usize].store(derived_kind, Ordering::Relaxed);
            }
        }

        // Override player type and npad from the CSS panel's human/CPU flag.
        let is_cpu = read_css_panel_is_cpu(entry_index);
        CSS_ENTRY_IS_HUMAN[entry_index as usize].store(!is_cpu, Ordering::Relaxed);
        if is_cpu {
            core::ptr::write_volatile(config.add(0x78) as *mut i32, 1); // CPU
            core::ptr::write_volatile(config.add(0x7C) as *mut i32, -1); // no controller
            HUMAN_ENTRY_NPAD[entry_index as usize].store(-1, Ordering::Relaxed);
        } else {
            core::ptr::write_volatile(config.add(0x78) as *mut i32, 0); // human
            // Read the hardware npad from CSS panel or fall back to FIM-tracked npad.
            let panel_npad = read_css_panel_npad(entry_index);
            let fim_npad = FIM_NPAD_FOR_ENTRY[entry_index as usize].load(Ordering::Relaxed);
            let npad = if panel_npad >= 0 {
                panel_npad
            } else if fim_npad >= 0 {
                fim_npad
            } else {
                entry_index as i32 // last resort fallback
            };
            core::ptr::write_volatile(config.add(0x7C) as *mut i32, npad);
            HUMAN_ENTRY_NPAD[entry_index as usize].store(npad, Ordering::Relaxed);
            if npad >= 0 && npad < 8 {
                NPAD_TO_ENTRY[npad as usize].store(entry_index as i32, Ordering::Relaxed);
            }
            debug_log(&format!(
                "clone_write: entry={} HUMAN npad={} (panel={} fim={})",
                entry_index, npad, panel_npad, fim_npad
            ));
        }

        // Override costume index so entries 2/3 get their CSS-selected costume
        // instead of CPU1's. config[0x90] = costume index (u8, 0-7 typically).
        // Panel+0x210 stores the costume index set by css_panel_set_chara_hash.
        //
        // Only override when a character was selected from CSS (panel_hash != 0).
        // When deactivated (panel_hash == 0), vanilla clones from P2 with
        // deduplicated costumes — overriding would break that.
        let saved_costume = core::ptr::read_volatile(config.add(0x90) as *const u8);
        if panel_hash != 0 {
            let panel_costume = read_css_panel_costume(entry_index);
            core::ptr::write_volatile(config.add(0x90) as *mut u8, panel_costume);
        }

        // Override tag/profile index so clone_write loads the correct button
        // mappings from the player's tag instead of CPU1's.
        // config[0x214] = tag index (used to look up profile at base + idx * 0xf7d8).
        let saved_tag = core::ptr::read_volatile(config.add(0x214) as *const u32);
        let panel_tag = read_css_panel_tag(entry_index);
        if !is_cpu {
            core::ptr::write_volatile(config.add(0x214) as *mut u32, panel_tag);
            // Save tag for later profile lookup in FIM extra calls.
            HUMAN_ENTRY_TAG[entry_index as usize].store(panel_tag, Ordering::Relaxed);
        }

        debug_log(&format!(
            "clone_write: entry={} is_cpu={} hash={:#018x} costume={} tag={}",
            entry_index, is_cpu,
            if panel_hash != 0 { panel_hash } else { saved_hash },
            core::ptr::read_volatile(config.add(0x90) as *const u8), panel_tag
        ));

        call_original!(config, entry_index, byte_flag, bss_out);

        // Restore so the next call sees the original CPU1 values.
        core::ptr::write_volatile(config.add(0x88) as *mut u64, saved_hash);
        core::ptr::write_volatile(config.add(0x78) as *mut i32, saved_type);
        core::ptr::write_volatile(config.add(0x7C) as *mut i32, saved_npad);
        core::ptr::write_volatile(config.add(0x90) as *mut u8, saved_costume);
        core::ptr::write_volatile(config.add(0x214) as *mut u32, saved_tag);
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

/// Read the human/CPU flag from the cached CSS panel for `entry_index`.
/// panel+0x1F8: u32, 0 = human, 1 = CPU (confirmed via GDB).
/// Returns true if CPU, false if human or on any failure (default to human).
unsafe fn read_css_panel_is_cpu(entry_index: u32) -> bool {
    if entry_index as usize >= CSS_PANEL_PTRS.len() {
        return false;
    }
    let panel = CSS_PANEL_PTRS[entry_index as usize].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        return false;
    }
    let flag = core::ptr::read_volatile(panel.add(0x1F8) as *const u32);
    flag != 0
}

/// Read the tag/profile index from the cached CSS panel for `entry_index`.
/// panel+0x394: u32 tag index (0-60). Used by clone_write to look up button
/// mappings from the profile at DAT_7105313510 → base + tag * 0xf7d8.
/// Returns 0 (default tag) on failure.
unsafe fn read_css_panel_tag(entry_index: u32) -> u32 {
    if entry_index as usize >= CSS_PANEL_PTRS.len() {
        return 0;
    }
    let panel = CSS_PANEL_PTRS[entry_index as usize].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        return 0;
    }
    let tag = core::ptr::read_volatile(panel.add(0x394) as *const u32);
    // Sanity check: tag index should be < 61 (0x3D).
    if tag < 0x3D {
        tag
    } else {
        0
    }
}

/// Read the costume index from the cached CSS panel for `entry_index`.
/// panel+0x210: u8 costume index (0-7), set by css_panel_set_chara_hash.
/// Returns 0 (default costume) on failure.
unsafe fn read_css_panel_costume(entry_index: u32) -> u8 {
    if entry_index as usize >= CSS_PANEL_PTRS.len() {
        return 0;
    }
    let panel = CSS_PANEL_PTRS[entry_index as usize].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        return 0;
    }
    core::ptr::read_volatile(panel.add(0x210))
}

/// Read the hardware npad from the cached CSS panel for `entry_index`.
/// Scans several candidate offsets since the exact field is unconfirmed.
/// Returns the npad (>= 0) if found, or -1 on failure.
unsafe fn read_css_panel_npad(entry_index: u32) -> i32 {
    if entry_index as usize >= CSS_PANEL_PTRS.len() {
        return -1;
    }
    let panel = CSS_PANEL_PTRS[entry_index as usize].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        return -1;
    }

    // Diagnostic: dump candidate panel offsets that might store npad.
    // panel+0x1F8 = is_cpu (known), so npad might be nearby.
    let v_1f4 = core::ptr::read_volatile(panel.add(0x1F4) as *const i32);
    let v_1fc = core::ptr::read_volatile(panel.add(0x1FC) as *const i32);
    let v_208 = core::ptr::read_volatile(panel.add(0x208) as *const i32);
    let v_20c = core::ptr::read_volatile(panel.add(0x20C) as *const i32);
    let v_210 = core::ptr::read_volatile(panel.add(0x210) as *const i32);
    let v_1f0 = core::ptr::read_volatile(panel.add(0x1F0) as *const i32);
    debug_log(&format!(
        "panel_npad_scan: entry={} +0x1F0={} +0x1F4={} +0x1FC={} +0x208={} +0x20C={} +0x210={}",
        entry_index, v_1f0, v_1f4, v_1fc, v_208, v_20c, v_210
    ));

    // Best guess: panel+0x1FC (right after is_cpu at +0x1F8).
    // Accept if it's a small non-negative integer (valid npad range 0..7).
    if v_1fc >= 0 && v_1fc < 8 {
        return v_1fc;
    }

    -1
}

/// Save all 4 panels' state so css_panel_layout_hook can restore it on re-entry.
/// Called once per training transition (from clone_write_hook, entry 0).
unsafe fn save_css_state_for_reentry() {
    let mut count = 0usize;
    for i in 0..4usize {
        let panel = CSS_PANEL_PTRS[i].load(Ordering::Relaxed) as *const u8;
        if panel.is_null() {
            continue;
        }
        count = i + 1;
        SAVED_CSS_HASH[i].store(
            core::ptr::read_volatile(panel.add(0x200) as *const u64),
            Ordering::Relaxed,
        );
        SAVED_CSS_IS_CPU[i].store(
            core::ptr::read_volatile(panel.add(0x1F8) as *const i32),
            Ordering::Relaxed,
        );
        SAVED_CSS_TAG[i].store(
            core::ptr::read_volatile(panel.add(0x394) as *const i32),
            Ordering::Relaxed,
        );
    }
    SAVED_CSS_SLOT_COUNT.store(count, Ordering::Relaxed);
    debug_log(&format!("save_css_state: {} slots saved", count));
}


