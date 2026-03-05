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

use skyline::nn::ui2d::{Material, MaterialFlags, MaterialColorType, Pane, ResColor};
use smash::app::{lua_bind::*, BattleObjectModuleAccessor};
use smash::lib::lua_const::*;
use smash::ui2d::{SmashPane, SmashTextBox};
use training_mod_sync::*;

// ---------------------------------------------------------------------------
// nn::hid constants for NpadFullKeyState button bitfield
// ---------------------------------------------------------------------------
const HID_X: u64 = 1 << 2;
const HID_ZR: u64 = 1 << 9;
const HID_DPAD_LEFT: u64 = 1 << 12;
const HID_DPAD_UP: u64 = 1 << 13;
const HID_DPAD_RIGHT: u64 = 1 << 14;
const HID_DPAD_DOWN: u64 = 1 << 15;

use crate::common::{FIGHTER_MANAGER_ADDR, MENU};

// ---------------------------------------------------------------------------
// Native FIM dispatch: BSS offsets and cached input manager pointer
// ---------------------------------------------------------------------------

/// BSS offset: global controller pointer table (10 entries, 8 bytes each).
/// The FIM dispatch loop reads controller ptrs from this table using the slot
/// index stored at input_mgr + 0x298 + player_idx * 4.
const CONTROLLER_TABLE_BSS: usize = 0x5338860;

/// BSS offset: controller system flag struct. The byte at +8 controls whether
/// the FIM dispatch loop uses per-slot controller lookup (nonzero) or a single
/// default controller for all entries (zero).
const CONTROLLER_FLAG_BSS: usize = 0x53388b0;

/// Cached pointer to the input manager struct (computed from FIM mappings arg - 0x18).
/// Used by inject_human_input to read native FIM output from the output array.
static INPUT_MGR_PTR: AtomicUsize = AtomicUsize::new(0);

/// Save the input manager pointer (called from FIM hook at player_idx==0).
pub fn save_input_mgr_ptr(ptr: usize) {
    INPUT_MGR_PTR.store(ptr, Ordering::Relaxed);
}

/// Set up the input manager's controller slot + mappings arrays so the FIM
/// dispatch loop fires natively for human entries 2/3.
///
/// Called once per frame from FIM hook at player_idx==0 (before the dispatch
/// loop reaches entries 2/3, since it processes sequentially).
///
/// Two-pass assignment:
///   Pass 1 — entries with a reliable npad (!= P1's): match to table entry by
///            reading npad_number from each Controller (offset 0xc4).
///   Pass 2 — entries with unreliable npad (== P1's): assign remaining slots.
///
/// This prevents wrong controller-type assignment when GC and Pro controllers
/// are mixed (the table scan order doesn't necessarily match entry order).
pub unsafe fn setup_native_fim_for_humans(
    input_mgr: usize,
    _mappings: *mut ControllerMapping,
) {
    use skyline::hooks::{getRegionAddress, Region};
    let text_base = getRegionAddress(Region::Text) as usize;
    let ctrl_table = (text_base + CONTROLLER_TABLE_BSS) as *const usize;

    static SETUP_LOG_COUNT: AtomicUsize = AtomicUsize::new(0);

    // Ensure the per-controller lookup flag is enabled (byte at flag_struct + 8).
    // When this is 0, the dispatch loop uses a single default controller for all
    // entries instead of looking up per-slot — we need per-slot mode.
    let flag_ptr = (text_base + CONTROLLER_FLAG_BSS + 8) as *mut u8;
    if core::ptr::read_volatile(flag_ptr) == 0 {
        core::ptr::write_volatile(flag_ptr, 1);
        debug_log("NATIVE_FIM_SETUP: enabled per-controller flag");
    }

    // --- Identify P1's controller and npad ---
    let p1_slot = core::ptr::read_volatile((input_mgr + 0x298) as *const i32);
    let p1_ctrl: usize = if p1_slot >= 0 && p1_slot < 10 {
        let some_ctrl = core::ptr::read_volatile(ctrl_table.add(p1_slot as usize));
        if some_ctrl != 0 {
            core::ptr::read_volatile((some_ctrl + 0x10) as *const usize)
        } else {
            0
        }
    } else {
        0
    };
    let p1_npad = HUMAN_ENTRY_NPAD[0].load(Ordering::Relaxed); // P1's npad (usually 0)

    // --- Scan table: build list of connected non-P1 controllers with npad_number ---
    // Each entry: (slot, controller_addr, npad_number)
    let mut avail_slot: [i32; 10] = [-1; 10];
    let mut avail_ctrl: [usize; 10] = [0; 10];
    let mut avail_npad: [i32; 10] = [-1; 10];
    let mut avail_used: [bool; 10] = [false; 10]; // tracks assignment
    let mut num_avail: usize = 0;

    for slot in 0..10i32 {
        if slot == p1_slot {
            continue;
        }
        let some_ctrl = core::ptr::read_volatile(ctrl_table.add(slot as usize));
        if some_ctrl == 0 {
            continue;
        }
        let controller = core::ptr::read_volatile((some_ctrl + 0x10) as *const usize);
        if controller == 0 || controller == p1_ctrl {
            continue;
        }
        // Check connected/valid flag at Controller + 0xb8
        let connected = core::ptr::read_volatile((controller + 0xb8) as *const u8);
        if connected == 0 {
            continue;
        }
        // Read npad_number at Controller + 0xc4
        let npad_num = core::ptr::read_volatile((controller + 0xc4) as *const u32) as i32;

        avail_slot[num_avail] = slot;
        avail_ctrl[num_avail] = controller;
        avail_npad[num_avail] = npad_num;
        num_avail += 1;
    }

    // --- Collect human entries ---
    let mut human_entries: [i32; 3] = [-1; 3];
    let mut num_humans: usize = 0;
    for entry_id in 1i32..=3 {
        if is_human_entry(entry_id) {
            human_entries[num_humans] = entry_id;
            num_humans += 1;
        }
    }

    // --- Per-entry assignment results ---
    let mut entry_assigned: [i32; 4] = [-1; 4]; // indexed by entry_id

    // --- Pass 1: entries with a RELIABLE npad (different from P1's) ---
    // Match by npad_number from the table. This ensures the correct physical
    // controller (and thus correct controller type for GC vs Pro) is assigned.
    for i in 0..num_humans {
        let entry_id = human_entries[i];
        let npad = HUMAN_ENTRY_NPAD[entry_id as usize].load(Ordering::Relaxed);
        if npad < 0 || npad == p1_npad {
            continue; // unreliable — defer to pass 2
        }
        // Find the table entry whose Controller has npad_number == npad
        for j in 0..num_avail {
            if avail_used[j] {
                continue;
            }
            if avail_npad[j] == npad {
                entry_assigned[entry_id as usize] = avail_slot[j] as i32;
                avail_used[j] = true;
                // Update CONTROLLER_PTRS so downstream code has the right address
                if (npad as usize) < CONTROLLER_PTRS.len() {
                    CONTROLLER_PTRS[npad as usize].store(avail_ctrl[j], Ordering::Relaxed);
                }
                break;
            }
        }
    }

    // --- Pass 2: entries with UNRELIABLE npad (== P1's or unknown) ---
    // Assign from remaining (unused) available slots.
    for i in 0..num_humans {
        let entry_id = human_entries[i];
        if entry_assigned[entry_id as usize] >= 0 {
            continue; // already assigned in pass 1
        }
        // Find next unused available slot
        for j in 0..num_avail {
            if avail_used[j] {
                continue;
            }
            entry_assigned[entry_id as usize] = avail_slot[j] as i32;
            avail_used[j] = true;
            // Self-correct npad to the table entry's npad_number
            let real_npad = avail_npad[j];
            set_human_entry_npad(entry_id, real_npad);
            if real_npad >= 0 && (real_npad as usize) < CONTROLLER_PTRS.len() {
                CONTROLLER_PTRS[real_npad as usize].store(avail_ctrl[j], Ordering::Relaxed);
            }
            break;
        }
    }

    // --- Write slots and mappings for all assigned entries ---
    for i in 0..num_humans {
        let entry_id = human_entries[i];
        let assigned_slot = entry_assigned[entry_id as usize];
        if assigned_slot < 0 {
            let log_count = SETUP_LOG_COUNT.fetch_add(1, Ordering::Relaxed);
            if log_count < 10 {
                let npad = HUMAN_ENTRY_NPAD[entry_id as usize].load(Ordering::Relaxed);
                debug_log(&format!(
                    "NATIVE_FIM_SETUP: entry={} SKIPPED — no available controllers (npad={} p1_npad={} avail={})",
                    entry_id, npad, p1_npad, num_avail
                ));
            }
            continue;
        }

        // Write the controller slot so FIM dispatch fires for this entry
        let slot_addr = (input_mgr + 0x298 + entry_id as usize * 4) as *mut i32;
        core::ptr::write_volatile(slot_addr, assigned_slot);

        // Write the correct profile button mappings.
        // tag == 0 means "no profile selected" (or first custom profile — accepted
        // edge case). Use game defaults so tap-jump and standard bindings work.
        let tag = HUMAN_ENTRY_TAG[entry_id as usize].load(Ordering::Relaxed);
        let dst = (input_mgr + 0x18 + entry_id as usize * 0x50) as *mut ControllerMapping;
        if tag > 0 {
            if let Some(mapping) = get_profile_mapping(tag) {
                core::ptr::write_volatile(dst, mapping);
            } else {
                core::ptr::write_volatile(dst, DEFAULT_CONTROLLER_MAPPING);
            }
        } else {
            core::ptr::write_volatile(dst, DEFAULT_CONTROLLER_MAPPING);
        }

        // Diagnostic logging
        let log_count = SETUP_LOG_COUNT.fetch_add(1, Ordering::Relaxed);
        if log_count < 15 {
            // Find which avail entry was used to get the controller type
            let mut ctrl_type: i32 = -1;
            let mut ctrl_npad: i32 = -1;
            for j in 0..num_avail {
                if avail_slot[j] == assigned_slot {
                    ctrl_npad = avail_npad[j];
                    // Read controller type at Controller + 0x9c
                    ctrl_type = core::ptr::read_volatile(
                        (avail_ctrl[j] + 0x9c) as *const i32,
                    );
                    break;
                }
            }
            let entry_npad = HUMAN_ENTRY_NPAD[entry_id as usize].load(Ordering::Relaxed);
            debug_log(&format!(
                "NATIVE_FIM_SETUP: entry={} npad={} slot={} ctrl_type={} ctrl_npad={} tag={} p1_slot={} avail={}",
                entry_id, entry_npad, assigned_slot, ctrl_type, ctrl_npad, tag, p1_slot, num_avail
            ));
        }
    }
}

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
// Approach: patch the input manager's controller slot and mappings arrays so
// the FIM dispatch loop (at offset 0x17547f0) fires natively for human entries.
// The dispatch loop reads controller slots sequentially per iteration, so
// writing valid slots during the player_idx=0 FIM hook makes them visible for
// entries 2/3 later in the same frame.
//
//   1. During CSS: save Controller ptrs per npad and button mappings per npad
//   2. During training FIM (player_idx=0): setup_native_fim_for_humans() writes
//      valid controller slots and saved mappings into the input manager
//   3. FIM dispatch loop fires natively for entries 2/3 (no extra calls needed)
//   4. In set_cpu_controls: inject_human_input reads native FIM output from the
//      input manager's output array at input_mgr + 0x2B8 + entry * 8

use crate::common::input::{
    Buttons, ControlModuleInternal, Controller, ControllerMapping, ControllerStyle, InputKind,
    MappedInputs,
};

/// Game-default controller mapping: used when no profile is selected (tag == 0).
/// Matches SSBU's factory-default controls for all controller types.
const DEFAULT_CONTROLLER_MAPPING: ControllerMapping = ControllerMapping {
    // GC Controller defaults
    gc_l: InputKind::Guard,
    gc_r: InputKind::Guard,
    gc_z: InputKind::Grab,
    gc_dup: InputKind::AppealHi,
    gc_dlr: InputKind::AppealS,
    gc_ddown: InputKind::AppealLw,
    gc_a: InputKind::Attack,
    gc_b: InputKind::Special,
    gc_cstick: InputKind::SmashAttack,
    gc_y: InputKind::Jump,
    gc_x: InputKind::Jump,
    gc_rumble: true,
    gc_absmash: false,
    gc_tapjump: true,
    gc_sensitivity: 0,
    // Pro Controller defaults
    pro_l: InputKind::Guard,
    pro_r: InputKind::Guard,
    pro_zl: InputKind::Guard,
    pro_zr: InputKind::Grab,
    pro_dup: InputKind::AppealHi,
    pro_dlr: InputKind::AppealS,
    pro_ddown: InputKind::AppealLw,
    pro_a: InputKind::Attack,
    pro_b: InputKind::Special,
    pro_cstick: InputKind::SmashAttack,
    pro_x: InputKind::Jump,
    pro_y: InputKind::Jump,
    pro_rumble: true,
    pro_absmash: false,
    pro_tapjump: true,
    pro_sensitivity: 0,
    // Joycon defaults
    joy_shoulder: InputKind::Guard,
    joy_zshoulder: InputKind::Grab,
    joy_sl: InputKind::Guard,
    joy_sr: InputKind::Guard,
    joy_up: InputKind::AppealHi,
    joy_right: InputKind::AppealS,
    joy_left: InputKind::AppealS,
    joy_down: InputKind::AppealLw,
    joy_rumble: true,
    joy_absmash: false,
    joy_tapjump: true,
    joy_sensitivity: 0,
    // Padding/unknown
    _2b: 0, _2c: 0, _2d: 0, _2e: 0, _2f: 0, _30: 0, _31: 0, _32: 0,
    is_absmash: false,
    _34: [0; 0x1C],
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

/// Called from set_cpu_controls AFTER call_original!.
/// Uses the per-frame call counter to identify the entry (Nth call = entry N).
/// For human entries: reads native FIM output from the input manager's output
/// array (populated by the dispatch loop after we set up valid controller slots).
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

    let input_mgr = INPUT_MGR_PTR.load(Ordering::Relaxed);
    if input_mgr == 0 {
        // Input manager not yet captured — zero ALL CMI fields to prevent AI autopilot.
        (*cmi).buttons = Buttons::empty();
        (*cmi).stick_x = 0.0;
        (*cmi).stick_y = 0.0;
        (*cmi).padding = [0.0; 2];
        (*cmi).unk = [0; 8];
        (*cmi).clamped_lstick_x = 0.0;
        (*cmi).clamped_lstick_y = 0.0;
        (*cmi).padding2 = [0.0; 2];
        (*cmi).clamped_rstick_x = 0.0;
        (*cmi).clamped_rstick_y = 0.0;
        return true;
    }

    // Read native FIM output from the input manager's output array.
    // The dispatch loop writes MappedInputs at input_mgr + 0x2B8 + player_idx * 8.
    // If FIM fired for this entry (we set up a valid slot), this contains correct
    // mapped input. If not (slot was invalid), the dispatch loop zeroed it —
    // which is also correct behavior (character stands still).
    let output_ptr = (input_mgr + 0x2B8 + entry_id as usize * 8) as *const MappedInputs;
    let mapped = core::ptr::read_volatile(output_ptr);

    use crate::training::input_record::{STICK_CLAMP_MULTIPLIER, STICK_NEUTRAL};

    // Convert MappedInputs → CMI format.
    // Zero ALL fields to prevent AI-generated data from call_original! leaking
    // through unwritten fields (padding, unk[8], padding2). The unk field likely
    // contains previous-buttons / just-pressed flags that the game reads.
    (*cmi).buttons = mapped.buttons;
    (*cmi).stick_x = (mapped.lstick_x as f32) / (i8::MAX as f32);
    (*cmi).stick_y = (mapped.lstick_y as f32) / (i8::MAX as f32);
    (*cmi).padding = [0.0; 2];
    (*cmi).unk = [0; 8];

    let clamp_x = ((mapped.lstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    let clamp_y = ((mapped.lstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    (*cmi).clamped_lstick_x = if clamp_x.abs() >= STICK_NEUTRAL { clamp_x } else { 0.0 };
    (*cmi).clamped_lstick_y = if clamp_y.abs() >= STICK_NEUTRAL { clamp_y } else { 0.0 };
    (*cmi).padding2 = [0.0; 2];

    let rclamp_x = ((mapped.rstick_x as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    let rclamp_y = ((mapped.rstick_y as f32) * STICK_CLAMP_MULTIPLIER).clamp(-1.0, 1.0);
    (*cmi).clamped_rstick_x = if rclamp_x.abs() >= STICK_NEUTRAL { rclamp_x } else { 0.0 };
    (*cmi).clamped_rstick_y = if rclamp_y.abs() >= STICK_NEUTRAL { rclamp_y } else { 0.0 };

    true
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

/// Offset of FUN_7101db1910 (btn_rule handler): manages the Solo/Team toggle
/// button on the CSS. Reads scene_obj+0x44d as a transient "press in progress"
/// flag; returns 1 when the toggle animation completes.
const OFFSET_BTN_RULE_HANDLER: usize = 0x1db1910;

/// Team mode flag — persists across training resets, toggled at CSS.
/// true = Team Battle, false = Solo Battle (default).
static TEAM_MODE: AtomicBool = AtomicBool::new(false);

/// Address of the vanilla team battle flag byte in .bss.
/// DAT_71052c41e8 — read by app::global_parameter::is_team_battle().
const TEAM_BATTLE_FLAG_BSS: usize = 0x52c41e8;

pub fn is_team_mode() -> bool {
    TEAM_MODE.load(Ordering::Relaxed)
}

/// Keep vanilla is_team_battle() in sync with our TEAM_MODE flag.
/// Called once per frame from once_per_frame_per_fighter (entry 0).
pub unsafe fn sync_team_battle_flag() {
    let text_base = skyline::hooks::getRegionAddress(
        skyline::hooks::Region::Text,
    ) as usize;
    let flag_ptr = (text_base + TEAM_BATTLE_FLAG_BSS) as *mut u8;
    let desired = if TEAM_MODE.load(Ordering::Relaxed) { 1u8 } else { 0u8 };
    core::ptr::write_volatile(flag_ptr, desired);
}

// ---------------------------------------------------------------------------
// Background nn::hid polling thread
// ---------------------------------------------------------------------------
//
// nn::hid::GetNpadFullKeyState crashes when called from the draw/render
// thread. To get controller input during CSS (where FIM hasn't fired yet),
// we spawn a background thread that polls nn::hid every ~16ms and stores
// the result in atomics. The draw hook reads these atomics for team toggle
// and color cycling.

/// Current button bitmask from the background hid polling thread.
static HID_POLL_CURRENT: AtomicU64 = AtomicU64::new(0);

/// Whether the hid polling thread has been started.
static HID_POLL_STARTED: AtomicBool = AtomicBool::new(false);

/// Start the background hid polling thread (idempotent — only starts once).
fn ensure_hid_poll_thread() {
    if HID_POLL_STARTED.swap(true, Ordering::SeqCst) {
        return; // already running
    }
    std::thread::spawn(|| unsafe {
        // Buffer must be >= 0x100 (matching HDR's approach).
        let mut buf = [0u8; 0x100];
        loop {
            skyline::nn::hid::GetNpadFullKeyState(
                buf.as_mut_ptr() as _,
                &0i32 as *const _ as _,
            );
            let buttons = core::ptr::read_volatile(buf.as_ptr().add(0x08) as *const u64);
            HID_POLL_CURRENT.store(buttons, Ordering::Relaxed);
            // ~60fps polling (16ms = 16_000_000 ns)
            std::thread::sleep(std::time::Duration::from_millis(16));
        }
    });
}

// ---------------------------------------------------------------------------
// Team flag material colors (white/black res colors for flag_color pane)
// ---------------------------------------------------------------------------

const TEAM_FLAG_COLORS: [(ResColor, ResColor); 4] = [
    // Red team — vivid red
    (ResColor { r: 255, g: 20, b: 20, a: 255 }, ResColor { r: 200, g: 0, b: 0, a: 0 }),
    // Blue team — vivid blue
    (ResColor { r: 20, g: 50, b: 255, a: 255 }, ResColor { r: 0, g: 30, b: 200, a: 0 }),
    // Green team — vivid green
    (ResColor { r: 20, g: 255, b: 20, a: 255 }, ResColor { r: 0, g: 200, b: 0, a: 0 }),
    // Yellow team — vivid yellow
    (ResColor { r: 255, g: 240, b: 20, a: 255 }, ResColor { r: 200, g: 190, b: 0, a: 0 }),
];

/// Read a material's current white or black ResColor, handling both byte and float storage.
unsafe fn read_material_color(material: &Material, color_type: MaterialColorType) -> ResColor {
    let (flag_bit, idx) = if color_type == MaterialColorType::BlackColor {
        (MaterialFlags::BlackColorFloat as u8, 0usize)
    } else {
        (MaterialFlags::WhiteColorFloat as u8, 1usize)
    };
    if material.m_flag & (1 << flag_bit) != 0 {
        // Float storage — values are in 0..255 range
        let pp = material.m_colors.p_float_color;
        let p = *pp.add(idx);
        ResColor {
            r: (*p.add(0)) as u8,
            g: (*p.add(1)) as u8,
            b: (*p.add(2)) as u8,
            a: (*p.add(3)) as u8,
        }
    } else {
        // Byte storage
        let c = material.m_colors.byte_color[idx];
        ResColor { r: c[0], g: c[1], b: c[2], a: c[3] }
    }
}

/// Saved original material colors for panel bg panes (for bracket restore).
/// [panel_idx][variant_idx] matching PANEL_BG_PANES layout.
static mut ORIG_PANEL_WHITE: [[ResColor; 3]; 4] = [[ResColor { r: 255, g: 255, b: 255, a: 255 }; 3]; 4];
static mut ORIG_PANEL_BLACK: [[ResColor; 3]; 4] = [[ResColor { r: 0, g: 0, b: 0, a: 0 }; 3]; 4];
static mut PANEL_COLORS_SAVED: bool = false;

/// Poll controller for team mode toggle and color cycling during CSS.
/// Called from css_btn_rule_draw (draw hook) during CSS only.
///
/// Controls:
///   X          — toggle Team/Solo mode
///   ZR + D-Up  — cycle P1's team color (R→B→G→Y→R)
///   ZR + D-Rt  — cycle P2's team color
///   ZR + D-Dn  — cycle P3's team color
///   ZR + D-Lt  — cycle P4's team color
///
/// Reads buttons from the background hid polling thread (works on cold boot).
/// Falls through to Controller.just_down when available (after first training
/// session) for lower-latency input.
unsafe fn poll_css_team_toggle() {
    // Cooldown prevents multi-fire from draw hook calling multiple times per frame.
    static COOLDOWN: AtomicU32 = AtomicU32::new(0);
    let cd = COOLDOWN.load(Ordering::Relaxed);
    if cd > 0 {
        COOLDOWN.store(cd - 1, Ordering::Relaxed);
        return;
    }

    // Previous buttons for edge detection (maintained by draw hook, not bg thread).
    static DRAW_PREV: AtomicU64 = AtomicU64::new(0);

    // Try Controller first (lower latency, has hardware just_down).
    let ctrl_addr = CONTROLLER_PTRS[0].load(Ordering::Relaxed);
    let (just_x, just_dup, just_drt, just_ddn, just_dlt, held_zr);

    if ctrl_addr != 0 {
        let c = &*(ctrl_addr as *const Controller);
        just_x = c.just_down.x();
        just_dup = c.just_down.dpad_up();
        just_drt = c.just_down.dpad_right();
        just_ddn = c.just_down.dpad_down();
        just_dlt = c.just_down.dpad_left();
        held_zr = c.current_buttons.zr();
    } else {
        // Background thread nn::hid polling — works on cold boot.
        ensure_hid_poll_thread();
        let current = HID_POLL_CURRENT.load(Ordering::Relaxed);
        let prev = DRAW_PREV.swap(current, Ordering::Relaxed);
        let just = current & !prev;
        just_x = just & HID_X != 0;
        just_dup = just & HID_DPAD_UP != 0;
        just_drt = just & HID_DPAD_RIGHT != 0;
        just_ddn = just & HID_DPAD_DOWN != 0;
        just_dlt = just & HID_DPAD_LEFT != 0;
        held_zr = current & HID_ZR != 0;
    }

    // X button: toggle team mode + re-layout CSS panels.
    if just_x {
        let new_val = !TEAM_MODE.load(Ordering::Relaxed);
        TEAM_MODE.store(new_val, Ordering::Relaxed);
        COOLDOWN.store(10, Ordering::Relaxed);
        let scene = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
        let orig_addr = PANEL_LAYOUT_ORIG_FN.load(Ordering::Relaxed);
        if scene != 0 && orig_addr != 0 {
            let scene_ptr = scene as *mut u8;
            let mode_ptr = scene_ptr.add(0x16C) as *mut u32;
            let arg2 = PANEL_LAYOUT_LAST_ARG2.load(Ordering::Relaxed);
            // Set mode byte and re-invoke the original layout function directly.
            // 0x6 = smash-like (team flags visible), 0xB = training CSS mode.
            let new_mode: u32 = if new_val { 0x6 } else { 0xB };
            core::ptr::write_volatile(mode_ptr, new_mode);
            type PanelLayoutFn = unsafe extern "C" fn(*mut u8, u32, u32);
            let orig_fn: PanelLayoutFn = core::mem::transmute(orig_addr);
            let slots: u32 = if new_val { 4 } else { 4 };
            orig_fn(scene_ptr, slots, arg2);
            debug_log(&format!(
                "CSS team toggle: team_mode={}, mode={:#x}, relayout {} slots",
                new_val, new_mode, slots
            ));
        } else {
            debug_log(&format!("CSS team toggle: team_mode={} (no scene/orig)", new_val));
        }
        return;
    }

    // Color cycling (only when team mode is ON).
    if !TEAM_MODE.load(Ordering::Relaxed) {
        return;
    }

    // ZR held + D-pad: cycle individual player's team color.
    if held_zr {
        let dirs = [just_dup, just_drt, just_ddn, just_dlt];
        let names = ["R", "B", "G", "Y"];
        for (i, &pressed) in dirs.iter().enumerate() {
            if pressed {
                let old = TEAM_COLORS[i].load(Ordering::Relaxed);
                let new_color = (old + 1) % 4;
                TEAM_COLORS[i].store(new_color, Ordering::Relaxed);
                COOLDOWN.store(10, Ordering::Relaxed);
                debug_log(&format!(
                    "CSS team color: P{} → {}",
                    i + 1, names[new_color as usize]
                ));
            }
        }
    }
}

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

/// Recursively search for a pane with the given name within the subtree rooted at `root`.
/// Returns a mutable pointer to the first matching pane, or null.
unsafe fn find_pane_by_name(root: *const Pane, name: &str) -> *mut Pane {
    use skyline::nn::ui2d::PaneNode;
    if root.is_null() {
        return core::ptr::null_mut();
    }
    let pane_name = skyline::from_c_str((*root).name.as_ptr());
    if pane_name == name {
        return root as *mut Pane;
    }
    let sentinel = &(*root).children_list as *const PaneNode as *mut PaneNode;
    let mut current = (*root).children_list.next;
    let mut count = 0u32;
    while current != sentinel && count < 500 {
        let child = (current as *const u8).sub(0x08) as *const Pane;
        let result = find_pane_by_name(child, name);
        if !result.is_null() {
            return result;
        }
        current = (*current).next;
        count += 1;
    }
    core::ptr::null_mut()
}

/// Cached pane pointers for the per-panel team flag hierarchy.
/// Populated once per CSS session by `cache_flag_panes`, cleared when
/// CSS_TRAINING_SCENE_PTR changes.
///
/// Per-panel flag hierarchy (confirmed via deep dump):
///   team → flag_pos → set_btn_flag_team (vis=0!) → btn_all → btn_size →
///     flag_sd, flag_line (vis=0), flag_color (vis=0),
///     color_r (a=0), color_b (a=0), color_g (a=0), color_y (a=0)
///
/// To show a flag: set_btn_flag_team vis=1, flag_line vis=1, flag_color vis=1,
/// team alpha=255, then set the desired color_X alpha=255 (others 0).
struct PanelFlagPanes {
    team: *mut Pane,
    flag_pos: *mut Pane,
    set_btn_flag_team: *mut Pane,
    btn_all: *mut Pane,
    btn_size: *mut Pane,
    flag_line: *mut Pane,
    flag_color: *mut Pane,
    flag_sd: *mut Pane,
    color_r: *mut Pane,
    color_b: *mut Pane,
    color_g: *mut Pane,
    color_y: *mut Pane,
}

impl PanelFlagPanes {
    const fn null() -> Self {
        Self {
            team: core::ptr::null_mut(),
            flag_pos: core::ptr::null_mut(),
            set_btn_flag_team: core::ptr::null_mut(),
            btn_all: core::ptr::null_mut(),
            btn_size: core::ptr::null_mut(),
            flag_line: core::ptr::null_mut(),
            flag_color: core::ptr::null_mut(),
            flag_sd: core::ptr::null_mut(),
            color_r: core::ptr::null_mut(),
            color_b: core::ptr::null_mut(),
            color_g: core::ptr::null_mut(),
            color_y: core::ptr::null_mut(),
        }
    }
}

// Only written/read from the draw hook (single thread).
static mut FLAG_PANE_CACHE: [PanelFlagPanes; 4] = [
    PanelFlagPanes::null(), PanelFlagPanes::null(),
    PanelFlagPanes::null(), PanelFlagPanes::null(),
];
/// Cached "Training" text pane (TextBox) — set to "Team Training" when active.
static mut TRAINING_TEXT_PANE: *mut Pane = core::ptr::null_mut();
/// Cached panel background panes for team coloring — 3 size variants per panel:
/// [0] = _l (large, ≤2 slots), [1] = _m (medium), [2] = _s (small, 3+ slots).
/// Game shows/hides variants based on player count; we tint all found.
static mut PANEL_BG_PANES: [[*mut Pane; 3]; 4] = [[core::ptr::null_mut(); 3]; 4];
static FLAG_CACHE_SCENE: AtomicUsize = AtomicUsize::new(0);

/// Log immediate children of a pane (for diagnostics).
unsafe fn dump_children(parent: *const Pane, label: &str) {
    use skyline::nn::ui2d::PaneNode;
    if parent.is_null() { return; }
    let sentinel = &(*parent).children_list as *const PaneNode as *mut PaneNode;
    let mut current = (*parent).children_list.next;
    let mut names = String::new();
    let mut count = 0u32;
    while current != sentinel && count < 100 {
        let child = (current as *const u8).sub(0x08) as *const Pane;
        let name = skyline::from_c_str((*child).name.as_ptr());
        if !names.is_empty() { names.push_str(", "); }
        names.push_str(&name);
        current = (*current).next;
        count += 1;
    }
    debug_log(&format!("{}: [{}]", label, names));
}

/// Log pane tree at depth 0..max_depth (for diagnostics).
unsafe fn dump_pane_tree(pane: *const Pane, depth: u32, max_depth: u32) {
    use skyline::nn::ui2d::PaneNode;
    if pane.is_null() || depth > max_depth { return; }
    let name = skyline::from_c_str((*pane).name.as_ptr());
    let indent = "  ".repeat(depth as usize);
    debug_log(&format!(
        "{}[{}] sz=({:.0},{:.0}) pos=({:.0},{:.0})",
        indent, name, (*pane).size_x, (*pane).size_y, (*pane).pos_x, (*pane).pos_y,
    ));
    let sentinel = &(*pane).children_list as *const PaneNode as *mut PaneNode;
    let mut current = (*pane).children_list.next;
    let mut count = 0u32;
    while current != sentinel && count < 200 {
        let child = (current as *const u8).sub(0x08) as *const Pane;
        dump_pane_tree(child, depth + 1, max_depth);
        current = (*current).next;
        count += 1;
    }
}

/// Populate FLAG_PANE_CACHE by searching the pane tree once.
/// Also caches the "Training" text pane and panel background panes.
unsafe fn cache_flag_panes(root_pane: *const Pane) {
    // Search for "Training" text pane — try common CSS text pane names.
    TRAINING_TEXT_PANE = core::ptr::null_mut();
    for name in &["txt_rule_name", "txt_training", "txt_rule", "txt_mode",
                   "txt_melee_type", "txt_title", "txt_melee"] {
        let p = find_pane_by_name(root_pane, name);
        if !p.is_null() {
            TRAINING_TEXT_PANE = p;
            debug_log(&format!("Found training text pane: '{}'", name));
            break;
        }
    }
    if TRAINING_TEXT_PANE.is_null() {
        debug_log("WARNING: could not find training text pane (tried common names)");
    }

    let panel_names = ["set_panel_1p", "set_panel_2p", "set_panel_3p", "set_panel_4p"];
    for (i, pn) in panel_names.iter().enumerate() {
        let panel = find_pane_by_name(root_pane, pn);
        if panel.is_null() { continue; }

        // Cache panel color window panes — 3 size variants (l/m/s).
        // Game shows _l when ≤2 slots, _s when 3+. We tint all found.
        let variant_names = ["window_l_color_l", "window_l_color_m", "window_l_color_s"];
        for (vi, vname) in variant_names.iter().enumerate() {
            let color_win = find_pane_by_name(panel as *const Pane, vname);
            PANEL_BG_PANES[i][vi] = color_win;
            if !color_win.is_null() {
                // Picture-compatible layout (material ptr at Pane+0).
                let picture = (&mut *color_win).as_picture();
                let material = &*picture.material;
                ORIG_PANEL_WHITE[i][vi] = read_material_color(material, MaterialColorType::WhiteColor);
                ORIG_PANEL_BLACK[i][vi] = read_material_color(material, MaterialColorType::BlackColor);
            }
        }
        if i == 0 {
            let found: Vec<&str> = variant_names.iter().enumerate()
                .filter(|(vi, _)| !PANEL_BG_PANES[0][*vi].is_null())
                .map(|(_, n)| *n)
                .collect();
            debug_log(&format!("Panel bg panes found in set_panel_1p: {:?}", found));
        }
        PANEL_COLORS_SAVED = true;

        let team = find_pane_by_name(panel as *const Pane, "team");
        if team.is_null() { continue; }
        let flag_pos = find_pane_by_name(team as *const Pane, "flag_pos");
        let btn = find_pane_by_name(team as *const Pane, "set_btn_flag_team");
        if btn.is_null() { continue; }
        let btn_all = find_pane_by_name(btn as *const Pane, "btn_all");
        let btn_size = if !btn_all.is_null() {
            find_pane_by_name(btn_all as *const Pane, "btn_size")
        } else {
            core::ptr::null_mut()
        };
        FLAG_PANE_CACHE[i] = PanelFlagPanes {
            team,
            flag_pos,
            set_btn_flag_team: btn,
            btn_all,
            btn_size,
            flag_line: find_pane_by_name(btn as *const Pane, "flag_line"),
            flag_color: find_pane_by_name(btn as *const Pane, "flag_color"),
            flag_sd: find_pane_by_name(btn as *const Pane, "flag_sd"),
            color_r: find_pane_by_name(btn as *const Pane, "color_r"),
            color_b: find_pane_by_name(btn as *const Pane, "color_b"),
            color_g: find_pane_by_name(btn as *const Pane, "color_g"),
            color_y: find_pane_by_name(btn as *const Pane, "color_y"),
        };
    }
}

/// Per-player team color assignment. 0=red, 1=blue, 2=green, 3=yellow.
/// Default: P1/P3 = red(0), P2/P4 = blue(1).
static TEAM_COLORS: [AtomicU32; 4] = [
    AtomicU32::new(0), AtomicU32::new(1), AtomicU32::new(0), AtomicU32::new(1),
];

/// Called from handle_draw for every layout each frame.
/// During training CSS on `chara_select_base`, shows/hides team flag panes
/// based on the current TEAM_MODE state, scales flags, updates "Training"
/// text, and tints panel backgrounds to match team colors.
pub unsafe fn css_btn_rule_draw(root_pane: &Pane, layout_name: &str) {
    if layout_name != "chara_select_base" {
        return;
    }

    let scene = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
    if scene == 0 {
        return;
    }

    // Poll P1 controller for team mode toggle (X button).
    poll_css_team_toggle();

    // Cache pane pointers once per CSS session.
    if FLAG_CACHE_SCENE.load(Ordering::Relaxed) != scene {
        FLAG_CACHE_SCENE.store(scene, Ordering::Relaxed);
        PANEL_COLORS_SAVED = false;
        cache_flag_panes(root_pane as *const Pane);
        debug_log(&format!(
            "css_flags: cached panes for scene {:#x} (team[0]={:?})",
            scene, FLAG_PANE_CACHE[0].team
        ));
    }

    let team_mode = TEAM_MODE.load(Ordering::Relaxed);

    // Keep vanilla is_team_battle() in sync during CSS.
    sync_team_battle_flag();

    // TODO: "Training" → "Team Training" text change disabled.
    // txt_title was found but as_textbox().set_text_string() crashes — likely
    // the TextBox buffer is too small for the longer string, or txt_title
    // isn't actually a TextBox type. Need to check pane type + buffer capacity.

    for i in 0..4 {
        let fp = &FLAG_PANE_CACHE[i];
        if fp.team.is_null() { continue; }

        let color = TEAM_COLORS[i].load(Ordering::Relaxed);

        if team_mode {
            // Show flag: force visibility + alpha on every pane in the chain.
            (*fp.team).alpha = 255;
            (*fp.team).global_alpha = 255;
            for p in [fp.flag_pos, fp.set_btn_flag_team, fp.btn_all, fp.btn_size,
                       fp.flag_sd, fp.flag_line, fp.flag_color] {
                if !p.is_null() {
                    (*p).flags |= 1;
                    (*p).alpha = 255;
                    (*p).global_alpha = 255;
                }
            }

            // Scale flags 33% larger.
            if !fp.set_btn_flag_team.is_null() {
                (*fp.set_btn_flag_team).scale_x = 1.33;
                (*fp.set_btn_flag_team).scale_y = 1.33;
            }

            // Set the correct team color pane visible + opaque, others invisible.
            let colors = [fp.color_r, fp.color_b, fp.color_g, fp.color_y];
            for (ci, cp) in colors.iter().enumerate() {
                if !cp.is_null() {
                    if ci as u32 == color {
                        (**cp).flags |= 1;
                        (**cp).alpha = 255;
                        (**cp).global_alpha = 255;
                    } else {
                        (**cp).flags &= !1;
                        (**cp).alpha = 0;
                        (**cp).global_alpha = 0;
                    }
                }
            }

            // Tint the flag_color background to match the team color.
            if !fp.flag_color.is_null() {
                let (white, black) = TEAM_FLAG_COLORS[color.min(3) as usize];
                let picture = (&mut *fp.flag_color).as_picture();
                let material = &mut *picture.material;
                material.set_white_res_color(white);
                material.set_black_res_color(black);
            }

            // Tint all panel bg variants via material colors (bracket approach).
            // We set team colors here BEFORE draw; css_post_draw() restores
            // originals AFTER original!() so shared materials don't persist.
            let (white, black) = TEAM_FLAG_COLORS[color.min(3) as usize];
            for vi in 0..3 {
                let bg = PANEL_BG_PANES[i][vi];
                if !bg.is_null() {
                    let picture = (&mut *bg).as_picture();
                    let material = &mut *picture.material;
                    material.set_white_res_color(white);
                    material.set_black_res_color(black);
                }
            }
        } else {
            // Hide flags: restore vanilla hidden state.
            (*fp.team).alpha = 0;
            (*fp.team).global_alpha = 0;
            if !fp.set_btn_flag_team.is_null() {
                (*fp.set_btn_flag_team).flags &= !1;
                (*fp.set_btn_flag_team).scale_x = 1.0;
                (*fp.set_btn_flag_team).scale_y = 1.0;
            }
            // Restore all panel bg variants to saved originals.
            if PANEL_COLORS_SAVED {
                for vi in 0..3 {
                    let bg = PANEL_BG_PANES[i][vi];
                    if !bg.is_null() {
                        let picture = (&mut *bg).as_picture();
                        let material = &mut *picture.material;
                        material.set_white_res_color(ORIG_PANEL_WHITE[i][vi]);
                        material.set_black_res_color(ORIG_PANEL_BLACK[i][vi]);
                    }
                }
            }
        }
    }
}

/// Called from handle_draw AFTER original!() to restore shared material colors.
/// This is the second half of the bracket: css_btn_rule_draw sets team colors,
/// original!() draws everything, then this restores originals so the shared
/// material doesn't persist team tinting to other panes (e.g., portraits).
pub unsafe fn css_post_draw(layout_name: &str) {
    if layout_name != "chara_select_base" {
        return;
    }
    if !TEAM_MODE.load(Ordering::Relaxed) || !PANEL_COLORS_SAVED {
        return;
    }
    for i in 0..4 {
        for vi in 0..3 {
            let bg = PANEL_BG_PANES[i][vi];
            if !bg.is_null() {
                let picture = (&mut *bg).as_picture();
                let material = &mut *picture.material;
                material.set_white_res_color(ORIG_PANEL_WHITE[i][vi]);
                material.set_black_res_color(ORIG_PANEL_BLACK[i][vi]);
            }
        }
    }
}

/// Hook the btn_rule button handler on the CSS. Vanilla manages the Solo/Team
/// toggle animation; we intercept its return value to flip our TEAM_MODE flag.
/// Returns 1 when a toggle completes, 0 otherwise.
#[skyline::hook(offset = OFFSET_BTN_RULE_HANDLER)]
pub unsafe fn btn_rule_handler_hook(scene_obj: *mut u8) -> u64 {
    let result = call_original!(scene_obj);
    if result == 1 {
        let new_val = !TEAM_MODE.load(Ordering::Relaxed);
        TEAM_MODE.store(new_val, Ordering::Relaxed);
        debug_log(&format!("btn_rule toggle: team_mode = {}", new_val));
    }
    result
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

/// Stored original panel layout fn pointer for re-invocation from toggle.
static PANEL_LAYOUT_ORIG_FN: AtomicUsize = AtomicUsize::new(0);
/// Last arg2 seen in panel layout hook (needed for re-invocation).
static PANEL_LAYOUT_LAST_ARG2: AtomicU32 = AtomicU32::new(0);

#[skyline::hook(offset = OFFSET_CSS_PANEL_LAYOUT)]
pub unsafe fn css_panel_layout_hook(scene: *mut u8, slot_count: u32, arg2: u32) {
    // Save original fn pointer on first call for re-invocation from toggle.
    if PANEL_LAYOUT_ORIG_FN.load(Ordering::Relaxed) == 0 {
        let f = original!();
        PANEL_LAYOUT_ORIG_FN.store(f as usize, Ordering::Relaxed);
    }
    PANEL_LAYOUT_LAST_ARG2.store(arg2, Ordering::Relaxed);

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
        // Read tag/profile index — prefer CSS panel (config may have P1's tag
        // from the shared buffer), fall back to config[0x214].
        let config_tag = core::ptr::read_volatile(config.add(0x214) as *const u32);
        let panel_tag = read_css_panel_tag(1);
        let entry1_tag = if panel_tag > 0 || is_entry1_human { panel_tag } else { config_tag };
        HUMAN_ENTRY_TAG[1].store(entry1_tag, Ordering::Relaxed);

        if is_entry1_human {
            // config[0x7C] is NOT reliable for entry 1 — the game reuses a
            // shared config buffer and may leave P1's npad (0) here instead of
            // P2's actual npad. Read from CSS panel like entries 2/3.
            let config_npad = core::ptr::read_volatile(config.add(0x7C) as *const i32);
            let panel_npad = read_css_panel_npad(1);
            let fim_npad = FIM_NPAD_FOR_ENTRY[1].load(Ordering::Relaxed);
            let entry1_npad = if panel_npad >= 0 {
                panel_npad
            } else if fim_npad >= 0 {
                fim_npad
            } else if config_npad >= 0 {
                config_npad // last resort: trust config
            } else {
                1 // fallback
            };
            // Write corrected npad back to config so the game's clone_write
            // original uses the right controller binding.
            core::ptr::write_volatile(config.add(0x7C) as *mut i32, entry1_npad);
            HUMAN_ENTRY_NPAD[1].store(entry1_npad, Ordering::Relaxed);
            if entry1_npad >= 0 && entry1_npad < 8 {
                NPAD_TO_ENTRY[entry1_npad as usize].store(1, Ordering::Relaxed);
            }
            debug_log(&format!(
                "clone_write: entry=1 HUMAN npad={} (panel={} fim={} config={})",
                entry1_npad, panel_npad, fim_npad, config_npad
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


