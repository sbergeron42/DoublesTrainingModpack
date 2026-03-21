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

use skyline::nn::ui2d::{AnimTransform, AnimTransformNode, Layout, Material, MaterialFlags, MaterialColorType, Pane, ResColor};
use smash::app::{self, lua_bind::*, BattleObjectModuleAccessor};
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
use training_mod_consts::OnOff;

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

        // Write only the first 0x2B bytes of ControllerMapping (profile/button data).
        // Bytes 0x2B-0x4F are persistent state used by fim_cstick_handler — writing
        // them every frame resets the c-stick shift register, hold counter, timer,
        // and direction lock, breaking c-stick on non-P1 human entries.
        let dst = (input_mgr + 0x18 + entry_id as usize * 0x50) as *mut u8;
        let mapping = get_cached_profile_mapping(entry_id as usize)
            .unwrap_or(DEFAULT_CONTROLLER_MAPPING);
        core::ptr::copy_nonoverlapping(
            &mapping as *const ControllerMapping as *const u8,
            dst,
            0x2B,
        );

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
            let tag = HUMAN_ENTRY_TAG[entry_id as usize].load(Ordering::Relaxed);
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

#[cfg(feature = "doubles_debug_log")]
const DOUBLES_DEBUG_LOG: &str = "sd:/ultimate/TrainingModpack/doubles_debug.log";

/// Appends `msg` with a timestamp to the persistent debug file on the SD card.
/// On the first call each session, the file is truncated (overwritten) so you
/// always get a fresh log without manual cleanup.
/// Errors are silently ignored so this is safe to call from any hook context.
///
/// Gated behind the `doubles_debug_log` feature flag — no-op in release builds
/// to avoid format! allocations and SD card I/O every frame.
#[cfg(feature = "doubles_debug_log")]
pub fn debug_log(msg: &str) {
    use std::io::Write;
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    static FIRST_CALL: AtomicBool = AtomicBool::new(true);
    let tick = COUNTER.fetch_add(1, Ordering::Relaxed);
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

#[cfg(not(feature = "doubles_debug_log"))]
#[inline(always)]
pub fn debug_log(_msg: &str) {}

// ---------------------------------------------------------------------------
// CPU↔CPU hit-team assignment
// ---------------------------------------------------------------------------

/// Per-entry flag: true once hit-team has been applied this session.
/// Reset by `invalidate_hit_teams()` on CSS re-entry or team mode toggle.
static HIT_TEAM_APPLIED: [AtomicBool; 4] = [
    AtomicBool::new(false), AtomicBool::new(false),
    AtomicBool::new(false), AtomicBool::new(false),
];

/// Call when team assignments may have changed (CSS re-entry, team toggle)
/// so hit-teams get re-applied on next frame.
pub fn invalidate_hit_teams() {
    for flag in &HIT_TEAM_APPLIED {
        flag.store(false, Ordering::Relaxed);
    }
}

pub unsafe fn set_cpu_hit_team(module_accessor: &mut BattleObjectModuleAccessor) {
    let entry_id =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID);

    // Outline color: write every frame so re-inits (respawn, savestate) pick
    // up the correct value. Gated on the TUI toggle.
    if is_team_mode() && team_outlines_enabled() && (entry_id as usize) < 4 {
        let team_color = TEAM_COLORS[entry_id as usize].load(Ordering::Relaxed);
        set_outline_team_color(entry_id, team_color);
    }

    // Sync fi_data team color and is_operation_cpu flag every frame.
    // The game's smoke trail code (FUN_710068f530) reads fi_data+0x92
    // directly — when non-zero it returns gray (8) instead of the team
    // color at fi_data+0x84.  Our is_operation_cpu() hook returns false
    // for human entries, but the smoke code bypasses it via raw memory.
    // Fix: clear the raw flag for human entries so they get team-colored
    // trails, and ensure fi_data+0x84 holds the team color even when
    // outlines are disabled.
    if is_team_mode() && (entry_id as usize) < 4 {
        let team_color = TEAM_COLORS[entry_id as usize].load(Ordering::Relaxed);
        let is_human = entry_id == 0 || is_human_entry(entry_id);
        sync_fi_data_cpu_flag(entry_id, team_color, is_human);
    }

    // Skip if already applied — team values don't change mid-match.
    if (entry_id as usize) < 4 && HIT_TEAM_APPLIED[entry_id as usize].load(Ordering::Relaxed) {
        return;
    }

    // Every fighter gets hit-team = entry_id so all 4 can hit each other.
    TeamModule::set_hit_team(module_accessor, entry_id);
    // Also set team_no to a unique value — game checks both for some interactions.
    TeamModule::set_team(module_accessor, entry_id, false);

    // Jostle: in team mode, same-color teammates share a jostle team.
    if is_team_mode() && (entry_id as usize) < 4 {
        let team_color = TEAM_COLORS[entry_id as usize].load(Ordering::Relaxed) as i32;
        JostleModule::set_team(module_accessor, team_color);
    } else {
        JostleModule::set_team(module_accessor, entry_id);
    }

    if (entry_id as usize) < 4 {
        HIT_TEAM_APPLIED[entry_id as usize].store(true, Ordering::Relaxed);
    }
}

/// Write team color to fi_data+0x2C for a fighter, which controls outline color.
///
/// Navigation: FIGHTER_MANAGER_ADDR → *(ptr) = FM → *(FM) = inner
///   → *(inner + entry_id*8 + 0x20) = FighterEntry
///   → *(FighterEntry + 0xF8) = fi_data → fi_data+0x2C = outline color (u32)
unsafe fn set_outline_team_color(entry_id: i32, team_color: u32) {
    let fm_singleton_ptr = read(&FIGHTER_MANAGER_ADDR);
    if fm_singleton_ptr == 0 {
        return;
    }
    let fm = *(fm_singleton_ptr as *const usize);
    if fm == 0 {
        return;
    }
    let inner = *(fm as *const usize);
    if inner == 0 {
        return;
    }
    let entry_ptr = *((inner + (entry_id as usize) * 8 + 0x20) as *const usize);
    if entry_ptr == 0 {
        return;
    }
    let fi_data = *((entry_ptr + 0xF8) as *const usize);
    if fi_data == 0 {
        return;
    }
    // fi_data+0x2C: outline color read by the renderer each frame.
    *((fi_data + 0x2C) as *mut u32) = team_color;
    // fi_data+0x84: team color read by get_team_color (effects, HUD).
    *((fi_data + 0x84) as *mut u32) = team_color;
    // entry+0x30: source field that the game's init copies to fi_data+0x2C.
    // Writing it ensures any re-initialization picks up the correct color.
    *((entry_ptr + 0x30) as *mut u32) = team_color;
}

/// Sync fi_data+0x84 (team color for effects) and fi_data+0x92 (is_operation_cpu
/// flag) for a fighter entry. The game's knockback smoke trail function reads
/// fi_data+0x92 directly: non-zero → gray trail, zero → team-colored trail
/// using fi_data+0x84. This is independent of outlines.
///
/// Navigation: same pointer chain as set_outline_team_color.
unsafe fn sync_fi_data_cpu_flag(entry_id: i32, team_color: u32, is_human: bool) {
    let fm_singleton_ptr = read(&FIGHTER_MANAGER_ADDR);
    if fm_singleton_ptr == 0 {
        return;
    }
    let fm = *(fm_singleton_ptr as *const usize);
    if fm == 0 {
        return;
    }
    let inner = *(fm as *const usize);
    if inner == 0 {
        return;
    }
    let entry_ptr = *((inner + (entry_id as usize) * 8 + 0x20) as *const usize);
    if entry_ptr == 0 {
        return;
    }
    let fi_data = *((entry_ptr + 0xF8) as *const usize);
    if fi_data == 0 {
        return;
    }
    // fi_data+0x84: team color index used by get_team_color → smoke trail, effects.
    *((fi_data + 0x84) as *mut u32) = team_color;
    // fi_data+0x92: is_operation_cpu raw flag. Clear for human entries so the
    // smoke trail code returns the team color instead of 8 (gray).
    if is_human {
        *((fi_data + 0x92) as *mut u8) = 0;
    }
}

// ---------------------------------------------------------------------------
// Team footstool prevention
// ---------------------------------------------------------------------------
//
// In team mode, teammates must not be able to footstool each other while
// still being able to hit/grab. The game's built-in team system is
// all-or-nothing (team_attack flag controls BOTH hits and footstools),
// so we hook StatusModule::change_status_request_from_script_impl.
//
// Strategy: intercept TREAD_JUMP (attacker) only. Read TREAD_TARGET_ID
// from the attacker to identify the victim. If same team → redirect to
// JUMP_AERIAL (preserves jump input). The detection code then checks the
// attacker's status, doesn't find TREAD_JUMP, and never calls TREAD_DAMAGE
// for the victim. No deferred state needed.

const OFFSET_CHANGE_STATUS_REQ_SCRIPT: usize = 0x20876e0;

/// Given a TREAD_TARGET_ID value from the attacker, resolve the victim's
/// entry_id by comparing against all fighters' battle_object_ids.
unsafe fn resolve_tread_target(target_id: i32) -> Option<usize> {
    let fm_addr = training_mod_sync::read(&FIGHTER_MANAGER_ADDR);
    if fm_addr == 0 {
        return None;
    }
    let fm = *(fm_addr as *mut *mut app::FighterManager);

    for entry in 0..4i32 {
        let entry_id = app::FighterEntryID(entry);
        let fighter_entry =
            FighterManager::get_fighter_entry(fm, entry_id) as *mut app::FighterEntry;
        if fighter_entry.is_null() {
            continue;
        }
        let obj_id = FighterEntry::current_fighter_id(fighter_entry);
        if obj_id as i32 == target_id {
            return Some(entry as usize);
        }
    }
    None
}

#[skyline::hook(offset = OFFSET_CHANGE_STATUS_REQ_SCRIPT)]
unsafe fn change_status_req_script_hook(
    boma: *mut BattleObjectModuleAccessor,
    status_kind: i32,
    unk: bool,
) -> u64 {
    // Only intercept TREAD_JUMP (attacker) in team mode.
    if !is_team_mode() || status_kind != *FIGHTER_STATUS_KIND_TREAD_JUMP {
        return call_original!(boma, status_kind, unk);
    }

    // Identify the attacker's team.
    let attacker_entry = WorkModule::get_int(
        &mut *boma,
        *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID,
    ) as usize;
    if attacker_entry >= 4 {
        return call_original!(boma, status_kind, unk);
    }
    let attacker_team = TEAM_COLORS[attacker_entry].load(Ordering::Relaxed);

    // Read TREAD_TARGET_ID to find the victim.
    let target_id = WorkModule::get_int(
        &mut *boma,
        *FIGHTER_INSTANCE_WORK_ID_INT_TREAD_TARGET_ID,
    );
    if let Some(victim_entry) = resolve_tread_target(target_id) {
        if victim_entry < 4 {
            let victim_team = TEAM_COLORS[victim_entry].load(Ordering::Relaxed);
            if attacker_team == victim_team {
                // Same team: convert footstool into a normal aerial jump
                // (since the detection code already consumed the jump input).
                // Manually increment JUMP_COUNT so the jump is properly
                // consumed — prevents infinite double jumps.
                let jump_count = WorkModule::get_int(
                    &mut *boma,
                    *FIGHTER_INSTANCE_WORK_ID_INT_JUMP_COUNT,
                );
                let jump_max = WorkModule::get_int(
                    &mut *boma,
                    *FIGHTER_INSTANCE_WORK_ID_INT_JUMP_COUNT_MAX,
                );
                if jump_count < jump_max {
                    WorkModule::set_int(
                        &mut *boma,
                        jump_count + 1,
                        *FIGHTER_INSTANCE_WORK_ID_INT_JUMP_COUNT,
                    );
                    return call_original!(
                        boma,
                        *FIGHTER_STATUS_KIND_JUMP_AERIAL,
                        unk
                    );
                }
                // No jumps remaining: just suppress the footstool.
                return 0;
            }
        }
    }

    // Different teams or unresolvable target: allow footstool.
    call_original!(boma, status_kind, unk)
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

/// Cached ControllerMapping per entry. Populated at CSS (when tag is set) so
/// setup_native_fim_for_humans doesn't walk the 6-pointer profile chain every frame.
/// Uses UnsafeCell because it's only written from clone_write (single-threaded CSS)
/// and read from FIM hook (single-threaded per-frame).
use core::cell::UnsafeCell;
struct SyncMapping(UnsafeCell<Option<ControllerMapping>>);
unsafe impl Sync for SyncMapping {}
impl SyncMapping {
    const fn new() -> Self { SyncMapping(UnsafeCell::new(None)) }
}
static CACHED_PROFILE_MAPPING: [SyncMapping; 4] = [
    SyncMapping::new(), SyncMapping::new(),
    SyncMapping::new(), SyncMapping::new(),
];

/// Cache the profile mapping for an entry. Called at CSS when tag is set.
unsafe fn cache_profile_mapping(entry_id: usize, tag: u32) {
    if entry_id < 4 {
        let mapping = if tag > 0 { get_profile_mapping(tag) } else { None };
        *CACHED_PROFILE_MAPPING[entry_id].0.get() = mapping;
    }
}

/// Get the cached profile mapping for an entry.
fn get_cached_profile_mapping(entry_id: usize) -> Option<ControllerMapping> {
    if entry_id < 4 {
        unsafe { *CACHED_PROFILE_MAPPING[entry_id].0.get() }
    } else {
        None
    }
}

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

/// Offset of the `ldr w19, [x20]` instruction inside the CSS restoration loop.
/// This instruction loads the controller slot from the second BSS array entry.
/// If the value is -1 (0xFFFFFFFF), the loop takes an alternate path that
/// restores the panel as CPU instead of human.
///
/// Our inline hook writes the saved controller slot to [X20] just before
/// this load executes, so human entries get restored correctly.
///
/// Registers at this point:
///   X20 = second BSS array entry pointer (stride 0x240)
///   X21 = loop index (0-based, = entry_index)
///   X25 = scene object
const OFFSET_CSS_RESTORE_LOOP: usize = 0x1843144;

/// Offset of the `cmp x21, x23` instruction at the restoration loop's back-edge.
/// The loop increments X21, then compares to X23 and branches back if not equal.
/// X23 holds the loop bound (normally 2 for training mode). We hook this to
/// override X23 so the loop processes all 4 entries.
///
/// GDB-confirmed register state at this point:
///   X21 = loop counter (already incremented)
///   X23 = loop bound (restored to 2 from stack, clobbering any earlier override)
const OFFSET_CSS_RESTORE_LOOP_BOUND: usize = 0x1843A00;

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

/// Offset of FUN_710064f820: Lua AI orchestrator — iterates all fighter entries
/// and runs AI think (path_builder → ai_init → vtable tick) per entry per frame.
/// Hooking here lets us skip the entire AI pipeline for human-controlled entries.
const OFFSET_LUA_AI_ORCHESTRATOR: usize = 0x64f820;

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

/// Offset of make_panel_human ($main + 0x1A1CF90).
/// Signature: fn(scene, panel_vec_entry, vectorA_entry, arg3: u32, arg4: u32)
///   arg3 = 1 during natural join (Start press), arg4 = 0.
/// Full human join flow: calls set_panel_type(panel, 0) internally, plus creates
/// cursor (hand), token (medal), controller binding, and tag/profile setup.
/// GDB-confirmed: this is the function called when pressing Start to join a CPU slot.
const OFFSET_MAKE_PANEL_HUMAN: usize = 0x1A1CF90;

/// Offset of the state_toggle_handler ($main + 0x1A1DBF0).
/// Signature: fn(scene: *mut u8, vec_entry: *mut u8, new_type: u32)
/// Properly transitions a panel between states including hash, token, level, etc.
const OFFSET_STATE_TOGGLE: usize = 0x1A1DBF0;

/// Offset of FUN_71032d0280: CSS medal (token) color setup.
/// Signature: fn(medal_mgr: *mut u8, medal_idx: u32, player_idx: u32,
///               player_type: u32, team_color: i32, display_data: u32)
/// Called during CSS panel init. team_color = -1 for no team, 0-3 for team color.
/// When team_color != -1: applies "team_color_%d" animation (team_color+1).
/// Also checks per-medal type field at +0x14 (stride 0x130): 1 = team medal appearance.
const OFFSET_CSS_MEDAL_COLOR: usize = 0x32D0280;

/// Offset of FUN_71032d28cc: CSS hand (cursor) color setup.
/// Signature: fn(hand_struct: *mut u8, player_idx: u32, player_type: u32, team_color: i32)
/// When team_color != -1: applies "team_color_%d" animation (team_color+1).
const OFFSET_CSS_HAND_COLOR: usize = 0x32D28CC;

/// Offset of FUN_71002c5cf0: main game update tick, runs per-frame on MainThread.
/// Signature: fn(param_1: *mut u8). Hooked to execute deferred state toggles
/// that require MainThread context (state_toggle crashes from draw hook thread).
const OFFSET_GAME_TICK: usize = 0x2C5CF0;

/// Deferred P2 state toggle: set by draw hook, consumed by game_tick hook on MainThread.
/// false = nothing pending, true = call state_toggle(scene, vec_entry_p2, 1).
static DEFERRED_P2_TOGGLE: AtomicBool = AtomicBool::new(false);

/// One-shot flag: set by clone_write_hook (entry 0), consumed by game_tick_hook.
/// Signals that fi_data+0x2C must be written before the first render pass.
static OUTLINE_INIT_PENDING: AtomicBool = AtomicBool::new(false);

/// Offset of FUN_7101db1910 (btn_rule handler): manages the Solo/Team toggle
/// button on the CSS. Reads scene_obj+0x44d as a transient "press in progress"
/// flag; returns 1 when the toggle animation completes.
const OFFSET_BTN_RULE_HANDLER: usize = 0x1db1910;

/// Offset of resolve_ui_chara_hash_to_kind (FUN_7103262130): resolves a ui_chara
/// hash to fighter_kind via binary search. Returns kind (i32) or -1/0x77 on failure.
const OFFSET_RESOLVE_HASH_TO_KIND: usize = 0x3262130;

/// Offset of check_char_availability (FUN_7103262710): returns character availability
/// state: 0=available, 1=locked(base), 2=DLC not purchased, 3=DLC available, 4=unknown.
const OFFSET_CHECK_CHAR_AVAILABILITY: usize = 0x3262710;

/// BSS root for the game's resource/character database manager.
/// DAT_710532e730 → *(+8) → inner; *(inner+0x168) = hash DB struct pointer.
const CHAR_DB_ROOT_BSS: usize = 0x532e730;

/// Team mode flag — persists across training resets, toggled at CSS.
/// true = Team Battle, false = Solo Battle (default).
static TEAM_MODE: AtomicBool = AtomicBool::new(false);

/// Address of the vanilla team battle flag byte in .bss.
/// DAT_71052c41e8 — read by app::global_parameter::is_team_battle().
const TEAM_BATTLE_FLAG_BSS: usize = 0x52c41e8;

/// Diagnostic: periodic state_toggle timer for P2. Non-zero = active, counts frames.
static P2_TOGGLE_TIMER: AtomicU32 = AtomicU32::new(0);

pub fn is_team_mode() -> bool {
    TEAM_MODE.load(Ordering::Relaxed)
}

fn team_outlines_enabled() -> bool {
    read(&MENU).team_outlines == OnOff::ON
}

/// Keep vanilla is_team_battle() in sync with our TEAM_MODE flag.
/// Always set when team mode is active — the flag controls the INT_ARRAY
/// color mapping in get_team_color(), which is needed by smoke trails,
/// HUD elements, and all other effect systems (10+ callers).
/// Outline *colors* are separately controlled by set_outline_team_color
/// writing fi_data+0x2C, gated on team_outlines_enabled().
/// Called once per frame from once_per_frame_per_fighter (entry 0).
pub unsafe fn sync_team_battle_flag() {
    let text_base = skyline::hooks::getRegionAddress(
        skyline::hooks::Region::Text,
    ) as usize;
    let flag_ptr = (text_base + TEAM_BATTLE_FLAG_BSS) as *mut u8;
    let desired = if TEAM_MODE.load(Ordering::Relaxed) {
        1u8
    } else {
        0u8
    };
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
            // Poll all NpadIds (0-7) via FullKey, plus handheld via
            // GetNpadHandheldState (FullKey doesn't return data for 0x20).
            let mut combined: u64 = 0;
            for npad_id in 0..8i32 {
                skyline::nn::hid::GetNpadFullKeyState(
                    buf.as_mut_ptr() as _,
                    &npad_id as *const _ as _,
                );
                let buttons = core::ptr::read_volatile(buf.as_ptr().add(0x08) as *const u64);
                combined |= buttons;
            }
            // Handheld (attached Joy-Cons): same struct layout, Buttons at +0x08.
            let handheld_id = 0x20u32;
            skyline::nn::hid::GetNpadHandheldState(
                buf.as_mut_ptr() as _,
                &handheld_id as *const _ as _,
            );
            let buttons = core::ptr::read_volatile(buf.as_ptr().add(0x08) as *const u64);
            combined |= buttons;
            HID_POLL_CURRENT.store(combined, Ordering::Relaxed);
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

/// Status bar team colors with zero black (matching vanilla bar style).
const STATUS_BAR_COLORS: [ResColor; 4] = [
    ResColor { r: 255, g: 20, b: 20, a: 255 },   // Red team
    ResColor { r: 20, g: 50, b: 255, a: 255 },    // Blue team
    ResColor { r: 20, g: 255, b: 20, a: 255 },    // Green team
    ResColor { r: 255, g: 240, b: 20, a: 255 },   // Yellow team
];
const STATUS_BAR_BLACK: ResColor = ResColor { r: 0, g: 0, b: 0, a: 0 };

/// Lighter/pastel nameplate colors (matching vanilla's style, e.g. P1 vanilla = 255,114,114,200).
const NAMEPLATE_COLORS: [ResColor; 4] = [
    ResColor { r: 255, g: 114, b: 114, a: 200 },  // Light red
    ResColor { r: 114, g: 150, b: 255, a: 200 },  // Light blue
    ResColor { r: 114, g: 230, b: 130, a: 200 },  // Light green
    ResColor { r: 255, g: 230, b: 100, a: 200 },  // Light yellow
];

/// Darker variant of status bar colors for the left-side background (stripes sit on top).
const STATUS_BAR_COLORS_DARK: [ResColor; 4] = [
    ResColor { r: 140, g: 10, b: 10, a: 255 },    // Dark red
    ResColor { r: 10, g: 25, b: 140, a: 255 },     // Dark blue
    ResColor { r: 10, g: 140, b: 10, a: 255 },     // Dark green
    ResColor { r: 140, g: 130, b: 10, a: 255 },    // Dark yellow
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

/// Get the content Material for a Window pane via two-step chain: pane+0x110 → +0x08 → Material.
unsafe fn window_content_material(pane: *mut Pane) -> *mut Material {
    let descriptor = *((pane as *const u8).add(0x110) as *const *const u8);
    if descriptor.is_null() { return core::ptr::null_mut(); }
    *(descriptor.add(0x08) as *const *mut Material)
}

/// Get the secondary Material for a Window pane at pane+0x118.
/// Used for nameplate color (btn_color_off) where the animation writes the player color.
unsafe fn window_secondary_material(pane: *mut Pane) -> *mut Material {
    *((pane as *const u8).add(0x118) as *const *mut Material)
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

    // Check ALL controllers (any connected controller can toggle).
    let mut just_x = false;
    let mut just_dup = false;
    let mut just_drt = false;
    let mut just_ddn = false;
    let mut just_dlt = false;
    let mut held_zr = false;
    let mut any_controller = false;

    for npad in 0..CONTROLLER_PTRS.len() {
        let ctrl_addr = CONTROLLER_PTRS[npad].load(Ordering::Relaxed);
        if ctrl_addr != 0 {
            any_controller = true;
            let c = &*(ctrl_addr as *const Controller);
            just_x |= c.just_down.x();
            just_dup |= c.just_down.dpad_up();
            just_drt |= c.just_down.dpad_right();
            just_ddn |= c.just_down.dpad_down();
            just_dlt |= c.just_down.dpad_left();
            held_zr |= c.current_buttons.zr();
        }
    }

    if !any_controller {
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
            let new_slots: u32 = if new_val { 4 } else { 2 };
            // Read the OLD visible slot count before changing anything.
            // css_panel_layout uses the difference between X1 (old count)
            // and scene+0x160 (new count) to tear down / set up slots.
            let old_slots = core::ptr::read_volatile(
                scene_ptr.add(0x160) as *const u32
            );
            // Write the NEW visible slot count to scene+0x160 first,
            // then pass the OLD count as X1 — matching the game's reduce
            // button behavior (GDB-confirmed).
            type PanelLayoutFn = unsafe extern "C" fn(*mut u8, u32, u32);
            let orig_fn: PanelLayoutFn = core::mem::transmute(orig_addr);
            if new_val {
                // Expanding to teams: 2→4 slots.
                core::ptr::write_volatile(scene_ptr.add(0x160) as *mut u32, new_slots);
                core::ptr::write_volatile(scene_ptr.add(0x180) as *mut u32, new_slots);
                core::ptr::write_volatile(mode_ptr, new_mode);
                orig_fn(scene_ptr, old_slots, 1);
                cache_panel_ptrs(scene_ptr);
                // Mark P2/P3/P4 eligible for controller takeover
                // (panel+0x1C0 = 1, GDB-confirmed gate).
                for i in 1..4 {
                    let panel = CSS_PANEL_PTRS[i].load(Ordering::Relaxed);
                    if panel != 0 {
                        core::ptr::write_volatile((panel as *mut u8).add(0x1C0), 1u8);
                    }
                }
            } else {
                // Collapsing to solo: reduce to 1 first so css_panel_layout
                // tears down P2's cursor/token via its built-in cleanup,
                // then expand back to 2 for a fresh P2 CPU slot.
                // Step 1: Reduce 4→1 (tears down P2, P3, P4 with cursor cleanup).
                core::ptr::write_volatile(scene_ptr.add(0x160) as *mut u32, 1);
                core::ptr::write_volatile(mode_ptr, 0x6u32); // keep smash mode for teardown
                orig_fn(scene_ptr, old_slots, 1);
                // Step 2: Expand 1→2 (adds fresh P2 slot).
                core::ptr::write_volatile(scene_ptr.add(0x160) as *mut u32, 2);
                core::ptr::write_volatile(scene_ptr.add(0x180) as *mut u32, 2);
                core::ptr::write_volatile(mode_ptr, new_mode); // now set training mode
                orig_fn(scene_ptr, 1, 1);
                cache_panel_ptrs(scene_ptr);
                // Lock P2 against controller takeover in solo mode.
                let p2 = CSS_PANEL_PTRS[1].load(Ordering::Relaxed);
                if p2 != 0 {
                    core::ptr::write_volatile((p2 as *mut u8).add(0x1C0), 0u8);
                }
                // Defer P2 init to MainThread via game_tick hook.
                // state_toggle must run on MainThread (crashes from TaskWorker).
                DEFERRED_P2_TOGGLE.store(true, Ordering::Relaxed);
            }
            debug_log(&format!(
                "CSS team toggle: team_mode={}, mode={:#x}, old_slots={}, new_slots={}",
                new_val, new_mode, old_slots, new_slots
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
                invalidate_hit_teams();
                // Directly refresh medal/hand colors for this player.
                DEFERRED_COLOR_REFRESH.store(1 << i, Ordering::Relaxed);
                refresh_medal_hand_colors();
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

/// Get the hash DB pointer used by the game's character database functions.
/// Pointer chain: DAT_710532e730 → *(+8) → *(+0x168) = hash DB struct ptr.
/// Returns 0 on failure (null at any level).
unsafe fn get_hash_db() -> usize {
    let text_base = skyline::hooks::getRegionAddress(
        skyline::hooks::Region::Text,
    ) as usize;
    let root = core::ptr::read_volatile((text_base + CHAR_DB_ROOT_BSS) as *const usize);
    if root == 0 { return 0; }
    let inner = core::ptr::read_volatile((root + 8) as *const usize);
    if inner == 0 { return 0; }
    core::ptr::read_volatile((inner + 0x168) as *const usize)
}

/// Check character availability via the game's native state function.
/// Returns: 0=available, 1=locked(base), 2=DLC not purchased, 3=DLC available,
/// 4=unknown. Returns -1 on failure (null pointers).
unsafe fn check_char_availability(hash: u64) -> i32 {
    let hash_db = get_hash_db();
    if hash_db == 0 { return -1; }
    let text_base = skyline::hooks::getRegionAddress(
        skyline::hooks::Region::Text,
    ) as usize;
    type CheckFn = unsafe extern "C" fn(usize, u64) -> i64;
    let check: CheckFn = core::mem::transmute(text_base + OFFSET_CHECK_CHAR_AVAILABILITY);
    check(hash_db, hash) as i32
}

/// Pick a random (fighter_kind, ui_chara_hash) from VALID_RANDOM_POOL using LCG PRNG.
/// Seeded lazily from CSS panel pointer addresses (vary with ASLR/allocation)
/// so different CSS sessions produce different sequences.
///
/// Each pick is validated: the character must be available (state 0 or 3) per the
/// game's native availability check. This gates out locked base-game characters and
/// unpurchased DLC on setups where not everything is unlocked (e.g. Yuzu).
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

    // Try up to 10 picks, validating availability through the game's native check.
    for _attempt in 0..10 {
        state = state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);

        let index = ((state >> 33) as usize) % VALID_RANDOM_POOL.len();
        let (kind, hash) = VALID_RANDOM_POOL[index];

        let avail = unsafe { check_char_availability(hash) };
        // 0 = available (base game), 3 = DLC available. Reject 1 (locked), 2 (not purchased).
        if avail == 0 || avail == 3 {
            RNG_STATE.store(state, Ordering::Relaxed);
            return (kind, hash);
        }
    }

    // Fallback: all 10 attempts were unavailable. Use the last pick anyway.
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
/// Cached status bar panes (wnd_color_r_on inside set_btn_panel) for team coloring.
static mut STATUS_BAR_PANES: [*mut Pane; 4] = [core::ptr::null_mut(); 4];
static mut ORIG_STATUS_WHITE: [ResColor; 4] = [ResColor { r: 255, g: 255, b: 255, a: 255 }; 4];
static mut ORIG_STATUS_BLACK: [ResColor; 4] = [ResColor { r: 0, g: 0, b: 0, a: 0 }; 4];
static mut STATUS_COLORS_SAVED: bool = false;
/// Cached left-side status bar Window panes (on + off variants for normal + hover states):
///   [0] wnd_color_l_on, [1] wnd_stripe_l_on, [2] wnd_color_l_all_on,
///   [3] wnd_color_l_off, [4] wnd_color_l_all_off.
/// Window panes store their content material via pane+0x110 → +0x08 and secondary at +0x118.
static mut STATUS_BAR_LEFT_PANES: [[*mut Pane; 5]; 4] = [[core::ptr::null_mut(); 5]; 4];
/// Cached nameplate color panes (btn_color_off inside btn_color_onoff).
/// The animation writes player color to the Window pane's +0x118 material each frame.
static mut NAMEPLATE_PANES: [*mut Pane; 4] = [core::ptr::null_mut(); 4];
/// Cached spirit button panes (set_btn_sp) — always hidden in doubles mod.
static mut SPIRIT_BTN_PANES: [*mut Pane; 4] = [core::ptr::null_mut(); 4];
/// Cached nameplate container panes (set_btn_name) for repositioning when spirits hidden.
static mut NAMEPLATE_CONTAINER_PANES: [*mut Pane; 4] = [core::ptr::null_mut(); 4];
/// Cached medal (token) Parts pane pointers — set_medal_00..07.
static mut CSS_MEDAL_PANES: [*mut Pane; 8] = [core::ptr::null_mut(); 8];
/// Cached hand (cursor) Parts pane pointers — set_hand_00..07.
static mut CSS_HAND_PANES: [*mut Pane; 8] = [core::ptr::null_mut(); 8];
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

    // One-shot: dump full pane tree of chara_select_base root to find token/cursor panes.
    debug_log("=== CSS PANE TREE (chara_select_base root, depth 3) ===");
    dump_pane_tree(root_pane, 0, 3);
    debug_log("=== END CSS ROOT TREE ===");

    let panel_names = ["set_panel_1p", "set_panel_2p", "set_panel_3p", "set_panel_4p"];
    for (i, pn) in panel_names.iter().enumerate() {
        let panel = find_pane_by_name(root_pane, pn);
        if panel.is_null() { continue; }

        // Diagnostic: dump set_panel_1p subtree at depth 6 to find token/cursor panes.
        if i == 0 {
            debug_log("=== CSS set_panel_1p SUBTREE (depth 6) ===");
            dump_pane_tree(panel as *const Pane, 0, 6);
            debug_log("=== END set_panel_1p SUBTREE ===");
        }

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

        // Cache status bar panes inside set_btn_panel.
        let btn_panel = find_pane_by_name(panel as *const Pane, "set_btn_panel");
        if !btn_panel.is_null() {
            // Right side (Picture pane — material at +0xD8 via as_picture).
            let sb = find_pane_by_name(btn_panel as *const Pane, "wnd_color_r_on");
            STATUS_BAR_PANES[i] = sb;
            if !sb.is_null() {
                let picture = (&mut *sb).as_picture();
                let mat = &*picture.material;
                ORIG_STATUS_WHITE[i] = read_material_color(mat, MaterialColorType::WhiteColor);
                ORIG_STATUS_BLACK[i] = read_material_color(mat, MaterialColorType::BlackColor);
                STATUS_COLORS_SAVED = true;
            }
            // Left side (Window panes — content material via pane+0x110 → +0x08).
            let left_names = [
                "wnd_color_l_on", "wnd_stripe_l_on", "wnd_color_l_all_on",
                "wnd_color_l_off", "wnd_color_l_all_off",
            ];
            for (li, lname) in left_names.iter().enumerate() {
                let lp = find_pane_by_name(btn_panel as *const Pane, lname);
                STATUS_BAR_LEFT_PANES[i][li] = lp;
            }
        }

        // Cache nameplate color pane (btn_color_off inside btn_color_onoff).
        // The animation writes the player color to btn_color_off's +0x118 material each frame.
        // We override it in the draw hook with the team color (bracket approach).
        let bco = find_pane_by_name(panel as *const Pane, "btn_color_onoff");
        if !bco.is_null() {
            let bcoff = find_pane_by_name(bco as *const Pane, "btn_color_off");
            NAMEPLATE_PANES[i] = bcoff;
        }

        // Cache spirit button and nameplate container panes.
        SPIRIT_BTN_PANES[i] = find_pane_by_name(panel as *const Pane, "set_btn_sp");
        NAMEPLATE_CONTAINER_PANES[i] = find_pane_by_name(panel as *const Pane, "set_btn_name");

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

    // Cache medal (token) and hand (cursor) Parts pane pointers.
    let medal_names = [
        "set_medal_00", "set_medal_01", "set_medal_02", "set_medal_03",
        "set_medal_04", "set_medal_05", "set_medal_06", "set_medal_07",
    ];
    for (i, name) in medal_names.iter().enumerate() {
        CSS_MEDAL_PANES[i] = find_pane_by_name(root_pane, name);
    }
    let hand_names = [
        "set_hand_00", "set_hand_01", "set_hand_02", "set_hand_03",
        "set_hand_04", "set_hand_05", "set_hand_06", "set_hand_07",
    ];
    for (i, name) in hand_names.iter().enumerate() {
        CSS_HAND_PANES[i] = find_pane_by_name(root_pane, name);
    }
    debug_log(&format!(
        "Cached medal/hand panes: medals={}, hands={}",
        CSS_MEDAL_PANES.iter().filter(|p| !p.is_null()).count(),
        CSS_HAND_PANES.iter().filter(|p| !p.is_null()).count(),
    ));
}

/// Per-player team color assignment. 0=red, 1=blue, 2=green, 3=yellow.
/// Default: P1/P3 = red(0), P2/P4 = blue(1).
static TEAM_COLORS: [AtomicU32; 4] = [
    AtomicU32::new(0), AtomicU32::new(1), AtomicU32::new(0), AtomicU32::new(1),
];

/// Set in-game portrait background colors to match team colors.
/// Called from handle_draw when layout is `info_melee` and team mode is active.
///
/// The vanilla game uses animation frame values on each player's Parts pane
/// sub-layout to select portrait colors. The first AnimTransform in the
/// anim_trans_list targets fp_bg/face_bg panes; its frame value selects the
/// color preset: 9=Red, 10=Blue, 11=Green, 12=Yellow.
pub unsafe fn melee_portrait_team_colors(root_pane: &Pane) {
    for (i, player_name) in ["p1", "p2", "p3", "p4"].iter().enumerate() {
        let color = TEAM_COLORS[i].load(Ordering::Relaxed);
        let target_frame = 9.0 + color as f32;

        if let Some(parent) = root_pane.find_pane_by_name_recursive(player_name) {
            let layout = &mut *parent.as_parts().layout;
            let anim_root = &mut layout.anim_trans_list as *mut AnimTransformNode;

            // The first real node in the circular list is the portrait color animation.
            let first_node = (*anim_root).next;
            if first_node.is_null() || std::ptr::eq(first_node, anim_root) {
                continue;
            }
            // AnimTransform sits right after the AnimTransformNode (at node + 0x10).
            let anim_transform = (first_node as *mut u64).add(2) as *mut AnimTransform;
            (*anim_transform).frame = target_frame;
        }
    }
}

/// Cursor color frame lookup. Human entries use "light" team color frames,
/// CPU entries use "bold" frames (same as portrait mapping).
///
/// Human: Red=0, Blue=1, Yellow=2, Green=3  (Ghidra param - 1)
/// CPU:   Red=9, Blue=10, Green=11, Yellow=12  (9 + team_color_index)
const CURSOR_FRAME_HUMAN: [f32; 4] = [0.0, 1.0, 3.0, 2.0]; // indexed by team_color

/// Set floating player cursor text colors to match team colors.
/// Called from handle_draw when layout is `info_playercursor` and team mode is active.
///
/// The pane hierarchy is: root → 720p → set_cursor_p1..p4 (Parts panes).
/// Each Parts pane's sub-layout has an anim_trans_list whose first node targets
/// set_txt_num/set_txt_name/set_pic_fp. The animation frame selects the color.
pub unsafe fn playercursor_team_colors(root_pane: &Pane) {
    for (i, cursor_name) in ["set_cursor_p1", "set_cursor_p2", "set_cursor_p3", "set_cursor_p4"]
        .iter()
        .enumerate()
    {
        if let Some(cursor_pane) = root_pane.find_pane_by_name_recursive(cursor_name) {
            // set_cursor_p* are Parts panes; read layout pointer at +0xE8
            let layout_ptr = *((cursor_pane as *const Pane as usize + 0xE8) as *const *mut Layout);
            if layout_ptr.is_null() {
                continue;
            }
            let layout = &mut *layout_ptr;
            let anim_root = &mut layout.anim_trans_list as *mut AnimTransformNode;
            let first_node = (*anim_root).next;
            if first_node.is_null() || std::ptr::eq(first_node, anim_root) {
                continue;
            }
            let anim_transform = (first_node as *mut u64).add(2) as *mut AnimTransform;
            let color = TEAM_COLORS[i].load(Ordering::Relaxed).min(3) as usize;
            let frame = if is_human_entry(i as i32) {
                CURSOR_FRAME_HUMAN[color]
            } else {
                9.0 + color as f32
            };
            (*anim_transform).frame = frame;
        }
    }
}

// ---------------------------------------------------------------------------
// CSS layout name logger — one-shot diagnostic to discover all layouts
// drawn during CSS. Captures unique layout names while CSS scene is active.
// ---------------------------------------------------------------------------
static mut CSS_LAYOUT_NAMES_LOGGED: bool = false;
static mut CSS_LAYOUT_NAMES: Option<std::collections::BTreeSet<String>> = None;

/// Called from handle_draw for every layout. When CSS scene is active,
/// dumps the `chara_select` layout tree and probes hand/medal AnimTransforms.
pub unsafe fn css_layout_name_logger(layout_name: &str) {
    let scene = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
    if scene == 0 {
        CSS_LAYOUT_NAMES_LOGGED = false;
        return;
    }
    if CSS_LAYOUT_NAMES_LOGGED {
        return;
    }

    // Dump the `chara_select` layout pane tree (separate from chara_select_base).
    if layout_name == "chara_select" {
        // Get root pane from the Layout* that handle_draw receives.
        // We can't access it here directly, so use a flag + do it in css_btn_rule_draw.
        // Instead, just mark that we've seen it. The actual dump happens below.
    }

    // Wait for chara_select_base draw to probe hand/medal panes.
    if layout_name != "chara_select_base" {
        return;
    }

    static CSS_DIAG_FRAME: AtomicU32 = AtomicU32::new(0);
    let frame = CSS_DIAG_FRAME.fetch_add(1, Ordering::Relaxed);
    // Wait 180 frames to ensure CSS is fully initialized.
    if frame != 180 {
        return;
    }
    CSS_LAYOUT_NAMES_LOGGED = true;
}

/// Probe a Parts pane's AnimTransform list and log frame values.
unsafe fn probe_parts_anim(pane: *const Pane, label: &str) {
    if pane.is_null() {
        debug_log(&format!("  {}: NULL pane", label));
        return;
    }
    // Parts pane has layout ptr at +0xE8.
    let layout_ptr = *((pane as usize + 0xE8) as *const *mut Layout);
    if layout_ptr.is_null() {
        debug_log(&format!("  {}: layout ptr at +0xE8 is NULL (not a Parts pane?)", label));
        return;
    }
    let layout = &*layout_ptr;
    let layout_name = skyline::from_c_str(layout.layout_name);
    debug_log(&format!("  {}: sub-layout = '{}'", label, layout_name));

    // Walk anim_trans_list (circular linked list).
    let anim_root = &layout.anim_trans_list as *const AnimTransformNode;
    let mut node = (*anim_root).next;
    let mut idx = 0u32;
    while !node.is_null() && !std::ptr::eq(node, anim_root) && idx < 10 {
        let anim = (node as *const u64).add(2) as *const AnimTransform;
        let frame_val = (*anim).frame;
        // Try to read animation name if available.
        debug_log(&format!("    anim[{}]: frame={:.1}", idx, frame_val));
        node = (*node).next;
        idx += 1;
    }
    if idx == 0 {
        debug_log(&format!("  {}: anim_trans_list is EMPTY", label));
    }
}

/// One-shot diagnostic: probe hand/medal panes for AnimTransform data.
/// Called from css_btn_rule_draw after the logger sets the flag.
pub unsafe fn css_probe_hand_medal(root_pane: &Pane) {
    static PROBED: AtomicBool = AtomicBool::new(false);
    if PROBED.swap(true, Ordering::SeqCst) {
        return;
    }

    debug_log("=== CSS HAND/MEDAL ANIMTRANSFORM PROBE ===");

    // Probe set_hand_00 and set_hand_01 (cursors).
    for name in &["set_hand_00", "set_hand_01", "set_hand_02", "set_hand_03"] {
        let pane = find_pane_by_name(root_pane as *const Pane, name);
        probe_parts_anim(pane, name);
    }

    // Probe set_medal_00 through set_medal_03 (tokens).
    for name in &["set_medal_00", "set_medal_01", "set_medal_02", "set_medal_03"] {
        let pane = find_pane_by_name(root_pane as *const Pane, name);
        probe_parts_anim(pane, name);
    }

    // Probe set_medal_hold_00 through set_medal_hold_03.
    for name in &["set_medal_hold_00", "set_medal_hold_01", "set_medal_hold_02", "set_medal_hold_03"] {
        let pane = find_pane_by_name(root_pane as *const Pane, name);
        probe_parts_anim(pane, name);
    }

    // Probe set_medal_over_00 through set_medal_over_03.
    for name in &["set_medal_over_00", "set_medal_over_01", "set_medal_over_02", "set_medal_over_03"] {
        let pane = find_pane_by_name(root_pane as *const Pane, name);
        probe_parts_anim(pane, name);
    }

    // Also dump deeper into set_hand_00 subtree.
    let hand0 = find_pane_by_name(root_pane as *const Pane, "set_hand_00");
    if !hand0.is_null() {
        debug_log("=== set_hand_00 SUBTREE (depth 4) ===");
        dump_pane_tree(hand0, 0, 4);
    }

    let medal0 = find_pane_by_name(root_pane as *const Pane, "set_medal_00");
    if !medal0.is_null() {
        debug_log("=== set_medal_00 SUBTREE (depth 4) ===");
        dump_pane_tree(medal0, 0, 4);
    }

    debug_log("=== END HAND/MEDAL PROBE ===");
}

/// One-shot dump of the `chara_select` layout (separate from chara_select_base).
pub unsafe fn css_dump_chara_select_layout(root_pane: &Pane, layout_name: &str) {
    if layout_name != "chara_select" {
        return;
    }
    let scene = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
    if scene == 0 {
        return;
    }
    static DUMPED: AtomicBool = AtomicBool::new(false);
    if DUMPED.swap(true, Ordering::SeqCst) {
        return;
    }
    debug_log("=== CHARA_SELECT LAYOUT PANE TREE (depth 4) ===");
    dump_pane_tree(root_pane as *const Pane, 0, 4);
    debug_log("=== END CHARA_SELECT LAYOUT ===");
}

// ---------------------------------------------------------------------------
// Loupe / Radar diagnostic dump — one-shot pane tree + anim probe
// ---------------------------------------------------------------------------
pub unsafe fn loupe_radar_diagnostic_dump(root_pane: &Pane, layout_name: &str) {
    match layout_name {
        "info_loupe" => {
            static DUMPED_LOUPE: AtomicBool = AtomicBool::new(false);
            if DUMPED_LOUPE.swap(true, Ordering::SeqCst) { return; }
            debug_log("=== INFO_LOUPE PANE TREE (depth 5) ===");
            dump_pane_tree(root_pane as *const Pane, 0, 5);
            debug_log("=== END INFO_LOUPE ===");

            // Probe per-player loupe Parts panes for AnimTransform data.
            for i in 1..=4 {
                let name = format!("set_parts_loupe_p{}", i);
                let pane = find_pane_by_name(root_pane as *const Pane, &name);
                probe_parts_anim(pane, &name);
            }
            // Probe arrow panes for color animations.
            for i in 0..4 {
                let name_l = format!("set_parts_arrow_l_{:02}", i);
                let name_r = format!("set_parts_arrow_r_{:02}", i);
                let pane_l = find_pane_by_name(root_pane as *const Pane, &name_l);
                let pane_r = find_pane_by_name(root_pane as *const Pane, &name_r);
                probe_parts_anim(pane_l, &name_l);
                probe_parts_anim(pane_r, &name_r);
            }
            // Probe arrow sub-pane materials: arrow_l_a vs arrow_l for two-tone.
            // Only probe P1 left arrow (set_parts_arrow_l_00) for brevity.
            let arrow_parts = find_pane_by_name(root_pane as *const Pane, "set_parts_arrow_l_00");
            if !arrow_parts.is_null() {
                for sub_name in &["arrow_l_a", "arrow_l", "arrow_c_a", "arrow_c", "arrow_r_a", "arrow_r"] {
                    let sub = (*(arrow_parts as *const Pane)).find_pane_by_name_recursive(sub_name);
                    if let Some(p) = sub {
                        // Try as Picture pane (material at as_picture())
                        let picture = p.as_picture();
                        let mat = &*picture.material;
                        let w = read_material_color(mat, MaterialColorType::WhiteColor);
                        let b = read_material_color(mat, MaterialColorType::BlackColor);
                        debug_log(&format!(
                            "  ARROW_MAT {}: white=({},{},{},{}) black=({},{},{},{})",
                            sub_name, w.r, w.g, w.b, w.a, b.r, b.g, b.b, b.a
                        ));
                    } else {
                        debug_log(&format!("  ARROW_MAT {}: pane NOT FOUND", sub_name));
                    }
                }
            }
            debug_log("=== END LOUPE ANIM PROBE ===");
        }
        "info_radar_a" => {
            static DUMPED_RADAR_A: AtomicBool = AtomicBool::new(false);
            if DUMPED_RADAR_A.swap(true, Ordering::SeqCst) { return; }
            debug_log("=== INFO_RADAR_A PANE TREE (depth 5) ===");
            dump_pane_tree(root_pane as *const Pane, 0, 5);
            debug_log("=== END INFO_RADAR_A ===");
            // Probe radar marker panes.
            for i in 1..=4 {
                let name = format!("set_parts_marker_{:02}", i);
                let pane = find_pane_by_name(root_pane as *const Pane, &name);
                probe_parts_anim(pane, &name);
            }
            debug_log("=== END RADAR_A MARKER PROBE ===");
        }
        "info_radar_b" => {
            static DUMPED_RADAR_B: AtomicBool = AtomicBool::new(false);
            if DUMPED_RADAR_B.swap(true, Ordering::SeqCst) { return; }
            debug_log("=== INFO_RADAR_B PANE TREE (depth 5) ===");
            dump_pane_tree(root_pane as *const Pane, 0, 5);
            debug_log("=== END INFO_RADAR_B ===");
        }
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Loupe (off-screen indicator) team colors
// ---------------------------------------------------------------------------

/// Set loupe bubble and arrow colors to match team colors.
/// Called from handle_draw when layout is `info_loupe` and team mode is active.
///
/// The `set_parts_loupe_p%d` Parts panes (sub-layout `info_loupe_lct_00`) each
/// have an anim_trans_list where anim[0] (`p_col`) controls the bubble color.
/// Frame mapping: 10=Red, 11=Blue, 12=Yellow(?), 13=Green(?) — base 10.
///
/// The `set_parts_arrow_{l,r}_%02d` Parts panes (sub-layout `info_melee_lct_arrow`)
/// have anim[0]=arrow_anim, anim[1]=arrow_color. Same base 10 mapping.
/// Known issue: Green team (frame 12) shows as dark blue for trio arrows.
pub unsafe fn loupe_team_colors(root_pane: &Pane) {
    // Loupe bubbles: set_parts_loupe_p1 through p4 (1-indexed).
    for i in 0..4usize {
        let color = TEAM_COLORS[i].load(Ordering::Relaxed);
        let target_frame = 10.0 + color as f32;

        let name = format!("set_parts_loupe_p{}", i + 1);
        if let Some(pane) = root_pane.find_pane_by_name_recursive(&name) {
            let layout_ptr = *((pane as *const Pane as usize + 0xE8) as *const *mut Layout);
            if layout_ptr.is_null() { continue; }
            let layout = &mut *layout_ptr;
            let anim_root = &mut layout.anim_trans_list as *mut AnimTransformNode;
            let first_node = (*anim_root).next;
            if first_node.is_null() || std::ptr::eq(first_node, anim_root) { continue; }
            let anim_transform = (first_node as *mut u64).add(2) as *mut AnimTransform;
            (*anim_transform).frame = target_frame;
        }
    }

    // Directional trio arrows: set_parts_arrow_{l,r}_%02d (0-indexed).
    // Two-tone mechanism:
    //   - _a panes (arrow_l_a etc): colored by arrow_color animation (light variant)
    //   - inner panes (arrow_l etc): colored by material black_color (dark variant)
    //   - arrow_anim alternates visibility between them
    // Arrow color paired frames: 9/10=Red, 11/12=Blue, 13/14=Yellow, 15/16=Green.
    const ARROW_COLOR_FRAME: [f32; 4] = [9.0, 11.0, 15.0, 13.0];
    //                       team_color: Red   Blue  Green Yellow
    // Dark variant material colors for inner panes (matching vanilla P1 red = 255,4,4,0).
    const ARROW_DARK_COLOR: [ResColor; 4] = [
        ResColor { r: 255, g: 4, b: 4, a: 0 },     // Red
        ResColor { r: 4, g: 4, b: 255, a: 0 },      // Blue
        ResColor { r: 4, g: 255, b: 4, a: 0 },      // Green
        ResColor { r: 255, g: 255, b: 4, a: 0 },    // Yellow
    ];
    for i in 0..4usize {
        let color = TEAM_COLORS[i].load(Ordering::Relaxed).min(3) as usize;
        let target_frame = ARROW_COLOR_FRAME[color];
        let dark_color = &ARROW_DARK_COLOR[color];

        for prefix in &["set_parts_arrow_l_", "set_parts_arrow_r_"] {
            let name = format!("{}{:02}", prefix, i);
            if let Some(pane) = root_pane.find_pane_by_name_recursive(&name) {
                let layout_ptr = *((pane as *const Pane as usize + 0xE8) as *const *mut Layout);
                if layout_ptr.is_null() { continue; }
                let layout = &mut *layout_ptr;
                let anim_root = &mut layout.anim_trans_list as *mut AnimTransformNode;
                let first_node = (*anim_root).next;
                if first_node.is_null() || std::ptr::eq(first_node, anim_root) { continue; }
                // Skip first (arrow_anim), get second (arrow_color).
                let second_node = (*first_node).next;
                if second_node.is_null() || std::ptr::eq(second_node, anim_root) { continue; }
                let anim_transform = (second_node as *mut u64).add(2) as *mut AnimTransform;
                (*anim_transform).frame = target_frame;

                // Also write dark color to inner panes' material black_color.
                for inner_name in &["arrow_l", "arrow_c", "arrow_r"] {
                    if let Some(inner) = pane.find_pane_by_name_recursive(inner_name) {
                        let mat = &mut *inner.as_picture().material;
                        mat.set_black_res_color(*dark_color);
                    }
                }
            }
        }
    }
}

/// Set minimap radar marker colors to match team colors.
/// Called from handle_draw when layout is `info_radar_a` and team mode is active.
///
/// `set_parts_marker_01`–`08` (1-indexed) each have anim[0] controlling color.
/// Vanilla: P1=frame 9, P2+=frame 10 (cloned default). Base appears to be 9.
pub unsafe fn radar_marker_team_colors(root_pane: &Pane) {
    for i in 0..4usize {
        let color = TEAM_COLORS[i].load(Ordering::Relaxed);
        // Radar markers use base 9 (from probe: P1=9=red, P2=10=blue).
        let target_frame = 9.0 + color as f32;

        let name = format!("set_parts_marker_{:02}", i + 1);
        if let Some(pane) = root_pane.find_pane_by_name_recursive(&name) {
            let layout_ptr = *((pane as *const Pane as usize + 0xE8) as *const *mut Layout);
            if layout_ptr.is_null() { continue; }
            let layout = &mut *layout_ptr;
            let anim_root = &mut layout.anim_trans_list as *mut AnimTransformNode;
            let first_node = (*anim_root).next;
            if first_node.is_null() || std::ptr::eq(first_node, anim_root) { continue; }
            let anim_transform = (first_node as *mut u64).add(2) as *mut AnimTransform;
            (*anim_transform).frame = target_frame;
        }
    }
}

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

    // One-shot probe of hand/medal AnimTransform data (DISABLED — crashes on stale pane ptr).
    // css_probe_hand_medal(root_pane);

    // Cache pane pointers once per CSS session.
    if FLAG_CACHE_SCENE.load(Ordering::Relaxed) != scene {
        FLAG_CACHE_SCENE.store(scene, Ordering::Relaxed);
        PANEL_COLORS_SAVED = false;
        STATUS_COLORS_SAVED = false;
        cache_flag_panes(root_pane as *const Pane);
        debug_log(&format!(
            "css_flags: cached panes for scene {:#x} (team[0]={:?})",
            scene, FLAG_PANE_CACHE[0].team
        ));
    }

    let team_mode = TEAM_MODE.load(Ordering::Relaxed);

    // Keep vanilla is_team_battle() in sync during CSS.
    sync_team_battle_flag();

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

            // Tint status bar to match team color (bracket approach).
            // Only for human slots — CPU slots keep their default gray.
            let panel_ptr = CSS_PANEL_PTRS[i].load(Ordering::Relaxed) as *const u8;
            let is_human = !panel_ptr.is_null()
                && core::ptr::read_volatile(panel_ptr.add(0x1F8) as *const u32) == 0;
            if is_human {
                let tc = STATUS_BAR_COLORS[color.min(3) as usize];
                // Right side (Picture pane).
                let sb = STATUS_BAR_PANES[i];
                if !sb.is_null() {
                    let picture = (&mut *sb).as_picture();
                    let material = &mut *picture.material;
                    material.set_white_res_color(tc);
                    material.set_black_res_color(STATUS_BAR_BLACK);
                }
                // Left side (Window panes — write both content (+0x110→+0x08) and secondary (+0x118) materials).
                // [0] bg=dark, [1] stripes=normal, [2] all=dark, [3] off_bg=dark, [4] off_all=dark.
                let tc_dark = STATUS_BAR_COLORS_DARK[color.min(3) as usize];
                let left_colors = [tc_dark, tc, tc_dark, tc_dark, tc_dark];
                for li in 0..5 {
                    let lp = STATUS_BAR_LEFT_PANES[i][li];
                    if !lp.is_null() {
                        let mat = window_content_material(lp);
                        if !mat.is_null() {
                            (*mat).set_white_res_color(left_colors[li]);
                            (*mat).set_black_res_color(STATUS_BAR_BLACK);
                        }
                        let mat2 = window_secondary_material(lp);
                        if !mat2.is_null() {
                            (*mat2).set_white_res_color(left_colors[li]);
                        }
                    }
                }

                // Nameplate background (btn_color_off's +0x118 material) — lighter pastel.
                let np = NAMEPLATE_PANES[i];
                if !np.is_null() {
                    let mat = window_secondary_material(np);
                    if !mat.is_null() {
                        (*mat).set_white_res_color(NAMEPLATE_COLORS[color.min(3) as usize]);
                    }
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
            // Restore status bar to original colors.
            if STATUS_COLORS_SAVED {
                let sb = STATUS_BAR_PANES[i];
                if !sb.is_null() {
                    let picture = (&mut *sb).as_picture();
                    let material = &mut *picture.material;
                    material.set_white_res_color(ORIG_STATUS_WHITE[i]);
                    material.set_black_res_color(ORIG_STATUS_BLACK[i]);
                }
            }
        }

        // Always hide spirit button and center nameplate (spirits not relevant to doubles).
        let sp = SPIRIT_BTN_PANES[i];
        if !sp.is_null() {
            (*sp).flags &= !1;
        }
        // Expand nameplate into spirit button space.
        let nc = NAMEPLATE_CONTAINER_PANES[i];
        if !nc.is_null() {
            (*nc).pos_x = 0.0;    // Center (vanilla = -88)
            (*nc).size_x = 500.0;  // Widen container (vanilla = 380)
            // Widen hit area within set_btn_name.
            let hit = find_pane_by_name(nc as *const Pane, "hit");
            if !hit.is_null() { (*hit).size_x = 400.0; }
            // Widen background group + panes (not the text 'name' pane).
            let grp = find_pane_by_name(nc as *const Pane, "btn_select_grp");
            if !grp.is_null() { (*grp).size_x = 320.0; }
            let bco = find_pane_by_name(nc as *const Pane, "btn_color_onoff");
            if !bco.is_null() {
                (*bco).size_x = 320.0;
                // Widen internal Window children (background/outline).
                let sentinel = &(*bco).children_list as *const skyline::nn::ui2d::PaneNode
                    as *mut skyline::nn::ui2d::PaneNode;
                let mut cur = (*bco).children_list.next;
                while cur != sentinel {
                    let child = (cur as *const u8).sub(0x08) as *mut Pane;
                    (*child).size_x = 320.0;
                    cur = (*cur).next;
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// CSS medal (token) and hand (cursor) team color hooks
// ---------------------------------------------------------------------------

/// Saved medal function params for re-calling on team color change.
/// medal_mgr is the same for all medals; per-medal params saved in arrays.
static SAVED_MEDAL_MGR: AtomicUsize = AtomicUsize::new(0);
static SAVED_MEDAL_PLAYER_IDX: [AtomicU32; 8] = [
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
];
static SAVED_MEDAL_PLAYER_TYPE: [AtomicU32; 8] = [
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
];
static SAVED_MEDAL_DISPLAY: [AtomicU32; 8] = [
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
];

/// Saved hand struct pointers for re-calling on team color change.
static SAVED_HAND_STRUCT: [AtomicUsize; 8] = [
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
    AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0), AtomicUsize::new(0),
];
static SAVED_HAND_PLAYER_TYPE: [AtomicU32; 8] = [
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
];

/// Flag: set when ZR+D-pad changes a team color, consumed by game_tick_hook
/// to re-call medal/hand color functions. Bitmask of player indices (bit 0-3).
static DEFERRED_COLOR_REFRESH: AtomicU32 = AtomicU32::new(0);

/// Hook the CSS medal color setup function to inject team colors.
/// The game calls this during CSS panel init/refresh. In training mode,
/// it passes team_color=-1 (no team). We override with TEAM_COLORS[medal_idx]
/// when team mode is active. Does NOT change the medal type field — keeps
/// the default medal appearance (player number text like "P3", "CPU").
#[skyline::hook(offset = OFFSET_CSS_MEDAL_COLOR)]
pub unsafe fn css_medal_color_hook(
    medal_mgr: *mut u8,
    medal_idx: u32,
    player_idx: u32,
    player_type: u32,
    team_color: i32,
    display_data: u32,
) {
    // Save params for re-calling on color change.
    let idx = medal_idx as usize;
    if idx < 8 {
        SAVED_MEDAL_MGR.store(medal_mgr as usize, Ordering::Relaxed);
        SAVED_MEDAL_PLAYER_IDX[idx].store(player_idx, Ordering::Relaxed);
        SAVED_MEDAL_PLAYER_TYPE[idx].store(player_type, Ordering::Relaxed);
        SAVED_MEDAL_DISPLAY[idx].store(display_data, Ordering::Relaxed);
    }

    if is_team_mode() && idx < 4 {
        let color = TEAM_COLORS[idx].load(Ordering::Relaxed) as i32;
        call_original!(medal_mgr, medal_idx, player_idx, player_type, color, display_data);
    } else {
        call_original!(medal_mgr, medal_idx, player_idx, player_type, team_color, display_data);
    }
}

/// Hook the CSS hand (cursor) color setup function to inject team colors.
#[skyline::hook(offset = OFFSET_CSS_HAND_COLOR)]
pub unsafe fn css_hand_color_hook(
    hand_struct: *mut u8,
    player_idx: u32,
    player_type: u32,
    team_color: i32,
) {
    // Save params for re-calling on color change.
    let idx = player_idx as usize;
    if idx < 8 {
        SAVED_HAND_STRUCT[idx].store(hand_struct as usize, Ordering::Relaxed);
        SAVED_HAND_PLAYER_TYPE[idx].store(player_type, Ordering::Relaxed);
    }

    if is_team_mode() && idx < 4 {
        let color = TEAM_COLORS[idx].load(Ordering::Relaxed) as i32;
        call_original!(hand_struct, player_idx, player_type, color);
    } else {
        call_original!(hand_struct, player_idx, player_type, team_color);
    }
}

/// Re-call medal and hand color functions for players whose team color changed.
/// Called from game_tick_hook on MainThread.
pub unsafe fn refresh_medal_hand_colors() {
    let mask = DEFERRED_COLOR_REFRESH.swap(0, Ordering::Relaxed);
    if mask == 0 {
        return;
    }
    let scene = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
    if scene == 0 || !is_team_mode() {
        debug_log(&format!("refresh_medal_hand: skip (scene={:#x}, team={})",
            scene, is_team_mode()));
        return;
    }
    let medal_mgr = SAVED_MEDAL_MGR.load(Ordering::Relaxed) as *mut u8;
    if medal_mgr.is_null() {
        debug_log("refresh_medal_hand: skip (medal_mgr NULL)");
        return;
    }

    debug_log(&format!("refresh_medal_hand: mask={:#x}, medal_mgr={:#x}",
        mask, medal_mgr as usize));

    // Get the hooked function entry points — calling these enters our hook,
    // which injects the current TEAM_COLORS and calls the original.
    type MedalFn = unsafe extern "C" fn(*mut u8, u32, u32, u32, i32, u32);
    type HandFn = unsafe extern "C" fn(*mut u8, u32, u32, i32);
    let text_base = skyline::hooks::getRegionAddress(skyline::hooks::Region::Text) as usize;
    let medal_fn: MedalFn = std::mem::transmute(text_base + OFFSET_CSS_MEDAL_COLOR);
    let hand_fn: HandFn = std::mem::transmute(text_base + OFFSET_CSS_HAND_COLOR);

    for i in 0..4u32 {
        if mask & (1 << i) == 0 {
            continue;
        }
        let color = TEAM_COLORS[i as usize].load(Ordering::Relaxed) as i32;

        // Re-call medal color.
        let pidx = SAVED_MEDAL_PLAYER_IDX[i as usize].load(Ordering::Relaxed);
        let ptype = SAVED_MEDAL_PLAYER_TYPE[i as usize].load(Ordering::Relaxed);
        let disp = SAVED_MEDAL_DISPLAY[i as usize].load(Ordering::Relaxed);
        debug_log(&format!("  medal[{}]: pidx={}, ptype={}, color={}, disp={}",
            i, pidx, ptype, color, disp));
        medal_fn(medal_mgr, i, pidx, ptype, color, disp);

        // Re-call hand color.
        let hand = SAVED_HAND_STRUCT[i as usize].load(Ordering::Relaxed) as *mut u8;
        if !hand.is_null() {
            let htype = SAVED_HAND_PLAYER_TYPE[i as usize].load(Ordering::Relaxed);
            debug_log(&format!("  hand[{}]: struct={:#x}, ptype={}, color={}",
                i, hand as usize, htype, color));
            hand_fn(hand, i, htype, color);
        } else {
            debug_log(&format!("  hand[{}]: NULL struct, skipping", i));
        }
    }
    debug_log("refresh_medal_hand: done");
}

/// Called from handle_draw AFTER original!() to restore shared material colors.
/// This is the second half of the bracket: css_btn_rule_draw sets team colors,
/// original!() draws everything, then this restores originals so the shared
/// material doesn't persist team tinting to other panes (e.g., portraits).
pub unsafe fn css_post_draw(layout_name: &str) {
    if layout_name != "chara_select_base" {
        return;
    }
    if CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed) == 0 {
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
        // Restore status bar material after draw (bracket).
        if STATUS_COLORS_SAVED {
            let sb = STATUS_BAR_PANES[i];
            if !sb.is_null() {
                let picture = (&mut *sb).as_picture();
                let material = &mut *picture.material;
                material.set_white_res_color(ORIG_STATUS_WHITE[i]);
                material.set_black_res_color(ORIG_STATUS_BLACK[i]);
            }
        }
    }
}

/// Hook the main game update tick to execute deferred state toggles on MainThread.
/// state_toggle_handler crashes from TaskWorker (draw hook) because set_panel_type
/// accesses thread-local state. This hook runs on MainThread every frame.
#[skyline::hook(offset = OFFSET_GAME_TICK)]
pub unsafe fn game_tick_hook(param_1: *mut u8) {
    // One-shot: write team outline colors BEFORE the first render pass.
    // The rendering init at 0x60EB08 reads fi_data+0x2C once to bake the
    // outline color. After that, once_per_frame_per_fighter maintains it.
    if OUTLINE_INIT_PENDING.load(Ordering::Relaxed) {
        OUTLINE_INIT_PENDING.store(false, Ordering::Relaxed);
        if team_outlines_enabled() {
            for entry_id in 0..4i32 {
                let tc = TEAM_COLORS[entry_id as usize].load(Ordering::Relaxed);
                set_outline_team_color(entry_id, tc);
            }
        }
    }
    call_original!(param_1);
    // Refresh medal/hand colors when ZR+D-pad changes a team color.
    refresh_medal_hand_colors();
    if !DEFERRED_P2_TOGGLE.load(Ordering::Relaxed) {
        return;
    }
    DEFERRED_P2_TOGGLE.store(false, Ordering::Relaxed);
    let scene = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
    if scene == 0 {
        return;
    }
    let scene_ptr = scene as *mut u8;
    let vec_a_start = core::ptr::read_volatile(
        scene_ptr.add(0x238) as *const usize,
    );
    if vec_a_start == 0 {
        return;
    }
    let vec_entry_p2 = (vec_a_start + 0x10) as *mut u8;
    let text = skyline::hooks::getRegionAddress(
        skyline::hooks::Region::Text,
    ) as usize;
    type StateToggleFn = unsafe extern "C" fn(*mut u8, *mut u8, u32);
    let state_toggle: StateToggleFn = core::mem::transmute(
        text + OFFSET_STATE_TOGGLE,
    );
    state_toggle(scene_ptr, vec_entry_p2, 1); // None → CPU
    // Lock P2 against controller takeover in solo mode.
    let p2 = CSS_PANEL_PTRS[1].load(Ordering::Relaxed);
    if p2 != 0 {
        core::ptr::write_volatile((p2 as *mut u8).add(0x1C0), 0u8);
    }
    debug_log("game_tick: deferred P2 state_toggle → CPU (MainThread)");
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
        invalidate_hit_teams();
        debug_log(&format!("btn_rule toggle: team_mode = {}", new_val));
    }
    result
}

/// Hook set_panel_type to prevent P1 (entry 0) from ever being set to CPU.
/// Training mode softlocks if P1 is CPU — no human-controlled fighter exists.
#[skyline::hook(offset = OFFSET_SET_PANEL_TYPE)]
pub unsafe fn set_panel_type_hook(panel: *mut u8, panel_type: i32) {
    let p1_panel = CSS_PANEL_PTRS[0].load(Ordering::Relaxed);
    if p1_panel != 0 && panel as usize == p1_panel && panel_type != 0 {
        // Block: P1 must stay human (type 0).
        return;
    }
    call_original!(panel, panel_type)
}

/// Hook the CSS setup function to expand training mode from 2 to 4 player slots.
/// When on the training CSS, patches the mode_params struct
/// so the CSS allocates UI for 4 players instead of the default 2.
#[skyline::hook(offset = OFFSET_CSS_SETUP)]
pub unsafe fn css_setup_hook(parent: *const u8, mode_params: *mut u8, data_buf: *const u8) {
    // Always clear the training scene pointer at CSS entry. The old scene is
    // destroyed; without this, the draw thread can race ahead and use stale
    // pane pointers from FLAG_PANE_CACHE before css_panel_layout_hook sets
    // the new address. css_panel_layout_hook will re-populate it for training.
    CSS_TRAINING_SCENE_PTR.store(0, Ordering::Relaxed);

    if !mode_params.is_null() {
        let mode = core::ptr::read_volatile(mode_params.add(0x0) as *const u32);
        if mode == 0xB {
            // Always allocate for 4 players so the toggle can expand mid-CSS.
            // The visible layout (mode + slot_count) is controlled separately
            // by css_panel_layout_hook and the toggle handler.
            core::ptr::write_volatile(mode_params.add(0xC) as *mut u32, 4);
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
static SAVED_CSS_COSTUME: [AtomicU32; 4] = [
    AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0), AtomicU32::new(0),
];
static SAVED_CSS_NPAD: [AtomicI32; 4] = [
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];
static SAVED_CSS_TEAM_COLOR: [AtomicI32; 4] = [
    AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1), AtomicI32::new(-1),
];
/// Saved secondary hash (panel+0x208) for CSS re-entry.
/// For Pokemon Trainer, this is the active sub-starter's hash (Squirtle/Ivysaur/
/// Charizard). Restored to main_bss+0x40 so the CSS shows the correct sub-starter.
static SAVED_CSS_SECONDARY_HASH: [AtomicU64; 4] = [
    AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0),
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

            if TEAM_MODE.load(Ordering::Relaxed) {
                // Team mode: expand to 4 slots with smash-like layout (0x6).
                // Use saved slot count from previous session if available.
                let saved_slots = SAVED_CSS_SLOT_COUNT.load(Ordering::Relaxed) as u32;
                let actual_slots = visible.max(2).max(saved_slots);
                core::ptr::write_volatile(mode_ptr, 0x6);
                call_original!(scene, actual_slots, arg2);
            } else {
                // Solo mode: vanilla training layout (mode=0xB, 2 slots).
                // Cap max_players to 2 so the join handler won't allow a
                // third controller even though 4 panel objects exist.
                core::ptr::write_volatile(scene.add(0x180) as *mut u32, 2);
                call_original!(scene, slot_count.min(2), arg2);
            }
            // Cache panel pointers after layout is done (panels now exist).
            cache_panel_ptrs(scene);
            return;
        }
    }
    call_original!(scene, slot_count, arg2)
}

/// Inline hook on the CSS restoration loop's `ldr w19, [x20]` instruction.
/// The game's CSS init resets the second BSS array to -1 for all entries,
/// which causes the restoration loop to skip human panel restoration.
/// This hook writes our saved controller slot (NpadId) to [X20] just before
/// the load executes, so the loop takes the human restoration path.
///
/// Extended for P3/P4 support:
///   - Caches BSS base addresses at iteration 0 (for write_saved_state_to_bss)
///   - Pre-writes second BSS NpadId for entries 2/3 at iteration 0 (in case
///     the loop reaches them after main BSS pre-population fixed the bound)
///   - Writes main BSS type/hash/team for ALL entry types (human + CPU)
///
/// Registers: X21 = entry index (0-based), X20 = second BSS array entry ptr,
///            X28 = main BSS entry ptr (current entry).
#[skyline::hook(offset = OFFSET_CSS_RESTORE_LOOP, inline)]
pub unsafe fn css_restore_loop_hook(ctx: &mut skyline::hooks::InlineCtx) {
    let entry_idx = ctx.registers[21].x() as usize;
    let second_bss_ptr = ctx.registers[20].x() as *mut i32;
    let main_bss_ptr = ctx.registers[28].x() as usize;

    if second_bss_ptr.is_null() || entry_idx >= 4 {
        return;
    }

    let saved_count = SAVED_CSS_SLOT_COUNT.load(Ordering::Relaxed);

    // --- Iteration 0: cache BSS base addresses + pre-write entries 2/3 ---
    if entry_idx == 0 {
        // Pre-write second BSS NpadId for entries 2/3. The -1 reset at
        // $main+0x23EBB68 has already fired by this point, so our writes
        // persist. The back-edge hook extends the loop to process entries
        // 2/3, which will find correct NpadId values here.
        for i in 2..saved_count.min(4) {
            let target_ptr = (second_bss_ptr as usize + i * BSS_CSS_SECOND_STRIDE) as *mut i32;
            let saved_is_cpu = SAVED_CSS_IS_CPU[i].load(Ordering::Relaxed);
            if saved_is_cpu == 0 {
                // Human: write saved NpadId so the loop takes the human path.
                let saved_npad = SAVED_CSS_NPAD[i].load(Ordering::Relaxed);
                if saved_npad >= 0 {
                    core::ptr::write_volatile(target_ptr, saved_npad);
                    debug_log(&format!(
                        "restore_loop[0]: pre-wrote second_bss[{}] npad={}",
                        i, saved_npad
                    ));
                }
            }
            // CPU entries: -1 is correct (already reset), no write needed.
        }

        debug_log(&format!(
            "restore_loop[0]: cached main_bss={:#x} second_bss={:#x} saved_count={}",
            main_bss_ptr, second_bss_ptr as usize, saved_count
        ));
    }

    if saved_count == 0 || entry_idx >= saved_count {
        return;
    }

    // --- Per-iteration: write BSS data for the current entry ---
    let saved_is_cpu = SAVED_CSS_IS_CPU[entry_idx].load(Ordering::Relaxed);

    // Second BSS: write NpadId for human entries.
    if saved_is_cpu == 0 {
        let saved_npad = SAVED_CSS_NPAD[entry_idx].load(Ordering::Relaxed);
        if saved_npad >= 0 {
            core::ptr::write_volatile(second_bss_ptr, saved_npad);
        }
    }

    // Main BSS: write type, hash, and team color for ALL entry types.
    // This ensures entries 2/3 have correct data even if write_saved_state_to_bss
    // couldn't run (first visit, cached address was 0).
    if main_bss_ptr != 0 {
        let main_bss = main_bss_ptr as *mut u8;
        // Type: 0=human, 1=CPU (overwrite disabled=3 or training's CPU=1 for P2).
        let target_type: i32 = if saved_is_cpu == 0 { 0 } else { 1 };
        core::ptr::write_volatile(main_bss.add(0x30) as *mut i32, target_type);

        // Primary hash at +0x38, secondary hash at +0x40.
        // +0x40 was previously written as a duplicate of +0x38, but for Pokemon
        // Trainer it must be the sub-starter's hash (Squirtle/Ivysaur/Charizard)
        // so the CSS panel shows the correct sub-starter on re-entry.
        let saved_hash = SAVED_CSS_HASH[entry_idx].load(Ordering::Relaxed);
        let saved_secondary = SAVED_CSS_SECONDARY_HASH[entry_idx].load(Ordering::Relaxed);
        let null_hash: u64 = 0xc1ffff0000000000;
        let random_hash: u64 = 0xc100000fd5f7fa78;
        if saved_hash != 0 && saved_hash != null_hash && saved_hash != random_hash {
            core::ptr::write_volatile(main_bss.add(0x38) as *mut u64, saved_hash);
            // Use secondary hash if saved, otherwise fall back to primary.
            let secondary = if saved_secondary != 0 { saved_secondary } else { saved_hash };
            core::ptr::write_volatile(main_bss.add(0x40) as *mut u64, secondary);
        }

        // Team color at +0x34.
        let saved_team = SAVED_CSS_TEAM_COLOR[entry_idx].load(Ordering::Relaxed);
        if saved_team >= 0 {
            core::ptr::write_volatile(main_bss.add(0x34) as *mut i32, saved_team);
        }
    }

    debug_log(&format!(
        "restore_loop[{}]: is_cpu={} npad={} hash={:#018x} team={}",
        entry_idx, saved_is_cpu,
        SAVED_CSS_NPAD[entry_idx].load(Ordering::Relaxed),
        SAVED_CSS_HASH[entry_idx].load(Ordering::Relaxed),
        SAVED_CSS_TEAM_COLOR[entry_idx].load(Ordering::Relaxed),
    ));

}

/// Inline hook on the restoration loop's back-edge `cmp x21, x23` instruction.
/// The loop exits when X21 == X23 (counter equals bound). X23 is normally 2
/// for training mode, limiting restoration to entries 0/1. This hook overrides
/// X23 to saved_count so entries 2/3 are also processed by the game's own
/// restoration logic (which correctly handles medals, panel images, etc.).
///
/// GDB-confirmed: X23 gets clobbered by `cset w23` in the loop body but is
/// restored from the stack before reaching this point. Our override at the
/// loop-body hook ($main+0x1843144) was therefore ineffective. This back-edge
/// hook fires right before the `cmp`, after the restore, so our value sticks.
#[skyline::hook(offset = OFFSET_CSS_RESTORE_LOOP_BOUND, inline)]
pub unsafe fn css_restore_loop_bound_hook(ctx: &mut skyline::hooks::InlineCtx) {
    let saved_count = SAVED_CSS_SLOT_COUNT.load(Ordering::Relaxed);
    if saved_count > 2 {
        ctx.registers[23].set_x(saved_count.min(4) as u64);
    }
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

/// Hook for FUN_710064f820 (lua_ai_orchestrator): skip the entire AI think
/// pipeline for human-controlled entries by temporarily NULLing their agent
/// pointers. The orchestrator naturally skips NULL entries.
///
/// Layout:
///   ai_mgr + 0x28:   entry count (i32)
///   ai_mgr + 0x4178: array of AI agent pointers (one per entry, 8 bytes each)
///
/// Entry 0 (P1) is left untouched — the post-loop section dereferences it
/// unconditionally for button-mapping and training-mode command processing.
#[skyline::hook(offset = OFFSET_LUA_AI_ORCHESTRATOR)]
pub unsafe fn lua_ai_orchestrator_hook(ai_mgr: *mut u8) {
    let entry_base = ai_mgr.add(0x4178) as *mut usize;
    let count = *(ai_mgr.add(0x28) as *const i32);
    let max = (count as usize).min(4);

    // Save and NULL human entries (skip index 0 — post-loop dereferences it)
    let mut saved: [(usize, usize); 3] = [(0, 0); 3];
    let mut n = 0usize;
    for i in 1..max {
        let ptr = core::ptr::read_volatile(entry_base.add(i));
        if ptr != 0 && is_human_entry(i as i32) {
            saved[n] = (i, ptr);
            n += 1;
            core::ptr::write_volatile(entry_base.add(i), 0usize);
        }
    }

    call_original!(ai_mgr);

    // Restore pointers so other systems can still read them
    for j in 0..n {
        let (idx, val) = saved[j];
        core::ptr::write_volatile(entry_base.add(idx), val);
    }
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
    // Only apply training-specific overrides when transitioning from the
    // training CSS. In other modes (Smash, etc.) just call through.
    if CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed) == 0 {
        call_original!(config, entry_index, byte_flag, bss_out);
        return;
    }

    // On the first clone_write call (entry 0), snapshot all panel state so we
    // can restore it when the user returns to CSS from training mode.
    if entry_index == 0 {
        save_css_state_for_reentry();
        invalidate_hit_teams();
        // Set the vanilla team battle flag early so the renderer's init
        // picks up team mode before the first render frame.
        sync_team_battle_flag();
        // Signal game_tick to write fi_data+0x2C before the first render pass.
        OUTLINE_INIT_PENDING.store(is_team_mode(), Ordering::Relaxed);
        // Reset npad→entry mapping. Entry 0 always uses npad 0.
        for i in 0..8 {
            NPAD_TO_ENTRY[i].store(-1, Ordering::Relaxed);
        }
        NPAD_TO_ENTRY[0].store(0, Ordering::Relaxed);
        // Reset human entry npad tracking.
        for slot in &HUMAN_ENTRY_NPAD {
            slot.store(-1, Ordering::Relaxed);
        }
        // Reset tag/profile tracking and cached mappings.
        for slot in &HUMAN_ENTRY_TAG {
            slot.store(0, Ordering::Relaxed);
        }
        for i in 0..4 {
            *CACHED_PROFILE_MAPPING[i].0.get() = None;
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
        cache_profile_mapping(1, entry1_tag);

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
            // Pokemon Trainer (db_index 38): the trainer itself has no fighter_kind
            // in the game's hash resolver — it returns -1, causing clone_write to
            // default to kind 0 (Mario). The native CSS resolves PT to a sub-starter
            // (Squirtle/Ivysaur/Charizard) before the config is built, but our
            // mod-added P3/P4 panels don't go through that resolution.
            // Fix: read the panel's secondary hash (panel+0x208) which stores the
            // active sub-starter. css_panel_set_chara_hash writes it there, and the
            // CSS cycling (A button) updates it. Falls back to Squirtle if unset.
            let write_hash = if db_idx == 38 {
                const SQUIRTLE_HASH: u64 = ui_hash(39, 0x1280f1c82e);
                const IVYSAUR_HASH: u64 = ui_hash(40, 0x14ef73f367);
                const CHARIZARD_HASH: u64 = ui_hash(41, 0x12915a4ff6);

                let sub_hash = read_css_panel_secondary_hash(entry_index);
                let sub_db = if sub_hash != 0 { db_index_from_hash(sub_hash) } else { -1 };
                let resolved = match sub_db {
                    39 => SQUIRTLE_HASH,
                    40 => IVYSAUR_HASH,
                    41 => CHARIZARD_HASH,
                    _ => SQUIRTLE_HASH, // default if secondary hash is unset/invalid
                };
                debug_log(&format!(
                    "clone_write: entry={} PT detected, sub_hash={:#018x} sub_db={} → {:#018x}",
                    entry_index, sub_hash, sub_db, resolved
                ));
                resolved
            } else {
                panel_hash
            };

            // Normal character (or PT→sub-starter substitution): override config hash.
            core::ptr::write_volatile(config.add(0x88) as *mut u64, write_hash);
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
            cache_profile_mapping(entry_index as usize, panel_tag);
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

        // Write team outline color to bss_out so fighter init picks it up.
        if is_team_mode() && !bss_out.is_null() {
            let team_color = TEAM_COLORS[entry_index as usize].load(Ordering::Relaxed);
            // bss_out+0x84: known team color field (u16), consumed by fighter init → fi_data+0x84
            core::ptr::write_volatile(bss_out.add(0x84) as *mut u16, team_color as u16);
        }
        return;
    }
    call_original!(config, entry_index, byte_flag, bss_out);

    // Write team outline color for entries 0/1 too.
    if is_team_mode() && (entry_index as usize) < 4 && !bss_out.is_null() {
        let team_color = TEAM_COLORS[entry_index as usize].load(Ordering::Relaxed);
        core::ptr::write_volatile(bss_out.add(0x84) as *mut u16, team_color as u16);
    }
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

/// Read the secondary ui_chara hash from the cached CSS panel for `entry_index`.
/// panel+0x208: u64, set by css_panel_set_chara_hash alongside the primary hash.
/// For Pokemon Trainer, this stores the active sub-starter's hash (Squirtle/
/// Ivysaur/Charizard), updated when the player cycles with A on the CSS.
/// Returns 0 on failure.
unsafe fn read_css_panel_secondary_hash(entry_index: u32) -> u64 {
    if entry_index as usize >= CSS_PANEL_PTRS.len() {
        return 0;
    }
    let panel = CSS_PANEL_PTRS[entry_index as usize].load(Ordering::Relaxed) as *const u8;
    if panel.is_null() {
        return 0;
    }
    let hash = core::ptr::read_volatile(panel.add(0x208) as *const u64);
    if (hash & 0xFF00000000000000) == 0xC100000000000000 && (hash & 0xFFFFFFFFFF) != 0 {
        hash
    } else {
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
        SAVED_CSS_COSTUME[i].store(
            core::ptr::read_volatile(panel.add(0x210) as *const u8) as u32,
            Ordering::Relaxed,
        );
        // Save NpadId (controller slot) from panel+0x390.
        // This is the authoritative source — panel+0x1FC and HUMAN_ENTRY_NPAD
        // both return 0 for all entries regardless of actual controller.
        SAVED_CSS_NPAD[i].store(
            core::ptr::read_volatile(panel.add(0x390) as *const i32),
            Ordering::Relaxed,
        );
        // Save team color from our TEAM_COLORS global (persists across frames).
        SAVED_CSS_TEAM_COLOR[i].store(
            TEAM_COLORS[i].load(Ordering::Relaxed) as i32,
            Ordering::Relaxed,
        );
        // Save secondary hash (panel+0x208) for PT sub-starter restoration.
        SAVED_CSS_SECONDARY_HASH[i].store(
            core::ptr::read_volatile(panel.add(0x208) as *const u64),
            Ordering::Relaxed,
        );
    }
    SAVED_CSS_SLOT_COUNT.store(count, Ordering::Relaxed);
    debug_log(&format!("save_css_state: {} slots saved", count));
}

/// Second BSS array stride (used by inline hook for pre-writing NpadId).
const BSS_CSS_SECOND_STRIDE: usize = 0x240;

/// One-shot restore of CSS panel states from saved data (LEGACY).
/// Kept for reference — the BSS-write approach in write_saved_state_to_bss
/// is preferred as it lets the game's own restoration loop handle everything.
///
/// For each entry with saved state:
///   - P1: always human, character preserved by vanilla — skip.
///   - P2: if saved as human, call set_panel_type(panel, 0).
///   - P3/P4: write character hash + costume to panel, then call
///     set_panel_type for the saved type (human=0 or CPU=1).
///
/// set_panel_type handles all internal bookkeeping: sub-objects, visual
/// refresh, tag/profile setup (human path), cursor creation, etc.
unsafe fn restore_css_panels_on_reentry() {
    use skyline::hooks::{getRegionAddress, Region};
    let text_base = getRegionAddress(Region::Text) as usize;

    let scene_addr = CSS_TRAINING_SCENE_PTR.load(Ordering::Relaxed);
    if scene_addr == 0 {
        return;
    }
    let scene = scene_addr as *mut u8;

    // Re-cache panel pointers from the (new) scene — panels are recreated
    // at new addresses on each CSS entry.
    cache_panel_ptrs(scene);

    let saved_count = SAVED_CSS_SLOT_COUNT.load(Ordering::Relaxed);

    // Read vector base pointers for make_panel_human args.
    let panel_vec_base = core::ptr::read_volatile(scene.add(0x250) as *const usize);
    let vec_a_base = core::ptr::read_volatile(scene.add(0x238) as *const usize);
    if panel_vec_base == 0 || vec_a_base == 0 {
        debug_log("css_restore: vector bases are null, aborting");
        return;
    }

    let make_panel_human: extern "C" fn(*mut u8, *mut u8, *mut u8, u32, u32) =
        core::mem::transmute(text_base + OFFSET_MAKE_PANEL_HUMAN);
    let set_panel_type: extern "C" fn(*mut u8, i32) =
        core::mem::transmute(text_base + OFFSET_SET_PANEL_TYPE);

    for i in 0..saved_count.min(4) {
        let panel = CSS_PANEL_PTRS[i].load(Ordering::Relaxed) as *mut u8;
        if panel.is_null() {
            continue;
        }

        let saved_is_cpu = SAVED_CSS_IS_CPU[i].load(Ordering::Relaxed);
        let saved_hash = SAVED_CSS_HASH[i].load(Ordering::Relaxed);
        let saved_costume = SAVED_CSS_COSTUME[i].load(Ordering::Relaxed) as u8;
        let current_type = core::ptr::read_volatile(panel.add(0x1F8) as *const i32);

        // P1 (entry 0): vanilla handles it correctly — always human, char preserved.
        if i == 0 {
            continue;
        }

        // Compute vector entry pointers for this index.
        let panel_vec_entry = (panel_vec_base + i * 0x10) as *mut u8;
        let vec_a_entry = (vec_a_base + i * 0x10) as *mut u8;

        // Ensure takeover-eligible byte is set for non-P1 panels so
        // controller join/state cycling works in team mode.
        core::ptr::write_volatile(panel.add(0x1C0) as *mut u8, 1);

        if saved_is_cpu == 0 {
            // Human entry: use make_panel_human for full join flow
            // (cursor, token, controller binding, set_panel_type internally).
            if current_type != 0 {
                // Write tag before — the human init path reads panel+0x394.
                let saved_tag = SAVED_CSS_TAG[i].load(Ordering::Relaxed);
                if saved_tag >= 0 {
                    core::ptr::write_volatile(panel.add(0x394) as *mut i32, saved_tag);
                }
                // Save current hash/costume — make_panel_human clears them
                // as part of its cursor cleanup sequence.
                let current_hash = core::ptr::read_volatile(panel.add(0x200) as *const u64);
                let current_costume = core::ptr::read_volatile(panel.add(0x210) as *const u8);
                debug_log(&format!(
                    "css_restore: entry={} → HUMAN via make_panel_human (was type={})",
                    i, current_type
                ));
                make_panel_human(scene, panel_vec_entry, vec_a_entry, 1, 0);
                // Restore character hash + costume after make_panel_human wiped them.
                let null_hash: u64 = 0xc1ffff0000000000;
                let restore_hash = if saved_hash != 0 && saved_hash != null_hash {
                    saved_hash
                } else {
                    current_hash
                };
                if restore_hash != 0 && restore_hash != null_hash {
                    core::ptr::write_volatile(panel.add(0x200) as *mut u64, restore_hash);
                    let restore_costume = if saved_hash != 0 && saved_hash != null_hash {
                        saved_costume
                    } else {
                        current_costume
                    };
                    core::ptr::write_volatile(panel.add(0x210) as *mut u8, restore_costume);
                }
            }
        } else {
            // CPU entry: write character hash + costume, then activate as CPU.
            let null_hash: u64 = 0xc100000fd5f7fa78;
            if saved_hash != 0 && saved_hash != null_hash {
                core::ptr::write_volatile(panel.add(0x200) as *mut u64, saved_hash);
                core::ptr::write_volatile(panel.add(0x210) as *mut u8, saved_costume);
            }
            if current_type != 1 {
                debug_log(&format!(
                    "css_restore: entry={} hash={:#018x} costume={} → CPU",
                    i, saved_hash, saved_costume
                ));
                set_panel_type(panel, 1);
            }
        }
    }

    debug_log(&format!("css_restore: done ({} entries processed)", saved_count.min(4)));
}

