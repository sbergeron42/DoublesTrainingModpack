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
/// - Teammate slot: auto-enables "CPU Behavior = Control" for a configured
///   teammate slot so P2's controller routes to that CPU fighter.
use core::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, AtomicUsize, Ordering};

use smash::app::{lua_bind::*, BattleObjectModuleAccessor};
use smash::lib::lua_const::*;
use training_mod_sync::*;

use crate::common::{FIGHTER_MANAGER_ADDR, MENU, TRAINING_MENU_ADDR};

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
///   - config[0x7C] (npad/controller)— entry_index for human, -1 for CPU
///
/// Panel access: scene+0x250 is a std::vector of (vtable, panel_ptr) pairs (0x10 each).
#[skyline::hook(offset = OFFSET_CLONE_WRITE)]
pub unsafe fn clone_write_hook(config: *mut u8, entry_index: u32, byte_flag: u8, bss_out: *mut u8) {
    // On the first clone_write call (entry 0), snapshot all panel state so we
    // can restore it when the user returns to CSS from training mode.
    if entry_index == 0 {
        save_css_state_for_reentry();
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
        if is_cpu {
            core::ptr::write_volatile(config.add(0x78) as *mut i32, 1); // CPU
            core::ptr::write_volatile(config.add(0x7C) as *mut i32, -1); // no controller
        } else {
            core::ptr::write_volatile(config.add(0x78) as *mut i32, 0); // human
            core::ptr::write_volatile(config.add(0x7C) as *mut i32, entry_index as i32); // controller = entry
        }

        // Override tag/profile index so clone_write loads the correct button
        // mappings from the player's tag instead of CPU1's.
        // config[0x214] = tag index (used to look up profile at base + idx * 0xf7d8).
        let saved_tag = core::ptr::read_volatile(config.add(0x214) as *const u32);
        let panel_tag = read_css_panel_tag(entry_index);
        if !is_cpu {
            core::ptr::write_volatile(config.add(0x214) as *mut u32, panel_tag);
        }

        debug_log(&format!(
            "clone_write: entry={} is_cpu={} hash={:#018x} tag={}",
            entry_index, is_cpu,
            if panel_hash != 0 { panel_hash } else { saved_hash },
            panel_tag
        ));

        call_original!(config, entry_index, byte_flag, bss_out);

        // Restore so the next call sees the original CPU1 values.
        core::ptr::write_volatile(config.add(0x88) as *mut u64, saved_hash);
        core::ptr::write_volatile(config.add(0x78) as *mut i32, saved_type);
        core::ptr::write_volatile(config.add(0x7C) as *mut i32, saved_npad);
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


