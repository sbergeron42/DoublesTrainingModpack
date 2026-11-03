#![allow(clippy::mut_from_ref)]
use core::sync::atomic::{AtomicU8, AtomicUsize, Ordering};
#[cfg(feature = "layout_arc_from_file")]
use byte_unit::MEBIBYTE;
use sarc::SarcFile;
use skyline::nn::ui2d::*;
use smash::ui2d::SmashPane;
use crate::common::text_box::SmashTextBox;
use training_mod_consts::{OnOff, MENU};

use crate::common::menu::QUICK_MENU_ACTIVE;
use crate::common::offsets::{OFFSET_DRAW, OFFSET_LAYOUT_ANIMATE, OFFSET_LAYOUT_ARC_MALLOC};
use crate::common::{is_ready_go, is_training_mode, is_training_mode_raw};
#[cfg(feature = "layout_arc_from_file")]
use crate::consts::LAYOUT_ARC_PATH;
use crate::training::frame_counter;
use training_mod_sync::*;

mod damage;
mod display;
pub mod menu;
pub mod notifications;
pub mod pane_cache;

pub unsafe fn set_icon_text(pane: &mut TextBox, icons: &[u16]) {
    pane.set_text_string("");

    let it = pane.text_buf as *mut u16;
    pane.text_len = icons.len() as u16;
    for (idx, icon) in icons.iter().enumerate() {
        *(it.add(idx)) = *icon;
    }

    // Add nullptr at end to be sure
    *(it.add(icons.len())) = 0x0;
}

pub fn fade_out(pane: &mut Pane, current_frame: u32, total_frames: u32) {
    if current_frame >= total_frames {
        pane.alpha = 255;
        pane.global_alpha = 255;
    } else if current_frame > 0 {
        // Logarithmic fade out
        let alpha = ((255.0 / (total_frames as f32 + 1.0).log10())
            * (current_frame as f32 + 1.0).log10()) as u8;
        pane.alpha = alpha;
        pane.global_alpha = alpha;

        // Linear fade out
        // let alpha = ((current_frame as f32 / 100.0) * 255.0) as u8;
        // pane.alpha = alpha;
        // pane.global_alpha = alpha;
    } else {
        pane.alpha = 0;
        pane.global_alpha = 0;
    }
}

#[skyline::hook(offset = *OFFSET_DRAW)]
pub unsafe fn handle_draw(layout: *mut Layout, draw_info: u64, cmd_buffer: u64) {
    // Borrowed, not copied: this runs for every layout every frame.
    let layout_name = core::ffi::CStr::from_ptr((*layout).layout_name as *const _)
        .to_str()
        .unwrap_or("");
    let root_pane = &mut *(*layout).root_pane;
    pane_cache::set_active(is_training_mode() && is_ready_go());

    // Set HUD to invisible if HUD is toggled off (team mode only)
    if crate::training::doubles::is_team_mode()
        && is_training_mode()
        && is_ready_go()
        && [
            "info_playercursor",
            "info_playercursor_item",
            "info_melee",
            "info_radar_a",
            "info_radar_b",
        ]
        .contains(&layout_name)
    {
        // InfluencedAlpha means "Should my children panes' alpha be influenced by mine, as the parent?"
        root_pane.flags |= 1 << PaneFlag::InfluencedAlpha as u8;
        root_pane.set_visible(read_with(&MENU, |m| m.hud) == OnOff::ON && !read(&QUICK_MENU_ACTIVE));
    }

    damage::draw(root_pane, layout_name);
    if crate::training::doubles::is_team_mode()
        && is_training_mode()
        && is_ready_go()
    {
        if layout_name == "info_melee" {
            crate::training::doubles::melee_portrait_team_colors(root_pane);
        } else if layout_name == "info_playercursor" {
            crate::training::doubles::playercursor_team_colors(root_pane);
        } else if layout_name == "info_loupe" {
            crate::training::doubles::loupe_team_colors(root_pane);
        } else if layout_name == "info_radar_a" {
            crate::training::doubles::radar_marker_team_colors(root_pane);
        }
    }
    crate::training::doubles::css_btn_rule_draw(root_pane, layout_name);
    crate::training::doubles::css_layout_name_logger(layout_name);
    // One-shot: dump the separate `chara_select` layout pane tree.
    crate::training::doubles::css_dump_chara_select_layout(root_pane, layout_name);
    // One-shot: dump loupe/radar pane trees for off-screen indicator team color research.
    #[cfg(feature = "doubles_ui_diagnostics")]
    crate::training::doubles::loupe_radar_diagnostic_dump(root_pane, layout_name);

    if layout_name == "info_training" {
        frame_counter::tick_real();
        crate::training::doubles::cpu_behavior_menu_draw(root_pane, !training_menu_closed(root_pane));
        if crate::training::doubles::is_team_mode() {
            display::draw(root_pane);
            menu::draw(root_pane);
        } else {
            // Solo mode: actively hide modpack UI panes so they don't show
            // stale/glitched content from a previous team mode session.
            if let Some(menu_pane) = pane_cache::find(root_pane, "TrModMenu") {
                menu_pane.set_visible(false);
            }
            if let Some(display_pane) = pane_cache::find(root_pane, "TrModDisplay") {
                display_pane.set_visible(false);
            }
        }

        // Hide the input log container — the input_log feature was removed
        // but the pane still exists in layout.arc. Must be set every frame
        // because the layout engine resets visibility during draw.
        if let Some(log_pane) = pane_cache::find(root_pane, "TrModInputLog") {
            log_pane.set_visible(false);
        }
    }

    original!()(layout, draw_info, cmd_buffer);
}

/// The game's own Training menu (the one + opens) has 116 option buttons, part layouts named
/// `info_training_btn0_*`, and the game animates them every frame whether the menu is open or closed: about
/// 1.1 ms per frame of the UI work the main thread waits on (SSBU Reversing Agent docs/FRAME_PROFILER.md, "The
/// game's UI work"). While the menu is fully closed they aren't animated; they carry on from where they were
/// when it opens.
///
/// info_training animates its own transforms, which include the menu's open and close animation, before its
/// part layouts, so the test sees this frame's menu position.
static INFO_TRAINING_ROOT: AtomicUsize = AtomicUsize::new(0);
/// For the info_training animate in progress: 0 not tested yet, 1 menu fully closed, 2 open or moving.
static TRAINING_MENU_STATE: AtomicU8 = AtomicU8::new(0);

/// The help footer pane of the info_training layout whose root is HELP_ROOT, found once per layout instance.
static HELP_ROOT: AtomicUsize = AtomicUsize::new(0);
static HELP_PANE: AtomicUsize = AtomicUsize::new(0);
const HELP_NAME: &[u8] = b"L_staying_help\0";

unsafe fn help_pane(root: *const Pane) -> *const Pane {
    let pane = HELP_PANE.load(Ordering::Relaxed) as *const Pane;
    if HELP_ROOT.load(Ordering::Relaxed) == root as usize
        && !pane.is_null()
        && (*pane).name[..HELP_NAME.len()].iter().zip(HELP_NAME).all(|(a, b)| *a as u8 == *b)
    {
        return pane;
    }
    let pane = (*root).find_pane_by_name_recursive("L_staying_help").map_or(core::ptr::null(), |p| p as *const Pane);
    HELP_ROOT.store(root as usize, Ordering::Relaxed);
    HELP_PANE.store(pane as usize, Ordering::Relaxed);
    pane
}

/// The help footer sits at y = -80 while the menu is closed and moves up as it opens (info_training_in_menu).
unsafe fn training_menu_closed(root: *const Pane) -> bool {
    match TRAINING_MENU_STATE.load(Ordering::Relaxed) {
        1 => true,
        2 => false,
        _ => {
            let help = help_pane(root);
            let closed = !help.is_null() && (*help).pos_y == -80.0;
            TRAINING_MENU_STATE.store(if closed { 1 } else { 2 }, Ordering::Relaxed);
            closed
        }
    }
}

#[skyline::hook(offset = *OFFSET_LAYOUT_ANIMATE)]
pub unsafe fn handle_layout_animate(layout: *mut Layout, step: f32) {
    if !is_training_mode_raw() {
        return original!()(layout, step);
    }
    let name = core::ffi::CStr::from_ptr((*layout).layout_name as *const _).to_bytes();
    if name == b"info_training" {
        // info_training also has a part layout of the same name: only the outermost one sets and clears this.
        let outer = INFO_TRAINING_ROOT
            .compare_exchange(0, (*layout).root_pane as usize, Ordering::Relaxed, Ordering::Relaxed)
            .is_ok();
        if outer {
            TRAINING_MENU_STATE.store(0, Ordering::Relaxed);
        }
        original!()(layout, step);
        if outer {
            INFO_TRAINING_ROOT.store(0, Ordering::Relaxed);
        }
        return;
    }
    if name.starts_with(b"info_training_btn0_") {
        let root = INFO_TRAINING_ROOT.load(Ordering::Relaxed) as *const Pane;
        if !root.is_null() && training_menu_closed(root) {
            return;
        }
    }
    original!()(layout, step)
}

// Allocate a static amount of memory that Smash isn't allowed to deallocate,
// in order for us to be able to swap the 'layout.arc' with the current
// version of the file in between loads of training mode.
#[cfg(feature = "layout_arc_from_file")]
const LAYOUT_ARC_SIZE: usize = (5 * MEBIBYTE) as usize;
#[cfg(feature = "layout_arc_from_file")]
static mut LAYOUT_ARC: &mut [u8; LAYOUT_ARC_SIZE] = &mut [0u8; LAYOUT_ARC_SIZE];

/// We are editing the info_training/layout.arc and replacing the original file with our
/// modified version from `LAYOUT_ARC_PATH`
///
/// When we edit the layout we are doing two things.
///
/// 1. Creating a new BFLYT inside the layout.arc for whatever component we are making. For example,
/// the slider menu button.
///
/// 2. Adding a Parts pane to the info_training.bflyt with the "Part Name" matching the name of
/// our new BFLYT without the file extension (mimicking how it's done with native Parts panes)
///
/// # Warnings
/// When creating a BFLYT from an existing one we need to edit the names of the panes so that
/// the default animations no longer modify them.  Otherwise the game will override properties,
/// i.e. material colours, and we won't be able to control them properly.
///
/// Once we have the file edited and saved to the correct location we can access the pane
/// from the layout as we normally would in our Draw function
/// `(root_pane.find_pane_by_name_recursive("name_of_parts_pane")`.
///
/// # Usage
/// Now say I want to edit background colour of the button's label.
/// I would have to grab the parts pane and find the pane I want to modify on it, then I'd be able
/// to make the modifications as I normally would.
///
/// ```rust
/// let slider_button = root_pane.find_pane_by_name_recursive("name_of_parts_pane");
/// let label_bg = slider_button.find_pane_by_name_recursive("name_of_picture_pane");
///
/// let label_material = &mut *label_bg.as_picture().material;
///
/// label_material.set_white_res_color(LABEL_WHITE_SELECTED_COLOR);
/// label_material.set_black_res_color(LABEL_BLACK_SELECTED_COLOR);
/// ```
#[skyline::hook(offset = *OFFSET_LAYOUT_ARC_MALLOC, inline)]
unsafe fn handle_layout_arc_malloc(ctx: &mut skyline::hooks::InlineCtx) {
    if !is_training_mode() {
        return;
    }

    let decompressed_file = ctx.registers[21].x() as *const u8;
    let decompressed_size = ctx.registers[1].x() as usize;

    let layout_arc = SarcFile::read(std::slice::from_raw_parts(
        decompressed_file,
        decompressed_size,
    ))
    .expect("Could not read layout.arc SarcFile!");
    let training_layout = layout_arc.files.iter().find(|f| {
        f.name.is_some() && f.name.as_ref().unwrap() == &String::from("blyt/info_training.bflyt")
    });
    if training_layout.is_none() {
        return;
    }

    let inject_arc;
    let inject_arc_size: u64;

    #[cfg(feature = "layout_arc_from_file")]
    #[allow(static_mut_refs)]
    {
        let inject_arc_from_file =
            std::fs::read(LAYOUT_ARC_PATH).expect("Could not read layout.arc from file!");
        inject_arc_size = inject_arc_from_file.len() as u64;

        // Copy read file to global
        inject_arc_from_file
            .iter()
            .enumerate()
            .for_each(|(idx, byte)| LAYOUT_ARC[idx] = *byte);
        inject_arc = LAYOUT_ARC.as_ptr();
    }

    #[cfg(not(feature = "layout_arc_from_file"))]
    {
        include_flate::flate!(static INJECT_ARC_FROM_FILE: [u8] from "src/static/layout.arc");

        inject_arc = INJECT_ARC_FROM_FILE.as_ptr();
        inject_arc_size = INJECT_ARC_FROM_FILE.len() as u64;
    }

    // Decompressed file pointer
    ctx.registers[21].set_x(inject_arc as u64);

    // Decompressed size is in each of these registers
    ctx.registers[1].set_x(inject_arc_size);
    ctx.registers[23].set_x(inject_arc_size);
    ctx.registers[24].set_x(inject_arc_size);
}

pub trait PaneExt {
    unsafe fn find_pane_by_name_recursive_expect(&self, name: &str) -> &mut Pane;
}

impl PaneExt for Pane {
    unsafe fn find_pane_by_name_recursive_expect(&self, name: &str) -> &mut Pane {
        self.find_pane_by_name_recursive(name)
            .unwrap_or_else(|| panic!("Could not find pane {}", name))
    }
}

pub fn init() {
    skyline::install_hooks!(handle_draw, handle_layout_animate, handle_layout_arc_malloc);
}
