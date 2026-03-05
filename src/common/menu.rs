use std::collections::HashMap;
use std::fs;

use skyline::nn::hid::GetNpadStyleSet;

use crate::common::button_config;
use crate::common::{ButtonConfig, DEFAULTS_MENU, MENU};
use crate::events::{Event, EVENT_QUEUE};
use crate::input::{ButtonBitfield, ControllerStyle, MappedInputs, SomeControllerStruct};
use crate::logging::*;
use crate::training::frame_counter;

use training_mod_consts::{
    create_app, profile_from_menu, CpuProfile, InputControl, MenuJsonStruct,
    DEFAULT_PROFILES, MAX_PROFILES, MENU_OPTIONS_PATH, PROFILES,
};
use training_mod_sync::*;
use training_mod_tui::AppPage;

use DirectionButton::*;

pub const MENU_CLOSE_WAIT_FRAMES: u32 = 15;
pub static QUICK_MENU_ACTIVE: RwLock<bool> = RwLock::new(false);

pub unsafe fn menu_condition() -> bool {
    button_config::combo_passes(button_config::ButtonCombo::OpenMenu)
}

/// Staged deserialization from file to avoid stack overflow.
/// Parses via serde_json::Value (heap-allocated) so only one struct is
/// deserialized at a time, limiting peak stack usage.
/// Marked #[inline(never)] so its stack frame is freed before create_app() runs.
#[inline(never)]
fn deserialize_menu_file() -> bool {
    info!("Checking for previous menu in {MENU_OPTIONS_PATH}...");
    if fs::metadata(MENU_OPTIONS_PATH).is_err() {
        info!("No previous menu file found.");
        return false;
    }

    let json_str = match fs::read_to_string(MENU_OPTIONS_PATH) {
        Ok(s) => s,
        Err(_) => {
            warn!("Could not read {MENU_OPTIONS_PATH}");
            return false;
        }
    };

    let v: serde_json::Value = match serde_json::from_str(&json_str) {
        Ok(v) => v,
        Err(_) => {
            warn!("Previous menu found but is invalid. Deleting...");
            let _ = fs::remove_file(MENU_OPTIONS_PATH);
            return false;
        }
    };

    // Deserialize menu parts separately to limit stack depth
    let menu: training_mod_consts::TrainingModpackMenu =
        match serde_json::from_value(v.get("menu").cloned().unwrap_or_default()) {
            Ok(m) => m,
            Err(_) => {
                warn!("Could not parse menu field. Deleting config...");
                let _ = fs::remove_file(MENU_OPTIONS_PATH);
                return false;
            }
        };
    let defaults_menu: training_mod_consts::TrainingModpackMenu =
        serde_json::from_value(v.get("defaults_menu").cloned().unwrap_or_default())
            .unwrap_or(menu);

    assign(&MENU, menu);
    assign(&DEFAULTS_MENU, defaults_menu);

    // Deserialize profiles one at a time to minimize stack usage
    let mut profiles = [CpuProfile::default(); MAX_PROFILES];
    let mut has_profiles = false;
    if let Some(arr) = v.get("profiles").and_then(|v| v.as_array()) {
        for (i, val) in arr.iter().enumerate().take(MAX_PROFILES) {
            if let Ok(p) = serde_json::from_value::<CpuProfile>(val.clone()) {
                profiles[i] = p;
                has_profiles = true;
            }
        }
    }

    let mut default_profiles = [CpuProfile::default(); MAX_PROFILES];
    if let Some(arr) = v.get("default_profiles").and_then(|v| v.as_array()) {
        for (i, val) in arr.iter().enumerate().take(MAX_PROFILES) {
            if let Ok(p) = serde_json::from_value::<CpuProfile>(val.clone()) {
                default_profiles[i] = p;
            }
        }
    }

    if !has_profiles {
        // Migration: old format didn't have profiles.
        // Copy per-CPU fields from MENU into profile 0 so nothing is lost.
        info!("Migrating old menu format: copying per-CPU settings to Profile 0...");
        let migrated = profile_from_menu();
        profiles[0] = migrated;
        default_profiles[0] = migrated;
    }

    assign(&PROFILES, profiles);
    assign(&DEFAULT_PROFILES, default_profiles);
    info!("Previous menu found. Loading...");
    true
}

pub fn load_from_file() {
    // Phase 1: deserialize from file (stack freed after this returns)
    deserialize_menu_file();

    // Phase 2: initialize app (triggers create_app() via lazy init)
    info!("Setting initial menu selections...");
    let mut app = lock_write(&QUICK_MENU_APP);
    // Load root tab settings from MENU
    app.serialized_default_settings =
        serde_json::to_string(&read(&DEFAULTS_MENU)).expect("Could not serialize DEFAULTS_MENU");
    app.update_all_from_json(
        &serde_json::to_string(&read(&MENU)).expect("Could not serialize MENU"),
    );
    // Load profile default settings for reset functionality
    let default_profile = read(&DEFAULT_PROFILES)[0];
    app.serialized_default_profile_settings =
        serde_json::to_string(&default_profile).expect("Could not serialize default profile");
    // Load cpu_profile_assign from MENU
    app.cpu_profile_assign = read(&MENU).cpu_profile_assign;
}

pub fn set_menu_from_json(message: &str) {
    let response = serde_json::from_str::<MenuJsonStruct>(message);
    info!("Received menu message: {message}");
    if let Ok(message_json) = response {
        assign(&MENU, message_json.menu);
        assign(&DEFAULTS_MENU, message_json.defaults_menu);
        assign(&PROFILES, message_json.profiles);
        assign(&DEFAULT_PROFILES, message_json.default_profiles);
        std::thread::spawn(move || {
            fs::write(
                MENU_OPTIONS_PATH,
                serde_json::to_string_pretty(&message_json)
                    .expect("Could not serialize menu settings"),
            )
            .expect("Failed to write menu settings file");
        });
    } else {
        skyline::error::show_error(
            0x70,
            "Could not parse the menu response!\nPlease send a screenshot of the details page to the developers.\n\0",
            &format!("{message:#?}\0"),
        );
    };
}

/// Build the full MenuJsonStruct from current app state and PROFILES
fn build_menu_json(app: &training_mod_tui::App<'static>) -> String {
    let menu_json_str = app.current_settings_to_json();
    let mut menu: training_mod_consts::TrainingModpackMenu =
        serde_json::from_str(&menu_json_str).expect("Could not parse root tab settings");
    menu.cpu_profile_assign = app.cpu_profile_assign;

    let defaults_json_str = &app.serialized_default_settings;
    let defaults_menu: training_mod_consts::TrainingModpackMenu =
        serde_json::from_str(defaults_json_str).expect("Could not parse default settings");

    let profiles = read(&PROFILES);
    let default_profiles = read(&DEFAULT_PROFILES);

    let full = MenuJsonStruct {
        menu,
        defaults_menu,
        profiles,
        default_profiles,
    };
    serde_json::to_string(&full).expect("Could not serialize full menu JSON")
}

fn close_menu(app: &training_mod_tui::App<'static>) {
    frame_counter::start_counting(*MENU_CLOSE_FRAME_COUNTER);
    assign(&QUICK_MENU_ACTIVE, false);
    let menu_json = build_menu_json(app);
    set_menu_from_json(&menu_json);

    let mut event_queue_lock = lock_write(&EVENT_QUEUE);
    (*event_queue_lock).push(Event::menu_open(menu_json));
    drop(event_queue_lock);
}

pub fn spawn_menu() {
    assign(&QUICK_MENU_ACTIVE, true);
    let mut app = lock_write(&QUICK_MENU_APP);
    app.page = AppPage::CPU_SETUP;
    app.editing_profile_idx = None;
    app.cpu_profile_assign = read(&MENU).cpu_profile_assign;

    // Detect which entries are CPUs vs human/nonexistent
    unsafe {
        let num_entries = crate::common::entry_count();
        for i in 0..training_mod_tui::MAX_CPU_SLOTS {
            let entry_id = i as i32 + 1;
            // Entry exists AND is not human-controlled
            app.cpu_is_active[i] =
                entry_id < num_entries && !crate::training::doubles::is_human_entry(entry_id);
        }
    }
    app.snap_to_active_cpu();

    assign(&MENU_RECEIVED_INPUT, true);
}

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
enum DirectionButton {
    LLeft,
    RLeft,
    LDown,
    RDown,
    LRight,
    RRight,
    LUp,
    RUp,
}

pub static QUICK_MENU_APP: LazyLock<RwLock<training_mod_tui::App<'static>>> = LazyLock::new(|| {
    RwLock::new({
        info!("Initialized lazy_static: QUICK_MENU_APP");
        unsafe { create_app() }
    })
});
pub static P1_CONTROLLER_STYLE: LazyLock<RwLock<ControllerStyle>> =
    LazyLock::new(|| RwLock::new(ControllerStyle::default()));
static DIRECTION_HOLD_FRAMES: LazyLock<RwLock<HashMap<DirectionButton, u32>>> =
    LazyLock::new(|| {
        RwLock::new(HashMap::from([
            (LLeft, 0),
            (RLeft, 0),
            (LDown, 0),
            (RDown, 0),
            (LRight, 0),
            (RRight, 0),
            (LUp, 0),
            (RUp, 0),
        ]))
    });
pub static MENU_RECEIVED_INPUT: RwLock<bool> = RwLock::new(true);

pub static MENU_CLOSE_FRAME_COUNTER: LazyLock<usize> =
    LazyLock::new(|| frame_counter::register_counter(frame_counter::FrameCounterType::Real));

/// Load a profile's data into the app's profile tabs
fn load_profile_into_tabs(app: &mut training_mod_tui::App<'static>, profile_idx: usize) {
    let profiles = read(&PROFILES);
    let profile = if profile_idx < MAX_PROFILES {
        profiles[profile_idx]
    } else {
        CpuProfile::default()
    };
    let json = serde_json::to_string(&profile).expect("Could not serialize profile");
    app.update_profile_from_json(&json);
}

/// Save the app's profile tabs data back into PROFILES
fn save_profile_from_tabs(app: &training_mod_tui::App<'static>, profile_idx: usize) {
    if profile_idx >= MAX_PROFILES {
        return;
    }
    let json = app.profile_settings_to_json();
    if let Ok(profile) = serde_json::from_str::<CpuProfile>(&json) {
        let mut profiles = lock_write(&PROFILES);
        profiles[profile_idx] = profile;
    } else {
        warn!("Could not deserialize profile from tabs!");
    }
}

pub unsafe fn handle_final_input_mapping(
    player_idx: i32,
    controller_struct: &mut SomeControllerStruct,
    out: *mut MappedInputs,
) {
    if player_idx == 0 {
        let p1_controller = &mut *controller_struct.controller;
        assign(&P1_CONTROLLER_STYLE, p1_controller.style);
        let visual_frame_count = frame_counter::get_frame_count(*MENU_CLOSE_FRAME_COUNTER);
        if visual_frame_count > 0 && visual_frame_count < MENU_CLOSE_WAIT_FRAMES {
            *out = MappedInputs::empty();
            p1_controller.current_buttons = ButtonBitfield::default();
            p1_controller.previous_buttons = ButtonBitfield::default();
            p1_controller.just_down = ButtonBitfield::default();
            p1_controller.just_release = ButtonBitfield::default();
        } else if visual_frame_count >= MENU_CLOSE_WAIT_FRAMES {
            frame_counter::stop_counting(*MENU_CLOSE_FRAME_COUNTER);
            frame_counter::reset_frame_count(*MENU_CLOSE_FRAME_COUNTER);
        }

        if read(&QUICK_MENU_ACTIVE) {
            *out = MappedInputs::empty();

            let mut received_input = false;

            const DIRECTION_HOLD_REPEAT_FRAMES: u32 = 20;
            use DirectionButton::*;
            let mut direction_hold_frames = read_clone(&DIRECTION_HOLD_FRAMES);

            let mut potential_controller_ids = (0..8).collect::<Vec<u32>>();
            potential_controller_ids.push(0x20);
            if potential_controller_ids
                .iter()
                .all(|i| GetNpadStyleSet(i as *const _).flags == 0)
            {
                assign(&QUICK_MENU_ACTIVE, false);
                return;
            }

            let style = p1_controller.style;
            let button_presses = p1_controller.just_down;

            let button_current_held = p1_controller.current_buttons;
            direction_hold_frames
                .iter_mut()
                .for_each(|(direction, frames)| {
                    let still_held = match direction {
                        LLeft => button_current_held.l_left(),
                        RLeft => button_current_held.r_left(),
                        LDown => button_current_held.l_down(),
                        RDown => button_current_held.r_down(),
                        LRight => button_current_held.l_right(),
                        RRight => button_current_held.r_right(),
                        LUp => button_current_held.l_up(),
                        RUp => button_current_held.r_up(),
                    };
                    if still_held {
                        *frames += 1;
                    } else {
                        *frames = 0;
                    }
                });

            let mut app = lock_write(&QUICK_MENU_APP);
            button_config::button_mapping(ButtonConfig::A, style, button_presses).then(|| {
                let on_cpu_setup = app.page == AppPage::CPU_SETUP;
                app.on_a();
                // If we just entered profile editing, load profile data into tabs
                if let Some(idx) = app.editing_profile_idx {
                    if on_cpu_setup {
                        load_profile_into_tabs(&mut app, idx);
                    }
                }
                received_input = true;
            });
            button_config::button_mapping(ButtonConfig::B, style, button_presses).then(|| {
                let was_editing = app.editing_profile_idx;
                received_input = true;
                app.on_b();
                // If we just exited profile editing, save profile data
                if let Some(profile_idx) = was_editing {
                    if app.editing_profile_idx.is_none() {
                        save_profile_from_tabs(&app, profile_idx);
                    }
                }
                if app.page == AppPage::CLOSE {
                    // Write cpu_profile_assign back to MENU before saving
                    {
                        let mut menu = lock_write(&MENU);
                        menu.cpu_profile_assign = app.cpu_profile_assign;
                    }
                    close_menu(&app);
                }
            });
            button_config::button_mapping(ButtonConfig::X, style, button_presses).then(|| {
                app.on_x();
                received_input = true;
            });
            button_config::button_mapping(ButtonConfig::Y, style, button_presses).then(|| {
                app.on_y();
                received_input = true;
            });

            button_config::button_mapping(ButtonConfig::ZL, style, button_presses).then(|| {
                app.on_zl();
                received_input = true;
            });
            button_config::button_mapping(ButtonConfig::ZR, style, button_presses).then(|| {
                app.on_zr();
                received_input = true;
            });
            button_config::button_mapping(ButtonConfig::R, style, button_presses).then(|| {
                app.on_r();
                received_input = true;
            });

            let hold_condition = |direction_button| {
                direction_hold_frames[direction_button] > DIRECTION_HOLD_REPEAT_FRAMES
            };
            (button_presses.dpad_left()
                || button_presses.l_left()
                || button_presses.r_left()
                || [LLeft, RLeft].iter().any(hold_condition))
            .then(|| {
                received_input = true;
                app.on_left();
            });
            (button_presses.dpad_right()
                || button_presses.l_right()
                || button_presses.r_right()
                || [LRight, RRight].iter().any(hold_condition))
            .then(|| {
                received_input = true;
                app.on_right();
            });
            (button_presses.dpad_up()
                || button_presses.l_up()
                || button_presses.r_up()
                || [LUp, RUp].iter().any(hold_condition))
            .then(|| {
                received_input = true;
                app.on_up();
            });
            (button_presses.dpad_down()
                || button_presses.l_down()
                || button_presses.r_down()
                || [LDown, RDown].iter().any(hold_condition))
            .then(|| {
                received_input = true;
                app.on_down();
            });

            if received_input {
                direction_hold_frames.iter_mut().for_each(|(_, f)| *f = 0);
                assign(&MENU_RECEIVED_INPUT, true);
            }
        }
    }
}
