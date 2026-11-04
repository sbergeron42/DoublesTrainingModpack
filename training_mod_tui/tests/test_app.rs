use training_mod_tui::*;

fn make_submenu<'a>(id: &'a str) -> SubMenu<'a> {
    SubMenu {
        title: id,
        id,
        help_text: "Help",
        submenu_type: SubMenuType::ToggleMultiple,
        toggles: StatefulTable::with_items(
            1,
            2,
            vec![
                Toggle {
                    title: "Off",
                    value: 0,
                    max: 1,
                },
                Toggle {
                    title: "On",
                    value: 1,
                    max: 1,
                },
            ],
        ),
        slider: None,
    }
}

fn make_tab<'a>(title: &'a str) -> Tab<'a> {
    // Two rows of two buttons
    Tab {
        title,
        id: title,
        submenus: StatefulTable::with_items(
            2,
            2,
            vec![
                make_submenu("a"),
                make_submenu("b"),
                make_submenu("c"),
                make_submenu("d"),
            ],
        ),
    }
}

/// Profile tabs Mash, Override, Defensive and root tabs Misc, Doubles; P3 and P4 are CPUs on profiles 2 and 6.
fn make_app<'a>() -> App<'a> {
    let mut app = App::new();
    for t in ["Mash", "Override", "Defensive"] {
        app.profile_tabs.push(make_tab(t));
    }
    for t in ["Misc", "Doubles"] {
        app.tabs.push(make_tab(t));
    }
    app.cpu_is_active = [false, true, true];
    app.cpu_profile_assign = [0, 1, 5];
    app.open();
    app
}

fn tab_title(app: &App) -> &'static str {
    match app.tab_titles()[1] {
        "Mash" => "Mash",
        "Override" => "Override",
        "Defensive" => "Defensive",
        "Misc" => "Misc",
        "Doubles" => "Doubles",
        _ => "?",
    }
}

#[test]
fn opens_on_first_active_cpu() {
    let app = make_app();
    assert_eq!(app.page, AppPage::SUBMENU);
    assert_eq!(app.edit_cpu, 1);
    assert_eq!(app.editing_profile_idx, Some(1));
    assert_eq!(app.tab_titles(), ["Doubles", "Mash", "Override"]);
}

#[test]
fn opens_on_root_tabs_without_cpus() {
    let mut app = make_app();
    app.cpu_is_active = [false; MAX_CPU_SLOTS];
    app.open();
    assert_eq!(app.editing_profile_idx, None);
    assert_eq!(app.tab_titles(), ["Doubles", "Misc", "Doubles"]);
    // The tab cycle skips the profile tabs
    app.on_zr();
    assert_eq!(tab_title(&app), "Doubles");
    app.on_zr();
    assert_eq!(tab_title(&app), "Misc");
    app.on_zl();
    assert_eq!(tab_title(&app), "Doubles");
}

#[test]
fn one_tab_cycle_through_profile_and_root_tabs() {
    let mut app = make_app();
    let mut seen = vec![tab_title(&app)];
    for _ in 0..5 {
        app.on_zr();
        seen.push(tab_title(&app));
    }
    assert_eq!(seen, ["Mash", "Override", "Defensive", "Misc", "Doubles", "Mash"]);
    assert_eq!(app.editing_profile_idx, Some(1));
    app.on_zl();
    assert_eq!(tab_title(&app), "Doubles");
    assert_eq!(app.editing_profile_idx, None);
    app.on_zl();
    app.on_zl();
    assert_eq!(tab_title(&app), "Defensive");
    assert_eq!(app.editing_profile_idx, Some(1));
}

#[test]
fn switch_cpu_keeps_tab_button_and_page() {
    let mut app = make_app();
    app.on_zr(); // Override
    app.on_right(); // button b
    app.on_a(); // open it
    assert_eq!(app.page, AppPage::TOGGLE);
    assert!(app.switch_cpu(1));
    assert_eq!(app.edit_cpu, 2);
    assert_eq!(app.editing_profile_idx, Some(5));
    assert_eq!(app.page, AppPage::TOGGLE);
    assert_eq!(tab_title(&app), "Override");
    assert_eq!(app.selected_submenu().id, "b");
    // Wraps over the inactive slot
    assert!(app.switch_cpu(1));
    assert_eq!(app.edit_cpu, 1);
    app.on_l();
    assert_eq!(app.edit_cpu, 2);
}

#[test]
fn switch_cpu_does_nothing_on_root_tabs_or_in_confirmation() {
    let mut app = make_app();
    app.on_r();
    assert_eq!(app.page, AppPage::CONFIRMATION);
    assert!(!app.switch_cpu(1));
    app.on_b();
    assert_eq!(app.page, AppPage::SUBMENU);
    app.on_zl(); // Doubles
    assert_eq!(app.editing_profile_idx, None);
    assert!(!app.switch_cpu(1));
    assert_eq!(app.edit_cpu, 1);
}

#[test]
fn header_changes_the_edited_cpus_profile() {
    let mut app = make_app();
    // Up from the top row selects the header
    app.on_up();
    assert_eq!(app.page, AppPage::PROFILE);
    app.on_right();
    assert_eq!(app.cpu_profile_assign, [0, 2, 5]);
    assert_eq!(app.editing_profile_idx, Some(2));
    app.on_left();
    app.on_left();
    app.on_left();
    assert_eq!(app.cpu_profile_assign, [0, 7, 5]);
    // Reset doesn't apply to the header
    app.on_r();
    assert_eq!(app.page, AppPage::PROFILE);
    app.on_down();
    assert_eq!(app.page, AppPage::SUBMENU);
}

#[test]
fn profile_sharing() {
    let mut app = make_app();
    assert!(app.profile_shared_with().is_empty());
    app.cpu_profile_assign = [5, 5, 5];
    // Slot 0 (P2) isn't a CPU, so only P4 shares P3's profile
    assert_eq!(app.profile_shared_with(), vec![2]);
}

#[test]
fn up_on_root_tab_wraps_rows() {
    let mut app = make_app();
    app.on_zl(); // Doubles
    app.on_up();
    assert_eq!(app.page, AppPage::SUBMENU);
    assert_eq!(app.selected_tab().submenus.state.selected_row(), Some(1));
}

#[test]
fn b_closes_from_buttons_and_returns_from_header() {
    let mut app = make_app();
    app.on_up();
    app.on_b();
    assert_eq!(app.page, AppPage::SUBMENU);
    app.on_b();
    assert_eq!(app.page, AppPage::CLOSE);
}

fn make_tall_submenu<'a>(id: &'a str) -> SubMenu<'a> {
    // One column of two options, so Up from the second row stays in the setting
    let mut s = make_submenu(id);
    s.toggles = StatefulTable::with_items(
        2,
        1,
        vec![
            Toggle {
                title: "Low",
                value: 0,
                max: 2,
            },
            Toggle {
                title: "High",
                value: 1,
                max: 2,
            },
        ],
    );
    s
}

#[test]
fn header_from_an_open_setting_returns_to_it() {
    let mut app = make_app();
    app.profile_tabs.items[0].submenus.items[0][0] = Some(make_tall_submenu("a"));
    app.on_a(); // open setting a
    assert_eq!(app.page, AppPage::TOGGLE);
    app.on_down(); // second option
    app.on_up(); // back to the first option, still in the setting
    assert_eq!(app.page, AppPage::TOGGLE);
    app.on_up(); // the header
    assert_eq!(app.page, AppPage::PROFILE);
    app.on_right();
    assert_eq!(app.cpu_profile_assign, [0, 2, 5]);
    // ZL/ZR don't change tabs from here, as they don't from the open setting
    app.on_zr();
    app.on_zl();
    assert_eq!(app.page, AppPage::PROFILE);
    assert_eq!(tab_title(&app), "Mash");
    // L/R still switch CPU
    app.on_l();
    assert_eq!(app.edit_cpu, 2);
    app.on_down();
    assert_eq!(app.page, AppPage::TOGGLE);
    assert_eq!(app.selected_submenu().id, "a");
    assert_eq!(app.selected_submenu().toggles.state.selected_row(), Some(0));
    // A and B from the header go back to the setting too
    app.on_up();
    app.on_b();
    assert_eq!(app.page, AppPage::TOGGLE);
    app.on_up();
    app.on_a();
    assert_eq!(app.page, AppPage::TOGGLE);
    // Leaving the setting, the header goes back to the buttons and ZL/ZR change tabs again
    app.on_b();
    assert_eq!(app.page, AppPage::SUBMENU);
    app.on_up();
    app.on_zr();
    assert_eq!(app.page, AppPage::SUBMENU);
    assert_eq!(tab_title(&app), "Override");
}

#[test]
fn header_from_a_slider_only_without_a_selected_handle() {
    let mut app = make_app();
    let mut s = make_submenu("a");
    s.submenu_type = SubMenuType::Slider;
    s.slider = Some(StatefulSlider::new());
    app.profile_tabs.items[0].submenus.items[0][0] = Some(s);
    app.on_a();
    assert_eq!(app.page, AppPage::SLIDER);
    app.on_a(); // select a handle: Up moves it
    app.on_up();
    assert_eq!(app.page, AppPage::SLIDER);
    app.on_b(); // deselect the handle
    app.on_up();
    assert_eq!(app.page, AppPage::PROFILE);
    app.on_down();
    assert_eq!(app.page, AppPage::SLIDER);
}

#[test]
fn up_in_a_root_setting_stays_in_it() {
    let mut app = make_app();
    app.on_zl(); // Doubles, a root tab
    app.on_a();
    assert_eq!(app.page, AppPage::TOGGLE);
    app.on_up();
    assert_eq!(app.page, AppPage::TOGGLE);
}
