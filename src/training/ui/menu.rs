use std::collections::HashMap;

use skyline::nn::ui2d::*;
use smash::ui2d::SmashPane;
use crate::common::text_box::SmashTextBox;
use training_mod_tui::{
    App, AppPage, ConfirmationState, SliderState, NX_SUBMENU_COLUMNS, NX_SUBMENU_ROWS,
};

use crate::common::menu::{
    MENU_CLOSE_FRAME_COUNTER, MENU_CLOSE_WAIT_FRAMES, MENU_RECEIVED_INPUT, P1_CONTROLLER_STYLE,
    QUICK_MENU_ACTIVE, QUICK_MENU_APP,
};
use crate::input::*;
use crate::training::frame_counter;
use crate::training::ui::{pane_cache, PaneExt};
use training_mod_consts::TOGGLE_MAX;
use training_mod_sync::*;

use super::fade_out;
use super::set_icon_text;

const BG_LEFT_ON_WHITE_COLOR: ResColor = ResColor {
    r: 0,
    g: 28,
    b: 118,
    a: 255,
};

const BG_LEFT_ON_BLACK_COLOR: ResColor = ResColor {
    r: 0,
    g: 22,
    b: 112,
    a: 0,
};

const BG_LEFT_OFF_WHITE_COLOR: ResColor = ResColor {
    r: 8,
    g: 13,
    b: 17,
    a: 255,
};

const BG_LEFT_OFF_BLACK_COLOR: ResColor = ResColor {
    r: 5,
    g: 10,
    b: 14,
    a: 0,
};

const BG_LEFT_SELECTED_BLACK_COLOR: ResColor = ResColor {
    r: 240,
    g: 154,
    b: 7,
    a: 0,
};

const BG_LEFT_SELECTED_WHITE_COLOR: ResColor = ResColor {
    r: 255,
    g: 166,
    b: 7,
    a: 255,
};

pub static VANILLA_MENU_ACTIVE: RwLock<bool> = RwLock::new(false);

static GCC_BUTTON_MAPPING: LazyLock<HashMap<&'static str, u16>> = LazyLock::new(|| {
    HashMap::from([
        ("L", 0xE204),
        ("R", 0xE205),
        ("X", 0xE206),
        ("Y", 0xE207),
        ("Z", 0xE208),
    ])
});
static PROCON_BUTTON_MAPPING: LazyLock<HashMap<&'static str, u16>> = LazyLock::new(|| {
    HashMap::from([
        ("L", 0xE0E4),
        ("R", 0xE0E5),
        ("X", 0xE0E2),
        ("Y", 0xE0E3),
        ("ZL", 0xE0E6),
        ("ZR", 0xE0E7),
    ])
});

/// The title's text colors as the layout has them, read the first time the title is drawn.
static TITLE_COLORS: RwLock<Option<[[[u8; 4]; 2]; 2]>> = RwLock::new(None);

// The key help bar, in 1080p screen pixels, measured on 13.0.5 (English) with the parts at their 1.25 scale. A
// part's icon ends, and its text starts, KEY_HELP_ANCHOR right of the part's own x (screen x = 960 + part x).
// Icons grow left from there. Text longer than KEY_HELP_TEXT_MAX_W is squeezed to that width; shorter text, and
// its icon with it, sits half the difference further right.
const KEY_HELP_ANCHOR: f32 = 37.0;
const KEY_HELP_GLYPH_W: f32 = 48.0;
const KEY_HELP_SLASH_W: f32 = 26.0;
const KEY_HELP_PLUS_W: f32 = 36.0;
const KEY_HELP_UPPER_W: f32 = 26.0;
const KEY_HELP_LOWER_W: f32 = 18.5;
const KEY_HELP_SPACE_W: f32 = 9.0;
const KEY_HELP_TEXT_MAX_W: f32 = 150.0;
const KEY_HELP_GAP: f32 = 40.0;
const KEY_HELP_RIGHT_EDGE: f32 = 1900.0;

/// One key help right of the tabs: its part, icon glyphs, and its title with a shorter one for a full bar.
struct KeyHelp {
    name: &'static str,
    glyphs: Vec<u16>,
    title: &'static str,
    short: &'static str,
}

fn key_help_icon_w(glyphs: &[u16]) -> f32 {
    glyphs
        .iter()
        .map(|&g| match g {
            0x2F => KEY_HELP_SLASH_W,
            0x2B => KEY_HELP_PLUS_W,
            _ => KEY_HELP_GLYPH_W,
        })
        .sum()
}

fn key_help_text_w(text: &str) -> f32 {
    text.chars()
        .map(|c| match c {
            ' ' => KEY_HELP_SPACE_W,
            c if c.is_ascii_uppercase() => KEY_HELP_UPPER_W,
            _ => KEY_HELP_LOWER_W,
        })
        .sum::<f32>()
        .min(KEY_HELP_TEXT_MAX_W)
}

/// Set a key help part's icon glyphs (as UTF-16, the way set_icon_text does) and text.
unsafe fn set_key_help_text(part: &mut Pane, glyphs: &[u16], title: &str) {
    let icon = part.find_pane_by_name_recursive_expect("set_txt_icon");
    icon.set_visible(true);
    let icon_tb = icon.as_textbox();
    // A placeholder of the same length makes sure the text buffer holds every glyph
    icon_tb.set_text_string(&"-".repeat(glyphs.len()));
    if icon_tb.text_len as usize == glyphs.len() {
        set_icon_text(icon_tb, glyphs);
    } else {
        set_icon_text(icon_tb, &glyphs[glyphs.len() - 1..]);
    }
    part.find_pane_by_name_recursive_expect("set_txt_help")
        .as_textbox()
        .set_text_string(title);
}

/// Lay out `items` left to right from screen x `start`, hiding the parts in `hidden`. When the bar would run off
/// the screen, titles switch to their short form, in the order given by `shorten_first` (indices into `items`).
unsafe fn layout_key_helps(
    root_pane: &Pane,
    start: f32,
    items: &[KeyHelp],
    shorten_first: &[usize],
    hidden: &[&'static str],
) {
    for name in hidden {
        root_pane.find_pane_by_name_recursive_expect(name).set_visible(false);
    }
    let mut titles: Vec<&str> = items.iter().map(|k| k.title).collect();
    let end = |titles: &[&str]| {
        start
            + items
                .iter()
                .zip(titles)
                .map(|(k, t)| KEY_HELP_GAP + key_help_icon_w(&k.glyphs) + key_help_text_w(t))
                .sum::<f32>()
    };
    for &i in shorten_first {
        if end(&titles) <= KEY_HELP_RIGHT_EDGE {
            break;
        }
        titles[i] = items[i].short;
    }
    let mut x = start;
    for (k, title) in items.iter().zip(titles) {
        let (icon_w, text_w) = (key_help_icon_w(&k.glyphs), key_help_text_w(title));
        let left = x + KEY_HELP_GAP;
        let anchor = left + icon_w - (KEY_HELP_TEXT_MAX_W - text_w) / 2.0;
        let part = root_pane.find_pane_by_name_recursive_expect(k.name);
        part.set_visible(true);
        part.pos_x = anchor - KEY_HELP_ANCHOR - 960.0;
        part.flags |= 1 << PaneFlag::IsGlobalMatrixDirty as u8;
        set_key_help_text(part, &k.glyphs, title);
        x = left + icon_w + text_w;
    }
}

/// The menu title: "Modpack Menu" on the root tabs; on the profile tabs the CPU being edited and its profile,
/// with arrows and in yellow while the header is selected (Left/Right change the profile).
unsafe fn draw_title(app: &App, root_pane: &Pane) {
    let Some(title) = root_pane.find_pane_by_name_recursive("TrModTitle") else {
        return;
    };
    let focused = app.page == AppPage::PROFILE;
    let text = if app.editing_profile_idx.is_some() {
        let profile = if focused {
            format!("< Profile {} >", app.edit_profile() + 1)
        } else {
            format!("Profile {}", app.edit_profile() + 1)
        };
        format!("P{} \u{b7} {}", app.edit_cpu + 2, profile)
    } else {
        "Modpack Menu".to_string()
    };
    let names = ["set_txt_00", "set_txt_01"];
    let panes: Vec<&mut TextBox> = names
        .iter()
        .filter_map(|n| title.find_pane_by_name_recursive(n))
        .map(|p| p.as_textbox())
        .collect();
    if panes.len() == names.len() && read(&TITLE_COLORS).is_none() {
        assign(&TITLE_COLORS, Some([panes[0].text_colors, panes[1].text_colors]));
    }
    let colors = read(&TITLE_COLORS);
    for (i, pane) in panes.into_iter().enumerate() {
        pane.set_text_string(&text);
        if focused {
            pane.set_color(255, 255, 0, 255);
        } else if let Some(c) = colors {
            let [r, g, b, a] = c[i][0];
            pane.set_color(r, g, b, a);
            pane.text_colors = c[i];
        }
    }
}

/// The CPUs that share the edited CPU's profile, as "P4" or "P3 and P4".
fn shared_with_text(app: &App) -> String {
    let names: Vec<String> = app
        .profile_shared_with()
        .iter()
        .map(|i| format!("P{}", i + 2))
        .collect();
    names.join(" and ")
}

/// The header is selected: the buttons, or the setting it was selected from, show without a highlight and the
/// footer says what Left/Right do, and which other CPUs use the same profile.
unsafe fn render_profile_header_page(app: &mut App, root_pane: &Pane) {
    match app.profile_return_page {
        AppPage::TOGGLE => render_toggle_page(app, root_pane, false),
        AppPage::SLIDER => render_slider_page(app, root_pane, false),
        _ => render_submenu_page(app, root_pane, false),
    }
    let player = app.edit_cpu + 2;
    let shared = shared_with_text(app);
    let help = if shared.is_empty() {
        format!("Left/Right: choose the profile P{player} uses.")
    } else {
        format!(
            "Left/Right: choose the profile P{player} uses. Profile {} is also used by {shared}.",
            app.edit_profile() + 1
        )
    };
    root_pane
        .find_pane_by_name_recursive_expect("FooterTxt")
        .as_textbox()
        .set_text_string(&help);
}

unsafe fn render_submenu_page(app: &mut App, root_pane: &Pane, highlight: bool) {
    let root_tabs_clone = app.tabs.clone();
    let profile_tabs_clone = app.profile_tabs.clone();
    let tab = app.selected_tab();
    for row in 0..NX_SUBMENU_ROWS {
        let menu_button_row = root_pane
            .find_pane_by_name_recursive_expect(format!("TrModMenuButtonRow{row}").as_str());
        menu_button_row.set_visible(true);
        for col in 0..NX_SUBMENU_COLUMNS {
            if let Some(submenu) = tab.submenus.get(row, col) {
                // Find all the panes we need to modify
                let menu_button = menu_button_row
                    .find_pane_by_name_recursive_expect(format!("Button{col}").as_str());
                let title_text = menu_button
                    .find_pane_by_name_recursive_expect("TitleTxt")
                    .as_textbox();
                let title_bg = menu_button
                    .find_pane_by_name_recursive_expect("TitleBg")
                    .as_picture();
                let title_bg_material = &mut *title_bg.material;
                let is_selected = highlight
                    && row == tab.submenus.state.selected_row().unwrap()
                    && col == tab.submenus.state.selected_col().unwrap();

                // Set Pane Visibility
                title_text.set_text_string(submenu.title);

                // In the actual 'layout.arc' file, every icon image is stacked
                // into a single container pane, with each image directly on top of another.
                // Hide all icon images, and strategically mark the icon that
                // corresponds with a particular button to be visible.

                for t in root_tabs_clone.iter().chain(profile_tabs_clone.iter()) {
                    for s in t.submenus.iter() {
                        let pane = menu_button.find_pane_by_name_recursive(s.id);
                        if let Some(p) = pane {
                            p.set_visible(s.id == submenu.id);
                        }
                    }
                }

                // Hide icon panes for removed features still in layout.arc
                for removed_id in ["input_display", "input_display_status"] {
                    if let Some(p) = menu_button.find_pane_by_name_recursive(removed_id) {
                        p.set_visible(false);
                    }
                }

                menu_button
                    .find_pane_by_name_recursive_expect("check")
                    .set_visible(false);

                for value in 1..=TOGGLE_MAX {
                    if let Some(pane) =
                        menu_button.find_pane_by_name_recursive(format!("{}", value).as_str())
                    {
                        pane.set_visible(false);
                    } else {
                        break;
                    }
                }

                if is_selected {
                    // Help text
                    root_pane
                        .find_pane_by_name_recursive_expect("FooterTxt")
                        .as_textbox()
                        .set_text_string(submenu.help_text);

                    title_bg_material.set_white_res_color(BG_LEFT_ON_WHITE_COLOR);
                    title_bg_material.set_black_res_color(BG_LEFT_ON_BLACK_COLOR);
                    title_text.text_shadow_enable(true);
                    title_text.text_outline_enable(true);
                    title_text.set_color(255, 255, 255, 255);
                } else {
                    title_bg_material.set_white_res_color(BG_LEFT_OFF_WHITE_COLOR);
                    title_bg_material.set_black_res_color(BG_LEFT_OFF_BLACK_COLOR);
                    title_text.text_shadow_enable(false);
                    title_text.text_outline_enable(false);
                    title_text.set_color(178, 199, 211, 255);
                }
                menu_button.set_visible(true);
                menu_button
                    .find_pane_by_name_recursive_expect("Icon")
                    .set_visible(true);
            }
        }
    }
}

unsafe fn render_toggle_page(app: &mut App, root_pane: &Pane, highlight: bool) {
    let root_tabs_clone = app.tabs.clone();
    let profile_tabs_clone = app.profile_tabs.clone();
    let submenu = app.selected_submenu();
    if highlight {
        // The setting's help, also after the profile header or a reset prompt changed the footer
        root_pane
            .find_pane_by_name_recursive_expect("FooterTxt")
            .as_textbox()
            .set_text_string(submenu.help_text);
    }
    // If the options can only be toggled on or off, then use the check icon
    // instead of the number icons
    let use_check_icon = submenu.toggles.get(0, 0).unwrap().max == 1;
    for row in 0..NX_SUBMENU_ROWS {
        let menu_button_row = root_pane
            .find_pane_by_name_recursive_expect(format!("TrModMenuButtonRow{row}").as_str());
        menu_button_row.set_visible(true);
        for col in 0..NX_SUBMENU_COLUMNS {
            if let Some(toggle) = submenu.toggles.get(row, col) {
                let menu_button = menu_button_row
                    .find_pane_by_name_recursive_expect(format!("Button{col}").as_str());
                menu_button.set_visible(true);
                let title_text = menu_button
                    .find_pane_by_name_recursive_expect("TitleTxt")
                    .as_textbox();
                let title_bg = menu_button
                    .find_pane_by_name_recursive_expect("TitleBg")
                    .as_picture();
                let title_bg_material = &mut *title_bg.material;
                let is_selected = highlight
                    && row == submenu.toggles.state.selected_row().unwrap()
                    && col == submenu.toggles.state.selected_col().unwrap();

                if is_selected {
                    title_text.text_shadow_enable(true);
                    title_text.text_outline_enable(true);
                    title_text.set_color(255, 255, 255, 255);
                    title_bg_material.set_white_res_color(BG_LEFT_ON_WHITE_COLOR);
                    title_bg_material.set_black_res_color(BG_LEFT_ON_BLACK_COLOR);
                } else {
                    title_text.text_shadow_enable(false);
                    title_text.text_outline_enable(false);
                    title_text.set_color(178, 199, 211, 255);
                    title_bg_material.set_white_res_color(BG_LEFT_OFF_WHITE_COLOR);
                    title_bg_material.set_black_res_color(BG_LEFT_OFF_BLACK_COLOR);
                }

                // Hide all submenu icons, since we're not on the submenu page
                for t in root_tabs_clone.iter().chain(profile_tabs_clone.iter()) {
                    for s in t.submenus.iter() {
                        let pane = menu_button.find_pane_by_name_recursive(s.id);
                        if let Some(p) = pane {
                            p.set_visible(false);
                        }
                    }
                }

                title_text.set_text_string(toggle.title);

                if use_check_icon {
                    menu_button
                        .find_pane_by_name_recursive_expect("check")
                        .set_visible(true);

                    menu_button
                        .find_pane_by_name_recursive_expect("Icon")
                        .set_visible(toggle.value > 0);
                } else {
                    menu_button
                        .find_pane_by_name_recursive_expect("check")
                        .set_visible(false);
                    menu_button
                        .find_pane_by_name_recursive_expect("Icon")
                        .set_visible(toggle.value > 0);

                    // Note there's no pane for 0
                    for value in 1..=toggle.max {
                        let err_msg = format!("Could not find pane with name {}", value);
                        menu_button
                            .find_pane_by_name_recursive(format!("{}", value).as_str())
                            .expect(&err_msg)
                            .set_visible(value == toggle.value);
                    }
                }
            }
        }
    }
}

unsafe fn render_slider_page(app: &mut App, root_pane: &Pane, highlight: bool) {
    let submenu = app.selected_submenu();
    let mut slider = submenu.slider.unwrap();
    if highlight {
        // The setting's help, also after the profile header or a reset prompt changed the footer
        root_pane
            .find_pane_by_name_recursive_expect("FooterTxt")
            .as_textbox()
            .set_text_string(submenu.help_text);
    } else {
        // Drawn under the selected header: neither handle shows as hovered
        slider.state = SliderState::None;
    }
    let selected_min = slider.lower;
    let selected_max = slider.upper;

    let slider_pane = root_pane.find_pane_by_name_recursive_expect("TrModSlider");
    slider_pane.set_visible(true);

    let _background = slider_pane
        .find_pane_by_name_recursive_expect("Background")
        .as_picture();
    let header = slider_pane
        .find_pane_by_name_recursive_expect("Header")
        .as_textbox();
    header.set_text_string(submenu.title);
    let min_button = slider_pane
        .find_pane_by_name_recursive_expect("MinButton")
        .as_picture();
    let max_button = slider_pane
        .find_pane_by_name_recursive_expect("MaxButton")
        .as_picture();
    let min_title_text = min_button
        .find_pane_by_name_recursive_expect("TitleTxt")
        .as_textbox();
    let min_title_bg = min_button
        .find_pane_by_name_recursive_expect("TitleBg")
        .as_picture();
    let min_value_text = min_button
        .find_pane_by_name_recursive_expect("ValueTxt")
        .as_textbox();
    let max_title_text = max_button
        .find_pane_by_name_recursive_expect("TitleTxt")
        .as_textbox();
    let max_title_bg = max_button
        .find_pane_by_name_recursive_expect("TitleBg")
        .as_picture();
    let max_value_text = max_button
        .find_pane_by_name_recursive_expect("ValueTxt")
        .as_textbox();

    min_title_text.set_text_string("Min");
    match slider.state {
        SliderState::LowerHover | SliderState::LowerSelected => {
            min_title_text.text_shadow_enable(true);
            min_title_text.text_outline_enable(true);
            min_title_text.set_color(255, 255, 255, 255);
        }
        _ => {
            min_title_text.text_shadow_enable(false);
            min_title_text.text_outline_enable(false);
            min_title_text.set_color(178, 199, 211, 255);
        }
    }

    max_title_text.set_text_string("Max");
    match slider.state {
        SliderState::UpperHover | SliderState::UpperSelected => {
            max_title_text.text_shadow_enable(true);
            max_title_text.text_outline_enable(true);
            max_title_text.set_color(255, 255, 255, 255);
        }
        _ => {
            max_title_text.text_shadow_enable(false);
            max_title_text.text_outline_enable(false);
            max_title_text.set_color(178, 199, 211, 255);
        }
    }

    min_value_text.set_text_string(&format!("{selected_min}"));
    max_value_text.set_text_string(&format!("{selected_max}"));

    let min_title_bg_material = &mut *min_title_bg.as_picture().material;
    let min_colors = match slider.state {
        SliderState::LowerHover => (BG_LEFT_ON_WHITE_COLOR, BG_LEFT_ON_BLACK_COLOR),
        SliderState::LowerSelected => (BG_LEFT_SELECTED_WHITE_COLOR, BG_LEFT_SELECTED_BLACK_COLOR),
        _ => (BG_LEFT_OFF_WHITE_COLOR, BG_LEFT_OFF_BLACK_COLOR),
    };

    min_title_bg_material.set_white_res_color(min_colors.0);
    min_title_bg_material.set_black_res_color(min_colors.1);

    let max_title_bg_material = &mut *max_title_bg.as_picture().material;
    let max_colors = match slider.state {
        SliderState::UpperHover => (BG_LEFT_ON_WHITE_COLOR, BG_LEFT_ON_BLACK_COLOR),
        SliderState::UpperSelected => (BG_LEFT_SELECTED_WHITE_COLOR, BG_LEFT_SELECTED_BLACK_COLOR),
        _ => (BG_LEFT_OFF_WHITE_COLOR, BG_LEFT_OFF_BLACK_COLOR),
    };

    max_title_bg_material.set_white_res_color(max_colors.0);
    max_title_bg_material.set_black_res_color(max_colors.1);

    min_value_text.set_visible(true);
    max_value_text.set_visible(true);

    // Hide the Icon pane for MinButton and MaxButton
    [min_button, max_button].iter().for_each(|button| {
        let icon = button.find_pane_by_name_recursive_expect("Icon");
        icon.set_visible(false);
    });
}

unsafe fn render_confirmation_page(app: &mut App, root_pane: &Pane) {
    let show_row = 3; // Row in the middle of the page
    let show_cols = [1, 2]; // Columns in the middle of the page
    let no_col = show_cols[0]; // Left
    let yes_col = show_cols[1]; // Right
    let help_text = match app.confirmation_return_page {
        AppPage::TOGGLE | AppPage::SLIDER => {
            "Are you sure you want to reset the current setting to the defaults?"
        }
        AppPage::SUBMENU => "Are you sure you want to reset ALL settings to the defaults?",
        _ => "", // Shouldn't ever get this case, but don't panic if we do
    };

    // Set help text
    root_pane
        .find_pane_by_name_recursive_expect("FooterTxt")
        .as_textbox()
        .set_text_string(help_text);

    // Show only the buttons that we care about
    for row in 0..NX_SUBMENU_ROWS {
        let should_show_row = row == show_row;
        let menu_button_row = root_pane
            .find_pane_by_name_recursive_expect(format!("TrModMenuButtonRow{row}").as_str());
        menu_button_row.set_visible(should_show_row);
        if should_show_row {
            for col in 0..NX_SUBMENU_COLUMNS {
                let should_show_col = show_cols.contains(&col);
                let menu_button = menu_button_row
                    .find_pane_by_name_recursive_expect(format!("Button{col}").as_str());
                menu_button.set_visible(should_show_col);
                if should_show_col {
                    let title_text = menu_button
                        .find_pane_by_name_recursive_expect("TitleTxt")
                        .as_textbox();
                    let title_bg = menu_button
                        .find_pane_by_name_recursive_expect("TitleBg")
                        .as_picture();
                    let title_bg_material = &mut *title_bg.material;

                    if col == no_col {
                        title_text.set_text_string("No");
                    } else if col == yes_col {
                        title_text.set_text_string("Yes");
                    }
                    let is_selected = (col == no_col
                        && app.confirmation_state == ConfirmationState::HoverNo)
                        || (col == yes_col
                            && app.confirmation_state == ConfirmationState::HoverYes);

                    if is_selected {
                        title_text.text_shadow_enable(true);
                        title_text.text_outline_enable(true);
                        title_text.set_color(255, 255, 255, 255);
                        title_bg_material.set_white_res_color(BG_LEFT_ON_WHITE_COLOR);
                        title_bg_material.set_black_res_color(BG_LEFT_ON_BLACK_COLOR);
                    } else {
                        title_text.text_shadow_enable(false);
                        title_text.text_outline_enable(false);
                        title_text.set_color(178, 199, 211, 255);
                        title_bg_material.set_white_res_color(BG_LEFT_OFF_WHITE_COLOR);
                        title_bg_material.set_black_res_color(BG_LEFT_OFF_BLACK_COLOR);
                    }

                    // Hide all submenu icons, since we're not on the submenu page
                    // TODO: Do we want to show the check on "Yes" and a red "X" on "No?"
                    for t in app.tabs.iter().chain(app.profile_tabs.iter()) {
                        for s in t.submenus.iter() {
                            let pane = menu_button.find_pane_by_name_recursive(s.id);
                            if let Some(p) = pane {
                                p.set_visible(false);
                            }
                        }
                    }
                }
            }
        }
    }
}

pub unsafe fn draw(root_pane: &Pane) {
    // Determine if we're in the menu by seeing if the "help" footer has
    // begun moving upward. It starts at -80 and moves to 0 over 10 frames
    // in info_training_in_menu.bflan
    assign(
        &VANILLA_MENU_ACTIVE,
        pane_cache::find_expect(root_pane, "L_staying_help").pos_y != -80.0,
    );

    let overall_parent_pane = pane_cache::find_expect(root_pane, "TrModMenu");
    overall_parent_pane.set_visible(read(&QUICK_MENU_ACTIVE) && !read(&VANILLA_MENU_ACTIVE));
    let menu_close_wait_frame = frame_counter::get_frame_count(*MENU_CLOSE_FRAME_COUNTER);
    fade_out(
        overall_parent_pane,
        MENU_CLOSE_WAIT_FRAMES - menu_close_wait_frame,
        MENU_CLOSE_WAIT_FRAMES,
    );

    // Only submit updates if we have received input
    let received_input = read(&MENU_RECEIVED_INPUT);
    if !received_input {
        return;
    } else {
        assign(&MENU_RECEIVED_INPUT, false);
    }

    // Make all invisible first
    for row_idx in 0..NX_SUBMENU_ROWS {
        for col_idx in 0..NX_SUBMENU_COLUMNS {
            let menu_button_row = root_pane.find_pane_by_name_recursive_expect(
                format!("TrModMenuButtonRow{row_idx}").as_str(),
            );
            menu_button_row.set_visible(false);

            let menu_button = menu_button_row
                .find_pane_by_name_recursive_expect(format!("Button{col_idx}").as_str());
            menu_button.set_visible(false);

            menu_button
                .find_pane_by_name_recursive_expect("ValueTxt")
                .set_visible(false);
        }
    }

    // Make normal training panes invisible if we're active
    // InfluencedAlpha means "Should my children panes' alpha be influenced by mine, as the parent?"
    let status_r_pane = root_pane
        .find_pane_by_name_recursive("status_R")
        .expect("Unable to find status_R pane");
    // status_r_pane.flags |= 1 << PaneFlag::InfluencedAlpha as u8;
    status_r_pane.set_visible(!read(&QUICK_MENU_ACTIVE));

    root_pane
        .find_pane_by_name_recursive_expect("TrModSlider")
        .set_visible(false);

    // Update menu display
    // Grabbing lock as read-only, essentially
    let mut app = lock_write(&QUICK_MENU_APP);
    // We don't really need to change anything, but get_before_selected requires &mut self

    draw_title(&app, root_pane);
    let tab_titles = app.tab_titles();

    let is_gcc = read(&P1_CONTROLLER_STYLE) == ControllerStyle::GCController;
    let button_mapping = if is_gcc {
        &(*GCC_BUTTON_MAPPING)
    } else {
        &(*PROCON_BUTTON_MAPPING)
    };

    let (x_key, y_key, l_key, r_key, zl_key, zr_key, z_key) = (
        button_mapping.get("X"),
        button_mapping.get("Y"),
        button_mapping.get("L"),
        button_mapping.get("R"),
        button_mapping.get("ZL"),
        button_mapping.get("ZR"),
        button_mapping.get("Z"),
    );

    let (left_tab_key, right_tab_key, save_defaults_key, clear_toggle_key) = if is_gcc {
        (l_key, r_key, x_key, y_key)
    } else {
        (zl_key, zr_key, x_key, y_key)
    };
    // Reset is Z on a GameCube controller and Y+R on a Pro Controller (R alone switches CPU there). Switch CPU
    // is Y+L/R on a GameCube controller and L/R on a Pro Controller.
    let plus = b'+' as u16;
    let slash = b'/' as u16;
    let reset_glyphs: Vec<u16> = if is_gcc {
        vec![*z_key.unwrap()]
    } else {
        vec![*y_key.unwrap(), plus, *r_key.unwrap()]
    };
    let switch_cpu_glyphs: Vec<u16> = if is_gcc {
        vec![*y_key.unwrap(), plus, *l_key.unwrap(), slash, *r_key.unwrap()]
    } else {
        vec![*l_key.unwrap(), slash, *r_key.unwrap()]
    };

    [
        (left_tab_key, "LeftTab"),
        (None, "CurrentTab"),
        (right_tab_key, "RightTab"),
    ]
    .iter()
    .enumerate()
    .for_each(|(idx, (key, name))| {
        let key_help_pane = root_pane.find_pane_by_name_recursive_expect(name);

        let icon_pane = key_help_pane
            .find_pane_by_name_recursive_expect("set_txt_icon")
            .as_textbox();
        let help_pane = key_help_pane
            .find_pane_by_name_recursive_expect("set_txt_help")
            .as_textbox();
        icon_pane.set_text_string("");

        // Left/Right tabs have keys
        if let Some(key) = key {
            set_icon_text(icon_pane, &[**key]);
        }

        if *name == "CurrentTab" {
            icon_pane.set_text_string("");
            // Center tab should be highlighted
            help_pane.set_default_material_colors();
            help_pane.set_color(255, 255, 0, 255);
        }
        help_pane.set_text_string(tab_titles[idx]);
    });

    // Right of the tabs: Save Defaults (root tabs) or Switch CPU (profile tabs, two CPUs or more, not while
    // confirming a reset), Reset, and Clear Toggle in a weighted toggle menu
    let mut items: Vec<KeyHelp> = Vec::new();
    let mut hidden: Vec<&'static str> = Vec::new();
    if app.editing_profile_idx.is_none() {
        items.push(KeyHelp {
            name: "SaveDefaults",
            glyphs: vec![*save_defaults_key.unwrap()],
            title: "Save Defaults",
            short: "Save",
        });
    } else if app.active_cpu_count() > 1 && app.page != AppPage::CONFIRMATION {
        items.push(KeyHelp {
            name: "SaveDefaults",
            glyphs: switch_cpu_glyphs,
            title: "Switch CPU",
            short: "CPU",
        });
    } else {
        hidden.push("SaveDefaults");
    }
    let reset_title = match app.page {
        AppPage::SUBMENU => Some(("Reset All", "Reset")),
        AppPage::SLIDER | AppPage::TOGGLE => Some(("Reset Current", "Reset")),
        AppPage::PROFILE | AppPage::CONFIRMATION | AppPage::CLOSE => None,
    };
    match reset_title {
        Some((title, short)) => items.push(KeyHelp {
            name: "ResetDefaults",
            glyphs: reset_glyphs,
            title,
            short,
        }),
        None => hidden.push("ResetDefaults"),
    }
    if app.should_show_clear_keyhelp() {
        // Only in a multiple selection toggle menu with toggle.max > 1
        items.push(KeyHelp {
            name: "ClearToggle",
            glyphs: vec![*clear_toggle_key.unwrap()],
            title: "Clear Toggle",
            short: "Clear",
        });
    } else {
        hidden.push("ClearToggle");
    }
    let reset_idx = items.iter().position(|k| k.name == "ResetDefaults");
    let shorten_first: Vec<usize> = reset_idx
        .into_iter()
        .chain(0..items.len())
        .collect();
    // After the right tab's title at its longest, so the key helps stay put from tab to tab
    let start = 960.0 + KEY_HELP_ANCHOR + KEY_HELP_TEXT_MAX_W;
    layout_key_helps(root_pane, start, &items, &shorten_first, &hidden);

    match app.page {
        AppPage::PROFILE => render_profile_header_page(&mut app, root_pane),
        AppPage::SUBMENU => render_submenu_page(&mut app, root_pane, true),
        AppPage::SLIDER => render_slider_page(&mut app, root_pane, true),
        AppPage::TOGGLE => render_toggle_page(&mut app, root_pane, true),
        AppPage::CONFIRMATION => render_confirmation_page(&mut app, root_pane),
        AppPage::CLOSE => {}
    }
}
