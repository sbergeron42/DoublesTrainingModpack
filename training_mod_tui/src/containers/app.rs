use serde::ser::{SerializeMap, Serializer};
use serde::Serialize;
use std::collections::HashMap;

use crate::{InputControl, StatefulList, SubMenu, SubMenuType, Tab};

pub const MAX_CPU_SLOTS: usize = 3;
pub const MAX_PROFILES: usize = 8;

#[derive(PartialEq, Debug, Serialize, Clone, Copy)]
pub enum AppPage {
    /// The profile header on the CPU profile tabs: Left/Right choose the edited CPU's profile.
    PROFILE,
    SUBMENU,
    TOGGLE,
    SLIDER,
    CONFIRMATION,
    CLOSE,
}

#[derive(PartialEq, Clone, Copy)]
pub enum ConfirmationState {
    HoverNo,
    HoverYes,
}

impl ConfirmationState {
    pub fn switch(&self) -> ConfirmationState {
        match self {
            ConfirmationState::HoverNo => ConfirmationState::HoverYes,
            ConfirmationState::HoverYes => ConfirmationState::HoverNo,
        }
    }
}

// Menu structure is one cycle of tabs (ZL/ZR):
// App
// │
// ├─ Profile Tabs (per-CPU: Mash, Override, Defensive), showing the profile of the CPU being edited
// │  ├─ PROFILE: the header above the buttons, Left/Right choose that CPU's profile. Up from the top row of
// │  │  the buttons, or of an open setting, selects it; Down, A or B go back where it was opened from.
// │  └─ Tab <StatefulTable<Submenu>>
// │     └─ Submenu → Toggle/Slider
// │
// └─ Root Tabs (global settings: Input Recording, Button Config, Save States, Misc, Doubles)
//    └─ Tab <StatefulTable<Submenu>>
//       └─ Submenu → Toggle/Slider
//
// The CPU being edited is switched from anywhere with `switch_cpu` (L/R on a Pro Controller, Y+L/R on a
// GameCube controller), keeping the open tab, button and setting.
#[derive(Clone)]
pub struct App<'a> {
    /// Root-level tabs (global settings)
    pub tabs: StatefulList<Tab<'a>>,
    /// Per-CPU profile tabs (Mash, Override, Defensive)
    pub profile_tabs: StatefulList<Tab<'a>>,
    /// The profile shown on the profile tabs while they are the active tabs (None = a root tab is active)
    pub editing_profile_idx: Option<usize>,
    /// The profile whose values `profile_tabs` currently hold (the plugin loads and saves it)
    pub loaded_profile: Option<usize>,
    /// Which profile each CPU slot uses (indices into profile pool)
    pub cpu_profile_assign: [u8; MAX_CPU_SLOTS],
    /// The CPU slot being edited (0..MAX_CPU_SLOTS-1, maps to entry_id+1)
    pub edit_cpu: usize,
    /// Which entries 1-3 are CPUs (vs human/nonexistent)
    pub cpu_is_active: [bool; MAX_CPU_SLOTS],
    pub page: AppPage,
    pub serialized_settings: String,
    pub serialized_default_settings: String,
    pub serialized_default_profile_settings: String,
    pub confirmation_state: ConfirmationState,
    pub confirmation_return_page: AppPage,
    /// Where the profile header was selected from (the buttons, or an open toggle or slider setting)
    pub profile_return_page: AppPage,
}

impl<'a> App<'a> {
    pub fn new() -> App<'a> {
        App {
            tabs: StatefulList::new(),
            profile_tabs: StatefulList::new(),
            editing_profile_idx: Some(0),
            loaded_profile: None,
            cpu_profile_assign: [0; MAX_CPU_SLOTS],
            edit_cpu: 0,
            cpu_is_active: [true; MAX_CPU_SLOTS],
            page: AppPage::SUBMENU,
            serialized_settings: String::new(),
            serialized_default_settings: String::new(),
            serialized_default_profile_settings: String::new(),
            confirmation_state: ConfirmationState::HoverNo,
            confirmation_return_page: AppPage::SUBMENU,
            profile_return_page: AppPage::SUBMENU,
        }
    }

    /// Get the currently active tab list (profile tabs when editing, root tabs otherwise)
    pub fn active_tabs(&self) -> &StatefulList<Tab<'a>> {
        if self.editing_profile_idx.is_some() {
            &self.profile_tabs
        } else {
            &self.tabs
        }
    }

    /// Get the currently active tab list mutably
    pub fn active_tabs_mut(&mut self) -> &mut StatefulList<Tab<'a>> {
        if self.editing_profile_idx.is_some() {
            &mut self.profile_tabs
        } else {
            &mut self.tabs
        }
    }

    /// Serialize root tabs (global settings) to JSON
    pub fn current_settings_to_json(&self) -> String {
        serde_json::to_string(&self).expect("Could not serialize the menu to JSON!")
    }

    pub fn get_serialized_settings_with_defaults(&self) -> String {
        format!(
            "{{\"menu\":{}, \"defaults_menu\":{}}}",
            self.serialized_settings, self.serialized_default_settings
        )
    }

    pub fn save_settings(&mut self) {
        self.serialized_settings = self.current_settings_to_json();
    }

    pub fn save_default_settings(&mut self) {
        self.serialized_default_settings = self.current_settings_to_json();
    }

    pub fn load_defaults(&mut self) {
        if self.editing_profile_idx.is_some() {
            let json = self.serialized_default_profile_settings.clone();
            self.update_profile_from_json(&json);
        } else {
            let json = self.serialized_default_settings.clone();
            self.update_all_from_json(&json);
        }
    }

    pub fn load_defaults_for_current_submenu(&mut self) {
        let submenu_id = self.selected_submenu().id;
        if self.editing_profile_idx.is_some() {
            let json = self.serialized_default_profile_settings.clone();
            self.update_profile_one_from_json(&json, submenu_id);
        } else {
            let json = self.serialized_default_settings.clone();
            self.update_one_from_json(&json, submenu_id);
        }
    }

    /// Update root tabs from JSON
    pub fn update_all_from_json(&mut self, json: &str) {
        let all_settings: HashMap<String, Vec<u8>> =
            serde_json::from_str(json).expect("Could not parse the json!");
        for tab in self.tabs.iter_mut() {
            for submenu_opt in tab.submenus.iter_mut() {
                if let Some(submenu) = submenu_opt {
                    if let Some(val) = all_settings.get(submenu.id) {
                        submenu.update_from_vec(val.clone());
                    }
                }
            }
        }
        self.save_settings();
    }

    #[allow(unused_labels)]
    pub fn update_one_from_json(&mut self, json: &str, submenu_id: &str) {
        let all_settings: HashMap<String, Vec<u8>> =
            serde_json::from_str(json).expect("Could not parse the json!");
        if let Some(val) = all_settings.get(submenu_id) {
            'tabs_scope: for tab in self.tabs.iter_mut() {
                'submenus_scope: for submenu_opt in tab.submenus.iter_mut() {
                    if let Some(submenu) = submenu_opt {
                        if submenu.id == submenu_id {
                            submenu.update_from_vec(val.clone());
                            break 'tabs_scope;
                        }
                    }
                }
            }
        }
        self.save_settings();
    }

    // --- Profile tab serialization ---

    /// Serialize profile tabs to JSON (flat map of submenu_id → values)
    pub fn profile_settings_to_json(&self) -> String {
        let mut map: HashMap<&str, serde_json::Value> = HashMap::new();
        for tab in self.profile_tabs.iter() {
            for submenu in tab.submenus.iter() {
                let value = serde_json::to_value(submenu)
                    .expect("Could not serialize profile submenu!");
                map.insert(submenu.id, value);
            }
        }
        serde_json::to_string(&map).expect("Could not serialize profile to JSON!")
    }

    /// Update profile tabs from JSON
    pub fn update_profile_from_json(&mut self, json: &str) {
        let all_settings: HashMap<String, Vec<u8>> =
            serde_json::from_str(json).expect("Could not parse profile json!");
        for tab in self.profile_tabs.iter_mut() {
            for submenu_opt in tab.submenus.iter_mut() {
                if let Some(submenu) = submenu_opt {
                    if let Some(val) = all_settings.get(submenu.id) {
                        submenu.update_from_vec(val.clone());
                    }
                }
            }
        }
    }

    /// Update a single profile submenu from JSON
    #[allow(unused_labels)]
    pub fn update_profile_one_from_json(&mut self, json: &str, submenu_id: &str) {
        let all_settings: HashMap<String, Vec<u8>> =
            serde_json::from_str(json).expect("Could not parse profile json!");
        if let Some(val) = all_settings.get(submenu_id) {
            'tabs_scope: for tab in self.profile_tabs.iter_mut() {
                'submenus_scope: for submenu_opt in tab.submenus.iter_mut() {
                    if let Some(submenu) = submenu_opt {
                        if submenu.id == submenu_id {
                            submenu.update_from_vec(val.clone());
                            break 'tabs_scope;
                        }
                    }
                }
            }
        }
    }

    pub fn confirm(&mut self) -> bool {
        self.confirmation_state == ConfirmationState::HoverYes
    }

    pub fn return_from_confirmation(&mut self) {
        self.confirmation_state = ConfirmationState::HoverNo;
        self.page = self.confirmation_return_page;
    }

    pub fn selected_tab(&mut self) -> &mut Tab<'a> {
        self.active_tabs_mut().get_selected().expect("No tab selected!")
    }

    pub fn selected_submenu(&mut self) -> &mut SubMenu<'a> {
        self.active_tabs_mut()
            .get_selected()
            .expect("No tab selected!")
            .submenus
            .get_selected()
            .expect("No submenu selected!")
    }

    /// Move edit_cpu to the next active CPU entry (wrapping)
    fn next_active_cpu(&mut self) {
        for _ in 0..MAX_CPU_SLOTS {
            self.edit_cpu = (self.edit_cpu + 1) % MAX_CPU_SLOTS;
            if self.cpu_is_active[self.edit_cpu] {
                return;
            }
        }
    }

    /// Move edit_cpu to the previous active CPU entry (wrapping)
    fn prev_active_cpu(&mut self) {
        for _ in 0..MAX_CPU_SLOTS {
            if self.edit_cpu == 0 {
                self.edit_cpu = MAX_CPU_SLOTS - 1;
            } else {
                self.edit_cpu -= 1;
            }
            if self.cpu_is_active[self.edit_cpu] {
                return;
            }
        }
    }

    /// Ensure edit_cpu points to an active entry
    pub fn snap_to_active_cpu(&mut self) {
        if !self.cpu_is_active[self.edit_cpu] {
            self.next_active_cpu();
        }
    }

    pub fn has_active_cpu(&self) -> bool {
        self.cpu_is_active.iter().any(|&a| a)
    }

    pub fn active_cpu_count(&self) -> usize {
        self.cpu_is_active.iter().filter(|&&a| a).count()
    }

    /// The profile the CPU being edited uses.
    pub fn edit_profile(&self) -> usize {
        self.cpu_profile_assign[self.edit_cpu] as usize
    }

    /// The other active CPU slots that use the same profile as the CPU being edited.
    pub fn profile_shared_with(&self) -> Vec<usize> {
        (0..MAX_CPU_SLOTS)
            .filter(|&i| {
                i != self.edit_cpu
                    && self.cpu_is_active[i]
                    && self.cpu_profile_assign[i] == self.cpu_profile_assign[self.edit_cpu]
            })
            .collect()
    }

    /// Show the profile tabs, at tab `tab`.
    fn enter_profile_tabs(&mut self, tab: usize) {
        self.editing_profile_idx = Some(self.edit_profile());
        self.profile_tabs.state.select(Some(tab));
    }

    /// Show the root tabs, at tab `tab`.
    fn enter_root_tabs(&mut self, tab: usize) {
        self.editing_profile_idx = None;
        self.tabs.state.select(Some(tab));
    }

    /// The menu is opening: back on the tab it was left on, editing an active CPU. With no CPU in the match,
    /// only the root tabs are shown. The plugin reloads the profile tabs (`loaded_profile` is cleared).
    pub fn open(&mut self) {
        self.page = AppPage::SUBMENU;
        self.profile_return_page = AppPage::SUBMENU;
        self.snap_to_active_cpu();
        if self.profile_tabs.state.selected().is_none() && !self.profile_tabs.items.is_empty() {
            self.profile_tabs.state.select(Some(0));
        }
        if self.tabs.state.selected().is_none() && !self.tabs.items.is_empty() {
            self.tabs.state.select(Some(0));
        }
        if self.editing_profile_idx.is_some() {
            if self.has_active_cpu() {
                self.editing_profile_idx = Some(self.edit_profile());
            } else {
                self.enter_root_tabs(0);
            }
        }
        self.loaded_profile = None;
    }

    /// On the profile tabs, edit the previous (`step` < 0) or next active CPU. The open tab, button and setting
    /// stay as they are; the plugin then loads the new CPU's profile into them. Returns whether the CPU changed.
    pub fn switch_cpu(&mut self, step: i32) -> bool {
        match self.page {
            AppPage::SUBMENU | AppPage::TOGGLE | AppPage::SLIDER | AppPage::PROFILE => {}
            AppPage::CONFIRMATION | AppPage::CLOSE => return false,
        }
        if self.editing_profile_idx.is_none() {
            return false;
        }
        let before = self.edit_cpu;
        if step < 0 {
            self.prev_active_cpu();
        } else {
            self.next_active_cpu();
        }
        if self.editing_profile_idx.is_some() {
            self.editing_profile_idx = Some(self.edit_profile());
        }
        self.edit_cpu != before
    }

    /// Select the profile header from the current page, to come back to it with Down, A or B.
    fn open_profile_header(&mut self) {
        self.profile_return_page = self.page;
        self.page = AppPage::PROFILE;
    }

    /// Change the profile the CPU being edited uses by `step`, wrapping.
    fn cycle_edit_profile(&mut self, step: i32) {
        let val = &mut self.cpu_profile_assign[self.edit_cpu];
        *val = (*val as i32 + step).rem_euclid(MAX_PROFILES as i32) as u8;
        self.editing_profile_idx = Some(self.edit_profile());
    }

    pub fn should_show_clear_keyhelp(&mut self) -> bool {
        if self.page != AppPage::TOGGLE {
            return false;
        }
        let submenu = self.selected_submenu();
        match submenu.submenu_type {
            SubMenuType::ToggleMultiple => submenu.selected_toggle().max > 1,
            _ => false,
        }
    }
}

/// Serialize always serializes root tabs (for TrainingModpackMenu compatibility)
impl<'a> Serialize for App<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Always serialize root tabs for the main menu JSON
        let len: usize = self.tabs.iter().map(|tab| tab.len()).sum();
        let mut map = serializer.serialize_map(Some(len))?;
        for tab in self.tabs.iter() {
            for submenu in tab.submenus.iter() {
                map.serialize_entry(submenu.id, submenu)?;
            }
        }
        map.end()
    }
}

impl<'a> InputControl for App<'a> {
    fn on_a(&mut self) {
        match self.page {
            AppPage::PROFILE => self.page = self.profile_return_page,
            AppPage::SUBMENU => {
                self.page = match self.selected_submenu().submenu_type {
                    SubMenuType::ToggleSingle => AppPage::TOGGLE,
                    SubMenuType::ToggleMultiple => AppPage::TOGGLE,
                    SubMenuType::Slider => AppPage::SLIDER,
                };
                self.selected_tab().on_a()
            }
            AppPage::TOGGLE => self.selected_submenu().on_a(),
            AppPage::SLIDER => self.selected_submenu().on_a(),
            AppPage::CONFIRMATION => {
                if self.confirm() {
                    match self.confirmation_return_page {
                        AppPage::SUBMENU => {
                            self.load_defaults();
                        }
                        AppPage::TOGGLE | AppPage::SLIDER => {
                            self.load_defaults_for_current_submenu();
                        }
                        _ => {}
                    }
                }
                self.return_from_confirmation();
            }
            AppPage::CLOSE => {}
        }
        self.save_settings();
    }
    fn on_b(&mut self) {
        match self.page {
            AppPage::PROFILE => self.page = self.profile_return_page,
            AppPage::SUBMENU => {
                // Close the menu (the plugin saves the profile being edited)
                self.page = AppPage::CLOSE;
            }
            AppPage::TOGGLE => {
                self.page = AppPage::SUBMENU;
            }
            AppPage::SLIDER => {
                let slider = self
                    .selected_submenu()
                    .slider
                    .as_mut()
                    .expect("No slider selected!");
                if !slider.is_handle_selected() {
                    self.page = AppPage::SUBMENU;
                } else {
                    self.selected_submenu().on_b();
                }
            }
            AppPage::CONFIRMATION => {
                self.return_from_confirmation();
            }
            AppPage::CLOSE => {}
        }
        self.save_settings();
    }
    fn on_x(&mut self) {
        // Save defaults applies to root settings only
        if self.editing_profile_idx.is_none() {
            self.save_default_settings();
        }
    }
    fn on_y(&mut self) {
        match self.page {
            AppPage::TOGGLE => self.selected_submenu().on_y(),
            _ => {}
        }
    }
    fn on_up(&mut self) {
        match self.page {
            AppPage::SUBMENU => {
                // Up from the top row of a profile tab selects the profile header
                let on_top_row = self
                    .selected_tab()
                    .submenus
                    .state
                    .selected_row()
                    == Some(0);
                if self.editing_profile_idx.is_some() && on_top_row {
                    self.open_profile_header();
                } else {
                    self.selected_tab().on_up();
                }
            }
            AppPage::TOGGLE => {
                // Up from the top row of an open setting selects the header too
                let on_top_row = self.selected_submenu().toggles.state.selected_row() == Some(0);
                if self.editing_profile_idx.is_some() && on_top_row {
                    self.open_profile_header();
                } else {
                    self.selected_submenu().on_up();
                }
            }
            AppPage::SLIDER => {
                // Up moves a selected slider handle; with none selected it selects the header
                let handle_selected = self
                    .selected_submenu()
                    .slider
                    .as_mut()
                    .map_or(false, |s| s.is_handle_selected());
                if self.editing_profile_idx.is_some() && !handle_selected {
                    self.open_profile_header();
                } else {
                    self.selected_submenu().on_up();
                }
            }
            AppPage::PROFILE | AppPage::CONFIRMATION | AppPage::CLOSE => {}
        }
    }
    fn on_down(&mut self) {
        match self.page {
            AppPage::PROFILE => self.page = self.profile_return_page,
            AppPage::SUBMENU => {
                self.selected_tab().on_down();
            }
            AppPage::TOGGLE | AppPage::SLIDER => {
                self.selected_submenu().on_down();
            }
            AppPage::CONFIRMATION | AppPage::CLOSE => {}
        }
    }
    fn on_left(&mut self) {
        match self.page {
            AppPage::PROFILE => self.cycle_edit_profile(-1),
            AppPage::SUBMENU => {
                self.selected_tab().on_left();
            }
            AppPage::TOGGLE | AppPage::SLIDER => {
                self.selected_submenu().on_left();
            }
            AppPage::CONFIRMATION => self.confirmation_state = self.confirmation_state.switch(),
            AppPage::CLOSE => {}
        }
    }
    fn on_right(&mut self) {
        match self.page {
            AppPage::PROFILE => self.cycle_edit_profile(1),
            AppPage::SUBMENU => {
                self.selected_tab().on_right();
            }
            AppPage::TOGGLE | AppPage::SLIDER => {
                self.selected_submenu().on_right();
            }
            AppPage::CONFIRMATION => self.confirmation_state = self.confirmation_state.switch(),
            AppPage::CLOSE => {}
        }
    }
    fn on_start(&mut self) {
        // Close menu from anywhere (the plugin saves the profile being edited)
        self.page = AppPage::CLOSE;
    }
    fn on_l(&mut self) {
        self.switch_cpu(-1);
    }
    fn on_r(&mut self) {
        // Reset (Y+R on a Pro Controller, Z on a GameCube controller)
        match self.page {
            AppPage::SUBMENU | AppPage::TOGGLE | AppPage::SLIDER => {
                self.confirmation_return_page = self.page;
                self.page = AppPage::CONFIRMATION;
            }
            AppPage::PROFILE | AppPage::CONFIRMATION | AppPage::CLOSE => {}
        }
    }
    fn on_zl(&mut self) {
        if self.page == AppPage::PROFILE && self.profile_return_page != AppPage::SUBMENU {
            return;
        }
        match self.page {
            AppPage::SUBMENU | AppPage::PROFILE => {
                self.page = AppPage::SUBMENU;
                if self.editing_profile_idx.is_some() {
                    if self.profile_tabs.state.selected() == Some(0) && !self.tabs.items.is_empty() {
                        self.enter_root_tabs(self.tabs.items.len() - 1);
                    } else {
                        self.profile_tabs.previous();
                    }
                } else if self.tabs.state.selected() == Some(0)
                    && self.has_active_cpu()
                    && !self.profile_tabs.items.is_empty()
                {
                    self.enter_profile_tabs(self.profile_tabs.items.len() - 1);
                } else {
                    self.tabs.previous();
                }
            }
            _ => {}
        }
    }
    fn on_zr(&mut self) {
        if self.page == AppPage::PROFILE && self.profile_return_page != AppPage::SUBMENU {
            return;
        }
        match self.page {
            AppPage::SUBMENU | AppPage::PROFILE => {
                self.page = AppPage::SUBMENU;
                if self.editing_profile_idx.is_some() {
                    let last = self.profile_tabs.items.len().saturating_sub(1);
                    if self.profile_tabs.state.selected() == Some(last) && !self.tabs.items.is_empty() {
                        self.enter_root_tabs(0);
                    } else {
                        self.profile_tabs.next();
                    }
                } else {
                    let last = self.tabs.items.len().saturating_sub(1);
                    if self.tabs.state.selected() == Some(last)
                        && self.has_active_cpu()
                        && !self.profile_tabs.items.is_empty()
                    {
                        self.enter_profile_tabs(0);
                    } else {
                        self.tabs.next();
                    }
                }
            }
            _ => {}
        }
    }
}

impl<'a> App<'a> {
    /// Titles of the tabs before, at and after the active one in the single tab cycle: profile tabs (when a
    /// CPU is in the match), then root tabs.
    pub fn tab_titles(&self) -> [&'a str; 3] {
        let mut titles: Vec<&'a str> = Vec::new();
        if self.has_active_cpu() {
            titles.extend(self.profile_tabs.items.iter().map(|t| t.title));
        }
        let root_start = titles.len();
        titles.extend(self.tabs.items.iter().map(|t| t.title));
        if titles.is_empty() {
            return ["", "", ""];
        }
        let current = match self.editing_profile_idx {
            Some(_) => self.profile_tabs.state.selected().unwrap_or(0),
            None => root_start + self.tabs.state.selected().unwrap_or(0),
        }
        .min(titles.len() - 1);
        let n = titles.len();
        [titles[(current + n - 1) % n], titles[current], titles[(current + 1) % n]]
    }
}
