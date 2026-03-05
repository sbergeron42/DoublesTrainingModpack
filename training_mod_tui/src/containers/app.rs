use serde::ser::{SerializeMap, Serializer};
use serde::Serialize;
use std::collections::HashMap;

use crate::{InputControl, StatefulList, SubMenu, SubMenuType, Tab};

pub const MAX_CPU_SLOTS: usize = 3;
pub const MAX_PROFILES: usize = 8;

#[derive(PartialEq, Serialize, Clone, Copy)]
pub enum AppPage {
    CPU_SETUP,
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

// Menu structure is:
// App <StatefulTable<Tab>>
// │
// ├─ CPU_SETUP page (assign profiles to CPU slots)
// │
// ├─ Root Tabs (global settings: Misc, Save States, Input Recording, Button Config)
// │  └─ Tab <StatefulTable<Submenu>>
// │     └─ Submenu → Toggle/Slider
// │
// └─ Profile Tabs (per-CPU: Mash, Override, Defensive)
//    └─ Tab <StatefulTable<Submenu>>
//       └─ Submenu → Toggle/Slider
#[derive(Clone)]
pub struct App<'a> {
    /// Root-level tabs (global settings)
    pub tabs: StatefulList<Tab<'a>>,
    /// Per-CPU profile tabs (Mash, Override, Defensive)
    pub profile_tabs: StatefulList<Tab<'a>>,
    /// Which profile index is being edited (None = root level / CPU_SETUP)
    pub editing_profile_idx: Option<usize>,
    /// Which profile each CPU slot uses (indices into profile pool)
    pub cpu_profile_assign: [u8; MAX_CPU_SLOTS],
    /// Selected entry index on CPU_SETUP page (0..MAX_CPU_SLOTS-1, maps to entry_id+1)
    pub cpu_setup_row: usize,
    /// Which entries 1-3 are CPUs (vs human/nonexistent)
    pub cpu_is_active: [bool; MAX_CPU_SLOTS],
    pub page: AppPage,
    pub serialized_settings: String,
    pub serialized_default_settings: String,
    pub serialized_default_profile_settings: String,
    pub confirmation_state: ConfirmationState,
    pub confirmation_return_page: AppPage,
}

impl<'a> App<'a> {
    pub fn new() -> App<'a> {
        App {
            tabs: StatefulList::new(),
            profile_tabs: StatefulList::new(),
            editing_profile_idx: None,
            cpu_profile_assign: [0; MAX_CPU_SLOTS],
            cpu_setup_row: 0,
            cpu_is_active: [true; MAX_CPU_SLOTS],
            page: AppPage::CPU_SETUP,
            serialized_settings: String::new(),
            serialized_default_settings: String::new(),
            serialized_default_profile_settings: String::new(),
            confirmation_state: ConfirmationState::HoverNo,
            confirmation_return_page: AppPage::SUBMENU,
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

    /// Move cpu_setup_row to the next active CPU entry (wrapping)
    fn next_active_cpu(&mut self) {
        for _ in 0..MAX_CPU_SLOTS {
            self.cpu_setup_row = (self.cpu_setup_row + 1) % MAX_CPU_SLOTS;
            if self.cpu_is_active[self.cpu_setup_row] {
                return;
            }
        }
    }

    /// Move cpu_setup_row to the previous active CPU entry (wrapping)
    fn prev_active_cpu(&mut self) {
        for _ in 0..MAX_CPU_SLOTS {
            if self.cpu_setup_row == 0 {
                self.cpu_setup_row = MAX_CPU_SLOTS - 1;
            } else {
                self.cpu_setup_row -= 1;
            }
            if self.cpu_is_active[self.cpu_setup_row] {
                return;
            }
        }
    }

    /// Ensure cpu_setup_row points to an active entry
    pub fn snap_to_active_cpu(&mut self) {
        if !self.cpu_is_active[self.cpu_setup_row] {
            self.next_active_cpu();
        }
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
            AppPage::CPU_SETUP => {
                // Only enter profile editing for active CPU entries
                if !self.cpu_is_active[self.cpu_setup_row] {
                    return;
                }
                // Enter profile editing for the selected CPU's profile
                let profile_idx = self.cpu_profile_assign[self.cpu_setup_row] as usize;
                self.editing_profile_idx = Some(profile_idx);
                self.page = AppPage::SUBMENU;
                // Reset profile tab selection to first tab
                if self.profile_tabs.state.selected().is_none()
                    || self.profile_tabs.items.is_empty()
                {
                    // nop if no profile tabs
                } else {
                    self.profile_tabs.state.select(Some(0));
                }
            }
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
            AppPage::CPU_SETUP => {
                // Close the menu
                self.page = AppPage::CLOSE;
            }
            AppPage::SUBMENU => {
                if self.editing_profile_idx.is_some() {
                    // Exit profile editing, return to CPU_SETUP
                    // (caller is responsible for saving profile data)
                    self.editing_profile_idx = None;
                    self.page = AppPage::CPU_SETUP;
                } else {
                    // Return to CPU_SETUP from root tabs
                    self.page = AppPage::CPU_SETUP;
                }
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
        match self.page {
            AppPage::CPU_SETUP => {
                // Save defaults applies to root settings only
                self.save_default_settings();
            }
            _ => {
                if self.editing_profile_idx.is_none() {
                    self.save_default_settings();
                }
            }
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
            AppPage::CPU_SETUP => {
                self.prev_active_cpu();
            }
            AppPage::SUBMENU => {
                self.active_tabs_mut()
                    .get_selected()
                    .expect("No tab selected!")
                    .on_up();
            }
            AppPage::TOGGLE | AppPage::SLIDER => {
                self.selected_submenu().on_up();
            }
            AppPage::CONFIRMATION | AppPage::CLOSE => {}
        }
    }
    fn on_down(&mut self) {
        match self.page {
            AppPage::CPU_SETUP => {
                self.next_active_cpu();
            }
            AppPage::SUBMENU => {
                self.active_tabs_mut()
                    .get_selected()
                    .expect("No tab selected!")
                    .on_down();
            }
            AppPage::TOGGLE | AppPage::SLIDER => {
                self.selected_submenu().on_down();
            }
            AppPage::CONFIRMATION | AppPage::CLOSE => {}
        }
    }
    fn on_left(&mut self) {
        match self.page {
            AppPage::CPU_SETUP => {
                if !self.cpu_is_active[self.cpu_setup_row] {
                    return;
                }
                // Cycle profile assignment down
                let val = &mut self.cpu_profile_assign[self.cpu_setup_row];
                if *val == 0 {
                    *val = (MAX_PROFILES - 1) as u8;
                } else {
                    *val -= 1;
                }
            }
            AppPage::SUBMENU => {
                self.active_tabs_mut()
                    .get_selected()
                    .expect("No tab selected!")
                    .on_left();
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
            AppPage::CPU_SETUP => {
                if !self.cpu_is_active[self.cpu_setup_row] {
                    return;
                }
                // Cycle profile assignment up
                let val = &mut self.cpu_profile_assign[self.cpu_setup_row];
                *val = (*val + 1) % MAX_PROFILES as u8;
            }
            AppPage::SUBMENU => {
                self.active_tabs_mut()
                    .get_selected()
                    .expect("No tab selected!")
                    .on_right();
            }
            AppPage::TOGGLE | AppPage::SLIDER => {
                self.selected_submenu().on_right();
            }
            AppPage::CONFIRMATION => self.confirmation_state = self.confirmation_state.switch(),
            AppPage::CLOSE => {}
        }
    }
    fn on_start(&mut self) {
        // Close menu from anywhere
        self.editing_profile_idx = None;
        self.page = AppPage::CLOSE;
    }
    fn on_l(&mut self) {}
    fn on_r(&mut self) {
        match self.page {
            AppPage::CPU_SETUP => {}
            _ => {
                self.confirmation_return_page = self.page;
                self.page = AppPage::CONFIRMATION;
            }
        }
    }
    fn on_zl(&mut self) {
        match self.page {
            AppPage::CPU_SETUP => {
                // Switch to root tabs
                self.editing_profile_idx = None;
                self.page = AppPage::SUBMENU;
                // Select last root tab (cycling left from CPU_SETUP)
                let len = self.tabs.items.len();
                if len > 0 {
                    self.tabs.state.select(Some(len - 1));
                }
            }
            AppPage::SUBMENU => {
                if self.editing_profile_idx.is_some() {
                    self.profile_tabs.previous();
                } else {
                    // If at first root tab, go back to CPU_SETUP
                    if self.tabs.state.selected() == Some(0) {
                        self.page = AppPage::CPU_SETUP;
                    } else {
                        self.tabs.previous();
                    }
                }
            }
            _ => {}
        }
    }
    fn on_zr(&mut self) {
        match self.page {
            AppPage::CPU_SETUP => {
                // Switch to root tabs
                self.editing_profile_idx = None;
                self.page = AppPage::SUBMENU;
                // Select first root tab (cycling right from CPU_SETUP)
                if !self.tabs.items.is_empty() {
                    self.tabs.state.select(Some(0));
                }
            }
            AppPage::SUBMENU => {
                if self.editing_profile_idx.is_some() {
                    self.profile_tabs.next();
                } else {
                    // If at last root tab, go back to CPU_SETUP
                    let len = self.tabs.items.len();
                    if self.tabs.state.selected() == Some(len - 1) {
                        self.page = AppPage::CPU_SETUP;
                    } else {
                        self.tabs.next();
                    }
                }
            }
            _ => {}
        }
    }
}
