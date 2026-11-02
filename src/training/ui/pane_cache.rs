//! Pane lookups by name, cached while a match runs.
//!
//! `find_pane_by_name_recursive` walks a layout's whole pane tree and allocates a C string for the name. The draw
//! hook did about 140 of them per frame during a match (damage digits, team colours, the menu and notification
//! panes), for panes that never move while the match runs. On the Switch that was most of the mod's 0.9 ms per
//! frame of UI work, on the worker the main thread waits for (SSBU Reversing Agent docs/FRAME_PROFILER.md).
//!
//! Results are kept per (parent pane, name) from the first draw of a match until it ends. A cached pane is
//! checked against its own name before use, so one freed and replaced is looked up again. Outside a match every
//! call is a plain lookup, as before.
use std::collections::HashMap;
use std::sync::Mutex;

use skyline::nn::ui2d::Pane;
use smash::ui2d::SmashPane;

/// Calls a miss is trusted before the name is searched again, for panes a layout shows later in the match.
const MISS_RETRY_CALLS: u32 = 60;

enum Entry {
    Found(usize),
    Missing(u32),
}

/// Keyed by (parent pane, name pointer, name length). Names are `&'static str`, so one name has one key.
static CACHE: Mutex<Option<HashMap<(usize, usize, usize), Entry>>> = Mutex::new(None);

/// Caches lookups while `in_match`; dropping out of a match forgets them, so the next one looks panes up afresh.
pub fn set_active(in_match: bool) {
    let mut cache = CACHE.lock().unwrap();
    if in_match != cache.is_some() {
        *cache = in_match.then(HashMap::new);
    }
}

unsafe fn has_name(pane: *const Pane, name: &str) -> bool {
    let stored = &(*pane).name;
    let bytes = name.as_bytes();
    bytes.len() < stored.len()
        && bytes.iter().zip(stored.iter()).all(|(a, b)| *a == *b as u8)
        && stored[bytes.len()] == 0
}

/// `parent.find_pane_by_name_recursive(name)`, cached while a match runs.
pub unsafe fn find<'a>(parent: &'a Pane, name: &'static str) -> Option<&'a mut Pane> {
    let mut guard = CACHE.lock().unwrap();
    let Some(cache) = guard.as_mut() else {
        return parent.find_pane_by_name_recursive(name);
    };
    let key = (parent as *const Pane as usize, name.as_ptr() as usize, name.len());
    match cache.get_mut(&key) {
        Some(Entry::Found(pane)) if has_name(*pane as *const Pane, name) => {
            return (*pane as *mut Pane).as_mut();
        }
        Some(Entry::Missing(retry_in)) if *retry_in > 0 => {
            *retry_in -= 1;
            return None;
        }
        _ => {}
    }
    let found = parent.find_pane_by_name_recursive(name);
    let entry = match &found {
        Some(pane) => Entry::Found(*pane as *const Pane as usize),
        None => Entry::Missing(MISS_RETRY_CALLS),
    };
    cache.insert(key, entry);
    found
}

/// [`find`] for panes the layout always has.
pub unsafe fn find_expect<'a>(parent: &'a Pane, name: &'static str) -> &'a mut Pane {
    find(parent, name).unwrap_or_else(|| panic!("Could not find pane {}", name))
}
