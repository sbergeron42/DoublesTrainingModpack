// skyline-smash's SmashTextBox::set_text_string calls a fixed 13.0.4 address (main+0x37a22f0). In 13.0.5
// the same function sits at main+0x37a28a0 and the old address is other code, so the mod uses this trait
// instead: same signature and behaviour, address chosen per game version.
use skyline::hooks::{getRegionAddress, Region};
use skyline::libc::c_char;
use skyline::nn::ui2d::TextBox;

use crate::common::offsets::by_version;

pub trait SmashTextBox {
    unsafe fn set_text_string(&mut self, s: &str);
}

impl SmashTextBox for TextBox {
    unsafe fn set_text_string(&mut self, s: &str) {
        type PaneSetTextString = extern "C" fn(*mut TextBox, *const c_char);
        let pane_set_text_string: PaneSetTextString = core::mem::transmute(
            getRegionAddress(Region::Text) as usize + by_version(0x37a22f0, 0x37a28a0),
        );
        let text = [s.as_bytes(), b"\0"].concat();
        pane_set_text_string(self, text.as_ptr() as *const c_char);
    }
}
