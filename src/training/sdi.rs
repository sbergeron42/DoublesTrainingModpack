use core::f64::consts::PI;
use core::sync::atomic::{AtomicU32, Ordering};

use smash::app::{self, lua_bind::*};
use smash::Vector2f;

use crate::common::consts::*;
use crate::common::*;
use crate::training::directional_influence;
use training_mod_sync::*;

const NUM_ENTRIES: usize = 4;
fn eidx() -> usize {
    (CURRENT_CPU_ENTRY_ID.load(Ordering::Relaxed) as usize).min(NUM_ENTRIES - 1)
}

static COUNTER: [AtomicU32; NUM_ENTRIES] = [
    AtomicU32::new(0), AtomicU32::new(0),
    AtomicU32::new(0), AtomicU32::new(0),
];
static DIRECTION: [RwLock<Direction>; NUM_ENTRIES] = [
    RwLock::new(Direction::NEUTRAL), RwLock::new(Direction::NEUTRAL),
    RwLock::new(Direction::NEUTRAL), RwLock::new(Direction::NEUTRAL),
];

pub fn roll_direction() {
    let ei = eidx();
    COUNTER[ei].store(0, Ordering::Relaxed);
    assign(&DIRECTION[ei], current_profile().sdi_state.get_random());
}

unsafe fn get_sdi_direction() -> Option<f64> {
    let direction = read(&DIRECTION[eidx()]);
    direction.into_angle().map(|angle| {
        if directional_influence::should_reverse_angle(direction) {
            PI - angle
        } else {
            angle
        }
    })
}

#[skyline::hook(replace = FighterControlModuleImpl::check_hit_stop_delay_command)]
pub unsafe fn check_hit_stop_delay_command(
    module_accessor: &mut app::BattleObjectModuleAccessor,
    sdi_direction: *mut Vector2f,
) -> u64 {
    // Function returns 1 if there is an SDI input, 0 is there is not

    if !is_training_mode() || !is_operation_cpu(module_accessor) {
        return original!()(module_accessor, sdi_direction);
    }
    crate::training::set_current_entry_id(module_accessor);
    let repeat = current_profile().sdi_strength.into_u32();
    let ei = eidx();
    let counter_val = (COUNTER[ei].load(Ordering::Relaxed) + 1) % repeat;
    COUNTER[ei].store(counter_val, Ordering::Relaxed);
    if counter_val == repeat - 1 {
        if let Some(angle) = get_sdi_direction() {
            // If there is a non-neutral direction picked,
            // modify the SDI angle Vector2f as a side-effect
            // and return 1 so the CPU knows that an SDI input occurred
            (*sdi_direction).x = angle.cos() as f32;
            (*sdi_direction).y = angle.sin() as f32;
            return 1;
        }
    }
    0
}
