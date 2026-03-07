use smash::app::{self};

use crate::common::consts::*;
use crate::common::*;
use training_mod_sync::*;

const NUM_ENTRIES: usize = 4;
fn eidx() -> usize {
    (CURRENT_CPU_ENTRY_ID.load(core::sync::atomic::Ordering::Relaxed) as usize).min(NUM_ENTRIES - 1)
}

static ATTACK_ANGLE_DIRECTION: [RwLock<AttackAngle>; NUM_ENTRIES] = [
    RwLock::new(AttackAngle::NEUTRAL), RwLock::new(AttackAngle::NEUTRAL),
    RwLock::new(AttackAngle::NEUTRAL), RwLock::new(AttackAngle::NEUTRAL),
];

pub fn roll_direction() {
    assign(
        &ATTACK_ANGLE_DIRECTION[eidx()],
        current_profile().attack_angle.get_random(),
    );
}

pub unsafe fn mod_get_stick_dir(
    module_accessor: &mut app::BattleObjectModuleAccessor,
) -> Option<f32> {
    if !is_operation_cpu(module_accessor) {
        return None;
    }

    match read(&ATTACK_ANGLE_DIRECTION[eidx()]) {
        AttackAngle::UP => Some(1.0),
        AttackAngle::DOWN => Some(-1.0),
        _ => None,
    }
}
