use core::f64::consts::PI;

use smash::app::{self, lua_bind::*, sv_system};
use smash::lib::lua_const::*;
use smash::lib::L2CValue;
use smash::lua2cpp::L2CFighterCommon;

use crate::common::consts::*;
use crate::common::*;
use training_mod_sync::*;

const NUM_ENTRIES: usize = 4;
fn eidx() -> usize {
    (CURRENT_CPU_ENTRY_ID.load(core::sync::atomic::Ordering::Relaxed) as usize).min(NUM_ENTRIES - 1)
}

static DI_CASE: [RwLock<Direction>; NUM_ENTRIES] = [
    RwLock::new(Direction::empty()), RwLock::new(Direction::empty()),
    RwLock::new(Direction::empty()), RwLock::new(Direction::empty()),
];

pub fn roll_di_case() {
    let mut di_case_lock = lock_write(&DI_CASE[eidx()]);
    if *di_case_lock != Direction::empty() {
        return;
    }
    *di_case_lock = current_profile().di_state.get_random();
}

pub fn reset_di_case(module_accessor: &mut app::BattleObjectModuleAccessor) {
    if is_in_hitstun(module_accessor) {
        return;
    }
    let mut di_case_lock = lock_write(&DI_CASE[eidx()]);
    if *di_case_lock != Direction::empty() {
        *di_case_lock = Direction::empty();
    }
}

#[skyline::hook(replace = smash::lua2cpp::L2CFighterCommon_FighterStatusDamage__correctDamageVectorCommon)]
pub unsafe fn handle_correct_damage_vector_common(
    fighter: &mut L2CFighterCommon,
    arg1: L2CValue,
) -> L2CValue {
    if is_training_mode() {
        mod_handle_di(fighter, arg1);
    }

    original!()(fighter, arg1)
}

unsafe fn mod_handle_di(fighter: &L2CFighterCommon, _arg1: L2CValue) {
    let module_accessor = sv_system::battle_object_module_accessor(fighter.lua_state_agent);
    crate::training::set_current_entry_id(module_accessor);

    if current_profile().di_state == Direction::empty() {
        return;
    }

    if !is_operation_cpu(module_accessor) {
        return;
    }

    roll_di_case();
    let di_case = read(&DI_CASE[eidx()]);
    let angle_tuple = di_case.into_angle().map_or((0.0, 0.0), |angle| {
        let a = if should_reverse_angle(di_case) {
            PI - angle
        } else {
            angle
        };

        (a.cos(), a.sin())
    });

    set_x_y(module_accessor, angle_tuple.0 as f32, angle_tuple.1 as f32);
}

pub fn should_reverse_angle(direction: Direction) -> bool {
    let cpu_id = match eidx() {
        0 => FighterId::Player,
        1 => FighterId::CPU,
        2 => FighterId::CPU2,
        3 => FighterId::CPU3,
        _ => FighterId::CPU,
    };
    let cpu_module_accessor = match try_get_module_accessor(cpu_id) {
        Some(acc) => acc,
        None => return false,
    };
    let player_module_accessor = try_get_module_accessor(FighterId::Player)
        .expect("Could not get player module accessor in should_reverse_angle");
    unsafe {
        PostureModule::pos_x(player_module_accessor) > PostureModule::pos_x(cpu_module_accessor)
            && ![Direction::LEFT, Direction::RIGHT].contains(&direction)
    }
}

fn set_x_y(module_accessor: &mut app::BattleObjectModuleAccessor, x: f32, y: f32) {
    unsafe {
        WorkModule::set_float(
            module_accessor,
            x,
            *FIGHTER_STATUS_DAMAGE_WORK_FLOAT_VECOR_CORRECT_STICK_X,
        );
        WorkModule::set_float(
            module_accessor,
            y,
            *FIGHTER_STATUS_DAMAGE_WORK_FLOAT_VECOR_CORRECT_STICK_Y,
        );
    }
}

pub fn get_command_flag_cat(module_accessor: &mut app::BattleObjectModuleAccessor) {
    if !is_operation_cpu(module_accessor) {
        return;
    }

    reset_di_case(module_accessor);
}
