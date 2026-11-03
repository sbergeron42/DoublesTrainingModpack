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

/// The DI picked for each fighter's current hitstun, cleared once it leaves hitstun: one slot per entry, plus one
/// per entry for Nana. Popo and Nana share an entry; with one slot, whichever of them was out of hitstun cleared
/// the other's DI every frame, so the Ice Climbers' DI was picked again each frame of a hit (13.0.5 hardware).
static DI_CASE: [RwLock<Direction>; NUM_ENTRIES * 2] = [
    RwLock::new(Direction::empty()), RwLock::new(Direction::empty()),
    RwLock::new(Direction::empty()), RwLock::new(Direction::empty()),
    RwLock::new(Direction::empty()), RwLock::new(Direction::empty()),
    RwLock::new(Direction::empty()), RwLock::new(Direction::empty()),
];

fn di_slot(module_accessor: &mut app::BattleObjectModuleAccessor) -> usize {
    let nana = unsafe { app::utility::get_kind(module_accessor) == *FIGHTER_KIND_NANA };
    eidx() + if nana { NUM_ENTRIES } else { 0 }
}

pub fn roll_di_case(slot: usize) {
    let mut di_case_lock = lock_write(&DI_CASE[slot]);
    if *di_case_lock != Direction::empty() {
        return;
    }
    *di_case_lock = current_profile_with(|p| p.di_state).get_random();
}

pub fn reset_di_case(module_accessor: &mut app::BattleObjectModuleAccessor) {
    if is_in_hitstun(module_accessor) {
        return;
    }
    let mut di_case_lock = lock_write(&DI_CASE[di_slot(module_accessor)]);
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

    if current_profile_with(|p| p.di_state) == Direction::empty() {
        return;
    }

    if !is_operation_cpu(module_accessor) {
        return;
    }

    let slot = di_slot(module_accessor);
    roll_di_case(slot);
    let di_case = read(&DI_CASE[slot]);
    let angle_tuple = di_case.into_angle().map_or((0.0, 0.0), |angle| {
        let a = if should_reverse_angle(module_accessor, di_case) {
            PI - angle
        } else {
            angle
        };

        (a.cos(), a.sin())
    });

    set_x_y(module_accessor, angle_tuple.0 as f32, angle_tuple.1 as f32);
}

/// Whether an In/Out direction (DI, SDI, hitstun air dodge) must be mirrored for this fighter. The angles are
/// written for a fighter launched to the right: Out points right, away from whoever hit it. So they are mirrored
/// when the hit launches it to the left (the damage energy's x speed), or, for a straight up or down launch, when
/// it faces right (the game turns a fighter it hits toward the hit). LEFT and RIGHT are never mirrored. UTM
/// compared the CPU's position with P1's instead, so a hit from P2 or another CPU on the other side got In and Out
/// swapped.
pub fn should_reverse_angle(module_accessor: &mut app::BattleObjectModuleAccessor, direction: Direction) -> bool {
    if [Direction::LEFT, Direction::RIGHT].contains(&direction) {
        return false;
    }
    unsafe {
        let launch_speed_x = KineticEnergy::get_speed_x(KineticModule::get_energy(
            module_accessor,
            *FIGHTER_KINETIC_ENERGY_ID_DAMAGE,
        ) as *mut app::KineticEnergy);
        if launch_speed_x != 0.0 {
            launch_speed_x < 0.0
        } else {
            PostureModule::lr(module_accessor) > 0.0
        }
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
