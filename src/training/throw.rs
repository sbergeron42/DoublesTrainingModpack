use core::sync::atomic::{AtomicU32, Ordering};

use smash::app::{self, lua_bind::*};
use smash::lib::lua_const::*;

use crate::common::consts::*;
use crate::common::*;
use crate::training::frame_counter;
use crate::training::mash;
use training_mod_sync::*;

const NUM_ENTRIES: usize = 4;
fn eidx() -> usize {
    (CURRENT_CPU_ENTRY_ID.load(core::sync::atomic::Ordering::Relaxed) as usize).min(NUM_ENTRIES - 1)
}

const NOT_SET: u32 = 9001;
static THROW_DELAY: [AtomicU32; NUM_ENTRIES] = [
    AtomicU32::new(NOT_SET), AtomicU32::new(NOT_SET),
    AtomicU32::new(NOT_SET), AtomicU32::new(NOT_SET),
];
static PUMMEL_DELAY: [AtomicU32; NUM_ENTRIES] = [
    AtomicU32::new(NOT_SET), AtomicU32::new(NOT_SET),
    AtomicU32::new(NOT_SET), AtomicU32::new(NOT_SET),
];
static THROW_CASE: [RwLock<ThrowOption>; NUM_ENTRIES] = [
    RwLock::new(ThrowOption::empty()), RwLock::new(ThrowOption::empty()),
    RwLock::new(ThrowOption::empty()), RwLock::new(ThrowOption::empty()),
];

static THROW_DELAY_COUNTERS: LazyLock<[usize; NUM_ENTRIES]> = LazyLock::new(|| [
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
]);
static PUMMEL_DELAY_COUNTERS: LazyLock<[usize; NUM_ENTRIES]> = LazyLock::new(|| [
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
    frame_counter::register_counter(frame_counter::FrameCounterType::InGame),
]);

// Rolling Throw Delays and Pummel Delays separately

pub fn reset_throw_delay() {
    let ei = eidx();
    if THROW_DELAY[ei].load(Ordering::Relaxed) != NOT_SET {
        THROW_DELAY[ei].store(NOT_SET, Ordering::Relaxed);
        frame_counter::full_reset(THROW_DELAY_COUNTERS[ei]);
    }
}

pub fn reset_pummel_delay() {
    let ei = eidx();
    if PUMMEL_DELAY[ei].load(Ordering::Relaxed) != NOT_SET {
        PUMMEL_DELAY[ei].store(NOT_SET, Ordering::Relaxed);
        frame_counter::full_reset(PUMMEL_DELAY_COUNTERS[ei]);
    }
}

pub fn reset_throw_case() {
    let ei = eidx();
    if read(&THROW_CASE[ei]) != ThrowOption::empty() {
        assign(&THROW_CASE[ei], ThrowOption::empty());
    }
}

fn roll_throw_delay() {
    let ei = eidx();
    if THROW_DELAY[ei].load(Ordering::Relaxed) == NOT_SET {
        THROW_DELAY[ei].store(
            current_profile().throw_delay.get_random().into_meddelay(),
            Ordering::Relaxed,
        );
    }
}

fn roll_pummel_delay() {
    let ei = eidx();
    if PUMMEL_DELAY[ei].load(Ordering::Relaxed) == NOT_SET {
        PUMMEL_DELAY[ei].store(
            current_profile().pummel_delay.get_random().into_meddelay(),
            Ordering::Relaxed,
        );
    }
}

fn roll_throw_case() {
    let ei = eidx();
    if read(&THROW_CASE[ei]) == ThrowOption::empty() {
        assign(&THROW_CASE[ei], current_profile().throw_state.get_random());
    }
}

pub unsafe fn get_command_flag_throw_direction(
    module_accessor: &mut app::BattleObjectModuleAccessor,
) -> i32 {
    if !is_operation_cpu(module_accessor) {
        return 0;
    }

    if StatusModule::status_kind(module_accessor) != *FIGHTER_STATUS_KIND_CATCH_WAIT
        && StatusModule::status_kind(module_accessor) != *FIGHTER_STATUS_KIND_CATCH_PULL
        && StatusModule::status_kind(module_accessor) != *FIGHTER_STATUS_KIND_CATCH_ATTACK
    {
        // No longer holding character, so re-roll the throw case and reset the delay counter for next time
        reset_throw_case();
        reset_throw_delay();

        reset_pummel_delay();
        return 0;
    }

    if !WorkModule::is_enable_transition_term(
        // If you can't throw right now, don't bother
        module_accessor,
        *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_THROW_HI,
    ) {
        return 0;
    }

    roll_throw_delay();
    roll_throw_case();

    roll_pummel_delay();

    let ei = eidx();

    if read(&THROW_CASE[ei]) == ThrowOption::NONE {
        // Do nothing, but don't reroll the throw case.
        return 0;
    }

    if frame_counter::should_delay(THROW_DELAY[ei].load(Ordering::Relaxed), THROW_DELAY_COUNTERS[ei]) {
        // Not yet time to perform the throw action
        if frame_counter::should_delay(PUMMEL_DELAY[ei].load(Ordering::Relaxed), PUMMEL_DELAY_COUNTERS[ei]) {
            // And not yet time to pummel either, so don't do anything
            return 0;
        }

        // If no pummel delay is selected (default), then don't pummel
        if current_profile().pummel_delay == MedDelay::empty() {
            return 0;
        }

        // (this conditional would need to be changed to speed up pummelling)
        if StatusModule::status_kind(module_accessor) == *FIGHTER_STATUS_KIND_CATCH_WAIT {
            let status = *FIGHTER_STATUS_KIND_CATCH_ATTACK; //.unwrap_or(0);
            StatusModule::change_status_request_from_script(module_accessor, status, true);
        }

        return 0;
    }

    // If you can uthrow, then throw (since all throws should be possible at the same times)
    if WorkModule::is_enable_transition_term(
        module_accessor,
        *FIGHTER_STATUS_TRANSITION_TERM_ID_CONT_THROW_HI,
    ) {
        let cmd = read(&THROW_CASE[ei]).into_cmd().unwrap_or(0);
        mash::external_buffer_menu_mash(current_profile().mash_state.get_random());
        return cmd;
    }

    0
}
