use core::sync::atomic::{AtomicBool, AtomicU8, AtomicU32, AtomicUsize, Ordering};

const MAX_COUNTERS: usize = 64;

#[derive(PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum FrameCounterType {
    InGame = 0,
    // "Reset" occurs when we enter training mode and when we run L+R+A or save state load
    // Some frame counters need in-game frames that do not reset when this occurs
    InGameNoReset = 1,
    Real = 2,
}

struct AtomicFrameCounter {
    count: AtomicU32,
    should_count: AtomicBool,
    counter_type: AtomicU8,
}

impl AtomicFrameCounter {
    const fn new() -> Self {
        Self {
            count: AtomicU32::new(0),
            should_count: AtomicBool::new(false),
            counter_type: AtomicU8::new(0),
        }
    }
}

static COUNTERS: [AtomicFrameCounter; MAX_COUNTERS] = {
    const INIT: AtomicFrameCounter = AtomicFrameCounter::new();
    [INIT; MAX_COUNTERS]
};
static COUNTER_LEN: AtomicUsize = AtomicUsize::new(0);

pub fn register_counter(counter_type: FrameCounterType) -> usize {
    let index = COUNTER_LEN.fetch_add(1, Ordering::Relaxed);
    assert!(index < MAX_COUNTERS, "Too many frame counters registered");
    COUNTERS[index].count.store(0, Ordering::Relaxed);
    COUNTERS[index].should_count.store(false, Ordering::Relaxed);
    COUNTERS[index].counter_type.store(counter_type as u8, Ordering::Relaxed);
    index
}

pub fn start_counting(index: usize) {
    COUNTERS[index].should_count.store(true, Ordering::Relaxed);
}

pub fn stop_counting(index: usize) {
    COUNTERS[index].should_count.store(false, Ordering::Relaxed);
}

pub fn _is_counting(index: usize) -> bool {
    COUNTERS[index].should_count.load(Ordering::Relaxed)
}

pub fn reset_frame_count(index: usize) {
    COUNTERS[index].count.store(0, Ordering::Relaxed);
}

/// Resets count to 0 and stops counting.
pub fn full_reset(index: usize) {
    COUNTERS[index].count.store(0, Ordering::Relaxed);
    COUNTERS[index].should_count.store(false, Ordering::Relaxed);
}

/// Returns true until a certain number of frames have passed.
pub fn should_delay(delay: u32, index: usize) -> bool {
    if delay == 0 {
        return false;
    }

    let c = &COUNTERS[index];
    if c.count.load(Ordering::Relaxed) == 0 {
        c.should_count.store(true, Ordering::Relaxed);
    }

    if c.count.load(Ordering::Relaxed) >= delay {
        c.count.store(0, Ordering::Relaxed);
        c.should_count.store(false, Ordering::Relaxed);
        return false;
    }

    true
}

pub fn get_frame_count(index: usize) -> u32 {
    COUNTERS[index].count.load(Ordering::Relaxed)
}

pub fn tick_idx(index: usize) {
    COUNTERS[index].count.fetch_add(1, Ordering::Relaxed);
}

pub fn tick_ingame() {
    let len = COUNTER_LEN.load(Ordering::Relaxed);
    for i in 0..len {
        let c = &COUNTERS[i];
        if !c.should_count.load(Ordering::Relaxed) {
            continue;
        }
        if c.counter_type.load(Ordering::Relaxed) == FrameCounterType::Real as u8 {
            continue;
        }
        c.count.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn tick_real() {
    let len = COUNTER_LEN.load(Ordering::Relaxed);
    for i in 0..len {
        let c = &COUNTERS[i];
        if !c.should_count.load(Ordering::Relaxed) {
            continue;
        }
        let ct = c.counter_type.load(Ordering::Relaxed);
        if ct == FrameCounterType::InGame as u8 || ct == FrameCounterType::InGameNoReset as u8 {
            continue;
        }
        c.count.fetch_add(1, Ordering::Relaxed);
    }
}

pub fn reset_all() {
    let len = COUNTER_LEN.load(Ordering::Relaxed);
    for i in 0..len {
        let c = &COUNTERS[i];
        if c.counter_type.load(Ordering::Relaxed) != FrameCounterType::InGame as u8 {
            continue;
        }
        c.count.store(0, Ordering::Relaxed);
        c.should_count.store(false, Ordering::Relaxed);
    }
}
