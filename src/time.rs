use std::{thread, time::Duration};
use std::time::{SystemTime, UNIX_EPOCH};

pub fn sleep(time: i32) {
    thread::sleep(Duration::from_millis(time as u64));
}

pub fn since_epoch() -> u64 {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(dur) => dur.as_secs(),
        Err(_) => panic!("SystemTime before UNIX_EPOCH"),
    }
}