use rand::prelude::*;

pub fn uuid() -> String {
    format!("{:X}", ThreadRng::default().random::<u128>())
}

pub fn range_int(from: i32, to: i32) -> i32 {
    ThreadRng::default().random_range(from..to)
}