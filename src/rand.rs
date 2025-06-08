use quad_rand as qrand;

pub fn uuid() -> String {
    format!("{:X}", qrand::gen_range(0, u64::MAX))
}

pub fn int(low: i32, high: i32) -> i32 {
    qrand::gen_range(low, high)
}

pub fn prepare() {
    qrand::srand(crate::time::since_epoch());
}