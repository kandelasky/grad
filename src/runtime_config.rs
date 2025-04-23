macro_rules! configuration {
    ($type:ty: $($cfg_name:ident = $value:expr),+) => {
        $(pub const $cfg_name: $type = $value;)+
    };
}

configuration! {
bool:
    DEBUG = true,
    BREAK_ON_ERROR = false,
    LOG_RUNNING_OP = false
}