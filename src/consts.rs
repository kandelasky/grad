macro_rules! consts {
    ($type:ty: $($cfg_name:ident = $value:expr),+) => {
        $(pub const $cfg_name: $type = $value;)+
    };
}

consts! {
&str:
    VERSION_STR = "0.1.0"
}