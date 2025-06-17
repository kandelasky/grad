macro_rules! consts {
    ($type:ty: $($cfg_name:ident = $value:expr)+) => {
        $(pub const $cfg_name: $type = $value;)+
    };
}

consts! {
usize:
    MAX_SCOPES = 256
    MAX_IDENT_LENGTH = 40
    MAX_CONTROL_DEPTH = 256
    MAX_CALL_DEPTH = 8192
}

/// `GRADCODE`
pub const MAGIC: [u8; 8] = [0x47, 0x52, 0x41, 0x44, 0x43, 0x4F, 0x44, 0x45];