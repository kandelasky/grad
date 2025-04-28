#![allow(dead_code)]

pub mod priority {
    pub const REM: u8   = 0; // %
    pub const DIV: u8   = 0;
    pub const MUL: u8   = 0;
    pub const L_NOT: u8 = 0; // logical NOT
    pub const B_NOT: u8 = 0; // per byte NOT
    
    pub const ADD: u8 = 1;
    pub const SUB: u8 = 1;
    
    pub const SHIFT_R: u8 = 2;
    pub const SHIFT_L: u8 = 2;
    
    pub const EQ: u8 = 3;
    pub const NE: u8 = 3;
    pub const LT: u8 = 3;
    pub const GT: u8 = 3;
    pub const LE: u8 = 3;
    pub const GE: u8 = 3;
    
    pub const B_OR: u8  = 4;
    pub const B_XOR: u8 = 4;
    pub const B_AND: u8 = 4;

    pub const L_OR: u8 = 5;
    pub const L_AND: u8 = 5;
}

pub mod instr {
    macro_rules! const_new {
        ($name:ident, $value:expr) => (
            pub const $name: u8 = $value;
        )
    }

    // TODO: replace this with macro as in config.rs

    const_new!(PAGE_MAIN_START, 0);
    const_new!(PAGE_FN_START, 1);

    const_new!(DEFINE_FN, 2);

    const_new!(SET_FLAG_FLOAT, 34);

    const_new!(SET_VALUE, 29);
    const_new!(SET_EXPR_VALUE, 30);
    const_new!(SET_VAR_VALUE, 31);
    const_new!(SET_FLOAT_VALUE, 41);

    const_new!(SET_FLAG_MEM_LOG, 35);
    const_new!(SET_FLAG_MEM_LOG_END, 36);

    const_new!(NEW_VARIABLE, 3);
    const_new!(DELETE_VARIABLE, 4);
    const_new!(SET_VARIABLE, 5);
    const_new!(NULLEAN_VARIABLE, 8);

    const_new!(SET_X, 24);
    const_new!(SET_Y, 25);
    const_new!(SET_EXPR_X, 26);
    const_new!(SET_EXPR_Y, 27);
    const_new!(SET_VAR_X, 32);
    const_new!(SET_VAR_Y, 33);

    const_new!(SET_MODE_ADDR, 40);
    const_new!(SET_VAR_ADDR, 39);

    const_new!(NEW_CONTAINER, 14);
    const_new!(DELETE_CONTAINER, 15);
    const_new!(SET_CONTAINER, 16);
    const_new!(CLEAR_CONTAINER, 18);
    const_new!(FILL_CONTAINER, 19);
    const_new!(COPY_CONTAINER, 22);
    const_new!(GET_CONTAINER, 23);

    const_new!(COMPARE_EXPR, 9);
    const_new!(COMPARE_EXPR_INV, 10);

    const_new!(DISPLAY_CONTAINER, 37);

    const_new!(HALT, 28);
}

// "GRADCODE"
pub const MAGIC: [u8; 8] = [0x47, 0x52, 0x41, 0x44, 0x43, 0x4F, 0x44, 0x45];