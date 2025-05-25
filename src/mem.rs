use std::collections::HashMap;
use crate::lang;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Int32(i32, bool /* ptr? */),
    Float32(f32),
    Str(String),
    Null,
}

pub type VarMap = HashMap<String, Value>;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Function {
    args: Vec<String>,
    body: Vec<Vec<lang::Token>>
}