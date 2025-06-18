use std::collections::HashMap;

use crate::lang::TokensGroup;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Int32(i32, bool /* ptr? */),
    Float32(f32),
    Str(String),
    Null,
}

pub type VarMap = HashMap<String, Value>;

pub fn fill(variables: &mut VarMap, map: &VarMap) {
    for (ident, value) in map {
        variables.insert(ident.to_string(), value.clone());
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub at: usize,
    pub body: TokensGroup
}

impl Function {
    pub fn new(at: usize, body: TokensGroup) -> Self {
        Self { at, body }
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new(0, Vec::new())
    }
}

pub type FnMap = HashMap<String, Function>;

#[derive(Clone, Debug, PartialEq)]
pub struct FnCall<'a> {
    pub function: &'a Function,
    pub at: usize,
    pub depth: u32
}

impl<'a> FnCall<'a> {
    pub fn from_fn(function: &'a Function) -> Self {
        Self { function, at: 0, depth: 0 }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueType {
    Int32,
    Float32,
    Str,
    Null,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        let string = match self {
            ValueType::Int32 => "integer",
            ValueType::Float32 => "float",
            ValueType::Str => "string",
            ValueType::Null => "null",
        };

        write!(f, "{}", string)
    }
}