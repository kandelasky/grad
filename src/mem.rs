use std::collections::{HashMap, HashSet};

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
    pub args: HashSet<String>,
    pub body: Vec<String>
}

impl Function {
    pub fn new(at: usize, args: HashSet<String>, body: Vec<String>) -> Self {
        Self { at, args, body }
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new(0, HashSet::new(), Vec::new())
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