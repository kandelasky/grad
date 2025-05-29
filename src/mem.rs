use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Int32(i32, bool /* ptr? */),
    Float32(f32),
    Str(String),
    Null,
}

pub type VarMap = HashMap<String, Value>;

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub args: HashSet<String>,
    pub body: Vec<String>
}

impl Function {
    pub fn new(args: HashSet<String>, body: Vec<String>) -> Self {
        Self { args, body }
    }
}

impl Default for Function {
    fn default() -> Self {
        Self::new(HashSet::new(), Vec::new())
    }
}

pub type FnMap = HashMap<String, Function>;