#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Int32(i32),
    Float32(f32),
    Str(String)
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Variable {
    pub value: Value,
    pub is_pointer: bool,
}

impl Variable {
    pub fn new(value: Value, is_pointer: bool) -> Self {
        Self { value, is_pointer }
    }
}