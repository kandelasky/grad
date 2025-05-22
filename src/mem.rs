#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Int32(i32, bool /* ptr? */),
    Float32(f32),
    Str(String)
}