use crate::{
    shell::{self, ErrorType::*},
};

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

pub fn eval(expr: &str) -> Result<Value, shell::Report> {
    match evalexpr::eval(expr) {
        Ok(value) => {
            match value {
                evalexpr::Value::Int(int) => match i32::try_from(int) {
                    Ok(value) => Ok(Value::Int32(value)),
                    Err(_) => { Err((IntOverflow, Some(String::from("result of the expression caused integer overflow")))) }
                },
                evalexpr::Value::Float(fl) => {
                    let fl = fl as f32;
                    if fl.is_finite() {
                        Ok(Value::Float32(fl))
                    } else {
                        Err((FloatOverflow, Some(String::from("result of the expression caused float overflow"))))
                    }
                },
                evalexpr::Value::Empty => {
                    Err((ExpectedExpr, None))
                }
                _ => panic!("uncovered type in match(eval(&expr)): {:?}", value)
            }
        }
        Err(_) => { Err((InvalidExpr, Some(String::from("when evaluating the expression")))) }
    }
}