#![allow(dead_code)]

//use crate::consts::instr;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Instruction {
    PageMainStart, PageFnStart,

    DefineFn { id: u16 },

    SetFlagFloat { value: u8 },

    SetValue { value: u32 },
    // SetExprValue { Vec<u32> },
    SetVarValue { id: u16 },
    SetFloatValue { mant: u32, ord: u8 },

    SetFlagMemLog { value: u32 },
    SetFlagMemLogEnd { value: u32 },

    NewVariable { id: u16 },
    DeleteVariable { id: u16 },
    SetVariable { id: u16 },
    CopyVariable { id: u16, id_from: u16 },
    NulleanVariable { id: u16 },

    SetX { value: u32 },
    SetY { value: u32 },
    // expr
    SetVarX { id: u16 },
    SetVarY { id: u16 },

    NewContainer { id: u16 },
    DeleteContainer { id: u16 },
    SetContainer { id: u16 },
    ClearContainer { id: u16 },
    FillContainer { id: u16 },
    CopyContainer { id: u16, id_from: u16 },
    GetContainer { id: u16, var: u16 },
    
    // compare

    JumpCmpTrue { op: usize },
    JumpCmpFalse { op: usize },
    Jump { op: usize },

    Halt,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Page {
    Fn, Main
}

/* #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Unsigned8,
    Unsigned32,
    Float32,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum VecTyped {
    Unsigned8(Vec<u8>),
    Unsigned32(Vec<u32>),
    Float32(Vec<f32>),
} */

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Float32 {
    pub mantissa: u32,
    pub order: u8,
}

impl Float32 {
    pub fn empty() -> Self {
        Self { mantissa: 0, order: 0 }
    }

    pub fn new(mantissa: u32, order: u8) -> Self {
        Self { mantissa, order }
    }

    pub fn try_f32(&self) -> Option<f32> {
        let order = if let Some(order) = 10u32.checked_pow(self.order as u32) {
            order as f32
        } else {
            return None;
        };

        let mantissa = self.mantissa as f32;

        Some(mantissa / order)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Container {
    pub data: Vec<u32>,
    pub size: (usize, usize)
}

impl Container {
    /// Creates a new container, returning `None` if there's a size overflow.
    pub fn new(size: (usize, usize)) -> Option<Self> {
        Some(Self { data: vec![0u32; size.0 * size.1], size })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FloatContainer {
    pub data: Vec<Float32>,
    pub size: (usize, usize)
}

impl FloatContainer {
    /// Creates a new container, returning `None` if there's a size overflow.
    pub fn new(size: (usize, usize)) -> Option<Self> {
        Some(Self {
            data: vec![Default::default() /* i don't understand how it works, but this is Float32 */; size.0 * size.1],
            size
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Integer(u32),
    Float(Float32)
}

pub mod interpreter {
    use super::*;

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub enum ParseError {
        UnknownInstruction
    }
    
    pub struct Runtime {
        func: Vec<Instruction>,
        main: Vec<Instruction>,
        at: usize,
    }
    
    impl Runtime {
        /* pub fn from_bytes(bytes: &[u8]) -> Result<Self, ParseError> {
            let mut stream = bytes.iter().peekable();
            let mut instr: Vec<Instruction> = Vec::new();
            let mut w_dest: Option<WritingDestination> = None;

            if let Some(&byte) = stream.next() {
                match byte {
                    instr::PAGE_MAIN_START => {/* ... */}
                    _ => return Err(ParseError::UnknownInstruction)
                }
            }

            Ok(Self { instr, at_instr: 0 })
        } */

        /* pub fn from_instr(instr: &Vec<Instruction>) -> Result<Self, ParseError> {
            let mut main: Vec<Instruction> = Vec::new();
        } */
    }
}

pub mod compiler {
    // use crate::lang::*;
/*
    pub fn make_bytecode(source: &str) -> Option<Vec<u8>> {
        let mut failed = false;
        let mut instr: Vec<Instruction> = Vec::new();

        let mut variables: Vec<String> = Vec::new();

        for (at_line, line) in source.lines().enumerate() {
            macro_rules! alc {
                ($log_type:expr, $what:expr) => {
                    failed = true;
                    alert($log_type, at_line+1, &$what);
                    continue;
                };
            }

            let tokens = match tokenize(line) {
                Ok(tokens) => {
                    if tokens.is_empty() { continue; }
                    tokens
                }
                Err(at) => {
                    let what = match at {
                        TokenizeError::InvalidCharacter(ch) => format!("unknown start of token: {}", ch),
                        TokenizeError::IntegerOverflow => format!("integer overflow (value > {} - unsigned 32-bit)", u32::MAX)
                    };

                    alc!(LogType::Error, what);
                }
            };

            let calling = {
                let first_token = &tokens[0];
                match first_token {
                    Token::Identifier(string) => string,
                    _ => {
                        if first_token != &Token::CommentLine {
                            alc!(LogType::Error, String::from("expected identifier"));
                        }
                        continue;
                    }
                }
            };

            match calling.as_str() {
                // new, delete, clone, ...
                "log_iwriteln" => {}
                _ => {
                    //
                }
            }
        }

        if !failed {
            Some(translate(instr))
        } else {
            None
        }
    }

    pub fn translate(instr: Vec<Instruction>) -> Vec<u8> {
        let result: Vec<u8> = Vec::new();
        result
    }
*/
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn float32_to_f32() -> Result<(), String> {
        let float32 = Float32::new(12345, 6);
        let float: f32 = 0.012345;

        let try_float = float32.try_f32().unwrap();
        
        println!("try_float\t= {}", try_float);
        println!("float\t\t= {}", float);

        if try_float == float {
            Ok(())
        } else {
            Err(String::from("values doesn't match!"))
        }
    }
}