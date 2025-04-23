#![allow(dead_code)]

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenizeError {
    InvalidCharacter(char),
    IntegerOverflow
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    Integer(u32),

    Assign, // =
    EvalFn, // $
    Add,  // +
    Sub, // -
    Mul, // *
    Div, // /
    Pow, // ^
    Rem, // %
    
    Dot, // .
    Comma, // ,
    DoubleQuote, // "
    OpenParen, // (
    CloseParen, // )
    CommentLine, // #
    ExclamationMark, // !
}

pub fn tokenize(line: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = line.chars().peekable();
    let mut at: usize = 0;

    while let Some(&ch) = chars.peek() {
        macro_rules! next {
            () => {
                chars.next();
                at += 1;
            };
        }

        match ch {
            ' ' | '\t' | '\n' => { next!(); },

            '=' => { tokens.push(Token::Assign); next!(); }
            '$' => { tokens.push(Token::EvalFn); next!(); }
            '+' => { tokens.push(Token::Add); next!(); }
            '-' => { tokens.push(Token::Sub); next!(); }
            '*' => { tokens.push(Token::Mul); next!(); }
            '/' => { tokens.push(Token::Div); next!(); }
            '^' => { tokens.push(Token::Pow); next!(); }
            '%' => { tokens.push(Token::Rem); next!(); }

            '.' => { tokens.push(Token::Dot); next!(); }
            ',' => { tokens.push(Token::Comma); next!(); }
            '(' => { tokens.push(Token::OpenParen); next!(); }
            ')' => { tokens.push(Token::CloseParen); next!(); }
            '#' => { break; }
            '!' => { tokens.push(Token::ExclamationMark); next!(); }

            '\"' => {
                next!();
                let mut string = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '\"' { break; }
                    string.push(c);
                    next!();
                }
                next!();
                tokens.push(Token::StringLiteral(string));
            },

            _ if ch.is_alphabetic() || ch == '_' => {
                let mut ident = String::new();
                while let Some(&c) = chars.peek() {
                    if !c.is_alphanumeric() && c != '_' { break; }
                    ident.push(c);
                    next!();
                }
                tokens.push(Token::Identifier(ident));
            },

            _ if ch.is_ascii_digit() => {
                let mut number = String::new();

                while let Some(&c) = chars.peek() {
                    if !c.is_ascii_digit() { break; }
                    number.push(c);
                    next!();
                }

                let int = if let Ok(int) = number.parse::<u32>() {
                    int
                } else {
                    return Err(TokenizeError::IntegerOverflow)
                };

                tokens.push(Token::Integer(int));
            }

            _ => {
                return Err(TokenizeError::InvalidCharacter(line.chars().nth(at).unwrap()))
            }
        }
    }

    Ok(tokens)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Operation {
    Set,
    Add,
    Substract,
    Multiply,
    Divide,
    Remainder
}

impl Operation {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Assign => Some(Operation::Set),
            Token::Add => Some(Operation::Add),
            Token::Sub => Some(Operation::Substract),
            Token::Mul => Some(Operation::Multiply),
            Token::Div => Some(Operation::Divide),
            Token::Rem => Some(Operation::Remainder),
            _ => None
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogType {
    Error,
    VmError,
    Warning,
}

impl LogType {
    pub fn display(&self) -> String {
        String::from(match self {
            LogType::Error => "error",
            LogType::VmError => "vm error",
            LogType::Warning => "warning",
        })
    }
}

pub fn alert(log_type: LogType, at: usize, what: &String) {
    println!("\n{}: at line {}:\n  {}", log_type.display(), at, what);
}

/* #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum EvalError {
    UnexpectedToken(Token),
    IntegerOverflow,
    TooDeepPriority,
    UnexpectedCloseParen,
    ExpectedCloseParen,
    ExpectedValueBefore,
    ExpectedValueAfter,
    ExpectedValueAfterOpenParen,
    DivisionByZero,
}

pub fn eval(tokens: &[Token]) -> Result<Vec<Token>, EvalError> {
    const DEBUG: bool = true;

    // 1. check for invalid tokens:
    for token in tokens {
        use Token::*;
        let allowed_tokens = [
            Add, Sub, Mul, Div,
            OpenParen, CloseParen, ExclamationMark
        ];

        let is_integer: bool = matches!(token, Integer(_));
        let is_stringliteral: bool = matches!(token, StringLiteral(_));

        if (!allowed_tokens.contains(token) || is_stringliteral) && !is_integer {
            return Err(EvalError::UnexpectedToken(token.clone()));
        }
    }

    // 2. count parentheses:
    let parens: u8 = {
        // 2.1. count:
        let mut parens: u8 = 0;
        if tokens.contains(&Token::OpenParen) {
            for token in tokens {
                if let Token::OpenParen = token {
                    parens += 1;
                }
                if parens >= u8::MAX - 5 /* max priority */ {
                    return Err(EvalError::TooDeepPriority)
                }
            }
        }

        // 2.2. count closed parentheses (check):
        let mut closed: u8 = 0;
        for token in tokens {
            if let Token::CloseParen = token {
                closed += 1;
            }
        }

        use std::cmp::Ordering;

        match closed.cmp(&parens) {
            Ordering::Greater => return Err(EvalError::UnexpectedCloseParen),
            Ordering::Less => return Err(EvalError::ExpectedCloseParen),
            Ordering::Equal => {}
        }

        parens
    };

    // 3. removing dead braces "(INT)":
    /* let tokens = {
        let mut buffer = tokens;
        for (pos, token) in tokens.iter().enumerate() {
            if let Token::OpenParen = token {
                let int = if let Some(Token::Integer(int)) = buffer.get(pos+1) {
                    int
                } else {
                    return Err(EvalError::ExpectedValueAfterOpenParen)
                };

                if let Some(Token::CloseParen) = buffer.get(pos+2) {}
            }
        }
        buffer
    }; */

    // 4. prioritize operators:
    let priority: Vec<Option<u8>> = {
        use super::consts::priority;
        let mut priority: Vec<Option<u8>> = Vec::new();
        let mut base: u8 = parens;
        for token in tokens {
            priority.push(
                match token {
                    Token::Integer(_) => None,
                    Token::Add => Some(base + priority::ADD),
                    Token::Sub => Some(base + priority::SUB),
                    Token::Mul => Some(base + priority::MUL),
                    Token::Div => Some(base + priority::DIV),

                    Token::ExclamationMark => Some(base + priority::L_NOT),

                    Token::OpenParen => {
                        base -= 1;
                        None
                    }
                    Token::CloseParen => {
                        base += 1;
                        None
                    }

                    _ => None
                }
            );
        }
        priority
    };

    // 5. shorten expression by computing constant values:
    let mut result: Vec<Token> = tokens.to_vec();
    let max_level = parens + 5;
    for level in 0..=max_level {
        'help: loop {
            // result.iter() will take read-only reference over `result`.
            // but we are going to replace tokens, so we create a buffer
            // and then move the contents of buffer to `result`.
            let mut buffer = result.clone();
            let mut ienum = result.iter().enumerate();
            for (pos, token) in ienum {
                if let Some(priority) = priority[pos] {
                    if level == priority {
                        let left_required: bool = {
                            let where_left_unrequired = [
                                Token::ExclamationMark
                            ];
                            !where_left_unrequired.contains(token)
                        };

                        if DEBUG {
                            println!("\n{:?}", buffer);
                        }

                        let left: u32 = {
                            if left_required {
                                if let Some(Token::Integer(int)) = buffer.get(pos-1) {
                                    *int
                                } else {
                                    return Err(EvalError::ExpectedValueBefore)
                                }
                            } else {
                                0 // none
                            }
                        };

                        let right: u32 = if let Some(Token::Integer(int)) = buffer.get(pos+1) {
                            *int
                        } else {
                            return Err(EvalError::ExpectedValueAfter)
                        };

                        if DEBUG {
                            println!("{}\t{:?}\t{}", left, token, right);
                        }

                        buffer.remove(pos+1);
                        buffer.remove(pos);
                        if left_required { buffer.remove(pos-1); }
                        buffer.insert(
                            pos-1,
                            Token::Integer(
                                match token {
                                    Token::Add => { left + right }
                                    Token::Sub => { left - right }
                                    Token::Mul => { left * right }
                                    Token::Div => {
                                        if right != 0 {
                                            left / right
                                        } else {
                                            return Err(EvalError::DivisionByZero)
                                        }
                                    }
                                    Token::ExclamationMark => {
                                        !(if right > 0 { 1 } else { 0 })
                                    }
                                    _ => panic!("unmatched token in `level == priority ... match token`")
                                }
                            )
                        );
                        result = buffer;
                    }
                }
            }
        }
        //result = buffer;
    }

    if DEBUG { println!("\n= {:?}", result); }
    Ok(result)
} */

// eval