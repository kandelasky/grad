use std::collections::HashMap;

use crate::{mem::{self, Value}, shell::*};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenizeError {
    InvalidCharacter(char),
    IntegerOverflow /* (usize) */,
    FloatParseError
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    Integer(i32),
    Float(f32),
    Null,

    Assign, // =
    DollarSign, // $
    Add,  // +
    Substract, // -
    Multiply, // *
    Divide, // /
    Exponent, // ^
    Remainder, // %
    VerticalLine, // |
    Ampersand, // &
    MoreThan, // >
    LessThan, // <
    
    Dot, // .
    Comma, // ,
    DoubleQuote, // "
    OpenParen, // (
    CloseParen, // )
    CommentLine, // #
    ExclamationMark, // !
    At, // @
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
            '$' => { tokens.push(Token::DollarSign); next!(); }
            '+' => { tokens.push(Token::Add); next!(); }
            '-' => { tokens.push(Token::Substract); next!(); }
            '*' => { tokens.push(Token::Multiply); next!(); }
            '/' => { tokens.push(Token::Divide); next!(); }
            '^' => { tokens.push(Token::Exponent); next!(); }
            '%' => { tokens.push(Token::Remainder); next!(); }
            '&' => { tokens.push(Token::Ampersand); next!(); }
            '|' => { tokens.push(Token::VerticalLine); next!(); }
            '>' => { tokens.push(Token::MoreThan); next!(); }
            '<' => { tokens.push(Token::LessThan); next!(); }

            '.' => { tokens.push(Token::Dot); next!(); }
            ',' => { tokens.push(Token::Comma); next!(); }
            '(' => { tokens.push(Token::OpenParen); next!(); }
            ')' => { tokens.push(Token::CloseParen); next!(); }
            '#' => { break; }
            '!' => { tokens.push(Token::ExclamationMark); next!(); }
            '@' => { tokens.push(Token::At); next!(); }

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
                tokens.push(
                    if ident == *"null" {
                        Token::Null
                    } else {
                        Token::Identifier(ident)
                    }
                );
            },

            _ if ch.is_ascii_digit() => {
                let mut number = String::new();

                while let Some(&c) = chars.peek() {
                    if !c.is_ascii_digit() { break; }
                    number.push(c);
                    next!();
                }

                let int = if let Ok(int) = number.parse::<i32>() {
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

    let tokens = process_floats(&tokens)?;

    Ok(tokens)
}

// sorry for using AI generated code :(
fn process_floats(tokens: &[Token]) -> Result<Vec<Token>, TokenizeError> {
    let mut result = Vec::new();
    let n = tokens.len();
    
    let mut i = 0;
    while i < n {
        if i + 2 < n {
            if let (Token::Integer(whole), Token::Dot, Token::Integer(fractional)) = 
                (&tokens[i], &tokens[i+1], &tokens[i+2]) 
            {
                if let Ok(number) = format!("{}.{}", whole, fractional).parse::<f32>() {
                    if number.is_infinite() {
                        return Err(TokenizeError::FloatParseError)
                    }
                    result.push(Token::Float(number));
                    i += 3;
                    continue
                } else {
                    return Err(TokenizeError::FloatParseError)
                }
            }
        }
        result.push(tokens[i].clone());
        i += 1;
    }

    Ok(result)
}

pub fn exprify(toks: &[Token], variables: Option<&HashMap<String, mem::Value>>) -> Result<String, ErrorType> {
    // 1. replace `true` with 1 and `false` with 0
    let mut tokens_ = {
        let mut tokens = toks.to_vec();
        for (at, token) in toks.iter().enumerate() {
            //dbg!(token);
            if let Token::Identifier(id) = token {
                //eprintln!("replaced");
                if id == "true" {
                    tokens[at] = Token::Integer(1);
                } else if id == "false" {
                    tokens[at] = Token::Integer(0);
                }
            }
        }
        tokens
    };

    // 2. replace variables with theirs value
    tokens_ = if let Some(variables) = variables {
        let mut tokens = tokens_.clone();
        for (at, token) in tokens_.iter().enumerate() {
            if let Token::Identifier(id) = token {
                if let Some(value) = variables.get(id) {
                    match value {
                        Value::Int32(int, is_ptr) => if !is_ptr {
                            tokens[at] = Token::Integer(*int)
                        } else {
                            return Err(ErrorType::UnexpectedPointer(id.to_string()))
                        },
                        Value::Float32(int) => tokens[at] = Token::Float(*int),
                        Value::Str(_) => return Err(ErrorType::StringInExpr)
                    }
                } else {
                    return Err(ErrorType::UndefinedVariable(id.to_string()))
                }
            }
        }
        tokens
    } else {
        // do nothing
        tokens_
    };

    // 3. stringify
    let mut result = String::new();
    for token in tokens_ {
        //result.push(' ');
        let pusheen = match token {
            Token::Integer(int) => &int.to_string(),
            Token::Float(fl) => &fl.to_string(),
            Token::Assign => "=",
            Token::Add => "+",
            Token::Substract => "-",
            Token::Multiply => "*",
            Token::Divide => "/",
            Token::Exponent => "^",
            Token::Remainder => "%",
            Token::Ampersand => "&",
            Token::VerticalLine => "|",
            Token::MoreThan => ">",
            Token::LessThan => "<",
            Token::Dot => ".",
            Token::Comma => ",",
            Token::OpenParen => "(",
            Token::CloseParen => ")",
            _ => return Err(ErrorType::InvalidExpr)
        };
        result.push_str(pusheen);
    }

    Ok(result)
}

pub fn eval(expr: &str) -> Result<Value, (ErrorType, Option<String>)> {
    match evalexpr::eval(expr) {
        Ok(value) => {
            match value {
                evalexpr::Value::Int(int) => match i32::try_from(int) {
                    Ok(value) => Ok(Value::Int32(value, false)),
                    Err(_) => { Err((ErrorType::IntOverflow, Some("result of the expression caused integer overflow".to_string()))) }
                },
                evalexpr::Value::Float(fl) => {
                    let fl = fl as f32;
                    if fl.is_finite() {
                        Ok(Value::Float32(fl))
                    } else {
                        Err((ErrorType::FloatOverflow, Some("result of the expression caused float overflow".to_string())))
                    }
                },
                evalexpr::Value::Boolean(b) => {
                    Ok(Value::Int32(b as i32, false))
                }
                evalexpr::Value::Empty => {
                    Err((ErrorType::ExpectedExpr, None))
                }
                _ => panic!("uncovered type in match(eval(&expr)): {:?}", value)
            }
        }
        Err(_) => { Err((ErrorType::InvalidExpr, Some("when evaluating the expression".to_string()))) }
    }
}

pub fn slice_cmp(toks: &[Token], variables: Option<&HashMap<String, mem::Value>>) -> Result<bool, (ErrorType, Option<String>)> {
    let expr = match exprify(toks, variables) {
        Ok(expr) => expr,
        Err(error) => { return Err((error, None)) }
    };

    match eval(&expr) {
        Ok(value) => match value {
            mem::Value::Int32(int, _) => Ok(int > 0),
            mem::Value::Float32(float) => Ok(float > 0.),
            mem::Value::Str(_) => panic!("a wild mem::Value::Str(_) has appeared!")
        }
        Err((error, note)) => {
            Err((error, note))
        }
    }
}

pub fn get_args(toks: &[Token]) -> Vec<&[Token]> {
    let mut result: Vec<&[Token]> = Vec::new();

    for sl in toks.split(|tok| *tok == Token::Comma) {
        result.push(sl);
    }

    result
}

/*
    escape sequences:

    case 'a': str[ w ] = 0x07; break; // alert
    case 'b': str[ w ] = 0x08; break; // backspace
    case 't': str[ w ] = 0x09; break; // tab
    case 'r': str[ w ] = 0x0D; break; // carriage-return
    case 'n': str[ w ] = 0x0A; break; // newline
    case 'v': str[ w ] = 0x0B; break; // vertical-tab
    case 'f': str[ w ] = 0x0C; break; // form-feed
    case 'e': str[ w ] = 0x1B; break; // escape character
*/

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum MatchEndingsError {
    TokenizeError(usize, TokenizeError),
    UnmatchedEnd(usize),
    UnterminatedBlock(usize),
}

pub fn match_endings(lines: &Vec<&str>) -> Result<HashMap<usize, usize>, MatchEndingsError> {
    let mut start: Vec<usize> = Vec::new();
    let mut end: Vec<usize> = Vec::new();

    for (at, line) in lines.iter().enumerate() {
        let tokens = match tokenize(line) {
            Ok(tokens) => tokens,
            Err(err) => {
                return Err(MatchEndingsError::TokenizeError(at, err))
            }
        };

        if let Some(Token::Identifier(ident)) = tokens.first() {
            match ident.as_str() {
                "if" | "while" => start.push(at),
                "end" => end.push(at),
                _ => {}
            }
        }
    }

    if start.len() > end.len() {
        Err(MatchEndingsError::UnterminatedBlock(*start.last().unwrap()))
    } else if end.len() > start.len() {
        Err(MatchEndingsError::UnmatchedEnd(*end.last().unwrap()))
    } else {
        let mut endings: HashMap<usize, usize> = HashMap::new();
        let iter = start.iter().zip(end.iter());
        for (&start, &end) in iter {
            endings.insert(start, end);
        }
        Ok(endings)
    }
}

pub fn find_deepest_call(toks: &[Token]) -> Option<usize> {
    let (mut deepest, mut depth, mut max_depth, mut found) = (0_usize, 0_usize, 0_usize, false);
    for (at, token) in toks.iter().enumerate() {
        if at < 1 { continue; }

        if let Token::Identifier(_) = toks[at-1] {
            match *token {
                Token::OpenParen => depth = depth.checked_add(1)?,
                Token::CloseParen => depth = depth.checked_sub(1)?,
                _ => {}
            }
            if depth > max_depth {
                max_depth = depth;
                deepest = at;
                found = true;
            }
        }
    }

    if found {
        Some(deepest.checked_add(1)?)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exprify_simple() {
        let toks = [
            Token::Integer(2),
            Token::Add,
            Token::Integer(3),
        ];
        let expected = "2+3";

        assert_eq!(exprify(&toks, None).unwrap(), expected)
    }

    #[test]
    fn exprify_variables() {
        let mut variables = HashMap::new();
        variables.insert(String::from("var1"), Value::Int32(2, false));
        variables.insert(String::from("var2"), Value::Int32(3, false));

        let toks = [
            Token::Identifier(String::from("var1")),
            Token::Add,
            Token::Identifier(String::from("var2")),
        ];
        let expected = "2+3";

        assert_eq!(exprify(&toks, Some(&variables)).unwrap(), expected)
    }

    #[test]
    pub fn exprify_bool() {
        let toks = [
            Token::Identifier(String::from("true")),
            Token::Assign, // ==
            Token::Assign,
            Token::Identifier(String::from("true")),
        ];
        let expected = "1==1";

        assert_eq!(exprify(&toks, None).unwrap(), expected)
    }

    #[test]
    fn exprify_ptr_partial() {
        let mut variables = HashMap::new();
        variables.insert(String::from("var1"), Value::Int32(2, false));
        variables.insert(String::from("var2"), Value::Int32(3, true));

        let toks = [
            Token::Identifier(String::from("var1")),
            Token::Add,
            Token::Identifier(String::from("var2")),
        ];

        assert_eq!(exprify(&toks, Some(&variables)), Err(ErrorType::UnexpectedPointer(String::from("var2"))))
    }

    #[test]
    fn exprify_ptr() {
        let mut variables = HashMap::new();
        variables.insert(String::from("var1"), Value::Int32(2, true));
        variables.insert(String::from("var2"), Value::Int32(3, true));

        let toks = [
            Token::Identifier(String::from("var1")),
            Token::Add,
            Token::Identifier(String::from("var2")),
        ];

        assert_eq!(exprify(&toks, Some(&variables)), Err(ErrorType::UnexpectedPointer(String::from("var1"))))
    }

    #[test]
    fn find_call() {
        let input = vec![
            Token::Integer(1),
            Token::Add,
            Token::Identifier(String::from("fn_call")),
            Token::OpenParen,
            Token::CloseParen,
        ];
        let result = find_deepest_call(&input);
        let expected: Option<usize> = Some(4);
        assert_eq!(result, expected);
    }

    #[test]
    fn find_call_empty() {
        let input = vec![
            Token::Integer(1),
            Token::Add,
            Token::Identifier(String::from("not_an_fn_call")),
        ];
        let result = find_deepest_call(&input);
        let expected: Option<usize> = None;
        assert_eq!(result, expected);
    }

    #[test]
    fn find_call_deep() {
        let input = vec![
            Token::Integer(1),
            Token::Add,
            Token::Identifier(String::from("fn_call")),
            Token::OpenParen,
                Token::Identifier(String::from("deeper_fn_call")),
                Token::OpenParen,
                Token::CloseParen,
            Token::CloseParen,
        ];
        let result = find_deepest_call(&input);
        let expected: Option<usize> = Some(6);
        assert_eq!(result, expected);
    }

    #[test]
    fn find_call_deeper_args() {
        let input = vec![
            Token::Integer(1),
            Token::Add,
            Token::Identifier(String::from("fn_call")),
            Token::OpenParen,
                Token::Identifier(String::from("deeper_fn_call")),
                Token::OpenParen,
                    Token::Identifier(String::from("more_deeper_fn_call")),
                    Token::OpenParen,
                        // arguments:
                        Token::Integer(2),
                        Token::Add,
                        Token::Integer(2),
                    Token::CloseParen,
                Token::CloseParen,
            Token::CloseParen,
        ];
        let result = find_deepest_call(&input);
        let expected: Option<usize> = Some(8);
        assert_eq!(result, expected);
    }
}