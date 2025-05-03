use std::collections::HashMap;

use crate::{mem::{self, Value}, shell::ErrorType};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenizeError {
    InvalidCharacter(char),
    IntegerOverflow /* (usize) */
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    Integer(i32),
    Float(f32),

    Assign, // =
    DollarSign, // $
    Add,  // +
    Substract, // -
    Multiply, // *
    Divide, // /
    PowerOf, // ^
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
            '^' => { tokens.push(Token::PowerOf); next!(); }
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
                tokens.push(Token::Identifier(ident));
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

    Ok(tokens)
}

pub fn exprify(toks: &[Token], variables: Option<&HashMap<String, mem::Variable>>) -> Result<String, ErrorType> {
    // 1. replace variables with its values
    let tokens = if let Some(variables) = variables {
        let mut tokens = toks.to_vec();
        for (at, token) in toks.iter().enumerate() {
            if let Token::Identifier(id) = token {
                if let Some(var) = variables.get(id) {
                    match var.value {
                        Value::Int32(int) => tokens[at] = Token::Integer(int),
                        Value::Float32(int) => tokens[at] = Token::Float(int),
                        Value::Str(_) => return Err(ErrorType::StringInExpr)
                    }
                } else {
                    return Err(ErrorType::UndefinedVariable(id.to_string()))
                }
            }
        }
        println!("{:#?}", tokens);
        tokens
    } else {
        toks.to_vec()
    };

    // 2. stringify
    let mut result = String::new();
    for token in tokens {
        //result.push(' ');
        let pusheen = match token {
            Token::Integer(int) => &int.to_string(),
            Token::Float(fl) => &fl.to_string(),
            Token::Add => "+",
            Token::Substract => "-",
            Token::Multiply => "*",
            Token::Divide => "/",
            Token::PowerOf => "^",
            Token::Remainder => "%",
            Token::Ampersand => "&",
            Token::VerticalLine => "|",
            _ => return Err(ErrorType::InvalidExpr)
        };
        result.push_str(pusheen);
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn exprify_simple() {
        let toks = [
            Token::Integer(2),
            Token::Add,
            Token::Integer(3),
        ];
        let expected = "2+3";

        assert_eq!(exprify(&toks, None).unwrap(), expected)
    }

    #[test]
    pub fn exprify_variables() {
        let mut variables = HashMap::new();
        variables.insert(String::from("var1"), Value::Int32(2));
        variables.insert(String::from("var2"), Value::Int32(3));

        let toks = [
            Token::Identifier(String::from("var1")),
            Token::Add,
            Token::Identifier(String::from("var2")),
        ];
        let expected = "2+3";

        assert_eq!(exprify(&toks, None).unwrap(), expected)
    }
}