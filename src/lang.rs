use std::collections::HashMap;

use crate::{consts, mem::{FnMap, Function, Value, VarMap}, shell::*};

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
    OpenCurly, // {
    CloseCurly, // }
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
            '{' => { tokens.push(Token::OpenCurly); next!(); }
            '}' => { tokens.push(Token::CloseCurly); next!(); }
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

pub fn exprify(toks: &[Token], variables: Option<&VarMap>) -> Result<String, ErrorType> {
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
                        Value::Str(_) => return Err(ErrorType::StringInExpr),
                        Value::Null => return Err(ErrorType::NullValue),
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
            Token::ExclamationMark => "!",
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

pub fn compute(toks: &[Token], variables: Option<&VarMap>) -> Result<Value, (ErrorType, Option<String>)> {
    let expr = match exprify(toks, variables) {
        Ok(expr) => expr,
        Err(error) => return Err((error, Some(String::from("when parsing the expression"))))
    };

    eval(&expr)
}

pub fn slice_cmp(toks: &[Token], variables: Option<&HashMap<String, Value>>) -> Result<bool, (ErrorType, Option<String>)> {
    let expr = match exprify(toks, variables) {
        Ok(expr) => expr,
        Err(error) => { return Err((error, None)) }
    };

    match eval(&expr) {
        Ok(value) => match value {
            Value::Int32(int, _) => Ok(int > 0),
            Value::Float32(float) => Ok(float > 0.),
            Value::Str(_) | Value::Null => panic!()
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

fn fn_decl_remover(source: &[&str]) -> Option<Vec<String>> {
    let mut src: Vec<String> = Vec::new();
    let mut fn_def: usize = 0;
    let mut ok = true;

    for (at, line) in source.iter().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, (at, line), $w, $n);
                if $rt == ReportType::Error {
                    ok = false;
                }
                continue;
            };
        }
        
        match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() {
                if let Token::Identifier(ident) = &tokens[0] {
                    match ident.as_str() {
                        "fn" => fn_def += 1,
                        "if" | "while" => if fn_def > 0 { fn_def += 1 },
                        "end" => if fn_def > 0 { fn_def = fn_def.saturating_sub(1); continue; },
                        _ => {}
                    }
                }
                
                if fn_def == 0 {
                    src.push(line.to_string());
                }
            } else {
                continue;
            }
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(ch) => { report!(ReportType::Error, ErrorType::InvalidChar(ch), None); },
                    TokenizeError::IntegerOverflow => { report!(ReportType::Error, ErrorType::IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")); },
                    TokenizeError::FloatParseError => { report!(ReportType::Error, ErrorType::FloatOverflow, None); },
                }
            }
        }
    }

    if ok { Some(src) } else { None }
}

fn else_unwrapper(source: Vec<String>) -> Option<Vec<String>> {
    let mut src: Vec<String> = Vec::new();
    let mut ctls: Vec<(bool, &str)> = Vec::new();
    let mut ok = true;

    for (at, line) in source.iter().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, (at, line), $w, $n);
                if $rt == ReportType::Error {
                    ok = false;
                }
                continue;
            };
        }

        let tokens = match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tokens } else { continue; },
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(char) => { report!(ReportType::Error, ErrorType::InvalidChar(char), None); },
                    TokenizeError::IntegerOverflow => { report!(ReportType::Error, ErrorType::IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")); },
                    TokenizeError::FloatParseError => { report!(ReportType::Error, ErrorType::FloatOverflow, None); },
                }
            }
        };

        let fword = if let Token::Identifier(ident) = &tokens[0] {
            ident
        } else {
            continue;
        };

        match fword.as_str() {
            "if" => {
                if line.chars().nth(3).is_some() {
                    ctls.push((true, line));
                } else {
                    report(ReportType::Error, (at, line), ErrorType::ExpectedExpr, None);
                    break;
                }
            },
            "while" => ctls.push((false, line)),
            "else" => {
                let (else_allowed, line) = if let Some(boo) = ctls.pop() {
                    (boo.0, boo.1)
                } else {
                    report!(ReportType::Error, ErrorType::UnmatchedElse, None);
                };

                if else_allowed {
                    src.push(String::from("end"));

                    let char = line.chars().nth(2).expect("`if` without an expression should be filtered");
                    let startpoint = if char == ' ' { 3.. } else { 2.. };
                    src.push(format!("if!({})", &line[startpoint]));

                    continue;
                } else {
                    report!(ReportType::Error, ErrorType::ElseForLoop, None);
                }
            }
            _ => {}
        }
        
        src.push(line.to_string());
    }

    if ok { Some(src) } else { None }
}

fn get_functions(source: &[&str]) -> Option<FnMap> {
    let mut fns: FnMap = HashMap::new();
    let mut receiver: Option<String>;
    let mut func: Function;

    let mut depth: usize = 0;
    
    macro_rules! collector_init {
        () => {
            receiver = None;
            func = Default::default();
        };
    }
    collector_init!();

    let mut ok = true;

    for (at, line) in source.iter().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, (at, line), $w, $n);
                if $rt == ReportType::Error {
                    ok = false;
                }
                continue;
            };
        }
        
        let tokens = match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tokens } else { continue; },
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(ch) => { report!(ReportType::Error, ErrorType::InvalidChar(ch), None); },
                    TokenizeError::IntegerOverflow => { report!(ReportType::Error, ErrorType::IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")); },
                    TokenizeError::FloatParseError => { report!(ReportType::Error, ErrorType::FloatOverflow, None); },
                }
            }
        };

        if let Some(ref name) = receiver {
            if let Token::Identifier(ident) = &tokens[0] {
                match ident.as_str() {
                    "if" | "while" => {
                        if depth < consts::MAX_SCOPES {
                            depth += 1;
                        } else {
                            report!(ReportType::Error, ErrorType::TooDeepControl(consts::MAX_CONTROL_DEPTH), None);
                        }
                    }
                    "end" => {
                        depth -= 1;
                        if depth == 0 {
                            fns.insert(name.to_string(), func);
                            collector_init!();
                            continue;
                        }
                    }
                    _ => {}
                }
            }
            func.body.push(line.to_string());
        } else if tokens[0] == Token::Identifier(String::from("fn")) {
            if let Some(Token::Identifier(ident)) = tokens.get(1) {
                if !fns.contains_key(ident) {
                    if ident.len() <= consts::MAX_IDENT_LENGTH {
                        func.at = at+1;
                        /* if let Some(tokens) = tokens.get(2..) {
                            for token in tokens {
                                if let Token::Identifier(arg) = token {
                                    func.args.insert(arg.to_string());
                                } else {
                                    report!(ReportType::Error, ErrorType::ExpectedIdent, None);
                                }
                            }
                        } */
                        receiver = Some(ident.to_string());
                        depth = 1;
                    } else {
                        report!(ReportType::Error, ErrorType::TooLongIdent(consts::MAX_IDENT_LENGTH), None);
                    }
                } else {
                    report!(ReportType::Error, ErrorType::FnRedef(ident.to_string()), None);
                }
            } else {
                report!(ReportType::Error, ErrorType::ExpectedIdent, None);
            }
        }
    }
    
    if ok {
        Some(fns)
    } else {
        None
    }
}

fn check_intrinsics_use(source: &[String]) -> bool {
    let mut bad = false;

    for (at, line) in source.iter().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, (at, line), $w, $n);
                if $rt == ReportType::Error {
                    bad = true;
                }
                continue;
            };
        }
        
        let tokens = match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tokens } else { continue; },
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(ch) => { report!(ReportType::Error, ErrorType::InvalidChar(ch), None); },
                    TokenizeError::IntegerOverflow => { report!(ReportType::Error, ErrorType::IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")); },
                    TokenizeError::FloatParseError => { report!(ReportType::Error, ErrorType::FloatOverflow, None); },
                }
            }
        };

        if tokens[0] == Token::Dot {
            if let Some(Token::Identifier(ident)) = tokens.get(1) {
                report!(ReportType::Error, ErrorType::IntrinsicUse(ident.to_string()), None);
            } else {
                report!(ReportType::Error, ErrorType::ExpectedIdent, None);
            }
        }
    }

    bad
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub src: Vec<String>,
    pub fns: FnMap,
    pub ends: BranchEndings,
}

impl Class {
    pub fn make(lines: &[&str]) -> Option<Class> {
        let funcs = get_functions(lines)?;
        let source = else_unwrapper(fn_decl_remover(lines)?)?;

        if check_intrinsics_use(&source) { return None }

        let endings = match match_endings(&source) {
            Ok(map) => map,
            Err((error, line)) => {
                use {
                    ReportType::*,
                    ErrorType::*
                };
                match error {
                    MatchEndingsError::TokenizeError(at, error) => match error {
                        TokenizeError::InvalidCharacter(ch) => report(Error, (at, line), InvalidChar(ch), None),
                        TokenizeError::IntegerOverflow => report(Error, (at, line), IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")),
                        TokenizeError::FloatParseError => report(Error, (at, line), FloatOverflow, Some("this line has an overflowing floating point number (expected signed 32-bit)")),
                    },
                    MatchEndingsError::UnterminatedBlock(at) => report(Error, (at, line), UnterminatedBlock, None),
                    MatchEndingsError::UnmatchedEnd(at) => report(Error, (at, line), UnmatchedEnd, None),
                }
                return None;
            }
        };

        Some(Class {
            src: source,
            fns: funcs,
            ends: endings,
        })
    }
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

type BranchEndings = HashMap<usize, usize>;

pub fn match_endings(lines: &[String]) -> Result<BranchEndings, (MatchEndingsError, &str)> {
    let mut start: Vec<usize> = Vec::new();
    let mut end: Vec<usize> = Vec::new();

    for (at, line) in lines.iter().enumerate() {
        let tokens = match tokenize(line) {
            Ok(tokens) => tokens,
            Err(err) => {
                return Err((MatchEndingsError::TokenizeError(at, err), line))
            }
        };

        if let Some(Token::Identifier(ident)) = tokens.first() {
            match ident.as_str() {
                "if" | "while" | "fn" => start.push(at),
                "end" => end.push(at),
                _ => {}
            }
        }
    }

    if start.len() > end.len() {
        let point = *start.last().unwrap();
        Err((MatchEndingsError::UnterminatedBlock(point), &lines[point]))
    } else if end.len() > start.len() {
        let point = *end.last().unwrap();
        Err((MatchEndingsError::UnmatchedEnd(point), &lines[point]))
    } else {
        let mut endings: HashMap<usize, usize> = HashMap::new();
        let iter = start.iter().zip(end.iter());
        for (&start, &end) in iter {
            endings.insert(start, end);
        }
        Ok(endings)
    }
}

/* pub fn process_fns(toks: &[Token], variables: Option<&VarMap>) -> Result<Vec<Token>, ErrorType> {
    while let Some(fn_at) = find_deepest_call(toks) {
        let ident = if let Token::Identifier(ident) = &toks[fn_at] { ident } else { panic!() };
        
        let body: Vec<Token> = {
            let mut body = Vec::new();

            if let Some(tokens) = toks.get((fn_at + 2)..) {
                let mut paren_depth: u32 = 1;
                for token in tokens {
                    match *token {
                        Token::OpenParen => paren_depth += 1,
                        Token::CloseParen => {
                            paren_depth -= 1;
                            if paren_depth == 0 { break }
                        }
                        _ => body.push(token.clone()),
                    }
                }
            }

            body
        };

        let args = {
            let args_ = get_args(&body);
            let mut args: Vec<Value> = Vec::new();

            for arg in args_ {
                match eval(&exprify(arg, variables)?) {
                    Ok(value) => args.push(value),
                    Err((error, _)) => return Err(error)
                }
            }

            args
        };
    }
    Ok(toks.to_vec())
} */

pub fn find_deepest_call(toks: &[Token]) -> Option<usize> {
    let (mut where_deepest, mut depth, mut max_depth, mut found) = (0_usize, 0_usize, 0_usize, false);
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
                where_deepest = at;
                found = true;
            }
        }
    }

    if found {
        Some(where_deepest.checked_sub(1)?)
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
        let expected: Option<usize> = Some(2);
        assert_eq!(result, expected);
    }

    #[test]
    fn find_call_empty() {
        let input = vec![
            Token::Integer(1),
            Token::Add,
            Token::OpenParen,
            Token::Identifier(String::from("not_an_fn_call")),
            Token::CloseParen,
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
        let expected: Option<usize> = Some(4);
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
        let expected: Option<usize> = Some(6);
        assert_eq!(result, expected);
    }

    #[test]
    fn get_functions() {
        let code = [
            // empty:
            "fn func_1",
            "end",

            // with body:
            "fn func_2",
            "    foo",
            "ebd",

            // with args:
            "fn func_3 ment argu",
            "    foo",
            "end",

            // with controls inside:
            "fn func_4 ment argu",
            "    if true",
            "        while false",
            "            foo",
            "        end",
            "    end",
            "end",
        ];

        let expected_names = vec![
            String::from("func_1"),
            String::from("func_2"),
            String::from("func_3"),
            String::from("func_4"),
        ];

        let fns = Class::make(&code).unwrap().fns;
        println!("{:#?}", fns);

        let mut names = fns.into_keys().collect::<Vec<String>>();
        names.sort();
        
        assert_eq!(expected_names, names);
        //println!("{:#?}", super::get_functions(&code));
    }

    #[test]
    fn get_functions_bad() {
        let code = [
            // note: tokenization errors are not covered here

            // no identifier:
            "fn",      // 1
            "end",

            // invalid fn's identifier:
            "fn 123",  // 3
            "end",

            // weird fn's arguments:
            "fn bad_args Rust++",  // 5
            "end",

            // redefinition:
            "fn func",
            "end",
            "fn func", // 9
            "end",
        ];
        assert_eq!(Class::make(&code), None);
    }

    #[test]
    fn else_unwrap() {
        macro_rules! test {
            ($expr:expr) => {
                let source: Vec<String> = vec![
                    concat!("if", $expr).to_string(),
                    "else".to_string(),
                    //"end".to_string(),
                ];

                let unwrapped = else_unwrapper(source).unwrap();
                println!("{:#?}", unwrapped);
            };
        }

        test!(" 1 == 1");
        test!("!(1)");
    }

    #[test]
    fn using_intrinsics() {
        let intris = [
            String::from(".i_am_an_intrinsic"),
        ];

        let no_intris = [
            String::from("i_am_not_an_intrinsic"),
        ];

        assert!(check_intrinsics_use(&intris));
        assert!(!check_intrinsics_use(&no_intris));
    }
}