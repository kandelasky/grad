use std::collections::{HashMap, HashSet};
use colored::Colorize;

use crate::{
    consts, lang::{self, *}, mem::{self, ValueType}, shell::{ErrorType::*, ReportType::*, *}, time::sleep
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct RunConfig {
    debug_line: bool,
    debug_mem: bool,
}

#[derive(Clone, Debug)]
pub struct Routine<'a> {
    pub body: &'a TokensGroup,
    pub at: usize,

}

impl<'a> Routine<'a> {
    pub fn new(body: &'a TokensGroup) -> Self {
        Self {
            body,
            at: 0,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum RoutineOp<'a> {
    Add(&'a TokensGroup),
    Pop,
}

pub fn exec(source: String) {
    let config = RunConfig {
        debug_line: false,
        debug_mem:  false,
    };

    let (source, functions, endings) = {
        let src: Vec<&str> = source.lines().collect();
        if let Some(class) = Class::make(&src) {
            drop(source);
            (class.src, class.fns, class.ends)
        } else {
            return
        }
    };

    let mut tks: TokensGroup = Vec::new();
    for (at, line) in source.iter().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, (at, line), $w, $n);
                return;
            };
        }
        
        match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tks.push(tokens); } else { continue; },
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(ch) => { report!(Error, InvalidChar(ch), None); },
                    TokenizeError::IntegerOverflow => { report!(Error, IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")); },
                    TokenizeError::FloatParseError => { report!(Error, FloatOverflow, None); },
                }
            }
        };
    }

    let mut routs: Vec<Routine> = vec![Routine::new(&tks)];
    let mut routs_len: usize = 1;
    let mut routs_op: Option<RoutineOp<'_>> = None;

    let mut variables: mem::VarMap = HashMap::new();
    
    let constants = HashMap::from([
        ( String::from("INT_MAX"), mem::Value::Int32(i32::MAX, false) ),
    ]);
    
    for (ident, value) in &constants {
        variables.insert(ident.to_string(), value.to_owned());
    }

    let constants: Vec<String> = constants.into_keys().collect();

    let mut controls: Vec<(Vec<Token> /* comparator */, bool /* do? */, bool /* loop? */, usize /* origin */)> = Vec::new();

    let mut scopes: Vec<HashSet<String>> = vec![HashSet::new()];
    
    while let Some(route) = routs.last_mut() {
        macro_rules! scope_cleanup {
            () => {
                let cleanup = scopes.get(controls.len()).unwrap();

                if !cleanup.is_empty() {
                    if config.debug_mem {
                        let len = cleanup.len();
                        println!("scope_cleanup: {} {} {} variable{}", "auto".bold(), "deleting".bold().red(), len, if len > 1 { 's' } else { ' ' });
                    }

                    for id in cleanup {
                        variables.remove(id);
                    }
                }
            };
        }

        macro_rules! scope_new {
            () => {
                scopes.push(HashSet::new());
            };
        }

        if let Some(op) = routs_op {
            match op {
                RoutineOp::Add(body) => {
                    routs.push(Routine::new(body));
                    scope_new!();
                }
                RoutineOp::Pop => {
                    scope_cleanup!();
                    scopes.pop();
                    routs.pop();
                    routs_len -= 1;
                }
            }
            routs_op = None;
            continue;
        }

        let (lines, at) = (route.body, route.at);

        let tokens = if let Some(line) = lines.get(at) {
            line
        } else {
            routs_op = Some(RoutineOp::Pop);
            continue;
        };

        let line = &source[at];

        if config.debug_line {
            eprintln!("[{}]\t{}", at + 1, line);
        }

        macro_rules! next {
            () => {
                route.at += 1;
                continue;
            };
        }

        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, (at, line), $w, $n);
                next!();
            };
        }
        
        macro_rules! get_args {
            ($expected:expr) => {{
                let args = get_args(&tokens[1..]);
                
                if args.len() > $expected {
                    report!(Error, LeftoverArgs($expected), None);
                } else if args.len() < $expected {
                    report!(Error, NotEnoughArgs($expected), None);
                } else {
                    args
                }
            }};
        }
        
        let calling = {
            let first_token = &tokens[0];
            match first_token {
                Token::Identifier(string) => string,
                _ => {
                    report!(Error, ExpectedIdent, None);
                }
            }
        };
        
        match calling.as_str() {
            // == Controls ====================================================

            "if" | "while" => {
                let origin = at + 1;
                if origin < route.body.len() {
                    if let Some(toks) = tokens.get(1..) {
                        let mets = match lang::slice_cmp(toks, Some(&variables)) {
                            Ok(b) => b,
                            Err((error, note)) => {
                                report!(Error, error, note.as_deref());
                            }
                        };

                        if mets {
                            let (v, do_, is_loop) = if calling == "if" {
                                (
                                    if mets {
                                        vec![Token::Integer(1)]
                                    } else {
                                        vec![Token::Integer(0)]
                                    },
                                    mets,
                                    false
                                )
                            } else {
                                (toks.to_vec(), mets, true)
                            };

                            if controls.len() < consts::MAX_SCOPES {
                                controls.push((v, do_, is_loop, origin));
                                scope_new!();
                            } else {
                                report!(Error, TooDeepControl(consts::MAX_CONTROL_DEPTH), None);
                            }
                        } else {
                            let jmp = *endings.get(&at).unwrap_or_else(|| panic!("unable to find endpoint for the control block at line {at}"));
                            route.at = jmp;
                        }
                    } else {
                        report!(Error, ExpectedExpr, None);
                    };
                } else {
                    report!(Error, UnexpectedEOF, None);
                }
            }

            /* "repeat" => {
            } */

            "end" => {
                if let Some((condition, do_, is_loop, start)) = controls.last() {
                    let pop: bool;

                    if *is_loop && *do_ {
                        let mets = match lang::slice_cmp(condition, Some(&variables)) {
                            Ok(boo) => boo,
                            Err((error, note)) => {
                                report!(Error, error, note.as_deref());
                            }
                        };

                        if mets {
                            route.at = *start;
                            continue;
                        } else {
                            pop = true;
                        }
                    } else {
                        pop = true;
                    }

                    if pop {
                        let trunc_vars = scopes.get(controls.len()).unwrap();

                        if !trunc_vars.is_empty() {
                            if config.debug_mem {
                                let len = trunc_vars.len();
                                println!("\n{} {} {} variable{}", "auto".bold(), "deleting".bold().red(), len, if len > 1 { 's' } else { ' ' });
                            }
    
                            for id in trunc_vars {
                                variables.remove(id);
                            }
                        }

                        scopes.pop();
                        controls.pop();
                    }
                } else {
                    report!(Error, UnmatchedEnd, None);
                }
            }

            /* "ret" => {
            } */

            // == Built-in fns ================================================

            "println" | "print" => {
                let args = get_args(&tokens[1..]);

                let mut buffer = String::new();
                for fmt in &args {
                    let string = if fmt.len() == 1 {
                        match &fmt[0] {
                            Token::Identifier(ident) => {
                                if let Some(value) = variables.get(ident) {
                                    match &value {
                                        mem::Value::Int32(int, _) => int.to_string(),
                                        mem::Value::Float32(fl) => fl.to_string(),
                                        mem::Value::Str(string) => string.to_string(),
                                        mem::Value::Null => { report!(Error, NullValue, None); },
                                    }
                                } else {
                                    report!(Error, UndefinedVariable(ident.to_string()), None);
                                }
                            }
                            Token::Integer(int) => int.to_string(),
                            Token::Float(fl) => fl.to_string(),
                            Token::StringLiteral(string) => string.to_string(),
                            _ => { report!(Error, InvalidExpr, None); }
                        }
                    } else {
                        let value = match lang::compute(fmt, Some(&variables)) {
                            Ok(value) => value,
                            Err((error, note)) => { report!(Error, error, note.as_deref()); }
                        };

                        match value {
                            mem::Value::Int32(int, is_ptr) => if !is_ptr {
                                int.to_string()
                            } else {
                                panic!("unexpected pointer");
                            }
                            mem::Value::Float32(float) => float.to_string(),
                            mem::Value::Str(string) => string,
                            mem::Value::Null => panic!("unexpected Null"),
                        }
                    };

                    buffer.push_str(string.as_str());
                }

                if calling == "println" {
                    println!("{buffer}");
                } else if !args.is_empty() {
                    print!("{buffer}");
                } else {
                    report(Warning, (at, line), UselessPrint, None);
                }
            }

            "sleep" => {
                let args = get_args!(1);
                let expr = args[0];

                let time = if expr.len() == 1 {
                    match &expr[0] {
                        Token::Integer(int) => *int,
                        Token::Float(float) => *float as i32,
                        _ => { report!(Error, MismTypes(ValueType::Int32), None); }
                    }
                } else {
                    let value = match lang::compute(expr, Some(&variables)) {
                        Ok(value) => value,
                        Err((error, note)) => { report!(Error, error, note.as_deref()); }
                    };

                    match value {
                            mem::Value::Int32(int, is_ptr) => if !is_ptr {
                                int
                            } else {
                                panic!("unexpected pointer");
                            }
                            mem::Value::Float32(float) => float as i32,
                            mem::Value::Str(_) => { report!(Error, MismTypes(ValueType::Int32), None); }
                            mem::Value::Null => panic!("unexpected Null"),
                        }
                };

                sleep(time);
            }

            "assert_eq" => {
                let args = get_args!(2);

                let left_eval = {
                    let expr = match exprify(args[0], Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("when parsing the first expression")); }
                    };

                    match lang::eval(&expr) {
                        Ok(value) => value,
                        Err((error, note)) => { report!(Error, error, note.as_deref()); }
                    }
                };

                let right_eval = {
                    let expr = match exprify(args[1], Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("when parsing the second expression")); }
                    };

                    match lang::eval(&expr) {
                        Ok(value) => value,
                        Err((error, note)) => { report!(Error, error, note.as_deref()); }
                    }
                };

                if left_eval != right_eval {
                    eprintln!("\nline {}: {} {}\n  left: {:?}\n  right: {:?}", at+1, "assertion".bold(), "failed".bold().red(), left_eval, right_eval);
                    break;
                }
            }

            "assert_ex" => {
                if let Some(Token::Identifier(ident)) = tokens.get(1) {
                    if !variables.contains_key(ident) {
                        eprintln!("\nline {}: {} {}\n  variable {} doesn\'t exists", at+1, "assertion".bold(), "failed".bold().red(), ident.bold());
                    }
                } else {
                    report!(Error, ExpectedIdent, None);
                }
            }

            "dbg_vars" => {
                //                  |------------- 32 -------------|
                const LINE: &str = "--------------------------------\n";

                println!("\n{LINE}*ID\t\tVALUE");
                for (ident, value) in &variables {
                    let is_ptr: bool = if let mem::Value::Int32(_, is_ptr) = value {
                        *is_ptr
                    } else {
                        false
                    };

                    if !constants.contains(ident) {
                        println!("{}{}\t\t{:?}", if is_ptr { '*' } else { ' ' }, ident, value);
                    }
                }

                println!("{LINE}\n");
            }

            "del" => {
                if let Some(tokens) = tokens.get(1..) {
                    if !tokens.is_empty() {
                        for token in tokens {
                            if let Token::Identifier(ident) = token {
                                let vec = scopes.get_mut(controls.len()).unwrap();
                                if !vec.remove(ident) || variables.remove(ident).is_none() {
                                    report!(Error, UndefinedVariable(ident.to_string()), None);
                                }
                            } else {
                                report!(Error, ExpectedIdent, None);
                            }
                        }
                    } else {
                        report!(Error, ExpectedIdent, None);
                    }
                } else {
                    report!(Error, ExpectedIdent, None);
                }
            }

            // ================================================================

            _ => {
                if calling.len() > consts::MAX_IDENT_LENGTH {
                    report!(Error, TooLongIdent(consts::MAX_IDENT_LENGTH), None);
                }

                let var_exist = variables.contains_key(calling);
                let func = functions.get(calling);

                if let Some(func) = func {
                    if routs_len <= consts::MAX_CALL_DEPTH {
                        routs_op = Some(RoutineOp::Add(&func.body));
                        routs_len += 1;
                        scope_new!();
                    } else {
                        report!(Error, TooDeepCall(consts::MAX_CALL_DEPTH), None);
                    }
                } else {
                    if constants.contains(calling) {
                        report(Error, (at, line), ConstRedef(calling.to_string()), None);
                    }

                    let op = if let Some(tok) = tokens.get(1) {
                        let allowed = [
                            Token::Assign,
                            Token::Add,
                            Token::Substract,
                            Token::Multiply,
                            Token::Divide,
                            Token::Remainder,
                            Token::Exponent
                        ];

                        if !allowed.contains(tok) {
                            report!(Error, InvalidOp, None);
                        }

                        if tok != &Token::Assign && !var_exist {
                            report!(Error, UndefinedVariable(String::from(calling)), None);
                        }

                        tok
                    } else {
                        report!(Error, ExpectedOp, None);
                    };

                    let toks = if let Some(toks) = tokens.get(2..) {
                        toks
                    } else {
                        report!(Error, ExpectedExpr, None);
                    };

                    let value: mem::Value = {
                        if toks.len() == 1 && *op == Token::Assign {
                            match &toks[0] {
                                Token::Identifier(ident) => match ident.as_str() {
                                    "true" | "false" => mem::Value::Int32(if ident == "true" { 1 } else { 0 }, false),
                                    "null" => mem::Value::Null,
                                    _ => {
                                        if let Some(value) = variables.get(ident) {
                                            if let mem::Value::Int32(_, is_ptr) = value {
                                                if !*is_ptr {
                                                    // TODO: make something that does NOT clones value
                                                    value.clone()
                                                } else {
                                                    report!(Error, UnexpectedPointer(ident.to_string()), None);
                                                }
                                            } else {
                                                // TODO: make something that does NOT clones value
                                                value.clone()
                                            }
                                        } else {
                                            report!(Error, UndefinedVariable(ident.to_string()), None);
                                        }
                                    }
                                }
                                Token::Integer(int) => mem::Value::Int32(*int, false),
                                Token::Float(float) => mem::Value::Float32(*float),
                                Token::StringLiteral(string) => {
                                    mem::Value::Str(string.to_string())
                                },
                                _ => { report!(Error, InvalidExpr, None); }
                            }
                        } else {
                            let expr = match exprify(toks, Some(&variables)) {
                                Ok(expr) => expr,
                                Err(error) => { report!(Error, error, Some("when parsing the expression")); }
                            };

                            match evalexpr::eval(&expr) {
                                Ok(value) => {
                                    let evaluaion = match value {
                                        evalexpr::Value::Int(int) => match i32::try_from(int) {
                                            Ok(value) => mem::Value::Int32(value, false),
                                            Err(_) => { report!(Error, IntOverflow, Some("result of the expression caused integer overflow")); }
                                        },
                                        evalexpr::Value::Float(fl) => {
                                            let fl = fl as f32;
                                            if fl.is_finite() {
                                                mem::Value::Float32(fl)
                                            } else {
                                                report!(Error, FloatOverflow, Some("result of the expression caused float overflow"));
                                            }
                                        },
                                        evalexpr::Value::Empty => {
                                            report!(Error, ExpectedExpr, None);
                                        }
                                        _ => panic!("uncovered type in match(eval(&expr)): {:?}", value)
                                    };
            
                                    if op == &Token::Assign {
                                        evaluaion
                                    } else {
                                        let og_value = &variables.get(calling).unwrap();
            
                                        match og_value {
                                            mem::Value::Int32(og_int, _) => {
                                                if let mem::Value::Int32(ev, _) = evaluaion {
                                                    let check = match op {
                                                        Token::Add => og_int.checked_add(ev),
                                                        Token::Substract => og_int.checked_sub(ev),
                                                        Token::Multiply => og_int.checked_mul(ev),
                                                        Token::Divide => og_int.checked_div(ev),
                                                        Token::Remainder => og_int.checked_rem(ev),
                                                        Token::Exponent => {
                                                            let expon: u32 = if let Ok(v) = u32::try_from(ev) {
                                                                v
                                                            } else {
                                                                report!(Error, IntOverflow, Some("when tried to compute the exponent"));
                                                            };
                                                            og_int.checked_pow(expon)
                                                        },
                                                        _ => panic!()
                                                    };
                                                    if let Some(result) = check {
                                                        mem::Value::Int32(result, false)
                                                    } else {
                                                        report!(Error, IntOverflow, Some("when doing compound assignment"));
                                                    }
                                                } else {
                                                    report!(Error, MismTypes(ValueType::Int32), None);
                                                }
                                            }
                                            mem::Value::Float32(og_float) => {
                                                if let mem::Value::Float32(ev) = evaluaion {
                                                    let result = match op {
                                                        Token::Add => og_float + ev,
                                                        Token::Substract => og_float - ev,
                                                        Token::Multiply => og_float * ev,
                                                        Token::Divide => og_float / ev,
                                                        Token::Remainder => og_float % ev,
                                                        Token::Exponent => { report!(Error, FloatExp, Some("when doing compound assignment")); },
                                                        _ => panic!()
                                                    };
                                                    if result.is_finite() {
                                                        mem::Value::Float32(result)
                                                    } else {
                                                        report!(Error, FloatOverflow, Some("when doing compound assignment"));
                                                    }
                                                } else {
                                                    report!(Error, MismTypes(ValueType::Float32), None);
                                                }
                                            }
                                            mem::Value::Null => { report!(Error, NullValue, Some(format!("in variable {}", calling.bold()).as_str())); }
                                            _ => panic!()
                                        }
                                    }
                                }
                                Err(_) => { report!(Error, InvalidExpr, Some("when evaluating the expression")); }
                            } // match evalexpr::eval(&expr)
                        } // if toks.len() == 1
                    }; // let value

                    variables.insert(calling.to_string(), value);

                    let defined_outer_scope: bool = {
                        let mut result: bool = false;
                        'g: for dive in 0..controls.len() {
                            for ident in scopes.get(dive).unwrap() {
                                if ident == calling {
                                    result = true;
                                    break 'g;
                                }
                            }
                        }
                        result
                    };

                    if !defined_outer_scope {
                        let set = scopes.get_mut(controls.len()).unwrap();
                        set.insert(calling.to_string());
                    }
                } // if let Some(func) = func
            } // _
        } // match calling.as_str()
        next!();
    } // routs.last_mut()
}