use std::collections::HashMap;

use colored::Colorize;

use crate::{
    lang::{self, *},
    mem,
    shell::{ErrorType::*, ReportType::*, *}
};

const DEBUG: bool = false;

pub fn exec(source: String) {
    let lines: Vec<&str> = source.lines().collect();
    let lines_max = lines.len();

    let mut variables: HashMap<String, mem::Variable> = HashMap::new();

    // constants:
    let constants = HashMap::from([
        (String::from("INT_MAX"), (mem::Value::Int32(i32::MAX), false)),
    ]);

    for (ident, (value, is_pointer)) in &constants {
        variables.insert(ident.to_string(), mem::Variable::new(value.clone(), *is_pointer));
    }

    let mut controls: Vec<(Vec<Token>, bool /* infinite? */, usize /* start line (after the control) */)> = Vec::new();
    
    let mut at: usize = 0;
    'l: while at < lines_max {
        let line = lines[at];

        if DEBUG {
            eprintln!("[{}]\t{}", at+1, line);
        }

        macro_rules! next {
            () => {
                at += 1;
                continue 'l;
            };
        }

        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, at, $w, $n);
                next!();
            };
        }
        
        let tokens = match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tokens } else { next!(); },
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(ch) => { report!(Error, InvalidChar(ch), None); },
                    TokenizeError::IntegerOverflow => { report!(Error, IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")); },
                }
            }
        };
        
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

        if let Some((condition, _, _)) = controls.last() {
            let mets = match lang::slice_cmp(condition, Some(&variables)) {
                Ok(b) => b,
                Err((error, note)) => {
                    report!(Error, error, note.as_deref());
                }
            };

            if !mets && calling != "end" {
                next!();
            }
        }
        
        match calling.as_str() {
            // == Controls ====================================================
            "if" => {
                let origin = at + 1;
                if origin < lines_max {
                    if let Some(toks) = tokens.get(1..) {
                        let mets = match lang::slice_cmp(toks, Some(&variables)) {
                            Ok(b) => b,
                            Err((error, note)) => {
                                report!(Error, error, note.as_deref());
                            }
                        };

                        let v = if mets {
                            vec![Token::Integer(1)]
                        } else {
                            vec![Token::Integer(0)]
                        };

                        controls.push((v, false, origin));
                    } else {
                        report!(Error, ExpectedExpr, None);
                    };
                } else {
                    report!(Error, UnexpectedEOF, None);
                }
            }
            "end" => {
                if let Some((condition, is_loop, start)) = controls.pop() {
                    if is_loop {
                        let mets = match lang::slice_cmp(&condition, Some(&variables)) {
                            Ok(b) => b,
                            Err((error, note)) => {
                                report!(Error, error, note.as_deref());
                            }
                        };

                        if mets {
                            at = start
                        }
                    }
                } else {
                    report!(Error, UnmatchedEnd, None);
                }
            }
            // == Built-in fns ================================================
            "println" | "print" => {
                let args = get_args(&tokens[1..]);
                for arg in &args {
                    if arg.len() == 1 {
                        match &arg[0] {
                            Token::Identifier(ident) => {
                                if let Some(var) = variables.get(ident) {
                                    match &var.value {
                                        mem::Value::Int32(int) => print!("{int}"),
                                        mem::Value::Float32(fl) => print!("{fl}"),
                                        mem::Value::Str(string) => print!("{string}"),
                                    }
                                } else {
                                    report!(Error, UndefinedVariable(ident.to_string()), None);
                                }
                            }
                            Token::StringLiteral(string) => print!("{string}"),
                            Token::Integer(int) => print!("{int}"),
                            Token::Float(fl) => print!("{fl}"),
                            _ => { report!(Error, InvalidExpr, None); }
                        }
                    }
                }
                if calling == "println" {
                    println!();
                } else if args.is_empty() {
                    report(Warning, at, UselessPrint, None);
                }
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
                    break 'l
                }
            }
            "dbg_vars" => {
                //                  |------------- 32 -------------|
                const LINE: &str = "--------------------------------\n";

                println!("\n{LINE}*ID\t\tVALUE");
                for (ident, var) in &variables {
                    if !constants.contains_key(ident) {
                        println!("{}{}\t\t{:?}", if var.is_pointer { '*' } else { ' ' }, ident, var.value);
                    }
                }
                println!("{LINE}\n");
            }
            _ => {
                const MAX_IDENT_LENGTH: usize = 32;
                if calling.len() > MAX_IDENT_LENGTH {
                    report!(Error, TooLongIdent(MAX_IDENT_LENGTH), None);
                }

                let var_exist = variables.contains_key(calling);

                if false /* fn_exist */ {
                    // call fn
                } else {
                    if constants.contains_key(calling) {
                        report(Warning, at, ConstRedefinition(calling.to_string()), None);
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

                    if toks.len() == 1 {
                        match &toks[0] {
                            Token::Identifier(ident) => match ident.as_str() {
                                "true" | "false" => {
                                    let value = mem::Value::Int32( if ident == "true" { 1 } else { 0 } );
                                    variables.insert(calling.to_string(), mem::Variable::new(value, false));
                                    next!();
                                },
                                _ => {}
                            }
                            Token::Null => {
                                // <- TODO: if this is pointer - remove container
                                variables.remove(calling);
                                next!();
                            }
                            /* Token::Integer(int) => {
                                variables.insert(calling.to_string(), mem::Variable::new(mem::Value::Int32(*int), false));
                                next!();
                            },
                            Token::Float(fl) => {
                                variables.insert(calling.to_string(), mem::Variable::new(mem::Value::Float32(*fl), false));
                                next!();
                            }, */
                            Token::StringLiteral(string) => {
                                variables.insert(calling.to_string(), mem::Variable::new(mem::Value::Str(string.to_string()), false));
                                next!();
                            },
                            _ => { /* report!(Error, InvalidExpr, None); */ }
                        }
                    }

                    let expr = match exprify(toks, Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("when parsing the expression")); }
                    };

                    match evalexpr::eval(&expr) {
                        Ok(value) => {
                            let evaluaion = match value {
                                evalexpr::Value::Int(int) => match i32::try_from(int) {
                                    Ok(value) => mem::Value::Int32(value),
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
    
                            let v = if op == &Token::Assign {
                                evaluaion
                            } else {
                                let og_value = &variables.get_mut(calling).unwrap().value;
    
                                match og_value {
                                    mem::Value::Int32(og_int) => {
                                        if let mem::Value::Int32(ev) = evaluaion {
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
                                                mem::Value::Int32(result)
                                            } else {
                                                report!(Error, IntOverflow, Some("when doing compound assignment"));
                                            }
                                        } else {
                                            report!(Error, MismTypes, Some("expected type of the expression: integer"));
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
                                            mem::Value::Float32(result)
                                        } else {
                                            report!(Error, MismTypes, Some("expected type of the expression: float"));
                                        }
                                    }
                                    _ => panic!()
                                }
                            };

                            variables.insert(calling.to_string(), mem::Variable::new(v, false));
                        }
                        Err(_) => { report!(Error, InvalidExpr, Some("when evaluating the expression")); }
                    }
                }
            }
        }

        next!();
    }
}