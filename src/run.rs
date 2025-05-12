use std::collections::HashMap;

use colored::Colorize;
use evalexpr::*;

use crate::{
    lang::*,
    mem,
    shell::{*, ErrorType::*, ReportType::*}
};

pub fn exec(source: String) {
    let mut variables: HashMap<String, mem::Variable> = HashMap::new();

    // constants:
    let constants = HashMap::from([
        (String::from("INT_MAX"), (mem::Value::Int32(i32::MAX), false)),
    ]);

    for (ident, (value, is_pointer)) in constants {
        variables.insert(ident, mem::Variable::new(value, is_pointer));
    }
    
    'l: for (at, line) in source.lines().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, at+1, $w, $n);
                continue 'l;
            };
        }

        let tokens = match tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tokens } else { continue 'l; },
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

        match calling.as_str() {
            // <- add built-in functions here
            "assert_eq" => {
                let args = get_args!(2);

                let left_eval = {
                    let expr = match exprify(args[0], Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("found when parsing the first expression")); }
                    };

                    match mem::eval(&expr) {
                        Ok(value) => value,
                        Err((error, note)) => { report!(Error, error, note.as_deref()); }
                    }
                };

                let right_eval = {
                    let expr = match exprify(args[1], Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("found when parsing the second expression")); }
                    };

                    match mem::eval(&expr) {
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

                println!("\n{LINE}[dbg_vars] list of the all existing variables:\n\nID\tVALUE\t\tPTR");
                for (ident, var) in &variables {
                    println!("{}\t{:?}\t{}", ident, var.value, var.is_pointer);
                }
                println!("{LINE}\n");
            }
            _ => {
                let var_exist = variables.contains_key(calling);

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

                let expr = if let Some(toks) = tokens.get(2..) {
                    toks
                } else {
                    report!(Error, ExpectedExpr, None);
                };

                if expr.len() == 1 {
                    let calling = calling.to_string();
                    match expr.first().unwrap() {
                        Token::Identifier(ident) => {
                            let ident = ident.as_str();
                            match ident {
                                "null" => {
                                    // if pointer - remove container
                                    variables.remove(&calling);
                                }
                                "true" | "false" => {
                                    let value = mem::Value::Int32(if ident == "true" { 1 } else { 0 });
                                    variables.insert(calling, mem::Variable::new(value, false));
                                },
                                _ => {
                                    if variables.contains_key(ident) {
                                        variables.insert(calling, variables.get(ident).unwrap().clone());
                                    } else {
                                        report!(Error, UndefinedVariable(ident.to_string()), None);
                                    }
                                }
                            }
                        }
                        Token::Integer(int) => {
                            variables.insert(calling, mem::Variable::new(mem::Value::Int32(*int), false));
                        }
                        Token::StringLiteral(string) => {
                            variables.insert(calling, mem::Variable::new(mem::Value::Str(string.to_string()), false));
                        }
                        _ => { /* report!(Error, InvalidExpr, None); */ }
                    }
                } else {
                    // the code below may be replaced with mem::eval(expr)

                    let expr = match exprify(tokens.get(2..).unwrap(), Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("found when parsing the expression")); }
                    };

                    match eval(&expr) {
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
    
                            let v = if let Token::Assign = op {
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
                                            report!(Error, MismTypes, Some("expected type of expression: integer"));
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
                                            report!(Error, MismTypes, Some("expected type of expression: float"));
                                        }
                                    }
                                    _ => panic!()
                                }
                            };
    
                            variables.insert(calling.to_string(), mem::Variable::new(v, false));
                        }
                        Err(_) => { report!(Error, InvalidExpr, Some("when evaluating expression")); }
                    }
                }
            }
        }
    }
}