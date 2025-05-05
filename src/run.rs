use std::collections::HashMap;

use evalexpr::*;

use crate::{
    lang::{self, *},
    mem,
    shell::{*, ErrorType::*, ReportType::*}
};

pub fn exec(source: String) {
    let mut variables: HashMap<String, mem::Variable> = HashMap::new();
    
    'l: for (at, line) in source.lines().enumerate() {
        macro_rules! report {
            ($rt:expr, $w:expr, $n:expr) => {
                report($rt, at, $w, $n);
                continue;
            };
        }

        let tokens = match lang::tokenize(line) {
            Ok(tokens) => if !tokens.is_empty() { tokens } else { continue 'l; },
            Err(err) => {
                match err {
                    TokenizeError::InvalidCharacter(ch) => report(Error, at, InvalidChar(ch), None),
                    TokenizeError::IntegerOverflow => report(Error, at, IntOverflow, Some("this line has an overflowing integer (expected signed 32-bit)")),
                }
                continue 'l;
            }
        };

        let calling = {
            let first_token = &tokens[0];
            match first_token {
                Token::Identifier(string) => string,
                _ => {
                    report!(Error, ExpectedIdent, None);
                }
            }
        };

        #[allow(clippy::match_single_binding)]
        match calling.as_str() {
            // <- add built-in functions here
            "dbg_vars" => {
                //                  |------------- 32 -------------|
                const LINE: &str = "--------------------------------\n";

                println!("\n{LINE}[dbg_vars] list of existing variables:\nID\tVALUE\tPTR");
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
                        Token::PowerOf
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
                    match exprify(toks, Some(&variables)) {
                        Ok(expr) => expr,
                        Err(error) => { report!(Error, error, Some("when parsing the expression")); }
                    }
                } else {
                    report!(Error, ExpectedExpr, None);
                };

                match eval(&expr) {
                    Ok(value) => {
                        let value = match value {
                            evalexpr::Value::Int(int) => match i32::try_from(int) {
                                Ok(value) => mem::Value::Int32(value),
                                Err(_) => { report!(Error, IntOverflow, Some("result of the expression caused integer overflow")); }
                            },
                            evalexpr::Value::Float(fl) => {
                                let fl = fl as f32;
                                if fl.is_finite() {
                                    report!(Error, FloatOverflow, Some("result of the expression caused float overflow"));
                                } else {
                                    mem::Value::Float32(fl)
                                }
                            },
                            _ => panic!("uncovered type in match(eval(&expr)): {:?}", value)
                        };

                        if let Token::Assign = op {
                            variables.insert(calling.to_string(), mem::Variable::new(value, false));
                        } else {
                            let og_value = variables.get_mut(calling).unwrap().value;
                            let new_value = match op {
                                Token::Add => {
                                    match value {
                                        // pls help!!!
                                        Int32(int) => mem::Value::Int32(og_value)
                                    }
                                }
                                _ => panic!("uncovered op in if(let Assign=op)->match(op): {:?}", op)
                            };
                            variables.insert(calling.to_string(), mem::Variable::new(new_value, false));
                        }
                    }
                    Err(_) => { report!(Error, InvalidExpr, Some("when evaluating the expression")); }
                }
            }
        }
    }
}