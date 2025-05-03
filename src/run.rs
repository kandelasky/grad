use std::collections::HashMap;

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
                    TokenizeError::IntegerOverflow => report(Error, at, IntOverflow, None),
                }
                continue 'l;
            }
        };

        let calling = {
            let first_token = &tokens[0];
            match first_token {
                Token::Identifier(string) => string.as_str(),
                _ => {
                    report!(Error, ExpectedIdent, None);
                }
            }
        };

        #[allow(clippy::match_single_binding)]
        match calling {
            // <- add built-in functions here
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

                let exp = if let Some(toks) = tokens.get(2..) {
                    match exprify(toks, Some(&variables)) {
                        Ok(exp) => exp,
                        Err(error) => { report!(Error, error, Some("when parsing expression")); }
                    }
                } else {
                    report!(Error, ExpectedExpr, None);
                };

                //
            }
        }
    }
}