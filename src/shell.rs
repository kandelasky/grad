use std::fmt;

use colored::Colorize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReportType {
    Error,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorType {
    IntOverflow,
    StringInExpr,
    InvalidChar(char),
    InvalidOp,
    InvalidExpr,
    ExpectedIdent,
    ExpectedOp,
    ExpectedExpr,
    UndefinedVariable(String),
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        let string = match self {
            IntOverflow => "IntegerOverflow",
            StringInExpr => "StringInExpr",
            InvalidChar(_) => "InvalidChar",
            InvalidOp => "InvalidOperator",
            InvalidExpr => "invalid expression",
            ExpectedIdent => "ExpectedIdentifier",
            ExpectedOp => "ExpectedOperator",
            ExpectedExpr => "ExpectedExpression",
            UndefinedVariable(_) => "UndefinedVariable",
        };

        write!(f, "{}", string)
    }
}

impl ErrorType {
    pub fn describe(&self) -> String {
        /* macro_rules! s {
            ($str:expr) => {
                String::from($str)
            };
        } */

        use ErrorType::*;
        match self {
            IntOverflow => format!("{}", "this line has a number that doesn't fit into 32-bit integer".bold()),
            StringInExpr => format!("{}", "strings cannot appear in expressions".bold()),
            InvalidChar(ch) => format!("{}: {}", "invalid character".bold(), ch),
            InvalidOp => format!("{}", "invalid operator".bold()),
            InvalidExpr => format!("{}", "invalid expression".bold()),
            ExpectedIdent => format!("{}", "expected identifier".bold()),
            ExpectedOp => format!("{}", "expected operator".bold()),
            ExpectedExpr => format!("{}", "expected expression".bold()),
            UndefinedVariable(name) => format!("{}: {}", "undefined variable".bold(), name),
        }
    }
}

pub fn report(rtype: ReportType, line: usize, what: ErrorType, note: Option<&str>) {
    let rtype = match rtype {
        ReportType::Error => "error".bold().red(),
    };

    eprintln!("\n{rtype}: line {line}:\n  {}: {}", what.to_string().bold(), what.describe());

    if let Some(text) = note {
        eprintln!("{}: {}", "hint".bold().green(), text);
    }
}