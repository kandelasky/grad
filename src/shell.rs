use std::fmt;

use colored::Colorize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ReportType {
    Error,
    Warning,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ErrorType {
    IntOverflow,
    FloatOverflow,
    StringInExpr,
    MismTypes,
    FloatExp,

    InvalidChar(char),
    InvalidOp,
    InvalidExpr,

    ExpectedIdent,
    ExpectedOp,
    ExpectedExpr,

    UndefinedVariable(String),
    ConstRedefinition(String),
    NotEnoughArgs(u16 /* expected */),
    LeftoverArgs(u16 /* expected */),
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        let string = match self {
            IntOverflow => "IntegerOverflow",
            FloatOverflow => "FloatOverflow",
            StringInExpr => "StringInExpr",
            MismTypes => "MismatchedTypes",
            FloatExp => "FloatExponentiation",

            InvalidChar(_) => "InvalidChar",
            InvalidOp => "InvalidOperator",
            InvalidExpr => "InvalidExpression",

            ExpectedIdent => "ExpectedIdentifier",
            ExpectedOp => "ExpectedOperator",
            ExpectedExpr => "ExpectedExpression",

            UndefinedVariable(_) => "UndefinedVariable",
            ConstRedefinition(_) => "ConstRedefinition",
            NotEnoughArgs(_) => "NotEnoughArguments",
            LeftoverArgs(_) => "LeftoverArguments",
        };

        write!(f, "{}", string)
    }
}

impl ErrorType {
    pub fn describe(&self) -> String {
        macro_rules! s {
            ($str:expr) => {
                String::from($str)
            };
        }

        use ErrorType::*;
        match self {
            IntOverflow => s!("integer overflow"),
            FloatOverflow => s!("float overflow"),
            StringInExpr => s!("strings cannot appear in expressions"),
            MismTypes => s!("mismatched types"),
            FloatExp => s!("float exponentiation"),

            InvalidChar(ch) => format!("invalid character: {ch}"),
            InvalidOp => s!("invalid operator"),
            InvalidExpr => s!("invalid expression"),

            ExpectedIdent => s!("expected identifier"),
            ExpectedOp => s!("expected operator"),
            ExpectedExpr => s!("expected expression"),

            UndefinedVariable(name) => format!("undefined variable: {}", name.to_string().bold()),
            ConstRedefinition(name) => format!("redefintion of constant: {}", name.to_string().bold()),
            NotEnoughArgs(expected) => format!("not enough arguments: this function takes {}", expected.to_string().bold()),
            LeftoverArgs(expected) => format!("leftover arguments: this function takes {}", expected.to_string().bold()),
        }
    }
}

pub type Report = (ErrorType, Option<String>);

pub fn report(rtype: ReportType, line: usize, what: ErrorType, note: Option<&str>) {
    let rtype = match rtype {
        ReportType::Error => "error".bold().red(),
        ReportType::Warning => "warning".bold().red(),
    };

    eprintln!("\n{rtype}: line {}:\n  {}: {}", line, what.to_string().bold(), what.describe());

    if let Some(text) = note {
        eprintln!("{}: {}", "note".bold().cyan(), text);
    }
}