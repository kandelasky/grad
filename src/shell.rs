use {
    std::fmt,
    colored::Colorize,
    crate::mem
};

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
    MismTypes(mem::ValueType /* expected */),
    FloatExp,
    NullValue,

    InvalidChar(char),
    InvalidOp,
    InvalidExpr,

    ExpectedIdent,
    ExpectedOp,
    ExpectedExpr,
    UnexpectedEOF,
    UnmatchedEnd,
    UnmatchedRet,
    UnmatchedElse,
    UnterminatedBlock,

    UndefinedVariable(String),
    UnexpectedPointer(String),
    NotEnoughArgs(u16 /* expected */),
    LeftoverArgs(u16 /* expected */),
    TooLongIdent(usize /* max */),
    TooDeepControl(usize /* max */),
    TooDeepCall(usize /* max */),
    NestedFnDefinition,
    ElseForLoop,

    ConstRedef(String),
    FnRedef(String),
    UselessPrint,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        let string = match self {
            IntOverflow => "IntegerOverflow",
            FloatOverflow => "FloatOverflow",
            StringInExpr => "StringInExpr",
            MismTypes(_) => "MismatchedTypes",
            FloatExp => "FloatExponentiation",

            InvalidChar(_) => "InvalidChar",
            InvalidOp => "InvalidOperator",
            InvalidExpr => "InvalidExpression",

            ExpectedIdent => "ExpectedIdentifier",
            ExpectedOp => "ExpectedOperator",
            ExpectedExpr => "ExpectedExpression",
            UnexpectedEOF => "UnexpectedEOF",
            UnmatchedEnd => "UnmatchedEnd",
            UnmatchedRet => "UnmatchedRet",
            UnmatchedElse => "UnmatchedElse",
            UnterminatedBlock => "UnterminatedBlock",

            UndefinedVariable(_) => "UndefinedVariable",
            UnexpectedPointer(_) => "UnexpectedPointer",
            NotEnoughArgs(_) => "NotEnoughArguments",
            LeftoverArgs(_) => "LeftoverArguments",
            TooLongIdent(_) => "TooLongIdentifier",
            TooDeepControl(_) => "TooDeepControl",
            TooDeepCall(_) => "TooDeepCall",
            NullValue => "NullValue",
            NestedFnDefinition => "NestedFnDefinition",
            ElseForLoop => "LoopElse",

            ConstRedef(_) => "ConstRedefinition",
            FnRedef(_) => "FuncRedefinition",
            UselessPrint => "UselessPrint",
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
            MismTypes(expected) => format!(
                "mismatched types: expected {}",
                expected.to_string().bold(),
            ),
            FloatExp => s!("float exponentiation"),

            InvalidChar(ch) => format!("invalid character: {ch}"),
            InvalidOp => s!("invalid operator"),
            InvalidExpr => s!("invalid expression"),

            ExpectedIdent => s!("expected identifier"),
            ExpectedOp => s!("expected operator"),
            ExpectedExpr => s!("expected expression"),
            UnexpectedEOF => s!("unexpected end of file"),
            UnmatchedEnd => format!("unmatched control statement: {}", "end".bold()),
            UnmatchedRet => format!("unmatched {}", "ret".bold()),
            UnmatchedElse => format!("unmatched {}", "else".bold()),
            UnterminatedBlock => s!("unterminated control block"),

            UndefinedVariable(name) => format!("undefined variable: {}", name.bold()),
            UnexpectedPointer(name) => format!("variable {} is pointer", name.bold()),
            NotEnoughArgs(expected) => format!("not enough arguments: this function takes {}", expected.to_string().bold()),
            LeftoverArgs(expected) => format!("leftover arguments: this function takes {}", expected.to_string().bold()),
            TooLongIdent(max) => format!("too long identifier (max length is {} characters)", max.to_string().bold()),
            TooDeepControl(max) => format!("too deep control block (max depth is {})", max.to_string().bold()),
            TooDeepCall(max) => format!("too deep function call (max depth is {})", max.to_string().bold()),
            NullValue => format!("the value has type {}", "null".bold()),
            NestedFnDefinition => s!("function definitions cannot be nested within each other"),
            ElseForLoop => format!("loops cannot have {} blocks", "else".bold()),

            ConstRedef(name) => format!("redefinition of constant: {}", name.bold()),
            FnRedef(name) => format!("redefinition of function: {}", name.bold()),
            UselessPrint => format!("useless use of {}", "print".bold()),
        }
    }
}

//pub type Report = (ErrorType, Option<&str>);

pub fn report(rtype: ReportType, line: (usize, &str), what: ErrorType, note: Option<&str>) {
    let rtype = match rtype {
        ReportType::Error => "error".bold().red(),
        ReportType::Warning => "warning".bold().yellow(),
    };

    eprintln!("{rtype}:\n {} {} {}\n{}: {}", (line.0+1).to_string().bold(), "|".bold(), line.1, what.to_string().bold(), what.describe());

    if let Some(text) = note {
        eprintln!("{}: {text}", "note".bold().cyan());
    }

    eprintln!();
}