use std::path::Path;

use grad::*;

fn main() -> Result<(), Error> {
    //eprintln!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));

    let path = Path::new("boot.gr");
    
    let bytes = if let Ok(bytes) = std::fs::read(path) {
        bytes
    } else {
        return Err(Error::FileReadFailed)
    };

    if bytes[0..8] == consts::MAGIC {
        // run_bytecode(SLICE);
    } else if let Ok(source) = String::from_utf8(bytes) {
        run::exec(source);
    } else {
        return Err(Error::InvalidUtf8)
    }
    
    Ok(())
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Error {
    FileReadFailed,
    // CompilationFailed,
    InvalidUtf8
}


impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Error::FileReadFailed => "unable to read file",
            Error::InvalidUtf8 => "invalid UTF-8 data",
        })
    }
}

/* struct GradConfig {
    width: u32,
    height: u32,
    is_fullscreen: bool,
    is_resizable: bool,
}

impl Default for GradConfig {
    fn default() -> Self {
        Self {
            width: 960,
            height: 768,
            is_fullscreen: false,
            is_resizable: true,
        }
    }
} */