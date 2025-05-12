use std::path::Path;

use grad::*;
use macroquad::prelude as mq;
use serde_derive::*;
use colored::Colorize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Error {
    FileReadFailed,
    // CompilationFailed,
    InvalidUtf8
}

#[macroquad::main(window_config)]
async fn main() -> Result<(), Error> {
    eprintln!("Grad v{}", consts::VERSION_STR);

    let path = Path::new("boot.gr");

    let bytes = match std::fs::read(path) {
        Ok(bytes) => bytes,
        Err(err) => {
            eprintln!("{}: failed to read file:\n  {}: {}", "fatal error".bold().red(), format!("{:?}", err.kind()).bold(), err.kind());
            return Err(Error::FileReadFailed)
        }
    };

    let is_bytecode = bytes[0..=8] == consts::MAGIC;
    if is_bytecode {
        // run_bytecode(SLICE);
    } else if let Ok(source) = String::from_utf8(bytes) {
        run::exec(source);
    } else {
        eprintln!("{}: failed to read file:\n  {}", "fatal error".bold().red(), "this file contains invalid UTF-8 data".bold());
        return Err(Error::InvalidUtf8)
    }

    //std::thread::sleep(std::time::Duration::from_secs(5));

    Ok(())
}

#[derive(Serialize, Deserialize)]
struct GradConfig {
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
}

fn window_config() -> mq::Conf {
    let conf = if let Ok(conf) = confy::load::<GradConfig>("grad", None) {
        conf
    } else {
        eprintln!("grad: unable to load config file, using default");
        Default::default()
    };

    mq::Conf {
        window_title: format!("Grad v{}", consts::VERSION_STR),
        window_width: conf.width as i32,
        window_height: conf.height as i32,
        window_resizable: conf.is_resizable,
        fullscreen: conf.is_fullscreen,
        // TODO: icon
        ..Default::default()
    }
}
