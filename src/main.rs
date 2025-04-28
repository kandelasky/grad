//use std::path::Path;

use grad::*;
use macroquad::prelude as mq;
use serde_derive::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum KickstartError {
    FileReadFailed,
    CompilationFailed,
    InvalidUtf8
}

#[macroquad::main(window_config)]
async fn main() -> Result<(), KickstartError> {
    eprintln!("Grad v{}", consts::VERSION_STR);

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
