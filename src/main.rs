//use std::path::Path;
use std::collections::HashMap;
use grad::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum KickstartError {
    FileReadFailed,
    CompilationFailed,
    InvalidUtf8
}

fn main() -> Result<(), KickstartError> {
    eprintln!("Grad v{}", meta::VERSION_STR);

    /* let path = Path::new("boot.grad");

    let bytes = match std::fs::read(path) {
        Ok(bytes) => bytes,
        Err(err) => {
            eprintln!("failed to read file: {0:?}: {0}", err.kind());
            return Err(KickstartError::FileReadFailed)
        }
    };

    let bytecode: Vec<u8> = {
        if bytes[0..=8] != consts::MAGIC {
            if let Ok(source) = String::from_utf8(bytes) {
                let compile_time = std::time::Instant::now();

                if let Some(bytecode) = vm::make_bytecode(source.as_str()) {
                    println!("compiled in {} ms", compile_time.elapsed().as_millis());
                    bytecode
                } else {
                    eprintln!("\nfailed to compile: {}", path.display());
                    return Err(KickstartError::CompilationFailed)
                }
            } else {
                eprintln!("failed to read file: invalid UTF-8 data");
                return Err(KickstartError::InvalidUtf8)
            }
        } else {
            bytes[8..].to_vec()
        }
    }; */

    use vm::Instruction::*;

    let main: Vec<vm::Instruction> = {
        // TODO: file read
        vec![
            SetFlagMemLog { value: 1 },
            SetFlagMemLogEnd { value: 1 },

            SetX { value: 16 },
            SetY { value: 16 },
            NewContainer { id: 0 },

            SetX { value: 0 },
            SetY { value: 0 },
            SetValue { value: 42 },
            SetContainer { id: 0 },

            NewVariable { id: 0 },
            GetContainer { id: 0, var: 0 },
            DeleteContainer { id: 0 },
        ]
    };
    
    let mut reg_xy: (Option<usize>, Option<usize>) = (None, None);
    let mut reg_value: Option<vm::Value> = None;
    
    let mut flag_flt: bool = false;
    let (mut mem_debug, mut mem_end) = (false, false);
    
    let mut cont: HashMap<u16, vm::Container> = HashMap::new();
    let mut float_cont: HashMap<u16, vm::FloatContainer> = HashMap::new();

    let mut vars: HashMap<u16, u32> = HashMap::new();
    let mut float_vars: HashMap<u16, vm::Float32> = HashMap::new();
    
    for (at, instr) in main.iter().enumerate() {
        macro_rules! pre_report {
            () => (
                if config::DEBUG {
                    format!("gradvm: b:{}: ", at)
                } else {
                    String::from("gradvm: ")
                }
            );
        }

        macro_rules! on_report {
            () => {
                if config::BREAK_ON_ERROR {
                    break;
                } else {
                    continue;
                }
            };
        }

        macro_rules! sreport {
            ($str:expr) => {
                eprintln!("{}{}", pre_report!(), String::from($str));
                on_report!();
            };
        }

        macro_rules! report {
            ($str:expr) => {
                eprintln!("{}{}", pre_report!(), $str);
                on_report!();
            };
        }

        macro_rules! mem_log {
            ($what:expr, $id:expr, $size:expr, $typ:expr) => {
                if mem_debug {
                    println!("{}: id {}: size = {}; {}", $what, $id, $size, $typ);
                }
            };
            ($what:expr, $id:expr, $typ:expr) => {
                if mem_debug {
                    println!("{}: id {}: type = {}", $what, $id, $typ);
                }
            };
            ($what:expr, $id:expr) => {
                if mem_debug {
                    println!("{}: id {}", $what, $id);
                }
            }
        }

        macro_rules! get_xsize {
            ($caller:expr) => {
                if let Some(xsize) = reg_xy.0 {
                    xsize
                } else {
                    report!(format!("{}: IDXX not set", $caller));
                }
            };
        }

        macro_rules! get_ysize {
            ($caller:expr) => {
                if let Some(ysize) = reg_xy.1 {
                    ysize
                } else {
                    report!(format!("{}: IDXY not set", $caller));
                }
            };
        }

        macro_rules! get_size {
            ($caller:expr) => {{
                (get_xsize!($caller), get_ysize!($caller))
            }};
        }

        macro_rules! get_value {
            ($caller:expr) => {
                if let Some(value) = reg_value {
                    value
                } else {
                    report!(format!("{}: VAL not set", $caller));
                }
            };
        }

        macro_rules! get_cont_id {
            () => {{
                let mut index: Option<u16> = None;
                for i in 0..=u16::MAX {
                    if !cont.contains_key(&i) {
                        index = Some(i);
                        break;
                    }
                }
                if let Some(index) = index {
                    index
                } else {
                    sreport!("all container slots are busy");
                }
            }};
        }

        macro_rules! get_fcont_id {
            () => {{
                let mut index: Option<u16> = None;
                for i in 0..=u16::MAX {
                    if !float_cont.contains_key(&i) {
                        index = Some(i);
                        break;
                    }
                }
                if let Some(index) = index {
                    index
                } else {
                    sreport!("all float container slots are busy");
                }
            }};
        }

        macro_rules! get_var_id {
            () => {{
                let mut index: Option<u16> = None;
                for i in 0..=u16::MAX {
                    if !vars.contains_key(&i) {
                        index = Some(i);
                        break;
                    }
                }
                if let Some(index) = index {
                    index
                } else {
                    sreport!("all variable slots are busy");
                }
            }};
        }

        macro_rules! get_fvar_id {
            () => {{
                let mut index: Option<u16> = None;
                for i in 0..=u16::MAX {
                    if !float_vars.contains_key(&i) {
                        index = Some(i);
                        break;
                    }
                }
                if let Some(index) = index {
                    index
                } else {
                    sreport!("all variable slots are busy");
                }
            }};
        }

        const MSG_UNDEF_CONT: &str = "undefined container";
        const MSG_UNDEF_VAR: &str = "undefined variable";

        const MSG_REDEF_CONT: &str = "redefinition of a container";
        const MSG_REDEF_VAR: &str = "redefinition of a variable";

        let (is_integer, is_float) = (!flag_flt, flag_flt);

        if config::DEBUG && config::LOG_RUNNING_OP {
            println!("op: {:?}", instr);
        }

        match *instr {
            PageMainStart | PageFnStart => { report!(format!("unexpected {:?} in `main` page", *instr)); },

            SetX { value } => reg_xy.0 = Some(value as usize),
            SetY { value } => reg_xy.1 = Some(value as usize),

            SetValue { value } => reg_value = Some(vm::Value::Integer(value)),

            SetFlagFloat { value } => flag_flt = value >= 1,
            SetFlagMemLog { value } => mem_debug = value >= 1,
            SetFlagMemLogEnd { value } => mem_end = value >= 1,

            NewContainer { id } => {
                if (is_integer && cont.contains_key(&id)) || (is_float && float_cont.contains_key(&id)) {
                    report!(format!("NewContainer: {}", MSG_REDEF_CONT));
                }

                let (xsize, ysize) = get_size!("NewContainer");

                if is_integer {
                    let index: u16 = get_cont_id!();

                    let container = if let Some(cont) = vm::Container::new((xsize, ysize)) {
                        mem_log!("new container", index, xsize*ysize, "int");
                        cont
                    } else {
                        sreport!("NewContainer: size overflow");
                    };
                    
                    cont.insert(index, container);
                } else if is_float {
                    let index: u16 = get_fcont_id!();

                    let container = if let Some(cont) = vm::FloatContainer::new((xsize, ysize)) {
                        mem_log!("new container", index, xsize*ysize, "float");
                        cont
                    } else {
                        sreport!("NewContainer: size overflow");
                    };
                    
                    float_cont.insert(index, container);
                }
            } // NewContainer

            SetContainer { id } => {
                if (is_integer && !cont.contains_key(&id)) || (is_float && !float_cont.contains_key(&id)) {
                    report!(format!("SetContainer: {}", MSG_UNDEF_CONT));
                }

                let (xsize, ysize) = get_size!("SetContainer");
                let value = get_value!("SetContainer");

                if is_integer {
                    let container: &mut vm::Container = cont.get_mut(&id).unwrap();

                    if xsize >= container.size.0 || ysize >= container.size.1 {
                        sreport!("SetContainer: unreachable");
                    }

                    let value: u32 = if let vm::Value::Integer(value) = value {
                        value
                    } else {
                        sreport!("SetContainer: VAL expected to have type integer");
                    };

                    if let Some(elem) = container.data.get_mut(xsize * ysize) {
                        *elem = value;
                    }
                } else if is_float {
                    let container: &mut vm::FloatContainer = float_cont.get_mut(&id).unwrap();
                    
                    if xsize >= container.size.0 || ysize >= container.size.1 {
                        sreport!("SetContainer: unreachable");
                    }
                    
                    let value: vm::Float32 = if let vm::Value::Float(value) = value {
                        value
                    } else {
                        sreport!("SetContainer: VAL expected to have type float");
                    };
                    
                    if let Some(elem) = container.data.get_mut(xsize * ysize) {
                        *elem = value;
                    }
                }
            } // SetContainer

            GetContainer { id, var } => {
                if (is_integer && !cont.contains_key(&id)) || (is_float && !float_cont.contains_key(&id)) {
                    report!(format!("GetContainer: {}", MSG_UNDEF_CONT));
                }
                if (is_integer && !vars.contains_key(&var)) || (is_float && !float_vars.contains_key(&var)) {
                    report!(format!("GetContainer: {}", MSG_UNDEF_VAR));
                }

                let (xsize, ysize) = get_size!("GetContainer");

                if is_integer {
                    let container = &cont.get(&id).unwrap().data;
                    let value = if let Some(value) = container.get(xsize * ysize) {
                        value
                    } else {
                        sreport!("GetContainer: unreachable");
                    };
                    vars.insert(var, *value);
                } else if is_float {
                    let container = &float_cont.get(&id).unwrap().data;
                    let value = if let Some(value) = container.get(xsize * ysize) {
                        value
                    } else {
                        sreport!("GetContainer: unreachable");
                    };
                    float_vars.insert(var, *value);
                }
            } // GetContainer

            DeleteContainer { id } => {
                if (is_integer && cont.remove(&id).is_none()) || (is_float && float_cont.remove(&id).is_none()) {
                    report!(format!("DeleteContainer: {}", MSG_UNDEF_CONT));
                }
                // and if succeed:
                if is_integer {
                    mem_log!("delete container", id, "int");
                } else if is_float {
                    mem_log!("delete container", id, "float");
                }
            } // DeleteContainer

            NewVariable { id } => {
                if (is_integer && vars.contains_key(&id)) || (is_float && float_vars.contains_key(&id)) {
                    report!(format!("NewVariable: {}", MSG_REDEF_VAR));
                }

                if is_integer {
                    let index: u16 = get_var_id!();
                    vars.insert(index, 0);
                    mem_log!("new variable", index, "int");
                } else if is_float {
                    let index: u16 = get_fvar_id!();
                    float_vars.insert(index, vm::Float32::empty());
                    mem_log!("new variable", index, "float");
                }
            } // NewVariable

            _ => todo!()
        }
    }

    if mem_end {
        //                       |------------ 30 ------------|
        const BIG_DELIM: &str = "------------------------------";

        println!("\n{BIG_DELIM}\nleft containers (int):\n{:#?}", cont);
        println!("\nleft containers (float):\n{:#?}", float_cont);

        println!("\n{BIG_DELIM}\nleft variables (int):\n{:#?}", vars);
        println!("\nleft variables (float):\n{:#?}", float_vars);

        println!("\n{BIG_DELIM}");
    }

    Ok(())
}
