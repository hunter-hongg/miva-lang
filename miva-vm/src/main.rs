use std::env;
use std::fs;
use std::process;

use miva_vm::MvmProgram;
use miva_vm::Mvm;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: mvm <bytecode.mvm> [--debug]");
        process::exit(1);
    }

    let file = &args[1];
    let debug = args.contains(&"--debug".to_string());

    let data = match fs::read(file) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Error reading {}: {}", file, e);
            process::exit(1);
        }
    };

    let program = match MvmProgram::from_bytes(&data) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error loading program: {}", e);
            process::exit(1);
        }
    };

    if debug {
        eprintln!("Loaded program with {} strings, {} functions",
            program.strings.len(), program.functions.len());
        for (i, f) in program.functions.iter().enumerate() {
            eprintln!("  func[{}]: {} (arity={}, locals={}, code_size={})",
                i, f.name(&program.strings), f.arity, f.locals, f.code.len());
        }
    }

    let mut vm = Mvm::new(program);
    match vm.run() {
        Ok(code) => {
            process::exit(code as i32);
        }
        Err(e) => {
            eprintln!("MVM runtime error: {}", e);
            process::exit(1);
        }
    }
}
