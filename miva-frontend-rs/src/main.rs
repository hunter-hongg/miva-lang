use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => {
            // miva-frontend <input.miva>  → stdout
            let input_file = &args[1];
            let source = match fs::read_to_string(input_file) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Error: cannot read '{}': {}", input_file, e);
                    process::exit(1);
                }
            };
            match miva_frontend::parse_to_json(&source, input_file) {
                Ok(json) => print!("{}", json),
                Err(e) => {
                    eprintln!("{}", e);
                    process::exit(1);
                }
            }
        }
        4 if args[2] == "-o" => {
            // miva-frontend <input.miva> -o <output.json>
            let input_file = &args[1];
            let output_file = &args[3];
            let source = match fs::read_to_string(input_file) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Error: cannot read '{}': {}", input_file, e);
                    process::exit(1);
                }
            };
            match miva_frontend::parse_to_json(&source, input_file) {
                Ok(json) => {
                    if let Err(e) = fs::write(output_file, json) {
                        eprintln!("Error: cannot write '{}': {}", output_file, e);
                        process::exit(1);
                    }
                }
                Err(e) => {
                    eprintln!("{}", e);
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("Usage: miva-frontend <input.miva> [-o output.ast.json]");
            process::exit(1);
        }
    }
}
