use std::process::{Command, ExitCode};

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} <command> <expected-output>", args[0]);
        return ExitCode::from(2);
    }

    let output = match Command::new("sh").arg("-c").arg(&args[1]).output() {
        Ok(o) => o,
        Err(e) => {
            eprintln!("FAIL (failed to execute command): {e}");
            return ExitCode::from(1);
        }
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("FAIL (command exited with {})", output.status);
        if !stderr.is_empty() {
            eprintln!("  stderr: {stderr}");
        }
        return ExitCode::from(1);
    }

    let actual = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let expected = args[2].trim().to_string();

    if actual == expected {
        println!("PASS");
        ExitCode::SUCCESS
    } else {
        eprintln!("FAIL");
        eprintln!("  expected: {expected:?}");
        eprintln!("  actual:   {actual:?}");
        ExitCode::from(1)
    }
}
