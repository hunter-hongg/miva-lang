use std::path::Path;
use std::process::Command;

pub fn find_frontend() -> Option<(String, Option<String>)> {
    if let Ok(output) = Command::new("miva-frontend").arg("--help").output() {
        if output.status.success() {
            return Some(("miva-frontend".to_string(), None));
        }
    }
    let exe_candidates = [
        "../miva-frontend/_build/default/bin/main.exe",
        "miva-frontend/_build/default/bin/main.exe",
        "../../miva-frontend/_build/default/bin/main.exe",
    ];
    for c in &exe_candidates {
        if Path::new(c).exists() {
            return Some((c.to_string(), None));
        }
    }
    let dune_candidates = [
        ("../miva-frontend", "../miva-frontend"),
        ("miva-frontend", "miva-frontend"),
        ("../../miva-frontend", "../../miva-frontend"),
    ];
    for (_check, dir) in &dune_candidates {
        if Path::new(dir).exists() && Path::new(dir).join("dune-project").exists() {
            return Some(("dune".to_string(), Some(dir.to_string())));
        }
    }
    None
}

pub fn run_frontend(
    frontend: &str,
    work_dir: &Option<String>,
    input: &str,
) -> anyhow::Result<String> {
    let output = if frontend == "dune" {
        let dir = work_dir
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("dune work directory not set"))?;
        Command::new("dune")
            .args(["exec", "bin/main.exe", "--", input])
            .current_dir(dir)
            .output()?
    } else {
        Command::new(frontend).arg(input).output()?
    };

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        anyhow::bail!("miva-frontend failed:\n{}", stderr);
    }

    String::from_utf8(output.stdout)
        .map_err(|e| anyhow::anyhow!("Invalid UTF-8 from miva-frontend: {}", e))
}
