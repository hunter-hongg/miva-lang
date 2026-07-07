use std::collections::HashMap;
use std::path::Path;

use serde::{Deserialize, Serialize};

static LOCK_FILE: &str = "miva.lock";

#[derive(Debug, Serialize, Deserialize)]
struct LockedDependency {
    name: String,
    version: String,
}

#[derive(Debug, Serialize, Deserialize, Default)]
struct LockFile {
    #[serde(default)]
    dependency: Vec<LockedDependency>,
}

pub fn read_lock() -> Option<HashMap<String, String>> {
    let content = std::fs::read_to_string(LOCK_FILE).ok()?;
    let lock: LockFile = toml::from_str(&content).ok()?;
    Some(lock.dependency.into_iter().map(|d| (d.name, d.version)).collect())
}

pub fn write_lock(deps: &HashMap<String, String>) -> anyhow::Result<()> {
    let lock = LockFile {
        dependency: {
            let mut v: Vec<_> = deps
                .iter()
                .map(|(n, v)| LockedDependency {
                    name: n.clone(),
                    version: v.clone(),
                })
                .collect();
            v.sort_by(|a, b| a.name.cmp(&b.name));
            v
        },
    };
    let content = toml::to_string_pretty(&lock)?;
    std::fs::write(LOCK_FILE, content)?;
    Ok(())
}

pub fn resolve(
    declared: &HashMap<String, String>,
    std_include_dir: &Path,
) -> anyhow::Result<HashMap<String, String>> {
    let locked = read_lock().unwrap_or_default();

    let mut resolved: HashMap<String, String> = HashMap::new();

    for (name, version) in declared {
        if let Some(lv) = locked.get(name) {
            if lv != version {
                anyhow::bail!(
                    "dependency '{}' locked at version {} but miva.toml requires {}.\n\
                     Run `miva dep` to update the lock file.",
                    name, lv, version
                );
            }
            resolved.insert(name.clone(), lv.clone());
        } else {
            let versioned_dir = std_include_dir.join(format!("{}-{}", name, version));
            if !versioned_dir.exists() {
                anyhow::bail!(
                    "dependency '{}' version {} is not installed.\n\
                     Expected at: {}",
                    name, version,
                    versioned_dir.display()
                );
            }
            resolved.insert(name.clone(), version.clone());
        }
    }

    Ok(resolved)
}

pub fn resolve_force(
    declared: &HashMap<String, String>,
    std_include_dir: &Path,
) -> anyhow::Result<HashMap<String, String>> {
    let mut resolved = HashMap::new();

    for (name, version) in declared {
        let versioned_dir = std_include_dir.join(format!("{}-{}", name, version));
        if !versioned_dir.exists() {
            anyhow::bail!(
                "dependency '{}' version {} is not installed.\n\
                 Expected at: {}",
                name, version,
                versioned_dir.display()
            );
        }
        resolved.insert(name.clone(), version.clone());
    }

    Ok(resolved)
}
