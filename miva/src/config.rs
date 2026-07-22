use std::collections::HashMap;

use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub project: Option<ProjectConfig>,
    #[serde(default)]
    pub dependencies: Option<HashMap<String, String>>,
    #[serde(default)]
    pub env: Option<EnvConfig>,
    #[serde(default)]
    pub scripts: Option<HashMap<String, String>>,
}

#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub project_type: Option<String>,
    pub version: Option<String>,
    pub backend: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
pub struct EnvConfig {}

impl Config {
    pub fn load() -> Option<Self> {
        Self::load_from("miva.toml")
    }

    pub fn load_from(path: &str) -> Option<Self> {
        let content = std::fs::read_to_string(path).ok()?;
        toml::from_str(&content).ok()
    }

    pub fn project_name() -> Option<String> {
        Self::load()?.project?.name
    }

    pub fn dependencies(&self) -> HashMap<String, String> {
        self.dependencies.clone().unwrap_or_default()
    }

    pub fn project_backend(&self) -> Option<String> {
        self.project.as_ref()?.backend.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn write_config(dir: &std::path::Path, content: &str) {
        let path = dir.join("miva.toml");
        let mut f = std::fs::File::create(&path).unwrap();
        f.write_all(content.as_bytes()).unwrap();
    }

    #[test]
    fn test_config_empty_file() {
        let dir = std::env::temp_dir().join("miva_test_empty");
        let _ = std::fs::create_dir_all(&dir);
        assert!(Config::load_from(&dir.join("miva.toml").to_string_lossy()).is_none());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_project_name() {
        let dir = std::env::temp_dir().join("miva_test_name");
        let _ = std::fs::create_dir_all(&dir);
        write_config(&dir, r#"[project]
name = "myapp""#);
        let orig_dir = std::env::current_dir().unwrap();
        let _ = std::env::set_current_dir(&dir);
        let result = Config::project_name();
        let _ = std::env::set_current_dir(orig_dir);
        assert_eq!(result, Some("myapp".to_string()));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_dependencies() {
        let dir = std::env::temp_dir().join("miva_test_deps");
        let _ = std::fs::create_dir_all(&dir);
        write_config(&dir, r#"
[project]
name = "myapp"

[dependencies]
std = "0.1.3"
json = "0.1.3"
"#);
        let config = Config::load_from(&dir.join("miva.toml").to_string_lossy()).unwrap();
        let deps = config.dependencies();
        assert_eq!(deps.get("std"), Some(&"0.1.3".to_string()));
        assert_eq!(deps.get("json"), Some(&"0.1.3".to_string()));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_empty_dependencies() {
        let dir = std::env::temp_dir().join("miva_test_empty_deps");
        let _ = std::fs::create_dir_all(&dir);
        write_config(&dir, r#"[project]
name = "myapp""#);
        let config = Config::load_from(&dir.join("miva.toml").to_string_lossy()).unwrap();
        let deps = config.dependencies();
        assert!(deps.is_empty());
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_project_backend() {
        let dir = std::env::temp_dir().join("miva_test_backend");
        let _ = std::fs::create_dir_all(&dir);
        write_config(&dir, r#"[project]
name = "myapp"
backend = "llvm""#);
        let config = Config::load_from(&dir.join("miva.toml").to_string_lossy()).unwrap();
        assert_eq!(config.project_backend(), Some("llvm".to_string()));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_project_type() {
        let dir = std::env::temp_dir().join("miva_test_type");
        let _ = std::fs::create_dir_all(&dir);
        write_config(&dir, r#"[project]
name = "mylib"
type = "lib""#);
        let config = Config::load_from(&dir.join("miva.toml").to_string_lossy()).unwrap();
        assert_eq!(config.project.as_ref().unwrap().project_type, Some("lib".to_string()));
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_no_project() {
        let dir = std::env::temp_dir().join("miva_test_no_project");
        let _ = std::fs::create_dir_all(&dir);
        write_config(&dir, r#"[dependencies]
std = "0.1.3""#);
        let config = Config::load_from(&dir.join("miva.toml").to_string_lossy()).unwrap();
        assert!(config.project.is_none());
        assert_eq!(config.project_backend(), None);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_config_malformed_toml() {
        let dir = std::env::temp_dir().join("miva_test_malformed");
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("miva.toml");
        let _ = std::fs::write(&path, "this is not valid toml {{{");
        assert!(Config::load_from(&path.to_string_lossy()).is_none());
        let _ = std::fs::remove_dir_all(&dir);
    }
}
