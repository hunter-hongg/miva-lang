#![allow(dead_code)]

use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Config {
    #[serde(default)]
    pub project: Option<ProjectConfig>,
    #[serde(default)]
    pub env: Option<EnvConfig>,
}

#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub project_type: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
pub struct EnvConfig {}

impl Config {
    pub fn load() -> Option<Self> {
        let content = std::fs::read_to_string("miva.toml").ok()?;
        toml::from_str(&content).ok()
    }

    pub fn project_name() -> Option<String> {
        Self::load()?.project?.name
    }
}
