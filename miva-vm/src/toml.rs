//! TOML parsing and serialization for the Miva VM runtime.
//!
//! Mirrors the `std/toml` contract: a TOML document is parsed into a
//! `serde_json::Value` tree (the same value model used by `std/json`), so the
//! handle/tree/ownership rules are identical across both modules.

use serde_json::Value as JsonValue;

/// Parse TOML text into a `serde_json::Value` tree.
pub fn parse(input: &str) -> Result<JsonValue, String> {
    let tv: toml::Value = toml::from_str(input).map_err(|e| format!("TOML parse error: {}", e))?;
    Ok(convert(tv))
}

fn convert(v: toml::Value) -> JsonValue {
    match v {
        toml::Value::String(s) => JsonValue::String(s),
        toml::Value::Integer(i) => JsonValue::Number(i.into()),
        toml::Value::Float(f) => serde_json::Number::from_f64(f)
            .map(JsonValue::Number)
            .unwrap_or(JsonValue::Null),
        toml::Value::Boolean(b) => JsonValue::Bool(b),
        toml::Value::Datetime(d) => JsonValue::String(d.to_string()),
        toml::Value::Array(a) => {
            JsonValue::Array(a.into_iter().map(convert).collect())
        }
        toml::Value::Table(t) => {
            let mut map = serde_json::Map::new();
            for (k, val) in t {
                map.insert(k, convert(val));
            }
            JsonValue::Object(map)
        }
    }
}

/// Serialize a TOML value tree back into TOML text (best effort).
pub fn stringify(value: &JsonValue) -> String {
    toml::to_string(value).unwrap_or_else(|_| value.to_string())
}
