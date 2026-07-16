//! YAML parsing and serialization for the Miva VM runtime.
//!
//! Mirrors the `std/yaml` contract: a YAML document is parsed into a
//! `serde_json::Value` tree (the same value model used by `std/json`), so the
//! handle/tree/ownership rules are identical across both modules.

use serde_json::Value as JsonValue;

/// Parse YAML text into a `serde_json::Value` tree.
pub fn parse(input: &str) -> Result<JsonValue, String> {
    let yv: serde_yaml::Value =
        serde_yaml::from_str(input).map_err(|e| format!("YAML parse error: {}", e))?;
    Ok(convert(yv))
}

fn convert(v: serde_yaml::Value) -> JsonValue {
    match v {
        serde_yaml::Value::Null => JsonValue::Null,
        serde_yaml::Value::Bool(b) => JsonValue::Bool(b),
        serde_yaml::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                JsonValue::Number(i.into())
            } else if let Some(f) = n.as_f64() {
                serde_json::Number::from_f64(f)
                    .map(JsonValue::Number)
                    .unwrap_or(JsonValue::Null)
            } else {
                JsonValue::Null
            }
        }
        serde_yaml::Value::String(s) => JsonValue::String(s),
        serde_yaml::Value::Sequence(seq) => {
            JsonValue::Array(seq.into_iter().map(convert).collect())
        }
        serde_yaml::Value::Mapping(map) => {
            let mut obj = serde_json::Map::new();
            for (k, val) in map {
                let key = match k {
                    serde_yaml::Value::String(s) => s,
                    other => serde_yaml::to_string(&other)
                        .unwrap_or_default()
                        .trim_matches('"')
                        .to_string(),
                };
                obj.insert(key, convert(val));
            }
            JsonValue::Object(obj)
        }
        serde_yaml::Value::Tagged(t) => convert(t.value),
    }
}

/// Serialize a YAML value tree back into YAML text.
///
/// Mirrors the block-style formatter used by the C/C++ runtime
/// (`mvp_yaml_stringify_impl` in `stdlib/mvp_builtin.h`) so the MVM backend
/// produces byte-identical output to the cxx/llvm backends:
///   * nested objects/arrays are emitted with a 2-space indent;
///   * scalars are emitted inline after `key: ` / `- `;
///   * object keys and scalar strings are written verbatim (no quoting).
pub fn stringify(value: &JsonValue) -> String {
    let mut out = String::with_capacity(64);
    stringify_impl(value, &mut out, 0);
    out
}

fn pad(out: &mut String, n: i32) {
    for _ in 0..n {
        out.push(' ');
    }
}

fn scalar_to_string(value: &JsonValue) -> String {
    match value {
        JsonValue::Null => "null".to_string(),
        JsonValue::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        JsonValue::Number(n) => number_to_string(n.as_f64().unwrap_or(0.0)),
        JsonValue::String(s) => s.clone(),
        // Containers are never emitted as scalars; fall back to empty.
        _ => String::new(),
    }
}

/// Replicates the C++ `std::to_string` + trailing-zero strip used by the C
/// runtime's YAML serializer.
fn number_to_string(n: f64) -> String {
    let mut s = format!("{}", n);
    if let Some(dot) = s.find('.') {
        // Last position whose character is not '0' ('.' counts as non-'0').
        if let Some(p) = s.rfind(|c: char| c != '0') {
            if p <= dot {
                // Nothing but zeros after the decimal point: drop it entirely.
                s.truncate(dot);
            } else {
                // Drop trailing zeros (keep through position `p`).
                s.truncate(p + 1);
            }
        }
    }
    s
}

fn stringify_impl(j: &JsonValue, out: &mut String, ind: i32) {
    match j {
        JsonValue::Object(map) => {
            let mut first = true;
            for (k, v) in map {
                if !first {
                    out.push('\n');
                }
                first = false;
                pad(out, ind);
                out.push_str(k);
                out.push(':');
                match v {
                    JsonValue::Object(_) | JsonValue::Array(_) => {
                        out.push('\n');
                        stringify_impl(v, out, ind + 2);
                    }
                    _ => {
                        out.push(' ');
                        stringify_impl(v, out, 0);
                    }
                }
            }
        }
        JsonValue::Array(arr) => {
            let mut first = true;
            for el in arr {
                if !first {
                    out.push('\n');
                }
                first = false;
                pad(out, ind);
                out.push('-');
                match el {
                    JsonValue::Object(_) | JsonValue::Array(_) => {
                        out.push('\n');
                        stringify_impl(el, out, ind + 2);
                    }
                    _ => {
                        out.push(' ');
                        stringify_impl(el, out, 0);
                    }
                }
            }
        }
        _ => {
            out.push_str(&scalar_to_string(j));
        }
    }
}
