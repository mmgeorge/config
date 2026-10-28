use serde::Deserialize;
use std::{collections::BTreeMap, error::Error, fmt::Write, fs, path::PathBuf};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Contract {
    version: u32,
    node: NodeContract,
    session: BTreeMap<String, Event>,
    backend: BTreeMap<String, Event>,
    request: BTreeMap<String, Event>,
    document: BTreeMap<String, Event>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct NodeContract {
    kind: Vec<String>,
    lifecycle: Vec<String>,
    display: Vec<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Event {
    #[serde(rename = "route")]
    _route: Route,
    required: BTreeMap<String, ExpectedType>,
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum Route {
    Handle,
    Refresh,
    Ignore,
    Transport,
    Progress,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ExpectedType {
    String,
    NullableString,
    Integer,
    Object,
    Boolean,
}

fn main() -> Result<(), Box<dyn Error>> {
    let source = PathBuf::from("../../../../lua/forge/protocol_contract.json");
    println!("cargo:rerun-if-changed={}", source.display());
    let contract: Contract = serde_json::from_slice(&fs::read(source)?)?;
    let NodeContract {
        kind,
        lifecycle,
        display,
    } = contract.node;
    for vocabulary in [kind, lifecycle, display] {
        if vocabulary.is_empty() || vocabulary.iter().any(String::is_empty) {
            return Err("node vocabulary must contain nonempty names".into());
        }
    }
    let mut generated = format!("const CONTRACT_VERSION: u32 = {};\n", contract.version);
    for (channel, events) in [
        ("SESSION", contract.session),
        ("BACKEND", contract.backend),
        ("REQUEST", contract.request),
        ("DOCUMENT", contract.document),
    ] {
        writeln!(generated, "static {channel}: &[Event] = &[")?;
        for (name, event) in events {
            if name.is_empty() {
                return Err("event names must be nonempty".into());
            }
            let Event {
                _route: _,
                required,
            } = event;
            writeln!(generated, "Event {{ name: {name:?}, required: &[")?;
            for (path, expected) in required {
                let segments = path.split('.').collect::<Vec<_>>();
                if segments.iter().any(|segment| segment.is_empty()) {
                    return Err(format!("invalid required field path: {path:?}").into());
                }
                let label = match expected {
                    ExpectedType::String => "string",
                    ExpectedType::NullableString => "nullable_string",
                    ExpectedType::Integer => "integer",
                    ExpectedType::Object => "object",
                    ExpectedType::Boolean => "boolean",
                };
                let accepts = match expected {
                    ExpectedType::String => {
                        "|value| value.as_str().is_some_and(|text| !text.is_empty())"
                    }
                    ExpectedType::NullableString => {
                        "|value| value.is_null() || value.as_str().is_some_and(|text| !text.is_empty())"
                    }
                    ExpectedType::Integer => {
                        "|value| value.as_u64().is_some_and(|number| number < (1 << 53))"
                    }
                    ExpectedType::Object => "Value::is_object",
                    ExpectedType::Boolean => "Value::is_boolean",
                };
                writeln!(
                    generated,
                    "RequiredField {{ path: &{segments:?}, label: {path:?}, accepts: {accepts}, description: {label:?} }},"
                )?;
            }
            writeln!(generated, "] }},")?;
        }
        writeln!(generated, "];\n")?;
    }
    fs::write(
        PathBuf::from(std::env::var_os("OUT_DIR").ok_or("Cargo OUT_DIR is missing")?)
            .join("event_contract.rs"),
        generated,
    )?;
    Ok(())
}
