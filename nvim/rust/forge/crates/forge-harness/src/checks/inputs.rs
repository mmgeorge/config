use anyhow::{Result, bail, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;

pub type CheckInputMap = BTreeMap<String, CheckInput>;
pub type CheckInputValues = BTreeMap<String, Value>;

#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckInputType {
    String,
    Integer,
    Number,
    Boolean,
    Array,
    Object,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckInputSource {
    ChangedFiles,
    PlannedCases,
    Workspace,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CheckInput {
    #[serde(rename = "type")]
    pub kind: CheckInputType,
    pub description: String,
    #[serde(default)]
    pub required: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub default: Option<Value>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source: Option<CheckInputSource>,
}

impl CheckInput {
    pub(super) fn validate(&self, name: &str, value: &Value) -> Result<()> {
        let valid = match self.kind {
            CheckInputType::String => value.is_string(),
            CheckInputType::Integer => value.is_i64() || value.is_u64(),
            CheckInputType::Number => value.is_number(),
            CheckInputType::Boolean => value.is_boolean(),
            CheckInputType::Array => value.is_array(),
            CheckInputType::Object => value.is_object(),
        };
        ensure!(valid, "check input {name} must have type {:?}", self.kind);
        Ok(())
    }
}

pub(super) fn validate_schema(schema: &CheckInputMap) -> Result<()> {
    for (name, input) in schema {
        ensure!(
            !name.is_empty()
                && name
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
                && !name.as_bytes()[0].is_ascii_digit(),
            "invalid check input name: {name}"
        );
        ensure!(
            !input.description.trim().is_empty(),
            "check input {name} needs a description"
        );
        if let Some(value) = &input.default {
            input.validate(name, value)?;
        }
        if let Some(source) = input.source {
            let expected = match source {
                CheckInputSource::Workspace => CheckInputType::String,
                CheckInputSource::ChangedFiles | CheckInputSource::PlannedCases => {
                    CheckInputType::Array
                }
            };
            ensure!(
                input.kind == expected,
                "check input {name} has an incompatible context source"
            );
        }
    }
    Ok(())
}

pub(super) fn resolve(
    schema: &CheckInputMap,
    explicit: &CheckInputValues,
    workspace: &std::path::Path,
    files: &[String],
    cases: &[super::CheckCase],
) -> Result<CheckInputValues> {
    for name in explicit.keys() {
        ensure!(schema.contains_key(name), "undeclared check input: {name}");
    }
    let mut values = BTreeMap::new();
    for (name, input) in schema {
        let value = explicit
            .get(name)
            .cloned()
            .or_else(|| {
                input.source.map(|source| match source {
                    CheckInputSource::Workspace => {
                        Value::String(workspace.to_string_lossy().into_owned())
                    }
                    CheckInputSource::ChangedFiles => serde_json::json!(files),
                    CheckInputSource::PlannedCases => serde_json::json!(cases),
                })
            })
            .or_else(|| input.default.clone());
        match value {
            Some(value) => {
                input.validate(name, &value)?;
                values.insert(name.clone(), value);
            }
            None if input.required => bail!("missing required check input: {name}"),
            None => {}
        }
    }
    Ok(values)
}

pub(super) fn argument(value: &Value) -> Result<String> {
    let text = match value {
        Value::String(text) => text.clone(),
        Value::Bool(_) | Value::Number(_) => value.to_string(),
        _ => bail!("command argument inputs must be scalar values"),
    };
    ensure!(!text.contains('\0'), "command argument contains NUL");
    Ok(text)
}
