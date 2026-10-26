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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn declared_inputs_validate_types_defaults_context_and_unknown_names() -> Result<()> {
        let schema: CheckInputMap = serde_json::from_value(serde_json::json!({
            "package": {"type":"string","description":"Repository package","required":true},
            "paths": {"type":"array","description":"Changed source paths","source":"changed_files"},
            "attempt": {"type":"integer","description":"Attempt number","default":1}
        }))?;
        validate_schema(&schema)?;
        let workspace = std::path::Path::new(".");
        let explicit = BTreeMap::from([("package".into(), Value::String("engine".into()))]);
        let resolved = resolve(&schema, &explicit, workspace, &["src/lib.rs".into()], &[])?;
        assert_eq!(resolved["package"], "engine");
        assert_eq!(resolved["attempt"], 1);
        assert_eq!(resolved["paths"], serde_json::json!(["src/lib.rs"]));
        assert!(resolve(&schema, &BTreeMap::new(), workspace, &[], &[]).is_err());
        assert!(
            resolve(
                &schema,
                &BTreeMap::from([("unknown".into(), Value::Bool(true))]),
                workspace,
                &[],
                &[]
            )
            .is_err()
        );
        assert!(
            resolve(
                &schema,
                &BTreeMap::from([("package".into(), Value::Bool(true))]),
                workspace,
                &[],
                &[]
            )
            .is_err()
        );
        Ok(())
    }

    #[test]
    fn explicit_values_override_sources_and_defaults_without_losing_other_context() -> Result<()> {
        let schema: CheckInputMap = serde_json::from_value(serde_json::json!({
            "paths":{"type":"array","description":"Paths","source":"changed_files","default":["default"]},
            "cases":{"type":"array","description":"Cases","source":"planned_cases"},
            "root":{"type":"string","description":"Root","source":"workspace"}
        }))?;
        validate_schema(&schema)?;
        let cases = [super::super::CheckCase {
            file: "tests/selected.rs".into(),
            name: "selected".into(),
            target: None,
        }];
        let explicit = BTreeMap::from([("paths".into(), serde_json::json!(["explicit"]))]);
        let values = resolve(
            &schema,
            &explicit,
            std::path::Path::new("."),
            &["changed".into()],
            &cases,
        )?;
        assert_eq!(values["paths"], serde_json::json!(["explicit"]));
        assert_eq!(values["cases"], serde_json::json!(cases));
        assert_eq!(values["root"], ".");
        let values = resolve(
            &schema,
            &BTreeMap::new(),
            std::path::Path::new("."),
            &[],
            &[],
        )?;
        assert_eq!(values["paths"], serde_json::json!([]));
        Ok(())
    }

    #[test]
    fn schemas_reject_invalid_defaults_sources_names_and_argument_values() -> Result<()> {
        for value in [
            serde_json::json!({"input":{"type":"integer","description":"Count","default":"wrong"}}),
            serde_json::json!({"input":{"type":"string","description":"Paths","source":"changed_files"}}),
            serde_json::json!({"input":{"type":"string","description":""}}),
            serde_json::json!({"1input":{"type":"string","description":"Value"}}),
        ] {
            let schema = serde_json::from_value(value)?;
            assert!(validate_schema(&schema).is_err());
        }
        for value in [
            serde_json::json!({}),
            serde_json::json!([]),
            Value::Null,
            serde_json::json!("bad\0argument"),
        ] {
            assert!(argument(&value).is_err());
        }
        Ok(())
    }
}
