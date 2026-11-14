use super::{CheckInputValues, CheckOperation, CheckSelection};
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, path::Path};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CheckInvocation {
    pub inputs: CheckInputValues,
    pub program: String,
    pub args: Vec<String>,
    pub directory: String,
    pub files: Vec<String>,
}

impl CheckInvocation {
    pub fn display(&self) -> String {
        display(&self.program, &self.args)
    }
}

pub(super) fn validate(operation: &CheckOperation) -> Result<()> {
    let empty = BTreeMap::new();
    let schema = operation.inputs.as_ref().unwrap_or(&empty);
    super::inputs::validate_schema(schema)?;
    if let Some(args) = &operation.args {
        ensure!(
            !operation.command.contains(['{', '}']),
            "executable cannot contain input bindings"
        );
        for argument in args {
            ensure!(!argument.contains('\0'), "command argument contains NUL");
            if let Some(name) = binding(argument) {
                ensure!(
                    schema.contains_key(name),
                    "undeclared command input: {name}"
                );
            }
        }
    } else {
        ensure!(
            schema.is_empty(),
            "shell commands cannot bind named inputs; use an argv command with complete {{name}} arguments"
        );
    }
    Ok(())
}

fn binding(argument: &str) -> Option<&str> {
    let name = argument.strip_prefix('{')?.strip_suffix('}')?;
    (!name.is_empty()
        && name
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_'))
    .then_some(name)
}

pub(super) fn display(program: &str, args: &[String]) -> String {
    std::iter::once(program)
        .chain(args.iter().map(String::as_str))
        .map(|value| {
            if !value.is_empty()
                && value.chars().all(|character| {
                    character.is_ascii_alphanumeric() || "_./:-=".contains(character)
                })
            {
                value.to_owned()
            } else {
                format!("\"{}\"", value.replace('`', "``").replace('"', "`\""))
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

pub(super) fn resolve(
    workspace: &Path,
    directory: &str,
    operation: &CheckOperation,
    selection: &CheckSelection,
    changed_files: &[String],
) -> Result<Vec<CheckInvocation>> {
    validate(operation)?;
    super::config::relative_path(directory)?;
    let empty = BTreeMap::new();
    let explicit = match selection {
        CheckSelection::Inputs { inputs } => inputs,
        _ => &empty,
    };
    let files = match selection {
        CheckSelection::Files { files } => files.as_slice(),
        _ => changed_files,
    };
    let schema = BTreeMap::new();
    let inputs = super::inputs::resolve(
        operation.inputs.as_ref().unwrap_or(&schema),
        explicit,
        workspace,
        files,
    )?;
    let mut args = Vec::new();
    for argument in operation.args.as_deref().unwrap_or_default() {
        match binding(argument) {
            Some(name) => {
                let value = inputs
                    .get(name)
                    .ok_or_else(|| anyhow::anyhow!("missing command input: {name}"))?;
                if let Some(values) = value.as_array() {
                    for value in values {
                        args.push(super::inputs::argument(value)?);
                    }
                } else {
                    args.push(super::inputs::argument(value)?);
                }
            }
            None => args.push(argument.clone()),
        }
    }
    Ok(vec![CheckInvocation {
        inputs,
        program: operation.command.clone(),
        args,
        directory: directory.into(),
        files: if matches!(selection, CheckSelection::Files { .. }) {
            files.to_vec()
        } else {
            Vec::new()
        },
    }])
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Expand complete argv bindings while preserving shell metacharacters as literal values.
    #[test]
    fn custom_inputs_preserve_argument_boundaries_and_reject_shell_bindings() -> Result<()> {
        let config: super::super::CheckConfig = serde_json::from_value(serde_json::json!({
            "custom": {"action": {
                "category": "custom", "command": ["tool", "{values}", "prefix-{values}"],
                "inputs": {"values": {"type": "array", "description": "Literal values", "required": true}}
            }}
        }))?;
        let mut operation = config.definitions[0].runner.command().unwrap().clone();
        let selection = CheckSelection::Inputs {
            inputs: BTreeMap::from([(
                "values".into(),
                serde_json::json!([
                    "with spaces",
                    "$(never-execute); \"quoted\" & literal",
                    42,
                    true
                ]),
            )]),
        };
        let resolved = resolve(Path::new("."), ".", &operation, &selection, &[])?;
        assert_eq!(resolved.len(), 1);
        assert_eq!(
            resolved[0].args,
            [
                "with spaces",
                "$(never-execute); \"quoted\" & literal",
                "42",
                "true",
                "prefix-{values}"
            ]
        );
        operation.args = None;
        let error = validate(&operation).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("shell commands cannot bind named inputs")
        );
        Ok(())
    }
}
