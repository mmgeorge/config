use super::{CheckCase, CheckInputValues, CheckOperation, CheckSelection};
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
    pub cases: Vec<CheckCase>,
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
            schema.is_empty() || operation.input,
            "shell commands with declared inputs must receive JSON on stdin"
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
    planned_cases: &[CheckCase],
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
    let cases = match selection {
        CheckSelection::Cases { cases } => cases.as_slice(),
        _ => planned_cases,
    };
    let schema = BTreeMap::new();
    let inputs = super::inputs::resolve(
        operation.inputs.as_ref().unwrap_or(&schema),
        explicit,
        workspace,
        files,
        cases,
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
        cases: if matches!(selection, CheckSelection::Cases { .. }) {
            cases.to_vec()
        } else {
            Vec::new()
        },
    }])
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn commands_are_opaque_and_named_inputs_preserve_argument_boundaries() -> Result<()> {
        for program in ["cargo", "npm", "nu", "custom-runner"] {
            let operation = CheckOperation {
                command: program.into(),
                args: Some(vec![
                    "test".into(),
                    "{package}".into(),
                    "{test}".into(),
                    "{flags}".into(),
                ]),
                input: false,
                inputs: Some(serde_json::from_value(json!({
                    "package":{"type":"string","description":"Package","required":true},
                    "test":{"type":"string","description":"Test","required":true},
                    "flags":{"type":"array","description":"Additional values","default":[]}
                }))?),
            };
            let selection = CheckSelection::Inputs {
                inputs: BTreeMap::from([
                    ("package".into(), json!("package with spaces")),
                    ("test".into(), json!("name; not a shell command")),
                    ("flags".into(), json!(["--exact", "one argument"])),
                ]),
            };
            let invocation = resolve(Path::new("."), ".", &operation, &selection, &[], &[])?;
            assert_eq!(invocation.len(), 1);
            assert_eq!(invocation[0].program, program);
            assert_eq!(
                invocation[0].args,
                [
                    "test",
                    "package with spaces",
                    "name; not a shell command",
                    "--exact",
                    "one argument"
                ]
            );
        }
        Ok(())
    }

    #[test]
    fn callback_inputs_are_resolved_without_executing_the_callback() -> Result<()> {
        let operation = CheckOperation {
            command: "nonexistent-callback".into(),
            args: Some(vec!["fixture".into()]),
            input: true,
            inputs: Some(serde_json::from_value(json!({
                "files":{"type":"array","description":"Changed files","source":"changed_files"}
            }))?),
        };
        let invocation = resolve(
            Path::new("."),
            ".",
            &operation,
            &CheckSelection::Files {
                files: vec!["a.rs".into(), "b.ts".into()],
            },
            &[],
            &[],
        )?;
        assert_eq!(invocation[0].inputs["files"], json!(["a.rs", "b.ts"]));
        assert_eq!(invocation[0].args, ["fixture"]);
        Ok(())
    }

    #[test]
    fn literal_json_and_unrecognized_program_names_remain_unchanged() -> Result<()> {
        let operation = CheckOperation {
            command: "missing-program".into(),
            args: Some(vec![r#"{"literal":true}"#.into(), "prefix-{name}".into()]),
            inputs: None,
            input: false,
        };
        let invocation = resolve(
            Path::new("."),
            ".",
            &operation,
            &CheckSelection::All,
            &[],
            &[],
        )?;
        assert_eq!(invocation[0].args, operation.args.unwrap());
        Ok(())
    }

    #[test]
    fn explicit_inputs_keep_file_and_plan_context_available() -> Result<()> {
        let operation = CheckOperation {
            command: "uninstalled-callback".into(),
            args: Some(Vec::new()),
            input: true,
            inputs: Some(serde_json::from_value(json!({
                "value":{"type":"string","description":"Explicit value","required":true},
                "paths":{"type":"array","description":"Paths","source":"changed_files"},
                "cases":{"type":"array","description":"Cases","source":"planned_cases"}
            }))?),
        };
        let cases = [CheckCase {
            file: "tests/selected.rs".into(),
            name: "selected".into(),
            target: None,
        }];
        let selection = CheckSelection::Inputs {
            inputs: BTreeMap::from([("value".into(), json!("explicit"))]),
        };
        let invocation = resolve(
            Path::new("."),
            ".",
            &operation,
            &selection,
            &["source.rs".into()],
            &cases,
        )?;
        assert_eq!(invocation[0].inputs["value"], "explicit");
        assert_eq!(invocation[0].inputs["paths"], json!(["source.rs"]));
        assert_eq!(invocation[0].inputs["cases"], json!(cases));
        assert!(invocation[0].cases.is_empty());
        assert!(invocation[0].files.is_empty());
        Ok(())
    }

    #[test]
    fn bindings_require_declared_values_and_never_substitute_shell_source() -> Result<()> {
        let mut operation = CheckOperation {
            command: "uninstalled-runner".into(),
            args: Some(vec!["{missing}".into()]),
            input: false,
            inputs: None,
        };
        assert!(validate(&operation).is_err());
        operation.args = None;
        operation.inputs = Some(serde_json::from_value(json!({
            "value":{"type":"string","description":"Value","default":"literal"}
        }))?);
        assert!(validate(&operation).is_err());
        operation.input = true;
        assert!(validate(&operation).is_ok());
        Ok(())
    }
}
