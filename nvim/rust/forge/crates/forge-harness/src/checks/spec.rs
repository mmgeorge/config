use super::{
    CheckCategory, CheckConfig, CheckDefinition, CheckInputMap, CheckOperation, CheckPurpose,
    CheckScope, CheckTrigger,
};
use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Trigger {
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub auto: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub paths: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub exclude: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Command {
    Arguments(Vec<String>),
    Shell(String),
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Action {
    pub command: Command,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inputs: Option<CheckInputMap>,
    #[serde(default = "root", skip_serializing_if = "is_root")]
    pub directory: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub category: Option<CheckCategory>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub prepare: bool,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub input: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub after: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Check {
    #[serde(default)]
    pub trigger: Trigger,
    pub action: Action,
}

fn root() -> String {
    ".".into()
}
fn is_root(value: &str) -> bool {
    value == "."
}

impl TryFrom<BTreeMap<String, Check>> for CheckConfig {
    type Error = anyhow::Error;

    fn try_from(checks: BTreeMap<String, Check>) -> Result<Self> {
        let mut definitions = Vec::new();
        for (id, check) in checks {
            let action = check.action;
            let (command, args) = match action.command {
                Command::Arguments(mut arguments) => {
                    ensure!(
                        !arguments.is_empty(),
                        "{id}: action.command cannot be empty"
                    );
                    (arguments.remove(0), Some(arguments))
                }
                Command::Shell(source) => (source, None),
            };
            let operation = CheckOperation {
                command,
                args,
                inputs: action.inputs,
                input: action.input,
            };
            super::invocation::validate(&operation)?;
            let trigger = if check.trigger.paths.is_empty() && check.trigger.exclude.is_empty() {
                CheckTrigger::AnyChange
            } else {
                CheckTrigger::Paths {
                    include: if check.trigger.paths.is_empty() {
                        vec!["**".into()]
                    } else {
                        check.trigger.paths
                    },
                    exclude: check.trigger.exclude,
                }
            };
            let source = operation
                .inputs
                .as_ref()
                .map(|inputs| {
                    (
                        inputs.values().any(|input| {
                            input.source == Some(super::CheckInputSource::ChangedFiles)
                        }),
                        inputs.values().any(|input| {
                            input.source == Some(super::CheckInputSource::PlannedCases)
                        }),
                    )
                })
                .unwrap_or_default();
            let scope = if source.1 {
                CheckScope::ChangedCases
            } else if operation.input || source.0 {
                CheckScope::ChangedFiles
            } else {
                CheckScope::All
            };
            definitions.push(CheckDefinition {
                id: id.clone(),
                name: action.label.unwrap_or(id),
                category: action.category.unwrap_or(CheckCategory::Check),
                purpose: if action.prepare {
                    CheckPurpose::Prepare
                } else {
                    CheckPurpose::Verify
                },
                enabled: true,
                auto: check.trigger.auto,
                area: action.directory.clone(),
                trigger,
                scope,
                directory: action.directory,
                all: operation.clone(),
                files: Some(operation.clone()),
                cases: Some(operation),
                depends_on: action.after,
            });
        }
        let config = Self {
            version: 2,
            definitions,
        };
        config.validate()?;
        Ok(config)
    }
}

impl From<CheckConfig> for BTreeMap<String, Check> {
    fn from(config: CheckConfig) -> Self {
        config
            .definitions
            .into_iter()
            .map(|definition| {
                let operation = &definition.all;
                let command = match &operation.args {
                    Some(args) => Command::Arguments(
                        std::iter::once(operation.command.clone())
                            .chain(args.iter().cloned())
                            .collect(),
                    ),
                    None => Command::Shell(operation.command.clone()),
                };
                let (paths, exclude) = match definition.trigger {
                    CheckTrigger::Paths { include, exclude }
                    | CheckTrigger::FilesChanged { include, exclude } => (include, exclude),
                    _ => (Vec::new(), Vec::new()),
                };
                (
                    definition.id.clone(),
                    Check {
                        trigger: Trigger {
                            auto: definition.auto,
                            paths,
                            exclude,
                        },
                        action: Action {
                            command,
                            inputs: operation.inputs.clone(),
                            directory: definition.directory,
                            label: (definition.name != definition.id).then_some(definition.name),
                            category: Some(definition.category),
                            prepare: definition.purpose == CheckPurpose::Prepare,
                            input: operation.input,
                            after: definition.depends_on,
                        },
                    },
                )
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn commands_do_not_infer_categories_or_rewrite_arguments() -> Result<()> {
        for program in ["cargo", "npm", "rustfmt", "custom"] {
            let value = json!({"check":{"action":{"command":[program,"test","--lib"]}}});
            let config: CheckConfig = serde_json::from_value(value)?;
            let definition = &config.definitions[0];
            assert_eq!(definition.category, CheckCategory::Check);
            assert!(!definition.auto);
            assert_eq!(definition.all.command, program);
            assert_eq!(definition.all.args.as_ref().unwrap(), &["test", "--lib"]);
            assert_eq!(definition.cases.as_ref(), Some(&definition.all));
        }
        Ok(())
    }

    #[test]
    fn input_contract_roundtrips_and_rejects_undeclared_bindings() -> Result<()> {
        let value = json!({"unit":{"action":{
            "command":["runner","{package}","{test}"],
            "inputs":{
                "package":{"type":"string","description":"Package","required":true},
                "test":{"type":"string","description":"Test","required":true}
            },
            "category":"test"
        }}});
        let config: CheckConfig = serde_json::from_value(value)?;
        let reopened: CheckConfig = serde_json::from_value(serde_json::to_value(&config)?)?;
        assert_eq!(config, reopened);
        assert!(
            serde_json::from_value::<CheckConfig>(json!({
                "unit":{"action":{"command":["runner","{undeclared}"]}}
            }))
            .is_err()
        );
        assert!(
            serde_json::from_value::<CheckConfig>(json!({
                "unit":{"action":{"command":["cargo","test"],"package_manager":"cargo"}}
            }))
            .is_err()
        );
        Ok(())
    }
}
