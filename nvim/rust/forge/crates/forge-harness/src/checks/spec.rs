use super::{
    CheckCategory, CheckConfig, CheckDefinition, CheckInputMap, CheckOperation, CheckRunner,
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub command: Option<Command>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub adapter: Option<super::adapter::AdapterConfig>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inputs: Option<CheckInputMap>,
    #[serde(default = "root", skip_serializing_if = "is_root")]
    pub directory: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[schemars(required)]
    pub category: Option<CheckCategory>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub prepare: bool,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    #[schemars(skip)]
    pub input: bool,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub after: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Check {
    /// Configured enablement before task-specific overrides.
    #[serde(default = "enabled_default", skip_serializing_if = "is_enabled")]
    pub enabled: bool,
    #[serde(default)]
    pub trigger: Trigger,
    pub action: Action,
}

/// Keep existing authored checks enabled when the setting is omitted.
fn enabled_default() -> bool {
    true
}

/// Omit the default enabled state without losing explicitly disabled definitions.
fn is_enabled(enabled: &bool) -> bool {
    *enabled
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
            let category = action
                .category
                .ok_or_else(|| anyhow::anyhow!("{id}: action.category is required; choose typecheck, test, format, lint, build, or custom"))?;
            ensure!(
                !action.input,
                "{id}: input callbacks are removed; use an adapter or a custom command with named argv inputs"
            );
            ensure!(
                action.command.is_some() != action.adapter.is_some(),
                "{id}: action requires exactly one of command or adapter"
            );
            if let Some(adapter) = action.adapter {
                ensure!(
                    action.inputs.is_none() && !action.input && !action.prepare,
                    "{id}: adapter checks reject inputs, input, and prepare"
                );
                let trigger = if check.trigger.paths.is_empty() && check.trigger.exclude.is_empty()
                {
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
                definitions.push(CheckDefinition {
                    id: id.clone(),
                    name: action.label.unwrap_or(id),
                    category,
                    enabled: check.enabled,
                    auto: check.trigger.auto,
                    area: action.directory.clone(),
                    trigger,
                    scope: if category == CheckCategory::Test {
                        CheckScope::Tests
                    } else {
                        CheckScope::Paths
                    },
                    directory: action.directory,
                    runner: CheckRunner::Adapter(adapter),
                    depends_on: action.after,
                });
                continue;
            }
            let (command, args) = match action
                .command
                .expect("command required when adapter is absent")
            {
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
                prepare: action.prepare,
                command,
                args,
                inputs: action.inputs,
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
                    inputs
                        .values()
                        .any(|input| input.source == Some(super::CheckInputSource::ChangedFiles))
                })
                .unwrap_or_default();
            let scope = if source {
                CheckScope::Paths
            } else {
                CheckScope::All
            };
            definitions.push(CheckDefinition {
                id: id.clone(),
                name: action.label.unwrap_or(id),
                category,
                enabled: check.enabled,
                auto: check.trigger.auto,
                area: action.directory.clone(),
                trigger,
                scope,
                directory: action.directory,
                runner: CheckRunner::Command(operation),
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
                let operation = definition.runner.command();
                let command = operation.map(|operation| match &operation.args {
                    Some(args) => Command::Arguments(
                        std::iter::once(operation.command.clone())
                            .chain(args.iter().cloned())
                            .collect(),
                    ),
                    None => Command::Shell(operation.command.clone()),
                });
                let adapter = match &definition.runner {
                    CheckRunner::Adapter(adapter) => Some(adapter.clone()),
                    CheckRunner::Command(_) => None,
                };
                let (paths, exclude) = match definition.trigger {
                    CheckTrigger::Paths { include, exclude }
                    | CheckTrigger::FilesChanged { include, exclude } => (include, exclude),
                    _ => (Vec::new(), Vec::new()),
                };
                (
                    definition.id.clone(),
                    Check {
                        enabled: definition.enabled,
                        trigger: Trigger {
                            auto: definition.auto,
                            paths,
                            exclude,
                        },
                        action: Action {
                            command,
                            adapter,
                            inputs: operation.and_then(|operation| operation.inputs.clone()),
                            directory: definition.directory,
                            label: (definition.name != definition.id).then_some(definition.name),
                            category: Some(definition.category),
                            prepare: operation.is_some_and(|operation| operation.prepare),
                            input: false,
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

    /// Preserve authored preparation policy inside the custom operation and reject obsolete records.
    #[test]
    fn custom_preparation_policy_round_trips_in_the_runner() {
        for prepare in [false, true] {
            let authored = serde_json::json!({"custom":{"action":{
                "category":"custom", "command":["unused"], "prepare":prepare
            }}});
            let config: CheckConfig = serde_json::from_value(authored).unwrap();
            let operation = config.definitions[0].runner.command().unwrap();
            assert_eq!(operation.prepare, prepare);
            assert_eq!(
                operation.purpose(),
                if prepare {
                    crate::checks::CheckPurpose::Prepare
                } else {
                    crate::checks::CheckPurpose::Verify
                }
            );
            let encoded = serde_json::to_value(&config).unwrap();
            assert_eq!(
                encoded["custom"]["action"]["prepare"]
                    .as_bool()
                    .unwrap_or(false),
                prepare
            );
            let restored: CheckConfig = serde_json::from_value(encoded).unwrap();
            assert_eq!(restored, config);
            let definition = serde_json::to_value(&config.definitions[0]).unwrap();
            assert!(definition.get("purpose").is_none());
            assert_eq!(definition["runner"]["command"]["prepare"], prepare);
            let mut obsolete = definition.clone();
            obsolete["purpose"] = "verify".into();
            assert!(serde_json::from_value::<CheckDefinition>(obsolete).is_err());
            let mut incomplete = definition;
            incomplete["runner"]["command"]
                .as_object_mut()
                .unwrap()
                .remove("prepare");
            assert!(serde_json::from_value::<CheckDefinition>(incomplete).is_err());
        }
    }

    /// Preserve configured disabling while keeping omitted enablement compatible with authored files.
    #[test]
    fn configured_enablement_round_trips_without_resetting_disabled_checks() {
        for enabled in [true, false] {
            let mut value =
                serde_json::json!({"unit":{"action":{"category":"custom","command":["unit"]}}});
            if !enabled {
                value["unit"]["enabled"] = false.into();
            }
            let config: CheckConfig = serde_json::from_value(value).unwrap();
            assert_eq!(config.definitions[0].enabled, enabled);
            assert!(config.validate_selection(&["unit".into()]).is_ok());
            assert!(config.validate_selection(&["missing".into()]).is_err());
            let encoded = serde_json::to_value(config).unwrap();
            assert_eq!(
                encoded["unit"].get("enabled"),
                (!enabled).then_some(&serde_json::Value::Bool(false))
            );
        }
    }

    /// Require an explicit category and preserve the runner required by its contract.
    #[test]
    fn categories_are_explicit_and_round_trip_through_the_edit_schema() {
        for category in ["typecheck", "test", "format", "lint", "build", "custom"] {
            let runner = if category == "custom" {
                serde_json::json!({"command":["unit"]})
            } else {
                serde_json::json!({"adapter":{"script":{"command":["nu","unit.nu"]}}})
            };
            let mut value = serde_json::json!({"unit":{"action":runner}});
            value["unit"]["action"]["category"] = category.into();
            let config: CheckConfig = serde_json::from_value(value).unwrap();
            assert_eq!(
                serde_json::to_value(config).unwrap()["unit"]["action"]["category"],
                category
            );
        }
        let error = serde_json::from_value::<CheckConfig>(
            serde_json::json!({"unit":{"action":{"command":["unit"]}}}),
        )
        .unwrap_err()
        .to_string();
        assert!(
            error.contains("unit: action.category is required"),
            "{error}"
        );
        let schema = serde_json::to_value(schemars::schema_for!(Action)).unwrap();
        assert!(
            schema["required"]
                .as_array()
                .unwrap()
                .contains(&serde_json::json!("category"))
        );
    }

    /// Reject opaque commands in contract categories without changing authored settings.
    #[test]
    fn contract_categories_require_adapters_at_load_and_internal_validation() {
        for category in ["typecheck", "test", "format", "lint", "build"] {
            let root = tempfile::tempdir().unwrap();
            let path = root.path().join(".forge.json");
            let source = serde_json::to_vec(&serde_json::json!({"checks":{
                "unit":{"action":{"category":category,"command":["unit"]}}
            }}))
            .unwrap();
            std::fs::write(&path, &source).unwrap();
            let error = super::super::ConfigSnapshot::load(root.path()).unwrap_err();
            let message = format!("{error:#}");
            assert!(
                message.contains(&format!("unit: category `{category}` requires an adapter")),
                "{message}"
            );
            assert!(message.contains("check_definition_list"), "{message}");
            assert_eq!(std::fs::read(&path).unwrap(), source);

            let mut config: CheckConfig = serde_json::from_value(adapter()).unwrap();
            config.definitions[0].category = serde_json::from_value(category.into()).unwrap();
            config.definitions[0].runner = CheckRunner::Command(CheckOperation {
                prepare: false,
                command: "unit".into(),
                args: None,
                inputs: None,
            });
            assert!(config.validate().is_err());
        }
        let mut value = adapter();
        value["unit"]["action"]["category"] = "custom".into();
        let error = serde_json::from_value::<CheckConfig>(value)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("unit: category `custom` requires a plain command"),
            "{error}"
        );
        assert!(error.contains("check_definition_list"), "{error}");
    }

    /// Preserve the adapter's rejection cause through Serde's configuration boundary.
    #[test]
    fn adapter_configuration_errors_name_the_check_and_original_cause() {
        for (category, options, cause) in [
            (
                "test",
                serde_json::json!({"script":{"command":[]}}),
                "script command requires a nonempty executable",
            ),
            (
                "format",
                serde_json::json!({"rust_nextest":{}}),
                "rust_nextest requires the test category",
            ),
        ] {
            let value =
                serde_json::json!({"unit":{"action":{"category":category,"adapter":options}}});
            let message = serde_json::from_value::<CheckConfig>(value)
                .unwrap_err()
                .to_string();
            assert!(message.contains("unit: invalid adapter:"), "{message}");
            assert!(message.contains(cause), "{message}");
            assert!(message.contains("check_definition_list"), "{message}");
        }
    }

    /// Explain the removed spelling without rewriting user-authored configuration.
    #[test]
    fn legacy_category_error_names_the_check_and_preserves_its_source() {
        let root = tempfile::tempdir().unwrap();
        let path = root.path().join(".forge.json");
        let source =
            br#"{"checks":{"compiler":{"action":{"category":"check","command":["compiler"]}}}}"#;
        std::fs::write(&path, source).unwrap();
        let error = super::super::ConfigSnapshot::load(root.path())
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("compiler: category `check` was renamed to `typecheck`"),
            "{error}"
        );
        assert_eq!(std::fs::read(path).unwrap(), source);
    }

    /// Reject removed callback transport without silently rewriting authored settings.
    #[test]
    fn legacy_callback_input_is_rejected_and_absent_from_the_edit_schema() {
        let root = tempfile::tempdir().unwrap();
        let path = root.path().join(".forge.json");
        let source = br#"{"checks":{"legacy":{"action":{"category":"custom","command":["script"],"input":true}}}}"#;
        std::fs::write(&path, source).unwrap();
        let error = super::super::ConfigSnapshot::load(root.path()).unwrap_err();
        let message = format!("{error:#}");
        assert!(
            message.contains("legacy: input callbacks are removed"),
            "{message}"
        );
        assert!(message.contains("named argv inputs"), "{message}");
        assert_eq!(std::fs::read(path).unwrap(), source);
        let schema = serde_json::to_value(schemars::schema_for!(Action)).unwrap();
        assert!(schema["properties"].get("input").is_none());
    }

    /// Create a configured test adapter at the external configuration boundary.
    fn adapter() -> serde_json::Value {
        serde_json::json!({"unit":{"action":{"category":"test","adapter":{"script":{"command":["nu","tests.nu"]}}}}})
    }

    #[test]
    fn adapter_configuration_round_trips_without_command_fields() {
        let original = adapter();
        let config: CheckConfig = serde_json::from_value(original).unwrap();
        assert!(matches!(
            config.definitions[0].runner,
            CheckRunner::Adapter(_)
        ));
        let encoded = serde_json::to_value(config).unwrap();
        assert!(encoded["unit"]["action"].get("command").is_none());
        assert_eq!(
            encoded["unit"]["action"]["adapter"]["script"]["command"][1],
            "tests.nu"
        );
    }

    #[test]
    fn adapters_reject_ambiguous_or_category_incompatible_configuration() {
        for (field, value) in [
            ("command", serde_json::json!(["other"])),
            ("input", serde_json::json!(true)),
            ("inputs", serde_json::json!({})),
            ("prepare", serde_json::json!(true)),
            ("category", serde_json::json!("custom")),
        ] {
            let mut config = adapter();
            config["unit"]["action"][field] = value;
            assert!(
                serde_json::from_value::<CheckConfig>(config).is_err(),
                "accepted {field}"
            );
        }
    }
}
