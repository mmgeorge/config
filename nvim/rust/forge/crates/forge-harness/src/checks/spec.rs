use std::collections::BTreeMap;

use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::{
    CheckCategory, CheckConfig, CheckDefinition, CheckOperation, CheckPurpose, CheckScope,
    CheckTrigger, PackageManager,
};

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
    pub package_manager: Option<PackageManager>,
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

pub(super) fn package_manager(program: &str) -> Option<PackageManager> {
    match program.rsplit(['/', '\\']).next()?.trim_end_matches(".exe") {
        "cargo" => Some(PackageManager::Cargo),
        "npm" | "npm.cmd" => Some(PackageManager::Npm),
        "pnpm" | "pnpm.cmd" => Some(PackageManager::Pnpm),
        _ => None,
    }
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
            let manager = action.package_manager;
            ensure!(
                manager.is_none() || args.is_some(),
                "{id}: package_manager requires an argument-array command"
            );
            if let (Some(selected), Some(expected)) = (manager, package_manager(&command)) {
                ensure!(
                    selected == expected,
                    "{id}: package_manager does not match the action executable"
                );
            }
            let mut operation = CheckOperation {
                command,
                args,
                package_manager: manager,
                input: action.input,
            };
            if let Some(args) = &mut operation.args {
                for argument in args.iter_mut() {
                    ensure!(
                        !argument.contains("{file.")
                            && !argument.contains("{package.")
                            && !argument.contains("{case."),
                        "{id}: use {{path}} or {{package}} in action.command"
                    );
                    *argument = argument
                        .replace("{path}", "{file.path}")
                        .replace("{package}", "{package.name}");
                }
            }
            super::invocation::validate(&operation)?;
            let arguments = operation.args.as_deref().unwrap_or_default();
            let category = action.category.unwrap_or_else(|| {
                let verb = arguments
                    .iter()
                    .find(|argument| !argument.starts_with('+'))
                    .map(String::as_str);
                match (operation.command.as_str(), verb) {
                    ("rustfmt", _) | ("cargo", Some("fmt")) => CheckCategory::Format,
                    ("cargo", Some("test")) => CheckCategory::Test,
                    ("cargo", Some("clippy")) => CheckCategory::Lint,
                    ("cargo", Some("build")) => CheckCategory::Build,
                    _ => CheckCategory::Check,
                }
            });
            let cases = (manager == Some(PackageManager::Cargo)
                && package_manager(&operation.command) == Some(PackageManager::Cargo)
                && arguments.iter().any(|arg| arg == "test"))
            .then(|| super::invocation::case_operation(&operation));
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
            let scoped = operation.input
                || arguments
                    .iter()
                    .any(|arg| arg.contains("{file.path}") || arg.contains("{package.name}"));
            definitions.push(CheckDefinition {
                id: id.clone(),
                name: action.label.unwrap_or(id),
                category,
                purpose: if action.prepare {
                    CheckPurpose::Prepare
                } else {
                    CheckPurpose::Verify
                },
                enabled: true,
                auto: check.trigger.auto,
                area: action.directory.clone(),
                trigger,
                scope: if scoped {
                    CheckScope::ChangedFiles
                } else {
                    CheckScope::All
                },
                directory: action.directory,
                all: operation.clone(),
                files: Some(operation),
                cases,
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
                let operation = if definition.scope == CheckScope::ChangedFiles {
                    definition.files.as_ref().unwrap_or(&definition.all)
                } else {
                    &definition.all
                };
                let command = match &operation.args {
                    Some(args) => Command::Arguments(
                        std::iter::once(operation.command.clone())
                            .chain(args.iter().map(|arg| {
                                arg.replace("{file.path}", "{path}")
                                    .replace("{package.name}", "{package}")
                            }))
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
                            package_manager: operation.package_manager,
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
    fn mixed_actions_keep_independent_package_managers_and_default_to_manual() -> Result<()> {
        let config: CheckConfig = serde_json::from_value(json!({
            "rust": {"trigger":{"paths":["rust/**/*.rs"]},"action":{"directory":"rust","package_manager":"cargo","command":["cargo","test","--package","{package}"]}},
            "web": {"trigger":{"auto":true,"paths":["web/**/*.ts"]},"action":{"directory":"web","package_manager":"npm","command":["npm","test","--workspace","{package}"]}}
        }))?;
        assert!(!config.definitions[0].auto);
        assert!(config.definitions[1].auto);
        assert_eq!(
            config.definitions[0]
                .files
                .as_ref()
                .unwrap()
                .package_manager,
            Some(PackageManager::Cargo)
        );
        assert_eq!(
            config.definitions[1]
                .files
                .as_ref()
                .unwrap()
                .package_manager,
            Some(PackageManager::Npm)
        );
        let saved = serde_json::to_value(&config)?;
        assert_eq!(saved["rust"]["action"]["command"][3], "{package}");
        assert!(saved.get("definitions").is_none());
        Ok(())
    }

    #[test]
    fn package_bindings_reject_missing_mismatched_and_unknown_managers() {
        for action in [
            json!({"command":["cargo","test","--package","{package}"]}),
            json!({"command":["cargo","test","--package","{package}"],"package_manager":"npm"}),
            json!({"command":["cargo","test","--package","{package}"],"package_manager":"unknown"}),
            json!({"command":["cargo","test","{packge}"],"package_manager":"cargo"}),
        ] {
            assert!(
                serde_json::from_value::<CheckConfig>(json!({"tests":{"action":action}})).is_err()
            );
        }
        assert!(
            serde_json::from_value::<CheckConfig>(json!({"version":2,"definitions":[]})).is_err()
        );
    }

    #[test]
    fn check_requests_accept_one_explicit_selector_without_kind() -> Result<()> {
        for selector in [
            json!({"files":["src/lib.rs"]}),
            json!({"packages":["engine"]}),
            json!({"tests":["tests::works"]}),
            json!({"all":true}),
        ] {
            let mut request = json!({"check":"tests"});
            request
                .as_object_mut()
                .unwrap()
                .extend(selector.as_object().unwrap().clone());
            let request: super::super::CheckRunRequest = serde_json::from_value(request)?;
            let selected = super::super::RunRequest::try_from(request)?;
            assert!(selected.selections.contains_key("tests"));
        }
        let conflicting = serde_json::from_value::<super::super::CheckRunRequest>(
            json!({"check":"tests","files":["src/lib.rs"],"packages":["engine"]}),
        )?;
        assert!(super::super::RunRequest::try_from(conflicting).is_err());
        assert!(
            serde_json::from_value::<super::super::CheckRunRequest>(json!({"ids":["tests"]}))
                .is_err()
        );
        Ok(())
    }
}
