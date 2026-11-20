use super::{CheckConfig, CheckScope};
use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
/// Selects coverage without allowing the caller to replace configured commands.
pub enum CheckSelection {
    #[default]
    All,
    Inputs {
        inputs: super::CheckInputValues,
    },
    Files {
        files: Vec<String>,
    },
    Tests {
        tests: super::adapter::TestSelection,
    },
    Paths {
        paths: super::adapter::PathSelection,
    },
}

impl CheckSelection {
    /// Canonicalizes repeated selectors and rejects paths outside the project.
    pub fn normalize(&mut self) -> Result<()> {
        match self {
            Self::All => {}
            Self::Inputs { .. } => {}
            Self::Tests { tests } => tests.normalize()?,
            Self::Paths { paths } => paths.normalize()?,
            Self::Files { files } => {
                for file in files.iter() {
                    super::config::relative_path(file)?;
                }
                files.sort();
                files.dedup();
            }
        }
        Ok(())
    }

    pub(crate) fn empty(&self) -> bool {
        match self {
            Self::All => false,
            Self::Inputs { .. } => false,
            Self::Files { files } => files.is_empty(),
            Self::Tests { .. } => false,
            Self::Paths { .. } => false,
        }
    }

    /// Recognize no applicable work in an implicit default without changing explicit all semantics.
    pub(super) fn default_empty(&self) -> bool {
        match self {
            Self::Tests { tests } => tests.is_all(),
            Self::Paths { paths } => paths.paths.is_empty(),
            Self::All | Self::Inputs { .. } | Self::Files { .. } => self.empty(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
/// Runs one saved check, with at most one explicit coverage override.
pub struct CheckRunRequest {
    pub check: CheckNames,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inputs: Option<super::CheckInputValues>,
    /// Explicit adapter item IDs and affected paths; empty arrays select all tests.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tests: Option<super::adapter::TestSelection>,
    /// Explicit paths for a diagnostic or formatting adapter; empty selects all.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub paths: Option<Vec<String>>,
    /// Request format apply or lint fix instead of read-only verification.
    #[serde(default)]
    pub apply: bool,
    #[serde(default)]
    pub all: bool,
}

#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(untagged)]
pub enum CheckNames {
    One(String),
    Many(Vec<String>),
}

impl TryFrom<CheckRunRequest> for RunRequest {
    type Error = anyhow::Error;

    fn try_from(request: CheckRunRequest) -> Result<Self> {
        let names = match request.check {
            CheckNames::One(name) => vec![name],
            CheckNames::Many(names) => names,
        };
        ensure!(
            !names.is_empty() && names.iter().all(|name| !name.is_empty()),
            "check is required"
        );
        ensure!(
            usize::from(request.inputs.is_some())
                + usize::from(request.tests.is_some())
                + usize::from(request.paths.is_some())
                + usize::from(request.all)
                <= 1,
            "choose inputs, tests, paths, or all, not more than one"
        );
        let selection = if let Some(inputs) = request.inputs {
            Some(CheckSelection::Inputs { inputs })
        } else if let Some(tests) = request.tests {
            Some(CheckSelection::Tests { tests })
        } else if let Some(paths) = request.paths {
            Some(CheckSelection::Paths {
                paths: super::adapter::PathSelection { paths },
            })
        } else if request.all {
            Some(CheckSelection::All)
        } else {
            None
        };
        let mut result = SelectedChecks {
            ids: names.clone(),
            purpose: if request.apply {
                super::CheckPurpose::Prepare
            } else {
                super::CheckPurpose::Verify
            },
            ..SelectedChecks::default()
        };
        if let Some(mut selection) = selection {
            selection.normalize()?;
            ensure!(
                !selection.empty(),
                "explicit check selection cannot be empty"
            );
            for name in names {
                result.selections.insert(name, selection.clone());
            }
        }
        Ok(Self::Selected(result))
    }
}

#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(try_from = "RunRequestWire", into = "RunRequestWire")]
#[schemars(with = "RunRequestWire")]
pub enum RunRequest {
    All,
    Selected(SelectedChecks),
}

#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
pub struct SelectedChecks {
    #[serde(default)]
    pub purpose: super::CheckPurpose,
    #[serde(default)]
    pub ids: Vec<String>,
    #[serde(default)]
    /// Adds explicit suites without excluding automatically applicable suites.
    pub include: Vec<String>,
    #[serde(default)]
    pub selections: BTreeMap<String, CheckSelection>,
}

#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
struct RunRequestWire {
    #[serde(default)]
    apply: bool,
    #[serde(default)]
    all: bool,
    #[serde(default)]
    ids: Vec<String>,
    #[serde(default)]
    include: Vec<String>,
    #[serde(default)]
    selections: BTreeMap<String, CheckSelection>,
}

impl TryFrom<RunRequestWire> for RunRequest {
    type Error = anyhow::Error;

    fn try_from(request: RunRequestWire) -> Result<Self> {
        if request.all {
            ensure!(
                request.ids.is_empty()
                    && request.include.is_empty()
                    && request.selections.is_empty()
                    && !request.apply,
                "choose all checks or explicit selections, not both"
            );
            Ok(Self::All)
        } else {
            ensure!(
                !request.apply || (!request.ids.is_empty() && request.include.is_empty()),
                "apply requires explicitly named checks"
            );
            Ok(Self::Selected(SelectedChecks {
                purpose: if request.apply {
                    super::CheckPurpose::Prepare
                } else {
                    super::CheckPurpose::Verify
                },
                ids: request.ids,
                include: request.include,
                selections: request.selections,
            }))
        }
    }
}

impl From<RunRequest> for RunRequestWire {
    fn from(request: RunRequest) -> Self {
        match request {
            RunRequest::All => Self {
                all: true,
                ..Self::default()
            },
            RunRequest::Selected(selection) => Self {
                apply: selection.purpose == super::CheckPurpose::Prepare,
                all: false,
                ids: selection.ids,
                include: selection.include,
                selections: selection.selections,
            },
        }
    }
}

impl Default for RunRequest {
    fn default() -> Self {
        Self::Selected(SelectedChecks::default())
    }
}

impl std::ops::Deref for RunRequest {
    type Target = SelectedChecks;

    fn deref(&self) -> &Self::Target {
        static EMPTY: SelectedChecks = SelectedChecks {
            purpose: super::CheckPurpose::Verify,
            ids: Vec::new(),
            include: Vec::new(),
            selections: BTreeMap::new(),
        };
        match self {
            Self::All => &EMPTY,
            Self::Selected(selection) => selection,
        }
    }
}

impl RunRequest {
    pub(crate) fn is_all(&self) -> bool {
        matches!(self, Self::All)
    }
    /// Resolve category defaults from the task's changed paths and explicit adapter selectors.
    pub(crate) fn selections(
        &self,
        config: &CheckConfig,
        inventory: &super::ChangeInventory,
    ) -> Result<BTreeMap<String, CheckSelection>> {
        let known: BTreeSet<_> = config
            .definitions
            .iter()
            .map(|definition| definition.id.as_str())
            .collect();
        ensure!(
            self.ids.iter().all(|id| known.contains(id.as_str())),
            "unknown check id"
        );
        ensure!(
            self.selections
                .keys()
                .all(|id| known.contains(id.as_str())
                    && (self.ids.is_empty() || self.ids.contains(id))),
            "selection is outside the requested checks"
        );
        let mut result = BTreeMap::new();
        for definition in &config.definitions {
            if !self.ids.is_empty() && !self.ids.contains(&definition.id) {
                continue;
            }
            if !definition.enabled {
                result.insert(definition.id.clone(), CheckSelection::All);
                continue;
            }
            let mut selection = if self.is_all() {
                CheckSelection::All
            } else if let Some(selection) = self.selections.get(&definition.id) {
                selection.clone()
            } else {
                match definition.scope {
                    CheckScope::All => CheckSelection::All,
                    CheckScope::Paths | CheckScope::Tests => {
                        ensure!(
                            inventory.tracked(),
                            "{} requires declared inputs or full context outside a task execution",
                            definition.id
                        );
                        let paths = inventory
                            .files()
                            .iter()
                            .filter(|file| definition.trigger.matches_path(file))
                            .cloned()
                            .collect();
                        match definition.scope {
                            CheckScope::Tests => CheckSelection::Tests {
                                tests: super::adapter::TestSelection {
                                    ids: Vec::new(),
                                    affected: paths,
                                },
                            },
                            CheckScope::Paths if definition.runner.command().is_some() => {
                                CheckSelection::Files { files: paths }
                            }
                            CheckScope::Paths => CheckSelection::Paths {
                                paths: super::adapter::PathSelection { paths },
                            },
                            CheckScope::All => {
                                unreachable!("all scope is handled before path selection")
                            }
                        }
                    }
                }
            };
            selection.normalize()?;
            match (&definition.runner, &selection) {
                (
                    super::CheckRunner::Command(_),
                    CheckSelection::Tests { .. } | CheckSelection::Paths { .. },
                ) => {
                    anyhow::bail!(
                        "{}: structured selection requires an adapter",
                        definition.id
                    )
                }
                (super::CheckRunner::Adapter(_), CheckSelection::Inputs { .. }) => anyhow::bail!(
                    "{}: adapter checks require tests or all, not custom inputs",
                    definition.id
                ),
                (super::CheckRunner::Adapter(_), CheckSelection::Files { .. }) => anyhow::bail!(
                    "{}: adapter checks require tests, paths, or all",
                    definition.id
                ),
                _ => {}
            }
            match (&selection, definition.category) {
                (CheckSelection::Tests { .. }, category)
                    if matches!(definition.runner, super::CheckRunner::Adapter(_))
                        && category != super::CheckCategory::Test =>
                {
                    anyhow::bail!(
                        "{}: test selection requires the test category",
                        definition.id
                    )
                }
                (
                    CheckSelection::Paths { .. },
                    super::CheckCategory::Test | super::CheckCategory::Custom,
                ) => anyhow::bail!(
                    "{}: paths selection requires a path-category adapter",
                    definition.id
                ),
                _ => {}
            }
            result.insert(definition.id.clone(), selection);
        }
        Ok(result)
    }
}

#[cfg(test)]
mod adapter_selection_tests {
    use super::*;

    /// Preserve the requested operation through public selection and owner receipt encoding.
    #[test]
    fn apply_round_trips_without_selecting_every_check() {
        let request: CheckRunRequest = serde_json::from_value(serde_json::json!({
            "check":["format","lint"], "apply":true, "all":true
        }))
        .unwrap();
        let request = RunRequest::try_from(request).unwrap();
        assert!(!request.is_all());
        assert_eq!(request.purpose, super::super::CheckPurpose::Prepare);
        assert_eq!(request.selections.len(), 2);
        assert!(
            request
                .selections
                .values()
                .all(|selection| *selection == CheckSelection::All)
        );
        let encoded = serde_json::to_value(&request).unwrap();
        let restored: RunRequest = serde_json::from_value(encoded).unwrap();
        assert_eq!(restored.purpose, super::super::CheckPurpose::Prepare);
        assert_eq!(restored.ids, request.ids);
        assert_eq!(restored.selections, request.selections);
        for invalid in [
            serde_json::json!({"apply":true,"all":true}),
            serde_json::json!({"apply":true,"include":["format"]}),
            serde_json::json!({"apply":true,"ids":["format"],"include":["lint"]}),
        ] {
            assert!(serde_json::from_value::<RunRequest>(invalid).is_err());
        }
    }

    /// Validate the entire selected set before any adapter receives incompatible selectors.
    #[test]
    fn paths_are_normalized_and_category_checked_for_every_selected_check() {
        let config: CheckConfig = serde_json::from_value(serde_json::json!({
            "compiler":{"action":{"category":"typecheck","adapter":{"script":{"command":["nu","compiler.nu"]}}}},
            "unit":{"action":{"category":"test","adapter":{"script":{"command":["nu","test.nu"]}}}}
        })).unwrap();
        let request: CheckRunRequest = serde_json::from_value(
            serde_json::json!({"check":"compiler","paths":["src/b.rs","src/a.rs","src/a.rs"]}),
        )
        .unwrap();
        let request = RunRequest::try_from(request).unwrap();
        let selected = request
            .selections(&config, &crate::checks::ChangeInventory::Untracked)
            .unwrap();
        assert_eq!(
            selected["compiler"],
            CheckSelection::Paths {
                paths: super::super::adapter::PathSelection {
                    paths: vec!["src/a.rs".into(), "src/b.rs".into()]
                }
            }
        );
        for value in [
            serde_json::json!({"check":["compiler","unit"],"paths":["src/a.rs"]}),
            serde_json::json!({"check":"compiler","tests":{}}),
        ] {
            let request =
                RunRequest::try_from(serde_json::from_value::<CheckRunRequest>(value).unwrap())
                    .unwrap();
            assert!(
                request
                    .selections(&config, &crate::checks::ChangeInventory::Untracked)
                    .is_err()
            );
        }
        let request = RunRequest::try_from(
            serde_json::from_value::<CheckRunRequest>(
                serde_json::json!({"check":"compiler","paths":[],"all":true}),
            )
            .unwrap(),
        );
        assert!(request.is_err());
    }

    /// Preserve opaque IDs while deriving implicit affected-path selections without widening emptiness.
    #[test]
    fn adapter_ids_and_implicit_affected_paths_keep_distinct_empty_semantics() {
        let config: CheckConfig = serde_json::from_value(serde_json::json!({
            "unit":{"action":{"category":"test","adapter":{"script":{"command":["nu","tests.nu"]}}}}
        }))
        .unwrap();
        let selection = CheckSelection::Tests {
            tests: super::super::adapter::TestSelection {
                ids: vec!["opaque ID".to_owned().try_into().unwrap()],
                affected: Vec::new(),
            },
        };
        let request = RunRequest::Selected(SelectedChecks {
            ids: vec!["unit".into()],
            selections: [("unit".into(), selection.clone())].into(),
            ..Default::default()
        });
        assert_eq!(
            request
                .selections(&config, &crate::checks::ChangeInventory::Known(Vec::new()))
                .unwrap()["unit"],
            selection
        );
        let request = RunRequest::default();
        let empty = request
            .selections(&config, &crate::checks::ChangeInventory::Known(Vec::new()))
            .unwrap();
        assert!(empty["unit"].default_empty());
        let selected = request
            .selections(
                &config,
                &crate::checks::ChangeInventory::Known(vec!["tests/unit.lua".into()]),
            )
            .unwrap();
        assert_eq!(
            selected["unit"],
            CheckSelection::Tests {
                tests: super::super::adapter::TestSelection {
                    ids: Vec::new(),
                    affected: vec!["tests/unit.lua".into()]
                }
            }
        );
        assert!(!selected["unit"].default_empty());
    }

    #[test]
    fn empty_explicit_test_arrays_mean_all_and_conflicts_are_rejected() {
        let request: CheckRunRequest =
            serde_json::from_value(serde_json::json!({"check":"unit","tests":{}})).unwrap();
        let resolved = RunRequest::try_from(request).unwrap();
        let CheckSelection::Tests { tests } = &resolved.selections["unit"] else {
            panic!("missing explicit test selection")
        };
        assert!(tests.is_all());
        assert!(!resolved.selections["unit"].empty());
        for conflicting in [
            serde_json::json!({"check":"unit","tests":{},"all":true}),
            serde_json::json!({"check":"unit","tests":{},"inputs":{}}),
        ] {
            let request: CheckRunRequest = serde_json::from_value(conflicting).unwrap();
            assert!(RunRequest::try_from(request).is_err());
        }
    }
}
