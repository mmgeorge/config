use super::{CheckConfig, CheckScope};
use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(deny_unknown_fields)]
/// Identifies a runner-resolvable test case and its project-relative source file.
pub struct CheckCase {
    pub file: String,
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Optional executable target, such as lib, bin:cli, or test:gameplay.
    pub target: Option<String>,
}

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
    Cases {
        cases: Vec<CheckCase>,
    },
}

impl CheckSelection {
    /// Canonicalizes repeated selectors and rejects paths outside the project.
    pub fn normalize(&mut self) -> Result<()> {
        match self {
            Self::All => {}
            Self::Inputs { .. } => {}
            Self::Files { files } => {
                for file in files.iter() {
                    super::config::relative_path(file)?;
                }
                files.sort();
                files.dedup();
            }
            Self::Cases { cases } => {
                for case in cases.iter() {
                    super::config::relative_path(&case.file)?;
                    ensure!(
                        !case.name.is_empty() && !case.name.contains('\0'),
                        "invalid test case name"
                    );
                    ensure!(
                        case.target
                            .as_ref()
                            .is_none_or(|target| !target.is_empty()
                                && !target.chars().any(char::is_control)),
                        "invalid test target"
                    );
                }
                cases.sort();
                cases.dedup();
            }
        }
        Ok(())
    }

    pub(crate) fn empty(&self) -> bool {
        match self {
            Self::All => false,
            Self::Inputs { .. } => false,
            Self::Files { files } => files.is_empty(),
            Self::Cases { cases } => cases.is_empty(),
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
            !(request.inputs.is_some() && request.all),
            "choose inputs or all, not both"
        );
        let selection = if let Some(inputs) = request.inputs {
            Some(CheckSelection::Inputs { inputs })
        } else if request.all {
            Some(CheckSelection::All)
        } else {
            None
        };
        let mut result = SelectedChecks {
            ids: names.clone(),
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
                    && request.selections.is_empty(),
                "choose all checks or explicit selections, not both"
            );
            Ok(Self::All)
        } else {
            Ok(Self::Selected(SelectedChecks {
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
    /// Resolves configured defaults from the task's actual files and accepted changed-case inventory.
    pub(crate) fn selections(
        &self,
        config: &CheckConfig,
        files: &[String],
        cases: Option<&[CheckCase]>,
        new_cases: Option<&[CheckCase]>,
        has_scope: bool,
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
                    CheckScope::ChangedFiles => {
                        ensure!(
                            has_scope,
                            "{} requires declared inputs or full context outside a task execution",
                            definition.id
                        );
                        CheckSelection::Files {
                            files: files
                                .iter()
                                .filter(|file| definition.trigger.matches_path(file))
                                .cloned()
                                .collect(),
                        }
                    }
                    CheckScope::ChangedCases | CheckScope::NewCases => {
                        ensure!(
                            has_scope,
                            "{} requires declared inputs or full context outside plan execution",
                            definition.id
                        );
                        CheckSelection::Cases {
                            cases: (if definition.scope == CheckScope::NewCases {
                                new_cases
                            } else {
                                cases
                            })
                            .unwrap_or_default()
                            .iter()
                            .filter(|case| definition.trigger.matches_path(&case.file))
                            .cloned()
                            .collect(),
                        }
                    }
                }
            };
            selection.normalize()?;
            result.insert(definition.id.clone(), selection);
        }
        Ok(result)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Proves the scoped handler resolved real coverage rather than accepting an empty test filter.
pub struct CheckReceipt {
    pub version: u32,
    pub selection: CheckSelection,
}

impl CheckReceipt {
    /// Validates a handler receipt before its successful process status can count as coverage.
    pub(crate) fn validate(&mut self) -> Result<()> {
        ensure!(self.version == 1, "unsupported selection receipt version");
        self.selection.normalize()?;
        ensure!(
            !self.selection.empty(),
            "requested scope resolved to no tests or a broader suite"
        );
        Ok(())
    }

    /// Rejects receipts that widen requested files or replace requested cases with file-wide coverage.
    pub(crate) fn validate_request(&mut self, requested: &CheckSelection) -> Result<()> {
        self.validate()?;
        let contained = match (requested, &self.selection) {
            (
                CheckSelection::Inputs { inputs: requested },
                CheckSelection::Inputs { inputs: resolved },
            ) => requested == resolved,
            (CheckSelection::All, _) => true,
            (
                CheckSelection::Files { files: requested },
                CheckSelection::Files { files: resolved },
            ) => resolved.iter().all(|file| requested.contains(file)),
            (
                CheckSelection::Files { files: requested },
                CheckSelection::Cases { cases: resolved },
            ) => resolved.iter().all(|case| requested.contains(&case.file)),
            (
                CheckSelection::Cases { cases: requested },
                CheckSelection::Cases { cases: resolved },
            ) => resolved.iter().all(|case| requested.contains(case)),
            _ => false,
        };
        ensure!(
            contained,
            "selection receipt broadens the requested coverage; fix the scoped handler"
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn all_and_selected_requests_preserve_the_wire_contract() {
        for request in [
            RunRequest::All,
            RunRequest::Selected(SelectedChecks {
                ids: vec!["unit".into()],
                ..Default::default()
            }),
        ] {
            let encoded = serde_json::to_value(&request).unwrap();
            let decoded: RunRequest = serde_json::from_value(encoded.clone()).unwrap();
            assert_eq!(serde_json::to_value(decoded).unwrap(), encoded);
        }
        for field in ["ids", "include", "selections"] {
            let mut value = serde_json::json!({"all": true});
            value[field] = if field == "selections" {
                serde_json::json!({"unit":{"kind":"all"}})
            } else {
                serde_json::json!(["unit"])
            };
            assert!(serde_json::from_value::<RunRequest>(value).is_err());
        }
    }
}
