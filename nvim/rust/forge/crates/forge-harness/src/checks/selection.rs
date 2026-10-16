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
            Self::Files { files } => {
                ensure!(files.len() <= 4096, "selection exceeds 4096 files");
                for file in files.iter() {
                    super::config::relative_path(file)?;
                }
                files.sort();
                files.dedup();
            }
            Self::Cases { cases } => {
                ensure!(cases.len() <= 4096, "selection exceeds 4096 cases");
                for case in cases.iter() {
                    super::config::relative_path(&case.file)?;
                    ensure!(
                        !case.name.is_empty() && !case.name.contains('\0'),
                        "invalid test case name"
                    );
                    ensure!(case.target.as_ref().is_none_or(|target| !target.is_empty() && target.len() <= 256 && !target.chars().any(char::is_control)), "invalid test target");
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
            Self::Files { files } => files.is_empty(),
            Self::Cases { cases } => cases.is_empty(),
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
/// Requests configured checks with optional explicit coverage, never replacement commands.
pub struct RunRequest {
    /// Explicitly runs every enabled check regardless of change triggers.
    #[serde(default)]
    pub all: bool,
    #[serde(default)]
    pub ids: Vec<String>,
    #[serde(default)]
    pub selections: BTreeMap<String, CheckSelection>,
}

impl RunRequest {
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
            let mut selection = if self.all {
                CheckSelection::All
            } else if let Some(selection) = self.selections.get(&definition.id) {
                selection.clone()
            } else {
                match definition.scope {
                    CheckScope::All => CheckSelection::All,
                    CheckScope::ChangedFiles => {
                        ensure!(
                            has_scope,
                            "{} requires explicit files outside a task execution",
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
                            "{} requires explicit cases outside a task execution",
                            definition.id
                        );
                        CheckSelection::Cases {
                            cases: (if definition.scope == CheckScope::NewCases { new_cases } else { cases })
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
            !self.selection.empty() && !matches!(self.selection, CheckSelection::All),
            "requested scope resolved to no tests or a broader suite"
        );
        Ok(())
    }

    /// Rejects receipts that widen requested files or replace requested cases with file-wide coverage.
    pub(crate) fn validate_request(&mut self, requested: &CheckSelection) -> Result<()> {
        self.validate()?;
        let contained = match (requested, &self.selection) {
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
