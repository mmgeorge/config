use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::{Context, Result};
use forge_diff::syntax::{DeclarationCalls, DeclarationContract, DeclarationOverview};
use serde::{Deserialize, Serialize};

use super::{DeclarationDesign, PlanExecutionRecord};
use crate::checkpoint::CheckpointRecord;
use crate::storage::objects::ObjectStore;

/// Compact, durable identity for report content stored outside session snapshots.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ImplementationReportRef {
    pub object_id: String,
    pub files: usize,
    pub helpers: usize,
    pub relationships: usize,
    pub incomplete: bool,
}

impl ImplementationReportRef {
    pub fn summary(&self) -> String {
        format!(
            "Implementation differences{} · {} files · {} internal additions · {} relationship changes",
            if self.incomplete { " (incomplete)" } else { "" },
            self.files,
            self.helpers,
            self.relationships
        )
    }
}

/// Source-derived details for one execution, independent of the model's final answer.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ImplementationReport {
    pub original_revision: u32,
    pub accepted_revision: u32,
    pub revisions: Vec<super::execution::ExecutionRevision>,
    pub file: Vec<ImplementationFile>,
    pub unavailable: Vec<String>,
}

/// Declaration and reference differences owned by one changed or planned file.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ImplementationFile {
    pub path: String,
    pub internal: Vec<String>,
    pub contract: Vec<String>,
    pub references: Vec<ReferenceDifference>,
}

/// Planned and observed occurrences for a single callable category.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ReferenceDifference {
    pub owner: String,
    pub category: String,
    pub expected: Option<Vec<String>>,
    pub observed: Vec<String>,
}

impl ImplementationReport {
    pub fn new(execution: &PlanExecutionRecord) -> Self {
        Self {
            original_revision: execution.original_revision,
            accepted_revision: execution.revision,
            revisions: execution.revision_history.clone(),
            file: Vec::new(),
            unavailable: Vec::new(),
        }
    }

    /// Inspect only accepted paths and checkpoint changes, retaining immutable source identities.
    pub fn capture(
        mut self,
        original: &DeclarationDesign,
        initial: Option<&CheckpointRecord>,
        current: Option<&CheckpointRecord>,
        objects: &ObjectStore,
        workspace: &Path,
    ) -> Result<(Self, BTreeMap<String, String>)> {
        let report = &mut self;
        let mut paths = original
            .proposed
            .keys()
            .chain(original.baseline.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        if let (Some(initial), Some(current)) = (initial, current) {
            paths.extend(initial.changed_paths(current)?);
        }
        if initial.is_none() || current.is_none() {
            report.unavailable.push(
                "Workspace-wide comparison unavailable without both execution checkpoints.".into(),
            );
        }
        let mut digests = BTreeMap::new();
        for path in paths {
            if !DeclarationOverview::supports(&path) {
                report.file.push(ImplementationFile {
                    path,
                    internal: Vec::new(),
                    contract: vec!["Changed artifact outside declaration analysis".into()],
                    references: Vec::new(),
                });
                continue;
            }
            let source = if let Some(current) = current {
                current
                    .read(objects, &path, usize::MAX)
                    .and_then(|bytes| bytes.map(String::from_utf8).transpose().map_err(Into::into))
            } else {
                super::workspace_source(workspace, &path)
            };
            let source = match source {
                Ok(source) => source,
                Err(error) => {
                    report.unavailable.push(format!("{path}: {error:#}"));
                    continue;
                }
            };
            digests.insert(
                path.clone(),
                source
                    .as_deref()
                    .map_or_else(|| "absent".into(), |text| super::digest(text.as_bytes())),
            );
            let expected = original.proposed.get(&path).cloned().unwrap_or_default();
            let mut file = ImplementationFile {
                path: path.clone(),
                internal: Vec::new(),
                contract: Vec::new(),
                references: Vec::new(),
            };
            let Some(source) = source else {
                if !expected.is_empty() {
                    file.contract.push("Planned file is absent".into());
                } else if !original.baseline.contains_key(&path)
                    && initial.is_some_and(|initial| {
                        initial
                            .read(objects, &path, usize::MAX)
                            .ok()
                            .flatten()
                            .is_some()
                    })
                {
                    file.contract
                        .push("File removed outside the original plan".into());
                }
                if !file.contract.is_empty() {
                    report.file.push(file);
                }
                continue;
            };
            if original.baseline.contains_key(&path) && !original.proposed.contains_key(&path) {
                file.contract.push("Planned deletion still exists".into());
            }
            if forge_diff::syntax::ConfigurationFormat::for_path(&path).is_some() {
                if expected.is_empty() {
                    file.contract
                        .push("Configuration changed outside the original plan".into());
                    report.file.push(file);
                    continue;
                }
                match super::conformance::compare(&path, &expected, &source) {
                    Ok(differences) if !differences.is_empty() => file
                        .contract
                        .push("Configuration or artifact differs from the original plan".into()),
                    Err(error) => report.unavailable.push(format!("{path}: {error:#}")),
                    _ => {}
                }
                if !file.contract.is_empty() {
                    report.file.push(file);
                }
                continue;
            }
            let result = (|| -> Result<()> {
                let overview = DeclarationOverview::extract(&path, &source)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                let actual = super::conformance::contract(workspace, &path, &overview)?;
                let mut planned = DeclarationContract::parse(&path, &expected)
                    .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                // Existing unplanned declarations are not additions made by this execution.
                if expected.is_empty()
                    && let Some(initial) = initial
                    && let Some(bytes) = initial.read(objects, &path, usize::MAX)?
                {
                    let baseline = String::from_utf8(bytes)?;
                    let baseline = DeclarationOverview::extract(&path, &baseline)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                    planned = DeclarationContract::parse(&path, &baseline)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
                }
                file.contract = planned.differences(&actual);
                for (identity, declarations) in &actual.declaration {
                    if !planned.declaration.contains_key(identity)
                        && declarations.iter().all(|item| !item.exposed)
                    {
                        file.internal.push(identity.clone());
                    }
                }
                match DeclarationCalls::extract(&path, &source, false) {
                    Ok(observed) => {
                        let observed = super::calls::from_extracted(observed);
                        let planned = original
                            .proposed_calls
                            .get(&path)
                            .map(Vec::as_slice)
                            .unwrap_or_default();
                        let owners = planned
                            .iter()
                            .chain(&observed)
                            .map(|body| body.owner.as_str())
                            .collect::<BTreeSet<_>>();
                        for owner in owners {
                            let expected_body = planned.iter().find(|body| body.owner == owner);
                            let actual_body = observed.iter().find(|body| body.owner == owner);
                            for (label, category) in [
                                ("Calls", super::CallKind::Call),
                                ("Accesses", super::CallKind::Property),
                            ] {
                                let category_known = expected_body
                                    .and_then(|body| body.evidence)
                                    .is_some_and(|evidence| {
                                        if category == super::CallKind::Call {
                                            evidence.calls
                                        } else {
                                            evidence.accesses
                                        }
                                    });
                                let targets = |body: Option<&super::FunctionBody>| {
                                    body.into_iter()
                                        .flat_map(|body| body.call.iter().flatten())
                                        .filter(|call| call.kind.category() == category)
                                        .map(|call| {
                                            if call.unresolved {
                                                format!("{} (unresolved)", call.name)
                                            } else {
                                                call.name.clone()
                                            }
                                        })
                                        .collect::<Vec<_>>()
                                };
                                let expected = category_known.then(|| targets(expected_body));
                                let observed = targets(actual_body);
                                if expected.as_ref() != Some(&observed)
                                    && (expected.is_some() || !observed.is_empty())
                                {
                                    file.references.push(ReferenceDifference {
                                        owner: owner.into(),
                                        category: label.into(),
                                        expected,
                                        observed,
                                    });
                                }
                            }
                        }
                    }
                    Err(error) => report
                        .unavailable
                        .push(format!("{path}: relationships unavailable: {error:?}")),
                }
                Ok(())
            })();
            if let Err(error) = result {
                report.unavailable.push(format!("{path}: {error:#}"));
            }
            if !file.internal.is_empty() || !file.contract.is_empty() || !file.references.is_empty()
            {
                report.file.push(file);
            }
        }
        Ok((self, digests))
    }

    /// Publish immutable report bytes before their reference joins the execution transaction.
    pub fn save(&self, objects: &ObjectStore, incomplete: bool) -> Result<ImplementationReportRef> {
        Ok(ImplementationReportRef {
            object_id: objects.put(&serde_json::to_vec(self)?)?,
            files: self.file.len(),
            helpers: self.file.iter().map(|file| file.internal.len()).sum(),
            relationships: self.file.iter().map(|file| file.references.len()).sum(),
            incomplete: incomplete || !self.unavailable.is_empty(),
        })
    }

    pub fn load(objects: &ObjectStore, reference: &ImplementationReportRef) -> Result<Self> {
        serde_json::from_slice(
            &objects
                .get(&reference.object_id, usize::MAX)?
                .context("implementation report is unavailable")?,
        )
        .map_err(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::calls;

    fn report() -> ImplementationReport {
        ImplementationReport {
            original_revision: 1,
            accepted_revision: 2,
            revisions: Vec::new(),
            file: Vec::new(),
            unavailable: Vec::new(),
        }
    }

    #[test]
    fn source_report_preserves_unknown_and_explicit_empty_relationships() {
        let workspace = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(data.path()).unwrap();
        let source = "pub fn run() { helper(); } fn helper() {}";
        std::fs::write(workspace.path().join("lib.rs"), source).unwrap();
        let mut design = DeclarationDesign::default();
        let (overview, body) = calls::parse("lib.rs", "pub fn run();\nCalls\n", &[]).unwrap();
        design.proposed.insert("lib.rs".into(), overview);
        design.proposed_calls.insert("lib.rs".into(), body);
        let (report, digests) = report()
            .capture(&design, None, None, &objects, workspace.path())
            .unwrap();
        assert_eq!(digests["lib.rs"], crate::plan::digest(source.as_bytes()));
        assert!(report.file[0].contract.is_empty());
        assert_eq!(report.file[0].internal.len(), 1);
        let calls = report.file[0]
            .references
            .iter()
            .find(|entry| entry.owner == "run" && entry.category == "Calls")
            .unwrap();
        assert_eq!(calls.expected, Some(Vec::new()));
        assert_eq!(calls.observed, ["helper"]);
        let reference = report.save(&objects, false).unwrap();
        assert!(reference.incomplete);
        let loaded = ImplementationReport::load(&objects, &reference).unwrap();
        assert_eq!(loaded.original_revision, 1);
        assert_eq!(loaded.accepted_revision, 2);
        let node = crate::exchange::ExchangeNode::ImplementationReport {
            id: "report".into(),
            reference,
            report: Some(std::sync::Arc::new(report)),
        };
        let serialized = serde_json::to_string(&node).unwrap();
        assert!(!serialized.contains("function helper"));
        assert!(!serialized.contains("observed"));
        assert!(!serialized.contains("source_report"));
    }

    #[test]
    fn checkpoint_comparison_includes_deletions_and_new_files_without_attributing_existing_edits() {
        use crate::checkpoint::CheckpointFile;
        let workspace = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let objects = ObjectStore::open(data.path()).unwrap();
        let mut initial = CheckpointRecord {
            id: "before".into(),
            session_id: "s".into(),
            workspace: workspace.path().to_string_lossy().into(),
            head: "UNBORN".into(),
            tree: None,
            file: Vec::new(),
            deleted: Vec::new(),
            checkout: BTreeMap::new(),
            created_at_ms: 0,
        };
        for (path, source) in [
            ("existing.rs", "fn user_edit() {}"),
            ("removed.rs", "fn removed() {}"),
        ] {
            initial.file.push(CheckpointFile {
                path: path.into(),
                object_id: objects.put(source.as_bytes()).unwrap(),
                mode: 0o100644,
            });
        }
        let mut current = initial.clone();
        current.file.retain(|file| file.path != "removed.rs");
        current.file.push(CheckpointFile {
            path: "added.rs".into(),
            object_id: objects.put(b"fn added() {}").unwrap(),
            mode: 0o100644,
        });
        assert_eq!(
            initial.changed_paths(&current).unwrap(),
            ["added.rs", "removed.rs"]
        );
        let (report, _) = report()
            .capture(
                &DeclarationDesign::default(),
                Some(&initial),
                Some(&current),
                &objects,
                workspace.path(),
            )
            .unwrap();
        assert!(!report.file.iter().any(|file| file.path == "existing.rs"));
        assert!(
            report
                .file
                .iter()
                .any(|file| file.path == "added.rs" && file.internal.len() == 1)
        );
        assert!(report.file.iter().any(|file| file.path == "removed.rs"));
    }
}
