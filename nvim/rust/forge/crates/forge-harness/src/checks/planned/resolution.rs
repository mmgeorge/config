//! Match planned tests across all participating checks without interpreting adapter IDs.

use super::inventory::{InventoryResult, PlannedInventory, SourceLocation};
use crate::checks::adapter::{DiscoverMode, TestItem, TestItemId};
use crate::plan::{DeclarationDesign, DesignTestChange, DesignTestFile};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Keep adapter-local identity qualified by its configured check.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(super) struct Candidate {
    pub(super) check: String,
    pub(super) id: TestItemId,
    pub(super) name: String,
    pub(super) source: SourceLocation,
}

/// Bind one authored entry to a single resolution outcome.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(super) struct Resolution {
    pub(super) file: String,
    pub(super) name: String,
    pub(super) change: DesignTestChange,
    pub(super) outcome: Outcome,
}

impl Resolution {
    /// Explain unresolved entries and inferred ownership without claiming static execution evidence.
    pub(super) fn warning(&self) -> Option<String> {
        let problem = match &self.outcome {
            Outcome::Resolved(candidate) if candidate.source == SourceLocation::Unverified => format!(
                "matches {} in check {} without a source location; source ownership is unverified",
                candidate.name, candidate.check,
            ),
            Outcome::Resolved(_) | Outcome::Absent => return None,
            Outcome::Problem(problem) => match problem {
                Problem::NoOwner => "no enabled test check discovered this file or a matching locationless case".into(),
                Problem::NotFound => "the file has an enabled test check, but the named test was not found".into(),
                Problem::Ambiguous(candidates) => format!(
                    "ambiguous across {}; qualify the name or disable an unintended check",
                    describe(candidates),
                ),
                Problem::DiscoveryFailed { check, message } => format!(
                    "check {check} could not establish its test inventory: {message}",
                ),
                Problem::StillPresent(candidates) => format!(
                    "planned removal still matches {}; exact discovery after implementation must prove absence",
                    describe(candidates),
                ),
                Problem::UnverifiedAbsence => "static discovery cannot verify removal; exact discovery is required after implementation".into(),
                Problem::UnsupportedTarget(target) => format!(
                    "target selector {target:?} is unsupported by test adapters; remove it and qualify the name or narrow the enabled checks",
                ),
                Problem::Duplicate(candidate) => format!(
                    "duplicates another planned entry for {} in check {}",
                    candidate.id.as_str(), candidate.check,
                ),
            },
        };
        Some(format!(
            "planned test {} in {}: {problem}",
            self.name, self.file
        ))
    }
}

/// Presence, exact absence, and resolution problems are disjoint outcomes.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(super) enum Outcome {
    Resolved(Candidate),
    Absent,
    Problem(Problem),
}

/// Preserve the reason a planned entry cannot yet be certified.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(super) enum Problem {
    NoOwner,
    NotFound,
    Ambiguous(Vec<Candidate>),
    DiscoveryFailed { check: String, message: String },
    StillPresent(Vec<Candidate>),
    UnverifiedAbsence,
    UnsupportedTarget(String),
    Duplicate(Candidate),
}

/// Produce compiler-style warnings for existing tests, keeping new tests for post-implementation resolution.
pub(in crate::checks) fn submission_warnings(
    design: &DeclarationDesign,
    inventories: &[PlannedInventory],
) -> anyhow::Result<Vec<super::PlannedTestWarning>> {
    let text = crate::plan::design_artifact_text(crate::plan::PlanArtifact::Tests, "", design)?;
    let mut file = "";
    let mut positions = BTreeMap::new();
    for (index, line) in text.lines().enumerate() {
        if let Some(path) = line.strip_prefix("## ") {
            file = path.trim();
        } else if let Some((_, rest)) = line
            .strip_prefix("- ")
            .and_then(|line| line.split_once('`'))
            && let Some((name, _)) = rest.split_once('`')
        {
            positions.insert((file, name), index + 1);
        }
    }
    resolve(&design.document.tests, DiscoverMode::Static, inventories)
        .into_iter()
        .filter_map(|resolution| {
            let message = resolution.warning()?;
            let line = positions
                .get(&(resolution.file.as_str(), resolution.name.as_str()))
                .copied();
            Some(
                line.ok_or_else(|| {
                    anyhow::anyhow!(
                        "submitted test warning has no authored line: {}: {}",
                        resolution.file,
                        resolution.name
                    )
                })
                .map(|line| super::PlannedTestWarning {
                    file: resolution.file,
                    name: resolution.name,
                    change: resolution.change,
                    line,
                    message,
                }),
            )
        })
        .collect::<anyhow::Result<Vec<_>>>()
}

/// Resolve all entries from one captured set of inventories; never choose one ambiguous check.
pub(super) fn resolve(
    planned: &[DesignTestFile],
    mode: DiscoverMode,
    inventories: &[PlannedInventory],
) -> Vec<Resolution> {
    let failure = inventories
        .iter()
        .find_map(|inventory| match inventory.discovery() {
            InventoryResult::Failed { reason } => {
                Some((inventory.check().to_owned(), reason.clone()))
            }
            InventoryResult::Available(discovery)
                if mode == DiscoverMode::Exact && discovery.mode() != DiscoverMode::Exact =>
            {
                Some((
                    inventory.check().to_owned(),
                    format!(
                        "expected {mode:?} discovery, received {:?}",
                        discovery.mode()
                    ),
                ))
            }
            InventoryResult::Available(_) => None,
        });
    let mut cases = Vec::new();
    let mut owned_files = std::collections::BTreeSet::new();
    for inventory in inventories {
        if let InventoryResult::Available(discovery) = inventory.discovery() {
            let mut locations = inventory.locations();
            for item in discovery.items() {
                let location = locations
                    .remove(&item.id)
                    .expect("every primary item has an indexed location");
                if let Some(path) = location.path() {
                    owned_files.insert(path.to_owned());
                }
                if item.case {
                    cases.push((inventory.check(), item, discovery.mode(), location));
                }
            }
        }
    }
    let mut resolutions = Vec::new();
    for file in planned {
        for test in &file.cases {
            if mode == DiscoverMode::Static && test.change == DesignTestChange::New {
                continue;
            }
            let outcome = if let Some(target) =
                test.target.as_ref().filter(|target| !target.is_empty())
            {
                Outcome::Problem(Problem::UnsupportedTarget(target.clone()))
            } else if let Some((check, message)) = &failure {
                Outcome::Problem(Problem::DiscoveryFailed {
                    check: check.clone(),
                    message: message.clone(),
                })
            } else {
                let mut candidates = candidates(&cases, &file.file, &test.name);
                candidates
                    .sort_by(|left, right| (&left.check, &left.id).cmp(&(&right.check, &right.id)));
                if test.change == DesignTestChange::Removed {
                    if !candidates.is_empty() {
                        Outcome::Problem(Problem::StillPresent(candidates))
                    } else if mode == DiscoverMode::Exact && !inventories.is_empty() {
                        Outcome::Absent
                    } else {
                        Outcome::Problem(Problem::UnverifiedAbsence)
                    }
                } else {
                    match candidates.as_slice() {
                        [candidate] => Outcome::Resolved(candidate.clone()),
                        [] if owned_files.contains(&file.file) => {
                            Outcome::Problem(Problem::NotFound)
                        }
                        [] => Outcome::Problem(Problem::NoOwner),
                        _ => Outcome::Problem(Problem::Ambiguous(candidates)),
                    }
                }
            };
            resolutions.push(Resolution {
                file: file.file.clone(),
                name: test.name.clone(),
                change: test.change,
                outcome,
            });
        }
    }

    // Mark every conflicting entry, including the first, so array order cannot pick a winner.
    let mut owners = BTreeMap::<(String, TestItemId), Vec<usize>>::new();
    for (index, resolution) in resolutions.iter().enumerate() {
        if let Outcome::Resolved(candidate) = &resolution.outcome {
            owners
                .entry((candidate.check.clone(), candidate.id.clone()))
                .or_default()
                .push(index);
        }
    }
    for indices in owners.values().filter(|indices| indices.len() > 1) {
        for &index in indices {
            let Outcome::Resolved(candidate) = &resolutions[index].outcome else {
                unreachable!()
            };
            resolutions[index].outcome = Outcome::Problem(Problem::Duplicate(candidate.clone()));
        }
    }
    resolutions
}

/// Prefer file-owned cases globally, then permit locationless names only with exact discovery.
fn candidates(
    cases: &[(&str, &TestItem, DiscoverMode, SourceLocation)],
    file: &str,
    name: &str,
) -> Vec<Candidate> {
    let matching = cases
        .iter()
        .filter(|(_, item, _, _)| {
            item.name
                .as_deref()
                .is_some_and(|found| names_match(found, name))
        })
        .collect::<Vec<_>>();
    let located = matching
        .iter()
        .copied()
        .filter(|(_, _, _, source)| source.path() == Some(file))
        .collect::<Vec<_>>();
    let chosen = if located.is_empty() {
        matching
            .into_iter()
            .filter(|(_, _, accuracy, source)| {
                source.path().is_none() && *accuracy == DiscoverMode::Exact
            })
            .collect()
    } else {
        located
    };
    chosen
        .into_iter()
        .map(|(check, item, _, source)| Candidate {
            check: (*check).into(),
            id: item.id.clone(),
            name: item.name.clone().expect("matching requires a name"),
            source: source.clone(),
        })
        .collect()
}

/// Render qualified candidate identity without assigning semantics to the opaque item ID.
fn describe(candidates: &[Candidate]) -> String {
    candidates
        .iter()
        .map(|candidate| format!("{} ({})", candidate.check, candidate.id.as_str()))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Match qualified names only at complete namespace boundaries, never arbitrary suffixes.
fn names_match(discovered: &str, planned: &str) -> bool {
    discovered == planned
        || discovered
            .strip_suffix(planned)
            .is_some_and(|prefix| prefix.ends_with("::"))
        || planned
            .strip_suffix(discovered)
            .is_some_and(|prefix| prefix.ends_with("::"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checks::adapter::Discovery;

    /// Construct a discovered case with opaque identity and optional source ownership.
    fn item(id: &str, name: &str, path: Option<&str>) -> TestItem {
        TestItem {
            id: id.to_owned().try_into().unwrap(),
            parent: None,
            label: name.into(),
            case: true,
            name: Some(name.into()),
            path: path.map(str::to_owned),
            line: None,
        }
    }

    /// Construct one successful check inventory at the requested accuracy.
    fn inventory(check: &str, mode: DiscoverMode, items: Vec<TestItem>) -> PlannedInventory {
        PlannedInventory::new(check.into(), Ok(Discovery::new(items, mode).unwrap())).unwrap()
    }

    /// Preserve valid authored test grammar without exposing plan implementation fields.
    fn planned(entries: &[(&str, &str, DesignTestChange)]) -> Vec<DesignTestFile> {
        entries
            .iter()
            .map(|(file, name, change)| {
                serde_json::from_value(serde_json::json!({
                    "file": file, "cases": [{"name":name,"change":change,"description":"scenario"}]
                }))
                .unwrap()
            })
            .collect()
    }

    /// Check identities qualify equal adapter IDs, and a failed peer cannot hide ambiguity.
    #[test]
    fn cross_check_candidates_are_ambiguous_and_failures_block_partial_evidence() {
        let planned = planned(&[("tests.rs", "works", DesignTestChange::Modified)]);
        let mut inventories = vec![
            inventory(
                "a",
                DiscoverMode::Exact,
                vec![item("same-id", "module::works", Some("tests.rs"))],
            ),
            inventory(
                "b",
                DiscoverMode::Exact,
                vec![item("same-id", "works", Some("tests.rs"))],
            ),
        ];
        let result = resolve(&planned, DiscoverMode::Exact, &inventories);
        let Outcome::Problem(Problem::Ambiguous(candidates)) = &result[0].outcome else {
            panic!("ambiguous checks were selected")
        };
        assert_eq!(candidates.len(), 2);
        assert_eq!(describe(candidates), "a (same-id), b (same-id)");
        inventories[1] = PlannedInventory::new(
            "b".into(),
            Err(crate::checks::owner::CheckOwnerFailure::Worker(
                anyhow::anyhow!("native list failed"),
            )),
        )
        .unwrap();
        let result = resolve(&planned, DiscoverMode::Exact, &inventories);
        assert!(
            matches!(&result[0].outcome, Outcome::Problem(Problem::DiscoveryFailed { check, message }) if check == "b" && message.contains("native list failed"))
        );
    }

    /// Prefer known file ownership globally and use locationless names only in exact results.
    #[test]
    fn source_matching_respects_files_accuracy_and_namespace_boundaries() {
        let planned = planned(&[("tests.rs", "works", DesignTestChange::Reused)]);
        let inventories = vec![
            inventory(
                "a",
                DiscoverMode::Exact,
                vec![
                    item("located", "module::works", Some("tests.rs")),
                    item("elsewhere", "works", Some("other.rs")),
                    item("substring", "notworks", Some("tests.rs")),
                ],
            ),
            inventory(
                "b",
                DiscoverMode::Exact,
                vec![item("generated", "works", None)],
            ),
        ];
        let result = resolve(&planned, DiscoverMode::Exact, &inventories);
        assert!(
            matches!(&result[0].outcome, Outcome::Resolved(candidate) if candidate.id.as_str() == "located" && matches!(&candidate.source, SourceLocation::Exact { .. }))
        );
        let generated = vec![inventory(
            "b",
            DiscoverMode::Exact,
            vec![item("generated", "qualified::works", None)],
        )];
        let result = resolve(&planned, DiscoverMode::Exact, &generated);
        assert!(
            matches!(&result[0].outcome, Outcome::Resolved(candidate) if candidate.source == SourceLocation::Unverified)
        );
        assert!(result[0].warning().unwrap().contains("qualified::works"));
        assert!(matches!(
            &resolve(&planned, DiscoverMode::Static, &generated)[0].outcome,
            Outcome::Resolved(candidate) if candidate.source == SourceLocation::Unverified
        ));
        let static_items = vec![inventory(
            "b",
            DiscoverMode::Static,
            vec![item("generated", "works", None)],
        )];
        assert_eq!(
            resolve(&planned, DiscoverMode::Static, &static_items)[0].outcome,
            Outcome::Problem(Problem::NoOwner)
        );
        let mut file = item("file", "file", Some("tests.rs"));
        file.case = false;
        let owner = vec![inventory("a", DiscoverMode::Static, vec![file])];
        assert_eq!(
            resolve(&planned, DiscoverMode::Static, &owner)[0].outcome,
            Outcome::Problem(Problem::NotFound)
        );
    }

    /// Only complete exact inventories can verify removal; static absence remains a warning.
    #[test]
    fn removal_requires_all_participating_exact_inventories() {
        let planned = planned(&[("tests.rs", "gone", DesignTestChange::Removed)]);
        let exact = vec![inventory("a", DiscoverMode::Exact, Vec::new())];
        assert_eq!(
            resolve(&planned, DiscoverMode::Exact, &exact)[0].outcome,
            Outcome::Absent
        );
        assert_eq!(
            resolve(&planned, DiscoverMode::Static, &exact)[0].outcome,
            Outcome::Problem(Problem::UnverifiedAbsence)
        );
        assert_eq!(
            resolve(&planned, DiscoverMode::Exact, &[])[0].outcome,
            Outcome::Problem(Problem::UnverifiedAbsence)
        );
        let static_items = vec![inventory("a", DiscoverMode::Static, Vec::new())];
        assert_eq!(
            resolve(&planned, DiscoverMode::Static, &static_items)[0].outcome,
            Outcome::Problem(Problem::UnverifiedAbsence)
        );
        assert!(matches!(
            &resolve(&planned, DiscoverMode::Exact, &static_items)[0].outcome,
            Outcome::Problem(Problem::DiscoveryFailed { .. })
        ));
        let present = vec![inventory(
            "a",
            DiscoverMode::Exact,
            vec![item("existing", "gone", None)],
        )];
        assert!(
            matches!(&resolve(&planned, DiscoverMode::Exact, &present)[0].outcome, Outcome::Problem(Problem::StillPresent(candidates)) if candidates.len() == 1)
        );
    }

    /// Report all duplicate entries and reject legacy targets rather than silently dropping them.
    #[test]
    fn duplicate_entries_and_unsupported_targets_are_not_execution_selections() {
        let mut planned = planned(&[
            ("tests.rs", "works", DesignTestChange::Modified),
            ("tests.rs", "module::works", DesignTestChange::Reused),
        ]);
        let inventories = vec![inventory(
            "a",
            DiscoverMode::Exact,
            vec![item("case", "module::works", Some("tests.rs"))],
        )];
        let result = resolve(&planned, DiscoverMode::Exact, &inventories);
        assert!(
            result
                .iter()
                .all(|result| matches!(&result.outcome, Outcome::Problem(Problem::Duplicate(_))))
        );
        planned.truncate(1);
        planned[0].cases[0].target = Some("legacy-binary".into());
        assert_eq!(
            resolve(&planned, DiscoverMode::Exact, &inventories)[0].outcome,
            Outcome::Problem(Problem::UnsupportedTarget("legacy-binary".into()))
        );
    }

    /// Submission skips new cases and anchors missing existing tests to their authored lines.
    #[test]
    fn submission_warnings_name_artifact_lines_without_claiming_absence() -> anyhow::Result<()> {
        let mut design = DeclarationDesign::default();
        design.document.tests = planned(&[
            ("tests.rs", "new_test", DesignTestChange::New),
            ("tests.rs", "missing", DesignTestChange::Modified),
            ("other.rs", "gone", DesignTestChange::Removed),
        ]);
        design.artifact_text.insert("plan:tests.md".into(), "## tests.rs\n- new `new_test`: scenario\n- modified `missing`: scenario\n\n## other.rs\n- removed `gone`: scenario\n".into());
        let inventories = vec![inventory("a", DiscoverMode::Static, Vec::new())];
        let warnings = submission_warnings(&design, &inventories)?;
        assert_eq!(warnings.len(), 2);
        assert_eq!(warnings[0].violation().path, "plan:tests.md:3");
        assert_eq!(warnings[1].violation().path, "plan:tests.md:6");
        assert!(warnings[1].message.contains("cannot verify removal"));
        Ok(())
    }
}
