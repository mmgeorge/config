use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use super::PlanSection;

/// Select the test coverage requested by the reviewer for one plan.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanTestScope {
    /// Run the required-present planned tests owned by the check.
    #[default]
    Planned,
    /// Run every test while still enforcing each planned test's outcome.
    WholeSuite,
}

#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Owns the reviewed intent and implementation guidance that the `plan:` artifacts project.
pub struct DesignDocument {
    /// States the requested outcome and scope.
    pub objective: String,
    /// Illustrates successful use when an observable interaction clarifies the change.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub usage: Option<String>,
    /// Records key acceptance criteria when they need a separate review section.
    pub requirements: Vec<String>,
    /// Describes inspected existing behavior and integration points needed by a new implementer.
    pub background: String,
    /// Preserves consequential choices and the reasons for selecting them.
    pub decisions: Vec<DesignDecision>,
    /// Explains the proposed solution and ownership boundaries.
    pub design: String,
    /// Holds the runtime flows as the Markdown of `plan:flows.md`: one `## Title` heading per
    /// flow with a short description and a Mermaid diagram.
    #[serde(default)]
    pub flows: String,
    /// Lists the manual checks Verify performs after automated checks pass, from
    /// `## Manual checks`.
    pub manual_checks: Vec<String>,
    /// Inventories changed and reused tests by file, independently of source language.
    pub tests: Vec<super::design_tests::DesignTestFile>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Adds configured checks explicitly, independently of automatic triggers.
    pub checks: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Suppresses configured checks for this plan, including automatic selection.
    pub disabled_checks: Vec<String>,
    /// Retain reviewer-owned test coverage choices independently of project defaults.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub test_scopes: BTreeMap<String, PlanTestScope>,
}

impl DesignDocument {
    /// Applies plan-only choices, enabling prerequisites and disabling dependent checks together.
    pub(crate) fn set_checks(
        &mut self,
        config: &crate::checks::CheckConfig,
        changes: &std::collections::BTreeMap<String, bool>,
        test_scopes: &BTreeMap<String, PlanTestScope>,
    ) -> Result<()> {
        ensure!(
            !changes.is_empty() || !test_scopes.is_empty(),
            "select at least one check"
        );
        Self::validate_test_scopes(config, test_scopes)?;
        for (id, enabled) in changes {
            let check = config
                .definitions
                .iter()
                .find(|check| &check.id == id)
                .ok_or_else(|| anyhow::anyhow!("unknown planned check: {id}"))?;
            ensure!(
                !enabled || check.enabled,
                "configured check is disabled: {id}"
            );
        }
        let mut resolved = changes.clone();
        loop {
            let mut added = false;
            for check in &config.definitions {
                if resolved.get(&check.id) == Some(&true) {
                    for dependency in &check.depends_on {
                        ensure!(
                            resolved.get(dependency) != Some(&false),
                            "conflicting choices for prerequisite {dependency}"
                        );
                        ensure!(
                            config
                                .definitions
                                .iter()
                                .any(|candidate| &candidate.id == dependency && candidate.enabled),
                            "configured prerequisite is disabled: {dependency}"
                        );
                        added |= resolved.insert(dependency.clone(), true).is_none();
                    }
                }
                if check
                    .depends_on
                    .iter()
                    .any(|dependency| resolved.get(dependency) == Some(&false))
                {
                    ensure!(
                        resolved.get(&check.id) != Some(&true),
                        "conflicting choices for dependent {}",
                        check.id
                    );
                    added |= resolved.insert(check.id.clone(), false).is_none();
                }
            }
            if !added {
                break;
            }
        }
        for (id, enabled) in resolved {
            self.checks.retain(|selected| selected != &id);
            self.disabled_checks.retain(|selected| selected != &id);
            if enabled {
                self.checks.push(id);
            } else {
                self.disabled_checks.push(id);
            }
        }
        self.test_scopes.extend(test_scopes.clone());
        self.validate()
    }

    /// Reject stale coverage choices without dropping the reviewer's authored setting.
    pub(crate) fn validate_test_scopes(
        config: &crate::checks::CheckConfig,
        scopes: &BTreeMap<String, PlanTestScope>,
    ) -> Result<()> {
        for id in scopes.keys() {
            ensure!(
                config.definitions.iter().any(|check| &check.id == id
                    && check.category == crate::checks::CheckCategory::Test
                    && matches!(check.runner, crate::checks::CheckRunner::Adapter(_))),
                "test scope requires an existing test adapter check: {id}"
            );
        }
        Ok(())
    }

    /// Projects metadata into one shared order for review, revision diffs, and section reads.
    pub(crate) fn sections(&self) -> Vec<DesignSection> {
        vec![
            DesignSection {
                path: "objective",
                title: "Objective",
                section: PlanSection::Objective,
                text: self.objective.clone(),
            },
            DesignSection {
                path: "usage",
                title: "Usage",
                section: PlanSection::Usage,
                text: self.usage.clone().unwrap_or_default(),
            },
            DesignSection {
                path: "requirements",
                title: "Requirements",
                section: PlanSection::Requirements,
                text: self
                    .requirements
                    .iter()
                    .map(|requirement| format!("- {}", requirement.replace('\n', "\n  ")))
                    .collect::<Vec<_>>()
                    .join("\n"),
            },
            DesignSection {
                path: "background",
                title: "Background",
                section: PlanSection::Background,
                text: self.background.clone(),
            },
            DesignSection {
                path: "decisions",
                title: "Decisions",
                section: PlanSection::Decisions,
                text: self
                    .decisions
                    .iter()
                    .map(|decision| {
                        format!(
                            "- **{}** {}",
                            decision
                                .decision
                                .split_whitespace()
                                .collect::<Vec<_>>()
                                .join(" "),
                            decision
                                .rationale
                                .split_whitespace()
                                .collect::<Vec<_>>()
                                .join(" ")
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n"),
            },
            DesignSection {
                path: "design",
                title: "Design",
                section: PlanSection::Design,
                text: self.design.clone(),
            },
            DesignSection {
                path: "flows",
                title: "Flows",
                section: PlanSection::Flows,
                text: self.flows.trim_end_matches('\n').to_owned(),
            },
            DesignSection {
                path: "tests",
                title: "Tests",
                section: PlanSection::Tests,
                text: super::design_tests::text(&self.tests),
            },
            DesignSection {
                path: "verification",
                title: "Verification",
                section: PlanSection::ManualVerification,
                text: self
                    .manual_checks
                    .iter()
                    .map(|item| format!("- {item}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            },
        ]
    }

    /// Validate metadata while allowing incomplete drafts.
    pub(crate) fn validate(&self) -> Result<()> {
        ensure!(
            self.test_scopes.keys().all(|id| !id.is_empty()),
            "invalid test scope check id"
        );
        ensure!(
            self.checks.iter().all(|id| !id.is_empty()),
            "invalid planned check id"
        );
        ensure!(
            self.disabled_checks.iter().all(|id| !id.is_empty()),
            "invalid disabled plan checks"
        );
        ensure!(
            self.checks
                .iter()
                .all(|id| !self.disabled_checks.contains(id)),
            "plan check cannot be enabled and disabled"
        );
        super::design_tests::validate(&self.tests)?;
        for action in &self.manual_checks {
            ensure!(
                !action.trim().is_empty(),
                "manual verification actions cannot be empty"
            );
        }
        for requirement in &self.requirements {
            ensure!(
                !requirement.trim().is_empty(),
                "plan requirement must not be empty"
            );
        }
        for decision in &self.decisions {
            ensure!(
                !decision.decision.trim().is_empty() && !decision.rationale.trim().is_empty(),
                "plan decision requires a choice and rationale"
            );
        }
        for section in self.sections() {
            ensure!(
                !section.text.contains('\0'),
                "plan {} contains a NUL byte",
                section.path
            );
        }
        Ok(())
    }

    /// Requires an actionable specification before it can enter review.
    pub(crate) fn validate_for_submission(&self) -> Result<()> {
        self.validate()?;
        for (name, text) in [
            ("Objective", &self.objective),
            ("Background", &self.background),
            ("Design", &self.design),
        ] {
            ensure!(
                !text.trim().is_empty(),
                "plan:overview.md `## {name}` is required before submission"
            );
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Records one consequential design choice and its justification.
pub struct DesignDecision {
    /// Names the selected approach.
    pub decision: String,
    /// Explains the constraints or tradeoffs that justify the choice.
    pub rationale: String,
}

/// Associates projected metadata with its canonical edit and navigation identity.
pub(crate) struct DesignSection {
    pub path: &'static str,
    pub title: &'static str,
    pub section: PlanSection,
    pub text: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve disabled Whole-suite choices and reject stale keys without mutating the document.
    #[test]
    fn test_scopes_are_review_owned_and_require_test_adapters() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let checks = serde_json::json!({
            "tests": {"enabled":false,"trigger":{},"action":{"category":"test","adapter":{"script":{"command":["unused"]}}}},
            "custom": {"trigger":{},"action":{"category":"custom","command":["unused"]}}
        });
        std::fs::write(
            workspace.path().join(".forge.json"),
            serde_json::to_vec(&serde_json::json!({"checks":checks}))?,
        )?;
        let config = crate::checks::ConfigSnapshot::load(workspace.path())?.config;
        let scopes = BTreeMap::from([("tests".into(), PlanTestScope::WholeSuite)]);
        let mut document = DesignDocument::default();
        document.set_checks(&config, &BTreeMap::new(), &scopes)?;
        assert!(document.checks.is_empty());
        assert_eq!(document.test_scopes, scopes);
        let restored: DesignDocument = serde_json::from_value(serde_json::to_value(&document)?)?;
        assert_eq!(restored, document);
        let mut design = crate::plan::DeclarationDesign {
            document: document.clone(),
            ..Default::default()
        };
        let parsed = crate::plan::design_artifact::parse_authored("Scope", &design, &config)?;
        assert_eq!(parsed.document.test_scopes, scopes);
        let mut snapshot = crate::checks::ConfigSnapshot::load(workspace.path())?;
        let test_check = snapshot
            .config
            .definitions
            .iter_mut()
            .find(|check| check.id == "tests")
            .unwrap();
        test_check.enabled = true;
        test_check.auto = false;
        design.check_configuration = Some(snapshot);
        let option = design
            .check_options()
            .into_iter()
            .find(|option| option["id"] == "tests")
            .unwrap();
        assert_eq!(option["enabled"], true);
        design.document.disabled_checks.push("tests".into());
        let option = design
            .check_options()
            .into_iter()
            .find(|option| option["id"] == "tests")
            .unwrap();
        assert_eq!(option["enabled"], false);
        for id in ["custom", "missing"] {
            assert!(
                document
                    .set_checks(
                        &config,
                        &BTreeMap::new(),
                        &BTreeMap::from([(id.into(), PlanTestScope::WholeSuite)])
                    )
                    .is_err()
            );
            assert_eq!(document, restored);
        }
        let mut legacy = serde_json::to_value(&document)?;
        legacy.as_object_mut().unwrap().remove("test_scopes");
        assert!(
            serde_json::from_value::<DesignDocument>(legacy)?
                .test_scopes
                .is_empty()
        );
        Ok(())
    }
}
