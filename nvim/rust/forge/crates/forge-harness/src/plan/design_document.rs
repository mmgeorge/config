use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::PlanSection;

#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Owns the reviewed intent and implementation guidance in virtual `plan.json`.
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
    /// Traces operations between concrete objects independently of Design prose.
    #[serde(default)]
    pub flows: Vec<super::design_flows::DesignFlow>,
    /// Lists manual actions and expected results after automated checks pass.
    pub verification: Vec<String>,
    /// Inventories changed and reused tests by file, independently of source language.
    pub tests: Vec<super::design_tests::DesignTestFile>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Adds configured checks explicitly, independently of automatic triggers.
    pub checks: Vec<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Suppresses configured checks for this plan, including automatic selection.
    pub disabled_checks: Vec<String>,
}

impl DesignDocument {
    /// Applies plan-only choices, enabling prerequisites and disabling dependent checks together.
    pub(crate) fn set_checks(&mut self, config: &crate::checks::CheckConfig, changes: &std::collections::BTreeMap<String, bool>) -> Result<()> {
        ensure!(!changes.is_empty() && changes.len() <= 128, "select 1 to 128 checks");
        for (id, enabled) in changes {
            let check = config.definitions.iter().find(|check| &check.id == id)
                .ok_or_else(|| anyhow::anyhow!("unknown planned check: {id}"))?;
            ensure!(!enabled || check.enabled, "configured check is disabled: {id}");
        }
        let mut resolved = changes.clone();
        loop {
            let mut added = false;
            for check in &config.definitions {
                if resolved.get(&check.id) == Some(&true) {
                    for dependency in &check.depends_on {
                        ensure!(resolved.get(dependency) != Some(&false), "conflicting choices for prerequisite {dependency}");
                        ensure!(config.definitions.iter().any(|candidate| &candidate.id == dependency && candidate.enabled), "configured prerequisite is disabled: {dependency}");
                        added |= resolved.insert(dependency.clone(), true).is_none();
                    }
                }
                if check.depends_on.iter().any(|dependency| resolved.get(dependency) == Some(&false)) {
                    ensure!(resolved.get(&check.id) != Some(&true), "conflicting choices for dependent {}", check.id);
                    added |= resolved.insert(check.id.clone(), false).is_none();
                }
            }
            if !added { break; }
        }
        for (id, enabled) in resolved {
            self.checks.retain(|selected| selected != &id);
            self.disabled_checks.retain(|selected| selected != &id);
            if enabled { self.checks.push(id); } else { self.disabled_checks.push(id); }
        }
        self.validate()
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
                            decision.decision.split_whitespace().collect::<Vec<_>>().join(" "),
                            decision.rationale.split_whitespace().collect::<Vec<_>>().join(" ")
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
                text: super::design_flows::text(&self.flows),
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
                text: self.verification.iter().map(|item| format!("- {item}")).collect::<Vec<_>>().join("\n"),
            },
        ]
    }

    /// Rejects malformed or oversized metadata while allowing incomplete drafts.
    pub(crate) fn validate(&self) -> Result<()> {
        ensure!(self.checks.len() <= 128, "plan checks exceeds 128 entries");
        ensure!(self.checks.iter().all(|id| !id.is_empty() && id.len() <= 128), "invalid planned check id");
        ensure!(self.disabled_checks.len() <= 128 && self.disabled_checks.iter().all(|id| !id.is_empty() && id.len() <= 128), "invalid disabled plan checks");
        ensure!(self.checks.iter().all(|id| !self.disabled_checks.contains(id)), "plan check cannot be enabled and disabled");
        super::design_flows::validate(&self.flows)?;
        super::design_tests::validate(&self.tests)?;
        for action in &self.verification {
            ensure!(!action.trim().is_empty(), "manual verification actions cannot be empty");
        }
        ensure!(
            self.requirements.len() <= 256,
            "plan requirements exceeds 256 entries"
        );
        ensure!(
            self.decisions.len() <= 128,
            "plan decisions exceeds 128 entries"
        );
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
                section.text.len() <= 16 * 1024,
                "plan {} exceeds 16 KiB",
                section.path
            );
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
            ("objective", &self.objective),
            ("background", &self.background),
            ("design", &self.design),
        ] {
            ensure!(
                !text.trim().is_empty(),
                "plan.json {name} is required before submission"
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
