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
    /// Defines the behavior and restrictions every valid implementation must satisfy.
    pub requirements: Vec<String>,
    /// Describes inspected existing behavior and integration points needed by a new implementer.
    pub background: String,
    /// Preserves consequential choices and the reasons for selecting them.
    pub decisions: Vec<DesignDecision>,
    /// Explains the proposed responsibilities, interactions, and lifecycle.
    pub design: String,
    /// Records the checks required before execution can complete.
    pub verification: DesignVerification,
}

impl DesignDocument {
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
                    .enumerate()
                    .map(|(index, decision)| {
                        format!(
                            "{}. {}\n\n   {}",
                            index + 1,
                            decision.decision.replace('\n', "\n   "),
                            decision.rationale.replace('\n', "\n   ")
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n"),
            },
            DesignSection {
                path: "design",
                title: "Design",
                section: PlanSection::Design,
                text: self.design.clone(),
            },
            DesignSection {
                path: "verification/automated",
                title: "Verification/Automated",
                section: PlanSection::AutomatedVerification,
                text: self.verification.automated.clone(),
            },
            DesignSection {
                path: "verification/manual",
                title: "Verification/Manual",
                section: PlanSection::ManualVerification,
                text: self.verification.manual.clone(),
            },
        ]
    }

    /// Rejects malformed or oversized metadata while allowing incomplete drafts.
    pub(crate) fn validate(&self) -> Result<()> {
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
        ensure!(
            !self.requirements.is_empty(),
            "plan.json requirements must contain at least one requirement before submission"
        );
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

#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Stores executable commands and observable manual checks separately from their results.
pub struct DesignVerification {
    /// Contains one executable command per nonblank line, without Markdown wrappers.
    pub automated: String,
    /// Contains a Markdown list of actions and expected results.
    pub manual: String,
}

/// Associates projected metadata with its canonical edit and navigation identity.
pub(crate) struct DesignSection {
    pub path: &'static str,
    pub title: &'static str,
    pub section: PlanSection,
    pub text: String,
}
