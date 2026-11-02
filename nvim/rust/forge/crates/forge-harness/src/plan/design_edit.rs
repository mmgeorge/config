use std::path::Path;

use anyhow::{Context, Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::design_document::DesignDecision;
use super::design_flows::DesignFlow;
use super::design_tests::DesignTestFile;
use super::{DesignPatchRequest, PatchField, PlanDocument};

/// Preserves ordered file, overview, and check edits in the existing completion stream.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum DesignEditRequest {
    /// Changes proposed project files while preserving the plan overview.
    File(DesignPatchRequest),
    /// Updates supplied overview fields while preserving project file proposals.
    Overview(PlanOverviewEditRequest),
    /// Selects configured checks without replacing other overview fields.
    Checks(PlanChecksEditRequest),
}

impl DesignEditRequest {
    /// Converts structured edits to the existing atomic document patch representation.
    pub(crate) fn into_patch(
        self,
        document: &PlanDocument,
        workspace: &Path,
    ) -> Result<DesignPatchRequest> {
        let (plan_id, expected_version) = match &self {
            Self::File(request) => (&request.plan_id, request.expected_version),
            Self::Overview(request) => (&request.plan_id, request.expected_version),
            Self::Checks(request) => (&request.plan_id, request.expected_version),
        };
        ensure!(
            plan_id == &document.plan_id,
            "design plan id does not match the active plan"
        );
        ensure!(
            expected_version == document.version,
            "design version conflict: expected_version must be {}",
            document.version
        );
        let (plan_id, expected_version, title, overview) = match self {
            Self::File(request) => return Ok(request),
            Self::Overview(request) => {
                let mut overview = document
                    .design
                    .as_ref()
                    .context("plan has no declaration design")?
                    .document
                    .clone();
                let PlanOverviewEditRequest {
                    plan_id,
                    expected_version,
                    title,
                    objective,
                    usage,
                    requirements,
                    background,
                    decisions,
                    design,
                    flows,
                    verification,
                    tests,
                } = request;
                if let Some(value) = objective {
                    overview.objective = value;
                }
                match usage {
                    PatchField::Missing => {}
                    PatchField::Null => overview.usage = None,
                    PatchField::Value(value) => overview.usage = Some(value),
                }
                if let Some(value) = requirements {
                    overview.requirements = value;
                }
                if let Some(value) = background {
                    overview.background = value;
                }
                if let Some(value) = decisions {
                    overview.decisions = value;
                }
                if let Some(value) = design {
                    overview.design = value;
                }
                if let Some(value) = flows {
                    overview.flows = value;
                }
                if let Some(value) = verification {
                    overview.verification = value;
                }
                if let Some(value) = tests {
                    overview.tests = value;
                }
                (plan_id, expected_version, title, overview)
            }
            Self::Checks(request) => {
                let mut overview = document
                    .design
                    .as_ref()
                    .context("plan has no declaration design")?
                    .document
                    .clone();
                let snapshot = crate::checks::ConfigSnapshot::load(workspace)?;
                snapshot.config.validate_selection(&request.check_ids)?;
                overview.set_checks(
                    &snapshot.config,
                    &request.check_ids.into_iter().map(|id| (id, true)).collect(),
                )?;
                (request.plan_id, request.expected_version, None, overview)
            }
        };
        let before = serde_json::to_string_pretty(
            &document
                .design
                .as_ref()
                .context("plan has no declaration design")?
                .document,
        )?;
        let after = serde_json::to_string_pretty(&overview)?;
        let patch = format!(
            "*** Begin Patch\n*** Update File: plan.json\n@@\n{}\n{}\n*** End Patch",
            before
                .lines()
                .map(|line| format!("-{line}"))
                .collect::<Vec<_>>()
                .join("\n"),
            after
                .lines()
                .map(|line| format!("+{line}"))
                .collect::<Vec<_>>()
                .join("\n"),
        );
        Ok(DesignPatchRequest {
            plan_id,
            expected_version,
            patch,
            title,
            source_digests: Default::default(),
        })
    }
}

impl From<DesignPatchRequest> for DesignEditRequest {
    /// Preserves the existing file-patch representation in the ordered edit stream.
    fn from(request: DesignPatchRequest) -> Self {
        Self::File(request)
    }
}

/// Updates supplied overview fields while retaining omitted fields and selected checks.
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanOverviewEditRequest {
    /// Identifies the active plan.
    pub plan_id: String,
    /// Must match the shared version returned by the last plan read or edit.
    #[schemars(range(min = 1))]
    pub expected_version: u64,
    /// Names the plan for review.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    /// States the requested outcome and scope; required at submission.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub objective: Option<String>,
    /// Shows one caller interaction and its expected result as Markdown, using fenced blocks for text inputs and outputs.
    /// For a CLI, include the full command and expected stdout, exit status, or artifact; describe non-text results briefly.
    /// Omit when no caller-facing behavior changes; null removes an existing example.
    #[serde(default, skip_serializing_if = "usage_is_missing")]
    #[schemars(with = "Option<String>")]
    pub usage: PatchField<String>,
    /// Lists the two to four key acceptance outcomes, or an empty list when the objective suffices.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub requirements: Option<Vec<String>>,
    /// Describes current behavior, relevant paths and symbols, and guarantees to preserve; required at submission.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub background: Option<String>,
    /// Records consequential choices and their reasons.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub decisions: Option<Vec<DesignDecision>>,
    /// Explains the solution, state ownership, failure behavior, and compatibility; required at submission. Describe unsupported or generated file changes here.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub design: Option<String>,
    /// Traces behavior with one initiating event per flow and ordered steps naming the objects or operations involved.
    /// Use separate flows for independent lifecycles and state the values, events, or results passed between owners in the steps and effects.
    /// Harness draws the diagrams; supply structured flows without hand-formatted diagrams, or an empty list when none are needed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub flows: Option<Vec<DesignFlow>>,
    /// Lists manual actions and expected results after automated checks pass; use an empty list when unnecessary.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub verification: Option<Vec<String>>,
    /// Groups intended coverage by file with exact test names, change kinds, scenarios, and expected results. Includes inline and reused tests; does not record executed results.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub tests: Option<Vec<DesignTestFile>>,
}

/// Adds saved check selections through the same ordered plan-edit stream.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanChecksEditRequest {
    /// Identifies the active plan.
    pub plan_id: String,
    /// Prevents applying the selection to a newer plan version.
    pub expected_version: u64,
    /// Names the saved checks to enable along with their prerequisites.
    pub check_ids: Vec<String>,
}

/// Keeps an omitted usage field distinct from an explicit null during replay.
fn usage_is_missing(value: &PatchField<String>) -> bool {
    matches!(value, PatchField::Missing)
}

/// Restricts model file edits to project files rather than the internal overview document.
pub(crate) fn ensure_file_patch(patch: &str) -> Result<()> {
    for line in patch.lines() {
        for prefix in [
            "*** Add File: ",
            "*** Update File: ",
            "*** Delete File: ",
            "*** Move to: ",
        ] {
            if let Some(path) = line.strip_prefix(prefix) {
                ensure!(
                    path != "plan.json",
                    "use plan_overview_edit to change the plan overview"
                );
            }
        }
    }
    Ok(())
}
