use super::document::PlanDocument;
use super::edit::{PlanEditOperation, PlanEditRequest, apply_plan_edit};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Defines whether one divergence changes accepted scope.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanDeviationKind {
    Informational,
    Scope,
}

/// Defines the configured review policy for scope deviations.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ScopeDeviationReview {
    #[default]
    Auto,
    Prompt,
}

/// Defines the final review disposition of one deviation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanDeviationDisposition {
    Recorded,
    Pending,
    AutoApproved,
    UserApproved,
    Rejected,
}

/// Represents one execution-time divergence from accepted intent.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanDeviation {
    pub id: String,
    pub plan_id: String,
    pub execution_id: String,
    pub kind: PlanDeviationKind,
    pub disposition: PlanDeviationDisposition,
    pub summary: String,
    pub reason: String,
    pub task_id: Option<String>,
    pub subtask_id: Option<String>,
    #[serde(default)]
    pub affected_paths: Vec<String>,
    #[serde(default)]
    pub proposed_operations: Vec<PlanEditOperation>,
    pub created_at_ms: i64,
    pub resolved_at_ms: Option<i64>,
}

/// Carries one model-authored divergence before Harness assigns lifecycle metadata.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanDeviationRequest {
    pub plan_id: String,
    pub kind: PlanDeviationKind,
    pub summary: String,
    pub reason: String,
    pub task_id: Option<String>,
    pub subtask_id: Option<String>,
    #[serde(default)]
    pub affected_paths: Vec<String>,
    #[serde(default)]
    pub proposed_operations: Vec<PlanEditOperation>,
}

impl PlanDeviation {
    /// Validate deviation evidence before persistence or review.
    pub fn validate(&self) -> Result<()> {
        anyhow::ensure!(
            !self.summary.trim().is_empty(),
            "deviation summary cannot be empty"
        );
        anyhow::ensure!(
            !self.reason.trim().is_empty(),
            "deviation reason cannot be empty"
        );
        match self.kind {
            PlanDeviationKind::Informational => anyhow::ensure!(
                self.proposed_operations.is_empty(),
                "informational deviation cannot change the plan"
            ),
            PlanDeviationKind::Scope => anyhow::ensure!(
                !self.proposed_operations.is_empty(),
                "scope deviation requires semantic operations"
            ),
        }
        Ok(())
    }
}

/// Represents accepted intent plus every approved scope overlay.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct EffectivePlan {
    pub accepted: PlanDocument,
    pub document: PlanDocument,
    pub deviation_ids: Vec<String>,
}

/// Build the execution view without mutating the accepted revision.
pub fn build_effective_plan(
    accepted: &PlanDocument,
    deviation_list: &[PlanDeviation],
) -> Result<EffectivePlan> {
    let mut document = accepted.clone();
    let mut deviation_ids = Vec::new();
    for deviation in deviation_list {
        deviation.validate()?;
        if deviation.kind != PlanDeviationKind::Scope
            || !matches!(
                deviation.disposition,
                PlanDeviationDisposition::AutoApproved | PlanDeviationDisposition::UserApproved
            )
        {
            continue;
        }
        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: document.plan_id.clone(),
                expected_version: document.version,
                operations: deviation.proposed_operations.clone(),
            },
        )?;
        document = result.document;
        deviation_ids.push(deviation.id.clone());
    }
    Ok(EffectivePlan {
        accepted: accepted.clone(),
        document,
        deviation_ids,
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn overlays_only_approved_scope_deviations() {
        let accepted = super::super::document::test_fixture("plan", "Accepted");
        let deviation = PlanDeviation {
            id: "deviation".into(),
            plan_id: "plan".into(),
            execution_id: "execution".into(),
            kind: PlanDeviationKind::Scope,
            disposition: PlanDeviationDisposition::AutoApproved,
            summary: "Change overview".into(),
            reason: "Repository evidence changed.".into(),
            task_id: Some("task".into()),
            subtask_id: None,
            affected_paths: Vec::new(),
            proposed_operations: vec![PlanEditOperation::OverviewUpdate {
                text: "Effective".into(),
            }],
            created_at_ms: 1,
            resolved_at_ms: Some(1),
        };
        let effective = build_effective_plan(&accepted, &[deviation]).unwrap();
        assert_eq!(effective.accepted.overview, "Accepted");
        assert_eq!(effective.document.overview, "Effective");
        assert_eq!(effective.deviation_ids, ["deviation"]);
    }
}
