//! Retain resume and plan-review caller intent independently of mutable task authority.

use super::{PlanSelection, TaskCommandFailure, TaskSelection};
use crate::task::{
    Task,
    command::{PlanReviewRequest, ResumeRequest},
};
use serde::{Deserialize, Serialize};

/// Resume the selected task with explicit user-supplied continuation text.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "StoredResume", into = "StoredResume")]
pub(crate) struct TaskResumeRequest {
    target: TaskSelection,
    message: Option<String>,
    continuation: Option<String>,
}

impl TaskResumeRequest {
    /// Capture task authority and validate the supported legacy resume payload.
    pub(crate) fn capture(
        task: &Task,
        request: &ResumeRequest,
    ) -> Result<Self, TaskCommandFailure> {
        if request.lifecycle.is_some() || !request.metadata.is_empty() {
            return Err(TaskCommandFailure::InvalidPlanSelection);
        }
        Self::try_from(StoredResume {
            target: TaskSelection::capture(task),
            message: request.message.clone(),
            continuation: request.continuation.clone(),
        })
    }
    /// Get the exact task revision selected before queue admission.
    pub(crate) fn target(&self) -> &TaskSelection {
        &self.target
    }
    /// Recreate the existing domain preparation request without transport metadata.
    pub(crate) fn request(&self) -> ResumeRequest {
        ResumeRequest {
            message: self.message.clone(),
            lifecycle: None,
            continuation: self.continuation.clone(),
            metadata: Default::default(),
        }
    }
    /// Match a retry's original intent without recapturing the current task.
    pub(crate) fn matches_request(&self, request: &ResumeRequest) -> bool {
        request.lifecycle.is_none()
            && request.metadata.is_empty()
            && self.message == request.message
            && self.continuation == request.continuation
    }
}

impl TryFrom<StoredResume> for TaskResumeRequest {
    type Error = TaskCommandFailure;
    /// Reject blank explicit instructions on transport and checkpoint recovery.
    fn try_from(stored: StoredResume) -> Result<Self, Self::Error> {
        let StoredResume {
            target,
            message,
            continuation,
        } = stored;
        if message
            .iter()
            .chain(continuation.iter())
            .any(|text| text.trim().is_empty())
        {
            return Err(TaskCommandFailure::InvalidObjective);
        }
        Ok(Self {
            target,
            message,
            continuation,
        })
    }
}

impl From<TaskResumeRequest> for StoredResume {
    /// Preserve both caller text fields and their original captured target.
    fn from(request: TaskResumeRequest) -> Self {
        let TaskResumeRequest {
            target,
            message,
            continuation,
        } = request;
        Self {
            target,
            message,
            continuation,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredResume {
    target: TaskSelection,
    message: Option<String>,
    continuation: Option<String>,
}

/// Request a revision of the captured saved plan without retargeting current selection.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct PlanChangeRequest(PlanReviewSelection);

impl PlanChangeRequest {
    /// Capture the exact reviewed artifact, task authority, and caller feedback.
    pub(crate) fn capture(
        plan: &crate::plan::PlanRecord,
        current: Option<&Task>,
        request: &PlanReviewRequest,
    ) -> Result<Self, TaskCommandFailure> {
        Ok(Self(PlanReviewSelection::capture(plan, current, request)?))
    }
    /// Get captured source and feedback for guarded preparation.
    pub(crate) fn selection(&self) -> &PlanReviewSelection {
        &self.0
    }
}

/// Reject the captured plan revision while preserving unrelated task ownership.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct PlanCancellation(PlanReviewSelection);

impl PlanCancellation {
    /// Capture rejection authority before any collection or artifact restoration starts.
    pub(crate) fn capture(
        plan: &crate::plan::PlanRecord,
        current: Option<&Task>,
        request: &PlanReviewRequest,
    ) -> Result<Self, TaskCommandFailure> {
        Ok(Self(PlanReviewSelection::capture(plan, current, request)?))
    }
    /// Get the selected plan and immutable original feedback.
    pub(crate) fn selection(&self) -> &PlanReviewSelection {
        &self.0
    }
}

/// Share source guards and feedback validation between explicit revision and rejection commands.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "StoredReviewSelection", into = "StoredReviewSelection")]
pub(crate) struct PlanReviewSelection {
    source: PlanSelection,
    prior: Option<TaskSelection>,
    request: ReviewFeedback,
}

impl PlanReviewSelection {
    /// Capture the source and original caller payload without requiring an executable plan.
    fn capture(
        plan: &crate::plan::PlanRecord,
        current: Option<&Task>,
        request: &PlanReviewRequest,
    ) -> Result<Self, TaskCommandFailure> {
        if !request.metadata.is_empty() {
            return Err(TaskCommandFailure::InvalidPlanSelection);
        }
        Self::try_from(StoredReviewSelection {
            source: PlanSelection::capture(plan)?,
            prior: current.map(TaskSelection::capture),
            request: ReviewFeedback {
                plan_id: request.plan_id.clone(),
                digest: request.digest.clone(),
                saved_source_digest: request.saved_source_digest.clone(),
                approval_mode: request.approval_mode,
                comment: request.comment.clone(),
                annotations: request.annotations.clone(),
            },
        })
    }
    /// Get exact saved-plan content and review authority.
    pub(crate) fn source(&self) -> &PlanSelection {
        &self.source
    }
    /// Get task authority captured alongside the saved plan.
    pub(crate) fn prior(&self) -> Option<&TaskSelection> {
        self.prior.as_ref()
    }
    /// Require the captured replacement or preservation target before any effects.
    pub(crate) fn validate_target(&self, current: Option<&Task>) -> Result<(), TaskCommandFailure> {
        match &self.prior {
            Some(target) => target.validate(current),
            None if current.is_none() => Ok(()),
            None => Err(TaskCommandFailure::Changed),
        }
    }
    /// Get original caller feedback for immutable retry normalization.
    pub(crate) fn request(&self) -> PlanReviewRequest {
        PlanReviewRequest {
            plan_id: self.request.plan_id.clone(),
            digest: self.request.digest.clone(),
            saved_source_digest: self.request.saved_source_digest.clone(),
            approval_mode: self.request.approval_mode,
            comment: self.request.comment.clone(),
            annotations: self.request.annotations.clone(),
            metadata: Default::default(),
        }
    }

    /// Get normalized feedback for existing domain preparation without mutable source lookup.
    pub(crate) fn normalized_request(&self) -> PlanReviewRequest {
        PlanReviewRequest {
            plan_id: Some(self.source.id().as_str().into()),
            digest: self.source.review_digest().map(str::to_owned),
            saved_source_digest: self.request.saved_source_digest.clone(),
            approval_mode: self.request.approval_mode,
            comment: self.request.comment.clone(),
            annotations: self.request.annotations.clone(),
            metadata: Default::default(),
        }
    }
    /// Match identified retry intent without consulting current plan or task state.
    pub(crate) fn matches_request(&self, request: &PlanReviewRequest) -> bool {
        request.metadata.is_empty()
            && self.request
                == ReviewFeedback {
                    plan_id: request.plan_id.clone(),
                    digest: request.digest.clone(),
                    saved_source_digest: request.saved_source_digest.clone(),
                    approval_mode: request.approval_mode,
                    comment: request.comment.clone(),
                    annotations: request.annotations.clone(),
                }
    }
}

impl TryFrom<StoredReviewSelection> for PlanReviewSelection {
    type Error = TaskCommandFailure;
    /// Validate explicit identity, digest, and annotation ranges during admission and recovery.
    fn try_from(stored: StoredReviewSelection) -> Result<Self, Self::Error> {
        let StoredReviewSelection {
            source,
            prior,
            request,
        } = stored;
        if request
            .plan_id
            .as_deref()
            .is_some_and(|id| id != "last" && id != source.id().as_str())
            || request.annotations.iter().any(|annotation| {
                annotation.start_line == 0
                    || annotation.start_line > annotation.end_line
                    || annotation.body.trim().is_empty()
            })
            || request
                .saved_source_digest
                .as_ref()
                .is_some_and(|digest| digest.trim().is_empty())
        {
            return Err(TaskCommandFailure::InvalidPlanSelection);
        }
        if let Some(digest) = &request.digest
            && source.review_digest() != Some(digest.as_str())
        {
            return Err(TaskCommandFailure::PlanChanged);
        }
        Ok(Self {
            source,
            prior,
            request,
        })
    }
}

impl From<PlanReviewSelection> for StoredReviewSelection {
    /// Preserve the original review intent and captured task independently.
    fn from(selection: PlanReviewSelection) -> Self {
        let PlanReviewSelection {
            source,
            prior,
            request,
        } = selection;
        Self {
            source,
            prior,
            request,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredReviewSelection {
    source: PlanSelection,
    prior: Option<TaskSelection>,
    request: ReviewFeedback,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
struct ReviewFeedback {
    plan_id: Option<String>,
    digest: Option<String>,
    saved_source_digest: Option<String>,
    approval_mode: Option<crate::session::ApprovalMode>,
    comment: Option<String>,
    annotations: Vec<crate::plan::PlanAnnotationInput>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Resume retries retain the original task while distinct caller text conflicts.
    #[test]
    fn resume_request_preserves_captured_target_and_validates_recovery() -> anyhow::Result<()> {
        let (_root, _store, controller) = crate::task::SessionTaskController::test_goal();
        let task = controller.current()?.unwrap();
        let request = ResumeRequest {
            message: Some("Finish the selected task".into()),
            ..Default::default()
        };
        let captured = TaskResumeRequest::capture(&task, &request)?;
        assert!(captured.matches_request(&request));
        let mut changed = request.clone();
        changed.message = Some("Different request".into());
        assert!(!captured.matches_request(&changed));
        let mut stored = serde_json::to_value(&captured)?;
        stored["message"] = serde_json::json!(" ");
        assert!(serde_json::from_value::<TaskResumeRequest>(stored).is_err());
        Ok(())
    }

    /// Review rejection keeps original source authority when a later revision appears.
    #[test]
    fn review_selection_rejects_source_changes_and_compares_original_intent() -> anyhow::Result<()>
    {
        let mut plan: crate::plan::PlanRecord = serde_json::from_value(serde_json::json!({
            "id":"plan","session_id":"session","request":"Design","state":"awaiting_review",
            "working_path":"working.json","document_version":1,"model_revision":1,"user_revision":0,
            "review_digest":"digest","created_at_ms":1,"updated_at_ms":1
        }))?;
        let request = PlanReviewRequest {
            comment: Some("Clarify scope".into()),
            ..Default::default()
        };
        let selection = PlanCancellation::capture(&plan, None, &request)?;
        plan.document_version += 1;
        assert!(selection.selection().source().validate(&plan).is_err());
        assert!(selection.selection().matches_request(&request));
        assert_eq!(
            selection
                .selection()
                .normalized_request()
                .plan_id
                .as_deref(),
            Some("plan")
        );
        Ok(())
    }
}
