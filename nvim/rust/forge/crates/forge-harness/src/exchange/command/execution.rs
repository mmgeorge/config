//! Capture approval intent independently of mutable plan selection and task replacement.

use serde::{Deserialize, Serialize};

use super::{ApprovalQuestionSetId, PlanSelection, TaskCommandFailure, TaskSelection};
use crate::{
    plan::PlanRecord,
    task::{Task, command::ExecuteSelection},
};

/// Bind an execute request to its selected plan/evidence and the task it may replace.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(
    try_from = "StoredExecutionSelection",
    into = "StoredExecutionSelection"
)]
pub(crate) struct PlanExecutionSelection {
    source: PlanSelection,
    prior: Option<TaskSelection>,
    request: ExecuteSelection,
}

impl PlanExecutionSelection {
    /// Capture the source and replacement authority once before asynchronous command admission.
    pub(crate) fn capture(
        plan: &PlanRecord,
        current: Option<&Task>,
        request: ExecuteSelection,
    ) -> Result<Self, TaskCommandFailure> {
        Self::try_from(StoredExecutionSelection {
            source: PlanSelection::capture(plan)?,
            prior: current.map(TaskSelection::capture),
            request,
        })
    }

    /// Get the exact source/evidence guard revalidated before effects and within the domain commit.
    pub(crate) fn source(&self) -> &PlanSelection {
        &self.source
    }

    /// Get the captured task replacement target, including the absence of a task.
    pub(crate) fn prior(&self) -> Option<&TaskSelection> {
        self.prior.as_ref()
    }

    /// Get immutable caller intent so a retry can be compared without recapturing current state.
    pub(crate) fn request(&self) -> &ExecuteSelection {
        &self.request
    }

    /// Require the task selected at admission, rather than replacing whichever task is current later.
    pub(crate) fn validate_target(&self, current: Option<&Task>) -> Result<(), TaskCommandFailure> {
        match &self.prior {
            Some(prior) => prior.validate(current),
            None if current.is_none() => Ok(()),
            None => Err(TaskCommandFailure::Changed),
        }
    }

    /// Resolve legacy latest-plan syntax to this captured source without selecting again in a worker.
    pub(crate) fn normalized_request(&self) -> ExecuteSelection {
        let mut request = self.request.clone();
        request.plan_id = Some(self.source.id().as_str().into());
        request.digest = Some(
            self.source
                .execution_digest()
                .expect("execution selection construction and recovery validate a submitted source")
                .into(),
        );
        request
    }
}

impl TryFrom<StoredExecutionSelection> for PlanExecutionSelection {
    type Error = TaskCommandFailure;
    /// Validate source, explicit identity/digest, and local review ranges whenever intent is decoded.
    fn try_from(stored: StoredExecutionSelection) -> Result<Self, Self::Error> {
        let StoredExecutionSelection {
            source,
            prior,
            request,
        } = stored;
        let digest = source.execution_digest()?;
        if request
            .plan_id
            .as_deref()
            .is_some_and(|id| id != "last" && id != source.id().as_str())
            || request
                .digest
                .as_deref()
                .is_some_and(|expected| expected != digest)
        {
            return Err(TaskCommandFailure::PlanChanged);
        }
        if request
            .saved_source_digest
            .as_ref()
            .is_some_and(|digest| digest.trim().is_empty())
            || request.annotations.iter().any(|annotation| {
                annotation.start_line == 0
                    || annotation.start_line > annotation.end_line
                    || annotation.body.trim().is_empty()
            })
        {
            return Err(TaskCommandFailure::InvalidPlanSelection);
        }
        Ok(Self {
            source,
            prior,
            request,
        })
    }
}

impl From<PlanExecutionSelection> for StoredExecutionSelection {
    /// Preserve original caller intent separately from its immutable captured authority.
    fn from(selection: PlanExecutionSelection) -> Self {
        let PlanExecutionSelection {
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
struct StoredExecutionSelection {
    source: PlanSelection,
    prior: Option<TaskSelection>,
    request: ExecuteSelection,
}

/// Bind pending approval to the exact committed question set and answer revision.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PlanQuestionSetSelection {
    id: ApprovalQuestionSetId,
    revision: std::num::NonZeroU32,
    answer_revision: u64,
}

impl PlanQuestionSetSelection {
    /// Capture the committed question identity and revision without retargeting later answers.
    pub(crate) fn capture(
        elicitation: &crate::plan::PlanElicitation,
    ) -> Result<Self, TaskCommandFailure> {
        Ok(Self {
            id: ApprovalQuestionSetId::new(elicitation.question_set.id.clone())
                .map_err(|_| TaskCommandFailure::InvalidPlanSelection)?,
            revision: std::num::NonZeroU32::new(elicitation.revision)
                .ok_or(TaskCommandFailure::InvalidPlanSelection)?,
            answer_revision: elicitation.answer_revision,
        })
    }

    /// Require the same question content and answer revision before a continuation may apply.
    pub(crate) fn validate(
        &self,
        elicitation: &crate::plan::PlanElicitation,
    ) -> Result<(), TaskCommandFailure> {
        if self.id.as_str() != elicitation.question_set.id
            || self.revision.get() != elicitation.revision
            || self.answer_revision != elicitation.answer_revision
        {
            return Err(TaskCommandFailure::PlanChanged);
        }
        Ok(())
    }

    /// Get the exact pending question set identity.
    pub(crate) fn id(&self) -> &ApprovalQuestionSetId {
        &self.id
    }

    /// Get the committed question and answer revision.
    pub(crate) fn revision(&self) -> u32 {
        self.revision.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Retained execution intent must neither select a newer plan nor widen absent/replaced task authority.
    #[test]
    fn captured_execution_rejects_changed_targets_and_revalidates_recovered_intent()
    -> anyhow::Result<()> {
        let plan: PlanRecord = serde_json::from_value(json!({
            "id":"selected","session_id":"session","request":"Design","state":"awaiting_review",
            "working_path":"working.json","document_version":1,"model_revision":1,"user_revision":0,
            "review_digest":"digest","created_at_ms":1,"updated_at_ms":1,
        }))?;
        let (_root, _store, controller) = crate::task::SessionTaskController::test_goal();
        let task = controller.current()?.unwrap();
        let request = ExecuteSelection::default();
        let existing = PlanExecutionSelection::capture(&plan, Some(&task), request.clone())?;
        assert!(matches!(
            existing.validate_target(None),
            Err(TaskCommandFailure::MissingTask)
        ));
        let absent = PlanExecutionSelection::capture(&plan, None, request)?;
        assert!(matches!(
            absent.validate_target(Some(&task)),
            Err(TaskCommandFailure::Changed)
        ));
        let mut changed = plan.clone();
        changed.review_digest = Some("new-digest".into());
        assert!(matches!(
            absent.source().validate(&changed),
            Err(TaskCommandFailure::PlanChanged)
        ));
        let restored: PlanExecutionSelection =
            serde_json::from_value(serde_json::to_value(&existing)?)?;
        assert_eq!(restored, existing);
        assert_eq!(
            restored.normalized_request().plan_id.as_deref(),
            Some("selected")
        );
        assert_eq!(
            restored.normalized_request().digest.as_deref(),
            Some("digest")
        );
        let mut invalid = serde_json::to_value(&existing)?;
        invalid["source"]["state"] = json!("generating");
        assert!(serde_json::from_value::<PlanExecutionSelection>(invalid).is_err());
        let mut invalid = serde_json::to_value(&existing)?;
        invalid["request"]["annotations"] =
            json!([{"start_line":0,"end_line":1,"body":"Invalid range"}]);
        assert!(serde_json::from_value::<PlanExecutionSelection>(invalid).is_err());
        Ok(())
    }
}
