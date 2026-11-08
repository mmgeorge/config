//! Capture direct plan-document edits without confusing them with source-file edits.

use super::{PlanId, PlanSelection, TaskCommandFailure, TaskSelection};
use crate::{plan::PlanRecord, task::Task};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Identify an entity by semantic symbol or its newly planned name.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", content = "identity", rename_all = "snake_case")]
pub(crate) enum PlanEntitySelection {
    /// An existing semantic identity supplied by the plan review.
    Symbol(String),
    /// A newly added entity selected by its current name.
    AddedEntity(String),
}

/// Change one coherent section of the reviewed plan document.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "change", rename_all = "snake_case")]
pub(crate) enum PlanEditChange {
    /// Rename plan declarations and their internal references.
    RenameEntity {
        target: PlanEntitySelection,
        name: String,
    },
    /// Remove selected planned tests without deleting source files.
    DeleteTests {
        tests: Vec<crate::plan::DesignTestSelection>,
    },
    /// Update enabled checks and remove selected planned tests.
    UpdateChecks {
        enabled: BTreeMap<String, bool>,
        tests: Vec<crate::plan::DesignTestSelection>,
    },
}

/// Retain explicit caller guards separately from captured source authority for retry comparison.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PlanEditRequest {
    /// Saved plan selected by the review UI.
    pub(crate) plan: PlanId,
    /// Explicit document revision, omitted only by old rename callers.
    pub(crate) expected_version: Option<u64>,
    /// Explicit submitted digest, omitted only by old rename callers.
    pub(crate) digest: Option<String>,
    /// Complete edit intent without source-file writes or execution policy.
    pub(crate) change: PlanEditChange,
}

/// Bind a direct edit to its saved source and observed task authority.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "StoredPlanEdit", into = "StoredPlanEdit")]
pub(crate) struct PlanEdit {
    source: PlanSelection,
    prior: Option<TaskSelection>,
    request: PlanEditRequest,
}

impl PlanEdit {
    /// Capture current authority before the edit waits in the owner FIFO.
    pub(crate) fn capture(
        plan: &PlanRecord,
        current: Option<&Task>,
        request: PlanEditRequest,
    ) -> Result<Self, TaskCommandFailure> {
        Self::try_from(StoredPlanEdit {
            source: PlanSelection::capture(plan)?,
            prior: current.map(TaskSelection::capture),
            request,
        })
    }
    /// Get the exact saved source guarded throughout preparation and commit.
    pub(crate) fn source(&self) -> &PlanSelection {
        &self.source
    }
    /// Get the task selected at admission, including explicit absence.
    pub(crate) fn prior(&self) -> Option<&TaskSelection> {
        self.prior.as_ref()
    }
    /// Get the original immutable edit payload for dispatch and retry comparison.
    pub(crate) fn request(&self) -> &PlanEditRequest {
        &self.request
    }
    /// Reject changed task authority without applying the edit to a newly attached task.
    pub(crate) fn validate_target(&self, current: Option<&Task>) -> Result<(), TaskCommandFailure> {
        match &self.prior {
            Some(target) => target.validate(current),
            None if current.is_none() => Ok(()),
            None => Err(TaskCommandFailure::Changed),
        }
    }
}

impl TryFrom<StoredPlanEdit> for PlanEdit {
    type Error = TaskCommandFailure;
    /// Validate every serialized edit and require its explicit guards to match the captured source.
    fn try_from(stored: StoredPlanEdit) -> Result<Self, Self::Error> {
        let StoredPlanEdit {
            source,
            prior,
            request,
        } = stored;
        if source.id() != &request.plan
            || request
                .expected_version
                .is_some_and(|version| version != source.document_version())
            || request
                .digest
                .as_deref()
                .is_some_and(|digest| source.review_digest() != Some(digest))
        {
            return Err(TaskCommandFailure::PlanChanged);
        }
        source.validate_edit()?;
        let tests = match &request.change {
            PlanEditChange::RenameEntity { target, name } => {
                let identity = match target {
                    PlanEntitySelection::Symbol(identity)
                    | PlanEntitySelection::AddedEntity(identity) => identity,
                };
                if identity.trim().is_empty() || name.trim().is_empty() {
                    return Err(TaskCommandFailure::InvalidPlanSelection);
                }
                if matches!(target, PlanEntitySelection::Symbol(_))
                    && request.expected_version.is_none()
                {
                    return Err(TaskCommandFailure::InvalidPlanSelection);
                }
                &[][..]
            }
            PlanEditChange::DeleteTests { tests } => {
                if request.expected_version.is_none() || request.digest.is_none() {
                    return Err(TaskCommandFailure::InvalidPlanSelection);
                }
                if tests.is_empty() {
                    return Err(TaskCommandFailure::InvalidPlanSelection);
                }
                tests.as_slice()
            }
            PlanEditChange::UpdateChecks { enabled, tests } => {
                if request.expected_version.is_none() || request.digest.is_none() {
                    return Err(TaskCommandFailure::InvalidPlanSelection);
                }
                if enabled.keys().any(|id| id.trim().is_empty()) {
                    return Err(TaskCommandFailure::InvalidPlanSelection);
                }
                tests.as_slice()
            }
        };
        if tests
            .iter()
            .any(|test| test.file.trim().is_empty() || test.name.trim().is_empty())
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

impl From<PlanEdit> for StoredPlanEdit {
    /// Preserve captured authority alongside the original caller guards.
    fn from(edit: PlanEdit) -> Self {
        let PlanEdit {
            source,
            prior,
            request,
        } = edit;
        Self {
            source,
            prior,
            request,
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct StoredPlanEdit {
    source: PlanSelection,
    prior: Option<TaskSelection>,
    request: PlanEditRequest,
}
