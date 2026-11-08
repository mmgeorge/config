//! Capture saved-plan authority without selecting a newer revision when queued work starts.

use std::num::NonZeroU32;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use super::{PlanId, TaskCommandFailure};
use crate::{
    plan::{PlanRecord, PlanState},
    session::SessionStore,
    storage::SqliteStore,
};

/// Select one saved plan at the exact document, model, user, and review revision observed by its caller.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawPlanSelection", into = "RawPlanSelection")]
pub(crate) struct PlanSelection {
    id: PlanId,
    session_id: String,
    document_version: u64,
    model_revision: u32,
    user_revision: u32,
    state: PlanState,
    accepted_revision: Option<u32>,
    review_digest: Option<String>,
    review_evidence: Option<ReviewFingerprint>,
}

impl PlanSelection {
    /// Capture validated authority from a saved artifact rather than a caller-supplied latest-plan flag.
    pub(crate) fn capture(plan: &PlanRecord) -> Result<Self, TaskCommandFailure> {
        Self::try_from(RawPlanSelection {
            id: PlanId::new(plan.id.clone())
                .map_err(|_| TaskCommandFailure::InvalidPlanSelection)?,
            session_id: plan.session_id.clone(),
            document_version: plan.document_version,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            state: plan.state,
            accepted_revision: plan.accepted_revision,
            review_digest: plan.review_digest.clone(),
            review_evidence: ReviewFingerprint::capture(plan)?,
        })
    }

    /// Get the selected artifact's validated identity.
    pub(crate) fn id(&self) -> &PlanId {
        &self.id
    }

    /// Get the exact document revision captured before queue admission.
    pub(crate) fn document_version(&self) -> u64 {
        self.document_version
    }

    /// Get the captured submitted digest for revision-checked review requests.
    pub(crate) fn review_digest(&self) -> Option<&str> {
        self.review_digest.as_deref()
    }

    /// Require an editable submitted plan rather than a historical accepted revision.
    pub(crate) fn validate_edit(&self) -> Result<(), TaskCommandFailure> {
        if self.state != PlanState::AwaitingReview {
            return Err(TaskCommandFailure::PlanChanged);
        }
        self.execution_digest().map(|_| ())
    }

    /// Require a submitted reviewable source before capturing an execution command.
    pub(crate) fn execution_digest(&self) -> Result<&str, TaskCommandFailure> {
        if self.model_revision == 0
            || !matches!(self.state, PlanState::AwaitingReview | PlanState::Accepted)
        {
            return Err(TaskCommandFailure::PlanRevisionUnavailable);
        }
        self.review_digest
            .as_deref()
            .ok_or(TaskCommandFailure::PlanRevisionUnavailable)
    }

    /// Require a live unaccepted source before attaching its existing lineage to this session task.
    pub(crate) fn validate_attachment(&self) -> Result<(), TaskCommandFailure> {
        if self.accepted_revision.is_some() {
            return Err(TaskCommandFailure::PlanRequiresFork);
        }
        match self.state {
            PlanState::Accepted => Err(TaskCommandFailure::PlanRequiresFork),
            PlanState::Cancelled | PlanState::Rejected => {
                Err(TaskCommandFailure::PlanAttachmentUnavailable)
            }
            PlanState::ConfiguringChecks
            | PlanState::Generating
            | PlanState::AwaitingInput
            | PlanState::AwaitingReview
            | PlanState::Revising
            | PlanState::Failed => Ok(()),
        }
    }

    /// Reject changed content or approval state without retargeting to the newest stored artifact.
    pub(crate) fn validate(&self, plan: &PlanRecord) -> Result<(), TaskCommandFailure> {
        if self.id.as_str() != plan.id
            || self.session_id != plan.session_id
            || self.document_version != plan.document_version
            || self.model_revision != plan.model_revision
            || self.user_revision != plan.user_revision
            || self.state != plan.state
            || self.accepted_revision != plan.accepted_revision
            || self.review_digest != plan.review_digest
            || self.review_evidence != ReviewFingerprint::capture(plan)?
        {
            return Err(TaskCommandFailure::PlanChanged);
        }
        Ok(())
    }

    /// Check saved content and scope before collection, after collection, and before task commit.
    fn validate_storage(
        &self,
        store: &SqliteStore,
        session_id: &str,
        scope: PlanSourceScope,
    ) -> Result<(), TaskCommandFailure> {
        let plan = store
            .load_plan(self.id.as_str())
            .map_err(|_| TaskCommandFailure::StateUnavailable)?
            .ok_or(TaskCommandFailure::PlanMissing)?;
        self.validate(&plan)?;
        self.validate_scope(store, session_id, scope)
    }

    /// Validate source ownership independently of mutable working-plan metadata.
    fn validate_scope(
        &self,
        store: &SqliteStore,
        session_id: &str,
        scope: PlanSourceScope,
    ) -> Result<(), TaskCommandFailure> {
        if self.session_id == session_id {
            return Ok(());
        }
        match scope {
            PlanSourceScope::Session => Err(TaskCommandFailure::PlanForeignSession),
            PlanSourceScope::Workspace => {
                let owner = store
                    .load_session(&self.session_id)
                    .map_err(|_| TaskCommandFailure::StateUnavailable)?
                    .ok_or(TaskCommandFailure::PlanMissing)?;
                let current = store
                    .load_session(session_id)
                    .map_err(|_| TaskCommandFailure::StateUnavailable)?
                    .ok_or(TaskCommandFailure::StateUnavailable)?;
                if owner.workspace != current.workspace {
                    return Err(TaskCommandFailure::PlanForeignWorkspace);
                }
                Ok(())
            }
        }
    }

    /// Validate the stable artifact owner used by an immutable submitted revision.
    fn validate_identity(&self, plan: &PlanRecord) -> Result<(), TaskCommandFailure> {
        if self.id.as_str() != plan.id || self.session_id != plan.session_id {
            return Err(TaskCommandFailure::PlanChanged);
        }
        Ok(())
    }
}

impl TryFrom<RawPlanSelection> for PlanSelection {
    type Error = TaskCommandFailure;
    /// Validate saved or external selection metadata without treating opaque digests as protocol strings.
    fn try_from(raw: RawPlanSelection) -> Result<Self, Self::Error> {
        let RawPlanSelection {
            id,
            session_id,
            document_version,
            model_revision,
            user_revision,
            state,
            accepted_revision,
            review_digest,
            review_evidence,
        } = raw;
        if session_id.trim().is_empty()
            || accepted_revision == Some(0)
            || review_digest
                .as_ref()
                .is_some_and(|digest| digest.trim().is_empty())
        {
            return Err(TaskCommandFailure::InvalidPlanSelection);
        }
        Ok(Self {
            id,
            session_id,
            document_version,
            model_revision,
            user_revision,
            state,
            accepted_revision,
            review_digest,
            review_evidence,
        })
    }
}

impl From<PlanSelection> for RawPlanSelection {
    /// Preserve captured authority without recomputing any current plan revision.
    fn from(selection: PlanSelection) -> Self {
        let PlanSelection {
            id,
            session_id,
            document_version,
            model_revision,
            user_revision,
            state,
            accepted_revision,
            review_digest,
            review_evidence,
        } = selection;
        Self {
            id,
            session_id,
            document_version,
            model_revision,
            user_revision,
            state,
            accepted_revision,
            review_digest,
            review_evidence,
        }
    }
}

/// Select an existing submitted revision as the source of a new planning lineage.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(
    try_from = "RawPlanRevisionSelection",
    into = "RawPlanRevisionSelection"
)]
pub(crate) struct PlanRevisionSelection {
    source: PlanSelection,
    revision: NonZeroU32,
}

impl PlanRevisionSelection {
    /// Require a positive revision that existed in the captured source artifact.
    pub(crate) fn new(source: PlanSelection, revision: u32) -> Result<Self, TaskCommandFailure> {
        let revision =
            NonZeroU32::new(revision).ok_or(TaskCommandFailure::PlanRevisionUnavailable)?;
        if revision.get() > source.model_revision {
            return Err(TaskCommandFailure::PlanRevisionUnavailable);
        }
        Ok(Self { source, revision })
    }

    /// Get the saved artifact authority that must remain unchanged until installation.
    pub(crate) fn source(&self) -> &PlanSelection {
        &self.source
    }

    /// Get the positive historical model revision requested by the caller.
    pub(crate) fn revision(&self) -> u32 {
        self.revision.get()
    }
}

impl TryFrom<RawPlanRevisionSelection> for PlanRevisionSelection {
    type Error = TaskCommandFailure;
    /// Revalidate historical revision bounds when loading a request or command journal.
    fn try_from(raw: RawPlanRevisionSelection) -> Result<Self, Self::Error> {
        Self::new(raw.source, raw.revision)
    }
}

impl From<PlanRevisionSelection> for RawPlanRevisionSelection {
    /// Preserve the originally selected historical revision.
    fn from(selection: PlanRevisionSelection) -> Self {
        Self {
            source: selection.source,
            revision: selection.revision.get(),
        }
    }
}

/// Bound a source plan to the requesting session or permit a saved revision in the same workspace.
#[derive(Clone, Copy)]
enum PlanSourceScope {
    /// Attachments and working-plan forks must belong to this exact conversation.
    Session,
    /// Replanning may read another conversation's saved revision in the same workspace.
    Workspace,
}

/// Preserve the different authority of a mutable working document and an immutable submitted revision.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum PlanSourceGuard {
    /// Fork the exact current document and approval state observed at admission in this session.
    Working(PlanSelection),
    /// Read the selected immutable submitted revision; later working revisions do not retarget it.
    Historical(PlanRevisionSelection),
}

impl PlanSourceGuard {
    /// Get the source artifact identity for transactional lookup.
    pub(crate) fn id(&self) -> &PlanId {
        match self {
            Self::Working(source) => source.id(),
            Self::Historical(selection) => selection.source().id(),
        }
    }

    /// Revalidate only the authority required by this kind of source.
    pub(crate) fn validate(&self, plan: &PlanRecord) -> Result<(), TaskCommandFailure> {
        match self {
            Self::Working(source) => source.validate(plan),
            Self::Historical(selection) => selection.source.validate_identity(plan),
        }
    }

    /// Reject foreign sources and stale working selections before any lifecycle or filesystem effect.
    pub(crate) fn validate_storage(
        &self,
        store: &SqliteStore,
        session_id: &str,
    ) -> Result<(), TaskCommandFailure> {
        match self {
            Self::Working(source) => {
                source.validate_storage(store, session_id, PlanSourceScope::Session)
            }
            Self::Historical(selection) => {
                let plan = store
                    .load_plan(selection.source.id().as_str())
                    .map_err(|_| TaskCommandFailure::StateUnavailable)?
                    .ok_or(TaskCommandFailure::PlanMissing)?;
                selection.source.validate_identity(&plan)?;
                selection
                    .source
                    .validate_scope(store, session_id, PlanSourceScope::Workspace)
            }
        }
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RawPlanSelection {
    id: PlanId,
    session_id: String,
    document_version: u64,
    model_revision: u32,
    user_revision: u32,
    state: PlanState,
    accepted_revision: Option<u32>,
    review_digest: Option<String>,
    #[serde(default)]
    review_evidence: Option<ReviewFingerprint>,
}

/// Bind comments, questions, answer order, and approval evidence without duplicating their bodies in command records.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
struct ReviewFingerprint([u8; 32]);

impl ReviewFingerprint {
    /// Capture deterministic normalized review evidence independently of the plan's content digest.
    fn capture(plan: &PlanRecord) -> Result<Option<Self>, TaskCommandFailure> {
        plan.review
            .as_ref()
            .map(|review| {
                let encoded =
                    serde_json::to_vec(review).map_err(|_| TaskCommandFailure::StateUnavailable)?;
                Ok(Self(Sha256::digest(encoded).into()))
            })
            .transpose()
    }
}

#[derive(Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
struct RawPlanRevisionSelection {
    source: PlanSelection,
    revision: u32,
}
