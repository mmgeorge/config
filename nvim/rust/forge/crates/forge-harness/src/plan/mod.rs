use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

use self::edit_trace::{PlanEditTrace, measure};
use crate::session::{ApprovalMode, continuation::ContinuationBudget};

mod artifact_recovery;
mod audit;
mod comment_lint;
pub(crate) mod conformance;
mod documentation;
pub(crate) mod implementation_report;
mod usage;
pub(crate) use design::{patch_single_file, workspace_source};
mod design;
mod design_artifact;
pub(crate) mod verification_artifact;
pub(crate) use design_artifact::{
    PlanArtifact, artifact_diagnostic, authored_text as design_artifact_text, changed_artifacts,
    preview_authored,
};
mod design_document;
mod design_flows;
mod design_tests;
pub use design_tests::DesignTestChange;
pub(crate) use design_tests::DesignTestSelection;
pub(crate) mod calls;
use crate::codec;
mod references;
mod workspace_definition;
pub use workspace_definition::{DefinitionLocation, WorkspaceDefinition};
mod reference_context;
pub use calls::{CallKind, CallPosition, CallSite, FunctionBehavior, FunctionBody};
mod design_review;
mod deviation;
mod document;
mod entry;
mod question_answer;
pub(crate) use question_answer::{
    PlanQuestionAnswerCommit, PlanQuestionAnswerError, PlanQuestionAnswerRequest, PlanQuestionOwner,
};
mod review_alignment;
mod review_delivery;
mod review_diff;
mod review_file_layout;
pub use design::{DeclarationDesign, DeclarationFile, DesignPatchRequest, ParsedDeclarations};
mod edit;
pub(crate) mod edit_trace;
pub(crate) mod event;
mod source_edit;
pub use event::{ExchangeAnchor, ExchangePlanEvent, PlanEventContent};
mod graph;
mod prompt;
mod render;
mod resolution;
mod review_annotation;
mod review_cache;
pub(crate) use review_cache::PlanReviewRevision;
mod review_feedback;
pub(crate) mod revision;
pub(crate) mod revision_review;
pub(crate) use review_annotation::resolve_annotations;
pub use review_annotation::{
    ReviewAnnotation, ReviewAnnotationAnchor, ReviewAnnotationKind, ReviewQuestionReply,
};
pub(crate) use review_feedback::render as render_review_feedback;
pub(crate) mod execution;
pub(crate) mod review_answer;
pub(crate) mod review_document;
mod review_projection;
pub(crate) mod review_source;
mod review_symbols;
mod scheduler;
pub use execution::{
    PlanExecutionLifecycleEvent, PlanExecutionLifecycleRecord, PlanExecutionRecord,
    PlanExecutionState, PlanPhase,
};
pub mod state_machine;
mod validation;

pub use audit::{
    PlanAudit, PlanAuditPathDifference, PlanAuditTask, build_plan_audit, render_plan_audit,
};
pub use deviation::{
    EffectivePlan, PlanDeviation, PlanDeviationDisposition, PlanDeviationKind,
    PlanDeviationRequest, build_effective_plan,
};
pub use document::*;
pub use edit::{
    PatchField, PlanEditRequest, PlanEditResult, PlanFieldPatch, PlanMutation, PlanMutationError,
    PlanResourceDelete, PlanResourceRename, PlanResourceSet, PlanSemanticRename, apply_plan_edit,
    apply_plan_mutation,
};
pub use graph::{PlanGraph, ResolvedPlanEntity};
pub use prompt::{PlanExecutionPromptKind, PlanPrompt};
pub use render::{
    PlanNavigationAnchor, PlanNavigationIndex, PlanReviewReferenceKind, PlanReviewTarget,
    PlanSection, RenderedPlan, render_plan, render_plan_at, render_plan_delta,
};
pub use resolution::{
    PlanResolutionEvidence, PlanResolutionKind, PlanResolutionRecord, PlanTaskSummary,
    PlanTestSummary, build_plan_resolution,
};
pub use scheduler::{
    PlanScheduler, PlanTaskExecution, PlanTaskOutcome, PlanTaskReport, PlanTaskState,
    PlanTestResult, PlanTestStatus,
};
pub use validation::{
    PlanValidationError, PlanValidationPhase, PlanViolation, validate_plan_edit,
    validate_plan_render, validate_plan_submission, validate_workspace_references,
};

/// Represents the review lifecycle of one model-authored plan.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanState {
    /// Retains the original request until project checks have been saved and validated.
    ConfiguringChecks,
    Generating,
    AwaitingInput,
    AwaitingReview,
    Revising,
    Accepted,
    Rejected,
    Cancelled,
    Failed,
}

/// Represents one durable plan and the exact digest under review.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanRecord {
    pub id: String,
    pub session_id: String,
    pub request: String,
    #[serde(default)]
    pub title: String,
    pub state: PlanState,
    pub working_path: String,
    #[serde(default)]
    pub document_version: u64,
    pub model_revision: u32,
    #[serde(default)]
    pub submitted_version: Option<u64>,
    #[serde(default)]
    pub accepted_revision: Option<u32>,
    pub user_revision: u32,
    #[serde(default)]
    /// Retains ordered user edits across reviewed revisions and final decisions.
    pub manual_changes: Vec<String>,
    pub review_digest: Option<String>,
    pub accepted_digest: Option<String>,
    #[serde(default)]
    pub elicitation: Option<PlanElicitation>,
    /// Retain revision-scoped review evidence without creating a separate approval lifecycle.
    #[serde(default, alias = "acceptance")]
    pub review: Option<PlanReview>,
    #[serde(default)]
    pub question_ledger: PlanQuestionLedger,
    #[serde(default)]
    pub generation: PlanGeneration,
    #[serde(default)]
    pub validation_warning: Vec<PlanViolation>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

impl PlanRecord {
    /// Get retained user-review questions only while this plan is awaiting review.
    /// Answered questions remain available for reopening and an explicit execute/continue action.
    pub fn review_questions(&self) -> Option<&PlanElicitation> {
        if self.state != PlanState::AwaitingReview {
            return None;
        }
        self.review
            .as_ref()
            .map(|review| &review.elicitation)
            .filter(|questions| !questions.question_set.questions.is_empty())
    }
}

/// Defines one durable event in a reviewed plan lifecycle.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanLifecycleKind {
    QuestionAsked,
    QuestionAnswered,
    QuestionWithdrawn,
    Created,
    ChangesRequested,
    RevisionCreated,
    Accepted,
    Cancelled,
}

/// Represents one immutable plan lifecycle event in the session timeline.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanLifecycleRecord {
    #[serde(default)]
    pub title: String,
    #[serde(default)]
    pub anchor: Option<ExchangeAnchor>,
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub kind: PlanLifecycleKind,
    pub model_revision: u32,
    pub user_revision: u32,
    pub overall_comment: Option<String>,
    #[serde(default)]
    pub annotation: Vec<PlanAnnotation>,
    #[serde(default)]
    pub question: Option<PlanQuestionSet>,
    #[serde(default)]
    pub answer: Option<String>,
    pub created_at_ms: i64,
}

/// Represents one selectable answer for a planning question.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestionOption {
    #[serde(default)]
    /// Marks the provider-recommended default for automatic answering.
    pub recommended: bool,
    pub label: String,
    pub description: String,
}

/// Represents one structured decision requested while creating a plan.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestion {
    #[serde(default)]
    pub id: String,
    pub header: String,
    pub question: String,
    #[serde(default)]
    pub options: Vec<PlanQuestionOption>,
    #[serde(default = "default_allow_freeform")]
    pub allow_freeform: bool,
}

/// Represents one atomic set of planning decisions presented to the user.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestionSet {
    #[serde(default)]
    pub id: String,
    pub questions: Vec<PlanQuestion>,
}

/// Defines one committed response to a planning question.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PlanQuestionResponse {
    Selected {
        option: String,
        feedback: Option<String>,
    },
    Other {
        text: String,
    },
    Skipped,
}

/// Defines how one planning question left the pending decision set.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanQuestionResolutionKind {
    Answered,
    Skipped,
    Withdrawn,
}

/// Stores one immutable planning decision so resolved questions cannot become pending again.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestionResolution {
    pub question_id: String,
    pub content_digest: String,
    pub kind: PlanQuestionResolutionKind,
    pub response: Option<PlanQuestionResponse>,
    pub resolved_at_ms: i64,
}

/// Owns durable planning decisions independently from transient elicitation presentation.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestionLedger {
    #[serde(default)]
    pub resolution: Vec<PlanQuestionResolution>,
}

impl PlanQuestionLedger {
    /// Record one terminal decision without allowing its identity to resolve twice.
    pub fn resolve(
        &mut self,
        question: &PlanQuestion,
        response: Option<PlanQuestionResponse>,
        resolved_at_ms: i64,
    ) {
        let content_digest = question.content_digest();
        if self
            .resolution
            .iter()
            .any(|item| item.question_id == question.id || item.content_digest == content_digest)
        {
            return;
        }
        let kind = match response.as_ref() {
            Some(PlanQuestionResponse::Skipped) => PlanQuestionResolutionKind::Skipped,
            Some(_) => PlanQuestionResolutionKind::Answered,
            None => PlanQuestionResolutionKind::Withdrawn,
        };
        self.resolution.push(PlanQuestionResolution {
            question_id: question.id.clone(),
            content_digest,
            kind,
            response,
            resolved_at_ms,
        });
    }

    /// Remove questions whose logical identifier or canonical content already resolved.
    pub fn unresolved(&self, mut question_set: PlanQuestionSet) -> Option<PlanQuestionSet> {
        question_set.questions.retain(|question| {
            let content_digest = question.content_digest();
            !self.resolution.iter().any(|item| {
                item.question_id == question.id || item.content_digest == content_digest
            })
        });
        (!question_set.questions.is_empty()).then_some(question_set)
    }
}

/// Tracks broker-owned planning retries and canonical document progress.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanGeneration {
    #[serde(flatten)]
    pub budget: ContinuationBudget,
    pub canonical_revision: u32,
}

impl PlanGeneration {
    /// Count one completed response and allow continuation while its progress budget remains.
    pub fn observe(&mut self, canonical_progress: bool) -> bool {
        self.budget.observe(canonical_progress)
    }

    /// Reset the bounded continuation budget while retaining plan and question state.
    pub fn reset(&mut self) {
        self.budget.reset();
    }

    /// Start a new progress interval after the user resolves pending input.
    pub fn reset_no_progress(&mut self) {
        self.budget.reset_no_progress();
    }
}

/// Associates one durable response with its planning question.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestionAnswer {
    #[serde(default)]
    /// Records whether the user or an elapsed deadline supplied this response.
    pub source: crate::session::ResolutionSource,
    pub question_id: String,
    pub response: PlanQuestionResponse,
}

/// Represents one model-reported reason that no pending user decision remains.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanQuestionWithdrawal {
    pub reason: String,
}

/// Tracks an unresolved planning decision set across answers and clarification turns.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanElicitation {
    #[serde(default, skip_serializing_if = "ElicitationPurpose::is_workflow")]
    purpose: ElicitationPurpose,
    #[serde(default)]
    /// Retains the original deadline across navigation and host recovery.
    pub deadline: Option<crate::session::InputDeadline>,
    #[serde(default)]
    /// Records automatic submission even when every answer was already confirmed.
    pub resolution_source: crate::session::ResolutionSource,
    pub question_set: PlanQuestionSet,
    pub revision: u32,
    #[serde(default)]
    pub answer_revision: u64,
    #[serde(default)]
    pub answer_order: std::collections::BTreeMap<String, (String, u64)>,
    #[serde(default)]
    pub answer: Vec<PlanQuestionAnswer>,
    #[serde(default)]
    pub current_index: usize,
    #[serde(default)]
    pub clarification_active: bool,
}

/// Retain revision-scoped reviewer evidence and questions; presence is not a lifecycle state.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(from = "StoredPlanReview")]
pub struct PlanReview {
    pub review_digest: String,
    /// Identify the approval question without interpreting opaque set or question identities.
    approval_question_id: String,
    /// Inline feedback captured with the approved source snapshot.
    #[serde(default)]
    pub annotations: Vec<PlanAnnotationInput>,
    #[serde(default)]
    /// Retains the reviewer's optional acceptance note without changing the reviewed design.
    pub overall_comment: Option<String>,
    #[serde(default)]
    pub saved_source_digest: Option<String>,
    pub approval_mode_list: Vec<ApprovalMode>,
    /// Preserve execution configuration selected before review questions are answered.
    #[serde(default)]
    pub(crate) execution_request: Option<crate::task::command::ExecuteSelection>,
    pub elicitation: PlanElicitation,
}

impl PlanReview {
    /// Create review questions from the normalized execution approval modes.
    pub fn new(review_digest: String, approval_mode_list: &[ApprovalMode]) -> Result<Self> {
        anyhow::ensure!(
            !approval_mode_list.is_empty(),
            "the active provider exposes no execution mode"
        );
        let mut question_list = Vec::new();
        let identity = uuid::Uuid::new_v4();
        let approval_question_id = format!("review-execution-mode:{identity}");
        if !approval_mode_list.is_empty() {
            question_list.push(PlanQuestion {
                id: approval_question_id.clone(),
                header: "Tool approval".into(),
                question: "How should execution handle tool approval?".into(),
                options: approval_mode_list
                    .iter()
                    .copied()
                    .map(|mode| PlanQuestionOption {
                        recommended: false,
                        label: approval_mode_option_label(mode).into(),
                        description: approval_mode_option_description(mode).into(),
                    })
                    .collect(),
                allow_freeform: false,
            });
        }
        let question_set = PlanQuestionSet {
            id: format!("plan-review:{identity}"),
            questions: question_list,
        }
        .normalize_for(ElicitationPurpose::PlanReview)?;
        let mut elicitation = PlanElicitation::new(question_set);
        elicitation.require_review_confirmation();
        Ok(Self {
            review_digest,
            approval_question_id,
            annotations: Vec::new(),
            overall_comment: None,
            saved_source_digest: None,
            approval_mode_list: approval_mode_list.to_vec(),
            execution_request: None,
            elicitation,
        })
    }

    /// Resolve execution approval from the committed answers to this review question set.
    pub fn approval_mode(&self) -> Result<ApprovalMode> {
        if self.approval_mode_list.len() == 1 {
            return Ok(self.approval_mode_list[0]);
        }
        match selected_option(&self.elicitation, &self.approval_question_id)? {
            "ask" => Ok(ApprovalMode::Ask),
            "deny" => Ok(ApprovalMode::Deny),
            "allow" => Ok(ApprovalMode::Allow),
            option => anyhow::bail!("unsupported execution access choice {option:?}"),
        }
    }
}

/// Distinguish time-bounded workflow questions from review decisions requiring explicit confirmation.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum ElicitationPurpose {
    #[default]
    Workflow,
    PlanReview,
}

impl ElicitationPurpose {
    /// Preserve the established wire shape for ordinary workflow elicitation.
    fn is_workflow(&self) -> bool {
        *self == Self::Workflow
    }
}

/// Decode legacy review evidence while preserving its question identities and committed answers.
#[derive(Deserialize)]
struct StoredPlanReview {
    review_digest: String,
    #[serde(default = "legacy_approval_question_id")]
    approval_question_id: String,
    #[serde(default)]
    annotations: Vec<PlanAnnotationInput>,
    #[serde(default)]
    overall_comment: Option<String>,
    #[serde(default)]
    saved_source_digest: Option<String>,
    approval_mode_list: Vec<ApprovalMode>,
    #[serde(default)]
    execution_request: Option<crate::task::command::ExecuteSelection>,
    elicitation: PlanElicitation,
}

impl From<StoredPlanReview> for PlanReview {
    /// Normalize legacy approval data into review evidence without granting execution authority.
    fn from(stored: StoredPlanReview) -> Self {
        let StoredPlanReview {
            review_digest,
            approval_question_id,
            annotations,
            overall_comment,
            saved_source_digest,
            approval_mode_list,
            execution_request,
            mut elicitation,
        } = stored;
        elicitation.require_review_confirmation();
        Self {
            review_digest,
            approval_question_id,
            annotations,
            overall_comment,
            saved_source_digest,
            approval_mode_list,
            execution_request,
            elicitation,
        }
    }
}

/// Preserve the approval question identity used by existing stored review data.
fn legacy_approval_question_id() -> String {
    "acceptance-execution-mode".into()
}

fn selected_option<'a>(elicitation: &'a PlanElicitation, question_id: &str) -> Result<&'a str> {
    let answer = elicitation
        .answer
        .iter()
        .find(|answer| answer.question_id == question_id)
        .with_context(|| format!("review question {question_id:?} has no answer"))?;
    anyhow::ensure!(
        answer.source == crate::session::ResolutionSource::User,
        "review approval requires an explicit user decision"
    );
    match &answer.response {
        PlanQuestionResponse::Selected { option, .. } => Ok(option),
        PlanQuestionResponse::Other { .. } | PlanQuestionResponse::Skipped => {
            anyhow::bail!("review question {question_id:?} requires a selected option")
        }
    }
}

const fn approval_mode_option_label(mode: ApprovalMode) -> &'static str {
    match mode {
        ApprovalMode::Ask => "ask",
        ApprovalMode::Deny => "deny",
        ApprovalMode::Allow => "allow",
    }
}

const fn approval_mode_option_description(mode: ApprovalMode) -> &'static str {
    match mode {
        ApprovalMode::Ask => "Apply saved rules and ask for remaining requests.",
        ApprovalMode::Deny => "Reject requests that require approval.",
        ApprovalMode::Allow => "Approve remaining requests once, preserving explicit denies.",
    }
}

impl PlanElicitation {
    /// Require explicit review confirmation independently of legacy question identifiers.
    pub(crate) fn require_review_confirmation(&mut self) {
        self.purpose = ElicitationPurpose::PlanReview;
        self.deadline = None;
    }

    /// Report unresolved decisions from their identities rather than the presentation cursor.
    pub fn has_pending_decisions(&self) -> bool {
        self.question_set.questions.iter().any(|question| {
            !self
                .answer
                .iter()
                .any(|answer| answer.question_id == question.id)
        })
    }

    /// Capture a workflow deadline while preserving explicit review confirmation.
    pub fn start_timeout(
        &mut self,
        timeout: crate::session::InputTimeout,
        now_ms: i64,
    ) -> Result<()> {
        if self.purpose == ElicitationPurpose::Workflow {
            self.deadline = timeout.deadline(now_ms)?;
        }
        Ok(())
    }

    /// Preserve confirmed responses and fill only unanswered questions before automatic submission.
    pub fn expire(&mut self) -> Result<()> {
        anyhow::ensure!(
            self.purpose != ElicitationPurpose::PlanReview,
            "plan review requires explicit confirmation"
        );
        for question in self.question_set.questions.clone() {
            if self
                .answer
                .iter()
                .any(|answer| answer.question_id == question.id)
            {
                continue;
            }
            let selected = question
                .options
                .iter()
                .find(|option| option.recommended)
                .or_else(|| question.options.first());
            let response = selected.map_or(PlanQuestionResponse::Skipped, |option| {
                PlanQuestionResponse::Selected {
                    option: option.label.clone(),
                    feedback: None,
                }
            });
            self.record_answer(
                &question.id,
                response,
                crate::session::ResolutionSource::Timeout,
            )?;
        }
        self.resolution_source = crate::session::ResolutionSource::Timeout;
        self.deadline = None;
        Ok(())
    }

    /// Build unresolved elicitation state from a normalized provider question set.
    pub fn new(question_set: PlanQuestionSet) -> Self {
        Self {
            purpose: ElicitationPurpose::Workflow,
            question_set,
            deadline: None,
            resolution_source: crate::session::ResolutionSource::User,
            revision: 1,
            answer_revision: 0,
            answer_order: Default::default(),
            answer: Vec::new(),
            current_index: 0,
            clarification_active: false,
        }
    }

    /// Replace provider questions while preserving responses that remain structurally valid.
    pub fn replace_question_set(&mut self, question_set: PlanQuestionSet) {
        self.answer.retain(|answer| {
            question_set
                .questions
                .iter()
                .find(|question| question.id == answer.question_id)
                .is_some_and(|question| validate_response(question, &answer.response).is_ok())
        });
        self.question_set = question_set;
        self.revision = self.revision.saturating_add(1);
        self.current_index = self
            .question_set
            .questions
            .iter()
            .position(|question| {
                !self
                    .answer
                    .iter()
                    .any(|answer| answer.question_id == question.id)
            })
            .unwrap_or(self.question_set.questions.len());
        self.clarification_active = false;
    }

    /// Resolve the question currently presented by the review UI.
    pub fn current_question(&self) -> Option<&PlanQuestion> {
        self.question_set.questions.get(self.current_index)
    }

    /// Resolve a question by its durable identifier for non-linear review navigation.
    pub fn question(&self, question_id: &str) -> Option<&PlanQuestion> {
        self.question_set
            .questions
            .iter()
            .find(|question| question.id == question_id)
    }

    /// Reopen a reviewed decision without consuming its prior selection as user feedback.
    pub fn begin_clarification(&mut self, question_id: &str) -> Result<()> {
        let question_index = if question_id == self.question_set.id {
            self.current_index
                .min(self.question_set.questions.len().saturating_sub(1))
        } else {
            self.question_set
                .questions
                .iter()
                .position(|question| question.id == question_id)
                .context("clarification question not found")?
        };
        let question = self
            .question_set
            .questions
            .get(question_index)
            .context("clarification requires a question")?;
        self.answer
            .retain(|answer| answer.question_id != question.id);
        self.current_index = question_index;
        self.revision = self.revision.saturating_add(1);
        self.clarification_active = true;
        Ok(())
    }

    /// Commit one response and advance presentation to the next question.
    pub fn answer(&mut self, question_id: &str, response: PlanQuestionResponse) -> Result<()> {
        self.record_answer(
            question_id,
            response,
            crate::session::ResolutionSource::User,
        )
    }

    fn record_answer(
        &mut self,
        question_id: &str,
        response: PlanQuestionResponse,
        source: crate::session::ResolutionSource,
    ) -> Result<()> {
        let question_index = self
            .question_set
            .questions
            .iter()
            .position(|question| question.id == question_id)
            .context("planning question not found")?;
        validate_response(&self.question_set.questions[question_index], &response)?;
        self.answer
            .retain(|answer| answer.question_id != question_id);
        self.answer.push(PlanQuestionAnswer {
            source,
            question_id: question_id.to_owned(),
            response,
        });
        self.answer_revision += 1;
        self.current_index = self
            .question_set
            .questions
            .iter()
            .position(|question| {
                !self
                    .answer
                    .iter()
                    .any(|answer| answer.question_id == question.id)
            })
            .unwrap_or(self.question_set.questions.len());
        self.clarification_active = false;
        Ok(())
    }

    /// Commit an explicit conversational answer and reopen presentation at the next decision.
    pub fn answer_from_model(
        &mut self,
        question_id: &str,
        response: PlanQuestionResponse,
    ) -> Result<()> {
        self.answer(question_id, response)?;
        self.revision = self.revision.saturating_add(1);
        Ok(())
    }

    /// Serialize every decision for the planning continuation contract.
    pub fn feedback(&self) -> String {
        let mut line_list = vec!["Planning feedback:".to_owned()];
        for question in &self.question_set.questions {
            let answer = self
                .answer
                .iter()
                .find(|answer| answer.question_id == question.id);
            let value = match answer.map(|answer| &answer.response) {
                Some(PlanQuestionResponse::Selected { option, feedback }) => feedback
                    .as_ref()
                    .filter(|feedback| !feedback.trim().is_empty())
                    .map(|feedback| format!("{option} — {feedback}"))
                    .unwrap_or_else(|| option.clone()),
                Some(PlanQuestionResponse::Other { text }) => text.clone(),
                Some(PlanQuestionResponse::Skipped) | None => {
                    "[intentionally unanswered; continue with best judgment]".into()
                }
            };
            let provenance = if answer
                .is_some_and(|answer| answer.source == crate::session::ResolutionSource::Timeout)
            {
                " [automatic timeout]"
            } else {
                ""
            };
            line_list.push(format!("- {}{provenance}: {value}", question.header));
        }
        if self.resolution_source == crate::session::ResolutionSource::Timeout {
            line_list.insert(0, "The questions timed out, and the user may be unavailable. Harness kept the answers the user confirmed, selected defaults for the rest, and skipped questions without options. Automatic selections are not user answers. Continue cautiously with the available context and existing permissions.".into());
        }
        line_list.join("\n")
    }
}

fn validate_response(question: &PlanQuestion, response: &PlanQuestionResponse) -> Result<()> {
    match response {
        PlanQuestionResponse::Selected { option, .. } => anyhow::ensure!(
            question
                .options
                .iter()
                .any(|choice| choice.label == *option),
            "selected planning option does not exist"
        ),
        PlanQuestionResponse::Other { text } => {
            anyhow::ensure!(
                question.allow_freeform,
                "planning question forbids free-form answers"
            );
            anyhow::ensure!(
                !text.trim().is_empty(),
                "free-form planning answer cannot be empty"
            );
        }
        PlanQuestionResponse::Skipped => {}
    }
    Ok(())
}

impl PlanQuestionSet {
    /// Build a free-form fallback from an ordinary assistant question.
    pub fn freeform(question: String) -> Self {
        Self {
            id: String::new(),
            questions: vec![PlanQuestion {
                id: String::new(),
                header: "Planning feedback".into(),
                question,
                options: Vec::new(),
                allow_freeform: true,
            }],
        }
    }

    /// Assign durable identifiers and validate the question set before persistence.
    pub fn normalize(self) -> Result<Self> {
        self.normalize_for(ElicitationPurpose::Workflow)
    }

    /// Validate option limits from the question's domain purpose instead of an opaque identifier.
    fn normalize_for(mut self, purpose: ElicitationPurpose) -> Result<Self> {
        anyhow::ensure!(
            !self.questions.is_empty() && self.questions.len() <= 3,
            "planning feedback must contain between one and three questions"
        );
        if self.id.is_empty() {
            self.id = self.content_digest();
        }
        for (index, question) in self.questions.iter_mut().enumerate() {
            anyhow::ensure!(
                !question.question.trim().is_empty(),
                "planning question text cannot be empty"
            );
            let option_count = match purpose {
                ElicitationPurpose::Workflow => 2..=3,
                ElicitationPurpose::PlanReview => 1..=4,
            };
            anyhow::ensure!(
                question.options.is_empty() || option_count.contains(&question.options.len()),
                "workflow questions require two or three choices; review questions require one to four"
            );
            for option in &mut question.options {
                option.recommended |= option.label.to_ascii_lowercase().contains("(recommended)");
                anyhow::ensure!(
                    !option.label.trim().is_empty() && !option.description.trim().is_empty(),
                    "planning question choices require labels and descriptions"
                );
            }
            if question.id.is_empty() {
                question.id = question.content_digest();
            }
            if question.header.trim().is_empty() {
                question.header = format!("Question {}", index + 1);
            }
        }
        Ok(self)
    }
}

impl PlanQuestion {
    /// Build a stable identity from the user-visible decision content.
    pub fn content_digest(&self) -> String {
        let mut digest = Sha256::new();
        digest.update(self.question.trim().as_bytes());
        digest.update([0]);
        digest.update([u8::from(self.allow_freeform)]);
        for option in &self.options {
            digest.update([0]);
            digest.update(option.label.trim().as_bytes());
            digest.update([0]);
            digest.update(option.description.trim().as_bytes());
        }
        format!("{:x}", digest.finalize())
    }
}

impl PlanQuestionSet {
    /// Build a stable set identity from ordered question content.
    fn content_digest(&self) -> String {
        let mut digest = Sha256::new();
        for question in &self.questions {
            digest.update(question.content_digest().as_bytes());
            digest.update([0]);
        }
        format!("{:x}", digest.finalize())
    }
}

fn default_allow_freeform() -> bool {
    true
}

/// Describes one plan artifact for the Harness picker and winbar.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ArtifactSummary {
    pub id: String,
    pub title: String,
    pub state: PlanState,
    pub working_path: String,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

impl From<&PlanRecord> for ArtifactSummary {
    fn from(plan: &PlanRecord) -> Self {
        Self {
            id: plan.id.clone(),
            title: plan.title.clone(),
            state: plan.state,
            working_path: plan.working_path.clone(),
            created_at_ms: plan.created_at_ms,
            updated_at_ms: plan.updated_at_ms,
        }
    }
}

/// Represents one raw review comment before Rust resolves its rendered range.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanAnnotationInput {
    pub start_line: u32,
    pub end_line: u32,
    pub body: String,
}

/// Represents one canonical plan subject covered by a review comment.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAnnotationSubject {
    pub target: PlanReviewTarget,
    pub json_path: String,
    pub label: String,
    pub path: Option<String>,
}

/// Represents one review comment anchored to an ordered canonical subject range.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAnnotation {
    pub subject: Vec<PlanAnnotationSubject>,
    pub label: String,
    pub body: String,
}

/// Selects enabled checks and removed test declarations in one plan edit.
pub(crate) struct PlanCheckSelection<'selection> {
    pub enabled: &'selection std::collections::BTreeMap<String, bool>,
    pub removed_tests: &'selection [DesignTestSelection],
}

/// The working plan changed after it was captured for an update.
pub(crate) struct WorkingDocumentChanged;

impl std::fmt::Display for WorkingDocumentChanged {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("plan document changed before write")
    }
}

impl std::fmt::Debug for WorkingDocumentChanged {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("WorkingDocumentChanged")
    }
}

impl std::error::Error for WorkingDocumentChanged {}

/// Owns physical plan files and immutable revision history.
pub struct PlanFileStore {
    review_cache: review_cache::ReviewCache,
    implementation_report_cache: std::sync::Mutex<
        std::collections::HashMap<
            String,
            std::sync::Arc<implementation_report::ImplementationReport>,
        >,
    >,
    _lease: crate::storage::state::StateLease,
    root: PathBuf,
    workspace: PathBuf,
}

/// Retain a validated and rendered plan until its captured baseline can be committed.
pub(crate) struct PreparedPlanSubmission {
    session_id: String,
    plan_id: String,
    revision: u32,
    expected: PlanDocument,
    document: PlanDocument,
    rendered: RenderedPlan,
}

/// Bind a dependent commit to the captured contents of one canonical working document.
pub(crate) struct WorkingDocumentGuard<'store> {
    files: &'store PlanFileStore,
    expected: &'store PlanDocument,
}

/// Permit artifact commits under an already validated working-document lock.
pub(crate) struct WorkingDocumentCommit<'store> {
    files: &'store PlanFileStore,
    session_id: &'store str,
    plan_id: &'store str,
}

impl WorkingDocumentCommit<'_> {
    pub(crate) fn matches(&self, plan: &PlanRecord) -> bool {
        plan.session_id == self.session_id && plan.id == self.plan_id
    }

    pub(crate) fn claim_artifacts(&self, plan: &PlanRecord) -> Result<bool> {
        anyhow::ensure!(
            self.matches(plan),
            "artifact commit changed its held document identity"
        );
        self.files.claim_plan_artifacts_locked(plan)
    }

    pub(crate) fn reconcile(&self, plan: &PlanRecord) -> Result<()> {
        anyhow::ensure!(
            self.matches(plan),
            "artifact commit changed its held document identity"
        );
        self.files.reconcile_artifact_locked(plan)
    }
}

impl WorkingDocumentGuard<'_> {
    /// Validate the complete document and exclude file writers until the dependent commit finishes.
    ///
    /// Task-dependent callers acquire their task mutation gate before entering this method.
    /// Artifact writes use the supplied permit without reentering the held file gate.
    pub(crate) fn commit<Output>(
        self,
        session_id: &str,
        commit: impl FnOnce(&WorkingDocumentCommit<'_>) -> Result<Output>,
    ) -> Result<Output> {
        let gate = self
            .files
            .working_document_gate(session_id, &self.expected.plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        if self
            .files
            .read_working_document(session_id, &self.expected.plan_id)?
            != *self.expected
        {
            return Err(WorkingDocumentChanged.into());
        }
        commit(&WorkingDocumentCommit {
            files: self.files,
            session_id,
            plan_id: &self.expected.plan_id,
        })
    }
}

impl PlanFileStore {
    /// Bind a later dependent commit to this store and its captured document.
    pub(crate) fn guard_working_document<'store>(
        &'store self,
        expected: &'store PlanDocument,
    ) -> WorkingDocumentGuard<'store> {
        WorkingDocumentGuard {
            files: self,
            expected,
        }
    }

    /// Share one short mutation gate for every store instance targeting this plan directory.
    fn working_document_gate(
        &self,
        session_id: &str,
        plan_id: &str,
    ) -> Result<std::sync::Arc<std::sync::Mutex<()>>> {
        static GATES: std::sync::OnceLock<
            std::sync::Mutex<
                std::collections::HashMap<PathBuf, std::sync::Weak<std::sync::Mutex<()>>>,
            >,
        > = std::sync::OnceLock::new();
        let directory = self.plan_dir(session_id, plan_id);
        fs::create_dir_all(&directory)?;
        let key = fs::canonicalize(&directory)?;
        let mut gates = GATES
            .get_or_init(Default::default)
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate registry poisoned"))?;
        if let Some(gate) = gates.get(&key).and_then(std::sync::Weak::upgrade) {
            return Ok(gate);
        }
        gates.retain(|_, gate| gate.strong_count() != 0);
        let gate = std::sync::Arc::new(std::sync::Mutex::new(()));
        gates.insert(key, std::sync::Arc::downgrade(&gate));
        Ok(gate)
    }

    /// Create a plan store that retains the current disposable-state version.
    pub fn new(root: impl Into<PathBuf>, workspace: impl Into<PathBuf>) -> Result<Self> {
        let root = root.into();
        let lease = crate::storage::state::StateLease::open(&root)?;
        Ok(Self {
            _lease: lease,
            root,
            workspace: workspace.into(),
            implementation_report_cache: Default::default(),
            review_cache: Default::default(),
        })
    }

    /// Share immutable report contents without copying them into durable exchanges.
    pub(crate) fn implementation_report(
        &self,
        reference: &implementation_report::ImplementationReportRef,
    ) -> Result<std::sync::Arc<implementation_report::ImplementationReport>> {
        let mut cache = self
            .implementation_report_cache
            .lock()
            .map_err(|_| anyhow::anyhow!("report cache lock poisoned"))?;
        if let Some(report) = cache.get(&reference.object_id) {
            return Ok(report.clone());
        }
        let objects = crate::storage::objects::ObjectStore::open(&self.root)?;
        let report = std::sync::Arc::new(implementation_report::ImplementationReport::load(
            &objects, reference,
        )?);
        cache.insert(reference.object_id.clone(), report.clone());
        Ok(report)
    }

    /// Preserve incomplete construction until submission validates the whole plan.
    #[cfg(test)]
    pub fn write_working_document(
        &self,
        session_id: &str,
        plan_id: &str,
        document: &PlanDocument,
    ) -> Result<PathBuf> {
        anyhow::ensure!(
            document.plan_id == plan_id,
            "working document plan id mismatch"
        );
        self.review_cache.validate(document, false)?;
        let gate = self.working_document_gate(session_id, plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        self.write_working_document_locked(session_id, plan_id, document)
    }

    /// Create a new working document without replacing an existing plan.
    pub fn create_working_document(
        &self,
        session_id: &str,
        plan_id: &str,
        document: &PlanDocument,
    ) -> Result<PathBuf> {
        anyhow::ensure!(
            document.plan_id == plan_id,
            "working document plan id mismatch"
        );
        self.review_cache.validate(document, false)?;
        let gate = self.working_document_gate(session_id, plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        anyhow::ensure!(
            !self
                .plan_dir(session_id, plan_id)
                .join("working.json")
                .exists(),
            "working plan already exists"
        );
        self.write_working_document_locked(session_id, plan_id, document)
    }

    /// Publish only if the canonical working document still equals the captured version.
    pub(crate) fn write_working_document_if_unchanged(
        &self,
        session_id: &str,
        plan_id: &str,
        expected: &PlanDocument,
        document: &PlanDocument,
    ) -> Result<PathBuf> {
        anyhow::ensure!(
            expected.plan_id == plan_id && document.plan_id == plan_id,
            "working document plan id mismatch"
        );
        self.review_cache.validate(document, false)?;
        let gate = self.working_document_gate(session_id, plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        let current = self.read_working_document(session_id, plan_id)?;
        if &current != expected {
            return Err(WorkingDocumentChanged.into());
        }
        self.write_working_document_locked(session_id, plan_id, document)
    }

    fn write_working_document_locked(
        &self,
        session_id: &str,
        plan_id: &str,
        document: &PlanDocument,
    ) -> Result<PathBuf> {
        self.require_reconciled_artifacts(session_id, plan_id)?;
        let directory = self.plan_dir(session_id, plan_id);
        fs::create_dir_all(directory.join("revisions"))
            .with_context(|| format!("create plan directory {}", directory.display()))?;
        write_json_atomically(&directory.join("working.json"), document)?;
        Ok(directory.join("working.md"))
    }

    /// Read and validate the canonical working document.
    pub fn read_working_document(&self, session_id: &str, plan_id: &str) -> Result<PlanDocument> {
        let path = self.plan_dir(session_id, plan_id).join("working.json");
        let content = fs::read_to_string(&path)
            .with_context(|| format!("read working plan document {}", path.display()))?;
        let document = codec::from_str::<PlanDocument>(&content)
            .with_context(|| format!("decode working plan document {}", path.display()))?;
        self.review_cache.validate(&document, false)?;
        Ok(document)
    }

    /// Apply one atomic semantic edit to the structurally valid working draft.
    pub fn edit_working_document(
        &self,
        session_id: &str,
        request: PlanEditRequest,
    ) -> Result<PlanEditResult> {
        let document = self.read_working_document(session_id, &request.plan_id)?;
        let result = apply_plan_edit(&document, request)?;
        self.write_working_document_if_unchanged(
            session_id,
            &result.plan_id,
            &document,
            &result.document,
        )?;
        Ok(result)
    }

    /// Rename one introduced definition and freeze its revision using only saved plan files.
    pub(crate) fn rename_symbol(
        &self,
        session_id: &str,
        plan_id: &str,
        expected_version: u64,
        identity: &str,
        name: &str,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(String, PlanDocument, RenderedPlan, String)> {
        let document = measure(trace, "rename.read_validate", || {
            self.read_working_document(session_id, plan_id)
        })?;
        anyhow::ensure!(
            document.version == expected_version,
            "plan version changed before rename"
        );
        self.rename_symbol_from_document(session_id, &document, identity, name, trace)
    }

    /// Rename from the exact canonical document captured by the owner command.
    pub(crate) fn rename_symbol_from_document(
        &self,
        session_id: &str,
        document: &PlanDocument,
        identity: &str,
        name: &str,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(String, PlanDocument, RenderedPlan, String)> {
        let plan_id = &document.plan_id;
        let index = measure(trace, "rename.reference_index", || {
            references::PlanReferenceIndex::planned(document, &self.workspace)
        })?;
        let (_, definition) = index.rename_definition(document, identity)?;
        let renamed = measure(trace, "rename.semantic_edit", || {
            index.renamed(document, identity, name)
        })?;
        let rendered = measure(trace, "rename.render_changed_plan", || {
            design_review::render(&renamed)
        })?;
        let (document, rendered, digest) = self.persist_review_draft_if_unchanged(
            session_id,
            document,
            PlanReviewRevision::new(plan_id, renamed, rendered)?,
            trace,
        )?;
        Ok((definition.name, document, rendered, digest))
    }

    /// Rename one newly added entity and persist the complete canonical document.
    pub fn rename_added_entity(
        &self,
        session_id: &str,
        plan_id: &str,
        entity_name: &str,
        new_name: String,
    ) -> Result<PlanEditResult> {
        let document = self.read_working_document(session_id, plan_id)?;
        self.rename_added_entity_from_document(session_id, &document, entity_name, new_name)
    }

    /// Rename an introduced entity while guarding the owner's captured canonical document.
    pub(crate) fn rename_added_entity_from_document(
        &self,
        session_id: &str,
        document: &PlanDocument,
        entity_name: &str,
        new_name: String,
    ) -> Result<PlanEditResult> {
        let plan_id = &document.plan_id;
        let result = edit::rename_added_entity(document, entity_name, new_name)?;
        let rendered = self.render_review(&result.document)?;
        self.persist_review_draft_if_unchanged(
            session_id,
            document,
            PlanReviewRevision::new(plan_id, result.document.clone(), rendered)?,
            None,
        )?;
        Ok(result)
    }

    /// Freeze one submitted JSON revision together with its exact rendered projection.
    pub fn submit_document_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let document = self.read_working_document(session_id, plan_id)?;
        self.submit_validated_document_revision_if_unchanged(
            session_id,
            plan_id,
            revision,
            expected_version,
            &document,
            document.clone(),
        )
    }

    /// Freeze one externally validated document without re-reading stale derived state.
    #[cfg(test)]
    pub fn submit_validated_document_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
        document: PlanDocument,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let current = self.read_working_document(session_id, plan_id)?;
        self.submit_validated_document_revision_if_unchanged(
            session_id,
            plan_id,
            revision,
            expected_version,
            &current,
            document,
        )
    }

    /// Commit a separately validated plan against the document that was validated.
    pub(crate) fn submit_validated_document_revision_if_unchanged(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
        current: &PlanDocument,
        document: PlanDocument,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let prepared = self.prepare_validated_submission(
            session_id,
            plan_id,
            revision,
            expected_version,
            current,
            document,
        )?;
        self.commit_prepared_submission(prepared)
    }

    /// Validate and render without holding either the task or working-document mutation gate.
    pub(crate) fn prepare_validated_submission(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
        current: &PlanDocument,
        mut document: PlanDocument,
    ) -> Result<PreparedPlanSubmission> {
        anyhow::ensure!(
            current.version == expected_version,
            "plan version changed before validated submission"
        );
        anyhow::ensure!(
            document.plan_id == plan_id && document.version == expected_version,
            "validated document identity changed before submission"
        );
        if let Some(design) = &document.design {
            design.validate()?;
            design.check_workspace(&self.workspace)?;
            crate::declaration::DeclarationValidationError::ensure_empty(
                design.authored_diagnostics(&self.workspace)?,
            )?;
        }
        document.validate_for_submission()?;
        if let Some(design) = &mut document.design {
            design.check_configuration =
                Some(crate::checks::ConfigSnapshot::load(&self.workspace)?);
            let fingerprint = crate::declaration::fingerprint(design)?;
            let report = match design.parsed_mut()?.validation.take() {
                Some(report) if report.fingerprint == fingerprint => report,
                _ => crate::declaration::DeclarationResolver::planned(
                    &self.workspace,
                    design,
                    false,
                )?
                .validate(design)?,
            };
            report.ensure_valid()?;
            design.parsed_mut()?.validation = Some(report);
        }
        let rendered = self.render_review(&document)?;
        Ok(PreparedPlanSubmission {
            session_id: session_id.into(),
            plan_id: plan_id.into(),
            revision,
            expected: current.clone(),
            document,
            rendered,
        })
    }

    /// Commit a prepared submission against the exact canonical baseline it validated.
    pub(crate) fn commit_prepared_submission(
        &self,
        prepared: PreparedPlanSubmission,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let PreparedPlanSubmission {
            session_id,
            plan_id,
            revision,
            expected,
            document,
            rendered,
        } = prepared;
        self.persist_rendered_revision(
            &session_id,
            &plan_id,
            revision,
            PlanReviewRevision::new(&plan_id, document, rendered)?,
            Some(&expected),
            None,
        )
    }

    /// Submit a validated execution derivative against its captured canonical baseline.
    pub(crate) fn submit_execution_revision_if_unchanged(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        current: &PlanDocument,
        document: PlanDocument,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        anyhow::ensure!(
            current.version == document.version && document.plan_id == plan_id,
            "execution revision changed before submission"
        );
        let design = document
            .design
            .as_ref()
            .context("execution revision requires semantic design")?;
        design.validate()?;
        crate::declaration::DeclarationValidationError::ensure_empty(
            design.authored_diagnostics(&self.workspace)?,
        )?;
        design
            .parsed()?
            .validation
            .as_ref()
            .context("execution revision has no validation evidence")?
            .ensure_valid()?;
        document.validate_for_submission()?;
        self.persist_revision(session_id, plan_id, revision, document, Some(current), None)
    }

    fn persist_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        document: PlanDocument,
        expected: Option<&PlanDocument>,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let rendered = measure(trace, "persist.render_plan", || {
            self.render_review(&document)
        })?;
        self.persist_rendered_revision(
            session_id,
            plan_id,
            revision,
            PlanReviewRevision::new(plan_id, document, rendered)?,
            expected,
            trace,
        )
    }

    /// Render a validated review document while reusing unchanged analysis.
    pub(crate) fn render_review(&self, document: &PlanDocument) -> Result<RenderedPlan> {
        self.review_cache.validate(document, true)?;
        if let Some(rendered) = self.review_cache.rendered(document)? {
            return Ok(rendered);
        }
        let rendered = if document.design.is_some() {
            design_review::render(document)?
        } else {
            render_plan_at(document, &self.workspace)?
        };
        self.review_cache.remember(&PlanReviewRevision::new(
            &document.plan_id,
            document.clone(),
            rendered.clone(),
        )?)?;
        Ok(rendered)
    }

    fn persist_rendered_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        review: PlanReviewRevision,
        expected: Option<&PlanDocument>,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let gate = self.working_document_gate(session_id, plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        if let Some(expected) = expected
            && self.read_working_document(session_id, plan_id)? != *expected
        {
            return Err(WorkingDocumentChanged.into());
        }
        if crate::storage::state::database(&self.root).exists()
            && let Some(prior) =
                crate::storage::SqliteStore::read_only(&self.root)?.load_plan(plan_id)?
        {
            self.require_reconciled_artifacts(session_id, plan_id)?;
            self.journal_revision_locked(
                &prior,
                Some(revision),
                &review.document,
                &review.rendered,
            )?;
            return Ok(review.into_parts());
        }
        let (document, rendered, checksum) =
            self.persist_review_draft_locked(session_id, review, trace)?;
        let revision_directory = self.plan_dir(session_id, plan_id).join("revisions");
        measure(trace, "persist.revision_directory", || {
            Ok(fs::create_dir_all(&revision_directory)?)
        })?;
        let stem = format!("submitted-{revision:04}");
        measure(trace, "persist.submitted_json", || {
            write_json_atomically(&revision_directory.join(format!("{stem}.json")), &document)
        })?;
        measure(trace, "persist.submitted_markdown", || {
            write_text_atomically(
                &revision_directory.join(format!("{stem}.md")),
                &rendered.markdown,
            )
        })?;
        measure(trace, "persist.submitted_navigation", || {
            write_json_atomically(
                &revision_directory.join(format!("{stem}.index.json")),
                &rendered.navigation,
            )
        })?;
        Ok((document, rendered, checksum))
    }

    /// Commit a prepared review only if its original canonical document is still current.
    pub(crate) fn persist_review_draft_if_unchanged(
        &self,
        session_id: &str,
        expected: &PlanDocument,
        revision: PlanReviewRevision,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        anyhow::ensure!(
            expected.plan_id == revision.document.plan_id,
            "review plan identity changed"
        );
        let gate = self.working_document_gate(session_id, &revision.document.plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        if self.read_working_document(session_id, &expected.plan_id)? != *expected {
            return Err(WorkingDocumentChanged.into());
        }
        self.persist_review_draft_locked(session_id, revision, trace)
    }

    fn persist_review_draft_locked(
        &self,
        session_id: &str,
        revision: PlanReviewRevision,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        self.require_reconciled_artifacts(session_id, &revision.document.plan_id)?;
        if crate::storage::state::database(&self.root).exists()
            && let Some(prior) = crate::storage::SqliteStore::read_only(&self.root)?
                .load_plan(&revision.document.plan_id)?
        {
            measure(trace, "persist.journal_revision", || {
                self.journal_revision_locked(&prior, None, &revision.document, &revision.rendered)
            })?;
            return Ok(revision.into_parts());
        }
        let plan_directory = self.plan_dir(session_id, &revision.document.plan_id);
        let document = &revision.document;
        let rendered = &revision.rendered;
        measure(trace, "persist.working_directory", || {
            Ok(fs::create_dir_all(&plan_directory)?)
        })?;
        measure(trace, "persist.working_json", || {
            write_json_atomically(&plan_directory.join("working.json"), &document)
        })?;
        measure(trace, "persist.working_markdown", || {
            write_text_atomically(&plan_directory.join("working.md"), &rendered.markdown)
        })?;
        measure(trace, "persist.working_navigation", || {
            write_json_atomically(
                &plan_directory.join("working.index.json"),
                &rendered.navigation,
            )
        })?;
        self.review_cache.remember(&revision)?;
        Ok(revision.into_parts())
    }

    /// Publish a reviewed test-inventory edit without changing declarations or project files.
    pub(crate) fn remove_tests(
        &self,
        session_id: &str,
        plan_id: &str,
        expected_version: u64,
        expected_digest: &str,
        selected: &[DesignTestSelection],
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let mut document = measure(trace, "delete.read_validate", || {
            self.read_working_document(session_id, plan_id)
        })?;
        anyhow::ensure!(
            document.version == expected_version,
            "plan version changed before test deletion"
        );
        anyhow::ensure!(
            digest(&crate::plan::codec::to_vec(&document)?) == expected_digest,
            "plan changed before test deletion"
        );
        let original = document.clone();
        let before = measure(trace, "delete.render_previous", || {
            self.render_review(&document)
        })?;
        let path = self.plan_dir(session_id, plan_id).join("working.json");
        let annotation = measure(trace, "delete.read_annotations", || {
            Ok(review_annotation::ReviewAnnotationStore::open(
                path.clone(),
                digest(&fs::read(&path)?),
            )?
            .annotation()
            .to_vec())
        })?;
        measure(trace, "delete.semantic_edit", || {
            let design = document
                .design
                .as_mut()
                .context("test deletion requires a declaration design")?;
            design_tests::remove(&mut design.document.tests, selected)?;
            design_artifact::revert_authored(PlanArtifact::Tests, design);
            Ok(())
        })?;
        document.version = document
            .version
            .checked_add(1)
            .context("plan version exhausted")?;
        let rendered = measure(trace, "delete.update_test_rows", || {
            design_review::replace_test_inventory(&before, &document)
        })?;
        self.persist_annotated_edit(
            session_id,
            &original,
            PlanReviewRevision::new(plan_id, document, rendered)?,
            &before,
            annotation,
            trace,
        )
    }

    /// Updates check selection without changing repository defaults or discarding review annotations.
    pub(crate) async fn update_checks(
        &self,
        session_id: &str,
        plan_id: &str,
        expected_version: u64,
        expected_digest: &str,
        selection: PlanCheckSelection<'_>,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let PlanCheckSelection {
            enabled: changes,
            removed_tests: tests,
        } = selection;
        let mut document = self.read_working_document(session_id, plan_id)?;
        anyhow::ensure!(
            document.version == expected_version,
            "plan version changed before check selection"
        );
        anyhow::ensure!(
            digest(&crate::plan::codec::to_vec(&document)?) == expected_digest,
            "plan changed before check selection"
        );
        let original = document.clone();
        let before = self.render_review(&document)?;
        let path = self.plan_dir(session_id, plan_id).join("working.json");
        let annotation =
            review_annotation::ReviewAnnotationStore::open(path.clone(), digest(&fs::read(path)?))?
                .annotation()
                .to_vec();
        let design = document
            .design
            .as_mut()
            .context("check selection requires a declaration design")?;
        let config = &design
            .check_configuration
            .as_ref()
            .context("plan has no configured checks")?
            .config;
        design.document.set_checks(config, changes)?;
        design_artifact::revert_authored(PlanArtifact::Checks, design);
        if !tests.is_empty() {
            design_tests::remove(&mut design.document.tests, tests)?;
            design_artifact::revert_authored(PlanArtifact::Tests, design);
        }
        design.check_preview = crate::checks::preview_plan(&self.workspace, design).await?;
        design.check_preview_source = design.check_preview_fingerprint()?;
        document.version = document
            .version
            .checked_add(1)
            .context("plan version exhausted")?;
        let rendered = design_review::render(&document)?;
        self.persist_annotated_edit(
            session_id,
            &original,
            PlanReviewRevision::new(plan_id, document, rendered)?,
            &before,
            annotation,
            trace,
        )
    }

    fn persist_annotated_edit(
        &self,
        session_id: &str,
        expected: &PlanDocument,
        revision: PlanReviewRevision,
        before: &RenderedPlan,
        annotation: Vec<ReviewAnnotation>,
        trace: Option<&PlanEditTrace>,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let gate = self.working_document_gate(session_id, &expected.plan_id)?;
        let _guard = gate
            .lock()
            .map_err(|_| anyhow::anyhow!("plan mutation gate poisoned"))?;
        if self.read_working_document(session_id, &expected.plan_id)? != *expected {
            return Err(WorkingDocumentChanged.into());
        }
        let path = self
            .plan_dir(session_id, &revision.document.plan_id)
            .join("working.json");
        let (document, rendered, checksum) =
            self.persist_review_draft_locked(session_id, revision, trace)?;
        let reanchor = trace.map(|trace| trace.stage("delete.reanchor_annotations"));
        let mut retained = Vec::new();
        for mut item in annotation {
            let find = |line| {
                before.navigation.resolve_line(line).and_then(|previous| {
                    rendered.navigation.anchor.iter().find(|current| {
                        current.target == previous.target && current.label == previous.label
                    })
                })
            };
            let (Some(start), Some(end)) =
                (find(item.source.start_line), find(item.source.end_line))
            else {
                continue;
            };
            if item.parent_id.as_ref().is_some_and(|parent| {
                !retained
                    .iter()
                    .any(|saved: &ReviewAnnotation| &saved.id == parent)
            }) {
                continue;
            }
            item.source.start_line = start.line.min(end.line);
            item.source.end_line = start.line.max(end.line);
            item.anchor = Some(review_annotation::ReviewAnnotationAnchor {
                start: start.target.clone(),
                end: end.target.clone(),
            });
            retained.push(item);
        }
        if !retained.is_empty() {
            review_annotation::ReviewAnnotationStore::open(path.clone(), digest(&fs::read(path)?))?
                .replace(retained)?;
        }
        if let Some(stage) = reanchor {
            stage.complete(serde_json::json!({}));
        }
        Ok((document, rendered, checksum))
    }

    /// Read one submitted canonical revision for acceptance or timeline expansion.
    pub fn read_submitted_document(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<PlanDocument> {
        let path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("submitted-{revision:04}.json"));
        let content = fs::read_to_string(&path)
            .with_context(|| format!("read submitted plan document {}", path.display()))?;
        let document: PlanDocument = codec::from_str(&content)
            .with_context(|| format!("decode submitted plan document {}", path.display()))?;
        document.validate()?;
        Ok(document)
    }

    /// Delete one physical plan artifact after its control state retracts.
    pub fn delete_plan(&self, session_id: &str, plan_id: &str) -> Result<()> {
        let directory = self.plan_dir(session_id, plan_id);
        if directory.exists() {
            fs::remove_dir_all(&directory)
                .with_context(|| format!("delete plan directory {}", directory.display()))?;
        }
        Ok(())
    }

    /// Delete physical plan files for one removed Harness session.
    pub fn delete_session(&self, session_id: &str) -> Result<()> {
        let session_path = PathBuf::from(session_id);
        let mut component = session_path.components();
        anyhow::ensure!(
            matches!(component.next(), Some(std::path::Component::Normal(_)))
                && component.next().is_none(),
            "invalid Harness session identifier"
        );
        let directory = crate::storage::state::directory(&self.root)
            .join("plans")
            .join(session_id);
        if directory.exists() {
            fs::remove_dir_all(&directory).with_context(|| {
                format!("delete session plan directory {}", directory.display())
            })?;
        }
        Ok(())
    }

    /// Resolve the physical editable path for Neovim PlanReview.
    pub fn working_path(&self, session_id: &str, plan_id: &str) -> PathBuf {
        self.plan_dir(session_id, plan_id).join("working.md")
    }

    /// Copy one complete plan artifact into a forked Harness session.
    pub fn copy_plan(
        &self,
        source_session_id: &str,
        source_plan_id: &str,
        target_session_id: &str,
        target_plan_id: &str,
    ) -> Result<PathBuf> {
        let source = self.plan_dir(source_session_id, source_plan_id);
        let target = self.plan_dir(target_session_id, target_plan_id);
        fs::create_dir_all(target.join("revisions"))?;
        let mut working = self.read_working_document(source_session_id, source_plan_id)?;
        working.plan_id = target_plan_id.to_owned();
        self.create_working_document(target_session_id, target_plan_id, &working)?;
        let source_revision = source.join("revisions");
        if source_revision.exists() {
            for entry in fs::read_dir(&source_revision)? {
                let entry = entry?;
                let path = entry.path();
                let name = entry.file_name().to_string_lossy().into_owned();
                if !entry.file_type()?.is_file()
                    || path.extension().and_then(|extension| extension.to_str()) != Some("json")
                    || name.ends_with(".index.json")
                {
                    continue;
                }
                let mut document = codec::from_slice::<PlanDocument>(&fs::read(&path)?)?;
                document.plan_id = target_plan_id.to_owned();
                let rendered = render_plan_at(&document, &self.workspace)?;
                let stem = name.trim_end_matches(".json");
                let target_revision = target.join("revisions");
                write_json_atomically(&target_revision.join(format!("{stem}.json")), &document)?;
                write_text_atomically(
                    &target_revision.join(format!("{stem}.md")),
                    &rendered.markdown,
                )?;
                write_json_atomically(
                    &target_revision.join(format!("{stem}.index.json")),
                    &rendered.navigation,
                )?;
            }
        }
        Ok(target.join("working.md"))
    }

    fn plan_dir(&self, session_id: &str, plan_id: &str) -> PathBuf {
        crate::storage::state::directory(&self.root)
            .join("plans")
            .join(session_id)
            .join(plan_id)
    }
}

fn write_json_atomically(path: &Path, value: &impl Serialize) -> Result<()> {
    let content = crate::plan::codec::to_vec_pretty(value)?;
    write_bytes_atomically(path, &content)
}

fn write_text_atomically(path: &Path, value: &str) -> Result<()> {
    write_bytes_atomically(path, value.as_bytes())
}

fn write_bytes_atomically(path: &Path, value: &[u8]) -> Result<()> {
    let temporary = path.with_extension(format!(
        "{}.tmp-{}",
        path.extension()
            .and_then(|extension| extension.to_str())
            .unwrap_or("data"),
        uuid::Uuid::new_v4()
    ));
    let result = (|| {
        use std::io::Write;
        let mut file = fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temporary)
            .with_context(|| format!("create temporary plan artifact {}", temporary.display()))?;
        file.write_all(value)?;
        file.sync_all()?;
        drop(file);
        fs::rename(&temporary, path)
            .with_context(|| format!("replace plan artifact {}", path.display()))
    })();
    if result.is_err() {
        let _ = fs::remove_file(&temporary);
    }
    result
}

/// Resolve a stable content digest for immutable plan acceptance.
pub fn digest(content: &[u8]) -> String {
    hex::encode(Sha256::digest(content))
}

#[cfg(test)]
mod test {
    #[test]
    fn input_timeout_preserves_confirmed_answers_and_marks_automatic_fallbacks() {
        use crate::session::ResolutionSource;
        let questions: PlanQuestionSet = serde_json::from_value(serde_json::json!({"id":"choices","questions":[
            {"id":"confirmed","header":"Confirmed","question":"Which format?","options":[{"label":"A","description":"A"},{"label":"B","description":"B"}]},
            {"id":"default","header":"Default","question":"Which option?","options":[{"label":"First","description":"First"},{"label":"Second (Recommended)","description":"Second"}]},
            {"id":"freeform","header":"Freeform","question":"What name?","options":[]}
        ]})).unwrap();
        let mut elicitation = PlanElicitation::new(questions.normalize().unwrap());
        elicitation
            .answer(
                "confirmed",
                PlanQuestionResponse::Selected {
                    option: "B".into(),
                    feedback: Some("keep note".into()),
                },
            )
            .unwrap();
        elicitation.expire().unwrap();
        assert_eq!(elicitation.answer[0].source, ResolutionSource::User);
        assert!(
            matches!(&elicitation.answer[0].response, PlanQuestionResponse::Selected {feedback:Some(note),..} if note == "keep note")
        );
        assert!(
            matches!(&elicitation.answer[1].response, PlanQuestionResponse::Selected {option,..} if option == "Second (Recommended)")
        );
        assert_eq!(elicitation.answer[1].source, ResolutionSource::Timeout);
        assert_eq!(
            elicitation.answer[2].response,
            PlanQuestionResponse::Skipped
        );
        assert!(elicitation.current_question().is_none());
        assert!(elicitation.feedback().contains("not user answers"));
        assert!(elicitation.feedback().contains("[automatic timeout]"));
        let mut fallback = PlanElicitation::new(elicitation.question_set.clone());
        fallback.question_set.questions[1].options[1].recommended = false;
        fallback.expire().unwrap();
        assert!(
            matches!(&fallback.answer[1].response, PlanQuestionResponse::Selected {option,..} if option == "First")
        );
        let mut acceptance =
            PlanReview::new("digest".into(), &[ApprovalMode::Ask, ApprovalMode::Ask]).unwrap();
        assert_eq!(acceptance.overall_comment, None);
        acceptance
            .elicitation
            .start_timeout(Default::default(), 0)
            .unwrap();
        assert!(acceptance.elicitation.deadline.is_none());
        assert!(acceptance.elicitation.expire().is_err());
    }

    use super::*;

    #[test]
    fn concurrent_plan_writers_cannot_both_commit_the_same_baseline() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let root = temporary.path().join("data");
        let workspace = temporary.path().to_path_buf();
        let initial_store = PlanFileStore::new(&root, &workspace).unwrap();
        let original = document::test_fixture("race-plan", "original");
        initial_store.write_working_document("session", "race-plan", &original)?;
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let workers: Vec<_> = ["first", "second"]
            .into_iter()
            .map(|name| {
                let root = root.clone();
                let workspace = workspace.clone();
                let original = original.clone();
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    let store = PlanFileStore::new(root, workspace).unwrap();
                    let mut updated = original.clone();
                    updated.overview = name.into();
                    barrier.wait();
                    store.write_working_document_if_unchanged(
                        "session",
                        "race-plan",
                        &original,
                        &updated,
                    )
                })
            })
            .collect();
        let results = workers
            .into_iter()
            .map(|worker| worker.join().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(results.iter().filter(|result| result.is_ok()).count(), 1);
        assert!(
            results
                .iter()
                .filter_map(|result| result.as_ref().err())
                .all(|error| error.downcast_ref::<WorkingDocumentChanged>().is_some())
        );
        Ok(())
    }
}
