use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

use crate::session::{ExecutionMode, continuation::ContinuationBudget};

mod audit;
mod deviation;
mod document;
mod edit;
mod graph;
mod prompt;
mod render;
mod resolution;
mod scheduler;
pub mod state_machine;
mod validation;

pub use audit::{
    PlanAudit, PlanAuditPathDifference, PlanAuditTask, build_plan_audit, render_plan_audit,
};
pub use deviation::{
    EffectivePlan, PlanDeviation, PlanDeviationDisposition, PlanDeviationKind,
    PlanDeviationRequest, ScopeDeviationReview, build_effective_plan,
};
pub use document::*;
pub(crate) use edit::restore_internal_identity;
pub use edit::{
    CollectionMutation, EnumVariantFieldPatch, EnumVariantPatch, PatchField, PlanAssumptionPatch,
    PlanEditRequest, PlanEditResult, PlanFieldMutation, PlanFieldPatch, PlanFilePatch,
    PlanFlowPatch, PlanFlowStepPatch, PlanMutation, PlanSubtaskPatch, PlanTaskPatch,
    PlanTestSubtaskPatch, PlanWorkSubtaskPatch, ProgramEntityMemberPatch, ProgramEntityPatch,
    apply_plan_edit, apply_plan_mutation,
};
pub use graph::{PlanGraph, ResolvedPlanEntity};
pub use prompt::{PlanExecutionPromptKind, PlanPrompt, execution_prompt};
pub use render::{
    PlanNavigationAnchor, PlanNavigationIndex, PlanReviewReferenceKind, PlanReviewTarget,
    PlanSection, RenderedPlan, render_plan, render_plan_at, render_plan_delta,
};
pub use resolution::{
    PlanResolutionKind, PlanResolutionRecord, PlanTaskSummary, PlanTestSummary,
    build_plan_resolution,
};
pub use scheduler::{
    PlanScheduler, PlanTaskExecution, PlanTaskReport, PlanTaskState, PlanTestResult, PlanTestStatus,
};
pub use validation::{
    PlanValidationError, PlanValidationPhase, PlanViolation, validate_plan_edit,
    validate_plan_render, validate_plan_submission, validate_workspace_references,
};

/// Represents the review lifecycle of one model-authored plan.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanState {
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
    pub review_digest: Option<String>,
    pub accepted_digest: Option<String>,
    #[serde(default)]
    pub elicitation: Option<PlanElicitation>,
    #[serde(default)]
    pub acceptance: Option<PlanAcceptance>,
    #[serde(default)]
    pub question_ledger: PlanQuestionLedger,
    #[serde(default)]
    pub generation: PlanGeneration,
    #[serde(default)]
    pub validation_warning: Vec<PlanViolation>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
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
    /// Record one provider turn and return whether another planning turn may run.
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
    pub question_set: PlanQuestionSet,
    pub revision: u32,
    #[serde(default)]
    pub answer: Vec<PlanQuestionAnswer>,
    #[serde(default)]
    pub current_index: usize,
    #[serde(default)]
    pub clarification_active: bool,
}

/// Defines whether accepted-plan execution reuses or replaces planning context.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ContextChoice {
    Continue,
    Fresh,
}

/// Owns the durable reviewer decisions required before a plan can execute.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAcceptance {
    pub review_digest: String,
    pub execution_mode_list: Vec<ExecutionMode>,
    pub elicitation: PlanElicitation,
}

impl PlanAcceptance {
    /// Build acceptance questions from the execution modes exposed by the active backend.
    pub fn new(review_digest: String, execution_mode_list: &[ExecutionMode]) -> Result<Self> {
        let context_question = PlanQuestion {
            id: "acceptance-context".into(),
            header: "Context".into(),
            question: "Which provider context should execute the accepted plan?".into(),
            options: vec![
                PlanQuestionOption {
                    label: "Continue context".into(),
                    description: "Continue from the planning conversation.".into(),
                },
                PlanQuestionOption {
                    label: "Fresh context".into(),
                    description: "execution without the planning conversation.".into(),
                },
            ],
            allow_freeform: false,
        };
        anyhow::ensure!(
            !execution_mode_list.is_empty(),
            "the active backend exposes no execution mode"
        );
        let mut question_list = vec![context_question];
        if execution_mode_list.len() > 1 {
            question_list.push(PlanQuestion {
                id: "acceptance-execution-mode".into(),
                header: "Execution access".into(),
                question: "What access should accepted-plan execution receive?".into(),
                options: execution_mode_list
                    .iter()
                    .copied()
                    .map(|mode| PlanQuestionOption {
                        label: execution_mode_option_label(mode).into(),
                        description: execution_mode_option_description(mode).into(),
                    })
                    .collect(),
                allow_freeform: false,
            });
        }
        let question_set = PlanQuestionSet {
            id: "plan-acceptance".into(),
            questions: question_list,
        }
        .normalize()?;
        Ok(Self {
            review_digest,
            execution_mode_list: execution_mode_list.to_vec(),
            elicitation: PlanElicitation::new(question_set),
        })
    }

    /// Resolve the selected context only after every acceptance question has an answer.
    pub fn context_choice(&self) -> Result<ContextChoice> {
        match selected_option(&self.elicitation, "acceptance-context")? {
            "Continue context" => Ok(ContextChoice::Continue),
            "Fresh context" => Ok(ContextChoice::Fresh),
            option => anyhow::bail!("unsupported plan context choice {option:?}"),
        }
    }

    /// Resolve the selected execution boundary only after every acceptance question has an answer.
    pub fn execution_mode(&self) -> Result<ExecutionMode> {
        if self.execution_mode_list.len() == 1 {
            return Ok(self.execution_mode_list[0]);
        }
        match selected_option(&self.elicitation, "acceptance-execution-mode")? {
            "Read-only" => Ok(ExecutionMode::Read),
            "Write workspace (Recommended)" => Ok(ExecutionMode::Write),
            "Full access" => Ok(ExecutionMode::Full),
            "YOLO" => Ok(ExecutionMode::Yolo),
            option => anyhow::bail!("unsupported execution access choice {option:?}"),
        }
    }
}

fn selected_option<'a>(elicitation: &'a PlanElicitation, question_id: &str) -> Result<&'a str> {
    let answer = elicitation
        .answer
        .iter()
        .find(|answer| answer.question_id == question_id)
        .with_context(|| format!("acceptance question {question_id:?} has no answer"))?;
    match &answer.response {
        PlanQuestionResponse::Selected { option, .. } => Ok(option),
        PlanQuestionResponse::Other { .. } | PlanQuestionResponse::Skipped => {
            anyhow::bail!("acceptance question {question_id:?} requires a selected option")
        }
    }
}

const fn execution_mode_option_label(mode: ExecutionMode) -> &'static str {
    match mode {
        ExecutionMode::Read => "Read-only",
        ExecutionMode::Write => "Write workspace (Recommended)",
        ExecutionMode::Full => "Full access",
        ExecutionMode::Yolo => "YOLO",
    }
}

const fn execution_mode_option_description(mode: ExecutionMode) -> &'static str {
    match mode {
        ExecutionMode::Read => "Inspect without changing workspace files.",
        ExecutionMode::Write => "Modify workspace files through the normal approval policy.",
        ExecutionMode::Full => "Use unrestricted filesystem and process access.",
        ExecutionMode::Yolo => "Run without interactive approval checks.",
    }
}

impl PlanElicitation {
    /// Build unresolved elicitation state from a normalized provider question set.
    pub fn new(question_set: PlanQuestionSet) -> Self {
        Self {
            question_set,
            revision: 1,
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

    /// Commit one response and advance presentation to the next question.
    pub fn answer(&mut self, question_id: &str, response: PlanQuestionResponse) -> Result<()> {
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
            question_id: question_id.to_owned(),
            response,
        });
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
            line_list.push(format!("- {}: {value}", question.header));
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
    pub fn normalize(mut self) -> Result<Self> {
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
            let maximum_option_count = if self.id == "plan-acceptance" { 4 } else { 3 };
            anyhow::ensure!(
                question.options.is_empty()
                    || (2..=maximum_option_count).contains(&question.options.len()),
                "structured planning questions require two or three choices, or four for plan acceptance"
            );
            for option in &question.options {
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

/// Defines terminal and nonterminal states for one accepted plan execution.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanExecutionState {
    Active,
    Complete,
    Paused,
    Stalled,
    Blocked,
    Cancelled,
}

/// Defines one scheduler-owned event in an accepted plan execution.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PlanExecutionLifecycleEvent {
    TaskStarted {
        task_id: String,
        ordinal: usize,
        total: usize,
        title: String,
    },
    TaskCompleted {
        task_id: String,
        ordinal: usize,
        total: usize,
        title: String,
        elapsed_ms: i64,
    },
    DeviationRecorded {
        deviation_id: String,
        summary: String,
    },
}

/// Tracks one causally ordered scheduler event for timeline projection.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanExecutionLifecycleRecord {
    pub sequence: u64,
    pub after_interaction_id: Option<String>,
    pub occurred_at_ms: i64,
    #[serde(flatten)]
    pub event: PlanExecutionLifecycleEvent,
}

/// Tracks one accepted plan through its guarded execution goal.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanExecutionRecord {
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub goal_id: String,
    pub state: PlanExecutionState,
    #[serde(default)]
    pub planning_backend_session_id: Option<String>,
    #[serde(default)]
    pub execution_backend_session_id: Option<String>,
    #[serde(default)]
    pub scheduler: PlanScheduler,
    #[serde(default)]
    pub lifecycle: Vec<PlanExecutionLifecycleRecord>,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
}

impl PlanExecutionRecord {
    /// Append one durable scheduler event after its causal interaction.
    pub fn append_lifecycle(
        &mut self,
        after_interaction_id: Option<String>,
        occurred_at_ms: i64,
        event: PlanExecutionLifecycleEvent,
    ) {
        let sequence = self
            .lifecycle
            .last()
            .map_or(1, |record| record.sequence.saturating_add(1));
        self.lifecycle.push(PlanExecutionLifecycleRecord {
            sequence,
            after_interaction_id,
            occurred_at_ms,
            event,
        });
    }
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
#[derive(Clone, Debug, Deserialize, Serialize)]
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

/// Owns physical plan files and immutable revision history.
pub struct PlanFileStore {
    root: PathBuf,
    workspace: PathBuf,
}

impl PlanFileStore {
    /// Build a plan file store beneath the Harness data directory.
    pub fn new(root: impl Into<PathBuf>, workspace: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            workspace: workspace.into(),
        }
    }

    /// Write the structurally valid working draft without rendering it.
    /// Preserve incomplete construction until submission validates the whole plan.
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
        document.validate()?;
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
        let document = serde_json::from_str::<PlanDocument>(&content)
            .with_context(|| format!("decode working plan document {}", path.display()))?;
        document.validate()?;
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
        self.write_working_document(session_id, &result.plan_id, &result.document)?;
        Ok(result)
    }

    /// Rename one newly added entity and persist the complete canonical document.
    pub fn rename_added_entity(
        &self,
        session_id: &str,
        plan_id: &str,
        entity_id: &str,
        new_name: String,
    ) -> Result<PlanEditResult> {
        let document = self.read_working_document(session_id, plan_id)?;
        let result = edit::rename_added_entity(&document, entity_id, new_name)?;
        self.write_working_document(session_id, &result.plan_id, &result.document)?;
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
        anyhow::ensure!(
            document.version == expected_version,
            "plan version changed before submission"
        );
        document.validate_for_submission()?;
        let rendered = render_plan_at(&document, &self.workspace)?;
        let plan_directory = self.plan_dir(session_id, plan_id);
        let revision_directory = plan_directory.join("revisions");
        fs::create_dir_all(&revision_directory)?;
        write_text_atomically(&plan_directory.join("working.md"), &rendered.markdown)?;
        write_json_atomically(
            &plan_directory.join("working.index.json"),
            &rendered.navigation,
        )?;
        let stem = format!("submitted-{revision:04}");
        write_json_atomically(&revision_directory.join(format!("{stem}.json")), &document)?;
        write_text_atomically(
            &revision_directory.join(format!("{stem}.md")),
            &rendered.markdown,
        )?;
        write_json_atomically(
            &revision_directory.join(format!("{stem}.index.json")),
            &rendered.navigation,
        )?;
        let checksum = digest(serde_json::to_vec(&document)?.as_slice());
        Ok((document, rendered, checksum))
    }

    /// Freeze one externally validated document without re-reading stale derived state.
    pub fn submit_validated_document_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
        document: PlanDocument,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let current = self.read_working_document(session_id, plan_id)?;
        anyhow::ensure!(
            current.version == expected_version,
            "plan version changed before validated submission"
        );
        anyhow::ensure!(
            document.plan_id == plan_id && document.version == expected_version,
            "validated document identity changed before submission"
        );
        document.validate_for_submission()?;
        self.write_working_document(session_id, plan_id, &document)?;
        let rendered = render_plan_at(&document, &self.workspace)?;
        let plan_directory = self.plan_dir(session_id, plan_id);
        let revision_directory = plan_directory.join("revisions");
        fs::create_dir_all(&revision_directory)?;
        write_text_atomically(&plan_directory.join("working.md"), &rendered.markdown)?;
        write_json_atomically(
            &plan_directory.join("working.index.json"),
            &rendered.navigation,
        )?;
        let stem = format!("submitted-{revision:04}");
        write_json_atomically(&revision_directory.join(format!("{stem}.json")), &document)?;
        write_text_atomically(
            &revision_directory.join(format!("{stem}.md")),
            &rendered.markdown,
        )?;
        write_json_atomically(
            &revision_directory.join(format!("{stem}.index.json")),
            &rendered.navigation,
        )?;
        let checksum = digest(serde_json::to_vec(&document)?.as_slice());
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
        serde_json::from_str(&content)
            .with_context(|| format!("decode submitted plan document {}", path.display()))
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
        let directory = self.root.join("plans").join(session_id);
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
        self.write_working_document(target_session_id, target_plan_id, &working)?;
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
                let mut document = serde_json::from_slice::<PlanDocument>(&fs::read(&path)?)?;
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
        self.root.join("plans").join(session_id).join(plan_id)
    }
}

fn write_json_atomically(path: &Path, value: &impl Serialize) -> Result<()> {
    let content = serde_json::to_vec_pretty(value)?;
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
    fs::write(&temporary, value)
        .with_context(|| format!("write temporary plan artifact {}", temporary.display()))?;
    if let Err(error) = fs::rename(&temporary, path) {
        if !path.exists() {
            return Err(error).with_context(|| format!("replace plan artifact {}", path.display()));
        }
        fs::remove_file(path)
            .with_context(|| format!("remove previous plan artifact {}", path.display()))?;
        fs::rename(&temporary, path)
            .with_context(|| format!("replace plan artifact {}", path.display()))?;
    }
    Ok(())
}

/// Resolve a stable content digest for immutable plan acceptance.
pub fn digest(content: &[u8]) -> String {
    hex::encode(Sha256::digest(content))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn preserves_submitted_json_revisions_while_the_working_document_changes() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let document = document::test_fixture("plan", "Initial");
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        store
            .edit_working_document(
                "session",
                PlanEditRequest {
                    plan_id: "plan".into(),
                    expected_version: 1,
                    mutation: PlanMutation {
                        plan: Some(PlanFieldMutation {
                            modify: PlanFieldPatch {
                                overview: Some("Edited".into()),
                                ..Default::default()
                            },
                        }),
                        ..Default::default()
                    },
                },
            )
            .unwrap();
        assert_eq!(
            store
                .read_submitted_document("session", "plan", 1)
                .unwrap()
                .overview,
            "Initial"
        );
        assert!(
            temporary
                .path()
                .join("plans/session/plan/revisions/submitted-0001.json")
                .exists()
        );
        assert!(
            temporary
                .path()
                .join("plans/session/plan/revisions/submitted-0001.index.json")
                .exists()
        );
        store.delete_session("session").unwrap();
        assert!(!temporary.path().join("plans/session").exists());
    }

    #[test]
    fn renders_only_complete_submissions_while_preserving_repairable_drafts() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path(), temporary.path());
        let document = document::test_fixture("plan", "Initial");
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let working_markdown_path = temporary.path().join("plans/session/plan/working.md");
        let submitted_markdown = fs::read_to_string(&working_markdown_path).unwrap();

        let incomplete = store
            .edit_working_document(
                "session",
                PlanEditRequest {
                    plan_id: "plan".into(),
                    expected_version: 1,
                    mutation: PlanMutation {
                        entity_changes: Some(CollectionMutation {
                            add: vec![ProgramEntityChange {
                                entity_id: "inspection_service".into(),
                                action: EntityChangeAction::Add,
                                kind: EntityKind::Struct,
                                renamed_from: None,
                                name: "InspectionService".into(),
                                description: "Own inspection.".into(),
                                path: "src/inspection.rs".into(),
                                members: Vec::new(),
                                variants: Vec::new(),
                                extends: None,
                                conforms_to: Vec::new(),
                            }],
                            ..Default::default()
                        }),
                        ..Default::default()
                    },
                },
            )
            .unwrap();
        assert_eq!(incomplete.version, 2);
        assert_eq!(
            fs::read_to_string(&working_markdown_path).unwrap(),
            submitted_markdown,
            "draft edits must not replace the last validated human projection"
        );
        let submission_error = store
            .submit_document_revision("session", "plan", 2, 2)
            .unwrap_err()
            .to_string();
        assert!(submission_error.contains("must belong to exactly one subtask"));

        let repaired = store
            .edit_working_document(
                "session",
                PlanEditRequest {
                    plan_id: "plan".into(),
                    expected_version: 2,
                    mutation: PlanMutation {
                        entity_changes: Some(CollectionMutation {
                            modify: vec![ProgramEntityPatch {
                                entity_id: "InspectionService".into(),
                                action: None,
                                kind: None,
                                renamed_from: PatchField::Missing,
                                name: None,
                                description: None,
                                path: None,
                                members: Some(CollectionMutation {
                                    add: vec![ProgramEntityMemberChange {
                                        member_id: "inspect".into(),
                                        action: ChangeAction::Add,
                                        kind: MemberKind::Method,
                                        name: "inspect".into(),
                                        description: "Inspect input.".into(),
                                        visibility: Some(Visibility::Public),
                                        type_name: None,
                                        parameters: Vec::new(),
                                        return_type: Some("InspectionReport".into()),
                                    }],
                                    ..Default::default()
                                }),
                                variants: None,
                                extends: PatchField::Missing,
                                conforms_to: None,
                            }],
                            ..Default::default()
                        }),
                        tasks: Some(CollectionMutation {
                            modify: vec![PlanTaskPatch {
                                task_id: "Create plan state".into(),
                                title: None,
                                description: None,
                                files: Some(CollectionMutation {
                                    add: vec![PlanFile {
                                        change: PlanFileChange::Add {
                                            path: "src/inspection.rs".into(),
                                        },
                                        subtasks: vec![PlanSubtask::Work(PlanWorkSubtask {
                                            subtask_id: "create_inspection".into(),
                                            action: SubtaskAction::Create,
                                            description: "the inspection owner.".into(),
                                            entity_ids: vec!["inspection_service".into()],
                                        })],
                                    }],
                                    ..Default::default()
                                }),
                            }],
                            ..Default::default()
                        }),
                        ..Default::default()
                    },
                },
            )
            .unwrap();
        assert_eq!(repaired.version, 3);
        store
            .submit_document_revision("session", "plan", 2, 3)
            .unwrap();
        assert!(
            fs::read_to_string(working_markdown_path)
                .unwrap()
                .contains("InspectionService")
        );
    }

    #[test]
    fn normalizes_durable_question_identifiers_and_freeform_defaults() {
        let question = PlanQuestionSet {
            id: String::new(),
            questions: vec![PlanQuestion {
                id: String::new(),
                header: String::new(),
                question: "Which migration?".into(),
                options: vec![
                    PlanQuestionOption {
                        label: "Staged".into(),
                        description: "Support both formats.".into(),
                    },
                    PlanQuestionOption {
                        label: "Immediate".into(),
                        description: "Replace immediately.".into(),
                    },
                ],
                allow_freeform: true,
            }],
        }
        .normalize()
        .unwrap();
        assert!(!question.id.is_empty());
        assert_eq!(question.questions[0].header, "Question 1");
        assert!(!question.questions[0].id.is_empty());
        assert!(question.questions[0].allow_freeform);
    }

    #[test]
    fn normalizes_identical_question_content_to_the_same_identity() {
        let question_set = PlanQuestionSet::freeform("Which output?".into());
        let first = question_set.clone().normalize().unwrap();
        let second = question_set.normalize().unwrap();
        assert_eq!(first.id, second.id);
        assert_eq!(first.questions[0].id, second.questions[0].id);
    }

    #[test]
    fn ledger_suppresses_resolved_content_even_when_the_provider_changes_ids() {
        let first = PlanQuestion {
            id: "provider-id-1".into(),
            header: "Output".into(),
            question: "Which output?".into(),
            options: Vec::new(),
            allow_freeform: true,
        };
        let mut ledger = PlanQuestionLedger::default();
        ledger.resolve(
            &first,
            Some(PlanQuestionResponse::Other {
                text: "Batch preview".into(),
            }),
            1,
        );
        let repeated = PlanQuestionSet {
            id: "new-set".into(),
            questions: vec![PlanQuestion {
                id: "provider-id-2".into(),
                header: "Renamed output header".into(),
                ..first
            }],
        };
        assert!(ledger.unresolved(repeated).is_none());
    }

    #[test]
    fn generation_stalls_after_two_turns_without_canonical_progress() {
        let mut generation = PlanGeneration::default();
        assert!(generation.observe(false));
        assert!(!generation.observe(false));
        generation.reset();
        assert!(generation.observe(true));
        assert_eq!(generation.budget.consecutive_no_progress, 0);
    }

    #[test]
    fn skipped_questions_commit_a_terminal_resolution() {
        let question = PlanQuestion {
            id: "output".into(),
            header: "Output".into(),
            question: "Which output?".into(),
            options: Vec::new(),
            allow_freeform: true,
        };
        let mut ledger = PlanQuestionLedger::default();
        ledger.resolve(&question, Some(PlanQuestionResponse::Skipped), 1);
        assert_eq!(
            ledger.resolution[0].kind,
            PlanQuestionResolutionKind::Skipped
        );
        assert!(
            ledger
                .unresolved(PlanQuestionSet {
                    id: "repeat".into(),
                    questions: vec![question],
                })
                .is_none()
        );
    }

    #[test]
    fn preserves_answer_notes_skips_and_unanswered_questions() {
        let question_set = PlanQuestionSet {
            id: "set".into(),
            questions: vec![
                PlanQuestion {
                    id: "migration".into(),
                    header: "Migration".into(),
                    question: "Which migration?".into(),
                    options: vec![
                        PlanQuestionOption {
                            label: "Staged".into(),
                            description: "Support both formats.".into(),
                        },
                        PlanQuestionOption {
                            label: "Immediate".into(),
                            description: "Replace immediately.".into(),
                        },
                    ],
                    allow_freeform: true,
                },
                PlanQuestion {
                    id: "storage".into(),
                    header: "Storage".into(),
                    question: "Which store?".into(),
                    options: Vec::new(),
                    allow_freeform: true,
                },
                PlanQuestion {
                    id: "testing".into(),
                    header: "Testing".into(),
                    question: "Which tests?".into(),
                    options: Vec::new(),
                    allow_freeform: true,
                },
            ],
        };
        let mut elicitation = PlanElicitation::new(question_set);
        elicitation
            .answer("storage", PlanQuestionResponse::Skipped)
            .unwrap();
        assert_eq!(elicitation.current_question().unwrap().id, "migration");
        elicitation
            .answer(
                "migration",
                PlanQuestionResponse::Selected {
                    option: "Staged".into(),
                    feedback: Some("Keep one compatibility release".into()),
                },
            )
            .unwrap();

        let feedback = elicitation.feedback();
        assert!(feedback.contains("Staged — Keep one compatibility release"));
        assert_eq!(feedback.matches("intentionally unanswered").count(), 2);
        assert_eq!(elicitation.current_question().unwrap().id, "testing");
    }

    #[test]
    fn replaces_questions_and_preserves_only_valid_answers() {
        let option = |label: &str| PlanQuestionOption {
            label: label.into(),
            description: format!("Use {label}"),
        };
        let question = |id: &str, options: Vec<PlanQuestionOption>, allow_freeform| PlanQuestion {
            id: id.into(),
            header: id.into(),
            question: format!("Choose {id}"),
            options,
            allow_freeform,
        };
        let mut elicitation = PlanElicitation::new(PlanQuestionSet {
            id: "first".into(),
            questions: vec![
                question("kept", vec![option("Staged"), option("Immediate")], true),
                question("invalid", vec![option("Local"), option("Remote")], true),
                question("removed", Vec::new(), true),
            ],
        });
        elicitation
            .answer(
                "kept",
                PlanQuestionResponse::Selected {
                    option: "Staged".into(),
                    feedback: Some("retain feedback".into()),
                },
            )
            .unwrap();
        elicitation
            .answer(
                "invalid",
                PlanQuestionResponse::Other {
                    text: "custom".into(),
                },
            )
            .unwrap();
        elicitation
            .answer("removed", PlanQuestionResponse::Skipped)
            .unwrap();

        elicitation.replace_question_set(PlanQuestionSet {
            id: "second".into(),
            questions: vec![
                question("kept", vec![option("Staged"), option("Immediate")], true),
                question("invalid", vec![option("Local"), option("Remote")], false),
                question("new", Vec::new(), true),
            ],
        });

        assert_eq!(elicitation.revision, 2);
        assert_eq!(elicitation.answer.len(), 1);
        assert_eq!(elicitation.answer[0].question_id, "kept");
        assert_eq!(elicitation.current_question().unwrap().id, "invalid");
        assert!(!elicitation.clarification_active);
    }

    #[test]
    fn model_answer_advances_and_revises_elicitation_presentation() {
        let mut elicitation = PlanElicitation::new(PlanQuestionSet {
            id: "set".into(),
            questions: vec![PlanQuestion {
                id: "migration".into(),
                header: "Migration".into(),
                question: "Which migration?".into(),
                options: vec![PlanQuestionOption {
                    label: "Staged".into(),
                    description: "Preserve compatibility.".into(),
                }],
                allow_freeform: true,
            }],
        });
        elicitation
            .answer_from_model(
                "migration",
                PlanQuestionResponse::Selected {
                    option: "Staged".into(),
                    feedback: None,
                },
            )
            .unwrap();
        assert_eq!(elicitation.revision, 2);
        assert!(elicitation.current_question().is_none());
    }

    #[test]
    fn acceptance_requires_context_and_execution_access_before_execution() {
        let mut acceptance = PlanAcceptance::new(
            "digest".into(),
            &[ExecutionMode::Write, ExecutionMode::Read],
        )
        .unwrap();
        assert_eq!(acceptance.elicitation.question_set.questions.len(), 2);
        assert!(acceptance.context_choice().is_err());
        acceptance
            .elicitation
            .answer(
                "acceptance-context",
                PlanQuestionResponse::Selected {
                    option: "Fresh context".into(),
                    feedback: None,
                },
            )
            .unwrap();
        acceptance
            .elicitation
            .answer(
                "acceptance-execution-mode",
                PlanQuestionResponse::Selected {
                    option: "Write workspace (Recommended)".into(),
                    feedback: None,
                },
            )
            .unwrap();
        assert_eq!(acceptance.context_choice().unwrap(), ContextChoice::Fresh);
        assert_eq!(acceptance.execution_mode().unwrap(), ExecutionMode::Write);
    }
}
