use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

use crate::session::{PermissionMode, continuation::ContinuationBudget};

mod audit;
pub(crate) mod conformance;
pub(crate) mod implementation_report;
mod comment_lint;
mod usage;
pub(crate) use design::workspace_source;
mod design;
mod design_document;
mod design_flows;
mod design_tests;
pub(crate) use design_tests::DesignTestSelection;
pub(crate) mod calls;
mod references;
mod reference_context;
pub use calls::{CallKind, CallSite, CallPosition, FunctionBody};
mod design_review;
mod deviation;
mod document;
mod review_file_layout;
pub use design::{DeclarationDesign, DeclarationFile, DesignPatchRequest};
mod edit;
pub(crate) mod event;
pub use event::{ExchangeAnchor, ExchangePlanEvent, PlanEventContent};
mod graph;
mod prompt;
mod render;
pub(crate) mod revision;
pub(crate) mod revision_review;
mod resolution;
mod review_annotation;
mod review_feedback;
pub(crate) use review_feedback::render as render_review_feedback;
pub(crate) use review_annotation::resolve_annotations;
pub use review_annotation::{ReviewAnnotation, ReviewAnnotationAnchor, ReviewAnnotationKind, ReviewQuestionReply};
pub(crate) mod review_document;
mod review_projection;
pub(crate) mod review_source;
mod scheduler;
pub(crate) mod execution;
pub(crate) mod checks;
pub use execution::{PlanExecutionState, PlanExecutionRecord, PlanExecutionLifecycleEvent, PlanExecutionLifecycleRecord, PlanPhase};
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
#[cfg(test)]
pub(crate) use edit::plan_edit_request_schema;
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

/// Owns the durable reviewer decisions required before a plan can execute.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAcceptance {
    pub review_digest: String,
    #[serde(default)]
    pub saved_source_digest: Option<String>,
    pub execution_mode_list: Vec<PermissionMode>,
    pub elicitation: PlanElicitation,
}

impl PlanAcceptance {
    /// Build acceptance questions from the execution modes exposed by the active backend.
    pub fn new(review_digest: String, execution_mode_list: &[PermissionMode]) -> Result<Self> {
        anyhow::ensure!(
            !execution_mode_list.is_empty(),
            "the active backend exposes no execution mode"
        );
        let mut question_list = Vec::new();
        if !execution_mode_list.is_empty() {
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
            saved_source_digest: None,
            execution_mode_list: execution_mode_list.to_vec(),
            elicitation: PlanElicitation::new(question_set),
        })
    }

    /// Resolve the selected execution boundary only after every acceptance question has an answer.
    pub fn execution_mode(&self) -> Result<PermissionMode> {
        if self.execution_mode_list.len() == 1 {
            return Ok(self.execution_mode_list[0]);
        }
        match selected_option(&self.elicitation, "acceptance-execution-mode")? {
            "Read" => Ok(PermissionMode::Read),
            "Write (Recommended)" => Ok(PermissionMode::Write),

            "YOLO" => Ok(PermissionMode::Yolo),
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

const fn execution_mode_option_label(mode: PermissionMode) -> &'static str {
    match mode {
        PermissionMode::Read => "Read",
        PermissionMode::Write => "Write (Recommended)",

        PermissionMode::Yolo => "YOLO",
    }
}

const fn execution_mode_option_description(mode: PermissionMode) -> &'static str {
    match mode {
        PermissionMode::Read => "Ask before edits and untrusted commands.",
        PermissionMode::Write => "Apply saved approval rules within configured access.",

        PermissionMode::Yolo => "Run without interactive approval checks.",
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
        self.clarification_active = true;
        Ok(())
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
    implementation_report_cache: std::sync::Mutex<std::collections::HashMap<String, std::sync::Arc<implementation_report::ImplementationReport>>>,
    root: PathBuf,
    workspace: PathBuf,
}

impl PlanFileStore {
    /// Build a plan file store beneath the Harness data directory.
    pub(crate) fn check_output_directory(&self, session: &str, run: &str) -> Result<PathBuf> {
        let path = self.root.join(session).join("checks").join(run);
        fs::create_dir_all(&path)?;
        Ok(path)
    }

    pub fn new(root: impl Into<PathBuf>, workspace: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            workspace: workspace.into(),
            implementation_report_cache: Default::default(),
        }
    }

    /// Share immutable report contents without copying them into durable exchanges.
    pub(crate) fn implementation_report(&self, reference: &implementation_report::ImplementationReportRef)
        -> Result<std::sync::Arc<implementation_report::ImplementationReport>> {
        let mut cache = self.implementation_report_cache.lock().map_err(|_| anyhow::anyhow!("report cache lock poisoned"))?;
        if let Some(report) = cache.get(&reference.object_id) { return Ok(report.clone()); }
        let objects = crate::storage::objects::ObjectStore::open(&self.root)?;
        let report = std::sync::Arc::new(implementation_report::ImplementationReport::load(&objects, reference)?);
        cache.insert(reference.object_id.clone(), report.clone());
        Ok(report)
    }

    /// Restore the accepted working view without overwriting any submitted revision.
    pub(crate) fn restore_revision(&self, session_id: &str, plan_id: &str, revision: u32) -> Result<PlanDocument> {
        let document = self.read_submitted_document(session_id, plan_id, revision)?;
        let directory = self.plan_dir(session_id, plan_id);
        for extension in ["md", "index.json"] {
            let source = directory.join("revisions").join(format!("submitted-{revision:04}.{extension}"));
            write_bytes_atomically(&directory.join(format!("working.{extension}")), &fs::read(source)?)?;
        }
        self.write_working_document(session_id, plan_id, &document)?;
        Ok(document)
    }

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

    /// Rename one introduced definition and freeze its revision using only saved plan files.
    pub(crate) fn rename_symbol(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
        identity: &str,
        name: &str,
    ) -> Result<(String, PlanDocument, RenderedPlan, String)> {
        let document = self.read_working_document(session_id, plan_id)?;
        anyhow::ensure!(
            document.version == expected_version,
            "plan version changed before rename"
        );
        let index = references::PlanReferenceIndex::planned(&document, &self.workspace)?;
        let (_, definition) = index.rename_definition(&document, identity)?;
        let mut renamed = index.renamed(&document, identity, name)?;
        renamed.design = Some(renamed.design.as_ref().unwrap().formatted()?);
        renamed.validate_for_submission()?;
        let (document, rendered, digest) =
            self.persist_revision(session_id, plan_id, revision, renamed)?;
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
        let result = edit::rename_added_entity(&document, entity_name, new_name)?;
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
        self.submit_validated_document_revision(
            session_id,
            plan_id,
            revision,
            expected_version,
            document,
        )
    }

    /// Freeze one externally validated document without re-reading stale derived state.
    pub fn submit_validated_document_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        expected_version: u64,
        mut document: PlanDocument,
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
        if let Some(design) = &document.design {
            let formatted = design.formatted()?;
            formatted.check_workspace(&self.workspace)?;
            comment_lint::validate(design)?;
            usage::validate(&formatted, &self.workspace)?;
            document.design = Some(formatted);
        }
        document.validate_for_submission()?;
        if let Some(design) = &mut document.design {
            if design
                .validation
                .as_ref()
                .is_none_or(|report| report.fingerprint != crate::declaration::fingerprint(design))
            {
                design.validation = Some(
                    crate::declaration::DeclarationResolver::local(&self.workspace, design, false)?
                        .validate(design),
                );
            }
            design.validation.as_ref().unwrap().ensure_valid()?;
        }
        self.persist_revision(session_id, plan_id, revision, document)
    }

    /// Freeze a validated execution revision without requiring unchanged original sources.
    pub(crate) fn submit_execution_revision(&self, session_id: &str, plan_id: &str, revision: u32, mut document: PlanDocument) -> Result<(PlanDocument, RenderedPlan, String)> {
        let current = self.read_working_document(session_id, plan_id)?;
        anyhow::ensure!(current.version == document.version && document.plan_id == plan_id, "execution revision changed before submission");
        let design = document.design.as_ref().context("execution revision requires semantic design")?.formatted()?;
        comment_lint::validate(&design)?;
        usage::validate(&design, &self.workspace)?;
        design.validation.as_ref().context("execution revision has no validation evidence")?.ensure_valid()?;
        document.design = Some(design);
        document.validate_for_submission()?;
        self.persist_revision(session_id, plan_id, revision, document)
    }

    fn persist_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        document: PlanDocument,
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let rendered = render_plan_at(&document, &self.workspace)?;
        self.write_working_document(session_id, plan_id, &document)?;
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

    /// Publish a reviewed test-inventory edit without changing declarations or project files.
    pub(crate) fn remove_tests(
        &self, session_id: &str, plan_id: &str, revision: u32, expected_version: u64,
        expected_digest: &str, selected: &[DesignTestSelection],
    ) -> Result<(PlanDocument, RenderedPlan, String)> {
        let mut document = self.read_working_document(session_id, plan_id)?;
        anyhow::ensure!(document.version == expected_version, "plan version changed before test deletion");
        anyhow::ensure!(digest(&serde_json::to_vec(&document)?) == expected_digest, "plan changed before test deletion");
        let before = render_plan_at(&document, &self.workspace)?;
        let path = self.plan_dir(session_id, plan_id).join("working.json");
        let annotation = review_annotation::ReviewAnnotationStore::open(path.clone(), digest(&fs::read(&path)?))?
            .annotation().to_vec();
        design_tests::remove(&mut document.design.as_mut().context("test deletion requires a declaration design")?.document.tests, selected)?;
        document.version = document.version.checked_add(1).context("plan version exhausted")?;
        document.validate_for_submission()?;
        let (document, rendered, checksum) = self.persist_revision(session_id, plan_id, revision, document)?;
        let mut retained = Vec::new();
        for mut item in annotation {
            let find = |line| before.navigation.resolve_line(line).and_then(|previous| rendered.navigation.anchor.iter()
                .find(|current| current.target == previous.target && current.label == previous.label));
            let (Some(start), Some(end)) = (find(item.source.start_line), find(item.source.end_line)) else { continue; };
            if item.parent_id.as_ref().is_some_and(|parent| !retained.iter().any(|saved: &ReviewAnnotation| &saved.id == parent)) { continue; }
            item.source.start_line = start.line.min(end.line);
            item.source.end_line = start.line.max(end.line);
            item.anchor = Some(review_annotation::ReviewAnnotationAnchor { start: start.target.clone(), end: end.target.clone() });
            retained.push(item);
        }
        if !retained.is_empty() {
            review_annotation::ReviewAnnotationStore::open(path.clone(), digest(&fs::read(path)?))?.replace(retained)?;
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
        let document: PlanDocument = serde_json::from_str(&content)
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
    use super::*;

    #[test]
    fn rename_persists_from_plan_files_when_workspace_sources_are_unavailable() {
        let temporary = tempfile::tempdir().unwrap();
        fs::create_dir(temporary.path().join("src")).unwrap();
        fs::write(temporary.path().join("Cargo.toml"), "invalid manifest {").unwrap();
        fs::write(temporary.path().join("src/lib.rs"), "invalid source {").unwrap();
        let mut document = document::test_fixture("rename", "Rename a planned definition");
        let mut design = DeclarationDesign::default();
        design.document.objective = "Introduce a function".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Keep its calls consistent".into();
        design.baseline.insert(
            "src/lib.rs".into(),
            DeclarationFile {
                text: "pub fn existing();\n".into(),
                source_digest: "captured".into(),
            },
        );
        design.proposed.insert(
            "src/lib.rs".into(),
            "pub fn existing();\npub fn introduced();\n".into(),
        );
        design.proposed_calls.insert(
            "src/lib.rs".into(),
            vec![FunctionBody { evidence: None, change: None,
                owner: "introduced".into(),
                call: Some(vec![
                    CallSite {
                        kind: crate::plan::CallKind::Call, name: "introduced".into(),
                        source: None,
                        unresolved: false,
                    },
                    CallSite {
                        kind: crate::plan::CallKind::Call, name: "existing".into(),
                        source: None,
                        unresolved: false,
                    },
                    CallSite {
                        kind: crate::plan::CallKind::Call, name: "introduced".into(),
                        source: None,
                        unresolved: false,
                    },
                ]),
            }],
        );
        document.design = Some(design);
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        store
            .persist_revision("session", "rename", 1, document.clone())
            .unwrap();
        let previous = fs::read(
            store
                .plan_dir("session", "rename")
                .join("revisions/submitted-0001.json"),
        )
        .unwrap();
        let index = references::PlanReferenceIndex::planned(&document, temporary.path()).unwrap();
        let identity = &index.definition[&("src/lib.rs".into(), 2, 7)].0;
        let (_, renamed, _, _) = store
            .rename_symbol(
                "session",
                "rename",
                2,
                document.version,
                identity,
                "dispatch",
            )
            .unwrap();
        assert_eq!(renamed.version, document.version + 1);
        assert_eq!(
            renamed.design.as_ref().unwrap().baseline,
            document.design.as_ref().unwrap().baseline
        );
        assert_eq!(
            renamed.design.as_ref().unwrap().proposed_calls["src/lib.rs"][0]
                .call.iter().flatten()
                .map(|call| call.name.as_str())
                .collect::<Vec<_>>(),
            vec!["dispatch", "existing", "dispatch"]
        );
        assert_eq!(
            previous,
            fs::read(
                store
                    .plan_dir("session", "rename")
                    .join("revisions/submitted-0001.json")
            )
            .unwrap()
        );
        let current = fs::read(store.plan_dir("session", "rename").join("working.json")).unwrap();
        assert!(
            store
                .rename_symbol("session", "rename", 3, document.version, identity, "stale")
                .is_err()
        );
        let index = references::PlanReferenceIndex::planned(&renamed, temporary.path()).unwrap();
        let existing = &index.definition[&("src/lib.rs".into(), 1, 7)].0;
        assert!(
            store
                .rename_symbol(
                    "session",
                    "rename",
                    3,
                    renamed.version,
                    existing,
                    "forbidden"
                )
                .is_err()
        );
        assert_eq!(
            current,
            fs::read(store.plan_dir("session", "rename").join("working.json")).unwrap()
        );
        assert_eq!(
            fs::read_to_string(temporary.path().join("src/lib.rs")).unwrap(),
            "invalid source {"
        );
    }

    #[test]
    fn manifest_changes_capture_patch_submit_and_remain_visible_without_source_writes() {
        let temporary = tempfile::tempdir().unwrap();
        assert!(
            std::process::Command::new("git")
                .args(["init", "--quiet"])
                .current_dir(temporary.path())
                .status()
                .unwrap()
                .success()
        );
        let manifest = "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\nengine = { version = \"1.2\", default-features = false, features = [\"render\"] }\n";
        fs::write(temporary.path().join("Cargo.toml"), manifest).unwrap();
        let design = DeclarationDesign::open(temporary.path()).unwrap();
        assert!(design.baseline.is_empty());
        let invalid = "*** Begin Patch\n*** Update File: Cargo.toml\n@@\n name = \"arena\"\n+name = \"duplicate\"\n*** End Patch";
        assert!(design.patch(temporary.path(), &Default::default(), invalid).is_err());
        let patch = "*** Begin Patch\n*** Update File: Cargo.toml\n@@\n-engine = { version = \"1.2\", default-features = false, features = [\"render\"] }\n+engine = { version = \"1.3\", default-features = false, features = [\"render\", \"input\"] }\n*** Add File: config/arena.toml\n+[arena]\n+speed = 200\n*** Update File: plan.json\n@@\n-  \"design\": \"\",\n+  \"design\": \"Enable engine input and configure arena speed.\",\n*** End Patch";
        let mut document = document::test_fixture("plan", "Arena dependencies");
        document.design = Some(design.patch(temporary.path(), &Default::default(), patch).unwrap());
        document.design.as_mut().unwrap().document.objective =
            "Enable keyboard input with configurable arena movement.".into();
        document.design.as_mut().unwrap().document.background = "Cargo.toml already enables engine rendering.".into();
        document.design.as_mut().unwrap().document.requirements = vec!["Enable input without disabling rendering.".into()];
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        let (submitted, rendered, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        assert!(rendered.markdown.contains("Modified Cargo.toml"));
        assert!(rendered.markdown.contains("engine = { version = \"1.3\""));
        assert!(rendered.markdown.contains("config/arena.toml"));
        let (rows, targets) = design_review::project(
            &submitted,
            &Default::default(),
            &[],
            &Default::default(),
            None,
            &Default::default(),
            true,
            None,
            &Default::default(),
        )
        .unwrap();
        let text = rows
            .iter()
            .flat_map(|row| row.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Modified Cargo.toml"));
        assert!(text.contains("speed = 200"));
        assert!(targets.values().any(|anchor| matches!(&anchor.target, PlanReviewTarget::Declaration { path, side, .. } if path == "Cargo.toml" && side == "proposed")));
        assert_eq!(
            store
                .capture_review_source("session", "plan", 1, &checksum)
                .unwrap()
                .document
                .design,
            submitted.design
        );
        assert_eq!(
            fs::read_to_string(temporary.path().join("Cargo.toml")).unwrap(),
            manifest
        );
        assert!(!temporary.path().join("config/arena.toml").exists());
    }

    #[test]
    fn configuration_changes_capture_validate_submit_and_render_with_saved_targets() {
        let temporary = tempfile::tempdir().unwrap();
        assert!(
            std::process::Command::new("git")
                .args(["init", "--quiet"])
                .current_dir(temporary.path())
                .status()
                .unwrap()
                .success()
        );
        let configurations = [
            (
                "package.json",
                "{\"name\":\"before\"}\n",
                "{\"name\":\"after\"}\n",
                "{\"name\":}",
            ),
            (
                "tsconfig.json",
                "{\"strict\":false,}\n",
                "{// Checking\n\"strict\":true,}\n",
                "{\"strict\":true \"other\":false}",
            ),
            (
                "ci.yaml",
                "script: |\n  echo before\n",
                "script: |\n  echo after\n",
                "script: [unclosed",
            ),
            (
                "App.csproj",
                "<Project><Name>before</Name></Project>\n",
                "<Project><Name>after</Name></Project>\n",
                "<Project><Name></Project>",
            ),
        ];
        for (path, baseline, _, _) in configurations {
            fs::write(temporary.path().join(path), baseline).unwrap();
        }
        fs::write(
            temporary.path().join("plan.json"),
            "{\"project_setting\":true}",
        )
        .unwrap();
        let mut design = DeclarationDesign::open(temporary.path()).unwrap();
        assert!(!design.baseline.contains_key("plan.json"));
        for (path, baseline, proposed, invalid) in configurations {
            assert_eq!(design.inspect(temporary.path(), Some(path), false).unwrap()["text"], baseline);
            let before = design.clone();
            let invalid_patch = format!(
                "*** Begin Patch\n*** Update File: {path}\n@@\n-{}\n+{invalid}\n*** End Patch",
                baseline.trim_end()
            );
            assert!(design.patch(temporary.path(), &Default::default(), &invalid_patch).is_err(), "{path}");
            assert_eq!(design, before);
            let removed = baseline
                .lines()
                .map(|line| format!("-{line}"))
                .collect::<Vec<_>>()
                .join("\n");
            let added = proposed
                .lines()
                .map(|line| format!("+{line}"))
                .collect::<Vec<_>>()
                .join("\n");
            design = design.patch(temporary.path(), &Default::default(), &format!("*** Begin Patch\n*** Update File: {path}\n@@\n{removed}\n{added}\n*** End Patch")).unwrap();
        }
        design = design.patch(temporary.path(), &Default::default(), "*** Begin Patch\n*** Add File: settings.jsonc\n+{\"enabled\":true,}\n*** End Patch").unwrap();
        design = design
            .patch(temporary.path(), &Default::default(), "*** Begin Patch\n*** Delete File: settings.jsonc\n*** End Patch")
            .unwrap();
        assert!(!design.proposed.contains_key("settings.jsonc"));
        design.document.objective = "Update project configuration across supported formats.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design =
            "Update package metadata, type checks, CI, and the XML project.".into();
        let mut document = document::test_fixture("plan", "Configuration");
        document.design = Some(design);
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        let (submitted, rendered, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        assert!(rendered.markdown.contains("Modified package.json"));
        for public_only in [false, true] {
            let (rows, targets) = design_review::project(
                &submitted,
                &Default::default(),
                &[],
                &Default::default(),
                None,
                &Default::default(),
                public_only,
                None,
                &Default::default(),
            )
            .unwrap();
            let text = rows
                .iter()
                .flat_map(|row| row.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n");
            for (path, baseline, proposed, _) in configurations {
                assert!(text.contains(path));
                assert!(text.contains(proposed.lines().last().unwrap()));
                assert!(targets.values().any(|anchor| matches!(&anchor.target, PlanReviewTarget::Declaration { path: target_path, side, .. } if target_path == path && side == "proposed")));
                assert_eq!(
                    fs::read_to_string(temporary.path().join(path)).unwrap(),
                    baseline
                );
                assert_eq!(submitted.design.as_ref().unwrap().proposed[path], proposed);
            }
        }
        assert_eq!(
            store
                .capture_review_source("session", "plan", 1, &checksum)
                .unwrap()
                .document
                .design,
            submitted.design
        );
    }

    #[test]
    fn submission_formats_saved_snapshots_before_diffing_without_touching_source() {
        let temporary = tempfile::tempdir().unwrap();
        let source = "pub struct Registry { pub first: u64, pub second: u64 }\n";
        fs::write(temporary.path().join("registry.rs"), source).unwrap();
        let mut document = document::test_fixture("plan", "Registry");
        let mut design = DeclarationDesign::default();
        design.document.objective =
            "Format registry declarations without changing their interface.".into();
        design.document.background = "registry.rs declares two public fields.".into();
        design.document.requirements = vec!["Preserve the public interface.".into()];
        design.document.design = "Preserve the registry interface.".into();
        design.line_width = 60;
        design.baseline.insert(
            "registry.rs".into(),
            DeclarationFile {
                text: forge_diff::syntax::DeclarationOverview::extract("registry.rs", source)
                    .unwrap(),
                source_digest: digest(source.as_bytes()),
            },
        );
        design.proposed.insert(
            "registry.rs".into(),
            "pub struct Registry {\n pub first: u64,\n pub second: u64\n}\n".into(),
        );
        document.design = Some(design);
        let store = PlanFileStore::new(temporary.path().join("data"), temporary.path());
        store
            .write_working_document("session", "plan", &document)
            .unwrap();
        let (submitted, rendered, checksum) = store
            .submit_document_revision("session", "plan", 1, 1)
            .unwrap();
        let design = submitted.design.as_ref().unwrap();
        assert!(
            design.changed_paths().is_empty(),
            "formatting created a false design change"
        );
        assert!(rendered.markdown.contains("No declaration changes"));
        assert_eq!(
            design.proposed["registry.rs"],
            "pub struct Registry {\n  pub first: u64,\n  pub second: u64,\n}\n"
        );
        assert_eq!(
            store
                .read_working_document("session", "plan")
                .unwrap()
                .design,
            submitted.design
        );
        assert_eq!(
            store
                .capture_review_source("session", "plan", 1, &checksum)
                .unwrap()
                .document
                .design,
            submitted.design
        );
        assert_eq!(
            fs::read_to_string(temporary.path().join("registry.rs")).unwrap(),
            source
        );
        let request = DesignPatchRequest { plan_id: "plan".into(), expected_version: 1, title: None, source_digests: Default::default(),
            patch: "*** Begin Patch\n*** Update File: registry.rs\n@@\n-  pub second: u64,\n+  pub second: String,\n*** End Patch".into() };
        let revised = submitted.patch_design(temporary.path(), request).unwrap();
        store
            .write_working_document("session", "plan", &revised)
            .unwrap();
        let (submitted, _, _) = store
            .submit_validated_document_revision("session", "plan", 2, 2, revised)
            .unwrap();
        assert_eq!(
            submitted.design.as_ref().unwrap().changed_paths(),
            vec!["registry.rs"]
        );
        assert_eq!(submitted.design.as_ref().unwrap().line_width, 60);
        assert_eq!(
            fs::read_to_string(temporary.path().join("registry.rs")).unwrap(),
            source
        );
    }

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
                        plan: Some(PlanFieldPatch {
                            overview: Some("Edited".into()),
                            ..Default::default()
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
                        set: Some(PlanResourceSet {
                            entity_changes: Some(vec![ProgramEntityChange {
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
                            }]),
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

        let mut repaired_entity = incomplete
            .document
            .entity_changes
            .iter()
            .find(|entity| entity.name == "InspectionService")
            .unwrap()
            .clone();
        repaired_entity.members.push(ProgramEntityMemberChange {
            action: ChangeAction::Add,
            renamed_from: None,
            kind: MemberKind::Method,
            name: "inspect".into(),
            description: Some("Inspect input.".into()),
            visibility: Some(Visibility::Public),
            type_name: None,
            parameters: Vec::new(),
            return_type: Some("InspectionReport".into()),
        });
        let mut repaired_task = incomplete
            .document
            .tasks()
            .find(|task| task.title == "Create plan state")
            .unwrap()
            .clone();
        repaired_task.files.push(PlanFile {
            change: PlanFileChange::Add {
                path: "src/inspection.rs".into(),
            },
            subtasks: vec![PlanSubtask::Work(PlanWorkSubtask {
                action: SubtaskAction::Create,
                description: "the inspection owner.".into(),
                entities: vec!["InspectionService".into()],
            })],
        });
        let repaired = store
            .edit_working_document(
                "session",
                PlanEditRequest {
                    plan_id: "plan".into(),
                    expected_version: 2,
                    mutation: PlanMutation {
                        stages: Some(vec![PlanStage {
                            id: "foundation".into(),
                            title: "Establish plan state".into(),
                            tasks: vec![repaired_task],
                        }]),
                        set: Some(PlanResourceSet {
                            entity_changes: Some(vec![repaired_entity]),

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
    fn clarification_reopens_only_the_question_being_reconsidered() {
        let mut elicitation = PlanElicitation::new(PlanQuestionSet {
            id: "set".into(),
            questions: ["scope", "testing"]
                .into_iter()
                .map(|id| PlanQuestion {
                    id: id.into(),
                    header: id.into(),
                    question: id.into(),
                    options: Vec::new(),
                    allow_freeform: true,
                })
                .collect(),
        });
        for id in ["scope", "testing"] {
            elicitation
                .answer(
                    id,
                    PlanQuestionResponse::Other {
                        text: "chosen".into(),
                    },
                )
                .unwrap();
        }
        assert!(elicitation.current_question().is_none());
        elicitation.begin_clarification("scope").unwrap();
        assert_eq!(elicitation.current_question().unwrap().id, "scope");
        assert_eq!(elicitation.answer.len(), 1);
        assert_eq!(elicitation.answer[0].question_id, "testing");
        assert!(elicitation.clarification_active);
        assert!(elicitation.begin_clarification("missing").is_err());
        assert_eq!(elicitation.answer.len(), 1);
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
    fn acceptance_requires_execution_access_before_execution() {
        let mut acceptance = PlanAcceptance::new(
            "digest".into(),
            &[PermissionMode::Write, PermissionMode::Read],
        )
        .unwrap();
        assert_eq!(acceptance.elicitation.question_set.questions.len(), 1);
        assert!(acceptance.execution_mode().is_err());
        acceptance
            .elicitation
            .answer(
                "acceptance-execution-mode",
                PlanQuestionResponse::Selected {
                    option: "Write (Recommended)".into(),
                    feedback: None,
                },
            )
            .unwrap();
        assert_eq!(acceptance.execution_mode().unwrap(), PermissionMode::Write);
    }
}
