use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

mod audit;
mod deviation;
mod document;
mod edit;
mod prompt;
mod render;
mod resolution;
mod scheduler;

pub use audit::{
    PlanAudit, PlanAuditPathDifference, PlanAuditTask, build_plan_audit, render_plan_audit,
};
pub use deviation::{
    EffectivePlan, PlanDeviation, PlanDeviationDisposition, PlanDeviationKind,
    PlanDeviationRequest, ScopeDeviationReview, build_effective_plan,
};
pub use document::*;
pub use edit::{PlanEditOperation, PlanEditRequest, PlanEditResult, TestCategory, apply_plan_edit};
pub use prompt::PlanPrompt;
pub use render::{PlanNavigationIndex, PlanNavigationTarget, RenderedPlan, render_plan};
pub use resolution::{
    PlanResolutionKind, PlanResolutionRecord, PlanTaskSummary, PlanTestSummary,
    build_plan_resolution,
};
pub use scheduler::{
    PlanScheduler, PlanTaskExecution, PlanTaskReport, PlanTaskState, PlanTestResult, PlanTestStatus,
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
    #[serde(default = "default_allow_freeform", alias = "allowFreeform")]
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
            self.id = uuid::Uuid::new_v4().to_string();
        }
        for (index, question) in self.questions.iter_mut().enumerate() {
            anyhow::ensure!(
                !question.question.trim().is_empty(),
                "planning question text cannot be empty"
            );
            anyhow::ensure!(
                question.options.is_empty() || (2..=3).contains(&question.options.len()),
                "structured planning questions require two or three choices"
            );
            for option in &question.options {
                anyhow::ensure!(
                    !option.label.trim().is_empty() && !option.description.trim().is_empty(),
                    "planning question choices require labels and descriptions"
                );
            }
            if question.id.is_empty() {
                question.id = format!("{}:{}", self.id, index + 1);
            }
            if question.header.trim().is_empty() {
                question.header = format!("Question {}", index + 1);
            }
        }
        Ok(self)
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
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
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

/// Represents one annotation anchored to a reviewed plan line.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAnnotation {
    pub line: u32,
    pub body: String,
}

/// Owns physical plan files and immutable revision history.
pub struct PlanFileStore {
    root: PathBuf,
}

impl PlanFileStore {
    /// Build a plan file store beneath the Harness data directory.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    /// Create or replace the canonical working document and its human projection.
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
        let rendered = render_plan(document)?;
        write_json_atomically(&directory.join("working.json"), document)?;
        write_text_atomically(&directory.join("working.md"), &rendered.markdown)?;
        write_json_atomically(&directory.join("working.index.json"), &rendered.navigation)?;
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

    /// Apply one atomic semantic edit and refresh both projections.
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
        let rendered = render_plan(&document)?;
        let directory = self.plan_dir(session_id, plan_id).join("revisions");
        fs::create_dir_all(&directory)?;
        let stem = format!("submitted-{revision:04}");
        write_json_atomically(&directory.join(format!("{stem}.json")), &document)?;
        write_text_atomically(&directory.join(format!("{stem}.md")), &rendered.markdown)?;
        write_json_atomically(
            &directory.join(format!("{stem}.index.json")),
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
                let rendered = render_plan(&document)?;
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
        let store = PlanFileStore::new(temporary.path());
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
                    operations: vec![PlanEditOperation::OverviewUpdate {
                        text: "Edited".into(),
                    }],
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
        assert!(question.questions[0].id.starts_with(&question.id));
        assert!(question.questions[0].allow_freeform);
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
}
