use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::PathBuf;

mod prompt;

pub use prompt::PlanPrompt;

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
    pub model_revision: u32,
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
}

/// Tracks one accepted plan through its guarded execution goal.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanExecutionRecord {
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub goal_id: String,
    pub state: PlanExecutionState,
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

    /// Write a model revision and refresh the editable working copy.
    pub fn write_model_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
        markdown: &str,
    ) -> Result<(PathBuf, String)> {
        let directory = self.plan_dir(session_id, plan_id);
        fs::create_dir_all(directory.join("revisions"))
            .with_context(|| format!("create plan directory {}", directory.display()))?;
        let revision_path = directory
            .join("revisions")
            .join(format!("model-{revision:04}.md"));
        fs::write(&revision_path, markdown)
            .with_context(|| format!("write model plan revision {}", revision_path.display()))?;
        let working_path = directory.join("working.md");
        fs::write(&working_path, markdown)
            .with_context(|| format!("write working plan {}", working_path.display()))?;
        Ok((working_path, digest(markdown.as_bytes())))
    }

    /// Save the complete edited plan as an immutable user revision.
    pub fn save_user_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<(String, String)> {
        let directory = self.plan_dir(session_id, plan_id);
        let working_path = directory.join("working.md");
        let markdown = fs::read_to_string(&working_path)
            .with_context(|| format!("read working plan {}", working_path.display()))?;
        let revision_path = directory
            .join("revisions")
            .join(format!("user-{revision:04}.md"));
        fs::write(&revision_path, &markdown)
            .with_context(|| format!("write user plan revision {}", revision_path.display()))?;
        Ok((markdown.clone(), digest(markdown.as_bytes())))
    }

    /// Read and hash the editable working plan before accepting it.
    pub fn read_working(&self, session_id: &str, plan_id: &str) -> Result<(String, String)> {
        let working_path = self.plan_dir(session_id, plan_id).join("working.md");
        let markdown = fs::read_to_string(&working_path)
            .with_context(|| format!("read working plan {}", working_path.display()))?;
        let checksum = digest(markdown.as_bytes());
        Ok((markdown, checksum))
    }

    /// Read one immutable model revision for timeline expansion.
    pub fn read_model_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<String> {
        let path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("model-{revision:04}.md"));
        fs::read_to_string(&path)
            .with_context(|| format!("read model plan revision {}", path.display()))
    }

    /// Read one immutable user revision for timeline expansion.
    pub fn read_user_revision(
        &self,
        session_id: &str,
        plan_id: &str,
        revision: u32,
    ) -> Result<String> {
        let path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("user-{revision:04}.md"));
        fs::read_to_string(&path)
            .with_context(|| format!("read user plan revision {}", path.display()))
    }

    /// Build the user-edit diff against the last immutable model revision.
    pub fn user_edit_diff(
        &self,
        session_id: &str,
        plan_id: &str,
        model_revision: u32,
        edited: &str,
    ) -> Result<String> {
        let revision_path = self
            .plan_dir(session_id, plan_id)
            .join("revisions")
            .join(format!("model-{model_revision:04}.md"));
        let model = fs::read_to_string(&revision_path)
            .with_context(|| format!("read model plan revision {}", revision_path.display()))?;
        Ok(similar::TextDiff::from_lines(&model, edited)
            .unified_diff()
            .context_radius(3)
            .header("model-plan.md", "user-edited-plan.md")
            .to_string())
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
        fs::copy(source.join("working.md"), target.join("working.md"))?;
        let source_revision = source.join("revisions");
        if source_revision.exists() {
            for entry in fs::read_dir(&source_revision)? {
                let entry = entry?;
                if entry.file_type()?.is_file() {
                    fs::copy(
                        entry.path(),
                        target.join("revisions").join(entry.file_name()),
                    )?;
                }
            }
        }
        Ok(target.join("working.md"))
    }

    fn plan_dir(&self, session_id: &str, plan_id: &str) -> PathBuf {
        self.root.join("plans").join(session_id).join(plan_id)
    }
}

/// Resolve a stable content digest for immutable plan acceptance.
pub fn digest(content: &[u8]) -> String {
    hex::encode(Sha256::digest(content))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn preserves_model_and_user_revisions() {
        let temporary = tempfile::tempdir().unwrap();
        let store = PlanFileStore::new(temporary.path());
        let (working, first_digest) = store
            .write_model_revision("session", "plan", 1, "# Plan\n\nInitial")
            .unwrap();
        fs::write(&working, "# Plan\n\nEdited").unwrap();
        let (edited, edited_digest) = store.save_user_revision("session", "plan", 1).unwrap();
        assert_eq!(edited, "# Plan\n\nEdited");
        assert_ne!(first_digest, edited_digest);
        assert!(
            store
                .user_edit_diff("session", "plan", 1, &edited)
                .unwrap()
                .contains("Edited")
        );
        assert!(
            temporary
                .path()
                .join("plans/session/plan/revisions/model-0001.md")
                .exists()
        );
        assert!(
            temporary
                .path()
                .join("plans/session/plan/revisions/user-0001.md")
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
}
