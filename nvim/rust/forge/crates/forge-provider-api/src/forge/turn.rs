//! One input's Harness content: why the turn happens and the elements that reason implies.
//!
//! A [`TurnInput`] holds exactly one [`TurnKind`]. The kind decides which elements render and
//! which fixed `<instructions>` accompany them, so callers never write instruction text and cannot
//! combine elements that do not belong together. User text exists only in the kinds where the
//! user typed something, and it is never wrapped.

use serde::{Deserialize, Serialize};

use super::{instructions, writer::Writer};
use crate::input::{PromptInput, QuestionReply, QuestionReplyResponse};

/// Harness content of one logical input.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TurnInput {
    /// Boxed so the many queues and records that carry an input stay small.
    kind: Box<TurnKind>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    notices: Vec<Notice>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    child_results: Vec<AgentResult>,
}

/// Why one turn happens.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TurnKind {
    /// The user typed a message, including the first planning request and planning feedback.
    UserMessage { input: PromptInput },
    /// The user typed while a submitted plan awaits review.
    PlanDiscussion { input: PromptInput },
    /// The user asked for a plan, but the project's checks must be configured first.
    ChecksSetup { input: PromptInput },
    /// The user typed in chat while questions are still open.
    PendingQuestions {
        input: PromptInput,
        questions: PendingQuestions,
        scope: PendingScope,
    },
    /// The user asked about the plan in a forked review session.
    PlanReviewQuestion { input: PromptInput },
    /// Questions were answered, and the original request without a task continues.
    QuestionsResolved { original: PromptInput },
    /// The user responded to one question in the question UI.
    QuestionResponse { response: QuestionReply },
    /// Recorded planning answers resume planning.
    PlanFeedback { feedback: PlanFeedback },
    /// Recorded answers continue a task's original request.
    WorkflowAnswers { answers: ResolvedQuestions },
    /// The user decided on a submitted plan.
    PlanReview { review: PlanReview },
    /// Saved checks block task completion.
    CheckRepair { repair: CheckRepair },
    /// Settled child results continue the parent's unfinished request.
    ChildResults,
    /// A parent assigned a child agent one task.
    AgentTask { task: AgentTask },
    /// Another session, such as an aside, sent this session a message.
    SessionMessage { message: SessionMessage },
    /// An approval request timed out while no task could continue.
    PermissionTimeout { feedback: String },
    /// Harness starts or resumes a task step on its own.
    TaskStep { step: TaskStep },
    /// A Background generation request: plain text, outside the `<forge>` protocol.
    Background { input: PromptInput },
}

/// Harness-started task steps, each with fixed instruction text.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "step", rename_all = "snake_case")]
pub enum TaskStep {
    ExecuteStart,
    ExecuteContinue,
    ExecuteResume,
    GoalStart,
    GoalContinue,
    GoalResume,
    PlanResume,
    PlanContinue {
        turn: u32,
        max_turns: u32,
    },
    ChecksSaved {
        revision: String,
    },
    AnswersResolved,
    ContinueRequest,
    /// Continue accepted work after a transient provider failure ended its execution.
    RetryContinue,
}

/// Which workflow a pending-question discussion belongs to.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PendingScope {
    /// The questions belong to a Plan task, which must not continue during the discussion.
    Plan,
    /// The questions belong to an ordinary request, which must not continue during the discussion.
    Conversation,
}

/// Open questions shown while the user discusses them instead of answering.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PendingQuestions {
    pub questions: Vec<PendingQuestion>,
}

/// One open question with its choices and any answer already recorded.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PendingQuestion {
    pub id: String,
    pub header: String,
    pub question: String,
    pub options: Vec<QuestionOption>,
    pub allow_freeform: bool,
    /// The response recorded so far, if any.
    pub answer: Option<String>,
}

/// One choice offered for a question.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct QuestionOption {
    pub label: String,
    pub description: String,
    pub recommended: bool,
}

/// Answers Harness recorded for a question set.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ResolvedQuestions {
    answers: Vec<RecordedAnswer>,
    timed_out: bool,
}

impl ResolvedQuestions {
    /// Build a non-empty answer set; `timed_out` marks a set that an elapsed deadline submitted.
    pub fn new(answers: Vec<RecordedAnswer>, timed_out: bool) -> Option<Self> {
        (!answers.is_empty()).then_some(Self { answers, timed_out })
    }

    /// Report whether an elapsed deadline submitted the set.
    pub fn timed_out(&self) -> bool {
        self.timed_out
    }
}

/// One recorded answer.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct RecordedAnswer {
    pub question: String,
    pub answer: AnswerValue,
    /// Whether an elapsed deadline, not the user, supplied this answer.
    pub automatic: bool,
}

/// The value of one recorded answer.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AnswerValue {
    Selected {
        option: String,
        feedback: Option<String>,
    },
    Other {
        text: String,
    },
    Skipped,
}

/// Planning input that resumes a paused plan.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PlanFeedback {
    /// The user answered the planning questions.
    Answers { answers: ResolvedQuestions },
    /// The model withdrew its questions.
    Withdrawn { reason: String },
}

/// The user's decision on a submitted plan.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "decision", rename_all = "snake_case")]
pub enum PlanReview {
    Accepted {
        #[serde(default)]
        edits: Vec<PlanEdit>,
    },
    ChangesRequested {
        feedback: ReviewFeedback,
        #[serde(default)]
        edits: Vec<PlanEdit>,
    },
}

/// A change request's overall comment and annotated review excerpt.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ReviewFeedback {
    overall: Option<String>,
    comments: String,
}

impl ReviewFeedback {
    /// Build feedback with an overall comment, review comments, or both.
    pub fn new(overall: Option<String>, comments: String) -> Option<Self> {
        let overall = overall.filter(|text| !text.trim().is_empty());
        (overall.is_some() || !comments.trim().is_empty()).then_some(Self { overall, comments })
    }
}

/// One edit the user applied with the review tools.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PlanEdit {
    Rename { from: String, to: String },
    RemoveTest { file: String, name: String },
    SetCheck { id: String, enabled: bool },
}

impl std::fmt::Display for PlanEdit {
    /// Describe the edit in one line for transcripts and tool responses.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rename { from, to } => write!(formatter, "Renamed {from} to {to}"),
            Self::RemoveTest { file, name } => {
                write!(formatter, "Removed planned test {file}::{name}")
            }
            Self::SetCheck { id, enabled } => write!(
                formatter,
                "{} plan check {id}",
                if *enabled { "Enabled" } else { "Disabled" }
            ),
        }
    }
}

/// Failing checks that block task completion.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckRepair {
    failures: Vec<CheckFailure>,
}

impl CheckRepair {
    /// Build a repair for at least one failure.
    pub fn new(failures: Vec<CheckFailure>) -> Option<Self> {
        (!failures.is_empty()).then_some(Self { failures })
    }
}

/// One failing check command.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckFailure {
    /// The command that failed.
    pub check: String,
    pub state: String,
    pub exit_code: Option<i32>,
    pub error: Option<String>,
    /// Path of the file holding the command's complete output.
    pub log: String,
}

/// A finished child agent's report, rendered as `<agent_message type="final_answer">`.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AgentResult {
    /// Path of the child that answered.
    pub agent: String,
    pub task_id: String,
    pub status: String,
    pub response: Option<String>,
    pub error: Option<String>,
}

/// One task a parent assigned a child agent, with information the parent queued for it before
/// the task started.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AgentTask {
    pub task_id: String,
    /// Path of the parent that assigned the task.
    pub from: String,
    pub text: String,
    /// Messages the parent sent before the task started, in arrival order.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub messages: Vec<AgentMessage>,
}

/// Information one agent sent another about a task.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AgentMessage {
    /// Path of the sending agent.
    pub from: String,
    pub text: String,
}

/// A message another session sent this one.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SessionMessage {
    pub from_session_id: String,
    /// The sender's display name; empty when it has none.
    pub from_session_name: String,
    pub text: String,
}

impl SessionMessage {
    /// Name the sender for transcripts: its display name with its ID, or its ID alone.
    fn sender(&self) -> String {
        if self.from_session_name.is_empty() {
            self.from_session_id.clone()
        } else {
            format!("\"{}\" ({})", self.from_session_name, self.from_session_id)
        }
    }
}

/// A side condition any turn can carry.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Notice {
    /// This turn starts a task; `label` names its kind, such as `Plan` or `Goal`.
    TaskStarted { label: String, title: String },
    /// The user resumed the task with a message of their own; the step says how to resume.
    TaskResumed { step: TaskStep },
    /// Approval requests timed out during the previous turn.
    PermissionTimeout { feedback: String },
    /// The provider removed an interrupted tool-call suffix from its history.
    HistoryRepaired,
}

impl TurnInput {
    /// Build input for one turn kind.
    pub fn new(kind: TurnKind) -> Self {
        debug_assert!(
            !matches!(kind, TurnKind::ChildResults),
            "child continuations are built with child_continuation"
        );
        Self {
            kind: Box::new(kind),
            notices: Vec::new(),
            child_results: Vec::new(),
        }
    }

    /// Build ordinary user input.
    pub fn user(input: PromptInput) -> Self {
        Self::new(TurnKind::UserMessage { input })
    }

    /// Build a parent continuation for at least one settled child result.
    pub fn child_continuation(results: Vec<AgentResult>) -> Option<Self> {
        (!results.is_empty()).then_some(Self {
            kind: Box::new(TurnKind::ChildResults),
            notices: Vec::new(),
            child_results: results,
        })
    }

    /// Get the turn kind.
    pub fn kind(&self) -> &TurnKind {
        &self.kind
    }

    /// Get the side conditions this turn carries.
    pub fn notices(&self) -> &[Notice] {
        &self.notices
    }

    /// Get the child results this turn reports.
    pub fn child_results(&self) -> &[AgentResult] {
        &self.child_results
    }

    /// Add a side condition.
    pub fn push_notice(&mut self, notice: Notice) {
        if !self.notices.contains(&notice) {
            self.notices.push(notice);
        }
    }

    /// Report child results with this turn.
    pub fn push_child_results(&mut self, results: impl IntoIterator<Item = AgentResult>) {
        self.child_results.extend(results);
    }

    /// Get the user's own input, which adapters send unwrapped after the turn's blocks.
    pub fn user_input(&self) -> Option<&PromptInput> {
        match self.kind.as_ref() {
            TurnKind::UserMessage { input }
            | TurnKind::PlanDiscussion { input }
            | TurnKind::ChecksSetup { input }
            | TurnKind::PendingQuestions { input, .. }
            | TurnKind::PlanReviewQuestion { input }
            | TurnKind::Background { input }
            | TurnKind::QuestionsResolved { original: input } => Some(input),
            TurnKind::QuestionResponse { .. }
            | TurnKind::PlanFeedback { .. }
            | TurnKind::WorkflowAnswers { .. }
            | TurnKind::PlanReview { .. }
            | TurnKind::CheckRepair { .. }
            | TurnKind::ChildResults
            | TurnKind::AgentTask { .. }
            | TurnKind::SessionMessage { .. }
            | TurnKind::PermissionTimeout { .. }
            | TurnKind::TaskStep { .. } => None,
        }
    }

    /// Replace the user's own input, keeping the kind; used to neutralize user text.
    pub fn map_user_input(&mut self, map: impl FnOnce(PromptInput) -> PromptInput) {
        let slot = match self.kind.as_mut() {
            TurnKind::UserMessage { input }
            | TurnKind::PlanDiscussion { input }
            | TurnKind::ChecksSetup { input }
            | TurnKind::PendingQuestions { input, .. }
            | TurnKind::PlanReviewQuestion { input }
            | TurnKind::Background { input }
            | TurnKind::QuestionsResolved { original: input } => input,
            _ => return,
        };
        let input = std::mem::replace(slot, PromptInput::Text(String::new()));
        *slot = map(input);
    }

    /// Report whether this is a Background request, which renders outside the protocol.
    pub fn is_background(&self) -> bool {
        matches!(*self.kind, TurnKind::Background { .. })
    }

    /// Report whether this turn answers a question.
    pub fn is_question_response(&self) -> bool {
        matches!(*self.kind, TurnKind::QuestionResponse { .. })
    }

    /// Describe the turn for transcripts and logs: the user's text, or a short label.
    pub fn label(&self) -> String {
        match self.kind.as_ref() {
            TurnKind::UserMessage { input }
            | TurnKind::PlanDiscussion { input }
            | TurnKind::ChecksSetup { input }
            | TurnKind::PendingQuestions { input, .. }
            | TurnKind::PlanReviewQuestion { input }
            | TurnKind::Background { input } => input.display_text(),
            TurnKind::QuestionsResolved { .. } => "Questions resolved".into(),
            TurnKind::QuestionResponse { response } => match &response.response {
                QuestionReplyResponse::Answer { answer, .. }
                | QuestionReplyResponse::Correction { answer, .. } => answer.clone(),
                QuestionReplyResponse::Clarification { text, .. } => text.clone(),
            },
            TurnKind::PlanFeedback { .. } => "Planning answers recorded".into(),
            TurnKind::WorkflowAnswers { .. } => "Questions resolved".into(),
            TurnKind::PlanReview {
                review: PlanReview::Accepted { .. },
            } => "Plan accepted".into(),
            TurnKind::PlanReview {
                review: PlanReview::ChangesRequested { .. },
            } => "Revision requested".into(),
            TurnKind::CheckRepair { .. } => "Automatic check repair".into(),
            TurnKind::ChildResults => "Child results".into(),
            TurnKind::AgentTask { task } => task.text.clone(),
            TurnKind::SessionMessage { message } => format!(
                "Message from aside session {}:\n\n{}",
                message.sender(),
                message.text
            ),
            TurnKind::PermissionTimeout { .. } => "An approval timed out".into(),
            TurnKind::TaskStep { step } => match step {
                TaskStep::ExecuteStart => "Execute started",
                TaskStep::ExecuteContinue => "Execute continued",
                TaskStep::ExecuteResume => "Execute resumed",
                TaskStep::GoalStart => "Goal started",
                TaskStep::GoalContinue => "Goal continued",
                TaskStep::GoalResume => "Goal resumed",
                TaskStep::PlanResume => "Planning resumed",
                TaskStep::PlanContinue { .. } => "Planning continued",
                TaskStep::ChecksSaved { .. } => "Project checks saved",
                TaskStep::AnswersResolved => "Questions resolved",
                TaskStep::ContinueRequest => "Request continued",
                TaskStep::RetryContinue => "Request retried",
            }
            .into(),
        }
    }

    /// Write the elements that carry instruction authority: `<instructions>` only.
    pub(crate) fn write_instructions(&self, writer: &mut Writer) {
        let mut parts = Vec::new();
        if let Some(text) = instructions::for_kind(&self.kind) {
            parts.push(text);
        }
        for notice in &self.notices {
            parts.push(instructions::for_notice(notice));
        }
        writer.element("instructions", &[], &parts.join("\n\n"));
    }

    /// Write the elements that report what the user said or what happened.
    pub(crate) fn write_data(&self, writer: &mut Writer) {
        match self.kind.as_ref() {
            TurnKind::PendingQuestions { questions, .. } => questions.write(writer),
            TurnKind::QuestionResponse { response } => write_question_response(response, writer),
            TurnKind::PlanFeedback { feedback } => match feedback {
                PlanFeedback::Answers { answers } => answers.write(writer),
                PlanFeedback::Withdrawn { reason } => {
                    writer.element("questions_withdrawn", &[], reason)
                }
            },
            TurnKind::WorkflowAnswers { answers } => answers.write(writer),
            TurnKind::PlanReview { review } => review.write(writer),
            TurnKind::CheckRepair { repair } => repair.write(writer),
            TurnKind::AgentTask { task } => task.write(writer),
            TurnKind::SessionMessage { message } => message.write(writer),
            TurnKind::UserMessage { .. }
            | TurnKind::PlanDiscussion { .. }
            | TurnKind::ChecksSetup { .. }
            | TurnKind::PlanReviewQuestion { .. }
            | TurnKind::QuestionsResolved { .. }
            | TurnKind::ChildResults
            | TurnKind::PermissionTimeout { .. }
            | TurnKind::TaskStep { .. }
            | TurnKind::Background { .. } => {}
        }
        for result in &self.child_results {
            result.write(writer);
        }
    }
}

impl From<String> for TurnInput {
    /// Build ordinary user text input.
    fn from(text: String) -> Self {
        Self::user(PromptInput::Text(text))
    }
}

impl From<&str> for TurnInput {
    /// Build ordinary user text input.
    fn from(text: &str) -> Self {
        Self::user(PromptInput::Text(text.to_owned()))
    }
}

impl PendingQuestions {
    fn write(&self, writer: &mut Writer) {
        writer.open("pending_questions", &[]);
        for question in &self.questions {
            let mut attributes = vec![
                ("id", question.id.clone()),
                ("header", question.header.clone()),
            ];
            if !question.allow_freeform {
                attributes.push(("freeform", "false".into()));
            }
            writer.open("question", &attributes);
            writer.text(&question.question);
            for option in &question.options {
                let mut attributes = vec![("label", option.label.clone())];
                if option.recommended {
                    attributes.push(("recommended", "true".into()));
                }
                if option.description.trim().is_empty() {
                    writer.empty("option", &attributes);
                } else {
                    writer.element("option", &attributes, &option.description);
                }
            }
            if let Some(answer) = &question.answer {
                writer.element("answer", &[], answer);
            }
            writer.close();
        }
        writer.close();
    }
}

impl ResolvedQuestions {
    fn write(&self, writer: &mut Writer) {
        let attributes = if self.timed_out {
            vec![("timed_out", "true".to_owned())]
        } else {
            Vec::new()
        };
        writer.open("recorded_answers", &attributes);
        for answer in &self.answers {
            let mut attributes = vec![("question", answer.question.clone())];
            if answer.automatic {
                attributes.push(("source", "timeout".into()));
            }
            match &answer.answer {
                AnswerValue::Selected { option, feedback } => {
                    attributes.push(("option", option.clone()));
                    match feedback.as_deref().filter(|text| !text.trim().is_empty()) {
                        Some(feedback) => writer.element("answer", &attributes, feedback),
                        None => writer.empty("answer", &attributes),
                    }
                }
                AnswerValue::Other { text } => writer.element("answer", &attributes, text),
                AnswerValue::Skipped => {
                    attributes.push(("skipped", "true".into()));
                    writer.empty("answer", &attributes);
                }
            }
        }
        writer.close();
    }
}

impl PlanReview {
    fn write(&self, writer: &mut Writer) {
        let (decision, feedback, edits) = match self {
            Self::Accepted { edits } => ("accepted", None, edits),
            Self::ChangesRequested { feedback, edits } => {
                ("changes_requested", Some(feedback), edits)
            }
        };
        writer.open("plan_review", &[("decision", decision.into())]);
        if let Some(feedback) = feedback {
            if let Some(overall) = &feedback.overall {
                writer.element("overall", &[], overall);
            }
            writer.element("comments", &[], &feedback.comments);
        }
        if !edits.is_empty() {
            writer.open("plan_edits", &[]);
            for edit in edits {
                match edit {
                    PlanEdit::Rename { from, to } => {
                        writer.empty("rename", &[("from", from.clone()), ("to", to.clone())])
                    }
                    PlanEdit::RemoveTest { file, name } => writer.empty(
                        "remove_test",
                        &[("file", file.clone()), ("name", name.clone())],
                    ),
                    PlanEdit::SetCheck { id, enabled } => writer.empty(
                        "set_check",
                        &[("id", id.clone()), ("enabled", enabled.to_string())],
                    ),
                }
            }
            writer.close();
        }
        writer.close();
    }

    /// Report whether the user applied review edits.
    pub fn has_edits(&self) -> bool {
        match self {
            Self::Accepted { edits } | Self::ChangesRequested { edits, .. } => !edits.is_empty(),
        }
    }
}

impl CheckRepair {
    fn write(&self, writer: &mut Writer) {
        writer.open("check_failures", &[]);
        for failure in &self.failures {
            let mut attributes = vec![
                ("check", failure.check.clone()),
                ("state", failure.state.clone()),
            ];
            if let Some(code) = failure.exit_code {
                attributes.push(("exit_code", code.to_string()));
            }
            attributes.push(("log", failure.log.clone()));
            match failure
                .error
                .as_deref()
                .filter(|error| !error.trim().is_empty())
            {
                Some(error) => writer.element("failure", &attributes, error),
                None => writer.empty("failure", &attributes),
            }
        }
        writer.close();
    }
}

/// Element that carries text one agent wrote to another.
const AGENT_MESSAGE: &str = "agent_message";

impl AgentResult {
    fn write(&self, writer: &mut Writer) {
        writer.open(
            AGENT_MESSAGE,
            &[
                ("type", "final_answer".into()),
                ("from", self.agent.clone()),
                ("task", self.task_id.clone()),
                ("status", self.status.clone()),
            ],
        );
        if let Some(response) = self.response.as_deref() {
            writer.text(response);
        }
        if let Some(error) = self.error.as_deref() {
            writer.element("error", &[], error);
        }
        writer.close();
    }
}

impl AgentTask {
    fn write(&self, writer: &mut Writer) {
        writer.element(
            AGENT_MESSAGE,
            &[
                ("type", "new_task".into()),
                ("from", self.from.clone()),
                ("task", self.task_id.clone()),
            ],
            &self.text,
        );
        for message in &self.messages {
            message.write(writer);
        }
    }
}

impl AgentMessage {
    fn write(&self, writer: &mut Writer) {
        writer.element(
            AGENT_MESSAGE,
            &[("type", "message".into()), ("from", self.from.clone())],
            &self.text,
        );
    }
}

impl SessionMessage {
    fn write(&self, writer: &mut Writer) {
        let mut attributes = vec![("from_session_id", self.from_session_id.clone())];
        if !self.from_session_name.is_empty() {
            attributes.push(("from_session_name", self.from_session_name.clone()));
        }
        writer.element("session_message", &attributes, &self.text);
    }
}

/// Write one `<question_response>`.
fn write_question_response(reply: &QuestionReply, writer: &mut Writer) {
    let kind = match &reply.response {
        QuestionReplyResponse::Answer { .. } => "answer",
        QuestionReplyResponse::Correction { .. } => "correction",
        QuestionReplyResponse::Clarification { .. } => "clarification",
    };
    writer.open(
        "question_response",
        &[
            ("id", reply.question_id.clone()),
            ("revision", reply.revision.to_string()),
            ("kind", kind.into()),
        ],
    );
    writer.element("question", &[], &reply.question);
    match &reply.response {
        QuestionReplyResponse::Answer { answer, .. }
        | QuestionReplyResponse::Correction { answer, .. } => {
            writer.element("answer", &[], answer);
        }
        QuestionReplyResponse::Clarification { options, text, .. } => {
            writer.open("options", &[]);
            for option in options {
                let mut attributes =
                    vec![("id", option.id.clone()), ("label", option.label.clone())];
                if option.recommended {
                    attributes.push(("recommended", "true".into()));
                }
                if option.description.trim().is_empty() {
                    writer.empty("option", &attributes);
                } else {
                    writer.element("option", &attributes, &option.description);
                }
            }
            writer.close();
            writer.element("clarification", &[], text);
        }
    }
    writer.close();
}
