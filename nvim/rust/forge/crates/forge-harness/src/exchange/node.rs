use crate::agent::Delegation;
use crate::plan::PlanAnnotation;
use serde::{Deserialize, Serialize};

/// Represents one durable item at a fixed position in an interaction timeline.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ExchangeNode {
    /// Keeps an owned check or session event at its captured source boundary.
    TimelineEvent {
        event: Box<crate::timeline::TimelineEvent>,
    },
    /// Records one successful selected-skill read per provider turn.
    SkillRead {
        id: String,
        turn_id: String,
        read: crate::skill::SkillRead,
    },
    /// Retains provider context replacement separately from model text and tool calls.
    Compaction {
        id: String,
        compaction: Box<crate::provider::Compaction>,
    },
    ImplementationReport {
        id: String,
        reference: crate::plan::implementation_report::ImplementationReportRef,
        #[serde(skip)]
        report: Option<std::sync::Arc<crate::plan::implementation_report::ImplementationReport>>,
    },
    VerificationResult {
        id: String,
        result: Box<VerificationResult>,
    },
    QuestionPresented {
        id: String,
        question: crate::plan::PlanQuestionSet,
        answer: Option<String>,
    },
    QuestionWithdrawn {
        id: String,
        question: crate::plan::PlanQuestionSet,
        reason: String,
        #[serde(default)]
        answer: Vec<crate::plan::PlanQuestionAnswer>,
    },
    PlanEvent {
        event: Box<crate::plan::ExchangePlanEvent>,
    },
    TurnContent {
        id: String,
        turn_id: String,
        item: crate::turn::TurnItem,
    },
    AgentReference {
        agent: Delegation,
    },
    ExchangeInput {
        prompt: ExchangeInput,
    },
    PlanCommentResolution {
        resolution: PlanCommentResolution,
    },
    ArtifactChange {
        change: ArtifactChange,
    },
}

impl ExchangeNode {
    pub fn id(&self) -> &str {
        match self {
            Self::TimelineEvent { event } => event.id(),
            Self::SkillRead { id, .. } => id,
            Self::Compaction { id, .. } => id,
            Self::ImplementationReport { id, .. } => id,
            Self::VerificationResult { id, .. } => id,
            Self::QuestionPresented { id, .. } => id,
            Self::QuestionWithdrawn { id, .. } => id,
            Self::PlanEvent { event } => &event.id,
            Self::TurnContent { id, .. } => id,
            Self::AgentReference { agent } => &agent.id,
            Self::ExchangeInput { prompt } => &prompt.id,
            Self::PlanCommentResolution { resolution } => &resolution.id,
            Self::ArtifactChange { change } => &change.id,
        }
    }
}

/// Represents inline review comments resolved by one submitted plan revision.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanCommentResolution {
    pub id: String,
    pub annotation: Vec<PlanAnnotation>,
    pub created_at_ms: i64,
}

/// Represents one canonical artifact delta produced by an interaction.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ArtifactChange {
    pub id: String,
    pub path: String,
    pub diff_text: String,
    pub created_at_ms: i64,
    #[serde(default)]
    pub revision_request: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub declaration: Option<DeclarationRevision>,
}

/// Owns published verification evidence independently of the current execution task.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct VerificationResult {
    pub status: crate::plan::execution::VerificationStatus,
    /// Holds the summary the agent passed to `plan_phase_done`.
    pub phase_summary: Option<String>,
    #[serde(default)]
    pub findings: Vec<String>,
    pub report: Option<crate::plan::execution::VerificationReport>,
    #[serde(default)]
    pub declaration_findings: Vec<String>,
    #[serde(default)]
    pub declaration_changes: Vec<crate::plan::execution::DeclarationMismatch>,
    /// Maps each cited evidence ID to the tool call that produced it.
    #[serde(default)]
    pub evidence_sources: std::collections::BTreeMap<String, crate::turn::ToolAddress>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Identifies immutable declaration revisions and their separate task and description delta.
pub struct DeclarationRevision {
    pub plan_id: String,
    pub revision: u32,
    pub document_diff: String,
    /// Selects newly captured source baselines instead of the preceding proposal.
    #[serde(default, skip_serializing_if = "std::collections::BTreeSet::is_empty")]
    pub baseline_paths: std::collections::BTreeSet<String>,
}

/// Distinguishes user input that extends an existing exchange.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputIntent {
    Steering,
    /// Preserve structured question content independently of its active-turn delivery route.
    QuestionResponse {
        responses: Vec<forge_provider_api::QuestionReply>,
    },
    Clarification,
    Answer,
}

/// Retains acknowledged user input in exchange order.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExchangeInput {
    #[serde(default)]
    pub question: Option<QuestionInput>,
    pub intent: InputIntent,
    pub id: String,
    pub text: String,
    pub created_at_ms: i64,
    /// The provider submission that delivered this input, which its native echo carries.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub submission_id: Option<forge_provider_api::SubmissionId>,
    /// What the provider has reported about this input joining its running turn.
    #[serde(default, skip_serializing_if = "InputConsumption::is_consumed")]
    pub consumption: InputConsumption,
}

impl ExchangeInput {
    /// Report whether this input waits in the status area for its provider's consumption report.
    pub fn is_awaiting_consumption(&self) -> bool {
        matches!(
            self.consumption,
            InputConsumption::AwaitingConsumption { .. }
        )
    }
}

/// Distinguishes acknowledged provider steering from steering the model has received.
///
/// A provider acknowledges steering when it owns the message, which can precede the model call
/// that reads it by the remainder of the current call. Only positive evidence that the input
/// joined processing, such as its native user item, establishes consumption; acceptance and a
/// turn's covered inputs do not.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputConsumption {
    /// The provider reported the input inside its turn, or the input needed no report.
    #[default]
    Consumed,
    /// The provider acknowledged receipt under this invocation and has not reported consumption.
    AwaitingConsumption {
        invocation: forge_provider_api::InvocationScope,
    },
    /// The exchange settled, or its provider can no longer report, without a consumption report.
    Unconfirmed,
}

impl InputConsumption {
    pub fn is_consumed(&self) -> bool {
        matches!(self, Self::Consumed)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains the question identity and submitted decisions independently of picker focus.
pub struct QuestionInput {
    pub set_id: String,
    pub question_id: Option<String>,
    #[serde(default)]
    pub answer: Vec<crate::plan::PlanQuestionAnswer>,
}

/// Tracks transient waiting chrome without adding it to durable history.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveWait {
    pub exchange_id: String,
    pub started_at_ms: i64,
    pub agent_count: usize,
}
