use serde::Deserialize;

/// Identifies an accepted Harness operation before session state is accessed.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
pub enum HarnessMethod {
    #[serde(rename = "state.get")]
    StateGet,
    #[serde(rename = "session.list")]
    SessionList,
    #[serde(rename = "trace.status")]
    TraceStatus,
    #[serde(rename = "trace.configure")]
    TraceConfigure,
    #[serde(rename = "trace.toggle")]
    TraceToggle,
    #[serde(rename = "trace.clear")]
    TraceClear,
    #[serde(rename = "backend.models")]
    BackendModels,
    #[serde(rename = "agent.list")]
    AgentList,
    #[serde(rename = "agent.start")]
    AgentStart,
    #[serde(rename = "agent.submit")]
    AgentSubmit,
    #[serde(rename = "permissions.open")]
    PermissionsOpen,
    #[serde(rename = "permissions.save")]
    PermissionsSave,
    #[serde(rename = "session.execution_mode")]
    SessionExecutionMode,
    #[serde(rename = "session.mode")]
    SessionMode,
    #[serde(rename = "interaction.resume")]
    InteractionResume,
    #[serde(rename = "prompt.submit")]
    PromptSubmit,
    #[serde(rename = "history.record")]
    HistoryRecord,
    #[serde(rename = "queue.edit_last")]
    QueueEditLast,
    #[serde(rename = "plan.accept")]
    PlanAccept,
    #[serde(rename = "plan.acceptance.begin")]
    PlanAcceptanceBegin,
    #[serde(rename = "plan.acceptance.cancel")]
    PlanAcceptanceCancel,
    #[serde(rename = "plan.entity.rename")]
    PlanEntityRename,
    #[serde(rename = "plan.rustdoc.hover")]
    PlanRustdocHover,
    #[serde(rename = "plan.rustdoc.source")]
    PlanRustdocSource,
    #[serde(rename = "plan.request_changes")]
    PlanRequestChanges,
    #[serde(rename = "plan.cancel")]
    PlanCancel,
    #[serde(rename = "plan.activate")]
    PlanActivate,
    #[serde(rename = "plan.scope_deviation_review")]
    PlanScopeDeviationReview,
    #[serde(rename = "plan.deviation.resolve")]
    PlanDeviationResolve,
    #[serde(rename = "question.answer")]
    QuestionAnswer,
    #[serde(rename = "question.skip")]
    QuestionSkip,
    #[serde(rename = "question.ask")]
    QuestionAsk,
    #[serde(rename = "question.continue")]
    QuestionContinue,
    #[serde(rename = "goal.set")]
    GoalSet,
    #[serde(rename = "goal.pause")]
    GoalPause,
    #[serde(rename = "goal.resume")]
    GoalResume,
    #[serde(rename = "goal.clear")]
    GoalClear,
    #[serde(rename = "goal.continue")]
    GoalContinue,
    #[serde(rename = "interaction.list")]
    InteractionList,
    #[serde(rename = "interaction.comment.save")]
    InteractionCommentSave,
    #[serde(rename = "interaction.request_changes")]
    InteractionRequestChanges,
    #[serde(rename = "interaction.rollback")]
    InteractionRollback,
    #[serde(rename = "session.new")]
    SessionNew,
    #[serde(rename = "session.clear")]
    SessionClear,
    #[serde(rename = "session.preview")]
    SessionPreview,
    #[serde(rename = "session.resume")]
    SessionResume,
    #[serde(rename = "session.rename")]
    SessionRename,
    #[serde(rename = "session.configure")]
    SessionConfigure,
    #[serde(rename = "session.compact")]
    SessionCompact,
    #[serde(rename = "session.delete")]
    SessionDelete,
    #[serde(rename = "session.fork")]
    SessionFork,
    #[serde(rename = "shutdown")]
    Shutdown,
    #[serde(rename = "turn.cancel")]
    TurnCancel,
    #[serde(rename = "turn.restart")]
    TurnRestart,
    #[serde(rename = "turn.steer")]
    TurnSteer,
    #[serde(rename = "approval.resolve")]
    ApprovalResolve,
    #[serde(rename = "backend.skills")]
    BackendSkills,
    #[serde(rename = "backend.skills.set_enabled")]
    BackendSkillsSetEnabled,
    #[serde(rename = "backend.mcp")]
    BackendMcp,
    #[serde(rename = "backend.mcp.set_enabled")]
    BackendMcpSetEnabled,
}

impl HarnessMethod {
    /// Decode an exact wire name and return a fixed rejection message.
    pub fn decode(method: &str) -> anyhow::Result<Self> {
        Self::deserialize(serde::de::value::StrDeserializer::<serde::de::value::Error>::new(method))
            .map_err(|_| anyhow::anyhow!("unknown Harness broker method"))
    }

    /// Return whether dispatch must await provider-fork readiness.
    pub fn requires_provider_fork(self) -> bool {
        matches!(
            self,
            Self::PromptSubmit
                | Self::AgentStart
                | Self::AgentSubmit
                | Self::InteractionResume
                | Self::PlanAccept
                | Self::PlanRequestChanges
                | Self::QuestionAsk
                | Self::QuestionContinue
                | Self::GoalSet
                | Self::GoalResume
                | Self::GoalContinue
                | Self::InteractionRequestChanges
                | Self::SessionCompact
        )
    }
}
