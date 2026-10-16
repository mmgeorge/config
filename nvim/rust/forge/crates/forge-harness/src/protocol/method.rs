use serde::Deserialize;

/// Identifies an accepted Harness operation before session state is accessed.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
pub enum HarnessMethod {
    #[serde(rename = "checks.list")] ChecksList,
    #[serde(rename = "checks.setup")] ChecksSetup,
    #[serde(rename = "checks.override")] ChecksOverride,
    #[serde(rename = "checks.run")] ChecksRun,
    #[serde(rename = "checks.status")] ChecksStatus,
    #[serde(rename = "checks.cancel")] ChecksCancel,
    #[serde(rename = "checks.save_defaults")] ChecksSaveDefaults,
    #[serde(rename = "task.list")]
    TaskList,
    #[serde(rename = "task.transition")]
    TaskTransition,
    #[serde(rename = "task.operation")]
    TaskOperation,
    #[serde(rename = "health.get")]
    Health,
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
    #[serde(rename = "trace.session.clear")]
    TraceSessionClear,
    #[serde(rename = "backend.models")]
    BackendModels,
    #[serde(rename = "backend.capabilities")]
    BackendCapabilities,
    #[serde(rename = "backend.authenticate")]
    BackendAuthenticate,
    #[serde(rename = "backend.setup")]
    BackendSetup,
    #[serde(rename = "settings.get")]
    SettingsGet,
    #[serde(rename = "settings.update")]
    SettingsUpdate,
    #[serde(rename = "settings.import.preview")]
    SettingsImportPreview,
    #[serde(rename = "settings.import.apply")]
    SettingsImportApply,
    #[serde(rename = "settings.reset")]
    SettingsReset,
    #[serde(rename = "backend.model_pins")]
    BackendModelPins,
    #[serde(rename = "backend.model_pin")]
    BackendModelPin,
    #[serde(rename = "agent.list")]
    AgentList,
    #[serde(rename = "agent.start")]
    AgentStart,
    #[serde(rename = "agent.message")]
    AgentMessage,
    #[serde(rename = "agent.followup")]
    AgentFollowup,
    #[serde(rename = "agent.interrupt")]
    AgentInterrupt,
    #[serde(rename = "agent.interrupt_all")]
    AgentInterruptAll,
    #[serde(rename = "agent.models")]
    AgentModels,
    #[serde(rename = "agent.profiles.get")]
    AgentProfilesGet,
    #[serde(rename = "agent.profiles.save")]
    AgentProfilesSave,
    #[serde(rename = "permissions.open")]
    PermissionsOpen,
    #[serde(rename = "permissions.save")]
    PermissionsSave,
    #[serde(rename = "session.execution_mode")]
    SessionPermissionMode,
    #[serde(rename = "session.mode")]
    SessionMode,
    #[serde(rename = "exchange.resume")]
    ExchangeResume,
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
    #[serde(rename = "plan.tests.delete")]
    PlanTestsDelete,
    #[serde(rename = "plan.request_changes")]
    PlanRequestChanges,
    #[serde(rename = "plan.questions.answer")]
    PlanAnswerQuestions,
    #[serde(rename = "plan.cancel")]
    PlanCancel,
    #[serde(rename = "plan.get")]
    PlanGet,
    #[serde(rename = "plan.declaration")]
    PlanDeclaration,
    #[serde(rename = "plan.list")]
    PlanList,
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
    #[serde(rename = "approval.continue")]
    ApprovalContinue,
    #[serde(rename = "session.input_timeouts")]
    InputTimeoutConfigure,
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
    #[serde(rename = "exchange.list")]
    ExchangeList,
    #[serde(rename = "exchange.comment.save")]
    ExchangeCommentSave,
    #[serde(rename = "exchange.request_changes")]
    ExchangeRequestChanges,
    #[serde(rename = "exchange.rollback.prepare")]
    ExchangeRollbackPrepare,
    #[serde(rename = "exchange.rollback.apply")]
    ExchangeRollbackApply,
    #[serde(rename = "exchange.rollback.preview")]
    ExchangeRollbackPreview,
    #[serde(rename = "exchange.recovery")]
    ExchangeRecovery,
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

    /// Identifies explicit workflow operations whose outcome may settle the selected task.
    pub fn settles_task(self) -> bool {
        matches!(self,
            Self::TaskTransition | Self::PromptSubmit | Self::ExchangeResume
            | Self::PlanAccept | Self::PlanAcceptanceBegin | Self::PlanRequestChanges
            | Self::QuestionAnswer | Self::QuestionSkip | Self::QuestionAsk | Self::QuestionContinue
            | Self::ApprovalContinue | Self::GoalSet | Self::GoalPause | Self::GoalResume
            | Self::GoalClear | Self::GoalContinue | Self::ExchangeRequestChanges | Self::TurnCancel)
    }

    /// Return whether dispatch must await provider-fork readiness.
    pub fn requires_provider_fork(self) -> bool {
        matches!(
            self,
            Self::PromptSubmit
                | Self::ChecksSetup
                | Self::AgentStart
                | Self::ExchangeResume
                | Self::PlanAccept
                | Self::PlanRequestChanges
                | Self::PlanAnswerQuestions
                | Self::QuestionAsk
                | Self::QuestionContinue
                | Self::GoalSet
                | Self::GoalResume
                | Self::GoalContinue
                | Self::ExchangeRequestChanges
                | Self::SessionCompact
        )
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn exchange_methods_replace_interaction_names_without_aliases() {
        for suffix in [
            "resume",
            "list",
            "comment.save",
            "request_changes",
            "rollback.prepare",
            "rollback.preview",
            "rollback.apply",
            "recovery",
        ] {
            assert!(super::HarnessMethod::decode(&format!("exchange.{suffix}")).is_ok());
            assert!(super::HarnessMethod::decode(&format!("interaction.{suffix}")).is_err());
        }
    }

    #[test]
    fn rejects_independent_child_submission() {
        assert!(super::HarnessMethod::decode("exchange.rollback").is_err());
        assert!(super::HarnessMethod::decode("agent.submit").is_err());
    }
}
