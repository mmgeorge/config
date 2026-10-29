use serde::Deserialize;

/// Identifies an accepted runtime operation before session state is accessed.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
pub enum RuntimeMethod {
    #[serde(rename = "session.command.status.get")]
    SessionCommandStatusGet,
    #[serde(rename = "session.input.apply")]
    SessionInputApply,
    #[serde(rename = "session.input.get")]
    SessionInputGet,
    #[serde(rename = "session.input.operation.get")]
    SessionInputOperationGet,
    #[serde(rename = "session.attachment.import")]
    SessionAttachmentImport,
    #[serde(rename = "session.lsp.authorize")]
    SessionLspAuthorize,
    #[serde(rename = "session.lsp.respond")]
    SessionLspRespond,
    #[serde(rename = "session.lsp.analyze")]
    SessionLspAnalyze,
    #[serde(rename = "session.check.list")]
    SessionCheckList,
    #[serde(rename = "session.check.editing.set")]
    SessionCheckEditingSet,
    #[serde(rename = "session.check.override")]
    SessionCheckOverride,
    #[serde(rename = "session.check.run")]
    SessionCheckRun,
    #[serde(rename = "session.check.complete")]
    SessionCheckComplete,
    #[serde(rename = "session.check.status")]
    SessionCheckStatus,
    #[serde(rename = "session.check.cancel")]
    SessionCheckCancel,
    #[serde(rename = "session.task.get")]
    SessionTaskGet,
    #[serde(rename = "session.command.apply")]
    SessionCommandApply,
    #[serde(rename = "session.command.operation.get")]
    SessionCommandOperationGet,
    #[serde(rename = "session.health.get")]
    SessionHealthGet,
    #[serde(rename = "session.state.get")]
    SessionStateGet,
    #[serde(rename = "session.list")]
    SessionList,
    #[serde(rename = "host.trace.status")]
    HostTraceStatus,
    #[serde(rename = "host.trace.configure")]
    HostTraceConfigure,
    #[serde(rename = "session.trace.clear")]
    SessionTraceClear,
    #[serde(rename = "session.provider.model.list")]
    SessionProviderModelList,
    #[serde(rename = "session.provider.mock_response")]
    SessionProviderMockResponse,
    #[serde(rename = "session.provider.capabilities.get")]
    SessionProviderCapabilitiesGet,
    #[serde(rename = "session.provider.authenticate")]
    SessionProviderAuthenticate,
    #[serde(rename = "session.provider.setup")]
    SessionProviderSetup,
    #[serde(rename = "settings.get")]
    SettingsGet,
    #[serde(rename = "settings.update")]
    SettingsUpdate,
    #[serde(rename = "settings.import.preview")]
    SettingsImportPreview,
    #[serde(rename = "settings.import.apply")]
    SettingsImportApply,
    #[serde(rename = "provider.model.pin.list")]
    ProviderModelPinList,
    #[serde(rename = "provider.model.pin.set")]
    ProviderModelPinSet,
    #[serde(rename = "session.agent.start")]
    SessionAgentStart,
    #[serde(rename = "session.agent.message.send")]
    SessionAgentMessageSend,
    #[serde(rename = "session.agent.followup")]
    SessionAgentFollowup,
    #[serde(rename = "session.agent.interrupt")]
    SessionAgentInterrupt,
    #[serde(rename = "session.agent.interrupt_all")]
    SessionAgentInterruptAll,
    #[serde(rename = "session.agent.model.list")]
    SessionAgentModelList,
    #[serde(rename = "agent.profile.get")]
    AgentProfileGet,
    #[serde(rename = "agent.profile.save")]
    AgentProfileSave,
    #[serde(rename = "permissions.document.open")]
    PermissionsDocumentOpen,
    #[serde(rename = "permissions.document.save")]
    PermissionsDocumentSave,
    #[serde(rename = "session.permission.set")]
    SessionPermissionSet,
    #[serde(rename = "session.prompt.submit")]
    SessionPromptSubmit,
    #[serde(rename = "session.prompt.history.record")]
    SessionPromptHistoryRecord,
    #[serde(rename = "session.plan.accept")]
    SessionPlanAccept,
    #[serde(rename = "session.plan.acceptance.begin")]
    SessionPlanAcceptanceBegin,
    #[serde(rename = "session.plan.acceptance.cancel")]
    SessionPlanAcceptanceCancel,
    #[serde(rename = "session.plan.entity.rename")]
    SessionPlanEntityRename,
    #[serde(rename = "session.plan.test.delete")]
    SessionPlanTestDelete,
    #[serde(rename = "session.plan.check.update")]
    SessionPlanCheckUpdate,
    #[serde(rename = "session.plan.changes.request")]
    SessionPlanChangesRequest,
    #[serde(rename = "session.plan.question.answer")]
    SessionPlanQuestionAnswer,
    #[serde(rename = "session.plan.cancel")]
    SessionPlanCancel,
    #[serde(rename = "session.plan.get")]
    SessionPlanGet,
    #[serde(rename = "session.plan.declaration.get")]
    SessionPlanDeclarationGet,
    #[serde(rename = "session.plan.list")]
    SessionPlanList,
    #[serde(rename = "session.question.answer")]
    SessionQuestionAnswer,
    #[serde(rename = "session.question.skip")]
    SessionQuestionSkip,
    #[serde(rename = "session.question.ask")]
    SessionQuestionAsk,
    #[serde(rename = "session.question.continue")]
    SessionQuestionContinue,
    #[serde(rename = "session.approval.continue")]
    SessionApprovalContinue,
    #[serde(rename = "session.input.timeout.configure")]
    SessionInputTimeoutConfigure,
    #[serde(rename = "session.goal.set")]
    SessionGoalSet,
    #[serde(rename = "session.goal.pause")]
    SessionGoalPause,
    #[serde(rename = "session.goal.resume")]
    SessionGoalResume,
    #[serde(rename = "session.goal.clear")]
    SessionGoalClear,
    #[serde(rename = "session.exchange.list")]
    SessionExchangeList,
    #[serde(rename = "session.exchange.comment.save")]
    SessionExchangeCommentSave,
    #[serde(rename = "session.exchange.rollback.prepare")]
    SessionExchangeRollbackPrepare,
    #[serde(rename = "session.exchange.rollback.apply")]
    SessionExchangeRollbackApply,
    #[serde(rename = "session.exchange.rollback.preview")]
    SessionExchangeRollbackPreview,
    #[serde(rename = "session.exchange.recovery")]
    SessionExchangeRecovery,
    #[serde(rename = "session.new")]
    SessionNew,
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
    #[serde(rename = "host.shutdown")]
    HostShutdown,
    #[serde(rename = "session.turn.cancel")]
    SessionTurnCancel,
    #[serde(rename = "session.turn.restart")]
    SessionTurnRestart,
    #[serde(rename = "session.turn.steer")]
    SessionTurnSteer,
    #[serde(rename = "session.approval.resolve")]
    SessionApprovalResolve,
    #[serde(rename = "session.provider.skill.list")]
    SessionProviderSkillList,
    #[serde(rename = "settings.skill.set_enabled")]
    SettingsSkillSetEnabled,
    #[serde(rename = "session.provider.mcp.list")]
    SessionProviderMcpList,
    #[serde(rename = "settings.mcp.set_enabled")]
    SettingsMcpSetEnabled,
}

impl RuntimeMethod {
    /// Decode an exact wire name and return a fixed rejection message.
    pub fn decode(method: &str) -> anyhow::Result<Self> {
        Self::deserialize(serde::de::value::StrDeserializer::<serde::de::value::Error>::new(method))
            .map_err(|_| anyhow::anyhow!("unknown runtime method"))
    }

    /// Identifies explicit workflow operations whose outcome may settle the selected task.
    pub fn settles_task(self) -> bool {
        matches!(
            self,
            Self::SessionCommandApply
                | Self::SessionPromptSubmit
                | Self::SessionCheckComplete
                | Self::SessionPlanAccept
                | Self::SessionPlanAcceptanceBegin
                | Self::SessionPlanChangesRequest
                | Self::SessionQuestionAnswer
                | Self::SessionQuestionSkip
                | Self::SessionQuestionAsk
                | Self::SessionQuestionContinue
                | Self::SessionApprovalContinue
                | Self::SessionGoalSet
                | Self::SessionGoalPause
                | Self::SessionGoalResume
                | Self::SessionGoalClear
                | Self::SessionTurnCancel
        )
    }

    /// Return whether dispatch must await provider-fork readiness.
    pub fn requires_provider_fork(self) -> bool {
        matches!(
            self,
            Self::SessionPromptSubmit
                | Self::SessionCheckComplete
                | Self::SessionAgentStart
                | Self::SessionPlanQuestionAnswer
                | Self::SessionQuestionAsk
                | Self::SessionQuestionContinue
                | Self::SessionCompact
        )
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn check_editing_has_no_guided_setup_operation() {
        assert_eq!(
            super::RuntimeMethod::decode("session.check.editing.set").unwrap(),
            super::RuntimeMethod::SessionCheckEditingSet
        );
        assert!(super::RuntimeMethod::decode("checks.setup").is_err());
    }

    #[test]
    fn exchange_methods_replace_interaction_names_without_aliases() {
        for (old, renamed) in [
            ("list", "list"),
            ("comment.save", "comment.save"),
            ("rollback.prepare", "rollback.prepare"),
            ("rollback.preview", "rollback.preview"),
            ("rollback.apply", "rollback.apply"),
            ("recovery", "recovery"),
        ] {
            assert!(super::RuntimeMethod::decode(&format!("session.exchange.{renamed}")).is_ok());
            assert!(super::RuntimeMethod::decode(&format!("exchange.{old}")).is_err());
            assert!(super::RuntimeMethod::decode(&format!("interaction.{old}")).is_err());
        }
    }

    #[test]
    fn rejects_independent_child_submission() {
        assert!(super::RuntimeMethod::decode("exchange.rollback").is_err());
        assert!(super::RuntimeMethod::decode("agent.submit").is_err());
    }
}
