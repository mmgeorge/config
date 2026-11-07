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
    #[serde(rename = "session.check.complete")]
    SessionCheckComplete,
    #[serde(rename = "session.check.status")]
    SessionCheckStatus,
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
    #[serde(rename = "provider.registry.list")]
    ProviderRegistryList,
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
    #[serde(rename = "session.question.continue")]
    SessionQuestionContinue,
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
        if matches!(
            method,
            "session.plan.acceptance.begin" | "session.plan.acceptance.cancel"
        ) {
            anyhow::bail!("unsupported runtime method: retired plan acceptance lifecycle");
        }
        if method == "session.approval.continue" {
            anyhow::bail!(
                "unsupported runtime method: approval timeout feedback is owned internally"
            );
        }
        if method == "session.turn.restart" {
            anyhow::bail!(
                "unsupported runtime method: restart is owned by configuration transitions"
            );
        }
        if matches!(
            method,
            "session.question.ask" | "session.check.run" | "session.check.cancel"
        ) {
            anyhow::bail!(
                "unsupported runtime method: use input submission, check tools, or exchange cancellation"
            );
        }
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
                | Self::SessionPlanChangesRequest
                | Self::SessionQuestionAnswer
                | Self::SessionQuestionSkip
                | Self::SessionQuestionContinue
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
                | Self::SessionQuestionContinue
        )
    }
}

#[cfg(test)]
mod test {

    /// Timeout feedback is an owner event and cannot be invoked as a public mutation.
    #[test]
    fn rejects_public_approval_timeout_continuation() {
        let error = super::RuntimeMethod::decode("session.approval.continue").unwrap_err();
        assert!(
            error
                .to_string()
                .contains("approval timeout feedback is owned internally")
        );
    }
}
