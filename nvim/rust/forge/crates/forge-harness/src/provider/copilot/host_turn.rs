use super::SharedCopilotSession;
use crate::permissions::provider::ProviderPermissionContext;
use crate::provider::{ProviderCompletion, ProviderError};
use crate::tools::router::{ControlToolRouter, ControlToolStream};
use crate::tools::{ControlToolInvocation, apply_invocation};
use async_trait::async_trait;
use forge_provider_api::turn_control::{HostTurnStatus, TurnControl};
use forge_providers::copilot::sessions::CopilotSessions;
use std::sync::{Arc, Mutex};

pub(super) struct HostTurn {
    router: Arc<ControlToolRouter>,
    stream: tokio::sync::Mutex<ControlToolStream>,
    permission: Arc<ProviderPermissionContext>,
    output: Mutex<ProviderCompletion>,
    sessions: Arc<CopilotSessions<crate::settings::ResourceProfile>>,
    runtime_key: String,
    session: SharedCopilotSession,
    execute_plan: bool,
}

impl HostTurn {
    pub(super) fn new(
        router: Arc<ControlToolRouter>,
        stream: ControlToolStream,
        permission: Arc<ProviderPermissionContext>,
        sessions: Arc<CopilotSessions<crate::settings::ResourceProfile>>,
        runtime_key: String,
        session: SharedCopilotSession,
        execute_plan: bool,
    ) -> Self {
        Self {
            router,
            stream: tokio::sync::Mutex::new(stream),
            permission,
            output: Mutex::new(ProviderCompletion::default()),
            sessions,
            runtime_key,
            session,
            execute_plan,
        }
    }

    pub(super) fn take_output(&self) -> Result<ProviderCompletion, ProviderError> {
        let mut output = self.output.lock().map_err(|_| unavailable())?;
        Ok(std::mem::take(&mut *output))
    }

    fn record(&self, invocation: ControlToolInvocation) -> Result<(), ProviderError> {
        let mut output = self.output.lock().map_err(|_| unavailable())?;
        if let Err(error) = apply_invocation(&invocation, &mut output) {
            output.control_error = Some(format!("{error:#}"));
        }
        Ok(())
    }
}

#[async_trait]
impl TurnControl for HostTurn {
    async fn before_send(&self) -> Result<(), ProviderError> {
        self.sessions
            .mark_submitted(&self.runtime_key, &self.session)
            .await
    }

    async fn receive(&self) -> Result<(), ProviderError> {
        match self.stream.lock().await.receive().await {
            Some(invocation) => self.record(invocation),
            None => std::future::pending().await,
        }
    }

    async fn close(&self) -> Result<(), ProviderError> {
        self.router.close().await?;
        let mut stream = self.stream.lock().await;
        while let Ok(invocation) = stream.try_receive() {
            self.record(invocation)?;
        }
        Ok(())
    }

    async fn status(&self) -> Result<HostTurnStatus, ProviderError> {
        let output = self.output.lock().map_err(|_| unavailable())?;
        Ok(HostTurnStatus {
            replay_safe: crate::provider::retry::replay_safe(&output),
            control_handoff: output.evidence.structured_complete
                || output.evidence.structured_blocked
                || output.plan_submit.is_some()
                || output.plan_question.is_some(),
            cancelled: self.permission.cancelled()?,
            error: output.control_error.clone(),
        })
    }

    fn tool_finished(&self, name: Option<&str>) -> bool {
        self.execute_plan && name == Some("plan_phase_done") && self.router.execution_finished()
    }
}

fn unavailable() -> ProviderError {
    ProviderError::Protocol {
        message: "Copilot host result lock poisoned".into(),
    }
}
