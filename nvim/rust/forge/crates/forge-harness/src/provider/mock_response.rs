//! Harness RPC adapter for provider-neutral deterministic response controls.

use std::sync::Arc;

use anyhow::Result;
use forge_provider_api::{
    ExecutionOwner,
    testing::{
        ProviderTestControl, TestControlCommand, TestControlResult, TestHoldId, TestPromptScript,
    },
};
use serde::Deserialize;
use serde_json::{Value, json};

use super::PromptMode;

/// Test-only operations accepted by `provider.mock_response` for the selected mock session.
#[derive(Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum MockResponseCommand {
    /// Hold the next matching prompt. An omitted mode matches any prompt mode.
    Hold {
        mode: Option<PromptMode>,
        #[serde(default)]
        any_session: bool,
    },
    /// Wait for the provider to reach a previously armed hold without polling.
    Wait { id: String },
    /// Release an arrived response. An unclaimed, cancelled, or unknown hold is an error.
    Release { id: String },
    /// Discard an armed hold or cancel a prompt already waiting on it.
    Cancel { id: String },
    /// Queue normalized prompt steps and terminal evidence for the next request.
    Script { script: TestPromptScript },
}

/// Preserve the existing Harness RPC surface while routing holds through a normalized provider API.
pub(crate) struct MockResponseControl {
    control: Arc<dyn ProviderTestControl>,
}

impl MockResponseControl {
    /// Adapt one provider's optional normalized testing capability for Harness callers.
    pub(crate) fn new(control: Arc<dyn ProviderTestControl>) -> Self {
        Self { control }
    }

    /// Apply an explicit test operation for one visible Harness session.
    pub(crate) async fn dispatch(
        &self,
        session_id: &str,
        command: MockResponseCommand,
    ) -> Result<Value> {
        let owner = ExecutionOwner::root(session_id);
        let command = match command {
            MockResponseCommand::Hold { mode, any_session } => TestControlCommand::Hold {
                mode,
                any_owner: any_session,
            },
            MockResponseCommand::Wait { id } => TestControlCommand::Wait {
                id: TestHoldId::new(id)?,
            },
            MockResponseCommand::Release { id } => TestControlCommand::Release {
                id: TestHoldId::new(id)?,
            },
            MockResponseCommand::Cancel { id } => TestControlCommand::Cancel {
                id: TestHoldId::new(id)?,
            },
            MockResponseCommand::Script { script } => TestControlCommand::Script { script },
        };
        let result = self
            .control
            .dispatch(&owner, command)
            .await
            .map_err(|error| anyhow::Error::msg(error.to_string()))?;
        match result {
            TestControlResult::Held { id } => Ok(json!({"id": id.as_str()})),
            TestControlResult::Pending { response } => Ok(json!({
                "id": response.id.as_str(),
                "session_id": response.owner.runtime_key(),
                "mode": response.mode,
                "prompt": response.prompt,
            })),
            TestControlResult::Released { id } => {
                Ok(json!({"id": id.as_str(), "released": true, "cancelled": false}))
            }
            TestControlResult::Cancelled { id } => {
                Ok(json!({"id": id.as_str(), "released": false, "cancelled": true}))
            }
            TestControlResult::Scripted => Ok(json!({"scripted": true})),
        }
    }
}
