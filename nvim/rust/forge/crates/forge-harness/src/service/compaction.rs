//! Route manual compaction through the session owner's durable FIFO and completion contract.

use super::*;
use crate::exchange::command::{
    Command, CommandId, CommandRequest, ContextCompactionRequest, ExecuteCommandError, InputControl,
};

/// Recognize caller compaction intent without exposing native cleanup or continuation stages.
pub(super) fn handles(request: &Request) -> bool {
    request.method == "session.compact"
        || request.method == "session.command.apply" && request.params["action"] == "compact"
}

/// Decode only caller intent and idempotency metadata; lifecycle flags are rejected.
pub(super) fn decode_command(request: &Request) -> Result<CommandRequest> {
    #[derive(serde::Deserialize)]
    #[serde(rename_all = "snake_case")]
    enum Action {
        Compact,
    }
    #[derive(serde::Deserialize)]
    #[serde(deny_unknown_fields)]
    struct Selection {
        operation_id: Option<String>,
        #[serde(rename = "action")]
        _action: Option<Action>,
    }
    let Selection {
        operation_id,
        _action: _,
    } = serde_json::from_value(if request.params.is_null() {
        json!({})
    } else {
        request.params.clone()
    })?;
    Ok(CommandRequest {
        id: CommandId::new(operation_id.unwrap_or_else(|| uuid::Uuid::new_v4().to_string()))?,
        command: Command::CompactContext(ContextCompactionRequest::default()),
        input_control: InputControl::AtExecution,
    })
}

/// Resolve after confirmed compaction and required replacement dispatch, independently of the answer.
pub(super) async fn apply(
    controller: &SessionController,
    request: &Request,
    messages: &MessageSender,
) -> Result<()> {
    let command = decode_command(request)?;
    let id = command.id.clone();
    let outcome = controller.execute_command(command, messages).await;
    let response = match outcome {
        Ok(result) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"compact","state":"completed","sequence":result.sequence,"revision":result.revision,"outcome":result.outcome,
            }),
        )?,
        Err(ExecuteCommandError::Failed(error)) if request.method == "session.compact" => {
            Response::failure(request.id, error.code(), error.to_string())
        }
        Err(ExecuteCommandError::Failed(error)) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"compact","state":"failed","error":error.to_string(),"error_code":error.code(),
            }),
        )?,
        Err(
            error
            @ (ExecuteCommandError::OutcomeUncommitted | ExecuteCommandError::ObservationLost),
        ) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"compact","state":"outcome_unknown","error":error.to_string(),"error_code":error.code(),
            }),
        )?,
        Err(error) => Response::failure(request.id, error.code(), error.to_string()),
    };
    messages.send_control_response(response).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exchange::command::{
        CommandFailure, ConfigurationChange, ContextCompactionFailure, SessionConfigurationPatch,
    };

    /// Reject caller lifecycle flags and leave execution available after read-only preflight failure.
    #[tokio::test]
    async fn compaction_preflight_rejects_without_fencing_later_commands() -> Result<()> {
        let invalid = Request {
            id: 1,
            method: "session.compact".into(),
            params: json!({"resume":true}),
        };
        assert!(decode_command(&invalid).is_err());
        let root = tempfile::tempdir()?;
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"compaction-preflight",
            "provider":{"kind":"mock","command":[]},
        }))?;
        let controller = SessionController::new(HarnessBroker::initialize(initialize)?)?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let request = Request {
            id: 1,
            method: "session.compact".into(),
            params: json!({"operation_id":"before-conversation"}),
        };
        let rejected = controller
            .exchange_owner
            .execute_identified(decode_command(&request)?, messages)
            .await;
        assert!(matches!(
            rejected,
            Err(ExecuteCommandError::Failed(CommandFailure::Compaction(
                ContextCompactionFailure::NoConversation
            )))
        ));
        let patch: SessionConfigurationPatch =
            serde_json::from_value(json!({"status_thoughts":false}))?;
        controller
            .exchange_owner
            .execute_command(Command::ConfigureSession(ConfigurationChange::Settings(
                patch,
            )))
            .await?;
        let execution = controller.coordinator.execution.acquire().await?;
        drop(execution);
        controller.finish_exchange_owner().await?;
        Ok(())
    }
}
