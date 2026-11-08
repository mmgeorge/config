//! Decode settings intent and observe the exchange owner's finite command completion.

use super::*;
use crate::exchange::command::{
    Command, CommandId, CommandRequest, ConfigurationChange, ExecuteCommandError, InputControl,
    SessionConfigurationPatch,
};

/// Recognize settings intent without excluding a separate question-model mutation path.
pub(super) fn handles(request: &Request) -> bool {
    request.method == "session.command.apply" && request.params["action"] == "configure"
        || request.method == "session.configure"
}

/// Normalize transport metadata separately from the immutable settings patch.
pub(super) fn decode_command(request: &Request) -> Result<CommandRequest> {
    let mut fields = request
        .params
        .as_object()
        .cloned()
        .context("configuration requires an object")?;
    let id = match fields.remove("operation_id") {
        Some(value) => CommandId::new(value.as_str().context("operation_id must be a string")?)?,
        None => CommandId::new(uuid::Uuid::new_v4().to_string())?,
    };
    let patch = if request.method == "session.command.apply" {
        #[derive(serde::Deserialize)]
        #[serde(rename_all = "snake_case")]
        enum Action {
            Configure,
        }
        #[derive(serde::Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Selection {
            #[serde(rename = "action")]
            _action: Action,
            config: SessionConfigurationPatch,
        }
        let Selection { _action: _, config } = serde_json::from_value(Value::Object(fields))?;
        config
    } else {
        serde_json::from_value(Value::Object(fields))?
    };
    Ok(CommandRequest {
        id,
        command: Command::ConfigureSession(ConfigurationChange::Settings(patch)),
        input_control: InputControl::AtExecution,
    })
}

/// Submit one settings command; its caller never owns collection or the replacement response.
pub(super) async fn apply(
    controller: &SessionController,
    request: &Request,
    messages: &MessageSender,
) -> Result<()> {
    let command = decode_command(request)?;
    let id = command.id.clone();
    let result = controller.execute_command(command, messages).await;
    let ask_model = match result.as_ref().ok().map(|result| &result.outcome) {
        Some(crate::exchange::command::CommandOutcome::Configured(
            crate::exchange::command::ConfigurationOutcome::Settings { ask_model, .. },
        )) => ask_model.clone(),
        Some(_) | None => None,
    };
    let response = match result {
        Ok(result) => Response::success(
            request.id,
            json!({
                "id":result.id.as_str(),"action":"configure","state":"completed",
                "sequence":result.sequence,"revision":result.revision,"outcome":result.outcome,"ask_model":ask_model,
            }),
        )?,
        Err(ExecuteCommandError::Failed(error)) if request.method == "session.configure" => {
            Response::failure(request.id, error.code(), error.to_string())
        }
        Err(ExecuteCommandError::Failed(error)) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"configure","state":"failed","error":error.to_string(),"error_code":error.code(),
            }),
        )?,
        Err(
            error
            @ (ExecuteCommandError::OutcomeUncommitted | ExecuteCommandError::ObservationLost),
        ) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"configure","state":"outcome_unknown","error":error.to_string(),"error_code":error.code(),
            }),
        )?,
        Err(error) => Response::failure(request.id, error.code(), error.to_string()),
    };
    messages.send_control_response(response).await?;
    Ok(())
}
