//! Normalize child control and checkpoint aliases into durable owner commands.
use super::*;
use crate::agent::AgentOperation;
use crate::exchange::agent_command::{AgentCommand, AgentOutcome};
use crate::exchange::command::{Command, CommandId, CommandOutcome, CommandRequest, InputControl};
use crate::exchange::rollback::{
    PreparedRollbackToken, RollbackCommand, RollbackOutcome, RollbackSelection,
};

/// Identify mutation aliases owned by this adapter; recovery status stays a query.
pub(super) fn handles(method: RuntimeMethod, request: &Request) -> bool {
    match method {
        RuntimeMethod::SessionAgentStart
        | RuntimeMethod::SessionAgentMessageSend
        | RuntimeMethod::SessionAgentFollowup
        | RuntimeMethod::SessionAgentInterrupt
        | RuntimeMethod::SessionAgentInterruptAll
        | RuntimeMethod::SessionExchangeRollbackPrepare
        | RuntimeMethod::SessionExchangeRollbackApply => true,
        RuntimeMethod::SessionExchangeRecovery => {
            matches!(request.params["action"].as_str(), Some("continue" | "undo"))
        }
        RuntimeMethod::SessionCommandApply => request.params["action"] == "undo",
        _ => false,
    }
}

/// Reuse retained intent before capturing mutable child, preview, or journal authority.
pub(super) async fn apply(
    controller: &SessionController,
    request: &Request,
    method: RuntimeMethod,
    sink: &MessageSender,
) -> Result<()> {
    let id = request
        .params
        .get("operation_id")
        .or_else(|| request.params.pointer("/_command/id"))
        .map(|value| serde_json::from_value::<CommandId>(value.clone()))
        .transpose()?
        .unwrap_or(CommandId::new(uuid::Uuid::new_v4().to_string())?);
    let operation = agent_operation(method, request)?;
    let previous = controller.exchange_owner.command_status(id.clone()).await?;
    let command = if let Some(previous) = previous {
        let command = previous.request.command;
        let matches = match (&command, &operation) {
            (Command::Agent(command), Some(operation)) => command.matches_operation(operation),
            (Command::Rollback(command), None) => rollback_matches(command, method, request)?,
            _ => false,
        };
        ensure!(
            matches,
            "command identity was reused for a different intent"
        );
        command
    } else if let Some(operation) = operation {
        Command::Agent(AgentCommand::capture(
            operation,
            &controller.agent_coordinator,
        )?)
    } else {
        let command = capture_rollback(controller, method, request).await?;
        Command::Rollback(controller.broker.lock().await.capture_rollback(&command)?)
    };
    let result = controller
        .execute_command(
            CommandRequest {
                id,
                command,
                input_control: InputControl::AtExecution,
            },
            sink,
        )
        .await;
    match result {
        Ok(result) => {
            let mut response = match &result.outcome {
                CommandOutcome::Agent(outcome) => agent_response(outcome)?,
                CommandOutcome::Rollback(outcome) => rollback_response(outcome),
                _ => anyhow::bail!("owner returned a different command family"),
            };
            if method == RuntimeMethod::SessionCommandApply {
                response["id"] = json!(result.id.as_str());
                response["operation_id"] = json!(result.id.as_str());
                response["state"] = json!("completed");
                response["action"] = json!("undo");
            }
            if let Some(object) = response.as_object_mut() {
                object.insert("command_result".into(), serde_json::to_value(&result)?);
            }
            sink.send_control_response(Response::success(request.id, response)?)
                .await?;
        }
        Err(error) => {
            sink.send_control_response(Response::failure(
                request.id,
                error.code(),
                error.to_string(),
            ))
            .await?
        }
    }
    Ok(())
}

/// Decode agent aliases without consulting mutable child ownership.
fn agent_operation(method: RuntimeMethod, request: &Request) -> Result<Option<AgentOperation>> {
    Ok(match method {
        RuntimeMethod::SessionAgentStart => {
            let mut params = request.params.clone();
            if let Some(params) = params.as_object_mut() {
                params.remove("operation_id");
                params.remove("_command");
            }
            Some(AgentOperation::Spawn(serde_json::from_value(params)?))
        }
        RuntimeMethod::SessionAgentMessageSend => Some(AgentOperation::Message {
            target: text(&request.params, "target")?,
            message: text(&request.params, "message")?,
        }),
        RuntimeMethod::SessionAgentFollowup => Some(AgentOperation::Followup {
            target: text(&request.params, "target")?,
            message: text(&request.params, "message")?,
        }),
        RuntimeMethod::SessionAgentInterrupt => Some(AgentOperation::Interrupt {
            target: text(&request.params, "target")?,
        }),
        RuntimeMethod::SessionAgentInterruptAll => Some(AgentOperation::InterruptAll),
        _ => None,
    })
}

/// Capture fresh authority only after ruling out a retained command with this identity.
async fn capture_rollback(
    controller: &SessionController,
    method: RuntimeMethod,
    request: &Request,
) -> Result<RollbackCommand> {
    match method {
        RuntimeMethod::SessionExchangeRollbackPrepare => {
            let latest = match request.params["latest_exchange_id"].as_str() {
                Some(latest) => latest.to_owned(),
                None => {
                    let history = controller
                        .observer
                        .query(RuntimeMethod::SessionExchangeList, json!({}))?
                        .context("exchange history is unavailable")?;
                    history
                        .as_array()
                        .and_then(|history| history.last())
                        .and_then(|exchange| exchange["id"].as_str())
                        .context("exchange history is empty")?
                        .to_owned()
                }
            };
            Ok(RollbackCommand::Prepare(RollbackSelection::new(
                text(&request.params, "exchange_id")?,
                latest,
            )))
        }
        RuntimeMethod::SessionExchangeRollbackApply => {
            ensure!(
                matches!(
                    request.params["scope"].as_str(),
                    None | Some("workspace" | "conversation")
                ),
                "unknown rollback scope"
            );
            if request.params["scope"] == "conversation" {
                return Ok(RollbackCommand::Apply(conversation_token(&request.params)?));
            }
            Ok(RollbackCommand::Apply(
                controller
                    .broker
                    .lock()
                    .await
                    .rollback_token(&text(&request.params, "preview_id")?)?,
            ))
        }
        RuntimeMethod::SessionExchangeRecovery => {
            let revision = controller.broker.lock().await.restore_revision()?;
            if let Some(expected) = request.params["preview_id"].as_str() {
                ensure!(expected == revision.preview_id, "restore journal changed");
            }
            match request.params["action"].as_str() {
                Some("continue") => Ok(RollbackCommand::ResumeRestore(revision)),
                Some("undo") => Ok(RollbackCommand::PrepareUndoRestore(revision)),
                _ => anyhow::bail!("recovery status is read-only"),
            }
        }
        RuntimeMethod::SessionCommandApply => Ok(RollbackCommand::Apply(conversation_token(
            &request.params["rollback"],
        )?)),
        _ => anyhow::bail!("unsupported owner command alias"),
    }
}

/// Compare retry intent to the captured request without reopening current authority.
fn rollback_matches(
    command: &RollbackCommand,
    method: RuntimeMethod,
    request: &Request,
) -> Result<bool> {
    Ok(match (command, method) {
        (RollbackCommand::Prepare(selection), RuntimeMethod::SessionExchangeRollbackPrepare) => {
            selection.exchange_id == text(&request.params, "exchange_id")?
                && request.params["latest_exchange_id"]
                    .as_str()
                    .is_none_or(|latest| latest == selection.latest_exchange_id)
        }
        (
            RollbackCommand::Apply(PreparedRollbackToken::Workspace { preview_id, .. }),
            RuntimeMethod::SessionExchangeRollbackApply,
        ) => {
            request.params["scope"] != "conversation"
                && preview_id == &text(&request.params, "preview_id")?
        }
        (
            RollbackCommand::Apply(PreparedRollbackToken::Conversation { selection }),
            RuntimeMethod::SessionExchangeRollbackApply,
        ) => {
            request.params["scope"] == "conversation"
                && selection_matches(selection, &request.params)?
        }
        (
            RollbackCommand::Apply(PreparedRollbackToken::Conversation { selection }),
            RuntimeMethod::SessionCommandApply,
        ) => {
            request.params["action"] == "undo"
                && selection_matches(selection, &request.params["rollback"])?
        }
        (RollbackCommand::ResumeRestore(revision), RuntimeMethod::SessionExchangeRecovery) => {
            request.params["action"] == "continue"
                && request.params["preview_id"]
                    .as_str()
                    .is_none_or(|preview| preview == revision.preview_id)
        }
        (RollbackCommand::PrepareUndoRestore(revision), RuntimeMethod::SessionExchangeRecovery) => {
            request.params["action"] == "undo"
                && request.params["preview_id"]
                    .as_str()
                    .is_none_or(|preview| preview == revision.preview_id)
        }
        _ => false,
    })
}

/// Validate the explicit conversation scope before creating an application token.
fn conversation_token(params: &Value) -> Result<PreparedRollbackToken> {
    ensure!(
        params["scope"] == "conversation",
        "undo requires conversation rollback scope"
    );
    Ok(PreparedRollbackToken::Conversation {
        selection: RollbackSelection::new(
            text(params, "exchange_id")?,
            text(params, "latest_exchange_id")?,
        ),
    })
}

/// Compare caller-selected history identities while retaining the saved digest.
fn selection_matches(selection: &RollbackSelection, params: &Value) -> Result<bool> {
    Ok(params["scope"] == "conversation"
        && selection.exchange_id == text(params, "exchange_id")?
        && selection.latest_exchange_id == text(params, "latest_exchange_id")?)
}

/// Preserve legacy child response bodies while adding the durable command receipt.
fn agent_response(outcome: &AgentOutcome) -> Result<Value> {
    Ok(match outcome {
        AgentOutcome::Admitted { agent, task } => json!({"agent":agent, "task":task}),
        AgentOutcome::Message { message, .. } => serde_json::to_value(message)?,
        AgentOutcome::Interrupted { targets } => json!({"interrupted":true, "targets":targets}),
    })
}

/// Preserve preview pagination and restore completion fields expected by existing clients.
fn rollback_response(outcome: &RollbackOutcome) -> Value {
    match outcome {
        RollbackOutcome::Prepared {
            token,
            files,
            warnings,
        } => {
            let preview_id = match token {
                PreparedRollbackToken::Workspace { preview_id, .. } => preview_id,
                PreparedRollbackToken::Conversation { .. } => {
                    unreachable!("only workspace preparation returns a preview")
                }
            };
            json!({"preview_id":preview_id,"files":files,"warning_count":warnings.len(),
                "warnings":warnings.iter().take(50).collect::<Vec<_>>(),"next_offset":(warnings.len()>50).then_some(50)})
        }
        RollbackOutcome::Restored => json!({"restored":true}),
    }
}

/// Require nonempty transport identity or content at the adapter boundary.
fn text(params: &Value, field: &str) -> Result<String> {
    params[field]
        .as_str()
        .filter(|text| !text.trim().is_empty())
        .map(str::to_owned)
        .with_context(|| format!("owner command requires {field}"))
}
