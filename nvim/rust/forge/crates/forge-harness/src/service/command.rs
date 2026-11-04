//! Admit normalized caller commands through the session's single owner boundary.

use super::*;
use crate::exchange::command::{CommandRequest, CommandResult, ExecuteCommandError};

impl SessionController {
    /// Admit one immutable intent and await its finite committed outcome.
    /// Stop admission when session shutdown begins; dropping the waiter never cancels owned work.
    pub(super) async fn execute_command(
        &self,
        request: CommandRequest,
        sink: &MessageSender,
    ) -> std::result::Result<CommandResult, ExecuteCommandError> {
        if self.input_stopping.load(Ordering::Acquire) {
            return Err(ExecuteCommandError::OwnerClosed);
        }
        self.exchange_owner
            .execute_identified(request, sink.clone())
            .await
    }
}

impl SessionController {
    /// Project immutable completion evidence using only retained outcomes and immutable input receipts.
    pub(super) fn command_response(
        &self,
        command: &crate::exchange::command::Command,
        result: &CommandResult,
    ) -> Result<Value> {
        use crate::exchange::agent_command::AgentOutcome;
        use crate::exchange::command::{
            Command, CommandOutcome, ConfigurationOutcome, TaskCommandOutcome,
        };
        use crate::exchange::rollback::{PreparedRollbackToken, RollbackOutcome};
        let mut response = match &result.outcome {
            CommandOutcome::Agent(AgentOutcome::Admitted { agent, task }) => {
                json!({"agent":agent,"task":task})
            }
            CommandOutcome::Agent(AgentOutcome::Message { message, .. }) => {
                serde_json::to_value(message)?
            }
            CommandOutcome::Agent(AgentOutcome::Interrupted { targets }) => {
                json!({"interrupted":true,"targets":targets})
            }
            CommandOutcome::Rollback(RollbackOutcome::Prepared {
                token,
                files,
                warnings,
            }) => {
                let preview_id = match token {
                    PreparedRollbackToken::Workspace { preview_id, .. } => {
                        Some(preview_id.as_str())
                    }
                    PreparedRollbackToken::Conversation { .. } => None,
                };
                json!({"preview_id":preview_id,"files":files,"warning_count":warnings.len(),
                    "warnings":warnings.iter().take(50).collect::<Vec<_>>(),"next_offset":(warnings.len()>50).then_some(50)})
            }
            CommandOutcome::Rollback(RollbackOutcome::Restored) => json!({"restored":true}),
            CommandOutcome::Task(TaskCommandOutcome::PlanEdited { plan, .. }) => {
                json!({"plan":plan,"review_reload_required":true})
            }
            CommandOutcome::Task(_) => {
                json!({"id":result.id,"state":"completed","outcome":result.outcome})
            }
            CommandOutcome::Compacted(_) => {
                json!({"id":result.id,"action":"compact","state":"completed","outcome":result.outcome})
            }
            CommandOutcome::Cancelled(_) => {
                json!({"cancel_requested":true,"collected":true,"command_id":result.id,"outcome":result.outcome})
            }
            CommandOutcome::Configured(ConfigurationOutcome::CheckEditing { enabled }) => {
                json!({"check_editing":enabled})
            }
            CommandOutcome::Configured(ConfigurationOutcome::CheckPreferences {
                check_timing,
                automatic_checks,
                checks_deferred,
            }) => {
                json!({"check_timing":check_timing,"automatic_checks":automatic_checks,"checks_deferred":checks_deferred})
            }
            CommandOutcome::Configured(ConfigurationOutcome::CheckOverrides {
                scope,
                overrides,
                configuration,
            }) => {
                json!({"configured":true,"revision":configuration.digest,"snapshot":configuration,"overrides":overrides,"task_id":scope.task_id()})
            }
            CommandOutcome::Configured(ConfigurationOutcome::InputTimeouts {
                approval_timeout,
                tool_timeout,
            }) => json!({"approval_timeout":approval_timeout,"tool_timeout":tool_timeout}),
            CommandOutcome::Configured(ConfigurationOutcome::Settings { ask_model, .. }) => {
                json!({"id":result.id,"action":"configure","state":"completed","outcome":result.outcome,"ask_model":ask_model})
            }
            CommandOutcome::Configured(ConfigurationOutcome::ExecutionPolicy { .. }) => {
                json!({"id":result.id,"action":"permission","state":"completed","outcome":result.outcome})
            }
            CommandOutcome::Input { .. } => match command {
                Command::Input(_) => {
                    match self.coordinator.submissions.operation(result.id.as_str())? {
                        Some(receipt) => serde_json::to_value(receipt)?,
                        None => serde_json::to_value(result)?,
                    }
                }
                Command::Task(_)
                | Command::ContinueWorkflow(_)
                | Command::Agent(_)
                | Command::Rollback(_)
                | Command::ConfigureSession(_)
                | Command::CompactContext(_)
                | Command::CancelExchange(_) => {
                    anyhow::bail!("input outcome does not match command family")
                }
            },
        };
        if let Some(fields) = response.as_object_mut() {
            fields.insert("command_result".into(), serde_json::to_value(result)?);
            fields
                .entry("sequence")
                .or_insert_with(|| json!(result.sequence));
            fields
                .entry("revision")
                .or_insert_with(|| json!(result.revision));
        }
        Ok(response)
    }
}

/// Prefer durable owner history; only unknown identities consult the old transport receipt cache.
pub(super) async fn respond_status(
    controller: &SessionController,
    request: &Request,
    sink: &MessageSender,
) -> Result<()> {
    use crate::exchange::command::CommandId;
    use crate::exchange::journal::CommandState;
    let id = CommandId::new(
        request.params["id"]
            .as_str()
            .context("command identity is required")?,
    )?;
    let result = match controller.exchange_owner.command_status(id.clone()).await? {
        None => controller.coordinator.status(id.as_str())?,
        Some(record) => {
            let mut status =
                json!({"id":id,"sequence":record.sequence,"revision":record.state.revision()});
            match &record.state {
                CommandState::Queued { .. } => status["state"] = json!("queued"),
                CommandState::Active { task_commit, .. } => {
                    status["state"] = json!("active");
                    status["progress"] = serde_json::to_value(task_commit)?;
                }
                CommandState::Finished { outcome, .. } => {
                    status["state"] = json!("completed");
                    match outcome {
                        Ok(outcome) => {
                            let result = CommandResult {
                                id: record.request.id.clone(),
                                sequence: record.sequence,
                                revision: record.state.revision(),
                                outcome: outcome.clone(),
                            };
                            status["outcome"] = serde_json::to_value(outcome)?;
                            status["response"] = json!({"result":controller.command_response(&record.request.command, &result)?});
                        }
                        Err(error) => {
                            status["error"] = json!(error.to_string());
                            status["error_code"] = json!(error.code());
                            status["response"] =
                                json!({"error":{"code":error.code(),"message":error.to_string()}});
                        }
                    }
                }
            }
            status
        }
    };
    sink.send_control_response(Response::success(request.id, result)?)
        .await?;
    Ok(())
}

/// Recognize all caller command aliases before the transport coordinator can create a second owner.
pub(super) fn handles_owner_command(
    controller: &SessionController,
    request: &Request,
) -> Result<bool> {
    if cancellation::handles(request)
        || native_settings::handles(request)
        || native_permission::handles(request)
        || compaction::handles(request)
        || local_configuration::handles(request)
        || task_state::handles(controller, request)?
    {
        return Ok(true);
    }
    let method = RuntimeMethod::decode(&request.method)?;
    Ok(owner_commands::handles(method, request)
        || matches!(
            method,
            RuntimeMethod::SessionInputApply
                | RuntimeMethod::SessionTurnSteer
                | RuntimeMethod::SessionPromptSubmit
                | RuntimeMethod::SessionQuestionContinue
        ))
}
