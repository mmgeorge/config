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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exchange::command::{
        CommandFailure, CommandOutcome, ConfigurationOutcome, InvocationDisposition,
    };

    /// Create an immutable command using the same validation as legacy configuration ingress.
    fn selection(id: &str, config: Value) -> Result<CommandRequest> {
        decode_command(&Request {
            id: 1,
            method: "session.command.apply".into(),
            params: json!({"action":"configure","operation_id":id,"config":config}),
        })
    }

    /// Build a host session with generic provider behavior and independently retained storage.
    fn controller(root: &std::path::Path) -> Result<Arc<SessionController>> {
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "workspace":root,"data_root":root.join("data"),"client_id":"settings-owner",
            "provider":{"kind":"mock","command":[]},
        }))?;
        SessionController::new(HarnessBroker::initialize(initialize)?)
    }

    /// Commit presentation policy without acquiring execution, and never reapply a durable replay.
    #[tokio::test]
    async fn host_setting_commits_while_execution_is_held_and_replay_does_not_write() -> Result<()>
    {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let execution = controller.coordinator.execution.acquire().await?;
        let control = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let request = selection(
            "presentation-setting",
            json!({"status_thoughts":false,"tool_timeout":{"amount":1,"unit":"m"}}),
        )?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let completed = tokio::time::timeout(
            Duration::from_secs(3),
            controller
                .exchange_owner
                .execute_identified(request.clone(), messages.clone()),
        )
        .await??;
        assert!(matches!(
            completed.outcome,
            CommandOutcome::Configured(ConfigurationOutcome::Settings {
                disposition: InvocationDisposition::Committed,
                ..
            })
        ));
        assert!(!controller.observer.status_thoughts.load(Ordering::Relaxed));
        let session = controller.observer.snapshot()?.session;
        let settings = crate::settings::SettingsStore::open(&controller.data_root)?;
        let revision = settings
            .get(&session.workspace, &session.id, &session.provider)?
            .revision;
        let replay = controller
            .exchange_owner
            .execute_identified(request, messages)
            .await?;
        assert_eq!(completed.revision, replay.revision);
        assert_eq!(
            revision,
            settings
                .get(&session.workspace, &session.id, &session.provider)?
                .revision
        );
        drop(execution);
        drop(control);
        controller.finish_exchange_owner().await?;
        Ok(())
    }

    /// Preserve both queued patches and their receipts instead of applying only the latest choice.
    #[tokio::test]
    async fn model_and_effort_settings_apply_in_fifo_without_losing_fields() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let gate = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let mut model = Box::pin(controller.exchange_owner.execute_identified(
            selection("first-model", json!({"model":"mock-model"}))?,
            messages.clone(),
        ));
        assert!(futures_util::poll!(model.as_mut()).is_pending());
        let mut effort = Box::pin(controller.exchange_owner.execute_identified(
            selection("second-effort", json!({"effort":"high"}))?,
            messages.clone(),
        ));
        assert!(futures_util::poll!(effort.as_mut()).is_pending());
        drop(gate);
        let (model, effort) = tokio::time::timeout(Duration::from_secs(3), async {
            tokio::join!(model, effort)
        })
        .await?;
        let model = model?;
        let effort = effort?;
        assert!(model.sequence < effort.sequence);
        let session = controller.observer.snapshot()?.session;
        assert_eq!(session.model, "mock-model");
        assert_eq!(session.effort, "high");
        controller.finish_exchange_owner().await?;
        Ok(())
    }

    /// Reject unavailable selections before collection and keep the next FIFO command usable.
    #[tokio::test]
    async fn question_model_override_and_clear_are_ordered_and_do_not_restart_root() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let root_model = controller.observer.snapshot()?.session.model;
        let execution = controller.coordinator.execution.acquire().await?;
        let control = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let original = selection(
            "question-override",
            json!({"ask_model":{"model":"mock-model","effort":"high"}}),
        )?;
        let mut selected = Box::pin(
            controller
                .exchange_owner
                .execute_identified(original.clone(), messages.clone()),
        );
        assert!(futures_util::poll!(selected.as_mut()).is_pending());
        let mut cleared = Box::pin(controller.exchange_owner.execute_identified(
            selection("question-inherit", json!({"ask_model":null}))?,
            messages.clone(),
        ));
        assert!(futures_util::poll!(cleared.as_mut()).is_pending());
        drop(control);
        let (selected, cleared) = tokio::time::timeout(Duration::from_secs(3), async {
            tokio::join!(selected, cleared)
        })
        .await?;
        let selected = selected?;
        let cleared = cleared?;
        assert!(selected.sequence < cleared.sequence);
        assert!(
            matches!(selected.outcome, CommandOutcome::Configured(ConfigurationOutcome::Settings {
            disposition: InvocationDisposition::Committed, ask_model: Some(ref choice),
        }) if choice.model.as_deref() == Some("mock-model"))
        );
        let session = controller.observer.snapshot()?.session;
        assert_eq!(session.model, root_model);
        let settings = crate::settings::SettingsStore::read_only(&controller.data_root)?;
        assert!(
            settings
                .get(&session.workspace, &session.id, &session.provider)?
                .ask_model()?
                .model
                .is_none()
        );
        let replay = controller
            .exchange_owner
            .execute_identified(original, messages)
            .await?;
        assert_eq!(replay.revision, selected.revision);
        assert!(
            settings
                .get(&session.workspace, &session.id, &session.provider)?
                .ask_model()?
                .model
                .is_none()
        );
        drop(execution);
        controller.finish_exchange_owner().await?;
        Ok(())
    }

    /// Reject unavailable selections before collection and keep the next FIFO command usable.
    #[tokio::test]
    async fn rejected_selection_does_not_fence_later_configuration() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let original = controller.observer.snapshot()?.session.model;
        let (messages, _output) = forge_protocol::outbound::channel();
        let rejected = controller
            .exchange_owner
            .execute_identified(
                selection(
                    "unavailable-model",
                    json!({"model":"unavailable-choice","validate":true}),
                )?,
                messages.clone(),
            )
            .await;
        assert!(matches!(
            rejected,
            Err(ExecuteCommandError::Failed(
                CommandFailure::ConfigurationRejected(
                    crate::exchange::command::ConfigurationRejection::ModelUnavailable
                )
            ))
        ));
        assert_eq!(controller.observer.snapshot()?.session.model, original);
        controller
            .exchange_owner
            .execute_identified(
                selection("after-rejection", json!({"status_thoughts":false}))?,
                messages,
            )
            .await?;
        assert!(!controller.observer.status_thoughts.load(Ordering::Relaxed));
        let execution = controller.coordinator.execution.acquire().await?;
        drop(execution);
        controller.finish_exchange_owner().await?;
        Ok(())
    }
}
