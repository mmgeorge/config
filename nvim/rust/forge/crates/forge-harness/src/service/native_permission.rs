//! Normalize execution permission intent while the exchange owner chooses collection and restart.

use super::*;
use crate::exchange::command::{
    Command, CommandId, CommandRequest, ConfigurationChange, ExecuteCommandError, InputControl,
    PermissionChange,
};

/// Recognize supported compatibility routes without exposing internal restart stages.
pub(super) fn handles(request: &Request) -> bool {
    request.method == "session.permission.set"
        || request.method == "session.command.apply" && request.params["action"] == "permission"
}

/// Decode execution-policy intent without accepting caller-controlled restart authority.
pub(super) fn decode_command(request: &Request) -> Result<CommandRequest> {
    #[derive(serde::Deserialize)]
    #[serde(rename_all = "snake_case")]
    enum Action {
        Permission,
    }
    #[derive(serde::Deserialize)]
    #[serde(deny_unknown_fields)]
    struct Selection {
        operation_id: Option<String>,
        #[serde(rename = "action")]
        _action: Option<Action>,
        change: crate::task::PermissionChange,
    }
    let Selection {
        operation_id,
        _action: _,
        change,
    } = serde_json::from_value(request.params.clone())?;
    let id = CommandId::new(operation_id.unwrap_or_else(|| uuid::Uuid::new_v4().to_string()))?;
    Ok(CommandRequest {
        id,
        command: Command::ConfigureSession(ConfigurationChange::Permission(
            PermissionChange::ExecutionPolicy { change },
        )),
        input_control: InputControl::AtExecution,
    })
}

/// Submit one immutable policy change and wait for committed policy plus any required handoff.
pub(super) async fn apply(
    controller: &SessionController,
    request: &Request,
    messages: &MessageSender,
) -> Result<()> {
    let command = decode_command(request)?;
    let id = command.id.clone();
    let result = controller.execute_command(command, messages).await;
    let response = match result {
        Ok(result) => Response::success(
            request.id,
            json!({
                "id":result.id.as_str(),"action":"permission","state":"completed",
                "sequence":result.sequence,"revision":result.revision,"outcome":result.outcome,
            }),
        )?,
        Err(ExecuteCommandError::Failed(error)) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"permission","state":"failed","error":error.to_string(),"error_code":error.code(),
            }),
        )?,
        Err(
            error
            @ (ExecuteCommandError::OutcomeUncommitted | ExecuteCommandError::ObservationLost),
        ) => Response::success(
            request.id,
            json!({
                "id":id.as_str(),"action":"permission","state":"outcome_unknown","error":error.to_string(),"error_code":error.code(),
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
        CancelExchangeRequest, CancellationOutcome, CommandOutcome, ConfigurationOutcome,
        InvocationDisposition,
    };

    /// Create a normalized policy command without admission-time toggle resolution.
    fn policy(id: &str) -> Result<CommandRequest> {
        Ok(CommandRequest {
            id: CommandId::new(id)?,
            command: Command::ConfigureSession(ConfigurationChange::Permission(
                PermissionChange::ExecutionPolicy {
                    change: crate::task::PermissionChange::ToggleWriteEnabled,
                },
            )),
            input_control: InputControl::AtExecution,
        })
    }

    /// Apply two toggles then cancellation in FIFO order and never replay a completed toggle.
    #[tokio::test]
    async fn idle_permission_commands_resolve_toggles_at_execution_and_replay_once() -> Result<()> {
        let root = tempfile::tempdir()?;
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"permission-fifo",
            "provider":{"kind":"mock","command":[]},
        }))?;
        let controller = SessionController::new(HarnessBroker::initialize(initialize)?).await?;
        let original = controller.observer.snapshot()?.session.access.write_access;
        let gate = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let request = policy("policy-first")?;
        let mut first = Box::pin(
            controller
                .exchange_owner
                .execute_identified(request.clone(), messages.clone()),
        );
        assert!(futures_util::poll!(first.as_mut()).is_pending());
        let mut second = Box::pin(
            controller
                .exchange_owner
                .execute_identified(policy("policy-second")?, messages.clone()),
        );
        assert!(futures_util::poll!(second.as_mut()).is_pending());
        let mut cancel = Box::pin(
            controller
                .exchange_owner
                .execute_command(Command::CancelExchange(CancelExchangeRequest::default())),
        );
        assert!(futures_util::poll!(cancel.as_mut()).is_pending());
        drop(gate);
        let (first, second, cancel) = async { tokio::join!(first, second, cancel) }.await;
        let first = first?;
        let second = second?;
        let cancel = cancel?;
        assert!(first.sequence < second.sequence && second.sequence < cancel.sequence);
        assert!(matches!(
            first.outcome,
            CommandOutcome::Configured(ConfigurationOutcome::ExecutionPolicy {
                disposition: InvocationDisposition::Committed
            })
        ));
        assert!(matches!(
            second.outcome,
            CommandOutcome::Configured(ConfigurationOutcome::ExecutionPolicy {
                disposition: InvocationDisposition::Committed
            })
        ));
        assert!(matches!(
            cancel.outcome,
            CommandOutcome::Cancelled(CancellationOutcome::NoWork)
        ));
        assert_eq!(
            controller.observer.snapshot()?.session.access.write_access,
            original
        );
        let session = controller.observer.snapshot()?.session;
        let settings = crate::settings::SettingsStore::open(&controller.data_root)?;
        let revision = settings
            .get(&session.workspace, &session.id, &session.provider)?
            .revision;
        let replay = controller
            .exchange_owner
            .execute_identified(request, messages)
            .await?;
        assert_eq!(replay.sequence, first.sequence);
        assert_eq!(replay.revision, first.revision);
        assert_eq!(
            controller.observer.snapshot()?.session.access.write_access,
            original
        );
        assert_eq!(
            settings
                .get(&session.workspace, &session.id, &session.provider)?
                .revision,
            revision
        );
        controller.finish_exchange_owner().await?;
        Ok(())
    }
}
