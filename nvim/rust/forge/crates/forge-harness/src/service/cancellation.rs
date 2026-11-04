//! Normalize caller cancellation intent and observe the exchange owner's committed result.

use super::*;
use crate::exchange::command::{
    CancelExchangeRequest, CancelledInputPolicy, Command, CommandId, CommandRequest, InputControl,
};

/// Recognize supported root cancellation aliases at the transport boundary.
pub(super) fn handles(request: &Request) -> bool {
    request.method == "session.turn.cancel"
        || request.method == "session.command.apply"
            && matches!(request.params["action"].as_str(), Some("cancel" | "stop"))
}

/// Submit one cancellation and reply only after collection and host settlement complete.
pub(super) async fn apply(
    controller: &SessionController,
    request: &Request,
    messages: &MessageSender,
) -> Result<()> {
    validate_root_control(request.params.get("target"))?;
    let id = match request.params.get("operation_id") {
        Some(value) => CommandId::new(value.as_str().context("operation_id must be a string")?)?,
        None => CommandId::new(uuid::Uuid::new_v4().to_string())?,
    };
    let input = match request.params.get("restore_prompt_if_no_output") {
        None | Some(Value::Null) | Some(Value::Bool(false)) => CancelledInputPolicy::Retain,
        Some(Value::Bool(true)) => CancelledInputPolicy::RestoreIfUnconsumed,
        Some(_) => anyhow::bail!("restore_prompt_if_no_output must be a boolean"),
    };
    let result = controller
        .exchange_owner
        .execute_identified(
            CommandRequest {
                id,
                command: Command::CancelExchange(CancelExchangeRequest { input }),
                input_control: InputControl::default(),
            },
            messages.clone(),
        )
        .await;
    let response = match result {
        Ok(result) => Response::success(
            request.id,
            json!({
                "cancel_requested":true,"collected":true,
                "command_id":result.id.as_str(),"sequence":result.sequence,"revision":result.revision,
                "outcome":result.outcome,
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
    use crate::exchange::command::{CancellationOutcome, CommandOutcome};
    use crate::exchange::journal::CommandState;

    /// Create an idle real session owner without starting a native process or automatic input work.
    fn controller(root: &std::path::Path) -> Result<Arc<SessionController>> {
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "workspace":root,"data_root":root.join("data"),"client_id":"owner-cancel",
            "provider":{"kind":"mock","command":[]},
        }))?;
        SessionController::new(HarnessBroker::initialize(initialize)?)
    }

    /// Create a durable cancellation whose result remains observable across RPC retries.
    fn command(id: &str) -> Result<CommandRequest> {
        Ok(CommandRequest {
            id: CommandId::new(id)?,
            command: Command::CancelExchange(CancelExchangeRequest {
                input: CancelledInputPolicy::Retain,
            }),
            input_control: InputControl::default(),
        })
    }

    /// Keep FIFO intent and durable completion when the first caller drops its response future.
    #[tokio::test]
    async fn queued_cancel_survives_dropped_waiter_and_replays_one_result() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let gate = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let first_request = command("cancel-first")?;
        let first_id = first_request.id.clone();
        let second_request = command("cancel-second")?;
        let second_id = second_request.id.clone();
        let mut first = Box::pin(
            controller
                .exchange_owner
                .execute_identified(first_request.clone(), messages.clone()),
        );
        assert!(futures_util::poll!(first.as_mut()).is_pending());
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let state = controller
                    .exchange_owner
                    .command_status(first_id.clone())
                    .await?;
                if state.is_some_and(|record| matches!(record.state, CommandState::Active { .. })) {
                    break;
                }
                tokio::task::yield_now().await;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await??;
        drop(first);
        let mut second = Box::pin(
            controller
                .exchange_owner
                .execute_identified(second_request, messages.clone()),
        );
        assert!(futures_util::poll!(second.as_mut()).is_pending());
        let queued = controller
            .exchange_owner
            .command_status(second_id)
            .await?
            .context("second cancellation was not admitted")?;
        assert!(matches!(queued.state, CommandState::Queued { .. }));
        drop(gate);
        let second = tokio::time::timeout(Duration::from_secs(2), second).await??;
        assert!(matches!(
            second.outcome,
            CommandOutcome::Cancelled(CancellationOutcome::NoWork)
        ));
        let first = controller
            .exchange_owner
            .command_status(first_id)
            .await?
            .context("first command was lost")?;
        let first_result = first
            .completion()
            .context("first command did not complete")??;
        assert!(first_result.sequence < second.sequence);
        let before = controller
            .coordinator
            .submissions
            .snapshot()?
            .control_revision;
        let replay = controller
            .exchange_owner
            .execute_identified(first_request, messages)
            .await?;
        assert_eq!(replay.sequence, first_result.sequence);
        assert_eq!(replay.revision, first_result.revision);
        assert_eq!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .control_revision,
            before
        );
        controller.finish_exchange_owner().await?;
        Ok(())
    }

    /// Resolve ordinary input policy after earlier commands while retaining explicit stale-caller guards.
    #[tokio::test]
    async fn fresh_input_control_resolves_after_prior_fifo_cancellation() -> Result<()> {
        use crate::exchange::command::{CommandFailure, InputChange, InputCommand};
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let gate = controller
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let mut cancel = Box::pin(
            controller
                .exchange_owner
                .execute_identified(command("cancel-before-resume")?, messages.clone()),
        );
        assert!(futures_util::poll!(cancel.as_mut()).is_pending());
        let mut resume = Box::pin(
            controller
                .exchange_owner
                .execute_command(Command::Input(InputCommand::ResumeDelivery)),
        );
        assert!(futures_util::poll!(resume.as_mut()).is_pending());
        drop(gate);
        let (cancelled, resumed) = tokio::time::timeout(Duration::from_secs(2), async {
            tokio::join!(cancel, resume)
        })
        .await?;
        let cancelled = cancelled?;
        let resumed = resumed?;
        assert!(cancelled.sequence < resumed.sequence);
        assert!(matches!(
            resumed.outcome,
            CommandOutcome::Input {
                change: InputChange::DeliveryResumed,
                ..
            }
        ));
        let snapshot = controller.coordinator.submissions.snapshot()?;
        assert!(!snapshot.suspended);
        let stale = CommandRequest::from_input(crate::input::InputOperation {
            id: "stale-resume".into(),
            control_revision: 0,
            after_control: None,
            action: crate::input::InputAction::Resume,
        })?;
        assert!(matches!(
            controller
                .exchange_owner
                .execute_identified(stale, messages)
                .await,
            Err(crate::exchange::command::ExecuteCommandError::Failed(
                CommandFailure::Input(crate::input::InputRejection::ControlChanged)
            ))
        ));
        assert_eq!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .control_revision,
            snapshot.control_revision
        );
        controller.finish_exchange_owner().await?;
        Ok(())
    }

    /// Reject malformed cancellation options before admitting any owner command.
    #[tokio::test]
    async fn invalid_cancel_request_preserves_input_control() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path())?;
        let (messages, _output) = forge_protocol::outbound::channel();
        for params in [
            json!({"restore_prompt_if_no_output":"true"}),
            json!({"target":"child"}),
            json!({"operation_id":2}),
        ] {
            assert!(
                apply(
                    &controller,
                    &Request {
                        id: 1,
                        method: "session.turn.cancel".into(),
                        params
                    },
                    &messages
                )
                .await
                .is_err()
            );
        }
        assert_eq!(
            controller
                .coordinator
                .submissions
                .snapshot()?
                .control_revision,
            0
        );
        controller.finish_exchange_owner().await?;
        Ok(())
    }
}
