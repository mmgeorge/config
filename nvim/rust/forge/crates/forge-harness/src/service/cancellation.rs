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

    /// Create an idle real session owner without starting a native process or automatic input work.
    async fn controller(root: &std::path::Path) -> Result<Arc<SessionController>> {
        let initialize: InitializeRequest = serde_json::from_value(json!({
            "workspace":root,"data_root":root.join("data"),"client_id":"owner-cancel",
            "provider":{"kind":"mock","command":[]},
        }))?;
        SessionController::new(HarnessBroker::initialize(initialize)?).await
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

    /// Retract a prompt that a cancel overtook before any owner claimed it.
    #[tokio::test]
    async fn restoring_cancel_retracts_an_unclaimed_submission() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let controller = controller(fixture.path()).await?;
        controller
            .coordinator
            .submissions
            .apply(serde_json::from_value(serde_json::json!({
                "id":"submit-unclaimed","action":{"kind":"submit","message":{
                    "id":"unclaimed","text":"Write a long story","intent":"auto"}}
            }))?)?;
        let follow_up = controller
            .coordinator
            .submissions
            .apply(serde_json::from_value(serde_json::json!({
                "id":"queue-follow-up","action":{"kind":"submit","message":{
                    "id":"follow-up","text":"Then summarize it","intent":"queue"}}
            }))?);
        follow_up?;
        let (messages, _output) = forge_protocol::outbound::channel();
        let mut request = command("cancel-unclaimed")?;
        request.command = Command::CancelExchange(CancelExchangeRequest {
            input: CancelledInputPolicy::RestoreIfUnconsumed,
        });
        controller
            .exchange_owner
            .execute_identified(request, messages)
            .await?;
        let snapshot = controller.coordinator.submissions.snapshot()?;
        assert!(
            snapshot
                .messages
                .iter()
                .all(|message| message.id != "unclaimed"),
            "the overtaken prompt stayed stranded in the suspended queue"
        );
        assert!(
            snapshot
                .messages
                .iter()
                .any(|message| message.id == "follow-up"),
            "a deliberately queued follow-up must survive the cancel"
        );
        Ok(())
    }
}
