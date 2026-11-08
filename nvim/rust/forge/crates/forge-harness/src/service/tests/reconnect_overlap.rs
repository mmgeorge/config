use super::super::{HarnessBroker, SessionController};
use super::bootstrap::initialize;
use crate::exchange::command::{
    CancelExchangeRequest, Command, CommandId, CommandRequest, InputControl,
};
use crate::input::{
    InputAction, InputDraft, InputIntent, InputMessage, InputOperation, InputPurpose, InputSnapshot,
};
use anyhow::{Context, Result};

/// Reconnect publishes current input even when its retained event page predates cancellation and edits.
#[tokio::test]
async fn owner_reconnect_captures_latest_input_after_held_cancellation_publications() -> Result<()>
{
    let root = tempfile::tempdir()?;
    let controller =
        SessionController::new(HarnessBroker::initialize(initialize(&root, "mock"))?).await?;
    let (sink, _output) = forge_protocol::outbound::channel();
    let scenario = async {
        let empty = controller.coordinator.submissions.snapshot()?;
        controller
            .exchange_owner
            .execute_identified(
                reconnect_input(
                    "initial-queued",
                    "Original queue",
                    InputIntent::Queue,
                    &empty,
                )?,
                sink.clone(),
            )
            .await?;
        let initial = controller
            .exchange_owner
            .reconnect_snapshot(None, std::num::NonZeroU16::new(1).unwrap())
            .await?;
        let (initial_state, held_publication) = initial.into_parts();
        let cursor = initial_state["owner_publication"]["high_water"].as_u64();
        let before = controller.coordinator.submissions.snapshot()?;
        for index in 0..3 {
            controller
                .exchange_owner
                .execute_identified(
                    CommandRequest {
                        id: CommandId::new(format!("reconnect-cancel-{index}"))?,
                        command: Command::CancelExchange(CancelExchangeRequest::default()),
                        input_control: InputControl::AtExecution,
                    },
                    sink.clone(),
                )
                .await?;
        }
        let cancelled = controller.coordinator.submissions.snapshot()?;
        assert!(cancelled.suspended);
        assert_eq!(cancelled.control_revision, before.control_revision + 3);
        assert_eq!(
            cancelled.control_operation_id.as_deref(),
            Some("reconnect-cancel-2")
        );
        controller
            .exchange_owner
            .execute_identified(
                reconnect_input(
                    "newer-queued",
                    "Newer queue survives replay",
                    InputIntent::Queue,
                    &cancelled,
                )?,
                sink.clone(),
            )
            .await?;
        let canonical = controller.coordinator.submissions.snapshot()?;
        assert!(canonical.revision > before.revision);
        assert_eq!(canonical.messages.len(), 2);
        drop(held_publication);

        let reconnect = controller
            .exchange_owner
            .reconnect_snapshot(cursor, std::num::NonZeroU16::new(1).unwrap())
            .await?;
        let (response, confirmation) = reconnect.into_parts();
        assert_eq!(response["input"], serde_json::to_value(&canonical)?);
        assert_eq!(
            response["owner_publication"]["more"], true,
            "fixture did not retain a page older than its authoritative input snapshot"
        );
        for record in response["owner_publication"]["events"]
            .as_array()
            .context("publication page missing")?
        {
            assert_eq!(
                record["event"]["payload"]["_publication"]["sequence"], record["sequence"],
                "retained page did not carry the live publication identity"
            );
        }
        controller
            .exchange_owner
            .execute_identified(
                CommandRequest {
                    id: CommandId::new("reconnect-cancel-2")?,
                    command: Command::CancelExchange(CancelExchangeRequest::default()),
                    input_control: InputControl::AtExecution,
                },
                sink.clone(),
            )
            .await?;
        controller
            .exchange_owner
            .confirm_reconnect_to(confirmation, Some(sink.clone()))
            .await?;
        assert_eq!(
            serde_json::to_value(controller.coordinator.submissions.snapshot()?)?,
            response["input"],
            "replayed cancellation or page acknowledgment mutated the current input snapshot"
        );

        let recovered: InputSnapshot = serde_json::from_value(response["input"].clone())?;
        controller
            .exchange_owner
            .execute_identified(
                reconnect_input(
                    "after-reconnect",
                    "Explicit input resumes delivery",
                    InputIntent::Auto,
                    &recovered,
                )?,
                sink.clone(),
            )
            .await?;
        let resumed = controller.coordinator.submissions.snapshot()?;
        assert!(
            !resumed.suspended,
            "current reconnect control did not authorize resumed delivery"
        );
        assert!(
            resumed
                .messages
                .iter()
                .any(|input| input.id == "newer-queued"
                    && input.text == "Newer queue survives replay"),
            "historical page replay replaced newer input"
        );
        assert!(
            resumed
                .messages
                .iter()
                .any(|input| input.id == "after-reconnect")
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    let cleanup = controller.finish_exchange_owner().await;
    scenario?;
    cleanup?;
    Ok(())
}

/// Build user input with the exact delivery-control authority returned by the owner.
fn reconnect_input(
    id: &str,
    text: &str,
    intent: InputIntent,
    snapshot: &InputSnapshot,
) -> Result<CommandRequest> {
    Ok(CommandRequest::from_input(InputOperation {
        id: format!("submit-{id}"),
        control_revision: snapshot.control_revision,
        after_control: snapshot.control_operation_id.clone(),
        action: InputAction::Submit {
            message: Box::new(InputMessage::queued(InputDraft {
                id: id.into(),
                text: text.into(),
                purpose: InputPurpose::User,
                attachments: Vec::new(),
                config: None,
                intent,
            })),
        },
    })?)
}
