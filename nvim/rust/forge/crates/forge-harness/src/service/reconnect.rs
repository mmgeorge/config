//! Return a coherent owner snapshot and acknowledge its retained publication page only after output admission.

use super::*;
use std::num::NonZeroU16;

/// Send one bounded retained page without allowing partial output to acknowledge unseen events.
pub(super) async fn respond(
    controller: &SessionController,
    request: Request,
    sink: &MessageSender,
) -> Result<()> {
    let cursor = match request.params.get("publication_cursor") {
        None | Some(Value::Null) => None,
        Some(value) => match value.as_u64().filter(|value| *value > 0) {
            Some(cursor) => Some(cursor),
            None => {
                sink.send_response(Response::failure(
                    request.id,
                    "reconnect_invalid_cursor",
                    "Publication cursor must be a positive integer",
                ))
                .await?;
                return Ok(());
            }
        },
    };
    let limit = NonZeroU16::new(128).expect("reconnect page limit is positive");
    let snapshot = match controller
        .exchange_owner
        .reconnect_snapshot(cursor, limit)
        .await
    {
        Ok(snapshot) => snapshot,
        Err(error) => {
            sink.send_response(Response::failure(
                request.id,
                error.code(),
                error.to_string(),
            ))
            .await?;
            return Ok(());
        }
    };
    let (payload, confirmation) = snapshot.into_parts();
    let acknowledgment = json!({
        "receipt_id": payload["owner_publication"]["receipt_id"],
        "runtime_epoch": payload["runtime_epoch"],
        "after": payload["owner_publication"]["after"],
        "through": payload["owner_publication"]["through"],
    });
    sink.send_response(Response::success(request.id, payload)?)
        .await?;
    // The response has already completed; a lost confirmation must never produce a second RPC reply.
    if let Err(error) = controller
        .exchange_owner
        .confirm_reconnect_to(confirmation, Some(sink.clone()))
        .await
    {
        controller.trace.record(
            controller.observer.session_id(),
            "exchange.reconnect.confirmation_failed",
            json!({
                "request_id": request.id, "code": error.code(),
            }),
        );
        return Ok(());
    }
    if let Err(error) = sink
        .send_event(SessionEvent {
            session_id: controller.observer.session_id().to_owned(),
            event: "publication_confirmed".into(),
            payload: acknowledgment,
        })
        .await
    {
        controller.trace.record(
            controller.observer.session_id(),
            "exchange.reconnect.acknowledgment_lost",
            json!({"request_id":request.id,"error":error.to_string()}),
        );
    }
    Ok(())
}
