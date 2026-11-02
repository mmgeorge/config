//! Run explicit completion checks and repair waits outside the session broker lock.

use super::*;
use crate::broker::{AutomaticCheckAction, PriorFinalizationAction};

/// Drive one explicit check request and always release its broker publication sink.
pub(super) async fn drive(
    controller: &SessionController,
    mut broker: MutexGuard<'_, HarnessBroker>,
    request: Request,
    sink: crate::broker::publication::SessionUpdateSink,
    messages: &MessageSender,
) -> crate::broker::DispatchResult {
    broker.trace_foreground_request(&request);
    let initial = broker.begin_check_completion(sink.clone());
    drop(broker);
    let result = async {
        let (mut transaction, mut action) = initial?;
        loop {
            action = match action {
                AutomaticCheckAction::Finish((value, events)) => {
                    sink.flush().await?;
                    return Ok(crate::broker::DispatchResult {
                        response: Response::success(request.id, value)?,
                        event: events,
                    });
                }
                AutomaticCheckAction::Refresh(job) => {
                    let result = job.run().await?;
                    apply_state(controller, &sink, |broker| {
                        broker.apply_automatic_check_refresh(
                            transaction
                                .as_mut()
                                .context("automatic check transaction is missing")?,
                            result,
                        )
                    })
                    .await?
                }
                AutomaticCheckAction::Run(job) => {
                    let result = job.run().await?;
                    apply_state(controller, &sink, |broker| {
                        broker.apply_automatic_check_result(
                            transaction
                                .as_mut()
                                .context("automatic check transaction is missing")?,
                            result,
                        )
                    })
                    .await?
                }
                AutomaticCheckAction::Publish { update, result } => {
                    sink.send_wait(update).await?;
                    AutomaticCheckAction::Finish(result)
                }
                AutomaticCheckAction::Revalidate => {
                    let mut events = settle_reopened(controller, &sink).await?;
                    for event in events.drain(..) {
                        messages.send_event(event).await?;
                    }
                    let action = apply_state(controller, &sink, |broker| {
                        broker.revalidate_automatic_checks(
                            transaction
                                .as_mut()
                                .context("automatic check transaction is missing")?,
                        )
                    })
                    .await?;
                    if matches!(&action, AutomaticCheckAction::Revalidate) {
                        controller.observer.session_events().notified().await?;
                    }
                    action
                }
                AutomaticCheckAction::Repair {
                    prompt,
                    change_owner,
                } => {
                    let repair = apply_state(controller, &sink, |broker| {
                        broker.begin_check_repair(request.id, prompt, change_owner, sink.clone())
                    })
                    .await?;
                    let repaired = drive_owned_foreground(controller, repair, messages).await?;
                    if repaired.response.error().is_some() {
                        return Ok(repaired);
                    }
                    let value = repaired
                        .response
                        .result()
                        .cloned()
                        .context("repair response has no result")?;
                    apply_state(controller, &sink, |broker| {
                        broker.apply_automatic_check_repair(
                            transaction
                                .as_mut()
                                .context("automatic check transaction is missing")?,
                            (value, repaired.event),
                        )
                    })
                    .await?
                }
            };
        }
    }
    .await;
    let mut broker = controller.broker.lock().await;
    let result = broker.finish_check_completion(request.id, result);
    broker
        .with_publication_sink(&sink, |broker| {
            Ok(broker.finish_owned_external_request(
                request.id,
                RuntimeMethod::SessionCheckComplete,
                result,
            ))
        })
        .unwrap_or_else(|error| crate::broker::DispatchResult {
            response: Response::failure(
                request.id,
                "request_failed",
                crate::storage::error_message(&error),
            ),
            event: Vec::new(),
        })
}

/// Collect reopened work and finalize its original exchange before recapturing check authority.
async fn settle_reopened(
    controller: &SessionController,
    sink: &crate::broker::publication::SessionUpdateSink,
) -> Result<Vec<SessionEvent>> {
    let mut events = Vec::new();
    loop {
        events.extend(apply_state(controller, sink, |broker| broker.drain_session_events()).await?);
        let Some(mut state) =
            apply_state(controller, sink, |broker| broker.begin_prior_finalization()).await?
        else {
            return Ok(events);
        };
        let settled: Result<()> = async {
            let mut action = apply_state(controller, sink, |broker| {
                broker.advance_prior_finalization(&mut state, &mut events)
            })
            .await?;
            loop {
                action = match action {
                    PriorFinalizationAction::Ready => return Ok(()),
                    PriorFinalizationAction::Wait(job) => {
                        job.run().await?;
                        apply_state(controller, sink, |broker| {
                            broker.advance_prior_finalization(&mut state, &mut events)
                        })
                        .await?
                    }
                    PriorFinalizationAction::Initial(job) => {
                        let result = job.run().await;
                        apply_state(controller, sink, |broker| {
                            broker.apply_prior_initial(&mut state, result)?;
                            broker.advance_prior_finalization(&mut state, &mut events)
                        })
                        .await?
                    }
                    PriorFinalizationAction::Final(job) => {
                        let result = job.run().await;
                        apply_state(controller, sink, |broker| {
                            broker.apply_prior_final(&mut state, result, &mut events)
                        })
                        .await?
                    }
                };
            }
        }
        .await;
        if let Err(error) = settled {
            if let Err(cleanup) = apply_state(controller, sink, |broker| {
                broker.abort_prior_finalization(&mut state, &error)
            })
            .await
            {
                return Err(
                    error.context(format!("prior finalization cleanup failed: {cleanup:#}"))
                );
            }
            return Err(error);
        }
    }
}

/// Capture each check phase's output under its own sink before releasing the broker.
async fn apply_state<Output>(
    controller: &SessionController,
    sink: &crate::broker::publication::SessionUpdateSink,
    apply: impl FnOnce(&mut HarnessBroker) -> Result<Output>,
) -> Result<Output> {
    controller
        .broker
        .lock()
        .await
        .with_publication_sink(sink, apply)
}
