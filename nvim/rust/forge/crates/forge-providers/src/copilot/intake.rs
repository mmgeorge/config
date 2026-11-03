//! Keep Copilot observation and decoding alive across foreground result waiters.

use std::collections::VecDeque;
use std::sync::Arc;

use forge_provider_api::completion::ProviderCompletion;
use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionUpdate, ObservationFailure};
use forge_provider_api::{ProviderError, ProviderEvent, ProviderEventSink};
use github_copilot_sdk::SessionEvent;
use tokio::sync::{Mutex, mpsc, oneshot, watch};
use tokio::task::JoinHandle;

use super::event_decoder::CopilotEventDecoder;
use super::event_stream::SessionEventStream;
use super::handle::CopilotClient;

/// Retain decoder identity and output after the prompt's host result has returned.
pub(crate) struct TurnContext {
    pub(crate) decoder: CopilotEventDecoder,
    pub(crate) output: ProviderCompletion,
    pub(crate) sink: ProviderEventSink,
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct DeliveryGeneration(uuid::Uuid);

enum Command {
    Attach {
        generation: DeliveryGeneration,
        context: Arc<Mutex<TurnContext>>,
        reply: oneshot::Sender<Result<mpsc::UnboundedReceiver<SessionEvent>, ProviderError>>,
    },
    Release {
        generation: DeliveryGeneration,
    },
    Close {
        reply: oneshot::Sender<()>,
    },
}

enum IntakeState {
    Unbound(VecDeque<SessionEvent>),
    Active {
        generation: DeliveryGeneration,
        context: Arc<Mutex<TurnContext>>,
        sender: mpsc::UnboundedSender<SessionEvent>,
    },
    Retained(Arc<Mutex<TurnContext>>),
}

/// Own the session reader independently of each temporary foreground subscriber.
pub(crate) struct SessionIntake {
    commands: mpsc::UnboundedSender<Command>,
    failure: watch::Sender<Option<ObservationFailure>>,
    client: CopilotClient,
    task: Mutex<Option<JoinHandle<()>>>,
}

/// Release a foreground observer while retaining its decoder for late native events.
pub(crate) struct TurnReceiver {
    receiver: mpsc::UnboundedReceiver<SessionEvent>,
    commands: mpsc::UnboundedSender<Command>,
    generation: DeliveryGeneration,
    failure: watch::Sender<Option<ObservationFailure>>,
    client: CopilotClient,
}

impl SessionIntake {
    pub(crate) fn new(mut stream: SessionEventStream, client: CopilotClient) -> Self {
        let (commands, mut receive) = mpsc::unbounded_channel();
        let (failure, mut failures) = watch::channel(None);
        let failed = failure.clone();
        let native = client.clone();
        let task = tokio::spawn(async move {
            let mut state = IntakeState::Unbound(VecDeque::new());
            let mut retired: Vec<Arc<Mutex<TurnContext>>> = Vec::new();
            let mut closing = None;
            let result: Result<(), ProviderError> = async {
                loop {
                    tokio::select! {
                        changed = failures.changed() => {
                            if changed.is_err() { return Ok(()); }
                            if let Some(failure) = failures.borrow().clone() { return Err(ProviderError::Observation(failure)); }
                        },
                        command = receive.recv() => match command {
                            Some(Command::Attach { generation, context, reply }) => {
                                if matches!(state, IntakeState::Active { .. }) {
                                    let _ = reply.send(Err(ProviderError::Rejected { message: "Copilot session already has a foreground observer".into() }));
                                    continue;
                                }
                                let (sender, receiver) = mpsc::unbounded_channel();
                                if let IntakeState::Retained(previous) = &state && !Arc::ptr_eq(previous, &context) {
                                    retired.push(previous.clone());
                                }
                                if let IntakeState::Unbound(pending) = &mut state {
                                    for event in pending.drain(..) { sender.send(event).map_err(|_| ProviderError::Observation(ObservationFailure::Disconnected))?; }
                                }
                                state = IntakeState::Active { generation, context, sender };
                                let _ = reply.send(Ok(receiver));
                            },
                            Some(Command::Release { generation }) => {
                                if let IntakeState::Active { generation: active, context, .. } = &state && *active == generation {
                                    state = IntakeState::Retained(context.clone());
                                }
                            },
                            Some(Command::Close { reply }) => { closing = Some(reply); return Ok(()); },
                            None => return Ok(()),
                        },
                        event = stream.recv() => {
                            let event = event.map_err(ProviderError::from)?;
                            let mut original = None;
                            for retained in &retired {
                                if retained.lock().await.decoder.owns_event(&event) {
                                    if original.is_some() {
                                        return Err(ProviderError::Observation(ObservationFailure::OwnershipConflict));
                                    }
                                    original = Some(retained.clone());
                                }
                            }
                            if let Some(original) = original {
                                decode_retained(&original, event).await?;
                                continue;
                            }
                            match &mut state {
                                IntakeState::Unbound(pending) => {
                                    pending.push_back(event);
                                },
                                IntakeState::Active { sender, context, .. } => {
                                    match sender.send(event) {
                                        Ok(()) => {},
                                        Err(mpsc::error::SendError(event)) => {
                                            let retained = context.clone();
                                            decode_retained(&retained, event).await?;
                                            state = IntakeState::Retained(retained);
                                        },
                                    }
                                },
                                IntakeState::Retained(context) => decode_retained(context, event).await?,
                            }
                        },
                    }
                }
            }.await;
            let result = result.and(stream.close().await.map_err(ProviderError::from));
            if let Err(error) = result {
                let reason = match error {
                    ProviderError::Observation(reason) => reason,
                    error => ObservationFailure::Protocol {
                        message: error.to_string(),
                    },
                };
                failed.send_replace(Some(reason.clone()));
                native.force_stop();
                let context = match state {
                    IntakeState::Active {
                        context, sender, ..
                    } => {
                        // The prompt holds this context until its receiver closes.
                        drop(sender);
                        Some(context)
                    }
                    IntakeState::Retained(context) => Some(context),
                    IntakeState::Unbound(_) => None,
                };
                if let Some(context) = context {
                    let context = context.lock().await;
                    if let Ok(scope) = context.sink.invocation_scope()
                        && let Ok(observation) = ExecutionObservation::new(
                            scope.clone(),
                            ExecutionUpdate::ObservationLost {
                                failure: reason,
                                completion: Some(context.output.clone().into()),
                            },
                        )
                    {
                        let _ = context
                            .sink
                            .send_wait(ProviderEvent::Execution { observation })
                            .await;
                    }
                }
            }
            if let Some(reply) = closing {
                let _ = reply.send(());
            }
        });
        Self {
            commands,
            failure,
            client,
            task: Mutex::new(Some(task)),
        }
    }

    pub(crate) async fn attach(
        &self,
        context: Arc<Mutex<TurnContext>>,
    ) -> Result<TurnReceiver, ProviderError> {
        if let Some(failure) = self.failure.borrow().clone() {
            return Err(ProviderError::Observation(failure));
        }
        let generation = DeliveryGeneration(uuid::Uuid::new_v4());
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::Attach {
                generation,
                context,
                reply,
            })
            .map_err(|_| self.closed())?;
        let receiver = response.await.map_err(|_| self.closed())??;
        Ok(TurnReceiver {
            receiver,
            commands: self.commands.clone(),
            generation,
            failure: self.failure.clone(),
            client: self.client.clone(),
        })
    }

    pub(crate) async fn close(&self) -> Result<(), ProviderError> {
        let mut task = self.task.lock().await;
        let Some(pending) = task.as_mut() else {
            return Ok(());
        };
        let (reply, response) = oneshot::channel();
        if self.commands.send(Command::Close { reply }).is_ok() {
            let _ = response.await;
        }
        pending.await.map_err(|error| ProviderError::Protocol {
            message: format!("Copilot intake task failed: {error}"),
        })?;
        task.take();
        Ok(())
    }

    /// Report whether the shared native transport has already been stopped.
    pub(crate) fn client_stopped(&self) -> bool {
        self.client.is_stopped()
    }

    fn closed(&self) -> ProviderError {
        ProviderError::Observation(
            self.failure
                .borrow()
                .clone()
                .unwrap_or(ObservationFailure::Disconnected),
        )
    }
}

impl Drop for SessionIntake {
    fn drop(&mut self) {
        if let Some(task) = self.task.get_mut().take() {
            task.abort();
        }
    }
}

impl TurnReceiver {
    pub(crate) async fn recv(&mut self) -> Result<SessionEvent, ProviderError> {
        self.receiver.recv().await.ok_or_else(|| {
            ProviderError::Observation(
                self.failure
                    .borrow()
                    .clone()
                    .unwrap_or(ObservationFailure::Disconnected),
            )
        })
    }
}

impl Drop for TurnReceiver {
    fn drop(&mut self) {
        if self
            .commands
            .send(Command::Release {
                generation: self.generation,
            })
            .is_err()
        {
            self.failure
                .send_replace(Some(ObservationFailure::Disconnected));
            self.client.force_stop();
        }
    }
}

async fn decode_retained(
    context: &Arc<Mutex<TurnContext>>,
    event: SessionEvent,
) -> Result<(), ProviderError> {
    let mut context = context.lock().await;
    let TurnContext {
        decoder,
        output,
        sink,
    } = &mut *context;
    let previous = decoder.root_address();
    decoder.decode(&event, output, sink).await?;
    if event.agent_id.is_none()
        && event.event_type == "assistant.turn_start"
        && decoder.root_address() != previous
    {
        return Err(ProviderError::Observation(
            ObservationFailure::OwnershipConflict,
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{ExecutionOwner, InvocationScope, MessageUpdate};
    use serde_json::json;

    fn event(id: &str, kind: &str, data: serde_json::Value) -> SessionEvent {
        serde_json::from_value(
            json!({"id":id,"type":kind,"timestamp":"2026-11-01T00:00:00Z","data":data}),
        )
        .unwrap()
    }

    async fn fixture() -> (
        SessionIntake,
        mpsc::UnboundedSender<SessionEvent>,
        Arc<Mutex<TurnContext>>,
        forge_provider_api::ProviderEventStream,
    ) {
        let (read, _read_peer) = tokio::io::duplex(4096);
        let (write, _write_peer) = tokio::io::duplex(4096);
        let client = CopilotClient::new(
            github_copilot_sdk::Client::from_streams(read, write, std::env::temp_dir()).unwrap(),
        );
        let (sender, receiver) = mpsc::unbounded_channel();
        let task = tokio::spawn(std::future::pending::<anyhow::Result<()>>());
        let intake = SessionIntake::new(SessionEventStream::fixture(receiver, task), client);
        let scope = InvocationScope::new(ExecutionOwner::root("session"), "exchange");
        let (sink, mut observed) = forge_provider_api::events::channel(scope.clone());
        let mut decoder = CopilotEventDecoder::new(scope, ".".into());
        let mut output = ProviderCompletion {
            provider_session_id: Some("native".to_owned().try_into().unwrap()),
            ..Default::default()
        };
        decoder
            .decode(
                &event("start", "assistant.turn_start", json!({"turnId":"first"})),
                &mut output,
                &sink,
            )
            .await
            .unwrap();
        while observed.try_next().unwrap().is_some() {}
        let context = Arc::new(Mutex::new(TurnContext {
            decoder,
            output,
            sink,
        }));
        (intake, sender, context, observed)
    }

    /// Retain startup and foreground bursts while the prompt temporarily stops receiving.
    #[tokio::test]
    async fn bursts_survive_before_attach_and_while_foreground_is_paused() {
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            for attached in [false, true] {
                let (intake, sender, context, _observed) = fixture().await;
                let mut receiver = if attached {
                    Some(intake.attach(context.clone()).await.unwrap())
                } else {
                    None
                };
                let event_count = 16_384;
                for sequence in 0..event_count {
                    sender
                        .send(event(
                            &format!("burst-{sequence}"),
                            "assistant.message_delta",
                            json!({"messageId":"answer","deltaContent":sequence.to_string()}),
                        ))
                        .unwrap();
                }
                tokio::task::yield_now().await;
                assert!(intake.failure.borrow().is_none());
                let receiver = match receiver.as_mut() {
                    Some(receiver) => receiver,
                    None => receiver.insert(intake.attach(context).await.unwrap()),
                };
                for sequence in 0..event_count {
                    assert_eq!(
                        receiver.recv().await.unwrap().id,
                        format!("burst-{sequence}")
                    );
                }
                intake.close().await.unwrap();
                assert!(!intake.client_stopped());
            }
        })
        .await
        .unwrap();
    }

    /// Close the receiver before failure publication waits for the prompt's decoder lock.
    #[tokio::test]
    async fn observation_loss_releases_locked_prompt_before_publishing_failure() {
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            let (intake, sender, context, mut observed) = fixture().await;
            let mut receiver = intake.attach(context.clone()).await.unwrap();
            let active = context.lock().await;
            for sequence in 0..256 {
                sender.send(event(&format!("prefix-{sequence}"), "session.usage_info", json!({}))).unwrap();
            }
            while receiver.receiver.len() != 256 {
                tokio::task::yield_now().await;
            }
            intake.failure.send_replace(Some(ObservationFailure::Disconnected));
            for sequence in 0..256 {
                assert_eq!(receiver.recv().await.unwrap().id, format!("prefix-{sequence}"));
            }
            assert!(matches!(receiver.recv().await, Err(ProviderError::Observation(ObservationFailure::Disconnected))));
            assert!(intake.client_stopped());
            drop(active);
            assert!(matches!(observed.recv().await.unwrap(), Some(ProviderEvent::Execution { observation })
                if matches!(observation.update(), ExecutionUpdate::ObservationLost { failure: ObservationFailure::Disconnected, .. })));
            intake.close().await.unwrap();
            assert!(matches!(intake.attach(context).await, Err(ProviderError::Observation(ObservationFailure::Disconnected))));
        }).await.unwrap();
    }

    /// A known old message keeps its original sink after another invocation attaches.
    #[tokio::test]
    async fn retired_message_never_enters_new_invocation() {
        let (intake, sender, old, mut observed) = fixture().await;
        {
            let mut old = old.lock().await;
            let TurnContext {
                decoder,
                output,
                sink,
            } = &mut *old;
            decoder
                .decode(
                    &event(
                        "old-start",
                        "assistant.message_start",
                        json!({
                            "messageId":"old-message", "phase":"final_answer", "content":"first"
                        }),
                    ),
                    output,
                    sink,
                )
                .await
                .unwrap();
        }
        while observed.try_next().unwrap().is_some() {}
        drop(intake.attach(old).await.unwrap());
        let scope = InvocationScope::new(ExecutionOwner::root("session"), "next");
        let (sink, _next_events) = forge_provider_api::events::channel(scope.clone());
        let mut decoder = CopilotEventDecoder::new(scope, ".".into());
        let mut output = ProviderCompletion {
            provider_session_id: Some("native".to_owned().try_into().unwrap()),
            ..Default::default()
        };
        decoder
            .decode(
                &event(
                    "next-start",
                    "assistant.turn_start",
                    json!({"turnId":"next"}),
                ),
                &mut output,
                &sink,
            )
            .await
            .unwrap();
        let next = Arc::new(Mutex::new(TurnContext {
            decoder,
            output,
            sink,
        }));
        let mut receiver = intake.attach(next).await.unwrap();
        sender
            .send(event(
                "old-delta",
                "assistant.message_delta",
                json!({
                    "messageId":"old-message", "deltaContent":" last"
                }),
            ))
            .unwrap();
        let late = tokio::time::timeout(std::time::Duration::from_secs(1), observed.recv())
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        assert_eq!(
            late.scope().invocation().unwrap().exchange_id.as_str(),
            "exchange"
        );
        assert!(futures_util::poll!(std::pin::pin!(receiver.recv())).is_pending());
        drop(receiver);
        intake.close().await.unwrap();
    }

    /// A result observer can leave without discarding subsequent output from its native turn.
    #[tokio::test]
    async fn retained_context_observes_output_after_foreground_receiver_drops() {
        let (intake, sender, context, mut observed) = fixture().await;
        drop(intake.attach(context).await.unwrap());
        sender
            .send(event(
                "answer",
                "assistant.message",
                json!({"messageId":"answer","phase":"final_answer","content":"late answer"}),
            ))
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                if let Some(ProviderEvent::Message {
                    update: MessageUpdate::Completed { text },
                    ..
                }) = observed.recv().await.unwrap()
                    && text == "late answer"
                {
                    break;
                }
            }
        })
        .await
        .unwrap();
        intake.close().await.unwrap();
        intake.client.force_stop();
    }

    /// Uncorrelated native work does not borrow a settled invocation's authority.
    #[tokio::test]
    async fn unsolicited_idle_turn_fails_observation() {
        let (intake, sender, context, mut observed) = fixture().await;
        drop(intake.attach(context).await.unwrap());
        sender
            .send(event(
                "unowned-start",
                "assistant.turn_start",
                json!({"turnId":"second"}),
            ))
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                if let Some(ProviderEvent::Execution { observation }) =
                    observed.recv().await.unwrap()
                    && matches!(
                        observation.update(),
                        ExecutionUpdate::ObservationLost {
                            failure: ObservationFailure::OwnershipConflict,
                            ..
                        }
                    )
                {
                    break;
                }
            }
        })
        .await
        .unwrap();
        assert!(matches!(
            *intake.failure.borrow(),
            Some(ObservationFailure::OwnershipConflict)
        ));
        intake.close().await.unwrap();
    }
}
