//! Keep Copilot observation and decoding alive across foreground result waiters.

use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use forge_provider_api::completion::ProviderCompletion;
use forge_provider_api::lifecycle::ObservationFailure;
use forge_provider_api::{
    AdapterActivityWriter, ProviderError, ProviderFailureKind, ProviderOperation,
};
use github_copilot_sdk::SessionEvent;
use tokio::sync::{Mutex, mpsc, oneshot, watch};
use tokio::task::JoinHandle;

use super::client_generation::ClientGeneration;
use super::event_decoder::CopilotEventDecoder;
use super::event_stream::{ObservationTimes, SessionEventStream};

/// Retain decoder identity and output after the prompt's host result has returned.
pub(crate) struct TurnContext {
    pub(crate) decoder: CopilotEventDecoder,
    pub(crate) output: ProviderCompletion,
    pub(crate) sink: AdapterActivityWriter,
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct DeliveryGeneration(uuid::Uuid);

enum Command {
    Bind {
        writer: Box<AdapterActivityWriter>,
        reply: oneshot::Sender<Result<(), ProviderError>>,
    },
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

enum TurnReceiveState {
    Live,
    Failed(ObservationFailure),
}

/// Own the session reader independently of each temporary foreground subscriber.
pub(crate) struct SessionIntake {
    commands: mpsc::UnboundedSender<Command>,
    failure: watch::Receiver<Option<ObservationFailure>>,
    client: ClientGeneration,
    task: Mutex<Option<JoinHandle<()>>>,
    observation_times: ObservationTimes,
}

/// Release a foreground observer while retaining its decoder for late native events.
pub(crate) struct TurnReceiver {
    receiver: mpsc::UnboundedReceiver<SessionEvent>,
    commands: mpsc::UnboundedSender<Command>,
    generation: DeliveryGeneration,
    failure: watch::Receiver<Option<ObservationFailure>>,
    state: TurnReceiveState,
    observation_times: ObservationTimes,
}

impl SessionIntake {
    pub(crate) fn new(mut stream: SessionEventStream, client: ClientGeneration) -> Self {
        let (commands, mut receive) = mpsc::unbounded_channel();
        let failure = client.failures();
        let mut failures = failure.clone();
        let native = client.clone();
        let observation_times = stream.observation_times();
        let observations = observation_times.clone();
        let task = tokio::spawn(async move {
            let mut state = IntakeState::Unbound(VecDeque::new());
            let mut retired: Vec<Arc<Mutex<TurnContext>>> = Vec::new();
            let mut closing = None;
            let mut binding_writer: Option<AdapterActivityWriter> = None;
            let mut session_errors = HashMap::new();
            let result: Result<(), ProviderError> = async {
                loop {
                    if let Some(failure) = failures.borrow().clone() { return Err(ProviderError::Observation(failure)); }
                    tokio::select! {
                        changed = failures.changed() => {
                            if changed.is_err() { return Ok(()); }
                            if let Some(failure) = failures.borrow().clone() { return Err(ProviderError::Observation(failure)); }
                        },
                        command = receive.recv() => match command {
                            Some(Command::Bind { writer, reply }) => {
                                if let IntakeState::Unbound(pending) = &mut state {
                                    let mut retained = VecDeque::new();
                                    while let Some(event) = pending.pop_front() {
                                        if event.event_type == "session.error" && event.agent_id.is_none() {
                                            publish_session_error(&writer, &mut session_errors, &event).await?;
                                        } else { retained.push_back(event); }
                                    }
                                    *pending = retained;
                                }
                                binding_writer = Some(*writer);
                                let _ = reply.send(Ok(()));
                            },
                            Some(Command::Attach { generation, context, reply }) => {
                                if matches!(state, IntakeState::Active { .. }) {
                                    let _ = reply.send(Err(native_failure(forge_provider_api::ProviderOperation::TurnStart, forge_provider_api::ProviderFailureKind::Rejected)));
                                    continue;
                                }
                                let (sender, receiver) = mpsc::unbounded_channel();
                                if let IntakeState::Retained(previous) = &state && !Arc::ptr_eq(previous, &context) {
                                    retired.push(previous.clone());
                                }
                                if let IntakeState::Unbound(pending) = &mut state {
                                    for event in pending.drain(..) {
                                        sender.send(event).map_err(|_| ProviderError::Observation(ObservationFailure::Disconnected))?;
                                    }
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
                            let event = event.map_err(|error| error.downcast::<ProviderError>().unwrap_or_else(|_| native_failure(ProviderOperation::ThreadClose, ProviderFailureKind::Transport)))?;
                            if event.event_type == "session.error" && event.agent_id.is_none()
                                && let Some(writer) = &binding_writer {
                                publish_session_error(writer, &mut session_errors, &event).await?;
                                continue;
                            }
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
                                decode_retained(&original, event, &observations).await?;
                                continue;
                            }
                            match &mut state {
                                IntakeState::Unbound(pending) => {
                                    pending.push_back(event);
                                },
                                IntakeState::Active { sender, context, .. } => {
                                    if let Err(error) = sender.send(event) {
                                        let retained = context.clone();
                                        decode_retained(&retained, error.0, &observations).await?;
                                        state = IntakeState::Retained(retained);
                                    }
                                },
                                IntakeState::Retained(context) => decode_retained(context, event, &observations).await?,
                            }
                        },
                    }
                }
            }.await;
            let result = result.and(stream.close().await.map_err(|_| {
                native_failure(
                    ProviderOperation::ThreadClose,
                    ProviderFailureKind::Transport,
                )
            }));
            let current = match state {
                IntakeState::Active {
                    context, sender, ..
                } => {
                    drop(sender);
                    Some(context)
                }
                IntakeState::Retained(context) => Some(context),
                IntakeState::Unbound(_) => None,
            };
            if let Err(error) = result {
                let reason = match error {
                    ProviderError::Observation(reason) => reason,
                    _ => ObservationFailure::Protocol,
                };
                native.fail(reason);
                let reason = native
                    .failure()
                    .expect("generation failure is sticky after publication");
                if let Some(context) = &current {
                    context.lock().await.sink.fail(
                        forge_provider_api::events::EventDeliveryError::Observation(reason),
                    );
                }
            }
            if let Some(context) = current {
                context.lock().await.decoder.close_children().await;
            }
            for context in retired {
                context.lock().await.decoder.close_children().await;
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
            observation_times,
        }
    }

    /// Publish thread errors immediately even before the first prompt creates a decoder.
    pub(crate) async fn bind(&self, writer: AdapterActivityWriter) -> Result<(), ProviderError> {
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::Bind {
                writer: Box::new(writer),
                reply,
            })
            .map_err(|_| self.closed())?;
        response.await.map_err(|_| self.closed())?
    }

    pub(crate) async fn attach(
        &self,
        context: Arc<Mutex<TurnContext>>,
    ) -> Result<TurnReceiver, ProviderError> {
        if let Some(failure) = self.failure.borrow().clone() {
            return Err(ProviderError::Observation(failure));
        }
        let generation = DeliveryGeneration(uuid::Uuid::new_v4());
        context.lock().await.sink.invocation_scope()?;
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
            state: TurnReceiveState::Live,
            observation_times: self.observation_times.clone(),
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
        pending.await.map_err(|_error| {
            super::error::native_failure(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Protocol,
            )
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
    /// Get first SDK receipt time for an event held behind native send acknowledgement.
    pub(crate) fn observed_at(&self, event: &SessionEvent) -> i64 {
        self.observation_times.observed_at(event)
    }

    /// Preserve sticky observation loss during an auxiliary history read.
    pub(crate) fn ensure_healthy(&self) -> Result<(), ProviderError> {
        match self.failure.borrow().clone() {
            Some(failure) => Err(ProviderError::Observation(failure)),
            None => Ok(()),
        }
    }

    /// Fence new writes on loss, drain the accepted prefix, then preserve its first typed failure.
    pub(crate) async fn recv(&mut self) -> Result<SessionEvent, ProviderError> {
        let mut failure = self.failure.clone();
        loop {
            if let TurnReceiveState::Failed(reason) = &self.state {
                return self
                    .receiver
                    .try_recv()
                    .map_err(|_| ProviderError::Observation(reason.clone()));
            }
            let reported = failure.borrow().clone();
            if let Some(reason) = reported {
                self.receiver.close();
                self.state = TurnReceiveState::Failed(reason);
                continue;
            }
            tokio::select! {
                biased;
                changed = failure.changed() => {
                    if changed.is_err() {
                        self.receiver.close();
                        self.state = TurnReceiveState::Failed(ObservationFailure::Disconnected);
                    }
                }
                event = self.receiver.recv() => match event {
                    Some(event) => return Ok(event),
                    None => {
                        let reason = failure.borrow().clone().unwrap_or(ObservationFailure::Disconnected);
                        self.state = TurnReceiveState::Failed(reason);
                    }
                }
            }
        }
    }
}

impl Drop for TurnReceiver {
    fn drop(&mut self) {
        let _ = self.commands.send(Command::Release {
            generation: self.generation,
        });
    }
}

async fn decode_retained(
    context: &Arc<Mutex<TurnContext>>,
    event: SessionEvent,
    observations: &ObservationTimes,
) -> Result<(), ProviderError> {
    let mut context = context.lock().await;
    let TurnContext {
        decoder,
        output,
        sink,
    } = &mut *context;
    if event.agent_id.is_none()
        && event.event_type == "assistant.turn_start"
        && !decoder.owns_event(&event)
        && decoder
            .root_address()
            .is_none_or(|address| address.turn_id.as_str() != event.id)
    {
        return Err(ProviderError::Observation(
            ObservationFailure::UnexpectedExecution,
        ));
    }
    decoder
        .decode_at(&event, observations.observed_at(&event), output, sink)
        .await?;
    Ok(())
}

/// Deduplicate session errors without assigning them to whichever turn is currently visible.
async fn publish_session_error(
    writer: &AdapterActivityWriter,
    observed: &mut HashMap<String, serde_json::Value>,
    event: &SessionEvent,
) -> Result<(), ProviderError> {
    if let Some(previous) = observed.get(&event.id) {
        if previous != &event.data {
            return Err(ProviderError::Observation(ObservationFailure::Protocol));
        }
        return Ok(());
    }
    if event.id.trim().is_empty() {
        return Err(ProviderError::Observation(ObservationFailure::Protocol));
    }
    writer
        .send_wait(super::error::session_error(
            writer.scope().thread_id().clone(),
            &event.data,
        ))
        .await?;
    observed.insert(event.id.clone(), event.data.clone());
    Ok(())
}

fn native_failure(
    operation: forge_provider_api::ProviderOperation,
    kind: forge_provider_api::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{
        BindingId, InvocationScope, ItemCompletion, ItemEvent, ProviderEvent,
    };
    use github_copilot_sdk::Client;
    use serde_json::json;

    /// Construct native envelopes without contacting a billable provider.
    fn event(id: &str, kind: &str, contents: serde_json::Value) -> SessionEvent {
        serde_json::from_value(
            json!({"id":id,"type":kind,"timestamp":"2026-11-05T00:00:00Z","data":contents}),
        )
        .unwrap()
    }

    /// Create an independent retained decoder on the same host binding.
    async fn context(
        label: &str,
    ) -> (
        Arc<Mutex<TurnContext>>,
        forge_provider_api::ProviderEventStream,
    ) {
        let scope =
            InvocationScope::new(forge_provider_api::testing::root_thread("session"), label);
        let (sink, stream) =
            forge_provider_api::events::channel(BindingId::new("intake").unwrap(), scope.clone());
        let sink = sink.activity_writer();
        let mut decoder = CopilotEventDecoder::new(scope, ".".into());
        let mut output = ProviderCompletion {
            provider_session_id: Some("native".try_into().unwrap()),
            ..Default::default()
        };
        decoder
            .decode(
                &event(
                    &format!("start-{label}"),
                    "assistant.turn_start",
                    json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await
            .unwrap();
        (
            Arc::new(Mutex::new(TurnContext {
                decoder,
                output,
                sink,
            })),
            stream,
        )
    }

    /// Create a session reader whose producer remains owned until explicit close.
    async fn fixture() -> (SessionIntake, mpsc::Sender<SessionEvent>) {
        let (read, _read_peer) = tokio::io::duplex(4096);
        let (write, _write_peer) = tokio::io::duplex(4096);
        let client = Client::from_streams(read, write, std::env::temp_dir()).unwrap();
        let (sender, receiver) = mpsc::channel(32);
        let producer = tokio::spawn(std::future::pending::<anyhow::Result<()>>());
        (
            SessionIntake::new(
                SessionEventStream::fixture(receiver, producer),
                ClientGeneration::new(client),
            ),
            sender,
        )
    }

    /// Native session errors remain visible before any foreground turn has been admitted.
    #[tokio::test]
    async fn unbound_session_error_publishes_without_starting_a_turn() -> anyhow::Result<()> {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (intake, sender) = fixture().await;
            let owner = forge_provider_api::testing::root_thread("idle-error");
            let (sink, mut events) = forge_provider_api::events::channel(BindingId::new("idle-error")?, forge_provider_api::EventScope::Binding { thread_id: owner.clone() });
            sender.send(event("idle-error", "session.error", json!({"errorType":"query","statusCode":503,"message":"private-native-failure"}))).await?;
            intake.bind(sink.activity_writer()).await?;
            let observed = events.recv().await?.unwrap().event;
            assert!(matches!(&observed, ProviderEvent::Thread { thread_id, event: forge_provider_api::ThreadEvent::Error { .. } } if thread_id == &owner.clone()));
            assert!(!serde_json::to_string(&observed)?.contains("private-native-failure"));
            assert!(events.try_next()?.is_none());
            assert!(intake.failure.borrow().is_none());
            intake.close().await?;
            Ok::<(), anyhow::Error>(())
        }).await?
    }

    /// A delayed foreground consumer retains every native event beyond the old arbitrary limit.
    #[tokio::test]
    async fn paused_bursts_over_capacity_retain_native_order() -> anyhow::Result<()> {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            for attached in [false, true] {
                let (intake, sender) = fixture().await;
                let (context, _events) = context("burst").await;
                let mut receiver = if attached {
                    Some(intake.attach(context.clone()).await?)
                } else {
                    None
                };
                let count = forge_protocol::MAX_PENDING_FRAMES + 37;
                for ordinal in 0..count {
                    sender
                        .send(event(
                            &ordinal.to_string(),
                            "assistant.streaming_delta",
                            json!({}),
                        ))
                        .await?;
                }
                if receiver.is_none() {
                    receiver = Some(intake.attach(context).await?);
                }
                let receiver = receiver.as_mut().unwrap();
                for ordinal in 0..count {
                    assert_eq!(receiver.recv().await?.id, ordinal.to_string());
                }
                assert!(intake.failure.borrow().is_none());
                intake.close().await?;
            }
            Ok::<(), anyhow::Error>(())
        })
        .await?
    }

    /// Native loss wakes a foreground owner holding the decoder and retains its queued prefix.
    #[tokio::test]
    async fn active_context_failure_drains_prefix_without_waiting_for_decoder_lock()
    -> anyhow::Result<()> {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (intake, sender) = fixture().await;
            let (context, mut observed) = context("loss").await;
            let mut receiver = intake.attach(context.clone()).await?;
            let mut owned = context.lock().await;
            sender.send(event("answer", "assistant.message", json!({"messageId":"answer","phase":"final_answer","content":"prefix"}))).await?;
            while receiver.receiver.is_empty() { tokio::task::yield_now().await; }
            intake.client.fail(ObservationFailure::Disconnected);
            let prefix = receiver.recv().await?;
            let TurnContext { decoder, output, sink } = &mut *owned;
            decoder.decode(&prefix, output, sink).await?;
            assert!(matches!(receiver.recv().await, Err(ProviderError::Observation(ObservationFailure::Disconnected))));
            assert!(!intake.task.lock().await.as_ref().unwrap().is_finished());
            drop(owned);
            drop(receiver);
            let mut preserved = false;
            while let Ok(Some(event)) = observed.try_next() {
                preserved |= matches!(event.event, ProviderEvent::Item { event: ItemEvent::Completed(ItemCompletion::AgentMessage(completion)), .. } if completion.text == "prefix");
            }
            // The stream may be fenced before a consumer drains it; native output remains in the retained decoder.
            assert!(preserved || context.lock().await.decoder.owns_event(&prefix));
            intake.close().await?;
            Ok::<(), anyhow::Error>(())
        }).await?
    }

    /// Late output retains its original item after a successor foreground attaches.
    #[tokio::test]
    async fn late_output_after_reattachment_keeps_original_turn() -> anyhow::Result<()> {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (intake, sender) = fixture().await;
            let (first, mut first_events) = context("first").await;
            let mut initial = intake.attach(first.clone()).await?;
            sender.send(event("message", "assistant.message_start", json!({"messageId":"original"}))).await?;
            let admitted = initial.recv().await?;
            { let mut context = first.lock().await; let TurnContext { decoder, output, sink } = &mut *context; decoder.decode(&admitted, output, sink).await?; }
            let original = first.lock().await.decoder.root_address().unwrap();
            drop(initial);
            let (second, _second_events) = context("second").await;
            let mut next = intake.attach(second).await?;
            sender.send(event("late", "assistant.message", json!({"messageId":"original","content":"late original","phase":"final_answer"}))).await?;
            let mut found = false;
            while !found {
                if let Some(bound) = first_events.recv().await?
                    && let ProviderEvent::Item { thread_id, turn_id, event: ItemEvent::Completed(ItemCompletion::AgentMessage(completion)), .. } = bound.event {
                        assert_eq!(thread_id, original.thread_id); assert_eq!(turn_id, original.turn_id); assert_eq!(completion.text, "late original"); found = true;
                    }
            }
            assert!(next.receiver.try_recv().is_err());
            intake.close().await?;
            Ok::<(), anyhow::Error>(())
        }).await?
    }

    /// A retained reader rejects new foreground work instead of reopening its retired turn.
    #[tokio::test]
    async fn retained_turn_rejects_unowned_native_start() -> anyhow::Result<()> {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (intake, sender) = fixture().await;
            let (context, _events) = context("retired").await;
            let receiver = intake.attach(context).await?;
            drop(receiver);
            sender
                .send(event(
                    "foreign-start",
                    "assistant.turn_start",
                    json!({"turnId":"0"}),
                ))
                .await?;
            let mut failures = intake.failure.clone();
            failures.wait_for(Option::is_some).await?;
            assert!(matches!(
                *failures.borrow(),
                Some(ObservationFailure::UnexpectedExecution)
            ));
            intake.close().await?;
            Ok::<(), anyhow::Error>(())
        })
        .await?
    }
}
