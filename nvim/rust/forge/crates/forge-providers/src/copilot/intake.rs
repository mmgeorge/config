//! Keep Copilot observation and decoding alive across foreground result waiters.

use std::collections::VecDeque;
use std::sync::Arc;

use forge_provider_api::completion::ProviderCompletion;
use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionUpdate, ObservationFailure};
use forge_provider_api::{
    ProviderError, ProviderEvent, ProviderEventSink, ProviderFailureKind, ProviderOperation,
};
use github_copilot_sdk::SessionEvent;
use tokio::sync::{Mutex, mpsc, oneshot, watch};
use tokio::task::JoinHandle;

use super::client_generation::ClientGeneration;
use super::event_decoder::CopilotEventDecoder;
use super::event_stream::{IntakeQueueStage, QueueDiagnostics, SessionEventStream};

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
        scope: forge_provider_api::InvocationScope,
        reply: oneshot::Sender<Result<mpsc::Receiver<SessionEvent>, ProviderError>>,
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
        sender: mpsc::Sender<SessionEvent>,
        scope: forge_provider_api::InvocationScope,
    },
    Retained(Arc<Mutex<TurnContext>>),
}

enum TurnReceiveState {
    Live,
    Failed(ObservationFailure),
}

/// Own the session reader independently of each temporary foreground subscriber.
pub(crate) struct SessionIntake {
    commands: mpsc::Sender<Command>,
    failure: watch::Receiver<Option<ObservationFailure>>,
    client: ClientGeneration,
    task: Mutex<Option<JoinHandle<()>>>,
    diagnostics: QueueDiagnostics,
}

/// Release a foreground observer while retaining its decoder for late native events.
pub(crate) struct TurnReceiver {
    receiver: mpsc::Receiver<SessionEvent>,
    commands: mpsc::Sender<Command>,
    generation: DeliveryGeneration,
    failure: watch::Receiver<Option<ObservationFailure>>,
    client: ClientGeneration,
    state: TurnReceiveState,
    scope: forge_provider_api::InvocationScope,
    diagnostics: QueueDiagnostics,
}

impl SessionIntake {
    pub(crate) fn new(mut stream: SessionEventStream, client: ClientGeneration) -> Self {
        let (commands, mut receive) = mpsc::channel(16);
        let failure = client.failures();
        let mut failures = failure.clone();
        let native = client.clone();
        let diagnostics = stream.queue_diagnostics();
        let queue_diagnostics = diagnostics.clone();
        let task = tokio::spawn(async move {
            let mut state = IntakeState::Unbound(VecDeque::new());
            let mut retired: Vec<Arc<Mutex<TurnContext>>> = Vec::new();
            let mut closing = None;
            let result: Result<(), ProviderError> = async {
                loop {
                    if let Some(failure) = failures.borrow().clone() { return Err(ProviderError::Observation(failure)); }
                    tokio::select! {
                        changed = failures.changed() => {
                            if changed.is_err() { return Ok(()); }
                            if let Some(failure) = failures.borrow().clone() { return Err(ProviderError::Observation(failure)); }
                        },
                        command = receive.recv() => match command {
                            Some(Command::Attach { generation, context, scope, reply }) => {
                                if matches!(state, IntakeState::Active { .. }) {
                                    let _ = reply.send(Err(native_failure(forge_provider_api::ProviderOperation::AdmitInput, forge_provider_api::ProviderFailureKind::Rejected)));
                                    continue;
                                }
                                let (sender, receiver) = mpsc::channel(forge_protocol::MAX_PENDING_FRAMES);
                                if let IntakeState::Retained(previous) = &state && !Arc::ptr_eq(previous, &context) {
                                    retired.push(previous.clone());
                                }
                                if let IntakeState::Unbound(pending) = &mut state {
                                    for event in pending.drain(..) { sender.try_send(event).map_err(|error| {
                                        queue_diagnostics.lost(IntakeQueueStage::Attachment, sender.max_capacity() - sender.capacity(), sender.max_capacity(), Some(&scope), Some(&error.into_inner()));
                                        ProviderError::Observation(ObservationFailure::Overflow)
                                    })?; }
                                }
                                state = IntakeState::Active { generation, context, sender, scope };
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
                            let event = event.map_err(|_| native_failure(ProviderOperation::CollectEventReader, ProviderFailureKind::Transport))?;
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
                                    if pending.len() == forge_protocol::MAX_PENDING_FRAMES {
                                        queue_diagnostics.lost(IntakeQueueStage::Unbound, pending.len(), forge_protocol::MAX_PENDING_FRAMES, None, Some(&event));
                                        return Err(ProviderError::Observation(ObservationFailure::Overflow));
                                    }
                                    pending.push_back(event);
                                },
                                IntakeState::Active { sender, context, scope, .. } => {
                                    match sender.try_send(event) {
                                        Ok(()) => {},
                                        Err(mpsc::error::TrySendError::Full(event)) => {
                                            queue_diagnostics.lost(IntakeQueueStage::Foreground, sender.max_capacity(), sender.max_capacity(), Some(scope), Some(&event));
                                            return Err(ProviderError::Observation(ObservationFailure::Overflow));
                                        },
                                        Err(mpsc::error::TrySendError::Closed(event)) => {
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
            let result = result.and(stream.close().await.map_err(|_| {
                native_failure(
                    ProviderOperation::CollectEventReader,
                    ProviderFailureKind::Transport,
                )
            }));
            if let Err(error) = result {
                let reason = match error {
                    ProviderError::Observation(reason) => reason,
                    _ => ObservationFailure::Protocol,
                };
                native.fail(reason);
                let reason = native
                    .failure()
                    .expect("generation failure is sticky after publication");
                let context = match state {
                    IntakeState::Active {
                        context, sender, ..
                    } => {
                        // The foreground owns the decoder lock while receiving.
                        // Release its sender before waiting for that lock.
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
            diagnostics,
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
        let scope = context.lock().await.sink.invocation_scope()?.clone();
        let (reply, response) = oneshot::channel();
        self.commands
            .send(Command::Attach {
                generation,
                context,
                scope: scope.clone(),
                reply,
            })
            .await
            .map_err(|_| self.closed())?;
        let receiver = response.await.map_err(|_| self.closed())??;
        Ok(TurnReceiver {
            receiver,
            commands: self.commands.clone(),
            generation,
            failure: self.failure.clone(),
            client: self.client.clone(),
            state: TurnReceiveState::Live,
            scope,
            diagnostics: self.diagnostics.clone(),
        })
    }

    pub(crate) async fn close(&self) -> Result<(), ProviderError> {
        let mut task = self.task.lock().await;
        let Some(pending) = task.as_mut() else {
            return Ok(());
        };
        let (reply, response) = oneshot::channel();
        if self.commands.send(Command::Close { reply }).await.is_ok() {
            let _ = response.await;
        }
        pending.await.map_err(|_error| {
            super::error::native_failure(
                forge_provider_api::ProviderOperation::NativeTransport,
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
        if self
            .commands
            .try_send(Command::Release {
                generation: self.generation,
            })
            .is_err()
        {
            if self.client.failure().is_none() {
                self.diagnostics.lost(
                    IntakeQueueStage::Release,
                    self.commands.max_capacity() - self.commands.capacity(),
                    self.commands.max_capacity(),
                    Some(&self.scope),
                    None,
                );
            }
            self.client.fail(ObservationFailure::Overflow);
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

fn native_failure(
    operation: forge_provider_api::ProviderOperation,
    kind: forge_provider_api::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{ExecutionOwner, InvocationScope, MessageUpdate};
    use github_copilot_sdk::Client;
    use serde_json::json;

    fn event(id: &str, kind: &str, data: serde_json::Value) -> SessionEvent {
        serde_json::from_value(
            json!({"id":id,"type":kind,"timestamp":"2026-11-01T00:00:00Z","data":data}),
        )
        .unwrap()
    }

    async fn fixture() -> (
        SessionIntake,
        mpsc::Sender<SessionEvent>,
        Arc<Mutex<TurnContext>>,
        forge_provider_api::ProviderEventStream,
    ) {
        let (sender, receiver) = mpsc::channel(32);
        let task = tokio::spawn(std::future::pending::<anyhow::Result<()>>());
        let (intake, context, observed) =
            fixture_stream(SessionEventStream::fixture(receiver, task)).await;
        (intake, sender, context, observed)
    }

    /// Bind a supplied native stream to the same retained decoder used by live prompts.
    async fn fixture_stream(
        stream: SessionEventStream,
    ) -> (
        SessionIntake,
        Arc<Mutex<TurnContext>>,
        forge_provider_api::ProviderEventStream,
    ) {
        let (read, _read_peer) = tokio::io::duplex(4096);
        let (write, _write_peer) = tokio::io::duplex(4096);
        let client = Client::from_streams(read, write, std::env::temp_dir()).unwrap();
        fixture_stream_on(stream, ClientGeneration::new(client)).await
    }

    /// Bind another session reader to the same actual SDK client generation.
    async fn fixture_stream_on(
        stream: SessionEventStream,
        generation: ClientGeneration,
    ) -> (
        SessionIntake,
        Arc<Mutex<TurnContext>>,
        forge_provider_api::ProviderEventStream,
    ) {
        let intake = SessionIntake::new(stream, generation);
        let scope = InvocationScope::new(ExecutionOwner::root("session"), "exchange");
        let binding = forge_provider_api::BindingId::new("intake-fixture").unwrap();
        let (sink, mut observed) = forge_provider_api::events::channel(binding, scope.clone());
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
        (intake, context, observed)
    }

    /// Native loss wakes a prompt holding the decoder without discarding its buffered prefix.
    #[tokio::test]
    async fn active_context_failure_drains_prefix_without_waiting_for_decoder_lock()
    -> anyhow::Result<()> {
        let (sender, receiver) = mpsc::channel(2);
        let (fail, failed) = oneshot::channel();
        let task = tokio::spawn(async move {
            failed.await?;
            Err(ProviderError::Observation(ObservationFailure::Disconnected).into())
        });
        let (intake, context, mut observed) =
            fixture_stream(SessionEventStream::fixture(receiver, task)).await;
        let mut turn = intake.attach(context.clone()).await?;
        let mut active = context.lock().await;
        sender
            .send(event(
                "answer",
                "assistant.message",
                json!({
                    "messageId":"answer", "phase":"final_answer", "content":"accepted before loss"
                }),
            ))
            .await?;
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            while turn.receiver.is_empty() {
                tokio::task::yield_now().await;
            }
        })
        .await?;
        fail.send(())
            .map_err(|_| anyhow::anyhow!("native fixture failure waiter closed"))?;
        drop(sender);
        let mut failure = intake.failure.clone();
        tokio::time::timeout(
            std::time::Duration::from_secs(1),
            failure.wait_for(Option::is_some),
        )
        .await??;

        let prefix = tokio::time::timeout(std::time::Duration::from_secs(1), turn.recv()).await??;
        assert_eq!(prefix.id, "answer");
        let TurnContext {
            decoder,
            output,
            sink,
        } = &mut *active;
        decoder.decode(&prefix, output, sink).await?;
        let error = tokio::time::timeout(std::time::Duration::from_secs(1), turn.recv())
            .await?
            .unwrap_err();
        assert!(matches!(
            error,
            ProviderError::Observation(ObservationFailure::Disconnected)
        ));
        assert!(
            !intake.task.lock().await.as_ref().unwrap().is_finished(),
            "fixture must retain its decoder lock until foreground loss is delivered"
        );
        drop(active);
        drop(turn);

        let mut prefix_preserved = false;
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            loop {
                match observed.recv().await? {
                    Some(event)
                        if matches!(&event.event, ProviderEvent::Message {
                        update: MessageUpdate::Completed { text },
                        ..
                    } if text == "accepted before loss") =>
                    {
                        prefix_preserved = true
                    }
                    Some(event)
                        if matches!(&event.event, ProviderEvent::Execution { observation }
                        if matches!(
                            observation.update(),
                            ExecutionUpdate::ObservationLost {
                                failure: ObservationFailure::Disconnected,
                                ..
                            }
                        )) =>
                    {
                        break;
                    }
                    Some(_) => {}
                    None => anyhow::bail!("native loss was not published"),
                }
            }
            Ok::<(), anyhow::Error>(())
        })
        .await??;
        assert!(
            prefix_preserved,
            "accepted native output disappeared before observation loss"
        );
        tokio::time::timeout(std::time::Duration::from_secs(1), intake.close()).await??;
        assert!(
            matches!(
                *intake.failure.borrow(),
                Some(ObservationFailure::Disconnected)
            ),
            "receiver disposal replaced the original native failure"
        );
        Ok(())
    }

    /// Retain a full bounded startup or foreground burst while the prompt stops receiving.
    #[tokio::test]
    async fn bursts_at_capacity_survive_before_attach_and_while_foreground_is_paused() {
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            for attached in [false, true] {
                let (intake, sender, context, _observed) = fixture().await;
                let mut receiver = if attached {
                    Some(intake.attach(context.clone()).await.unwrap())
                } else {
                    None
                };
                let event_count = forge_protocol::MAX_PENDING_FRAMES;
                for sequence in 0..event_count {
                    sender
                        .send(event(
                            &format!("burst-{sequence}"),
                            "assistant.message_delta",
                            json!({"messageId":"answer","deltaContent":sequence.to_string()}),
                        ))
                        .await
                        .unwrap();
                }
                while sender.capacity() != sender.max_capacity() {
                    tokio::task::yield_now().await;
                }
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

    /// Report explicit loss instead of retaining an unbounded paused-reader backlog.
    #[tokio::test]
    async fn paused_bursts_over_capacity_publish_sticky_overflow() {
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            for attached in [false, true] {
                let (intake, sender, context, _observed) = fixture().await;
                let receiver = if attached {
                    Some(intake.attach(context).await.unwrap())
                } else {
                    None
                };
                for sequence in 0..=forge_protocol::MAX_PENDING_FRAMES {
                    sender
                        .send(event(
                            &format!("overflow-{sequence}"),
                            "session.usage_info",
                            json!({}),
                        ))
                        .await
                        .unwrap();
                }
                let mut failure = intake.failure.clone();
                failure.wait_for(Option::is_some).await.unwrap();
                assert!(matches!(
                    *failure.borrow(),
                    Some(ObservationFailure::Overflow)
                ));
                assert!(intake.client_stopped());
                drop(receiver);
                intake.close().await.unwrap();
                assert!(matches!(
                    *failure.borrow(),
                    Some(ObservationFailure::Overflow)
                ));
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
            for sequence in 0..forge_protocol::MAX_PENDING_FRAMES {
                sender.send(event(&format!("prefix-{sequence}"), "session.usage_info", json!({}))).await.unwrap();
            }
            while receiver.receiver.len() != forge_protocol::MAX_PENDING_FRAMES {
                tokio::task::yield_now().await;
            }
            intake.client.fail(ObservationFailure::Disconnected);
            for sequence in 0..forge_protocol::MAX_PENDING_FRAMES {
                assert_eq!(receiver.recv().await.unwrap().id, format!("prefix-{sequence}"));
            }
            assert!(matches!(receiver.recv().await, Err(ProviderError::Observation(ObservationFailure::Disconnected))));
            assert!(intake.client_stopped());
            drop(active);
            assert!(matches!(observed.recv().await.unwrap(), Some(event) if matches!(&event.event, ProviderEvent::Execution { observation }
                if matches!(observation.update(), ExecutionUpdate::ObservationLost { failure: ObservationFailure::Disconnected, .. }))));
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
        let binding = forge_provider_api::BindingId::new("next-intake-fixture").unwrap();
        let (sink, _next_events) = forge_provider_api::events::channel(binding, scope.clone());
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
            .await
            .unwrap();
        let late = tokio::time::timeout(std::time::Duration::from_secs(1), observed.recv())
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        assert_eq!(
            late.event
                .scope()
                .invocation()
                .unwrap()
                .exchange_id
                .as_str(),
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
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                if let Some(event) = observed.recv().await.unwrap()
                    && let ProviderEvent::Message {
                        update: MessageUpdate::Completed { text },
                        ..
                    } = event.event
                    && text == "late answer"
                {
                    break;
                }
            }
        })
        .await
        .unwrap();
        intake.close().await.unwrap();
        intake.client.fail(ObservationFailure::Disconnected);
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
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                if let Some(event) = observed.recv().await.unwrap()
                    && let ProviderEvent::Execution { observation } = event.event
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

    /// One failed reader wakes all active sessions sharing its native client, even under decoder locks.
    #[tokio::test]
    async fn shared_client_failure_wakes_all_session_readers_and_collects_them() {
        let (first_sender, first_receiver) = mpsc::channel(32);
        let first_task = tokio::spawn(async {
            Err(ProviderError::Observation(ObservationFailure::Disconnected).into())
        });
        let (first, first_context, _first_observed) =
            fixture_stream(SessionEventStream::fixture(first_receiver, first_task)).await;
        let (second_sender, second_receiver) = mpsc::channel(32);
        let second_task = tokio::spawn(std::future::pending::<anyhow::Result<()>>());
        let (second, second_context, _second_observed) = fixture_stream_on(
            SessionEventStream::fixture(second_receiver, second_task),
            first.client.clone(),
        )
        .await;
        let mut first_receiver = first.attach(first_context.clone()).await.unwrap();
        let mut second_receiver = second.attach(second_context.clone()).await.unwrap();
        let first_guard = first_context.lock().await;
        let second_guard = second_context.lock().await;
        drop(first_sender);
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            assert!(matches!(
                first_receiver.recv().await,
                Err(ProviderError::Observation(ObservationFailure::Disconnected))
            ));
            assert!(matches!(
                second_receiver.recv().await,
                Err(ProviderError::Observation(ObservationFailure::Disconnected))
            ));
        })
        .await
        .expect("shared native failure waited for a decoder lock or unrelated sender drop");
        drop(first_guard);
        drop(second_guard);
        drop(first_receiver);
        drop(second_receiver);
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            first.close().await.unwrap();
            second.close().await.unwrap();
        })
        .await
        .expect("failed generation left intake tasks uncollected");
        assert!(first.task.lock().await.is_none() && second.task.lock().await.is_none());
        assert!(
            second_sender.is_closed(),
            "collected generation retained an open native stream"
        );
    }
}
