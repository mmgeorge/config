//! Exercise the production engine with independently controlled adapter acknowledgments.

use super::*;
use crate::provider::submission::SubmissionStore;
use crate::provider::{PromptMode, ProviderInput, SteeringInput};
use anyhow::Context;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink, ProviderEventStream},
    lifecycle::{ExecutionObservation, ExecutionResult},
    provider::Provider as NativeProvider,
    session::{
        BindingId, ContinuationOutcome, InputAcceptance, SessionBinding, SessionRequest,
        SteeringAcceptance, SteeringRequest,
    },
};

enum Dispatch {
    Prompt {
        scope: InvocationScope,
        input: SubmissionId,
    },
    Steer {
        scope: InvocationScope,
        input: SubmissionId,
        acknowledgment: oneshot::Sender<()>,
    },
}

/// Hold only adapter acknowledgments; lifecycle observations still use the real intake.
struct ControlledProvider {
    sessions: Mutex<HashMap<BindingId, Arc<ControlledSession>>>,
    owners: Mutex<HashMap<ExecutionOwner, BindingId>>,
    streams: Mutex<HashMap<ExecutionOwner, ProviderEventSink>>,
    invocations: Mutex<HashMap<InvocationScope, ProviderEventSink>>,
    dispatch: mpsc::Sender<Dispatch>,
    subscriptions: Mutex<HashMap<ExecutionOwner, usize>>,
    cleanup: Mutex<Vec<String>>,
    panic_prompt: bool,
}

struct ControlledSession {
    owner: ExecutionOwner,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
}

impl ControlledProvider {
    fn new(dispatch: mpsc::Sender<Dispatch>, panic_prompt: bool) -> Self {
        Self {
            sessions: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
            streams: Mutex::new(HashMap::new()),
            invocations: Mutex::new(HashMap::new()),
            dispatch,
            subscriptions: Mutex::new(HashMap::new()),
            cleanup: Mutex::new(Vec::new()),
            panic_prompt,
        }
    }

    /// Publish normalized lifecycle evidence through its original owner's stream.
    async fn publish(
        &self,
        scope: &InvocationScope,
        update: ExecutionUpdate,
    ) -> anyhow::Result<()> {
        let sink = self
            .invocations
            .lock()
            .unwrap()
            .get(scope)
            .cloned()
            .context("owner was not subscribed")?;
        sink.send_wait(ProviderEvent::Execution {
            observation: ExecutionObservation::new(scope.clone(), update)?,
        })
        .await?;
        Ok(())
    }

    async fn accept(
        receipt: Arc<dyn forge_provider_api::submission::InputReceipt>,
    ) -> Result<SubmissionId, ProviderError> {
        let id = receipt.id().clone();
        let token =
            forge_provider_api::RecoveryToken::new(format!("fixture:{}", id.as_str()).into_bytes())
                .map_err(|_| {
                    failure(ProviderOperation::RecordInput, ProviderFailureKind::Unknown)
                })?;
        receipt.clone().record_sending(Some(token.clone())).await?;
        receipt.record_accepted(Some(token)).await?;
        Ok(id)
    }

    async fn session(&self, binding: &BindingId) -> Result<Arc<ControlledSession>, ProviderError> {
        self.sessions
            .lock()
            .unwrap()
            .get(binding)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })
    }
}

#[async_trait::async_trait]
impl NativeProvider for ControlledProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner: request.owner.clone(),
            operation_id: id.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(id.clone(), scope);
        let binding = SessionBinding {
            id: id.clone(),
            provider_session_id: request.provider_session_id,
        };
        let session = Arc::new(ControlledSession {
            owner: request.owner.clone(),
            sink: sink.clone(),
            stream: Mutex::new(Some(stream)),
        });
        self.streams
            .lock()
            .unwrap()
            .insert(request.owner.clone(), sink);
        self.owners
            .lock()
            .unwrap()
            .insert(request.owner, id.clone());
        self.sessions.lock().unwrap().insert(id, session);
        Ok(binding)
    }

    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> Result<ProviderEventStream, ProviderError> {
        let session = self.session(binding).await?;
        *self
            .subscriptions
            .lock()
            .unwrap()
            .entry(session.owner.clone())
            .or_default() += 1;
        session.stream.lock().unwrap().take().ok_or_else(|| {
            failure(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::Rejected,
            )
        })
    }

    /// Acknowledge a prompt without publishing terminal evidence.
    async fn session_prompt(
        &self,
        binding: &BindingId,
        request: forge_provider_api::PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        let session = self.session(binding).await?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let input = Self::accept(request.receipt.clone()).await?;
        let scope = request.scope.clone();
        let sink = session.sink.invocation_sink(&scope)?;
        self.invocations.lock().unwrap().insert(scope.clone(), sink);
        if self.panic_prompt {
            panic!("controlled native submission task panic");
        }
        self.dispatch
            .send(Dispatch::Prompt {
                scope,
                input: input.clone(),
            })
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Transport,
                )
            })?;
        Ok(InputAcceptance {
            submission_id: input,
        })
    }

    /// Allow terminal evidence to precede an independently held steering acknowledgment.
    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        let session = self.session(binding).await?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let scope = request.scope.clone();
        let Some(receipt) = request.delivery.receipt().cloned() else {
            return Ok(SteeringAcceptance::Context);
        };
        let input = Self::accept(receipt).await?;
        let sink = session.sink.invocation_sink(&scope)?;
        self.invocations.lock().unwrap().insert(scope.clone(), sink);
        let (acknowledgment, received) = oneshot::channel();
        self.dispatch
            .send(Dispatch::Steer {
                scope,
                input: input.clone(),
                acknowledgment,
            })
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Transport,
                )
            })?;
        received.await.map_err(|_| {
            failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Transport,
            )
        })?;
        Ok(SteeringAcceptance::Journaled {
            submission_id: input,
        })
    }

    /// Revoke this test owner's callbacks without touching another owner.
    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        self.cleanup
            .lock()
            .unwrap()
            .push(format!("fence:{}", session.owner.runtime_key()));
        Ok(())
    }

    /// Collect one runtime through the same cleanup path used by loss and explicit close.
    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        self.cleanup
            .lock()
            .unwrap()
            .push(format!("collect:{}", session.owner.runtime_key()));
        Ok(())
    }

    /// Finish the fixture only after the registry has explicitly closed its engines.
    async fn shutdown(&self) -> Result<(), ProviderError> {
        self.streams.lock().unwrap().clear();
        self.sessions.lock().unwrap().clear();
        self.owners.lock().unwrap().clear();
        Ok(())
    }

    /// Describe only the synthetic provider contract exercised by this fixture.
    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            label: "Controlled owner fixture".into(),
            capability: Default::default(),
        }
    }

    /// Native work is externally terminated by the fixture's cleanup acknowledgment.
    async fn has_active_turn(&self, _owner: &ExecutionOwner) -> bool {
        false
    }

    /// Keep native identity optional without manufacturing a provider session.
    async fn active_session_id_for(
        &self,
        _owner: &ExecutionOwner,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        Ok(None)
    }

    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        self.cleanup_execution(binding).await
    }

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.session(binding).await?;
        self.sessions.lock().unwrap().remove(binding);
        self.owners.lock().unwrap().remove(&session.owner);
        self.streams.lock().unwrap().remove(&session.owner);
        Ok(())
    }

    async fn recover_session(
        &self,
        request: SessionRequest,
        _token: Option<forge_provider_api::RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        self.initialize_session(request).await
    }

    async fn continue_session(
        &self,
        _binding: &BindingId,
        _request: forge_provider_api::ContinuationRequest,
    ) -> Result<ContinuationOutcome, ProviderError> {
        Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ))
    }
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Match production identities: managed children share a session, Background has its own.
fn owners() -> [ExecutionOwner; 3] {
    [
        ExecutionOwner::root("conversation"),
        ExecutionOwner {
            session_id: "conversation".into(),
            agent_id: "managed-child".into(),
        },
        ExecutionOwner::root("background-session"),
    ]
}

/// Prepare a request with an explicit owner and the same persisted journal as production.
fn request(owner: &ExecutionOwner, directory: &std::path::Path) -> ProviderRequest {
    ProviderRequest {
        execution_owner: Some(owner.clone()),
        harness_session_id: owner.session_id.to_string(),
        workspace: directory.to_string_lossy().into_owned(),
        input: ProviderInput::from_text("inspect"),
        mode: if owner.session_id == forge_provider_api::SessionId::from("background-session") {
            PromptMode::Background
        } else {
            PromptMode::Chat
        },
        model: "mock".into(),
        effort: "low".into(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
        provider_session_id: None,
        submission: None,
        recovered_input: Vec::new(),
        control_context: None,
    }
}

/// Construct a native address without conflating its owner, exchange, and execution anchor.
fn address(scope: &InvocationScope, turn: &str) -> crate::provider::TurnAddress {
    crate::provider::TurnAddress {
        scope: scope.clone(),
        thread_id: "controlled-thread".into(),
        turn_id: turn.into(),
    }
}

/// Publish native terminal evidence for exactly the supplied admitted journal inputs.
fn finished(scope: &InvocationScope, turn: &str, covered: Vec<SubmissionId>) -> ExecutionUpdate {
    ExecutionUpdate::Finished {
        address: address(scope, turn),
        covered_submissions: covered,
        completed_at_ms: 1,
        result: ExecutionResult::Completed {
            completion: forge_provider_api::completion::ProviderCompletion::default().into(),
        },
    }
}

/// Preserve old A's live boundary and fail both A and newer B without replaying either input.
#[tokio::test]
async fn committed_old_owner_execution_conflicts_with_new_active_exchange_without_losing_attribution()
-> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
        for _ in 0..10 {
            let directory = tempfile::tempdir()?;
            let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
            let store = SubmissionStore::open(directory.path(),trace.clone())?;
            let (dispatch, mut calls) = mpsc::channel(16);
            let peer = Arc::new(ControlledProvider::new(dispatch, false));
            let provider_session = crate::provider::test_support::ProviderTestSession::from_native(peer.clone())?;
            let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace);
            for owner in owners() {
                let engine = registry.owner(owner.clone()).await?;
                let observer = registry.observer(owner.clone())?;
                let a = InvocationScope::new(owner.clone(),"committed-a");
                let mut first_request = request(&owner,directory.path());
                let receipt_a = store.prepare(&first_request,forge_providers::registry::nonselectable_id().expect("registry supplies a nonselectable fixture identity"))?;
                first_request.submission = Some(receipt_a.clone());
                let (sink_a, mut events_a) = crate::provider::events::channel(a.clone());
                let running = engine.clone(); let journal = store.clone();
                let first = tokio::spawn(async move { running.run_until_ready(first_request,sink_a,&journal).await });
                let Some(Dispatch::Prompt { scope, input }) = calls.recv().await else { anyhow::bail!("missing A prompt"); };
                assert_eq!(scope,a);
                peer.publish(&a,ExecutionUpdate::Started { address:address(&a,"a-original") }).await?;
                peer.publish(&a,finished(&a,"a-original",vec![input])).await?;
                let ready = first.await??;
                ready.ticket.commit().await?;
                let b = InvocationScope::new(owner.clone(),"active-b");
                let mut next_request = request(&owner,directory.path());
                let receipt_b = store.prepare(&next_request,forge_providers::registry::nonselectable_id().expect("registry supplies a nonselectable fixture identity"))?;
                next_request.submission = Some(receipt_b.clone());
                let (sink_b, mut events_b) = crate::provider::events::channel(b.clone());
                let running = engine.clone(); let journal = store.clone();
                let second = tokio::spawn(async move { running.run_until_ready(next_request,sink_b,&journal).await });
                let Some(Dispatch::Prompt { scope, .. }) = calls.recv().await else { anyhow::bail!("missing B prompt"); };
                assert_eq!(scope,b);
                peer.publish(&b,ExecutionUpdate::Started { address:address(&b,"b-active") }).await?;
                loop {
                    if let Some(ProviderEvent::Execution { observation }) = events_b.recv().await?
                        && matches!(observation.update(),ExecutionUpdate::Started {..}) { break; }
                }
                peer.publish(&a,ExecutionUpdate::Started { address:address(&a,"a-live-late") }).await?;
                let error = match second.await? { Err(error) => error, Ok(_) => panic!("ownership conflict produced a complete B result") };
                assert!(matches!(error,ProviderError::Observation(ObservationFailure::OwnershipConflict)));
                engine.close().await?;
                for (scope, events) in [(&a,&mut events_a),(&b,&mut events_b)] {
                    let mut lost = false;
                    while let Some(event) = events.recv().await? {
                        if let ProviderEvent::Execution { observation } = event
                            && matches!(observation.update(),ExecutionUpdate::ObservationLost { failure:ObservationFailure::OwnershipConflict,.. }) {
                            assert_eq!(observation.scope(),scope); lost=true; break;
                        }
                    }
                    assert!(lost,"conflict did not preserve typed loss for {scope:?}");
                }
                let prefix = observer.drain()?;
                assert!(prefix.iter().any(|event| matches!(event,ProviderEvent::Execution {observation}
                    if observation.scope()==&a && matches!(observation.update(),ExecutionUpdate::Started {address} if address.turn_id==forge_provider_api::ProviderTurnId::from("a-live-late")))));
                assert!(matches!(observer.end(),Some(super::super::ObservationEnd::Lost(ObservationFailure::OwnershipConflict))));
                for receipt in [receipt_a,receipt_b] {
                    assert!(matches!(receipt.record()?.delivery,crate::provider::submission::SubmissionState::Accepted {..}));
                }
                assert!(!engine.is_healthy().await);
                assert!(calls.try_recv().is_err(),"ownership conflict replayed an accepted native input");
            }
            registry.close().await?;
            peer.shutdown().await?;
        }
        Ok::<_,anyhow::Error>(())
    }).await?
}

/// Reject nominally successful cleanup that has no terminal coverage for accepted input.
#[tokio::test]
async fn intentional_retirement_rejects_accepted_input_without_terminal_proof() -> anyhow::Result<()>
{
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (dispatch, mut calls) = mpsc::channel(4);
        let peer = Arc::new(ControlledProvider::new(dispatch, false));
        let provider_session =
            crate::provider::test_support::ProviderTestSession::from_native(peer.clone())?;
        let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace);
        let owner = ExecutionOwner::root("missing-terminal");
        let engine = registry.owner(owner.clone()).await?;
        let observer = registry.observer(owner.clone())?;
        let scope = InvocationScope::new(owner.clone(), "accepted-without-start");
        let mut input = request(&owner, directory.path());
        let receipt = store.prepare(
            &input,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
        )?;
        input.submission = Some(receipt.clone());
        let (sink, _events) = crate::provider::events::channel(scope);
        let running = engine.clone();
        let journal = store.clone();
        let result =
            tokio::spawn(async move { running.run_until_ready(input, sink, &journal).await });
        assert!(matches!(calls.recv().await, Some(Dispatch::Prompt { .. })));
        assert!(matches!(
            engine.close_with_reason(OwnerCloseReason::Cancelled).await,
            Err(ProviderError::Observation(
                ObservationFailure::LifecycleUnavailable
            ))
        ));
        assert!(matches!(
            result.await?,
            Err(ProviderError::Observation(
                ObservationFailure::LifecycleUnavailable
            ))
        ));
        assert!(matches!(
            observer.end(),
            Some(super::super::ObservationEnd::Lost(
                ObservationFailure::LifecycleUnavailable
            ))
        ));
        assert!(matches!(
            receipt.record()?.delivery,
            crate::provider::submission::SubmissionState::Accepted { .. }
        ));
        assert!(calls.try_recv().is_err());
        registry.close().await?;
        peer.shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Observe that the production engine reduced a native boundary before checking its waiter.
async fn wait_finished(
    events: &mut crate::provider::events::ProviderEventStream,
    turn: &str,
) -> anyhow::Result<()> {
    loop {
        if let Some(ProviderEvent::Execution { observation }) = events.recv().await?
            && matches!(observation.update(), ExecutionUpdate::Finished { address, .. } if address.turn_id == forge_provider_api::ProviderTurnId::from(turn))
        {
            return Ok(());
        }
    }
}

/// Create a journaled steering input instead of inventing a second submission identity.
fn steering(
    store: &Arc<SubmissionStore>,
    owner: &ExecutionOwner,
    directory: &std::path::Path,
) -> anyhow::Result<SteeringInput> {
    let catalog = crate::provider::ProviderCatalogRequest {
        execution_owner: Some(owner.clone()),
        harness_session_id: owner.session_id.to_string(),
        workspace: directory.to_string_lossy().into_owned(),
        provider_session_id: None,
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
    };
    Ok(SteeringInput::User {
        text: "continue".into(),
        attachments: Vec::new(),
        submission: Some(
            store.prepare_steering(
                &catalog,
                ProviderInput::from_text("continue"),
                forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
            )?,
        ),
    })
}

/// Repeat terminal-before-ack ordering for every owner and retain intake after host return.
#[tokio::test]
async fn every_owner_retains_pending_steering_and_idle_observation() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
        for iteration in 0..10 {
            let directory = tempfile::tempdir()?;
            let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
            let store = SubmissionStore::open(directory.path(), trace.clone())?;
            let (dispatch, mut calls) = mpsc::channel(16);
            let peer = Arc::new(ControlledProvider::new(dispatch, false));
            let provider_session =
                crate::provider::test_support::ProviderTestSession::from_native(peer.clone())?;
            let registry =
                SessionExecutionRegistry::new(provider_session.provider_handle(), trace.clone());
            for owner in owners() {
                // Admit through the production registry and wait for an actual native dispatch.
                let engine = registry.owner(owner.clone()).await?;
                let mut idle = engine.session_events().await?;
                let scope = InvocationScope::new(owner.clone(), format!("exchange-{iteration}"));
                let (sink, mut events) = crate::provider::events::channel(scope.clone());
                let running = engine.clone();
                let journal = store.clone();
                let input_request = request(&owner, directory.path());
                let result =
                    tokio::spawn(
                        async move { running.run_once(input_request, sink, &journal).await },
                    );
                let Dispatch::Prompt {
                    scope: observed,
                    input,
                } = calls.recv().await.context("missing initial dispatch")?
                else {
                    anyhow::bail!("expected prompt")
                };
                assert_eq!(observed, scope);
                peer.publish(
                    &scope,
                    ExecutionUpdate::Started {
                        address: address(&scope, "initial"),
                    },
                )
                .await?;
                assert!(!result.is_finished(), "acceptance settled {owner:?}");

                // Reduce both native terminal boundaries while the steering acknowledgment is held.
                let running = engine.clone();
                let steer_scope = scope.clone();
                let steer_input = steering(&store, &owner, directory.path())?;
                let steer =
                    tokio::spawn(async move { running.steer(steer_scope, steer_input).await });
                let Dispatch::Steer {
                    scope: observed,
                    input: steered,
                    acknowledgment,
                } = calls.recv().await.context("missing steering dispatch")?
                else {
                    anyhow::bail!("expected steer")
                };
                assert_eq!(observed, scope);
                peer.publish(&scope, finished(&scope, "initial", vec![input]))
                    .await?;
                wait_finished(&mut events, "initial").await?;
                assert!(
                    !result.is_finished(),
                    "earlier terminal covered pending input"
                );
                peer.publish(
                    &scope,
                    ExecutionUpdate::Started {
                        address: address(&scope, "steered"),
                    },
                )
                .await?;
                peer.publish(&scope, finished(&scope, "steered", vec![steered]))
                    .await?;
                wait_finished(&mut events, "steered").await?;
                assert!(
                    !result.is_finished(),
                    "terminal bypassed pending acknowledgment"
                );
                acknowledgment
                    .send(())
                    .map_err(|_| anyhow::anyhow!("steering acknowledgment lost"))?;
                steer.await??;
                result.await??;

                // A returned host result keeps its original observer and invocation authority.
                let running = engine.clone();
                let steer_scope = scope.clone();
                let steer_input = steering(&store, &owner, directory.path())?;
                let continuation =
                    tokio::spawn(async move { running.steer(steer_scope, steer_input).await });
                let Dispatch::Steer {
                    scope: observed,
                    input,
                    acknowledgment,
                } = calls.recv().await.context("missing idle steering")?
                else {
                    anyhow::bail!("expected retained steer")
                };
                assert_eq!(observed, scope);
                peer.publish(
                    &scope,
                    ExecutionUpdate::Started {
                        address: address(&scope, "idle"),
                    },
                )
                .await?;
                peer.publish(&scope, finished(&scope, "idle", vec![input]))
                    .await?;
                acknowledgment
                    .send(())
                    .map_err(|_| anyhow::anyhow!("idle acknowledgment lost"))?;
                continuation.await??;
                wait_finished(&mut idle, "idle").await?;
                let ready = engine.wait_ready(scope.clone()).await?;
                ready.outcome?;
                ready.ticket.commit().await?;
                assert_eq!(peer.subscriptions.lock().unwrap().get(&owner), Some(&1));
                assert!(
                    calls.try_recv().is_err(),
                    "provider silently replayed input"
                );
            }
            registry.close().await?;
            for slot in registry.owners.lock().unwrap().values() {
                assert!(
                    !slot.engine.get().unwrap().healthy.load(Ordering::Acquire)
                        || slot.engine.get().unwrap().commands.is_closed()
                );
            }
            peer.shutdown().await?;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Fence pending controls without cancelling peer owners or replaying accepted input.
#[tokio::test]
async fn every_owner_isolates_pending_control_cancellation_and_loss() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(30), async {
        for iteration in 0..10 {
            for target in 0..3 {
                for lose_observation in [false, true] {
                    let directory = tempfile::tempdir()?;
                    let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
                    let store = SubmissionStore::open(directory.path(), trace.clone())?;
                    let (dispatch, mut calls) = mpsc::channel(16);
                    let peer = Arc::new(ControlledProvider::new(dispatch, false));
                    let provider_session = crate::provider::test_support::ProviderTestSession::from_native(peer.clone())?;
                    let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace.clone());
                    let owners = owners();
                    let mut active = Vec::new();

                    // Keep all three independent owners active before disrupting one of them.
                    for owner in &owners {
                        let engine = registry.owner(owner.clone()).await?;
                        let scope = InvocationScope::new(owner.clone(), format!("isolated-{iteration}"));
                        let (sink, events) = crate::provider::events::channel(scope.clone());
                        let running = engine.clone();
                        let journal = store.clone();
                        let input_request = request(owner, directory.path());
                        let result = tokio::spawn(async move { running.run_once(input_request, sink, &journal).await });
                        let Dispatch::Prompt { scope: observed, input } = calls.recv().await.context("missing concurrent prompt")? else { anyhow::bail!("unexpected control before prompt") };
                        assert_eq!(observed, scope);
                        peer.publish(&scope, ExecutionUpdate::Started { address: address(&scope, "active") }).await?;
                        active.push((engine, scope, input, result, events));
                    }
                    let (engine, scope, _, _, _) = &active[target];
                    let steer_input = steering(&store, &owners[target], directory.path())?;
                    let receipt = steer_input.submission().context("missing held steering journal")?;
                    let running = engine.clone();
                    let steer_scope = scope.clone();
                    let steer = tokio::spawn(async move { running.steer(steer_scope, steer_input).await });
                    let Dispatch::Steer { scope: observed, acknowledgment, .. } = calls.recv().await.context("missing held control")? else { anyhow::bail!("expected pending control") };
                    assert_eq!(&observed, scope);

                    // Interrupt only the selected owner while its accepted acknowledgment is pending.
                    if lose_observation {
                        let sink = peer.streams.lock().unwrap().get(&owners[target]).cloned().unwrap();
                        sink.fail(crate::provider::events::EventDeliveryError::Observation(ObservationFailure::Overflow));
                    } else {
                        registry.fence(&owners[target]).await?;
                    }
                    let selected = active.remove(target);
                    let failure = selected.3.await?.expect_err("interrupted work returned successful completion");
                    assert!(matches!(&failure,
                        ProviderError::Observation(ObservationFailure::Overflow) if lose_observation)
                        || matches!(&failure,
                        ProviderError::Observation(ObservationFailure::Disconnected) if !lose_observation),
                        "owner interruption lost its typed cause: {failure}");
                    assert!(steer.await?.is_err(), "pending control outlived its fenced owner");
                    drop(acknowledgment);
                    assert!(matches!(receipt.record()?.delivery, crate::provider::submission::SubmissionState::Accepted { .. }), "cancellation discarded native acceptance");
                    assert!(!selected.0.is_healthy().await);
                    assert!(!peer.cleanup.lock().unwrap().is_empty());
                    assert!(peer.cleanup.lock().unwrap().iter().all(|entry| entry.ends_with(&owners[target].runtime_key())), "cleanup targeted an unrelated owner");

                    // Peer results remain pending and then finish through their unchanged listeners.
                    for (engine, scope, input, result, mut events) in active {
                        assert!(!result.is_finished(), "another owner's failure released a peer result");
                        assert!(engine.is_healthy().await);
                        peer.publish(&scope, finished(&scope, "active", vec![input])).await?;
                        wait_finished(&mut events, "active").await?;
                        result.await??;
                        assert_eq!(peer.subscriptions.lock().unwrap().get(&scope.owner), Some(&1));
                    }
                    assert!(calls.try_recv().is_err(), "cleanup replayed a native input");
                    registry.close().await?;
                    peer.shutdown().await?;
                }
            }
        }
        Ok::<_, anyhow::Error>(())
    }).await?
}

/// Normalize a panicking submission without exposing its payload or losing owner fencing.
#[tokio::test]
async fn submission_task_failure_is_normalized_and_fences_owner() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (dispatch, _calls) = mpsc::channel(4);
        let peer = Arc::new(ControlledProvider::new(dispatch, true));
        let provider_session =
            crate::provider::test_support::ProviderTestSession::from_native(peer.clone())?;
        let registry = SessionExecutionRegistry::new(provider_session.provider_handle(), trace);
        let owner = ExecutionOwner::root("submission-panic");
        let engine = registry.owner(owner.clone()).await?;
        let (sink, _events) =
            crate::provider::events::channel(InvocationScope::new(owner.clone(), "exchange"));
        let outcome = engine
            .run_until_ready(request(&owner, directory.path()), sink, &store)
            .await;
        let error = match outcome {
            Err(error) => error,
            Ok(_) => panic!("panicking submission completed"),
        };
        assert!(matches!(&error, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::Runtime));
        assert!(!format!("{error:?} {error}").contains("controlled native submission task panic"));
        let mut source: Option<&(dyn std::error::Error + 'static)> = Some(&error);
        let mut observed = false;
        while let Some(error) = source {
            observed |= error.downcast_ref::<tokio::task::JoinError>().is_some();
            source = error.source();
        }
        assert!(!observed, "engine exposed an unnormalized worker failure");
        assert!(!engine.is_healthy().await);
        registry.close().await?;
        peer.shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}
