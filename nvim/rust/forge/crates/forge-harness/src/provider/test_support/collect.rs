//! Result-oriented test observers over the production session submission contract.

use forge_harness as harness;

use std::collections::HashMap;
use std::ops::Deref;
use std::sync::Arc;

use async_trait::async_trait;
use harness::provider::{
    ExecutionOwner, InvocationScope, ProviderAdapter, ProviderCompletion, ProviderError,
    ProviderEvent, ProviderEventSink, ProviderOutput, ProviderRequest,
};
use tokio::sync::Mutex;

/// Retain one observer per owner across complete-result assertions.
pub struct ProviderTestSession<P: ?Sized> {
    provider: Arc<P>,
    native: Arc<harness::provider::Provider>,
    registry: Arc<harness::provider::session_execution::SessionExecutionRegistry>,
    owners: Arc<Mutex<HashMap<ExecutionOwner, Arc<TestOwner>>>>,
    store: Arc<harness::provider::submission::SubmissionStore>,
    _directory: Arc<tempfile::TempDir>,
}

struct TestOwner {
    engine: Arc<harness::provider::session_execution::SessionExecution>,
    idle: Arc<Mutex<IdleObservations>>,
    observer: Mutex<Option<tokio::task::JoinHandle<()>>>,
}

#[derive(Default)]
struct IdleObservations {
    events: Vec<ProviderEvent>,
    failure: Option<forge_provider_api::lifecycle::ObservationFailure>,
}

impl Drop for TestOwner {
    fn drop(&mut self) {
        if let Some(observer) = self.observer.get_mut().take() {
            observer.abort();
        }
    }
}

impl<P: ProviderAdapter<Host = harness::provider::HarnessProviderHost>> ProviderTestSession<P> {
    /// Create a test-owned runtime without putting collection on the production trait.
    pub fn new(provider: P) -> anyhow::Result<Self> {
        Self::from_arc(Arc::new(provider))
    }

    /// Retain a concrete provider and its erased execution interface together.
    pub fn from_arc(provider: Arc<P>) -> anyhow::Result<Self> {
        let native: Arc<harness::provider::Provider> = provider.clone();
        Self::attach(provider, native)
    }
}

impl ProviderTestSession<harness::provider::Provider> {
    /// Attach an existing heterogeneous provider to the complete-result fixture.
    pub fn from_dyn(provider: Arc<harness::provider::Provider>) -> anyhow::Result<Self> {
        Self::attach(provider.clone(), provider)
    }
}

impl<P: ProviderAdapter<Host = harness::provider::HarnessProviderHost> + ?Sized>
    ProviderTestSession<P>
{
    fn attach(provider: Arc<P>, native: Arc<harness::provider::Provider>) -> anyhow::Result<Self> {
        let directory = Arc::new(tempfile::tempdir()?);
        let trace = Arc::new(harness::trace::TraceStore::open(directory.path())?);
        let store = harness::provider::submission::SubmissionStore::open(directory.path(), trace)?;
        let registry = Arc::new(
            harness::provider::session_execution::SessionExecutionRegistry::new(native.clone()),
        );
        Ok(Self {
            provider,
            native,
            registry,
            owners: Arc::default(),
            store,
            _directory: directory,
        })
    }

    /// Get the production provider interface for journal reconciliation.
    pub fn as_provider(&self) -> &harness::provider::Provider {
        self.native.as_ref()
    }

    /// Install session observation before native initialization can emit updates.
    pub async fn initialize_session(
        &self,
        request: ProviderRequest,
    ) -> anyhow::Result<Option<forge_provider_api::ProviderSessionId>> {
        self.owner(request.owner()?).await?;
        self.provider.initialize_session(request).await
    }

    /// Steer through the same owner and reducer used for complete-result collection.
    pub async fn steer_current(
        &self,
        owner: ExecutionOwner,
        input: harness::provider::SteeringInput,
    ) -> Result<(), ProviderError> {
        self.owner(owner).await?.engine.steer_current(input).await
    }

    /// Replace a failed owner through the production recovery path before another test request.
    pub async fn recover(&self, owner: ExecutionOwner) -> anyhow::Result<()> {
        self.registry.owner(owner.clone()).await?.close().await?;
        let inbox = self.registry.observer(owner.clone())?;
        // This fixture replaces inbox delivery with its separately joined stream.
        inbox.close();
        loop {
            match inbox.drain() {
                Ok(events) if events.is_empty() => break,
                Ok(_) => {}
                Err(error)
                    if matches!(
                        error.downcast_ref::<ProviderError>(),
                        Some(ProviderError::Observation(_))
                    ) =>
                {
                    break;
                }
                Err(error) => return Err(error),
            }
        }
        self.registry.replace(owner.clone()).await?;
        if let Some(previous) = self.owners.lock().await.remove(&owner)
            && let Some(observer) = previous.observer.lock().await.take()
        {
            observer.await?;
        }
        self.owner(owner).await?;
        Ok(())
    }

    /// Close all engines and join their independently retained observers.
    pub async fn close(&self) -> anyhow::Result<()> {
        self.registry.close().await?;
        let owners: Vec<_> = self.owners.lock().await.values().cloned().collect();
        let mut observer_failure = None;
        for owner in owners {
            let mut observer = owner.observer.lock().await;
            if let Some(handle) = observer.as_mut() {
                if let Err(error) = handle.await {
                    observer_failure.get_or_insert(error);
                }
                observer.take();
            }
        }
        match observer_failure {
            None => Ok(()),
            Some(error) => Err(anyhow::Error::new(error).context("test observer task failed")),
        }
    }

    /// Keep the engine and its idle observer alive independently of result waiters.
    async fn owner(&self, owner: ExecutionOwner) -> Result<Arc<TestOwner>, ProviderError> {
        let mut owners = self.owners.lock().await;
        if let Some(existing) = owners.get(&owner) {
            if let Some(failure) = existing.idle.lock().await.failure.clone() {
                return Err(ProviderError::Observation(failure));
            }
            return Ok(existing.clone());
        }
        let engine = self.registry.owner(owner.clone()).await?;
        let (_, mut stream) = engine.session_observer().await?;
        let idle = Arc::new(Mutex::new(IdleObservations::default()));
        let retained = idle.clone();
        let observer = tokio::spawn(async move {
            loop {
                let event = match stream.recv().await {
                    Ok(Some(event)) => event,
                    Ok(None) => break,
                    Err(error) => {
                        retained.lock().await.failure = Some(match error {
                            harness::provider::events::EventDeliveryError::Observation(failure) => {
                                failure
                            }
                            harness::provider::events::EventDeliveryError::OwnershipMismatch {
                                ..
                            } => {
                                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict
                            }
                            _ => forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                        });
                        break;
                    }
                };
                let mut idle = retained.lock().await;
                idle.events.push(event);
            }
        });
        let execution = Arc::new(TestOwner {
            engine,
            idle,
            observer: Mutex::new(Some(observer)),
        });
        owners.insert(owner, execution.clone());
        Ok(execution)
    }

    /// Collect active events while the production engine owns admission and settlement.
    async fn execute(
        &self,
        request: ProviderRequest,
        sink: Option<ProviderEventSink>,
    ) -> Result<ProviderOutput, ProviderError> {
        let owner = request.owner()?;
        let scope = match &sink {
            Some(sink) => sink.invocation_scope()?.clone(),
            None => InvocationScope::new(owner.clone(), uuid::Uuid::new_v4().to_string()),
        };
        if scope.owner != owner {
            return Err(ProviderError::Rejected {
                message: "test observer belongs to another owner".into(),
            });
        }
        let execution = self.owner(owner).await?;
        let (output, mut stream) = harness::provider::events::channel(scope.clone());
        let mut completion: std::pin::Pin<
            Box<
                dyn std::future::Future<
                        Output = Result<
                            harness::provider::session_execution::NativeReady,
                            ProviderError,
                        >,
                    > + Send
                    + '_,
            >,
        > = Box::pin(
            execution
                .engine
                .run_until_ready(request, output, &self.store),
        );
        let mut observed = Vec::new();
        let mut stream_open = true;
        loop {
            tokio::select! {
                result = &mut completion => {
                    while let Ok(event) = stream.try_recv() {
                        if let Some(sink) = &sink && !matches!(event, ProviderEvent::Execution { .. }) {
                            sink.send_wait(event.clone()).await?;
                        }
                        observed.push(event);
                    }
                    let ready = result?;
                    match ready.ticket.commit().await {
                        Ok(()) => return Ok(ProviderOutput { event: observed, completion: ready.outcome? }),
                        Err(harness::provider::session_execution::SettlementError::Stale) => {
                            completion = Box::pin(execution.engine.wait_ready(scope.clone()));
                        }
                        Err(harness::provider::session_execution::SettlementError::Unavailable) => {
                            return Err(ProviderError::Protocol { message: "execution owner closed before settlement".into() });
                        }
                        Err(harness::provider::session_execution::SettlementError::Provider(error)) => return Err(error),
                    }
                }
                event = stream.recv(), if stream_open => {
                    let Some(event) = event? else {
                        stream_open = false;
                        continue;
                    };
                    if let Some(sink) = &sink && !matches!(event, ProviderEvent::Execution { .. }) {
                        sink.send_wait(event.clone()).await?;
                    }
                    observed.push(event);
                }
            }
        }
    }
}

impl<P: ?Sized> Clone for ProviderTestSession<P> {
    fn clone(&self) -> Self {
        Self {
            provider: self.provider.clone(),
            native: self.native.clone(),
            registry: self.registry.clone(),
            owners: self.owners.clone(),
            store: self.store.clone(),
            _directory: self._directory.clone(),
        }
    }
}

impl<P: ?Sized> Deref for ProviderTestSession<P> {
    type Target = P;
    fn deref(&self) -> &P {
        &self.provider
    }
}

/// Collect complete output above the session API without taking over its native reader.
#[async_trait]
pub trait ProviderTestExt {
    async fn session_prompt_and_collect(
        &self,
        request: ProviderRequest,
    ) -> Result<ProviderOutput, ProviderError>;
    async fn session_prompt_and_wait(
        &self,
        request: ProviderRequest,
        sink: ProviderEventSink,
    ) -> Result<ProviderCompletion, ProviderError>;
}

#[async_trait]
impl<P: ProviderAdapter<Host = harness::provider::HarnessProviderHost> + ?Sized> ProviderTestExt
    for ProviderTestSession<P>
{
    async fn session_prompt_and_collect(
        &self,
        request: ProviderRequest,
    ) -> Result<ProviderOutput, ProviderError> {
        self.execute(request, None).await
    }
    async fn session_prompt_and_wait(
        &self,
        request: ProviderRequest,
        sink: ProviderEventSink,
    ) -> Result<ProviderCompletion, ProviderError> {
        Ok(self.execute(request, Some(sink)).await?.completion)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::lifecycle::ExecutionUpdate;
    use harness::provider::{PromptMode, ProviderInput, ProviderLaunch, SteeringInput};

    /// Keep the production observer active after a complete-result waiter returns.
    #[tokio::test]
    async fn collected_result_retains_idle_continuation_observation() -> anyhow::Result<()> {
        let workspace = tempfile::tempdir()?;
        let trace = Arc::new(harness::trace::TraceStore::open(workspace.path())?);
        let provider: Arc<harness::provider::Provider> = Arc::from(harness::provider::build(
            ProviderLaunch {
                kind: "mock".into(),
                command: Vec::new(),
            },
            harness::provider::approval::PermissionCoordinator::transient(workspace.path())?,
            trace,
            workspace.path().into(),
        )?);
        let fixture = ProviderTestSession::from_dyn(provider.clone())?;
        let owner = ExecutionOwner::root("collector");
        let request = ProviderRequest {
            execution_owner: Some(owner.clone()),
            harness_session_id: "collector".into(),
            workspace: workspace.path().to_string_lossy().into_owned(),
            input: ProviderInput::from_text("hello"),
            mode: PromptMode::Chat,
            model: "mock".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: harness::session::ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        fixture.session_prompt_and_collect(request.clone()).await?;
        let catalog = harness::provider::ProviderCatalogRequest {
            execution_owner: Some(owner.clone()),
            harness_session_id: request.harness_session_id,
            workspace: request.workspace,
            provider_session_id: None,
            access: Default::default(),
            approval_mode: harness::session::ApprovalMode::Ask,
        };
        let receipt = fixture.store.prepare_steering(
            &catalog,
            ProviderInput::from_text("continue"),
            harness::provider::ProviderKind::Mock,
        )?;
        fixture
            .steer_current(
                owner.clone(),
                SteeringInput::User {
                    text: "continue".into(),
                    attachments: Vec::new(),
                    submission: Some(receipt),
                },
            )
            .await?;
        let execution = fixture.owner(owner).await?;
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                if execution.idle.lock().await.events.iter().any(|event| {
                    matches!(
                        event, ProviderEvent::Execution { observation }
                        if matches!(observation.update(), ExecutionUpdate::Finished { .. })
                    )
                }) {
                    break;
                }
                tokio::task::yield_now().await;
            }
        })
        .await?;
        fixture.close().await?;
        provider.shutdown().await?;
        Ok(())
    }
}
