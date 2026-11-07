use super::client_generation::{
    ClientGeneration, CollectedClient, CollectedSessions, FailedGeneration,
};
use super::compaction::{CompactionMonitor, CompactionSubscription};
use super::handle::{CopilotClient, CopilotSession, SkillSelection};
use super::history_repair::{self, HistoryRepair};
use super::session::SessionOptions;
use super::terminal::CopilotTerminalMonitor;
use super::turn::CopilotTurnEvent;
use anyhow::{Context, Result};
use forge_provider_api::control::{ControlToolDefinition, ProviderTools};
use forge_provider_api::permissions::ProviderPermissions;
use forge_provider_api::terminal::TerminalSnapshot;
use forge_provider_api::trace::ProviderTrace;
use forge_provider_api::{Compaction, ProviderError, ProviderEventSink, ThreadOwner};
use serde_json::{Map, Value};
use std::collections::HashMap;
use std::future::Future;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Select a compatible binding or replace it before applying changed resources.
#[derive(Clone, Copy, PartialEq)]
pub enum SessionReuse {
    /// Keep an existing session when its tool catalog still matches.
    Compatible,
    /// Prepare a replacement even when the catalog is unchanged.
    Refresh,
}

/// Whether shutdown collected the owning SDK client generation.
pub enum ClientShutdown {
    Collected,
    Uncertain,
}

/// Select native history and model settings independently of host workflow state.
pub struct SessionSelection {
    /// Retain callback authority even when this preparation has no event subscriber yet.
    pub execution_owner: forge_provider_api::ThreadOwner,
    /// Correlate diagnostics with the host session.
    pub correlation_id: String,
    /// Resume this native conversation only when no binding is retained.
    pub session_id: Option<String>,
    /// Override the native model when explicitly selected.
    pub model: Option<String>,
    /// Override the native context tier for the selected model.
    pub context_tier: Option<String>,
    /// Require this tool catalog, or retain the catalog on inspection-only calls.
    pub tools: Option<Vec<ControlToolDefinition>>,
    /// Choose whether compatible native history may stay attached.
    pub reuse: SessionReuse,
}

/// Retain the host's resource lease for exactly the lifetime of its native binding.
pub struct PreparedSession<P> {
    /// Configure the replacement session.
    pub options: SessionOptions,
    /// Reconcile selected skills only when the host manages resources.
    pub skills: Option<Vec<SkillSelection>>,
    /// Launch the selected MCP servers only for managed resources.
    pub mcp: Option<Vec<(String, Map<String, Value>)>>,
    /// Keep host resource files alive until the binding is retired.
    pub profile: Option<P>,
}

type SessionSlot<P> = Arc<Mutex<Option<SessionBinding<P>>>>;
type StartupTask =
    Arc<Mutex<Option<tokio::task::JoinHandle<Result<Arc<CopilotSession>, ProviderError>>>>>;

/// Own native session replacement, history boundaries, and background readers.
pub struct CopilotSessions<P> {
    slots: Mutex<HashMap<ThreadOwner, SessionSlot<P>>>,
    provisional: Arc<std::sync::Mutex<HashMap<ThreadOwner, Arc<CopilotSession>>>>,
    leases: Arc<std::sync::Mutex<HashMap<ThreadOwner, Arc<P>>>>,
    startups: std::sync::Mutex<HashMap<ThreadOwner, StartupTask>>,
    failed_startups: std::sync::Mutex<HashMap<ThreadOwner, Arc<ProviderError>>>,
    terminals: Mutex<HashMap<ThreadOwner, CopilotTerminalMonitor>>,
    subscriptions: Mutex<HashMap<ThreadOwner, CompactionSubscription>>,
    turns: Mutex<HashMap<ThreadOwner, Arc<CopilotTurnEvent>>>,
    trace: Arc<dyn ProviderTrace>,
    generations: Mutex<HashMap<ThreadOwner, GenerationBinding>>,
    collections: Mutex<Vec<SessionCollection>>,
}

struct GenerationBinding {
    owner: forge_provider_api::ThreadOwner,
    generation: ClientGeneration,
    observer: Option<tokio::task::JoinHandle<()>>,
}

struct SessionCollection {
    failed: FailedGeneration,
    state: SessionCollectionState,
}

enum SessionCollectionState {
    Running(tokio::task::JoinHandle<Result<(), Arc<ProviderError>>>),
    Complete(Result<(), Arc<ProviderError>>),
}

enum SessionHistory {
    Prepared,
    Submitted,
}

struct SessionBinding<P> {
    session: Arc<CopilotSession>,
    history: SessionHistory,
    repair: Option<HistoryRepair>,
    tool_schema: Vec<Value>,
    compaction: Option<CompactionMonitor>,
    mcp_failure: HashMap<String, String>,
    _profile: Option<Arc<P>>,
}

impl<P: Send + Sync + 'static> CopilotSessions<P> {
    /// Create an empty registry using the host's diagnostic sink.
    pub fn new(trace: Arc<dyn ProviderTrace>) -> Self {
        Self {
            slots: Mutex::new(HashMap::new()),
            provisional: Arc::new(std::sync::Mutex::new(HashMap::new())),
            leases: Arc::new(std::sync::Mutex::new(HashMap::new())),
            startups: std::sync::Mutex::new(HashMap::new()),
            failed_startups: std::sync::Mutex::new(HashMap::new()),
            terminals: Mutex::new(HashMap::new()),
            subscriptions: Mutex::new(HashMap::new()),
            turns: Mutex::new(HashMap::new()),
            trace,
            generations: Mutex::new(HashMap::new()),
            collections: Mutex::new(Vec::new()),
        }
    }

    /// Reuse or replace a native binding under one session's preparation lock.
    pub async fn acquire<F, Fut, V>(
        &self,
        client: impl Future<Output = Result<(CopilotClient, Option<String>)>> + Send,
        selection: SessionSelection,
        permissions: Arc<dyn ProviderPermissions>,
        tools: Arc<dyn ProviderTools>,
        prepare: F,
        validate: V,
    ) -> Result<Arc<CopilotSession>>
    where
        F: FnOnce() -> Fut + Send,
        Fut: Future<Output = Result<PreparedSession<P>>> + Send,
        V: FnOnce(&CopilotSession) -> Result<()> + Send,
    {
        let slot = self.slot(&selection.execution_owner).await;
        let turn = self.turn(&selection.execution_owner).await;
        let mut active = slot.lock().await;
        self.ensure_startup_collected(&selection.execution_owner)?;
        let _ = self.collect_startup(&selection.execution_owner).await?;
        let prior = self
            .provisional
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
            .get(&selection.execution_owner)
            .cloned();
        if let Some(previous) = prior {
            previous
                .disconnect()
                .await
                .map_err(|cleanup| ProviderError::PreparationCleanup {
                    cause: Box::new(super::error::native_failure(
                        forge_provider_api::ProviderOperation::ThreadOpen,
                        forge_provider_api::ProviderFailureKind::Runtime,
                    )),
                    cleanup: Box::new(cleanup),
                })?;
            self.provisional
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
                .remove(&selection.execution_owner);
            self.leases
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                .remove(&selection.execution_owner);
        }
        if let Some(binding) = active.as_mut().filter(|binding| {
            selection.reuse == SessionReuse::Compatible
                && !binding.session.client_stopped()
                && selection.tools.as_ref().is_none_or(|tools| {
                    tools
                        .iter()
                        .map(|tool| tool.mcp_value())
                        .collect::<Vec<_>>()
                        == binding.tool_schema
                })
        }) {
            binding.session.ensure_healthy()?;
            if binding.compaction.is_none() {
                binding.compaction = self.monitor(&selection, &binding.session, turn).await;
            }
            if let Some(model) = &selection.model {
                let (_, reasoning_effort) = client.await?;
                binding
                    .session
                    .set_model(model, reasoning_effort, selection.context_tier.as_deref())
                    .await
                    .context("set Copilot session model")?;
            }
            return Ok(binding.session.clone());
        }

        // Preserve the old binding until preparation succeeds; same-ID resume must
        // unregister its SDK router before registering the replacement.
        let fresh_resume = active.is_none() && selection.session_id.is_some();
        let previous_repair = active.as_ref().and_then(|binding| binding.repair.clone());
        let retained_id = match active.as_ref() {
            Some(binding) => match binding.history {
                SessionHistory::Prepared => None,
                SessionHistory::Submitted => Some(binding.session.id().to_owned()),
            },
            None => selection.session_id.clone(),
        };
        let (client, reasoning_effort) = client.await?;
        client.ensure_healthy()?;
        self.bind_generation(
            &selection.execution_owner,
            client.generation.clone(),
            selection.execution_owner.clone(),
        )
        .await?;
        let mut prepared = prepare().await?;
        prepared.options.reasoning_effort = reasoning_effort;
        prepared.options.session_id = retained_id.clone();
        let tool_schema = prepared
            .options
            .tools
            .iter()
            .map(|tool| tool.mcp_value())
            .collect();
        if active.is_some() {
            super::operation::dispatch_for(forge_provider_api::ProviderOperation::ThreadConfigure)?;
            self.disconnect(&selection.execution_owner, &mut active)
                .await?;
        }
        let startup_key = selection.execution_owner.clone();
        let provisional = self.provisional.clone();
        let trace = self.trace.clone();
        let correlation_id = selection.correlation_id.clone();
        let isolated_text = prepared.options.isolated_text;
        let options = prepared.options;
        let profile = prepared.profile.map(Arc::new);
        if let Some(profile) = &profile {
            self.leases
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                .insert(selection.execution_owner.clone(), profile.clone());
        }
        let worker_profile = profile.clone();
        let native_operation = super::operation::current();
        let task = tokio::spawn(super::operation::scope(native_operation, async move {
            let _profile = worker_profile;
            let session = Arc::new(
                super::session::open(&client, options, permissions, tools, trace, &correlation_id)
                    .await?,
            );
            provisional
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .insert(startup_key, session.clone());
            Ok(session)
        }));
        let startup = Arc::new(Mutex::new(Some(task)));
        self.startups
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot startup registry poisoned"))?
            .insert(selection.execution_owner.clone(), startup);
        let session = self
            .collect_startup(&selection.execution_owner)
            .await?
            .context("Copilot startup task missing")?;
        let configuration = async {
            if isolated_text {
                super::session::confirm_isolated_instructions(&session).await?;
            }
            let repair = if fresh_resume {
                history_repair::repair(&session).await?
            } else {
                previous_repair
            };
            if let Some(skills) = prepared.skills {
                session.reconcile_skills(&skills).await?;
            }
            let failures = if let Some(mcp) = prepared.mcp {
                super::mcp::start(
                    &session,
                    &mcp,
                    self.trace.as_ref(),
                    &selection.correlation_id,
                )
                .await?
            } else {
                HashMap::new()
            };
            self.disconnect(&selection.execution_owner, &mut active)
                .await?;
            validate(&session)?;
            Ok::<_, anyhow::Error>((repair, failures))
        }
        .await;
        let (repair, mcp_failure) = match configuration {
            Ok(configured) => configured,
            Err(error) => {
                let cause = error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::ThreadOpen,
                        forge_provider_api::ProviderFailureKind::Runtime,
                    )
                });
                if let Err(cleanup) = session.disconnect().await {
                    return Err(ProviderError::PreparationCleanup {
                        cause: Box::new(cause),
                        cleanup: Box::new(cleanup),
                    }
                    .into());
                }
                self.provisional
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
                    .remove(&selection.execution_owner);
                self.leases
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                    .remove(&selection.execution_owner);
                return Err(cause.into());
            }
        };
        let compaction = self.monitor(&selection, &session, turn).await;
        *active = Some(SessionBinding {
            session: session.clone(),
            history: if retained_id.is_some() {
                SessionHistory::Submitted
            } else {
                SessionHistory::Prepared
            },
            repair,
            tool_schema,
            compaction,
            mcp_failure,
            _profile: profile,
        });
        self.provisional
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
            .remove(&selection.execution_owner);
        self.leases
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
            .remove(&selection.execution_owner);
        Ok(session)
    }

    /// Get the currently published native binding without creating one.
    pub async fn active(&self, key: &ThreadOwner) -> Option<Arc<CopilotSession>> {
        let slot = self.slots.lock().await.get(key).cloned()?;
        slot.lock()
            .await
            .as_ref()
            .map(|binding| binding.session.clone())
    }

    /// Record possible native input only against the binding that will receive it.
    pub async fn mark_submitted(
        &self,
        key: &ThreadOwner,
        session: &Arc<CopilotSession>,
    ) -> Result<(), ProviderError> {
        let slot = self.slot(key).await;
        let mut active = slot.lock().await;
        let binding = active
            .as_mut()
            .filter(|binding| Arc::ptr_eq(&binding.session, session))
            .ok_or_else(|| {
                super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?;
        binding.history = SessionHistory::Submitted;
        Ok(())
    }

    /// Get the repair caution associated with the published history.
    pub async fn repair(&self, key: &ThreadOwner) -> Option<HistoryRepair> {
        self.slot(key)
            .await
            .lock()
            .await
            .as_ref()
            .and_then(|binding| binding.repair.clone())
    }

    /// Get startup failures without exposing mutable native session state.
    pub async fn mcp_failures(&self, key: &ThreadOwner) -> HashMap<String, String> {
        self.slot(key)
            .await
            .lock()
            .await
            .as_ref()
            .map(|binding| binding.mcp_failure.clone())
            .unwrap_or_default()
    }

    /// Get the control lane shared by native prompt and cancellation operations.
    pub async fn turn(&self, key: &ThreadOwner) -> Arc<CopilotTurnEvent> {
        self.turns
            .lock()
            .await
            .entry(key.to_owned())
            .or_default()
            .clone()
    }

    /// Bind background compaction observations before creating a native session.
    pub async fn subscribe(&self, key: ThreadOwner, sink: ProviderEventSink) {
        self.subscriptions
            .lock()
            .await
            .insert(key.clone(), CompactionSubscription::new(sink));
        let generation = self
            .generations
            .lock()
            .await
            .get(&key)
            .map(|binding| (binding.generation.clone(), binding.owner.clone()));
        // Existing bindings normally subscribe before acquisition. A later
        // observer must still receive a previously established client loss.
        if let Some((generation, owner)) = generation
            && let Err(error) = self.bind_generation(&key, generation, owner).await
            && let Some(subscription) = self.subscriptions.lock().await.get(&key)
        {
            subscription
                .sink
                .fail(forge_provider_api::events::EventDeliveryError::transport(
                    error,
                ));
        }
    }

    /// Fail every source observer sharing this client before callbacks or readers are retired.
    pub async fn fail_generation(
        &self,
        failed: &FailedGeneration,
    ) -> Result<Vec<forge_provider_api::ThreadOwner>, ProviderError> {
        let keys = self
            .generations
            .lock()
            .await
            .iter()
            .filter(|(_, binding)| failed.matches(&binding.generation))
            .map(|(key, binding)| (key.clone(), binding.owner.clone()))
            .collect::<Vec<_>>();
        let targets = {
            let subscriptions = self.subscriptions.lock().await;
            keys.into_iter()
                .map(|(key, owner)| {
                    (
                        owner,
                        subscriptions
                            .get(&key)
                            .map(|subscription| subscription.sink.clone()),
                    )
                })
                .collect::<Vec<_>>()
        };
        let mut owners = Vec::new();
        for (owner, sink) in targets {
            if let Some(sink) = sink {
                sink.observation_unavailable(failed.failure()).await;
            }
            owners.push(owner);
        }
        Ok(owners)
    }

    /// Join failed-session cleanup without transferring its worker to a cancellable waiter.
    pub async fn collect_failed_generation(
        self: &Arc<Self>,
        client: &CollectedClient,
    ) -> Result<CollectedSessions, ProviderError> {
        let mut collections = self.collections.lock().await;
        let index = match collections
            .iter()
            .position(|entry| entry.failed.same(client.failed_generation()))
        {
            Some(index) => index,
            None => {
                let owner = self.clone();
                let proof = client.clone();
                collections.push(SessionCollection {
                    failed: client.failed_generation().clone(),
                    state: SessionCollectionState::Running(tokio::spawn(async move {
                        owner.collect_failed_work(&proof).await.map_err(Arc::new)
                    })),
                });
                collections.len() - 1
            }
        };
        let entry = &mut collections[index];
        if let SessionCollectionState::Running(task) = &mut entry.state {
            let joined = super::operation::wait(task).await.map_err(|_| {
                ProviderError::Cleanup(Arc::new(
                    forge_provider_api::error::ObservationCleanup::new(
                        client.failed_generation().failure(),
                        None,
                        None,
                        Some(forge_provider_api::error::CleanupFailure::TimedOut),
                    ),
                ))
            })?;
            entry.state = SessionCollectionState::Complete(joined.unwrap_or_else(|_error| {
                Err(Arc::new(super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )))
            }));
        }
        match &entry.state {
            SessionCollectionState::Complete(Ok(())) => {
                collections.remove(index);
                Ok(CollectedSessions::new(client))
            }
            SessionCollectionState::Complete(Err(cleanup)) => {
                Err(ProviderError::PreparationCleanup {
                    cause: Box::new(ProviderError::Observation(
                        client.failed_generation().failure(),
                    )),
                    cleanup: Box::new(cleanup.as_ref().clone()),
                })
            }
            SessionCollectionState::Running(_) => {
                unreachable!("joined session collection has a retained result")
            }
        }
    }

    /// Preserve idle failure delivery independently of active invocation contexts or queue capacity.
    async fn bind_generation(
        &self,
        key: &ThreadOwner,
        generation: ClientGeneration,
        owner: forge_provider_api::ThreadOwner,
    ) -> Result<(), ProviderError> {
        if &owner != key {
            return Err(super::error::native_failure(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Rejected,
            ));
        }
        let sink = self
            .subscriptions
            .lock()
            .await
            .get(key)
            .map(|subscription| subscription.sink.clone());
        if let (Some(sink), Some(failure)) = (&sink, generation.failure()) {
            sink.observation_unavailable(failure).await;
        }
        let mut generations = self.generations.lock().await;
        // Serialize admission with the collector's key snapshot. A stopped
        // generation must never acquire a late binding after its collection proof.
        generation.ensure_healthy()?;
        if let Some(previous) = generations.get(key) {
            if !previous.generation.same(&generation) {
                return Err(super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ));
            }
            if previous.observer.is_some() {
                return Ok(());
            }
        }
        let observer = sink.map(|sink| {
            let mut failure = generation.failures();
            tokio::spawn(async move {
                loop {
                    let reason = failure.borrow().clone();
                    if let Some(reason) = reason {
                        sink.observation_unavailable(reason).await;
                        break;
                    }
                    if failure.changed().await.is_err() {
                        break;
                    }
                }
            })
        });
        generations.insert(
            key.to_owned(),
            GenerationBinding {
                owner,
                generation,
                observer,
            },
        );
        Ok(())
    }

    /// Collect only this failed client's local work; never issue detach over its stopped transport.
    async fn collect_failed_work(&self, proof: &CollectedClient) -> Result<(), ProviderError> {
        self.fail_generation(proof.failed_generation()).await?;
        let keys = self
            .generations
            .lock()
            .await
            .iter()
            .filter(|(_, binding)| proof.failed_generation().matches(&binding.generation))
            .map(|(key, _)| key.clone())
            .collect::<Vec<_>>();
        for key in keys {
            let startup = self
                .startups
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .get(&key)
                .cloned();
            if let Some(startup) = startup {
                let mut retained = startup.lock().await;
                if let Some(task) = retained.as_mut() {
                    match task.await {
                        Ok(Ok(session)) => session.local_collect_after_client_stop(proof).await?,
                        Ok(Err(ProviderError::PreparationCleanup { cause, cleanup })) => {
                            return Err(ProviderError::PreparationCleanup { cause, cleanup });
                        }
                        Ok(Err(_)) => {}
                        Err(_error) => {
                            return Err(super::error::native_failure(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Protocol,
                            ));
                        }
                    }
                    retained.take();
                }
                self.startups
                    .lock()
                    .map_err(|_| {
                        super::error::native_failure(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Protocol,
                        )
                    })?
                    .remove(&key);
            }
            if let Some(mut monitor) = self.terminals.lock().await.remove(&key) {
                monitor.shutdown().await?;
            }
            let slot = self.slots.lock().await.get(&key).cloned();
            if let Some(slot) = slot {
                let mut active = slot.lock().await;
                if let Some(binding) = active.as_mut() {
                    if !binding.session.belongs_to(proof.failed_generation()) {
                        return Err(super::error::native_failure(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ));
                    }
                    if let Some(monitor) = binding.compaction.as_mut() {
                        monitor.shutdown().await?;
                    }
                    binding
                        .session
                        .local_collect_after_client_stop(proof)
                        .await?;
                }
                active.take();
            }
            let provisional = self
                .provisional
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .get(&key)
                .cloned();
            if let Some(session) = provisional {
                session.local_collect_after_client_stop(proof).await?;
            }
            self.provisional
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .remove(&key);
            self.leases
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .remove(&key);
            self.failed_startups
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .remove(&key);
            let observer = self
                .generations
                .lock()
                .await
                .get_mut(&key)
                .and_then(|binding| binding.observer.take());
            if let Some(observer) = observer {
                // Sticky failure is already published; this owned worker joins its failure relay.
                observer.await.map_err(|_error| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?;
            }
            self.generations.lock().await.remove(&key);
            self.subscriptions.lock().await.remove(&key);
            self.turns.lock().await.remove(&key);
        }
        Ok(())
    }

    /// Correlate one manual compaction with native notifications around its RPC.
    pub async fn compact(
        &self,
        key: &ThreadOwner,
        id: String,
    ) -> Result<Compaction, ProviderError> {
        let slot = self.slot(key).await;
        let (session, manual) = {
            let active = slot.lock().await;
            let binding = active.as_ref().ok_or_else(|| {
                super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?;
            (
                binding.session.clone(),
                binding
                    .compaction
                    .as_ref()
                    .map(|monitor| monitor.manual.clone()),
            )
        };
        if let Some(manual) = &manual {
            manual.lock().await.register(id.clone());
        }
        let result = session.compact(id.clone()).await;
        if let Some(manual) = &manual {
            manual.lock().await.finish(&id);
        }
        result
    }

    /// Prove a refresh retains the submitted native identity before preserving its shell helper.
    pub(super) async fn retains_identity_on_refresh(&self, key: &ThreadOwner) -> bool {
        let slot = self.slot(key).await;
        slot.lock()
            .await
            .as_ref()
            .is_some_and(|binding| matches!(binding.history, SessionHistory::Submitted))
    }

    /// Attach native terminal notifications before publishing a new or configured binding.
    pub async fn observe_terminals(
        &self,
        publication: super::terminal::TerminalPublication,
    ) -> Result<()> {
        let session = self
            .active(&publication.owner)
            .await
            .context("terminal session is not attached")?;
        session
            .events
            .bind(publication.sink.activity_writer())
            .await?;
        let mut monitors = self.terminals.lock().await;
        if let Some(monitor) = monitors.get_mut(&publication.owner) {
            if monitor.observes(&session) {
                return Ok(());
            }
            monitor.shutdown().await?;
            *publication.registry.lock().await = super::terminal::TerminalRegistry::default();
        }
        monitors.insert(
            publication.owner.clone(),
            CopilotTerminalMonitor::new(&session, self.trace.clone(), publication),
        );
        Ok(())
    }

    /// Get the latest streamed inventory without issuing a native request.
    pub async fn terminals(&self, key: &ThreadOwner) -> Result<TerminalSnapshot> {
        match self
            .terminals
            .lock()
            .await
            .get(key)
            .and_then(CopilotTerminalMonitor::snapshot)
        {
            Some(result) => result.map_err(anyhow::Error::from),
            None => Ok(TerminalSnapshot {
                supported: true,
                starting: true,
                terminal: Vec::new(),
            }),
        }
    }

    /// Stop one native command and invalidate its retained snapshot.
    pub async fn terminate(&self, key: &ThreadOwner, id: &str) -> Result<()> {
        self.active(key)
            .await
            .context("Copilot session is not attached")?
            .terminate_terminal(id)
            .await?;
        Ok(())
    }

    /// Join every retained observation reader, preserving failed handles for a subsequent cleanup.
    pub async fn close_observers(&self) -> Result<(), ProviderError> {
        let mut failures = Vec::new();
        {
            let mut terminals = self.terminals.lock().await;
            let mut collected = Vec::new();
            for (key, monitor) in terminals.iter_mut() {
                match monitor.shutdown().await {
                    Ok(()) => collected.push(key.clone()),
                    Err(error) => failures.push(error),
                }
            }
            for key in collected {
                terminals.remove(&key);
            }
        }
        let slots = self
            .slots
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        let mut sessions = Vec::new();
        for slot in slots {
            if let Some(binding) = slot.lock().await.as_mut() {
                if let Some(monitor) = binding.compaction.as_mut() {
                    match monitor.shutdown().await {
                        Ok(()) => {
                            binding.compaction.take();
                        }
                        Err(error) => failures.push(error),
                    }
                }
                sessions.push(binding.session.clone());
            }
        }
        sessions.extend(
            self.provisional
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::Shutdown,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .values()
                .cloned(),
        );
        for session in sessions {
            if let Err(error) = session.events.close().await {
                failures.push(error);
            }
        }
        if failures.is_empty() {
            self.subscriptions.lock().await.clear();
            return Ok(());
        }
        let primary = failures.remove(0);
        Err(if failures.is_empty() {
            primary
        } else {
            ProviderError::Combined {
                primary: Box::new(primary),
                secondary: failures,
            }
        })
    }

    /// Retain the native profile independently until binding cleanup has joined every foreground worker.
    pub async fn resources(&self, key: &ThreadOwner) -> Option<Arc<P>> {
        let slot = self.slots.lock().await.get(key).cloned()?;
        let active = slot.lock().await;
        active.as_ref().and_then(|binding| binding._profile.clone())
    }

    /// Disconnect one collected owner before allowing its history to be resumed.
    pub async fn retire(&self, key: &ThreadOwner) -> Result<()> {
        self.ensure_startup_collected(key)?;
        let slot = self.slots.lock().await.get(key).cloned();
        if let Some(slot) = slot {
            let mut binding = slot.lock().await;
            self.disconnect(key, &mut binding).await?;
        }
        let _ = self.collect_startup(key).await?;
        let provisional = self
            .provisional
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
            .get(key)
            .cloned();
        if let Some(session) = provisional {
            session.disconnect().await?;
            self.provisional
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
                .remove(key);
        }
        self.leases
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
            .remove(key);
        self.subscriptions.lock().await.remove(key);
        self.turns.lock().await.remove(key);
        self.retire_generation_observer(key).await?;
        Ok(())
    }

    /// Join a healthy owner's failure relay before removing its generation registration.
    async fn retire_generation_observer(&self, key: &ThreadOwner) -> Result<(), ProviderError> {
        let mut generations = self.generations.lock().await;
        if let Some(binding) = generations.get_mut(key)
            && let Some(observer) = binding.observer.as_mut()
        {
            observer.abort();
            match observer.await {
                Ok(()) => {}
                Err(error) if error.is_cancelled() => {}
                Err(_error) => {
                    return Err(super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    ));
                }
            }
        }
        generations.remove(key);
        Ok(())
    }

    /// Release bindings after the owning SDK client has fully stopped.
    pub async fn clear(&self, client_stopped: ClientShutdown) -> Result<(), ProviderError> {
        let keys: Vec<_> = self
            .startups
            .lock()
            .map_err(|_| {
                super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?
            .keys()
            .cloned()
            .collect();
        let mut failed_startups: Vec<_> = self
            .failed_startups
            .lock()
            .map_err(|_| {
                super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?
            .iter()
            .map(|(key, error)| (key.clone(), error.as_ref().clone()))
            .collect();
        failed_startups.sort_by(|left, right| {
            (left.0.session_id().as_str(), left.0.agent_id().as_str())
                .cmp(&(right.0.session_id().as_str(), right.0.agent_id().as_str()))
        });
        let mut failures: Vec<_> = failed_startups
            .into_iter()
            .map(|(_, error)| error)
            .collect();
        if let Err(error) = self.close_observers().await {
            failures.push(error);
        }
        for key in keys {
            if let Err(error) = self.collect_startup(&key).await {
                failures.push(error.downcast::<ProviderError>().unwrap_or_else(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::ThreadOpen,
                        forge_provider_api::ProviderFailureKind::Runtime,
                    )
                }));
            }
        }
        let slots: Vec<_> = self
            .slots
            .lock()
            .await
            .iter()
            .map(|(key, slot)| (key.clone(), slot.clone()))
            .collect();
        let provisional: Vec<_> = {
            let pending = self.provisional.lock().map_err(|_| {
                super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?;
            pending
                .iter()
                .map(|(key, session)| (key.clone(), session.clone()))
                .collect()
        };
        for (key, session) in provisional {
            if !matches!(client_stopped, ClientShutdown::Collected) {
                failures.push(super::error::native_failure(
                    forge_provider_api::ProviderOperation::Shutdown,
                    forge_provider_api::ProviderFailureKind::ResourceConflict,
                ));
                continue;
            }
            match session.disconnect().await {
                Err(error) => {
                    failures.push(error);
                }
                Ok(()) => {
                    self.provisional
                        .lock()
                        .map_err(|_| {
                            super::error::native_failure(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Protocol,
                            )
                        })?
                        .remove(&key);
                    self.leases
                        .lock()
                        .map_err(|_| {
                            super::error::native_failure(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Protocol,
                            )
                        })?
                        .remove(&key);
                }
            }
        }
        for (key, slot) in slots {
            let mut retained = slot.lock().await;
            if !matches!(client_stopped, ClientShutdown::Collected) {
                failures.push(super::error::native_failure(
                    forge_provider_api::ProviderOperation::Shutdown,
                    forge_provider_api::ProviderFailureKind::ResourceConflict,
                ));
                continue;
            }
            if let Some(binding) = retained.as_ref()
                && let Err(error) = binding.session.disconnect().await
            {
                failures.push(error);
                continue;
            }
            retained.take();
            self.slots.lock().await.remove(&key);
        }
        let keys = self
            .generations
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for key in keys {
            if let Err(error) = self.retire_generation_observer(&key).await {
                failures.push(error);
            }
        }
        self.turns.lock().await.clear();
        if matches!(client_stopped, ClientShutdown::Collected)
            && self.slots.lock().await.is_empty()
            && self
                .startups
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .is_empty()
            && self
                .provisional
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .is_empty()
        {
            self.failed_startups
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .clear();
            self.leases
                .lock()
                .map_err(|_| {
                    super::error::native_failure(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    )
                })?
                .clear();
        }
        let mut failures = failures.into_iter();
        let Some(primary) = failures.next() else {
            return Ok(());
        };
        let secondary: Vec<_> = failures.collect();
        if secondary.is_empty() {
            Err(primary)
        } else {
            Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary,
            })
        }
    }

    /// Get the preparation lock for one native session owner.
    async fn slot(&self, key: &ThreadOwner) -> SessionSlot<P> {
        self.slots
            .lock()
            .await
            .entry(key.to_owned())
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone()
    }

    /// Join one owned startup without surrendering its handle when the waiter is cancelled.
    async fn collect_startup(&self, key: &ThreadOwner) -> Result<Option<Arc<CopilotSession>>> {
        let startup = self
            .startups
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot startup registry poisoned"))?
            .get(key)
            .cloned();
        let Some(startup) = startup else {
            return Ok(None);
        };
        let mut guard = startup.lock().await;
        let task = guard
            .as_mut()
            .context("Copilot startup already collected")?;
        let result = match super::operation::wait(task).await {
            Ok(result) => result,
            Err(_) => {
                return Err(ProviderError::PreparationCleanup {
                    cause: Box::new(super::error::native_failure(
                        forge_provider_api::ProviderOperation::ThreadOpen,
                        forge_provider_api::ProviderFailureKind::Runtime,
                    )),
                    cleanup: Box::new(ProviderError::DeliveryUncertain),
                }
                .into());
            }
        };
        guard.take();
        self.startups
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot startup registry poisoned"))?
            .remove(key);
        match result {
            Ok(Ok(session)) => Ok(Some(session)),
            Ok(Err(error)) => {
                if matches!(&error, ProviderError::PreparationCleanup { .. }) {
                    self.failed_startups
                        .lock()
                        .map_err(|_| anyhow::anyhow!("Copilot startup failure registry poisoned"))?
                        .insert(key.to_owned(), Arc::new(error));
                    self.ensure_startup_collected(key)?;
                    unreachable!()
                }
                self.leases
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                    .remove(key);
                Err(error.into())
            }
            Err(_error) => {
                let error = Arc::new(super::error::native_failure(
                    forge_provider_api::ProviderOperation::ThreadOpen,
                    forge_provider_api::ProviderFailureKind::Runtime,
                ));
                self.failed_startups
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot startup failure registry poisoned"))?
                    .insert(key.to_owned(), error);
                self.ensure_startup_collected(key)?;
                unreachable!()
            }
        }
    }

    /// Fence reuse while a startup failed without proving its cleanup.
    fn ensure_startup_collected(&self, key: &ThreadOwner) -> Result<()> {
        if let Some(error) = self
            .failed_startups
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot startup failure registry poisoned"))?
            .get(key)
        {
            return Err(ProviderError::PreparationCleanup {
                cause: Box::new(error.as_ref().clone()),
                cleanup: Box::new(super::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::Runtime,
                )),
            }
            .into());
        }
        Ok(())
    }

    /// Create an observer for the current native binding, if the host subscribed.
    async fn monitor(
        &self,
        selection: &SessionSelection,
        session: &Arc<CopilotSession>,
        turn: Arc<CopilotTurnEvent>,
    ) -> Option<CompactionMonitor> {
        self.subscriptions
            .lock()
            .await
            .get(&selection.execution_owner)
            .cloned()
            .map(|subscription| {
                CompactionMonitor::new(
                    session,
                    subscription,
                    turn,
                    self.trace.clone(),
                    selection.correlation_id.clone(),
                )
            })
    }

    /// Join old observers and retire its native router while retaining every unfinished handle.
    async fn disconnect(
        &self,
        key: &ThreadOwner,
        active: &mut Option<SessionBinding<P>>,
    ) -> Result<()> {
        let Some(binding) = active.as_mut() else {
            return Ok(());
        };
        {
            let mut terminals = self.terminals.lock().await;
            if let Some(monitor) = terminals.get_mut(key) {
                monitor.shutdown().await?;
            }
            terminals.remove(key);
        }
        if let Some(monitor) = binding.compaction.as_mut() {
            monitor.shutdown().await?;
        }
        binding.compaction.take();
        binding
            .session
            .disconnect()
            .await
            .context("disconnect Copilot session before refreshing tools")?;
        active.take();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};

    mod completion_ancestry;
    mod recovery;
    mod startup_loss;
    mod support;

    struct Trace;

    /// Distinct structured owners cannot alias a native slot, turn, or event subscription.
    #[tokio::test]
    async fn structured_owners_keep_colliding_text_labels_independent() {
        let sessions = CopilotSessions::<()>::new(Arc::new(Trace));
        let child = ThreadOwner::new(
            "a".into(),
            forge_provider_api::agent::AgentId::new("b").unwrap(),
        )
        .expect("registered session and agent identities are nonempty");
        let root = ThreadOwner::root("a:agent:b");
        assert_ne!(child, root);
        assert!(!Arc::ptr_eq(
            &sessions.slot(&child).await,
            &sessions.slot(&root).await
        ));
        assert!(!Arc::ptr_eq(
            &sessions.turn(&child).await,
            &sessions.turn(&root).await
        ));
        for owner in [child, root] {
            let (sink, _) = forge_provider_api::events::channel(
                forge_provider_api::BindingId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
                forge_provider_api::EventScope::Binding {
                    owner: owner.clone(),
                },
            );
            sessions.subscribe(owner, sink).await;
        }
        assert_eq!(sessions.subscriptions.lock().await.len(), 2);
        sessions.close_observers().await.unwrap();
    }

    impl ProviderTrace for Trace {
        /// Discard fixture diagnostics independently of the native transport assertions.
        fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
    }

    struct Host;
    #[async_trait]
    impl ProviderPermissions for Host {
        /// Reject unexpected native permission callbacks in the startup fixture.
        async fn authorize(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: forge_provider_api::permissions::PermissionRequest,
        ) -> forge_provider_api::permissions::PermissionResponse {
            forge_provider_api::permissions::PermissionResponse::Cancelled
        }
        /// Keep the fixture's host barrier open without authorizing tool calls.
        async fn before_tool(
            &self,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), ProviderError> {
            Ok(())
        }
    }
    #[async_trait]
    impl ProviderTools for Host {
        /// Reject unexpected native tool execution before prompt dispatch.
        async fn call(
            &self,
            _: &forge_provider_api::InvocationScope,
            _: forge_provider_api::control::ToolCall,
        ) -> Result<String, ProviderError> {
            Err(crate::copilot::error::native_failure(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        }
    }

    /// Read a single SDK JSON-RPC frame from the fake transport.
    async fn read_frame(reader: &mut tokio::io::DuplexStream) -> Value {
        let mut header = Vec::new();
        loop {
            header.push(reader.read_u8().await.unwrap());
            if header.ends_with(b"\r\n\r\n") {
                break;
            }
        }
        let header = std::str::from_utf8(&header).unwrap();
        let size: usize = header
            .trim()
            .strip_prefix("Content-Length: ")
            .unwrap()
            .parse()
            .unwrap();
        let mut body = vec![0; size];
        reader.read_exact(&mut body).await.unwrap();
        serde_json::from_slice(&body).unwrap()
    }

    /// Reply to one SDK request using its original RPC identifier.
    async fn reply_frame(writer: &mut tokio::io::DuplexStream, request: &Value, result: Value) {
        let body = serde_json::to_vec(
            &serde_json::json!({"jsonrpc":"2.0", "id":request["id"], "result":result}),
        )
        .unwrap();
        writer
            .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
            .await
            .unwrap();
        writer.write_all(&body).await.unwrap();
        writer.flush().await.unwrap();
    }

    /// Exclude inherited servers before create/resume while retaining explicitly selected servers.
    #[tokio::test]
    async fn inherited_mcp_is_disabled_before_create_and_resume() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            for isolated_text in [false, true] {
                for resumed in [false, true] {
                    for selected in [false, true] {
                        let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
                        let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
                        let client = CopilotClient::new(
                            github_copilot_sdk::Client::from_streams(
                                client_read,
                                client_write,
                                std::env::temp_dir(),
                            )
                            .unwrap(),
                        );
                        let peer = tokio::spawn(async move {
                            let discovery = read_frame(&mut peer_read).await;
                            assert_eq!(discovery["method"], "mcp.discover");
                            reply_frame(
                            &mut peer_write,
                            &discovery,
                            serde_json::json!({
                                "servers":[
                                    {"name":"github", "source":"builtin", "enabled":true},
                                    {"name":"selected", "source":"user", "enabled":true},
                                    {"name":"workspace-extra", "source":"workspace", "enabled":true}
                                ]
                            }),
                        )
                        .await;
                            let create = read_frame(&mut peer_read).await;
                            assert_eq!(
                                create["method"],
                                if resumed {
                                    "session.resume"
                                } else {
                                    "session.create"
                                }
                            );
                            let expected = if isolated_text || !selected {
                                serde_json::json!(["github", "selected", "workspace-extra"])
                            } else {
                                serde_json::json!(["github", "workspace-extra"])
                            };
                            assert_eq!(create["params"]["disabledMcpServers"], expected);
                            assert_eq!(create["params"]["enableConfigDiscovery"], false);
                            let id = create["params"]["sessionId"].as_str().unwrap();
                            reply_frame(
                                &mut peer_write,
                                &create,
                                serde_json::json!({
                                    "sessionId":id, "workspacePath":std::env::temp_dir(),
                                }),
                            )
                            .await;
                            loop {
                                let request = read_frame(&mut peer_read).await;
                                match request["method"].as_str().unwrap() {
                                    "session.skills.reload" => {
                                        reply_frame(
                                            &mut peer_write,
                                            &request,
                                            serde_json::json!({}),
                                        )
                                        .await;
                                    }
                                    "session.options.update" => {
                                        reply_frame(
                                            &mut peer_write,
                                            &request,
                                            serde_json::json!({"success":true}),
                                        )
                                        .await;
                                    }
                                    "session.detach" => {
                                        reply_frame(
                                            &mut peer_write,
                                            &request,
                                            serde_json::json!({"success":true}),
                                        )
                                        .await;
                                        break;
                                    }
                                    method => panic!("unexpected session setup request: {method}"),
                                }
                            }
                        });
                        let session = super::super::session::open(
                            &client,
                            SessionOptions {
                                isolated_text,
                                session_id: resumed.then(|| "saved-session".to_owned()),
                                model: None,
                                reasoning_effort: None,
                                context_tier: None,
                                workspace: std::env::temp_dir(),
                                system_message: String::new(),
                                resources: super::super::session::SessionResources {
                                    skill_directories: Vec::new(),
                                    selected_mcp: if selected {
                                        vec!["selected".to_owned()]
                                    } else {
                                        Vec::new()
                                    },
                                },
                                excluded_tools: Vec::new(),
                                tools: Vec::new(),
                                native_callbacks: None,
                            },
                            Arc::new(Host),
                            Arc::new(Host),
                            Arc::new(Trace),
                            "mcp-before-create",
                        )
                        .await
                        .unwrap();
                        session.disconnect().await.unwrap();
                        peer.await.unwrap();
                        client.fail_generation(
                            forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                        );
                    }
                }
            }
        })
        .await
        .expect("pre-start MCP selection fixture timed out");
    }

    /// Collect bound and provisional intake before SDK detach can close their subscriptions.
    #[tokio::test]
    async fn shutdown_collects_intake_before_detaching_sessions() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            for provisional in [false, true] {
                let (client_write, mut peer_read) = tokio::io::duplex(65536);
                let (mut peer_write, client_read) = tokio::io::duplex(65536);
                let client = CopilotClient::new(
                    github_copilot_sdk::Client::from_streams(
                        client_read,
                        client_write,
                        std::env::temp_dir(),
                    )
                    .unwrap(),
                );
                let peer = tokio::spawn(async move {
                    let create = read_frame(&mut peer_read).await;
                    reply_frame(
                        &mut peer_write,
                        &create,
                        serde_json::json!({"sessionId":"shutdown", "workspacePath":"."}),
                    )
                    .await;
                    let detach = read_frame(&mut peer_read).await;
                    assert_eq!(detach["method"], "session.detach");
                    reply_frame(
                        &mut peer_write,
                        &detach,
                        serde_json::json!({"success":true}),
                    )
                    .await;
                });
                let native = Arc::new(
                    client
                        .inner
                        .prepare_session(
                            github_copilot_sdk::SessionConfig::default()
                                .with_session_id(github_copilot_sdk::SessionId::new("shutdown")),
                        )
                        .unwrap()
                        .start()
                        .await
                        .unwrap(),
                );
                let stream = super::super::event_stream::SessionEventStream::from_subscription(
                    native.subscribe(),
                    native.id().to_string(),
                    |_| true,
                    Arc::new(Trace),
                    "shutdown".into(),
                    super::super::event_stream::StartupState::Finished(
                        super::super::event_stream::StartupOutcome::Started,
                    ),
                    client.generation.clone(),
                );
                let session = Arc::new(CopilotSession {
                    inner: native,
                    events: super::super::intake::SessionIntake::new(
                        stream,
                        client.generation.clone(),
                    ),
                    generation: client.generation.clone(),
                });
                let sessions = CopilotSessions::<()>::new(Arc::new(Trace));
                if provisional {
                    sessions
                        .provisional
                        .lock()
                        .unwrap()
                        .insert(ThreadOwner::root("owner"), session.clone());
                } else {
                    sessions.slots.lock().await.insert(
                        ThreadOwner::root("owner"),
                        Arc::new(Mutex::new(Some(SessionBinding {
                            session: session.clone(),
                            history: SessionHistory::Prepared,
                            repair: None,
                            tool_schema: Vec::new(),
                            compaction: None,
                            mcp_failure: HashMap::new(),
                            _profile: None,
                        }))),
                    );
                }
                if !provisional {
                    sessions
                        .mark_submitted(&ThreadOwner::root("owner"), &session)
                        .await
                        .unwrap();
                    let slot = sessions.slot(&ThreadOwner::root("owner")).await;
                    assert!(matches!(
                        slot.lock().await.as_ref().unwrap().history,
                        SessionHistory::Submitted
                    ));
                }
                sessions.close_observers().await.unwrap();
                let scope = forge_provider_api::InvocationScope::new(
                    forge_provider_api::ThreadOwner::root("owner"),
                    "exchange",
                );
                let (sink, _events) = forge_provider_api::events::channel(
                    forge_provider_api::BindingId::new("sessions-fixture").unwrap(),
                    scope.clone(),
                );
                let context = Arc::new(Mutex::new(super::super::intake::TurnContext {
                    decoder: super::super::event_decoder::CopilotEventDecoder::new(
                        scope,
                        ".".into(),
                    ),
                    output: forge_provider_api::completion::ProviderCompletion::default(),
                    sink: sink.activity_writer(),
                }));
                assert!(
                    session.events.attach(context).await.is_err(),
                    "shutdown left intake alive"
                );
                session.disconnect().await.unwrap();
                assert!(
                    client.generation.failure().is_none(),
                    "orderly observer collection was treated as observation loss"
                );
                client.stop().await.unwrap();
                peer.await.unwrap();
                assert!(client.is_stopped());
                assert!(
                    client.generation.failure().is_none(),
                    "orderly shutdown invented observation loss"
                );
            }
        })
        .await
        .expect("observer shutdown fixture timed out");
    }

    /// A rejected create closes only its startup subscription and leaves the SDK client usable.
    #[tokio::test]
    async fn rejected_sdk_create_collects_subscription_without_stopping_client() {
        let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
        let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
        let sdk = github_copilot_sdk::Client::from_streams(
            client_read,
            client_write,
            std::env::temp_dir(),
        )
        .unwrap();
        let client = CopilotClient::new(sdk.clone());
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let profile = tempfile::tempdir().unwrap();
        let path = profile.path().to_owned();
        let owner = sessions.clone();
        let waiter = tokio::spawn(async move {
            owner
                .acquire(
                    async move { Ok((client, None)) },
                    SessionSelection {
                        execution_owner: forge_provider_api::ThreadOwner::root("owner"),
                        correlation_id: "owner".into(),
                        session_id: None,
                        model: None,
                        context_tier: None,
                        tools: None,
                        reuse: SessionReuse::Compatible,
                    },
                    Arc::new(Host),
                    Arc::new(Host),
                    || async move {
                        Ok(PreparedSession {
                            options: SessionOptions {
                                isolated_text: false,
                                session_id: None,
                                model: None,
                                reasoning_effort: None,
                                context_tier: None,
                                workspace: std::env::temp_dir(),
                                system_message: String::new(),
                                resources: super::super::session::SessionResources {
                                    skill_directories: Vec::new(),
                                    selected_mcp: Vec::new(),
                                },
                                excluded_tools: Vec::new(),
                                tools: Vec::new(),
                                native_callbacks: None,
                            },
                            skills: None,
                            mcp: None,
                            profile: Some(profile),
                        })
                    },
                    |_| Ok(()),
                )
                .await
        });
        let discovery = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            read_frame(&mut peer_read),
        )
        .await
        .unwrap();
        assert_eq!(discovery["method"], "mcp.discover");
        reply_frame(
            &mut peer_write,
            &discovery,
            serde_json::json!({"servers":[]}),
        )
        .await;
        let create = read_frame(&mut peer_read).await;
        assert_eq!(create["method"], "session.create");
        let body = serde_json::to_vec(&serde_json::json!({"jsonrpc":"2.0", "id":create["id"], "error":{"code":-32000,"message":"create rejected"}})).unwrap();
        peer_write
            .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
            .await
            .unwrap();
        peer_write.write_all(&body).await.unwrap();
        peer_write.flush().await.unwrap();
        let result = tokio::time::timeout(std::time::Duration::from_secs(5), waiter)
            .await
            .unwrap()
            .unwrap();
        let error = match result {
            Err(error) => error,
            Ok(_) => panic!("create unexpectedly succeeded"),
        };
        assert!(!matches!(
            error.downcast_ref::<ProviderError>(),
            Some(ProviderError::PreparationCleanup { .. })
        ));
        assert!(
            !path.exists(),
            "failed create retained a fully collected profile"
        );
        assert!(sessions.failed_startups.lock().unwrap().is_empty());
        let second = sdk
            .prepare_session(
                github_copilot_sdk::SessionConfig::default()
                    .with_session_id(github_copilot_sdk::SessionId::new("second")),
            )
            .unwrap();
        let second = tokio::spawn(second.start());
        let create = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            read_frame(&mut peer_read),
        )
        .await
        .unwrap();
        assert_eq!(create["method"], "session.create");
        reply_frame(
            &mut peer_write,
            &create,
            serde_json::json!({"sessionId":"second", "workspacePath":std::env::temp_dir()}),
        )
        .await;
        let session = tokio::time::timeout(std::time::Duration::from_secs(5), second)
            .await
            .unwrap()
            .unwrap()
            .unwrap();
        drop(session);
    }

    /// Collect a real SDK startup after its waiter drops, retaining its resource files until then.
    #[tokio::test]
    async fn dropped_acquire_during_sdk_create_is_collected_before_lease_release() {
        let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
        let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
        let client = CopilotClient::new(
            github_copilot_sdk::Client::from_streams(
                client_read,
                client_write,
                std::env::temp_dir(),
            )
            .unwrap(),
        );
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let profile = tempfile::tempdir().unwrap();
        let profile_path = profile.path().to_owned();
        let (started, started_rx) = tokio::sync::oneshot::channel();
        let (release, release_rx) = tokio::sync::oneshot::channel::<()>();
        let methods = Arc::new(std::sync::Mutex::new(Vec::new()));
        let peer_methods = methods.clone();
        let peer = tokio::spawn(async move {
            let discovery = read_frame(&mut peer_read).await;
            assert_eq!(discovery["method"], "mcp.discover");
            reply_frame(
                &mut peer_write,
                &discovery,
                serde_json::json!({"servers":[]}),
            )
            .await;
            let create = read_frame(&mut peer_read).await;
            assert_eq!(create["method"], "session.create");
            peer_methods
                .lock()
                .unwrap()
                .push("session.create".to_owned());
            let id = create["params"]["sessionId"].as_str().unwrap().to_owned();
            started.send(()).unwrap();
            release_rx.await.unwrap();
            reply_frame(
                &mut peer_write,
                &create,
                serde_json::json!({"sessionId":id, "workspacePath":std::env::temp_dir()}),
            )
            .await;
            loop {
                let request = read_frame(&mut peer_read).await;
                let method = request["method"].as_str().unwrap().to_owned();
                let result = match method.as_str() {
                    "session.detach" => serde_json::json!({"success":true}),
                    _ => serde_json::json!({}),
                };
                peer_methods.lock().unwrap().push(method);
                reply_frame(&mut peer_write, &request, result).await;
            }
        });
        let owner = sessions.clone();
        let waiter = tokio::spawn(async move {
            owner
                .acquire(
                    async move { Ok((client, None)) },
                    SessionSelection {
                        execution_owner: forge_provider_api::ThreadOwner::root("owner"),
                        correlation_id: "owner".into(),
                        session_id: None,
                        model: None,
                        context_tier: None,
                        tools: None,
                        reuse: SessionReuse::Compatible,
                    },
                    Arc::new(Host),
                    Arc::new(Host),
                    || async move {
                        Ok(PreparedSession {
                            options: SessionOptions {
                                isolated_text: false,
                                session_id: None,
                                model: None,
                                reasoning_effort: None,
                                context_tier: None,
                                workspace: std::env::temp_dir(),
                                system_message: String::new(),
                                resources: super::super::session::SessionResources {
                                    skill_directories: vec![profile_path.clone()],
                                    selected_mcp: Vec::new(),
                                },
                                excluded_tools: Vec::new(),
                                tools: Vec::new(),
                                native_callbacks: None,
                            },
                            skills: None,
                            mcp: None,
                            profile: Some(profile),
                        })
                    },
                    |_| Ok(()),
                )
                .await
        });
        tokio::time::timeout(std::time::Duration::from_secs(5), started_rx)
            .await
            .unwrap()
            .unwrap();
        waiter.abort();
        let _ = waiter.await;
        let path = sessions
            .leases
            .lock()
            .unwrap()
            .get(&ThreadOwner::root("owner"))
            .unwrap()
            .path()
            .to_owned();
        assert!(path.exists());
        release.send(()).unwrap();
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            sessions.retire(&ThreadOwner::root("owner")),
        )
        .await
        .unwrap()
        .unwrap();
        assert!(!path.exists(), "profile survived completed retirement");
        assert!(
            !methods
                .lock()
                .unwrap()
                .iter()
                .any(|method| method == "session.send")
        );
        peer.abort();
        let _ = peer.await;
    }

    /// Preserve the startup worker and its resource lease across cancellation of collection.
    #[tokio::test]
    async fn dropped_startup_waiter_retains_worker_for_owner_collection() {
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let profile = Arc::new(tempfile::tempdir().unwrap());
        let profile_path = profile.path().to_owned();
        sessions
            .leases
            .lock()
            .unwrap()
            .insert(ThreadOwner::root("owner"), profile.clone());
        let worker_profile = profile.clone();
        drop(profile);
        let (release, wait) = tokio::sync::oneshot::channel::<()>();
        let task = tokio::spawn(async move {
            let _profile = worker_profile;
            let _ = wait.await;
            Err(crate::copilot::error::native_failure(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        });
        let startup = Arc::new(Mutex::new(Some(task)));
        sessions
            .startups
            .lock()
            .unwrap()
            .insert(ThreadOwner::root("owner"), startup.clone());
        let waiter_sessions = sessions.clone();
        let waiter = tokio::spawn(async move {
            waiter_sessions
                .collect_startup(&ThreadOwner::root("owner"))
                .await
        });
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            while startup.try_lock().is_ok() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("startup waiter did not reach the held worker");
        waiter.abort();
        let _ = waiter.await;
        assert!(
            sessions
                .startups
                .lock()
                .unwrap()
                .contains_key(&ThreadOwner::root("owner"))
        );
        assert!(
            profile_path.exists(),
            "resource lease disappeared while startup was owned"
        );
        release.send(()).unwrap();
        let error = match sessions.collect_startup(&ThreadOwner::root("owner")).await {
            Err(error) => error,
            Ok(_) => panic!("startup failure was lost"),
        };
        assert!(matches!(
            error.downcast_ref::<ProviderError>(),
            Some(ProviderError::Failure(failure))
                if failure.kind() == forge_provider_api::ProviderFailureKind::Rejected
        ));
        assert!(
            !sessions
                .startups
                .lock()
                .unwrap()
                .contains_key(&ThreadOwner::root("owner"))
        );
        assert!(
            !profile_path.exists(),
            "resource lease was not released after collection"
        );
    }

    /// Preserve each independently failed startup when shutdown reports its outcome.
    #[tokio::test]
    async fn shutdown_preserves_all_failed_startup_causes() {
        use forge_provider_api::{ProviderFailureKind, ProviderOperation};
        let sessions = CopilotSessions::<()>::new(Arc::new(Trace));
        for (key, kind) in [
            ("first", ProviderFailureKind::Protocol),
            ("second", ProviderFailureKind::Transport),
        ] {
            sessions.failed_startups.lock().unwrap().insert(
                ThreadOwner::root(key),
                Arc::new(super::super::error::native_failure(
                    ProviderOperation::ThreadOpen,
                    kind,
                )),
            );
        }
        let error = sessions.clear(ClientShutdown::Collected).await.unwrap_err();
        let ProviderError::Combined { primary, secondary } = error else {
            panic!("shutdown discarded an independent startup failure");
        };
        assert!(matches!(*primary, ProviderError::Failure(failure)
            if failure.kind() == ProviderFailureKind::Protocol));
        assert!(matches!(&secondary[..], [ProviderError::Failure(failure)]
            if failure.kind() == ProviderFailureKind::Transport));
        assert!(sessions.failed_startups.lock().unwrap().is_empty());
        sessions.clear(ClientShutdown::Collected).await.unwrap();
    }

    /// Keep failed cleanup fenced and retain its profile until collection can be proven.
    #[tokio::test]
    async fn failed_startup_cleanup_fences_reuse_and_retains_profile() {
        let sessions = CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace));
        let profile = Arc::new(tempfile::tempdir().unwrap());
        let path = profile.path().to_owned();
        sessions
            .leases
            .lock()
            .unwrap()
            .insert(ThreadOwner::root("owner"), profile);
        let task = tokio::spawn(async {
            Err(ProviderError::PreparationCleanup {
                cause: Box::new(crate::copilot::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Rejected,
                )),
                cleanup: Box::new(crate::copilot::error::native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Transport,
                )),
            })
        });
        sessions
            .startups
            .lock()
            .unwrap()
            .insert(ThreadOwner::root("owner"), Arc::new(Mutex::new(Some(task))));
        assert!(
            sessions
                .collect_startup(&ThreadOwner::root("owner"))
                .await
                .is_err()
        );
        assert!(
            sessions
                .ensure_startup_collected(&ThreadOwner::root("owner"))
                .is_err()
        );
        assert!(path.exists(), "failed cleanup released its resource lease");
        assert!(sessions.clear(ClientShutdown::Uncertain).await.is_err());
        assert!(
            path.exists(),
            "unproven client shutdown released its profile"
        );
        assert!(sessions.clear(ClientShutdown::Collected).await.is_err());
        assert!(!path.exists());
        assert!(sessions.clear(ClientShutdown::Collected).await.is_ok());
    }
}
