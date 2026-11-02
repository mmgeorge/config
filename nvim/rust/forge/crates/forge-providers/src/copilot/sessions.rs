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
use forge_provider_api::{Compaction, ProviderError, ProviderEventSink};
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
    /// Identify the host execution owner whose native binding may be reused.
    pub runtime_key: String,
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
    slots: Mutex<HashMap<String, SessionSlot<P>>>,
    provisional: Arc<std::sync::Mutex<HashMap<String, Arc<CopilotSession>>>>,
    leases: Arc<std::sync::Mutex<HashMap<String, Arc<P>>>>,
    startups: std::sync::Mutex<HashMap<String, StartupTask>>,
    failed_startups: std::sync::Mutex<HashMap<String, Arc<ProviderError>>>,
    terminals: Mutex<HashMap<String, CopilotTerminalMonitor>>,
    subscriptions: Mutex<HashMap<String, CompactionSubscription>>,
    turns: Mutex<HashMap<String, Arc<CopilotTurnEvent>>>,
    trace: Arc<dyn ProviderTrace>,
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
        let slot = self.slot(&selection.runtime_key).await;
        let turn = self.turn(&selection.runtime_key).await;
        let mut active = slot.lock().await;
        self.ensure_startup_collected(&selection.runtime_key)?;
        let _ = self.collect_startup(&selection.runtime_key).await?;
        let prior = self
            .provisional
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
            .get(&selection.runtime_key)
            .cloned();
        if let Some(previous) = prior {
            previous
                .disconnect()
                .await
                .map_err(|cleanup| ProviderError::PreparationCleanup {
                    cause: Arc::new(ProviderError::Protocol {
                        message: "previous Copilot startup was abandoned".into(),
                    }),
                    cleanup: Arc::new(cleanup),
                })?;
            self.provisional
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
                .remove(&selection.runtime_key);
            self.leases
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                .remove(&selection.runtime_key);
        }
        if let Some(binding) = active.as_mut().filter(|binding| {
            selection.reuse == SessionReuse::Compatible
                && selection.tools.as_ref().is_none_or(|tools| {
                    tools
                        .iter()
                        .map(|tool| tool.mcp_value())
                        .collect::<Vec<_>>()
                        == binding.tool_schema
                })
        }) {
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
            Some(binding) => matches!(binding.history, SessionHistory::Submitted)
                .then(|| binding.session.id().to_owned()),
            None => selection.session_id.clone(),
        };
        let (client, reasoning_effort) = client.await?;
        let mut prepared = prepare().await?;
        prepared.options.reasoning_effort = reasoning_effort;
        prepared.options.session_id = retained_id.clone();
        let tool_schema = prepared
            .options
            .tools
            .iter()
            .map(|tool| tool.mcp_value())
            .collect();
        if active
            .as_ref()
            .is_some_and(|binding| matches!(binding.history, SessionHistory::Submitted))
        {
            self.disconnect(&selection.runtime_key, &mut active).await?;
        }
        let startup_key = selection.runtime_key.clone();
        let provisional = self.provisional.clone();
        let trace = self.trace.clone();
        let correlation_id = selection.correlation_id.clone();
        let options = prepared.options;
        let profile = prepared.profile.map(Arc::new);
        if let Some(profile) = &profile {
            self.leases
                .lock()
                .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                .insert(selection.runtime_key.clone(), profile.clone());
        }
        let worker_profile = profile.clone();
        let task = tokio::spawn(async move {
            let _profile = worker_profile;
            let session = Arc::new(
                super::session::open(&client, options, permissions, tools, trace, &correlation_id)
                    .await?,
            );
            provisional
                .lock()
                .map_err(|_| ProviderError::Protocol {
                    message: "Copilot provisional session registry poisoned".into(),
                })?
                .insert(startup_key, session.clone());
            Ok(session)
        });
        let startup = Arc::new(Mutex::new(Some(task)));
        self.startups
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot startup registry poisoned"))?
            .insert(selection.runtime_key.clone(), startup);
        let session = self
            .collect_startup(&selection.runtime_key)
            .await?
            .context("Copilot startup task missing")?;
        let configuration = async {
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
                    std::time::Duration::from_secs(30),
                )
                .await?
            } else {
                HashMap::new()
            };
            self.disconnect(&selection.runtime_key, &mut active).await?;
            validate(&session)?;
            Ok::<_, anyhow::Error>((repair, failures))
        }
        .await;
        let (repair, mcp_failure) = match configuration {
            Ok(configured) => configured,
            Err(error) => {
                let cause = ProviderError::from(error);
                if let Err(cleanup) = session.disconnect().await {
                    return Err(ProviderError::PreparationCleanup {
                        cause: Arc::new(cause),
                        cleanup: Arc::new(cleanup),
                    }
                    .into());
                }
                self.provisional
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot provisional session registry poisoned"))?
                    .remove(&selection.runtime_key);
                self.leases
                    .lock()
                    .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
                    .remove(&selection.runtime_key);
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
            .remove(&selection.runtime_key);
        self.leases
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot resource lease registry poisoned"))?
            .remove(&selection.runtime_key);
        Ok(session)
    }

    /// Get the currently published native binding without creating one.
    pub async fn active(&self, key: &str) -> Option<Arc<CopilotSession>> {
        let slot = self.slots.lock().await.get(key).cloned()?;
        slot.lock()
            .await
            .as_ref()
            .map(|binding| binding.session.clone())
    }

    /// Record possible native input only against the binding that will receive it.
    pub async fn mark_submitted(
        &self,
        key: &str,
        session: &Arc<CopilotSession>,
    ) -> Result<(), ProviderError> {
        let slot = self.slot(key).await;
        let mut active = slot.lock().await;
        let binding = active
            .as_mut()
            .filter(|binding| Arc::ptr_eq(&binding.session, session))
            .ok_or_else(|| ProviderError::Protocol {
                message: "Copilot submission changed or lost its session".into(),
            })?;
        binding.history = SessionHistory::Submitted;
        Ok(())
    }

    /// Get the repair caution associated with the published history.
    pub async fn repair(&self, key: &str) -> Option<HistoryRepair> {
        self.slot(key)
            .await
            .lock()
            .await
            .as_ref()
            .and_then(|binding| binding.repair.clone())
    }

    /// Get startup failures without exposing mutable native session state.
    pub async fn mcp_failures(&self, key: &str) -> HashMap<String, String> {
        self.slot(key)
            .await
            .lock()
            .await
            .as_ref()
            .map(|binding| binding.mcp_failure.clone())
            .unwrap_or_default()
    }

    /// Get the control lane shared by native prompt and cancellation operations.
    pub async fn turn(&self, key: &str) -> Arc<CopilotTurnEvent> {
        self.turns
            .lock()
            .await
            .entry(key.to_owned())
            .or_default()
            .clone()
    }

    /// Bind background compaction observations before creating a native session.
    pub async fn subscribe(&self, key: String, sink: ProviderEventSink) {
        self.subscriptions
            .lock()
            .await
            .insert(key, CompactionSubscription::new(sink));
    }

    /// Report whether the session has a separate compaction observer.
    pub async fn observes_compaction(&self, key: &str) -> bool {
        self.subscriptions.lock().await.contains_key(key)
    }

    /// Correlate one manual compaction with native notifications around its RPC.
    pub async fn compact(&self, key: &str, id: String) -> Result<Compaction, ProviderError> {
        let slot = self.slot(key).await;
        let (session, manual) = {
            let active = slot.lock().await;
            let binding = active.as_ref().ok_or_else(|| ProviderError::Protocol {
                message: "Copilot compaction lost its session".into(),
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

    /// Get a nonblocking snapshot while another caller prepares the session.
    pub async fn terminals(&self, key: &str, correlation: &str) -> Result<TerminalSnapshot> {
        let slot = self.slot(key).await;
        let active = match slot.try_lock() {
            Ok(active) => active,
            Err(_) => {
                return Ok(TerminalSnapshot {
                    supported: true,
                    starting: true,
                    terminal: Vec::new(),
                });
            }
        };
        let Some(binding) = active.as_ref() else {
            return Ok(TerminalSnapshot {
                supported: true,
                starting: false,
                terminal: Vec::new(),
            });
        };
        let mut monitors = self.terminals.lock().await;
        let monitor = monitors.entry(key.to_owned()).or_insert_with(|| {
            CopilotTerminalMonitor::new(
                &binding.session,
                self.trace.clone(),
                correlation.to_owned(),
            )
        });
        if !monitor.observes(&binding.session) {
            *monitor = CopilotTerminalMonitor::new(
                &binding.session,
                self.trace.clone(),
                correlation.to_owned(),
            );
        }
        match monitor.snapshot() {
            Some(result) => result.map_err(anyhow::Error::msg),
            None => Ok(TerminalSnapshot {
                supported: true,
                starting: true,
                terminal: Vec::new(),
            }),
        }
    }

    /// Stop one native command and invalidate its retained snapshot.
    pub async fn terminate(&self, key: &str, id: &str) -> Result<()> {
        self.active(key)
            .await
            .context("Copilot session is not attached")?
            .terminate_terminal(id)
            .await?;
        self.terminals.lock().await.remove(key);
        Ok(())
    }

    /// Drop observation readers before shutting down the SDK client.
    pub async fn close_observers(&self) {
        self.terminals.lock().await.clear();
        for slot in self.slots.lock().await.values() {
            if let Some(binding) = slot.lock().await.as_mut() {
                binding.compaction.take();
            }
        }
        self.subscriptions.lock().await.clear();
    }

    /// Disconnect one collected owner before allowing its history to be resumed.
    pub async fn retire(&self, key: &str) -> Result<()> {
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
        Ok(())
    }

    /// Release bindings after the owning SDK client has fully stopped.
    pub async fn clear(&self, client_stopped: ClientShutdown) -> Result<(), ProviderError> {
        let keys: Vec<_> = self
            .startups
            .lock()
            .map_err(|_| ProviderError::Protocol {
                message: "Copilot startup registry poisoned".into(),
            })?
            .keys()
            .cloned()
            .collect();
        let mut startup_failure = None;
        let failed_key = self
            .failed_startups
            .lock()
            .map_err(|_| ProviderError::Protocol {
                message: "Copilot startup failure registry poisoned".into(),
            })?
            .keys()
            .next()
            .cloned();
        if let Some(key) = failed_key {
            startup_failure = Some(ProviderError::from(
                self.ensure_startup_collected(&key)
                    .expect_err("failed startup must remain fenced"),
            ));
        }
        for key in keys {
            if let Err(error) = self.collect_startup(&key).await {
                startup_failure.get_or_insert(ProviderError::from(error));
            }
        }
        let slots = std::mem::take(&mut *self.slots.lock().await);
        let mut failure = startup_failure;
        let provisional: Vec<_> = {
            let pending = self
                .provisional
                .lock()
                .map_err(|_| ProviderError::Protocol {
                    message: "Copilot provisional session registry poisoned".into(),
                })?;
            pending
                .iter()
                .map(|(key, session)| (key.clone(), session.clone()))
                .collect()
        };
        for (key, session) in provisional {
            match session.disconnect().await {
                Err(error) => {
                    failure.get_or_insert(error);
                }
                Ok(()) => {
                    self.provisional
                        .lock()
                        .map_err(|_| ProviderError::Protocol {
                            message: "Copilot provisional session registry poisoned".into(),
                        })?
                        .remove(&key);
                    self.leases
                        .lock()
                        .map_err(|_| ProviderError::Protocol {
                            message: "Copilot resource lease registry poisoned".into(),
                        })?
                        .remove(&key);
                }
            }
        }
        for (_, slot) in slots {
            let binding = slot.lock().await.take();
            if let Some(binding) = binding
                && let Err(error) = binding.session.events.close().await
            {
                failure.get_or_insert(error);
            }
        }
        self.turns.lock().await.clear();
        if matches!(client_stopped, ClientShutdown::Collected)
            && self
                .startups
                .lock()
                .map_err(|_| ProviderError::Protocol {
                    message: "Copilot startup registry poisoned".into(),
                })?
                .is_empty()
            && self
                .provisional
                .lock()
                .map_err(|_| ProviderError::Protocol {
                    message: "Copilot provisional session registry poisoned".into(),
                })?
                .is_empty()
        {
            self.failed_startups
                .lock()
                .map_err(|_| ProviderError::Protocol {
                    message: "Copilot startup failure registry poisoned".into(),
                })?
                .clear();
            self.leases
                .lock()
                .map_err(|_| ProviderError::Protocol {
                    message: "Copilot resource lease registry poisoned".into(),
                })?
                .clear();
        }
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    /// Get the preparation lock for one native session owner.
    async fn slot(&self, key: &str) -> SessionSlot<P> {
        self.slots
            .lock()
            .await
            .entry(key.to_owned())
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone()
    }

    /// Join one owned startup without surrendering its handle when the waiter is cancelled.
    async fn collect_startup(&self, key: &str) -> Result<Option<Arc<CopilotSession>>> {
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
        let result = match tokio::time::timeout(std::time::Duration::from_secs(30), task).await {
            Ok(result) => result,
            Err(_) => {
                return Err(ProviderError::PreparationCleanup {
                    cause: Arc::new(ProviderError::Protocol {
                        message: "Copilot startup has not settled".into(),
                    }),
                    cleanup: Arc::new(ProviderError::DeliveryUncertain {
                        message:
                            "Copilot startup collection exceeded its deadline; worker retained"
                                .into(),
                    }),
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
            Err(error) => {
                let error = Arc::new(ProviderError::Protocol {
                    message: format!("Copilot startup worker failed: {error}"),
                });
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
    fn ensure_startup_collected(&self, key: &str) -> Result<()> {
        if let Some(error) = self
            .failed_startups
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot startup failure registry poisoned"))?
            .get(key)
        {
            return Err(ProviderError::PreparationCleanup {
                cause: error.clone(),
                cleanup: Arc::new(ProviderError::Protocol {
                    message: "Copilot startup cleanup remains uncollected".into(),
                }),
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
            .get(&selection.runtime_key)
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

    /// Retire the old router before publishing a replacement with the same native ID.
    async fn disconnect(&self, key: &str, active: &mut Option<SessionBinding<P>>) -> Result<()> {
        let Some(mut binding) = active.take() else {
            return Ok(());
        };
        if let Some(monitor) = self.terminals.lock().await.remove(key) {
            monitor.shutdown().await;
        }
        binding.compaction.take();
        if let Err(error) = binding.session.disconnect().await {
            *active = Some(binding);
            return Err(error).context("disconnect Copilot session before refreshing tools");
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};

    struct Trace;
    impl ProviderTrace for Trace {
        /// Discard fixture diagnostics independently of the native transport assertions.
        fn record(&self, _: &str, _: &str, _: Value) {}
    }

    struct Host;
    #[async_trait]
    impl ProviderPermissions for Host {
        /// Reject unexpected native permission callbacks in the startup fixture.
        async fn authorize(
            &self,
            _: &str,
            _: forge_provider_api::permissions::PermissionRequest,
        ) -> forge_provider_api::permissions::PermissionResponse {
            forge_provider_api::permissions::PermissionResponse::Cancelled
        }
        /// Keep the fixture's host barrier open without authorizing tool calls.
        async fn before_tool(&self) -> Result<(), ProviderError> {
            Ok(())
        }
    }
    #[async_trait]
    impl ProviderTools for Host {
        /// Reject unexpected native tool execution before prompt dispatch.
        async fn call(
            &self,
            _: forge_provider_api::control::ToolCall,
        ) -> Result<String, ProviderError> {
            Err(ProviderError::Rejected {
                message: "unexpected tool call".into(),
            })
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
            for mode in [
                forge_provider_api::PromptMode::Chat,
                forge_provider_api::PromptMode::Background,
            ] {
                for resumed in [false, true] {
                    let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
                    let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
                    let client = CopilotClient {
                        inner: github_copilot_sdk::Client::from_streams(
                            client_read,
                            client_write,
                            std::env::temp_dir(),
                        )
                        .unwrap(),
                    };
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
                        let expected = if mode == forge_provider_api::PromptMode::Background {
                            serde_json::json!(["github", "selected", "workspace-extra"])
                        } else {
                            serde_json::json!(["github", "workspace-extra"])
                        };
                        assert_eq!(create["params"]["disabledMcpServers"], expected);
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
                                    reply_frame(&mut peer_write, &request, serde_json::json!({}))
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
                            session_id: resumed.then(|| "saved-session".to_owned()),
                            model: None,
                            reasoning_effort: None,
                            context_tier: None,
                            workspace: std::env::temp_dir(),
                            mode,
                            system_message: String::new(),
                            resources: if mode == forge_provider_api::PromptMode::Background {
                                super::super::session::ResourceDiscovery::Native
                            } else {
                                super::super::session::ResourceDiscovery::Managed {
                                    skill_directories: Vec::new(),
                                    selected_mcp: vec!["selected".to_owned()],
                                }
                            },
                            excluded_tools: Vec::new(),
                            tools: Vec::new(),
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
                    client.inner.force_stop();
                }
            }
        })
        .await
        .expect("pre-start MCP selection fixture timed out");
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
        let client = CopilotClient { inner: sdk.clone() };
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let profile = tempfile::tempdir().unwrap();
        let path = profile.path().to_owned();
        let owner = sessions.clone();
        let waiter = tokio::spawn(async move {
            owner
                .acquire(
                    async move { Ok((client, None)) },
                    SessionSelection {
                        runtime_key: "owner".into(),
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
                                session_id: None,
                                model: None,
                                reasoning_effort: None,
                                context_tier: None,
                                workspace: std::env::temp_dir(),
                                mode: forge_provider_api::PromptMode::Chat,
                                system_message: String::new(),
                                resources: super::super::session::ResourceDiscovery::Managed {
                                    skill_directories: Vec::new(),
                                    selected_mcp: Vec::new(),
                                },
                                excluded_tools: Vec::new(),
                                tools: Vec::new(),
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
        let client = CopilotClient {
            inner: github_copilot_sdk::Client::from_streams(
                client_read,
                client_write,
                std::env::temp_dir(),
            )
            .unwrap(),
        };
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
                        runtime_key: "owner".into(),
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
                                session_id: None,
                                model: None,
                                reasoning_effort: None,
                                context_tier: None,
                                workspace: std::env::temp_dir(),
                                mode: forge_provider_api::PromptMode::Chat,
                                system_message: String::new(),
                                resources: super::super::session::ResourceDiscovery::Managed {
                                    skill_directories: vec![profile_path.clone()],
                                    selected_mcp: Vec::new(),
                                },
                                excluded_tools: Vec::new(),
                                tools: Vec::new(),
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
            .get("owner")
            .unwrap()
            .path()
            .to_owned();
        assert!(path.exists());
        release.send(()).unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(5), sessions.retire("owner"))
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
            .insert("owner".into(), profile.clone());
        let worker_profile = profile.clone();
        drop(profile);
        let (release, wait) = tokio::sync::oneshot::channel::<()>();
        let task = tokio::spawn(async move {
            let _profile = worker_profile;
            let _ = wait.await;
            Err(ProviderError::Rejected {
                message: "startup failed".into(),
            })
        });
        let startup = Arc::new(Mutex::new(Some(task)));
        sessions
            .startups
            .lock()
            .unwrap()
            .insert("owner".into(), startup.clone());
        let waiter_sessions = sessions.clone();
        let waiter = tokio::spawn(async move { waiter_sessions.collect_startup("owner").await });
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            while startup.try_lock().is_ok() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .expect("startup waiter did not reach the held worker");
        waiter.abort();
        let _ = waiter.await;
        assert!(sessions.startups.lock().unwrap().contains_key("owner"));
        assert!(
            profile_path.exists(),
            "resource lease disappeared while startup was owned"
        );
        release.send(()).unwrap();
        let error = match sessions.collect_startup("owner").await {
            Err(error) => error,
            Ok(_) => panic!("startup failure was lost"),
        };
        assert!(
            matches!(error.downcast_ref::<ProviderError>(), Some(ProviderError::Rejected { message }) if message == "startup failed")
        );
        assert!(!sessions.startups.lock().unwrap().contains_key("owner"));
        assert!(
            !profile_path.exists(),
            "resource lease was not released after collection"
        );
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
            .insert("owner".into(), profile);
        let task = tokio::spawn(async {
            Err(ProviderError::PreparationCleanup {
                cause: Arc::new(ProviderError::Rejected {
                    message: "startup failed".into(),
                }),
                cleanup: Arc::new(ProviderError::Transport {
                    message: "disconnect failed".into(),
                }),
            })
        });
        sessions
            .startups
            .lock()
            .unwrap()
            .insert("owner".into(), Arc::new(Mutex::new(Some(task))));
        assert!(sessions.collect_startup("owner").await.is_err());
        assert!(sessions.ensure_startup_collected("owner").is_err());
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
