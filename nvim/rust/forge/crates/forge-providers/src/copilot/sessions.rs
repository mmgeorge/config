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

/// Own native session replacement, history boundaries, and background readers.
pub struct CopilotSessions<P> {
    slots: Mutex<HashMap<String, SessionSlot<P>>>,
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
    _profile: Option<P>,
}

impl<P: Send + Sync> CopilotSessions<P> {
    /// Create an empty registry using the host's diagnostic sink.
    pub fn new(trace: Arc<dyn ProviderTrace>) -> Self {
        Self {
            slots: Mutex::new(HashMap::new()),
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
        let session = Arc::new(
            super::session::open(
                &client,
                prepared.options,
                permissions,
                tools,
                self.trace.as_ref(),
                &selection.correlation_id,
            )
            .await?,
        );
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
                let _ = session.disconnect().await;
                return Err(error);
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
            _profile: prepared.profile,
        });
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

    /// Release bindings after the owning SDK client has fully stopped.
    pub async fn clear(&self) {
        self.slots.lock().await.clear();
        self.turns.lock().await.clear();
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
