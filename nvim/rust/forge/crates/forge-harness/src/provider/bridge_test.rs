use super::*;

use forge_provider_api::{
    BindingId, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink},
    provider::Provider as NativeProvider,
    turn_control::TurnControl as _,
};
use std::{
    collections::HashMap,
    sync::{
        Arc, Mutex,
        atomic::{AtomicUsize, Ordering},
    },
};

struct BridgeFixtureProvider {
    sinks: Mutex<HashMap<BindingId, ProviderEventSink>>,
    duplicate_sinks: Mutex<Vec<ProviderEventSink>>,
    open_binding_override: Mutex<Option<BindingId>>,
    shutdown_failed: std::sync::atomic::AtomicBool,
    shutdown_calls: AtomicUsize,
    shutdown_callback: tokio::sync::Mutex<
        Option<tokio::task::JoinHandle<forge_provider_api::permissions::PermissionResponse>>,
    >,
    fail_fence: bool,
    close_failed: std::sync::atomic::AtomicBool,
    retire_calls: AtomicUsize,
    close_contexts: Mutex<Vec<forge_provider_api::OperationId>>,
    close_failures: Mutex<HashMap<forge_provider_api::OperationId, ProviderError>>,
    close_event_count: AtomicUsize,
    reconcile_calls: AtomicUsize,
    configuration_deadlines: Mutex<Vec<Option<tokio::time::Instant>>>,
    cancel_calls: AtomicUsize,
    terminal_calls: AtomicUsize,
    terminal_started: tokio::sync::Notify,
    terminal_release: tokio::sync::Notify,
    open_blocked: std::sync::atomic::AtomicBool,
    open_started: tokio::sync::Notify,
    open_failure: Mutex<Option<ProviderError>>,
    open_persistent_failure: Mutex<Option<ProviderError>>,
    open_attempts: Mutex<
        Vec<(
            forge_provider_api::OperationId,
            forge_provider_api::SessionRequestIdentity,
        )>,
    >,
    open_instructions: Mutex<Vec<String>>,
    open_models: Mutex<Vec<Option<String>>>,
    input_failure: Mutex<Option<ProviderError>>,
    open_thread_override: Mutex<Option<forge_provider_api::ThreadId>>,
    open_identity: Mutex<Option<forge_provider_api::ProviderSessionId>>,
    configured_identity: Mutex<Option<forge_provider_api::ProviderSessionId>>,
    fork_attempts: Mutex<
        Vec<(
            forge_provider_api::OperationId,
            forge_provider_api::SessionForkRequest,
        )>,
    >,
    compact_attempts: Mutex<
        Vec<(
            forge_provider_api::OperationId,
            BindingId,
            forge_provider_api::CompactionRequest,
        )>,
    >,
    fork_blocked: std::sync::atomic::AtomicBool,
    fork_started: tokio::sync::Notify,
    compact_watermark: std::sync::atomic::AtomicU64,
    compact_started: tokio::sync::Notify,
}

impl BridgeFixtureProvider {
    fn new(fail_fence: bool) -> Arc<Self> {
        Arc::new(Self {
            sinks: Mutex::new(HashMap::new()),
            duplicate_sinks: Mutex::new(Vec::new()),
            open_binding_override: Mutex::new(None),
            shutdown_failed: std::sync::atomic::AtomicBool::new(false),
            shutdown_calls: AtomicUsize::new(0),
            shutdown_callback: tokio::sync::Mutex::new(None),
            fail_fence,
            close_failed: std::sync::atomic::AtomicBool::new(false),
            retire_calls: AtomicUsize::new(0),
            close_contexts: Mutex::new(Vec::new()),
            close_failures: Mutex::new(HashMap::new()),
            close_event_count: AtomicUsize::new(0),
            reconcile_calls: AtomicUsize::new(0),
            configuration_deadlines: Mutex::new(Vec::new()),
            cancel_calls: AtomicUsize::new(0),
            terminal_calls: AtomicUsize::new(0),
            terminal_started: tokio::sync::Notify::new(),
            terminal_release: tokio::sync::Notify::new(),
            open_blocked: std::sync::atomic::AtomicBool::new(false),
            open_started: tokio::sync::Notify::new(),
            open_failure: Mutex::new(None),
            open_persistent_failure: Mutex::new(None),
            open_attempts: Mutex::new(Vec::new()),
            open_instructions: Mutex::new(Vec::new()),
            open_models: Mutex::new(Vec::new()),
            input_failure: Mutex::new(None),
            open_thread_override: Mutex::new(None),
            open_identity: Mutex::new(None),
            configured_identity: Mutex::new(None),
            fork_attempts: Mutex::new(Vec::new()),
            compact_attempts: Mutex::new(Vec::new()),
            fork_blocked: std::sync::atomic::AtomicBool::new(false),
            fork_started: tokio::sync::Notify::new(),
            compact_watermark: std::sync::atomic::AtomicU64::new(0),
            compact_started: tokio::sync::Notify::new(),
        })
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id().expect("fixture provider identity"),
            label: "bridge fixture".into(),
            capability: Default::default(),
        }
    }

    fn close_native_stream(&self, binding: &BindingId) {
        self.sinks
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .remove(binding);
    }
}

#[async_trait::async_trait]
impl NativeProvider for BridgeFixtureProvider {
    async fn thread_fork(
        &self,
        context: forge_provider_api::OperationContext,
        request: forge_provider_api::SessionForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        {
            let mut attempts = self.fork_attempts.lock().unwrap();
            if attempts
                .iter()
                .any(|(id, previous)| id == context.id() && previous != &request)
            {
                return Err(provider_failure(
                    ProviderOperation::ThreadFork,
                    ProviderFailureKind::OperationConflict,
                ));
            }
            attempts.push((context.id().clone(), request));
            if attempts.len() == 1 && !self.fork_blocked.load(Ordering::Acquire) {
                return Err(ProviderFailure::new(
                    ProviderOperation::ThreadFork,
                    ProviderFailureKind::Transport,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                )
                .into());
            }
        }
        if self.fork_blocked.load(Ordering::Acquire) {
            self.fork_started.notify_one();
            return Err(context.interrupted().await.failure(
                ProviderOperation::ThreadFork,
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ));
        }
        Ok(forge_provider_api::ProviderForkResult::unprofiled(
            "forked-history"
                .to_owned()
                .try_into()
                .expect("fixture history identity"),
        ))
    }

    async fn thread_compact(
        &self,
        context: forge_provider_api::OperationContext,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::CompactionResult, ProviderError> {
        request.validate_context(&context)?;
        let mut attempts = self.compact_attempts.lock().unwrap();
        if attempts.iter().any(|(id, previous_binding, previous)| {
            id == context.id() && (previous_binding != binding || previous != &request)
        }) {
            return Err(provider_failure(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::OperationConflict,
            ));
        }
        attempts.push((context.id().clone(), binding.clone(), request));
        if attempts.len() == 1 {
            return Err(ProviderFailure::new(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::Transport,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            )
            .into());
        }
        self.compact_started.notify_one();
        Ok(forge_provider_api::CompactionResult {
            compaction: forge_provider_api::Compaction {
                id: context.id().as_str().into(),
                thread_id: "history".into(),
                state: forge_provider_api::CompactionState::Completed,
                manual: true,
                started_at_ms: None,
                completed_at_ms: None,
                result: Default::default(),
            },
            watermark: forge_provider_api::EventSequence::new(
                self.compact_watermark.load(Ordering::Acquire),
            ),
        })
    }

    async fn thread_open(
        &self,
        operation: forge_provider_api::OperationContext,
        request: forge_provider_api::SessionRequest,
    ) -> Result<forge_provider_api::OpenedSession, ProviderError> {
        if let Some(error) = self.open_persistent_failure.lock().unwrap().clone() {
            return Err(error);
        }
        operation.check_admission(ProviderOperation::ThreadOpen)?;
        self.open_instructions
            .lock()
            .unwrap()
            .push(request.system_instructions.clone());
        self.open_models
            .lock()
            .unwrap()
            .push(request.configuration.model.model.clone());
        {
            let mut attempts = self.open_attempts.lock().unwrap();
            let identity = request.identity();
            if attempts
                .iter()
                .any(|(id, prior)| id == operation.id() && prior != &identity)
            {
                return Err(provider_failure(
                    ProviderOperation::ThreadOpen,
                    ProviderFailureKind::OperationConflict,
                ));
            }
            attempts.push((operation.id().clone(), identity));
        }
        if let Some(error) = self.open_failure.lock().unwrap().take() {
            return Err(error);
        }
        if self.open_blocked.load(Ordering::Acquire) {
            self.open_started.notify_one();
            return Err(operation.interrupted().await.failure(
                ProviderOperation::ThreadOpen,
                forge_provider_api::ProviderPhase::Preparation,
                forge_provider_api::FailureDisposition::Unchanged,
            ));
        }
        let binding = self
            .open_binding_override
            .lock()
            .unwrap()
            .take()
            .unwrap_or_else(|| BindingId::new(uuid::Uuid::new_v4().to_string()).unwrap());
        let (sink, events) = events::channel(
            binding.clone(),
            forge_provider_api::EventScope::Binding {
                owner: request.owner,
            },
        );
        let mut sinks = self.sinks.lock().unwrap();
        match sinks.entry(binding.clone()) {
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(sink);
            }
            std::collections::hash_map::Entry::Occupied(_) => {
                self.duplicate_sinks.lock().unwrap().push(sink)
            }
        }
        drop(sinks);
        Ok(forge_provider_api::OpenedSession {
            binding: forge_provider_api::SessionBinding {
                thread_id: self
                    .open_thread_override
                    .lock()
                    .unwrap()
                    .clone()
                    .unwrap_or_else(|| request.thread_id.clone()),
                id: binding,
                provider_session_id: self.open_identity.lock().unwrap().clone(),
            },
            capabilities: forge_provider_api::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            events,
        })
    }

    async fn thread_configure(
        &self,
        operation: forge_provider_api::OperationContext,
        binding: &BindingId,
        configuration: forge_provider_api::SessionConfiguration,
    ) -> Result<forge_provider_api::ConfiguredSession, ProviderError> {
        operation.check_admission(ProviderOperation::ThreadConfigure)?;
        self.configuration_deadlines
            .lock()
            .unwrap()
            .push(operation.deadline());
        self.reconcile_calls.fetch_add(1, Ordering::AcqRel);
        let identity = self.configured_identity.lock().unwrap().clone();
        let sink = self
            .sinks
            .lock()
            .unwrap()
            .get(binding)
            .cloned()
            .ok_or_else(|| {
                provider_failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        if let Some(provider_session_id) = identity {
            sink.activity_writer()
                .send_wait(forge_provider_api::AdapterActivity::Thread {
                    thread_id: sink.scope().owner().thread_id(),
                    event: forge_provider_api::ThreadEvent::IdentityResolved(provider_session_id),
                })
                .await?;
        }
        Ok(forge_provider_api::ConfiguredSession {
            configuration,
            capabilities: forge_provider_api::CapabilityManifest::from_descriptor(
                self.descriptor(),
            ),
            watermark: sink.watermark(),
        })
    }

    async fn thread_close(
        &self,
        operation: forge_provider_api::OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        if let Some(error) = self
            .close_failures
            .lock()
            .unwrap()
            .get(operation.id())
            .cloned()
        {
            return Err(error);
        }
        operation.check_admission(ProviderOperation::ThreadClose)?;
        self.retire_calls.fetch_add(1, Ordering::AcqRel);
        self.close_contexts
            .lock()
            .unwrap()
            .push(operation.id().clone());
        if self.fail_fence && !self.close_failed.swap(true, Ordering::AcqRel) {
            let error =
                provider_failure(ProviderOperation::ThreadClose, ProviderFailureKind::Runtime);
            self.close_failures
                .lock()
                .unwrap()
                .insert(operation.id().clone(), error.clone());
            return Err(error);
        }
        let sink = self.sinks.lock().unwrap().get(binding).cloned();
        if let Some(sink) = sink {
            for _ in 0..self.close_event_count.load(Ordering::Acquire) {
                sink.activity_writer()
                    .send_wait(forge_provider_api::AdapterActivity::Warning(
                        forge_provider_api::ProviderWarning {
                            scope: forge_provider_api::WarningScope::Provider,
                            kind: forge_provider_api::ProviderWarningKind::NativeNotice {
                                category: ProviderFailureKind::Unknown,
                            },
                            message: forge_provider_api::SanitizedMessage::new("cleanup progress")
                                .unwrap(),
                        },
                    ))
                    .await?;
            }
        }
        self.close_native_stream(binding);
        Ok(())
    }

    async fn turn_start(
        &self,
        operation: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::PromptRequest,
    ) -> Result<forge_provider_api::InputAcceptance, ProviderError> {
        operation.check_admission(ProviderOperation::TurnStart)?;
        if let Some(error) = self.input_failure.lock().unwrap().clone() {
            return Err(error);
        }
        Err(provider_failure(
            ProviderOperation::TurnStart,
            ProviderFailureKind::Rejected,
        ))
    }

    async fn turn_interrupt(
        &self,
        operation: forge_provider_api::OperationContext,
        _: &BindingId,
        _: forge_provider_api::CancelRequest,
    ) -> Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        operation.check_admission(ProviderOperation::TurnInterrupt)?;
        self.cancel_calls.fetch_add(1, Ordering::AcqRel);
        Err(provider_failure(
            ProviderOperation::TurnInterrupt,
            ProviderFailureKind::WrongInvocation,
        ))
    }

    async fn inspect(
        &self,
        operation: forge_provider_api::OperationContext,
        _: forge_provider_api::WorkspaceContext,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        operation.check_admission(ProviderOperation::Inspect)?;
        Err(provider_failure(
            ProviderOperation::Inspect,
            ProviderFailureKind::Runtime,
        ))
    }

    async fn shutdown(
        &self,
        operation: forge_provider_api::OperationContext,
    ) -> Result<(), ProviderError> {
        operation.check_admission(ProviderOperation::Shutdown)?;
        self.shutdown_calls.fetch_add(1, Ordering::AcqRel);
        let callback = self.shutdown_callback.lock().await.take();
        if let Some(callback) = callback {
            let response = callback.await.map_err(|_| {
                provider_failure(ProviderOperation::Shutdown, ProviderFailureKind::Runtime)
            })?;
            if !matches!(
                response,
                forge_provider_api::permissions::PermissionResponse::Deny { .. }
            ) {
                return Err(provider_failure(
                    ProviderOperation::Shutdown,
                    ProviderFailureKind::Authorization,
                ));
            }
        }
        if self.shutdown_failed.load(Ordering::Acquire) {
            return Err(provider_failure(
                ProviderOperation::Shutdown,
                ProviderFailureKind::Transport,
            ));
        }
        self.sinks.lock().unwrap().clear();
        self.duplicate_sinks.lock().unwrap().clear();
        Ok(())
    }

    async fn terminal_list(
        &self,
        operation: forge_provider_api::OperationContext,
        _: &BindingId,
    ) -> Result<forge_provider_api::terminal::TerminalSnapshot, ProviderError> {
        operation.check_admission(ProviderOperation::TerminalList)?;
        self.terminal_calls.fetch_add(1, Ordering::AcqRel);
        self.terminal_started.notify_one();
        self.terminal_release.notified().await;
        Ok(forge_provider_api::terminal::TerminalSnapshot {
            supported: true,
            starting: false,
            terminal: Vec::new(),
        })
    }
}

fn provider_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Consume the host-owned manifest that every successful binding open publishes first.
fn expect_negotiated_capabilities(events: &mut HostEventStream) {
    let observed = events
        .try_recv()
        .expect("a successful open publishes its negotiated capabilities");
    assert!(
        observed.native.is_none(),
        "negotiated capabilities are a host publication without a native sequence"
    );
    assert!(matches!(
        observed.event,
        super::super::session_execution::OwnerObservation::Event(
            super::super::HarnessEvent::Host {
                scope: forge_provider_api::EventScope::Binding { .. },
                event: super::super::HostEvent::CapabilitiesNegotiated { .. },
            }
        )
    ));
}

fn provider_request(root: &std::path::Path) -> ProviderRequest {
    ProviderRequest {
        dispatch: forge_provider_api::submission::DispatchTracker::default(),
        execution_owner: None,
        harness_session_id: "bridge-test-session".into(),
        workspace: root.to_string_lossy().into_owned(),
        input: super::super::ProviderInput::from_text(""),
        mode: super::super::PromptMode::Chat,
        model: "default".into(),
        effort: "default".into(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        approval_mode: Default::default(),
        provider_session_id: None,
        submission: None,
        acknowledged_child_reports: Vec::new(),
        recovered_input: Vec::new(),
        control_context: None,
    }
}

async fn bridge(
    root: &std::path::Path,
    native: Arc<BridgeFixtureProvider>,
) -> Result<ProviderBridge, anyhow::Error> {
    Ok(ProviderBridge::new(
        forge_providers::registry::RegisteredProvider::new(native.descriptor(), native, None),
        PermissionCoordinator::transient(root)?,
        Arc::new(crate::trace::TraceStore::open(root)?),
        root.to_owned(),
    ))
}

/// Quarantine a reused generation and retain both streams until provider-wide collection succeeds.
#[tokio::test]
async fn duplicate_binding_preserves_original_registry_and_quarantines_cleanup()
-> anyhow::Result<()> {
    let stage = std::sync::Mutex::new("open original");
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let root = tempfile::tempdir()?;
        drop(crate::storage::SqliteStore::open(root.path())?);
        let database = rusqlite::Connection::open(crate::storage::state::database(root.path()))?;
        database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('bridge-test-session','workspace',1,'{}'),('second-bridge-owner','workspace',1,'{}')", [])?;
        let native = BridgeFixtureProvider::new(false);
        let provider = Arc::new(bridge(root.path(), native.clone()).await?);
        let binding = BindingId::new("duplicate-generation").unwrap();
        *native.open_binding_override.lock().unwrap() = Some(binding.clone());
        let request = provider_request(root.path());
        let first_owner = request.owner()?;
        let mut original_events = provider.session_events(first_owner.clone()).await?;
        let observing = provider.clone();
        let observer = tokio::spawn(async move {
            while let Some(event) = original_events.recv().await? {
                if let Some((binding, sequence)) = event.native { observing.event_consumed(&binding, sequence).await?; }
            }
            Ok::<_, ProviderError>(())
        });
        provider.thread_open(request).await?;
        let original = provider
            .ownership
            .lock()
            .unwrap()
            .get(&binding)
            .unwrap()
            .clone();
        let original_scope =
            forge_provider_api::InvocationScope::new(first_owner.clone(), "original-exchange");
        original.lock().unwrap().prepare(original_scope.clone())?;
        let original_sink = native.sinks.lock().unwrap().get(&binding).unwrap().clone();
        let child_scope = forge_provider_api::InvocationScope::new(
            ThreadOwner::new(first_owner.session_id().clone(), forge_provider_api::AgentId::new("pending-native-child-owner")?)?,
            original_scope.exchange_id.clone(),
        );
        let child_address = forge_provider_api::TurnAddress { thread_id: child_scope.owner.thread_id(), turn_id:child_scope.turn_id()?, scope:child_scope.clone() };
        let agent_id = forge_provider_api::AgentId::new("pending-native-child")?;
        let proof = {
            let mut registry = original.lock().unwrap();
            registry.resolve(&binding, &ProviderEvent::Agent { agent_id:agent_id.clone(), event:forge_provider_api::agent::AgentEvent::Lifecycle { event:forge_provider_api::agent::AgentLifecycleEvent::Started {
                agent:forge_provider_api::agent::AgentDescription { parent_agent_id:Some(first_owner.agent_id().clone()),delegation:Some(forge_provider_api::agent::AgentDelegation {parent_thread_id:first_owner.thread_id(),parent_turn_id:original_scope.turn_id()?,parent_item_id:None}),thread_id:Some(child_address.thread_id.clone()),definition:None,nickname:None,task:None },
                state:forge_provider_api::agent::AgentActiveState::Running,
            } } })?;
            registry.admit_observed(child_address.clone())?;
            registry.observed_agent(&agent_id, Some(&child_address.turn_id))?
        };
        let mut original_request = provider_request(root.path());
        original_request.approval_mode = crate::session::ApprovalMode::Ask;
        let _directories = provider.permissions.directories(Some(root.path()), &original_request, &original_scope)?;
        provider.permissions.store().write().unwrap().set_rule_list(&[(crate::permissions::document::CATEGORY_TOOL.into(), "fixture-pending-child".into(), crate::permissions::document::PermissionDecision::Ask)])?;
        let template = crate::permissions::provider::ProviderPermissionContext::new(provider.permissions.clone());
        template.bind_provider_fence(provider.closed.clone())?;
        template.configure(first_owner.clone(), crate::session::ApprovalMode::Ask, root.path().display().to_string(), Some(crate::provider::events::channel(original_scope.clone()).0))?;
        let (child_sink, mut child_events) = crate::provider::events::channel(child_scope.clone());
        let child_policy = template.fork_observed(&proof, child_sink)?;
        provider.sessions.lock().await.get(&first_owner).unwrap().observed_contexts.lock().unwrap().insert(child_address.turn_id.clone(), child_policy.clone());
        let mut callback = tokio::spawn(async move {
            use forge_provider_api::permissions::ProviderPermissions;
            child_policy.authorize(&child_scope, forge_provider_api::permissions::PermissionRequest { id:"native-pending".into(),provider:forge_providers::registry::nonselectable_id().unwrap(),reason:None,target_list:vec![forge_provider_api::permissions::PermissionTarget::Tool {target:"fixture-pending-child".into()}] }).await
        });
        *stage.lock().unwrap() = "await child approval";
        loop {
            let event = tokio::select! {
                event = child_events.recv() => event?.expect("pending native child approval is published"),
                result = &mut callback => anyhow::bail!("child approval finished before request: {result:?}"),
            };
            if matches!(event, super::super::session_execution::OwnerObservation::Event(crate::provider::HarnessEvent::Host {event:crate::provider::HostEvent::PermissionApproval {event:crate::provider::PermissionApprovalEvent::Requested(_),..},..})) { break; }
        }
        assert!(!callback.is_finished());
        *native.shutdown_callback.lock().await = Some(callback);
        *stage.lock().unwrap() = "open duplicate";
        *native.open_binding_override.lock().unwrap() = Some(binding.clone());
        let mut second = provider_request(root.path());
        second.harness_session_id = "second-bridge-owner".into();
        let second_owner = second.owner()?;
        database.execute_batch("CREATE TRIGGER reject_duplicate_open_completion BEFORE INSERT ON owner_core_log WHEN json_extract(NEW.record,'$.record')='operation_completed' AND json_extract(NEW.record,'$.operation.kind')='thread_open' BEGIN SELECT RAISE(ABORT,'duplicate completion evidence failure'); END;")?;
        assert!(matches!(provider.thread_open(second).await, Err(ProviderError::Combined { .. })));
        assert!(Arc::ptr_eq(
            &original,
            provider.ownership.lock().unwrap().get(&binding).unwrap()
        ));
        assert_eq!(
            original.lock().unwrap().source_status(&first_owner, None)?,
            NativeBindingStatus::Active
        );
        let started = ProviderEvent::Turn {
            thread_id: first_owner.thread_id(),
            turn_id: original_scope.turn_id()?,
            event: forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 1,
            }),
        };
        assert_eq!(
            original.lock().unwrap().resolve(&binding, &started)?,
            original_scope.into()
        );
        let rejected = provider
            .sessions
            .lock()
            .await
            .get(&second_owner)
            .unwrap()
            .clone();
        assert_eq!(
            rejected.open_operation.quarantined_binding(),
            Some(binding.clone())
        );
        let rejected_sink = native.duplicate_sinks.lock().unwrap()[0].clone();
        assert!(provider.session_close_owner(&second_owner).await.is_err());
        assert_eq!(native.retire_calls.load(Ordering::Acquire), 0);
        let capabilities =
            forge_provider_api::CapabilityManifest::from_descriptor(native.descriptor());
        original_sink.capabilities_changed(capabilities.clone()).await?;
        rejected_sink.capabilities_changed(capabilities.clone()).await?;
        native.shutdown_failed.store(true, Ordering::Release);
        *stage.lock().unwrap() = "first shutdown";
        assert!(provider.shutdown().await.is_err());
        assert!(
            rejected
                .initialized
                .get()
                .unwrap()
                .rejected_events
                .as_ref()
                .unwrap()
                .lock()
                .await
                .is_some()
        );
        assert_eq!(native.retire_calls.load(Ordering::Acquire), 0);
        original_sink.capabilities_changed(capabilities.clone()).await?;
        rejected_sink.capabilities_changed(capabilities.clone()).await?;
        native.shutdown_failed.store(false, Ordering::Release);
        *stage.lock().unwrap() = "second shutdown";
        provider.shutdown().await?;
        assert_eq!(native.retire_calls.load(Ordering::Acquire), 0);
        assert_eq!(native.shutdown_calls.load(Ordering::Acquire), 2);
        assert!(provider.sessions.lock().await.is_empty());
        *stage.lock().unwrap() = "collect observer";
        observer.await??;
        assert!(
            rejected_sink
                .capabilities_changed(capabilities.clone()).await
                .is_err()
        );
        assert!(original_sink.capabilities_changed(capabilities).await.is_err());
        Ok::<_, anyhow::Error>(())
    })
    .await.map_err(|error| anyhow::anyhow!("{error} at {}", *stage.lock().unwrap()))?
}

/// Failed post-allocation core evidence closes the exact returned binding without losing its sole stream.
#[tokio::test]
async fn open_completion_log_failure_collects_allocated_binding_and_stream() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    drop(crate::storage::SqliteStore::open(root.path())?);
    let database = rusqlite::Connection::open(crate::storage::state::database(root.path()))?;
    database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('bridge-test-session','workspace',1,'{}')", [])?;
    database.execute_batch("CREATE TRIGGER reject_open_completion BEFORE INSERT ON owner_core_log WHEN json_extract(NEW.record,'$.record')='operation_completed' AND json_extract(NEW.record,'$.operation.kind')='thread_open' BEGIN SELECT RAISE(ABORT,'fixture completion evidence failure'); END;")?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let error = bridge.thread_open(request).await.unwrap_err();
    assert!(matches!(error, ProviderError::Failure(failure)
        if failure.kind() == ProviderFailureKind::Persistence
            && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated));
    assert_eq!(native.open_attempts.lock().unwrap().len(), 1);
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    assert!(native.sinks.lock().unwrap().is_empty());
    assert!(
        bridge
            .session(owner.clone())
            .await
            .initialized
            .get()
            .is_none()
    );
    bridge.session_close_owner(&owner).await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// A failed close remains recorded, while an explicit retry drains saturated output through a fresh exact-binding attempt.
#[tokio::test]
async fn open_log_failure_retries_native_cleanup_and_drains_retained_stream() -> anyhow::Result<()>
{
    let root = tempfile::tempdir()?;
    drop(crate::storage::SqliteStore::open(root.path())?);
    let database = rusqlite::Connection::open(crate::storage::state::database(root.path()))?;
    database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('bridge-test-session','workspace',1,'{}')", [])?;
    database.execute_batch("CREATE TRIGGER reject_open_completion BEFORE INSERT ON owner_core_log WHEN json_extract(NEW.record,'$.record')='operation_completed' AND json_extract(NEW.record,'$.operation.kind')='thread_open' BEGIN SELECT RAISE(ABORT,'fixture completion evidence failure'); END;")?;
    let native = BridgeFixtureProvider::new(true);
    native.close_event_count.store(4097, Ordering::Release);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    assert!(bridge.thread_open(request).await.is_err());
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    assert_eq!(native.sinks.lock().unwrap().len(), 1);
    tokio::time::timeout(
        std::time::Duration::from_secs(5),
        bridge.session_close_owner(&owner),
    )
    .await??;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 2);
    {
        let contexts = native.close_contexts.lock().unwrap();
        assert_eq!(contexts.len(), 2);
        assert_ne!(contexts[0], contexts[1]);
    }
    assert!(native.sinks.lock().unwrap().is_empty());
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    Ok(())
}

/// Broken core admission storage cannot erase an already uncertain native open during cleanup reobservation.
#[tokio::test]
async fn retained_open_cleanup_bypasses_failed_core_start_and_preserves_uncertainty()
-> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    drop(crate::storage::SqliteStore::open(root.path())?);
    let database = rusqlite::Connection::open(crate::storage::state::database(root.path()))?;
    database.execute("INSERT INTO session_record(id,workspace,updated_at_ms,payload) VALUES('bridge-test-session','workspace',1,'{}')", [])?;
    let native = BridgeFixtureProvider::new(false);
    *native.open_persistent_failure.lock().unwrap() = Some(
        ProviderFailure::new(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Transport,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        )
        .into(),
    );
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    assert!(bridge.thread_open(request).await.is_err());
    database.execute_batch("CREATE TRIGGER reject_open_evidence BEFORE INSERT ON owner_core_log BEGIN SELECT RAISE(ABORT,'fixture core unavailable'); END;")?;
    assert!(bridge.session_close_owner(&owner).await.is_err());
    assert!(bridge.sessions.lock().await.contains_key(&owner));
    *native.open_persistent_failure.lock().unwrap() = Some(
        ProviderFailure::new(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Cancelled,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Close,
            forge_provider_api::FailureDisposition::Closed,
        )
        .into(),
    );
    bridge.session_close_owner(&owner).await?;
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    Ok(())
}

/// A retained fork must replay the original source and target even after caller settings change.
#[tokio::test]
async fn fork_retry_preserves_complete_original_request() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let mut source = provider_request(root.path());
    source.provider_session_id = Some("source-history".to_owned().try_into()?);
    let first = super::super::ProviderForkRequest {
        source,
        target_harness_session_id: "fork-target".into(),
        checkpoint_id: Some("checkpoint-one".into()),
    };
    assert!(bridge.thread_fork(first.clone()).await.is_err());
    let mut retry = first;
    retry.source.model = "changed-model".into();
    retry.source.effort = "high".into();
    retry.checkpoint_id = Some("checkpoint-two".into());
    bridge.thread_fork(retry).await?;
    let attempts = native.fork_attempts.lock().unwrap();
    assert_eq!(attempts.len(), 2);
    assert_eq!(attempts[0], attempts[1]);
    Ok(())
}

/// Compaction retry must not open or target a later caller-selected owner.
#[tokio::test]
async fn compact_retry_preserves_original_binding_and_scope() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let first = super::super::ProviderCompactionRequest {
        id: "retained-compaction".into(),
        request: provider_request(root.path()),
    };
    assert!(bridge.thread_compact(first.clone()).await.is_err());
    let mut retry = first;
    retry.request.harness_session_id = "different-owner".into();
    bridge.thread_compact(retry).await?;
    assert_eq!(native.open_attempts.lock().unwrap().len(), 1);
    let attempts = native.compact_attempts.lock().unwrap();
    assert_eq!(attempts.len(), 2);
    assert_eq!(attempts[0], attempts[1]);
    Ok(())
}

/// Reject operation-kind collisions before resolving a binding or performing preparation.
#[tokio::test]
async fn history_identity_conflict_does_not_open_compaction_target() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let mut source = provider_request(root.path());
    source.provider_session_id = Some("source-history".to_owned().try_into()?);
    assert!(
        bridge
            .thread_fork(super::super::ProviderForkRequest {
                source,
                target_harness_session_id: "shared-id".into(),
                checkpoint_id: Some("checkpoint".into()),
            })
            .await
            .is_err()
    );
    let error = bridge
        .thread_compact(super::super::ProviderCompactionRequest {
            id: "fork/shared-id".into(),
            request: provider_request(root.path()),
        })
        .await
        .unwrap_err();
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::OperationConflict)
    );
    assert!(native.open_attempts.lock().unwrap().is_empty());
    assert!(native.compact_attempts.lock().unwrap().is_empty());
    Ok(())
}

/// Shutdown must interrupt a retained fork before awaiting its admission guard.
#[tokio::test]
async fn history_shutdown_cancels_pending_fork() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    native.fork_blocked.store(true, Ordering::Release);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let mut source = provider_request(root.path());
    source.provider_session_id = Some("source-history".to_owned().try_into()?);
    let request = super::super::ProviderForkRequest {
        source,
        target_harness_session_id: "fork-target".into(),
        checkpoint_id: Some("checkpoint".into()),
    };
    let pending = tokio::spawn({
        let bridge = bridge.clone();
        let request = request.clone();
        async move { bridge.thread_fork(request).await }
    });
    native.fork_started.notified().await;
    tokio::time::timeout(std::time::Duration::from_secs(2), bridge.shutdown()).await??;
    assert!(
        matches!(pending.await?, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Cancelled)
    );
    assert!(bridge.thread_fork(request).await.is_err());
    assert_eq!(native.fork_attempts.lock().unwrap().len(), 1);
    Ok(())
}

/// Missing stream evidence must not deadlock shutdown after native compaction has returned.
#[tokio::test]
async fn history_shutdown_cancels_compaction_watermark_wait() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    native.compact_watermark.store(1, Ordering::Release);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = super::super::ProviderCompactionRequest {
        id: "pending-watermark".into(),
        request: provider_request(root.path()),
    };
    assert!(bridge.thread_compact(request.clone()).await.is_err());
    let pending = tokio::spawn({
        let bridge = bridge.clone();
        async move { bridge.thread_compact(request).await }
    });
    native.compact_started.notified().await;
    tokio::time::timeout(std::time::Duration::from_secs(2), bridge.shutdown()).await??;
    assert!(
        matches!(pending.await?, Err(ProviderError::Failure(failure))
        if failure.kind() == ProviderFailureKind::Cancelled && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain)
    );
    Ok(())
}

/// Caller cancellation does not release observation ownership before the actual native response.
#[tokio::test]
async fn terminal_observation_holds_retirement_until_response_after_waiter_is_dropped()
-> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.thread_open(request.clone()).await?;
    let querying = bridge.clone();
    let query =
        tokio::spawn(async move { querying.terminal_list(catalog_request(&request)).await });
    native.terminal_started.notified().await;
    query.abort();
    let _ = query.await;
    let retiring = bridge.retire_owner(&owner);
    tokio::pin!(retiring);
    assert!(futures_util::poll!(&mut retiring).is_pending());
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    native.terminal_release.notify_one();
    retiring.await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    assert!(
        bridge
            .terminal_list(catalog_request(&provider_request(root.path())))
            .await
            .is_err()
    );
    assert_eq!(native.terminal_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// Steering keeps its target invocation's immutable preferences and rejects a mismatched exchange.
#[tokio::test]
async fn steering_seals_original_invocation_configuration_and_validates_full_scope()
-> anyhow::Result<()> {
    use crate::provider::submission::{SubmissionOrigin, SubmissionState, SubmissionStore};
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native).await?;
    let store = SubmissionStore::open(
        root.path(),
        Arc::new(crate::trace::TraceStore::open(root.path())?),
    )?;
    let provider = forge_providers::registry::nonselectable_id()
        .expect("registry supplies a fixture identity");
    let mut first = provider_request(root.path());
    first.model = "original-invocation-model".into();
    first.effort = "high".into();
    first.submission = Some(store.prepare(&first, provider.clone())?);
    let owner = first.owner()?;
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "original-exchange");
    let _original = bridge
        .prepare_dispatch(first.clone(), scope.clone())
        .await?;

    let mut later = provider_request(root.path());
    later.model = "later-invocation-model".into();
    later.submission = Some(store.prepare(&later, provider.clone())?);
    let _later = bridge
        .prepare_dispatch(
            later.clone(),
            forge_provider_api::InvocationScope::new(owner.clone(), "later-exchange"),
        )
        .await?;
    later.input = super::super::ProviderInput::from_text("steering text");
    let receipt = store.prepare_input_request(
        &later,
        provider,
        SubmissionOrigin::QueuedInput(crate::exchange::command::InputId::new("queued-steering")?),
    )?;
    let input = super::super::SteeringInput::User {
        text: "steering text".into(),
        attachments: Vec::new(),
        submission: Some(receipt.clone()),
    };
    let (_, prepared) = bridge
        .prepare_steering(&owner, scope.clone(), input.clone())
        .await?;
    assert_eq!(prepared.scope, scope);
    assert_eq!(receipt.record()?.request.model, "original-invocation-model");
    assert_eq!(receipt.record()?.request.effort, "high");
    assert_eq!(receipt.record()?.request.input.text(), "steering text");
    assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);

    let mut mismatched = scope;
    mismatched.exchange_id = "wrong-exchange".into();
    assert!(
        bridge
            .prepare_steering(&owner, mismatched, input)
            .await
            .is_err()
    );
    assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);
    bridge.shutdown().await?;
    Ok(())
}

/// Collect positively prevented host preparation without inventing a native cancellation target.
#[tokio::test]
async fn prevented_host_preparation_skips_native_cancel_and_keeps_binding_reusable()
-> anyhow::Result<()> {
    use crate::provider::host_invocation::NativeInputAdmission;
    use crate::provider::submission::{SubmissionRetirement, SubmissionStore};
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let store = SubmissionStore::open(
        root.path(),
        Arc::new(crate::trace::TraceStore::open(root.path())?),
    )?;
    let provider = forge_providers::registry::nonselectable_id().unwrap();
    let mut expected_cancels = 0;
    for admission in [
        NativeInputAdmission::Preparing,
        NativeInputAdmission::Requested,
    ] {
        let mut request = provider_request(root.path());
        let receipt = store.prepare(&request, provider.clone())?;
        request.submission = Some(receipt.clone());
        let scope = forge_provider_api::InvocationScope::new(request.owner()?, "prepared-exchange");
        let prepared = bridge.prepare_dispatch(request, scope.clone()).await?;
        let binding = prepared.binding.clone();
        let effects = prepared.effects.clone();
        if matches!(admission, NativeInputAdmission::Requested) {
            effects.request_native_input()?;
            expected_cancels += 1;
        }
        let retirement = receipt.retire_prepared_attempt(&receipt.record()?.attempt_id)?;
        assert_eq!(retirement, SubmissionRetirement::Cancelled);
        match admission {
            NativeInputAdmission::Preparing => {
                let error = match prepared.admit(receipt.clone()) {
                    Ok(_) => panic!("retired preparation regained native admission"),
                    Err(error) => error,
                };
                assert!(
                    error.is_clean_cancellation(),
                    "journal retirement lost cancellation identity: {error}"
                );
            }
            NativeInputAdmission::Requested => drop(prepared),
        }
        bridge
            .cancel_prepared_invocation(&scope, retirement)
            .await?;
        assert_eq!(
            native.cancel_calls.load(Ordering::Acquire),
            expected_cancels
        );
        assert!(
            !effects.foreground_pending(),
            "host-only preparation resources were not collected"
        );
        assert!(
            effects
                .request_native_input()
                .unwrap_err()
                .is_clean_cancellation(),
            "collected preparation lost cancellation identity"
        );
        assert!(
            native.sinks.lock().unwrap().contains_key(&binding),
            "preparation cancellation retired its reusable binding"
        );
    }
    bridge.shutdown().await?;
    Ok(())
}

/// Collect positively rejected native admission while retaining every uncertain or invalidated cause.
#[tokio::test]
async fn native_input_rejection_preserves_delivery_proof_and_exact_collection() -> anyhow::Result<()>
{
    use forge_provider_api::{FailureDisposition as Disposition, ProviderPhase as Phase};
    for (phase, disposition, unconsumed) in [
        (Phase::Admission, Disposition::Unchanged, true),
        (Phase::Preparation, Disposition::Unchanged, true),
        (Phase::Observation, Disposition::Uncertain, false),
        (Phase::Collection, Disposition::Invalidated, false),
        (Phase::Admission, Disposition::Closed, false),
        (Phase::Observation, Disposition::Unchanged, false),
    ] {
        let root = tempfile::tempdir()?;
        let native = BridgeFixtureProvider::new(false);
        *native.input_failure.lock().unwrap() = Some(
            ProviderFailure::new(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            )
            .at_phase(phase, disposition)
            .into(),
        );
        let bridge = bridge(root.path(), native.clone()).await?;
        let store = super::super::submission::SubmissionStore::open(
            root.path(),
            Arc::new(crate::trace::TraceStore::open(root.path())?),
        )?;
        let mut request = provider_request(root.path());
        let owner = request.owner()?;
        request.submission = Some(store.prepare(
            &request,
            forge_providers::registry::nonselectable_id().unwrap(),
        )?);
        let scope = forge_provider_api::InvocationScope::new(owner.clone(), "native-rejection");
        let error = bridge.turn_start(request, scope.clone()).await.unwrap_err();
        let cause = match error {
            ProviderError::PreparationFailed(cause) if unconsumed => *cause,
            error if !unconsumed => error,
            error => panic!("positive native admission proof was lost: {error:?}"),
        };
        assert!(matches!(cause, ProviderError::Failure(failure)
            if failure.operation() == ProviderOperation::TurnStart
                && failure.kind() == ProviderFailureKind::WrongInvocation
                && failure.phase() == phase && failure.disposition() == disposition));
        assert_eq!(
            native.cancel_calls.load(Ordering::Acquire),
            usize::from(unconsumed)
        );
        let host = bridge.session(owner).await;
        let effects = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned()
            .unwrap();
        assert_eq!(effects.foreground_pending(), !unconsumed);
        assert_eq!(
            native.sinks.lock().unwrap().len(),
            1,
            "rejection retired the stable binding"
        );
        bridge.shutdown().await?;
    }
    Ok(())
}

/// Keep pre-input settings local, then open one native conversation for the first input.
#[tokio::test]
async fn first_input_opens_one_native_conversation_after_local_configuration() -> anyhow::Result<()>
{
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let selection = crate::settings::PortableSettings::default();
    let mut request = provider_request(root.path());
    // A first `/plan` turn opens the conversation, but later chat turns must not inherit its
    // task contract from the open-time instructions.
    request.mode = crate::provider::PromptMode::Plan;
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    for configured_model in [
        "first-local-model",
        "second-local-model",
        "third-local-model",
    ] {
        request.model = configured_model.into();
        let receipt = bridge
            .thread_configure(request.clone(), &selection, None)
            .await?;
        assert!(matches!(receipt, ConfigureReceipt::Deferred));
    }
    assert!(native.open_attempts.lock().unwrap().is_empty());
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 0);
    // Deferred configuration retains nothing native; the open must use the admitted request.
    request.model = "admitted-model".into();

    let store = super::super::submission::SubmissionStore::open(
        root.path(),
        Arc::new(crate::trace::TraceStore::open(root.path())?),
    )?;
    request.submission = Some(store.prepare(
        &request,
        forge_providers::registry::nonselectable_id().unwrap(),
    )?);
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "first-input");
    // The fixture rejects every input, which still proves the open preceded native dispatch.
    let rejected = bridge.turn_start(request.clone(), scope).await.unwrap_err();
    assert!(
        matches!(&rejected, ProviderError::PreparationFailed(cause)
            if matches!(cause.as_ref(), ProviderError::Failure(failure)
                if failure.operation() == ProviderOperation::TurnStart
                    && failure.kind() == ProviderFailureKind::Rejected)),
        "{rejected:?}"
    );
    assert_eq!(native.open_attempts.lock().unwrap().len(), 1);
    assert_eq!(native.sinks.lock().unwrap().len(), 1);
    let chat_baseline = crate::provider::ProviderRequest {
        mode: crate::provider::PromptMode::Chat,
        ..request.clone()
    }
    .system_message();
    assert_eq!(
        *native.open_instructions.lock().unwrap(),
        vec![chat_baseline]
    );
    assert_eq!(
        *native.open_models.lock().unwrap(),
        vec![Some("admitted-model".to_owned())]
    );
    expect_negotiated_capabilities(&mut events);

    // After the first open captured settings, configuration follows the native ordered path.
    bridge
        .thread_configure(request.clone(), &selection, None)
        .await?;
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);
    assert_eq!(native.open_attempts.lock().unwrap().len(), 1);
    bridge.shutdown().await?;
    Ok(())
}

#[tokio::test]
async fn cold_configuration_defers_native_write_until_initialization() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let selection = crate::settings::PortableSettings::default();

    let receipt = bridge
        .thread_configure(request.clone(), &selection, None)
        .await?;
    assert!(matches!(receipt, ConfigureReceipt::Deferred));
    let _events = bridge.session_events(request.owner()?).await?;
    let receipt = bridge
        .thread_configure(request.clone(), &selection, None)
        .await?;
    assert!(matches!(receipt, ConfigureReceipt::Deferred));
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 0);

    bridge.thread_open(request.clone()).await?;
    bridge
        .thread_configure(request.clone(), &selection, None)
        .await?;
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);

    bridge.shutdown().await?;
    assert!(matches!(
        bridge
            .thread_configure(request.clone(), &selection, None)
            .await,
        Err(ProviderError::Failure(failure))
            if failure.operation() == ProviderOperation::ThreadConfigure
                && failure.kind() == ProviderFailureKind::BindingClosed
    ));
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// Bind configuration history to its consumed native watermark without retaining closed resources.
#[tokio::test]
async fn configuration_receipt_rejects_new_operation_identity_and_retired_generation()
-> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let root = tempfile::tempdir()?;
        let native = BridgeFixtureProvider::new(false);
        *native.open_identity.lock().unwrap() = Some("before".try_into()?);
        *native.configured_identity.lock().unwrap() = Some("after-first".try_into()?);
        let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
        let request = provider_request(root.path());
        let owner = request.owner()?;
        let mut events = bridge.session_events(owner.clone()).await?;
        bridge.thread_open(request.clone()).await?;

        // A configured result cannot certify identity before the owner consumes its exact publication.
        let applying = bridge.clone();
        let desired = request.clone();
        let configured = tokio::spawn(async move {
            applying
                .thread_configure(desired, &crate::settings::PortableSettings::default(), None)
                .await
        });
        let (binding, watermark) = identity_boundary(&mut events, "after-first").await?;
        assert!(
            !configured.is_finished(),
            "configuration bypassed its identity watermark"
        );
        bridge.event_consumed(&binding, watermark).await?;
        let ConfigureReceipt::Applied(first) = configured.await?? else {
            anyhow::bail!("open binding configuration was deferred");
        };
        assert_eq!(
            first
                .before_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("before")
        );
        assert_eq!(
            first
                .after_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-first")
        );
        first.validate(&owner)?;
        assert!(first.validate(&ThreadOwner::root("foreign-owner")).is_err());
        assert!(
            first
                .observed_history(&ThreadOwner::root("foreign-owner"))
                .is_err()
        );

        // Merely beginning a newer operation revokes the old proof, even before its identity is consumed.
        *native.configured_identity.lock().unwrap() = Some("after-second".try_into()?);
        let applying = bridge.clone();
        let desired = request.clone();
        let configured = tokio::spawn(async move {
            applying
                .thread_configure(desired, &crate::settings::PortableSettings::default(), None)
                .await
        });
        let (binding, watermark) = identity_boundary(&mut events, "after-second").await?;
        assert!(!configured.is_finished());
        assert!(
            first.validate(&owner).is_err(),
            "new configuration retained old commit authority"
        );
        assert_eq!(
            first
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-first")
        );
        bridge.event_consumed(&binding, watermark).await?;
        let ConfigureReceipt::Applied(second) = configured.await?? else {
            anyhow::bail!("second open binding configuration was deferred");
        };
        assert_eq!(
            second
                .before_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-first")
        );
        assert_eq!(
            second
                .after_history()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-second")
        );
        second.validate(&owner)?;
        assert_eq!(
            first
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("after-second")
        );

        // A later consumed history change and binding retirement independently invalidate the receipt.
        {
            let sink = native.sinks.lock().unwrap().get(&binding).cloned().unwrap();
            sink.activity_writer()
                .send_wait(forge_provider_api::AdapterActivity::Thread {
                    thread_id: sink.scope().owner().thread_id(),
                    event: forge_provider_api::ThreadEvent::IdentityResolved(
                        "later-history".try_into()?,
                    ),
                })
                .await?;
        }
        let (binding, watermark) = identity_boundary(&mut events, "later-history").await?;
        bridge.event_consumed(&binding, watermark).await?;
        assert!(
            second.validate(&owner).is_err(),
            "later history reused a configuration receipt"
        );
        assert_eq!(
            second
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("later-history")
        );
        let retained_host = second.host.clone();
        bridge.session_close_owner(&owner).await?;
        assert!(second.validate(&owner).is_err());
        assert!(second.observed_history(&owner).is_err());
        assert!(
            retained_host.upgrade().is_none(),
            "configuration proof retained host resources"
        );
        bridge.shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

/// Capture a native identity boundary while leaving its consumption acknowledgment to the test owner.
async fn identity_boundary(
    events: &mut HostEventStream,
    expected: &str,
) -> anyhow::Result<(BindingId, forge_provider_api::EventSequence)> {
    loop {
        let event = events
            .recv()
            .await?
            .ok_or_else(|| anyhow::anyhow!("identity observation closed"))?;
        if let crate::provider::session_execution::OwnerObservation::Event(
            crate::provider::HarnessEvent::Native {
                event: boxed_native_event,
                ..
            },
        ) = event.event
            && let ProviderEvent::Thread {
                event: forge_provider_api::ThreadEvent::IdentityResolved(provider_session_id),
                ..
            } = boxed_native_event.as_ref()
        {
            assert_eq!(provider_session_id.as_str(), expected);
            return event
                .native
                .ok_or_else(|| anyhow::anyhow!("native identity lost its binding watermark"));
        }
        assert!(
            event.native.is_none(),
            "unconsumed native event preceded identity boundary"
        );
    }
}

/// Publish a new binding identity only after metadata used by the broker's stale-event filter advances.
#[tokio::test]
async fn native_identity_metadata_precedes_queued_owner_publication() -> anyhow::Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let root = tempfile::tempdir()?;
        let native = BridgeFixtureProvider::new(false);
        *native.open_identity.lock().unwrap() = Some("opened".try_into()?);
        *native.configured_identity.lock().unwrap() = Some("configured".try_into()?);
        let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
        let request = provider_request(root.path());
        let owner = request.owner()?;
        let inbox =
            Arc::new(crate::provider::session_execution::SessionObservationInbox::default());
        let registry = crate::provider::session_execution::SessionExecutionRegistry::with_observer(
            bridge.clone(),
            owner.clone(),
            inbox.clone(),
            Arc::new(crate::trace::TraceStore::open(root.path())?),
        );
        let engine = registry.owner(owner.clone()).await?;
        bridge.thread_open(request.clone()).await?;
        let ConfigureReceipt::Applied(receipt) = bridge
            .thread_configure(request, &crate::settings::PortableSettings::default(), None)
            .await?
        else {
            anyhow::bail!("initialized configuration was deferred");
        };

        // Drain a FIFO marker before gating consumption, excluding bootstrap publications from the assertion.
        let host = bridge.session(owner.clone()).await;
        host.sender
            .send(Ok(ProviderObservation::host(crate::provider::HarnessEvent::Host {
                scope: forge_provider_api::EventScope::Binding { owner: owner.clone() },
                event: crate::provider::HostEvent::Failure {
                    failure: forge_provider_api::ProviderFailure::new(ProviderOperation::ThreadConfigure, ProviderFailureKind::Unknown),
                    message: forge_provider_api::SanitizedMessage::new("identity-publication-marker")?,
                },
            })))?;
        loop {
            if inbox.drain()?.iter().any(|event| {
                matches!(event, crate::provider::session_execution::OwnerObservation::Event(crate::provider::HarnessEvent::Host { event: crate::provider::HostEvent::Failure { message, .. }, .. }) if message.as_str() == "identity-publication-marker")
            }) {
                break;
            }
            inbox.notified().await?;
        }
        assert_eq!(engine.presentation_queued_snapshot(), 0);
        let sessions = bridge.sessions.lock().await;
        let consuming = bridge.consumption_entered.notified();
        tokio::pin!(consuming);
        consuming.as_mut().enable();
        {
            let binding = &host.initialized.get().unwrap().binding.id;
            let sink = native.sinks.lock().unwrap().get(binding).cloned().unwrap();
            sink.activity_writer().send_wait(forge_provider_api::AdapterActivity::Thread { thread_id: sink.scope().owner().thread_id(), event: forge_provider_api::ThreadEvent::IdentityResolved("advanced".try_into()?) })
            .await?;
        }
        consuming.await;

        // The current-thread worker either retains an enqueued frame or pushes its inbox without another await.
        assert_eq!(
            engine.presentation_queued_snapshot(),
            0,
            "identity was queued before metadata consumption"
        );
        assert!(
            inbox.drain()?.is_empty(),
            "identity was published before metadata consumption"
        );
        assert_eq!(
            receipt
                .observed_history(&owner)?
                .as_ref()
                .map(forge_provider_api::ProviderSessionId::as_str),
            Some("configured")
        );
        drop(sessions);
        loop {
            let events = inbox.drain()?;
            if events.iter().any(|event| {
                matches!(event, crate::provider::session_execution::OwnerObservation::Event(crate::provider::HarnessEvent::Native { event: boxed_native_event, .. })
                 if matches!(boxed_native_event.as_ref(), ProviderEvent::Thread { event: forge_provider_api::ThreadEvent::IdentityResolved(provider_session_id), .. } if provider_session_id.as_str() == "advanced"))
            }) {
                assert_eq!(
                    receipt
                        .observed_history(&owner)?
                        .as_ref()
                        .map(forge_provider_api::ProviderSessionId::as_str),
                    Some("advanced")
                );
                break;
            }
            inbox.notified().await?;
        }
        assert!(
            receipt.validate(&owner).is_err(),
            "later identity retained configuration commit authority"
        );
        registry.close().await?;
        drop(host);
        bridge.shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await?
}

#[tokio::test]
async fn unexpected_native_eof_retains_failed_close_for_retry() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(true);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner).await?;
    bridge.thread_open(request).await?;
    expect_negotiated_capabilities(&mut events);
    assert!(matches!(events.try_recv(), Err(EventDeliveryError::Empty)));

    let session = bridge
        .sessions
        .lock()
        .await
        .values()
        .next()
        .cloned()
        .unwrap();
    let binding = session.initialized.get().unwrap().binding.id.clone();
    native.close_native_stream(&binding);

    let error = tokio::time::timeout(std::time::Duration::from_secs(2), events.recv())
        .await?
        .expect_err("unexpected native EOF was not reported to the host stream");
    assert!(matches!(error, EventDeliveryError::Closed));
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        while native.retire_calls.load(Ordering::Acquire) == 0 {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    let _ = bridge.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn foreground_cleanup_does_not_cancel_a_settled_invocation_observer() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.thread_open(request).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().unwrap();
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "settled-test-turn");
    let (sink, _control_stream) = crate::provider::events::channel(scope.clone());
    let endpoint = Arc::new(crate::tools::control_session::ControlSession::new(
        Vec::new(),
    ));
    let context = crate::tools::ControlTurnContext::inactive(super::super::PromptMode::Chat);
    let permissions = Arc::new(
        crate::permissions::provider::ProviderPermissionContext::new(bridge.permissions.clone()),
    );
    permissions.configure(
        owner.clone(),
        crate::session::ApprovalMode::Ask,
        root.path().to_string_lossy().into_owned(),
        Some(sink.clone()),
    )?;
    endpoint.begin(context.clone(), sink).await;
    let (_event_sink, event_stream) = crate::provider::events::channel(scope.clone());
    let server = ControlServer::start(endpoint.clone()).await?;
    let endpoint_url = server.url.clone();
    let endpoint_lease = Arc::new(EndpointLease { _server: server });
    let relay = crate::provider::host_invocation::HostEventRelay::start(
        bound.binding.id.clone(),
        event_stream,
        host.sender.clone(),
    );
    let invocation = Arc::new(crate::provider::host_invocation::HostInvocation::new(
        bound.binding.id.clone(),
        scope.clone(),
        &provider_request(root.path()),
        context,
        &[],
        crate::provider::host_invocation::HostInvocationResources {
            observed_authority: Arc::new(ObservedPermissionAuthority::new(
                bound.binding.id.clone(),
                scope.clone(),
                Arc::downgrade(&permissions),
                Arc::downgrade(&host),
                crate::provider::events::channel(scope.clone()).0,
            )),
            provider_closed: bridge.closed.clone(),
            permissions: permissions.clone(),
            directories: None,
            endpoint: endpoint.clone(),
            endpoint_lease,
            relay,
            skills: None,
        },
    ));
    let delegated: Arc<dyn forge_provider_api::control::ProviderTools> = invocation.clone();
    endpoint.bind_host(scope.clone(), Arc::downgrade(&delegated));
    // The HTTP route used by MCP adapters must enforce the invocation's empty allowlist.
    let client = reqwest::Client::builder().no_proxy().build()?;
    let initialized = client
        .post(&endpoint_url)
        .json(&serde_json::json!({"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}))
        .send()
        .await?;
    let session_id = initialized
        .headers()
        .get("mcp-session-id")
        .unwrap()
        .to_str()?
        .to_owned();
    let listed: serde_json::Value = client
        .post(&endpoint_url)
        .header("Mcp-Session-Id", &session_id)
        .json(&serde_json::json!({"jsonrpc":"2.0","id":2,"method":"tools/list"}))
        .send()
        .await?
        .json()
        .await?;
    assert_eq!(listed["result"]["tools"], serde_json::json!([]));
    let rejected: serde_json::Value = client.post(&endpoint_url).header("Mcp-Session-Id", &session_id)
        .json(&serde_json::json!({"jsonrpc":"2.0","id":3,"method":"tools/call", "params":{"name":"goal_complete","arguments":{"summary":"must not execute"}}}))
        .send().await?.json().await?;
    assert_eq!(rejected["result"]["isError"], true);
    host.invocations
        .lock()
        .await
        .insert(scope.invocation_id.clone(), invocation.clone());
    forge_provider_api::session::ProviderHostEffects::settle_foreground(
        invocation.as_ref(),
        &bound.binding.id,
        &scope,
    )
    .await?;
    assert!(!invocation.foreground_pending());
    assert!(!invocation.status(&scope).await?.cancelled);

    let mut wrong_scope = scope.clone();
    wrong_scope.exchange_id = "foreign-exchange".into();
    assert!(bridge.cancel_invocation(wrong_scope).await.is_err());
    assert!(!invocation.status(&scope).await?.cancelled);
    bridge.collect_host_effects(&scope).await?;

    assert!(!invocation.status(&scope).await?.cancelled);
    assert!(
        host.invocations
            .lock()
            .await
            .contains_key(&scope.invocation_id)
    );
    assert!(
        forge_provider_api::permissions::ProviderPermissions::before_tool(
            permissions.as_ref(),
            &scope
        )
        .await
        .is_ok()
    );
    bridge.session_close_owner(&owner).await?;
    assert!(matches!(
        forge_provider_api::permissions::ProviderPermissions::before_tool(
            permissions.as_ref(),
            &scope
        )
        .await,
        Err(forge_provider_api::ProviderError::Cancelled)
    ));
    let _ = bridge.shutdown().await;
    Ok(())
}

#[tokio::test]
async fn mcp_mutation_reply_waits_for_live_commit_and_replays_same_call() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.thread_open(request).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().unwrap();
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "settled-test-turn");
    let (sink, _control_stream) = crate::provider::events::channel(scope.clone());
    let endpoint = Arc::new(crate::tools::control_session::ControlSession::new(
        crate::tools::ControlToolRegistry.definition_list(),
    ));
    let mut context = crate::tools::ControlTurnContext::inactive(super::super::PromptMode::Chat);
    let (_task_root, _task_store, task_controller) =
        crate::task::SessionTaskController::test_goal();
    context.bind_test_task_controller(task_controller);
    let (live, mut live_receiver) = crate::provider::LiveHostEffectSender::channel();
    context.live_host_effects = Some(live);
    let permissions = Arc::new(
        crate::permissions::provider::ProviderPermissionContext::new(bridge.permissions.clone()),
    );
    permissions.configure(
        owner.clone(),
        crate::session::ApprovalMode::Ask,
        root.path().to_string_lossy().into_owned(),
        Some(sink.clone()),
    )?;
    endpoint.begin(context.clone(), sink).await;
    let (_event_sink, event_stream) = crate::provider::events::channel(scope.clone());
    let server = ControlServer::start(endpoint.clone()).await?;
    let endpoint_lease = Arc::new(EndpointLease { _server: server });
    let relay = crate::provider::host_invocation::HostEventRelay::start(
        bound.binding.id.clone(),
        event_stream,
        host.sender.clone(),
    );
    let invocation = Arc::new(crate::provider::host_invocation::HostInvocation::new(
        bound.binding.id.clone(),
        scope.clone(),
        &provider_request(root.path()),
        context,
        &crate::tools::ControlToolRegistry.definition_list(),
        crate::provider::host_invocation::HostInvocationResources {
            observed_authority: Arc::new(ObservedPermissionAuthority::new(
                bound.binding.id.clone(),
                scope.clone(),
                Arc::downgrade(&permissions),
                Arc::downgrade(&host),
                crate::provider::events::channel(scope.clone()).0,
            )),
            provider_closed: bridge.closed.clone(),
            permissions: permissions.clone(),
            directories: None,
            endpoint: endpoint.clone(),
            endpoint_lease,
            relay,
            skills: None,
        },
    ));
    let delegated: Arc<dyn forge_provider_api::control::ProviderTools> = invocation.clone();
    endpoint.bind_host(scope.clone(), Arc::downgrade(&delegated));
    let call = forge_provider_api::control::ToolCall {
        id: forge_provider_api::control::ToolCallId::new("mcp-live-goal")?,
        name: "goal_complete".into(),
        arguments: serde_json::json!({"summary":"completed goal"}),
    };
    let mut pending = Box::pin(crate::tools::control_session::ControlEndpoint::call(
        endpoint.as_ref(),
        call.clone(),
    ));
    assert!(futures_util::poll!(pending.as_mut()).is_pending());
    let effect = tokio::select! {
        effect = live_receiver.recv() => effect.expect("mutation requests owner commit"),
        result = pending.as_mut() => panic!("mutation replied before owner commit: {result:?}"),
    };
    assert_eq!(effect.call_id, call.id);
    assert!(effect.batch.completion.goal_control.is_some());
    assert!(futures_util::poll!(pending.as_mut()).is_pending());
    let mut concurrent = Box::pin(crate::tools::control_session::ControlEndpoint::call(
        endpoint.as_ref(),
        call.clone(),
    ));
    assert!(futures_util::poll!(concurrent.as_mut()).is_pending());
    assert!(
        live_receiver.try_recv().is_err(),
        "same-call retry must share the original commit"
    );
    effect
        .reply
        .send(Ok(crate::provider::LiveHostCommit { task_permit: None }))
        .map_err(|_| anyhow::anyhow!("callback lost its commit waiter"))?;
    let reply = pending.await?;
    assert_eq!(concurrent.await?.message, reply.message);
    invocation.cancel();
    let replay =
        crate::tools::control_session::ControlEndpoint::call(endpoint.as_ref(), call).await?;
    assert_eq!(reply.message, replay.message);
    assert!(live_receiver.try_recv().is_err());
    invocation.cancel();
    let _ = bridge.shutdown().await;
    Ok(())
}

/// A host tool that fails before producing effects leaves host collection certain.
///
/// Cancelling a turn while `question_ask` waits makes the executor fail the call before it
/// publishes anything. Collection must still succeed, because an uncertain collection fails the
/// owner's observation and blocks every later prompt. The fixture fails the call through task
/// admission, which rejects a plan tool while the session runs a goal.
#[tokio::test]
async fn host_call_failing_before_publication_keeps_collection_certain() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.thread_open(request).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().unwrap();
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "unpublished-call-turn");
    let (sink, _control_stream) = crate::provider::events::channel(scope.clone());
    let endpoint = Arc::new(crate::tools::control_session::ControlSession::new(
        crate::tools::ControlToolRegistry.definition_list(),
    ));
    let mut context = crate::tools::ControlTurnContext::inactive(super::super::PromptMode::Plan);
    let (_task_root, _task_store, task_controller) =
        crate::task::SessionTaskController::test_goal();
    context.bind_test_task_controller(task_controller);
    let permissions = Arc::new(
        crate::permissions::provider::ProviderPermissionContext::new(bridge.permissions.clone()),
    );
    permissions.configure(
        owner.clone(),
        crate::session::ApprovalMode::Ask,
        root.path().to_string_lossy().into_owned(),
        Some(sink.clone()),
    )?;
    endpoint.begin(context.clone(), sink).await;
    let (_event_sink, event_stream) = crate::provider::events::channel(scope.clone());
    let server = ControlServer::start(endpoint.clone()).await?;
    let endpoint_lease = Arc::new(EndpointLease { _server: server });
    let relay = crate::provider::host_invocation::HostEventRelay::start(
        bound.binding.id.clone(),
        event_stream,
        host.sender.clone(),
    );
    let invocation = Arc::new(crate::provider::host_invocation::HostInvocation::new(
        bound.binding.id.clone(),
        scope.clone(),
        &provider_request(root.path()),
        context,
        &crate::tools::ControlToolRegistry.definition_list(),
        crate::provider::host_invocation::HostInvocationResources {
            observed_authority: Arc::new(ObservedPermissionAuthority::new(
                bound.binding.id.clone(),
                scope.clone(),
                Arc::downgrade(&permissions),
                Arc::downgrade(&host),
                crate::provider::events::channel(scope.clone()).0,
            )),
            provider_closed: bridge.closed.clone(),
            permissions: permissions.clone(),
            directories: None,
            endpoint: endpoint.clone(),
            endpoint_lease,
            relay,
            skills: None,
        },
    ));
    let call = forge_provider_api::control::ToolCall {
        id: forge_provider_api::control::ToolCallId::new("unpublished-plan-read")?,
        name: "plan_file_read".into(),
        arguments: serde_json::json!({}),
    };
    let failed =
        forge_provider_api::control::ProviderTools::call(invocation.as_ref(), &scope, call).await;
    assert!(
        failed.is_err(),
        "the fixture call must fail before publishing: {failed:?}"
    );
    host.invocations
        .lock()
        .await
        .insert(scope.invocation_id.clone(), invocation.clone());
    bridge.collect_host_effects(&scope).await.map_err(|error| {
        anyhow::anyhow!("a call without effects made collection fail: {error:?}")
    })?;
    invocation.cancel();
    let _ = bridge.shutdown().await;
    Ok(())
}

/// A returned lifecycle result cannot advance before its event is reduced by the owner.
#[tokio::test]
async fn operation_watermark_waits_for_owner_consumption() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    bridge.thread_open(request.clone()).await?;
    let host = bridge.session(owner.clone()).await;
    let bound = host.initialized.get().expect("the session is opened");
    let sink = native
        .sinks
        .lock()
        .unwrap()
        .get(&bound.binding.id)
        .cloned()
        .unwrap();
    let mut capabilities =
        forge_provider_api::CapabilityManifest::from_descriptor(native.descriptor());
    capabilities.effective_revision = 7;
    sink.capabilities_changed(capabilities).await?;
    let observation = loop {
        let observation = events
            .recv()
            .await?
            .expect("capability update is published");
        if observation.native.is_some() {
            break observation;
        }
    };
    let (binding, sequence) = observation
        .native
        .expect("native events carry their sequence");
    assert_ne!(
        bridge
            .capabilities(catalog_request(&request))
            .await?
            .effective_revision,
        7
    );
    let barrier = bridge.wait_watermark(&owner, sequence);
    tokio::pin!(barrier);
    assert!(futures_util::poll!(barrier.as_mut()).is_pending());
    bridge.event_consumed(&binding, sequence).await?;
    barrier.await?;
    assert_eq!(
        bridge
            .capabilities(catalog_request(&request))
            .await?
            .effective_revision,
        7
    );
    let mut stale = forge_provider_api::CapabilityManifest::from_descriptor(native.descriptor());
    stale.effective_revision = 3;
    sink.capabilities_changed(stale).await?;
    let stale_observation = events
        .recv()
        .await?
        .expect("stale capability event is retained");
    let (binding, sequence) = stale_observation
        .native
        .expect("original native provenance is retained");
    bridge.event_consumed(&binding, sequence).await?;
    assert_eq!(
        bridge
            .capabilities(catalog_request(&request))
            .await?
            .effective_revision,
        7
    );
    drop(sink);
    bridge.shutdown().await?;
    Ok(())
}

/// Closed observation cannot certify an operation whose published prefix was never consumed.
#[tokio::test]
async fn operation_watermark_rejects_early_stream_closure() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    bridge.thread_open(request).await?;
    expect_negotiated_capabilities(&mut events);
    let host = bridge.session(owner.clone()).await;
    let binding = host
        .initialized
        .get()
        .expect("the session is opened")
        .binding
        .id
        .clone();
    assert!(matches!(events.try_recv(), Err(EventDeliveryError::Empty)));
    native.close_native_stream(&binding);
    assert!(
        tokio::time::timeout(std::time::Duration::from_secs(2), events.recv())
            .await?
            .is_err()
    );
    let error = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        bridge.wait_watermark(&owner, forge_provider_api::EventSequence::new(1)),
    )
    .await?
    .unwrap_err();
    assert!(matches!(error, ProviderError::Observation(_)));
    let _ = bridge.shutdown().await;
    Ok(())
}

/// Intentional close keeps a published final prefix routable until the owner reduces it.
#[tokio::test]
async fn close_waits_for_published_prefix_without_fabricating_observation_loss()
-> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    let mut events = bridge.session_events(owner.clone()).await?;
    bridge.thread_open(request).await?;
    expect_negotiated_capabilities(&mut events);
    let host = bridge.session(owner.clone()).await;
    let binding = host
        .initialized
        .get()
        .expect("opened session has a binding")
        .binding
        .id
        .clone();
    let sink = native.sinks.lock().unwrap().get(&binding).cloned().unwrap();
    sink.activity_writer()
        .send_wait(forge_provider_api::AdapterActivity::Thread {
            thread_id: sink.scope().owner().thread_id(),
            event: forge_provider_api::ThreadEvent::IdentityResolved(
                "final-history".to_owned().try_into()?,
            ),
        })
        .await?;
    let observed = events
        .recv()
        .await?
        .expect("final native event is published");
    let (_, sequence) = observed
        .native
        .expect("native identity event has a sequence");
    drop(sink);
    let closing = bridge.session_close_owner(&owner);
    tokio::pin!(closing);
    assert!(futures_util::poll!(closing.as_mut()).is_pending());
    assert!(bridge.sessions.lock().await.contains_key(&owner));
    bridge.event_consumed(&binding, sequence).await?;
    tokio::time::timeout(std::time::Duration::from_secs(2), closing).await??;
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// A definite open rejection releases its attempt identity before settings are corrected.
#[tokio::test]
async fn rejected_open_allows_corrected_configuration_without_operation_conflict()
-> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    *native.open_failure.lock().unwrap() = Some(
        forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
        )
        .into(),
    );
    let bridge = bridge(root.path(), native.clone()).await?;
    let mut request = provider_request(root.path());
    request.context_window = Some("unavailable".into());
    assert!(matches!(
        bridge.thread_open(request.clone()).await,
        Err(ProviderError::Unsupported(_))
    ));
    request.context_window = None;
    bridge.thread_open(request).await?;
    {
        let attempts = native.open_attempts.lock().unwrap();
        assert_eq!(attempts.len(), 2);
        assert_ne!(attempts[0].0, attempts[1].0);
    }
    tokio::time::timeout(std::time::Duration::from_secs(2), bridge.shutdown()).await??;
    Ok(())
}

/// A detached observer must not rebuild an unresolved native open with another payload.
#[tokio::test]
async fn detached_open_observes_original_request_and_operation() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    native.open_blocked.store(true, Ordering::Release);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let mut request = provider_request(root.path());
    let original = request.clone();
    let first_bridge = bridge.clone();
    let first = tokio::spawn(async move { first_bridge.thread_open(original).await });
    tokio::time::timeout(
        std::time::Duration::from_secs(2),
        native.open_started.notified(),
    )
    .await?;
    first.abort();
    let _ = first.await;
    native.open_blocked.store(false, Ordering::Release);
    request.model = "changed-after-detach".into();
    bridge.thread_open(request).await?;
    {
        let attempts = native.open_attempts.lock().unwrap();
        assert_eq!(attempts.len(), 2);
        assert_eq!(attempts[0].0, attempts[1].0);
        assert!(attempts[0].1 == attempts[1].1);
    }
    tokio::time::timeout(std::time::Duration::from_secs(2), bridge.shutdown()).await??;
    Ok(())
}

/// Shutdown interrupts a pending open before awaiting its shared admission guard.
#[tokio::test]
async fn closed_open_transaction_rejects_work_before_native_dispatch() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let session = bridge.session(request.owner()?).await;
    session.open_operation.cancel();
    assert!(matches!(bridge.thread_open(request).await,
        Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed));
    assert!(native.open_attempts.lock().unwrap().is_empty());
    bridge.shutdown().await?;
    Ok(())
}

/// Close cannot forget an unresolved opening merely because no binding has been installed.
#[tokio::test]
async fn dropped_session_receiver_keeps_native_binding_owned_until_close() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    drop(bridge.session_events(owner.clone()).await?);
    assert!(bridge.thread_open(request).await.is_err());
    assert_eq!(native.sinks.lock().unwrap().len(), 1);
    bridge.session_close_owner(&owner).await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    bridge.session_close_owner(&owner).await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    bridge.shutdown().await?;
    Ok(())
}

/// A failed open with positive Closed evidence must not remain unresolved forever.
#[tokio::test]
async fn collected_open_failure_releases_its_pending_transaction() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    *native.open_persistent_failure.lock().unwrap() = Some(
        ProviderFailure::new(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Cancelled,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Close,
            forge_provider_api::FailureDisposition::Closed,
        )
        .into(),
    );
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    assert!(bridge.thread_open(request).await.is_err());
    bridge.session_close_owner(&owner).await?;
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    bridge.shutdown().await?;
    Ok(())
}

/// Close cannot forget an unresolved opening merely because no binding has been installed.
#[tokio::test]
async fn close_retains_uncertain_open_until_collection_is_proven() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    *native.open_persistent_failure.lock().unwrap() = Some(
        ProviderFailure::new(
            ProviderOperation::ThreadOpen,
            ProviderFailureKind::Transport,
        )
        .at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        )
        .into(),
    );
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    assert!(bridge.thread_open(request).await.is_err());
    assert!(bridge.session_close_owner(&owner).await.is_err());
    assert!(
        bridge.sessions.lock().await.contains_key(&owner),
        "uncertain opening lost its owner"
    );
    *native.open_persistent_failure.lock().unwrap() = None;
    bridge.session_close_owner(&owner).await?;
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    bridge.shutdown().await?;
    Ok(())
}

/// Shutdown interrupts a pending open before awaiting its shared admission guard.
#[tokio::test]
async fn shutdown_revokes_pending_open_before_waiting_for_admission() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    native.open_blocked.store(true, Ordering::Release);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let opening = bridge.clone();
    let worker = tokio::spawn(async move { opening.thread_open(request).await });
    native.open_started.notified().await;
    tokio::time::timeout(std::time::Duration::from_secs(2), bridge.shutdown()).await??;
    let error = worker
        .await?
        .expect_err("shutdown must interrupt the held open");
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Cancelled)
    );
    let error = bridge
        .thread_open(provider_request(root.path()))
        .await
        .expect_err("closed admission must reject reopening");
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::BindingClosed)
    );
    Ok(())
}

/// A failed native close retains its live reader without waiting for a success-only EOF barrier.
#[tokio::test]
async fn failed_native_close_retains_reader_for_prompt_exact_binding_retry() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(true);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.thread_open(request.clone()).await?;
    let querying = bridge.clone();
    let query =
        tokio::spawn(async move { querying.terminal_list(catalog_request(&request)).await });
    native.terminal_started.notified().await;
    let error = tokio::time::timeout(
        std::time::Duration::from_secs(1),
        bridge.session_close_owner(&owner),
    )
    .await?
    .unwrap_err();
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.operation() == ProviderOperation::ThreadClose && failure.kind() == ProviderFailureKind::Runtime)
    );
    let host = bridge
        .sessions
        .lock()
        .await
        .get(&owner)
        .cloned()
        .expect("failed close retains host ownership");
    let bound = host
        .initialized
        .get()
        .expect("the retained native binding is initialized");
    let binding = bound.binding.id.clone();
    assert!(native.sinks.lock().unwrap().contains_key(&binding));
    assert!(
        bound
            .relay
            .lock()
            .await
            .as_ref()
            .is_some_and(|reader| !reader.is_finished())
    );
    assert_eq!(
        *host.observers.lock().unwrap(),
        1,
        "failed collection retains the pending query lease"
    );
    native.terminal_release.notify_one();
    query.await??;
    tokio::time::timeout(
        std::time::Duration::from_secs(1),
        bridge.session_close_owner(&owner),
    )
    .await??;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 2);
    assert!(!native.sinks.lock().unwrap().contains_key(&binding));
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    Ok(())
}

/// A pending host query is collection failure even after native resources are positively closed.
#[tokio::test(start_paused = true)]
async fn host_collection_timeout_preserves_closed_native_binding_proof() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = Arc::new(bridge(root.path(), native.clone()).await?);
    let request = provider_request(root.path());
    let owner = request.owner()?;
    bridge.thread_open(request.clone()).await?;
    let querying = bridge.clone();
    let query =
        tokio::spawn(async move { querying.terminal_list(catalog_request(&request)).await });
    native.terminal_started.notified().await;
    let closing = bridge.session_close_owner(&owner);
    tokio::pin!(closing);
    assert!(futures_util::poll!(closing.as_mut()).is_pending());
    assert!(
        tokio::time::timeout(std::time::Duration::from_secs(16), closing.as_mut())
            .await
            .is_err()
    );
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    native.terminal_release.notify_one();
    query.await??;
    closing.await?;
    assert_eq!(
        native.retire_calls.load(Ordering::Acquire),
        1,
        "host retry must preserve the completed native close"
    );
    assert!(!bridge.sessions.lock().await.contains_key(&owner));
    Ok(())
}

/// Carry the enclosing command deadline into native configuration and reject expired work before mutation.
#[tokio::test]
async fn configuration_preserves_original_deadline_before_native_admission() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let _events = bridge.session_events(request.owner()?).await?;
    bridge.thread_open(request.clone()).await?;
    let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(5);
    bridge
        .thread_configure(
            request.clone(),
            &crate::settings::PortableSettings::default(),
            Some(deadline),
        )
        .await?;
    assert_eq!(
        *native.configuration_deadlines.lock().unwrap(),
        vec![Some(deadline)]
    );
    let expired = tokio::time::Instant::now() - std::time::Duration::from_secs(1);
    let failure = bridge
        .thread_configure(
            request,
            &crate::settings::PortableSettings::default(),
            Some(expired),
        )
        .await
        .unwrap_err();
    assert!(matches!(failure, ProviderError::Failure(failure)
        if failure.kind() == ProviderFailureKind::Timeout
            && failure.phase() == forge_provider_api::ProviderPhase::Admission
            && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged));
    assert_eq!(native.reconcile_calls.load(Ordering::Acquire), 1);
    bridge.shutdown().await?;
    Ok(())
}

/// Invalid returned ownership is fenced without dropping the native resource needed by close.
#[tokio::test]
async fn foreign_opened_thread_is_rejected_and_remains_collectable() -> anyhow::Result<()> {
    let root = tempfile::tempdir()?;
    let native = BridgeFixtureProvider::new(false);
    *native.open_thread_override.lock().unwrap() = Some(ThreadOwner::root("foreign").thread_id());
    let bridge = bridge(root.path(), native.clone()).await?;
    let request = provider_request(root.path());
    let owner = request.owner()?;
    assert!(matches!(bridge.thread_open(request.clone()).await,
        Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation));
    assert_eq!(native.open_attempts.lock().unwrap().len(), 1);
    assert!(matches!(bridge.thread_open(request).await,
        Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::WrongInvocation));
    assert_eq!(native.open_attempts.lock().unwrap().len(), 1);
    bridge.session_close_owner(&owner).await?;
    assert_eq!(native.retire_calls.load(Ordering::Acquire), 1);
    Ok(())
}

/// Preserve independent native-child policy and publication after the original parent is collected.
#[tokio::test]
async fn observed_child_policy_survives_parent_collection_and_retires_on_child_close()
-> anyhow::Result<()> {
    use crate::provider::test_support::ProviderTestExt as _;
    use forge_provider_api::agent::{
        AgentActiveState, AgentDelegation, AgentDescription, AgentEvent, AgentLifecycleEvent,
        AgentOutcome,
    };
    use forge_provider_api::message::{ItemCompletion, ItemEvent, ItemStart};
    use forge_provider_api::permissions::{
        PermissionRequest, PermissionResponse, PermissionTarget,
    };
    use forge_provider_api::tool::{
        InspectableArguments, ToolCompletion, ToolDescriptor, ToolOutcome, ToolSemantics,
    };
    use forge_provider_api::{AdapterActivity, AgentId, ItemId};
    let phase = std::sync::Mutex::new("fixture setup");
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let root = tempfile::tempdir()?;
        let readable = root.path().join("child-readable.txt");
        std::fs::write(&readable, "child resource")?;
        let retained = Arc::new(std::sync::Mutex::new(None));
        let capture = retained.clone();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            forge_providers::registry::descriptor(
                &forge_providers::registry::nonselectable_id().unwrap(),
            )?,
            move |request, writer, start, _| {
                let capture = capture.clone();
                async move {
                    start.observed(1).await?;
                    let agent_id = AgentId::new("observed-child").unwrap();
                    let child_owner = ThreadOwner::new(
                        request.scope.owner.session_id().clone(),
                        AgentId::new("independent-child-owner").unwrap(),
                    )
                    .unwrap();
                    let scope = forge_provider_api::InvocationScope::new(
                        child_owner.clone(),
                        request.scope.exchange_id.clone(),
                    );
                    let address = forge_provider_api::TurnAddress {
                        thread_id: child_owner.thread_id(),
                        turn_id: scope.turn_id().unwrap(),
                        scope,
                    };
                    let description = AgentDescription {
                        parent_agent_id: Some(request.scope.owner.agent_id().clone()),
                        delegation: Some(AgentDelegation {
                            parent_thread_id: request.thread_id.clone(),
                            parent_turn_id: request.turn_id.clone(),
                            parent_item_id: None,
                        }),
                        thread_id: Some(address.thread_id.clone()),
                        definition: None,
                        nickname: Some("Independent child".into()),
                        task: None,
                    };
                    writer
                        .send_wait(AdapterActivity::Agent {
                            agent_id: agent_id.clone(),
                            event: AgentEvent::Lifecycle {
                                event: AgentLifecycleEvent::Started {
                                    agent: description.clone(),
                                    state: AgentActiveState::Running,
                                },
                            },
                        })
                        .await?;
                    let child_writer = writer.register_observed_turn(address.clone())?;
                    *capture.lock().unwrap() = Some((
                        request.permissions.clone(),
                        request.provider_tools.clone(),
                        request.scope.clone(),
                        address,
                        writer,
                        child_writer,
                        agent_id,
                        description,
                    ));
                    Ok(crate::provider::test_support::native_result(
                        &request,
                        forge_provider_api::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                    ))
                }
            },
        );
        let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let mut request = provider_request(root.path());
        request.approval_mode = crate::session::ApprovalMode::Allow;
        *phase.lock().unwrap() = "root collection";
        fixture.session_prompt_and_collect(request).await?;
        let (permissions, tools, parent, child, writer, child_writer, agent_id, description) =
            retained
                .lock()
                .unwrap()
                .take()
                .expect("native fixture recorded its child");
        assert_ne!(
            fixture
                .provider_handle()
                .retained_session_id(&parent.owner)
                .await
                .as_ref()
                .map(|id| id.as_str()),
            Some("child-native-history")
        );
        *phase.lock().unwrap() = "first positive child activity after root collection";
        child_writer
            .send_wait(AdapterActivity::Thread {
                thread_id: child.thread_id.clone(),
                event: forge_provider_api::ThreadEvent::IdentityResolved(
                    "child-native-history".try_into().unwrap(),
                ),
            })
            .await?;
        child_writer
            .send_wait(AdapterActivity::Item {
                thread_id: child.thread_id.clone(),
                turn_id: child.turn_id.clone(),
                item_id: ItemId::new("child-tool").unwrap(),
                event: ItemEvent::Started(ItemStart::ToolCall(ToolDescriptor {
                    name: "inspect".into(),
                    title: "Inspect child resource".into(),
                    semantics: ToolSemantics::Other,
                    arguments: InspectableArguments::new(serde_json::Value::Null),
                })),
            })
            .await?;
        *phase.lock().unwrap() = "late child permission";
        permissions.before_observed_child(&parent, &child).await?;
        let call = forge_provider_api::control::ToolCall {
            id: forge_provider_api::control::ToolCallId::new("child-workflow-call").unwrap(),
            name: "goal_status".into(),
            arguments: serde_json::json!({}),
        };
        let foreign_parent = forge_provider_api::InvocationScope::new(
            parent.owner.clone(),
            parent.exchange_id.clone(),
        );
        assert!(
            tools
                .call_observed_child(&foreign_parent, &child, call.clone())
                .await
                .is_err()
        );
        *phase.lock().unwrap() = "late child host callback";
        let denied = tools
            .call_observed_child(&parent, &child, call.clone())
            .await;
        assert!(match &denied {
            Ok(message) => message.contains("does not authorize parent workflow"),
            Err(_) => true,
        });
        let replay = tools
            .call_observed_child(&parent, &child, call.clone())
            .await;
        assert_eq!(format!("{denied:?}"), format!("{replay:?}"));

        let permission = permissions
            .authorize_observed_child(
                &parent,
                &child,
                PermissionRequest {
                    id: "child-read".into(),
                    provider: forge_providers::registry::nonselectable_id().unwrap(),
                    reason: None,
                    target_list: vec![PermissionTarget::Read {
                        path: readable.to_string_lossy().into_owned(),
                    }],
                },
            )
            .await;
        assert!(matches!(permission, PermissionResponse::Allow));
        child_writer
            .send_wait(AdapterActivity::Item {
                thread_id: child.thread_id.clone(),
                turn_id: child.turn_id.clone(),
                item_id: ItemId::new("child-tool").unwrap(),
                event: ItemEvent::Completed(ItemCompletion::ToolCall(ToolCompletion {
                    outcome: ToolOutcome::Succeeded,
                    descriptor: None,
                    output: Some("child read completed".into()),
                    changes: Default::default(),
                })),
            })
            .await?;
        let gate = crate::checkpoint::ToolGate::new();
        let host = fixture
            .provider_handle()
            .sessions
            .lock()
            .await
            .get(&parent.owner)
            .cloned()
            .expect("original parent binding is retained");
        let child_context = host
            .observed_contexts
            .lock()
            .unwrap()
            .get(&child.turn_id)
            .cloned()
            .expect("first positive child activity retained its independent policy");
        let mut child_controls =
            crate::tools::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        child_controls.question_invocation = Some(child.scope.clone());
        child_controls.checkpoint = Some(gate.clone());
        child_context.bind_context(Some(&child_controls))?;
        let callback = permissions.before_observed_child(&parent, &child);
        tokio::pin!(callback);
        assert!(futures_util::poll!(callback.as_mut()).is_pending());
        writer
            .send_wait(AdapterActivity::Agent {
                agent_id,
                event: AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Finished {
                        agent: description,
                        outcome: AgentOutcome::Completed,
                    },
                },
            })
            .await?;
        *phase.lock().unwrap() = "native child retirement releases held original callback";
        assert!(callback.await.is_err());
        assert!(
            !gate.ready(),
            "terminal delivery must release the callback without checkpoint completion"
        );

        loop {
            if permissions
                .before_observed_child(&parent, &child)
                .await
                .is_err()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        assert!(
            tools
                .call_observed_child(&parent, &child, call)
                .await
                .is_err()
        );
        *phase.lock().unwrap() = "fixture owner close";
        fixture.close().await?;
        *phase.lock().unwrap() = "provider shutdown";
        fixture.provider_handle().shutdown().await?;
        Ok::<_, anyhow::Error>(())
    })
    .await
    .map_err(|error| anyhow::anyhow!("{}: {error}", *phase.lock().unwrap()))?
}
