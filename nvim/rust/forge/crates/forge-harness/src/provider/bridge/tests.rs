use super::*;
use crate::provider::ScopeOwner;

use forge_provider_api::{
    BindingId, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    events::{self, ProviderEventSink},
    provider::Provider as NativeProvider,
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
                thread_id: forge_provider_api::ThreadId::from(request.owner()),
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
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
        operation.check_admission(ProviderOperation::TerminalList)?;
        self.terminal_calls.fetch_add(1, Ordering::AcqRel);
        self.terminal_started.notify_one();
        self.terminal_release.notified().await;
        Ok(forge_provider_api::terminal::TerminalInventory::observed(
            Vec::new(),
        ))
    }
}

fn provider_failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
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
    async {
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
        let child_address = forge_provider_api::TurnAddress { thread_id: child_scope.owner().thread_id(), turn_id:child_scope.turn_id()?, scope:child_scope.clone() };
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
    }.await
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
    bridge.shutdown().await?;
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
