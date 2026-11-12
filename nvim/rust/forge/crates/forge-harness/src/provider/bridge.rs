use crate::provider::ScopeOwner;
use std::collections::HashMap;
use std::sync::{Arc, Weak};

use crate::provider::ThreadOwner;
use forge_provider_api::{
    BindingId, ProviderError, ProviderEvent,
    error::{ProviderFailure, ProviderFailureKind, ProviderOperation},
    events::{EventDeliveryError, ProviderEventStream},
    provider::Provider as NativeProvider,
    session::{HostToolEndpoint, ResourceLease, SessionBinding, SessionMode, SessionRequest},
};
use tokio::sync::{Mutex, OnceCell, mpsc};

use crate::tools::control_server::ControlServer;
use crate::tools::control_session::ControlSession;

use super::{ProviderRequest, approval::PermissionCoordinator, operation::OperationControl};

mod observed_permissions;
pub(super) use observed_permissions::ObservedPermissionAuthority;

mod history_operation;

#[cfg(test)]
mod tests;

/// Distinguish retained provenance from a generation still authorized to mutate session metadata.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum NativeBindingStatus {
    Active,
    RetiredKnown,
}

/// Prepare host resources and consume normalized provider observations for each owner.
pub struct ProviderBridge {
    native: Arc<dyn NativeProvider>,
    descriptor: forge_provider_api::ProviderDescriptor,
    test_control: Option<Arc<dyn forge_provider_api::testing::ProviderTestControl>>,
    utility: super::text_generation::TextGenerationJobs,
    history_operations: history_operation::HistoryOperations,
    permissions: Arc<PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    core_events: crate::exchange::core_log::CoreEventLog,
    root: std::path::PathBuf,
    sessions: Mutex<HashMap<ThreadOwner, Arc<HostSession>>>,
    retired_bindings: std::sync::Mutex<HashMap<ThreadOwner, Option<BindingId>>>,
    ownership: std::sync::Mutex<
        HashMap<BindingId, Arc<std::sync::Mutex<super::ownership::OwnershipRegistry>>>,
    >,
    binding_collection: Arc<BindingCollection>,
    admission: tokio::sync::RwLock<()>,
    closed: Arc<std::sync::atomic::AtomicBool>,
}

/// Retain ambiguous native generations until provider-wide collection proves all resources closed.
#[derive(Default)]
pub(super) struct BindingCollection {
    returned: std::sync::Mutex<std::collections::HashSet<BindingId>>,
    fence: Arc<std::sync::atomic::AtomicBool>,
    ambiguous: std::sync::Mutex<std::collections::HashSet<BindingId>>,
    collected: std::sync::atomic::AtomicBool,
    changed: tokio::sync::Notify,
}
impl BindingCollection {
    /// Reserve one returned native generation before completion bookkeeping may attempt cleanup.
    pub(super) fn observe_returned(&self, binding: &BindingId) -> bool {
        if self
            .returned
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(binding.clone())
        {
            return true;
        }
        self.reject(binding);
        false
    }
    /// Fence all per-binding authority once an identifier no longer selects one native resource.
    fn reject(&self, binding: &BindingId) {
        self.ambiguous
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(binding.clone());
        self.fence.store(true, std::sync::atomic::Ordering::Release);
        self.changed.notify_waiters();
    }
    /// Get whether provider-wide shutdown has confirmed all native resources collected.
    pub(super) fn collected(&self) -> bool {
        self.collected.load(std::sync::atomic::Ordering::Acquire)
    }
    /// Get whether this identifier no longer uniquely identifies native cleanup authority.
    pub(super) fn ambiguous(&self, binding: &BindingId) -> bool {
        self.ambiguous
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .contains(binding)
    }
    /// Wait for confirmed provider-wide collection while retaining the sole stream.
    async fn wait(&self) {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.collected.load(std::sync::atomic::Ordering::Acquire) {
                return;
            }
            changed.await;
        }
    }
}

struct HostSession {
    provider_closed: Arc<std::sync::atomic::AtomicBool>,
    initialized: OnceCell<BoundSession>,
    observed_authorities: std::sync::Mutex<
        HashMap<forge_provider_api::InvocationId, Arc<ObservedPermissionAuthority>>,
    >,
    observed_contexts: std::sync::Mutex<
        HashMap<
            forge_provider_api::TurnId,
            Arc<crate::permissions::provider::ProviderPermissionContext>,
        >,
    >,
    open_operation: super::session_opening::SessionOpening,
    preparation: Mutex<()>,
    closing: std::sync::atomic::AtomicBool,
    sender: mpsc::UnboundedSender<Result<ProviderObservation, EventDeliveryError>>,
    observation: std::sync::Mutex<ObservationState>,
    changed: tokio::sync::Notify,
    observers: std::sync::Mutex<usize>,
    receiver: Mutex<Option<HostEventStream>>,
    invocations: Mutex<
        HashMap<forge_provider_api::InvocationId, Arc<super::host_invocation::HostInvocation>>,
    >,
    request: Mutex<Option<ProviderRequest>>,
    /// Context sections this binding's native conversation already sees.
    context_delivery: std::sync::Mutex<super::context_delivery::ContextDelivery>,
}

impl HostSession {
    /// Forget delivered context after a history change that may have removed it.
    fn clear_context(&self) {
        self.context_delivery
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clear();
    }
}

struct BoundSession {
    binding: SessionBinding,
    rejected_events: Option<Mutex<Option<ProviderEventStream>>>,
    registrations: forge_provider_api::TurnRegistrationReader,
    relay: Mutex<Option<tokio::task::JoinHandle<Result<(), ProviderError>>>>,
    relay_stop: tokio::sync::watch::Sender<Option<forge_provider_api::EventSequence>>,
    retiring: Arc<std::sync::atomic::AtomicBool>,
    retired: Arc<Mutex<bool>>,
}

/// Retain retirement and quarantine authority for exactly one native generation.
struct NativeBindingLifetime {
    binding: BindingId,
    retiring: Arc<std::sync::atomic::AtomicBool>,
    retired: Arc<Mutex<bool>>,
    collection: Arc<BindingCollection>,
}

/// Retain an admitted host query across a dropped caller while allowing close to revoke native work.
struct HostObservation {
    host: Arc<HostSession>,
}

impl Drop for HostObservation {
    /// Release only this query's collection obligation and wake retained closure.
    fn drop(&mut self) {
        let mut pending = self
            .host
            .observers
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        *pending = pending
            .checked_sub(1)
            .expect("each admitted observation releases exactly one lease");
        drop(pending);
        self.host.changed.notify_waiters();
    }
}

pub(super) struct EndpointLease {
    _server: ControlServer,
}

impl EndpointLease {
    /// Stop the exact invocation endpoint and collect all accepted HTTP calls.
    pub(super) async fn close(&self) -> Result<(), forge_provider_api::HostEffectFailure> {
        self._server
            .close()
            .await
            .map_err(|_| forge_provider_api::HostEffectFailure::Uncertain)
    }
}

struct PreparedOperation {
    binding: BindingId,
    request: Box<forge_provider_api::PromptRequest>,
    effects: Arc<super::host_invocation::HostInvocation>,
}

impl PreparedOperation {
    /// Admit the original journal attempt before handing any request to the native adapter.
    fn admit(
        self,
        submission: Arc<super::submission::Submission>,
    ) -> Result<NativeOperation, ProviderError> {
        let prepare = |error| ProviderError::PreparationFailed(Box::new(error));
        if self.request.submission_id != *submission.receipt_id() {
            return Err(prepare(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::WrongInvocation,
            )));
        }
        let flight = submission.begin_prompt().map_err(|error| {
            prepare(super::lifecycle_error::classify(
                ProviderOperation::TurnStart,
                error,
            ))
        })?;
        let operation = OperationControl::fresh(ProviderOperation::TurnStart).map_err(prepare)?;
        self.effects.request_native_input()?;
        Ok(NativeOperation {
            binding: self.binding,
            request: self.request,
            context: operation.context(),
            flight,
        })
    }
}

/// Own a successfully admitted journal flight and its immutable native operation context.
struct NativeOperation {
    binding: BindingId,
    request: Box<forge_provider_api::PromptRequest>,
    context: forge_provider_api::OperationContext,
    flight: super::submission::SubmissionFlight,
}

/// Distinguish unopened policy from a configuration applied to one retained binding generation.
#[derive(Clone, Debug)]
pub(crate) enum ConfigureReceipt {
    /// Native initialization will read the owner's committed policy later.
    Deferred,
    /// Native configuration and its ordered identity observation completed for this exact binding.
    Applied(ConfiguredBindingReceipt),
}

/// Prove ordered configuration without retaining native resources or extending a binding's lifetime.
#[derive(Clone, Debug)]
pub(crate) struct ConfiguredBindingReceipt {
    operation: forge_provider_api::OperationId,
    owner: ThreadOwner,
    binding: BindingId,
    before_history: Option<forge_provider_api::ProviderSessionId>,
    after_history: Option<forge_provider_api::ProviderSessionId>,
    watermark: forge_provider_api::EventSequence,
    host: Weak<HostSession>,
    provider_closed: Weak<std::sync::atomic::AtomicBool>,
}

impl ConfiguredBindingReceipt {
    /// Get owner-consumed native history before this configuration operation.
    pub(crate) fn before_history(&self) -> Option<&forge_provider_api::ProviderSessionId> {
        self.before_history.as_ref()
    }

    /// Get native history consumed through this operation's configuration watermark.
    pub(crate) fn after_history(&self) -> Option<&forge_provider_api::ProviderSessionId> {
        self.after_history.as_ref()
    }

    /// Get the latest consumed history of this live binding without granting configuration commit authority.
    pub(crate) fn observed_history(
        &self,
        owner: &ThreadOwner,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        let host = self.live_host(owner)?;
        let bound = host
            .initialized
            .get()
            .expect("validated binding initialization cannot be removed");
        let state = host.observation.lock().map_err(|_| {
            failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Runtime,
            )
        })?;
        if state.closed {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        Ok(state
            .identity
            .clone()
            .or_else(|| bound.binding.provider_session_id.clone()))
    }

    /// Reject changed owners, retired generations, newer configuration, or superseding native identity.
    pub(crate) fn validate(&self, owner: &ThreadOwner) -> Result<(), ProviderError> {
        let host = self.live_host(owner)?;
        let bound = host
            .initialized
            .get()
            .expect("validated binding initialization cannot be removed");
        let state = host.observation.lock().map_err(|_| {
            failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Runtime,
            )
        })?;
        if state.closed {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        if state.last_configuration.as_ref() != Some(&self.operation)
            || state.consumed < self.watermark
            || state
                .identity
                .as_ref()
                .or(bound.binding.provider_session_id.as_ref())
                != self.after_history.as_ref()
        {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::HistoryConflict,
            ));
        }
        Ok(())
    }

    /// Resolve only the original live owner and public binding generation through weak metadata.
    fn live_host(&self, owner: &ThreadOwner) -> Result<Arc<HostSession>, ProviderError> {
        if owner != &self.owner {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        let closed = || {
            failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            )
        };
        let provider_closed = self.provider_closed.upgrade().ok_or_else(closed)?;
        let host = self.host.upgrade().ok_or_else(closed)?;
        let bound = host.initialized.get().ok_or_else(closed)?;
        if provider_closed.load(std::sync::atomic::Ordering::Acquire)
            || host.closing.load(std::sync::atomic::Ordering::Acquire)
            || bound.retiring.load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(closed());
        }
        if bound.binding.id != self.binding {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::WrongInvocation,
            ));
        }
        Ok(host)
    }
}

/// Carry native sequence evidence separately from host-generated publications.
#[derive(Debug)]
pub(crate) struct ProviderObservation {
    pub(crate) event: super::session_execution::OwnerObservation,
    pub(crate) native: Option<(BindingId, forge_provider_api::EventSequence)>,
}

impl ProviderObservation {
    /// Wrap a host publication without assigning a native stream position.
    pub(crate) fn host(event: super::events::HarnessEvent) -> Self {
        Self {
            event: super::session_execution::OwnerObservation::Event(event),
            native: None,
        }
    }
}

#[derive(Default)]
struct ObservationState {
    consumed: forge_provider_api::EventSequence,
    /// Highest native position handed to the owner channel; it advances only together with the
    /// send, so a relay failure between validation and forwarding cannot publish a position
    /// that no consumer will ever see.
    published: forge_provider_api::EventSequence,
    forwarded: forge_provider_api::EventSequence,
    /// Highest native position the owner refused without consuming, such as an event that
    /// arrived after its execution was fenced. A refused position applies no metadata and ends
    /// every watermark wait through it as abandoned rather than leaving it unsatisfiable.
    rejected: forge_provider_api::EventSequence,
    closed: bool,
    capabilities: Option<forge_provider_api::CapabilityManifest>,
    identity: Option<forge_provider_api::ProviderSessionId>,
    last_configuration: Option<forge_provider_api::OperationId>,
    pending: std::collections::BTreeMap<forge_provider_api::EventSequence, BindingMetadata>,
    ownership: Option<Arc<std::sync::Mutex<super::ownership::OwnershipRegistry>>>,
}

enum BindingMetadata {
    Capabilities(forge_provider_api::CapabilityManifest),
    Identity(forge_provider_api::ProviderSessionId),
}

impl ResourceLease for EndpointLease {}

/// Keep host observation available before native initialization without inventing events.
pub(crate) struct HostEventStream {
    receiver: mpsc::UnboundedReceiver<Result<ProviderObservation, EventDeliveryError>>,
}

impl HostEventStream {
    /// Receive the next already-normalized event or its typed delivery failure.
    pub(crate) async fn recv(&mut self) -> Result<Option<ProviderObservation>, EventDeliveryError> {
        self.receiver.recv().await.transpose()
    }

    /// Drain one pending event without waiting for native work.
    pub(crate) fn try_recv(&mut self) -> Result<ProviderObservation, EventDeliveryError> {
        match self.receiver.try_recv() {
            Ok(event) => event,
            Err(mpsc::error::TryRecvError::Empty) => Err(EventDeliveryError::Empty),
            Err(mpsc::error::TryRecvError::Disconnected) => Err(EventDeliveryError::Closed),
        }
    }
}

impl ProviderBridge {
    /// Retain host policy independently of native construction and session lifetime.
    pub fn new(
        registration: forge_providers::registry::RegisteredProvider,
        permissions: Arc<PermissionCoordinator>,
        trace: Arc<crate::trace::TraceStore>,
        root: std::path::PathBuf,
    ) -> Self {
        let closed = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let binding_collection = Arc::new(BindingCollection {
            fence: closed.clone(),
            ..Default::default()
        });
        Self {
            native: registration.adapter().clone(),
            descriptor: registration.descriptor().clone(),
            test_control: registration.test_control().cloned(),
            utility: Default::default(),
            history_operations: Default::default(),
            permissions,
            trace,
            core_events: crate::exchange::core_log::CoreEventLog::new(root.clone()),
            root,
            sessions: Mutex::new(HashMap::new()),
            binding_collection,
            ownership: Default::default(),
            retired_bindings: Default::default(),
            admission: tokio::sync::RwLock::new(()),
            closed,
        }
    }

    /// Take the one host consumer before native setup or input dispatch begins.
    pub(crate) async fn session_events(
        &self,
        owner: ThreadOwner,
    ) -> Result<HostEventStream, ProviderError> {
        let _admission = self.admission.read().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.session(owner).await;
        let result = session.receiver.lock().await.take();
        result.ok_or_else(|| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::ThreadClose,
                ProviderFailureKind::Rejected,
            ))
        })
    }

    /// Initialize native history using admitted host resources and a normalized callback endpoint.
    async fn binding_for(
        &self,
        request: &ProviderRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let _admission = self.admission.read().await;
        self.binding_under_admission(request).await
    }

    /// Initialize while the caller retains admission against closure and shutdown.
    async fn binding_under_admission(
        &self,
        request: &ProviderRequest,
    ) -> Result<SessionBinding, ProviderError> {
        self.ensure_admission(ProviderOperation::ThreadOpen)?;
        let owner = request.owner().map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Rejected,
            ))
        })?;
        let session = self.session(owner.clone()).await;
        if session.closing.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let initialized = session
            .initialized
            .get_or_try_init(|| async {
                if self.closed.load(std::sync::atomic::Ordering::Acquire)
                    || session.closing.load(std::sync::atomic::Ordering::Acquire)
                {
                    session.changed.notify_waiters();
                    session.open_operation.cancel();
                }
                let descriptor = self.descriptor();
                let catalog = super::ProviderCatalogRequest {
                    execution_owner: Some(owner.clone()),
                    harness_session_id: request.harness_session_id.clone(),
                    workspace: request.workspace.clone(),
                    access: request.access.clone(),
                    approval_mode: request.approval_mode,
                    provider_session_id: request.provider_session_id.clone(),
                };
                // Background generation is tool-free, so it selects no skills or MCP servers; an
                // empty selection excludes inherited resources rather than restoring defaults.
                let resources = match descriptor.capability.integration.resource_target.as_deref() {
                    Some(_) if request.mode == super::PromptMode::Background => Default::default(),
                    Some(target) => crate::settings::SessionResources::load(
                        &self.root,
                        &catalog,
                        descriptor.kind.as_str(),
                        target,
                    )
                    .map_err(|_| {
                        ProviderError::Failure(ProviderFailure::new(
                            ProviderOperation::ThreadOpen,
                            ProviderFailureKind::Rejected,
                        ))
                    })?
                    .selection(),
                    None => Default::default(),
                };
                let tools = crate::tools::ControlToolRegistry
                    .registration_definitions(&owner, request.mode);
                // Adapters retain open-time instructions for every later turn, so they hold only
                // static text. Each turn's purpose, task contract, and settings travel as tracked
                // context sections that the binding's delivery record sends once per change.
                let system_instructions =
                    request.open_instructions(descriptor.capability.integration.context_channel);
                let opening_request = SessionRequest {
                    thread_id: (owner.clone()).thread_id(),
                    mode: if request.mode == super::PromptMode::Background {
                        SessionMode::Background
                    } else if owner.is_root() {
                        SessionMode::Interactive
                    } else {
                        SessionMode::Child
                    },
                    workspace: request.workspace.clone(),
                    history: forge_provider_api::SessionHistory::Durable {
                        resume: request.provider_session_id.clone(),
                    },
                    configuration: forge_provider_api::SessionConfiguration {
                        model: model_selection(request),
                        access: request.access.clone(),
                        approval: request.approval_mode,
                        resources,
                    },
                    resource_lease: None,
                    tools,
                    tool_endpoint: None,
                    system_instructions,
                    trace: self.trace.clone(),
                };
                let opened = session
                    .open_operation
                    .open(
                        self.native.as_ref(),
                        self.trace.clone(),
                        &self.core_events,
                        opening_request,
                        self.binding_collection.clone(),
                    )
                    .await?;
                Ok::<_, ProviderError>(self.install_session(&owner, &session, opened))
            })
            .await?;
        if self.binding_collection.ambiguous(&initialized.binding.id) {
            return Err(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ));
        }
        if initialized.binding.thread_id != owner.thread_id() {
            initialized
                .retiring
                .store(true, std::sync::atomic::Ordering::Release);
            return Err(ProviderFailure::new(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::WrongInvocation,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Invalidated,
            )
            .into());
        }
        if session.sender.is_closed() {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::Transport,
            ));
        }
        if initialized
            .retiring
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(failure(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::BindingClosed,
            ));
        }
        Ok(initialized.binding.clone())
    }

    /// Install the sole stream and binding from either ordinary opening or late opening collection.
    fn install_session(
        &self,
        owner: &ThreadOwner,
        session: &Arc<HostSession>,
        opened: forge_provider_api::OpenedSession,
    ) -> BoundSession {
        let forge_provider_api::OpenedSession {
            binding,
            capabilities,
            events,
        } = opened;
        let registrations = events.registrations();
        let (relay_stop, stopping) = tokio::sync::watch::channel(None);
        let ownership = Arc::new(std::sync::Mutex::new(
            super::ownership::OwnershipRegistry::new(
                binding.id.clone(),
                owner.clone(),
                self.core_events.operations(),
            ),
        ));
        let admitted = {
            let mut registrations = self
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            match registrations.entry(binding.id.clone()) {
                std::collections::hash_map::Entry::Vacant(entry)
                    if !self.binding_collection.ambiguous(&binding.id) =>
                {
                    entry.insert(ownership.clone());
                    true
                }
                std::collections::hash_map::Entry::Vacant(_) => false,
                std::collections::hash_map::Entry::Occupied(_) => false,
            }
        };
        if !admitted {
            self.binding_collection.reject(&binding.id);
            let _ = session.sender.send(Err(EventDeliveryError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            )));
            return BoundSession {
                binding,
                registrations,
                rejected_events: Some(Mutex::new(Some(events))),
                relay: Mutex::new(None),
                relay_stop,
                retiring: Arc::new(std::sync::atomic::AtomicBool::new(true)),
                retired: Arc::new(Mutex::new(false)),
            };
        }
        {
            let mut observed = session
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            observed.capabilities = Some(capabilities.clone());
            observed.identity = binding.provider_session_id.clone();
            observed.ownership = Some(ownership);
        }
        // Keep the binding even if its receiver closed; the relay reports that failure.
        let _ = session
            .sender
            .send(Ok(ProviderObservation::host(super::HarnessEvent::Host {
                scope: forge_provider_api::EventScope::Binding {
                    thread_id: forge_provider_api::ThreadId::from(owner.clone()),
                },
                event: super::HostEvent::CapabilitiesNegotiated {
                    binding_id: binding.id.clone(),
                    manifest: capabilities,
                    history: binding.provider_session_id.clone(),
                },
            })));
        let registrations = events.registrations();
        let retiring = Arc::new(std::sync::atomic::AtomicBool::new(
            session.closing.load(std::sync::atomic::Ordering::Acquire),
        ));
        let retired = Arc::new(Mutex::new(false));
        let lifetime = NativeBindingLifetime {
            binding: binding.id.clone(),
            retiring: retiring.clone(),
            retired: retired.clone(),
            collection: self.binding_collection.clone(),
        };
        let relay = tokio::spawn(relay_events(
            self.native.clone(),
            owner.clone(),
            events,
            session.clone(),
            lifetime,
            stopping,
        ));
        BoundSession {
            binding,
            rejected_events: None,
            registrations,
            relay: Mutex::new(Some(relay)),
            relay_stop,
            retiring,
            retired,
        }
    }

    /// Submit the host's admitted input through the normalized provider interface.
    pub(crate) async fn turn_start(
        &self,
        request: ProviderRequest,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let _dispatch_observation = request.dispatch.observe_worker();
        let _admission = self.admission.read().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Rejected,
            ))));
        }
        let owner = request.owner().map_err(|_| {
            ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Rejected,
            )))
        })?;
        let host = self.session(owner).await;
        let _preparation = host.preparation.lock().await;
        let submission = request.submission.clone();
        let prepared = self
            .prepare_dispatch(request, scope)
            .await
            .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
        let submission = submission.ok_or_else(|| {
            ProviderError::PreparationFailed(Box::new(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Rejected,
            )))
        })?;
        let NativeOperation {
            binding,
            request,
            context,
            flight: _flight,
        } = prepared.admit(submission)?;
        let expected = request.submission_id.clone();
        let delivered_context = request.context.clone();
        drop(_preparation);
        drop(_admission);
        let native_scope = request.scope.clone();
        let result = super::operation::observe(
            self.trace.clone(),
            &self.core_events,
            ProviderOperation::TurnStart,
            &context,
            native_scope.clone().into(),
            Some(binding.clone()),
            self.native.turn_start(context.clone(), &binding, *request),
        )
        .await
        .map_err(|error| input_rejection(ProviderOperation::TurnStart, error));
        let accepted = match result {
            Ok(accepted) => accepted,
            Err(primary @ (ProviderError::NotConsumed | ProviderError::PreparationFailed(_))) => {
                drop(_flight);
                // Negative delivery evidence does not prove an allocated native scope has collected.
                return match self
                    .collect_unconsumed_invocation(
                        &native_scope,
                        OperationControl::cancellation_collection()?,
                    )
                    .await
                {
                    Ok(()) => Err(primary),
                    Err(cleanup) => Err(ProviderError::Combined {
                        primary: Box::new(primary),
                        secondary: vec![cleanup],
                    }),
                };
            }
            Err(error) => return Err(error),
        };
        if accepted.submission_id != expected {
            return self.invalid_acceptance(&binding).await;
        }
        // Only an accepted submission proves the conversation received its context sections.
        host.context_delivery
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .commit(&delivered_context);
        Ok(())
    }

    /// Complete all host preparation before a native operation can consume input.
    async fn prepare_dispatch(
        &self,
        request: ProviderRequest,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<PreparedOperation, ProviderError> {
        let owner = request.owner().map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Rejected,
            ))
        })?;
        if owner != scope.owner() {
            return Err(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch.into());
        }
        let submission = request
            .submission
            .as_ref()
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))?;
        let invocation = request.invocation_snapshot();
        submission
            .persist(move |receipt| receipt.seal_request(invocation).map_err(Into::into))
            .await
            .map_err(super::submission::journal_failure)?;
        let prepared_attachments =
            prepare_attachments(request.input.attachments().to_vec()).await?;
        let input = neutralized_input(request.input.normalized());
        let included_inputs = submission.included_inputs()?;
        if included_inputs.len() != request.recovered_input.len() {
            return Err(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ));
        }
        let mut recovered_inputs = Vec::with_capacity(request.recovered_input.len());
        for (recovered, covered_input) in request.recovered_input.iter().zip(included_inputs) {
            recovered_inputs.push(forge_provider_api::RecoveredInput {
                covered_input,
                input: neutralized_input(recovered.normalized()),
                attachments: prepare_attachments(recovered.attachments().to_vec()).await?,
            });
        }
        let binding = self.binding_under_admission(&request).await?;
        let host = self.session(owner.clone()).await;
        host.initialized.get().ok_or_else(|| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::ThreadOpen,
                ProviderFailureKind::SessionMissing,
            ))
        })?;

        let previous = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        if let Some(previous) = previous {
            if previous.foreground_pending() {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Rejected,
                ));
            }
            // A settled attempt cannot authorize callbacks against its replacement's policy.
            previous.cancel();
        }
        host.observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .ownership
            .as_mut()
            .ok_or_else(|| {
                ProviderError::Observation(
                    forge_provider_api::ObservationFailure::OwnershipConflict,
                )
            })?
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .prepare(scope.clone())?;
        let (sink, events) = super::events::channel(scope.clone());
        let skills = if self
            .descriptor()
            .capability
            .integration
            .resource_target
            .is_some()
        {
            Some(
                self.permissions
                    .skills(Some(&self.root), &request, &self.descriptor)
                    .map_err(|_| {
                        failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
                    })?,
            )
        } else {
            None
        };
        let mut context = request
            .control_context
            .clone()
            .unwrap_or_else(|| crate::tools::ControlTurnContext::inactive(request.mode));
        context.question_invocation = Some(scope.clone());
        context.bind_task_authority_progress();
        let directories = self
            .permissions
            .directories(Some(&self.root), &request, &scope)
            .map_err(|error| {
                super::lifecycle_error::classify(ProviderOperation::TurnStart, error)
            })?;
        if let Some(lsp) = &context.lsp {
            let protected_path = self
                .permissions
                .store()
                .read()
                .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))?
                .open()
                .0
                .into();
            lsp.bind_mutation_policy(directories.policy(), protected_path)
                .map_err(|error| {
                    super::lifecycle_error::classify(ProviderOperation::TurnStart, error)
                })?;
        }
        let tools = crate::tools::ControlToolRegistry
            .definition_list_for_context(Some(&context), scope.owner().is_root());
        let turn_context = host
            .context_delivery
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .pending(request.standing_sections());
        let permissions = Arc::new(
            crate::permissions::provider::ProviderPermissionContext::new(self.permissions.clone()),
        );
        permissions
            .bind_provider_fence(self.closed.clone())
            .map_err(|error| {
                super::lifecycle_error::classify(ProviderOperation::TurnStart, error)
            })?;
        permissions
            .configure(
                owner,
                request.approval_mode,
                request.workspace.clone(),
                Some(sink.clone()),
            )
            .map_err(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Rejected,
                ))
            })?;
        // Background generation returns text only, so every native tool request is rejected.
        if request.mode == super::PromptMode::Background {
            permissions.deny_all_tools().map_err(|_| {
                failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected)
            })?;
        }
        let authority = Arc::new(ObservedPermissionAuthority::new(
            binding.id.clone(),
            scope.clone(),
            Arc::downgrade(&permissions),
            Arc::downgrade(&host),
            sink.clone(),
        ));
        host.observed_authorities
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(scope.invocation_id.clone(), authority.clone());
        permissions
            .bind_observed_authority(authority.clone())
            .map_err(|error| {
                super::lifecycle_error::classify(ProviderOperation::TurnStart, error)
            })?;
        permissions.bind_context(Some(&context)).map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Rejected,
            ))
        })?;
        context.file_permissions = Some(permissions.clone());
        let endpoint = Arc::new(ControlSession::new(tools.clone()));
        endpoint.begin(context.clone(), sink).await;
        let server = ControlServer::start(endpoint.clone())
            .await
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?;
        let tool_endpoint = HostToolEndpoint {
            url: server.url.clone(),
            headers: Default::default(),
        };
        let lease = Arc::new(EndpointLease { _server: server });
        let relay = super::host_invocation::HostEventRelay::start(
            binding.id.clone(),
            events,
            host.sender.clone(),
        );
        let effects = Arc::new(super::host_invocation::HostInvocation::new(
            binding.id.clone(),
            scope.clone(),
            &request,
            context.clone(),
            &tools,
            super::host_invocation::HostInvocationResources {
                observed_authority: authority,
                provider_closed: self.closed.clone(),
                permissions: permissions.clone(),
                directories: Some(directories),
                endpoint: endpoint.clone(),
                endpoint_lease: lease.clone(),
                relay,
                skills,
            },
        ));
        let delegated: Arc<dyn forge_provider_api::control::ProviderTools> = effects.clone();
        endpoint.bind_host(scope.clone(), Arc::downgrade(&delegated));
        *host.request.lock().await = Some(request.clone());
        host.invocations
            .lock()
            .await
            .insert(scope.invocation_id.clone(), effects.clone());
        let configuration = forge_provider_api::InvocationConfiguration {
            model: model_selection(&request),
            access: request.access.clone(),
            approval: request.approval_mode,
        };
        let receipt = request
            .submission
            .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))?;
        let native_receipt = receipt.receipt_for_attempt()?;
        let submission_id = native_receipt.id().clone();
        let attempt_id = native_receipt.attempt_id()?;
        let dispatch =
            receipt.prepare_dispatch(&request.dispatch, binding.id.clone(), scope.clone())?;
        Ok(PreparedOperation {
            binding: binding.id,
            effects: effects.clone(),
            request: Box::new(forge_provider_api::PromptRequest {
                configuration,
                tools,
                tool_endpoint: Some(tool_endpoint),
                resource_lease: Some(lease),
                thread_id: (scope).owner().thread_id(),
                turn_id: (scope)
                    .turn_id()
                    .expect("admitted scope has a nonempty host turn identity"),

                scope,
                execution: request.mode.execution(),
                attempt_id,
                submission_id,
                dispatch,
                input: {
                    let mut input = input;
                    crate::exchange::TaskStart::announce(context.task_start.as_ref(), &mut input);
                    input
                },
                recovered_inputs,
                context: turn_context,
                attachments: prepared_attachments,
                receipt: native_receipt,
                permissions,
                provider_tools: effects.clone(),
                turn_control: effects.clone(),
                host_effects: effects,
            }),
        })
    }

    /// Describe the selected implementation using only registry-owned normalized metadata.
    pub fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        self.descriptor.clone()
    }

    /// Adapt deterministic provider controls without exposing any native protocol.
    pub(crate) fn mock_response_control(
        &self,
    ) -> Option<super::mock_response::MockResponseControl> {
        self.test_control
            .clone()
            .map(super::mock_response::MockResponseControl::new)
    }

    /// Deliver independently journaled input to its original owner and invocation.
    pub(crate) async fn turn_steer(
        &self,
        owner: &ThreadOwner,
        scope: forge_provider_api::InvocationScope,
        input: super::SteeringInput,
    ) -> Result<(), ProviderError> {
        let _admission = self.admission.read().await;
        let (binding, request) =
            self.prepare_steering(owner, scope, input)
                .await
                .map_err(|error| {
                    ProviderError::PreparationFailed(Box::new(
                        error.for_operation(ProviderOperation::TurnSteer),
                    ))
                })?;
        let expected = request.delivery.submission_id().cloned();
        let operation = OperationControl::fresh(ProviderOperation::TurnSteer)?;
        let context = operation.context();
        let accepted = super::operation::observe(
            self.trace.clone(),
            &self.core_events,
            ProviderOperation::TurnSteer,
            &context,
            request.scope.clone().into(),
            Some(binding.clone()),
            self.native.turn_steer(context.clone(), &binding, request),
        )
        .await
        .map_err(|error| input_rejection(ProviderOperation::TurnSteer, error))?;
        let actual = match accepted {
            forge_provider_api::SteeringAcceptance::Journaled { submission_id } => {
                Some(submission_id)
            }
            forge_provider_api::SteeringAcceptance::Context => None,
        };
        if actual != expected {
            return self.invalid_acceptance(&binding).await;
        }
        Ok(())
    }

    /// Prepare steering without changing the original invocation or inventing receipt identities.
    async fn prepare_steering(
        &self,
        owner: &ThreadOwner,
        scope: forge_provider_api::InvocationScope,
        input: super::SteeringInput,
    ) -> Result<(BindingId, forge_provider_api::SteeringRequest), ProviderError> {
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::Rejected,
            ));
        }
        if owner != &scope.owner() {
            return Err(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch.into());
        }
        let session = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnSteer,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let native = session.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::TurnSteer,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        let invocation = session
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::TurnSteer, ProviderFailureKind::Rejected))?;
        let request = invocation.request_snapshot(&scope)?;
        let (delivery, normalized, attachments) = match input.submission() {
            Some(receipt) => {
                receipt
                    .persist(move |receipt| receipt.seal_request(request).map_err(Into::into))
                    .await
                    .map_err(super::submission::journal_failure)?;
                let record = receipt.record().map_err(|_| {
                    failure(
                        ProviderOperation::TurnSteer,
                        ProviderFailureKind::Persistence,
                    )
                })?;
                let normalized = record.request.input.normalized();
                let attachments =
                    prepare_attachments(record.request.input.attachments().to_vec()).await?;
                (
                    forge_provider_api::SteeringDelivery::Journaled {
                        receipt: receipt.receipt_for_attempt()?,
                    },
                    normalized,
                    attachments,
                )
            }
            None => match input.agent_result() {
                Some(result) => (
                    forge_provider_api::SteeringDelivery::Context,
                    forge_provider_api::forge::TurnInput::child_continuation(vec![result.clone()])
                        .expect("one child result is not empty"),
                    Vec::new(),
                ),
                None => {
                    return Err(failure(
                        ProviderOperation::TurnSteer,
                        ProviderFailureKind::Rejected,
                    ));
                }
            },
        };
        Ok((
            native.binding.id.clone(),
            forge_provider_api::SteeringRequest {
                scope,
                delivery,
                input: {
                    let mut normalized = neutralized_input(normalized);
                    crate::exchange::TaskStart::announce(input.task_start(), &mut normalized);
                    normalized
                },
                attachments,
            },
        ))
    }

    /// Fence a provider that acknowledges a different normalized input than was admitted.
    async fn invalid_acceptance(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let primary = ProviderError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
        );
        let context = OperationControl::fresh(ProviderOperation::ThreadClose)?.context();
        self.register_binding_operation(&context, binding).await?;
        match self.native.thread_close(context, binding).await {
            Ok(()) => Err(primary),
            Err(cleanup) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![cleanup],
            }),
        }
    }

    /// Open or resume native history without creating an exchange.
    pub async fn thread_open(
        &self,
        request: ProviderRequest,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
        Ok(self.binding_for(&request).await?.provider_session_id)
    }

    /// Read negotiated owner state, using registry baseline before a binding exists.
    pub async fn capabilities(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::CapabilityManifest, ProviderError> {
        let owner = request
            .owner()
            .map_err(|_| failure(ProviderOperation::Inspect, ProviderFailureKind::Rejected))?;
        let session = self.sessions.lock().await.get(&owner).cloned();
        Ok(session
            .and_then(|host| {
                host.observation
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .capabilities
                    .clone()
            })
            .unwrap_or_else(|| {
                forge_provider_api::CapabilityManifest::from_descriptor(self.descriptor())
            }))
    }

    /// Select discovery context without opening or reconfiguring a session.
    pub async fn query_context(
        &self,
        request: &super::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::QueryContext, ProviderError> {
        self.ensure_admission(ProviderOperation::Inspect)?;
        let owner = request
            .owner()
            .map_err(|_| failure(ProviderOperation::Inspect, ProviderFailureKind::Rejected))?;
        let sessions = self.sessions.lock().await;
        Ok(
            match sessions.get(&owner).and_then(|host| host.initialized.get()) {
                Some(bound) if !bound.retiring.load(std::sync::atomic::Ordering::Acquire) => {
                    forge_provider_api::QueryContext::Binding(bound.binding.id.clone())
                }
                Some(_) => {
                    return Err(failure(
                        ProviderOperation::Inspect,
                        ProviderFailureKind::BindingClosed,
                    ));
                }
                None => forge_provider_api::QueryContext::Workspace(
                    forge_provider_api::WorkspaceContext {
                        workspace: request.workspace.clone(),
                    },
                ),
            },
        )
    }

    /// Apply host-selected resources through the normalized configuration boundary.
    pub(crate) async fn thread_configure(
        &self,
        desired: ProviderRequest,
        selection: &crate::settings::PortableSettings,
        deadline: Option<tokio::time::Instant>,
    ) -> Result<ConfigureReceipt, ProviderError> {
        self.ensure_admission(ProviderOperation::ThreadConfigure)?;
        let request = catalog_request(&desired);
        let _admission = self.admission.write().await;
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Rejected,
            ));
        }
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::Rejected,
            )
        })?;
        let host = self.sessions.lock().await.get(&owner).cloned();
        let Some(host) = host else {
            return Ok(ConfigureReceipt::Deferred);
        };
        if host.closing.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        // Initialization reads committed settings; no native configuration exists to mutate yet.
        let Some(bound) = host.initialized.get() else {
            return Ok(ConfigureReceipt::Deferred);
        };
        if bound.retiring.load(std::sync::atomic::Ordering::Acquire) {
            return Err(failure(
                ProviderOperation::ThreadConfigure,
                ProviderFailureKind::BindingClosed,
            ));
        }
        let operation = OperationControl::fresh(ProviderOperation::ThreadConfigure)?;
        let context = match deadline {
            Some(deadline) => operation.context().bounded_by(deadline),
            None => operation.context(),
        };
        context.check_admission(ProviderOperation::ThreadConfigure)?;
        let before_history = {
            let mut state = host.observation.lock().map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Runtime,
                )
            })?;
            if state.closed {
                return Err(failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::BindingClosed,
                ));
            }
            state.last_configuration = Some(context.id().clone());
            state
                .identity
                .clone()
                .or_else(|| bound.binding.provider_session_id.clone())
        };
        let descriptor = self.descriptor();
        let resources = match descriptor.capability.integration.resource_target.as_deref() {
            Some(target) => crate::settings::SessionResources::load_with_selection(
                &self.root,
                &request,
                descriptor.kind.as_str(),
                target,
                Some(selection),
            )
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Rejected,
                )
            })?
            .selection(),
            None => Default::default(),
        };
        let configuration = forge_provider_api::SessionConfiguration {
            model: model_selection(&desired),
            access: desired.access.clone(),
            approval: request.approval_mode,
            resources,
        };
        let configured = super::operation::observe(
            self.trace.clone(),
            &self.core_events,
            ProviderOperation::ThreadConfigure,
            &context,
            forge_provider_api::EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            },
            Some(bound.binding.id.clone()),
            self.native
                .thread_configure(context.clone(), &bound.binding.id, configuration),
        )
        .await;
        let configured = match configured {
            Ok(configured) => configured,
            Err(error) => {
                if super::operation::primary_failure(&error).is_some_and(|failure| {
                    failure.kind() == ProviderFailureKind::Persistence
                        && failure.disposition()
                            == forge_provider_api::FailureDisposition::Invalidated
                }) {
                    host.closing
                        .store(true, std::sync::atomic::Ordering::Release);
                    bound
                        .retiring
                        .store(true, std::sync::atomic::Ordering::Release);
                    for invocation in host.invocations.lock().await.values() {
                        invocation.cancel();
                    }
                }
                return Err(error);
            }
        };
        self.wait_watermark(&owner, configured.watermark).await?;
        let after_history = {
            let mut state = host
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if state.capabilities.as_ref().is_none_or(|current| {
                current.effective_revision <= configured.capabilities.effective_revision
            }) {
                state.capabilities = Some(configured.capabilities.clone());
            }
            state
                .identity
                .clone()
                .or_else(|| bound.binding.provider_session_id.clone())
        };
        host.sender
            .send(Ok(ProviderObservation::host(super::HarnessEvent::Host {
                scope: forge_provider_api::EventScope::Binding {
                    thread_id: forge_provider_api::ThreadId::from(owner.clone()),
                },
                event: super::HostEvent::CapabilitiesNegotiated {
                    binding_id: bound.binding.id.clone(),
                    manifest: configured.capabilities.clone(),
                    history: None,
                },
            })))
            .map_err(|_| {
                failure(
                    ProviderOperation::ThreadConfigure,
                    ProviderFailureKind::Transport,
                )
            })?;
        *host.request.lock().await = Some(desired);
        let receipt = ConfiguredBindingReceipt {
            operation: context.id().clone(),
            owner: owner.clone(),
            binding: bound.binding.id.clone(),
            before_history,
            after_history,
            watermark: configured.watermark,
            host: Arc::downgrade(&host),
            provider_closed: Arc::downgrade(&self.closed),
        };
        receipt.validate(&owner)?;
        Ok(ConfigureReceipt::Applied(receipt))
    }

    /// Read model choices through a read-only workspace or exact-binding context.
    pub async fn model_list(
        &self,
        context: forge_provider_api::QueryContext,
    ) -> Result<Vec<super::ProviderModel>, ProviderError> {
        self.model_list_before(context, None).await
    }

    /// Preserve an enclosing configuration deadline while ordinary discovery receives its own budget.
    pub(crate) async fn model_list_before(
        &self,
        query: forge_provider_api::QueryContext,
        deadline: Option<tokio::time::Instant>,
    ) -> Result<Vec<super::ProviderModel>, ProviderError> {
        self.ensure_admission(ProviderOperation::ModelList)?;
        let operation = OperationControl::fresh(ProviderOperation::ModelList)?;
        let context = match deadline {
            Some(deadline) => operation.context().bounded_by(deadline),
            None => operation.context(),
        };
        context.check_admission(ProviderOperation::ModelList)?;
        self.register_query_operation(&context, &query).await?;
        let span = tracing::info_span!(
            "provider.catalog",
            operation_id = context.id().as_str(),
            operation = ?ProviderOperation::ModelList,
        );
        tracing::Instrument::instrument(self.native.model_list(context, query), span).await
    }

    /// Read skill choices without changing enablement.
    pub async fn skill_list(
        &self,
        context: forge_provider_api::QueryContext,
    ) -> Result<Vec<super::SkillDefinition>, ProviderError> {
        self.ensure_admission(ProviderOperation::SkillList)?;
        let operation = OperationControl::fresh(ProviderOperation::SkillList)?.context();
        self.register_query_operation(&operation, &context).await?;
        let span = tracing::info_span!(
            "provider.catalog",
            operation_id = operation.id().as_str(),
            operation = ?ProviderOperation::SkillList,
        );
        tracing::Instrument::instrument(self.native.skill_list(operation, context), span).await
    }

    /// Read configured definitions or observed readiness without hidden configuration writes.
    pub async fn mcp_list(
        &self,
        query: forge_provider_api::McpQuery,
    ) -> Result<Vec<super::McpDefinition>, ProviderError> {
        self.ensure_admission(ProviderOperation::McpList)?;
        let context = OperationControl::fresh(ProviderOperation::McpList)?.context();
        match &query {
            forge_provider_api::McpQuery::Configured(query)
            | forge_provider_api::McpQuery::Observed(query) => {
                self.register_query_operation(&context, query).await?
            }
        }
        let span = tracing::info_span!(
            "provider.catalog",
            operation_id = context.id().as_str(),
            operation = ?ProviderOperation::McpList,
        );
        tracing::Instrument::instrument(self.native.mcp_list(context, query), span).await
    }

    /// Register an operation against the exact retained generation before native request dispatch.
    async fn register_binding_operation(
        &self,
        context: &forge_provider_api::OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError> {
        let sessions = self.sessions.lock().await;
        let owner = sessions
            .iter()
            .find_map(|(owner, host)| {
                host.initialized
                    .get()
                    .filter(|bound| &bound.binding.id == binding)
                    .map(|_| owner.clone())
            })
            .ok_or_else(|| {
                ProviderError::Observation(
                    forge_provider_api::ObservationFailure::OwnershipConflict,
                )
            })?;
        self.core_events.operations().register(
            context.id().clone(),
            forge_provider_api::EventScope::Binding {
                thread_id: owner.thread_id(),
            },
            Some(binding.clone()),
        )
    }

    /// Register discovery only when its explicit query targets an existing binding.
    async fn register_query_operation(
        &self,
        context: &forge_provider_api::OperationContext,
        query: &forge_provider_api::QueryContext,
    ) -> Result<(), ProviderError> {
        match query {
            forge_provider_api::QueryContext::Binding(binding) => {
                self.register_binding_operation(context, binding).await
            }
            forge_provider_api::QueryContext::Workspace(_) => Ok(()),
        }
    }

    /// Retain a read operation without delaying immediate native admission revocation.
    async fn observe_binding(
        &self,
        operation: ProviderOperation,
        owner: &ThreadOwner,
    ) -> Result<(BindingId, HostObservation), ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))?;
        let bound = host
            .initialized
            .get()
            .ok_or_else(|| failure(operation, ProviderFailureKind::SessionMissing))?;
        let binding = bound.binding.id.clone();
        {
            let mut pending = host
                .observers
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if host.closing.load(std::sync::atomic::Ordering::Acquire)
                || bound.retiring.load(std::sync::atomic::Ordering::Acquire)
            {
                return Err(failure(operation, ProviderFailureKind::BindingClosed));
            }
            *pending = pending
                .checked_add(1)
                .ok_or_else(|| failure(operation, ProviderFailureKind::Runtime))?;
        }
        Ok((binding, HostObservation { host }))
    }

    /// Read terminals from an existing exact binding without opening a missing session.
    pub async fn terminal_list(
        &self,
        request: super::ProviderCatalogRequest,
    ) -> Result<super::terminal::TerminalInventory, ProviderError> {
        self.ensure_admission(ProviderOperation::TerminalList)?;
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::TerminalList,
                ProviderFailureKind::Rejected,
            )
        })?;
        // A registered owner opens its native conversation on first input, so an
        // unopened owner has no provider-owned shells to report.
        let unopened = self
            .sessions
            .lock()
            .await
            .get(&owner)
            .is_some_and(|host| host.initialized.get().is_none());
        if unopened {
            return Ok(super::terminal::TerminalInventory::observed(Vec::new()));
        }
        let (binding, permit) = self
            .observe_binding(ProviderOperation::TerminalList, &owner)
            .await?;
        let context = OperationControl::fresh(ProviderOperation::TerminalList)?.context();
        self.register_binding_operation(&context, &binding).await?;
        let native = self.native.clone();
        tokio::spawn(async move {
            let _permit = permit;
            native.terminal_list(context, &binding).await
        })
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::TerminalList,
                ProviderFailureKind::Runtime,
            )
        })?
    }

    /// Stop an opaque terminal only in the explicitly selected live binding.
    pub async fn terminal_terminate(
        &self,
        request: super::ProviderCatalogRequest,
        terminal: &forge_provider_api::terminal::TerminalId,
    ) -> Result<(), ProviderError> {
        self.ensure_admission(ProviderOperation::TerminalTerminate)?;
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Rejected,
            )
        })?;
        let (binding, permit) = self
            .observe_binding(ProviderOperation::TerminalTerminate, &owner)
            .await?;
        let context = OperationControl::fresh(ProviderOperation::TerminalTerminate)?.context();
        self.register_binding_operation(&context, &binding).await?;
        let native = self.native.clone();
        let terminal = terminal.clone();
        tokio::spawn(async move {
            let _permit = permit;
            native
                .terminal_terminate(context, &binding, &terminal)
                .await
        })
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::TerminalTerminate,
                ProviderFailureKind::Runtime,
            )
        })?
    }

    /// Fork native history using the first complete request retained for this attempt.
    pub async fn thread_fork(
        &self,
        request: super::ProviderForkRequest,
    ) -> Result<super::ProviderForkResult, ProviderError> {
        let _admission = self.admission.read().await;
        self.ensure_admission(ProviderOperation::ThreadFork)?;
        let id = forge_provider_api::OperationId::new(format!(
            "fork/{}/{}",
            request.target_harness_session_id, request.attempt_id
        ))?;
        self.history_operations
            .fork(
                self.native.as_ref(),
                self.trace.clone(),
                &self.core_events,
                id,
                self.prepare_fork_request(request),
            )
            .await
    }

    /// Run bounded isolated generation through the shared retained utility lifecycle.
    pub async fn generate_text(
        &self,
        request: super::ProviderCatalogRequest,
        purpose: super::TextGeneration,
        selection: forge_provider_api::ModelSelection,
        history: &str,
    ) -> Result<String, ProviderError> {
        self.ensure_admission(ProviderOperation::ThreadOpen)?;
        // Check workflow support before allocating temporary history.
        let manifest = self.capabilities(request.clone()).await?;
        if !manifest.supports(forge_provider_api::capability::CapabilityId::IsolatedGeneration) {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::TextGeneration,
            )
            .into());
        }
        let tier_supported = match selection.service_tier {
            forge_provider_api::ServiceTier::Standard => true,
            forge_provider_api::ServiceTier::Fast => {
                manifest.supports(forge_provider_api::capability::CapabilityId::FastMode)
            }
            forge_provider_api::ServiceTier::Ultrafast => {
                manifest.supports(forge_provider_api::capability::CapabilityId::UltrafastMode)
            }
        };
        if !tier_supported {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ServiceTier,
            )
            .into());
        }
        self.utility
            .generate(
                self.native.clone(),
                self.trace.clone(),
                self.descriptor.capability.resolve_access(
                    forge_provider_api::access::AccessPolicy {
                        write_access: forge_provider_api::access::WriteScope::Off,
                        ..Default::default()
                    },
                ),
                forge_provider_api::TextGenerationRequest {
                    workspace: request.workspace,
                    model: selection.model.unwrap_or_else(|| "default".to_owned()),
                    reasoning: selection.reasoning,
                    service_tier: selection.service_tier,
                    instructions: purpose.instructions().to_owned(),
                    input: purpose.input(history),
                    validator: Arc::new(purpose),
                },
            )
            .await
    }

    /// Read durable identity already published by open or an ordered binding observation.
    pub async fn retained_session_id(
        &self,
        owner: &ThreadOwner,
    ) -> Option<forge_provider_api::ProviderSessionId> {
        self.sessions.lock().await.get(owner).and_then(|host| {
            host.observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .identity
                .clone()
        })
    }

    /// Retract the exact checked cancelled submission without selecting current history.
    pub async fn input_retract(
        &self,
        context: forge_provider_api::OperationContext,
        owner: &ThreadOwner,
        request: forge_provider_api::InputRetractRequest,
    ) -> Result<forge_provider_api::InputRetractOutcome, ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputRetract,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let bound = host.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::InputRetract,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        // Retracting a turn can also remove the context delivered with it.
        host.clear_context();
        super::operation::observe(
            self.trace.clone(),
            &self.core_events,
            ProviderOperation::InputRetract,
            &context,
            request.scope.clone().into(),
            Some(bound.binding.id.clone()),
            self.native
                .input_retract(context.clone(), &bound.binding.id, request),
        )
        .await
    }

    /// Rewind the owner's native conversation so it ends at `after`, opening or resuming its binding.
    ///
    /// Each call is a fresh operation: the native rewind is idempotent, so a repeated rollback after
    /// an uncertain failure sends the same request rather than replaying a retained outcome.
    pub async fn thread_rewind(
        &self,
        request: ProviderRequest,
        after: Option<String>,
    ) -> Result<(), ProviderError> {
        let _admission = self.admission.read().await;
        self.ensure_admission(ProviderOperation::ThreadRewind)?;
        let rejected = || {
            failure(
                ProviderOperation::ThreadRewind,
                ProviderFailureKind::Rejected,
            )
        };
        let after = after
            .map(forge_provider_api::CheckpointId::new)
            .transpose()
            .map_err(|_| rejected())?;
        let owner = request.owner().map_err(|_| rejected())?;
        let binding = self.binding_under_admission(&request).await?;
        // A rewind, even an uncertain one, may remove context delivered after its boundary.
        self.session(owner.clone()).await.clear_context();
        let control = OperationControl::fresh(ProviderOperation::ThreadRewind)?;
        let context = control.context();
        super::operation::observe(
            self.trace.clone(),
            &self.core_events,
            ProviderOperation::ThreadRewind,
            &context,
            forge_provider_api::EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(owner),
            },
            Some(binding.id.clone()),
            self.native.thread_rewind(
                context.clone(),
                &binding.id,
                forge_provider_api::ThreadRewindRequest { after },
            ),
        )
        .await
    }

    /// Compact the retained target without resolving a different binding on retry.
    pub async fn thread_compact(
        &self,
        request: super::ProviderCompactionRequest,
    ) -> Result<forge_provider_api::Compaction, ProviderError> {
        let _admission = self.admission.read().await;
        self.ensure_admission(ProviderOperation::ThreadCompact)?;
        let id = forge_provider_api::OperationId::new(request.id)?;
        self.history_operations
            .compact(
                self.native.as_ref(),
                self.trace.clone(),
                &self.core_events,
                id,
                async {
                    let binding = self.binding_under_admission(&request.request).await?;
                    let owner = request.request.owner().map_err(|_| {
                        failure(
                            ProviderOperation::ThreadCompact,
                            ProviderFailureKind::Rejected,
                        )
                    })?;
                    let host =
                        self.sessions
                            .lock()
                            .await
                            .get(&owner)
                            .cloned()
                            .ok_or_else(|| {
                                failure(
                                    ProviderOperation::ThreadCompact,
                                    ProviderFailureKind::SessionMissing,
                                )
                            })?;
                    // Compaction summarizes delivered context, so the next turn sends it again.
                    host.clear_context();
                    Ok(history_operation::CompactionTarget::new(
                        host, owner, binding.id,
                    ))
                },
            )
            .await
    }

    /// Cancel only the identified invocation and collect its host-owned effects separately.
    pub async fn cancel_invocation(
        &self,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        self.cancel_in_context(
            scope,
            OperationControl::fresh(ProviderOperation::TurnInterrupt)?,
        )
        .await
    }

    /// Collect an exact invocation from a retained host worker without a second response deadline.
    /// The exchange owner or tool watchdog retains this future through native and host settlement.
    pub(crate) async fn collect_invocation(
        &self,
        scope: forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        self.cancel_in_context(scope, OperationControl::cancellation_collection()?)
            .await
    }

    /// Preserve one cancellation identity and its caller-selected observation policy until collection.
    async fn cancel_in_context(
        &self,
        scope: forge_provider_api::InvocationScope,
        operation: OperationControl,
    ) -> Result<forge_provider_api::TurnInterruptResult, ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(&scope.owner())
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::TurnInterrupt,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let bound = host.initialized.get().ok_or_else(|| {
            failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::SessionMissing,
            )
        })?;
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        if let Some(invocation) = &invocation {
            invocation.request_snapshot(&scope)?;
            if invocation.foreground_pending() {
                invocation.cancel();
            }
        }
        let context = operation.context();
        let native = super::operation::observe(
            self.trace.clone(),
            &self.core_events,
            ProviderOperation::TurnInterrupt,
            &context,
            scope.clone().into(),
            Some(bound.binding.id.clone()),
            self.native.turn_interrupt(
                context.clone(),
                &bound.binding.id,
                forge_provider_api::CancelRequest {
                    scope: scope.clone(),
                },
            ),
        )
        .await;
        let host_collection = collect_selected_invocations(invocation.into_iter().collect()).await;
        match (native, host_collection) {
            (Ok(settlement), Ok(())) => {
                if settlement.operation_id != *context.id()
                    || settlement.thread_id != scope.owner().thread_id()
                    || settlement.turn_id
                        != scope.turn_id().map_err(|_| {
                            ProviderError::Observation(
                                forge_provider_api::ObservationFailure::OwnershipConflict,
                            )
                        })?
                {
                    return Err(ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                    ));
                }
                self.wait_watermark(&scope.owner(), settlement.watermark)
                    .await?;
                Ok(settlement)
            }
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        }
    }

    /// Collect adapter preparation after the journal positively prevented this exact input's send.
    /// Ignore only an exact WrongInvocation rejection: it proves no retained native invocation
    /// was installed. Other failures retain uncertainty and cannot authorize a replacement.
    pub(crate) async fn cancel_prepared_invocation(
        &self,
        scope: &forge_provider_api::InvocationScope,
        retirement: super::submission::SubmissionRetirement,
    ) -> Result<(), ProviderError> {
        match retirement {
            super::submission::SubmissionRetirement::Cancelled => {}
            super::submission::SubmissionRetirement::Sending
            | super::submission::SubmissionRetirement::Accepted => {
                return Err(ProviderError::DeliveryUncertain);
            }
        }
        self.collect_unconsumed_invocation(scope, OperationControl::cancellation_collection()?)
            .await
    }

    /// Join an exact native rejection or positively prevented input before releasing its host scope.
    /// Call only after positive non-consumption proof; an uncertain send requires reconciliation.
    async fn collect_unconsumed_invocation(
        &self,
        scope: &forge_provider_api::InvocationScope,
        collection: OperationControl,
    ) -> Result<(), ProviderError> {
        let host = self.sessions.lock().await.get(&scope.owner()).cloned();
        let Some(host) = host else {
            return Ok(());
        };
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        let native_input = if let Some(invocation) = invocation {
            invocation.request_snapshot(scope)?;
            invocation.cancel()
        } else {
            super::host_invocation::NativeInputAdmission::Preparing
        };
        let result = if let (super::host_invocation::NativeInputAdmission::Requested, Some(bound)) =
            (native_input, host.initialized.get())
        {
            self.register_binding_operation(&collection.context(), &bound.binding.id)
                .await?;
            match self
                .native
                .turn_interrupt(
                    collection.context(),
                    &bound.binding.id,
                    forge_provider_api::CancelRequest {
                        scope: scope.clone(),
                    },
                )
                .await
            {
                Ok(settlement)
                    if settlement.operation_id == *collection.context().id()
                        && settlement.thread_id == scope.owner().thread_id()
                        && scope.turn_id().is_ok_and(|turn| settlement.turn_id == turn) =>
                {
                    self.wait_watermark(&scope.owner(), settlement.watermark)
                        .await
                }
                Ok(_) => Err(ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                )),
                Err(ProviderError::Failure(failure))
                    if failure.operation() == ProviderOperation::TurnInterrupt
                        && failure.kind() == ProviderFailureKind::WrongInvocation =>
                {
                    Ok(())
                }
                Err(error) => Err(error),
            }
        } else {
            // Cancellation atomically excluded native admission; only host preparation can remain.
            Ok(())
        };
        let collection = self.collect_host_effects(scope).await;
        self.trace.record(scope.owner().session_id().as_str(), "provider.preparation.collected", serde_json::json!({
            "scope":scope,
            "native_requested":matches!(native_input, super::host_invocation::NativeInputAdmission::Requested),
            "native_collected":result.is_ok(), "host_collected":collection.is_ok(),
        }));
        match (result, collection) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Err(error), Ok(())) | (Ok(()), Err(error)) => Err(error),
        }
    }

    /// Collect only host-owned jobs after provider settlement; never issue a native cleanup request.
    pub async fn collect_host_effects(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let Some(host) = self.sessions.lock().await.get(&scope.owner()).cloned() else {
            return Ok(());
        };
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned();
        if let Some(invocation) = &invocation {
            invocation.request_snapshot(scope)?;
        }
        collect_selected_invocations(invocation.into_iter().collect()).await
    }

    /// Close the exact retained owner without exposing a separate fence operation.
    pub async fn session_close_owner(
        &self,
        owner: &ThreadOwner,
    ) -> Result<Option<BindingId>, ProviderError> {
        if let Some(host) = self.sessions.lock().await.get(owner).cloned() {
            host.observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .last_configuration = None;
            host.closing
                .store(true, std::sync::atomic::Ordering::Release);
            if let Some(registry) = host
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .ownership
                .as_ref()
            {
                registry
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .retire();
            }
            for context in host
                .observed_contexts
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .values()
            {
                context.retire().map_err(|error| {
                    super::lifecycle_error::classify(ProviderOperation::ThreadClose, error)
                })?;
            }
            host.changed.notify_waiters();
            for invocation in host.invocations.lock().await.values() {
                invocation.collect_observed().await?;
            }
            host.open_operation.cancel();
            if let Some(bound) = host.initialized.get() {
                bound
                    .retiring
                    .store(true, std::sync::atomic::Ordering::Release);
            }
            for invocation in host.invocations.lock().await.values() {
                invocation.cancel();
            }
        }
        let _admission = self.admission.write().await;
        self.retire_owner(owner).await
    }

    /// Collect all bindings and preserve every distinct cleanup failure.
    pub async fn shutdown(&self) -> Result<(), ProviderError> {
        self.closed
            .store(true, std::sync::atomic::Ordering::Release);
        self.history_operations.close();
        let mut errors = Vec::new();
        // Revoke pending opens before waiting for their shared admission guards.
        let pending = self
            .sessions
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for session in pending {
            session
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .last_configuration = None;
            session
                .closing
                .store(true, std::sync::atomic::Ordering::Release);
            session.open_operation.cancel();
            for context in session
                .observed_contexts
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .values()
            {
                if let Err(error) = context.retire() {
                    errors.push(super::lifecycle_error::classify(
                        ProviderOperation::ThreadClose,
                        error,
                    ));
                }
            }
            session.changed.notify_waiters();
            for invocation in session.invocations.lock().await.values() {
                invocation.cancel();
                if let Err(error) = invocation.retire_observed(None, None).await {
                    errors.push(error);
                }
            }
            if let Some(bound) = session.initialized.get() {
                bound
                    .retiring
                    .store(true, std::sync::atomic::Ordering::Release);
            }
        }
        let _admission = self.admission.write().await;
        let owners = self
            .sessions
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for owner in owners {
            let ambiguous = self
                .sessions
                .lock()
                .await
                .get(&owner)
                .and_then(|host| host.initialized.get())
                .is_some_and(|bound| self.binding_collection.ambiguous(&bound.binding.id));
            if ambiguous {
                continue;
            }
            if let Err(error) = self.retire_owner(&owner).await {
                let ambiguous = self
                    .sessions
                    .lock()
                    .await
                    .get(&owner)
                    .and_then(|host| host.initialized.get())
                    .is_some_and(|bound| self.binding_collection.ambiguous(&bound.binding.id));
                if !ambiguous {
                    errors.push(error);
                }
            }
        }
        if let Err(error) = self.utility.shutdown().await {
            errors.push(error);
        }
        match self
            .native
            .shutdown(OperationControl::fresh(ProviderOperation::Shutdown)?.context())
            .await
        {
            Ok(()) => {
                self.binding_collection
                    .collected
                    .store(true, std::sync::atomic::Ordering::Release);
                self.binding_collection.changed.notify_waiters();
                let remaining = self
                    .sessions
                    .lock()
                    .await
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>();
                for owner in remaining {
                    if let Err(error) = self.retire_owner(&owner).await {
                        errors.push(error);
                    }
                }
            }
            Err(error) => errors.push(error),
        }
        combined(errors)
    }

    /// Collect the exact retained reader before allowing another channel for this owner.
    async fn retire_owner(&self, owner: &ThreadOwner) -> Result<Option<BindingId>, ProviderError> {
        let session = self.sessions.lock().await.get(owner).cloned();
        let Some(session) = session else {
            return Ok(self
                .retired_bindings
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .get(owner)
                .cloned()
                .flatten());
        };
        session
            .closing
            .store(true, std::sync::atomic::Ordering::Release);
        session
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .last_configuration = None;
        session.changed.notify_waiters();
        let mut errors = Vec::new();
        for context in session
            .observed_contexts
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .values()
        {
            if let Err(error) = context.retire() {
                errors.push(super::lifecycle_error::classify(
                    ProviderOperation::ThreadClose,
                    error,
                ));
            }
        }
        for invocation in session.invocations.lock().await.values() {
            invocation.cancel();
        }
        if session.initialized.get().is_none()
            && let Some(opened) = session
                .open_operation
                .collect(
                    self.native.as_ref(),
                    self.trace.clone(),
                    &self.core_events,
                    self.binding_collection.clone(),
                )
                .await?
        {
            let bound = self.install_session(owner, &session, opened);
            session.initialized.set(bound).map_err(|_| {
                failure(
                    ProviderOperation::ThreadClose,
                    ProviderFailureKind::OperationConflict,
                )
            })?;
        }
        let retired_binding = session
            .initialized
            .get()
            .map(|bound| bound.binding.id.clone());
        if let Some(native) = session.initialized.get() {
            let ambiguous = self.binding_collection.ambiguous(&native.binding.id);
            if ambiguous
                && !self
                    .binding_collection
                    .collected
                    .load(std::sync::atomic::Ordering::Acquire)
            {
                return Err(ProviderError::Observation(
                    forge_provider_api::ObservationFailure::OwnershipConflict,
                ));
            }
            if self.binding_collection.collected() {
                *native.retired.lock().await = true;
                if let Some(events) = &native.rejected_events {
                    events.lock().await.take();
                }
            }
            native
                .retiring
                .store(true, std::sync::atomic::Ordering::Release);
            {
                let mut retired = native.retired.lock().await;
                if !*retired {
                    let context =
                        OperationControl::fresh(ProviderOperation::ThreadClose)?.context();
                    match super::operation::observe(
                        self.trace.clone(),
                        &self.core_events,
                        ProviderOperation::ThreadClose,
                        &context,
                        forge_provider_api::EventScope::Binding {
                            thread_id: forge_provider_api::ThreadId::from(owner.clone()),
                        },
                        Some(native.binding.id.clone()),
                        self.native
                            .thread_close(context.clone(), &native.binding.id),
                    )
                    .await
                    {
                        Ok(()) => *retired = true,
                        Err(error) => errors.push(error),
                    }
                }
            }
            let native_collected = *native.retired.lock().await;
            if native_collected {
                native
                    .relay_stop
                    .send_replace(Some(native.registrations.watermark()));
            }
            if let Err(error) = collect_invocations(&session).await {
                errors.push(error);
            }
            if native_collected {
                loop {
                    let changed = session.changed.notified();
                    tokio::pin!(changed);
                    changed.as_mut().enable();
                    if *session
                        .observers
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        == 0
                    {
                        break;
                    }
                    changed.await;
                }
            }
            let mut relay = native.relay.lock().await;
            if let Some(task) = relay.as_mut()
                && (native_collected || task.is_finished())
            {
                match task.await {
                    Ok(Ok(())) => {}
                    Ok(Err(error)) => errors.push(error),
                    Err(_) => errors.push(
                        ProviderFailure::new(
                            ProviderOperation::ThreadClose,
                            ProviderFailureKind::Runtime,
                        )
                        .at_phase(
                            forge_provider_api::ProviderPhase::Collection,
                            forge_provider_api::FailureDisposition::Invalidated,
                        )
                        .into(),
                    ),
                }
                relay.take();
            }
        }
        // Failed native collection still owns its producer and reader for the next close attempt.
        combined(errors)?;
        // An owner takes the host stream under shared admission, which every caller of this close
        // holds exclusively. A stream still parked here, as when the binding was opened only as a
        // fork source, can therefore never be consumed. Dropping it ends the wait below as
        // abandoned instead of blocking this close and every close queued behind it.
        session.receiver.lock().await.take();
        // Closing the native reader may leave a valid published prefix queued for the owner.
        // Keep the binding routable until that exact prefix has been consumed, or until the
        // owner refuses it or exits, since neither can consume it and close holds admission.
        let watermark = session
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .published;
        wait_for_consumption(&session, watermark).await?;
        session.invocations.lock().await.clear();
        let mut sessions = self.sessions.lock().await;
        sessions.remove(owner);
        if let Some(binding) = &retired_binding
            && let Some(registry) = self
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .get(binding)
        {
            registry
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .retire();
        }
        self.retired_bindings
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .insert(owner.clone(), retired_binding.clone());
        Ok(retired_binding)
    }

    /// Observe durable delivery evidence without admitting a new input or mutating its journal.
    pub(crate) async fn input_reconcile(
        &self,
        request: forge_provider_api::InputReconcileRequest,
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        self.ensure_admission(ProviderOperation::InputReconcile)?;
        self.native
            .input_reconcile(
                OperationControl::fresh(ProviderOperation::InputReconcile)?.context(),
                request,
            )
            .await
    }

    /// Prove successful native collection ended before the original invocation ever started.
    pub(crate) async fn not_started_error(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<Option<ProviderError>, ProviderError> {
        let (registrations, turn) = self.invocation_registration(scope).await?;
        Ok(registrations.not_started_error(&turn))
    }

    /// Await definitive start or collected no-start evidence from the same retained native owner.
    pub(crate) async fn wait_start_result(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        let (registrations, turn) = self.invocation_registration(scope).await?;
        registrations.start_result(&turn).await
    }

    /// Validate exact original provenance before applying mutable native session metadata.
    pub(crate) fn binding_source_status(
        &self,
        binding: &BindingId,
        owner: &ThreadOwner,
        thread: Option<&forge_provider_api::ThreadId>,
    ) -> Result<NativeBindingStatus, ProviderError> {
        let ownership = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(binding)
            .cloned()
            .ok_or(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ))?;
        let status = ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .source_status(owner, thread)?;
        Ok(
            if self.closed.load(std::sync::atomic::Ordering::Acquire)
                || self.binding_collection.ambiguous(binding)
            {
                NativeBindingStatus::RetiredKnown
            } else {
                status
            },
        )
    }
    /// Resolve observed child metadata from its exact original generation without retaining native resources.
    pub(crate) fn observed_agent(
        &self,
        binding: &BindingId,
        agent_id: &forge_provider_api::AgentId,
        turn_id: Option<&forge_provider_api::TurnId>,
    ) -> Result<super::ownership::ObservedAgentAssociation, ProviderError> {
        let ownership = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(binding)
            .cloned()
            .ok_or(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ))?;
        ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .observed_agent(agent_id, turn_id)
            .map_err(Into::into)
    }

    /// Resolve host publications only to an exact previously registered native child scope.
    pub(crate) fn observed_scope(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<super::ObservedAgentAssociation, ProviderError> {
        let rejected = || {
            ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
        };
        let turn = scope.turn_id().map_err(|_| rejected())?;
        let registrations = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .values()
            .cloned()
            .collect::<Vec<_>>();
        let mut matched = None;
        for registration in registrations {
            let proof = registration
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .observed_child(&scope.owner().thread_id(), &turn);
            if let Ok(proof) = proof
                && proof.turn() == Some(scope)
            {
                if matched.is_some() {
                    return Err(rejected());
                }
                matched = Some(proof);
            }
        }
        matched.ok_or_else(rejected)
    }

    /// Resolve an observed child's exact original thread and turn under its preserved native generation.
    pub(crate) fn observed_child(
        &self,
        binding: &BindingId,
        thread: &forge_provider_api::ThreadId,
        turn: &forge_provider_api::TurnId,
    ) -> Result<super::ownership::ObservedAgentAssociation, ProviderError> {
        let ownership = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(binding)
            .cloned()
            .ok_or(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ))?;
        ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .observed_child(thread, turn)
            .map_err(Into::into)
    }

    /// Persist consumption metadata before publishing the native consumer's watermark.
    /// Callers skip output deltas, whose only durable form is the exchange text.
    pub(crate) fn record_core_event(
        &self,
        owner: &ThreadOwner,
        record: &crate::exchange::core_log::CoreRecord,
    ) -> Result<(), ProviderError> {
        self.core_events
            .append(owner.session_id().as_str(), record)
            .map(|_| ())
            .map_err(|_| {
                forge_provider_api::ProviderFailure::new(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Persistence,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Persistence,
                    forge_provider_api::FailureDisposition::Invalidated,
                )
                .into()
            })
    }

    /// Commit owner-consumed stream progress and apply ordered binding metadata.
    pub(crate) async fn event_consumed(
        &self,
        binding: &BindingId,
        sequence: forge_provider_api::EventSequence,
    ) -> Result<(), ProviderError> {
        let host = self.consuming_host(binding).await?;
        let mut state = host
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if sequence > state.published {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            ));
        }
        if sequence <= state.consumed {
            return Ok(());
        }
        while state
            .pending
            .first_key_value()
            .is_some_and(|(position, _)| *position <= sequence)
        {
            let (_, event) = state
                .pending
                .pop_first()
                .expect("the checked pending prefix contains an event");
            match event {
                BindingMetadata::Capabilities(capabilities) => {
                    if state.capabilities.as_ref().is_none_or(|current| {
                        current.effective_revision <= capabilities.effective_revision
                    }) {
                        state.capabilities = Some(capabilities);
                    }
                }
                BindingMetadata::Identity(identity) => state.identity = Some(identity),
            }
        }
        state.consumed = sequence;
        drop(state);
        host.changed.notify_waiters();
        Ok(())
    }

    /// Record that the owner refused a published position and will never consume it.
    ///
    /// The refused event's binding metadata is discarded rather than applied, so identity and
    /// capabilities still advance only through consumed events. Waits through the position end
    /// as abandoned instead of waiting for a consumption that cannot happen.
    pub(crate) async fn event_rejected(
        &self,
        binding: &BindingId,
        sequence: forge_provider_api::EventSequence,
    ) -> Result<(), ProviderError> {
        let host = self.consuming_host(binding).await?;
        let mut state = host
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if sequence > state.published {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Protocol,
            ));
        }
        if sequence <= state.consumed || sequence <= state.rejected {
            return Ok(());
        }
        state.pending.remove(&sequence);
        state.rejected = sequence;
        drop(state);
        host.changed.notify_waiters();
        Ok(())
    }

    /// Find the live host whose current ownership registry still matches `binding`'s original.
    async fn consuming_host(&self, binding: &BindingId) -> Result<Arc<HostSession>, ProviderError> {
        let original = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get(binding)
            .cloned()
            .ok_or(ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            ))?;
        let sessions = self.sessions.lock().await;
        sessions
            .values()
            .find(|host| {
                host.initialized
                    .get()
                    .is_some_and(|bound| &bound.binding.id == binding)
                    && host
                        .observation
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .ownership
                        .as_ref()
                        .is_some_and(|ownership| Arc::ptr_eq(ownership, &original))
            })
            .cloned()
            .ok_or(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            ))
    }

    /// Wait for authoritative event consumption through an operation's publication boundary.
    pub(crate) async fn wait_watermark(
        &self,
        owner: &ThreadOwner,
        watermark: forge_provider_api::EventSequence,
    ) -> Result<(), ProviderError> {
        let host = self
            .sessions
            .lock()
            .await
            .get(owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ThreadClose,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        wait_for_watermark(&host, watermark).await
    }

    /// Resolve only the original invocation's immutable binding and native registration capability.
    async fn invocation_registration(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> Result<
        (
            forge_provider_api::TurnRegistrationReader,
            forge_provider_api::TurnId,
        ),
        ProviderError,
    > {
        let rejected = || {
            ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
        };
        let host = self
            .sessions
            .lock()
            .await
            .get(&scope.owner())
            .cloned()
            .ok_or_else(rejected)?;
        let invocation = host
            .invocations
            .lock()
            .await
            .get(&scope.invocation_id)
            .cloned()
            .ok_or_else(rejected)?;
        let binding = invocation.binding_for(scope)?;
        let bound = host
            .initialized
            .get()
            .filter(|bound| &bound.binding.id == binding)
            .ok_or_else(rejected)?;
        let turn = scope.turn_id().map_err(|_| rejected())?;
        let address = bound
            .registrations
            .registered_turn(&turn)
            .ok_or_else(rejected)?;
        if address.scope != *scope
            || address.thread_id != scope.owner().thread_id()
            || address.turn_id != turn
        {
            return Err(rejected());
        }
        Ok((bound.registrations.clone(), turn))
    }

    /// Reject new native work as soon as host shutdown closes its global admission gate.
    fn ensure_admission(&self, operation: ProviderOperation) -> Result<(), ProviderError> {
        if self.closed.load(std::sync::atomic::Ordering::Acquire) {
            return Err(
                ProviderFailure::new(operation, ProviderFailureKind::BindingClosed)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Admission,
                        forge_provider_api::FailureDisposition::Invalidated,
                    )
                    .into(),
            );
        }
        Ok(())
    }

    /// Capture the exact source generation and target resources before native fork admission.
    async fn prepare_fork_request(
        &self,
        request: super::ProviderForkRequest,
    ) -> Result<forge_provider_api::SessionForkRequest, ProviderError> {
        let owner = ThreadOwner::root(request.target_harness_session_id.clone());
        let source = catalog_request(&request.source);
        let target_catalog = super::ProviderCatalogRequest {
            execution_owner: Some(owner.clone()),
            harness_session_id: request.target_harness_session_id,
            provider_session_id: None,
            ..source.clone()
        };
        let descriptor = self.descriptor();
        let resources = match descriptor.capability.integration.resource_target.as_deref() {
            Some(target) => crate::settings::SessionResources::load(
                &self.root,
                &target_catalog,
                descriptor.kind.as_str(),
                target,
            )
            .map_err(|_| failure(ProviderOperation::ThreadFork, ProviderFailureKind::Rejected))?
            .selection(),
            None => Default::default(),
        };
        let target = forge_provider_api::ForkTarget {
            thread_id: owner.thread_id(),
            workspace: request.source.workspace.clone(),
            configuration: forge_provider_api::SessionConfiguration {
                model: model_selection(&request.source),
                access: request.source.access.clone(),
                approval: request.source.approval_mode,
                resources,
            },
            // The forked target opens with its own binding and delivery record, which delivers
            // every context section again on its first turn.
            system_instructions: request
                .source
                .open_instructions(descriptor.capability.integration.context_channel),
        };
        let checkpoint = request
            .checkpoint_id
            .map(forge_provider_api::CheckpointId::new)
            .transpose()
            .map_err(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::ThreadFork,
                    ProviderFailureKind::Rejected,
                ))
            })?;
        let source = match checkpoint {
            Some(checkpoint) => forge_provider_api::ForkSource::Checkpoint {
                provider_session_id: request.source.provider_session_id.ok_or_else(|| {
                    failure(
                        ProviderOperation::ThreadFork,
                        ProviderFailureKind::SessionMissing,
                    )
                })?,
                checkpoint,
            },
            None => {
                // Session forks also support stored source conversations whose binding is not open yet.
                // The caller already holds bridge admission; do not recursively acquire its read lock.
                let binding = self.binding_under_admission(&request.source).await?;
                forge_provider_api::ForkSource::Binding(binding.id)
            }
        };
        Ok(forge_provider_api::SessionForkRequest { source, target })
    }

    /// Allocate only host delivery state; native initialization remains explicit.
    async fn session(&self, owner: ThreadOwner) -> Arc<HostSession> {
        self.sessions
            .lock()
            .await
            .entry(owner.clone())
            .or_insert_with(|| {
                self.retired_bindings
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .remove(&owner);
                let (sender, receiver) = mpsc::unbounded_channel();
                Arc::new(HostSession {
                    provider_closed: self.closed.clone(),
                    initialized: OnceCell::new(),
                    observed_contexts: Default::default(),
                    observed_authorities: Default::default(),
                    open_operation: Default::default(),
                    sender,
                    preparation: Mutex::new(()),
                    closing: std::sync::atomic::AtomicBool::new(false),
                    receiver: Mutex::new(Some(HostEventStream { receiver })),
                    invocations: Mutex::new(HashMap::new()),
                    request: Mutex::new(None),
                    context_delivery: std::sync::Mutex::new(
                        super::context_delivery::ContextDelivery::new(),
                    ),
                    observation: Default::default(),
                    changed: Default::default(),
                    observers: Default::default(),
                })
            })
            .clone()
    }
}

/// Convert only host-owned catalog context into its normalized request.
fn catalog_request(request: &ProviderRequest) -> super::ProviderCatalogRequest {
    super::ProviderCatalogRequest {
        execution_owner: request.execution_owner.clone(),
        harness_session_id: request.harness_session_id.clone(),
        workspace: request.workspace.clone(),
        access: request.access.clone(),
        approval_mode: request.approval_mode,
        provider_session_id: request.provider_session_id.clone(),
    }
}

/// Keep open catalog identifiers together with typed reasoning and service choices.
fn model_selection(request: &ProviderRequest) -> forge_provider_api::ModelSelection {
    forge_provider_api::ModelSelection {
        model: (request.model != "default").then(|| request.model.clone()),
        reasoning: if request.effort == "default" {
            forge_provider_api::ReasoningSelection::Default
        } else {
            forge_provider_api::ReasoningSelection::Level(request.effort.clone())
        },
        context_window: request.context_window.clone(),
        service_tier: request.service_tier,
    }
}

/// Retain only binding metadata until its source observation is durably consumed.
fn binding_metadata(event: &ProviderEvent, owner: &ThreadOwner) -> Option<BindingMetadata> {
    match event {
        ProviderEvent::Provider { event } => match event {
            forge_provider_api::ProviderConnectionEvent::CapabilitiesChanged(capabilities) => {
                Some(BindingMetadata::Capabilities(capabilities.clone()))
            }
            forge_provider_api::ProviderConnectionEvent::Warning(_)
            | forge_provider_api::ProviderConnectionEvent::Unavailable(_) => None,
        },
        ProviderEvent::Thread { thread_id, event } => match event {
            forge_provider_api::ThreadEvent::IdentityResolved(identity) => (thread_id
                == &owner.thread_id())
                .then(|| BindingMetadata::Identity(identity.clone())),
            forge_provider_api::ThreadEvent::Error { .. }
            | forge_provider_api::ThreadEvent::ContextUsage(_)
            | forge_provider_api::ThreadEvent::TerminalsUpdated(_)
            | forge_provider_api::ThreadEvent::CompactionUpdated(_) => None,
        },
        ProviderEvent::Turn { .. } | ProviderEvent::Item { .. } | ProviderEvent::Agent { .. } => {
            None
        }
    }
}

/// Disarm `<forge>` markers in the user's own words before they reach a provider.
///
/// The open-time instructions tell the model that every `<forge>` block in a user message comes
/// from Harness, so user text must not be able to open or close one. Text inside `<forge>`
/// elements is escaped where those elements are rendered.
fn neutralized_input(
    mut input: forge_provider_api::forge::TurnInput,
) -> forge_provider_api::forge::TurnInput {
    use forge_provider_api::PromptInput;
    use forge_provider_api::forge::neutralize;
    input.map_user_input(|user| match user {
        PromptInput::Text(text) => PromptInput::Text(neutralize(&text).into_owned()),
        PromptInput::Skill { name, arguments } => PromptInput::Skill {
            name,
            arguments: neutralize(&arguments).into_owned(),
        },
    });
    input
}

/// Resolve admitted attachments on the blocking pool before any native dispatch.
async fn prepare_attachments(
    attachments: Vec<crate::attachment::InputAttachment>,
) -> Result<Vec<forge_provider_api::FileAttachment>, ProviderError> {
    tokio::task::spawn_blocking(move || {
        attachments
            .iter()
            .map(crate::attachment::InputAttachment::prepare)
            .collect::<Result<Vec<_>, crate::attachment::AttachmentPreparationError>>()
    })
    .await
    .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?
    .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))
}

/// Keep host failures typed and independent of native diagnostics.
fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// Preserve positive native admission evidence in the owner's input-delivery result vocabulary.
fn input_rejection(operation: ProviderOperation, error: ProviderError) -> ProviderError {
    if matches!(&error, ProviderError::Failure(failure)
        if failure.operation() == operation
            && matches!(failure.phase(), forge_provider_api::ProviderPhase::Admission | forge_provider_api::ProviderPhase::Preparation)
            && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
    {
        return ProviderError::PreparationFailed(Box::new(error));
    }
    error
}

/// Check binding identity before forwarding an event without reinterpreting its lifecycle.
async fn relay_events(
    native: Arc<dyn NativeProvider>,
    owner: ThreadOwner,
    mut events: ProviderEventStream,
    host: Arc<HostSession>,
    lifetime: NativeBindingLifetime,
    mut stopping: tokio::sync::watch::Receiver<Option<forge_provider_api::EventSequence>>,
) -> Result<(), ProviderError> {
    let NativeBindingLifetime {
        binding,
        retiring,
        retired,
        collection,
    } = lifetime;
    struct ClosedObservation(Arc<HostSession>);
    impl Drop for ClosedObservation {
        fn drop(&mut self) {
            let mut state = self
                .0
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            state.closed = true;
            state.last_configuration = None;
            drop(state);
            self.0.changed.notify_waiters();
        }
    }
    let _closed = ClosedObservation(host.clone());
    let sender = &host.sender;
    loop {
        if let Some(boundary) = *stopping.borrow_and_update()
            && host
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .forwarded
                >= boundary
        {
            return Ok(());
        }
        let changed = collection.changed.notified();
        tokio::pin!(changed);
        changed.as_mut().enable();
        let received = tokio::select! {
            received = events.recv() => Some(received),
            _ = &mut changed => None,
            _ = stopping.changed() => None,
        };
        let Some(received) = received else {
            continue;
        };
        match received {
            Ok(Some(observation)) if observation.binding_id == binding => {
                let registration = match &observation.event {
                    ProviderEvent::Turn { turn_id, .. } | ProviderEvent::Item { turn_id, .. } => {
                        events.registrations().registered_turn(turn_id)
                    }
                    ProviderEvent::Agent {
                        event: forge_provider_api::agent::AgentEvent::Lifecycle { event },
                        ..
                    } => {
                        let agent = match event {
                            forge_provider_api::agent::AgentLifecycleEvent::Started {
                                agent,
                                ..
                            }
                            | forge_provider_api::agent::AgentLifecycleEvent::Updated {
                                agent,
                                ..
                            }
                            | forge_provider_api::agent::AgentLifecycleEvent::Finished {
                                agent,
                                ..
                            } => agent,
                        };
                        agent.delegation.as_ref().and_then(|delegation| {
                            events
                                .registrations()
                                .registered_turn(&delegation.parent_turn_id)
                        })
                    }
                    ProviderEvent::Provider {
                        event: forge_provider_api::ProviderConnectionEvent::Warning(warning),
                    } => match &warning.scope {
                        forge_provider_api::WarningScope::Turn { turn_id, .. }
                        | forge_provider_api::WarningScope::Item { turn_id, .. } => {
                            events.registrations().registered_turn(turn_id)
                        }
                        forge_provider_api::WarningScope::Provider
                        | forge_provider_api::WarningScope::Thread { .. }
                        | forge_provider_api::WarningScope::Agent { .. }
                        | forge_provider_api::WarningScope::Operation { .. } => None,
                    },
                    ProviderEvent::Provider { .. }
                    | ProviderEvent::Thread { .. }
                    | ProviderEvent::Agent { .. } => None,
                };
                match &observation.event {
                    // Completed or unconfirmed compaction may have summarized away earlier context.
                    ProviderEvent::Thread {
                        thread_id,
                        event: forge_provider_api::ThreadEvent::CompactionUpdated(compaction),
                    } if thread_id == &owner.thread_id()
                        && matches!(
                            compaction.state,
                            forge_provider_api::CompactionState::Completed
                                | forge_provider_api::CompactionState::Unconfirmed
                        ) =>
                    {
                        host.clear_context();
                    }
                    ProviderEvent::Provider {
                        event: forge_provider_api::ProviderConnectionEvent::Warning(warning),
                    } => tracing::warn!(target: "forge::provider", event_name = "provider.warning",
                        session_id = owner.session_id().as_str(), message = %warning.message),
                    ProviderEvent::Provider {
                        event: forge_provider_api::ProviderConnectionEvent::Unavailable(unavailable),
                    } => {
                        tracing::error!(target: "forge::provider", event_name = "provider.unavailable",
                        session_id = owner.session_id().as_str(), error = %unavailable.failure,
                        message = %unavailable.message)
                    }
                    _ => {}
                }
                let thread_owner = match &observation.event {
                    ProviderEvent::Thread { thread_id, .. } => events
                        .registrations()
                        .thread_registered(thread_id)
                        .then(|| ThreadOwner::from_thread_id(thread_id).ok())
                        .flatten()
                        .map(|owner| (thread_id.clone(), owner)),
                    ProviderEvent::Provider {
                        event: forge_provider_api::ProviderConnectionEvent::Warning(warning),
                    } => match &warning.scope {
                        forge_provider_api::WarningScope::Thread { thread_id } => events
                            .registrations()
                            .thread_registered(thread_id)
                            .then(|| ThreadOwner::from_thread_id(thread_id).ok())
                            .flatten()
                            .map(|owner| (thread_id.clone(), owner)),
                        forge_provider_api::WarningScope::Provider
                        | forge_provider_api::WarningScope::Turn { .. }
                        | forge_provider_api::WarningScope::Item { .. }
                        | forge_provider_api::WarningScope::Agent { .. }
                        | forge_provider_api::WarningScope::Operation { .. } => None,
                    },
                    ProviderEvent::Provider { .. }
                    | ProviderEvent::Turn { .. }
                    | ProviderEvent::Item { .. }
                    | ProviderEvent::Agent { .. } => None,
                };
                if matches!(
                    &observation.event,
                    ProviderEvent::Provider {
                        event: forge_provider_api::ProviderConnectionEvent::Unavailable(_)
                    }
                ) && let Some(registry) = host
                    .observation
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .ownership
                    .as_ref()
                {
                    registry
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .retire();
                }
                let resolved = (|| {
                    let mut state = host
                        .observation
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    if observation.sequence <= state.published {
                        return Err(EventDeliveryError::InvalidTransportData);
                    }
                    if let Some((thread, owner)) = thread_owner {
                        state
                            .ownership
                            .as_mut()
                            .ok_or(EventDeliveryError::InvalidTransportData)?
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .learn_thread_owner(&thread, owner)?;
                    }
                    if let Some(address) = registration {
                        state
                            .ownership
                            .as_mut()
                            .ok_or(EventDeliveryError::InvalidTransportData)?
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .admit_observed(address)?;
                    }
                    let scope = state
                        .ownership
                        .as_mut()
                        .ok_or(EventDeliveryError::InvalidTransportData)?
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .resolve(&observation.binding_id, &observation.event)?;
                    Ok::<_, EventDeliveryError>(scope)
                })();
                let scope = match resolved {
                    Ok(scope) => scope,
                    Err(error) => {
                        let _ = sender.send(Err(error.clone()));
                        return retire_failed_observer(
                            &native,
                            &binding,
                            &retired,
                            &retiring,
                            &collection,
                            error.into(),
                        )
                        .await;
                    }
                };
                if matches!(&observation.event, ProviderEvent::Agent { .. })
                    && let Some(parent) = scope.invocation()
                {
                    for invocation in host.invocations.lock().await.values() {
                        invocation
                            .mark_descendant_work(parent)
                            .map_err(|_| EventDeliveryError::InvalidTransportData)?;
                    }
                }
                let invocation = if let Some(scope) = scope.invocation() {
                    host.invocations
                        .lock()
                        .await
                        .get(&scope.invocation_id)
                        .cloned()
                } else {
                    None
                };
                let observed_context = if invocation.is_none() {
                    if let Some(child) = scope.invocation() {
                        let ownership = host
                            .observation
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .ownership
                            .clone()
                            .ok_or(EventDeliveryError::InvalidTransportData)?;
                        let association = ownership
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .observed_child(
                                &child.owner().thread_id(),
                                &child
                                    .turn_id()
                                    .map_err(|_| EventDeliveryError::InvalidTransportData)?,
                            )?;
                        let authority = host
                            .observed_authorities
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .get(&association.parent().invocation_id)
                            .cloned()
                            .ok_or(EventDeliveryError::InvalidTransportData)?;
                        Some(authority.context_for(&host, &association)?)
                    } else {
                        None
                    }
                } else {
                    None
                };
                let additional = match invocation {
                    Some(invocation) => invocation.observe(&observation.event),
                    None => match observed_context {
                        Some(context) => context
                            .skill_observer()?
                            .map_or_else(
                                || Ok(Vec::new()),
                                |observer| observer.additional_events(&scope, &observation.event),
                            )
                            .map(|events| {
                                events
                                    .into_iter()
                                    .map(|event| super::HarnessEvent::Host {
                                        scope: scope.clone(),
                                        event,
                                    })
                                    .collect()
                            }),
                        None => Ok(Vec::new()),
                    },
                };
                let additional = match additional {
                    Ok(events) => events,
                    Err(error) => {
                        let _ = sender.send(Err(error.clone()));
                        return retire_failed_observer(
                            &native,
                            &binding,
                            &retired,
                            &retiring,
                            &collection,
                            error.into(),
                        )
                        .await;
                    }
                };
                if let ProviderEvent::Turn {
                    turn_id,
                    event: forge_provider_api::TurnEvent::Completed(_),
                    ..
                } = &observation.event
                {
                    for invocation in host.invocations.lock().await.values() {
                        invocation
                            .retire_observed(None, Some(turn_id))
                            .await
                            .map_err(|_| EventDeliveryError::InvalidTransportData)?;
                    }
                    if let Some(context) = host
                        .observed_contexts
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .get(turn_id)
                    {
                        context
                            .retire()
                            .map_err(|_| EventDeliveryError::InvalidTransportData)?;
                    }
                }
                if let ProviderEvent::Agent {
                    agent_id,
                    event:
                        forge_provider_api::agent::AgentEvent::Lifecycle {
                            event: forge_provider_api::agent::AgentLifecycleEvent::Finished { .. },
                        },
                } = &observation.event
                {
                    let child_thread = {
                        let ownership = host
                            .observation
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .ownership
                            .clone()
                            .ok_or(EventDeliveryError::InvalidTransportData)?;
                        let proof = ownership
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .observed_agent(agent_id, None)?;
                        proof.thread_id().cloned()
                    };
                    if let Some(thread) = child_thread {
                        for invocation in host.invocations.lock().await.values() {
                            invocation
                                .retire_observed(Some(&thread), None)
                                .await
                                .map_err(|_| EventDeliveryError::InvalidTransportData)?;
                        }
                        let ownership = host
                            .observation
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner)
                            .ownership
                            .clone()
                            .ok_or(EventDeliveryError::InvalidTransportData)?;
                        let contexts = host
                            .observed_contexts
                            .lock()
                            .unwrap_or_else(std::sync::PoisonError::into_inner);
                        for (turn, context) in contexts.iter() {
                            if ownership
                                .lock()
                                .unwrap_or_else(std::sync::PoisonError::into_inner)
                                .observed_agent(agent_id, Some(turn))
                                .is_ok_and(|proof| proof.thread_id() == Some(&thread))
                            {
                                context
                                    .retire()
                                    .map_err(|_| EventDeliveryError::InvalidTransportData)?;
                            }
                        }
                    }
                }
                // Publish the position and its metadata only now, after every fallible step, so
                // the close watermark never names a position that was not handed to the owner.
                // Metadata is retained before the send, because the owner may consume at once.
                {
                    let metadata = binding_metadata(&observation.event, &owner);
                    let mut state = host
                        .observation
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner);
                    state.published = observation.sequence;
                    if let Some(metadata) = metadata {
                        state.pending.insert(observation.sequence, metadata);
                    }
                }
                if sender
                    .send(Ok(ProviderObservation {
                        event: super::session_execution::OwnerObservation::Event(
                            super::events::HarnessEvent::Native {
                                source: super::events::NativeEventSource {
                                    binding_id: binding.clone(),
                                    sequence: observation.sequence,
                                },
                                scope: scope.clone(),
                                event: Box::new(observation.event),
                            },
                        ),
                        native: Some((binding.clone(), observation.sequence)),
                    }))
                    .is_err()
                {
                    return retire_failed_observer(
                        &native,
                        &binding,
                        &retired,
                        &retiring,
                        &collection,
                        EventDeliveryError::Closed.into(),
                    )
                    .await;
                }
                host.observation
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .forwarded = observation.sequence;
                host.changed.notify_waiters();
                for event in additional {
                    if sender.send(Ok(ProviderObservation::host(event))).is_err() {
                        return retire_failed_observer(
                            &native,
                            &binding,
                            &retired,
                            &retiring,
                            &collection,
                            EventDeliveryError::Closed.into(),
                        )
                        .await;
                    }
                }
            }
            Ok(Some(_)) => {
                let failure = forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict;
                let error = EventDeliveryError::Observation(failure);
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(
                    &native,
                    &binding,
                    &retired,
                    &retiring,
                    &collection,
                    error.into(),
                )
                .await;
            }
            Ok(None) if retiring.load(std::sync::atomic::Ordering::Acquire) => return Ok(()),
            Ok(None) => {
                let error = EventDeliveryError::Closed;
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(
                    &native,
                    &binding,
                    &retired,
                    &retiring,
                    &collection,
                    error.into(),
                )
                .await;
            }
            Err(EventDeliveryError::Closed)
                if retiring.load(std::sync::atomic::Ordering::Acquire) =>
            {
                return Ok(());
            }
            Err(error) => {
                let _ = sender.send(Err(error.clone()));
                return retire_failed_observer(
                    &native,
                    &binding,
                    &retired,
                    &retiring,
                    &collection,
                    error.into(),
                )
                .await;
            }
        }
    }
}

/// Always attempt collection after fencing, retaining the original observation failure.
async fn retire_failed_observer(
    native: &Arc<dyn NativeProvider>,
    binding: &BindingId,
    retired: &Mutex<bool>,
    retiring: &std::sync::atomic::AtomicBool,
    collection: &BindingCollection,
    primary: ProviderError,
) -> Result<(), ProviderError> {
    if collection.collected() {
        return Ok(());
    }
    if collection.ambiguous(binding) {
        collection.wait().await;
        return Ok(());
    }
    retiring.store(true, std::sync::atomic::Ordering::Release);
    let mut errors = Vec::new();
    let mut retired = retired.lock().await;
    if !*retired {
        match native
            .thread_close(
                OperationControl::fresh(ProviderOperation::ThreadClose)?.context(),
                binding,
            )
            .await
        {
            Ok(()) => *retired = true,
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: errors,
        })
    }
}

/// Join retained host event producers before forgetting their invocation authority.
async fn collect_invocations(session: &HostSession) -> Result<(), ProviderError> {
    let invocations = session
        .invocations
        .lock()
        .await
        .values()
        .cloned()
        .collect::<Vec<_>>();
    collect_selected_invocations(invocations).await
}

/// Bound collection to the invocations whose foreground lifetime is being retired.
async fn collect_selected_invocations(
    invocations: Vec<Arc<super::host_invocation::HostInvocation>>,
) -> Result<(), ProviderError> {
    let mut errors = Vec::new();
    for invocation in invocations {
        if invocation.collect().await.is_err() {
            errors.push(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Unknown,
            ));
        }
    }
    combined(errors)
}

/// Preserve the primary failure and every independent cleanup failure.
/// Consume evidence from the captured reader, never a replacement selected by owner name.
///
/// Configuration, settlement, and compaction need proof of consumption, so an owner that
/// refused the position or exited fails the wait.
async fn wait_for_watermark(
    host: &HostSession,
    watermark: forge_provider_api::EventSequence,
) -> Result<(), ProviderError> {
    match wait_for_consumption(host, watermark).await? {
        WatermarkProgress::Consumed => Ok(()),
        WatermarkProgress::Abandoned => Err(ProviderError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::Disconnected,
        )),
    }
}

/// Wait until the owner consumes through `watermark` or can no longer do so.
///
/// The wait ends as [`WatermarkProgress::Abandoned`] when the owner refused a position at or
/// past the watermark, or its receiver is gone. Without that, a close holding provider
/// admission would wait forever on a fenced or exited owner.
async fn wait_for_consumption(
    host: &HostSession,
    watermark: forge_provider_api::EventSequence,
) -> Result<WatermarkProgress, ProviderError> {
    loop {
        let notified = host.changed.notified();
        tokio::pin!(notified);
        notified.as_mut().enable();
        {
            let state = host
                .observation
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if state.consumed >= watermark {
                return Ok(WatermarkProgress::Consumed);
            }
            if state.closed && state.published < watermark {
                return Err(ProviderError::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                ));
            }
            if state.rejected >= watermark || host.sender.is_closed() {
                return Ok(WatermarkProgress::Abandoned);
            }
        }
        tokio::select! {
            () = &mut notified => {}
            () = host.sender.closed() => {}
        }
    }
}

/// Distinguish owner consumption through a watermark from an owner that will never reach it.
enum WatermarkProgress {
    Consumed,
    Abandoned,
}

fn combined(errors: Vec<ProviderError>) -> Result<(), ProviderError> {
    let mut errors = errors.into_iter();
    match errors.next() {
        None => Ok(()),
        Some(primary) => {
            let secondary = errors.collect::<Vec<_>>();
            if secondary.is_empty() {
                Err(primary)
            } else {
                Err(ProviderError::Combined {
                    primary: Box::new(primary),
                    secondary,
                })
            }
        }
    }
}
