use forge_provider_api::{
    AuthenticationMethodId, BindingId, CancelRequest, CompactionRequest, CompactionResult,
    ConfiguredSession, InputAcceptance, InputReconcileRequest, InputRetractOutcome,
    InputRetractRequest, InvocationSettlement, OpenedSession, OperationContext, OperationId,
    PromptRequestIdentity, ProviderError, ProviderForkResult, ProviderModel, ProviderOperation,
    ProviderSetup, SessionConfiguration, SessionForkRequest, SessionRequestIdentity,
    SteeringAcceptance, SteeringRequestIdentity, SubmissionEvidence,
    catalog::{McpDefinition, McpQuery, QueryContext, SkillDefinition, WorkspaceContext},
    retained_operation::{IdentityRegistry, Store},
    terminal::{TerminalId, TerminalSnapshot},
};
use std::{
    future::Future,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

/// Own every typed operation record under one cross-domain identity and admission registry.
pub(super) struct Records {
    /// Reject cross-domain operation identity reuse and stop ordinary provider admission.
    pub(super) registry: Arc<IdentityRegistry>,
    /// Retain fresh or resumed opens and transfer their sole stream once.
    pub(super) open: Store<SessionRequestIdentity, OpenedSession>,
    /// Retain complete configuration transactions for exact binding identities.
    pub(super) configure: Store<(BindingId, SessionConfiguration), ConfiguredSession>,
    /// Retain durable target disposition without creating another fork on retry.
    pub(super) fork: Store<SessionForkRequest, ProviderForkResult>,
    /// Retain correlated compaction evidence for one context and scope.
    pub(super) compact: Store<(BindingId, CompactionRequest), CompactionResult>,
    /// Retain complete binding collection and cleanup retry outcomes.
    pub(super) close: Store<BindingId, ()>,
    /// Retain exact invocation interruption and settlement outcomes.
    pub(super) cancel: Store<(BindingId, CancelRequest), InvocationSettlement>,
    /// Retain input acceptance with weak callback and lease identity metadata.
    pub(super) submit: Store<(BindingId, PromptRequestIdentity), InputAcceptance>,
    /// Retain exact active-invocation delivery with weak authority identity.
    pub(super) steer: Store<(BindingId, SteeringRequestIdentity), SteeringAcceptance>,
    /// Retain read-only durable delivery observations.
    pub(super) reconcile: Store<InputReconcileRequest, SubmissionEvidence>,
    /// Retain authorized destructive history mutation dispositions.
    pub(super) retract: Store<(BindingId, InputRetractRequest), InputRetractOutcome>,
    /// Retain normalized installation and credential discovery observations.
    pub(super) inspect: Store<WorkspaceContext, ProviderSetup>,
    /// Retain explicitly selected authentication method outcomes.
    pub(super) authenticate: Store<(WorkspaceContext, AuthenticationMethodId), ()>,
    /// Retain provider-wide collection without joining peer shutdown jobs recursively.
    pub(super) shutdown: Store<(), ()>,
    /// Retain read-only opaque model choices.
    pub(super) models: Store<QueryContext, Vec<ProviderModel>>,
    /// Retain read-only skill catalog observations.
    pub(super) skills: Store<QueryContext, Vec<SkillDefinition>>,
    /// Retain configured or observed MCP query results.
    pub(super) mcp: Store<McpQuery, Vec<McpDefinition>>,
    /// Retain binding-scoped shell status observations.
    pub(super) terminals: Store<BindingId, TerminalSnapshot>,
    /// Retain exact shell termination disposition independently of foreground work.
    pub(super) terminate: Store<(BindingId, TerminalId), ()>,
}

impl Records {
    /// Create the typed stores without starting native processes or admitting model work.
    pub(super) fn new() -> Self {
        let registry = Arc::new(IdentityRegistry::new());
        Self {
            open: Store::registered(registry.clone()),
            configure: Store::registered(registry.clone()),
            fork: Store::registered(registry.clone()),
            compact: Store::registered(registry.clone()),
            close: Store::registered(registry.clone()),
            cancel: Store::registered(registry.clone()),
            submit: Store::registered(registry.clone()),
            steer: Store::registered(registry.clone()),
            reconcile: Store::registered(registry.clone()),
            retract: Store::registered(registry.clone()),
            inspect: Store::registered(registry.clone()),
            authenticate: Store::registered(registry.clone()),
            shutdown: Store::registered(registry.clone()),
            models: Store::registered(registry.clone()),
            skills: Store::registered(registry.clone()),
            mcp: Store::registered(registry.clone()),
            terminals: Store::registered(registry.clone()),
            terminate: Store::registered(registry.clone()),
            registry,
        }
    }

    /// Join all typed native workers and cleanup except the shutdown job which owns this collection.
    pub(super) async fn collect_except(&self, id: &OperationId) -> Result<(), ProviderError> {
        let mut failures = Vec::new();
        macro_rules! collect { ($($store:ident),*) => { $(if let Err(error) = self.$store.join_all_except(id).await { failures.push(error); })* }; }
        collect!(
            open,
            configure,
            fork,
            compact,
            close,
            cancel,
            submit,
            steer,
            reconcile,
            retract,
            inspect,
            authenticate,
            models,
            skills,
            mcp,
            terminals,
            terminate
        );
        match failures.split_first() {
            None => Ok(()),
            Some((primary, [])) => Err(primary.clone()),
            Some((primary, secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary.clone()),
                secondary: secondary.to_vec(),
            }),
        }
    }
}

/// Propagate the host deadline only through one retained operation's native calls.
/// Spawned invocation/background readers intentionally own independent lifetimes after admission.
pub(super) struct NativeContext {
    /// Retain the immutable host deadline and explicit cancellation observer.
    pub(super) context: OperationContext,
    /// Attribute native failures to the semantic operation requested by the host.
    pub(super) operation: ProviderOperation,
    changed: AtomicBool,
}

impl NativeContext {
    /// Create the dispatch boundary used by interruption classification and cleanup ownership.
    pub(super) fn new(context: OperationContext, operation: ProviderOperation) -> Arc<Self> {
        Arc::new(Self {
            context,
            operation,
            changed: AtomicBool::new(false),
        })
    }
    /// Get whether native mutation or observer ownership handoff may have happened.
    pub(super) fn changed(&self) -> bool {
        self.changed.load(Ordering::Acquire)
    }
    /// Record the first possible mutation before crossing its native dispatch boundary.
    pub(super) fn mark_changed(&self) {
        self.changed.store(true, Ordering::Release);
    }
}

tokio::task_local! { static NATIVE_CONTEXT: Option<Arc<NativeContext>>; }

/// Scope a retained worker's native calls to its immutable host context.
pub(super) async fn scope<T>(context: Arc<NativeContext>, work: impl Future<Output = T>) -> T {
    NATIVE_CONTEXT.scope(Some(context), work).await
}

/// Collect interrupted native ownership independently of the operation's expired observation deadline.
pub(super) async fn cleanup<T>(work: impl Future<Output = T>) -> T {
    NATIVE_CONTEXT.scope(None, work).await
}

/// Get the current operation without allowing descendants to inherit its completion deadline implicitly.
pub(super) fn current() -> Option<Arc<NativeContext>> {
    NATIVE_CONTEXT.try_with(Clone::clone).ok().flatten()
}

/// Reject a new native mutation immediately before dispatch, without altering cached operation outcomes.
pub(super) fn check() -> Result<(), ProviderError> {
    match current() {
        Some(context) => context.context.check_admission(context.operation),
        None => Ok(()),
    }
}

/// Mark an adapter-local observer handoff as the point after which interruption requires invalidation.
pub(super) fn mark_changed() {
    if let Some(context) = current() {
        context.mark_changed();
    }
}
