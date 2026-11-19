use super::matcher::PermissionRequest;
use crate::provider::ScopeOwner;
use crate::provider::approval::{
    ApprovalAuthority, ApprovalResolution, PermissionCoordinator, protected_target,
};
use crate::provider::{HarnessEventSink, ProviderError};
use crate::session::ApprovalMode;
use anyhow::Result;
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionResponse, ProviderPermissions};
use std::sync::{Arc, Mutex as StandardMutex};

#[derive(Clone)]
struct PermissionState {
    task_authority: Option<crate::tools::TaskAuthority>,
    checkpoint: Option<crate::checkpoint::ToolGate>,
    owner: Option<crate::provider::ThreadOwner>,
    approval_mode: ApprovalMode,
    execution_kind: crate::provider::ApprovalExecutionKind,
    lsp_mutations: bool,
    /// Reject every native tool, command, and file request, as Background generation requires.
    tool_free: bool,
    event_sink: Option<HarnessEventSink>,
    inheritance: Option<crate::provider::approval::PermissionInheritance>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum PermissionOrigin {
    Ordinary,
    SessionStorage,
}

/// Resolve independently admitted native-child callbacks through the binding's trusted registry.
#[async_trait]
pub(crate) trait ObservedChildAuthority: Send + Sync {
    /// Get the retained child context only after validating binding, parent relation, and original turn.
    async fn context(
        &self,
        parent: &crate::provider::InvocationScope,
        child: &crate::provider::TurnAddress,
    ) -> std::result::Result<Arc<ProviderPermissionContext>, ProviderError>;
}

/// Own the authorization and checkpoint context used by native provider callbacks.
pub(crate) struct ProviderPermissionContext {
    coordinator: Arc<PermissionCoordinator>,
    state: StandardMutex<PermissionState>,
    provider_fence: std::sync::OnceLock<Arc<std::sync::atomic::AtomicBool>>,
    retired: tokio::sync::watch::Sender<bool>,
    approval_authority: Arc<ApprovalAuthority>,
    observed_authority: StandardMutex<Option<Arc<dyn ObservedChildAuthority>>>,
    inherited_lease: StandardMutex<Option<crate::provider::approval::InheritedPermissionLease>>,
}

impl ProviderPermissionContext {
    /// Check immutable invocation routing before or after an asynchronous permission decision.
    fn matches_scope(&self, scope: &forge_provider_api::InvocationScope) -> bool {
        !*self.retired.borrow()
            && self
                .provider_fence
                .get()
                .is_none_or(|fence| !fence.load(std::sync::atomic::Ordering::Acquire))
            && self.state.lock().is_ok_and(|state| {
                state
                    .event_sink
                    .as_ref()
                    .and_then(|sink| sink.scope().invocation())
                    .is_some_and(|active| active == scope)
            })
    }

    /// Create an inactive callback context bound to the shared permission coordinator.
    pub(crate) fn new(coordinator: Arc<PermissionCoordinator>) -> Self {
        Self {
            coordinator,
            provider_fence: std::sync::OnceLock::new(),
            retired: tokio::sync::watch::channel(false).0,
            approval_authority: Arc::new(ApprovalAuthority::default()),
            observed_authority: StandardMutex::new(None),
            inherited_lease: StandardMutex::new(None),
            state: StandardMutex::new(PermissionState {
                task_authority: None,
                checkpoint: None,
                owner: None,
                approval_mode: ApprovalMode::Ask,
                execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                lsp_mutations: false,
                tool_free: false,
                event_sink: None,
                inheritance: None,
            }),
        }
    }

    /// Reject every later request from this invocation, including those of its native children.
    ///
    /// Background generation returns text only, so no provider tool may run on its behalf,
    /// whatever saved rules, approval mode, or access scope would otherwise allow.
    pub(crate) fn deny_all_tools(&self) -> Result<()> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("permission state lock poisoned"))?
            .tool_free = true;
        Ok(())
    }

    /// Bind the exact provider quarantine latch before admitting native callbacks.
    pub(crate) fn bind_provider_fence(
        &self,
        fence: Arc<std::sync::atomic::AtomicBool>,
    ) -> Result<()> {
        self.provider_fence
            .set(fence)
            .map_err(|_| anyhow::anyhow!("provider permission fence already installed"))
    }

    /// Install the binding-owned native-child authority without replacing an existing owner.
    pub(crate) fn bind_observed_authority(
        &self,
        authority: Arc<dyn ObservedChildAuthority>,
    ) -> Result<()> {
        let mut current = self
            .observed_authority
            .lock()
            .map_err(|_| anyhow::anyhow!("native child authority lock poisoned"))?;
        anyhow::ensure!(
            current.is_none(),
            "native child authority already installed"
        );
        *current = Some(authority);
        Ok(())
    }

    /// Create a distinct child policy context after the binding registry proves its original owner.
    pub(crate) fn fork_observed(
        &self,
        association: &crate::provider::ObservedAgentAssociation,
        sink: HarnessEventSink,
    ) -> std::result::Result<Arc<Self>, ProviderError> {
        let rejected = || {
            ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Authorization,
            ))
        };
        let scope = association.turn().cloned().ok_or_else(rejected)?;
        let address = crate::provider::TurnAddress {
            thread_id: association.thread_id().cloned().ok_or_else(rejected)?,
            turn_id: scope.turn_id().map_err(|_| rejected())?,
            scope,
        };
        let mut state = self.state.lock().map_err(|_| rejected())?.clone();
        let parent = state
            .event_sink
            .as_ref()
            .and_then(|sink| sink.scope().invocation())
            .ok_or_else(rejected)?;
        if parent != association.parent()
            || parent.owner() == address.scope.owner()
            || parent.owner().session_id() != address.scope.owner().session_id()
            || sink.scope().invocation() != Some(&address.scope)
            || address.thread_id != address.scope.owner().thread_id()
        {
            return Err(rejected());
        }
        let captured = state.inheritance.as_ref().ok_or_else(rejected)?;
        let inherited = self
            .coordinator
            .inherit(&address.scope, captured)
            .map_err(|error| {
                crate::provider::lifecycle_error::classify(
                    forge_provider_api::ProviderOperation::TurnStart,
                    error,
                )
            })?;
        state.owner = Some(address.scope.owner().clone());
        state.execution_kind = crate::provider::ApprovalExecutionKind::NativeObserved;
        state.event_sink = Some(sink);
        Ok(Arc::new(Self {
            coordinator: self.coordinator.clone(),
            state: StandardMutex::new(state),
            provider_fence: self.provider_fence.clone(),
            retired: tokio::sync::watch::channel(false).0,
            approval_authority: Arc::new(ApprovalAuthority::default()),
            observed_authority: StandardMutex::new(
                self.observed_authority
                    .lock()
                    .map_err(|_| rejected())?
                    .clone(),
            ),
            inherited_lease: StandardMutex::new(Some(inherited)),
        }))
    }

    /// Get the independently retained child's selected-skill attribution observer.
    pub(crate) fn skill_observer(
        &self,
    ) -> std::result::Result<Option<Arc<dyn crate::provider::events::EventObserver>>, ProviderError>
    {
        Ok(self
            .inherited_lease
            .lock()
            .map_err(|_| ProviderError::Cancelled)?
            .as_ref()
            .and_then(crate::provider::approval::InheritedPermissionLease::skill_observer))
    }

    /// Get the host-proven original child context while keeping root matching exact.
    async fn observed_context(
        &self,
        parent: &crate::provider::InvocationScope,
        child: &crate::provider::TurnAddress,
    ) -> std::result::Result<Arc<Self>, ProviderError> {
        let authority = self
            .observed_authority
            .lock()
            .map_err(|_| ProviderError::Cancelled)?
            .clone()
            .ok_or(ProviderError::Cancelled)?;
        authority.context(parent, child).await
    }

    /// Bind callbacks to the current invocation while retaining its native session directory.
    pub(crate) fn configure(
        &self,
        owner: crate::provider::ThreadOwner,
        approval_mode: ApprovalMode,
        _workspace: String,
        event_sink: Option<HarnessEventSink>,
    ) -> Result<()> {
        anyhow::ensure!(
            !*self.retired.borrow(),
            "provider permission context is retired"
        );
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?;
        let inheritance = event_sink
            .as_ref()
            .and_then(|sink| sink.scope().invocation())
            .map(|scope| self.coordinator.capture_inheritance(scope))
            .transpose()?;
        self.coordinator
            .record_approval_mode(&owner, approval_mode)?;
        *state = PermissionState {
            task_authority: None,
            checkpoint: None,
            owner: Some(owner),
            approval_mode,
            execution_kind: crate::provider::ApprovalExecutionKind::Managed,
            lsp_mutations: false,
            tool_free: false,
            event_sink,
            inheritance,
        };
        Ok(())
    }
}

impl ProviderPermissionContext {
    /// Revoke this native generation before disconnecting its callback transport.
    pub(crate) fn retire(&self) -> Result<()> {
        self.retired.send_replace(true);
        self.approval_authority.retire()?;
        self.inherited_lease
            .lock()
            .map_err(|_| anyhow::anyhow!("native child permission lease lock poisoned"))?
            .take();
        Ok(())
    }

    /// Capture checkpoint and task authority before native callbacks can run.
    pub(crate) fn bind_context(
        &self,
        context: Option<&crate::tools::ControlTurnContext>,
    ) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?;
        state.checkpoint = context.and_then(|context| context.checkpoint.clone());
        state.lsp_mutations = context.is_some_and(|context| {
            crate::tools::lsp::allowed_mode(context.mode)
                && !context.question_discussion
                && context.lsp.as_ref().is_some_and(|scope| scope.available())
        });
        state.task_authority = context.and_then(crate::tools::TaskAuthority::capture);
        Ok(())
    }
}

#[async_trait]
impl ProviderPermissions for ProviderPermissionContext {
    /// Resolve permission only through the binding-proven independent child context.
    async fn authorize_observed_child(
        &self,
        parent: &crate::provider::InvocationScope,
        child: &crate::provider::TurnAddress,
        request: PermissionRequest,
    ) -> PermissionResponse {
        match self.observed_context(parent, child).await {
            Ok(context) => {
                context
                    .authorize_request(&child.scope, request, PermissionOrigin::Ordinary)
                    .await
            }
            Err(_) => {
                Denial::unavailable("Native child permission ownership is unavailable or retired.")
                    .response(None)
            }
        }
    }

    /// Apply the captured execution barrier under the child's original authority.
    async fn before_observed_child(
        &self,
        parent: &crate::provider::InvocationScope,
        child: &crate::provider::TurnAddress,
    ) -> std::result::Result<(), ProviderError> {
        self.observed_context(parent, child)
            .await?
            .before_tool(&child.scope)
            .await
    }

    async fn before_tool(
        &self,
        scope: &forge_provider_api::InvocationScope,
    ) -> std::result::Result<(), ProviderError> {
        if !self.matches_scope(scope) {
            return Err(ProviderError::Cancelled);
        }
        let checkpoint = self
            .state
            .lock()
            .map(|state| state.checkpoint.clone())
            .map_err(|_| {
                ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ))
            })?;
        if let Some(gate) = checkpoint {
            let mut retired = self.retired.subscribe();
            let result = tokio::select! {
                biased;
                _ = retired.wait_for(|retired| *retired) => return Err(ProviderError::Cancelled),
                result = gate.wait() => result,
            };
            result.map_err(|_| {
                ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Rejected,
                ))
            })?;
        }
        if !self.matches_scope(scope) {
            return Err(ProviderError::Cancelled);
        }
        Ok(())
    }

    async fn authorize(
        &self,
        scope: &forge_provider_api::InvocationScope,
        request: PermissionRequest,
    ) -> PermissionResponse {
        self.authorize_request(scope, request, PermissionOrigin::Ordinary)
            .await
    }

    async fn authorize_session_storage(
        &self,
        scope: &forge_provider_api::InvocationScope,
        request: PermissionRequest,
    ) -> PermissionResponse {
        self.authorize_request(scope, request, PermissionOrigin::SessionStorage)
            .await
    }
}

impl ProviderPermissionContext {
    /// Answer one request, recording any refusal Harness makes before the coordinator sees it.
    async fn authorize_request(
        &self,
        scope: &forge_provider_api::InvocationScope,
        request: PermissionRequest,
        origin: PermissionOrigin,
    ) -> PermissionResponse {
        let recorded = request.clone();
        match self.evaluate_request(scope, request, origin).await {
            RequestDecision::Respond(response) => response,
            RequestDecision::Refuse {
                denial,
                response,
                event_sink,
                execution_kind,
            } => {
                let reason = denial
                    .reason
                    .as_deref()
                    .unwrap_or("Harness policy rejected this tool call.");
                // A failed record leaves the provider's own denial on the row; the refusal stands.
                let _ = PermissionCoordinator::record_policy_denial(
                    event_sink.as_ref(),
                    &recorded,
                    reason,
                    execution_kind,
                )
                .await;
                response
            }
        }
    }

    async fn evaluate_request(
        &self,
        scope: &forge_provider_api::InvocationScope,
        request: PermissionRequest,
        origin: PermissionOrigin,
    ) -> RequestDecision {
        if !self.matches_scope(scope) {
            return RequestDecision::Respond(
                Denial::unavailable("Provider session observation was retired.").response(None),
            );
        }
        let (approval_mode, execution_kind, event_sink, owner, task_authority, tool_free) =
            match self.state.lock() {
                Ok(state) => (
                    state.approval_mode,
                    state.execution_kind,
                    state.event_sink.clone(),
                    state.owner.clone(),
                    state.task_authority.clone(),
                    state.tool_free,
                ),
                Err(_) => {
                    return RequestDecision::Respond(
                        Denial::unavailable("Harness permission context is unavailable.")
                            .response(None),
                    );
                }
            };
        // A mode change applies to callbacks already bound to this turn, so the coordinator's
        // current mode for the owner wins over the one captured when the turn started.
        let current_mode = || {
            owner
                .as_ref()
                .and_then(|owner| self.coordinator.approval_mode(owner).ok().flatten())
                .unwrap_or(approval_mode)
        };
        let approval_mode = current_mode();
        let refuse = |denial: Denial| RequestDecision::Refuse {
            response: denial.response(Some(approval_mode)),
            denial,
            event_sink: event_sink.clone(),
            execution_kind,
        };
        let mutating = origin != PermissionOrigin::SessionStorage
            && (request.target_list.is_empty()
                || request.target_list.iter().any(|target| {
                    use crate::permissions::matcher::PermissionTarget;
                    match target {
                        PermissionTarget::Read { .. } => false,
                        PermissionTarget::UnknownFile { operation } => {
                            *operation != crate::permissions::directory::DirectoryOperation::Read
                        }
                        PermissionTarget::HostTool { name } => !crate::tools::is_read_only(name),
                        PermissionTarget::Unknown
                        | PermissionTarget::Command { .. }
                        | PermissionTarget::Write { .. }
                        | PermissionTarget::Network { .. }
                        | PermissionTarget::Mcp { .. }
                        | PermissionTarget::Elevate { .. }
                        | PermissionTarget::Tool { .. } => true,
                    }
                }));
        if mutating
            && let Some(authority) = &task_authority
            && let Err(error) = authority.admit_mutation()
        {
            return refuse(Denial::policy(error.to_string()));
        }
        if tool_free {
            return refuse(Denial::policy(
                "Background generation has no tools. Return only the requested text from the supplied evidence.",
            ));
        }
        if let Some(authority) = &task_authority
            && let Err(error) = authority.validate_task_for_new_request()
        {
            return refuse(Denial::unavailable(format!(
                "Task authority changed before permission approval: {error:#}"
            )));
        }
        if let Some(writer) = request.target_list.iter().find_map(|target| match target {
            crate::permissions::matcher::PermissionTarget::Command { command, shell } => {
                crate::permissions::command::shell_file_writer(command, shell.unwrap_or_default())
            }
            _ => None,
        }) {
            return refuse(Denial::shell_writer(writer));
        }
        if request.target_list.iter().any(|target| {
            matches!(
                target,
                crate::permissions::matcher::PermissionTarget::Write { .. }
                    | crate::permissions::matcher::PermissionTarget::UnknownFile {
                        operation: crate::permissions::directory::DirectoryOperation::Write
                    }
            )
        }) {
            match self.coordinator.writes_disabled(Some(scope)) {
                Ok(true) => return refuse(Denial::writes_off()),
                Ok(false) => {}
                Err(error) => {
                    return refuse(Denial::unavailable(format!(
                        "Harness permission evaluation failed: {error:#}"
                    )));
                }
            }
        }
        let protected = match self.coordinator.store().read() {
            Ok(store) => protected_target(&request, &store),
            Err(_) => {
                return refuse(Denial::unavailable(
                    "Harness permission store is unavailable.",
                ));
            }
        };
        if protected {
            return refuse(Denial::policy("The target is protected by Harness."));
        }
        // Rename owns path authorization after the server computes the complete edit set.
        // Never open a generic host-tool approval before that fail-closed check.
        let is_rename = request.target_list.len() == 1
            && matches!(&request.target_list[0],
            crate::permissions::matcher::PermissionTarget::HostTool { name } if crate::tools::lsp::is_mutation(name));
        if is_rename && self.state.lock().is_ok_and(|state| state.lsp_mutations) {
            let evaluation = self
                .coordinator
                .store()
                .read()
                .ok()
                .and_then(|store| store.compiled().evaluate(&request).ok());
            return match evaluation {
                Some(evaluation)
                    if evaluation.decision
                        == crate::permissions::document::PermissionDecision::Allow
                        || (evaluation.decision
                            == crate::permissions::document::PermissionDecision::Ask
                            && evaluation.matched_pattern_list.is_empty()) =>
                {
                    RequestDecision::Respond(PermissionResponse::Allow)
                }
                _ => refuse(Denial::policy(
                    "LSP rename is not allowed by the saved tool policy.",
                )),
            };
        }
        if origin == PermissionOrigin::SessionStorage {
            if request.target_list.is_empty()
                || request.target_list.iter().any(|target| {
                    !matches!(
                        target,
                        crate::permissions::matcher::PermissionTarget::Read { .. }
                            | crate::permissions::matcher::PermissionTarget::Write { .. }
                    )
                })
            {
                return refuse(Denial::policy(
                    "Invalid session storage permission request.",
                ));
            }
            let evaluation = self
                .coordinator
                .store()
                .read()
                .map_err(|_| anyhow::anyhow!("permission store unavailable"))
                .and_then(|store| {
                    store
                        .compiled()
                        .evaluate(&request)
                        .map_err(anyhow::Error::from)
                });
            return match evaluation {
                Ok(evaluation)
                    if evaluation.decision
                        != crate::permissions::document::PermissionDecision::Deny =>
                {
                    if !self.matches_scope(scope) {
                        refuse(Denial::unavailable(
                            "Provider session observation was retired.",
                        ))
                    } else {
                        RequestDecision::Respond(PermissionResponse::Allow)
                    }
                }
                Ok(_) => refuse(Denial::policy(
                    "Harness permission policy rejects this session file.",
                )),
                Err(error) => refuse(Denial::unavailable(format!(
                    "Harness permission evaluation failed: {error:#}"
                ))),
            };
        }
        let approval = self.coordinator.authorize_scoped(
            approval_mode,
            request,
            None,
            event_sink.as_ref(),
            Some(self.approval_authority.clone()),
            execution_kind,
        );
        let deny = |denial: Denial| RequestDecision::Respond(denial.response(Some(approval_mode)));
        tokio::pin!(approval);
        let mut retired = self.retired.subscribe();
        let resolution = tokio::select! {
            resolution = &mut approval => resolution,
            _ = async { let _ = retired.wait_for(|retired| *retired).await; } => {
                let _ = self.coordinator.cancel_owner(&scope.owner(), event_sink.as_ref()).await;
                return deny(Denial::unavailable("Provider session observation was retired while approval was pending."));
            }
        };
        match resolution {
            Ok(
                ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad,
            ) => {
                if !self.matches_scope(scope) {
                    return deny(Denial::unavailable(
                        "Provider session observation was retired while approval was pending.",
                    ));
                }
                if let Some(authority) = &task_authority
                    && let Err(error) = authority.validate_scope()
                {
                    return deny(Denial::unavailable(format!(
                        "Task authority changed while permission approval was pending: {error:#}"
                    )));
                }
                if mutating
                    && let Some(authority) = &task_authority
                    && let Err(error) = authority.admit_mutation()
                {
                    return deny(Denial::policy(error.to_string()));
                }
                RequestDecision::Respond(PermissionResponse::Allow)
            }
            Ok(ApprovalResolution::TimedOut) => deny(Denial::timeout()),
            Ok(ApprovalResolution::DenyPolicy) => deny(Denial::policy(
                "A saved permission rule or Harness access policy rejects this operation.",
            )),
            Ok(ApprovalResolution::DenyMode) => {
                RequestDecision::Respond(Denial::mode().response(Some(current_mode())))
            }
            Ok(
                ApprovalResolution::DenyOnce
                | ApprovalResolution::DenyExact
                | ApprovalResolution::DenyBroad,
            ) => deny(Denial::user()),
            Ok(ApprovalResolution::Cancel) => {
                RequestDecision::Respond(PermissionResponse::Cancelled)
            }
            Err(error) => deny(Denial::unavailable(format!(
                "Harness permission evaluation failed: {error:#}"
            ))),
        }
    }
}

/// Separate refusals Harness made on its own from answers the coordinator already published.
enum RequestDecision {
    /// The coordinator recorded this answer, or no stream exists to record it on.
    Respond(PermissionResponse),
    /// Harness refused before the coordinator evaluated the request, so nothing recorded it yet.
    Refuse {
        denial: Denial,
        response: PermissionResponse,
        event_sink: Option<HarnessEventSink>,
        execution_kind: crate::provider::ApprovalExecutionKind,
    },
}

/// Name who refused a request, so a model never reads Harness's own decision as the user's.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DenialSource {
    /// Saved rules, access scopes, or another Harness policy refused the request unreviewed.
    HarnessPolicy,
    /// The user rejected the request in the approval popup.
    User,
    /// The approval expired before the user answered.
    ApprovalTimeout,
    /// The Deny approval mode refused a request that needed confirmation.
    ApprovalMode,
    /// Harness could not evaluate the request and refused it rather than guess.
    Unavailable,
}

/// State what the model should do next, which depends on whether a permitted route exists.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DenialNext {
    /// Retrying or rerouting the operation is the risk, so forbid both.
    DoNotRetry,
    /// File edits have a permitted route; direct the model to it instead of forbidding retries.
    UseFileEditingTool,
}

/// Explain one refusal to the model: who decided, why, and what to do next.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Denial {
    source: DenialSource,
    reason: Option<String>,
    next: DenialNext,
}

impl Denial {
    fn policy(reason: impl Into<String>) -> Self {
        Self {
            source: DenialSource::HarnessPolicy,
            reason: Some(reason.into()),
            next: DenialNext::DoNotRetry,
        }
    }

    fn unavailable(reason: impl Into<String>) -> Self {
        Self {
            source: DenialSource::Unavailable,
            reason: Some(reason.into()),
            next: DenialNext::DoNotRetry,
        }
    }

    fn user() -> Self {
        Self {
            source: DenialSource::User,
            reason: None,
            next: DenialNext::DoNotRetry,
        }
    }

    fn timeout() -> Self {
        Self {
            source: DenialSource::ApprovalTimeout,
            reason: None,
            next: DenialNext::DoNotRetry,
        }
    }

    fn mode() -> Self {
        Self {
            source: DenialSource::ApprovalMode,
            reason: None,
            next: DenialNext::DoNotRetry,
        }
    }

    /// Refuse a shell command that writes files, because only a file-editing tool reports the
    /// concrete path that write scopes and rules can authorize.
    fn shell_writer(writer: &str) -> Self {
        Self {
            source: DenialSource::HarnessPolicy,
            reason: Some(format!(
                "`{writer}` writes files through the shell, where Harness cannot check the target path."
            )),
            next: DenialNext::UseFileEditingTool,
        }
    }

    fn writes_off() -> Self {
        Self::policy(
            "Filesystem writing is off for this session. Tell the user the change needs writes; they can enable them with /write.",
        )
    }

    /// Render the model-facing text for the approval mode in force.
    fn message(&self, approval_mode: Option<ApprovalMode>) -> String {
        let actor = match self.source {
            DenialSource::HarnessPolicy => {
                "Harness policy rejected this tool call; the user did not review or reject it. The requested operation did not run."
            }
            DenialSource::User => {
                "The user rejected this tool call. The requested operation did not run."
            }
            DenialSource::ApprovalTimeout => {
                "The approval request expired before the user answered, so Harness rejected it; the user did not reject it. The requested operation did not run and is not approved."
            }
            DenialSource::ApprovalMode => {
                "The current approval mode denies requests that need confirmation; the user did not review this one. The requested operation did not run."
            }
            DenialSource::Unavailable => {
                "Harness could not evaluate this tool call and rejected it. The requested operation did not run."
            }
        };
        let mut parts = vec![actor.to_owned()];
        if let Some(reason) = &self.reason {
            parts.push(format!("Reason: {reason}"));
        }
        match self.next {
            DenialNext::DoNotRetry => {
                parts.push(
                    "Do not retry or bypass this denial, including through another tool. Continue with permitted actions, or explain the blocker to the user."
                        .into(),
                );
                if approval_mode == Some(ApprovalMode::Deny) {
                    parts.push(forge_provider_api::forge::REJECT_ALL_GUIDANCE.into());
                }
            }
            DenialNext::UseFileEditingTool => parts.push(
                "Do not write files from the shell. Make this edit now with your file-editing tool, such as apply_patch."
                    .into(),
            ),
        }
        parts.join(" ")
    }

    /// Deny execution with feedback that the provider can return to the model.
    fn response(&self, approval_mode: Option<ApprovalMode>) -> PermissionResponse {
        PermissionResponse::Deny {
            reason: Some(self.message(approval_mode)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A shell write is refused by Harness policy, and the model is sent to its file-editing tool.
    ///
    /// The refusal used to read as a user rejection followed by "Do not retry or bypass this
    /// denial", so a model told to use another tool in the same breath stopped and reported that
    /// the user had rejected the edit.
    #[test]
    fn shell_writer_denial_names_policy_and_directs_the_edit_to_the_file_tool() {
        let message = Denial::shell_writer("Set-Content").message(Some(ApprovalMode::Ask));
        assert!(message.contains("Harness policy rejected this tool call"));
        assert!(message.contains("the user did not review or reject it"));
        assert!(message.contains("did not run"));
        assert!(message.contains("Do not write files from the shell."));
        assert!(message.contains("Make this edit now with your file-editing tool"));
        assert!(!message.contains("Do not retry or bypass"), "{message}");
        assert!(!message.contains("rejected by the user"), "{message}");
        // Deny mode's reject-all guidance forbids working around restrictions, which would
        // contradict the permitted route.
        let under_deny = Denial::shell_writer("Set-Content").message(Some(ApprovalMode::Deny));
        assert!(!under_deny.contains(forge_provider_api::forge::REJECT_ALL_GUIDANCE));
    }

    /// Writes-off names Harness policy and `/write`, and forbids rerouting the write.
    #[test]
    fn writes_off_denial_names_policy_and_the_write_command() {
        let message = Denial::writes_off().message(Some(ApprovalMode::Ask));
        assert!(message.contains("Harness policy rejected this tool call"));
        assert!(message.contains("writing is off"));
        assert!(message.contains("/write"));
        assert!(message.contains("Do not retry or bypass this denial"));
    }

    /// Only the user's own rejection is attributed to the user.
    #[test]
    fn only_user_rejections_name_the_user_as_the_actor() {
        let user = Denial::user().message(None);
        assert!(user.starts_with("The user rejected this tool call."));
        for denial in [
            Denial::policy("rule"),
            Denial::timeout(),
            Denial::mode(),
            Denial::unavailable("store"),
        ] {
            let message = denial.message(None);
            assert!(!message.starts_with("The user rejected"), "{message}");
        }
        let deny_mode = Denial::mode().message(Some(ApprovalMode::Deny));
        assert!(deny_mode.contains("current approval mode denies"));
        assert!(deny_mode.contains(forge_provider_api::forge::REJECT_ALL_GUIDANCE));
    }

    /// Standing Allow policy cannot reopen writes after terminal intent, while reads remain available.
    #[tokio::test]
    async fn terminal_seal_rejects_allowed_native_writes_and_preserves_reads() -> anyhow::Result<()>
    {
        terminal_seal_permission_case(false).await
    }

    /// An approval captured before terminal intent cannot reopen mutation admission afterward.
    #[tokio::test]
    async fn pending_native_approval_cannot_reopen_terminal_seal() -> anyhow::Result<()> {
        terminal_seal_permission_case(true).await
    }

    async fn terminal_seal_permission_case(approval_wait: bool) -> anyhow::Result<()> {
        let (root, _store, controller) = crate::task::SessionTaskController::test_goal();
        let coordinator =
            PermissionCoordinator::load(root.path().join("permissions.json"), root.path())?;
        {
            let store = coordinator.store();
            let mut store = store.write().unwrap();
            let mut document = crate::permissions::document::default_permission_document();
            crate::permissions::document::set_permission_rule(
                &mut document,
                "edit",
                "*",
                crate::permissions::document::PermissionDecision::Ask,
            )?;
            store.save(&crate::permissions::document::serialize_permission_document(&document)?)?;
        }
        let owner = crate::provider::ThreadOwner::root("session");
        let scope = forge_provider_api::InvocationScope::new(owner.clone(), "terminal-turn");
        let mut tools =
            crate::tools::ControlTurnContext::inactive(crate::provider::PromptMode::Chat);
        tools.bind_test_task_controller(controller);
        let provider_request = crate::provider::ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: root.path().display().to_string(),
            input: crate::provider::ProviderInput::from_user_text("Test mutation admission"),
            mode: crate::provider::PromptMode::Chat,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: crate::session::AccessPolicy {
                sandbox: false,
                ..Default::default()
            },
            approval_mode: ApprovalMode::Allow,
            provider_session_id: None,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
            child_agent: None,
        };
        let _directories = coordinator.directories(None, &provider_request, &scope)?;
        let context = ProviderPermissionContext::new(coordinator.clone());
        let (events, _receiver) = crate::provider::events::channel(scope.clone());
        context.configure(
            owner.clone(),
            ApprovalMode::Allow,
            root.path().display().to_string(),
            Some(events.clone()),
        )?;
        context.bind_context(Some(&tools))?;
        let request = |write| PermissionRequest {
            id: "file-request".into(),
            provider: forge_providers::registry::nonselectable_id().unwrap(),
            subject: forge_provider_api::permissions::PermissionSubject::Unattached,
            reason: None,
            target_list: vec![if write {
                crate::permissions::matcher::PermissionTarget::Write {
                    path: root.path().join("file.rs").display().to_string(),
                }
            } else {
                crate::permissions::matcher::PermissionTarget::Read {
                    path: root.path().join("file.rs").display().to_string(),
                }
            }],
        };
        let before = context.authorize(&scope, request(true)).await;
        assert!(matches!(before, PermissionResponse::Allow), "{before:?}");
        if approval_wait {
            context.configure(
                owner,
                ApprovalMode::Ask,
                root.path().display().to_string(),
                Some(events),
            )?;
            context.bind_context(Some(&tools))?;
            let pending = context.authorize(
                &scope,
                PermissionRequest {
                    target_list: vec![crate::permissions::matcher::PermissionTarget::Write {
                        path: root
                            .path()
                            .parent()
                            .unwrap()
                            .join("approval-only.rs")
                            .display()
                            .to_string(),
                    }],
                    ..request(true)
                },
            );
            tokio::pin!(pending);
            let approval = tokio::select! {
                response = &mut pending => panic!("permission resolved before approval: {response:?}"),
                approval = tokio::time::timeout(std::time::Duration::from_secs(5), async {
                    loop {
                        if let Some(approval) = coordinator.pending_list()?.into_iter().next() {
                            break anyhow::Ok(approval);
                        }
                        tokio::task::yield_now().await;
                    }
                }) => approval??,
            };
            crate::tools::TaskAuthority::capture(&tools)
                .unwrap()
                .seal_mutations(|| Ok(()))?;
            let answers = approval
                .item_list
                .iter()
                .map(|item| crate::provider::approval::ApprovalAnswer {
                    item_id: item.id.clone(),
                    choice_id: item
                        .choice_list
                        .iter()
                        .find(|choice| choice.id == "allow_once")
                        .expect("one-time approval available")
                        .id
                        .clone(),
                })
                .collect::<Vec<_>>();
            coordinator.resolve(&approval.id, &answers, None).await?;
            assert!(matches!(pending.await, PermissionResponse::Deny { .. }));
        } else {
            crate::tools::TaskAuthority::capture(&tools)
                .unwrap()
                .seal_mutations(|| Ok(()))?;
        }
        assert!(matches!(
            context.authorize(&scope, request(true)).await,
            PermissionResponse::Deny { .. }
        ));
        assert!(matches!(
            context.authorize(&scope, request(false)).await,
            PermissionResponse::Allow
        ));
        Ok(())
    }

    /// Reject every Background request with the tool-free reason, regardless of Allow mode.
    #[tokio::test]
    async fn tool_free_context_rejects_requests_that_policy_allows() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let coordinator =
            PermissionCoordinator::load(root.path().join("permissions.json"), root.path())?;
        let owner = crate::provider::ThreadOwner::root("background-session");
        let scope = forge_provider_api::InvocationScope::new(owner.clone(), "background-turn");
        let request = || forge_provider_api::permissions::PermissionRequest {
            id: "read".into(),
            provider: forge_providers::registry::nonselectable_id().unwrap(),
            subject: forge_provider_api::permissions::PermissionSubject::Unattached,
            reason: None,
            target_list: vec![forge_provider_api::permissions::PermissionTarget::Read {
                path: root.path().join("file.rs").to_string_lossy().into_owned(),
            }],
        };
        let context = ProviderPermissionContext::new(coordinator);
        context.configure(
            owner,
            ApprovalMode::Allow,
            root.path().display().to_string(),
            Some(crate::provider::events::channel(scope.clone()).0),
        )?;
        let background_denial = |response: PermissionResponse| {
            matches!(response, PermissionResponse::Deny { reason: Some(reason) }
                if reason.contains("Background generation has no tools"))
        };
        assert!(!background_denial(
            context.authorize(&scope, request()).await
        ));
        context.deny_all_tools()?;
        assert!(background_denial(
            context.authorize(&scope, request()).await
        ));
        Ok(())
    }
}
