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

pub(crate) const REJECT_ALL_GUIDANCE: &str = "Tool approval is set to reject all, and no user is reviewing requests. Commands and tools that saved permission rules do not allow are rejected automatically. Continue with the allowed tools and commands. Do not wait for approval, retry rejected operations, or work around the restrictions.";

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
            Err(_) => denied(
                "Native child permission ownership is unavailable or retired.",
                None,
            ),
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
    async fn authorize_request(
        &self,
        scope: &forge_provider_api::InvocationScope,
        request: PermissionRequest,
        origin: PermissionOrigin,
    ) -> PermissionResponse {
        if !self.matches_scope(scope) {
            return denied("Provider session observation was retired.", None);
        }
        let (approval_mode, execution_kind, event_sink, _owner, task_authority, tool_free) =
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
                    return denied(
                        "Automatically rejected because Harness permission context is unavailable.",
                        None,
                    );
                }
            };
        let deny = |reason: &str| denied(reason, Some(approval_mode));
        if tool_free {
            return deny(
                "Automatically rejected because Background generation has no tools. Return only the requested text from the supplied evidence.",
            );
        }
        if let Some(authority) = &task_authority
            && let Err(error) = authority.validate_task_for_new_request()
        {
            return deny(&format!(
                "Task authority changed before permission approval: {error:#}"
            ));
        }
        if let Some(writer) = request.target_list.iter().find_map(|target| match target {
            crate::permissions::matcher::PermissionTarget::Command { command, shell } => {
                crate::permissions::command::shell_file_writer(command, shell.unwrap_or_default())
            }
            _ => None,
        }) {
            return deny(&format!(
                "Automatically rejected because `{writer}` writes files through the shell. Create and edit files with your file-editing tool, such as apply_patch, instead of shell commands."
            ));
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
                Ok(true) => {
                    return deny(
                        "Automatically rejected because filesystem writing is off. The user can enable it with /write.",
                    );
                }
                Ok(false) => {}
                Err(error) => {
                    return deny(&format!(
                        "Automatically rejected because Harness permission evaluation failed: {error:#}"
                    ));
                }
            }
        }
        let protected = match self.coordinator.store().read() {
            Ok(store) => protected_target(&request, &store),
            Err(_) => {
                return deny(
                    "Automatically rejected because Harness permission store is unavailable.",
                );
            }
        };
        if protected {
            return deny("Automatically rejected because the target is protected by Harness.");
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
                    PermissionResponse::Allow
                }
                _ => deny("LSP rename is not allowed by the saved tool policy."),
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
                return deny("Invalid session storage permission request.");
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
                        deny("Provider session observation was retired.")
                    } else {
                        PermissionResponse::Allow
                    }
                }
                Ok(_) => deny(
                    "Automatically rejected by Harness permission policy for this session file.",
                ),
                Err(error) => deny(&format!(
                    "Automatically rejected because Harness permission evaluation failed: {error:#}"
                )),
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
        tokio::pin!(approval);
        let mut retired = self.retired.subscribe();
        let resolution = tokio::select! {
            resolution = &mut approval => resolution,
            _ = async { let _ = retired.wait_for(|retired| *retired).await; } => {
                let _ = self.coordinator.cancel_owner(&scope.owner(), event_sink.as_ref()).await;
                return deny("Provider session observation was retired while approval was pending.");
            }
        };
        match resolution {
            Ok(
                ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad,
            ) => {
                if !self.matches_scope(scope) {
                    return deny(
                        "Provider session observation was retired while approval was pending.",
                    );
                }
                if let Some(authority) = &task_authority
                    && let Err(error) = authority.validate_scope()
                {
                    return deny(&format!(
                        "Task authority changed while permission approval was pending: {error:#}"
                    ));
                }
                PermissionResponse::Allow
            }
            Ok(ApprovalResolution::TimedOut) => deny(
                "Automatically rejected because the approval timeout expired. The user may be unavailable.",
            ),
            Ok(ApprovalResolution::DenyPolicy) => {
                deny("Automatically rejected by Harness permission or filesystem access policy.")
            }
            Ok(ApprovalResolution::DenyMode) => deny(
                "Automatically rejected because the current approval mode denies requests that need confirmation.",
            ),
            Ok(
                ApprovalResolution::DenyOnce
                | ApprovalResolution::DenyExact
                | ApprovalResolution::DenyBroad,
            ) => deny("Rejected by the user."),
            Ok(ApprovalResolution::Cancel) => PermissionResponse::Cancelled,
            Err(error) => deny(&format!(
                "Automatically rejected because Harness permission evaluation failed: {error:#}"
            )),
        }
    }
}

/// Denies execution with feedback that the provider can return to the model.
fn denied(reason: &str, approval_mode: Option<ApprovalMode>) -> PermissionResponse {
    let guidance = if approval_mode == Some(ApprovalMode::Deny) {
        REJECT_ALL_GUIDANCE
    } else {
        ""
    };
    PermissionResponse::Deny {
        reason: Some(format!(
            "Tool call rejected. The requested operation was not executed. Reason: {reason} \
         Do not retry or bypass this denial. Continue with permitted actions, or explain the blocker to the user. {guidance}"
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
