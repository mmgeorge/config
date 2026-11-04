use super::matcher::PermissionRequest;
use crate::provider::approval::{
    ApprovalAuthority, ApprovalResolution, PermissionCoordinator, protected_target,
};
use crate::provider::{ProviderError, ProviderEventSink};
use crate::session::ApprovalMode;
use anyhow::Result;
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionResponse, ProviderPermissions};
use std::sync::{Arc, Mutex as StandardMutex};

pub(crate) const REJECT_ALL_GUIDANCE: &str = "Tool approval is set to reject all, and no user is reviewing requests. Commands and tools that saved permission rules do not allow are rejected automatically. Continue with the allowed tools and commands. Do not wait for approval, retry rejected operations, or work around the restrictions.";

struct PermissionState {
    task_authority: Option<crate::tools::TaskAuthority>,
    checkpoint: Option<crate::checkpoint::ToolGate>,
    owner: Option<crate::provider::ExecutionOwner>,
    approval_mode: ApprovalMode,
    lsp_mutations: bool,
    event_sink: Option<ProviderEventSink>,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum PermissionOrigin {
    Ordinary,
    SessionStorage,
}

/// Own the authorization and checkpoint context used by native provider callbacks.
pub(crate) struct ProviderPermissionContext {
    coordinator: Arc<PermissionCoordinator>,
    state: StandardMutex<PermissionState>,
    retired: tokio::sync::watch::Sender<bool>,
    approval_authority: Arc<ApprovalAuthority>,
}

impl ProviderPermissionContext {
    /// Check immutable invocation routing before or after an asynchronous permission decision.
    fn matches_scope(&self, scope: &forge_provider_api::InvocationScope) -> bool {
        !*self.retired.borrow()
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
            retired: tokio::sync::watch::channel(false).0,
            approval_authority: Arc::new(ApprovalAuthority::default()),
            state: StandardMutex::new(PermissionState {
                task_authority: None,
                checkpoint: None,
                owner: None,
                approval_mode: ApprovalMode::Ask,
                lsp_mutations: false,
                event_sink: None,
            }),
        }
    }

    /// Bind callbacks to the current invocation while retaining its native session directory.
    pub(crate) fn configure(
        &self,
        owner: crate::provider::ExecutionOwner,
        approval_mode: ApprovalMode,
        _workspace: String,
        event_sink: Option<ProviderEventSink>,
    ) -> Result<()> {
        anyhow::ensure!(
            !*self.retired.borrow(),
            "provider permission context is retired"
        );
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?;
        *state = PermissionState {
            task_authority: None,
            checkpoint: None,
            owner: Some(owner),
            approval_mode,
            lsp_mutations: false,
            event_sink,
        };
        Ok(())
    }
}

impl ProviderPermissionContext {
    /// Revoke this native generation before disconnecting its callback transport.
    pub(crate) fn retire(&self) -> Result<()> {
        self.retired.send_replace(true);
        self.approval_authority.retire()?;
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
                    forge_provider_api::ProviderOperation::HostEffectPublication,
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
                    forge_provider_api::ProviderOperation::HostEffectPublication,
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
        let (approval_mode, event_sink, _owner, task_authority) = match self.state.lock() {
            Ok(state) => (
                state.approval_mode,
                state.event_sink.clone(),
                state.owner.clone(),
                state.task_authority.clone(),
            ),
            Err(_) => {
                return denied(
                    "Automatically rejected because Harness permission context is unavailable.",
                    None,
                );
            }
        };
        let deny = |reason: &str| denied(reason, Some(approval_mode));
        if let Some(authority) = &task_authority
            && let Err(error) = authority.validate_scope()
        {
            return deny(&format!(
                "Task authority changed before permission approval: {error:#}"
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
        match self
            .coordinator
            .authorize_scoped(
                approval_mode,
                request,
                None,
                event_sink.as_ref(),
                Some(self.approval_authority.clone()),
            )
            .await
        {
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
    use forge_provider_api::permissions::PermissionTarget;

    #[tokio::test]
    async fn retirement_releases_a_callback_waiting_for_checkpoint() -> anyhow::Result<()> {
        let coordinator = PermissionCoordinator::transient(".")?;
        let context = ProviderPermissionContext::new(coordinator);
        let owner = crate::provider::ExecutionOwner::root("checkpoint-cancel");
        let scope = crate::provider::InvocationScope::new(owner.clone(), "exchange");
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        context.configure(owner, ApprovalMode::Ask, ".".into(), Some(sink))?;
        let gate = crate::checkpoint::ToolGate::new();
        context.state.lock().unwrap().checkpoint = Some(gate.clone());
        let mut waiting = Box::pin(context.before_tool(&scope));
        assert!(futures_util::poll!(&mut waiting).is_pending());
        context.retire()?;
        assert!(matches!(
            tokio::time::timeout(std::time::Duration::from_secs(1), waiting).await?,
            Err(ProviderError::Cancelled)
        ));
        assert!(!gate.ready());
        Ok(())
    }

    #[tokio::test]
    async fn session_storage_respects_explicit_denial_and_invocation_retirement()
    -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let coordinator = PermissionCoordinator::transient(root.path())?;
        let context = ProviderPermissionContext::new(coordinator.clone());
        let owner = crate::provider::ExecutionOwner::root("storage-policy");
        let scope = crate::provider::InvocationScope::new(owner.clone(), "exchange");
        let (sink, _events) = crate::provider::events::channel(scope.clone());
        context.configure(
            owner,
            ApprovalMode::Deny,
            root.path().display().to_string(),
            Some(sink),
        )?;
        let path = root.path().join("state.json").display().to_string();
        let request = PermissionRequest {
            id: "storage-read".into(),
            provider: forge_provider_api::ProviderId::new("fixture")?,
            reason: None,
            target_list: vec![PermissionTarget::Read { path: path.clone() }],
        };
        assert!(matches!(
            context
                .authorize_session_storage(&scope, request.clone())
                .await,
            PermissionResponse::Allow
        ));
        coordinator.store().write().unwrap().set_rule_list(&[(
            crate::permissions::document::CATEGORY_READ.into(),
            path,
            crate::permissions::document::PermissionDecision::Deny,
        )])?;
        assert!(matches!(
            context
                .authorize_session_storage(&scope, request.clone())
                .await,
            PermissionResponse::Deny { .. }
        ));
        context.retire()?;
        assert!(matches!(
            context.authorize_session_storage(&scope, request).await,
            PermissionResponse::Deny { .. }
        ));
        Ok(())
    }
}
