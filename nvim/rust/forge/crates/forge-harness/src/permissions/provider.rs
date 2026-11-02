use super::matcher::PermissionRequest;
use super::session_directory::SessionDirectory;
use crate::provider::approval::{
    ApprovalAuthority, ApprovalResolution, PermissionCoordinator, protected_target,
};
use crate::provider::{ProviderError, ProviderEventSink};
use crate::session::ApprovalMode;
use anyhow::Result;
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionResponse, ProviderPermissions};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex as StandardMutex};

pub(crate) const REJECT_ALL_GUIDANCE: &str = "Tool approval is set to reject all, and no user is reviewing requests. Commands and tools that saved permission rules do not allow are rejected automatically. Continue with the allowed tools and commands. Do not wait for approval, retry rejected operations, or work around the restrictions.";

struct PermissionState {
    task_authority: Option<crate::tools::TaskAuthority>,
    cancelled: bool,
    checkpoint: Option<crate::checkpoint::ToolGate>,
    owner: Option<crate::provider::ExecutionOwner>,
    approval_mode: ApprovalMode,
    workspace: String,
    event_sink: Option<ProviderEventSink>,
    session_directory: Option<SessionDirectory>,
}

/// Own the authorization and checkpoint context used by native provider callbacks.
pub(crate) struct ProviderPermissionContext {
    coordinator: Arc<PermissionCoordinator>,
    state: StandardMutex<PermissionState>,
    retired: AtomicBool,
    approval_authority: Arc<ApprovalAuthority>,
}

impl ProviderPermissionContext {
    /// Create an inactive callback context bound to the shared permission coordinator.
    pub(crate) fn new(coordinator: Arc<PermissionCoordinator>) -> Self {
        Self {
            coordinator,
            retired: AtomicBool::new(false),
            approval_authority: Arc::new(ApprovalAuthority::default()),
            state: StandardMutex::new(PermissionState {
                task_authority: None,
                cancelled: false,
                checkpoint: None,
                owner: None,
                approval_mode: ApprovalMode::Ask,
                workspace: ".".into(),
                event_sink: None,
                session_directory: None,
            }),
        }
    }

    /// Bind callbacks to the current invocation while retaining its native session directory.
    pub(crate) fn configure(
        &self,
        owner: crate::provider::ExecutionOwner,
        approval_mode: ApprovalMode,
        workspace: String,
        event_sink: Option<ProviderEventSink>,
    ) -> Result<()> {
        anyhow::ensure!(
            !self.retired.load(Ordering::Acquire),
            "provider permission context is retired"
        );
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?;
        let session_directory = state.session_directory.take();
        *state = PermissionState {
            task_authority: None,
            cancelled: false,
            checkpoint: None,
            owner: Some(owner),
            approval_mode,
            workspace,
            event_sink,
            session_directory,
        };
        Ok(())
    }
}

impl ProviderPermissionContext {
    /// Revoke this native generation before disconnecting its callback transport.
    pub(crate) fn retire(&self) -> Result<()> {
        self.retired.store(true, Ordering::Release);
        self.approval_authority.retire()?;
        Ok(())
    }

    /// Install the directory reported by the native session for limited automatic access.
    pub(crate) fn set_session_directory(&self, directory: Option<SessionDirectory>) -> Result<()> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?
            .session_directory = directory;
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
        state.task_authority = context.and_then(crate::tools::TaskAuthority::capture);
        Ok(())
    }

    /// Get whether a permission decision cancelled the current invocation.
    pub(crate) fn cancelled(&self) -> Result<bool> {
        Ok(self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?
            .cancelled)
    }

    /// Simulate an explicit user cancellation in host integration fixtures.
    #[cfg(test)]
    pub(crate) fn cancel(&self) -> Result<()> {
        self.state
            .lock()
            .map_err(|_| anyhow::anyhow!("provider permission context lock poisoned"))?
            .cancelled = true;
        Ok(())
    }
}

#[async_trait]
impl ProviderPermissions for ProviderPermissionContext {
    async fn before_tool(&self) -> std::result::Result<(), ProviderError> {
        if self.retired.load(Ordering::Acquire) {
            return Err(ProviderError::Cancelled);
        }
        let checkpoint = self
            .state
            .lock()
            .map(|state| state.checkpoint.clone())
            .map_err(|_| anyhow::anyhow!("provider checkpoint context lock poisoned"))?;
        if let Some(gate) = checkpoint {
            gate.wait().await.map_err(ProviderError::from)?;
        }
        if self.retired.load(Ordering::Acquire) {
            return Err(ProviderError::Cancelled);
        }
        Ok(())
    }

    async fn authorize(&self, session_id: &str, request: PermissionRequest) -> PermissionResponse {
        if self.retired.load(Ordering::Acquire) {
            return denied("Provider session observation was retired.", None);
        }
        let (approval_mode, workspace, event_sink, owner, session_directory, task_authority) =
            match self.state.lock() {
                Ok(state) => (
                    state.approval_mode,
                    state.workspace.clone(),
                    state.event_sink.clone(),
                    state.owner.clone(),
                    state.session_directory.clone(),
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
            && let Err(error) = authority.validate()
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
            match self.coordinator.writes_disabled(owner.as_ref()) {
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
        if let Some(directory) = session_directory {
            match directory.approves(session_id, &request, std::path::Path::new(&workspace)) {
                Ok(true) => {
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
                            if self.retired.load(Ordering::Acquire) {
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
                Ok(false) => {}
                Err(error) => {
                    return deny(&format!(
                        "Automatically rejected because Harness session directory validation failed: {error:#}"
                    ));
                }
            }
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
                if self.retired.load(Ordering::Acquire) {
                    return deny(
                        "Provider session observation was retired while approval was pending.",
                    );
                }
                if let Some(authority) = &task_authority
                    && let Err(error) = authority.validate()
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
            Ok(ApprovalResolution::Cancel) => {
                match self.state.lock() {
                    Ok(mut state) => state.cancelled = true,
                    Err(_) => {
                        return deny(
                            "Automatically rejected because Harness permission context is unavailable.",
                        );
                    }
                }
                PermissionResponse::Cancelled
            }
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
