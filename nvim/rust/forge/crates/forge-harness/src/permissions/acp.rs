use crate::provider::approval::{ApprovalResolution, PermissionCoordinator, protected_target};
use crate::provider::{ProviderError, ProviderEventSink, TurnAddress};
use crate::session::ApprovalMode;
use crate::tools::TaskAuthority;
use async_trait::async_trait;
use forge_provider_api::permissions::{
    PermissionRequest, PermissionResponse, PermissionTarget, ScopedPermissions,
};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Clone)]
struct ActivePermission {
    generation: Arc<()>,
    mode: ApprovalMode,
    authority: Option<TaskAuthority>,
    sink: ProviderEventSink,
}

/// Retains Harness policy and task authority outside the native ACP session.
pub(crate) struct AcpPermissionContext {
    coordinator: Arc<PermissionCoordinator>,
    active: RwLock<Option<ActivePermission>>,
}

impl AcpPermissionContext {
    pub(crate) fn new(coordinator: Arc<PermissionCoordinator>) -> Self {
        Self {
            coordinator,
            active: RwLock::new(None),
        }
    }

    pub(crate) async fn configure(
        &self,
        mode: ApprovalMode,
        authority: Option<TaskAuthority>,
        sink: ProviderEventSink,
    ) {
        *self.active.write().await = Some(ActivePermission {
            generation: Arc::new(()),
            mode,
            authority,
            sink,
        });
    }

    pub(crate) async fn close(&self) {
        *self.active.write().await = None;
    }
}

#[async_trait]
impl ScopedPermissions for AcpPermissionContext {
    async fn authorize(
        &self,
        address: TurnAddress,
        request: PermissionRequest,
    ) -> Result<PermissionResponse, ProviderError> {
        let active = self
            .active
            .read()
            .await
            .clone()
            .ok_or_else(|| ProviderError::Rejected {
                message: "ACP permission request has no active turn".into(),
            })?;
        if active.sink.invocation_scope()? != &address.scope {
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            ));
        }
        let protected = {
            let store = self.coordinator.store();
            let store = store.read().map_err(|_| ProviderError::Rejected {
                message: "permission store lock poisoned".into(),
            })?;
            protected_target(&request, &store)
        };
        if protected {
            return Ok(PermissionResponse::Deny {
                reason: Some("Target is protected by Harness".into()),
            });
        }
        let authority = if address.scope.owner.is_root() {
            active.authority.as_ref()
        } else {
            None
        };
        if let Some(authority) = authority {
            authority.validate()?;
        }
        let internal = matches!(request.target_list.as_slice(), [PermissionTarget::Mcp { target }]
            if target.strip_prefix("forge-harness-control/").is_some_and(|name|
                crate::tools::ControlToolRegistry.definition_list().iter().any(|definition| definition.name == name)));
        let resolution = if internal {
            ApprovalResolution::AllowOnce
        } else {
            self.coordinator
                .authorize(active.mode, request, Some(address), Some(&active.sink))
                .await?
        };
        match resolution {
            ApprovalResolution::AllowOnce
            | ApprovalResolution::AllowExact
            | ApprovalResolution::AllowBroad => {
                if let Some(authority) = authority {
                    authority.validate()?;
                }
                if !self
                    .active
                    .read()
                    .await
                    .as_ref()
                    .is_some_and(|current| Arc::ptr_eq(&current.generation, &active.generation))
                {
                    return Ok(PermissionResponse::Cancelled);
                }
                Ok(PermissionResponse::Allow)
            }
            ApprovalResolution::Cancel => Ok(PermissionResponse::Cancelled),
            ApprovalResolution::DenyOnce
            | ApprovalResolution::DenyExact
            | ApprovalResolution::DenyBroad
            | ApprovalResolution::DenyPolicy
            | ApprovalResolution::DenyMode
            | ApprovalResolution::TimedOut => Ok(PermissionResponse::Deny { reason: None }),
        }
    }
}
