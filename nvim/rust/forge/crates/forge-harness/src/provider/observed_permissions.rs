//! Retain native-child policy independently of its delegating foreground lifetime.
use super::HostSession;
use forge_provider_api::{BindingId, ProviderError};
use std::sync::{Arc, Weak};

/// Validate native child registrations and inherit independently owned callback policy.
#[derive(Clone)]
pub(crate) struct ObservedPermissionAuthority {
    binding: BindingId,
    root: forge_provider_api::InvocationScope,
    template: Weak<crate::permissions::provider::ProviderPermissionContext>,
    host: Weak<HostSession>,
    sink: crate::provider::events::HarnessEventSink,
}
impl ObservedPermissionAuthority {
    /// Create the original binding's independently retained child-policy authority.
    pub(super) fn new(
        binding: BindingId,
        root: forge_provider_api::InvocationScope,
        template: Weak<crate::permissions::provider::ProviderPermissionContext>,
        host: Weak<HostSession>,
        sink: crate::provider::events::HarnessEventSink,
    ) -> Self {
        Self {
            binding,
            root,
            template,
            host,
            sink,
        }
    }

    /// Check original root provenance for replay safety without selecting any current invocation.
    pub(crate) fn owns_parent(&self, scope: &forge_provider_api::InvocationScope) -> bool {
        if scope == &self.root {
            return true;
        }
        self.host.upgrade().is_some_and(|host| {
            host.observed_authorities
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .get(&scope.invocation_id)
                .is_some_and(|authority| authority.root == self.root)
        })
    }
    /// Get proven original ancestors in declaration order without selecting a current child turn.
    pub(crate) fn ancestry(
        &self,
        association: &crate::provider::ObservedAgentAssociation,
    ) -> Result<Vec<crate::provider::ObservedAgentAssociation>, ProviderError> {
        let rejected = || {
            ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
        };
        let host = self.host.upgrade().ok_or_else(rejected)?;
        let ownership = host
            .observation
            .lock()
            .map_err(|_| rejected())?
            .ownership
            .clone()
            .ok_or_else(rejected)?;
        let ownership = ownership.lock().map_err(|_| rejected())?;
        let mut chain = vec![association.clone()];
        let mut seen = std::collections::HashSet::new();
        while chain.last().ok_or_else(rejected)?.parent() != &self.root {
            let parent = chain.last().ok_or_else(rejected)?.parent();
            if !seen.insert(parent.invocation_id.clone()) {
                return Err(rejected());
            }
            chain.push(ownership.observed_child(
                &parent.owner.thread_id(),
                &parent.turn_id().map_err(|_| rejected())?,
            )?);
        }
        chain.reverse();
        Ok(chain)
    }
    /// Create a direct original-generation sink only for a proof admitted by this authority.
    pub(crate) fn tool_sink(
        &self,
        association: &crate::provider::ObservedAgentAssociation,
    ) -> Result<crate::provider::HarnessEventSink, ProviderError> {
        let host = self.host.upgrade().ok_or(ProviderError::Cancelled)?;
        let scope = association.turn().ok_or(ProviderError::Cancelled)?;
        Ok(self
            .sink
            .observed_invocation_sink(scope, self.binding.clone(), host.sender.clone())?)
    }
    /// Resolve the original child policy and binding-owned publication capability before host tool admission.
    pub(crate) async fn resolve(
        &self,
        parent: &forge_provider_api::InvocationScope,
        child: &forge_provider_api::TurnAddress,
    ) -> Result<
        (
            crate::provider::ObservedAgentAssociation,
            crate::provider::HarnessEventSink,
        ),
        ProviderError,
    > {
        use crate::permissions::provider::ObservedChildAuthority;
        use forge_provider_api::permissions::ProviderPermissions;
        self.context(parent, child)
            .await?
            .before_tool(&child.scope)
            .await?;
        let rejected = || {
            ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
        };
        let host = self.host.upgrade().ok_or_else(rejected)?;
        let ownership = host
            .observation
            .lock()
            .map_err(|_| rejected())?
            .ownership
            .clone()
            .ok_or_else(rejected)?;
        let proof = ownership
            .lock()
            .map_err(|_| rejected())?
            .validate_child(parent, child)?;
        let sink = self.sink.observed_invocation_sink(
            &child.scope,
            self.binding.clone(),
            host.sender.clone(),
        )?;
        Ok((proof, sink))
    }
    /// Create or reuse independently retained policy after exact native child registration is proven.
    pub(super) fn context_for(
        &self,
        host: &Arc<HostSession>,
        association: &crate::provider::ObservedAgentAssociation,
    ) -> Result<Arc<crate::permissions::provider::ProviderPermissionContext>, ProviderError> {
        let rejected = || {
            ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
        };
        let scope = association.turn().cloned().ok_or_else(rejected)?;
        let turn_id = scope.turn_id().map_err(|_| rejected())?;
        let parent = association.parent();
        if parent != &self.root
            && !host
                .observed_authorities
                .lock()
                .map_err(|_| rejected())?
                .get(&parent.invocation_id)
                .is_some_and(|authority| authority.root == self.root)
        {
            return Err(rejected());
        }
        let mut contexts = host.observed_contexts.lock().map_err(|_| rejected())?;
        if let Some(context) = contexts.get(&turn_id) {
            return Ok(context.clone());
        }
        let template = if parent == &self.root {
            self.template.upgrade().ok_or_else(rejected)?
        } else {
            contexts
                .get(&parent.turn_id().map_err(|_| rejected())?)
                .cloned()
                .ok_or_else(rejected)?
        };
        let sink = self.sink.observed_invocation_sink(
            &scope,
            self.binding.clone(),
            host.sender.clone(),
        )?;
        let mut authorities = host.observed_authorities.lock().map_err(|_| rejected())?;
        let context = template.fork_observed(association, sink)?;
        contexts.insert(turn_id, context.clone());
        authorities.insert(scope.invocation_id.clone(), Arc::new(self.clone()));
        Ok(context)
    }
}

#[async_trait::async_trait]
impl crate::permissions::provider::ObservedChildAuthority for ObservedPermissionAuthority {
    /// Resolve positively registered child authority after its preceding native parent relation is forwarded.
    async fn context(
        &self,
        parent: &forge_provider_api::InvocationScope,
        child: &forge_provider_api::TurnAddress,
    ) -> Result<Arc<crate::permissions::provider::ProviderPermissionContext>, ProviderError> {
        let rejected = || {
            ProviderError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
        };
        // Validate the exact original native registration before policy inheritance.
        let host = self.host.upgrade().ok_or_else(rejected)?;
        let bound = host.initialized.get().ok_or_else(rejected)?;
        if host
            .provider_closed
            .load(std::sync::atomic::Ordering::Acquire)
            || bound.binding.id != self.binding
            || host.closing.load(std::sync::atomic::Ordering::Acquire)
            || bound.retiring.load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(rejected());
        }
        if bound.registrations.registered_turn(&child.turn_id).as_ref() != Some(child) {
            return Err(rejected());
        }
        // Wait until earlier native parent relations enter original owner delivery.
        let boundary = bound.registrations.watermark();
        loop {
            let changed = host.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let forwarded = {
                let observation = host
                    .observation
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if observation.closed || host.closing.load(std::sync::atomic::Ordering::Acquire) {
                    return Err(rejected());
                }
                observation.forwarded >= boundary
            };
            if forwarded {
                break;
            }
            changed.await;
        }
        let ownership = host
            .observation
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .ownership
            .clone()
            .ok_or_else(rejected)?;
        // Commit only a child relation proven by the same original binding registry.
        let association = {
            let mut ownership = ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            ownership.admit_observed(child.clone())?;
            ownership.validate_child(parent, child)?
        };
        self.context_for(&host, &association)
    }
}
