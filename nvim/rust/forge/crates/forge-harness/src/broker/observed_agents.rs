//! Route provider-observed children through their own proven histories.
use super::{AppliedProviderEvent, HarnessBroker, SessionUpdate};
use crate::provider::ScopeOwner;
use crate::provider::{EventScope, HarnessEvent, NativeEventSource, ObservedAgentAssociation};
use anyhow::{Context, Result};
use forge_provider_api::ProviderEvent;
use forge_provider_api::agent::{AgentEvent, AgentId};

impl HarnessBroker {
    /// Stop host display clocks only after the exact native generation was actually fenced or closed.
    pub(crate) fn observed_binding_retired(
        &mut self,
        binding: &forge_provider_api::BindingId,
    ) -> Result<super::PendingProviderDelivery> {
        self.store.mark_observed_binding_unavailable(binding)?;
        let patch = self.reconcile_timeline()?;
        self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))
    }

    /// End observation of the native children a failed exchange delegated to.
    ///
    /// A failed turn reports nothing more for its children, so a child still active would keep
    /// its live clock forever. Freezing it keeps its last native state and evidence.
    pub(crate) fn retire_observed_children_of(&mut self, exchange_id: &str) -> Result<()> {
        let records = self.store.list_observed_agents(&self.session.id)?;
        for record in records.iter().filter(|record| {
            record.parent_scope().exchange_id.as_str() == exchange_id
                && record.availability() == crate::agent::ObservedAgentAvailability::Available
                && !matches!(
                    record.state(),
                    crate::agent::ObservedAgentState::Finished(_)
                )
        }) {
            self.store.mark_observed_agent_unavailable(
                record.key().binding_id(),
                record.key().agent_id(),
            )?;
        }
        Ok(())
    }

    /// Retain the actual native provider generation before managed-parent resources can disappear.
    fn observed_agent_proof(
        &mut self,
        source: &NativeEventSource,
        scope: &EventScope,
        agent_id: &AgentId,
    ) -> Result<ObservedAgentAssociation> {
        if let Some(provider) = self.native_providers_by_binding.get(&source.binding_id) {
            return Ok(provider.observed_agent(&source.binding_id, agent_id, None)?);
        }
        let provider = if scope.owner().is_root() {
            self.provider.clone()
        } else {
            self.child_provider_by_agent
                .get(scope.owner().agent_id().as_str())
                .cloned()
                .context("native agent binding has no original managed provider owner")?
        };
        let proof = provider.observed_agent(&source.binding_id, agent_id, None)?;
        self.native_providers_by_binding
            .insert(source.binding_id.clone(), provider);
        Ok(proof)
    }

    /// Persist a trusted native lifecycle without creating managed task authority.
    ///
    /// A lifecycle the host cannot place ends observation of that one child; only a storage
    /// failure fails the observation, because a native child is not the delegating turn.
    pub(super) fn apply_agent_lifecycle(
        &mut self,
        source: &NativeEventSource,
        scope: &EventScope,
        agent_id: &AgentId,
        event: &AgentEvent,
    ) -> Result<AppliedProviderEvent> {
        match self.place_agent_lifecycle(source, scope, agent_id, event) {
            Ok(applied) => Ok(applied),
            Err(error) => self.reject_observed_child(Some(source), Some(agent_id), error),
        }
    }

    /// Persist one native lifecycle under its retained registry proof.
    fn place_agent_lifecycle(
        &mut self,
        source: &NativeEventSource,
        scope: &EventScope,
        agent_id: &AgentId,
        event: &AgentEvent,
    ) -> Result<AppliedProviderEvent> {
        let proof = self.observed_agent_proof(source, scope, agent_id)?;
        anyhow::ensure!(
            Some(proof.parent()) == scope.invocation(),
            "native agent lifecycle crosses original parent"
        );
        self.store
            .observe_native_agent(&proof, source, event, self.clock.now_ms())?;
        self.trace.record(
            &self.session.id,
            "agent.native_observed",
            serde_json::json!({"source":source,"scope":scope,"agent_id":agent_id,"event":event}),
        );
        Ok(AppliedProviderEvent::Timeline(self.reconcile_timeline()?))
    }

    /// Encode idle observed-child publications after their independent state has committed.
    pub(super) fn observe_child_session_event(
        &mut self,
        observation: &HarnessEvent,
    ) -> Result<Vec<super::SessionEvent>> {
        let mut pending = vec![self.apply_observed_child(observation)?];
        let mut events = Vec::new();
        while let Some(effect) = pending.pop() {
            match effect {
                AppliedProviderEvent::None => {}
                AppliedProviderEvent::Timeline(patch) => {
                    if !patch.is_empty() {
                        events.push(self.event("document_changed", patch.notification())?);
                    }
                }
                AppliedProviderEvent::Update(update) => events.push(self.event(
                    "provider_event",
                    super::publication::encode_session_update(&update)?,
                )?),
                AppliedProviderEvent::Effects(effects) => pending.extend(effects.into_iter().rev()),
                AppliedProviderEvent::Live(publication) => {
                    if !publication.timeline.is_empty() {
                        events.push(
                            self.event("document_changed", publication.timeline.notification())?,
                        );
                    }
                    events.push(self.event(
                        "provider_event",
                        super::publication::encode_session_update(&publication.update)?,
                    )?);
                }
            }
        }
        Ok(events)
    }

    /// Commit a binding-owned child's exact physical effect independently of parent collection.
    pub(super) fn apply_observed_file_change(
        &mut self,
        binding: &forge_provider_api::BindingId,
        scope: &crate::provider::InvocationScope,
        change: &crate::tools::lsp::HostFileChange,
    ) -> Result<AppliedProviderEvent> {
        anyhow::ensure!(
            scope == &change.scope,
            "observed host file effect crosses original scope"
        );
        self.record_observed_host_file_change(Some(binding), change)?;
        Ok(AppliedProviderEvent::Timeline(self.reconcile_timeline()?))
    }

    /// Attribute a committed host mutation to a registry-proven native child without a native event.
    pub(super) fn record_observed_host_file_change(
        &mut self,
        binding: Option<&forge_provider_api::BindingId>,
        change: &crate::tools::lsp::HostFileChange,
    ) -> Result<()> {
        anyhow::ensure!(
            change.scope.owner().session_id().as_str() == self.session.id,
            "native child host file effect crosses host session ownership"
        );
        let records = self.store.list_observed_agents(&self.session.id)?;
        let mut matches = records.iter().filter(|record| {
            binding.is_none_or(|binding| record.key().binding_id() == binding)
                && (record.owner() == Some(&change.scope.owner())
                    || record.thread_id() == Some(&change.scope.owner().thread_id()))
                && record.parent_scope().exchange_id == change.scope.exchange_id
        });
        let record = matches
            .next()
            .context("native child host file effect has no original proven owner")?;
        anyhow::ensure!(
            matches.next().is_none(),
            "native child file effect has ambiguous binding ownership"
        );
        let provider = self
            .native_providers_by_binding
            .get(record.key().binding_id())
            .context("native child file effect lost its original provider generation")?;
        let proof = provider.observed_agent(
            record.key().binding_id(),
            record.key().agent_id(),
            Some(&change.scope.turn_id()?),
        )?;
        self.store
            .observe_native_agent_file_change(&proof, change, self.clock.now_ms())?;
        change.recorded()?;
        Ok(())
    }

    /// Persist a native child's own item, turn, thread, or host-policy state under retained proof.
    ///
    /// Evidence the host cannot place ends observation of its child only. A storage failure
    /// still fails the observation, because no later evidence could be committed either.
    pub(super) fn apply_observed_child(
        &mut self,
        observation: &HarnessEvent,
    ) -> Result<AppliedProviderEvent> {
        match self.place_observed_child(observation) {
            Ok(applied) => {
                if let HarnessEvent::Native { source, .. } = observation {
                    self.trace.record(
                        &self.session.id,
                        "agent.native_placed",
                        serde_json::json!({
                            "native_sequence": source.sequence.get(),
                            "kind": observation_kind(observation),
                        }),
                    );
                }
                Ok(applied)
            }
            Err(error) => {
                let (source, agent_id) = match observation {
                    HarnessEvent::Native { source, event, .. } => (
                        Some(source),
                        match event.as_ref() {
                            ProviderEvent::Agent { agent_id, .. } => Some(agent_id),
                            _ => None,
                        },
                    ),
                    HarnessEvent::Host { .. } => (None, None),
                };
                let scope_agent = observation.scope().owner();
                let agent_id =
                    agent_id.or_else(|| (!scope_agent.is_root()).then(|| scope_agent.agent_id()));
                let error = error.context(format!("refused {}", observation_kind(observation)));
                self.reject_observed_child(source, agent_id, error)
            }
        }
    }

    /// End observation of the one child whose evidence could not be placed, leaving the
    /// delegating turn and the child's siblings running.
    fn reject_observed_child(
        &mut self,
        source: Option<&NativeEventSource>,
        agent_id: Option<&AgentId>,
        error: anyhow::Error,
    ) -> Result<AppliedProviderEvent> {
        if crate::exchange::storage_failure(&error) {
            return Err(error);
        }
        let marked = match agent_id {
            Some(agent_id) => {
                let records = self.store.list_observed_agents(&self.session.id)?;
                let binding = records
                    .iter()
                    .find(|record| {
                        record.key().agent_id() == agent_id
                            && source.is_none_or(|source| {
                                record.key().binding_id() == &source.binding_id
                            })
                    })
                    .map(|record| record.key().binding_id().clone());
                match binding {
                    Some(binding) => self
                        .store
                        .mark_observed_agent_unavailable(&binding, agent_id)?
                        .is_some(),
                    None => false,
                }
            }
            None => false,
        };
        self.trace.record(
            &self.session.id,
            "agent.native_rejected",
            serde_json::json!({
                "agent_id":agent_id,
                "error_code":"observed_child_unplaced",
                "placement_failure":format!("{error:#}"),
                "outcome":if marked { "marked_unavailable" } else { "dropped" },
            }),
        );
        Ok(AppliedProviderEvent::Timeline(self.reconcile_timeline()?))
    }

    /// Persist one native child observation under its retained registry proof.
    fn place_observed_child(&mut self, observation: &HarnessEvent) -> Result<AppliedProviderEvent> {
        let scope = observation.scope();
        if let HarnessEvent::Native {
            source,
            event: boxed_native_event,
            ..
        } = observation
            && let ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Unavailable(_),
            } = boxed_native_event.as_ref()
        {
            self.store
                .mark_observed_binding_unavailable(&source.binding_id)?;
            let mut effects = vec![AppliedProviderEvent::Timeline(self.reconcile_timeline()?)];
            if let Some(update) = SessionUpdate::from_observation(observation) {
                effects.push(AppliedProviderEvent::Update(update));
            }
            return Ok(AppliedProviderEvent::Effects(effects));
        }
        anyhow::ensure!(
            scope.owner().session_id().as_str() == self.session.id,
            "observed child crosses host session ownership"
        );
        let mut access_update = None;
        match observation {
            HarnessEvent::Native {
                source,
                scope,
                event,
            } => {
                if let ProviderEvent::Agent { agent_id, event } = event.as_ref() {
                    return self.apply_agent_lifecycle(source, scope, agent_id, event);
                }
                let provider = self
                    .native_providers_by_binding
                    .get(&source.binding_id)
                    .cloned()
                    .context("observed child has no retained original provider generation")?;
                let proof = match event.as_ref() {
                    ProviderEvent::Turn {
                        thread_id, turn_id, ..
                    }
                    | ProviderEvent::Item {
                        thread_id, turn_id, ..
                    } => provider.observed_child(&source.binding_id, thread_id, turn_id)?,
                    ProviderEvent::Thread { thread_id, .. } => {
                        let records = self.store.list_observed_agents(&self.session.id)?;
                        let record = records
                            .iter()
                            .find(|record| {
                                record.key().binding_id() == &source.binding_id
                                    && record.thread_id() == Some(thread_id)
                            })
                            .context("native child thread has no proven original agent")?;
                        provider.observed_agent(
                            &source.binding_id,
                            record.key().agent_id(),
                            None,
                        )?
                    }
                    ProviderEvent::Provider { .. } => {
                        return Ok(SessionUpdate::from_observation(observation)
                            .map_or(AppliedProviderEvent::None, AppliedProviderEvent::Update));
                    }
                    ProviderEvent::Agent { .. } => {
                        unreachable!("native agent lifecycle routed before child content")
                    }
                };
                self.store.observe_native_agent_event(
                    &proof,
                    source,
                    scope,
                    event,
                    self.clock.now_ms(),
                )?;
            }
            HarnessEvent::Host { scope, event } => {
                let invocation = scope
                    .invocation()
                    .context("observed child host policy has no original invocation")?;
                let records = self.store.list_observed_agents(&self.session.id)?;
                let mut matches = records.iter().filter(|record| {
                    (record.owner() == Some(&invocation.owner())
                        || record.thread_id() == Some(&invocation.owner().thread_id()))
                        && record.parent_scope().exchange_id == invocation.exchange_id
                });
                let record = matches
                    .next()
                    .context("observed child host policy has no proven original history")?;
                anyhow::ensure!(
                    matches.next().is_none(),
                    "observed child host policy has ambiguous binding ownership"
                );
                let provider = self
                    .native_providers_by_binding
                    .get(record.key().binding_id())
                    .context("observed child host policy lost original provider generation")?;
                let proof = provider.observed_agent(
                    record.key().binding_id(),
                    record.key().agent_id(),
                    Some(&invocation.turn_id()?),
                )?;
                self.store.observe_native_agent_host_event(
                    &proof,
                    scope,
                    event,
                    self.clock.now_ms(),
                )?;
                if let crate::provider::HostEvent::DirectoryAccessGranted(grant) = event {
                    access_update = Some(self.apply_directory_access(vec![grant.grant.clone()])?);
                }
            }
        }
        let mut effects = Vec::new();
        if let Some(update) = access_update {
            effects.push(AppliedProviderEvent::Update(update));
        }
        if let Some(update) = SessionUpdate::from_observation(observation) {
            effects.push(AppliedProviderEvent::Update(update));
        }
        effects.push(AppliedProviderEvent::Timeline(self.reconcile_timeline()?));
        Ok(AppliedProviderEvent::Effects(effects))
    }
}

/// Name an observation by its nested event kinds alone, never its content.
fn observation_kind(observation: &HarnessEvent) -> String {
    let encoded = match observation {
        HarnessEvent::Native { event, .. } => serde_json::to_value(event.as_ref()),
        HarnessEvent::Host { event, .. } => serde_json::to_value(event),
    }
    .unwrap_or_default();
    let mut kinds = Vec::new();
    let mut level = Some(&encoded);
    while let Some(current) = level {
        if let Some(kind) = current.get("kind").and_then(serde_json::Value::as_str) {
            kinds.push(kind.to_owned());
        }
        level = current.get("event").filter(|inner| inner.is_object());
    }
    kinds.join(".")
}
