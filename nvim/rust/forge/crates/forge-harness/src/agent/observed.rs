//! Persist provider-observed children without granting managed task authority.
use crate::provider::ScopeOwner;

use crate::exchange::{Exchange, ExchangeOrigin, ExchangeOutcome};
use crate::provider::events::NativeEventSource;
use crate::provider::{
    EventScope, InvocationScope, ObservedAgentAssociation, ThreadOwner, TurnAddress,
};
use forge_provider_api::agent::{
    AgentActiveState, AgentDescription, AgentEvent, AgentId, AgentLifecycleEvent, AgentOutcome,
};
use forge_provider_api::{
    BindingId, EventSequence, ProviderEvent, ThreadEvent, ThreadId, TurnEvent,
};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Preserve a native child's observed state without creating a managed AgentTask.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum ObservedAgentState {
    /// No lifecycle summary accompanied the registered conversation metadata.
    NotObserved,
    /// Native evidence reports live work or a resumable wait.
    Active(AgentActiveState),
    /// Native evidence reports an end without certifying host-managed cleanup.
    Finished(AgentOutcome),
}

/// Preserve host observation availability independently of native execution outcome.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ObservedAgentAvailability {
    /// The original binding still supplies registered native observations.
    Available,
    /// Observation ended without replacing the last native lifecycle or terminal evidence.
    Unavailable,
}

/// Identify one observed native agent within its original connection generation.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ObservedAgentKey {
    binding_id: BindingId,
    agent_id: AgentId,
}
impl ObservedAgentKey {
    /// Create a generation-scoped identity from already checked components.
    pub(crate) fn new(binding_id: BindingId, agent_id: AgentId) -> Self {
        Self {
            binding_id,
            agent_id,
        }
    }
    /// Get the original connection generation.
    pub fn binding_id(&self) -> &BindingId {
        &self.binding_id
    }
    /// Get the native child's registered normalized identity.
    pub fn agent_id(&self) -> &AgentId {
        &self.agent_id
    }
}

/// Reject unproved ownership, conflicting lifecycle evidence, or source-order regression.
#[derive(Debug, thiserror::Error)]
pub(crate) enum ObservedAgentError {
    /// The observation does not belong to its registered original agent and parent.
    #[error("observed native child ownership conflicts with its registered association")]
    Ownership,
    /// A terminal lifecycle summary cannot be rewritten or implicitly restarted.
    #[error("observed native child lifecycle conflicts with its retained outcome")]
    Lifecycle,
    /// The ordered publisher cannot regress its binding sequence.
    #[error("observed native child publication sequence regressed")]
    Sequence,
    /// A child item or accounting event arrived without its own admitted start.
    #[error("observed native child event has no admitted turn")]
    MissingTurn,
    /// Existing exchange reducers rejected contradictory native contents.
    #[error("observed native child exchange rejected an observation")]
    Exchange(#[source] anyhow::Error),
    /// Typed observation serialization could not establish a replay fingerprint.
    #[error("observed native child replay contents could not be encoded")]
    Encoding(#[source] serde_json::Error),
}

/// Retain one observed child's ancestry, metadata, and independent native exchanges.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "StoredObservedAgent")]
pub struct ObservedAgent {
    key: ObservedAgentKey,
    parent_scope: InvocationScope,
    parent_agent_id: Option<AgentId>,
    description: AgentDescription,
    thread_id: Option<ThreadId>,
    owner: Option<ThreadOwner>,
    state: ObservedAgentState,
    availability: ObservedAgentAvailability,
    exchanges: Vec<Exchange>,
    thread_events: Vec<ObservedThreadEvent>,
    host_events: Vec<ObservedHostEvent>,
    pending_file_changes: Vec<crate::tools::lsp::HostFileChange>,
    created_at_ms: i64,
    updated_at_ms: i64,
    publication: Option<ObservedPublication>,
}
impl ObservedAgent {
    /// Get the exact original binding and agent identity.
    pub fn key(&self) -> &ObservedAgentKey {
        &self.key
    }
    /// Get the original trusted delegating invocation, including nested parent identity.
    pub fn parent_scope(&self) -> &InvocationScope {
        &self.parent_scope
    }
    /// Get the trusted native parent identity when supplied independently of turn metadata.
    pub fn parent_agent_id(&self) -> Option<&AgentId> {
        self.parent_agent_id.as_ref()
    }
    /// Get independently refined native description fields.
    pub fn description(&self) -> &AgentDescription {
        &self.description
    }
    /// Get the positively associated child conversation when available.
    pub fn thread_id(&self) -> Option<&ThreadId> {
        self.thread_id.as_ref()
    }
    /// Get the child owner learned through registered native turn evidence.
    pub fn owner(&self) -> Option<&ThreadOwner> {
        self.owner.as_ref()
    }
    /// Get the native lifecycle summary independently of child turn completion.
    pub fn state(&self) -> ObservedAgentState {
        self.state
    }
    /// Get host observation availability without interpreting native lifecycle as a cleanup certificate.
    pub fn availability(&self) -> ObservedAgentAvailability {
        self.availability
    }
    /// Freeze host display observation without fabricating native terminal evidence or timestamps.
    pub(crate) fn mark_unavailable(&mut self) -> bool {
        if self.availability == ObservedAgentAvailability::Unavailable {
            return false;
        }
        self.availability = ObservedAgentAvailability::Unavailable;
        let boundary = self.observation_boundary_ms();
        for exchange in &mut self.exchanges {
            exchange.pause(boundary);
        }
        true
    }
    /// Get independently owned child exchanges in admission order.
    pub fn exchanges(&self) -> &[Exchange] {
        &self.exchanges
    }
    /// Get committed host effects whose native transcript admission has not yet arrived.
    pub(crate) fn pending_file_changes(&self) -> &[crate::tools::lsp::HostFileChange] {
        &self.pending_file_changes
    }
    /// Get the first positive native association time.
    pub fn created_at_ms(&self) -> i64 {
        self.created_at_ms
    }
    /// Get the last native observation boundary without treating later host-only effects as native time.
    pub fn observation_boundary_ms(&self) -> i64 {
        self.publication
            .as_ref()
            .map_or(self.created_at_ms, |publication| publication.observed_at_ms)
    }
    /// Get the latest consumed native observation time.
    pub fn updated_at_ms(&self) -> i64 {
        self.updated_at_ms
    }
    /// Get a deterministic transport identity without concatenating or decoding IDs.
    pub fn storage_id(&self) -> String {
        serde_json::to_string(&self.key).expect("checked identity serialization is infallible")
    }
    /// Validate generation-scoped native ancestry without inventing an unresolved parent.
    pub(crate) fn hierarchy(agents: &[Self]) -> Result<Vec<Option<usize>>, ObservedAgentError> {
        let mut identities = std::collections::HashSet::new();
        let mut parent = Vec::new();
        for (index, child) in agents.iter().enumerate() {
            if !identities.insert(child.key.clone()) {
                return Err(ObservedAgentError::Ownership);
            }
            let candidates: Vec<_> = agents
                .iter()
                .enumerate()
                .filter(|(other, candidate)| {
                    *other != index
                        && candidate.key.binding_id == child.key.binding_id
                        && child.parent_agent_id.as_ref().map_or_else(
                            || candidate.owner.as_ref() == Some(&child.parent_scope.owner()),
                            |parent| &candidate.key.agent_id == parent,
                        )
                })
                .map(|(index, _)| index)
                .collect();
            if candidates.len() > 1 {
                return Err(ObservedAgentError::Ownership);
            }
            parent.push(candidates.first().copied());
        }
        for index in 0..agents.len() {
            let mut path = std::collections::HashSet::new();
            let mut next = Some(index);
            while let Some(index) = next {
                if !path.insert(index) {
                    return Err(ObservedAgentError::Ownership);
                }
                next = parent[index];
            }
        }
        Ok(parent)
    }

    /// Create an unobserved child exclusively from the bridge's trusted registration proof.
    pub(crate) fn registered(
        association: &ObservedAgentAssociation,
        now_ms: i64,
    ) -> Result<Self, ObservedAgentError> {
        let agent = Self {
            key: ObservedAgentKey::new(
                association.binding_id().clone(),
                association.agent_id().clone(),
            ),
            parent_scope: association.parent().clone(),
            parent_agent_id: association.parent_agent_id().cloned(),
            description: AgentDescription {
                parent_agent_id: None,
                delegation: None,
                thread_id: None,
                definition: None,
                nickname: None,
                task: None,
            },
            thread_id: association.thread_id().cloned(),
            owner: association.owner().cloned(),
            state: ObservedAgentState::NotObserved,
            availability: ObservedAgentAvailability::Available,
            exchanges: vec![],
            thread_events: vec![],
            host_events: vec![],
            pending_file_changes: vec![],
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
            publication: None,
        };
        agent.validate()?;
        Ok(agent)
    }
    /// Apply lifecycle metadata under the exact bridge proof without creating native turn evidence.
    pub(crate) fn observe_agent(
        &mut self,
        association: &ObservedAgentAssociation,
        source: &NativeEventSource,
        event: &AgentEvent,
        now_ms: i64,
    ) -> Result<bool, ObservedAgentError> {
        let mut next = self.clone();
        next.associate(association)?;
        if !next.admit_source(source, fingerprint(&("agent", event))?, now_ms)? {
            return Ok(false);
        }
        match event {
            AgentEvent::ConversationResolved { thread_id } => {
                retain_identity(&mut next.thread_id, thread_id)?;
                retain_identity(&mut next.description.thread_id, thread_id)?;
            }
            AgentEvent::Lifecycle { event } => {
                let (description, state) = match event {
                    AgentLifecycleEvent::Started { agent, state }
                    | AgentLifecycleEvent::Updated { agent, state } => {
                        (agent, ObservedAgentState::Active(*state))
                    }
                    AgentLifecycleEvent::Finished { agent, outcome } => {
                        (agent, ObservedAgentState::Finished(*outcome))
                    }
                };
                if let ObservedAgentState::Finished(previous) = next.state
                    && state != ObservedAgentState::Finished(previous)
                {
                    return Err(ObservedAgentError::Lifecycle);
                }
                next.refine_description(description)?;
                next.state = state;
            }
        }
        next.updated_at_ms = now_ms;
        next.validate()?;
        *self = next;
        Ok(true)
    }
    /// Apply child thread, turn, and item evidence exclusively to its registered original scope.
    pub(crate) fn observe_event(
        &mut self,
        association: &ObservedAgentAssociation,
        source: &NativeEventSource,
        scope: &EventScope,
        event: &ProviderEvent,
        now_ms: i64,
    ) -> Result<bool, ObservedAgentError> {
        let mut next = self.clone();
        next.associate(association)?;
        if !next.admit_source(source, fingerprint(&("native", scope, event))?, now_ms)? {
            return Ok(false);
        }
        let (thread_id, turn_id) = match event {
            ProviderEvent::Thread { thread_id, event } => {
                if next.thread_id.as_ref() != Some(thread_id)
                    || next.owner.as_ref() != Some(&scope.owner())
                {
                    return Err(ObservedAgentError::Ownership);
                }
                next.thread_events.push(ObservedThreadEvent {
                    sequence: source.sequence,
                    event: event.clone(),
                });
                next.updated_at_ms = now_ms;
                next.validate()?;
                *self = next;
                return Ok(true);
            }
            ProviderEvent::Turn {
                thread_id, turn_id, ..
            }
            | ProviderEvent::Item {
                thread_id, turn_id, ..
            } => (thread_id, turn_id),
            ProviderEvent::Provider { .. } | ProviderEvent::Agent { .. } => {
                return Err(ObservedAgentError::Ownership);
            }
        };
        let original = association.turn().ok_or(ObservedAgentError::Ownership)?;
        if scope.invocation() != Some(original)
            || next.thread_id.as_ref() != Some(thread_id)
            || original
                .turn_id()
                .map_err(|_| ObservedAgentError::Ownership)?
                != *turn_id
        {
            return Err(ObservedAgentError::Ownership);
        }
        let address = TurnAddress {
            scope: original.clone(),
            thread_id: thread_id.clone(),
            turn_id: turn_id.clone(),
        };
        let position = next
            .exchanges
            .iter()
            .position(|exchange| exchange.turn.iter().any(|turn| turn.provider() == &address));
        let position = match position {
            Some(position) => position,
            None => {
                if next.availability == ObservedAgentAvailability::Unavailable {
                    return Err(ObservedAgentError::Ownership);
                }
                let ProviderEvent::Turn {
                    event: TurnEvent::Started(started),
                    ..
                } = event
                else {
                    return Err(ObservedAgentError::MissingTurn);
                };
                let id = serde_json::to_string(&(&next.key, turn_id))
                    .expect("checked identity serialization is infallible");
                let ordinal = u64::try_from(next.exchanges.len())
                    .ok()
                    .and_then(|count| count.checked_add(1))
                    .ok_or(ObservedAgentError::Sequence)?;
                let mut exchange = Exchange::observed(id, ordinal, address, started.started_at_ms)
                    .map_err(ObservedAgentError::Exchange)?;
                for host in &next.host_events {
                    if host.scope.invocation() == Some(original) {
                        apply_host(&mut exchange, &host.scope, &host.event, host.observed_at_ms)?;
                    }
                }
                next.exchanges.push(exchange);
                next.exchanges.len() - 1
            }
        };
        let exchange = &mut next.exchanges[position];
        exchange
            .observe_provider_event(scope, event, now_ms)
            .map_err(ObservedAgentError::Exchange)?;
        if let ProviderEvent::Turn {
            event: TurnEvent::Completed(completed),
            ..
        } = event
        {
            let outcome = match completed.result().outcome() {
                forge_provider_api::turn::TurnOutcome::Completed { .. } => {
                    ExchangeOutcome::Complete
                }
                forge_provider_api::turn::TurnOutcome::Interrupted => ExchangeOutcome::Interrupted,
                forge_provider_api::turn::TurnOutcome::Failed { .. } => ExchangeOutcome::Failed,
            };
            exchange
                .finish(outcome, completed.result().completed_at_ms())
                .map_err(ObservedAgentError::Exchange)?;
        }
        next.flush_file_changes()?;
        next.updated_at_ms = now_ms;
        next.validate()?;
        *self = next;
        Ok(true)
    }
    /// Retain host effects under an already registered child owner without advancing native sequence.
    pub(crate) fn observe_host_event(
        &mut self,
        association: &ObservedAgentAssociation,
        scope: &EventScope,
        event: &crate::provider::HostEvent,
        now_ms: i64,
    ) -> Result<bool, ObservedAgentError> {
        let mut next = self.clone();
        next.associate(association)?;
        if next.owner.as_ref() != Some(&scope.owner()) {
            return Err(ObservedAgentError::Ownership);
        }
        if let Some(invocation) = scope.invocation() {
            if association.turn() != Some(invocation) {
                return Err(ObservedAgentError::Ownership);
            }
            if let Some(exchange) = next.exchanges.iter_mut().find(|exchange| {
                exchange
                    .turn
                    .iter()
                    .any(|turn| &turn.provider().scope == invocation)
            }) {
                apply_host(exchange, scope, event, now_ms)?;
            }
        }
        next.host_events.push(ObservedHostEvent {
            scope: scope.clone(),
            event: event.clone(),
            observed_at_ms: now_ms,
        });
        next.updated_at_ms = now_ms;
        next.validate()?;
        *self = next;
        Ok(true)
    }

    /// Retain an authenticated committed host mutation without fabricating native item events.
    pub(crate) fn observe_file_change(
        &mut self,
        association: &ObservedAgentAssociation,
        effect: &crate::tools::lsp::HostFileChange,
        now_ms: i64,
    ) -> Result<bool, ObservedAgentError> {
        let mut next = self.clone();
        next.associate(association)?;
        if association.turn() != Some(&effect.scope)
            || next.owner.as_ref() != Some(&effect.scope.owner())
        {
            return Err(ObservedAgentError::Ownership);
        }
        if let Some(previous) = next
            .pending_file_changes
            .iter_mut()
            .find(|previous| previous.scope == effect.scope && previous.call_id == effect.call_id)
        {
            if previous.changes != effect.changes
                || previous
                    .native_item_id
                    .as_ref()
                    .zip(effect.native_item_id.as_ref())
                    .is_some_and(|(old, new)| old != new)
            {
                return Err(ObservedAgentError::Ownership);
            }
            if previous.native_item_id.is_none() {
                previous.native_item_id = effect.native_item_id.clone();
            }
        } else {
            next.pending_file_changes.push(effect.clone());
        }
        next.flush_file_changes()?;
        next.updated_at_ms = now_ms;
        next.validate()?;
        *self = next;
        Ok(true)
    }

    /// Replace an existing timeline snapshot only when its registered child ownership remains unchanged.
    pub(crate) fn replace_exchange(
        &mut self,
        replacement: &Exchange,
    ) -> Result<bool, ObservedAgentError> {
        let Some(position) = self
            .exchanges
            .iter()
            .position(|exchange| exchange.id == replacement.id)
        else {
            return Ok(false);
        };
        if self.exchanges[position]
            .turn
            .iter()
            .map(|turn| turn.provider())
            .ne(replacement.turn.iter().map(|turn| turn.provider()))
        {
            return Err(ObservedAgentError::Ownership);
        }
        let mut next = self.clone();
        next.exchanges[position] = replacement.clone();
        next.validate()?;
        *self = next;
        Ok(true)
    }

    /// Validate loaded records before their contents can enter host routing or rendering.
    fn validate(&self) -> Result<(), ObservedAgentError> {
        if self.parent_agent_id.as_ref() == Some(&self.key.agent_id) {
            return Err(ObservedAgentError::Ownership);
        }
        if self
            .description
            .parent_agent_id
            .as_ref()
            .is_some_and(|parent| Some(parent) != self.parent_agent_id.as_ref())
        {
            return Err(ObservedAgentError::Ownership);
        }
        if let Some(delegation) = &self.description.delegation
            && (delegation.parent_thread_id != self.parent_scope.owner().thread_id()
                || delegation.parent_turn_id
                    != self
                        .parent_scope
                        .turn_id()
                        .map_err(|_| ObservedAgentError::Ownership)?)
        {
            return Err(ObservedAgentError::Ownership);
        }
        if self
            .description
            .thread_id
            .as_ref()
            .is_some_and(|thread| self.thread_id.as_ref() != Some(thread))
        {
            return Err(ObservedAgentError::Ownership);
        }
        if let Some(owner) = &self.owner
            && (owner.session_id() != self.parent_scope.owner().session_id()
                || owner == &self.parent_scope.owner()
                || self.thread_id.as_ref() != Some(&owner.thread_id()))
        {
            return Err(ObservedAgentError::Ownership);
        }
        if self
            .publication
            .as_ref()
            .is_some_and(|publication| publication.sequence.get() == 0)
            || (self.publication.is_none()
                && (!self.exchanges.is_empty() || self.state != ObservedAgentState::NotObserved))
            || self.thread_events.iter().any(|event| {
                self.publication
                    .as_ref()
                    .is_none_or(|publication| event.sequence > publication.sequence)
            })
            || self
                .thread_events
                .windows(2)
                .any(|events| events[0].sequence >= events[1].sequence)
            || self.host_events.iter().any(|event| {
                self.owner.as_ref() != Some(&event.scope.owner())
                    || event
                        .scope
                        .invocation()
                        .is_some_and(|scope| scope.exchange_id != self.parent_scope.exchange_id)
            })
        {
            return Err(ObservedAgentError::Ownership);
        }
        if self.pending_file_changes.iter().any(|effect| {
            self.owner.as_ref() != Some(&effect.scope.owner())
                || effect.scope.exchange_id != self.parent_scope.exchange_id
        }) {
            return Err(ObservedAgentError::Ownership);
        }
        let mut turns = std::collections::HashSet::new();
        for (index, exchange) in self.exchanges.iter().enumerate() {
            let Some(turn) = exchange.turn.first().filter(|_| exchange.turn.len() == 1) else {
                return Err(ObservedAgentError::Ownership);
            };
            let expected_id = serde_json::to_string(&(&self.key, &turn.provider().turn_id))
                .expect("checked identity serialization is infallible");
            if !turns.insert(turn.provider().turn_id.clone())
                || exchange.id != expected_id
                || u64::try_from(index)
                    .ok()
                    .and_then(|index| index.checked_add(1))
                    != Some(exchange.ordinal)
            {
                return Err(ObservedAgentError::Ownership);
            }
        }
        for exchange in &self.exchanges {
            if exchange.origin != ExchangeOrigin::NativeAgent
                || exchange.session_id != self.parent_scope.owner().session_id().as_str()
                || self
                    .owner
                    .as_ref()
                    .is_none_or(|owner| exchange.agent_id != owner.agent_id().as_str())
                || exchange.turn.iter().any(|turn| {
                    self.thread_id.as_ref() != Some(&turn.provider().thread_id)
                        || self.owner.as_ref() != Some(&turn.provider().scope.owner())
                        || turn.provider().scope.exchange_id != self.parent_scope.exchange_id
                })
            {
                return Err(ObservedAgentError::Ownership);
            }
        }
        Ok(())
    }
    /// Attach pending committed effects only when the matching native turn and optional item exist.
    fn flush_file_changes(&mut self) -> Result<(), ObservedAgentError> {
        let mut retained = Vec::new();
        for effect in self.pending_file_changes.drain(..) {
            let exchange = self.exchanges.iter_mut().find(|exchange| {
                exchange
                    .turn
                    .iter()
                    .any(|turn| turn.provider().scope == effect.scope)
                    && effect.native_item_id.as_ref().is_none_or(|item| {
                        exchange
                            .turn
                            .iter()
                            .filter(|turn| turn.provider().scope == effect.scope)
                            .any(|turn| {
                                exchange
                                    .tools_for_turn(turn.id())
                                    .any(|tool| tool.id == item.as_str())
                            })
                    })
            });
            if let Some(exchange) = exchange {
                exchange
                    .record_host_file_change(&effect)
                    .map_err(ObservedAgentError::Exchange)?;
            } else {
                retained.push(effect);
            }
        }
        self.pending_file_changes = retained;
        Ok(())
    }

    /// Merge only registry-confirmed identity refinements while retaining the original parent.
    fn associate(
        &mut self,
        association: &ObservedAgentAssociation,
    ) -> Result<(), ObservedAgentError> {
        if self.key.binding_id != *association.binding_id()
            || self.key.agent_id != *association.agent_id()
            || self.parent_scope != *association.parent()
        {
            return Err(ObservedAgentError::Ownership);
        }
        if let Some(parent) = association.parent_agent_id() {
            retain_identity(&mut self.parent_agent_id, parent)?;
        }
        if let Some(thread) = association.thread_id() {
            retain_identity(&mut self.thread_id, thread)?;
        }
        if let Some(owner) = association.owner() {
            retain_identity(&mut self.owner, owner)?;
        }
        self.validate()
    }
    /// Consume one position from the authoritative ordered native publisher.
    fn admit_source(
        &mut self,
        source: &NativeEventSource,
        fingerprint: [u8; 32],
        observed_at_ms: i64,
    ) -> Result<bool, ObservedAgentError> {
        if source.binding_id != self.key.binding_id || source.sequence.get() == 0 {
            return Err(ObservedAgentError::Ownership);
        }
        if let Some(previous) = &self.publication {
            if source.sequence < previous.sequence {
                return Err(ObservedAgentError::Sequence);
            }
            if source.sequence == previous.sequence {
                if fingerprint != previous.fingerprint {
                    return Err(ObservedAgentError::Sequence);
                }
                return Ok(false);
            }
        }
        self.publication = Some(ObservedPublication {
            sequence: source.sequence,
            fingerprint,
            observed_at_ms,
        });
        Ok(true)
    }

    /// Retain independently arriving relationships and replace only positively supplied display fields.
    fn refine_description(
        &mut self,
        description: &AgentDescription,
    ) -> Result<(), ObservedAgentError> {
        if let Some(parent) = &description.parent_agent_id {
            retain_identity(&mut self.description.parent_agent_id, parent)?;
        }
        if let Some(delegation) = &description.delegation {
            retain_identity(&mut self.description.delegation, delegation)?;
        }
        if let Some(thread) = &description.thread_id {
            retain_identity(&mut self.description.thread_id, thread)?;
            retain_identity(&mut self.thread_id, thread)?;
        }
        if let Some(definition) = &description.definition {
            self.description.definition = Some(definition.clone());
        }
        if let Some(nickname) = &description.nickname {
            self.description.nickname = Some(nickname.clone());
        }
        if let Some(task) = &description.task {
            self.description.task = Some(task.clone());
        }
        self.validate()
    }
}

/// Bind replay identity to the exact typed contents published at one native sequence.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct ObservedPublication {
    sequence: EventSequence,
    fingerprint: [u8; 32],
    observed_at_ms: i64,
}

/// Retain host-policy history without granting adapter publication or managed task authority.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct ObservedHostEvent {
    scope: EventScope,
    event: crate::provider::HostEvent,
    observed_at_ms: i64,
}

/// Retain thread-level observations separately from canonical child-turn completion.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct ObservedThreadEvent {
    sequence: EventSequence,
    event: ThreadEvent,
}

#[derive(Deserialize)]
struct StoredObservedAgent {
    key: ObservedAgentKey,
    parent_scope: InvocationScope,
    parent_agent_id: Option<AgentId>,
    description: AgentDescription,
    thread_id: Option<ThreadId>,
    owner: Option<ThreadOwner>,
    state: ObservedAgentState,
    availability: ObservedAgentAvailability,
    exchanges: Vec<Exchange>,
    thread_events: Vec<ObservedThreadEvent>,
    host_events: Vec<ObservedHostEvent>,
    pending_file_changes: Vec<crate::tools::lsp::HostFileChange>,
    created_at_ms: i64,
    updated_at_ms: i64,
    publication: Option<ObservedPublication>,
}
impl TryFrom<StoredObservedAgent> for ObservedAgent {
    type Error = ObservedAgentError;
    /// Validate persisted ancestry and owned exchanges through the same domain contract.
    fn try_from(value: StoredObservedAgent) -> Result<Self, Self::Error> {
        let StoredObservedAgent {
            key,
            parent_scope,
            parent_agent_id,
            description,
            thread_id,
            owner,
            state,
            availability,
            exchanges,
            thread_events,
            host_events,
            pending_file_changes,
            created_at_ms,
            updated_at_ms,
            publication,
        } = value;
        let observed = Self {
            key,
            parent_scope,
            parent_agent_id,
            description,
            thread_id,
            owner,
            state,
            availability,
            exchanges,
            thread_events,
            host_events,
            pending_file_changes,
            created_at_ms,
            updated_at_ms,
            publication,
        };
        observed.validate()?;
        Ok(observed)
    }
}

/// Retain one identity refinement without erasing or silently replacing known ownership.
fn retain_identity<T: Clone + Eq>(
    retained: &mut Option<T>,
    incoming: &T,
) -> Result<(), ObservedAgentError> {
    if retained
        .as_ref()
        .is_some_and(|retained| retained != incoming)
    {
        return Err(ObservedAgentError::Ownership);
    }
    *retained = Some(incoming.clone());
    Ok(())
}

/// Apply permission waiting and committed host history to the exact registered child exchange.
fn apply_host(
    exchange: &mut Exchange,
    scope: &EventScope,
    event: &crate::provider::HostEvent,
    now_ms: i64,
) -> Result<(), ObservedAgentError> {
    if let crate::provider::HostEvent::PermissionApproval { event, .. } = event {
        let (request_id, pending) = match event {
            crate::provider::PermissionApprovalEvent::Requested(request) => {
                (&request.request_id, true)
            }
            crate::provider::PermissionApprovalEvent::Resolved(resolution) => {
                (&resolution.request_id, false)
            }
            crate::provider::PermissionApprovalEvent::Cancelled(cancellation) => {
                (&cancellation.request_id, false)
            }
        };
        exchange.observe_blocker(
            format!("approval:observed:{}", request_id.as_str()),
            pending,
            now_ms,
        );
    }
    exchange
        .observe_host_event(scope, event, now_ms)
        .map_err(ObservedAgentError::Exchange)?;
    Ok(())
}

/// Compare typed replay contents without retaining a second native payload or dispatching on JSON.
fn fingerprint(value: &impl Serialize) -> Result<[u8; 32], ObservedAgentError> {
    let encoded = serde_json::to_vec(value).map_err(ObservedAgentError::Encoding)?;
    Ok(Sha256::digest(encoded).into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::AgentMessageRole;
    use forge_provider_api::agent::AgentDelegation;
    use forge_provider_api::{
        AgentMessageStart, ItemCompletion, ItemEvent, ItemId, ItemStart, MessageClassification,
        MessageCompletion, MessageOutcome,
    };

    /// Register public native identity separately from the positively learned child execution owner.
    fn fixture() -> (ObservedAgentAssociation, TurnAddress, AgentDescription) {
        let parent = InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(ThreadOwner::root("session")),
            exchange_id: "parent-exchange".into(),
            invocation_id: "parent-turn".into(),
        };
        let public = AgentId::new("native-child").unwrap();
        let child_owner = ThreadOwner::new(
            parent.owner().session_id().clone(),
            AgentId::new("host-child-owner").unwrap(),
        )
        .unwrap();
        let scope = InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(child_owner),
            exchange_id: parent.exchange_id.clone(),
            invocation_id: "child-turn".into(),
        };
        let child = TurnAddress {
            thread_id: scope.owner().thread_id(),
            turn_id: scope.turn_id().unwrap(),
            scope,
        };
        let description = AgentDescription {
            parent_agent_id: Some(parent.owner().agent_id().clone()),
            delegation: Some(AgentDelegation {
                parent_thread_id: parent.owner().thread_id(),
                parent_turn_id: parent.turn_id().unwrap(),
                parent_item_id: None,
            }),
            thread_id: Some(child.thread_id.clone()),
            definition: Some("explorer".into()),
            nickname: Some("Inspect".into()),
            task: Some("Read the repository".into()),
        };
        let proof = crate::provider::test_support::observed_agent_fixture(
            BindingId::new("binding").unwrap(),
            parent,
            vec![(public.clone(), description.clone(), Some(child.clone()))],
            public,
            Some(child.turn_id.clone()),
        )
        .unwrap();
        (proof, child, description)
    }
    /// Create a source position from the fixture binding's ordered native publisher.
    fn source(proof: &ObservedAgentAssociation, sequence: u64) -> NativeEventSource {
        NativeEventSource {
            binding_id: proof.binding_id().clone(),
            sequence: EventSequence::new(sequence),
        }
    }
    /// A terminal native summary cannot manufacture transcript turns or collection evidence.
    #[test]
    fn lifecycle_summary_never_manufactures_a_child_turn() {
        let (proof, _, description) = fixture();
        let mut child = ObservedAgent::registered(&proof, 1).unwrap();
        child
            .observe_agent(
                &proof,
                &source(&proof, 1),
                &AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Finished {
                        agent: description,
                        outcome: AgentOutcome::Completed,
                    },
                },
                2,
            )
            .unwrap();
        assert_eq!(
            child.state(),
            ObservedAgentState::Finished(AgentOutcome::Completed)
        );
        assert!(child.exchanges().is_empty());
        let restored: ObservedAgent =
            serde_json::from_value(serde_json::to_value(&child).unwrap()).unwrap();
        assert!(restored.exchanges().is_empty());
        assert_ne!(
            restored.key().agent_id(),
            restored.owner().unwrap().agent_id()
        );
    }
    /// A child's own terminal certificate and late accounting retain the original child turn.
    #[test]
    fn child_items_and_completed_turn_retain_original_ownership_after_summary_finish() {
        let (proof, address, description) = fixture();
        let mut child = ObservedAgent::registered(&proof, 1).unwrap();
        child
            .observe_agent(
                &proof,
                &source(&proof, 1),
                &AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Started {
                        agent: description.clone(),
                        state: AgentActiveState::Running,
                    },
                },
                1,
            )
            .unwrap();
        let scope = EventScope::from(address.scope.clone());
        let event = |event| ProviderEvent::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            event,
        };
        child
            .observe_event(
                &proof,
                &source(&proof, 2),
                &scope,
                &event(TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 2,
                })),
                2,
            )
            .unwrap();
        let item = |event| ProviderEvent::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: ItemId::new("answer").unwrap(),
            event,
        };
        child
            .observe_event(
                &proof,
                &source(&proof, 3),
                &scope,
                &item(ItemEvent::Started(ItemStart::AgentMessage(
                    AgentMessageStart {
                        text: "partial".into(),
                        classification: MessageClassification::Known(AgentMessageRole::Response),
                    },
                ))),
                3,
            )
            .unwrap();
        child
            .observe_event(
                &proof,
                &source(&proof, 4),
                &scope,
                &item(ItemEvent::Completed(ItemCompletion::AgentMessage(
                    MessageCompletion {
                        text: "retained partial".into(),
                        outcome: MessageOutcome::Interrupted,
                    },
                ))),
                4,
            )
            .unwrap();
        child
            .observe_agent(
                &proof,
                &source(&proof, 5),
                &AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Finished {
                        agent: description,
                        outcome: AgentOutcome::Completed,
                    },
                },
                5,
            )
            .unwrap();
        let result = forge_provider_api::TurnResult::new(
            forge_provider_api::turn::TurnOutcome::Interrupted,
            6,
            None,
            vec![],
            vec![],
            None,
        )
        .unwrap();
        let completed =
            crate::provider::test_support::completed_turn(address.scope.clone(), result).unwrap();
        child
            .observe_event(
                &proof,
                &source(&proof, 6),
                &scope,
                &event(TurnEvent::Completed(completed.clone())),
                6,
            )
            .unwrap();
        let report = forge_provider_api::TurnUsageReport::timing(
            forge_provider_api::UsageReportId::new("late").unwrap(),
            7,
            3,
        );
        child
            .observe_event(
                &proof,
                &source(&proof, 7),
                &scope,
                &event(TurnEvent::Usage(report)),
                7,
            )
            .unwrap();
        let restored: ObservedAgent =
            serde_json::from_value(serde_json::to_value(&child).unwrap()).unwrap();
        assert_eq!(restored.exchanges().len(), 1);
        let exchange = &restored.exchanges()[0];
        assert_ne!(exchange.id, address.scope.exchange_id.as_str());
        assert_eq!(exchange.turn[0].provider(), &address);
        assert_eq!(exchange.turn[0].completed(), Some(&completed));
        assert_eq!(exchange.turn[0].messages()[0].text(), "retained partial");
        assert_eq!(restored.parent_scope(), proof.parent());
        assert_eq!(
            restored.state(),
            ObservedAgentState::Finished(AgentOutcome::Completed)
        );
    }
    /// Publication replay cannot change evidence or move the child to another generation.
    #[test]
    fn conflicting_replay_and_foreign_binding_leave_observed_child_unchanged() {
        let (proof, _, description) = fixture();
        let mut child = ObservedAgent::registered(&proof, 1).unwrap();
        let event = AgentEvent::Lifecycle {
            event: AgentLifecycleEvent::Started {
                agent: description.clone(),
                state: AgentActiveState::Running,
            },
        };
        child
            .observe_agent(&proof, &source(&proof, 1), &event, 1)
            .unwrap();
        let before = serde_json::to_value(&child).unwrap();
        assert!(
            !child
                .observe_agent(&proof, &source(&proof, 1), &event, 2)
                .unwrap()
        );
        let mut changed = description;
        changed.nickname = Some("conflicting replay".into());
        assert!(matches!(
            child.observe_agent(
                &proof,
                &source(&proof, 1),
                &AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Updated {
                        agent: changed,
                        state: AgentActiveState::Running
                    }
                },
                2
            ),
            Err(ObservedAgentError::Sequence)
        ));
        let foreign = NativeEventSource {
            binding_id: BindingId::new("replacement").unwrap(),
            sequence: EventSequence::new(2),
        };
        assert!(matches!(
            child.observe_agent(&proof, &foreign, &event, 2),
            Err(ObservedAgentError::Ownership)
        ));
        assert_eq!(serde_json::to_value(&child).unwrap(), before);
    }
    /// Explicit native ancestry retains summary-only nested children without guessed execution owners.
    #[test]
    fn summary_only_nested_agents_keep_explicit_hierarchy_and_reject_cycles() {
        let root = InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(ThreadOwner::root("session")),
            exchange_id: "parent".into(),
            invocation_id: "root-turn".into(),
        };
        let parent_id = AgentId::new("native-parent").unwrap();
        let nested_id = AgentId::new("nested").unwrap();
        let parent = AgentDescription {
            parent_agent_id: Some(root.owner().agent_id().clone()),
            delegation: Some(AgentDelegation {
                parent_thread_id: root.owner().thread_id(),
                parent_turn_id: root.turn_id().unwrap(),
                parent_item_id: None,
            }),
            thread_id: None,
            definition: None,
            nickname: None,
            task: None,
        };
        let nested = AgentDescription {
            parent_agent_id: Some(parent_id.clone()),
            delegation: None,
            thread_id: None,
            definition: None,
            nickname: None,
            task: None,
        };
        let registrations = vec![
            (parent_id.clone(), parent.clone(), None),
            (nested_id.clone(), nested.clone(), None),
        ];
        let proof = |target| {
            crate::provider::test_support::observed_agent_fixture(
                BindingId::new("binding").unwrap(),
                root.clone(),
                registrations.clone(),
                target,
                None,
            )
            .unwrap()
        };
        let parent_proof = proof(parent_id.clone());
        let nested_proof = proof(nested_id.clone());
        let mut first = ObservedAgent::registered(&parent_proof, 1).unwrap();
        let mut second = ObservedAgent::registered(&nested_proof, 2).unwrap();
        first
            .observe_agent(
                &parent_proof,
                &source(&parent_proof, 1),
                &AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Started {
                        agent: parent,
                        state: AgentActiveState::Running,
                    },
                },
                1,
            )
            .unwrap();
        second
            .observe_agent(
                &nested_proof,
                &source(&nested_proof, 2),
                &AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Started {
                        agent: nested,
                        state: AgentActiveState::Waiting,
                    },
                },
                2,
            )
            .unwrap();
        assert!(first.owner().is_none() && second.owner().is_none());
        assert_eq!(
            ObservedAgent::hierarchy(&[second.clone(), first.clone()]).unwrap(),
            vec![Some(1), None]
        );
        let timeline = crate::timeline::TimelineProjector::with_observed_agents(
            vec![],
            vec![second.clone(), first.clone()],
        )
        .unwrap();
        assert!(
            matches!(&timeline[..],[crate::timeline::TimelineEntry::ObservedAgent {children,..}] if children.len()==1)
        );
        first.parent_agent_id = Some(nested_id);
        first.description.parent_agent_id = first.parent_agent_id.clone();
        assert!(ObservedAgent::hierarchy(&[first, second]).is_err());
    }
    /// Actual host writes wait for original transcript evidence without fabricating a native item or end.
    #[test]
    fn committed_host_file_changes_and_unavailable_observation_do_not_fabricate_native_lifecycle() {
        let (proof, address, _) = fixture();
        let mut child = ObservedAgent::registered(&proof, 1).unwrap();
        let effect = crate::tools::lsp::HostFileChange {
            scope: address.scope.clone(),
            call_id: forge_provider_api::control::ToolCallId::new("rename").unwrap(),
            native_item_id: None,
            changes: forge_provider_api::ProviderChangeSet {
                file: vec![forge_provider_api::ProviderFileChange {
                    path: "source.rs".into(),
                    move_path: None,
                    kind: forge_provider_api::ProviderChangeKind::Update,
                    diff: "@@ -1 +1 @@\n-before\n+after\n".into(),
                }],
            },
        };
        child.observe_file_change(&proof, &effect, 2).unwrap();
        assert_eq!(child.pending_file_changes().len(), 1);
        assert!(child.exchanges().is_empty());
        let started = ProviderEvent::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            event: TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 3 }),
        };
        child
            .observe_event(
                &proof,
                &source(&proof, 1),
                &EventScope::from(address.scope.clone()),
                &started,
                3,
            )
            .unwrap();
        assert!(child.pending_file_changes().is_empty());
        assert_eq!(
            child.exchanges()[0].host_file_changes,
            std::slice::from_ref(&effect)
        );
        assert!(child.exchanges()[0].turn[0].items().is_empty());
        let before = serde_json::to_value(&child).unwrap();
        let mut conflict = effect;
        conflict.changes.file[0].diff = "different".into();
        assert!(child.observe_file_change(&proof, &conflict, 4).is_err());
        assert_eq!(serde_json::to_value(&child).unwrap(), before);
        assert!(child.mark_unavailable());
        assert!(!child.mark_unavailable());
        assert_eq!(child.availability(), ObservedAgentAvailability::Unavailable);
        assert_eq!(child.state(), ObservedAgentState::NotObserved);
        assert_eq!(child.exchanges()[0].turn[0].completed(), None);
        assert_eq!(
            child.exchanges()[0].turn[0].state(),
            crate::turn::TurnState::Running
        );
        let restored: ObservedAgent =
            serde_json::from_value(serde_json::to_value(&child).unwrap()).unwrap();
        assert_eq!(
            restored.availability(),
            ObservedAgentAvailability::Unavailable
        );
        assert_eq!(restored.updated_at_ms(), 3);
    }
}
