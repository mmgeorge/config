//! Resolve normalized identities to their immutable host invocation before delivery.
use crate::provider::ScopeOwner;
use crate::provider::ThreadOwner;
use forge_provider_api::agent::{AgentEvent, AgentId, AgentLifecycleEvent};
use forge_provider_api::events::EventDeliveryError;
use forge_provider_api::{
    BindingId, EventScope, InvocationScope, ItemId, ProviderEvent, ThreadId, TurnEvent, TurnId,
};
use std::collections::HashMap;

/// Prove one observed native child belongs to its original registered delegation and generation.
#[derive(Clone, Debug)]
pub(crate) struct ObservedAgentAssociation {
    binding_id: BindingId,
    agent_id: AgentId,
    parent_agent_id: Option<AgentId>,
    parent: InvocationScope,
    thread_id: Option<ThreadId>,
    owner: Option<ThreadOwner>,
    turn: Option<InvocationScope>,
}
impl ObservedAgentAssociation {
    /// Get the exact original native binding generation.
    pub(crate) fn binding_id(&self) -> &BindingId {
        &self.binding_id
    }
    /// Get the registered observed agent identity.
    pub(crate) fn agent_id(&self) -> &AgentId {
        &self.agent_id
    }
    /// Get a positively observed parent agent relationship without inferring it from a turn owner.
    pub(crate) fn parent_agent_id(&self) -> Option<&AgentId> {
        self.parent_agent_id.as_ref()
    }
    /// Get the original trusted delegating invocation.
    pub(crate) fn parent(&self) -> &InvocationScope {
        &self.parent
    }
    /// Get the positively associated child conversation when it has been learned.
    pub(crate) fn thread_id(&self) -> Option<&ThreadId> {
        self.thread_id.as_ref()
    }
    /// Get the positively registered child owner without selecting any response cycle.
    pub(crate) fn owner(&self) -> Option<&ThreadOwner> {
        self.owner.as_ref()
    }
    /// Get only the explicitly requested registered child invocation; never select current work.
    pub(crate) fn turn(&self) -> Option<&InvocationScope> {
        self.turn.as_ref()
    }
}

/// Retain ownership throughout preparation, foreground completion, and late observations.
pub(super) struct OwnershipRegistry {
    binding: BindingId,
    retired: bool,
    operations: std::sync::Arc<super::operation::OperationRegistry>,
    owner: ThreadOwner,
    threads: HashMap<ThreadId, EventScope>,
    turns: HashMap<TurnId, RegisteredTurn>,
    child_owners: HashMap<ThreadId, ThreadOwner>,
    items: HashMap<ItemId, RegisteredItem>,
    agents: HashMap<AgentId, EventScope>,
    agent_threads: HashMap<AgentId, ThreadId>,
    parent_agents: HashMap<AgentId, AgentId>,
    retired_agents: std::collections::HashSet<AgentId>,
}
impl OwnershipRegistry {
    /// Create the ownership boundary for one native binding generation.
    pub(super) fn new(
        binding: BindingId,
        owner: ThreadOwner,
        operations: std::sync::Arc<super::operation::OperationRegistry>,
    ) -> Self {
        let mut threads = HashMap::new();
        threads.insert(
            owner.thread_id(),
            EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(owner.clone()),
            },
        );
        Self {
            binding,
            retired: false,
            operations,
            owner,
            threads,
            turns: HashMap::new(),
            child_owners: HashMap::new(),
            items: HashMap::new(),
            agents: HashMap::new(),
            agent_threads: HashMap::new(),
            parent_agents: HashMap::new(),
            retired_agents: Default::default(),
        }
    }
    /// Mark the original native generation unavailable for mutable session metadata.
    pub(super) fn retire(&mut self) {
        self.retired = true;
    }
    /// Validate original owner and thread before permitting mutable session metadata.
    pub(super) fn source_status(
        &self,
        owner: &ThreadOwner,
        thread: Option<&ThreadId>,
    ) -> Result<super::bridge::NativeBindingStatus, EventDeliveryError> {
        if owner != &self.owner || thread.is_some_and(|id| id != &owner.thread_id()) {
            return Err(conflict());
        }
        Ok(if self.retired {
            super::bridge::NativeBindingStatus::RetiredKnown
        } else {
            super::bridge::NativeBindingStatus::Active
        })
    }
    /// Capture immutable registered child metadata for a later host consumer.
    pub(super) fn observed_agent(
        &self,
        agent_id: &AgentId,
        turn_id: Option<&TurnId>,
    ) -> Result<ObservedAgentAssociation, EventDeliveryError> {
        let parent = self
            .agents
            .get(agent_id)
            .and_then(EventScope::invocation)
            .cloned()
            .ok_or_else(conflict)?;
        let thread_id = self.agent_threads.get(agent_id).cloned();
        let turn = match turn_id {
            Some(turn_id) => {
                let original = self.turns.get(turn_id).ok_or_else(conflict)?;
                if thread_id.as_ref() != Some(&original.thread_id) {
                    return Err(conflict());
                }
                Some(original.scope.clone())
            }
            None => None,
        };
        Ok(ObservedAgentAssociation {
            binding_id: self.binding.clone(),
            agent_id: agent_id.clone(),
            parent_agent_id: self.parent_agents.get(agent_id).cloned(),
            parent,
            owner: thread_id
                .as_ref()
                .and_then(|thread| self.child_owners.get(thread))
                .cloned(),
            thread_id,
            turn,
        })
    }

    /// Resolve one child by its exact registered thread and original turn.
    pub(super) fn observed_child(
        &self,
        thread: &ThreadId,
        turn: &TurnId,
    ) -> Result<ObservedAgentAssociation, EventDeliveryError> {
        let agent = self
            .agent_threads
            .iter()
            .find_map(|(agent, original)| (original == thread).then_some(agent))
            .ok_or_else(conflict)?;
        self.observed_agent(agent, Some(turn))
    }

    /// Validate one child scope against its positively known original delegating invocation.
    pub(super) fn validate_child(
        &self,
        parent: &InvocationScope,
        address: &forge_provider_api::TurnAddress,
    ) -> Result<ObservedAgentAssociation, EventDeliveryError> {
        let agent = self
            .agent_threads
            .iter()
            .find_map(|(agent, thread)| (thread == &address.thread_id).then_some(agent))
            .ok_or_else(conflict)?;
        if self.retired_agents.contains(agent)
            || self
                .turns
                .get(&address.turn_id)
                .is_none_or(|turn| turn.completed)
        {
            return Err(conflict());
        }
        let proof = self.observed_agent(agent, Some(&address.turn_id))?;
        if proof.parent() != parent || proof.turn() != Some(&address.scope) {
            return Err(conflict());
        }
        Ok(proof)
    }

    /// Retain a preparing invocation before the adapter may dispatch native work.
    pub(super) fn prepare(&mut self, scope: InvocationScope) -> Result<(), EventDeliveryError> {
        if scope.owner() != self.owner {
            return Err(conflict());
        }
        let turn_id = scope.turn_id().map_err(|_| conflict())?;
        let thread_id = scope.owner().thread_id();
        if let Some(existing) = self.turns.get(&turn_id) {
            if existing.completed {
                return Err(conflict());
            }
            if existing.thread_id == thread_id && existing.scope == scope {
                return Ok(());
            }
            return Err(conflict());
        }
        self.turns.insert(
            turn_id,
            RegisteredTurn {
                thread_id,
                scope,
                completed: false,
            },
        );
        Ok(())
    }
    /// Learn a child conversation owner only from its immutable native registration.
    pub(super) fn learn_thread_owner(
        &mut self,
        thread: &ThreadId,
        owner: ThreadOwner,
    ) -> Result<(), EventDeliveryError> {
        if thread == &self.owner.thread_id() {
            return if owner == self.owner {
                Ok(())
            } else {
                Err(conflict())
            };
        }
        if !self
            .agent_threads
            .values()
            .any(|original| original == thread)
            || owner.thread_id() != *thread
            || owner.session_id() != self.owner.session_id()
            || self
                .child_owners
                .get(thread)
                .is_some_and(|original| original != &owner)
        {
            return Err(conflict());
        }
        self.child_owners.insert(thread.clone(), owner);
        Ok(())
    }

    /// Admit an adapter-registered child turn only after its trusted parent association is known.
    pub(super) fn admit_observed(
        &mut self,
        address: forge_provider_api::TurnAddress,
    ) -> Result<(), EventDeliveryError> {
        if let Some(existing) = self.turns.get(&address.turn_id) {
            if existing.thread_id == address.thread_id && existing.scope == address.scope {
                return Ok(());
            }
            return Err(conflict());
        }
        let original = self
            .threads
            .get(&address.thread_id)
            .and_then(EventScope::invocation)
            .ok_or_else(conflict)?;
        if address.thread_id != address.scope.owner().thread_id()
            || address.thread_id == self.owner.thread_id()
            || address.scope.owner().session_id() != self.owner.session_id()
            || address.scope.exchange_id != original.exchange_id
            || address.scope.turn_id().map_err(|_| conflict())? != address.turn_id
        {
            return Err(conflict());
        }
        if self
            .child_owners
            .get(&address.thread_id)
            .is_some_and(|owner| owner != &address.scope.owner())
        {
            return Err(conflict());
        }
        self.child_owners
            .insert(address.thread_id.clone(), address.scope.owner().clone());
        self.turns.insert(
            address.turn_id,
            RegisteredTurn {
                thread_id: address.thread_id,
                scope: address.scope,
                completed: false,
            },
        );
        Ok(())
    }
    /// Validate generation and all relationships before admitting this observation.
    pub(super) fn resolve(
        &mut self,
        binding: &BindingId,
        event: &ProviderEvent,
    ) -> Result<EventScope, EventDeliveryError> {
        if binding != &self.binding {
            return Err(conflict());
        }
        match event {
            ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Warning(warning),
            } => self.warning_scope(&warning.scope),
            ProviderEvent::Provider {
                event:
                    forge_provider_api::ProviderConnectionEvent::CapabilitiesChanged(_)
                    | forge_provider_api::ProviderConnectionEvent::Unavailable(_),
            } => Ok(EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(self.owner.clone()),
            }),
            ProviderEvent::Thread { thread_id, .. } => {
                if thread_id == &self.owner.thread_id() {
                    return Ok(EventScope::Binding {
                        thread_id: forge_provider_api::ThreadId::from(self.owner.clone()),
                    });
                }
                self.child_owners
                    .get(thread_id)
                    .cloned()
                    .map(|owner| EventScope::Binding {
                        thread_id: owner.thread_id(),
                    })
                    .ok_or_else(conflict)
            }
            ProviderEvent::Turn {
                thread_id,
                turn_id,
                event,
            } => {
                let turn = self.turns.get_mut(turn_id).ok_or_else(conflict)?;
                if &turn.thread_id != thread_id {
                    return Err(conflict());
                }
                match event {
                    TurnEvent::Completed(_) => turn.completed = true,
                    TurnEvent::Started(_) if turn.completed => return Err(conflict()),
                    TurnEvent::Started(_)
                    | TurnEvent::Error { .. }
                    | TurnEvent::ChecklistUpdated(_)
                    | TurnEvent::Usage(_)
                    | TurnEvent::ModelResolved(_) => {}
                }
                Ok(turn.scope.clone().into())
            }
            ProviderEvent::Item {
                thread_id,
                turn_id,
                item_id,
                event,
            } => {
                let turn = self.turns.get(turn_id).ok_or_else(conflict)?;
                if &turn.thread_id != thread_id {
                    return Err(conflict());
                }
                let kind = item_kind(event);
                let expected_message = matches!(
                    event,
                    forge_provider_api::message::ItemEvent::Message { .. }
                );
                let expected_tool =
                    matches!(event, forge_provider_api::message::ItemEvent::Tool { .. });
                if let Some(item) = self.items.get_mut(item_id) {
                    if &item.turn_id != turn_id || kind.is_some_and(|kind| kind != item.kind) {
                        return Err(conflict());
                    }
                    if (expected_message
                        && !matches!(
                            item.kind,
                            ItemKind::UserMessage | ItemKind::AgentMessage | ItemKind::Reasoning
                        ))
                        || (expected_tool
                            && !matches!(
                                item.kind,
                                ItemKind::CommandExecution
                                    | ItemKind::FileChange
                                    | ItemKind::ToolCall
                            ))
                    {
                        return Err(conflict());
                    }
                    if turn.completed && !item.background {
                        return Err(conflict());
                    }
                    match event {
                        forge_provider_api::message::ItemEvent::Tool {
                            event: forge_provider_api::tool::ToolItemEvent::Backgrounded,
                        } => item.background = true,
                        forge_provider_api::message::ItemEvent::Started(_)
                        | forge_provider_api::message::ItemEvent::Message { .. }
                        | forge_provider_api::message::ItemEvent::Tool { .. }
                        | forge_provider_api::message::ItemEvent::Completed(_) => {}
                    }
                } else {
                    if turn.completed {
                        return Err(conflict());
                    }
                    let forge_provider_api::message::ItemEvent::Started(_) = event else {
                        return Err(conflict());
                    };
                    self.items.insert(
                        item_id.clone(),
                        RegisteredItem {
                            turn_id: turn_id.clone(),
                            kind: kind.expect("started items always define their immutable type"),
                            background: false,
                        },
                    );
                }
                Ok(turn.scope.clone().into())
            }
            ProviderEvent::Agent { agent_id, event } => self.resolve_agent(agent_id, event),
        }
    }
    /// Validate warning identities without weakening them to connection scope.
    fn warning_scope(
        &self,
        scope: &forge_provider_api::WarningScope,
    ) -> Result<EventScope, EventDeliveryError> {
        use forge_provider_api::WarningScope;
        match scope {
            WarningScope::Provider => Ok(EventScope::Binding {
                thread_id: forge_provider_api::ThreadId::from(self.owner.clone()),
            }),
            WarningScope::Thread { thread_id } => {
                if thread_id == &self.owner.thread_id() {
                    return Ok(EventScope::Binding {
                        thread_id: forge_provider_api::ThreadId::from(self.owner.clone()),
                    });
                }
                self.child_owners
                    .get(thread_id)
                    .cloned()
                    .map(|owner| EventScope::Binding {
                        thread_id: owner.thread_id(),
                    })
                    .ok_or_else(conflict)
            }
            WarningScope::Turn { thread_id, turn_id } => {
                let turn = self.turns.get(turn_id).ok_or_else(conflict)?;
                if &turn.thread_id != thread_id {
                    return Err(conflict());
                }
                Ok(turn.scope.clone().into())
            }
            WarningScope::Item {
                thread_id,
                turn_id,
                item_id,
            } => {
                let turn = self.turns.get(turn_id).ok_or_else(conflict)?;
                if &turn.thread_id != thread_id
                    || self
                        .items
                        .get(item_id)
                        .is_none_or(|item| &item.turn_id != turn_id)
                {
                    return Err(conflict());
                }
                Ok(turn.scope.clone().into())
            }
            WarningScope::Agent { agent_id } => {
                self.agents.get(agent_id).cloned().ok_or_else(conflict)
            }
            WarningScope::Operation { operation_id } => {
                self.operations
                    .resolve(operation_id, &self.binding, &self.owner)
            }
        }
    }
    /// Establish child association only from a known delegating turn or recorded agent.
    fn resolve_agent(
        &mut self,
        agent_id: &AgentId,
        event: &AgentEvent,
    ) -> Result<EventScope, EventDeliveryError> {
        match event {
            AgentEvent::Lifecycle { event } => {
                let agent = match event {
                    AgentLifecycleEvent::Started { agent, .. }
                    | AgentLifecycleEvent::Updated { agent, .. }
                    | AgentLifecycleEvent::Finished { agent, .. } => agent,
                };
                let scope = if let Some(delegation) = &agent.delegation {
                    let turn = self
                        .turns
                        .get(&delegation.parent_turn_id)
                        .ok_or_else(conflict)?;
                    if turn.thread_id != delegation.parent_thread_id {
                        return Err(conflict());
                    }
                    if let Some(item_id) = &delegation.parent_item_id
                        && self
                            .items
                            .get(item_id)
                            .is_none_or(|item| item.turn_id != delegation.parent_turn_id)
                    {
                        return Err(conflict());
                    }
                    EventScope::from(turn.scope.clone())
                } else {
                    self.agents
                        .get(agent_id)
                        .cloned()
                        .or_else(|| {
                            agent
                                .parent_agent_id
                                .as_ref()
                                .and_then(|parent| self.agents.get(parent).cloned())
                        })
                        .ok_or_else(conflict)?
                };
                if let Some(parent_id) = &agent.parent_agent_id {
                    let mut ancestor = Some(parent_id.clone());
                    let mut visited = std::collections::HashSet::new();
                    while let Some(parent) = ancestor {
                        if &parent == agent_id || !visited.insert(parent.clone()) {
                            return Err(conflict());
                        }
                        ancestor = self.parent_agents.get(&parent).cloned();
                    }
                    if self
                        .parent_agents
                        .get(agent_id)
                        .is_some_and(|original| original != parent_id)
                    {
                        return Err(conflict());
                    }
                    if parent_id != self.owner.agent_id() {
                        let parent = self.agents.get(parent_id).ok_or_else(conflict)?;
                        if parent.owner().session_id() != scope.owner().session_id() {
                            return Err(conflict());
                        }
                        if let Some(delegation) = &agent.delegation
                            && self.agent_threads.get(parent_id)
                                != Some(&delegation.parent_thread_id)
                        {
                            return Err(conflict());
                        }
                    } else if agent.delegation.as_ref().is_some_and(|delegation| {
                        delegation.parent_thread_id != self.owner.thread_id()
                    }) {
                        return Err(conflict());
                    }
                }
                if self
                    .agents
                    .get(agent_id)
                    .is_some_and(|existing| existing != &scope)
                {
                    return Err(conflict());
                }
                if let Some(thread_id) = &agent.thread_id {
                    if self
                        .agent_threads
                        .get(agent_id)
                        .is_some_and(|original| original != thread_id)
                        || self
                            .agent_threads
                            .iter()
                            .any(|(other, original)| other != agent_id && original == thread_id)
                    {
                        return Err(conflict());
                    }
                    if self
                        .threads
                        .get(thread_id)
                        .is_some_and(|existing| existing != &scope)
                    {
                        return Err(conflict());
                    }
                }
                if matches!(event, AgentLifecycleEvent::Started { .. })
                    && self.retired_agents.contains(agent_id)
                {
                    return Err(conflict());
                }
                if matches!(event, AgentLifecycleEvent::Finished { .. }) {
                    self.retired_agents.insert(agent_id.clone());
                }
                if let Some(parent) = &agent.parent_agent_id {
                    self.parent_agents.insert(agent_id.clone(), parent.clone());
                }
                self.agents.insert(agent_id.clone(), scope.clone());
                if let Some(thread_id) = &agent.thread_id {
                    self.threads.insert(thread_id.clone(), scope.clone());
                    self.agent_threads
                        .insert(agent_id.clone(), thread_id.clone());
                }
                Ok(scope)
            }
            AgentEvent::ConversationResolved { thread_id } => {
                if self
                    .agent_threads
                    .get(agent_id)
                    .is_some_and(|original| original != thread_id)
                    || self
                        .agent_threads
                        .iter()
                        .any(|(other, original)| other != agent_id && original == thread_id)
                {
                    return Err(conflict());
                }
                let scope = self.agents.get(agent_id).cloned().ok_or_else(conflict)?;
                if self
                    .threads
                    .get(thread_id)
                    .is_some_and(|existing| existing != &scope)
                {
                    return Err(conflict());
                }
                self.threads.insert(thread_id.clone(), scope.clone());
                self.agent_threads
                    .insert(agent_id.clone(), thread_id.clone());
                Ok(scope)
            }
        }
    }
}
struct RegisteredTurn {
    thread_id: ThreadId,
    scope: InvocationScope,
    completed: bool,
}
struct RegisteredItem {
    turn_id: TurnId,
    kind: ItemKind,
    background: bool,
}
#[derive(Clone, Copy, Eq, PartialEq)]
enum ItemKind {
    UserMessage,
    AgentMessage,
    Reasoning,
    CommandExecution,
    FileChange,
    ToolCall,
}
/// Read the immutable item kind from admission or final snapshots.
fn item_kind(event: &forge_provider_api::message::ItemEvent) -> Option<ItemKind> {
    use forge_provider_api::message::{ItemCompletion, ItemEvent, ItemStart};
    match event {
        ItemEvent::Started(start) => Some(match start {
            ItemStart::UserMessage(_) => ItemKind::UserMessage,
            ItemStart::AgentMessage(_) => ItemKind::AgentMessage,
            ItemStart::Reasoning(_) => ItemKind::Reasoning,
            ItemStart::CommandExecution(_) => ItemKind::CommandExecution,
            ItemStart::FileChange(_) => ItemKind::FileChange,
            ItemStart::ToolCall(_) => ItemKind::ToolCall,
        }),
        ItemEvent::Completed(completion) => Some(match completion {
            ItemCompletion::UserMessage(_) => ItemKind::UserMessage,
            ItemCompletion::AgentMessage(_) => ItemKind::AgentMessage,
            ItemCompletion::Reasoning(_) => ItemKind::Reasoning,
            ItemCompletion::CommandExecution(_) => ItemKind::CommandExecution,
            ItemCompletion::FileChange(_) => ItemKind::FileChange,
            ItemCompletion::ToolCall(_) => ItemKind::ToolCall,
        }),
        ItemEvent::Message { .. } | ItemEvent::Tool { .. } => None,
    }
}
/// Reject foreign or stale associations without reassigning their ownership.
fn conflict() -> EventDeliveryError {
    EventDeliveryError::Observation(forge_provider_api::ObservationFailure::OwnershipConflict)
}
/// Build native-child test proofs through actual parent, thread, and turn registration.
#[cfg(test)]
pub(crate) fn observed_agent_fixture(
    binding: BindingId,
    root: InvocationScope,
    registrations: Vec<(
        AgentId,
        forge_provider_api::agent::AgentDescription,
        Option<forge_provider_api::TurnAddress>,
    )>,
    target: AgentId,
    turn: Option<TurnId>,
) -> Result<ObservedAgentAssociation, EventDeliveryError> {
    let mut registry =
        OwnershipRegistry::new(binding.clone(), root.owner().clone(), Default::default());
    registry.prepare(root)?;
    for (agent_id, description, address) in registrations {
        registry.resolve(
            &binding,
            &ProviderEvent::Agent {
                agent_id,
                event: AgentEvent::Lifecycle {
                    event: AgentLifecycleEvent::Started {
                        agent: description,
                        state: forge_provider_api::agent::AgentActiveState::Starting,
                    },
                },
            },
        )?;
        if let Some(address) = address {
            registry.admit_observed(address)?;
        }
    }
    registry.observed_agent(&target, turn.as_ref())
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::message::{
        AgentMessageStart, ItemEvent, ItemStart, MessageClassification, MessageItemEvent,
    };

    /// Keep retired provenance readable while denying mutable session metadata authority.
    #[test]
    fn retired_binding_preserves_original_source_without_global_authority() {
        let owner = ThreadOwner::root("source-session");
        let mut registry = OwnershipRegistry::new(
            BindingId::new("source-binding").unwrap(),
            owner.clone(),
            Default::default(),
        );
        assert_eq!(
            registry
                .source_status(&owner, Some(&owner.thread_id()))
                .unwrap(),
            super::super::bridge::NativeBindingStatus::Active
        );
        assert!(
            registry
                .source_status(&ThreadOwner::root("foreign-session"), None)
                .is_err()
        );
        assert!(
            registry
                .source_status(&owner, Some(&ThreadId::new("foreign-thread").unwrap()))
                .is_err()
        );
        registry.retire();
        assert_eq!(
            registry
                .source_status(&owner, Some(&owner.thread_id()))
                .unwrap(),
            super::super::bridge::NativeBindingStatus::RetiredKnown
        );
    }
    /// Reject reused item identity from a successor turn and events from a retired generation.
    #[test]
    fn original_item_owner_survives_successor_preparation() {
        let owner = ThreadOwner::root("session");
        let binding = BindingId::new("binding").unwrap();
        let mut registry =
            OwnershipRegistry::new(binding.clone(), owner.clone(), Default::default());
        let first = InvocationScope::new(owner.clone(), "exchange");
        let successor = InvocationScope::new(owner.clone(), "exchange");
        registry.prepare(first.clone()).unwrap();
        let item_id = ItemId::new("item").unwrap();
        let start = ProviderEvent::Item {
            thread_id: owner.thread_id(),
            turn_id: first.turn_id().unwrap(),
            item_id: item_id.clone(),
            event: ItemEvent::Started(ItemStart::AgentMessage(AgentMessageStart {
                text: String::new(),
                classification: MessageClassification::Unclassified,
            })),
        };
        assert_eq!(
            registry.resolve(&binding, &start).unwrap(),
            EventScope::from(first.clone())
        );
        registry.prepare(successor.clone()).unwrap();
        let late = ProviderEvent::Item {
            thread_id: owner.thread_id(),
            turn_id: first.turn_id().unwrap(),
            item_id: item_id.clone(),
            event: ItemEvent::Message {
                event: MessageItemEvent::Delta("late".into()),
            },
        };
        assert_eq!(
            registry.resolve(&binding, &late).unwrap(),
            EventScope::from(first)
        );
        let reassigned = ProviderEvent::Item {
            thread_id: owner.thread_id(),
            turn_id: successor.turn_id().unwrap(),
            item_id,
            event: ItemEvent::Message {
                event: MessageItemEvent::Delta("foreign".into()),
            },
        };
        assert!(registry.resolve(&binding, &reassigned).is_err());
        assert!(
            registry
                .resolve(&BindingId::new("retired").unwrap(), &late)
                .is_err()
        );
    }

    /// Reject valid typed identities that claim a turn in a foreign conversation.
    #[test]
    fn foreign_thread_cannot_claim_preparing_turn() {
        let owner = ThreadOwner::root("session");
        let binding = BindingId::new("binding").unwrap();
        let mut registry =
            OwnershipRegistry::new(binding.clone(), owner.clone(), Default::default());
        let scope = InvocationScope::new(owner, "exchange");
        registry.prepare(scope.clone()).unwrap();
        let event = ProviderEvent::Turn {
            thread_id: ThreadId::new("foreign").unwrap(),
            turn_id: scope.turn_id().unwrap(),
            event: TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 0 }),
        };
        assert!(registry.resolve(&binding, &event).is_err());
    }
    /// Keep a certified background item under its original completed turn during later work.
    #[test]
    fn certified_background_item_retains_original_turn_after_completion() {
        use forge_provider_api::message::{ItemEvent, ItemStart};
        use forge_provider_api::tool::{
            InspectableArguments, TextUpdate, ToolDescriptor, ToolItemEvent, ToolSemantics,
        };
        let owner = ThreadOwner::root("session");
        let binding = BindingId::new("binding").unwrap();
        let mut registry =
            OwnershipRegistry::new(binding.clone(), owner.clone(), Default::default());
        let scope = InvocationScope::new(owner.clone(), "exchange");
        registry.prepare(scope.clone()).unwrap();
        let item_id = ItemId::new("background").unwrap();
        let item_event = |event| ProviderEvent::Item {
            thread_id: owner.thread_id(),
            turn_id: scope.turn_id().unwrap(),
            item_id: item_id.clone(),
            event,
        };
        registry
            .resolve(
                &binding,
                &item_event(ItemEvent::Started(ItemStart::CommandExecution(
                    ToolDescriptor {
                        name: "exec".into(),
                        title: "Command".into(),
                        semantics: ToolSemantics::Other,
                        arguments: InspectableArguments::new(serde_json::Value::Null),
                    },
                ))),
            )
            .unwrap();
        registry
            .resolve(
                &binding,
                &item_event(ItemEvent::Tool {
                    event: ToolItemEvent::Backgrounded,
                }),
            )
            .unwrap();
        let result = forge_provider_api::TurnResult::new(
            forge_provider_api::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            2,
            None,
            Vec::new(),
            Vec::new(),
            None,
        )
        .unwrap();
        let completed =
            crate::provider::test_support::completed_turn(scope.clone(), result).unwrap();
        registry
            .resolve(
                &binding,
                &ProviderEvent::Turn {
                    thread_id: owner.thread_id(),
                    turn_id: scope.turn_id().unwrap(),
                    event: TurnEvent::Completed(completed),
                },
            )
            .unwrap();
        assert!(registry.prepare(scope.clone()).is_err());
        registry
            .prepare(InvocationScope::new(owner.clone(), "later-exchange"))
            .unwrap();
        let late = item_event(ItemEvent::Tool {
            event: ToolItemEvent::Output(TextUpdate::Append("late".into())),
        });
        assert_eq!(
            registry.resolve(&binding, &late).unwrap(),
            EventScope::from(scope.clone())
        );
        let foreground = ProviderEvent::Item {
            thread_id: scope.owner().thread_id(),
            turn_id: scope.turn_id().unwrap(),
            item_id: ItemId::new("new-foreground").unwrap(),
            event: ItemEvent::Started(ItemStart::AgentMessage(AgentMessageStart {
                text: "invalid".into(),
                classification: MessageClassification::Unclassified,
            })),
        };
        assert!(registry.resolve(&binding, &foreground).is_err());
    }
    /// Admit a child only through trusted delegation and retain its exact original scope.
    #[test]
    fn observed_child_registration_requires_trusted_parent_thread() {
        use forge_provider_api::agent::{
            AgentActiveState, AgentDelegation, AgentDescription, AgentLifecycleEvent,
        };
        let owner = ThreadOwner::root("session");
        let binding = BindingId::new("binding").unwrap();
        let mut registry =
            OwnershipRegistry::new(binding.clone(), owner.clone(), Default::default());
        let parent = InvocationScope::new(owner.clone(), "exchange");
        registry.prepare(parent.clone()).unwrap();
        let agent_id = AgentId::new("child").unwrap();
        let child_owner = ThreadOwner::new(owner.session_id().clone(), agent_id.clone()).unwrap();
        let child = InvocationScope::new(child_owner.clone(), parent.exchange_id.clone());
        let address = forge_provider_api::TurnAddress {
            scope: child.clone(),
            thread_id: child_owner.thread_id(),
            turn_id: child.turn_id().unwrap(),
        };
        assert!(registry.admit_observed(address.clone()).is_err());
        let description = AgentDescription {
            parent_agent_id: Some(owner.agent_id().clone()),
            delegation: Some(AgentDelegation {
                parent_thread_id: owner.thread_id(),
                parent_turn_id: parent.turn_id().unwrap(),
                parent_item_id: None,
            }),
            thread_id: Some(child_owner.thread_id()),
            definition: None,
            nickname: None,
            task: None,
        };
        registry
            .resolve(
                &binding,
                &ProviderEvent::Agent {
                    agent_id: agent_id.clone(),
                    event: AgentEvent::Lifecycle {
                        event: AgentLifecycleEvent::Started {
                            agent: description.clone(),
                            state: AgentActiveState::Starting,
                        },
                    },
                },
            )
            .unwrap();
        registry.admit_observed(address.clone()).unwrap();
        let started = ProviderEvent::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            event: TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 2 }),
        };
        assert_eq!(
            registry.resolve(&binding, &started).unwrap(),
            EventScope::from(child)
        );
        let foreign = ProviderEvent::Agent {
            agent_id: AgentId::new("other-child").unwrap(),
            event: AgentEvent::Lifecycle {
                event: AgentLifecycleEvent::Started {
                    agent: description,
                    state: AgentActiveState::Starting,
                },
            },
        };
        assert!(registry.resolve(&binding, &foreign).is_err());
    }
}
