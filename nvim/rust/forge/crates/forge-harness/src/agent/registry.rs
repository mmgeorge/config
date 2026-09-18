use super::Agent;
use std::collections::HashMap;

/// Owns active child identities and provider-thread lookup for one session.
#[derive(Clone, Debug, Default)]
pub struct AgentRegistry {
    run_by_id: HashMap<String, Agent>,
    run_id_by_thread: HashMap<String, String>,
    execution_by_run_id: HashMap<String, AgentExecution>,
}

/// Tracks transient provider execution data outside the reusable agent identity.
#[derive(Clone, Debug, Default)]
pub struct AgentExecution {
    pub parent_exchange_id: Option<String>,
    pub parent_thread_id: Option<String>,
    pub active_turn_id: Option<String>,
    pub task: String,
}

impl AgentRegistry {
    /// Build the registry from durable runs while restoring provider-thread indexes.
    pub fn from_run_list(run_list: Vec<Agent>) -> Self {
        let mut registry = Self::default();
        for run in run_list {
            registry.insert(run);
        }
        registry
    }

    /// Insert or replace one run and synchronize its provider-thread lookup.
    pub fn insert(&mut self, run: Agent) {
        self.execution_by_run_id.entry(run.id.clone()).or_default();
        self.run_id_by_thread
            .retain(|_, indexed_run_id| indexed_run_id != &run.id);
        if let Some(thread_id) = run.provider_thread_id.as_ref() {
            self.run_id_by_thread
                .insert(thread_id.clone(), run.id.clone());
        }
        self.run_by_id.insert(run.id.clone(), run);
    }

    /// Resolve a run by its Harness identifier.
    pub fn get(&self, run_id: &str) -> Option<&Agent> {
        self.run_by_id.get(run_id)
    }

    /// Resolve a mutable run by its Harness identifier.
    pub fn get_mut(&mut self, run_id: &str) -> Option<&mut Agent> {
        self.run_by_id.get_mut(run_id)
    }

    /// Resolve transient execution data for one agent identity.
    pub fn execution(&self, run_id: &str) -> Option<&AgentExecution> {
        self.execution_by_run_id.get(run_id)
    }

    /// Create or resolve transient execution data for one agent identity.
    pub fn execution_mut(&mut self, run_id: &str) -> &mut AgentExecution {
        self.execution_by_run_id
            .entry(run_id.to_owned())
            .or_default()
    }

    /// Resolve a run through one provider child-thread identifier.
    pub fn get_by_thread(&self, thread_id: &str) -> Option<&Agent> {
        self.run_id_by_thread
            .get(thread_id)
            .and_then(|run_id| self.run_by_id.get(run_id))
    }

    /// Resolve one active run that has not received its provider child-thread identifier.
    pub fn resolve_unbound(
        &self,
        parent_exchange_id: Option<&str>,
        parent_thread_id: Option<&str>,
        turn_id: Option<&str>,
    ) -> Option<&Agent> {
        let mut candidate = self.run_by_id.values().filter(|run| {
            let execution = self.execution_by_run_id.get(&run.id);
            !run.is_primary()
                && run.provider_thread_id.is_none()
                && run.state.is_open()
                && parent_exchange_id.is_none_or(|exchange_id| {
                    execution.and_then(|value| value.parent_exchange_id.as_deref())
                        == Some(exchange_id)
                })
                && parent_thread_id.is_none_or(|thread_id| {
                    execution
                        .and_then(|value| value.parent_thread_id.as_deref())
                        .as_deref()
                        .is_none_or(|known_thread_id| known_thread_id == thread_id)
                })
                && turn_id.is_none_or(|active_turn_id| {
                    execution
                        .and_then(|value| value.active_turn_id.as_deref())
                        .as_deref()
                        .is_none_or(|known_turn_id| known_turn_id == active_turn_id)
                })
        });
        let run = candidate.next()?;
        candidate.next().is_none().then_some(run)
    }

    /// Return runs in stable creation order for snapshot presentation.
    pub fn list(&self) -> Vec<Agent> {
        let mut run_list = self
            .run_by_id
            .values()
            .filter(|agent| !agent.is_primary())
            .cloned()
            .collect::<Vec<_>>();
        run_list.sort_by_key(|run| (run.created_at_ms, run.id.clone()));
        run_list
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::agent::AgentState;
    #[test]
    fn registry_supports_multiple_instances_of_one_definition() {
        let first = Agent::pending("session", "explorer", "inspect Bevy", 1);
        let second = Agent::pending("session", "explorer", "inspect physics", 2);
        let registry = AgentRegistry::from_run_list(vec![first, second]);
        assert_eq!(registry.list().len(), 2);
    }

    #[test]
    fn registry_replaces_a_run_provider_thread_without_leaving_a_stale_index() {
        let mut run = Agent::pending("session", "explorer", "inspect Bevy", 1);
        run.provider_thread_id = Some("child-old".into());
        let mut registry = AgentRegistry::from_run_list(vec![run.clone()]);
        run.provider_thread_id = Some("child-new".into());
        registry.insert(run);
        assert!(registry.get_by_thread("child-old").is_none());
        assert_eq!(
            registry.get_by_thread("child-new").unwrap().definition,
            "explorer"
        );
    }

    #[test]
    fn registry_resolves_one_unbound_run_through_its_parent_identity() {
        let mut matching = Agent::pending("session", "explorer", "inspect Bevy", 1);
        matching.state = AgentState::Ready;
        let other = Agent::pending("session", "explorer", "inspect physics", 2);
        let mut registry = AgentRegistry::from_run_list(vec![matching.clone(), other.clone()]);
        *registry.execution_mut(&matching.id) = AgentExecution {
            parent_exchange_id: Some("parent-one".into()),
            parent_thread_id: Some("thread-one".into()),
            active_turn_id: Some("turn-one".into()),
            task: "inspect Bevy".into(),
        };
        registry.execution_mut(&other.id).parent_exchange_id = Some("parent-two".into());

        assert_eq!(
            registry
                .resolve_unbound(Some("parent-one"), Some("thread-one"), Some("turn-one"))
                .map(|run| run.id.as_str()),
            Some(matching.id.as_str())
        );
    }

    #[test]
    fn registry_refuses_to_guess_between_unbound_sibling_runs() {
        let first = Agent::pending("session", "explorer", "inspect Bevy", 1);
        let second = Agent::pending("session", "explorer", "inspect physics", 2);
        let mut registry = AgentRegistry::from_run_list(vec![first.clone(), second.clone()]);
        registry.execution_mut(&first.id).parent_exchange_id = Some("parent".into());
        registry.execution_mut(&second.id).parent_exchange_id = Some("parent".into());

        assert!(
            registry
                .resolve_unbound(Some("parent"), None, None)
                .is_none()
        );
    }
}
