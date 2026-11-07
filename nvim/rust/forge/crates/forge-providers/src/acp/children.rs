use anyhow::{Context, Result, ensure};
use forge_provider_api::*;
use serde_json::Value;
use std::collections::HashMap;

/// Retain native child associations independently of their parent's prompt lifetime.
#[derive(Default)]
pub struct AcpChildren {
    root: String,
    root_turn: Option<TurnAddress>,
    child: HashMap<String, ChildTurn>,
}

/// Preserve one child's original host turn, parent, and current native state.
#[derive(Clone)]
struct ChildTurn {
    address: TurnAddress,
    parent: String,
    agent_id: AgentId,
    description: AgentDescription,
    outcome: Option<AgentOutcome>,
}

/// Carry an observed child lifecycle without claiming provider resource collection.
pub(super) struct ChildObservation {
    pub activity: AdapterActivity,
    pub address: TurnAddress,
    pub outcome: Option<turn::TurnOutcome>,
}

impl AcpChildren {
    /// Create routing for an admitted root turn.
    pub fn new(root: String, scope: InvocationScope) -> Self {
        let root_turn = Some(TurnAddress {
            thread_id: scope.owner.thread_id(),
            turn_id: scope
                .turn_id()
                .expect("an admitted invocation has a nonempty identity"),
            scope,
        });
        Self {
            root,
            root_turn,
            child: HashMap::new(),
        }
    }

    /// Replace only root routing while retaining independently observed children.
    pub fn begin(&mut self, root: String, scope: InvocationScope) {
        let next = Self::new(root, scope);
        self.root = next.root;
        self.root_turn = next.root_turn;
    }

    /// Identify metadata from the selected native root conversation.
    pub fn is_root(&self, session: &str) -> bool {
        session == self.root
    }

    /// Resolve a trusted child lifecycle before any child-local item can be published.
    pub(super) fn lifecycle(
        &mut self,
        parent: &str,
        update: &Value,
    ) -> Result<Option<ChildObservation>> {
        let kind = update["sessionUpdate"].as_str().unwrap_or_default();
        if !matches!(kind, "subagent_spawned" | "subagent_state_update") {
            return Ok(None);
        }
        let id = update["subagentSessionId"]
            .as_str()
            .filter(|id| !id.trim().is_empty())
            .context("ACP child lifecycle has no session identity")?;
        ensure!(
            id != self.root,
            "ACP child cannot reuse root session identity"
        );
        let parent_address = self
            .address(parent, true)
            .context("ACP child has no known parent")?;
        let parent_agent_id = if parent == self.root {
            None
        } else {
            Some(
                self.child
                    .get(parent)
                    .context("ACP child parent is unknown")?
                    .agent_id
                    .clone(),
            )
        };
        let starts = kind == "subagent_spawned";
        let previously_known = self.child.contains_key(id);
        if starts {
            ensure!(
                parent == self.root
                    || self
                        .child
                        .get(parent)
                        .is_some_and(|parent| parent.outcome.is_none()),
                "ACP child cannot delegate from a completed native parent"
            );
            if let Some(child) = self.child.get(id) {
                ensure!(child.parent == parent, "ACP child parent changed");
                ensure!(
                    child.outcome.is_none(),
                    "ACP child native identity was reused after closure"
                );
            } else {
                let mut ancestor = parent;
                while let Some(child) = self.child.get(ancestor) {
                    ensure!(ancestor != id, "ACP child cannot become its own ancestor");
                    ancestor = &child.parent;
                }
                let agent_id = AgentId::new(uuid::Uuid::new_v4().to_string())?;
                let owner = ThreadOwner::new(
                    parent_address.scope.owner.session_id().clone(),
                    agent_id.clone(),
                )?;
                let child_scope =
                    InvocationScope::new(owner.clone(), parent_address.scope.exchange_id.clone());
                let address = TurnAddress {
                    thread_id: owner.thread_id(),
                    turn_id: child_scope.turn_id()?,
                    scope: child_scope,
                };
                let description = AgentDescription {
                    parent_agent_id,
                    delegation: Some(AgentDelegation {
                        parent_thread_id: parent_address.thread_id,
                        parent_turn_id: parent_address.turn_id,
                        parent_item_id: None,
                    }),
                    thread_id: Some(address.thread_id.clone()),
                    definition: None,
                    nickname: None,
                    task: None,
                };
                self.child.insert(
                    id.into(),
                    ChildTurn {
                        address,
                        parent: parent.into(),
                        agent_id,
                        description,
                        outcome: None,
                    },
                );
            }
        }
        let mut child = self
            .child
            .get(id)
            .context("ACP child state has no known child")?
            .clone();
        ensure!(child.parent == parent, "ACP child parent changed");
        if let Some(name) = update["name"].as_str() {
            child.description.definition = Some(name.into());
            child.description.nickname = Some(name.into());
        }
        if let Some(task) = update["task"]
            .as_str()
            .or_else(|| update["prompt"].as_str())
        {
            child.description.task = Some(task.into());
        }
        let (event, outcome) = if starts {
            let event = if previously_known {
                AgentLifecycleEvent::Updated {
                    agent: child.description.clone(),
                    state: AgentActiveState::Running,
                }
            } else {
                AgentLifecycleEvent::Started {
                    agent: child.description.clone(),
                    state: AgentActiveState::Running,
                }
            };
            (event, None)
        } else {
            let state = update["state"]
                .as_str()
                .context("ACP child state is missing")?;
            let native_outcome = match state {
                "starting" => None,
                "running" | "in_progress" => None,
                "waiting" | "waiting_for_input" => None,
                "completed" => Some(AgentOutcome::Completed),
                "failed" => Some(AgentOutcome::Failed(
                    ProviderFailure::new(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::Unknown,
                    )
                    .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain),
                )),
                "cancelled" | "disconnected" => Some(AgentOutcome::Interrupted),
                "closed" => Some(AgentOutcome::Closed),
                _ => anyhow::bail!("unsupported ACP child state"),
            };
            if let Some(outcome) = native_outcome {
                if let Some(retained) = child.outcome {
                    ensure!(retained == outcome, "ACP child terminal outcome changed");
                    return Ok(None);
                }
                child.outcome = Some(outcome);
                let turn_outcome = match outcome {
                    AgentOutcome::Completed => turn::TurnOutcome::Completed {
                        stop_reason: turn::TurnCompletionReason::Normal,
                    },
                    AgentOutcome::Failed(error) => turn::TurnOutcome::Failed {
                        error,
                        retry: turn::RetryDecision::NotAllowed,
                    },
                    AgentOutcome::Interrupted | AgentOutcome::Closed => {
                        turn::TurnOutcome::Interrupted
                    }
                };
                (
                    AgentLifecycleEvent::Finished {
                        agent: child.description.clone(),
                        outcome,
                    },
                    Some(turn_outcome),
                )
            } else {
                ensure!(
                    child.outcome.is_none(),
                    "ACP child resumed after native completion"
                );
                let state = match state {
                    "starting" => AgentActiveState::Starting,
                    "running" | "in_progress" => AgentActiveState::Running,
                    "waiting" | "waiting_for_input" => AgentActiveState::Waiting,
                    _ => unreachable!("terminal states were handled above"),
                };
                (
                    AgentLifecycleEvent::Updated {
                        agent: child.description.clone(),
                        state,
                    },
                    None,
                )
            }
        };
        let observation = ChildObservation {
            address: child.address.clone(),
            outcome,
            activity: AdapterActivity::Agent {
                agent_id: child.agent_id.clone(),
                event: AgentEvent::Lifecycle { event },
            },
        };
        self.child.insert(id.into(), child);
        Ok(Some(observation))
    }

    /// Resolve an item's original native session without assigning unknown children to root.
    pub fn address(&self, session: &str, allow_closed: bool) -> Option<TurnAddress> {
        if self.is_root(session) {
            return self.root_turn.clone();
        }
        let child = self.child.get(session)?;
        (child.outcome.is_none() || allow_closed).then(|| child.address.clone())
    }

    /// Resolve an explicitly retained child owner for delayed item observations.
    pub fn observed_address(&self, thread_id: &ThreadId, turn_id: &TurnId) -> Option<TurnAddress> {
        self.child
            .values()
            .find(|child| {
                &child.address.thread_id == thread_id && &child.address.turn_id == turn_id
            })
            .map(|child| child.address.clone())
    }

    /// Resolve thread-scoped observations without assigning them to the current root turn.
    pub fn thread_address(&self, thread_id: &ThreadId) -> Option<TurnAddress> {
        self.child
            .values()
            .find(|child| &child.address.thread_id == thread_id)
            .map(|child| child.address.clone())
    }

    /// Get the admitted root response-cycle identity.
    pub fn root_address(&self) -> Option<TurnAddress> {
        self.root_turn.clone()
    }
}

/// Read the native observation clock without a narrowing integer cast.
pub fn timestamp() -> i64 {
    i64::try_from(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis(),
    )
    .unwrap_or(i64::MAX)
}

#[cfg(test)]
mod test {
    use super::*;
    use serde_json::json;

    /// A root prompt successor cannot reassign an independently running child's items.
    #[test]
    fn child_owner_survives_parent_completion_and_successor_prompt() -> Result<()> {
        let scope = InvocationScope::new(ThreadOwner::root("host"), "exchange");
        let mut children = AcpChildren::new("root".into(), scope);
        let observed = children
            .lifecycle(
                "root",
                &json!({"sessionUpdate":"subagent_spawned",
            "subagentSessionId":"child"}),
            )?
            .unwrap();
        assert_ne!(
            observed.address.thread_id,
            children.root_address().unwrap().thread_id
        );
        let child = observed.address;
        children.begin(
            "root".into(),
            InvocationScope::new(ThreadOwner::root("host"), "next"),
        );
        assert_eq!(children.address("child", false), Some(child.clone()));
        let finish = children
            .lifecycle(
                "root",
                &json!({"sessionUpdate":"subagent_state_update",
            "subagentSessionId":"child", "state":"completed"}),
            )?
            .unwrap();
        assert_eq!(finish.address, child);
        assert!(finish.outcome.unwrap().permits_automatic_continuation());
        assert!(children.address("child", false).is_none());
        assert!(children.address("child", true).is_some());
        Ok(())
    }

    /// Supplied foreign parent identities never mutate an admitted child relationship.
    #[test]
    fn child_parent_conflict_is_rejected() -> Result<()> {
        let mut children = AcpChildren::new(
            "root".into(),
            InvocationScope::new(ThreadOwner::root("host"), "exchange"),
        );
        children.lifecycle(
            "root",
            &json!({"sessionUpdate":"subagent_spawned", "subagentSessionId":"child"}),
        )?;
        assert!(children.lifecycle("foreign", &json!({"sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":"failed"})).is_err());
        assert!(children.address("child", false).is_some());
        Ok(())
    }
    /// Active refinement and terminal replay cannot create another native child occurrence.
    #[test]
    fn child_refinement_and_terminal_replay_preserve_original_owner_and_reject_ambiguous_restart()
    -> Result<()> {
        let mut children = AcpChildren::new(
            "root".into(),
            InvocationScope::new(ThreadOwner::root("host"), "first"),
        );
        let spawn = json!({"sessionUpdate":"subagent_spawned", "subagentSessionId":"child", "name":"First"});
        let original = children.lifecycle("root", &spawn)?.unwrap();
        let refined = children.lifecycle("root", &json!({"sessionUpdate":"subagent_spawned", "subagentSessionId":"child", "name":"Refined"}))?.unwrap();
        assert_eq!(refined.address, original.address);
        assert!(
            matches!(refined.activity, AdapterActivity::Agent { event: AgentEvent::Lifecycle {
            event: AgentLifecycleEvent::Updated { agent, .. } }, .. } if agent.nickname.as_deref() == Some("Refined"))
        );
        let terminal = json!({"sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":"completed"});
        children.lifecycle("root", &terminal)?;
        assert!(children.lifecycle("root", &terminal)?.is_none());
        assert!(children.lifecycle("root", &json!({"sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":"failed"})).is_err());
        children.begin(
            "root".into(),
            InvocationScope::new(ThreadOwner::root("host"), "successor"),
        );
        assert!(children.lifecycle("root", &spawn).is_err());
        assert_eq!(children.address("child", true), Some(original.address));
        Ok(())
    }
}
