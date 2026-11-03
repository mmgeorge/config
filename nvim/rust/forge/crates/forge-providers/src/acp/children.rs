use anyhow::{Context, Result, ensure};
use forge_provider_api::agent::{AgentExecutionState, AgentLifecycleEvent, AgentLifecycleKind};
use forge_provider_api::turn::TurnOutcome;
use forge_provider_api::{AgentUpdate, InvocationScope, ProviderEvent, TurnAddress, TurnUpdate};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Default)]
/// Routes native child sessions into separate Harness execution lifecycles.
pub struct AcpChildren {
    root: String,
    root_execution: Option<TurnAddress>,
    child: HashMap<String, ChildExecution>,
}

impl AcpChildren {
    /// Start one prompt's routing without retaining children from an earlier prompt.
    pub fn new(root: String, scope: InvocationScope) -> Self {
        let root_execution = TurnAddress {
            scope,
            thread_id: root.clone(),
            turn_id: format!("acp:{}", uuid::Uuid::new_v4()).into(),
        };
        Self {
            root,
            root_execution: Some(root_execution),
            child: HashMap::new(),
        }
    }

    /// Identify metadata that belongs to the selected provider conversation.
    pub fn is_root(&self, session: &str) -> bool {
        self.root.is_empty() || session == self.root
    }

    /// Normalize advertised child lifecycle updates before their content can arrive.
    pub fn lifecycle(
        &mut self,
        parent: &str,
        update: &Value,
    ) -> Result<Option<Vec<ProviderEvent>>> {
        let kind = update["sessionUpdate"].as_str().unwrap_or_default();
        if !matches!(kind, "subagent_spawned" | "subagent_state_update") {
            return Ok(None);
        }
        let id = update["subagentSessionId"]
            .as_str()
            .filter(|id| !id.is_empty())
            .context("ACP child lifecycle has no session identity")?;
        ensure!(
            id != self.root,
            "ACP child cannot reuse the root session identity"
        );
        if parent != self.root && self.child.get(parent).is_none_or(|child| child.closed) {
            return Ok(Some(Vec::new()));
        }
        let starts_child = kind == "subagent_spawned";
        let (state, boundary, address) = if starts_child {
            if self.child.get(id).is_some_and(|child| !child.closed) {
                return Ok(Some(Vec::new()));
            }
            let mut ancestor = parent;
            while let Some(child) = self.child.get(ancestor) {
                ensure!(ancestor != id, "ACP child cannot become its own ancestor");
                ancestor = &child.parent;
            }
            let address = TurnAddress {
                scope: self
                    .root_execution
                    .as_ref()
                    .context("ACP children have no invocation")?
                    .scope
                    .clone(),
                thread_id: id.into(),
                turn_id: format!("acp-child:{}", uuid::Uuid::new_v4()).into(),
            };
            self.child.insert(
                id.into(),
                ChildExecution {
                    address: address.clone(),
                    parent: parent.into(),
                    closed: false,
                },
            );
            (
                AgentExecutionState::Running,
                TurnUpdate::Started {
                    started_at_ms: timestamp(),
                },
                address,
            )
        } else {
            let Some(child) = self.child.get_mut(id) else {
                return Ok(Some(Vec::new()));
            };
            if child.closed || child.parent != parent {
                return Ok(Some(Vec::new()));
            }
            let (state, outcome) = match update["state"].as_str() {
                Some("completed") => (AgentExecutionState::Completed, TurnOutcome::Completed),
                Some("failed") => (AgentExecutionState::Failed, TurnOutcome::Failed),
                Some("cancelled" | "disconnected") => {
                    (AgentExecutionState::Interrupted, TurnOutcome::Interrupted)
                }
                _ => return Ok(Some(Vec::new())),
            };
            child.closed = true;
            (
                state,
                TurnUpdate::Finished {
                    outcome,
                    completed_at_ms: timestamp(),
                },
                child.address.clone(),
            )
        };
        let lifecycle = AgentLifecycleEvent {
            agent_id: id.to_owned().try_into()?,
            parent_agent_id: (parent != self.root)
                .then(|| parent.to_owned().try_into())
                .transpose()?,
            conversation_id: Some(id.to_owned().try_into()?),
            kind: if starts_child {
                AgentLifecycleKind::Started
            } else {
                AgentLifecycleKind::Finished
            },
            definition: update["name"].as_str().map(str::to_owned),
            nickname: update["name"].as_str().map(str::to_owned),
            task: update["task"]
                .as_str()
                .or_else(|| update["prompt"].as_str())
                .map(str::to_owned),
            state,
        };
        let lifecycle = ProviderEvent::Agent {
            scope: address.scope.clone(),
            update: AgentUpdate::Lifecycle { event: lifecycle },
        };
        let boundary = ProviderEvent::Turn {
            address,
            update: boundary,
        };
        Ok(Some(if starts_child {
            vec![lifecycle, boundary]
        } else {
            vec![boundary, lifecycle]
        }))
    }

    /// Attribute child content and drop unknown or completed child streams.
    pub fn address(&self, session: &str, allow_closed: bool) -> Option<TurnAddress> {
        if self.is_root(session) {
            let mut address = self.root_execution.clone()?;
            if address.thread_id.is_empty() {
                address.thread_id = session.into();
            }
            return Some(address);
        }
        let child = self.child.get(session)?;
        (!child.closed || allow_closed).then(|| child.address.clone())
    }

    pub fn root_address(&self) -> Option<TurnAddress> {
        self.address(&self.root, false)
    }

    /// Interrupt unclosed child runs when their owning prompt stops being observed.
    pub fn finish(&mut self) -> Result<Vec<ProviderEvent>> {
        let mut pending = self
            .child
            .iter()
            .filter(|(_, child)| !child.closed)
            .map(|(id, child)| {
                let mut depth = 0;
                let mut parent = child.parent.as_str();
                while let Some(ancestor) = self.child.get(parent) {
                    depth += 1;
                    parent = &ancestor.parent;
                }
                (std::cmp::Reverse(depth), id, child)
            })
            .collect::<Vec<_>>();
        pending.sort_by(|left, right| left.0.cmp(&right.0).then(left.1.cmp(right.1)));
        let mut events = Vec::new();
        for (_, id, child) in pending {
            events.push(ProviderEvent::Turn {
                address: child.address.clone(),
                update: TurnUpdate::Finished {
                    outcome: TurnOutcome::Interrupted,
                    completed_at_ms: timestamp(),
                },
            });
            events.push(ProviderEvent::Agent {
                scope: child.address.scope.clone(),
                update: AgentUpdate::Lifecycle {
                    event: AgentLifecycleEvent {
                        agent_id: id.clone().try_into()?,
                        parent_agent_id: (child.parent != self.root)
                            .then(|| child.parent.clone().try_into())
                            .transpose()?,
                        conversation_id: Some(id.clone().try_into()?),
                        kind: AgentLifecycleKind::Finished,
                        definition: None,
                        nickname: None,
                        task: None,
                        state: AgentExecutionState::Interrupted,
                    },
                },
            });
        }
        for child in self.child.values_mut() {
            child.closed = true;
        }
        Ok(events)
    }
}

struct ChildExecution {
    address: TurnAddress,
    parent: String,
    closed: bool,
}

pub fn timestamp() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

#[cfg(test)]
mod test {
    use super::*;
    use serde_json::json;

    /// Normal prompts under one retained invocation have distinct terminal anchors.
    #[test]
    fn continued_prompt_gets_a_new_root_execution_identity() {
        let scope =
            InvocationScope::new(forge_provider_api::ExecutionOwner::root("host"), "exchange");
        let first = AcpChildren::new("native".into(), scope.clone())
            .root_address()
            .unwrap();
        let second = AcpChildren::new("native".into(), scope)
            .root_address()
            .unwrap();
        assert_eq!(first.scope, second.scope);
        assert_eq!(first.thread_id, second.thread_id);
        assert_ne!(first.turn_id, second.turn_id);
    }

    #[test]
    fn child_routing_preserves_all_sessions_beyond_former_limit() -> Result<()> {
        let mut children = AcpChildren::new(
            "root".into(),
            InvocationScope::new(forge_provider_api::ExecutionOwner::root("test"), "exchange"),
        );
        for index in 0..300 {
            let id = format!("child-{index}");
            let events = children
                .lifecycle(
                    "root",
                    &json!({
                        "sessionUpdate": "subagent_spawned",
                        "subagentSessionId": id,
                    }),
                )?
                .unwrap();
            assert_eq!(events.len(), 2);
            assert_eq!(events[1].turn_address().unwrap().thread_id, id);
        }
        let events = children.finish()?;
        assert_eq!(events.len(), 600);
        assert_eq!(
            events
                .iter()
                .filter(|event| matches!(event, ProviderEvent::Turn { .. }))
                .count(),
            300
        );
        assert!(children.finish()?.is_empty());
        Ok(())
    }

    #[test]
    fn child_generations_keep_content_and_completion_out_of_the_root() -> Result<()> {
        let mut children = AcpChildren::new(
            "root".into(),
            InvocationScope::new(forge_provider_api::ExecutionOwner::root("test"), "exchange"),
        );
        let spawn = json!({"sessionUpdate":"subagent_spawned","subagentSessionId":"child","name":"Reviewer","task":"Review"});
        let admitted = children.lifecycle("root", &spawn)?.unwrap();
        assert!(matches!(admitted[0], ProviderEvent::Agent { .. }));
        assert!(matches!(
            admitted[1],
            ProviderEvent::Turn {
                update: TurnUpdate::Started { .. },
                ..
            }
        ));
        let ProviderEvent::Agent {
            update: AgentUpdate::Lifecycle { event: lifecycle },
            ..
        } = &admitted[0]
        else {
            panic!("expected child lifecycle")
        };
        assert_eq!(lifecycle.kind, AgentLifecycleKind::Started);
        assert_eq!(lifecycle.agent_id.as_str(), "child");
        assert!(children.address("unannounced", false).is_none());
        assert_eq!(
            children.address("child", false).as_ref(),
            admitted[1].turn_address()
        );
        assert!(children.lifecycle("root", &spawn)?.unwrap().is_empty());
        let nested = children
            .lifecycle(
                "child",
                &json!({"sessionUpdate":"subagent_spawned","subagentSessionId":"nested"}),
            )?
            .unwrap();
        assert_eq!(
            children.address("nested", false).as_ref(),
            nested[1].turn_address()
        );
        let finish = json!({"sessionUpdate":"subagent_state_update","subagentSessionId":"child","state":"failed"});
        assert!(
            children
                .lifecycle("another-parent", &finish)?
                .unwrap()
                .is_empty()
        );
        let finished = children.lifecycle("root", &finish)?.unwrap();
        assert!(matches!(
            finished[0],
            ProviderEvent::Turn {
                update: TurnUpdate::Finished {
                    outcome: TurnOutcome::Failed,
                    ..
                },
                ..
            }
        ));
        assert!(children.address("child", false).is_none());
        assert!(children.address("child", true).is_some());
        assert!(children.lifecycle("root", &finish)?.unwrap().is_empty());
        assert!(children.lifecycle("nested", &spawn).is_err());
        let restarted = children.lifecycle("root", &spawn)?.unwrap();
        assert_ne!(restarted[1].turn_address(), admitted[1].turn_address());
        let interrupted = children.finish()?;
        assert_eq!(interrupted.len(), 4);
        assert_eq!(interrupted[0].turn_address(), nested[1].turn_address());
        assert_eq!(interrupted[2].turn_address(), restarted[1].turn_address());
        assert!(matches!(
            interrupted[0],
            ProviderEvent::Turn {
                update: TurnUpdate::Finished {
                    outcome: TurnOutcome::Interrupted,
                    ..
                },
                ..
            }
        ));
        assert!(children.finish()?.is_empty());
        assert!(children.address("nested", false).is_none());
        Ok(())
    }
}
