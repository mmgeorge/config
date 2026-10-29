use crate::agent::{AgentExecutionState, AgentLifecycleEvent};
use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
use crate::turn::TurnOutcome;
use anyhow::{Context, Result, ensure};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Default)]
/// Routes native child sessions into separate Harness execution lifecycles.
pub(super) struct AcpChildren {
    root: String,
    child: HashMap<String, ChildExecution>,
}

impl AcpChildren {
    /// Start one prompt's routing without retaining children from an earlier prompt.
    pub(super) fn new(root: String) -> Self {
        Self {
            root,
            child: HashMap::new(),
        }
    }

    /// Identify metadata that belongs to the selected provider conversation.
    pub(super) fn is_root(&self, session: &str) -> bool {
        self.root.is_empty() || session == self.root
    }

    /// Normalize advertised child lifecycle updates before their content can arrive.
    pub(super) fn lifecycle(
        &mut self,
        parent: &str,
        update: &Value,
    ) -> Result<Option<Vec<BackendEvent>>> {
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
            let address = ProviderAddress {
                owner: None,
                thread_id: id.into(),
                turn_id: format!("acp-child:{}", uuid::Uuid::new_v4()),
            };
            self.child.insert(
                id.into(),
                ChildExecution {
                    address: address.clone(),
                    parent: parent.into(),
                    closed: false,
                },
            );
            (AgentExecutionState::Running, TurnBoundary::Started, address)
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
                TurnBoundary::Finished { outcome },
                child.address.clone(),
            )
        };
        let lifecycle = AgentLifecycleEvent {
            operation: kind.into(),
            starts_child,
            parent_thread_id: Some(parent.into()),
            provider_thread_id: Some(id.into()),
            turn_id: Some(address.turn_id.clone()),
            definition: update["name"].as_str().map(str::to_owned),
            nickname: update["name"].as_str().map(str::to_owned),
            task: update["task"]
                .as_str()
                .or_else(|| update["prompt"].as_str())
                .map(str::to_owned),
            status: state,
        };
        let lifecycle = BackendEvent {
            message: None,
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "agent_lifecycle".into(),
            text: None,
            data: serde_json::to_value(lifecycle)?,
            activity: None,
            summary: None,
            checklist_update: None,
        };
        let boundary = BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(address),
            turn_boundary: Some(boundary),
            kind: if starts_child {
                "turn_started"
            } else {
                "turn_completed"
            }
            .into(),
            text: None,
            data: Value::Null,
            activity: None,
            summary: None,
            checklist_update: None,
        };
        Ok(Some(if starts_child {
            vec![lifecycle, boundary]
        } else {
            vec![boundary, lifecycle]
        }))
    }

    /// Attribute child content and drop unknown or completed child streams.
    pub(super) fn route(&self, session: &str, event: &mut BackendEvent) -> bool {
        if self.is_root(session) {
            return true;
        }
        let Some(child) = self.child.get(session) else {
            return false;
        };
        if child.closed && event.kind != "usage" {
            return false;
        }
        event.address = Some(child.address.clone());
        true
    }

    /// Interrupt unclosed child runs when their owning prompt stops being observed.
    pub(super) fn finish(&mut self) -> Result<Vec<BackendEvent>> {
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
            events.push(BackendEvent {
                message: None,
                received_at_ms: None,
                address: Some(child.address.clone()),
                turn_boundary: Some(TurnBoundary::Finished {
                    outcome: TurnOutcome::Interrupted,
                }),
                kind: "turn_completed".into(),
                text: None,
                data: Value::Null,
                activity: None,
                summary: None,
                checklist_update: None,
            });
            events.push(BackendEvent {
                message: None,
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: "agent_lifecycle".into(),
                text: None,
                data: serde_json::to_value(AgentLifecycleEvent {
                    operation: "subagent_state_update".into(),
                    starts_child: false,
                    parent_thread_id: Some(child.parent.clone()),
                    provider_thread_id: Some(id.clone()),
                    turn_id: Some(child.address.turn_id.clone()),
                    definition: None,
                    nickname: None,
                    task: None,
                    status: AgentExecutionState::Interrupted,
                })?,
                activity: None,
                summary: None,
                checklist_update: None,
            });
        }
        for child in self.child.values_mut() {
            child.closed = true;
        }
        Ok(events)
    }
}

struct ChildExecution {
    address: ProviderAddress,
    parent: String,
    closed: bool,
}

#[cfg(test)]
mod test {
    use super::*;
    use serde_json::json;

    #[test]
    fn child_routing_preserves_all_sessions_beyond_former_limit() -> Result<()> {
        let mut children = AcpChildren::new("root".into());
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
            assert_eq!(events[1].address.as_ref().unwrap().thread_id, id);
        }
        let events = children.finish()?;
        assert_eq!(events.len(), 600);
        assert_eq!(
            events
                .iter()
                .filter(|event| event.turn_boundary.is_some())
                .count(),
            300
        );
        assert!(children.finish()?.is_empty());
        Ok(())
    }

    #[test]
    fn child_generations_keep_content_and_completion_out_of_the_root() -> Result<()> {
        let mut children = AcpChildren::new("root".into());
        let spawn = json!({"sessionUpdate":"subagent_spawned","subagentSessionId":"child","name":"Reviewer","task":"Review"});
        let admitted = children.lifecycle("root", &spawn)?.unwrap();
        assert_eq!(admitted[0].kind, "agent_lifecycle");
        assert_eq!(admitted[1].turn_boundary, Some(TurnBoundary::Started));
        let lifecycle: AgentLifecycleEvent = serde_json::from_value(admitted[0].data.clone())?;
        assert!(lifecycle.starts_child);
        assert_eq!(lifecycle.provider_thread_id.as_deref(), Some("child"));
        let mut content = BackendEvent {
            message: None,
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("Child output".into()),
            data: Value::Null,
            activity: None,
            summary: None,
            checklist_update: None,
        };
        assert!(!children.route("unannounced", &mut content));
        assert!(children.route("child", &mut content));
        assert_eq!(content.address, admitted[1].address);
        assert!(children.lifecycle("root", &spawn)?.unwrap().is_empty());
        let nested = children
            .lifecycle(
                "child",
                &json!({"sessionUpdate":"subagent_spawned","subagentSessionId":"nested"}),
            )?
            .unwrap();
        assert!(children.route("nested", &mut content));
        assert_eq!(content.address, nested[1].address);
        let finish = json!({"sessionUpdate":"subagent_state_update","subagentSessionId":"child","state":"failed"});
        assert!(
            children
                .lifecycle("another-parent", &finish)?
                .unwrap()
                .is_empty()
        );
        let finished = children.lifecycle("root", &finish)?.unwrap();
        assert_eq!(
            finished[0].turn_boundary,
            Some(TurnBoundary::Finished {
                outcome: TurnOutcome::Failed
            })
        );
        assert!(!children.route("child", &mut content));
        content.kind = "usage".into();
        assert!(children.route("child", &mut content));
        assert!(children.lifecycle("root", &finish)?.unwrap().is_empty());
        assert!(children.lifecycle("nested", &spawn).is_err());
        let restarted = children.lifecycle("root", &spawn)?.unwrap();
        assert_ne!(restarted[1].address, admitted[1].address);
        let interrupted = children.finish()?;
        assert_eq!(interrupted.len(), 4);
        assert_eq!(interrupted[0].address, nested[1].address);
        assert_eq!(interrupted[2].address, restarted[1].address);
        assert_eq!(
            interrupted[0].turn_boundary,
            Some(TurnBoundary::Finished {
                outcome: TurnOutcome::Interrupted
            })
        );
        assert!(children.finish()?.is_empty());
        content.kind = "assistant_message".into();
        assert!(!children.route("nested", &mut content));
        Ok(())
    }
}
