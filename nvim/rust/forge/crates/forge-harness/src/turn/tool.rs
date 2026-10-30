use crate::backend::{
    ProviderChangeSet, TextUpdate, ToolAddress, ToolInvocation, ToolOutcome, ToolUpdate,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Execution state independent of provider-specific status spelling.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ToolState {
    Running,
    Completed,
    Failed,
    Denied,
    Cancelled,
    Interrupted,
}

/// Owns one provider tool call across progress, output, and completion events.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ToolCall {
    /// Time at which Harness first observed this call running.
    pub started_at_ms: Option<i64>,
    /// First terminal observation, retained across replay and session reopening.
    pub completed_at_ms: Option<i64>,
    #[serde(default, alias = "task_id")]
    pub checklist_item_id: Option<String>,
    pub id: String,
    pub kind: String,
    pub title: String,
    pub output: super::ToolOutputText,
    pub status: String,
    pub failed: bool,
    #[serde(default)]
    pub change: ProviderChangeSet,
}

impl ToolCall {
    /// Returns provider or adapter-observed runtime, frozen at completion and absent without a start.
    pub fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        let start = self.started_at_ms?;
        let end = self
            .completed_at_ms
            .or_else(|| (self.state() == ToolState::Running).then_some(now_ms))?;
        Some(end.saturating_sub(start).max(0) as u64)
    }

    /// Resolve the provider status into a stable execution state.
    pub fn state(&self) -> ToolState {
        match self.status.to_ascii_lowercase().as_str() {
            "completed" | "complete" | "success" | "succeeded" => ToolState::Completed,
            "failed" | "error" => ToolState::Failed,
            "denied" | "declined" | "rejected" => ToolState::Denied,
            "cancelled" | "canceled" => ToolState::Cancelled,
            "interrupted" => ToolState::Interrupted,
            _ => ToolState::Running,
        }
    }
}

/// Stores tool lifecycle records in first-seen provider order.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ToolStore {
    order: Vec<String>,
    item: HashMap<String, ToolCall>,
    execution: HashMap<String, ToolExecution>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ToolExecution {
    pub address: ToolAddress,
    pub invocation: ToolInvocation,
    pub input: String,
    pub outcome: Option<ToolOutcome>,
}

impl ToolStore {
    pub(crate) fn omit_output(&mut self) {
        for tool in self.item.values_mut() {
            tool.output = super::ToolOutputText::default();
        }
    }
    /// Visit calls in first-seen provider order.
    pub(crate) fn iter(&self) -> impl Iterator<Item = &ToolCall> {
        self.order.iter().filter_map(|id| self.item.get(id))
    }

    /// Settle unfinished calls without replacing provider-reported terminal outcomes.
    pub(crate) fn finish(&mut self, outcome: super::TurnOutcome, now_ms: i64) {
        let status = match outcome {
            super::TurnOutcome::Failed => "failed",
            super::TurnOutcome::Cancelled => "cancelled",
            // A completed turn does not prove an unreported tool succeeded.
            super::TurnOutcome::Completed | super::TurnOutcome::Interrupted => "interrupted",
        };
        for tool in self.item.values_mut() {
            if tool.state() == ToolState::Running {
                tool.completed_at_ms = Some(now_ms.max(tool.started_at_ms.unwrap_or(now_ms)));
                tool.status = status.into();
                tool.failed = outcome == super::TurnOutcome::Failed;
                if let Some(execution) = self.execution.get_mut(&tool.id) {
                    execution.outcome = Some(match outcome {
                        super::TurnOutcome::Failed => ToolOutcome::Failed {
                            diagnostic: crate::backend::ToolDiagnostic {
                                code: None,
                                message: "provider execution failed before tool completion".into(),
                            },
                        },
                        super::TurnOutcome::Cancelled => ToolOutcome::Cancelled,
                        super::TurnOutcome::Completed | super::TurnOutcome::Interrupted => {
                            ToolOutcome::Interrupted
                        }
                    });
                }
            }
        }
    }

    /// Assign a call to its first observed task without changing historical attribution.
    pub(crate) fn attribute(&mut self, id: &str, checklist_item_id: &str) -> bool {
        let Some(tool) = self.item.get_mut(id) else {
            return false;
        };
        if tool.checklist_item_id.is_some() {
            return false;
        }
        tool.checklist_item_id = Some(checklist_item_id.to_owned());
        true
    }

    pub(crate) fn merge(
        &mut self,
        address: &ToolAddress,
        update: &ToolUpdate,
        now_ms: i64,
    ) -> Result<(), crate::backend::BackendError> {
        let id = address.tool_id.as_str();
        let protocol = |message: &str| crate::backend::BackendError::Protocol {
            message: message.into(),
        };
        if !self.execution.contains_key(id) {
            let ToolUpdate::Requested { invocation } = update else {
                return Err(protocol("provider tool update arrived before its request"));
            };
            self.order.push(id.to_owned());
            self.execution.insert(
                id.to_owned(),
                ToolExecution {
                    address: address.clone(),
                    invocation: invocation.clone(),
                    input: String::new(),
                    outcome: None,
                },
            );
        }
        let execution = self.execution.get_mut(id).expect("admitted tool execution");
        if execution.address != *address {
            return Err(protocol(
                "provider tool update changed its execution address",
            ));
        }
        let tool = self.item.entry(id.to_owned()).or_insert_with(|| ToolCall {
            started_at_ms: None,
            completed_at_ms: None,
            checklist_item_id: None,
            id: id.to_owned(),
            kind: tool_kind(execution.invocation.kind),
            title: execution.invocation.title.clone(),
            output: super::ToolOutputText::default(),
            status: "running".into(),
            failed: false,
            change: ProviderChangeSet::default(),
        });
        match update {
            ToolUpdate::Requested { invocation } => {
                if execution.invocation != *invocation {
                    return Err(protocol(
                        "provider tool request replay changed its invocation",
                    ));
                }
            }
            ToolUpdate::Input { update } => match update {
                TextUpdate::Append(text) => execution.input.push_str(text),
                TextUpdate::Replace(text) => execution.input.clone_from(text),
            },
            ToolUpdate::Started if tool.state() == ToolState::Running => {
                tool.started_at_ms.get_or_insert(now_ms);
            }
            ToolUpdate::Output { update } => match update {
                TextUpdate::Append(text) => tool.output.push_str(text),
                TextUpdate::Replace(text) => tool.output.replace(text),
            },
            ToolUpdate::Changes { changes } => {
                tool.change.clone_from(changes);
                tool.kind = tool_kind(crate::backend::ToolActivityKind::FileChange);
            }
            ToolUpdate::Finished { outcome } if tool.state() == ToolState::Running => {
                tool.status = match outcome {
                    ToolOutcome::Succeeded => "completed",
                    ToolOutcome::Failed { .. } => "failed",
                    ToolOutcome::Denied { .. } => "denied",
                    ToolOutcome::Cancelled => "cancelled",
                    ToolOutcome::Interrupted => "interrupted",
                }
                .into();
                execution.outcome = Some(outcome.clone());
            }
            ToolUpdate::Finished { outcome } if execution.outcome.as_ref() != Some(outcome) => {
                return Err(protocol(
                    "provider tool terminal outcome contradicts its settled state",
                ));
            }
            ToolUpdate::Started | ToolUpdate::Finished { .. } | ToolUpdate::SkillRead { .. } => {}
        }
        tool.failed = tool_failed(&tool.status);
        if tool.state() != ToolState::Running && tool.completed_at_ms.is_none() {
            tool.completed_at_ms = Some(now_ms.max(tool.started_at_ms.unwrap_or(now_ms)));
        }
        Ok(())
    }

    pub(crate) fn get(&self, id: &str) -> Option<&ToolCall> {
        self.item.get(id)
    }

    pub fn execution(&self, id: &str) -> Option<&ToolExecution> {
        self.execution.get(id)
    }
}

fn tool_kind(kind: crate::backend::ToolActivityKind) -> String {
    match kind {
        crate::backend::ToolActivityKind::Command => "command",
        crate::backend::ToolActivityKind::FileChange => "file_change",
        crate::backend::ToolActivityKind::ToolCall => "tool_call",
    }
    .into()
}

fn tool_failed(status: &str) -> bool {
    matches!(status.to_ascii_lowercase().as_str(), "failed" | "error")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tool_identity_input_and_denial_survive_reopening_without_restarting_execution()
    -> Result<(), crate::backend::BackendError> {
        let address = ToolAddress {
            turn: crate::backend::TurnAddress {
                scope: crate::backend::InvocationScope::new(
                    crate::backend::ExecutionOwner::root("session"),
                    "exchange",
                ),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            tool_id: "call".into(),
        };
        let mut store = ToolStore::default();
        store.merge(
            &address,
            &ToolUpdate::Requested {
                invocation: ToolInvocation {
                    kind: crate::backend::ToolActivityKind::Command,
                    name: "inspect".into(),
                    title: "Inspect files".into(),
                    arguments: serde_json::json!({"path":"source.rs"}),
                },
            },
            10,
        )?;
        assert_eq!(store.get("call").unwrap().started_at_ms, None);
        store.merge(
            &address,
            &ToolUpdate::Input {
                update: TextUpdate::Append("partial".into()),
            },
            11,
        )?;
        store.merge(&address, &ToolUpdate::Started, 12)?;
        store.merge(
            &address,
            &ToolUpdate::Finished {
                outcome: ToolOutcome::Denied {
                    reason: "workspace policy".into(),
                },
            },
            13,
        )?;
        store.merge(
            &address,
            &ToolUpdate::Output {
                update: TextUpdate::Replace("denied".into()),
            },
            14,
        )?;
        store.merge(
            &address,
            &ToolUpdate::Output {
                update: TextUpdate::Replace(String::new()),
            },
            15,
        )?;
        store.merge(
            &address,
            &ToolUpdate::Input {
                update: TextUpdate::Replace("complete".into()),
            },
            16,
        )?;
        store.merge(&address, &ToolUpdate::Started, 20)?;
        assert!(
            store
                .merge(
                    &address,
                    &ToolUpdate::Finished {
                        outcome: ToolOutcome::Succeeded,
                    },
                    21,
                )
                .is_err()
        );
        let restored: ToolStore =
            serde_json::from_value(serde_json::to_value(store).unwrap()).unwrap();
        let call = restored.get("call").unwrap();
        assert_eq!(call.state(), ToolState::Denied);
        assert_eq!(call.started_at_ms, Some(12));
        assert_eq!(call.completed_at_ms, Some(13));
        assert!(!call.failed);
        assert!(call.output.is_empty());
        let execution = restored.execution("call").unwrap();
        assert_eq!(execution.address, address);
        assert_eq!(execution.input, "complete");
        assert_eq!(execution.invocation.arguments["path"], "source.rs");
        assert!(matches!(&execution.outcome,
            Some(ToolOutcome::Denied { reason }) if reason == "workspace policy"));
        Ok(())
    }

    #[test]
    fn only_structured_execution_failures_count_as_failed_tools()
    -> Result<(), crate::backend::BackendError> {
        for status in [
            "denied",
            "declined",
            "rejected",
            "cancelled",
            "canceled",
            "interrupted",
            "completed",
        ] {
            assert!(!tool_failed(status), "{status}");
        }
        assert!(tool_failed("error"));
        assert!(tool_failed("failed"));
        let mut store = ToolStore::default();
        let address = ToolAddress {
            turn: crate::backend::TurnAddress {
                scope: crate::backend::InvocationScope::new(
                    crate::backend::ExecutionOwner::root("session"),
                    "exchange",
                ),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            tool_id: "successful".into(),
        };
        store.merge(
            &address,
            &ToolUpdate::Requested {
                invocation: ToolInvocation {
                    kind: crate::backend::ToolActivityKind::Command,
                    name: "inspect".into(),
                    title: "Inspect files".into(),
                    arguments: serde_json::json!({}),
                },
            },
            0,
        )?;
        store.merge(
            &address,
            &ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
            1,
        )?;
        store.merge(
            &address,
            &ToolUpdate::Output {
                update: TextUpdate::Replace("0 rejected: valid output".into()),
            },
            2,
        )?;
        assert!(!store.get("successful").unwrap().failed);
        assert_eq!(
            store.get("successful").unwrap().state(),
            ToolState::Completed
        );
        Ok(())
    }
}
