use anyhow::{Result, ensure};
use serde::Serialize;
use serde_json::Value;
use std::collections::{HashMap, BTreeSet};

use super::TimelineEntry;

/// Represents one atomic change to a session's projected timeline.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineOperation {
    Insert { index: usize, entry: TimelineEntry },
    Replace { index: usize, entry: TimelineEntry },
    Remove { index: usize, id: String },
    ToolOutput { index: usize, entry_id: String, exchange_id: String, call_id: String, delta: String },
    Message { index: usize, entry_id: String, exchange_id: String,
        turn_id: String, block_id: String, message: crate::turn::Message },
}

/// Represents one causally ordered timeline revision for a single session.
#[derive(Clone, Debug, Serialize)]
pub struct TimelinePatch {
    pub session_id: String,
    pub base_revision: u64,
    pub revision: u64,
    pub operation: Vec<TimelineOperation>,
}

impl TimelinePatch {
    /// Describes changed presentation state without copying exchange contents.
    pub fn notification(&self) -> Value {
        let mut notification = serde_json::json!({"session_id":self.session_id,"revision":self.revision});
        for operation in &self.operation {
            match operation {
                TimelineOperation::Insert { entry: TimelineEntry::Status { status, .. }, .. }
                | TimelineOperation::Replace { entry: TimelineEntry::Status { status, .. }, .. } => {
                    notification["status"] = serde_json::to_value(status).expect("serializable status");
                }
                TimelineOperation::Remove { id, .. } if id == &format!("{}:status", self.session_id) => {
                    notification["status"] = serde_json::json!({"kind":"idle"});
                }
                _ => {}
            }
        }
        notification
    }

    /// Report whether this revision carries any visible timeline mutation.
    pub fn is_empty(&self) -> bool {
        self.operation.is_empty()
    }
}

/// Owns the last projected timeline and its monotonically increasing revision.
pub struct TimelineStream {
    session_id: String,
    revision: u64,
    entry_list: Vec<TimelineEntry>,
    value_list: Vec<Value>,
    entry_index: HashMap<String, usize>,
    exchange_owner: HashMap<String, usize>,
    ticking: BTreeSet<usize>,
}

impl TimelineStream {
    pub(crate) fn update_message(&mut self, exchange: &crate::exchange::Exchange,
        event: &crate::backend::BackendEvent) -> Result<Option<TimelinePatch>> {
        let Some(index) = self.exchange_owner.get(&exchange.id).copied() else { return Ok(None); };
        let Some(address) = event.address.as_ref() else { return Ok(None); };
        let Some(source) = exchange.turn.iter().find(|turn| turn.provider() == address) else { return Ok(None); };
        let kind = match event.kind.as_str() {
            "assistant_message" => crate::turn::MessageKind::Assistant,
            "reasoning" => crate::turn::MessageKind::Reasoning,
            "reasoning_summary" => crate::turn::MessageKind::ReasoningSummary,
            _ => return Ok(None),
        };
        let Some(message) = source.messages().iter().rev().find(|message|
            message.kind() == kind && event.message_id().is_none_or(|id|message.provider_id() == Some(id)))
            else { return Ok(None); };
        let entry_id = self.entry_list[index].id();
        let main_exchange = matches!(&self.entry_list[index],TimelineEntry::Exchange { exchange:owner,.. } if owner.id == exchange.id);
        let target = self.entry_list[index].exchange_mut(&exchange.id).expect("indexed exchange");
        if !target.turn.iter().any(|turn| turn.provider() == address
            && turn.messages().iter().any(|existing| existing.id() == message.id()
                && existing.delivery() == message.delivery())) { return Ok(None); }
        ensure!(self.revision < forge_buffer::MAX_COUNTER, "timeline revision exhausted");
        target.observe_turn(event,exchange.created_at_ms)?;
        target.metrics = exchange.metrics.clone();
        let base_revision = self.revision;
        self.revision += 1;
        self.value_list[index] = Value::Null;
        let mut operation = vec![TimelineOperation::Message { index,entry_id,exchange_id:exchange.id.clone(),
                turn_id:source.id().to_owned(), block_id:format!("{}:content:{}",source.id(),message.id()),
                message:message.clone() }];
        if main_exchange && let Some(status_index) = self.entry_index.get(&format!("{}:status",self.session_id)).copied()
            && let TimelineEntry::Status { status:crate::session::state_machine::SessionPhase::Working { reasoning_summary,.. },.. }
                = &mut self.entry_list[status_index] {
            let summary = exchange.latest_reasoning_summary().map(str::to_owned);
            if *reasoning_summary != summary {
                *reasoning_summary = summary;
                self.value_list[status_index] = serde_json::to_value(&self.entry_list[status_index])?;
                operation.push(TimelineOperation::Replace { index:status_index,entry:self.entry_list[status_index].clone() });
            }
        }
        Ok(Some(TimelinePatch { session_id:self.session_id.clone(),base_revision,revision:self.revision,operation }))
    }

    pub(crate) fn append_tool_output(&mut self, exchange: &crate::exchange::Exchange,
        event: &crate::backend::BackendEvent) -> Result<Option<TimelinePatch>> {
        let Some(index) = self.exchange_owner.get(&exchange.id).copied() else { return Ok(None); };
        let Some(address) = event.address.as_ref() else { return Ok(None); };
        let Some(activity) = event.activity.as_ref() else { return Ok(None); };
        let entry_id = self.entry_list[index].id();
        let target = self.entry_list[index].exchange_mut(&exchange.id).expect("indexed exchange");
        let Some(turn) = target.turn.iter_mut().find(|turn| turn.provider() == address) else { return Ok(None); };
        if !turn.tools().any(|tool| tool.id == activity.id) { return Ok(None); }
        let call_id = format!("{}:{}",turn.id(),activity.id);
        turn.record_tool_event(event, exchange.created_at_ms)?;
        target.metrics = exchange.metrics.clone();
        let base_revision = self.revision;
        ensure!(self.revision < forge_buffer::MAX_COUNTER, "timeline revision exhausted");
        self.revision += 1;
        self.value_list[index] = Value::Null;
        Ok(Some(TimelinePatch { session_id: self.session_id.clone(), base_revision,
            revision: self.revision, operation: vec![TimelineOperation::ToolOutput {
                index, entry_id, exchange_id:exchange.id.clone(), call_id, delta: activity.output.clone().unwrap_or_default(),
            }] }))
    }

    /// Create an empty revision stream for one durable Harness session.
    pub fn new(session_id: String) -> Self {
        Self {
            session_id,
            revision: 0,
            entry_list: Vec::new(),
            value_list: Vec::new(),
            entry_index: HashMap::new(),
            exchange_owner: HashMap::new(),
            ticking: BTreeSet::new(),
        }
    }

    /// Initialize the stream from the full snapshot returned to a new client.
    pub fn initialize(&mut self, entry_list: Vec<TimelineEntry>) -> Result<()> {
        ensure!(self.revision == 0, "timeline stream is already initialized");
        self.value_list = serialize_entry_list(&entry_list)?;
        self.entry_list = entry_list;
        self.reindex();
        self.revision = 1;
        Ok(())
    }

    /// Return the current session-local timeline revision.
    pub fn revision(&self) -> u64 {
        self.revision
    }

    /// Resolve the current canonical entry list for targeted Rust projections.
    pub fn entry_list(&self) -> &[TimelineEntry] {
        &self.entry_list
    }

    pub(crate) fn ticking_entries(&self) -> impl Iterator<Item=&TimelineEntry> {
        self.ticking.iter().map(|index|&self.entry_list[*index])
    }

    /// Updates only the active interaction and transient status without copying settled history.
    pub fn update_live(
        &mut self,
        interaction: Option<&crate::exchange::Exchange>,
        removed: Option<&str>,
        status: crate::session::state_machine::SessionPhase,
    ) -> Result<TimelinePatch> {
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        let next_interaction = interaction.map(|interaction| {
            if let Some(index) = self.exchange_owner.get(&interaction.id) {
                let mut entry = self.entry_list[*index].clone();
                let replaced = entry.replace_exchange(interaction);
                debug_assert!(replaced, "exchange owner index must resolve its exchange");
                return entry;
            }
            TimelineEntry::Exchange {
                id: interaction.id.clone(),
                created_at_ms: interaction.created_at_ms,
                exchange: interaction.clone(),
                agent_by_id: HashMap::new(),
            }
        });
        let status_id = format!("{}:status", self.session_id);
        let next_status = status.visible().then(|| TimelineEntry::Status {
            id: status_id.clone(),
            created_at_ms: 0,
            status,
        });
        let next_interaction = next_interaction
            .map(|entry| serde_json::to_value(&entry).map(|value| (entry, value)))
            .transpose()?;
        let next_status = next_status
            .map(|entry| serde_json::to_value(&entry).map(|value| (entry, value)))
            .transpose()?;
        let base_revision = self.revision;
        let mut operation = Vec::new();
        if let Some(id) = removed {
            if let Some(index) = self.entry_index.get(id).copied() {
                let id = self.entry_list.remove(index).id();
                self.value_list.remove(index);
                operation.push(TimelineOperation::Remove { index, id });
                self.reindex();
            } else if let Some(index) = self.exchange_owner.get(id).copied() {
                let mut entry = self.entry_list[index].clone();
                entry.remove_exchange(id);
                let value = serde_json::to_value(&entry)?;
                self.upsert(entry, value, index, &mut operation);
            }
        }
        if let Some((entry, value)) = next_interaction {
            let insertion = self
                .entry_index
                .get(&status_id)
                .copied()
                .unwrap_or(self.entry_list.len());
            self.upsert(entry, value, insertion, &mut operation);
        }
        if let Some((entry, value)) = next_status {
            self.upsert(entry, value, self.entry_list.len(), &mut operation);
        } else if let Some(index) = self.entry_index.get(&status_id).copied() {
            self.entry_list.remove(index);
            self.value_list.remove(index);
            operation.push(TimelineOperation::Remove {
                index,
                id: status_id,
            });
            self.reindex();
        }
        if !operation.is_empty() {
            self.revision += 1;
        }
        Ok(TimelinePatch {
            session_id: self.session_id.clone(),
            base_revision,
            revision: self.revision,
            operation,
        })
    }

    fn upsert(
        &mut self,
        entry: TimelineEntry,
        value: Value,
        insertion: usize,
        operation: &mut Vec<TimelineOperation>,
    ) {
        if let Some(index) = self.entry_index.get(&entry.id()).copied() {
            if self.value_list[index] != value {
                self.entry_list[index] = entry.clone();
                self.value_list[index] = value;
                operation.push(TimelineOperation::Replace { index, entry });
                self.exchange_owner.retain(|_, owner| *owner != index);
                self.entry_list[index].index_exchanges(index, &mut self.exchange_owner);
                if ticking(&self.entry_list[index]) { self.ticking.insert(index); } else { self.ticking.remove(&index); }
            }
        } else {
            self.entry_list.insert(insertion, entry.clone());
            self.value_list.insert(insertion, value);
            operation.push(TimelineOperation::Insert {
                index: insertion,
                entry,
            });
            self.reindex();
        }
    }

    fn reindex(&mut self) {
        self.entry_index = self
            .entry_list
            .iter()
            .enumerate()
            .map(|(index, entry)| (entry.id(), index))
            .collect();
        self.exchange_owner.clear();
        self.ticking.clear();
        for (index, entry) in self.entry_list.iter().enumerate() {
            entry.index_exchanges(index, &mut self.exchange_owner);
            if ticking(entry) { self.ticking.insert(index); }
        }
    }

    /// Reconcile one canonical projection into ordered top-level operations.
    pub fn reconcile(&mut self, next_entry_list: Vec<TimelineEntry>) -> Result<TimelinePatch> {
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        let base_revision = self.revision;
        let next_value_list = serialize_entry_list(&next_entry_list)?;
        let mut working_entry_list = self.entry_list.clone();
        let mut working_value_list = self.value_list.clone();
        let mut operation = Vec::new();

        for index in (0..working_entry_list.len()).rev() {
            let id = working_entry_list[index].id();
            if !next_entry_list.iter().any(|entry| entry.id() == id) {
                working_entry_list.remove(index);
                working_value_list.remove(index);
                operation.push(TimelineOperation::Remove { index, id });
            }
        }

        for (index, next_entry) in next_entry_list.iter().enumerate() {
            let next_id = next_entry.id();
            if working_entry_list
                .get(index)
                .is_some_and(|entry| entry.id() == next_id)
            {
                if working_value_list[index] != next_value_list[index] {
                    working_entry_list[index] = next_entry.clone();
                    working_value_list[index] = next_value_list[index].clone();
                    operation.push(TimelineOperation::Replace {
                        index,
                        entry: next_entry.clone(),
                    });
                }
                continue;
            }

            if let Some(previous_index) = working_entry_list
                .iter()
                .position(|entry| entry.id() == next_id)
            {
                let removed_id = working_entry_list[previous_index].id();
                working_entry_list.remove(previous_index);
                working_value_list.remove(previous_index);
                operation.push(TimelineOperation::Remove {
                    index: previous_index,
                    id: removed_id,
                });
            }
            working_entry_list.insert(index, next_entry.clone());
            working_value_list.insert(index, next_value_list[index].clone());
            operation.push(TimelineOperation::Insert {
                index,
                entry: next_entry.clone(),
            });
        }

        ensure!(
            working_value_list == next_value_list,
            "timeline reconciliation did not converge"
        );
        if !operation.is_empty() {
            self.revision += 1;
            self.entry_list = next_entry_list;
            self.value_list = next_value_list;
            self.reindex();
        }
        Ok(TimelinePatch {
            session_id: self.session_id.clone(),
            base_revision,
            revision: self.revision,
            operation,
        })
    }
}

fn ticking(entry: &TimelineEntry) -> bool {
    match entry {
        TimelineEntry::Exchange { exchange,agent_by_id,.. } =>
            exchange.completed_at_ms.is_none() && exchange.execution_started_at_ms.is_some()
                || agent_by_id.values().any(ticking),
        TimelineEntry::AgentLifecycle { run,.. } => run.state.is_open(),
        TimelineEntry::Status { status,.. } => matches!(status,
            crate::session::state_machine::SessionPhase::Working { .. }
            | crate::session::state_machine::SessionPhase::WaitingForAgent { .. }),
        _ => false,
    }
}

fn serialize_entry_list(entry_list: &[TimelineEntry]) -> Result<Vec<Value>> {
    entry_list
        .iter()
        .map(serde_json::to_value)
        .collect::<Result<Vec<_>, _>>()
        .map_err(Into::into)
}

#[cfg(test)]
mod test {
    use super::{TimelineOperation, TimelineStream};
    use crate::{
        session::state_machine::{SessionPhase, WorkflowActivity},
        timeline::TimelineEntry,
    };

    fn status(kind: SessionPhase) -> TimelineEntry {
        TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: kind,
        }
    }

    #[test]
    fn replaces_one_stable_status_entry_without_resending_the_frame() {
        let mut stream = TimelineStream::new("session".into());
        stream
            .initialize(vec![status(SessionPhase::Working {
                started_at_ms: 10,
                activity: WorkflowActivity::Working,
                execution: None,
                reasoning_summary: None,
            })])
            .unwrap();

        let patch = stream
            .reconcile(vec![status(SessionPhase::AwaitingPlanReview {
                plan_id: "plan".into(),
                revision: 2,
            })])
            .unwrap();

        assert_eq!(patch.base_revision, 1);
        assert_eq!(patch.revision, 2);
        assert!(matches!(
            patch.operation.as_slice(),
            [TimelineOperation::Replace { index: 0, .. }]
        ));
    }

    #[test]
    fn unchanged_projection_does_not_advance_the_revision() {
        let mut stream = TimelineStream::new("session".into());
        stream.initialize(vec![status(SessionPhase::Idle)]).unwrap();
        let patch = stream.reconcile(vec![status(SessionPhase::Idle)]).unwrap();
        assert!(patch.is_empty());
        assert_eq!(patch.base_revision, patch.revision);
    }

    #[test]
    fn sessions_advance_independent_revision_sequences() {
        let mut first = TimelineStream::new("first".into());
        let mut second = TimelineStream::new("second".into());
        first.initialize(vec![status(SessionPhase::Idle)]).unwrap();
        second.initialize(vec![status(SessionPhase::Idle)]).unwrap();

        let first_patch = first
            .reconcile(vec![status(SessionPhase::Working {
                started_at_ms: 5,
                activity: WorkflowActivity::Working,
                execution: None,
                reasoning_summary: None,
            })])
            .unwrap();

        assert_eq!(first_patch.session_id, "first");
        assert_eq!(first.revision(), 2);
        assert_eq!(second.revision(), 1);
    }

    #[test]
    fn live_status_update_retains_settled_history_allocations() {
        let mut stream = TimelineStream::new("session".into());
        let mut history = (0..1000)
            .map(|ordinal| TimelineEntry::SessionEvent {
                id: format!("event-{ordinal}"),
                created_at_ms: ordinal,
                event: crate::timeline::SessionEventRecord {
                    id: format!("event-{ordinal}"),
                    session_id: "session".into(),
                    created_at_ms: ordinal,
                    detail: crate::timeline::SessionEventKind::Renamed {
                        name: "settled".repeat(100),
                    },
                },
            })
            .collect::<Vec<_>>();
        history.push(status(SessionPhase::Working {
            started_at_ms: 1,
            activity: WorkflowActivity::Working,
            execution: None,
            reasoning_summary: None,
        }));
        stream.initialize(history).unwrap();
        let allocation = stream.value_list[500]["event"]["name"]
            .as_str()
            .unwrap()
            .as_ptr();
        let patch = stream
            .update_live(
                None,
                None,
                SessionPhase::Working {
                    started_at_ms: 2,
                    activity: WorkflowActivity::Working,
                    execution: None,
                    reasoning_summary: None,
                },
            )
            .unwrap();
        assert!(matches!(
            patch.operation.as_slice(),
            [TimelineOperation::Replace { index: 1000, .. }]
        ));
        assert_eq!(
            stream.value_list[500]["event"]["name"]
                .as_str()
                .unwrap()
                .as_ptr(),
            allocation
        );
        assert!(
            stream
                .update_live(
                    None,
                    None,
                    SessionPhase::Working {
                        started_at_ms: 2,
                        activity: WorkflowActivity::Working,
                        execution: None,
                        reasoning_summary: None,
                    }
                )
                .unwrap()
                .is_empty()
        );
    }
}
