use anyhow::{Result, ensure};
use serde::Serialize;
use serde_json::Value;
use std::collections::{BTreeSet, HashMap};

use super::TimelineEntry;

/// Represents one atomic change to a session's projected timeline.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineOperation {
    Insert {
        index: usize,
        entry: TimelineEntry,
    },
    Replace {
        index: usize,
        entry: TimelineEntry,
    },
    Remove {
        index: usize,
        id: String,
    },
    ToolOutput {
        index: usize,
        entry_id: String,
        exchange_id: String,
        call: crate::turn::ToolAddress,
        delta: String,
    },
    Message {
        index: usize,
        entry_id: String,
        exchange_id: String,
        turn_id: String,
        block_id: String,
        message: crate::turn::Message,
    },
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
        let mut notification =
            serde_json::json!({"session_id":self.session_id,"revision":self.revision});
        for operation in &self.operation {
            match operation {
                TimelineOperation::Insert {
                    entry: TimelineEntry::Status { status, checks, .. },
                    ..
                }
                | TimelineOperation::Replace {
                    entry: TimelineEntry::Status { status, checks, .. },
                    ..
                } => {
                    notification["status"] =
                        crate::codec::to_value(status).expect("serializable status");
                    notification["checks_running"] = Value::Bool(checks.is_some());
                    notification["active_check_run"] =
                        checks.as_ref().map_or(Value::Null, |activity| {
                            Value::String(activity.run_id.clone())
                        });
                }
                TimelineOperation::Remove { id, .. }
                    if id == &format!("{}:status", self.session_id) =>
                {
                    notification["status"] = serde_json::json!({"kind":"idle"});
                    notification["checks_running"] = Value::Bool(false);
                    notification["active_check_run"] = Value::Null;
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
    removed_exchange: std::collections::HashSet<String>,
    ticking: BTreeSet<usize>,
}

impl TimelineStream {
    pub(crate) fn update_message(
        &mut self,
        exchange: &crate::exchange::Exchange,
        event: &crate::provider::ProviderEvent,
    ) -> Result<Option<TimelinePatch>> {
        let Some(index) = self.exchange_owner.get(&exchange.id).copied() else {
            return Ok(None);
        };
        let crate::provider::ProviderEvent::Item {
            thread_id,
            turn_id,
            item_id,
            ..
        } = event
        else {
            return Ok(None);
        };
        let Some(source) = exchange.turn.iter().find(|turn| {
            &turn.provider().thread_id == thread_id && &turn.provider().turn_id == turn_id
        }) else {
            return Ok(None);
        };
        let Some(message) = source
            .messages()
            .iter()
            .rev()
            .find(|message| message.provider_id() == item_id)
        else {
            return Ok(None);
        };
        let address = source.provider();
        let entry_id = self.entry_list[index].id();
        let main_exchange = matches!(&self.entry_list[index],TimelineEntry::Exchange { exchange:owner,.. } if owner.id == exchange.id);
        let target = self.entry_list[index]
            .exchange_mut(&exchange.id)
            .expect("indexed exchange");
        if !target.turn.iter().any(|turn| {
            turn.provider() == address
                && turn.messages().iter().any(|existing| {
                    existing.id() == message.id() && existing.role() == message.role()
                })
        }) {
            return Ok(None);
        }
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        target.observe_provider_event(
            &crate::provider::EventScope::Invocation {
                invocation: address.scope.clone(),
            },
            event,
            exchange.created_at_ms,
        )?;
        target.metrics = exchange.metrics.clone();
        let base_revision = self.revision;
        self.revision += 1;
        self.value_list[index] = Value::Null;
        let mut operation = vec![TimelineOperation::Message {
            index,
            entry_id,
            exchange_id: exchange.id.clone(),
            turn_id: source.id().to_owned(),
            block_id: format!("{}:content:{}", source.id(), message.id()),
            message: message.clone(),
        }];
        if main_exchange
            && let Some(status_index) = self
                .entry_index
                .get(&format!("{}:status", self.session_id))
                .copied()
            && let TimelineEntry::Status {
                status_thoughts,
                status:
                    crate::session::state_machine::SessionPhase::Working {
                        reasoning_summary, ..
                    },
                ..
            } = &mut self.entry_list[status_index]
        {
            let summary = exchange.latest_reasoning_summary().map(str::to_owned);
            if *reasoning_summary != summary {
                *reasoning_summary = summary;
                let visible = *status_thoughts;
                self.value_list[status_index] =
                    crate::codec::to_value(&self.entry_list[status_index])?;
                if visible {
                    operation.push(TimelineOperation::Replace {
                        index: status_index,
                        entry: self.entry_list[status_index].clone(),
                    });
                }
            }
        }
        Ok(Some(TimelinePatch {
            session_id: self.session_id.clone(),
            base_revision,
            revision: self.revision,
            operation,
        }))
    }

    pub(crate) fn append_tool_output(
        &mut self,
        exchange: &crate::exchange::Exchange,
        event: &crate::provider::ProviderEvent,
    ) -> Result<Option<TimelinePatch>> {
        let Some(index) = self.exchange_owner.get(&exchange.id).copied() else {
            return Ok(None);
        };
        let crate::provider::ProviderEvent::Item {
            thread_id,
            turn_id,
            item_id,
            event:
                item_event @ forge_provider_api::ItemEvent::Tool {
                    event:
                        forge_provider_api::ToolItemEvent::Output(
                            forge_provider_api::TextUpdate::Append(output),
                        ),
                },
        } = event
        else {
            return Ok(None);
        };
        let Some(source) = exchange.turn.iter().find(|turn| {
            &turn.provider().thread_id == thread_id && &turn.provider().turn_id == turn_id
        }) else {
            return Ok(None);
        };
        let address = source.provider();
        if exchange
            .turn
            .iter()
            .find(|turn| turn.provider() == address)
            .and_then(|turn| {
                exchange
                    .tools_for_turn(turn.id())
                    .find(|tool| tool.id == item_id.as_str())
            })
            .is_some_and(|tool| tool.kind == "file_change")
        {
            return Ok(None);
        }
        let address = source.provider();
        let entry_id = self.entry_list[index].id();
        let target = self.entry_list[index]
            .exchange_mut(&exchange.id)
            .expect("indexed exchange");
        let Some(turn) = target
            .turn
            .iter_mut()
            .find(|turn| turn.provider() == address)
        else {
            return Ok(None);
        };
        let turn_id = turn.id().to_owned();
        if !target
            .tools_for_turn(&turn_id)
            .any(|tool| tool.id == item_id.as_str())
        {
            return Ok(None);
        }
        let call = crate::turn::ToolAddress {
            turn: address.clone(),
            item_id: item_id.clone(),
        };
        target.record_tool_event(
            &turn_id,
            address,
            item_id,
            item_event,
            exchange.created_at_ms,
        )?;
        target.metrics = exchange.metrics.clone();
        let base_revision = self.revision;
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        self.revision += 1;
        self.value_list[index] = Value::Null;
        Ok(Some(TimelinePatch {
            session_id: self.session_id.clone(),
            base_revision,
            revision: self.revision,
            operation: vec![TimelineOperation::ToolOutput {
                index,
                entry_id,
                exchange_id: exchange.id.clone(),
                call,
                delta: output.to_owned(),
            }],
        }))
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
            removed_exchange: Default::default(),
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

    pub(crate) fn ticking_entries(&self) -> impl Iterator<Item = &TimelineEntry> {
        self.ticking.iter().map(|index| &self.entry_list[*index])
    }

    /// Toggles the current status preview while retaining streamed messages and phase state.
    pub(crate) fn set_status_thoughts(&mut self, enabled: bool) -> Result<TimelinePatch> {
        let base_revision = self.revision;
        let mut operation = Vec::new();
        if let Some(index) = self
            .entry_index
            .get(&format!("{}:status", self.session_id))
            .copied()
            && let TimelineEntry::Status {
                status_thoughts, ..
            } = &mut self.entry_list[index]
            && *status_thoughts != enabled
        {
            ensure!(
                self.revision < forge_buffer::MAX_COUNTER,
                "timeline revision exhausted"
            );
            *status_thoughts = enabled;
            self.value_list[index] = crate::codec::to_value(&self.entry_list[index])?;
            operation.push(TimelineOperation::Replace {
                index,
                entry: self.entry_list[index].clone(),
            });
            self.revision += 1;
        }
        Ok(TimelinePatch {
            session_id: self.session_id.clone(),
            base_revision,
            revision: self.revision,
            operation,
        })
    }

    /// Updates only the active interaction and transient status without copying settled history.
    pub fn update_live(
        &mut self,
        interaction: Option<&crate::exchange::Exchange>,
        removed: Option<&str>,
        status: crate::session::state_machine::SessionPhase,
        status_thoughts: bool,
    ) -> Result<TimelinePatch> {
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        let previous_owners = if removed.is_some() {
            self.exchange_owner.keys().cloned().collect::<Vec<_>>()
        } else {
            Vec::new()
        };
        let next_interaction = interaction
            .map(|interaction| -> Result<TimelineEntry> {
                if let Some(index) = self.exchange_owner.get(&interaction.id) {
                    if let TimelineEntry::Exchange {
                        id,
                        created_at_ms,
                        exchange,
                        agent_by_id,
                    } = &self.entry_list[*index]
                        && exchange.id == interaction.id
                    {
                        return Ok(TimelineEntry::Exchange {
                            id: id.clone(),

                            created_at_ms: *created_at_ms,
                            exchange: (crate::plan::event::replacement_exchange(
                                exchange,
                                interaction,
                            )?)
                            .into(),

                            agent_by_id: agent_by_id.clone(),
                        });
                    }
                    let mut entry = self.entry_list[*index].clone();
                    let replaced = entry.replace_exchange(interaction)?;
                    debug_assert!(replaced, "exchange owner index must resolve its exchange");
                    return Ok(entry);
                }
                Ok(TimelineEntry::Exchange {
                    id: interaction.id.clone(),

                    created_at_ms: interaction.created_at_ms,
                    exchange: (interaction.clone()).into(),

                    agent_by_id: HashMap::new(),
                })
            })
            .transpose()?;
        let status_id = format!("{}:status", self.session_id);
        let agent_count = self
            .entry_list
            .iter()
            .find_map(|entry| match entry {
                TimelineEntry::Status { agent_count, .. } => Some(*agent_count),
                _ => None,
            })
            .unwrap_or(0);
        let checks =
            self.entry_index
                .get(&status_id)
                .and_then(|index| match &self.entry_list[*index] {
                    TimelineEntry::Status { checks, .. } => checks.clone(),
                    _ => None,
                });
        let next_status = TimelineEntry::Status {
            checks,
            agent_count,
            status_thoughts,
            id: status_id.clone(),
            created_at_ms: 0,
            status,
        };
        let next_interaction = next_interaction
            .map(|entry| crate::codec::to_value(&entry).map(|value| (entry, value)))
            .transpose()?;
        let status_value = crate::codec::to_value(&next_status)?;
        let base_revision = self.revision;
        let mut operation = Vec::new();
        if let Some(id) = removed {
            self.removed_exchange.insert(id.to_owned());
            if let Some(index) = self.entry_index.get(id).copied() {
                let id = self.entry_list.remove(index).id();
                self.value_list.remove(index);
                operation.push(TimelineOperation::Remove { index, id });
                self.reindex();
            } else if let Some(index) = self.exchange_owner.get(id).copied() {
                let mut entry = self.entry_list[index].clone();
                entry.remove_exchange(id);
                let value = crate::codec::to_value(&entry)?;
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
        self.upsert(
            next_status,
            status_value,
            self.entry_list.len(),
            &mut operation,
        );
        self.removed_exchange.extend(
            previous_owners
                .into_iter()
                .filter(|owner| !self.exchange_owner.contains_key(owner)),
        );
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
                if ticking(&self.entry_list[index]) {
                    self.ticking.insert(index);
                } else {
                    self.ticking.remove(&index);
                }
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
            if ticking(entry) {
                self.ticking.insert(index);
            }
        }
    }

    /// Publish live checks under status and insert one history result after completion.
    pub(crate) fn update_checks(&mut self, run: &crate::checks::CheckRun) -> Result<TimelinePatch> {
        ensure!(
            run.owner.session_id() == self.session_id,
            "check event crosses timeline sessions"
        );
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        let base_revision = self.revision;
        let mut operation = Vec::new();
        let activity = crate::checks::CheckActivity::from_run(run);
        let status_id = format!("{}:status", self.session_id);
        if activity.is_none() {
            if let Some(owner) = run.owner.exchange_id() {
                if let Some(index) = self.exchange_owner.get(owner).copied() {
                    let mut entry = self.entry_list[index].clone();
                    if entry.attach_event(&super::TimelineEvent::Check {
                        run: Box::new(run.clone()),
                    })? {
                        let value = crate::codec::to_value(&entry)?;
                        self.upsert(entry, value, index, &mut operation);
                    }
                } else if !self.removed_exchange.contains(owner) {
                    anyhow::bail!("check event owner is absent from the timeline: {}", run.id);
                }
            } else {
                let entry = TimelineEntry::Checks {
                    id: run.id.clone(),

                    created_at_ms: run.started_at_ms,
                    run: (run.clone()).into(),
                };
                let insertion = self
                    .entry_list
                    .iter()
                    .position(|current| current.order_key() > entry.order_key())
                    .unwrap_or(self.entry_list.len());
                let value = crate::codec::to_value(&entry)?;
                self.upsert(entry, value, insertion, &mut operation);
            }
        }
        let previous = self
            .entry_index
            .get(&status_id)
            .map(|index| self.entry_list[*index].clone());
        if previous.is_some() || activity.is_some() {
            let mut entry = previous.unwrap_or_else(|| TimelineEntry::Status {
                id: status_id,
                created_at_ms: 0,
                status: crate::session::state_machine::SessionPhase::Idle,
                status_thoughts: true,
                agent_count: 0,
                checks: None,
            });
            if let TimelineEntry::Status { checks, .. } = &mut entry {
                if let Some(activity) = activity {
                    *checks = Some(activity);
                } else if checks
                    .as_ref()
                    .is_some_and(|checks| checks.run_id == run.id)
                {
                    *checks = None;
                }
            }
            self.retain_check_progress(&mut entry);
            let value = crate::codec::to_value(&entry)?;
            self.upsert(entry, value, self.entry_list.len(), &mut operation);
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

    /// Keep the last visible command through preparation and handoff snapshots.
    fn retain_check_progress(&self, entry: &mut TimelineEntry) {
        let TimelineEntry::Status { id, checks, .. } = entry else {
            return;
        };
        if !checks
            .as_ref()
            .is_some_and(|activity| activity.command_list.is_empty())
        {
            return;
        }
        *checks = self
            .entry_index
            .get(id)
            .and_then(|index| match &self.entry_list[*index] {
                TimelineEntry::Status { checks, .. } => checks.clone(),
                _ => None,
            });
    }

    pub fn reconcile(&mut self, mut next_entry_list: Vec<TimelineEntry>) -> Result<TimelinePatch> {
        ensure!(
            self.revision < forge_buffer::MAX_COUNTER,
            "timeline revision exhausted"
        );
        let base_revision = self.revision;
        for entry in &mut next_entry_list {
            self.retain_check_progress(entry);
        }
        let next_value_list = serialize_entry_list(&next_entry_list)?;
        for (index, value) in self.value_list.iter_mut().enumerate() {
            if value.is_null() {
                *value = crate::codec::to_value(&self.entry_list[index])?;
            }
        }
        let mut working_entry_list: Vec<_> = self.entry_list.iter().collect();
        let mut working_value_list: Vec<_> = self.value_list.iter().collect();
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
                if working_value_list[index] != &next_value_list[index] {
                    working_entry_list[index] = next_entry;
                    working_value_list[index] = &next_value_list[index];
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
            working_entry_list.insert(index, next_entry);
            working_value_list.insert(index, &next_value_list[index]);
            operation.push(TimelineOperation::Insert {
                index,
                entry: next_entry.clone(),
            });
        }

        ensure!(
            working_value_list.into_iter().eq(next_value_list.iter()),
            "timeline reconciliation did not converge"
        );
        if !operation.is_empty() {
            self.revision += 1;
            let mut next_owner = HashMap::new();
            for (index, entry) in next_entry_list.iter().enumerate() {
                entry.index_exchanges(index, &mut next_owner);
            }
            self.removed_exchange.extend(
                self.exchange_owner
                    .keys()
                    .filter(|owner| !next_owner.contains_key(*owner))
                    .cloned(),
            );
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
        TimelineEntry::Exchange {
            exchange,
            agent_by_id,
            ..
        } => {
            !exchange.is_finished()
                && (exchange.execution_started_at_ms().is_some()
                    || exchange.turn.iter().any(|turn| {
                        exchange
                            .tools_for_turn(turn.id())
                            .any(|tool| tool.state() == crate::turn::ToolState::Running)
                    }))
                || agent_by_id.values().any(ticking)
        }
        TimelineEntry::AgentLifecycle {
            run,
            exchange,
            agent,
            ..
        } => {
            matches!(
                run.state,
                crate::agent::AgentState::Starting | crate::agent::AgentState::Closing
            ) || exchange.iter().any(|exchange| {
                !exchange.is_finished()
                    && (exchange.execution_started_at_ms().is_some()
                        || exchange.turn.iter().any(|turn| {
                            exchange
                                .tools_for_turn(turn.id())
                                .any(|tool| tool.state() == crate::turn::ToolState::Running)
                        }))
            }) || agent.iter().any(ticking)
        }
        TimelineEntry::ObservedAgent {
            agent, children, ..
        } => {
            agent.availability() == crate::agent::ObservedAgentAvailability::Available
                && agent.exchanges().iter().any(|exchange| {
                    !exchange.is_finished() && exchange.execution_started_at_ms().is_some()
                })
                || children.iter().any(ticking)
        }
        TimelineEntry::Status { status, checks, .. } => status.ticking() || checks.is_some(),
        TimelineEntry::Checks { .. } => false,
        _ => false,
    }
}

fn serialize_entry_list(entry_list: &[TimelineEntry]) -> Result<Vec<Value>> {
    entry_list
        .iter()
        .map(crate::codec::to_value)
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
            checks: None,
            agent_count: 0,
            status_thoughts: true,
            id: "session:status".into(),
            created_at_ms: 0,
            status: kind,
        }
    }

    #[test]
    fn file_edit_output_reconciles_the_change_node_without_a_tool_cache() -> anyhow::Result<()> {
        use forge_provider_api::{
            InspectableArguments, ItemCompletion, ItemEvent, ItemId, ItemStart, ProviderEvent,
            TextUpdate, ToolCompletion, ToolDescriptor, ToolDiagnostic, ToolItemEvent, ToolOutcome,
        };
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(serde_json::json!({
            "id":"edit", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Implement", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            "patch",
            ItemStart::FileChange(ToolDescriptor {
                semantics: Default::default(),
                name: "patch".into(),
                arguments: InspectableArguments::new(serde_json::json!({})),
                title: "file changes".into(),
            }),
            [ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            }],
            1,
        )?;
        let mut stream = TimelineStream::new("session".into());
        stream.initialize(vec![TimelineEntry::Exchange {
            id: exchange.id.clone(),

            created_at_ms: 0,
            exchange: (exchange.clone()).into(),

            agent_by_id: Default::default(),
        }])?;
        let event = ProviderEvent::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: ItemId::new("patch").unwrap(),
            event: ItemEvent::Tool {
                event: ToolItemEvent::Output(TextUpdate::Append(
                    "Patch context did not match".into(),
                )),
            },
        };
        let scope = crate::provider::EventScope::Invocation {
            invocation: address.scope.clone(),
        };
        exchange.observe_provider_event(&scope, &event, 2)?;
        exchange.observe_provider_event(
            &scope,
            &ProviderEvent::Item {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                item_id: ItemId::new("patch").unwrap(),
                event: ItemEvent::Completed(ItemCompletion::FileChange(ToolCompletion {
                    outcome: ToolOutcome::Failed {
                        diagnostic: ToolDiagnostic {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: forge_provider_api::SanitizedMessage::new(
                                "Patch context did not match",
                            )
                            .unwrap(),
                        },
                    },
                    descriptor: None,
                    output: None,
                    changes: Default::default(),
                })),
            },
            2,
        )?;
        assert!(stream.append_tool_output(&exchange, &event)?.is_none());
        assert_eq!(stream.revision(), 1);
        let patch = stream.update_live(Some(&exchange), None, SessionPhase::Idle, true)?;
        assert!(matches!(
            patch.operation.as_slice(),
            [
                TimelineOperation::Replace { .. },
                TimelineOperation::Insert {
                    entry: TimelineEntry::Status {
                        status: SessionPhase::Idle,
                        ..
                    },
                    ..
                }
            ]
        ));
        let TimelineEntry::Exchange {
            exchange: updated, ..
        } = &stream.entry_list()[0]
        else {
            panic!("expected reconciled exchange");
        };
        let tool = updated.tools_for_turn(updated.turn[0].id()).next().unwrap();
        assert_eq!(tool.output, "Patch context did not match");
        assert!(tool.failed);
        Ok(())
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
    fn reconciliation_preserves_patch_order_across_moves_removals_and_replacements() {
        let entry = |id: &str, name: &str| TimelineEntry::SessionEvent {
            id: id.into(),
            created_at_ms: 0,
            event: crate::timeline::SessionEventRecord {
                id: id.into(),
                session_id: "session".into(),
                owner: crate::timeline::TimelineOwner::session("session"),
                created_at_ms: 0,
                detail: crate::timeline::SessionEventKind::Renamed { name: name.into() },
            },
        };
        let original = vec![
            entry("first", "original"),
            entry("removed", "removed"),
            entry("last", "last"),
        ];
        let mut replica: Vec<_> = original
            .iter()
            .map(|entry| crate::codec::to_value(entry).unwrap())
            .collect();
        let mut stream = TimelineStream::new("session".into());
        stream.initialize(original).unwrap();
        let next = vec![
            entry("last", "last"),
            entry("first", "changed"),
            entry("new", "new"),
        ];
        let expected: Vec<_> = next
            .iter()
            .map(|entry| crate::codec::to_value(entry).unwrap())
            .collect();
        let patch = stream.reconcile(next).unwrap();
        for operation in patch.operation {
            match operation {
                TimelineOperation::Remove { index, .. } => {
                    replica.remove(index);
                }
                TimelineOperation::Insert { index, entry } => {
                    replica.insert(index, crate::codec::to_value(entry).unwrap())
                }
                TimelineOperation::Replace { index, entry } => {
                    replica[index] = crate::codec::to_value(entry).unwrap()
                }
                _ => panic!("reconciliation emitted a streaming operation"),
            }
        }
        assert_eq!(replica, expected);
        assert_eq!(stream.value_list, expected);
        let unchanged = vec![
            entry("last", "last"),
            entry("first", "changed"),
            entry("new", "new"),
        ];
        assert!(stream.reconcile(unchanged).unwrap().is_empty());
    }

    #[test]
    fn sessions_advance_independent_revision_sequences() {
        let mut first = TimelineStream::new("first".into());
        let mut second = TimelineStream::new("second".into());
        first.initialize(vec![status(SessionPhase::Idle)]).unwrap();
        second.initialize(vec![status(SessionPhase::Idle)]).unwrap();

        let first_patch = first
            .reconcile(vec![status(SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: Some(5),
                activity: WorkflowActivity::Working,
                execution: None,
                reasoning_summary: None,
            })])
            .unwrap();

        assert_eq!(first_patch.session_id, "first");
        assert_eq!(first.revision(), 2);
        assert_eq!(second.revision(), 1);
    }
}
