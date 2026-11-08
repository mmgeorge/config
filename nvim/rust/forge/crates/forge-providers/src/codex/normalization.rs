use super::decode::*;
use anyhow::Result;
use forge_provider_api::agent::{
    AgentActiveState, AgentDelegation, AgentDescription, AgentEvent, AgentLifecycleEvent,
};
use forge_provider_api::compaction::CompactionResult;

use forge_provider_api::*;
use serde_json::Value;
use std::collections::HashMap;

/// Classify terminal status once for transcript and retained native execution.
pub(super) fn terminal_outcome(
    turn: &Value,
) -> std::result::Result<forge_provider_api::turn::TurnOutcome, ProviderError> {
    use forge_provider_api::turn::{TurnCompletionReason, TurnOutcome};
    match turn.get("status").and_then(Value::as_str) {
        Some("completed") => Ok(TurnOutcome::Completed {
            stop_reason: TurnCompletionReason::Normal,
        }),
        Some("interrupted" | "cancelled") => Ok(TurnOutcome::Interrupted),
        Some("failed") => Ok(TurnOutcome::Failed {
            error: ProviderFailure::new(
                ProviderOperation::TurnStart,
                turn.get("error")
                    .map_or(ProviderFailureKind::Unknown, super::retry::failure_kind),
            )
            .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain),
            retry: forge_provider_api::turn::RetryDecision::NotAllowed,
        }),
        Some(_) | None => Err(protocol_error()),
    }
}

/// Validate every supplied spelling of one native error identity before resolving ownership.
fn error_identity(
    params: &Value,
    paths: &[&str],
) -> std::result::Result<Option<String>, ProviderError> {
    let mut identity = None;
    for path in paths {
        match params.pointer(path) {
            None | Some(Value::Null) => {}
            Some(Value::String(value)) if !value.trim().is_empty() => {
                if identity.as_ref().is_some_and(|retained| retained != value) {
                    return Err(protocol_error());
                }
                identity = Some(value.clone());
            }
            Some(
                Value::String(_)
                | Value::Bool(_)
                | Value::Number(_)
                | Value::Array(_)
                | Value::Object(_),
            ) => return Err(protocol_error()),
        }
    }
    Ok(identity)
}

fn protocol_error() -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(
        ProviderOperation::TurnStart,
        ProviderFailureKind::Protocol,
    ))
}

/// Collect decoded activity before ordered delivery to the retained host owner.
#[derive(Default)]
pub(super) struct CodexOutput {
    pub(super) event: Vec<AdapterActivity>,
}

/// Map native protocol identities to normalized host objects under exact retained owners.
#[derive(Default)]
pub struct CodexNormalization {
    scope: Option<EventScope>,
    threads: HashMap<String, ThreadId>,
    turns: HashMap<(String, String), TurnAddress>,
    items: HashMap<(TurnId, String, ItemPart), NativeItem>,
    pending_child: HashMap<String, Vec<Value>>,
    observed_starts: Vec<(TurnAddress, String, String, i64)>,
    /// Native `(thread, turn)` cycles that carried a host session operation, such as
    /// manual compaction. They own no host invocation, so late notifications for them
    /// contribute only thread-level facts.
    operation_turns: std::collections::HashSet<(String, String)>,
    terminal: HashMap<TurnId, (forge_provider_api::turn::TurnOutcome, i64)>,
    usage: HashMap<TurnId, Vec<TurnUsageReport>>,
    /// Native thread → the turn that owns its latest cumulative usage key. Codex
    /// repeats the previous call's `last` breakdown with an unchanged cumulative
    /// total when a later turn ends before any model call completes.
    thread_usage: HashMap<String, (TurnId, String)>,
    models: HashMap<TurnId, ModelId>,
    unresolved_sessions: std::collections::HashSet<TurnId>,
    agents: HashMap<String, forge_provider_api::AgentId>,
    pending_agents: HashMap<(TurnId, String), forge_provider_api::AgentId>,
    delegations: HashMap<String, AgentDelegation>,
    /// Native thread → compaction notifications observed on it. Compaction rebuilds model-visible
    /// history, so context the adapter appended earlier may no longer reach the model.
    compaction_counts: HashMap<String, u64>,
    pub replay_sensitive: bool,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum ItemPart {
    Message,
    Summary(u64),
    Reasoning(u64),
    Tool,
}

/// Retain a positively established background item's immutable host owner for native collection.
pub(super) struct BackgroundItemAddress {
    turn: TurnAddress,
    item_id: ItemId,
}

/// Preserve positive native unified-exec ownership separately from independent lifetime proof.
#[derive(Clone, Debug, Eq, PartialEq)]
enum NativeItemLifetime {
    Foreground,
    UnifiedExec { process_id: String },
    Background,
    NativeExited,
    UnobservableProcess,
}

/// Retain exact callback routing without substituting the active root for a native child.
pub(super) enum NativeCallbackOwner {
    Root,
    ObservedChild(TurnAddress),
}

struct NativeItem {
    id: ItemId,
    start: ItemStart,
    text: String,
    completed: Option<String>,
    lifetime: NativeItemLifetime,
    descriptor: Option<forge_provider_api::tool::ToolDescriptor>,
    output: String,
    changes: ProviderChangeSet,
}

impl NativeItem {
    /// Report whether Forge closed this tool itself, at turn end or background
    /// teardown, rather than from a native terminal snapshot. Native completions
    /// record their non-empty JSON signature; `finish_tool` records an empty one.
    fn completion_synthesized(&self) -> bool {
        self.completed.as_deref() == Some("")
            && !matches!(
                self.start,
                ItemStart::UserMessage(_) | ItemStart::AgentMessage(_) | ItemStart::Reasoning(_)
            )
    }
}

impl CodexNormalization {
    /// Get the number of compaction notifications observed on one native thread.
    pub(super) fn compaction_count(&self, native_thread: &str) -> u64 {
        self.compaction_counts
            .get(native_thread)
            .copied()
            .unwrap_or_default()
    }

    /// Bind the next admitted operation without reassigning retained native objects.
    pub fn bind(&mut self, scope: Option<impl Into<EventScope>>) {
        self.scope = scope.map(Into::into);
        self.replay_sensitive = false;
    }

    /// Register a positively established native conversation with its independent host owner.
    pub(super) fn admit_thread(
        &mut self,
        native: &str,
        owner: &ThreadId,
    ) -> std::result::Result<(), ProviderError> {
        if native.trim().is_empty() || self.threads.get(native).is_some_and(|old| old != owner) {
            return Err(protocol_error());
        }
        self.threads.insert(native.to_owned(), owner.clone());
        Ok(())
    }

    /// Allocate one host turn identity for the acknowledged native response cycle.
    pub(super) fn admit_turn(
        &mut self,
        thread: &str,
        native_turn: &str,
        scope: &InvocationScope,
    ) -> std::result::Result<TurnAddress, ProviderError> {
        self.admit_thread(thread, &scope.thread_id)?;
        if native_turn.trim().is_empty() {
            return Err(protocol_error());
        }
        let key = (thread.to_owned(), native_turn.to_owned());
        if let Some(old) = self.turns.get(&key) {
            if &old.scope != scope {
                return Err(protocol_error());
            }
            return Ok(old.clone());
        }
        let address = TurnAddress {
            scope: scope.clone(),
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().map_err(|_| protocol_error())?,
        };
        self.turns.insert(key, address.clone());
        Ok(address)
    }

    /// Identify a native turn that a host session operation, not an invocation, owns.
    fn operation_turn(&self, params: &Value) -> bool {
        let Some(thread) = pointer_string(params, &["/threadId", "/thread_id"]) else {
            return false;
        };
        pointer_string(params, &["/turnId", "/turn_id", "/turn/id"])
            .is_some_and(|turn| self.operation_turns.contains(&(thread, turn)))
    }

    /// Identify a compaction item by the bound host operation when it compacts that
    /// operation's thread, so host attempt and native progress settle one record.
    fn compaction_id(
        &self,
        thread: &ThreadId,
        params: &Value,
    ) -> std::result::Result<String, ProviderError> {
        if let Some(EventScope::SessionOperation {
            thread_id: owner,
            operation_id,
        }) = &self.scope
            && owner == thread
        {
            return Ok(operation_id.as_str().to_owned());
        }
        pointer_string(params, &["/item/id"]).ok_or_else(protocol_error)
    }

    /// Transfer positively observed child starts to their common retained lifecycle owner.
    pub(super) fn take_observed_starts(&mut self) -> Vec<(TurnAddress, String, String, i64)> {
        std::mem::take(&mut self.observed_starts)
    }

    /// Check a native child thread against its retained trusted registration.
    pub(super) fn owns_child_thread(&self, native: &str) -> bool {
        self.agents.contains_key(native) && self.threads.contains_key(native)
    }

    /// Resolve explicit native callback identities through their original admitted ancestry.
    pub(super) fn callback_owner(
        &self,
        params: &Value,
        parent: &InvocationScope,
    ) -> std::result::Result<NativeCallbackOwner, ProviderError> {
        let thread = error_identity(
            params,
            &[
                "/threadId",
                "/thread_id",
                "/turn/threadId",
                "/turn/thread_id",
            ],
        )?
        .ok_or_else(protocol_error)?;
        let turn = error_identity(params, &["/turnId", "/turn_id", "/turn/id"])?
            .ok_or_else(protocol_error)?;
        let address = self
            .turns
            .get(&(thread.clone(), turn))
            .ok_or_else(protocol_error)?;
        if self.terminal.contains_key(&address.turn_id) {
            return Err(protocol_error());
        }
        if &address.scope == parent {
            return Ok(NativeCallbackOwner::Root);
        }
        let mut ancestor_thread = thread;
        let mut visited = std::collections::HashSet::new();
        loop {
            let delegation = self
                .delegations
                .get(&ancestor_thread)
                .ok_or_else(protocol_error)?;
            let ((native_thread, _), ancestor) = self
                .turns
                .iter()
                .find(|(_, candidate)| {
                    candidate.thread_id == delegation.parent_thread_id
                        && candidate.turn_id == delegation.parent_turn_id
                })
                .ok_or_else(protocol_error)?;
            if &ancestor.scope == parent {
                return Ok(NativeCallbackOwner::ObservedChild(address.clone()));
            }
            if ancestor.scope.thread_id == parent.thread_id
                || !visited.insert(ancestor.turn_id.clone())
            {
                return Err(protocol_error());
            }
            ancestor_thread = native_thread.clone();
        }
    }

    /// Get the native terminal outcome retained before any provider collection waits.
    pub(super) fn terminal_evidence(
        &self,
        address: &TurnAddress,
    ) -> Option<(forge_provider_api::turn::TurnOutcome, i64)> {
        self.terminal.get(&address.turn_id).copied()
    }

    /// Publish a newly confirmed model under the original turn while deduplicating identical replay.
    pub(super) fn resolve_model(
        &mut self,
        address: &TurnAddress,
        model: ResolvedModel,
    ) -> Option<AdapterActivity> {
        if self.models.get(&address.turn_id) == Some(&model.model_id) {
            return None;
        }
        self.models
            .insert(address.turn_id.clone(), model.model_id.clone());
        Some(AdapterActivity::Turn {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            event: TurnMetadataEvent::ModelResolved(model),
        })
    }

    /// Identify native terminal evidence whose command lifetime remains unobservable.
    pub(super) fn has_unresolved_sessions(&self, address: &TurnAddress) -> bool {
        self.unresolved_sessions.contains(&address.turn_id)
    }

    /// Get identified accounting retained for the original response cycle.
    pub(super) fn final_usage(&self, address: &TurnAddress) -> Vec<TurnUsageReport> {
        self.usage
            .get(&address.turn_id)
            .cloned()
            .unwrap_or_default()
    }

    /// Resolve a normalized activity to its retained native owner's writer scope.
    pub(super) fn activity_address(&self, activity: &AdapterActivity) -> Option<TurnAddress> {
        let (thread, turn) = match activity {
            AdapterActivity::Turn {
                thread_id, turn_id, ..
            }
            | AdapterActivity::Item {
                thread_id, turn_id, ..
            } => (thread_id, Some(turn_id)),
            AdapterActivity::Thread { thread_id, .. } => (thread_id, None),
            AdapterActivity::Agent { .. } | AdapterActivity::Warning(_) => return None,
        };
        self.turns
            .values()
            .find(|address| {
                &address.thread_id == thread && turn.is_none_or(|turn| &address.turn_id == turn)
            })
            .cloned()
    }

    /// Resolve only previously admitted native turns; current admission never supplies missing ownership.
    fn address(&self, params: &Value) -> Option<TurnAddress> {
        let thread = pointer_string(
            params,
            &[
                "/threadId",
                "/thread_id",
                "/turn/threadId",
                "/turn/thread_id",
            ],
        )?;
        if let Some(turn) = pointer_string(params, &["/turnId", "/turn_id", "/turn/id"]) {
            return self.turns.get(&(thread, turn)).cloned();
        }
        if ["/turnId", "/turn_id", "/turn/id"]
            .iter()
            .any(|path| params.pointer(path).is_some())
        {
            return None;
        }
        let item = pointer_string(params, &["/itemId", "/item_id", "/item/id"])?;
        let mut owners = self
            .items
            .iter()
            .filter(|((_, native, part), value)| {
                native == &item
                    && *part == ItemPart::Tool
                    && value.lifetime == NativeItemLifetime::Background
            })
            .filter_map(|((turn, _, _), _)| {
                self.turns.iter().find_map(|((native_thread, _), address)| {
                    (native_thread == &thread && &address.turn_id == turn)
                        .then_some(address.clone())
                })
            });
        let owner = owners.next()?;
        if owners.any(|other| other != owner) {
            return None;
        }
        Some(owner)
    }

    /// Route explicitly correlated native errors without inventing a turn from current work.
    fn native_error(&self, params: &Value) -> std::result::Result<AdapterActivity, ProviderError> {
        let thread = error_identity(params, &["/threadId", "/thread_id", "/thread/id"])?
            .ok_or_else(protocol_error)?;
        let error = ProviderFailure::new(
            ProviderOperation::TurnStart,
            super::retry::failure_kind(params.get("error").unwrap_or(params)),
        )
        .at_phase(ProviderPhase::Observation, FailureDisposition::Uncertain);
        match error_identity(params, &["/turnId", "/turn_id", "/turn/id"])? {
            Some(turn)
                if !self
                    .operation_turns
                    .contains(&(thread.clone(), turn.clone())) =>
            {
                let address = self.turns.get(&(thread, turn)).ok_or_else(protocol_error)?;
                Ok(AdapterActivity::Turn {
                    thread_id: address.thread_id.clone(),
                    turn_id: address.turn_id.clone(),
                    event: TurnMetadataEvent::Error { error },
                })
            }
            Some(_) | None => Ok(AdapterActivity::Thread {
                thread_id: self
                    .threads
                    .get(&thread)
                    .ok_or_else(protocol_error)?
                    .clone(),
                event: ThreadEvent::Error { error },
            }),
        }
    }

    /// Normalize late observations only for exact retained native ownership.
    pub async fn normalize_retained(
        &mut self,
        message: &Value,
        workspace: &str,
    ) -> Result<Option<Vec<AdapterActivity>>> {
        let params = message.get("params").unwrap_or(&Value::Null);
        if message["method"]
            .as_str()
            .is_some_and(|method| method.eq_ignore_ascii_case("error"))
        {
            return Ok(Some(vec![self.native_error(params)?]));
        }
        if self.operation_turn(params) {
            return self.activities(message, workspace).map(Some);
        }
        if self.address(params).is_none() && message["method"] != "turn/started" {
            if message["method"]
                .as_str()
                .is_some_and(|method| method.to_ascii_lowercase().contains("tokenusage"))
            {
                return self.activities(message, workspace).map(Some);
            }
            self.defer_child(message);
            return Ok(None);
        }
        if message["method"] == "turn/started"
            && let Some(thread) = pointer_string(params, &["/threadId", "/thread_id"])
        {
            if self.address(params).is_none()
                && self.threads.contains_key(&thread)
                && !self.agents.contains_key(&thread)
            {
                return Err(protocol_error().into());
            }
            if !self.threads.contains_key(&thread) {
                self.pending_child
                    .entry(thread)
                    .or_default()
                    .push(message.clone());
                return Ok(None);
            }
        }
        self.activities(message, workspace).map(Some)
    }

    /// Retain early child observations privately until native delegation establishes a trusted relationship.
    pub(super) fn defer_child(&mut self, message: &Value) {
        if let Some(thread) = pointer_string(
            message.get("params").unwrap_or(&Value::Null),
            &["/threadId", "/thread_id"],
        ) {
            self.pending_child
                .entry(thread)
                .or_default()
                .push(message.clone());
        }
    }

    /// Observe output for positively established independent background work.
    pub async fn normalize_background(
        &mut self,
        message: &Value,
        workspace: &str,
    ) -> Result<Option<Vec<AdapterActivity>>> {
        let params = message.get("params").unwrap_or(&Value::Null);
        let Some(address) = self.address(params) else {
            return Ok(None);
        };
        let Some(item) = pointer_string(params, &["/itemId", "/item_id", "/item/id"]) else {
            return Ok(None);
        };
        if !self
            .items
            .get(&(address.turn_id, item, ItemPart::Tool))
            .is_some_and(|item| item.lifetime == NativeItemLifetime::Background)
        {
            return Ok(None);
        }
        self.activities(message, workspace).map(Some)
    }

    /// Identify retained background work without reopening completed foreground items.
    pub fn has_pending_background_tools(&self) -> bool {
        self.items
            .values()
            .any(|item| item.lifetime == NativeItemLifetime::Background && item.completed.is_none())
    }

    /// Resolve a retained host command to private protocol identities for exact native collection.
    pub(super) fn native_tool(&self, address: &BackgroundItemAddress) -> Option<(String, String)> {
        let native = self.items.iter().find_map(|((turn, native, part), item)| {
            (turn == &address.turn.turn_id
                && *part == ItemPart::Tool
                && item.id.as_str() == address.item_id.as_str())
            .then_some(native.clone())
        })?;
        let thread = self
            .turns
            .iter()
            .find_map(|((thread, _), turn)| (turn == &address.turn).then_some(thread.clone()))?;
        Some((thread, native))
    }

    /// Get exact background item owners for native collection.
    pub(super) fn pending_background_addresses(&self) -> Vec<BackgroundItemAddress> {
        self.items
            .iter()
            .filter(|(_, item)| {
                item.lifetime == NativeItemLifetime::Background && item.completed.is_none()
            })
            .filter_map(|((turn, _, _), item)| {
                self.turns
                    .values()
                    .find(|address| &address.turn_id == turn)
                    .map(|address| BackgroundItemAddress {
                        turn: address.clone(),
                        item_id: item.id.clone(),
                    })
            })
            .collect()
    }

    /// Name the native command items a user interrupt must stop: foreground commands of
    /// the interrupted native turn that have no native terminal status. Backgrounded and
    /// unified-exec sessions keep their independent lifetime.
    pub(super) fn foreground_command_items(
        &self,
        thread_id: &str,
        native_turn: &str,
    ) -> std::collections::HashSet<String> {
        let Some(address) = self
            .turns
            .get(&(thread_id.to_owned(), native_turn.to_owned()))
        else {
            return Default::default();
        };
        self.items
            .iter()
            .filter(|((turn, _, part), item)| {
                turn == &address.turn_id
                    && *part == ItemPart::Tool
                    && matches!(item.start, ItemStart::CommandExecution(_))
                    && item.lifetime == NativeItemLifetime::Foreground
                    && (item.completed.is_none() || item.completion_synthesized())
            })
            .map(|((_, native, _), _)| native.clone())
            .collect()
    }

    /// Close retained background work only after the native inventory establishes absence.
    pub(super) fn interrupt_background(
        &mut self,
        address: &BackgroundItemAddress,
    ) -> Option<AdapterActivity> {
        let item = self.items.iter_mut().find_map(|((turn, _, _), item)| {
            (turn == &address.turn.turn_id
                && item.id.as_str() == address.item_id.as_str()
                && item.lifetime == NativeItemLifetime::Background
                && item.completed.is_none())
            .then_some(item)
        })?;
        Some(finish_tool(
            &address.turn,
            item,
            ToolOutcome::Interrupted,
            None,
            None,
        ))
    }

    /// Decode one native notification into restricted activity records.
    pub(super) async fn normalize(
        &mut self,
        message: &Value,
        output: &mut CodexOutput,
        sink: Option<&AdapterActivityWriter>,
        workspace: &str,
    ) -> Result<()> {
        let activities = self.activities(message, workspace)?;
        for activity in activities {
            if let Some(sink) = sink {
                sink.send_wait(activity).await?;
            } else {
                output.event.push(activity);
            }
        }
        Ok(())
    }

    /// Preserve typed item identity, content, outcome, and exact native error scope.
    fn activities(&mut self, message: &Value, workspace: &str) -> Result<Vec<AdapterActivity>> {
        let method = message["method"].as_str().unwrap_or_default();
        let params = message.get("params").unwrap_or(&Value::Null);
        if method.eq_ignore_ascii_case("error")
            || params.pointer("/item/type").and_then(Value::as_str) == Some("error")
        {
            return Ok(vec![self.native_error(params)?]);
        }
        let mut events = Vec::new();
        let native_thread = pointer_string(params, &["/threadId", "/thread_id"]);
        if let Some(usage) = context_usage(params) {
            let thread = native_thread
                .as_ref()
                .and_then(|thread| self.threads.get(thread))
                .ok_or_else(protocol_error)?;
            events.push(AdapterActivity::Thread {
                thread_id: thread.clone(),
                event: ThreadEvent::ContextUsage(usage),
            });
        }
        let item_type = params
            .pointer("/item/type")
            .and_then(Value::as_str)
            .unwrap_or_default();
        if matches!(method, "item/started" | "item/completed") && item_type == "contextCompaction" {
            let (native_id, thread) = native_thread
                .as_ref()
                .and_then(|native_id| self.threads.get_key_value(native_id))
                .ok_or_else(protocol_error)?;
            *self.compaction_counts.entry(native_id.clone()).or_default() += 1;
            events.push(AdapterActivity::Thread {
                thread_id: thread.clone(),
                event: ThreadEvent::CompactionUpdated(Box::new(Compaction {
                    id: self.compaction_id(thread, params)?,
                    thread_id: thread.clone().as_str().to_owned(),
                    state: if method == "item/started" {
                        CompactionState::Running
                    } else {
                        CompactionState::Completed
                    },
                    manual: self
                        .scope
                        .as_ref()
                        .is_some_and(|scope| scope.invocation().is_none()),
                    started_at_ms: None,
                    completed_at_ms: None,
                    result: CompactionResult::default(),
                })),
            });
            return Ok(events);
        }
        if method == "turn/started"
            && let (Some(thread), Some(turn)) = (
                native_thread.as_ref(),
                pointer_string(params, &["/turnId", "/turn/id"]),
            )
            && !self.turns.contains_key(&(thread.clone(), turn.clone()))
        {
            let owner = self
                .threads
                .get(thread)
                .cloned()
                .ok_or_else(protocol_error)?;
            // Codex runs `thread/compact/start` as a turn on the compacted thread. That
            // turn executes the bound host operation; admitting it as an invocation would
            // publish activity under a scope the host never created.
            if let Some(EventScope::SessionOperation {
                thread_id: operation_owner,
                ..
            }) = &self.scope
                && operation_owner == &owner
            {
                self.operation_turns.insert((thread.clone(), turn));
                return Ok(events);
            }
            let scope = InvocationScope::new(owner, uuid::Uuid::new_v4().to_string());
            let address = self.admit_turn(thread, &turn, &scope)?;
            self.observed_starts
                .push((address, thread.clone(), turn, timestamp()));
        }
        if self.operation_turn(params) {
            return Ok(events);
        }
        let Some(address) = self.address(params) else {
            if method == "turn/started" || method == "turn/completed" || method.starts_with("item/")
            {
                return Err(protocol_error().into());
            }
            return Ok(events);
        };
        if let Some(model) = params
            .pointer("/turn/model")
            .or_else(|| params.get("model"))
            .and_then(Value::as_str)
        {
            let model = ResolvedModel {
                model_id: ModelId::new(model).map_err(|_| protocol_error())?,
                observed_at_ms: timestamp(),
            };
            if let Some(event) = self.resolve_model(&address, model) {
                events.push(event);
            }
        }
        if method == "rawResponseItem/completed" {
            let raw = &params["item"];
            if matches!(
                raw["type"].as_str(),
                Some("function_call_output" | "custom_tool_call_output")
            ) {
                let call = raw
                    .get("call_id")
                    .and_then(Value::as_str)
                    .filter(|call| !call.is_empty())
                    .ok_or_else(protocol_error)?;
                if let Some(item) =
                    self.items
                        .get_mut(&(address.turn_id.clone(), call.to_owned(), ItemPart::Tool))
                    && let NativeItemLifetime::UnifiedExec { process_id } = &item.lifetime
                {
                    let output = raw
                        .get("output")
                        .and_then(Value::as_str)
                        .ok_or_else(protocol_error)?;
                    match native_exec_reply(output)? {
                        NativeExecReply::Yielded {
                            process_id: observed,
                        } => {
                            if process_id != observed {
                                return Err(protocol_error().into());
                            }
                            item.lifetime = NativeItemLifetime::Background;
                            events.push(item_event(
                                &address,
                                &item.id,
                                ItemEvent::Tool {
                                    event: forge_provider_api::tool::ToolItemEvent::Backgrounded,
                                },
                            ));
                        }
                        NativeExecReply::Exited => item.lifetime = NativeItemLifetime::NativeExited,
                        NativeExecReply::Unknown => {}
                    }
                }
            }
        } else if method == "turn/completed" {
            let outcome = terminal_outcome(&params["turn"])?;
            if self
                .terminal
                .get(&address.turn_id)
                .is_some_and(|(old, _)| old != &outcome)
            {
                return Err(protocol_error().into());
            }
            self.terminal
                .entry(address.turn_id.clone())
                .or_insert((outcome, timestamp()));
            // Codex 0.164 keeps unified_exec_manager in Session after normal TurnComplete;
            // only its exit watcher emits command completion. A known started process with
            // no command-end at this ordered boundary therefore retains an independent lifetime.
            if matches!(
                outcome,
                forge_provider_api::turn::TurnOutcome::Completed { .. }
            ) {
                for ((turn, _, _), item) in &mut self.items {
                    if turn == &address.turn_id
                        && item.completed.is_none()
                        && matches!(item.lifetime, NativeItemLifetime::UnifiedExec { .. })
                    {
                        item.lifetime = NativeItemLifetime::Background;
                        events.push(item_event(
                            &address,
                            &item.id,
                            ItemEvent::Tool {
                                event: forge_provider_api::tool::ToolItemEvent::Backgrounded,
                            },
                        ));
                    }
                }
            }
            for ((turn, _, _), item) in &mut self.items {
                if turn != &address.turn_id
                    || item.completed.is_some()
                    || item.lifetime == NativeItemLifetime::Background
                {
                    continue;
                }
                if matches!(
                    item.lifetime,
                    NativeItemLifetime::UnifiedExec { .. }
                        | NativeItemLifetime::UnobservableProcess
                ) {
                    self.unresolved_sessions.insert(address.turn_id.clone());
                    continue;
                }
                match &item.start {
                    ItemStart::UserMessage(_)
                    | ItemStart::AgentMessage(_)
                    | ItemStart::Reasoning(_) => {
                        events.push(finish_message(&address, item, MessageOutcome::Interrupted));
                    }
                    ItemStart::CommandExecution(_)
                    | ItemStart::FileChange(_)
                    | ItemStart::ToolCall(_) => {
                        events.push(finish_tool(
                            &address,
                            item,
                            ToolOutcome::Interrupted,
                            None,
                            None,
                        ));
                    }
                }
            }
        } else if method.to_ascii_lowercase().contains("tokenusage") {
            // Context adjustments describe occupancy and do not certify token expenditure.
            if let Some(NativeUsageObservation::Expenditure {
                report_key,
                usage,
                cumulative,
                cumulative_total,
            }) = usage_update(params)
            {
                // An unchanged cumulative total spends nothing, so a repeat that reaches a
                // later turn belongs to the turn that first reported it.
                let thread = address.thread_id.as_str().to_owned();
                if self
                    .thread_usage
                    .get(&thread)
                    .is_some_and(|(turn, key)| *key == report_key && *turn != address.turn_id)
                {
                    return Ok(events);
                }
                self.thread_usage
                    .insert(thread, (address.turn_id.clone(), report_key.clone()));
                let report = TurnUsageReport::tokens(
                    UsageReportId::new(report_key).map_err(|_| protocol_error())?,
                    None,
                    timestamp(),
                    Some(usage),
                    Some(cumulative),
                    cumulative_total,
                )
                .map_err(|_| protocol_error())?;
                let reports = self.usage.entry(address.turn_id.clone()).or_default();
                if let Some(old) = reports
                    .iter_mut()
                    .find(|old| old.report_id() == report.report_id())
                {
                    old.merge(&report).map_err(|_| protocol_error())?;
                } else {
                    reports.push(report.clone());
                }
                events.push(AdapterActivity::Turn {
                    thread_id: address.thread_id.clone(),
                    turn_id: address.turn_id.clone(),
                    event: TurnMetadataEvent::Usage(report),
                });
            }
        } else if matches!(method, "item/started" | "item/completed") && item_type == "userMessage"
        {
            let native = pointer_string(params, &["/item/id"]).ok_or_else(protocol_error)?;
            let text = params
                .pointer("/item/content")
                .and_then(Value::as_array)
                .map(|content| {
                    content
                        .iter()
                        .filter_map(|item| item.get("text").and_then(Value::as_str))
                        .collect::<Vec<_>>()
                        .join("\n")
                })
                .or_else(|| {
                    params
                        .pointer("/item/text")
                        .and_then(Value::as_str)
                        .map(str::to_owned)
                });
            self.message(
                &address,
                native,
                ItemPart::Message,
                ItemStart::UserMessage(UserMessageStart {
                    text: String::new(),
                    submission_id: params
                        .pointer("/item/clientId")
                        .and_then(Value::as_str)
                        .and_then(super::provider::submission_from_client_id),
                }),
                if method == "item/completed" {
                    NativeTextObservation::Completed(text.as_deref())
                } else {
                    NativeTextObservation::Progress(text.as_deref())
                },
                &mut events,
            )?;
        } else if method.eq_ignore_ascii_case("item/agentMessage/delta")
            || (matches!(method, "item/started" | "item/completed") && item_type == "agentMessage")
        {
            let native = pointer_string(params, &["/itemId", "/item_id", "/item/id"])
                .ok_or_else(protocol_error)?;
            let classification = match params
                .pointer("/item/phase")
                .or_else(|| params.get("phase"))
            {
                None | Some(Value::Null) => MessageClassification::Unclassified,
                Some(Value::String(phase)) if phase == "commentary" => {
                    MessageClassification::Known(AgentMessageRole::Commentary)
                }
                Some(Value::String(phase)) if phase == "final_answer" => {
                    MessageClassification::Known(AgentMessageRole::Response)
                }
                Some(Value::String(_)) => {
                    events.push(AdapterActivity::Warning(ProviderWarning {
                        scope: WarningScope::Turn {
                            thread_id: address.thread_id.clone(),
                            turn_id: address.turn_id.clone(),
                        },
                        kind: ProviderWarningKind::UnsupportedMessageClassification,
                        message: SanitizedMessage::new(
                            "Provider reported an unsupported message phase.",
                        )
                        .expect("static text"),
                    }));
                    MessageClassification::Unclassified
                }
                Some(_) => return Err(protocol_error().into()),
            };
            let completed = method == "item/completed";
            let text = if method.ends_with("/delta") {
                params["delta"].as_str()
            } else {
                params.pointer("/item/text").and_then(Value::as_str)
            };
            self.message(
                &address,
                native,
                ItemPart::Message,
                ItemStart::AgentMessage(AgentMessageStart {
                    text: String::new(),
                    classification,
                }),
                if completed {
                    NativeTextObservation::Completed(text)
                } else {
                    NativeTextObservation::Progress(text)
                },
                &mut events,
            )?;
        } else if method.starts_with("item/reasoning/")
            || (matches!(method, "item/started" | "item/completed") && item_type == "reasoning")
        {
            let native = pointer_string(params, &["/itemId", "/item_id", "/item/id"])
                .ok_or_else(protocol_error)?;
            if method.ends_with("summaryTextDelta")
                || method.ends_with("textDelta")
                || method.ends_with("summaryPartAdded")
            {
                let summary =
                    method.ends_with("summaryTextDelta") || method.ends_with("summaryPartAdded");
                let index = params[if summary {
                    "summaryIndex"
                } else {
                    "contentIndex"
                }]
                .as_u64()
                .ok_or_else(protocol_error)?;
                self.message(
                    &address,
                    native,
                    if summary {
                        ItemPart::Summary(index)
                    } else {
                        ItemPart::Reasoning(index)
                    },
                    ItemStart::Reasoning(ReasoningStart {
                        text: String::new(),
                        kind: if summary {
                            ReasoningKind::Summary
                        } else {
                            ReasoningKind::Full
                        },
                    }),
                    NativeTextObservation::Progress(params["delta"].as_str()),
                    &mut events,
                )?;
            } else if matches!(method, "item/started" | "item/completed") {
                for (field, summary) in [("summary", true), ("content", false)] {
                    for (index, text) in params["item"][field]
                        .as_array()
                        .into_iter()
                        .flatten()
                        .enumerate()
                    {
                        let index = u64::try_from(index).map_err(|_| protocol_error())?;
                        self.message(
                            &address,
                            native.clone(),
                            if summary {
                                ItemPart::Summary(index)
                            } else {
                                ItemPart::Reasoning(index)
                            },
                            ItemStart::Reasoning(ReasoningStart {
                                text: String::new(),
                                kind: if summary {
                                    ReasoningKind::Summary
                                } else {
                                    ReasoningKind::Full
                                },
                            }),
                            if method == "item/completed" {
                                NativeTextObservation::Completed(
                                    text.as_str().or_else(|| text["text"].as_str()),
                                )
                            } else {
                                NativeTextObservation::Progress(
                                    text.as_str().or_else(|| text["text"].as_str()),
                                )
                            },
                            &mut events,
                        )?;
                    }
                }
            }
        } else if method == "item/commandExecution/terminalInteraction" {
            let native =
                pointer_string(params, &["/itemId", "/item_id"]).ok_or_else(protocol_error)?;
            let process = pointer_string(params, &["/processId", "/process_id"])
                .filter(|id| !id.is_empty())
                .ok_or_else(protocol_error)?;
            let stdin = params
                .get("stdin")
                .and_then(Value::as_str)
                .ok_or_else(protocol_error)?;
            let item = self
                .items
                .get_mut(&(address.turn_id.clone(), native, ItemPart::Tool))
                .ok_or_else(protocol_error)?;
            if item.completion_synthesized() && matches!(item.start, ItemStart::CommandExecution(_))
            {
                return Ok(events);
            }
            if !matches!(item.start, ItemStart::CommandExecution(_)) || item.completed.is_some() {
                return Err(protocol_error().into());
            }
            if let NativeItemLifetime::UnifiedExec { process_id } = &item.lifetime
                && process_id != &process
            {
                return Err(protocol_error().into());
            }
            // In pinned process_manager::write_stdin, empty stdin emits TerminalInteraction
            // only while its response retains process_id; nonempty stdin also emits on exit.
            if stdin.is_empty() && item.lifetime != NativeItemLifetime::Background {
                item.lifetime = NativeItemLifetime::Background;
                events.push(item_event(
                    &address,
                    &item.id,
                    ItemEvent::Tool {
                        event: forge_provider_api::tool::ToolItemEvent::Backgrounded,
                    },
                ));
            }
        } else if is_tool_lifecycle_event(&method.to_ascii_lowercase(), params) {
            let snapshot =
                normalize_tool_activity(method, params, workspace)?.ok_or_else(protocol_error)?;
            let key = (address.turn_id.clone(), snapshot.id, ItemPart::Tool);
            let mut descriptor = super::tool_semantics::normalize_descriptor(
                forge_provider_api::tool::ToolDescriptor {
                    semantics: Default::default(),
                    name: snapshot.name,
                    title: snapshot.title,
                    arguments: forge_provider_api::tool::InspectableArguments::new(
                        snapshot.arguments,
                    ),
                },
                snapshot.kind,
            );
            let first = !self.items.contains_key(&key);
            if first && self.terminal.contains_key(&address.turn_id) {
                return Err(protocol_error().into());
            }
            let item = self.items.entry(key).or_insert_with(|| NativeItem {
                id: new_item_id(),
                start: match snapshot.kind {
                    NativeToolKind::Command => ItemStart::CommandExecution(descriptor.clone()),
                    NativeToolKind::FileChange => ItemStart::FileChange(descriptor.clone()),
                    NativeToolKind::ToolCall => ItemStart::ToolCall(descriptor.clone()),
                },
                text: String::new(),
                completed: None,
                lifetime: NativeItemLifetime::Foreground,
                descriptor: Some(descriptor.clone()),
                output: String::new(),
                changes: Default::default(),
            });
            if method == "item/started"
                && snapshot.kind == NativeToolKind::Command
                && snapshot.status == Some(ToolStatus::Running)
                && params.pointer("/item/source").and_then(Value::as_str)
                    == Some("unifiedExecStartup")
            {
                let process_id = params
                    .pointer("/item/processId")
                    .and_then(Value::as_str)
                    .filter(|id| !id.is_empty())
                    .ok_or_else(protocol_error)?;
                match &item.lifetime {
                    NativeItemLifetime::UnifiedExec {
                        process_id: retained,
                    } if retained != process_id => return Err(protocol_error().into()),
                    NativeItemLifetime::Foreground => {
                        item.lifetime = NativeItemLifetime::UnifiedExec {
                            process_id: process_id.to_owned(),
                        }
                    }
                    NativeItemLifetime::UnifiedExec { .. }
                    | NativeItemLifetime::Background
                    | NativeItemLifetime::NativeExited
                    | NativeItemLifetime::UnobservableProcess => {}
                }
            }
            if method == "item/started"
                && snapshot.kind == NativeToolKind::Command
                && params
                    .pointer("/item/processId")
                    .and_then(Value::as_str)
                    .is_some_and(|id| !id.is_empty())
                && params
                    .pointer("/item/source")
                    .and_then(Value::as_str)
                    .is_none_or(|source| {
                        !matches!(
                            source,
                            "agent" | "userShell" | "unifiedExecStartup" | "unifiedExecInteraction"
                        )
                    })
            {
                item.lifetime = NativeItemLifetime::UnobservableProcess;
            }
            if let Some(previous) = &item.descriptor {
                if descriptor.name == "tool" {
                    descriptor.name = previous.name.clone();
                }
                if matches!(
                    descriptor.title.as_str(),
                    "tool" | "command" | "file changes"
                ) {
                    descriptor.title = previous.title.clone();
                }
                let envelope = params.get("update").unwrap_or(params);
                let supplied = envelope.get("item").unwrap_or(envelope);
                if !["arguments", "command", "cwd"]
                    .iter()
                    .any(|field| supplied.get(field).is_some() || envelope.get(field).is_some())
                {
                    descriptor.arguments = previous.arguments.clone();
                    descriptor.semantics = previous.semantics.clone();
                }
            }
            let same_kind = matches!(
                (&item.start, snapshot.kind),
                (ItemStart::CommandExecution(_), NativeToolKind::Command)
                    | (ItemStart::FileChange(_), NativeToolKind::FileChange)
                    | (ItemStart::ToolCall(_), NativeToolKind::ToolCall)
            );
            if !same_kind {
                return Err(protocol_error().into());
            }
            if item.completion_synthesized() {
                // Codex keeps a process alive after `turn/interrupt`, so its output and
                // final status can arrive after Forge closed the item as interrupted.
                // The turn is settled; this late activity belongs to no observer.
                return Ok(events);
            }
            if item.completed.is_some() {
                if method != "item/completed" {
                    return Err(protocol_error().into());
                }
                let outcome = match snapshot.status {
                    Some(ToolStatus::Succeeded) => ToolOutcome::Succeeded,
                    Some(ToolStatus::Cancelled) => ToolOutcome::Cancelled,
                    Some(ToolStatus::Interrupted) => ToolOutcome::Interrupted,
                    Some(ToolStatus::Failed) => ToolOutcome::Failed {
                        diagnostic: ToolDiagnostic {
                            kind: ProviderFailureKind::Unknown,
                            message: SanitizedMessage::new("Provider tool failed")
                                .expect("static text"),
                        },
                    },
                    Some(ToolStatus::Denied) => ToolOutcome::Denied {
                        reason: SanitizedMessage::new("Provider denied this tool call")
                            .expect("static text"),
                    },
                    Some(ToolStatus::Running) | None => return Err(protocol_error().into()),
                };
                let signature = serde_json::to_string(&(
                    &outcome,
                    &descriptor,
                    &snapshot.output,
                    &snapshot.change,
                ))?;
                if item.completed.as_ref() == Some(&signature) {
                    return Ok(events);
                }
                return Err(protocol_error().into());
            }
            if first {
                events.push(item_event(
                    &address,
                    &item.id,
                    ItemEvent::Started(item.start.clone()),
                ));
            } else if item.descriptor.as_ref() != Some(&descriptor) {
                item.descriptor = Some(descriptor.clone());
                events.push(item_event(
                    &address,
                    &item.id,
                    ItemEvent::Tool {
                        event: forge_provider_api::tool::ToolItemEvent::DescriptorUpdated(
                            descriptor.clone(),
                        ),
                    },
                ));
            }
            self.replay_sensitive = true;
            if let Some(status) = snapshot.status
                && status == ToolStatus::Running
                && method == "item/started"
            {
                events.push(item_event(
                    &address,
                    &item.id,
                    ItemEvent::Tool {
                        event: forge_provider_api::tool::ToolItemEvent::ExecutionStarted,
                    },
                ));
            }
            if let Some(text) = &snapshot.output {
                if snapshot.output_delta {
                    item.output.push_str(text);
                } else {
                    item.output = text.clone();
                }
                events.push(item_event(
                    &address,
                    &item.id,
                    ItemEvent::Tool {
                        event: forge_provider_api::tool::ToolItemEvent::Output(
                            if snapshot.output_delta {
                                TextUpdate::Append(text.clone())
                            } else {
                                TextUpdate::Replace(text.clone())
                            },
                        ),
                    },
                ));
            }
            if !snapshot.change.is_empty() {
                item.changes = snapshot.change.clone();
                events.push(item_event(
                    &address,
                    &item.id,
                    ItemEvent::Tool {
                        event: forge_provider_api::tool::ToolItemEvent::Changes(
                            snapshot.change.clone(),
                        ),
                    },
                ));
            }
            if let Some(status) = snapshot.status {
                let outcome = match status {
                    ToolStatus::Running => None,
                    ToolStatus::Succeeded => Some(ToolOutcome::Succeeded),
                    ToolStatus::Cancelled => Some(ToolOutcome::Cancelled),
                    ToolStatus::Interrupted => Some(ToolOutcome::Interrupted),
                    ToolStatus::Failed => Some(ToolOutcome::Failed {
                        diagnostic: ToolDiagnostic {
                            kind: ProviderFailureKind::Unknown,
                            message: SanitizedMessage::new("Provider tool failed")
                                .expect("static text"),
                        },
                    }),
                    ToolStatus::Denied => Some(ToolOutcome::Denied {
                        reason: SanitizedMessage::new("Provider denied this tool call")
                            .expect("static text"),
                    }),
                };
                if let Some(outcome) = outcome {
                    let signature = serde_json::to_string(&(
                        &outcome,
                        &descriptor,
                        &snapshot.output,
                        &snapshot.change,
                    ))?;
                    if item.completed.as_ref().is_some_and(|old| old != &signature) {
                        return Err(protocol_error().into());
                    }
                    if item.completed.is_none() {
                        let event =
                            finish_tool(&address, item, outcome, Some(descriptor), snapshot.output);
                        item.completed = Some(signature);
                        events.push(event);
                    }
                }
            }
        } else if let Some(update) = normalize_checklist_update(method, params) {
            self.replay_sensitive = true;
            events.push(AdapterActivity::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event: TurnMetadataEvent::ChecklistUpdated(update),
            });
        }
        for native in normalize_agent_lifecycle(method, params).map_err(|_| protocol_error())? {
            let Some(parent) = native
                .parent_thread_id
                .as_ref()
                .and_then(|parent| self.threads.get(parent))
                .cloned()
            else {
                return Err(protocol_error().into());
            };
            let pending_key = (
                address.turn_id.clone(),
                pointer_string(params, &["/itemId", "/item_id", "/item/id"])
                    .ok_or_else(protocol_error)?,
            );
            let parent_agent_id = native
                .parent_thread_id
                .as_ref()
                .and_then(|thread| self.agents.get(thread))
                .cloned();
            let delegation = AgentDelegation {
                parent_thread_id: address.thread_id.clone(),
                parent_turn_id: address.turn_id.clone(),
                parent_item_id: self
                    .items
                    .get(&(
                        address.turn_id.clone(),
                        pending_key.1.clone(),
                        ItemPart::Tool,
                    ))
                    .map(|item| item.id.clone()),
            };
            let Some(thread) = native.provider_thread_id else {
                if !native.starts_child {
                    return Err(protocol_error().into());
                }
                if let std::collections::hash_map::Entry::Vacant(e) =
                    self.pending_agents.entry(pending_key)
                {
                    let agent_id =
                        forge_provider_api::AgentId::new(uuid::Uuid::new_v4().to_string())
                            .expect("UUID is nonempty");
                    e.insert(agent_id.clone());
                    events.push(AdapterActivity::Agent {
                        agent_id,
                        event: AgentEvent::Lifecycle {
                            event: AgentLifecycleEvent::Started {
                                agent: AgentDescription {
                                    parent_agent_id,
                                    delegation: Some(delegation),
                                    thread_id: None,
                                    definition: None,
                                    nickname: None,
                                    task: native.task,
                                },
                                state: AgentActiveState::Starting,
                            },
                        },
                    });
                }
                continue;
            };
            let pending = self.pending_agents.remove(&pending_key);
            let first = !self.agents.contains_key(&thread);
            let first_lifecycle = first && pending.is_none();
            if first && !native.starts_child {
                return Err(protocol_error().into());
            }
            let agent_id = self
                .agents
                .entry(thread.clone())
                .or_insert_with(|| {
                    pending.unwrap_or_else(|| {
                        forge_provider_api::AgentId::new(uuid::Uuid::new_v4().to_string())
                            .expect("UUID is nonempty")
                    })
                })
                .clone();
            let child_owner = parent.child(&agent_id).map_err(|_| protocol_error())?;
            self.admit_thread(&thread, &child_owner)?;
            let pending_child = self.pending_child.remove(&thread).unwrap_or_default();
            let delegation = self
                .delegations
                .entry(thread.clone())
                .or_insert(delegation)
                .clone();
            let description = AgentDescription {
                parent_agent_id,
                delegation: Some(delegation),
                thread_id: Some(child_owner.clone()),
                definition: None,
                nickname: None,
                task: native.task,
            };
            let lifecycle = match native.status {
                NativeAgentStatus::Active(state) if first_lifecycle => {
                    AgentLifecycleEvent::Started {
                        agent: description,
                        state,
                    }
                }
                NativeAgentStatus::Active(state) => AgentLifecycleEvent::Updated {
                    agent: description,
                    state,
                },
                NativeAgentStatus::Finished(outcome) => AgentLifecycleEvent::Finished {
                    agent: description,
                    outcome,
                },
            };
            events.push(AdapterActivity::Agent {
                agent_id: agent_id.clone(),
                event: AgentEvent::Lifecycle { event: lifecycle },
            });
            if first {
                events.push(AdapterActivity::Agent {
                    agent_id,
                    event: AgentEvent::ConversationResolved {
                        thread_id: child_owner.clone(),
                    },
                });
            }
            for pending in pending_child {
                events.extend(self.activities(&pending, workspace)?);
            }
        }
        Ok(events)
    }

    /// Admit immutable text types and reject conflicting final snapshots or post-completion deltas.
    fn message(
        &mut self,
        address: &TurnAddress,
        native: String,
        part: ItemPart,
        start: ItemStart,
        observation: NativeTextObservation<'_>,
        events: &mut Vec<AdapterActivity>,
    ) -> Result<()> {
        let (text, completed) = match observation {
            NativeTextObservation::Progress(text) => (text, false),
            NativeTextObservation::Completed(text) => (text, true),
        };
        let key = (address.turn_id.clone(), native, part);
        let first = !self.items.contains_key(&key);
        if first && self.terminal.contains_key(&address.turn_id) {
            return Err(protocol_error().into());
        }
        let item = self.items.entry(key).or_insert_with(|| NativeItem {
            id: new_item_id(),
            start: start.clone(),
            text: String::new(),
            completed: None,
            lifetime: NativeItemLifetime::Foreground,
            descriptor: None,
            output: String::new(),
            changes: Default::default(),
        });
        let same_kind = matches!(
            (&item.start, &start),
            (ItemStart::UserMessage(_), ItemStart::UserMessage(_))
                | (ItemStart::AgentMessage(_), ItemStart::AgentMessage(_))
                | (ItemStart::Reasoning(_), ItemStart::Reasoning(_))
        );
        if !same_kind {
            return Err(protocol_error().into());
        }
        if let (ItemStart::AgentMessage(old), ItemStart::AgentMessage(incoming)) =
            (&item.start, &start)
            && incoming.classification != MessageClassification::Unclassified
            && incoming.classification != old.classification
            && (old.classification != MessageClassification::Unclassified
                || item.completed.is_some())
        {
            return Err(protocol_error().into());
        }
        if item.completed.is_some() {
            if completed && text.is_some_and(|text| text == item.text) {
                return Ok(());
            }
            return Err(protocol_error().into());
        }
        if first {
            events.push(item_event(
                address,
                &item.id,
                ItemEvent::Started(start.clone()),
            ));
        }
        if let (ItemStart::AgentMessage(old), ItemStart::AgentMessage(incoming)) =
            (&mut item.start, start)
            && incoming.classification != MessageClassification::Unclassified
            && incoming.classification != old.classification
        {
            if old.classification != MessageClassification::Unclassified {
                return Err(protocol_error().into());
            }
            old.classification = incoming.classification;
            events.push(item_event(
                address,
                &item.id,
                ItemEvent::Message {
                    event: MessageItemEvent::Classified(incoming.classification),
                },
            ));
        }
        if completed {
            if let Some(text) = text {
                item.text = text.to_owned();
            }
            events.push(finish_message(address, item, MessageOutcome::Completed));
        } else if let Some(text) = text {
            item.text.push_str(text);
            events.push(item_event(
                address,
                &item.id,
                ItemEvent::Message {
                    event: MessageItemEvent::Delta(text.to_owned()),
                },
            ));
        }
        Ok(())
    }
}

/// Distinguish a native text delta from an authoritative final snapshot.
enum NativeTextObservation<'a> {
    Progress(Option<&'a str>),
    Completed(Option<&'a str>),
}

/// Distinguish native unified-exec yield evidence from exit and unsupported output shapes.
enum NativeExecReply<'a> {
    Yielded { process_id: &'a str },
    Exited,
    Unknown,
}

/// Parse only the pinned native generated header, never command output after its Output delimiter.
fn native_exec_reply(output: &str) -> Result<NativeExecReply<'_>> {
    let Some((header, _)) = output.split_once("\nOutput:\n") else {
        return Ok(NativeExecReply::Unknown);
    };
    let mut lines = header.lines().peekable();
    if lines
        .peek()
        .is_some_and(|line| line.starts_with("Chunk ID: "))
    {
        lines.next();
    }
    let Some(wall_time) = lines
        .next()
        .and_then(|line| line.strip_prefix("Wall time: "))
        .and_then(|value| value.strip_suffix(" seconds"))
    else {
        return Ok(NativeExecReply::Unknown);
    };
    if !wall_time
        .parse::<f64>()
        .is_ok_and(|value| value.is_finite() && value >= 0.0)
    {
        return Ok(NativeExecReply::Unknown);
    }
    let mut process = None;
    let mut exited = false;
    for line in lines {
        if let Some(id) = line.strip_prefix("Process running with session ID ") {
            if id.is_empty()
                || !id.bytes().all(|byte| byte.is_ascii_digit())
                || process.replace(id).is_some()
            {
                return Err(protocol_error().into());
            }
        } else if let Some(code) = line.strip_prefix("Process exited with code ") {
            if exited || code.parse::<i32>().is_err() {
                return Err(protocol_error().into());
            }
            exited = true;
        } else if let Some(count) = line.strip_prefix("Original token count: ") {
            if count.parse::<u64>().is_err() {
                return Err(protocol_error().into());
            }
        } else {
            return Ok(NativeExecReply::Unknown);
        }
    }
    Ok(match process {
        Some(process_id) => NativeExecReply::Yielded { process_id },
        None if exited => NativeExecReply::Exited,
        None => NativeExecReply::Unknown,
    })
}

/// Allocate an opaque host item identity independently of native strings and derived reasoning keys.
fn new_item_id() -> ItemId {
    ItemId::new(uuid::Uuid::new_v4().to_string()).expect("UUID is nonempty")
}

/// Address one typed update at its immutable host item owner.
fn item_event(address: &TurnAddress, item_id: &ItemId, event: ItemEvent) -> AdapterActivity {
    AdapterActivity::Item {
        thread_id: address.thread_id.clone(),
        turn_id: address.turn_id.clone(),
        item_id: item_id.clone(),
        event,
    }
}

/// Freeze partial or final text while retaining the admitted message representation.
fn finish_message(
    address: &TurnAddress,
    item: &mut NativeItem,
    outcome: MessageOutcome,
) -> AdapterActivity {
    let completion = MessageCompletion {
        text: item.text.clone(),
        outcome,
    };
    item.completed = Some(item.text.clone());
    let completion = match item.start {
        ItemStart::UserMessage(_) => ItemCompletion::UserMessage(completion),
        ItemStart::AgentMessage(_) => ItemCompletion::AgentMessage(completion),
        ItemStart::Reasoning(_) => ItemCompletion::Reasoning(completion),
        ItemStart::CommandExecution(_) | ItemStart::FileChange(_) | ItemStart::ToolCall(_) => {
            unreachable!("text completion only applies to message items")
        }
    };
    item_event(address, &item.id, ItemEvent::Completed(completion))
}

/// Freeze actual native tool outcome and all independently observed final snapshots.
fn finish_tool(
    address: &TurnAddress,
    item: &mut NativeItem,
    outcome: ToolOutcome,
    descriptor: Option<forge_provider_api::tool::ToolDescriptor>,
    output: Option<String>,
) -> AdapterActivity {
    let completion = forge_provider_api::tool::ToolCompletion {
        outcome,
        descriptor,
        output,
        changes: item.changes.clone(),
    };
    item.completed = Some(String::new());
    let completion = match item.start {
        ItemStart::CommandExecution(_) => ItemCompletion::CommandExecution(completion),
        ItemStart::FileChange(_) => ItemCompletion::FileChange(completion),
        ItemStart::ToolCall(_) => ItemCompletion::ToolCall(completion),
        ItemStart::UserMessage(_) | ItemStart::AgentMessage(_) | ItemStart::Reasoning(_) => {
            unreachable!("tool completion only applies to tool items")
        }
    };
    item_event(address, &item.id, ItemEvent::Completed(completion))
}

/// Capture the native observation time before cleanup or delayed publication.
fn timestamp() -> i64 {
    let elapsed = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    i64::try_from(elapsed.as_millis()).unwrap_or(i64::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Trust only generated header lifetime evidence for the exact original unified-exec call and process.
    #[test]
    fn raw_yield_evidence_checks_owner_process_and_ignores_stdout_spoofing() -> Result<()> {
        for (reported_process, stdout_spoof, expected) in [
            ("42", false, true),
            ("99", false, false),
            ("42", true, false),
        ] {
            let scope =
                InvocationScope::new(forge_provider_api::testing::root_thread("session"), "one");
            let mut normalization = CodexNormalization::default();
            let address = normalization.admit_turn("native", "turn", &scope)?;
            normalization.activities(&json!({"method":"item/started","params":{"threadId":"native","turnId":"turn","item":{"type":"commandExecution","id":"command","command":"server","status":"inProgress","source":"unifiedExecStartup","processId":"42"}}}), "")?;
            let output = if stdout_spoof {
                "Wall time: 0.0100 seconds\nProcess exited with code 0\nOutput:\nProcess running with session ID 42".to_owned()
            } else {
                format!(
                    "Chunk ID: native-chunk\nWall time: 0.0100 seconds\nProcess running with session ID {reported_process}\nOriginal token count: 2\nOutput:\nready"
                )
            };
            let result = normalization.activities(&json!({"method":"rawResponseItem/completed","params":{"threadId":"native","turnId":"turn","item":{"type":"function_call_output","call_id":"command","output":output}}}), "");
            assert_eq!(normalization.has_pending_background_tools(), expected);
            assert_eq!(result.is_ok(), reported_process == "42");
            normalization.activities(&json!({"method":"turn/completed","params":{"threadId":"native","turn":{"id":"turn","status":"interrupted"}}}), "")?;
            assert_eq!(
                normalization.has_unresolved_sessions(&address),
                !expected && !stdout_spoof
            );
        }
        Ok(())
    }
}
