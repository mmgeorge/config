use std::collections::{HashMap, HashSet};

use github_copilot_sdk::SessionEvent;
use serde_json::Value;

use forge_provider_api::progress::TurnProgress;
use forge_provider_api::turn::TurnOutcome;
use forge_provider_api::{
    AdapterActivity, AdapterActivityWriter, AgentMessageRole, AgentMessageStart,
    InspectableArguments, InvocationScope, ItemCompletion, ItemEvent, ItemId, ItemStart,
    MessageClassification, MessageCompletion, MessageItemEvent, MessageOutcome, ProviderError,
    ProviderFailureKind, ReasoningKind, ReasoningStart, SanitizedMessage, TextUpdate, ThreadEvent,
    ToolCompletion, ToolDescriptor, ToolDiagnostic, ToolInputUpdate, ToolItemEvent, ToolOutcome,
    TurnAddress, TurnMetadataEvent,
};

use super::message_state::{CopilotMessageStore, CopilotPhase, CopilotPhaseMetadata};
use super::tool_semantics::{NativeToolDescription, NativeToolKind};

#[derive(Clone)]
struct ItemAddress {
    turn: TurnAddress,
    item_id: ItemId,
    reasoning: bool,
}

impl ItemAddress {
    /// Address activity to the item originally admitted by this decoder.
    fn activity(&self, event: ItemEvent) -> AdapterActivity {
        AdapterActivity::Item {
            thread_id: self.turn.thread_id.clone(),
            turn_id: self.turn.turn_id.clone(),
            item_id: self.item_id.clone(),
            event,
        }
    }
    /// Freeze a message snapshot while preserving its immutable reasoning or prose type.
    fn completion(&self, text: String, outcome: MessageOutcome) -> AdapterActivity {
        let completion = MessageCompletion { text, outcome };
        self.activity(ItemEvent::Completed(if self.reasoning {
            ItemCompletion::Reasoning(completion)
        } else {
            ItemCompletion::AgentMessage(completion)
        }))
    }
}

struct NativeTurn {
    address: TurnAddress,
    native_id: String,
    completed_at_ms: Option<i64>,
    settled: bool,
    model: Option<String>,
    writer: AdapterActivityWriter,
    child: Option<super::child_turn::CopilotChildTurn>,
}

struct NativeTool {
    address: ItemAddress,
    invocation: NativeToolDescription,
    started: bool,
    finished: bool,
    output_sequence: Option<u64>,
    output_chunks: HashMap<u64, String>,
    final_native: Option<Value>,
    has_changes: bool,
}

pub struct CopilotEventDecoder {
    scope: InvocationScope,
    workspace: String,
    root_thread: Option<String>,
    turn: HashMap<String, NativeTurn>,
    message: CopilotMessageStore,
    reasoning: CopilotMessageStore,
    message_address: HashMap<(String, String), ItemAddress>,
    reasoning_address: HashMap<(String, String), ItemAddress>,
    tool: HashMap<(String, String), NativeTool>,
    pending_input: HashMap<(String, String), String>,
    compaction: HashMap<String, forge_provider_api::Compaction>,
    observed: HashMap<String, Value>,
    pending_checklist: Option<forge_provider_api::ProviderChecklistUpdate>,
    settlement: Option<TurnOutcome>,
    pending_model: HashMap<String, String>,
    children: HashMap<String, forge_provider_api::InvocationScope>,
    child_events: HashMap<String, forge_provider_api::InvocationScope>,
    /// Observed agent identity of each child thread this decoder minted.
    child_agents: HashMap<forge_provider_api::ThreadId, forge_provider_api::agent::AgentId>,
    callbacks: Option<std::sync::Arc<dyn super::provider::NativeCallbackRouter>>,
    turn_owner: Option<forge_provider_api::ProviderEventSink>,
    unsupported_events: HashSet<String>,
}

impl CopilotEventDecoder {
    pub fn new(scope: InvocationScope, workspace: String) -> Self {
        Self {
            scope,
            workspace,
            root_thread: None,
            turn: HashMap::new(),
            message: Default::default(),
            reasoning: Default::default(),
            message_address: HashMap::new(),
            reasoning_address: HashMap::new(),
            tool: HashMap::new(),
            pending_input: HashMap::new(),
            compaction: HashMap::new(),
            observed: HashMap::new(),
            pending_checklist: None,
            settlement: None,
            pending_model: HashMap::new(),
            callbacks: None,
            turn_owner: None,
            children: HashMap::new(),
            child_events: HashMap::new(),
            child_agents: HashMap::new(),
            unsupported_events: HashSet::new(),
        }
    }

    /// Keep exact callback routing alive with the retained decoder after the foreground finishes.
    pub(crate) fn with_callbacks(
        mut self,
        callbacks: std::sync::Arc<dyn super::provider::NativeCallbackRouter>,
    ) -> Self {
        self.callbacks = Some(callbacks);
        self
    }

    /// Collect original child observations when this binding loses or closes its native reader.
    pub(crate) async fn close_children(&self) {
        for turn in self.turn.values() {
            if let Some(child) = &turn.child {
                child.close().await;
            }
        }
    }

    /// Retain the binding lifecycle authority for independently observed child response cycles.
    pub(crate) fn with_turn_owner(mut self, sink: forge_provider_api::ProviderEventSink) -> Self {
        self.turn_owner = Some(sink);
        self
    }

    /// Get exact native tool ownership only after its item and ancestry publications succeeded.
    pub(crate) fn callback_address(&self, event: &SessionEvent) -> Option<TurnAddress> {
        if !self.observed.contains_key(&event.id) {
            return None;
        }
        let call_id = event.data.get("toolCallId")?.as_str()?;
        let scope = self.child_events.get(&event.id).unwrap_or(&self.scope);
        self.tool
            .iter()
            .find(|((_, id), tool)| id == call_id && tool.address.turn.scope == *scope)
            .map(|(_, tool)| tool.address.turn.clone())
    }

    /// Match late events only against native identities already owned by this invocation.
    pub(crate) fn owns_event(&self, event: &SessionEvent) -> bool {
        if self.observed.contains_key(&event.id) {
            return true;
        }
        if event
            .agent_id
            .as_ref()
            .is_some_and(|child| self.children.contains_key(child))
        {
            return true;
        }
        if event
            .parent_id
            .as_ref()
            .is_some_and(|parent| self.child_events.contains_key(parent))
        {
            return true;
        }
        let Some(thread) = event.agent_id.as_ref().or(self.root_thread.as_ref()) else {
            return false;
        };
        if let Some(id) = event
            .data
            .get("messageId")
            .or_else(|| event.data.get("reasoningId"))
            .and_then(Value::as_str)
        {
            let key = (thread.clone(), id.to_owned());
            if self.message_address.contains_key(&key) || self.reasoning_address.contains_key(&key)
            {
                return true;
            }
        }
        if let Some(id) = event.data.get("toolCallId").and_then(Value::as_str)
            && self.tool.contains_key(&(thread.clone(), id.to_owned()))
        {
            return true;
        }
        // Native background wakeups may reuse turnId; it cannot distinguish old output here.
        false
    }

    pub fn root_address(&self) -> Option<TurnAddress> {
        self.root_thread
            .as_ref()
            .and_then(|thread| self.turn.get(thread))
            .map(|turn| turn.address.clone())
    }

    fn protocol(_message: impl Into<String>) -> ProviderError {
        ProviderError::Failure(
            forge_provider_api::error::ProviderFailure::new(
                forge_provider_api::error::ProviderOperation::TurnStart,
                forge_provider_api::error::ProviderFailureKind::Protocol,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
        )
    }

    fn thread(
        &self,
        event: &SessionEvent,
        output: &impl TurnProgress,
    ) -> Result<String, ProviderError> {
        if let Some(native_child) = &event.agent_id {
            if let Some(scope) = self.children.get(native_child)
                && let Some((thread, _)) = self
                    .turn
                    .iter()
                    .find(|(_, turn)| &turn.address.scope == scope)
            {
                return Ok(thread.clone());
            }
            return Ok(native_child.clone());
        }
        output
            .session_id()
            .map(|id| id.as_str().to_owned())
            .ok_or_else(|| Self::protocol("Copilot event has no conversation identity"))
    }

    fn address(&self, thread: &str) -> Result<TurnAddress, ProviderError> {
        self.turn
            .get(thread)
            .map(|turn| turn.address.clone())
            .ok_or_else(|| {
                Self::protocol(format!(
                    "Copilot event precedes turn admission for {thread}"
                ))
            })
    }

    /// Publish the point where Copilot consumed accepted steering input inside the running turn.
    ///
    /// Copilot can hold steering until the current model call ends, so the input's acknowledgement
    /// is not where the model read it. Only input consumed after the root turn started is
    /// published; the turn's own prompt precedes that start and is already the exchange prompt.
    pub async fn consumed_input(
        &mut self,
        event: &SessionEvent,
        submission_id: forge_provider_api::SubmissionId,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let Some(thread) = self.root_thread.clone() else {
            return Ok(());
        };
        if self.turn.get(&thread).is_none_or(|turn| turn.settled) {
            return Ok(());
        }
        let text = event
            .data
            .get("content")
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot user message omitted content"))?;
        let address = ItemAddress {
            turn: self.address(&thread)?,
            item_id: allocated_item_id(),
            reasoning: false,
        };
        sink.send_wait(
            address
                .clone()
                .activity(ItemEvent::Started(ItemStart::UserMessage(
                    forge_provider_api::UserMessageStart {
                        text: text.to_owned(),
                        submission_id: Some(submission_id),
                    },
                ))),
        )
        .await?;
        sink.send_wait(
            address.activity(ItemEvent::Completed(ItemCompletion::UserMessage(
                MessageCompletion {
                    text: text.into(),
                    outcome: MessageOutcome::Completed,
                },
            ))),
        )
        .await?;
        Ok(())
    }

    /// Warn once per binding about a native notification type this adapter does not apply.
    async fn unsupported(
        &mut self,
        event_type: &str,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        if !self.unsupported_events.insert(event_type.to_owned()) {
            return Ok(());
        }
        sink.send_wait(AdapterActivity::Warning(
            forge_provider_api::ProviderWarning {
                scope: forge_provider_api::WarningScope::Thread {
                    thread_id: sink.scope().thread_id().clone(),
                },
                kind: forge_provider_api::ProviderWarningKind::UnsupportedNativeEvent,
                message: SanitizedMessage::new(format!(
                    "Copilot reported `{event_type}`, which Forge does not apply"
                ))
                .map_err(|_| Self::protocol("unsupported native event has no type"))?,
            },
        ))
        .await?;
        Ok(())
    }

    /// Report a recoverable native limitation without changing turn availability.
    async fn diagnostic(
        &self,
        sink: &AdapterActivityWriter,
        kind: ProviderFailureKind,
    ) -> Result<(), ProviderError> {
        sink.send_wait(AdapterActivity::Warning(
            forge_provider_api::ProviderWarning {
                scope: forge_provider_api::WarningScope::Thread {
                    thread_id: sink.scope().thread_id().clone(),
                },
                kind: forge_provider_api::ProviderWarningKind::NativeNotice { category: kind },
                message: SanitizedMessage::new("Provider reported incomplete metadata")
                    .expect("static diagnostic is nonempty"),
            },
        ))
        .await?;
        Ok(())
    }

    /// Normalize a fixture observation at its local receipt boundary.
    #[cfg(test)]
    pub async fn decode(
        &mut self,
        event: &SessionEvent,
        output: &mut impl TurnProgress,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        self.decode_at(event, super::decode::timestamp_ms(), output, sink)
            .await
    }

    /// Normalize one native event with its original SDK receipt timestamp.
    pub async fn decode_at(
        &mut self,
        event: &SessionEvent,
        observed_at_ms: i64,
        output: &mut impl TurnProgress,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let snapshot = serde_json::to_value(event)
            .map_err(|_| Self::protocol("native event could not be retained"))?;
        if let Some(previous) = self.observed.get(&event.id) {
            if previous != &snapshot {
                return Err(Self::protocol(
                    "native event replay conflicts with retained content",
                ));
            }
            return Ok(());
        }
        if event.id.trim().is_empty() {
            return Err(Self::protocol("native event identity is empty"));
        }
        if event.event_type == "session.error" {
            let error_writer = if let Some(child) = &event.agent_id {
                let scope = self
                    .children
                    .get(child)
                    .ok_or_else(|| Self::protocol("session error refers to an unknown child"))?;
                sink.register_observed_turn(TurnAddress {
                    scope: scope.clone(),
                    thread_id: scope.thread_id.clone(),
                    turn_id: scope
                        .turn_id()
                        .map_err(|_| Self::protocol("invalid child turn"))?,
                })?
            } else {
                sink.clone()
            };
            error_writer
                .send_wait(super::error::session_error(
                    error_writer.scope().thread_id().clone(),
                    &event.data,
                ))
                .await?;
            self.observed.insert(event.id.clone(), snapshot);
            return Ok(());
        }
        if let Some(child) = &event.agent_id
            && !self.children.contains_key(child)
            && let Some(scope) = event
                .parent_id
                .as_ref()
                .and_then(|parent| self.child_events.get(parent))
                .cloned()
        {
            self.children.insert(child.clone(), scope);
        }
        let thread = self.thread(event, output)?;
        if matches!(
            event.event_type.as_str(),
            "assistant.turn_start"
                | "assistant.turn_end"
                | "assistant.turn_retry"
                | "assistant.usage"
        ) && let Some(model) = super::decode::string_field(&event.data, &["model"])
        {
            if model.trim().is_empty() {
                return Err(Self::protocol("Copilot confirmed an empty model"));
            }
            self.pending_model.insert(thread.clone(), model);
        }
        if event.agent_id.is_some()
            && !self.turn.contains_key(&thread)
            && matches!(
                event.event_type.as_str(),
                "assistant.turn_start"
                    | "assistant.message_start"
                    | "assistant.message_delta"
                    | "assistant.message"
                    | "assistant.reasoning_delta"
                    | "assistant.reasoning"
                    | "assistant.usage"
                    | "tool.execution_start"
            )
        {
            let scope = self
                .children
                .get(&thread)
                .ok_or_else(|| Self::protocol("child content has no observed delegation"))?
                .clone();
            let address = TurnAddress {
                thread_id: scope.thread_id.clone(),
                turn_id: scope
                    .turn_id()
                    .map_err(|_| Self::protocol("invalid child turn"))?,
                scope,
            };
            let writer = sink.register_observed_turn(address.clone())?;
            let resources = match &self.callbacks {
                Some(callbacks) => callbacks.native_child_resources(&address).await?,
                None => super::child_turn::ChildResources::new(),
            };
            let owner = self.turn_owner.clone().ok_or_else(|| {
                Self::protocol("child processing has no retained lifecycle owner")
            })?;
            let child = super::child_turn::CopilotChildTurn::new(
                owner,
                address.clone(),
                resources,
                self.callbacks.clone(),
            )?;
            child.observe(observed_at_ms).await?;
            let native_id = super::decode::string_field(&event.data, &["turnId"])
                .unwrap_or_else(|| event.id.clone());
            self.turn.insert(
                thread.clone(),
                NativeTurn {
                    address,
                    native_id,
                    completed_at_ms: None,
                    settled: false,
                    model: None,
                    writer,
                    child: Some(child),
                },
            );
        }
        let child_writer = self.turn.get(&thread).map(|turn| turn.writer.clone());
        let sink = child_writer.as_ref().unwrap_or(sink);
        match event.event_type.as_str() {
            "assistant.turn_start" => {
                let native_id = super::decode::string_field(&event.data, &["turnId"])
                    .ok_or_else(|| Self::protocol("Copilot turn start omitted turnId"))?;
                if let Some(turn) = self.turn.get_mut(&thread) {
                    if turn.settled {
                        return Err(Self::protocol(
                            "Copilot started unowned work after collection",
                        ));
                    }
                    turn.native_id = native_id;
                    turn.completed_at_ms = None;
                } else {
                    let address = if event.agent_id.is_none() {
                        self.root_thread = Some(thread.clone());
                        TurnAddress {
                            scope: self.scope.clone(),
                            thread_id: self.scope.thread_id.clone(),
                            turn_id: self.scope.turn_id().map_err(|_| {
                                Self::protocol("Copilot host turn identity is invalid")
                            })?,
                        }
                    } else {
                        let child = self.children.get(&thread).ok_or_else(|| {
                            Self::protocol("Copilot child content has no observed delegation")
                        })?;
                        TurnAddress {
                            scope: child.clone(),
                            thread_id: child.thread_id.clone(),
                            turn_id: child
                                .turn_id()
                                .map_err(|_| Self::protocol("invalid child turn"))?,
                        }
                    };
                    let writer = if event.agent_id.is_none() {
                        sink.clone()
                    } else {
                        sink.register_observed_turn(address.clone())?
                    };
                    self.turn.insert(
                        thread.clone(),
                        NativeTurn {
                            address,
                            native_id,
                            completed_at_ms: None,
                            settled: false,
                            model: None,
                            writer,
                            child: None,
                        },
                    );
                }
                if event.agent_id.is_none()
                    && let Some(update) = self.pending_checklist.take()
                {
                    sink.send_wait(turn_activity(
                        self.address(&thread)?,
                        TurnMetadataEvent::ChecklistUpdated(update),
                    ))
                    .await?;
                }
            }
            "assistant.turn_end" => {
                if event.id.trim().is_empty() {
                    return Err(Self::protocol(
                        "Copilot model-call end omitted event identity",
                    ));
                }
                let report_id = forge_provider_api::identity::UsageReportId::new(format!(
                    "copilot:call-end:{}",
                    event.id
                ))
                .map_err(|_| {
                    Self::protocol("Copilot model-call end has an invalid report identity")
                })?;
                let model_call_id = match event.data.get("apiCallId") {
                    None | Some(Value::Null) => None,
                    Some(Value::String(id)) => Some(
                        forge_provider_api::identity::ModelCallId::new(id.clone())
                            .map_err(|_| Self::protocol("Copilot model-call identity is blank"))?,
                    ),
                    Some(
                        Value::Bool(_) | Value::Number(_) | Value::Array(_) | Value::Object(_),
                    ) => return Err(Self::protocol("Copilot model-call identity is malformed")),
                };
                let native_id = event.data.get("turnId").and_then(Value::as_str);
                let turn = self
                    .turn
                    .get_mut(&thread)
                    .ok_or_else(|| Self::protocol("Copilot turn end precedes turn start"))?;
                if native_id.is_some_and(|id| id != turn.native_id) {
                    return Err(Self::protocol(
                        "Copilot turn end changed native turn identity",
                    ));
                }
                let completed_at_ms = *turn.completed_at_ms.get_or_insert(observed_at_ms);
                let report = forge_provider_api::TurnUsageReport::interval(
                    report_id,
                    model_call_id,
                    observed_at_ms,
                    None,
                    Some(completed_at_ms),
                )
                .map_err(|_| {
                    Self::protocol("Copilot model-call end omitted its timing observation")
                })?;
                sink.send_wait(turn_activity(
                    turn.address.clone(),
                    TurnMetadataEvent::Usage(report),
                ))
                .await?;
            }
            "assistant.message_start" | "assistant.message_delta" | "assistant.message" => {
                self.message(event, &thread, sink).await?;
                if event.event_type == "assistant.message"
                    && let Some(requests) = event.data.get("toolRequests").and_then(Value::as_array)
                {
                    for request in requests {
                        self.request_tool(&thread, request, sink).await?;
                    }
                }
            }
            "assistant.reasoning_delta" | "assistant.reasoning" => {
                self.reasoning(event, &thread, sink).await?;
            }
            "assistant.tool_call_delta" => {
                let id = event
                    .data
                    .get("toolCallId")
                    .and_then(Value::as_str)
                    .ok_or_else(|| Self::protocol("Copilot tool input omitted toolCallId"))?;
                let text = event
                    .data
                    .get("inputDelta")
                    .and_then(Value::as_str)
                    .ok_or_else(|| Self::protocol("Copilot tool input omitted inputDelta"))?;
                if let Some(tool) = self.tool.get(&(thread.clone(), id.into())) {
                    sink.send_wait(tool.address.clone().activity(ItemEvent::Tool {
                        event: ToolItemEvent::Input(ToolInputUpdate::Text(TextUpdate::Append(
                            text.into(),
                        ))),
                    }))
                    .await?;
                } else {
                    self.pending_input
                        .entry((thread.clone(), id.into()))
                        .or_default()
                        .push_str(text);
                }
            }
            "tool.execution_start"
            | "tool.execution_partial_result"
            | "tool.execution_progress"
            | "tool.execution_complete"
            | "tool.shell_output" => {
                self.tool(event, &thread, output, sink).await?;
            }
            // Native progress and root idle are consumed by the retained prompt owner.
            "assistant.streaming_delta" | "assistant.idle" => {}
            "session.idle" => {}
            "session.usage_info" => {
                if let Some(usage) =
                    super::decode::unsigned_field(&event.data, &["currentTokens", "used"])
                        .zip(super::decode::unsigned_field(
                            &event.data,
                            &["tokenLimit", "size"],
                        ))
                        .and_then(|(used, size)| {
                            forge_provider_api::context::ContextUsage::reported(used, size)
                        })
                {
                    output.set_context_usage(usage.clone());
                    sink.send_wait(AdapterActivity::Thread {
                        thread_id: self.scope.thread_id.clone(),
                        event: ThreadEvent::ContextUsage(usage),
                    })
                    .await?;
                }
            }
            "assistant.usage" => {
                let report_id = super::decode::string_field(&event.data, &["apiCallId"])
                    .unwrap_or_else(|| event.id.clone());
                let report_id = forge_provider_api::identity::UsageReportId::new(report_id)
                    .map_err(|_| Self::protocol("Copilot usage report has no identity"))?;
                let usage = forge_provider_api::usage::TokenUsage {
                    input: super::decode::unsigned_field(&event.data, &["inputTokens"]),
                    cached_input: super::decode::unsigned_field(&event.data, &["cacheReadTokens"]),
                    reasoning: super::decode::unsigned_field(&event.data, &["reasoningTokens"]),
                    output: super::decode::unsigned_field(&event.data, &["outputTokens"]),
                };
                let model_call_id = super::decode::string_field(&event.data, &["apiCallId"])
                    .map(forge_provider_api::ModelCallId::new)
                    .transpose()
                    .map_err(|_| Self::protocol("invalid model-call identity"))?;
                let report = forge_provider_api::TurnUsageReport::call(
                    report_id,
                    model_call_id,
                    observed_at_ms,
                    Some(usage),
                    super::decode::unsigned_field(&event.data, &["duration"]),
                )
                .map_err(|_| Self::protocol("Copilot usage report contains no measurement"))?;
                sink.send_wait(turn_activity(
                    self.address(&thread)?,
                    TurnMetadataEvent::Usage(report),
                ))
                .await?;
            }
            "subagent.started" | "subagent.completed" | "subagent.failed" => {
                self.agent(event, sink).await?;
            }
            "session.compaction_start" | "session.compaction_complete" => {
                self.compaction(event, &thread, output, sink).await?;
            }
            other => match unmapped_disposition(other) {
                UnmappedDisposition::OwnedElsewhere | UnmappedDisposition::NoNormalizedMeaning => {}
                UnmappedDisposition::Unsupported => self.unsupported(other, sink).await?,
            },
        }
        if let Some(turn) = self.turn.get_mut(&thread)
            && let Some(model) = self.pending_model.remove(&thread)
            && turn.model.as_ref() != Some(&model)
        {
            let model_id = forge_provider_api::ModelId::new(model.clone())
                .map_err(|_| Self::protocol("invalid confirmed model"))?;
            turn.writer
                .send_wait(turn_activity(
                    turn.address.clone(),
                    TurnMetadataEvent::ModelResolved(forge_provider_api::ResolvedModel {
                        model_id,
                        observed_at_ms,
                    }),
                ))
                .await?;
            turn.model = Some(model);
        }
        if let Some(scope) = event
            .agent_id
            .as_ref()
            .and_then(|child| self.children.get(child))
        {
            self.child_events.insert(event.id.clone(), scope.clone());
        }
        self.observed.insert(event.id.clone(), snapshot);
        if let Some(callbacks) = &self.callbacks {
            if event.event_type == "subagent.started"
                && let Some(scope) = self.child_events.get(&event.id)
            {
                callbacks
                    .observe_native_child(
                        &self.scope,
                        TurnAddress {
                            thread_id: scope.thread_id.clone(),
                            turn_id: scope
                                .turn_id()
                                .map_err(|_| Self::protocol("invalid child turn"))?,
                            scope: scope.clone(),
                        },
                        false,
                    )
                    .await?;
            }
            if let Some(address) = self.callback_address(event)
                && let Some(call_id) = event.data.get("toolCallId").and_then(Value::as_str)
            {
                if event.event_type == "tool.execution_complete" {
                    callbacks
                        .release_owned_native_call(call_id, &address.scope)
                        .await;
                } else if address.scope == self.scope {
                    callbacks.bind_native_call(call_id, &self.scope).await?;
                } else {
                    callbacks
                        .bind_observed_native_call(call_id, &self.scope, address)
                        .await?;
                }
            }
        }
        if matches!(
            event.event_type.as_str(),
            "subagent.completed" | "subagent.failed"
        ) {
            self.finish_child(event, observed_at_ms).await?;
        }
        Ok(())
    }

    /// Seal one positively processing child from its own native terminal observation.
    async fn finish_child(
        &mut self,
        event: &SessionEvent,
        completed_at_ms: i64,
    ) -> Result<(), ProviderError> {
        let scope = self
            .child_events
            .get(&event.id)
            .cloned()
            .ok_or_else(|| Self::protocol("child terminal has no original ownership"))?;
        let thread = self
            .turn
            .iter()
            .find(|(_, turn)| turn.address.scope == scope)
            .map(|(thread, _)| thread.clone());
        let Some(thread) = thread else {
            if let Some(callbacks) = &self.callbacks {
                callbacks
                    .observe_native_child(
                        &self.scope,
                        TurnAddress {
                            thread_id: scope.thread_id.clone(),
                            turn_id: scope
                                .turn_id()
                                .map_err(|_| Self::protocol("invalid child turn"))?,
                            scope,
                        },
                        true,
                    )
                    .await?;
            }
            return Ok(());
        };
        let outcome = if event.event_type == "subagent.failed" {
            TurnOutcome::Failed {
                error: forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnStart,
                    super::error::session_failure_kind(&event.data),
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
                retry: forge_provider_api::turn::RetryDecision::NotAllowed,
            }
        } else if event.data.get("cancelled").and_then(Value::as_bool) == Some(true) {
            TurnOutcome::Interrupted
        } else {
            TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            }
        };
        let writer = self
            .turn
            .get(&thread)
            .expect("original child turn remains retained")
            .writer
            .clone();
        if matches!(outcome, TurnOutcome::Completed { .. }) {
            if self.message.has_unfinished_text(Some(&thread))
                || self.reasoning.has_unfinished_text(Some(&thread))
                || self
                    .tool
                    .values()
                    .any(|tool| !tool.finished && tool.address.turn.scope == scope)
            {
                return Err(Self::protocol(
                    "native child ended before foreground item endings",
                ));
            }
        } else {
            for (key, text) in self.message.interrupt_open(Some(&thread)) {
                let address = self
                    .message_address
                    .get(&key)
                    .expect("child message has original item ownership");
                writer
                    .send_wait(address.completion(text, MessageOutcome::Interrupted))
                    .await?;
            }
            for (key, text) in self.reasoning.interrupt_open(Some(&thread)) {
                let address = self
                    .reasoning_address
                    .get(&key)
                    .expect("child reasoning has original item ownership");
                writer
                    .send_wait(address.completion(text, MessageOutcome::Interrupted))
                    .await?;
            }
            for tool in self
                .tool
                .values_mut()
                .filter(|tool| !tool.finished && tool.address.turn.scope == scope)
            {
                writer
                    .send_wait(
                        tool.address
                            .activity(tool_completion(&tool.invocation, ToolOutcome::Interrupted)),
                    )
                    .await?;
                tool.finished = true;
            }
        }
        let turn = self
            .turn
            .get_mut(&thread)
            .expect("original child turn remains retained");
        turn.settled = true;
        turn.child
            .as_ref()
            .ok_or_else(|| Self::protocol("child terminal lacks native driver"))?
            .complete(outcome, completed_at_ms)
            .await
    }

    /// Register child ownership from explicit native delegation before admitting child content.
    async fn agent(
        &mut self,
        event: &SessionEvent,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let native_id = event
            .agent_id
            .clone()
            .or_else(|| super::decode::string_field(&event.data, &["agentId", "toolCallId"]))
            .ok_or_else(|| Self::protocol("child lifecycle omitted identity"))?;
        let parent_native =
            super::decode::string_field(&event.data, &["parentId", "parentAgentId"]);
        let parent_scope = match parent_native.as_ref() {
            Some(parent) => self
                .children
                .get(parent)
                .ok_or_else(|| Self::protocol("child refers to an unknown parent"))?
                .clone(),
            None => self.scope.clone(),
        };
        if event.event_type == "subagent.started" && !self.children.contains_key(&native_id) {
            let agent_id =
                forge_provider_api::agent::AgentId::new(uuid::Uuid::new_v4().to_string())
                    .map_err(|_| Self::protocol("invalid child identity"))?;
            let owner = self
                .scope
                .thread_id
                .child(&agent_id)
                .map_err(|_| Self::protocol("invalid observed child owner"))?;
            self.child_agents.insert(owner.clone(), agent_id);
            self.children.insert(
                native_id.clone(),
                InvocationScope::new(owner, self.scope.exchange_id.clone()),
            );
        }
        let child = self
            .children
            .get(&native_id)
            .ok_or_else(|| Self::protocol("child end precedes observed delegation"))?
            .clone();
        if let Some(call_id) = super::decode::string_field(&event.data, &["toolCallId"]) {
            if self
                .children
                .get(&call_id)
                .is_some_and(|known| known != &child)
            {
                return Err(Self::protocol(
                    "child delegation alias conflicts with an existing child",
                ));
            }
            self.children.insert(call_id, child.clone());
        }
        self.child_events.insert(event.id.clone(), child.clone());
        let agent_id = self
            .child_agents
            .get(&child.thread_id)
            .cloned()
            .ok_or_else(|| Self::protocol("invalid child identity"))?;
        let parent_agent_id = if parent_scope == self.scope {
            None
        } else {
            Some(
                self.child_agents
                    .get(&parent_scope.thread_id)
                    .cloned()
                    .ok_or_else(|| Self::protocol("invalid parent agent"))?,
            )
        };
        let agent = forge_provider_api::AgentDescription {
            parent_agent_id,
            delegation: Some(forge_provider_api::AgentDelegation {
                parent_thread_id: parent_scope.thread_id.clone(),
                parent_turn_id: parent_scope
                    .turn_id()
                    .map_err(|_| Self::protocol("invalid parent turn"))?,
                parent_item_id: None,
            }),
            thread_id: Some(child.thread_id.clone()),
            definition: super::decode::string_field(&event.data, &["agentName"]),
            nickname: super::decode::string_field(
                &event.data,
                &["agentDisplayName", "displayName"],
            ),
            task: super::decode::string_field(&event.data, &["task", "prompt", "agentDescription"]),
        };
        let lifecycle = match event.event_type.as_str() {
            "subagent.started" => forge_provider_api::AgentLifecycleEvent::Started {
                agent,
                state: forge_provider_api::AgentActiveState::Starting,
            },
            "subagent.completed" => forge_provider_api::AgentLifecycleEvent::Finished {
                agent,
                outcome: if event.data.get("cancelled").and_then(Value::as_bool) == Some(true) {
                    forge_provider_api::AgentOutcome::Interrupted
                } else {
                    forge_provider_api::AgentOutcome::Completed
                },
            },
            "subagent.failed" => forge_provider_api::AgentLifecycleEvent::Finished {
                agent,
                outcome: forge_provider_api::AgentOutcome::Failed(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        super::error::session_failure_kind(&event.data),
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Observation,
                        forge_provider_api::FailureDisposition::Uncertain,
                    ),
                ),
            },
            _ => return Err(Self::protocol("unexpected child lifecycle")),
        };
        sink.send_wait(AdapterActivity::Agent {
            agent_id,
            event: forge_provider_api::AgentEvent::Lifecycle { event: lifecycle },
        })
        .await?;
        Ok(())
    }

    async fn message(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let id = event
            .data
            .get("messageId")
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot message omitted messageId"))?;
        let phase = CopilotPhaseMetadata::parse(event.data.get("phase").and_then(Value::as_str));
        if let CopilotPhaseMetadata::Unknown(_) = &phase {
            self.diagnostic(sink, ProviderFailureKind::Protocol).await?;
        }
        let key = (thread.into(), id.into());
        if (self.settlement.is_some() && self.root_thread.as_deref() == Some(thread)
            || self.turn.get(thread).is_some_and(|turn| turn.settled))
            && !self.message_address.contains_key(&key)
        {
            return Err(Self::protocol(
                "new foreground message follows native completion",
            ));
        }
        let address = if let Some(address) = self.message_address.get(&key) {
            address.clone()
        } else {
            let address = ItemAddress {
                turn: self.address(thread)?,
                item_id: allocated_item_id(),
                reasoning: false,
            };
            self.message_address.insert(key, address.clone());
            address
        };
        let message = self
            .message
            .observe(thread, id, phase, event.agent_id.is_none())
            .map_err(|error| Self::protocol(error.to_string()))?;
        if event.event_type == "assistant.message_start" {
            return Ok(());
        }
        let text = event
            .data
            .get(if event.event_type == "assistant.message_delta" {
                "deltaContent"
            } else {
                "content"
            })
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot message omitted text content"))?;
        let classification = match message.phase {
            CopilotPhaseMetadata::Known(CopilotPhase::Commentary) => {
                MessageClassification::Known(AgentMessageRole::Commentary)
            }
            CopilotPhaseMetadata::Known(CopilotPhase::FinalAnswer) => {
                MessageClassification::Known(AgentMessageRole::Response)
            }
            _ => MessageClassification::Unclassified,
        };
        let was_completed = message.completed;
        if event.event_type == "assistant.message_delta" {
            CopilotMessageStore::append(message, text)
                .map_err(|error| Self::protocol(error.to_string()))?;
        } else {
            CopilotMessageStore::complete(message, text)
                .map_err(|error| Self::protocol(error.to_string()))?;
        }
        if was_completed {
            if message.published
                && let MessageClassification::Known(role) = classification
            {
                sink.send_wait(address.activity(ItemEvent::Message {
                    event: MessageItemEvent::Classified(MessageClassification::Known(role)),
                }))
                .await?;
            }
            return Ok(());
        }
        if !message.published {
            if message.text.is_empty() {
                return Ok(());
            }
            sink.send_wait(
                address
                    .clone()
                    .activity(ItemEvent::Started(ItemStart::AgentMessage(
                        AgentMessageStart {
                            classification,
                            text: message.text.clone(),
                        },
                    ))),
            )
            .await?;
            message.published = true;
        } else if event.event_type == "assistant.message_delta" {
            sink.send_wait(address.clone().activity(ItemEvent::Message {
                event: MessageItemEvent::Delta(text.into()),
            }))
            .await?;
        }
        if event.event_type == "assistant.message" {
            if let MessageClassification::Known(role) = classification {
                sink.send_wait(address.clone().activity(ItemEvent::Message {
                    event: MessageItemEvent::Classified(MessageClassification::Known(role)),
                }))
                .await?;
            }
            sink.send_wait(address.completion(text.into(), MessageOutcome::Completed))
                .await?;
        }
        Ok(())
    }

    async fn reasoning(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let id = event
            .data
            .get("reasoningId")
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot reasoning omitted reasoningId"))?;
        let key = (thread.into(), id.into());
        if (self.settlement.is_some() && self.root_thread.as_deref() == Some(thread)
            || self.turn.get(thread).is_some_and(|turn| turn.settled))
            && !self.reasoning_address.contains_key(&key)
        {
            return Err(Self::protocol(
                "new foreground reasoning follows native completion",
            ));
        }
        let address = if let Some(address) = self.reasoning_address.get(&key) {
            address.clone()
        } else {
            let address = ItemAddress {
                turn: self.address(thread)?,
                item_id: allocated_item_id(),
                reasoning: true,
            };
            self.reasoning_address.insert(key, address.clone());
            address
        };
        let message = self
            .reasoning
            .observe(thread, id, CopilotPhaseMetadata::Missing, false)
            .map_err(|error| Self::protocol(error.to_string()))?;
        let complete = event.event_type == "assistant.reasoning";
        let text = event
            .data
            .get(if complete { "content" } else { "deltaContent" })
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot reasoning omitted text content"))?;
        if complete {
            CopilotMessageStore::complete(message, text)
                .map_err(|error| Self::protocol(error.to_string()))?;
        } else {
            CopilotMessageStore::append(message, text)
                .map_err(|error| Self::protocol(error.to_string()))?;
        }
        if !message.published {
            if message.text.is_empty() {
                return Ok(());
            }
            sink.send_wait(
                address
                    .clone()
                    .activity(ItemEvent::Started(ItemStart::Reasoning(ReasoningStart {
                        text: message.text.clone(),
                        kind: ReasoningKind::Full,
                    }))),
            )
            .await?;
            message.published = true;
        } else if !complete {
            sink.send_wait(address.clone().activity(ItemEvent::Message {
                event: MessageItemEvent::Delta(text.into()),
            }))
            .await?;
        }
        if complete {
            sink.send_wait(address.completion(text.into(), MessageOutcome::Completed))
                .await?;
        }
        Ok(())
    }

    async fn request_tool(
        &mut self,
        thread: &str,
        request: &Value,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let id = super::decode::string_field(request, &["toolCallId", "id"])
            .ok_or_else(|| Self::protocol("Copilot tool request omitted toolCallId"))?;
        let key = (thread.into(), id.clone());
        if (self.settlement.is_some() && self.root_thread.as_deref() == Some(thread)
            || self.turn.get(thread).is_some_and(|turn| turn.settled))
            && !self.tool.contains_key(&key)
        {
            return Err(Self::protocol(
                "new foreground tool follows native completion",
            ));
        }
        let name = super::decode::string_field(request, &["name", "toolName"])
            .ok_or_else(|| Self::protocol("Copilot tool request omitted tool name"))?;
        let arguments = request.get("arguments").cloned().unwrap_or(Value::Null);
        let file_tool = matches!(name.as_str(), "apply_patch" | "edit" | "create")
            || name == "str_replace_editor"
                && matches!(
                    arguments.get("command").and_then(Value::as_str),
                    Some("str_replace" | "create" | "insert" | "undo_edit")
                );
        let invocation =
            crate::copilot::tool_semantics::normalize_invocation(NativeToolDescription {
                semantics: Default::default(),
                kind: if file_tool {
                    NativeToolKind::FileChange
                } else {
                    NativeToolKind::ToolCall
                },
                title: crate::tool_display::call_title(&name, &arguments)
                    .unwrap_or_else(|| name.clone()),
                name,
                arguments,
            });
        if let Some(existing) = self.tool.get_mut(&key) {
            if existing.invocation.kind != invocation.kind
                || existing.finished && existing.invocation != invocation
            {
                return Err(Self::protocol(
                    "tool replay conflicts with its immutable item",
                ));
            }
            if existing.invocation != invocation {
                sink.send_wait(existing.address.activity(ItemEvent::Tool {
                    event: ToolItemEvent::DescriptorUpdated(descriptor(&invocation)),
                }))
                .await?;
                existing.invocation = invocation;
            }
            return Ok(());
        }
        let address = ItemAddress {
            turn: self.address(thread)?,
            item_id: allocated_item_id(),
            reasoning: false,
        };
        sink.send_wait(address.clone().activity(tool_start(&invocation)))
            .await?;
        let input = self.pending_input.remove(&key).unwrap_or_default();
        if !input.is_empty() {
            sink.send_wait(address.clone().activity(ItemEvent::Tool {
                event: ToolItemEvent::Input(ToolInputUpdate::Text(TextUpdate::Append(
                    input.clone(),
                ))),
            }))
            .await?;
        }
        self.tool.insert(
            key,
            NativeTool {
                address,
                invocation,
                started: false,
                finished: false,
                output_sequence: None,
                output_chunks: HashMap::new(),
                final_native: None,
                has_changes: false,
            },
        );
        Ok(())
    }

    async fn tool(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        output: &mut impl TurnProgress,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let id = super::decode::string_field(&event.data, &["toolCallId", "id"])
            .ok_or_else(|| Self::protocol("Copilot tool event omitted identity"))?;
        let key = (thread.into(), id);
        if !self.tool.contains_key(&key) {
            self.request_tool(thread, &event.data, sink).await?;
        }
        let tool = self.tool.get_mut(&key).expect("tool request inserted");
        if event.event_type == "tool.shell_output" {
            let sequence = event
                .data
                .get("sequence")
                .and_then(Value::as_u64)
                .ok_or_else(|| Self::protocol("Copilot shell output omitted its sequence"))?;
            let chunk = event
                .data
                .get("text")
                .and_then(Value::as_str)
                .ok_or_else(|| Self::protocol("shell output omitted text"))?;
            if let Some(previous) = tool.output_chunks.get(&sequence) {
                if previous != chunk {
                    return Err(Self::protocol(
                        "shell output sequence conflicts with retained text",
                    ));
                }
                return Ok(());
            }
            if tool
                .output_sequence
                .is_some_and(|previous| sequence < previous)
            {
                return Err(Self::protocol("shell output sequence arrived out of order"));
            }
            if tool
                .output_sequence
                .is_some_and(|previous| previous.checked_add(1) != Some(sequence))
            {
                sink.send_wait(AdapterActivity::Warning(
                    forge_provider_api::ProviderWarning {
                        scope: forge_provider_api::WarningScope::Thread {
                            thread_id: sink.scope().thread_id().clone(),
                        },
                        kind: forge_provider_api::ProviderWarningKind::NativeNotice {
                            category: ProviderFailureKind::Protocol,
                        },
                        message: SanitizedMessage::new(
                            "Provider tool output sequence was incomplete",
                        )
                        .expect("static diagnostic is nonempty"),
                    },
                ))
                .await?;
            }
            tool.output_chunks.insert(sequence, chunk.to_owned());
            tool.output_sequence = Some(sequence);
        }
        output.record_tool_call();
        if event.event_type == "tool.execution_start" {
            if !tool.started && !tool.finished {
                sink.send_wait(tool.address.clone().activity(ItemEvent::Tool {
                    event: ToolItemEvent::ExecutionStarted,
                }))
                .await?;
                tool.started = true;
            }
            let changes = super::decode::provider_change_set(&event.data);
            if !changes.is_empty() {
                tool.has_changes = true;
                sink.send_wait(tool.address.clone().activity(ItemEvent::Tool {
                    event: ToolItemEvent::Changes(changes),
                }))
                .await?;
            }
            return Ok(());
        }
        let complete = event.event_type == "tool.execution_complete";
        if complete && let Some(previous) = &tool.final_native {
            if previous != &event.data {
                return Err(Self::protocol(
                    "tool completion replay conflicts with retained result",
                ));
            }
            return Ok(());
        }
        let succeeded = if complete {
            event
                .data
                .get("success")
                .and_then(Value::as_bool)
                .ok_or_else(|| Self::protocol("Copilot tool result omitted success"))?
        } else {
            false
        };
        let text = if complete {
            event
                .data
                .pointer("/result/detailedContent")
                .or_else(|| event.data.pointer("/result/content"))
                .and_then(Value::as_str)
                .map(strip_ansi_escapes::strip_str)
        } else {
            super::decode::string_field(
                &event.data,
                &[
                    "partialOutput",
                    "progressMessage",
                    "text",
                    "output",
                    "chunk",
                ],
            )
        };
        let mut changes = super::decode::provider_change_set(&event.data);
        if complete
            && succeeded
            && changes.is_empty()
            && !tool.has_changes
            && tool.invocation.kind == NativeToolKind::FileChange
        {
            match super::file_change::from_output(
                text.as_deref().unwrap_or_default(),
                &self.workspace,
            ) {
                Ok(reported) => changes = reported,
                Err(_) => {
                    sink.send_wait(AdapterActivity::Warning(
                        forge_provider_api::ProviderWarning {
                            scope: forge_provider_api::WarningScope::Thread {
                                thread_id: sink.scope().thread_id().clone(),
                            },
                            kind: forge_provider_api::ProviderWarningKind::NativeNotice {
                                category: ProviderFailureKind::Protocol,
                            },
                            message: SanitizedMessage::new(
                                "Provider file changes could not be normalized",
                            )
                            .expect("static diagnostic is nonempty"),
                        },
                    ))
                    .await?;
                }
            }
        }
        let retain_streamed_output = complete
            && tool.output_sequence.is_some()
            && event.data.pointer("/result/detailedContent").is_none();
        if let Some(text) = text.filter(|_| !retain_streamed_output) {
            let update = if event.event_type == "tool.shell_output" {
                TextUpdate::Append(text)
            } else {
                TextUpdate::Replace(text)
            };
            sink.send_wait(tool.address.clone().activity(ItemEvent::Tool {
                event: ToolItemEvent::Output(update),
            }))
            .await?;
        }
        if !changes.is_empty() {
            tool.has_changes = true;
            sink.send_wait(tool.address.clone().activity(ItemEvent::Tool {
                event: ToolItemEvent::Changes(changes),
            }))
            .await?;
        }
        output.record_workspace_change(complete && succeeded && tool.has_changes);
        if complete && !tool.finished {
            let outcome = if succeeded {
                ToolOutcome::Succeeded
            } else if super::decode::is_denied(&event.data) {
                ToolOutcome::Denied {
                    reason: forge_provider_api::diagnostic::SanitizedMessage::new(
                        "Provider denied this operation",
                    )
                    .expect("static denial is nonempty"),
                }
            } else {
                ToolOutcome::Failed {
                    diagnostic: ToolDiagnostic {
                        kind: super::error::tool_failure_kind(&event.data),
                        message: SanitizedMessage::new("Provider tool execution failed")
                            .expect("static diagnostic is nonempty"),
                    },
                }
            };
            sink.send_wait(
                tool.address
                    .clone()
                    .activity(tool_completion(&tool.invocation, outcome)),
            )
            .await?;
            tool.finished = true;
            tool.final_native = Some(event.data.clone());
        }
        Ok(())
    }

    async fn compaction(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        _output: &mut impl TurnProgress,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let scope = if event.agent_id.is_some() {
            self.children
                .get(thread)
                .ok_or_else(|| Self::protocol("compaction refers to an unknown child"))?
        } else {
            &self.scope
        };
        let thread_id = scope.thread_id.clone();
        let update = super::decode::decode_compaction(
            event,
            thread_id.as_str(),
            self.compaction.get(thread),
        )?;
        let writer = if scope != &self.scope {
            sink.register_observed_turn(TurnAddress {
                scope: scope.clone(),
                thread_id: thread_id.clone(),
                turn_id: scope
                    .turn_id()
                    .map_err(|_| Self::protocol("invalid child compaction owner"))?,
            })?
        } else {
            sink.clone()
        };
        writer
            .send_wait(AdapterActivity::Thread {
                thread_id,
                event: ThreadEvent::CompactionUpdated(Box::new(update.clone())),
            })
            .await?;
        self.compaction.insert(thread.into(), update);
        Ok(())
    }

    pub async fn publish_task_snapshot(
        &mut self,
        data: Value,
        scope_id: String,
        _output: &mut impl TurnProgress,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        let Some(update) = super::decode::checklist_update(&data, scope_id) else {
            return Ok(());
        };
        if let Some(address) = self
            .root_thread
            .as_deref()
            .and_then(|thread| self.turn.get(thread))
            .map(|turn| turn.address.clone())
        {
            sink.send_wait(turn_activity(
                address,
                TurnMetadataEvent::ChecklistUpdated(update),
            ))
            .await?;
        } else {
            self.pending_checklist = Some(update);
        }
        Ok(())
    }

    pub async fn finish_success(
        &mut self,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        match self.settlement {
            Some(TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            }) => return Ok(()),
            Some(_) => {
                return Err(Self::protocol(
                    "unsuccessful Copilot invocation cannot become successful",
                ));
            }
            None => {}
        }
        if self.tool.values().any(|tool| {
            !tool.finished && tool.address.turn.thread_id == self.scope.thread_id.clone()
        }) {
            return Err(Self::protocol(
                "Copilot became idle before completing a foreground tool",
            ));
        }
        if self
            .reasoning
            .has_unfinished_text(self.root_thread.as_deref())
        {
            return Err(Self::protocol(
                "Copilot became idle before completing streamed reasoning",
            ));
        }
        if self
            .message
            .has_unfinished_text(self.root_thread.as_deref())
        {
            return Err(Self::protocol(
                "Copilot became idle before completing streamed message text",
            ));
        }
        let thread = self
            .root_thread
            .clone()
            .ok_or_else(|| Self::protocol("Copilot completed without a root conversation"))?;
        if let Some((_id, text)) = self.message.fallback(&thread) {
            let address = ItemAddress {
                turn: self.address(&thread)?,
                item_id: allocated_item_id(),
                reasoning: false,
            };
            sink.send_wait(
                address
                    .clone()
                    .activity(ItemEvent::Started(ItemStart::AgentMessage(
                        AgentMessageStart {
                            classification: MessageClassification::Known(
                                AgentMessageRole::Response,
                            ),
                            text: text.clone(),
                        },
                    ))),
            )
            .await?;
            sink.send_wait(address.completion(text, MessageOutcome::Completed))
                .await?;
        }
        self.settlement = Some(TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        });
        Ok(())
    }

    /// Interrupt unfinished content without inventing a native turn outcome.
    pub(super) async fn interrupt_content(
        &mut self,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        for (key, text) in self.message.interrupt_open(self.root_thread.as_deref()) {
            let address = self
                .message_address
                .get(&key)
                .expect("published Copilot message has an address")
                .clone();
            sink.send_wait(address.completion(text, MessageOutcome::Interrupted))
                .await?;
        }
        for (key, text) in self.reasoning.interrupt_open(self.root_thread.as_deref()) {
            let address = self
                .reasoning_address
                .get(&key)
                .expect("published Copilot reasoning has an address")
                .clone();
            sink.send_wait(address.completion(text, MessageOutcome::Interrupted))
                .await?;
        }
        for tool in self.tool.values_mut().filter(|tool| {
            !tool.finished && tool.address.turn.thread_id == self.scope.thread_id.clone()
        }) {
            sink.send_wait(
                tool.address
                    .clone()
                    .activity(tool_completion(&tool.invocation, ToolOutcome::Interrupted)),
            )
            .await?;
            tool.finished = true;
        }
        Ok(())
    }

    /// Close native foreground content before callback collection can delay its deadlines.
    pub(super) async fn finish_native(
        &mut self,
        outcome: TurnOutcome,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        if self.settlement.is_some_and(|retained| retained != outcome) {
            return Err(Self::protocol(
                "Copilot collected outcome conflicts with retained completion",
            ));
        }
        match outcome {
            TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            } => self.finish_success(sink).await?,
            TurnOutcome::Completed {
                stop_reason:
                    forge_provider_api::turn::TurnCompletionReason::TokenLimit
                    | forge_provider_api::turn::TurnCompletionReason::RequestLimit
                    | forge_provider_api::turn::TurnCompletionReason::Refusal,
            } => {
                return Err(Self::protocol(
                    "Copilot did not establish this completion reason",
                ));
            }
            TurnOutcome::Interrupted | TurnOutcome::Failed { .. } => {
                self.finish_failure(outcome, sink).await?
            }
        }
        if let Some(root) = self
            .root_thread
            .as_ref()
            .and_then(|thread| self.turn.get_mut(thread))
        {
            root.settled = true;
        }
        Ok(())
    }

    pub async fn finish_failure(
        &mut self,
        outcome: TurnOutcome,
        sink: &AdapterActivityWriter,
    ) -> Result<(), ProviderError> {
        if self.settlement
            == Some(TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            })
        {
            return Err(Self::protocol(
                "completed Copilot invocation cannot change its outcome",
            ));
        }
        self.settlement = Some(outcome);
        self.interrupt_content(sink).await?;
        Ok(())
    }
}

/// Allocate an opaque normalized item identity independently of reused native IDs.
fn allocated_item_id() -> ItemId {
    ItemId::new(uuid::Uuid::new_v4().to_string()).expect("UUID always names a nonempty item")
}

/// Attribute metadata to the original normalized turn.
fn turn_activity(address: TurnAddress, event: TurnMetadataEvent) -> AdapterActivity {
    AdapterActivity::Turn {
        thread_id: address.thread_id,
        turn_id: address.turn_id,
        event,
    }
}

/// Preserve the normalized operation meaning and inspectable arguments.
fn descriptor(invocation: &NativeToolDescription) -> ToolDescriptor {
    ToolDescriptor {
        name: invocation.name.clone(),
        title: invocation.title.clone(),
        semantics: invocation.semantics.clone(),
        arguments: InspectableArguments::new(invocation.arguments.clone()),
    }
}

/// Admit a native operation with a stable item type.
fn tool_start(invocation: &NativeToolDescription) -> ItemEvent {
    let descriptor = descriptor(invocation);
    ItemEvent::Started(match invocation.kind {
        NativeToolKind::Command => ItemStart::CommandExecution(descriptor),
        NativeToolKind::FileChange => ItemStart::FileChange(descriptor),
        NativeToolKind::ToolCall => ItemStart::ToolCall(descriptor),
    })
}

/// Complete the originally admitted operation type without inventing absent snapshots.
fn tool_completion(invocation: &NativeToolDescription, outcome: ToolOutcome) -> ItemEvent {
    let completion = ToolCompletion {
        outcome,
        descriptor: None,
        output: None,
        changes: Default::default(),
    };
    ItemEvent::Completed(match invocation.kind {
        NativeToolKind::Command => ItemCompletion::CommandExecution(completion),
        NativeToolKind::FileChange => ItemCompletion::FileChange(completion),
        NativeToolKind::ToolCall => ItemCompletion::ToolCall(completion),
    })
}

/// Explain why the decoder's event dispatch does not apply a native notification.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum UnmappedDisposition {
    /// Another retained adapter observer consumes this notification from the same stream.
    OwnedElsewhere,
    /// The pinned SDK defines this notification as presentation, catalog, or configuration
    /// metadata that has no normalized provider event.
    NoNormalizedMeaning,
    /// The notification may carry content, lifecycle, or interaction evidence that the adapter
    /// does not map, so it is reported rather than discarded.
    Unsupported,
}

/// Classify Copilot SDK 1.0.26 notifications that the main decoder dispatch does not map.
///
/// The prompt driver owns idle, abort, checklist, and turn-boundary evidence; the terminal
/// observer owns background inventory; the SDK handle and turn evidence own input ancestry; and
/// the permission callback owns permission requests. Unknown and interaction-bearing types such
/// as `session.warning`, `model.call_failure`, or `elicitation.requested` remain `Unsupported`.
fn unmapped_disposition(event_type: &str) -> UnmappedDisposition {
    match event_type {
        "session.todos_changed"
        | "session.background_tasks_changed"
        | "user.message"
        | "system.message"
        | "assistant.turn_retry"
        | "permission.requested"
        | "permission.completed"
        // The SDK tool handler owns host tool execution; these notifications only announce
        // its start and the result the handler already returned.
        | "external_tool.requested"
        | "external_tool.completed"
        | "pending_messages.modified" => UnmappedDisposition::OwnedElsewhere,
        "session.start"
        | "session.resume"
        | "session.info"
        | "session.title_changed"
        | "session.model_change"
        | "session.model_deselected"
        | "session.mode_changed"
        | "session.mode_notice_delivered"
        | "session.plan_changed"
        | "session.tools_updated"
        | "session.skills_loaded"
        | "session.extensions_loaded"
        | "session.extensions.attachments_pushed"
        | "session.custom_agents_updated"
        | "session.mcp_servers_loaded"
        | "session.mcp_server_status_changed"
        | "session.mcp_server_tools_loaded"
        | "session.mcp_server_removed"
        | "session.mcp_server_needs_reconnect"
        | "session.workspace_file_changed"
        | "session.context_changed"
        | "session.usage_checkpoint"
        | "session.quota_observation"
        | "session.session_limits_changed"
        | "session.permissions_changed"
        | "session.managed_plugin_progress"
        | "session.managed_settings_enforced"
        | "session.managed_settings_resolved"
        | "session.remote_steerable_changed"
        | "session.auto_mode_resolved"
        | "session.usage_record"
        | "session.auto_tier_recommendation"
        | "session.indexed_search"
        | "session.completion_receipt"
        | "session.binary_asset"
        | "session.canvas.closed"
        | "session.canvas.opened"
        | "session.canvas.recorded"
        | "session.canvas.registry_changed"
        | "session.canvas.removed"
        | "session.canvas.unavailable"
        | "session.schedule_cancelled"
        | "session.schedule_created"
        | "session.schedule_rearmed"
        | "capabilities.changed"
        | "commands.changed"
        | "instruction.update"
        | "mcp.prompts.list_changed"
        | "mcp.resources.list_changed"
        | "mcp.tools.list_changed"
        | "mcp.headers_refresh_completed"
        | "mcp.oauth_completed"
        | "mcp.oauth_recovery_completed"
        | "hook.start"
        | "hook.progress"
        | "hook.end"
        | "skill.invoked"
        | "skill.invoked_ref"
        | "skill.context_delivered"
        | "skill.context_delivered_ref"
        | "tool_search.activated"
        | "assistant.intent"
        | "assistant.server_tool_progress"
        | "sandbox.decision"
        | "subagent.selected"
        | "subagent.deselected"
        | "subagent.configured"
        | "ui.ephemeral_query"
        // The retained prompt owner settles on abort; a running turn's aborted idle follows it.
        | "abort"
        // An interrupted agent reports its terminal outcome through its aborted idle.
        | "agent.interrupted" => UnmappedDisposition::NoNormalizedMeaning,
        // `model.*` is the runtime's model-call telemetry; `assistant.*` and `session.error`
        // carry the same turns, content, usage, and failures that Forge normalizes.
        telemetry if telemetry.starts_with("model.") => UnmappedDisposition::NoNormalizedMeaning,
        _ => UnmappedDisposition::Unsupported,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{BindingId, ProviderEvent, TurnEvent};

    /// Construct a native SDK envelope without a live provider request.
    fn event(id: &str, kind: &str, contents: Value) -> SessionEvent {
        serde_json::from_value(serde_json::json!({"id":id,"type":kind,"timestamp":"2026-11-05T00:00:00Z","data":contents})).unwrap()
    }

    /// Give native observations their admitted host turn and positive-start capability.
    async fn fixture() -> (
        CopilotEventDecoder,
        forge_provider_api::completion::ProviderCompletion,
        AdapterActivityWriter,
        forge_provider_api::ProviderEventStream,
        super::super::test_turn::NativeTurnFixture,
    ) {
        let scope = InvocationScope::new(
            forge_provider_api::testing::root_thread("decoder"),
            "exchange",
        );
        let address = TurnAddress {
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().unwrap(),
            scope: scope.clone(),
        };
        let (sink, stream) =
            forge_provider_api::events::channel(BindingId::new("binding").unwrap(), scope.clone());
        let start = super::super::test_turn::native_turn(sink.clone(), address).await;
        let output = forge_provider_api::completion::ProviderCompletion {
            provider_session_id: Some("native-session".try_into().unwrap()),
            runtime: forge_provider_api::ProviderRuntime {
                provider: "Copilot CLI".into(),
                model: Some("configured-default".into()),
            },
            ..Default::default()
        };
        (
            CopilotEventDecoder::new(scope, ".".into()).with_turn_owner(sink.clone()),
            output,
            sink.activity_writer(),
            stream,
            start,
        )
    }

    /// Actual model evidence is ordered after start and never comes from configured defaults.
    #[tokio::test]
    async fn confirmed_models_follow_start_and_keep_thread_defaults_separate() -> anyhow::Result<()>
    {
        let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
        decoder
            .decode(
                &event(
                    "early-model",
                    "assistant.turn_retry",
                    serde_json::json!({"model":"configured-default","turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        assert!(stream.try_next()?.is_none());
        start.start.observed(10).await?;
        decoder
            .decode(
                &event(
                    "start",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        let replay = event(
            "same-model",
            "assistant.usage",
            serde_json::json!({"model":"configured-default","apiCallId":"call","inputTokens":3}),
        );
        decoder.decode(&replay, &mut output, &sink).await?;
        decoder.decode(&replay, &mut output, &sink).await?;
        decoder
            .decode(
                &event(
                    "changed-model",
                    "assistant.turn_retry",
                    serde_json::json!({"model":"actual-second","turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        let mut models = Vec::new();
        let mut started = false;
        while let Some(bound) = stream.try_next()? {
            match bound.event {
                ProviderEvent::Turn {
                    event: TurnEvent::Started(_),
                    ..
                } => started = true,
                ProviderEvent::Turn {
                    event: TurnEvent::ModelResolved(model),
                    ..
                } => {
                    assert!(started);
                    models.push(model.model_id.as_str().to_owned());
                }
                ProviderEvent::Turn {
                    event: TurnEvent::Usage(_),
                    ..
                } => {}
                unexpected => panic!("unexpected model observation: {unexpected:?}"),
            }
        }
        assert_eq!(models, ["configured-default", "actual-second"]);
        assert_eq!(output.runtime.model.as_deref(), Some("configured-default"));
        Ok(())
    }

    /// A native session error remains thread-scoped with and without active work.
    #[tokio::test]
    async fn session_errors_neither_start_nor_finish_a_turn() -> anyhow::Result<()> {
        for active in [false, true] {
            let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
            if active {
                start.start.observed(10).await?;
                decoder
                    .decode(
                        &event(
                            "start",
                            "assistant.turn_start",
                            serde_json::json!({"turnId":"0"}),
                        ),
                        &mut output,
                        &sink,
                    )
                    .await?;
                while stream.try_next()?.is_some() {}
            }
            let native = event(
                "error",
                "session.error",
                serde_json::json!({"errorType":"query","statusCode":503,"message":"private-native-payload"}),
            );
            decoder.decode(&native, &mut output, &sink).await?;
            decoder.decode(&native, &mut output, &sink).await?;
            let observation = stream.try_next()?.unwrap().event;
            assert!(
                matches!(&observation, ProviderEvent::Thread { thread_id, event: ThreadEvent::Error { .. } } if thread_id == &decoder.scope.thread_id.clone())
            );
            assert!(!serde_json::to_string(&observation)?.contains("private-native-payload"));
            assert!(stream.try_next()?.is_none());
            assert!(decoder.settlement.is_none());
        }
        Ok(())
    }

    /// Usage preserves the actual call, missing token categories, and provider-measured duration.
    #[tokio::test]
    async fn native_usage_publishes_identified_turn_report() -> anyhow::Result<()> {
        let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
        start.start.observed(10).await?;
        decoder
            .decode(
                &event(
                    "start",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        decoder.decode(&event("usage", "assistant.usage", serde_json::json!({"apiCallId":"call","inputTokens":3,"outputTokens":4,"duration":730})), &mut output, &sink).await?;
        let mut reports = Vec::new();
        while let Some(bound) = stream.try_next()? {
            if let ProviderEvent::Turn {
                event: TurnEvent::Usage(report),
                ..
            } = bound.event
            {
                reports.push(report);
            }
        }
        assert_eq!(reports.len(), 1);
        assert_eq!(reports[0].report_id().as_str(), "call");
        assert_eq!(
            reports[0].model_call_id().as_ref().unwrap().as_str(),
            "call"
        );
        let usage = reports[0].usage().as_ref().unwrap();
        assert_eq!(usage.output, Some(4));
        assert_eq!(usage.cached_input, None);
        assert_eq!(*reports[0].reported_duration_ms(), Some(730));
        assert!(
            decoder
                .decode(
                    &event("empty", "assistant.usage", serde_json::json!({})),
                    &mut output,
                    &sink
                )
                .await
                .is_err()
        );
        Ok(())
    }

    /// Preserve failed operation output while withholding native error envelopes.
    #[tokio::test]
    async fn failed_tool_output_never_publishes_native_error_payload() -> anyhow::Result<()> {
        let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
        start.start.observed(10).await?;
        decoder
            .decode(
                &event(
                    "start",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        decoder.decode(&event("result", "tool.execution_complete", serde_json::json!({"toolCallId":"tool","toolName":"shell","arguments":{"command":"fixture"},"success":false,"result":{"content":"visible failed-tool output"},"error":{"code":"private-code","message":"private-message"}})), &mut output, &sink).await?;
        let mut visible = false;
        while let Some(bound) = stream.try_next()? {
            let text = serde_json::to_string(&bound.event)?;
            visible |= text.contains("visible failed-tool output");
            assert!(!text.contains("private-code"));
            assert!(!text.contains("private-message"));
        }
        assert!(visible);
        Ok(())
    }

    /// Require actual foreground item endings before normal native completion can seal the turn.
    #[tokio::test]
    async fn normal_idle_requires_explicit_foreground_tool_ending() -> anyhow::Result<()> {
        let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
        start.start.observed(1).await?;
        decoder
            .decode(
                &event(
                    "root",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        decoder.decode(&event("tool", "tool.execution_start",
            serde_json::json!({"toolCallId":"call","toolName":"read","arguments":{"path":"a.rs"}})),
            &mut output, &sink).await?;
        assert!(
            decoder
                .finish_native(
                    TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                    &sink
                )
                .await
                .is_err()
        );
        while let Some(bound) = stream.try_next()? {
            assert!(!matches!(
                bound.event,
                ProviderEvent::Item {
                    event: ItemEvent::Completed(_),
                    ..
                }
            ));
        }
        Ok(())
    }

    /// Unmapped native notifications are admitted by explicit policy or reported once per type.
    /// Steering Copilot holds until a model call ends is published where the model consumed it.
    ///
    /// In the observed order the root turn ends one native turn, consumes queued steering as a new
    /// `user.message`, and answers it in the same host turn. Without a consumed-input item the
    /// host can only place the steering at acknowledgement, before the earlier answer.
    #[tokio::test]
    async fn consumed_steering_publishes_a_user_item_inside_the_root_turn() -> anyhow::Result<()> {
        let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
        start.start.observed(1).await?;
        let submission = forge_provider_api::SubmissionId::try_from("steering-submission")?;
        let steering = event(
            "steering-user",
            "user.message",
            serde_json::json!({"messageId":"steering-message","content":"Reply with BETA."}),
        );
        while stream.try_next()?.is_some() {}
        decoder
            .consumed_input(&steering, submission.clone(), &sink)
            .await?;
        assert!(
            stream.try_next()?.is_none(),
            "input before the root turn started duplicated the exchange prompt"
        );
        decoder
            .decode(
                &event(
                    "root",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        while stream.try_next()?.is_some() {}
        decoder
            .consumed_input(&steering, submission.clone(), &sink)
            .await?;
        let mut started = None;
        let mut completed = false;
        while let Some(bound) = stream.try_next()? {
            if let ProviderEvent::Item { event, .. } = bound.event {
                match event {
                    forge_provider_api::ItemEvent::Started(ItemStart::UserMessage(user)) => {
                        started = Some(user);
                    }
                    forge_provider_api::ItemEvent::Completed(_) => completed = true,
                    _ => {}
                }
            }
        }
        let started = started.expect("consumed steering published no user item");
        assert_eq!(started.text, "Reply with BETA.");
        assert_eq!(started.submission_id, Some(submission));
        assert!(completed, "consumed steering left its user item open");
        Ok(())
    }

    #[tokio::test]
    async fn unmapped_native_events_warn_once_unless_documented_irrelevant() -> anyhow::Result<()> {
        let (mut decoder, mut output, sink, mut stream, start) = fixture().await;
        start.start.observed(1).await?;
        decoder
            .decode(
                &event(
                    "root",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await?;
        for (id, kind) in [
            ("title", "session.title_changed"),
            ("abort", "abort"),
            ("model-turn-ended", "model.turn_ended"),
            ("model-turn-failed", "model.turn_failed"),
            ("model-telemetry", "model.telemetry"),
            ("model-snapshot", "model.messages_snapshot"),
            ("agent-interrupted", "agent.interrupted"),
            ("external-tool", "external_tool.requested"),
            ("external-tool-done", "external_tool.completed"),
            ("usage-record", "session.usage_record"),
            ("warning-1", "session.warning"),
            ("warning-2", "session.warning"),
            ("future", "session.future_sdk_event"),
        ] {
            decoder
                .decode(&event(id, kind, serde_json::json!({})), &mut output, &sink)
                .await?;
        }
        let mut reported = Vec::new();
        while let Some(bound) = stream.try_next()? {
            if let ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Warning(notice),
            } = bound.event
            {
                assert!(matches!(
                    notice.kind,
                    forge_provider_api::ProviderWarningKind::UnsupportedNativeEvent
                ));
                reported.push(notice.message.as_str().to_owned());
            }
        }
        assert_eq!(
            reported,
            [
                "Copilot reported `session.warning`, which Forge does not apply",
                "Copilot reported `session.future_sdk_event`, which Forge does not apply",
            ]
        );
        Ok(())
    }
}
