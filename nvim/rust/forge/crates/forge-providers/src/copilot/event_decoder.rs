use std::collections::{HashMap, HashSet};

use github_copilot_sdk::SessionEvent;
use serde_json::Value;

use forge_provider_api::progress::TurnProgress;
use forge_provider_api::turn::TurnOutcome;
use forge_provider_api::{
    AgentUpdate, InvocationScope, MessageAddress, MessageClassification, MessageId, MessageRole,
    MessageUpdate, ProviderDiagnostic, ProviderError, ProviderEvent, ProviderEventSink,
    ProviderFailureKind, RuntimeUpdate, SanitizedMessage, TextUpdate, ToolActivityKind,
    ToolAddress, ToolDiagnostic, ToolInvocation, ToolOutcome, ToolUpdate, TurnAddress, TurnUpdate,
};

use super::message_state::{CopilotMessageStore, CopilotPhase, CopilotPhaseMetadata};

struct NativeTurn {
    address: TurnAddress,
    native_id: String,
    completed_at_ms: Option<i64>,
    settled: bool,
}

struct NativeTool {
    address: ToolAddress,
    invocation: ToolInvocation,
    started: bool,
    finished: bool,
    output_sequence: Option<u64>,
    has_changes: bool,
}

pub struct CopilotEventDecoder {
    scope: InvocationScope,
    workspace: String,
    root_thread: Option<String>,
    turn: HashMap<String, NativeTurn>,
    message: CopilotMessageStore,
    reasoning: CopilotMessageStore,
    message_address: HashMap<(String, String), MessageAddress>,
    reasoning_address: HashMap<(String, String), MessageAddress>,
    tool: HashMap<(String, String), NativeTool>,
    pending_input: HashMap<(String, String), String>,
    compaction: HashMap<String, forge_provider_api::Compaction>,
    observed: HashSet<String>,
    pending_checklist: Option<forge_provider_api::ProviderChecklistUpdate>,
    settlement: Option<TurnOutcome>,
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
            observed: HashSet::new(),
            pending_checklist: None,
            settlement: None,
        }
    }

    pub fn replay_safe(&self) -> bool {
        !self.message.has_text()
            && !self.reasoning.has_text()
            && self.tool.is_empty()
            && self.pending_input.is_empty()
    }

    /// Match late events only against native identities already owned by this invocation.
    pub(crate) fn owns_event(&self, event: &SessionEvent) -> bool {
        if self.observed.contains(&event.id) {
            return true;
        }
        let Some(thread) = event.agent_id.as_ref().or(self.root_thread.as_ref()) else {
            return false;
        };
        if let Some(id) = event.data.get("messageId").and_then(Value::as_str) {
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
        ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
            forge_provider_api::error::ProviderOperation::NativeTransport,
            forge_provider_api::error::ProviderFailureKind::Protocol,
        ))
    }

    fn thread(
        &self,
        event: &SessionEvent,
        output: &impl TurnProgress,
    ) -> Result<String, ProviderError> {
        event
            .agent_id
            .clone()
            .or_else(|| output.session_id().map(|id| id.as_str().to_owned()))
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

    async fn diagnostic(
        &self,
        sink: &ProviderEventSink,
        kind: ProviderFailureKind,
    ) -> Result<(), ProviderError> {
        sink.send_wait(ProviderEvent::Diagnostic {
            scope: self.scope.clone().into(),
            diagnostic: ProviderDiagnostic {
                kind,
                message: SanitizedMessage::new("Provider reported a failure")
                    .expect("static diagnostic is nonempty"),
                fatal: false,
            },
        })
        .await?;
        Ok(())
    }

    async fn finish_turn(
        &mut self,
        thread: &str,
        outcome: TurnOutcome,
        sink: &ProviderEventSink,
    ) -> Result<(), ProviderError> {
        let Some(turn) = self.turn.get_mut(thread) else {
            return Ok(());
        };
        if !turn.settled {
            sink.send_wait(ProviderEvent::Turn {
                address: turn.address.clone(),
                update: TurnUpdate::Finished {
                    outcome,
                    completed_at_ms: turn
                        .completed_at_ms
                        .unwrap_or_else(super::decode::timestamp_ms),
                },
            })
            .await?;
            turn.settled = true;
        }
        Ok(())
    }

    pub async fn decode(
        &mut self,
        event: &SessionEvent,
        output: &mut impl TurnProgress,
        sink: &ProviderEventSink,
    ) -> Result<(), ProviderError> {
        if event.ephemeral != Some(true) && !self.observed.insert(event.id.clone()) {
            return Ok(());
        }
        let thread = self.thread(event, output)?;
        if event.agent_id.is_some()
            && !self.turn.contains_key(&thread)
            && matches!(
                event.event_type.as_str(),
                "assistant.message_start"
                    | "assistant.message_delta"
                    | "assistant.message"
                    | "assistant.reasoning_delta"
                    | "assistant.reasoning"
                    | "assistant.usage"
                    | "tool.execution_start"
            )
        {
            let native_id = super::decode::string_field(&event.data, &["turnId"])
                .unwrap_or_else(|| event.id.clone());
            let address = TurnAddress {
                scope: self.scope.clone(),
                thread_id: thread.clone(),
                turn_id: format!("native-child:{native_id}").into(),
            };
            sink.send_wait(ProviderEvent::Turn {
                address: address.clone(),
                update: TurnUpdate::Started {
                    started_at_ms: super::decode::timestamp_ms(),
                },
            })
            .await?;
            self.turn.insert(
                thread.clone(),
                NativeTurn {
                    address,
                    native_id,
                    completed_at_ms: None,
                    settled: false,
                },
            );
        }
        if event.agent_id.is_none() {
            self.root_thread = Some(thread.clone());
            if !event.event_type.starts_with("session.compaction_")
                && let Some(model) = super::decode::string_field(&event.data, &["model"])
                && !model.is_empty()
                && output.runtime().model.as_ref() != Some(&model)
            {
                output.set_model(model);
                sink.send_wait(ProviderEvent::Runtime {
                    scope: self.scope.clone(),
                    update: RuntimeUpdate::Resolved {
                        runtime: output.runtime().clone(),
                    },
                })
                .await?;
            }
        }
        match event.event_type.as_str() {
            "assistant.turn_start" => {
                self.finish_turn(&thread, TurnOutcome::Completed, sink)
                    .await?;
                let native_id = super::decode::string_field(&event.data, &["turnId"])
                    .ok_or_else(|| Self::protocol("Copilot turn start omitted turnId"))?;
                // Background wakeups reuse interactionId and turnId; only the durable event identifies this execution.
                let address = TurnAddress {
                    scope: self.scope.clone(),
                    thread_id: thread.clone(),
                    turn_id: event.id.clone().into(),
                };
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: TurnUpdate::Started {
                        started_at_ms: super::decode::timestamp_ms(),
                    },
                })
                .await?;
                self.turn.insert(
                    thread.clone(),
                    NativeTurn {
                        address: address.clone(),
                        native_id,
                        completed_at_ms: None,
                        settled: false,
                    },
                );
                if event.agent_id.is_none()
                    && let Some(update) = self.pending_checklist.take()
                {
                    sink.send_wait(ProviderEvent::Checklist { address, update })
                        .await?;
                }
            }
            "assistant.turn_end" => {
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
                let completed_at_ms = super::decode::timestamp_ms();
                turn.completed_at_ms = Some(completed_at_ms);
                sink.send_wait(ProviderEvent::Turn {
                    address: turn.address.clone(),
                    update: TurnUpdate::CallFinished { completed_at_ms },
                })
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
                    sink.send_wait(ProviderEvent::Tool {
                        address: tool.address.clone(),
                        update: ToolUpdate::Input {
                            update: TextUpdate::Append(text.into()),
                        },
                    })
                    .await?;
                } else {
                    self.pending_input
                        .entry((thread, id.into()))
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
            "assistant.streaming_delta" => {
                let received_bytes =
                    super::decode::unsigned_field(&event.data, &["totalResponseSizeBytes"])
                        .ok_or_else(|| {
                            Self::protocol("Copilot streaming progress omitted byte count")
                        })?;
                sink.send_wait(ProviderEvent::Runtime {
                    scope: self.scope.clone(),
                    update: RuntimeUpdate::Progress { received_bytes },
                })
                .await?;
            }
            "assistant.idle" if event.agent_id.is_none() => {
                sink.send_wait(ProviderEvent::Runtime {
                    scope: self.scope.clone(),
                    update: RuntimeUpdate::ForegroundIdle,
                })
                .await?;
            }
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
                    sink.send_wait(ProviderEvent::Runtime {
                        scope: self.scope.clone(),
                        update: RuntimeUpdate::ContextUsage { usage },
                    })
                    .await?;
                }
            }
            "assistant.usage" => {
                let update = forge_provider_api::usage::UsageUpdate {
                    context_adjustment: false,
                    id: super::decode::string_field(&event.data, &["apiCallId"])
                        .unwrap_or_else(|| event.id.clone()),
                    usage: forge_provider_api::usage::TokenUsage {
                        input: super::decode::unsigned_field(&event.data, &["inputTokens"]),
                        cached_input: super::decode::unsigned_field(
                            &event.data,
                            &["cacheReadTokens"],
                        ),
                        reasoning: super::decode::unsigned_field(&event.data, &["reasoningTokens"]),
                        output: super::decode::unsigned_field(&event.data, &["outputTokens"]),
                    },
                    cumulative: None,
                    cumulative_total: None,
                };
                sink.send_wait(ProviderEvent::Usage {
                    address: self.address(&thread)?,
                    update,
                })
                .await?;
            }
            "subagent.started" | "subagent.completed" | "subagent.failed" => {
                if let Some(event) = super::decode::agent_lifecycle(event) {
                    sink.send_wait(ProviderEvent::Agent {
                        scope: self.scope.clone(),
                        update: AgentUpdate::Lifecycle { event },
                    })
                    .await?;
                }
            }
            "session.compaction_start" | "session.compaction_complete" => {
                self.compaction(event, &thread, output, sink).await?;
            }
            "session.error" if !event.is_transient_error() => {
                self.diagnostic(sink, super::error::session_failure_kind(&event.data))
                    .await?;
            }
            _ => {}
        }
        Ok(())
    }

    async fn message(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        sink: &ProviderEventSink,
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
        let address = if let Some(address) = self.message_address.get(&key) {
            address.clone()
        } else {
            let address = MessageAddress {
                turn: self.address(thread)?,
                message_id: MessageId::Native(id.into()),
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
                MessageClassification::Known(MessageRole::Commentary)
            }
            CopilotPhaseMetadata::Known(CopilotPhase::FinalAnswer) => {
                MessageClassification::Known(MessageRole::Response)
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
                sink.send_wait(ProviderEvent::Message {
                    address,
                    update: MessageUpdate::Classified { role },
                })
                .await?;
            }
            return Ok(());
        }
        if !message.published {
            if message.text.is_empty() {
                return Ok(());
            }
            sink.send_wait(ProviderEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification,
                    text: message.text.clone(),
                },
            })
            .await?;
            message.published = true;
        } else if event.event_type == "assistant.message_delta" {
            sink.send_wait(ProviderEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Delta { text: text.into() },
            })
            .await?;
        }
        if event.event_type == "assistant.message" {
            if let MessageClassification::Known(role) = classification {
                sink.send_wait(ProviderEvent::Message {
                    address: address.clone(),
                    update: MessageUpdate::Classified { role },
                })
                .await?;
            }
            sink.send_wait(ProviderEvent::Message {
                address,
                update: MessageUpdate::Completed { text: text.into() },
            })
            .await?;
        }
        Ok(())
    }

    async fn reasoning(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        sink: &ProviderEventSink,
    ) -> Result<(), ProviderError> {
        let id = event
            .data
            .get("reasoningId")
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot reasoning omitted reasoningId"))?;
        let key = (thread.into(), id.into());
        let address = if let Some(address) = self.reasoning_address.get(&key) {
            address.clone()
        } else {
            let address = MessageAddress {
                turn: self.address(thread)?,
                message_id: MessageId::Synthetic(format!("reasoning:{id}")),
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
            sink.send_wait(ProviderEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification: MessageClassification::Known(MessageRole::ThoughtSummary),
                    text: message.text.clone(),
                },
            })
            .await?;
            message.published = true;
        } else if !complete {
            sink.send_wait(ProviderEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Delta { text: text.into() },
            })
            .await?;
        }
        if complete {
            sink.send_wait(ProviderEvent::Message {
                address,
                update: MessageUpdate::Completed { text: text.into() },
            })
            .await?;
        }
        Ok(())
    }

    async fn request_tool(
        &mut self,
        thread: &str,
        request: &Value,
        sink: &ProviderEventSink,
    ) -> Result<(), ProviderError> {
        let id = super::decode::string_field(request, &["toolCallId", "id"])
            .ok_or_else(|| Self::protocol("Copilot tool request omitted toolCallId"))?;
        let key = (thread.into(), id.clone());
        if self.tool.contains_key(&key) {
            return Ok(());
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
        let invocation = crate::copilot::tool_semantics::normalize_invocation(ToolInvocation {
            semantics: Default::default(),
            kind: if file_tool {
                ToolActivityKind::FileChange
            } else {
                ToolActivityKind::ToolCall
            },
            title: crate::tool_display::call_title(&name, &arguments)
                .unwrap_or_else(|| name.clone()),
            name,
            arguments,
        });
        let address = ToolAddress {
            turn: self.address(thread)?,
            tool_id: id.into(),
        };
        sink.send_wait(ProviderEvent::Tool {
            address: address.clone(),
            update: ToolUpdate::Requested {
                invocation: invocation.clone(),
            },
        })
        .await?;
        let input = self.pending_input.remove(&key).unwrap_or_default();
        if !input.is_empty() {
            sink.send_wait(ProviderEvent::Tool {
                address: address.clone(),
                update: ToolUpdate::Input {
                    update: TextUpdate::Append(input.clone()),
                },
            })
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
        sink: &ProviderEventSink,
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
            if tool
                .output_sequence
                .is_some_and(|previous| sequence <= previous)
            {
                return Ok(());
            }
            if tool
                .output_sequence
                .is_some_and(|previous| sequence != previous + 1)
            {
                sink.send_wait(ProviderEvent::Diagnostic {
                    scope: self.scope.clone().into(),
                    diagnostic: ProviderDiagnostic {
                        kind: ProviderFailureKind::Protocol,
                        message: SanitizedMessage::new(
                            "Provider tool output sequence was incomplete",
                        )
                        .expect("static diagnostic is nonempty"),
                        fatal: false,
                    },
                })
                .await?;
            }
            tool.output_sequence = Some(sequence);
        }
        output.record_tool_call();
        if event.event_type == "tool.execution_start" {
            if !tool.started && !tool.finished {
                sink.send_wait(ProviderEvent::Tool {
                    address: tool.address.clone(),
                    update: ToolUpdate::Started,
                })
                .await?;
                tool.started = true;
            }
            let changes = super::decode::provider_change_set(&event.data);
            if !changes.is_empty() {
                tool.has_changes = true;
                sink.send_wait(ProviderEvent::Tool {
                    address: tool.address.clone(),
                    update: ToolUpdate::Changes { changes },
                })
                .await?;
            }
            return Ok(());
        }
        let complete = event.event_type == "tool.execution_complete";
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
            && tool.invocation.kind == ToolActivityKind::FileChange
        {
            match super::file_change::from_output(
                text.as_deref().unwrap_or_default(),
                &self.workspace,
            ) {
                Ok(reported) => changes = reported,
                Err(_) => {
                    sink.send_wait(ProviderEvent::Diagnostic {
                        scope: self.scope.clone().into(),
                        diagnostic: ProviderDiagnostic {
                            kind: ProviderFailureKind::Protocol,
                            message: SanitizedMessage::new(
                                "Provider file changes could not be normalized",
                            )
                            .expect("static diagnostic is nonempty"),
                            fatal: false,
                        },
                    })
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
            sink.send_wait(ProviderEvent::Tool {
                address: tool.address.clone(),
                update: ToolUpdate::Output { update },
            })
            .await?;
        }
        if !changes.is_empty() {
            tool.has_changes = true;
            sink.send_wait(ProviderEvent::Tool {
                address: tool.address.clone(),
                update: ToolUpdate::Changes { changes },
            })
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
            sink.send_wait(ProviderEvent::Tool {
                address: tool.address.clone(),
                update: ToolUpdate::Finished { outcome },
            })
            .await?;
            tool.finished = true;
        }
        Ok(())
    }

    async fn compaction(
        &mut self,
        event: &SessionEvent,
        thread: &str,
        _output: &mut impl TurnProgress,
        sink: &ProviderEventSink,
    ) -> Result<(), ProviderError> {
        let update = super::decode::decode_compaction(event, thread, self.compaction.get(thread))?;
        self.compaction.insert(thread.into(), update.clone());
        sink.send_wait(ProviderEvent::Compaction {
            scope: self.scope.clone().into(),
            update: update.into(),
        })
        .await?;
        Ok(())
    }

    pub async fn publish_task_snapshot(
        &mut self,
        data: Value,
        scope_id: String,
        _output: &mut impl TurnProgress,
        sink: &ProviderEventSink,
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
            sink.send_wait(ProviderEvent::Checklist { address, update })
                .await?;
        } else {
            self.pending_checklist = Some(update);
        }
        Ok(())
    }

    pub async fn finish_success(&mut self, sink: &ProviderEventSink) -> Result<(), ProviderError> {
        match self.settlement {
            Some(TurnOutcome::Completed) => return Ok(()),
            Some(_) => {
                return Err(Self::protocol(
                    "unsuccessful Copilot invocation cannot become successful",
                ));
            }
            None => {}
        }
        if self.message.has_unfinished_text() {
            return Err(Self::protocol(
                "Copilot became idle before completing streamed message text",
            ));
        }
        let thread = self
            .root_thread
            .clone()
            .ok_or_else(|| Self::protocol("Copilot completed without a root conversation"))?;
        if let Some((id, text)) = self.message.fallback(&thread) {
            let address = MessageAddress {
                turn: self.address(&thread)?,
                message_id: MessageId::Synthetic(id),
            };
            sink.send_wait(ProviderEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification: MessageClassification::Known(MessageRole::Response),
                    text: text.clone(),
                },
            })
            .await?;
            sink.send_wait(ProviderEvent::Message {
                address,
                update: MessageUpdate::Completed { text },
            })
            .await?;
        }
        let threads: Vec<_> = self.turn.keys().cloned().collect();
        for thread in threads {
            self.finish_turn(&thread, TurnOutcome::Completed, sink)
                .await?;
        }
        self.settlement = Some(TurnOutcome::Completed);
        Ok(())
    }

    pub async fn finish_failure(
        &mut self,
        outcome: TurnOutcome,
        sink: &ProviderEventSink,
    ) -> Result<(), ProviderError> {
        if self.settlement == Some(TurnOutcome::Completed) {
            return Err(Self::protocol(
                "completed Copilot invocation cannot change its outcome",
            ));
        }
        self.settlement = Some(outcome);
        for key in self.message.interrupt_open() {
            let address = self
                .message_address
                .get(&key)
                .expect("published Copilot message has an address")
                .clone();
            sink.send_wait(ProviderEvent::Message {
                address,
                update: MessageUpdate::Interrupted,
            })
            .await?;
        }
        for key in self.reasoning.interrupt_open() {
            let address = self
                .reasoning_address
                .get(&key)
                .expect("published Copilot reasoning has an address")
                .clone();
            sink.send_wait(ProviderEvent::Message {
                address,
                update: MessageUpdate::Interrupted,
            })
            .await?;
        }
        for tool in self.tool.values_mut().filter(|tool| !tool.finished) {
            let tool_outcome = match outcome {
                TurnOutcome::Cancelled => ToolOutcome::Cancelled,
                _ => ToolOutcome::Interrupted,
            };
            sink.send_wait(ProviderEvent::Tool {
                address: tool.address.clone(),
                update: ToolUpdate::Finished {
                    outcome: tool_outcome,
                },
            })
            .await?;
            tool.finished = true;
        }
        let threads: Vec<_> = self.turn.keys().cloned().collect();
        for thread in threads {
            self.finish_turn(&thread, outcome, sink).await?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::{BindingId, ExecutionOwner, InvocationScope};

    /// Keep provider error envelopes private while retaining genuine failed-tool output.
    #[tokio::test]
    async fn failed_tool_output_never_publishes_native_error_payload() {
        let scope = InvocationScope::new(ExecutionOwner::root("copilot-decoder-test"), "exchange");
        let (sink, mut stream) = forge_provider_api::events::channel(
            BindingId::new("copilot-decoder-binding").expect("binding id is valid"),
            scope.clone(),
        );
        let mut output = forge_provider_api::completion::ProviderCompletion {
            provider_session_id: Some(
                "copilot-native-session"
                    .to_owned()
                    .try_into()
                    .expect("fixture session id is valid"),
            ),
            ..Default::default()
        };
        let mut decoder = CopilotEventDecoder::new(scope, ".".into());
        let event = |id: &str, kind: &str, data: Value| -> SessionEvent {
            serde_json::from_value(serde_json::json!({
                "id":id,
                "type":kind,
                "timestamp":"2026-11-02T00:00:00Z",
                "data":data,
            }))
            .expect("fixture event has a valid SDK envelope")
        };

        decoder
            .decode(
                &event(
                    "turn-start",
                    "assistant.turn_start",
                    serde_json::json!({"turnId":"0"}),
                ),
                &mut output,
                &sink,
            )
            .await
            .unwrap();
        decoder
            .decode(
                &event(
                    "tool-result",
                    "tool.execution_complete",
                    serde_json::json!({
                        "toolCallId":"call-1",
                        "toolName":"shell",
                        "arguments":{"command":"safe fixture"},
                        "success":false,
                        "result":{"content":"visible failed-tool output"},
                        "error":{"code":"native-code-sentinel","message":"native-message-sentinel"}
                    }),
                ),
                &mut output,
                &sink,
            )
            .await
            .unwrap();

        let mut visible_output = false;
        while let Ok(event) = stream.try_recv() {
            let encoded = serde_json::to_string(&event.event).unwrap();
            assert!(!encoded.contains("native-code-sentinel"));
            assert!(!encoded.contains("native-message-sentinel"));
            visible_output |= encoded.contains("visible failed-tool output");
        }
        assert!(
            visible_output,
            "the normalized stream retains actual tool output"
        );
    }
}
