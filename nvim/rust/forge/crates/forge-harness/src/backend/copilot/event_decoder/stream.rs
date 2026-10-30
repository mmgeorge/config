use std::collections::{HashMap, HashSet};

use github_copilot_sdk::SessionEvent;
use serde_json::Value;

use crate::backend::{
    AgentUpdate, BackendCompletion, BackendError, BackendEvent, BackendEventSink, InvocationScope,
    MessageAddress, MessageClassification, MessageId, MessageRole, MessageUpdate,
    ProviderDiagnostic, RuntimeUpdate, TextUpdate, ToolActivityKind, ToolAddress, ToolDiagnostic,
    ToolInvocation, ToolOutcome, ToolUpdate, TurnAddress, TurnUpdate,
};
use crate::turn::TurnOutcome;

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
    compaction: HashMap<String, crate::backend::Compaction>,
    observed: HashSet<String>,
    pending_checklist: Option<crate::backend::ProviderChecklistUpdate>,
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

    pub fn root_address(&self) -> Option<TurnAddress> {
        self.root_thread
            .as_ref()
            .and_then(|thread| self.turn.get(thread))
            .map(|turn| turn.address.clone())
    }

    fn protocol(message: impl Into<String>) -> BackendError {
        BackendError::Protocol {
            message: message.into(),
        }
    }

    fn thread(
        &self,
        event: &SessionEvent,
        output: &BackendCompletion,
    ) -> Result<String, BackendError> {
        event
            .agent_id
            .clone()
            .or_else(|| output.backend_session_id.clone())
            .ok_or_else(|| Self::protocol("Copilot event has no conversation identity"))
    }

    fn address(&self, thread: &str) -> Result<TurnAddress, BackendError> {
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
        sink: &BackendEventSink,
        code: &str,
        message: String,
    ) -> Result<(), BackendError> {
        sink.send_wait(BackendEvent::Diagnostic {
            scope: self.scope.clone().into(),
            diagnostic: ProviderDiagnostic {
                code: Some(code.into()),
                message,
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
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
        let Some(turn) = self.turn.get_mut(thread) else {
            return Ok(());
        };
        if !turn.settled {
            sink.send_wait(BackendEvent::Turn {
                address: turn.address.clone(),
                update: TurnUpdate::Finished {
                    outcome,
                    completed_at_ms: turn.completed_at_ms.unwrap_or_else(super::timestamp_ms),
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
        output: &mut BackendCompletion,
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
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
            let native_id =
                super::string_field(&event.data, &["turnId"]).unwrap_or_else(|| event.id.clone());
            let address = TurnAddress {
                scope: self.scope.clone(),
                thread_id: thread.clone(),
                turn_id: format!("native-child:{native_id}").into(),
            };
            sink.send_wait(BackendEvent::Turn {
                address: address.clone(),
                update: TurnUpdate::Started {
                    started_at_ms: super::timestamp_ms(),
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
                && let Some(model) = super::string_field(&event.data, &["model"])
                && !model.is_empty()
                && output.runtime.model.as_ref() != Some(&model)
            {
                output.runtime.model = Some(model);
                sink.send_wait(BackendEvent::Runtime {
                    scope: self.scope.clone(),
                    update: RuntimeUpdate::Resolved {
                        runtime: output.runtime.clone(),
                    },
                })
                .await?;
            }
        }
        match event.event_type.as_str() {
            "assistant.turn_start" => {
                self.finish_turn(&thread, TurnOutcome::Completed, sink)
                    .await?;
                let native_id = super::string_field(&event.data, &["turnId"])
                    .ok_or_else(|| Self::protocol("Copilot turn start omitted turnId"))?;
                let interaction = super::string_field(&event.data, &["interactionId"])
                    .unwrap_or_else(|| event.id.clone());
                let address = TurnAddress {
                    scope: self.scope.clone(),
                    thread_id: thread.clone(),
                    turn_id: format!("{interaction}:{native_id}").into(),
                };
                sink.send_wait(BackendEvent::Turn {
                    address: address.clone(),
                    update: TurnUpdate::Started {
                        started_at_ms: super::timestamp_ms(),
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
                    sink.send_wait(BackendEvent::Checklist { address, update })
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
                let completed_at_ms = super::timestamp_ms();
                turn.completed_at_ms = Some(completed_at_ms);
                sink.send_wait(BackendEvent::Turn {
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
                    sink.send_wait(BackendEvent::Tool {
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
                    super::unsigned_field(&event.data, &["totalResponseSizeBytes"]).ok_or_else(
                        || Self::protocol("Copilot streaming progress omitted byte count"),
                    )?;
                sink.send_wait(BackendEvent::Runtime {
                    scope: self.scope.clone(),
                    update: RuntimeUpdate::Progress { received_bytes },
                })
                .await?;
            }
            "assistant.idle" if event.agent_id.is_none() => {
                sink.send_wait(BackendEvent::Runtime {
                    scope: self.scope.clone(),
                    update: RuntimeUpdate::ForegroundIdle,
                })
                .await?;
            }
            "session.idle" => {}
            "session.usage_info" => {
                if let Some(usage) = super::unsigned_field(&event.data, &["currentTokens", "used"])
                    .zip(super::unsigned_field(&event.data, &["tokenLimit", "size"]))
                    .and_then(|(used, size)| crate::session::ContextUsage::reported(used, size))
                {
                    output.metrics.context_usage = Some(usage.clone());
                    sink.send_wait(BackendEvent::Runtime {
                        scope: self.scope.clone(),
                        update: RuntimeUpdate::ContextUsage { usage },
                    })
                    .await?;
                }
            }
            "assistant.usage" => {
                let update = crate::backend::usage::UsageUpdate {
                    id: super::string_field(&event.data, &["apiCallId"])
                        .unwrap_or_else(|| event.id.clone()),
                    usage: crate::backend::usage::TokenUsage {
                        input: super::unsigned_field(&event.data, &["inputTokens"]),
                        cached_input: super::unsigned_field(&event.data, &["cacheReadTokens"]),
                        reasoning: super::unsigned_field(&event.data, &["reasoningTokens"]),
                        output: super::unsigned_field(&event.data, &["outputTokens"]),
                    },
                    cumulative: None,
                    cumulative_total: None,
                };
                sink.send_wait(BackendEvent::Usage {
                    address: self.address(&thread)?,
                    update,
                })
                .await?;
            }
            "subagent.started" | "subagent.completed" | "subagent.failed" => {
                sink.send_wait(BackendEvent::Agent {
                    scope: self.scope.clone(),
                    update: AgentUpdate::Lifecycle {
                        event: super::agent_lifecycle(event),
                    },
                })
                .await?;
            }
            "session.compaction_start" | "session.compaction_complete" => {
                self.compaction(event, &thread, output, sink).await?;
            }
            "session.error" if !event.is_transient_error() => {
                self.diagnostic(
                    sink,
                    "copilot_session_error",
                    super::session_error(event, output.backend_session_id.as_deref()),
                )
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
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
        let id = event
            .data
            .get("messageId")
            .and_then(Value::as_str)
            .ok_or_else(|| Self::protocol("Copilot message omitted messageId"))?;
        let phase = CopilotPhaseMetadata::parse(event.data.get("phase").and_then(Value::as_str));
        if let CopilotPhaseMetadata::Unknown(value) = &phase {
            self.diagnostic(
                sink,
                "unknown_copilot_phase",
                format!("Unknown Copilot phase: {value}"),
            )
            .await?;
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
                sink.send_wait(BackendEvent::Message {
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
            sink.send_wait(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification,
                    text: message.text.clone(),
                },
            })
            .await?;
            message.published = true;
        } else if event.event_type == "assistant.message_delta" {
            sink.send_wait(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Delta { text: text.into() },
            })
            .await?;
        }
        if event.event_type == "assistant.message" {
            if let MessageClassification::Known(role) = classification {
                sink.send_wait(BackendEvent::Message {
                    address: address.clone(),
                    update: MessageUpdate::Classified { role },
                })
                .await?;
            }
            sink.send_wait(BackendEvent::Message {
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
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
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
            sink.send_wait(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification: MessageClassification::Known(MessageRole::ThoughtSummary),
                    text: message.text.clone(),
                },
            })
            .await?;
            message.published = true;
        } else if !complete {
            sink.send_wait(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Delta { text: text.into() },
            })
            .await?;
        }
        if complete {
            sink.send_wait(BackendEvent::Message {
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
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
        let id = super::string_field(request, &["toolCallId", "id"])
            .ok_or_else(|| Self::protocol("Copilot tool request omitted toolCallId"))?;
        let key = (thread.into(), id.clone());
        if self.tool.contains_key(&key) {
            return Ok(());
        }
        let name = super::string_field(request, &["name", "toolName"])
            .ok_or_else(|| Self::protocol("Copilot tool request omitted tool name"))?;
        let arguments = request.get("arguments").cloned().unwrap_or(Value::Null);
        let file_tool = matches!(name.as_str(), "apply_patch" | "edit" | "create")
            || name == "str_replace_editor"
                && matches!(
                    arguments.get("command").and_then(Value::as_str),
                    Some("str_replace" | "create" | "insert" | "undo_edit")
                );
        let invocation = ToolInvocation {
            kind: if file_tool {
                ToolActivityKind::FileChange
            } else {
                ToolActivityKind::ToolCall
            },
            title: ToolInvocation::call_title(&name, &arguments).unwrap_or_else(|| name.clone()),
            name,
            arguments,
        };
        let address = ToolAddress {
            turn: self.address(thread)?,
            tool_id: id.into(),
        };
        sink.send_wait(BackendEvent::Tool {
            address: address.clone(),
            update: ToolUpdate::Requested {
                invocation: invocation.clone(),
            },
        })
        .await?;
        let input = self.pending_input.remove(&key).unwrap_or_default();
        if !input.is_empty() {
            sink.send_wait(BackendEvent::Tool {
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
        output: &mut BackendCompletion,
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
        let id = super::string_field(&event.data, &["toolCallId", "id"])
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
                sink.send_wait(BackendEvent::Diagnostic {
                    scope: self.scope.clone().into(),
                    diagnostic: ProviderDiagnostic {
                        code: Some("copilot_shell_output_gap".into()),
                        message: format!(
                            "Copilot shell output sequence changed from {:?} to {sequence}",
                            tool.output_sequence
                        ),
                        fatal: false,
                    },
                })
                .await?;
            }
            tool.output_sequence = Some(sequence);
        }
        output.evidence.tool_called = true;
        if event.event_type == "tool.execution_start" {
            if !tool.started && !tool.finished {
                sink.send_wait(BackendEvent::Tool {
                    address: tool.address.clone(),
                    update: ToolUpdate::Started,
                })
                .await?;
                tool.started = true;
            }
            let changes = super::provider_change_set(&event.data);
            if !changes.is_empty() {
                tool.has_changes = true;
                sink.send_wait(BackendEvent::Tool {
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
                .or_else(|| event.data.pointer("/error/message"))
                .and_then(Value::as_str)
                .map(strip_ansi_escapes::strip_str)
        } else {
            super::string_field(
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
        let mut changes = super::provider_change_set(&event.data);
        if complete
            && succeeded
            && changes.is_empty()
            && !tool.has_changes
            && tool.invocation.kind == ToolActivityKind::FileChange
        {
            match super::super::file_change::from_output(
                text.as_deref().unwrap_or_default(),
                &self.workspace,
            ) {
                Ok(reported) => changes = reported,
                Err(error) => {
                    sink.send_wait(BackendEvent::Diagnostic {
                        scope: self.scope.clone().into(),
                        diagnostic: ProviderDiagnostic {
                            code: Some("invalid_copilot_file_changes".into()),
                            message: format!("Invalid Copilot file changes: {error:#}"),
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
            sink.send_wait(BackendEvent::Tool {
                address: tool.address.clone(),
                update: ToolUpdate::Output { update },
            })
            .await?;
        }
        if !changes.is_empty() {
            tool.has_changes = true;
            sink.send_wait(BackendEvent::Tool {
                address: tool.address.clone(),
                update: ToolUpdate::Changes { changes },
            })
            .await?;
        }
        output.evidence.workspace_changed |= complete && succeeded && tool.has_changes;
        if complete && !tool.finished {
            let outcome = if succeeded {
                ToolOutcome::Succeeded
            } else if super::super::rejection::is_denied(&event.data) {
                ToolOutcome::Denied {
                    reason: event
                        .data
                        .pointer("/error/message")
                        .and_then(Value::as_str)
                        .unwrap_or("Copilot denied this operation")
                        .into(),
                }
            } else {
                ToolOutcome::Failed {
                    diagnostic: ToolDiagnostic {
                        code: event
                            .data
                            .pointer("/error/code")
                            .and_then(Value::as_str)
                            .map(str::to_owned),
                        message: event
                            .data
                            .pointer("/error/message")
                            .and_then(Value::as_str)
                            .unwrap_or("Copilot tool failed without diagnostic details")
                            .into(),
                    },
                }
            };
            sink.send_wait(BackendEvent::Tool {
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
        _output: &mut BackendCompletion,
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
        let update = super::decode_compaction(event, thread, self.compaction.get(thread))?;
        self.compaction.insert(thread.into(), update.clone());
        sink.send_wait(BackendEvent::Compaction {
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
        _output: &mut BackendCompletion,
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
        let Some(update) = super::checklist_update(&data, scope_id) else {
            return Ok(());
        };
        if let Some(address) = self
            .root_thread
            .as_deref()
            .and_then(|thread| self.turn.get(thread))
            .map(|turn| turn.address.clone())
        {
            sink.send_wait(BackendEvent::Checklist { address, update })
                .await?;
        } else {
            self.pending_checklist = Some(update);
        }
        Ok(())
    }

    pub async fn finish_success(&mut self, sink: &BackendEventSink) -> Result<(), BackendError> {
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
            sink.send_wait(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification: MessageClassification::Known(MessageRole::Response),
                    text: text.clone(),
                },
            })
            .await?;
            sink.send_wait(BackendEvent::Message {
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
        sink: &BackendEventSink,
    ) -> Result<(), BackendError> {
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
            sink.send_wait(BackendEvent::Message {
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
            sink.send_wait(BackendEvent::Message {
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
            sink.send_wait(BackendEvent::Tool {
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
