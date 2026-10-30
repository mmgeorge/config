use super::json_rpc::*;
use crate::backend::*;
use crate::control_tools::apply_invocation;
use anyhow::Result;
use serde_json::Value;
use std::collections::HashMap;

#[derive(Default)]
pub(super) struct CodexNormalization {
    scope: Option<EventScope>,
    message: HashMap<MessageAddress, NativeMessage>,
    tool: HashMap<ToolAddress, NativeTool>,
    checklist: HashMap<(TurnAddress, String), ProviderChecklistUpdate>,
    pub(super) replay_sensitive: bool,
}

#[derive(Default)]
struct NativeMessage {
    role: Option<MessageRole>,
    text: String,
    published: bool,
    completed: bool,
}

struct NativeTool {
    title: String,
    terminal: bool,
    kind: ToolActivityKind,
    has_changes: bool,
}

impl CodexNormalization {
    pub(super) fn bind(&mut self, scope: Option<impl Into<EventScope>>) {
        *self = Self {
            scope: scope.map(Into::into),
            ..Default::default()
        };
    }

    fn address(&self, params: &Value) -> Option<TurnAddress> {
        Some(TurnAddress {
            scope: self.scope.as_ref()?.invocation()?.clone(),
            thread_id: pointer_string(
                params,
                &[
                    "/threadId",
                    "/thread_id",
                    "/turn/threadId",
                    "/turn/thread_id",
                ],
            )?,
            turn_id: pointer_string(params, &["/turnId", "/turn_id", "/turn/id"])?.into(),
        })
    }

    fn required_address(&self, method: &str, params: &Value) -> Result<TurnAddress> {
        self.address(params).ok_or_else(|| {
            BackendError::Protocol {
                message: format!("Codex {method} omitted its required thread or turn identity"),
            }
            .into()
        })
    }

    fn required_item_id(method: &str, params: &Value) -> Result<String> {
        pointer_string(params, &["/itemId", "/item_id", "/item/id"]).ok_or_else(|| {
            BackendError::Protocol {
                message: format!("Codex {method} omitted its required item identity"),
            }
            .into()
        })
    }

    fn text(
        &mut self,
        address: MessageAddress,
        classification: Option<MessageClassification>,
        text: Option<String>,
        completed: bool,
    ) -> Result<Vec<BackendEvent>> {
        let state = self.message.entry(address.clone()).or_default();
        let mut events = Vec::new();
        if let Some(classification) = classification
            && let Some(previous) = state.role
            && classification != MessageClassification::Known(previous)
        {
            return Err(BackendError::Protocol {
                message: format!("Codex message {:?} changed its known role from {previous:?} to {classification:?}", address.message_id),
            }.into());
        }
        if state.completed
            && let Some(text) = &text
            && ((completed && text != &state.text) || (!completed && !text.is_empty()))
        {
            return Err(BackendError::Protocol {
                message: format!(
                    "Codex message {:?} changed text after completion",
                    address.message_id
                ),
            }
            .into());
        }
        if let Some(MessageClassification::Known(role)) = classification {
            if state.published && state.role.is_none() {
                events.push(BackendEvent::Message {
                    address: address.clone(),
                    update: MessageUpdate::Classified { role },
                });
            }
            state.role = Some(role);
        }
        let Some(text) = text else { return Ok(events) };
        if state.completed {
            return Ok(events);
        }
        if completed {
            state.text = text.clone();
        } else {
            state.text.push_str(&text);
        }
        if !state.published && !state.text.is_empty() {
            state.published = true;
            self.replay_sensitive = true;
            events.push(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Started {
                    classification: state
                        .role
                        .map(MessageClassification::Known)
                        .unwrap_or(MessageClassification::Unclassified),
                    text: state.text.clone(),
                },
            });
        } else if state.published && !completed && !text.is_empty() {
            self.replay_sensitive = true;
            events.push(BackendEvent::Message {
                address: address.clone(),
                update: MessageUpdate::Delta { text: text.clone() },
            });
        }
        if completed {
            state.completed = true;
            if state.published {
                events.push(BackendEvent::Message {
                    address,
                    update: MessageUpdate::Completed { text },
                });
            }
        }
        Ok(events)
    }

    pub(super) async fn normalize(
        &mut self,
        message: &Value,
        output: &mut BackendOutput,
        sink: Option<&BackendEventSink>,
        workspace: &str,
        apply_control: bool,
    ) -> Result<()> {
        let Some(event_scope) = self.scope.clone() else {
            return Ok(());
        };
        let method = message["method"].as_str().unwrap_or_default();
        let params = message.get("params").cloned().unwrap_or(Value::Null);
        if let Some(value) = available_compact_update(&params) {
            output.completion.metrics.native_compact_update = Some(value);
        }
        let address = self.address(&params);
        let mut events = Vec::new();
        let item_type = params
            .pointer("/item/type")
            .and_then(Value::as_str)
            .unwrap_or_default();
        if matches!(method, "item/started" | "item/completed") && item_type == "contextCompaction" {
            {
                let id = Self::required_item_id(method, &params)?;
                let thread_id =
                    pointer_string(&params, &["/threadId", "/thread_id"]).ok_or_else(|| {
                        BackendError::Protocol {
                            message: format!(
                                "Codex {method} compaction omitted its required thread identity"
                            ),
                        }
                    })?;
                events.push(BackendEvent::Compaction {
                    scope: event_scope.clone(),
                    update: Box::new(Compaction {
                        id,
                        thread_id,
                        state: if method == "item/started" {
                            CompactionState::Running
                        } else {
                            CompactionState::Completed
                        },
                        manual: event_scope.invocation().is_none(),
                        started_at_ms: None,
                        completed_at_ms: None,
                        result: CompactionResult::default(),
                    }),
                });
            }
            for event in events {
                if let Some(sink) = sink {
                    sink.send_wait(event).await?;
                } else {
                    output.event.push(event);
                }
            }
            return Ok(());
        }
        let Some(scope) = event_scope.invocation().cloned() else {
            if let Some(usage) = context_usage(&params) {
                output.completion.metrics.context_usage = Some(usage);
            }
            if method.eq_ignore_ascii_case("error") {
                let event = BackendEvent::Diagnostic {
                    scope: event_scope,
                    diagnostic: ProviderDiagnostic {
                        code: None,
                        message: first_text(&params).unwrap_or_else(|| params.to_string()),
                        fatal: true,
                    },
                };
                if let Some(sink) = sink {
                    sink.send_wait(event).await?;
                } else {
                    output.event.push(event);
                }
            }
            return Ok(());
        };
        if method.eq_ignore_ascii_case("item/agentMessage/delta")
            || (matches!(method, "item/started" | "item/completed")
                && item_type.eq_ignore_ascii_case("agentMessage"))
        {
            {
                let turn = self.required_address(method, &params)?;
                let id = Self::required_item_id(method, &params)?;
                let phase = params
                    .pointer("/item/phase")
                    .or_else(|| params.get("phase"));
                let phase = match phase {
                    None | Some(Value::Null) => None,
                    Some(Value::String(phase)) => Some(phase.as_str()),
                    Some(_) => {
                        return Err(BackendError::Protocol {
                            message: format!("Codex {method} supplied a non-string message phase"),
                        }
                        .into());
                    }
                };
                let classification = match phase {
                    Some("commentary") => {
                        Some(MessageClassification::Known(MessageRole::Commentary))
                    }
                    Some("final_answer") => {
                        Some(MessageClassification::Known(MessageRole::Response))
                    }
                    None => None,
                    Some(phase) => {
                        events.push(BackendEvent::Diagnostic {
                            scope: scope.clone().into(),
                            diagnostic: ProviderDiagnostic {
                                code: Some("unknown_message_phase".into()),
                                message: format!("Unsupported Codex message phase: {phase}"),
                                fatal: false,
                            },
                        });
                        Some(MessageClassification::Unclassified)
                    }
                };
                let completed = method == "item/completed";
                let text = if method.eq_ignore_ascii_case("item/agentMessage/delta") {
                    params["delta"].as_str().map(str::to_owned)
                } else {
                    params
                        .pointer("/item/text")
                        .and_then(Value::as_str)
                        .map(str::to_owned)
                        .or_else(|| completed.then(String::new))
                };
                events.extend(self.text(
                    MessageAddress {
                        turn,
                        message_id: MessageId::Native(id),
                    },
                    classification,
                    text,
                    completed,
                )?);
            }
        } else if matches!(
            method,
            "item/reasoning/summaryTextDelta"
                | "item/reasoning/textDelta"
                | "item/reasoning/summaryPartAdded"
        ) || (matches!(method, "item/started" | "item/completed")
            && item_type == "reasoning")
        {
            {
                let turn = self.required_address(method, &params)?;
                let id = Self::required_item_id(method, &params)?;
                let completed = method == "item/completed";
                if method.eq_ignore_ascii_case("item/reasoning/summaryTextDelta") {
                    let index = params["summaryIndex"].as_u64().unwrap_or(0);
                    events.extend(self.text(
                        MessageAddress {
                            turn,
                            message_id: MessageId::Synthetic(format!("{id}:summary:{index}")),
                        },
                        Some(MessageClassification::Known(MessageRole::ThoughtSummary)),
                        params["delta"].as_str().map(str::to_owned),
                        false,
                    )?);
                } else if method.eq_ignore_ascii_case("item/reasoning/textDelta") {
                    let index = params["contentIndex"].as_u64().unwrap_or(0);
                    events.extend(self.text(
                        MessageAddress {
                            turn,
                            message_id: MessageId::Synthetic(format!("{id}:thought:{index}")),
                        },
                        Some(MessageClassification::Known(MessageRole::Thought)),
                        params["delta"].as_str().map(str::to_owned),
                        false,
                    )?);
                } else if matches!(method, "item/started" | "item/completed") {
                    for (field, role, suffix) in [
                        ("summary", MessageRole::ThoughtSummary, "summary"),
                        ("content", MessageRole::Thought, "thought"),
                    ] {
                        for (index, content) in params["item"][field]
                            .as_array()
                            .into_iter()
                            .flatten()
                            .enumerate()
                        {
                            let text = content
                                .as_str()
                                .or_else(|| content["text"].as_str())
                                .map(str::to_owned);
                            events.extend(self.text(
                                MessageAddress {
                                    turn: turn.clone(),
                                    message_id: MessageId::Synthetic(format!(
                                        "{id}:{suffix}:{index}"
                                    )),
                                },
                                Some(MessageClassification::Known(role)),
                                text,
                                completed,
                            )?);
                        }
                    }
                }
            }
        } else if matches!(method, "turn/started" | "turn/completed") {
            {
                let address = self.required_address(method, &params)?;
                let now = timestamp();
                let update = if method == "turn/started" {
                    TurnUpdate::Started { started_at_ms: now }
                } else {
                    let outcome = match params.pointer("/turn/status").and_then(Value::as_str) {
                        Some("failed") => crate::turn::TurnOutcome::Failed,
                        Some("interrupted") => crate::turn::TurnOutcome::Interrupted,
                        _ => crate::turn::TurnOutcome::Completed,
                    };
                    let pending = self
                        .message
                        .iter()
                        .filter(|(message, state)| {
                            message.turn == address && state.published && !state.completed
                        })
                        .map(|(message, _)| message.clone())
                        .collect::<Vec<_>>();
                    for message in pending {
                        let state = self.message.get_mut(&message).expect("known Codex message");
                        state.completed = true;
                        events.push(BackendEvent::Message {
                            address: message,
                            update: if outcome == crate::turn::TurnOutcome::Completed {
                                MessageUpdate::Completed {
                                    text: state.text.clone(),
                                }
                            } else {
                                MessageUpdate::Interrupted
                            },
                        });
                    }
                    TurnUpdate::Finished {
                        outcome,
                        completed_at_ms: now,
                    }
                };
                events.push(BackendEvent::Turn { address, update });
            }
        } else if method.to_ascii_lowercase().contains("tokenusage") {
            if let (Some(address), Some(update)) = (address.clone(), usage_update(&params)) {
                events.push(BackendEvent::Usage { address, update });
            }
            if let Some(usage) = context_usage(&params) {
                output.completion.metrics.context_usage = Some(usage.clone());
                events.push(BackendEvent::Runtime {
                    scope: scope.clone(),
                    update: RuntimeUpdate::ContextUsage { usage },
                });
            }
        } else {
            let lifecycle = normalize_agent_lifecycle(method, &params);
            if !lifecycle.is_empty() {
                events.extend(lifecycle.into_iter().map(|event| BackendEvent::Agent {
                    scope: scope.clone(),
                    update: AgentUpdate::Lifecycle { event },
                }));
            } else if is_tool_lifecycle_event(&method.to_ascii_lowercase(), &params) {
                {
                    let turn = self.required_address(method, &params)?;
                    let snapshot = normalize_tool_activity(method, &params, workspace)
                        .map_err(|error| BackendError::Protocol {
                            message: format!("{error:#}"),
                        })?
                        .ok_or_else(|| BackendError::Protocol {
                            message: format!("Codex {method} omitted its required tool identity"),
                        })?;
                    if apply_control && let Some(invocation) = control_invocation(method, &params)?
                    {
                        apply_invocation(&invocation, &mut output.completion)?;
                    }
                    self.replay_sensitive = true;
                    output.completion.evidence.tool_called = true;
                    output.completion.evidence.workspace_changed |=
                        successful_file_change(&snapshot);
                    let address = ToolAddress {
                        turn,
                        tool_id: snapshot.id.clone().into(),
                    };
                    let first = !self.tool.contains_key(&address);
                    let native = self
                        .tool
                        .entry(address.clone())
                        .or_insert_with(|| NativeTool {
                            title: String::new(),
                            terminal: false,
                            kind: snapshot.kind,
                            has_changes: false,
                        });
                    native.has_changes |= !snapshot.change.is_empty();
                    if native.kind == ToolActivityKind::FileChange
                        && native.has_changes
                        && snapshot.status == Some(ToolStatus::Succeeded)
                    {
                        output.completion.evidence.workspace_changed = true;
                    }
                    if first
                        || (native.title != snapshot.title
                            && !matches!(
                                snapshot.title.as_str(),
                                "tool" | "command" | "file changes"
                            ))
                    {
                        native.title = snapshot.title.clone();
                        events.push(BackendEvent::Tool {
                            address: address.clone(),
                            update: ToolUpdate::Requested {
                                invocation: ToolInvocation {
                                    kind: snapshot.kind,
                                    name: snapshot.name,
                                    title: snapshot.title,
                                    arguments: snapshot.arguments,
                                },
                            },
                        });
                    }
                    if first {
                        events.push(BackendEvent::Tool {
                            address: address.clone(),
                            update: ToolUpdate::Started,
                        });
                    }
                    if let Some(text) = snapshot.output {
                        events.push(BackendEvent::Tool {
                            address: address.clone(),
                            update: ToolUpdate::Output {
                                update: if snapshot.output_delta {
                                    TextUpdate::Append(text)
                                } else {
                                    TextUpdate::Replace(text)
                                },
                            },
                        });
                    }
                    if !snapshot.change.is_empty() {
                        events.push(BackendEvent::Tool {
                            address: address.clone(),
                            update: ToolUpdate::Changes {
                                changes: snapshot.change,
                            },
                        });
                    }
                    if !native.terminal
                        && let Some(status) = snapshot.status
                    {
                        let outcome = match status {
                            ToolStatus::Running => None,
                            ToolStatus::Succeeded => Some(ToolOutcome::Succeeded),
                            ToolStatus::Failed => Some(ToolOutcome::Failed {
                                diagnostic: ToolDiagnostic {
                                    code: None,
                                    message: "Codex tool failed".into(),
                                },
                            }),
                            ToolStatus::Denied => Some(ToolOutcome::Denied {
                                reason: "Codex tool denied".into(),
                            }),
                            ToolStatus::Cancelled => Some(ToolOutcome::Cancelled),
                            ToolStatus::Interrupted => Some(ToolOutcome::Interrupted),
                        };
                        if let Some(outcome) = outcome {
                            native.terminal = true;
                            events.push(BackendEvent::Tool {
                                address,
                                update: ToolUpdate::Finished { outcome },
                            });
                        }
                    }
                }
            } else if method.to_ascii_lowercase().contains("plan")
                && let (Some(address), Some(mut update)) =
                    (address, normalize_checklist_update(method, &params))
            {
                let key = (address.clone(), update.scope_id.clone());
                if let Some(previous) = self.checklist.get(&key) {
                    if update.name.is_none() {
                        update.name = previous.name.clone();
                    }
                    if !update.replace_entries {
                        update.entry_list = previous.entry_list.clone();
                    }
                }
                self.replay_sensitive = true;
                self.checklist.insert(key, update.clone());
                events.push(BackendEvent::Checklist { address, update });
            } else if method.eq_ignore_ascii_case("error")
                || item_type.eq_ignore_ascii_case("error")
            {
                let retryable = params
                    .get("error")
                    .and_then(super::retry::transient_code)
                    .is_some();
                if !retryable || self.replay_sensitive {
                    events.push(BackendEvent::Diagnostic {
                        scope: scope.clone().into(),
                        diagnostic: ProviderDiagnostic {
                            code: None,
                            message: first_text(&params).unwrap_or_else(|| params.to_string()),
                            fatal: true,
                        },
                    });
                }
            }
        }
        if let Some(state) = output
            .completion
            .backend_session_id
            .as_deref()
            .and_then(|thread| native_goal_state(message, thread))
        {
            output.completion.evidence.native_state = Some(state);
        }
        for event in events {
            if let Some(sink) = sink {
                sink.send_wait(event).await?;
            } else {
                output.event.push(event);
            }
        }
        Ok(())
    }
}

fn timestamp() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

#[cfg(test)]
mod test {
    use super::*;
    use serde_json::json;

    fn scope() -> InvocationScope {
        InvocationScope::new(ExecutionOwner::root("session"), "exchange")
    }

    fn message_address(scope: InvocationScope) -> MessageAddress {
        MessageAddress {
            turn: TurnAddress {
                scope,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            message_id: MessageId::Native("message".into()),
        }
    }

    #[test]
    fn completed_text_is_immutable_and_duplicate_snapshots_are_idempotent() -> Result<()> {
        let mut normalization = CodexNormalization::default();
        let address = message_address(scope());
        normalization.text(
            address.clone(),
            Some(MessageClassification::Known(MessageRole::Response)),
            Some("partial".into()),
            false,
        )?;
        normalization.text(address.clone(), None, Some("authoritative".into()), true)?;
        assert!(
            normalization
                .text(address.clone(), None, Some("authoritative".into()), true)?
                .is_empty()
        );
        for (text, completed) in [("changed", true), ("late delta", false)] {
            let error = normalization
                .text(address.clone(), None, Some(text.into()), completed)
                .unwrap_err();
            assert!(matches!(
                BackendError::from(error),
                BackendError::Protocol { .. }
            ));
            assert_eq!(normalization.message[&address].text, "authoritative");
        }
        let empty = MessageAddress {
            message_id: MessageId::Native("empty".into()),
            ..address
        };
        assert!(
            normalization
                .text(empty.clone(), None, Some(String::new()), true)?
                .is_empty()
        );
        assert!(
            normalization
                .text(empty, None, Some("revived".into()), true)
                .is_err()
        );
        Ok(())
    }

    #[test]
    fn known_classification_cannot_change_after_publication() -> Result<()> {
        let mut normalization = CodexNormalization::default();
        let address = message_address(scope());
        normalization.text(
            address.clone(),
            Some(MessageClassification::Unclassified),
            Some("working".into()),
            false,
        )?;
        let classified = normalization.text(
            address.clone(),
            Some(MessageClassification::Known(MessageRole::Commentary)),
            None,
            false,
        )?;
        assert!(matches!(
            classified.as_slice(),
            [BackendEvent::Message {
                update: MessageUpdate::Classified {
                    role: MessageRole::Commentary
                },
                ..
            }]
        ));
        for classification in [
            MessageClassification::Known(MessageRole::Response),
            MessageClassification::Unclassified,
        ] {
            let error = normalization
                .text(address.clone(), Some(classification), None, false)
                .unwrap_err();
            assert!(matches!(
                BackendError::from(error),
                BackendError::Protocol { .. }
            ));
            assert_eq!(
                normalization.message[&address].role,
                Some(MessageRole::Commentary)
            );
        }
        Ok(())
    }

    #[tokio::test]
    async fn recognized_content_requires_native_identity_but_inactive_scopes_do_not() -> Result<()>
    {
        for message in [
            json!({"method":"item/agentMessage/delta","params":{"threadId":"thread","itemId":"message","delta":"text"}}),
            json!({"method":"item/agentMessage/delta","params":{"turnId":"turn","itemId":"message","delta":"text"}}),
            json!({"method":"item/agentMessage/delta","params":{"threadId":"thread","turnId":"turn","delta":"text"}}),
            json!({"method":"item/reasoning/summaryTextDelta","params":{"threadId":"thread","turnId":"turn","delta":"thought"}}),
            json!({"method":"item/started","params":{"threadId":"thread","turnId":"turn","item":{"type":"commandExecution","command":"check"}}}),
            json!({"method":"item/commandExecution/outputDelta","params":{"threadId":"thread","itemId":"command","delta":"output"}}),
        ] {
            let mut normalization = CodexNormalization::default();
            let mut output = BackendOutput::default();
            normalization
                .normalize(&message, &mut output, None, "", false)
                .await?;
            normalization.bind(Some(EventScope::SessionOperation {
                owner: ExecutionOwner::root("session"),
                operation_id: "catalog".into(),
            }));
            normalization
                .normalize(&message, &mut output, None, "", false)
                .await?;
            normalization.bind(Some(scope()));
            let error = normalization
                .normalize(&message, &mut output, None, "", false)
                .await
                .unwrap_err();
            assert!(
                matches!(BackendError::from(error), BackendError::Protocol { .. }),
                "{message}"
            );
            assert!(output.event.is_empty());
            assert!(normalization.message.is_empty());
            assert!(normalization.tool.is_empty());
            assert!(!output.completion.evidence.tool_called);
        }
        let mut normalization = CodexNormalization::default();
        normalization.bind(Some(scope()));
        normalization.normalize(&json!({"method":"item/commandExecution/requestApproval","params":{"threadId":"thread","turnId":"turn","command":"check"}}), &mut BackendOutput::default(), None, "", false).await?;
        Ok(())
    }

    #[tokio::test]
    async fn unknown_and_missing_phases_remain_unclassified() -> Result<()> {
        for phase in [Value::Null, json!("future_phase"), json!("")] {
            let mut normalization = CodexNormalization::default();
            normalization.bind(Some(scope()));
            let mut output = BackendOutput::default();
            normalization
                .normalize(
                    &json!({"method":"item/started","params":{"threadId":"thread","turnId":"turn",
                "item":{"type":"agentMessage","id":"message","phase":phase,"text":""}}}),
                    &mut output,
                    None,
                    "",
                    false,
                )
                .await?;
            normalization.normalize(&json!({"method":"item/agentMessage/delta","params":{"threadId":"thread","turnId":"turn","itemId":"message","delta":"text"}}), &mut output, None, "", false).await?;
            assert!(output.event.iter().any(|event| matches!(event, BackendEvent::Message { update: MessageUpdate::Started { classification: MessageClassification::Unclassified, text }, .. } if text == "text")));
            assert!(!output.event.iter().any(|event| matches!(
                event,
                BackendEvent::Message {
                    update: MessageUpdate::Classified { .. },
                    ..
                }
            )));
            assert_eq!(output.event.iter().filter(|event| matches!(event, BackendEvent::Diagnostic { diagnostic, .. } if diagnostic.code.as_deref() == Some("unknown_message_phase"))).count(), usize::from(!phase.is_null()));
        }
        Ok(())
    }

    #[tokio::test]
    async fn derived_reasoning_identity_cannot_alias_a_native_message() -> Result<()> {
        let mut normalization = CodexNormalization::default();
        normalization.bind(Some(scope()));
        let mut output = BackendOutput::default();
        for message in [
            json!({"method":"item/completed","params":{"threadId":"thread","turnId":"turn",
                "item":{"type":"agentMessage","id":"reasoning:summary:0","phase":"final_answer","text":"response"}}}),
            json!({"method":"item/reasoning/summaryTextDelta","params":{"threadId":"thread","turnId":"turn","itemId":"reasoning","summaryIndex":0,"delta":"summary"}}),
            json!({"method":"item/reasoning/textDelta","params":{"threadId":"thread","turnId":"turn","itemId":"reasoning","contentIndex":0,"delta":"thought"}}),
            json!({"method":"item/completed","params":{"threadId":"thread","turnId":"turn",
                "item":{"type":"reasoning","id":"reasoning","summary":["final summary"],"content":["final thought"]}}}),
        ] {
            normalization
                .normalize(&message, &mut output, None, "", false)
                .await?;
        }
        assert_eq!(normalization.message.len(), 3);
        for (id, expected, role) in [
            (
                MessageId::Native("reasoning:summary:0".into()),
                "response",
                MessageRole::Response,
            ),
            (
                MessageId::Synthetic("reasoning:summary:0".into()),
                "final summary",
                MessageRole::ThoughtSummary,
            ),
            (
                MessageId::Synthetic("reasoning:thought:0".into()),
                "final thought",
                MessageRole::Thought,
            ),
        ] {
            let state = normalization
                .message
                .iter()
                .find_map(|(address, state)| (address.message_id == id).then_some(state))
                .unwrap();
            assert_eq!(state.text, expected);
            assert_eq!(state.role, Some(role));
            assert!(state.completed);
        }
        Ok(())
    }
}
