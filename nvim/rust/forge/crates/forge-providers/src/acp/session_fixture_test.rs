use super::{AcpHandler, AcpKind, session::AcpSessionState};
use async_trait::async_trait;
use forge_provider_api::events;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, ProviderPermissions};
use forge_provider_api::{InvocationScope, ProviderError};
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};

#[derive(Default)]
struct FixturePermissions {
    observed: Mutex<Vec<PermissionRequest>>,
}

#[async_trait]
impl ProviderPermissions for FixturePermissions {
    async fn authorize(
        &self,
        _: &InvocationScope,
        request: PermissionRequest,
    ) -> PermissionResponse {
        self.observed.lock().unwrap().push(request);
        PermissionResponse::Allow
    }

    async fn before_tool(&self, _: &InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
}

async fn activate(
    kind: AcpKind,
) -> (
    Arc<AcpSessionState>,
    Arc<FixturePermissions>,
    events::ProviderEventStream,
) {
    let state = Arc::new(AcpSessionState::new_native(kind));
    let permission = Arc::new(FixturePermissions::default());
    let scope = InvocationScope::new(
        forge_provider_api::testing::root_thread("fixture"),
        "exchange",
    );
    let binding = forge_provider_api::BindingId::new("fixture-binding").unwrap();
    let (sink, stream) = events::channel(binding, scope.clone());
    state
        .begin_native("root", sink, permission.clone())
        .await
        .unwrap();
    (state, permission, stream)
}

fn drain(stream: &mut events::ProviderEventStream) -> Vec<forge_provider_api::ProviderEvent> {
    std::iter::from_fn(|| stream.try_next().transpose())
        .map(|event| event.map(|event| event.event))
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
}

#[tokio::test]
async fn native_message_and_tool_updates_publish_normalized_addresses_and_outcomes() {
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root","update":{
                "sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"wire answer"}
            }}),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"root","update":{
                "sessionUpdate":"tool_call","toolCallId":"edit-1","kind":"edit","title":"Edit file",
                "content":[{"type":"diff","path":"src/main.rs","oldText":"old\n","newText":"new\n"}]
            }}),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"root","update":{
                "sessionUpdate":"tool_call_update","toolCallId":"edit-1","status":"completed"
            }}),
        )
        .await
        .unwrap();
    state
        .finish(forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();

    let events = drain(&mut stream);
    assert!(!events.iter().any(|event| matches!(
        event,
        forge_provider_api::ProviderEvent::Turn {
            event: forge_provider_api::TurnEvent::Completed(_),
            ..
        }
    )));

    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Item { event: forge_provider_api::ItemEvent::Completed(forge_provider_api::ItemCompletion::AgentMessage(forge_provider_api::MessageCompletion { text, .. })), .. }
            if text == "wire answer")));
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Item { event: forge_provider_api::ItemEvent::Tool { event: forge_provider_api::ToolItemEvent::Changes(changes) }, .. }
            if changes.file.iter().any(|change| change.path == "src/main.rs"))));
}

#[tokio::test]
async fn native_permission_callback_receives_a_normalized_target_and_once_choice() {
    let (state, permission, _stream) = activate(AcpKind::Claude).await;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root","update":{
                "sessionUpdate":"tool_call","toolCallId":"edit-1","kind":"edit","title":"Edit",
                "rawInput":{"file_path":"src/main.rs"}
            }}),
        )
        .await
        .unwrap();
    let response = state.request("session/request_permission", json!({
        "sessionId":"root",
        "toolCall":{"toolCallId":"edit-1"},
        "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
    })).await.unwrap();
    assert_eq!(response["outcome"]["optionId"], "allow");
    let observed = permission.observed.lock().unwrap();
    assert_eq!(observed.len(), 1);
    assert!(
        matches!(observed[0].target_list.as_slice(), [forge_provider_api::permissions::PermissionTarget::Write { path }] if path == "src/main.rs")
    );
}

#[tokio::test]
async fn native_compaction_updates_keep_summary_and_operation_scope() {
    let state = Arc::new(AcpSessionState::new_native(AcpKind::Claude));
    let owner = forge_provider_api::testing::root_thread("fixture");
    let scope = forge_provider_api::EventScope::SessionOperation {
        thread_id: owner,
        operation_id: forge_provider_api::OperationId::new("compact-fixture").unwrap(),
    };
    let binding = forge_provider_api::BindingId::new("fixture-binding").unwrap();
    let (sink, mut stream) = events::channel(binding, scope.clone());
    state.begin_compaction("root", sink).await.unwrap();
    for update in [
        json!({"sessionUpdate":"compaction_update","compactionId":"compact-1","status":"in_progress","_meta":{"trigger":"manual"}}),
        json!({"sessionUpdate":"compaction_summary_chunk","compactionId":"compact-1","content":{"type":"text","text":"kept summary"}}),
        json!({"sessionUpdate":"compaction_update","compactionId":"compact-1","status":"completed"}),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root","update":update}),
            )
            .await
            .unwrap();
    }
    state.finish_compaction().await;
    let events = drain(&mut stream);
    assert!(events.iter().all(|event| matches!(event, forge_provider_api::ProviderEvent::Thread { thread_id, .. } if thread_id == &scope.thread_id().clone())));
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Thread { thread_id, event: forge_provider_api::ThreadEvent::CompactionUpdated(update) }
            if update.thread_id == thread_id.as_str() && update.result.summary_content.as_deref() == Some("kept summary") && update.manual)));
}

#[test]
fn native_prompt_response_fixture_requires_a_valid_v1_prompt_result() {
    assert!(super::validate_prompt_response(json!({"stopReason":"end_turn"})).is_ok());
    assert!(super::validate_prompt_response(Value::Null).is_err());
}

/// Native plan replacements keep one owning turn and reject malformed entry states.
#[tokio::test]
async fn native_plan_updates_preserve_owner_and_replace_entries() {
    for kind in [AcpKind::Claude, AcpKind::Antigravity] {
        let (state, _, mut stream) = activate(kind).await;
        for entries in [
            json!([{"content":"First", "priority":"high", "status":"in_progress"}]),
            json!([{"content":"First", "priority":"high", "status":"completed"},
                {"content":"Second", "priority":"low", "status":"pending"}]),
            json!([]),
        ] {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"root", "update":{
                        "sessionUpdate":"plan", "entries":entries,
                    }}),
                )
                .await
                .unwrap();
        }
        let events = drain(&mut stream);
        let plans = events
            .iter()
            .filter_map(|event| match event {
                forge_provider_api::ProviderEvent::Turn {
                    thread_id,
                    turn_id,
                    event: forge_provider_api::TurnEvent::ChecklistUpdated(update),
                } => Some(((thread_id, turn_id), update)),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(plans.len(), 3);
        assert!(plans.iter().all(|(address, update)| *address == plans[0].0
            && update.scope_id == plans[0].1.scope_id
            && update.replace_entries));
        assert_eq!(
            plans[0].1.entry_list[0].status,
            forge_provider_api::ChecklistStatus::InProgress
        );
        assert_eq!(plans[1].1.entry_list.len(), 2);
        assert_eq!(plans[1].1.entry_list[1].provider_ordinal, 1);
        assert_eq!(plans[1].1.entry_list[1].priority.as_deref(), Some("low"));
        assert!(plans[2].1.entry_list.is_empty());
        assert!(state.notification("session/update", json!({"sessionId":"root", "update":{
            "sessionUpdate":"plan", "entries":[{"content":"Invalid", "priority":"high", "status":"future"}],
        }})).await.is_err());
        assert!(matches!(
            stream.try_next(),
            Err(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Protocol
            ))
        ));
    }
}

/// Descriptor changes may refine the original item but cannot replace its category.
#[tokio::test]
async fn native_descriptor_updates_reject_item_category_changes() {
    for kind in [AcpKind::Claude, AcpKind::Antigravity] {
        let (state, _, mut stream) = activate(kind).await;
        for (event, category, title) in [
            ("tool_call", "edit", "Edit file"),
            ("tool_call_update", "move", "Move file"),
        ] {
            state.notification("session/update", json!({"sessionId":"root", "update":{
                "sessionUpdate":event, "toolCallId":"owned-tool", "kind":category, "title":title,
            }})).await.unwrap();
        }
        let events = drain(&mut stream);
        let descriptors = events
            .iter()
            .filter_map(|event| match event {
                forge_provider_api::ProviderEvent::Item {
                    item_id,
                    event:
                        forge_provider_api::ItemEvent::Started(
                            forge_provider_api::ItemStart::FileChange(descriptor),
                        ),
                    ..
                }
                | forge_provider_api::ProviderEvent::Item {
                    item_id,
                    event:
                        forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::DescriptorUpdated(descriptor),
                        },
                    ..
                } => Some((item_id, descriptor)),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].0, descriptors[1].0);
        assert_eq!(descriptors[1].1.title, "Move file");
        assert!(state.notification("session/update", json!({"sessionId":"root", "update":{
            "sessionUpdate":"tool_call_update", "toolCallId":"owned-tool", "kind":"execute",
        }})).await.is_err());
        assert!(matches!(
            stream.try_next(),
            Err(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict
            ))
        ));
    }
}

/// Native result replay retains one report identity for both token and timing observations.
#[tokio::test]
async fn claude_result_accounting_uses_one_identified_report_for_tokens_and_timing() {
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    let result = json!({"sessionId":"root", "message":{
        "type":"result", "uuid":"native-report", "duration_ms":15,
        "usage":{"input_tokens":1, "cache_read_input_tokens":0,
            "cache_creation_input_tokens":0, "output_tokens":2},
    }});
    for _ in 0..2 {
        state
            .notification("_claude/sdkMessage", result.clone())
            .await
            .unwrap();
    }
    let events = drain(&mut stream);
    let reports = events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Turn {
                event: forge_provider_api::TurnEvent::Usage(report),
                ..
            } => Some(report),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(reports.len(), 2);
    assert!(
        reports
            .iter()
            .all(|report| report.report_id().as_str() == "native-report"
                && *report.reported_duration_ms() == Some(15)
                && report.usage().as_ref().unwrap().input == Some(1)
                && report.usage().as_ref().unwrap().output == Some(2))
    );
}

/// Missing accounting identity must be observable instead of silently dropping token evidence.
#[tokio::test]
async fn claude_result_accounting_rejects_missing_or_blank_report_identity() {
    for uuid in [Value::Null, json!(""), json!(" ")] {
        let (state, _, mut stream) = activate(AcpKind::Claude).await;
        assert!(
            state
                .notification(
                    "_claude/sdkMessage",
                    json!({"sessionId":"root", "message":{
                        "type":"result", "uuid":uuid, "duration_ms":15,
                        "usage":{"input_tokens":1,"output_tokens":2},
                    }})
                )
                .await
                .is_err()
        );
        assert!(matches!(
            stream.try_next(),
            Err(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Protocol
            ))
        ));
    }
}

/// Pending tools acquire their immutable kind before admission and require positive execution evidence.
#[tokio::test]
async fn pending_tool_refines_descriptor_without_starting_execution_or_duplicating_items() {
    use forge_provider_api::*;
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    for update in [
        json!({"sessionUpdate":"tool_call", "toolCallId":"tool", "title":"Pending", "status":"pending", "rawInput":{"path":"kept"}}),
        json!({"sessionUpdate":"tool_call_update", "toolCallId":"tool", "kind":"execute"}),
        json!({"sessionUpdate":"tool_call_update", "toolCallId":"tool", "title":"Run command"}),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root", "update":update}),
            )
            .await
            .unwrap();
    }
    let pending = drain(&mut stream);
    let starts = pending
        .iter()
        .filter_map(|event| match event {
            ProviderEvent::Item {
                item_id,
                event: ItemEvent::Started(ItemStart::CommandExecution(descriptor)),
                ..
            } => Some((item_id, descriptor)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(starts.len(), 1);
    assert_eq!(starts[0].1.arguments.value()["path"], "kept");
    assert!(!pending.iter().any(|event| matches!(
        event,
        ProviderEvent::Item {
            event: ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted
            },
            ..
        }
    )));
    assert!(pending.iter().any(|event| matches!(event, ProviderEvent::Item { item_id, event: ItemEvent::Tool { event: ToolItemEvent::DescriptorUpdated(descriptor) }, .. } if item_id == starts[0].0 && descriptor.title == "Run command")));
    for status in ["in_progress", "in_progress", "completed", "completed"] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root", "update":{
                    "sessionUpdate":"tool_call_update", "toolCallId":"tool", "status":status
                }}),
            )
            .await
            .unwrap();
    }
    let completed = drain(&mut stream);
    assert_eq!(
        completed
            .iter()
            .filter(|event| matches!(
                event,
                ProviderEvent::Item {
                    event: ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted
                    },
                    ..
                }
            ))
            .count(),
        1
    );
    assert_eq!(
        completed
            .iter()
            .filter(|event| matches!(
                event,
                ProviderEvent::Item {
                    event: ItemEvent::Completed(ItemCompletion::CommandExecution(_)),
                    ..
                }
            ))
            .count(),
        1
    );
}

/// A tool that completes without reporting `in_progress` still executed, so it gets a start.
///
/// Claude reports its own tools (ToolSearch) as `pending` and then `completed`. Without a start,
/// Harness has no execution time and renders the duration column as an unknown placeholder.
#[tokio::test]
async fn tool_completed_from_pending_reports_its_execution_start() {
    use forge_provider_api::*;
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    for (tool, terminal) in [
        ("searched", "completed"),
        ("refused", "failed"),
        ("blocked", "cancelled"),
    ] {
        for update in [
            json!({"sessionUpdate":"tool_call", "toolCallId":tool, "title":"ToolSearch", "kind":"other", "status":"pending"}),
            json!({"sessionUpdate":"tool_call_update", "toolCallId":tool, "status":terminal}),
        ] {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"root", "update":update}),
                )
                .await
                .unwrap();
        }
        let events = drain(&mut stream);
        let started = events.iter().position(|event| {
            matches!(
                event,
                ProviderEvent::Item {
                    event: ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted
                    },
                    ..
                }
            )
        });
        let completed = events
            .iter()
            .position(|event| {
                matches!(
                    event,
                    ProviderEvent::Item {
                        event: ItemEvent::Completed(_),
                        ..
                    }
                )
            })
            .expect("terminal tool status must complete its item");
        if terminal == "cancelled" {
            assert!(started.is_none(), "a cancelled pending tool never executed");
        } else {
            assert!(
                started.is_some_and(|started| started < completed),
                "{terminal} tool must start before it completes"
            );
        }
    }
}

/// ACP thoughts remain reasoning items while repeated chunks retain their original item identity.
#[tokio::test]
async fn reasoning_and_prose_chunks_keep_distinct_items_and_interrupted_content() {
    use forge_provider_api::*;
    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    for (kind, text) in [
        ("agent_thought_chunk", "think"),
        ("agent_thought_chunk", " more"),
        ("agent_message_chunk", "partial"),
        ("agent_message_chunk", " answer"),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root", "update":{
                    "sessionUpdate":kind, "content":{"type":"text", "text":text}
                }}),
            )
            .await
            .unwrap();
    }
    state.finish(turn::TurnOutcome::Interrupted).await.unwrap();
    let events = drain(&mut stream);
    let starts = events
        .iter()
        .filter(|event| {
            matches!(
                event,
                ProviderEvent::Item {
                    event: ItemEvent::Started(_),
                    ..
                }
            )
        })
        .count();
    assert_eq!(starts, 2);
    assert!(events.iter().any(|event| matches!(
        event,
        ProviderEvent::Item {
            event: ItemEvent::Started(ItemStart::Reasoning(ReasoningStart {
                kind: ReasoningKind::Full,
                ..
            })),
            ..
        }
    )));
    for (expected, reasoning) in [("think more", true), ("partial answer", false)] {
        assert!(events.iter().any(|event| match event {
            ProviderEvent::Item {
                event: ItemEvent::Completed(ItemCompletion::Reasoning(completion)),
                ..
            } if reasoning =>
                completion.text == expected && completion.outcome == MessageOutcome::Interrupted,
            ProviderEvent::Item {
                event: ItemEvent::Completed(ItemCompletion::AgentMessage(completion)),
                ..
            } if !reasoning =>
                completion.text == expected && completion.outcome == MessageOutcome::Interrupted,
            _ => false,
        }));
    }
    assert!(!events.iter().any(|event| matches!(
        event,
        ProviderEvent::Turn {
            event: TurnEvent::Completed(_),
            ..
        }
    )));
}

/// Parent completion does not finish or reassign native child streams when a root successor begins.
#[tokio::test]
async fn child_items_keep_original_turn_after_root_completion_and_successor() {
    use forge_provider_api::*;
    let state = AcpSessionState::new_native(AcpKind::Claude);
    let owner = forge_provider_api::testing::root_thread("child-fixture");
    let (sink, mut stream) = events::channel(
        BindingId::new("children").unwrap(),
        EventScope::Binding {
            thread_id: owner.clone(),
        },
    );
    let first = InvocationScope::new(owner.clone(), "first");
    state
        .begin("root", sink.invocation_sink(&first).unwrap())
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"subagent_spawned", "subagentSessionId":"child"
            }}),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"child", "update":{
                "sessionUpdate":"agent_message_chunk", "content":{"type":"text", "text":"first"}
            }}),
        )
        .await
        .unwrap();
    state
        .finish(turn::TurnOutcome::Completed {
            stop_reason: turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();
    let first_events = drain(&mut stream);
    let original = first_events
        .iter()
        .find_map(|event| match event {
            ProviderEvent::Item {
                thread_id,
                turn_id,
                item_id,
                event: ItemEvent::Started(_),
                ..
            } => Some((thread_id.clone(), turn_id.clone(), item_id.clone())),
            _ => None,
        })
        .unwrap();
    assert!(!first_events.iter().any(|event| matches!(
        event,
        ProviderEvent::Item {
            event: ItemEvent::Completed(_),
            ..
        }
    )));
    let next = InvocationScope::new(owner, "next");
    state
        .begin("root", sink.invocation_sink(&next).unwrap())
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"child", "update":{
                "sessionUpdate":"agent_message_chunk", "content":{"type":"text", "text":" late"}
            }}),
        )
        .await
        .unwrap();
    let late = drain(&mut stream);
    assert!(late.iter().any(|event| matches!(event, ProviderEvent::Item { thread_id, turn_id, item_id,
        event: ItemEvent::Message { event: MessageItemEvent::Delta(text) } } if (thread_id, turn_id, item_id) == (&original.0, &original.1, &original.2) && text == " late")));
    state.notification("session/update", json!({"sessionId":"root", "update":{
        "sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":"completed"
    }})).await.unwrap();
    assert!(
        !state.execution_started().await,
        "late native child completion cannot start the root successor"
    );
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let envelope = stream.recv().await.unwrap().unwrap();
            if let ProviderEvent::Turn {
                thread_id,
                turn_id,
                event: TurnEvent::Completed(completed),
            } = envelope.event
            {
                assert_eq!(thread_id, original.0);
                assert_eq!(turn_id, original.1);
                assert_eq!(
                    completed.result().outcome(),
                    turn::TurnOutcome::Completed {
                        stop_reason: turn::TurnCompletionReason::Normal
                    }
                );
                break;
            }
        }
    })
    .await
    .unwrap();
    state.notification("_claude/sdkMessage", json!({"sessionId":"child", "message":{
        "type":"result", "uuid":"late-child-report", "duration_ms":15,
        "usage":{"input_tokens":1, "cache_read_input_tokens":0, "cache_creation_input_tokens":0, "output_tokens":2}
    }})).await.unwrap();
    let late_accounting = stream.recv().await.unwrap().unwrap();
    assert!(
        matches!(late_accounting.event, ProviderEvent::Turn { thread_id, turn_id,
        event: TurnEvent::Usage(report) } if thread_id == original.0 && turn_id == original.1
            && report.report_id().as_str() == "late-child-report")
    );
}

/// Context occupancy and optional compaction warnings remain thread metadata while no turn is active.
#[tokio::test]
async fn idle_thread_metadata_does_not_invent_a_turn_or_ignore_malformed_compaction() {
    use forge_provider_api::*;
    let state = AcpSessionState::new_native(AcpKind::Claude);
    let owner = forge_provider_api::testing::root_thread("idle-fixture");
    let (sink, mut stream) = events::channel(
        BindingId::new("idle").unwrap(),
        EventScope::Binding {
            thread_id: owner.clone(),
        },
    );
    let manifest = capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(&AcpKind::Claude.id()).unwrap(),
    );
    state.install_capabilities("root", sink, manifest).await;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"usage_update", "used":2, "size":10
            }}),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"compaction_update", "compactionId":"compact", "status":"future"
            }}),
        )
        .await
        .unwrap();
    let events = drain(&mut stream);
    assert!(events.iter().any(|event| matches!(event, ProviderEvent::Thread { thread_id, event: ThreadEvent::ContextUsage(_) } if thread_id == &owner.clone())));
    assert!(events.iter().any(|event| matches!(
        event,
        ProviderEvent::Provider {
            event: ProviderConnectionEvent::Warning(ProviderWarning {
                kind: ProviderWarningKind::IncompleteCompactionMetadata,
                ..
            })
        }
    )));
    assert!(
        !events
            .iter()
            .any(|event| matches!(event, ProviderEvent::Turn { .. }))
    );
}

/// Unmapped v1 session updates are admitted by documented policy or reported once per kind.
#[tokio::test]
async fn unmapped_session_updates_warn_once_unless_documented_metadata() {
    use forge_provider_api::*;
    let state = AcpSessionState::new_native(AcpKind::Claude);
    let owner = forge_provider_api::testing::root_thread("unmapped-fixture");
    let (sink, mut stream) = events::channel(
        BindingId::new("unmapped").unwrap(),
        EventScope::Binding {
            thread_id: owner.clone(),
        },
    );
    let manifest = capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(&AcpKind::Claude.id()).unwrap(),
    );
    state.install_capabilities("root", sink, manifest).await;
    for update in [
        json!({"sessionUpdate":"session_info_update", "title":"Title"}),
        json!({"sessionUpdate":"future_update"}),
        json!({"sessionUpdate":"future_update"}),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root", "update":update}),
            )
            .await
            .unwrap();
    }
    let warnings = drain(&mut stream)
        .into_iter()
        .filter_map(|event| match event {
            ProviderEvent::Provider {
                event:
                    ProviderConnectionEvent::Warning(ProviderWarning {
                        kind: ProviderWarningKind::UnsupportedNativeEvent,
                        scope: WarningScope::Thread { thread_id },
                        message,
                    }),
            } => Some((thread_id, message.as_str().to_owned())),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        warnings,
        [(
            owner.clone(),
            "ACP agent reported session update `future_update`, which Forge does not apply"
                .to_owned()
        )]
    );
}

/// Agent notices (such as an MCP server that failed to start) are warnings, never answer text,
/// and a repeated notice is surfaced once.
#[tokio::test]
async fn agent_notices_become_warnings_instead_of_answer_text() {
    use forge_provider_api::*;
    let state = AcpSessionState::new_native(AcpKind::Claude);
    let owner = forge_provider_api::testing::root_thread("notice-fixture");
    let (sink, mut stream) = events::channel(
        BindingId::new("notice").unwrap(),
        EventScope::Binding {
            thread_id: owner.clone(),
        },
    );
    let manifest = capability::CapabilityManifest::from_descriptor(
        crate::registry::descriptor(&AcpKind::Claude.id()).unwrap(),
    );
    state.install_capabilities("root", sink, manifest).await;
    // Claude repeats an unchanged startup notice with every turn; it surfaces once per binding.
    for _ in 0..2 {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root", "update":{
                    "sessionUpdate":"notice", "severity":"warning",
                    "title":"MCP server \"terminal-mcp\" failed to start",
                    "description":"Its tools are unavailable in this session. Run /mcp for the error."
                }}),
            )
            .await
            .unwrap();
    }
    let events = drain(&mut stream);
    let notices = events
        .iter()
        .filter_map(|event| match event {
            ProviderEvent::Provider {
                event:
                    ProviderConnectionEvent::Warning(ProviderWarning {
                        kind: ProviderWarningKind::NativeNotice { .. },
                        scope: WarningScope::Thread { thread_id },
                        message,
                    }),
            } => Some((thread_id.clone(), message.as_str().to_owned())),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        notices,
        [(
            owner,
            "MCP server \"terminal-mcp\" failed to start: Its tools are unavailable in this session. Run /mcp for the error."
                .to_owned()
        )]
    );
    assert!(
        !events
            .iter()
            .any(|event| matches!(event, ProviderEvent::Turn { .. }))
    );
}

/// Exercise independently retained native child permission work through the dedicated host boundary.
#[derive(Default)]
struct ChildFixturePermissions {
    observed: Mutex<Option<(InvocationScope, forge_provider_api::TurnAddress)>>,
    entered: tokio::sync::Notify,
    release: tokio::sync::Notify,
}

#[async_trait]
impl ProviderPermissions for ChildFixturePermissions {
    async fn authorize(&self, _: &InvocationScope, _: PermissionRequest) -> PermissionResponse {
        panic!("native child approval must not route through ordinary root authorization")
    }
    async fn before_tool(&self, _: &InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn before_observed_child(
        &self,
        _: &InvocationScope,
        _: &forge_provider_api::TurnAddress,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn authorize_observed_child(
        &self,
        parent: &InvocationScope,
        child: &forge_provider_api::TurnAddress,
        _: PermissionRequest,
    ) -> PermissionResponse {
        *self.observed.lock().unwrap() = Some((parent.clone(), child.clone()));
        self.entered.notify_one();
        self.release.notified().await;
        PermissionResponse::Allow
    }
}

/// Parent collection cannot cancel or wait on a proven independent child's pending permission work.
#[tokio::test]
async fn child_permission_authority_survives_parent_collection_with_original_scope() {
    use forge_provider_api::*;
    let state = Arc::new(AcpSessionState::new_native(AcpKind::Claude));
    let permissions = Arc::new(ChildFixturePermissions::default());
    let owner = forge_provider_api::testing::root_thread("permission-fixture");
    let scope = InvocationScope::new(owner.clone(), "parent");
    let (sink, mut stream) = events::channel(
        BindingId::new("permission-child").unwrap(),
        EventScope::Binding { thread_id: owner },
    );
    state
        .begin_native(
            "root",
            sink.invocation_sink(&scope).unwrap(),
            permissions.clone(),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"subagent_spawned", "subagentSessionId":"child"
            }}),
        )
        .await
        .unwrap();
    let waiting_state = state.clone();
    let waiting = tokio::spawn(async move {
        waiting_state.request("session/request_permission", json!({
        "sessionId":"child", "toolCall":{"toolCallId":"read", "kind":"read", "rawInput":{"path":"file"}},
        "options":[{"kind":"allow_once", "optionId":"allow"}, {"kind":"reject_once", "optionId":"deny"}]
    })).await
    });
    tokio::time::timeout(
        std::time::Duration::from_secs(2),
        permissions.entered.notified(),
    )
    .await
    .unwrap();
    let published = drain(&mut stream);
    let relation = published
        .iter()
        .position(|event| {
            matches!(
                event,
                ProviderEvent::Agent {
                    event: AgentEvent::Lifecycle {
                        event: AgentLifecycleEvent::Started { .. }
                    },
                    ..
                }
            )
        })
        .unwrap();
    let native_start = published
        .iter()
        .position(|event| {
            matches!(event, ProviderEvent::Turn {
        thread_id, event: TurnEvent::Started(_), .. } if thread_id != &scope.thread_id.clone())
        })
        .unwrap();
    assert!(relation < native_start);
    state
        .finish(turn::TurnOutcome::Completed {
            stop_reason: turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();
    tokio::time::timeout(
        std::time::Duration::from_secs(2),
        state.revoke_invocation_callbacks(&scope),
    )
    .await
    .unwrap();
    assert!(!waiting.is_finished());
    permissions.release.notify_one();
    let response = waiting.await.unwrap().unwrap();
    assert_eq!(response["outcome"]["optionId"], "allow");
    let retained = permissions.observed.lock().unwrap().clone().unwrap();
    assert_eq!(retained.0, scope);
    assert_ne!(retained.1.thread_id, scope.thread_id.clone());
    state.notification("session/update", json!({"sessionId":"root", "update":{
        "sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":"completed"
    }})).await.unwrap();
    state.revoke_callbacks().await;
}

/// Pending child metadata cannot acquire a native turn or collection certificate from parent completion.
#[tokio::test]
async fn metadata_only_child_does_not_start_or_complete_when_parent_finishes() {
    use forge_provider_api::*;
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"subagent_spawned", "subagentSessionId":"pending"
            }}),
        )
        .await
        .unwrap();
    state
        .finish(turn::TurnOutcome::Completed {
            stop_reason: turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();
    let events = drain(&mut stream);
    assert!(events.iter().any(|event| matches!(
        event,
        ProviderEvent::Agent {
            event: AgentEvent::Lifecycle {
                event: AgentLifecycleEvent::Started { .. }
            },
            ..
        }
    )));
    assert!(!events.iter().any(|event| matches!(
        event,
        ProviderEvent::Turn {
            event: TurnEvent::Started(_) | TurnEvent::Completed(_),
            ..
        }
    )));
    state.revoke_callbacks().await;
}

/// Losing an active child observation cannot manufacture terminal native evidence or a collection certificate.
#[tokio::test]
async fn child_binding_loss_publishes_unavailable_without_completed_certificate() {
    use forge_provider_api::*;
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"subagent_spawned", "subagentSessionId":"child"
            }}),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"child", "update":{
                "sessionUpdate":"agent_message_chunk", "content":{"type":"text", "text":"partial"}
            }}),
        )
        .await
        .unwrap();
    state
        .invalidate_binding(
            ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Transport)
                .at_phase(ProviderPhase::Observation, FailureDisposition::Invalidated)
                .into(),
        )
        .await;
    tokio::time::timeout(std::time::Duration::from_secs(2), state.revoke_callbacks())
        .await
        .unwrap();
    let mut unavailable = false;
    loop {
        match stream.try_next() {
            Ok(Some(envelope)) => {
                assert!(!matches!(
                    envelope.event,
                    ProviderEvent::Turn {
                        event: TurnEvent::Completed(_),
                        ..
                    }
                ));
                unavailable |= matches!(
                    envelope.event,
                    ProviderEvent::Provider {
                        event: ProviderConnectionEvent::Unavailable(_)
                    }
                );
            }
            Err(events::EventDeliveryError::Observation(_)) => break,
            other => panic!("child observation loss did not fence its binding: {other:?}"),
        }
    }
    assert!(unavailable);
}

/// A positive child terminal with no text starts native ownership before its terminal agent summary.
#[tokio::test]
async fn child_terminal_without_items_preserves_native_outcome_and_lifecycle_order() {
    use forge_provider_api::*;
    for status in ["completed", "failed", "cancelled"] {
        let (state, _, mut stream) = activate(AcpKind::Claude).await;
        state
            .notification(
                "session/update",
                json!({"sessionId":"root", "update":{
                    "sessionUpdate":"subagent_spawned", "subagentSessionId":"child"
                }}),
            )
            .await
            .unwrap();
        state.notification("session/update", json!({"sessionId":"root", "update":{
            "sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":status
        }})).await.unwrap();
        let events = drain(&mut stream);
        let started = events.iter().position(|event| {
            matches!(
                event,
                ProviderEvent::Turn {
                    event: TurnEvent::Started(_),
                    ..
                }
            )
        });
        let agent_ended = events
            .iter()
            .position(|event| {
                matches!(
                    event,
                    ProviderEvent::Agent {
                        event: AgentEvent::Lifecycle {
                            event: AgentLifecycleEvent::Finished { .. }
                        },
                        ..
                    }
                )
            })
            .unwrap();
        let completed = events.iter().position(|event| {
            matches!(
                event,
                ProviderEvent::Turn {
                    event: TurnEvent::Completed(_),
                    ..
                }
            )
        });
        if status == "cancelled" {
            assert!(started.is_none() && completed.is_none());
        } else {
            assert!(started.unwrap() < agent_ended && agent_ended < completed.unwrap());
            assert!(events.iter().any(|event| match event {
                ProviderEvent::Turn {
                    event: TurnEvent::Completed(completed),
                    ..
                } => matches!(
                    (status, completed.result().outcome()),
                    (
                        "completed",
                        turn::TurnOutcome::Completed {
                            stop_reason: turn::TurnCompletionReason::Normal,
                        },
                    ) | (
                        "failed",
                        turn::TurnOutcome::Failed {
                            retry: turn::RetryDecision::NotAllowed,
                            ..
                        },
                    )
                ),
                _ => false,
            }));
        }
        state.revoke_callbacks().await;
    }
}

/// Child context occupancy stays thread metadata while a child plan positively starts its own turn.
#[tokio::test]
async fn child_context_and_plan_preserve_thread_scope_and_processing_boundary() {
    use forge_provider_api::*;
    let (state, _, mut stream) = activate(AcpKind::Claude).await;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root", "update":{
                "sessionUpdate":"subagent_spawned", "subagentSessionId":"child"
            }}),
        )
        .await
        .unwrap();
    state
        .notification(
            "session/update",
            json!({"sessionId":"child", "update":{
                "sessionUpdate":"usage_update", "used":3, "size":20
            }}),
        )
        .await
        .unwrap();
    let metadata = drain(&mut stream);
    let child_thread = metadata
        .iter()
        .find_map(|event| match event {
            ProviderEvent::Thread {
                thread_id,
                event: ThreadEvent::ContextUsage(_),
            } => Some(thread_id.clone()),
            _ => None,
        })
        .unwrap();
    assert_ne!(
        child_thread,
        forge_provider_api::testing::root_thread("fixture").clone()
    );
    assert!(
        !metadata
            .iter()
            .any(|event| matches!(event, ProviderEvent::Turn { .. }))
    );
    state.notification("session/update", json!({"sessionId":"child", "update":{
        "sessionUpdate":"plan", "entries":[{"content":"Work", "priority":"medium", "status":"pending"}]
    }})).await.unwrap();
    let processing = drain(&mut stream);
    let started = processing
        .iter()
        .position(|event| {
            matches!(event, ProviderEvent::Turn { thread_id,
        event: TurnEvent::Started(_), .. } if thread_id == &child_thread)
        })
        .unwrap();
    let plan = processing
        .iter()
        .position(|event| {
            matches!(event, ProviderEvent::Turn { thread_id,
        event: TurnEvent::ChecklistUpdated(_), .. } if thread_id == &child_thread)
        })
        .unwrap();
    assert!(started < plan);
    state.notification("session/update", json!({"sessionId":"root", "update":{
        "sessionUpdate":"subagent_state_update", "subagentSessionId":"child", "state":"completed"
    }})).await.unwrap();
    state.revoke_callbacks().await;
}
