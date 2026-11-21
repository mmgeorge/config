use super::{AcpHandler, AcpKind, session::AcpSessionState};
use async_trait::async_trait;
use forge_provider_api::events;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, ProviderPermissions};
use forge_provider_api::{InvocationScope, ProviderError};
use serde_json::json;
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
    let state = Arc::new(AcpSessionState::new_native(kind, String::new()));
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

/// Deliver one tool frame on the active fixture session.
async fn tool_update(state: &AcpSessionState, update: serde_json::Value) {
    state
        .notification(
            "session/update",
            json!({"sessionId":"root","update":update}),
        )
        .await
        .unwrap();
}

/// A running tool with the default kind is visible before the agent reports completion.
#[tokio::test]
async fn tool_default_kind_admits_pending_and_reports_live_output() {
    use forge_provider_api::{ItemEvent, ItemStart, ProviderEvent, TextUpdate, ToolItemEvent};

    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    tool_update(
        &state,
        json!({"sessionUpdate":"tool_call","toolCallId":"call","title":"Working"}),
    )
    .await;
    let events = drain(&mut stream);
    let item = events
        .iter()
        .find_map(|event| match event {
            ProviderEvent::Item {
                item_id,
                event: ItemEvent::Started(ItemStart::ToolCall(_)),
                ..
            } => Some(item_id),
            _ => None,
        })
        .expect("pending default-kind call is admitted immediately");
    assert!(!events.iter().any(|event| matches!(
        event,
        ProviderEvent::Item {
            event: ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted
            },
            ..
        }
    )));

    tool_update(&state, json!({"sessionUpdate":"tool_call_update","toolCallId":"call","status":"in_progress","rawOutput":"working"})).await;
    let events = drain(&mut stream);
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { item_id, event: ItemEvent::Tool { event: ToolItemEvent::ExecutionStarted }, .. } if item_id == item)));
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { item_id, event: ItemEvent::Tool { event: ToolItemEvent::Output(TextUpdate::Replace(output)) }, .. }
        if item_id == item && output == "working")));
}

/// A kind refinement updates the same admitted item without invalidating the binding.
#[tokio::test]
async fn tool_kind_refinement_keeps_the_admitted_item() {
    use forge_provider_api::{
        ItemCompletion, ItemEvent, ItemStart, ProviderEvent, ToolItemEvent, tool::ToolSemantics,
    };

    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    tool_update(
        &state,
        json!({"sessionUpdate":"tool_call","toolCallId":"call","title":"Working","kind":"other"}),
    )
    .await;
    tool_update(&state, json!({"sessionUpdate":"tool_call_update","toolCallId":"call","kind":"execute","rawInput":{"command":"Get-Location"},"status":"in_progress"})).await;
    tool_update(
        &state,
        json!({"sessionUpdate":"tool_call_update","toolCallId":"call","status":"completed"}),
    )
    .await;
    let events = drain(&mut stream);
    let item = events
        .iter()
        .find_map(|event| match event {
            ProviderEvent::Item {
                item_id,
                event: ItemEvent::Started(ItemStart::ToolCall(_)),
                ..
            } => Some(item_id),
            _ => None,
        })
        .expect("original generic item exists");
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { item_id, event: ItemEvent::Tool { event: ToolItemEvent::DescriptorUpdated(descriptor) }, .. }
        if item_id == item && matches!(&descriptor.semantics, ToolSemantics::Command { text, .. } if text == "Get-Location"))));
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { item_id, event: ItemEvent::Completed(ItemCompletion::ToolCall(_)), .. }
        if item_id == item)));
}

/// Native name and locations refine the existing descriptor and raw arguments retain their shape.
#[tokio::test]
async fn tool_metadata_refinement_preserves_location_and_native_arguments() {
    use forge_provider_api::{ItemEvent, ProviderEvent, ToolItemEvent, tool::ToolSemantics};

    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    tool_update(&state, json!({"sessionUpdate":"tool_call","toolCallId":"call","kind":"read","title":"Reading configuration","rawInput":"false"})).await;
    tool_update(&state, json!({"sessionUpdate":"tool_call_update","toolCallId":"call","name":"read_file","locations":[{"path":"D:/config/README.md"}]})).await;
    let events = drain(&mut stream);
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { event: ItemEvent::Tool { event: ToolItemEvent::DescriptorUpdated(descriptor) }, .. }
        if descriptor.name == "read_file" && descriptor.arguments.value() == "false"
            && matches!(&descriptor.semantics, ToolSemantics::Read { paths, .. } if paths == &["D:/config/README.md"]))));
}

/// An omitted oldText denotes a new file, and an empty content snapshot clears previous content.
#[tokio::test]
async fn tool_content_replacement_clears_output_and_file_changes() {
    use forge_provider_api::{
        ItemEvent, ProviderEvent, TextUpdate, ToolItemEvent, change::ProviderChangeKind,
    };

    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    tool_update(&state, json!({"sessionUpdate":"tool_call","toolCallId":"call","kind":"edit","title":"Edit","status":"in_progress",
        "content":[{"type":"diff","path":"D:/config/new.txt","newText":"new\n"},{"type":"content","content":{"type":"text","text":"working"}}]})).await;
    assert!(drain(&mut stream).iter().any(|event| matches!(event,
        ProviderEvent::Item { event: ItemEvent::Tool { event: ToolItemEvent::Changes(changes) }, .. }
        if changes.file.iter().any(|change| change.kind == ProviderChangeKind::Add && change.path == "D:/config/new.txt"))));

    tool_update(&state, json!({"sessionUpdate":"tool_call_update","toolCallId":"call","status":"completed","content":[]})).await;
    let events = drain(&mut stream);
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { event: ItemEvent::Tool { event: ToolItemEvent::Output(TextUpdate::Replace(output)) }, .. } if output.is_empty())));
    assert!(events.iter().any(|event| matches!(event,
        ProviderEvent::Item { event: ItemEvent::Tool { event: ToolItemEvent::Changes(changes) }, .. } if changes.is_empty())));
}

/// An identical terminal frame is a no-op even when its result contains output.
#[tokio::test]
async fn tool_terminal_replay_does_not_publish_output_twice() {
    let (state, permission, mut stream) = activate(AcpKind::Antigravity).await;
    tool_update(
        &state,
        json!({"sessionUpdate":"tool_call","toolCallId":"call","kind":"other","title":"Tool"}),
    )
    .await;
    let completed = json!({"sessionUpdate":"tool_call_update","toolCallId":"call","status":"completed","rawOutput":"result"});
    tool_update(&state, completed.clone()).await;
    assert!(!drain(&mut stream).is_empty());
    tool_update(&state, completed.clone()).await;
    assert!(drain(&mut stream).is_empty());
    state
        .finish(forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();
    let scope = InvocationScope::new(
        forge_provider_api::testing::root_thread("fixture"),
        "next-exchange",
    );
    let binding = forge_provider_api::BindingId::new("fixture-binding").unwrap();
    let (sink, mut stream) = events::channel(binding, scope);
    state.begin_native("root", sink, permission).await.unwrap();
    tool_update(&state, completed).await;
    assert!(drain(&mut stream).is_empty());
}

#[tokio::test]
async fn native_message_and_tool_updates_publish_normalized_addresses_and_outcomes() {
    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
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

/// Collect the classification each assistant message received, keyed by its completed text.
fn message_classifications(
    events: &[forge_provider_api::ProviderEvent],
) -> Vec<(String, Vec<forge_provider_api::MessageClassification>)> {
    let mut message_list: Vec<(
        forge_provider_api::ItemId,
        String,
        Vec<forge_provider_api::MessageClassification>,
    )> = Vec::new();
    for event in events {
        let forge_provider_api::ProviderEvent::Item { item_id, event, .. } = event else {
            continue;
        };
        match event {
            forge_provider_api::ItemEvent::Started(
                forge_provider_api::ItemStart::AgentMessage(start),
            ) => message_list.push((item_id.clone(), start.text.clone(), Vec::new())),
            forge_provider_api::ItemEvent::Message {
                event: forge_provider_api::MessageItemEvent::Classified(classification),
            } => {
                if let Some((_, _, classification_list)) = message_list
                    .iter_mut()
                    .find(|(message_id, ..)| message_id == item_id)
                {
                    classification_list.push(*classification);
                }
            }
            forge_provider_api::ItemEvent::Completed(
                forge_provider_api::ItemCompletion::AgentMessage(completion),
            ) => {
                if let Some((_, text, _)) = message_list
                    .iter_mut()
                    .find(|(message_id, ..)| message_id == item_id)
                {
                    text.clone_from(&completion.text);
                }
            }
            _ => {}
        }
    }
    message_list
        .into_iter()
        .map(|(_, text, classification_list)| (text, classification_list))
        .collect()
}

/// A late tool update does not classify already-streaming final prose as commentary.
#[tokio::test]
async fn late_update_for_an_earlier_tool_keeps_the_final_answer_a_response() {
    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    let session_update = |update: serde_json::Value| {
        let state = state.clone();
        async move {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"root","update":update}),
                )
                .await
                .unwrap();
        }
    };
    session_update(
        json!({"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"progress"}}),
    )
    .await;
    session_update(json!({"sessionUpdate":"tool_call","toolCallId":"abandoned","kind":"search","title":"grep"})).await;
    session_update(json!({"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"final answer"}})).await;
    session_update(
        json!({"sessionUpdate":"tool_call_update","toolCallId":"abandoned","status":"failed"}),
    )
    .await;
    state
        .finish(forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();

    let commentary = forge_provider_api::MessageClassification::Known(
        forge_provider_api::AgentMessageRole::Commentary,
    );
    let response = forge_provider_api::MessageClassification::Known(
        forge_provider_api::AgentMessageRole::Response,
    );
    assert_eq!(
        message_classifications(&drain(&mut stream)),
        vec![
            ("progress".to_owned(), vec![commentary]),
            ("final answer".to_owned(), vec![response]),
        ]
    );
}

/// An interrupted turn settles its pending prose as a completed turn does.
///
/// Harness hides an interrupted response and keeps other prose, so prose left unclassified at
/// an interruption stayed in the transcript while other providers' interrupted replies left it.
#[tokio::test]
async fn interrupted_turn_classifies_its_last_prose_as_the_response() {
    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    for (native_message_id, text) in [("msg-1", "progress"), ("msg-2", "partial answer")] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"root","update":{
                    "sessionUpdate":"agent_message_chunk","messageId":native_message_id,
                    "content":{"type":"text","text":text}
                }}),
            )
            .await
            .unwrap();
    }
    state
        .finish(forge_provider_api::turn::TurnOutcome::Interrupted)
        .await
        .unwrap();

    let events = drain(&mut stream);
    let commentary = forge_provider_api::MessageClassification::Known(
        forge_provider_api::AgentMessageRole::Commentary,
    );
    let response = forge_provider_api::MessageClassification::Known(
        forge_provider_api::AgentMessageRole::Response,
    );
    assert_eq!(
        message_classifications(&events),
        vec![
            ("progress".to_owned(), vec![commentary]),
            ("partial answer".to_owned(), vec![response]),
        ]
    );
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Item { event: forge_provider_api::ItemEvent::Completed(forge_provider_api::ItemCompletion::AgentMessage(forge_provider_api::MessageCompletion { text, outcome })), .. }
            if text == "partial answer" && *outcome == forge_provider_api::MessageOutcome::Interrupted)));
}

#[tokio::test]
async fn native_message_ids_split_adjacent_prose_into_separate_messages() {
    let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
    for (native_message_id, text) in [
        (Some("msg-1"), "first "),
        (Some("msg-1"), "message"),
        (Some("msg-2"), "second message"),
        (None, " without"),
        (None, " an id"),
    ] {
        let mut update = json!({
            "sessionUpdate":"agent_message_chunk","content":{"type":"text","text":text}
        });
        if let Some(native_message_id) = native_message_id {
            update["messageId"] = json!(native_message_id);
        }
        state
            .notification(
                "session/update",
                json!({"sessionId":"root","update":update}),
            )
            .await
            .unwrap();
    }
    state
        .finish(forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();

    let commentary = forge_provider_api::MessageClassification::Known(
        forge_provider_api::AgentMessageRole::Commentary,
    );
    let response = forge_provider_api::MessageClassification::Known(
        forge_provider_api::AgentMessageRole::Response,
    );
    assert_eq!(
        message_classifications(&drain(&mut stream)),
        vec![
            ("first message".to_owned(), vec![commentary]),
            ("second message".to_owned(), vec![commentary]),
            (" without an id".to_owned(), vec![response]),
        ]
    );
}

/// Reconcile anonymous wire text only when native history proves the whole item.
#[tokio::test]
async fn native_transcript_splits_only_exact_anonymous_replies() {
    use forge_provider_api::MessageOutcome::{Completed, Interrupted};
    for (message_id, reply, wire_tail, trailing_thought, interrupted, expected) in [
        (
            None,
            Some("second"),
            "\n\nsecond",
            false,
            false,
            vec![("first\n\nstill first", Completed), ("second", Completed)],
        ),
        (
            None,
            Some("different"),
            "\n\nsecond",
            false,
            false,
            vec![("first\n\nstill first\n\nsecond", Completed)],
        ),
        (
            Some("wire-id"),
            Some("second"),
            "\n\nsecond",
            false,
            false,
            vec![("first\n\nstill first\n\nsecond", Completed)],
        ),
        (
            None,
            Some("second"),
            "\n\nsecond",
            false,
            true,
            vec![("first\n\nstill first", Completed), ("second", Interrupted)],
        ),
        (
            None,
            None,
            "",
            false,
            true,
            vec![("first\n\nstill first", Completed)],
        ),
        (
            None,
            None,
            "",
            true,
            true,
            vec![("first\n\nstill first", Completed)],
        ),
        (
            None,
            None,
            "\n\n継続\n\n…",
            false,
            true,
            vec![
                ("first\n\nstill first", Completed),
                ("継続\n\n…", Interrupted),
            ],
        ),
        (
            None,
            None,
            "\nsecond",
            false,
            true,
            vec![("first\n\nstill first\nsecond", Interrupted)],
        ),
    ] {
        let (state, _, mut stream) = activate(AcpKind::Antigravity).await;
        let directory = tempfile::tempdir().unwrap();
        let session_id = "35b67ea7-0c3f-4fcb-b3c0-8932fa0a064e";
        let logs = directory
            .path()
            .join("antigravity-cli/brain")
            .join(session_id)
            .join(".system_generated/logs");
        std::fs::create_dir_all(&logs).unwrap();
        let path = super::transcript::path(directory.path(), session_id).unwrap();
        let history = super::transcript::TranscriptCursor::capture(&path)
            .await
            .unwrap();
        let mut rows = vec![
            json!({"type":"USER_INPUT","status":"DONE","content":"original prompt"}),
            json!({"type":"PLANNER_RESPONSE","status":"DONE","thinking":"considering",
                "tool_calls":[{"name":"read_file","args":{"path":"example.txt"}}]}),
            json!({"type":"PLANNER_RESPONSE","status":"DONE","content":"first\n\nstill first"}),
            json!({"type":"USER_INPUT","status":"DONE","content":"steer"}),
            json!({"type":"SYSTEM_MESSAGE","status":"DONE","content":"runtime metadata"}),
        ];
        if let Some(reply) = reply {
            rows.push(json!({"type":"PLANNER_RESPONSE","status":"DONE","content":reply}));
        }
        if trailing_thought {
            rows.push(json!({"type":"PLANNER_RESPONSE","status":"DONE","thinking":"thinking"}));
        }
        if interrupted {
            rows.push(json!({"type":"ERROR_MESSAGE","status":"DONE","content":"stopped"}));
        }
        let records = rows
            .into_iter()
            .map(|row| format!("{row}\n"))
            .collect::<String>();
        std::fs::write(logs.join("transcript_full.jsonl"), records).unwrap();
        std::fs::write(logs.join("transcript.jsonl"), "abbreviated record\n").unwrap();
        for text in ["first\n\nstill ", "first", wire_tail] {
            let mut update = json!({
                "sessionUpdate":"agent_message_chunk","content":{"type":"text","text":text}
            });
            if let Some(message_id) = message_id {
                update["messageId"] = json!(message_id);
            }
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"root","update":update}),
                )
                .await
                .unwrap();
        }
        if trailing_thought {
            state
                .notification(
                    "session/update",
                    json!({"sessionId":"root","update":{
                        "sessionUpdate":"agent_thought_chunk","content":{"type":"text","text":"thinking"}
                    }}),
                )
                .await
                .unwrap();
        }
        let outcome = if interrupted {
            forge_provider_api::turn::TurnOutcome::Interrupted
        } else {
            forge_provider_api::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            }
        };
        state
            .finish_with_transcript(outcome, Some(history))
            .await
            .unwrap();

        let events = drain(&mut stream);
        let completed = events
            .iter()
            .filter_map(|event| match event {
                forge_provider_api::ProviderEvent::Item {
                    event:
                        forge_provider_api::ItemEvent::Completed(
                            forge_provider_api::ItemCompletion::AgentMessage(message),
                        ),
                    ..
                } => Some((message.text.as_str(), message.outcome)),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(completed, expected);
    }
}

#[tokio::test]
async fn native_permission_callback_receives_a_normalized_target_and_once_choice() {
    let (state, permission, _stream) = activate(AcpKind::Antigravity).await;
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

/// Catalog entries that were never launched cannot make a valid MCP tool name ambiguous.
#[tokio::test]
async fn native_mcp_permission_ignores_disabled_catalog_servers() {
    let (state, permission, _stream) = activate(AcpKind::Antigravity).await;
    state
        .set_mcp_server_names(vec!["project_tools".into()])
        .unwrap();
    state
        .update_catalog(json!({"mcpResources":[
            {"name":"project_tools","state":"requested"},
            {"name":"project_tools","state":"disabled"},
            {"name":"project","state":"disabled"}
        ]}))
        .await;

    let response = state
        .request(
            "session/request_permission",
            json!({
                "sessionId":"root",
                "toolCall":{
                    "toolCallId":"fixture-call",
                    "name":"mcp_project_tools_read_item"
                },
                "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
            }),
        )
        .await
        .unwrap();
    assert_eq!(response["outcome"]["optionId"], "allow");
    let observed = permission.observed.lock().unwrap();
    assert_eq!(observed.len(), 1);
    assert_eq!(
        observed[0].target_list,
        vec![forge_provider_api::permissions::PermissionTarget::Mcp {
            target: "project_tools/read_item".into(),
        }]
    );
}

/// Allow delivery to the mounted host endpoint without asking its ACP policy callback again.
#[tokio::test]
async fn native_host_mcp_uses_http_permission_boundary() {
    let (state, permission, _stream) = activate(AcpKind::Antigravity).await;
    state
        .set_mcp_server_names(vec![super::permission::HOST_TOOL_SERVER.into()])
        .unwrap();
    for tool_call in [
        json!({"toolCallId":"host-catalog", "name":"ToolSearch", "rawInput":{
            "query":"select:mcp__forge-harness-control__harness_config"
        }}),
        json!({"toolCallId":"host-call","name":"mcp_forge-harness-control_harness_config"}),
    ] {
        let response = state
            .request(
                "session/request_permission",
                json!({
                    "sessionId":"root",
                    "toolCall":tool_call,
                    "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
                }),
            )
            .await
            .unwrap();
        assert_eq!(response["outcome"]["optionId"], "allow");
    }
    assert!(permission.observed.lock().unwrap().is_empty());
}

/// Reject native options before the host can record a durable decision.
#[tokio::test]
async fn native_permission_rejects_unusable_once_options_before_policy() {
    let (state, permission, _stream) = activate(AcpKind::Antigravity).await;
    for options in [
        json!([{"kind":"allow_once","optionId":"allow"}]),
        json!([{"kind":"allow_once"},{"kind":"reject_once","optionId":"deny"}]),
        json!([{"kind":"allow_once","optionId":"shared"},{"kind":"reject_once","optionId":"shared"}]),
        json!([{"kind":"allow_once","optionId":"allow"},
               {"kind":"allow_once","optionId":"also-allow"},
               {"kind":"reject_once","optionId":"deny"}]),
    ] {
        let response = state
            .request(
                "session/request_permission",
                json!({
                    "sessionId":"root",
                    "toolCall":{"toolCallId":"fixture","kind":"read","rawInput":{"path":"fixture.txt"}},
                    "options":options
                }),
            )
            .await
            .unwrap();
        assert_eq!(response["outcome"]["outcome"], "cancelled");
    }
    assert!(permission.observed.lock().unwrap().is_empty());
}

/// Pass the command and shell from encoded native arguments to policy.
#[tokio::test]
async fn native_permission_preserves_encoded_command_and_reported_shell() {
    let (state, permission, _stream) = activate(AcpKind::Antigravity).await;
    let response = state
        .request(
            "session/request_permission",
            json!({
                "sessionId":"root",
                "toolCall":{
                    "toolCallId":"fixture-command",
                    "name":"run_command",
                    "kind":"execute",
                    "rawInput":json!({"cmd":"Get-Date","shell":"pwsh"}).to_string()
                },
                "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
            }),
        )
        .await
        .unwrap();
    assert_eq!(response["outcome"]["optionId"], "allow");
    assert_eq!(
        permission.observed.lock().unwrap()[0].target_list,
        vec![forge_provider_api::permissions::PermissionTarget::Command {
            command: "Get-Date".into(),
            shell: Some(forge_shell::shell::CommandShell::PowerShell),
        }]
    );
}

/// Ignore traffic from a session the peer never registered without starting or revoking the root.
#[tokio::test]
async fn unregistered_child_work_preserves_root_authority() {
    let (state, permission, mut stream) = activate(AcpKind::Antigravity).await;
    send_child_update(
        &state,
        "unregistered-child",
        json!({"sessionUpdate":"tool_call","toolCallId":"fixture-read","title":"Read","kind":"read"}),
    )
    .await;
    assert_eq!(
        child_fixture_permission(&state, "unregistered-child").await["outcome"]["outcome"],
        "cancelled"
    );
    assert!(!state.execution_started().await);
    assert!(drain(&mut stream).is_empty());
    assert!(permission.observed.lock().unwrap().is_empty());

    assert_eq!(
        child_fixture_permission(&state, "root").await["outcome"]["optionId"],
        "allow"
    );
    assert_eq!(permission.observed.lock().unwrap().len(), 1);
}

/// A child tool update or request cannot affect the successor root after the child completes.
#[tokio::test]
async fn retired_child_work_preserves_successor_root() {
    let (state, permission, mut stream) = activate(AcpKind::Antigravity).await;
    send_child_update(
        &state,
        "root",
        json!({"sessionUpdate":"subagent_spawned","subagentSessionId":"child"}),
    )
    .await;
    assert!(state.address("child").await.is_some());
    send_child_update(
        &state,
        "child",
        json!({"sessionUpdate":"tool_call","toolCallId":"fixture-read","title":"Child read","kind":"read"}),
    )
    .await;
    send_child_update(
        &state,
        "root",
        json!({"sessionUpdate":"subagent_state_update","subagentSessionId":"child","state":"completed"}),
    )
    .await;
    assert!(state.address("child").await.is_none());
    assert!(!drain(&mut stream).is_empty());
    state
        .finish(forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        })
        .await
        .unwrap();

    let scope = InvocationScope::new(
        forge_provider_api::testing::root_thread("fixture"),
        "successor-exchange",
    );
    let binding = forge_provider_api::BindingId::new("fixture-binding").unwrap();
    let (sink, mut successor_stream) = events::channel(binding, scope);
    state
        .begin_native("root", sink, permission.clone())
        .await
        .unwrap();
    send_child_update(
        &state,
        "child",
        json!({"sessionUpdate":"tool_call_update","toolCallId":"fixture-read","status":"completed"}),
    )
    .await;
    assert_eq!(
        child_fixture_permission(&state, "child").await["outcome"]["outcome"],
        "cancelled"
    );
    assert!(!state.execution_started().await);
    assert!(drain(&mut successor_stream).is_empty());
    assert!(permission.observed.lock().unwrap().is_empty());

    assert_eq!(
        child_fixture_permission(&state, "root").await["outcome"]["optionId"],
        "allow"
    );
    assert_eq!(permission.observed.lock().unwrap().len(), 1);
}

/// Deliver a session update with the native envelope's original session identity.
async fn send_child_update(state: &AcpSessionState, session: &str, update: serde_json::Value) {
    state
        .notification(
            "session/update",
            json!({"sessionId":session,"update":update}),
        )
        .await
        .unwrap();
}

/// Request a read with valid ACP options so session ownership determines whether it reaches policy.
async fn child_fixture_permission(state: &AcpSessionState, session: &str) -> serde_json::Value {
    state
        .request(
            "session/request_permission",
            json!({
                "sessionId":session,
                "toolCall":{"toolCallId":"fixture-read","kind":"read","rawInput":{"path":"fixture.txt"}},
                "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
            }),
        )
        .await
        .unwrap()
}
