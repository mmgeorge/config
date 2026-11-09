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
