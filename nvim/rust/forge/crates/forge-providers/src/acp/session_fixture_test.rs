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
