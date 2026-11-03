use super::{AcpHandler, AcpKind, session::AcpSessionState};
use async_trait::async_trait;
use forge_provider_api::events;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, ProviderPermissions};
use forge_provider_api::{ExecutionOwner, InvocationScope, ProviderError};
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
    let scope = InvocationScope::new(ExecutionOwner::root("fixture"), "exchange");
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
        .finish(forge_provider_api::turn::TurnOutcome::Completed)
        .await
        .unwrap();

    let events = drain(&mut stream);
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Execution { observation }
            if matches!(observation.update(), forge_provider_api::lifecycle::ExecutionUpdate::Started { .. }))));
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Message { update: forge_provider_api::MessageUpdate::Completed { text }, .. }
            if text == "wire answer")));
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Tool { update: forge_provider_api::ToolUpdate::Changes { changes }, .. }
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
    let owner = ExecutionOwner::root("fixture");
    let scope = forge_provider_api::EventScope::SessionOperation {
        owner,
        operation_id: "compact-fixture".into(),
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
    assert!(events.iter().all(|event| event.scope() == scope));
    assert!(events.iter().any(|event| matches!(event,
        forge_provider_api::ProviderEvent::Compaction { update, .. }
            if update.result.summary_content.as_deref() == Some("kept summary") && update.manual)));
}

#[test]
fn native_prompt_response_fixture_requires_a_valid_v1_prompt_result() {
    assert!(super::validate_prompt_response(json!({"stopReason":"end_turn"})).is_ok());
    assert!(super::validate_prompt_response(Value::Null).is_err());
}
