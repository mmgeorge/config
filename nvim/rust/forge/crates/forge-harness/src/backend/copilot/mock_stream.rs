use crate::backend::{BackendCompletion, BackendError, BackendEventSink, BackendRequest};
use serde_json::{Value, json};

pub(crate) async fn replay(
    request: &BackendRequest,
    sink: &BackendEventSink,
) -> Result<BackendCompletion, BackendError> {
    let mut completion = BackendCompletion {
        backend_session_id: Some(
            request
                .backend_session_id
                .clone()
                .unwrap_or_else(|| "mock-copilot-session".into()),
        ),
        runtime: crate::backend::BackendRuntime {
            provider: "Mock Copilot stream".into(),
            model: Some(request.model.clone()),
        },
        ..Default::default()
    };
    let mut decoder = super::CopilotEventDecoder::new(
        sink.invocation_scope()?.clone(),
        request.workspace.clone(),
    );
    let native: Vec<(&str, Value)> = vec![
        (
            "assistant.turn_start",
            json!({"turnId":"0","interactionId":"mock"}),
        ),
        (
            "assistant.reasoning_delta",
            json!({"reasoningId":"thought","deltaContent":"Checking caller relationships"}),
        ),
        (
            "assistant.message_start",
            json!({"messageId":"commentary","phase":"commentary"}),
        ),
        (
            "assistant.message_delta",
            json!({"messageId":"commentary","deltaContent":"Inspecting callers"}),
        ),
        (
            "assistant.message",
            json!({
                "messageId":"commentary","phase":"commentary","content":"Inspecting callers",
                "toolRequests":[{"toolCallId":"first","name":"read","arguments":{"path":"main.rs"}}]
            }),
        ),
        (
            "assistant.reasoning",
            json!({"reasoningId":"thought","content":"Checking caller relationships"}),
        ),
        (
            "tool.execution_start",
            json!({"toolCallId":"first","toolName":"read"}),
        ),
        (
            "tool.execution_complete",
            json!({"toolCallId":"first","success":true,"result":{"content":"source"}}),
        ),
        ("assistant.turn_end", json!({"turnId":"0"})),
        (
            "assistant.turn_start",
            json!({"turnId":"1","interactionId":"mock"}),
        ),
        (
            "assistant.message",
            json!({
                "messageId":"tool-only","content":"",
                "toolRequests":[
                    {"toolCallId":"second","name":"read","arguments":{"path":"second.rs"}},
                    {"toolCallId":"third","name":"read","arguments":{"path":"third.rs"}}
                ]
            }),
        ),
        (
            "tool.execution_start",
            json!({"toolCallId":"second","toolName":"read"}),
        ),
        (
            "tool.execution_start",
            json!({"toolCallId":"third","toolName":"read"}),
        ),
        (
            "tool.execution_complete",
            json!({"toolCallId":"second","success":true,"result":{"content":"second source"}}),
        ),
        (
            "tool.execution_complete",
            json!({"toolCallId":"third","success":true,"result":{"content":"third source"}}),
        ),
        ("assistant.turn_end", json!({"turnId":"1"})),
        (
            "assistant.turn_start",
            json!({"turnId":"2","interactionId":"mock"}),
        ),
        (
            "assistant.message",
            json!({
                "messageId":"last","phase":"commentary","content":"Checked all callers"
            }),
        ),
        ("assistant.turn_end", json!({"turnId":"2"})),
        ("assistant.idle", json!({})),
        ("session.idle", json!({})),
    ];
    for (kind, data) in native {
        let event = serde_json::from_value(json!({
            "id":uuid::Uuid::new_v4().to_string(),
            "timestamp":"2026-10-29T12:00:00Z",
            "type":kind, "data":data,
        }))
        .map_err(|error| BackendError::Protocol {
            message: format!("Invalid mock native event: {error}"),
        })?;
        decoder.decode(&event, &mut completion, sink).await?;
        tokio::task::yield_now().await;
    }
    decoder.finish_success(sink).await?;
    Ok(completion)
}
