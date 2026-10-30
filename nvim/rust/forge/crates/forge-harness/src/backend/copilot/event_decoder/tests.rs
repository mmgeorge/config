use super::*;
use crate::backend::{
    BackendCompletion, BackendEvent, InvocationScope, MessageClassification, MessageRole,
    MessageUpdate, RuntimeUpdate, ToolOutcome, ToolUpdate,
};
use serde_json::json;

#[path = "conformance_tests.rs"]
mod conformance_tests;

fn event(kind: &str, data: Value) -> SessionEvent {
    serde_json::from_value(json!({
        "id": uuid::Uuid::new_v4().to_string(),
        "timestamp":"2026-10-29T12:00:00Z",
        "type":kind, "data":data,
    }))
    .unwrap()
}

fn scope() -> InvocationScope {
    InvocationScope::new(crate::backend::ExecutionOwner::root("session"), "exchange")
}

async fn replay(native: Vec<SessionEvent>, synthesize: bool) -> anyhow::Result<Vec<BackendEvent>> {
    let scope = scope();
    let (sink, mut receiver) = crate::backend::events::channel(scope.clone());
    let execution = async {
        let mut decoder = CopilotEventDecoder::new(scope, String::new());
        let mut output = BackendCompletion {
            backend_session_id: Some("native-session".into()),
            ..Default::default()
        };
        for event in native {
            decoder.decode(&event, &mut output, &sink).await?;
        }
        if synthesize {
            decoder.finish_success(&sink).await?;
            decoder.finish_success(&sink).await?;
        }
        Ok::<_, crate::backend::BackendError>(())
    };
    tokio::pin!(execution);
    let mut result = Vec::new();
    loop {
        tokio::select! {
            event = receiver.recv() => {
                if let Some(event) = event? {
                    result.push(event);
                }
            }
            completed = &mut execution => {
                completed?;
                while let Some(event) = receiver.try_next()? { result.push(event); }
                return Ok(result);
            }
        }
    }
}

fn start() -> SessionEvent {
    event(
        "assistant.turn_start",
        json!({"turnId":"0","interactionId":"interaction"}),
    )
}

#[tokio::test]
async fn phase_start_routes_first_text_without_empty_entry() -> anyhow::Result<()> {
    let metadata = replay(
        vec![
            start(),
            event(
                "assistant.message_start",
                json!({"messageId":"answer","phase":"final_answer"}),
            ),
        ],
        false,
    )
    .await?;
    assert!(
        !metadata
            .iter()
            .any(|event| matches!(event, BackendEvent::Message { .. }))
    );
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.message_start",
                json!({"messageId":"answer","phase":"final_answer"}),
            ),
            event(
                "assistant.message_delta",
                json!({"messageId":"answer","deltaContent":"First"}),
            ),
            event(
                "assistant.message_delta",
                json!({"messageId":"answer","deltaContent":" second"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"answer","content":"First second","phase":"final_answer"}),
            ),
            event("assistant.turn_end", json!({"turnId":"0"})),
            event("session.idle", json!({})),
        ],
        true,
    )
    .await?;
    let messages: Vec<_> = delivered
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Message { address, update } => Some((address, update)),
            _ => None,
        })
        .collect();
    assert!(matches!(messages[0].1,
        MessageUpdate::Started { classification: MessageClassification::Known(MessageRole::Response), text }
        if text == "First"));
    assert!(
        messages
            .iter()
            .all(|(address, _)| *address == messages[0].0)
    );
    assert_eq!(
        messages
            .iter()
            .filter(|(_, update)| matches!(update, MessageUpdate::Started { .. }))
            .count(),
        1
    );
    Ok(())
}

#[tokio::test]
async fn repeated_native_turn_numbers_keep_distinct_interaction_addresses() -> anyhow::Result<()> {
    let delivered = replay(
        vec![
            event("assistant.turn_start", json!({"turnId":"0"})),
            event(
                "assistant.message",
                json!({"messageId":"first","phase":"commentary","content":"First"}),
            ),
            event("assistant.turn_end", json!({"turnId":"0"})),
            event("assistant.turn_start", json!({"turnId":"0"})),
            event(
                "assistant.message",
                json!({"messageId":"second","phase":"final_answer","content":"Second"}),
            ),
            event("assistant.turn_end", json!({"turnId":"0"})),
        ],
        true,
    )
    .await?;
    let addresses: Vec<_> = delivered
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Turn {
                address,
                update: crate::backend::TurnUpdate::Started { .. },
            } => Some(address),
            _ => None,
        })
        .collect();
    assert_eq!(addresses.len(), 2);
    assert_ne!(addresses[0], addresses[1]);
    Ok(())
}

#[tokio::test]
async fn missing_and_unknown_phases_preserve_unclassified_text() -> anyhow::Result<()> {
    for phase in [None, Some("future_phase")] {
        let mut data = json!({"messageId":"message", "content":"Unclassified text"});
        if let Some(phase) = phase {
            data["phase"] = json!(phase);
        }
        let delivered = replay(vec![start(), event("assistant.message", data)], true).await?;
        assert!(delivered.iter().any(|event| matches!(event,
            BackendEvent::Message { update: MessageUpdate::Started {
                classification: MessageClassification::Unclassified, text }, .. }
            if text == "Unclassified text")));
        assert!(!delivered.iter().any(|event| matches!(
            event,
            BackendEvent::Message {
                update: MessageUpdate::Started {
                    classification: MessageClassification::Known(MessageRole::Response),
                    ..
                },
                ..
            }
        )));
        assert_eq!(
            delivered
                .iter()
                .any(|event| matches!(event, BackendEvent::Diagnostic { .. })),
            phase.is_some()
        );
    }
    Ok(())
}

#[tokio::test]
async fn later_known_phase_classifies_the_same_unclassified_message() -> anyhow::Result<()> {
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.message_start",
                json!({"messageId":"answer","phase":"future_phase"}),
            ),
            event(
                "assistant.message_delta",
                json!({"messageId":"answer","deltaContent":"Answer"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"answer","phase":"final_answer","content":"Answer"}),
            ),
        ],
        true,
    )
    .await?;
    let messages: Vec<_> = delivered
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Message { address, update } => Some((address, update)),
            _ => None,
        })
        .collect();
    assert!(matches!(
        messages[0].1,
        MessageUpdate::Started {
            classification: MessageClassification::Unclassified,
            ..
        }
    ));
    assert!(messages.iter().any(|(_, update)| matches!(
        update,
        MessageUpdate::Classified {
            role: MessageRole::Response
        }
    )));
    assert!(
        messages
            .iter()
            .all(|(address, _)| *address == messages[0].0)
    );
    assert_eq!(
        messages
            .iter()
            .filter(|(_, update)| matches!(update, MessageUpdate::Started { .. }))
            .count(),
        1
    );
    Ok(())
}

#[tokio::test]
async fn idle_synthesizes_latest_commentary_as_a_distinct_response_once() -> anyhow::Result<()> {
    let idle = event("session.idle", json!({}));
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.message_start",
                json!({"messageId":"first","phase":"commentary"}),
            ),
            event(
                "assistant.message_start",
                json!({"messageId":"latest","phase":"commentary"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"latest","phase":"commentary","content":"Latest answer"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"first","phase":"commentary","content":"Older snapshot"}),
            ),
            event("assistant.turn_end", json!({"turnId":"0"})),
            idle.clone(),
            idle,
        ],
        true,
    )
    .await?;
    let starts: Vec<_> = delivered
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Message {
                address,
                update:
                    MessageUpdate::Started {
                        classification,
                        text,
                    },
            } => Some((address, classification, text)),
            _ => None,
        })
        .collect();
    assert_eq!(starts.len(), 3);
    let response = starts
        .iter()
        .find(|(_, classification, _)| {
            **classification == MessageClassification::Known(MessageRole::Response)
        })
        .unwrap();
    assert_eq!(response.2, "Latest answer");
    let commentary = starts
        .iter()
        .find(|(_, classification, text)| {
            **classification == MessageClassification::Known(MessageRole::Commentary)
                && text.as_str() == "Latest answer"
        })
        .unwrap();
    assert_ne!(response.0, commentary.0);
    let response_position = delivered
        .iter()
        .position(|event| {
            matches!(event,
        BackendEvent::Message { address, update: MessageUpdate::Completed { .. } }
        if address == response.0)
        })
        .unwrap();
    let terminal_position = delivered
        .iter()
        .position(|event| {
            matches!(
                event,
                BackendEvent::Turn {
                    update: crate::backend::TurnUpdate::Finished { .. },
                    ..
                }
            )
        })
        .unwrap();
    assert!(response_position < terminal_position);
    Ok(())
}

#[tokio::test]
async fn explicit_empty_final_suppresses_commentary_fallback() -> anyhow::Result<()> {
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.message",
                json!({"messageId":"commentary","phase":"commentary","content":"Progress"}),
            ),
            event(
                "assistant.message_start",
                json!({"messageId":"final","phase":"final_answer"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"final","phase":"final_answer","content":""}),
            ),
        ],
        true,
    )
    .await?;
    assert_eq!(
        delivered
            .iter()
            .filter(|event| matches!(
                event,
                BackendEvent::Message {
                    update: MessageUpdate::Started { .. },
                    ..
                }
            ))
            .count(),
        1
    );
    Ok(())
}

#[tokio::test]
async fn tool_only_messages_emit_one_scoped_tool_without_commentary() -> anyhow::Result<()> {
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.tool_call_delta",
                json!({"toolCallId":"tool","inputDelta":"{}"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"empty","content":"",
            "toolRequests":[{"toolCallId":"tool","name":"read","arguments":{}}]}),
            ),
            event(
                "tool.execution_start",
                json!({"toolCallId":"tool","toolName":"read","arguments":{}}),
            ),
            event(
                "tool.execution_complete",
                json!({"toolCallId":"tool","success":false,
            "error":{"code":"denied","message":"Permission denied"}}),
            ),
        ],
        false,
    )
    .await?;
    assert!(
        !delivered
            .iter()
            .any(|event| matches!(event, BackendEvent::Message { .. }))
    );
    assert_eq!(
        delivered
            .iter()
            .filter(|event| matches!(
                event,
                BackendEvent::Tool {
                    update: ToolUpdate::Requested { .. },
                    ..
                }
            ))
            .count(),
        1
    );
    assert!(delivered.iter().any(|event| matches!(
        event,
        BackendEvent::Tool {
            update: ToolUpdate::Finished {
                outcome: ToolOutcome::Denied { .. }
            },
            ..
        }
    )));
    Ok(())
}

#[tokio::test]
async fn commentary_and_response_overlap_without_identity_or_status_reversal() -> anyhow::Result<()>
{
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.reasoning_delta",
                json!({"reasoningId":"thought","deltaContent":"Checking"}),
            ),
            event(
                "assistant.message_start",
                json!({"messageId":"commentary","phase":"commentary"}),
            ),
            event(
                "assistant.message_delta",
                json!({"messageId":"commentary","deltaContent":"Progress"}),
            ),
            event(
                "assistant.message_start",
                json!({"messageId":"answer","phase":"final_answer"}),
            ),
            event(
                "assistant.message_delta",
                json!({"messageId":"answer","deltaContent":"Result"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"commentary","phase":"commentary","content":"Progress"}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"answer","phase":"final_answer","content":"Result"}),
            ),
            event(
                "assistant.reasoning",
                json!({"reasoningId":"thought","content":"Checking"}),
            ),
            event(
                "assistant.reasoning",
                json!({"reasoningId":"empty","content":""}),
            ),
            event(
                "assistant.streaming_delta",
                json!({"totalResponseSizeBytes":128}),
            ),
        ],
        true,
    )
    .await?;
    let starts: Vec<_> = delivered
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Message {
                address,
                update: MessageUpdate::Started { classification, .. },
            } => Some((address, classification)),
            _ => None,
        })
        .collect();
    assert_eq!(starts.len(), 3);
    assert_eq!(
        starts[0].1,
        &MessageClassification::Known(MessageRole::ThoughtSummary)
    );
    assert_eq!(
        starts[1].1,
        &MessageClassification::Known(MessageRole::Commentary)
    );
    assert_eq!(
        starts[2].1,
        &MessageClassification::Known(MessageRole::Response)
    );
    assert!(delivered.iter().any(|event| matches!(
        event,
        BackendEvent::Runtime {
            update: RuntimeUpdate::Progress {
                received_bytes: 128
            },
            ..
        }
    )));
    Ok(())
}

#[tokio::test]
async fn cancelled_stream_does_not_synthesize_response() -> anyhow::Result<()> {
    let scope = scope();
    let (sink, mut receiver) = crate::backend::events::channel(scope.clone());
    let mut decoder = CopilotEventDecoder::new(scope, String::new());
    let mut output = BackendCompletion {
        backend_session_id: Some("native".into()),
        ..Default::default()
    };
    decoder.decode(&start(), &mut output, &sink).await?;
    decoder
        .decode(
            &event(
                "assistant.message",
                json!({
                    "messageId":"commentary","phase":"commentary","content":"Progress"
                }),
            ),
            &mut output,
            &sink,
        )
        .await?;
    decoder
        .finish_failure(crate::turn::TurnOutcome::Cancelled, &sink)
        .await?;
    assert!(decoder.finish_success(&sink).await.is_err());
    while let Some(event) = receiver.try_next()? {
        assert!(!matches!(
            event,
            BackendEvent::Message {
                update: MessageUpdate::Started {
                    classification: MessageClassification::Known(MessageRole::Response),
                    ..
                },
                ..
            }
        ));
    }
    Ok(())
}

#[tokio::test]
async fn tool_only_stream_and_synthetic_response_reopen_without_empty_or_duplicate_blocks()
-> anyhow::Result<()> {
    use crate::buffer::session::SessionPresentation;
    use crate::exchange::{Exchange, ExchangeEntry};
    use crate::timeline::TimelineEntry;
    use forge_buffer::identity::{DocumentId, ViewId};
    use forge_buffer::width::WidthProfile;

    let delivered = replay(vec![
        start(),
        event("assistant.message", json!({
            "messageId":"commentary","phase":"commentary","content":"Inspecting callers",
            "toolRequests":[{"toolCallId":"first","name":"read","arguments":{"path":"main.rs"}}]
        })),
        event("tool.execution_start", json!({"toolCallId":"first","toolName":"read"})),
        event("tool.execution_complete", json!({"toolCallId":"first","success":true,"result":{"content":"source"}})),
        event("assistant.turn_end", json!({"turnId":"0"})),
        event("assistant.turn_start", json!({"turnId":"1","interactionId":"interaction"})),
        event("assistant.message", json!({
            "messageId":"tool-only","content":"",
            "toolRequests":[
                {"toolCallId":"second","name":"read","arguments":{"path":"second.rs"}},
                {"toolCallId":"third","name":"read","arguments":{"path":"third.rs"}}
            ]
        })),
        event("tool.execution_start", json!({"toolCallId":"second","toolName":"read"})),
        event("tool.execution_start", json!({"toolCallId":"third","toolName":"read"})),
        event("tool.execution_complete", json!({"toolCallId":"second","success":true,"result":{"content":"second source"}})),
        event("tool.execution_complete", json!({"toolCallId":"third","success":true,"result":{"content":"third source"}})),
        event("assistant.turn_end", json!({"turnId":"1"})),
        event("assistant.turn_start", json!({"turnId":"2","interactionId":"interaction"})),
        event("assistant.message", json!({
            "messageId":"last","phase":"commentary","content":"Checked all callers"
        })),
        event("assistant.turn_end", json!({"turnId":"2"})),
        event("session.idle", json!({})),
    ], true).await?;

    let owner = delivered[0].scope().owner().clone();
    let mut exchange: Exchange = serde_json::from_value(json!({
        "id":"exchange", "session_id":owner.session_id, "agent_id":owner.agent_id,
        "ordinal":1, "prompt":"Inspect callers", "state":"running", "created_at_ms":0,
        "attributed_matches_checkpoint":false, "node_list":[]
    }))?;
    let entry = |exchange: &Exchange| TimelineEntry::Exchange {
        id: exchange.id.clone(),
        created_at_ms: 0,
        exchange: Box::new(exchange.clone()),
        agent_by_id: Default::default(),
    };
    let mut presentation = SessionPresentation::new("session".into());
    presentation.initialize(vec![entry(&exchange)])?;
    let document = DocumentId("native-stream".into());
    let opened = presentation.open(
        document.clone(),
        ViewId("native-view".into()),
        WidthProfile::default(),
    )?;
    for (index, update) in delivered.iter().enumerate() {
        exchange.observe_turn(update, index as i64)?;
        presentation.reconcile(vec![entry(&exchange)])?;
    }
    assert_eq!(exchange.tools().count(), 3);
    let commentary: Vec<_> = exchange
        .entry
        .iter()
        .filter_map(|entry| match entry {
            ExchangeEntry::Commentary { commentary } => Some(commentary),
            _ => None,
        })
        .collect();
    assert_eq!(commentary.len(), 2);
    assert_eq!(
        commentary[0]
            .tool
            .iter()
            .map(|tools| tools.store.iter().count())
            .sum::<usize>(),
        3
    );
    assert_eq!(
        commentary[1]
            .tool
            .iter()
            .map(|tools| tools.store.iter().count())
            .sum::<usize>(),
        0
    );
    assert_eq!(
        exchange
            .entry
            .iter()
            .filter(|entry| matches!(entry, ExchangeEntry::Response { .. }))
            .count(),
        1
    );
    presentation.sync(&document, opened.transcript.revision)?;
    let snapshot = presentation.snapshot(&document)?;
    assert_eq!(
        snapshot
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .filter(|row| row.contains("Checked all callers"))
            .count(),
        2
    );
    let reopened_exchange: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
    let mut reopened = SessionPresentation::new("session".into());
    reopened.initialize(vec![entry(&reopened_exchange)])?;
    let reopened_document = DocumentId("reopened-native".into());
    reopened.open(
        reopened_document.clone(),
        ViewId("reopened-view".into()),
        WidthProfile::default(),
    )?;
    assert_eq!(
        reopened
            .snapshot(&reopened_document)?
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .filter(|row| row.contains("Checked all callers"))
            .count(),
        2
    );
    Ok(())
}

#[tokio::test]
async fn cumulative_tool_output_replaces_while_argument_deltas_remain_input() -> anyhow::Result<()>
{
    let delivered = replay(vec![
        start(),
        event("assistant.tool_call_delta", json!({"toolCallId":"tool","inputDelta":"{\"value\":"})),
        event("assistant.tool_call_delta", json!({"toolCallId":"tool","inputDelta":"\"input\"}"})),
        event("assistant.message", json!({
            "messageId":"tool-only","content":"",
            "toolRequests":[{"toolCallId":"tool","name":"read","arguments":{"value":"input"}}]
        })),
        event("tool.execution_start", json!({"toolCallId":"tool","toolName":"read"})),
        event("tool.execution_partial_result", json!({"toolCallId":"tool","partialOutput":"first"})),
        event("tool.execution_partial_result", json!({"toolCallId":"tool","partialOutput":"first second"})),
        event("tool.execution_complete", json!({"toolCallId":"tool","success":true,"result":{"detailedContent":"first second complete"}})),
        event("tool.shell_output", json!({"toolCallId":"tool","sequence":4,"text":" late output"})),
        event("tool.shell_output", json!({"toolCallId":"tool","sequence":4,"text":" late output"})),
    ], false).await?;
    let mut output = String::new();
    let mut input = String::new();
    let mut outcomes = 0;
    for event in delivered {
        match event {
            BackendEvent::Tool {
                update: ToolUpdate::Output { update },
                ..
            } => match update {
                crate::backend::TextUpdate::Append(text) => output.push_str(&text),
                crate::backend::TextUpdate::Replace(text) => output = text,
            },
            BackendEvent::Tool {
                update:
                    ToolUpdate::Input {
                        update: crate::backend::TextUpdate::Append(text),
                    },
                ..
            } => input.push_str(&text),
            BackendEvent::Tool {
                update: ToolUpdate::Finished { .. },
                ..
            } => outcomes += 1,
            _ => {}
        }
    }
    assert_eq!(input, "{\"value\":\"input\"}");
    assert_eq!(output, "first second complete late output");
    assert_eq!(outcomes, 1);
    Ok(())
}

#[tokio::test]
async fn synthetic_response_preserves_text_beyond_transport_frame_size() -> anyhow::Result<()> {
    let text = "complete content λ ".repeat(forge_protocol::MAX_FRAME_BYTES / 4);
    let delivered = replay(
        vec![
            start(),
            event(
                "assistant.message",
                json!({
                    "messageId":"large","phase":"commentary","content":text
                }),
            ),
            event("assistant.turn_end", json!({"turnId":"0"})),
        ],
        true,
    )
    .await?;
    let snapshots: Vec<_> = delivered
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Message {
                update: MessageUpdate::Completed { text },
                ..
            } => Some(text),
            _ => None,
        })
        .collect();
    assert_eq!(snapshots.len(), 2);
    assert_eq!(snapshots[0], &text);
    assert_eq!(snapshots[1], &text);
    Ok(())
}
