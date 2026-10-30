use super::*;
use crate::backend::{
    BackendOutput, CompactionState, TextUpdate, ToolActivityKind, ToolInvocation,
};

async fn collect(native: Vec<SessionEvent>, tasks: Option<Value>) -> anyhow::Result<BackendOutput> {
    let binding = scope();
    let (sink, mut receiver) = crate::backend::events::channel(binding.clone());
    let execution = async {
        let mut decoder = CopilotEventDecoder::new(binding, String::new());
        let mut completion = BackendCompletion {
            backend_session_id: Some("native-session".into()),
            runtime: crate::backend::BackendRuntime {
                provider: "Copilot CLI".into(),
                model: None,
            },
            ..Default::default()
        };
        for event in native {
            decoder.decode(&event, &mut completion, &sink).await?;
        }
        if let Some(tasks) = tasks {
            decoder
                .publish_task_snapshot(tasks, "copilot-root".into(), &mut completion, &sink)
                .await?;
        }
        Ok::<_, crate::backend::BackendError>(completion)
    };
    tokio::pin!(execution);
    let mut delivered = Vec::new();
    loop {
        tokio::select! {
            update = receiver.recv() => {
                if let Some(update) = update? { delivered.push(update); }
            }
            completed = &mut execution => {
                let completion = completed?;
                while let Some(update) = receiver.try_next()? { delivered.push(update); }
                return Ok(BackendOutput { event: delivered, completion });
            }
        }
    }
}

fn invocation(event: &BackendEvent) -> Option<&ToolInvocation> {
    match event {
        BackendEvent::Tool {
            update: ToolUpdate::Requested { invocation },
            ..
        } => Some(invocation),
        _ => None,
    }
}

#[tokio::test]
async fn file_tool_failures_and_read_only_diffs_never_claim_workspace_changes() -> anyhow::Result<()>
{
    let diff = "diff --git a/file b/file\n--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+new\n";
    for (name, arguments, success, expected) in [
        (
            "apply_patch",
            Value::Null,
            false,
            ToolActivityKind::FileChange,
        ),
        (
            "bash",
            json!({"command":"git diff"}),
            true,
            ToolActivityKind::ToolCall,
        ),
        (
            "str_replace_editor",
            json!({"command":"view","path":"file"}),
            true,
            ToolActivityKind::ToolCall,
        ),
    ] {
        let output = collect(vec![
            start(),
            event("tool.execution_start", json!({"toolCallId":"tool","toolName":name,"arguments":arguments})),
            event("tool.execution_complete", json!({"toolCallId":"tool","success":success,"result":{"detailedContent":diff}})),
        ], None).await?;
        assert_eq!(
            output.event.iter().find_map(invocation).unwrap().kind,
            expected
        );
        assert!(!output.completion.evidence.workspace_changed);
        assert!(!output.event.iter().any(|event| matches!(event,
            BackendEvent::Tool { update: ToolUpdate::Changes { changes }, .. } if !changes.is_empty())));
        if !success {
            assert!(output.event.iter().any(|event| matches!(
                event,
                BackendEvent::Tool {
                    update: ToolUpdate::Finished {
                        outcome: ToolOutcome::Failed { .. }
                    },
                    ..
                }
            )));
        }
    }
    Ok(())
}

#[tokio::test]
async fn malformed_file_diffs_report_errors_without_losing_tool_completion() -> anyhow::Result<()> {
    let output = collect(
        vec![
            start(),
            event(
                "tool.execution_start",
                json!({"toolCallId":"patch","toolName":"apply_patch"}),
            ),
            event(
                "tool.execution_complete",
                json!({"toolCallId":"patch","success":true,
            "result":{"detailedContent":"diff --git a/file b/file\n@@ -1 +1 @@\n-old\n"}}),
            ),
        ],
        None,
    )
    .await?;
    assert!(output.event.iter().any(|event| matches!(event,
        BackendEvent::Diagnostic { diagnostic, .. } if diagnostic.message.contains("truncated saved patch hunk"))));
    assert!(output.event.iter().any(|event| matches!(
        event,
        BackendEvent::Tool {
            update: ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded
            },
            ..
        }
    )));
    assert!(!output.completion.evidence.workspace_changed);
    Ok(())
}

#[tokio::test]
async fn structured_changes_upgrade_generic_tools_and_survive_nameless_completion()
-> anyhow::Result<()> {
    let output = collect(vec![
        start(),
        event("tool.execution_start", json!({"toolCallId":"tool","toolName":"custom_edit"})),
        event("tool.execution_start", json!({"toolCallId":"tool","changes":[{"path":"file","diff":"@@ -1 +1 @@\n-old\n+new\n"}]})),
        event("tool.execution_complete", json!({"toolCallId":"tool","success":true,"result":{"content":"done"}})),
    ], None).await?;
    let mut store = crate::turn::ToolStore::default();
    for update in &output.event {
        if let BackendEvent::Tool { address, update } = update {
            store.merge(address, update, 1)?;
        }
    }
    assert!(output.completion.evidence.workspace_changed);
    assert_eq!(store.get("tool").unwrap().kind, "file_change");
    assert_eq!(store.get("tool").unwrap().change.file.len(), 1);
    Ok(())
}

#[test]
fn compaction_retains_summary_failure_and_identity_across_replays() -> anyhow::Result<()> {
    let started = event(
        "session.compaction_start",
        json!({"model":"summary-model","conversationTokens":100}),
    );
    let completed = event(
        "session.compaction_complete",
        json!({
            "success":false,"error":"summary request failed","summaryContent":"Retain the contract.",
            "preCompactionTokens":100,"postCompactionTokens":40,"tokensRemoved":60,"messagesRemoved":0,
            "compactionTokensUsed":{"inputTokens":100,"outputTokens":20,"cacheReadTokens":0,
                "copilotUsage":{"totalNanoAiu":1.5,"tokenDetails":[{"tokenType":"input","tokenCount":100}]}}
        }),
    );
    let first = super::super::decode_compaction(&started, "root", None)?;
    let settled = super::super::decode_compaction(&completed, "root", Some(&first))?;
    let replayed = super::super::decode_compaction(&completed, "root", Some(&settled))?;
    assert_eq!(replayed.id, started.id);
    assert_eq!(replayed.state, CompactionState::Failed);
    assert_eq!(
        replayed.result.summary_content.as_deref(),
        Some("Retain the contract.")
    );
    assert_eq!(
        replayed.result.error.as_deref(),
        Some("summary request failed")
    );
    assert_eq!(replayed.result.messages_removed, Some(0));
    assert_eq!(replayed.result.model.as_deref(), Some("summary-model"));
    assert_eq!(
        replayed.result.before_context.unwrap().conversation_tokens,
        Some(100)
    );
    let usage = replayed.result.compaction_tokens_used.unwrap();
    assert_eq!(usage.cached_input, Some(0));
    assert_eq!(usage.billing.unwrap().total_nano_aiu, Some(1.5));
    let orphan = super::super::decode_compaction(
        &event("session.compaction_complete", json!({"success":true})),
        "root",
        None,
    )?;
    assert_eq!(orphan.result.pre_compaction_tokens, None);
    Ok(())
}

#[tokio::test]
async fn publishes_root_model_before_turn_and_ignores_child_model() -> anyhow::Result<()> {
    let mut child = event("session.start", json!({"model":"child-model"}));
    child.agent_id = Some("child".into());
    let output = collect(
        vec![
            event("session.start", json!({"model":"root-model"})),
            child,
            event("session.start", json!({"model":"root-model"})),
        ],
        None,
    )
    .await?;
    assert_eq!(
        output.completion.runtime.model.as_deref(),
        Some("root-model")
    );
    let resolved: Vec<_> = output
        .event
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Runtime {
                update: RuntimeUpdate::Resolved { runtime },
                ..
            } => Some(runtime),
            _ => None,
        })
        .collect();
    assert_eq!(resolved.len(), 1);
    assert_eq!(resolved[0].model.as_deref(), Some("root-model"));
    Ok(())
}

#[tokio::test]
async fn reports_every_model_call_and_addresses_late_child_usage() -> anyhow::Result<()> {
    let mut child_started = event("assistant.turn_start", json!({"turnId":"child-turn"}));
    child_started.agent_id = Some("child".into());
    let mut child_finished = event("assistant.turn_end", json!({"turnId":"child-turn"}));
    child_finished.agent_id = Some("child".into());
    let mut child_usage = event(
        "assistant.usage",
        json!({"apiCallId":"child-call","inputTokens":10,"outputTokens":20}),
    );
    child_usage.agent_id = Some("child".into());
    let mut native = vec![start(), child_started];
    for id in ["call-one", "call-two"] {
        native.push(event("assistant.usage", json!({
            "apiCallId":id,"inputTokens":1000,"cacheReadTokens":900,"reasoningTokens":200,"outputTokens":280,
        })));
    }
    native.extend([child_finished, child_usage]);
    let output = collect(native, None).await?;
    let usage: Vec<_> = output
        .event
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Usage { address, update } => Some((address, update)),
            _ => None,
        })
        .collect();
    assert_eq!(usage.len(), 3);
    assert_eq!(usage[1].1.id, "call-two");
    assert_eq!(usage[2].0.thread_id, "child");
    assert_eq!(usage[2].1.usage.reasoning, None);
    assert_eq!(usage[0].1.usage.non_reasoning_output(), Some(80));
    Ok(())
}

#[tokio::test]
async fn maps_usage_tasks_and_subagent_events() -> anyhow::Result<()> {
    let output = collect(
        vec![
            start(),
            event(
                "session.usage_info",
                json!({"currentTokens":20,"tokenLimit":100}),
            ),
            event(
                "subagent.started",
                json!({"agentId":"child","agentName":"explorer"}),
            ),
        ],
        Some(json!({"rows":[{"id":"one","title":"Inspect","status":"completed"}]})),
    )
    .await?;
    assert_eq!(
        output
            .completion
            .metrics
            .context_usage
            .unwrap()
            .remaining_percent,
        80
    );
    assert!(output.event.iter().any(|event| matches!(event,
        BackendEvent::Checklist { update, .. } if update.complete)));
    assert!(output.event.iter().any(|event| matches!(event,
        BackendEvent::Agent { update: crate::backend::AgentUpdate::Lifecycle { event }, .. }
            if event.status == crate::agent::AgentExecutionState::Running)));
    Ok(())
}

#[tokio::test]
async fn late_background_output_preserves_original_tool_address_and_outcome() -> anyhow::Result<()>
{
    let output = collect(
        vec![
            start(),
            event(
                "tool.execution_start",
                json!({"toolCallId":"shell","toolName":"bash","arguments":{"command":"task"}}),
            ),
            event(
                "tool.execution_complete",
                json!({"toolCallId":"shell","success":true,"result":{"content":"done"}}),
            ),
            event("assistant.turn_end", json!({"turnId":"0"})),
            event(
                "assistant.turn_start",
                json!({"turnId":"1","interactionId":"next"}),
            ),
            event(
                "tool.execution_partial_result",
                json!({"toolCallId":"shell","partialOutput":"late"}),
            ),
        ],
        None,
    )
    .await?;
    let tool: Vec<_> = output
        .event
        .iter()
        .filter_map(|event| match event {
            BackendEvent::Tool { address, update } => Some((address, update)),
            _ => None,
        })
        .collect();
    let address = tool.first().unwrap().0;
    assert!(tool.iter().all(|(observed, _)| *observed == address));
    assert!(tool.iter().any(|(_, update)| matches!(update,
        ToolUpdate::Output { update: TextUpdate::Replace(text) } if text == "late")));
    assert_eq!(
        tool.iter()
            .filter(|(_, update)| matches!(update, ToolUpdate::Started))
            .count(),
        1
    );
    assert_eq!(
        tool.iter()
            .filter(|(_, update)| matches!(
                update,
                ToolUpdate::Finished {
                    outcome: ToolOutcome::Succeeded
                }
            ))
            .count(),
        1
    );
    Ok(())
}

/// Keep full request replay and partial execution snapshots on one immutable call.
#[tokio::test]
async fn tool_request_replay_preserves_invocation_through_failure_and_next_call()
-> anyhow::Result<()> {
    let request = json!({
        "messageId":"request-one","content":"",
        "toolRequests":[{"toolCallId":"first","name":"read","arguments":{"path":"first.txt"}}]
    });
    let output = collect(
        vec![
            start(),
            event("assistant.message", request.clone()),
            event("assistant.message", request),
            event(
                "tool.execution_start",
                json!({"toolCallId":"first","toolName":"read"}),
            ),
            event(
                "tool.execution_start",
                json!({"toolCallId":"first","toolName":"read","arguments":{"path":"first.txt"}}),
            ),
            event(
                "tool.execution_complete",
                json!({"toolCallId":"first","success":false,"error":{"message":"not found"}}),
            ),
            event(
                "tool.execution_complete",
                json!({"toolCallId":"first","success":false,"error":{"message":"not found"}}),
            ),
            event(
                "assistant.message",
                json!({"messageId":"request-two","content":"",
                    "toolRequests":[{"toolCallId":"second","name":"read","arguments":{"path":"second.txt"}}]}),
            ),
            event(
                "tool.execution_start",
                json!({"toolCallId":"second","toolName":"read"}),
            ),
            event(
                "tool.execution_complete",
                json!({"toolCallId":"second","success":true,"result":{"content":"found"}}),
            ),
        ],
        None,
    )
    .await?;
    let mut store = crate::turn::ToolStore::default();
    let mut request_count = 0;
    for observation in &output.event {
        if let BackendEvent::Tool { address, update } = observation {
            store.merge(address, update, 1)?;
            if matches!(update, ToolUpdate::Requested { .. }) {
                request_count += 1;
            }
        }
    }
    assert_eq!(request_count, 2);
    assert_eq!(
        store.get("first").unwrap().state(),
        crate::turn::ToolState::Failed
    );
    assert_eq!(
        store.get("second").unwrap().state(),
        crate::turn::ToolState::Completed
    );
    assert_eq!(
        store.execution("first").unwrap().invocation.arguments,
        json!({"path":"first.txt"})
    );
    assert_eq!(
        store.execution("second").unwrap().invocation.arguments,
        json!({"path":"second.txt"})
    );
    assert_eq!(store.get("second").unwrap().output.as_ref(), "found");
    Ok(())
}
