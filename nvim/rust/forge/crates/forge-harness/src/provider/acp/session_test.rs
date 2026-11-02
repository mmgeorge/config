use crate::permissions::acp::AcpPermissionContext;
use crate::provider::approval::PermissionCoordinator;
use crate::provider::*;
use crate::session::ApprovalMode;
use anyhow::Result;
use forge_providers::acp::{AcpHandler, session::AcpSessionState as NativeState};
use serde_json::{Value, json};
use std::sync::Arc;

struct AcpSessionState {
    native: NativeState,
    permission: Arc<AcpPermissionContext>,
}
impl AcpSessionState {
    fn new(kind: ProviderKind, coordinator: Arc<PermissionCoordinator>) -> Self {
        let permission = Arc::new(AcpPermissionContext::new(coordinator));
        Self {
            native: NativeState::new(kind, permission.clone()),
            permission,
        }
    }
    async fn authorize(&self, value: Value) -> Result<Value> {
        Ok(self
            .native
            .request("session/request_permission", value)
            .await?)
    }
}
impl std::ops::Deref for AcpSessionState {
    type Target = NativeState;
    fn deref(&self) -> &NativeState {
        &self.native
    }
}

async fn activate(state: &AcpSessionState, session: &str) -> events::ProviderEventStream {
    let owner = ExecutionOwner::root(session);
    let scope = InvocationScope::new(owner.clone(), "exchange");
    let (sink, stream) = events::channel(scope.clone());
    state
        .permission
        .configure(ApprovalMode::Ask, None, sink.clone())
        .await;
    state.begin("", sink).await.unwrap();
    stream
}

fn drain(stream: &mut events::ProviderEventStream) -> Vec<ProviderEvent> {
    std::iter::from_fn(|| stream.try_next().transpose())
        .collect::<std::result::Result<Vec<_>, _>>()
        .unwrap()
}

fn tool_events(
    events: &[ProviderEvent],
    id: &str,
) -> (
    ToolInvocation,
    Option<String>,
    Option<ToolOutcome>,
    ProviderChangeSet,
) {
    let mut invocation = None;
    let mut output = None;
    let mut outcome = None;
    let mut changes = ProviderChangeSet::default();
    for event in events {
        if let ProviderEvent::Tool { address, update } = event {
            if address.tool_id.as_str() != id {
                continue;
            }
            match update {
                ToolUpdate::Requested { invocation: value } => invocation = Some(value.clone()),
                ToolUpdate::Output {
                    update: TextUpdate::Replace(text),
                } => output = Some(text.clone()),
                ToolUpdate::Output {
                    update: TextUpdate::Append(text),
                } => output.get_or_insert_default().push_str(text),
                ToolUpdate::Changes { changes: value } => changes = value.clone(),
                ToolUpdate::Finished { outcome: value } => outcome = Some(value.clone()),
                _ => {}
            }
        }
    }
    (
        invocation.expect("requested ACP tool"),
        output,
        outcome,
        changes,
    )
}

#[tokio::test]
async fn manual_compaction_retains_operation_scope_without_a_provider_turn() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let state = AcpSessionState::new(
        ProviderKind::Claude,
        PermissionCoordinator::transient(fixture.path())?,
    );
    let scope = EventScope::SessionOperation {
        owner: ExecutionOwner::root("session"),
        operation_id: "compact".into(),
    };
    let (sink, mut stream) = events::channel(scope.clone());
    state.begin_compaction("native", sink).await?;
    assert!(state.has_active_work().await);
    for update in [
        json!({"sessionUpdate":"agent_message_chunk","content":{"type":"text","text":"Not a response"}}),
        json!({"sessionUpdate":"compaction_update","compactionId":"native-compact","status":"in_progress"}),
        json!({"sessionUpdate":"compaction_summary_chunk","compactionId":"native-compact","content":{"type":"text","text":"Retained"}}),
        json!({"sessionUpdate":"compaction_update","compactionId":"native-compact","status":"completed"}),
        json!({"sessionUpdate":"usage_update","used":20,"size":100}),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"native","update":update}),
            )
            .await?;
    }
    state.finish_compaction().await;
    assert!(!state.has_active_work().await);
    let events = drain(&mut stream);
    assert_eq!(events.len(), 3);
    assert!(events.iter().all(
        |event| matches!(event, ProviderEvent::Compaction { update, .. } if update.manual)
            && event.scope() == scope
    ));
    assert!(!state.has_active_work().await);
    assert_eq!(
        state
            .completion()
            .await
            .metrics
            .context_usage
            .as_ref()
            .unwrap()
            .used,
        20
    );
    Ok(())
}

#[tokio::test]
async fn message_chunks_complete_with_one_identity_and_preserve_interruption() -> Result<()> {
    for outcome in [
        crate::turn::TurnOutcome::Completed,
        crate::turn::TurnOutcome::Cancelled,
    ] {
        let fixture = tempfile::tempdir()?;
        let state = AcpSessionState::new(
            ProviderKind::Claude,
            PermissionCoordinator::transient(fixture.path())?,
        );
        let mut stream = activate(&state, "test").await;
        for text in ["First ", "answer"] {
            state
                .notification(
                    "session/update",
                    json!({"update":{
                        "sessionUpdate":"agent_message_chunk","content":{"type":"text","text":text}
                    }}),
                )
                .await?;
        }
        state.finish(outcome).await?;
        let mut messages = Vec::new();
        while let Some(update) = stream.try_next()? {
            if let ProviderEvent::Message { address, update } = update {
                messages.push((address, update));
            }
        }
        assert!(messages.iter().all(|message| message.0 == messages[0].0));
        let last = messages.last().unwrap();
        if outcome == crate::turn::TurnOutcome::Completed {
            assert!(messages.iter().any(|message| matches!(
                message.1,
                MessageUpdate::Classified {
                    role: MessageRole::Response
                }
            )));
            assert!(matches!(&last.1, MessageUpdate::Completed { text } if text == "First answer"));
        } else {
            assert!(
                !messages
                    .iter()
                    .any(|message| matches!(message.1, MessageUpdate::Classified { .. }))
            );
            assert!(matches!(last.1, MessageUpdate::Interrupted));
        }
    }
    Ok(())
}

#[tokio::test]
async fn child_callbacks_preserve_approval_ownership_and_root_metadata() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let permission = PermissionCoordinator::transient(fixture.path())?;
    permission
        .store()
        .write()
        .unwrap()
        .save(r#"{"permission":{"edit":{"*":"ask"}}}"#)?;
    let state = Arc::new(AcpSessionState::new(
        ProviderKind::Claude,
        permission.clone(),
    ));
    let _stream = activate(&state, "test").await;
    let (sink, replacement) = events::channel(state.scope().await?);
    state
        .permission
        .configure(ApprovalMode::Ask, None, sink.clone())
        .await;
    state.begin("root", sink).await?;
    let mut stream = replacement;
    state
        .notification(
            "session/update",
            json!({"sessionId":"root","update":{
            "sessionUpdate":"subagent_spawned","subagentSessionId":"child"}}),
        )
        .await?;
    for update in [
        json!({"sessionUpdate":"config_option_update","configOptions":[{"id":"model","currentValue":"child-model"}]}),
        json!({"sessionUpdate":"available_commands_update","availableCommands":[{"name":"child-command"}]}),
        json!({"sessionUpdate":"usage_update","used":10,"size":100}),
        json!({"sessionUpdate":"compaction_update","compactionId":"child-compaction","status":"completed"}),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"child","update":update}),
            )
            .await?;
    }
    state.notification("_claude/sdkMessage", json!({"sessionId":"child","message":{
            "type":"result","uuid":"child-result","usage":{"input_tokens":10,"cache_read_input_tokens":0,
                "cache_creation_input_tokens":0,"output_tokens":5},"duration_ms":20}})).await?;
    assert_eq!(state.catalog().await, json!({}));
    assert!(state.completion().await.metrics.context_usage.is_none());
    let initial = drain(&mut stream);
    assert_eq!(initial.len(), 2);
    let waiting_state = state.clone();
    let waiting = tokio::spawn(async move {
        waiting_state.authorize(json!({"sessionId":"child",
            "toolCall":{"kind":"edit","rawInput":{"file_path":"child.txt"}},
            "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
        })).await
    });
    let approval = tokio::time::timeout(std::time::Duration::from_secs(1), stream.recv())
        .await??
        .unwrap();
    let ProviderEvent::Approval {
        scope,
        update: ApprovalUpdate::Requested(approval),
    } = approval
    else {
        panic!("expected approval")
    };
    let child = state.address("child").await.unwrap();
    assert_eq!(child.thread_id, "child");
    assert_eq!(scope, child.scope);
    assert_eq!(approval.execution_owner.as_ref(), Some(&child.scope.owner));
    let pending = permission.pending_list()?.remove(0);
    permission
        .resolve(
            &pending.id,
            &[crate::provider::approval::ApprovalAnswer {
                item_id: pending.item_list[0].id.clone(),
                choice_id: "deny_once".into(),
            }],
            None,
        )
        .await?;
    assert_eq!(waiting.await??["outcome"]["optionId"], "deny");
    assert!(
        state
            .authorize(json!({"sessionId":"unowned"}))
            .await
            .is_err()
    );
    state.finish(crate::turn::TurnOutcome::Completed).await?;
    assert!(!state.has_active_work().await);
    assert!(drain(&mut stream).iter().any(|event| matches!(event, ProviderEvent::Agent { update: AgentUpdate::Lifecycle { event }, .. } if event.status == crate::agent::AgentExecutionState::Interrupted)));
    Ok(())
}

#[tokio::test]
async fn permission_callbacks_preserve_native_target_namespaces() {
    let fixture = tempfile::tempdir().unwrap();
    let permission = PermissionCoordinator::transient(fixture.path()).unwrap();
    permission
        .store()
        .write()
        .unwrap()
        .save(r#"{"permission":{"edit":{"*":"ask"},"mcp":{"*":"ask"},"webfetch":{"*":"ask"}}}"#)
        .unwrap();
    let state = Arc::new(AcpSessionState::new(
        ProviderKind::Claude,
        permission.clone(),
    ));
    let _stream = activate(&state, "test").await;
    for (tool, expected) in [
        (
            json!({"kind":"edit","rawInput":{"TargetFile":"target.txt"}}),
            ("edit", "target.txt"),
        ),
        (
            json!({"kind":"other","_meta":{"claudeCode":{"toolName":"mcp__github__search"}},
                "rawInput":{"command":"remote argument"}}),
            ("mcp", "github/search"),
        ),
        (
            json!({"kind":"other","_meta":{"mcp":{"server":"docs","tool":"lookup"}},
                "rawInput":{"path":"remote argument"}}),
            ("mcp", "docs/lookup"),
        ),
        (
            json!({"kind":"fetch","rawInput":{"url":"https://example.test/docs"}}),
            ("webfetch", "https://example.test/docs"),
        ),
    ] {
        let waiting_state = state.clone();
        let waiting = tokio::spawn(async move {
            waiting_state.authorize(json!({"toolCall":tool,
                "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]
            })).await.unwrap()
        });
        let approval = tokio::time::timeout(std::time::Duration::from_secs(1), async {
            loop {
                if let Some(approval) = permission.pending_list().unwrap().into_iter().next() {
                    break approval;
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        assert_eq!(approval.item_list.len(), 1);
        let rule = approval.item_list[0]
            .choice_list
            .iter()
            .find(|choice| choice.id == "allow_exact")
            .unwrap();
        assert_eq!(
            rule.rule_list,
            vec![(expected.0.to_owned(), expected.1.to_owned())]
        );
        permission
            .resolve(
                &approval.id,
                &[crate::provider::approval::ApprovalAnswer {
                    item_id: approval.item_list[0].id.clone(),
                    choice_id: "deny_once".into(),
                }],
                None,
            )
            .await
            .unwrap();
        assert_eq!(waiting.await.unwrap()["outcome"]["optionId"], "deny");
    }
    let internal = tokio::time::timeout(std::time::Duration::from_secs(1), state.authorize(json!({
            "toolCall":{"kind":"other","_meta":{"mcp":{"server":"forge-harness-control","tool":"goal_complete"}}},
            "options":[{"kind":"allow_once","optionId":"internal-allow"},{"kind":"reject_once","optionId":"deny"}]
        }))).await.unwrap().unwrap();
    assert_eq!(internal["outcome"]["optionId"], "internal-allow");
    assert!(permission.pending_list().unwrap().is_empty());
}

#[tokio::test]
async fn callback_updates_preserve_tool_identity_and_compaction_summary() {
    let fixture = tempfile::tempdir().unwrap();
    let state = AcpSessionState::new(
        ProviderKind::Claude,
        PermissionCoordinator::transient(fixture.path()).unwrap(),
    );
    state
        .notification(
            "session/update",
            json!({"update":{"sessionUpdate":"available_commands_update","availableCommands":[]}}),
        )
        .await
        .unwrap();
    assert_eq!(state.catalog().await["availableCommands"], json!([]));
    let mut stream = activate(&state, "test").await;
    for update in [
        json!({"sessionUpdate":"tool_call","toolCallId":"command-1","title":"Run check","kind":"execute","status":"in_progress"}),
        json!({"sessionUpdate":"tool_call_update","toolCallId":"command-1","status":"completed","content":[{"type":"content","content":{"type":"text","text":"passed"}}]}),
        json!({"sessionUpdate":"compaction_update","compactionId":"compact-1","status":"in_progress","_meta":{"trigger":"manual"}}),
        json!({"sessionUpdate":"compaction_summary_chunk","compactionId":"compact-1","content":{"type":"text","text":"Retained summary"}}),
        json!({"sessionUpdate":"compaction_update","compactionId":"compact-1","status":"completed"}),
        json!({"sessionUpdate":"usage_update","used":50000,"size":200000}),
    ] {
        state
            .notification(
                "session/update",
                json!({"sessionId":"native-1","update":update}),
            )
            .await
            .unwrap();
    }
    let events = drain(&mut stream);
    let (invocation, output, outcome, _) = tool_events(&events, "command-1");
    assert_eq!(invocation.title, "Run check");
    assert!(matches!(invocation.kind, ToolActivityKind::Command));
    assert_eq!(output.as_deref(), Some("passed"));
    assert!(matches!(outcome, Some(ToolOutcome::Succeeded)));
    assert_eq!(
        state
            .completion()
            .await
            .metrics
            .context_usage
            .as_ref()
            .unwrap()
            .remaining_percent,
        75
    );
    let compaction = events
        .iter()
        .rev()
        .find_map(|event| match event {
            ProviderEvent::Compaction { update, .. } if update.id == "compact-1" => Some(update),
            _ => None,
        })
        .unwrap();
    assert!(compaction.manual);
    assert_eq!(compaction.thread_id, "native-1");
    assert_eq!(
        compaction.result.summary_content.as_deref(),
        Some("Retained summary")
    );
    assert_eq!(compaction.state, CompactionState::Completed);
    state.notification("_claude/sdkMessage", json!({"sessionId":"native-1","message":{
            "type":"result","uuid":"call-1","duration_ms":1234,
            "usage":{"input_tokens":10,"cache_read_input_tokens":20,"cache_creation_input_tokens":30,"output_tokens":40}
        }})).await.unwrap();
    let events = drain(&mut stream);
    let usage = events
        .iter()
        .find_map(|event| match event {
            ProviderEvent::Usage { update, .. } => Some(update),
            _ => None,
        })
        .unwrap();
    assert_eq!(usage.usage.input, Some(60));
    assert_eq!(usage.usage.cached_input, Some(20));
    assert_eq!(usage.usage.reasoning, None);
    assert!(events.iter().any(|event| matches!(event, ProviderEvent::Turn { update: TurnUpdate::Summary { summary }, .. } if summary.duration_ms == 1234)));
}

#[tokio::test]
async fn sparse_tool_completion_preserves_acp_file_diffs() {
    let fixture = tempfile::tempdir().unwrap();
    let state = AcpSessionState::new(
        ProviderKind::Claude,
        PermissionCoordinator::transient(fixture.path()).unwrap(),
    );
    let mut stream = activate(&state, "test").await;
    state.notification("session/update", json!({"update":{
            "sessionUpdate":"tool_call","toolCallId":"edit-1","kind":"edit","title":"Edit files",
            "content":[
                {"type":"diff","path":"/project/config.txt","oldText":"before\n","newText":"after\n"},
                {"type":"diff","path":"/project/new.txt","oldText":null,"newText":"created\n"},
                {"type":"diff","path":"/project/invalid.txt","newText":"unknown baseline"},
                {"type":"content","content":{"type":"text","text":"Edited"}}
            ]
        }})).await.unwrap();
    state
        .notification(
            "session/update",
            json!({"update":{
                "sessionUpdate":"tool_call_update","toolCallId":"edit-1","status":"completed"
            }}),
        )
        .await
        .unwrap();
    let events = drain(&mut stream);
    let (_, output, outcome, changes) = tool_events(&events, "edit-1");
    assert!(matches!(outcome, Some(ToolOutcome::Succeeded)));
    assert_eq!(output.as_deref(), Some("Edited"));
    assert_eq!(changes.file.len(), 2);
    let changed = &changes.file[0];
    assert_eq!(changed.kind, ProviderChangeKind::Update);
    assert!(changed.diff.contains("-before\n+after\n"));
    let added = &changes.file[1];
    assert_eq!(added.kind, ProviderChangeKind::Add);
    assert!(added.diff.starts_with("--- /dev/null\n"));
    assert!(added.diff.contains("+created\n"));
}

#[tokio::test]
async fn raw_tool_completion_preserves_command_output_and_errors() {
    let fixture = tempfile::tempdir().unwrap();
    let state = AcpSessionState::new(
        ProviderKind::Antigravity,
        PermissionCoordinator::transient(fixture.path()).unwrap(),
    );
    let mut stream = activate(&state, "test").await;
    for update in [
        json!({"sessionUpdate":"tool_call","toolCallId":"command","kind":"execute","title":"Check","status":"in_progress"}),
        json!({"sessionUpdate":"tool_call_update","toolCallId":"command","status":"completed","content":[],
                "rawOutput":{"combinedOutput":"COMMAND_RESULT","exitCode":7}}),
        json!({"sessionUpdate":"tool_call_update","toolCallId":"error","status":"failed","rawOutput":"PROVIDER_ERROR"}),
        json!({"sessionUpdate":"tool_call_update","toolCallId":"structured","status":"completed","rawOutput":{"result":[1,2]}}),
        json!({"sessionUpdate":"tool_call_update","toolCallId":"explicit","status":"completed","rawOutput":"DUPLICATED_TEXT",
                "content":[{"type":"content","content":{"type":"text","text":"RENDERED_TEXT"}}]}),
    ] {
        state
            .notification("session/update", json!({"update":update}))
            .await
            .unwrap();
    }
    let events = drain(&mut stream);
    let (command, output, outcome, _) = tool_events(&events, "command");
    assert_eq!(command.title, "Check");
    assert_eq!(output.as_deref(), Some("COMMAND_RESULT\nExit code: 7"));
    assert!(matches!(outcome, Some(ToolOutcome::Succeeded)));
    assert_eq!(
        tool_events(&events, "error").1.as_deref(),
        Some("PROVIDER_ERROR")
    );
    let structured = tool_events(&events, "structured").1.unwrap();
    assert_eq!(
        serde_json::from_str::<Value>(&structured).unwrap(),
        json!({"result":[1,2]})
    );
    assert_eq!(
        tool_events(&events, "explicit").1.as_deref(),
        Some("RENDERED_TEXT")
    );
}

#[tokio::test]
async fn sparse_permission_uses_tool_snapshot_and_isolated_session_identity() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let permission = PermissionCoordinator::transient(fixture.path())?;
    permission.store().write().unwrap().set_rule_list(&[(
        crate::permissions::document::CATEGORY_EDIT.into(),
        "one.rs".into(),
        crate::permissions::document::PermissionDecision::Allow,
    )])?;
    let state = AcpSessionState::new(ProviderKind::Claude, permission.clone());
    let _stream = activate(&state, "session").await;
    state.notification("session/update",json!({"sessionId":"native","update":{
            "sessionUpdate":"tool_call","toolCallId":"edit","kind":"edit","locations":[{"path":"one.rs"}]
        }})).await?;
    let approval = json!({"sessionId":"native","toolCall":{"toolCallId":"edit"},
            "options":[{"kind":"allow_once","optionId":"allow"},{"kind":"reject_once","optionId":"deny"}]});
    let result = tokio::time::timeout(std::time::Duration::from_secs(1), state.authorize(approval))
        .await??;
    assert_eq!(result["outcome"]["optionId"], "allow");
    assert!(permission.pending_list()?.is_empty());
    state.finish(crate::turn::TurnOutcome::Completed).await?;
    assert!(!state.has_active_work().await);
    assert!(
        state
            .authorize(json!({"sessionId":"other","toolCall":{"toolCallId":"edit"}}))
            .await
            .is_err()
    );
    Ok(())
}
