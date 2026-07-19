use diff_review_harness::agent::AgentRunStatus;
use diff_review_harness::backend::codex::CodexBackend;
use diff_review_harness::backend::{
    Backend, BackendCatalogRequest, BackendInput, BackendLaunch, BackendRequest, PromptMode,
    ToolActivityKind,
};
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::plan::PlanPrompt;
use diff_review_harness::protocol::BrokerRequest;
use diff_review_harness::session::ExecutionMode;
use std::fs;
use std::path::Path;
use std::time::Duration;

fn git(workspace: &Path, args: &[&str]) {
    let status = std::process::Command::new("git")
        .args(args)
        .current_dir(workspace)
        .status()
        .unwrap();
    assert!(status.success(), "git {} failed", args.join(" "));
}

fn request(workspace: &Path, mode: PromptMode, text: &str) -> BackendRequest {
    BackendRequest {
        harness_session_id: "harness-session".into(),
        workspace: workspace.to_string_lossy().into_owned(),
        input: BackendInput::from_text(text),
        mode,
        model: "gpt-5.6-terra".into(),
        effort: "low".into(),
        context_window: None,
        fast_mode: true,
        execution_mode: if mode == PromptMode::Plan {
            ExecutionMode::Read
        } else {
            ExecutionMode::Write
        },
        backend_session_id: None,
    }
}

#[tokio::test]
#[ignore = "requires an installed and authenticated Codex CLI"]
async fn lists_backend_owned_codex_model_metadata() {
    let repository = tempfile::tempdir().unwrap();
    let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();

    let model_list = tokio::time::timeout(
        Duration::from_secs(30),
        backend.model_list(request(repository.path(), PromptMode::Chat, "")),
    )
    .await
    .expect("Codex model list timed out")
    .unwrap();

    assert!(!model_list.is_empty());
    assert!(model_list.iter().all(|model| !model.id.is_empty()));
    assert!(
        model_list
            .iter()
            .all(|model| model.selected_reasoning.is_none())
    );
    assert!(model_list.iter().any(|model| !model.reasoning.is_empty()));
    assert!(model_list.iter().any(|model| model.description.is_some()));
}

#[tokio::test]
#[ignore = "requires an installed and authenticated Codex CLI"]
async fn lists_provider_skills_and_complete_mcp_rows() {
    let repository = tempfile::tempdir().unwrap();
    let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
    let catalog_request = BackendCatalogRequest {
        harness_session_id: "catalog-session".into(),
        workspace: repository.path().to_string_lossy().into_owned(),
        execution_mode: ExecutionMode::Read,
        backend_session_id: None,
    };

    let skill_list = tokio::time::timeout(
        Duration::from_secs(30),
        backend.skill_list(catalog_request.clone()),
    )
    .await
    .expect("Codex skill list timed out")
    .unwrap();
    assert!(!skill_list.is_empty());
    assert!(skill_list.iter().all(|skill| !skill.name.is_empty()));
    assert!(skill_list.iter().all(|skill| skill.path.is_some()));

    let mcp_list = tokio::time::timeout(Duration::from_secs(30), backend.mcp_list(catalog_request))
        .await
        .expect("Codex MCP list timed out")
        .unwrap();
    assert!(!mcp_list.is_empty());
    assert!(mcp_list.iter().all(|server| !server.name.is_empty()));
    assert!(mcp_list.iter().any(|server| server.transport != "unknown"));
    assert!(mcp_list.iter().any(|server| !server.tools.is_empty()));
    assert!(
        mcp_list
            .iter()
            .all(|server| server.tools.iter().all(|tool| !tool.name.is_empty()))
    );
}

#[tokio::test]
#[ignore = "requires an installed and authenticated Codex CLI"]
async fn plans_without_writing_then_executes_and_forks_in_a_temporary_repository() {
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    git(
        repository.path(),
        &["config", "user.email", "harness@example.invalid"],
    );
    git(
        repository.path(),
        &["config", "user.name", "Harness Integration"],
    );
    fs::write(
        repository.path().join("README.md"),
        "# Harness integration\n",
    )
    .unwrap();
    git(repository.path(), &["add", "."]);
    git(repository.path(), &["commit", "-qm", "seed"]);

    let backend =
        CodexBackend::new(vec!["codex".into(), "app-server".into(), "--stdio".into()]).unwrap();
    let (answer_event_sink, mut answer_event_stream) = tokio::sync::mpsc::unbounded_channel();
    let mut answer_future = Box::pin(backend.prompt_stream(
        BackendRequest {
            model: "default".into(),
            ..request(
                repository.path(),
                PromptMode::Chat,
                "What is this repo? Answer in one sentence after reading README.md.",
            )
        },
        Some(answer_event_sink),
    ));
    tokio::time::timeout(Duration::from_secs(30), async {
        loop {
            tokio::select! {
                output = &mut answer_future => {
                    panic!("Codex completed before streaming assistant output: {output:#?}");
                }
                event = answer_event_stream.recv() => {
                    let event = event.expect("Codex closed the event stream before completion");
                    if event.kind == "assistant_message"
                        && event.text.as_deref().is_some_and(|text| !text.trim().is_empty())
                    {
                        break;
                    }
                }
            }
        }
    })
    .await
    .expect("Codex did not stream assistant output within 30 seconds");
    let answered = answer_future.await.unwrap();
    assert!(answered.event.iter().any(|event| {
        event.kind == "assistant_message"
            && event
                .text
                .as_deref()
                .is_some_and(|text| !text.trim().is_empty())
    }));
    assert_eq!(answered.runtime.provider, "Codex CLI");
    assert!(
        answered
            .runtime
            .model
            .as_deref()
            .is_some_and(|model| model != "default" && !model.is_empty())
    );
    assert!(
        answered
            .metrics
            .token_count
            .is_some_and(|token_count| token_count > 0),
        "Codex should report final turn token usage"
    );
    let model_list = backend
        .model_list(request(repository.path(), PromptMode::Chat, ""))
        .await
        .unwrap();
    let model = model_list
        .iter()
        .find(|model| model.id == "gpt-5.6-terra")
        .expect("gpt-5.6-terra is required for the fast Harness integration test");
    assert!(model.reasoning.iter().any(|effort| effort == "low"));

    let planning = backend.prompt(request(
        repository.path(),
        PromptMode::Plan,
        &PlanPrompt::draft(
            "Create a concise plan to add harness-integration.txt containing exactly `verified`. Do not modify files.",
        ),
    ));
    let steering = async {
        tokio::time::sleep(Duration::from_millis(50)).await;
        backend
            .steer(
                "Include the literal marker `STEERING_CONSTRAINT_7B3D` in the plan Overview."
                    .into(),
            )
            .await
    };
    let (planned, steered) = tokio::join!(planning, steering);
    let planned = planned.unwrap();
    steered.unwrap();
    assert!(
        planned
            .plan_markdown
            .as_deref()
            .is_some_and(|plan| !plan.trim().is_empty()),
        "Codex planning events did not contain a complete plan: {:#?}",
        planned.event
    );
    assert!(
        planned
            .plan_markdown
            .as_deref()
            .is_some_and(|plan| plan.contains("STEERING_CONSTRAINT_7B3D")),
        "Codex planning ignored the active-turn steering constraint: {:#?}",
        planned.plan_markdown
    );
    assert!(!repository.path().join("harness-integration.txt").exists());

    let mut execute = request(
        repository.path(),
        PromptMode::ExecutePlan,
        "Execute the accepted plan now. Create harness-integration.txt containing exactly `verified` followed by a newline, then call harness_goal_complete.",
    );
    execute.backend_session_id = planned.backend_session_id.clone();
    let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
    let executed = backend
        .prompt_stream(execute.clone(), Some(event_sink))
        .await
        .unwrap();
    let streamed_event_list: Vec<_> = std::iter::from_fn(|| event_stream.try_recv().ok()).collect();
    assert!(!streamed_event_list.is_empty());
    assert!(
        streamed_event_list.iter().any(|event| {
            event.kind == "tool"
                && event.activity.as_ref().is_some_and(|tool| {
                    tool.kind == ToolActivityKind::FileChange
                        && tool.status.as_deref() == Some("completed")
                        && tool.change.file.iter().any(|change| {
                            change.path.replace('\\', "/") == "harness-integration.txt"
                                && !change.diff.trim().is_empty()
                        })
                })
        }),
        "Codex did not stream a completed structured file change: {streamed_event_list:#?}"
    );
    assert_eq!(
        fs::read_to_string(repository.path().join("harness-integration.txt")).unwrap(),
        "verified\n"
    );
    assert!(executed.evidence.structured_complete || executed.evidence.native_complete);

    execute.backend_session_id = executed.backend_session_id;
    backend
        .goal_status(execute.clone(), Some("Complete the plan".into()), "paused")
        .await
        .unwrap();
    backend
        .goal_status(execute.clone(), Some("Complete the plan".into()), "active")
        .await
        .unwrap();
    backend
        .goal_status(execute.clone(), None, "cleared")
        .await
        .unwrap();
    let forked = backend.fork(execute).await.unwrap();
    assert!(!forked.backend_session_id.trim().is_empty());
}

#[tokio::test]
#[ignore = "requires an installed and authenticated Codex CLI with subagents enabled"]
async fn streams_structured_child_agent_lifecycle() {
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    fs::write(repository.path().join("README.md"), "# Child lifecycle\n").unwrap();
    let backend =
        CodexBackend::new(vec!["codex".into(), "app-server".into(), "--stdio".into()]).unwrap();
    let mut backend_request = request(
        repository.path(),
        PromptMode::Chat,
        "Spawn an explorer subagent to read README.md and report its heading. Wait for it to finish.",
    );
    backend_request.execution_mode = ExecutionMode::Read;
    let output = tokio::time::timeout(Duration::from_secs(90), backend.prompt(backend_request))
        .await
        .expect("Codex subagent turn exceeded 90 seconds")
        .unwrap();
    assert!(
        output
            .event
            .iter()
            .any(|event| event.kind == "agent_lifecycle"),
        "Codex did not emit a structured child lifecycle event: {:#?}",
        output.event
    );
    let lifecycle_list = output
        .event
        .iter()
        .filter(|event| event.kind == "agent_lifecycle")
        .map(|event| {
            serde_json::from_value::<diff_review_harness::agent::AgentLifecycleEvent>(
                event.data.clone(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let child_thread_list = lifecycle_list
        .iter()
        .filter_map(|lifecycle| lifecycle.provider_thread_id.as_deref())
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(
        child_thread_list.len(),
        1,
        "one requested child must not manufacture extra provider identities: {lifecycle_list:#?}"
    );
    let child_thread_id = child_thread_list.into_iter().next().unwrap();
    let child_completion_index = output
        .event
        .iter()
        .position(|event| {
            event.kind == "turn_completed"
                && event
                    .data
                    .pointer("/params/threadId")
                    .and_then(serde_json::Value::as_str)
                    == Some(child_thread_id)
        })
        .expect("the explorer must emit a terminal turn event for its provider identity");
    let parent_thread_id = output
        .backend_session_id
        .as_deref()
        .expect("the parent turn must preserve its provider thread identity");
    let parent_response_index = output
        .event
        .iter()
        .enumerate()
        .skip(child_completion_index + 1)
        .find_map(|(index, event)| {
            (event.kind == "assistant_message"
                && event
                    .data
                    .pointer("/params/threadId")
                    .and_then(serde_json::Value::as_str)
                    == Some(parent_thread_id)
                && event
                    .text
                    .as_deref()
                    .is_some_and(|text| text.contains("Child lifecycle")))
            .then_some(index)
        })
        .expect("the parent must synthesize the child result after the child completes");
    let parent_completion_index = output
        .event
        .iter()
        .enumerate()
        .skip(parent_response_index + 1)
        .find_map(|(index, event)| {
            (event.kind == "turn_completed"
                && event
                    .data
                    .pointer("/params/threadId")
                    .and_then(serde_json::Value::as_str)
                    == Some(parent_thread_id))
            .then_some(index)
        })
        .expect("the parent turn must complete after reporting the child result");
    assert!(
        child_completion_index < parent_response_index
            && parent_response_index < parent_completion_index,
        "the explorer must emit a terminal turn event for its provider identity: {:#?}",
        output.event
    );
}

#[tokio::test]
#[ignore = "requires an installed Codex CLI and the configured local-code-explorer definition"]
async fn broker_runs_the_configured_local_code_explorer_to_parent_completion() {
    let repository = tempfile::tempdir().unwrap();
    let data_root = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "-q"]);
    fs::write(
        repository.path().join("README.md"),
        "# Broker child lifecycle\n",
    )
    .unwrap();
    let mut broker = HarnessBroker::initialize(InitializeRequest {
        data_root: data_root.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: repository.path().to_string_lossy().into_owned(),
        client_id: "real-local-code-explorer-test".into(),
        backend: BackendLaunch {
            kind: "codex".into(),
            command: vec!["codex".into(), "app-server".into(), "--stdio".into()],
        },
        model: "gpt-5.6-terra".into(),
        effort: "low".into(),
        session_id: None,
        goal_max_turns: 20,
        lease_conflict_action: None,
    })
    .unwrap();
    let dispatch = tokio::time::timeout(
        Duration::from_secs(90),
        broker.dispatch(BrokerRequest {
            id: 1,
            method: "agent.start".into(),
            params: serde_json::json!({
                "definition": "local-code-explorer",
                "task": "Read README.md and report its heading to the parent."
            }),
        }),
    )
    .await
    .expect("the broker child-agent flow exceeded 90 seconds");
    assert!(
        dispatch.response.error.is_none(),
        "the real /agent flow failed: {:#?}",
        dispatch.response.error
    );
    let snapshot = broker.snapshot().unwrap();
    assert_eq!(
        snapshot.agent.run.len(),
        1,
        "one request must create one durable child run"
    );
    let run = &snapshot.agent.run[0];
    assert_eq!(run.definition, "local-code-explorer");
    assert_eq!(run.status, AgentRunStatus::Completed);
    assert!(run.parent_interaction_id.is_some());
    assert!(run.provider_thread_id.is_some());
    assert_eq!(
        snapshot.agent.turn.len(),
        1,
        "the child timeline must persist under its run"
    );
    let parent = snapshot
        .interaction
        .iter()
        .find(|interaction| Some(&interaction.id) == run.parent_interaction_id.as_ref())
        .expect("the child run must reference its spawning parent interaction");
    assert!(parent.node_list.iter().any(|node| match node {
        diff_review_harness::interaction::InteractionNode::MainSegment { segment } =>
            segment.response.as_deref().is_some_and(|response| {
                response.contains("Broker child lifecycle")
                    || response.contains("# Broker child lifecycle")
            }),
        _ => false,
    }));
}
