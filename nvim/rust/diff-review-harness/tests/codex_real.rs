use diff_review_harness::backend::BackendLaunch;
use diff_review_harness::broker::{HarnessBroker, InitializeRequest};
use diff_review_harness::protocol::BrokerRequest;
use diff_review_harness::session::ExecutionMode;
use serde_json::{Value, json};
use std::path::Path;
use std::process::Command;
use std::time::Duration;

const GEOPARQUET_PLAN_TURN_TIMEOUT: Duration = Duration::from_secs(600);

fn initialize_git_workspace(workspace: &Path) {
    std::fs::create_dir_all(workspace).unwrap();
    let status = Command::new("git")
        .args(["init", "--quiet"])
        .current_dir(workspace)
        .status()
        .unwrap();
    assert!(status.success());
}

fn trace_tail(data_root: &Path) -> String {
    let trace = std::fs::read_to_string(data_root.join("harness-trace.jsonl")).unwrap_or_default();
    let mut record_list = trace
        .lines()
        .rev()
        .take(40)
        .filter_map(|line| serde_json::from_str::<Value>(line).ok())
        .map(|record| {
            let event = record
                .get("event")
                .and_then(Value::as_str)
                .unwrap_or("unknown");
            let payload = record.get("payload").unwrap_or(&Value::Null);
            let detail_list = ["method", "status", "tool_name", "message", "text"]
                .into_iter()
                .filter_map(|field| {
                    payload.get(field).and_then(Value::as_str).map(|value| {
                        let compact = value.split_whitespace().collect::<Vec<_>>().join(" ");
                        format!(
                            "{field}={}",
                            &compact[..compact.floor_char_boundary(compact.len().min(160))]
                        )
                    })
                })
                .collect::<Vec<_>>();
            if detail_list.is_empty() {
                event.to_owned()
            } else {
                format!("{event} {}", detail_list.join(" "))
            }
        })
        .collect::<Vec<_>>();
    record_list.reverse();
    record_list.join("\n")
}

async fn initialize_real_codex_broker(
    data_root: &Path,
    workspace: &Path,
    client_id: &str,
    effort: &str,
    prefer_mini_model: bool,
) -> HarnessBroker {
    let mut broker = HarnessBroker::initialize(InitializeRequest {
        data_root: data_root.to_string_lossy().into_owned(),
        permission_file: None,
        workspace: workspace.to_string_lossy().into_owned(),
        client_id: client_id.into(),
        backend: BackendLaunch {
            kind: "codex".into(),
            command: vec!["codex".into(), "app-server".into()],
        },
        model: "default".into(),
        effort: effort.into(),
        session_id: None,
        new_session_name: None,
        goal_max_turns: 20,
        lease_conflict_action: None,
    })
    .unwrap();

    let model_response = broker
        .dispatch(BrokerRequest {
            id: 1,
            method: "backend.models".into(),
            params: Value::Null,
        })
        .await;
    assert!(model_response.response.error.is_none());
    let model_list = model_response
        .response
        .result
        .as_ref()
        .and_then(Value::as_array)
        .expect("Codex model catalog");
    let selected_model = model_list
        .iter()
        .find(|model| {
            if prefer_mini_model {
                model
                    .get("id")
                    .and_then(Value::as_str)
                    .is_some_and(|id| id.contains("mini"))
            } else {
                model
                    .get("is_default")
                    .and_then(Value::as_bool)
                    .unwrap_or(false)
            }
        })
        .or_else(|| model_list.first())
        .and_then(|model| model.get("id"))
        .and_then(Value::as_str)
        .expect("at least one Codex model")
        .to_owned();
    let configured_effort = if prefer_mini_model { "low" } else { effort };
    let configured = broker
        .dispatch(BrokerRequest {
            id: 2,
            method: "session.configure".into(),
            params: json!({
                "model": selected_model,
                "effort": configured_effort,
                "fast_mode": true
            }),
        })
        .await;
    assert!(configured.response.error.is_none());
    broker
}

#[tokio::test]
#[ignore = "requires an authenticated Codex CLI and performs a real model turn"]
async fn asks_for_feedback_then_creates_a_plan_without_native_collaboration_mode() {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    initialize_git_workspace(&workspace);
    std::fs::write(workspace.join("README.md"), "# Fixture\n").unwrap();

    let mut broker = initialize_real_codex_broker(
        &temporary.path().join("data"),
        &workspace,
        "real-codex-test",
        "low",
        true,
    )
    .await;

    let planned = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 3,
            method: "prompt.submit".into(),
            params: json!({
                "text": "/plan add a Testing section to README.md without changing any files. Your first action must call harness_plan_question with exactly one question asking which test type the section should describe. Give exactly two options named Unit tests and Integration tests. Do not call any plan create, edit, or submit tool until I answer."
            }),
        }),
    )
    .await
    .expect("real Codex planning timeout");
    assert!(
        planned.response.error.is_none(),
        "{:?}",
        planned.response.error
    );
    let paused_snapshot = broker.snapshot().unwrap();
    let paused_plan = paused_snapshot.active_plan.expect("paused planning state");
    assert_eq!(
        paused_plan.state,
        diff_review_harness::plan::PlanState::AwaitingInput
    );
    assert_eq!(paused_snapshot.artifact.len(), 1);
    assert_eq!(
        paused_snapshot.artifact[0].state,
        diff_review_harness::plan::PlanState::AwaitingInput
    );
    let elicitation = paused_plan.elicitation.expect("pending elicitation");
    let initial_revision = elicitation.revision;

    let replaced = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 4,
            method: "prompt.submit".into(),
            params: json!({
                "text": "Replace the pending question with exactly two options named Unit tests and Integration tests. Keep one question and do not choose an answer for me."
            }),
        }),
    )
    .await
    .expect("real Codex question replacement timeout");
    assert!(
        replaced.response.error.is_none(),
        "{:?}",
        replaced.response.error
    );
    let replaced_plan = broker
        .snapshot()
        .unwrap()
        .active_plan
        .expect("planning state after question replacement");
    let replaced_elicitation = replaced_plan.elicitation.expect("replaced elicitation");
    assert!(replaced_elicitation.revision > initial_revision);
    let question = replaced_elicitation
        .current_question()
        .expect("current planning question");
    assert_eq!(question.options.len(), 2);
    let question_id = question.id.clone();
    let selected_option = question
        .options
        .iter()
        .find(|option| option.label.to_ascii_lowercase().contains("integration"))
        .or_else(|| question.options.first())
        .expect("Codex planning question should provide a selectable answer")
        .label
        .clone();

    let answered = broker
        .dispatch(BrokerRequest {
            id: 5,
            method: "question.answer".into(),
            params: json!({
                "question_id": question_id,
                "response": {
                    "kind": "selected",
                    "option": selected_option,
                    "feedback": null
                }
            }),
        })
        .await;
    assert!(
        answered.response.error.is_none(),
        "{:?}",
        answered.response.error
    );

    let resumed = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 6,
            method: "question.continue".into(),
            params: Value::Null,
        }),
    )
    .await
    .expect("real Codex planning continuation timeout");
    assert!(
        resumed.response.error.is_none(),
        "{:?}",
        resumed.response.error
    );
    let snapshot = broker.snapshot().unwrap();
    assert_eq!(snapshot.artifact.len(), 1);
    assert!(std::path::Path::new(&snapshot.artifact[0].working_path).exists());
    let submitted_plan = snapshot.active_plan.expect("submitted plan");
    assert_eq!(
        submitted_plan.state,
        diff_review_harness::plan::PlanState::AwaitingReview
    );
    assert!(submitted_plan.elicitation.is_none());
    assert_eq!(submitted_plan.question_ledger.resolution.len(), 1);
    assert_eq!(
        snapshot
            .interaction
            .iter()
            .filter(|interaction| interaction.prompt.starts_with("Planning feedback:"))
            .count(),
        1,
        "feedback continuation should remain one Harness interaction"
    );
    assert!(snapshot.timeline.iter().any(|entry| matches!(
        entry,
        diff_review_harness::timeline::TimelineEntry::Status {
            status:
                diff_review_harness::session::state_machine::SessionPhase::AwaitingPlanReview {
                    ..
                },
            ..
        }
    )));
    assert_eq!(snapshot.session.execution_mode, ExecutionMode::Read);

    let new_session = broker
        .dispatch(BrokerRequest {
            id: 7,
            method: "session.new".into(),
            params: Value::Null,
        })
        .await;
    assert!(new_session.response.error.is_none());
    assert_eq!(
        broker.snapshot().unwrap().session.execution_mode,
        ExecutionMode::Read
    );
    let write_mode = broker
        .dispatch(BrokerRequest {
            id: 8,
            method: "session.execution_mode".into(),
            params: json!({ "mode": "write" }),
        })
        .await;
    assert!(write_mode.response.error.is_none());
    let write_result = tokio::time::timeout(
        Duration::from_secs(120),
        broker.dispatch(BrokerRequest {
            id: 9,
            method: "prompt.submit".into(),
            params: json!({
                "text": "Create mode-write-proof.txt in the workspace with exactly this content: `Harness native Write mode verified`. Do not change any other file."
            }),
        }),
    )
    .await
    .expect("real Codex Write-mode timeout");
    assert!(
        write_result.response.error.is_none(),
        "{:?}",
        write_result.response.error
    );
    let write_proof = std::fs::read_to_string(workspace.join("mode-write-proof.txt"))
        .expect("Write mode should create the requested workspace file");
    assert_eq!(write_proof.trim_end(), "Harness native Write mode verified");
}

#[tokio::test]
#[ignore = "requires an authenticated Codex CLI and performs real GeoParquet planning turns"]
async fn geoparquet_prompt_reaches_plan_review_after_scope_feedback() {
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    let crate_root = workspace.join("hello");
    let data_root = temporary.path().join("data");
    initialize_git_workspace(&workspace);
    std::fs::create_dir_all(crate_root.join("src")).unwrap();
    std::fs::write(
        crate_root.join("Cargo.toml"),
        "[package]\nname = \"hello\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[dependencies]\n",
    )
    .unwrap();
    std::fs::write(
        crate_root.join("src").join("main.rs"),
        "fn main() {\n    println!(\"Hello, world!\");\n}\n",
    )
    .unwrap();
    std::fs::create_dir_all(&data_root).unwrap();
    std::fs::write(
        data_root.join("harness-trace-config.json"),
        "{\"enabled\":true}",
    )
    .unwrap();

    let mut broker = initialize_real_codex_broker(
        &data_root,
        &workspace,
        "real-geoparquet-plan-test",
        "medium",
        false,
    )
    .await;
    let planned = match tokio::time::timeout(
        GEOPARQUET_PLAN_TURN_TIMEOUT,
        broker.dispatch(BrokerRequest {
            id: 3,
            method: "prompt.submit".into(),
            params: json!({
                "text": "/plan turn this into cli tool for geoparquet inspection use datafusion"
            }),
        }),
    )
    .await
    {
        Ok(planned) => planned,
        Err(error) => panic!(
            "GeoParquet planning timeout: {error:?}\ntrace tail:\n{}",
            trace_tail(&data_root)
        ),
    };
    assert!(
        planned.response.error.is_none(),
        "{:?}",
        planned.response.error
    );

    let mut next_request_id = 4;
    for _ in 0..3 {
        let snapshot = broker.snapshot().unwrap();
        let plan = snapshot.active_plan.expect("active GeoParquet plan");
        match plan.state {
            diff_review_harness::plan::PlanState::AwaitingReview => break,
            diff_review_harness::plan::PlanState::AwaitingInput => {
                let question = plan
                    .elicitation
                    .as_ref()
                    .and_then(|elicitation| elicitation.current_question())
                    .expect("pending GeoParquet scope question");
                let selected_option = question
                    .options
                    .iter()
                    .find(|option| {
                        let scope =
                            format!("{} {}", option.label, option.description).to_ascii_lowercase();
                        scope.contains("metadata")
                            && scope.contains("schema")
                            && (scope.contains("row") || scope.contains("count"))
                    })
                    .unwrap_or_else(|| {
                        panic!(
                            "GeoParquet scope should offer metadata, schema, and rows: {:?}",
                            question.options
                        )
                    })
                    .label
                    .clone();
                let question_id = question.id.clone();
                let answered = broker
                    .dispatch(BrokerRequest {
                        id: next_request_id,
                        method: "question.answer".into(),
                        params: json!({
                            "question_id": question_id,
                            "response": {
                                "kind": "selected",
                                "option": selected_option,
                                "feedback": null
                            }
                        }),
                    })
                    .await;
                next_request_id += 1;
                assert!(
                    answered.response.error.is_none(),
                    "{:?}",
                    answered.response.error
                );
                let continued = match tokio::time::timeout(
                    GEOPARQUET_PLAN_TURN_TIMEOUT,
                    broker.dispatch(BrokerRequest {
                        id: next_request_id,
                        method: "question.continue".into(),
                        params: Value::Null,
                    }),
                )
                .await
                {
                    Ok(continued) => continued,
                    Err(error) => panic!(
                        "GeoParquet feedback continuation timeout: {error:?}\ntrace tail:\n{}",
                        trace_tail(&data_root)
                    ),
                };
                next_request_id += 1;
                assert!(
                    continued.response.error.is_none(),
                    "{:?}",
                    continued.response.error
                );
            }
            state => panic!("GeoParquet planning stopped in {state:?}"),
        }
    }

    let snapshot = broker.snapshot().unwrap();
    let plan = snapshot.active_plan.expect("submitted GeoParquet plan");
    assert_eq!(
        plan.state,
        diff_review_harness::plan::PlanState::AwaitingReview
    );
    assert!(plan.submitted_version.is_some());
    assert!(plan.elicitation.is_none());
    assert_eq!(snapshot.artifact.len(), 1);
    assert!(Path::new(&snapshot.artifact[0].working_path).exists());
    assert!(snapshot.timeline.iter().any(|entry| matches!(
        entry,
        diff_review_harness::timeline::TimelineEntry::Status {
            status:
                diff_review_harness::session::state_machine::SessionPhase::AwaitingPlanReview {
                    ..
                },
            ..
        }
    )));
}
