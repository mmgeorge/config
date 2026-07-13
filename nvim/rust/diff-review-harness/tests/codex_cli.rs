use diff_review_harness::backend::codex::CodexBackend;
use diff_review_harness::backend::{Backend, BackendRequest, PromptMode, TrustPolicy};
use diff_review_harness::plan::PlanPrompt;
use diff_review_harness::session::WriteMode;
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
        workspace: workspace.to_string_lossy().into_owned(),
        text: text.into(),
        mode,
        model: "gpt-5.4-mini".into(),
        effort: "low".into(),
        fast_mode: true,
        write_mode: if mode == PromptMode::Plan {
            WriteMode::Read
        } else {
            WriteMode::Write
        },
        trust_profile: "workspace".into(),
        trust_policy: TrustPolicy::default(),
        backend_session_id: None,
    }
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
        .find(|model| model.id == "gpt-5.4-mini")
        .expect("gpt-5.4-mini is required for the fast Harness integration test");
    assert!(model.effort.iter().any(|effort| effort == "low"));

    let planned = backend
        .prompt(request(
            repository.path(),
            PromptMode::Plan,
            &PlanPrompt::draft(
                "Create a concise plan to add harness-integration.txt containing exactly `verified`. Do not modify files.",
            ),
        ))
        .await
        .unwrap();
    assert!(
        planned
            .plan_markdown
            .as_deref()
            .is_some_and(|plan| !plan.trim().is_empty()),
        "Codex planning events did not contain a complete plan: {:#?}",
        planned.event
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
    assert!(event_stream.try_recv().is_ok());
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
    assert!(!forked.trim().is_empty());
}
