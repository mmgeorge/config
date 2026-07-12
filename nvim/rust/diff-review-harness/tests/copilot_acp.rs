use diff_review_harness::backend::acp::AcpBackend;
use diff_review_harness::backend::{Backend, BackendRequest, PromptMode, TrustPolicy};
use diff_review_harness::session::WriteMode;
use std::fs;

#[tokio::test]
#[ignore = "requires an installed and authenticated Copilot CLI"]
async fn answers_a_basic_read_only_prompt_through_the_default_acp_command() {
    let repository = tempfile::tempdir().unwrap();
    let status = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(repository.path())
        .status()
        .unwrap();
    assert!(status.success());
    fs::write(
        repository.path().join("README.md"),
        "# ACP integration fixture\n\nA small test repository.\n",
    )
    .unwrap();

    let backend = AcpBackend::new(vec!["copilot".into(), "--acp".into()]).unwrap();
    let output = backend
        .prompt(BackendRequest {
            workspace: repository.path().to_string_lossy().into_owned(),
            text: "What is this repo? Answer in one sentence after reading README.md.".into(),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            fast_mode: false,
            write_mode: WriteMode::Read,
            trust_profile: "workspace".into(),
            trust_policy: TrustPolicy::default(),
            backend_session_id: None,
        })
        .await
        .unwrap();

    assert!(output.backend_session_id.is_some());
    assert!(output.event.iter().any(|event| {
        event.kind == "assistant_message"
            && event
                .text
                .as_deref()
                .is_some_and(|text| !text.trim().is_empty())
    }));
}
