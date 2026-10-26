use forge_harness::backend::codex::CodexBackend;
use forge_harness::backend::copilot::CopilotBackend;
use forge_harness::backend::{Backend, TextGeneration};

const HISTORY: &str = "User: Fix terminal counts.\nAssistant: The counts are fixed and tests pass. Next validate the picker.";

#[tokio::test]
#[ignore = "requires authenticated Codex and makes one isolated naming request"]
async fn live_codex_session_name() {
    let workspace = tempfile::tempdir().unwrap();
    let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
    let result = backend
        .generate_text(
            super::catalog_request(workspace.path(), "codex-name"),
            TextGeneration::SessionName,
            "gpt-5.6-sol",
            None,
            HISTORY,
        )
        .await;
    backend.shutdown().await.unwrap();
    assert!(
        TextGeneration::SessionName
            .validate(&result.unwrap())
            .is_ok()
    );
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and makes one isolated naming request"]
async fn live_copilot_session_name() {
    let workspace = tempfile::tempdir().unwrap();
    let backend = CopilotBackend::new(vec!["copilot".into()]).unwrap();
    let result = backend
        .generate_text(
            super::catalog_request(workspace.path(), "copilot-name"),
            TextGeneration::SessionName,
            "gpt-5.6-sol",
            None,
            HISTORY,
        )
        .await;
    backend.shutdown().await.unwrap();
    assert!(
        TextGeneration::SessionName
            .validate(&result.unwrap())
            .is_ok()
    );
}

#[tokio::test]
#[ignore = "requires authenticated Codex and makes one isolated recap request"]
async fn live_codex_recap() {
    let workspace = tempfile::tempdir().unwrap();
    let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
    let result = backend
        .generate_text(
            super::catalog_request(workspace.path(), "codex-recap"),
            TextGeneration::Recap,
            "default",
            None,
            HISTORY,
        )
        .await;
    backend.shutdown().await.unwrap();
    assert!(!result.unwrap().is_empty());
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and makes one isolated recap request"]
async fn live_copilot_recap() {
    let workspace = tempfile::tempdir().unwrap();
    let backend = CopilotBackend::new(vec!["copilot".into()]).unwrap();
    let result = backend
        .generate_text(
            super::catalog_request(workspace.path(), "copilot-recap"),
            TextGeneration::Recap,
            "gpt-5.6-sol",
            None,
            HISTORY,
        )
        .await;
    backend.shutdown().await.unwrap();
    assert!(!result.unwrap().is_empty());
}
