use forge_harness::provider::ProviderAdapter as _;
use forge_harness::provider::TextGeneration;
use forge_harness::provider::codex::CodexProvider;
use forge_harness::provider::copilot::CopilotProvider;

const HISTORY: &str = "User: Fix terminal counts.\nAssistant: The counts are fixed and tests pass. Next validate the picker.";

#[tokio::test]
#[ignore = "requires authenticated Codex and makes one isolated naming request"]
async fn live_codex_session_name() {
    let workspace = tempfile::tempdir().unwrap();
    let provider = CodexProvider::new(vec!["codex".into(), "app-server".into()]).unwrap();
    let result = provider
        .generate_text(
            super::catalog_request(workspace.path(), "codex-name"),
            TextGeneration::SessionName,
            "gpt-5.6-sol",
            None,
            HISTORY,
        )
        .await;
    provider.shutdown().await.unwrap();
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
    let provider = CopilotProvider::new(vec!["copilot".into()]).unwrap();
    let result = provider
        .generate_text(
            super::catalog_request(workspace.path(), "copilot-name"),
            TextGeneration::SessionName,
            "gpt-5.6-sol",
            None,
            HISTORY,
        )
        .await;
    provider.shutdown().await.unwrap();
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
    let provider = CodexProvider::new(vec!["codex".into(), "app-server".into()]).unwrap();
    let result = provider
        .generate_text(
            super::catalog_request(workspace.path(), "codex-recap"),
            TextGeneration::Recap,
            "default",
            None,
            HISTORY,
        )
        .await;
    provider.shutdown().await.unwrap();
    assert!(!result.unwrap().is_empty());
}

#[tokio::test]
#[ignore = "requires authenticated Copilot and makes one isolated recap request"]
async fn live_copilot_recap() {
    let workspace = tempfile::tempdir().unwrap();
    let provider = CopilotProvider::new(vec!["copilot".into()]).unwrap();
    let result = provider
        .generate_text(
            super::catalog_request(workspace.path(), "copilot-recap"),
            TextGeneration::Recap,
            "gpt-5.6-sol",
            None,
            HISTORY,
        )
        .await;
    provider.shutdown().await.unwrap();
    assert!(!result.unwrap().is_empty());
}
