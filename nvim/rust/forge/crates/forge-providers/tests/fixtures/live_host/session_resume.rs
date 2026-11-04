use super::collect::ProviderTestExt as _;
use anyhow::{Context, Result};
use forge_harness::provider::{MessageUpdate, ProviderEvent, ProviderInput, ProviderRequest};
use std::path::Path;
use std::sync::Arc;

/// Construct an isolated runtime while retaining the installed provider's authentication.
fn create_provider(
    provider: &str,
    root: &Path,
    request: &ProviderRequest,
) -> Result<super::collect::ProviderTestSession> {
    let command = match provider {
        "codex" => vec!["codex".into(), "app-server".into()],
        "copilot" => Vec::new(),
        _ => unreachable!("fixture provider is explicit"),
    };
    super::provider_session(provider, command, root, Path::new(&request.workspace))
}

/// Exercise preparation, restart, first submission, warm reuse, and durable resumption.
async fn session_lifecycle(provider: &str) -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let config = fixture.path().join("config");
    let data = fixture.path().join("data");
    let workspace = fixture.path().join("workspace");
    std::fs::create_dir_all(config.join("rulesync-global/.rulesync"))?;
    std::fs::create_dir_all(&workspace)?;
    std::fs::write(
        config.join("rulesync-global/.rulesync/mcp.json"),
        "{\"mcpServers\":{}}\n",
    )?;
    forge_harness::settings::SettingsStore::open(&data)?.configure_catalog(&config)?;
    let catalog = super::catalog_request(&workspace, &format!("{provider}-eager-resume"));
    let mut request = super::prompt_request(&catalog);
    request.access.sandbox = provider == "codex";
    let first = create_provider(provider, &data, &request)?;
    let mut missing = request.clone();
    missing.provider_session_id = Some(uuid::Uuid::new_v4().to_string().try_into()?);
    let unavailable = first.session_open(missing).await;
    eprintln!("{provider}: missing history result {unavailable:?}");
    assert!(
        unavailable.is_err(),
        "missing history was silently replaced"
    );
    assert!(matches!(
        unavailable
            .unwrap_err()
            .downcast_ref::<forge_harness::provider::ProviderError>(),
        Some(forge_harness::provider::ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::SessionMissing
    ));
    let prepared = first.session_open(request.clone()).await;
    if let Ok(Some(thread)) = &prepared {
        assert_eq!(
            first.session_open(request.clone()).await?.as_ref(),
            Some(thread)
        );
    }
    first.as_provider().shutdown().await?;
    let thread = prepared?.context("provider initialization omitted its thread")?;
    eprintln!("{provider}: prepared {}", thread.as_str());
    // Reopening a local session with no exchanges discards its provisional provider ID.
    request.provider_session_id = None;
    let second = create_provider(provider, &data, &request)?;
    let result: Result<forge_provider_api::ProviderSessionId> = async {
        let prepared = second.session_open(request.clone()).await?.context("missing prepared thread")?;
        assert_ne!(prepared, thread);
        let mut inventory = catalog.clone();
        inventory.access = request.access.clone();
        inventory.provider_session_id = Some(prepared.clone());
        second.as_provider().mcp_list(forge_provider_api::McpQuery::Observed(second.as_provider().query_context(&inventory).await?)).await?;
        second.as_provider().terminal_list(inventory.clone()).await?;
        // Exercise the live binding independently of a caller-supplied identity.
        request.provider_session_id = None;
        request.input = ProviderInput::from_text("Remember the exact token FORGE-RESUME-72931. Reply only with that token. Do not use any tools.");
        let output = second.session_prompt_and_collect(request.clone()).await?;
        let conversation = output.completion.provider_session_id.clone().context("missing conversation thread")?;
        assert_eq!(conversation, prepared);
        inventory.provider_session_id = Some(conversation.clone());
        assert_token(&output.event);
        request.provider_session_id = Some(conversation.clone());
        request.input = ProviderInput::from_text("What exact token did I ask you to remember? Reply only with the token. Do not use tools.");
        let warm = second.session_prompt_and_collect(request.clone()).await?;
        assert_eq!(warm.completion.provider_session_id.as_ref().map(|id| id.as_str()), Some(conversation.as_str()));
        assert_token(&warm.event);
        second.as_provider().terminal_list(inventory).await?;
        Ok(conversation)
    }.await;
    second.as_provider().shutdown().await?;
    request.provider_session_id = Some(result?);
    let third = create_provider(provider, &data, &request)?;
    let resumed: Result<()> = async {
        let resumed = third.session_open(request.clone()).await?;
        assert_eq!(resumed, request.provider_session_id);
        let mut inventory = catalog.clone();
        inventory.access = request.access.clone();
        inventory.provider_session_id = resumed;
        third.as_provider().terminal_list(inventory).await?;
        assert_token(&third.session_prompt_and_collect(request).await?.event);
        Ok(())
    }
    .await;
    third.as_provider().shutdown().await?;
    resumed?;
    eprintln!("{provider}: preparation, warm turns, cold resume, history and observers passed");
    Ok(())
}

/// Verify the provider retained conversation context across each transition.
fn assert_token(event: &[ProviderEvent]) {
    assert!(
        event.iter().any(|event| matches!(event,
            ProviderEvent::Message { update: MessageUpdate::Completed { text }, .. }
                if text.contains("FORGE-RESUME-72931")
        )),
        "provider did not retain the conversation token: {event:?}"
    );
}

#[tokio::test]
#[ignore = "requires installed authenticated Codex CLI"]
async fn codex_prepared_and_durable_session_lifecycle() -> Result<()> {
    session_lifecycle("codex").await
}

#[tokio::test]
#[ignore = "requires installed authenticated pinned Copilot CLI"]
async fn copilot_prepared_and_durable_session_lifecycle() -> Result<()> {
    session_lifecycle("copilot").await
}

/// Exercise recovery through the production service, including durable publication.
async fn service_recovery(provider: &str) -> Result<()> {
    use forge_harness::session::SessionStore;
    let fixture = tempfile::tempdir()?;
    let config = fixture.path().join("config");
    let data = fixture.path().join("data");
    let workspace = fixture.path().join("workspace");
    std::fs::create_dir_all(config.join("rulesync-global/.rulesync"))?;
    std::fs::create_dir_all(&workspace)?;
    std::fs::write(
        config.join("rulesync-global/.rulesync/mcp.json"),
        "{\"mcpServers\":{}}\n",
    )?;
    let command = if provider == "codex" {
        vec!["codex", "app-server"]
    } else {
        Vec::new()
    };
    let initialize: forge_harness::broker::InitializeRequest =
        serde_json::from_value(serde_json::json!({
            "config_dir":config,"data_root":data,"workspace":workspace,"client_id":"live-recovery",
            "provider":{"kind":provider,"command":command}
        }))?;
    let broker = forge_harness::broker::HarnessBroker::initialize(initialize.clone())?;
    let mut session = broker.snapshot()?.session;
    drop(broker);
    session.provider_session_id = Some(uuid::Uuid::new_v4().to_string());
    forge_harness::storage::SqliteStore::open(&data)?.save_session(&session)?;
    let diff = forge_diff::engine::DiffEngine::new(Default::default(), 1);
    let syntax = forge_diff::syntax::SyntaxEngine::new(diff.analysis_pool());
    let service = forge_harness::service::HarnessService::new(
        Arc::new(forge_git::store::RepositoryStore::default()),
        diff,
        syntax,
    );
    let result: Result<()> = async {
        let response = service.open_session(1, initialize.clone()).await?;
        anyhow::ensure!(
            response.result().is_some(),
            "open session failed: {response:?}"
        );
        let (sink, mut output) = forge_protocol::outbound::channel();
        service.dispatch(Some(session.id.clone()), forge_protocol::message::Request {
            id: 2, method: "session.rename".into(),
            params: serde_json::json!({"session_id":session.id,"name":"Renamed without an exchange"}),
        }, &sink).await?;
        let renamed = output.recv().await?.context("rename response missing")?;
        let renamed: serde_json::Value = serde_json::from_slice(renamed.bytes())?;
        anyhow::ensure!(renamed.get("error").is_none(), "rename failed: {renamed}");
        service.start_provider_session(&session.id, &sink).await?;
        loop {
            let frame = output
                .recv()
                .await?
                .context("service event stream closed")?;
            let event: serde_json::Value = serde_json::from_slice(frame.bytes())?;
            anyhow::ensure!(
                event["payload"]["kind"] != "error",
                "startup failed: {event}"
            );
            if event["event"] == "session_configured" {
                assert!(event["payload"]["session"]["provider_session_id"].is_string());
                assert_ne!(event["payload"]["session"]["provider_session_id"], serde_json::json!(session.provider_session_id));
                break;
            }
        }
        let store = forge_harness::storage::SqliteStore::open(&data)?;
        let initialized = store.load_session(&session.id)?.unwrap().provider_session_id;
        assert!(initialized.is_some());
        assert_ne!(initialized, session.provider_session_id);
        assert!(store.list_exchange(&session.id)?.is_empty());
        assert_eq!(store.load_session(&session.id)?.unwrap().name, "Renamed without an exchange");
        Ok(())
    }
    .await;
    service.shutdown(std::time::Duration::from_secs(10)).await?;
    result?;
    let reopened = forge_harness::broker::HarnessBroker::initialize(initialize)?;
    let reopened = reopened.snapshot()?.session;
    assert_eq!(reopened.name, "Renamed without an exchange");
    assert!(reopened.provider_session_id.is_none());
    eprintln!("{provider}: production startup recovered the missing empty session");
    Ok(())
}

#[tokio::test]
#[ignore = "requires installed authenticated Codex CLI"]
async fn codex_service_recovers_missing_empty_session() -> Result<()> {
    service_recovery("codex").await
}

#[tokio::test]
#[ignore = "requires installed authenticated pinned Copilot CLI"]
async fn copilot_service_recovers_missing_empty_session() -> Result<()> {
    service_recovery("copilot").await
}
