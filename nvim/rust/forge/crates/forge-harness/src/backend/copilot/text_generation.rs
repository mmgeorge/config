use crate::backend::TextGeneration;
use anyhow::{Context, Result};
use github_copilot_sdk::{
    CliProgram, Client, ClientMode, ClientOptions, MessageOptions, SessionConfig,
    SystemMessageConfig,
};
use std::{sync::Arc, time::Duration};

/// Summarize captured history without attaching to or sending input to the main session.
pub(super) async fn generate(
    command: &[String],
    workspace: &str,
    purpose: TextGeneration,
    model: &str,
    history: &str,
) -> Result<String> {
    let directory = tempfile::Builder::new().prefix("forge-text-").tempdir()?;
    let mut options = ClientOptions::new()
        .with_cwd(workspace)
        .with_mode(ClientMode::Empty)
        .with_base_directory(directory.path())
        .with_use_logged_in_user(true);
    if let Some(program) = command.first() {
        options = options
            .with_program(CliProgram::Path(program.into()))
            .with_prefix_args(command.iter().skip(1));
    }
    let client = tokio::time::timeout(Duration::from_secs(20), Client::start(options))
        .await
        .context("Isolated provider startup timed out")??;
    let result = summarize(&client, workspace, purpose, model, history).await;
    tokio::time::timeout(Duration::from_secs(8), client.stop())
        .await
        .context("Isolated provider shutdown timed out")?
        .map_err(|error| anyhow::anyhow!(error.to_string()))?;
    result
}

/// Own the temporary session and remove its stored state before the isolated client stops.
async fn summarize(
    client: &Client,
    workspace: &str,
    purpose: TextGeneration,
    model: &str,
    history: &str,
) -> Result<String> {
    let mut config = SessionConfig::default();
    config.model = (model != "default").then(|| model.to_owned());
    if purpose == TextGeneration::SessionName && model != "default" && model != "auto" {
        let catalog = tokio::time::timeout(Duration::from_secs(20), client.list_models())
            .await
            .context("Isolated model discovery timed out")??;
        let descriptor = catalog
            .into_iter()
            .map(super::model_descriptor)
            .find(|entry| entry.id == model)
            .context("Selected model is missing from the Copilot model catalog")?;
        config.reasoning_effort = crate::backend::text_generation::lowest_effort(&descriptor)?;
    }
    config.streaming = Some(true);
    config.working_directory = Some(workspace.into());
    config.available_tools = Some(Vec::new());
    config.tools = Some(Vec::new());
    config.enable_config_discovery = Some(false);
    config.enable_skills = Some(false);
    config.enable_file_hooks = Some(false);
    config.hooks = Some(false);
    config.enable_session_store = Some(false);
    config.skip_custom_instructions = Some(true);
    config.system_message = Some(
        SystemMessageConfig::new()
            .with_mode("replace")
            .with_content(purpose.instructions()),
    );
    let session = tokio::time::timeout(
        Duration::from_secs(20),
        client.create_session(
            config.with_permission_handler(Arc::new(github_copilot_sdk::handler::DenyAllHandler)),
        ),
    )
    .await
    .context("Isolated session creation timed out")??;
    let result = tokio::time::timeout(
        Duration::from_secs(60),
        session.send_and_wait(MessageOptions::new(format!("Conversation:\n{history}"))),
    )
    .await
    .context("Isolated text generation timed out")
    .and_then(|result| result.map_err(anyhow::Error::from))
    .and_then(|event| event.context("Text generation completed without an assistant message"))
    .and_then(|event| {
        purpose.validate(
            event
                .data
                .get("content")
                .and_then(serde_json::Value::as_str)
                .unwrap_or_default(),
        )
    });
    let cleanup = tokio::time::timeout(Duration::from_secs(8), async {
        let aborted = if result.is_err() {
            session.abort().await
        } else {
            Ok(())
        };
        let disconnected = session.disconnect().await;
        let deleted = client.delete_session(session.id()).await;
        deleted.and(disconnected).and(aborted)
    })
    .await
    .context("Isolated session cleanup timed out")?;
    cleanup.context("Remove temporary generation session")?;
    result
}
