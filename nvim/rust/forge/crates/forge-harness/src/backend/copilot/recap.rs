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
    model: &str,
    history: &str,
) -> Result<String> {
    let directory = tempfile::Builder::new().prefix("forge-recap-").tempdir()?;
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
        .context("Recap provider startup timed out")??;
    let result = summarize(&client, workspace, model, history).await;
    tokio::time::timeout(Duration::from_secs(8), client.stop())
        .await
        .context("Recap provider shutdown timed out")?
        .map_err(|error| anyhow::anyhow!(error.to_string()))?;
    result
}

/// Own the temporary session and remove its stored state before the isolated client stops.
async fn summarize(client: &Client, workspace: &str, model: &str, history: &str) -> Result<String> {
    let mut config = SessionConfig::default();
    config.model = (model != "default").then(|| model.to_owned());
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
            .with_content(crate::backend::recap::INSTRUCTIONS),
    );
    let session = tokio::time::timeout(
        Duration::from_secs(20),
        client.create_session(
            config.with_permission_handler(Arc::new(github_copilot_sdk::handler::DenyAllHandler)),
        ),
    )
    .await
    .context("Recap session creation timed out")??;
    let result = session
        .send_and_wait(MessageOptions::new(format!(
            "Conversation to recap:\n{history}"
        )))
        .await
        .context("Generate Copilot recap")
        .and_then(|event| event.context("Recap completed without an assistant message"))
        .and_then(|event| {
            crate::backend::recap::validate(
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
    .context("Recap session cleanup timed out")?;
    cleanup.context("Remove temporary recap session")?;
    result
}
