use crate::backend::TextGeneration;
use anyhow::{Context, Result};
use github_copilot_sdk::{
    CliProgram, Client, ClientOptions, MessageOptions, SessionConfig, SystemMessageConfig,
};
use std::{sync::Arc, time::Duration};

/// Summarize captured history without attaching to or sending input to the main session.
pub(super) async fn generate(
    command: &[String],
    workspace: &str,
    purpose: TextGeneration,
    model: &str,
    reasoning_effort: Option<&str>,
    history: &str,
) -> Result<String> {
    let mut options = ClientOptions::new().with_cwd(workspace);
    if let Some(program) = command.first() {
        options = options
            .with_program(CliProgram::Path(program.into()))
            .with_prefix_args(command.iter().skip(1));
    }
    let client = tokio::time::timeout(Duration::from_secs(20), Client::start(options))
        .await
        .context("isolated Copilot SDK startup exceeded 20 seconds")?
        .context("start isolated Copilot SDK client")?;
    let result = summarize(
        &client,
        workspace,
        purpose,
        model,
        reasoning_effort,
        history,
    )
    .await;
    let shutdown = tokio::time::timeout(Duration::from_secs(8), client.stop())
        .await
        .context("isolated Copilot SDK shutdown exceeded 8 seconds")
        .and_then(|result| result.context("stop isolated Copilot SDK client"));
    match (result, shutdown) {
        (Err(error), Err(shutdown)) => {
            Err(error.context(format!("Copilot SDK shutdown also failed: {shutdown:#}")))
        }
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        (Ok(text), Ok(())) => Ok(text),
    }
}

/// Own the temporary session and remove its stored state before the isolated client stops.
async fn summarize(
    client: &Client,
    workspace: &str,
    purpose: TextGeneration,
    model: &str,
    reasoning_effort: Option<&str>,
    history: &str,
) -> Result<String> {
    let mut config = SessionConfig::default();
    config.model = (model != "default").then(|| model.to_owned());
    if let Some(effort) = reasoning_effort {
        config.reasoning_effort = Some(effort.to_owned());
    } else if purpose == TextGeneration::SessionName && model != "default" && model != "auto" {
        let catalog = tokio::time::timeout(Duration::from_secs(20), client.list_models())
            .await
            .context("isolated Copilot model discovery exceeded 20 seconds")?
            .context("list isolated Copilot models")?;
        let descriptor = catalog
            .into_iter()
            .map(super::model_descriptor)
            .find(|entry| entry.id == model)
            .with_context(|| {
                format!("selected model {model} is absent from the Copilot model catalog")
            })?;
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
    .context("isolated Copilot session creation exceeded 20 seconds")?
    .context("create isolated Copilot text-generation session")?;
    let result = tokio::time::timeout(
        Duration::from_secs(60),
        session.send_and_wait(MessageOptions::new(purpose.input(history))),
    )
    .await
    .with_context(|| {
        format!(
            "Copilot session {} text generation exceeded 60 seconds",
            session.id()
        )
    })
    .and_then(|result| result.context("send isolated Copilot text-generation prompt"))
    .and_then(|event| {
        event.with_context(|| {
            format!(
                "Copilot session {} completed text generation without an assistant message",
                session.id()
            )
        })
    })
    .and_then(|event| {
        let content = event
            .data
            .get("content")
            .and_then(serde_json::Value::as_str)
            .with_context(|| {
                format!(
                    "Copilot session {} text-generation event {} omitted string content",
                    session.id(),
                    event.id
                )
            })?;
        purpose.validate(content).with_context(|| {
            format!(
                "validate Copilot session {} text-generation event {}",
                session.id(),
                event.id
            )
        })
    });
    let cleanup = tokio::time::timeout(Duration::from_secs(8), async {
        let mut failures = Vec::new();
        if result.is_err()
            && let Err(error) = session.abort().await
        {
            failures.push(format!("abort: {error}"));
        }
        if let Err(error) = session.disconnect().await {
            failures.push(format!("disconnect: {error}"));
        }
        if let Err(error) = client.delete_session(session.id()).await {
            failures.push(format!("delete: {error}"));
        }
        anyhow::ensure!(
            failures.is_empty(),
            "Copilot session {} cleanup failed: {}",
            session.id(),
            failures.join(", ")
        );
        Ok(())
    })
    .await
    .with_context(|| {
        format!(
            "Copilot session {} cleanup exceeded 8 seconds",
            session.id()
        )
    })
    .and_then(|result| result);
    match (result, cleanup) {
        (Err(error), Err(cleanup)) => {
            Err(error.context(format!("Copilot session cleanup also failed: {cleanup:#}")))
        }
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        (Ok(text), Ok(())) => Ok(text),
    }
}
