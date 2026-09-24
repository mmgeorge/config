use super::json_rpc::CodexJsonRpc;
use crate::backend::{BackendOutput, TextGeneration};
use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use std::time::Duration;

/// Generate text in a tool-free ephemeral thread with captured visible history.
pub(super) async fn generate(
    process: &mut CodexJsonRpc,
    output: &mut BackendOutput,
    workspace: &str,
    purpose: TextGeneration,
    model: &str,
    history: &str,
) -> Result<String> {
    let mut config = json!({"web_search":"disabled", "skills.include_instructions":false,
        "orchestrator.skills.enabled":false, "tools.update_plan.enabled":false,
        "tools.experimental_request_user_input.enabled":false});
    for feature in [
        "apps",
        "code_mode",
        "code_mode_only",
        "context_management",
        "current_time_reminder",
        "deferred_executor",
        "enable_fanout",
        "goals",
        "hooks",
        "image_generation",
        "memories",
        "multi_agent",
        "multi_agent_v2",
        "plugins",
        "request_permissions_tool",
        "shell_snapshot",
        "shell_tool",
        "standalone_web_search",
        "token_budget",
        "tool_suggest",
        "unified_exec",
        "view_image",
    ] {
        config[format!("features.{feature}")] = json!(false);
    }
    let started = tokio::time::timeout(Duration::from_secs(20), async {
        let effective = process
            .request(
                "config/read",
                json!({"includeLayers":false,"cwd":workspace}),
                output,
            )
            .await?;
        let effective = effective
            .get("config")
            .and_then(Value::as_object)
            .context("Missing effective Codex config")?;
        let mut servers = serde_json::Map::new();
        if let Some(names) = effective.get("mcp_servers").and_then(Value::as_object) {
            for name in names.keys() {
                servers.insert(name.clone(), json!({"enabled":false}));
            }
        }
        config["mcp_servers"] = Value::Object(servers);
        if purpose == TextGeneration::SessionName {
            let selected = if model == "default" {
                effective.get("model").and_then(Value::as_str)
            } else {
                Some(model)
            };
            let catalog = super::CodexBackend::model_catalog(process, output).await?;
            let descriptor = catalog
                .iter()
                .find(|entry| selected.map_or(entry.is_default, |model| entry.id == model))
                .context("Selected model is missing from the Codex model catalog")?;
            if let Some(effort) = crate::backend::text_generation::lowest_effort(descriptor)? {
                config["model_reasoning_effort"] = json!(effort);
            }
        }
        process
            .request(
                "thread/start",
                json!({"model": (model != "default").then_some(model),
            "cwd":workspace,"approvalPolicy":"never","sandbox":"read-only","ephemeral":true,
            "dynamicTools":[],"config":config,"baseInstructions":purpose.instructions()}),
                output,
            )
            .await
    })
    .await
    .context("Isolated thread creation timed out")??;
    let thread = started
        .pointer("/thread/id")
        .and_then(Value::as_str)
        .context("Missing isolated thread identity")?;
    let result = tokio::time::timeout(Duration::from_secs(60), async {
        let request_id = process.send_request("turn/start", json!({"threadId":thread,
            "input":[{"type":"text","text":format!("Conversation:\n{history}"),"text_elements":[]}]})).await?;
        let mut text = String::new();
        loop {
            let event = process.read_message(output).await?;
            if let Some(result) = CodexJsonRpc::request_result(&event, request_id, "turn/start") { result?; }
            let params = &event["params"];
            if params["threadId"].as_str() != Some(thread) { continue; }
            match event["method"].as_str() {
                Some("item/completed") if params["item"]["type"] == "agentMessage" => {
                    text = params["item"]["text"].as_str().unwrap_or_default().to_owned();
                }
                Some("turn/completed") => {
                    ensure!(params["turn"]["status"] == "completed", "Isolated turn failed: {}", params["turn"]["error"]);
                    return purpose.validate(&text);
                }
                _ => {}
            }
        }
    }).await.context("Isolated text generation timed out").and_then(|result| result);
    tokio::time::timeout(
        Duration::from_secs(8),
        process.request("thread/unsubscribe", json!({"threadId":thread}), output),
    )
    .await
    .context("Isolated thread cleanup timed out")??;
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::{SinkExt, StreamExt};

    #[tokio::test]
    async fn session_name_uses_selected_model_and_minimum_effort_in_an_ephemeral_thread()
    -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = crate::backend::approval::PermissionCoordinator::transient(&workspace)?;
        let trace = std::sync::Arc::new(crate::trace::TraceStore::open(fixture.path())?);
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for method in [
                "config/read",
                "model/list",
                "thread/start",
                "turn/start",
                "thread/unsubscribe",
            ] {
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], method);
                let result = match method {
                    "config/read" => {
                        json!({"config":{"model":"different-default","model_reasoning_effort":"high","mcp_servers":{"test":{}}}})
                    }
                    "model/list" => {
                        json!({"data":[{"model":"selected-model","supportedReasoningEfforts":[{"reasoningEffort":"high"},{"reasoningEffort":"minimal"},{"reasoningEffort":"low"}]}]})
                    }
                    "thread/start" => {
                        assert_eq!(request["params"]["model"], "selected-model");
                        assert_eq!(
                            request["params"]["config"]["model_reasoning_effort"],
                            "minimal"
                        );
                        assert_eq!(request["params"]["ephemeral"], true);
                        assert_eq!(
                            request["params"]["config"]["mcp_servers"]["test"]["enabled"],
                            false
                        );
                        assert_eq!(request["params"]["config"]["features.shell_tool"], false);
                        json!({"thread":{"id":"temporary"}})
                    }
                    _ => {
                        assert_eq!(request["params"]["threadId"], "temporary");
                        json!({})
                    }
                };
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":request["id"],"result":result})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
                if method == "turn/start" {
                    for notification in [
                        json!({"method":"item/completed","params":{"threadId":"temporary","item":{"type":"agentMessage","text":"Terminal status fixes"}}}),
                        json!({"method":"turn/completed","params":{"threadId":"temporary","turn":{"status":"completed"}}}),
                    ] {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                notification.to_string().into(),
                            ))
                            .await
                            .unwrap();
                    }
                }
            }
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            crate::session::ExecutionMode::Read,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        let result = generate(
            &mut process,
            &mut BackendOutput::default(),
            &workspace,
            TextGeneration::SessionName,
            "selected-model",
            "User: Fix terminal status",
        )
        .await?;
        assert_eq!(result, "Terminal status fixes");
        tokio::time::timeout(Duration::from_secs(2), server).await??;
        Ok(())
    }
}
