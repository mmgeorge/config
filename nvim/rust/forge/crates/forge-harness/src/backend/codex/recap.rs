use super::json_rpc::CodexJsonRpc;
use crate::backend::BackendOutput;
use anyhow::{Context, Result, ensure};
use serde_json::{Value, json};
use std::time::Duration;

/// Use the Codex CLI recap strategy: a tool-free ephemeral thread with captured visible history.
pub(super) async fn generate(
    process: &mut CodexJsonRpc,
    output: &mut BackendOutput,
    workspace: &str,
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
        let effective = process.request("config/read", json!({"includeLayers":false,"cwd":workspace}), output).await?;
        let effective = effective.get("config").and_then(Value::as_object).context("Missing effective Codex config")?;
        let mut servers = serde_json::Map::new();
        if let Some(names) = effective.get("mcp_servers").and_then(Value::as_object) {
            for name in names.keys() { servers.insert(name.clone(), json!({"enabled":false})); }
        }
        config["mcp_servers"] = Value::Object(servers);
        process.request("thread/start", json!({"model": (model != "default").then_some(model),
            "cwd":workspace,"approvalPolicy":"never","sandbox":"read-only","ephemeral":true,
            "dynamicTools":[],"config":config,"baseInstructions":crate::backend::recap::INSTRUCTIONS}), output).await
    }).await.context("Recap thread creation timed out")??;
    let thread = started
        .pointer("/thread/id")
        .and_then(Value::as_str)
        .context("Missing recap thread identity")?;
    let result = tokio::time::timeout(Duration::from_secs(60), async {
        let request_id = process.send_request("turn/start", json!({"threadId":thread,
            "input":[{"type":"text","text":format!("Conversation to recap:\n{history}"),"text_elements":[]}]})).await?;
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
                    ensure!(params["turn"]["status"] == "completed", "Recap turn failed: {}", params["turn"]["error"]);
                    return crate::backend::recap::validate(&text);
                }
                _ => {}
            }
        }
    }).await.context("Recap generation timed out").and_then(|result| result);
    tokio::time::timeout(
        Duration::from_secs(8),
        process.request("thread/unsubscribe", json!({"threadId":thread}), output),
    )
    .await
    .context("Recap thread cleanup timed out")??;
    result
}
