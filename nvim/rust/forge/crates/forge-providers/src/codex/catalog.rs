use forge_provider_api::ProviderModel;
use forge_provider_api::catalog::{
    McpDefinition, McpStatus, McpToolDefinition, McpTransportKind, SkillDefinition,
};
use serde_json::{Value, json};
use std::collections::HashSet;

fn sanitized_diagnostic(message: &str) -> forge_provider_api::diagnostic::SanitizedMessage {
    forge_provider_api::diagnostic::SanitizedMessage::new(message)
        .expect("static diagnostic text is nonempty")
}

/// Decode native skill discovery into provider-neutral catalog entries.
pub fn parse_skill_catalog(result: &Value) -> Vec<SkillDefinition> {
    result
        .get("data")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .flat_map(|workspace| {
            workspace
                .get("skills")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
        })
        .filter_map(|skill| {
            Some(SkillDefinition {
                scope: None,
                resource_id: None,
                name: skill.get("name")?.as_str()?.to_owned(),
                description: skill
                    .get("description")
                    .and_then(Value::as_str)
                    .unwrap_or_default()
                    .to_owned(),
                enabled: skill
                    .get("enabled")
                    .and_then(Value::as_bool)
                    .unwrap_or(true),
                user_invocable: skill
                    .get("userInvocable")
                    .and_then(Value::as_bool)
                    .unwrap_or(true),
                path: skill.get("path").and_then(Value::as_str).map(str::to_owned),
                source: skill
                    .get("scope")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                argument_hint: skill
                    .get("argumentHint")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
            })
        })
        .collect()
}

/// Merge native runtime status with configured MCP servers without losing disabled entries.
pub fn parse_mcp_catalog(result: &Value, config_result: &Value) -> Vec<McpDefinition> {
    let mut server_list = result
        .get("data")
        .or_else(|| result.get("servers"))
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default()
        .into_iter()
        .map(|server| (server, true))
        .collect::<Vec<_>>();
    let config_map = config_result
        .pointer("/config/mcp_servers")
        .or_else(|| config_result.pointer("/config/mcpServers"))
        .and_then(Value::as_object);
    let listed_name_set = server_list
        .iter()
        .filter_map(|(server, _)| server.get("name").and_then(Value::as_str))
        .map(str::to_owned)
        .collect::<HashSet<_>>();
    if let Some(config_map) = config_map {
        for name in config_map.keys() {
            if !listed_name_set.contains(name.as_str()) {
                server_list.push((json!({ "name": name }), false));
            }
        }
    }

    let mut definition_list = server_list
        .iter()
        .filter_map(|(server, reported_status)| {
            let name = server
                .get("name")
                .or_else(|| server.get("server"))
                .and_then(Value::as_str)?
                .to_owned();
            let config = config_map.and_then(|config_map| config_map.get(&name));
            let status_text = server
                .get("runtimeStatus")
                .filter(|status| !status.is_null())
                .or_else(|| server.get("status"))
                .and_then(|status| status.as_str().or_else(|| status.get("status")?.as_str()))
                .unwrap_or_default()
                .to_ascii_lowercase();
            let auth_text = server
                .get("authStatus")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_ascii_lowercase();
            let enabled = config
                .and_then(|config| config.get("enabled"))
                .or_else(|| server.get("enabled"))
                .and_then(Value::as_bool)
                .unwrap_or(!status_text.contains("disabled"));
            let tool_error = server
                .get("toolsError")
                .or_else(|| server.get("toolError"))
                .is_some_and(|error| !error.is_null())
                .then(|| sanitized_diagnostic("Provider reported an MCP tool discovery error"));
            let error = server
                .get("error")
                .is_some_and(|error| !error.is_null())
                .then(|| sanitized_diagnostic("Provider reported an MCP server error"))
                .or_else(|| tool_error.clone());
            let tool_value = server.get("tools");
            let status = if !enabled || status_text.contains("disabled") {
                McpStatus::Disabled
            } else if auth_text == "notloggedin"
                || auth_text.contains("needs")
                || status_text.contains("auth")
            {
                McpStatus::NeedsAuthentication
            } else if error.is_some()
                || status_text.contains("fail")
                || status_text.contains("error")
            {
                McpStatus::Failed
            } else if status_text == "notstarted" || status_text == "cancelled" {
                McpStatus::Unavailable
            } else if status_text.contains("start")
                || status_text.contains("load")
                || status_text.contains("pending")
            {
                McpStatus::Loading
            } else if status_text.contains("connect")
                || status_text.contains("ready")
                || (*reported_status && tool_value.is_some())
            {
                McpStatus::Connected
            } else {
                McpStatus::Unavailable
            };
            let mut tools = Vec::new();
            if let Some(tool_map) = tool_value.and_then(Value::as_object) {
                for (name, tool) in tool_map {
                    tools.push(McpToolDefinition {
                        name: tool
                            .get("name")
                            .and_then(Value::as_str)
                            .unwrap_or(name)
                            .to_owned(),
                        description: tool
                            .get("description")
                            .and_then(Value::as_str)
                            .map(str::to_owned),
                    });
                }
            } else if let Some(tool_list) = tool_value.and_then(Value::as_array) {
                tools.extend(tool_list.iter().filter_map(|tool| {
                    if let Some(name) = tool.as_str() {
                        return Some(McpToolDefinition {
                            name: name.to_owned(),
                            description: None,
                        });
                    }
                    Some(McpToolDefinition {
                        name: tool.get("name")?.as_str()?.to_owned(),
                        description: tool
                            .get("description")
                            .and_then(Value::as_str)
                            .map(str::to_owned),
                    })
                }));
            }
            let token_count = tool_value.map(|tool_value| {
                serde_json::to_vec(tool_value)
                    .map(|serialized| serialized.len().div_ceil(4) as u64)
                    .unwrap_or_default()
            });
            let transport = config
                .and_then(|config| {
                    if config.get("command").is_some() {
                        return Some("stdio");
                    }
                    if config.get("url").is_some() {
                        return Some("http");
                    }
                    config.get("transport").and_then(|transport| {
                        transport
                            .as_str()
                            .or_else(|| transport.get("type").and_then(Value::as_str))
                    })
                })
                .or_else(|| {
                    server.get("transport").and_then(|transport| {
                        transport
                            .as_str()
                            .or_else(|| transport.get("type").and_then(Value::as_str))
                    })
                })
                .or_else(|| server.get("type").and_then(Value::as_str))
                .map(|transport| match transport.to_ascii_lowercase().as_str() {
                    "stdio" => McpTransportKind::Stdio,
                    "http" => McpTransportKind::Http,
                    "sse" => McpTransportKind::Sse,
                    _ => McpTransportKind::Unknown,
                })
                .unwrap_or(McpTransportKind::Unknown);
            Some(McpDefinition {
                scope: None,
                source_path: None,
                resource_id: None,
                name,
                transport,
                enabled,
                status,
                status_detail: error,
                token_count,
                token_estimated: token_count.is_some(),
                tools,
                tool_error,
            })
        })
        .collect::<Vec<_>>();
    definition_list.sort_by(|left, right| left.name.cmp(&right.name));
    definition_list
}

/// Decode supported models, reasoning levels, and service tiers.
pub fn parse_model_catalog(result: &Value) -> Vec<ProviderModel> {
    result
        .get("data")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|model| {
            let id = model
                .get("model")
                .or_else(|| model.get("id"))
                .and_then(Value::as_str)?;
            let effort = model
                .get("supportedReasoningEfforts")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
                .filter_map(|entry| {
                    entry
                        .get("reasoningEffort")
                        .and_then(Value::as_str)
                        .map(str::to_owned)
                })
                .collect();
            Some(ProviderModel {
                service_tiers: model
                    .get("serviceTiers")
                    .and_then(Value::as_array)
                    .into_iter()
                    .flatten()
                    .filter_map(|tier| match tier.get("id").and_then(Value::as_str) {
                        Some("priority" | "fast") => Some(forge_provider_api::ServiceTier::Fast),
                        Some("ultrafast") => Some(forge_provider_api::ServiceTier::Ultrafast),
                        _ => None,
                    })
                    .collect(),
                id: id.to_owned(),
                label: model
                    .get("displayName")
                    .and_then(Value::as_str)
                    .filter(|name| !name.trim().is_empty())
                    .map(str::to_owned),
                default_reasoning: model
                    .get("defaultReasoningEffort")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                reasoning: ordered_efforts(effort),
                selected_reasoning: None,
                context_window: Vec::new(),
                default_context_window: None,
                selected_context_window: None,
                vision: model
                    .get("supportsVision")
                    .and_then(Value::as_bool)
                    .unwrap_or(false),
                description: model
                    .get("description")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                is_default: model
                    .get("isDefault")
                    .and_then(Value::as_bool)
                    .unwrap_or(false),
            })
        })
        .collect()
}

/// Order this provider's native effort vocabulary before publishing its model catalog.
fn ordered_efforts(mut choices: Vec<String>) -> Vec<String> {
    let order = [
        "none", "minimal", "low", "medium", "high", "xhigh", "max", "ultra",
    ];
    choices.sort_by_key(|value| {
        order
            .iter()
            .position(|item| *item == value)
            .unwrap_or(order.len())
    });
    choices
}
