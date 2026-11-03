use crate::codex::{CodexConnection, runtime::CodexRuntime};
use forge_provider_api::{
    ProviderError, ProviderFailure,
    error::{ProviderFailureKind, ProviderOperation},
    generation::{ReasoningSelection, TextGenerationRequest},
};
use serde_json::{Value, json};

/// Generate and validate one string in a temporary provider conversation.
pub(super) async fn generate(
    runtime: &CodexRuntime,
    command: &[String],
    request: TextGenerationRequest,
) -> Result<String, ProviderError> {
    let mut connection =
        tokio::time::timeout(std::time::Duration::from_secs(20), runtime.connect())
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::GenerateText,
                    ProviderFailureKind::Transport,
                )
            })?
            .map_err(|_| {
                failure(
                    ProviderOperation::GenerateText,
                    ProviderFailureKind::Transport,
                )
            })?;
    let result = tokio::time::timeout(
        std::time::Duration::from_secs(60),
        run(&mut connection, command, &request),
    )
    .await
    .unwrap_or_else(|_| {
        Err(failure(
            ProviderOperation::GenerateText,
            ProviderFailureKind::Transport,
        ))
    });
    let cleanup = tokio::time::timeout(std::time::Duration::from_secs(8), cleanup(&mut connection))
        .await
        .unwrap_or_else(|_| {
            Err(failure(
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            ))
        });
    match (result, cleanup) {
        (Ok(text), Ok(())) => Ok(text),
        (Err(primary), Ok(())) => Err(primary),
        (Ok(_), Err(cleanup)) => Err(cleanup),
        (Err(primary), Err(cleanup)) => Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![cleanup],
        }),
    }
}

async fn run(
    connection: &mut CodexConnection,
    command: &[String],
    request: &TextGenerationRequest,
) -> Result<String, ProviderError> {
    let mut request_id = 0_u64;
    let initialized = rpc(
        connection,
        &mut request_id,
        "initialize",
        json!({
            "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
            "capabilities":{"experimentalApi":true}
        }),
        ProviderOperation::InitializeSession,
    )
    .await?;
    let requirement = forge_provider_api::version::RuntimeRequirement::new(
        forge_providers_id(),
        command
            .first()
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|| "app-server".into()),
        semver::Version::new(0, 164, 0),
    );
    let reported = initialized
        .get("userAgent")
        .and_then(Value::as_str)
        .and_then(|agent| agent.split_whitespace().next())
        .and_then(|product| product.split_once('/').map(|(_, version)| version))
        .ok_or_else(|| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Initialization,
            )
        })?;
    requirement
        .validate(Some(reported))
        .map_err(ProviderError::from)?;
    notification(connection, "initialized", Value::Null).await?;

    let effective = rpc(
        connection,
        &mut request_id,
        "config/read",
        json!({"includeLayers":false,"cwd":request.workspace}),
        ProviderOperation::GenerateText,
    )
    .await?;
    let mut config = tool_free_config();
    let server_names = effective
        .pointer("/config/mcp_servers")
        .and_then(Value::as_object)
        .map(|servers| {
            servers
                .keys()
                .map(|name| (name.clone(), json!({"enabled":false})))
                .collect()
        })
        .unwrap_or_default();
    config["mcp_servers"] = Value::Object(server_names);
    match &request.reasoning {
        ReasoningSelection::Default => {}
        ReasoningSelection::Level(level) => config["model_reasoning_effort"] = json!(level),
        ReasoningSelection::Lowest => {
            let catalog = rpc(
                connection,
                &mut request_id,
                "model/list",
                json!({}),
                ProviderOperation::GenerateText,
            )
            .await?;
            let model_list = crate::codex::catalog::parse_model_catalog(&catalog);
            let selected = model_list.iter().find(|model| model.id == request.model);
            if let Some(effort) = selected.and_then(|model| model.reasoning.first()) {
                config["model_reasoning_effort"] = json!(effort);
            }
        }
    }

    let mut thread_parameters = json!({
        "cwd":request.workspace, "ephemeral":true, "historyMode":"paginated",
        "approvalPolicy":"never", "sandbox":"read-only",
        "developerInstructions":request.instructions,
        "config":config,
    });
    if request.model != "default" {
        thread_parameters["model"] = json!(request.model);
    }
    let thread = rpc(
        connection,
        &mut request_id,
        "thread/start",
        thread_parameters,
        ProviderOperation::GenerateText,
    )
    .await?
    .pointer("/thread/id")
    .and_then(Value::as_str)
    .map(str::to_owned)
    .ok_or_else(|| {
        failure(
            ProviderOperation::GenerateText,
            ProviderFailureKind::Protocol,
        )
    })?;

    let generated = async {
        let turn_request_id = send_request(
            connection,
            &mut request_id,
            "turn/start",
            json!({"threadId":thread,"input":[{"type":"text","text":request.input,"text_elements":[]}]}),
        )
        .await?;
        let mut response_text = String::new();
        loop {
            let value = receive(connection).await?;
            if response_id(&value) == Some(turn_request_id) {
                result(&value, ProviderOperation::GenerateText)?;
                continue;
            }
            let params = value.get("params").unwrap_or(&Value::Null);
            if params.get("threadId").and_then(Value::as_str) != Some(thread.as_str()) {
                continue;
            }
            match value.get("method").and_then(Value::as_str) {
                Some("item/completed") if params.pointer("/item/type").and_then(Value::as_str) == Some("agentMessage") => {
                    response_text = params.pointer("/item/text").and_then(Value::as_str).unwrap_or_default().to_owned();
                }
                Some("turn/completed") => {
                    if params.pointer("/turn/status").and_then(Value::as_str) != Some("completed") {
                        return Err(failure(ProviderOperation::GenerateText, ProviderFailureKind::Unknown));
                    }
                    return request.validator.validate(&response_text);
                }
                _ => {}
            }
        }
    }
    .await;
    let unsubscribe = rpc(
        connection,
        &mut request_id,
        "thread/unsubscribe",
        json!({"threadId":thread}),
        ProviderOperation::CollectEventReader,
    )
    .await
    .map(|_| ());
    match (generated, unsubscribe) {
        (Ok(text), Ok(())) => Ok(text),
        (Err(primary), Ok(())) => Err(primary),
        (Ok(_), Err(cleanup)) => Err(cleanup),
        (Err(primary), Err(cleanup)) => Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![cleanup],
        }),
    }
}

async fn rpc(
    connection: &mut CodexConnection,
    request_id: &mut u64,
    method: &str,
    params: Value,
    operation: ProviderOperation,
) -> Result<Value, ProviderError> {
    let id = send_request(connection, request_id, method, params).await?;
    loop {
        let value = receive(connection).await?;
        if response_id(&value) == Some(id) {
            return result(&value, operation);
        }
    }
}

async fn send_request(
    connection: &mut CodexConnection,
    request_id: &mut u64,
    method: &str,
    params: Value,
) -> Result<u64, ProviderError> {
    *request_id = request_id.checked_add(1).ok_or_else(|| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Unknown,
        )
    })?;
    let id = *request_id;
    let message = json!({"id":id,"method":method,"params":params});
    let encoded = serde_json::to_string(&message).map_err(|_| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Protocol,
        )
    })?;
    connection.send(encoded).await.map_err(|_| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Transport,
        )
    })?;
    Ok(id)
}

async fn notification(
    connection: &mut CodexConnection,
    method: &str,
    params: Value,
) -> Result<(), ProviderError> {
    let message = json!({"method":method,"params":params});
    let encoded = serde_json::to_string(&message).map_err(|_| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Protocol,
        )
    })?;
    connection.send(encoded).await.map_err(|_| {
        failure(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Transport,
        )
    })
}

async fn receive(connection: &mut CodexConnection) -> Result<Value, ProviderError> {
    connection
        .receive()
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Transport,
            )
        })?
        .value
        .map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })
}

fn response_id(value: &Value) -> Option<u64> {
    if value.get("method").is_some() {
        return None;
    }
    value.get("id").and_then(Value::as_u64)
}

fn result(value: &Value, operation: ProviderOperation) -> Result<Value, ProviderError> {
    if value.get("result").is_some() == value.get("error").is_some() {
        return Err(failure(operation, ProviderFailureKind::Protocol));
    }
    if value.get("error").is_some() {
        return Err(failure(operation, ProviderFailureKind::Unknown));
    }
    Ok(value["result"].clone())
}

async fn cleanup(connection: &mut CodexConnection) -> Result<(), ProviderError> {
    connection.close().await.map_err(|_| {
        failure(
            ProviderOperation::CollectNativeRuntime,
            ProviderFailureKind::Transport,
        )
    })
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

fn forge_providers_id() -> forge_provider_api::ProviderId {
    crate::registry::select("codex").expect("Codex is registered")
}

fn tool_free_config() -> Value {
    let mut config = json!({
        "web_search":"disabled",
        "skills.include_instructions":false,
        "orchestrator.skills.enabled":false,
        "tools.update_plan.enabled":false,
        "tools.experimental_request_user_input.enabled":false
    });
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
    config
}
