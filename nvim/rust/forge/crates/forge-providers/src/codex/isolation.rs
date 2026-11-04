use super::rpc::NativeRpc;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderModel, ProviderOperation,
    ServiceTier,
};
use serde_json::{Value, json};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

/// Reuse the canonical native credential owner without copying its credential files or refresh secrets.
pub(super) struct IsolationAuth {
    source: Arc<NativeRpc>,
}

impl IsolationAuth {
    /// Create the observation-only bridge to the existing native credential owner.
    pub(super) fn new(source: Arc<NativeRpc>) -> Self {
        Self { source }
    }

    /// Authenticate only the private process, using native-exported ephemeral credentials.
    pub(super) async fn bootstrap(&self, rpc: &NativeRpc) -> Result<(), ProviderError> {
        let credentials = self.credentials(false).await?;
        let params = match credentials {
            ExportedCredentials::Anonymous => return Ok(()),
            ExportedCredentials::ApiKey(api_key) => json!({"type":"apiKey","apiKey":api_key}),
            ExportedCredentials::Chatgpt {
                token,
                account_id,
                plan,
            } => {
                json!({"type":"chatgptAuthTokens","accessToken":token,"chatgptAccountId":account_id,"chatgptPlanType":plan})
            }
        };
        rpc.request(
            "account/login/start",
            params,
            ProviderOperation::SessionOpen,
        )
        .await?;
        Ok(())
    }

    /// Delegate access-token renewal to the canonical native owner; never persist refresh material here.
    pub(super) async fn refresh_response(&self) -> Result<Value, ProviderError> {
        match self.credentials(true).await? {
            ExportedCredentials::Chatgpt {
                token,
                account_id,
                plan,
            } => Ok(
                json!({"accessToken":token,"chatgptAccountId":account_id,"chatgptPlanType":plan}),
            ),
            ExportedCredentials::Anonymous | ExportedCredentials::ApiKey(_) => {
                Err(preparation_failure(ProviderFailureKind::Authentication))
            }
        }
    }

    /// Read native credential evidence without exposing tokens through normalized diagnostics.
    async fn credentials(&self, refresh: bool) -> Result<ExportedCredentials, ProviderError> {
        let status = self
            .source
            .request(
                "getAuthStatus",
                json!({"includeToken":true,"refreshToken":refresh}),
                ProviderOperation::SessionOpen,
            )
            .await?;
        if status.get("requiresOpenaiAuth").and_then(Value::as_bool) == Some(false) {
            return Ok(ExportedCredentials::Anonymous);
        }
        let token = status
            .get("authToken")
            .and_then(Value::as_str)
            .filter(|token| !token.is_empty())
            .ok_or_else(|| preparation_failure(ProviderFailureKind::Authentication))?;
        match status.get("authMethod").and_then(Value::as_str) {
            Some("apikey") => {
                self.confirm_status(&status).await?;
                Ok(ExportedCredentials::ApiKey(token.to_owned()))
            }
            Some("chatgpt" | "chatgptAuthTokens") => {
                let account = self
                    .source
                    .request(
                        "account/read",
                        json!({"refreshToken":false}),
                        ProviderOperation::SessionOpen,
                    )
                    .await?;
                let account_id = account
                    .pointer("/workspaceRouting/chatgptAccountId")
                    .and_then(Value::as_str)
                    .filter(|id| !id.is_empty())
                    .ok_or_else(|| preparation_failure(ProviderFailureKind::Authentication))?;
                let plan = account
                    .pointer("/account/planType")
                    .and_then(Value::as_str)
                    .map(str::to_owned);
                self.confirm_status(&status).await?;
                let confirmed_account = self
                    .source
                    .request(
                        "account/read",
                        json!({"refreshToken":false}),
                        ProviderOperation::SessionOpen,
                    )
                    .await?;
                if confirmed_account != account {
                    return Err(preparation_failure(ProviderFailureKind::Authentication));
                }
                Ok(ExportedCredentials::Chatgpt {
                    token: token.to_owned(),
                    account_id: account_id.to_owned(),
                    plan,
                })
            }
            Some(_) | None => Err(preparation_failure(ProviderFailureKind::Authentication)),
        }
    }

    /// Reject credential-owner changes between native token and account observations before private login.
    async fn confirm_status(&self, original: &Value) -> Result<(), ProviderError> {
        let confirmed = self
            .source
            .request(
                "getAuthStatus",
                json!({"includeToken":true,"refreshToken":false}),
                ProviderOperation::SessionOpen,
            )
            .await?;
        if ["authMethod", "authToken", "requiresOpenaiAuth"]
            .iter()
            .any(|key| confirmed.get(*key) != original.get(*key))
        {
            return Err(preparation_failure(ProviderFailureKind::Authentication));
        }
        Ok(())
    }
}

/// Keep native secrets in memory without deriving Debug or serializing them into host state.
enum ExportedCredentials {
    Anonymous,
    ApiKey(String),
    Chatgpt {
        token: String,
        account_id: String,
        plan: Option<String>,
    },
}

/// Launch temporary work with authoritative model metadata which cannot advertise model-owned tools.
/// The catalog is process-scoped: the isolated binding must own its runtime until close.
pub(super) fn runtime_command(
    command: &[String],
    profile: &Path,
    model: &ProviderModel,
    native_config: &Value,
) -> Result<(Vec<String>, PathBuf), ProviderError> {
    let private_home = profile.join("home");
    std::fs::create_dir(&private_home)
        .map_err(|_| preparation_failure(ProviderFailureKind::Runtime))?;
    let mut configuration = String::new();
    for key in [
        "model_provider",
        "model_providers",
        "chatgpt_base_url",
        "forced_login_method",
        "forced_chatgpt_workspace_id",
        "forced_chatgpt_workspace_ids",
    ] {
        if let Some(value) = native_config.get(key).filter(|value| !value.is_null()) {
            configuration.push_str(&format!("{key} = {}\n", toml_value(value)?));
        }
    }
    std::fs::write(private_home.join("config.toml"), configuration)
        .map_err(|_| preparation_failure(ProviderFailureKind::Runtime))?;
    let catalog_path = profile.join("isolated-models.json");
    let catalog = model_catalog(model);
    let encoded = serde_json::to_vec(&catalog)
        .map_err(|_| preparation_failure(ProviderFailureKind::Protocol))?;
    std::fs::write(&catalog_path, encoded)
        .map_err(|_| preparation_failure(ProviderFailureKind::Runtime))?;
    let mut command = command.to_vec();
    let path = serde_json::to_string(&catalog_path.to_string_lossy())
        .map_err(|_| preparation_failure(ProviderFailureKind::Protocol))?;
    command.extend(["--config".into(), format!("model_catalog_json={path}")]);
    // Native 0.164 rejects an empty model_instructions_file during startup, even
    // before thread instructions arrive. The private home excludes inherited files;
    // the authoritative catalog has empty base instructions and thread/start supplies
    // the host's explicit baseInstructions, so no file override is needed.
    command.extend([
        "--config".into(),
        "cli_auth_credentials_store=\"ephemeral\"".into(),
    ]);
    Ok((command, private_home))
}

/// Build the pinned native catalog schema with the discovered opaque model identity and supported effort.
fn model_catalog(model: &ProviderModel) -> Value {
    let reasoning = model
        .reasoning
        .iter()
        .map(|effort| json!({"effort":effort,"description":effort}))
        .collect::<Vec<_>>();
    let tiers = model
        .service_tiers
        .iter()
        .map(|tier| {
            let id = match tier {
                ServiceTier::Standard => "default",
                ServiceTier::Fast => "fast",
                ServiceTier::Ultrafast => "ultrafast",
            };
            json!({"id":id,"name":id,"description":id})
        })
        .collect::<Vec<_>>();
    json!({"models":[{
        "slug":model.id,"display_name":model.id,"description":null,
        "default_reasoning_level":model.default_reasoning,"supported_reasoning_levels":reasoning,
        "shell_type":"disabled","visibility":"list","supported_in_api":true,"priority":0,
        "service_tiers":tiers,"availability_nux":null,"upgrade":null,
        "base_instructions":"","model_messages":null,
        "include_skills_usage_instructions":false,"include_plugin_usage_instructions":false,"include_apps_usage_instructions":false,
        "supports_reasoning_summary_parameter":true,"default_reasoning_summary":"none",
        "support_verbosity":false,"default_verbosity":null,
        "apply_patch_tool_type":null,"experimental_supported_tools":[],
        "truncation_policy":{"mode":"bytes","limit":10000},
        "context_window":null,"auto_compact_token_limit":null,"input_modalities":["text"],
        "supports_search_tool":false,"supports_experimental_context":false,
        "node_repl_disabled":true,"tool_mode":null,"multi_agent_version":null
    }]})
}

/// Classify private isolation preparation before starting any temporary native work.
fn preparation_failure(kind: ProviderFailureKind) -> ProviderError {
    ProviderFailure::new(ProviderOperation::SessionOpen, kind)
        .at_phase(
            forge_provider_api::error::ProviderPhase::Preparation,
            forge_provider_api::error::FailureDisposition::Unchanged,
        )
        .into()
}

/// Encode native provider connection configuration without putting credential values in process arguments.
fn toml_value(value: &Value) -> Result<String, ProviderError> {
    match value {
        Value::Null => Err(preparation_failure(ProviderFailureKind::Protocol)),
        Value::Bool(value) => Ok(value.to_string()),
        Value::Number(value) => Ok(value.to_string()),
        Value::String(value) => serde_json::to_string(value)
            .map_err(|_| preparation_failure(ProviderFailureKind::Protocol)),
        Value::Array(values) => Ok(format!(
            "[{}]",
            values
                .iter()
                .map(toml_value)
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        )),
        Value::Object(values) => {
            let fields = values
                .iter()
                .filter(|(_, value)| !value.is_null())
                .map(|(key, value)| {
                    let key = serde_json::to_string(key)
                        .map_err(|_| preparation_failure(ProviderFailureKind::Protocol))?;
                    Ok(format!("{key} = {}", toml_value(value)?))
                })
                .collect::<Result<Vec<_>, ProviderError>>()?;
            Ok(format!("{{ {} }}", fields.join(", ")))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::{SinkExt, StreamExt};

    /// Private startup excludes inherited instruction files while preserving authoritative tool-free model metadata.
    #[test]
    fn private_launch_uses_catalog_instructions_without_an_empty_file_override() {
        let profile = tempfile::tempdir().unwrap();
        let model = super::super::catalog::parse_model_catalog(&json!({"data":[{"model":"gpt-6-luna","isDefault":true,"supportedReasoningEfforts":[{"reasoningEffort":"medium"}],"defaultReasoningEffort":"medium"}]})).into_iter().next().unwrap();
        let native_config = json!({"model_provider":"openai","model_instructions_file":"untrusted-user-instructions.md","instructions":"untrusted-user-instructions","developer_instructions":"untrusted-user-developer","hooks":{"enabled":true}});
        let (command, home) = runtime_command(
            &["codex".into(), "app-server".into()],
            profile.path(),
            &model,
            &native_config,
        )
        .unwrap();
        assert!(
            !command
                .iter()
                .any(|argument| argument.contains("model_instructions_file"))
        );
        assert!(!profile.path().join("isolated-instructions.md").exists());
        let private_config = std::fs::read_to_string(home.join("config.toml")).unwrap();
        assert!(!private_config.contains("untrusted-user"));
        assert!(!private_config.contains("hooks"));
        let catalog: Value = serde_json::from_slice(
            &std::fs::read(profile.path().join("isolated-models.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(catalog["models"][0]["slug"], "gpt-6-luna");
        assert_eq!(catalog["models"][0]["base_instructions"], "");
        assert_eq!(catalog["models"][0]["shell_type"], "disabled");
        assert!(catalog["models"][0]["apply_patch_tool_type"].is_null());
        assert_eq!(
            catalog["models"][0]["experimental_supported_tools"],
            json!([])
        );
    }

    /// Validate the certified native startup parser with the generated private catalog, without credentials or model work.
    #[tokio::test]
    #[ignore = "requires installed certified Codex CLI 0.164.0; starts no model turn"]
    async fn isolated_native_0164_catalog_initializes_without_credentials_or_turn() {
        tokio::time::timeout(std::time::Duration::from_secs(10), async {
            let profile = tempfile::tempdir().unwrap();
            let model = super::super::catalog::parse_model_catalog(&json!({"data":[{"model":"gpt-6-luna","isDefault":true,"supportedReasoningEfforts":[{"reasoningEffort":"medium"}],"defaultReasoningEffort":"medium"}]})).into_iter().next().unwrap();
            let (command, home) = runtime_command(&["codex".into(), "app-server".into()], profile.path(), &model, &json!({})).unwrap();
            let runtime = super::super::runtime::CodexRuntime::isolated(command, home);
            let connection = runtime.connect().await.unwrap();
            let rpc = NativeRpc::new(connection);
            let initialized = rpc.request("initialize", json!({"clientInfo":{"name":"forge-isolated-startup-fixture","version":"0.1.0"},"capabilities":{"experimentalApi":true}}), ProviderOperation::SessionOpen).await.unwrap();
            rpc.close().await.unwrap(); runtime.shutdown().await.unwrap();
            assert!(initialized["userAgent"].as_str().unwrap().starts_with("forge-isolated-startup-fixture/0.164.0 "));
            assert!(!profile.path().join("home/auth.json").exists());
        }).await.unwrap();
    }

    /// Build the real adapter dispatcher against a private native transport fixture.
    async fn fixture() -> (
        NativeRpc,
        tokio_tungstenite::WebSocketStream<tokio::net::TcpStream>,
    ) {
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
        let endpoint = format!("ws://{}", listener.local_addr().unwrap());
        let accept = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            tokio_tungstenite::accept_async(stream).await.unwrap()
        });
        let connection = super::super::connection::CodexConnection::connect(&endpoint)
            .await
            .unwrap();
        (NativeRpc::new(connection), accept.await.unwrap())
    }

    /// Bootstrap managed credentials from the native owner and use native workspace identity, never token claims.
    #[tokio::test]
    async fn bootstrap_uses_native_export_and_workspace_identity_without_refresh_material() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (source, mut source_wire) = fixture().await;
            let (private, mut private_wire) = fixture().await;
            let auth = IsolationAuth::new(Arc::new(source));
            let source_task = tokio::spawn(async move {
                for _ in 0..2 {
                    let status: Value = serde_json::from_str(source_wire.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                    assert_eq!(status["method"], "getAuthStatus");
                    assert_eq!(status["params"], json!({"includeToken":true,"refreshToken":false}));
                    source_wire.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":status["id"],"result":{"authMethod":"chatgpt","authToken":"opaque-native-access-token","requiresOpenaiAuth":true}}).to_string().into())).await.unwrap();
                    let account: Value = serde_json::from_str(source_wire.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                    assert_eq!(account["method"], "account/read");
                    source_wire.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":account["id"],"result":{"workspaceRouting":{"chatgptAccountId":"native-workspace"},"account":{"type":"chatgpt","planType":"pro"}}}).to_string().into())).await.unwrap();
                }
                while let Some(message) = source_wire.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let private_task = tokio::spawn(async move {
                let login: Value = serde_json::from_str(private_wire.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                assert_eq!(login["method"], "account/login/start");
                assert_eq!(login["params"], json!({"type":"chatgptAuthTokens","accessToken":"opaque-native-access-token","chatgptAccountId":"native-workspace","chatgptPlanType":"pro"}));
                assert!(login["params"].get("refreshToken").is_none());
                private_wire.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":login["id"],"result":{"type":"chatgptAuthTokens"}}).to_string().into())).await.unwrap();
                while let Some(message) = private_wire.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            auth.bootstrap(&private).await.unwrap();
            private.close().await.unwrap(); auth.source.close().await.unwrap();
            source_task.await.unwrap(); private_task.await.unwrap();
        }).await.unwrap();
    }

    /// A native credential switch cannot pair the previous token with a later workspace identity.
    #[tokio::test]
    async fn bootstrap_rejects_changed_native_token_before_private_login() {
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (source, mut source_wire) = fixture().await;
            let (private, mut private_wire) = fixture().await;
            let auth = IsolationAuth::new(Arc::new(source));
            let source_task = tokio::spawn(async move {
                for (method, result) in [
                    ("getAuthStatus", json!({"authMethod":"chatgpt","authToken":"first-token","requiresOpenaiAuth":true})),
                    ("account/read", json!({"workspaceRouting":{"chatgptAccountId":"native-workspace"},"account":{"type":"chatgpt","planType":"pro"}})),
                    ("getAuthStatus", json!({"authMethod":"chatgpt","authToken":"changed-token","requiresOpenaiAuth":true})),
                ] {
                    let request: Value = serde_json::from_str(source_wire.next().await.unwrap().unwrap().to_text().unwrap()).unwrap();
                    assert_eq!(request["method"], method);
                    source_wire.send(tokio_tungstenite::tungstenite::Message::Text(json!({"id":request["id"],"result":result}).to_string().into())).await.unwrap();
                }
                while let Some(message) = source_wire.next().await { if matches!(message, Ok(tokio_tungstenite::tungstenite::Message::Close(_))) { break; } }
            });
            let error = auth.bootstrap(&private).await.unwrap_err();
            assert!(matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::Authentication && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged));
            private.close().await.unwrap();
            assert!(matches!(private_wire.next().await, Some(Ok(tokio_tungstenite::tungstenite::Message::Close(_)))));
            auth.source.close().await.unwrap(); source_task.await.unwrap();
        }).await.unwrap();
    }
}
