use super::runtime::CodexRuntime;
use super::{BackendCatalogRequest, BackendOutput, CodexBackend, CodexJsonRpc, McpDefinition};
use crate::settings::SessionResources;
use anyhow::{Context, Result, ensure};
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::Path;
use tokio::sync::Mutex;

/// Owns a session's isolated Codex configuration without changing native preference files.
pub(super) struct CodexResources {
    /// Collects only this session's provider process when resource selection changes.
    pub(super) runtime: CodexRuntime,
    /// Retains canonical metadata independently of provider-local server names.
    pub(super) catalog: SessionResources,
    binding: HashMap<String, String>,
    config: Mutex<Option<Value>>,
    thread: Mutex<Option<String>>,
}

impl CodexResources {
    /// Resolve shared selection before creating a private provider runtime.
    pub(super) fn new(
        root: &Path,
        request: &BackendCatalogRequest,
        command: Vec<String>,
    ) -> Result<Self> {
        let catalog = SessionResources::load(root, request, "codex", "codexcli")?;
        let binding = catalog
            .mcp
            .iter()
            .map(|server| {
                let identity = server.resource_id.as_deref().unwrap_or(&server.name);
                let label: String = server
                    .name
                    .chars()
                    .map(|character| {
                        if character.is_ascii_alphanumeric() || character == '_' {
                            character
                        } else {
                            '_'
                        }
                    })
                    .take(40)
                    .collect();
                (
                    server.name.clone(),
                    format!(
                        "forge_{}_{}",
                        &hex::encode(Sha256::digest(identity.as_bytes()))[..12],
                        label
                    ),
                )
            })
            .collect();
        Ok(Self {
            runtime: CodexRuntime::new(command),
            catalog,
            binding,
            config: Mutex::new(None),
            thread: Mutex::new(None),
        })
    }

    /// Build one configuration after discovering inherited names on this private runtime.
    pub(super) async fn configure(
        &self,
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        workspace: &str,
    ) -> Result<Value> {
        process.set_mcp_bindings(
            self.binding
                .iter()
                .map(|(name, alias)| (alias.clone(), name.clone()))
                .collect(),
        );
        let mut configured = self.config.lock().await;
        if let Some(config) = configured.as_ref() {
            return Ok(config.clone());
        }
        let native = process
            .request(
                "config/read",
                json!({"cwd":workspace,"includeLayers":false}),
                output,
            )
            .await?;
        let mut server = Map::new();
        for name in native
            .pointer("/config/mcp_servers")
            .and_then(Value::as_object)
            .into_iter()
            .flat_map(|entry| entry.keys())
        {
            server.insert(name.clone(), json!({"enabled":false}));
        }
        for (name, launch) in &self.catalog.launch {
            let alias = &self.binding[name];
            ensure!(
                !server.contains_key(alias),
                "Native MCP name collides with a Forge resource binding"
            );
            server.insert(alias.clone(), native_server(launch)?);
        }
        let plugin: Map<_, _> = native
            .pointer("/config/plugins")
            .and_then(Value::as_object)
            .into_iter()
            .flat_map(|entry| entry.keys())
            .map(|name| (name.clone(), json!({"enabled":false})))
            .collect();
        let root: Vec<_> = self
            .catalog
            .selected_skill
            .iter()
            .map(|skill| {
                Path::new(&skill.source)
                    .parent()
                    .context("skill source has no directory")
                    .map(|path| path.to_string_lossy().into_owned())
            })
            .collect::<Result<_>>()?;
        process
            .request("skills/extraRoots/set", json!({"extraRoots":root}), output)
            .await?;
        let discovered = CodexBackend::skill_catalog(process, output, workspace, true).await?;
        let mut skill: Vec<_> = discovered
            .iter()
            .filter_map(|skill| skill.path.as_ref())
            .map(|path| json!({"path":path,"enabled":false}))
            .collect();
        skill.extend(
            self.catalog
                .selected_skill
                .iter()
                .map(|skill| json!({"path":skill.source,"enabled":true})),
        );
        let config = json!({"mcp_servers":server,"plugins":plugin,"hooks":native.pointer("/config/hooks").filter(|value|value.is_object()).cloned().unwrap_or(json!({})),
            "skills":{"bundled":{"enabled":false},"config":skill}});
        *configured = Some(config.clone());
        Ok(config)
    }

    /// Retain the loaded conversation used by out-of-band MCP discovery.
    pub(super) async fn retain_thread(&self, thread: &str) {
        *self.thread.lock().await = Some(thread.into());
    }

    /// Resolve the last loaded conversation without creating provider history for a picker.
    pub(super) async fn thread(&self) -> Option<String> {
        self.thread.lock().await.clone()
    }

    /// Overlay live status onto canonical rows while hiding inherited native definitions.
    pub(super) fn mcp(&self, native: Vec<McpDefinition>) -> Vec<McpDefinition> {
        let mut native: HashMap<_, _> = native
            .into_iter()
            .map(|entry| (entry.name.clone(), entry))
            .collect();
        self.catalog
            .mcp
            .iter()
            .cloned()
            .map(|mut entry| {
                if entry.enabled
                    && let Some(mut observed) = native.remove(&self.binding[&entry.name])
                {
                    observed.name = entry.name;
                    observed.resource_id = entry.resource_id;
                    observed.scope = entry.scope;
                    observed.source_path = entry.source_path;
                    entry = observed;
                }
                entry
            })
            .collect()
    }
}

fn native_server(launch: &Value) -> Result<Value> {
    let mut server = Map::new();
    server.insert("enabled".into(), json!(true));
    match launch["type"]
        .as_str()
        .context("MCP transport is missing")?
    {
        "stdio" => {
            for field in ["command", "args", "env", "cwd"] {
                if let Some(value) = launch.get(field) {
                    server.insert(field.into(), value.clone());
                }
            }
        }
        "http" | "sse" => {
            server.insert("url".into(), launch["url"].clone());
            if let Some(value) = launch.get("headers") {
                server.insert("http_headers".into(), value.clone());
            }
        }
        transport => anyhow::bail!("Codex does not support MCP transport {transport}"),
    }
    if let Some(tools) = launch.get("tools").and_then(Value::as_array)
        && !tools.iter().any(|tool| tool.as_str() == Some("*"))
    {
        server.insert("enabled_tools".into(), json!(tools));
    }
    if let Some(timeout) = launch.get("timeout").and_then(Value::as_f64) {
        ensure!(
            timeout.is_finite() && timeout > 0.0,
            "MCP timeout must be positive"
        );
        server.insert("tool_timeout_sec".into(), json!(timeout / 1000.0));
    }
    Ok(Value::Object(server))
}

#[cfg(test)]
mod test {
    use super::*;

    #[tokio::test]
    async fn canonical_configuration_disables_inherited_resources_and_redacts_launch_credentials()
    -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        use std::sync::Arc;
        let root = tempfile::tempdir()?;
        let definitions = root.path().join("rulesync-global/.rulesync");
        let skill = definitions.join("skills/selected");
        std::fs::create_dir_all(&skill)?;
        std::fs::write(
            skill.join("SKILL.md"),
            "---\nname: selected\ndescription: fixture\n---\nUse this skill.",
        )?;
        std::fs::write(definitions.join("mcp.json"),json!({"mcpServers":{
            "selected":{"command":std::env::current_exe().unwrap(),"env":{"CUSTOM_CREDENTIAL":"selected-secret-fixture"}}
        }}).to_string())?;
        crate::settings::SettingsStore::open(root.path())?.configure_catalog(root.path())?;
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: root.path().to_string_lossy().into_owned(),
            access: Default::default(),
            execution_mode: crate::session::PermissionMode::Read,
            backend_session_id: None,
        };
        let resources = CodexResources::new(root.path(), &request, vec!["unused".into()])?;
        let selected_path = dunce::canonicalize(skill.join("SKILL.md"))?
            .to_string_lossy()
            .into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let source = selected_path.clone();
        let server = tokio::spawn(async move {
            let (socket, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(socket).await.unwrap();
            for method in [
                "config/read",
                "skills/extraRoots/set",
                "skills/list",
                "thread/start",
            ] {
                let frame = socket.next().await.unwrap().unwrap();
                let message: Value = serde_json::from_str(frame.to_text().unwrap()).unwrap();
                assert_eq!(message["method"], method);
                let result = match method {
                    "config/read" => {
                        json!({"config":{"mcp_servers":{"inherited":{"command":"native","env":{"OTHER":"native-secret-fixture"}}},
                        "plugins":{"inherited-plugin":{"enabled":true}}}})
                    }
                    "skills/list" => {
                        json!({"data":[{"skills":[{"name":"inherited","path":"C:/inherited/SKILL.md"},
                        {"name":"selected","path":source}]}]})
                    }
                    "thread/start" => {
                        assert_eq!(
                            message.pointer("/params/config/mcp_servers/inherited/enabled"),
                            Some(&json!(false))
                        );
                        assert!(!message.to_string().contains("native-secret-fixture"));
                        assert!(message.to_string().contains("selected-secret-fixture"));
                        json!({"thread":{"id":"configured-thread"}})
                    }
                    _ => json!({}),
                };
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":message["id"],"result":result})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
            }
        });
        let trace = Arc::new(crate::trace::TraceStore::open(root.path())?);
        trace.configure(true)?;
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &request.workspace,
            request.execution_mode,
            crate::backend::approval::PermissionCoordinator::transient(&request.workspace)?,
            None,
            trace.clone(),
            "session".into(),
        )
        .await?;
        let mut output = BackendOutput::default();
        let config = resources
            .configure(&mut process, &mut output, &request.workspace)
            .await?;
        assert_eq!(config["plugins"]["inherited-plugin"]["enabled"], false);
        let rules = config["skills"]["config"].as_array().unwrap();
        assert!(
            rules
                .iter()
                .any(|rule| rule["path"] == "C:/inherited/SKILL.md" && rule["enabled"] == false)
        );
        assert_eq!(
            rules.last().unwrap(),
            &json!({"path":selected_path,"enabled":true})
        );
        process
            .request("thread/start", json!({"config":config}), &mut output)
            .await?;
        server.await?;
        let trace = std::fs::read_to_string(trace.session_status("session").path)?;
        assert!(
            !trace.contains("selected-secret-fixture") && !trace.contains("native-secret-fixture")
        );
        Ok(())
    }

    #[test]
    fn native_configuration_preserves_transport_credentials_and_tool_restrictions() {
        let stdio = native_server(&json!({"type":"stdio","command":"fixture","args":["one"],
            "env":{"FIXTURE_SECRET":"value"},"cwd":"workspace","tools":["allowed"],"timeout":2500}))
        .unwrap();
        assert_eq!(stdio["env"]["FIXTURE_SECRET"], "value");
        assert_eq!(stdio["enabled_tools"], json!(["allowed"]));
        assert_eq!(stdio["tool_timeout_sec"], 2.5);
        assert!(stdio.get("type").is_none());
        let http = native_server(&json!({"type":"http","url":"https://example.invalid/mcp",
            "headers":{"Authorization":"Bearer fixture"},"tools":["*"]}))
        .unwrap();
        assert_eq!(http["http_headers"]["Authorization"], "Bearer fixture");
        assert!(http.get("enabled_tools").is_none());
    }
}
