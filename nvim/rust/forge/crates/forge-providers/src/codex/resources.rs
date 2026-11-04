use super::rpc::NativeRpc;
use forge_provider_api::{
    ProviderError, ProviderFailure,
    catalog::{McpDefinition, McpStatus, McpTransportKind, ResourceScope},
    error::{ProviderFailureKind, ProviderOperation},
    session::{McpLaunch, McpTransport, ResourceSelection},
};
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};
use std::collections::HashMap;

/// Own native MCP aliases and Codex resource configuration for one binding.
pub(super) struct CodexResources {
    selection: ResourceSelection,
    native_name: HashMap<String, String>,
    profile_root: std::path::PathBuf,
}

impl CodexResources {
    /// Build native MCP aliases from already admitted normalized resources.
    pub(super) fn new(selection: ResourceSelection, profile_root: std::path::PathBuf) -> Self {
        let native_name = selection
            .mcp
            .iter()
            .map(|resource| {
                let digest = Sha256::digest(resource.name.as_bytes());
                let label = resource
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
                    .collect::<String>();
                (
                    resource.name.clone(),
                    format!("forge_{}_{}", &hex::encode(digest)[..12], label),
                )
            })
            .collect();
        Self {
            selection,
            native_name,
            profile_root,
        }
    }

    /// Map private launch aliases back to canonical resource names before publishing activity.
    pub(super) fn mcp_aliases(&self) -> HashMap<String, String> {
        self.native_name
            .iter()
            .map(|(canonical, native)| (native.clone(), canonical.clone()))
            .collect()
    }

    /// Create native config that disables inherited providers and enables only admitted resources.
    pub(super) async fn configure(
        &self,
        rpc: &NativeRpc,
        workspace: &str,
        retired_aliases: &[String],
    ) -> Result<Value, ProviderError> {
        let native = rpc
            .request(
                "config/read",
                json!({"cwd":workspace,"includeLayers":false}),
                ProviderOperation::SessionConfigure,
            )
            .await?;
        let servers = self.server_configuration(
            native
                .pointer("/config/mcp_servers")
                .and_then(Value::as_object),
            retired_aliases,
        )?;
        let mut roots = Vec::new();
        for skill in self.selection.skills.iter().filter(|skill| skill.enabled) {
            if skill.path.is_none() {
                return Err(failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Rejected,
                ));
            }
            let root = self
                .profile_root
                .join("skills")
                .to_string_lossy()
                .into_owned();
            if !roots.contains(&root) {
                roots.push(root);
            }
        }
        rpc.request(
            "skills/extraRoots/set",
            json!({"extraRoots":roots}),
            ProviderOperation::SessionConfigure,
        )
        .await?;
        let discovered = rpc
            .request(
                "skills/list",
                json!({"cwd":workspace,"forceReload":true}),
                ProviderOperation::SessionConfigure,
            )
            .await?;
        let mut skills = crate::codex::catalog::parse_skill_catalog(&discovered)
            .into_iter()
            .filter_map(|skill| skill.path.map(|path| json!({"path":path,"enabled":false})))
            .collect::<Vec<_>>();
        skills.extend(
            self.selection
                .skills
                .iter()
                .filter(|skill| skill.enabled)
                .filter_map(|skill| {
                    let file_name = std::path::Path::new(skill.path.as_deref()?).file_name()?;
                    let path = self
                        .profile_root
                        .join("skills")
                        .join(&skill.name)
                        .join(file_name);
                    Some(json!({"path":path,"enabled":true}))
                }),
        );
        let plugins = native
            .pointer("/config/plugins")
            .and_then(Value::as_object)
            .map(|plugins| {
                plugins
                    .keys()
                    .map(|name| (name.clone(), json!({"enabled":false})))
                    .collect::<Map<_, _>>()
            })
            .unwrap_or_default();
        Ok(json!({
            "mcp_servers":servers,
            "plugins":plugins,
            "hooks":native.pointer("/config/hooks").filter(|hooks| hooks.is_object()).cloned().unwrap_or_else(|| json!({})),
            "skills":{"bundled":{"enabled":false},"config":skills}
        }))
    }

    /// Exclude inherited and retired launches even when no selected server remains.
    fn server_configuration(
        &self,
        inherited: Option<&Map<String, Value>>,
        retired_aliases: &[String],
    ) -> Result<Map<String, Value>, ProviderError> {
        let mut servers = Map::new();
        if let Some(inherited) = inherited {
            for name in inherited.keys() {
                servers.insert(name.clone(), json!({"enabled":false}));
            }
        }
        for resource in &self.selection.mcp {
            let native_name = self.native_name.get(&resource.name).ok_or_else(|| {
                failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Unknown,
                )
            })?;
            if servers.contains_key(native_name) {
                return Err(failure(
                    ProviderOperation::SessionConfigure,
                    ProviderFailureKind::Rejected,
                ));
            }
            servers.insert(native_name.clone(), native_server(resource)?);
        }
        // Thread resume merges its prior overrides; omission would preserve an old launch.
        for name in retired_aliases {
            servers
                .entry(name.clone())
                .or_insert_with(|| json!({"enabled":false}));
        }
        Ok(servers)
    }

    /// Resolve an enabled, user-invocable skill through native discovery before encoding its item.
    pub(super) async fn skill_path(
        &self,
        rpc: &NativeRpc,
        workspace: &str,
        name: &str,
    ) -> Result<String, ProviderError> {
        let selected = self
            .selection
            .skills
            .iter()
            .find(|skill| skill.name == name && skill.enabled && skill.user_invocable)
            .and_then(|skill| {
                let file_name = std::path::Path::new(skill.path.as_deref()?).file_name()?;
                Some(
                    self.profile_root
                        .join("skills")
                        .join(&skill.name)
                        .join(file_name)
                        .to_string_lossy()
                        .into_owned(),
                )
            });
        if let Some(path) = selected {
            return Ok(path);
        }
        let catalog = rpc
            .request(
                "skills/list",
                json!({"cwd":workspace,"forceReload":true}),
                ProviderOperation::SessionConfigure,
            )
            .await?;
        crate::codex::catalog::parse_skill_catalog(&catalog)
            .into_iter()
            .find(|skill| skill.name == name && skill.enabled && skill.user_invocable)
            .and_then(|skill| skill.path)
            .ok_or_else(|| {
                failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                )
            })
    }

    /// Restore canonical names and hide inherited rows from normalized MCP discovery.
    pub(super) fn normalize_mcp(&self, native: Vec<McpDefinition>) -> Vec<McpDefinition> {
        let mut native = native
            .into_iter()
            .map(|entry| (entry.name.clone(), entry))
            .collect::<HashMap<_, _>>();
        self.selection
            .mcp
            .iter()
            .map(|selection| {
                let mut entry = native
                    .remove(&self.native_name[&selection.name])
                    .unwrap_or_else(|| disabled(selection));
                entry.name = selection.name.clone();
                entry.resource_id = None;
                entry.scope = None;
                entry
            })
            .collect()
    }
}

fn native_server(resource: &McpLaunch) -> Result<Value, ProviderError> {
    let mut server = Map::new();
    server.insert("enabled".into(), json!(true));
    match &resource.transport {
        McpTransport::Stdio {
            command,
            arguments,
            environment,
        } => {
            server.insert("command".into(), json!(command));
            server.insert("args".into(), json!(arguments));
            server.insert("env".into(), json!(environment));
            if let Some(directory) = &resource.working_directory {
                server.insert("cwd".into(), json!(directory));
            }
        }
        McpTransport::Http { url, headers } | McpTransport::Sse { url, headers } => {
            server.insert("url".into(), json!(url));
            server.insert("http_headers".into(), json!(headers));
        }
    }
    if let Some(forge_provider_api::session::McpToolSelection::Only(tools)) = &resource.tools {
        server.insert(
            "enabled_tools".into(),
            json!(tools.iter().map(|tool| tool.as_str()).collect::<Vec<_>>()),
        );
    }
    if let Some(timeout) = resource.timeout {
        let seconds = timeout.as_secs_f64();
        if !seconds.is_finite() || seconds <= 0.0 {
            return Err(failure(
                ProviderOperation::SessionConfigure,
                ProviderFailureKind::Rejected,
            ));
        }
        server.insert("tool_timeout_sec".into(), json!(seconds));
    }
    Ok(Value::Object(server))
}

fn disabled(resource: &McpLaunch) -> McpDefinition {
    let transport = match &resource.transport {
        McpTransport::Stdio { .. } => McpTransportKind::Stdio,
        McpTransport::Http { .. } => McpTransportKind::Http,
        McpTransport::Sse { .. } => McpTransportKind::Sse,
    };
    McpDefinition {
        resource_id: None,
        scope: Some(ResourceScope::Workspace),
        source_path: resource
            .working_directory
            .as_ref()
            .map(|path| path.to_string_lossy().into_owned()),
        name: resource.name.clone(),
        transport,
        enabled: false,
        status: McpStatus::Disabled,
        status_detail: None,
        token_count: None,
        token_estimated: false,
        tools: Vec::new(),
        tool_error: None,
    }
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    #[test]
    fn native_mcp_config_preserves_admitted_launch_details() {
        let resource = McpLaunch {
            name: "repo tools".into(),
            transport: McpTransport::Stdio {
                command: "tool-server".into(),
                arguments: vec!["--stdio".into()],
                environment: BTreeMap::from([("TOKEN".into(), "secret".into())]),
            },
            timeout: Some(std::time::Duration::from_secs(17)),
            working_directory: Some(std::path::PathBuf::from("/workspace")),
            tools: Some(
                forge_provider_api::session::McpToolSelection::from_names(vec!["search".into()])
                    .unwrap(),
            ),
        };
        let config = native_server(&resource).expect("valid normalized launch");
        assert_eq!(config["enabled"], true);
        assert_eq!(config["command"], "tool-server");
        assert_eq!(config["args"], json!(["--stdio"]));
        assert_eq!(config["env"]["TOKEN"], "secret");
        assert_eq!(config["cwd"], "/workspace");
        assert_eq!(config["enabled_tools"], json!(["search"]));
        assert_eq!(config["tool_timeout_sec"], 17.0);
    }

    #[test]
    fn private_aliases_restore_canonical_mcp_identity_and_hide_other_servers() {
        let resource = McpLaunch {
            name: "repo tools".into(),
            transport: McpTransport::Http {
                url: "https://mcp.example.invalid".into(),
                headers: BTreeMap::new(),
            },
            timeout: None,
            working_directory: None,
            tools: None,
        };
        let resources = CodexResources::new(
            ResourceSelection {
                skills: Vec::new(),
                mcp: vec![resource],
            },
            std::path::PathBuf::from("/private/profile"),
        );
        let alias = resources.native_name["repo tools"].clone();
        let native = McpDefinition {
            resource_id: None,
            scope: None,
            source_path: None,
            name: alias.clone(),
            transport: McpTransportKind::Http,
            enabled: true,
            status: McpStatus::Connected,
            status_detail: None,
            token_count: None,
            token_estimated: false,
            tools: Vec::new(),
            tool_error: None,
        };
        let normalized = resources.normalize_mcp(vec![native.clone()]);
        assert_eq!(normalized.len(), 1);
        assert_eq!(normalized[0].name, "repo tools");
        assert_eq!(normalized[0].status, McpStatus::Connected);
        assert_eq!(normalized[0].resource_id, None);

        let retired = vec![alias.clone()];
        let inherited = json!({"unselected":{"command":"must-not-run"}});
        let selected = resources
            .server_configuration(inherited.as_object(), &retired)
            .unwrap();
        assert_eq!(selected[&alias]["enabled"], true);
        assert_eq!(selected["unselected"]["enabled"], false);

        let empty = CodexResources::new(ResourceSelection::default(), "/private/empty".into());
        let config = empty
            .server_configuration(inherited.as_object(), &retired)
            .unwrap();
        assert_eq!(config[&alias]["enabled"], false);
        assert_eq!(config["unselected"]["enabled"], false);
        assert!(empty.normalize_mcp(vec![native]).is_empty());
    }
}
