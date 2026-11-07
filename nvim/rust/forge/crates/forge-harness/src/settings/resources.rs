use super::{ResourceCatalog, SettingsStore, SkillResource};
use crate::provider::{McpDefinition, McpStatus, ProviderCatalogRequest, SkillDefinition};
use anyhow::{Result, ensure};
use forge_provider_api::session::{McpLaunch, McpTransport, ResourceSelection};
use std::collections::HashSet;
use std::path::Path;

/// Resolves one provider's shared catalog while retaining credentials only in launch payloads.
pub(crate) struct SessionResources {
    /// Contains every applicable skill with its requested enabled state.
    pub(crate) skill: Vec<SkillDefinition>,
    /// Contains only the canonical sources to bind into a provider profile.
    pub(crate) selected_skill: Vec<SkillResource>,
    /// Contains every applicable server with credential failures isolated to its row.
    pub(crate) mcp: Vec<McpDefinition>,
    /// Contains typed launch requests with ephemeral credentials, never persisted in history.
    pub(crate) launch: Vec<McpLaunch>,
    /// Maps resources skipped for missing executables to their user-facing explanation.
    pub(crate) launch_notice: std::collections::HashMap<String, String>,
}

impl SessionResources {
    /// Supply admitted selections without choosing any provider wire representation.
    pub(crate) fn selection(&self) -> ResourceSelection {
        ResourceSelection {
            skills: self.skill.clone(),
            mcp: self.launch.clone(),
        }
    }

    /// Resolve persisted selection and reject provider-name collisions before launching a session.
    pub(crate) fn load(
        root: &Path,
        request: &ProviderCatalogRequest,
        provider: &str,
        target: &str,
    ) -> Result<Self> {
        Self::load_with_selection(root, request, provider, target, None)
    }

    /// Resolve a proposed selection before committing it to the host settings store.
    pub(crate) fn load_with_selection(
        root: &Path,
        request: &ProviderCatalogRequest,
        provider: &str,
        target: &str,
        selection: Option<&super::PortableSettings>,
    ) -> Result<Self> {
        let store = SettingsStore::open(root)?;
        let catalog = store.catalog(&request.workspace, &request.harness_session_id, provider)?;
        let settings = selection.unwrap_or(&catalog.settings.desired);
        let mut selected_skill = Vec::new();
        let mut skill = Vec::new();
        let mut selected_name = HashSet::new();
        for definition in catalog.skill {
            if !ResourceCatalog::supports_target(&definition.targets, target) {
                continue;
            }
            let enabled = settings.skill.get(&definition.id) != Some(&false);
            if enabled {
                ensure!(
                    selected_name.insert(definition.name.clone()),
                    "Skill {} has multiple selected definitions",
                    definition.name
                );
                selected_skill.push(definition.clone());
            }
            let source = if definition.id.starts_with("import:") {
                "imported"
            } else {
                "rulesync"
            };
            skill.push(SkillDefinition {
                scope: definition.scope,
                resource_id: Some(definition.id),
                name: definition.name,
                description: definition.description,
                enabled,
                user_invocable: definition.user_invocable,
                path: Some(definition.source),
                source: Some(source.into()),
                argument_hint: None,
            });
        }
        let mut mcp = Vec::new();
        let mut launch = Vec::new();
        let mut launch_notice = std::collections::HashMap::new();
        let mut selected_name = HashSet::new();
        for definition in catalog.server {
            if !ResourceCatalog::supports_target(&definition.targets, target) {
                continue;
            }
            let enabled = settings.mcp.get(&definition.id) != Some(&false);
            let (status, status_detail) = if enabled {
                match definition.launch() {
                    Ok(payload) => {
                        ensure!(
                            selected_name.insert(definition.name.clone()),
                            "MCP server {} collides with another selected definition or Harness control",
                            definition.name
                        );
                        if let Some(command) =
                            unavailable_command(&payload, Path::new(&request.workspace))
                        {
                            let message = format!(
                                "MCP server {} was not started: executable '{command}' was not found or is not executable. Install it and retry in /mcp.",
                                definition.name
                            );
                            launch_notice.insert(definition.id.clone(), message.clone());
                            (McpStatus::Unavailable, Some(message))
                        } else {
                            launch.push(payload);
                            (McpStatus::Loading, None)
                        }
                    }
                    Err(error) => {
                        let message = error.to_string();
                        launch_notice.insert(definition.id.clone(), message.clone());
                        (McpStatus::Unavailable, Some(message))
                    }
                }
            } else {
                (McpStatus::Disabled, None)
            };
            mcp.push(McpDefinition {
                scope: definition.scope,
                source_path: Some(definition.source),
                resource_id: Some(definition.id),
                name: definition.name,
                transport: match definition.transport {
                    super::catalog::ServerTransport::Stdio => {
                        forge_provider_api::catalog::McpTransportKind::Stdio
                    }
                    super::catalog::ServerTransport::Http => {
                        forge_provider_api::catalog::McpTransportKind::Http
                    }
                    super::catalog::ServerTransport::Sse => {
                        forge_provider_api::catalog::McpTransportKind::Sse
                    }
                },
                enabled,
                status,
                status_detail: status_detail
                    .and_then(|message| forge_provider_api::SanitizedMessage::new(message).ok()),
                token_count: None,
                token_estimated: false,
                tools: Vec::new(),
                tool_error: None,
            });
        }
        Ok(Self {
            skill,
            selected_skill,
            mcp,
            launch,
            launch_notice,
        })
    }
}

fn unavailable_command(payload: &McpLaunch, workspace: &Path) -> Option<String> {
    let McpTransport::Stdio {
        command,
        environment,
        ..
    } = &payload.transport
    else {
        return None;
    };
    let directory = payload
        .working_directory
        .as_ref()
        .map(|directory| workspace.join(directory))
        .unwrap_or_else(|| workspace.to_path_buf());
    let inherited_path = std::env::var_os("PATH").unwrap_or_default();
    let path = {
        environment.iter().find(|(name, _)| {
            if cfg!(windows) {
                name.eq_ignore_ascii_case("PATH")
            } else {
                name.as_str() == "PATH"
            }
        })
    }
    .map(|(_, value)| value.as_str())
    .map(std::ffi::OsStr::new)
    .unwrap_or(&inherited_path);
    crate::process::executable_at(command, &directory, path)
        .is_none()
        .then(|| command.to_owned())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::session::{AccessPolicy, ApprovalMode};

    #[test]
    fn shared_resources_apply_selection_without_exposing_launch_credentials() {
        let root = tempfile::tempdir().unwrap();
        let global = root.path().join("rulesync-global/.rulesync");
        let workspace = root.path().join("workspace");
        std::fs::create_dir_all(global.join("skills/shared-skill")).unwrap();
        std::fs::create_dir_all(workspace.join(".rulesync/skills/fixture-gamma-only")).unwrap();
        std::fs::write(global.join("skills/shared-skill/SKILL.md"), "---\nname: shared-skill\ndescription: Shared fixture\ntargets: [fixture-beta-target]\n---\nFixture").unwrap();
        std::fs::write(
            workspace.join(".rulesync/skills/fixture-gamma-only/SKILL.md"),
            "---\nname: fixture-gamma-only\ntargets: [fixture-gamma-target]\n---\nFixture",
        )
        .unwrap();
        std::fs::write(global.join("mcp.json"), serde_json::json!({"mcpServers": {
            "selected": {"command":std::env::current_exe().unwrap(), "env":{"FIXTURE_TOKEN":"credential-fixture"}},
            "missing": {"command":"fixture", "envVars":["FORGE_RESOURCE_TEST_MISSING_81D3"]}
        }}).to_string()).unwrap();
        let mut store = SettingsStore::open(root.path()).unwrap();
        store.configure_catalog(root.path()).unwrap();
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: workspace.to_string_lossy().into_owned(),
            access: AccessPolicy::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
        };
        let loaded =
            SessionResources::load(root.path(), &request, "fixture-beta", "fixture-beta-target")
                .unwrap();
        assert_eq!(loaded.skill.len(), 1);
        assert_eq!(loaded.selected_skill[0].name, "shared-skill");
        assert_eq!(loaded.launch.len(), 1);
        assert!(matches!(&loaded.launch[0].transport,
            McpTransport::Stdio { environment, .. }
                if environment.get("FIXTURE_TOKEN").map(String::as_str) == Some("credential-fixture")));
        assert!(
            !serde_json::to_string(&loaded.mcp)
                .unwrap()
                .contains("credential-fixture")
        );
        assert_eq!(
            loaded
                .mcp
                .iter()
                .find(|server| server.name == "missing")
                .unwrap()
                .status,
            McpStatus::Unavailable
        );
        let mut setting = super::super::PortableSettings::default();
        setting
            .skill
            .insert(loaded.skill[0].resource_id.clone().unwrap(), false);
        let missing_id = loaded
            .mcp
            .iter()
            .find(|server| server.name == "missing")
            .unwrap()
            .resource_id
            .clone()
            .unwrap();
        store
            .update(
                0,
                super::super::SettingScope::Global,
                &request.workspace,
                &request.harness_session_id,
                &setting,
            )
            .unwrap();
        assert!(
            SessionResources::load(root.path(), &request, "fixture-beta", "fixture-beta-target")
                .unwrap()
                .selected_skill
                .is_empty()
        );
        setting.mcp.insert(missing_id, true);
        store
            .update(
                1,
                super::super::SettingScope::Global,
                &request.workspace,
                &request.harness_session_id,
                &setting,
            )
            .unwrap();
        for (provider, target) in [
            ("fixture-alpha", "fixture-alpha-target"),
            ("fixture-beta", "fixture-beta-target"),
            ("fixture-gamma", "fixture-gamma-target"),
            ("fixture-delta", "fixture-delta-target"),
        ] {
            let loaded = SessionResources::load(root.path(), &request, provider, target).unwrap();
            let missing = loaded
                .mcp
                .iter()
                .find(|server| server.name == "missing")
                .unwrap();
            assert!(missing.enabled);
            assert_eq!(missing.status, McpStatus::Unavailable);
            assert!(
                missing
                    .status_detail
                    .as_ref()
                    .map(forge_provider_api::SanitizedMessage::as_str)
                    .unwrap()
                    .contains("FORGE_RESOURCE_TEST_MISSING_81D3")
            );
            assert_eq!(loaded.launch.len(), 1);
            assert_eq!(loaded.launch[0].name, "selected");
        }
    }
}
