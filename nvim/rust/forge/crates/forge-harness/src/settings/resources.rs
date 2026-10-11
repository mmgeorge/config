use super::{SettingsStore, SkillResource};
use crate::backend::{BackendCatalogRequest, McpDefinition, McpStatus, SkillDefinition};
use anyhow::{Result, ensure};
use serde_json::Value;
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
    /// Contains resolved native launch payloads and must never be serialized into history.
    pub(crate) launch: Vec<(String, Value)>,
}

impl SessionResources {
    /// Encode the selected launch payloads through ACP's portable server contract.
    pub(crate) fn acp(&self) -> Result<Vec<Value>> {
        self.launch.iter().map(|(name, launch)| {
            ensure!(launch.get("tools").is_none_or(|tools| tools == &serde_json::json!(["*"]))
                && launch.get("timeout").is_none() && launch.get("cwd").is_none(),
                "ACP cannot preserve MCP tool selection, timeout, or working directory for {name}");
            let mut server = launch.clone();
            let fields = server.as_object_mut().expect("validated MCP launch object");
            fields.remove("tools");
            if fields.get("type").and_then(Value::as_str) == Some("stdio") { fields.remove("type"); }
            fields.insert("name".into(), serde_json::json!(name));
            for key in ["env", "headers"] {
                if let Some(value) = fields.get(key).and_then(Value::as_object) {
                    let value = value.iter().map(|(name, value)|serde_json::json!({"name":name,"value":value})).collect::<Vec<_>>();
                    fields.insert(key.into(), serde_json::json!(value));
                }
            }
            Ok(server)
        }).collect()
    }

    /// Resolve persisted selection and reject provider-name collisions before launching a session.
    pub(crate) fn load(root: &Path, request: &BackendCatalogRequest, backend: &str, target: &str) -> Result<Self> {
        let store = SettingsStore::open(root)?;
        let catalog = store.catalog(&request.workspace, &request.harness_session_id, backend)?;
        let settings = catalog.settings;
        let mut selected_skill = Vec::new();
        let mut skill = Vec::new();
        let mut selected_name = HashSet::new();
        for definition in catalog.skill {
            if !definition.targets.is_empty() && !definition.targets.iter().any(|candidate| candidate == target) { continue; }
            let enabled = settings.desired.skill.get(&definition.id) != Some(&false);
            if enabled {
                ensure!(selected_name.insert(definition.name.clone()), "Skill {} has multiple selected definitions", definition.name);
                selected_skill.push(definition.clone());
            }
            let source = if definition.id.starts_with("import:") { "imported" } else { "rulesync" };
            skill.push(SkillDefinition {
                resource_id: Some(definition.id), name: definition.name, description: definition.description,
                enabled, user_invocable: definition.user_invocable, path: Some(definition.source),
                source: Some(source.into()), argument_hint: None,
            });
        }
        let mut mcp = Vec::new();
        let mut launch = Vec::new();
        let mut selected_name = HashSet::from(["forge-harness-control".to_owned()]);
        for definition in catalog.server {
            if !definition.targets.is_empty() && !definition.targets.iter().any(|candidate| candidate == target) { continue; }
            let enabled = settings.desired.mcp.get(&definition.id) != Some(&false);
            let (status, status_detail) = if enabled {
                match definition.native() {
                    Ok(payload) => {
                        ensure!(selected_name.insert(definition.name.clone()),
                            "MCP server {} collides with another selected definition or Harness control", definition.name);
                        launch.push((definition.name.clone(), payload));
                        (McpStatus::Loading, None)
                    }
                    Err(error) if settings.desired.mcp.get(&definition.id) != Some(&true) => {
                        (McpStatus::Unavailable, Some(error.to_string()))
                    }
                    Err(error) => return Err(error.context("Explicitly enabled MCP server cannot be launched")),
                }
            } else { (McpStatus::Disabled, None) };
            mcp.push(McpDefinition {
                resource_id: Some(definition.id), name: definition.name, transport: definition.transport,
                enabled, status, status_detail, token_count: None, token_estimated: false,
                tools: Vec::new(), tool_error: None,
            });
        }
        Ok(Self { skill, selected_skill, mcp, launch })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::session::{AccessPolicy, PermissionMode};

    #[test]
    fn shared_resources_apply_selection_without_exposing_launch_credentials() {
        let root = tempfile::tempdir().unwrap();
        let global = root.path().join("rulesync-global/.rulesync");
        let workspace = root.path().join("workspace");
        std::fs::create_dir_all(global.join("skills/shared-skill")).unwrap();
        std::fs::create_dir_all(workspace.join(".rulesync/skills/claude-only")).unwrap();
        std::fs::write(global.join("skills/shared-skill/SKILL.md"), "---\nname: shared-skill\ndescription: Shared fixture\ntargets: [copilotcli]\n---\nFixture").unwrap();
        std::fs::write(workspace.join(".rulesync/skills/claude-only/SKILL.md"), "---\nname: claude-only\ntargets: [claudecode]\n---\nFixture").unwrap();
        std::fs::write(global.join("mcp.json"), serde_json::json!({"mcpServers": {
            "selected": {"command":"fixture", "env":{"FIXTURE_TOKEN":"credential-fixture"}},
            "missing": {"command":"fixture", "envVars":["FORGE_RESOURCE_TEST_MISSING_81D3"]}
        }}).to_string()).unwrap();
        let mut store = SettingsStore::open(root.path()).unwrap();
        store.configure_catalog(root.path()).unwrap();
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id:"session".into(), workspace:workspace.to_string_lossy().into_owned(),
            access:AccessPolicy::default(), execution_mode:PermissionMode::Read, backend_session_id:None,
        };
        let loaded = SessionResources::load(root.path(), &request, "copilot", "copilotcli").unwrap();
        assert_eq!(loaded.skill.len(), 1);
        assert_eq!(loaded.selected_skill[0].name, "shared-skill");
        assert_eq!(loaded.launch.len(), 1);
        assert_eq!(loaded.launch[0].1["env"]["FIXTURE_TOKEN"], "credential-fixture");
        assert!(!serde_json::to_string(&loaded.mcp).unwrap().contains("credential-fixture"));
        assert_eq!(loaded.mcp.iter().find(|server|server.name == "missing").unwrap().status, McpStatus::Unavailable);
        let mut setting = super::super::PortableSettings::default();
        setting.skill.insert(loaded.skill[0].resource_id.clone().unwrap(), false);
        let missing_id = loaded.mcp.iter().find(|server|server.name == "missing").unwrap().resource_id.clone().unwrap();
        store.update(0, super::super::SettingScope::Global, &request.workspace, &request.harness_session_id, &setting).unwrap();
        assert!(SessionResources::load(root.path(), &request, "copilot", "copilotcli").unwrap().selected_skill.is_empty());
        setting.mcp.insert(missing_id, true);
        store.update(1, super::super::SettingScope::Global, &request.workspace, &request.harness_session_id, &setting).unwrap();
        assert!(SessionResources::load(root.path(), &request, "copilot", "copilotcli").is_err());
    }
}
