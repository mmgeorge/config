use crate::backend::ResourceScope;
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::Path;

#[derive(Clone, Deserialize, Serialize)]
/// Identifies one skill definition independently of provider installation paths.
pub(crate) struct SkillResource {
    /// Identifies the canonical Rulesync definition across content changes.
    pub(crate) id: String,
    /// Names the skill in provider selection requests.
    pub(crate) name: String,
    /// Supplies the parsed frontmatter description without retaining prompt contents.
    pub(crate) description: String,
    /// Identifies the exact canonical definition path.
    pub(crate) source: String,
    #[serde(skip)]
    /// Carries discovery or import scope without duplicating persisted import ownership.
    pub(crate) scope: Option<ResourceScope>,
    /// Restricts provider bindings when the source declares targets.
    pub(crate) targets: Vec<String>,
    /// Determines whether the skill appears in explicit invocation menus.
    pub(crate) user_invocable: bool,
}

#[derive(Deserialize)]
struct SkillMetadata {
    name: String,
    #[serde(default)]
    description: String,
    #[serde(default)]
    targets: Vec<String>,
    #[serde(default = "invocable", rename = "user-invocable")]
    user_invocable: bool,
}

fn invocable() -> bool {
    true
}

#[derive(Clone, Serialize)]
/// Identifies one Rulesync server without exposing launch credentials to presentation.
pub(crate) struct ServerResource {
    /// Identifies the canonical source path and source key across content revisions.
    pub(crate) id: String,
    /// Retains the Rulesync key used by provider bindings.
    pub(crate) name: String,
    /// Records the actual definition source.
    pub(crate) source: String,
    /// Carries the scope of the source definition.
    pub(crate) scope: Option<ResourceScope>,
    /// Records which provider targets can express this definition.
    pub(crate) targets: Vec<String>,
    /// Records the configured transport independently of live availability.
    pub(crate) transport: String,
    #[serde(skip)]
    definition: Value,
}

impl ServerResource {
    /// Validate an imported launch definition without resolving credential references.
    pub(crate) fn imported(
        id: String,
        name: String,
        source: String,
        definition: Value,
    ) -> Result<Self> {
        let transport = definition["type"]
            .as_str()
            .unwrap_or(if definition["url"].is_string() {
                "http"
            } else {
                "stdio"
            });
        ensure!(
            matches!(transport, "stdio" | "http" | "sse"),
            "unsupported MCP transport {transport}"
        );
        Ok(Self {
            id,
            name,
            source,
            scope: None,
            transport: transport.into(),
            targets: definition["targets"]
                .as_array()
                .into_iter()
                .flatten()
                .filter_map(|target| target.as_str().map(str::to_owned))
                .collect(),
            definition,
        })
    }

    /// Resolve a native SDK launch definition without persisting credential values.
    pub(crate) fn native(&self) -> Result<Value> {
        let mut launch = self.acp()?;
        launch["type"] = json!(self.transport);
        launch
            .as_object_mut()
            .context("MCP launch must be an object")?
            .remove("name");
        for field in ["env", "headers"] {
            if let Some(entries) = launch[field].as_array() {
                let value = entries
                    .iter()
                    .map(|entry| {
                        (
                            entry["name"].as_str().unwrap_or_default().to_owned(),
                            entry["value"].clone(),
                        )
                    })
                    .collect::<serde_json::Map<_, _>>();
                launch[field] = Value::Object(value);
            }
        }
        for field in ["tools", "timeout", "cwd"] {
            if let Some(value) = self.definition.get(field) {
                launch[field] = value.clone();
            }
        }
        Ok(launch)
    }

    /// Resolve credential references only into the ephemeral ACP launch payload.
    pub(crate) fn acp(&self) -> Result<Value> {
        let definition = &self.definition;
        let mut native = super::import::NativeValues::default();
        native.validate(&self.name, definition)?;
        let mut environment = BTreeMap::<String, String>::new();
        for name in definition["envVars"]
            .as_array()
            .into_iter()
            .flatten()
            .filter_map(Value::as_str)
        {
            environment.insert(
                name.into(),
                std::env::var(name).with_context(|| {
                    format!("MCP {} requires environment variable {name}", self.name)
                })?,
            );
        }
        for (name, value) in definition["env"].as_object().into_iter().flatten() {
            environment.insert(
                name.clone(),
                expand(
                    value
                        .as_str()
                        .context("MCP environment value must be text")?,
                )?,
            );
        }
        for (name, reference) in definition["native_env"].as_object().into_iter().flatten() {
            environment.insert(name.clone(), native.resolve(reference)?);
        }
        if self.transport == "stdio" {
            let command = definition["command"]
                .as_str()
                .context("stdio MCP command missing")?;
            let argument = if let Some(reference) = definition["native_args"].as_array() {
                reference
                    .iter()
                    .map(|reference| native.resolve(reference))
                    .collect::<Result<Vec<_>>>()?
            } else {
                serde_json::from_value(definition.get("args").cloned().unwrap_or(json!([])))?
            };
            return Ok(json!({"name":self.name,"command":command,"args":argument,
                "env":environment.into_iter().map(|(name,value)|json!({"name":name,"value":value})).collect::<Vec<_>>()}));
        }
        let mut headers = BTreeMap::<String, String>::new();
        for key in ["headers", "http_headers"] {
            for (name, value) in definition[key].as_object().into_iter().flatten() {
                headers.insert(
                    name.clone(),
                    expand(value.as_str().context("MCP header value must be text")?)?,
                );
            }
        }
        for (name, reference) in definition["native_headers"]
            .as_object()
            .into_iter()
            .flatten()
        {
            headers.insert(name.clone(), native.resolve(reference)?);
        }
        for (name, value) in definition["env_http_headers"]
            .as_object()
            .into_iter()
            .flatten()
        {
            let reference = value
                .as_str()
                .context("MCP header reference must be text")?;
            headers.insert(
                name.clone(),
                std::env::var(reference).with_context(|| {
                    format!(
                        "MCP {} requires environment variable {reference}",
                        self.name
                    )
                })?,
            );
        }
        if let Some(name) = definition["bearer_token_env_var"].as_str() {
            headers.insert(
                "Authorization".into(),
                format!(
                    "Bearer {}",
                    std::env::var(name).with_context(|| format!(
                        "MCP {} requires environment variable {name}",
                        self.name
                    ))?
                ),
            );
        }
        let url = if let Some(reference) = definition.get("native_url") {
            native.resolve(reference)?
        } else {
            definition["url"]
                .as_str()
                .context("HTTP MCP URL missing")?
                .to_owned()
        };
        Ok(json!({"type":self.transport,"name":self.name,"url":url,
            "headers":headers.into_iter().map(|(name,value)|json!({"name":name,"value":value})).collect::<Vec<_>>()}))
    }
}

/// Reads shared resource definitions without modifying Rulesync or generated provider files.
pub(crate) struct ResourceCatalog;

impl ResourceCatalog {
    /// Accept empty targets, the all-provider `*`, or an exact provider target name.
    pub(crate) fn supports_target(targets: &[String], target: &str) -> bool {
        targets.is_empty()
            || targets
                .iter()
                .any(|candidate| candidate == "*" || candidate == target)
    }

    /// Parse global and workspace skill sources without importing generated files.
    pub(crate) fn skills(
        config_dir: Option<&Path>,
        workspace: &Path,
    ) -> Result<Vec<SkillResource>> {
        let mut pending = Vec::new();
        if let Some(config_dir) = config_dir {
            pending.push((
                config_dir.join("rulesync-global/.rulesync/skills"),
                Some(ResourceScope::Global),
            ));
        }
        pending.push((
            workspace.join(".rulesync/skills"),
            Some(ResourceScope::Workspace),
        ));
        Self::skill_sources(pending, "rulesync")
    }

    /// Inspect native skill metadata without importing it into shared configuration.
    pub(crate) fn native_skills(directory: Vec<std::path::PathBuf>) -> Result<Vec<SkillResource>> {
        Self::skill_sources(
            directory.into_iter().map(|path| (path, None)).collect(),
            "native",
        )
    }

    fn skill_sources(
        mut pending: Vec<(std::path::PathBuf, Option<ResourceScope>)>,
        namespace: &str,
    ) -> Result<Vec<SkillResource>> {
        let mut visited = std::collections::HashSet::new();
        let mut resource = Vec::new();
        while let Some((directory, scope)) = pending.pop() {
            if !directory.is_dir() {
                continue;
            }
            let directory = dunce::canonicalize(directory)?;
            if !visited.insert(directory.clone()) {
                continue;
            }
            let source = directory.join("SKILL.md");
            if source.is_file() {
                let text = std::fs::read_to_string(&source)?;
                let text = text.replace("\r\n", "\n");
                let body = text
                    .strip_prefix("---\n")
                    .with_context(|| format!("skill frontmatter missing: {}", source.display()))?;
                let (header, _) = body
                    .split_once("\n---")
                    .context("skill frontmatter closing delimiter missing")?;
                let metadata: SkillMetadata = serde_yaml_ng::from_str(header)
                    .with_context(|| format!("skill frontmatter invalid: {}", source.display()))?;
                ensure!(
                    !metadata.name.is_empty()
                        && metadata
                            .name
                            .chars()
                            .all(|character| character.is_ascii_alphanumeric()
                                || "-_.:".contains(character)),
                    "skill name is invalid"
                );
                let source = dunce::canonicalize(source)?;
                let identity = source.to_string_lossy().replace('\\', "/");
                let identity = if cfg!(windows) {
                    identity.to_lowercase()
                } else {
                    identity
                };
                resource.push(SkillResource {
                    id: format!(
                        "{namespace}:{}",
                        hex::encode(Sha256::digest(identity.as_bytes()))
                    ),
                    name: metadata.name,
                    description: metadata.description,
                    source: source.display().to_string(),
                    scope,
                    targets: metadata.targets,
                    user_invocable: metadata.user_invocable,
                });
            } else {
                for entry in std::fs::read_dir(directory)? {
                    let path = entry?.path();
                    if path.is_dir() {
                        pending.push((path, scope));
                    }
                }
            }
        }
        resource.sort_by(|first, second| first.source.cmp(&second.source));
        Ok(resource)
    }

    /// Load global defaults and an explicitly present workspace source with distinct identities.
    pub(crate) fn servers(
        config_dir: Option<&Path>,
        workspace: &Path,
    ) -> Result<Vec<ServerResource>> {
        let mut source_list = Vec::new();
        if let Some(config_dir) = config_dir {
            source_list.push((
                config_dir.join("rulesync-global/.rulesync/mcp.json"),
                ResourceScope::Global,
            ));
        }
        source_list.push((
            workspace.join(".rulesync/mcp.json"),
            ResourceScope::Workspace,
        ));
        let mut server_list = Vec::new();
        for (source, scope) in source_list {
            if !source.is_file() {
                continue;
            }
            let source = dunce::canonicalize(source)?;
            let source_identity = source.to_string_lossy().replace('\\', "/");
            let source_identity = if cfg!(windows) {
                source_identity.to_lowercase()
            } else {
                source_identity
            };
            let catalog: Value = serde_json::from_slice(&std::fs::read(&source)?)
                .with_context(|| format!("read Rulesync MCP source {}", source.display()))?;
            for (name, definition) in catalog["mcpServers"].as_object().into_iter().flatten() {
                let identity = format!("{source_identity}\0{name}");
                let id = format!(
                    "rulesync:{}",
                    hex::encode(Sha256::digest(identity.as_bytes()))
                );
                let transport =
                    definition["type"]
                        .as_str()
                        .unwrap_or(if definition["url"].is_string() {
                            "http"
                        } else {
                            "stdio"
                        });
                ensure!(
                    matches!(transport, "stdio" | "http" | "sse"),
                    "unsupported Rulesync MCP transport {transport}"
                );
                server_list.push(ServerResource {
                    id,
                    name: name.clone(),
                    source: source.display().to_string(),
                    scope: Some(scope),
                    targets: definition["targets"]
                        .as_array()
                        .into_iter()
                        .flatten()
                        .filter_map(|target| target.as_str().map(str::to_owned))
                        .collect(),
                    transport: transport.into(),
                    definition: definition.clone(),
                });
            }
        }
        Ok(server_list)
    }
}

fn expand(value: &str) -> Result<String> {
    let mut result = String::new();
    let mut remainder = value;
    while let Some(start) = remainder.find('$') {
        result.push_str(&remainder[..start]);
        remainder = &remainder[start + 1..];
        let (name, rest) = if let Some(braced) = remainder.strip_prefix('{') {
            let end = braced
                .find('}')
                .context("unterminated MCP environment reference")?;
            (&braced[..end], &braced[end + 1..])
        } else {
            let end = remainder
                .find(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                .unwrap_or(remainder.len());
            (&remainder[..end], &remainder[end..])
        };
        ensure!(!name.is_empty(), "MCP environment reference is empty");
        result.push_str(
            &std::env::var(name)
                .with_context(|| format!("MCP requires environment variable {name}"))?,
        );
        remainder = rest;
    }
    result.push_str(remainder);
    Ok(result)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn harness_check_skill_is_discoverable_for_every_provider() {
        let fixture = tempfile::tempdir().unwrap();
        let source = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../../../../rulesync-global/.rulesync/skills/harness-checks/SKILL.md");
        let directory = fixture
            .path()
            .join("rulesync-global/.rulesync/skills/harness-checks");
        std::fs::create_dir_all(&directory).unwrap();
        std::fs::copy(source, directory.join("SKILL.md")).unwrap();
        let skill =
            ResourceCatalog::skills(Some(fixture.path()), &fixture.path().join("workspace"))
                .unwrap();
        assert_eq!(skill.len(), 1);
        assert_eq!(skill[0].name, "harness-checks");
        assert_eq!(skill[0].scope, Some(crate::backend::ResourceScope::Global));
        assert!(skill[0].user_invocable);
        assert!(skill[0].description.contains("checks_configure"));
        for target in ["codexcli", "copilotcli", "claudecode", "antigravity-cli"] {
            assert!(ResourceCatalog::supports_target(&skill[0].targets, target));
        }
    }

    #[test]
    fn source_identity_survives_definition_change_and_does_not_merge_same_names() {
        let fixture = tempfile::tempdir().unwrap();
        let global = fixture.path().join("global");
        let workspace = fixture.path().join("workspace");
        std::fs::create_dir_all(global.join("rulesync-global/.rulesync")).unwrap();
        std::fs::create_dir_all(workspace.join(".rulesync")).unwrap();
        let source = global.join("rulesync-global/.rulesync/mcp.json");
        std::fs::write(&source, r#"{"mcpServers":{"same":{"command":"first"}}}"#).unwrap();
        std::fs::write(
            workspace.join(".rulesync/mcp.json"),
            r#"{"mcpServers":{"same":{"command":"other"}}}"#,
        )
        .unwrap();
        let before = ResourceCatalog::servers(Some(&global), &workspace).unwrap();
        std::fs::write(source, r#"{"mcpServers":{"same":{"command":"changed"}}}"#).unwrap();
        let after = ResourceCatalog::servers(Some(&global), &workspace).unwrap();
        assert_eq!(before[0].id, after[0].id);
        assert_ne!(before[0].id, before[1].id);
        assert!(
            !serde_json::to_string(&after[0])
                .unwrap()
                .contains("changed")
        );
    }

    #[test]
    fn skill_identity_retains_folded_metadata_and_distinct_sources() {
        let fixture = tempfile::tempdir().unwrap();
        let global = fixture.path().join("global");
        let workspace = fixture.path().join("workspace");
        let global_skill = global.join("rulesync-global/.rulesync/skills/example");
        let workspace_skill = workspace.join(".rulesync/skills/example");
        std::fs::create_dir_all(&global_skill).unwrap();
        std::fs::create_dir_all(&workspace_skill).unwrap();
        let source = "---\nname: example\ndescription: >-\n  First line\n  continues here.\nuser-invocable: false\n---\nPrivate prompt contents\n";
        std::fs::write(global_skill.join("SKILL.md"), source).unwrap();
        std::fs::write(workspace_skill.join("SKILL.md"), source).unwrap();
        let before = ResourceCatalog::skills(Some(&global), &workspace).unwrap();
        assert_eq!(before.len(), 2);
        assert_ne!(before[0].id, before[1].id);
        assert_eq!(before[0].description, "First line continues here.");
        assert!(!before[0].user_invocable);
        assert!(
            !serde_json::to_string(&before)
                .unwrap()
                .contains("Private prompt")
        );
        std::fs::write(
            global_skill.join("SKILL.md"),
            source.replace("Private prompt", "Changed prompt"),
        )
        .unwrap();
        assert_eq!(
            before[0].id,
            ResourceCatalog::skills(Some(&global), &workspace).unwrap()[0].id
        );
    }
}
