use super::catalog::{ResourceCatalog, ServerResource, SkillResource};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::io::Read;
use std::path::{Path, PathBuf};

const SOURCE_LIMIT: u64 = 1024 * 1024;

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects the native source format without importing authentication or tuning.
pub(crate) enum ImportKind {
    /// Reads one native JSON or TOML MCP configuration file.
    Mcp,
    /// Reads a native skill directory and retains its canonical definitions.
    Skills,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Stores explicit resource imports independently from generated provider files.
pub(crate) enum ImportedResource {
    /// Retains a skill source and its portable metadata.
    Skill { resource: SkillResource },
    /// Retains a sanitized MCP definition with unresolved credential references.
    Mcp { id: String, name: String, source: String, definition: Value },
}

impl ImportedResource {
    /// Identify the persistent canonical source binding.
    pub(crate) fn id(&self) -> &str {
        match self { Self::Skill { resource } => &resource.id, Self::Mcp { id, .. } => id }
    }

    /// Identify the provider-visible resource name used for collision validation.
    pub(crate) fn name(&self) -> &str {
        match self { Self::Skill { resource } => &resource.name, Self::Mcp { name, .. } => name }
    }

    /// Identify the preference map that controls this resource.
    pub(crate) fn kind(&self) -> &'static str {
        match self { Self::Skill { .. } => "skill", Self::Mcp { .. } => "mcp" }
    }

    /// Reconstruct a provider-independent launch definition without reading native configuration.
    pub(crate) fn server(&self) -> Result<Option<ServerResource>> {
        match self {
            Self::Mcp { id, name, source, definition } => Ok(Some(ServerResource::imported(
                id.clone(), name.clone(), source.clone(), definition.clone())?)),
            Self::Skill { .. } => Ok(None),
        }
    }
}

#[derive(Serialize)]
/// Shows import provenance and rejection reasons without exposing credential values.
pub(crate) struct ImportCandidate {
    /// Identifies one source entry across preview and apply.
    pub(crate) id: String,
    /// Identifies the native resource name.
    pub(crate) name: String,
    /// Distinguishes skills from MCP definitions.
    pub(crate) kind: &'static str,
    /// Records the canonical file supplying the entry.
    pub(crate) source: String,
    /// Reports MCP transport when applicable.
    pub(crate) transport: Option<String>,
    /// Reports an actionable validation failure without native values.
    pub(crate) blocked: Option<String>,
    /// Reports colliding selected source names before application.
    pub(crate) conflict: Vec<String>,
}

#[derive(Serialize)]
/// Retains a bounded source revision and the entries the user can explicitly import.
pub(crate) struct NativeImport {
    /// Identifies the canonical source directory or configuration file.
    pub(crate) source: PathBuf,
    /// Detects native source changes between preview and application.
    pub(crate) fingerprint: String,
    /// Identifies the committed preference revision used for conflict inspection.
    pub(crate) revision: u64,
    /// Exposes only provenance and portable import readiness.
    pub(crate) candidate: Vec<ImportCandidate>,
    #[serde(skip)]
    resource: Vec<ImportedResource>,
}

impl NativeImport {
    /// Read native resource definitions without changing provider files or resolving credentials.
    pub(crate) fn read(kind: ImportKind, source: &Path, namespace: &str, workspace: &Path) -> Result<Self> {
        let source = if source.is_absolute() { source.to_path_buf() } else { workspace.join(source) };
        let source = dunce::canonicalize(source).context("native import source does not exist")?;
        let mut digest = Sha256::new();
        digest.update(source.to_string_lossy().as_bytes());
        let mut imported = Self { source:source.clone(), fingerprint:String::new(), revision:0, candidate:Vec::new(), resource:Vec::new() };
        match kind {
            ImportKind::Skills => {
                ensure!(source.is_dir(), "skill import requires a directory");
                let resource = ResourceCatalog::native_skills(vec![source])?;
                ensure!(resource.len() <= 128, "native import exceeds 128 skills");
                for mut skill in resource {
                    digest.update(skill.source.as_bytes());
                    digest.update(bounded_read(Path::new(&skill.source), 65536)?);
                    skill.id = identity(namespace, &skill.source, &skill.name);
                    imported.candidate.push(ImportCandidate { id:skill.id.clone(), name:skill.name.clone(), kind:"skill",
                        source:skill.source.clone(), transport:None, blocked:None, conflict:Vec::new() });
                    imported.resource.push(ImportedResource::Skill { resource:skill });
                }
            }
            ImportKind::Mcp => {
                ensure!(source.is_file(), "MCP import requires a configuration file");
                let content = bounded_read(&source, SOURCE_LIMIT)?;
                digest.update(&content);
                let document = document(&source, &content)?;
                let mut server = BTreeMap::new();
                for key in ["mcpServers", "mcp_servers"] {
                    if let Some(definition) = document[key].as_object() {
                        for (name, definition) in definition { server.insert(name, (definition, format!("/{key}/{}", pointer_key(name)))); }
                    }
                }
                let mut project_entry = document["projects"].as_object().into_iter().flatten()
                    .filter(|(path, _)| path_identity(path) == path_identity(workspace.to_string_lossy().as_ref()));
                if let Some((path, project)) = project_entry.next() {
                    ensure!(project_entry.next().is_none(), "native MCP source contains ambiguous workspace entries");
                    if let Some(definition) = project["mcpServers"].as_object() {
                        for (name, definition) in definition { server.insert(name, (definition,
                            format!("/projects/{}/mcpServers/{}", pointer_key(path), pointer_key(name)))); }
                    }
                }
                ensure!(server.len() <= 128, "native import exceeds 128 MCP servers");
                for (name, (definition, pointer)) in server {
                    let id = identity(namespace, source.to_string_lossy().as_ref(), name);
                    let sanitized = sanitize_server(name, definition, &source, &pointer);
                    let (transport, blocked) = match &sanitized {
                        Ok(definition) => (definition["type"].as_str().map(str::to_owned), None),
                        Err(error) => (None, Some(error.to_string())),
                    };
                    imported.candidate.push(ImportCandidate { id:id.clone(), name:name.clone(), kind:"mcp",
                        source:source.display().to_string(), transport, blocked, conflict:Vec::new() });
                    if let Ok(definition) = sanitized {
                        imported.resource.push(ImportedResource::Mcp { id, name:name.clone(), source:source.display().to_string(), definition });
                    }
                }
            }
        }
        ensure!(!imported.candidate.is_empty(), "native source contains no importable resource entries");
        imported.fingerprint = hex::encode(digest.finalize());
        ensure!(serde_json::to_vec(&imported)?.len() <= 256 * 1024, "native import preview exceeds 256 KiB");
        Ok(imported)
    }

    /// Resolve an exact reviewed selection, rejecting blocked, duplicated, or changed source entries.
    pub(crate) fn select(&self, fingerprint: &str, selected: &[String]) -> Result<Vec<ImportedResource>> {
        ensure!(self.fingerprint == fingerprint, "native import source changed. Refresh its preview before applying");
        ensure!(!selected.is_empty(), "native import requires an explicit resource selection");
        let mut seen = BTreeSet::new();
        selected.iter().map(|id| {
            ensure!(seen.insert(id), "native import selection contains duplicate entries");
            let candidate = self.candidate.iter().find(|candidate| &candidate.id == id).context("native import entry no longer exists")?;
            ensure!(candidate.blocked.is_none(), "native import entry is blocked: {}", candidate.name);
            ensure!(candidate.conflict.is_empty(), "native import conflicts with selected definitions: {}", candidate.name);
            self.resource.iter().find(|resource| resource.id() == id).cloned().context("native import entry cannot be stored")
        }).collect()
    }
}

#[derive(Default)]
/// Resolves native launch references from one bounded file snapshot per server.
pub(super) struct NativeValues {
    source: BTreeMap<PathBuf, Value>,
}

impl NativeValues {
    /// Reject changed launch structure or newly added native policies before reading credentials.
    pub(super) fn validate(&mut self, name: &str, definition: &Value) -> Result<()> {
        let Some(reference) = definition.get("native_definition") else { return Ok(()); };
        let source = PathBuf::from(reference["source"].as_str().context("native MCP definition source missing")?);
        let pointer = reference["pointer"].as_str().context("native MCP definition pointer missing")?;
        let value = self.value(&source, pointer)?;
        let current = sanitize_server(name, value, &source, pointer)?;
        ensure!(&current == definition, "native MCP launch structure changed. Import its definition again");
        Ok(())
    }

    /// Read a retained native MCP field without persisting its value.
    pub(super) fn resolve(&mut self, reference: &Value) -> Result<String> {
        let source = PathBuf::from(reference["source"].as_str().context("native MCP value source missing")?);
        let pointer = reference["pointer"].as_str().context("native MCP value pointer missing")?;
        Ok(self.value(&source, pointer)?.as_str()
            .context("native MCP referenced value must be text")?.into())
    }

    fn value(&mut self, source: &Path, pointer: &str) -> Result<&Value> {
        if !self.source.contains_key(source) {
            let value = document(&source, &bounded_read(&source, SOURCE_LIMIT)?)?;
            self.source.insert(source.to_owned(), value);
        }
        self.source[source].pointer(pointer).context("native MCP referenced value no longer exists")
    }
}

fn identity(namespace: &str, source: &str, name: &str) -> String {
    let source = path_identity(source);
    format!("import:{}", hex::encode(Sha256::digest(format!("{namespace}\0{source}\0{name}").as_bytes())))
}

fn path_identity(source: &str) -> String {
    let source = source.replace('\\', "/");
    let source = source.trim_end_matches('/');
    if cfg!(windows) { source.to_lowercase() } else { source.to_owned() }
}

fn bounded_read(source: &Path, limit: u64) -> Result<Vec<u8>> {
    ensure!(source.metadata()?.len() <= limit, "native import source exceeds its size limit");
    let mut content = Vec::new();
    std::fs::File::open(source)?.take(limit + 1).read_to_end(&mut content)?;
    ensure!(content.len() as u64 <= limit, "native import source exceeds its size limit");
    Ok(content)
}

fn sanitize_server(name: &str, source: &Value, file: &Path, pointer: &str) -> Result<Value> {
    ensure!(!name.is_empty() && name.len() <= 128 && name.chars().all(|character| character.is_ascii_alphanumeric() || "-_.:".contains(character)), "MCP name is not portable");
    ensure!(name != "forge-harness-control", "MCP name is reserved by Forge");
    let fields = source.as_object().context("MCP definition must be an object")?;
    let unsupported = fields.keys().filter(|key| !matches!(key.as_str(),
        "type" | "command" | "args" | "url" | "env" | "headers" | "http_headers"
        | "envVars" | "env_vars" | "bearer_token_env_var" | "env_http_headers" | "tools" | "targets"))
        .cloned().collect::<Vec<_>>();
    ensure!(unsupported.is_empty(), "MCP options cannot transfer without changing behavior: {}", unsupported.join(", "));
    if let Some(tools) = source.get("tools") {
        ensure!(tools == &json!(["*"]), "MCP tool selection cannot transfer through ACP");
    }
    if let Some(targets) = source.get("targets") {
        ensure!(targets.as_array().is_some_and(|targets| targets.iter().all(Value::is_string)), "MCP targets must be text names");
    }
    let transport = match source["type"].as_str().unwrap_or(if source["url"].is_string() { "http" } else { "stdio" }) {
        "local" => "stdio", "remote" => "http", transport => transport,
    };
    ensure!(matches!(transport, "stdio" | "http" | "sse"), "MCP transport is not portable");
    let mut definition = json!({"type":transport,"native_definition":{"source":file,"pointer":pointer}});
    if transport == "stdio" {
        let command = source["command"].as_str().filter(|command| !command.is_empty()).context("stdio MCP command missing")?;
        definition["command"] = json!(command);
        let argument = source.get("args").cloned().unwrap_or(json!([]));
        ensure!(argument.as_array().is_some_and(|argument| argument.iter().all(Value::is_string)), "MCP arguments must be text");
        definition["args"] = json!([]);
        definition["native_args"] = json!(argument.as_array().unwrap().iter().enumerate().map(|(index, _)|
            json!({"source":file,"pointer":format!("{pointer}/args/{index}")})).collect::<Vec<_>>());
    } else {
        ensure!(source["url"].as_str().is_some_and(|url| !url.is_empty()), "HTTP MCP URL missing");
        definition["native_url"] = json!({"source":file,"pointer":format!("{pointer}/url")});
    }
    for key in ["env", "headers", "http_headers"] {
        if let Some(value) = source.get(key) {
            let values = value.as_object().context("MCP credential fields must be objects")?;
            ensure!(values.values().all(Value::is_string), "MCP environment and header values must be text");
            let references = values.keys().map(|name| (name.clone(), json!({"source":file,
                "pointer":format!("{pointer}/{key}/{}", pointer_key(name))}))).collect::<serde_json::Map<_,_>>();
            let field = if key == "env" { "native_env" } else { "native_headers" };
            let target = definition.as_object_mut().unwrap().entry(field).or_insert(json!({})).as_object_mut().unwrap();
            target.extend(references);
        }
    }
    let mut environment = BTreeSet::new();
    for key in ["envVars", "env_vars"] {
        if let Some(value) = source.get(key) {
            for name in value.as_array().context("MCP environment references must be a list")? {
                let name = name.as_str().filter(|name| variable_name(name)).context("MCP environment reference is invalid")?;
                environment.insert(name);
            }
        }
    }
    if !environment.is_empty() { definition["envVars"] = json!(environment); }
    if let Some(value) = source.get("bearer_token_env_var") {
        ensure!(value.as_str().is_some_and(variable_name), "MCP bearer token reference is invalid");
        definition["bearer_token_env_var"] = value.clone();
    }
    if let Some(value) = source.get("env_http_headers") {
        ensure!(value.as_object().is_some_and(|header| header.values().all(|value| value.as_str().is_some_and(variable_name))), "MCP header environment reference is invalid");
        definition["env_http_headers"] = value.clone();
    }
    if let Some(value) = source.get("targets") { definition["targets"] = value.clone(); }
    Ok(definition)
}

fn pointer_key(value: &str) -> String { value.replace('~', "~0").replace('/', "~1") }

fn document(source: &Path, content: &[u8]) -> Result<Value> {
    if source.extension().is_some_and(|extension| extension == "toml") {
        let document = toml::from_str::<toml::Value>(std::str::from_utf8(content)?)
            .map_err(|error| anyhow::anyhow!("native MCP TOML is invalid at byte range {:?}", error.span()))?;
        Ok(serde_json::to_value(document)?)
    } else { serde_json::from_slice(content).context("native MCP source must be JSON or TOML") }
}

fn variable_name(name: &str) -> bool {
    !name.is_empty() && name.chars().all(|character| character.is_ascii_alphanumeric() || character == '_')
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn import_retains_credential_references_and_detects_source_changes() -> Result<()> {
        let root = tempfile::tempdir()?;
        let source = root.path().join("settings.json");
        std::fs::write(&source, serde_json::to_vec(&json!({"mcpServers":{
            "safe":{"command":"node","env":{"TOKEN":"${TOKEN}"}},
            "secret":{"command":"node","env":{"TOKEN":"private-value"}},
            "invalid":{"command":"node","env":{"TOKEN":42}}
        },"apiKey":"private-authentication"}))?)?;
        let preview = NativeImport::read(ImportKind::Mcp, &source, "global", root.path())?;
        let serialized = serde_json::to_string(&preview)?;
        assert!(!serialized.contains("private-value") && !serialized.contains("private-authentication"));
        let safe = preview.candidate.iter().find(|candidate| candidate.name == "safe").unwrap();
        assert_eq!(preview.select(&preview.fingerprint, &[safe.id.clone()])?.len(), 1);
        let secret = preview.candidate.iter().find(|candidate| candidate.name == "secret").unwrap();
        let selected = preview.select(&preview.fingerprint, &[secret.id.clone()])?;
        assert!(!serde_json::to_string(&selected)?.contains("private-value"));
        assert_eq!(selected[0].server()?.unwrap().native()?["env"]["TOKEN"], "private-value");
        let invalid = preview.candidate.iter().find(|candidate| candidate.name == "invalid").unwrap();
        assert!(preview.select(&preview.fingerprint, &[invalid.id.clone()]).is_err());
        std::fs::write(&source, r#"{"mcpServers":{"safe":{"command":"changed"}}}"#)?;
        let changed = NativeImport::read(ImportKind::Mcp, &source, "global", root.path())?;
        assert!(changed.select(&preview.fingerprint, &[safe.id.clone()]).is_err());
        assert_eq!(changed.candidate[0].id, safe.id);
        Ok(())
    }

    #[test]
    fn imports_are_atomic_scoped_and_disabled_until_selected() -> Result<()> {
        use crate::settings::{SettingScope, SettingsStore, SessionResources};
        let root = tempfile::tempdir()?;
        let workspace = root.path().join("workspace");
        std::fs::create_dir(&workspace)?;
        let source = root.path().join("native.json");
        let original = json!({"mcpServers":{"portable":{"command":std::env::current_exe()?,"env":{"TOKEN":"native-token-fixture"}}},
            "apiKey":"provider-authentication-fixture"}).to_string();
        std::fs::write(&source, &original)?;
        let workspace = workspace.to_string_lossy().into_owned();
        let mut store = SettingsStore::open(root.path())?;
        let preview = store.preview_import(ImportKind::Mcp, &source, SettingScope::Workspace, &workspace, "session", "claude")?;
        let id = preview.candidate[0].id.clone();
        let selected = preview.select(&preview.fingerprint, &[id.clone()])?;
        let receipt = json!({"source":source,"fingerprint":preview.fingerprint});
        let mutation = store.import_operation(0, SettingScope::Workspace, &workspace, "session", &selected, "import-1", &receipt)?;
        assert_eq!(mutation.revision, 1);
        assert_eq!(store.import_operation(0, SettingScope::Workspace, &workspace, "session", &selected, "import-1", &receipt)?.revision, 1);
        assert!(store.import_operation(0, SettingScope::Workspace, &workspace, "session", &selected, "stale", &receipt).is_err());
        assert!(store.catalog("another-workspace", "session", "claude")?.server.is_empty());
        for backend in ["codex", "copilot", "claude", "antigravity"] {
            let snapshot = store.catalog(&workspace, "session", backend)?;
            assert_eq!(snapshot.server.len(), 1);
            assert_eq!(snapshot.server[0].scope, Some(crate::backend::ResourceScope::Workspace));
            assert_eq!(snapshot.settings.desired.mcp.get(&id), Some(&false));
            let request = crate::backend::BackendCatalogRequest { execution_owner: None, harness_session_id:"session".into(), workspace:workspace.clone(),
                access:Default::default(), execution_mode:crate::session::PermissionMode::Read, backend_session_id:None };
            assert!(SessionResources::load(root.path(), &request, backend, "test")?.launch.is_empty());
        }
        let mut desired = store.get(&workspace, "session", "claude")?.desired;
        desired.mcp.insert(id, true);
        store.update(1, SettingScope::Workspace, &workspace, "session", &desired)?;
        let request = crate::backend::BackendCatalogRequest { execution_owner: None, harness_session_id:"session".into(), workspace:workspace.clone(),
            access:Default::default(), execution_mode:crate::session::PermissionMode::Read, backend_session_id:None };
        let launched = SessionResources::load(root.path(), &request, "antigravity", "test")?;
        assert_eq!(launched.launch[0].1["env"]["TOKEN"], "native-token-fixture");
        assert_eq!(launched.mcp[0].scope, Some(crate::backend::ResourceScope::Workspace));
        assert_eq!(launched.mcp[0].source_path.as_deref(), Some(dunce::canonicalize(&source)?.to_string_lossy().as_ref()));
        let connection = rusqlite::Connection::open(root.path().join("settings.db"))?;
        let stored: String = connection.query_row("SELECT payload FROM setting_resource", [], |row| row.get(0))?;
        assert!(!stored.contains("native-token-fixture") && !stored.contains("provider-authentication-fixture"));
        assert_eq!(std::fs::read_to_string(source)?, original);
        Ok(())
    }

    #[test]
    fn native_skill_import_retains_source_and_scope_without_copying_prompt_body() -> Result<()> {
        use crate::settings::{SettingScope, SettingsStore};
        let root = tempfile::tempdir()?;
        let source = root.path().join("native-skill");
        std::fs::create_dir(&source)?;
        std::fs::write(source.join("SKILL.md"), "---\nname: imported-skill\ndescription: Native skill\n---\nPROMPT_BODY_FIXTURE")?;
        let mut store = SettingsStore::open(root.path())?;
        let preview = store.preview_import(ImportKind::Skills, &source, SettingScope::Global, "workspace", "session", "claude")?;
        let selected = preview.select(&preview.fingerprint, &[preview.candidate[0].id.clone()])?;
        store.import_operation(0, SettingScope::Global, "workspace", "session", &selected, "skill-import", &json!({"fingerprint":preview.fingerprint}))?;
        let snapshot = store.catalog("another-workspace", "another-session", "antigravity")?;
        assert_eq!(snapshot.skill[0].name, "imported-skill");
        assert_eq!(snapshot.skill[0].scope, Some(crate::backend::ResourceScope::Global));
        assert!(Path::new(&snapshot.skill[0].source).is_file());
        assert_eq!(snapshot.settings.desired.skill.get(&snapshot.skill[0].id), Some(&false));
        assert!(!serde_json::to_string(&selected)?.contains("PROMPT_BODY_FIXTURE"));
        let preview = store.preview_import(ImportKind::Skills, &source, SettingScope::Workspace, "workspace", "session", "claude")?;
        let selected = preview.select(&preview.fingerprint, &[preview.candidate[0].id.clone()])?;
        store.import_operation(1, SettingScope::Workspace, "workspace", "session", &selected, "workspace-skill-import", &json!({"fingerprint":preview.fingerprint}))?;
        let skill = store.skills("workspace")?;
        assert_eq!(skill.len(), 2);
        assert!(skill.iter().any(|skill| skill.scope == Some(crate::backend::ResourceScope::Workspace)));
        assert!(skill.iter().any(|skill| skill.scope == Some(crate::backend::ResourceScope::Global)));
        Ok(())
    }

    #[test]
    fn native_policy_and_launch_changes_cannot_silently_widen_an_import() -> Result<()> {
        let root = tempfile::tempdir()?;
        let source = root.path().join("native.json");
        let write = |server: Value| -> Result<()> {
            std::fs::write(&source, json!({"mcpServers":{"fixture":server}}).to_string())?;
            Ok(())
        };
        write(json!({"command":"node","args":["fixture.js"],"env":{"TOKEN":"literal$token"},"tools":["*"]}))?;
        let preview = NativeImport::read(ImportKind::Mcp, &source, "global", root.path())?;
        let selected = preview.select(&preview.fingerprint, &[preview.candidate[0].id.clone()])?;
        let server = selected[0].server()?.unwrap();
        assert_eq!(server.native()?["env"]["TOKEN"], "literal$token");
        write(json!({"command":"node","args":["fixture.js"],"env":{"TOKEN":"rotated$token"},"tools":["*"]}))?;
        assert_eq!(server.native()?["env"]["TOKEN"], "rotated$token");
        for key in ["enabled_tools", "disabled_tools", "cwd", "timeout", "auth", "disabled"] {
            let mut definition = json!({"command":"node"});
            definition[key] = json!("PRIVATE_POLICY_VALUE");
            write(definition)?;
            let blocked = NativeImport::read(ImportKind::Mcp, &source, "global", root.path())?;
            let reason = blocked.candidate[0].blocked.as_ref().unwrap();
            assert!(reason.contains(key) && !reason.contains("PRIVATE_POLICY_VALUE"));
            assert!(server.native().is_err());
        }
        write(json!({"command":"node","tools":["only-this-tool"]}))?;
        assert!(NativeImport::read(ImportKind::Mcp, &source, "global", root.path())?.candidate[0].blocked.is_some());
        write(json!({"command":"another-program","args":["fixture.js"],"env":{"TOKEN":"rotated$token"}}))?;
        assert!(server.native().is_err());
        let workspace = root.path().to_string_lossy().into_owned();
        let project_path = if cfg!(windows) { workspace.to_uppercase().replace('\\', "/") } else { workspace.clone() };
        std::fs::write(&source, json!({"projects":{project_path:{"mcpServers":{"fixture":{"command":"project-command"}}}}}).to_string())?;
        let project = NativeImport::read(ImportKind::Mcp, &source, "global", root.path())?;
        assert_eq!(project.select(&project.fingerprint, &[project.candidate[0].id.clone()])?[0].server()?.unwrap().native()?["command"], "project-command");
        Ok(())
    }
}
