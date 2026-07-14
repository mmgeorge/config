use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use uuid::Uuid;

/// Defines how one backend exposes and controls child agents.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentControlMode {
    #[default]
    Unsupported,
    ParentMediated,
    Direct,
}

/// Represents the agent operations available through one backend.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AgentCapability {
    pub observe: bool,
    pub catalog: bool,
    pub spawn: AgentControlMode,
    pub input: AgentControlMode,
    pub interrupt: AgentControlMode,
    pub parallel: bool,
}

impl AgentCapability {
    /// Build the native Codex child-agent contract exposed through app-server.
    pub fn codex() -> Self {
        Self {
            observe: true,
            catalog: true,
            spawn: AgentControlMode::ParentMediated,
            input: AgentControlMode::Direct,
            interrupt: AgentControlMode::Direct,
            parallel: true,
        }
    }
}

/// Identifies where one selectable agent definition originated.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentDefinitionSource {
    BuiltIn,
    Personal,
    Project,
}

/// Represents one provider agent definition selectable from Neovim.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct AgentDefinition {
    pub name: String,
    pub description: String,
    pub source: AgentDefinitionSource,
    pub path: Option<String>,
}

/// Resolve a catalog name into the identifier accepted by Codex spawn tools.
pub fn codex_agent_type(name: &str) -> String {
    name.chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || character == '_' {
                character
            } else {
                '_'
            }
        })
        .collect()
}

/// Defines the lifecycle of one concrete child-agent run.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentRunStatus {
    Starting,
    Running,
    Waiting,
    Completed,
    Failed,
    Interrupted,
    Closed,
}

impl AgentRunStatus {
    /// Return whether the run can still accept input or interruption.
    pub fn is_active(self) -> bool {
        matches!(self, Self::Starting | Self::Running | Self::Waiting)
    }

    /// Return whether a provider-backed child can start another user turn.
    pub fn accepts_prompt(self) -> bool {
        matches!(self, Self::Running | Self::Waiting | Self::Completed)
    }
}

/// Stores one provider-backed child-agent identity and lifecycle.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentRun {
    pub id: String,
    pub session_id: String,
    #[serde(default)]
    pub parent_interaction_id: Option<String>,
    pub parent_thread_id: Option<String>,
    pub provider_thread_id: Option<String>,
    pub active_turn_id: Option<String>,
    pub definition: String,
    pub nickname: Option<String>,
    pub task: String,
    pub status: AgentRunStatus,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

impl AgentRun {
    /// Create one pending run before the provider reports its child thread.
    pub fn pending(session_id: &str, definition: &str, task: &str, now_ms: i64) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            session_id: session_id.into(),
            parent_interaction_id: None,
            parent_thread_id: None,
            provider_thread_id: None,
            active_turn_id: None,
            definition: definition.into(),
            nickname: None,
            task: task.into(),
            status: AgentRunStatus::Starting,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        }
    }

    /// Return the best stable label for winbar and picker presentation.
    pub fn label(&self) -> &str {
        self.nickname.as_deref().unwrap_or(&self.definition)
    }
}

/// Stores one child-agent turn using the shared Harness interaction shape.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentTurnRecord {
    pub id: String,
    pub session_id: String,
    pub agent_run_id: String,
    pub ordinal: u64,
    pub interaction: crate::interaction::InteractionRecord,
}

/// Represents one provider child-agent lifecycle update.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentLifecycleEvent {
    pub operation: String,
    #[serde(default)]
    pub starts_child: bool,
    pub parent_thread_id: Option<String>,
    pub provider_thread_id: Option<String>,
    pub turn_id: Option<String>,
    pub definition: Option<String>,
    pub nickname: Option<String>,
    pub task: Option<String>,
    pub status: AgentRunStatus,
}

/// Owns active child identities and provider-thread lookup for one session.
#[derive(Clone, Debug, Default)]
pub struct AgentRegistry {
    run_by_id: HashMap<String, AgentRun>,
    run_id_by_thread: HashMap<String, String>,
}

impl AgentRegistry {
    /// Build the registry from durable runs while restoring provider-thread indexes.
    pub fn from_run_list(run_list: Vec<AgentRun>) -> Self {
        let mut registry = Self::default();
        for run in run_list {
            registry.insert(run);
        }
        registry
    }

    /// Insert or replace one run and synchronize its provider-thread lookup.
    pub fn insert(&mut self, run: AgentRun) {
        self.run_id_by_thread
            .retain(|_, indexed_run_id| indexed_run_id != &run.id);
        if let Some(thread_id) = run.provider_thread_id.as_ref() {
            self.run_id_by_thread
                .insert(thread_id.clone(), run.id.clone());
        }
        self.run_by_id.insert(run.id.clone(), run);
    }

    /// Resolve a run by its Harness identifier.
    pub fn get(&self, run_id: &str) -> Option<&AgentRun> {
        self.run_by_id.get(run_id)
    }

    /// Resolve a mutable run by its Harness identifier.
    pub fn get_mut(&mut self, run_id: &str) -> Option<&mut AgentRun> {
        self.run_by_id.get_mut(run_id)
    }

    /// Resolve a run through one provider child-thread identifier.
    pub fn get_by_thread(&self, thread_id: &str) -> Option<&AgentRun> {
        self.run_id_by_thread
            .get(thread_id)
            .and_then(|run_id| self.run_by_id.get(run_id))
    }

    /// Resolve one active run that has not received its provider child-thread identifier.
    pub fn resolve_unbound(
        &self,
        parent_interaction_id: Option<&str>,
        parent_thread_id: Option<&str>,
        turn_id: Option<&str>,
    ) -> Option<&AgentRun> {
        let mut candidate = self.run_by_id.values().filter(|run| {
            run.provider_thread_id.is_none()
                && run.status.is_active()
                && parent_interaction_id.is_none_or(|interaction_id| {
                    run.parent_interaction_id.as_deref() == Some(interaction_id)
                })
                && parent_thread_id.is_none_or(|thread_id| {
                    run.parent_thread_id
                        .as_deref()
                        .is_none_or(|known_thread_id| known_thread_id == thread_id)
                })
                && turn_id.is_none_or(|active_turn_id| {
                    run.active_turn_id
                        .as_deref()
                        .is_none_or(|known_turn_id| known_turn_id == active_turn_id)
                })
        });
        let run = candidate.next()?;
        candidate.next().is_none().then_some(run)
    }

    /// Return runs in stable creation order for snapshot presentation.
    pub fn list(&self) -> Vec<AgentRun> {
        let mut run_list = self.run_by_id.values().cloned().collect::<Vec<_>>();
        run_list.sort_by_key(|run| (run.created_at_ms, run.id.clone()));
        run_list
    }
}

#[derive(Deserialize)]
struct CodexAgentConfig {
    description: Option<String>,
}

/// Load Codex agent definitions using project-over-personal precedence.
pub fn load_codex_agent_catalog(workspace: &Path) -> Result<Vec<AgentDefinition>> {
    let personal_directory = std::env::var_os("CODEX_HOME")
        .map(PathBuf::from)
        .or_else(|| {
            std::env::var_os("USERPROFILE")
                .or_else(|| std::env::var_os("HOME"))
                .map(|home| Path::new(&home).join(".codex"))
        })
        .map(|codex_home| codex_home.join("agents"));
    load_codex_agent_catalog_from_directory(workspace, personal_directory.as_deref())
}

fn load_codex_agent_catalog_from_directory(
    workspace: &Path,
    personal_directory: Option<&Path>,
) -> Result<Vec<AgentDefinition>> {
    let mut definition_by_name = built_in_definition_list()
        .into_iter()
        .map(|definition| (definition.name.clone(), definition))
        .collect::<HashMap<_, _>>();
    if let Some(directory) = personal_directory {
        load_definition_directory(
            directory.to_owned(),
            AgentDefinitionSource::Personal,
            &mut definition_by_name,
        )?;
    }
    load_definition_directory(
        workspace.join(".codex").join("agents"),
        AgentDefinitionSource::Project,
        &mut definition_by_name,
    )?;
    let mut definition_list = definition_by_name.into_values().collect::<Vec<_>>();
    definition_list.sort_by(|left, right| left.name.cmp(&right.name));
    Ok(definition_list)
}

fn built_in_definition_list() -> Vec<AgentDefinition> {
    [
        ("default", "General-purpose Codex agent"),
        ("worker", "Implementation-focused Codex agent"),
        ("explorer", "Read-oriented repository explorer"),
    ]
    .into_iter()
    .map(|(name, description)| AgentDefinition {
        name: name.into(),
        description: description.into(),
        source: AgentDefinitionSource::BuiltIn,
        path: None,
    })
    .collect()
}

fn load_definition_directory(
    directory: PathBuf,
    source: AgentDefinitionSource,
    definition_by_name: &mut HashMap<String, AgentDefinition>,
) -> Result<()> {
    if !directory.is_dir() {
        return Ok(());
    }
    for entry in fs::read_dir(&directory)
        .with_context(|| format!("read Codex agent directory {}", directory.display()))?
    {
        let path = entry?.path();
        if path.extension().and_then(|value| value.to_str()) != Some("toml") {
            continue;
        }
        let name = path
            .file_stem()
            .and_then(|value| value.to_str())
            .context("Codex agent filename must contain UTF-8")?
            .to_owned();
        let config: CodexAgentConfig = toml::from_str(&fs::read_to_string(&path)?)
            .with_context(|| format!("parse Codex agent definition {}", path.display()))?;
        definition_by_name.insert(
            name.clone(),
            AgentDefinition {
                name,
                description: config.description.unwrap_or_default(),
                source: source.clone(),
                path: Some(path.to_string_lossy().into_owned()),
            },
        );
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn converts_catalog_names_to_codex_spawn_identifiers() {
        assert_eq!(
            codex_agent_type("local-code-explorer"),
            "local_code_explorer"
        );
        assert_eq!(codex_agent_type("explorer"), "explorer");
    }

    #[test]
    fn registry_supports_multiple_instances_of_one_definition() {
        let first = AgentRun::pending("session", "explorer", "inspect Bevy", 1);
        let second = AgentRun::pending("session", "explorer", "inspect physics", 2);
        let registry = AgentRegistry::from_run_list(vec![first, second]);
        assert_eq!(registry.list().len(), 2);
    }

    #[test]
    fn registry_replaces_a_run_provider_thread_without_leaving_a_stale_index() {
        let mut run = AgentRun::pending("session", "explorer", "inspect Bevy", 1);
        run.provider_thread_id = Some("child-old".into());
        let mut registry = AgentRegistry::from_run_list(vec![run.clone()]);
        run.provider_thread_id = Some("child-new".into());
        registry.insert(run);
        assert!(registry.get_by_thread("child-old").is_none());
        assert_eq!(
            registry.get_by_thread("child-new").unwrap().definition,
            "explorer"
        );
    }

    #[test]
    fn registry_resolves_one_unbound_run_through_its_parent_identity() {
        let mut matching = AgentRun::pending("session", "explorer", "inspect Bevy", 1);
        matching.parent_interaction_id = Some("parent-one".into());
        matching.parent_thread_id = Some("thread-one".into());
        matching.active_turn_id = Some("turn-one".into());
        matching.status = AgentRunStatus::Running;
        let mut other = AgentRun::pending("session", "explorer", "inspect physics", 2);
        other.parent_interaction_id = Some("parent-two".into());
        let registry = AgentRegistry::from_run_list(vec![matching.clone(), other]);

        assert_eq!(
            registry
                .resolve_unbound(Some("parent-one"), Some("thread-one"), Some("turn-one"))
                .map(|run| run.id.as_str()),
            Some(matching.id.as_str())
        );
    }

    #[test]
    fn registry_refuses_to_guess_between_unbound_sibling_runs() {
        let mut first = AgentRun::pending("session", "explorer", "inspect Bevy", 1);
        first.parent_interaction_id = Some("parent".into());
        let mut second = AgentRun::pending("session", "explorer", "inspect physics", 2);
        second.parent_interaction_id = Some("parent".into());
        let registry = AgentRegistry::from_run_list(vec![first, second]);

        assert!(
            registry
                .resolve_unbound(Some("parent"), None, None)
                .is_none()
        );
    }

    #[test]
    fn project_definition_overrides_the_builtin_definition() {
        let workspace = tempfile::tempdir().unwrap();
        let directory = workspace.path().join(".codex").join("agents");
        fs::create_dir_all(&directory).unwrap();
        fs::write(
            directory.join("explorer.toml"),
            "description = \"Project explorer\"\n",
        )
        .unwrap();
        let catalog = load_codex_agent_catalog(workspace.path()).unwrap();
        let explorer = catalog
            .iter()
            .find(|definition| definition.name == "explorer")
            .unwrap();
        assert_eq!(explorer.description, "Project explorer");
        assert_eq!(explorer.source, AgentDefinitionSource::Project);
    }

    #[test]
    fn project_definition_overrides_a_codex_home_definition() {
        let workspace = tempfile::tempdir().unwrap();
        let personal = tempfile::tempdir().unwrap();
        fs::write(
            personal.path().join("explorer.toml"),
            "description = \"Personal explorer\"\n",
        )
        .unwrap();
        let project_directory = workspace.path().join(".codex").join("agents");
        fs::create_dir_all(&project_directory).unwrap();
        fs::write(
            project_directory.join("explorer.toml"),
            "description = \"Project explorer\"\n",
        )
        .unwrap();
        let catalog =
            load_codex_agent_catalog_from_directory(workspace.path(), Some(personal.path()))
                .unwrap();
        let explorer = catalog
            .iter()
            .find(|definition| definition.name == "explorer")
            .unwrap();
        assert_eq!(explorer.description, "Project explorer");
        assert_eq!(explorer.source, AgentDefinitionSource::Project);
    }
}
