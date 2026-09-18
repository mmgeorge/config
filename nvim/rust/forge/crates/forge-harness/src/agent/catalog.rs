use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

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
