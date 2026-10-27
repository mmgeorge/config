use anyhow::{Context, Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Component, Path, PathBuf},
};

#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Classifies check purpose independently of its runner or execution dependencies.
pub enum CheckCategory {
    Format,
    Lint,
    Check,
    Test,
    Build,
}

impl CheckCategory {
    pub(crate) fn priority(self) -> u8 {
        match self {
            Self::Format => 0,
            Self::Lint | Self::Check => 1,
            Self::Test => 2,
            Self::Build => 3,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Determines whether successful execution certifies stable inputs or prepares new inputs.
pub enum CheckPurpose {
    #[default]
    /// Certifies unchanged inputs after successful execution.
    Verify,
    /// Permits source edits and records the resulting inputs after successful execution.
    Prepare,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects project-wide or task-derived coverage without changing command text.
pub enum CheckScope {
    #[default]
    All,
    ChangedFiles,
    ChangedCases,
    /// Runs only tests marked new in the accepted plan.
    NewCases,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Supplies shell source or a direct executable with resolved argument bindings.
pub struct CheckOperation {
    #[serde(default)]
    pub input: bool,
    /// Shell source when args is absent, otherwise an executable name or path.
    pub command: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Direct arguments. Bound values remain individual arguments, never shell source.
    pub args: Option<Vec<String>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Named arguments declared by this check, with no runner-specific semantics.
    pub inputs: Option<super::CheckInputMap>,
}

impl CheckOperation {
    /// Renders the complete configured invocation for review and permission matching.
    pub fn display(&self) -> String {
        self.args.as_ref().map_or_else(
            || self.command.clone(),
            |args| super::invocation::display(&self.command, args),
        )
    }
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Defines one named suite and the selection contracts implemented by its commands.
pub struct CheckDefinition {
    pub id: String,
    pub name: String,
    pub category: CheckCategory,
    #[serde(default)]
    /// Source mutation contract, defaulting to read-only verification.
    pub purpose: CheckPurpose,
    pub enabled: bool,
    #[serde(default = "automatic_default")]
    /// Selects this check automatically when its trigger matches.
    pub auto: bool,
    /// Functional area shown in check selection and review surfaces.
    pub area: String,
    /// Changes that automatically select this check, independent of its scope.
    pub trigger: super::CheckTrigger,
    #[serde(default)]
    pub scope: CheckScope,
    #[serde(default = "root_directory")]
    pub directory: String,
    pub all: CheckOperation,
    #[serde(default)]
    pub files: Option<CheckOperation>,
    #[serde(default)]
    pub cases: Option<CheckOperation>,
    #[serde(default)]
    pub depends_on: Vec<String>,
}

fn root_directory() -> String {
    ".".into()
}

fn automatic_default() -> bool {
    true
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(
    try_from = "BTreeMap<String, super::spec::Check>",
    into = "BTreeMap<String, super::spec::Check>"
)]
/// Owns explicit project configuration, including an intentionally empty definition list.
pub struct CheckConfig {
    pub version: u32,
    pub definitions: Vec<CheckDefinition>,
}

impl CheckConfig {
    /// Rejects explicit plan selections that cannot execute from this configuration.
    pub(crate) fn validate_selection(&self, ids: &[String]) -> Result<()> {
        for id in ids {
            let definition = self
                .definitions
                .iter()
                .find(|definition| &definition.id == id)
                .with_context(|| format!("unknown planned check: {id}"))?;
            ensure!(definition.enabled, "planned check is disabled: {id}");
        }
        Ok(())
    }

    /// Rejects unsupported scopes, duplicate identities, and missing or cyclic prerequisites.
    pub fn validate(&self) -> Result<()> {
        ensure!(
            self.version == 2,
            "unsupported checks configuration version"
        );
        let mut identities = BTreeSet::new();
        for definition in &self.definitions {
            ensure!(
                !definition.area.trim().is_empty() && !definition.area.contains(['\0', '\n', '\r']),
                "invalid check area"
            );
            definition.trigger.validate()?;
            ensure!(
                !definition.id.is_empty()
                    && definition
                        .id
                        .bytes()
                        .all(|byte| byte.is_ascii_alphanumeric()
                            || matches!(byte, b'-' | b'_' | b'.')),
                "invalid check id: {}",
                definition.id
            );
            ensure!(
                identities.insert(&definition.id),
                "duplicate check id: {}",
                definition.id
            );
            ensure!(!definition.name.trim().is_empty(), "invalid check name");
            relative_path(&definition.directory)?;
            for operation in std::iter::once(&definition.all)
                .chain(definition.files.iter())
                .chain(definition.cases.iter())
            {
                ensure!(
                    !operation.command.trim().is_empty()
                        && !operation.command.contains(['\0', '\r', '\n']),
                    "invalid command for {}",
                    definition.id
                );
                super::invocation::validate(operation)?;
            }
            match definition.scope {
                CheckScope::ChangedFiles => ensure!(
                    definition.files.is_some(),
                    "{} does not implement file selection",
                    definition.id
                ),
                CheckScope::ChangedCases | CheckScope::NewCases => ensure!(
                    definition.cases.is_some(),
                    "{} does not implement case selection",
                    definition.id
                ),
                CheckScope::All => {}
            }
        }
        self.order()?;
        Ok(())
    }

    /// Orders the catalog by preparation, category priority, and explicit prerequisites.
    ///
    /// Configured order breaks priority ties. Missing or cyclic prerequisites are errors.
    pub fn order(&self) -> Result<Vec<usize>> {
        self.order_selected(
            &self
                .definitions
                .iter()
                .map(|definition| definition.id.clone())
                .collect(),
        )
    }

    /// Orders selected preparation and its prerequisites before unrelated verification.
    ///
    /// Includes unselected catalog entries without promoting their preparation prerequisites.
    /// Explicit dependencies precede category priority. Missing or cyclic prerequisites are errors.
    pub(crate) fn order_selected(&self, selected: &BTreeSet<String>) -> Result<Vec<usize>> {
        // Promote preparation prerequisites so unrelated verification cannot precede source edits.
        let mut preparation: BTreeSet<_> = self
            .definitions
            .iter()
            .filter(|definition| {
                selected.contains(&definition.id) && definition.purpose == CheckPurpose::Prepare
            })
            .map(|definition| definition.id.as_str())
            .collect();
        let mut pending: Vec<_> = preparation.iter().copied().collect();
        while let Some(id) = pending.pop() {
            let definition = self
                .definitions
                .iter()
                .find(|definition| definition.id == id)
                .with_context(|| format!("missing check prerequisite: {id}"))?;
            for dependency in &definition.depends_on {
                if preparation.insert(dependency.as_str()) {
                    pending.push(dependency.as_str());
                }
            }
        }
        let mut ordered = Vec::new();
        let mut completed = BTreeSet::new();
        // Apply category priority only after each check's prerequisites have been ordered.
        while ordered.len() < self.definitions.len() {
            let next = self
                .definitions
                .iter()
                .enumerate()
                .filter(|(_, definition)| {
                    !completed.contains(definition.id.as_str())
                        && definition
                            .depends_on
                            .iter()
                            .all(|id| completed.contains(id.as_str()))
                })
                .min_by_key(|(index, definition)| {
                    let category = definition.category.priority();
                    (
                        !preparation.contains(definition.id.as_str()),
                        category,
                        *index,
                    )
                });
            let Some((index, definition)) = next else {
                anyhow::bail!("checks has missing or cyclic prerequisites");
            };
            completed.insert(definition.id.as_str());
            ordered.push(index);
        }
        Ok(ordered)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
/// Captures exact configuration and managed helper identities used by a check run.
pub struct ConfigSnapshot {
    pub digest: String,
    pub config: CheckConfig,
    pub helper_digest: BTreeMap<String, String>,
}

impl ConfigSnapshot {
    /// Reads saved configuration without inventing defaults or requiring a Git repository.
    pub fn load(workspace: &Path) -> Result<Self> {
        let document = read_document(workspace)?;
        let checks = document
            .get("checks")
            .context("checks_not_configured: enable /checks setup on and request configuration in chat, then retry submission")?;
        let config: CheckConfig =
            serde_json::from_value(checks.clone()).context("invalid checks configuration")?;
        config.validate()?;
        let mut helper_digest = BTreeMap::new();
        collect_helpers(
            workspace,
            &workspace.join(".forge/checks"),
            &mut helper_digest,
        )?;
        let digest = crate::plan::digest(&serde_json::to_vec(&(&config, &helper_digest))?);
        Ok(Self {
            digest,
            config,
            helper_digest,
        })
    }
}

/// Reads the shared repository settings object while preserving unrelated fields.
pub(crate) fn read_document(workspace: &Path) -> Result<serde_json::Value> {
    let path = workspace.join(".forge.json");
    reject_links(workspace, &path)?;
    let source = match std::fs::read(&path) {
        Ok(source) => source,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
            return Ok(serde_json::json!({}));
        }
        Err(error) => return Err(error).context("read .forge.json"),
    };
    let document: serde_json::Value =
        serde_json::from_slice(&source).context("invalid .forge.json")?;
    ensure!(document.is_object(), ".forge.json must contain an object");
    Ok(document)
}

/// Rejects absolute and parent-relative paths before joining a project directory.
pub(crate) fn relative_path(path: &str) -> Result<PathBuf> {
    ensure!(
        !path.is_empty() && !path.contains(['\0', '\\', ':']),
        "path must use project-relative forward slashes"
    );
    let path = PathBuf::from(path);
    ensure!(
        !path.is_absolute()
            && path
                .components()
                .all(|part| matches!(part, Component::Normal(_) | Component::CurDir)),
        "path must stay within the project"
    );
    Ok(path)
}

fn collect_helpers(
    workspace: &Path,
    directory: &Path,
    result: &mut BTreeMap<String, String>,
) -> Result<()> {
    reject_links(workspace, directory)?;
    if !directory.exists() {
        return Ok(());
    }
    ensure!(
        !std::fs::symlink_metadata(directory)?
            .file_type()
            .is_symlink(),
        "checks helper directory cannot be a symlink"
    );
    for entry in std::fs::read_dir(directory)? {
        let entry = entry?;
        let kind = entry.file_type()?;
        ensure!(!kind.is_symlink(), "checks helpers cannot be symlinks");
        if kind.is_dir() {
            collect_helpers(workspace, &entry.path(), result)?;
        } else if kind.is_file() {
            let source = std::fs::read(entry.path())?;
            result.insert(
                entry
                    .path()
                    .strip_prefix(workspace)?
                    .to_string_lossy()
                    .replace('\\', "/"),
                crate::plan::digest(&source),
            );
        }
    }
    Ok(())
}

/// Rejects linked components before reading or publishing managed project configuration.
pub(crate) fn reject_links(workspace: &Path, destination: &Path) -> Result<()> {
    let relative = destination
        .strip_prefix(workspace)
        .context("configuration path escapes the project")?;
    let mut current = workspace.to_path_buf();
    for component in relative.components() {
        current.push(component);
        match std::fs::symlink_metadata(&current) {
            Ok(metadata) => ensure!(
                !metadata.file_type().is_symlink(),
                "managed checks paths cannot contain symlinks"
            ),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => break,
            Err(error) => return Err(error.into()),
        }
    }
    Ok(())
}

#[cfg(test)]
/// Configures an explicit empty gate for fixtures whose subject is unrelated to command selection.
pub(crate) fn configure_fixture(workspace: &Path) {
    let mut document = read_document(workspace).unwrap();
    if document.get("checks").is_none() {
        document["checks"] = serde_json::json!({});
        std::fs::write(
            workspace.join(".forge.json"),
            serde_json::to_vec(&document).unwrap(),
        )
        .unwrap();
    }
}
