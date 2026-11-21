use forge_provider_api::catalog::SkillDefinition;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation, SanitizedMessage,
    SessionHistory,
};
use std::path::{Path, PathBuf};

use super::AcpKind;
use crate::profile::{ProfileError, ResourceProfile, selected_skill_paths};

const CLI_DATA_DIRECTORY: &str = "forge-cli-settings";
const NATIVE_SETTINGS_PATH: &str = "antigravity-cli/settings.json";

/// Project settings that can authenticate or restrict the native worker.
#[derive(Default, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "camelCase")]
struct NativeSettings {
    #[serde(skip_serializing_if = "Option::is_none")]
    gcp: Option<CloudSettings>,
    #[serde(skip_serializing_if = "Option::is_none")]
    model_provider: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    permissions: Option<RestrictivePermissions>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tool_permission: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    enable_terminal_sandbox: Option<bool>,
}

/// Pass the project and location while leaving credential resolution to the CLI.
#[derive(serde::Deserialize, serde::Serialize)]
struct CloudSettings {
    #[serde(skip_serializing_if = "Option::is_none")]
    project: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<String>,
}

/// Retain restrictions without giving native allow rules authority to skip Forge.
#[derive(serde::Deserialize, serde::Serialize)]
struct RestrictivePermissions {
    #[serde(skip_serializing_if = "Option::is_none")]
    ask: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    deny: Option<Vec<String>>,
}

/// Prepare an ACP profile with selected skills and storage for the requested history lifetime.
pub(crate) fn create(
    kind: AcpKind,
    root: &Path,
    skills: &[SkillDefinition],
    history: &SessionHistory,
) -> Result<ResourceProfile, ProviderError> {
    let profile = match kind {
        AcpKind::Antigravity => antigravity(root, skills, history),
    };
    let profile =
        profile.map_err(|error| error.into_provider_error(ProviderOperation::ThreadOpen))?;
    write_native_settings(
        profile.path(),
        native_settings_source().as_deref(),
        ProviderOperation::ThreadOpen,
    )?;
    Ok(profile)
}

/// Get the CLI data path relative to Antigravity's application-data directory.
pub(super) fn cli_data_argument() -> PathBuf {
    Path::new("..").join(CLI_DATA_DIRECTORY)
}

/// Derive the native agent name from this private profile's generated directory name.
pub(super) fn agent_name(profile: &Path) -> String {
    let directory_name = profile
        .file_name()
        .and_then(std::ffi::OsStr::to_str)
        .expect("resource profile directories have generated ASCII names");
    format!("forge-harness-{}", hex::encode(directory_name))
}

/// Open a disposable catalog profile with its own native storage.
pub(super) fn discovery(root: &Path) -> Result<ResourceProfile, ProviderError> {
    let profile = antigravity(root, &[], &SessionHistory::Temporary)
        .map_err(|error| error.into_provider_error(ProviderOperation::ModelList))?;
    write_native_settings(
        profile.path(),
        native_settings_source().as_deref(),
        ProviderOperation::ModelList,
    )?;
    Ok(profile)
}

/// Locate the settings file the native CLI uses before Forge overrides its Gemini directory.
fn native_settings_source() -> Option<PathBuf> {
    let native = std::env::var_os("ANTIGRAVITY_GEMINI_DIR").filter(|value| !value.is_empty());
    let root = match native.as_deref().map(Path::new) {
        Some(path) if path.is_absolute() => path.to_owned(),
        relative => std::env::home_dir()?.join(relative.unwrap_or_else(|| Path::new(".gemini"))),
    };
    Some(root.join(NATIVE_SETTINGS_PATH))
}

/// Write the permitted settings projection to the private CLI data directory.
fn write_native_settings(
    profile: &Path,
    source: Option<&Path>,
    operation: ProviderOperation,
) -> Result<(), ProviderError> {
    let Some(source) = source else {
        return Ok(());
    };
    let settings = match std::fs::read(source) {
        Ok(bytes) => bytes,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => {
            let cause = ProfileError::Io(error).into_provider_error(operation);
            return Err(settings_error(
                cause,
                "Could not read Antigravity CLI settings.json".into(),
            ));
        }
    };
    let mut settings = serde_json::from_slice::<NativeSettings>(&settings).map_err(|error| {
        settings_error(
            ProviderFailure::new(operation, ProviderFailureKind::Rejected).into(),
            format!(
                "Antigravity CLI settings.json has invalid content at line {}, column {}",
                error.line(),
                error.column()
            ),
        )
    })?;
    settings.tool_permission = settings.tool_permission.filter(|mode| mode == "strict");
    settings.enable_terminal_sandbox = settings.enable_terminal_sandbox.filter(|enabled| *enabled);
    let directory = profile.join(CLI_DATA_DIRECTORY);
    std::fs::create_dir_all(&directory)
        .map_err(|error| ProfileError::Io(error).into_provider_error(operation))?;
    let file = std::fs::File::create(directory.join("settings.json"))
        .map_err(|error| ProfileError::Io(error).into_provider_error(operation))?;
    serde_json::to_writer(file, &settings).map_err(|error| {
        settings_error(
            ProviderFailure::new(operation, ProviderFailureKind::Runtime).into(),
            format!(
                "Could not write private Antigravity CLI settings.json: {:?}",
                error.classify()
            ),
        )
    })
}

/// Render a settings failure without exposing file content or authentication values.
fn settings_error(cause: ProviderError, explanation: String) -> ProviderError {
    match SanitizedMessage::new(explanation) {
        Ok(explanation) => ProviderError::Explained {
            cause: Box::new(cause),
            explanation,
        },
        Err(_) => cause,
    }
}

/// Keep temporary history in its private profile and link durable history to the state version.
fn antigravity(
    root: &Path,
    skills: &[SkillDefinition],
    history: &SessionHistory,
) -> Result<ResourceProfile, ProfileError> {
    let mut profile = ResourceProfile::create(root, "antigravity")?;
    match history {
        SessionHistory::Durable { .. } => {
            let directory = root.join("provider-runtime/antigravity/cli");
            std::fs::create_dir_all(&directory)?;
            profile.bind(&directory, "antigravity-cli")?;
        }
        SessionHistory::Temporary => {}
    }
    write_agent_definition(profile.path(), skills)?;
    Ok(profile)
}

/// Configure the main agent and give admitted skill entrypoints native discovery precedence.
fn write_agent_definition(profile: &Path, skills: &[SkillDefinition]) -> Result<(), ProfileError> {
    let config = profile.join("config");
    // Keep native migration from moving legacy skill files out of shared runtime storage.
    std::fs::create_dir_all(config.join("skills"))?;
    let skill_directory = write_skill_entrypoints(profile, skills)?;
    let agent_name = agent_name(profile);
    let agents = config.join("agents");
    std::fs::create_dir_all(&agents)?;
    let excluded_skill = skills
        .iter()
        .map(|skill| skill.name.as_str())
        .collect::<std::collections::BTreeSet<_>>();
    if !excluded_skill.is_empty() {
        let exclusions = serde_json::json!({"exclude": excluded_skill});
        std::fs::write(config.join("skills.json"), exclusions.to_string())?;
    }
    std::fs::write(
        agents.join(format!("{agent_name}.md")),
        format!(
            "---\nname: {agent_name}\ndescription: Forge Harness main agent.\nmainAgent: true\nsubagent: false\ninheritMcp: false\ninheritCustomizations: false\ntools: [read_file, write_file, edit_file, run_command, manage_task, search_web, ask_question, define_subagent, invoke_subagent, manage_subagents, send_message]\n---\n{skill_directory}"
        ),
    )?;
    Ok(())
}

/// Stage enabled skill entrypoints in the shared-skill scope, which precedes workspace discovery.
fn write_skill_entrypoints(
    profile: &Path,
    skills: &[SkillDefinition],
) -> Result<String, ProfileError> {
    use std::fmt::Write as _;

    let skill_root = profile.join("skills");
    std::fs::create_dir_all(&skill_root)?;
    let mut locations = String::new();
    for selection in selected_skill_paths(skills) {
        let (name, source) = selection?;
        let source = dunce::canonicalize(source)?;
        if !std::fs::metadata(&source)?.is_file() {
            return Err(ProfileError::MissingSkillSource);
        }
        let directory = source
            .parent()
            .ok_or(ProfileError::MissingSkillDirectory)?
            .to_str()
            .ok_or(ProfileError::MissingSkillSource)?;
        let destination = skill_root.join(name);
        std::fs::create_dir(&destination)?;
        // ACP 1.5.13 skips junctions during skill discovery. Only the entrypoint needs a
        // regular file in the private profile. The original bundle owns supporting files.
        std::fs::write(destination.join("SKILL.md"), std::fs::read(&source)?)?;
        writeln!(locations, "- `{name}` in {directory:?}")
            .expect("writing to a string is infallible");
    }
    if !locations.is_empty() {
        locations.insert_str(0, "\nResolve skill-relative references, supporting files, and scripts against these original bundle directories.\n\n");
    }
    Ok(locations)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Collecting an isolated profile must preserve the native history it linked.
    #[test]
    fn antigravity_profiles_share_history_and_isolate_customizations() {
        let root = tempfile::tempdir().unwrap();
        let durable = SessionHistory::Durable { resume: None };
        let first = antigravity(root.path(), &[], &durable).unwrap();
        let second = antigravity(root.path(), &[], &durable).unwrap();
        let first_agent = agent_name(first.path());
        let second_agent = agent_name(second.path());
        assert_ne!(first_agent, second_agent);
        let history = "antigravity-cli/history-fixture";
        std::fs::write(first.path().join(history), "native history").unwrap();
        let first_path = first.path().to_owned();
        drop(first);

        assert!(!first_path.exists());
        assert_eq!(
            std::fs::read_to_string(second.path().join(history)).unwrap(),
            "native history"
        );
        let definition = std::fs::read_to_string(
            second
                .path()
                .join(format!("config/agents/{second_agent}.md")),
        )
        .unwrap();
        assert!(definition.contains(&format!("\nname: {second_agent}\n")));
        assert!(definition.contains("run_command"));
        assert!(!definition.contains("schedule"));
        assert!(!second.path().join("config/mcp_config.json").exists());
    }

    /// Stage enabled entrypoints, suppress native namesakes, and preserve original bundle paths.
    #[test]
    fn antigravity_main_agent_registers_only_enabled_skill_sources() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("skill source");
        std::fs::create_dir(&source).unwrap();
        let selected = source.join("SKILL.md");
        std::fs::write(&selected, "selected skill").unwrap();
        std::fs::write(source.join("reference.md"), "supporting file").unwrap();
        let selected_directory = dunce::canonicalize(&source).unwrap();
        let workspace_skill = root
            .path()
            .join("workspace/.agents/skills/selected-skill/SKILL.md");
        std::fs::create_dir_all(workspace_skill.parent().unwrap()).unwrap();
        std::fs::write(&workspace_skill, "different workspace skill").unwrap();
        let enabled = SkillDefinition {
            resource_id: None,
            scope: None,
            name: "selected-skill".into(),
            description: String::new(),
            enabled: true,
            user_invocable: true,
            path: Some(selected.to_str().unwrap().into()),
            source: None,
            argument_hint: None,
        };
        let disabled = SkillDefinition {
            name: "disabled-skill".into(),
            enabled: false,
            path: None,
            ..enabled.clone()
        };
        let profile = antigravity(
            root.path(),
            &[enabled, disabled],
            &SessionHistory::Temporary,
        )
        .unwrap();
        let agent = agent_name(profile.path());
        let definition =
            std::fs::read_to_string(profile.path().join(format!("config/agents/{agent}.md")))
                .unwrap();
        assert!(definition.contains("\ninheritCustomizations: false\n"));
        assert!(definition.contains(&format!(
            "- `selected-skill` in {:?}",
            selected_directory.to_str().unwrap()
        )));
        let staged = profile.path().join("skills/selected-skill");
        assert_eq!(
            std::fs::read_to_string(staged.join("SKILL.md")).unwrap(),
            "selected skill"
        );
        assert!(!staged.join("reference.md").exists());
        assert!(!profile.path().join("skills/disabled-skill").exists());
        let exclusions: serde_json::Value = serde_json::from_slice(
            &std::fs::read(profile.path().join("config/skills.json")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            exclusions,
            serde_json::json!({"exclude": ["disabled-skill", "selected-skill"]})
        );
        drop(profile);
        assert_eq!(std::fs::read_to_string(selected).unwrap(), "selected skill");
        assert_eq!(
            std::fs::read_to_string(source.join("reference.md")).unwrap(),
            "supporting file"
        );
        assert_eq!(
            std::fs::read_to_string(workspace_skill).unwrap(),
            "different workspace skill"
        );
    }

    /// Remove temporary native history with its profile and keep it out of durable history.
    #[test]
    fn temporary_antigravity_profile_history_is_collected() {
        let root = tempfile::tempdir().unwrap();
        let durable =
            antigravity(root.path(), &[], &SessionHistory::Durable { resume: None }).unwrap();
        let temporary = antigravity(root.path(), &[], &SessionHistory::Temporary).unwrap();
        let temporary_path = temporary.path().to_owned();
        let temporary_history = temporary_path.join("antigravity-cli");
        std::fs::create_dir_all(&temporary_history).unwrap();
        std::fs::write(temporary_history.join("generated-history"), "temporary").unwrap();

        assert!(
            !durable
                .path()
                .join("antigravity-cli/generated-history")
                .exists()
        );
        drop(temporary);
        assert!(!temporary_path.exists());
    }

    /// Keep native settings snapshots independent of user files and shared native history.
    #[test]
    fn antigravity_projects_authentication_and_restrictions_into_private_cli_data() {
        let root = tempfile::tempdir().unwrap();
        let home = tempfile::tempdir().unwrap();
        let source = home.path().join(NATIVE_SETTINGS_PATH);
        std::fs::create_dir_all(source.parent().unwrap()).unwrap();
        let settings = serde_json::json!({
            "gcp": {
                "project": "fixture-project",
                "location": "fixture-region",
                "credentials": "unrelated fixture secret",
            },
            "modelProvider": "gemini",
            "enableTerminalSandbox": true,
            "permissions": {
                "allow": ["command(fixture)"],
                "ask": ["read_file(fixture)"],
                "deny": ["write_file(fixture)"],
            },
            "toolPermission": "always-proceed",
            "agentMode": "accept-edits",
            "apiKey": "unrelated fixture key",
        });
        std::fs::write(&source, settings.to_string()).unwrap();
        let durable = SessionHistory::Durable { resume: None };
        let first = antigravity(root.path(), &[], &durable).unwrap();
        let second = antigravity(root.path(), &[], &durable).unwrap();
        write_native_settings(first.path(), Some(&source), ProviderOperation::ThreadOpen).unwrap();
        write_native_settings(second.path(), Some(&source), ProviderOperation::ThreadOpen).unwrap();

        let first_path = first.path().join(CLI_DATA_DIRECTORY).join("settings.json");
        let second_path = second.path().join(CLI_DATA_DIRECTORY).join("settings.json");
        let expected = serde_json::json!({
            "gcp": {"project": "fixture-project", "location": "fixture-region"},
            "modelProvider": "gemini",
            "enableTerminalSandbox": true,
            "permissions": {
                "ask": ["read_file(fixture)"],
                "deny": ["write_file(fixture)"],
            },
        });
        assert!(!first.path().join(NATIVE_SETTINGS_PATH).exists());
        assert_eq!(
            serde_json::from_slice::<serde_json::Value>(&std::fs::read(&first_path).unwrap())
                .unwrap(),
            expected,
        );
        std::fs::write(first_path, "{}").unwrap();
        drop(first);
        assert_eq!(
            serde_json::from_slice::<serde_json::Value>(&std::fs::read(second_path).unwrap())
                .unwrap(),
            expected,
        );
        assert_eq!(
            std::fs::read_to_string(source).unwrap(),
            settings.to_string()
        );
    }

    /// Report malformed user settings without including a value that the user entered.
    #[test]
    fn antigravity_reports_invalid_native_settings_without_echoing_values() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("settings.json");
        let invalid = r#"{"gcp":{"project":["fixture-secret"]}}"#;
        std::fs::write(&source, invalid).unwrap();
        let profile = antigravity(root.path(), &[], &SessionHistory::Temporary).unwrap();

        let error =
            write_native_settings(profile.path(), Some(&source), ProviderOperation::ModelList)
                .unwrap_err();

        assert!(
            error
                .to_string()
                .contains("settings.json has invalid content")
        );
        assert!(!error.to_string().contains("fixture-secret"));
        assert!(!profile.path().join(CLI_DATA_DIRECTORY).exists());
        assert_eq!(std::fs::read_to_string(source).unwrap(), invalid);
    }
}
