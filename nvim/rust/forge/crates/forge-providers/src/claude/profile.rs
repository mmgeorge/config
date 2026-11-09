//! The per-session plugin directory that exposes Harness-selected skills to Claude Code.

use forge_provider_api::catalog::SkillDefinition;
use forge_provider_api::{ProviderError, ProviderOperation};
use std::path::Path;

/// Prepare an isolated `--plugin-dir` whose `skills/` holds links to the selected skills.
pub(crate) fn create(
    root: &Path,
    skills: &[SkillDefinition],
) -> Result<crate::profile::ResourceProfile, ProviderError> {
    build(root, skills).map_err(|error| error.into_provider_error(ProviderOperation::ThreadOpen))
}

fn build(
    root: &Path,
    skills: &[SkillDefinition],
) -> Result<crate::profile::ResourceProfile, crate::profile::ProfileError> {
    let mut profile = crate::profile::ResourceProfile::create(root, "claude")?;
    std::fs::create_dir_all(profile.path().join(".claude-plugin"))?;
    std::fs::write(
        profile.path().join(".claude-plugin/plugin.json"),
        r#"{"name":"forge-managed","version":"1.0.0","description":"Forge session skill selection"}"#,
    )?;
    profile.bind_skills(skills, "skills")?;
    Ok(profile)
}
