use crate::settings::ResourceProfile;
use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

impl ResourceProfile {
    /// Bind selected skill definitions and native private state without copying credentials.
    pub(crate) fn antigravity(
        root: &Path,
        native_home: &Path,
        skill: &[crate::settings::SkillResource],
    ) -> Result<Self> {
        let mut profile = Self::create(root, "antigravity")?;
        let private = native_home.join("antigravity-acp");
        std::fs::create_dir_all(&private)?;
        profile.bind(&private, "antigravity-acp")?;
        profile.bind_skills(skill, "config/skills")?;
        Ok(profile)
    }

    /// Expose selected canonical definitions as a session-local Claude plugin.
    pub(crate) fn claude(root: &Path, skill: &[crate::settings::SkillResource]) -> Result<Self> {
        let mut profile = Self::create(root, "claude")?;
        std::fs::create_dir_all(profile.path().join(".claude-plugin"))?;
        std::fs::write(
            profile.path().join(".claude-plugin/plugin.json"),
            r#"{"name":"forge-managed","version":"1.0.0","description":"Forge session skill selection"}"#,
        )?;
        profile.bind_skills(skill, "skills")?;
        Ok(profile)
    }

    /// Resolve the native provider home before overriding the child process environment.
    pub(crate) fn native_home() -> Result<PathBuf> {
        match std::env::var_os("GEMINI_HOME").filter(|value| !value.is_empty()) {
            Some(value) => {
                let path = PathBuf::from(value);
                if let Ok(relative) = path.strip_prefix("~") {
                    return Ok(home::home_dir()
                        .context("home directory unavailable")?
                        .join(relative));
                }
                Ok(path)
            }
            None => Ok(home::home_dir()
                .context("home directory unavailable")?
                .join(".gemini")),
        }
    }
}
