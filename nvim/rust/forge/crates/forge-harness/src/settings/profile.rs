use anyhow::{Context, Result, ensure};
use std::path::{Path, PathBuf};

/// Owns ephemeral provider resource bindings without changing native configuration.
pub(crate) struct ResourceProfile {
    directory: Option<tempfile::TempDir>,
    link: Vec<PathBuf>,
}

impl ResourceProfile {
    /// Bind selected skill definitions and native private state without copying credentials.
    pub(crate) fn antigravity(root: &Path, native_home: &Path, skill: &[crate::settings::SkillResource]) -> Result<Self> {
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
        std::fs::write(profile.path().join(".claude-plugin/plugin.json"),
            r#"{"name":"forge-managed","version":"1.0.0","description":"Forge session skill selection"}"#)?;
        profile.bind_skills(skill, "skills")?;
        Ok(profile)
    }

    /// Bind only selected skills for a Copilot SDK session.
    pub(crate) fn copilot(root: &Path, skill: &[crate::settings::SkillResource]) -> Result<Self> {
        let mut profile = Self::create(root, "copilot")?;
        profile.bind_skills(skill, "skills")?;
        Ok(profile)
    }

    fn create(root: &Path, backend: &str) -> Result<Self> {
        let root = root.join("provider-runtime").join(backend);
        std::fs::create_dir_all(&root)?;
        let directory = tempfile::Builder::new().prefix("session-").tempdir_in(root)?;
        Ok(Self { directory:Some(directory),link:Vec::new() })
    }

    fn bind_skills(&mut self, skill: &[crate::settings::SkillResource], directory: &str) -> Result<()> {
        std::fs::create_dir_all(self.path().join(directory))?;
        let mut name = std::collections::HashSet::new();
        for definition in skill {
            ensure!(
                !definition.name.is_empty()
                    && definition.name != "."
                    && definition.name != ".."
                    && !definition.name.ends_with('.')
                    && definition.name.chars().all(|character| character.is_ascii_alphanumeric() || "-_.".contains(character)),
                "Skill {} cannot be bound as a portable directory name", definition.name
            );
            ensure!(name.insert(definition.name.clone()), "Skill {} has multiple selected definitions", definition.name);
            let source = Path::new(&definition.source).parent().context("skill source has no directory")?;
            self.bind(source, &format!("{directory}/{}", definition.name))?;
        }
        Ok(())
    }

    /// Resolve the native provider home before overriding the child process environment.
    pub(crate) fn native_home() -> Result<PathBuf> {
        match std::env::var_os("GEMINI_HOME").filter(|value| !value.is_empty()) {
            Some(value) => {
                let path = PathBuf::from(value);
                if let Ok(relative) = path.strip_prefix("~") {
                    return Ok(home::home_dir().context("home directory unavailable")?.join(relative));
                }
                Ok(path)
            }
            None => Ok(home::home_dir().context("home directory unavailable")?.join(".gemini")),
        }
    }

    /// Supply the isolated child-process configuration root.
    pub(crate) fn path(&self) -> &Path {
        self.directory.as_ref().expect("profile owns its directory").path()
    }

    fn bind(&mut self, source: &Path, relative: &str) -> Result<()> {
        let target = self.path().join(relative);
        #[cfg(windows)]
        junction::create(dunce::canonicalize(source)?, &target)?;
        #[cfg(unix)]
        std::os::unix::fs::symlink(dunce::canonicalize(source)?, &target)?;
        self.link.push(target);
        Ok(())
    }
}

impl Drop for ResourceProfile {
    fn drop(&mut self) {
        let mut detached = true;
        for link in self.link.iter().rev() {
            #[cfg(windows)]
            let result = junction::delete(link);
            #[cfg(unix)]
            let result = std::fs::remove_file(link);
            detached &= result.is_ok();
        }
        if !detached && let Some(directory) = self.directory.take() {
            let _ = directory.keep();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn isolated_profile_keeps_native_auth_and_excludes_native_mcp() {
        let root = tempfile::tempdir().unwrap();
        let native = root.path().join("native");
        std::fs::create_dir_all(native.join("antigravity-acp")).unwrap();
        std::fs::create_dir_all(native.join("config")).unwrap();
        std::fs::write(native.join("config/mcp_config.json"), "native configuration").unwrap();
        std::fs::write(native.join("antigravity-acp/auth-fixture"), "credential fixture").unwrap();
        let profile = ResourceProfile::antigravity(root.path(), &native, &[]).unwrap();
        let profile_path = profile.path().to_owned();
        assert!(profile_path.join("antigravity-acp/auth-fixture").is_file());
        assert!(!profile_path.join("config/mcp_config.json").exists());
        drop(profile);
        assert!(!profile_path.exists());
        assert_eq!(std::fs::read_to_string(native.join("antigravity-acp/auth-fixture")).unwrap(), "credential fixture");
        assert_eq!(std::fs::read_to_string(native.join("config/mcp_config.json")).unwrap(), "native configuration");
    }

    #[test]
    fn copilot_profile_excludes_sibling_skills_and_preserves_sources_on_drop() {
        let root = tempfile::tempdir().unwrap();
        let selected = root.path().join("source/selected");
        let unselected = root.path().join("source/unselected");
        std::fs::create_dir_all(&selected).unwrap();
        std::fs::create_dir_all(&unselected).unwrap();
        std::fs::write(selected.join("SKILL.md"), "selected content").unwrap();
        std::fs::write(unselected.join("SKILL.md"), "unselected content").unwrap();
        let profile = ResourceProfile::copilot(root.path(), &[crate::settings::SkillResource {
            id:"selected".into(), name:"selected".into(), description:String::new(),
            source:selected.join("SKILL.md").to_string_lossy().into_owned(), targets:Vec::new(), user_invocable:true,
        }]).unwrap();
        let profile_path = profile.path().to_owned();
        assert_eq!(std::fs::read_to_string(profile_path.join("skills/selected/SKILL.md")).unwrap(), "selected content");
        assert!(!profile_path.join("skills/unselected").exists());
        drop(profile);
        assert!(!profile_path.exists());
        assert_eq!(std::fs::read_to_string(selected.join("SKILL.md")).unwrap(), "selected content");
        assert_eq!(std::fs::read_to_string(unselected.join("SKILL.md")).unwrap(), "unselected content");
    }

    #[test]
    fn invalid_skill_names_cannot_escape_or_alias_the_profile_directory() {
        let root = tempfile::tempdir().unwrap();
        let source = root.path().join("source");
        std::fs::create_dir_all(&source).unwrap();
        std::fs::write(source.join("SKILL.md"), "source content").unwrap();
        for name in ["", ".", "..", "../escape", "..\\escape", "/escape", "C:escape", "trailing."] {
            let result = ResourceProfile::copilot(root.path(), &[crate::settings::SkillResource {
                id:"invalid".into(), name:name.into(), description:String::new(),
                source:source.join("SKILL.md").to_string_lossy().into_owned(), targets:Vec::new(), user_invocable:true,
            }]);
            assert!(result.is_err(), "accepted invalid skill name {name:?}");
            assert_eq!(std::fs::read_to_string(source.join("SKILL.md")).unwrap(), "source content");
        }
    }
}
