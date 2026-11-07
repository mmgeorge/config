use forge_provider_api::catalog::SkillDefinition;
use std::path::{Path, PathBuf};

/// Identify private resource preparation and collection failures without flattening their cause.
#[derive(Debug, thiserror::Error)]
pub(crate) enum ProfileError {
    #[error("resource filesystem operation failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("skill name is not a portable resource directory")]
    InvalidSkillName,
    #[error("duplicate skill resource name")]
    DuplicateSkillName,
    #[error("skill source is unavailable")]
    MissingSkillSource,
    #[error("skill source has no directory")]
    MissingSkillDirectory,
    #[error("invalid resource destination")]
    InvalidDestination,
    #[error("resource links could not be collected")]
    IncompleteCleanup,
}

impl ProfileError {
    /// Classify private resource failures without exposing filesystem error payloads.
    pub(crate) fn into_provider_error(
        self,
        operation: forge_provider_api::ProviderOperation,
    ) -> forge_provider_api::ProviderError {
        use forge_provider_api::ProviderFailureKind;
        let kind = match self {
            Self::Io(error) if error.kind() == std::io::ErrorKind::PermissionDenied => {
                ProviderFailureKind::Authorization
            }
            Self::Io(_) | Self::IncompleteCleanup => ProviderFailureKind::Runtime,
            Self::InvalidSkillName
            | Self::DuplicateSkillName
            | Self::MissingSkillSource
            | Self::MissingSkillDirectory
            | Self::InvalidDestination => ProviderFailureKind::Rejected,
        };
        forge_provider_api::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
            operation, kind,
        ))
    }
}

/// Own private native resource layouts without changing admitted source files.
pub(crate) struct ResourceProfile {
    directory: Option<tempfile::TempDir>,
    links: Vec<PathBuf>,
}

impl ResourceProfile {
    /// Create a temporary layout beneath the runtime's managed resource root.
    pub(crate) fn create(root: &Path, provider: &str) -> Result<Self, ProfileError> {
        let root = root.join("provider-runtime").join(provider);
        std::fs::create_dir_all(&root)?;
        Ok(Self {
            directory: Some(
                tempfile::Builder::new()
                    .prefix("session-")
                    .tempdir_in(root)?,
            ),
            links: Vec::new(),
        })
    }

    /// Bind enabled skill sources after validating their portable destination names.
    pub(crate) fn bind_skills(
        &mut self,
        skills: &[SkillDefinition],
        directory: &str,
    ) -> Result<(), ProfileError> {
        std::fs::create_dir_all(self.path().join(directory))?;
        let mut names = std::collections::HashSet::new();
        for skill in skills.iter().filter(|skill| skill.enabled) {
            if skill.name.is_empty()
                || skill.name == "."
                || skill.name == ".."
                || skill.name.ends_with('.')
                || !skill
                    .name
                    .chars()
                    .all(|character| character.is_ascii_alphanumeric() || "-_.".contains(character))
            {
                return Err(ProfileError::InvalidSkillName);
            }
            if !names.insert(&skill.name) {
                return Err(ProfileError::DuplicateSkillName);
            }
            let source = Path::new(
                skill
                    .path
                    .as_deref()
                    .ok_or(ProfileError::MissingSkillSource)?,
            )
            .parent()
            .ok_or(ProfileError::MissingSkillDirectory)?;
            self.bind(source, &format!("{directory}/{}", skill.name))?;
        }
        Ok(())
    }

    /// Get the native layout retained until all readers and processes are collected.
    pub(crate) fn path(&self) -> &Path {
        self.directory
            .as_ref()
            .expect("profile owns its directory until drop")
            .path()
    }

    /// Link an admitted directory into a private relative layout position.
    pub(crate) fn bind(&mut self, source: &Path, relative: &str) -> Result<(), ProfileError> {
        if relative.is_empty()
            || !Path::new(relative)
                .components()
                .all(|component| matches!(component, std::path::Component::Normal(_)))
        {
            return Err(ProfileError::InvalidDestination);
        }
        let target = self.path().join(relative);
        #[cfg(windows)]
        junction::create(dunce::canonicalize(source)?, &target)?;
        #[cfg(unix)]
        std::os::unix::fs::symlink(dunce::canonicalize(source)?, &target)?;
        self.links.push(target);
        Ok(())
    }

    /// Collect links and the private directory while retaining failed cleanup for retry.
    pub(crate) fn close(&mut self) -> Result<(), ProfileError> {
        let mut failed = Vec::new();
        for link in self.links.drain(..).rev() {
            #[cfg(windows)]
            let result = junction::delete(&link);
            #[cfg(unix)]
            let result = std::fs::remove_file(&link);
            if result.is_err_and(|error| error.kind() != std::io::ErrorKind::NotFound) {
                failed.push(link);
            }
        }
        self.links = failed;
        if !self.links.is_empty() {
            return Err(ProfileError::IncompleteCleanup);
        }
        if let Some(directory) = &self.directory {
            match std::fs::remove_dir_all(directory.path()) {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(ProfileError::Io(error)),
            }
            self.directory.take();
        }
        Ok(())
    }
}

impl Drop for ResourceProfile {
    /// Detach links before removing the layout so source resources survive failed cleanup.
    fn drop(&mut self) {
        if self.close().is_err()
            && let Some(directory) = self.directory.take()
        {
            let _ = directory.keep();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn skill(name: &str, path: &Path, enabled: bool) -> SkillDefinition {
        SkillDefinition {
            resource_id: None,
            scope: None,
            name: name.into(),
            description: String::new(),
            enabled,
            user_invocable: true,
            path: Some(path.to_string_lossy().into_owned()),
            source: None,
            argument_hint: None,
        }
    }

    #[test]
    fn selected_skill_links_exclude_siblings_and_drop_preserves_sources() {
        let root = tempfile::tempdir().unwrap();
        let selected = root.path().join("source/selected");
        let unselected = root.path().join("source/unselected");
        std::fs::create_dir_all(&selected).unwrap();
        std::fs::create_dir_all(&unselected).unwrap();
        let selected_file = selected.join("SKILL.md");
        let unselected_file = unselected.join("SKILL.md");
        std::fs::write(&selected_file, "selected content").unwrap();
        std::fs::write(&unselected_file, "unselected content").unwrap();

        let mut profile = ResourceProfile::create(root.path(), "fixture").unwrap();
        profile
            .bind_skills(&[skill("selected", &selected_file, true)], "skills")
            .unwrap();
        let profile_path = profile.path().to_owned();
        assert_eq!(
            std::fs::read_to_string(profile_path.join("skills/selected/SKILL.md")).unwrap(),
            "selected content",
        );
        assert!(!profile_path.join("skills/unselected").exists());

        drop(profile);
        assert!(!profile_path.exists());
        assert_eq!(
            std::fs::read_to_string(selected_file).unwrap(),
            "selected content"
        );
        assert_eq!(
            std::fs::read_to_string(unselected_file).unwrap(),
            "unselected content"
        );
    }
}
