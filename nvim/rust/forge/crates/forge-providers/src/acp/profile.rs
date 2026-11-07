use forge_provider_api::catalog::SkillDefinition;
use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use std::path::{Path, PathBuf};

/// Prepare the isolated profile and selected skill links for one ACP adapter.
pub(crate) fn create(
    kind: super::AcpKind,
    root: &Path,
    skills: &[SkillDefinition],
) -> Result<crate::profile::ResourceProfile, ProviderError> {
    let profile = match kind {
        super::AcpKind::Claude => claude(root, skills),
        super::AcpKind::Antigravity => {
            let home = native_home()?;
            antigravity(root, skills, &home)
        }
    };
    profile.map_err(|error| error.into_provider_error(ProviderOperation::ThreadOpen))
}

fn claude(
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

fn antigravity(
    root: &Path,
    skills: &[SkillDefinition],
    home: &Path,
) -> Result<crate::profile::ResourceProfile, crate::profile::ProfileError> {
    let mut profile = crate::profile::ResourceProfile::create(root, "antigravity")?;
    let private = home.join("antigravity-acp");
    std::fs::create_dir_all(&private)?;
    profile.bind(&private, "antigravity-acp")?;
    profile.bind_skills(skills, "config/skills")?;
    Ok(profile)
}

/// Resolve the native Antigravity credential root without copying credentials.
pub(crate) fn native_home() -> Result<PathBuf, ProviderError> {
    let native = std::env::var_os("GEMINI_HOME")
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
        .or_else(|| home::home_dir().map(|home| home.join(".gemini")))
        .ok_or_else(|| failure(ProviderFailureKind::Runtime))?;
    if let Ok(relative) = native.strip_prefix("~") {
        return home::home_dir()
            .map(|home| home.join(relative))
            .ok_or_else(|| failure(ProviderFailureKind::Runtime));
    }
    Ok(native)
}

fn failure(kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(ProviderOperation::ThreadOpen, kind))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn antigravity_profile_preserves_native_auth_without_native_mcp_configuration() {
        let root = tempfile::tempdir().unwrap();
        let home = tempfile::tempdir().unwrap();
        let auth = home.path().join("antigravity-acp/auth-fixture");
        let native_mcp = home.path().join("config/mcp_config.json");
        std::fs::create_dir_all(auth.parent().unwrap()).unwrap();
        std::fs::create_dir_all(native_mcp.parent().unwrap()).unwrap();
        std::fs::write(&auth, "native auth fixture").unwrap();
        std::fs::write(&native_mcp, "native mcp fixture").unwrap();

        let profile = antigravity(root.path(), &[], home.path()).unwrap();
        let profile_path = profile.path().to_owned();
        assert_eq!(
            std::fs::read_to_string(profile_path.join("antigravity-acp/auth-fixture")).unwrap(),
            "native auth fixture",
        );
        assert!(!profile_path.join("config/mcp_config.json").exists());

        drop(profile);
        assert!(!profile_path.exists());
        assert_eq!(
            std::fs::read_to_string(auth).unwrap(),
            "native auth fixture"
        );
        assert_eq!(
            std::fs::read_to_string(native_mcp).unwrap(),
            "native mcp fixture"
        );
    }
}
