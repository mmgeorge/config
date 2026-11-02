use crate::settings::ResourceProfile;
use anyhow::Result;
use std::path::Path;

impl ResourceProfile {
    /// Bind only selected skills for a Copilot SDK session.
    pub(crate) fn copilot(root: &Path, skill: &[crate::settings::SkillResource]) -> Result<Self> {
        let mut profile = Self::create(root, "copilot")?;
        profile.bind_skills(skill, "skills")?;
        Ok(profile)
    }
}
