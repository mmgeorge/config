use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Controls provider isolation independently of approval mode.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AccessPolicy {
    pub sandbox: bool,
    pub write_access: WriteAccess,
    pub writable_directory: Vec<String>,
    pub windows_sandbox: WindowsSandbox,
}

impl Default for AccessPolicy {
    fn default() -> Self {
        Self {
            sandbox: true,
            write_access: WriteAccess::Workspace,
            writable_directory: Vec::new(),
            windows_sandbox: WindowsSandbox::Elevated,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WriteAccess {
    #[default]
    Workspace,
    Full,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WindowsSandbox {
    #[default]
    Elevated,
    Unelevated,
    Mxc,
}

impl AccessPolicy {
    /// Resolve existing directories before admitting a configuration change.
    pub fn validate(mut self) -> Result<Self> {
        let mut normalized = Vec::new();
        for directory in &self.writable_directory {
            let path = Path::new(directory.trim());
            ensure!(path.is_absolute(), "writable directory must be absolute: {directory}");
            ensure!(path.is_dir(), "writable directory does not exist: {directory}");
            let canonical = path.canonicalize()
                .with_context(|| format!("resolve writable directory: {directory}"))?;
            let canonical = canonical.to_string_lossy().replace('\\', "/");
            let canonical = canonical.strip_prefix("//?/UNC/").map(|value| format!("//{value}"))
                .unwrap_or_else(|| canonical.strip_prefix("//?/").unwrap_or(&canonical).to_owned());
            ensure!(!normalized.iter().any(|existing: &String| if cfg!(windows) {
                existing.eq_ignore_ascii_case(&canonical)
            } else { existing == &canonical }), "duplicate writable directory: {directory}");
            normalized.push(canonical);
        }
        self.writable_directory = normalized;
        Ok(self)
    }

    /// Include every mounted drive when sandboxed writes have full scope.
    pub fn writable_roots(&self, workspace: &str) -> Vec<String> {
        if self.write_access == WriteAccess::Workspace {
            let mut roots = vec![workspace.to_owned()];
            roots.extend(self.writable_directory.clone());
            return roots;
        }
        #[cfg(windows)]
        {
            #[link(name = "kernel32")]
            unsafe extern "system" { fn GetLogicalDrives() -> u32; }
            // Query the drive map without opening removable or network drives.
            let drive_mask = unsafe { GetLogicalDrives() };
            let mut roots: Vec<_> = ('A'..='Z').enumerate()
                .filter(|(index, _)| drive_mask & (1 << index) != 0)
                .map(|(_, drive)| format!("{drive}:/")).collect();
            // Preserve access to a workspace on an unmapped network share.
            if let Some(root) = Path::new(workspace).ancestors().last() {
                let root = root.to_string_lossy().into_owned();
                if !roots.contains(&root) { roots.push(root); }
            }
            roots
        }
        #[cfg(not(windows))]
        { vec!["/".into()] }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_and_deduplicates_directory_identity() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().to_string_lossy().into_owned();
        let mut policy = AccessPolicy::default();
        policy.writable_directory = vec![path.clone()];
        let policy = policy.validate()?;
        assert_eq!(policy.writable_roots("/workspace").len(), 2);
        let mut duplicate = policy.clone();
        duplicate.writable_directory.push(path);
        assert!(duplicate.validate().is_err());
        let mut relative = policy;
        relative.writable_directory = vec!["relative".into()];
        assert!(relative.validate().is_err());
        Ok(())
    }
}
