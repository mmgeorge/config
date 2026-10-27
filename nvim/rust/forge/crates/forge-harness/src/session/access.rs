use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Controls file authorization and provider isolation independently of approval mode.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AccessPolicy {
    pub sandbox: bool,
    #[serde(default)]
    pub read_access: DirectoryScope,
    pub write_access: WriteScope,
    #[serde(default)]
    pub readable_directory: Vec<String>,
    pub writable_directory: Vec<String>,
    pub windows_sandbox: WindowsSandbox,
}

impl Default for AccessPolicy {
    fn default() -> Self {
        Self {
            sandbox: true,
            read_access: DirectoryScope::Full,
            write_access: WriteScope::Workspace,
            readable_directory: Vec::new(),
            writable_directory: Vec::new(),
            windows_sandbox: WindowsSandbox::Elevated,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DirectoryScope {
    Workspace,
    #[default]
    Full,
}

/// Selects write authorization independently of read scope and approval handling.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WriteScope {
    /// Reject reported writes, including writes covered by saved grants.
    Off,
    /// Authorize workspace writes and configured directory grants.
    #[default]
    Workspace,
    /// Authorize writes across the filesystem.
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
        for (label, directories) in [
            ("readable", &mut self.readable_directory),
            ("writable", &mut self.writable_directory),
        ] {
            let mut normalized = Vec::new();
            for directory in directories.iter() {
                let path = Path::new(directory.trim());
                ensure!(
                    path.is_absolute(),
                    "{label} directory must be absolute: {directory}"
                );
                ensure!(
                    path.is_dir(),
                    "{label} directory does not exist: {directory}"
                );
                let canonical = path
                    .canonicalize()
                    .with_context(|| format!("resolve {label} directory: {directory}"))?;
                let canonical = canonical.to_string_lossy().replace('\\', "/");
                let canonical = canonical
                    .strip_prefix("//?/UNC/")
                    .map(|value| format!("//{value}"))
                    .unwrap_or_else(|| {
                        canonical
                            .strip_prefix("//?/")
                            .unwrap_or(&canonical)
                            .to_owned()
                    });
                ensure!(
                    !normalized.iter().any(|existing: &String| if cfg!(windows) {
                        existing.eq_ignore_ascii_case(&canonical)
                    } else {
                        existing == &canonical
                    }),
                    "duplicate {label} directory: {directory}"
                );
                normalized.push(canonical);
            }
            *directories = normalized;
        }
        Ok(self)
    }

    /// Return the six-state access label without incorporating approval handling.
    pub const fn abbreviation(&self) -> &'static str {
        match (self.read_access, self.write_access) {
            (DirectoryScope::Workspace, WriteScope::Off) => "r",
            (DirectoryScope::Full, WriteScope::Off) => "rx",
            (DirectoryScope::Workspace, WriteScope::Workspace) => "rw",
            (DirectoryScope::Full, WriteScope::Workspace) => "rxw",
            (DirectoryScope::Workspace, WriteScope::Full) => "rwx",
            (DirectoryScope::Full, WriteScope::Full) => "rxwx",
        }
    }

    /// Include every mounted drive when sandboxed writes have full scope.
    pub fn writable_roots(&self, workspace: &str) -> Vec<String> {
        if self.write_access == WriteScope::Off {
            return Vec::new();
        }
        if self.write_access == WriteScope::Workspace {
            let mut roots = vec![workspace.to_owned()];
            roots.extend(self.writable_directory.clone());
            return roots;
        }
        #[cfg(windows)]
        {
            #[link(name = "kernel32")]
            unsafe extern "system" {
                fn GetLogicalDrives() -> u32;
            }
            // Query the drive map without opening removable or network drives.
            let drive_mask = unsafe { GetLogicalDrives() };
            let mut roots: Vec<_> = ('A'..='Z')
                .enumerate()
                .filter(|(index, _)| drive_mask & (1 << index) != 0)
                .map(|(_, drive)| format!("{drive}:/"))
                .collect();
            // Preserve access to a workspace on an unmapped network share.
            if let Some(root) = Path::new(workspace).ancestors().last() {
                let root = root.to_string_lossy().into_owned();
                if !roots.contains(&root) {
                    roots.push(root);
                }
            }
            roots
        }
        #[cfg(not(windows))]
        {
            vec!["/".into()]
        }
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
