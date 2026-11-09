use serde::{Deserialize, Serialize};
use std::path::Path;

/// Identify the configured directory list that failed validation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AccessDirectoryKind {
    Readable,
    Writable,
}

impl std::fmt::Display for AccessDirectoryKind {
    /// Render the directory-list role in a configuration diagnostic.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Readable => "readable",
            Self::Writable => "writable",
        })
    }
}

/// Preserve the directory and validation category without erasing filesystem causes.
#[derive(Debug, thiserror::Error)]
pub enum AccessPolicyError {
    #[error("{kind} directory must be absolute: {path}")]
    NotAbsolute {
        kind: AccessDirectoryKind,
        path: String,
    },
    #[error("{kind} directory does not exist: {path}")]
    NotDirectory {
        kind: AccessDirectoryKind,
        path: String,
    },
    #[error("resolve {kind} directory: {path}")]
    Canonicalize {
        kind: AccessDirectoryKind,
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("duplicate {kind} directory: {path}")]
    Duplicate {
        kind: AccessDirectoryKind,
        path: String,
    },
}

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
    #[serde(default)]
    pub isolation_mode: Option<String>,
    /// Restrict one invocation to exact writable roots without persisting workflow authority.
    #[serde(skip)]
    pub effective_write_roots: Option<std::sync::Arc<Vec<String>>>,
}

impl Default for AccessPolicy {
    fn default() -> Self {
        Self {
            sandbox: true,
            read_access: DirectoryScope::Full,
            write_access: WriteScope::Workspace,
            readable_directory: Vec::new(),
            writable_directory: Vec::new(),
            isolation_mode: None,
            effective_write_roots: None,
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

impl AccessPolicy {
    /// Resolve existing directories before admitting a configuration change.
    pub fn validate(mut self) -> Result<Self, AccessPolicyError> {
        for (kind, directories) in [
            (AccessDirectoryKind::Readable, &mut self.readable_directory),
            (AccessDirectoryKind::Writable, &mut self.writable_directory),
        ] {
            let mut normalized = Vec::new();
            for directory in directories.iter() {
                let path = Path::new(directory.trim());
                if !path.is_absolute() {
                    return Err(AccessPolicyError::NotAbsolute {
                        kind,
                        path: directory.clone(),
                    });
                }
                if !path.is_dir() {
                    return Err(AccessPolicyError::NotDirectory {
                        kind,
                        path: directory.clone(),
                    });
                }
                let canonical =
                    path.canonicalize()
                        .map_err(|source| AccessPolicyError::Canonicalize {
                            kind,
                            path: directory.clone(),
                            source,
                        })?;
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
                if normalized.iter().any(|existing: &String| {
                    if cfg!(windows) {
                        existing.eq_ignore_ascii_case(&canonical)
                    } else {
                        existing == &canonical
                    }
                }) {
                    return Err(AccessPolicyError::Duplicate {
                        kind,
                        path: directory.clone(),
                    });
                }
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
        if let Some(roots) = &self.effective_write_roots {
            return roots.as_ref().clone();
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

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DirectoryOperation {
    Read,
    Write,
}
