use super::document::PermissionDecision;
use super::matcher::{
    CompiledPermissionDocument, PermissionEvaluation, PermissionRequest, PermissionTarget,
    path_starts_with,
};
use crate::session::WriteScope;
use crate::session::{AccessPolicy, DirectoryScope};
use crate::settings::{SettingScope, SettingsStore};
use anyhow::{Context, Result, ensure};
use std::path::{Component, Path, PathBuf};

/// Owns reported file authorization for one provider execution without claiming shell confinement.
pub(crate) struct DirectoryPolicy {
    pub(crate) access: AccessPolicy,
    pub(crate) workspace: PathBuf,
    pub(crate) session_id: String,
    pub(crate) settings_root: Option<PathBuf>,
}

impl DirectoryPolicy {
    pub(crate) fn evaluate(
        &self,
        compiled: &CompiledPermissionDocument,
        request: &mut PermissionRequest,
    ) -> Result<PermissionEvaluation> {
        let compiled =
            CompiledPermissionDocument::compile(compiled.document.clone(), &self.workspace);
        let original_target_list = request.target_list.clone();
        for target in &mut request.target_list {
            if let PermissionTarget::Read { path } | PermissionTarget::Write { path } = target {
                *path = resolve_path(Path::new(path), &self.workspace)?
                    .to_string_lossy()
                    .into_owned();
            }
        }
        let access = self.effective_access()?;
        let mut evaluation = compiled.evaluate(request)?;
        let mut denied = evaluation.decision == PermissionDecision::Deny;
        for (target, original) in request.target_list.iter().zip(original_target_list) {
            let (path, full, directories) = match target {
                PermissionTarget::Read { path } => (
                    path,
                    access.read_access == DirectoryScope::Full,
                    &access.readable_directory,
                ),
                PermissionTarget::Write { .. }
                | PermissionTarget::UnknownFile {
                    operation: DirectoryOperation::Write,
                } if access.write_access == WriteScope::Off => {
                    denied = true;
                    evaluation
                        .pending_target_list
                        .retain(|pending| pending != target);
                    continue;
                }
                PermissionTarget::Write { path } => (
                    path,
                    access.write_access == WriteScope::Full,
                    &access.writable_directory,
                ),
                _ => continue,
            };
            let single = PermissionRequest {
                target_list: vec![target.clone()],
                ..request.clone()
            };
            let explicit = compiled.evaluate(&single)?;
            let original = compiled.evaluate(&PermissionRequest {
                target_list: vec![original],
                ..request.clone()
            })?;
            let covered =
                full || path_starts_with(
                    Path::new(path),
                    &resolve_path(&self.workspace, &self.workspace)?,
                ) || directories
                    .iter()
                    .any(|directory| path_starts_with(Path::new(path), Path::new(directory)));
            let specific_allow = explicit.decision == PermissionDecision::Allow
                && explicit
                    .matched_pattern_list
                    .iter()
                    .any(|pattern| !matches!(pattern.as_str(), "read:*" | "edit:*"));
            let decision = if explicit.decision == PermissionDecision::Deny
                || original.decision == PermissionDecision::Deny
            {
                PermissionDecision::Deny
            } else if (!covered && !specific_allow)
                || [&explicit, &original].into_iter().any(|evaluation| {
                    evaluation.decision == PermissionDecision::Ask
                        && !evaluation.matched_pattern_list.is_empty()
                })
            {
                PermissionDecision::Ask
            } else {
                PermissionDecision::Allow
            };
            evaluation
                .pending_target_list
                .retain(|pending| pending != target);
            if decision == PermissionDecision::Ask {
                evaluation.pending_target_list.push(target.clone());
            }
            denied |= decision == PermissionDecision::Deny;
        }
        evaluation.decision = if denied {
            PermissionDecision::Deny
        } else if evaluation.pending_target_list.is_empty() {
            PermissionDecision::Allow
        } else {
            PermissionDecision::Ask
        };
        Ok(evaluation)
    }

    pub(crate) fn grant(&self, target: &PermissionTarget) -> Result<Option<DirectoryGrant>> {
        let (path, operation) = match target {
            PermissionTarget::Read { path } => (path, DirectoryOperation::Read),
            PermissionTarget::Write { path } => (path, DirectoryOperation::Write),
            _ => return Ok(None),
        };
        let path = resolve_path(Path::new(path), &self.workspace)?;
        let mut directory = path.as_path();
        while !directory.is_dir() {
            directory = directory
                .parent()
                .context("file target has no existing parent directory")?;
        }
        let repository = if operation == DirectoryOperation::Write {
            forge_git::discover_identity(directory)?
        } else {
            None
        };
        let directory = repository
            .as_ref()
            .and_then(|identity| identity.worktree_root.as_deref())
            .unwrap_or(directory);
        Ok(Some(DirectoryGrant {
            operation,
            path: directory.to_string_lossy().into_owned(),
        }))
    }

    pub(crate) fn save(&self, grants: &[DirectoryGrant]) -> Result<()> {
        let root = self
            .settings_root
            .as_deref()
            .context("global directory settings are unavailable")?;
        SettingsStore::open(root)?.grant_directories(grants)
    }

    fn effective_access(&self) -> Result<AccessPolicy> {
        if let Some(root) = &self.settings_root {
            let settings = SettingsStore::open(root)?;
            return self.access_from(&settings);
        }
        Ok(self.access.clone())
    }

    pub(crate) fn access_from(&self, settings: &SettingsStore) -> Result<AccessPolicy> {
        let mut access = self.access.clone();
        if let Some(current) = settings
            .get(&self.workspace.to_string_lossy(), &self.session_id, "")?
            .desired
            .access
        {
            access.readable_directory = current.readable_directory;
            access.writable_directory = current.writable_directory;
        }
        let global = settings.layer_settings(SettingScope::Global, "", "")?;
        if let Some(global) = global.access {
            for directory in global.readable_directory {
                if !access.readable_directory.contains(&directory) {
                    access.readable_directory.push(directory);
                }
            }
            for directory in global.writable_directory {
                if !access.writable_directory.contains(&directory) {
                    access.writable_directory.push(directory);
                }
            }
        }
        Ok(access)
    }
}

pub(crate) fn resolve_path(path: &Path, workspace: &Path) -> Result<PathBuf> {
    ensure!(
        !path.as_os_str().is_empty(),
        "file permission target is empty"
    );
    let absolute = if path.is_absolute() {
        path.to_owned()
    } else {
        workspace.join(path)
    };
    ensure!(
        absolute.is_absolute(),
        "file permission workspace must be absolute"
    );
    let mut resolved = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::CurDir => continue,
            Component::ParentDir => {
                ensure!(resolved.pop(), "file target escapes filesystem root");
            }
            component => resolved.push(component.as_os_str()),
        }
        if !resolved.is_absolute() {
            continue;
        }
        match dunce::canonicalize(&resolved) {
            Ok(canonical) => resolved = canonical,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                ensure!(
                    std::fs::symlink_metadata(&resolved).is_err(),
                    "file target contains a dangling symlink"
                );
            }
            Err(error) => {
                return Err(error).with_context(|| {
                    format!("resolve file permission target {}", resolved.display())
                });
            }
        }
    }
    Ok(resolved)
}

pub use forge_provider_api::access::DirectoryOperation;

/// Persist one saved host directory grant together with the operation it authorizes.
#[derive(Clone, Debug, serde::Deserialize, Eq, PartialEq, serde::Serialize)]
pub struct DirectoryGrant {
    pub operation: DirectoryOperation,
    pub path: String,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::permissions::document::parse_permission_document;

    #[test]
    fn write_off_rejects_known_and_unknown_writes_despite_grants() -> Result<()> {
        let root = tempfile::tempdir()?;
        let workspace = dunce::canonicalize(root.path())?;
        let mut policy = DirectoryPolicy {
            access: AccessPolicy {
                write_access: WriteScope::Off,
                writable_directory: vec![workspace.display().to_string()],
                ..Default::default()
            },
            workspace: workspace.clone(),
            session_id: "fixture".into(),
            settings_root: None,
        };
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(
                r#"{"permission":{"read":{"*":"allow"},"edit":{"*":"allow",".":"allow"}}}"#,
            )?,
            &workspace,
        );
        for read_access in [DirectoryScope::Workspace, DirectoryScope::Full] {
            policy.access.read_access = read_access;
            for target in [
                PermissionTarget::Write {
                    path: "new/file".into(),
                },
                PermissionTarget::UnknownFile {
                    operation: DirectoryOperation::Write,
                },
            ] {
                let mut request = PermissionRequest {
                    id: "write".into(),
                    provider: forge_provider_api::ProviderId::new("test")
                        .expect("static provider identity is valid"),
                    reason: None,
                    target_list: vec![target],
                };
                assert_eq!(
                    policy.evaluate(&compiled, &mut request)?.decision,
                    PermissionDecision::Deny
                );
            }
        }
        Ok(())
    }

    #[test]
    fn directory_rules_resolve_against_the_execution_workspace() -> Result<()> {
        let root = tempfile::tempdir()?;
        let workspace = dunce::canonicalize(root.path())?.join("workspace");
        std::fs::create_dir(&workspace)?;
        let policy = DirectoryPolicy {
            access: AccessPolicy::default(),
            workspace,
            session_id: "fixture".into(),
            settings_root: None,
        };
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(r#"{"permission":{"read":{"*":"allow","blocked":"deny"}}}"#)?,
            root.path(),
        );
        {
            let mut request = PermissionRequest {
                id: "file".into(),
                provider: forge_provider_api::ProviderId::new("test")
                    .expect("static provider identity is valid"),
                reason: None,
                target_list: vec![PermissionTarget::Read {
                    path: "blocked/new-file".into(),
                }],
            };
            assert_eq!(
                policy.evaluate(&compiled, &mut request)?.decision,
                PermissionDecision::Deny
            );
        }
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn directory_symlinks_and_parent_traversal_do_not_escape_workspace_grants() -> Result<()> {
        let root = tempfile::tempdir()?;
        let workspace = dunce::canonicalize(root.path())?.join("workspace");
        let external = dunce::canonicalize(root.path())?.join("outside/nested");
        std::fs::create_dir_all(&workspace)?;
        std::fs::create_dir_all(&external)?;
        std::os::unix::fs::symlink(&external, workspace.join("alias"))?;
        std::os::unix::fs::symlink(root.path().join("missing"), workspace.join("dangling"))?;
        let policy = DirectoryPolicy {
            access: AccessPolicy {
                read_access: DirectoryScope::Workspace,
                ..AccessPolicy::default()
            },
            workspace: workspace.clone(),
            session_id: "fixture".into(),
            settings_root: None,
        };
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(r#"{"permission":{"read":{"*":"allow"}}}"#)?,
            &workspace,
        );
        let mut request = PermissionRequest {
            id: "file".into(),
            provider: forge_provider_api::ProviderId::new("test")
                .expect("static provider identity is valid"),
            reason: None,
            target_list: vec![PermissionTarget::Read {
                path: "alias/../file".into(),
            }],
        };
        assert_eq!(
            policy.evaluate(&compiled, &mut request)?.decision,
            PermissionDecision::Ask
        );
        assert_eq!(
            request.target_list,
            vec![PermissionTarget::Read {
                path: external
                    .parent()
                    .unwrap()
                    .join("file")
                    .display()
                    .to_string()
            }]
        );
        request.target_list = vec![PermissionTarget::Write {
            path: "alias/new/file".into(),
        }];
        let write_compiled = CompiledPermissionDocument::compile(
            parse_permission_document(r#"{"permission":{"edit":{".":"allow"}}}"#)?,
            &workspace,
        );
        assert_eq!(
            policy.evaluate(&write_compiled, &mut request)?.decision,
            PermissionDecision::Ask
        );
        request.target_list = vec![PermissionTarget::Read {
            path: "dangling/file".into(),
        }];
        assert!(policy.evaluate(&compiled, &mut request).is_err());
        Ok(())
    }

    #[test]
    fn directory_global_grants_merge_atomically_and_observe_revocation() -> Result<()> {
        let root = tempfile::tempdir()?;
        let first = root.path().join("first");
        let second = root.path().join("second");
        std::fs::create_dir(&first)?;
        std::fs::create_dir(&second)?;
        let settings_root = root.path().join("settings");
        let mut settings = SettingsStore::open(&settings_root)?;
        settings.update(
            0,
            SettingScope::Global,
            "",
            "",
            &crate::settings::PortableSettings {
                plan_compact: Some(true),
                ..Default::default()
            },
        )?;
        let mut worker_list = Vec::new();
        for (operation, path) in [
            (DirectoryOperation::Read, first.clone()),
            (DirectoryOperation::Write, second.clone()),
        ] {
            let settings_root = settings_root.clone();
            worker_list.push(std::thread::spawn(move || -> Result<()> {
                SettingsStore::open(&settings_root)?.grant_directories(&[DirectoryGrant {
                    operation,
                    path: path.display().to_string(),
                }])
            }));
        }
        for worker in worker_list {
            worker.join().expect("grant worker must not panic")?;
        }
        let mut settings = SettingsStore::open(&settings_root)?;
        let global = settings.layer_settings(SettingScope::Global, "", "")?;
        assert_eq!(global.plan_compact, Some(true));
        let access = global.access.as_ref().unwrap();
        assert_eq!(access.readable_directory.len(), 1);
        assert_eq!(access.writable_directory.len(), 1);
        let revision = settings.get("", "fixture", "")?.revision;
        settings.update(
            revision,
            SettingScope::Session,
            "",
            "fixture",
            &crate::settings::PortableSettings {
                access: Some(AccessPolicy {
                    read_access: DirectoryScope::Workspace,
                    ..Default::default()
                }),
                ..Default::default()
            },
        )?;
        let policy = DirectoryPolicy {
            access: AccessPolicy {
                read_access: DirectoryScope::Workspace,
                readable_directory: access.readable_directory.clone(),
                ..AccessPolicy::default()
            },
            workspace: dunce::canonicalize(root.path())?.join("workspace"),
            session_id: "fixture".into(),
            settings_root: Some(settings_root),
        };
        std::fs::create_dir(&policy.workspace)?;
        let compiled = CompiledPermissionDocument::compile(
            parse_permission_document(r#"{"permission":{"read":{"*":"allow"}}}"#)?,
            &policy.workspace,
        );
        let mut request = PermissionRequest {
            id: "file".into(),
            provider: forge_provider_api::ProviderId::new("test")
                .expect("static provider identity is valid"),
            reason: None,
            target_list: vec![PermissionTarget::Read {
                path: first.join("file").display().to_string(),
            }],
        };
        assert_eq!(
            policy.evaluate(&compiled, &mut request)?.decision,
            PermissionDecision::Allow
        );
        let snapshot = settings.get("", "", "")?;
        let mut global = global;
        global.access.as_mut().unwrap().readable_directory.clear();
        settings.update(snapshot.revision, SettingScope::Global, "", "", &global)?;
        assert_eq!(
            policy.evaluate(&compiled, &mut request)?.decision,
            PermissionDecision::Ask
        );
        assert!(
            settings
                .update(snapshot.revision, SettingScope::Global, "", "", &global)
                .is_err()
        );
        let before = settings.get("", "", "")?;
        assert!(
            settings
                .grant_directories(&[DirectoryGrant {
                    operation: DirectoryOperation::Read,
                    path: root.path().join("missing").display().to_string()
                }])
                .is_err()
        );
        assert_eq!(settings.get("", "", "")?.revision, before.revision);
        assert!(
            settings
                .layer_settings(SettingScope::Global, "", "")?
                .access
                .unwrap()
                .readable_directory
                .is_empty()
        );
        Ok(())
    }
}
