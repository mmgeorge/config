use std::{
    fs::{self, File, OpenOptions},
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, ensure};

/// Disposable-state contract shared by databases, journals, plans, and captured files.
pub const VERSION: u32 = 60;

/// Pin one executable's state version until its final storage user releases it.
#[derive(Clone, Debug)]
pub(crate) struct StateLease {
    _file: std::sync::Arc<File>,
}

impl StateLease {
    /// Acquire the current version while excluding directory cleanup.
    pub(crate) fn open(root: &Path) -> Result<Self> {
        super::layout::prepare(root)?;
        fs::create_dir_all(root)?;
        let _admission = super::bootstrap_lock(root, "state")?;
        let lease = version_lock(root, VERSION)?;
        lease
            .try_lock_shared()
            .context("Forge state is being cleared")?;
        fs::create_dir_all(directory(root))?;
        Ok(Self {
            _file: std::sync::Arc::new(lease),
        })
    }
}

/// Locate this executable's disposable files without consulting another host's version.
pub fn directory(root: &Path) -> PathBuf {
    root.join("state").join(format!("v{VERSION}"))
}

/// Locate this executable's history database.
pub fn database(root: &Path) -> PathBuf {
    directory(root).join("harness.sqlite3")
}

/// Report each retained directory independently after attempting every eligible version.
#[derive(Debug, serde::Serialize)]
pub struct ClearReport {
    /// Whether this executable has no remaining disposable state.
    pub current_cleared: bool,
    /// Version directories completely removed.
    pub removed: Vec<PathBuf>,
    /// Version directories that could not be completely removed.
    pub retained: Vec<ClearFailure>,
}

/// Explain why one version remains without treating partial deletion as complete success.
#[derive(Debug, serde::Serialize)]
pub struct ClearFailure {
    /// Directory retained by cleanup.
    pub path: PathBuf,
    /// Rendered cleanup failure for command consumers.
    pub message: String,
}

/// Delete unlocked disposable versions without opening or modifying persistent settings.
pub fn clear(root: &Path) -> Result<ClearReport> {
    if super::layout::clear_legacy(root)? {
        return Ok(ClearReport {
            current_cleared: true,
            removed: vec![root.to_owned()],
            retained: Vec::new(),
        });
    }
    fs::create_dir_all(root)?;
    let _admission = super::bootstrap_lock(root, "state")?;
    // Retired global journals can outlive every session that originally referenced them.
    for journal in crate::repository_access::reset_preflight()? {
        crate::repository_access::clear_retired(&journal)?;
    }
    let state = root.join("state");
    let mut report = ClearReport {
        current_cleared: true,
        removed: Vec::new(),
        retained: Vec::new(),
    };
    match fs::symlink_metadata(&state) {
        Ok(metadata) => ensure!(
            metadata.is_dir() && !super::layout::is_link(&state)?,
            "Forge state root is not a plain directory"
        ),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(report),
        Err(error) => return Err(error).context("inspect Forge state root"),
    }
    let entries = fs::read_dir(&state)
        .context("read Forge state versions")?
        .collect::<std::io::Result<Vec<_>>>()
        .context("enumerate Forge state versions")?;
    for entry in entries {
        let path = entry.path();
        let name = entry.file_name();
        let version = name
            .to_str()
            .and_then(|name| name.strip_prefix('v'))
            .and_then(|name| name.parse::<u32>().ok());
        let result = (|| -> Result<()> {
            let version = version.context("unrecognized state directory")?;
            ensure!(
                entry.file_type()?.is_dir() && !super::layout::is_link(&path)?,
                "state version is not a directory"
            );
            let lease = version_lock(root, version)?;
            lease
                .try_lock()
                .context("state version is locked by a running Forge host")?;
            let legacy_path = root.join("state-locks").join(format!("v{version}.lock"));
            let legacy_lease = if legacy_path.try_exists()? {
                let legacy = OpenOptions::new()
                    .read(true)
                    .write(true)
                    .open(&legacy_path)?;
                legacy
                    .try_lock()
                    .context("state version is locked by a retired Forge host")?;
                Some(legacy)
            } else {
                None
            };
            let mut owners = super::layout::CleanupOwnership::default();
            owners.inspect(&path)?;
            let repositories = crate::repository_access::clear_version(&path)?;
            owners.close_databases();
            fs::remove_dir_all(&path).with_context(|| format!("delete {}", path.display()))?;
            drop(repositories);
            drop(legacy_lease);
            Ok(())
        })();
        match result {
            Ok(()) => report.removed.push(path),
            Err(error) => report.retained.push(ClearFailure {
                path,
                message: format!("{error:#}"),
            }),
        }
    }
    if report.retained.is_empty()
        && let Err(error) = fs::remove_dir(&state)
    {
        report.retained.push(ClearFailure {
            path: state,
            message: format!("remove empty Forge state directory: {error}"),
        });
    }
    report.current_cleared = !directory(root).try_exists()?;
    Ok(report)
}

/// Keep lock inodes outside disposable directories so deletion cannot split ownership.
fn version_lock(root: &Path, version: u32) -> Result<File> {
    let locks = root.join("locks");
    fs::create_dir_all(&locks)?;
    Ok(OpenOptions::new()
        .create(true)
        .truncate(false)
        .read(true)
        .write(true)
        .open(locks.join(format!("v{version}.lock")))?)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{session::SessionStore, storage::SqliteStore};

    /// Preserve the whole locked version and clear unlocked siblings, then retry after release.
    #[test]
    fn clear_versions_excludes_live_owners_and_preserves_settings() -> Result<()> {
        let root = tempfile::tempdir()?;
        super::super::layout::prepare(root.path())?;
        let old = root.path().join("state/v1");
        fs::create_dir_all(old.join("objects"))?;
        fs::write(old.join("objects/retained"), "captured file")?;
        fs::create_dir_all(old.join("plan-workspaces/session/plan/files"))?;
        fs::write(
            old.join("plan-workspaces/session/plan/files/retained.rs"),
            "pub fn retained();",
        )?;
        let old_owner = version_lock(root.path(), 1)?;
        old_owner.lock_shared()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&crate::storage::test::session("session", "workspace"))?;
        let mut pins = crate::storage::model_pin::ModelPinStore::open(root.path())?;
        pins.set("mock", "chosen-model", true)?;
        super::super::preferences::host_set(root.path(), "provider", &"mock")?;
        super::super::preferences::host_set(root.path(), "logging", &crate::trace::LogLevel::Warn)?;
        let preference = crate::session::HarnessPreference {
            access: Default::default(),
            shell: Default::default(),
            model: "chosen-model".into(),
            effort: "high".into(),
            model_setting: Default::default(),
            service_tier: crate::provider::ServiceTier::Fast,
            plan_executor: Default::default(),
            plan_compact: true,
            approval_timeout: Default::default(),
            tool_timeout: Default::default(),
        };
        store.save_preference("workspace", "mock", &preference)?;
        let mut settings = crate::settings::SettingsStore::open(root.path())?;
        settings.save_provider(
            0,
            "mock",
            &serde_json::json!({"model":"provider-default","effort":"medium"}),
        )?;
        let readable = root.path().join("readable");
        let writable = root.path().join("writable");
        fs::create_dir_all(&readable)?;
        fs::create_dir_all(&writable)?;
        settings.grant_directories(&[
            crate::permissions::directory::DirectoryGrant {
                operation: crate::permissions::directory::DirectoryOperation::Read,
                path: readable.to_string_lossy().into_owned(),
            },
            crate::permissions::directory::DirectoryGrant {
                operation: crate::permissions::directory::DirectoryOperation::Write,
                path: writable.to_string_lossy().into_owned(),
            },
        ])?;
        let revision = settings.get("workspace", "session", "mock")?.revision;
        settings.update(
            revision,
            crate::settings::SettingScope::Workspace,
            "workspace",
            "session",
            &crate::settings::PortableSettings {
                status_thoughts: Some(true),
                ..Default::default()
            },
        )?;
        let captured = settings.get("workspace", "session", "mock")?;
        let captured_settings =
            serde_json::to_value((&captured.desired, &captured.provider, captured.revision))?;
        use crate::permissions::document::{
            PermissionDecision, default_permission_document, parse_permission_document,
            serialize_permission_document, set_permission_rule,
        };
        let mut permissions = default_permission_document();
        set_permission_rule(
            &mut permissions,
            "read",
            "retained/**",
            PermissionDecision::Allow,
        )?;
        set_permission_rule(
            &mut permissions,
            "edit",
            "protected/**",
            PermissionDecision::Deny,
        )?;
        fs::write(
            root.path().join("permissions.json"),
            serialize_permission_document(&permissions)?,
        )?;
        let assert_persistent_settings = || -> Result<()> {
            assert_eq!(
                super::super::preferences::host_get::<String>(root.path(), "provider")?.as_deref(),
                Some("mock")
            );
            assert_eq!(
                super::super::preferences::host_get::<crate::trace::LogLevel>(
                    root.path(),
                    "logging"
                )?,
                Some(crate::trace::LogLevel::Warn)
            );
            let reopened = crate::settings::SettingsStore::open(root.path())?;
            let after = reopened.get("workspace", "session", "mock")?;
            assert_eq!(
                serde_json::to_value((&after.desired, &after.provider, after.revision))?,
                captured_settings
            );
            assert_eq!(
                parse_permission_document(&fs::read_to_string(
                    root.path().join("permissions.json")
                )?)?,
                permissions
            );
            Ok(())
        };
        drop(settings);
        let locked = clear(root.path())?;
        assert!(!locked.current_cleared);
        assert_eq!(locked.retained.len(), 2);
        assert!(old.join("objects/retained").is_file());
        assert!(
            old.join("plan-workspaces/session/plan/files/retained.rs")
                .is_file()
        );
        assert_persistent_settings()?;
        drop(store);

        let partial = clear(root.path())?;
        assert!(partial.current_cleared);
        assert_eq!(partial.removed, vec![directory(root.path())]);
        assert_eq!(partial.retained.len(), 1);
        assert!(old.join("objects/retained").is_file());
        assert!(
            old.join("plan-workspaces/session/plan/files/retained.rs")
                .is_file()
        );
        assert_eq!(pins.load("mock")?, vec!["chosen-model"]);
        assert_persistent_settings()?;
        drop(old_owner);
        let complete = clear(root.path())?;
        assert!(complete.retained.is_empty());
        assert!(!old.exists());
        assert_persistent_settings()?;
        let fresh = SqliteStore::open(root.path())?;
        assert!(fresh.load_session("session")?.is_none());
        assert_eq!(pins.load("mock")?, vec!["chosen-model"]);
        assert_eq!(
            serde_json::to_value(fresh.load_preference("workspace", "mock")?.unwrap())?,
            serde_json::to_value(preference)?
        );
        Ok(())
    }

    /// A legacy-only lease protects its whole version while unlocked siblings can be removed.
    #[test]
    fn legacy_lease_retains_only_its_version() -> Result<()> {
        let root = tempfile::tempdir()?;
        super::super::layout::prepare(root.path())?;
        fs::create_dir_all(root.path().join("state/v1"))?;
        fs::create_dir_all(root.path().join("state/v2"))?;
        fs::create_dir_all(root.path().join("state-locks"))?;
        let owner = OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(root.path().join("state-locks/v1.lock"))?;
        owner.lock_shared()?;
        fs::write(root.path().join("state/v1/retained"), "keep")?;
        let report = clear(root.path())?;
        assert_eq!(report.retained.len(), 1);
        assert!(root.path().join("state/v1/retained").is_file());
        assert!(!root.path().join("state/v2").exists());
        Ok(())
    }
}
