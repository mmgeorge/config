use std::{
    fs,
    fs::File,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, ensure};

const MARKER: &str = "{\"layout\":\"harness-settings-and-versioned-state\"}\n";
const RESET_NOTICE: &str = "Forge reset the old Harness storage layout. Previous sessions, settings, and preferences were discarded; defaults now apply.";

/// Initialize the canonical layout under a gate outside the reset boundary.
pub(crate) fn prepare(root: &Path) -> Result<()> {
    initialize(root, Initialization::Startup).map(|_| ())
}

/// Reset a recognized retired layout only after an explicit history-clear command.
pub(crate) fn clear_legacy(root: &Path) -> Result<bool> {
    initialize(root, Initialization::HistoryClear)
}

/// Distinguish read-only startup detection from user-requested destructive cleanup.
#[derive(Clone, Copy, PartialEq)]
enum Initialization {
    Startup,
    HistoryClear,
}

/// Serialize layout initialization and explicit cutover under the same stable gate.
fn initialize(root: &Path, operation: Initialization) -> Result<bool> {
    let root = checked_root(root)?;
    let identity = crate::plan::digest(root.to_string_lossy().as_bytes());
    let parent = root.parent().context("Harness root has no parent")?;
    let gate = parent.join(format!(".harness-{identity}.lock"));
    let _gate = super::exclusive_file_lock(&gate, "Harness layout initialization")?;
    let pending = parent.join(format!(".harness-{identity}.reset"));
    let marker = root.join("layout.json");
    if marker.try_exists()? {
        ensure!(
            fs::read_to_string(&marker)? == MARKER,
            "unknown Harness layout marker at {}",
            marker.display()
        );
        if pending.try_exists()? {
            ensure!(
                operation == Initialization::HistoryClear,
                "Harness cleanup was interrupted. Run :ForgeHistoryClear before starting Harness."
            );
            ensure!(
                fs::read_to_string(&pending)? == root.to_string_lossy(),
                "Harness reset record belongs to another root"
            );
            super::preferences::initialize(&root)?;
            fs::remove_file(pending)?;
        }
        return Ok(false);
    }

    let entries = fs::read_dir(&root)?.collect::<std::io::Result<Vec<_>>>()?;
    let resetting = pending.try_exists()?;
    if resetting {
        ensure!(
            fs::read_to_string(&pending)? == root.to_string_lossy(),
            "Harness reset record belongs to another root"
        );
    } else if !entries.is_empty() {
        ensure!(
            legacy(&root)?,
            "unknown Harness layout at {}; refusing to reset",
            root.display()
        );
    }
    let resetting = resetting || !entries.is_empty();
    ensure!(
        !resetting || operation == Initialization::HistoryClear,
        "An incompatible Harness storage layout was detected at {}. Run :ForgeHistoryClear before starting Harness. This one-time cleanup discards old sessions, settings, and preferences.",
        root.display()
    );
    let mut owners = CleanupOwnership::default();
    if resetting {
        // Resolve every ownership boundary before deleting the first dependent file.
        let retired = crate::repository_access::reset_preflight()?;
        owners.inspect(&root)?;
        fs::write(&pending, root.to_string_lossy().as_bytes())?;
        fs::OpenOptions::new()
            .write(true)
            .open(&pending)?
            .sync_all()?;
        for journal in retired {
            crate::repository_access::clear_retired(&journal)?;
        }
        owners.close_databases();
        for entry in fs::read_dir(&root)? {
            let entry = entry?;
            if entry.file_type()?.is_dir() {
                fs::remove_dir_all(entry.path())
                    .with_context(|| format!("reset {}", entry.path().display()))?;
            } else {
                fs::remove_file(entry.path())
                    .with_context(|| format!("reset {}", entry.path().display()))?;
            }
        }
        drop(owners);
    }

    // Publish settings before disposable storage opens, so logging can diagnose history failures.
    super::preferences::initialize(&root)?;
    if resetting {
        fs::write(root.join("reset-notice.txt"), RESET_NOTICE)?;
        eprintln!("{RESET_NOTICE}");
    }
    fs::write(&marker, MARKER)?;
    fs::OpenOptions::new()
        .write(true)
        .open(&marker)?
        .sync_all()?;
    if resetting {
        fs::remove_file(&pending)?;
    }
    Ok(resetting)
}

/// Resolve the exact reset boundary and reject roots, aliases, and broad configuration directories.
fn checked_root(root: &Path) -> Result<PathBuf> {
    ensure!(root.is_absolute(), "Harness root must be absolute");
    let mut absolute = PathBuf::new();
    for component in std::path::absolute(root)?.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                ensure!(absolute.pop(), "Harness root escapes its filesystem root");
            }
            component => absolute.push(component.as_os_str()),
        }
    }
    ensure!(
        absolute
            .parent()
            .is_some_and(|parent| parent.parent().is_some()),
        "Harness root is too broad"
    );
    for ancestor in absolute.ancestors() {
        if ancestor.try_exists()? {
            ensure!(
                !is_link(ancestor)?,
                "Harness root crosses a symlink or junction: {}",
                ancestor.display()
            );
        }
    }
    let name = absolute
        .file_name()
        .and_then(|name| name.to_str())
        .context("Harness root has no name")?;
    ensure!(
        ![
            ".config",
            "config",
            "AppData",
            "Local",
            "Roaming",
            "nvim",
            "nvim-data",
            "forge",
            "Users"
        ]
        .iter()
        .any(|reserved| name.eq_ignore_ascii_case(reserved)),
        "Harness root names a broader configuration directory"
    );
    fs::create_dir_all(&absolute)?;
    Ok(dunce::canonicalize(absolute)?)
}

/// Recognize only structural evidence from the retired Harness layout.
fn legacy(root: &Path) -> Result<bool> {
    for name in [
        "state-locks",
        "preferences.sqlite3",
        "provider.json",
        "backend.json",
        "harness-trace-config.json",
        "harness.sqlite3",
    ] {
        if root.join(name).try_exists()? {
            return Ok(true);
        }
    }
    let settings = root.join("settings.db");
    if settings.try_exists()? {
        #[expect(
            clippy::disallowed_methods,
            reason = "layout detection reads the retired settings header without modifying it"
        )]
        let connection = rusqlite::Connection::open_with_flags(
            settings,
            rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
        )?;
        return Ok(connection
            .pragma_query_value(None, "user_version", |row| row.get::<_, i64>(0))?
            == 1);
    }
    Ok(false)
}

/// Retain every discovered owner until all preflight checks finish.
#[derive(Default)]
pub(super) struct CleanupOwnership {
    leases: Vec<File>,
    databases: Vec<rusqlite::Connection>,
}

impl CleanupOwnership {
    /// Inspect a complete tree without following links or deleting dependent files.
    pub(super) fn inspect(&mut self, root: &Path) -> Result<()> {
        ensure!(
            !is_link(root)?,
            "cleanup refuses symlink or junction {}",
            root.display()
        );
        for entry in fs::read_dir(root)? {
            let path = entry?.path();
            // Generated provider resources link outside state; their targets are not owned here.
            if is_link(&path)? {
                continue;
            }
            if path.is_dir() {
                self.inspect(&path)?;
            } else if path
                .extension()
                .is_some_and(|extension| extension == "lock")
                && path
                    .parent()
                    .and_then(Path::file_name)
                    .is_none_or(|name| name != "owners")
            {
                let owner = fs::OpenOptions::new().read(true).write(true).open(&path)?;
                owner.try_lock().with_context(|| {
                    format!("live Harness owner blocks cleanup: {}", path.display())
                })?;
                self.leases.push(owner);
            } else if path
                .extension()
                .is_some_and(|extension| extension == "db" || extension == "sqlite3")
            {
                #[expect(
                    clippy::disallowed_methods,
                    reason = "cleanup verifies retired database ownership without interpreting its schema"
                )]
                let connection = rusqlite::Connection::open_with_flags(
                    &path,
                    rusqlite::OpenFlags::SQLITE_OPEN_READ_WRITE,
                )?;
                connection.busy_timeout(std::time::Duration::ZERO)?;
                connection.pragma_update(None, "locking_mode", "EXCLUSIVE")?;
                connection
                    .execute_batch("BEGIN EXCLUSIVE")
                    .with_context(|| {
                        format!("database owner blocks cleanup: {}", path.display())
                    })?;
                self.databases.push(connection);
            }
        }
        Ok(())
    }

    /// Close database handles before unlinking while retaining independent ownership leases.
    pub(super) fn close_databases(&mut self) {
        self.databases.clear();
    }
}

/// Identify reparse points as well as ordinary symbolic links without following them.
pub(crate) fn is_link(path: &Path) -> Result<bool> {
    let metadata = fs::symlink_metadata(path)?;
    #[cfg(windows)]
    {
        use std::os::windows::fs::MetadataExt;
        Ok(metadata.file_attributes() & 0x400 != 0)
    }
    #[cfg(not(windows))]
    {
        Ok(metadata.file_type().is_symlink())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Fresh roots initialize once and preserve canonical preferences on repeated starts.
    #[test]
    fn fresh_layout_preserves_settings() -> Result<()> {
        let root = tempfile::tempdir()?;
        prepare(root.path())?;
        assert!(
            !super::super::state::directory(root.path()).exists(),
            "settings initialization must not open disposable history"
        );
        assert!(!root.path().join("reset-notice.txt").exists());
        super::super::preferences::host_set(root.path(), "provider", &"mock")?;
        prepare(root.path())?;
        assert_eq!(
            super::super::preferences::host_get::<String>(root.path(), "provider")?.as_deref(),
            Some("mock")
        );
        Ok(())
    }

    /// A live retired lease prevents every deletion, then a released lease permits the whole reset.
    #[test]
    fn old_owner_blocks_reset_without_partial_deletion() -> Result<()> {
        let root = tempfile::tempdir()?;
        fs::create_dir(root.path().join("state-locks"))?;
        let lease_path = root.path().join("state-locks/v1.lock");
        let lease = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create_new(true)
            .open(&lease_path)?;
        lease.lock_shared()?;
        fs::write(root.path().join("provider.json"), "old choice")?;
        fs::write(root.path().join("unknown"), "old content")?;
        assert!(
            prepare(root.path())
                .unwrap_err()
                .to_string()
                .contains(":ForgeHistoryClear")
        );
        assert!(
            clear_legacy(root.path())
                .unwrap_err()
                .to_string()
                .contains("live Harness owner")
        );
        assert_eq!(
            fs::read_to_string(root.path().join("unknown"))?,
            "old content"
        );
        drop(lease);
        assert!(prepare(root.path()).is_err());
        clear_legacy(root.path())?;
        assert!(!root.path().join("unknown").exists());
        assert!(!root.path().join("provider.json").exists());
        assert!(root.path().join("reset-notice.txt").exists());
        Ok(())
    }

    /// A missing marker or malformed canonical database never grants reset authority.
    #[test]
    fn unknown_and_corrupt_layouts_are_preserved() -> Result<()> {
        let root = tempfile::tempdir()?;
        fs::write(root.path().join("unknown"), "retained")?;
        assert!(
            prepare(root.path())
                .unwrap_err()
                .to_string()
                .contains("unknown Harness layout")
        );
        fs::remove_file(root.path().join("unknown"))?;
        prepare(root.path())?;
        fs::write(root.path().join("settings.db"), "corrupt")?;
        assert!(super::super::preferences::open(root.path()).is_err());
        assert_eq!(
            fs::read_to_string(root.path().join("settings.db"))?,
            "corrupt"
        );
        Ok(())
    }

    /// Resume a positively identified interrupted reset even after old evidence was removed.
    #[test]
    fn interrupted_reset_resumes_from_external_record() -> Result<()> {
        let parent = tempfile::tempdir()?;
        let root = parent.path().join("harness");
        fs::create_dir(&root)?;
        let canonical = checked_root(&root)?;
        let identity = crate::plan::digest(canonical.to_string_lossy().as_bytes());
        fs::write(
            parent.path().join(format!(".harness-{identity}.reset")),
            canonical.to_string_lossy().as_bytes(),
        )?;
        fs::write(root.join("partially-deleted"), "discard")?;
        assert!(
            prepare(&root)
                .unwrap_err()
                .to_string()
                .contains(":ForgeHistoryClear")
        );
        assert!(root.join("partially-deleted").exists());
        clear_legacy(&root)?;
        assert!(!root.join("partially-deleted").exists());
        assert!(root.join("layout.json").exists());
        Ok(())
    }

    /// Remove generated skill junctions while preserving their external targets during both cleanup modes.
    #[cfg(windows)]
    #[test]
    fn cleanup_preserves_external_junction_targets() -> Result<()> {
        let external = tempfile::tempdir()?;
        fs::write(external.path().join("settings"), "keep")?;
        for legacy in [true, false] {
            let root = tempfile::tempdir()?;
            if legacy {
                fs::create_dir(root.path().join("state-locks"))?;
            } else {
                prepare(root.path())?;
            }
            let skills = root
                .path()
                .join("state/v1/provider-runtime/codex/session/skills");
            fs::create_dir_all(&skills)?;
            let link = skills.join("academic-pseudocode");
            junction::create(external.path(), &link)?;
            let report = super::super::state::clear(root.path())?;
            assert!(report.retained.is_empty());
            assert!(!link.exists());
            assert_eq!(
                fs::read_to_string(external.path().join("settings"))?,
                "keep"
            );
        }
        Ok(())
    }
}
