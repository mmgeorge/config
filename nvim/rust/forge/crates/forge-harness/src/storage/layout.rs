use std::{
    fs,
    fs::File,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, ensure};

const MARKER: &str = "{\"layout\":\"harness-settings-and-versioned-state\"}\n";
const RESET_NOTICE: &str = "Forge cleared the retired Harness history.";
const RETAINED_RESET_NOTICE: &str = "Forge cleared the retired Harness history. Older settings formats were retained in retired-settings under the Harness data directory. Review those files to restore their choices.";
const RETIRED_SETTINGS_DIRECTORY: &str = "retired-settings";
const CURRENT_SETTING_FILES: &[&str] = &["permissions.json", "subagents.json"];
const SETTINGS_DATABASE_FILES: &[&str] = &[
    "settings.db",
    "settings.db-wal",
    "settings.db-shm",
    "settings.db-journal",
];
const RETIRED_SETTING_FILES: &[&str] = &[
    "provider.json",
    "backend.json",
    "harness-trace-config.json",
    "preferences.sqlite3",
    "preferences.sqlite3-wal",
    "preferences.sqlite3-shm",
    "preferences.sqlite3-journal",
];
/// Holds the reset notice at the Harness root until one session start takes it.
const RESET_NOTICE_FILE: &str = "reset-notice.txt";

/// Initialize the canonical layout under a gate outside the reset boundary.
pub(crate) fn prepare(root: &Path) -> Result<()> {
    initialize(root, Initialization::Startup).map(|_| ())
}

/// Reset a recognized retired layout only after an explicit history-clear command.
pub(crate) fn clear_legacy(root: &Path) -> Result<bool> {
    initialize(root, Initialization::HistoryClear)
}

/// Take the pending reset notice so that exactly one Harness session start reports it.
///
/// The reset leaves the notice at the root because the editor that ran the reset has no host to
/// receive it. Taking it under the layout gate lets one of several concurrently starting hosts
/// report it. The notice is removed before it is returned, so a process that stops between the
/// read and the removal leaves it for the next start, and no start can report it twice.
pub(crate) fn take_reset_notice(root: &Path) -> Result<Option<String>> {
    let gate = LayoutGate::acquire(root)?;
    let notice = gate.root.join(RESET_NOTICE_FILE);
    let text = match fs::read_to_string(&notice) {
        Ok(text) => text,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => {
            return Err(error).with_context(|| format!("read {}", notice.display()));
        }
    };
    fs::remove_file(&notice).with_context(|| format!("remove {}", notice.display()))?;
    Ok(Some(text))
}

/// Serializes layout initialization, reset, and notice delivery for one canonical root.
///
/// The lock and the interrupted-reset record live beside the root, outside the reset boundary.
struct LayoutGate {
    root: PathBuf,
    interrupted_reset: PathBuf,
    _lock: File,
}

impl LayoutGate {
    fn acquire(root: &Path) -> Result<Self> {
        let root = checked_root(root)?;
        let identity = crate::plan::digest(root.to_string_lossy().as_bytes());
        let parent = root.parent().context("Harness root has no parent")?;
        let lock = super::exclusive_file_lock(
            &parent.join(format!(".harness-{identity}.lock")),
            "Harness layout initialization",
        )?;
        Ok(Self {
            interrupted_reset: parent.join(format!(".harness-{identity}.reset")),
            root,
            _lock: lock,
        })
    }
}

/// Distinguish read-only startup detection from user-requested destructive cleanup.
#[derive(Clone, Copy, PartialEq)]
enum Initialization {
    Startup,
    HistoryClear,
}

/// Serialize layout initialization and explicit cutover under the same stable gate.
fn initialize(root: &Path, operation: Initialization) -> Result<bool> {
    let gate = LayoutGate::acquire(root)?;
    let root = gate.root.as_path();
    let pending = gate.interrupted_reset.as_path();
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
                fs::read_to_string(pending)? == root.to_string_lossy(),
                "Harness reset record belongs to another root"
            );
            super::preferences::initialize(root)?;
            fs::remove_file(pending)?;
        }
        return Ok(false);
    }

    let entries = fs::read_dir(root)?.collect::<std::io::Result<Vec<_>>>()?;
    let resetting = pending.try_exists()?;
    if resetting {
        ensure!(
            fs::read_to_string(pending)? == root.to_string_lossy(),
            "Harness reset record belongs to another root"
        );
    } else if !entries.is_empty() {
        ensure!(
            legacy(root)?,
            "unknown Harness layout at {}; refusing to reset",
            root.display()
        );
    }
    let resetting = resetting || !entries.is_empty();
    ensure!(
        !resetting || operation == Initialization::HistoryClear,
        "An incompatible Harness storage layout was detected at {}. Run :ForgeHistoryClear to discard retired sessions and retain previous settings before starting Harness.",
        root.display()
    );
    let mut owners = CleanupOwnership::default();
    if resetting {
        // Resolve every ownership boundary before deleting the first dependent file.
        let retained = root.join(RETIRED_SETTINGS_DIRECTORY);
        check_retained_directory(&retained)?;
        for name in CURRENT_SETTING_FILES.iter().chain(SETTINGS_DATABASE_FILES) {
            plain_settings_file(&root.join(name))?;
        }
        let retired = crate::repository_access::reset_preflight()?;
        owners.inspect_excluding(root, Some(RETIRED_SETTINGS_DIRECTORY))?;
        let keep_database = owners.settings_are_compatible(root);
        fs::write(pending, root.to_string_lossy().as_bytes())?;
        fs::OpenOptions::new()
            .write(true)
            .open(pending)?
            .sync_all()?;
        // Put settings outside the deletion set before discarding retired state.
        owners.close_databases();
        retain_retired_settings(root, &retained, keep_database)?;
        for journal in retired {
            crate::repository_access::clear_retired(&journal)?;
        }
        for entry in fs::read_dir(root)? {
            let entry = entry?;
            let name = entry.file_name();
            if name.to_str().is_some_and(|name| {
                file_name_matches(name, RETIRED_SETTINGS_DIRECTORY)
                    || CURRENT_SETTING_FILES
                        .iter()
                        .any(|file| file_name_matches(name, file))
                    || (keep_database
                        && SETTINGS_DATABASE_FILES
                            .iter()
                            .any(|file| file_name_matches(name, file)))
            }) {
                continue;
            }
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
    super::preferences::initialize(root)?;
    if resetting {
        let notice = if root.join(RETIRED_SETTINGS_DIRECTORY).try_exists()? {
            RETAINED_RESET_NOTICE
        } else {
            RESET_NOTICE
        };
        fs::write(root.join(RESET_NOTICE_FILE), notice)?;
        eprintln!("{notice}");
    }
    fs::write(&marker, MARKER)?;
    fs::OpenOptions::new()
        .write(true)
        .open(&marker)?
        .sync_all()?;
    if resetting {
        fs::remove_file(pending)?;
    }
    Ok(resetting)
}

/// Reject an archive alias before any legacy file moves into it.
fn check_retained_directory(path: &Path) -> Result<()> {
    match fs::symlink_metadata(path) {
        Ok(metadata) => ensure!(
            metadata.is_dir() && !is_link(path)?,
            "retired settings location {} is not a plain directory",
            path.display()
        ),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(error).with_context(|| format!("inspect {}", path.display())),
    }
    Ok(())
}

/// Match reserved paths using their ordinary platform case convention.
fn file_name_matches(name: &str, expected: &str) -> bool {
    name == expected || (cfg!(windows) && name.eq_ignore_ascii_case(expected))
}

/// Check that an existing settings file can be preserved without following an alias.
fn plain_settings_file(path: &Path) -> Result<bool> {
    match fs::symlink_metadata(path) {
        Ok(metadata) => {
            ensure!(
                metadata.is_file() && !is_link(path)?,
                "settings path {} is not a plain file",
                path.display()
            );
            Ok(true)
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(error).with_context(|| format!("inspect {}", path.display())),
    }
}

/// Move incompatible settings and SQLite sidecars together under the interrupted-reset record.
fn retain_retired_settings(root: &Path, retained: &Path, keep_database: bool) -> Result<()> {
    let names = RETIRED_SETTING_FILES
        .iter()
        .chain(SETTINGS_DATABASE_FILES.iter().filter(|_| !keep_database));
    let mut moves = Vec::new();
    for name in names {
        let source = root.join(name);
        if plain_settings_file(&source)? {
            let target = retained.join(name);
            ensure!(
                !plain_settings_file(&target)?,
                "retired settings already exist at {}; current files remain at {}",
                target.display(),
                root.display()
            );
            moves.push((source, target));
        }
    }
    if moves.is_empty() {
        return Ok(());
    }
    match fs::create_dir(retained) {
        Ok(()) => {}
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {
            check_retained_directory(retained)?;
        }
        Err(error) => return Err(error).with_context(|| format!("create {}", retained.display())),
    }
    for (source, target) in moves {
        fs::rename(&source, &target).with_context(|| {
            format!(
                "retain settings from {} at {}",
                source.display(),
                target.display()
            )
        })?;
    }
    Ok(())
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
    if plain_settings_file(&settings)? {
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
    databases: Vec<(PathBuf, rusqlite::Connection)>,
}

impl CleanupOwnership {
    /// Inspect a complete tree without following links or deleting dependent files.
    pub(super) fn inspect(&mut self, root: &Path) -> Result<()> {
        self.inspect_excluding(root, None)
    }

    /// Inspect ownership without entering a directory kept outside the deletion set.
    fn inspect_excluding(&mut self, root: &Path, retained: Option<&str>) -> Result<()> {
        ensure!(
            !is_link(root)?,
            "cleanup refuses symlink or junction {}",
            root.display()
        );
        for entry in fs::read_dir(root)? {
            let path = entry?.path();
            if retained.is_some_and(|name| {
                path.file_name()
                    .and_then(|entry| entry.to_str())
                    .is_some_and(|entry| file_name_matches(entry, name))
            }) {
                continue;
            }
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
                .and_then(|extension| extension.to_str())
                .is_some_and(|extension| {
                    file_name_matches(extension, "db") || file_name_matches(extension, "sqlite3")
                })
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
                self.databases.push((path, connection));
            }
        }
        Ok(())
    }

    /// Validate current settings under the same exclusive connection used for cleanup preflight.
    fn settings_are_compatible(&self, root: &Path) -> bool {
        self.databases.iter().any(|(path, connection)| {
            path.parent() == Some(root)
                && path
                    .file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| file_name_matches(name, "settings.db"))
                && super::preferences::validate(connection).is_ok()
        })
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
        assert_eq!(
            fs::read_to_string(root.path().join("provider.json"))?,
            "old choice"
        );
        assert!(!root.path().join(RETIRED_SETTINGS_DIRECTORY).exists());
        drop(lease);
        assert!(prepare(root.path()).is_err());
        clear_legacy(root.path())?;
        assert!(!root.path().join("unknown").exists());
        assert!(!root.path().join("provider.json").exists());
        assert_eq!(
            fs::read_to_string(
                root.path()
                    .join(RETIRED_SETTINGS_DIRECTORY)
                    .join("provider.json"),
            )?,
            "old choice"
        );
        assert!(root.path().join("reset-notice.txt").exists());
        Ok(())
    }

    /// A reset leaves one notice: the first taker receives it, and later takers find none.
    #[test]
    fn reset_notice_is_taken_once() -> Result<()> {
        let fresh_root = tempfile::tempdir()?;
        prepare(fresh_root.path())?;
        assert_eq!(take_reset_notice(fresh_root.path())?, None);
        let retired_root = tempfile::tempdir()?;
        fs::write(retired_root.path().join("provider.json"), "old choice")?;
        assert!(clear_legacy(retired_root.path())?);
        assert_eq!(
            take_reset_notice(retired_root.path())?.as_deref(),
            Some(RETAINED_RESET_NOTICE)
        );
        assert!(!retired_root.path().join(RESET_NOTICE_FILE).exists());
        assert_eq!(take_reset_notice(retired_root.path())?, None);
        // Taking the notice leaves a canonical layout that needs no further reset.
        prepare(retired_root.path())?;
        assert!(!clear_legacy(retired_root.path())?);
        Ok(())
    }

    /// Retain unsupported settings while clearing retired history and initializing fresh state.
    #[test]
    fn retired_layout_retains_incompatible_settings() -> Result<()> {
        let root = tempfile::tempdir()?;
        let settings = root.path().join("settings.db");
        #[expect(
            clippy::disallowed_methods,
            reason = "build a retired settings database without opening current state"
        )]
        let previous = rusqlite::Connection::open(&settings)?;
        previous.pragma_update(None, "journal_mode", "WAL")?;
        previous.execute_batch(
            "PRAGMA user_version = 1;
             CREATE TABLE setting_layer (scope TEXT, payload TEXT);
             INSERT INTO setting_layer VALUES ('global', 'saved preference');",
        )?;
        drop(previous);
        let permissions = root.path().join("permissions.json");
        fs::write(&permissions, r#"{"rules":["fixture"]}"#)?;
        let history = root.path().join("objects");
        fs::create_dir(&history)?;
        fs::write(history.join("retired-object"), "discard")?;

        assert!(prepare(root.path()).is_err());
        assert!(clear_legacy(root.path())?);
        assert!(!history.exists());
        assert_eq!(fs::read_to_string(permissions)?, r#"{"rules":["fixture"]}"#);
        let retained = root
            .path()
            .join(RETIRED_SETTINGS_DIRECTORY)
            .join("settings.db");
        #[expect(
            clippy::disallowed_methods,
            reason = "read the retained retired database without normalizing it"
        )]
        let previous = rusqlite::Connection::open_with_flags(
            &retained,
            rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
        )?;
        let payload: String = previous.query_row(
            "SELECT payload FROM setting_layer WHERE scope='global'",
            [],
            |row| row.get(0),
        )?;
        assert_eq!(payload, "saved preference");
        drop(previous);
        super::super::preferences::open(root.path())?;
        assert!(!super::super::state::directory(root.path()).exists());
        assert!(!clear_legacy(root.path())?);
        assert!(retained.is_file());
        Ok(())
    }

    /// Keep a canonical settings database when disposable files still use the retired layout.
    #[test]
    fn retired_layout_preserves_compatible_settings_in_place() -> Result<()> {
        let root = tempfile::tempdir()?;
        prepare(root.path())?;
        super::super::preferences::host_set(root.path(), "provider", &"antigravity")?;
        fs::remove_file(root.path().join("layout.json"))?;
        fs::create_dir(root.path().join("state-locks"))?;

        assert!(clear_legacy(root.path())?);
        assert_eq!(
            super::super::preferences::host_get::<String>(root.path(), "provider")?.as_deref(),
            Some("antigravity")
        );
        assert!(!root.path().join(RETIRED_SETTINGS_DIRECTORY).exists());
        assert_eq!(take_reset_notice(root.path())?.as_deref(), Some(RESET_NOTICE));
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
        let retained = root.join(RETIRED_SETTINGS_DIRECTORY);
        fs::create_dir(&retained)?;
        fs::write(retained.join("provider.json"), "saved provider")?;
        fs::write(root.join("harness-trace-config.json"), "saved trace config")?;
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
        assert_eq!(
            fs::read_to_string(retained.join("provider.json"))?,
            "saved provider"
        );
        assert_eq!(
            fs::read_to_string(retained.join("harness-trace-config.json"))?,
            "saved trace config"
        );
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
