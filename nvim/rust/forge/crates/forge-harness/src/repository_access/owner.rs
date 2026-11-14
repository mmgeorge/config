use super::journal::{BorrowedOwner, JournalError, RepositoryJournal};
use std::{
    fs::{File, OpenOptions},
    path::{Path, PathBuf},
    sync::{Arc, Mutex, Weak},
};

/// Limit an operation to reading source, or permit it to change source.
#[derive(Clone, Copy, Debug, Eq, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RepositoryAccess {
    Read,
    Write,
}

/// Preserve admission failures independently of their presentation in a tool response.
#[derive(Debug, thiserror::Error)]
pub(crate) enum RepositoryAccessError {
    #[error("{0}")]
    Journal(#[from] JournalError),
    #[error("repository_busy: workspace check reservation is unavailable for {}; another operation owns access", .0.display())]
    Busy(PathBuf),
    #[error("repository_scope_expired: repository ownership was revoked for {}", .0.display())]
    Expired(PathBuf),
    #[error("repository_read_only: shared access cannot authorize a writer for {}", .0.display())]
    ReadOnly(PathBuf),
    #[error("repository_scope_mismatch: access to {} cannot authorize {}", owned.display(), requested.display())]
    DifferentRepository { owned: PathBuf, requested: PathBuf },
    #[error("repository_ownership_unavailable: {0}")]
    Unavailable(&'static str),
    #[error("repository access I/O failed for {}: {source}", path.display())]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
}

/// Retain repository exclusion while delegating one execution slot at a time.
#[derive(Debug)]
#[must_use = "retain repository ownership until its operations have been collected"]
pub(crate) struct RepositoryReservation {
    core: Arc<ReservationCore>,
    id: uuid::Uuid,
}

impl RepositoryReservation {
    /// Retain a provider's enclosing reservation while its native process can execute work.
    pub(crate) fn process_owner(&self) -> Arc<dyn forge_shell::process::ProcessOwner> {
        Arc::new(RepositorySessionProcess {
            core: self.core.clone(),
            id: self.id,
        })
    }

    /// Mark the enclosing operation's durable result before releasing its reservation.
    pub(crate) fn published(&self) -> Result<(), RepositoryAccessError> {
        Ok(self.core.journal().published_for(self.id)?)
    }

    /// Acquire source access in the per-user namespace without waiting on an owner.
    pub(crate) fn acquire(
        workspace: &Path,
        access: RepositoryAccess,
    ) -> Result<Self, RepositoryAccessError> {
        Self::acquire_in(&reservation_directory()?, workspace, access)
    }

    /// Get a revocable capability without extending this owner's lifetime.
    pub(crate) fn scope(&self) -> RepositoryScope {
        RepositoryScope {
            core: Arc::downgrade(&self.core),
            workspace: self.core.workspace.clone(),
            id: self.id,
        }
    }

    /// Acquire access under the journal admission gate in an explicitly selected namespace.
    fn acquire_in(
        directory: &Path,
        workspace: &Path,
        access: RepositoryAccess,
    ) -> Result<Self, RepositoryAccessError> {
        let workspace = Arc::new(canonical_workspace(workspace)?);
        let identity = workspace
            .to_str()
            .ok_or(RepositoryAccessError::Unavailable(
                "repository identity is not valid UTF-8",
            ))?;
        let key = crate::plan::digest(identity.as_bytes());
        std::fs::create_dir_all(directory).map_err(|source| RepositoryAccessError::Io {
            path: directory.to_owned(),
            source,
        })?;
        // Keep the gate separate from source access so healthy readers can coexist.
        let gate = lock_file(&directory.join(format!("{key}.admission.lock")))?;
        lock(&gate, &workspace, RepositoryAccess::Write)?;
        RepositoryJournal::recover(directory, &key, &workspace)?;
        let file = lock_file(&directory.join(format!("{key}.lock")))?;
        lock(&file, &workspace, access)?;
        let id = uuid::Uuid::new_v4();
        let journal = RepositoryJournal::begin(directory, &key, &workspace, id, access)?;
        Ok(Self {
            core: Arc::new(ReservationCore {
                file: Some(file),
                journal: Some(journal),
                workspace,
                state: Mutex::new(ReservationState {
                    levels: vec![ReservationLevel {
                        id,
                        access,
                        live: true,
                    }],
                    executing: None,
                }),
            }),
            id,
        })
    }
}

impl Drop for RepositoryReservation {
    fn drop(&mut self) {
        if let Ok(mut state) = self.core.state.lock() {
            if let Some(level) = state.levels.iter_mut().find(|level| level.id == self.id) {
                level.live = false;
            }
            if state.levels.first().is_some_and(|level| !level.live)
                && let Err(error) = self.core.journal().collecting()
            {
                tracing::error!(error = %error, "failed to record repository collection");
            }
            state.collect_retired();
        }
    }
}

/// Authorize a descendant without permitting context clones to create execution slots.
#[derive(Clone, Debug)]
pub(crate) struct RepositoryScope {
    core: Weak<ReservationCore>,
    workspace: Arc<PathBuf>,
    id: uuid::Uuid,
}

impl RepositoryScope {
    /// Suspend this idle owner and lend its protection to one descendant.
    pub(crate) fn borrow(
        &self,
        workspace: &Path,
        access: RepositoryAccess,
    ) -> Result<RepositoryReservation, RepositoryAccessError> {
        let core = self.resolve(workspace)?;
        let id = uuid::Uuid::new_v4();
        {
            let mut state = core.state.lock().map_err(|_| {
                RepositoryAccessError::Unavailable("repository execution state lock poisoned")
            })?;
            state.authorize(self.id, access, &core.workspace)?;
            let mut borrowers = state.borrowers();
            borrowers.push(BorrowedOwner::new(id, self.id, access));
            core.journal().borrowed(borrowers)?;
            state.levels.push(ReservationLevel {
                id,
                access,
                live: true,
            });
        }
        Ok(RepositoryReservation { core, id })
    }

    /// Hold this owner's execution slot through a native operation or physical mutation.
    pub(crate) fn enter(
        &self,
        workspace: &Path,
        access: RepositoryAccess,
    ) -> Result<RepositoryExecution, RepositoryAccessError> {
        let core = self.resolve(workspace)?;
        {
            let mut state = core.state.lock().map_err(|_| {
                RepositoryAccessError::Unavailable("repository execution state lock poisoned")
            })?;
            state.authorize(self.id, access, &core.workspace)?;
            core.journal().started(state.borrowers())?;
            state.executing = Some(self.id);
        }
        let lifetime = Arc::new(ExecutionLifetime {
            core: core.clone(),
            id: self.id,
        });
        Ok(RepositoryExecution {
            core,
            id: self.id,
            lifetime,
        })
    }

    /// Resolve an existing owner without falling back to independent admission after revocation.
    fn resolve(&self, workspace: &Path) -> Result<Arc<ReservationCore>, RepositoryAccessError> {
        let requested = canonical_workspace(workspace)?;
        if requested != *self.workspace {
            return Err(RepositoryAccessError::DifferentRepository {
                owned: (*self.workspace).clone(),
                requested,
            });
        }
        self.core
            .upgrade()
            .ok_or_else(|| RepositoryAccessError::Expired((*self.workspace).clone()))
    }
}

/// Retain the source lock and exclude borrowers until execution has been collected.
#[derive(Debug)]
#[must_use = "retain execution through process collection and mutation recording"]
pub(crate) struct RepositoryExecution {
    core: Arc<ReservationCore>,
    id: uuid::Uuid,
    lifetime: Arc<ExecutionLifetime>,
}

impl RepositoryExecution {
    /// Retain source access in every process worker until physical collection ends.
    pub(crate) fn process_owner(&self) -> Arc<dyn forge_shell::process::ProcessOwner> {
        Arc::new(RepositoryProcess {
            core: self.core.clone(),
            id: self.id,
            _execution: self.lifetime.clone(),
        })
    }

    /// Commit the durable result independently of the process collection state.
    pub(crate) fn published(&self) -> Result<(), RepositoryAccessError> {
        Ok(self.core.journal().published_for(self.id)?)
    }

    /// Preserve a failed result publication while permitting eventual process collection.
    pub(crate) fn unresolved(&self, reason: String) -> Result<(), RepositoryAccessError> {
        Ok(self.core.journal().unresolved(self.id, reason)?)
    }

    /// Reject a revoked operation before its next side effect without releasing its exclusion.
    pub(crate) fn validate(&self) -> Result<(), RepositoryAccessError> {
        let state = self.core.state.lock().map_err(|_| {
            RepositoryAccessError::Unavailable("repository execution state lock poisoned")
        })?;
        if state.executing != Some(self.id)
            || state.levels.iter().any(|level| !level.live)
            || state.levels.last().map(|level| level.id) != Some(self.id)
        {
            return Err(RepositoryAccessError::Expired(
                (*self.core.workspace).clone(),
            ));
        }
        Ok(())
    }
}

struct RepositoryProcess {
    core: Arc<ReservationCore>,
    id: uuid::Uuid,
    _execution: Arc<ExecutionLifetime>,
}

/// Retain the execution slot until every worker capable of owning a child has ended.
#[derive(Debug)]
struct ExecutionLifetime {
    core: Arc<ReservationCore>,
    id: uuid::Uuid,
}

struct RepositorySessionProcess {
    core: Arc<ReservationCore>,
    id: uuid::Uuid,
}

impl forge_shell::process::ProcessOwner for RepositorySessionProcess {
    /// Admit provider startup under its enclosing reservation before any native child exists.
    fn spawn(
        &self,
        command: tokio::process::Command,
    ) -> std::io::Result<Box<dyn process_wrap::tokio::ChildWrapper>> {
        let state = self
            .core
            .state
            .lock()
            .map_err(|_| std::io::Error::other("repository execution state lock poisoned"))?;
        state
            .authorize(self.id, RepositoryAccess::Write, &self.core.workspace)
            .map_err(std::io::Error::other)?;
        self.core
            .journal()
            .started(state.borrowers())
            .map_err(std::io::Error::other)?;
        self.core.journal().containment().spawn(command)
    }
}

impl forge_shell::process::ProcessOwner for RepositoryProcess {
    /// Recheck the admitted execution before atomically creating its contained child.
    fn spawn(
        &self,
        command: tokio::process::Command,
    ) -> std::io::Result<Box<dyn process_wrap::tokio::ChildWrapper>> {
        let state = self
            .core
            .state
            .lock()
            .map_err(|_| std::io::Error::other("repository execution state lock poisoned"))?;
        if state.executing != Some(self.id) || state.levels.iter().any(|level| !level.live) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "repository execution was revoked before launch",
            ));
        }
        self.core.journal().containment().spawn(command)
    }
}

impl Drop for ExecutionLifetime {
    fn drop(&mut self) {
        if let Ok(mut state) = self.core.state.lock() {
            if state.executing == Some(self.id) {
                state.executing = None;
            }
            state.collect_retired();
        }
    }
}

#[derive(Debug)]
struct ReservationCore {
    file: Option<File>,
    journal: Option<RepositoryJournal>,
    workspace: Arc<PathBuf>,
    state: Mutex<ReservationState>,
}

impl ReservationCore {
    /// Get the journal retained until the last owner and execution guard are collected.
    fn journal(&self) -> &RepositoryJournal {
        self.journal
            .as_ref()
            .expect("a live reservation owns its journal")
    }
}

impl Drop for ReservationCore {
    fn drop(&mut self) {
        let journal = self
            .journal
            .take()
            .expect("a live reservation owns its journal");
        let file = self
            .file
            .take()
            .expect("a live reservation owns its source lock");
        RepositoryJournal::finish_or_retain(journal, file);
    }
}

#[derive(Debug)]
struct ReservationState {
    levels: Vec<ReservationLevel>,
    executing: Option<uuid::Uuid>,
}

impl ReservationState {
    /// Capture the last admitted descendant chain for crash diagnostics.
    fn borrowers(&self) -> Vec<BorrowedOwner> {
        self.levels
            .windows(2)
            .map(|pair| BorrowedOwner::new(pair[1].id, pair[0].id, pair[1].access))
            .collect()
    }

    /// Validate the leaf and access mode before atomically taking its single execution slot.
    fn authorize(
        &self,
        id: uuid::Uuid,
        access: RepositoryAccess,
        workspace: &Path,
    ) -> Result<(), RepositoryAccessError> {
        let Some(owner) = self.levels.iter().find(|level| level.id == id) else {
            return Err(RepositoryAccessError::Expired(workspace.to_owned()));
        };
        if self.levels.iter().any(|level| !level.live) {
            return Err(RepositoryAccessError::Expired(workspace.to_owned()));
        }
        if owner.access == RepositoryAccess::Read && access == RepositoryAccess::Write {
            return Err(RepositoryAccessError::ReadOnly(workspace.to_owned()));
        }
        if self.levels.last().map(|level| level.id) != Some(id) || self.executing.is_some() {
            return Err(RepositoryAccessError::Busy(workspace.to_owned()));
        }
        Ok(())
    }

    /// Resume an ancestor only after its retired descendants and their execution are gone.
    fn collect_retired(&mut self) {
        while self
            .levels
            .last()
            .is_some_and(|level| !level.live && self.executing != Some(level.id))
        {
            self.levels.pop();
        }
    }
}

#[derive(Debug)]
struct ReservationLevel {
    id: uuid::Uuid,
    access: RepositoryAccess,
    live: bool,
}

/// Resolve filesystem aliases before either acquiring or borrowing repository access.
fn canonical_workspace(workspace: &Path) -> Result<PathBuf, RepositoryAccessError> {
    let workspace =
        std::fs::canonicalize(workspace).map_err(|source| RepositoryAccessError::Io {
            path: workspace.to_owned(),
            source,
        })?;
    // Windows canonical paths can retain the caller's drive-letter casing.
    if cfg!(windows) {
        return workspace
            .to_str()
            .map(|path| PathBuf::from(path.to_lowercase()))
            .ok_or(RepositoryAccessError::Unavailable(
                "repository identity is not valid UTF-8",
            ));
    }
    Ok(workspace)
}

/// Open a stable lock inode without replacing a live owner's file.
fn lock_file(path: &Path) -> Result<File, RepositoryAccessError> {
    OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)
        .map_err(|source| RepositoryAccessError::Io {
            path: path.to_owned(),
            source,
        })
}

/// Preserve lock contention separately from filesystem or platform errors.
fn lock(
    file: &File,
    workspace: &Path,
    access: RepositoryAccess,
) -> Result<(), RepositoryAccessError> {
    let result = match access {
        RepositoryAccess::Read => file.try_lock_shared(),
        RepositoryAccess::Write => file.try_lock(),
    };
    match result {
        Ok(()) => Ok(()),
        Err(std::fs::TryLockError::WouldBlock) => {
            Err(RepositoryAccessError::Busy(workspace.to_owned()))
        }
        Err(std::fs::TryLockError::Error(source)) => Err(RepositoryAccessError::Io {
            path: workspace.to_owned(),
            source,
        }),
    }
}

/// Select a shared per-user namespace independently of disposable session state.
fn reservation_directory() -> Result<PathBuf, RepositoryAccessError> {
    if let Some(directory) = std::env::var_os("FORGE_REPOSITORY_RESERVATIONS_DIR") {
        let directory = PathBuf::from(directory);
        if !directory.is_absolute() {
            return Err(RepositoryAccessError::Unavailable(
                "FORGE_REPOSITORY_RESERVATIONS_DIR must be an absolute path",
            ));
        }
        return Ok(directory);
    }
    let directory = if cfg!(windows) {
        PathBuf::from(std::env::var_os("LOCALAPPDATA").ok_or(
            RepositoryAccessError::Unavailable("LOCALAPPDATA is unavailable for repository access"),
        )?)
    } else {
        PathBuf::from(
            std::env::var_os("HOME").ok_or(RepositoryAccessError::Unavailable(
                "HOME is unavailable for repository access",
            ))?,
        )
        .join("Library")
        .join("Application Support")
    };
    if !directory.is_absolute() {
        return Err(RepositoryAccessError::Unavailable(
            "repository access user directory must be absolute",
        ));
    }
    Ok(directory.join("forge").join("repository-reservations"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A detached process worker owns the execution slot even after its requesting task disappears.
    #[test]
    fn detached_worker_retains_execution_until_collection() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let root = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let scope = root.scope();
        let child = scope.borrow(workspace.path(), RepositoryAccess::Read)?;
        let execution = child
            .scope()
            .enter(workspace.path(), RepositoryAccess::Read)?;
        let worker = execution.process_owner();
        drop(execution);
        drop(child);
        assert!(matches!(
            scope.enter(workspace.path(), RepositoryAccess::Write),
            Err(RepositoryAccessError::Expired(_) | RepositoryAccessError::Busy(_))
        ));
        drop(worker);
        let resumed = scope.enter(workspace.path(), RepositoryAccess::Write)?;
        resumed.published()?;
        Ok(())
    }

    /// Admit independent readers and clones without conflating them with a writer or path alias.
    #[test]
    fn aliases_share_access_while_independent_repositories_do_not() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let alias = workspace.path().join(".");
        let first = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Read,
        )?;
        let second =
            RepositoryReservation::acquire_in(directory.path(), &alias, RepositoryAccess::Read)?;
        assert!(matches!(
            RepositoryReservation::acquire_in(directory.path(), &alias, RepositoryAccess::Write),
            Err(RepositoryAccessError::Busy(_))
        ));
        drop(first);
        assert!(
            RepositoryReservation::acquire_in(directory.path(), &alias, RepositoryAccess::Write)
                .is_err()
        );
        drop(second);
        let writer =
            RepositoryReservation::acquire_in(directory.path(), &alias, RepositoryAccess::Write)?;
        assert!(
            RepositoryReservation::acquire_in(
                directory.path(),
                workspace.path(),
                RepositoryAccess::Read
            )
            .is_err()
        );
        let independent = tempfile::tempdir()?;
        let _other = RepositoryReservation::acquire_in(
            directory.path(),
            independent.path(),
            RepositoryAccess::Write,
        )?;
        drop(writer);
        let _released = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Read,
        )?;
        Ok(())
    }

    /// Serialize borrowers without upgrading a reader or releasing the parent's OS exclusion.
    #[test]
    fn borrowing_suspends_parent_and_limits_child_access() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let parent = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let scope = parent.scope();
        let child = scope.borrow(workspace.path(), RepositoryAccess::Read)?;
        assert!(matches!(
            scope.enter(workspace.path(), RepositoryAccess::Write),
            Err(RepositoryAccessError::Busy(_))
        ));
        assert!(
            scope
                .borrow(workspace.path(), RepositoryAccess::Read)
                .is_err()
        );
        assert!(matches!(
            child
                .scope()
                .borrow(workspace.path(), RepositoryAccess::Write),
            Err(RepositoryAccessError::ReadOnly(_))
        ));
        let reading = child
            .scope()
            .enter(workspace.path(), RepositoryAccess::Read)?;
        reading.validate()?;
        drop(reading);
        drop(child);
        let writing = scope.enter(workspace.path(), RepositoryAccess::Write)?;
        assert!(
            scope
                .borrow(workspace.path(), RepositoryAccess::Read)
                .is_err()
        );
        writing.validate()?;
        assert!(
            RepositoryReservation::acquire_in(
                directory.path(),
                workspace.path(),
                RepositoryAccess::Read
            )
            .is_err()
        );
        Ok(())
    }

    /// Revoke descendants on ancestor loss while keeping exclusion through the last live execution.
    #[test]
    fn ancestor_drop_revokes_descendants_but_retains_running_work() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let parent = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let scope = parent.scope();
        let child = scope.borrow(workspace.path(), RepositoryAccess::Write)?;
        let child_scope = child.scope();
        let executing = child_scope.enter(workspace.path(), RepositoryAccess::Write)?;
        drop(parent);
        assert!(matches!(
            executing.validate(),
            Err(RepositoryAccessError::Expired(_))
        ));
        assert!(
            child_scope
                .borrow(workspace.path(), RepositoryAccess::Read)
                .is_err()
        );
        drop(child);
        assert!(
            RepositoryReservation::acquire_in(
                directory.path(),
                workspace.path(),
                RepositoryAccess::Read
            )
            .is_err()
        );
        drop(executing);
        let _released = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Read,
        )?;
        assert!(matches!(
            scope.enter(workspace.path(), RepositoryAccess::Write),
            Err(RepositoryAccessError::Expired(_))
        ));
        Ok(())
    }

    /// Retiring an intermediate borrower cannot resume its parent while a descendant survives.
    #[test]
    fn nested_collection_resumes_only_the_live_ancestor() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let root = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let child = root
            .scope()
            .borrow(workspace.path(), RepositoryAccess::Write)?;
        let stale = child.scope();
        let descendant = stale.borrow(workspace.path(), RepositoryAccess::Read)?;
        drop(child);
        assert!(
            root.scope()
                .enter(workspace.path(), RepositoryAccess::Write)
                .is_err()
        );
        assert!(
            descendant
                .scope()
                .enter(workspace.path(), RepositoryAccess::Read)
                .is_err()
        );
        drop(descendant);
        let _resumed = root
            .scope()
            .enter(workspace.path(), RepositoryAccess::Write)?;
        assert!(matches!(
            stale.borrow(workspace.path(), RepositoryAccess::Read),
            Err(RepositoryAccessError::Expired(_))
        ));
        Ok(())
    }

    /// Reject a foreign workspace and preserve an actual open failure instead of calling it busy.
    #[test]
    fn foreign_scopes_and_io_failures_keep_their_causes() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let foreign = tempfile::tempdir()?;
        let root = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        assert!(matches!(
            root.scope().borrow(foreign.path(), RepositoryAccess::Read),
            Err(RepositoryAccessError::DifferentRepository { .. })
        ));
        let file = directory.path().join("not-a-directory");
        std::fs::write(&file, b"fixture")?;
        assert!(matches!(
            RepositoryReservation::acquire_in(&file, workspace.path(), RepositoryAccess::Read),
            Err(RepositoryAccessError::Io { .. })
        ));
        Ok(())
    }

    /// Race context clones against the same idle owner and grant exactly one child slot.
    #[test]
    fn cloned_scopes_do_not_duplicate_execution_authority() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let root = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let barrier = Arc::new(std::sync::Barrier::new(2));
        std::thread::scope(|threads| {
            let handles = (0..2)
                .map(|_| {
                    let scope = root.scope();
                    let barrier = barrier.clone();
                    let workspace = workspace.path();
                    threads.spawn(move || {
                        barrier.wait();
                        let child = scope.borrow(workspace, RepositoryAccess::Write);
                        barrier.wait();
                        child
                    })
                })
                .collect::<Vec<_>>();
            let results = handles
                .into_iter()
                .map(|handle| handle.join().unwrap())
                .collect::<Vec<_>>();
            assert_eq!(results.iter().filter(|result| result.is_ok()).count(), 1);
            assert_eq!(
                results
                    .iter()
                    .filter(|result| matches!(result, Err(RepositoryAccessError::Busy(_))))
                    .count(),
                1
            );
        });
        Ok(())
    }

    /// Keep both kernel locks after terminal publication fails, then retry without losing ownership.
    #[test]
    fn failed_terminal_record_retains_access_until_publication_recovers() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let root = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let canonical = canonical_workspace(workspace.path())?;
        let key = crate::plan::digest(canonical.to_str().unwrap().as_bytes());
        let record = directory
            .path()
            .join(format!("{key}.operations"))
            .join(format!("{}.json", root.id));
        let backup = record.with_extension("saved");
        std::fs::rename(&record, &backup)?;
        std::fs::create_dir(&record)?;
        drop(root);
        let source = lock_file(&directory.path().join(format!("{key}.lock")))?;
        assert!(matches!(
            source.try_lock_shared(),
            Err(std::fs::TryLockError::WouldBlock)
        ));
        assert!(matches!(
            RepositoryReservation::acquire_in(
                directory.path(),
                workspace.path(),
                RepositoryAccess::Read
            ),
            Err(RepositoryAccessError::Journal(JournalError::Io { .. }))
        ));
        std::fs::remove_dir(&record)?;
        std::fs::rename(&backup, &record)?;
        let next = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Read,
        )?;
        assert!(!record.exists());
        drop(next);
        source.try_lock()?;
        Ok(())
    }

    /// Failed journal publication cannot consume an idle owner's execution or borrowing slot.
    #[test]
    fn failed_admission_record_does_not_grant_or_consume_an_execution_slot() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let root = RepositoryReservation::acquire_in(
            directory.path(),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let canonical = canonical_workspace(workspace.path())?;
        let key = crate::plan::digest(canonical.to_str().unwrap().as_bytes());
        let record = directory
            .path()
            .join(format!("{key}.operations"))
            .join(format!("{}.json", root.id));
        let backup = record.with_extension("saved");
        std::fs::rename(&record, &backup)?;
        std::fs::create_dir(&record)?;
        let scope = root.scope();
        assert!(
            scope
                .enter(workspace.path(), RepositoryAccess::Write)
                .is_err()
        );
        assert!(
            scope
                .borrow(workspace.path(), RepositoryAccess::Read)
                .is_err()
        );
        std::fs::remove_dir(&record)?;
        std::fs::rename(&backup, &record)?;
        let child = scope.borrow(workspace.path(), RepositoryAccess::Read)?;
        let executing = child
            .scope()
            .enter(workspace.path(), RepositoryAccess::Read)?;
        executing.validate()?;
        Ok(())
    }
}
