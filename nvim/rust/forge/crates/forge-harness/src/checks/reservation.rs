use crate::repository_access::{
    RepositoryAccess, RepositoryExecution, RepositoryReservation, RepositoryScope,
};
use anyhow::Result;
use std::{
    path::Path,
    sync::{Arc, Mutex},
};

/// Share a source writer's execution until its physical change is durably recorded.
#[derive(Clone, Debug)]
pub(crate) struct MutationReservation(Arc<Mutex<Option<CheckReservation>>>);

impl MutationReservation {
    /// Acquire independent write access before a source transaction can commit.
    pub(crate) fn acquire(root: &Path, workspace: &Path) -> Result<Self> {
        Self::acquire_scoped(root, workspace, None)
    }

    /// Borrow the caller's write protection, or acquire independent access when no owner exists.
    pub(crate) fn acquire_scoped(
        root: &Path,
        workspace: &Path,
        scope: Option<&RepositoryScope>,
    ) -> Result<Self> {
        Ok(Self(Arc::new(Mutex::new(Some(
            CheckReservation::acquire_scoped(root, workspace, super::CheckPurpose::Prepare, scope)?,
        )))))
    }

    /// Reject a revoked transaction before its next physical write.
    pub(crate) fn validate(&self) -> Result<()> {
        self.0
            .lock()
            .map_err(|_| anyhow::anyhow!("mutation reservation lock poisoned"))?
            .as_ref()
            .ok_or_else(|| {
                anyhow::anyhow!("repository_scope_expired: mutation was already recorded")
            })?
            .execution
            .validate()?;
        Ok(())
    }

    /// Release every retained copy after the broker commits the physical change record.
    pub(crate) fn recorded(&self) -> Result<()> {
        let mut reservation = self
            .0
            .lock()
            .map_err(|_| anyhow::anyhow!("mutation reservation lock poisoned"))?;
        if let Some(owned) = reservation.as_ref() {
            owned.published()?;
        }
        reservation.take();
        Ok(())
    }
}

/// Retain a check's source access through process collection and result publication.
#[derive(Debug)]
pub(crate) struct CheckReservation {
    execution: RepositoryExecution,
    _owner: RepositoryReservation,
}

impl CheckReservation {
    /// Keep admitted ownership attached to each spawned process worker.
    pub(crate) fn process_owner(&self) -> Arc<dyn forge_shell::process::ProcessOwner> {
        self.execution.process_owner()
    }

    /// Mark the retained result durable before releasing its source reservation.
    pub(crate) fn published(&self) -> Result<()> {
        Ok(self.execution.published()?)
    }

    /// Retain a publication failure independently of prunable display history.
    pub(crate) fn unresolved(&self, reason: String) -> Result<()> {
        Ok(self.execution.unresolved(reason)?)
    }

    /// Admit verification alongside readers, or preparation only with no competing owner.
    pub(crate) fn acquire(
        root: &Path,
        workspace: &Path,
        purpose: super::CheckPurpose,
    ) -> Result<Self> {
        Self::acquire_scoped(root, workspace, purpose, None)
    }

    /// Suspend an existing caller while this check owns its descendant execution slot.
    pub(crate) fn acquire_scoped(
        root: &Path,
        workspace: &Path,
        purpose: super::CheckPurpose,
        scope: Option<&RepositoryScope>,
    ) -> Result<Self> {
        let access = match purpose {
            super::CheckPurpose::Verify => RepositoryAccess::Read,
            super::CheckPurpose::Prepare => RepositoryAccess::Write,
        };
        let owner = match scope {
            Some(scope) => scope.borrow(workspace, access)?,
            None => RepositoryReservation::acquire(root, workspace, access)?,
        };
        let execution = owner.scope().enter(workspace, access)?;
        Ok(Self {
            execution,
            _owner: owner,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Borrowing a check suspends its source writer without conflicting with its own file lock.
    #[test]
    fn borrowed_checks_hold_the_parent_until_their_result_is_recorded() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let owner = RepositoryReservation::acquire(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let scope = owner.scope();
        let verification = CheckReservation::acquire_scoped(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            super::super::CheckPurpose::Verify,
            Some(&scope),
        )?;
        assert!(
            scope
                .enter(workspace.path(), RepositoryAccess::Write)
                .is_err()
        );
        assert!(
            CheckReservation::acquire(
                &(workspace.path()).join("harness-state"),
                workspace.path(),
                super::super::CheckPurpose::Verify
            )
            .is_err()
        );
        drop(verification);
        let mutation = MutationReservation::acquire_scoped(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            Some(&scope),
        )?;
        let copy = mutation.clone();
        mutation.validate()?;
        assert!(
            scope
                .enter(workspace.path(), RepositoryAccess::Write)
                .is_err()
        );
        mutation.recorded()?;
        assert!(copy.validate().is_err());
        let _resumed = scope.enter(workspace.path(), RepositoryAccess::Write)?;
        Ok(())
    }

    /// An expired supplied owner cannot silently fall back to a new independent reservation.
    #[test]
    fn expired_borrow_does_not_fall_back_to_independent_access() -> Result<()> {
        let workspace = tempfile::tempdir()?;
        let owner = RepositoryReservation::acquire(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            RepositoryAccess::Write,
        )?;
        let scope = owner.scope();
        drop(owner);
        let error = MutationReservation::acquire_scoped(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
            Some(&scope),
        )
        .unwrap_err();
        assert!(matches!(
            error.downcast_ref::<crate::repository_access::RepositoryAccessError>(),
            Some(crate::repository_access::RepositoryAccessError::Expired(_))
        ));
        let _independent = MutationReservation::acquire(
            &(workspace.path()).join("harness-state"),
            workspace.path(),
        )?;
        Ok(())
    }
}
