use super::document::{
    PermissionDecision, PermissionDocument, default_permission_document, parse_permission_document,
    serialize_permission_document, set_permission_rule,
};
use super::matcher::CompiledPermissionDocument;
use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Owns the durable permission document and its compiled evaluator.
#[derive(Debug)]
pub struct PermissionStore {
    path: PathBuf,
    workspace: PathBuf,
    source: String,
    compiled: Arc<CompiledPermissionDocument>,
    revocation_epoch: PermissionRevocationEpoch,
}

/// Changes whenever a policy edit can revoke a captured callback's authority.
#[derive(Clone, Debug)]
pub(crate) struct PermissionRevocationEpoch(Arc<()>);

impl PartialEq for PermissionRevocationEpoch {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for PermissionRevocationEpoch {}

enum PermissionEdit {
    Replacement,
    ApprovedAllow,
}

impl PermissionStore {
    pub fn load(path: impl Into<PathBuf>, workspace: impl Into<PathBuf>) -> Result<Self> {
        let path = path.into();
        let workspace = workspace.into();
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("create permission directory {}", parent.display()))?;
        }
        let document = {
            let _writer = permission_writer_lock(&path)?;
            match read_disk_document(&path)? {
                Some(document) => document,
                None => {
                    let document = default_permission_document();
                    atomic_write(&path, &serialize_permission_document(&document)?)?;
                    document
                }
            }
        };
        let source = serialize_permission_document(&document)?;
        let compiled = Arc::new(CompiledPermissionDocument::compile(document, &workspace));
        Ok(Self {
            path,
            workspace,
            source,
            compiled,
            revocation_epoch: PermissionRevocationEpoch(Arc::new(())),
        })
    }

    pub fn open(&self) -> (String, String) {
        (
            self.path.to_string_lossy().into_owned(),
            self.source.clone(),
        )
    }

    /// Replace the whole document with an edited source, as `:ForgePermissions` does.
    ///
    /// A replacement is the user's complete intended policy, so it does not merge with disk.
    pub fn save(&mut self, source: &str) -> Result<()> {
        let document = parse_permission_document(source)?;
        let _writer = permission_writer_lock(&self.path)?;
        let disk = read_disk_document(&self.path)?;
        self.commit(document, disk, PermissionEdit::Replacement)
    }

    /// Install `document` in memory and on disk while the caller holds the writer lock.
    ///
    /// `disk` is the document read under that lock. Equivalent documents keep compiled identity,
    /// even when disk formatting changed. An approved allow keeps the revocation epoch only when
    /// it extended exactly the policy this store already enforced; when another process changed
    /// the file, the merged result may be stricter, so captured callbacks are revoked.
    fn commit(
        &mut self,
        document: PermissionDocument,
        disk: Option<PermissionDocument>,
        edit: PermissionEdit,
    ) -> Result<()> {
        let source = serialize_permission_document(&document)?;
        let disk_matches = disk.as_ref() == Some(&document)
            && fs::read_to_string(&self.path).ok().as_deref() == Some(source.as_str());
        if !disk_matches {
            atomic_write(&self.path, &source)?;
        }
        if source == self.source {
            return Ok(());
        }
        let extends_enforced = disk.as_ref() == Some(&self.compiled.document);
        let epoch = match edit {
            PermissionEdit::ApprovedAllow if extends_enforced => self.revocation_epoch.clone(),
            PermissionEdit::ApprovedAllow | PermissionEdit::Replacement => {
                PermissionRevocationEpoch(Arc::new(()))
            }
        };
        self.compiled = Arc::new(CompiledPermissionDocument::compile(
            document,
            &self.workspace,
        ));
        self.source = source;
        self.revocation_epoch = epoch;
        Ok(())
    }

    /// Apply exact rules to the document currently on disk, so rules saved by another editor
    /// sharing the file survive, and install the merged result.
    fn merge_rules(
        &mut self,
        rule_list: &[(String, String, PermissionDecision)],
        edit: PermissionEdit,
    ) -> Result<()> {
        let _writer = permission_writer_lock(&self.path)?;
        let disk = read_disk_document(&self.path)?;
        let mut document = disk
            .clone()
            .unwrap_or_else(|| self.compiled.document.clone());
        for (category, pattern, decision) in rule_list {
            set_permission_rule(&mut document, category, pattern, *decision)?;
        }
        self.commit(document, disk, edit)
    }

    /// Commit allow-only rules from a validated approval without revoking stricter captured policies.
    pub(crate) fn grant_approved(
        &mut self,
        rules: &[(String, String, PermissionDecision)],
    ) -> Result<()> {
        anyhow::ensure!(
            !rules.is_empty()
                && rules
                    .iter()
                    .all(|(_, _, decision)| *decision == PermissionDecision::Allow),
            "approved permission grant must contain only allow decisions"
        );
        self.merge_rules(rules, PermissionEdit::ApprovedAllow)
    }

    pub(crate) fn revocation_epoch(&self) -> PermissionRevocationEpoch {
        self.revocation_epoch.clone()
    }

    pub fn set_rule_list(
        &mut self,
        rule_list: &[(String, String, PermissionDecision)],
    ) -> Result<()> {
        self.merge_rules(rule_list, PermissionEdit::Replacement)
    }

    pub fn compiled(&self) -> Arc<CompiledPermissionDocument> {
        Arc::clone(&self.compiled)
    }

    pub fn protects(&self, path: &Path) -> bool {
        let path = path
            .to_string_lossy()
            .replace('\\', "/")
            .to_ascii_lowercase();
        let protected = self
            .path
            .to_string_lossy()
            .replace('\\', "/")
            .to_ascii_lowercase();
        path == protected
    }

    pub fn protects_command_in(&self, command: &str, shell: super::shell::CommandShell) -> bool {
        super::command::normalize_command_in(command, shell)
            .invocation_list
            .iter()
            .any(|invocation| {
                invocation.token_list.iter().skip(1).any(|token| {
                    let token = token.trim_matches(['"', '\'', '(', ')', '[', ']', '{', '}', ',']);
                    if token.is_empty() || token.starts_with('-') {
                        return false;
                    }
                    let path = Path::new(token);
                    let candidate = if path.is_absolute() {
                        path.to_path_buf()
                    } else {
                        self.workspace.join(path)
                    };
                    self.protects(&candidate)
                })
            })
    }
}

/// Serialize read-merge-write cycles across every host that shares the permission document.
///
/// The lock file sits beside the document rather than replacing it, because the document itself
/// is swapped by rename and a lock on a renamed-away inode would exclude nobody.
fn permission_writer_lock(path: &Path) -> Result<fs::File> {
    crate::storage::exclusive_file_lock(
        &path.with_extension("json.lock"),
        "permission document writer",
    )
}

/// Read and validate the document on disk, or `None` when no document exists yet.
///
/// An invalid document is an error rather than an absent one, so a hand-edited file with a
/// mistake is reported and never overwritten by a merge.
fn read_disk_document(path: &Path) -> Result<Option<PermissionDocument>> {
    let source = match fs::read_to_string(path) {
        Ok(source) => source,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
        Err(error) => {
            return Err(error)
                .with_context(|| format!("read permission document {}", path.display()));
        }
    };
    parse_permission_document(&source)
        .map(Some)
        .with_context(|| format!("validate permission document {}", path.display()))
}

/// Replace the document with one rename, so a concurrent reader sees either the old or the new
/// document and never a missing file. Both platforms' `rename` replace an existing destination.
fn atomic_write(path: &Path, source: &str) -> Result<()> {
    let temporary = path.with_extension(format!("json.{}.tmp", std::process::id()));
    let write_temporary = || -> std::io::Result<()> {
        let mut file = fs::File::create(&temporary)?;
        std::io::Write::write_all(&mut file, source.as_bytes())?;
        file.sync_all()
    };
    if let Err(error) = write_temporary() {
        let _ = fs::remove_file(&temporary);
        return Err(error).with_context(|| {
            format!(
                "write temporary permission document {}",
                temporary.display()
            )
        });
    }
    if let Err(error) = fs::rename(&temporary, path) {
        let _ = fs::remove_file(&temporary);
        return Err(error)
            .with_context(|| format!("replace permission document {}", path.display()));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::permissions::document::{CATEGORY_BASH, CATEGORY_TOOL};

    fn rule(
        category: &str,
        pattern: &str,
        decision: PermissionDecision,
    ) -> (String, String, PermissionDecision) {
        (category.into(), pattern.into(), decision)
    }

    fn disk_rule(path: &Path, category: &str, pattern: &str) -> Option<PermissionDecision> {
        read_disk_document(path)
            .unwrap()
            .unwrap()
            .permission
            .get(category)
            .and_then(|rule_map| rule_map.get(pattern))
            .copied()
    }

    /// Two hosts sharing one document keep each other's grants instead of saving private snapshots.
    #[test]
    fn grants_from_two_hosts_merge_into_the_shared_document() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("permissions.json");
        let mut first_host = PermissionStore::load(&path, directory.path())?;
        let mut second_host = PermissionStore::load(&path, directory.path())?;

        first_host.grant_approved(&[rule(CATEGORY_TOOL, "first", PermissionDecision::Allow)])?;
        second_host.grant_approved(&[rule(CATEGORY_TOOL, "second", PermissionDecision::Allow)])?;

        assert_eq!(
            disk_rule(&path, CATEGORY_TOOL, "first"),
            Some(PermissionDecision::Allow)
        );
        assert_eq!(
            disk_rule(&path, CATEGORY_TOOL, "second"),
            Some(PermissionDecision::Allow)
        );
        assert_eq!(
            second_host.compiled().document.permission[CATEGORY_TOOL].get("first"),
            Some(&PermissionDecision::Allow)
        );
        Ok(())
    }

    /// A rule tightened to deny by one host is not resurrected by another host's later grant, and
    /// the grant revokes captured authority because the enforced policy became stricter.
    #[test]
    fn a_denial_saved_by_another_host_survives_a_later_grant() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("permissions.json");
        let mut first_host = PermissionStore::load(&path, directory.path())?;
        let mut second_host = PermissionStore::load(&path, directory.path())?;
        first_host.set_rule_list(&[rule(CATEGORY_BASH, "git push", PermissionDecision::Allow)])?;
        second_host.set_rule_list(&[rule(CATEGORY_BASH, "cargo", PermissionDecision::Allow)])?;
        first_host.set_rule_list(&[rule(CATEGORY_BASH, "git push", PermissionDecision::Deny)])?;
        let epoch_before_grant = second_host.revocation_epoch();

        second_host.grant_approved(&[rule(CATEGORY_TOOL, "probe", PermissionDecision::Allow)])?;

        assert_eq!(
            disk_rule(&path, CATEGORY_BASH, "git push"),
            Some(PermissionDecision::Deny)
        );
        assert_eq!(
            disk_rule(&path, CATEGORY_BASH, "cargo"),
            Some(PermissionDecision::Allow)
        );
        assert!(second_host.revocation_epoch() != epoch_before_grant);
        Ok(())
    }

    /// A grant that only extends the enforced policy keeps captured callbacks valid.
    #[test]
    fn an_uncontended_grant_keeps_the_revocation_epoch() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("permissions.json");
        let mut store = PermissionStore::load(&path, directory.path())?;
        let epoch_before_grant = store.revocation_epoch();

        store.grant_approved(&[rule(CATEGORY_TOOL, "probe", PermissionDecision::Allow)])?;

        assert!(store.revocation_epoch() == epoch_before_grant);
        Ok(())
    }

    /// An invalid hand-edited document is reported, never replaced by a merged snapshot.
    #[test]
    fn a_merge_refuses_to_overwrite_an_invalid_document() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("permissions.json");
        let mut store = PermissionStore::load(&path, directory.path())?;
        fs::write(&path, "{ not json")?;

        let failure = store
            .grant_approved(&[rule(CATEGORY_TOOL, "probe", PermissionDecision::Allow)])
            .unwrap_err();

        assert!(format!("{failure:#}").contains("validate permission document"));
        assert_eq!(fs::read_to_string(&path)?, "{ not json");
        Ok(())
    }
}
