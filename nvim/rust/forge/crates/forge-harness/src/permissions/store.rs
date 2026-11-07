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
        if !path.exists() {
            let source = serialize_permission_document(&default_permission_document())?;
            atomic_write(&path, &source)?;
        }
        let source = fs::read_to_string(&path)
            .with_context(|| format!("read permission document {}", path.display()))?;
        let document = parse_permission_document(&source)
            .with_context(|| format!("validate permission document {}", path.display()))?;
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

    pub fn save(&mut self, source: &str) -> Result<()> {
        self.save_policy(source, PermissionEdit::Replacement)
    }

    /// Preserve compiled identity for equivalent documents, even when disk formatting changed.
    fn save_policy(&mut self, source: &str, edit: PermissionEdit) -> Result<()> {
        let document = parse_permission_document(source)?;
        let source = serialize_permission_document(&document)?;
        if source == self.source {
            if fs::read_to_string(&self.path).ok().as_deref() != Some(source.as_str()) {
                atomic_write(&self.path, &source)?;
            }
            return Ok(());
        }
        let epoch = match edit {
            PermissionEdit::Replacement => PermissionRevocationEpoch(Arc::new(())),
            PermissionEdit::ApprovedAllow => self.revocation_epoch.clone(),
        };
        let compiled = Arc::new(CompiledPermissionDocument::compile(
            document,
            &self.workspace,
        ));
        atomic_write(&self.path, &source)?;
        self.source = source;
        self.compiled = compiled;
        self.revocation_epoch = epoch;
        Ok(())
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
        let mut document = self.compiled.document.clone();
        for (category, pattern, decision) in rules {
            set_permission_rule(&mut document, category, pattern, *decision)?;
        }
        self.save_policy(
            &serialize_permission_document(&document)?,
            PermissionEdit::ApprovedAllow,
        )
    }

    pub(crate) fn revocation_epoch(&self) -> PermissionRevocationEpoch {
        self.revocation_epoch.clone()
    }

    pub fn set_rule_list(
        &mut self,
        rule_list: &[(String, String, PermissionDecision)],
    ) -> Result<()> {
        let mut document: PermissionDocument = self.compiled.document.clone();
        for (category, pattern, decision) in rule_list {
            set_permission_rule(&mut document, category, pattern, *decision)?;
        }
        self.save(&serialize_permission_document(&document)?)
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

fn atomic_write(path: &Path, source: &str) -> Result<()> {
    let temporary = path.with_extension(format!("json.{}.tmp", std::process::id()));
    let backup = path.with_extension(format!("json.{}.bak", std::process::id()));
    fs::write(&temporary, source).with_context(|| {
        format!(
            "write temporary permission document {}",
            temporary.display()
        )
    })?;
    if path.exists() {
        fs::rename(path, &backup)
            .with_context(|| format!("backup permission document {}", path.display()))?;
    }
    if let Err(error) = fs::rename(&temporary, path) {
        if backup.exists() {
            let _ = fs::rename(&backup, path);
        }
        return Err(error)
            .with_context(|| format!("replace permission document {}", path.display()));
    }
    if backup.exists() {
        fs::remove_file(&backup)
            .with_context(|| format!("remove permission backup {}", backup.display()))?;
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::permissions::document::PermissionDecision;

    #[test]
    fn semantically_unchanged_save_repairs_disk_without_revoking_snapshot() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let path = directory.path().join("permissions.json");
        let mut store = PermissionStore::load(&path, directory.path())?;
        let source = store.open().1;
        let policy = store.compiled();
        let epoch = store.revocation_epoch();
        fs::write(&path, format!("\n{source}"))?;
        store.save(&source)?;
        assert!(Arc::ptr_eq(&policy, &store.compiled()));
        assert_eq!(epoch, store.revocation_epoch());
        assert_eq!(fs::read_to_string(path)?, source);
        Ok(())
    }

    #[test]
    fn approval_grant_rejects_mixed_or_denied_rules() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let mut store =
            PermissionStore::load(directory.path().join("permissions.json"), directory.path())?;
        let before = store.open().1;
        let epoch = store.revocation_epoch();
        assert!(
            store
                .grant_approved(&[
                    ("bash".into(), "rg *".into(), PermissionDecision::Allow),
                    ("read".into(), "private/**".into(), PermissionDecision::Deny),
                ])
                .is_err()
        );
        assert_eq!(store.open().1, before);
        assert_eq!(store.revocation_epoch(), epoch);
        Ok(())
    }
}
