use super::command::normalize_command;
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
        })
    }

    pub fn open(&self) -> (String, String) {
        (
            self.path.to_string_lossy().into_owned(),
            self.source.clone(),
        )
    }

    pub fn save(&mut self, source: &str) -> Result<()> {
        let document = parse_permission_document(source)?;
        let source = serialize_permission_document(&document)?;
        let compiled = Arc::new(CompiledPermissionDocument::compile(
            document,
            &self.workspace,
        ));
        atomic_write(&self.path, &source)?;
        self.source = source;
        self.compiled = compiled;
        Ok(())
    }

    pub fn set_rule_list(
        &mut self,
        rule_list: &[(String, String)],
        decision: PermissionDecision,
    ) -> Result<()> {
        let mut document: PermissionDocument = self.compiled.document.clone();
        for (category, pattern) in rule_list {
            set_permission_rule(&mut document, category, pattern, decision)?;
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

    pub fn protects_command(&self, command: &str) -> bool {
        normalize_command(command)
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
    fn swaps_only_valid_documents_and_persists_replaced_rules() {
        let directory = tempfile::tempdir().unwrap();
        let path = directory.path().join("permissions.json");
        let mut store = PermissionStore::load(&path, "D:/repo").unwrap();
        let original = store.open().1;
        assert!(store.save("not json").is_err());
        assert_eq!(fs::read_to_string(&path).unwrap(), original);
        store
            .set_rule_list(
                &[("bash".into(), "git commit".into())],
                PermissionDecision::Deny,
            )
            .unwrap();
        assert_eq!(
            store.compiled().document.permission["bash"]["git commit"],
            PermissionDecision::Deny
        );
    }
}
