use std::collections::BTreeMap;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use super::mutation_error::{MutationError, Result};
use super::service::LspScope;
use crate::permissions::{
    directory::DirectoryPolicy,
    document::PermissionDecision,
    matcher::{PermissionRequest, PermissionTarget},
};

/// Carry a complete, unpaginated editor preparation into the filesystem owner.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Prepared {
    operations: Vec<Operation>,
}

/// Preserve resource ordering and exact source preconditions from the editor.
#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
enum Operation {
    Text {
        path: String,
        before: String,
        after: String,
        edits: u32,
    },
    Create {
        path: String,
        #[serde(default)]
        overwrite: bool,
        #[serde(default)]
        ignore_if_exists: bool,
    },
    Rename {
        old_path: String,
        new_path: String,
        #[serde(default)]
        overwrite: bool,
        #[serde(default)]
        ignore_if_exists: bool,
    },
    Delete {
        path: String,
        #[serde(default)]
        recursive: bool,
        #[serde(default)]
        ignore_if_not_exists: bool,
    },
}

/// Retain bytes and modes for freshness checks and complete rollback.
#[derive(Clone)]
struct Node {
    path: PathBuf,
    bytes: Option<Vec<u8>>,
    permissions: fs::Permissions,
}

/// Own all prepared mutations and their recovery image until commit settles.
pub(super) struct Transaction {
    before: BTreeMap<String, Node>,
    after: BTreeMap<String, Node>,
    roots: Vec<PathBuf>,
    edits: u64,
    moves: Vec<(PathBuf, PathBuf)>,
}

/// Resolve paths without permitting symlinks to redirect a mutation.
pub(super) fn authorize(
    scope: &LspScope,
    policy: &DirectoryPolicy,
    path: &str,
    write: bool,
) -> Result<PathBuf> {
    scope.check().map_err(MutationError::Service)?;
    let supplied = Path::new(path);
    let absolute = if supplied.is_absolute() {
        supplied.to_owned()
    } else {
        scope.workspace.join(supplied)
    };
    let mut cursor = PathBuf::new();
    for component in absolute.components() {
        cursor.push(component);
        match fs::symlink_metadata(&cursor) {
            Ok(metadata) if metadata.file_type().is_symlink() => {
                return Err(MutationError::UnsupportedPath(cursor));
            }
            Ok(_) => {}
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
            Err(error) => return Err(error.into()),
        }
    }
    let resolved = crate::permissions::directory::resolve_path(&absolute, &scope.workspace)
        .map_err(MutationError::Service)?;
    if write
        && scope
            .protected_path
            .read()
            .map_err(|_| MutationError::Poisoned)?
            .as_ref()
            .is_some_and(|protected| inside(protected, &resolved))
    {
        return Err(MutationError::PathDenied(resolved));
    }
    let path = resolved.to_string_lossy().into_owned();
    let mut request = PermissionRequest {
        id: scope.interaction.clone(),
        provider: forge_provider_api::ProviderId::new("harness-lsp")
            .expect("static provider identity is valid"),
        reason: None,
        target_list: vec![PermissionTarget::Read { path: path.clone() }],
    };
    if write {
        request.target_list.push(PermissionTarget::Write { path });
    }
    if policy
        .evaluate(&scope.policy, &mut request)
        .map_err(MutationError::Service)?
        .decision
        != PermissionDecision::Allow
    {
        return Err(MutationError::PathDenied(resolved));
    }
    Ok(resolved)
}

impl Transaction {
    /// Validate every path and replay the complete operation sequence without changing files.
    pub(super) fn prepare(
        scope: &LspScope,
        policy: &DirectoryPolicy,
        prepared: Prepared,
    ) -> Result<Self> {
        let mut roots = Vec::new();
        for operation in &prepared.operations {
            let paths = match operation {
                Operation::Text { path, .. }
                | Operation::Create { path, .. }
                | Operation::Delete { path, .. } => vec![path],
                Operation::Rename {
                    old_path, new_path, ..
                } => vec![old_path, new_path],
            };
            for path in paths {
                let path = authorize(scope, policy, path, true)?;
                let workspace =
                    crate::permissions::directory::resolve_path(&scope.workspace, &scope.workspace)
                        .map_err(MutationError::Service)?;
                if workspace.starts_with(&path) {
                    return Err(MutationError::UnsupportedPath(path));
                }
                roots.push(path);
            }
        }
        roots.sort_by_key(|path| path.components().count());
        let mut distinct = Vec::<PathBuf>::new();
        for path in roots {
            if !distinct.iter().any(|root| inside(&path, root)) {
                distinct.push(path);
            }
        }
        let mut before = BTreeMap::new();
        for root in &distinct {
            capture(root, &mut before, &mut |path| {
                authorize(scope, policy, &path.to_string_lossy(), true).map(|_| ())
            })?;
        }
        let mut after = before.clone();
        let mut edits = 0;
        let mut moves = Vec::new();
        for operation in prepared.operations {
            match operation {
                Operation::Text {
                    path,
                    before,
                    after: contents,
                    edits: count,
                } => {
                    let path = authorize(scope, policy, &path, true)?;
                    let node = after
                        .get_mut(&key(&path))
                        .ok_or_else(|| MutationError::PathMissing(path.clone()))?;
                    if node.bytes.as_deref() != Some(before.as_bytes()) {
                        return Err(MutationError::SourceChanged(path));
                    }
                    node.bytes = Some(contents.into_bytes());
                    edits += u64::from(count);
                }
                Operation::Create {
                    path,
                    overwrite,
                    ignore_if_exists,
                } => {
                    let path = authorize(scope, policy, &path, true)?;
                    if let Some(node) = after.get(&key(&path)) {
                        if !overwrite && ignore_if_exists {
                            continue;
                        }
                        if !overwrite || node.bytes.is_none() {
                            return Err(MutationError::DestinationExists(path));
                        }
                    }
                    require_parent(&path, &after, &before)?;
                    let permissions = tempfile::tempfile()?.metadata()?.permissions();
                    after.insert(
                        key(&path),
                        Node {
                            path,
                            bytes: Some(Vec::new()),
                            permissions,
                        },
                    );
                }
                Operation::Rename {
                    old_path,
                    new_path,
                    overwrite,
                    ignore_if_exists,
                } => {
                    let old = authorize(scope, policy, &old_path, true)?;
                    // Preserve requested casing: canonicalization would hide a Windows case-only rename.
                    let new = absolute_destination(scope, &new_path)?;
                    authorize(scope, policy, &new_path, true)?;
                    if !after.contains_key(&key(&old)) {
                        return Err(MutationError::PathMissing(old));
                    }
                    same_filesystem(&old, &new)?;
                    if inside(&new, &old) && key(&old) != key(&new) {
                        return Err(MutationError::InvalidEdit(
                            "cannot move a directory into itself",
                        ));
                    }
                    if key(&old) != key(&new) && after.contains_key(&key(&new)) {
                        if !overwrite && ignore_if_exists {
                            continue;
                        }
                        if !overwrite {
                            return Err(MutationError::DestinationExists(new));
                        }
                        after.retain(|_, node| !inside(&node.path, &new));
                    }
                    require_parent(&new, &after, &before)?;
                    moves.push((old.clone(), new.clone()));
                    let moving: Vec<_> = after
                        .values()
                        .filter(|node| inside(&node.path, &old))
                        .cloned()
                        .collect();
                    after.retain(|_, node| !inside(&node.path, &old));
                    for mut node in moving {
                        let relative = node
                            .path
                            .strip_prefix(&old)
                            .map_err(|_| MutationError::InvalidEdit("inconsistent move source"))?;
                        node.path = if relative.as_os_str().is_empty() {
                            new.clone()
                        } else {
                            new.join(relative)
                        };
                        authorize(scope, policy, &node.path.to_string_lossy(), true)?;
                        after.insert(key(&node.path), node);
                    }
                }
                Operation::Delete {
                    path,
                    recursive,
                    ignore_if_not_exists,
                } => {
                    let path = authorize(scope, policy, &path, true)?;
                    if !after.contains_key(&key(&path)) {
                        if ignore_if_not_exists {
                            continue;
                        }
                        return Err(MutationError::PathMissing(path));
                    }
                    if !recursive
                        && after
                            .values()
                            .any(|node| key(&node.path) != key(&path) && inside(&node.path, &path))
                    {
                        return Err(MutationError::InvalidEdit(
                            "nonrecursive deletion of a nonempty directory",
                        ));
                    }
                    after.retain(|_, node| !inside(&node.path, &path));
                }
            }
        }
        // Preflight the final destinations as well as the server-reported roots.
        for node in after.values() {
            authorize(scope, policy, &node.path.to_string_lossy(), true)?;
        }
        Ok(Self {
            before,
            after,
            roots: distinct,
            edits,
            moves,
        })
    }

    /// Commit staged content after a final disk freshness check, rolling back every write on failure.
    pub(super) fn commit(&mut self) -> Result<()> {
        let mut current = BTreeMap::new();
        for root in &self.roots {
            capture(root, &mut current, &mut |_| Ok(()))?;
        }
        if !same(&current, &self.before) {
            return Err(MutationError::SourceChanged(
                self.roots.first().cloned().unwrap_or_default(),
            ));
        }
        if let Err(cause) = reconcile(&self.before, &self.after) {
            let mut partial = BTreeMap::new();
            for root in &self.roots {
                if let Err(error) = capture(root, &mut partial, &mut |_| Ok(())) {
                    return Err(MutationError::Rollback {
                        path: root.display().to_string(),
                        rollback: std::io::Error::other(error),
                        cause: Box::new(cause),
                    });
                }
            }
            if let Err(error) = reconcile(&partial, &self.before) {
                return Err(MutationError::Rollback {
                    path: self
                        .roots
                        .iter()
                        .map(|path| path.display().to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    rollback: std::io::Error::other(error),
                    cause: Box::new(cause),
                });
            }
            return Err(cause);
        }
        Ok(())
    }

    /// Report whether the transaction changes any resource or bytes.
    pub(super) fn changed(&self) -> bool {
        !same(&self.before, &self.after)
    }

    /// Describe net file changes for the existing timeline and checkpoint attribution.
    pub(super) fn changes(&self) -> forge_provider_api::ProviderChangeSet {
        use forge_provider_api::{ProviderChangeKind, ProviderFileChange};
        let mut file = Vec::new();
        let mut moved = std::collections::BTreeSet::new();
        for node in self.before.values() {
            if node.bytes.is_none() {
                continue;
            }
            let mut destination = node.path.clone();
            for (old, new) in &self.moves {
                if let Ok(relative) = destination.strip_prefix(old) {
                    destination = if relative.as_os_str().is_empty() {
                        new.clone()
                    } else {
                        new.join(relative)
                    };
                }
            }
            let destination_key = key(&destination);
            let after = self
                .after
                .get(&destination_key)
                .filter(|node| node.bytes.is_some());
            if after.is_some_and(|after| same_node(node, after)) {
                continue;
            }
            let before_text = String::from_utf8_lossy(node.bytes.as_deref().unwrap_or_default());
            let next = after
                .map(|node| String::from_utf8_lossy(node.bytes.as_deref().unwrap_or_default()))
                .unwrap_or_default();
            let moved_to = after.filter(|after| after.path != node.path);
            let kind = if moved_to.is_some() {
                ProviderChangeKind::Move
            } else if after.is_some() {
                ProviderChangeKind::Update
            } else {
                ProviderChangeKind::Delete
            };
            if moved_to.is_some() {
                moved.insert(destination_key);
            }
            file.push(ProviderFileChange {
                path: node.path.to_string_lossy().into_owned(),
                move_path: moved_to.map(|after| after.path.to_string_lossy().into_owned()),
                kind,
                diff: similar::TextDiff::from_lines(before_text.as_ref(), next.as_ref())
                    .unified_diff()
                    .to_string(),
            });
        }
        for (key, node) in &self.after {
            if moved.contains(key)
                || self
                    .before
                    .get(key)
                    .is_some_and(|node| node.bytes.is_some())
                || node.bytes.is_none()
            {
                continue;
            }
            let next = String::from_utf8_lossy(node.bytes.as_deref().unwrap_or_default());
            file.push(ProviderFileChange {
                path: node.path.to_string_lossy().into_owned(),
                move_path: None,
                kind: ProviderChangeKind::Add,
                diff: similar::TextDiff::from_lines("", next.as_ref())
                    .unified_diff()
                    .to_string(),
            });
        }
        forge_provider_api::ProviderChangeSet { file }
    }

    /// Summarize committed changes without paginating or truncating the mutation itself.
    pub(super) fn summary(&self) -> String {
        let mut output = format!("Renamed: {} text edits.\n", self.edits);
        for (old, new) in &self.moves {
            if output.len() < 6000 {
                output.push_str(&format!("moved: {} -> {}\n", old.display(), new.display()));
            }
        }
        let mut count = 0;
        for (key, before) in &self.before {
            match self.after.get(key) {
                None => {
                    count += 1;
                    if output.len() < 6000 {
                        output.push_str(&format!("deleted: {}\n", before.path.display()));
                    }
                }
                Some(after) if !same_node(before, after) => {
                    count += 1;
                    if output.len() < 6000 {
                        output.push_str(&format!("updated: {}\n", after.path.display()));
                    }
                }
                Some(_) => {}
            }
        }
        for (key, after) in &self.after {
            if !self.before.contains_key(key) {
                count += 1;
                if output.len() < 6000 {
                    output.push_str(&format!("created: {}\n", after.path.display()));
                }
            }
        }
        if count == 0 {
            return "No changes required.".into();
        }
        output.push_str(&format!("{count} resources changed."));
        output
    }
}

/// Preserve destination spelling while resolving its existing parent.
fn absolute_destination(scope: &LspScope, path: &str) -> Result<PathBuf> {
    let path = Path::new(path);
    let path = if path.is_absolute() {
        path.to_owned()
    } else {
        scope.workspace.join(path)
    };
    let parent = path
        .parent()
        .ok_or(MutationError::InvalidEdit("destination has no parent"))?;
    let parent = crate::permissions::directory::resolve_path(parent, &scope.workspace)
        .map_err(MutationError::Service)?;
    Ok(parent.join(
        path.file_name()
            .ok_or(MutationError::InvalidEdit("destination has no name"))?,
    ))
}

/// Reject cross-volume moves before copying or deleting any resource.
fn same_filesystem(old: &Path, new: &Path) -> Result<()> {
    let mut parent = new
        .parent()
        .ok_or(MutationError::InvalidEdit("destination has no parent"))?;
    while !parent.exists() {
        parent = parent.parent().ok_or(MutationError::InvalidEdit(
            "destination has no existing ancestor",
        ))?;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::MetadataExt;
        let mut existing = old;
        while !existing.exists() {
            existing = existing.parent().ok_or(MutationError::InvalidEdit(
                "source has no existing ancestor",
            ))?;
        }
        if fs::metadata(existing)?.dev() != fs::metadata(parent)?.dev() {
            return Err(MutationError::UnsupportedPath(new.to_owned()));
        }
    }
    #[cfg(windows)]
    {
        if old.components().next() != parent.components().next() {
            return Err(MutationError::UnsupportedPath(new.to_owned()));
        }
    }
    Ok(())
}

/// Compare Windows paths case-insensitively without losing their display spelling.
fn key(path: &Path) -> String {
    let value = path.to_string_lossy().replace('\\', "/");
    if cfg!(windows) {
        value.to_lowercase()
    } else {
        value
    }
}

/// Compare complete path components rather than granting prefix siblings.
fn inside(path: &Path, root: &Path) -> bool {
    let path = key(path);
    let root = key(root);
    path == root || path.starts_with(&(root.trim_end_matches('/').to_owned() + "/"))
}

/// Require a real or staged directory for newly created resources.
fn require_parent(
    path: &Path,
    staged: &BTreeMap<String, Node>,
    original: &BTreeMap<String, Node>,
) -> Result<()> {
    let parent = path
        .parent()
        .ok_or(MutationError::InvalidEdit("path has no parent"))?;
    if staged
        .get(&key(parent))
        .is_some_and(|node| node.bytes.is_none())
        || (!original.contains_key(&key(parent)) && parent.is_dir())
    {
        Ok(())
    } else {
        Err(MutationError::PathMissing(parent.to_owned()))
    }
}

/// Capture a complete authorized resource tree without following symlinks.
fn capture(
    path: &Path,
    nodes: &mut BTreeMap<String, Node>,
    authorize: &mut impl FnMut(&Path) -> Result<()>,
) -> Result<()> {
    authorize(path)?;
    let metadata = match fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
        Err(error) => return Err(error.into()),
    };
    if metadata.file_type().is_symlink() || (!metadata.is_dir() && !metadata.is_file()) {
        return Err(MutationError::UnsupportedPath(path.to_owned()));
    }
    let bytes = if metadata.is_file() {
        Some(fs::read(path)?)
    } else {
        None
    };
    nodes.insert(
        key(path),
        Node {
            path: path.to_owned(),
            bytes,
            permissions: metadata.permissions(),
        },
    );
    if metadata.is_dir() {
        for entry in fs::read_dir(path)? {
            capture(&entry?.path(), nodes, authorize)?;
        }
    }
    Ok(())
}

/// Compare captured content and Unix modes without depending on timestamps.
fn same_node(left: &Node, right: &Node) -> bool {
    let same = left.path == right.path
        && left.bytes == right.bytes
        && left.permissions.readonly() == right.permissions.readonly();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        same && left.permissions.mode() == right.permissions.mode()
    }
    #[cfg(not(unix))]
    {
        same
    }
}

/// Compare whole staged trees, including additions and deletions.
fn same(left: &BTreeMap<String, Node>, right: &BTreeMap<String, Node>) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .all(|(key, node)| right.get(key).is_some_and(|other| same_node(node, other)))
}

/// Reconcile only changed resources, publishing replacement files atomically.
fn reconcile(before: &BTreeMap<String, Node>, after: &BTreeMap<String, Node>) -> Result<()> {
    let mut removed: Vec<_> = before
        .iter()
        .filter(|(key, node)| {
            after.get(*key).is_none_or(|next| {
                node.path != next.path || node.bytes.is_some() != next.bytes.is_some()
            })
        })
        .map(|(_, node)| node)
        .collect();
    removed.sort_by_key(|node| std::cmp::Reverse(node.path.components().count()));
    for node in removed {
        if node.bytes.is_some() {
            fs::remove_file(&node.path)?;
        } else {
            fs::remove_dir(&node.path)?;
        }
    }
    let mut ordered: Vec<_> = after.values().collect();
    ordered.sort_by_key(|node| node.path.components().count());
    for node in ordered {
        if before
            .get(&key(&node.path))
            .is_some_and(|previous| same_node(previous, node))
        {
            continue;
        }
        if let Some(bytes) = &node.bytes {
            let parent = node
                .path
                .parent()
                .ok_or(MutationError::InvalidEdit("file has no parent"))?;
            let mut staged = tempfile::NamedTempFile::new_in(parent)?;
            staged.write_all(bytes)?;
            staged.as_file().set_permissions(node.permissions.clone())?;
            staged.as_file().sync_all()?;
            staged
                .persist(&node.path)
                .map_err(|error| MutationError::Io(error.error))?;
        } else {
            fs::create_dir_all(&node.path)?;
            fs::set_permissions(&node.path, node.permissions.clone())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::lsp::{LspLease, LspService};
    use std::sync::Arc;

    /// Create a live scope with ordinary workspace writes and no editor dependency.
    fn fixture(root: &Path) -> (Arc<LspScope>, LspLease, DirectoryPolicy) {
        let service = Arc::new(LspService::default());
        service.set_enabled("test", true);
        let compiled = Arc::new(
            crate::permissions::matcher::CompiledPermissionDocument::compile(
                crate::permissions::document::default_permission_document(),
                root,
            ),
        );
        let (scope, lease) = service.scope(
            "test".into(),
            "turn".into(),
            root.into(),
            compiled,
            tokio::sync::watch::channel(false).1,
        );
        let policy = DirectoryPolicy {
            access: Default::default(),
            workspace: root.into(),
            session_id: "test".into(),
            settings_root: None,
        };
        (scope, lease, policy)
    }

    /// Stage the exact wire format produced by the editor adapter.
    fn prepared(value: serde_json::Value) -> Prepared {
        serde_json::from_value(serde_json::json!({"operations": value})).unwrap()
    }

    #[test]
    fn denied_destination_leaves_prior_text_edit_untouched() {
        let root = tempfile::tempdir().unwrap();
        let other = tempfile::tempdir().unwrap();
        fs::write(root.path().join("a"), b"old").unwrap();
        let (scope, _lease, policy) = fixture(root.path());
        let result = Transaction::prepare(
            &scope,
            &policy,
            prepared(serde_json::json!([
                {"kind":"text","path":"a","before":"old","after":"new","edits":1},
                {"kind":"rename","old_path":"a","new_path":other.path().join("a")}
            ])),
        );
        assert!(matches!(result, Err(MutationError::PathDenied(_))));
        assert_eq!(fs::read(root.path().join("a")).unwrap(), b"old");
    }

    #[test]
    fn stale_disk_rejects_commit_and_write_off_rejects_prepare() {
        let root = tempfile::tempdir().unwrap();
        fs::write(root.path().join("a"), b"old").unwrap();
        let (scope, _lease, mut policy) = fixture(root.path());
        let edits =
            serde_json::json!([{"kind":"text","path":"a","before":"old","after":"new","edits":1}]);
        let mut transaction =
            Transaction::prepare(&scope, &policy, prepared(edits.clone())).unwrap();
        fs::write(root.path().join("a"), b"user").unwrap();
        assert!(matches!(
            transaction.commit(),
            Err(MutationError::SourceChanged(_))
        ));
        assert_eq!(fs::read(root.path().join("a")).unwrap(), b"user");
        policy.access.write_access = crate::session::WriteScope::Off;
        assert!(matches!(
            Transaction::prepare(&scope, &policy, prepared(edits)),
            Err(MutationError::PathDenied(_))
        ));
    }

    #[test]
    fn failed_publication_rolls_back_files_already_written() {
        let root = tempfile::tempdir().unwrap();
        fs::write(root.path().join("a"), b"old").unwrap();
        let (scope, _lease, policy) = fixture(root.path());
        let mut transaction = Transaction::prepare(
            &scope,
            &policy,
            prepared(serde_json::json!([
                {"kind":"text","path":"a","before":"old","after":"new","edits":1},
                {"kind":"create","path":"z"}
            ])),
        )
        .unwrap();
        // Inject an impossible final destination after validation, exercising recovery after a writes.
        let entry = transaction
            .after
            .get_mut(&key(&root.path().join("z")))
            .unwrap();
        entry.path = root.path().join("missing/z");
        assert!(transaction.commit().is_err());
        assert_eq!(fs::read(root.path().join("a")).unwrap(), b"old");
        assert!(!root.path().join("z").exists());
    }
}
