use super::manifest::{CheckpointFile, CheckpointRecord, Content, ResolvedFile};
use crate::storage::objects::ObjectStore;
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeSet,
    fs,
    io::Write,
    path::{Component, Path, PathBuf},
};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RestorePath {
    pub path: String,
    pub before: Option<CheckpointFile>,
    pub after: Option<CheckpointFile>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RestorePreview {
    pub id: String,
    pub session_id: String,
    pub workspace: String,
    pub head: String,
    pub path: Vec<RestorePath>,
    pub warning: Vec<String>,
    pub exchange_ids: Vec<String>,
    pub recovery_of: Option<String>,
}

impl RestorePreview {
    pub(crate) fn prepare(
        objects: &ObjectStore,
        root: &Path,
        expected: &CheckpointRecord,
        target: &CheckpointRecord,
    ) -> Result<Self> {
        ensure!(
            expected.session_id == target.session_id,
            "restore crossed a session boundary"
        );
        ensure!(
            expected.workspace == target.workspace,
            "restore crossed a worktree boundary"
        );
        ensure!(
            fs::canonicalize(root)? == fs::canonicalize(&target.workspace)?,
            "checkpoint belongs to another worktree"
        );
        let before = expected.resolved()?;
        let after = target.resolved()?;
        let mut path = Vec::new();
        let mut warning = Vec::new();
        let head = current_head(root)?;
        if head != expected.head || head != target.head {
            warning.push(
                "Git HEAD changed. Restore will use the checkpoint's exact file contents.".into(),
            );
        }
        let paths = before
            .keys()
            .chain(after.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        for relative in paths {
            #[cfg(windows)]
            if !after.contains_key(&relative)
                && after
                    .keys()
                    .any(|path| path.eq_ignore_ascii_case(&relative))
            {
                continue;
            }
            let old = before.get(&relative);
            #[cfg(windows)]
            let old = old.or_else(|| {
                before
                    .iter()
                    .find(|(path, _)| path.eq_ignore_ascii_case(&relative))
                    .map(|(_, file)| file)
            });
            let new = after.get(&relative);
            if old == new && before.contains_key(&relative) {
                continue;
            }
            let old = prepare_content(objects, root, &relative, old)?;
            let new = prepare_content(objects, root, &relative, new)?;
            if old == new && !cfg!(windows) {
                continue;
            }
            let actual = read_actual(objects, root, &relative)?;
            if actual.as_ref().map(|file| (&file.object_id, file.mode))
                != old.as_ref().map(|file| (&file.object_id, file.mode))
            {
                warning.push(format!("Later changes will be overwritten: {relative}"));
            }
            if actual == new {
                continue;
            }
            path.push(RestorePath {
                path: relative,
                before: actual,
                after: new,
            });
        }
        order_paths(&mut path);
        Ok(Self {
            id: uuid::Uuid::new_v4().to_string(),
            session_id: target.session_id.clone(),
            workspace: target.workspace.clone(),
            head,
            path,
            warning,
            exchange_ids: Vec::new(),
            recovery_of: None,
        })
    }

    pub(crate) fn retained_bytes(&self) -> Result<usize> {
        let mut bytes = std::mem::size_of::<Self>();
        for path in &self.path {
            bytes = bytes
                .checked_add(std::mem::size_of::<RestorePath>() + path.path.capacity())
                .context("restore input size overflow")?;
            for file in [path.before.as_ref(), path.after.as_ref()]
                .into_iter()
                .flatten()
            {
                bytes = bytes
                    .checked_add(file.path.capacity() + file.object_id.capacity())
                    .context("restore input size overflow")?;
            }
        }
        Ok(bytes)
    }

    pub(crate) fn save(&self, objects: &ObjectStore) -> Result<()> {
        let root = objects
            .recovery_root()
            .join(crate::plan::digest(self.session_id.as_bytes()));
        fs::create_dir_all(&root)?;
        let mut output = tempfile::NamedTempFile::new_in(&root)?;
        serde_json::to_writer(&mut output, self)?;
        output.as_file().sync_all()?;
        output
            .persist(root.join(format!(
                "preview-{}.json",
                crate::plan::digest(self.id.as_bytes())
            )))
            .map_err(|error| error.error)?;
        Ok(())
    }

    pub(crate) fn load(objects: &ObjectStore, session: &str, id: &str) -> Result<Self> {
        let root = objects
            .recovery_root()
            .join(crate::plan::digest(session.as_bytes()));
        let file = fs::File::open(root.join(format!(
            "preview-{}.json",
            crate::plan::digest(id.as_bytes())
        )))?;
        let preview: Self = serde_json::from_reader(file)?;
        ensure!(
            preview.id == id && preview.session_id == session,
            "restore preview belongs to another session"
        );
        Ok(preview)
    }

    pub(crate) fn summary(&self, offset: usize) -> serde_json::Value {
        serde_json::json!({ "preview_id": self.id, "files": self.path.len(), "warning_count": self.warning.len(),
            "warnings": self.warning.iter().skip(offset).take(50).collect::<Vec<_>>(),
            "next_offset": (offset + 50 < self.warning.len()).then_some(offset + 50) })
    }

    pub(crate) fn revalidate(&self, objects: &ObjectStore) -> Result<()> {
        let root = Path::new(&self.workspace);
        ensure!(
            current_head(root)? == self.head,
            "restore preview is stale: Git HEAD changed"
        );
        for path in &self.path {
            ensure!(
                same_file(
                    read_actual(objects, root, &path.path)?.as_ref(),
                    path.before.as_ref()
                ),
                "restore preview is stale: {} changed",
                path.path
            );
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) enum RestoreState {
    Applying,
    FilesComplete,
    Complete,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RestoreJournal {
    pub preview: RestorePreview,
    pub applied: usize,
    pub state: RestoreState,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct RestoreProgress {
    pub preview_id: String,
    pub session_id: String,
    pub applied: usize,
    pub files: usize,
    pub state: RestoreState,
}

impl RestoreJournal {
    pub(crate) fn begin(objects: &ObjectStore, preview: RestorePreview) -> Result<Self> {
        preview.revalidate(objects)?;
        preview.save(objects)?;
        let journal = Self {
            preview,
            applied: 0,
            state: RestoreState::Applying,
        };
        journal.save(objects)?;
        Ok(journal)
    }

    pub(crate) fn save(&self, objects: &ObjectStore) -> Result<()> {
        let directory = objects
            .recovery_root()
            .join(crate::plan::digest(self.preview.session_id.as_bytes()));
        fs::create_dir_all(&directory)?;
        let mut temporary = tempfile::NamedTempFile::new_in(&directory)?;
        serde_json::to_writer(
            &mut temporary,
            &RestoreProgress {
                preview_id: self.preview.id.clone(),
                session_id: self.preview.session_id.clone(),
                applied: self.applied,
                files: self.preview.path.len(),
                state: self.state,
            },
        )?;
        temporary.flush()?;
        temporary.as_file().sync_all()?;
        temporary
            .persist(directory.join("journal.json"))
            .map_err(|error| error.error)?;
        Ok(())
    }

    pub(crate) fn progress(
        objects: &ObjectStore,
        session: &str,
    ) -> Result<Option<RestoreProgress>> {
        let path = objects
            .recovery_root()
            .join(crate::plan::digest(session.as_bytes()))
            .join("journal.json");
        match fs::File::open(path) {
            Ok(file) => {
                let progress: RestoreProgress = serde_json::from_reader(file)?;
                ensure!(
                    progress.session_id == session,
                    "restore progress belongs to another session"
                );
                Ok(Some(progress))
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(error) => Err(error.into()),
        }
    }

    pub(crate) fn load(objects: &ObjectStore, session: &str) -> Result<Option<Self>> {
        Self::progress(objects, session)?
            .map(|progress| {
                let preview = RestorePreview::load(objects, session, &progress.preview_id)?;
                ensure!(
                    preview.path.len() == progress.files && progress.applied <= progress.files,
                    "restore progress is corrupt"
                );
                Ok(Self {
                    preview,
                    applied: progress.applied,
                    state: progress.state,
                })
            })
            .transpose()
    }

    pub(crate) fn apply(
        &mut self,
        objects: &ObjectStore,
        check: &mut dyn FnMut() -> Result<()>,
    ) -> Result<()> {
        let root = Path::new(&self.preview.workspace);
        for position in 0..self.preview.path.len() {
            check()?;
            let change = &self.preview.path[position];
            let actual = read_actual(objects, root, &change.path)?;
            if change.after.is_none() && actual.as_ref().is_some_and(|file| file.mode == 0o040000) {
                self.applied = position + 1;
                self.save(objects)?;
                continue;
            }
            if !same_file(actual.as_ref(), change.after.as_ref()) {
                ensure!(
                    same_file(actual.as_ref(), change.before.as_ref()),
                    "restore interrupted by a concurrent edit: {}",
                    change.path
                );
                write_file(objects, root, &change.path, change.after.as_ref())?;
            }
            self.applied = position + 1;
            self.save(objects)?;
        }
        self.state = RestoreState::FilesComplete;
        self.save(objects)
    }

    pub(crate) fn reverse(&self, objects: &ObjectStore) -> Result<RestorePreview> {
        let root = Path::new(&self.preview.workspace);
        let mut preview = self.preview.clone();
        preview.id = uuid::Uuid::new_v4().to_string();
        preview.recovery_of = Some(self.preview.id.clone());
        preview.head = current_head(root)?;
        preview.warning.clear();
        preview.exchange_ids.clear();
        for change in &mut preview.path {
            let current = read_actual(objects, root, &change.path)?;
            if current != change.after && current != change.before {
                preview.warning.push(format!(
                    "Later changes will be overwritten: {}",
                    change.path
                ));
            }
            change.after = change.before.take();
            change.before = current;
            if let Some(file) = &change.after {
                change.path = file.path.clone();
            }
        }
        order_paths(&mut preview.path);
        Ok(preview)
    }
}

fn prepare_content(
    objects: &ObjectStore,
    root: &Path,
    path: &str,
    file: Option<&ResolvedFile>,
) -> Result<Option<CheckpointFile>> {
    let Some(file) = file else { return Ok(None) };
    ensure!(
        file.mode != 0o160000,
        "submodule restore is unavailable: {path}"
    );
    let object_id = match &file.source {
        Content::Stored(identity) => {
            objects.verify(identity)?;
            identity.clone()
        }
        Content::Git(source) => objects.put_git(root, source)?,
    };
    Ok(Some(CheckpointFile {
        path: path.into(),
        object_id,
        mode: file.mode,
    }))
}

pub(super) fn file_mode(metadata: &fs::Metadata) -> u32 {
    if metadata.is_symlink() {
        return 0o120000;
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        if metadata.permissions().mode() & 0o111 != 0 {
            return 0o100755;
        }
    }
    0o100644
}

pub(super) fn checked_path(root: &Path, path: &str) -> Result<PathBuf> {
    let native = forge_git::checkpoint::native_path(path)?;
    let relative = Path::new(&native);
    ensure!(
        !relative.as_os_str().is_empty()
            && relative
                .components()
                .all(|part| matches!(part, Component::Normal(_)))
            && !relative.components().any(|part| part
                .as_os_str()
                .to_string_lossy()
                .eq_ignore_ascii_case(".git")),
        "invalid checkpoint path: {path}"
    );
    let mut current = root.to_owned();
    let parts = relative.components().collect::<Vec<_>>();
    for (position, part) in parts.iter().enumerate() {
        current.push(part.as_os_str());
        if position + 1 != parts.len() {
            match fs::symlink_metadata(&current) {
                Ok(metadata) => ensure!(
                    !metadata.is_symlink(),
                    "unsafe checkpoint ancestor: {}",
                    current.display()
                ),
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error.into()),
            }
        }
    }
    Ok(current)
}

fn read_actual(
    objects: &ObjectStore,
    root: &Path,
    relative: &str,
) -> Result<Option<CheckpointFile>> {
    let path = checked_path(root, relative)?;
    let metadata = match fs::symlink_metadata(&path) {
        Ok(metadata) => metadata,
        Err(error)
            if matches!(
                error.kind(),
                std::io::ErrorKind::NotFound | std::io::ErrorKind::NotADirectory
            ) =>
        {
            return Ok(None);
        }
        Err(error) => return Err(error.into()),
    };
    if metadata.is_dir() {
        return Ok(Some(CheckpointFile {
            path: relative.into(),
            object_id: String::new(),
            mode: 0o040000,
        }));
    }
    ensure!(
        metadata.is_file() || metadata.is_symlink(),
        "restore destination is not a file: {relative}"
    );
    let object_id = if metadata.is_symlink() {
        objects.put(fs::read_link(&path)?.as_os_str().as_encoded_bytes())?
    } else {
        objects.put_file(&path)?
    };
    #[allow(unused_mut)]
    let mut actual_path = relative.to_owned();
    #[cfg(windows)]
    {
        if !metadata.is_symlink() {
            let actual = fs::canonicalize(&path)?;
            let canonical_root = fs::canonicalize(root)?;
            actual_path = actual
                .strip_prefix(canonical_root)?
                .to_string_lossy()
                .replace('\\', "/");
        }
    }
    Ok(Some(CheckpointFile {
        path: actual_path,
        object_id,
        mode: file_mode(&metadata),
    }))
}

fn write_file(
    objects: &ObjectStore,
    root: &Path,
    relative: &str,
    file: Option<&CheckpointFile>,
) -> Result<()> {
    let path = checked_path(root, relative)?;
    if let Some(file) = file {
        if file.mode == 0o040000 {
            if path.is_file() {
                fs::remove_file(&path)?;
            }
            fs::create_dir_all(&path)?;
            return Ok(());
        }
        if fs::symlink_metadata(&path).is_ok_and(|metadata| metadata.is_dir()) {
            fs::remove_dir(&path)?;
        }
        if file.mode == 0o120000 {
            let bytes = objects
                .get(&file.object_id, 1024 * 1024)?
                .context("symlink target exceeds limit")?;
            #[cfg(windows)]
            let target = Path::new(std::str::from_utf8(&bytes)?);
            #[cfg(unix)]
            let target = {
                use std::os::unix::ffi::OsStrExt;
                Path::new(std::ffi::OsStr::from_bytes(&bytes))
            };
            if fs::symlink_metadata(&path).is_ok() {
                fs::remove_file(&path)?;
            }
            fs::create_dir_all(path.parent().context("missing destination parent")?)?;
            #[cfg(unix)]
            std::os::unix::fs::symlink(target, &path)?;
            #[cfg(windows)]
            std::os::windows::fs::symlink_file(target, &path)?;
        } else {
            objects.restore_file(&file.object_id, &path)?;
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                fs::set_permissions(&path, fs::Permissions::from_mode(file.mode & 0o777))?;
            }
        }
    } else if fs::symlink_metadata(&path).is_ok() {
        fs::remove_file(path)?;
    }
    Ok(())
}

fn current_head(root: &Path) -> Result<String> {
    Ok(forge_git::checkpoint::head(root)?.unwrap_or_else(|| "UNBORN".into()))
}

fn same_file(left: Option<&CheckpointFile>, right: Option<&CheckpointFile>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.path == right.path
                && left.object_id == right.object_id
                && (left.mode == right.mode
                    || cfg!(windows) && left.mode & 0o170000 == right.mode & 0o170000)
        }
        (None, None) => true,
        _ => false,
    }
}

fn order_paths(paths: &mut [RestorePath]) {
    paths.sort_by(
        |left, right| match (left.after.is_none(), right.after.is_none()) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            (true, true) => right
                .path
                .len()
                .cmp(&left.path.len())
                .then_with(|| left.path.cmp(&right.path)),
            _ => left
                .path
                .len()
                .cmp(&right.path.len())
                .then_with(|| left.path.cmp(&right.path)),
        },
    );
}
