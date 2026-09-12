use anyhow::{Context, Result, ensure};
use forge_git::RepositoryPath;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{
    collections::HashSet,
    fs::{File, OpenOptions},
    io::{Read, Write},
    path::{Path, PathBuf},
};

pub(crate) type IgnoredPathSet = HashSet<RepositoryPath>;
const MAX_BYTES: usize = 1024 * 1024;
const MAX_PATHS: usize = 10_000;

#[derive(Deserialize, Serialize)]
struct Payload {
    version: u32,
    root: String,
    ignored_paths: Vec<String>,
}

fn identity(root: &Path) -> Result<String> {
    let mut key = root
        .to_str()
        .context("Forge ignored paths require a UTF-8 worktree root")?
        .replace('\\', "/");
    while key.len() > 3 && key.ends_with('/') {
        key.pop();
    }
    if cfg!(windows) {
        key.make_ascii_lowercase();
    }
    Ok(key)
}

fn path(directory: &Path, root: &Path) -> Result<(PathBuf, String)> {
    let key = identity(root)?;
    let hash = Sha256::digest(key.as_bytes());
    let name: String = hash.iter().map(|byte| format!("{byte:02x}")).collect();
    Ok((directory.join(format!("{name}.json")), key))
}

pub(crate) fn load(directory: Option<&Path>, root: &Path) -> Result<IgnoredPathSet> {
    let Some(directory) = directory else {
        return Ok(HashSet::new());
    };
    let (path, key) = path(directory, root)?;
    read(&path, &key)
}

fn read(path: &Path, key: &str) -> Result<IgnoredPathSet> {
    let metadata = match std::fs::symlink_metadata(path) {
        Ok(metadata) => metadata,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(HashSet::new()),
        Err(error) => return Err(error.into()),
    };
    ensure!(
        metadata.is_file() && !metadata.file_type().is_symlink(),
        "ignored store is not a regular file"
    );
    ensure!(
        metadata.len() <= MAX_BYTES as u64,
        "ignored store exceeds 1 MiB"
    );
    let mut bytes = Vec::new();
    File::open(path)?
        .take(MAX_BYTES as u64 + 1)
        .read_to_end(&mut bytes)?;
    ensure!(bytes.len() <= MAX_BYTES, "ignored store grew beyond 1 MiB");
    if bytes.is_empty() {
        return Ok(HashSet::new());
    }
    let payload: Payload =
        serde_json::from_slice(&bytes).context("invalid Forge ignored-path JSON")?;
    ensure!(
        payload.version == 1,
        "unsupported Forge ignored-path version"
    );
    ensure!(payload.root == key, "ignored-path worktree root differs");
    ensure!(
        payload.ignored_paths.len() <= MAX_PATHS,
        "ignored store exceeds 10000 paths"
    );
    let mut result = HashSet::new();
    for path in payload.ignored_paths {
        let path = path.replace('\\', "/");
        let path = path.strip_prefix("./").unwrap_or(&path);
        let path = RepositoryPath::new(path.as_bytes().to_vec())?;
        ensure!(
            result.insert(path),
            "ignored store contains duplicate paths"
        );
    }
    Ok(result)
}

pub(crate) fn update(
    directory: &Path,
    root: &Path,
    expected: &IgnoredPathSet,
    selected: &[RepositoryPath],
    ignored: bool,
) -> Result<IgnoredPathSet> {
    ensure!(
        !selected.is_empty() && selected.len() <= MAX_PATHS,
        "ignored change requires 1 to 10000 paths"
    );
    let (destination, key) = path(directory, root)?;
    std::fs::create_dir_all(directory)?;
    let lock_path = destination.with_extension("lock");
    let lock = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(lock_path)?;
    lock.try_lock()
        .context("another owner is updating Forge ignored paths")?;
    let mut current = read(&destination, &key)?;
    for path in selected {
        ensure!(
            current.contains(path) == expected.contains(path),
            "selected Forge ignore state changed before settlement"
        );
        if ignored {
            current.insert(path.clone());
        } else {
            current.remove(path);
        }
    }
    ensure!(
        current.len() <= MAX_PATHS,
        "ignored store exceeds 10000 paths"
    );
    let mut paths: Vec<_> = current
        .iter()
        .map(|path| std::str::from_utf8(path.raw()).map(str::to_owned))
        .collect::<std::result::Result<_, _>>()
        .context("Forge ignored-path storage requires UTF-8 paths")?;
    paths.sort();
    let bytes = serde_json::to_vec(&Payload {
        version: 1,
        root: key,
        ignored_paths: paths,
    })?;
    ensure!(bytes.len() <= MAX_BYTES, "ignored store exceeds 1 MiB");
    let temporary = destination.with_extension(format!("{}.tmp", uuid::Uuid::new_v4()));
    let mut temporary = Temporary {
        path: temporary,
        file: None,
        published: false,
    };
    temporary.file = Some(
        OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&temporary.path)?,
    );
    let output = temporary.file.as_mut().expect("created ignored temporary");
    output.write_all(&bytes)?;
    output.sync_all()?;
    drop(temporary.file.take());
    std::fs::rename(&temporary.path, &destination)?;
    temporary.published = true;
    #[cfg(unix)]
    File::open(directory)?.sync_all()?;
    Ok(current)
}

struct Temporary {
    path: PathBuf,
    file: Option<File>,
    published: bool,
}
impl Drop for Temporary {
    fn drop(&mut self) {
        drop(self.file.take());
        if !self.published {
            let _ = std::fs::remove_file(&self.path);
        }
    }
}
