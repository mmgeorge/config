use std::{
    fs::OpenOptions,
    io,
    path::Path,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, bail, ensure};
use forge_diff::source::{Representation, SourceIdentity, SourcePair, SourceVersion};

use super::{
    ContentRequest, ContentResult, ContentSource, WorktreeConversion, conversion::ConversionSession,
};
use crate::{
    RepositoryPath,
    repository::{RepositoryRead, RepositoryState},
    store::RepositoryStore,
};

struct LfsPointer {
    object: gix::ObjectId,
    size: usize,
}

impl LfsPointer {
    /// Parse recognized LFS pointers, rejecting malformed identities and unsupported extensions.
    fn parse(text: &str) -> Result<Option<Self>> {
        if !text.starts_with("version https://git-lfs.github.com/spec/")
            && !text.starts_with("version https://hawser.github.com/spec/")
        {
            return Ok(None);
        }
        let mut lines = text.split_terminator('\n');
        ensure!(
            matches!(
                lines.next(),
                Some(
                    "version https://git-lfs.github.com/spec/v1"
                        | "version https://hawser.github.com/spec/v1"
                )
            ),
            "unsupported LFS pointer version"
        );
        let object = lines
            .next()
            .and_then(|line| line.strip_prefix("oid sha256:"))
            .context("invalid LFS pointer or unsupported LFS extensions")?;
        ensure!(
            object.len() == 64
                && object
                    .bytes()
                    .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)),
            "invalid LFS SHA-256 object identity"
        );
        let size = lines
            .next()
            .and_then(|line| line.strip_prefix("size "))
            .context("LFS pointer has no size")?;
        ensure!(
            !size.is_empty() && size.bytes().all(|byte| byte.is_ascii_digit()),
            "invalid LFS object size"
        );
        let size = size
            .parse()
            .context("LFS object size exceeds addressable memory")?;
        ensure!(
            lines.next().is_none() && text.ends_with('\n'),
            "unsupported or malformed LFS pointer fields"
        );
        Ok(Some(Self {
            object: gix::ObjectId::from_hex(object.as_bytes())?,
            size,
        }))
    }

    /// Read only a locally stored payload and verify its size and SHA-256 before text decoding.
    fn read(&self, storage: &Path, check: &mut dyn FnMut() -> Result<()>) -> Result<SourceVersion> {
        check()?;
        let object = self.object.to_string();
        let path = storage
            .join("objects")
            .join(&object[..2])
            .join(&object[2..4])
            .join(&object);
        let bytes = if self.size == 0 {
            Vec::new()
        } else {
            let mut options = OpenOptions::new();
            options.read(true);
            #[cfg(unix)]
            {
                use std::os::unix::fs::OpenOptionsExt;
                options.custom_flags(libc::O_NOFOLLOW);
            }
            #[cfg(windows)]
            {
                use std::os::windows::fs::OpenOptionsExt;
                options.custom_flags(0x0020_0000);
            }
            let mut file = match options.open(&path) {
                Ok(file) => file,
                Err(error) if error.kind() == io::ErrorKind::NotFound => {
                    bail!("LFS content not downloaded: {object}")
                }
                Err(error) => return Err(error).context("open local LFS content"),
            };
            let metadata = file.metadata()?;
            ensure!(
                metadata.is_file() && !metadata.is_symlink(),
                "LFS object is not a regular file"
            );
            ensure!(
                usize::try_from(metadata.len())? == self.size,
                "LFS object size mismatch: {object}"
            );
            super::read_exact_content(&mut file, self.size, check)?
        };
        check()?;
        let identity = SourceIdentity::from_bytes(&bytes, Representation::DisplayOnly);
        ensure!(
            identity.content_hash.as_slice() == self.object.as_bytes(),
            "LFS object SHA-256 mismatch: {object}"
        );
        SourceVersion::new(bytes, Representation::DisplayOnly)
            .context("LFS payload is not supported UTF-8 text")
    }
}

impl RepositoryState {
    /// Resolve local LFS text for display without changing canonical acquisition or executing filters.
    ///
    /// Both selected sides belong to `path`; absent sides represent additions or deletions.
    /// Non-LFS attributes return `None`. Missing payloads, unsupported pointers or text, failed
    /// integrity checks, and invalidated reads return errors without publishing a source pair.
    pub async fn lfs_diff_sources(
        self: &Arc<Self>,
        store: &RepositoryStore,
        path: RepositoryPath,
        sources: [Option<ContentSource>; 2],
    ) -> Result<RepositoryRead<Option<SourcePair>>> {
        let input_bytes =
            sources
                .iter()
                .flatten()
                .try_fold(path.retained_bytes(), |total, source| {
                    total
                        .checked_add(source.retained_bytes())
                        .context("LFS input accounting overflow")
                })?;
        let worker = Arc::clone(self);
        let generation = self.generation();
        store
            .read(
                Arc::clone(self),
                input_bytes,
                move |mut local, cancellation| {
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            worker.generation() == generation,
                            "content acquisition was invalidated"
                        );
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "LFS content acquisition exceeded 30-second deadline"
                        );
                        Ok(())
                    };
                    let mut session =
                        ConversionSession::new(&mut local, &worker.identity, &mut check)?;
                    if !session.is_lfs(&local, &worker.identity, &path, &mut check)? {
                        session.verify(&mut local, &worker.identity, &mut check)?;
                        return Ok(None);
                    }
                    let configured = local
                        .config_snapshot()
                        .string("lfs.storage")
                        .map(|value| gix::path::from_bstr(value).into_owned());
                    let storage = worker.identity.common_directory.join(
                        configured
                            .filter(|path| !path.as_os_str().is_empty())
                            .unwrap_or_else(|| "lfs".into()),
                    );
                    let [old, new] = sources;
                    let old = read_side(&mut local, &worker, old, &storage, &mut check)?;
                    let new = read_side(&mut local, &worker, new, &storage, &mut check)?;
                    session.verify(&mut local, &worker.identity, &mut check)?;
                    Ok(Some(SourcePair { old, new }))
                },
            )
            .await
    }

    /// Identify LFS paths under one verified attribute snapshot without reading payloads.
    pub async fn lfs_paths(
        self: &Arc<Self>,
        store: &RepositoryStore,
        paths: Vec<RepositoryPath>,
    ) -> Result<RepositoryRead<Vec<RepositoryPath>>> {
        let worker = Arc::clone(self);
        let generation = self.generation();
        store
            .read(
                Arc::clone(self),
                paths.iter().map(RepositoryPath::retained_bytes).sum(),
                move |mut local, cancellation| {
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            worker.generation() == generation,
                            "content acquisition was invalidated"
                        );
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "LFS attribute read exceeded 30-second deadline"
                        );
                        Ok(())
                    };
                    let mut session =
                        ConversionSession::new(&mut local, &worker.identity, &mut check)?;
                    let mut selected = Vec::new();
                    for path in paths {
                        if session.is_lfs(&local, &worker.identity, &path, &mut check)? {
                            selected.push(path);
                        }
                    }
                    session.verify(&mut local, &worker.identity, &mut check)?;
                    Ok(selected)
                },
            )
            .await
    }
}

/// Read a selected side without clean filters and expand a pointer only inside an LFS-marked file.
fn read_side(
    local: &mut gix::Repository,
    repository: &RepositoryState,
    source: Option<ContentSource>,
    storage: &Path,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<SourceVersion> {
    let Some(source) = source else {
        return Ok(SourceVersion::empty(Representation::DisplayOnly));
    };
    let source = match source {
        ContentSource::Worktree { path, .. } => ContentSource::Worktree {
            path,
            conversion: WorktreeConversion::Raw,
        },
        source @ (ContentSource::Object(_)
        | ContentSource::IndexStage { .. }
        | ContentSource::Supplied(_)) => source,
    };
    local.objects.ignore_replacements = true;
    let content = match super::acquire(
        local,
        repository,
        ContentRequest {
            source,
            expected: None,
        },
        check,
    )? {
        ContentResult::Ready(content) => content.source,
        ContentResult::Failed(error) => return Err(error),
        ContentResult::Binary => bail!("LFS source is binary; only text LFS diffs are supported"),
        ContentResult::Missing => bail!("LFS diff source is missing"),
        ContentResult::Unavailable(reason) => bail!("LFS source unavailable: {reason:?}"),
    };
    if let Some(pointer) = LfsPointer::parse(content.text())? {
        return pointer.read(storage, check);
    }
    Ok(SourceVersion::new(
        content.bytes().to_vec(),
        Representation::DisplayOnly,
    )?)
}
