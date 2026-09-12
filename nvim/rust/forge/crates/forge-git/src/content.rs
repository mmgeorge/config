use std::{
    fs::OpenOptions,
    io::{self, Read},
    path::Path,
    process::Command,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};
use forge_diff::source::{
    MAX_SOURCE_BYTES, Representation, SourceError, SourceIdentity, SourceVersion,
};

use crate::{
    RepositoryIdentity, RepositoryPath, WorktreeId,
    command::{CommandLimits, read_command},
    repository::{RepositoryRead, RepositoryState},
    resolve_argument,
    snapshot::{
        ObjectState, WorktreeStamp, metadata_stamp, parse_object_state, read_worktree_stamp,
    },
    store::RepositoryStore,
    validate_path,
};

mod batch;
mod conversion;
pub(crate) use batch::ContentBatch;
pub use conversion::ConversionIdentity;

pub(crate) fn verify_conversion(
    local: &mut gix::Repository,
    repository: &RepositoryIdentity,
    path: &RepositoryPath,
    identity: &ConversionIdentity,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<bool> {
    match conversion::verify_context(local, repository, path, identity, check) {
        Ok(()) => Ok(true),
        Err(error)
            if matches!(
                error.to_string().as_str(),
                "configuration changed during canonical conversion"
                    | "index changed during canonical conversion"
                    | "attributes changed during canonical conversion"
            ) =>
        {
            Ok(false)
        }
        Err(error) => Err(error),
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WorktreeConversion {
    Raw,
    GitCanonical,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum IndexStage {
    Normal,
    Base,
    Ours,
    Theirs,
}

#[derive(Clone, Debug)]
/// Selects immutable object bytes, a current index stage, worktree conversion, or validated input.
pub enum ContentSource {
    Object(gix::ObjectId),
    IndexStage {
        path: RepositoryPath,
        stage: IndexStage,
    },
    Worktree {
        path: RepositoryPath,
        conversion: WorktreeConversion,
    },
    Supplied(SourceVersion),
}

#[derive(Clone, Copy, Debug)]
/// Applies a caller-lowered byte and line bound within the shared 8 MiB source ceiling.
pub struct ContentLimits {
    bytes: usize,
    lines: usize,
}

#[derive(Clone, Debug)]
pub struct ContentRequest {
    pub source: ContentSource,
    pub limits: ContentLimits,
    /// Rejects a ready source whose bytes or representation differ from this identity.
    pub expected: Option<SourceIdentity>,
}

struct WorktreeRead<'session> {
    path: RepositoryPath,
    conversion: WorktreeConversion,
    limits: ContentLimits,
    expected: Option<SourceIdentity>,
    session: Option<&'session mut conversion::ConversionSession>,
}

#[derive(Clone, Debug)]
pub enum ContentOrigin {
    Object(gix::ObjectId),
    IndexStage {
        worktree: WorktreeId,
        path: RepositoryPath,
        stage: IndexStage,
        state: ObjectState,
    },
    Worktree {
        worktree: WorktreeId,
        path: RepositoryPath,
        stamp: WorktreeStamp,
        conversion: Option<Arc<ConversionIdentity>>,
    },
    Supplied,
}

#[derive(Clone, Debug)]
/// Shares the immutable analysis allocation and retains acquisition provenance separately.
pub struct FileContent {
    pub source: SourceVersion,
    pub origin: ContentOrigin,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ContentLimit {
    Bytes,
    Lines,
    RetainedCapacity,
    ConversionCapacity,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ContentUnavailable {
    UnsupportedEncoding,
    NotBlob(gix::objs::Kind),
    Directory,
    SpecialFile,
    Gitlink,
    ExternalFilter,
}

#[derive(Debug)]
/// Keeps unavailable content separate from acquisition failures and successful empty sources.
pub enum ContentResult {
    Ready(FileContent),
    Binary,
    TooLarge {
        kind: ContentLimit,
        observed: u64,
        limit: u64,
    },
    Missing,
    Unavailable(ContentUnavailable),
    Failed(anyhow::Error),
}

#[derive(Debug)]
pub struct ContentAcquisition {
    pub content: ContentResult,
    pub identity: Option<SourceIdentity>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BoundedLineCount {
    Exact(usize),
    ExceedsLimit { at_least: usize },
}

impl Default for ContentLimits {
    fn default() -> Self {
        Self {
            bytes: MAX_SOURCE_BYTES,
            lines: usize::MAX,
        }
    }
}

impl ContentLimits {
    /// Allows zero bytes or lines for an empty-only request and rejects a byte bound above 8 MiB.
    pub fn new(bytes: usize, lines: usize) -> Result<Self> {
        ensure!(
            bytes <= MAX_SOURCE_BYTES,
            "content byte limit exceeds 8 MiB"
        );
        Ok(Self { bytes, lines })
    }
}

impl RepositoryState {
    /// Acquires exact bounded source bytes on the shared read pool and rejects invalidation.
    ///
    /// Object bytes use immutable object identity, index reads verify the selected stage before
    /// and after acquisition, and worktree reads verify metadata without following leaf symlinks.
    /// Worktree requests select raw bytes or canonical built-in conversion. Failures remain distinct from missing,
    /// binary, unsupported, and oversized content. Admission and cancellation fail the outer read.
    pub async fn content(
        self: &Arc<Self>,
        store: &RepositoryStore,
        request: ContentRequest,
    ) -> Result<RepositoryRead<ContentResult>> {
        let result = self.acquire_content(store, request, false).await?;
        Ok(RepositoryRead {
            generation: result.generation,
            value: result.value.content,
        })
    }

    /// Retains the canonical byte identity before text classification, including binary sources.
    /// Sources rejected before bounded acquisition have no identity.
    pub async fn content_with_identity(
        self: &Arc<Self>,
        store: &RepositoryStore,
        request: ContentRequest,
    ) -> Result<RepositoryRead<ContentAcquisition>> {
        self.acquire_content(store, request, true).await
    }

    async fn acquire_content(
        self: &Arc<Self>,
        store: &RepositoryStore,
        request: ContentRequest,
        retain_identity: bool,
    ) -> Result<RepositoryRead<ContentAcquisition>> {
        let retained = match &request.source {
            ContentSource::Object(_) => 0,
            ContentSource::IndexStage { path, .. } | ContentSource::Worktree { path, .. } => {
                path.retained_bytes()
            }
            ContentSource::Supplied(source) => source.retained_bytes(),
        };
        let input_bytes = retained
            .checked_add(size_of::<ContentRequest>())
            .context("content request accounting overflow")?;
        let worker = Arc::clone(self);
        let generation = self.generation();
        store
            .read(
                Arc::clone(self),
                input_bytes,
                move |mut local, cancellation| {
                    local.objects.ignore_replacements = true;
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            worker.generation() == generation,
                            "content acquisition was invalidated"
                        );
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "content acquisition exceeded 30-second deadline"
                        );
                        Ok(())
                    };
                    check()?;
                    let mut identity = None;
                    let result = acquire_with_identity(
                        &mut local,
                        &worker,
                        request,
                        retain_identity.then_some(&mut identity),
                        &mut check,
                    )
                    .unwrap_or_else(ContentResult::Failed);
                    check()?;
                    if matches!(result, ContentResult::Failed(_)) {
                        identity = None;
                    }
                    Ok(ContentAcquisition {
                        content: result,
                        identity,
                    })
                },
            )
            .await
    }
}

pub(crate) fn acquire(
    local: &mut gix::Repository,
    repository: &RepositoryState,
    request: ContentRequest,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<ContentResult> {
    acquire_with_identity(local, repository, request, None, check)
}

fn acquire_with_identity(
    local: &mut gix::Repository,
    repository: &RepositoryState,
    request: ContentRequest,
    identity: Option<&mut Option<SourceIdentity>>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<ContentResult> {
    match request.source {
        ContentSource::Object(object) => read_object(
            local,
            object,
            request.limits,
            request.expected,
            ContentOrigin::Object(object),
            identity,
            check,
        ),
        ContentSource::Supplied(source) => {
            if let Some(identity) = identity {
                *identity = Some(source.identity());
            }
            finish_source(
                source,
                ContentOrigin::Supplied,
                request.limits,
                request.expected,
            )
        }
        ContentSource::Worktree { path, conversion } => read_worktree(
            local,
            &repository.identity,
            WorktreeRead {
                path,
                conversion,
                limits: request.limits,
                expected: request.expected,
                session: None,
            },
            identity,
            check,
        ),
        ContentSource::IndexStage { path, stage } => {
            let root = repository
                .identity
                .worktree_root
                .as_ref()
                .context("index source requires a worktree")?;
            let Some(state) = read_index_stage(root, &path, stage, local.object_hash(), check)?
            else {
                return Ok(ContentResult::Missing);
            };
            if state.mode == 0o160000 {
                return Ok(ContentResult::Unavailable(ContentUnavailable::Gitlink));
            }
            let origin = ContentOrigin::IndexStage {
                worktree: repository
                    .identity
                    .worktree
                    .clone()
                    .context("index source has no worktree identity")?,
                path: path.clone(),
                stage,
                state: state.clone(),
            };
            let result = read_object(
                local,
                state.object,
                request.limits,
                request.expected,
                origin,
                identity,
                check,
            )?;
            ensure!(
                Some(state) == read_index_stage(root, &path, stage, local.object_hash(), check)?,
                "index stage changed during content acquisition"
            );
            Ok(result)
        }
    }
}

fn read_object(
    repository: &gix::Repository,
    object: gix::ObjectId,
    limits: ContentLimits,
    expected: Option<SourceIdentity>,
    origin: ContentOrigin,
    identity: Option<&mut Option<SourceIdentity>>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<ContentResult> {
    check()?;
    ensure!(
        object.kind() == repository.object_hash(),
        "content object hash kind mismatch"
    );
    let Some(header) = repository
        .try_find_header(object)
        .context("read content object header")?
    else {
        return Ok(ContentResult::Missing);
    };
    if header.kind() != gix::objs::Kind::Blob {
        return Ok(ContentResult::Unavailable(ContentUnavailable::NotBlob(
            header.kind(),
        )));
    }
    if header.size() > limits.bytes as u64 {
        return Ok(ContentResult::TooLarge {
            kind: ContentLimit::Bytes,
            observed: header.size(),
            limit: limits.bytes as u64,
        });
    }
    check()?;
    let loaded = repository
        .find_object(object)
        .context("decode admitted content object")?;
    ensure!(
        loaded.kind == header.kind() && loaded.data.len() as u64 == header.size(),
        "object changed after header admission"
    );
    check()?;
    finish_bytes(
        loaded.detach().data,
        Representation::GitCanonical,
        origin,
        limits,
        expected,
        identity,
    )
}

fn read_worktree(
    local: &mut gix::Repository,
    repository: &RepositoryIdentity,
    request: WorktreeRead<'_>,
    identity: Option<&mut Option<SourceIdentity>>,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<ContentResult> {
    let WorktreeRead {
        path,
        conversion,
        limits,
        expected,
        session,
    } = request;
    check()?;
    let root = repository
        .worktree_root
        .as_ref()
        .context("worktree source requires a worktree")?;
    let worktree = repository
        .worktree
        .clone()
        .context("worktree source has no identity")?;
    let stamp = read_worktree_stamp(root, &path)?;
    let bytes = match &stamp {
        WorktreeStamp::Unobserved => anyhow::bail!("content read requires inspected metadata"),
        WorktreeStamp::Missing => return Ok(ContentResult::Missing),
        WorktreeStamp::Directory(_) => {
            return Ok(ContentResult::Unavailable(ContentUnavailable::Directory));
        }
        WorktreeStamp::Other(_) => {
            return Ok(ContentResult::Unavailable(ContentUnavailable::SpecialFile));
        }
        WorktreeStamp::Symlink { target, .. } => {
            let bytes = target.as_os_str().as_encoded_bytes();
            if bytes.len() > limits.bytes {
                return Ok(ContentResult::TooLarge {
                    kind: ContentLimit::Bytes,
                    observed: bytes.len() as u64,
                    limit: limits.bytes as u64,
                });
            }
            bytes.to_vec()
        }
        WorktreeStamp::File(metadata) => {
            if metadata.length > limits.bytes as u64 {
                return Ok(ContentResult::TooLarge {
                    kind: ContentLimit::Bytes,
                    observed: metadata.length,
                    limit: limits.bytes as u64,
                });
            }
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
                const OPEN_REPARSE_POINT: u32 = 0x0020_0000;
                options.custom_flags(OPEN_REPARSE_POINT);
            }
            let mut file = options
                .open(validate_path(root, &path)?)
                .context("open worktree content without following a leaf symlink")?;
            let opened = file.metadata()?;
            ensure!(
                opened.is_file() && !opened.is_symlink() && *metadata == metadata_stamp(&opened)?,
                "worktree changed before content acquisition"
            );
            let bytes = read_bounded(&mut file, metadata.length as usize, limits.bytes, check)?;
            ensure!(
                *metadata == metadata_stamp(&file.metadata()?)?,
                "worktree changed during content acquisition"
            );
            bytes
        }
    };
    check()?;
    ensure!(
        stamp == read_worktree_stamp(root, &path)?,
        "worktree path changed during content acquisition"
    );
    let (bytes, representation, conversion) = match conversion {
        WorktreeConversion::Raw => (bytes, Representation::Raw, None),
        WorktreeConversion::GitCanonical if matches!(stamp, WorktreeStamp::Symlink { .. }) => {
            (bytes, Representation::GitCanonical, None)
        }
        WorktreeConversion::GitCanonical => match if let Some(session) = session {
            session.convert(local, repository, &path, bytes, limits.bytes, check)
        } else {
            conversion::convert(local, repository, &path, bytes, limits.bytes, check)
        }? {
            conversion::ConversionResult::Ready { bytes, identity } => {
                (bytes, Representation::GitCanonical, Some(identity))
            }
            conversion::ConversionResult::Unavailable(reason) => {
                return Ok(ContentResult::Unavailable(reason));
            }
            conversion::ConversionResult::TooLarge { capacity } => {
                return Ok(ContentResult::TooLarge {
                    kind: ContentLimit::ConversionCapacity,
                    observed: capacity as u64,
                    limit: limits.bytes as u64,
                });
            }
        },
    };
    check()?;
    ensure!(
        stamp == read_worktree_stamp(root, &path)?,
        "worktree changed during conversion"
    );
    finish_bytes(
        bytes,
        representation,
        ContentOrigin::Worktree {
            worktree,
            path,
            stamp,
            conversion,
        },
        limits,
        expected,
        identity,
    )
}

fn read_bounded(
    file: &mut impl Read,
    expected: usize,
    limit: usize,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<u8>> {
    let mut bytes = Vec::new();
    bytes
        .try_reserve_exact(expected)
        .context("reserve admitted worktree content")?;
    ensure!(
        bytes.capacity() <= limit,
        "worktree allocation exceeds content limit"
    );
    let mut scratch = [0; 8192];
    loop {
        check()?;
        let requested = scratch
            .len()
            .min(expected.saturating_sub(bytes.len()).saturating_add(1));
        let count = match file.read(&mut scratch[..requested]) {
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            result => result.context("read bounded worktree content")?,
        };
        if count == 0 {
            break;
        }
        ensure!(
            count <= expected.saturating_sub(bytes.len()),
            "worktree grew during content acquisition"
        );
        bytes.extend_from_slice(&scratch[..count]);
    }
    ensure!(
        bytes.len() == expected,
        "worktree shrank during content acquisition"
    );
    Ok(bytes)
}

fn read_index_stage(
    root: &Path,
    path: &RepositoryPath,
    stage: IndexStage,
    hash: gix::hash::Kind,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Option<ObjectState>> {
    let output = read_command(
        Command::new("git")
            .args([
                "--no-pager",
                "--no-optional-locks",
                "--literal-pathspecs",
                "-C",
            ])
            .arg(root)
            .args([
                "-c",
                "core.fsmonitor=false",
                "ls-files",
                "--stage",
                "-z",
                "--",
            ])
            .arg(resolve_argument(path)?),
        CommandLimits {
            stdout_bytes: 64 * 1024,
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(30),
        },
        &mut *check,
    )?;
    ensure!(
        output.status.success(),
        "Git index read failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stage = match stage {
        IndexStage::Normal => b'0',
        IndexStage::Base => b'1',
        IndexStage::Ours => b'2',
        IndexStage::Theirs => b'3',
    };
    let mut selected = None;
    let mut seen = [false; 4];
    if output.stdout.is_empty() {
        return Ok(None);
    }
    ensure!(
        output.stdout.last() == Some(&0),
        "index stage output is not NUL terminated"
    );
    for entry in output.stdout[..output.stdout.len() - 1].split(|byte| *byte == 0) {
        let separator = entry
            .iter()
            .position(|byte| *byte == b'\t')
            .context("index stage has no path separator")?;
        ensure!(
            &entry[separator + 1..] == path.raw(),
            "index stage returned another path"
        );
        let field = entry[..separator]
            .split(|byte| *byte == b' ')
            .collect::<Vec<_>>();
        ensure!(
            field.len() == 3 && field[2].len() == 1 && matches!(field[2][0], b'0'..=b'3'),
            "invalid index stage record"
        );
        let position = (field[2][0] - b'0') as usize;
        ensure!(!seen[position], "duplicate index stage");
        seen[position] = true;
        let state = parse_object_state(field[0], field[1], hash)?;
        if field[2][0] == stage {
            selected = Some(state);
        }
    }
    check()?;
    Ok(selected)
}

fn finish_bytes(
    bytes: Vec<u8>,
    representation: Representation,
    origin: ContentOrigin,
    limits: ContentLimits,
    expected: Option<SourceIdentity>,
    identity: Option<&mut Option<SourceIdentity>>,
) -> Result<ContentResult> {
    if bytes.capacity() > limits.bytes {
        return Ok(ContentResult::TooLarge {
            kind: ContentLimit::RetainedCapacity,
            observed: bytes.capacity() as u64,
            limit: limits.bytes as u64,
        });
    }
    let bytes = Arc::new(bytes);
    let source = SourceVersion::from_shared(Arc::clone(&bytes), representation);
    if let Some(identity) = identity {
        *identity = Some(match &source {
            Ok(source) => source.identity(),
            Err(_) => SourceIdentity::from_bytes(&bytes, representation),
        });
        ensure!(
            expected.is_none_or(|expected| Some(expected) == *identity),
            "content identity differs from the requested source"
        );
    }
    if limits.lines < bytes.len()
        && let BoundedLineCount::ExceedsLimit { at_least } =
            count_bounded_lines(&bytes, limits.lines)
    {
        return Ok(ContentResult::TooLarge {
            kind: ContentLimit::Lines,
            observed: at_least as u64,
            limit: limits.lines as u64,
        });
    }
    let source = match source {
        Ok(source) => source,
        Err(SourceError::Binary) => return Ok(ContentResult::Binary),
        Err(SourceError::UnsupportedEncoding) => {
            return Ok(ContentResult::Unavailable(
                ContentUnavailable::UnsupportedEncoding,
            ));
        }
        Err(error) => return Err(error.into()),
    };
    finish_source(source, origin, limits, expected)
}

fn finish_source(
    source: SourceVersion,
    origin: ContentOrigin,
    limits: ContentLimits,
    expected: Option<SourceIdentity>,
) -> Result<ContentResult> {
    if source.retained_bytes() > limits.bytes {
        return Ok(ContentResult::TooLarge {
            kind: ContentLimit::RetainedCapacity,
            observed: source.retained_bytes() as u64,
            limit: limits.bytes as u64,
        });
    }
    if source.newline().line_count > limits.lines {
        return Ok(ContentResult::TooLarge {
            kind: ContentLimit::Lines,
            observed: source.newline().line_count as u64,
            limit: limits.lines as u64,
        });
    }
    ensure!(
        expected.is_none_or(|expected| expected == source.identity()),
        "content identity differs from the requested source"
    );
    Ok(ContentResult::Ready(FileContent { source, origin }))
}

/// Counts LF-delimited lines without adding a line for a terminal LF, stopping at the first excess.
pub fn count_bounded_lines(bytes: &[u8], limit: usize) -> BoundedLineCount {
    let mut count = 0;
    for byte in bytes {
        if *byte == b'\n' {
            count += 1;
            if count > limit {
                return BoundedLineCount::ExceedsLimit { at_least: count };
            }
        }
    }
    count += usize::from(!bytes.is_empty() && bytes.last() != Some(&b'\n'));
    if count > limit {
        BoundedLineCount::ExceedsLimit { at_least: count }
    } else {
        BoundedLineCount::Exact(count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::git;

    #[test]
    fn bounded_lines_preserve_empty_final_newline_and_lower_bound_semantics() {
        for (bytes, count) in [
            (b"".as_slice(), 0),
            (b"\n", 1),
            (b"one", 1),
            (b"one\r\n", 1),
            (b"one\ntwo", 2),
            (b"one\rtwo", 1),
        ] {
            assert_eq!(
                count_bounded_lines(bytes, count),
                BoundedLineCount::Exact(count)
            );
        }
        assert_eq!(
            count_bounded_lines(b"one\ntwo\nthree\nfour", 2),
            BoundedLineCount::ExceedsLimit { at_least: 3 }
        );
        assert_eq!(
            count_bounded_lines(b"one", 0),
            BoundedLineCount::ExceedsLimit { at_least: 1 }
        );
    }

    #[test]
    fn bounded_stream_rejects_growth_shrinkage_and_cancellation() {
        assert!(
            read_bounded(&mut b"longer".as_slice(), 3, 3, &mut || Ok(()))
                .unwrap_err()
                .to_string()
                .contains("grew")
        );
        assert!(
            read_bounded(&mut b"short".as_slice(), 8, 8, &mut || Ok(()))
                .unwrap_err()
                .to_string()
                .contains("shrank")
        );
        assert_eq!(
            read_bounded(&mut b"exact".as_slice(), 5, 5, &mut || Ok(())).unwrap(),
            b"exact"
        );
        assert_eq!(
            read_bounded(&mut b"bytes".as_slice(), 5, 5, &mut || anyhow::bail!(
                "cancelled"
            ))
            .unwrap_err()
            .to_string(),
            "cancelled"
        );
    }

    #[test]
    fn worktree_change_after_open_rejects_acquisition() {
        let fixture = tempfile::tempdir().unwrap();
        let root = fixture.path();
        git(root, &["init", "--quiet"]);
        std::fs::write(root.join("file"), b"initial contents").unwrap();
        let (identity, shared) = crate::identity::discover_repository(root).unwrap().unwrap();
        let mut local = shared.to_thread_local();
        let repository = RepositoryState::new(identity, shared);
        let mut checks = 0;
        let result = read_worktree(
            &mut local,
            &repository.identity,
            WorktreeRead {
                path: RepositoryPath::new(b"file".to_vec()).unwrap(),
                conversion: WorktreeConversion::Raw,
                limits: ContentLimits::default(),
                expected: None,
                session: None,
            },
            Some(&mut None),
            &mut || {
                checks += 1;
                if checks == 2 {
                    std::fs::write(root.join("file"), b"changed")?;
                }
                Ok(())
            },
        );
        assert!(result.unwrap_err().to_string().contains("shrank"));
    }

    #[test]
    fn binary_classification_retains_exact_canonical_identity() {
        let mut identity = None;
        let result = finish_bytes(
            b"binary\0one".to_vec(),
            Representation::GitCanonical,
            ContentOrigin::Supplied,
            ContentLimits::default(),
            None,
            Some(&mut identity),
        )
        .unwrap();
        assert!(matches!(result, ContentResult::Binary));
        assert_eq!(
            identity,
            Some(SourceIdentity::from_bytes(
                b"binary\0one",
                Representation::GitCanonical
            ))
        );
        let result = finish_bytes(
            b"binary\0two".to_vec(),
            Representation::GitCanonical,
            ContentOrigin::Supplied,
            ContentLimits::default(),
            identity,
            Some(&mut None),
        );
        assert!(result.unwrap_err().to_string().contains("identity differs"));
    }
}
