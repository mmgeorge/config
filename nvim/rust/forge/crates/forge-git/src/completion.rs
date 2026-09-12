use std::{
    process::Command,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};

use crate::{
    GitStorageId,
    command::{CommandLimits, read_command},
    repository::{RepositoryGeneration, RepositoryRead, RepositoryState},
    store::RepositoryStore,
};

const MAX_CANDIDATES: usize = 20_000;
const MAX_CANDIDATE_BYTES: usize = 2 * 1024 * 1024;
const MAX_ENUMERATION_BYTES: usize = 16 * 1024 * 1024;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RevisionId(pub u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RevisionBackend {
    GitForEachRef,
}

/// Bounds candidate storage independently from bounded Git enumeration output.
#[derive(Clone, Copy, Debug)]
pub struct CandidateLimits {
    items: usize,
    bytes: usize,
}

/// Immutable, sorted raw revision arguments accepted for one repository generation.
#[derive(Debug)]
pub struct RevisionCandidates {
    pub revision: RevisionId,
    pub generation: RepositoryGeneration,
    pub storage: GitStorageId,
    pub backend: RevisionBackend,
    pub reference_digest: String,
    pub values: Vec<Vec<u8>>,
    pub truncated: bool,
    pub retained_bytes: usize,
}

#[derive(Default)]
pub(crate) struct RevisionState {
    requested: u64,
    pub(crate) current: Option<Arc<RevisionCandidates>>,
}

#[derive(Clone, Copy)]
struct RevisionRequest {
    revision: RevisionId,
    generation: RepositoryGeneration,
}

struct CandidateValues {
    values: Vec<Vec<u8>>,
    truncated: bool,
    retained_bytes: usize,
}

impl Default for CandidateLimits {
    fn default() -> Self {
        Self {
            items: MAX_CANDIDATES,
            bytes: MAX_CANDIDATE_BYTES,
        }
    }
}

impl CandidateLimits {
    /// Accepts lower limits, including zero, without increasing the shared cache ceilings.
    pub fn new(items: usize, bytes: usize) -> Result<Self> {
        ensure!(
            items <= MAX_CANDIDATES && bytes <= MAX_CANDIDATE_BYTES,
            "revision candidate limits exceed supported ceilings"
        );
        Ok(Self { items, bytes })
    }
}

impl RepositoryState {
    /// Refreshes local and remote branch arguments on the shared read pool.
    ///
    /// Two equal bounded enumerations fence detected ref changes. A newer request or repository
    /// invalidation rejects adoption. Failed refreshes preserve the previous accepted snapshot.
    /// Native filesystem waits can outlive the 30-second cooperative deadline.
    pub async fn refresh_revisions(
        self: &Arc<Self>,
        store: &RepositoryStore,
        limits: CandidateLimits,
    ) -> Result<Arc<RevisionCandidates>> {
        let request = self.begin_revisions()?;
        let worker = Arc::clone(self);
        let result = store
            .read(
                Arc::clone(self),
                size_of::<RevisionRequest>()
                    + size_of::<CandidateLimits>()
                    + size_of::<Arc<Self>>(),
                move |local, cancellation| {
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            worker.generation() == request.generation,
                            "revision refresh was invalidated"
                        );
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "revision refresh exceeded 30-second deadline"
                        );
                        Ok(())
                    };
                    let candidates =
                        collect_candidates(local.object_hash(), limits, &mut check, |check| {
                            enumerate(&worker, limits, check)
                        })?;
                    Ok(RevisionCandidates {
                        revision: request.revision,
                        generation: request.generation,
                        storage: worker.identity.storage.clone(),
                        backend: RevisionBackend::GitForEachRef,
                        reference_digest: format!("{:064x}", request.revision.0),
                        values: candidates.values,
                        truncated: candidates.truncated,
                        retained_bytes: candidates.retained_bytes,
                    })
                },
            )
            .await?;
        self.adopt_revisions(result)
    }

    /// Returns the last accepted snapshot without repository I/O.
    pub fn current_revisions(&self) -> Option<Arc<RevisionCandidates>> {
        self.revision
            .lock()
            .expect("repository revision lock")
            .current
            .clone()
    }

    fn begin_revisions(&self) -> Result<RevisionRequest> {
        let mut state = self.revision.lock().expect("repository revision lock");
        state.requested = state
            .requested
            .checked_add(1)
            .context("revision candidate sequence exhausted")?;
        Ok(RevisionRequest {
            revision: RevisionId(state.requested),
            generation: self.generation(),
        })
    }

    fn adopt_revisions(
        &self,
        result: RepositoryRead<RevisionCandidates>,
    ) -> Result<Arc<RevisionCandidates>> {
        let mut state = self.revision.lock().expect("repository revision lock");
        ensure!(
            result.generation == result.value.generation && result.generation == self.generation(),
            "revision candidate generation was superseded"
        );
        ensure!(
            result.value.storage == self.identity.storage,
            "revision candidates belong to another repository"
        );
        ensure!(
            result.value.revision.0 == state.requested,
            "revision candidate request was superseded"
        );
        ensure!(
            state
                .current
                .as_ref()
                .is_none_or(|current| current.revision < result.value.revision),
            "revision candidates were already adopted"
        );
        let candidates = Arc::new(result.value);
        let previous = state.current.replace(Arc::clone(&candidates));
        drop(state);
        drop(previous);
        Ok(candidates)
    }
}

fn collect_candidates(
    hash: gix::hash::Kind,
    limits: CandidateLimits,
    check: &mut dyn FnMut() -> Result<()>,
    mut enumerate: impl FnMut(&mut dyn FnMut() -> Result<()>) -> Result<Vec<u8>>,
) -> Result<CandidateValues> {
    check()?;
    let first = enumerate(check)?;
    let candidates = build_candidates(&first, hash, limits)?;
    check()?;
    let second = enumerate(check)?;
    ensure!(
        first == second,
        "references changed during candidate refresh"
    );
    check()?;
    Ok(candidates)
}

fn enumerate(
    repository: &RepositoryState,
    limits: CandidateLimits,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<u8>> {
    let output = read_command(
        Command::new("git")
            .args(["--no-pager", "--no-optional-locks", "-C"])
            .arg(
                repository
                    .identity
                    .worktree_root
                    .as_ref()
                    .unwrap_or(&repository.identity.git_directory),
            )
            .args([
                "for-each-ref",
                "--sort=refname",
                "--format=%(refname)%00%(refname:short)%00%(objectname)%00",
            ])
            .arg(format!("--count={}", limits.items + 1))
            .args(["refs/heads", "refs/remotes"]),
        CommandLimits {
            stdout_bytes: MAX_ENUMERATION_BYTES,
            stderr_bytes: 64 * 1024,
            timeout: Duration::from_secs(30),
        },
        check,
    )?;
    ensure!(
        output.status.success(),
        "Git revision enumeration failed with {}: {}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(output.stdout)
}

fn build_candidates(
    bytes: &[u8],
    hash: gix::hash::Kind,
    limits: CandidateLimits,
) -> Result<CandidateValues> {
    ensure!(
        bytes.len() <= MAX_ENUMERATION_BYTES,
        "revision enumeration exceeds byte limit"
    );
    ensure!(
        bytes.is_empty() || bytes.ends_with(b"\n"),
        "revision enumeration is truncated"
    );
    let record_count = bytes.iter().filter(|byte| **byte == b'\n').count();
    ensure!(
        record_count <= limits.items + 1,
        "revision enumeration exceeds record limit"
    );
    let capacity = record_count
        .min(limits.items)
        .min(limits.bytes / size_of::<Vec<u8>>());
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .context("reserve revision candidate index")?;
    let mut retained_bytes = values.capacity() * size_of::<Vec<u8>>();
    ensure!(
        retained_bytes <= limits.bytes,
        "revision candidate index exceeds byte limit"
    );
    let mut truncated = false;
    let mut previous: Option<&[u8]> = None;
    for record in bytes.split_inclusive(|byte| *byte == b'\n') {
        let mut field = record[..record.len() - 1].split(|byte| *byte == 0);
        let reference = field.next().context("missing full revision name")?;
        let short = field.next().context("missing short revision name")?;
        let object = field.next().context("missing revision object")?;
        ensure!(
            field.next() == Some(b"".as_slice()) && field.next().is_none(),
            "invalid revision record framing"
        );
        ensure!(
            (reference.starts_with(b"refs/heads/") || reference.starts_with(b"refs/remotes/"))
                && !short.is_empty(),
            "invalid revision candidate namespace"
        );
        ensure!(
            !reference
                .iter()
                .chain(short)
                .any(|byte| *byte <= b' ' || *byte == 127),
            "invalid revision candidate control byte"
        );
        ensure!(
            previous.is_none_or(|previous| previous < reference),
            "revision enumeration is not strictly sorted"
        );
        previous = Some(reference);
        ensure!(
            gix::ObjectId::from_hex(object)?.kind() == hash,
            "revision object hash format mismatch"
        );
        if truncated
            || values.len() == capacity
            || short.len() > limits.bytes.saturating_sub(retained_bytes)
        {
            truncated = true;
            continue;
        }
        let mut value = Vec::new();
        value
            .try_reserve_exact(short.len())
            .context("reserve revision argument")?;
        ensure!(
            value.capacity() <= limits.bytes.saturating_sub(retained_bytes),
            "revision allocation exceeds byte limit"
        );
        value.extend_from_slice(short);
        retained_bytes += value.capacity();
        values.push(value);
    }
    values.sort_unstable();
    ensure!(
        !values.windows(2).any(|pair| pair[0] == pair[1]),
        "revision enumeration returned duplicate arguments"
    );
    Ok(CandidateValues {
        values,
        truncated,
        retained_bytes,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::git;

    fn record(reference: &[u8], short: &[u8]) -> Vec<u8> {
        [
            reference,
            b"\0",
            short,
            b"\0",
            b"1111111111111111111111111111111111111111\0\n",
        ]
        .concat()
    }

    #[test]
    fn candidates_preserve_raw_arguments_and_bound_index_and_value_capacity() {
        let bytes = [
            record(b"refs/heads/a", b"heads/a"),
            record(b"refs/heads/b", b"b"),
            record(b"refs/remotes/origin/\xff", b"origin/\xff"),
        ]
        .concat();
        let complete =
            build_candidates(&bytes, gix::hash::Kind::Sha1, CandidateLimits::default()).unwrap();
        assert_eq!(
            complete.values,
            [b"b".to_vec(), b"heads/a".to_vec(), b"origin/\xff".to_vec()]
        );
        assert!(!complete.truncated);
        assert_eq!(
            complete.retained_bytes,
            complete.values.capacity() * size_of::<Vec<u8>>()
                + complete.values.iter().map(Vec::capacity).sum::<usize>()
        );
        let limited = build_candidates(
            &bytes,
            gix::hash::Kind::Sha1,
            CandidateLimits::new(2, MAX_CANDIDATE_BYTES).unwrap(),
        )
        .unwrap();
        assert_eq!(limited.values.len(), 2);
        assert!(limited.truncated);
        let bytes_limit = 3 * size_of::<Vec<u8>>() + 7;
        let limited = build_candidates(
            &bytes,
            gix::hash::Kind::Sha1,
            CandidateLimits::new(3, bytes_limit).unwrap(),
        )
        .unwrap();
        assert_eq!(limited.values, [b"heads/a".to_vec()]);
        assert!(limited.truncated);
        assert!(limited.retained_bytes <= bytes_limit);
        let zero = build_candidates(
            &record(b"refs/heads/a", b"a"),
            gix::hash::Kind::Sha1,
            CandidateLimits::new(0, 0).unwrap(),
        )
        .unwrap();
        assert!(zero.values.is_empty() && zero.truncated);
        assert_eq!(zero.retained_bytes, 0);
    }

    #[test]
    fn malformed_and_oversized_enumerations_do_not_publish_partial_candidates() {
        for bytes in [
            b"refs/heads/a\0a\0".to_vec(),
            record(b"refs/tags/a", b"a"),
            record(b"refs/heads/a", b"a b"),
            [record(b"refs/heads/b", b"b"), record(b"refs/heads/a", b"a")].concat(),
            [record(b"refs/heads/a", b"a"), record(b"refs/heads/b", b"a")].concat(),
        ] {
            assert!(
                build_candidates(&bytes, gix::hash::Kind::Sha1, CandidateLimits::default())
                    .is_err()
            );
        }
        assert!(
            build_candidates(
                &record(b"refs/heads/a", b"a"),
                gix::hash::Kind::Sha256,
                CandidateLimits::default()
            )
            .is_err()
        );
        assert!(CandidateLimits::new(MAX_CANDIDATES + 1, 0).is_err());
        assert!(CandidateLimits::new(0, MAX_CANDIDATE_BYTES + 1).is_err());
        assert!(
            build_candidates(
                &[record(b"refs/heads/a", b"a"), record(b"refs/heads/b", b"b")].concat(),
                gix::hash::Kind::Sha1,
                CandidateLimits::new(0, 0).unwrap()
            )
            .is_err()
        );
    }

    #[test]
    fn ref_changes_and_cancellation_between_enumerations_reject_collection() {
        let mut enumeration = 0;
        let changed = collect_candidates(
            gix::hash::Kind::Sha1,
            CandidateLimits::default(),
            &mut || Ok(()),
            |_| {
                enumeration += 1;
                Ok(record(
                    if enumeration == 1 {
                        b"refs/heads/a"
                    } else {
                        b"refs/heads/b"
                    },
                    if enumeration == 1 { b"a" } else { b"b" },
                ))
            },
        );
        assert!(
            changed
                .err()
                .unwrap()
                .to_string()
                .contains("references changed")
        );
        assert_eq!(enumeration, 2);
        let mut checks = 0;
        enumeration = 0;
        let cancelled = collect_candidates(
            gix::hash::Kind::Sha1,
            CandidateLimits::default(),
            &mut || {
                checks += 1;
                ensure!(checks < 2, "cancelled between enumerations");
                Ok(())
            },
            |_| {
                enumeration += 1;
                Ok(record(b"refs/heads/a", b"a"))
            },
        );
        assert!(cancelled.err().unwrap().to_string().contains("cancelled"));
        assert_eq!(enumeration, 1);
    }

    #[tokio::test]
    async fn superseded_duplicate_and_invalidated_refreshes_cannot_replace_current_candidates() {
        let fixture = tempfile::tempdir().unwrap();
        git(fixture.path(), &["init", "--quiet"]);
        let store = RepositoryStore::default();
        let repository = store
            .open(fixture.path().to_path_buf())
            .await
            .unwrap()
            .unwrap();
        let older = repository.begin_revisions().unwrap();
        let newer = repository.begin_revisions().unwrap();
        assert!(
            repository
                .adopt_revisions(result(&repository, older))
                .is_err()
        );
        let accepted = repository
            .adopt_revisions(result(&repository, newer))
            .unwrap();
        assert!(Arc::ptr_eq(
            &accepted,
            &repository.current_revisions().unwrap()
        ));
        assert!(
            repository
                .adopt_revisions(result(&repository, newer))
                .is_err()
        );
        let pending = repository.begin_revisions().unwrap();
        repository.invalidate().unwrap();
        assert!(repository.current_revisions().is_none());
        assert!(
            repository
                .adopt_revisions(result(&repository, pending))
                .is_err()
        );
        assert_eq!(accepted.revision, newer.revision);
    }

    fn result(
        repository: &RepositoryState,
        request: RevisionRequest,
    ) -> RepositoryRead<RevisionCandidates> {
        RepositoryRead {
            generation: request.generation,
            value: RevisionCandidates {
                revision: request.revision,
                generation: request.generation,
                storage: repository.identity.storage.clone(),
                backend: RevisionBackend::GitForEachRef,
                reference_digest: format!("{:064x}", request.revision.0),
                values: Vec::new(),
                truncated: false,
                retained_bytes: 0,
            },
        }
    }
}
