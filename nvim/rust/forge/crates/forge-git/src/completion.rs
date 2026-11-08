use std::{
    collections::HashMap,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};

use crate::{
    GitStorageId,
    repository::{RepositoryGeneration, RepositoryRead, RepositoryState},
    store::RepositoryStore,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RevisionId(pub u64);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RevisionProvider {
    Gix,
}

/// Immutable, sorted raw revision arguments accepted for one repository generation.
#[derive(Debug)]
pub struct RevisionCandidates {
    pub revision: RevisionId,
    pub generation: RepositoryGeneration,
    pub storage: GitStorageId,
    pub provider: RevisionProvider,
    pub reference_digest: String,
    pub values: Vec<Vec<u8>>,
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
    retained_bytes: usize,
}

impl RepositoryState {
    /// Refreshes local and remote branch arguments on the shared read pool.
    ///
    /// Two equal complete enumerations fence detected ref changes. A newer request or repository
    /// invalidation rejects adoption. Failed refreshes preserve the previous accepted snapshot.
    /// Native filesystem waits can outlive the 30-second cooperative deadline.
    pub async fn refresh_revisions(
        self: &Arc<Self>,
        store: &RepositoryStore,
    ) -> Result<Arc<RevisionCandidates>> {
        let request = self.begin_revisions()?;
        let worker = Arc::clone(self);
        let result = store
            .read(
                Arc::clone(self),
                size_of::<RevisionRequest>() + size_of::<Arc<Self>>(),
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
                        collect_candidates(local.object_hash(), &mut check, |check| {
                            enumerate(&local, check)
                        })?;
                    Ok(RevisionCandidates {
                        revision: request.revision,
                        generation: request.generation,
                        storage: worker.identity.storage.clone(),
                        provider: RevisionProvider::Gix,
                        reference_digest: format!("{:064x}", request.revision.0),
                        values: candidates.values,
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
    check: &mut dyn FnMut() -> Result<()>,
    mut enumerate: impl FnMut(&mut dyn FnMut() -> Result<()>) -> Result<Vec<u8>>,
) -> Result<CandidateValues> {
    check()?;
    let first = enumerate(check)?;
    let candidates = build_candidates(&first, hash)?;
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
    repository: &gix::Repository,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<u8>> {
    use gix::bstr::ByteSlice;
    check()?;
    let platform = repository.references()?;
    let references = platform.all()?;
    let mut names = Vec::new();
    let mut branches = Vec::new();
    for reference in references {
        check()?;
        let reference =
            reference.map_err(|error| anyhow::anyhow!("enumerate revision reference: {error}"))?;
        let name = reference.name().as_bstr().as_bytes().to_vec();
        if name.starts_with(b"refs/heads/") || name.starts_with(b"refs/remotes/") {
            branches.push(name.clone());
        }
        names.push(name);
    }
    branches.sort_unstable();
    branches.dedup();
    let mut short_counts = HashMap::new();
    for name in &names {
        *short_counts.entry(abbreviated_name(name)).or_insert(0usize) += 1;
    }
    let mut output = Vec::new();
    for name in branches {
        check()?;
        let abbreviated = abbreviated_name(&name);
        let collisions = short_counts.get(&abbreviated).copied().unwrap_or(0);
        let short = if collisions == 1 {
            abbreviated
        } else {
            name.strip_prefix(b"refs/")
                .context("invalid reference namespace")?
                .to_vec()
        };
        let object = repository
            .find_reference(gix::bstr::BStr::new(&name))?
            .peel_to_id()?
            .detach();
        output.extend_from_slice(&name);
        output.push(0);
        output.extend_from_slice(&short);
        output.push(0);
        output.extend_from_slice(object.to_string().as_bytes());
        output.extend_from_slice(b"\0\n");
    }
    Ok(output)
}

fn abbreviated_name(name: &[u8]) -> Vec<u8> {
    for prefix in [b"refs/heads/".as_slice(), b"refs/remotes/", b"refs/tags/"] {
        if let Some(short) = name.strip_prefix(prefix) {
            if prefix == b"refs/remotes/" && short.ends_with(b"/HEAD") {
                return short[..short.len() - b"/HEAD".len()].to_vec();
            }
            return short.to_vec();
        }
    }
    name.to_vec()
}

fn build_candidates(bytes: &[u8], hash: gix::hash::Kind) -> Result<CandidateValues> {
    ensure!(
        bytes.is_empty() || bytes.ends_with(b"\n"),
        "revision enumeration is truncated"
    );
    let record_count = bytes.iter().filter(|byte| **byte == b'\n').count();
    let mut values = Vec::new();
    values
        .try_reserve_exact(record_count)
        .context("reserve revision candidate index")?;
    let mut retained_bytes = values.capacity() * size_of::<Vec<u8>>();
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
        let mut value = Vec::new();
        value
            .try_reserve_exact(short.len())
            .context("reserve revision argument")?;
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
        retained_bytes,
    })
}
