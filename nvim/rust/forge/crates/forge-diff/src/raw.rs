//! Exact change ranges retain source identity independently of display context and truncation.

use std::fmt;
use std::ops::Range;

use imara_diff::{Algorithm, Diff, InternedInput};
use sha2::{Digest, Sha256};

use crate::source::{Representation, SourceError, SourcePair, validate_source_pair};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RawHunkId([u8; 32]);

/// Uses zero-based half-open line and byte ranges in the exact source pair.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RawHunk {
    pub id: RawHunkId,
    pub old_lines: Range<usize>,
    pub new_lines: Range<usize>,
    pub old_bytes: Range<usize>,
    pub new_bytes: Range<usize>,
}

#[derive(Clone, Debug)]
pub struct RawDiff {
    source: SourcePair,
    hunk: Vec<RawHunk>,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct LineCounts {
    pub added: u64,
    pub deleted: u64,
}

/// Contains all removed and added lines, including missing-final-newline markers, without headers.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PatchBody {
    pub bytes: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PatchError {
    NonCanonicalSource,
    UnknownHunk,
}

impl fmt::Display for PatchError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Self::NonCanonicalSource => "patch bodies require Git-canonical source bytes",
            Self::UnknownHunk => "raw hunk does not belong to this source pair",
        })
    }
}

impl std::error::Error for PatchError {}

impl RawDiff {
    pub fn line_counts(&self) -> LineCounts {
        LineCounts {
            added: self
                .hunk
                .iter()
                .map(|hunk| hunk.new_lines.len() as u64)
                .sum(),
            deleted: self
                .hunk
                .iter()
                .map(|hunk| hunk.old_lines.len() as u64)
                .sum(),
        }
    }
    pub fn source(&self) -> &SourcePair {
        &self.source
    }

    pub fn hunks(&self) -> &[RawHunk] {
        &self.hunk
    }

    pub fn retained_result_bytes(&self) -> usize {
        std::mem::size_of::<Self>() + self.hunk.capacity() * std::mem::size_of::<RawHunk>()
    }

    pub fn retained_source_bytes(&self) -> usize {
        self.source.old.retained_bytes()
            + if self.source.old.shares_bytes(&self.source.new) {
                0
            } else {
                self.source.new.retained_bytes()
            }
    }
}

/// Computes one pinned histogram result. Empty-side changes bypass tokenization and comparison.
pub fn compute_hunks(source: SourcePair) -> Result<RawDiff, SourceError> {
    validate_source_pair(&source)?;
    if source.old.identity() == source.new.identity() {
        return Ok(RawDiff {
            source,
            hunk: Vec::new(),
        });
    }
    if let Some(result) = synthetic_hunk(source.clone()) {
        return Ok(result);
    }
    let old_offset = line_offsets(source.old.bytes());
    let new_offset = line_offsets(source.new.bytes());
    let comparison = compare_lines(&source);
    let hunk = comparison
        .hunks()
        .map(|change| {
            let old_lines = change.before.start as usize..change.before.end as usize;
            let new_lines = change.after.start as usize..change.after.end as usize;
            make_hunk(
                &source,
                old_offset[old_lines.start]..old_offset[old_lines.end],
                new_offset[new_lines.start]..new_offset[new_lines.end],
                old_lines,
                new_lines,
            )
        })
        .collect();
    Ok(RawDiff { source, hunk })
}

/// Counts exact changes without constructing display hunks or their byte ranges and hashes.
pub fn compute_counts(source: &SourcePair) -> LineCounts {
    if source.old.bytes() == source.new.bytes() {
        return LineCounts::default();
    }
    if source.old.bytes().is_empty() || source.new.bytes().is_empty() {
        return LineCounts {
            added: source.new.newline().line_count as u64,
            deleted: source.old.newline().line_count as u64,
        };
    }
    let comparison = compare_lines(source);
    LineCounts {
        added: comparison.hunks().map(|hunk| hunk.after.len() as u64).sum(),
        deleted: comparison
            .hunks()
            .map(|hunk| hunk.before.len() as u64)
            .sum(),
    }
}

fn compare_lines(source: &SourcePair) -> Diff {
    let input = InternedInput::new(source.old.bytes(), source.new.bytes());
    let mut comparison = Diff::compute(Algorithm::Histogram, &input);
    comparison.postprocess_lines(&input);
    comparison
}

/// Returns an exact added/deleted result only when at least one source side is empty.
pub fn synthetic_hunk(source: SourcePair) -> Option<RawDiff> {
    if !source.old.bytes().is_empty() && !source.new.bytes().is_empty() {
        return None;
    }
    let hunk = if source.old.bytes().is_empty() && source.new.bytes().is_empty() {
        Vec::new()
    } else {
        vec![make_hunk(
            &source,
            0..source.old.bytes().len(),
            0..source.new.bytes().len(),
            0..source.old.newline().line_count,
            0..source.new.newline().line_count,
        )]
    };
    Some(RawDiff { source, hunk })
}

/// Extracts complete canonical changes. The Git owner must still verify mutation preconditions.
pub fn patch_body(diff: &RawDiff, identity: RawHunkId) -> Result<PatchBody, PatchError> {
    if diff.source.old.identity().representation != Representation::GitCanonical
        || diff.source.new.identity().representation != Representation::GitCanonical
    {
        return Err(PatchError::NonCanonicalSource);
    }
    let hunk = diff
        .hunk
        .iter()
        .find(|hunk| hunk.id == identity)
        .ok_or(PatchError::UnknownHunk)?;
    let mut bytes = Vec::new();
    append_patch_lines(
        &mut bytes,
        b'-',
        &diff.source.old.bytes()[hunk.old_bytes.clone()],
    );
    append_patch_lines(
        &mut bytes,
        b'+',
        &diff.source.new.bytes()[hunk.new_bytes.clone()],
    );
    Ok(PatchBody { bytes })
}

fn make_hunk(
    source: &SourcePair,
    old_bytes: Range<usize>,
    new_bytes: Range<usize>,
    old_lines: Range<usize>,
    new_lines: Range<usize>,
) -> RawHunk {
    let mut digest = Sha256::new();
    digest.update(b"forge-raw-hunk-v1");
    for identity in [source.old.identity(), source.new.identity()] {
        digest.update(identity.content_hash);
        digest.update([match identity.representation {
            Representation::Raw => 0,
            Representation::GitCanonical => 1,
            Representation::DisplayOnly => 2,
        }]);
    }
    for position in [
        old_bytes.start,
        old_bytes.end,
        new_bytes.start,
        new_bytes.end,
    ] {
        digest.update((position as u64).to_le_bytes());
    }
    RawHunk {
        id: RawHunkId(digest.finalize().into()),
        old_lines,
        new_lines,
        old_bytes,
        new_bytes,
    }
}

fn line_offsets(content: &[u8]) -> Vec<usize> {
    let mut offset = vec![0];
    for (position, byte) in content.iter().enumerate() {
        if *byte == b'\n' {
            offset.push(position + 1);
        }
    }
    if offset.last().copied() != Some(content.len()) {
        offset.push(content.len());
    }
    offset
}

fn append_patch_lines(output: &mut Vec<u8>, prefix: u8, content: &[u8]) {
    if content.is_empty() {
        return;
    }
    for line in content.split_inclusive(|byte| *byte == b'\n') {
        output.push(prefix);
        output.extend_from_slice(line);
        if line.last() != Some(&b'\n') {
            output.extend_from_slice(b"\n\\ No newline at end of file\n");
        }
    }
}
