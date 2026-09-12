use crate::StatusSection;
use crate::document::{DOCUMENT_BYTES, FileTarget, StatusDocument, context_cost, inventory};
use crate::protocol::*;
use anyhow::{Result, ensure};
use forge_git::snapshot::PathState;
use std::collections::HashSet;

pub(crate) fn reconcile(
    document: &mut StatusDocument,
    observed_head: &forge_git::snapshot::HeadState,
    observed_path: &[forge_git::snapshot::ObservedPath],
    context: Option<crate::context::StatusContext>,
    ignored: crate::ignored::IgnoredPathSet,
) -> Result<Option<StatusDelta>> {
    document._admission.check()?;
    let before = document.snapshot();
    let mut sequence = document.sequence;
    let (order, mut file, mut retained_bytes) =
        inventory(observed_path, &ignored, &document.file, &mut sequence)?;
    retained_bytes += context_cost(context.as_ref())
        + document
            .local_path
            .as_ref()
            .map_or(0, |path| path.retained_bytes());
    let mut unchanged = HashSet::new();
    for (target, next) in &mut file {
        if let Some(previous) = document.file.get(target) {
            if same_source(previous, next) {
                unchanged.insert(target.clone());
                retained_bytes += previous.retained_bytes;
            } else {
                ensure!(
                    previous.generation < forge_buffer::MAX_COUNTER,
                    "body generation exhausted"
                );
                next.generation = previous.generation + 1;
            }
        }
    }
    ensure!(
        retained_bytes <= DOCUMENT_BYTES,
        "status refresh exceeds 16 MiB document budget"
    );
    let next_revision = document.revision.next()?;
    for target in &unchanged {
        let mut previous = document.file.remove(target).expect("retained file");
        let next = file.get_mut(target).expect("replacement file");
        previous.record = next.record.clone();
        previous.stamp = next.stamp.clone();
        previous.origin_stamp = next.origin_stamp.clone();
        *next = previous;
    }
    document.file = file;
    document.order = order;
    document.sequence = sequence;
    document
        .hunk
        .retain(|_, hunk| unchanged.contains(&hunk.file));
    document.observed_head = observed_head.clone();
    document.context = context;
    document.ignored = ignored;
    document.retained_bytes = retained_bytes;
    let after = document.snapshot();
    if before == after {
        return Ok(None);
    }
    document.revision = next_revision;
    Ok(Some(delta(&before, &document.snapshot())))
}

pub(crate) fn delta(before: &crate::StatusSnapshot, after: &crate::StatusSnapshot) -> StatusDelta {
    let previous: std::collections::HashMap<_, _> =
        before.file.iter().map(|file| (file.id, file)).collect();
    let current: HashSet<_> = after.file.iter().map(|file| file.id).collect();
    let mut section: Vec<_> = after
        .section
        .iter()
        .filter(|section| !before.section.contains(section))
        .cloned()
        .collect();
    for previous in &before.section {
        if !after
            .section
            .iter()
            .any(|section| section.kind == previous.kind)
        {
            section.push(StatusSectionRecord {
                kind: previous.kind,
                file: Vec::new(),
            });
        }
    }
    StatusDelta {
        document: after.document.clone(),
        base: before.revision,
        next: after.revision,
        removed: before
            .file
            .iter()
            .filter(|file| !current.contains(&file.id))
            .map(|file| file.id)
            .collect(),
        file: after
            .file
            .iter()
            .filter(|file| {
                previous
                    .get(&file.id)
                    .is_none_or(|previous| **previous != **file)
            })
            .cloned()
            .collect(),
        section,
        pending: after.pending.clone(),
        head: (before.head != after.head).then_some(after.head.clone()),
        context: (before.context != after.context)
            .then_some(after.context.clone())
            .flatten(),
    }
}

fn same_source(previous: &FileTarget, next: &FileTarget) -> bool {
    if previous.section != next.section || previous.record.path != next.record.path {
        return false;
    }
    if next.section != StatusSection::Staged
        && (previous.stamp == forge_git::snapshot::WorktreeStamp::Unobserved
            || next.stamp == forge_git::snapshot::WorktreeStamp::Unobserved)
    {
        return false;
    }
    match (&previous.record.state, &next.record.state, next.section) {
        (
            PathState::Tracked {
                head: previous_head,
                index: previous_index,
                ..
            },
            PathState::Tracked {
                head: next_head,
                index: next_index,
                ..
            },
            StatusSection::Staged,
        ) => {
            previous_head == next_head
                && previous_index == next_index
                && previous.record.staged == next.record.staged
        }
        (
            PathState::Tracked {
                index: previous_index,
                ..
            },
            PathState::Tracked {
                index: next_index, ..
            },
            StatusSection::Unstaged,
        ) => {
            previous_index == next_index
                && previous.stamp == next.stamp
                && previous.record.unstaged == next.record.unstaged
        }
        _ => {
            previous.record.state == next.record.state
                && previous.record.staged == next.record.staged
                && previous.record.unstaged == next.record.unstaged
                && previous.record.worktree_mode == next.record.worktree_mode
                && previous.record.submodule == next.record.submodule
                && previous.record.content == next.record.content
                && previous.stamp == next.stamp
                && previous.origin_stamp == next.origin_stamp
        }
    }
}
