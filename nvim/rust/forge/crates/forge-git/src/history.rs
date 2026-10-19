use anyhow::{Context, Result};

/// Complete message of one reachable commit, in newest-first traversal order.
pub struct HistoryCommit {
    /// Commit object identity.
    pub id: String,
    /// Complete message including its body.
    pub message: String,
}

/// Read a requested number of recent commits without limiting message sizes.
pub fn recent(
    repository: &gix::Repository,
    head: gix::ObjectId,
    count: usize,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<Vec<HistoryCommit>> {
    let commit = repository.find_object(head)?.try_into_commit()?;
    let walk = commit
        .ancestors()
        .sorting(gix::revision::walk::Sorting::ByCommitTime(
            gix::traverse::commit::simple::CommitTimeOrder::NewestFirst,
        ))
        .all()?;
    let mut history = Vec::new();
    for entry in walk.take(count) {
        check()?;
        let entry = entry?;
        let commit = repository.find_object(entry.id)?.try_into_commit()?;
        history.push(HistoryCommit {
            id: entry.id.to_string(),
            message: String::from_utf8(commit.message_raw()?.to_vec())
                .context("Decode Git commit message as UTF-8")?,
        });
    }
    Ok(history)
}
