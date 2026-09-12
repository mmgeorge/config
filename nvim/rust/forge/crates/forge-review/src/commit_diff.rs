use std::collections::BTreeMap;

use anyhow::{Result, ensure};
use forge_github::review_source::{ReviewCommitDetail, ReviewComparisonRequest};
use serde::Serialize;

pub const MAX_COMMIT_DIFF_RECORDS: usize = 64;
pub const MAX_COMMIT_DIFF_BYTES: usize = 16 * 1024 * 1024;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct CommitDiffKey {
    pub commit: String,
    pub path: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub enum CommitDiffAvailability {
    SubjectOnly,
    RootCommit,
    Expanded,
}

#[derive(Clone, Debug, Serialize)]
pub struct CommitDiffRecord {
    pub key: CommitDiffKey,
    pub parent: Option<String>,
    pub subject: String,
    pub availability: CommitDiffAvailability,
    pub diff: Vec<u8>,
}

#[derive(Clone, Copy, Debug)]
pub struct CommitDiffLimits {
    pub records: usize,
    pub bytes: usize,
}

impl Default for CommitDiffLimits {
    fn default() -> Self {
        Self {
            records: MAX_COMMIT_DIFF_RECORDS,
            bytes: MAX_COMMIT_DIFF_BYTES,
        }
    }
}

#[derive(Debug, Default)]
pub struct CommitDiffStore {
    records: BTreeMap<CommitDiffKey, CommitDiffRecord>,
    retained_bytes: usize,
    limits: CommitDiffLimits,
}

impl CommitDiffStore {
    pub fn new(limits: CommitDiffLimits) -> Result<Self> {
        ensure!(
            limits.records > 0 && limits.bytes > 0,
            "commit diff limits must be nonzero"
        );
        Ok(Self {
            records: BTreeMap::new(),
            retained_bytes: 0,
            limits,
        })
    }

    pub fn comparison(
        detail: &ReviewCommitDetail,
        repository: forge_github::model::GithubRepositoryId,
    ) -> Option<ReviewComparisonRequest> {
        detail
            .parent
            .as_ref()
            .map(|parent| ReviewComparisonRequest {
                repository,
                base: parent.clone(),
                head: detail.commit.clone(),
            })
    }

    pub fn retain_subject(&mut self, detail: &ReviewCommitDetail, path: String) -> Result<()> {
        self.retain(
            detail,
            path,
            Vec::new(),
            CommitDiffAvailability::SubjectOnly,
        )
    }

    pub fn retain_root(&mut self, detail: &ReviewCommitDetail, path: String) -> Result<()> {
        ensure!(
            detail.parent.is_none(),
            "non-root commit cannot use the root boundary"
        );
        self.retain(detail, path, Vec::new(), CommitDiffAvailability::RootCommit)
    }

    pub fn retain_expanded(
        &mut self,
        detail: &ReviewCommitDetail,
        path: String,
        diff: Vec<u8>,
    ) -> Result<()> {
        ensure!(
            detail.parent.is_some(),
            "root commit diff expansion is unavailable"
        );
        ensure!(!diff.is_empty(), "expanded commit diff is empty");
        self.retain(detail, path, diff, CommitDiffAvailability::Expanded)
    }

    pub fn retain_expanded_batch(
        &mut self,
        detail: &ReviewCommitDetail,
        diff: Vec<(String, Vec<u8>)>,
    ) -> Result<()> {
        ensure!(
            detail.parent.is_some(),
            "root commit diff expansion is unavailable"
        );
        ensure!(!diff.is_empty(), "expanded commit diff is empty");
        let mut candidate = self.records.clone();
        for (path, bytes) in diff {
            ensure!(valid_path(&path), "invalid commit diff path");
            ensure!(!bytes.is_empty(), "expanded commit diff is empty");
            let key = CommitDiffKey {
                commit: detail.commit.clone(),
                path,
            };
            candidate.insert(
                key.clone(),
                CommitDiffRecord {
                    key,
                    parent: detail.parent.clone(),
                    subject: detail.subject.clone(),
                    availability: CommitDiffAvailability::Expanded,
                    diff: bytes,
                },
            );
        }
        let retained_bytes = candidate.values().map(retained_bytes).sum::<usize>();
        ensure!(
            candidate.len() <= self.limits.records,
            "commit diff record limit reached"
        );
        ensure!(
            retained_bytes <= self.limits.bytes,
            "commit diff byte limit reached"
        );
        self.records = candidate;
        self.retained_bytes = retained_bytes;
        Ok(())
    }

    pub fn get(&self, commit: &str, path: &str) -> Option<&CommitDiffRecord> {
        self.records.get(&CommitDiffKey {
            commit: commit.into(),
            path: path.into(),
        })
    }

    pub fn retained_bytes(&self) -> usize {
        self.retained_bytes
    }

    fn retain(
        &mut self,
        detail: &ReviewCommitDetail,
        path: String,
        diff: Vec<u8>,
        availability: CommitDiffAvailability,
    ) -> Result<()> {
        ensure!(valid_path(&path), "invalid commit diff path");
        let key = CommitDiffKey {
            commit: detail.commit.clone(),
            path,
        };
        let record = CommitDiffRecord {
            key: key.clone(),
            parent: detail.parent.clone(),
            subject: detail.subject.clone(),
            availability,
            diff,
        };
        let bytes = retained_bytes(&record);
        let previous = self.records.get(&key).map(retained_bytes).unwrap_or(0);
        ensure!(
            self.records.contains_key(&key) || self.records.len() < self.limits.records,
            "commit diff record limit reached"
        );
        ensure!(
            self.retained_bytes - previous + bytes <= self.limits.bytes,
            "commit diff byte limit reached"
        );
        self.retained_bytes = self.retained_bytes - previous + bytes;
        self.records.insert(key, record);
        Ok(())
    }
}

fn valid_path(path: &str) -> bool {
    !path.is_empty()
        && path.len() <= 4096
        && !path.contains('\0')
        && !path.starts_with('/')
        && path.split('/').all(|part| !matches!(part, "" | "." | ".."))
}

fn retained_bytes(record: &CommitDiffRecord) -> usize {
    record.key.commit.len()
        + record.key.path.len()
        + record.parent.as_ref().map(String::len).unwrap_or(0)
        + record.subject.len()
        + record.diff.len()
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_github::model::GithubRepositoryId;

    fn detail(commit: char, parent: Option<char>) -> ReviewCommitDetail {
        ReviewCommitDetail {
            commit: commit.to_string().repeat(40),
            parent: parent.map(|value| value.to_string().repeat(40)),
            subject: format!("{commit} subject"),
            file: Vec::new(),
        }
    }

    #[test]
    fn comparison_uses_first_parent_to_commit_and_root_is_explicitly_unavailable() {
        let repository = GithubRepositoryId::new("github.com", "owner", "repo").unwrap();
        let commit = detail('b', Some('a'));
        let comparison = CommitDiffStore::comparison(&commit, repository.clone()).unwrap();
        assert_eq!(comparison.base, "a".repeat(40));
        assert_eq!(comparison.head, "b".repeat(40));

        let root = detail('a', None);
        assert!(CommitDiffStore::comparison(&root, repository).is_none());
        let mut store = CommitDiffStore::new(CommitDiffLimits::default()).unwrap();
        store.retain_root(&root, "src/lib.rs".into()).unwrap();
        assert_eq!(
            store.get(&root.commit, "src/lib.rs").unwrap().availability,
            CommitDiffAvailability::RootCommit
        );
        assert!(
            store
                .retain_expanded(&root, "src/lib.rs".into(), b"diff".to_vec())
                .is_err()
        );
    }

    #[test]
    fn full_commit_and_path_keys_do_not_alias_pr_path_state_or_other_commits() {
        let first = detail('b', Some('a'));
        let second = detail('c', Some('b'));
        let mut store = CommitDiffStore::new(CommitDiffLimits::default()).unwrap();
        store
            .retain_expanded(&first, "src/lib.rs".into(), b"first".to_vec())
            .unwrap();
        store
            .retain_expanded(&second, "src/lib.rs".into(), b"second".to_vec())
            .unwrap();
        assert_eq!(
            store.get(&first.commit, "src/lib.rs").unwrap().diff,
            b"first"
        );
        assert_eq!(
            store.get(&second.commit, "src/lib.rs").unwrap().diff,
            b"second"
        );
    }

    #[test]
    fn subject_target_survives_expansion_and_limits_reject_without_eviction() {
        let commit = detail('b', Some('a'));
        let mut store = CommitDiffStore::new(CommitDiffLimits {
            records: 1,
            bytes: 256,
        })
        .unwrap();
        store.retain_subject(&commit, "src/lib.rs".into()).unwrap();
        let subject = store
            .get(&commit.commit, "src/lib.rs")
            .unwrap()
            .subject
            .clone();
        store
            .retain_expanded(&commit, "src/lib.rs".into(), b"diff".to_vec())
            .unwrap();
        assert_eq!(
            store.get(&commit.commit, "src/lib.rs").unwrap().subject,
            subject
        );
        assert!(
            store
                .retain_subject(&commit, "src/other.rs".into())
                .is_err()
        );
        assert!(
            store
                .retain_expanded(&commit, "src/lib.rs".into(), vec![0; 256])
                .is_err()
        );
        assert_eq!(
            store.get(&commit.commit, "src/lib.rs").unwrap().diff,
            b"diff"
        );
    }

    #[test]
    fn batch_admission_is_atomic_when_commit_exceeds_remaining_records() {
        let commit = detail('b', Some('a'));
        let mut store = CommitDiffStore::new(CommitDiffLimits {
            records: 1,
            bytes: 1024,
        })
        .unwrap();
        assert!(
            store
                .retain_expanded_batch(
                    &commit,
                    vec![
                        ("src/one.rs".into(), b"one".to_vec()),
                        ("src/two.rs".into(), b"two".to_vec()),
                    ],
                )
                .is_err()
        );
        assert!(store.get(&commit.commit, "src/one.rs").is_none());
        assert_eq!(store.retained_bytes(), 0);
    }
}
