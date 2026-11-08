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
        self.records = candidate;
        self.evict();
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
        self.records.insert(key, record);
        self.evict();
        Ok(())
    }

    fn evict(&mut self) {
        self.retained_bytes = self.records.values().map(retained_bytes).sum();
        while self.records.len() > self.limits.records || self.retained_bytes > self.limits.bytes {
            let Some((_, record)) = self.records.pop_first() else {
                break;
            };
            self.retained_bytes -= retained_bytes(&record);
        }
    }
}

fn valid_path(path: &str) -> bool {
    !path.is_empty()
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
