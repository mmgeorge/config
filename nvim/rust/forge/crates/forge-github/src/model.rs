use serde::{Deserialize, Serialize};

/// Normalizes a DNS hostname and rejects URL, port, path, and option syntax.
pub fn normalize_hostname(hostname: &str) -> Result<String, String> {
    let hostname = hostname.trim().trim_end_matches('.').to_ascii_lowercase();
    if hostname.is_empty()
        || hostname.len() > 253
        || hostname.split('.').any(|label| {
            label.is_empty()
                || label.len() > 63
                || label.starts_with('-')
                || label.ends_with('-')
                || !label
                    .bytes()
                    .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
        })
    {
        return Err("invalid GitHub hostname".into());
    }
    Ok(hostname)
}

/// Validated remote identity independent of a local checkout or cache path.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "RepositoryIdentityInput")]
pub struct GithubRepositoryId {
    hostname: String,
    owner: String,
    name: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RepositoryIdentityInput {
    hostname: String,
    owner: String,
    name: String,
}

impl TryFrom<RepositoryIdentityInput> for GithubRepositoryId {
    type Error = String;

    fn try_from(input: RepositoryIdentityInput) -> Result<Self, Self::Error> {
        Self::new(&input.hostname, &input.owner, &input.name)
    }
}

impl GithubRepositoryId {
    /// Normalizes ASCII hostname, owner, and repository components and rejects URL/path syntax.
    pub fn new(hostname: &str, owner: &str, name: &str) -> Result<Self, String> {
        let hostname = normalize_hostname(hostname)?;
        let owner = owner.trim().to_ascii_lowercase();
        let name = name.trim().to_ascii_lowercase();
        if owner.is_empty()
            || owner.len() > 100
            || owner.starts_with('-')
            || owner.ends_with('-')
            || !owner
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')
        {
            return Err("invalid GitHub repository owner".into());
        }
        if name.is_empty()
            || name.len() > 100
            || matches!(name.as_str(), "." | "..")
            || !name
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
        {
            return Err("invalid GitHub repository name".into());
        }
        Ok(Self {
            hostname,
            owner,
            name,
        })
    }

    /// Returns the normalized remote host used for request routing.
    pub fn hostname(&self) -> &str {
        &self.hostname
    }

    /// Returns owner/name in the existing persisted issue-record format.
    pub fn repository_name(&self) -> String {
        format!("{}/{}", self.owner, self.name)
    }
}

/// Remote issue history selected for page-level synchronization.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SyncScope {
    /// Open issues only.
    Open,
    /// Open and closed issues.
    All,
}

/// Issue states included in a completion snapshot.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotState {
    /// Open issues only.
    Open,
    /// Open and closed issues.
    All,
}

/// Remote label metadata stored with an issue.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct LabelRecord {
    /// Remote label name.
    pub name: String,
    #[serde(default)]
    /// Optional remote label color.
    pub color: Option<String>,
    #[serde(default)]
    /// Optional remote label description.
    pub description: Option<String>,
}

/// Cached issue metadata and optional body in the existing redb format.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct IssueRecord {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Remote issue title at the last cache update.
    pub title: String,
    /// Remote issue state or snapshot state filter.
    pub state: String,
    /// Remote issue URL.
    pub url: String,
    #[serde(default)]
    /// Optional remote creation timestamp.
    pub created_at: Option<String>,
    #[serde(default)]
    /// Optional remote last-update timestamp used for completion ordering.
    pub updated_at: Option<String>,
    #[serde(default)]
    /// Optional remote issue body, excluded from completion snapshots.
    pub body: Option<String>,
    #[serde(default)]
    /// Remote labels attached to the cached issue.
    pub labels: Vec<LabelRecord>,
}

/// Completion metadata that excludes issue bodies.
#[derive(Clone, Debug, Serialize)]
pub struct SnapshotIssueRecord {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Remote issue title at the last cache update.
    pub title: String,
    /// Remote issue state or snapshot state filter.
    pub state: String,
    /// Remote issue URL.
    pub url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Optional remote creation timestamp.
    pub created_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Optional remote last-update timestamp used for completion ordering.
    pub updated_at: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Remote labels attached to the cached issue.
    pub labels: Vec<LabelRecord>,
}

impl From<IssueRecord> for SnapshotIssueRecord {
    fn from(issue: IssueRecord) -> Self {
        Self {
            repo: issue.repo,
            number: issue.number,
            title: issue.title,
            state: issue.state,
            url: issue.url,
            created_at: issue.created_at,
            updated_at: issue.updated_at,
            labels: issue.labels,
        }
    }
}

/// One remote page and its cursor and high-water update.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PageInput {
    #[serde(default)]
    /// Issue records belonging to this page or snapshot.
    pub issues: Vec<IssueRecord>,
    #[serde(default)]
    /// Remote cursor for the next history page.
    pub cursor: Option<String>,
    #[serde(default)]
    /// Whether the remote page has a successor.
    pub has_next_page: bool,
    #[serde(default)]
    /// Optional remote count for the selected history scope.
    pub total_count: Option<u64>,
    #[serde(default)]
    /// Whether this page completes the selected history.
    pub completed: bool,
    #[serde(default)]
    /// Optional newest remote update timestamp observed in this page.
    pub high_water: Option<String>,
    #[serde(default)]
    /// Optional timestamp of the completed remote check.
    pub checked_at: Option<i64>,
}

/// Persisted open and all synchronization progress for one repository.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct RepoSyncState {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Revision of committed issue records and sync state. Detail-only writes do not advance it.
    #[serde(default)]
    pub revision: u64,
    #[serde(default)]
    /// Persisted cursor for open-issue history.
    pub open_cursor: Option<String>,
    #[serde(default)]
    /// Persisted cursor for all-issue history.
    pub all_cursor: Option<String>,
    #[serde(default)]
    /// Whether initial open-issue history completed.
    pub open_historical_complete: bool,
    #[serde(default)]
    /// Whether initial all-issue history completed.
    pub all_historical_complete: bool,
    #[serde(default)]
    /// Newest remote update timestamp observed in open-issue sync.
    pub open_high_water: Option<String>,
    #[serde(default)]
    /// Newest remote update timestamp observed in all-issue sync.
    pub all_high_water: Option<String>,
    #[serde(default)]
    /// Last remote open-issue count.
    pub open_total_count: Option<u64>,
    #[serde(default)]
    /// Last remote all-issue count.
    pub all_total_count: Option<u64>,
    #[serde(default)]
    /// Timestamp of the last open-issue check.
    pub last_open_checked_at: Option<i64>,
    #[serde(default)]
    /// Timestamp of the last all-issue check.
    pub last_all_checked_at: Option<i64>,
    #[serde(default)]
    /// Number of issue records in this state or snapshot.
    pub issue_count: u64,
}

/// Committed page count and resulting repository synchronization state.
#[derive(Debug, Serialize)]
pub struct UpsertOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Number of issue records committed from the input page.
    pub upserted: usize,
    /// Remote issue state or snapshot state filter.
    pub state: RepoSyncState,
}

/// Ordered completion records for one repository and state filter.
#[derive(Debug, Serialize)]
pub struct SnapshotOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Issue-state revision read from the same transaction as the completion records.
    pub revision: u64,
    /// Remote issue state or snapshot state filter.
    pub state: String,
    /// Number of issue records in this state or snapshot.
    pub issue_count: usize,
    /// Issue records belonging to this page or snapshot.
    pub issues: Vec<SnapshotIssueRecord>,
}

/// State and publication outcome observed while one database handle excludes competing writers.
#[derive(Debug, Serialize)]
pub struct SnapshotRecovery {
    pub state: RepoSyncState,
    pub ready: bool,
    pub republished: bool,
}

/// Normalized issue content shared by remote reads and the detail cache.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct IssueDetail {
    pub kind: String,
    pub node_id: String,
    pub repo: String,
    pub number: u64,
    pub title: String,
    pub body: String,
    pub url: String,
    pub state: String,
    pub author: String,
    pub created_at: String,
    pub updated_at: String,
    pub labels: Vec<String>,
    pub assignees: Vec<String>,
    pub milestone: String,
    pub projects: Vec<String>,
    pub comments_count: usize,
    pub comments: Vec<ConversationComment>,
    pub subscription: String,
    pub is_draft: bool,
}

/// Remote issue conversation content with a source URL for navigation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ConversationComment {
    pub body: String,
    pub author: String,
    pub created_at: String,
    pub updated_at: String,
    pub url: String,
}

/// Cached remote issue detail and its fetch timestamp.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DetailRecord {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Timestamp of the remote detail fetch.
    pub fetched_at: i64,
    /// Remote detail payload retained without completion truncation.
    pub item: serde_json::Value,
}

/// Lookup result that distinguishes absent details from cached details.
#[derive(Debug, Serialize)]
pub struct DetailOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Whether the requested detail exists in the cache.
    pub found: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Timestamp of the remote detail fetch.
    pub fetched_at: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Remote detail payload retained without completion truncation.
    pub item: Option<serde_json::Value>,
}

/// Detail lookup results in the caller-provided issue order.
#[derive(Debug, Serialize)]
pub struct DetailsOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Detail results in request order, including misses.
    pub details: Vec<DetailOutput>,
}
