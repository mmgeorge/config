use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result, ensure};
use forge_buffer::identity::DocumentId;
use forge_github::review_api::{GithubReviewRemote, ReviewReadRequest, ReviewSection};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::ReviewService;

const MAX_SECTION_BYTES: usize = 8 * 1024 * 1024;
const MAX_DOCUMENT_SECTION_BYTES: usize = 16 * 1024 * 1024;
const MAX_SECTION_RECORDS: usize = 10_000;

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum ReviewSectionKind {
    Overview,
    Commits,
    Files,
    Reviews,
    Conversation,
    ReviewComments,
    Threads,
    Checks,
    RequestedReviewers,
}

#[derive(Debug, Serialize)]
pub struct ReviewSectionItem {
    pub identity: String,
    pub title: String,
    pub body: Option<String>,
    pub url: Option<String>,
    pub detail: Vec<(String, String)>,
    pub source: Option<ReviewFileSource>,
    pub comparison: Option<ReviewComparisonIdentity>,
    pub commit: Option<ReviewCommitPresentation>,
    pub check: Option<ReviewCheckPresentation>,
    pub review: Option<ReviewSummaryPresentation>,
    pub thread: Option<Arc<super::thread::ReviewThread>>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewCommitPresentation {
    pub sha: String,
    pub headline: String,
    pub committed_at: String,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewCheckPresentation {
    pub name: String,
    pub state: String,
    pub workflow: Option<String>,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewSummaryPresentation {
    pub author: String,
    pub state: String,
    pub submitted_at: String,
}

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewComparisonIdentity {
    pub base_commit: String,
    pub head_commit: String,
    pub head_repository: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct ReviewFileSource {
    pub path: String,
    pub status: String,
    pub previous_path: Option<String>,
    pub blob: String,
    pub patch: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct ReviewSectionState {
    pub item: Vec<Arc<ReviewSectionItem>>,
    pub next_cursor: Option<String>,
    pub complete: bool,
    pub loading: bool,
    pub diagnostic: Option<String>,
    #[serde(skip)]
    pub(crate) bytes: usize,
    #[serde(skip)]
    pub(super) charge: Option<Arc<SectionCharge>>,
}

#[derive(Debug)]
pub(super) struct SectionCharge {
    budget: Arc<std::sync::atomic::AtomicUsize>,
    bytes: usize,
}

impl SectionCharge {
    pub(super) fn reserve(
        budget: Arc<std::sync::atomic::AtomicUsize>,
        bytes: usize,
    ) -> Result<Arc<Self>> {
        use std::sync::atomic::Ordering;
        budget
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |used| {
                used.checked_add(bytes)
                    .filter(|next| *next <= 64 * 1024 * 1024)
            })
            .map_err(|_| anyhow::anyhow!("shared review section budget is full"))?;
        Ok(Arc::new(Self { budget, bytes }))
    }
}

impl Drop for SectionCharge {
    fn drop(&mut self) {
        self.budget
            .fetch_sub(self.bytes, std::sync::atomic::Ordering::AcqRel);
    }
}

impl ReviewSectionKind {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Overview => "Pull request",
            Self::Commits => "Recent Commits",
            Self::Files => "Files",
            Self::Reviews => "Reviews",
            Self::Conversation => "Conversation",
            Self::ReviewComments => "Review comments",
            Self::Threads => "Review threads",
            Self::Checks => "Checks",
            Self::RequestedReviewers => "Requested reviewers",
        }
    }

    fn remote(self) -> ReviewSection {
        match self {
            Self::Overview => ReviewSection::Overview,
            Self::Commits => ReviewSection::Commits,
            Self::Files => ReviewSection::Files,
            Self::Reviews => ReviewSection::Reviews,
            Self::Conversation => ReviewSection::Conversation,
            Self::ReviewComments => ReviewSection::ReviewComments,
            Self::Threads => ReviewSection::Threads,
            Self::Checks => ReviewSection::Checks,
            Self::RequestedReviewers => ReviewSection::RequestedReviewers,
        }
    }
}

impl ReviewSectionState {
    pub(super) fn partition_files(
        self,
        viewed_file: &std::collections::BTreeSet<String>,
    ) -> (Self, Self) {
        let Self {
            item,
            next_cursor,
            complete,
            loading,
            diagnostic,
            bytes,
            charge,
        } = self;
        let (viewed_item, unviewed_item): (Vec<_>, Vec<_>) = item.into_iter().partition(|item| {
            item.source
                .as_ref()
                .is_some_and(|source| viewed_file.contains(&source.path))
        });
        let viewed = Self {
            item: viewed_item,
            next_cursor: None,
            complete: true,
            loading: false,
            diagnostic: None,
            bytes: 0,
            charge: None,
        };
        let unviewed = Self {
            item: unviewed_item,
            next_cursor,
            complete,
            loading,
            diagnostic,
            bytes,
            charge,
        };
        (unviewed, viewed)
    }
}

impl ReviewService {
    pub async fn read_section(
        &self,
        id: &DocumentId,
        directory: PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        section: ReviewSectionKind,
        cursor: Option<String>,
    ) -> Result<ReviewSectionState> {
        let owner = self.owner(id)?;
        owner.validate_directory(&directory)?;
        let analysis = self
            .analysis
            .clone()
            .context("review analysis service is unavailable")?;
        let admission = self
            .job_admission
            .clone()
            .try_acquire_owned()
            .context("review section admission is full")?;
        let (request, parent_node) = {
            let mut document = owner.document.lock().expect("review document poisoned");
            document.section_revision = document
                .section_revision
                .checked_add(1)
                .context("review section revision exhausted")?;
            let target = document.target.clone();
            let state = document.section.entry(section).or_default();
            ensure!(!state.loading, "review section read is already running");
            ensure!(
                cursor.is_none() || cursor == state.next_cursor,
                "review section cursor is stale"
            );
            state.loading = true;
            state.diagnostic = None;
            (
                ReviewReadRequest {
                    repository: target.repository,
                    number: target.number,
                    view: section.remote(),
                    cursor: cursor.clone(),
                },
                target.node_id,
            )
        };
        let service = self.clone();
        let rollback = owner.clone();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        let spawned = self.spawn(async move {
            let _admission = admission;
            let number = request.number;
            let observed = service.github.review_page(directory, remote, request).await;
            let observed = async {
                let page = observed?;
                ensure!(
                    page.records.len() <= 100 && page.complete == page.next_cursor.is_none(),
                    "review section page has invalid completion"
                );
                ensure!(
                    page.next_cursor.is_none() || page.next_cursor != cursor,
                    "review section cursor did not advance"
                );
                let permit = analysis
                    .reserve(
                        forge_diff::workers::WorkPriority::Foreground,
                        forge_diff::workers::WorkBudget::new(MAX_SECTION_BYTES, None),
                    )
                    .map_err(|failure| {
                        anyhow::anyhow!("review section analysis admission failed: {failure:?}")
                    })?;
                let ticket = permit.ticket();
                let (sender, receiver) = tokio::sync::oneshot::channel();
                permit.submit(move |_| {
                    let result = (|| {
                        let bytes = serde_json::to_vec(&page.records)?.len();
                        ensure!(
                            bytes <= MAX_SECTION_BYTES,
                            "review section page exceeds its byte limit"
                        );
                        let item = page
                            .records
                            .into_iter()
                            .map(|record| {
                                normalize(section, record, number, &parent_node).map(Arc::new)
                            })
                            .collect::<Result<Vec<_>>>()?;
                        Ok::<_, anyhow::Error>((item, bytes, page.next_cursor, page.complete))
                    })();
                    let _ = sender.send(result);
                });
                let result = receiver
                    .await
                    .context("review section analysis ended without a result");
                ticket.completed().await;
                result?
            }
            .await;
            let result = (|| {
                let mut document = owner.document.lock().expect("review document poisoned");
                document.section_revision = document
                    .section_revision
                    .checked_add(1)
                    .context("review section revision exhausted")?;
                let outcome = (|| {
                    let (item, page_bytes, next_cursor, complete) = observed?;
                    let previous = document.section.get(&section).expect("admitted section");
                    let mut candidate = if cursor.is_some() {
                        previous.clone()
                    } else {
                        ReviewSectionState::default()
                    };
                    candidate.item.extend(item);
                    candidate.bytes = candidate
                        .bytes
                        .checked_add(page_bytes)
                        .context("section byte count overflow")?;
                    ensure!(
                        candidate.bytes <= MAX_SECTION_BYTES
                            && candidate.item.len() <= MAX_SECTION_RECORDS,
                        "review section collection limit reached"
                    );
                    let total: usize = document
                        .section
                        .iter()
                        .filter(|(kind, _)| **kind != section)
                        .map(|(_, state)| state.bytes)
                        .sum();
                    ensure!(
                        total + candidate.bytes <= MAX_DOCUMENT_SECTION_BYTES,
                        "review document section limit reached"
                    );
                    let mut identities = std::collections::HashSet::new();
                    ensure!(
                        candidate
                            .item
                            .iter()
                            .all(|item| identities.insert(item.identity.clone())),
                        "review section contains duplicate identities"
                    );
                    candidate.next_cursor = next_cursor;
                    candidate.complete = complete;
                    candidate.loading = false;
                    candidate.diagnostic = None;
                    candidate.charge = Some(SectionCharge::reserve(
                        service.section_bytes.clone(),
                        candidate.bytes,
                    )?);
                    Ok::<_, anyhow::Error>(candidate)
                })();
                match outcome {
                    Ok(candidate) => {
                        document.section.insert(section, candidate.clone());
                        Ok(candidate)
                    }
                    Err(failure) => {
                        let state = document
                            .section
                            .get_mut(&section)
                            .expect("admitted section");
                        state.loading = false;
                        state.diagnostic = Some(failure.to_string());
                        Err(failure)
                    }
                }
            })();
            let _ = sender.send(result);
        });
        if let Err(failure) = spawned {
            if let Some(state) = rollback
                .document
                .lock()
                .expect("review document poisoned")
                .section
                .get_mut(&section)
            {
                state.loading = false;
                state.diagnostic = Some(failure.to_string());
            }
            return Err(failure);
        }
        receiver
            .await
            .context("review section ended without a result")?
    }
}

fn required(record: &Value, key: &str) -> Result<String> {
    record
        .get(key)
        .and_then(Value::as_str)
        .filter(|value| !value.contains('\0'))
        .map(str::to_owned)
        .with_context(|| format!("review record omitted {key}"))
}

fn identity(record: &Value) -> Result<String> {
    match record.get("id") {
        Some(Value::String(value)) if !value.is_empty() && value.len() <= 256 => Ok(value.clone()),
        Some(Value::Number(value)) if value.as_u64().is_some_and(|value| value > 0) => {
            Ok(value.to_string())
        }
        _ => anyhow::bail!("review record omitted stable identity"),
    }
}

fn normalize(
    kind: ReviewSectionKind,
    record: Value,
    number: u64,
    node_id: &str,
) -> Result<ReviewSectionItem> {
    let mut item = ReviewSectionItem {
        identity: String::new(),
        title: String::new(),
        body: None,
        url: None,
        detail: Vec::new(),
        source: None,
        comparison: None,
        commit: None,
        check: None,
        review: None,
        thread: None,
    };
    match kind {
        ReviewSectionKind::Overview => {
            ensure!(
                record.get("number").and_then(Value::as_u64) == Some(number)
                    && record.get("node_id").and_then(Value::as_str) == Some(node_id),
                "PR section identity changed"
            );
            item.identity = node_id.into();
            required(&record, "title")?;
            item.title = format!("PR #{number}");
            if let (Some(base_commit), Some(head_commit), Some(head_repository)) = (
                record.pointer("/base/sha").and_then(Value::as_str),
                record.pointer("/head/sha").and_then(Value::as_str),
                record
                    .pointer("/head/repo/full_name")
                    .and_then(Value::as_str),
            ) {
                item.comparison = Some(ReviewComparisonIdentity {
                    base_commit: base_commit.into(),
                    head_commit: head_commit.into(),
                    head_repository: head_repository.into(),
                });
            }
            for (label, pointer) in [
                ("State", "/state"),
                ("Author", "/user/login"),
                ("Base", "/base/ref"),
                ("Head", "/head/ref"),
                ("Base commit", "/base/sha"),
                ("Head commit", "/head/sha"),
                ("Milestone", "/milestone/title"),
                ("Created", "/created_at"),
                ("Updated", "/updated_at"),
            ] {
                if let Some(value) = record.pointer(pointer).and_then(Value::as_str) {
                    item.detail.push((label.into(), value.into()));
                }
            }
            if record.get("draft").and_then(Value::as_bool) == Some(true) {
                item.detail.push(("Draft".into(), "Yes".into()));
            }
            for (key, field, label) in [
                ("labels", "name", "Label"),
                ("assignees", "login", "Assignee"),
            ] {
                for value in record
                    .get(key)
                    .and_then(Value::as_array)
                    .into_iter()
                    .flatten()
                {
                    item.detail.push((label.into(), required(value, field)?));
                }
            }
        }
        ReviewSectionKind::Commits => {
            let sha = required(&record, "sha")?;
            let message = record
                .pointer("/commit/message")
                .and_then(Value::as_str)
                .context("commit message is missing")?;
            ensure!(!message.contains('\0'), "commit message contains NUL");
            let headline = message.lines().next().unwrap_or("").trim_end().to_owned();
            let committed_at = record
                .pointer("/commit/committer/date")
                .and_then(Value::as_str)
                .or_else(|| {
                    record
                        .pointer("/commit/author/date")
                        .and_then(Value::as_str)
                })
                .context("commit date is missing")?
                .to_owned();
            item.identity = sha.clone();
            item.title = headline.clone();
            item.commit = Some(ReviewCommitPresentation {
                sha,
                headline,
                committed_at,
            });
        }
        ReviewSectionKind::Files => {
            let path = required(&record, "filename")?;
            ensure!(
                !path.is_empty() && !path.contains(['\r', '\n']),
                "invalid review file path"
            );
            item.identity = path.clone();
            item.title = path.clone();
            item.source = Some(ReviewFileSource {
                path,
                status: required(&record, "status")?,
                previous_path: record
                    .get("previous_filename")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                blob: required(&record, "sha")?,
                patch: record
                    .get("patch")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
            });
            for key in ["status", "additions", "deletions"] {
                if let Some(value) = record.get(key) {
                    item.detail.push((
                        key.into(),
                        value
                            .as_str()
                            .map(str::to_owned)
                            .unwrap_or_else(|| value.to_string()),
                    ));
                }
            }
        }
        ReviewSectionKind::RequestedReviewers => {
            item.identity = "requested-reviewers".into();
            item.title = kind.label().into();
            for (key, field) in [("users", "login"), ("teams", "slug")] {
                for value in record
                    .get(key)
                    .and_then(Value::as_array)
                    .context("reviewer collection is missing")?
                {
                    item.detail.push((key.into(), required(value, field)?));
                }
            }
        }
        ReviewSectionKind::Checks => {
            item.identity = identity(&record)?;
            item.title = record
                .get("name")
                .or_else(|| record.get("context"))
                .and_then(Value::as_str)
                .context("check name is missing")?
                .into();
            for key in ["status", "conclusion", "state"] {
                if let Some(value) = record.get(key).and_then(Value::as_str) {
                    item.detail.push((key.into(), value.into()));
                }
            }
            let state = record
                .get("conclusion")
                .and_then(Value::as_str)
                .filter(|value| !value.is_empty())
                .or_else(|| record.get("state").and_then(Value::as_str))
                .or_else(|| record.get("status").and_then(Value::as_str))
                .unwrap_or("")
                .to_owned();
            let workflow = record
                .get("workflow_name")
                .or_else(|| record.get("workflowName"))
                .or_else(|| record.pointer("/workflow/name"))
                .or_else(|| record.pointer("/checkSuite/workflowRun/workflow/name"))
                .and_then(Value::as_str)
                .map(str::trim)
                .filter(|value| !value.is_empty() && *value != item.title)
                .map(str::to_owned);
            item.check = Some(ReviewCheckPresentation {
                name: item.title.clone(),
                state,
                workflow,
            });
        }
        ReviewSectionKind::Threads => {
            item.thread = Some(Arc::new(super::thread::decode(&record)?));
            item.identity = identity(&record)?;
            item.title = required(&record, "path")?;
            for key in ["isResolved", "isOutdated", "line", "startLine"] {
                if let Some(value) = record.get(key) {
                    item.detail.push((key.into(), value.to_string()));
                }
            }
        }
        ReviewSectionKind::Reviews
        | ReviewSectionKind::Conversation
        | ReviewSectionKind::ReviewComments => {
            item.identity = identity(&record)?;
            item.title = record
                .pointer("/user/login")
                .and_then(Value::as_str)
                .context("comment author is missing")?
                .into();
            item.body = Some(
                record
                    .get("body")
                    .and_then(Value::as_str)
                    .unwrap_or("")
                    .into(),
            );
            if kind == ReviewSectionKind::Reviews {
                item.review = Some(ReviewSummaryPresentation {
                    author: item.title.clone(),
                    state: record
                        .get("state")
                        .and_then(Value::as_str)
                        .unwrap_or("")
                        .to_owned(),
                    submitted_at: record
                        .get("submitted_at")
                        .or_else(|| record.get("submittedAt"))
                        .or_else(|| record.get("updated_at"))
                        .or_else(|| record.get("updatedAt"))
                        .or_else(|| record.get("created_at"))
                        .or_else(|| record.get("createdAt"))
                        .and_then(Value::as_str)
                        .unwrap_or("")
                        .to_owned(),
                });
            }
            for key in [
                "state",
                "path",
                "line",
                "side",
                "commit_id",
                "in_reply_to_id",
                "created_at",
                "updated_at",
            ] {
                if let Some(value) = record.get(key) {
                    item.detail.push((
                        key.into(),
                        value
                            .as_str()
                            .map(str::to_owned)
                            .unwrap_or_else(|| value.to_string()),
                    ));
                }
            }
        }
    }
    item.url = record
        .get("html_url")
        .or_else(|| record.get("detailsUrl"))
        .or_else(|| record.get("targetUrl"))
        .and_then(Value::as_str)
        .map(str::to_owned);
    Ok(item)
}
