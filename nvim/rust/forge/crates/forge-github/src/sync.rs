use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, bail, ensure};
use serde::{Deserialize, Serialize};

use crate::issue_store::IssueStore;
use crate::model::{GithubRepositoryId, PageInput, RepoSyncState, SnapshotState, SyncScope};
use crate::remote::{GithubRemote, IssuePage, IssuePageRequest, RemoteFailureKind};
use crate::service::{GithubService, IssueOperation};

const MAX_PAGES: usize = 10_000;
const MAX_CURSOR_BYTES: usize = 512;
const MAX_RATE_RETRIES: usize = 3;
const REMOTE_DEADLINE: Duration = Duration::from_secs(120);
const RATE_DELAY: Duration = Duration::from_secs(60);
const PAGE_DELAY: Duration = Duration::from_millis(150);

/// One repository refresh using the existing scope and snapshot-path contract.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SyncRequest {
    pub repository: GithubRepositoryId,
    pub scope: SyncScope,
    #[serde(default)]
    pub manual: bool,
    pub snapshot: PathBuf,
}

/// Completed refresh or a ten-minute freshness skip.
#[derive(Clone, Debug, Serialize)]
pub struct SyncOutcome {
    pub refreshed: bool,
    pub fetched: usize,
    pub pages: usize,
}

/// Current sync activity. Slow observers retain only the newest progress value.
#[derive(Clone, Debug, Default, Serialize)]
pub struct SyncProgress {
    pub phase: SyncPhase,
    pub fetched: usize,
    pub total: Option<u64>,
    pub pages: usize,
    pub retry_after_ms: Option<u64>,
}

#[derive(Clone, Copy, Debug, Default, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SyncPhase {
    #[default]
    Starting,
    Reading,
    Indexing,
    Publishing,
    RateLimited,
    Complete,
    Fresh,
}

struct SyncReporter {
    sender: Option<tokio::sync::watch::Sender<SyncProgress>>,
    current: SyncProgress,
}

impl SyncReporter {
    fn publish(&mut self, phase: SyncPhase, retry_after_ms: Option<u64>) {
        self.current.phase = phase;
        self.current.retry_after_ms = retry_after_ms;
        if let Some(sender) = &self.sender {
            sender.send_replace(self.current.clone());
        }
    }
}

pub(crate) async fn run(
    service: GithubService,
    remote: Arc<dyn GithubRemote>,
    store: IssueStore,
    request: SyncRequest,
    state: RepoSyncState,
    snapshot_exists: bool,
    sender: Option<tokio::sync::watch::Sender<SyncProgress>>,
) -> Result<SyncOutcome> {
    let mut progress = SyncReporter {
        sender,
        current: SyncProgress::default(),
    };
    let (incremental, previous_cursor, high_water, checked_at) = match request.scope {
        SyncScope::Open => (
            state.open_historical_complete,
            state.open_cursor,
            state.open_high_water,
            state.last_open_checked_at,
        ),
        SyncScope::All => (
            state.all_historical_complete,
            state.all_cursor,
            state.all_high_water,
            state.last_all_checked_at,
        ),
    };
    let now = unix_seconds()?;
    let fresh = checked_at.is_some_and(|checked| (0..600).contains(&now.saturating_sub(checked)));
    if !request.manual && incremental && snapshot_exists && fresh {
        progress.publish(SyncPhase::Fresh, None);
        return Ok(SyncOutcome {
            refreshed: false,
            fetched: 0,
            pages: 0,
        });
    }
    let mut cursor = if incremental { None } else { previous_cursor };
    let mut seen_cursor = HashSet::new();
    if let Some(cursor) = &cursor {
        validate_cursor(cursor)?;
        seen_cursor.insert(cursor.clone());
    }
    let mut outcome = SyncOutcome {
        refreshed: true,
        fetched: 0,
        pages: 0,
    };
    loop {
        ensure!(
            outcome.pages < MAX_PAGES,
            "issue sync exceeds its page limit"
        );
        let mut rate_retries = 0;
        let page = loop {
            progress.publish(SyncPhase::Reading, None);
            let page_request = IssuePageRequest {
                repository: request.repository.clone(),
                scope: request.scope,
                incremental,
                cursor: cursor.clone(),
            };
            let result = tokio::select! {
                biased;
                _ = service.closed() => bail!("GitHub sync admission is closed"),
                result = tokio::time::timeout(REMOTE_DEADLINE, async { remote.read_issues(page_request).await }) => result.context("GitHub issue page deadline exceeded")?,
            };
            match result {
                Ok(page) => break page,
                Err(failure)
                    if failure.kind == RemoteFailureKind::RateLimited
                        && rate_retries < MAX_RATE_RETRIES =>
                {
                    rate_retries += 1;
                    progress.publish(SyncPhase::RateLimited, Some(60_000));
                    pause(&service, RATE_DELAY).await?;
                }
                Err(failure) => return Err(failure.into()),
            }
        };
        validate_page(&request.repository, &page)?;
        let reached_high_water = incremental
            && high_water.as_ref().is_some_and(|high_water| {
                page.issues.iter().any(|issue| {
                    issue
                        .updated_at
                        .as_ref()
                        .is_some_and(|updated| updated <= high_water)
                })
            });
        let next_cursor = if reached_high_water {
            None
        } else {
            page.next_cursor.clone()
        };
        if let Some(next) = &next_cursor {
            ensure!(
                seen_cursor.insert(next.clone()),
                "GitHub issue pagination repeated a cursor"
            );
        }
        let newest = page
            .issues
            .first()
            .and_then(|issue| issue.updated_at.clone());
        let count = page.issues.len();
        progress.current.fetched += count;
        progress.current.total = page.total_count.or(progress.current.total);
        let payload = PageInput {
            issues: page.issues,
            cursor: next_cursor.clone(),
            has_next_page: next_cursor.is_some(),
            total_count: page.total_count,
            completed: next_cursor.is_none(),
            high_water: newest,
            checked_at: Some(unix_seconds()?),
        };
        progress.publish(SyncPhase::Indexing, None);
        service
            .execute(
                store.clone(),
                IssueOperation::UpsertPage {
                    scope: request.scope,
                    page: payload,
                },
            )
            .await?;
        progress.publish(SyncPhase::Publishing, None);
        service
            .execute(
                store.clone(),
                IssueOperation::PublishSnapshot {
                    state: SnapshotState::Open,
                    output: request.snapshot.clone(),
                },
            )
            .await?;
        outcome.fetched += count;
        outcome.pages += 1;
        progress.current.pages = outcome.pages;
        let Some(next_cursor) = next_cursor else {
            progress.publish(SyncPhase::Complete, None);
            return Ok(outcome);
        };
        cursor = Some(next_cursor);
        progress.publish(
            if page.rate_remaining == Some(0) {
                SyncPhase::RateLimited
            } else {
                SyncPhase::Reading
            },
            Some(if page.rate_remaining == Some(0) {
                60_000
            } else {
                150
            }),
        );
        pause(
            &service,
            if page.rate_remaining == Some(0) {
                RATE_DELAY
            } else {
                PAGE_DELAY
            },
        )
        .await?;
    }
}

async fn pause(service: &GithubService, duration: Duration) -> Result<()> {
    tokio::select! {
        biased;
        _ = service.closed() => bail!("GitHub sync admission is closed"),
        _ = tokio::time::sleep(duration) => Ok(()),
    }
}

fn unix_seconds() -> Result<i64> {
    i64::try_from(SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs())
        .context("system timestamp exceeds issue storage range")
}

fn validate_cursor(cursor: &str) -> Result<()> {
    ensure!(
        !cursor.is_empty() && cursor.len() <= MAX_CURSOR_BYTES,
        "GitHub issue cursor has invalid length"
    );
    Ok(())
}

fn validate_page(repository: &GithubRepositoryId, page: &IssuePage) -> Result<()> {
    ensure!(
        page.issues.len() <= 100,
        "GitHub issue page exceeds 100 records"
    );
    if let Some(cursor) = &page.next_cursor {
        validate_cursor(cursor)?;
        ensure!(
            !page.issues.is_empty(),
            "GitHub issue page advances without records"
        );
    }
    let repository_name = repository.repository_name();
    let mut previous_update: Option<&str> = None;
    let mut issue_number = HashSet::new();
    for issue in &page.issues {
        ensure!(
            issue.repo == repository_name && issue.number > 0 && issue_number.insert(issue.number),
            "GitHub issue page has invalid or duplicate identity"
        );
        ensure!(
            matches!(issue.state.as_str(), "OPEN" | "CLOSED"),
            "GitHub issue page has invalid state"
        );
        let expected_url = format!(
            "https://{}/{}/issues/{}",
            repository.hostname(),
            repository_name,
            issue.number
        );
        ensure!(
            issue.url.eq_ignore_ascii_case(&expected_url),
            "GitHub issue URL differs from its remote identity"
        );
        let updated = issue
            .updated_at
            .as_deref()
            .context("GitHub issue page omits update time")?;
        ensure!(
            !updated.is_empty() && updated.len() <= 64,
            "GitHub issue update time has invalid length"
        );
        ensure!(
            previous_update.is_none_or(|previous| previous >= updated),
            "GitHub issue page is not ordered by descending update time"
        );
        previous_update = Some(updated);
    }
    Ok(())
}
