use forge_github::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};
use std::collections::VecDeque;
use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use anyhow::Result;
use forge_github::issue_store::IssueStore;
use forge_github::lease::RepositoryLease;
use forge_github::model::{GithubRepositoryId, IssueDetail, IssueRecord, SnapshotState, SyncScope};
use forge_github::remote::{
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure, RemoteFailureKind,
};
use forge_github::service::{GithubService, IssueOperation};
use forge_github::sync::SyncRequest;

#[derive(Default)]
struct RemoteFixture {
    page: Mutex<VecDeque<Result<IssuePage, RemoteFailure>>>,
    request: Mutex<Vec<IssuePageRequest>>,
    snapshot: Option<PathBuf>,
    observed_count: Mutex<Vec<usize>>,
}

impl GithubRemote for RemoteFixture {
    fn read_pull_request(
        &self,
        _: PullRequestTarget,
        _: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>> {
        panic!("unexpected PR state read")
    }
    fn mutate_pull_request(
        &self,
        _: PullRequestTarget,
        _: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        panic!("unexpected PR state mutation")
    }

    fn read_repository_users(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<forge_github::metadata::RepositoryUsers, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        panic!("issue sync must not read repository users")
    }

    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        panic!("issue sync must not fetch details")
    }

    fn read_issues(
        &self,
        request: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        self.request.lock().unwrap().push(request);
        if let Some(path) = &self.snapshot {
            let count = std::fs::read(path)
                .ok()
                .map(|bytes| {
                    serde_json::from_slice::<serde_json::Value>(&bytes).unwrap()["issue_count"]
                        .as_u64()
                        .unwrap() as usize
                })
                .unwrap_or(0);
            self.observed_count.lock().unwrap().push(count);
        }
        let result = self
            .page
            .lock()
            .unwrap()
            .pop_front()
            .expect("unexpected remote request");
        Box::pin(async move { result })
    }
}

struct BlockedRemote {
    started: tokio::sync::Notify,
    release: tokio::sync::Notify,
}

impl GithubRemote for BlockedRemote {
    fn read_pull_request(
        &self,
        _: PullRequestTarget,
        _: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>> {
        panic!("unexpected PR state read")
    }
    fn mutate_pull_request(
        &self,
        _: PullRequestTarget,
        _: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        panic!("unexpected PR state mutation")
    }

    fn read_repository_users(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<forge_github::metadata::RepositoryUsers, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        panic!("issue sync must not read repository users")
    }

    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        panic!("issue sync must not fetch details")
    }

    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            self.started.notify_one();
            self.release.notified().await;
            Ok(page(1, "2026-09-07T00:00:00Z", None))
        })
    }
}

fn request(root: &std::path::Path) -> Result<(IssueStore, SyncRequest)> {
    let directory = root.join("repo/issues");
    Ok((
        IssueStore::new(directory.join("issues.redb"), "owner/repo", Duration::ZERO)?,
        SyncRequest {
            repository: GithubRepositoryId::new("github.com", "Owner", "Repo").unwrap(),
            scope: SyncScope::Open,
            manual: true,
            snapshot: directory.join("open-snapshot.json"),
        },
    ))
}

fn page(number: u64, updated: &str, cursor: Option<&str>) -> IssuePage {
    IssuePage {
        issues: vec![IssueRecord {
            repo: "owner/repo".into(),
            number,
            title: format!("Issue {number}"),
            state: "OPEN".into(),
            url: format!("https://github.com/owner/repo/issues/{number}"),
            created_at: None,
            updated_at: Some(updated.into()),
            body: Some("Excluded from completion".into()),
            labels: vec![],
        }],
        next_cursor: cursor.map(str::to_owned),
        total_count: Some(2),
        rate_remaining: Some(100),
    }
}

#[tokio::test]
async fn history_publishes_each_page_and_incremental_refresh_stops_at_high_water() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (store, mut request) = request(root.path())?;
    let remote = Arc::new(RemoteFixture {
        page: Mutex::new(VecDeque::from([
            Ok(page(2, "2026-09-07T02:00:00Z", Some("second"))),
            Ok(page(1, "2026-09-07T01:00:00Z", None)),
        ])),
        snapshot: Some(request.snapshot.clone()),
        ..Default::default()
    });
    let service = GithubService::default();
    let outcome = service
        .sync(remote.clone(), store.clone(), request.clone(), None)
        .await?;
    assert_eq!((outcome.pages, outcome.fetched), (2, 2));
    assert_eq!(*remote.observed_count.lock().unwrap(), vec![0, 1]);
    {
        let history = remote.request.lock().unwrap();
        assert!(!history[0].incremental && history[0].cursor.is_none());
        assert_eq!(history[1].cursor.as_deref(), Some("second"));
    }
    assert_eq!(
        store.read_state()?.open_high_water.as_deref(),
        Some("2026-09-07T02:00:00Z")
    );
    let snapshot = std::fs::read_to_string(&request.snapshot)?;
    assert!(!snapshot.contains("Excluded from completion"));
    let mut closed = page(2, "2026-09-07T03:00:00Z", None);
    closed.issues[0].state = "CLOSED".into();
    closed
        .issues
        .push(page(1, "2026-09-07T01:00:00Z", None).issues.remove(0));
    closed.next_cursor = Some("must-not-fetch".into());
    remote.page.lock().unwrap().push_back(Ok(closed));
    assert_eq!(
        service
            .sync(remote.clone(), store.clone(), request.clone(), None)
            .await?
            .pages,
        1
    );
    assert!(remote.request.lock().unwrap()[2].incremental);
    assert!(remote.request.lock().unwrap()[2].cursor.is_none());
    assert_eq!(store.snapshot(SnapshotState::Open)?.issue_count, 1);
    request.manual = false;
    assert!(
        !service
            .sync(remote.clone(), store.clone(), request.clone(), None)
            .await?
            .refreshed
    );
    assert_eq!(remote.request.lock().unwrap().len(), 3);
    std::fs::remove_file(&request.snapshot)?;
    assert!(
        !service
            .sync(remote.clone(), store.clone(), request.clone(), None)
            .await?
            .refreshed
    );
    assert!(request.snapshot.exists());
    assert_eq!(remote.request.lock().unwrap().len(), 3);
    let published: serde_json::Value = serde_json::from_slice(&std::fs::read(&request.snapshot)?)?;
    assert_eq!(
        published["revision"].as_u64(),
        Some(store.read_state()?.revision)
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test]
async fn failed_history_resumes_committed_cursor_and_repeated_cursor_does_not_commit() -> Result<()>
{
    let root = tempfile::tempdir()?;
    let (store, request) = request(root.path())?;
    let remote = Arc::new(RemoteFixture::default());
    remote.page.lock().unwrap().extend([
        Ok(page(2, "2026-09-07T02:00:00Z", Some("resume"))),
        Err(RemoteFailure {
            kind: RemoteFailureKind::Transport,
            message: "offline".into(),
        }),
    ]);
    let service = GithubService::default();
    assert!(
        service
            .sync(remote.clone(), store.clone(), request.clone(), None)
            .await
            .is_err()
    );
    assert_eq!(store.read_state()?.open_cursor.as_deref(), Some("resume"));
    let snapshot = std::fs::read(&request.snapshot)?;
    remote
        .page
        .lock()
        .unwrap()
        .push_back(Ok(page(1, "2026-09-07T01:00:00Z", Some("resume"))));
    assert!(
        service
            .sync(remote.clone(), store.clone(), request.clone(), None)
            .await
            .unwrap_err()
            .to_string()
            .contains("repeated")
    );
    assert_eq!(std::fs::read(&request.snapshot)?, snapshot);
    assert_eq!(store.read_state()?.issue_count, 1);
    remote
        .page
        .lock()
        .unwrap()
        .push_back(Ok(page(1, "2026-09-07T01:00:00Z", None)));
    service
        .sync(remote.clone(), store.clone(), request, None)
        .await?;
    assert_eq!(
        remote
            .request
            .lock()
            .unwrap()
            .last()
            .unwrap()
            .cursor
            .as_deref(),
        Some("resume")
    );
    assert!(store.read_state()?.open_historical_complete);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test]
async fn remote_wait_and_abandoned_caller_retain_deletion_and_sync_exclusion() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (store, request) = request(root.path())?;
    let remote = Arc::new(BlockedRemote {
        started: Default::default(),
        release: Default::default(),
    });
    let service = GithubService::default();
    let caller = {
        let service = service.clone();
        let store = store.clone();
        let request = request.clone();
        let remote = remote.clone();
        tokio::spawn(async move { service.sync(remote, store, request, None).await })
    };
    tokio::time::timeout(Duration::from_secs(2), remote.started.notified()).await?;
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    let directory = root.path().join("repo");
    assert!(RepositoryLease::deletion(&directory).is_err());
    assert!(RepositoryLease::sync(&directory).is_err());
    assert!(
        service
            .execute(store.clone(), IssueOperation::DeleteCache {})
            .await
            .is_err()
    );
    assert!(
        service
            .execute(store.clone(), IssueOperation::State {})
            .await
            .is_ok()
    );
    let independent = GithubService::default();
    assert!(
        independent
            .execute(store, IssueOperation::DeleteCache {})
            .await
            .is_err()
    );
    service.close();
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    assert!(RepositoryLease::deletion(&directory).is_ok());
    assert_eq!(
        independent
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test(start_paused = true)]
async fn rate_limit_retries_are_bounded_and_preserve_the_remote_diagnostic() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (store, request) = request(root.path())?;
    let remote = Arc::new(RemoteFixture::default());
    for _ in 0..4 {
        remote.page.lock().unwrap().push_back(Err(RemoteFailure {
            kind: RemoteFailureKind::RateLimited,
            message: "API limit for this host".into(),
        }));
    }
    let service = GithubService::default();
    let started = tokio::time::Instant::now();
    let failure = service
        .sync(remote.clone(), store.clone(), request, None)
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("API limit for this host"));
    assert_eq!(remote.request.lock().unwrap().len(), 4);
    assert!(started.elapsed() >= Duration::from_secs(180));
    assert_eq!(store.read_state()?.issue_count, 0);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[test]
fn remote_identity_normalizes_components_and_rejects_path_or_url_injection() {
    let first = GithubRepositoryId::new("GITHUB.COM.", "Owner", "Repo").unwrap();
    assert_eq!(first.repository_name(), "owner/repo");
    assert_eq!(first.hostname(), "github.com");
    assert_ne!(
        first,
        GithubRepositoryId::new("enterprise.example", "owner", "repo").unwrap()
    );
    for (host, owner, name) in [
        ("https://github.com", "owner", "repo"),
        ("github.com", "../owner", "repo"),
        ("github.com", "owner", ".."),
        ("github.com", "owner", "repo/child"),
    ] {
        assert!(GithubRepositoryId::new(host, owner, name).is_err());
    }
    assert!(
        serde_json::from_str::<GithubRepositoryId>(
            r#"{"hostname":"github.com","owner":"owner","name":"../repo"}"#
        )
        .is_err()
    );
}

#[tokio::test]
async fn all_scope_preserves_closed_records_and_rejects_foreign_records_before_commit() -> Result<()>
{
    let root = tempfile::tempdir()?;
    let (store, mut request) = request(root.path())?;
    request.scope = SyncScope::All;
    let remote = Arc::new(RemoteFixture::default());
    let mut closed = page(1, "2026-09-07T01:00:00Z", None);
    closed.issues[0].state = "CLOSED".into();
    remote.page.lock().unwrap().push_back(Ok(closed));
    let service = GithubService::default();
    service
        .sync(remote.clone(), store.clone(), request.clone(), None)
        .await?;
    assert!(matches!(
        remote.request.lock().unwrap()[0].scope,
        SyncScope::All
    ));
    assert!(store.read_state()?.all_historical_complete);
    assert!(!store.read_state()?.open_historical_complete);
    assert_eq!(store.snapshot(SnapshotState::All)?.issue_count, 1);
    assert_eq!(store.snapshot(SnapshotState::Open)?.issue_count, 0);
    let before = std::fs::read(&request.snapshot)?;
    let mut foreign = page(2, "2026-09-07T02:00:00Z", None);
    foreign.issues[0].url = "https://enterprise.example/owner/repo/issues/2".into();
    remote.page.lock().unwrap().push_back(Ok(foreign));
    assert!(
        service
            .sync(remote, store.clone(), request.clone(), None)
            .await
            .unwrap_err()
            .to_string()
            .contains("remote identity")
    );
    assert_eq!(store.read_state()?.issue_count, 1);
    assert_eq!(std::fs::read(request.snapshot)?, before);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test]
async fn two_sync_jobs_leave_storage_capacity_and_reject_a_third_sync() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (store, request) = request(root.path())?;
    let service = GithubService::default();
    let remote = Arc::new(BlockedRemote {
        started: Default::default(),
        release: Default::default(),
    });
    let mut caller = Vec::new();
    for name in ["first", "second"] {
        let store = IssueStore::new(
            root.path().join(name).join("issues/issues.redb"),
            &format!("owner/{name}"),
            Duration::ZERO,
        )?;
        let mut request = request.clone();
        request.repository = GithubRepositoryId::new("github.com", "owner", name).unwrap();
        request.snapshot = root.path().join(name).join("issues/open-snapshot.json");
        let service = service.clone();
        let task_remote = remote.clone();
        caller.push(tokio::spawn(async move {
            service.sync(task_remote, store, request, None).await
        }));
        tokio::time::timeout(Duration::from_secs(2), remote.started.notified()).await?;
    }
    assert!(
        service
            .sync(remote, store.clone(), request, None)
            .await
            .unwrap_err()
            .to_string()
            .contains("admission is full")
    );
    assert!(
        service
            .execute(store, IssueOperation::State {})
            .await
            .is_ok()
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    for caller in caller {
        assert!(caller.await?.is_err());
    }
    Ok(())
}

#[tokio::test(start_paused = true)]
async fn close_interrupts_rate_delay_without_issuing_another_remote_read() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (store, request) = request(root.path())?;
    let remote = Arc::new(RemoteFixture::default());
    remote.page.lock().unwrap().push_back(Err(RemoteFailure {
        kind: RemoteFailureKind::RateLimited,
        message: "retry later".into(),
    }));
    let service = GithubService::default();
    let caller = {
        let service = service.clone();
        let remote = remote.clone();
        tokio::spawn(async move { service.sync(remote, store, request, None).await })
    };
    while remote.request.lock().unwrap().is_empty() {
        tokio::task::yield_now().await;
    }
    service.close();
    assert!(caller.await?.unwrap_err().to_string().contains("closed"));
    assert_eq!(remote.request.lock().unwrap().len(), 1);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test(start_paused = true)]
async fn remote_deadline_releases_sync_ownership_without_publishing() -> Result<()> {
    let root = tempfile::tempdir()?;
    let (store, request) = request(root.path())?;
    let snapshot = request.snapshot.clone();
    let remote = Arc::new(BlockedRemote {
        started: Default::default(),
        release: Default::default(),
    });
    let service = GithubService::default();
    let failure = service
        .sync(remote, store, request, None)
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("deadline"));
    assert!(!snapshot.exists());
    assert_eq!(
        service
            .shutdown(Duration::from_secs(2))
            .await
            .unfinished_jobs,
        0
    );
    assert!(RepositoryLease::deletion(&root.path().join("repo")).is_ok());
    Ok(())
}
