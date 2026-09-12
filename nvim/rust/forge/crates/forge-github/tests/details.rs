use forge_github::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Result;
use forge_github::issue_store::IssueStore;
use forge_github::lease::RepositoryLease;
use forge_github::model::{GithubRepositoryId, IssueDetail};
use forge_github::remote::{
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure, RemoteFailureKind,
};
use forge_github::service::{GithubService, IssueOperation};

#[derive(Default)]
struct DetailRemote {
    started: tokio::sync::Notify,
    release: tokio::sync::Notify,
    failure: Option<RemoteFailure>,
}

impl GithubRemote for DetailRemote {
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
        panic!("detail fetch must not read repository users")
    }

    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        panic!("detail fetch must not read issue pages")
    }

    fn read_issue_detail(
        &self,
        request: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            self.started.notify_one();
            self.release.notified().await;
            if let Some(failure) = &self.failure {
                return Err(failure.clone());
            }
            Ok(IssueDetail {
                kind: "issue".into(),
                node_id: "ISSUE_7".into(),
                repo: request.repository.repository_name(),
                number: request.number,
                title: "Owned detail".into(),
                body: "Remote body".into(),
                url: "https://github.com/owner/repo/issues/7".into(),
                state: "OPEN".into(),
                author: "alice".into(),
                created_at: "2026-09-01T00:00:00Z".into(),
                updated_at: "2026-09-07T00:00:00Z".into(),
                labels: Vec::new(),
                assignees: Vec::new(),
                milestone: String::new(),
                projects: Vec::new(),
                comments_count: 0,
                comments: Vec::new(),
                subscription: String::new(),
                is_draft: false,
            })
        })
    }
}

fn store(root: &std::path::Path) -> Result<IssueStore> {
    IssueStore::new(
        root.join("repo/issues/issues.redb"),
        "owner/repo",
        Duration::ZERO,
    )
}

fn request() -> IssueDetailRequest {
    IssueDetailRequest {
        repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        number: 7,
    }
}

fn fetch(
    service: GithubService,
    remote: Arc<DetailRemote>,
    store: IssueStore,
) -> tokio::task::JoinHandle<Result<forge_github::model::DetailRecord>> {
    tokio::spawn(async move { service.fetch_detail(remote, store, request()).await })
}

#[tokio::test]
async fn abandoned_caller_retains_detail_and_deletion_ownership_through_persistence() -> Result<()>
{
    let root = tempfile::tempdir()?;
    let store = store(root.path())?;
    let remote = Arc::new(DetailRemote::default());
    let service = GithubService::default();
    let caller = fetch(service.clone(), remote.clone(), store.clone());
    tokio::time::timeout(Duration::from_secs(1), remote.started.notified()).await?;
    assert!(!store.read_detail(7)?.found);
    assert!(RepositoryLease::deletion(&root.path().join("repo")).is_err());
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    assert!(
        service
            .execute(store.clone(), IssueOperation::DeleteCache {})
            .await
            .is_err()
    );
    remote.release.notify_one();
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            match RepositoryLease::deletion(&root.path().join("repo")) {
                Ok(lease) => {
                    drop(lease);
                    break Ok::<(), anyhow::Error>(());
                }
                Err(failure) if failure.to_string().contains("Busy") => {}
                Err(failure) => break Err(failure),
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await??;
    assert_eq!(store.read_detail(7)?.item.unwrap()["body"], "Remote body");
    let shutdown = service.shutdown(Duration::from_secs(1)).await;
    assert_eq!(shutdown.unfinished_jobs, 0);
    assert_eq!(shutdown.failed_jobs, 0);
    RepositoryLease::deletion(&root.path().join("repo"))?;
    Ok(())
}

#[tokio::test]
async fn close_cancels_remote_detail_without_creating_a_cached_success() -> Result<()> {
    let root = tempfile::tempdir()?;
    let store = store(root.path())?;
    let remote = Arc::new(DetailRemote::default());
    let service = GithubService::default();
    let caller = fetch(service.clone(), remote.clone(), store.clone());
    tokio::time::timeout(Duration::from_secs(1), remote.started.notified()).await?;
    let shutdown = service.shutdown(Duration::from_secs(1)).await;
    assert_eq!(shutdown.unfinished_jobs, 0);
    assert!(caller.await?.unwrap_err().to_string().contains("closed"));
    assert!(!store.read_detail(7)?.found);
    RepositoryLease::deletion(&root.path().join("repo"))?;
    Ok(())
}

#[tokio::test]
async fn detail_failures_preserve_remote_diagnostics_and_release_ownership() -> Result<()> {
    let root = tempfile::tempdir()?;
    let store = store(root.path())?;
    let remote = Arc::new(DetailRemote {
        failure: Some(RemoteFailure {
            kind: RemoteFailureKind::NotFound,
            message: "missing issue fixture".into(),
        }),
        ..Default::default()
    });
    remote.release.notify_one();
    let service = GithubService::default();
    let failure = service
        .fetch_detail(remote, store.clone(), request())
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("missing issue fixture"));
    assert!(!store.read_detail(7)?.found);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    RepositoryLease::deletion(&root.path().join("repo"))?;
    Ok(())
}

#[tokio::test]
async fn detail_admission_keeps_blocking_storage_capacity_available() -> Result<()> {
    let root = tempfile::tempdir()?;
    let store = store(root.path())?;
    let service = GithubService::default();
    let mut caller = Vec::new();
    for _ in 0..2 {
        let remote = Arc::new(DetailRemote::default());
        caller.push(fetch(service.clone(), remote.clone(), store.clone()));
        tokio::time::timeout(Duration::from_secs(1), remote.started.notified()).await?;
    }
    assert!(
        service
            .fetch_detail(Arc::new(DetailRemote::default()), store.clone(), request())
            .await
            .unwrap_err()
            .to_string()
            .contains("full")
    );
    service.execute(store, IssueOperation::State {}).await?;
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    for caller in caller {
        assert!(caller.await?.is_err());
    }
    Ok(())
}
