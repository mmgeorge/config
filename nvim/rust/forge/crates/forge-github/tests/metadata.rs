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
use forge_github::metadata::{MetadataRequest, MetadataStore, RepositoryUser, RepositoryUsers};
use forge_github::model::{GithubRepositoryId, IssueDetail};
use forge_github::remote::{
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure,
};
use forge_github::service::{GithubService, IssueOperation};

#[derive(Default)]
struct MetadataRemote {
    started: tokio::sync::Notify,
    release: tokio::sync::Notify,
    invalid: bool,
}

impl GithubRemote for MetadataRemote {
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
    ) -> Pin<Box<dyn Future<Output = Result<RepositoryUsers, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            self.started.notify_one();
            self.release.notified().await;
            Ok(RepositoryUsers {
                contributors: vec![RepositoryUser {
                    login: if self.invalid { "bad login" } else { "alice" }.into(),
                    name: Some("Alice".into()),
                }],
                failure: Vec::new(),
            })
        })
    }

    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        panic!("metadata must not fetch issue pages")
    }

    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        panic!("metadata must not fetch issue details")
    }
}

fn request() -> MetadataRequest {
    MetadataRequest {
        repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        ttl_seconds: 600,
    }
}

#[tokio::test]
async fn metadata_retains_refresh_and_deletion_ownership_after_caller_drop() -> Result<()> {
    let root = tempfile::tempdir()?;
    let directory = root.path().join("github.com/repos/owner/repo");
    let service = GithubService::default();
    let remote = Arc::new(MetadataRemote::default());
    let caller = tokio::spawn({
        let service = service.clone();
        let remote = remote.clone();
        let directory = directory.clone();
        async move { service.metadata(remote, directory, request()).await }
    });
    tokio::time::timeout(Duration::from_secs(1), remote.started.notified()).await?;
    assert!(RepositoryLease::deletion(&directory).is_err());
    assert!(RepositoryLease::metadata(&directory).is_err());
    assert!(RepositoryLease::sync(&directory).is_ok());
    let issue_store = IssueStore::new(
        directory.join("issues/issues.redb"),
        "owner/repo",
        Duration::ZERO,
    )?;
    assert!(
        service
            .execute(issue_store.clone(), IssueOperation::DeleteCache {})
            .await
            .is_err()
    );
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    remote.release.notify_one();
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if let Ok(lease) = RepositoryLease::deletion(&directory) {
                drop(lease);
                break;
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await?;
    let store = MetadataStore::new(directory.clone(), request().repository)?;
    assert_eq!(store.read()?.unwrap().contributors[0].login, "alice");
    let cached = tokio::time::timeout(
        Duration::from_secs(1),
        service.metadata(remote, directory, request()),
    )
    .await??;
    assert_eq!(cached.contributors[0].name.as_deref(), Some("Alice"));
    service
        .execute(issue_store, IssueOperation::DeleteCache {})
        .await?;
    assert!(store.read()?.is_none());
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test]
async fn shutdown_cancels_metadata_without_publishing_and_releases_ownership() -> Result<()> {
    let root = tempfile::tempdir()?;
    let directory = root.path().join("github.com/repos/owner/repo");
    let service = GithubService::default();
    let remote = Arc::new(MetadataRemote::default());
    let caller = tokio::spawn({
        let service = service.clone();
        let remote = remote.clone();
        let directory = directory.clone();
        async move { service.metadata(remote, directory, request()).await }
    });
    tokio::time::timeout(Duration::from_secs(1), remote.started.notified()).await?;
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    assert!(caller.await?.is_err());
    assert!(!directory.join("metadata.json").exists());
    assert!(RepositoryLease::deletion(&directory).is_ok());
    Ok(())
}

#[tokio::test]
async fn invalid_refresh_preserves_the_previous_metadata_and_identity_is_enforced() -> Result<()> {
    let root = tempfile::tempdir()?;
    let directory = root.path().join("github.com/repos/owner/repo");
    std::fs::create_dir_all(&directory)?;
    let previous = r#"{"repo":"owner/repo","fetched_at":1,"contributors":[{"login":"retained"}]}"#;
    std::fs::write(directory.join("metadata.json"), previous)?;
    let service = GithubService::default();
    let remote = Arc::new(MetadataRemote {
        invalid: true,
        ..Default::default()
    });
    remote.release.notify_one();
    assert!(
        service
            .metadata(remote, directory.clone(), request())
            .await
            .is_err()
    );
    assert_eq!(
        std::fs::read_to_string(directory.join("metadata.json"))?,
        previous
    );
    assert!(
        MetadataStore::new(
            root.path().join("other.example/repos/owner/repo"),
            request().repository
        )
        .is_err()
    );
    assert!(
        MetadataStore::new(
            root.path().join("github.com/repos/owner/other"),
            request().repository
        )
        .is_err()
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}

#[tokio::test]
async fn legacy_metadata_remains_fresh_without_rewriting_its_path_or_contents() -> Result<()> {
    let root = tempfile::tempdir()?;
    let directory = root.path().join("github.com/repos/Owner/Repo");
    std::fs::create_dir_all(&directory)?;
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)?
        .as_secs();
    let previous = format!(
        r#"{{"repo":"Owner/Repo","fetched_at":{now},"contributors":[{{"login":"legacy"}}]}}"#
    );
    std::fs::write(directory.join("metadata.json"), &previous)?;
    let service = GithubService::default();
    let metadata = tokio::time::timeout(
        Duration::from_secs(1),
        service.metadata(
            Arc::new(MetadataRemote::default()),
            directory.clone(),
            request(),
        ),
    )
    .await??;
    assert_eq!(metadata.contributors[0].login, "legacy");
    assert_eq!(
        std::fs::read_to_string(directory.join("metadata.json"))?,
        previous
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    Ok(())
}
