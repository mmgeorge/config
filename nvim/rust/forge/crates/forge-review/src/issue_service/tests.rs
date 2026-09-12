use super::*;
use forge_buffer::identity::{EditSequence, RegionId, RegionRevision};
use forge_github::{
    metadata::RepositoryUsers, model::*, pull_request::*, remote::*,
    review_mutation::ReviewMutationOutcome,
};
use std::{
    pin::Pin,
    sync::atomic::{AtomicUsize, Ordering},
    time::Duration,
};
struct Remote {
    started: tokio::sync::Notify,
    release: tokio::sync::Notify,
    writes: AtomicUsize,
    outcome_unknown: bool,
}
impl GithubRemote for Remote {
    fn read_actor(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RemoteActor, RemoteFailure>> + Send + '_>> {
        Box::pin(async {
            if false {
                Err(RemoteFailure {
                    kind: RemoteFailureKind::Unauthorized,
                    message: "captured account is unavailable".into(),
                })
            } else {
                Ok(RemoteActor {
                    login: "actor".into(),
                    node_id: "ACTOR".into(),
                })
            }
        })
    }
    fn read_pull_request(
        &self,
        _: PullRequestTarget,
        _: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>> {
        unimplemented!()
    }
    fn mutate_pull_request(
        &self,
        _: PullRequestTarget,
        _: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        unimplemented!()
    }
    fn read_repository_users(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RepositoryUsers, RemoteFailure>> + Send + '_>> {
        unimplemented!()
    }
    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        unimplemented!()
    }
    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        Box::pin(async { Ok(detail()) })
    }
}
fn detail() -> IssueDetail {
    IssueDetail {
        kind: "issue".into(),
        node_id: "ISSUE_7".into(),
        repo: "owner/repo".into(),
        number: 7,
        title: "Original title".into(),
        body: "exact\r\nbody\n".into(),
        url: "https://github.com/owner/repo/issues/7".into(),
        state: "OPEN".into(),
        author: "author".into(),
        created_at: "2026-09-07T00:00:00Z".into(),
        updated_at: "2026-09-07T00:00:00Z".into(),
        labels: vec!["bug".into()],
        assignees: vec!["keep".into(), "remove".into()],
        milestone: String::new(),
        projects: Vec::new(),
        comments_count: 0,
        comments: Vec::new(),
        subscription: "subscribed".into(),
        is_draft: false,
    }
}
impl GithubReviewWriteRemote for Remote {
    fn read_review_write_result(
        &self,
        _: ReviewMutationRequest,
        _: u64,
    ) -> Pin<Box<dyn Future<Output = Result<serde_json::Value, RemoteFailure>> + Send + '_>> {
        unimplemented!()
    }
    fn validate_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<(), RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            request.validate().unwrap();
            Ok(())
        })
    }
    fn mutate_review(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = ReviewMutationOutcome> + Send + '_>> {
        Box::pin(async move {
            assert_eq!(request.parent_node_id.as_deref(), Some("ISSUE_7"));
            self.started.notify_one();
            self.release.notified().await;
            self.writes.fetch_add(1, Ordering::SeqCst);
            if self.outcome_unknown {
                ReviewMutationOutcome::OutcomeUnknown(RemoteFailure {
                    kind: RemoteFailureKind::Transport,
                    message: "connection lost after dispatch".into(),
                })
            } else {
                ReviewMutationOutcome::Confirmed(serde_json::Value::Null)
            }
        })
    }
}
fn edit(id: &DocumentId, base: u64, sequence: u64, text: &str) -> RegionEdit {
    RegionEdit {
        document: id.clone(),
        region: RegionId("body".into()),
        base: RegionRevision(base),
        sequence: EditSequence(sequence),
        text: text.into(),
    }
}
#[tokio::test]
async fn durable_issue_save_retains_newer_typing_and_collected_close() {
    let directory = tempfile::tempdir().unwrap();
    let github = GithubService::default();
    github
        .configure_recovery(directory.path().join("forge/recovery/github/v1"))
        .unwrap();
    let pool = analysis();
    let service = IssueDocumentService::new(
        github.clone(),
        EditBudget::new(32 * 1024 * 1024).unwrap(),
        pool.clone(),
    );
    let remote = Arc::new(Remote {
        started: Default::default(),
        release: Default::default(),
        writes: AtomicUsize::new(0),
        outcome_unknown: false,
    });
    let repository = GithubRepositoryId::new("github.com", "owner", "repo").unwrap();
    let store = IssueStore::new(
        directory.path().join("issues.redb"),
        "owner/repo",
        Duration::from_secs(1),
    )
    .unwrap();
    let id = DocumentId("issue-owned-save".into());
    service
        .open(
            id.clone(),
            store.clone(),
            repository.clone(),
            7,
            remote.clone(),
        )
        .await
        .unwrap();
    let started = Arc::new(tokio::sync::Notify::new());
    let signal = started.clone();
    let (release, held) = std::sync::mpsc::channel();
    let blocker = pool
        .submit(
            forge_diff::workers::WorkPriority::Foreground,
            forge_diff::workers::WorkBudget::new(64, None),
            move |_| {
                signal.notify_one();
                let _ = held.recv();
            },
        )
        .unwrap();
    started.notified().await;
    let viewing = {
        let service = service.clone();
        let id = id.clone();
        tokio::spawn(async move {
            service
                .view(
                    &id,
                    forge_buffer::identity::ViewId("issue-width".into()),
                    forge_buffer::width::WidthProfile {
                        columns: 40,
                        ..Default::default()
                    },
                )
                .await
        })
    };
    tokio::time::timeout(Duration::from_secs(1), async {
        while pool.usage().admitted_jobs < 2 {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    tokio::time::timeout(
        Duration::from_secs(1),
        service.edit(RegionEdit {
            document: id.clone(),
            region: RegionId("title".into()),
            base: RegionRevision(0),
            sequence: EditSequence(1),
            text: "Typed while view analysis waits".into(),
        }),
    )
    .await
    .unwrap()
    .unwrap();
    release.send(()).unwrap();
    blocker.completed().await;
    viewing.await.unwrap().unwrap();
    let after_view = service.snapshot(&id).await.unwrap();
    assert_eq!(after_view.fields[0].sequence, EditSequence(1));
    assert_eq!(
        after_view
            .snapshot
            .block
            .iter()
            .find(|block| block.id.0 == "region:title")
            .unwrap()
            .text
            .wire_rows(),
        vec!["Typed while view analysis waits"]
    );
    service.edit(edit(&id, 0, 1, "captured\r\n")).await.unwrap();
    let saving = {
        let service = service.clone();
        let id = id.clone();
        tokio::spawn(async move { service.save(&id).await })
    };
    remote.started.notified().await;
    service.edit(edit(&id, 1, 2, "newer\r\n")).await.unwrap();
    let closing = {
        let service = service.clone();
        let id = id.clone();
        tokio::spawn(async move { service.close_collected(&id).await })
    };
    tokio::task::yield_now().await;
    assert!(!closing.is_finished());
    remote.release.notify_one();
    let saved = saving.await.unwrap().unwrap();
    assert!(saved.fields[2].dirty);
    assert!(closing.await.unwrap());
    let reopened = service
        .open(id.clone(), store, repository, 7, remote)
        .await
        .unwrap();
    assert_eq!(reopened.fields[2].sequence, EditSequence(2));
    assert!(reopened.fields[2].dirty);
    let rows = reopened
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "region:body")
        .unwrap()
        .text
        .wire_rows();
    assert_eq!(rows, vec!["newer\r", ""]);
    assert!(service.close_collected(&id).await);
    let report = service.shutdown(Duration::from_secs(1)).await;
    assert_eq!(report.unfinished_jobs, 0);
    assert_eq!(report.failed_jobs, 0);
}

#[tokio::test]
async fn unknown_issue_save_survives_restart_and_requires_explicit_nonreplay_recovery() {
    let directory = tempfile::tempdir().unwrap();
    let recovery_directory = directory.path().join("forge/recovery/github/v1");
    let database = directory.path().join("issues.redb");
    let repository = GithubRepositoryId::new("github.com", "owner", "repo").unwrap();
    let remote = Arc::new(Remote {
        started: Default::default(),
        release: Default::default(),
        writes: AtomicUsize::new(0),
        outcome_unknown: true,
    });
    let github = GithubService::default();
    github
        .configure_recovery(recovery_directory.clone())
        .unwrap();
    let service = IssueDocumentService::new(
        github,
        EditBudget::new(32 * 1024 * 1024).unwrap(),
        analysis(),
    );
    let id = DocumentId("issue-restart-recovery".into());
    service
        .open(
            id.clone(),
            IssueStore::new(database.clone(), "owner/repo", Duration::from_secs(1)).unwrap(),
            repository.clone(),
            7,
            remote.clone(),
        )
        .await
        .unwrap();
    service
        .edit(edit(&id, 0, 1, "captured before restart"))
        .await
        .unwrap();
    let saving = {
        let service = service.clone();
        let id = id.clone();
        tokio::spawn(async move { service.save(&id).await })
    };
    remote.started.notified().await;
    remote.release.notify_one();
    let saved = saving.await.unwrap().unwrap();
    let operation_id = saved
        .recovery
        .as_ref()
        .unwrap()
        .capture
        .operation_id
        .clone();
    assert!(matches!(
        saved.recovery.as_ref().unwrap().state,
        RecoveryPhase::OutcomeUnknown { .. }
    ));
    assert_eq!(remote.writes.load(Ordering::SeqCst), 1);
    assert!(service.close_collected(&id).await);

    let restarted_github = GithubService::default();
    restarted_github
        .configure_recovery(recovery_directory)
        .unwrap();
    let restarted = IssueDocumentService::new(
        restarted_github,
        EditBudget::new(32 * 1024 * 1024).unwrap(),
        analysis(),
    );
    let reopened = restarted
        .open(
            id.clone(),
            IssueStore::new(database, "owner/repo", Duration::from_secs(1)).unwrap(),
            repository,
            7,
            remote.clone(),
        )
        .await
        .unwrap();
    assert!(matches!(
        reopened.recovery.as_ref().unwrap().state,
        RecoveryPhase::OutcomeUnknown { .. }
    ));
    assert!(reopened.fields.iter().any(|field| field.uncertain));
    assert!(restarted.save(&id).await.is_err());
    assert_eq!(remote.writes.load(Ordering::SeqCst), 1);

    let resolved = restarted
        .resolve(
            &id,
            operation_id,
            forge_github::review_mutation::RecoveryResolution::CloseUnknown,
        )
        .await
        .unwrap();
    assert!(resolved.fresh_required);
    assert!(matches!(
        resolved.recovery.as_ref().unwrap().state,
        RecoveryPhase::UserClosedUnknown { .. }
    ));
    assert_eq!(remote.writes.load(Ordering::SeqCst), 1);
    assert!(restarted.save(&id).await.is_err());
    assert!(restarted.close_collected(&id).await);
}

#[tokio::test]
async fn issue_shutdown_deadline_retains_owned_job_until_collection() {
    let service = IssueDocumentService::new(
        GithubService::default(),
        EditBudget::new(1024).unwrap(),
        analysis(),
    );
    let release = Arc::new(tokio::sync::Notify::new());
    let waiting = release.clone();
    service
        .spawn(async move { waiting.notified().await })
        .unwrap();
    let report = service.shutdown(Duration::ZERO).await;
    assert_eq!(report.unfinished_jobs, 1);
    release.notify_one();
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

fn analysis() -> Arc<forge_diff::workers::AnalysisPool> {
    Arc::new(forge_diff::workers::AnalysisPool::new(
        forge_diff::workers::PoolLimits {
            workers: 1,
            jobs: 8,
            input_bytes: 32 * 1024 * 1024,
        },
    ))
}
