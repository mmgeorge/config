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
    let service = IssueDocumentService::new(github, EditBudget::default(), analysis());
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
        tokio::spawn(async move { service.save(&id, vec![]).await })
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
    let restarted = IssueDocumentService::new(restarted_github, EditBudget::default(), analysis());
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
    assert!(restarted.save(&id, vec![]).await.is_err());
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
    assert!(restarted.save(&id, vec![]).await.is_err());
    assert!(restarted.close_collected(&id).await);
}

fn analysis() -> Arc<forge_diff::workers::AnalysisPool> {
    Arc::new(forge_diff::workers::AnalysisPool::new(
        forge_diff::workers::PoolLimits {
            workers: 1,
            jobs: 8,
        },
    ))
}
