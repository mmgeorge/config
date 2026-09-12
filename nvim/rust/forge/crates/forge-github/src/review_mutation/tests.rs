use super::*;
use crate::metadata::RepositoryUsers;
use crate::model::{GithubRepositoryId, IssueDetail};
use crate::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};
use crate::remote::{
    IssueDetailRequest, IssuePage, IssuePageRequest, RemoteActor, RemoteFailureKind,
};
use std::sync::atomic::{AtomicUsize, Ordering};

struct Remote {
    store: crate::recovery::RecoveryStore,
    calls: AtomicUsize,
    wrong_actor: bool,
    uncertain: bool,
}

fn creation_request() -> ReviewMutationRequest {
    let mut capture = request();
    capture.resource.kind = RecoveryResourceKind::Repository;
    capture.resource.number = 0;
    capture.draft_target = Some("pr:create".into());
    capture.mutation = ReviewMutation::PullRequestCreate {
        repository_node_id: "REPOSITORY".into(),
        title: "Captured title".into(),
        body: "exact\r\nbody".into(),
        base: "main".into(),
        head: "feature".into(),
        head_commit: "1".repeat(40),
        draft: true,
    };
    capture
}

#[tokio::test]
async fn repository_creation_retains_exact_capture_and_blocks_repost_after_restart() {
    let (_directory, service, remote) = setup(false, true);
    let capture = creation_request();
    let result = service
        .review_mutation(remote.clone(), capture.clone())
        .await
        .unwrap();
    assert!(matches!(result.state, RecoveryPhase::OutcomeUnknown { .. }));
    let restarted = GithubService::default();
    restarted
        .configure_recovery(remote.store.directory().to_path_buf())
        .unwrap();
    assert!(
        restarted
            .review_mutation(remote.clone(), capture.clone())
            .await
            .is_err()
    );
    assert_eq!(remote.calls.load(Ordering::SeqCst), 1);
    let closed = restarted
        .recovery_resolve(
            remote.clone(),
            capture.resource.clone(),
            capture.operation_id.clone(),
            RecoveryResolution::CloseUnknown,
        )
        .await
        .unwrap();
    restarted
        .recovery_settle_draft(
            capture.resource.clone(),
            capture.operation_id.clone(),
            serde_json::json!({"repo":"owner/repo","number":0,"creation":{"sequence":99,"title":"newer text"}}),
        )
        .await
        .unwrap();
    assert!(matches!(
        closed.state,
        RecoveryPhase::UserClosedUnknown { .. }
    ));
    let stored = restarted
        .review_draft(capture.resource)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(stored["creation"]["title"], "newer text");
    assert_eq!(
        stored["creation_receipt"]["capture"]["submitted"]["mutation"]["body"],
        "exact\r\nbody"
    );
    assert!(
        stored["retired_operation"]
            .get(capture.operation_id)
            .is_some()
    );
    assert_eq!(remote.calls.load(Ordering::SeqCst), 1);
}

#[test]
fn repository_creation_scope_rejects_number_aliases_and_missing_durable_draft() {
    let mut capture = creation_request();
    capture.validate().unwrap();
    capture.resource.number = 1;
    assert!(capture.validate().is_err());
    capture.resource.number = 0;
    capture.resource.kind = RecoveryResourceKind::PullRequest;
    assert!(capture.validate().is_err());
    capture.resource.kind = RecoveryResourceKind::Repository;
    capture.edit_sequence = None;
    assert!(capture.validate().is_err());
}

#[tokio::test]
async fn repository_creation_draft_keeps_newer_text_and_rejects_sequence_reuse() {
    let (_directory, service, _) = setup(false, false);
    let resource = creation_request().resource;
    let draft = |sequence, title| {
        serde_json::json!({"repo":"owner/repo","number":0,
        "creation":{"sequence":sequence,"title":title,"body":"exact\r\nbody"}})
    };
    service
        .review_draft_write(resource.clone(), draft(2, "newer"))
        .await
        .unwrap();
    service
        .review_draft_write(resource.clone(), draft(1, "older"))
        .await
        .unwrap();
    assert_eq!(
        service
            .review_draft(resource.clone())
            .await
            .unwrap()
            .unwrap()["creation"]["title"],
        "newer"
    );
    assert!(
        service
            .review_draft_write(resource.clone(), draft(2, "conflicting"))
            .await
            .is_err()
    );
    service
        .review_draft_write(resource.clone(), draft(2, "newer"))
        .await
        .unwrap();
    assert_eq!(
        service.review_draft(resource).await.unwrap().unwrap()["creation"]["title"],
        "newer"
    );
}

impl GithubRemote for Remote {
    fn read_actor(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RemoteActor, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            Ok(RemoteActor {
                login: "viewer".into(),
                node_id: if self.wrong_actor { "OTHER" } else { "ACTOR" }.into(),
            })
        })
    }
    fn read_pull_request(
        &self,
        _: PullRequestTarget,
        _: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>> {
        unreachable!()
    }
    fn mutate_pull_request(
        &self,
        _: PullRequestTarget,
        _: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        unreachable!()
    }
    fn read_repository_users(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RepositoryUsers, RemoteFailure>> + Send + '_>> {
        unreachable!()
    }
    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        unreachable!()
    }
    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        unreachable!()
    }
}

impl GithubReviewWriteRemote for Remote {
    fn plan_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<ReviewMutation>, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            if let ReviewMutation::ReviewerChange { add, remove } = request.mutation {
                Ok(vec![
                    ReviewMutation::ReviewerRemove {
                        reviewer: remove.reviewer,
                        team: remove.team,
                    },
                    ReviewMutation::ReviewerAdd {
                        reviewer: add.reviewer,
                        team: add.team,
                    },
                ])
            } else if let ReviewMutation::MilestoneCreateAndAssign { title } = request.mutation {
                Ok(vec![
                    ReviewMutation::MilestoneCreate { title },
                    ReviewMutation::MilestoneAssignCreated,
                ])
            } else {
                Ok(vec![request.mutation])
            }
        })
    }
    fn read_review_write_result(
        &self,
        _: ReviewMutationRequest,
        id: u64,
    ) -> Pin<Box<dyn Future<Output = Result<Value, RemoteFailure>> + Send + '_>> {
        Box::pin(async move { Ok(serde_json::json!({"id":id})) })
    }
    fn validate_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<(), RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            assert!(matches!(
                self.store
                    .inspect(&request.resource)
                    .unwrap()
                    .unwrap()
                    .state,
                RecoveryPhase::Prepared | RecoveryPhase::DispatchPossible
            ));
            Ok(())
        })
    }
    fn mutate_review(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = ReviewMutationOutcome> + Send + '_>> {
        Box::pin(async move {
            let record = self.store.inspect(&request.resource).unwrap().unwrap();
            assert!(matches!(record.state, RecoveryPhase::DispatchPossible));
            let call = self.calls.fetch_add(1, Ordering::SeqCst);
            let grouped = record.capture.submitted["mutation"]["operation"] == "reviewer_change";
            let milestone =
                record.capture.submitted["mutation"]["operation"] == "milestone_create_and_assign";
            if milestone {
                assert_eq!(record.confirmed_steps.len(), call);
                if call == 1 {
                    assert_eq!(record.confirmed_steps[0]["number"], 55);
                    assert!(matches!(
                        request.mutation,
                        ReviewMutation::MilestoneAssign {
                            milestone_number: Some(55)
                        }
                    ));
                }
            } else if grouped {
                assert_eq!(record.confirmed_steps.len(), call);
                if call == 1 {
                    assert_eq!(record.confirmed_steps[0]["id"], 7);
                }
            } else {
                assert_eq!(
                    record.capture.submitted["mutation"]["body"],
                    "exact\r\nbody"
                );
            }
            if self.uncertain && ((!grouped && !milestone) || call == 1) {
                ReviewMutationOutcome::OutcomeUnknown(RemoteFailure {
                    kind: RemoteFailureKind::Transport,
                    message: "connection lost".into(),
                })
            } else if milestone && call == 0 {
                ReviewMutationOutcome::Confirmed(serde_json::json!({"number":55,"title":"Release"}))
            } else {
                ReviewMutationOutcome::Confirmed(serde_json::json!({"id":7}))
            }
        })
    }
}

#[tokio::test]
async fn milestone_assignment_uses_durable_creation_identity_without_recreating() {
    let (_directory, service, remote) = setup(false, true);
    let mut capture = request();
    capture.mutation = ReviewMutation::MilestoneCreateAndAssign {
        title: "Release".into(),
    };
    let result = service
        .review_mutation(remote.clone(), capture.clone())
        .await
        .unwrap();
    assert!(matches!(result.state, RecoveryPhase::OutcomeUnknown { .. }));
    assert_eq!(result.confirmed_steps[0]["number"], 55);
    assert_eq!(result.capture.submitted["mutation"]["title"], "Release");
    assert!(
        service
            .review_mutation(remote.clone(), capture)
            .await
            .is_err()
    );
    assert_eq!(remote.calls.load(Ordering::SeqCst), 2);
}

#[tokio::test]
async fn second_step_uncertainty_retains_first_confirmation_and_never_replays() {
    let (_directory, service, remote) = setup(false, true);
    let mut capture = request();
    capture.mutation = ReviewMutation::ReviewerChange {
        add: ReviewerSelection {
            reviewer: vec!["new".into()],
            team: vec![],
        },
        remove: ReviewerSelection {
            reviewer: vec!["old".into()],
            team: vec![],
        },
    };
    let result = service
        .review_mutation(remote.clone(), capture.clone())
        .await
        .unwrap();
    assert!(matches!(result.state, RecoveryPhase::OutcomeUnknown { .. }));
    assert_eq!(result.confirmed_steps, vec![serde_json::json!({"id":7})]);
    assert_eq!(remote.calls.load(Ordering::SeqCst), 2);
    assert!(
        service
            .review_mutation(remote.clone(), capture)
            .await
            .is_err()
    );
    assert_eq!(remote.calls.load(Ordering::SeqCst), 2);
}

fn request() -> ReviewMutationRequest {
    ReviewMutationRequest {
        parent_node_id: None,
        resource: RecoveryResource {
            repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
            kind: RecoveryResourceKind::PullRequest,
            number: 1,
        },
        operation_id: "operation-1".into(),
        actor_node_id: "ACTOR".into(),
        edit_sequence: Some(42),
        draft_target: Some("local-comment-1".into()),
        mutation: ReviewMutation::ConversationCreate {
            body: "exact\r\nbody".into(),
        },
    }
}

fn setup(wrong_actor: bool, uncertain: bool) -> (tempfile::TempDir, GithubService, Arc<Remote>) {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path().join("forge/recovery/github/v1");
    let service = GithubService::default();
    service.configure_recovery(root.clone()).unwrap();
    let remote = Arc::new(Remote {
        store: crate::recovery::RecoveryStore::new(root).unwrap(),
        calls: AtomicUsize::new(0),
        wrong_actor,
        uncertain,
    });
    (directory, service, remote)
}

#[tokio::test]
async fn durable_dispatch_precedes_one_attempt_and_confirmation_requires_exact_ack() {
    let (_directory, service, remote) = setup(false, false);
    let result = service
        .review_mutation(remote.clone(), request())
        .await
        .unwrap();
    assert!(matches!(result.state, RecoveryPhase::Confirmed { .. }));
    assert_eq!(result.capture.edit_sequence, Some(42));
    assert!(
        service
            .review_mutation(remote.clone(), request())
            .await
            .is_err()
    );
    assert_eq!(remote.calls.load(Ordering::SeqCst), 1);
    assert!(
        service
            .recovery_acknowledge(result.resource.clone(), "wrong-id".into())
            .await
            .is_err()
    );
    service
        .recovery_settle_draft(result.resource, result.capture.operation_id, serde_json::json!({"repo":"owner/repo","number":1,"review_comments":[],"review_comment_text":"newer unsent text"}))
        .await
        .unwrap();
    assert!(remote.store.inspect(&request().resource).unwrap().is_none());
}

#[tokio::test]
async fn actor_change_persists_rejection_without_remote_mutation() {
    let (_directory, service, remote) = setup(true, false);
    let result = service
        .review_mutation(remote.clone(), request())
        .await
        .unwrap();
    assert!(matches!(result.state, RecoveryPhase::Rejected { .. }));
    assert_eq!(remote.calls.load(Ordering::SeqCst), 0);
}

#[tokio::test]
async fn restart_never_reposts_unknown_creation_and_explicit_resolution_preserves_text() {
    let (_directory, service, remote) = setup(false, true);
    let result = service
        .review_mutation(remote.clone(), request())
        .await
        .unwrap();
    assert!(matches!(result.state, RecoveryPhase::OutcomeUnknown { .. }));
    let restarted = GithubService::default();
    restarted
        .configure_recovery(remote.store.directory().into())
        .unwrap();
    assert!(
        restarted
            .review_mutation(remote.clone(), request())
            .await
            .is_err()
    );
    assert_eq!(remote.calls.load(Ordering::SeqCst), 1);
    assert!(
        restarted
            .recovery_resolve(
                remote.clone(),
                result.resource.clone(),
                result.capture.operation_id.clone(),
                RecoveryResolution::NotDispatched
            )
            .await
            .is_err()
    );
    let resolved = restarted
        .recovery_resolve(
            remote,
            result.resource,
            result.capture.operation_id,
            RecoveryResolution::CloseUnknown,
        )
        .await
        .unwrap();
    assert!(matches!(
        resolved.state,
        RecoveryPhase::UserClosedUnknown { .. }
    ));
    assert_eq!(
        resolved.capture.submitted["mutation"]["body"],
        "exact\r\nbody"
    );
}
