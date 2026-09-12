use std::{
    collections::VecDeque,
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
};

use anyhow::Result;
use forge_github::{
    comment::*,
    model::GithubRepositoryId,
    pull_request::PullRequestTarget,
    remote::{RemoteFailure, RemoteFailureKind},
    service::GithubService,
};

struct Remote {
    read: Mutex<VecDeque<Option<CommentState>>>,
    write: Mutex<VecDeque<CommentMutationOutcome>>,
}

impl GithubCommentRemote for Remote {
    fn create_conversation_comment(
        &self,
        _: ConversationCommentCreation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        panic!("unexpected conversation creation")
    }

    fn read_comment(
        &self,
        _: CommentTarget,
    ) -> Pin<Box<dyn Future<Output = Result<Option<CommentState>, RemoteFailure>> + Send + '_>>
    {
        Box::pin(async {
            Ok(self
                .read
                .lock()
                .unwrap()
                .pop_front()
                .expect("unexpected read"))
        })
    }
    fn mutate_comment(
        &self,
        _: CommentTarget,
        _: CommentOperation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(async {
            self.write
                .lock()
                .unwrap()
                .pop_front()
                .expect("unexpected mutation")
        })
    }
}

fn target() -> CommentTarget {
    CommentTarget {
        pull_request: PullRequestTarget {
            repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
            number: 7,
            node_id: "PR_test".into(),
        },
        kind: CommentKind::IssueComment,
        node_id: "COMMENT_test".into(),
        database_id: 123,
    }
}

fn state(body: &str) -> CommentState {
    CommentState {
        kind: CommentKind::IssueComment,
        id: "COMMENT_test".into(),
        database_id: 123,
        body: body.into(),
        url: "https://github.com/owner/repo/pull/7#issuecomment-123".into(),
        viewer_did_author: true,
    }
}

fn edit() -> CommentOperation {
    CommentOperation::Edit {
        body: "new".into(),
        receipt: "operation-1".into(),
    }
}

fn remote(read: Vec<Option<CommentState>>, write: Vec<CommentMutationOutcome>) -> Arc<Remote> {
    Arc::new(Remote {
        read: Mutex::new(read.into()),
        write: Mutex::new(write.into()),
    })
}

#[tokio::test]
async fn invalid_request_foreign_observation_and_readonly_owner_never_write() {
    let service = GithubService::default();
    let mut foreign = state("old");
    foreign.url = "https://github.com/owner/repo/pull/8#issuecomment-123".into();
    let mut readonly = state("old");
    readonly.viewer_did_author = false;
    let remote = remote(vec![Some(foreign), Some(readonly), None], vec![]);
    assert!(
        service
            .comment(
                remote.clone(),
                target(),
                CommentOperation::Edit {
                    body: " ".into(),
                    receipt: "operation-1".into()
                }
            )
            .await
            .is_err()
    );
    let mut invalid = target();
    invalid.database_id = 0;
    assert!(
        service
            .comment(remote.clone(), invalid, edit())
            .await
            .is_err()
    );
    assert!(
        service
            .comment(remote.clone(), target(), edit())
            .await
            .is_err()
    );
    assert!(matches!(
        service
            .comment(remote.clone(), target(), edit())
            .await
            .unwrap(),
        CommentResult::Rejected { .. }
    ));
    assert!(matches!(
        service.comment(remote, target(), edit()).await.unwrap(),
        CommentResult::Rejected { .. }
    ));
}

#[tokio::test]
async fn matching_body_skips_write_and_invalid_confirmation_blocks_until_observed() {
    let service = GithubService::default();
    let remote = remote(
        vec![
            Some(state("new")),
            Some(state("old")),
            Some(state("observed")),
        ],
        vec![CommentMutationOutcome::Confirmed(Some(state("wrong")))],
    );
    assert!(matches!(
        service
            .comment(remote.clone(), target(), edit())
            .await
            .unwrap(),
        CommentResult::Confirmed { .. }
    ));
    assert!(matches!(
        service
            .comment(remote.clone(), target(), edit())
            .await
            .unwrap(),
        CommentResult::OutcomeUnknown { .. }
    ));
    assert!(
        service
            .comment(remote.clone(), target(), edit())
            .await
            .is_err()
    );
    assert!(
        matches!(service.comment(remote, target(), CommentOperation::Reconcile).await.unwrap(), CommentResult::Reconciled { state: Some(state) } if state.body == "observed")
    );
}

#[tokio::test]
async fn rejected_admission_keeps_next_explicit_mutation_available() {
    let service = GithubService::default();
    let remote = remote(
        vec![Some(state("old")), Some(state("old"))],
        vec![
            CommentMutationOutcome::Rejected(RemoteFailure {
                kind: RemoteFailureKind::InvalidResponse,
                message: "not admitted".into(),
            }),
            CommentMutationOutcome::Confirmed(None),
        ],
    );
    assert!(matches!(
        service
            .comment(remote.clone(), target(), edit())
            .await
            .unwrap(),
        CommentResult::Rejected { .. }
    ));
    assert!(matches!(
        service
            .comment(
                remote,
                target(),
                CommentOperation::Delete {
                    receipt: "operation-2".into()
                }
            )
            .await
            .unwrap(),
        CommentResult::Confirmed { state: None }
    ));
}

#[test]
fn bigint_comment_identity_accepts_exact_numbers_and_rejects_lossy_values() {
    let mut encoded = serde_json::to_value(state("body")).unwrap();
    for identity in [
        serde_json::json!("4000000000"),
        serde_json::json!(4_000_000_000u64),
    ] {
        encoded["databaseId"] = identity;
        assert_eq!(
            serde_json::from_value::<CommentState>(encoded.clone())
                .unwrap()
                .database_id,
            4_000_000_000
        );
    }
    for identity in [
        serde_json::json!(1.5),
        serde_json::json!("+123"),
        serde_json::json!("12345678901234567"),
        serde_json::Value::Null,
    ] {
        encoded["databaseId"] = identity;
        assert!(serde_json::from_value::<CommentState>(encoded.clone()).is_err());
    }
}
