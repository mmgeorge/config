use super::*;
use forge_github::{
    metadata::RepositoryUsers, model::*, notification::NotificationPage, pull_request::*,
    remote::*, review_mutation::ReviewMutationOutcome,
};
use std::{future::Future, pin::Pin};
struct Remote {
    writes: Mutex<Vec<ReviewMutationRequest>>,
    fail_actor: bool,
    empty: bool,
    page_started: Option<Arc<tokio::sync::Notify>>,
    page_release: Option<Arc<tokio::sync::Notify>>,
}
impl GithubRemote for Remote {
    fn read_actor(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RemoteActor, RemoteFailure>> + Send + '_>> {
        Box::pin(async {
            if self.fail_actor {
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
        unimplemented!()
    }
}
fn record(number: u64) -> NotificationRecord {
    NotificationRecord {
        id: number.to_string(),
        repository: GithubRepositoryId::new(
            "github.com",
            if number.is_multiple_of(2) {
                "first"
            } else {
                "second"
            },
            "repo",
        )
        .unwrap(),
        title: "Literal *title* and [brackets]".into(),
        kind: "Issue".into(),
        unread: true,
        reason: "mention".into(),
        updated_at: "today".into(),
        subject_endpoint: Some(format!(
            "repos/{}/repo/issues/7",
            if number.is_multiple_of(2) {
                "first"
            } else {
                "second"
            }
        )),
        comment_endpoint: Some(format!(
            "repos/{}/repo/issues/comments/9",
            if number.is_multiple_of(2) {
                "first"
            } else {
                "second"
            }
        )),
        number: Some(7),
        browser_url: Some("https://github.com/first/repo/issues/7".into()),
    }
}
impl GithubNotificationRemote for Remote {
    fn notification_page(
        &self,
        request: NotificationReadRequest,
    ) -> Pin<Box<dyn Future<Output = Result<NotificationPage, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            if request.cursor.is_some() {
                if let Some(started) = &self.page_started {
                    started.notify_one();
                }
                if let Some(release) = &self.page_release {
                    release.notified().await;
                }
            }
            assert_eq!(request.hostname, "github.com");
            Ok(NotificationPage {
                record: if self.empty {
                    Vec::new()
                } else if request.cursor.is_none() {
                    (1..=12)
                        .map(record)
                        .chain([record(9_007_199_254_740_993)])
                        .collect()
                } else {
                    vec![record(20)]
                },
                next_cursor: if self.empty {
                    None
                } else if request.cursor.is_none() {
                    Some(2)
                } else {
                    None
                },
                complete: request.cursor.is_some(),
            })
        })
    }
    fn notification_detail(
        &self,
        request: NotificationDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<NotificationDetail, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            request.endpoint().unwrap();
            Ok(NotificationDetail {
                body: "# Native detail\n\nexact body".into(),
                author: Some("person".into()),
                comments: if request.latest_comment {
                    None
                } else {
                    Some(5)
                },
                comment: request.latest_comment,
            })
        })
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
            self.writes.lock().unwrap().push(request);
            ReviewMutationOutcome::Confirmed(serde_json::Value::Null)
        })
    }
}
fn input(
    service: &NotificationDocumentService,
    id: &DocumentId,
    record: &str,
    sequence: u64,
    action: &str,
) -> DocumentInput {
    let snapshot = service.snapshot(id).unwrap();
    let block = snapshot
        .block
        .iter()
        .find(|block| block.id.0 == format!("notification:{record}"))
        .unwrap();
    DocumentInput {
        document: id.clone(),
        revision: snapshot.revision,
        view: ViewId("view".into()),
        sequence: InputSequence(sequence),
        action: action.into(),
        block: block.id.clone(),
        position: TextPosition { row: 0, column: 0 },
        target: Some(block.metadata.target[0].id.clone()),
    }
}

#[tokio::test]
async fn local_saved_choice_survives_remote_admission_failure_and_reopen() {
    let directory = tempfile::tempdir().unwrap();
    let service = NotificationDocumentService::new(GithubService::default());
    let remote = Arc::new(Remote {
        writes: Mutex::new(Vec::new()),
        fail_actor: true,
        empty: false,
        page_started: None,
        page_release: None,
    });
    let id = DocumentId("notification-local-choice".into());
    service
        .open(
            id.clone(),
            directory.path().to_owned(),
            "github.com".into(),
            remote.clone(),
        )
        .await
        .unwrap();
    service
        .view(&id, ViewId("view".into()), WidthProfile::default())
        .unwrap();
    let result = service
        .act(input(&service, &id, "1", 1, "save"))
        .await
        .unwrap();
    assert!(
        result
            .diagnostic
            .unwrap()
            .contains("captured account is unavailable")
    );
    assert!(result.patch.is_some());
    assert!(result.recovery.is_none());
    assert!(remote.writes.lock().unwrap().is_empty());
    assert!(service.close_collected(&id).await);
    let reopened = service
        .open(
            id.clone(),
            directory.path().to_owned(),
            "github.com".into(),
            remote,
        )
        .await
        .unwrap();
    let saved = reopened
        .snapshot
        .block
        .iter()
        .position(|block| block.id.0 == "notifications:Saved")
        .unwrap();
    assert_eq!(reopened.snapshot.block[saved + 1].id.0, "notification:1");
    assert!(service.close_collected(&id).await);
}
