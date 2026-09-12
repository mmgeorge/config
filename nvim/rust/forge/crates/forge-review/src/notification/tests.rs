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
            if number % 2 == 0 { "first" } else { "second" },
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
            if number % 2 == 0 { "first" } else { "second" }
        )),
        comment_endpoint: Some(format!(
            "repos/{}/repo/issues/comments/9",
            if number % 2 == 0 { "first" } else { "second" }
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

fn snapshot_rows(snapshot: &BufferSnapshot) -> Vec<String> {
    snapshot
        .block
        .iter()
        .flat_map(|block| {
            (0..block.text.row_count()).map(|row| block.text.row(row).unwrap().to_owned())
        })
        .collect()
}

#[tokio::test]
async fn empty_notifications_match_the_legacy_scaffold_exactly() {
    let directory = tempfile::tempdir().unwrap();
    let service = NotificationDocumentService::new(GithubService::default());
    let id = DocumentId("notification-empty".into());
    let opened = service
        .open(
            id.clone(),
            directory.path().to_owned(),
            "github.com".into(),
            Arc::new(Remote {
                writes: Mutex::new(Vec::new()),
                fail_actor: false,
                empty: true,
                page_started: None,
                page_release: None,
            }),
        )
        .await
        .unwrap();

    assert_eq!(
        snapshot_rows(&opened.snapshot),
        [
            "Hint: <tab> expand | <cr> open | b browse | S save | U unread | D done | r refresh | q close",
            "",
            "Unread:",
            "none",
            "",
            "Saved:",
            "none",
            "",
            "Done:",
            "none",
        ]
    );
    assert!(
        opened
            .snapshot
            .block
            .iter()
            .all(|block| block.metadata.fold.is_empty())
    );
    assert!(!opened.more);
    assert!(service.close_collected(&id).await);
}

#[tokio::test]
async fn populated_notifications_match_legacy_rows_and_detail_fold_boundaries() {
    let directory = tempfile::tempdir().unwrap();
    let service = NotificationDocumentService::new(GithubService::default());
    let id = DocumentId("notification-rows".into());
    let remote = Arc::new(Remote {
        writes: Mutex::new(Vec::new()),
        fail_actor: false,
        empty: false,
        page_started: None,
        page_release: None,
    });
    let opened = service
        .open(
            id.clone(),
            directory.path().to_owned(),
            "github.com".into(),
            remote,
        )
        .await
        .unwrap();
    let rows = snapshot_rows(&opened.snapshot);
    assert_eq!(
        rows[0],
        "Hint: <tab> expand | <cr> open | b browse | S save | U unread | D done | r refresh | q close"
    );
    assert_eq!(
        rows[1..4],
        ["", "Unread:", "#7 Literal *title* and [brackets] (...)"]
    );
    assert!(!rows[3].contains("second/repo"));
    assert_eq!(&rows[16..22], ["", "Saved:", "none", "", "Done:", "none"]);
    assert_eq!(rows[22], "Load more notifications");
    assert!(
        opened
            .snapshot
            .block
            .iter()
            .all(|block| block.metadata.fold.is_empty())
    );

    service
        .view(&id, ViewId("view".into()), WidthProfile::default())
        .unwrap();
    service
        .act(input(&service, &id, "1", 1, "expand"))
        .await
        .unwrap();
    let expanded = service.snapshot(&id).unwrap();
    let detail = expanded
        .block
        .iter()
        .find(|block| block.id.0 == "notification:1:detail")
        .unwrap();
    assert_eq!(
        (0..detail.text.row_count())
            .map(|row| detail.text.row(row).unwrap())
            .collect::<Vec<_>>(),
        [
            "  Last comment by person",
            "  # Native detail",
            "  ",
            "  exact body"
        ]
    );
    let header = expanded
        .block
        .iter()
        .find(|block| block.id.0 == "notification:1")
        .unwrap();
    assert_eq!(header.metadata.fold.len(), 1);
    let fold = &header.metadata.fold[0];
    assert_eq!(fold.start, TextPosition { row: 0, column: 0 });
    assert_eq!(fold.end.block, detail.id);
    assert_eq!(
        fold.end.position,
        TextPosition {
            row: detail.text.row_count(),
            column: 0,
        }
    );
    assert!(!fold.closed);
    assert!(service.close_collected(&id).await);
}

#[tokio::test]
async fn native_notifications_preserve_overlays_detail_counts_and_exact_durable_thread() {
    let directory = tempfile::tempdir().unwrap();
    let github = GithubService::default();
    github
        .configure_recovery(directory.path().join("forge/recovery/github/v1"))
        .unwrap();
    let service = NotificationDocumentService::new(github);
    let remote = Arc::new(Remote {
        writes: Mutex::new(Vec::new()),
        fail_actor: false,
        empty: false,
        page_started: None,
        page_release: None,
    });
    let id = DocumentId("notification-test".into());
    let opened = service
        .open(
            id.clone(),
            directory.path().to_owned(),
            "github.com".into(),
            remote.clone(),
        )
        .await
        .unwrap();
    std::fs::write(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../target/notification-fixture.json"),
        serde_json::to_vec(&opened.snapshot).unwrap(),
    )
    .unwrap();
    assert!(opened.more);
    service
        .view(&id, ViewId("view".into()), WidthProfile::default())
        .unwrap();
    let first = input(&service, &id, "9007199254740993", 1, "count");
    service.act(first.clone()).await.unwrap();
    assert!(service.act(first).await.is_err());
    service
        .act(input(&service, &id, "9007199254740993", 2, "expand"))
        .await
        .unwrap();
    let snapshot = service.snapshot(&id).unwrap();
    std::fs::write(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../target/notification-expanded-fixture.json"),
        serde_json::to_vec(&snapshot).unwrap(),
    )
    .unwrap();
    let title = snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "notification:9007199254740993")
        .unwrap();
    assert!(
        title
            .text
            .row(0)
            .unwrap()
            .contains("Literal *title* and [brackets]")
    );
    assert!((0..title.text.row_count()).any(|row| title.text.row(row).unwrap().contains("(5)")));
    assert!(!title.metadata.fold.is_empty());
    let outcome = service
        .act(input(&service, &id, "9007199254740993", 3, "save"))
        .await
        .unwrap();
    assert_eq!(
        serde_json::to_value(&outcome).unwrap()["recovery"]["resource"]["number"],
        "9007199254740993"
    );
    assert!(matches!(
        outcome.recovery.unwrap().state,
        forge_github::recovery::RecoveryPhase::Confirmed { .. }
    ));
    assert_eq!(
        remote.writes.lock().unwrap()[0].resource.number,
        9_007_199_254_740_993
    );
    service
        .act(input(&service, &id, "9007199254740993", 4, "unread"))
        .await
        .unwrap();
    assert_eq!(remote.writes.lock().unwrap().len(), 1);
    for number in 1..=12 {
        service
            .act(input(
                &service,
                &id,
                &number.to_string(),
                number + 4,
                "done",
            ))
            .await
            .unwrap();
    }
    let owner = service.get(&id).unwrap();
    let local = owner.lock().unwrap().local.clone();
    assert_eq!(local.lock().unwrap().done.len(), 10);
    assert_eq!(local.lock().unwrap().done.front().unwrap().id, "12");
    assert!(matches!(
        remote.writes.lock().unwrap().last().unwrap().mutation,
        ReviewMutation::NotificationDone
    ));
    drop(owner);
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
    assert!(
        reopened
            .snapshot
            .block
            .iter()
            .any(|block| block.id.0 == "notifications:Done")
    );
    assert!(service.close_collected(&id).await);
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

#[tokio::test]
async fn notification_close_collects_pending_page_without_reflowing_canonical_rows() {
    let directory = tempfile::tempdir().unwrap();
    let service = Arc::new(NotificationDocumentService::new(GithubService::default()));
    let started = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Notify::new());
    let remote = Arc::new(Remote {
        writes: Mutex::new(Vec::new()),
        fail_actor: false,
        empty: false,
        page_started: Some(started.clone()),
        page_release: Some(release.clone()),
    });
    let id = DocumentId("notification-lifetime".into());
    service
        .open(
            id.clone(),
            directory.path().to_owned(),
            "github.com".into(),
            remote,
        )
        .await
        .unwrap();
    service
        .view(&id, ViewId("view".into()), WidthProfile::default())
        .unwrap();
    let revision = service.snapshot(&id).unwrap().revision;
    let mut narrow = WidthProfile::default();
    narrow.columns = 16;
    assert!(
        service
            .view(&id, ViewId("secondary".into()), narrow)
            .unwrap()
            .is_none()
    );
    assert_eq!(service.snapshot(&id).unwrap().revision, revision);
    assert!(
        service
            .close_view(&id, &ViewId("view".into()))
            .unwrap()
            .is_none()
    );
    assert_eq!(service.snapshot(&id).unwrap().revision, revision);
    let snapshot = service.snapshot(&id).unwrap();
    let footer = snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "notifications:page")
        .unwrap();
    let captured = DocumentInput {
        document: id.clone(),
        revision: snapshot.revision,
        view: ViewId("secondary".into()),
        sequence: InputSequence(1),
        action: "more".into(),
        block: footer.id.clone(),
        position: TextPosition { row: 0, column: 0 },
        target: Some(footer.metadata.target[0].id.clone()),
    };
    let worker = service.clone();
    let action = tokio::spawn(async move { worker.act(captured).await });
    started.notified().await;
    let worker = service.clone();
    let closing_id = id.clone();
    let mut closing = tokio::spawn(async move { worker.close_collected(&closing_id).await });
    assert!(
        tokio::time::timeout(std::time::Duration::from_millis(20), &mut closing)
            .await
            .is_err()
    );
    release.notify_one();
    assert!(action.await.unwrap().is_err());
    assert!(closing.await.unwrap());
    assert!(service.snapshot(&id).is_err());
}
