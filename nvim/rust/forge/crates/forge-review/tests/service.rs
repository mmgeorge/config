use std::future::Future;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use forge_buffer::identity::{DocumentId, EditSequence, RegionId};
use forge_github::comment::*;
use forge_github::metadata::RepositoryUsers;
use forge_github::model::{GithubRepositoryId, IssueDetail};
use forge_github::pull_request::*;
use forge_github::remote::{
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure, RemoteFailureKind,
};
use forge_github::service::GithubService;
use forge_review::edit::RegionEdit;
use forge_review::service::ReviewService;
use tokio::sync::Semaphore;

struct TestRemote {
    source_content: Mutex<Option<(Vec<u8>, Vec<u8>)>>,
    source_request: Mutex<Vec<forge_github::review_source::ReviewSourceRequest>>,
    section_page: Mutex<std::collections::VecDeque<forge_github::review_api::ReviewPage>>,
    recovery: tempfile::TempDir,
    state: Mutex<PullRequestState>,
    comment: Mutex<Option<CommentState>>,
    comment_writes: Mutex<Vec<CommentOperation>>,
    creation: Mutex<Vec<ConversationCommentCreation>>,
    inline_creation: Mutex<Vec<forge_github::review_mutation::ReviewMutation>>,
    submission: Mutex<Vec<forge_github::review_mutation::ReviewMutationRequest>>,
    writes: Mutex<Vec<PullRequestMutation>>,
    entered: Semaphore,
    release: Semaphore,
    hold: AtomicBool,
    uncertain: AtomicBool,
    reject: AtomicBool,
    panic: AtomicBool,
}

impl GithubRemote for TestRemote {
    fn read_actor(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<forge_github::remote::RemoteActor, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(async {
            Ok(forge_github::remote::RemoteActor {
                login: "viewer".into(),
                node_id: "ACTOR_test".into(),
            })
        })
    }
    fn read_pull_request(
        &self,
        _: PullRequestTarget,
        _: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>> {
        Box::pin(async { Ok(self.state.lock().unwrap().clone()) })
    }
    fn mutate_pull_request(
        &self,
        _: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        Box::pin(async move {
            self.writes.lock().unwrap().push(mutation.clone());
            self.entered.add_permits(1);
            if self.hold.load(Ordering::Acquire) {
                self.release.acquire().await.unwrap().forget();
            }
            assert!(!self.panic.load(Ordering::Acquire), "injected remote panic");
            let failure = || RemoteFailure {
                kind: RemoteFailureKind::Unauthorized,
                message: "injected remote failure".into(),
            };
            if self.reject.load(Ordering::Acquire) {
                return PullRequestMutationOutcome::Rejected(failure());
            }
            let mut state = self.state.lock().unwrap();
            match mutation {
                PullRequestMutation::Edit(edit) => {
                    if let Some(title) = edit.title {
                        state.title = Some(title);
                    }
                    if let Some(body) = edit.body {
                        state.body = Some(body);
                    }
                }
                PullRequestMutation::Close => state.state = PullRequestLifecycle::Closed,
                PullRequestMutation::Reopen => state.state = PullRequestLifecycle::Open,
                PullRequestMutation::Draft => state.is_draft = true,
                PullRequestMutation::Ready => state.is_draft = false,
            }
            if self.uncertain.load(Ordering::Acquire) {
                PullRequestMutationOutcome::Uncertain(failure())
            } else {
                PullRequestMutationOutcome::Confirmed(state.clone())
            }
        })
    }
    fn read_repository_users(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RepositoryUsers, RemoteFailure>> + Send + '_>> {
        panic!("unexpected metadata read")
    }
    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        panic!("unexpected issue read")
    }
    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        panic!("unexpected detail read")
    }
}

fn remote() -> Arc<TestRemote> {
    Arc::new(TestRemote {
        source_content: Mutex::new(None),
        source_request: Mutex::new(Vec::new()),
        section_page: Mutex::new(std::collections::VecDeque::new()),
        recovery: tempfile::tempdir().unwrap(),
        comment: Mutex::new(Some(comment_state())),
        comment_writes: Mutex::new(Vec::new()),
        creation: Mutex::new(Vec::new()),
        inline_creation: Mutex::new(Vec::new()),
        submission: Mutex::new(Vec::new()),
        state: Mutex::new(PullRequestState {
            node_id: "PR_test".into(),
            number: 7,
            state: PullRequestLifecycle::Open,
            is_draft: false,
            title: Some("Initial title".into()),
            body: Some("Initial body".into()),
        }),
        writes: Mutex::new(Vec::new()),
        entered: Semaphore::new(0),
        release: Semaphore::new(0),
        hold: AtomicBool::new(false),
        uncertain: AtomicBool::new(false),
        reject: AtomicBool::new(false),
        panic: AtomicBool::new(false),
    })
}

impl forge_github::review_api::GithubReviewRemote for TestRemote {
    fn read_comparison(
        &self,
        request: forge_github::review_source::ReviewComparisonRequest,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = Result<forge_github::review_source::ReviewComparison, RemoteFailure>,
                > + Send
                + '_,
        >,
    > {
        Box::pin(async move {
            Ok(forge_github::review_source::ReviewComparison {
                base: request.base,
                head: request.head,
                merge_base: "c".repeat(40),
            })
        })
    }
    fn read_source(
        &self,
        request: forge_github::review_source::ReviewSourceRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<forge_github::review_source::ReviewSource, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(async move {
            self.source_request.lock().unwrap().push(request.clone());
            let old = request.commit == "c".repeat(40);
            Ok(forge_github::review_source::ReviewSource {
                request,
                blob: Some(if old { "d".repeat(40) } else { "e".repeat(40) }),
                bytes: self
                    .source_content
                    .lock()
                    .unwrap()
                    .as_ref()
                    .map(|(previous, current)| {
                        if old {
                            previous.clone()
                        } else {
                            current.clone()
                        }
                    })
                    .unwrap_or_else(|| {
                        if old {
                            b"fn old() {}\r\n".to_vec()
                        } else {
                            b"fn new() {}\n".to_vec()
                        }
                    }),
            })
        })
    }
    fn read_review(
        &self,
        _: forge_github::review_api::ReviewReadRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<forge_github::review_api::ReviewPage, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(async {
            self.section_page
                .lock()
                .unwrap()
                .pop_front()
                .ok_or(RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: "no queued section page".into(),
                })
        })
    }
}

#[tokio::test]
async fn section_pages_retain_previous_records_after_duplicate_or_stale_delivery() {
    use forge_buffer::width::WidthProfile;
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;
    let remote = remote();
    let record = |identity| serde_json::json!({"id": identity, "body":"**Native Markdown**", "user":{"login":"author"}});
    remote.section_page.lock().unwrap().extend([
        ReviewPage {
            records: vec![record(1)],
            next_cursor: Some("2".into()),
            complete: false,
        },
        ReviewPage {
            records: vec![record(1)],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![record(2)],
            next_cursor: None,
            complete: true,
        },
    ]);
    let pool = Arc::new(AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
        input_bytes: 32 * 1024 * 1024,
    }));
    let service = ReviewService::with_analysis(github(&remote), pool.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let directory = remote.recovery.path().to_path_buf();
    let first = service
        .read_section(
            &opened.document,
            directory.clone(),
            remote.clone(),
            ReviewSectionKind::Conversation,
            None,
        )
        .await
        .unwrap();
    assert_eq!(first.item.len(), 1);
    assert!(!first.complete);
    assert!(
        service
            .read_section(
                &opened.document,
                directory.clone(),
                remote.clone(),
                ReviewSectionKind::Conversation,
                Some("wrong".into())
            )
            .await
            .is_err()
    );
    let failed = service
        .read_section(
            &opened.document,
            directory.clone(),
            remote.clone(),
            ReviewSectionKind::Conversation,
            Some("2".into()),
        )
        .await
        .unwrap_err();
    assert!(failed.to_string().contains("duplicate"));
    let retained = service
        .materialize(&opened.document, WidthProfile::default())
        .await
        .unwrap();
    assert!(retained.snapshot.block.iter().any(|block| {
        block
            .text
            .wire_rows()
            .join("\n")
            .contains("Native Markdown")
    }));
    assert!(
        retained.snapshot.block.iter().any(|block| block
            .text
            .wire_rows()
            .join("\n")
            .contains("duplicate"))
    );
    let completed = service
        .read_section(
            &opened.document,
            directory,
            remote.clone(),
            ReviewSectionKind::Conversation,
            Some("2".into()),
        )
        .await
        .unwrap();
    assert_eq!(completed.item.len(), 2);
    assert!(Arc::ptr_eq(&first.item[0], &completed.item[0]));
    assert!(completed.complete && completed.next_cursor.is_none());
    assert_eq!(pool.usage().admitted_jobs, 0);
    service.close_document(&opened.document).unwrap();
}

fn recovery_journal_path(root: &std::path::Path, operation_id: &str) -> std::path::PathBuf {
    fn find_operation(
        directory: &std::path::Path,
        operation_id: &str,
    ) -> Option<std::path::PathBuf> {
        for entry in std::fs::read_dir(directory).ok()? {
            let path = entry.ok()?.path();
            if path.is_dir() {
                if let Some(found) = find_operation(&path, operation_id) {
                    return Some(found);
                }
            } else if path
                .extension()
                .is_some_and(|extension| extension == "json")
            {
                let record: serde_json::Value =
                    serde_json::from_slice(&std::fs::read(&path).ok()?).ok()?;
                if record["capture"]["operation_id"] == operation_id {
                    return Some(path);
                }
            }
        }
        None
    }

    find_operation(root, operation_id).expect("batched submission must retain its recovery journal")
}

fn github(remote: &TestRemote) -> GithubService {
    let service = GithubService::default();
    service
        .configure_recovery(remote.recovery.path().join("forge/recovery/github/v1"))
        .unwrap();
    service
}

#[tokio::test]
async fn checks_and_submitted_reviews_preserve_the_lua_summary_rows() {
    use forge_buffer::width::WidthProfile;
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;

    let make_remote = remote;
    let remote = make_remote();
    remote.section_page.lock().unwrap().extend([
        ReviewPage {
            records: vec![
                serde_json::json!({
                    "id": 41,
                    "name": "Build",
                    "status": "completed",
                    "conclusion": "success",
                    "workflow_name": "CI",
                    "detailsUrl": "https://example.invalid/check/41"
                }),
                serde_json::json!({
                    "id": 42, "name": "Lint", "conclusion": "failure"
                }),
                serde_json::json!({
                    "id": 43, "name": "Optional", "conclusion": "skipped"
                }),
                serde_json::json!({
                    "id": 44, "name": "Deploy", "status": "in_progress"
                }),
            ],
            next_cursor: Some("checks:2".into()),
            complete: false,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "id": 51,
                "user": { "login": "alice" },
                "state": "APPROVED",
                "submitted_at": "2000-01-02T03:04:05Z",
                "body": "Ship it\r\n```rust\r\nignored()\r\n```\r\nEnd"
            })],
            next_cursor: None,
            complete: true,
        },
    ]);
    let service = ReviewService::with_analysis(
        github(&remote),
        Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 32 * 1024 * 1024,
        })),
    );
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let directory = remote.recovery.path().to_path_buf();
    for section in [ReviewSectionKind::Checks, ReviewSectionKind::Reviews] {
        service
            .read_section(
                &opened.document,
                directory.clone(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let materialized = service
        .materialize(&opened.document, WidthProfile::default())
        .await
        .unwrap();
    let check = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0.starts_with("section:Checks:") && block.id.0.ends_with(":summary"))
        .expect("check summary row");
    assert_eq!(check.text.wire_rows(), ["✓ Build | CI"]);
    assert_eq!(
        check
            .metadata
            .decoration
            .iter()
            .map(|decoration| (
                decoration.capture.as_str(),
                decoration.range.start.column,
                decoration.range.end.column,
            ))
            .collect::<Vec<_>>(),
        [
            ("ForgeAddRange", 0, 3),
            ("ForgeStatusPath", 4, 9),
            ("Comment", 9, 12),
            ("ForgeStatusRemote", 12, 14),
        ]
    );
    assert_eq!(check.metadata.target.len(), 1);
    let check_rows = materialized
        .snapshot
        .block
        .iter()
        .filter(|block| {
            block.id.0.starts_with("section:Checks:") && block.id.0.ends_with(":summary")
        })
        .map(|block| {
            (
                block.text.wire_rows()[0].to_owned(),
                block.metadata.decoration[0].capture.clone(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        check_rows,
        [
            ("✓ Build | CI".into(), "ForgeAddRange".into()),
            ("✗ Lint".into(), "ForgeDeleteRange".into()),
            ("! Optional".into(), "ForgeStatusFetching".into()),
            ("◷ Deploy".into(), "ForgeStatusFetching".into()),
        ]
    );
    let loading = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "section:Checks:completion")
        .expect("incomplete checks message");
    assert_eq!(loading.text.wire_rows(), ["...loading checks..."]);
    assert_eq!(
        loading.metadata.decoration[0].capture,
        "ForgeStatusFetching"
    );

    let review = materialized
        .snapshot
        .block
        .iter()
        .find(|block| {
            block.id.0.starts_with("section:Reviews:") && block.id.0.ends_with(":summary")
        })
        .expect("submitted review summary row");
    assert_eq!(
        review.text.wire_rows(),
        ["✓ alice January 2, 2000  Ship it End"]
    );
    assert_eq!(
        review
            .metadata
            .decoration
            .iter()
            .map(|decoration| (
                decoration.capture.as_str(),
                decoration.range.start.column,
                decoration.range.end.column,
            ))
            .collect::<Vec<_>>(),
        [
            ("ForgeAddRange", 0, 3),
            ("ForgeReviewComment", 4, 9),
            ("ForgeStatusDate", 10, 25),
            ("ForgeReviewComment", 27, 38),
        ]
    );
    assert_eq!(review.metadata.fold.len(), 1);
    assert!(review.metadata.fold[0].closed);
    let review_body = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0.starts_with("section:Reviews:") && block.id.0.ends_with(":body"))
        .expect("submitted review body");
    assert_eq!(review.metadata.fold[0].end.block, review_body.id);
    assert_eq!(
        review.metadata.fold[0].end.position.row,
        review_body.text.row_count()
    );

    let empty_remote = make_remote();
    empty_remote
        .section_page
        .lock()
        .unwrap()
        .push_back(ReviewPage {
            records: Vec::new(),
            next_cursor: None,
            complete: true,
        });
    let empty_service = ReviewService::with_analysis(
        github(&empty_remote),
        Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 32 * 1024 * 1024,
        })),
    );
    let empty = empty_service
        .open_pr(empty_remote.clone(), target())
        .await
        .unwrap();
    empty_service
        .read_section(
            &empty.document,
            empty_remote.recovery.path().into(),
            empty_remote,
            ReviewSectionKind::Checks,
            None,
        )
        .await
        .unwrap();
    let empty = empty_service
        .materialize(&empty.document, WidthProfile::default())
        .await
        .unwrap();
    let empty = empty
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "section:Checks:empty")
        .expect("empty checks row");
    assert_eq!(empty.text.wire_rows(), ["No checks"]);
    assert_eq!(empty.metadata.decoration[0].capture, "ForgeStatusDate");
}

#[tokio::test]
async fn pull_request_head_row_preserves_commit_date_branch_and_subject() {
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;

    let remote = remote();
    remote.section_page.lock().unwrap().extend([
        ReviewPage {
            records: vec![serde_json::json!({
                "number": 7,
                "node_id": "PR_test",
                "title": "Initial title",
                "state": "open",
                "html_url": "https://example.invalid/owner/repo/pull/7",
                "base": { "sha": "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" },
                "head": {
                    "sha": "abc1234def5678abc1234def5678abc1234def56",
                    "ref": "feature",
                    "repo": { "full_name": "owner/repo" }
                },
                "milestone": { "title": "v1.2.0" }
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "users": [{ "login": "alice-dev" }, { "login": "bobtown" }],
                "teams": []
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![
                serde_json::json!({
                    "sha": "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                    "commit": {
                        "message": "feat: base commit",
                        "committer": { "date": "1 day ago" }
                    }
                }),
                serde_json::json!({
                    "sha": "abc1234def5678abc1234def5678abc1234def56",
                    "commit": {
                        "message": "chore: head commit\n\nHead commit body.",
                        "committer": { "date": "1 day ago" }
                    }
                }),
            ],
            next_cursor: None,
            complete: true,
        },
    ]);
    let service = ReviewService::with_analysis(
        github(&remote),
        Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 32 * 1024 * 1024,
        })),
    );
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    for section in [
        ReviewSectionKind::Overview,
        ReviewSectionKind::RequestedReviewers,
        ReviewSectionKind::Commits,
    ] {
        service
            .read_section(
                &opened.document,
                remote.recovery.path().into(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let materialized = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let head = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "overview:Head")
        .expect("head row");

    assert_eq!(
        head.text.wire_rows(),
        ["Head:   abc1234 1 day ago feature chore: head commit"]
    );
    assert!(head.metadata.decoration.iter().any(|decoration| {
        decoration.capture == "ForgeStatusDate"
            && decoration.range.start.column == 16
            && decoration.range.end.column == 25
    }));
    assert!(head.metadata.decoration.iter().any(|decoration| {
        decoration.capture == "ForgeStatusCommitType"
            && decoration.range.start.column == 34
            && decoration.range.end.column == 39
    }));
    assert!(!head.metadata.decoration.iter().any(|decoration| {
        decoration.range.start.column >= 39
            && decoration.range.end.column <= head.text.wire_rows()[0].len()
    }));
    let release = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "overview:Release")
        .expect("release row");
    assert_eq!(release.text.wire_rows(), ["Release: ◆ v1.2.0"]);
    assert!(
        release.metadata.decoration.iter().any(|decoration| {
            decoration.capture == "ForgeStatusBranch"
                && decoration.range.start.column == "Release: ".len()
                && decoration.range.end.column == "Release: ◆ v1.2.0".len()
        }),
        "{:?}",
        release.metadata.decoration
    );
    let review = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "overview:Review")
        .expect("review row");
    assert_eq!(review.text.wire_rows(), ["Review: ◷ @alice-dev @bobtown"]);
    assert!(review.metadata.decoration.iter().any(|decoration| {
        decoration.capture == "ForgeReviewPending"
            && decoration.range.start.column == "Review: ".len()
            && decoration.range.end.column == "Review: ◷ @alice-dev @bobtown".len()
    }));
    let activity = materialized
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "overview:Activity")
        .expect("activity row");
    assert_eq!(activity.text.wire_rows(), ["Activity: 1 day ago"]);
}

#[tokio::test]
async fn pull_request_recent_commits_are_last_newest_first_and_closed() {
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;

    let remote = remote();
    let directory = remote.recovery.path().to_path_buf();
    remote.section_page.lock().unwrap().extend([
        ReviewPage {
            records: vec![serde_json::json!({
                "number": 7,
                "node_id": "PR_test",
                "title": "Initial title",
                "state": "open",
                "base": { "sha": "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" },
                "head": {
                    "sha": "abc1234def5678abc1234def5678abc1234def56",
                    "ref": "feature",
                    "repo": { "full_name": "fork/project" }
                }
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![
                serde_json::json!({
                    "sha": "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                    "commit": {
                        "message": "feat: base commit",
                        "committer": { "date": "2 days ago" }
                    }
                }),
                serde_json::json!({
                    "sha": "abc1234def5678abc1234def5678abc1234def56",
                    "commit": {
                        "message": "chore: head commit\n\nHead commit body.",
                        "committer": { "date": "1 day ago" }
                    }
                }),
            ],
            next_cursor: None,
            complete: true,
        },
    ]);
    let service = ReviewService::with_analysis(
        github(&remote),
        Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 32 * 1024 * 1024,
        })),
    );
    let opened = service
        .open_pr_in_directory(directory.clone(), remote.clone(), target())
        .await
        .unwrap();
    for section in [ReviewSectionKind::Overview, ReviewSectionKind::Commits] {
        service
            .read_section(
                &opened.document,
                remote.recovery.path().into(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let materialized = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let blocks = &materialized.snapshot.block;
    let title = blocks
        .iter()
        .find(|block| block.id.0 == "region:title")
        .expect("title value");
    assert_eq!(title.metadata.decoration.len(), 1);
    assert_eq!(title.metadata.decoration[0].capture, "ForgeStatusPath");
    assert_eq!(
        title.metadata.decoration[0].range,
        forge_buffer::block::TextRange {
            start: forge_buffer::block::TextPosition { row: 0, column: 0 },
            end: forge_buffer::block::TextPosition {
                row: 0,
                column: "Initial title".len(),
            },
        }
    );
    let heading_index = blocks
        .iter()
        .position(|block| block.text.wire_rows() == ["Recent Commits (2):"])
        .expect("recent commits heading");
    let commit_rows = blocks[heading_index + 1..]
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>();

    assert_eq!(commit_rows[0], "abc1234  1 day ago  chore: head commit");
    assert_eq!(commit_rows[1], "1111111  2 days ago feat: base commit");
    assert_eq!(heading_index + 3, blocks.len());
    assert_eq!(blocks[heading_index].metadata.fold.len(), 1);
    assert!(blocks[heading_index].metadata.fold[0].closed);

    let head_commit = blocks
        .iter()
        .find(|block| block.text.wire_rows() == ["abc1234  1 day ago  chore: head commit"])
        .expect("head commit row");
    let head_rows = head_commit.text.wire_rows();
    let head_row = &head_rows[0];
    let subject_column = head_row.find("chore: head commit").unwrap();
    assert_eq!(head_commit.metadata.target.len(), 2);
    assert!(head_commit.metadata.target[0].id.0.ends_with(":diff"));
    assert_eq!(
        head_commit.metadata.target[0].range,
        forge_buffer::block::TextRange {
            start: forge_buffer::block::TextPosition { row: 0, column: 0 },
            end: forge_buffer::block::TextPosition {
                row: 0,
                column: subject_column,
            },
        }
    );
    assert!(head_commit.metadata.target[1].id.0.ends_with(":message"));
    assert_eq!(
        head_commit.metadata.target[1].range,
        forge_buffer::block::TextRange {
            start: forge_buffer::block::TextPosition {
                row: 0,
                column: subject_column,
            },
            end: forge_buffer::block::TextPosition {
                row: 0,
                column: head_row.len(),
            },
        }
    );

    let view = forge_buffer::identity::ViewId("recent-commit-activation".into());
    service
        .view(&opened.document, view.clone(), Default::default())
        .await
        .unwrap();
    let effect = service
        .act(
            forge_buffer::input::DocumentInput {
                document: opened.document.clone(),
                revision: materialized.snapshot.revision,
                view,
                sequence: forge_buffer::identity::InputSequence(1),
                action: "activate".into(),
                block: head_commit.id.clone(),
                position: forge_buffer::block::TextPosition {
                    row: 0,
                    column: subject_column,
                },
                target: Some(head_commit.metadata.target[1].id.clone()),
            },
            directory,
        )
        .await
        .unwrap()
        .effect
        .expect("commit activation effect");
    assert_eq!(effect.kind, "open_commit");
    assert_eq!(
        effect.workspace.as_deref(),
        Some(remote.recovery.path().to_string_lossy().as_ref())
    );
    assert_eq!(
        effect.oid.as_deref(),
        Some("abc1234def5678abc1234def5678abc1234def56")
    );
}

#[tokio::test]
async fn pull_request_sections_keep_legacy_order_and_open_description_fold() {
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;

    let remote = remote();
    remote.section_page.lock().unwrap().extend([
        ReviewPage {
            records: vec![serde_json::json!({
                "number": 7,
                "node_id": "PR_test",
                "title": "Initial title",
                "state": "open",
                "html_url": "https://example.invalid/owner/repo/pull/7",
                "milestone": { "title": "v1.2.0" },
                "base": { "sha": "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" },
                "head": {
                    "sha": "abc1234def5678abc1234def5678abc1234def56",
                    "ref": "feature",
                    "repo": { "full_name": "owner/repo" }
                }
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "users": [{ "login": "alice" }],
                "teams": [{ "slug": "core" }]
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: Vec::new(),
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "id": 31,
                "user": { "login": "alice" },
                "state": "APPROVED",
                "submitted_at": "2 days ago",
                "body": "Looks good"
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "id": 41,
                "user": { "login": "alice" },
                "body": "Please retain this behavior",
                "created_at": "2 days ago",
                "html_url": "https://example.invalid/owner/repo/pull/7#issuecomment-41"
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "filename": "notes.txt",
                "status": "modified",
                "sha": "c".repeat(40),
                "additions": 1,
                "deletions": 1
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![
                serde_json::json!({
                    "sha": "1111111aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
                    "commit": {
                        "message": "feat: base commit",
                        "committer": { "date": "3 days ago" }
                    }
                }),
                serde_json::json!({
                    "sha": "abc1234def5678abc1234def5678abc1234def56",
                    "commit": {
                        "message": "chore: head commit\n\nFull body",
                        "committer": { "date": "2 days ago" }
                    }
                }),
            ],
            next_cursor: None,
            complete: true,
        },
    ]);
    let service = ReviewService::with_analysis(
        github(&remote),
        Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
            input_bytes: 32 * 1024 * 1024,
        })),
    );
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    for section in [
        ReviewSectionKind::Overview,
        ReviewSectionKind::RequestedReviewers,
        ReviewSectionKind::Checks,
        ReviewSectionKind::Reviews,
        ReviewSectionKind::Conversation,
        ReviewSectionKind::Files,
        ReviewSectionKind::Commits,
    ] {
        service
            .read_section(
                &opened.document,
                remote.recovery.path().into(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let materialized = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let blocks = &materialized.snapshot.block;
    let heading_index = |text: &str| {
        blocks
            .iter()
            .position(|block| block.text.wire_rows() == [text])
            .unwrap_or_else(|| panic!("missing {text}"))
    };
    let description = heading_index("Description:");
    let checks = heading_index("Checks:");
    let reviews = heading_index("Reviews (1):");
    let comments = heading_index("Comments (1):");
    let changes = heading_index("Changes (1):");
    let commits = heading_index("Recent Commits (2):");

    assert!(
        description < checks
            && checks < reviews
            && reviews < comments
            && comments < changes
            && changes < commits
    );
    assert_eq!(
        blocks[heading_index("Activity: 2 days ago")]
            .text
            .wire_rows(),
        ["Activity: 2 days ago"]
    );
    for row in [
        "Release: ◆ v1.2.0",
        "Review: ◷ @alice @core",
        "✓ alice 2 days ago  Looks good",
        "Modified notes.txt +1 -1",
        "abc1234  2 days ago chore: head commit",
        "1111111  3 days ago feat: base commit",
    ] {
        heading_index(row);
    }
    let fold = &blocks[description].metadata.fold;
    assert_eq!(fold.len(), 1);
    assert_eq!(fold[0].id.0, "review:description");
    assert!(!fold[0].closed);
    assert_eq!(fold[0].end.block, blocks[description + 1].id);
    assert_eq!(
        fold[0].end.position.row,
        blocks[description + 1].text.row_count()
    );
    let conversation = blocks
        .iter()
        .find(|block| {
            block.id.0.starts_with("section:Conversation:") && block.id.0.ends_with(":summary")
        })
        .expect("conversation summary");
    assert_eq!(
        conversation.text.wire_rows(),
        ["󰅺 alice 2 days ago  Please retain this behavior"]
    );
    assert_eq!(conversation.metadata.decoration.len(), 2);
    assert_eq!(
        conversation.metadata.decoration[0].capture,
        "ForgeStatusPath"
    );
    assert_eq!(conversation.metadata.decoration[0].range.start.column, 0);
    assert_eq!(
        conversation.metadata.decoration[0].range.end.column,
        conversation.text.wire_rows()[0].len()
    );
    assert_eq!(conversation.metadata.decoration[1].capture, "ForgeStatusPR");
    assert_eq!(conversation.metadata.decoration[1].priority, 110);
    assert_eq!(conversation.metadata.decoration[1].range.start.column, 0);
    assert_eq!(
        conversation.metadata.decoration[1].range.end.column,
        "󰅺".len()
    );
    assert_eq!(conversation.metadata.target.len(), 1);
    assert_eq!(conversation.metadata.target[0].range.start.column, 0);
    assert_eq!(
        conversation.metadata.target[0].range.end.column,
        conversation.text.wire_rows()[0].len()
    );
    let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/review-pr-rich-fixture.json");
    std::fs::write(
        fixture,
        serde_json::to_vec(&serde_json::json!({
            "snapshot": materialized.snapshot,
        }))
        .unwrap(),
    )
    .unwrap();
}

#[tokio::test]
async fn immutable_file_continuation_publishes_only_new_rows_after_an_edit() {
    use forge_diff::{
        cache::CacheLimits,
        engine::DiffEngine,
        syntax::{SyntaxEngine, SyntaxLimits},
    };
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;
    let remote = remote();
    *remote.source_content.lock().unwrap() = Some((
        (0..180)
            .map(|row| format!("old {row}\n"))
            .collect::<String>()
            .into_bytes(),
        (0..180)
            .map(|row| format!("new {row}\n"))
            .collect::<String>()
            .into_bytes(),
    ));
    remote.section_page.lock().unwrap().extend([
        ReviewPage { records: vec![serde_json::json!({"number":7,"node_id":"PR_test","title":"Initial title", "state":"open", "html_url":"https://example.invalid/owner/repo/pull/7",
            "base":{"sha":"a".repeat(40)},"head":{"sha":"b".repeat(40),"repo":{"full_name":"fork/project"}}})], next_cursor: None, complete:true },
        ReviewPage { records: vec![serde_json::json!({"filename":"notes.txt","status":"modified","sha":"e".repeat(40),"additions":180,"deletions":180})], next_cursor:None, complete:true },
        ReviewPage { records: vec![], next_cursor:None, complete:true },
    ]);
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default());
    let service = ReviewService::with_engines(github(&remote), diff.clone(), syntax);
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let directory = remote.recovery.path().to_path_buf();
    for section in [
        ReviewSectionKind::Overview,
        ReviewSectionKind::Files,
        ReviewSectionKind::Checks,
    ] {
        service
            .read_section(
                &opened.document,
                directory.clone(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let initial = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let file_header = initial
        .snapshot
        .block
        .iter()
        .find(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("notes.txt"))
        })
        .expect("materialized review should contain the file header");
    let file_body_fold = file_header
        .metadata
        .fold
        .iter()
        .find(|fold| fold.id.0.starts_with("review:item:section:Files"))
        .expect("file header should own its diff body fold");
    assert!(file_body_fold.closed, "file entries should start collapsed");
    assert_eq!(
        file_header.text.wire_rows(),
        ["Modified notes.txt +180 -180"]
    );
    let open_file = initial
        .snapshot
        .block
        .iter()
        .find(|block| block.text.wire_rows() == ["Open working file"])
        .expect("working-file action");
    assert_eq!(file_body_fold.end.block, open_file.id);
    assert_eq!(file_body_fold.end.position.row, open_file.text.row_count());
    let view = forge_buffer::identity::ViewId("review-visual-fixture".into());
    service
        .view(&opened.document, view.clone(), Default::default())
        .await
        .unwrap();
    let status = initial
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "overview:Status")
        .unwrap();
    let lifecycle_choice = service
        .act(
            forge_buffer::input::DocumentInput {
                document: opened.document.clone(),
                revision: initial.snapshot.revision,
                view,
                sequence: forge_buffer::identity::InputSequence(1),
                action: "activate".into(),
                block: status.id.clone(),
                position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                target: Some(status.metadata.target[0].id.clone()),
            },
            directory.clone(),
        )
        .await
        .unwrap();
    assert!(lifecycle_choice.effect.is_none() && lifecycle_choice.patch.is_empty());
    assert_eq!(
        lifecycle_choice
            .choice
            .as_ref()
            .unwrap()
            .iter()
            .map(|choice| choice.value)
            .collect::<Vec<_>>(),
        ["lifecycle:draft", "lifecycle:closed"]
    );
    assert!(remote.writes.lock().unwrap().is_empty());
    let first = service
        .read_file(
            &opened.document,
            directory,
            remote.clone(),
            "notes.txt".into(),
        )
        .await
        .unwrap();
    assert!(first.more);
    let edit = RegionEdit {
        document: opened.document.clone(),
        region: RegionId("title".into()),
        base: forge_buffer::identity::RegionRevision(0),
        sequence: EditSequence(1),
        text: "Newer local title\nAdditional title row".into(),
    };
    let captured_edit = serde_json::json!({"document":edit.document,"region":edit.region,"base":edit.base,"sequence":edit.sequence,"text":edit.text});
    let accepted = service.region_edit(edit).await.unwrap();
    let mut revision = accepted.patch.as_ref().unwrap().next;
    let mut continuation = Vec::new();
    let mut more = true;
    let mut batches = 0;
    while more {
        let delivered = service
            .read_file_more(&opened.document, "notes.txt".into())
            .await
            .unwrap();
        let patch = delivered.patch.as_ref().unwrap();
        assert_eq!(patch.base, revision);
        revision = patch.next;
        patch.validate().unwrap();
        continuation.push(serde_json::to_value(patch).unwrap());
        more = delivered.more;
        batches += 1;
        assert!(batches <= 3);
    }
    assert_eq!(batches, 2);
    let materialized = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    assert!(materialized.patch.is_none());
    let rows = materialized
        .snapshot
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>();
    assert_eq!(
        rows.iter().filter(|row| row.starts_with("old ")).count(),
        180
    );
    assert_eq!(
        rows.iter().filter(|row| row.starts_with("new ")).count(),
        180
    );
    assert!(rows.iter().any(|row| *row == "Newer local title"));
    assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
    let fixture = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/review-interleave-fixture.json");
    std::fs::write(
        fixture,
        serde_json::to_vec(&serde_json::json!({
            "snapshot":initial.snapshot,"file_patch":first.patch,"edit":captured_edit,
            "acknowledgement":accepted,"continuation":continuation,
            "expected_snapshot":materialized.snapshot,
            "lifecycle_choice":lifecycle_choice,
            "arrival_order":["acknowledgement","file_patch","continuation"]
        }))
        .unwrap(),
    )
    .unwrap();
}

#[tokio::test]
async fn batched_file_sections_remain_stable_and_move_viewed_files() {
    use forge_diff::{
        cache::CacheLimits,
        engine::DiffEngine,
        syntax::{SyntaxEngine, SyntaxLimits},
    };
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;

    let remote = remote();
    remote.section_page.lock().unwrap().push_back(ReviewPage {
        records: vec![
            serde_json::json!({
                "filename": "src/alpha.rs",
                "status": "modified",
                "sha": "a".repeat(40),
                "additions": 1,
                "deletions": 1
            }),
            serde_json::json!({
                "filename": "src/beta.rs",
                "status": "added",
                "sha": "b".repeat(40),
                "additions": 2,
                "deletions": 0
            }),
        ],
        next_cursor: None,
        complete: true,
    });
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default());
    let service = ReviewService::with_engines(github(&remote), diff, syntax);
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    service
        .read_section(
            &opened.document,
            remote.recovery.path().to_path_buf(),
            remote,
            ReviewSectionKind::Files,
            None,
        )
        .await
        .unwrap();
    service.begin_batched(&opened.document).await.unwrap();

    let initial = service
        .materialize(&opened.document, Default::default())
        .await
        .unwrap();
    let initial_headings = initial
        .snapshot
        .block
        .iter()
        .filter(|block| block.id.0.ends_with(":heading"))
        .map(|block| block.text.wire_rows()[0])
        .collect::<Vec<_>>();
    assert_eq!(
        initial_headings,
        ["Unviewed Changes (2):", "Viewed Changes (0):"]
    );

    service
        .set_viewed(&opened.document, "src/beta.rs".into(), true)
        .await
        .unwrap();
    let moved = service
        .materialize(&opened.document, Default::default())
        .await
        .unwrap();
    let blocks = &moved.snapshot.block;
    let unviewed_heading = blocks
        .iter()
        .position(|block| block.id.0 == "section:Files:Unviewed:heading")
        .unwrap();
    let viewed_heading = blocks
        .iter()
        .position(|block| block.id.0 == "section:Files:Viewed:heading")
        .unwrap();
    assert_eq!(
        blocks[unviewed_heading].text.wire_rows(),
        ["Unviewed Changes (1):"]
    );
    assert_eq!(
        blocks[viewed_heading].text.wire_rows(),
        ["Viewed Changes (1):"]
    );
    assert!(unviewed_heading < viewed_heading);
    assert!(
        blocks[unviewed_heading..viewed_heading]
            .iter()
            .any(|block| block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("src/alpha.rs")))
    );
    assert!(blocks[viewed_heading..].iter().any(|block| {
        block
            .text
            .wire_rows()
            .iter()
            .any(|row| row.contains("src/beta.rs"))
    }));
    for (heading, fold_id) in [
        (unviewed_heading, "review:section:Files:Unviewed"),
        (viewed_heading, "review:section:Files:Viewed"),
    ] {
        let fold = blocks[heading]
            .metadata
            .fold
            .iter()
            .find(|fold| fold.id.0 == fold_id)
            .expect("populated viewed section should own an independent fold");
        assert!(!fold.closed);
    }
}

#[tokio::test]
async fn materialized_edits_preserve_raw_text_and_advance_projection_once() {
    use forge_buffer::identity::RegionRevision;
    use forge_buffer::width::WidthProfile;
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    let remote = remote();
    let pool = Arc::new(AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 1,
        input_bytes: 1024 * 1024,
    }));
    let service = ReviewService::with_analysis(github(&remote), pool.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let original = service
        .materialize(&opened.document, WidthProfile::default())
        .await
        .unwrap();
    assert!(original.patch.is_none());
    let title = original
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "region:title")
        .unwrap();
    assert_eq!(title.text.row(0), Some("Initial title"));
    assert_eq!(title.metadata.gutter[0].chunk[0].text, "Title:  ");
    assert_eq!(title.metadata.editable_region[0].range.start.column, 0);
    assert!(
        !original
            .snapshot
            .block
            .iter()
            .any(|block| block.id.0 == "label:title")
    );
    assert_eq!(pool.usage().admitted_jobs, 0);
    let accepted = service
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: RegionId("body".into()),
            base: RegionRevision(0),
            sequence: EditSequence(1),
            text: "Î»\r\n  body\n\n".into(),
        })
        .await
        .unwrap();
    assert_eq!(accepted.revision, RegionRevision(1));
    let patch = accepted.patch.unwrap();
    assert_eq!(patch.base, original.snapshot.revision);
    assert_eq!(patch.next.0, original.snapshot.revision.0 + 1);
    patch.validate().unwrap();
    let refreshed = service
        .materialize(&opened.document, WidthProfile::default())
        .await
        .unwrap();
    assert!(refreshed.patch.is_none());
    assert_eq!(refreshed.snapshot.revision, patch.next);
    let body = refreshed
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "region:body")
        .unwrap();
    assert_eq!(body.text.wire_rows(), vec!["Î»\r", "  body", "", ""]);
    assert!(
        service
            .region_edit(RegionEdit {
                document: opened.document.clone(),
                region: RegionId("body".into()),
                base: RegionRevision(0),
                sequence: EditSequence(2),
                text: "stale".into(),
            })
            .await
            .is_err()
    );
    assert_eq!(
        service.snapshot(&opened.document).unwrap().field[1].text,
        "Î»\r\n  body\n\n"
    );
    service.close_document(&opened.document).unwrap();
}

#[tokio::test]
async fn materialized_analysis_rejects_text_changed_while_queued() {
    use forge_buffer::width::WidthProfile;
    use forge_diff::workers::{AnalysisPool, PoolLimits, WorkBudget, WorkPriority};
    let remote = remote();
    let pool = Arc::new(AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
        input_bytes: 1024 * 1024,
    }));
    let service = ReviewService::with_analysis(github(&remote), pool.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let (release, held) = std::sync::mpsc::channel();
    let blocker = pool
        .reserve(WorkPriority::Foreground, WorkBudget::new(1, None))
        .unwrap();
    blocker.submit(move |_| {
        held.recv_timeout(Duration::from_secs(5)).unwrap();
    });
    let rendering = {
        let service = service.clone();
        let document = opened.document.clone();
        tokio::spawn(async move {
            service
                .materialize(&document, WidthProfile::default())
                .await
        })
    };
    tokio::time::timeout(Duration::from_secs(2), async {
        while pool.usage().admitted_jobs != 2 {
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    change(&service, &opened.document, "body", "newer queued text").await;
    release.send(()).unwrap();
    assert!(
        rendering
            .await
            .unwrap()
            .unwrap_err()
            .to_string()
            .contains("superseded")
    );
    let current = service
        .materialize(&opened.document, WidthProfile::default())
        .await
        .unwrap();
    let body = current
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == "region:body")
        .unwrap();
    assert_eq!(body.text.wire_rows(), vec!["newer queued text"]);
    service.close_document(&opened.document).unwrap();
}

impl forge_github::review_mutation::GithubReviewWriteRemote for TestRemote {
    fn read_review_write_result(
        &self,
        request: forge_github::review_mutation::ReviewMutationRequest,
        _: u64,
    ) -> Pin<Box<dyn Future<Output = Result<serde_json::Value, RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            if !matches!(
                request.mutation,
                forge_github::review_mutation::ReviewMutation::PullRequestEdit { .. }
            ) {
                let comment =
                    self.comment
                        .lock()
                        .unwrap()
                        .clone()
                        .ok_or_else(|| RemoteFailure {
                            kind: RemoteFailureKind::InvalidResponse,
                            message: "comment missing".into(),
                        })?;
                let expected = match &request.mutation {
                    forge_github::review_mutation::ReviewMutation::ConversationCreate { body }
                    | forge_github::review_mutation::ReviewMutation::ConversationEdit {
                        body,
                        ..
                    }
                    | forge_github::review_mutation::ReviewMutation::InlineCreate {
                        body, ..
                    }
                    | forge_github::review_mutation::ReviewMutation::ReplyCreate { body, .. }
                    | forge_github::review_mutation::ReviewMutation::InlineEdit { body, .. } => {
                        Some(body)
                    }
                    _ => None,
                };
                if expected.is_none_or(|body| body != &comment.body) {
                    return Err(RemoteFailure {
                        kind: RemoteFailureKind::InvalidResponse,
                        message: "observed comment differs from captured intent".into(),
                    });
                }
                return Ok(
                    serde_json::json!({"id":comment.database_id,"node_id":comment.id,"body":comment.body,"html_url":comment.url,"user":{"node_id":"ACTOR_test"}}),
                );
            }
            let state = self.state.lock().unwrap().clone();
            if let forge_github::review_mutation::ReviewMutation::PullRequestEdit {
                title,
                body,
                ..
            } = request.mutation
                && (title.is_some_and(|title| state.title.as_ref() != Some(&title))
                    || body.is_some_and(|body| state.body.as_ref() != Some(&body)))
            {
                return Err(RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: "observed fields do not match captured intent".into(),
                });
            }
            Ok(serde_json::to_value(state).unwrap())
        })
    }
    fn validate_review_write(
        &self,
        request: forge_github::review_mutation::ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<(), RemoteFailure>> + Send + '_>> {
        Box::pin(async move {
            if request
                .parent_node_id
                .as_ref()
                .is_some_and(|node_id| self.state.lock().unwrap().node_id != *node_id)
            {
                return Err(RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: "captured parent identity changed".into(),
                });
            }
            Ok(())
        })
    }
    fn mutate_review(
        &self,
        request: forge_github::review_mutation::ReviewMutationRequest,
    ) -> Pin<
        Box<dyn Future<Output = forge_github::review_mutation::ReviewMutationOutcome> + Send + '_>,
    > {
        Box::pin(async move {
            use forge_github::review_mutation::{ReviewMutation, ReviewMutationOutcome};
            if matches!(request.mutation, ReviewMutation::ReviewSubmit { .. }) {
                self.submission.lock().unwrap().push(request.clone());
                let failure = || RemoteFailure {
                    kind: RemoteFailureKind::Unauthorized,
                    message: "injected review submission failure".into(),
                };
                if self.reject.load(Ordering::Acquire) {
                    return ReviewMutationOutcome::Rejected(failure());
                }
                if self.uncertain.load(Ordering::Acquire) {
                    return ReviewMutationOutcome::OutcomeUnknown(failure());
                }
                return ReviewMutationOutcome::Confirmed(serde_json::json!({
                    "id": 901, "state": "COMMENTED", "body": "summary"
                }));
            }
            if let ReviewMutation::InlineCreate { body, .. }
            | ReviewMutation::ReplyCreate { body, .. } = &request.mutation
            {
                self.inline_creation
                    .lock()
                    .unwrap()
                    .push(request.mutation.clone());
                let identity = 500 + self.inline_creation.lock().unwrap().len() as u64;
                let state = CommentState {
                    kind: CommentKind::PullRequestReviewComment,
                    id: format!("INLINE_{identity}"),
                    database_id: identity,
                    body: body.clone(),
                    url: format!("https://github.com/owner/repo/pull/7#discussion_r{identity}"),
                    viewer_did_author: true,
                };
                *self.comment.lock().unwrap() = Some(state.clone());
                return ReviewMutationOutcome::Confirmed(
                    serde_json::json!({"id":state.database_id,"node_id":state.id,"body":state.body,"html_url":state.url,"user":{"node_id":"ACTOR_test"}}),
                );
            }
            let comment_operation = match &request.mutation {
                ReviewMutation::ConversationEdit { body, .. }
                | ReviewMutation::InlineEdit { body, .. } => Some(CommentOperation::Edit {
                    body: body.clone(),
                    receipt: request.operation_id.clone(),
                }),
                ReviewMutation::ConversationDelete { .. } | ReviewMutation::InlineDelete { .. } => {
                    Some(CommentOperation::Delete {
                        receipt: request.operation_id.clone(),
                    })
                }
                _ => None,
            };
            let comment_outcome = if let Some(operation) = comment_operation {
                Some(
                    self.mutate_comment(
                        CommentTarget {
                            pull_request: target(),
                            kind: CommentKind::IssueComment,
                            node_id: "COMMENT_test".into(),
                            database_id: 123,
                        },
                        operation,
                    )
                    .await,
                )
            } else if let ReviewMutation::ConversationCreate { body } = &request.mutation {
                Some(
                    self.create_conversation_comment(ConversationCommentCreation {
                        target: target(),
                        body: body.clone(),
                        receipt: request.operation_id.clone(),
                    })
                    .await,
                )
            } else {
                None
            };
            if let Some(outcome) = comment_outcome {
                return match outcome {
                    CommentMutationOutcome::Confirmed(state) => ReviewMutationOutcome::Confirmed(state.map_or(serde_json::Value::Null, |state| serde_json::json!({"id":state.database_id,"node_id":state.id,"body":state.body,"html_url":state.url,"user":{"node_id":"ACTOR_test"}}))),
                    CommentMutationOutcome::Rejected(failure) => ReviewMutationOutcome::Rejected(failure),
                    CommentMutationOutcome::Uncertain(failure) => ReviewMutationOutcome::OutcomeUnknown(failure),
                };
            }
            if let ReviewMutation::PullRequestTransition { desired, .. } = request.mutation {
                let mutation = match desired {
                    DesiredPullRequestState::Closed => PullRequestMutation::Close,
                    DesiredPullRequestState::Draft => PullRequestMutation::Draft,
                    DesiredPullRequestState::Open => PullRequestMutation::Ready,
                };
                return match self.mutate_pull_request(target(), mutation).await {
                    PullRequestMutationOutcome::Confirmed(state) => {
                        ReviewMutationOutcome::Confirmed(serde_json::to_value(state).unwrap())
                    }
                    PullRequestMutationOutcome::Rejected(failure) => {
                        ReviewMutationOutcome::Rejected(failure)
                    }
                    PullRequestMutationOutcome::Uncertain(failure) => {
                        ReviewMutationOutcome::OutcomeUnknown(failure)
                    }
                };
            }
            let ReviewMutation::PullRequestEdit { title, body, .. } = request.mutation else {
                panic!("unexpected durable review mutation")
            };
            match self
                .mutate_pull_request(
                    target(),
                    PullRequestMutation::Edit(Box::new(PullRequestEdit { title, body })),
                )
                .await
            {
                PullRequestMutationOutcome::Confirmed(state) => {
                    ReviewMutationOutcome::Confirmed(serde_json::to_value(state).unwrap())
                }
                PullRequestMutationOutcome::Rejected(failure) => {
                    ReviewMutationOutcome::Rejected(failure)
                }
                PullRequestMutationOutcome::Uncertain(failure) => {
                    ReviewMutationOutcome::OutcomeUnknown(failure)
                }
            }
        })
    }
}

fn target() -> PullRequestTarget {
    PullRequestTarget {
        repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        number: 7,
        node_id: "PR_test".into(),
    }
}

#[tokio::test]
async fn captured_workspace_rejects_foreign_read_and_input_before_remote_admission() {
    let remote = remote();
    let analysis =
        forge_diff::engine::DiffEngine::new(forge_diff::cache::CacheLimits::default(), 1)
            .analysis_pool();
    let service = ReviewService::with_analysis(github(&remote), analysis.clone());
    assert!(
        service
            .open_pr_in_directory("relative".into(), remote.clone(), target())
            .await
            .is_err()
    );
    let opened = service
        .open_pr_in_directory(remote.recovery.path().into(), remote.clone(), target())
        .await
        .unwrap();
    let foreign = remote.recovery.path().join("different-workspace");
    let failure = service
        .read_section(
            &opened.document,
            foreign.clone(),
            remote.clone(),
            forge_review::service::ReviewSectionKind::Overview,
            None,
        )
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("captured owner"));
    let failure = service
        .read_thread(
            &opened.document,
            foreign.clone(),
            remote.clone(),
            "THREAD_1".into(),
            None,
        )
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("captured owner"));
    let failure = service
        .read_file(
            &opened.document,
            foreign.clone(),
            remote.clone(),
            "src/lib.rs".into(),
        )
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("captured owner"));
    let failure = service
        .act(
            forge_buffer::input::DocumentInput {
                document: opened.document,
                revision: forge_buffer::identity::DocumentRevision(0),
                view: forge_buffer::identity::ViewId("view".into()),
                sequence: forge_buffer::identity::InputSequence(1),
                action: "activate".into(),
                block: forge_buffer::identity::BlockId("block".into()),
                position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                target: None,
            },
            foreign,
        )
        .await
        .unwrap_err();
    assert!(failure.to_string().contains("captured owner"));
    assert_eq!(analysis.usage().admitted_jobs, 0);
    assert!(remote.section_page.lock().unwrap().is_empty());
}

#[tokio::test]
async fn document_identity_cannot_alias_a_replacement_host() {
    let remote = remote();
    let first = ReviewService::new(github(&remote));
    let replacement = ReviewService::new(github(&remote));
    let original = first.open_pr(remote.clone(), target()).await.unwrap();
    let reopened = replacement.open_pr(remote, target()).await.unwrap();
    assert_ne!(original.document, reopened.document);
    assert!(replacement.save(&original.document).await.is_err());
    assert!(replacement.snapshot(&original.document).is_err());
}

#[tokio::test]
async fn lifecycle_transition_uses_a_durable_record_and_requires_a_fresh_observation() {
    use forge_github::pull_request::{DesiredPullRequestState, PullRequestLifecycle};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let transition = service
        .transition(&opened.document, DesiredPullRequestState::Closed)
        .await
        .unwrap();
    assert_eq!(
        transition.lifecycle,
        Some(forge_review::service::ReviewLifecycle {
            state: PullRequestLifecycle::Closed,
            is_draft: false,
        })
    );
    assert!(transition.recovery.is_none());
    let observed = service.lifecycle_reconcile(&opened.document).await.unwrap();
    assert_eq!(
        observed.lifecycle,
        Some(forge_review::service::ReviewLifecycle {
            state: PullRequestLifecycle::Closed,
            is_draft: false,
        })
    );
    assert!(observed.recovery.is_none());
    assert_eq!(
        remote.state.lock().unwrap().state,
        PullRequestLifecycle::Closed
    );
}

#[tokio::test]
async fn acknowledged_local_text_survives_document_and_service_replacement() {
    let remote = remote();
    let first = ReviewService::new(github(&remote));
    let original = first.open_pr(remote.clone(), target()).await.unwrap();
    first
        .region_edit(RegionEdit {
            document: original.document.clone(),
            region: RegionId("body".into()),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(100),
            text: "unsent\r\nbody\n".into(),
        })
        .await
        .unwrap();
    first
        .region_edit(RegionEdit {
            document: original.document.clone(),
            region: RegionId("title".into()),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(200),
            text: "Initial title".into(),
        })
        .await
        .unwrap();
    first.close_document(&original.document).unwrap();
    assert_eq!(
        first.shutdown(Duration::from_secs(1)).await.unfinished_jobs,
        0
    );
    let replacement = ReviewService::new(github(&remote));
    remote.state.lock().unwrap().title = Some("Fresh remote title".into());
    remote.state.lock().unwrap().body = Some("External body".into());
    let reopened = replacement.open_pr(remote.clone(), target()).await.unwrap();
    let body = reopened
        .field
        .iter()
        .find(|field| field.region.0 == "body")
        .unwrap();
    assert_eq!(body.text, "unsent\r\nbody\n");
    assert_eq!(body.baseline, "Initial body");
    assert_eq!(body.sequence, EditSequence(100));
    let title = reopened
        .field
        .iter()
        .find(|field| field.region.0 == "title")
        .unwrap();
    assert_eq!(
        (title.text.as_str(), title.sequence),
        ("Fresh remote title", EditSequence(200))
    );
    assert!(body.dirty);
    assert!(
        replacement.save(&reopened.document).await.is_err(),
        "reopened remote conflict was overwritten"
    );
    assert!(remote.writes.lock().unwrap().is_empty());
}

#[tokio::test]
async fn uncertain_pr_capture_survives_restart_and_settles_without_reposting() {
    let remote = remote();
    let first = ReviewService::new(github(&remote));
    let opened = first.open_pr(remote.clone(), target()).await.unwrap();
    first
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: RegionId("body".into()),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(100),
            text: "captured body".into(),
        })
        .await
        .unwrap();
    remote.uncertain.store(true, Ordering::Release);
    let _ = first.save(&opened.document).await;
    let snapshot = first.snapshot(&opened.document).unwrap();
    assert!(snapshot.uncertain);
    let body = snapshot
        .field
        .iter()
        .find(|field| field.region.0 == "body")
        .unwrap();
    first
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: body.region.clone(),
            base: body.revision,
            sequence: EditSequence(200),
            text: "newer body".into(),
        })
        .await
        .unwrap();
    first.close_document(&opened.document).unwrap();
    let replacement = ReviewService::new(github(&remote));
    let reopened = replacement.open_pr(remote.clone(), target()).await.unwrap();
    assert!(reopened.uncertain);
    let body = reopened
        .field
        .iter()
        .find(|field| field.region.0 == "body")
        .unwrap();
    assert_eq!(body.sequence, EditSequence(200));
    assert!(replacement.save(&reopened.document).await.is_err());
    let settled = replacement.reconcile(&reopened.document).await.unwrap();
    assert!(!settled.uncertain);
    let body = settled
        .field
        .iter()
        .find(|field| field.region.0 == "body")
        .unwrap();
    assert_eq!(
        (body.text.as_str(), body.baseline.as_str()),
        ("newer body", "captured body")
    );
    assert!(body.dirty);
    assert_eq!(body.sequence, EditSequence(200));
    replacement.close_document(&reopened.document).unwrap();
    let third = ReviewService::new(github(&remote));
    let reopened = third.open_pr(remote.clone(), target()).await.unwrap();
    assert!(!reopened.uncertain);
    assert_eq!(
        reopened
            .field
            .iter()
            .find(|field| field.region.0 == "body")
            .unwrap()
            .sequence,
        EditSequence(200)
    );
    assert_eq!(remote.writes.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn unsent_conversation_identity_and_body_survive_restart_without_remote_creation() {
    use forge_review::review::ReviewCommentCommand;
    let remote = remote();
    let first = ReviewService::new(github(&remote));
    let original = first.open_pr(remote.clone(), target()).await.unwrap();
    let draft = first
        .comment(&original.document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    first
        .region_edit(RegionEdit {
            document: original.document.clone(),
            region: draft.region.clone(),
            base: draft.revision,
            sequence: EditSequence(203),
            text: "unsent\r\ncomment\n".into(),
        })
        .await
        .unwrap();
    first.close_document(&original.document).unwrap();
    let replacement = ReviewService::new(github(&remote));
    let reopened = replacement.open_pr(remote.clone(), target()).await.unwrap();
    let restored = replacement
        .comment(
            &reopened.document,
            ReviewCommentCommand::Snapshot {
                comment: draft.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(restored.text, "unsent\r\ncomment\n");
    assert_eq!(restored.baseline, "");
    assert_eq!(restored.sequence, EditSequence(203));
    assert!(restored.dirty && !restored.saving);
    assert!(remote.creation.lock().unwrap().is_empty());
}

#[tokio::test]
async fn unknown_creation_links_after_restart_without_reposting_and_keeps_newer_text() {
    use forge_github::review_mutation::RecoveryResolution;
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let first = ReviewService::new(github(&remote));
    let opened = first.open_pr(remote.clone(), target()).await.unwrap();
    let draft = first
        .comment(&opened.document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    edit_comment(&first, &draft, "captured create").await;
    remote.uncertain.store(true, Ordering::Release);
    let uncertain = first
        .comment(
            &opened.document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    edit_comment(&first, &uncertain, "newer unsent text").await;
    first.close_document(&opened.document).unwrap();
    let replacement = ReviewService::new(github(&remote));
    let reopened = replacement.open_pr(remote.clone(), target()).await.unwrap();
    let restored = replacement
        .comment(
            &reopened.document,
            ReviewCommentCommand::Snapshot {
                comment: draft.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(restored.uncertain);
    assert_eq!(restored.sequence, EditSequence(2));
    assert!(
        replacement
            .comment(
                &reopened.document,
                ReviewCommentCommand::Save {
                    comment: draft.comment,
                    action: CommentSaveAction::Save
                }
            )
            .await
            .is_err()
    );
    let linked = replacement
        .comment(
            &reopened.document,
            ReviewCommentCommand::Recover {
                comment: draft.comment,
                resolution: Some(RecoveryResolution::Link { remote_id: 123 }),
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(
        (linked.text.as_str(), linked.baseline.as_str()),
        ("newer unsent text", "captured create")
    );
    assert!(linked.dirty && !linked.uncertain);
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
    remote.uncertain.store(false, Ordering::Release);
    replacement
        .comment(
            &reopened.document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap();
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
    assert_eq!(remote.comment_writes.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn explicit_creation_closure_keeps_captured_and_newer_text_without_remote_retry() {
    use forge_github::review_mutation::RecoveryResolution;
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let github = github(&remote);
    let service = ReviewService::new(github.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let draft = service
        .comment(&opened.document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &draft, "captured create").await;
    remote.uncertain.store(true, Ordering::Release);
    let uncertain = service
        .comment(
            &opened.document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &uncertain, "newer local text").await;
    let closed = service
        .comment(
            &opened.document,
            ReviewCommentCommand::Recover {
                comment: draft.comment,
                resolution: Some(RecoveryResolution::CloseUnknown),
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(
        (closed.text.as_str(), closed.baseline.as_str()),
        ("newer local text", "")
    );
    assert!(closed.dirty && !closed.uncertain);
    let resource = forge_github::recovery::RecoveryResource {
        repository: target().repository,
        kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
        number: 7,
    };
    assert!(
        github
            .recovery_inspect(resource.clone())
            .await
            .unwrap()
            .is_none()
    );
    let stored = github.review_draft(resource).await.unwrap().unwrap();
    let retained = stored["retired_operation"]
        .as_object()
        .unwrap()
        .values()
        .next()
        .unwrap();
    assert_eq!(
        retained["capture"]["submitted"]["mutation"]["body"],
        "captured create"
    );
    assert_eq!(retained["state"]["phase"], "user_closed_unknown");
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
}

async fn change(service: &ReviewService, document: &DocumentId, region: &str, text: &str) {
    let snapshot = service.snapshot(document).unwrap();
    let field = snapshot
        .field
        .iter()
        .find(|field| field.region.0 == region)
        .unwrap();
    service
        .region_edit(RegionEdit {
            document: document.clone(),
            region: RegionId(region.into()),
            base: field.revision,
            sequence: EditSequence(field.sequence.0 + 1),
            text: text.into(),
        })
        .await
        .unwrap();
}

async fn entered(remote: &TestRemote) {
    tokio::time::timeout(Duration::from_secs(2), remote.entered.acquire())
        .await
        .unwrap()
        .unwrap()
        .forget();
}

#[tokio::test]
async fn held_save_keeps_newer_native_edits_and_batches_dirty_fields() {
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let initial = service.open_pr(remote.clone(), target()).await.unwrap();
    assert!(
        service
            .save(&initial.document)
            .await
            .unwrap()
            .remote
            .is_none()
    );
    change(&service, &initial.document, "title", "Submitted title").await;
    change(&service, &initial.document, "body", "Î»\n  body\n").await;
    remote.hold.store(true, Ordering::Release);
    let owner = service.clone();
    let document = initial.document.clone();
    let saving = tokio::spawn(async move { owner.save(&document).await });
    entered(&remote).await;
    assert!(service.save(&initial.document).await.is_err());
    change(&service, &initial.document, "body", "newer native text").await;
    remote.release.add_permits(1);
    let result = saving.await.unwrap().unwrap();
    assert!(!result.snapshot.field[0].dirty);
    assert!(result.snapshot.field[1].dirty);
    assert_eq!(result.snapshot.field[1].text, "newer native text");
    assert_eq!(result.snapshot.field[1].baseline, "Î»\n  body\n");
    assert_eq!(remote.writes.lock().unwrap().len(), 1);
    remote.hold.store(false, Ordering::Release);
    let result = service.save(&initial.document).await.unwrap();
    assert!(result.snapshot.field.iter().all(|field| !field.dirty));
    let recorded = remote.writes.lock().unwrap();
    let PullRequestMutation::Edit(edit) = &recorded[1] else {
        panic!("expected edit")
    };
    assert!(edit.title.is_none());
    assert_eq!(edit.body.as_deref(), Some("newer native text"));
}

#[tokio::test]
async fn unknown_write_reconciles_without_reposting_or_clobbering_text() {
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let initial = service.open_pr(remote.clone(), target()).await.unwrap();
    change(&service, &initial.document, "body", "submitted").await;
    remote.uncertain.store(true, Ordering::Release);
    let result = service.save(&initial.document).await.unwrap();
    assert!(result.snapshot.uncertain);
    assert_eq!(
        result.remote.unwrap().outcome,
        PullRequestOutcome::OutcomeUnknown
    );
    change(&service, &initial.document, "body", "newer").await;
    assert!(service.save(&initial.document).await.is_err());
    let reconciled = service.reconcile(&initial.document).await.unwrap();
    assert!(!reconciled.uncertain);
    assert_eq!(reconciled.field[1].baseline, "submitted");
    assert_eq!(reconciled.field[1].text, "newer");
    assert!(reconciled.field[1].dirty);
    assert_eq!(remote.writes.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn closed_receiver_and_document_do_not_cancel_admitted_write() {
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let initial = service.open_pr(remote.clone(), target()).await.unwrap();
    change(&service, &initial.document, "body", "retained write").await;
    remote.hold.store(true, Ordering::Release);
    let owner = service.clone();
    let document = initial.document.clone();
    let saving = tokio::spawn(async move { owner.save(&document).await });
    entered(&remote).await;
    saving.abort();
    assert!(saving.await.unwrap_err().is_cancelled());
    service.close_document(&initial.document).unwrap();
    assert!(service.snapshot(&initial.document).is_err());
    let pending = service.shutdown(Duration::from_millis(10)).await;
    assert_eq!(pending.unfinished_jobs, 1);
    remote.release.add_permits(1);
    let finished = service.shutdown(Duration::from_secs(2)).await;
    assert_eq!(finished.unfinished_jobs, 0);
    assert_eq!(finished.failed_jobs, 0);
    assert_eq!(
        remote.state.lock().unwrap().body.as_deref(),
        Some("retained write")
    );
}

#[tokio::test]
async fn rejection_retains_baseline_and_invalid_title_never_writes() {
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let initial = service.open_pr(remote.clone(), target()).await.unwrap();
    change(&service, &initial.document, "title", "").await;
    assert!(service.save(&initial.document).await.is_err());
    assert!(remote.writes.lock().unwrap().is_empty());
    change(&service, &initial.document, "title", "Rejected title").await;
    remote.reject.store(true, Ordering::Release);
    let result = service.save(&initial.document).await.unwrap();
    assert_eq!(result.remote.unwrap().outcome, PullRequestOutcome::Rejected);
    assert!(result.snapshot.field[0].dirty);
    assert!(!result.snapshot.saving && !result.snapshot.uncertain);
    assert_eq!(result.snapshot.field[0].baseline, "Initial title");
}

#[tokio::test]
async fn foreign_open_does_not_adopt_text_and_document_admission_recovers() {
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    remote.state.lock().unwrap().node_id = "foreign".into();
    assert!(service.open_pr(remote.clone(), target()).await.is_err());
    remote.state.lock().unwrap().node_id = "PR_test".into();
    let mut opened = Vec::new();
    for _ in 0..64 {
        opened.push(
            service
                .open_pr(remote.clone(), target())
                .await
                .unwrap()
                .document,
        );
    }
    assert!(service.open_pr(remote.clone(), target()).await.is_err());
    service.close_document(&opened[0]).unwrap();
    let replacement = service.open_pr(remote.clone(), target()).await.unwrap();
    assert_ne!(replacement.document, opened[0]);
}

#[tokio::test]
async fn remote_panic_retains_unknown_save_for_explicit_reconciliation() {
    let remote = remote();
    let github = github(&remote);
    let service = ReviewService::new(github.clone());
    let initial = service.open_pr(remote.clone(), target()).await.unwrap();
    change(&service, &initial.document, "body", "not confirmed").await;
    remote.panic.store(true, Ordering::Release);
    assert!(service.save(&initial.document).await.is_err());
    let snapshot = service.snapshot(&initial.document).unwrap();
    assert!(snapshot.uncertain && !snapshot.saving);
    assert_eq!(snapshot.field[1].text, "not confirmed");
    assert!(service.reconcile(&initial.document).await.is_err());
    let resource = forge_github::recovery::RecoveryResource {
        repository: target().repository,
        kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
        number: 7,
    };
    let record = github
        .recovery_inspect(resource.clone())
        .await
        .unwrap()
        .unwrap();
    github
        .recovery_resolve(
            remote.clone(),
            resource,
            record.capture.operation_id,
            forge_github::review_mutation::RecoveryResolution::CloseUnknown,
        )
        .await
        .unwrap();
    assert_eq!(
        service.reconcile(&initial.document).await.unwrap().field[1].baseline,
        "Initial body"
    );
    assert_eq!(github.shutdown(Duration::from_secs(1)).await.failed_jobs, 1);
}

fn comment_state() -> CommentState {
    CommentState {
        kind: CommentKind::IssueComment,
        id: "COMMENT_test".into(),
        database_id: 123,
        body: "comment old".into(),
        url: "https://github.com/owner/repo/pull/7#issuecomment-123".into(),
        viewer_did_author: true,
    }
}

impl GithubCommentRemote for TestRemote {
    fn create_conversation_comment(
        &self,
        request: ConversationCommentCreation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(async move {
            self.creation.lock().unwrap().push(request.clone());
            self.entered.add_permits(1);
            if self.hold.load(Ordering::Acquire) {
                self.release.acquire().await.unwrap().forget();
            }
            assert!(
                !self.panic.load(Ordering::Acquire),
                "injected creation panic"
            );
            let failure = || RemoteFailure {
                kind: RemoteFailureKind::Unauthorized,
                message: "injected creation failure".into(),
            };
            if self.reject.load(Ordering::Acquire) {
                return CommentMutationOutcome::Rejected(failure());
            }
            let mut state = comment_state();
            state.kind = CommentKind::IssueComment;
            state.body = request.body;
            *self.comment.lock().unwrap() = Some(state.clone());
            if self.uncertain.load(Ordering::Acquire) {
                CommentMutationOutcome::Uncertain(failure())
            } else {
                CommentMutationOutcome::Confirmed(Some(state))
            }
        })
    }

    fn read_comment(
        &self,
        _: CommentTarget,
    ) -> Pin<Box<dyn Future<Output = Result<Option<CommentState>, RemoteFailure>> + Send + '_>>
    {
        Box::pin(async { Ok(self.comment.lock().unwrap().clone()) })
    }
    fn mutate_comment(
        &self,
        _: CommentTarget,
        operation: CommentOperation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(async move {
            self.comment_writes.lock().unwrap().push(operation.clone());
            self.entered.add_permits(1);
            if self.hold.load(Ordering::Acquire) {
                self.release.acquire().await.unwrap().forget();
            }
            assert!(
                !self.panic.load(Ordering::Acquire),
                "injected comment panic"
            );
            let failure = || RemoteFailure {
                kind: RemoteFailureKind::Unauthorized,
                message: "injected comment failure".into(),
            };
            if self.reject.load(Ordering::Acquire) {
                return CommentMutationOutcome::Rejected(failure());
            }
            let mut state = self.comment.lock().unwrap();
            match operation {
                CommentOperation::Edit { body, .. } => state.as_mut().unwrap().body = body,
                CommentOperation::Delete { .. } => *state = None,
                CommentOperation::Reconcile => panic!("reconciliation attempted a mutation"),
            }
            if self.uncertain.load(Ordering::Acquire) {
                CommentMutationOutcome::Uncertain(failure())
            } else {
                CommentMutationOutcome::Confirmed(state.clone())
            }
        })
    }
}

fn load_comment() -> forge_review::review::ReviewCommentCommand {
    forge_review::review::ReviewCommentCommand::Load {
        target: Box::new(CommentTarget {
            pull_request: target(),
            kind: CommentKind::IssueComment,
            node_id: "COMMENT_test".into(),
            database_id: 123,
        }),
        anchor: None,
    }
}

async fn edit_comment(
    service: &ReviewService,
    snapshot: &forge_review::review::ReviewCommentSnapshot,
    text: &str,
) {
    service
        .region_edit(RegionEdit {
            document: snapshot.document.clone(),
            region: snapshot.region.clone(),
            base: snapshot.revision,
            sequence: EditSequence(snapshot.sequence.0 + 1),
            text: text.into(),
        })
        .await
        .unwrap();
}

#[tokio::test]
async fn held_comment_save_advances_only_submitted_baseline_and_preserves_newer_edits() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let loaded = service
        .comment(&document, load_comment())
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &loaded, "submitted").await;
    remote.hold.store(true, Ordering::Release);
    let worker = service.clone();
    let identity = document.clone();
    let comment = loaded.comment;
    let saving = tokio::spawn(async move {
        worker
            .comment(
                &identity,
                ReviewCommentCommand::Save {
                    comment,
                    action: CommentSaveAction::Save,
                },
            )
            .await
    });
    remote.entered.acquire().await.unwrap().forget();
    let held = service
        .comment(&document, ReviewCommentCommand::Snapshot { comment })
        .await
        .unwrap()
        .snapshot;
    assert!(held.saving);
    assert_eq!(held.baseline, "comment old");
    edit_comment(&service, &held, "newer").await;
    assert!(service.save(&document).await.is_err());
    remote.release.add_permits(1);
    let settled = saving.await.unwrap().unwrap().snapshot;
    assert_eq!(
        (settled.text.as_str(), settled.baseline.as_str()),
        ("newer", "submitted")
    );
    assert!(settled.dirty && !settled.saving && !settled.uncertain);
    assert_eq!(remote.comment_writes.lock().unwrap().len(), 1);
    service.close_document(&document).unwrap();
    let replacement = ReviewService::new(github(&remote));
    let reopened = replacement.open_pr(remote.clone(), target()).await.unwrap();
    let restored = replacement
        .comment(
            &reopened.document,
            ReviewCommentCommand::Snapshot { comment },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(
        (restored.text.as_str(), restored.baseline.as_str()),
        ("newer", "submitted")
    );
    assert!(restored.dirty && !restored.uncertain);
}

#[tokio::test]
async fn uncertain_comment_save_survives_pr_reconciliation_and_does_not_repost() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let loaded = service
        .comment(&document, load_comment())
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &loaded, "submitted").await;
    remote.uncertain.store(true, Ordering::Release);
    let result = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: loaded.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap();
    assert!(result.snapshot.uncertain);
    assert_eq!(result.snapshot.baseline, "comment old");
    edit_comment(&service, &result.snapshot, "newer").await;
    assert!(service.reconcile(&document).await.unwrap().uncertain);
    assert!(service.save(&document).await.is_err());
    assert!(service.comment(&document, load_comment()).await.is_err());
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Save {
                    comment: loaded.comment,
                    action: CommentSaveAction::Save
                }
            )
            .await
            .is_err()
    );
    let settled = service
        .comment(
            &document,
            ReviewCommentCommand::Reconcile {
                comment: loaded.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(
        (settled.text.as_str(), settled.baseline.as_str()),
        ("newer", "submitted")
    );
    assert!(settled.dirty && !settled.uncertain);
    assert_eq!(remote.comment_writes.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn closed_comment_receiver_and_document_retain_native_mutation_until_settlement() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let github = github(&remote);
    let service = ReviewService::new(github.clone());
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let loaded = service
        .comment(&document, load_comment())
        .await
        .unwrap()
        .snapshot;
    remote.hold.store(true, Ordering::Release);
    let worker = service.clone();
    let identity = document.clone();
    let caller = tokio::spawn(async move {
        worker
            .comment(
                &identity,
                ReviewCommentCommand::Save {
                    comment: loaded.comment,
                    action: CommentSaveAction::Delete,
                },
            )
            .await
    });
    remote.entered.acquire().await.unwrap().forget();
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    service.close_document(&document).unwrap();
    assert_eq!(
        service
            .shutdown(Duration::from_millis(10))
            .await
            .unfinished_jobs,
        1
    );
    remote.release.add_permits(1);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    assert!(remote.comment.lock().unwrap().is_none());
    assert_eq!(
        github
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn comment_panic_retains_unknown_capture_and_foreign_load_is_rejected() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let github = github(&remote);
    let service = ReviewService::new(github.clone());
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let mut invalid = load_comment();
    if let ReviewCommentCommand::Load { target, .. } = &mut invalid {
        target.pull_request.number = 8;
    }
    assert!(service.comment(&document, invalid).await.is_err());
    let loaded = service
        .comment(&document, load_comment())
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &loaded, "submitted").await;
    remote.panic.store(true, Ordering::Release);
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Save {
                    comment: loaded.comment,
                    action: CommentSaveAction::Save
                }
            )
            .await
            .is_err()
    );
    let snapshot = service
        .comment(
            &document,
            ReviewCommentCommand::Snapshot {
                comment: loaded.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(snapshot.uncertain && snapshot.dirty && !snapshot.saving);
    assert_eq!(snapshot.baseline, "comment old");
    remote.panic.store(false, Ordering::Release);
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Reconcile {
                    comment: loaded.comment
                }
            )
            .await
            .is_err()
    );
    let resource = forge_github::recovery::RecoveryResource {
        repository: target().repository,
        kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
        number: 7,
    };
    let retained = github
        .recovery_inspect(resource.clone())
        .await
        .unwrap()
        .unwrap();
    github
        .recovery_resolve(
            remote.clone(),
            resource,
            retained.capture.operation_id,
            forge_github::review_mutation::RecoveryResolution::CloseUnknown,
        )
        .await
        .unwrap();
    let snapshot = service
        .comment(
            &document,
            ReviewCommentCommand::Reconcile {
                comment: loaded.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(snapshot.dirty && !snapshot.uncertain);
    assert_eq!(remote.comment_writes.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn comment_refresh_conflict_requires_revision_checked_resolution() {
    use forge_review::{
        comments::CommentSaveAction, edit::ConflictResolution, review::ReviewCommentCommand,
    };
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let loaded = service
        .comment(&document, load_comment())
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &loaded, "local draft").await;
    remote.comment.lock().unwrap().as_mut().unwrap().body = "remote edit".into();
    let refreshed = service
        .comment(&document, load_comment())
        .await
        .unwrap()
        .snapshot;
    assert_eq!(refreshed.comment, loaded.comment);
    assert_eq!(refreshed.text, "local draft");
    assert_eq!(refreshed.conflict.as_deref(), Some("remote edit"));
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Save {
                    comment: loaded.comment,
                    action: CommentSaveAction::Save
                }
            )
            .await
            .is_err()
    );
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Resolve {
                    comment: loaded.comment,
                    base: loaded.revision,
                    choice: ConflictResolution::KeepLocal
                }
            )
            .await
            .is_err()
    );
    let resolved = service
        .comment(
            &document,
            ReviewCommentCommand::Resolve {
                comment: loaded.comment,
                base: refreshed.revision,
                choice: ConflictResolution::KeepLocal,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(
        (resolved.text.as_str(), resolved.baseline.as_str()),
        ("local draft", "remote edit")
    );
    assert!(resolved.dirty && resolved.conflict.is_none());
    let saved = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: loaded.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(saved.baseline, "local draft");
    assert!(!saved.dirty);
}

#[tokio::test]
async fn conversation_creation_adopts_identity_and_preserves_newer_draft_text() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let draft = service
        .comment(&document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    assert_eq!(draft.baseline, "");
    edit_comment(&service, &draft, "submitted Î»\n\n").await;
    remote.hold.store(true, Ordering::Release);
    let worker = service.clone();
    let identity = document.clone();
    let comment = draft.comment;
    let saving = tokio::spawn(async move {
        worker
            .comment(
                &identity,
                ReviewCommentCommand::Save {
                    comment,
                    action: CommentSaveAction::Save,
                },
            )
            .await
    });
    entered(&remote).await;
    let held = service
        .comment(&document, ReviewCommentCommand::Snapshot { comment })
        .await
        .unwrap()
        .snapshot;
    assert!(held.saving);
    edit_comment(&service, &held, "newer").await;
    remote.release.add_permits(1);
    let saved = saving.await.unwrap().unwrap().snapshot;
    assert_eq!(saved.comment, comment);
    assert_eq!(saved.region, draft.region);
    assert_eq!(saved.text, "newer");
    assert_eq!(saved.baseline, "submitted Î»\n\n");
    assert!(saved.dirty && !saved.uncertain && !saved.saving);
    remote.hold.store(false, Ordering::Release);
    let edited = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(edited.baseline, "newer");
    assert!(!edited.dirty);
    let deleted = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment,
                action: CommentSaveAction::Delete,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(deleted.deleted);
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
    assert_eq!(remote.comment_writes.lock().unwrap().len(), 2);
}

#[tokio::test]
async fn conversation_creation_uncertainty_cannot_adopt_an_unrelated_observation() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let draft = service
        .comment(&document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &draft, "submitted").await;
    remote.uncertain.store(true, Ordering::Release);
    let saved = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(saved.uncertain && saved.dirty && !saved.saving);
    assert_eq!(saved.baseline, "");
    edit_comment(&service, &saved, "newer").await;
    assert!(service.reconcile(&document).await.unwrap().uncertain);
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Reconcile {
                    comment: draft.comment
                }
            )
            .await
            .is_err()
    );
    assert!(service.comment(&document, load_comment()).await.is_err());
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Save {
                    comment: draft.comment,
                    action: CommentSaveAction::Save
                }
            )
            .await
            .is_err()
    );
    let retained = service
        .comment(
            &document,
            ReviewCommentCommand::Snapshot {
                comment: draft.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(retained.text, "newer");
    assert!(retained.uncertain);
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn rejected_conversation_creation_keeps_draft_available_for_explicit_retry() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let draft = service
        .comment(&document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Save {
                    comment: draft.comment,
                    action: CommentSaveAction::Save
                }
            )
            .await
            .is_err()
    );
    assert!(
        service
            .comment(
                &document,
                ReviewCommentCommand::Save {
                    comment: draft.comment,
                    action: CommentSaveAction::Delete
                }
            )
            .await
            .is_err()
    );
    edit_comment(&service, &draft, "submitted").await;
    remote.reject.store(true, Ordering::Release);
    let rejected = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(rejected.dirty && !rejected.uncertain && !rejected.saving);
    assert_eq!(rejected.baseline, "");
    remote.reject.store(false, Ordering::Release);
    let saved = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(saved.baseline, "submitted");
    assert!(!saved.dirty);
    assert_eq!(remote.creation.lock().unwrap().len(), 2);
}

#[tokio::test]
async fn conversation_creation_survives_caller_and_document_closure() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let github = github(&remote);
    let service = ReviewService::new(github.clone());
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let draft = service
        .comment(&document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &draft, "retained submission").await;
    remote.hold.store(true, Ordering::Release);
    let worker = service.clone();
    let identity = document.clone();
    let caller = tokio::spawn(async move {
        worker
            .comment(
                &identity,
                ReviewCommentCommand::Save {
                    comment: draft.comment,
                    action: CommentSaveAction::Save,
                },
            )
            .await
    });
    entered(&remote).await;
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    service.close_document(&document).unwrap();
    assert_eq!(
        service
            .shutdown(Duration::from_millis(10))
            .await
            .unfinished_jobs,
        1
    );
    remote.release.add_permits(1);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    assert_eq!(
        remote.comment.lock().unwrap().as_ref().unwrap().body,
        "retained submission"
    );
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
    assert_eq!(
        github
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn conversation_creation_preflight_failure_releases_capture_without_writing() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let draft = service
        .comment(&document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &draft, "submitted").await;
    remote.state.lock().unwrap().node_id = "PR_foreign".into();
    let rejected = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap();
    assert!(matches!(
        rejected.remote,
        Some(CommentResult::Rejected { .. })
    ));
    assert!(rejected.snapshot.dirty && !rejected.snapshot.uncertain && !rejected.snapshot.saving);
    assert_eq!(remote.creation.lock().unwrap().len(), 0);
    remote.state.lock().unwrap().node_id = "PR_test".into();
    let saved = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(saved.baseline, "submitted");
    assert!(!saved.dirty);
    assert_eq!(remote.creation.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn mutation_admission_failure_preserves_unwritten_pr_comment_and_creation_drafts() {
    use forge_github::service::MutationNotStarted;
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    for closed in [false, true] {
        let remote = remote();
        let github = github(&remote);
        let service = ReviewService::new(github.clone());
        let first = service
            .open_pr(remote.clone(), target())
            .await
            .unwrap()
            .document;
        let second = service
            .open_pr(remote.clone(), target())
            .await
            .unwrap()
            .document;
        let comment = service
            .comment(&second, load_comment())
            .await
            .unwrap()
            .snapshot;
        let draft = service
            .comment(&second, ReviewCommentCommand::DraftConversation)
            .await
            .unwrap()
            .snapshot;
        edit_comment(&service, &comment, "edited comment").await;
        edit_comment(&service, &draft, "new conversation").await;
        change(&service, &second, "title", "second title").await;
        if closed {
            github.close();
        } else {
            change(&service, &first, "title", "first title").await;
            remote.uncertain.store(true, Ordering::Release);
            assert!(service.save(&first).await.unwrap().snapshot.uncertain);
        }
        let failure = service.save(&second).await.unwrap_err();
        assert!(
            failure.is::<MutationNotStarted>(),
            "closed={closed}: {failure:#}"
        );
        assert!(!service.snapshot(&second).unwrap().uncertain);
        for comment in [comment.comment, draft.comment] {
            let failure = service
                .comment(
                    &second,
                    ReviewCommentCommand::Save {
                        comment,
                        action: CommentSaveAction::Save,
                    },
                )
                .await
                .unwrap_err();
            assert!(failure.is::<MutationNotStarted>());
            let snapshot = service
                .comment(&second, ReviewCommentCommand::Snapshot { comment })
                .await
                .unwrap()
                .snapshot;
            assert!(snapshot.dirty && !snapshot.uncertain && !snapshot.saving);
        }
        assert_eq!(remote.comment_writes.lock().unwrap().len(), 0);
        assert_eq!(remote.creation.lock().unwrap().len(), 0);
        assert_eq!(remote.writes.lock().unwrap().len(), usize::from(!closed));
        if !closed {
            assert!(service.snapshot(&first).unwrap().uncertain);
            remote.uncertain.store(false, Ordering::Release);
            service.reconcile(&first).await.unwrap();
            let saved = service.save(&second).await.unwrap();
            assert!(!saved.snapshot.uncertain);
            assert_eq!(remote.writes.lock().unwrap().len(), 2);
        }
        assert_eq!(
            service
                .shutdown(Duration::from_secs(1))
                .await
                .unfinished_jobs,
            0
        );
        assert_eq!(
            github
                .shutdown(Duration::from_secs(1))
                .await
                .unfinished_jobs,
            0
        );
    }
}

#[tokio::test]
async fn inline_and_reply_creation_retain_native_anchors_and_distinct_endpoints() {
    use forge_github::review_mutation::{DiffSide, ReviewMutation};
    use forge_review::{
        comments::{CommentAnchor, CommentSaveAction, CommentSide},
        review::ReviewCommentCommand,
    };
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let anchor = CommentAnchor {
        revision: "a".repeat(40),
        path: "src/lib.rs".into(),
        side: CommentSide::Right,
        first_line: 3,
        last_line: 5,
    };
    let draft = service
        .comment(&document, ReviewCommentCommand::DraftInline { anchor })
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &draft, "inline λ\n").await;
    let saved = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: draft.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert!(!saved.dirty && !saved.uncertain);
    let reply = service
        .comment(
            &document,
            ReviewCommentCommand::DraftReply {
                parent: saved.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    let repeated = service
        .comment(
            &document,
            ReviewCommentCommand::DraftReply {
                parent: saved.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(reply.comment, repeated.comment);
    service
        .region_edit(RegionEdit {
            document: document.clone(),
            region: reply.region.clone(),
            base: reply.revision,
            sequence: EditSequence(203),
            text: "reply".into(),
        })
        .await
        .unwrap();
    service.close_document(&document).unwrap();
    let service = ReviewService::new(github(&remote));
    let document = service
        .open_pr(remote.clone(), target())
        .await
        .unwrap()
        .document;
    let restored = service
        .comment(
            &document,
            ReviewCommentCommand::DraftReply {
                parent: saved.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(restored.comment, reply.comment);
    assert_eq!(restored.sequence, EditSequence(203));
    assert_eq!(restored.text, "reply");
    let saved_reply = service
        .comment(
            &document,
            ReviewCommentCommand::Save {
                comment: reply.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_eq!(saved_reply.baseline, "reply");
    assert_eq!(saved_reply.sequence, EditSequence(203));
    service.close_document(&document).unwrap();
    let replacement = ReviewService::new(github(&remote));
    let reopened = replacement.open_pr(remote.clone(), target()).await.unwrap();
    let next_reply = replacement
        .comment(
            &reopened.document,
            ReviewCommentCommand::DraftReply {
                parent: saved.comment,
            },
        )
        .await
        .unwrap()
        .snapshot;
    assert_ne!(next_reply.comment, saved_reply.comment);
    let creation = remote.inline_creation.lock().unwrap();
    assert!(
        matches!(&creation[0], ReviewMutation::InlineCreate { body, path, line:5, start_line:Some(3), side:DiffSide::Right, start_side:Some(DiffSide::Right), commit_id } if body == "inline λ\n" && path == "src/lib.rs" && commit_id == &"a".repeat(40))
    );
    assert!(
        matches!(&creation[1], ReviewMutation::ReplyCreate { body, parent_comment_id:501 } if body == "reply")
    );
    assert!(remote.creation.lock().unwrap().is_empty());
}

#[tokio::test]
async fn materialized_comment_creation_edit_and_delete_publish_only_owned_blocks() {
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let analysis =
        forge_diff::engine::DiffEngine::new(forge_diff::cache::CacheLimits::default(), 1)
            .analysis_pool();
    let service = ReviewService::with_analysis(github(&remote), analysis);
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let materialized = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let draft = service
        .comment(&opened.document, ReviewCommentCommand::DraftConversation)
        .await
        .unwrap();
    let patch = draft
        .patch
        .as_ref()
        .expect("new comment has a projection patch");
    assert_eq!(patch.base, materialized.snapshot.revision);
    let encoded = serde_json::to_value(patch).unwrap();
    assert!(!encoded.to_string().contains("region:title"));
    assert!(!encoded.to_string().contains("region:body"));
    let refreshed = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    assert!(refreshed.patch.is_none());
    edit_comment(&service, &draft.snapshot, "created\n\n").await;
    let saved = service
        .comment(
            &opened.document,
            ReviewCommentCommand::Save {
                comment: draft.snapshot.comment,
                action: CommentSaveAction::Save,
            },
        )
        .await
        .unwrap();
    assert!(saved.patch.is_none());
    assert_eq!(saved.snapshot.baseline, "created\n\n");
    let deleted = service
        .comment(
            &opened.document,
            ReviewCommentCommand::Save {
                comment: draft.snapshot.comment,
                action: CommentSaveAction::Delete,
            },
        )
        .await
        .unwrap();
    assert!(deleted.patch.is_some());
    assert!(deleted.snapshot.deleted);
    let refreshed = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    assert!(refreshed.patch.is_none());
}

#[tokio::test]
async fn retained_thread_comment_import_uses_native_membership_and_readonly_markdown() {
    use forge_review::{
        comments::CommentSaveAction, review::ReviewCommentCommand, service::ReviewSectionKind,
    };
    let remote = remote();
    remote.section_page.lock().unwrap().push_back(forge_github::review_api::ReviewPage {
        records: vec![serde_json::json!({"id":"THREAD_1","path":"src/lib.rs","isResolved":false,"isOutdated":true,
            "viewerCanReply":true,"viewerCanResolve":false,"viewerCanUnresolve":false,
            "diffSide":"RIGHT","originalLine":5,"originalStartLine":3,
            "comments":{"totalCount":2,"pageInfo":{"hasNextPage":true,"endCursor":"comments-2"},
                "nodes":[{"id":"COMMENT_THREAD","databaseId":45,"body":"**Native body**","viewerDidAuthor":false,
                    "url":"https://github.com/owner/repo/pull/7#discussion_r45","author":{"login":"other"},
                    "originalCommit":{"oid":"a".repeat(40)},"originalLine":5,"originalStartLine":3}]}})],
        next_cursor:None, complete:true,
    });
    let analysis =
        forge_diff::engine::DiffEngine::new(forge_diff::cache::CacheLimits::default(), 1)
            .analysis_pool();
    let service = ReviewService::with_analysis(github(&remote), analysis.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let initial = service
        .read_section(
            &opened.document,
            remote.recovery.path().into(),
            remote.clone(),
            ReviewSectionKind::Threads,
            None,
        )
        .await
        .unwrap();
    let presentation = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let view = forge_buffer::identity::ViewId("review-thread-view".into());
    service
        .view(
            &opened.document,
            view.clone(),
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let browser = presentation
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0.ends_with(":browser"))
        .expect("native thread browser action");
    let input = forge_buffer::input::DocumentInput {
        document: opened.document.clone(),
        revision: presentation.snapshot.revision,
        view: view.clone(),
        sequence: forge_buffer::identity::InputSequence(1),
        action: "activate".into(),
        block: browser.id.clone(),
        position: forge_buffer::block::TextPosition { row: 0, column: 0 },
        target: Some(browser.metadata.target[0].id.clone()),
    };
    let mut invalid = input.clone();
    invalid.view = forge_buffer::identity::ViewId("unknown-view".into());
    assert!(
        service
            .act(invalid, remote.recovery.path().into())
            .await
            .is_err()
    );
    let mut invalid = input.clone();
    invalid.position.column = 999;
    assert!(
        service
            .act(invalid, remote.recovery.path().into())
            .await
            .is_err()
    );
    let acted = service
        .act(input.clone(), remote.recovery.path().into())
        .await
        .unwrap();
    assert!(acted.patch.is_empty());
    assert_eq!(
        acted.effect.unwrap().url.as_deref(),
        Some("https://github.com/owner/repo/pull/7#discussion_r45")
    );
    assert!(
        service
            .act(input, remote.recovery.path().into())
            .await
            .is_err()
    );
    service.close_view(&opened.document, &view).await.unwrap();
    let loaded = service
        .comment(
            &opened.document,
            ReviewCommentCommand::LoadThreadComment {
                thread_node_id: "THREAD_1".into(),
                comment_node_id: "COMMENT_THREAD".into(),
            },
        )
        .await
        .unwrap();
    assert_eq!(loaded.snapshot.text, "**Native body**");
    assert!(!loaded.snapshot.viewer_did_author);
    assert!(loaded.patch.is_some());
    let rendered = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    assert!(rendered.patch.is_none());
    let block = rendered
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0 == format!("region:{}", loaded.snapshot.region.0))
        .unwrap();
    assert_eq!(block.text.wire_rows(), ["Native body"]);
    assert!(block.metadata.editable_region.is_empty());
    assert!(
        service
            .comment(
                &opened.document,
                ReviewCommentCommand::Save {
                    comment: loaded.snapshot.comment,
                    action: CommentSaveAction::Save,
                }
            )
            .await
            .is_err()
    );
    assert!(
        service
            .comment(
                &opened.document,
                ReviewCommentCommand::LoadThreadComment {
                    thread_node_id: "THREAD_1".into(),
                    comment_node_id: "FOREIGN".into(),
                }
            )
            .await
            .is_err()
    );
    let reply = service
        .comment(
            &opened.document,
            ReviewCommentCommand::DraftReply {
                parent: loaded.snapshot.comment,
            },
        )
        .await
        .unwrap();
    assert!(reply.snapshot.viewer_did_author && reply.patch.is_some());
    let next_comment = |identity, database_id| {
        serde_json::json!({
            "id":identity,"databaseId":database_id,"body":"second body","viewerDidAuthor":true,
            "url":format!("https://github.com/owner/repo/pull/7#discussion_r{database_id}"),
            "author":{"login":"viewer"},"originalCommit":{"oid":"a".repeat(40)},"originalLine":5,
        })
    };
    remote.section_page.lock().unwrap().extend([
        forge_github::review_api::ReviewPage {
            records: vec![next_comment("COMMENT_THREAD", 45)],
            next_cursor: None,
            complete: true,
        },
        forge_github::review_api::ReviewPage {
            records: vec![next_comment("COMMENT_SECOND", 46)],
            next_cursor: None,
            complete: true,
        },
    ]);
    assert!(
        service
            .read_thread(
                &opened.document,
                remote.recovery.path().into(),
                remote.clone(),
                "THREAD_1".into(),
                Some("wrong".into())
            )
            .await
            .unwrap_err()
            .to_string()
            .contains("stale")
    );
    assert!(
        service
            .read_thread(
                &opened.document,
                remote.recovery.path().into(),
                remote.clone(),
                "THREAD_1".into(),
                Some("comments-2".into())
            )
            .await
            .unwrap_err()
            .to_string()
            .contains("duplicate")
    );
    let local = service
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: RegionId("title".into()),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(1),
            text: "Local title\ncontinued".into(),
        })
        .await
        .unwrap();
    let completed = service
        .read_thread(
            &opened.document,
            remote.recovery.path().into(),
            remote.clone(),
            "THREAD_1".into(),
            Some("comments-2".into()),
        )
        .await
        .unwrap();
    assert_eq!(completed.thread.comment.len(), 2);
    let serialized = serde_json::to_value(&completed).unwrap();
    assert!(serialized["thread"].get("comment").is_none());
    assert_eq!(serialized["thread"]["loaded_comments"], 2);
    let continuation_patch = completed
        .patch
        .as_ref()
        .expect("native thread continuation patch");
    assert_eq!(continuation_patch.base, local.patch.unwrap().next);
    assert_eq!(continuation_patch.removed_block.len(), 1);
    assert!(continuation_patch.removed_block[0].0.ends_with(":more"));
    assert!(continuation_patch.metadata_edit.iter().all(|edit| {
        !presentation
            .snapshot
            .block
            .iter()
            .any(|block| block.id == edit.block)
    }));
    assert!(completed.thread.next_cursor.is_none());
    assert!(Arc::ptr_eq(
        &initial.item[0].thread.as_ref().unwrap().comment[0],
        &completed.thread.comment[0]
    ));
    let second = service
        .comment(
            &opened.document,
            ReviewCommentCommand::LoadThreadComment {
                thread_node_id: "THREAD_1".into(),
                comment_node_id: "COMMENT_SECOND".into(),
            },
        )
        .await
        .unwrap();
    assert_eq!(second.snapshot.text, "second body");
    assert!(second.snapshot.viewer_did_author);
    assert_eq!(analysis.usage().admitted_jobs, 0);
}

#[tokio::test]
async fn immutable_file_analysis_uses_merge_base_and_fork_head_sources() {
    use forge_diff::{
        cache::CacheLimits,
        engine::DiffEngine,
        syntax::{SyntaxEngine, SyntaxLimits},
    };
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;
    let remote = remote();
    remote.section_page.lock().unwrap().extend([
    ReviewPage { records: vec![serde_json::json!({"number":7,"node_id":"PR_test","title":"Title",
        "base":{"sha":"a".repeat(40)},"head":{"sha":"b".repeat(40),"repo":{"full_name":"fork/project"}}})], next_cursor: None, complete:true },
    ReviewPage { records: vec![serde_json::json!({"filename":"src/new.rs","previous_filename":"src/old.rs","status":"renamed","sha":"e".repeat(40)})], next_cursor:None, complete:true },
]);
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default());
    let service = ReviewService::with_engines(github(&remote), diff.clone(), syntax.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let directory = remote.recovery.path().to_path_buf();
    for section in [ReviewSectionKind::Overview, ReviewSectionKind::Files] {
        service
            .read_section(
                &opened.document,
                directory.clone(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let projection = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    let file = service
        .read_file(
            &opened.document,
            directory,
            remote.clone(),
            "src/new.rs".into(),
        )
        .await
        .unwrap();
    assert_eq!(
        file.patch.as_ref().unwrap().base,
        projection.snapshot.revision
    );
    assert_eq!(file.merge_base, "c".repeat(40));
    assert_eq!(file.old_blob.as_deref(), Some("d".repeat(40).as_str()));
    assert_eq!(file.new_blob.as_deref(), Some("e".repeat(40).as_str()));
    assert!(
        file.syntax_diagnostic.is_none(),
        "{:?}",
        file.syntax_diagnostic
    );
    let request = remote.source_request.lock().unwrap();
    assert_eq!(request.len(), 2);
    assert_eq!(request[0].commit, "c".repeat(40));
    assert_eq!(request[0].path, "src/old.rs");
    assert_eq!(request[1].repository.repository_name(), "fork/project");
    assert_eq!(request[1].commit, "b".repeat(40));
    assert_eq!(diff.usage().active_jobs, 0);
    assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
    assert_eq!(syntax.usage().active_jobs, 0);
    drop(request);
    let rendered = service
        .materialize(
            &opened.document,
            forge_buffer::width::WidthProfile::default(),
        )
        .await
        .unwrap();
    assert!(rendered.patch.is_none());
    assert!(
        rendered
            .snapshot
            .block
            .iter()
            .any(|block| block.text.wire_rows() == vec!["fn old() {}"])
    );
    assert!(
        rendered
            .snapshot
            .block
            .iter()
            .any(|block| block.text.wire_rows() == vec!["fn new() {}"])
    );
}

#[tokio::test]
async fn batched_submission_captures_summary_and_inline_draft_in_one_mutation() {
    use forge_github::review_mutation::{ReviewEvent, ReviewMutation};
    use forge_review::{
        comments::{CommentAnchor, CommentSide},
        review::{ReviewCommentCommand, ReviewVerdict},
    };
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    service.begin_batched(&opened.document).await.unwrap();
    service
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: RegionId("review_summary".into()),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(1),
            text: "Summary".into(),
        })
        .await
        .unwrap();
    let draft = service
        .comment(
            &opened.document,
            ReviewCommentCommand::DraftInline {
                anchor: CommentAnchor {
                    revision: "b".repeat(40),
                    path: "src/lib.rs".into(),
                    side: CommentSide::Right,
                    first_line: 4,
                    last_line: 4,
                },
            },
        )
        .await
        .unwrap()
        .snapshot;
    service
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: draft.region.clone(),
            base: draft.revision,
            sequence: EditSequence(2),
            text: "Inline finding".into(),
        })
        .await
        .unwrap();
    let delivered = service
        .submit_batched(&opened.document, ReviewVerdict::Comment)
        .await
        .unwrap();
    assert_eq!(delivered.outcome, "confirmed");
    assert!(delivered.operation_id.is_none());
    let submitted = remote.submission.lock().unwrap();
    assert_eq!(submitted.len(), 1);
    let ReviewMutation::ReviewSubmit {
        body,
        event,
        comment,
        ..
    } = &submitted[0].mutation
    else {
        panic!("missing review submission");
    };
    assert_eq!(body, "Summary");
    assert!(matches!(event, ReviewEvent::Comment));
    assert_eq!(comment.len(), 1);
    assert_eq!(comment[0].body, "Inline finding");
    assert_eq!(comment[0].path, "src/lib.rs");
}

#[tokio::test]
async fn unknown_batched_submission_blocks_repost_and_explicit_rejection_settles_once() {
    use forge_github::review_mutation::RecoveryResolution;
    use forge_review::{
        comments::{CommentAnchor, CommentSide},
        review::{ReviewCommentCommand, ReviewVerdict},
    };
    let remote = remote();
    remote.uncertain.store(true, Ordering::Release);
    let service = ReviewService::new(github(&remote));
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    service.begin_batched(&opened.document).await.unwrap();
    let draft = service
        .comment(
            &opened.document,
            ReviewCommentCommand::DraftInline {
                anchor: CommentAnchor {
                    revision: "b".repeat(40),
                    path: "src/lib.rs".into(),
                    side: CommentSide::Right,
                    first_line: 4,
                    last_line: 4,
                },
            },
        )
        .await
        .unwrap()
        .snapshot;
    service
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: draft.region,
            base: draft.revision,
            sequence: EditSequence(1),
            text: "Uncertain finding".into(),
        })
        .await
        .unwrap();
    let unknown = service
        .submit_batched(&opened.document, ReviewVerdict::Comment)
        .await
        .unwrap();
    assert_eq!(unknown.outcome, "outcome_unknown");
    let operation = unknown.operation_id.clone().unwrap();
    assert!(
        service
            .submit_batched(&opened.document, ReviewVerdict::Comment)
            .await
            .is_err()
    );
    assert_eq!(remote.submission.lock().unwrap().len(), 1);
    assert!(
        service
            .recover_batched_submission(
                &opened.document,
                "other-operation".into(),
                RecoveryResolution::NotDispatched
            )
            .await
            .is_err()
    );
    let journal = recovery_journal_path(remote.recovery.path(), &operation);
    std::fs::remove_file(journal).unwrap();
    let settled = service
        .recover_batched_submission(
            &opened.document,
            operation,
            RecoveryResolution::NotDispatched,
        )
        .await
        .unwrap();
    assert_eq!(settled.submission.outcome, "rejected");
    assert!(!settled.fresh_required);
    assert_eq!(remote.submission.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn mismatched_batched_recovery_capture_is_rejected() {
    use forge_github::review_mutation::RecoveryResolution;
    use forge_review::{
        comments::{CommentAnchor, CommentSide},
        review::{ReviewCommentCommand, ReviewVerdict},
    };
    let remote = remote();
    remote.uncertain.store(true, Ordering::Release);
    let service = ReviewService::new(github(&remote));
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    service.begin_batched(&opened.document).await.unwrap();
    let draft = service
        .comment(
            &opened.document,
            ReviewCommentCommand::DraftInline {
                anchor: CommentAnchor {
                    revision: "b".repeat(40),
                    path: "src/lib.rs".into(),
                    side: CommentSide::Right,
                    first_line: 4,
                    last_line: 4,
                },
            },
        )
        .await
        .unwrap()
        .snapshot;
    service
        .region_edit(RegionEdit {
            document: opened.document.clone(),
            region: draft.region,
            base: draft.revision,
            sequence: EditSequence(1),
            text: "Captured finding".into(),
        })
        .await
        .unwrap();
    let unknown = service
        .submit_batched(&opened.document, ReviewVerdict::Comment)
        .await
        .unwrap();
    let operation = unknown.operation_id.unwrap();
    let journal = recovery_journal_path(remote.recovery.path(), &operation);
    let mut record: serde_json::Value =
        serde_json::from_slice(&std::fs::read(&journal).unwrap()).unwrap();
    record["capture"]["submitted"]["mutation"]["body"] = serde_json::json!("tampered");
    std::fs::write(&journal, serde_json::to_vec(&record).unwrap()).unwrap();
    assert!(
        service
            .recover_batched_submission(
                &opened.document,
                operation,
                RecoveryResolution::CloseUnknown,
            )
            .await
            .is_err()
    );
    assert_eq!(remote.submission.lock().unwrap().len(), 1);
}

#[tokio::test]
async fn native_file_targets_open_the_retained_diff_workspace_file_and_browser_url() {
    use forge_buffer::{
        block::TextPosition,
        identity::{InputSequence, ViewId},
        input::DocumentInput,
        width::WidthProfile,
    };
    use forge_diff::{
        cache::CacheLimits,
        engine::DiffEngine,
        syntax::{SyntaxEngine, SyntaxLimits},
    };
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;

    let remote = remote();
    remote.section_page.lock().unwrap().extend([
        ReviewPage {
            records: vec![serde_json::json!({
                "number":7,"node_id":"PR_test","title":"Title",
                "base":{"sha":"a".repeat(40)},
                "head":{"sha":"b".repeat(40),"repo":{"full_name":"fork/project"}}
            })],
            next_cursor: None,
            complete: true,
        },
        ReviewPage {
            records: vec![serde_json::json!({
                "filename":"src/lib.rs","status":"modified","sha":"e".repeat(40),
                "html_url":"https://github.example.test/owner/repository/pull/7/files#diff"
            })],
            next_cursor: None,
            complete: true,
        },
    ]);
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool(), SyntaxLimits::default());
    let service = ReviewService::with_engines(github(&remote), diff, syntax);
    let directory = remote.recovery.path().to_path_buf();
    let opened = service
        .open_pr_in_directory(directory.clone(), remote.clone(), target())
        .await
        .unwrap();
    for section in [ReviewSectionKind::Overview, ReviewSectionKind::Files] {
        service
            .read_section(
                &opened.document,
                directory.clone(),
                remote.clone(),
                section,
                None,
            )
            .await
            .unwrap();
    }
    let presentation = service
        .materialize(&opened.document, WidthProfile::default())
        .await
        .unwrap();
    assert!(presentation.snapshot.block.iter().any(|block| {
        block
            .metadata
            .fold
            .iter()
            .any(|fold| fold.id.0.starts_with("review:section:"))
    }));
    assert!(presentation.snapshot.block.iter().any(|block| {
        block
            .metadata
            .fold
            .iter()
            .any(|fold| fold.id.0.starts_with("review:item:"))
    }));
    let view = ViewId("review-file-navigation".into());
    service
        .view(&opened.document, view.clone(), WidthProfile::default())
        .await
        .unwrap();
    let activate = |suffix: &str, sequence| {
        let block = presentation
            .snapshot
            .block
            .iter()
            .find(|block| block.id.0.ends_with(suffix) && !block.metadata.target.is_empty())
            .unwrap_or_else(|| panic!("missing native review target {suffix}"));
        DocumentInput {
            document: opened.document.clone(),
            revision: presentation.snapshot.revision,
            view: view.clone(),
            sequence: InputSequence(sequence),
            action: "activate".into(),
            block: block.id.clone(),
            position: TextPosition { row: 0, column: 0 },
            target: Some(block.metadata.target[0].id.clone()),
        }
    };
    let mut expansion = activate(":open-file", 1);
    expansion.action = "expand".into();
    let expansion = service.act(expansion, directory.clone()).await.unwrap();
    assert!(expansion.effect.is_none() && expansion.patch.is_empty());
    let working = service
        .act(activate(":open-file", 2), directory.clone())
        .await
        .unwrap();
    let working = working.effect.expect("native working-file effect");
    assert_eq!(working.kind, "open_file");
    assert_eq!(
        working.path.as_deref(),
        Some(directory.join("src/lib.rs").to_string_lossy().as_ref())
    );
    let browser = service
        .act(activate(":browser", 3), directory.clone())
        .await
        .unwrap();
    let browser = browser.effect.expect("native browser effect");
    assert_eq!(browser.kind, "browser");
    assert_eq!(
        browser.url.as_deref(),
        Some("https://github.example.test/owner/repository/pull/7/files#diff")
    );
    let mut expansion = activate(":title", 4);
    expansion.action = "expand".into();
    let diff = service.act(expansion, directory).await.unwrap();
    assert!(diff.effect.is_none());
    assert!(
        diff.patch.len() == 1,
        "native file action did not publish a diff patch"
    );
    let file_patch = &diff.patch[0];
    let inline = file_patch
        .metadata_edit
        .iter()
        .find_map(|edit| {
            edit.metadata
                .target
                .iter()
                .find(|target| target.id.0.contains(":inline:"))
                .map(|target| (edit.block.clone(), target.id.clone()))
        })
        .expect("changed native diff row has an inline comment target");
    let drafted = service
        .act(
            DocumentInput {
                document: opened.document.clone(),
                revision: file_patch.next,
                view,
                sequence: InputSequence(5),
                action: "activate".into(),
                block: inline.0,
                position: TextPosition { row: 0, column: 0 },
                target: Some(inline.1),
            },
            remote.recovery.path().into(),
        )
        .await
        .unwrap();
    let drafted = drafted.comment.expect("inline draft snapshot");
    assert!(drafted.viewer_did_author && drafted.text.is_empty() && !drafted.dirty);
    let batched = service.begin_batched(&opened.document).await.unwrap();
    assert_eq!(batched.mode, forge_review::review::ReviewMode::Batched);
    assert_eq!(
        batched
            .snapshot
            .summary
            .as_ref()
            .map(|field| field.text.as_str()),
        Some("")
    );
    let viewed = service
        .set_viewed(&opened.document, "src/lib.rs".into(), true)
        .await
        .unwrap();
    assert_eq!(viewed.viewed_file, ["src/lib.rs"]);
}

#[tokio::test]
async fn repository_pr_discovery_validates_summary_before_adopting_canonical_fields() {
    let remote = remote();
    remote.section_page.lock().unwrap().extend(
        [(8, "PR_test"), (7, "PR_foreign"), (7, "PR_test")]
            .into_iter()
            .map(|(number, node_id)| forge_github::review_api::ReviewPage {
                records: vec![serde_json::json!({"number":number,"node_id":node_id})],
                next_cursor: None,
                complete: true,
            }),
    );
    let service = ReviewService::new(github(&remote));
    assert!(
        service
            .open_repository_pr(
                remote.recovery.path().into(),
                remote.clone(),
                target().repository,
                7
            )
            .await
            .unwrap_err()
            .to_string()
            .contains("another number")
    );
    assert!(
        service
            .open_repository_pr(
                remote.recovery.path().into(),
                remote.clone(),
                target().repository,
                7
            )
            .await
            .is_err()
    );
    let opened = service
        .open_repository_pr(
            remote.recovery.path().into(),
            remote.clone(),
            target().repository,
            7,
        )
        .await
        .unwrap();
    assert_eq!(
        opened
            .field
            .iter()
            .find(|field| field.region.0 == "body")
            .unwrap()
            .text,
        remote.state.lock().unwrap().body.as_deref().unwrap()
    );
    assert!(remote.writes.lock().unwrap().is_empty());
}
