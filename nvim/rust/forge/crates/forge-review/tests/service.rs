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
    commit_detail: Mutex<Option<forge_github::review_source::ReviewCommitDetail>>,
    immutable_content: Mutex<std::collections::BTreeMap<(String, String), Vec<u8>>>,
    section_barrier: Mutex<Option<Arc<tokio::sync::Barrier>>>,
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
    reviewer_writes: Mutex<Vec<forge_github::review_mutation::ReviewMutation>>,
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
        commit_detail: Mutex::new(None),
        immutable_content: Mutex::new(Default::default()),
        section_barrier: Mutex::new(None),
        source_content: Mutex::new(None),
        source_request: Mutex::new(Vec::new()),
        section_page: Mutex::new(std::collections::VecDeque::new()),
        recovery: tempfile::tempdir().unwrap(),
        comment: Mutex::new(Some(comment_state())),
        comment_writes: Mutex::new(Vec::new()),
        creation: Mutex::new(Vec::new()),
        inline_creation: Mutex::new(Vec::new()),
        submission: Mutex::new(Vec::new()),
        reviewer_writes: Mutex::new(Vec::new()),
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
    fn read_commit_detail(
        &self,
        request: forge_github::review_source::ReviewCommitDetailRequest,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = Result<forge_github::review_source::ReviewCommitDetail, RemoteFailure>,
                > + Send
                + '_,
        >,
    > {
        Box::pin(async move {
            let detail = self
                .commit_detail
                .lock()
                .unwrap()
                .clone()
                .expect("unexpected commit detail read");
            assert_eq!(detail.commit, request.commit);
            Ok(detail)
        })
    }
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
                bytes: if let Some(bytes) = self
                    .immutable_content
                    .lock()
                    .unwrap()
                    .get(&(request.commit.clone(), request.path.clone()))
                {
                    bytes.clone()
                } else {
                    self.source_content
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
                        })
                },
                request,
                blob: Some(if old { "d".repeat(40) } else { "e".repeat(40) }),
            })
        })
    }
    fn read_review(
        &self,
        request: forge_github::review_api::ReviewReadRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<forge_github::review_api::ReviewPage, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(async move {
            let barrier = self.section_barrier.lock().unwrap().clone();
            if let Some(barrier) = barrier {
                barrier.wait().await;
            }
            if matches!(
                request.view,
                forge_github::review_api::ReviewSection::EditableFields
            ) {
                return Ok(forge_github::review_api::ReviewPage {
                    records: vec![
                        serde_json::to_value(self.state.lock().unwrap().clone()).unwrap(),
                    ],
                    next_cursor: None,
                    complete: true,
                });
            }
            let mut pages = self.section_page.lock().unwrap();
            let index = pages
                .iter()
                .position(|page| {
                    page.records
                        .first()
                        .is_some_and(|record| match request.view {
                            forge_github::review_api::ReviewSection::Overview => {
                                record.get("number").is_some()
                            }
                            forge_github::review_api::ReviewSection::RequestedReviewers => {
                                record.get("users").is_some()
                            }
                            _ => false,
                        })
                })
                .unwrap_or(0);
            pages.remove(index).ok_or(RemoteFailure {
                kind: RemoteFailureKind::InvalidResponse,
                message: "no queued section page".into(),
            })
        })
    }
}

fn github(remote: &TestRemote) -> GithubService {
    let service = GithubService::default();
    service
        .configure_recovery(remote.recovery.path().join("forge/recovery/github/v1"))
        .unwrap();
    service
}

#[tokio::test]
async fn file_diff_controls_share_visibility_and_preserve_lazy_source_actions() {
    use forge_buffer::{
        block::TextPosition,
        identity::{InputSequence, ViewId},
        input::DocumentInput,
    };
    use forge_diff::{
        cache::CacheLimits,
        engine::DiffEngine,
        syntax::SyntaxEngine,
        view::{DiffPresentation, DiffVisibility},
    };
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;
    let remote = remote();
    *remote.source_content.lock().unwrap() = Some((
        b"pub fn visible() {\n    previous();\n}\nfn hidden() {\n    old_private();\n}\n".to_vec(),
        b"pub fn visible() {\n    updated();\n}\nfn hidden() {\n    new_private();\n}\n".to_vec(),
    ));
    remote.section_page.lock().unwrap().extend([
        ReviewPage { records: vec![serde_json::json!({"number":7,"node_id":"PR_test","title":"Initial title", "state":"open", "html_url":"https://example.invalid/owner/repo/pull/7",
            "base":{"sha":"a".repeat(40)},"head":{"sha":"b".repeat(40),"repo":{"full_name":"fork/project"}}})], next_cursor: None, complete:true },
        ReviewPage { records: vec![serde_json::json!({"filename":"source.rs","status":"modified","sha":"e".repeat(40),"additions":2,"deletions":2})], next_cursor:None, complete:true },
    ]);
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool());
    let service = ReviewService::with_engines(github(&remote), diff, syntax);
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
    service
        .materialize(&opened.document, Default::default())
        .await
        .unwrap();
    service
        .read_file(
            &opened.document,
            directory.clone(),
            remote.clone(),
            "source.rs".into(),
        )
        .await
        .unwrap();
    let view = ViewId("semantic-file-controls".into());
    service
        .view(&opened.document, view.clone(), Default::default())
        .await
        .unwrap();
    let mut frame = service
        .materialize(&opened.document, Default::default())
        .await
        .unwrap();
    assert_eq!(frame.diff_view.visibility, DiffVisibility::All);
    assert_eq!(frame.diff_view.presentation, DiffPresentation::Full);
    assert!(
        frame
            .snapshot
            .block
            .iter()
            .any(|block| block.text.row(0) == Some("    new_private();"))
    );
    let initial = serde_json::to_value(&frame).unwrap();
    let mut transition = Vec::new();
    let acquired = remote.source_request.lock().unwrap().len();
    for (sequence, action) in [
        "toggle_presentation",
        "source_expand",
        "toggle_visibility",
        "toggle_presentation",
        "toggle_visibility",
    ]
    .into_iter()
    .enumerate()
    {
        let selected = if action == "source_expand" {
            frame
                .snapshot
                .block
                .iter()
                .find(|block| block.text.row(0) == Some("pub fn visible() {"))
                .unwrap()
        } else {
            &frame.snapshot.block[0]
        };
        let delivery = service
            .act(
                DocumentInput {
                    document: opened.document.clone(),
                    revision: frame.snapshot.revision,
                    view: view.clone(),
                    sequence: InputSequence(sequence as u64 + 1),
                    action: action.into(),
                    block: selected.id.clone(),
                    position: TextPosition { row: 0, column: 0 },
                    target: None,
                },
                directory.clone(),
            )
            .await
            .unwrap();
        for patch in &delivery.patch {
            patch.validate().unwrap();
        }
        frame = service
            .materialize(&opened.document, Default::default())
            .await
            .unwrap();
        transition.push(serde_json::json!({"action":action,"delivery":delivery,"expected":frame}));
        let contains = |text: &str| {
            frame
                .snapshot
                .block
                .iter()
                .any(|block| block.text.wire_rows().iter().any(|row| row.contains(text)))
        };
        match sequence {
            0 => {
                assert_eq!(frame.diff_view.presentation, DiffPresentation::Semantic);
                assert!(!contains("updated()") && !contains("new_private()"));
                let heading = frame
                    .snapshot
                    .block
                    .iter()
                    .find(|block| block.text.row(0) == Some("pub fn visible() {"))
                    .unwrap();
                assert_eq!(
                    heading.metadata.fold[0].collapsed_suffix.as_deref(),
                    Some(" … 1 change }")
                );
                assert!(
                    heading
                        .metadata
                        .gutter
                        .iter()
                        .any(|gutter| gutter.chunk.iter().any(|chunk| chunk.text.contains('~')))
                );
            }
            1 => {
                assert!(contains("updated()") && !contains("new_private()"));
                let added = frame
                    .snapshot
                    .block
                    .iter()
                    .find(|block| block.text.row(0) == Some("    updated();"))
                    .unwrap();
                assert!(!added.metadata.target.is_empty());
            }
            2 => {
                assert_eq!(frame.diff_view.visibility, DiffVisibility::Public);
                assert!(contains("updated()") && !contains("hidden"));
            }
            3 => {
                assert_eq!(frame.diff_view.presentation, DiffPresentation::Full);
                assert_eq!(frame.diff_view.visibility, DiffVisibility::Public);
                assert!(contains("updated()") && !contains("new_private()"));
            }
            4 => assert!(contains("updated()") && contains("new_private()")),
            _ => unreachable!(),
        }
    }
    assert_eq!(remote.source_request.lock().unwrap().len(), acquired);
    assert!(remote.writes.lock().unwrap().is_empty());
    assert!(remote.inline_creation.lock().unwrap().is_empty());
    forge_recorded_fixtures::assert_matches_recording(
        "review-semantic.json",
        &serde_json::json!({"initial":initial,"transition":transition}),
    );
}

#[tokio::test]
async fn immutable_file_continuation_publishes_only_new_rows_after_an_edit() {
    use forge_diff::{cache::CacheLimits, engine::DiffEngine, syntax::SyntaxEngine};
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
    let syntax = SyntaxEngine::new(diff.analysis_pool());
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
    let deferred = initial
        .snapshot
        .block
        .iter()
        .find(|block| block.id.0.ends_with(":deferred"))
        .expect("deferred diff body");
    assert_eq!(file_body_fold.end.block, deferred.id);
    assert_eq!(file_body_fold.end.position.row, deferred.text.row_count());
    assert!(
        !initial
            .snapshot
            .block
            .iter()
            .any(|block| block.text.wire_rows() == ["Open working file"])
    );
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
                view: view.clone(),
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
            directory.clone(),
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
    let accepted = service.region_edit(edit).await.unwrap();
    let mut revision = accepted.patch.as_ref().unwrap().next;
    let mut continuation = Vec::new();
    let mut more = true;
    let mut batches = 0;
    while more {
        let frame = service
            .materialize(&opened.document, Default::default())
            .await
            .unwrap();
        assert!(frame.patch.is_none());
        let selected = frame
            .snapshot
            .block
            .iter()
            .find(|block| block.id.0.ends_with(":more"))
            .unwrap();
        let delivered = service
            .act(
                forge_buffer::input::DocumentInput {
                    document: opened.document.clone(),
                    revision: frame.snapshot.revision,
                    view: view.clone(),
                    sequence: forge_buffer::identity::InputSequence(batches + 2),
                    action: "activate".into(),
                    block: selected.id.clone(),
                    position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                    target: Some(selected.metadata.target[0].id.clone()),
                },
                directory.clone(),
            )
            .await
            .unwrap();
        assert_eq!(delivered.patch.len(), 1);
        let patch = &delivered.patch[0];
        assert_eq!(patch.base, revision);
        revision = patch.next;
        patch.validate().unwrap();
        continuation.push(serde_json::to_value(patch).unwrap());
        let frame = service
            .materialize(&opened.document, Default::default())
            .await
            .unwrap();
        more = frame
            .snapshot
            .block
            .iter()
            .any(|block| block.id.0.ends_with(":more"));
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
    assert!(rows.contains(&"Newer local title"));
    assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
}

#[tokio::test]
async fn materialized_analysis_rejects_text_changed_while_queued() {
    use forge_buffer::width::WidthProfile;
    use forge_diff::workers::{AnalysisPool, PoolLimits, WorkBudget, WorkPriority};
    let remote = remote();
    let pool = Arc::new(AnalysisPool::new(PoolLimits {
        workers: 1,
        jobs: 2,
    }));
    let service = ReviewService::with_analysis(github(&remote), pool.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let (release, held) = std::sync::mpsc::channel();
    let blocker = pool
        .reserve(WorkPriority::Foreground, WorkBudget::new(1, None))
        .unwrap();
    blocker.submit(move |_| {
        held.recv().unwrap();
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
    async {
        while pool.usage().admitted_jobs != 2 {
            tokio::task::yield_now().await;
        }
    }
    .await;
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
            if matches!(
                request.mutation,
                forge_github::review_mutation::ReviewMutation::ReviewerChange { .. }
            ) {
                return Err(RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: "this operation requires explicit unknown closure or operation-specific reconciliation".into(),
                });
            }
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
            if matches!(request.mutation, ReviewMutation::ReviewerChange { .. }) {
                self.reviewer_writes
                    .lock()
                    .unwrap()
                    .push(request.mutation.clone());
                self.entered.add_permits(1);
                if self.hold.load(Ordering::Acquire) {
                    self.release.acquire().await.unwrap().forget();
                }
                let failure = || RemoteFailure {
                    kind: RemoteFailureKind::Transport,
                    message: "reviewer write failed".into(),
                };
                if self.reject.load(Ordering::Acquire) {
                    return ReviewMutationOutcome::Rejected(failure());
                }
                if self.uncertain.load(Ordering::Acquire) {
                    return ReviewMutationOutcome::OutcomeUnknown(failure());
                }
                return ReviewMutationOutcome::Confirmed(
                    serde_json::json!({"users":[{"login":"bob"}],"teams":[]}),
                );
            }
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
        replacement
            .save(&reopened.document, vec![], vec![])
            .await
            .is_err(),
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
    let _ = first.save(&opened.document, vec![], vec![]).await;
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
    assert!(
        replacement
            .save(&reopened.document, vec![], vec![])
            .await
            .is_err()
    );
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
async fn unknown_creation_links_after_restart_without_reposting_and_keeps_newer_text() {
    use forge_github::review_mutation::RecoveryResolution;
    use forge_review::{comments::CommentSaveAction, review::ReviewCommentCommand};
    let remote = remote();
    let first = ReviewService::new(github(&remote));
    let opened = first.open_pr(remote.clone(), target()).await.unwrap();
    let draft = first
        .comment(
            &opened.document,
            vec![],
            vec![],
            ReviewCommentCommand::DraftConversation,
        )
        .await
        .unwrap()
        .snapshot;
    edit_comment(&first, &draft, "captured create").await;
    remote.uncertain.store(true, Ordering::Release);
    let uncertain = first
        .comment(
            &opened.document,
            vec![],
            vec![],
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
            vec![],
            vec![],
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
                vec![],
                vec![],
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
            vec![],
            vec![],
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
            vec![],
            vec![],
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
    remote.entered.acquire().await.unwrap().forget();
}

#[tokio::test]
async fn held_save_keeps_newer_native_edits_and_batches_dirty_fields() {
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let initial = service.open_pr(remote.clone(), target()).await.unwrap();
    assert!(
        service
            .save(&initial.document, vec![], vec![])
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
    let saving = tokio::spawn(async move { owner.save(&document, vec![], vec![]).await });
    entered(&remote).await;
    assert!(
        service
            .save(&initial.document, vec![], vec![])
            .await
            .is_err()
    );
    let before = service.snapshot(&initial.document).unwrap();
    let body = before
        .field
        .iter()
        .find(|field| field.region.0 == "body")
        .unwrap();
    let rejected = RegionEdit {
        document: initial.document.clone(),
        region: body.region.clone(),
        base: body.revision,
        sequence: EditSequence(body.sequence.0 + 1),
        text: "rejected concurrent capture".into(),
    };
    assert!(
        service
            .save(&initial.document, vec![rejected], vec![])
            .await
            .is_err()
    );
    let rejected_comment = RegionEdit {
        document: initial.document.clone(),
        region: body.region.clone(),
        base: body.revision,
        sequence: EditSequence(body.sequence.0 + 1),
        text: "rejected comment capture".into(),
    };
    assert!(
        service
            .comment(
                &initial.document,
                vec![rejected_comment],
                vec![],
                load_comment()
            )
            .await
            .is_err()
    );
    let rejected_submission = RegionEdit {
        document: initial.document.clone(),
        region: body.region.clone(),
        base: body.revision,
        sequence: EditSequence(body.sequence.0 + 1),
        text: "rejected submission capture".into(),
    };
    assert!(
        service
            .submit_batched(
                &initial.document,
                vec![rejected_submission],
                vec![],
                forge_review::review::ReviewVerdict::Comment,
            )
            .await
            .is_err()
    );
    assert_eq!(
        serde_json::to_value(service.snapshot(&initial.document).unwrap()).unwrap(),
        serde_json::to_value(before).unwrap()
    );
    change(&service, &initial.document, "body", "newer native text").await;
    remote.release.add_permits(1);
    let result = saving.await.unwrap().unwrap();
    assert!(!result.snapshot.field[0].dirty);
    assert!(result.snapshot.field[1].dirty);
    assert_eq!(result.snapshot.field[1].text, "newer native text");
    assert_eq!(result.snapshot.field[1].baseline, "Î»\n  body\n");
    assert_eq!(remote.writes.lock().unwrap().len(), 1);
    remote.hold.store(false, Ordering::Release);
    let result = service
        .save(&initial.document, vec![], vec![])
        .await
        .unwrap();
    assert!(result.snapshot.field.iter().all(|field| !field.dirty));
    let recorded = remote.writes.lock().unwrap();
    let PullRequestMutation::Edit(edit) = &recorded[1] else {
        panic!("expected edit")
    };
    assert!(edit.title.is_none());
    assert_eq!(edit.body.as_deref(), Some("newer native text"));
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
        .comment(&document, vec![], vec![], load_comment())
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &loaded, "local draft").await;
    remote.comment.lock().unwrap().as_mut().unwrap().body = "remote edit".into();
    let refreshed = service
        .comment(&document, vec![], vec![], load_comment())
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
                vec![],
                vec![],
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
                vec![],
                vec![],
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
            vec![],
            vec![],
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
            vec![],
            vec![],
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
        .comment(
            &document,
            vec![],
            vec![],
            ReviewCommentCommand::DraftInline { anchor },
        )
        .await
        .unwrap()
        .snapshot;
    edit_comment(&service, &draft, "inline λ\n").await;
    let saved = service
        .comment(
            &document,
            vec![],
            vec![],
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
            vec![],
            vec![],
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
            vec![],
            vec![],
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
            vec![],
            vec![],
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
            vec![],
            vec![],
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
            vec![],
            vec![],
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
async fn immutable_file_analysis_uses_merge_base_and_fork_head_sources() {
    use forge_diff::{cache::CacheLimits, engine::DiffEngine, syntax::SyntaxEngine};
    use forge_github::review_api::ReviewPage;
    use forge_review::service::ReviewSectionKind;
    let remote = remote();
    remote.section_page.lock().unwrap().extend([
    ReviewPage { records: vec![serde_json::json!({"number":7,"node_id":"PR_test","title":"Title",
        "base":{"sha":"a".repeat(40)},"head":{"sha":"b".repeat(40),"repo":{"full_name":"fork/project"}}})], next_cursor: None, complete:true },
    ReviewPage { records: vec![serde_json::json!({"filename":"src/new.rs","previous_filename":"src/old.rs","status":"renamed","sha":"e".repeat(40)})], next_cursor:None, complete:true },
]);
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool());
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
    {
        let request = remote.source_request.lock().unwrap();
        assert_eq!(request.len(), 2);
        assert_eq!(request[0].commit, "c".repeat(40));
        assert_eq!(request[0].path, "src/old.rs");
        assert_eq!(request[1].repository.repository_name(), "fork/project");
        assert_eq!(request[1].commit, "b".repeat(40));
    }
    assert_eq!(diff.usage().active_jobs, 0);
    assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
    assert_eq!(syntax.usage().active_jobs, 0);
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
        review::ReviewVerdict,
    };
    let remote = remote();
    let service = ReviewService::new(github(&remote));
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    service.begin_batched(&opened.document).await.unwrap();
    let region = RegionId("draft-comment/local-batched/body".into());
    let capture = vec![
        RegionEdit {
            document: opened.document.clone(),
            region: RegionId("review_summary".into()),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(1),
            text: "Summary".into(),
        },
        RegionEdit {
            document: opened.document.clone(),
            region: region.clone(),
            base: forge_buffer::identity::RegionRevision(0),
            sequence: EditSequence(2),
            text: "Inline finding".into(),
        },
    ];
    let draft = vec![forge_review::comments::DraftComment {
        region,
        anchor: Some(CommentAnchor {
            revision: "b".repeat(40),
            path: "src/lib.rs".into(),
            side: CommentSide::Right,
            first_line: 4,
            last_line: 4,
        }),
        reply_to: None,
    }];
    let delivered = service
        .submit_batched(&opened.document, capture, draft, ReviewVerdict::Comment)
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

/// Opens an editable reviewer field through the same section boundary used by the PR header.
async fn reviewer_document(remote: &Arc<TestRemote>) -> (ReviewService, DocumentId) {
    use forge_diff::workers::{AnalysisPool, PoolLimits};
    let service = ReviewService::with_analysis(
        github(remote),
        Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 8,
        })),
    );
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    remote
        .section_page
        .lock()
        .unwrap()
        .push_back(forge_github::review_api::ReviewPage {
            records: vec![
                serde_json::json!({"users":[{"login":"alice"}],"teams":[{"slug":"core"}]}),
            ],
            next_cursor: None,
            complete: true,
        });
    service
        .read_section(
            &opened.document,
            remote.recovery.path().into(),
            remote.clone(),
            forge_review::service::ReviewSectionKind::RequestedReviewers,
            None,
        )
        .await
        .unwrap();
    (service, opened.document)
}

#[tokio::test]
async fn reviewer_save_preserves_teams_and_newer_edits_then_restores_the_draft() {
    let remote = remote();
    let (service, document) = reviewer_document(&remote).await;
    change(&service, &document, "title", "Edited title").await;
    change(&service, &document, "body", "Edited description").await;
    change(&service, &document, "reviewers", "@bob @owner/core").await;
    let saved = service.save(&document, vec![], vec![]).await.unwrap();
    assert!(saved.snapshot.field.iter().all(|field| !field.dirty));
    let writes = remote.reviewer_writes.lock().unwrap().clone();
    let forge_github::review_mutation::ReviewMutation::ReviewerChange { add, remove } = &writes[0]
    else {
        panic!("reviewer mutation missing")
    };
    assert_eq!(add.reviewer, ["bob"]);
    assert_eq!(remove.reviewer, ["alice"]);
    assert!(add.team.is_empty() && remove.team.is_empty());
    assert_eq!(
        remote.writes.lock().unwrap().len(),
        1,
        "title and description must share one mutation"
    );
    change(&service, &document, "reviewers", "@carol @owner/core").await;
    remote.hold.store(true, Ordering::Release);
    remote
        .entered
        .forget_permits(remote.entered.available_permits());
    let saving = {
        let service = service.clone();
        let document = document.clone();
        tokio::spawn(async move { service.save(&document, vec![], vec![]).await })
    };
    remote.entered.acquire().await.unwrap().forget();
    change(&service, &document, "reviewers", "@dave @owner/core").await;
    remote.release.add_permits(1);
    let saved = saving.await.unwrap().unwrap();
    let field = saved
        .snapshot
        .field
        .iter()
        .find(|field| field.region.0 == "reviewers")
        .unwrap();
    assert!(field.dirty);
    assert_eq!(field.baseline, "@carol @owner/core");
    assert_eq!(field.text, "@dave @owner/core");
    service.close_document(&document).unwrap();
    let replacement = ReviewService::new(github(&remote));
    let restored = replacement.open_pr(remote.clone(), target()).await.unwrap();
    let field = restored
        .field
        .iter()
        .find(|field| field.region.0 == "reviewers")
        .unwrap();
    assert!(field.dirty);
    assert_eq!(field.text, "@dave @owner/core");
    assert_eq!(field.baseline, "@carol @owner/core");
    replacement.close();
    service.close();
}

#[tokio::test]
async fn commit_diff_controls_isolate_immutable_sources_and_preserve_them_on_refresh() {
    use forge_buffer::{
        block::TextPosition,
        identity::{InputSequence, ViewId},
        input::DocumentInput,
    };
    use forge_diff::{
        cache::CacheLimits,
        engine::DiffEngine,
        syntax::SyntaxEngine,
        view::{DiffPresentation, DiffVisibility},
    };
    use forge_github::{
        review_api::ReviewPage,
        review_source::{ReviewCommitDetail, ReviewCommitFile},
    };
    use forge_review::service::ReviewSectionKind;
    let remote = remote();
    let source = |public: &str, private: &str| {
        format!("pub fn visible() {{\n    {public}();\n}}\nfn hidden() {{\n    {private}();\n}}\n")
            .into_bytes()
    };
    remote.immutable_content.lock().unwrap().extend([
        (
            ("c".repeat(40), "source.rs".into()),
            source("previous", "old_private"),
        ),
        (
            ("b".repeat(40), "source.rs".into()),
            source("pr_current", "pr_private"),
        ),
        (
            ("f".repeat(40), "source.rs".into()),
            source("first_commit", "first_private"),
        ),
        (
            ("a".repeat(40), "source.rs".into()),
            source("second_commit", "second_private"),
        ),
    ]);
    remote.section_page.lock().unwrap().extend([
        ReviewPage { records: vec![serde_json::json!({"number":7,"node_id":"PR_test","title":"Initial title", "state":"open",
            "base":{"sha":"a".repeat(40)},"head":{"sha":"b".repeat(40),"repo":{"full_name":"owner/repo"}}})], next_cursor:None, complete:true },
        ReviewPage { records: vec![serde_json::json!({"filename":"source.rs","status":"modified","sha":"e".repeat(40)})], next_cursor:None, complete:true },
        ReviewPage { records: vec![
            serde_json::json!({"sha":"f".repeat(40),"commit":{"message":"first commit", "committer":{"date":"1 day ago"}}}),
            serde_json::json!({"sha":"a".repeat(40),"commit":{"message":"second commit", "committer":{"date":"2 days ago"}}}),
        ], next_cursor:None, complete:true },
    ]);
    let diff = DiffEngine::new(CacheLimits::default(), 2);
    let syntax = SyntaxEngine::new(diff.analysis_pool());
    let service = ReviewService::with_engines(github(&remote), diff.clone(), syntax.clone());
    let opened = service.open_pr(remote.clone(), target()).await.unwrap();
    let directory = remote.recovery.path().to_path_buf();
    for section in [
        ReviewSectionKind::Overview,
        ReviewSectionKind::Files,
        ReviewSectionKind::Commits,
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
    let view = ViewId("semantic-commit-controls".into());
    service
        .view(&opened.document, view.clone(), Default::default())
        .await
        .unwrap();
    service
        .read_file(
            &opened.document,
            directory.clone(),
            remote.clone(),
            "source.rs".into(),
        )
        .await
        .unwrap();
    for commit in ["f".repeat(40), "a".repeat(40)] {
        *remote.commit_detail.lock().unwrap() = Some(ReviewCommitDetail {
            commit: commit.clone(),
            parent: Some("c".repeat(40)),
            subject: "commit".into(),
            file: vec![ReviewCommitFile {
                path: "source.rs".into(),
                previous_path: None,
                status: "modified".into(),
                patch: Some("@@ -1 +1 @@\n-sparse_previous\n+sparse_current".into()),
            }],
        });
        let frame = service
            .materialize(&opened.document, Default::default())
            .await
            .unwrap();
        let anchor = frame
            .snapshot
            .block
            .iter()
            .find(|block| {
                block
                    .metadata
                    .target
                    .iter()
                    .any(|target| target.id.0.ends_with(":diff"))
                    && block
                        .text
                        .wire_rows()
                        .iter()
                        .any(|row| row.starts_with(&commit[..7]))
            })
            .unwrap()
            .id
            .clone();
        service
            .read_commit_diff(
                &opened.document,
                directory.clone(),
                remote.clone(),
                commit.clone(),
                anchor.clone(),
            )
            .await
            .unwrap()
            .unwrap();
        assert!(
            service
                .read_commit_diff(
                    &opened.document,
                    directory.clone(),
                    remote.clone(),
                    commit,
                    anchor
                )
                .await
                .unwrap()
                .is_none()
        );
    }
    let mut frame = service
        .materialize(&opened.document, Default::default())
        .await
        .unwrap();
    let contains = |frame: &forge_review::service::ReviewMaterialization, text: &str| {
        frame
            .snapshot
            .block
            .iter()
            .any(|block| block.text.wire_rows().iter().any(|row| row.contains(text)))
    };
    for text in [
        "pr_current",
        "pr_private",
        "first_commit",
        "first_private",
        "second_commit",
        "second_private",
    ] {
        assert!(contains(&frame, text), "{text}");
    }
    assert!(!contains(&frame, "sparse_current"));
    let acquired = remote.source_request.lock().unwrap().len();
    let initial = serde_json::to_value(&frame).unwrap();
    let mut transitions = Vec::new();
    for (sequence, action) in [
        "toggle_presentation",
        "source_expand",
        "toggle_visibility",
        "toggle_presentation",
        "toggle_visibility",
    ]
    .into_iter()
    .enumerate()
    {
        let selected = if action == "source_expand" {
            frame
                .snapshot
                .block
                .iter()
                .find(|block| {
                    block
                        .id
                        .0
                        .starts_with(&format!("commit:{}:", "f".repeat(40)))
                        && block.text.row(0) == Some("pub fn visible() {")
                })
                .unwrap()
        } else {
            &frame.snapshot.block[0]
        };
        let delivery = service
            .act(
                DocumentInput {
                    document: opened.document.clone(),
                    revision: frame.snapshot.revision,
                    view: view.clone(),
                    sequence: InputSequence(sequence as u64 + 1),
                    action: action.into(),
                    block: selected.id.clone(),
                    position: TextPosition { row: 0, column: 0 },
                    target: None,
                },
                directory.clone(),
            )
            .await
            .unwrap();
        for patch in &delivery.patch {
            patch.validate().unwrap();
        }
        frame = service
            .materialize(&opened.document, Default::default())
            .await
            .unwrap();
        assert!(
            frame.patch.is_none(),
            "refresh must retain the published commit bodies"
        );
        transitions.push(serde_json::json!({"action":action,"delivery":delivery,"expected":frame}));
        match sequence {
            0 => {
                assert_eq!(frame.diff_view.presentation, DiffPresentation::Semantic);
                assert!(
                    !contains(&frame, "first_commit")
                        && !contains(&frame, "second_commit")
                        && !contains(&frame, "pr_current")
                );
                let headings = frame
                    .snapshot
                    .block
                    .iter()
                    .filter(|block| block.text.row(0) == Some("pub fn visible() {"))
                    .collect::<Vec<_>>();
                assert_eq!(headings.len(), 3);
                assert!(
                    headings
                        .iter()
                        .all(|block| block.metadata.fold[0].collapsed_suffix.as_deref()
                            == Some(" … 1 change }"))
                );
            }
            1 => {
                assert!(contains(&frame, "first_commit"));
                assert!(!contains(&frame, "second_commit") && !contains(&frame, "pr_current"));
            }
            2 => {
                assert_eq!(frame.diff_view.visibility, DiffVisibility::Public);
                assert!(!contains(&frame, "hidden") && contains(&frame, "first_commit"));
            }
            3 => {
                assert_eq!(frame.diff_view.presentation, DiffPresentation::Full);
                for text in ["first_commit", "second_commit", "pr_current"] {
                    assert!(contains(&frame, text));
                }
                assert!(!contains(&frame, "first_private"));
            }
            4 => {
                for text in ["first_private", "second_private", "pr_private"] {
                    assert!(contains(&frame, text));
                }
            }
            _ => unreachable!(),
        }
        assert!(
            frame
                .inline_anchor
                .keys()
                .all(|target| !target.0.starts_with("commit:")),
            "commit rows cannot create current PR comments"
        );
    }
    assert_eq!(remote.source_request.lock().unwrap().len(), acquired);
    assert!(remote.inline_creation.lock().unwrap().is_empty());
    assert_eq!(diff.analysis_pool().usage().admitted_jobs, 0);
    assert_eq!(syntax.usage().active_jobs, 0);
    forge_recorded_fixtures::assert_matches_recording(
        "review-commit-semantic.json",
        &serde_json::json!({"initial":initial,"transition":transitions}),
    );
}
