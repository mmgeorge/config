use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use anyhow::Result;
use forge_git::command::{CommandLimits, read_command};
use forge_github::model::{GithubRepositoryId, SyncScope};
use forge_github::remote::{
    GhClient, GithubRemote, IssueDetailRequest, IssuePageRequest, RemoteFailureKind,
};
use serde_json::json;

struct FixtureExecutable {
    _directory: tempfile::TempDir,
    path: PathBuf,
}
static EXECUTABLE: OnceLock<FixtureExecutable> = OnceLock::new();

fn executable() -> &'static Path {
    &EXECUTABLE
        .get_or_init(|| {
            let directory = tempfile::tempdir().unwrap();
            let source = directory.path().join("gh.rs");
            let path = directory.path().join(if cfg!(windows) {
                "gh-fixture.exe"
            } else {
                "gh-fixture"
            });
            fs::write(&source, include_str!("fixtures/gh.rs")).unwrap();
            let output = read_command(
                Command::new("rustup")
                    .args(["run", "1.94.0", "rustc", "--edition=2024"])
                    .arg(&source)
                    .arg("-o")
                    .arg(&path),
                CommandLimits {
                    stdout_bytes: 64 * 1024,
                    stderr_bytes: 64 * 1024,
                    timeout: Duration::from_secs(30),
                },
                || Ok(()),
            )
            .unwrap();
            assert!(
                output.status.success(),
                "{}",
                String::from_utf8_lossy(&output.stderr)
            );
            FixtureExecutable {
                _directory: directory,
                path,
            }
        })
        .path
}

fn fixture(mode: &str) -> Result<(tempfile::TempDir, Arc<GhClient>)> {
    let root = tempfile::tempdir()?;
    fs::write(root.path().join("host"), "enterprise.example")?;
    fs::write(root.path().join("mode"), mode)?;
    fs::write(
        root.path().join("response.json"),
        serde_json::to_vec(&json!({"data": {
            "rateLimit": {"remaining":42}, "repository": {"issues": {"totalCount":1,
                "pageInfo":{"hasNextPage":false,"endCursor":"last"}, "nodes":[{
                    "number":7,"title":"Unicode é record","state":"OPEN","url":"https://enterprise.example/owner/repo/issues/7",
                    "createdAt":"2026-09-01T00:00:00Z","updatedAt":"2026-09-07T00:00:00Z",
                    "labels":{"nodes":[{"name":"bug","color":"ffffff","description":null}]}
                }]
            }}
        }}))?,
    )?;
    let client = Arc::new(GhClient::new(
        executable().to_owned(),
        root.path().to_owned(),
    )?);
    Ok((root, client))
}

fn request() -> IssuePageRequest {
    IssuePageRequest {
        repository: GithubRepositoryId::new("enterprise.example", "Owner", "Repo").unwrap(),
        scope: SyncScope::Open,
        incremental: false,
        cursor: None,
    }
}

#[tokio::test]
async fn immutable_source_acquires_raw_git_blob_without_text_or_symlink_conversion() -> Result<()> {
    use forge_github::{review_api::GithubReviewRemote, review_source::ReviewSourceRequest};
    let (root, client) = fixture("success")?;
    let bytes = b"../target\r\n\0\xff";
    fs::write(root.path().join("source-blob"), bytes)?;
    fs::write(
        root.path().join("source-identity.json"),
        serde_json::to_vec(&json!({"data":{"repository":{
            "commit":{"__typename":"Commit","oid":"a".repeat(40)},
            "source":{"__typename":"Blob","oid":"b".repeat(40),"byteSize":bytes.len()}
        }}}))?,
    )?;
    let path = "folder/λ \"quoted\".rs";
    let source = client
        .read_source(ReviewSourceRequest {
            repository: request().repository,
            commit: "a".repeat(40),
            path: path.into(),
        })
        .await?;
    assert_eq!(source.bytes, bytes);
    assert_eq!(source.blob, Some("b".repeat(40)));
    let captured: serde_json::Value =
        serde_json::from_slice(&fs::read(root.path().join("source-request.json"))?)?;
    assert_eq!(
        captured["variables"]["expression"],
        format!("{}:{path}", "a".repeat(40))
    );
    assert!(root.path().join("source-blob-read").exists());
    Ok(())
}

#[tokio::test]
async fn immutable_source_absence_never_requests_a_blob() -> Result<()> {
    use forge_github::{review_api::GithubReviewRemote, review_source::ReviewSourceRequest};
    let (root, client) = fixture("success")?;
    fs::write(
        root.path().join("source-identity.json"),
        serde_json::to_vec(&json!({"data":{"repository":{
            "commit":{"__typename":"Commit","oid":"a".repeat(40)},"source":null
        }}}))?,
    )?;
    let source = client
        .read_source(ReviewSourceRequest {
            repository: request().repository,
            commit: "a".repeat(40),
            path: "absent.rs".into(),
        })
        .await?;
    assert!(source.blob.is_none() && source.bytes.is_empty());
    assert!(!root.path().join("source-blob-read").exists());
    Ok(())
}

#[tokio::test]
async fn pending_submission_capture_and_preflight_reject_changed_membership_or_text() -> Result<()>
{
    use forge_github::{
        recovery::{RecoveryResource, RecoveryResourceKind},
        remote::PendingReviewExpected,
        review_mutation::{
            GithubReviewWriteRemote, ReviewEvent, ReviewMutation, ReviewMutationRequest,
        },
    };
    let (root, client) = fixture("success")?;
    fs::write(
        root.path().join("submission-parent.json"),
        serde_json::to_vec(&json!({"number":7,"node_id":"PR_test","head":{"sha":"a".repeat(40)}}))?,
    )?;
    fs::write(
        root.path().join("submission-review.json"),
        serde_json::to_vec(&json!({"id":9,"state":"PENDING","user":{"node_id":"ACTOR_fixture"}}))?,
    )?;
    let member = |body| {
        json!({"id":12,"node_id":"COMMENT_12","body":body,"path":"file.rs",
        "commit_id":"a".repeat(40),"position":1,"line":1,"side":"RIGHT"})
    };
    fs::write(
        root.path().join("submission-comments.json"),
        serde_json::to_vec(&json!([member("captured\r\nbody")]))?,
    )?;
    let directory = client.for_directory(root.path().to_path_buf())?;
    let context = directory
        .submission_context(
            request().repository,
            7,
            Some(9),
            vec![PendingReviewExpected {
                id: 12,
                body: "captured\r\nbody".into(),
            }],
        )
        .await?;
    assert_eq!(context.pending_comment.len(), 1);
    let captured = ReviewMutationRequest {
        resource: RecoveryResource {
            repository: request().repository,
            kind: RecoveryResourceKind::PullRequest,
            number: 7,
        },
        operation_id: "submission_test".into(),
        actor_node_id: context.actor_node_id,
        edit_sequence: Some(1),
        draft_target: Some("review:submission".into()),
        parent_node_id: Some("PR_test".into()),
        mutation: ReviewMutation::ReviewSubmit {
            body: "summary".into(),
            event: ReviewEvent::Comment,
            pending_review_id: context.pending_review_id,
            commit_id: Some(context.commit_id),
            pending_comment: Some(context.pending_comment),
            comment: Vec::new(),
        },
    };
    captured.validate()?;
    directory.validate_review_write(captured.clone()).await?;
    fs::write(
        root.path().join("submission-comments.json"),
        serde_json::to_vec(&json!([member("newer body")]))?,
    )?;
    let rejected = directory
        .validate_review_write(captured.clone())
        .await
        .unwrap_err();
    assert!(
        rejected.message.contains("membership or text changed"),
        "{rejected}"
    );
    assert!(
        directory
            .submission_context(
                request().repository,
                7,
                Some(9),
                vec![PendingReviewExpected {
                    id: 12,
                    body: "captured\r\nbody".into()
                }]
            )
            .await
            .is_err()
    );
    assert!(
        directory
            .submission_context(request().repository, 7, Some(9), Vec::new())
            .await
            .is_err()
    );
    fs::write(
        root.path().join("submission-comments.json"),
        serde_json::to_vec(&json!([member("captured\r\nbody")]))?,
    )?;
    fs::write(
        root.path().join("submission-after.json"),
        serde_json::to_vec(&json!([member("changed during dispatch")]))?,
    )?;
    fs::write(
        root.path().join("submission-result.json"),
        serde_json::to_vec(&json!({"id":9,"state":"COMMENTED","body":"summary",
        "user":{"node_id":"ACTOR_fixture"},"pull_request_url":"https://enterprise.example/api/v3/repos/owner/repo/pulls/7"}))?,
    )?;
    let outcome = directory.mutate_review(captured.clone()).await;
    assert!(
        matches!(outcome, forge_github::review_mutation::ReviewMutationOutcome::OutcomeUnknown(ref failure)
        if failure.message.contains("membership or text differs")),
        "post-dispatch membership change was not retained as unknown"
    );
    assert!(
        directory
            .read_review_write_result(captured, 9)
            .await
            .is_err()
    );
    Ok(())
}

#[tokio::test]
async fn repository_users_merge_sources_and_preserve_partial_failure() -> Result<()> {
    let (root, client) = fixture("success")?;
    fs::write(
        root.path().join("contributors.json"),
        r#"[[{"login":"Bob"},{"login":"Alice"}]]"#,
    )?;
    fs::write(
        root.path().join("collaborators.json"),
        r#"[[{"login":"alice","name":"Alice Developer"},{"login":"carol"}]]"#,
    )?;
    let users = client.read_repository_users(request().repository).await?;
    assert_eq!(
        users
            .contributors
            .iter()
            .map(|user| user.login.as_str())
            .collect::<Vec<_>>(),
        ["Alice", "Bob", "carol"]
    );
    assert_eq!(
        users.contributors[0].name.as_deref(),
        Some("Alice Developer")
    );
    assert!(users.failure.is_empty());
    fs::write(
        root.path().join("collaborators.failure"),
        "HTTP 403: Requires repository permission",
    )?;
    let users = client.read_repository_users(request().repository).await?;
    assert_eq!(users.contributors.len(), 2);
    assert_eq!(users.failure.len(), 1);
    assert!(users.failure[0].message.contains("collaborators"));
    fs::write(
        root.path().join("contributors.failure"),
        "HTTP 404: Not Found",
    )?;
    let failure = client
        .read_repository_users(request().repository)
        .await
        .unwrap_err();
    assert!(failure.message.contains("contributors") && failure.message.contains("collaborators"));
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn repository_users_distinguish_empty_pages_from_invalid_or_oversized_pages() -> Result<()> {
    let (root, client) = fixture("success")?;
    fs::write(root.path().join("contributors.json"), "[[]]")?;
    fs::write(root.path().join("collaborators.json"), "[]")?;
    assert!(
        client
            .read_repository_users(request().repository)
            .await?
            .contributors
            .is_empty()
    );
    for invalid in [
        "{}".to_owned(),
        "[[{}]]".into(),
        "[[{\"login\":\"bad user\"}]]".into(),
        serde_json::to_string(&vec![vec![json!({"login":"user"}); 101]])?,
    ] {
        fs::write(root.path().join("contributors.json"), &invalid)?;
        fs::write(root.path().join("collaborators.json"), &invalid)?;
        assert_eq!(
            client
                .read_repository_users(request().repository)
                .await
                .unwrap_err()
                .kind,
            RemoteFailureKind::InvalidResponse
        );
    }
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

async fn started(root: &Path, count: usize) -> Result<Vec<PathBuf>> {
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            let marker = fs::read_dir(root)?
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.path())
                .filter(|path| {
                    path.file_name()
                        .unwrap()
                        .to_string_lossy()
                        .starts_with("started.")
                })
                .collect::<Vec<_>>();
            if marker.len() == count {
                return Ok(marker);
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await?
}

#[tokio::test]
async fn native_gh_arguments_input_decode_and_cleanup_preserve_the_remote_contract() -> Result<()> {
    let (root, client) = fixture("success")?;
    let mut request = request();
    request.incremental = true;
    request.cursor = Some("opaque cursor with spaces".into());
    let page = client.read_issues(request).await?;
    assert_eq!(page.issues.len(), 1);
    assert_eq!(page.issues[0].repo, "owner/repo");
    assert_eq!(page.issues[0].title, "Unicode é record");
    assert_eq!(page.issues[0].labels[0].name, "bug");
    assert!(page.issues[0].body.is_none());
    assert!(page.next_cursor.is_none());
    assert_eq!(page.rate_remaining, Some(42));
    let marker = started(root.path(), 1).await?;
    let id = marker[0]
        .file_name()
        .unwrap()
        .to_string_lossy()
        .replace("started.", "");
    let encoded: serde_json::Value =
        serde_json::from_slice(&fs::read(root.path().join(format!("request.{id}.json")))?)?;
    assert_eq!(encoded["variables"]["owner"], "owner");
    assert_eq!(encoded["variables"]["states"], json!(["OPEN", "CLOSED"]));
    assert_eq!(encoded["variables"]["cursor"], "opaque cursor with spaces");
    let input = fs::read_to_string(root.path().join(format!("input.{id}")))?;
    assert!(!Path::new(&input).exists());
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn native_issue_detail_preserves_normalized_records_and_explicit_host_routing() -> Result<()>
{
    let (root, client) = fixture("success")?;
    fs::write(
        root.path().join("response.json"),
        include_str!("fixtures/detail.json"),
    )?;
    let request = IssueDetailRequest {
        repository: request().repository,
        number: 7,
    };
    let detail = client.read_issue_detail(request.clone()).await?;
    assert_eq!(detail.repo, "owner/repo");
    assert_eq!(detail.number, 7);
    assert_eq!(detail.comments_count, 1);
    assert!(detail.body.contains('λ'));
    let marker = started(root.path(), 1).await?;
    let id = marker[0]
        .file_name()
        .unwrap()
        .to_string_lossy()
        .replace("started.", "");
    let fields = fs::read_to_string(root.path().join(format!("fields.{id}")))?;
    assert!(fields.split(',').any(|field| field == "body"));
    assert!(fields.split(',').any(|field| field == "comments"));
    assert!(!root.path().join(format!("input.{id}")).exists());
    assert!(
        client
            .read_issue_detail(IssueDetailRequest {
                number: 0,
                ..request
            })
            .await
            .is_err()
    );
    assert_eq!(started(root.path(), 1).await?.len(), 1);
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn detail_and_page_reads_share_admission_and_reap_abandoned_children() -> Result<()> {
    let (root, client) = fixture("block")?;
    let mut caller = Vec::new();
    for position in 0..4 {
        let client = client.clone();
        caller.push(tokio::spawn(async move {
            if position < 2 {
                client.read_issues(request()).await.map(|_| ())
            } else {
                client
                    .read_issue_detail(IssueDetailRequest {
                        repository: request().repository,
                        number: 7,
                    })
                    .await
                    .map(|_| ())
            }
        }));
    }
    let marker = started(root.path(), 4).await?;
    let failure = client
        .read_issue_detail(IssueDetailRequest {
            repository: request().repository,
            number: 7,
        })
        .await
        .unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::Busy);
    let mut alive = Vec::new();
    for marker in marker {
        let id = marker
            .file_name()
            .unwrap()
            .to_string_lossy()
            .replace("started.", "");
        let lock = fs::File::options()
            .read(true)
            .write(true)
            .open(root.path().join(format!("alive.{id}")))?;
        assert!(lock.try_lock().is_err());
        alive.push(lock);
    }
    for caller in caller {
        caller.abort();
        assert!(caller.await.unwrap_err().is_cancelled());
    }
    client.shutdown(Duration::from_secs(2)).await?;
    for lock in alive {
        lock.try_lock()?;
    }
    Ok(())
}

#[tokio::test]
async fn native_detail_failures_preserve_diagnostics_and_never_return_empty_records() -> Result<()>
{
    let (root, client) = fixture("failure")?;
    fs::write(root.path().join("response.json"), "remote failure body")?;
    let request = IssueDetailRequest {
        repository: request().repository,
        number: 7,
    };
    let failure = client.read_issue_detail(request.clone()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::RateLimited);
    assert!(failure.message.contains("API rate limit exceeded"));
    assert!(failure.message.contains("remote failure body"));
    fs::write(root.path().join("mode"), "success")?;
    fs::write(root.path().join("response.json"), "{}")?;
    let failure = client.read_issue_detail(request).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::InvalidResponse);
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn abandoned_native_request_is_reaped_before_its_ownership_is_released() -> Result<()> {
    let (root, client) = fixture("block")?;
    let caller = {
        let client = client.clone();
        tokio::spawn(async move { client.read_issues(request()).await })
    };
    let marker = started(root.path(), 1).await?;
    let id = marker[0]
        .file_name()
        .unwrap()
        .to_string_lossy()
        .replace("started.", "");
    let alive = fs::File::options()
        .read(true)
        .write(true)
        .open(root.path().join(format!("alive.{id}")))?;
    assert!(alive.try_lock().is_err());
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    client.shutdown(Duration::from_secs(2)).await?;
    alive.try_lock()?;
    let input = fs::read_to_string(root.path().join(format!("input.{id}")))?;
    assert!(!Path::new(&input).exists());
    Ok(())
}

#[tokio::test]
async fn four_native_requests_bound_admission_and_overflow_does_not_start_a_child() -> Result<()> {
    let (root, client) = fixture("block")?;
    let mut caller = Vec::new();
    for _ in 0..4 {
        let client = client.clone();
        caller.push(tokio::spawn(
            async move { client.read_issues(request()).await },
        ));
    }
    started(root.path(), 4).await?;
    let failure = client.read_issues(request()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::Busy, "{failure}");
    assert_eq!(started(root.path(), 4).await?.len(), 4);
    for caller in caller {
        caller.abort();
        let _ = caller.await;
    }
    client.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn oversized_output_and_command_failure_do_not_become_empty_success() -> Result<()> {
    let (_root, client) = fixture("oversized")?;
    let failure = client.read_issues(request()).await.unwrap_err();
    assert!(failure.message.contains("stdout exceeds"), "{failure}");
    client.shutdown(Duration::from_secs(1)).await?;
    let (_root, client) = fixture("failure")?;
    let failure = client.read_issues(request()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::RateLimited);
    assert!(failure.message.contains("HTTP 403"));
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn checkout_contexts_share_one_native_admission_budget() -> Result<()> {
    let (first, client) = fixture("block")?;
    let (second, unused) = fixture("block")?;
    drop(unused);
    let context = client.for_directory(second.path().to_owned())?;
    let mut caller = Vec::new();
    for _ in 0..2 {
        let client = client.clone();
        caller.push(tokio::spawn(
            async move { client.read_issues(request()).await },
        ));
        let context = context.clone();
        caller.push(tokio::spawn(
            async move { context.read_issues(request()).await },
        ));
    }
    started(first.path(), 2).await?;
    started(second.path(), 2).await?;
    assert_eq!(
        context.read_issues(request()).await.unwrap_err().kind,
        RemoteFailureKind::Busy
    );
    for caller in caller {
        caller.abort();
        let _ = caller.await;
    }
    client.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}

#[tokio::test]
async fn inherited_pipe_handles_retain_admission_until_the_readers_exit() -> Result<()> {
    let (root, client) = fixture("descendant")?;
    let caller = {
        let client = client.clone();
        tokio::spawn(async move { client.read_issues(request()).await })
    };
    started(root.path(), 1).await?;
    tokio::time::timeout(Duration::from_secs(3), async {
        while !root.path().join("descendant.ready").exists() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await?;
    caller.abort();
    let _ = caller.await;
    assert!(client.shutdown(Duration::ZERO).await.is_err());
    fs::write(root.path().join("release-descendant"), "release")?;
    client.shutdown(Duration::from_secs(2)).await?;
    Ok(())
}
use forge_github::pull_request::{
    DesiredPullRequestState, PullRequestLifecycle, PullRequestOperation, PullRequestOutcome,
    PullRequestRequest, PullRequestTarget,
};
use forge_github::service::GithubService;

fn pr_request(operation: PullRequestOperation) -> PullRequestRequest {
    PullRequestRequest {
        target: PullRequestTarget {
            repository: request().repository,
            number: 7,
            node_id: "PR_fixture".into(),
        },
        request: operation,
    }
}

#[tokio::test]
async fn native_pr_transition_reopens_before_draft_and_unknown_requires_read_only_reconciliation()
-> Result<()> {
    let (root, client) = fixture("pr_success")?;
    let service = GithubService::default();
    let draft = pr_request(PullRequestOperation::Transition {
        desired: DesiredPullRequestState::Draft,
    });
    let result = service.pull_request(client.clone(), draft.clone()).await?;
    assert!(result.ok);
    assert_eq!(result.state, Some(PullRequestLifecycle::Open));
    assert_eq!(result.is_draft, Some(true));
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "reopenPullRequest\nconvertPullRequestToDraft\n"
    );
    let before = fs::read(root.path().join("mutation-log"))?;
    assert!(service.pull_request(client.clone(), draft).await?.ok);
    assert_eq!(fs::read(root.path().join("mutation-log"))?, before);
    fs::write(root.path().join("mode"), "pr_uncertain")?;
    let close = pr_request(PullRequestOperation::Transition {
        desired: DesiredPullRequestState::Closed,
    });
    let result = service.pull_request(client.clone(), close.clone()).await?;
    assert_eq!(result.outcome, PullRequestOutcome::OutcomeUnknown);
    assert!(result.state.is_none());
    assert_eq!(
        fs::read_to_string(root.path().join("pr-state"))?,
        "CLOSED true"
    );
    let before = started(root.path(), 6).await?.len();
    assert!(
        service
            .pull_request(client.clone(), close.clone())
            .await
            .unwrap_err()
            .to_string()
            .contains("OutcomeUnknown")
    );
    assert_eq!(started(root.path(), before).await?.len(), before);
    let reconcile = service
        .pull_request(client.clone(), pr_request(PullRequestOperation::Reconcile))
        .await?;
    assert_eq!(reconcile.outcome, PullRequestOutcome::Reconciled);
    assert_eq!(reconcile.state, Some(PullRequestLifecycle::Closed));
    let writes = fs::read(root.path().join("mutation-log"))?;
    assert!(service.pull_request(client.clone(), close).await?.ok);
    assert_eq!(fs::read(root.path().join("mutation-log"))?, writes);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn abandoned_pr_caller_retains_native_write_and_fifo_ownership_until_collection() -> Result<()>
{
    let (root, client) = fixture("pr_block")?;
    fs::write(root.path().join("pr-state"), "OPEN false")?;
    let service = GithubService::default();
    let caller = {
        let service = service.clone();
        let client = client.clone();
        tokio::spawn(async move {
            service
                .pull_request(
                    client,
                    pr_request(PullRequestOperation::Transition {
                        desired: DesiredPullRequestState::Closed,
                    }),
                )
                .await
        })
    };
    tokio::time::timeout(Duration::from_secs(3), async {
        while !root.path().join("mutation-started").exists() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await?;
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    let queued = {
        let service = service.clone();
        let client = client.clone();
        tokio::spawn(async move {
            service
                .pull_request(
                    client,
                    pr_request(PullRequestOperation::Transition {
                        desired: DesiredPullRequestState::Draft,
                    }),
                )
                .await
        })
    };
    tokio::time::sleep(Duration::from_millis(25)).await;
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "closePullRequest\n"
    );
    fs::write(root.path().join("release-mutation"), "release")?;
    assert!(queued.await??.ok);
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "closePullRequest\nreopenPullRequest\nconvertPullRequestToDraft\n"
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn shutdown_rejects_queued_mutations_and_retains_the_active_native_owner() -> Result<()> {
    let (root, client) = fixture("pr_block")?;
    fs::write(root.path().join("pr-state"), "OPEN false")?;
    let service = GithubService::default();
    let active = {
        let service = service.clone();
        let client = client.clone();
        tokio::spawn(async move {
            service
                .pull_request(
                    client,
                    pr_request(PullRequestOperation::Transition {
                        desired: DesiredPullRequestState::Closed,
                    }),
                )
                .await
        })
    };
    tokio::time::timeout(Duration::from_secs(3), async {
        while !root.path().join("mutation-started").exists() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await?;
    let waiting = {
        let service = service.clone();
        let client = client.clone();
        tokio::spawn(async move {
            service
                .pull_request(
                    client,
                    pr_request(PullRequestOperation::Transition {
                        desired: DesiredPullRequestState::Draft,
                    }),
                )
                .await
        })
    };
    tokio::task::yield_now().await;
    let shutdown = service.shutdown(Duration::from_millis(20)).await;
    assert_eq!(shutdown.unfinished_jobs, 1);
    assert!(waiting.await?.unwrap_err().to_string().contains("closed"));
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "closePullRequest\n"
    );
    fs::write(root.path().join("release-mutation"), "release")?;
    assert!(active.await??.ok);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn native_pr_edits_preserve_exact_text_and_reconcile_without_reposting() -> Result<()> {
    use forge_github::pull_request::PullRequestEdit;
    let (root, client) = fixture("pr_success")?;
    let service = GithubService::default();
    let title = "Unicode λ \"title\"".to_owned();
    let body = "First line\n\tIndented `code` λ\n".to_owned();
    fs::write(
        root.path().join("submitted-title.json"),
        serde_json::to_vec(&title)?,
    )?;
    fs::write(
        root.path().join("submitted-body.json"),
        serde_json::to_vec(&body)?,
    )?;
    let edit = PullRequestEdit {
        title: Some(title.clone()),
        body: Some(body.clone()),
    };
    let result = service
        .pull_request(
            client.clone(),
            pr_request(PullRequestOperation::Edit { edit: edit.clone() }),
        )
        .await?;
    assert!(result.ok && result.matches_submission == Some(true));
    assert_eq!(
        serde_json::from_slice::<String>(&fs::read(root.path().join("pr-title.json"))?)?,
        title
    );
    assert_eq!(
        serde_json::from_slice::<String>(&fs::read(root.path().join("pr-body.json"))?)?,
        body
    );
    assert!(
        service
            .pull_request(
                client.clone(),
                pr_request(PullRequestOperation::Edit { edit })
            )
            .await?
            .ok
    );
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "updatePullRequest\n"
    );
    fs::remove_file(root.path().join("submitted-title.json"))?;
    fs::write(root.path().join("submitted-body.json"), "\"\"")?;
    assert!(
        service
            .pull_request(
                client.clone(),
                pr_request(PullRequestOperation::Edit {
                    edit: PullRequestEdit {
                        title: None,
                        body: Some(String::new())
                    }
                })
            )
            .await?
            .ok
    );
    assert_eq!(
        serde_json::from_slice::<String>(&fs::read(root.path().join("pr-body.json"))?)?,
        ""
    );
    assert_eq!(
        serde_json::from_slice::<String>(&fs::read(root.path().join("pr-title.json"))?)?,
        title
    );
    fs::write(root.path().join("mode"), "pr_uncertain")?;
    let edit = PullRequestEdit {
        title: None,
        body: Some("Applied with an interrupted response".into()),
    };
    fs::write(
        root.path().join("submitted-body.json"),
        serde_json::to_vec(edit.body.as_ref().unwrap())?,
    )?;
    let result = service
        .pull_request(
            client.clone(),
            pr_request(PullRequestOperation::Edit { edit: edit.clone() }),
        )
        .await?;
    assert_eq!(result.outcome, PullRequestOutcome::OutcomeUnknown);
    let writes = fs::read(root.path().join("mutation-log"))?;
    let recovered = service
        .pull_request(
            client.clone(),
            pr_request(PullRequestOperation::ReconcileEdit { edit }),
        )
        .await?;
    assert_eq!(recovered.outcome, PullRequestOutcome::Reconciled);
    assert_eq!(recovered.matches_submission, Some(true));
    assert_eq!(fs::read(root.path().join("mutation-log"))?, writes);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn field_edits_and_lifecycle_changes_share_fifo_native_ownership() -> Result<()> {
    use forge_github::pull_request::PullRequestEdit;
    let (root, client) = fixture("pr_block")?;
    fs::write(root.path().join("pr-state"), "OPEN false")?;
    fs::write(root.path().join("submitted-title.json"), "\"Queued title\"")?;
    let service = GithubService::default();
    let active = {
        let service = service.clone();
        let client = client.clone();
        tokio::spawn(async move {
            service
                .pull_request(
                    client,
                    pr_request(PullRequestOperation::Edit {
                        edit: PullRequestEdit {
                            title: Some("Queued title".into()),
                            body: None,
                        },
                    }),
                )
                .await
        })
    };
    tokio::time::timeout(Duration::from_secs(3), async {
        while !root.path().join("mutation-started").exists() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await?;
    let next = {
        let service = service.clone();
        let client = client.clone();
        tokio::spawn(async move {
            service
                .pull_request(
                    client,
                    pr_request(PullRequestOperation::Transition {
                        desired: DesiredPullRequestState::Closed,
                    }),
                )
                .await
        })
    };
    tokio::time::sleep(Duration::from_millis(20)).await;
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "updatePullRequest\n"
    );
    fs::write(root.path().join("release-mutation"), "release")?;
    assert!(active.await??.ok && next.await??.ok);
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-log"))?,
        "updatePullRequest\nclosePullRequest\n"
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

fn comment_target(
    kind: forge_github::comment::CommentKind,
) -> forge_github::comment::CommentTarget {
    forge_github::comment::CommentTarget {
        pull_request: forge_github::pull_request::PullRequestTarget {
            repository: GithubRepositoryId::new("enterprise.example", "owner", "repo").unwrap(),
            number: 7,
            node_id: "PR_fixture".into(),
        },
        kind,
        node_id: "COMMENT_fixture".into(),
        database_id: 4_000_000_000,
    }
}

fn comment_state(kind: forge_github::comment::CommentKind, body: &str) -> serde_json::Value {
    json!({"__typename":kind,"id":"COMMENT_fixture","databaseId":"4000000000","body":body,
        "url":"https://enterprise.example/owner/repo/pull/7#discussion_r4000000000","viewerDidAuthor":true})
}

#[tokio::test]
async fn native_existing_comment_edits_and_deletes_validate_graphql_inputs_and_receipts()
-> Result<()> {
    use forge_github::comment::{CommentKind, CommentOperation, CommentResult};
    use forge_github::service::GithubService;
    for kind in [
        CommentKind::IssueComment,
        CommentKind::PullRequestReviewComment,
    ] {
        let (root, client) = fixture("comment_ok")?;
        let service = GithubService::default();
        let target = comment_target(kind);
        let body = "raw λ\n  body\n";
        fs::write(
            root.path().join("comment-read.json"),
            serde_json::to_vec(&json!({"data":{"node":comment_state(kind,"old")}}))?,
        )?;
        fs::write(
            root.path().join("comment-after.json"),
            serde_json::to_vec(&json!({"data":{"node":comment_state(kind,body)}}))?,
        )?;
        fs::write(
            root.path().join("comment-result.json"),
            serde_json::to_vec(
                &json!({"data":{"mutation":{"clientMutationId":"edit-1","comment":comment_state(kind,body)}}}),
            )?,
        )?;
        fs::write(
            root.path().join("expected-mutation"),
            if kind == CommentKind::IssueComment {
                "updateIssueComment"
            } else {
                "updatePullRequestReviewComment"
            },
        )?;
        let result = service
            .comment(
                client.clone(),
                target.clone(),
                CommentOperation::Edit {
                    body: body.into(),
                    receipt: "edit-1".into(),
                },
            )
            .await?;
        assert!(
            matches!(result, CommentResult::Confirmed {state:Some(state)} if state.body == body && state.database_id == 4_000_000_000)
        );
        fs::write(
            root.path().join("expected-mutation"),
            if kind == CommentKind::IssueComment {
                "deleteIssueComment"
            } else {
                "deletePullRequestReviewComment"
            },
        )?;
        fs::write(
            root.path().join("comment-after.json"),
            br#"{"data":{"node":null}}"#,
        )?;
        fs::write(
            root.path().join("comment-result.json"),
            br#"{"data":{"mutation":{"clientMutationId":"delete-1"}}}"#,
        )?;
        assert!(matches!(
            service
                .comment(
                    client.clone(),
                    target,
                    CommentOperation::Delete {
                        receipt: "delete-1".into()
                    }
                )
                .await?,
            CommentResult::Confirmed { state: None }
        ));
        assert_eq!(
            fs::read_to_string(root.path().join("comment-mutation-log"))?
                .lines()
                .count(),
            2
        );
        assert_eq!(
            service
                .shutdown(Duration::from_secs(1))
                .await
                .unfinished_jobs,
            0
        );
        client.shutdown(Duration::from_secs(1)).await?;
    }
    Ok(())
}

#[tokio::test]
async fn native_unknown_comment_write_reconciles_without_reposting() -> Result<()> {
    use forge_github::comment::{CommentKind, CommentOperation, CommentResult};
    use forge_github::service::GithubService;
    let kind = CommentKind::PullRequestReviewComment;
    let (root, client) = fixture("comment_uncertain")?;
    let service = GithubService::default();
    let target = comment_target(kind);
    fs::write(
        root.path().join("expected-mutation"),
        "updatePullRequestReviewComment",
    )?;
    fs::write(
        root.path().join("comment-read.json"),
        serde_json::to_vec(&json!({"data":{"node":comment_state(kind,"old")}}))?,
    )?;
    fs::write(
        root.path().join("comment-after.json"),
        serde_json::to_vec(&json!({"data":{"node":comment_state(kind,"saved")}}))?,
    )?;
    let edit = CommentOperation::Edit {
        body: "saved".into(),
        receipt: "edit-1".into(),
    };
    assert!(matches!(
        service
            .comment(client.clone(), target.clone(), edit.clone())
            .await?,
        CommentResult::OutcomeUnknown { .. }
    ));
    assert!(
        service
            .comment(client.clone(), target.clone(), edit)
            .await
            .is_err()
    );
    assert!(
        matches!(service.comment(client.clone(),target,CommentOperation::Reconcile).await?,CommentResult::Reconciled {state:Some(state)} if state.body == "saved")
    );
    assert_eq!(
        fs::read_to_string(root.path().join("comment-mutation-log"))?
            .lines()
            .count(),
        1
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn native_comment_decoder_rejects_missing_nodes_and_foreign_receipts() -> Result<()> {
    use forge_github::comment::{
        CommentKind, CommentOperation, CommentResult, GithubCommentRemote,
    };
    use forge_github::service::GithubService;
    let kind = CommentKind::IssueComment;
    let (root, client) = fixture("comment_ok")?;
    let target = comment_target(kind);
    for response in [
        json!({"data":{}}),
        json!({"data":{"node":null},"errors":[{"message":"not authorized"}]}),
        json!({"data":{"node":{"__typename":"IssueComment","id":"foreign"}}}),
    ] {
        fs::write(
            root.path().join("comment-read.json"),
            serde_json::to_vec(&response)?,
        )?;
        assert!(client.read_comment(target.clone()).await.is_err());
    }
    fs::write(
        root.path().join("comment-read.json"),
        br#"{"data":{"node":null}}"#,
    )?;
    assert!(client.read_comment(target.clone()).await?.is_none());
    fs::write(
        root.path().join("comment-read.json"),
        serde_json::to_vec(&json!({"data":{"node":comment_state(kind,"old")}}))?,
    )?;
    fs::write(
        root.path().join("comment-after.json"),
        serde_json::to_vec(&json!({"data":{"node":comment_state(kind,"saved")}}))?,
    )?;
    fs::write(
        root.path().join("comment-result.json"),
        serde_json::to_vec(
            &json!({"data":{"mutation":{"clientMutationId":"another-operation","comment":comment_state(kind,"saved")}}}),
        )?,
    )?;
    fs::write(root.path().join("expected-mutation"), "updateIssueComment")?;
    let service = GithubService::default();
    assert!(matches!(
        service
            .comment(
                client.clone(),
                target.clone(),
                CommentOperation::Edit {
                    body: "saved".into(),
                    receipt: "expected-operation".into()
                }
            )
            .await?,
        CommentResult::OutcomeUnknown { .. }
    ));
    assert!(
        matches!(service.comment(client.clone(),target,CommentOperation::Reconcile).await?,CommentResult::Reconciled {state:Some(state)} if state.body == "saved")
    );
    assert_eq!(
        fs::read_to_string(root.path().join("comment-mutation-log"))?
            .lines()
            .count(),
        1
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}

#[tokio::test]
async fn native_conversation_creation_confirms_only_correlated_owned_content() -> Result<()> {
    use forge_github::comment::{CommentKind, CommentResult, ConversationCommentCreation};
    use forge_github::service::GithubService;
    for fault in [
        "none", "receipt", "subject", "body", "owner", "kind", "lost", "parent",
    ] {
        let (root, client) = fixture(if fault == "lost" {
            "creation_uncertain"
        } else {
            "creation_ok"
        })?;
        let service = GithubService::default();
        let body = "raw λ\n  body\n\n";
        let request = ConversationCommentCreation {
            target: comment_target(CommentKind::IssueComment).pull_request,
            body: body.into(),
            receipt: "creation-1".into(),
        };
        for (field, value) in [
            ("subjectId", "PR_fixture"),
            ("body", body),
            ("clientMutationId", "creation-1"),
        ] {
            fs::write(
                root.path().join(format!("expected-{field}")),
                serde_json::to_vec(value)?,
            )?;
        }
        fs::write(
            root.path().join("creation-parent.json"),
            serde_json::to_vec(&json!({"data":{"repository":{"pullRequest":{
                "id":if fault == "parent" {"PR_foreign"} else {"PR_fixture"}, "number":7,"state":"OPEN","isDraft":false
            }}}}))?,
        )?;
        let mut state = comment_state(CommentKind::IssueComment, body);
        if fault == "body" {
            state["body"] = json!("different");
        }
        if fault == "owner" {
            state["viewerDidAuthor"] = json!(false);
        }
        if fault == "kind" {
            state["__typename"] = json!("PullRequestReviewComment");
        }
        fs::write(
            root.path().join("creation-result.json"),
            serde_json::to_vec(&json!({"data":{"mutation":{
                "clientMutationId":if fault == "receipt" {"foreign"} else {"creation-1"},
                "subject":{"id":if fault == "subject" {"PR_foreign"} else {"PR_fixture"}},
                "commentEdge":{"node":state}
            }}}))?,
        )?;
        let result = service
            .create_conversation_comment(client.clone(), request.clone())
            .await;
        if fault == "parent" {
            assert!(matches!(result?, CommentResult::Rejected { .. }));
            assert!(!root.path().join("creation-log").exists());
        } else {
            let result = result?;
            if fault == "none" {
                assert!(
                    matches!(result, CommentResult::Confirmed {state:Some(state)} if state.body == body && state.database_id == 4_000_000_000)
                );
            } else {
                assert!(
                    matches!(result, CommentResult::OutcomeUnknown { .. }),
                    "{fault}"
                );
                assert!(
                    service
                        .create_conversation_comment(client.clone(), request)
                        .await
                        .is_err()
                );
            }
            assert_eq!(
                fs::read_to_string(root.path().join("creation-log"))?
                    .lines()
                    .count(),
                1
            );
            assert!(root.path().join("created").exists());
        }
        assert_eq!(
            service
                .shutdown(Duration::from_secs(1))
                .await
                .unfinished_jobs,
            0
        );
        client.shutdown(Duration::from_secs(1)).await?;
    }
    Ok(())
}

#[tokio::test]
async fn native_issue_assignee_deltas_preserve_unrelated_members_and_partial_results() -> Result<()>
{
    use forge_github::recovery::{RecoveryPhase, RecoveryResource, RecoveryResourceKind};
    use forge_github::review_mutation::{ReviewMutation, ReviewMutationRequest};
    use forge_github::service::GithubService;
    for reject_add in [false, true] {
        let (root, client) = fixture("success")?;
        let issue = |title: &str, body: &str, assignee: Vec<&str>| {
            json!({
                "number":7,"node_id":"ISSUE_fixture","title":title,"body":body,
                "assignees":assignee.into_iter().map(|login| json!({"login":login})).collect::<Vec<_>>()
            })
        };
        let body = "exact\r\nbody\n";
        for (name, value) in [
            (
                "current",
                issue("Original", "before", vec!["keep", "remove"]),
            ),
            (
                "fields-result",
                issue("Captured", body, vec!["keep", "remove"]),
            ),
            ("remove-result", issue("Captured", body, vec!["keep"])),
            ("add-result", issue("Captured", body, vec!["keep", "added"])),
            ("fields-expected", json!({"title":"Captured","body":body})),
            ("remove-expected", json!({"assignees":["remove"]})),
            ("add-expected", json!({"assignees":["added"]})),
        ] {
            fs::write(
                root.path().join(format!("issue-edit-{name}.json")),
                serde_json::to_vec(&value)?,
            )?;
        }
        if reject_add {
            fs::write(root.path().join("issue-edit-reject-add"), "reject")?;
        }
        let service = GithubService::default();
        service.configure_recovery(root.path().join("forge/recovery/github/v1"))?;
        let request = ReviewMutationRequest {
            resource: RecoveryResource {
                repository: request().repository,
                kind: RecoveryResourceKind::Issue,
                number: 7,
            },
            operation_id: "issue-assignee-fixture".into(),
            actor_node_id: "ACTOR_fixture".into(),
            parent_node_id: Some("ISSUE_fixture".into()),
            edit_sequence: Some(1),
            draft_target: Some("issue:7".into()),
            mutation: ReviewMutation::IssueEdit {
                title: Some("Captured".into()),
                body: Some(body.into()),
                add_assignees: vec!["added".into()],
                remove_assignees: vec!["remove".into()],
            },
        };
        request.validate()?;
        let result = service
            .review_mutation(client.clone(), request.clone())
            .await?;
        let log = fs::read_to_string(root.path().join("issue-edit-log"))?;
        if reject_add {
            assert!(
                matches!(result.state, RecoveryPhase::OutcomeUnknown { .. }),
                "{:?}",
                result.state
            );
            assert_eq!(result.confirmed_steps.len(), 2);
            assert_eq!(log, "fields\nremove\n");
        } else {
            assert!(
                matches!(result.state, RecoveryPhase::Confirmed { .. }),
                "{:?}",
                result.state
            );
            assert_eq!(result.confirmed_steps.len(), 2);
            assert_eq!(log, "fields\nremove\nadd\n");
        }
        let observed: serde_json::Value =
            serde_json::from_slice(&fs::read(root.path().join("issue-edit-current.json"))?)?;
        assert_eq!(observed["assignees"][0]["login"], "keep");
        assert_eq!(observed["body"], body);
        assert!(
            service
                .review_mutation(client.clone(), request)
                .await
                .is_err()
        );
        assert_eq!(fs::read_to_string(root.path().join("issue-edit-log"))?, log);
        service.shutdown(Duration::from_secs(2)).await;
        client.shutdown(Duration::from_secs(2)).await?;
    }
    Ok(())
}
