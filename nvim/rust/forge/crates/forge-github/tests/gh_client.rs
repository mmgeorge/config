use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use anyhow::Result;
use forge_git::command::{CommandLimits, read_command};
use forge_github::model::{GithubRepositoryId, SyncScope};
use forge_github::remote::{GhClient, GithubRemote, IssuePageRequest, RemoteFailureKind};
use serde_json::json;

struct FixtureExecutable {
    _directory: tempfile::TempDir,
    path: PathBuf,
}
static EXECUTABLE: OnceLock<FixtureExecutable> = OnceLock::new();
// Each case controls its own native-process concurrency without competing with other cases.
static NATIVE_TEST_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

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
            let output = std::thread::scope(|scope| {
                scope
                    .spawn(|| {
                        read_command(
                            Command::new("rustup")
                                .args(["run", "1.94.0", "rustc", "--edition=2024"])
                                .arg(&source)
                                .arg("-o")
                                .arg(&path),
                            CommandLimits {
                                stderr_bytes: 64 * 1024,
                                timeout: Duration::from_secs(30),
                            },
                            || Ok(()),
                        )
                    })
                    .join()
                    .expect("GitHub fixture compiler thread")
            })
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

async fn started(root: &Path, count: usize) -> Result<Vec<PathBuf>> {
    async {
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
    }
    .await
}

#[tokio::test]
async fn native_gh_arguments_input_decode_and_cleanup_preserve_the_remote_contract() -> Result<()> {
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
async fn abandoned_native_request_is_reaped_before_its_ownership_is_released() -> Result<()> {
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
async fn oversized_output_and_command_failure_do_not_become_empty_success() -> Result<()> {
    let _native_test = NATIVE_TEST_LOCK.lock().await;
    let (root, client) = fixture("success")?;
    let mut response: serde_json::Value =
        serde_json::from_slice(&fs::read(root.path().join("response.json"))?)?;
    let title = format!("{}complete title tail", "x".repeat(8 * 1024 * 1024 + 1));
    response["data"]["repository"]["issues"]["nodes"][0]["title"] = json!(title);
    fs::write(
        root.path().join("response.json"),
        serde_json::to_vec(&response)?,
    )?;
    assert_eq!(client.read_issues(request()).await?.issues[0].title, title);
    client.shutdown(Duration::from_secs(1)).await?;
    let (_root, client) = fixture("oversized")?;
    let failure = client.read_issues(request()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::InvalidResponse);
    assert!(failure.message.contains("invalid issue JSON"), "{failure}");
    client.shutdown(Duration::from_secs(1)).await?;
    let (_root, client) = fixture("failure")?;
    let failure = client.read_issues(request()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::RateLimited);
    assert!(failure.message.contains("HTTP 403"));
    client.shutdown(Duration::from_secs(1)).await?;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    async {
        while !root.path().join("mutation-started").exists() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    }
    .await;
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
    fs::write(root.path().join("release-mutation"), "release")?;
    assert!(queued.await??.ok);
    // The fixture records whether release had happened when each gh process started, so a
    // queued write that ran ahead of the abandoned owner would log `released=false`.
    assert_eq!(
        fs::read_to_string(root.path().join("mutation-release-order"))?,
        "closePullRequest released=false\nreopenPullRequest released=true\nconvertPullRequestToDraft released=true\n"
    );
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    async {
        while !root.path().join("mutation-started").exists() {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    }
    .await;
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
    let shutdown = service.shutdown(Duration::ZERO).await;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
    let _native_test = NATIVE_TEST_LOCK.lock().await;
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
async fn native_review_routes_preserve_long_thread_identity_and_complete_pagination() -> Result<()>
{
    let _native_test = NATIVE_TEST_LOCK.lock().await;
    use forge_github::review_api::{GithubReviewRemote, ReviewReadRequest, ReviewSection};
    let (root, client) = fixture("review_read")?;
    let repository = request().repository;
    let response = root.path().join("response.json");
    let endpoint = root.path().join("review-endpoint");
    let records = (0..100)
        .map(|index| json!({"sha":format!("{index:040}")}))
        .collect::<Vec<_>>();
    fs::write(
        &endpoint,
        "repos/owner/repo/pulls/7/commits?per_page=100&page=100002",
    )?;
    fs::write(&response, serde_json::to_vec(&records)?)?;
    let page = client
        .read_review(ReviewReadRequest {
            repository: repository.clone(),
            number: 7,
            view: ReviewSection::Commits,
            cursor: Some("100002".into()),
        })
        .await?;
    assert_eq!(page.records, records);
    assert_eq!(page.next_cursor.as_deref(), Some("100003"));
    assert!(!page.complete);
    fs::write(
        &endpoint,
        "repos/owner/repo/pulls/7/commits?per_page=100&page=100003",
    )?;
    fs::write(&response, br#"[{"sha":"complete tail"}]"#)?;
    let page = client
        .read_review(ReviewReadRequest {
            repository: repository.clone(),
            number: 7,
            view: ReviewSection::Commits,
            cursor: page.next_cursor,
        })
        .await?;
    assert_eq!(page.records, vec![json!({"sha":"complete tail"})]);
    assert!(page.complete && page.next_cursor.is_none());

    let thread_node_id = format!("THREAD_{}", "x".repeat(513));
    fs::write(
        &response,
        serde_json::to_vec(&json!({"data":{"node":{
            "pullRequest":{"number":7,"repository":{"nameWithOwner":"owner/repo"}},
            "comments":{"nodes":[{"id":"comment","body":"complete λ tail"}],
                "pageInfo":{"hasNextPage":false,"endCursor":null}}
        }}}))?,
    )?;
    let page = client
        .read_review(ReviewReadRequest {
            repository: repository.clone(),
            number: 7,
            view: ReviewSection::ThreadComments {
                thread_node_id: thread_node_id.clone(),
            },
            cursor: None,
        })
        .await?;
    assert_eq!(page.records[0]["body"], "complete λ tail");
    assert!(page.complete && page.next_cursor.is_none());
    let encoded: serde_json::Value =
        serde_json::from_slice(&fs::read(root.path().join("review-request.json"))?)?;
    assert_eq!(encoded["variables"]["thread"], thread_node_id);
    assert_eq!(encoded["variables"]["owner"], "owner");
    assert_eq!(encoded["variables"]["name"], "repo");

    fs::write(&endpoint, "repos/owner/repo/pulls/7")?;
    let diff =
        "diff --git a/file b/file\n--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+complete λ tail\n";
    fs::write(&response, diff)?;
    let page = client
        .read_review(ReviewReadRequest {
            repository: repository.clone(),
            number: 7,
            view: ReviewSection::Diff,
            cursor: None,
        })
        .await?;
    assert_eq!(page.records, vec![json!({"diff":diff})]);
    assert!(page.complete && page.next_cursor.is_none());

    fs::write(&endpoint, "repos/owner/repo/issues/7")?;
    fs::write(&response, br#"{"number":7,"title":"complete object"}"#)?;
    let page = client
        .read_review(ReviewReadRequest {
            repository,
            number: 7,
            view: ReviewSection::Issue,
            cursor: None,
        })
        .await?;
    assert_eq!(
        page.records,
        vec![json!({"number":7,"title":"complete object"})]
    );
    assert!(page.complete && page.next_cursor.is_none());
    client.shutdown(Duration::from_secs(1)).await?;
    Ok(())
}
