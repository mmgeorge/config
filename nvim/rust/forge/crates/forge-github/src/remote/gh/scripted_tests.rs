//! Decoding and routing tests that answer `gh` requests in-process.
//!
//! Tests that verify the process contract itself (argument pipes, `--input` files, exit status
//! handling over real pipes, admission, and reaping) stay in `tests/gh_client.rs`, where a compiled
//! fixture executable stands in for `gh`.

use std::sync::{Arc, Mutex};
use std::time::Duration;

use serde_json::{Value, json};

use super::{GhClient, ScriptedGhInvocation, ScriptedGhOutcome};
use crate::model::{GithubRepositoryId, SyncScope};
use crate::remote::{GithubRemote, IssueDetailRequest, IssuePageRequest, RemoteFailureKind};
use crate::review_api::GithubReviewRemote;
use crate::review_source::ReviewSourceRequest;

fn repository() -> GithubRepositoryId {
    GithubRepositoryId::new("enterprise.example", "Owner", "Repo").unwrap()
}

fn argument_text(invocation: &ScriptedGhInvocation) -> Vec<String> {
    invocation
        .argument
        .iter()
        .map(|argument| argument.to_string_lossy().into_owned())
        .collect()
}

/// Keep every request a scripted client received, in order.
#[derive(Clone, Default)]
struct RequestLog(Arc<Mutex<Vec<(Vec<String>, Option<Value>)>>>);

impl RequestLog {
    fn record(&self, invocation: &ScriptedGhInvocation) {
        let input = invocation
            .input
            .as_ref()
            .map(|input| serde_json::from_slice(input).unwrap());
        self.0
            .lock()
            .unwrap()
            .push((argument_text(invocation), input));
    }

    fn requests(&self) -> Vec<(Vec<String>, Option<Value>)> {
        self.0.lock().unwrap().clone()
    }
}

#[tokio::test]
async fn immutable_source_acquires_raw_git_blob_without_text_or_symlink_conversion() {
    let bytes = b"../target\r\n\0\xff".to_vec();
    let log = RequestLog::default();
    let client = {
        let (log, bytes) = (log.clone(), bytes.clone());
        GhClient::scripted(move |invocation| {
            log.record(invocation);
            if invocation.input.is_some() {
                return ScriptedGhOutcome::stdout(
                    serde_json::to_vec(&json!({"data":{"repository":{
                        "commit":{"__typename":"Commit","oid":"a".repeat(40)},
                        "source":{"__typename":"Blob","oid":"b".repeat(40),"byteSize":bytes.len()}
                    }}}))
                    .unwrap(),
                );
            }
            ScriptedGhOutcome::stdout(bytes.clone())
        })
    };
    let path = "folder/λ \"quoted\".rs";
    let source = client
        .read_source(ReviewSourceRequest {
            repository: repository(),
            commit: "a".repeat(40),
            path: path.into(),
        })
        .await
        .unwrap();
    assert_eq!(source.bytes, bytes);
    assert_eq!(source.blob, Some("b".repeat(40)));
    let requests = log.requests();
    assert_eq!(requests.len(), 2);
    assert_eq!(
        requests[0].0,
        ["api", "--hostname", "enterprise.example", "graphql"]
    );
    assert_eq!(
        requests[0].1.as_ref().unwrap()["variables"]["expression"],
        format!("{}:{path}", "a".repeat(40))
    );
    assert_eq!(
        requests[1].0,
        [
            "api".to_owned(),
            "--hostname".into(),
            "enterprise.example".into(),
            "--header".into(),
            "Accept: application/vnd.github.raw+json".into(),
            format!("repos/owner/repo/git/blobs/{}", "b".repeat(40)),
        ]
    );
    client.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn immutable_source_absence_never_requests_a_blob() {
    let log = RequestLog::default();
    let client = {
        let log = log.clone();
        GhClient::scripted(move |invocation| {
            log.record(invocation);
            ScriptedGhOutcome::stdout(
                serde_json::to_vec(&json!({"data":{"repository":{
                    "commit":{"__typename":"Commit","oid":"a".repeat(40)},"source":null
                }}}))
                .unwrap(),
            )
        })
    };
    let source = client
        .read_source(ReviewSourceRequest {
            repository: repository(),
            commit: "a".repeat(40),
            path: "absent.rs".into(),
        })
        .await
        .unwrap();
    assert!(source.blob.is_none() && source.bytes.is_empty());
    assert_eq!(log.requests().len(), 1);
    client.shutdown(Duration::from_secs(1)).await.unwrap();
}

/// Serve contributor and collaborator pages, optionally failing either endpoint.
fn users_client(
    contributors: &'static str,
    collaborators: &'static str,
    failures: Arc<Mutex<[Option<&'static str>; 2]>>,
) -> GhClient {
    GhClient::scripted(move |invocation| {
        assert!(invocation.directory.is_absolute());
        let endpoint = argument_text(invocation)[3].clone();
        assert_eq!(
            argument_text(invocation),
            [
                "api",
                "--hostname",
                "enterprise.example",
                endpoint.as_str(),
                "--paginate",
                "--slurp"
            ]
        );
        let (index, body) = if endpoint == "/repos/owner/repo/collaborators?per_page=100" {
            (1, collaborators)
        } else {
            assert_eq!(endpoint, "/repos/owner/repo/contributors?per_page=100");
            (0, contributors)
        };
        match failures.lock().unwrap()[index] {
            Some(message) => ScriptedGhOutcome {
                stdout: body.into(),
                stderr: message.into(),
                exit_code: 1,
            },
            None => ScriptedGhOutcome::stdout(body),
        }
    })
}

#[tokio::test]
async fn repository_users_merge_sources_and_preserve_partial_failure() {
    let failures = Arc::new(Mutex::new([None, None]));
    let client = users_client(
        r#"[[{"login":"Bob"},{"login":"Alice"}]]"#,
        r#"[[{"login":"alice","name":"Alice Developer"},{"login":"carol"}]]"#,
        failures.clone(),
    );
    let users = client.read_repository_users(repository()).await.unwrap();
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
    failures.lock().unwrap()[1] = Some("HTTP 403: Requires repository permission");
    let users = client.read_repository_users(repository()).await.unwrap();
    assert_eq!(users.contributors.len(), 2);
    assert_eq!(users.failure.len(), 1);
    assert!(users.failure[0].message.contains("collaborators"));
    failures.lock().unwrap()[0] = Some("HTTP 404: Not Found");
    let failure = client
        .read_repository_users(repository())
        .await
        .unwrap_err();
    assert!(failure.message.contains("contributors") && failure.message.contains("collaborators"));
    client.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn repository_users_distinguish_empty_pages_from_invalid_pages() {
    let pages = Arc::new(Mutex::new(("[[]]".to_owned(), "[]".to_owned())));
    let client = {
        let pages = pages.clone();
        GhClient::scripted(move |invocation| {
            let pages = pages.lock().unwrap();
            if argument_text(invocation)[3].contains("/collaborators?") {
                ScriptedGhOutcome::stdout(pages.1.clone())
            } else {
                ScriptedGhOutcome::stdout(pages.0.clone())
            }
        })
    };
    assert!(
        client
            .read_repository_users(repository())
            .await
            .unwrap()
            .contributors
            .is_empty()
    );
    for invalid in ["{}", "[[{}]]", "[[{\"login\":\"bad user\"}]]"] {
        *pages.lock().unwrap() = (invalid.to_owned(), invalid.to_owned());
        assert_eq!(
            client
                .read_repository_users(repository())
                .await
                .unwrap_err()
                .kind,
            RemoteFailureKind::InvalidResponse
        );
    }
    let users = serde_json::to_string(&vec![
        (0..101)
            .map(|index| json!({"login":format!("user-{index}")}))
            .collect::<Vec<_>>(),
    ])
    .unwrap();
    *pages.lock().unwrap() = (users, "[]".to_owned());
    assert_eq!(
        client
            .read_repository_users(repository())
            .await
            .unwrap()
            .contributors
            .len(),
        101
    );
    client.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn native_issue_detail_preserves_normalized_records_and_explicit_host_routing() {
    let log = RequestLog::default();
    let client = {
        let log = log.clone();
        GhClient::scripted(move |invocation| {
            log.record(invocation);
            ScriptedGhOutcome::stdout(include_str!("../../../tests/fixtures/detail.json"))
        })
    };
    let request = IssueDetailRequest {
        repository: repository(),
        number: 7,
    };
    let detail = client.read_issue_detail(request.clone()).await.unwrap();
    assert_eq!(detail.repo, "owner/repo");
    assert_eq!(detail.number, 7);
    assert_eq!(detail.comments_count, 1);
    assert!(detail.body.contains('λ'));
    let requests = log.requests();
    assert_eq!(requests.len(), 1);
    let (argument, input) = &requests[0];
    assert_eq!(
        argument[..7],
        [
            "issue",
            "view",
            "7",
            "--repo",
            "enterprise.example/owner/repo",
            "--json",
            argument[6].as_str()
        ]
    );
    assert_eq!(argument.len(), 7);
    assert!(argument[6].split(',').any(|field| field == "body"));
    assert!(argument[6].split(',').any(|field| field == "comments"));
    assert!(input.is_none());
    assert!(
        client
            .read_issue_detail(IssueDetailRequest {
                number: 0,
                ..request
            })
            .await
            .is_err()
    );
    assert_eq!(log.requests().len(), 1);
    client.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn native_detail_failures_preserve_diagnostics_and_never_return_empty_records() {
    let fail = Arc::new(Mutex::new(true));
    let client = {
        let fail = fail.clone();
        GhClient::scripted(move |_| {
            if *fail.lock().unwrap() {
                ScriptedGhOutcome {
                    stdout: b"remote failure body".to_vec(),
                    stderr: b"HTTP 403: API rate limit exceeded".to_vec(),
                    exit_code: 1,
                }
            } else {
                ScriptedGhOutcome::stdout("{}")
            }
        })
    };
    let request = IssueDetailRequest {
        repository: repository(),
        number: 7,
    };
    let failure = client.read_issue_detail(request.clone()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::RateLimited);
    assert!(failure.message.contains("API rate limit exceeded"));
    assert!(failure.message.contains("remote failure body"));
    *fail.lock().unwrap() = false;
    let failure = client.read_issue_detail(request).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::InvalidResponse);
    client.shutdown(Duration::from_secs(1)).await.unwrap();
}

#[tokio::test]
async fn issue_page_failure_and_malformed_json_are_classified_not_empty() {
    let failing = GhClient::scripted(|_| ScriptedGhOutcome {
        stdout: b"{}".to_vec(),
        stderr: b"HTTP 403: API rate limit exceeded".to_vec(),
        exit_code: 1,
    });
    let request = || IssuePageRequest {
        repository: repository(),
        scope: SyncScope::Open,
        incremental: false,
        cursor: None,
    };
    let failure = failing.read_issues(request()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::RateLimited);
    assert!(failure.message.contains("HTTP 403"));
    let malformed = GhClient::scripted(|_| ScriptedGhOutcome::stdout("xxxx"));
    let failure = malformed.read_issues(request()).await.unwrap_err();
    assert_eq!(failure.kind, RemoteFailureKind::InvalidResponse);
    assert!(failure.message.contains("invalid issue JSON"), "{failure}");
}
