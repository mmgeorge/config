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
use crate::remote::{GithubRemote, IssuePageRequest, RemoteFailureKind};
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
