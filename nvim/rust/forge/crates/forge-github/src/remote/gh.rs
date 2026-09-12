use std::ffi::OsString;
use std::fmt;
use std::future::Future;
use std::io::Write;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::pin::Pin;
use std::process::{Command, Output};
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use forge_git::command::{CommandLimits, read_command};
use forge_git::read_pool::{BlockingReadPool, ReadAdmissionError};
use serde::de::{SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};

use super::{
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure, RemoteFailureKind,
};
use crate::model::{IssueDetail, IssueRecord, LabelRecord, SyncScope};
use crate::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};

mod actor;
mod comment;
mod creation;
pub use creation::CreationContext;
mod detail;
mod notification;
mod pull_request;
mod review;
mod review_mutation;
mod source;
mod submission;
pub use submission::{PendingReviewExpected, ReviewSubmissionContext};
mod users;

const MAX_REQUEST_BYTES: usize = 16 * 1024 * 1024;
const MAX_RESPONSE_BYTES: usize = 8 * 1024 * 1024;
const MAX_DIAGNOSTIC_BYTES: usize = 64 * 1024;
const ISSUE_QUERY: &str = "query($owner:String!, $name:String!, $states:[IssueState!], $cursor:String) { rateLimit { remaining } repository(owner:$owner, name:$name) { issues(first:100, after:$cursor, states:$states, orderBy:{field:UPDATED_AT, direction:DESC}) { totalCount pageInfo { hasNextPage endCursor } nodes { number title state url createdAt updatedAt labels(first:50) { nodes { name color description } } } } } }";

/// Bounded gh requests with inherited authentication and explicit per-request host routing.
///
/// Four native jobs retain admission through process exit, pipe drainage, decode, and result
/// collection. Dropped futures signal cancellation but do not release live native ownership.
/// Mutation services retain their futures until collection and treat interrupted writes as uncertain.
pub struct GhClient {
    executable: PathBuf,
    directory: PathBuf,
    requests: BlockingReadPool,
}

/// Checkout context that exposes GitHub capabilities while sharing one native process owner.
pub struct GhDirectory {
    client: Arc<GhClient>,
    directory: PathBuf,
}

#[derive(Deserialize)]
struct GraphResponse {
    data: Option<GraphData>,
    #[serde(default)]
    errors: BoundedVec<GraphFailure, 32>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GraphData {
    repository: Option<GraphRepository>,
    rate_limit: Option<GraphRateLimit>,
}

#[derive(Deserialize)]
struct GraphRateLimit {
    remaining: u64,
}

#[derive(Deserialize)]
struct GraphRepository {
    issues: GraphConnection,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GraphConnection {
    total_count: u64,
    page_info: GraphPageInfo,
    nodes: BoundedVec<GraphIssue, 100>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GraphPageInfo {
    has_next_page: bool,
    end_cursor: Option<String>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct GraphIssue {
    number: u64,
    title: String,
    state: String,
    url: String,
    created_at: String,
    updated_at: String,
    labels: GraphLabels,
}

#[derive(Deserialize)]
struct GraphLabels {
    nodes: BoundedVec<LabelRecord, 50>,
}

#[derive(Deserialize)]
struct GraphFailure {
    message: String,
    #[serde(rename = "type")]
    kind: Option<String>,
}

struct BoundedVec<Value, const LIMIT: usize>(Vec<Value>);

impl GhClient {
    /// Binds a checkout context while sharing this owner's global native request admission.
    pub fn for_directory(self: &Arc<Self>, directory: PathBuf) -> Result<Arc<GhDirectory>> {
        ensure!(
            directory.as_os_str().len() <= 16 * 1024,
            "gh checkout path exceeds its input limit"
        );
        Ok(Arc::new(GhDirectory {
            client: Arc::clone(self),
            directory: std::path::absolute(directory)?,
        }))
    }

    /// Validates local launch configuration without starting a process or reading credentials.
    pub fn new(executable: PathBuf, directory: PathBuf) -> Result<Self> {
        ensure!(
            !executable.as_os_str().is_empty(),
            "gh executable must not be empty"
        );
        ensure!(
            executable.as_os_str().len() <= 16 * 1024 && directory.as_os_str().len() <= 16 * 1024,
            "gh launch path exceeds its input limit"
        );
        Ok(Self {
            executable,
            directory: std::path::absolute(directory)?,
            requests: BlockingReadPool::new(4, MAX_REQUEST_BYTES)?,
        })
    }

    /// Cancels admitted requests and reports native work that still owns resources at the deadline.
    pub async fn shutdown(&self, deadline: Duration) -> Result<()> {
        let outcome = self.requests.shutdown(deadline).await;
        ensure!(
            outcome.unfinished.is_empty(),
            "{} gh requests still own native resources",
            outcome.unfinished.len()
        );
        Ok(())
    }
}

impl GithubRemote for GhClient {
    fn read_actor(
        &self,
        repository: crate::model::GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<Output = std::result::Result<super::RemoteActor, RemoteFailure>> + Send + '_,
        >,
    > {
        Box::pin(self.read_actor_at(self.directory.clone(), repository))
    }
    fn read_pull_request(
        &self,
        target: PullRequestTarget,
        include_text: bool,
    ) -> Pin<
        Box<dyn Future<Output = std::result::Result<PullRequestState, RemoteFailure>> + Send + '_>,
    > {
        Box::pin(self.read_pr(self.directory.clone(), target, include_text))
    }

    fn mutate_pull_request(
        &self,
        target: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        Box::pin(self.mutate_pr(self.directory.clone(), target, mutation))
    }

    fn read_repository_users(
        &self,
        repository: crate::model::GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = std::result::Result<crate::metadata::RepositoryUsers, RemoteFailure>,
                > + Send
                + '_,
        >,
    > {
        Box::pin(self.read_users(self.directory.clone(), repository))
    }

    fn read_issues(
        &self,
        request: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<IssuePage, RemoteFailure>> + Send + '_>>
    {
        Box::pin(
            async move { remote_result(self.read_page(self.directory.clone(), request).await) },
        )
    }

    fn read_issue_detail(
        &self,
        request: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<IssueDetail, RemoteFailure>> + Send + '_>>
    {
        Box::pin(
            async move { remote_result(self.read_detail(self.directory.clone(), request).await) },
        )
    }
}

impl GithubRemote for GhDirectory {
    fn read_actor(
        &self,
        repository: crate::model::GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<Output = std::result::Result<super::RemoteActor, RemoteFailure>> + Send + '_,
        >,
    > {
        Box::pin(
            self.client
                .read_actor_at(self.directory.clone(), repository),
        )
    }
    fn read_pull_request(
        &self,
        target: PullRequestTarget,
        include_text: bool,
    ) -> Pin<
        Box<dyn Future<Output = std::result::Result<PullRequestState, RemoteFailure>> + Send + '_>,
    > {
        Box::pin(
            self.client
                .read_pr(self.directory.clone(), target, include_text),
        )
    }

    fn mutate_pull_request(
        &self,
        target: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        Box::pin(
            self.client
                .mutate_pr(self.directory.clone(), target, mutation),
        )
    }

    fn read_repository_users(
        &self,
        repository: crate::model::GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = std::result::Result<crate::metadata::RepositoryUsers, RemoteFailure>,
                > + Send
                + '_,
        >,
    > {
        Box::pin(self.client.read_users(self.directory.clone(), repository))
    }

    fn read_issues(
        &self,
        request: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<IssuePage, RemoteFailure>> + Send + '_>>
    {
        Box::pin(async move {
            remote_result(self.client.read_page(self.directory.clone(), request).await)
        })
    }

    fn read_issue_detail(
        &self,
        request: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<IssueDetail, RemoteFailure>> + Send + '_>>
    {
        Box::pin(async move {
            remote_result(
                self.client
                    .read_detail(self.directory.clone(), request)
                    .await,
            )
        })
    }
}

impl GhClient {
    async fn read_page(
        &self,
        directory: PathBuf,
        request: IssuePageRequest,
    ) -> Result<std::result::Result<IssuePage, RemoteFailure>> {
        ensure!(
            request
                .cursor
                .as_ref()
                .is_none_or(|cursor| !cursor.is_empty() && cursor.len() <= 512),
            "GitHub cursor exceeds its request limit"
        );
        let name = request.repository.repository_name();
        let (owner, repository) = name
            .split_once('/')
            .context("GitHub repository identity omits its owner")?;
        let states: &[&str] = if request.incremental || matches!(request.scope, SyncScope::All) {
            &["OPEN", "CLOSED"]
        } else {
            &["OPEN"]
        };
        let encoded = serde_json::to_vec(&serde_json::json!({
            "query": ISSUE_QUERY, "variables": {"owner": owner, "name": repository, "states": states, "cursor": request.cursor}
        }))?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
            "graphql".into(),
        ];
        self.run_native(
            directory,
            argument,
            Some(encoded),
            name.capacity(),
            move |output| decode_page(output, &name),
        )
        .await
    }

    async fn run_native<Response: Send + 'static>(
        &self,
        directory: PathBuf,
        argument: Vec<OsString>,
        encoded: Option<Vec<u8>>,
        decode_bytes: usize,
        decode: impl FnOnce(Output) -> std::result::Result<Response, RemoteFailure> + Send + 'static,
    ) -> Result<std::result::Result<Response, RemoteFailure>> {
        let executable = self.executable.clone();
        let input_bytes = encoded.as_ref().map_or(0, Vec::capacity)
            + executable.as_os_str().len()
            + directory.as_os_str().len()
            + argument
                .iter()
                .map(|value| value.as_os_str().len())
                .sum::<usize>()
            + decode_bytes;
        self.requests
            .submit(input_bytes, move |cancellation| {
                let mut command = Command::new(executable);
                command.current_dir(directory).args(argument);
                let _input = if let Some(encoded) = encoded {
                    let mut input =
                        tempfile::NamedTempFile::new().context("create gh request input")?;
                    input.write_all(&encoded)?;
                    input.flush()?;
                    command.arg("--input").arg(input.path());
                    Some(input)
                } else {
                    None
                };
                cancellation.check()?;
                #[cfg(windows)]
                {
                    use std::os::windows::process::CommandExt;
                    command.creation_flags(0x08000000);
                }
                let output = read_command(
                    &mut command,
                    CommandLimits {
                        stdout_bytes: MAX_RESPONSE_BYTES,
                        stderr_bytes: MAX_DIAGNOSTIC_BYTES,
                        timeout: Duration::from_secs(120),
                    },
                    || cancellation.check(),
                )?;
                cancellation.check()?;
                Ok(decode(output))
            })?
            .finish()
            .await
    }
}

fn remote_result<Response>(
    result: Result<std::result::Result<Response, RemoteFailure>>,
) -> std::result::Result<Response, RemoteFailure> {
    result.unwrap_or_else(|failure| {
        Err(RemoteFailure {
            kind: if matches!(
                failure.downcast_ref::<ReadAdmissionError>(),
                Some(ReadAdmissionError::Busy)
            ) {
                RemoteFailureKind::Busy
            } else {
                RemoteFailureKind::Transport
            },
            message: diagnostic(&format!("{failure:#}")),
        })
    })
}

fn decode_page(output: Output, repository: &str) -> std::result::Result<IssuePage, RemoteFailure> {
    let decoded = serde_json::from_slice::<GraphResponse>(&output.stdout);
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let mut message = diagnostic(&format!("gh exited {}: {stderr}", output.status));
        if let Ok(response) = decoded {
            append_graph_failures(&mut message, &response.errors.0);
        } else {
            append_diagnostic(&mut message, &String::from_utf8_lossy(&output.stdout));
        }
        return Err(classify_failure(message));
    }
    let response = decoded.map_err(|failure| RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: diagnostic(&format!(
            "gh returned invalid issue JSON: {failure}; {}",
            String::from_utf8_lossy(&output.stderr)
        )),
    })?;
    if !response.errors.0.is_empty() {
        let mut message = String::new();
        append_graph_failures(&mut message, &response.errors.0);
        return Err(classify_failure(message));
    }
    let data = response.data.ok_or_else(|| RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: "gh returned no GraphQL data".into(),
    })?;
    let connection = data
        .repository
        .ok_or_else(|| RemoteFailure {
            kind: RemoteFailureKind::NotFound,
            message: "GitHub returned no repository".into(),
        })?
        .issues;
    let next_cursor = if connection.page_info.has_next_page {
        match connection.page_info.end_cursor {
            Some(cursor)
                if !cursor.is_empty() && cursor.len() <= 512 && !connection.nodes.0.is_empty() =>
            {
                Some(cursor)
            }
            _ => {
                return Err(RemoteFailure {
                    kind: RemoteFailureKind::InvalidResponse,
                    message: "GitHub issue pagination omits a valid next cursor or records".into(),
                });
            }
        }
    } else {
        None
    };
    let issues = connection
        .nodes
        .0
        .into_iter()
        .map(|issue| IssueRecord {
            repo: repository.into(),
            number: issue.number,
            title: issue.title,
            state: issue.state,
            url: issue.url,
            created_at: Some(issue.created_at),
            updated_at: Some(issue.updated_at),
            body: None,
            labels: issue.labels.nodes.0,
        })
        .collect();
    Ok(IssuePage {
        issues,
        next_cursor,
        total_count: Some(connection.total_count),
        rate_remaining: data.rate_limit.map(|rate| rate.remaining),
    })
}

fn append_graph_failures(message: &mut String, failure: &[GraphFailure]) {
    for failure in failure {
        if message.len() >= MAX_DIAGNOSTIC_BYTES {
            break;
        }
        append_diagnostic(message, "\n");
        append_diagnostic(message, failure.kind.as_deref().unwrap_or("GraphQL"));
        append_diagnostic(message, ": ");
        append_diagnostic(message, &failure.message);
    }
}

fn append_diagnostic(message: &mut String, addition: &str) {
    let suffix = "\n[diagnostic truncated]";
    if message.ends_with(suffix) {
        return;
    }
    if message.len() + addition.len() <= MAX_DIAGNOSTIC_BYTES {
        message.push_str(addition);
        return;
    }
    let prefix_limit = MAX_DIAGNOSTIC_BYTES - suffix.len();
    if message.len() > prefix_limit {
        let mut boundary = prefix_limit;
        while !message.is_char_boundary(boundary) {
            boundary -= 1;
        }
        message.truncate(boundary);
    }
    let mut boundary = addition.len().min(prefix_limit - message.len());
    while !addition.is_char_boundary(boundary) {
        boundary -= 1;
    }
    message.push_str(&addition[..boundary]);
    message.push_str(suffix);
}

fn classify_failure(message: String) -> RemoteFailure {
    let lower = message.to_ascii_lowercase();
    let kind = if lower.contains("rate limit") || lower.contains("rate_limit") {
        RemoteFailureKind::RateLimited
    } else if lower.contains("http 401")
        || lower.contains("bad credentials")
        || lower.contains("unauthorized")
    {
        RemoteFailureKind::Unauthorized
    } else if lower.contains("not_found")
        || lower.contains("not found")
        || lower.contains("http 404")
        || lower.contains("could not resolve to a repository")
    {
        RemoteFailureKind::NotFound
    } else {
        RemoteFailureKind::Transport
    };
    RemoteFailure { kind, message }
}

fn diagnostic(message: &str) -> String {
    if message.len() <= MAX_DIAGNOSTIC_BYTES {
        return message.into();
    }
    let suffix = "\n[diagnostic truncated]";
    let mut boundary = MAX_DIAGNOSTIC_BYTES - suffix.len();
    while !message.is_char_boundary(boundary) {
        boundary -= 1;
    }
    format!("{}{suffix}", &message[..boundary])
}

impl<Value, const LIMIT: usize> Default for BoundedVec<Value, LIMIT> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<'input, Value: Deserialize<'input>, const LIMIT: usize> Deserialize<'input>
    for BoundedVec<Value, LIMIT>
{
    fn deserialize<Decoder: Deserializer<'input>>(
        decoder: Decoder,
    ) -> std::result::Result<Self, Decoder::Error> {
        struct BoundedVisitor<Value, const LIMIT: usize>(PhantomData<Value>);
        impl<'input, Value: Deserialize<'input>, const LIMIT: usize> Visitor<'input>
            for BoundedVisitor<Value, LIMIT>
        {
            type Value = BoundedVec<Value, LIMIT>;
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "at most {LIMIT} records")
            }
            fn visit_seq<Sequence: SeqAccess<'input>>(
                self,
                mut sequence: Sequence,
            ) -> std::result::Result<Self::Value, Sequence::Error> {
                let mut values = Vec::with_capacity(sequence.size_hint().unwrap_or(0).min(LIMIT));
                for _ in 0..LIMIT {
                    match sequence.next_element()? {
                        Some(value) => values.push(value),
                        None => return Ok(BoundedVec(values)),
                    }
                }
                if sequence.next_element::<serde::de::IgnoredAny>()?.is_some() {
                    return Err(serde::de::Error::custom(format!(
                        "response exceeds {LIMIT} records"
                    )));
                }
                Ok(BoundedVec(values))
            }
        }
        decoder.deserialize_seq(BoundedVisitor::<Value, LIMIT>(PhantomData))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(unix)]
    use std::os::unix::process::ExitStatusExt;
    #[cfg(windows)]
    use std::os::windows::process::ExitStatusExt;

    fn output(value: serde_json::Value) -> Output {
        Output {
            status: std::process::ExitStatus::from_raw(0),
            stdout: serde_json::to_vec(&value).unwrap(),
            stderr: Vec::new(),
        }
    }

    #[test]
    fn empty_connection_is_success_but_missing_data_and_fields_are_failures() {
        let page = decode_page(
            output(serde_json::json!({"data":{"repository":{"issues":{
                "totalCount":0,"nodes":[],"pageInfo":{"hasNextPage":false,"endCursor":null}
            }}}})),
            "owner/repo",
        )
        .unwrap();
        assert!(page.issues.is_empty());
        assert_eq!(page.total_count, Some(0));
        for response in [
            serde_json::json!({}),
            serde_json::json!({"data":{}}),
            serde_json::json!({"data":{"repository":{"issues":{}}}}),
        ] {
            assert!(decode_page(output(response), "owner/repo").is_err());
        }
    }

    #[test]
    fn graphql_errors_preserve_categories_and_never_publish_partial_data() {
        for (kind, expected) in [
            ("RATE_LIMITED", RemoteFailureKind::RateLimited),
            ("NOT_FOUND", RemoteFailureKind::NotFound),
            ("UNAUTHORIZED", RemoteFailureKind::Unauthorized),
        ] {
            let failure = decode_page(output(serde_json::json!({"errors":[{"type":kind,"message":"API diagnostic"}],"data":null})), "owner/repo").unwrap_err();
            assert_eq!(failure.kind, expected);
            assert!(failure.message.contains("API diagnostic"));
        }
    }

    #[test]
    fn decoded_collections_stop_at_their_record_limit() {
        let record = serde_json::json!({"number":1,"title":"title","state":"OPEN","url":"https://github.com/owner/repo/issues/1",
            "createdAt":"2026-09-01T00:00:00Z","updatedAt":"2026-09-07T00:00:00Z","labels":{"nodes":[]}});
        let response = |records| {
            serde_json::json!({"data":{"repository":{"issues":{"totalCount":records,
            "nodes":vec![record.clone();records],"pageInfo":{"hasNextPage":false,"endCursor":null}}}}})
        };
        assert_eq!(
            decode_page(output(response(100)), "owner/repo")
                .unwrap()
                .issues
                .len(),
            100
        );
        let failure = decode_page(output(response(101)), "owner/repo").unwrap_err();
        assert_eq!(failure.kind, RemoteFailureKind::InvalidResponse);
        assert!(failure.message.contains("100 records"));
        let labels = serde_json::json!({"nodes":vec![serde_json::json!({"name":"label"});51]});
        assert!(serde_json::from_value::<GraphLabels>(labels).is_err());
        assert!(
            serde_json::from_value::<BoundedVec<GraphFailure, 32>>(serde_json::json!(vec![
                serde_json::json!({"message":"failure"});
                33
            ]))
            .is_err()
        );
    }

    #[test]
    fn diagnostics_preserve_utf8_and_mark_truncation() {
        let oversized = "é".repeat(MAX_DIAGNOSTIC_BYTES);
        let failure = decode_page(
            output(serde_json::json!({"errors":[{"type":"RATE_LIMITED","message":oversized}]})),
            "owner/repo",
        )
        .unwrap_err();
        assert_eq!(failure.kind, RemoteFailureKind::RateLimited);
        assert!(failure.message.len() <= MAX_DIAGNOSTIC_BYTES);
        assert!(failure.message.ends_with("[diagnostic truncated]"));
    }
}
