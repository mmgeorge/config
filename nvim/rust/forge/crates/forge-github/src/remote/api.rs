use std::future::Future;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use reqwest::header::{ACCEPT, AUTHORIZATION, CONTENT_TYPE, HeaderMap, LINK, USER_AGENT};
use reqwest::{Method, StatusCode};
use serde::Deserialize;
use serde_json::Value;
use tokio::sync::Semaphore;

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
mod token;
mod users;

use token::TokenStore;

const MAX_DIAGNOSTIC_BYTES: usize = 64 * 1024;
const MAX_CONCURRENT_REQUESTS: u32 = 4;
const REQUEST_TIMEOUT: Duration = Duration::from_secs(120);
const DEFAULT_ACCEPT: &str = "application/vnd.github+json";
const ISSUE_QUERY: &str = "query($owner:String!, $name:String!, $states:[IssueState!], $cursor:String) { rateLimit { remaining } repository(owner:$owner, name:$name) { issues(first:100, after:$cursor, states:$states, orderBy:{field:UPDATED_AT, direction:DESC}) { totalCount pageInfo { hasNextPage endCursor } nodes { number title state url createdAt updatedAt labels(first:50) { nodes { name color description } } } } } }";

/// Sends GitHub REST and GraphQL requests over one pooled HTTPS client.
///
/// At most four requests run at once across all checkouts and hosts. Each request carries the
/// token `gh auth token` reports for its host; `gh` is never launched per request. Dropping a
/// request future cancels it, so admission never outlives its caller. A request whose bytes may
/// have reached GitHub fails as sent, and mutation callers treat that failure as uncertain.
pub struct GithubClient {
    http: reqwest::Client,
    admission: Semaphore,
    closed: AtomicBool,
    tokens: TokenStore,
}

/// Proves that a request never left the host, so a mutation that failed with it was not applied.
#[derive(Debug)]
pub(super) struct RequestNotSent {
    kind: RemoteFailureKind,
    message: String,
}

impl std::fmt::Display for RequestNotSent {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for RequestNotSent {}

/// Reports whether a transport error proves its request never reached GitHub.
pub(super) fn request_not_sent(error: &anyhow::Error) -> bool {
    error.downcast_ref::<RequestNotSent>().is_some()
}

enum ApiEndpoint {
    Graphql,
    Rest(String),
}

/// One GitHub request with explicit host routing.
pub(super) struct ApiRequest {
    hostname: String,
    method: Method,
    endpoint: ApiEndpoint,
    body: Option<Vec<u8>>,
    accept: &'static str,
    all_pages: bool,
}

impl ApiRequest {
    /// Posts one GraphQL document with its variables.
    pub(super) fn graphql(hostname: &str, payload: &Value) -> Self {
        Self {
            hostname: hostname.into(),
            method: Method::POST,
            endpoint: ApiEndpoint::Graphql,
            body: Some(payload.to_string().into_bytes()),
            accept: DEFAULT_ACCEPT,
            all_pages: false,
        }
    }

    /// Reads one REST route, relative to the host's API root, such as `repos/owner/name/pulls/1`.
    pub(super) fn get(hostname: &str, route: impl Into<String>) -> Self {
        Self {
            hostname: hostname.into(),
            method: Method::GET,
            endpoint: ApiEndpoint::Rest(route.into()),
            body: None,
            accept: DEFAULT_ACCEPT,
            all_pages: false,
        }
    }

    pub(super) fn with_method(mut self, method: Method) -> Self {
        self.method = method;
        self
    }

    pub(super) fn with_json(mut self, body: &Value) -> Self {
        self.body = Some(body.to_string().into_bytes());
        self
    }

    pub(super) fn with_accept(mut self, accept: &'static str) -> Self {
        self.accept = accept;
        self
    }

    /// Follows `Link: rel="next"` and returns every page as one JSON array of page bodies.
    pub(super) fn all_pages(mut self) -> Self {
        self.all_pages = true;
        self
    }

    fn url(&self) -> String {
        match &self.endpoint {
            ApiEndpoint::Graphql => graphql_url(&self.hostname),
            ApiEndpoint::Rest(route) => {
                format!(
                    "{}{}",
                    rest_prefix(&self.hostname),
                    route.trim_start_matches('/')
                )
            }
        }
    }
}

/// Returns the REST API root for a GitHub host, ending in `/`.
pub(crate) fn rest_prefix(hostname: &str) -> String {
    if hostname == "github.com" {
        "https://api.github.com/".to_owned()
    } else {
        format!("https://{hostname}/api/v3/")
    }
}

fn graphql_url(hostname: &str) -> String {
    if hostname == "github.com" {
        "https://api.github.com/graphql".to_owned()
    } else {
        format!("https://{hostname}/api/graphql")
    }
}

/// A complete GitHub response, including failures, for its decoder to classify.
pub(super) struct ApiResponse {
    status: StatusCode,
    headers: HeaderMap,
    graphql: bool,
    body: Vec<u8>,
}

impl ApiResponse {
    /// Returns the body of a successful response.
    ///
    /// A non-success status, or a GraphQL response that reports errors, becomes a classified
    /// failure that keeps GitHub's diagnostic text.
    pub(super) fn into_success(self) -> std::result::Result<Vec<u8>, RemoteFailure> {
        let graph_failure = if self.graphql {
            serde_json::from_slice::<GraphFailures>(&self.body)
                .map(|response| response.errors)
                .unwrap_or_default()
        } else {
            Vec::new()
        };
        if self.status.is_success() && graph_failure.is_empty() {
            return Ok(self.body);
        }
        let mut message = if self.status.is_success() {
            String::from("GitHub GraphQL returned errors:")
        } else {
            diagnostic(&format!("GitHub returned HTTP {}", self.status))
        };
        if graph_failure.is_empty() {
            append_diagnostic(&mut message, "\n");
            append_diagnostic(&mut message, &String::from_utf8_lossy(&self.body));
        } else {
            append_graph_failures(&mut message, &graph_failure);
        }
        Err(RemoteFailure {
            kind: classify_failure(self.status, &self.headers, &graph_failure),
            message,
        })
    }

    /// Decodes a successful JSON body.
    pub(super) fn into_json<Decoded: serde::de::DeserializeOwned>(
        self,
    ) -> std::result::Result<Decoded, RemoteFailure> {
        serde_json::from_slice(&self.into_success()?).map_err(|failure| RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: diagnostic(&format!("GitHub returned invalid JSON: {failure}")),
        })
    }
}

#[derive(Deserialize)]
struct GraphFailures {
    #[serde(default)]
    errors: Vec<GraphFailure>,
}

#[derive(Deserialize)]
struct GraphResponse {
    data: Option<GraphData>,
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
    nodes: Vec<GraphIssue>,
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
    nodes: Vec<LabelRecord>,
}

#[derive(Deserialize)]
struct GraphFailure {
    message: String,
    #[serde(rename = "type")]
    kind: Option<String>,
}

impl GithubClient {
    /// Builds the HTTPS client without reading credentials or contacting GitHub.
    pub fn new(gh_executable: PathBuf) -> Result<Self> {
        ensure!(
            !gh_executable.as_os_str().is_empty(),
            "gh executable must not be empty"
        );
        let http = reqwest::Client::builder()
            .timeout(REQUEST_TIMEOUT)
            .build()
            .context("build GitHub HTTP client")?;
        Ok(Self {
            http,
            admission: Semaphore::new(MAX_CONCURRENT_REQUESTS as usize),
            closed: AtomicBool::new(false),
            tokens: TokenStore::new(gh_executable),
        })
    }

    /// Rejects new requests and waits for admitted requests to finish until the deadline.
    pub async fn shutdown(&self, deadline: Duration) -> Result<()> {
        self.closed.store(true, Ordering::SeqCst);
        let drained = tokio::time::timeout(
            deadline,
            self.admission.acquire_many(MAX_CONCURRENT_REQUESTS),
        )
        .await;
        ensure!(
            matches!(drained, Ok(Ok(_))),
            "{} GitHub requests were still running at shutdown",
            MAX_CONCURRENT_REQUESTS as usize - self.admission.available_permits()
        );
        Ok(())
    }

    /// Sends one request and decodes its response.
    ///
    /// The outer error reports a transport failure. It is a [`RequestNotSent`] only when the
    /// request provably never reached GitHub. The inner result is the decoder's verdict on a
    /// complete response.
    pub(super) async fn send<Response>(
        &self,
        request: ApiRequest,
        decode: impl FnOnce(ApiResponse) -> std::result::Result<Response, RemoteFailure>,
    ) -> Result<std::result::Result<Response, RemoteFailure>> {
        let not_sent = |kind, message: String| RequestNotSent { kind, message };
        if self.closed.load(Ordering::SeqCst) {
            return Err(not_sent(
                RemoteFailureKind::Busy,
                "GitHub client is shutting down".into(),
            )
            .into());
        }
        let _permit = self.admission.acquire().await.map_err(|_| {
            not_sent(
                RemoteFailureKind::Busy,
                "GitHub client is shutting down".into(),
            )
        })?;
        if !request.all_pages {
            return Ok(decode(self.exchange(&request, request.url()).await?));
        }
        let api_root = rest_prefix(&request.hostname);
        let mut page_body = Vec::new();
        let mut url = request.url();
        loop {
            let response = self.exchange(&request, url).await?;
            if !response.status.is_success() {
                return Ok(decode(response));
            }
            let next = next_page(&response.headers);
            if !response.body.is_empty() {
                page_body.push(response.body);
            }
            match next {
                Some(next) => {
                    ensure!(
                        next.starts_with(&api_root),
                        "GitHub pagination left the API host: {next}"
                    );
                    url = next;
                }
                None => break,
            }
        }
        let mut body = Vec::with_capacity(page_body.iter().map(Vec::len).sum::<usize>() + 2);
        body.push(b'[');
        for (index, page) in page_body.into_iter().enumerate() {
            if index > 0 {
                body.push(b',');
            }
            body.extend(page);
        }
        body.push(b']');
        Ok(decode(ApiResponse {
            status: StatusCode::OK,
            headers: HeaderMap::new(),
            graphql: false,
            body,
        }))
    }

    /// Sends one HTTP exchange, refreshing a rejected token once.
    ///
    /// GitHub answers 401 before acting on a request, so the single retry is safe for writes.
    async fn exchange(&self, request: &ApiRequest, url: String) -> Result<ApiResponse> {
        let mut refreshed = false;
        loop {
            let token = self.tokens.token(&request.hostname).await?;
            let mut builder = self
                .http
                .request(request.method.clone(), &url)
                .header(USER_AGENT, "forge")
                .header(ACCEPT, request.accept)
                .header("X-GitHub-Api-Version", "2022-11-28")
                .header(AUTHORIZATION, format!("Bearer {token}"));
            if let Some(body) = &request.body {
                builder = builder
                    .header(CONTENT_TYPE, "application/json")
                    .body(body.clone());
            }
            let response = match builder.send().await {
                Ok(response) => response,
                Err(failure) if failure.is_connect() || failure.is_builder() => {
                    return Err(RequestNotSent {
                        kind: RemoteFailureKind::Transport,
                        message: diagnostic(&format!("GitHub request was not sent: {failure}")),
                    }
                    .into());
                }
                Err(failure) => {
                    return Err(anyhow::Error::new(failure).context("GitHub request failed"));
                }
            };
            if response.status() == StatusCode::UNAUTHORIZED && !refreshed {
                self.tokens.invalidate(&request.hostname, &token).await;
                refreshed = true;
                continue;
            }
            let status = response.status();
            let headers = response.headers().clone();
            let body = response
                .bytes()
                .await
                .context("read GitHub response body")?
                .to_vec();
            return Ok(ApiResponse {
                status,
                headers,
                graphql: matches!(request.endpoint, ApiEndpoint::Graphql),
                body,
            });
        }
    }
}

fn next_page(headers: &HeaderMap) -> Option<String> {
    let link = headers.get(LINK)?.to_str().ok()?;
    link.split(',').find_map(|entry| {
        let (target, parameters) = entry.split_once(';')?;
        parameters
            .split(';')
            .any(|parameter| parameter.trim() == "rel=\"next\"")
            .then(|| {
                target
                    .trim()
                    .trim_start_matches('<')
                    .trim_end_matches('>')
                    .to_owned()
            })
    })
}

impl GithubRemote for GithubClient {
    fn read_actor(
        &self,
        repository: crate::model::GithubRepositoryId,
    ) -> Pin<
        Box<
            dyn Future<Output = std::result::Result<super::RemoteActor, RemoteFailure>> + Send + '_,
        >,
    > {
        Box::pin(self.read_actor_at(repository))
    }

    fn read_pull_request(
        &self,
        target: PullRequestTarget,
        include_text: bool,
    ) -> Pin<
        Box<dyn Future<Output = std::result::Result<PullRequestState, RemoteFailure>> + Send + '_>,
    > {
        Box::pin(self.read_pr(target, include_text))
    }

    fn mutate_pull_request(
        &self,
        target: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        Box::pin(self.mutate_pr(target, mutation))
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
        Box::pin(self.read_users(repository))
    }

    fn read_issues(
        &self,
        request: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<IssuePage, RemoteFailure>> + Send + '_>>
    {
        Box::pin(async move { remote_result(self.read_page(request).await) })
    }

    fn read_issue_detail(
        &self,
        request: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<IssueDetail, RemoteFailure>> + Send + '_>>
    {
        Box::pin(async move { remote_result(self.read_detail(request).await) })
    }
}

impl GithubClient {
    async fn read_page(
        &self,
        request: IssuePageRequest,
    ) -> Result<std::result::Result<IssuePage, RemoteFailure>> {
        ensure!(
            request
                .cursor
                .as_ref()
                .is_none_or(|cursor| !cursor.is_empty()),
            "GitHub cursor must not be empty"
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
        let payload = serde_json::json!({
            "query": ISSUE_QUERY, "variables": {"owner": owner, "name": repository, "states": states, "cursor": request.cursor}
        });
        self.send(
            ApiRequest::graphql(request.repository.hostname(), &payload),
            move |response| decode_page(response, &name),
        )
        .await
    }
}

/// Converts a transport failure into a remote failure, keeping the not-sent failure's category.
fn remote_result<Response>(
    result: Result<std::result::Result<Response, RemoteFailure>>,
) -> std::result::Result<Response, RemoteFailure> {
    result.unwrap_or_else(|failure| {
        Err(RemoteFailure {
            kind: failure
                .downcast_ref::<RequestNotSent>()
                .map_or(RemoteFailureKind::Transport, |not_sent| not_sent.kind),
            message: diagnostic(&format!("{failure:#}")),
        })
    })
}

fn decode_page(
    response: ApiResponse,
    repository: &str,
) -> std::result::Result<IssuePage, RemoteFailure> {
    let response: GraphResponse = response.into_json()?;
    let data = response.data.ok_or_else(|| RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: "GitHub returned no GraphQL data".into(),
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
            Some(cursor) if !cursor.is_empty() && !connection.nodes.is_empty() => Some(cursor),
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
            labels: issue.labels.nodes,
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

/// Classifies a failed response from GitHub's documented status codes, rate-limit headers, and
/// GraphQL error types.
fn classify_failure(
    status: StatusCode,
    headers: &HeaderMap,
    graph_failure: &[GraphFailure],
) -> RemoteFailureKind {
    let graph_kind = |kind: &str| {
        graph_failure
            .iter()
            .any(|failure| failure.kind.as_deref() == Some(kind))
    };
    let rate_exhausted = headers
        .get("x-ratelimit-remaining")
        .and_then(|value| value.to_str().ok())
        == Some("0")
        || headers.contains_key("retry-after");
    if status == StatusCode::TOO_MANY_REQUESTS
        || (status == StatusCode::FORBIDDEN && rate_exhausted)
        || graph_kind("RATE_LIMITED")
    {
        RemoteFailureKind::RateLimited
    } else if status == StatusCode::UNAUTHORIZED {
        RemoteFailureKind::Unauthorized
    } else if status == StatusCode::NOT_FOUND || graph_kind("NOT_FOUND") {
        RemoteFailureKind::NotFound
    } else {
        RemoteFailureKind::Transport
    }
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
