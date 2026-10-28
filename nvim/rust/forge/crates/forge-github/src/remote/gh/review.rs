use std::{future::Future, path::PathBuf, pin::Pin};

use serde_json::{Value, json};

use super::{GhClient, GhDirectory, classify_failure, diagnostic, remote_result};
use crate::remote::{RemoteFailure, RemoteFailureKind};
use crate::review_api::{GithubReviewRemote, ReviewPage, ReviewReadRequest, ReviewSection};

impl GithubReviewRemote for GhClient {
    fn read_commit_detail(
        &self,
        request: crate::review_source::ReviewCommitDetailRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<crate::review_source::ReviewCommitDetail, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(self.read_commit_detail_at(self.directory.clone(), request))
    }
    fn read_source(
        &self,
        request: crate::review_source::ReviewSourceRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<crate::review_source::ReviewSource, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(self.read_source_at(self.directory.clone(), request))
    }
    fn read_comparison(
        &self,
        request: crate::review_source::ReviewComparisonRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<crate::review_source::ReviewComparison, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(self.read_comparison_at(self.directory.clone(), request))
    }
    fn read_review(
        &self,
        request: ReviewReadRequest,
    ) -> Pin<Box<dyn Future<Output = Result<ReviewPage, RemoteFailure>> + Send + '_>> {
        Box::pin(self.read_review_at(self.directory.clone(), request))
    }
}

impl GithubReviewRemote for GhDirectory {
    fn read_commit_detail(
        &self,
        request: crate::review_source::ReviewCommitDetailRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<crate::review_source::ReviewCommitDetail, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(
            self.client
                .read_commit_detail_at(self.directory.clone(), request),
        )
    }
    fn read_source(
        &self,
        request: crate::review_source::ReviewSourceRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<crate::review_source::ReviewSource, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(self.client.read_source_at(self.directory.clone(), request))
    }
    fn read_comparison(
        &self,
        request: crate::review_source::ReviewComparisonRequest,
    ) -> Pin<
        Box<
            dyn Future<Output = Result<crate::review_source::ReviewComparison, RemoteFailure>>
                + Send
                + '_,
        >,
    > {
        Box::pin(
            self.client
                .read_comparison_at(self.directory.clone(), request),
        )
    }
    fn read_review(
        &self,
        request: ReviewReadRequest,
    ) -> Pin<Box<dyn Future<Output = Result<ReviewPage, RemoteFailure>> + Send + '_>> {
        Box::pin(self.client.read_review_at(self.directory.clone(), request))
    }
}

enum ReviewOperation {
    Graph(GraphReview),
    Rest { path: String, kind: RestReview },
}

enum GraphReview {
    EditableFields,
    Threads,
    ThreadComments { thread_node_id: String },
    Checks,
}

enum RestReview {
    Object,
    Collection { filter: &'static str },
    Diff,
}

#[derive(Clone, Copy)]
struct RestPage(std::num::NonZeroU32);

enum ReviewResponse {
    Graph(GraphReview),
    Object,
    Collection(RestPage),
    Diff,
}

impl ReviewOperation {
    fn from_section(view: &ReviewSection, number: u64) -> Self {
        let (path, kind) = match view {
            ReviewSection::EditableFields => return Self::Graph(GraphReview::EditableFields),
            ReviewSection::Threads => return Self::Graph(GraphReview::Threads),
            ReviewSection::ThreadComments { thread_node_id } => {
                return Self::Graph(GraphReview::ThreadComments {
                    thread_node_id: thread_node_id.clone(),
                });
            }
            ReviewSection::Checks => return Self::Graph(GraphReview::Checks),
            ReviewSection::Overview => (format!("pulls/{number}"), RestReview::Object),
            ReviewSection::Issue => (format!("issues/{number}"), RestReview::Object),
            ReviewSection::RequestedReviewers => (
                format!("pulls/{number}/requested_reviewers"),
                RestReview::Object,
            ),
            ReviewSection::Diff => (format!("pulls/{number}"), RestReview::Diff),
            ReviewSection::Commits => (
                format!("pulls/{number}/commits"),
                RestReview::Collection { filter: "" },
            ),
            ReviewSection::Files => (
                format!("pulls/{number}/files"),
                RestReview::Collection { filter: "" },
            ),
            ReviewSection::Reviews => (
                format!("pulls/{number}/reviews"),
                RestReview::Collection { filter: "" },
            ),
            ReviewSection::ReviewComments => (
                format!("pulls/{number}/comments"),
                RestReview::Collection { filter: "" },
            ),
            ReviewSection::Conversation => (
                format!("issues/{number}/comments"),
                RestReview::Collection { filter: "" },
            ),
            ReviewSection::Milestones => (
                "milestones".into(),
                RestReview::Collection {
                    filter: "&state=all",
                },
            ),
            ReviewSection::Notifications => (
                "notifications".into(),
                RestReview::Collection {
                    filter: "&all=true",
                },
            ),
        };
        Self::Rest { path, kind }
    }
}

impl GraphReview {
    fn query(&self) -> &'static str {
        match self {
            Self::EditableFields => {
                "query($owner:String!,$name:String!,$number:Int!){repository(owner:$owner,name:$name){pullRequest(number:$number){id number state isDraft title body}}}"
            }
            Self::Threads => THREAD_QUERY,
            Self::ThreadComments { .. } => THREAD_COMMENT_QUERY,
            Self::Checks => CHECK_QUERY,
        }
    }
}

impl RestPage {
    fn parse(cursor: Option<&str>) -> Result<Self, RemoteFailure> {
        cursor
            .unwrap_or("1")
            .parse::<std::num::NonZeroU32>()
            .map(Self)
            .map_err(|_| invalid("invalid REST page cursor"))
    }

    fn next(self) -> Result<Self, RemoteFailure> {
        self.0
            .checked_add(1)
            .map(Self)
            .ok_or_else(|| invalid("REST page number overflow"))
    }
}

impl GhClient {
    async fn read_review_at(
        &self,
        directory: PathBuf,
        request: ReviewReadRequest,
    ) -> Result<ReviewPage, RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        let mut argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
        ];
        let (encoded, response) = match ReviewOperation::from_section(&request.view, request.number)
        {
            ReviewOperation::Graph(view) => {
                let thread = match &view {
                    GraphReview::ThreadComments { thread_node_id } => json!(thread_node_id),
                    _ => Value::Null,
                };
                let encoded = json!({"query":view.query(),"variables":{
                    "owner":request.repository.owner(), "name":request.repository.name(),
                    "number":request.number, "cursor":request.cursor, "thread":thread
                }})
                .to_string()
                .into_bytes();
                argument.push("graphql".into());
                (Some(encoded), ReviewResponse::Graph(view))
            }
            ReviewOperation::Rest { path, kind } => {
                let page = RestPage::parse(request.cursor.as_deref())?;
                let prefix = format!("repos/{}/{path}", request.repository.repository_name());
                let (endpoint, response) = match kind {
                    RestReview::Object => (prefix, ReviewResponse::Object),
                    RestReview::Collection { filter } => (
                        format!("{prefix}?per_page=100&page={}{filter}", page.0),
                        ReviewResponse::Collection(page),
                    ),
                    RestReview::Diff => {
                        argument.extend([
                            "--header".into(),
                            "Accept: application/vnd.github.diff".into(),
                        ]);
                        (prefix, ReviewResponse::Diff)
                    }
                };
                argument.push(endpoint.into());
                (None, response)
            }
        };
        remote_result(
            self.run_native(directory, argument, encoded, 4096, move |output| {
                if !output.status.success() {
                    return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
                        if output.stderr.is_empty() {
                            &output.stdout
                        } else {
                            &output.stderr
                        },
                    ))));
                }
                match response {
                    ReviewResponse::Diff => {
                        let diff = String::from_utf8(output.stdout)
                            .map_err(|error| invalid(error.to_string()))?;
                        Ok(ReviewPage {
                            records: vec![json!({"diff":diff})],
                            next_cursor: None,
                            complete: true,
                        })
                    }
                    ReviewResponse::Object => {
                        let value: Value = serde_json::from_slice(&output.stdout)
                            .map_err(|error| invalid(error.to_string()))?;
                        Ok(ReviewPage {
                            records: vec![value],
                            next_cursor: None,
                            complete: true,
                        })
                    }
                    ReviewResponse::Collection(page) => {
                        let value: Value = serde_json::from_slice(&output.stdout)
                            .map_err(|error| invalid(error.to_string()))?;
                        decode_collection(value, page)
                    }
                    ReviewResponse::Graph(view) => {
                        let value: Value = serde_json::from_slice(&output.stdout)
                            .map_err(|error| invalid(error.to_string()))?;
                        decode_graph(value, &request, view)
                    }
                }
            })
            .await,
        )
    }
}

fn decode_collection(value: Value, page: RestPage) -> Result<ReviewPage, RemoteFailure> {
    let records = value
        .as_array()
        .cloned()
        .ok_or_else(|| invalid("REST collection is not an array"))?;
    let next_cursor = if records.len() >= crate::review_api::REVIEW_PAGE_SIZE {
        Some(page.next()?.0.to_string())
    } else {
        None
    };
    Ok(ReviewPage {
        records,
        complete: next_cursor.is_none(),
        next_cursor,
    })
}

fn decode_graph(
    value: Value,
    request: &ReviewReadRequest,
    view: GraphReview,
) -> Result<ReviewPage, RemoteFailure> {
    if value
        .get("errors")
        .and_then(Value::as_array)
        .is_some_and(|errors| !errors.is_empty())
    {
        return Err(invalid("GitHub GraphQL returned errors"));
    }
    let connection = match view {
        GraphReview::ThreadComments { .. } => {
            let thread = value
                .pointer("/data/node")
                .ok_or_else(|| invalid("review thread is missing"))?;
            let parent = thread
                .get("pullRequest")
                .ok_or_else(|| invalid("thread parent is missing"))?;
            if parent.get("number").and_then(Value::as_u64) != Some(request.number)
                || parent
                    .pointer("/repository/nameWithOwner")
                    .and_then(Value::as_str)
                    .is_none_or(|name| {
                        !name.eq_ignore_ascii_case(&request.repository.repository_name())
                    })
            {
                return Err(invalid("thread belongs to another pull request"));
            }
            thread.get("comments")
        }
        GraphReview::Threads => value.pointer("/data/repository/pullRequest/reviewThreads"),
        GraphReview::Checks => {
            let rollup = value
                .pointer("/data/repository/pullRequest/commits/nodes/0/commit/statusCheckRollup");
            if rollup == Some(&Value::Null) {
                return Ok(ReviewPage {
                    records: vec![],
                    next_cursor: None,
                    complete: true,
                });
            }
            rollup.and_then(|rollup| rollup.get("contexts"))
        }
        GraphReview::EditableFields => {
            return value
                .pointer("/data/repository/pullRequest")
                .filter(|record| record.is_object())
                .map(|record| ReviewPage {
                    records: vec![record.clone()],
                    next_cursor: None,
                    complete: true,
                })
                .ok_or_else(|| invalid("GitHub pull request is missing"));
        }
    }
    .ok_or_else(|| invalid("GitHub review connection is missing"))?;
    let records = connection
        .get("nodes")
        .and_then(Value::as_array)
        .cloned()
        .ok_or_else(|| invalid("GitHub connection nodes are missing"))?;
    let has_next = connection
        .pointer("/pageInfo/hasNextPage")
        .and_then(Value::as_bool)
        .ok_or_else(|| invalid("GitHub page completion is missing"))?;
    let next_cursor = if has_next {
        Some(
            connection
                .pointer("/pageInfo/endCursor")
                .and_then(Value::as_str)
                .filter(|cursor| !cursor.is_empty())
                .ok_or_else(|| invalid("GitHub incomplete page has no cursor"))?
                .to_owned(),
        )
    } else {
        None
    };
    Ok(ReviewPage {
        records,
        next_cursor,
        complete: !has_next,
    })
}

fn invalid(message: impl Into<String>) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: message.into(),
    }
}

const THREAD_QUERY: &str = "query($owner:String!,$name:String!,$number:Int!,$cursor:String){repository(owner:$owner,name:$name){pullRequest(number:$number){reviewThreads(first:100,after:$cursor){pageInfo{hasNextPage endCursor} nodes{id isResolved isOutdated viewerCanReply viewerCanResolve viewerCanUnresolve path line startLine originalLine originalStartLine diffSide startDiffSide comments(first:1){totalCount pageInfo{hasNextPage endCursor} nodes{id databaseId body viewerDidAuthor path line originalLine startLine originalStartLine diffHunk commit{oid} originalCommit{oid} position createdAt updatedAt url author{login} pullRequestReview{id databaseId state}}}}}}}}";
const THREAD_COMMENT_QUERY: &str = "query($thread:ID!,$cursor:String){node(id:$thread){... on PullRequestReviewThread{pullRequest{number repository{nameWithOwner}} comments(first:100,after:$cursor){pageInfo{hasNextPage endCursor} nodes{id databaseId body viewerDidAuthor path line originalLine startLine originalStartLine diffHunk commit{oid} originalCommit{oid} position createdAt updatedAt url author{login} pullRequestReview{id databaseId state}}}}}}";
const CHECK_QUERY: &str = "query($owner:String!,$name:String!,$number:Int!,$cursor:String){repository(owner:$owner,name:$name){pullRequest(number:$number){commits(last:1){nodes{commit{statusCheckRollup{contexts(first:100,after:$cursor){pageInfo{hasNextPage endCursor} nodes{__typename ... on CheckRun{id name status conclusion detailsUrl startedAt completedAt checkSuite{workflowRun{workflow{name}}}} ... on StatusContext{id context state targetUrl description}}}}}}}}}}";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_query_requests_the_workflow_name_used_by_review_projection() {
        assert!(CHECK_QUERY.contains("checkSuite{workflowRun{workflow{name}}}"));
    }

    #[test]
    fn editable_fields_preserve_markdown_and_reject_missing_pr() {
        let request = ReviewReadRequest {
            repository: crate::model::GithubRepositoryId::new("github.com", "owner", "repo")
                .unwrap(),
            number: 7,
            view: ReviewSection::EditableFields,
            cursor: None,
        };
        let record = json!({"id":"PR_test","number":7,"state":"OPEN","isDraft":true,
            "title":"Title","body":"# Body\n\n```ts\nconst value = 1;\n```"});
        let page = decode_graph(
            json!({"data":{"repository":{"pullRequest":record}}}),
            &request,
            GraphReview::EditableFields,
        )
        .unwrap();
        assert_eq!(page.records, vec![record]);
        assert!(page.complete && page.next_cursor.is_none());
        assert!(
            decode_graph(
                json!({"data":{"repository":{"pullRequest":null}}}),
                &request,
                GraphReview::EditableFields
            )
            .is_err()
        );
        assert!(
            decode_graph(
                json!({"errors":[{"message":"denied"}]}),
                &request,
                GraphReview::EditableFields
            )
            .is_err()
        );
    }

    #[test]
    fn checks_without_a_rollup_are_complete_but_missing_parent_and_errors_fail() {
        let request = ReviewReadRequest {
            repository: crate::model::GithubRepositoryId::new("github.com", "owner", "repo")
                .unwrap(),
            number: 7,
            view: ReviewSection::Checks,
            cursor: None,
        };
        let response = serde_json::json!({"data":{"repository":{"pullRequest":{"commits":{"nodes":[{"commit":{"statusCheckRollup":null}}]}}}}});
        let page = decode_graph(response.clone(), &request, GraphReview::Checks).unwrap();
        assert!(page.complete && page.records.is_empty() && page.next_cursor.is_none());
        assert!(
            decode_graph(
                serde_json::json!({"data":{"repository":{"pullRequest":null}}}),
                &request,
                GraphReview::Checks
            )
            .is_err()
        );
        let mut denied = response;
        denied["errors"] = serde_json::json!([{"message":"permission denied"}]);
        assert!(decode_graph(denied, &request, GraphReview::Checks).is_err());
    }

    #[test]
    fn rest_commit_pages_preserve_records_and_advance_only_at_the_page_bound() {
        let records = (0..100)
            .map(|index| {
                json!({
                    "sha": format!("{index:040}"),
                    "commit": {
                        "message": format!("subject {index}\n\nbody"),
                        "committer": { "date": "2026-06-14T03:20:00Z" }
                    }
                })
            })
            .collect::<Vec<_>>();
        let page = decode_collection(
            Value::Array(records.clone()),
            RestPage::parse(Some("4")).unwrap(),
        )
        .unwrap();
        assert_eq!(page.records, records);
        assert_eq!(page.next_cursor.as_deref(), Some("5"));
        assert!(!page.complete);

        let final_page = decode_collection(
            Value::Array(vec![json!({"sha":"head"})]),
            RestPage::parse(Some("5")).unwrap(),
        )
        .unwrap();
        assert_eq!(final_page.records, vec![json!({"sha":"head"})]);
        assert!(final_page.next_cursor.is_none() && final_page.complete);
    }

    #[test]
    fn commits_are_a_distinct_serialized_review_section() {
        assert_eq!(
            serde_json::to_value(ReviewSection::Commits).unwrap(),
            json!({"section":"commits"})
        );
        let ReviewOperation::Rest {
            path,
            kind: RestReview::Collection { filter },
        } = ReviewOperation::from_section(&ReviewSection::Commits, 37)
        else {
            panic!("commits must use a REST collection")
        };
        assert_eq!(path, "pulls/37/commits");
        assert_eq!(filter, "");
    }
}
