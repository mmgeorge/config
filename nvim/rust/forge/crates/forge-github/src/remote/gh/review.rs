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

impl GhClient {
    async fn read_review_at(
        &self,
        directory: PathBuf,
        request: ReviewReadRequest,
    ) -> Result<ReviewPage, RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        let graph = matches!(
            request.view,
            ReviewSection::Threads | ReviewSection::ThreadComments { .. } | ReviewSection::Checks
        );
        let number = request.number;
        let repository = request.repository.repository_name();
        let mut argument = vec![
            "api".into(),
            "--hostname".into(),
            request.repository.hostname().into(),
        ];
        let mut encoded = None;
        let mut collection = false;
        let mut page = 1u32;
        if graph {
            let (owner, name) = repository
                .split_once('/')
                .ok_or_else(|| invalid("repository omits owner"))?;
            let (query, thread) = match &request.view {
                ReviewSection::Threads => (THREAD_QUERY, Value::Null),
                ReviewSection::ThreadComments { thread_node_id } => {
                    (THREAD_COMMENT_QUERY, json!(thread_node_id))
                }
                _ => (CHECK_QUERY, Value::Null),
            };
            encoded = Some(serde_json::to_vec(&json!({"query":query,"variables":{"owner":owner,"name":name,"number":number,"cursor":request.cursor,"thread":thread}})).map_err(|error| invalid(error.to_string()))?);
            argument.push("graphql".into());
        } else {
            page = request
                .cursor
                .as_deref()
                .unwrap_or("1")
                .parse::<u32>()
                .ok()
                .filter(|page| *page > 0 && *page <= 100_001)
                .ok_or_else(|| invalid("invalid REST page cursor"))?;
            let prefix = format!("repos/{repository}");
            let endpoint = match request.view {
                ReviewSection::Overview => format!("{prefix}/pulls/{number}"),
                ReviewSection::Issue => format!("{prefix}/issues/{number}"),
                ReviewSection::RequestedReviewers => {
                    format!("{prefix}/pulls/{number}/requested_reviewers")
                }
                ReviewSection::Diff => {
                    argument.extend([
                        "--header".into(),
                        "Accept: application/vnd.github.diff".into(),
                    ]);
                    format!("{prefix}/pulls/{number}")
                }
                ref view => {
                    collection = true;
                    let (path, filter) = rest_collection(view, number)
                        .expect("non-collection review section reached collection branch");
                    format!("{prefix}/{path}?per_page=100&page={page}{filter}")
                }
            };
            argument.push(endpoint.into());
        }
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
                if matches!(request.view, ReviewSection::Diff) {
                    let diff = String::from_utf8(output.stdout)
                        .map_err(|error| invalid(error.to_string()))?;
                    return Ok(ReviewPage {
                        records: vec![json!({"diff":diff})],
                        next_cursor: None,
                        complete: true,
                    });
                }
                let value: Value = serde_json::from_slice(&output.stdout)
                    .map_err(|error| invalid(error.to_string()))?;
                if graph {
                    return decode_graph(value, &request);
                }
                decode_rest(value, collection, page)
            })
            .await,
        )
    }
}

fn rest_collection(view: &ReviewSection, number: u64) -> Option<(String, &'static str)> {
    match view {
        ReviewSection::Commits => Some((format!("pulls/{number}/commits"), "")),
        ReviewSection::Files => Some((format!("pulls/{number}/files"), "")),
        ReviewSection::Reviews => Some((format!("pulls/{number}/reviews"), "")),
        ReviewSection::ReviewComments => Some((format!("pulls/{number}/comments"), "")),
        ReviewSection::Conversation => Some((format!("issues/{number}/comments"), "")),
        ReviewSection::Milestones => Some(("milestones".into(), "&state=all")),
        ReviewSection::Notifications => Some(("notifications".into(), "&all=true")),
        _ => None,
    }
}

fn decode_rest(value: Value, collection: bool, page: u32) -> Result<ReviewPage, RemoteFailure> {
    let records = if collection {
        value
            .as_array()
            .cloned()
            .ok_or_else(|| invalid("REST collection is not an array"))?
    } else {
        vec![value]
    };
    if records.len() > 100 {
        return Err(invalid("REST page exceeds 100 records"));
    }
    let next_cursor = (collection && records.len() == 100).then(|| (page + 1).to_string());
    Ok(ReviewPage {
        records,
        complete: next_cursor.is_none(),
        next_cursor,
    })
}

fn decode_graph(value: Value, request: &ReviewReadRequest) -> Result<ReviewPage, RemoteFailure> {
    if value
        .get("errors")
        .and_then(Value::as_array)
        .is_some_and(|errors| !errors.is_empty())
    {
        return Err(invalid("GitHub GraphQL returned errors"));
    }
    let connection = match request.view {
        ReviewSection::ThreadComments { .. } => {
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
        ReviewSection::Threads => value.pointer("/data/repository/pullRequest/reviewThreads"),
        ReviewSection::Checks => {
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
        _ => None,
    }
    .ok_or_else(|| invalid("GitHub review connection is missing"))?;
    let records = connection
        .get("nodes")
        .and_then(Value::as_array)
        .cloned()
        .ok_or_else(|| invalid("GitHub connection nodes are missing"))?;
    if records.len() > 100 {
        return Err(invalid("GraphQL page exceeds 100 records"));
    }
    let has_next = connection
        .pointer("/pageInfo/hasNextPage")
        .and_then(Value::as_bool)
        .ok_or_else(|| invalid("GitHub page completion is missing"))?;
    let next_cursor = if has_next {
        Some(
            connection
                .pointer("/pageInfo/endCursor")
                .and_then(Value::as_str)
                .filter(|cursor| !cursor.is_empty() && cursor.len() <= 512)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_query_requests_the_workflow_name_used_by_review_projection() {
        assert!(CHECK_QUERY.contains("checkSuite{workflowRun{workflow{name}}}"));
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
        let page = decode_graph(response.clone(), &request).unwrap();
        assert!(page.complete && page.records.is_empty() && page.next_cursor.is_none());
        assert!(
            decode_graph(
                serde_json::json!({"data":{"repository":{"pullRequest":null}}}),
                &request
            )
            .is_err()
        );
        let mut denied = response;
        denied["errors"] = serde_json::json!([{"message":"permission denied"}]);
        assert!(decode_graph(denied, &request).is_err());
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
        let page = decode_rest(Value::Array(records.clone()), true, 4).unwrap();
        assert_eq!(page.records, records);
        assert_eq!(page.next_cursor.as_deref(), Some("5"));
        assert!(!page.complete);

        let final_page = decode_rest(Value::Array(vec![json!({"sha":"head"})]), true, 5).unwrap();
        assert_eq!(final_page.records, vec![json!({"sha":"head"})]);
        assert!(final_page.next_cursor.is_none() && final_page.complete);
    }

    #[test]
    fn commits_are_a_distinct_serialized_review_section() {
        assert_eq!(
            serde_json::to_value(ReviewSection::Commits).unwrap(),
            json!({"section":"commits"})
        );
        assert_eq!(
            rest_collection(&ReviewSection::Commits, 37),
            Some(("pulls/37/commits".into(), ""))
        );
    }
}

const THREAD_QUERY: &str = "query($owner:String!,$name:String!,$number:Int!,$cursor:String){repository(owner:$owner,name:$name){pullRequest(number:$number){reviewThreads(first:100,after:$cursor){pageInfo{hasNextPage endCursor} nodes{id isResolved isOutdated viewerCanReply viewerCanResolve viewerCanUnresolve path line startLine originalLine originalStartLine diffSide startDiffSide comments(first:1){totalCount pageInfo{hasNextPage endCursor} nodes{id databaseId body viewerDidAuthor path line originalLine startLine originalStartLine diffHunk commit{oid} originalCommit{oid} position createdAt updatedAt url author{login} pullRequestReview{id databaseId state}}}}}}}}";
const THREAD_COMMENT_QUERY: &str = "query($thread:ID!,$cursor:String){node(id:$thread){... on PullRequestReviewThread{pullRequest{number repository{nameWithOwner}} comments(first:100,after:$cursor){pageInfo{hasNextPage endCursor} nodes{id databaseId body viewerDidAuthor path line originalLine startLine originalStartLine diffHunk commit{oid} originalCommit{oid} position createdAt updatedAt url author{login} pullRequestReview{id databaseId state}}}}}}";
const CHECK_QUERY: &str = "query($owner:String!,$name:String!,$number:Int!,$cursor:String){repository(owner:$owner,name:$name){pullRequest(number:$number){commits(last:1){nodes{commit{statusCheckRollup{contexts(first:100,after:$cursor){pageInfo{hasNextPage endCursor} nodes{__typename ... on CheckRun{id name status conclusion detailsUrl startedAt completedAt checkSuite{workflowRun{workflow{name}}}} ... on StatusContext{id context state targetUrl description}}}}}}}}}}";
