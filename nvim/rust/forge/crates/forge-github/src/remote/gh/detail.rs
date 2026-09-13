use std::path::PathBuf;
use std::process::Output;

use anyhow::{Result, ensure};
use serde::{Deserialize, Deserializer};

use super::{BoundedVec, GhClient, append_diagnostic, classify_failure, diagnostic};
use crate::model::{ConversationComment, IssueDetail};
use crate::remote::{IssueDetailRequest, RemoteFailure, RemoteFailureKind};

const DETAIL_FIELDS: &str = "id,number,title,body,url,state,author,assignees,labels,milestone,projectItems,comments,createdAt,updatedAt";

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct DetailResponse {
    id: String,
    number: u64,
    title: String,
    body: String,
    url: String,
    state: String,
    author: Option<Login>,
    created_at: String,
    updated_at: String,
    #[serde(deserialize_with = "nullable_collection")]
    labels: BoundedVec<NamedValue, 100>,
    #[serde(deserialize_with = "nullable_collection")]
    assignees: BoundedVec<Login, 100>,
    milestone: Option<TitledValue>,
    #[serde(deserialize_with = "nullable_collection")]
    project_items: BoundedVec<ProjectItem, 100>,
    #[serde(deserialize_with = "nullable_collection")]
    comments: BoundedVec<CommentResponse, 1000>,
}

#[derive(Deserialize)]
struct Login {
    login: String,
}

#[derive(Deserialize)]
struct NamedValue {
    name: String,
}

#[derive(Deserialize)]
struct TitledValue {
    title: String,
}

#[derive(Deserialize)]
struct ProjectItem {
    title: String,
    status: Option<NamedValue>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct CommentResponse {
    body: String,
    author: Option<Login>,
    created_at: String,
    url: String,
}

impl GhClient {
    pub(super) async fn read_detail(
        &self,
        directory: PathBuf,
        request: IssueDetailRequest,
    ) -> Result<std::result::Result<IssueDetail, RemoteFailure>> {
        ensure!(
            request.number > 0 && request.number <= i32::MAX as u64,
            "invalid GitHub issue number"
        );
        let repository = request.repository.repository_name();
        let argument = vec![
            "issue".into(),
            "view".into(),
            request.number.to_string().into(),
            "--repo".into(),
            format!("{}/{}", request.repository.hostname(), repository).into(),
            "--json".into(),
            DETAIL_FIELDS.into(),
        ];
        let decode_bytes = repository.capacity() + request.repository.hostname().len();
        self.run_native(directory, argument, None, decode_bytes, move |output| {
            decode_detail(output, &request)
        })
        .await
    }
}

fn decode_detail(
    output: Output,
    request: &IssueDetailRequest,
) -> std::result::Result<IssueDetail, RemoteFailure> {
    if !output.status.success() {
        let mut message = diagnostic(&format!(
            "gh exited {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
        append_diagnostic(&mut message, &String::from_utf8_lossy(&output.stdout));
        return Err(classify_failure(message));
    }
    let response: DetailResponse = serde_json::from_slice(&output.stdout).map_err(|failure| {
        invalid_response(&format!("gh returned invalid issue detail JSON: {failure}"))
    })?;
    let repository = request.repository.repository_name();
    let expected_url = format!(
        "https://{}/{}/issues/{}",
        request.repository.hostname(),
        repository,
        request.number
    );
    if response.number != request.number
        || response.id.is_empty()
        || response.id.len() > 256
        || !response.id.bytes().all(|byte| byte.is_ascii_graphic())
        || !response.url.eq_ignore_ascii_case(&expected_url)
        || !matches!(response.state.as_str(), "OPEN" | "CLOSED")
        || response.title.trim().is_empty()
        || response.created_at.is_empty()
        || response.updated_at.is_empty()
    {
        return Err(invalid_response(
            "GitHub issue detail has invalid identity, state, title, or timestamps",
        ));
    }
    let mut comments = Vec::with_capacity(response.comments.0.len());
    for comment in response.comments.0 {
        let Some((source, anchor)) = comment.url.split_once("#issuecomment-") else {
            return Err(invalid_response(
                "GitHub issue comment omits its source anchor",
            ));
        };
        if !source.eq_ignore_ascii_case(&expected_url)
            || anchor.is_empty()
            || !anchor.bytes().all(|byte| byte.is_ascii_digit())
            || comment.created_at.is_empty()
        {
            return Err(invalid_response(
                "GitHub issue comment has an invalid source anchor or timestamp",
            ));
        }
        comments.push(ConversationComment {
            body: comment.body,
            author: comment
                .author
                .map_or_else(String::new, |author| author.login),
            created_at: comment.created_at,
            updated_at: String::new(),
            url: comment.url,
        });
    }
    let mut projects = Vec::new();
    for project in response.project_items.0 {
        let status = project
            .status
            .map_or_else(String::new, |status| status.name);
        if project.title.is_empty() {
            continue;
        }
        let name = if status.is_empty() {
            project.title
        } else {
            format!("{} ({status})", project.title)
        };
        if !projects.contains(&name) {
            projects.push(name);
        }
    }
    Ok(IssueDetail {
        kind: "issue".into(),
        node_id: response.id,
        repo: repository,
        number: response.number,
        title: response.title,
        body: response.body,
        url: response.url,
        state: response.state,
        author: response
            .author
            .map_or_else(String::new, |author| author.login),
        created_at: response.created_at,
        updated_at: response.updated_at,
        labels: response
            .labels
            .0
            .into_iter()
            .map(|label| label.name)
            .filter(|name| !name.is_empty())
            .collect(),
        assignees: response
            .assignees
            .0
            .into_iter()
            .map(|assignee| assignee.login)
            .filter(|login| !login.is_empty())
            .collect(),
        milestone: response
            .milestone
            .map_or_else(String::new, |milestone| milestone.title),
        projects,
        comments_count: comments.len(),
        comments,
        subscription: String::new(),
        is_draft: false,
    })
}

fn invalid_response(message: &str) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: diagnostic(message),
    }
}

fn nullable_collection<
    'input,
    Value: Deserialize<'input>,
    Decoder: Deserializer<'input>,
    const LIMIT: usize,
>(
    deserializer: Decoder,
) -> std::result::Result<BoundedVec<Value, LIMIT>, Decoder::Error> {
    Option::<BoundedVec<Value, LIMIT>>::deserialize(deserializer).map(Option::unwrap_or_default)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::GithubRepositoryId;
    use serde_json::{Value, json};
    #[cfg(unix)]
    use std::os::unix::process::ExitStatusExt;
    #[cfg(windows)]
    use std::os::windows::process::ExitStatusExt;

    fn response() -> Value {
        serde_json::from_str(include_str!("../../../tests/fixtures/detail.json")).unwrap()
    }

    fn decode(value: Value) -> std::result::Result<IssueDetail, RemoteFailure> {
        decode_detail(
            Output {
                status: std::process::ExitStatus::from_raw(0),
                stdout: serde_json::to_vec(&value).unwrap(),
                stderr: Vec::new(),
            },
            &IssueDetailRequest {
                repository: GithubRepositoryId::new("enterprise.example", "owner", "repo").unwrap(),
                number: 7,
            },
        )
    }

    #[test]
    fn normalizes_cli_details_without_losing_body_metadata_or_comment_sources() {
        let detail = decode(response()).unwrap();
        assert_eq!(detail.repo, "owner/repo");
        assert_eq!(detail.kind, "issue");
        assert_eq!(detail.node_id, "ISSUE_7");
        assert_eq!(detail.title, "Unicode é issue");
        assert!(detail.body.contains("**Markdown**"));
        assert_eq!(detail.labels, ["bug"]);
        assert_eq!(detail.assignees, ["bob"]);
        assert_eq!(detail.projects, ["Planning (In progress)"]);
        assert_eq!(detail.milestone, "v1.0");
        assert_eq!(detail.comments_count, 1);
        assert_eq!(detail.comments[0].author, "app/automation");
        assert!(detail.comments[0].url.ends_with("#issuecomment-123"));
        assert_eq!(detail.comments[0].updated_at, "");
    }

    #[test]
    fn empty_and_null_collections_are_valid_but_missing_fields_are_not() {
        for empty in [Value::Null, json!([])] {
            let mut value = response();
            for field in ["labels", "assignees", "comments", "projectItems"] {
                value[field] = empty.clone();
            }
            value["author"] = Value::Null;
            value["milestone"] = Value::Null;
            let detail = decode(value).unwrap();
            assert_eq!(detail.comments_count, 0);
            assert!(detail.labels.is_empty());
            assert_eq!(detail.author, "");
        }
        for field in [
            "id",
            "number",
            "body",
            "comments",
            "assignees",
            "labels",
            "projectItems",
            "updatedAt",
        ] {
            let mut value = response();
            value.as_object_mut().unwrap().remove(field);
            assert_eq!(
                decode(value).unwrap_err().kind,
                RemoteFailureKind::InvalidResponse,
                "{field}"
            );
        }
    }

    #[test]
    fn rejects_cross_issue_identity_and_comment_anchors() {
        for (field, replacement) in [
            ("number", json!(8)),
            (
                "url",
                json!("https://elsewhere.example/owner/repo/issues/7"),
            ),
            (
                "url",
                json!("https://enterprise.example/owner/other/issues/7"),
            ),
            ("state", json!("UNKNOWN")),
            ("title", json!("")),
            ("createdAt", json!("")),
        ] {
            let mut value = response();
            value[field] = replacement;
            assert!(decode(value).is_err(), "{field}");
        }
        for anchor in [
            "",
            "https://enterprise.example/owner/repo/issues/8#issuecomment-123",
            "https://enterprise.example/owner/repo/issues/7#issuecomment-x",
        ] {
            let mut value = response();
            value["comments"][0]["url"] = json!(anchor);
            assert!(decode(value).is_err());
        }
    }

    #[test]
    fn collection_limits_fail_instead_of_returning_truncated_detail() {
        for (field, limit) in [
            ("comments", 1000),
            ("labels", 100),
            ("assignees", 100),
            ("projectItems", 100),
        ] {
            let mut value = response();
            let record = value[field][0].clone();
            value[field] = json!(vec![record.clone(); limit]);
            assert!(decode(value.clone()).is_ok(), "{field} at limit");
            value[field] = json!(vec![record; limit + 1]);
            let failure = decode(value).unwrap_err();
            assert_eq!(failure.kind, RemoteFailureKind::InvalidResponse);
            assert!(failure.message.contains(&format!("{limit} records")));
        }
    }
}
