use anyhow::{Result, ensure};
use serde::{Deserialize, Deserializer};
use serde_json::{Value, json};

use super::{ApiRequest, GithubClient, diagnostic, remote_result};
use crate::model::{ConversationComment, IssueDetail};
use crate::remote::{IssueDetailRequest, RemoteFailure, RemoteFailureKind};

const DETAIL_QUERY: &str = "query($owner:String!,$name:String!,$number:Int!){repository(owner:$owner,name:$name){issue(number:$number){id number title body url state createdAt updatedAt author{login} milestone{title} labels(first:100){pageInfo{hasNextPage endCursor} nodes{name}} assignees(first:100){pageInfo{hasNextPage endCursor} nodes{login}} projectItems(first:100){pageInfo{hasNextPage endCursor} nodes{project{title} status:fieldValueByName(name:\"Status\"){... on ProjectV2ItemFieldSingleSelectValue{name}}}} comments(first:100){pageInfo{hasNextPage endCursor} nodes{body createdAt url author{login}}}}}}";

/// Each issue collection with the node selection its continuation pages request.
const DETAIL_CONNECTIONS: [(&str, &str); 4] = [
    ("labels", "name"),
    ("assignees", "login"),
    (
        "projectItems",
        "project{title} status:fieldValueByName(name:\"Status\"){... on ProjectV2ItemFieldSingleSelectValue{name}}",
    ),
    ("comments", "body createdAt url author{login}"),
];

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
    labels: Vec<NamedValue>,
    #[serde(deserialize_with = "nullable_collection")]
    assignees: Vec<Login>,
    milestone: Option<TitledValue>,
    #[serde(deserialize_with = "nullable_collection")]
    project_items: Vec<ProjectItem>,
    #[serde(deserialize_with = "nullable_collection")]
    comments: Vec<CommentResponse>,
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

impl GithubClient {
    pub(super) async fn read_detail(
        &self,
        request: IssueDetailRequest,
    ) -> Result<std::result::Result<IssueDetail, RemoteFailure>> {
        ensure!(
            request.number > 0 && request.number <= i32::MAX as u64,
            "invalid GitHub issue number"
        );
        let hostname = request.repository.hostname();
        let payload = json!({"query":DETAIL_QUERY,"variables":{
            "owner":request.repository.owner(),"name":request.repository.name(),"number":request.number
        }});
        let mut issue = match self
            .send(ApiRequest::graphql(hostname, &payload), |response| {
                let mut value: Value = response.into_json()?;
                match value.pointer_mut("/data/repository/issue").map(Value::take) {
                    Some(issue @ Value::Object(_)) => Ok(issue),
                    _ => Err(RemoteFailure {
                        kind: RemoteFailureKind::NotFound,
                        message: format!("GitHub returned no issue #{}", request.number),
                    }),
                }
            })
            .await?
        {
            Ok(issue) => issue,
            Err(failure) => return Ok(Err(failure)),
        };
        for (connection, selection) in DETAIL_CONNECTIONS {
            match self
                .complete_connection(&mut issue, hostname, connection, selection)
                .await
            {
                Ok(()) => {}
                Err(failure) => return Ok(Err(failure)),
            }
        }
        flatten_project_items(&mut issue);
        Ok(serde_json::from_value::<DetailResponse>(issue)
            .map_err(|failure| {
                invalid_response(&format!(
                    "GitHub returned invalid issue detail JSON: {failure}"
                ))
            })
            .and_then(|response| decode_detail(response, &request)))
    }

    /// Replaces an issue connection with the complete array of its nodes, reading every
    /// continuation page through the issue's node identity.
    async fn complete_connection(
        &self,
        issue: &mut Value,
        hostname: &str,
        connection: &str,
        selection: &str,
    ) -> std::result::Result<(), RemoteFailure> {
        let issue_node_id = issue
            .get("id")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
        let mut page = issue
            .get_mut(connection)
            .map(Value::take)
            .unwrap_or(Value::Null);
        if page.is_null() {
            return Ok(());
        }
        let mut node = Vec::new();
        loop {
            node.extend(
                page.get_mut("nodes")
                    .map(Value::take)
                    .and_then(|nodes| match nodes {
                        Value::Array(nodes) => Some(nodes),
                        _ => None,
                    })
                    .ok_or_else(|| invalid_response("GitHub issue connection omits its nodes"))?,
            );
            let has_next = page
                .pointer("/pageInfo/hasNextPage")
                .and_then(Value::as_bool)
                .ok_or_else(|| invalid_response("GitHub issue connection omits page state"))?;
            if !has_next {
                break;
            }
            let cursor = page
                .pointer("/pageInfo/endCursor")
                .and_then(Value::as_str)
                .filter(|cursor| !cursor.is_empty())
                .ok_or_else(|| invalid_response("GitHub incomplete issue page has no cursor"))?
                .to_owned();
            let query = format!(
                "query($id:ID!,$cursor:String){{node(id:$id){{... on Issue{{{connection}(first:100,after:$cursor){{pageInfo{{hasNextPage endCursor}} nodes{{{selection}}}}}}}}}}}"
            );
            let payload = json!({"query":query,"variables":{"id":issue_node_id,"cursor":cursor}});
            page = remote_result(
                self.send(ApiRequest::graphql(hostname, &payload), |response| {
                    let mut value: Value = response.into_json()?;
                    value
                        .pointer_mut(&format!("/data/node/{connection}"))
                        .map(Value::take)
                        .filter(Value::is_object)
                        .ok_or_else(|| {
                            invalid_response("GitHub issue continuation page is missing")
                        })
                })
                .await,
            )?;
        }
        issue[connection] = Value::Array(node);
        Ok(())
    }
}

/// Reshapes project items into `{title, status}` records with an optional single-select status.
fn flatten_project_items(issue: &mut Value) {
    let Some(Value::Array(item)) = issue.get_mut("projectItems") else {
        return;
    };
    for item in item {
        let title = item
            .pointer("/project/title")
            .cloned()
            .unwrap_or(Value::Null);
        let status = item
            .pointer("/status/name")
            .filter(|name| name.is_string())
            .map_or(Value::Null, |name| json!({"name": name}));
        *item = json!({"title": title, "status": status});
    }
}

fn decode_detail(
    response: DetailResponse,
    request: &IssueDetailRequest,
) -> std::result::Result<IssueDetail, RemoteFailure> {
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
    let mut comments = Vec::with_capacity(response.comments.len());
    for comment in response.comments {
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
    for project in response.project_items {
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
            .into_iter()
            .map(|label| label.name)
            .filter(|name| !name.is_empty())
            .collect(),
        assignees: response
            .assignees
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

fn nullable_collection<'input, Value: Deserialize<'input>, Decoder: Deserializer<'input>>(
    deserializer: Decoder,
) -> std::result::Result<Vec<Value>, Decoder::Error> {
    Option::<Vec<Value>>::deserialize(deserializer).map(Option::unwrap_or_default)
}
