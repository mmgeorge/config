use std::collections::BTreeMap;

use super::ReviewSectionKind;
use anyhow::{Context, Result, ensure};
use forge_github::{
    model::GithubRepositoryId,
    pull_request::{PullRequestEdit, PullRequestTarget},
    review_api::ReviewPage,
};
use serde_json::{Value, json};

/// Converts one status lookup into the native PR fields and complete header pages.
pub(super) fn decode(
    repository: GithubRepositoryId,
    number: u64,
    snapshot: Value,
) -> Result<(
    PullRequestTarget,
    PullRequestEdit,
    BTreeMap<ReviewSectionKind, ReviewPage>,
)> {
    ensure!(
        snapshot["number"].as_u64() == Some(number),
        "PR snapshot number changed"
    );
    let target = PullRequestTarget {
        repository,
        number,
        node_id: required(&snapshot, "id")?.into(),
    };
    let text = PullRequestEdit {
        title: Some(required(&snapshot, "title")?.into()),
        body: Some(required(&snapshot, "body")?.into()),
    };
    let head_repository = snapshot["headRepository"]["nameWithOwner"]
        .as_str()
        .filter(|identity| !identity.is_empty())
        .map(str::to_owned)
        .or_else(|| {
            Some(format!(
                "{}/{}",
                snapshot["headRepositoryOwner"]["login"].as_str()?,
                snapshot["headRepository"]["name"].as_str()?
            ))
        });
    let head = snapshot["commits"].as_array().and_then(|commits| {
        commits
            .iter()
            .find(|commit| commit["oid"] == snapshot["headRefOid"])
    });
    let overview = json!({
        "number": number, "node_id": target.node_id, "title": text.title,
        "body": text.body, "html_url": snapshot["url"],
        "state": snapshot["state"], "draft": snapshot["isDraft"],
        "base": { "sha": snapshot["baseRefOid"], "ref": snapshot["baseRefName"] },
        "head": { "sha": snapshot["headRefOid"], "ref": snapshot["headRefName"], "repo": { "full_name": head_repository } },
        "milestone": snapshot["milestone"], "created_at": snapshot["createdAt"], "updated_at": snapshot["updatedAt"],
        "head_commit": head.map(|commit| json!({ "sha": commit["oid"], "headline": commit["messageHeadline"], "committed_at": commit["committedDate"] })),
    });
    let requests = snapshot["reviewRequests"]
        .as_array()
        .context("PR snapshot omitted reviewer requests")?;
    let mut users = Vec::new();
    let mut teams = Vec::new();
    for request in requests {
        let reviewer = request.get("requestedReviewer").unwrap_or(request);
        if let Some(login) = reviewer["login"].as_str() {
            users.push(json!({ "login": login }));
        } else if let Some(slug) = reviewer["slug"].as_str() {
            teams.push(json!({ "slug": slug }));
        } else {
            anyhow::bail!("PR snapshot contains an unsupported reviewer");
        }
    }
    let page = |record| ReviewPage {
        records: vec![record],
        next_cursor: None,
        complete: true,
    };
    let initial = BTreeMap::from([
        (ReviewSectionKind::Overview, page(overview)),
        (
            ReviewSectionKind::RequestedReviewers,
            page(json!({ "users": users, "teams": teams })),
        ),
    ]);
    Ok((target, text, initial))
}

/// Reads a required string while preserving editable Markdown source exactly.
fn required<'a>(snapshot: &'a Value, field: &str) -> Result<&'a str> {
    snapshot[field]
        .as_str()
        .with_context(|| format!("PR snapshot omitted {field}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_head_name_with_owner_retains_the_fork_identity() {
        let (_, _, pages) = decode(
            GithubRepositoryId::new("github.example", "base-owner", "base-repo").unwrap(),
            7,
            json!({
                "number": 7, "id": "PR_test", "title": "Title", "body": "Body",
                "headRepository": { "nameWithOwner": "", "name": "fork-repo" },
                "headRepositoryOwner": { "login": "fork-owner" },
                "reviewRequests": []
            }),
        )
        .unwrap();
        assert_eq!(
            pages[&ReviewSectionKind::Overview].records[0]["head"]["repo"]["full_name"],
            "fork-owner/fork-repo"
        );
    }
}
