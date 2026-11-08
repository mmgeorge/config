use serde::Deserialize;

use super::{ApiRequest, ApiResponse, GithubClient, remote_result, request_not_sent};
use crate::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};
use crate::remote::{RemoteFailure, RemoteFailureKind};

const STATE_QUERY: &str = "query($owner:String!, $name:String!, $number:Int!, $includeText:Boolean!) { repository(owner:$owner, name:$name) { pullRequest(number:$number) { id number state isDraft title @include(if:$includeText) body @include(if:$includeText) } } }";

#[derive(Deserialize)]
struct StateResponse {
    data: Option<StateData>,
}

#[derive(Deserialize)]
struct StateData {
    repository: Option<StateContainer>,
    transition: Option<StateContainer>,
}

#[derive(Deserialize)]
struct StateContainer {
    #[serde(rename = "pullRequest")]
    pull_request: Option<PullRequestState>,
}

impl GithubClient {
    pub(super) async fn read_pr(
        &self,
        target: PullRequestTarget,
        include_text: bool,
    ) -> Result<PullRequestState, RemoteFailure> {
        if let Err(failure) = target.validate() {
            return Err(RemoteFailure {
                kind: RemoteFailureKind::InvalidResponse,
                message: failure.to_string(),
            });
        }
        let owner = target.repository.owner();
        let name = target.repository.name();
        let payload = serde_json::json!({
            "query": STATE_QUERY, "variables": { "owner": owner, "name": name, "number": target.number, "includeText": include_text }
        });
        remote_result(
            self.send(
                ApiRequest::graphql(target.repository.hostname(), &payload),
                move |response| decode_state(response, &target, false, include_text),
            )
            .await,
        )
    }

    pub(super) async fn mutate_pr(
        &self,
        target: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> PullRequestMutationOutcome {
        if let Err(failure) = target.validate() {
            return PullRequestMutationOutcome::Rejected(RemoteFailure {
                kind: RemoteFailureKind::InvalidResponse,
                message: failure.to_string(),
            });
        }
        if let PullRequestMutation::Edit(edit) = &mutation
            && let Err(failure) = edit.validate()
        {
            return PullRequestMutationOutcome::Rejected(RemoteFailure {
                kind: RemoteFailureKind::InvalidResponse,
                message: failure.to_string(),
            });
        }
        let mut variables = match &mutation {
            PullRequestMutation::Edit(edit) => {
                serde_json::to_value(edit).expect("serialize PR fields")
            }
            _ => serde_json::json!({}),
        };
        variables["pullRequestId"] = serde_json::json!(&target.node_id);
        let include_text = matches!(mutation, PullRequestMutation::Edit(_));
        let content = if include_text { " title body" } else { "" };
        let (field, input) = match mutation {
            PullRequestMutation::Close => ("closePullRequest", "ClosePullRequestInput"),
            PullRequestMutation::Reopen => ("reopenPullRequest", "ReopenPullRequestInput"),
            PullRequestMutation::Draft => (
                "convertPullRequestToDraft",
                "ConvertPullRequestToDraftInput",
            ),
            PullRequestMutation::Ready => (
                "markPullRequestReadyForReview",
                "MarkPullRequestReadyForReviewInput",
            ),
            PullRequestMutation::Edit(_) => ("updatePullRequest", "UpdatePullRequestInput"),
        };
        let query = format!(
            "mutation($input:{input}!) {{ transition: {field}(input:$input) {{ pullRequest {{ id number state isDraft{content} }} }} }}"
        );
        let payload = serde_json::json!({
            "query": query, "variables": { "input": variables }
        });
        drop(variables);
        let result = self
            .send(
                ApiRequest::graphql(target.repository.hostname(), &payload),
                move |response| decode_state(response, &target, true, include_text),
            )
            .await;
        let rejected = result.as_ref().err().is_some_and(request_not_sent);
        match remote_result(result) {
            Ok(observed) => PullRequestMutationOutcome::Confirmed(observed),
            Err(failure) if rejected => PullRequestMutationOutcome::Rejected(failure),
            Err(failure) => PullRequestMutationOutcome::Uncertain(failure),
        }
    }
}

fn decode_state(
    response: ApiResponse,
    target: &PullRequestTarget,
    mutation: bool,
    include_text: bool,
) -> Result<PullRequestState, RemoteFailure> {
    let response: StateResponse = response.into_json()?;
    let observed = response
        .data
        .and_then(|data| {
            if mutation {
                data.transition
            } else {
                data.repository
            }
        })
        .and_then(|container| container.pull_request)
        .ok_or_else(|| RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: "GitHub returned no pull request state".into(),
        })?;
    observed.validate(target).map_err(|failure| RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: failure.to_string(),
    })?;
    if include_text && (observed.title.is_none() || observed.body.is_none()) {
        return Err(RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: "GitHub omitted requested PR text".into(),
        });
    }
    Ok(observed)
}
