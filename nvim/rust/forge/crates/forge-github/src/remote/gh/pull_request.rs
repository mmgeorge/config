use std::{path::PathBuf, process::Output};

use serde::Deserialize;

use super::{
    BoundedVec, GhClient, GraphFailure, append_diagnostic, append_graph_failures, classify_failure,
    diagnostic, remote_result,
};
use crate::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};
use crate::remote::{RemoteFailure, RemoteFailureKind};

const STATE_QUERY: &str = "query($owner:String!, $name:String!, $number:Int!, $includeText:Boolean!) { repository(owner:$owner, name:$name) { pullRequest(number:$number) { id number state isDraft title @include(if:$includeText) body @include(if:$includeText) } } }";

#[derive(Deserialize)]
struct StateResponse {
    data: Option<StateData>,
    #[serde(default)]
    errors: BoundedVec<GraphFailure, 32>,
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

impl GhClient {
    pub(super) async fn read_pr(
        &self,
        directory: PathBuf,
        target: PullRequestTarget,
        include_text: bool,
    ) -> Result<PullRequestState, RemoteFailure> {
        if let Err(failure) = target.validate() {
            return Err(RemoteFailure {
                kind: RemoteFailureKind::InvalidResponse,
                message: failure.to_string(),
            });
        }
        let repository = target.repository.repository_name();
        let (owner, name) = repository
            .split_once('/')
            .expect("validated GitHub repository");
        let encoded = serde_json::to_vec(&serde_json::json!({
            "query": STATE_QUERY, "variables": { "owner": owner, "name": name, "number": target.number, "includeText": include_text }
        })).expect("serialize bounded PR query");
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            target.repository.hostname().into(),
            "graphql".into(),
        ];
        remote_result(
            self.run_native(
                directory,
                argument,
                Some(encoded),
                target.node_id.capacity() + 1024,
                move |output| decode_state(output, &target, false, include_text),
            )
            .await,
        )
    }

    pub(super) async fn mutate_pr(
        &self,
        directory: PathBuf,
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
        let encoded = serde_json::to_vec(&serde_json::json!({
            "query": query, "variables": { "input": variables }
        }))
        .expect("serialize bounded PR mutation");
        drop(variables);
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            target.repository.hostname().into(),
            "graphql".into(),
        ];
        let result = self
            .run_native(
                directory,
                argument,
                Some(encoded),
                target.node_id.capacity() + 1024,
                move |output| decode_state(output, &target, true, include_text),
            )
            .await;
        let rejected = result.as_ref().err().is_some_and(|failure| {
            failure
                .downcast_ref::<forge_git::read_pool::ReadAdmissionError>()
                .is_some()
        });
        match remote_result(result) {
            Ok(observed) => PullRequestMutationOutcome::Confirmed(observed),
            Err(failure) if rejected => PullRequestMutationOutcome::Rejected(failure),
            Err(failure) => PullRequestMutationOutcome::Uncertain(failure),
        }
    }
}

fn decode_state(
    output: Output,
    target: &PullRequestTarget,
    mutation: bool,
    include_text: bool,
) -> Result<PullRequestState, RemoteFailure> {
    let decoded = serde_json::from_slice::<StateResponse>(&output.stdout);
    if !output.status.success() {
        let mut message = diagnostic(&format!(
            "gh exited {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
        if let Ok(response) = decoded {
            append_graph_failures(&mut message, &response.errors.0);
        } else {
            append_diagnostic(&mut message, &String::from_utf8_lossy(&output.stdout));
        }
        return Err(classify_failure(message));
    }
    let response = decoded.map_err(|failure| RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: diagnostic(&format!("gh returned invalid PR state JSON: {failure}")),
    })?;
    if !response.errors.0.is_empty() {
        let mut message = String::new();
        append_graph_failures(&mut message, &response.errors.0);
        return Err(classify_failure(message));
    }
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
            message: "gh returned no pull request state".into(),
        })?;
    observed.validate(target).map_err(|failure| RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: failure.to_string(),
    })?;
    if include_text && (observed.title.is_none() || observed.body.is_none()) {
        return Err(RemoteFailure {
            kind: RemoteFailureKind::InvalidResponse,
            message: "gh omitted requested PR text".into(),
        });
    }
    Ok(observed)
}
