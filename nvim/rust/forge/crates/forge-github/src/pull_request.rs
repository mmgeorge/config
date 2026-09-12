use std::sync::Arc;

use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::model::GithubRepositoryId;
use crate::queue::{RemoteCompletion, RemoteOperation};
use crate::remote::{GithubRemote, RemoteFailure};

/// Expected PR identity, validated again against remote truth before a transition.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PullRequestTarget {
    pub repository: GithubRepositoryId,
    pub number: u64,
    pub node_id: String,
}

impl PullRequestTarget {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            self.number > 0 && self.number <= i32::MAX as u64,
            "invalid GitHub pull request number"
        );
        ensure!(
            !self.node_id.is_empty()
                && self.node_id.len() <= 256
                && self.node_id.bytes().all(|byte| byte.is_ascii_graphic()),
            "invalid GitHub pull request node identity"
        );
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum PullRequestLifecycle {
    Open,
    Closed,
    Merged,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum DesiredPullRequestState {
    Open,
    Closed,
    Draft,
}

/// Remote lifecycle observation bound to one node and repository number.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct PullRequestState {
    #[serde(rename = "id")]
    pub node_id: String,
    pub number: u64,
    pub state: PullRequestLifecycle,
    #[serde(rename = "isDraft")]
    pub is_draft: bool,
    /// Title selected by edit or reconciliation reads, absent from lifecycle-only observations.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    /// Raw body selected by edit or reconciliation reads, without Markdown normalization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
}

/// Submitted fields. None leaves a field unchanged, while an empty body clears it.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct PullRequestEdit {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
}

impl PullRequestEdit {
    /// Rejects missing fields, invalid titles, NUL bytes, and text above the admission limits.
    /// Titles accept at most 4 KiB without line breaks. Bodies accept at most 256 KiB and may be empty.
    pub fn validate(&self) -> Result<()> {
        ensure!(
            self.title.is_some() || self.body.is_some(),
            "GitHub PR edit has no submitted fields"
        );
        if let Some(title) = &self.title {
            ensure!(
                !title.trim().is_empty()
                    && title.len() <= 4096
                    && !title.contains(['\r', '\n', '\0']),
                "invalid GitHub PR title"
            );
        }
        if let Some(body) = &self.body {
            ensure!(
                body.len() <= 256 * 1024 && !body.contains('\0'),
                "GitHub PR body exceeds its text limit"
            );
        }
        Ok(())
    }

    fn matches(&self, observed: &PullRequestState) -> bool {
        self.title
            .as_ref()
            .is_none_or(|title| observed.title.as_ref() == Some(title))
            && self
                .body
                .as_ref()
                .is_none_or(|body| observed.body.as_ref() == Some(body))
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        self.title.as_ref().map_or(0, String::capacity)
            + self.body.as_ref().map_or(0, String::capacity)
    }
}

impl PullRequestState {
    pub fn validate(&self, target: &PullRequestTarget) -> Result<()> {
        ensure!(
            self.node_id == target.node_id && self.number == target.number,
            "GitHub returned a different pull request identity"
        );
        ensure!(
            self.title.as_ref().is_none_or(|title| title.len() <= 4096)
                && self
                    .body
                    .as_ref()
                    .is_none_or(|body| body.len() <= 256 * 1024),
            "GitHub returned oversized PR text"
        );
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PullRequestMutation {
    Close,
    Reopen,
    Draft,
    Ready,
    Edit(Box<PullRequestEdit>),
}

/// A write failure is rejected only when the implementation proves it did not take effect.
#[derive(Debug)]
pub enum PullRequestMutationOutcome {
    Confirmed(PullRequestState),
    Rejected(RemoteFailure),
    Uncertain(RemoteFailure),
}

#[derive(Clone, Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum PullRequestOperation {
    Transition { desired: DesiredPullRequestState },
    Edit { edit: PullRequestEdit },
    Reconcile,
    ReconcileEdit { edit: PullRequestEdit },
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PullRequestRequest {
    pub target: PullRequestTarget,
    pub request: PullRequestOperation,
}

#[derive(Clone, Copy, Debug, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum PullRequestOutcome {
    Confirmed,
    Rejected,
    OutcomeUnknown,
    Reconciled,
}

/// Final observation or an explicit unresolved outcome. Unknown writes expose no current state.
#[derive(Debug, Serialize)]
pub struct PullRequestResult {
    pub ok: bool,
    pub outcome: PullRequestOutcome,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<PullRequestLifecycle>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_draft: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// Equality of every submitted field with observed values, absent for lifecycle requests.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub matches_submission: Option<bool>,
    /// Exact remote fields returned by reconciliation for feature-owned saved baselines.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<PullRequestEdit>,
}

/// Reads truth and applies one text edit or at most two lifecycle transitions under one owner.
///
/// Reconciliation performs no mutation. A failed or malformed write is never retried, and an
/// uncertain result prevents later writes until a separate successful reconciliation request.
pub(crate) async fn run_pull_request(
    remote: Arc<dyn GithubRemote>,
    request: PullRequestRequest,
    mut operation: RemoteOperation,
) -> Result<PullRequestResult> {
    let edit = match &request.request {
        PullRequestOperation::Edit { edit } | PullRequestOperation::ReconcileEdit { edit } => {
            Some(edit)
        }
        _ => None,
    };
    let include_text = edit.is_some() || matches!(request.request, PullRequestOperation::Reconcile);
    let mut observed = remote
        .read_pull_request(request.target.clone(), include_text)
        .await?;
    observed.validate(&request.target)?;
    if include_text {
        ensure!(
            observed.title.is_some() && observed.body.is_some(),
            "GitHub PR read omitted editable text"
        );
    }
    if matches!(
        request.request,
        PullRequestOperation::Reconcile | PullRequestOperation::ReconcileEdit { .. }
    ) {
        operation.complete(RemoteCompletion::Reconciled)?;
        return Ok(observation(observed, PullRequestOutcome::Reconciled, edit));
    }
    if observed.state == PullRequestLifecycle::Merged && edit.is_none() {
        operation.complete(RemoteCompletion::Unchanged)?;
        let mut result = observation(observed, PullRequestOutcome::Rejected, edit);
        result.message = Some("A merged pull request cannot change lifecycle state".into());
        return Ok(result);
    }
    for _ in 0..2 {
        let next = match &request.request {
            PullRequestOperation::Transition { desired } => next_transition(&observed, *desired),
            PullRequestOperation::Edit { edit } if !edit.matches(&observed) => {
                Some(PullRequestMutation::Edit(Box::new(edit.clone())))
            }
            _ => None,
        };
        let Some(mutation) = next else {
            operation.complete(RemoteCompletion::Confirmed)?;
            return Ok(observation(observed, PullRequestOutcome::Confirmed, edit));
        };
        operation.begin_mutation()?;
        match remote
            .mutate_pull_request(request.target.clone(), mutation.clone())
            .await
        {
            PullRequestMutationOutcome::Confirmed(updated) => {
                if updated.validate(&request.target).is_err()
                    || !confirms_transition(&updated, &mutation)
                {
                    operation.complete(RemoteCompletion::Uncertain)?;
                    return Ok(uncertain(
                        "GitHub mutation returned an inconsistent pull request state".into(),
                    ));
                }
                observed = updated;
            }
            PullRequestMutationOutcome::Rejected(failure) => {
                operation.complete(RemoteCompletion::Confirmed)?;
                let mut result = observation(observed, PullRequestOutcome::Rejected, edit);
                result.message = Some(failure.to_string());
                return Ok(result);
            }
            PullRequestMutationOutcome::Uncertain(failure) => {
                operation.complete(RemoteCompletion::Uncertain)?;
                return Ok(uncertain(failure.to_string()));
            }
        }
    }
    if let PullRequestOperation::Transition { desired } = request.request {
        ensure!(
            next_transition(&observed, desired).is_none(),
            "GitHub state transition did not converge"
        );
    }
    operation.complete(RemoteCompletion::Confirmed)?;
    Ok(observation(observed, PullRequestOutcome::Confirmed, edit))
}

fn next_transition(
    observed: &PullRequestState,
    desired: DesiredPullRequestState,
) -> Option<PullRequestMutation> {
    match desired {
        DesiredPullRequestState::Closed if observed.state != PullRequestLifecycle::Closed => {
            Some(PullRequestMutation::Close)
        }
        DesiredPullRequestState::Closed => None,
        _ if observed.state == PullRequestLifecycle::Closed => Some(PullRequestMutation::Reopen),
        DesiredPullRequestState::Draft if !observed.is_draft => Some(PullRequestMutation::Draft),
        DesiredPullRequestState::Open if observed.is_draft => Some(PullRequestMutation::Ready),
        _ => None,
    }
}

fn confirms_transition(observed: &PullRequestState, mutation: &PullRequestMutation) -> bool {
    match mutation {
        PullRequestMutation::Close => observed.state == PullRequestLifecycle::Closed,
        PullRequestMutation::Reopen => observed.state == PullRequestLifecycle::Open,
        PullRequestMutation::Draft => {
            observed.state == PullRequestLifecycle::Open && observed.is_draft
        }
        PullRequestMutation::Ready => {
            observed.state == PullRequestLifecycle::Open && !observed.is_draft
        }
        PullRequestMutation::Edit(edit) => edit.matches(observed),
    }
}

fn observation(
    observed: PullRequestState,
    outcome: PullRequestOutcome,
    edit: Option<&PullRequestEdit>,
) -> PullRequestResult {
    PullRequestResult {
        ok: outcome != PullRequestOutcome::Rejected,
        outcome,
        state: Some(observed.state),
        is_draft: Some(observed.is_draft),
        message: None,
        matches_submission: edit.map(|edit| edit.matches(&observed)),
        text: (outcome == PullRequestOutcome::Reconciled).then_some(PullRequestEdit {
            title: observed.title,
            body: observed.body,
        }),
    }
}

fn uncertain(message: String) -> PullRequestResult {
    PullRequestResult {
        ok: false,
        outcome: PullRequestOutcome::OutcomeUnknown,
        state: None,
        is_draft: None,
        message: Some(message),
        matches_submission: None,
        text: None,
    }
}
