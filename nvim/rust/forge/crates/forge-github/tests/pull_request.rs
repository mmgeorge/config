use std::{
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
    time::Duration,
};

use forge_github::metadata::RepositoryUsers;
use forge_github::model::{GithubRepositoryId, IssueDetail};
use forge_github::pull_request::*;
use forge_github::remote::{
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure, RemoteFailureKind,
};
use forge_github::service::GithubService;

#[derive(Clone, Copy)]
enum Failure {
    None,
    RejectSecond,
    ForeignWrite,
}

struct StateRemote {
    state: Mutex<PullRequestState>,
    mutation: Mutex<Vec<PullRequestMutation>>,
    failure: Failure,
}

impl GithubRemote for StateRemote {
    fn read_pull_request(
        &self,
        _: PullRequestTarget,
        _: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>> {
        Box::pin(async { Ok(self.state.lock().unwrap().clone()) })
    }
    fn mutate_pull_request(
        &self,
        _: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>> {
        Box::pin(async move {
            let mut recorded = self.mutation.lock().unwrap();
            recorded.push(mutation.clone());
            if recorded.len() == 2 && matches!(self.failure, Failure::RejectSecond) {
                return PullRequestMutationOutcome::Rejected(RemoteFailure {
                    kind: RemoteFailureKind::Unauthorized,
                    message: "proven rejection".into(),
                });
            }
            let mut state = self.state.lock().unwrap();
            match mutation {
                PullRequestMutation::Close => state.state = PullRequestLifecycle::Closed,
                PullRequestMutation::Reopen => state.state = PullRequestLifecycle::Open,
                PullRequestMutation::Draft => state.is_draft = true,
                PullRequestMutation::Ready => state.is_draft = false,
                PullRequestMutation::Edit(edit) => {
                    if let Some(title) = edit.title {
                        state.title = Some(title);
                    }
                    if let Some(body) = edit.body {
                        state.body = Some(body);
                    }
                }
            }
            let mut observed = state.clone();
            if matches!(self.failure, Failure::ForeignWrite) {
                observed.node_id = "foreign".into();
            }
            PullRequestMutationOutcome::Confirmed(observed)
        })
    }
    fn read_repository_users(
        &self,
        _: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RepositoryUsers, RemoteFailure>> + Send + '_>> {
        panic!("unexpected metadata read")
    }
    fn read_issues(
        &self,
        _: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>> {
        panic!("unexpected issue read")
    }
    fn read_issue_detail(
        &self,
        _: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>> {
        panic!("unexpected detail read")
    }
}

fn remote(state: PullRequestLifecycle, is_draft: bool, failure: Failure) -> Arc<StateRemote> {
    Arc::new(StateRemote {
        state: Mutex::new(PullRequestState {
            node_id: "PR_test".into(),
            number: 7,
            state,
            is_draft,
            title: Some("Initial title".into()),
            body: Some("Initial body".into()),
        }),
        mutation: Mutex::new(Vec::new()),
        failure,
    })
}

fn request(desired: DesiredPullRequestState) -> PullRequestRequest {
    PullRequestRequest {
        target: PullRequestTarget {
            repository: GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
            number: 7,
            node_id: "PR_test".into(),
        },
        request: PullRequestOperation::Transition { desired },
    }
}

#[tokio::test]
async fn field_edits_preserve_omitted_values_clear_empty_body_and_skip_matching_writes() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Open, false, Failure::None);
    let mut request = request(DesiredPullRequestState::Open);
    request.request = PullRequestOperation::Edit {
        edit: PullRequestEdit {
            title: Some("New title".into()),
            body: None,
        },
    };
    let result = service
        .pull_request(remote.clone(), request.clone())
        .await
        .unwrap();
    assert!(result.ok && result.matches_submission == Some(true));
    assert_eq!(
        remote.state.lock().unwrap().body.as_deref(),
        Some("Initial body")
    );
    assert!(
        service
            .pull_request(remote.clone(), request.clone())
            .await
            .unwrap()
            .ok
    );
    assert_eq!(remote.mutation.lock().unwrap().len(), 1);
    request.request = PullRequestOperation::Edit {
        edit: PullRequestEdit {
            title: None,
            body: Some(String::new()),
        },
    };
    assert!(
        service
            .pull_request(remote.clone(), request)
            .await
            .unwrap()
            .ok
    );
    assert_eq!(remote.state.lock().unwrap().body.as_deref(), Some(""));
    assert_eq!(
        remote.state.lock().unwrap().title.as_deref(),
        Some("New title")
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn edit_reconciliation_compares_submitted_fields_without_reposting() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Open, false, Failure::ForeignWrite);
    let edit = PullRequestEdit {
        title: Some("Applied despite broken response".into()),
        body: None,
    };
    let mut request = request(DesiredPullRequestState::Open);
    request.request = PullRequestOperation::Edit { edit: edit.clone() };
    assert_eq!(
        service
            .pull_request(remote.clone(), request.clone())
            .await
            .unwrap()
            .outcome,
        PullRequestOutcome::OutcomeUnknown
    );
    assert!(
        service
            .pull_request(remote.clone(), request.clone())
            .await
            .is_err()
    );
    request.request = PullRequestOperation::ReconcileEdit { edit };
    assert_eq!(
        service
            .pull_request(remote.clone(), request.clone())
            .await
            .unwrap()
            .matches_submission,
        Some(true)
    );
    request.request = PullRequestOperation::ReconcileEdit {
        edit: PullRequestEdit {
            title: Some("Different submission".into()),
            body: None,
        },
    };
    assert_eq!(
        service
            .pull_request(remote.clone(), request)
            .await
            .unwrap()
            .matches_submission,
        Some(false)
    );
    assert_eq!(remote.mutation.lock().unwrap().len(), 1);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn invalid_edit_requests_never_reach_the_remote() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Open, false, Failure::None);
    for edit in [
        PullRequestEdit {
            title: None,
            body: None,
        },
        PullRequestEdit {
            title: Some(" ".into()),
            body: None,
        },
        PullRequestEdit {
            title: Some("first\nsecond".into()),
            body: None,
        },
        PullRequestEdit {
            title: None,
            body: Some("\0".into()),
        },
        PullRequestEdit {
            title: None,
            body: Some("x".repeat(256 * 1024 + 1)),
        },
    ] {
        let mut request = request(DesiredPullRequestState::Open);
        request.request = PullRequestOperation::Edit { edit };
        assert!(service.pull_request(remote.clone(), request).await.is_err());
    }
    assert!(remote.mutation.lock().unwrap().is_empty());
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn closed_draft_reopens_before_ready_and_open_draft_noop_does_not_write() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Closed, true, Failure::None);
    let result = service
        .pull_request(remote.clone(), request(DesiredPullRequestState::Open))
        .await
        .unwrap();
    assert_eq!(result.is_draft, Some(false));
    assert_eq!(
        *remote.mutation.lock().unwrap(),
        [PullRequestMutation::Reopen, PullRequestMutation::Ready]
    );
    let result = service
        .pull_request(remote.clone(), request(DesiredPullRequestState::Open))
        .await
        .unwrap();
    assert!(result.ok);
    assert_eq!(remote.mutation.lock().unwrap().len(), 2);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn proven_second_step_rejection_preserves_confirmed_reopen_state() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Closed, false, Failure::RejectSecond);
    let result = service
        .pull_request(remote.clone(), request(DesiredPullRequestState::Draft))
        .await
        .unwrap();
    assert!(!result.ok);
    assert_eq!(result.outcome, PullRequestOutcome::Rejected);
    assert_eq!(result.state, Some(PullRequestLifecycle::Open));
    assert_eq!(result.is_draft, Some(false));
    assert_eq!(
        *remote.mutation.lock().unwrap(),
        [PullRequestMutation::Reopen, PullRequestMutation::Draft]
    );
    assert!(
        service
            .pull_request(remote.clone(), request(DesiredPullRequestState::Open))
            .await
            .unwrap()
            .ok
    );
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn foreign_mutation_result_blocks_writes_and_reconciliation_never_reposts() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Open, false, Failure::ForeignWrite);
    let result = service
        .pull_request(remote.clone(), request(DesiredPullRequestState::Closed))
        .await
        .unwrap();
    assert_eq!(result.outcome, PullRequestOutcome::OutcomeUnknown);
    assert!(result.state.is_none());
    assert!(
        service
            .pull_request(remote.clone(), request(DesiredPullRequestState::Draft))
            .await
            .is_err()
    );
    let mut reconcile = request(DesiredPullRequestState::Draft);
    reconcile.request = PullRequestOperation::Reconcile;
    let result = service
        .pull_request(remote.clone(), reconcile)
        .await
        .unwrap();
    assert_eq!(result.state, Some(PullRequestLifecycle::Closed));
    assert_eq!(remote.mutation.lock().unwrap().len(), 1);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn invalid_targets_foreign_reads_and_merged_state_never_mutate() {
    let service = GithubService::default();
    let remote = remote(PullRequestLifecycle::Merged, false, Failure::None);
    for desired in [
        DesiredPullRequestState::Open,
        DesiredPullRequestState::Draft,
        DesiredPullRequestState::Closed,
    ] {
        assert_eq!(
            service
                .pull_request(remote.clone(), request(desired))
                .await
                .unwrap()
                .outcome,
            PullRequestOutcome::Rejected
        );
    }
    let mut foreign = request(DesiredPullRequestState::Closed);
    foreign.target.node_id = "foreign".into();
    assert!(service.pull_request(remote.clone(), foreign).await.is_err());
    let mut invalid = request(DesiredPullRequestState::Closed);
    invalid.target.number = 0;
    assert!(service.pull_request(remote.clone(), invalid).await.is_err());
    assert!(remote.mutation.lock().unwrap().is_empty());
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}
