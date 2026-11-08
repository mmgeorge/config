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
    GithubRemote, IssueDetailRequest, IssuePage, IssuePageRequest, RemoteFailure,
};
use forge_github::service::GithubService;

#[derive(Clone, Copy)]
enum Failure {
    None,
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
