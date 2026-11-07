use super::*;
use crate::model::GithubRepositoryId;

fn resource() -> RemoteResource {
    RemoteResource::new(
        GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        7,
    )
    .unwrap()
}

#[tokio::test]
async fn mutation_saturation_and_queued_shutdown_prove_no_dispatch() {
    let service = GithubService::default();
    let mut admitted = Vec::new();
    for _ in 0..MAX_REMOTE_OPERATIONS {
        admitted.push(
            service
                .mutation
                .enqueue(resource(), RemoteIntent::Mutation, 0)
                .unwrap(),
        );
    }
    let failure = service
        .run_mutation(
            resource(),
            RemoteScope::PullRequest,
            RemoteIntent::Mutation,
            0,
            |_| async { Err::<(), _>(anyhow!("unexpected execution")) },
        )
        .await
        .unwrap_err();
    assert!(failure.is::<MutationNotStarted>());
    assert!(failure.to_string().contains("full"));
    admitted.truncate(1);
    let mut queued = Box::pin(service.run_mutation(
        resource(),
        RemoteScope::PullRequest,
        RemoteIntent::Mutation,
        0,
        |_| async { Err::<(), _>(anyhow!("unexpected execution")) },
    ));
    poll_fn(|context| {
        assert!(queued.as_mut().poll(context).is_pending());
        std::task::Poll::Ready(())
    })
    .await;
    service.close();
    let failure = queued.await.unwrap_err();
    assert!(failure.is::<MutationNotStarted>());
    assert!(failure.to_string().contains("closed"));
    drop(admitted);
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}
