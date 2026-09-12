use super::*;
use crate::model::GithubRepositoryId;
use crate::queue::RemoteCompletion;

fn resource() -> RemoteResource {
    RemoteResource::new(
        GithubRepositoryId::new("github.com", "owner", "repo").unwrap(),
        7,
    )
    .unwrap()
}

#[tokio::test]
async fn comment_uncertainty_survives_unrelated_service_reconciliation() {
    let service = GithubService::default();
    let scope = RemoteScope::Creation("operation-1".into());
    service
        .run_mutation(
            resource(),
            scope.clone(),
            RemoteIntent::Mutation,
            0,
            |mut operation| async move {
                operation.begin_mutation()?;
                operation.complete(RemoteCompletion::Uncertain)
            },
        )
        .await
        .unwrap();
    service
        .run_mutation(
            resource(),
            RemoteScope::PullRequest,
            RemoteIntent::Reconcile,
            0,
            |operation| async move { operation.complete(RemoteCompletion::Reconciled) },
        )
        .await
        .unwrap();
    assert!(
        service
            .run_mutation(
                resource(),
                RemoteScope::PullRequest,
                RemoteIntent::Mutation,
                0,
                |_| async {
                    panic!("unrelated reconciliation admitted a write");
                    #[allow(unreachable_code)]
                    Ok(())
                }
            )
            .await
            .is_err()
    );
    service
        .run_mutation(
            resource(),
            scope,
            RemoteIntent::Reconcile,
            0,
            |operation| async move { operation.complete(RemoteCompletion::Reconciled) },
        )
        .await
        .unwrap();
    service
        .run_mutation(
            resource(),
            RemoteScope::PullRequest,
            RemoteIntent::Mutation,
            0,
            |operation| async move { operation.complete(RemoteCompletion::Confirmed) },
        )
        .await
        .unwrap();
    assert_eq!(
        service
            .shutdown(Duration::from_secs(1))
            .await
            .unfinished_jobs,
        0
    );
}

#[tokio::test]
async fn dropped_comment_receiver_keeps_active_operation_through_shutdown() {
    let service = GithubService::default();
    let worker = service.clone();
    let (started, running) = oneshot::channel();
    let (release, blocked) = oneshot::channel();
    let caller = tokio::spawn(async move {
        worker
            .run_mutation(
                resource(),
                RemoteScope::Comment("COMMENT_one".into()),
                RemoteIntent::Mutation,
                0,
                |mut operation| async move {
                    operation.begin_mutation()?;
                    started.send(()).unwrap();
                    blocked.await.unwrap();
                    operation.complete(RemoteCompletion::Confirmed)
                },
            )
            .await
    });
    running.await.unwrap();
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    assert_eq!(
        service
            .shutdown(Duration::from_millis(10))
            .await
            .unfinished_jobs,
        1
    );
    release.send(()).unwrap();
    assert_eq!(
        service.shutdown(Duration::from_secs(1)).await,
        StorageShutdown {
            unfinished_jobs: 0,
            failed_jobs: 0
        }
    );
}

#[tokio::test]
async fn mutation_failure_evidence_tracks_errors_and_panics_across_dispatch() {
    for began in [false, true] {
        for panicked in [false, true] {
            let service = GithubService::default();
            let failure = service
                .run_mutation(
                    resource(),
                    RemoteScope::PullRequest,
                    RemoteIntent::Mutation,
                    0,
                    move |mut operation| async move {
                        if began {
                            operation.begin_mutation()?;
                        }
                        assert!(!panicked, "injected mutation task panic");
                        Err::<(), _>(anyhow!("injected mutation task failure"))
                    },
                )
                .await
                .unwrap_err();
            assert_eq!(failure.is::<MutationNotStarted>(), !began);
            if !panicked {
                assert!(
                    failure
                        .to_string()
                        .contains("injected mutation task failure")
                );
            }
            let next = service
                .run_mutation(
                    resource(),
                    RemoteScope::PullRequest,
                    RemoteIntent::Mutation,
                    0,
                    |operation| async move { operation.complete(RemoteCompletion::Unchanged) },
                )
                .await;
            if began {
                assert!(next.unwrap_err().is::<MutationNotStarted>());
                service
                    .run_mutation(
                        resource(),
                        RemoteScope::PullRequest,
                        RemoteIntent::Reconcile,
                        0,
                        |operation| async move { operation.complete(RemoteCompletion::Reconciled) },
                    )
                    .await
                    .unwrap();
            } else {
                next.unwrap();
            }
            assert_eq!(
                service
                    .shutdown(Duration::from_secs(1))
                    .await
                    .unfinished_jobs,
                0
            );
        }
    }
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
