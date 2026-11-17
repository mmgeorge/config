use super::*;
use crate::provider::ScopeOwner;
use crate::provider::submission::SubmissionStore;
use crate::provider::{PromptMode, ProviderInput, ServiceTier};
use crate::session::ApprovalMode;

/// Persist actual child mutation facts separately when the canonical native outcome is failure.
#[tokio::test]
async fn committed_child_file_change_survives_native_error() -> Result<()> {
    let directory = tempfile::tempdir()?;
    crate::storage::layout::prepare(directory.path())?;
    let path = directory.path().join("changed.rs");
    std::fs::write(&path, "old")?;
    let context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
    let slot = context.provider_completion.clone();
    let retained = slot.clone();
    let changed_path = path.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().unwrap(),
        )?,
        move |request, _writer, start, _cancel| {
            let slot = retained.clone();
            let path = changed_path.clone();
            async move {
                start.observed(1).await?;
                std::fs::write(&path, "new").map_err(crate::provider::test_support::io_failure)?;
                let mut completion = ProviderCompletion::default();
                completion
                    .host_file_changes
                    .push(crate::tools::lsp::HostFileChange {
                        reservation: None,
                        scope: request.scope.clone(),
                        call_id: forge_provider_api::control::ToolCallId::new("committed-call")
                            .unwrap(),
                        native_item_id: None,
                        changes: forge_provider_api::ProviderChangeSet {
                            file: vec![forge_provider_api::ProviderFileChange {
                                path: path.to_string_lossy().into_owned(),
                                move_path: None,
                                kind: forge_provider_api::ProviderChangeKind::Update,
                                diff: "@@ -1 +1 @@\n-old\n+new\n".into(),
                            }],
                        },
                    });
                slot.publish(completion)?;
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Failed {
                        error: forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Rejected,
                        ),
                        retry: forge_provider_api::turn::RetryDecision::NotAllowed,
                    },
                ))
            }
        },
    );
    let RuntimeFixture {
        runtime,
        coordinator,
        admitted,
        cancel: _cancel,
    } = RuntimeFixture::new(directory.path(), native, "child", Some(context))?;
    let owner = admitted.owner().clone();
    runtime.run().await?;
    let (output, changes) = loop {
        match coordinator
            .try_receive()
            .context("child completion missing")?
        {
            AgentCommand::Completed {
                output,
                host_file_changes,
                ..
            } => break (output, host_file_changes),
            AgentCommand::Event { .. } => {}
            AgentCommand::Request { .. } => anyhow::bail!("fixture requested unrelated work"),
        }
    };
    assert!(output.is_err());
    assert_eq!(changes.len(), 1);
    assert_eq!(changes[0].scope.owner(), owner);
    assert_eq!(changes[0].scope.exchange_id.as_str(), admitted.task.id);
    assert_eq!(changes[0].call_id.as_str(), "committed-call");
    assert!(changes[0].native_item_id.is_none());
    assert_eq!(std::fs::read_to_string(path)?, "new");
    assert!(slot.preview()?.batches.is_empty());
    assert_eq!(slot.collected_file_changes(), changes);
    Ok(())
}

/// Own one admitted child runtime and the coordinator that receives its completion.
struct RuntimeFixture {
    runtime: AgentRuntime,
    coordinator: Arc<crate::agent::AgentCoordinator>,
    #[allow(
        dead_code,
        reason = "a fixture's admitted work is read only by some cases"
    )]
    admitted: crate::agent::AgentWork,
    cancel: watch::Sender<bool>,
}

/// Hold the services one broker shares across all of its child runtimes.
#[derive(Clone)]
struct RuntimeServices {
    coordinator: Arc<crate::agent::AgentCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
    submission_store: Arc<SubmissionStore>,
    permission: Arc<crate::provider::approval::PermissionCoordinator>,
}

impl RuntimeServices {
    /// Open the services a broker would own for the session rooted at `directory`.
    fn open(directory: &std::path::Path) -> Result<Self> {
        let trace = Arc::new(crate::trace::TraceStore::open(directory)?);
        Ok(Self {
            coordinator: crate::agent::AgentCoordinator::new("session".into()),
            submission_store: SubmissionStore::open(directory, trace.clone())?,
            permission: crate::provider::approval::PermissionCoordinator::load(
                directory.join("permissions.json"),
                directory,
            )?,
            trace,
        })
    }
}

impl RuntimeFixture {
    /// Build a runtime for one newly admitted child of a fresh coordinator.
    fn new(
        directory: &std::path::Path,
        native: Arc<crate::provider::test_support::EventFixtureProvider>,
        name: &str,
        context: Option<crate::tools::ControlTurnContext>,
    ) -> Result<Self> {
        Self::with_services(
            directory,
            native,
            name,
            context,
            &RuntimeServices::open(directory)?,
        )
    }

    /// Build a runtime for one newly admitted child of the coordinator in `services`.
    fn with_services(
        directory: &std::path::Path,
        native: Arc<crate::provider::test_support::EventFixtureProvider>,
        name: &str,
        context: Option<crate::tools::ControlTurnContext>,
        services: &RuntimeServices,
    ) -> Result<Self> {
        let session = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let provider = session.provider_handle();
        let coordinator = services.coordinator.clone();
        let trace = services.trace.clone();
        let (cancel, cancellation) = watch::channel(false);
        let admitted =
            crate::agent::coordinator_test::admit(&coordinator, &format!("{name}-operation"), name);
        let owner = admitted.owner().clone();
        let runtime = AgentRuntime {
            execution_registry: Arc::new(
                crate::provider::session_execution::SessionExecutionRegistry::new(
                    provider.clone(),
                    trace.clone(),
                ),
            ),
            provider,
            tool_timeout: Default::default(),
            trace,
            submission_store: services.submission_store.clone(),
            permission: services.permission.clone(),
            request: ProviderRequest {
                dispatch: Default::default(),
                execution_owner: Some(owner),
                harness_session_id: "session".into(),
                workspace: directory.to_string_lossy().into_owned(),
                input: ProviderInput::from_user_text("change"),
                mode: PromptMode::Chat,
                model: "fixture".into(),
                effort: "low".into(),
                context_window: None,
                service_tier: ServiceTier::Standard,
                access: Default::default(),
                approval_mode: ApprovalMode::Ask,
                provider_session_id: None,
                submission: None,
                acknowledged_child_reports: Vec::new(),
                recovered_input: Vec::new(),
                control_context: context,
                child_agent: None,
            },
            task_id: admitted.task.id.clone(),
            cancellation,
            sender: coordinator.sender(),
            lsp_lease: None,
        };
        Ok(Self {
            runtime,
            coordinator,
            admitted,
            cancel,
        })
    }
}

/// Receive the single completion a runtime reports, skipping its forwarded events.
fn completion(
    coordinator: &crate::agent::AgentCoordinator,
) -> Result<(Box<Result<ProviderCompletion>>, Option<String>)> {
    let mut completed = None;
    while let Some(command) = coordinator.try_receive() {
        match command {
            AgentCommand::Completed {
                output,
                cleanup_error,
                ..
            } => {
                anyhow::ensure!(completed.is_none(), "child reported a second completion");
                completed = Some((output, cleanup_error));
            }
            AgentCommand::Event { .. } => {}
            AgentCommand::Request { .. } => anyhow::bail!("fixture requested unrelated work"),
        }
    }
    completed.context("child completion missing")
}

/// Build a fixture provider that counts starts and holds each turn until `release` fires.
fn held_provider(
    started: Arc<std::sync::atomic::AtomicUsize>,
    arrived: Arc<tokio::sync::Notify>,
    release: Arc<tokio::sync::Notify>,
) -> Result<Arc<crate::provider::test_support::EventFixtureProvider>> {
    Ok(crate::provider::test_support::EventFixtureProvider::new(
        forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().unwrap(),
        )?,
        move |request, _writer, start, _cancel| {
            let started = started.clone();
            let arrived = arrived.clone();
            let release = release.clone();
            async move {
                start.observed(1).await?;
                let released = release.notified();
                tokio::pin!(released);
                released.as_mut().enable();
                started.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
                arrived.notify_one();
                released.await;
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                ))
            }
        },
    ))
}

/// A task cancelled before repository admission completes once as cancelled, starts no
/// provider, and reports no fabricated cleanup failure.
#[tokio::test]
async fn cancel_before_repository_admission_is_cancelled() -> Result<()> {
    let directory = tempfile::tempdir()?;
    crate::storage::layout::prepare(directory.path())?;
    let started = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let native = held_provider(
        started.clone(),
        Arc::new(tokio::sync::Notify::new()),
        Arc::new(tokio::sync::Notify::new()),
    )?;
    let fixture = RuntimeFixture::new(directory.path(), native, "cancelled-child", None)?;
    fixture.cancel.send_replace(true);
    let coordinator = fixture.coordinator.clone();
    fixture.runtime.run().await?;
    let (output, cleanup_error) = completion(&coordinator)?;
    let error = output
        .err()
        .context("a cancelled task cannot complete successfully")?;
    assert!(error.is::<crate::broker::TurnCancelled>(), "{error:#}");
    assert_eq!(cleanup_error, None);
    assert_eq!(started.load(std::sync::atomic::Ordering::Acquire), 0);
    Ok(())
}

/// Write-off children share the repository, so a second reader starts while the first runs.
///
/// Every child previously took the exclusive writer reservation, so explorers that could not
/// write still ran one at a time.
#[tokio::test]
async fn read_only_children_start_before_any_child_finishes() -> Result<()> {
    let directory = tempfile::tempdir()?;
    crate::storage::layout::prepare(directory.path())?;
    let started = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let arrived = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Notify::new());
    let services = RuntimeServices::open(directory.path())?;
    let mut runs = Vec::new();
    // Start each child only after the previous one holds its reservation inside its provider.
    // Starting them together would race on the reservation's admission gate, whose busy retry
    // sleeps on the real clock; this order keeps every acquisition uncontended while a child
    // that needed exclusive access would still wait behind the readers already running.
    for name in ["reader-one", "reader-two"] {
        let mut fixture = RuntimeFixture::with_services(
            directory.path(),
            held_provider(started.clone(), arrived.clone(), release.clone())?,
            name,
            None,
            &services,
        )?;
        fixture.runtime.request.access.write_access = crate::session::WriteScope::Off;
        let expected = runs.len() + 1;
        let arrival = async {
            loop {
                let notified = arrived.notified();
                if started.load(std::sync::atomic::Ordering::Acquire) == expected {
                    break;
                }
                notified.await;
            }
        };
        tokio::pin!(arrival);
        // Register interest before the child can arrive, so its notification is not missed.
        assert!(futures_util::poll!(arrival.as_mut()).is_pending());
        runs.push((tokio::spawn(fixture.runtime.run()), fixture.cancel));
        tokio::time::timeout(std::time::Duration::from_secs(1), arrival)
            .await
            .with_context(|| format!("write-off child {name} waited for a running reader"))?;
    }
    release.notify_waiters();
    for (run, _cancel) in runs {
        run.await??;
    }
    Ok(())
}

/// A child's native terminal does not complete its task before provider collection finishes.
///
/// The coordinator learns a task is finished only from `Completed`, and checkpoint settlement
/// trusts that, so a completion sent before shutdown would let a parent freeze a workspace that
/// the child's provider could still change.
#[tokio::test]
async fn child_completion_waits_for_provider_collection() -> Result<()> {
    let directory = tempfile::tempdir()?;
    crate::storage::layout::prepare(directory.path())?;
    let started = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let release = Arc::new(tokio::sync::Notify::new());
    release.notify_one();
    let services = RuntimeServices::open(directory.path())?;
    let coordinator = services.coordinator.clone();
    let completed_during_shutdown = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let shutdown_ran = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let observer = coordinator.clone();
    let observed = completed_during_shutdown.clone();
    let shutdown_witness = shutdown_ran.clone();
    let native = held_provider(started, Arc::new(tokio::sync::Notify::new()), release)?
        .with_shutdown(move || {
            let observer = observer.clone();
            let observed = observed.clone();
            let shutdown_witness = shutdown_witness.clone();
            async move {
                shutdown_witness.store(true, std::sync::atomic::Ordering::Release);
                while let Some(command) = observer.try_receive() {
                    if matches!(command, AgentCommand::Completed { .. }) {
                        observed.store(true, std::sync::atomic::Ordering::Release);
                    }
                }
                Ok(())
            }
        });
    let fixture = RuntimeFixture::with_services(
        directory.path(),
        native,
        "collected-child",
        None,
        &services,
    )?;
    fixture.runtime.run().await?;
    assert!(shutdown_ran.load(std::sync::atomic::Ordering::Acquire));
    assert!(
        !completed_during_shutdown.load(std::sync::atomic::Ordering::Acquire),
        "the child completed before its provider was collected"
    );
    let (output, cleanup_error) = completion(&coordinator)?;
    output.map_err(|error| error.context("the collected child turn succeeds"))?;
    assert_eq!(cleanup_error, None);
    Ok(())
}

/// Cancellation that arrives after repository admission, while the provider is starting,
/// still yields exactly one completion and no cleanup failure.
#[tokio::test]
async fn cancel_after_repository_admission_reports_one_completion() -> Result<()> {
    let directory = tempfile::tempdir()?;
    crate::storage::layout::prepare(directory.path())?;
    let started = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let arrived = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Notify::new());
    let native = held_provider(started.clone(), arrived.clone(), release.clone())?;
    let RuntimeFixture {
        runtime,
        coordinator,
        admitted: _admitted,
        cancel,
    } = RuntimeFixture::new(directory.path(), native, "racing-child", None)?;
    let arrival = arrived.notified();
    tokio::pin!(arrival);
    arrival.as_mut().enable();
    let run = tokio::spawn(runtime.run());
    tokio::time::timeout(std::time::Duration::from_secs(1), arrival)
        .await
        .context("the admitted child never reached its provider")?;
    // The reservation succeeded and the provider holds its turn when cancellation wins.
    cancel.send_replace(true);
    release.notify_one();
    run.await??;
    assert_eq!(started.load(std::sync::atomic::Ordering::Acquire), 1);
    let (output, cleanup_error) = completion(&coordinator)?;
    assert_eq!(cleanup_error, None);
    if let Err(error) = *output {
        assert!(error.is::<crate::broker::TurnCancelled>(), "{error:#}");
    }
    Ok(())
}
