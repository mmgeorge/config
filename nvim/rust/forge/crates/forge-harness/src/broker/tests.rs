use super::*;
use crate::agent::AgentExecutionState;
use crate::session::state_machine::SessionPhase;

#[path = "admission_test.rs"]
mod admission;
#[path = "agent_test.rs"]
mod agent;
#[path = "child_background_test.rs"]
mod child_background;
#[path = "child_cancellation_test.rs"]
mod child_cancellation;
#[path = "input_test.rs"]
mod input;
#[path = "test_support.rs"]
mod test_support;
#[path = "workflow_test.rs"]
mod workflow;

use test_support::{
    PreparationProbe, check_fixture, checkpoint_fixture, completed_message, settle_managed_children,
};
pub(super) use test_support::{planning_question_broker, repository};

fn compaction_event(session_id: &str, compaction: crate::provider::Compaction) -> ProviderEvent {
    ProviderEvent::Compaction {
        scope: crate::provider::EventScope::SessionOperation {
            owner: crate::provider::ExecutionOwner::root(session_id),
            operation_id: forge_provider_api::OperationId::new(compaction.id.clone())
                .expect("fixture compaction identity is nonempty"),
        },
        update: Box::new(compaction),
    }
}

fn active_execution(broker: &HarnessBroker) -> crate::plan::PlanExecutionRecord {
    match broker
        .task_controller
        .current()
        .unwrap()
        .unwrap()
        .workflow
        .clone()
    {
        crate::task::WorkflowSnapshot::Execute { execution } => execution.record,
        _ => panic!("expected an active execution"),
    }
}

#[tokio::test]
async fn live_status_and_agent_queries_do_not_reload_unrelated_history() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.create_task(crate::task::TaskKind::Goal, "active".into(), None, None)?;
    let task = broker.task_controller.current()?.unwrap();
    let connection = rusqlite::Connection::open(data.path().join("harness.sqlite3"))?;
    connection.execute("INSERT INTO exchange_record(id,session_id,agent_id,ordinal,payload) VALUES('old',?1,?2,1,?3)",rusqlite::params![broker.session.id,HarnessSession::primary_agent_id(&broker.session.id),serde_json::json!({"state":"complete","turn":"malformed history"}).to_string()])?;
    assert_eq!(
        broker.task_controller.current()?.unwrap().revision,
        task.revision
    );
    assert_eq!(
        broker.live_interaction_status(None)?,
        crate::session::state_machine::SessionPhase::Idle
    );
    assert!(broker.retry_finalization().await?.0["finalized_exchange_id"].is_null());
    connection.execute("DELETE FROM exchange_record WHERE id='old'", [])?;
    connection.execute_batch("ALTER TABLE prompt_history_record RENAME TO unavailable_history")?;
    let observer = broker.observer.clone();
    assert!(observer.snapshot().is_err());
    Ok(())
}

fn automatic_check_fixture(
    turns: Arc<std::sync::atomic::AtomicUsize>,
    repair: bool,
    explicit: bool,
    workspace: PathBuf,
) -> Result<crate::provider::test_support::ProviderTestSession> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Automatic check fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let turns = turns.clone();
            let workspace = workspace.clone();
            let descriptor = descriptor.clone();
            async move {
                let turn = turns.fetch_add(1, Ordering::SeqCst);
                if turn == 0 || repair {
                    request.permissions.before_tool(&request.scope).await?;
                    std::fs::write(
                        workspace.join("seed.txt"),
                        if turn > 0 { "fixed" } else { "broken" },
                    )
                    .map_err(|_| {
                        crate::provider::ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::InputSubmit,
                                forge_provider_api::ProviderFailureKind::Unknown,
                            ),
                        )
                    })?;
                }
                if turn == 0 && explicit {
                    assert!(request.tools.iter().any(|tool| tool.name == "check_run"));
                    assert!(
                        !request
                            .tools
                            .iter()
                            .any(|tool| tool.name.starts_with("check_")
                                && tool.name != "check_run"
                                && tool.name != "check_status")
                    );
                    let started = crate::provider::test_support::call_tool(
                        &request,
                        "check_run",
                        json!({"check":"verify"}),
                    )
                    .await?;
                    loop {
                        let run = crate::provider::test_support::call_tool(
                            &request,
                            "check_status",
                            json!({"run_id":started["run_id"],"wait_ms":30000}),
                        )
                        .await?;
                        if run["state"] != "running" {
                            break;
                        }
                    }
                }
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "automatic-check-stream".into(),
                    turn_id: turn.to_string().into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                for event in completed_message(
                    &address,
                    "repair-message",
                    crate::provider::MessageRole::Commentary,
                    "Check repair response",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: 100,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 100,
                            result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    );
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

#[tokio::test]
async fn automatic_checks_default_off_until_explicitly_enabled() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.shell = crate::session::CheckShell::Nushell;
    let provider = automatic_check_fixture(
        Arc::new(std::sync::atomic::AtomicUsize::new(0)),
        true,
        false,
        repository.path().to_owned(),
    )?;
    broker.provider = provider.provider_handle();
    std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true,"paths":["seed.txt"]},"action":{"command":"print ok","label":"Verify","category":"test"}}}}).to_string())?;
    assert!(!broker.snapshot()?.automatic_checks);
    broker
        .run_interaction(
            "Default off".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Default off".into())),
        )
        .await?;
    assert!(broker.check_runner.snapshot()?.history.is_empty());
    broker
        .configure_session(json!({"automatic_checks":true}))
        .await?;
    assert!(broker.snapshot()?.automatic_checks);
    broker
        .run_interaction(
            "Explicitly enabled".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Explicitly enabled".into())),
        )
        .await?;
    assert_eq!(broker.check_runner.snapshot()?.history.len(), 1);
    broker
        .configure_session(json!({"automatic_checks":false}))
        .await?;
    broker
        .run_interaction(
            "Explicitly disabled".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Explicitly disabled".into())),
        )
        .await?;
    assert_eq!(broker.check_runner.snapshot()?.history.len(), 1);
    Ok(())
}

#[tokio::test]
async fn automatic_checks_complete_when_remaining_queue_is_removed() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .configure_session(json!({"automatic_checks":true}))
        .await?;
    broker.session.approval_mode = ApprovalMode::Allow;
    broker.session.shell = crate::session::CheckShell::Nushell;
    let turns = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let provider =
        automatic_check_fixture(turns.clone(), false, false, repository.path().to_owned())?;
    broker.provider = provider.provider_handle();
    std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true,"paths":["seed.txt"]},"action":{"command":"print queue-pass","label":"Verify","category":"test"}}}}).to_string())?;
    assert_eq!(
        broker.snapshot()?.check_timing,
        crate::checks::CheckTiming::AfterQueue
    );
    broker.observer.input.apply(crate::input::InputOperation {
        id: "queue-completion-input".into(),
        control_revision: 0,
        after_control: None,
        action: crate::input::InputAction::Submit {
            message: Box::new(serde_json::from_value(json!({
                "id":"pending-completion-input", "text":"Remaining queued work",
                "intent":"queue", "attachments":[]
            }))?),
        },
    })?;
    broker
        .run_interaction(
            "Queued edit".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Queued edit".into())),
        )
        .await?;
    assert!(broker.snapshot()?.checks_deferred);
    broker.observer.input.apply(crate::input::InputOperation {
        id: "remove-completion-input".into(),
        control_revision: 0,
        after_control: None,
        action: crate::input::InputAction::Remove {
            id: "pending-completion-input".into(),
            revision: 1,
        },
    })?;
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.check.complete".into(),
            params: json!({}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    assert_eq!(broker.check_runner.snapshot()?.history.len(), 1);
    assert!(!broker.snapshot()?.checks_deferred);
    assert_eq!(turns.load(Ordering::SeqCst), 1);
    Ok(())
}

#[tokio::test]
async fn automatic_checks_repair_after_permission_resume_on_worker() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .configure_session(json!({"automatic_checks":true}))
        .await?;
    broker.session.shell = crate::session::CheckShell::Nushell;
    let provider = automatic_check_fixture(
        Arc::new(std::sync::atomic::AtomicUsize::new(0)),
        true,
        false,
        repository.path().to_owned(),
    )?;
    broker.provider = provider.provider_handle();
    std::fs::write(repository.path().join(".forge.json"), json!({"checks":{"verify":{"trigger":{"auto":true,"paths":["seed.txt"]},"action":{"command":"if ((open --raw seed.txt) | str contains fixed) { print ok } else { exit 7 }","label":"Verify","category":"test"}}}}).to_string())?;
    let (mut exchange, _, _) = broker
        .test_stage_interaction_for_turn("Resumed edit", true, broker.clock.now_ms())
        .await?;
    broker.begin_exchange_runtime(&mut exchange, broker.clock.now_ms())?;
    let mut runtime = broker.exchange_runtime.take().unwrap();
    broker
        .collect_initial_checkpoint(&mut runtime, &mut exchange)
        .await?;
    broker.exchange_runtime = Some(runtime);
    exchange.pause(broker.clock.now_ms());
    broker.store.save_exchange(&exchange)?;
    let worker = std::thread::spawn(move || -> Result<()> {
        // Reserve half the normal worker stack for the enclosing host request lifecycle.
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .thread_stack_size(1024 * 1024)
            .enable_all()
            .build()?;
        runtime.block_on(async move {
            tokio::spawn(async move {
                let result = broker
                    .dispatch(Request {
                        id: 1,
                        method: "session.command.apply".into(),
                        params: json!({"action":"permission","change":{"kind":"set_approval","mode":"ask"},"resume":true}),
                    })
                    .await;
                anyhow::ensure!(
                    result.response.error().is_none(),
                    "{:?}",
                    result.response.error()
                );
                anyhow::ensure!(
                    broker
                        .check_runner
                        .history()?
                        .iter()
                        .any(|run| run.state == crate::checks::CheckState::Passed),
                    "resumed repair did not pass"
                );
                Ok::<_, anyhow::Error>(())
            })
            .await?
        })
    });
    worker.join().expect("repair worker panicked")
}

#[tokio::test]
async fn interaction_future_has_bounded_inline_storage() {
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let future = Box::pin(broker.run_interaction_input(
        "Plan a fixture".into(),
        PromptMode::Plan,
        None,
        ProviderInput::from_text("Plan a fixture"),
    ));
    let bytes = std::mem::size_of_val(future.as_ref().get_ref());
    assert!(
        bytes <= 16 * 1024,
        "interaction retains {bytes} inline bytes"
    );
}

/// A failed pre-baseline owner releases its own capture while an unrelated Git editor stays admitted.
#[tokio::test]
async fn prebaseline_failure_settles_while_shared_git_admission_remains_held() -> Result<()> {
    for native_rejection in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let rejection = Arc::new(PreparationProbe {
            started: Notify::new(),
            release: Notify::new(),
            fail: true,
        });
        let tool_ran = Arc::new(AtomicBool::new(false));
        let active = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let provider = checkpoint_fixture(
            repository.path().to_owned(),
            (!native_rejection).then(|| rejection.clone()),
            Arc::new(Notify::new()),
            tool_ran.clone(),
            active.clone(),
            native_rejection.then(|| rejection.clone()),
            false,
        )?;
        broker.provider = provider.provider_handle();
        let repositories = broker.repositories.clone();
        let handle = repositories
            .open(repository.path().to_owned())
            .await?
            .context("fixture repository missing")?;
        let operation = repositories.writes.admit(
            vec![forge_git::mutation::MutationScope::WorktreeFiles(
                handle
                    .identity
                    .worktree
                    .clone()
                    .context("fixture worktree missing")?,
            )],
            0,
        )?;
        let held = repositories
            .writes
            .start(operation)?
            .context("external Git mutation was not admitted")?;
        let reject = async {
            rejection.started.notified().await;
            while repositories.writes.usage().operations < 2 {
                tokio::task::yield_now().await;
            }
            rejection.release.notify_one();
        };
        let prompt = "Fail before the baseline while Git commit owns the workspace";
        let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(3), async {
            tokio::join!(
                broker.run_interaction(
                    prompt.into(),
                    PromptMode::Chat,
                    Some(ExchangeAdmission::chat(prompt.into()))
                ),
                reject
            )
        })
        .await
        .context("failed execution waited for unrelated Git admission")?;
        let error = result.expect_err("pre-baseline failure became successful execution");
        let expected = if native_rejection {
            "Rejected"
        } else {
            "Runtime"
        };
        assert!(format!("{error:#}").contains(expected), "{error:#}");
        assert!(!error.is::<TurnCancelled>() && !error.is::<TurnRetracted>());
        assert!(!tool_ran.load(Ordering::Acquire) && active.load(Ordering::Acquire) == 0);
        assert!(broker.host_settlement().is_none() && broker.exchange_runtime.is_none());
        assert_eq!(
            repositories.writes.state(operation),
            Some(forge_git::coordinator::OperationState::Running {
                cancellation_requested: false
            })
        );
        assert_eq!(
            repositories.writes.usage().operations,
            1,
            "aborted baseline retained mutation admission"
        );
        let exchanges = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchanges.len(), 1);
        assert_eq!(exchanges[0].state(), ExchangeState::Failed);
        assert!(
            exchanges[0].checkpoint_before.is_none() && exchanges[0].checkpoint_after.is_none()
        );
        assert!(
            exchanges[0]
                .finalization_error
                .as_deref()
                .is_some_and(|error| error.contains(expected))
        );
        let query = broker.provider_request(ProviderInput::from_text(prompt), PromptMode::Chat)?;
        let pending = broker.submission_store.pending(
            &query,
            forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            None,
        )?;
        assert_eq!(pending.len(), 1);
        assert!(matches!(
            pending[0].delivery,
            crate::provider::submission::SubmissionState::Prepared
        ));
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt"))?,
            "seed\n"
        );
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
    }
    Ok(())
}

/// Collect a denied baseline despite native cleanup failure, without claiming rollback proof.
#[tokio::test]
async fn cancellation_cleanup_failure_collects_denied_baseline_without_native_rollback()
-> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let started = Arc::new(Notify::new());
    let tool_ran = Arc::new(AtomicBool::new(false));
    let active = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let provider = checkpoint_fixture(
        repository.path().to_owned(),
        None,
        started.clone(),
        tool_ran.clone(),
        active.clone(),
        None,
        true,
    )?;
    broker.provider = provider.provider_handle();
    broker.capability.native_turn_rollback = true;
    let repositories = broker.repositories.clone();
    let handle = repositories
        .open(repository.path().to_owned())
        .await?
        .context("fixture repository")?;
    let operation = repositories.writes.admit(
        vec![forge_git::mutation::MutationScope::WorktreeFiles(
            handle
                .identity
                .worktree
                .clone()
                .context("fixture worktree")?,
        )],
        0,
    )?;
    let held = repositories
        .writes
        .start(operation)?
        .context("external Git admission")?;
    let cancellation = broker.turn_cancellation();
    cancellation.arm();
    let cancel = async {
        started.notified().await;
        cancellation.request();
    };
    let result = tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::join!(
            broker.run_interaction(
                "Cancel with failed native cleanup".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat(
                    "Cancel with failed native cleanup".into()
                )),
            ),
            cancel,
        )
        .0
    })
    .await;
    // Inspect while admission stays held, then release it even if verification failed.
    // An abandoned Git operation intentionally remains a safety fence for later workers.
    let verified: Result<()> = (|| {
        let result = result
            .context("native cleanup failure left the denied baseline waiting for external Git")?;
        let error = result
            .err()
            .context("failed native cleanup became success")?;
        anyhow::ensure!(
            error.is::<TurnCancelled>() && !error.is::<TurnRetracted>(),
            "{error:#}"
        );
        let cleanup = error
            .downcast_ref::<foreground_failure::ForegroundFailureContext>()
            .and_then(|context| context.cleanup.as_ref());
        let Some(crate::provider::ProviderError::Cleanup(report)) = cleanup else {
            anyhow::bail!("native cleanup lost its typed report: {error:#}");
        };
        anyhow::ensure!(
            matches!(report.cancellation(), [
        forge_provider_api::error::CleanupFailure::Failed(error)
    ] if matches!(error.as_ref(), crate::provider::ProviderError::Failure(failure)
        if failure.operation() == forge_provider_api::ProviderOperation::Cancel
            && failure.kind() == forge_provider_api::ProviderFailureKind::Transport)),
            "{error:#}"
        );
        anyhow::ensure!(
            broker.host_settlement().is_none() && broker.exchange_runtime.is_none(),
            "foreground retained its failed owner"
        );
        anyhow::ensure!(
            !tool_ran.load(Ordering::Acquire) && active.load(Ordering::Acquire) == 0,
            "native producer or tool remained active"
        );
        anyhow::ensure!(
            repositories.writes.usage().operations == 1,
            "baseline retained Git admission"
        );
        anyhow::ensure!(
            repositories.writes.state(operation)
                == Some(forge_git::coordinator::OperationState::Running {
                    cancellation_requested: false
                }),
            "failed baseline altered external Git admission"
        );
        let exchanges = broker.store.list_exchange(&broker.session.id)?;
        anyhow::ensure!(
            exchanges.len() == 1,
            "cleanup failure must retain the accepted input"
        );
        anyhow::ensure!(
            exchanges[0].state() == ExchangeState::Cancelled,
            "cancelled exchange changed outcome"
        );
        anyhow::ensure!(
            exchanges[0].checkpoint_before.is_none() && exchanges[0].checkpoint_after.is_none(),
            "denied baseline published checkpoint evidence"
        );
        anyhow::ensure!(
            exchanges[0]
                .finalization_error
                .as_deref()
                .is_some_and(
                    |error| error.contains("CleanupExecution") && error.contains("Transport")
                ),
            "durable exchange lost native cleanup error"
        );
        anyhow::ensure!(
            std::fs::read_to_string(repository.path().join("seed.txt"))? == "seed\n",
            "failed native cleanup changed workspace"
        );
        Ok(())
    })();
    let released = held.finish(forge_git::mutation::OperationCompletion::Completed);
    repositories.writes.take_receipt(operation);
    verified.and(released)
}

#[tokio::test]
async fn cancellation_rewinds_before_checkpoint_barrier_without_waiting_for_scan() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let started = Arc::new(Notify::new());
    let tool_ran = Arc::new(AtomicBool::new(false));
    let active = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let provider = checkpoint_fixture(
        repository.path().to_owned(),
        None,
        Arc::clone(&started),
        Arc::clone(&tool_ran),
        Arc::clone(&active),
        None,
        false,
    )?;
    broker.provider = provider.provider_handle();
    broker.capability.native_turn_rollback = true;
    broker.trace.configure(true)?;
    let repositories = Arc::clone(&broker.repositories);
    let handle = repositories
        .open(repository.path().to_owned())
        .await?
        .context("fixture repository")?;
    let operation = repositories.writes.admit(
        vec![forge_git::mutation::MutationScope::WorktreeFiles(
            handle
                .identity
                .worktree
                .clone()
                .context("fixture worktree")?,
        )],
        0,
    )?;
    let held = repositories
        .writes
        .start(operation)?
        .context("fixture mutation admission")?;
    let cancellation = broker.turn_cancellation();
    for attempt in 0..16 {
        cancellation.arm();
        let cancel = async {
            started.notified().await;
            cancellation.request();
        };
        let execution = broker.run_interaction(
            "Undo this new prompt".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Undo this new prompt".into())),
        );
        let result = tokio::time::timeout(std::time::Duration::from_millis(200), async {
            tokio::join!(execution, cancel).0
        })
        .await;
        let error = result
            .with_context(|| format!(
                "cancellation waited for the blocked checkpoint scan; attempt={attempt}, native_active={}, cancel_requested={}, stored={}",
                active.load(Ordering::Acquire),
                cancellation.is_requested(),
                broker.store.list_exchange(&broker.session.id)
                    .and_then(|exchanges| Ok(serde_json::to_string(&exchanges)?))
                    .unwrap_or_else(|error| format!("read failed: {error:#}")),
            ) + &format!("; trace={}", std::fs::read_to_string(broker.trace.session_status(&broker.session.id).path)
                .unwrap_or_else(|error| format!("read failed: {error}"))))?
            .unwrap_err();
        assert!(error.is::<TurnRetracted>(), "{error:#}");
        assert!(
            broker.host_settlement().is_none(),
            "cancelled owner retained its host settlement fence"
        );
        assert!(!tool_ran.load(Ordering::SeqCst));
        assert!(broker.store.list_exchange(&broker.session.id)?.is_empty());
        assert!(broker.exchange_runtime.is_none());
        assert_eq!(
            active.load(Ordering::SeqCst),
            0,
            "cancelled provider still owns its turn"
        );
        assert_eq!(
            Arc::strong_count(&active),
            2,
            "cancelled provider retained its lifetime probe"
        );
    }
    held.finish(forge_git::mutation::OperationCompletion::Completed)?;
    repositories.writes.take_receipt(operation);
    assert_eq!(
        std::fs::read_to_string(repository.path().join("seed.txt"))?,
        "seed\n"
    );
    Ok(())
}

#[tokio::test]
async fn native_abort_completion_rewinds_before_pending_checkpoint_finalization() -> Result<()> {
    for (cleanup_before_completion, visible_output, restore_prompt) in [
        (true, false, true),
        (false, false, true),
        (true, true, true),
        (false, true, true),
        (true, false, false),
        (false, false, false),
    ] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (provider, signals) = cleanup_retry_provider(&broker, visible_output, false);
        broker.provider = provider.clone();
        broker.capability.native_turn_rollback = true;
        broker.trace.configure(true)?;
        let execution_registry = broker.execution_registry();
        let repositories = Arc::clone(&broker.repositories);
        let handle = repositories
            .open(repository.path().to_owned())
            .await?
            .context("fixture repository")?;
        let operation = repositories.writes.admit(
            vec![forge_git::mutation::MutationScope::WorktreeFiles(
                handle
                    .identity
                    .worktree
                    .clone()
                    .context("fixture worktree")?,
            )],
            0,
        )?;
        let held = repositories
            .writes
            .start(operation)?
            .context("fixture mutation admission")?;
        let cancellation = broker.turn_cancellation();
        cancellation.arm();
        let session_id = broker.session.id.clone();
        let cancel = async {
            signals.started.notified().await;
            if cleanup_before_completion {
                cancellation.block_submissions();
                let scope = signals
                    .scope
                    .lock()
                    .unwrap()
                    .clone()
                    .context("fixture invocation scope missing")?;
                provider.cancel_invocation(scope).await?;
            } else {
                signals.complete.notify_one();
                signals.settled.notified().await;
                tokio::task::yield_now().await;
                cancellation.block_submissions();
            }
            cancellation.request();
            Ok::<_, anyhow::Error>(())
        };
        let execution = broker.run_interaction(
            "Cancel after native dispatch".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat(
                "Cancel after native dispatch".into(),
            )),
        );
        let result = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::join!(execution, cancel)
        })
        .await;
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
        let (result, cancelled) = result.with_context(|| format!(
            "native abort completion waited for checkpoint finalization; cleanup_before_completion={cleanup_before_completion}, visible_output={visible_output}, restore_prompt={restore_prompt}, native_settled={}, cleanup_attempts={}, cancel_requested={}, stored={}",
            signals.native_settled.load(Ordering::Acquire),
            signals.attempt.load(Ordering::Acquire),
            cancellation.is_requested(),
            broker.store.list_exchange(&broker.session.id)
                .and_then(|exchanges| Ok(serde_json::to_string(&exchanges)?))
                .unwrap_or_else(|error| format!("read failed: {error:#}")),
        ) + &format!("; trace={}", std::fs::read_to_string(broker.trace.session_status(&broker.session.id).path)
            .unwrap_or_else(|error| format!("read failed: {error}"))))?;
        cancelled?;
        let error = result.unwrap_err();
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        if restore_prompt && !visible_output {
            assert!(
                error.is::<TurnRetracted>(),
                "cleanup_before_completion={cleanup_before_completion}, visible_output={visible_output}, restore_prompt={restore_prompt}; {error:#}; trace={}",
                std::fs::read_to_string(broker.trace.session_status(&broker.session.id).path)
                    .unwrap_or_else(|error| format!("read failed: {error}"))
            );
            assert!(exchange.is_empty());
        } else {
            assert!(error.is::<TurnCancelled>(), "{error:#}");
            assert_eq!(exchange.len(), 1);
            assert_eq!(exchange[0].state(), ExchangeState::Cancelled);
            assert!(exchange[0].checkpoint_before.is_none());
            assert!(exchange[0].checkpoint_after.is_none());
            assert!(exchange[0].finalization_error.is_none());
            if visible_output {
                assert!(exchange[0].turn.iter().any(|turn| {
                    turn.messages()
                        .iter()
                        .any(|message| message.text() == "Partial response")
                }));
            }
        }
        assert!(broker.exchange_runtime.is_none());
        assert!(
            !execution_registry
                .owner(crate::provider::ExecutionOwner::root(&session_id))
                .await?
                .is_healthy()
                .await,
            "cancelled baseline owner must remain fenced until replacement collection"
        );
    }
    Ok(())
}

#[tokio::test]
async fn checkpoint_does_not_delay_prompt_admission_but_blocks_first_tool() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let started = Arc::new(tokio::sync::Notify::new());
    let tool_ran = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let document = forge_buffer::identity::DocumentId("checkpoint-transcript".into());
    {
        let mut presentation = broker.presentation.lock().unwrap();
        presentation.open(
            document.clone(),
            forge_buffer::identity::ViewId("checkpoint-view".into()),
            forge_buffer::width::WidthProfile::default(),
        )?;
        presentation.begin_submission(&document, 1, "Capture before my first tool")?;
    }
    broker.prompt_admission = Some((Some(document), 1));
    let (sink, mut stream) = publication::channel();
    broker.event_sink = Some(sink);
    let provider = checkpoint_fixture(
        repository.path().to_owned(),
        None,
        started.clone(),
        tool_ran.clone(),
        Default::default(),
        None,
        false,
    )?;
    broker.provider = provider.provider_handle();
    let repositories = broker.repositories.clone();
    let handle = repositories
        .open(repository.path().to_owned())
        .await?
        .context("fixture repository")?;
    let operation = repositories.writes.admit(
        vec![forge_git::mutation::MutationScope::WorktreeFiles(
            handle
                .identity
                .worktree
                .clone()
                .context("fixture worktree")?,
        )],
        0,
    )?;
    let held = repositories
        .writes
        .start(operation)?
        .context("fixture mutation admission")?;
    let session_id = broker.session.id.clone();
    let observe = async {
        tokio::time::timeout(std::time::Duration::from_secs(2), started.notified()).await?;
        let mut accepted = false;
        let mut projected = false;
        while let Ok(event) = stream.try_recv() {
            accepted |= matches!(
                &event,
                SessionUpdate::Submission {
                    transition: publication::SubmissionUpdate {
                        state: publication::SubmissionState::Accepted,
                        ..
                    }
                }
            );
            projected |= matches!(&event, SessionUpdate::DocumentChanged { .. });
        }
        assert!(
            accepted,
            "composer acceptance waited for checkpoint capture"
        );
        assert!(
            projected,
            "timeline did not project before checkpoint capture"
        );
        let store = SqliteStore::open(data.path())?;
        let exchange = store.list_exchange(&session_id)?;
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].prompt, "Capture before my first tool");
        assert!(exchange[0].checkpoint_before.is_none());
        assert!(!tool_ran.load(Ordering::SeqCst));
        assert_eq!(
            std::fs::read_to_string(repository.path().join("seed.txt"))?,
            "seed\n"
        );
        held.finish(forge_git::mutation::OperationCompletion::Completed)?;
        repositories.writes.take_receipt(operation);
        Ok::<_, anyhow::Error>(())
    };
    let (result, observed) = tokio::time::timeout(std::time::Duration::from_secs(10), async {
        tokio::join!(
            broker.run_interaction(
                "Capture before my first tool".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat(
                    "Capture before my first tool".into()
                ))
            ),
            observe
        )
    })
    .await?;
    observed?;
    result?;
    assert!(tool_ran.load(Ordering::SeqCst));
    let exchange = broker
        .store
        .list_exchange(&session_id)?
        .pop()
        .context("completed fixture exchange")?;
    let before = broker
        .store
        .load_checkpoint(exchange.checkpoint_before.as_deref().context("before id")?)?
        .context("before checkpoint")?;
    assert!(!before.file.iter().any(|file| file.path == "seed.txt"));
    let after = broker
        .store
        .load_checkpoint(exchange.checkpoint_after.as_deref().context("after id")?)?
        .context("after checkpoint")?;
    assert!(after.file.iter().any(|file| file.path == "seed.txt"));
    Ok(())
}

#[tokio::test]
async fn reopened_pause_finalizes_before_executing_the_admitted_prompt() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let session_id = broker.session.id.clone();
    let mut interrupted =
        completed_interaction("stopped-before-checkpoint", &session_id, Vec::new());
    interrupted.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Finalizing,
        interrupted.completed_at_ms(),
        interrupted.finalization_outcome(),
    );
    interrupted.phase = crate::exchange::ExchangePhase::fixture(
        interrupted.state(),
        None,
        interrupted.finalization_outcome(),
    );
    interrupted.phase =
        crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Cancelled);
    broker.store.save_exchange(&interrupted)?;
    drop(broker);
    let mut reopened = planning_question_broker(repository.path(), data.path(), false);
    assert_eq!(reopened.session.id, session_id);
    let occupied: Vec<_> = (0..4)
        .map(|_| reopened.repositories.reads.submit(0, |_| Ok(())).unwrap())
        .collect();
    let error = reopened
        .run_interaction(
            "Wait for recovery".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Wait for recovery".into())),
        )
        .await
        .unwrap_err();
    assert!(format!("{error:#}").contains("capacity is full"));
    let pending = reopened.store.list_exchange(&session_id)?;
    assert_eq!(pending.len(), 2);
    assert_eq!(pending[0].state(), ExchangeState::Finalizing);
    assert!(pending[0].finalization_error.is_some());
    assert_eq!(pending[1].state(), ExchangeState::Failed);
    assert!(pending[1].checkpoint_before.is_none());
    for task in occupied {
        task.finish().await?;
    }
    reopened
        .run_interaction(
            "Continue after reopening".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Continue after reopening".into())),
        )
        .await?;
    let history = reopened.store.list_exchange(&session_id)?;
    assert_eq!(history.len(), 3);
    assert_eq!(history[0].state(), ExchangeState::Cancelled);
    assert!(history[0].checkpoint_after.is_some());
    assert!(history[0].finalization_error.is_none());
    assert_eq!(history[1].state(), ExchangeState::Failed);
    assert_eq!(history[2].state(), ExchangeState::Complete);
    Ok(())
}

#[tokio::test]
async fn initial_checkpoint_failure_can_finalize_without_replaying_provider_work() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let occupied: Vec<_> = (0..4)
        .map(|_| broker.repositories.reads.submit(0, |_| Ok(())).unwrap())
        .collect();
    let error = broker
        .run_interaction(
            "blocked acquisition".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("blocked acquisition".into())),
        )
        .await
        .unwrap_err();
    assert!(format!("{error:#}").contains("capacity is full"));
    let failed = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(failed.len(), 1);
    assert_eq!(failed[0].state(), ExchangeState::Finalizing);
    assert!(failed[0].checkpoint_before.is_none());
    for task in occupied {
        task.finish().await?;
    }
    broker.retry_finalization().await?;
    let settled = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(settled[0].state(), ExchangeState::Failed);
    assert_eq!(settled[0].checkpoint_before, settled[0].checkpoint_after);
    assert!(settled[0].turn.is_empty());
    Ok(())
}

#[tokio::test]
async fn manual_child_admission_streams_and_followup_reuses_only_child_history() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.provider_session_id = Some("root-history".into());
    assert!(!broker.agent_coordinator.enabled());
    let (admitted, _) = broker
        .start_agent(json!({"profile":"worker","text":"CHILD_FIRST"}))
        .await?;
    let agent_id = admitted["agent"]["id"]
        .as_str()
        .context("admission has no child identity")?
        .to_owned();
    assert_eq!(admitted["status"], "admitted");
    let event = settle_managed_children(&mut broker).await?;
    assert!(
        event.iter().any(|event| event.event == "document_changed"
            || event.payload["kind"] == "document_changed"),
        "child events did not update the shared document: {:?}",
        event
            .iter()
            .map(|event| (&event.event, &event.payload["kind"]))
            .collect::<Vec<_>>()
    );
    let child = broker.agent_coordinator.resolve(&agent_id)?;
    assert!(child.provider_thread_id.is_some());
    assert_ne!(child.provider_thread_id.as_deref(), Some("root-history"));
    assert_eq!(
        broker.session.provider_session_id.as_deref(),
        Some("root-history")
    );
    let exchange = broker.store.list_agent_exchange(&agent_id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Complete);
    assert!(
        exchange[0]
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text().contains("CHILD_FIRST"))
    );
    let sender = broker.agent_coordinator.sender();
    let request = crate::agent::AgentRequest {
        operation_id: "followup".into(),
        source: crate::agent::AgentSource::User,
        operation: crate::agent::AgentOperation::Followup {
            target: agent_id.clone(),
            message: "CHILD_SECOND".into(),
        },
    };
    let response = tokio::spawn(async move { sender.request(request).await });
    tokio::task::yield_now().await;
    broker.drain_agent_commands(None).await?;
    assert_eq!(response.await??["agent"]["id"], agent_id);
    settle_managed_children(&mut broker).await?;
    let exchange = broker.store.list_agent_exchange(&agent_id)?;
    assert_eq!(exchange.len(), 2);
    assert!(
        exchange
            .iter()
            .all(|exchange| exchange.state() == ExchangeState::Complete)
    );
    assert_eq!(broker.agent_coordinator.snapshot(None).len(), 1);
    assert_eq!(broker.store.list_agent_task(&broker.session.id)?.len(), 2);
    Ok(())
}

fn delegating_root_fixture() -> Result<(
    Arc<AtomicU64>,
    crate::provider::test_support::ProviderTestSession,
)> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized mock".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let turn = Arc::new(AtomicU64::new(0));
    let provider = crate::provider::test_support::EventFixtureProvider::new(descriptor.clone(), {
        let turn = turn.clone();
        move |request, sink, _cancelled| {
            let turn = turn.clone();
            let descriptor = descriptor.clone();
            async move {
                let ordinal = turn.fetch_add(1, Ordering::AcqRel);
                let text = match &request.input {
                    forge_provider_api::PromptInput::Text(text) => text.clone(),
                    _ => String::new(),
                };
                if ordinal == 0 {
                    let result = crate::provider::test_support::call_tool(
                        &request,
                        "agent_spawn",
                        json!({"profile":"worker","text":"CHILD_RESULT"}),
                    )
                    .await?;
                    assert!(result.to_string().contains("admitted"));
                } else {
                    assert!(ordinal == 1, "parent continuation duplicated");
                    assert!(
                        text.contains("CHILD_RESULT") && text.contains("\"response\""),
                        "parent continuation omitted child result"
                    );
                }
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "parent-history".into(),
                    turn_id: format!("root-{ordinal}").into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                let response = if ordinal == 0 {
                    "Waiting for child"
                } else {
                    "SYNTHESIZED_RESULT"
                };
                for event in completed_message(
                    &address,
                    "root-response",
                    crate::provider::MessageRole::Response,
                    response,
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: 2,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 2,
                            result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    provider_session_id: Some(
                                        "parent-history"
                                            .to_owned()
                                            .try_into()
                                            .expect("fixture ID is valid"),
                                    ),
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        }
    });
    let provider = provider.with_session_identity(|_| async {
        Ok(Some(
            "parent-history"
                .to_owned()
                .try_into()
                .expect("fixture ID is valid"),
        ))
    });
    Ok((
        turn,
        crate::provider::test_support::ProviderTestSession::from_native(provider)?,
    ))
}

#[tokio::test]
async fn model_child_completes_through_the_active_root_lane_and_continues_the_same_exchange()
-> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (turn, provider) = delegating_root_fixture()?;
    broker.provider = provider.provider_handle();
    broker.agent_coordinator.set_enabled(true);
    tokio::time::timeout(
        std::time::Duration::from_secs(5),
        broker.run_interaction(
            "Root request".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Root request".into())),
        ),
    )
    .await??;
    assert_eq!(turn.load(Ordering::Acquire), 2);
    let exchange = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].state(), ExchangeState::Complete);
    assert!(
        exchange[0]
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text().contains("SYNTHESIZED_RESULT"))
    );
    let task = broker.store.list_agent_task(&broker.session.id)?;
    assert_eq!(task.len(), 1);
    assert!(task[0].parent_reported);
    assert_eq!(task[0].parent_exchange_id, exchange[0].id);
    Ok(())
}

#[tokio::test]
async fn yolo_toggles_sandbox_in_direct_and_admitted_transitions() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    for registration in forge_providers::registry::descriptors() {
        let sandbox_supported = registration.capability.supports_sandbox();
        broker.session.provider = registration.id.to_string();
        for admitted in [false, true] {
            broker
                .configure_session(json!({"access":crate::session::AccessPolicy {
                sandbox: sandbox_supported, ..Default::default()
            }, "approval_mode":"ask"}))
                .await
                .unwrap();
            for (sandbox, approval) in [
                (false, "allow"),
                (sandbox_supported, "ask"),
                (false, "allow"),
            ] {
                let mut action = json!({"change":{"kind":"enable_yolo"}});
                if admitted {
                    action["permission_selection"] = json!({
                        "sandbox":sandbox,"read_access":"full","write_access":"full","approval_mode":approval
                    });
                }
                broker
                    .apply_configuration(action, configuration::ConfigurationCommand::Permission)
                    .await
                    .unwrap();
                let stored = broker
                    .store
                    .load_session(&broker.session.id)
                    .unwrap()
                    .unwrap();
                assert_eq!(stored.access.sandbox, sandbox);
                assert_eq!(stored.access.abbreviation(), "rxwx");
                assert_eq!(stored.approval_mode.label().to_lowercase(), approval);
                assert!(stored.active_task.is_none());
            }
        }
    }
}

#[tokio::test]
async fn sandbox_preferences_resolve_on_load_and_configuration_for_each_provider() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let preferences = crate::settings::PortableSettings {
        access: Some(crate::session::AccessPolicy::default()),
        ..Default::default()
    };
    for registration in forge_providers::registry::descriptors() {
        let provider = registration.id.as_str();
        let sandbox_supported = registration.capability.supports_sandbox();
        broker.session.provider = provider.into();
        preferences.apply(&mut broker.session, true).unwrap();
        assert_eq!(broker.session.access.sandbox, sandbox_supported);
        for (enabled, mode) in [
            (true, Some("mxc".to_owned())),
            (false, Some("unelevated".to_owned())),
            (true, Some("elevated".to_owned())),
        ] {
            let requested = crate::session::AccessPolicy {
                sandbox: enabled,
                isolation_mode: mode.clone(),
                ..Default::default()
            };
            broker
                .configure_session(json!({"access":requested,"session_policy":true}))
                .await
                .unwrap();
            broker
                .apply_configuration(
                    json!({"change":{"kind":"set_approval","mode":"allow"}}),
                    configuration::ConfigurationCommand::Permission,
                )
                .await
                .unwrap();
            for kind in [
                "toggle_read_scope",
                "toggle_read_scope",
                "toggle_write_enabled",
                "toggle_write_enabled",
                "toggle_write_scope",
                "toggle_write_scope",
            ] {
                broker
                    .apply_configuration(
                        json!({"change":{"kind":kind}}),
                        configuration::ConfigurationCommand::Permission,
                    )
                    .await
                    .unwrap();
            }
            let snapshot = broker.snapshot().unwrap();
            assert_eq!(
                snapshot.session.access.sandbox,
                enabled && sandbox_supported
            );
            assert_eq!(snapshot.access_preference, requested);
            let saved = crate::settings::SettingsStore::read_only(data.path())
                .unwrap()
                .get(&snapshot.session.workspace, &snapshot.session.id, provider)
                .unwrap()
                .desired;
            assert_eq!(saved.access.as_ref(), Some(&requested));
            let mut restored = snapshot.session.clone();
            saved.apply(&mut restored, true).unwrap();
            assert_eq!(restored.access.sandbox, enabled && sandbox_supported);
            assert_eq!(restored.access.isolation_mode, mode);
            for target in forge_providers::registry::descriptors() {
                restored.provider = target.id.to_string();
                saved.apply(&mut restored, true).unwrap();
                assert_eq!(
                    restored.access.sandbox,
                    enabled && target.capability.supports_sandbox(),
                    "selected preference changed during a provider switch"
                );
            }
        }
    }
    assert!(preferences.access.unwrap().sandbox);
}

#[tokio::test]
async fn access_configuration_is_atomic_persistent_and_independent_of_approvals() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let writable = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let policy = crate::session::AccessPolicy {
        sandbox: false,
        writable_directory: vec![writable.path().to_string_lossy().into_owned()],
        ..Default::default()
    };
    broker
        .configure_session(json!({"access":policy}))
        .await
        .unwrap();
    let stored = broker
        .store
        .load_session(&broker.session.id)
        .unwrap()
        .unwrap();
    assert!(!stored.access.sandbox);
    assert_eq!(stored.access.writable_directory.len(), 1);
    assert_eq!(stored.approval_mode, ApprovalMode::Ask);
    let preference = broker
        .store
        .load_preference(&stored.workspace, &stored.provider)
        .unwrap()
        .unwrap();
    assert_eq!(preference.access, stored.access);
    broker
        .configure_session(json!({"approval_mode":"allow"}))
        .await
        .unwrap();
    assert_eq!(broker.session.access, stored.access);
    assert!(
        broker
            .configure_session(json!({"approval_mode":"allow","effort":""}))
            .await
            .is_err()
    );
    assert_eq!(broker.session.approval_mode, ApprovalMode::Allow);
    assert!(broker.configure_session(json!({"access":{"sandbox":true,"write_access":"workspace","writable_directory":["relative"],"isolation_mode":"elevated"}})).await.is_err());
    assert_eq!(broker.session.access, stored.access);
}

#[tokio::test]
async fn task_replacement_keeps_one_owner_and_clear_records_outcome() {
    use crate::task::TaskKind;
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .create_task(TaskKind::Goal, "First".into(), None, None)
        .unwrap();
    let first = broker
        .task_controller
        .current()
        .unwrap()
        .unwrap()
        .revision
        .clone();
    broker
        .create_task(TaskKind::Goal, "Second".into(), None, None)
        .unwrap();
    let second = broker.task_controller.current().unwrap().unwrap();
    assert_ne!(first, second.revision);
    assert_eq!(second.title(), "Second");
    broker.pause_task(true).await.unwrap();
    assert!(broker.task_controller.current().unwrap().is_none());
    let events = broker.store.list_session_event(&broker.session.id).unwrap();
    assert!(events.iter().any(|event| matches!(&event.detail,
        SessionEventKind::TaskFinished { outcome } if outcome.title == "Second")));
}

#[tokio::test]
async fn paused_task_discussion_does_not_resume_task() -> Result<()> {
    use crate::task::{TaskKind, TaskStatus};
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let goal = broker.create_goal("Persist goal".into())?;
    broker.create_task(TaskKind::Goal, goal.objective.clone(), None, Some(goal.id))?;
    broker.pause_task(false).await?;
    let paused = broker.task_controller.current()?.unwrap();
    let response = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Explain the current approach"}),
        })
        .await;
    assert!(
        response.response.error().is_none(),
        "{:?}",
        response.response.error()
    );
    let current = broker.task_controller.current()?.unwrap();
    assert_eq!(current.title(), paused.title());
    assert_eq!(current.revision, paused.revision);
    assert_eq!(current.status(), TaskStatus::Paused);
    let exchange = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].prompt, "Explain the current approach");
    assert_eq!(exchange[0].origin, crate::exchange::ExchangeOrigin::User);
    Ok(())
}

#[tokio::test]
async fn task_permission_change_while_paused_cannot_restart_the_provider() {
    use crate::task::{TaskKind, TaskStatus};
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let goal = broker.create_goal("Persist goal".into()).unwrap();
    broker
        .create_task(TaskKind::Goal, goal.objective.clone(), None, Some(goal.id))
        .unwrap();
    broker.pause_task(false).await.unwrap();
    broker
        .transition_task(
            json!({"action":"permission","change":{"kind":"set_approval","mode":"ask"}}),
        )
        .await
        .unwrap();
    let task = broker.task_controller.current().unwrap().unwrap();
    assert_eq!(task.status(), TaskStatus::Paused);
    assert_eq!(broker.session.approval_mode, ApprovalMode::Ask);
    assert!(
        broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .is_empty()
    );
}

#[test]
fn committed_task_clear_survives_runtime_recovery() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .create_task(
            crate::task::TaskKind::Goal,
            "Interrupted".into(),
            None,
            None,
        )
        .unwrap();
    let task = broker.task_controller.current().unwrap().unwrap();
    broker
        .task_controller
        .change(
            &mut broker.store,
            Some(&task.revision),
            crate::task::TaskChange::Clear,
            1,
        )
        .unwrap();
    assert!(broker.task_controller.current().unwrap().is_none());
    broker
        .store
        .interrupt_detached_execution(&broker.session.id)
        .unwrap();
    assert!(
        broker
            .store
            .load_session(&broker.session.id)
            .unwrap()
            .unwrap()
            .active_task
            .is_none()
    );
    assert!(broker.store.list_session_event(&broker.session.id).unwrap().iter().any(|event|
        matches!(&event.detail, SessionEventKind::TaskFinished { outcome } if outcome.title == "Interrupted")));
}

fn save_check_exchange(broker: &mut HarnessBroker, id: &str, execution: bool) -> Result<()> {
    let exchange: Exchange = serde_json::from_value(json!({
        "id": id, "session_id": broker.session.id,
        "agent_id": broker.session.primary_agent_id,
        "ordinal": broker.store.list_exchange(&broker.session.id)?.len() + 1,
        "prompt": "Check owner", "state": "complete",
        "kind": if execution { "plan_execution" } else { "chat" },
        "plan_id": execution.then_some("check-plan"),
        "execution_id": execution.then_some("check-execution"),
        "created_at_ms": 1, "completed_at_ms": 2,
        "attributed_matches_checkpoint": true, "node_list": []
    }))?;
    broker.store.save_exchange(&exchange)?;
    broker.reconcile_timeline()?;
    Ok(())
}

#[cfg(windows)]
fn update_check_execution(broker: &mut HarnessBroker, record: PlanExecutionRecord) {
    let task = broker.task_controller.current().unwrap().unwrap();
    broker
        .task_controller
        .update(&mut broker.store, &task.permit(), 100, |task| {
            let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow else {
                anyhow::bail!("check fixture has no selected execution");
            };
            anyhow::ensure!(execution.record.id == record.id, "check execution changed");
            execution.record = record;
            Ok(())
        })
        .unwrap();
}

#[cfg(windows)]
#[tokio::test]
async fn harness_checks_own_exit_codes_output_and_manual_gate_without_model_turns() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = check_fixture(
        repository.path(),
        data.path(),
        vec![
            "Write-Output 'accepted stdout'; [Console]::Error.WriteLine('accepted stderr')".into(),
            "exit 7".into(),
            "Write-Output 'last check'".into(),
        ],
        true,
    )
    .await;
    broker.run_checks().await.unwrap();
    let execution = broker
        .store
        .load_plan_execution("check-execution")
        .unwrap()
        .unwrap();
    assert_eq!(execution.phase, crate::plan::PlanPhase::Resolve);
    let run = &execution.check_runs[0];
    assert_eq!(run.commands[1].exit_code, Some(7));
    assert_eq!(run.commands[2].state, crate::checks::CheckState::Passed);
    assert!(
        std::fs::read_to_string(&run.commands[0].stdout)
            .unwrap()
            .contains("accepted stdout")
    );
    assert!(
        std::fs::read_to_string(&run.commands[0].stderr)
            .unwrap()
            .contains("accepted stderr")
    );
    let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].id, "check-exchange");
    assert!(execution.verification.is_empty());
}

#[cfg(windows)]
#[tokio::test]
async fn harness_checks_reject_unplanned_public_api_without_manual_verification() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = check_fixture(repository.path(), data.path(), Vec::new(), false).await;
    std::fs::write(
        repository.path().join("extra.rs"),
        "pub fn unplanned() {}\n",
    )
    .unwrap();
    broker.run_checks().await.unwrap();
    let execution = broker
        .store
        .load_plan_execution("check-execution")
        .unwrap()
        .unwrap();
    assert_eq!(execution.phase, crate::plan::PlanPhase::Resolve);
    assert!(!execution.check_runs[0].progress.different.is_empty());
    let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].id, "check-exchange");
}

#[cfg(windows)]
#[tokio::test]
async fn harness_checks_pass_before_manual_verification() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = check_fixture(
        repository.path(),
        data.path(),
        vec!["Write-Output 'passed'".into()],
        true,
    )
    .await;
    broker.run_checks().await.unwrap();
    let execution = broker
        .store
        .load_plan_execution("check-execution")
        .unwrap()
        .unwrap();
    assert_eq!(execution.phase, crate::plan::PlanPhase::Verify);
    assert_eq!(
        execution.check_runs[0].state,
        crate::checks::CheckState::Passed
    );
    let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].id, "check-exchange");
}

#[cfg(windows)]
#[tokio::test]
async fn detached_check_recovery_retains_saved_output_and_pauses_the_gate() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = check_fixture(
        repository.path(),
        data.path(),
        vec!["Write-Output retained".into()],
        true,
    )
    .await;
    broker.run_checks().await.unwrap();
    let mut execution = broker
        .store
        .load_plan_execution("check-execution")
        .unwrap()
        .unwrap();
    execution.phase = crate::plan::PlanPhase::Checking;
    let run = &mut execution.check_runs[0];
    run.state = crate::checks::CheckState::Running;
    run.commands[0].state = crate::checks::CheckState::Running;
    run.commands[0].output_bytes = 0;
    let output = run.commands[0].output.clone();
    update_check_execution(&mut broker, execution);
    broker
        .store
        .interrupt_detached_execution(&broker.session.id)
        .unwrap();
    let recovered = broker
        .store
        .load_plan_execution("check-execution")
        .unwrap()
        .unwrap();
    assert_eq!(recovered.state, PlanExecutionState::Paused);
    assert_eq!(recovered.phase, crate::plan::PlanPhase::Checking);
    assert_eq!(
        recovered.check_runs[0].state,
        crate::checks::CheckState::Interrupted
    );
    assert_eq!(
        recovered.check_runs[0].commands[0].state,
        crate::checks::CheckState::Interrupted
    );
    assert_eq!(
        recovered.check_runs[0].commands[0].output_bytes,
        std::fs::metadata(&output).unwrap().len()
    );
    assert!(
        std::fs::read_to_string(output)
            .unwrap()
            .contains("retained")
    );
    assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
}

#[cfg(windows)]
#[tokio::test]
async fn harness_checks_complete_without_a_manual_exchange_when_no_actions_apply() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = check_fixture(
        repository.path(),
        data.path(),
        vec!["Write-Output complete".into()],
        false,
    )
    .await;
    broker.run_checks().await.unwrap();
    let execution = broker
        .store
        .load_plan_execution("check-execution")
        .unwrap()
        .unwrap();
    assert_eq!(execution.state, PlanExecutionState::Complete);
    assert!(broker.task_controller.current().unwrap().is_none());
    assert!(execution.check_runs[0].completed_plan.is_some());
    let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchange.len(), 1);
    assert_eq!(exchange[0].id, "check-exchange");
}

#[cfg(windows)]
#[tokio::test]
async fn harness_check_cancellation_preserves_results_and_resumes_interrupted_command() {
    for changed in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker=check_fixture(repository.path(),data.path(),vec!["New-Item target -ItemType Directory -Force | Out-Null; Add-Content target/completed.txt first".into(),
        "Set-Content target/started.txt started; if (-not (Test-Path target/resume.txt)) { Start-Sleep 30 }; Write-Output resumed".into()],true).await;
        let cancellation = broker.turn_cancellation();
        let marker = repository.path().join("target/started.txt");
        let cancel = tokio::spawn(async move {
            tokio::time::timeout(std::time::Duration::from_secs(10), async {
                while !marker.is_file() {
                    tokio::time::sleep(std::time::Duration::from_millis(10)).await;
                }
            })
            .await
            .unwrap();
            cancellation.request();
        });
        tokio::time::timeout(std::time::Duration::from_secs(15), broker.run_checks())
            .await
            .unwrap()
            .unwrap();
        cancel.await.unwrap();
        let mut execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(execution.state, PlanExecutionState::Paused);
        assert_eq!(
            execution.check_runs[0].commands[1].state,
            crate::checks::CheckState::Interrupted
        );
        // Reuse is intentionally rejected when user changes workspace contents.
        std::fs::write(repository.path().join("target/resume.txt"), "resume").unwrap();
        if changed {
            std::fs::write(repository.path().join("extra.rs"), "fn helper() {}\n").unwrap();
        }
        broker.turn_cancellation.arm();
        execution.state = PlanExecutionState::Active;
        broker.store.save_plan_execution(&execution).unwrap();
        let task = broker.task_controller.current().unwrap().unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &task.permit(), 100, |task| {
                let crate::task::WorkflowSnapshot::Execute { execution: active } =
                    &mut task.workflow
                else {
                    anyhow::bail!("expected an execution task");
                };
                active.record = execution.clone();
                Ok(())
            })
            .unwrap();
        broker.run_checks().await.unwrap();
        let execution = broker
            .store
            .load_plan_execution("check-execution")
            .unwrap()
            .unwrap();
        assert_eq!(execution.phase, crate::plan::PlanPhase::Verify);
        assert_eq!(execution.check_runs.len(), if changed { 2 } else { 1 });
        assert_eq!(
            std::fs::read_to_string(repository.path().join("target/completed.txt"))
                .unwrap()
                .lines()
                .count(),
            if changed { 2 } else { 1 }
        );
    }
}

#[tokio::test]
async fn saved_checks_refresh_review_without_changing_design_metadata() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"/plan add coverage"}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    run_plan_to_input_boundary(&mut broker).await.unwrap();
    let plan = broker.snapshot().unwrap().active_plan.unwrap();
    let before = broker
        .plan_file
        .read_working_document(&broker.session.id, &plan.id)
        .unwrap();
    std::fs::write(repository.path().join(".forge.json"),serde_json::to_vec(&json!({"checks":{"unit":{"trigger":{"auto":true},"action":{"command":"exit 0","label":"Unit tests","category":"test"}}}})).unwrap()).unwrap();
    assert!(broker.refresh_check_review(&plan.id).unwrap());
    let after = broker
        .plan_file
        .read_working_document(&broker.session.id, &plan.id)
        .unwrap();
    assert_eq!(
        before.design.as_ref().unwrap().document,
        after.design.as_ref().unwrap().document
    );
    assert_eq!(
        after
            .design
            .as_ref()
            .unwrap()
            .check_configuration
            .as_ref()
            .unwrap()
            .config
            .definitions[0]
            .id,
        "unit"
    );
    assert!(!broker.refresh_check_review(&plan.id).unwrap());
}

#[tokio::test]
async fn retired_acceptance_cancellation_preserves_review_state_and_history() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"/plan retain the review questions"}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    run_plan_to_input_boundary(&mut broker).await.unwrap();
    let plan = broker.snapshot().unwrap().active_plan.unwrap();
    broker.capability.approval_mode_list =
        vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow];
    broker
        .dispatch_session_command(
            "session.plan.accept",
            json!({"plan_id":plan.id,"digest":plan.review_digest}),
        )
        .unwrap();
    let before = serde_json::to_value(broker.snapshot().unwrap()).unwrap();
    assert!(!before["active_elicitation"].is_null());
    let stored = serde_json::to_value(broker.store.load_plan(&plan.id).unwrap()).unwrap();
    let history = serde_json::to_value(
        broker
            .store
            .list_plan_lifecycle(&broker.session.id)
            .unwrap(),
    )
    .unwrap();
    let result = broker
        .dispatch(Request {
            id: 2,
            method: "session.plan.acceptance.cancel".into(),
            params: json!({"plan_id":plan.id,"operation_id":"retired-cancel"}),
        })
        .await;
    assert_eq!(result.response.error().unwrap().code, "unknown_method");
    assert!(result.event.is_empty());
    let after = serde_json::to_value(broker.snapshot().unwrap()).unwrap();
    assert_eq!(after["active_elicitation"], before["active_elicitation"]);
    assert_eq!(after["active_plan"], before["active_plan"]);
    assert_eq!(
        serde_json::to_value(broker.store.load_plan(&plan.id).unwrap()).unwrap(),
        stored
    );
    assert_eq!(
        serde_json::to_value(
            broker
                .store
                .list_plan_lifecycle(&broker.session.id)
                .unwrap()
        )
        .unwrap(),
        history
    );
}

#[tokio::test]
async fn deleting_selected_plan_tests_preserves_other_cases_and_rejects_stale_confirmation() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"/plan add test coverage"}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    run_plan_to_input_boundary(&mut broker).await.unwrap();
    let mut plan = broker.snapshot().unwrap().active_plan.unwrap();
    let mut document = broker
        .plan_file
        .read_working_document(&broker.session.id, &plan.id)
        .unwrap();
    document.design.as_mut().unwrap().document.tests = serde_json::from_value(json!([
        {"file":"src/game.rs","cases":[
            {"name":"first","change":"new","description":"First expected result"},
            {"name":"second","change":"new","description":"Second expected result"}]},
        {"file":"tests/game.rs","cases":[
            {"name":"first","change":"new","description":"Separate file"},
            {"name":"fourth","change":"modified","description":"Fourth result"},
            {"name":"keep","change":"reused","description":"Existing coverage"}]}
    ]))
    .unwrap();
    let (_, _, checksum) = broker
        .plan_file
        .submit_validated_document_revision(
            &broker.session.id,
            &plan.id,
            plan.model_revision,
            document.version,
            document.clone(),
        )
        .unwrap();
    plan.review_digest = Some(checksum.clone());
    broker.publish_plan(&plan).unwrap();
    let source = broker
        .observer
        .capture_plan_review(&plan.id, &checksum)
        .unwrap();
    let review = crate::plan::review_document::ReadOnlyPlanReview::new(
        forge_buffer::identity::DocumentId("delete-tests".into()),
        forge_buffer::identity::ViewId("view".into()),
        source,
        Default::default(),
        None,
    )
    .unwrap();
    let store = crate::plan::review_document::PlanReviewStore::default();
    let admission = store
        .admit(forge_buffer::identity::DocumentId("delete-tests".into()))
        .unwrap();
    let opened = store.insert(review, admission).unwrap();
    assert!(
        opened["source_edit"]["upsert"]
            .as_array()
            .unwrap()
            .iter()
            .any(|row| row["test"]["name"] == "fourth")
    );
    let mut annotation = Vec::new();
    for name in ["keep", "fourth"] {
        let line = opened["source_edit"]["upsert"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| row["test"]["name"] == name)
            .unwrap()["source_line"]
            .as_u64()
            .unwrap() as u32;
        annotation.push(crate::plan::ReviewAnnotation {
            id: name.into(),
            parent_id: None,
            source: crate::plan::PlanAnnotationInput {
                start_line: line,
                end_line: line,
                body: format!("Review {name}"),
            },
            anchor: None,
            kind: crate::plan::ReviewAnnotationKind::Comment,
            reply: None,
        });
    }
    store
        .save_annotations(
            &forge_buffer::identity::DocumentId("delete-tests".into()),
            opened["saved_source_digest"].as_str().unwrap(),
            annotation,
        )
        .unwrap();
    let selected = json!([
        {"file":"src/game.rs","name":"first"}, {"file":"src/game.rs","name":"second"},
        {"file":"tests/game.rs","name":"first"}, {"file":"tests/game.rs","name":"fourth"}
    ]);
    let params = json!({"plan_id":plan.id,"digest":checksum,"expected_version":document.version,"tests":selected});
    let mut invalid = params.clone();
    invalid["tests"][3]["name"] = json!("missing");
    assert!(broker.delete_plan_tests(invalid).is_err());
    assert_eq!(
        broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap(),
        document
    );
    let request = Request {
        id: 2,
        method: "session.plan.test.delete".into(),
        params: params.clone(),
    };
    let envelope = json!({"id":request.id,"method":request.method,"params":request.params,
        "session_id":broker.session.id});
    let dispatched = broker.dispatch(request).await;
    assert!(
        dispatched.response.error().is_none(),
        "{:?}",
        dispatched.response.error()
    );
    eprintln!(
        "API_CAPTURE {}",
        json!({"request":envelope,"response":dispatched.response})
    );
    let result = dispatched.response.result().unwrap().clone();
    let updated = broker
        .plan_file
        .read_working_document(&broker.session.id, &plan.id)
        .unwrap();
    let tests = &updated.design.as_ref().unwrap().document.tests;
    assert_eq!(tests.len(), 1);
    assert_eq!(tests[0].file, "tests/game.rs");
    assert_eq!(tests[0].cases.len(), 1);
    assert_eq!(tests[0].cases[0].name, "keep");
    assert_eq!(
        document.design.as_ref().unwrap().proposed,
        updated.design.as_ref().unwrap().proposed
    );
    assert_eq!(updated.version, document.version + 1);
    assert_eq!(result["plan"]["model_revision"], plan.model_revision);
    assert_eq!(result["plan"]["user_revision"], plan.user_revision);
    assert_eq!(
        result["plan"]["submitted_version"],
        serde_json::to_value(plan.submitted_version).unwrap()
    );
    assert!(result.get("lifecycle").is_none());
    assert!(
        broker
            .delete_plan_tests(params)
            .unwrap_err()
            .to_string()
            .contains("plan changed")
    );
    assert_eq!(
        broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap(),
        updated
    );
    let reopened: PlanRecord = serde_json::from_value(result["plan"].clone()).unwrap();
    let source = broker
        .observer
        .capture_plan_review(&plan.id, reopened.review_digest.as_deref().unwrap())
        .unwrap();
    assert!(source.rendered.markdown.contains("keep"));
    let review = crate::plan::review_document::ReadOnlyPlanReview::new(
        forge_buffer::identity::DocumentId("after-delete".into()),
        forge_buffer::identity::ViewId("after-view".into()),
        source,
        Default::default(),
        None,
    )
    .unwrap();
    let admission = store
        .admit(forge_buffer::identity::DocumentId("after-delete".into()))
        .unwrap();
    let opened = store.insert(review, admission).unwrap();
    assert_eq!(opened["annotation"].as_array().unwrap().len(), 1);
    assert_eq!(opened["annotation"][0]["id"], "keep");
    let line = opened["annotation"][0]["source"]["start_line"]
        .as_u64()
        .unwrap();
    assert!(
        opened["source_edit"]["upsert"]
            .as_array()
            .unwrap()
            .iter()
            .any(|row| row["source_line"] == line && row["test"]["name"] == "keep")
    );
    assert!(!repository.path().join("src/game.rs").exists());
    broker
        .delete_plan_tests(json!({"plan_id":plan.id,"digest":reopened.review_digest,
        "expected_version":updated.version,"tests":[{"file":"tests/game.rs","name":"keep"}]}))
        .unwrap();
    assert!(
        broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap()
            .design
            .unwrap()
            .document
            .tests
            .is_empty()
    );
    let draft = broker.store.load_plan(&plan.id).unwrap().unwrap();
    let saved_revision = broker
        .plan_file
        .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision)
        .unwrap();
    assert_eq!(saved_revision, document);
    assert!(
        broker
            .plan_file
            .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
            .is_err()
    );
    broker.capability.approval_mode_list =
        vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow];
    broker.snapshot().unwrap();
    let selected_task = serde_json::to_value(broker.task_controller.current().unwrap()).unwrap();
    assert!(
        broker
            .plan_file
            .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
            .is_err()
    );
    let invalid = broker
        .dispatch(Request {
            id: 2,
            method: "session.plan.accept".into(),
            params: json!({"plan_id":plan.id,"digest":"stale","approval_mode":"ask"}),
        })
        .await;
    assert!(invalid.response.error().is_some());
    assert_eq!(
        serde_json::to_value(broker.task_controller.current().unwrap()).unwrap(),
        selected_task,
        "a rejected plan approval must preserve the active task"
    );
    assert!(
        broker
            .plan_file
            .read_submitted_document(&broker.session.id, &plan.id, plan.model_revision + 1)
            .is_err()
    );
    broker.provider =
        semantic_execution_provider(&broker, plan.id.clone(), u64::from(plan.model_revision + 1));
    let accepted = broker
        .dispatch(Request {
            id: 3,
            method: "session.plan.accept".into(),
            params: json!({"plan_id":plan.id,"digest":draft.review_digest,"approval_mode":"ask"}),
        })
        .await;
    assert!(
        accepted.response.error().is_none(),
        "{:?}",
        accepted.response.error()
    );
    let finalized = broker.store.load_plan(&plan.id).unwrap().unwrap();
    assert_eq!(finalized.model_revision, plan.model_revision + 1);
    assert_eq!(finalized.user_revision, plan.user_revision + 1);
    assert_eq!(finalized.accepted_digest, draft.review_digest);
    assert_eq!(finalized.manual_changes, draft.manual_changes);
    assert_eq!(
        broker
            .plan_file
            .read_submitted_document(&broker.session.id, &plan.id, finalized.model_revision)
            .unwrap(),
        broker
            .plan_file
            .read_working_document(&broker.session.id, &plan.id)
            .unwrap()
    );
    assert_eq!(
        broker
            .store
            .list_plan_lifecycle(&broker.session.id)
            .unwrap()
            .iter()
            .filter(|record| record.kind == PlanLifecycleKind::RevisionCreated)
            .count(),
        1
    );
}

fn workflow_fixture_bridge(
    broker: &HarnessBroker,
    native: std::sync::Arc<crate::provider::test_support::EventFixtureProvider>,
) -> Arc<Provider> {
    Arc::new(Provider::new(
        native.registration(),
        broker.permission_coordinator.clone(),
        broker.trace.clone(),
        broker.data_root.clone(),
    ))
}

fn stopped_execution_provider(broker: &HarnessBroker, cancelled: bool) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Workflow lifecycle fixture".into(),
        capability: Default::default(),
    };
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, _cancelled| async move {
            use forge_provider_api::lifecycle::{
                ExecutionFailure, ExecutionObservation, ExecutionResult, ExecutionUpdate,
            };
            use forge_provider_api::{ProviderEvent, ProviderFailureKind, TurnAddress};

            let address = TurnAddress {
                scope: request.scope.clone(),
                thread_id: "workflow-fixture".into(),
                turn_id: "turn-1".into(),
            };
            sink.send_wait(ProviderEvent::Execution {
                observation: ExecutionObservation::new(
                    request.scope.clone(),
                    ExecutionUpdate::Started {
                        address: address.clone(),
                    },
                )?,
            })
            .await?;
            let completion = forge_provider_api::completion::ProviderCompletion::default();
            let result = if cancelled {
                ExecutionResult::Cancelled {
                    completion: completion.into(),
                }
            } else {
                ExecutionResult::Failed {
                    completion: completion.into(),
                    failure: ExecutionFailure::Terminal {
                        kind: ProviderFailureKind::Transport,
                    },
                }
            };
            sink.send_wait(ProviderEvent::Execution {
                observation: ExecutionObservation::new(
                    request.scope.clone(),
                    ExecutionUpdate::Finished {
                        address,
                        covered_submissions: vec![request.submission_id],
                        completed_at_ms: 1,
                        result,
                    },
                )?,
            })
            .await?;
            Ok::<(), forge_provider_api::ProviderError>(())
        },
    );
    workflow_fixture_bridge(broker, native)
}

fn no_progress_execution_provider(broker: &HarnessBroker, plan_id: &str) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Workflow tool fixture".into(),
        capability: Default::default(),
    };
    let plan_id = plan_id.to_owned();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, _cancelled| {
            let plan_id = plan_id.clone();
            async move {
                use forge_provider_api::completion::{ProviderCompletion, TurnEvidence};
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::{ProviderEvent, TurnAddress};

                let _ = crate::provider::test_support::call_tool(
                    &request,
                    "plan_overview_read",
                    json!({"plan_id":plan_id}),
                )
                .await?;
                assert!(
                    crate::provider::test_support::call_tool(
                        &request,
                        "goal_complete",
                        json!({"summary":"premature"}),
                    )
                    .await
                    .is_err()
                );

                let address = TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: "workflow-fixture".into(),
                    turn_id: "turn-1".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                let completion = ProviderCompletion {
                    evidence: TurnEvidence {
                        tool_called: true,
                        structured_complete: true,
                        ..Default::default()
                    },
                    ..Default::default()
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 1,
                            result: ExecutionResult::Completed {
                                completion: completion.into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok::<(), forge_provider_api::ProviderError>(())
            }
        },
    );
    workflow_fixture_bridge(broker, native)
}

fn semantic_execution_provider(
    broker: &HarnessBroker,
    plan_id: String,
    revision: u64,
) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Semantic execution fixture".into(),
        capability: Default::default(),
    };
    let turn = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let descriptor = descriptor.clone();
            let turn = turn.clone();
            let plan_id = plan_id.clone();
            async move {
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::tool::{ToolInvocation, ToolOutcome, ToolUpdate};
                use forge_provider_api::{ProviderEvent, ToolAddress, TurnAddress};

                let index = turn.fetch_add(1, Ordering::SeqCst);
                let scope = request.scope.clone();
                let address = TurnAddress {
                    scope: scope.clone(),
                    thread_id: "semantic-execution-fixture".into(),
                    turn_id: format!("phase-{index}").into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started {
                        started_at_ms: index as i64,
                    },
                })
                .await?;

                let overview = crate::provider::test_support::call_tool(
                    &request,
                    "plan_overview_read",
                    json!({"plan_id":plan_id}),
                )
                .await?;
                let expected_phase = match index {
                    0 => "implement",
                    1 | 3 | 4 => "verify",
                    _ => "resolve",
                };
                assert_eq!(
                    overview.pointer("/execution/phase").and_then(Value::as_str),
                    Some(expected_phase)
                );
                let version = overview
                    .get("version")
                    .and_then(Value::as_u64)
                    .unwrap_or(revision);

                if index == 0 {
                    assert!(crate::provider::test_support::call_tool(
                        &request,
                        "plan_file_edit",
                        json!({"plan_id":plan_id,"expected_version":version,"patch":"*** Begin Patch\n*** End Patch"}),
                    ).await.is_err(), "implementation phase exposed plan editing");
                }

                let mut evidence = Vec::new();
                if matches!(index, 1 | 3 | 4) {
                    let id = format!("check-{index}");
                    let tool_address = ToolAddress {
                        turn: address.clone(),
                        tool_id: id.clone().into(),
                    };
                    let invocation = ToolInvocation {
                        semantics: Default::default(),
                        kind: forge_provider_api::ToolActivityKind::Command,
                        name: "fixture".into(),
                        title: "Observe fixture".into(),
                        arguments: Value::Null,
                    };
                    sink.send_wait(ProviderEvent::Tool {
                        address: tool_address.clone(),
                        update: ToolUpdate::Requested { invocation },
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Tool {
                        address: tool_address.clone(),
                        update: ToolUpdate::Started,
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Tool {
                        address: tool_address.clone(),
                        update: ToolUpdate::Output {
                            update: forge_provider_api::tool::TextUpdate::Append(
                                if index == 1 {
                                    "round reset failed"
                                } else {
                                    "all checks passed"
                                }
                                .into(),
                            ),
                        },
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Tool {
                        address: tool_address,
                        update: ToolUpdate::Finished {
                            outcome: ToolOutcome::Succeeded,
                        },
                    })
                    .await?;
                    let observed = crate::provider::test_support::call_tool(
                        &request,
                        "plan_overview_read",
                        json!({"plan_id":plan_id}),
                    )
                    .await?;
                    let available = observed
                        .pointer("/execution/verification_evidence")
                        .and_then(Value::as_array)
                        .expect("verification evidence is returned in Verify");
                    let evidence_item = available
                        .iter()
                        .find(|item| item.get("id").and_then(Value::as_str) == Some(id.as_str()))
                        .expect("the completed tool call is available as evidence");
                    assert_eq!(
                        evidence_item.get("title").and_then(Value::as_str),
                        Some("Observe fixture")
                    );
                    evidence.push(id);
                }

                let phase = match index {
                    0 => "implement",
                    2 => "resolve",
                    _ => "verify",
                };
                let outcome = if index == 1 { "failed" } else { "passed" };
                let summary = if index == 1 {
                    "Round reset retains score"
                } else {
                    "Checked planned behavior"
                };
                let checks = if phase == "verify" {
                    vec![json!({
                        "category":"manual", "label":"fixture check", "outcome":outcome,
                        "summary":summary, "evidence":evidence
                    })]
                } else {
                    Vec::<Value>::new()
                };
                let valid = json!({
                    "phase":phase, "revision":revision, "summary":summary,
                    "verification":{
                        "outcome":outcome, "checks":checks, "evidence":evidence,
                        "findings":if index == 1 { vec!["Round reset retains score"] } else { Vec::<&str>::new() }
                    }
                });
                if index == 1 {
                    let mut invalid = valid.clone();
                    invalid["verification"]["evidence"] = json!(["unobserved-check"]);
                    assert!(
                        crate::provider::test_support::call_tool(
                            &request,
                            "plan_phase_done",
                            invalid,
                        )
                        .await
                        .is_err(),
                        "unobserved completion ID was accepted as verification evidence"
                    );
                }
                if index == 3 {
                    let mut invalid = valid.clone();
                    invalid["verification"]["evidence"] = json!(["check-1"]);
                    assert!(
                        crate::provider::test_support::call_tool(
                            &request,
                            "plan_phase_done",
                            invalid,
                        )
                        .await
                        .is_err(),
                        "reused verification evidence was accepted"
                    );
                }
                crate::provider::test_support::call_tool(&request, "plan_phase_done", valid)
                    .await?;

                for event in completed_message(
                    &address,
                    format!("semantic-response-{index}"),
                    crate::provider::MessageRole::Response,
                    "Phase evidence recorded.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: index as i64 + 1,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope,
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: index as i64 + 1,
                            result: ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok::<(), forge_provider_api::ProviderError>(())
            }
        },
    );
    workflow_fixture_bridge(broker, fixture)
}

#[tokio::test]
async fn obsolete_task_sessions_remain_hidden_without_reactivation() -> Result<()> {
    for executing in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.provider = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            broker.permission_coordinator.clone(),
            broker.trace.clone(),
            broker.data_root.clone(),
        )?;
        broker
            .submit_prompt(json!({"text":"/plan add a reusable API"}))
            .await?;
        run_plan_to_input_boundary(&mut broker).await?;
        if executing {
            let plan = broker
                .snapshot()?
                .active_plan
                .context("test plan missing")?;
            let fixture_provider = stopped_execution_provider(&broker, true);
            broker.provider = fixture_provider;
            broker.dispatch_session_command(
                "session.plan.accept",
                json!({"plan_id":plan.id,"approval_mode":"ask"}),
            )?;
        }
        let before = broker
            .store
            .load_active_task(&broker.session.id)?
            .context("task missing before migration")?;
        let plan_id = match &before.workflow {
            crate::task::WorkflowSnapshot::Plan { plan } => plan.artifact.id.clone(),
            crate::task::WorkflowSnapshot::Execute { execution } => {
                execution.record.plan_id.clone()
            }
            crate::task::WorkflowSnapshot::Goal { .. } => unreachable!(),
        };
        let title = broker.store.load_plan(&plan_id)?.unwrap().title;
        let session_id = broker.session.id.clone();
        let path = broker.data_root.join("harness.sqlite3");
        let connection = rusqlite::Connection::open(&path)?;
        connection.execute("UPDATE session_record SET payload=json_set(payload,'$.format_version',51,'$.session.active_task',NULL) WHERE id=?1", [&session_id])?;
        connection.pragma_update(None, "user_version", 3)?;
        drop(connection);
        drop(broker);
        let migrated = SqliteStore::open(data.path())?;
        assert!(migrated.load_session(&session_id)?.is_none());
        assert!(migrated.load_active_task(&session_id).is_err());
        let connection = rusqlite::Connection::open(&path)?;
        let version: i64 = connection.query_row(
            "SELECT json_extract(payload, '$.format_version') FROM session_record WHERE id=?1",
            [&session_id],
            |row| row.get(0),
        )?;
        assert_eq!(
            version, 51,
            "storage schema setup rewrote an obsolete session"
        );
        assert_eq!(migrated.load_plan(&plan_id)?.unwrap().title, title);
    }
    Ok(())
}

#[tokio::test]
async fn execution_failure_and_cancellation_resume_the_phase_without_accepting_again() {
    for cancelled in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.provider = crate::provider::build(
            ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            Arc::clone(&broker.permission_coordinator),
            Arc::clone(&broker.trace),
            broker.data_root.clone(),
        )
        .unwrap();
        broker
            .submit_prompt(json!({"text":"/plan add a reusable API"}))
            .await
            .unwrap();
        run_plan_to_input_boundary(&mut broker).await.unwrap();
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        let plan_id = plan.id.clone();
        let fixture_provider = stopped_execution_provider(&broker, cancelled);
        broker.provider = fixture_provider;
        broker
            .dispatch_session_command(
                "session.plan.accept",
                json!({"plan_id":plan_id.clone(),"approval_mode":"ask"}),
            )
            .unwrap();
        let result = dispatch_pending_task_input(&mut broker).await;
        assert!(result.is_err(), "stopped provider unexpectedly succeeded");
        let history = broker.store.list_exchange(&broker.session.id).unwrap();
        let stopped = history.last().unwrap();
        assert_eq!(
            stopped.state(),
            if cancelled {
                ExchangeState::Cancelled
            } else {
                ExchangeState::Failed
            }
        );
        assert!(
            stopped
                .node_list
                .iter()
                .all(|node| !matches!(node, ExchangeNode::VerificationResult { .. }))
        );
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let text = timeline_text(&broker.snapshot().unwrap());
        assert_eq!(text.contains("Plan failed:"), !cancelled, "{text}");
        assert_eq!(text.matches("Plan accepted:").count(), 1);
        let stopped_id = stopped.id.clone();
        let fixture_provider = no_progress_execution_provider(&broker, &plan_id);
        broker.provider = fixture_provider;
        broker
            .dispatch_session_command("session.goal.resume", json!({}))
            .unwrap();
        dispatch_pending_task_input(&mut broker).await.unwrap();
        let resumed = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_ne!(resumed.last().unwrap().id, stopped_id);
        assert!(matches!(
            &resumed.last().unwrap().origin,
            crate::exchange::ExchangeOrigin::TaskStarted { start }
                if start.kind == crate::task::TaskKind::Execute
        ));
        assert_eq!(
            timeline_text(&broker.snapshot().unwrap())
                .matches("Plan accepted:")
                .count(),
            1
        );
    }
}

#[tokio::test]
async fn semantic_execution_stalls_after_five_read_only_turns_and_resumes_the_same_phase() {
    let repository = repository();
    crate::checks::configure_fixture(repository.path());
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "guard-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "medium".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    broker
        .submit_prompt(json!({"text":"/plan add a reusable API"}))
        .await
        .unwrap();
    run_plan_to_input_boundary(&mut broker).await.unwrap();
    let identity = broker
        .task_controller
        .current()
        .unwrap()
        .unwrap()
        .id
        .clone();
    let planning_attempt_count = broker
        .store
        .list_exchange(&broker.session.id)
        .unwrap()
        .len();
    let plan = broker.snapshot().unwrap().active_plan.unwrap();
    let fixture_provider = no_progress_execution_provider(&broker, &plan.id);
    broker.provider = fixture_provider;
    broker
        .dispatch_session_command(
            "session.plan.accept",
            json!({"plan_id":plan.id,"approval_mode":"ask"}),
        )
        .unwrap();
    for turn in 1..=5 {
        dispatch_pending_task_input(&mut broker).await.unwrap();
        let current = broker.task_controller.current().unwrap().unwrap();
        assert_eq!(current.id, identity);
        assert_eq!(
            current.status(),
            if turn < 5 {
                crate::task::TaskStatus::Running
            } else {
                crate::task::TaskStatus::Blocked
            }
        );
    }
    let stalled = active_execution(&broker);
    assert_eq!(stalled.state, PlanExecutionState::Stalled);
    assert_eq!(stalled.phase, crate::plan::PlanPhase::Implement);
    assert!(stalled.completed_at_ms.is_none());
    assert_eq!(broker.active_goal().unwrap().continuation.turn_count, 6);
    let attempts = broker
        .store
        .list_exchange(&broker.session.id)
        .unwrap()
        .into_iter()
        .skip(planning_attempt_count)
        .collect::<Vec<_>>();
    assert_eq!(attempts.len(), 5);
    assert_ne!(attempts[0].id, attempts[1].id);
    assert_eq!(attempts[0].state(), ExchangeState::Complete);
    assert!(
        !attempts[0]
            .node_list
            .iter()
            .any(|node| matches!(node, ExchangeNode::VerificationResult { .. }))
    );
    assert!(matches!(
        &attempts[1].origin,
        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
    ));
    assert_eq!(attempts[0].session_id, attempts[1].session_id);
    broker
        .dispatch_session_command("session.goal.resume", json!({}))
        .unwrap();
    dispatch_pending_task_input(&mut broker).await.unwrap();
    let resumed = active_execution(&broker);
    assert_eq!(resumed.id, stalled.id);
    assert_eq!(resumed.state, PlanExecutionState::Active);
    assert_eq!(resumed.phase, stalled.phase);
    assert_eq!(broker.active_goal().unwrap().continuation.turn_count, 1);
}

/// Render persisted timeline state through the native document projection.
fn timeline_text(snapshot: &BrokerSnapshot) -> String {
    snapshot
        .timeline
        .iter()
        .flat_map(|entry| {
            let rendered = crate::buffer::projection::project(
                entry,
                &Default::default(),
                false,
                &Default::default(),
                &Default::default(),
                &mut crate::buffer::identity::DisplayRegistry::default(),
            )
            .unwrap();
            rendered
                .entry
                .block
                .into_iter()
                .flat_map(|block| {
                    (0..block.text.row_count())
                        .map(|row| block.text.row(row).unwrap().to_owned())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn resolves_explicit_named_and_unnamed_fork_names() {
    let existing_name_set = HashSet::from(["review-fork-0".to_owned()]);
    assert_eq!(
        resolve_fork_name(
            &json!({ "name": "  investigation  " }),
            "review",
            &existing_name_set,
        ),
        "investigation"
    );
    assert_eq!(
        resolve_fork_name(&json!({}), "review", &existing_name_set),
        "review-fork-1"
    );
    assert_eq!(resolve_fork_name(&json!({}), "", &HashSet::new()), "fork-0");
}

pub(super) fn completed_interaction(
    id: &str,
    session_id: &str,
    node_list: Vec<ExchangeNode>,
) -> Exchange {
    Exchange {
        approval_mode: None,
        finalization_error: None,
        check_owner: None,
        output_store: None,
        archived_output: Default::default(),
        input_message_id: None,
        provider_retry: None,
        agent_id: HarnessSession::primary_agent_id(session_id),
        id: id.into(),
        session_id: session_id.into(),
        ordinal: 1,
        prompt: "test interaction".into(),
        origin: crate::exchange::ExchangeOrigin::User,

        phase: crate::exchange::ExchangePhase::fixture(ExchangeState::Complete, Some(2), None),
        checkpoint_before: None,
        checkpoint_after: None,
        attributed_diff_text: None,
        checkpoint_diff_text: None,
        attributed_matches_checkpoint: false,
        approval_resolution: Vec::new(),
        check_setup: None,
        turn: Vec::new(),
        created_at_ms: 1,
        entry: Vec::new(),
        tool_counter: Default::default(),
        node_list,
        metrics: crate::exchange::ExchangeMetrics::default(),
        comment: Vec::new(),
        checklist: None,

        question: crate::exchange::ExchangeQuestion::None,
        clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 1 },
    }
}

fn saturating_provider(
    broker: &HarnessBroker,
    completed: bool,
    wait_for_capacity: bool,
    cancelled: Arc<AtomicBool>,
) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Event delivery fixture".into(),
        capability: Default::default(),
    };
    let cleanup_cancelled = cancelled.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, mut cancellation| {
            let cancelled = cancelled.clone();
            async move {
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::message::{
                    MessageClassification, MessageId, MessageRole, MessageUpdate,
                };
                use forge_provider_api::{MessageAddress, ProviderEvent, TurnAddress};

                let turn = TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: "delivery-fixture-session".into(),
                    turn_id: "delivery-fixture-turn".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: turn.clone(),
                        },
                    )?,
                })
                .await?;
                let address = MessageAddress {
                    turn: turn.clone(),
                    message_id: MessageId::Native("saturated-message".into()),
                };
                let mut text = String::new();
                for sequence in 0..384 {
                    let delta = sequence.to_string();
                    text.push_str(&delta);
                    let update = if sequence == 0 {
                        MessageUpdate::Started {
                            classification: MessageClassification::Known(MessageRole::Response),
                            text: delta,
                        }
                    } else {
                        MessageUpdate::Delta { text: delta }
                    };
                    let event = ProviderEvent::Message {
                        address: address.clone(),
                        update,
                    };
                    if wait_for_capacity {
                        sink.send_wait(event).await?;
                    } else {
                        sink.send(event)?;
                    }
                }
                if wait_for_capacity {
                    sink.send_wait(ProviderEvent::Message {
                        address: address.clone(),
                        update: MessageUpdate::Completed { text },
                    })
                    .await?;
                }
                if completed {
                    sink.send_wait(ProviderEvent::Execution {
                        observation: ExecutionObservation::new(
                            request.scope.clone(),
                            ExecutionUpdate::Finished {
                                address: turn,
                                covered_submissions: vec![request.submission_id],
                                completed_at_ms: 100,
                                result: ExecutionResult::Completed {
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion::default(
                                        )
                                        .into(),
                                },
                            },
                        )?,
                    })
                    .await?;
                    return Ok::<(), forge_provider_api::ProviderError>(());
                }
                if cancellation.changed().await.is_ok() {
                    cancelled.store(true, Ordering::Release);
                }
                Ok(())
            }
        },
    )
    .with_cleanup_request(move |_binding| {
        let cancelled = cleanup_cancelled.clone();
        async move {
            cancelled.store(true, Ordering::Release);
            Ok::<(), forge_provider_api::ProviderError>(())
        }
    });
    workflow_fixture_bridge(broker, native)
}
#[tokio::test]
async fn event_saturation_cancels_pending_and_completed_provider_turns() {
    for completed in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                provider_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data.path().to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.path().to_string_lossy().into_owned(),
                client_id: "saturation-test".into(),
                provider: ProviderLaunch {
                    kind: forge_providers::registry::nonselectable_id()
                        .expect("registry supplies a nonselectable fixture identity"),
                    command: vec!["mock".into()],
                },
                model: "mock-model".into(),
                effort: "low".into(),
                session_id: None,
                new_session_name: None,
                goal_max_turns: 20,
                lease_conflict_action: None,
            },
            Box::new(FixedClock(200)),
        )
        .unwrap();
        let cancelled = Arc::new(AtomicBool::new(false));
        let fixture_provider = saturating_provider(&broker, completed, false, cancelled.clone());
        broker.provider = fixture_provider;
        let admitted = broker
            .dispatch(Request {
                id: 1,
                method: "session.goal.set".into(),
                params: json!({ "objective": "test bounded provider delivery" }),
            })
            .await;
        assert!(admitted.response.error().is_none());
        let error = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            dispatch_pending_task_input(&mut broker),
        )
        .await
        .expect("saturation must terminate the affected turn")
        .expect_err("saturation must not report success");
        assert!(
            error
                .downcast_ref::<crate::provider::events::EventDeliveryError>()
                .is_some()
                || matches!(
                    error.downcast_ref::<crate::provider::ProviderError>(),
                    Some(crate::provider::ProviderError::Delivery(_))
                ),
            "{error:?}"
        );
        assert!(cancelled.load(Ordering::Acquire));
        assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
        let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].state(), ExchangeState::Failed);
    }
}

#[tokio::test]
async fn burst_delivery_drains_both_queues_and_keeps_the_next_prompt_usable() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "burst-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();
    let cancelled = Arc::new(AtomicBool::new(false));
    let fixture_provider = saturating_provider(&broker, true, true, cancelled.clone());
    broker.provider = fixture_provider;
    for request_id in 1..=2 {
        let session_id = broker.session.id.clone();
        let (sink, mut stream) = publication::channel();
        let dispatch = broker.dispatch_stream(
            Request {
                id: request_id,
                method: "session.prompt.submit".into(),
                params: json!({ "text": "burst then final answer" }),
            },
            sink,
        );
        tokio::pin!(dispatch);
        // Hold the consumer until the producer reaches bounded backpressure.
        assert!(
            std::future::poll_fn(|context| {
                std::task::Poll::Ready(std::future::Future::poll(dispatch.as_mut(), context))
            })
            .await
            .is_pending()
        );
        let consume = async move {
            let mut notifications = 0;
            while let Some(event) = stream.recv().await.unwrap() {
                let channel = if matches!(event, SessionUpdate::DocumentChanged { .. }) {
                    notifications += 1;
                    "document_changed"
                } else {
                    "provider_event"
                };
                let wire = forge_protocol::message::Message::Event(SessionEvent {
                    session_id: session_id.clone(),
                    event: channel.into(),
                    payload: publication::encode_session_update(&event).unwrap(),
                });
                serde_json::to_value(wire)
                    .expect("broker notifications must satisfy the editor wire contract");
            }
            assert!(notifications > 0);
        };
        let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(10), async {
            tokio::join!(dispatch, consume)
        })
        .await
        .expect("bounded delivery must make progress");
        assert!(
            result.response.error().is_none(),
            "{:?}",
            result.response.error()
        );
    }
    assert!(!cancelled.load(Ordering::Acquire));
    let exchange = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchange.len(), 2);
    assert!(
        exchange
            .iter()
            .all(|exchange| exchange.state() == ExchangeState::Complete)
    );
    assert!(exchange.iter().all(|exchange| {
        exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text.ends_with("383"))
    }));
}

#[tokio::test]
async fn automatic_resume_excludes_answer_forks() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let broker = planning_question_broker(repository.path(), data.path(), true);
    let source_id = broker.session.id.clone();
    let child = prepare_provider_fork(
        data.path(),
        "answer-client",
        "mock",
        &broker.capability,
        &source_id,
        &json!({"name":"Plan Review answers"}),
        crate::session::SessionPurpose::PlanReviewAnswer,
    )?;
    assert_eq!(
        child.child.purpose,
        crate::session::SessionPurpose::PlanReviewAnswer
    );
    let mut legacy = serde_json::to_value(&broker.session)?;
    legacy.as_object_mut().unwrap().remove("purpose");
    assert_eq!(
        serde_json::from_value::<HarnessSession>(legacy)?.purpose,
        crate::session::SessionPurpose::Conversation
    );
    drop(broker);
    let resumed = planning_question_broker(repository.path(), data.path(), true);
    assert_eq!(resumed.session.id, source_id);
    assert!(resumed.store.load_session(&child.child.id)?.is_some());
    Ok(())
}

/// Coordinate the fixture's native completion and retryable cleanup barriers.
struct CleanupSignals {
    scope: std::sync::Mutex<Option<crate::provider::InvocationScope>>,
    started: Notify,
    complete: Notify,
    settled: Notify,
    native_settled: AtomicBool,
    attempt: std::sync::atomic::AtomicUsize,
}

fn cleanup_retry_provider(
    broker: &HarnessBroker,
    visible_output: bool,
    fail_first_cleanup: bool,
) -> (Arc<Provider>, Arc<CleanupSignals>) {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Cleanup lifecycle fixture".into(),
        capability: crate::provider::ProviderCapability {
            native_turn_rollback: true,
            ..Default::default()
        },
    };
    let signals = Arc::new(CleanupSignals {
        scope: std::sync::Mutex::new(None),
        started: Notify::new(),
        complete: Notify::new(),
        settled: Notify::new(),
        native_settled: AtomicBool::new(false),
        attempt: std::sync::atomic::AtomicUsize::new(usize::from(!fail_first_cleanup)),
    });
    let handler_signals = signals.clone();
    let cleanup_signals = signals.clone();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, _cancelled| {
            let signals = handler_signals.clone();
            async move {
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::message::{
                    MessageClassification, MessageId, MessageRole, MessageUpdate,
                };
                use forge_provider_api::{MessageAddress, ProviderEvent, TurnAddress};

                let address = TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: "cleanup-fixture-session".into(),
                    turn_id: "cleanup-fixture-turn".into(),
                };
                let checkpoint = format!(
                    "cleanup-fixture:{}:{}",
                    request.submission_id.as_str(),
                    request.attempt_id.as_str()
                );
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                if visible_output {
                    sink.send_wait(ProviderEvent::Message {
                        address: MessageAddress {
                            turn: address.clone(),
                            message_id: MessageId::Native("partial-response".into()),
                        },
                        update: MessageUpdate::Started {
                            classification: MessageClassification::Known(MessageRole::Response),
                            text: "Partial response".into(),
                        },
                    })
                    .await?;
                }
                *signals.scope.lock().unwrap() = Some(request.scope.clone());
                signals.started.notify_one();
                signals.complete.notified().await;
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 100,
                            result: ExecutionResult::Cancelled {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    provider_checkpoint_id: Some(checkpoint),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                signals.native_settled.store(true, Ordering::Release);
                signals.settled.notify_waiters();
                Ok::<(), forge_provider_api::ProviderError>(())
            }
        },
    )
    .with_cleanup_request(move |_binding| {
        let signals = cleanup_signals.clone();
        async move {
            if signals.attempt.fetch_add(1, Ordering::SeqCst) == 0 {
                return Err(forge_provider_api::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::Cancel,
                        forge_provider_api::ProviderFailureKind::Timeout,
                    ),
                ));
            }
            signals.complete.notify_one();
            loop {
                let settled = signals.settled.notified();
                tokio::pin!(settled);
                settled.as_mut().enable();
                if signals.native_settled.load(Ordering::Acquire) {
                    break;
                }
                settled.await;
            }
            Ok(())
        }
    });
    (workflow_fixture_bridge(broker, native), signals)
}
#[tokio::test]
async fn cancelling_paused_restart_persists_goal_and_exchange_stop() {
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker
        .create_goal("Complete implementation".into())
        .unwrap();
    let mut exchange = completed_interaction("paused-restart", &broker.session.id, Vec::new());
    exchange.phase = crate::exchange::ExchangePhase::Running;
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        exchange.state(),
        None,
        exchange.finalization_outcome(),
    );
    broker.store.save_exchange(&exchange).unwrap();
    broker.cancel_settled_execution().await.unwrap();
    assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
    let stopped = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(stopped[0].state(), ExchangeState::Cancelled);
    broker.cancel_settled_execution().await.unwrap();
    assert_eq!(
        broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .len(),
        1
    );
}

#[cfg(windows)]
#[tokio::test]
async fn goal_resume_retries_locked_checkpoint_before_provider_work() {
    use std::os::windows::fs::OpenOptionsExt;

    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.create_goal("Finish implementation".into()).unwrap();
    broker
        .task_controller
        .change(
            &mut broker.store,
            broker
                .task_controller
                .current()
                .unwrap()
                .as_ref()
                .map(|task| &task.revision),
            crate::task::TaskChange::Pause,
            2,
        )
        .unwrap();
    let before = GitCheckpoint::new(repository.path())
        .capture(
            &broker.store.objects,
            &broker.repositories,
            &broker.session.id,
            1,
        )
        .await
        .unwrap();
    let mut exchange = completed_interaction("locked-checkpoint", &broker.session.id, Vec::new());
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Finalizing,
        exchange.completed_at_ms(),
        exchange.finalization_outcome(),
    );
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        exchange.state(),
        None,
        exchange.finalization_outcome(),
    );
    exchange.phase =
        crate::exchange::ExchangePhase::Finalizing(crate::exchange::ExchangeOutcome::Cancelled);
    exchange.checkpoint_before = Some(before.id.clone());
    broker
        .store
        .save_checkpoint_exchange(&before, &exchange)
        .unwrap();
    std::fs::write(
        repository.path().join("seed.txt"),
        "changed after checkpoint\n",
    )
    .unwrap();
    let locked = std::fs::OpenOptions::new()
        .read(true)
        .share_mode(0)
        .open(repository.path().join("seed.txt"))
        .unwrap();
    broker
        .dispatch_session_command("session.goal.resume", json!({}))
        .unwrap();
    let error = dispatch_pending_task_input(&mut broker).await.unwrap_err();
    assert!(format!("{error:#}").contains("seed.txt"));
    assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
    let stored = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(stored.len(), 2);
    assert_eq!(stored[1].state(), ExchangeState::Failed);
    assert!(stored[1].turn.is_empty());
    assert_eq!(stored[0].state(), ExchangeState::Finalizing);
    drop(locked);

    broker
        .dispatch_session_command("session.goal.resume", json!({}))
        .unwrap();
    dispatch_pending_task_input(&mut broker).await.unwrap();
    let stored = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(stored[0].state(), ExchangeState::Cancelled);
    assert!(stored[0].finalization_error.is_none());
    assert!(stored[0].checkpoint_after.is_some());
}

#[tokio::test]
async fn first_prompt_names_unnamed_sessions_without_overwriting_explicit_names() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session = broker
        .store
        .rename_owned_session(&broker.session.id, &broker.client_id, "", None, 100)
        .unwrap();
    broker
        .test_stage_interaction_for_turn("  Explain\n this repository  ", true, 100)
        .await
        .unwrap();
    assert_eq!(broker.session.name, "Explain this repository");
    assert_eq!(
        broker
            .store
            .load_session(&broker.session.id)
            .unwrap()
            .unwrap()
            .name,
        "Explain this repository"
    );
    broker
        .test_stage_interaction_for_turn("Another prompt", true, 101)
        .await
        .unwrap();
    assert_eq!(broker.session.name, "Explain this repository");
    broker.session.name = "My explicit name".into();
    broker
        .test_stage_interaction_for_turn("Do not replace this name", true, 102)
        .await
        .unwrap();
    assert_eq!(broker.session.name, "My explicit name");
}

#[tokio::test]
async fn first_slash_prompt_names_session_from_its_request() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session = broker
        .store
        .rename_owned_session(&broker.session.id, &broker.client_id, "", None, 100)
        .unwrap();
    broker
        .test_stage_interaction_for_turn("/plan convert to rust", true, 100)
        .await
        .unwrap();
    assert_eq!(broker.session.name, "convert to rust");
    assert_eq!(
        broker
            .store
            .load_session(&broker.session.id)
            .unwrap()
            .unwrap()
            .name,
        "convert to rust"
    );
}

#[tokio::test]
async fn first_slash_prompt_caps_persisted_session_name_at_thirty_characters() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.name.clear();
    broker
        .test_stage_interaction_for_turn(
            "/plan convert a rather long repository from TypeScript to Rust",
            true,
            100,
        )
        .await
        .unwrap();
    assert_eq!(broker.session.name, "convert a rather long reposito");
    assert_eq!(
        broker
            .store
            .load_session(&broker.session.id)
            .unwrap()
            .unwrap()
            .name,
        "convert a rather long reposito"
    );
}

#[tokio::test]
async fn conversation_rollback_preserves_files_and_restarts_with_retained_context() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut first = completed_interaction("retained", &broker.session.id, Vec::new());
    first.prompt = "KEEP_THIS_CONTEXT".into();
    let mut second = completed_interaction("removed", &broker.session.id, Vec::new());
    second.ordinal = 2;
    second.prompt = "DROP_THIS_CONTEXT".into();
    assert!(second.checkpoint_before.is_none());
    broker.store.save_exchange(&first)?;
    broker.store.save_exchange(&second)?;
    broker.session.provider_session_id = Some("old-provider-history".into());
    broker.save_session()?;
    std::fs::write(
        repository.path().join("seed.txt"),
        "Keep these file changes\n",
    )?;
    std::fs::write(
        repository.path().join("new-file.txt"),
        "Keep this new file\n",
    )?;
    let mut params =
        json!({"scope":"conversation","exchange_id":"removed","latest_exchange_id":"stale"});
    assert!(broker.apply_rollback(params.clone()).await.is_err());
    assert_eq!(
        broker.store.list_exchange(&broker.session.id)?[1].disposition(),
        crate::exchange::HistoryDisposition::Current
    );
    params["latest_exchange_id"] = json!("removed");
    let (result, events) = broker.apply_rollback(params).await?;
    assert_eq!(result["scope"], "conversation");
    assert!(
        events
            .iter()
            .any(|event| event.event == "interaction_rolled_back")
    );
    assert_eq!(
        std::fs::read_to_string(repository.path().join("seed.txt"))?,
        "Keep these file changes\n"
    );
    assert_eq!(
        std::fs::read_to_string(repository.path().join("new-file.txt"))?,
        "Keep this new file\n"
    );
    assert!(
        crate::checkpoint::RestoreJournal::progress(&broker.store.objects, &broker.session.id)?
            .is_none()
    );
    assert!(
        broker
            .store
            .load_session(&broker.session.id)?
            .unwrap()
            .provider_session_id
            .is_none()
    );
    assert!(
        !broker
            .snapshot()?
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == "removed"))
    );
    let input =
        broker.rollback_context("new-request", ProviderInput::from_text("CURRENT_REQUEST"))?;
    let ProviderInput::Text { text, .. } = input else {
        unreachable!()
    };
    assert!(text.contains("KEEP_THIS_CONTEXT") && text.contains("CURRENT_REQUEST"));
    assert!(!text.contains("DROP_THIS_CONTEXT"));
    Ok(())
}

#[tokio::test]
async fn consecutive_rollbacks_follow_restored_workspace_history() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut exchange_id = Vec::new();
    for content in ["first\n", "second\n"] {
        let (mut exchange, _, _) = broker
            .test_stage_interaction_for_turn(content, true, 100)
            .await
            .unwrap();
        broker.begin_exchange_runtime(&mut exchange, 100).unwrap();
        let mut runtime = broker.exchange_runtime.take().unwrap();
        broker
            .collect_initial_checkpoint(&mut runtime, &mut exchange)
            .await
            .unwrap();
        broker.exchange_runtime = Some(runtime);
        std::fs::write(repository.path().join("seed.txt"), content).unwrap();
        broker
            .capture_final_checkpoint(&mut exchange, crate::exchange::ExchangeOutcome::Complete)
            .await
            .unwrap();
        exchange_id.push(exchange.id);
        broker.exchange_runtime = None;
    }
    broker
        .rollback_exchange(json!({"exchange_id":exchange_id[1]}))
        .await
        .unwrap();
    assert_eq!(
        std::fs::read_to_string(repository.path().join("seed.txt")).unwrap(),
        "first\n"
    );
    let visible = broker.snapshot().unwrap().timeline;
    assert!(
        visible.iter().any(
            |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange_id[0])
        )
    );
    assert!(
        !visible.iter().any(
            |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange_id[1])
        ),
        "rollback must remove the reverted request instead of leaving a timeline tombstone"
    );
    assert!(
        broker
            .rollback_exchange(json!({"exchange_id":exchange_id[1]}))
            .await
            .is_err()
    );
    broker
        .rollback_exchange(json!({"exchange_id":exchange_id[0]}))
        .await
        .unwrap();
    assert_eq!(
        std::fs::read_to_string(repository.path().join("seed.txt")).unwrap(),
        "seed\n"
    );
    let history = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(history[0].state(), ExchangeState::Complete);
    assert_eq!(
        history[0].disposition(),
        crate::exchange::HistoryDisposition::RolledBack
    );
    assert_eq!(
        history[1].disposition(),
        crate::exchange::HistoryDisposition::Superseded
    );
    assert!(
        !broker
            .snapshot()
            .unwrap()
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Exchange { .. })),
        "rolled-back and superseded requests must stay out of the rebuilt timeline"
    );
}

#[test]
fn delegation_admission_survives_restart_before_the_child_turn_starts() {
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let child = Agent::pending(&broker.session.id, "reviewer", "Review", 1);
    broker.store.save_agent_run(&child).unwrap();
    let mut parent = completed_interaction("parent", &broker.session.id, Vec::new());
    parent.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Running,
        parent.completed_at_ms(),
        parent.finalization_outcome(),
    );
    parent.phase = crate::exchange::ExchangePhase::fixture(
        parent.state(),
        None,
        parent.finalization_outcome(),
    );
    parent
        .append_delegation(&child.id, "spawn", "Review", 1)
        .unwrap();
    broker.store.save_exchange(&parent).unwrap();
    let queued = broker.store.list_agent_exchange(&child.id).unwrap();
    assert_eq!(queued.len(), 1);
    assert_eq!(queued[0].state(), ExchangeState::Queued);
    assert_eq!(queued[0].ordinal, 1);
    parent
        .append_delegation("missing-agent", "bad", "Invalid", 2)
        .unwrap();
    assert!(broker.store.save_exchange(&parent).is_err());
    assert_eq!(
        broker.store.list_exchange(&broker.session.id).unwrap()[0]
            .node_list
            .len(),
        1
    );
    let mut reopened = SqliteStore::open(data.path()).unwrap();
    reopened
        .interrupt_detached_execution(&broker.session.id)
        .unwrap();
    let recovered = reopened.list_agent_exchange(&child.id).unwrap();
    assert_eq!(recovered[0].state(), ExchangeState::Interrupted);
    assert!(recovered[0].turn.is_empty());
    reopened
        .require_settled_exchange_tree(&broker.session.id, &[parent.id])
        .unwrap();
}

fn failing_provider_fixture(
    workspace: PathBuf,
) -> Result<crate::provider::test_support::ProviderTestSession> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized failure fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let workspace = workspace.clone();
            let descriptor = descriptor.clone();
            async move {
                let nested = workspace.join("failed/deep/write.txt");
                std::fs::create_dir_all(nested.parent().expect("nested path has parent")).map_err(
                    |_| {
                        crate::provider::ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::InputSubmit,
                                forge_provider_api::ProviderFailureKind::Unknown,
                            ),
                        )
                    },
                )?;
                std::fs::write(&nested, "written before provider failure\n").map_err(|_| {
                    crate::provider::ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::InputSubmit,
                            forge_provider_api::ProviderFailureKind::Unknown,
                        ),
                    )
                })?;
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "failing-fixture".into(),
                    turn_id: "turn".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Failed,
                        completed_at_ms: 2,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 2,
                            result: forge_provider_api::lifecycle::ExecutionResult::Failed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                                failure:
                                    forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                                        kind: forge_provider_api::ProviderFailureKind::Transport,
                                    },
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    );
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

fn invalid_control_output_fixture() -> Result<crate::provider::test_support::ProviderTestSession> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized failure fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let descriptor = descriptor.clone();
            async move {
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "fixture".into(),
                    turn_id: "turn".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                for event in completed_message(
                    &address,
                    "failure-message",
                    crate::provider::MessageRole::Response,
                    "Provider execution failed after producing output.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Failed,
                        completed_at_ms: 2,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 2,
                            result: forge_provider_api::lifecycle::ExecutionResult::Failed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                                failure:
                                    forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                                        kind: forge_provider_api::ProviderFailureKind::Protocol,
                                    },
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    );
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

struct AdvancingClock(std::sync::atomic::AtomicI64);

impl Clock for AdvancingClock {
    fn now_ms(&self) -> i64 {
        self.0.fetch_add(10, std::sync::atomic::Ordering::Relaxed)
    }
}

fn parent_boundary_fixture() -> Result<crate::provider::test_support::ProviderTestSession> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized parent boundary fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let descriptor = descriptor.clone();
            async move {
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "fixture".into(),
                    turn_id: "turn".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 1 },
                })
                .await?;
                for event in completed_message(
                    &address,
                    "intermediate-answer",
                    crate::provider::MessageRole::Response,
                    "An intermediate answer.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::ParentBoundary {
                    scope: scope.clone(),
                    update: crate::provider::ParentBoundaryUpdate::WaitStarted { agent_count: 1 },
                })
                .await?;
                sink.send_wait(ProviderEvent::ParentBoundary {
                    scope: scope.clone(),
                    update: crate::provider::ParentBoundaryUpdate::WaitFinished,
                })
                .await?;
                for event in completed_message(
                    &address,
                    "final-synthesis",
                    crate::provider::MessageRole::Response,
                    "A final synthesis.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: 2,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 2,
                            result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    provider_session_id: Some(
                                        "parent-boundary"
                                            .to_owned()
                                            .try_into()
                                            .expect("fixture ID is valid"),
                                    ),
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    );
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

#[derive(Clone, Copy)]
enum PlanFixtureBehavior {
    Questions { structured: bool },
    Retry { submit_from_turn: usize },
    RepeatEdit,
}

fn plan_document_in_request(request: &forge_provider_api::PromptRequest) -> Option<Value> {
    let mut text = String::new();
    text.push_str(&request.instructions);
    if let forge_provider_api::input::PromptInput::Text(input) = &request.input {
        text.push('\n');
        text.push_str(input);
    }
    for recovered in &request.recovered_inputs {
        if let forge_provider_api::input::PromptInput::Text(input) = &recovered.input {
            text.push('\n');
            text.push_str(input);
        }
    }
    let marker = "Active declaration design:\n```json\n";
    let start = text.find(marker)? + marker.len();
    let end = text[start..].find("\n```")? + start;
    serde_json::from_str(&text[start..end]).ok()
}

async fn plan_fixture_tool(
    request: &forge_provider_api::PromptRequest,
    submit: bool,
) -> Result<(), forge_provider_api::ProviderError> {
    let Some(document) = plan_document_in_request(request) else {
        return Ok(());
    };
    let Some(plan_id) = document.get("plan_id").and_then(Value::as_str) else {
        return Ok(());
    };
    let overview = crate::provider::test_support::call_tool(
        request,
        "plan_overview_read",
        json!({"plan_id":plan_id}),
    )
    .await?;
    let version = overview
        .get("version")
        .and_then(Value::as_u64)
        .or_else(|| document.get("version").and_then(Value::as_u64))
        .unwrap_or(1);
    let current = overview.get("overview").cloned().unwrap_or(Value::Null);
    let required = |name: &str, fallback: &str| {
        current
            .get(name)
            .and_then(Value::as_str)
            .filter(|value| !value.trim().is_empty())
            .unwrap_or(fallback)
            .to_owned()
    };
    let objective = required("objective", "Implement the requested migration.");
    let background = required("background", "Preserve the existing caller contract.");
    let design = required(
        "design",
        "Keep ownership explicit and preserve compatibility.",
    );
    if !request
        .tools
        .iter()
        .any(|tool| tool.name == "plan_overview_edit")
    {
        return Err(forge_provider_api::ProviderError::Failure(
            forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::InputSubmit,
                forge_provider_api::ProviderFailureKind::Authorization,
            ),
        ));
    }
    request
        .provider_tools
        .call(
            &request.scope,
            forge_provider_api::control::ToolCall {
                id: forge_provider_api::control::ToolCallId::new("fixture-call-1")
                    .expect("fixture call identity is nonempty"),
                name: "plan_overview_edit".into(),
                arguments: json!({
                    "plan_id":plan_id,
                    "expected_version":version,
                    "objective":objective,
                    "background":background,
                    "design":design
                }),
            },
        )
        .await?;
    if submit {
        let updated = crate::provider::test_support::call_tool(
            request,
            "plan_overview_read",
            json!({"plan_id":plan_id}),
        )
        .await?;
        let version = updated
            .get("version")
            .and_then(Value::as_u64)
            .unwrap_or(version);
        if !request.tools.iter().any(|tool| tool.name == "plan_submit") {
            return Err(forge_provider_api::ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::InputSubmit,
                    forge_provider_api::ProviderFailureKind::Authorization,
                ),
            ));
        }
        request
            .provider_tools
            .call(
                &request.scope,
                forge_provider_api::control::ToolCall {
                    id: forge_provider_api::control::ToolCallId::new("fixture-call-2")
                        .expect("fixture call identity is nonempty"),
                    name: "plan_submit".into(),
                    arguments: json!({"plan_id":plan_id,"expected_version":version}),
                },
            )
            .await?;
    }
    Ok(())
}

fn plan_fixture_provider(broker: &HarnessBroker, behavior: PlanFixtureBehavior) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Plan workflow fixture".into(),
        capability: Default::default(),
    };
    let turn = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let descriptor = descriptor.clone();
            let turn = turn.clone();
            async move {
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::{ProviderEvent, TurnAddress};

                let index = turn.fetch_add(1, Ordering::SeqCst);
                match behavior {
                    PlanFixtureBehavior::Questions { structured: true } if index == 0 => {
                        crate::provider::test_support::call_tool(&request, "question_ask", json!({
                            "questions":[{"header":"Migration","question":"Which migration strategy should the plan use?","options":[
                                {"label":"Staged","description":"Support both formats temporarily.","recommended":false},
                                {"label":"Immediate","description":"Remove the old format now.","recommended":false}
                            ],"allow_freeform":true}]
                        })).await?;
                    }
                    PlanFixtureBehavior::Retry { submit_from_turn }
                        if index >= submit_from_turn =>
                    {
                        plan_fixture_tool(&request, true).await?;
                    }
                    PlanFixtureBehavior::RepeatEdit => {
                        plan_fixture_tool(&request, false).await?;
                    }
                    PlanFixtureBehavior::Questions { .. }
                        if index > 0 && request.mode == PromptMode::Plan =>
                    {
                        plan_fixture_tool(&request, true).await?;
                    }
                    _ => {}
                }
                let scope = request.scope.clone();
                let address = TurnAddress {
                    scope: scope.clone(),
                    thread_id: "plan-workflow-fixture".into(),
                    turn_id: format!("turn-{index}").into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started {
                        started_at_ms: index as i64,
                    },
                })
                .await?;
                for event in completed_message(
                    &address,
                    format!("plan-response-{index}"),
                    crate::provider::MessageRole::Response,
                    if index == 0 {
                        "Which migration strategy should the plan use?"
                    } else {
                        "The plan is ready for review."
                    },
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: index as i64 + 1,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope,
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: index as i64 + 1,
                            result: ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok::<(), forge_provider_api::ProviderError>(())
            }
        },
    );
    workflow_fixture_bridge(broker, fixture)
}
fn nested_write_provider(broker: &HarnessBroker, workspace: &Path) -> Arc<Provider> {
    let workspace = workspace.to_owned();
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Nested write fixture".into(),
        capability: Default::default(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let descriptor = descriptor.clone();
            let workspace = workspace.clone();
            async move {
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::tool::{ToolInvocation, ToolOutcome, ToolUpdate};
                use forge_provider_api::{ProviderEvent, ToolAddress, TurnAddress};

                let scope = request.scope.clone();
                let turn = TurnAddress {
                    scope: scope.clone(),
                    thread_id: "nested-write-fixture".into(),
                    turn_id: "turn-1".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope.clone(),
                        ExecutionUpdate::Started {
                            address: turn.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: turn.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 0 },
                })
                .await?;
                for event in completed_message(
                    &turn,
                    "nested-commentary",
                    crate::provider::MessageRole::Commentary,
                    "Creating the nested module.",
                ) {
                    sink.send_wait(event).await?;
                }

                let file_changes = forge_provider_api::ProviderChangeSet {
                    file: vec![forge_provider_api::ProviderFileChange {
                        path: "seed.txt".into(),
                        move_path: None,
                        kind: forge_provider_api::ProviderChangeKind::Update,
                        diff: "@@ -1 +1 @@\n-seed\n+provider edit".into(),
                    }],
                };
                let file_call = ToolAddress {
                    turn: turn.clone(),
                    tool_id: "provider-file-change".into(),
                };
                let file_invocation = ToolInvocation {
                    semantics: Default::default(),
                    kind: forge_provider_api::ToolActivityKind::FileChange,
                    name: "fixture".into(),
                    title: "file changes".into(),
                    arguments: Value::Null,
                };
                sink.send_wait(ProviderEvent::Tool {
                    address: file_call.clone(),
                    update: ToolUpdate::Requested {
                        invocation: file_invocation,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Tool {
                    address: file_call.clone(),
                    update: ToolUpdate::Started,
                })
                .await?;
                sink.send_wait(ProviderEvent::Tool {
                    address: file_call.clone(),
                    update: ToolUpdate::Changes {
                        changes: file_changes.clone(),
                    },
                })
                .await?;
                std::fs::write(workspace.join("seed.txt"), "provider edit\n")
                    .map_err(crate::provider::test_support::io_failure)?;
                sink.send_wait(ProviderEvent::Tool {
                    address: file_call,
                    update: ToolUpdate::Finished {
                        outcome: ToolOutcome::Succeeded,
                    },
                })
                .await?;

                let command_call = ToolAddress {
                    turn: turn.clone(),
                    tool_id: "nested-command".into(),
                };
                sink.send_wait(ProviderEvent::Tool {
                    address: command_call.clone(),
                    update: ToolUpdate::Requested {
                        invocation: ToolInvocation {
                            semantics: Default::default(),
                            kind: forge_provider_api::ToolActivityKind::Command,
                            name: "fixture".into(),
                            title: "create nested module".into(),
                            arguments: Value::Null,
                        },
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Tool {
                    address: command_call.clone(),
                    update: ToolUpdate::Started,
                })
                .await?;
                let nested = workspace.join("apps/new/deep/module");
                std::fs::create_dir_all(&nested)
                    .map_err(crate::provider::test_support::io_failure)?;
                std::fs::write(nested.join("lib.rs"), "pub fn nested() {}\n")
                    .map_err(crate::provider::test_support::io_failure)?;
                let ignored = workspace.join("target/generated/deep");
                std::fs::create_dir_all(&ignored)
                    .map_err(crate::provider::test_support::io_failure)?;
                std::fs::write(ignored.join("artifact.txt"), "ignored\n")
                    .map_err(crate::provider::test_support::io_failure)?;
                sink.send_wait(ProviderEvent::Tool {
                    address: command_call.clone(),
                    update: ToolUpdate::Output {
                        update: forge_provider_api::tool::TextUpdate::Append(
                            "created nested module".into(),
                        ),
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Tool {
                    address: command_call,
                    update: ToolUpdate::Finished {
                        outcome: ToolOutcome::Succeeded,
                    },
                })
                .await?;
                for event in completed_message(
                    &turn,
                    "nested-response",
                    crate::provider::MessageRole::Response,
                    "The nested module is ready.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: turn.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: 1,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope,
                        ExecutionUpdate::Finished {
                            address: turn,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 1,
                            result: ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok::<(), forge_provider_api::ProviderError>(())
            }
        },
    );
    workflow_fixture_bridge(broker, fixture)
}
fn task_update_provider(broker: &HarnessBroker) -> Arc<Provider> {
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id().expect("fixture identity is available"),
        label: "Task update fixture".into(),
        capability: Default::default(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let descriptor = descriptor.clone();
            async move {
                use forge_provider_api::lifecycle::{
                    ExecutionObservation, ExecutionResult, ExecutionUpdate,
                };
                use forge_provider_api::{ProviderEvent, TurnAddress};

                let scope = request.scope.clone();
                let address = TurnAddress {
                    scope: scope.clone(),
                    thread_id: "task-update-fixture".into(),
                    turn_id: "ordinary-turn".into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started { started_at_ms: 0 },
                })
                .await?;
                sink.send_wait(ProviderEvent::Checklist {
                    address: address.clone(),
                    update: forge_provider_api::ProviderChecklistUpdate {
                        scope_id: "ordinary-turn".into(),
                        name: None,
                        complete: false,
                        replace_entries: true,
                        entry_list: vec![forge_provider_api::ProviderChecklistEntry {
                            provider_id: None,
                            content: "Inspect ordinary chat".into(),
                            priority: None,
                            status: forge_provider_api::ChecklistStatus::InProgress,
                            provider_ordinal: 0,
                        }],
                    },
                })
                .await?;
                for event in completed_message(
                    &address,
                    "ordinary-response",
                    crate::provider::MessageRole::Response,
                    "Ordinary chat completed.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: 1,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        scope,
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 1,
                            result: ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok::<(), forge_provider_api::ProviderError>(())
            }
        },
    );
    workflow_fixture_bridge(broker, fixture)
}

struct FixedClock(i64);
impl Clock for FixedClock {
    fn now_ms(&self) -> i64 {
        self.0
    }
}

#[tokio::test]
async fn plan_settings_preserve_the_planning_model_and_reject_invalid_executor_choices() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: workspace.path().to_string_lossy().into_owned(),
            client_id: "test-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let rejected = broker
        .configure_session(json!({
            "plan_executor_model": "unknown-model", "plan_executor_effort": "high"
        }))
        .await;
    assert!(rejected.is_err());
    assert!(broker.session.plan_executor.model.is_none());
    broker
        .configure_session(json!({
            "plan_executor_model": "mock-model", "plan_executor_effort": "high",
            "plan_executor_enabled": true, "plan_compact": true
        }))
        .await
        .unwrap();
    assert_eq!(broker.session.model, "mock-model");
    assert_eq!(broker.session.effort, "low");
    broker.session.provider_session_id = Some("mock-session".into());
    let (_, events) = tokio::time::timeout(
        std::time::Duration::from_secs(5),
        broker.compact_session(true),
    )
    .await
    .expect("local compaction must consume its own provider observations")
    .unwrap();
    assert!(
        events
            .iter()
            .any(|event| event.event == "context_compacted")
    );
    assert_eq!(broker.session.effort, "low");
    let preference = broker
        .store
        .load_preference(&broker.session.workspace, "mock")
        .unwrap()
        .unwrap();
    assert_eq!(preference.effort, "low");
    assert!(preference.plan_compact);
    assert!(preference.plan_executor.enabled);
    broker.session.effort = "high".into();
    broker.save_session().unwrap();
    drop(broker);
    let next = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: workspace.path().to_string_lossy().into_owned(),
            client_id: "next-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "medium".into(),
            session_id: None,
            new_session_name: Some("next".into()),
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();
    assert_eq!(next.session.effort, "low");
    assert!(next.session.plan_executor.enabled);
}

#[test]
fn rejects_a_second_live_session_controller() {
    let mut session = HarnessSession {
        purpose: crate::session::SessionPurpose::Conversation,
        id: "session".into(),
        primary_agent_id: HarnessSession::primary_agent_id("session"),
        name: "name".into(),
        workspace: "work".into(),
        provider: "mock".into(),
        provider_session_id: None,
        provider_checkpoint_id: None,
        provider_fork_state: ProviderForkState::Ready,
        model: "model".into(),
        provider_label: "Mock provider".into(),
        resolved_model: Some("model".into()),
        effort: "low".into(),
        plan_executor: Default::default(),
        plan_compact: false,
        approval_timeout: Default::default(),
        tool_timeout: Default::default(),
        context_window: None,
        service_tier: crate::provider::ServiceTier::Standard,
        access: Default::default(),
        shell: Default::default(),
        approval_mode: ApprovalMode::Ask,
        active_task: None,
        created_at_ms: 0,
        updated_at_ms: 0,

        lease_owner: Some("one".into()),
        native_fork: false,
        native_compact: false,
        context_usage: None,
        background: None,
        auto_compaction: Default::default(),
        last_compaction_turn: None,
    };
    assert!(session.acquire_lease("two").is_err());
    assert!(session.acquire_lease("one").is_ok());
    session.lease_owner = None;
    assert!(session.acquire_lease("two").is_ok());
    assert_eq!(FixedClock(5).now_ms(), 5);

    let mut model_setting = std::collections::BTreeMap::new();
    model_setting.insert(
        "other-model".into(),
        ModelSetting {
            reasoning: Some("high".into()),
            context_window: Some("long_context".into()),
        },
    );
    let preference = preference_for_session(
        &session,
        Some(HarnessPreference {
            access: Default::default(),
            shell: Default::default(),
            model: "other-model".into(),
            effort: "high".into(),
            model_setting,
            service_tier: crate::provider::ServiceTier::Standard,
            plan_executor: Default::default(),
            plan_compact: false,
            approval_timeout: Default::default(),
            tool_timeout: Default::default(),
        }),
    );
    assert_eq!(preference.model_setting.len(), 2);
    assert_eq!(
        preference.model_setting["other-model"]
            .context_window
            .as_deref(),
        Some("long_context")
    );
    assert_eq!(
        preference.model_setting["model"].reasoning.as_deref(),
        Some("low")
    );
}

fn git(workspace: &Path, args: &[&str]) {
    let status = std::process::Command::new("git")
        .args(args)
        .current_dir(workspace)
        .status()
        .unwrap();
    assert!(status.success());
}

async fn dispatch_pending_task_input(
    broker: &mut HarnessBroker,
) -> Result<(Value, Vec<SessionEvent>)> {
    let task = broker
        .task_controller
        .current()?
        .context("task is missing")?;
    let (instruction, purpose) = if let Some(entry) = task.entry.as_ref()
        && entry.delivery == crate::task::TaskInputDelivery::Pending
    {
        anyhow::ensure!(
            broker.task_controller.acknowledge_entry(
                &mut broker.store,
                &task.revision,
                &entry.id,
                broker.clock.now_ms()
            )?,
            "task entry was replaced"
        );
        (
            entry.instruction.clone(),
            crate::input::InputPurpose::TaskEntry {
                revision: task.revision.clone(),
                id: entry.id.clone(),
                start: crate::exchange::TaskStart {
                    kind: task.kind(),
                    title: task.title().to_owned(),
                },
            },
        )
    } else {
        let turn = task.turn.as_ref().context("task turn is missing")?;
        let continuation = turn
            .continuation
            .as_ref()
            .context("task continuation is missing")?;
        anyhow::ensure!(
            continuation.delivery == crate::task::TaskInputDelivery::Pending,
            "task continuation was already delivered"
        );
        anyhow::ensure!(
            broker.task_controller.acknowledge_continuation(
                &mut broker.store,
                &task.revision,
                &continuation.id,
                broker.clock.now_ms()
            )?,
            "task continuation was replaced"
        );
        (
            continuation.work.instruction().unwrap().to_owned(),
            crate::input::InputPurpose::TaskContinuation {
                revision: task.revision.clone(),
                id: continuation.id.clone(),
                completed_turn_id: turn.id.clone(),
                reason: continuation.reason.clone(),
            },
        )
    };
    broker.task_controller.admit(task.kind())?;
    broker.input_purpose = purpose;
    let result = broker.submit_prompt(json!({"text": instruction})).await;
    broker.input_purpose = crate::input::InputPurpose::User;
    result
}

async fn run_plan_to_input_boundary(broker: &mut HarnessBroker) -> Result<()> {
    let task = broker
        .task_controller
        .current()?
        .context("pending plan task is missing")?;
    let entry = task
        .entry
        .as_ref()
        .context("pending plan entry is missing")?
        .clone();
    anyhow::ensure!(
        broker.task_controller.acknowledge_entry(
            &mut broker.store,
            &task.revision,
            &entry.id,
            broker.clock.now_ms()
        )?,
        "plan entry was replaced before test dispatch"
    );
    broker.input_purpose = crate::input::InputPurpose::TaskEntry {
        revision: task.revision.clone(),
        id: entry.id,
        start: crate::exchange::TaskStart {
            kind: task.kind(),
            title: task.title().to_owned(),
        },
    };
    let result = broker
        .submit_prompt(json!({"text":entry.instruction}))
        .await;
    broker.input_purpose = crate::input::InputPurpose::User;
    result?;
    loop {
        let task = broker
            .task_controller
            .current()?
            .context("planning owner disappeared")?;
        let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else {
            anyhow::bail!("plan test owner changed")
        };
        if matches!(
            plan.artifact.state,
            PlanState::AwaitingInput | PlanState::AwaitingReview
        ) {
            return Ok(());
        }
        anyhow::ensure!(
            matches!(
                plan.artifact.state,
                PlanState::Generating | PlanState::Revising
            ),
            "planning stopped before input boundary: {:?}",
            plan.artifact.state
        );
        let turn = task
            .turn
            .as_ref()
            .context("planning turn did not record its completion")?;
        let continuation = turn
            .continuation
            .as_ref()
            .context("planning turn did not schedule its continuation")?
            .clone();
        anyhow::ensure!(
            continuation.delivery == crate::task::TaskInputDelivery::Pending,
            "planning continuation was not pending"
        );
        anyhow::ensure!(
            broker
                .store
                .continuation_source_complete(&broker.session.id, &turn.id)?,
            "planning continuation source was not finalized"
        );
        anyhow::ensure!(
            broker.task_controller.acknowledge_continuation(
                &mut broker.store,
                &task.revision,
                &continuation.id,
                broker.clock.now_ms()
            )?,
            "planning continuation authority changed"
        );
        broker.task_controller.admit(crate::task::TaskKind::Plan)?;
        broker.input_purpose = crate::input::InputPurpose::TaskContinuation {
            revision: task.revision.clone(),
            id: continuation.id,
            completed_turn_id: turn.id.clone(),
            reason: continuation.reason,
        };
        let result = broker
            .submit_prompt(json!({"text":continuation.work.instruction().unwrap()}))
            .await;
        broker.input_purpose = crate::input::InputPurpose::User;
        result?;
    }
}

fn mutable_question_fixture() -> Arc<crate::provider::test_support::EventFixtureProvider> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized question fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let turns = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let turns = turns.clone();
            let descriptor = descriptor.clone();
            async move {
                let turn = turns.fetch_add(1, Ordering::SeqCst);
                let scope = request.scope.clone();
                if turn == 0 {
                    crate::provider::test_support::call_tool(&request, "question_ask", json!({
                    "questions":[{"header":"Migration","question":"Which migration should the implementation use?","options":[
                        {"label":"Staged","description":"Preserve compatibility temporarily.","recommended":false},
                        {"label":"Immediate","description":"Replace the old format now.","recommended":false}
                    ],"allow_freeform":true}]
                })).await?;
                }
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "question-fixture".into(),
                    turn_id: turn.to_string().into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started {
                        started_at_ms: turn as i64,
                    },
                })
                .await?;
                for event in completed_message(
                    &address,
                    "question-response",
                    crate::provider::MessageRole::Response,
                    if turn == 0 {
                        "Which migration strategy should the plan use?"
                    } else {
                        "The migration strategy is recorded."
                    },
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: turn as i64 + 1,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: turn as i64 + 1,
                            result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    )
}

fn mutable_question_broker(repository: &Path, data_root: &Path) -> HarnessBroker {
    let mut broker = planning_question_broker(repository, data_root, false);
    let native = mutable_question_fixture();
    broker.provider = Arc::new(crate::provider::Provider::new(
        native.registration(),
        broker.permission_coordinator.clone(),
        broker.trace.clone(),
        broker.data_root.clone(),
    ));
    broker
}

/// Preserve the selected context for inherited question models without changing ordinary input.
#[test]
fn question_model_inherits_selected_context_without_mutating_source() -> Result<()> {
    let repository = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.context_window = Some("long_context".into());
    let source_model = broker.session.model.clone();
    let source_effort = broker.session.effort.clone();
    let source_tier = broker.session.service_tier;
    broker.question_discussion = true;
    for model in [
        None,
        Some(source_model.clone()),
        Some("different-question-model".into()),
    ] {
        let same_model = model.as_ref().is_none_or(|model| model == &source_model);
        broker.question_model = Some(crate::settings::AskModel {
            model,
            effort: Some("high".into()),
            service_tier: Some(crate::provider::ServiceTier::Fast),
        });
        let question = broker.provider_request(
            ProviderInput::from_text("Explain this choice"),
            PromptMode::Chat,
        )?;
        assert_eq!(
            question.context_window.as_deref(),
            same_model.then_some("long_context")
        );
        assert_eq!(
            broker.session.context_window.as_deref(),
            Some("long_context")
        );
        assert_eq!(broker.session.model, source_model);
        assert_eq!(broker.session.effort, source_effort);
        assert_eq!(broker.session.service_tier, source_tier);
    }
    broker.question_model = None;
    broker.question_discussion = false;
    let ordinary = broker.provider_request(
        ProviderInput::from_text("Continue ordinary work"),
        PromptMode::Chat,
    )?;
    assert_eq!(ordinary.context_window.as_deref(), Some("long_context"));
    assert_eq!(ordinary.model, source_model);
    assert_eq!(ordinary.effort, source_effort);
    assert_eq!(ordinary.service_tier, source_tier);
    Ok(())
}

#[test]
fn question_discussion_is_read_only_without_changing_session_access() {
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.access.sandbox = false;
    broker.session.access.write_access = crate::session::WriteScope::Full;
    broker.question_discussion = true;
    let request = broker
        .provider_request(
            ProviderInput::from_text("Explain this choice"),
            PromptMode::Chat,
        )
        .unwrap();
    assert!(!request.access.sandbox);
    assert_eq!(request.access.write_access, crate::session::WriteScope::Off);
    assert!(request.access.writable_directory.is_empty());
    assert!(!broker.session.access.sandbox);
    assert_eq!(
        broker.session.access.write_access,
        crate::session::WriteScope::Full
    );
}

#[tokio::test]
async fn persistent_question_discussion_starts_after_completed_exchange() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.provider = crate::provider::build(
        ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            command: vec!["mock".into()],
        },
        broker.permission_coordinator.clone(),
        broker.trace.clone(),
        broker.data_root.clone(),
    )?;
    broker
        .submit_prompt(json!({"text":"Original request"}))
        .await?;
    let original = broker.store.latest_exchange(&broker.session.id)?.unwrap();
    let ids = broker.questions.ask(
        "ask-after-completion",
        &original.id,
        crate::question::QuestionOrigin::Chat,
        vec![crate::question::QuestionDraft {
            header: "Layout".into(),
            question: "Which layout?".into(),
            requirement: crate::question::QuestionRequirement::Optional,
            options: Vec::new(),
        }],
    )?;
    for message_kind in [
        crate::question::QuestionMessageKind::Clarification,
        crate::question::QuestionMessageKind::Answer,
    ] {
        broker.input_purpose = crate::input::InputPurpose::Question {
            content_revision: None,
            question_id: ids[0].clone(),
            task_id: None,
            message_kind,
            reply: crate::provider::test_support::question_reply(
                &ids[0],
                match message_kind {
                    crate::question::QuestionMessageKind::Clarification => {
                        forge_provider_api::QuestionReplyResponse::Clarification {
                            options: Vec::new(),
                            text: "Clarify".into(),
                            reply_to: None,
                        }
                    }
                    _ => forge_provider_api::QuestionReplyResponse::Answer {
                        answer: "Layout".into(),
                        required_questions_remain: false,
                    },
                },
            ),
        };
        broker
            .submit_prompt(json!({"text":"Internal question delivery context"}))
            .await?;
        let discussion = broker.store.latest_exchange(&broker.session.id)?.unwrap();
        assert_ne!(discussion.id, original.id);
        assert!(discussion.prompt.ends_with(": Layout"));
        assert!(
            !discussion
                .prompt
                .contains("Internal question delivery context")
        );
    }
    assert!(broker.questions.snapshot()?.questions[0].pending());
    assert!(!broker.question_discussion);
    Ok(())
}

#[tokio::test]
async fn session_mutations_enforce_target_ownership() -> Result<()> {
    use std::fs;

    let source_workspace = tempfile::tempdir()?;
    let target_workspace = tempfile::tempdir()?;
    let data = tempfile::tempdir()?;
    let mut source = mutable_question_broker(source_workspace.path(), data.path());
    let target = mutable_question_broker(target_workspace.path(), data.path());
    let target_id = target.session.id.clone();
    let target_name = target.session.name.clone();
    let plan_directory = data.path().join("plans").join(&target_id);
    fs::create_dir_all(&plan_directory)?;
    let plan = plan_directory.join("preserved.md");
    fs::write(&plan, "preserve active plan")?;
    for (id, method, params) in [
        (
            1,
            "session.rename",
            json!({"session_id":target_id,"name":"invalid"}),
        ),
        (2, "session.delete", json!({"session_id":target_id})),
    ] {
        let response = source
            .dispatch(Request {
                id,
                method: method.into(),
                params,
            })
            .await;
        assert!(
            response.response.error().is_some(),
            "{method} accepted a foreign owner"
        );
        assert_eq!(
            source.store.load_session(&target_id)?.unwrap().name,
            target_name
        );
        assert_eq!(fs::read_to_string(&plan)?, "preserve active plan");
    }
    let source_id = source.session.id.clone();
    let rename = source
        .dispatch(Request {
            id: 3,
            method: "session.rename".into(),
            params: json!({"session_id":source_id,"name":"owned rename"}),
        })
        .await;
    assert!(rename.response.error().is_none());
    assert_eq!(source.session.name, "owned rename");
    drop(target);
    let deletion = source
        .dispatch(Request {
            id: 4,
            method: "session.delete".into(),
            params: json!({"session_id":target_id}),
        })
        .await;
    assert!(deletion.response.error().is_none());
    assert!(source.store.load_session(&target_id)?.is_none());
    assert!(!plan_directory.exists());
    Ok(())
}

#[tokio::test]
async fn unknown_non_spawn_lifecycle_does_not_create_an_agent_run() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut event = Vec::new();
    let lifecycle = AgentLifecycleEvent {
        agent_id: forge_provider_api::agent::AgentId::new("observed-child").unwrap(),
        parent_agent_id: None,
        conversation_id: None,
        kind: forge_provider_api::agent::AgentLifecycleKind::Finished,
        definition: None,
        nickname: None,
        task: None,
        state: AgentExecutionState::Completed,
    };

    broker
        .apply_agent_lifecycle(&lifecycle, None, &mut event)
        .unwrap();

    assert!(broker.agent_registry.list().is_empty());
    assert!(event.is_empty());
}

#[tokio::test]
async fn observed_agent_start_cannot_create_durable_managed_identity() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let lifecycle = AgentLifecycleEvent {
        agent_id: forge_provider_api::agent::AgentId::new("observed-child").unwrap(),
        parent_agent_id: None,
        conversation_id: Some("observed-history".try_into().unwrap()),
        kind: forge_provider_api::agent::AgentLifecycleKind::Started,
        definition: Some("Reviewer".into()),
        nickname: Some("Review worker".into()),
        task: Some("Inspect the selected change".into()),
        state: AgentExecutionState::Running,
    };
    let mut events = Vec::new();
    let spawned = broker
        .apply_agent_lifecycle(&lifecycle, None, &mut events)
        .unwrap();
    let saved = broker.store.list_agent_run(&broker.session.id).unwrap();
    let child = saved
        .iter()
        .filter(|agent| !agent.is_primary())
        .collect::<Vec<_>>();
    assert!(spawned.is_none());
    assert!(child.is_empty());
    assert!(events.is_empty());
}

#[tokio::test]
async fn observed_agent_activity_cannot_bind_a_managed_identity() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let pending = Agent::pending(&broker.session.id, "explorer", "inspect Bevy", 100);
    broker.store.save_agent_run(&pending).unwrap();
    broker.agent_registry.insert(pending.clone());
    broker
        .agent_registry
        .execution_mut(&pending.id)
        .parent_exchange_id = Some("parent-interaction".into());
    let mut event = Vec::new();
    let lifecycle = AgentLifecycleEvent {
        agent_id: forge_provider_api::agent::AgentId::new("observed-child").unwrap(),
        parent_agent_id: Some(forge_provider_api::agent::AgentId::new("observed-parent").unwrap()),
        conversation_id: Some("observed-history".try_into().unwrap()),
        kind: forge_provider_api::agent::AgentLifecycleKind::Updated,
        definition: None,
        nickname: None,
        task: None,
        state: AgentExecutionState::Running,
    };

    broker
        .apply_agent_lifecycle(&lifecycle, None, &mut event)
        .unwrap();

    let run_list = broker.agent_registry.list();
    assert_eq!(run_list.len(), 1);
    assert_eq!(run_list[0].id, pending.id);
    assert_eq!(run_list[0].definition, "explorer");
    assert_eq!(
        broker
            .agent_registry
            .execution(&run_list[0].id)
            .and_then(|execution| execution.parent_exchange_id.as_deref()),
        Some("parent-interaction")
    );
    assert!(run_list[0].provider_thread_id.is_none());
}

/// Persist late background output and completion only on the admitted historical owner.
#[tokio::test]
async fn background_tool_completes_on_settled_owner_without_touching_current_turn() -> Result<()> {
    use crate::provider::{TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate};

    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut settled = completed_interaction("background-owner", &broker.session.id, Vec::new());
    let provider = crate::turn::test_support::address(&settled, "background-provider-turn");
    let mut turn =
        crate::turn::test_support::TestTurn::new("background-turn".into(), provider.clone(), 1);
    for update in [
        ToolUpdate::Requested {
            invocation: ToolInvocation {
                semantics: Default::default(),
                kind: ToolActivityKind::Command,
                name: "exec".into(),
                title: "Background command".into(),
                arguments: Value::Null,
            },
        },
        ToolUpdate::Started,
        ToolUpdate::Output {
            update: TextUpdate::Append("START\n".into()),
        },
        ToolUpdate::Backgrounded,
    ] {
        turn.record_tool("background", update, 2)?;
    }
    turn.finish(crate::turn::TurnOutcome::Completed, 3)?;
    let recorded = turn.into_exchange();
    settled.turn = recorded.turn;
    settled.node_list = recorded.node_list;
    settled.entry = recorded.entry;
    settled.rebuild_tool_counter();
    broker.store.save_exchange(&settled)?;

    let mut current = completed_interaction("foreground-owner", &broker.session.id, Vec::new());
    current.ordinal = 2;
    broker.store.save_exchange(&current)?;
    let current_before = serde_json::to_value(&current)?;
    let address = crate::provider::ToolAddress {
        turn: provider,
        tool_id: "background".into(),
    };
    let mut event = Vec::new();
    for update in [
        ToolUpdate::Output {
            update: TextUpdate::Append("END\n".into()),
        },
        ToolUpdate::Finished {
            outcome: ToolOutcome::Succeeded,
        },
    ] {
        assert!(
            broker
                .test_apply_settled_parent_event(
                    &current,
                    &ProviderEvent::Tool {
                        address: address.clone(),
                        update
                    },
                    &mut event,
                )
                .await?
        );
    }
    let saved = broker.store.load_exchange(&settled.id)?.unwrap();
    let tool = saved.tools().next().unwrap().1;
    assert_eq!(tool.state(), crate::turn::ToolState::Completed);
    assert_eq!(tool.output, "START\nEND\n");
    assert_eq!(saved.state(), ExchangeState::Complete);
    assert_eq!(
        serde_json::to_value(broker.store.load_exchange(&current.id)?.unwrap())?,
        current_before
    );
    assert!(!event.is_empty());
    let mut unknown = address.clone();
    unknown.tool_id = "unknown".into();
    assert!(!saved.accepts_settled_tool_update(
        &unknown,
        &ToolUpdate::Finished {
            outcome: ToolOutcome::Succeeded,
        }
    ));
    assert!(
        !saved.accepts_settled_tool_update(
            &address,
            &ToolUpdate::Finished {
                outcome: ToolOutcome::Failed {
                    diagnostic: crate::provider::ToolDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new("late contradiction")
                            .expect("host diagnostic is nonempty"),
                    }
                },
            }
        )
    );
    Ok(())
}

#[tokio::test]
async fn late_tool_title_updates_the_settled_owner_without_touching_current_turn() -> Result<()> {
    use crate::provider::{ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate};

    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut settled = completed_interaction("settled-title", &broker.session.id, Vec::new());
    let provider = crate::turn::test_support::address(&settled, "settled-provider-turn");
    let mut turn =
        crate::turn::test_support::TestTurn::new("settled-turn".into(), provider.clone(), 1);
    for (update, now_ms) in [
        (
            ToolUpdate::Requested {
                invocation: ToolInvocation {
                    semantics: Default::default(),
                    kind: ToolActivityKind::ToolCall,
                    name: "webSearch".into(),
                    title: "Web Search".into(),
                    arguments: Value::Null,
                },
            },
            1,
        ),
        (ToolUpdate::Started, 2),
        (
            ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
            3,
        ),
    ] {
        turn.record_tool("search", update, now_ms)?;
    }
    turn.finish(crate::turn::TurnOutcome::Completed, 3)?;
    let recorded = turn.into_exchange();
    settled.turn = recorded.turn;
    settled.node_list = recorded.node_list;
    settled.entry = recorded.entry;
    settled.rebuild_tool_counter();
    broker.store.save_exchange(&settled)?;

    let mut current = completed_interaction("current-title", &broker.session.id, Vec::new());
    current.ordinal = 2;
    current.phase = crate::exchange::ExchangePhase::Running;
    let current_provider = crate::turn::test_support::address(&current, "current-provider-turn");
    current.turn.push(crate::turn::Turn::new(
        "current-turn".into(),
        current_provider,
        4,
    ));
    broker.store.save_exchange(&current)?;
    let current_before = serde_json::to_value(&current)?;
    let mut event = Vec::new();
    assert!(
        broker
            .test_apply_settled_parent_event(
                &current,
                &ProviderEvent::Tool {
                    address: crate::provider::ToolAddress {
                        turn: provider,
                        tool_id: "search".into(),
                    },
                    update: ToolUpdate::TitleChanged {
                        title: "Web Search · documentation".into(),
                    },
                },
                &mut event,
            )
            .await?
    );
    let saved = broker.store.load_exchange(&settled.id)?.unwrap();
    let tool = saved.tools().next().unwrap().1;
    assert_eq!(tool.title, "Web Search · documentation");
    assert_eq!(tool.state(), crate::turn::ToolState::Completed);
    assert_eq!(tool.started_at_ms, Some(2));
    assert_eq!(tool.completed_at_ms, Some(3));
    assert_eq!(saved.state(), ExchangeState::Complete);
    assert_eq!(serde_json::to_value(&current)?, current_before);
    assert_eq!(
        serde_json::to_value(broker.store.load_exchange(&current.id)?.unwrap())?,
        current_before
    );
    assert!(!event.is_empty(), "late metadata did not reach the client");
    Ok(())
}

#[tokio::test]
async fn publishes_and_persists_resolved_model_before_provider_turn_starts() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.session.model = "default".into();
    broker.session.resolved_model = None;
    let mut exchange = completed_interaction("runtime-identity", &broker.session.id, Vec::new());
    exchange.phase = crate::exchange::ExchangePhase::Running;
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        exchange.state(),
        None,
        exchange.finalization_outcome(),
    );
    let mut runtime = ExchangeRuntime {
        task_permit: None,
        exchange_id: exchange.id.clone(),
        checkpoint: None,
        tool_gate: None,

        checklist: ChecklistTracker::default(),
        retraction_eligible: true,
        active_wait: None,
    };
    let fixture_scope = crate::provider::InvocationScope::new(
        crate::provider::ExecutionOwner::root(&broker.session.id),
        exchange.id.clone(),
    );
    let mut events = Vec::new();
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut exchange,
            crate::provider::ProviderEvent::Runtime {
                scope: fixture_scope,
                update: crate::provider::RuntimeUpdate::Resolved {
                    runtime: crate::provider::ProviderRuntime {
                        provider: "Fixture Provider".into(),
                        model: Some("resolved-model".into()),
                    },
                },
            },
            &mut events,
        )
        .await
        .unwrap();

    assert_eq!(
        broker.session.resolved_model.as_deref(),
        Some("resolved-model")
    );
    let persisted = broker
        .store
        .load_session(&broker.session.id)
        .unwrap()
        .unwrap();
    assert_eq!(persisted.resolved_model.as_deref(), Some("resolved-model"));
    assert_eq!(events.len(), 1);
    assert_eq!(events[0].payload["kind"], "runtime_resolved");
    assert_eq!(events[0].payload["data"]["model"], "resolved-model");
    assert_eq!(events[0].payload["data"]["session_id"], broker.session.id);
    assert!(exchange.turn.is_empty());
    assert_eq!(runtime.exchange_id, exchange.id);
    assert!(runtime.retraction_eligible);
}

#[tokio::test]
async fn native_provider_content_never_creates_presentation_segments() {
    use crate::provider::{
        TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate, TurnAddress,
        TurnUpdate,
    };
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut exchange = completed_interaction("native-runtime", &broker.session.id, Vec::new());
    exchange.phase = crate::exchange::ExchangePhase::Running;
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        exchange.state(),
        None,
        exchange.finalization_outcome(),
    );
    let mut runtime = ExchangeRuntime {
        task_permit: None,
        checkpoint: None,
        tool_gate: None,
        exchange_id: exchange.id.clone(),

        checklist: ChecklistTracker::default(),
        retraction_eligible: true,
        active_wait: None,
    };
    let fixture_scope = crate::provider::InvocationScope::new(
        crate::provider::ExecutionOwner::root(&broker.session.id),
        exchange.id.clone(),
    );
    let address = TurnAddress {
        scope: fixture_scope.clone(),
        thread_id: "parent".into(),
        turn_id: "native".into(),
    };
    let mut events = Vec::new();
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut exchange,
            ProviderEvent::Turn {
                address: address.clone(),
                update: TurnUpdate::Started { started_at_ms: 0 },
            },
            &mut events,
        )
        .await
        .unwrap();
    for update in completed_message(
        &address,
        "inspection",
        crate::provider::MessageRole::Commentary,
        "Inspecting",
    ) {
        broker
            .test_apply_provider_event(&mut runtime, &mut exchange, update, &mut events)
            .await
            .unwrap();
    }
    for update in crate::provider::test_support::tool(
        &address,
        "tool".into(),
        ToolInvocation {
            semantics: Default::default(),
            kind: ToolActivityKind::Command,
            name: "inspect".into(),
            title: "inspect".into(),
            arguments: Value::Null,
        },
        [
            ToolUpdate::Output {
                update: TextUpdate::Replace("result".into()),
            },
            ToolUpdate::Finished {
                outcome: ToolOutcome::Succeeded,
            },
        ],
    ) {
        broker
            .test_apply_provider_event(&mut runtime, &mut exchange, update, &mut events)
            .await
            .unwrap();
    }
    assert_eq!(exchange.turn[0].messages()[0].text(), "Inspecting");
    assert_eq!(exchange.tools_for_turn(exchange.turn[0].id()).count(), 1);
    assert!(
        exchange
            .node_list
            .iter()
            .all(|node| matches!(node, ExchangeNode::TurnContent { .. }))
    );
    assert_eq!(runtime.exchange_id, exchange.id);
    let persisted = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(persisted[0].node_list.len(), 2);
    assert!(!runtime.retraction_eligible);
    let compaction = crate::provider::Compaction {
        id: "native-compaction".into(),
        thread_id: "parent".into(),
        state: crate::provider::CompactionState::Running,
        manual: false,
        started_at_ms: Some(200),
        completed_at_ms: None,
        result: Default::default(),
    };
    let notification = ProviderEvent::Compaction {
        scope: fixture_scope.clone().into(),
        update: Box::new(compaction.clone()),
    };
    broker
        .test_apply_provider_event(&mut runtime, &mut exchange, notification, &mut events)
        .await
        .unwrap();
    let position = exchange.node_list.len() - 1;
    let original_id = exchange.node_list[position].id().to_owned();
    let mut unrelated = compaction.clone();
    unrelated.thread_id = "unrelated".into();
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut exchange,
            ProviderEvent::Compaction {
                scope: fixture_scope.clone().into(),
                update: Box::new(unrelated),
            },
            &mut events,
        )
        .await
        .unwrap();
    assert_eq!(exchange.node_list.len(), 3);
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut exchange,
            ProviderEvent::Turn {
                address,
                update: TurnUpdate::Finished {
                    outcome: crate::turn::TurnOutcome::Interrupted,
                    completed_at_ms: 300,
                },
            },
            &mut events,
        )
        .await
        .unwrap();
    let ExchangeNode::Compaction {
        compaction: pending,
        ..
    } = &exchange.node_list[position]
    else {
        panic!("compaction row missing")
    };
    assert_eq!(pending.state, crate::provider::CompactionState::Unconfirmed);
    let mut completed = compaction;
    completed.state = crate::provider::CompactionState::Completed;
    completed.result.summary_content = Some("Retain the original task.".into());
    completed.completed_at_ms = Some(400);
    let notification = ProviderEvent::Compaction {
        scope: fixture_scope.into(),
        update: Box::new(completed),
    };
    broker
        .test_apply_provider_event(&mut runtime, &mut exchange, notification, &mut events)
        .await
        .unwrap();
    let ExchangeNode::Compaction {
        compaction: completed,
        ..
    } = &exchange.node_list[position]
    else {
        panic!("compaction position changed")
    };
    assert_eq!(completed.state, crate::provider::CompactionState::Completed);
    assert_eq!(completed.started_at_ms, Some(200));
    assert_eq!(completed.completed_at_ms, Some(400));
    assert_eq!(exchange.node_list[position].id(), original_id);
    assert_eq!(exchange.turn.len(), 1);
    assert_eq!(exchange.turn[0].messages().len(), 1);
    assert_eq!(exchange.tools_for_turn(exchange.turn[0].id()).count(), 1);
}

/// Reject autonomous execution after settlement without reopening committed history.
#[tokio::test]
async fn native_execution_after_settlement_fences_reuse_without_reopening_history() -> Result<()> {
    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.checkpoint = None;
    let observed_scope = Arc::new(std::sync::Mutex::new(None));
    let supplied_scope = observed_scope.clone();
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().expect("fixture identity"),
    )?;
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        move |request, sink, _| {
            *supplied_scope.lock().unwrap() = Some(request.scope.clone());
            async move {
                let address =
                    crate::provider::test_support::address(&request.scope, "native", "first");
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope.clone(),
                        ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                for event in completed_message(
                    &address,
                    "answer",
                    crate::provider::MessageRole::Response,
                    "Settled answer",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(
                        request.scope,
                        ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: 40,
                            result: ExecutionResult::Completed {
                                completion:
                                    forge_provider_api::completion::ProviderCompletion::default()
                                        .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    );
    let provider = crate::provider::test_support::ProviderTestSession::from_native(native.clone())?;
    broker.set_provider_for_test(provider.provider_handle());
    broker
        .run_interaction(
            "Finish one invocation".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Finish one invocation".into())),
        )
        .await?;
    let scope = observed_scope
        .lock()
        .unwrap()
        .clone()
        .expect("ordinary input supplied its scope");
    let engine = broker
        .execution_registry()
        .existing_owner(&scope.owner)?
        .expect("the settled owner remains observable");
    let saved = broker
        .store
        .latest_exchange(&broker.session.id)?
        .expect("completed exchange was saved");
    let turn_count = saved.turn.len();
    native
        .invocation_sink(&scope)
        .await?
        .send_wait(ProviderEvent::Execution {
            observation: ExecutionObservation::new(
                scope.clone(),
                ExecutionUpdate::Started {
                    address: crate::provider::test_support::address(
                        &scope,
                        "native",
                        "unrequested",
                    ),
                },
            )?,
        })
        .await?;
    tokio::time::timeout(std::time::Duration::from_secs(3), async {
        while engine.is_healthy().await {
            tokio::task::yield_now().await;
        }
    })
    .await?;
    assert!(matches!(
        broker.observer.session_events().end(),
        Some(crate::provider::session_execution::ObservationEnd::Lost(
            forge_provider_api::ObservationFailure::OwnershipConflict
        ))
    ));
    assert!(engine.invocation_active_snapshot(&scope).is_err());
    let saved = broker
        .store
        .load_exchange(&saved.id)?
        .expect("history survives rejected native work");
    assert_eq!(saved.turn.len(), turn_count);
    assert_eq!(saved.state(), ExchangeState::Complete);
    Ok(())
}

#[tokio::test]
async fn failed_checkpoint_finalization_persists_failure_and_blocks_provider_execution() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut exchange = completed_interaction("unfinished", &broker.session.id, Vec::new());
    exchange.phase = crate::exchange::ExchangePhase::Running;
    exchange.phase = crate::exchange::ExchangePhase::fixture(
        exchange.state(),
        None,
        exchange.finalization_outcome(),
    );
    exchange.checkpoint_before = Some("missing-checkpoint".into());
    broker.store.save_exchange(&exchange).unwrap();
    assert!(
        broker
            .capture_final_checkpoint(&mut exchange, crate::exchange::ExchangeOutcome::Failed)
            .await
            .is_err()
    );
    let persisted = broker
        .store
        .list_exchange(&broker.session.id)
        .unwrap()
        .pop()
        .unwrap();
    assert_eq!(persisted.state(), ExchangeState::Finalizing);
    assert!(
        broker
            .snapshot()
            .unwrap()
            .timeline
            .iter()
            .any(|entry| matches!(
                entry,
                TimelineEntry::Status {
                    status: crate::session::state_machine::SessionPhase::Finalizing { .. },
                    ..
                }
            ))
    );
    assert!(
        persisted
            .finalization_error
            .as_deref()
            .unwrap()
            .contains("before checkpoint is missing")
    );
    let rejected = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"new request"}),
        })
        .await;
    assert!(rejected.response.error().is_some());
    let visible = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(visible.len(), 2);
    let rejected_exchange = visible
        .iter()
        .find(|candidate| candidate.id != exchange.id)
        .unwrap();
    assert_eq!(rejected_exchange.state(), ExchangeState::Failed);
    assert!(rejected_exchange.turn.is_empty());
    let before = GitCheckpoint::new(repository.path())
        .capture(
            &broker.store.objects,
            &broker.repositories,
            &broker.session.id,
            100,
        )
        .await
        .unwrap();
    broker.store.save_checkpoint(&before).unwrap();
    exchange.checkpoint_before = Some(before.id);
    broker.store.save_exchange(&exchange).unwrap();
    let nodes_before = serde_json::to_value(&exchange.node_list).unwrap();
    let retried = broker
        .dispatch(Request {
            id: 2,
            method: "session.turn.cancel".into(),
            params: json!({}),
        })
        .await;
    assert!(
        retried.response.error().is_none(),
        "{:?}",
        retried.response.error()
    );
    let settled = broker.store.load_exchange(&exchange.id).unwrap().unwrap();
    assert_eq!(settled.state(), ExchangeState::Failed);
    assert!(settled.finalization_error.is_none());
    assert_eq!(
        serde_json::to_value(&settled.node_list).unwrap(),
        nodes_before
    );
    assert_eq!(settled.turn.len(), exchange.turn.len());
    assert_eq!(settled.duration_ms(), exchange.duration_ms());
    let settled_json = serde_json::to_value(&settled).unwrap();
    let repeated = broker
        .dispatch(Request {
            id: 3,
            method: "session.turn.cancel".into(),
            params: json!({}),
        })
        .await;
    assert!(repeated.response.error().is_none());
    assert_eq!(
        serde_json::to_value(broker.store.load_exchange(&exchange.id).unwrap().unwrap()).unwrap(),
        settled_json
    );
}

#[tokio::test]
async fn shares_prompt_history_across_repository_sessions() {
    let first_repository = repository();
    let second_repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut first = planning_question_broker(first_repository.path(), data.path(), false);
    let recorded = first
        .dispatch(Request {
            id: 1,
            method: "session.prompt.history.record".into(),
            params: json!({ "text": "inspect the architecture" }),
        })
        .await;
    assert!(recorded.response.error().is_none());
    drop(first);

    let second = planning_question_broker(second_repository.path(), data.path(), false);
    assert_eq!(
        second.snapshot().unwrap().prompt_history,
        vec![crate::attachment::PromptDraft {
            text: "inspect the architecture".into(),
            attachments: Vec::new()
        }]
    );
}

#[tokio::test]
async fn check_setup_followups_preserve_discovery_and_reviewed_state_after_restart() {
    for state in [
        crate::checks::CheckSetupState::Discovery,
        crate::checks::CheckSetupState::Reviewed,
    ] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (mut setup, _, _) = broker
            .test_stage_interaction_for_turn("Configure project checks", true, 100)
            .await
            .unwrap();
        setup.check_setup = Some(state);
        setup.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Interrupted,
            setup.completed_at_ms(),
            setup.finalization_outcome(),
        );
        broker.store.save_exchange(&setup).unwrap();
        let session_id = broker.session.id.clone();
        drop(broker);
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        assert_eq!(broker.session.id, session_id);

        broker
            .run_interaction(
                "Continue the interrupted setup".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Continue setup".into())),
            )
            .await
            .unwrap();

        let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
        let continued = exchanges.last().unwrap();
        assert_ne!(continued.id, setup.id);
        assert_eq!(continued.check_setup, Some(state));
        assert_eq!(
            broker
                .continued_check_setup(&ExchangeAdmission::chat("Another follow-up".into()))
                .unwrap(),
            Some(state)
        );
    }
}

#[tokio::test]
async fn check_tool_scope_observes_live_settings_and_editing_permission() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    save_check_exchange(&mut broker, "editing-test", false)?;
    let fixture_scope = crate::provider::InvocationScope::new(
        crate::provider::ExecutionOwner::root(&broker.session.id),
        "editing-test",
    );
    let (sink, _events) = crate::provider::events::channel(fixture_scope.clone());
    let (scope, _lease) = crate::checks::CheckToolScope::open(
        Arc::clone(&broker.check_runner),
        crate::checks::CheckAuthority {
            task_authority: None,
            data_root: broker.data_root.clone(),
            provider: broker.session.provider.clone(),
            task_id: None,
            shell: broker.session.shell,
            mode: PromptMode::Chat,
            permission: ApprovalMode::Allow,
            permissions: Arc::clone(&broker.permission_coordinator),
            scope: fixture_scope.clone(),
            authorization_sink: sink,
            editing: Arc::clone(&broker.observer.check_editing),
            can_run: true,
            agent: Arc::clone(&broker.agent_coordinator),
            cancelled: tokio::sync::watch::channel(false).1,
            sink: None,
        },
    );
    let catalog: Value =
        serde_json::from_str(&scope.invoke("check_definition_list", json!({})).await?)?;
    assert_eq!(catalog["automatic_checks"], false);
    assert_eq!(catalog["check_timing"], "after_queue");
    let configured = catalog["configured"].clone();
    for (automatic_checks, check_timing) in [
        (false, crate::checks::CheckTiming::BetweenWork),
        (true, crate::checks::CheckTiming::AfterQueue),
    ] {
        broker
            .configure_session(json!({
                "automatic_checks": automatic_checks, "check_timing": check_timing
            }))
            .await?;
        let catalog: Value =
            serde_json::from_str(&scope.invoke("check_definition_list", json!({})).await?)?;
        assert_eq!(catalog["automatic_checks"], json!(automatic_checks));
        assert_eq!(catalog["check_timing"], json!(check_timing));
        assert_eq!(catalog["configured"], configured);
        assert!(catalog.get("history").is_none());
    }
    let request = || -> Result<Value> {
        Ok(json!({
            "expected_revision": broker.check_runner.catalog(None)?["revision"],
            "config": {}
        }))
    };
    assert!(
        scope
            .invoke("check_definition_edit", request()?)
            .await
            .unwrap_err()
            .to_string()
            .contains("disabled")
    );
    broker.observer.set_check_editing(true)?;
    scope.invoke("check_definition_edit", request()?).await?;
    broker.observer.set_check_editing(false)?;
    assert!(
        scope
            .invoke("check_definition_edit", request()?)
            .await
            .unwrap_err()
            .to_string()
            .contains("disabled")
    );
    assert_eq!(broker.store.list_exchange(&broker.session.id)?.len(), 1);
    Ok(())
}

#[tokio::test]
async fn configured_check_tools_run_without_command_approval() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    std::fs::write(
        repository.path().join(".forge.json"),
        serde_json::to_vec(
            &json!({"checks":{"configured":{"trigger":{"auto":false},"action":{"command":"echo configured-check","label":"Configured check","category":"test"}}}}),
        )?,
    )?;
    let (owner, mut requests) = crate::checks::owner::CheckOwnerSender::channel();
    broker.check_runner.install_owner(owner)?;
    let mut runs = crate::checks::owner::CheckOwnerRuns::new(broker.check_runner.clone());
    for permission in [ApprovalMode::Ask, ApprovalMode::Ask] {
        save_check_exchange(
            &mut broker,
            &format!("configured-check-{permission:?}"),
            false,
        )?;
        let fixture_scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root(&broker.session.id),
            format!("configured-check-{permission:?}"),
        );
        let (sink, mut events) = crate::provider::events::channel(fixture_scope.clone());
        let (scope, lease) = crate::checks::CheckToolScope::open(
            Arc::clone(&broker.check_runner),
            crate::checks::CheckAuthority {
                task_authority: None,
                data_root: broker.data_root.clone(),
                provider: broker.session.provider.clone(),
                task_id: None,
                shell: crate::session::CheckShell::System,
                mode: PromptMode::Chat,
                permission,
                permissions: Arc::clone(&broker.permission_coordinator),
                scope: fixture_scope.clone(),
                authorization_sink: sink,
                editing: Arc::clone(&broker.observer.check_editing),
                can_run: true,
                agent: Arc::clone(&broker.agent_coordinator),
                cancelled: tokio::sync::watch::channel(false).1,
                sink: None,
            },
        );
        let call = scope.invoke_with_identity(
            "check_run",
            json!({"check":"configured"}),
            forge_provider_api::control::ToolCallId::new(uuid::Uuid::new_v4().to_string())?,
        );
        let admit = async {
            let Some(crate::checks::owner::CheckOwnerEvent::StartTool(request)) =
                requests.recv().await
            else {
                anyhow::bail!("check did not reach its owner");
            };
            runs.start(*request)?;
            Ok::<_, anyhow::Error>(())
        };
        let (response, admission) =
            tokio::time::timeout(std::time::Duration::from_secs(5), async {
                tokio::join!(call, admit)
            })
            .await?;
        admission?;
        let response: Value = serde_json::from_str(&response?)?;
        assert_eq!(response.as_object().unwrap().len(), 1);
        let run = broker
            .check_runner
            .status(response["run_id"].as_str().unwrap(), 5000, true)
            .await?;
        assert_eq!(run.state, crate::checks::CheckState::Passed);
        assert_eq!(run.commands.len(), 1);
        assert_eq!(run.commands[0].command, "echo configured-check");
        assert_eq!(run.commands[0].exit_code, Some(0));
        let completion = runs.next().await.context("check worker did not settle")??;
        assert!(completion.outcome.is_ok());
        runs.settle(&completion);
        let summary: Value = serde_json::from_str(
            &scope
                .invoke(
                    "check_status",
                    json!({"run_id":response["run_id"],"wait_ms":10}),
                )
                .await?,
        )?;
        assert_eq!(summary["state"], "passed");
        assert_eq!(summary["exit_code"], 0);
        assert_eq!(summary["latest_output"], "configured-check");
        assert!(summary.get("context").is_none());
        assert!(summary.get("commands").is_none());
        let details: Value = serde_json::from_str(
            &scope
                .invoke("check_detail", json!({"run_id":response["run_id"]}))
                .await?,
        )?;
        assert!(
            details["text"]
                .as_str()
                .unwrap()
                .contains("configured-check")
        );
        assert!(details["command"]["invocation"]["args"].is_array());
        let running: Value = serde_json::from_str(&scope.invoke("check_list", json!({})).await?)?;
        assert_eq!(running, json!({"runs":[]}));
        assert!(events.try_recv().is_err(), "check requested approval");
        assert!(
            scope
                .invoke(
                    "check_run",
                    json!({"check":"configured","command":"echo unconfigured"}),
                )
                .await
                .is_err(),
            "check tools accepted a replacement command"
        );
        assert!(
            scope
                .invoke(
                    "check_definition_edit",
                    json!({"expected_revision":"","config":{}}),
                )
                .await
                .unwrap_err()
                .to_string()
                .contains("disabled")
        );
        drop(lease);
        assert!(
            scope
                .invoke("check_run", json!({"check":"configured"}))
                .await
                .is_err(),
            "inactive check scope admitted execution"
        );
    }
    Ok(())
}

#[tokio::test]
async fn check_setup_followups_stop_after_configuration_publication() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (mut setup, _, _) = broker
        .test_stage_interaction_for_turn("Configure project checks", true, 100)
        .await
        .unwrap();
    setup.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
    setup.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Complete,
        setup.completed_at_ms(),
        setup.finalization_outcome(),
    );
    broker.store.save_exchange(&setup).unwrap();
    let admission = ExchangeAdmission::chat("Continue setup".into());
    assert_eq!(
        broker.continued_check_setup(&admission).unwrap(),
        Some(crate::checks::CheckSetupState::Reviewed)
    );

    let catalog = broker.check_runner.catalog(None).unwrap();
    broker
        .check_runner
        .configure(
            broker.check_runner.session_owner(),
            serde_json::from_value(json!({
                "expected_revision": catalog["revision"],
                "config": {}
            }))
            .unwrap(),
            None,
            None,
        )
        .await
        .unwrap();

    assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);
    assert!(
        broker
            .check_runner
            .history()
            .unwrap()
            .iter()
            .all(|run| run.commands.is_empty())
    );
}

#[tokio::test]
async fn check_setup_followups_do_not_cross_task_or_rollback_boundaries() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let (mut setup, _, _) = broker
        .test_stage_interaction_for_turn("Configure project checks", true, 100)
        .await
        .unwrap();
    let admission = ExchangeAdmission::chat("Ordinary chat".into());
    broker.store.save_exchange(&setup).unwrap();
    assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);

    setup.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
    setup.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Complete,
        setup.completed_at_ms(),
        setup.finalization_outcome(),
    );
    broker.store.save_exchange(&setup).unwrap();

    setup.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Cancelled,
        setup.completed_at_ms(),
        setup.finalization_outcome(),
    );
    broker.store.save_exchange(&setup).unwrap();
    assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);
    setup.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Complete,
        setup.completed_at_ms(),
        setup.finalization_outcome(),
    );
    setup
        .set_disposition(crate::exchange::HistoryDisposition::RolledBack)
        .unwrap();
    broker.store.save_exchange(&setup).unwrap();
    assert_eq!(broker.continued_check_setup(&admission).unwrap(), None);
}

#[tokio::test]
async fn pauses_and_resumes_structured_questions_during_ordinary_chat() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), true);

    let prompted = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "prompt me with a multiple-choice question" }),
        })
        .await;
    assert!(prompted.response.error().is_none());
    assert!(prompted.event.iter().any(|event| event.event == "question"));
    let initial = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(initial.len(), 1);
    let exchange_id = initial[0].id.clone();
    let checkpoint_before = initial[0].checkpoint_before.clone();
    assert!(initial[0].checkpoint_after.is_none());
    assert!(initial[0].completed_at_ms().is_none());
    let pending = broker.snapshot().unwrap().active_elicitation.unwrap();
    assert_eq!(pending.owner, ElicitationOwner::Interaction);
    assert_eq!(
        pending.elicitation.current_question().unwrap().header,
        "Migration"
    );

    let answered = broker
        .dispatch(Request {
            id: 2,
            method: "session.question.answer".into(),
            params: json!({
                "question_id": pending.elicitation.current_question().unwrap().id,
                "response": { "kind": "selected", "option": "Staged" }
            }),
        })
        .await;
    assert!(answered.response.error().is_none());

    let continued = broker
        .dispatch(Request {
            id: 3,
            method: "session.question.continue".into(),
            params: json!({}),
        })
        .await;
    assert!(continued.response.error().is_none());
    assert!(
        continued
            .event
            .iter()
            .filter(|event| event.event == "document_changed")
            .any(|event| event.payload.to_string().contains("\"kind\":\"working\"")),
        "resuming a chat clarification must restore the Working footer in live patches"
    );
    assert!(broker.snapshot().unwrap().active_elicitation.is_none());
    let completed = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(completed.len(), 1);
    assert_eq!(completed[0].id, exchange_id);
    assert_eq!(completed[0].checkpoint_before, checkpoint_before);
    assert!(completed[0].checkpoint_after.is_some());
    assert_eq!(completed[0].state(), ExchangeState::Complete);
    assert!(completed[0].node_list.iter().any(|node| matches!(node,
        ExchangeNode::QuestionPresented { question, answer: Some(answer), .. }
            if question.questions[0].header == "Migration" && answer.contains("Staged"))));
    assert!(completed[0].node_list.iter().any(|node| matches!(node,
        ExchangeNode::ExchangeInput { prompt } if prompt.intent == crate::exchange::InputIntent::Answer)));
    let rendered = timeline_text(&broker.snapshot().unwrap());
    assert!(rendered.contains("Questions · 1/1 answered"));
    assert!(rendered.contains("You answered: Staged"));
}

#[tokio::test]
async fn auto_compaction_requires_fresh_usage_and_runs_once_per_turn() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "data_root":data.path(),"workspace":repository.path(),"client_id":"automatic-compaction",
        "provider":{"kind":"mock","command":[]},"model":"mock-model","effort":"low"
    }))?)?;
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"First turn"}),
        })
        .await;
    assert!(result.response.error().is_none());
    broker.session.auto_compaction = crate::session::AutoCompaction {
        enabled: true,
        threshold_tokens: 40_000,
    };
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.context_usage_fresh = false;
    broker.auto_compact().await?;
    assert!(
        broker
            .store
            .list_session_event(&broker.session.id)?
            .is_empty()
    );
    broker.context_usage_fresh = true;
    broker.auto_compact().await?;
    let records = broker.store.list_session_event(&broker.session.id)?;
    assert_eq!(records.len(), 1);
    let SessionEventKind::Compaction { compaction } = &records[0].detail else {
        panic!("missing compaction")
    };
    assert!(!compaction.manual);
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.auto_compact().await?;
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        1
    );
    let result = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Second turn"}),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(
        !broker.context_usage_fresh,
        "new input reused prior-turn context usage"
    );
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        1,
        "a turn without fresh usage triggered compaction"
    );
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.context_usage_fresh = true;
    broker.auto_compact().await?;
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        2
    );
    let result = broker
        .dispatch(Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Third turn"}),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(!broker.context_usage_fresh);
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        2
    );
    broker.store.save_session_event(&SessionEventRecord {
        id: "native-running".into(),
        session_id: broker.session.id.clone(),
        owner: crate::timeline::TimelineOwner::session(&broker.session.id),
        created_at_ms: 1,
        detail: SessionEventKind::Compaction {
            compaction: (crate::provider::Compaction {
                id: "native-running".into(),
                thread_id: broker.session.provider_session_id.clone().unwrap(),
                state: crate::provider::CompactionState::Running,
                manual: false,
                started_at_ms: Some(1),
                completed_at_ms: None,
                result: Default::default(),
            })
            .into(),
        },
    })?;
    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    broker.context_usage_fresh = true;
    broker.auto_compact().await?;
    assert_eq!(
        broker.store.list_session_event(&broker.session.id)?.len(),
        3,
        "native compaction must exclude another request"
    );
    Ok(())
}

#[tokio::test]
async fn compaction_captures_context_usage_without_creating_an_interaction() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "compact-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let prompted = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "establish provider context" }),
        })
        .await;
    assert!(prompted.response.error().is_none());
    let interaction_count = broker
        .store
        .list_exchange(&broker.session.id)
        .unwrap()
        .len();

    broker.session.context_usage = ContextUsage::reported(50_000, 100_000);
    let compacted = broker
        .dispatch(Request {
            id: 2,
            method: "session.compact".into(),
            params: json!({}),
        })
        .await;

    assert!(compacted.response.error().is_none());
    let statuses = compacted
        .event
        .iter()
        .filter(|event| event.event == "document_changed")
        .filter_map(|event| {
            event
                .payload
                .pointer("/status/kind")
                .and_then(Value::as_str)
        })
        .collect::<Vec<_>>();
    assert_eq!(statuses, ["compacting", "idle"]);
    assert!(timeline_text(&broker.snapshot().unwrap()).contains("Context compacted"));
    assert_eq!(
        broker
            .session
            .context_usage
            .as_ref()
            .unwrap()
            .remaining_percent,
        80
    );
    assert_eq!(
        broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .len(),
        interaction_count
    );
    assert!(
        compacted
            .event
            .iter()
            .any(|event| event.event == "context_compacted")
    );
    let records = broker.store.list_session_event(&broker.session.id).unwrap();
    assert_eq!(records.len(), 1);
    let SessionEventKind::Compaction { compaction } = &records[0].detail else {
        panic!("compaction record missing")
    };
    assert_eq!(
        compaction.state,
        crate::provider::CompactionState::Completed
    );
    assert!(compaction.manual);
    assert_eq!(compaction.result.context_tokens_before, Some(50_000));
    assert_eq!(compaction.result.context_tokens_after, Some(20_000));
    assert!(timeline_text(&broker.snapshot().unwrap()).contains("50.0k → 20.0k tokens"));
    for (state, heading) in [
        (crate::provider::CompactionState::Running, None),
        (
            crate::provider::CompactionState::Failed,
            Some("Context compaction failed"),
        ),
        (
            crate::provider::CompactionState::Unconfirmed,
            Some("Context compaction completion unconfirmed"),
        ),
        (
            crate::provider::CompactionState::Completed,
            Some("Context compacted"),
        ),
    ] {
        let mut record = records[0].clone();
        let SessionEventKind::Compaction { compaction } = &mut record.detail else {
            unreachable!()
        };
        compaction.state = state;
        broker.store.save_session_event(&record).unwrap();
        let snapshot = broker.snapshot().unwrap();
        let text = timeline_text(&snapshot);
        if let Some(heading) = heading {
            assert!(matches!(
                snapshot.status,
                crate::session::state_machine::SessionPhase::Idle
            ));
            assert!(text.contains(heading));
        } else {
            assert!(matches!(
                snapshot.status,
                crate::session::state_machine::SessionPhase::Compacting { started_at_ms: 100 }
            ));
            assert!(
                !snapshot
                    .timeline
                    .iter()
                    .any(|entry| matches!(entry, TimelineEntry::SessionEvent { .. }))
            );
            assert!(
                !text.contains("Compacting"),
                "live status belongs to the status document"
            );
        }
    }

    let mut exchange = broker
        .store
        .list_exchange(&broker.session.id)
        .unwrap()
        .remove(0);
    let automatic = crate::provider::Compaction {
        id: "background".into(),
        thread_id: broker.session.provider_session_id.clone().unwrap(),
        state: crate::provider::CompactionState::Running,
        manual: false,
        started_at_ms: None,
        completed_at_ms: None,
        result: Default::default(),
    };
    exchange.observe_compaction(automatic.clone(), 200);
    let position = exchange.node_list.len() - 1;
    broker.store.save_exchange(&exchange).unwrap();
    let running = broker.snapshot().unwrap();
    assert!(matches!(
        running.status,
        crate::session::state_machine::SessionPhase::Compacting { started_at_ms: 200 }
    ));
    let running_text = timeline_text(&running);
    assert!(!running_text.contains("Compacting"));
    assert!(!running_text.contains("Compacting context"));
    let mut completion = automatic;
    completion.state = crate::provider::CompactionState::Completed;
    completion.result.summary_content = Some("Retain the original constraints.".into());
    let update = compaction_event(&broker.session.id, completion);
    let mut current =
        completed_interaction("next-compaction-exchange", &broker.session.id, Vec::new());
    current.ordinal = interaction_count as u64 + 1;
    current.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Running,
        current.completed_at_ms(),
        current.finalization_outcome(),
    );
    current.phase = crate::exchange::ExchangePhase::fixture(
        current.state(),
        None,
        current.finalization_outcome(),
    );
    broker.store.save_exchange(&current).unwrap();
    let mut runtime = ExchangeRuntime {
        task_permit: None,
        exchange_id: current.id.clone(),

        checklist: ChecklistTracker::default(),
        checkpoint: None,
        tool_gate: None,
        retraction_eligible: true,
        active_wait: None,
    };
    let fixture_scope = crate::provider::InvocationScope::new(
        crate::provider::ExecutionOwner::root(&broker.session.id),
        current.id.clone(),
    );
    let (sink, mut stream) = publication::channel();
    broker.event_sink = Some(sink);
    let mut events = Vec::new();
    broker
        .test_apply_provider_event(&mut runtime, &mut current, update.clone(), &mut events)
        .await
        .unwrap();
    assert!(
        current.node_list.is_empty(),
        "background completion entered the next exchange"
    );
    let notification = tokio::time::timeout(std::time::Duration::from_secs(1), stream.recv())
        .await
        .expect("cross-turn compaction patch was not delivered")
        .unwrap()
        .unwrap();
    assert!(matches!(
        notification,
        SessionUpdate::DocumentChanged { .. }
    ));
    assert!(events.is_empty());
    broker.event_sink = None;
    assert!(
        broker
            .observe_session_event(update.clone())
            .unwrap()
            .is_empty()
    );
    assert!(broker.observe_session_event(update).unwrap().is_empty());
    let restored = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(restored.len(), interaction_count + 1);
    let ExchangeNode::Compaction { compaction, .. } = &restored[0].node_list[position] else {
        panic!("original compaction position lost")
    };
    assert_eq!(
        compaction.state,
        crate::provider::CompactionState::Completed
    );
    assert_eq!(
        compaction.result.summary_content.as_deref(),
        Some("Retain the original constraints.")
    );
    assert_eq!(
        broker
            .store
            .list_session_event(&broker.session.id)
            .unwrap()
            .len(),
        1
    );
    assert!(matches!(
        broker.snapshot().unwrap().status,
        crate::session::state_machine::SessionPhase::Working { .. }
    ));

    let mut record = broker
        .store
        .list_session_event(&broker.session.id)
        .unwrap()
        .remove(0);
    record.id = "late-manual-compaction".into();
    record.created_at_ms = 300;
    let SessionEventKind::Compaction { compaction } = &mut record.detail else {
        unreachable!()
    };
    compaction.id = "late-manual-compaction".into();
    compaction.started_at_ms = Some(300);
    compaction.completed_at_ms = Some(400);
    compaction.result.context_tokens_after = None;
    broker.store.save_session_event(&record).unwrap();
    for used in [32_000, 64_000] {
        let usage_event = ProviderEvent::Runtime {
            scope: fixture_scope.clone(),
            update: crate::provider::RuntimeUpdate::ContextUsage {
                usage: ContextUsage::reported(used, 100_000).unwrap(),
            },
        };
        broker
            .test_apply_provider_event(&mut runtime, &mut current, usage_event, &mut events)
            .await
            .unwrap();
    }
    let record = broker
        .store
        .list_session_event(&broker.session.id)
        .unwrap()
        .pop()
        .unwrap();
    let SessionEventKind::Compaction { compaction } = &record.detail else {
        unreachable!()
    };
    assert_eq!(compaction.result.context_tokens_before, Some(50_000));
    assert_eq!(compaction.result.context_tokens_after, Some(32_000));
    assert!(timeline_text(&broker.snapshot().unwrap()).contains("50.0k → 32.0k tokens"));

    let automatic = crate::provider::Compaction {
        id: "current-compaction".into(),
        thread_id: broker.session.provider_session_id.clone().unwrap(),
        state: crate::provider::CompactionState::Running,
        manual: false,
        started_at_ms: None,
        completed_at_ms: None,
        result: Default::default(),
    };
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut current,
            compaction_event(&broker.session.id, automatic.clone()),
            &mut events,
        )
        .await
        .unwrap();
    let mut completed = automatic;
    completed.state = crate::provider::CompactionState::Completed;
    broker
        .test_apply_provider_event(
            &mut runtime,
            &mut current,
            compaction_event(&broker.session.id, completed),
            &mut events,
        )
        .await
        .unwrap();
    let usage_event = ProviderEvent::Runtime {
        scope: fixture_scope.clone(),
        update: crate::provider::RuntimeUpdate::ContextUsage {
            usage: ContextUsage::reported(12_000, 100_000).unwrap(),
        },
    };
    broker
        .test_apply_provider_event(&mut runtime, &mut current, usage_event, &mut events)
        .await
        .unwrap();
    let ExchangeNode::Compaction { compaction, .. } = current.node_list.last().unwrap() else {
        unreachable!()
    };
    assert_eq!(compaction.result.context_tokens_before, Some(64_000));
    assert_eq!(compaction.result.context_tokens_after, Some(12_000));
    assert!(timeline_text(&broker.snapshot().unwrap()).contains("64.0k → 12.0k tokens"));
}

#[tokio::test]
async fn initializes_one_named_session_without_resuming_a_leased_session() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let initialize = |client_id: &str, name: &str| InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: repository.path().to_string_lossy().into_owned(),
        client_id: client_id.into(),
        provider: ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            command: vec!["mock".into()],
        },
        model: "mock-model".into(),
        effort: "low".into(),
        session_id: None,
        new_session_name: Some(name.into()),
        goal_max_turns: 20,
        lease_conflict_action: None,
    };

    let first = HarnessBroker::initialize_with_clock(
        initialize("first-client", "first"),
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let second = HarnessBroker::initialize_with_clock(
        initialize("second-client", "  second  "),
        Box::new(FixedClock(110)),
    )
    .unwrap();

    assert_ne!(first.session.id, second.session.id);
    assert_eq!(first.session.name, "first");
    assert_eq!(second.session.name, "second");
    assert_eq!(first.session.approval_mode, ApprovalMode::Ask);
    assert_eq!(second.session.approval_mode, ApprovalMode::Ask);
    assert_eq!(second.store.list_session(None).unwrap().len(), 2);
}

#[test]
fn explicit_session_initialization_validates_before_leasing_and_resumes_exact_id() {
    let workspace = repository();
    let other_workspace = repository();
    let data = tempfile::tempdir().unwrap();
    let mut request = InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: workspace.path().to_string_lossy().into_owned(),
        client_id: "source-client".into(),
        provider: ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            command: vec!["mock".into()],
        },
        model: "mock-model".into(),
        effort: "low".into(),
        session_id: None,
        new_session_name: Some("source".into()),
        goal_max_turns: 20,
        lease_conflict_action: None,
    };
    let mut source =
        HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100))).unwrap();
    source.release_lease().unwrap();
    request.client_id = "destination-client".into();
    request.new_session_name = Some(String::new());
    let newest =
        HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(110))).unwrap();
    assert_ne!(source.session.id, newest.session.id);
    request.new_session_name = None;
    request.session_id = Some(source.session.id.clone());

    let mut incompatible = request.clone();
    incompatible.workspace = other_workspace.path().to_string_lossy().into_owned();
    let failure = HarnessBroker::initialize_with_clock(incompatible, Box::new(FixedClock(120)))
        .err()
        .unwrap();
    assert!(failure.to_string().contains("different workspace"));
    assert!(
        source
            .store
            .load_session(&source.session.id)
            .unwrap()
            .unwrap()
            .lease_owner
            .is_none()
    );

    let mut stored = source
        .store
        .load_session(&source.session.id)
        .unwrap()
        .unwrap();
    stored.provider = forge_providers::registry::alternate_id(&request.provider.kind)
        .expect("alternate registered provider")
        .to_string();
    source.store.save_session(&stored).unwrap();
    let failure = HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(130)))
        .err()
        .unwrap();
    assert!(
        failure
            .to_string()
            .contains("different configured provider")
    );
    assert!(
        source
            .store
            .load_session(&source.session.id)
            .unwrap()
            .unwrap()
            .lease_owner
            .is_none()
    );
    stored.provider = "mock".into();
    stored.approval_mode = ApprovalMode::Ask;
    stored.workspace = format!("{}//./", stored.workspace);
    source.store.save_session(&stored).unwrap();

    let resumed = HarnessBroker::initialize_with_clock(request, Box::new(FixedClock(140))).unwrap();
    assert_eq!(resumed.session.id, source.session.id);
    assert_eq!(resumed.session.name, "source");
    assert_eq!(resumed.session.approval_mode, ApprovalMode::Ask);
    assert_eq!(resumed.store.list_session(None).unwrap().len(), 2);
}

#[tokio::test]
async fn forks_a_live_session_without_taking_its_lease() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let initialize = |client_id: &str, lease_conflict_action: Option<String>| InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: repository.path().to_string_lossy().into_owned(),
        client_id: client_id.into(),
        provider: ProviderLaunch {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            command: vec!["mock".into()],
        },
        model: "mock-model".into(),
        effort: "low".into(),
        session_id: None,
        new_session_name: None,
        goal_max_turns: 20,
        lease_conflict_action,
    };

    let mut source = HarnessBroker::initialize_with_clock(
        initialize("source-client", None),
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let source_session_id = source.session.id.clone();
    source
        .dispatch(Request {
            id: 0,
            method: "session.permission.set".into(),
            params: json!({ "change":{"kind":"set_approval","mode":"ask"} }),
        })
        .await;
    let prompt = source
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "preserve this interaction" }),
        })
        .await;
    assert!(prompt.response.error().is_none());
    assert!(source.session.native_fork);
    source.session.context_usage = ContextUsage::reported(50, 200);
    source.save_session().unwrap();

    let conflict = match HarnessBroker::initialize_with_clock(
        initialize("fork-client", None),
        Box::new(FixedClock(110)),
    ) {
        Ok(_) => panic!("second controller should encounter the live lease"),
        Err(error) => error,
    };
    let conflict = conflict
        .downcast_ref::<crate::session::SessionLeaseConflict>()
        .expect("structured lease conflict");
    assert_eq!(conflict.session_id, source_session_id);
    assert!(conflict.native_fork);

    let fork_controller = HarnessBroker::initialize_with_clock(
        initialize("fork-client", Some("new".into())),
        Box::new(FixedClock(110)),
    )
    .unwrap();
    let preparation = prepare_provider_fork(
        data.path(),
        "fork-client",
        "mock",
        &fork_controller.capability,
        &fork_controller.session.id,
        &json!({ "session_id": source_session_id, "name": "fork investigation" }),
        crate::session::SessionPurpose::Conversation,
    )
    .unwrap();
    assert!(matches!(
        preparation.child.provider_fork_state,
        ProviderForkState::Preparing { .. }
    ));
    assert_eq!(preparation.provider_request.checkpoint_id.as_deref(), None);
    let child_session_id = preparation.child.id.clone();
    let inherited_source = source
        .snapshot()
        .unwrap()
        .timeline
        .into_iter()
        .filter(|entry| !matches!(entry, TimelineEntry::Status { .. }))
        .map(|entry| serde_json::to_value(entry).unwrap())
        .collect::<Vec<_>>();
    let provider_fork = fork_controller
        .provider
        .session_fork(preparation.provider_request)
        .await
        .unwrap();
    let mut child_initialize = initialize("fork-client", None);
    child_initialize.session_id = Some(child_session_id.clone());
    let mut child_controller = HarnessBroker::initialize_with_clock(
        child_initialize,
        Box::new(FixedClock(preparation.child.created_at_ms + 1)),
    )
    .unwrap();
    let ready_event = child_controller
        .complete_provider_fork(Ok(provider_fork))
        .unwrap();
    assert_eq!(ready_event.event, "session_fork_ready");
    let snapshot = serde_json::to_value(child_controller.snapshot().unwrap()).unwrap();
    assert_ne!(snapshot["session"]["id"], source_session_id);
    assert_eq!(snapshot["session"]["name"], "fork investigation");
    assert_eq!(snapshot["session"]["approval_mode"], "ask");
    assert_eq!(snapshot["session"]["provider_fork_state"]["state"], "ready");
    assert_eq!(snapshot["session"]["context_usage"]["used"], 50);
    assert_eq!(snapshot["session"]["context_usage"]["size"], 200);
    assert_eq!(
        snapshot["session"]["context_usage"]["remaining_percent"],
        75
    );
    assert!(snapshot.get("exchange").is_none() && snapshot.get("timeline").is_none());
    let timeline = child_controller.snapshot().unwrap().timeline;
    assert_eq!(timeline.len(), inherited_source.len() + 2);
    let inherited = timeline
        .iter()
        .filter_map(|entry| match entry {
            TimelineEntry::Inherited { entry, .. } => Some(serde_json::to_value(entry).unwrap()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(inherited, inherited_source);
    assert!(
        inherited
            .iter()
            .any(|entry| entry["exchange"]["prompt"] == "preserve this interaction")
    );
    assert!(
        child_controller
            .store
            .list_exchange(&child_session_id)
            .unwrap()
            .is_empty()
    );
    assert!(
        child_controller
            .task_controller
            .current()
            .unwrap()
            .is_none()
    );
    assert!(matches!(
        &timeline[inherited.len()],
        TimelineEntry::SessionEvent { .. }
    ));
    assert!(matches!(
        timeline.last().unwrap(),
        TimelineEntry::Status {
            status: SessionPhase::Idle,
            ..
        }
    ));
    let failed_preparation = prepare_provider_fork(
        data.path(),
        "fork-client",
        "mock",
        &fork_controller.capability,
        &fork_controller.session.id,
        &json!({ "session_id": source_session_id }),
        crate::session::SessionPurpose::Conversation,
    )
    .unwrap();
    let mut failed_initialize = initialize("fork-client", None);
    failed_initialize.session_id = Some(failed_preparation.child.id);
    let mut failed_controller =
        HarnessBroker::initialize_with_clock(failed_initialize, Box::new(FixedClock(130))).unwrap();
    let failed_event = failed_controller
        .complete_provider_fork(Err("native fork failed".into()))
        .unwrap();
    assert_eq!(failed_event.event, "session_fork_failed");
    assert!(matches!(
        failed_controller.provider_fork_state(),
        ProviderForkState::Failed { message, .. } if message == "native fork failed"
    ));
    assert!(
        failed_controller
            .snapshot()
            .unwrap()
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Inherited { .. }))
    );

    let later_source = source
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "source-only later message" }),
        })
        .await;
    assert!(later_source.response.error().is_none());
    let child_prompt = child_controller
        .dispatch(Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "child-only new message" }),
        })
        .await;
    assert!(child_prompt.response.error().is_none());
    let child_timeline = child_controller.snapshot().unwrap().timeline;
    let child_inherited = child_timeline
        .iter()
        .filter_map(|entry| match entry {
            TimelineEntry::Inherited { entry, .. } => Some(serde_json::to_value(entry).unwrap()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(child_inherited, inherited_source);
    assert!(child_timeline.iter().skip(inherited_source.len() + 1).any(
        |entry| matches!(entry, TimelineEntry::Exchange { exchange, .. }
            if exchange.prompt == "child-only new message")
    ));
    let preview = source
        .dispatch(Request {
            id: 4,
            method: "session.preview".into(),
            params: json!({ "session_id": child_session_id }),
        })
        .await;
    assert!(preview.response.error().is_none());
    assert!(
        serde_json::to_string(preview.response.result().unwrap())
            .unwrap()
            .contains("preserve this interaction")
    );

    let grandchild = prepare_provider_fork(
        data.path(),
        "fork-client",
        "mock",
        &child_controller.capability,
        &child_session_id,
        &json!({ "session_id": child_session_id }),
        crate::session::SessionPurpose::Conversation,
    )
    .unwrap();
    let grandchild_history = child_controller
        .store
        .with_fork_history(
            &grandchild.child.id,
            Vec::new(),
            &child_controller.plan_file,
        )
        .unwrap();
    assert_eq!(grandchild_history.len(), child_timeline.len() - 1);
    for (inherited, source) in grandchild_history.iter().zip(&child_timeline) {
        let TimelineEntry::Inherited { entry, .. } = inherited else {
            panic!("grandchild history must be presentation-only");
        };
        assert!(!matches!(entry.as_ref(), TimelineEntry::Inherited { .. }));
        let source = match source {
            TimelineEntry::Inherited { entry, .. } => entry.as_ref(),
            source => source,
        };
        assert_eq!(
            serde_json::to_value(entry).unwrap(),
            serde_json::to_value(source).unwrap()
        );
    }
    let persisted_source = child_controller
        .store
        .load_session(&source_session_id)
        .unwrap()
        .expect("source session");
    assert_eq!(
        persisted_source.lease_owner.as_deref(),
        Some("source-client")
    );
}

fn preference_provider() -> anyhow::Result<crate::provider::test_support::ProviderTestSession> {
    use forge_provider_api::{ProviderFailure, ProviderFailureKind, ProviderOperation};
    let descriptor = forge_provider_api::ProviderDescriptor {
        kind: forge_providers::registry::default_id(),
        label: "Preference fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |_request, _sink, _cancelled| async {
            Err(crate::provider::ProviderError::Failure(
                ProviderFailure::new(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                ),
            ))
        },
    )
    .with_models(vec![
        forge_provider_api::ProviderModel {
            id: "gpt-5.6".into(),
            service_tiers: vec![
                forge_provider_api::ServiceTier::Standard,
                forge_provider_api::ServiceTier::Fast,
                forge_provider_api::ServiceTier::Ultrafast,
            ],
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: vec![
                forge_provider_api::ProviderContextWindow {
                    id: "default".into(),
                    token_limit: None,
                },
                forge_provider_api::ProviderContextWindow {
                    id: "long_context".into(),
                    token_limit: None,
                },
            ],
            default_context_window: Some("default".into()),
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: true,
        },
        forge_provider_api::ProviderModel {
            id: "gpt-5.4".into(),
            service_tiers: vec![
                forge_provider_api::ServiceTier::Standard,
                forge_provider_api::ServiceTier::Fast,
                forge_provider_api::ServiceTier::Ultrafast,
            ],
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: vec![
                forge_provider_api::ProviderContextWindow {
                    id: "default".into(),
                    token_limit: None,
                },
                forge_provider_api::ProviderContextWindow {
                    id: "long_context".into(),
                    token_limit: None,
                },
            ],
            default_context_window: Some("default".into()),
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: false,
        },
    ]);
    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

#[tokio::test]
async fn carries_preferences_into_a_new_session_and_mode_into_a_resume() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "settings-client".into(),
            provider: crate::provider::test_support::default_launch(),
            model: "default".into(),
            effort: "medium".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let preference_provider = self::preference_provider().unwrap();
    broker.provider = preference_provider.provider_handle();
    let selected = broker
        .dispatch(Request {
            id: 0,
            method: "session.permission.set".into(),
            params: json!({ "change":{"kind":"enable_yolo"} }),
        })
        .await;
    assert!(selected.response.error().is_none());
    let configured = broker
        .dispatch(Request {
            id: 1,
            method: "session.configure".into(),
            params: json!({
                "model": "gpt-5.6",
                "effort": "low",
                "context_window": "default",
                "service_tier": "ultrafast"
            }),
        })
        .await;
    assert!(
        configured.response.error().is_none(),
        "{:?}",
        configured.response.error()
    );
    for invalid in [json!("warp"), json!(true), json!(null)] {
        assert!(
            broker
                .configure_session(json!({"service_tier":invalid}))
                .await
                .is_err()
        );
        assert_eq!(
            broker.session.service_tier,
            crate::provider::ServiceTier::Ultrafast
        );
    }
    broker.capability.ultrafast_mode = false;
    assert!(
        broker
            .configure_session(json!({"service_tier":"ultrafast"}))
            .await
            .is_err()
    );
    assert_eq!(
        broker.session.service_tier,
        crate::provider::ServiceTier::Ultrafast
    );
    broker.capability.ultrafast_mode = true;

    broker
        .dispatch(Request {
            id: 11,
            method: "session.configure".into(),
            params: json!({
                "model": "gpt-5.4",
                "effort": "high",
                "context_window": "long_context"
            }),
        })
        .await;
    broker
        .dispatch(Request {
            id: 12,
            method: "session.configure".into(),
            params: json!({
                "model": "gpt-5.6",
                "effort": "low",
                "context_window": "default"
            }),
        })
        .await;
    let preference = broker
        .store
        .load_preference(&broker.session.workspace, &broker.session.provider)
        .unwrap()
        .unwrap();
    assert_eq!(
        preference.model_setting["gpt-5.4"]
            .context_window
            .as_deref(),
        Some("long_context")
    );
    assert_eq!(
        preference.model_setting["gpt-5.6"].reasoning.as_deref(),
        Some("low")
    );
    let created = broker
        .dispatch(Request {
            id: 2,
            method: "session.new".into(),
            params: json!({}),
        })
        .await;
    let session = created.response.result().unwrap()["session"].clone();
    assert_eq!(session["name"], "");
    assert_eq!(session["model"], "gpt-5.6");
    assert_eq!(session["effort"], "low");
    assert_eq!(session["context_window"], "default");
    assert_eq!(session["service_tier"], "ultrafast");
    assert_eq!(session["approval_mode"], "ask");
    drop(broker);
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "settings-client".into(),
            provider: crate::provider::test_support::default_launch(),
            model: "default".into(),
            effort: "medium".into(),
            session_id: Some(session["id"].as_str().expect("child session id").to_owned()),
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let preference_provider = self::preference_provider().unwrap();
    broker.provider = preference_provider.provider_handle();
    let selected = broker
        .dispatch(Request {
            id: 21,
            method: "session.permission.set".into(),
            params: json!({ "change":{"kind":"set_approval","mode":"ask"} }),
        })
        .await;
    assert!(selected.response.error().is_none());
    let renamed = broker
        .dispatch(Request {
            id: 3,
            method: "session.rename".into(),
            params: json!({ "session_id": session["id"], "name": "  Architecture review  " }),
        })
        .await;
    assert_eq!(
        renamed.response.result().unwrap()["name"],
        "Architecture review"
    );
    assert!(!renamed.event.iter().any(|event| {
        event.event == "provider_event"
            && event.payload["kind"] == "timeline_session_event"
            && event.payload["data"]["event"]["name"] == "Architecture review"
    }));
    let stale = broker
        .observer
        .rename_session(json!({ "session_id": session["id"],
        "name": "Generated name", "expected_name": "Previous name" }));
    assert!(
        stale
            .unwrap_err()
            .to_string()
            .contains("changed during generation")
    );
    assert_eq!(
        broker
            .store
            .load_session(session["id"].as_str().unwrap())
            .unwrap()
            .unwrap()
            .name,
        "Architecture review"
    );
    drop(broker);

    let resumed_session_id = session["id"].as_str().expect("new session id").to_owned();
    let restarted = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "restarted-settings-client".into(),
            provider: crate::provider::test_support::default_launch(),
            model: "default".into(),
            effort: "medium".into(),
            session_id: Some(resumed_session_id),
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();
    assert_eq!(restarted.session.model, "gpt-5.6");
    assert_eq!(restarted.session.effort, "low");
    assert_eq!(
        restarted.session.service_tier,
        crate::provider::ServiceTier::Ultrafast
    );
    assert_eq!(restarted.session.approval_mode, ApprovalMode::Ask);
    assert!(
        !restarted
            .snapshot()
            .unwrap()
            .timeline
            .iter()
            .any(|entry| matches!(
                entry,
                TimelineEntry::SessionEvent { event, .. }
                    if matches!(
                        &event.detail,
                        SessionEventKind::Renamed { .. }
                    )
            ))
    );
}

#[tokio::test]
async fn canonical_provider_defaults_apply_to_new_conversations_without_changing_resumes() {
    let other_repository = repository();
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let request = InitializeRequest {
        background: None,
        provider_launch_by_provider: Default::default(),
        config_dir: None,
        data_root: data.path().to_string_lossy().into_owned(),
        permission_file: None,
        workspace: repository.path().to_string_lossy().into_owned(),
        client_id: "provider-default-test".into(),
        provider: crate::provider::test_support::default_launch(),
        model: "original-model".into(),
        effort: "medium".into(),
        session_id: None,
        new_session_name: None,
        goal_max_turns: 20,
        lease_conflict_action: None,
    };
    let mut initial =
        HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100))).unwrap();
    assert_eq!(initial.session.approval_mode, ApprovalMode::Ask);
    initial.session.approval_mode = ApprovalMode::Allow;
    initial.save_session().unwrap();
    let original_id = initial.session.id.clone();
    let mut settings = crate::settings::SettingsStore::open(data.path()).unwrap();
    settings.save_provider(0, forge_providers::registry::default_id().as_str(), &json!({
        "model": "preferred-model", "effort": "high", "context_window": "long_context", "service_tier": "fast"
    })).unwrap();
    drop(initial);

    let mut resumed =
        HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(200))).unwrap();
    assert_eq!(resumed.session.id, original_id);
    assert_eq!(resumed.session.model, "original-model");
    assert_eq!(resumed.session.approval_mode, ApprovalMode::Allow);
    let created = resumed
        .dispatch(Request {
            id: 1,
            method: "session.new".into(),
            params: json!({}),
        })
        .await;
    let child = &created.response.result().unwrap()["session"];
    assert_eq!(child["model"], "preferred-model");
    assert_eq!(child["effort"], "high");
    assert_eq!(child["context_window"], "long_context");
    assert_eq!(child["service_tier"], "fast");
    assert_eq!(child["approval_mode"], "ask");

    let fresh = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            workspace: other_repository.path().to_string_lossy().into_owned(),
            ..request
        },
        Box::new(FixedClock(300)),
    )
    .unwrap();
    assert_eq!(fresh.session.model, "preferred-model");
    assert_eq!(fresh.session.approval_mode, ApprovalMode::Ask);
    assert_eq!(
        fresh.session.resolved_model.as_deref(),
        Some("preferred-model")
    );
    assert_eq!(fresh.session.effort, "high");
    assert_eq!(
        fresh.session.context_window.as_deref(),
        Some("long_context")
    );
    assert_eq!(
        fresh.session.service_tier,
        crate::provider::ServiceTier::Fast
    );
}

#[tokio::test]
async fn rejects_inline_model_effort_and_mode_values_outside_provider_capabilities() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "inline-config-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "medium".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();

    let invalid_model = broker
        .dispatch(Request {
            id: 1,
            method: "session.configure".into(),
            params: json!({ "model": "sol" }),
        })
        .await;
    assert!(invalid_model.response.error().is_some());
    assert_eq!(broker.session.model, "mock-model");

    let invalid_effort = broker
        .dispatch(Request {
            id: 2,
            method: "session.configure".into(),
            params: json!({ "effort": "xhigh" }),
        })
        .await;
    assert!(invalid_effort.response.error().is_some());
    assert_eq!(broker.session.effort, "medium");

    let invalid_mode = broker
        .dispatch(Request {
            id: 3,
            method: "session.permission.set".into(),
            params: json!({ "mode": "invalid" }),
        })
        .await;
    assert!(invalid_mode.response.error().is_some());
    assert_eq!(broker.session.approval_mode, ApprovalMode::Ask);
}

#[tokio::test]
async fn previews_a_stored_session_without_acquiring_its_lease() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "preview-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let source_session_id = broker.session.id.clone();
    let prompt = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "preserve preview history" }),
        })
        .await;
    assert!(prompt.response.error().is_none());
    let created = broker
        .dispatch(Request {
            id: 2,
            method: "session.new".into(),
            params: json!({}),
        })
        .await;
    assert!(created.response.error().is_none());
    let mut source_session = broker
        .store
        .load_session(&source_session_id)
        .unwrap()
        .expect("source session");
    source_session.lease_owner = Some("foreign-client".into());
    broker.store.save_session(&source_session).unwrap();

    let preview = broker
        .dispatch(Request {
            id: 3,
            method: "session.preview".into(),
            params: json!({ "session_id": source_session_id }),
        })
        .await;
    assert!(preview.response.error().is_none());
    let result = preview.response.result().expect("session preview");
    assert_eq!(result["session"]["id"], source_session_id);
    assert!(result.get("exchange").is_none() && result.get("timeline").is_none());
    assert!(
        result["lines"]
            .as_array()
            .unwrap()
            .iter()
            .any(|line| line.as_str().unwrap().contains("preserve preview history"))
    );
    let persisted = broker
        .store
        .load_session(&source_session_id)
        .unwrap()
        .expect("persisted source session");
    assert_eq!(persisted.lease_owner.as_deref(), Some("foreign-client"));
}

#[tokio::test]
async fn resume_keeps_native_identity_without_complete_unconsumed_input_evidence() -> Result<()> {
    for boundary in [
        "missing",
        "sending",
        "accepted",
        "foreign_owner",
        "native_checkpoint",
        "missing_second_input",
    ] {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let session_id = broker.session.id.clone();
        broker.session.provider_session_id = Some("unused-native-history".into());
        if boundary == "native_checkpoint" {
            broker.session.provider_checkpoint_id = Some("native-checkpoint".into());
        }
        broker.store.save_session(&broker.session)?;
        let (mut exchange, _, _) = broker
            .test_stage_interaction_for_turn("Retain this input", true, 100)
            .await?;
        exchange.input_message_id = Some("retained-input".into());
        exchange.checkpoint_before = Some("host-workspace-baseline".into());
        exchange.finish(crate::exchange::ExchangeOutcome::Cancelled, 101)?;
        broker.store.save_exchange(&exchange)?;
        if boundary != "missing" {
            let mut request = broker.input_submission_request(
                ProviderInput::from_text(&exchange.prompt),
                PromptMode::Chat,
            )?;
            if boundary == "foreign_owner" {
                request.execution_owner = Some(crate::provider::ExecutionOwner {
                    session_id: session_id.clone().into(),
                    agent_id: "other-agent".into(),
                });
            }
            let submission = broker.submission_store.prepare_input_request(
                &request,
                broker.provider.descriptor().kind,
                crate::provider::submission::SubmissionOrigin::QueuedInput(
                    crate::exchange::command::InputId::new("retained-input")?,
                ),
            )?;
            match boundary {
                "sending" => {
                    submission.sending(None)?;
                    submission.failed("unknown transport outcome".into())?;
                }
                "accepted" => submission.accepted(None)?,
                _ => submission.cancel()?,
            }
        }
        if boundary == "missing_second_input" {
            let mut second = exchange.clone();
            second.id = uuid::Uuid::new_v4().to_string();
            second.input_message_id = Some("unjournaled-input".into());
            second.ordinal += 1;
            broker.store.save_exchange(&second)?;
        }
        broker.release_lease()?;
        drop(broker);
        let mut request = InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "resumed-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id().unwrap(),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: Some(session_id.clone()),
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        };
        let resumed =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(200)))?;
        assert_eq!(
            resumed.session.provider_session_id.as_deref(),
            Some("unused-native-history"),
            "{boundary}"
        );
        assert_eq!(
            resumed
                .store
                .load_exchange(&exchange.id)?
                .unwrap()
                .checkpoint_before,
            exchange.checkpoint_before
        );
        // A second reopen must not turn retained uncertainty into positive evidence.
        drop(resumed);
        request.client_id = "resumed-again".into();
        let resumed = HarnessBroker::initialize_with_clock(request, Box::new(FixedClock(300)))?;
        assert_eq!(
            resumed.session.provider_session_id.as_deref(),
            Some("unused-native-history"),
            "{boundary}"
        );
    }
    Ok(())
}

#[tokio::test]
async fn resumes_latest_repository_session_after_broker_restart() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "first-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(100)),
    )
    .unwrap();
    let session_id = broker.session.id.clone();
    broker.session = broker
        .store
        .rename_owned_session(
            &broker.session.id,
            &broker.client_id,
            "Persistent analysis",
            None,
            100,
        )
        .unwrap();
    let (mut exchange, _, _) = broker
        .test_stage_interaction_for_turn("interrupted request", true, 100)
        .await
        .unwrap();
    exchange.resume(100).unwrap();
    exchange
        .start_turn(
            crate::provider::TurnAddress {
                scope: crate::turn::test_support::address(&exchange, "unfinished-turn").scope,
                thread_id: "main-thread".into(),
                turn_id: "unfinished-turn".into(),
            },
            100,
        )
        .unwrap();
    broker.store.save_exchange(&exchange).unwrap();
    let child = crate::agent::Agent::pending(&session_id, "reviewer", "review", 100);
    broker.store.save_agent_run(&child).unwrap();
    drop(broker);

    let restarted = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "second-client".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "different-model".into(),
            effort: "high".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();

    assert_eq!(restarted.session.id, session_id);
    assert_eq!(restarted.session.name, "Persistent analysis");
    assert_eq!(restarted.session.model, "mock-model");
    assert_eq!(restarted.session.effort, "low");
    let snapshot = restarted.snapshot().unwrap();
    assert_eq!(snapshot.exchange.len(), 1);
    let recovered = &snapshot.exchange[0];
    assert_eq!(recovered.id, exchange.id);
    assert_eq!(recovered.state(), ExchangeState::Interrupted);
    assert_eq!(recovered.elapsed(90_000), 0);
    assert_eq!(
        recovered.turn[0].state(),
        crate::turn::TurnState::Finished {
            outcome: crate::turn::TurnOutcome::Interrupted,
        }
    );
    let child = restarted
        .store
        .list_agent_run(&session_id)
        .unwrap()
        .pop()
        .unwrap();
    assert_eq!(child.state, crate::agent::AgentState::Closed);
    assert_eq!(
        restarted
            .store
            .list_session(Some(&restarted.session.workspace))
            .unwrap()
            .len(),
        1
    );
}

#[tokio::test]
async fn retry_resumes_the_same_failed_plan_with_a_fresh_budget() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.goal_max_turns = 2;
    broker.provider = plan_fixture_provider(
        &broker,
        PlanFixtureBehavior::Retry {
            submit_from_turn: 2,
        },
    );

    let failed = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan migrate the event format" }),
        })
        .await;
    run_plan_to_input_boundary(&mut broker)
        .await
        .expect_err("fixture must fail planning");

    assert!(failed.response.error().is_none());
    let failed_plan = broker.snapshot().unwrap().active_plan.unwrap();
    let plan_id = failed_plan.id.clone();
    assert_eq!(failed_plan.state, PlanState::Failed);
    assert_eq!(failed_plan.generation.budget.turn_count, 2);
    let rejected = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "keep going" }),
        })
        .await;
    assert_eq!(
        rejected
            .response
            .error()
            .map(|error| error.message.as_str()),
        Some("plan generation stopped; run /plan retry or /plan cancel")
    );

    let retried = broker
        .dispatch(Request {
            id: 3,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan retry" }),
        })
        .await;
    assert!(retried.response.error().is_none());
    run_plan_to_input_boundary(&mut broker).await.unwrap();
    let submitted = broker.snapshot().unwrap().active_plan.unwrap();
    assert_eq!(submitted.id, plan_id);
    assert_eq!(submitted.state, PlanState::AwaitingReview);
    assert_eq!(submitted.generation.budget.turn_count, 1);
    let mut interrupted = submitted;
    PlanStateMachine::apply(
        &mut interrupted,
        PlanEvent::TransitionFailed,
        broker.clock.now_ms(),
    )
    .unwrap();
    broker.store.save_plan(&interrupted).unwrap();
    broker.pause_task(false).await.unwrap();
    let paused = broker.task_controller.current().unwrap().unwrap();
    let resumed = broker
        .dispatch(Request {
            id: 4,
            method: "session.prompt.submit".into(),
            params: json!({"text":"continue the existing design"}),
        })
        .await;
    assert!(
        resumed.response.error().is_none(),
        "{:?}",
        resumed.response.error()
    );
    let recovered = broker.snapshot().unwrap().active_plan.unwrap();
    assert_eq!(recovered.id, plan_id);
    assert_eq!(recovered.state, PlanState::AwaitingReview);
    assert_eq!(
        broker.task_controller.current().unwrap().unwrap().revision,
        paused.revision
    );
}

#[tokio::test]
async fn abort_plan_clears_review_and_question_waits_and_restores_approval_mode() {
    for structured in [false, true] {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), structured);
        broker.session.approval_mode = ApprovalMode::Ask;
        let planned = broker
            .dispatch(Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"/plan migrate"}),
            })
            .await;
        assert!(planned.response.error().is_none());
        run_plan_to_input_boundary(&mut broker).await.unwrap();
        let plan = broker.snapshot().unwrap().active_plan.unwrap();
        assert!(matches!(
            plan.state,
            PlanState::AwaitingInput | PlanState::AwaitingReview
        ));
        let cancelled = broker
            .dispatch(Request {
                id: 2,
                method: "session.plan.cancel".into(),
                params: json!({"plan_id":plan.id}),
            })
            .await;
        assert!(cancelled.response.error().is_none());
        let snapshot = broker.snapshot().unwrap();
        assert!(snapshot.active_plan.is_none());
        assert_eq!(broker.session.approval_mode, ApprovalMode::Ask);
        assert!(snapshot.active_elicitation.is_none());
        assert!(snapshot.timeline.iter().any(|entry| matches!(
            entry,
            TimelineEntry::Status {
                status: SessionPhase::Idle,
                ..
            }
        )));
        assert!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .iter()
                .all(|exchange| exchange.is_finished() && !exchange.awaiting_input())
        );
    }
}

#[tokio::test]
async fn cancel_terminates_a_failed_plan_without_reopening_input() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.goal_max_turns = 2;
    broker.provider = plan_fixture_provider(
        &broker,
        PlanFixtureBehavior::Retry {
            submit_from_turn: 2,
        },
    );
    broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan migrate the event format" }),
        })
        .await;
    run_plan_to_input_boundary(&mut broker)
        .await
        .expect_err("fixture must fail planning");

    assert_eq!(
        broker.snapshot().unwrap().active_plan.unwrap().state,
        PlanState::Failed
    );

    let cancelled = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan cancel" }),
        })
        .await;
    assert!(cancelled.response.error().is_none());
    assert!(broker.snapshot().unwrap().active_plan.is_none());
    assert!(broker.task_controller.current().unwrap().is_none());
}

#[tokio::test]
async fn identical_plan_writes_do_not_evade_the_no_progress_guard() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.provider = plan_fixture_provider(&broker, PlanFixtureBehavior::RepeatEdit);

    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan migrate the event format" }),
        })
        .await;
    run_plan_to_input_boundary(&mut broker)
        .await
        .expect_err("unchanged plan must exhaust its progress budget");

    assert!(result.response.error().is_none());
    let plan = broker.snapshot().unwrap().active_plan.unwrap();
    assert_eq!(plan.state, PlanState::Failed);
    assert_eq!(
        plan.generation.budget.turn_count,
        u32::from(plan.generation.budget.max_consecutive_no_progress) + 1
    );
    assert_eq!(plan.generation.canonical_revision, 1);
    assert_eq!(
        plan.generation.budget.consecutive_no_progress,
        plan.generation.budget.max_consecutive_no_progress
    );
}

#[tokio::test]
async fn explicit_chat_answer_resumes_an_ordinary_request() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = mutable_question_broker(repository.path(), data.path());
    broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "ask which migration to use" }),
        })
        .await;
    let result = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "use staged" }),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(broker.snapshot().unwrap().active_elicitation.is_none());
    assert!(
        result
            .event
            .iter()
            .any(|event| event.event == "question_answered")
    );
}

#[tokio::test]
async fn normalized_provider_failure_finalizes_the_interaction_timeline() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = invalid_control_output_fixture().unwrap();
    broker.provider = provider.provider_handle();
    broker.clock = Box::new(AdvancingClock(std::sync::atomic::AtomicI64::new(100)));

    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan migrate the event format" }),
        })
        .await;
    assert!(dispatch_pending_task_input(&mut broker).await.is_err());

    assert!(result.response.error().is_none());
    let interaction = broker
        .store
        .list_exchange(&broker.session.id)
        .unwrap()
        .into_iter()
        .last()
        .expect("failed planning interaction");
    assert_eq!(interaction.state(), ExchangeState::Failed);
    assert!(interaction.is_finished());
    assert!(interaction.duration_ms() > 0);
    assert!(
        interaction
            .turn
            .iter()
            .all(|turn| { !matches!(turn.state(), crate::turn::TurnState::Running) })
    );
}

#[tokio::test]
async fn planning_retries_start_separate_labeled_exchanges() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);

    let paused = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "/plan migrate the event format" }),
        })
        .await;
    run_plan_to_input_boundary(&mut broker).await.unwrap();

    assert!(paused.response.error().is_none());
    let plan = broker.snapshot().unwrap().active_plan.unwrap();
    assert_eq!(plan.state, PlanState::AwaitingReview);
    assert!(plan.elicitation.is_none());
    let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchanges.len(), 2);
    assert!(exchanges[0].prompt.contains("migrate the event format"));
    assert!(
        !exchanges[1].prompt.is_empty(),
        "internal continuation omitted its provider instruction"
    );
    assert!(matches!(
        &exchanges[1].origin,
        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
    ));
    assert_ne!(exchanges[0].id, exchanges[1].id);
    assert!(
        exchanges
            .iter()
            .all(|exchange| exchange.state() == ExchangeState::Complete
                && exchange.is_finished()
                && exchange.turn.len() == 1)
    );
    let restored: Vec<Exchange> =
        serde_json::from_value(serde_json::to_value(&exchanges).unwrap()).unwrap();
    assert!(matches!(
        &restored[1].origin,
        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
    ));
    assert_eq!(restored[0].turn.len(), 1);
    assert_eq!(restored[1].turn.len(), 1);
}

#[tokio::test]
async fn rejected_agent_admissions_preserve_names_without_orphaned_identity() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    for params in [
        json!({"task_name":"bad name","message":"Inspect tests"}),
        json!({"task_name":"worker","message":"Inspect tests","provider":"unknown"}),
        json!({"task_name":"worker","message":"Inspect tests","model":"unavailable-model"}),
    ] {
        assert!(broker.start_agent(params).await.is_err());
        assert!(broker.agent_registry.list().is_empty());
        assert!(broker.store.list_agent_task(&broker.session.id)?.is_empty());
        assert_eq!(broker.agent_coordinator.active_count(), 0);
    }
    let params = json!({"profile":"code-explorer","text":"Inspect tests"});
    let (admitted, _) = broker.start_agent(params.clone()).await?;
    assert_eq!(admitted["agent"]["task_name"], "code-explorer-1");
    settle_managed_children(&mut broker).await?;
    assert!(broker.start_agent(params).await.is_ok());
    settle_managed_children(&mut broker).await?;
    assert_eq!(broker.agent_registry.list().len(), 2);
    let task = broker.store.list_agent_task(&broker.session.id)?;
    assert_eq!(task.len(), 2);
    assert_eq!(task[0].state, ExchangeState::Complete);
    assert_eq!(broker.agent_coordinator.active_count(), 0);
    Ok(())
}
fn native_settlement_fixture() -> Result<crate::provider::test_support::ProviderTestSession> {
    let descriptor = crate::provider::ProviderDescriptor {
        kind: forge_providers::registry::nonselectable_id()
            .expect("registry supplies a nonselectable fixture identity"),
        label: "Normalized goal fixture".into(),
        capability: crate::provider::test_support::fixture_capability(),
    };
    let turns = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let fixture = crate::provider::test_support::EventFixtureProvider::new(
        descriptor.clone(),
        move |request, sink, _cancelled| {
            let turns = turns.clone();
            let descriptor = descriptor.clone();
            async move {
                let turn = turns.fetch_add(1, Ordering::SeqCst);
                if turn >= 2 {
                    crate::provider::test_support::call_tool(
                        &request,
                        "goal_complete",
                        json!({"summary":"The continued goal is complete."}),
                    )
                    .await?;
                }
                let scope = request.scope.clone();
                let address = crate::provider::TurnAddress {
                    scope: scope.clone(),
                    thread_id: "goal-fixture".into(),
                    turn_id: turn.to_string().into(),
                };
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope.clone(),
                        forge_provider_api::lifecycle::ExecutionUpdate::Started {
                            address: address.clone(),
                        },
                    )?,
                })
                .await?;
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Started {
                        started_at_ms: turn as i64,
                    },
                })
                .await?;
                for event in completed_message(
                    &address,
                    "goal-response",
                    crate::provider::MessageRole::Response,
                    "Continuing the goal.",
                ) {
                    sink.send_wait(event).await?;
                }
                sink.send_wait(ProviderEvent::Turn {
                    address: address.clone(),
                    update: crate::provider::TurnUpdate::Finished {
                        outcome: forge_provider_api::turn::TurnOutcome::Completed,
                        completed_at_ms: turn as i64 + 1,
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Execution {
                    observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                        scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                            address,
                            covered_submissions: vec![request.submission_id],
                            completed_at_ms: turn as i64 + 1,
                            result: forge_provider_api::lifecycle::ExecutionResult::Completed {
                                completion: forge_provider_api::completion::ProviderCompletion {
                                    capability: descriptor.capability.clone(),
                                    ..Default::default()
                                }
                                .into(),
                            },
                        },
                    )?,
                })
                .await?;
                Ok(())
            }
        },
    );

    crate::provider::test_support::ProviderTestSession::from_native(fixture)
}

#[tokio::test]
async fn goal_continuations_start_separate_labeled_exchanges() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let provider = native_settlement_fixture().unwrap();
    broker.provider = provider.provider_handle();
    broker
        .dispatch_session_command(
            "session.goal.set",
            json!({"objective":"complete a continued goal"}),
        )
        .unwrap();
    dispatch_pending_task_input(&mut broker).await.unwrap();
    broker
        .dispatch_session_command("session.command.apply", json!({"action":"continue"}))
        .unwrap();
    dispatch_pending_task_input(&mut broker).await.unwrap();
    broker
        .dispatch_session_command("session.command.apply", json!({"action":"continue"}))
        .unwrap();
    dispatch_pending_task_input(&mut broker).await.unwrap();
    let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(exchanges.len(), 3);
    assert!(exchanges.iter().all(
        |exchange| exchange.state() == ExchangeState::Complete && exchange.turn.len() == 1
    ));
    assert!(
        exchanges[1..]
            .iter()
            .all(|exchange| !exchange.prompt.is_empty()
                && matches!(
                    exchange.origin,
                    crate::exchange::ExchangeOrigin::TaskContinuation { .. }
                ))
    );
}

#[tokio::test]
async fn broker_snapshots_use_the_committed_task_owner() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    assert!(broker.session.active_task.is_none());
    let controller = broker.task_controller.clone();
    let installed = controller
        .change(
            &mut broker.store,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Current task".into(),
                Default::default(),
                1,
            ))),
            1,
        )
        .unwrap()
        .unwrap();
    assert_eq!(
        broker
            .snapshot()
            .unwrap()
            .session
            .active_task
            .unwrap()
            .revision,
        installed.revision
    );
    assert_eq!(
        broker
            .observer
            .snapshot()
            .unwrap()
            .session
            .active_task
            .unwrap()
            .revision,
        installed.revision
    );
    controller
        .change(
            &mut broker.store,
            Some(&installed.revision),
            crate::task::TaskChange::Clear,
            2,
        )
        .unwrap();
    assert!(broker.snapshot().unwrap().session.active_task.is_none());
    assert!(
        broker
            .observer
            .snapshot()
            .unwrap()
            .session
            .active_task
            .is_none()
    );
}

#[tokio::test]
async fn pauses_an_active_goal_after_a_provider_failure() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "failure-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(200)),
    )
    .unwrap();
    let provider = failing_provider_fixture(repository.path().to_owned()).unwrap();
    broker.provider = provider.provider_handle();
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.goal.set".into(),
            params: json!({ "objective": "finish the work" }),
        })
        .await;
    assert!(result.response.error().is_none());
    assert!(dispatch_pending_task_input(&mut broker).await.is_err());
    assert_eq!(broker.active_goal().unwrap().state, GoalState::Paused);
    let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0].state(), ExchangeState::Failed);
    assert!(interaction[0].checkpoint_after.is_some());
    assert!(
        interaction[0]
            .checkpoint_diff_text
            .as_deref()
            .is_some_and(|diff| diff.contains("failed/deep/write.txt"))
    );
}

#[tokio::test]
async fn stores_provider_tasks_for_an_ordinary_chat_interaction() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "task-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(250)),
    )
    .unwrap();
    broker.provider = task_update_provider(&broker);
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "answer an ordinary question" }),
        })
        .await;
    assert!(result.response.error().is_none());
    let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
    let checklist = interaction[0]
        .checklist
        .as_ref()
        .expect("ordinary checklist snapshot");
    assert_eq!(checklist.current[0].title, "Inspect ordinary chat");
    assert!(!matches!(
        &interaction[0].origin,
        crate::exchange::ExchangeOrigin::TaskContinuation { .. }
    ));
}

#[tokio::test]
async fn removes_transient_wait_without_persisting_a_timeline_node() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "parent-boundary-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(500)),
    )
    .unwrap();
    let provider = parent_boundary_fixture().unwrap();
    broker.provider = provider.provider_handle();

    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "wait for the explorer" }),
        })
        .await;
    assert!(result.response.error().is_none());
    let wait_update_list = result
        .event
        .iter()
        .filter(|event| {
            event.event == "provider_event"
                && event.payload.get("kind").and_then(Value::as_str)
                    == Some("timeline_wait_updated")
        })
        .collect::<Vec<_>>();
    assert_eq!(wait_update_list.len(), 2);
    assert!(
        wait_update_list[0]
            .payload
            .pointer("/data/wait")
            .is_some_and(Value::is_object)
    );
    assert!(
        wait_update_list[1]
            .payload
            .pointer("/data/wait")
            .is_some_and(Value::is_null)
    );

    let snapshot = broker.snapshot().unwrap();
    assert!(snapshot.active_wait.is_none());
    assert!(
        snapshot.exchange[0]
            .node_list
            .iter()
            .all(|node| !node.id().ends_with(":wait"))
    );
    let message_list = snapshot.exchange[0].turn[0].messages();
    assert_eq!(message_list.len(), 2);
    assert_eq!(message_list[0].text(), "An intermediate answer.");
    assert_eq!(message_list[1].text(), "A final synthesis.");
}

#[tokio::test]
async fn non_git_exchanges_save_reported_changes_without_initializing_repository() {
    let workspace = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: workspace.path().to_string_lossy().into_owned(),
            client_id: "reported-edits-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(300)),
    )
    .unwrap();
    broker.provider = nested_write_provider(&broker, Path::new(&broker.session.workspace));
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"create a module"}),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    let exchanges = broker.store.list_exchange(&broker.session.id).unwrap();
    let exchange = &exchanges[0];
    assert_eq!(exchange.state(), ExchangeState::Complete);
    assert!(exchange.checkpoint_before.is_none() && exchange.checkpoint_after.is_none());
    assert!(exchange.checkpoint_diff_text.is_none());
    assert!(!exchange.attributed_matches_checkpoint);
    let diff = exchange
        .attributed_diff_text
        .as_deref()
        .expect("persisted reported edits");
    assert!(diff.contains("seed.txt") && diff.contains("+provider edit"));
    assert!(
        !diff.contains("apps/new/deep/module/lib.rs"),
        "command edits have no provider provenance"
    );
    assert!(!workspace.path().join(".git").exists());
}

#[tokio::test]
async fn provider_and_command_changes_use_separate_diff_sources() {
    let repository = repository();
    std::fs::write(repository.path().join(".gitignore"), "target/\n").unwrap();
    git(repository.path(), &["add", ".gitignore"]);
    git(repository.path(), &["commit", "-qm", "ignore build output"]);
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "interaction-diff-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(300)),
    )
    .unwrap();
    broker.provider = nested_write_provider(&broker, Path::new(&broker.session.workspace));
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "create a nested module" }),
        })
        .await;
    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(interaction.len(), 1);
    assert_eq!(interaction[0].turn.len(), 1);
    assert_eq!(
        interaction[0].turn[0].messages()[0].text(),
        "Creating the nested module."
    );
    assert_eq!(
        interaction[0]
            .tools_for_turn(interaction[0].turn[0].id())
            .count(),
        2
    );
    assert!(
        interaction[0]
            .checkpoint_diff_text
            .as_deref()
            .is_some_and(|diff| diff.contains("apps/new/deep/module/lib.rs"))
    );
    let attributed_diff = interaction[0]
        .attributed_diff_text
        .as_deref()
        .expect("attributed interaction diff");
    assert!(attributed_diff.contains("seed.txt"));
    assert!(!attributed_diff.contains("apps/new/deep/module/lib.rs"));
    assert!(!interaction[0].attributed_matches_checkpoint);
    let interaction_diff = interaction[0].checkpoint_diff_text.as_deref().unwrap();
    assert!(interaction_diff.contains("seed.txt"));
    assert!(!interaction_diff.contains("target/generated/deep/artifact.txt"));
    assert!(
        interaction[0].turn[0]
            .messages()
            .iter()
            .any(|message| message.text() == "The nested module is ready.")
    );
}

#[tokio::test]
async fn matching_provider_and_checkpoint_changes_share_one_canonical_diff() {
    let repository = repository();
    std::fs::write(repository.path().join(".gitignore"), "apps/\ntarget/\n").unwrap();
    git(repository.path(), &["add", ".gitignore"]);
    git(
        repository.path(),
        &["commit", "-qm", "ignore generated output"],
    );
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "matching-interaction-diff-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(300)),
    )
    .unwrap();
    broker.provider = nested_write_provider(&broker, Path::new(&broker.session.workspace));

    let occupied: Vec<_> = (0..4)
        .map(|_| broker.repositories.reads.submit(0, |_| Ok(())).unwrap())
        .collect();
    let (mut rejected, _, _) = broker
        .test_stage_interaction_for_turn("not started", true, 300)
        .await
        .unwrap();
    broker.begin_exchange_runtime(&mut rejected, 300).unwrap();
    let mut runtime = broker.exchange_runtime.take().unwrap();
    let captured = (&mut runtime.checkpoint.as_mut().unwrap().0).await.unwrap();
    let error = broker
        .finish_initial_checkpoint(
            &mut runtime,
            &mut rejected,
            captured.map_err(anyhow::Error::new),
        )
        .unwrap_err();
    assert!(error.to_string().contains("capacity is full"));
    assert!(runtime.tool_gate.as_ref().unwrap().wait().await.is_err());
    assert!(rejected.checkpoint_before.is_none());
    assert!(broker.exchange_runtime.is_none());
    assert!(
        broker
            .store
            .list_exchange(&broker.session.id)
            .unwrap()
            .is_empty()
    );
    for task in occupied {
        task.finish().await.unwrap();
    }

    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({ "text": "change the seed" }),
        })
        .await;

    assert!(
        result.response.error().is_none(),
        "{:?}",
        result.response.error()
    );
    let interaction = broker.store.list_exchange(&broker.session.id).unwrap();
    assert_eq!(interaction.len(), 1);
    assert!(interaction[0].attributed_matches_checkpoint);
    assert_eq!(broker.diff.usage().cache.cached_entries, 1);
    assert_eq!(
        interaction[0].attributed_diff_text,
        interaction[0].checkpoint_diff_text
    );
}

#[tokio::test]
async fn provider_change_index_includes_referenced_child_turns() {
    let repository = tempfile::tempdir().unwrap();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "child-attribution-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(300)),
    )
    .unwrap();
    let child_run_id = "child-run";
    let child_exchange = |id: &str, path: &str, ordinal: u64| {
        let mut exchange = completed_interaction(id, &broker.session.id, Vec::new());
        exchange.ordinal = ordinal;
        let mut provider = crate::turn::test_support::address(&exchange, &format!("{id}:provider"));
        provider.thread_id = child_run_id.into();
        let mut turn =
            crate::turn::test_support::TestTurn::new(format!("{id}:turn:1"), provider, 1);
        let call_id = format!("{id}:change");
        for update in [
            crate::provider::ToolUpdate::Requested {
                invocation: crate::provider::ToolInvocation {
                    semantics: Default::default(),
                    kind: crate::provider::ToolActivityKind::FileChange,
                    name: "edit".into(),
                    title: "file changes".into(),
                    arguments: Value::Null,
                },
            },
            crate::provider::ToolUpdate::Changes {
                changes: crate::provider::ProviderChangeSet {
                    file: vec![crate::provider::ProviderFileChange {
                        path: path.into(),
                        move_path: None,
                        kind: crate::provider::ProviderChangeKind::Add,
                        diff: "child".into(),
                    }],
                },
            },
            crate::provider::ToolUpdate::Finished {
                outcome: crate::provider::ToolOutcome::Succeeded,
            },
        ] {
            turn.record_tool(&call_id, update, 1).unwrap();
        }
        turn.finish(crate::turn::TurnOutcome::Completed, 2).unwrap();
        let owned = turn.into_exchange();
        exchange.turn = owned.turn;
        exchange.node_list = owned.node_list;
        exchange.entry = owned.entry;
        exchange.rebuild_tool_counter();
        exchange
    };
    let mut child_interaction = child_exchange("child-interaction", "child.rs", 1);
    let mut unrelated = child_exchange("unrelated-child-exchange", "unrelated.rs", 2);
    let mut child_run = Agent::pending(&broker.session.id, "explorer", "edit child", 1);
    child_run.id = child_run_id.into();
    broker.store.save_agent_run(&child_run).unwrap();
    unrelated.agent_id = child_run_id.into();
    broker.store.save_exchange(&unrelated).unwrap();

    child_interaction.agent_id = child_run_id.into();
    broker.store.save_exchange(&child_interaction).unwrap();
    let mut parent_interaction = completed_interaction(
        "parent-interaction",
        &broker.session.id,
        vec![ExchangeNode::AgentReference {
            agent: crate::agent::Delegation {
                id: "child-reference".into(),
                parent_exchange_id: "parent-interaction".into(),
                parent_turn_id: None,
                task: "inspect".into(),
                child_agent_id: child_run_id.into(),
                child_exchange_id: "child-interaction".into(),
                created_at_ms: 1,
            },
        }],
    );

    let index = broker
        .interaction_provider_change_index(&parent_interaction)
        .unwrap();

    assert_eq!(
        index.paths().iter().map(String::as_str).collect::<Vec<_>>(),
        vec!["child.rs"]
    );
    parent_interaction.phase = crate::exchange::ExchangePhase::fixture(
        ExchangeState::Running,
        parent_interaction.completed_at_ms(),
        parent_interaction.finalization_outcome(),
    );
    parent_interaction.phase = crate::exchange::ExchangePhase::fixture(
        parent_interaction.state(),
        None,
        parent_interaction.finalization_outcome(),
    );
    broker
        .capture_final_checkpoint(
            &mut parent_interaction,
            crate::exchange::ExchangeOutcome::Complete,
        )
        .await
        .unwrap();
    let diff = parent_interaction
        .attributed_diff_text
        .as_deref()
        .expect("reported child edits");
    assert!(diff.contains("child.rs") && !diff.contains("unrelated.rs"));
}

#[tokio::test]
async fn rejects_resuming_a_session_owned_by_another_provider() {
    let repository = repository();
    let data = tempfile::tempdir().unwrap();
    let mut broker = HarnessBroker::initialize_with_clock(
        InitializeRequest {
            background: None,
            provider_launch_by_provider: Default::default(),
            config_dir: None,
            data_root: data.path().to_string_lossy().into_owned(),
            permission_file: None,
            workspace: repository.path().to_string_lossy().into_owned(),
            client_id: "provider-test".into(),
            provider: ProviderLaunch {
                kind: forge_providers::registry::nonselectable_id()
                    .expect("registry supplies a nonselectable fixture identity"),
                command: vec!["mock".into()],
            },
            model: "mock-model".into(),
            effort: "low".into(),
            session_id: None,
            new_session_name: None,
            goal_max_turns: 20,
            lease_conflict_action: None,
        },
        Box::new(FixedClock(300)),
    )
    .unwrap();
    let active_id = broker.session.id.clone();
    let mut incompatible = broker.session.clone();
    incompatible.id = "different-provider-session".into();
    incompatible.primary_agent_id = HarnessSession::primary_agent_id(&incompatible.id);
    incompatible.provider = forge_providers::registry::alternate_id(
        &forge_providers::registry::select(&broker.session.provider).unwrap(),
    )
    .expect("alternate registered provider")
    .to_string();
    incompatible.lease_owner = None;
    broker.store.save_session(&incompatible).unwrap();
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.resume".into(),
            params: json!({ "session_id": incompatible.id }),
        })
        .await;
    assert!(result.response.error().is_some());
    assert_eq!(broker.session.id, active_id);
}
