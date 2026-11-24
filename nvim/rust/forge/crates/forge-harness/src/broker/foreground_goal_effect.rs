use super::*;

/// Explain a completion refused because another owner held the workspace when the turn ended.
pub(crate) const GOAL_WORKSPACE_BUSY: &str = "another operation, such as a check run this turn started, held the workspace when the turn ended, so Harness could not verify the source; review the check results, then request completion again";

/// Source evidence for one goal turn's progress and completion decision.
pub(crate) enum GoalWorkspace {
    /// Protected from source writers until the goal's state transition commits, so a completion
    /// request can be verified against exactly this source.
    Reserved(execution::ReservedWorkspace),
    /// Captured while another owner held the workspace, such as a check run the turn started and
    /// left collecting. It measures progress but cannot certify completion.
    Unprotected {
        checkpoint: crate::checkpoint::CheckpointRecord,
    },
}

impl GoalWorkspace {
    /// Capture protected evidence, or unprotected progress evidence while another owner is active.
    ///
    /// A busy workspace is an ordinary state at the end of a goal turn: the model may end its turn
    /// while its own check run still holds the reservation. Failing the turn for it would discard
    /// the check result that is about to start the next turn.
    pub(crate) async fn capture(
        reserved: execution::WorkspaceCaptureJob,
        unprotected: execution::WorkspaceCaptureJob,
    ) -> Result<Self> {
        match reserved.run_reserved().await {
            Ok(workspace) => Ok(Self::Reserved(workspace)),
            Err(error)
                if error
                    .downcast_ref::<crate::repository_access::RepositoryAccessError>()
                    .is_some_and(crate::repository_access::RepositoryAccessError::is_busy) =>
            {
                Ok(Self::Unprotected {
                    checkpoint: unprotected.run().await?,
                })
            }
            Err(error) => Err(error),
        }
    }

    /// Get the captured source snapshot.
    pub(crate) fn checkpoint(&self) -> &crate::checkpoint::CheckpointRecord {
        match self {
            Self::Reserved(workspace) => workspace.checkpoint(),
            Self::Unprotected { checkpoint } => checkpoint,
        }
    }
}

/// Capture workspace evidence while retaining the exact host-effect reservation.
pub(crate) struct GoalProgressJob {
    pub(super) capture: execution::WorkspaceCaptureJob,
    /// Capture used only when another owner holds the workspace reservation.
    pub(super) unprotected_capture: execution::WorkspaceCaptureJob,
    pub(crate) previous: Option<crate::checkpoint::CheckpointRecord>,
    pub(crate) previous_id: Option<String>,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

pub(crate) struct GoalProgressResult {
    pub(crate) workspace: GoalWorkspace,
    pub(crate) changed: bool,
    pub(crate) previous_id: Option<String>,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

impl GoalProgressJob {
    pub(crate) async fn run(self) -> Result<GoalProgressResult> {
        let Self {
            capture,
            unprotected_capture,
            previous,
            previous_id,
            permit,
            guard,
        } = self;
        let workspace = GoalWorkspace::capture(capture, unprotected_capture).await?;
        let changed = previous
            .as_ref()
            .map(|before| before.changed_paths(workspace.checkpoint()))
            .transpose()?
            .is_some_and(|paths| !paths.is_empty());
        Ok(GoalProgressResult {
            workspace,
            changed,
            previous_id,
            permit,
            guard,
        })
    }
}

/// Retain an Execute-plan evidence reservation during an unlocked progress scan.
pub(crate) struct ExecutionProgressJob {
    pub(crate) progress: execution_progress::PlanProgressJob,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

pub(crate) struct ExecutionProgressResult {
    pub(crate) progress: execution_progress::PlanProgressResult,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

impl ExecutionProgressJob {
    pub(crate) async fn run(self) -> Result<ExecutionProgressResult> {
        let Self {
            progress,
            permit,
            guard,
        } = self;
        let progress = progress.run().await?;
        Ok(ExecutionProgressResult {
            progress,
            permit,
            guard,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;

    /// Script goal turns: the first requests completion, and every turn answers with prose.
    fn goal_fixture_provider(
        broker: &HarnessBroker,
        turn_count: Arc<AtomicUsize>,
    ) -> Arc<Provider> {
        let descriptor = forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
            label: "Goal completion fixture".into(),
            capability: Default::default(),
        };
        let native = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, sink, start, _cancelled| {
                let turn_count = turn_count.clone();
                async move {
                    start.observed(0).await?;
                    let index = turn_count.fetch_add(1, Ordering::SeqCst);
                    if index == 0 {
                        crate::provider::test_support::call_tool(
                            &request,
                            "goal_complete",
                            json!({"summary":"Every requested change is in place"}),
                        )
                        .await?;
                    }
                    let scope = request.scope.clone();
                    let address = crate::provider::TurnAddress {
                        thread_id: scope.owner().thread_id(),
                        turn_id: scope.turn_id().unwrap(),
                        scope: scope.clone(),
                    };
                    for event in crate::provider::test_support::message(
                        &address,
                        format!("{}:goal-response-{index}", address.turn_id.as_str()),
                        Some(crate::provider::TestMessageContent::Response),
                        format!("Goal turn {index} finished."),
                    ) {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                    }
                    Ok::<_, forge_provider_api::ProviderError>(
                        forge_provider_api::TurnResult::new(
                            forge_provider_api::turn::TurnOutcome::Completed {
                                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                            },
                            index as i64 + 1,
                            None,
                            request.input_coverage(),
                            Vec::new(),
                            None,
                        )
                        .expect("fixture coverage is valid"),
                    )
                }
            },
        );
        Arc::new(Provider::new(
            native.registration(),
            broker.permission_coordinator.clone(),
            broker.trace.clone(),
            broker.data_root.clone(),
        ))
    }

    /// Deliver the active task's pending entry or continuation as the transport would.
    async fn dispatch_task_input(broker: &mut HarnessBroker) -> Result<(Value, Vec<SessionEvent>)> {
        let task = broker
            .task_controller
            .current()?
            .context("task is missing")?;
        let (instruction, purpose) = if let Some(entry) = task
            .entry
            .as_ref()
            .filter(|entry| entry.delivery == crate::task::TaskInputDelivery::Pending)
        {
            broker.task_controller.acknowledge_entry(
                &mut broker.store,
                &task.revision,
                &entry.id,
                broker.clock.now_ms(),
            )?;
            (
                entry.instruction.clone(),
                crate::input::InputPurpose::TaskEntry {
                    revision: task.revision.clone(),
                    id: entry.id.clone(),
                    start: Some(crate::exchange::TaskStart {
                        kind: task.kind(),
                        title: task.title().to_owned(),
                    }),
                },
            )
        } else {
            let turn = task.turn.as_ref().context("task turn is missing")?;
            let continuation = turn
                .continuation
                .as_ref()
                .context("task continuation is missing")?;
            broker.task_controller.acknowledge_continuation(
                &mut broker.store,
                &task.revision,
                &continuation.id,
                broker.clock.now_ms(),
            )?;
            (
                continuation
                    .work
                    .instruction()
                    .context("continuation has no provider input")?
                    .clone(),
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
        let result = broker
            .submit_prompt(json!({"text": instruction.label()}))
            .await;
        broker.input_purpose = crate::input::InputPurpose::User;
        result
    }

    /// Reopen a session that recorded a pending format requirement under the previous policy.
    fn reopen_with_legacy_format_requirement(broker: &mut HarnessBroker) -> Result<()> {
        let workspace = PathBuf::from(&broker.session.workspace);
        let root = crate::storage::state::directory(&broker.data_root)
            .join(&broker.session.id)
            .join("checks");
        let definition = crate::checks::CheckDefinition {
            auto: true,
            id: "format".into(),
            name: "Formatting".into(),
            category: crate::checks::CheckCategory::Format,
            enabled: true,
            area: "Project".into(),
            trigger: crate::checks::CheckTrigger::AnyChange,
            scope: crate::checks::CheckScope::All,
            directory: ".".into(),
            runner: crate::checks::CheckRunner::Command(crate::checks::CheckOperation {
                prepare: true,
                args: None,
                inputs: None,
                command: "unused".into(),
            }),
            depends_on: Vec::new(),
        };
        let ledger = json!({
            "version": crate::storage::state::VERSION,
            "session_id": broker.session.id,
            "workspace": workspace.canonicalize()?,
            "generation": 1,
            "requirements": {
                "format-fix": {
                    "generation": 1,
                    "task_id": null,
                    "definition": definition,
                    "helper_digest": {},
                    "requested": crate::checks::CheckSelection::All,
                    "state": {"state": "prepared"},
                }
            }
        });
        std::fs::create_dir_all(root.join("verification"))?;
        std::fs::write(
            root.join("verification").join("requirements.json"),
            serde_json::to_vec(&ledger)?,
        )?;
        broker.check_runner = crate::checks::CheckSupervisor::open(
            crate::checks::CheckSupervisorContext {
                session_id: broker.session.id.clone(),
                data_root: broker.data_root.clone(),
                workspace,
                root,
            },
            Arc::clone(&broker.presentation),
            broker.store.objects.clone(),
            Arc::clone(&broker.repositories),
            broker.trace.clone(),
        )?;
        Ok(())
    }

    /// A saved formatter mutation cannot add a check to goal completion.
    #[tokio::test]
    async fn goal_completion_ignores_previous_format_mutation() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let turn_count = Arc::new(AtomicUsize::new(0));
        broker.provider = goal_fixture_provider(&broker, turn_count.clone());
        let admitted = broker
            .dispatch(Request {
                id: 1,
                method: "session.goal.set".into(),
                params: json!({"objective":"Change the project and verify it"}),
            })
            .await;
        assert!(admitted.response.error().is_none());
        reopen_with_legacy_format_requirement(&mut broker)?;
        dispatch_task_input(&mut broker).await?;
        assert!(broker.host_settlement().is_none());
        assert!(broker.task_controller.current()?.is_none());
        assert_eq!(turn_count.load(Ordering::SeqCst), 1);
        assert!(broker.check_runner.history()?.is_empty());
        Ok(())
    }

    /// A foreground that fails while applying goal evidence releases its settlement fence.
    #[tokio::test]
    async fn failed_goal_evidence_commit_releases_host_settlement() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.provider = goal_fixture_provider(&broker, Arc::new(AtomicUsize::new(1)));
        let database = crate::storage::test::raw_connection(data.path())?;
        // Refuse the turn's goal observation after evidence application has begun.
        database.execute_batch(
            "CREATE TRIGGER refuse_goal_observation BEFORE UPDATE ON session_record
             WHEN instr(NEW.payload,'\"observed_turn\":{')>0
             BEGIN SELECT RAISE(ABORT,'goal observation refused'); END;",
        )?;
        let admitted = broker
            .dispatch(Request {
                id: 1,
                method: "session.goal.set".into(),
                params: json!({"objective":"Fail while recording progress"}),
            })
            .await;
        assert!(admitted.response.error().is_none());
        let error = dispatch_task_input(&mut broker)
            .await
            .expect_err("the refused observation did not fail the turn");
        assert!(
            format!("{error:#}").contains("goal observation refused"),
            "{error:#}"
        );
        database.execute_batch("DROP TRIGGER refuse_goal_observation;")?;
        // The unsettled effect is reported, and the fence that would refuse every later
        // foreground with "prior host settlement is still pending" is gone.
        assert!(
            format!("{error:#}").contains("failed foreground left a host effect unsettled"),
            "{error:#}"
        );
        assert!(
            broker.host_settlement().is_none(),
            "the failed foreground kept its host settlement"
        );
        assert_eq!(broker.active_goal()?.state, GoalState::Paused);
        Ok(())
    }

    /// Completion on unprotected evidence is refused, and the goal continues.
    #[tokio::test]
    async fn completion_on_a_busy_workspace_is_refused_without_failing() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.dispatch_session_command(
            "session.goal.set",
            json!({"objective":"Complete while a check run collects"}),
        )?;
        let task = broker
            .task_controller
            .current()?
            .context("goal task missing")?;
        let entry = task.entry.as_ref().context("goal entry missing")?;
        broker.task_controller.acknowledge_entry(
            &mut broker.store,
            &task.revision,
            &entry.id,
            1,
        )?;
        let task = broker
            .task_controller
            .current()?
            .context("goal task missing")?;
        let evidence = GoalWorkspace::Unprotected {
            checkpoint: broker.prepare_workspace_capture().run().await?,
        };
        let continuing = broker.apply_goal_evidence_strict(
            crate::goal::TurnEvidence {
                structured_complete: true,
                ..Default::default()
            },
            Some(task.permit()),
            crate::task::TaskTurnId {
                exchange_ordinal: 1,
                turn_ordinal: 1,
            },
            None,
            Some((&evidence, None)),
            &mut Vec::new(),
        )?;
        assert!(continuing, "a refused completion ended the goal");
        assert_eq!(broker.active_goal()?.state, GoalState::Active);
        Ok(())
    }

    /// A busy workspace leaves progress evidence unprotected instead of failing the turn.
    #[tokio::test]
    async fn busy_workspace_capture_falls_back_to_unprotected_progress() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let check = crate::checks::CheckReservation::acquire(
            data.path(),
            workspace.path(),
            crate::checks::CheckPurpose::Prepare,
        )?;
        let evidence = GoalWorkspace::capture(
            broker.prepare_workspace_capture(),
            broker.prepare_workspace_capture(),
        )
        .await?;
        assert!(matches!(evidence, GoalWorkspace::Unprotected { .. }));
        drop(check);
        let evidence = GoalWorkspace::capture(
            broker.prepare_workspace_capture(),
            broker.prepare_workspace_capture(),
        )
        .await?;
        assert!(matches!(evidence, GoalWorkspace::Reserved(_)));
        Ok(())
    }
}
