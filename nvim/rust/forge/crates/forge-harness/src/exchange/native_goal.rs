//! Synchronize native goals as retained owner effects, independently of caller commands.

use std::sync::Arc;

use tokio::{
    sync::{OwnedMutexGuard, OwnedSemaphorePermit, oneshot},
    task::JoinHandle,
    time::Instant,
};

use super::services::{OwnerServices, ProviderGenerationPermit};
use crate::session::SessionStore;
use crate::{
    broker::HarnessBroker,
    provider::{
        NativeGoalUpdate, ProviderError, ProviderRequest, session_execution::SessionExecution,
    },
    trace::TraceStore,
};

/// Retain the exact task, native conversation, and transport epoch acknowledged by the owner.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct NativeGoalSelection {
    /// Native conversation observed in the committed session record.
    provider_session_id: forge_provider_api::ProviderSessionId,
    /// Task authority selected before the write and revalidated after its completion.
    revision: Option<crate::task::TaskRevision>,
    /// Reject cached acknowledgments invalidated by any other transport writer.
    transport_epoch: u64,
    /// Provider-neutral task state, available to existing integration fixtures.
    pub(crate) update: NativeGoalUpdate,
}

/// Distinguish failed observation, local state, and uncertain native writes without native text.
#[derive(Clone, Debug, thiserror::Error)]
pub(crate) enum NativeGoalError {
    /// Intake closed before this internal observation was admitted.
    #[error("native goal synchronization owner is closed")]
    OwnerClosed,
    /// The observer disappeared independently of the retained synchronization work.
    #[error("native goal synchronization result was not observed")]
    ObservationLost,
    /// Bounded internal observation capacity was exhausted without admitting this waiter.
    #[error("native goal synchronization observer capacity is full")]
    QueueFull,
    /// Committed task, session, or question authority could not be read or certified.
    #[error("native goal synchronization state is unavailable")]
    StateUnavailable,
    /// Observation expired while the actual local or native work remained owned.
    #[error("native goal synchronization exceeded its observation deadline")]
    Deadline,
    /// A retained job terminated without delivering its typed result.
    #[error("native goal synchronization worker observation was lost")]
    WorkerLost,
    /// The captured provider generation no longer matches the current selection.
    #[error("provider generation changed during native goal synchronization")]
    GenerationChanged,
    /// Native effects cannot be certified until the retained generation is reconciled.
    #[error("native goal delivery is uncertain; reconcile the retained generation before retrying")]
    Uncertain,
    /// A normalized provider operation failed without exposing its native protocol details.
    #[error("native goal synchronization failed: {0}")]
    Provider(#[source] ProviderError),
}

impl NativeGoalError {
    /// Get the stable host error identity for status and correlated observation logs.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::OwnerClosed => "native_goal_owner_closed",
            Self::ObservationLost => "native_goal_observation_lost",
            Self::QueueFull => "native_goal_observer_capacity",
            Self::StateUnavailable => "native_goal_state_unavailable",
            Self::Deadline => "native_goal_timeout",
            Self::WorkerLost => "native_goal_worker_lost",
            Self::GenerationChanged => "native_goal_generation_changed",
            Self::Uncertain => "native_goal_delivery_uncertain",
            Self::Provider(_) => "native_goal_provider_failed",
        }
    }
}

/// Observe one synchronization pass without making its lifetime depend on the caller.
pub(super) type NativeGoalReply =
    oneshot::Sender<Result<Option<NativeGoalSelection>, NativeGoalError>>;

/// Own the canonical acknowledgment cache, bounded observers, and the actual outstanding work.
#[derive(Default)]
pub(super) struct NativeGoalSync {
    applied: Option<AcknowledgedGoal>,
    pending: Vec<(uuid::Uuid, NativeGoalReply)>,
    run: Option<NativeGoalRun>,
}

impl NativeGoalSync {
    /// Attach an internal readiness observer; observations may share one canonical synchronization pass.
    pub(super) fn request(
        &mut self,
        reply: NativeGoalReply,
        available: Result<(), NativeGoalError>,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) -> uuid::Uuid {
        let id = uuid::Uuid::new_v4();
        trace.record(
            session,
            "exchange.event.read",
            serde_json::json!({"source":"native_goal_request","sync_id":id,"pass_id":self.run.as_ref().map(|run|run.id),"update_step":step}),
        );
        if let Err(error) = available {
            respond(id, reply, Err(error), trace, session, step);
        } else if let Some(error) = self.run.as_ref().and_then(|run| run.failed.clone()) {
            respond(id, reply, Err(error), trace, session, step);
        } else if self.pending.len() >= forge_protocol::MAX_PENDING_FRAMES {
            respond(
                id,
                reply,
                Err(NativeGoalError::QueueFull),
                trace,
                session,
                step,
            );
        } else {
            self.pending.push((id, reply));
        }
        id
    }

    /// Keep new native effects behind finite preparation while local input commands remain usable.
    pub(super) fn preparing(&self) -> bool {
        self.run.as_ref().is_some_and(|run| run.failed.is_none())
    }

    /// Get whether shutdown still has observers or real work to drain.
    pub(super) fn is_idle(&self) -> bool {
        self.pending.is_empty() && self.run.is_none()
    }

    /// Start one internal pass after earlier commands settle or the active command requests this effect.
    pub(super) fn start(
        &mut self,
        services: &OwnerServices,
        broker: &mut HarnessBroker,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) {
        if self.run.is_some() || self.pending.is_empty() {
            return;
        }
        if services.tasks.native_goal_is_uncertain() {
            self.finish(Err(NativeGoalError::Uncertain), trace, session, step);
            return;
        }
        match services.observer.native_goal_supported() {
            Ok(false) => {
                self.finish(Ok(None), trace, session, step);
                return;
            }
            Err(_) => {
                self.finish(Err(NativeGoalError::StateUnavailable), trace, session, step);
                return;
            }
            Ok(true) => {}
        }
        let id = self.pending[0].0;
        for (observer, _) in &self.pending {
            trace.record(
                session,
                "exchange.native_goal.observer_attached",
                serde_json::json!({"sync_id":observer,"pass_id":id,"update_step":step}),
            );
        }
        let registry = broker.execution_registry();
        let owner = crate::provider::ExecutionOwner::root(session);
        let acquisition = services.clone();
        let task = tokio::spawn(async move {
            // Preserve control-before-generation ordering while a provider retirement may be queued.
            let control = acquisition
                .control
                .clone()
                .acquire_owned()
                .await
                .map_err(|_| NativeGoalError::OwnerClosed)?;
            let generation = acquisition.observe_provider_generation().await;
            if !registry.matches(generation.provider()) {
                return Err(NativeGoalError::GenerationChanged);
            }
            let engine = registry
                .owner(owner)
                .await
                .map_err(NativeGoalError::Provider)?;
            Ok(AcquiredResources {
                _control: control,
                generation,
                engine,
            })
        });
        self.run = Some(NativeGoalRun {
            id,
            observation: GoalObservation::Waiting,
            stage: Stage::Acquiring {
                task,
                deadline: Some(deadline()),
            },
            fence: None,
            failed: None,
            issued: None,
            expires: Instant::now() + std::time::Duration::from_secs(60),
        });
        trace.record(
            session,
            "exchange.native_goal.started",
            serde_json::json!({"sync_id":id,"update_step":step}),
        );
    }

    /// Observe jobs by mutable handle so cancelling this select never detaches their work.
    pub(super) async fn next(&mut self) -> NativeGoalEvent {
        let Some(run) = self.run.as_mut() else {
            return std::future::pending().await;
        };
        if matches!(run.observation, GoalObservation::Pending) {
            return std::future::pending().await;
        }
        let event = tokio::select! {
            event = run.stage.next() => event,
            result = async { match run.fence.as_mut() { Some(task) => task.await, None => std::future::pending().await } } => NativeGoalEvent::Fenced(result),
        };
        // The owner may buffer this event across another ready-event phase.
        // Its JoinHandle output has already been consumed and must not be polled again.
        run.observation = GoalObservation::Pending;
        event
    }

    /// Commit all state decisions in an owner update step, retaining effects after observation failure.
    pub(super) fn update(
        &mut self,
        event: NativeGoalEvent,
        certified: Result<(), NativeGoalError>,
        services: &OwnerServices,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) {
        let mut run = self
            .run
            .take()
            .expect("native goal event has a retained pass");
        run.observation = GoalObservation::Waiting;
        let source_event = event.name();
        trace.record(session, "exchange.event.read", serde_json::json!({"source":"native_goal","sync_id":run.id,"event":event.name(),"update_step":step}));
        if let Err(error) = certified {
            run.fail(error.clone(), services);
            self.applied = None;
            self.finish(Err(error), trace, session, step);
        }
        let progress = run.update(event, &mut self.applied, services);
        if let Err(error) = progress {
            trace.record(session, "exchange.native_goal.effect_failed", serde_json::json!({"sync_id":run.id,"source_event":source_event,"error_code":error.code(),"reason":error.to_string(),"primary":run.failed.is_none(),"update_step":step}));
            run.fail(error.clone(), services);
            self.applied = None;
            self.finish(Err(error), trace, session, step);
        }
        if let Some(error) = &run.failed {
            trace.record(
                session,
                "exchange.native_goal.retained",
                serde_json::json!({"sync_id":run.id,"code":error.code(),"update_step":step}),
            );
        }
        trace.record(session, "exchange.event.applied", serde_json::json!({"source":"native_goal","sync_id":run.id,"stage":run.stage.name(),"fence_pending":run.fence.is_some(),"error_code":run.failed.as_ref().map(NativeGoalError::code),"update_step":step}));
        if matches!(run.stage, Stage::Drained { .. }) && run.fence.is_none() {
            let completed = run.failed.is_none();
            let wrote = run.issued.is_some();
            // Release local admission before waking readiness; cached no-op passes must not self-notify forever.
            drop(run);
            if completed {
                self.finish(
                    Ok(self.applied.as_ref().map(|goal| goal.selection.clone())),
                    trace,
                    session,
                    step,
                );
            }
            if wrote {
                services.input.changed.notify_one();
            }
        } else {
            self.run = Some(run);
        }
    }

    /// Release observers on session closure while retaining admitted work until it joins.
    pub(super) fn close(&mut self, trace: &TraceStore, session: &str, step: u64) {
        self.finish(Err(NativeGoalError::OwnerClosed), trace, session, step);
    }

    /// Consume every notification sender after committing the pass's finite result.
    fn finish(
        &mut self,
        result: Result<Option<NativeGoalSelection>, NativeGoalError>,
        trace: &TraceStore,
        session: &str,
        step: u64,
    ) {
        for (id, reply) in self.pending.drain(..) {
            respond(id, reply, result.clone(), trace, session, step);
        }
    }
}

struct AcknowledgedGoal {
    selection: NativeGoalSelection,
    generation: std::sync::Weak<crate::provider::Provider>,
}

/// Retain generation admission before acquiring the serial native goal transport lane.
pub(super) struct AcquiredResources {
    _control: OwnedSemaphorePermit,
    generation: ProviderGenerationPermit,
    engine: Arc<SessionExecution>,
}

struct Resources {
    acquired: AcquiredResources,
    _lane: OwnedMutexGuard<()>,
}

struct NativeGoalRun {
    id: uuid::Uuid,
    observation: GoalObservation,
    stage: Stage,
    fence: Option<JoinHandle<Result<(), ProviderError>>>,
    failed: Option<NativeGoalError>,
    issued: Option<NativeGoalSelection>,
    expires: Instant,
}

enum GoalObservation {
    Waiting,
    Pending,
}

impl NativeGoalRun {
    /// Advance owned resources, native transport, and canonical acknowledgment without awaiting I/O.
    fn update(
        &mut self,
        event: NativeGoalEvent,
        applied: &mut Option<AcknowledgedGoal>,
        services: &OwnerServices,
    ) -> Result<(), NativeGoalError> {
        match event {
            NativeGoalEvent::Acquired(result) => {
                self.stage = Stage::Drained { resources: None };
                if self.failed.is_some() {
                    return Ok(());
                }
                let acquired = result.map_err(|_| NativeGoalError::WorkerLost)??;
                let lane = services.tasks.native_goal_lane();
                self.stage = Stage::Lane {
                    acquired,
                    task: tokio::spawn(async move { lane.lock_owned().await }),
                    deadline: Some(deadline()),
                };
            }
            NativeGoalEvent::Lane(result) => {
                let Stage::Lane { acquired, .. } =
                    std::mem::replace(&mut self.stage, Stage::Drained { resources: None })
                else {
                    unreachable!("lane result retains its generation admission")
                };
                if self.failed.is_some() {
                    return Ok(());
                }
                let lane = result.map_err(|_| NativeGoalError::WorkerLost)?;
                self.stage = Stage::Ready(Arc::new(Resources {
                    acquired,
                    _lane: lane,
                }));
            }
            NativeGoalEvent::Ready => {
                if self.failed.is_some() {
                    return Ok(());
                }
                let Stage::Ready(resources) =
                    std::mem::replace(&mut self.stage, Stage::Drained { resources: None })
                else {
                    unreachable!("only fully admitted transport resources can select native work")
                };
                // Revalidate authority without releasing the resources needed to fence a failed write.
                let prepared = prepare_write(services, &resources, applied, self.expires);
                let (mut selected, request) = match prepared {
                    Ok(Some(prepared)) => prepared,
                    Ok(None) => {
                        self.stage = Stage::Drained {
                            resources: Some(resources),
                        };
                        return Ok(());
                    }
                    Err(error) => {
                        self.stage = Stage::Drained {
                            resources: Some(resources),
                        };
                        return Err(error);
                    }
                };
                // Invalidate earlier acknowledgments before polling another native write.
                selected.transport_epoch = match services.tasks.native_goal_written() {
                    Ok(epoch) => epoch,
                    Err(_) => {
                        self.stage = Stage::Drained {
                            resources: Some(resources),
                        };
                        return Err(NativeGoalError::Uncertain);
                    }
                };
                self.issued = Some(selected.clone());
                let transport = resources.clone();
                let update = selected.update.clone();
                self.stage = Stage::Writing {
                    resources,
                    task: tokio::spawn(async move {
                        transport
                            .acquired
                            .generation
                            .provider()
                            .goal_status(request, update)
                            .await
                    }),
                    selected,
                    deadline: Some(deadline().min(self.expires)),
                };
            }
            NativeGoalEvent::Written(result) => {
                let Stage::Writing {
                    resources,
                    selected,
                    ..
                } = std::mem::replace(&mut self.stage, Stage::Drained { resources: None })
                else {
                    unreachable!(
                        "write result retains its selected revision and transport resources"
                    )
                };
                self.stage = Stage::Drained {
                    resources: Some(resources),
                };
                result
                    .map_err(|_| NativeGoalError::WorkerLost)?
                    .map_err(NativeGoalError::Provider)?;
                if self.failed.is_some() {
                    return Ok(());
                }
                let resources = self
                    .stage
                    .resources()
                    .expect("completed write retains transport resources");
                let current = services
                    .provider
                    .read()
                    .unwrap_or_else(std::sync::PoisonError::into_inner)
                    .clone();
                if !Arc::ptr_eq(resources.acquired.generation.provider(), &current) {
                    return Err(NativeGoalError::Uncertain);
                }
                // Commit only the acknowledgment still covered by the canonical task and epoch.
                if acknowledge(&selected, services)? {
                    *applied = Some(AcknowledgedGoal {
                        selection: selected,
                        generation: Arc::downgrade(resources.acquired.generation.provider()),
                    });
                } else {
                    let Stage::Drained {
                        resources: Some(resources),
                    } = std::mem::replace(&mut self.stage, Stage::Drained { resources: None })
                    else {
                        unreachable!(
                            "stale acknowledgment retains the admitted transport resources"
                        )
                    };
                    self.stage = Stage::Ready(resources);
                }
            }
            NativeGoalEvent::Deadline => {
                // Abort only local lock acquisition; keep actual native writes retained and polled.
                match &mut self.stage {
                    Stage::Acquiring { task, deadline } => {
                        *deadline = None;
                        task.abort();
                    }
                    Stage::Lane { task, deadline, .. } => {
                        *deadline = None;
                        task.abort();
                    }
                    Stage::Writing { deadline, .. } => {
                        *deadline = None;
                    }
                    Stage::Ready(_) | Stage::Drained { .. } => {
                        unreachable!("only a retained job has an observation deadline")
                    }
                }
                return Err(NativeGoalError::Deadline);
            }
            NativeGoalEvent::Fenced(result) => {
                self.fence.take();
                result
                    .map_err(|_| NativeGoalError::WorkerLost)?
                    .map_err(NativeGoalError::Provider)?;
            }
        }
        Ok(())
    }

    /// Retain uncertain native effects and fence the exact captured engine, never its replacement.
    fn fail(&mut self, error: NativeGoalError, services: &OwnerServices) {
        if self.failed.is_some() {
            return;
        }
        if self.issued.is_some() {
            services.tasks.mark_native_goal_uncertain();
            if let Some(resources) = self.stage.resources() {
                let transport = resources.clone();
                self.fence = Some(tokio::spawn(async move {
                    transport.acquired.engine.close().await
                }));
            }
        }
        if matches!(self.stage, Stage::Ready(_)) {
            let Stage::Ready(resources) =
                std::mem::replace(&mut self.stage, Stage::Drained { resources: None })
            else {
                unreachable!("a synchronous failure retains the same ready transport resources")
            };
            self.stage = Stage::Drained {
                resources: Some(resources),
            };
        }
        self.failed = Some(error);
    }
}

enum Stage {
    Acquiring {
        task: JoinHandle<Result<AcquiredResources, NativeGoalError>>,
        deadline: Option<Instant>,
    },
    Lane {
        acquired: AcquiredResources,
        task: JoinHandle<OwnedMutexGuard<()>>,
        deadline: Option<Instant>,
    },
    Ready(Arc<Resources>),
    Writing {
        resources: Arc<Resources>,
        task: JoinHandle<Result<(), ProviderError>>,
        selected: NativeGoalSelection,
        deadline: Option<Instant>,
    },
    Drained {
        resources: Option<Arc<Resources>>,
    },
}

impl Stage {
    /// Get the finite stage identity for owner transition logs.
    fn name(&self) -> &'static str {
        match self {
            Self::Acquiring { .. } => "acquiring_generation",
            Self::Lane { .. } => "acquiring_transport_lane",
            Self::Ready(_) => "selecting_goal",
            Self::Writing { .. } => "writing_goal",
            Self::Drained { .. } => "work_drained",
        }
    }

    /// Get the transport resources owned by an admitted write or its collection phase.
    fn resources(&self) -> Option<&Arc<Resources>> {
        match self {
            Self::Ready(resources)
            | Self::Writing { resources, .. }
            | Self::Drained {
                resources: Some(resources),
            } => Some(resources),
            Self::Acquiring { .. } | Self::Lane { .. } | Self::Drained { resources: None } => None,
        }
    }

    /// Poll the retained job without consuming its handle until its result is applied.
    async fn next(&mut self) -> NativeGoalEvent {
        match self {
            Self::Acquiring { task, deadline } => {
                tokio::select! { result = task => NativeGoalEvent::Acquired(result), _ = wait_deadline(*deadline) => NativeGoalEvent::Deadline }
            }
            Self::Lane { task, deadline, .. } => {
                tokio::select! { result = task => NativeGoalEvent::Lane(result), _ = wait_deadline(*deadline) => NativeGoalEvent::Deadline }
            }
            Self::Writing { task, deadline, .. } => {
                tokio::select! { result = task => NativeGoalEvent::Written(result), _ = wait_deadline(*deadline) => NativeGoalEvent::Deadline }
            }
            Self::Ready(_) => NativeGoalEvent::Ready,
            Self::Drained { .. } => std::future::pending().await,
        }
    }
}

/// Select a write under current authority, or complete a verified no-op without transport I/O.
fn prepare_write(
    services: &OwnerServices,
    resources: &Resources,
    applied: &mut Option<AcknowledgedGoal>,
    expires: Instant,
) -> Result<Option<(NativeGoalSelection, ProviderRequest)>, NativeGoalError> {
    if services.tasks.native_goal_is_uncertain() {
        return Err(NativeGoalError::Uncertain);
    }
    let Some((selected, request)) = capture(services)? else {
        *applied = None;
        return Ok(None);
    };
    let provider = resources.acquired.generation.provider();
    let current = services
        .provider
        .read()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .clone();
    if !Arc::ptr_eq(provider, &current) {
        return Err(NativeGoalError::GenerationChanged);
    }
    let cached = applied.as_ref().is_some_and(|goal| {
        goal.selection == selected
            && goal
                .generation
                .upgrade()
                .is_some_and(|generation| Arc::ptr_eq(&generation, provider))
    });
    if cached && acknowledge(&selected, services)? {
        return Ok(None);
    }
    if Instant::now() >= expires {
        return Err(NativeGoalError::Deadline);
    }
    Ok(Some((selected, request)))
}

/// Identify the actual retained goal effect whose result the owner is applying.
pub(super) enum NativeGoalEvent {
    /// Observe local admission and the provider generation captured under its retirement permit.
    Acquired(Result<Result<AcquiredResources, NativeGoalError>, tokio::task::JoinError>),
    /// Observe exclusive admission to the shared native goal transport lane.
    Lane(Result<OwnedMutexGuard<()>, tokio::task::JoinError>),
    /// Observe the actual selected native write, including late completion after a deadline.
    Written(Result<Result<(), ProviderError>, tokio::task::JoinError>),
    /// Observe fencing of the exact captured execution engine after native uncertainty.
    Fenced(Result<Result<(), ProviderError>, tokio::task::JoinError>),
    /// Revalidate current authority before beginning another effect or acknowledging a cached value.
    Ready,
    /// Expire an observation without dropping its actual native worker.
    Deadline,
}

impl NativeGoalEvent {
    /// Get a bounded event label without recording goal text or opaque native identity.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Acquired(_) => "native_goal_resources_acquired",
            Self::Lane(_) => "native_goal_lane_acquired",
            Self::Written(_) => "native_goal_write_completed",
            Self::Fenced(_) => "native_goal_fence_completed",
            Self::Ready => "native_goal_selection_ready",
            Self::Deadline => "native_goal_observation_deadline",
        }
    }
}

/// Capture a complete normalized request and task selection under current committed authority.
fn capture(
    services: &OwnerServices,
) -> Result<Option<(NativeGoalSelection, ProviderRequest)>, NativeGoalError> {
    services
        .tasks
        .with_task_snapshot(|task| capture_task(services, task).map_err(Into::into))
        .map_err(state_failure)
}

/// Read the native conversation and question gates while task replacement stays excluded.
fn capture_task(
    services: &OwnerServices,
    task: Option<crate::task::Task>,
) -> Result<Option<(NativeGoalSelection, ProviderRequest)>, NativeGoalError> {
    let session = crate::storage::SqliteStore::read_only(&services.root)
        .and_then(|store| store.load_session(services.observer.session_id()))
        .map_err(|_| NativeGoalError::StateUnavailable)?
        .ok_or(NativeGoalError::StateUnavailable)?;
    let Some(id) = session.provider_session_id else {
        return Ok(None);
    };
    let provider_session_id: forge_provider_api::ProviderSessionId = id
        .try_into()
        .map_err(|_| NativeGoalError::StateUnavailable)?;
    let questions = services
        .questions
        .snapshot()
        .map_err(|_| NativeGoalError::StateUnavailable)?;
    let outbox = services
        .questions
        .outbox()
        .map_err(|_| NativeGoalError::StateUnavailable)?;
    let input = services
        .input
        .snapshot()
        .map_err(|_| NativeGoalError::StateUnavailable)?;
    let blocked = questions.required_pending()
        || !outbox.is_empty()
        || input.messages.iter().any(|message| {
            matches!(
                message.purpose,
                crate::input::InputPurpose::Question { .. }
                    | crate::input::InputPurpose::Clarification { .. }
                    | crate::input::InputPurpose::QuestionContinuation { .. }
                    | crate::input::InputPurpose::WorkflowContinuation { .. }
            ) && message.delivery != crate::input::InputDelivery::Accepted
        });
    let update = select_update(task.as_ref(), blocked);
    let selected = NativeGoalSelection {
        provider_session_id: provider_session_id.clone(),
        revision: task.map(|task| task.revision.clone()),
        transport_epoch: services.tasks.native_goal_epoch(),
        update,
    };
    let request = ProviderRequest {
        dispatch: Default::default(),
        execution_owner: None,
        harness_session_id: session.id,
        workspace: session.workspace,
        input: crate::provider::ProviderInput::from_text(""),
        mode: crate::provider::PromptMode::GoalContinuation,
        model: session.model,
        effort: session.effort,
        context_window: session.context_window,
        service_tier: session.service_tier,
        access: session.access,
        approval_mode: session.approval_mode,
        provider_session_id: Some(provider_session_id),
        submission: None,
        recovered_input: Vec::new(),
        control_context: None,
    };
    Ok(Some((selected, request)))
}

/// Select normalized native state while preserving suspended tasks and pending required decisions.
fn select_update(task: Option<&crate::task::Task>, blocked: bool) -> NativeGoalUpdate {
    use crate::{
        goal::GoalState, plan::PlanExecutionState, provider::native_goal_update,
        task::WorkflowSnapshot,
    };
    let Some(task) = task else {
        return NativeGoalUpdate::Absent;
    };
    match &task.workflow {
        WorkflowSnapshot::Goal { goal } if goal.native => native_goal_update(
            goal.objective.clone(),
            if (!task.entry_accepted() || blocked) && goal.state == GoalState::Active {
                GoalState::Paused
            } else {
                goal.state
            },
        ),
        WorkflowSnapshot::Execute { execution } if execution.native => native_goal_update(
            execution.title.clone(),
            match execution.record.state {
                PlanExecutionState::Active if task.entry_accepted() && !blocked => {
                    GoalState::Active
                }
                PlanExecutionState::Active
                | PlanExecutionState::Paused
                | PlanExecutionState::Stalled => GoalState::Paused,
                PlanExecutionState::Complete => GoalState::Complete,
                PlanExecutionState::Blocked => GoalState::Blocked,
                PlanExecutionState::Cancelled => GoalState::Cleared,
            },
        ),
        WorkflowSnapshot::Goal { .. }
        | WorkflowSnapshot::Execute { .. }
        | WorkflowSnapshot::Plan { .. } => NativeGoalUpdate::Absent,
    }
}

/// Publish acknowledgment only if task, question gate, native conversation, and epoch still match.
fn acknowledge(
    expected: &NativeGoalSelection,
    services: &OwnerServices,
) -> Result<bool, NativeGoalError> {
    services
        .tasks
        .with_task_snapshot(|task| {
            if services.tasks.native_goal_is_uncertain() {
                return Err(NativeGoalError::Uncertain.into());
            }
            let current = capture_task(services, task)?.map(|(selection, _)| selection);
            if current.as_ref() != Some(expected) {
                return Ok(false);
            }
            services
                .tasks
                .acknowledge_native_goal(expected.transport_epoch);
            Ok(true)
        })
        .map_err(state_failure)
}

/// Preserve typed failures through the task controller's existing legacy callback contract.
fn state_failure(error: anyhow::Error) -> NativeGoalError {
    error
        .downcast::<NativeGoalError>()
        .unwrap_or(NativeGoalError::StateUnavailable)
}

/// Resolve internal observation once and log whether its waiter still exists.
fn respond(
    id: uuid::Uuid,
    reply: NativeGoalReply,
    result: Result<Option<NativeGoalSelection>, NativeGoalError>,
    trace: &TraceStore,
    session: &str,
    step: u64,
) {
    let code = result.as_ref().err().map(NativeGoalError::code);
    let observed = reply.send(result).is_ok();
    trace.record(
        session,
        "exchange.native_goal.response",
        serde_json::json!({"sync_id":id,"update_step":step,"error_code":code,"observed":observed}),
    );
}

/// Bound each asynchronous observation while keeping a separate total pass deadline.
fn deadline() -> Instant {
    Instant::now() + std::time::Duration::from_secs(30)
}

/// Disable only the deadline branch after its first observation; the actual job remains polled.
async fn wait_deadline(deadline: Option<Instant>) {
    match deadline {
        Some(deadline) => tokio::time::sleep_until(deadline).await,
        None => std::future::pending().await,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures_util::FutureExt;

    /// Ready-event buffering must not poll a consumed worker result before the owner applies it.
    #[tokio::test]
    async fn native_goal_buffers_completed_worker_until_owner_update() {
        let mut sync = NativeGoalSync {
            run: Some(NativeGoalRun {
                id: uuid::Uuid::new_v4(),
                observation: GoalObservation::Waiting,
                stage: Stage::Acquiring {
                    task: tokio::spawn(async { Err(NativeGoalError::StateUnavailable) }),
                    deadline: None,
                },
                fence: None,
                failed: None,
                issued: None,
                expires: deadline(),
            }),
            ..Default::default()
        };
        let event = tokio::time::timeout(std::time::Duration::from_secs(1), sync.next())
            .await
            .expect("completed acquisition should become ready");
        assert!(matches!(
            event,
            NativeGoalEvent::Acquired(Ok(Err(NativeGoalError::StateUnavailable)))
        ));
        assert!(sync.next().now_or_never().is_none());
        assert!(sync.next().now_or_never().is_none());
    }
}
