//! Collect and settle exchange transitions from identified evidence inside the session owner.
use crate::provider::ScopeOwner;

use tokio::sync::OwnedSemaphorePermit;

use super::command::{
    CancellationOutcome, CancelledInputPolicy, CommandFailure, CommandId, InputRevision,
};
use super::services::{OwnerServices, ProviderGenerationPermit};
use crate::broker::{HarnessBroker, PriorFinalization, PriorFinalizationAction};
use crate::provider::{ExchangeId, ProviderError};

/// Bound retained stop collection after its command already answered at the stop deadline.
///
/// Twenty seconds is the longest interrupt settlement grace any adapter allows (Claude's), so
/// a collection retained at the thirty-second stop deadline has had every adapter's full grace
/// before the backstop ends its binding; a stop leaves `Stopping` within fifty seconds at most.
pub(super) const STOP_COLLECTION_BACKSTOP: std::time::Duration = std::time::Duration::from_secs(20);

/// Report the owner's actual foreground availability, without inspecting semaphore counts.
#[derive(Clone)]
pub(super) enum TurnStatus {
    /// An admitted foreground transaction still owns work or settlement.
    Running(crate::provider::InvocationScope),
    /// Owned host work has not admitted a provider invocation.
    Local,
    /// No foreground transaction owns work.
    Idle,
    /// Prior cleanup did not establish usable execution availability.
    Blocked,
}

/// Select whether a root transition changes authority used by existing child invocations.
#[derive(Clone, Copy)]
pub(super) enum ChildCollectionPolicy {
    /// Preserve independent child sessions while only the root invocation's configuration changes.
    Preserve,
    /// Collect children and retain admission quiescence while their shared policy changes.
    Collect,
}

/// Describe the internal collection purpose; callers never enqueue a restart command.
#[derive(Clone, Copy)]
pub(super) enum StopIntent {
    /// Settle explicit user cancellation, including local work and optional draft restoration.
    Cancel(CancelledInputPolicy),
    /// Park affected root work for owner-controlled reconfiguration and replacement dispatch.
    Reconfigure { children: ChildCollectionPolicy },
}

impl StopIntent {
    /// Get the label an exchange keeps once its turn is collected, or `None` to clear it.
    ///
    /// While the stopping command still runs, both intents keep the exchange labelled as
    /// stopping until that command settles it: a reconfiguration's command resumes, closes, or
    /// releases the parked exchange. A collection that arrives after its command already failed
    /// leaves no command to do so, so a reconfiguration clears the label then, while a
    /// cancellation stays visible until its host settlement ends.
    fn collected_status(self, command_running: bool) -> Option<super::ExchangeCollection> {
        match self {
            Self::Cancel(_) => Some(super::ExchangeCollection::Settling),
            Self::Reconfigure { .. } => {
                command_running.then_some(super::ExchangeCollection::Settling)
            }
        }
    }
    /// Get collection scope without consulting semaphore counts or provider identity.
    fn children(self) -> ChildCollectionPolicy {
        match self {
            Self::Cancel(_) => ChildCollectionPolicy::Collect,
            Self::Reconfigure { children } => children,
        }
    }

    /// Get retraction intent; internal reconfiguration never restores the user's draft.
    fn input(self) -> CancelledInputPolicy {
        match self {
            Self::Cancel(input) => input,
            Self::Reconfigure { .. } => CancelledInputPolicy::Retain,
        }
    }
}

/// Preserve the exact postcondition of collection before the command's next domain stage.
pub(super) enum TransitionOutcome {
    /// Explicit cancellation settled its selected work.
    Cancelled(CancellationOutcome),
    /// Root collection settled; only a continuation authorized before collection may be replaced.
    Reconfigured { exchange: Option<ExchangeId> },
}

/// Retain the exact invocation owner selected before asynchronous cancellation.
#[derive(Clone)]
struct InvocationCollection {
    engine: std::sync::Arc<crate::provider::session_execution::SessionExecution>,
    scope: crate::provider::InvocationScope,
    input: forge_provider_api::operations::InterruptInput,
    collection: std::sync::Arc<
        std::sync::Mutex<Option<crate::provider::session_execution::ExchangeCollection>>,
    >,
}

/// Select when the owner freezes the invocation targeted by collection.
#[derive(Clone, Copy)]
enum TargetCapture {
    Admission,
    Acquisition,
}

/// Keep legacy admission and child resource gates owned until cancellation settles.
pub(super) struct ControlOwnership {
    control: Option<OwnedSemaphorePermit>,
    children: Option<crate::agent::AgentQuiescence>,
}

/// Retain one command's internal collection purpose, scope, and asynchronous stage.
pub(super) struct ExchangeStop {
    id: TransitionIdentity,
    intent: StopIntent,
    stage: TransitionStage,
    generation: Option<ProviderGenerationPermit>,
    invocation: Option<InvocationCollection>,
    target_capture: TargetCapture,
    uncommitted: Option<TransitionEvent>,
    foreground_signals: std::collections::VecDeque<crate::broker::ForegroundSignal>,
    reporting: TransitionReporting,
    backstop: StopBackstop,
}

/// Reach the session's root binding when a stop's collection outlives `STOP_COLLECTION_BACKSTOP`.
struct StopBackstop {
    provider: std::sync::Arc<std::sync::RwLock<std::sync::Arc<crate::provider::Provider>>>,
    owner: crate::provider::ThreadOwner,
}

impl StopBackstop {
    fn new(services: &OwnerServices) -> Self {
        Self {
            provider: services.provider.clone(),
            owner: crate::provider::ThreadOwner::root(services.observer.session_id()),
        }
    }

    /// Force-end the root binding so its stuck native collection returns or is abandoned.
    async fn terminate(&self) -> bool {
        let provider = self
            .provider
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let terminated = provider.session_terminate_native(&self.owner).await;
        tracing::error!(target: "forge::harness", event_name = "exchange.stop.backstop",
            reason = "collection_unsettled",
            backstop_ms = STOP_COLLECTION_BACKSTOP.as_millis() as u64,
            terminated = terminated.is_ok(),
            "Stop collection never returned, so the binding was force-terminated");
        terminated.is_ok()
    }
}

/// Report a stop whose native collection the backstop abandoned.
fn abandoned_collection() -> ProviderError {
    forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::TurnInterrupt,
        forge_provider_api::ProviderFailureKind::Timeout,
    )
    .at_phase(
        forge_provider_api::ProviderPhase::Collection,
        forge_provider_api::FailureDisposition::Invalidated,
    )
    .into()
}

impl ExchangeStop {
    /// Project the retained owner stage without consuming native or host work.
    pub(super) fn cancellation_progress(&self) -> (super::CancellationPhase, Option<ExchangeId>) {
        use super::CancellationPhase;
        let phase = match self.stage {
            TransitionStage::Acquiring(_) | TransitionStage::Preparing { .. } => {
                CancellationPhase::Acquiring
            }
            TransitionStage::Collecting { .. } => CancellationPhase::Collecting,
            TransitionStage::Draining { .. } => CancellationPhase::Draining,
            TransitionStage::Settling { .. } => CancellationPhase::Settling,
            TransitionStage::Ready { .. } | TransitionStage::Consumed => CancellationPhase::Settled,
            TransitionStage::Uncertain { .. } => CancellationPhase::Recovering,
        };
        let target = match &self.stage {
            TransitionStage::Collecting { target, .. }
            | TransitionStage::Draining { target, .. }
            | TransitionStage::Settling { target, .. } => {
                target.as_ref().map(|target| target.id().clone())
            }
            _ => None,
        };
        (
            phase,
            self.invocation
                .as_ref()
                .map(|invocation| invocation.scope.exchange_id.clone())
                .or(target),
        )
    }

    /// End response observation without abandoning collection or foreground settlement.
    pub(super) fn expire_response(&mut self) {
        self.reporting.response_expired = true;
    }

    pub(super) fn response_expired(&self) -> bool {
        self.reporting.response_expired
    }

    /// Correlate retained cleanup evidence without exposing provider payloads or writable state.
    pub(super) fn core_transition(
        &self,
        kind: super::core_log::CoreTransitionKind,
        outcome: super::core_log::CoreOutcome,
    ) -> super::core_log::CoreRecord {
        super::core_log::CoreRecord::TransitionCommitted {
            command: match &self.id {
                TransitionIdentity::Command(id) => Some(id.clone()),
                TransitionIdentity::Shutdown(_) | TransitionIdentity::Input(_) => None,
            },
            invocation: self
                .invocation
                .as_ref()
                .map(|invocation| invocation.scope.clone()),
            kind,
            outcome,
        }
    }

    /// Freeze the exact cancellation target before asynchronous control acquisition can time out.
    pub(super) fn pin_cancellation_target(
        &mut self,
        foreground: &TurnStatus,
        broker: &mut HarnessBroker,
        session: &str,
        input: forge_provider_api::operations::InterruptInput,
    ) -> Result<(), TransitionFailure> {
        self.target_capture = TargetCapture::Admission;
        let owner = crate::provider::ThreadOwner::root(session);
        let engine = broker
            .execution_registry()
            .existing_owner(&owner)
            .map_err(|error| TransitionFailure::Domain(error.into()))?;
        // A failing foreground closes its engine before settling, and its failure cleanup then
        // owns the native work. Cancellation pins nothing and waits for that foreground to settle.
        let Some(engine) = engine else {
            return Ok(());
        };
        let scope = match foreground {
            TurnStatus::Running(scope) => Some(scope.clone()),
            TurnStatus::Local | TurnStatus::Idle | TurnStatus::Blocked => engine
                .active_invocation_scope()
                .map_err(|error| TransitionFailure::Domain(error.into()))?,
        };
        self.invocation = scope.map(|scope| InvocationCollection {
            engine,
            scope,
            input,
            collection: Default::default(),
        });
        Ok(())
    }

    /// Authorize a configured successor only after this exact reconfiguration has drained.
    pub(super) fn authorize_replacement(
        &self,
        selected: &ExchangeId,
    ) -> Result<Option<tokio::task::JoinHandle<Result<(), ProviderError>>>, TransitionFailure> {
        if !matches!(self.intent, StopIntent::Reconfigure { .. })
            || !matches!(self.stage, TransitionStage::Ready { .. })
        {
            return Err(TransitionFailure::Uncertain);
        }
        let Some(invocation) = self.invocation.clone() else {
            return Ok(None);
        };
        if &invocation.scope.exchange_id != selected {
            return Err(TransitionFailure::Rejected(CommandFailure::ExchangeChanged));
        }
        let collection = invocation
            .collection
            .lock()
            .map_err(|_| TransitionFailure::Uncertain)?
            .take()
            .ok_or(TransitionFailure::Uncertain)?;
        Ok(Some(tokio::spawn(async move {
            invocation.engine.resume_exchange(collection).await
        })))
    }

    /// Transfer ordered collection decisions to the owner-held foreground before reading native events.
    pub(super) fn take_collection_signal(&mut self) -> Option<crate::broker::ForegroundSignal> {
        self.foreground_signals.pop_front()
    }

    /// Preserve cleanup uncertainty when either the command or its collection deadline expires.
    pub(super) fn report_collection_timeout(
        &mut self,
        broker: &mut HarnessBroker,
    ) -> Result<(), TransitionFailure> {
        if let Some(invocation) = &self.invocation {
            broker
                .record_collection_timeout(&invocation.scope.exchange_id)
                .map_err(transition_failure)?;
            self.foreground_signals
                .push_back(crate::broker::ForegroundSignal::CollectionTimedOut);
        }
        Ok(())
    }

    /// Transfer settled compatibility control into the next phase of the same internal operation.
    pub(super) fn take_control(&mut self) -> Option<OwnedSemaphorePermit> {
        match &mut self.stage {
            TransitionStage::Ready { _ownership } => _ownership.control.take(),
            TransitionStage::Acquiring(_)
            | TransitionStage::Preparing { .. }
            | TransitionStage::Collecting { .. }
            | TransitionStage::Draining { .. }
            | TransitionStage::Settling { .. }
            | TransitionStage::Uncertain { .. }
            | TransitionStage::Consumed => None,
        }
    }
    /// Start bounded ownership acquisition after the active command revision commits.
    pub(super) fn start(
        id: CommandId,
        intent: StopIntent,
        services: &OwnerServices,
        generation: Option<ProviderGenerationPermit>,
        admitted_control: Option<OwnedSemaphorePermit>,
    ) -> Self {
        Self::start_at(
            id,
            intent,
            services,
            generation,
            admitted_control,
            tokio::time::Instant::now(),
        )
    }

    /// Preserve the active command admission boundary through every asynchronous phase.
    pub(super) fn start_at(
        id: CommandId,
        intent: StopIntent,
        services: &OwnerServices,
        generation: Option<ProviderGenerationPermit>,
        admitted_control: Option<OwnedSemaphorePermit>,
        admitted_at: tokio::time::Instant,
    ) -> Self {
        Self::begin(
            TransitionIdentity::Command(id),
            intent,
            services,
            generation,
            admitted_control,
            admitted_at,
        )
    }

    /// Collect session work during owner shutdown without admitting a synthetic public command.
    pub(super) fn for_shutdown(services: &OwnerServices) -> Self {
        Self::begin(
            TransitionIdentity::Shutdown(uuid::Uuid::new_v4()),
            StopIntent::Cancel(CancelledInputPolicy::Retain),
            services,
            None,
            None,
            tokio::time::Instant::now(),
        )
    }

    /// Prompt preparation collects the previous provider invocation. Commands and shutdown
    /// also collect the session's independently owned check process.
    fn collect_check(services: &OwnerServices, collect: bool) -> anyhow::Result<Option<String>> {
        if !collect {
            return Ok(None);
        }
        let check = services.checks.active()?;
        if let Some(id) = &check {
            services.checks.cancel_for_collection(id)?;
        }
        Ok(check)
    }

    /// Collect for an identified internal input without manufacturing a public command.
    pub(super) fn for_input(
        input: InputRevision,
        children: ChildCollectionPolicy,
        services: &OwnerServices,
        generation: Option<ProviderGenerationPermit>,
        admitted_control: Option<OwnedSemaphorePermit>,
    ) -> Self {
        Self::begin(
            TransitionIdentity::Input(input),
            StopIntent::Reconfigure { children },
            services,
            generation,
            admitted_control,
            tokio::time::Instant::now(),
        )
    }

    /// Retain collection resources under the caller command or internal input that authorized them.
    fn begin(
        id: TransitionIdentity,
        intent: StopIntent,
        services: &OwnerServices,
        generation: Option<ProviderGenerationPermit>,
        admitted_control: Option<OwnedSemaphorePermit>,
        admitted_at: tokio::time::Instant,
    ) -> Self {
        let reporting = TransitionReporting::new(&id, intent, admitted_at);
        let control = services.control.clone();
        let children = services.children.clone();
        let child_policy = intent.children();
        let restart = matches!(
            intent,
            StopIntent::Reconfigure {
                children: ChildCollectionPolicy::Collect
            }
        )
        .then(|| id.clone());
        let task = tokio::spawn(async move {
            let control = match admitted_control {
                Some(control) => control,
                None => match control.acquire_owned().await {
                    Ok(control) => control,
                    Err(_) => return Acquisition::Unavailable,
                },
            };
            let ownership = ControlOwnership {
                control: Some(control),
                children: match child_policy {
                    ChildCollectionPolicy::Collect => Some(children.begin_quiescence()),
                    ChildCollectionPolicy::Preserve => None,
                },
            };
            if let Some(id) = restart
                && interrupt_children(&children, &id).await.is_err()
            {
                return Acquisition::Uncertain(ownership);
            }
            Acquisition::Ready(ownership)
        });
        Self {
            backstop: StopBackstop::new(services),
            id,
            intent,
            stage: TransitionStage::Acquiring(task),
            generation,
            invocation: None,
            target_capture: TargetCapture::Acquisition,
            uncommitted: None,
            foreground_signals: std::collections::VecDeque::new(),
            reporting,
        }
    }

    /// Retain a yielded fact when storage failure prevents its normal owner transition.
    pub(super) fn retain_uncommitted(&mut self, event: TransitionEvent) {
        self.uncommitted = Some(event);
    }

    /// Await identified facts while the driver keeps processing native events and queued intent.
    pub(super) async fn next(&mut self, foreground: TurnStatus) -> TransitionEvent {
        if self.reporting.response_expired {
            if matches!(self.stage, TransitionStage::Draining { .. }) {
                // A turn whose collection completed, including by termination, settles from its
                // adapter's terminal report. One that has not settled two backstops after the
                // deadline never will, and the stop fails so the provider restart ends it.
                let unavailable_at = self.reporting.response_at() + 2 * STOP_COLLECTION_BACKSTOP;
                return tokio::select! {
                    biased;
                    event = self.next_stage(foreground) => event,
                    () = tokio::time::sleep_until(unavailable_at) => {
                        TransitionEvent::ForegroundUnavailable
                    }
                };
            }
            if !matches!(self.stage, TransitionStage::Collecting { .. }) {
                return self.next_stage(foreground).await;
            }
            // The command already answered at the stop deadline. Collection still pending a
            // backstop later never returns by itself, so the binding is terminated and the
            // collection gets one more backstop to observe that before it is abandoned. Both
            // boundaries are absolute, because the driver re-polls this future after other wakes.
            let terminate_at = self.reporting.response_at() + STOP_COLLECTION_BACKSTOP;
            tokio::select! {
                biased;
                event = self.next_stage(foreground.clone()) => return event,
                () = tokio::time::sleep_until(terminate_at) => {}
            }
            let terminated = self.backstop.terminate().await;
            let event = tokio::select! {
                biased;
                event = self.next_stage(foreground) => event,
                () = tokio::time::sleep_until(terminate_at + STOP_COLLECTION_BACKSTOP) => {
                    TransitionEvent::Collected(Err(abandoned_collection()))
                }
            };
            // Termination ends everything the binding owned, which is the collection the stop
            // waited for, so the cancellation settles normally; the interrupt's own error then
            // only reports that its binding is gone.
            return match event {
                TransitionEvent::Collected(_) if terminated => TransitionEvent::Collected(Ok(())),
                event => event,
            };
        }
        let report_at = self.reporting.next_at();
        tokio::select! {
            biased;
            event = self.next_stage(foreground) => event,
            () = tokio::time::sleep_until(report_at) => self.reporting.event(),
        }
    }

    /// Prefer already available authoritative facts over an elapsed reporting boundary.
    async fn next_stage(&mut self, foreground: TurnStatus) -> TransitionEvent {
        match &mut self.stage {
            TransitionStage::Acquiring(task) => {
                TransitionEvent::Acquired(task.await.unwrap_or(Acquisition::ObservationLost))
            }
            TransitionStage::Collecting { task, .. } => TransitionEvent::Collected(
                task.await.unwrap_or(Err(ProviderError::DeliveryUncertain)),
            ),
            TransitionStage::Draining { .. } => match foreground {
                TurnStatus::Running(_) | TurnStatus::Local => std::future::pending().await,
                TurnStatus::Idle => TransitionEvent::ForegroundSettled,
                TurnStatus::Blocked => TransitionEvent::ForegroundUnavailable,
            },
            TransitionStage::Settling { job, .. } => match job {
                SettlementJob::Advance => TransitionEvent::AdvanceSettlement,
                SettlementJob::Observation(task) => {
                    TransitionEvent::Observed(task.await.unwrap_or_else(|error| Err(error.into())))
                }
                SettlementJob::Initial(task) => {
                    TransitionEvent::Initial(task.await.unwrap_or_else(|error| Err(error.into())))
                }
                SettlementJob::Final(task) => TransitionEvent::Final(
                    task.await
                        .unwrap_or_else(|error| Err(error.into()))
                        .map(Box::new),
                ),
            },
            TransitionStage::Preparing { .. }
            | TransitionStage::Ready { .. }
            | TransitionStage::Uncertain { .. }
            | TransitionStage::Consumed => std::future::pending().await,
        }
    }

    /// Apply collection facts and select the next effect only under owner state ownership.
    pub(super) fn update(
        &mut self,
        event: TransitionEvent,
        foreground: TurnStatus,
        broker: &mut HarnessBroker,
        services: &OwnerServices,
    ) -> Result<TransitionProgress, TransitionFailure> {
        match event {
            TransitionEvent::Acquired(Acquisition::Unavailable) => {
                self.stage = TransitionStage::Consumed;
                Err(TransitionFailure::Rejected(
                    CommandFailure::OwnershipUnavailable,
                ))
            }
            TransitionEvent::Acquired(Acquisition::ObservationLost) => {
                self.stage = TransitionStage::Uncertain {
                    _ownership: ControlOwnership {
                        control: None,
                        children: match self.intent.children() {
                            ChildCollectionPolicy::Collect => {
                                Some(services.children.begin_quiescence())
                            }
                            ChildCollectionPolicy::Preserve => None,
                        },
                    },
                };
                Err(TransitionFailure::Uncertain)
            }
            TransitionEvent::Acquired(Acquisition::Uncertain(ownership)) => {
                self.stage = TransitionStage::Uncertain {
                    _ownership: ownership,
                };
                Err(TransitionFailure::Uncertain)
            }
            TransitionEvent::Acquired(Acquisition::Ready(ownership)) => {
                self.stage = TransitionStage::Preparing { ownership };
                let view: crate::broker::ExchangeControlView =
                    broker.owner_control_view().map_err(transition_failure)?;
                let input = match self.intent {
                    StopIntent::Cancel(_)
                        if matches!(self.target_capture, TargetCapture::Admission) =>
                    {
                        None
                    }
                    StopIntent::Cancel(_) => {
                        let command = match &self.id {
                            TransitionIdentity::Command(id) => Some(id.as_str()),
                            TransitionIdentity::Shutdown(_) => None,
                            TransitionIdentity::Input(_) => unreachable!(
                                "internal input collection never creates explicit cancellation"
                            ),
                        };
                        broker
                            .cancel_question_delivery()
                            .map_err(transition_failure)?;
                        Some(Box::new(
                            services
                                .input
                                .suspend(true, command)
                                .map_err(transition_failure)?,
                        ))
                    }
                    StopIntent::Reconfigure { .. } => None,
                };
                let check =
                    Self::collect_check(services, !matches!(self.id, TransitionIdentity::Input(_)))
                        .map_err(transition_failure)?;
                let TransitionStage::Preparing { ownership } =
                    std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                else {
                    unreachable!("acquired control ownership was installed before preparation")
                };
                let scope = match &foreground {
                    TurnStatus::Running(scope) => Some(scope.clone()),
                    TurnStatus::Local | TurnStatus::Idle | TurnStatus::Blocked => {
                        view.steering_scope.clone()
                    }
                };
                if matches!(self.target_capture, TargetCapture::Acquisition) {
                    self.invocation = scope
                        .map(|scope| {
                            let engine = broker
                                .execution_registry()
                                .existing_owner(&scope.owner())
                                .map_err(|error| TransitionFailure::Domain(error.into()))?
                                .ok_or(TransitionFailure::Uncertain)?;
                            Ok::<_, TransitionFailure>(InvocationCollection {
                                engine,
                                scope,
                                input: forge_provider_api::operations::InterruptInput::Retain,
                                collection: Default::default(),
                            })
                        })
                        .transpose()?;
                }
                let children = self.intent.children();
                let has_collection_work =
                    matches!(foreground, TurnStatus::Running(_) | TurnStatus::Local)
                        || self.invocation.is_some()
                        || check.is_some()
                        || (matches!(children, ChildCollectionPolicy::Collect)
                            && services.children.collection_count().has_work());
                if has_collection_work {
                    // A collection of the whole tree, such as a task control (`/plan cancel`,
                    // a pause, a plan decision) or a tree-wide configuration change, stops the
                    // delegating turn and the provider's native subagents with it, as a
                    // cancellation does. The foreground reducer ends those subagents for a
                    // cancellation; this collection reaches it as a restart, so it ends them
                    // here, before any of their queued output is drawn. A root-only
                    // reconfiguration keeps them, and a new turn of the exchange observes its
                    // subagents again.
                    if matches!(
                        self.intent,
                        StopIntent::Reconfigure {
                            children: ChildCollectionPolicy::Collect
                        }
                    ) && let Some(target) = &view.selection
                    {
                        broker.cancel_observed_children_of(target.id().as_str());
                    }
                    self.foreground_signals.push_back(
                        crate::broker::ForegroundSignal::CollectionStarted(match self.intent {
                            StopIntent::Cancel(_) => {
                                crate::broker::ForegroundCollection::Cancel(self.intent.input())
                            }
                            StopIntent::Reconfigure { .. } => {
                                crate::broker::ForegroundCollection::Restart
                            }
                        }),
                    );
                    services.cancellation.block_submissions();
                    let services = services.clone();
                    let child_operation =
                        matches!(self.intent, StopIntent::Cancel(_)).then(|| self.id.clone());
                    let generation = self.generation.clone();
                    let invocation = self.invocation.clone();
                    let task = tokio::spawn(async move {
                        collect_resources(
                            &services,
                            child_operation.as_ref(),
                            check,
                            children,
                            generation,
                            invocation,
                        )
                        .await
                    });
                    self.stage = TransitionStage::Collecting {
                        target: view.selection,
                        ownership,
                        task,
                    };
                } else {
                    self.stage = TransitionStage::Draining {
                        target: view.selection,
                        ownership,
                    };
                }
                Ok(TransitionProgress::Waiting {
                    events: view.events,
                    input,
                })
            }
            TransitionEvent::Collected(Ok(())) => {
                if !matches!(self.stage, TransitionStage::Collecting { .. }) {
                    return Err(TransitionFailure::Uncertain);
                }
                let TransitionStage::Collecting {
                    target, ownership, ..
                } = std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                else {
                    unreachable!(
                        "collection stage was validated before consuming its retained worker"
                    );
                };
                self.stage = TransitionStage::Draining {
                    target: target.clone(),
                    ownership,
                };
                let events = broker
                    .owner_collection_completed(
                        target
                            .as_ref()
                            .map(crate::broker::ExchangeControlSelection::id),
                        self.intent.collected_status(true),
                    )
                    .map_err(transition_failure)?;
                self.foreground_signals
                    .push_back(collection_completed_signal(
                        self.intent,
                        self.invocation.as_ref(),
                    ));
                Ok(TransitionProgress::Waiting {
                    events,
                    input: None,
                })
            }
            TransitionEvent::Collected(Err(_)) => {
                if !matches!(self.stage, TransitionStage::Collecting { .. }) {
                    return Err(TransitionFailure::Uncertain);
                }
                let TransitionStage::Collecting { ownership, .. } =
                    std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                else {
                    unreachable!(
                        "collection stage was validated before consuming its retained worker"
                    );
                };
                self.stage = TransitionStage::Uncertain {
                    _ownership: ownership,
                };
                Err(TransitionFailure::Uncertain)
            }
            TransitionEvent::CollectionPending => {
                self.reporting.pending_published = true;
                let policy = self
                    .reporting
                    .policy
                    .expect("pending reports require a cleanup policy");
                let update = crate::broker::publication::SessionUpdate::CleanupProgress {
                    operation_id: self.id.operation_id(),
                    policy,
                    progress: forge_provider_api::CleanupProgress::Pending,
                };
                let event = crate::broker::publication::encode_session_event(
                    services.observer.session_id(),
                    &update,
                )
                .map_err(|error| TransitionFailure::Domain(error.into()))?;
                Ok(TransitionProgress::Waiting {
                    events: vec![event],
                    input: None,
                })
            }
            TransitionEvent::CollectionDeadline => {
                if self.reporting.response_expired {
                    return Ok(TransitionProgress::Waiting {
                        events: Vec::new(),
                        input: None,
                    });
                }
                self.report_collection_timeout(broker)?;
                Err(TransitionFailure::Overdue)
            }
            TransitionEvent::ForegroundUnavailable => Err(TransitionFailure::Uncertain),
            TransitionEvent::ForegroundSettled => {
                let target = match &self.stage {
                    TransitionStage::Draining { target, .. } => target.clone(),
                    _ => {
                        return Err(TransitionFailure::Uncertain);
                    }
                };
                services.cancellation.arm();
                if matches!(self.intent, StopIntent::Reconfigure { .. }) {
                    let view = broker.owner_control_view().map_err(transition_failure)?;
                    if target.is_some()
                        && view.selection.is_some()
                        && target
                            .as_ref()
                            .map(crate::broker::ExchangeControlSelection::id)
                            != view
                                .selection
                                .as_ref()
                                .map(crate::broker::ExchangeControlSelection::id)
                    {
                        return Err(TransitionFailure::Rejected(CommandFailure::ExchangeChanged));
                    }
                    let TransitionStage::Draining { ownership, .. } =
                        std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                    else {
                        unreachable!("a collected transition retains its admission ownership")
                    };
                    self.stage = TransitionStage::Ready {
                        _ownership: ownership,
                    };
                    return Ok(TransitionProgress::Finished {
                        outcome: TransitionOutcome::Reconfigured {
                            exchange: target
                                .filter(|target| {
                                    view.selection
                                        .as_ref()
                                        .is_some_and(|selected| selected.id() == target.id())
                                })
                                .and_then(
                                    crate::broker::ExchangeControlSelection::into_continuation,
                                ),
                        },
                        events: view.events,
                    });
                }
                let (prior, events) = broker
                    .owner_cancel_local(
                        target
                            .as_ref()
                            .map(crate::broker::ExchangeControlSelection::id),
                    )
                    .map_err(transition_failure)?;
                let completed = if prior.is_none() {
                    Some(
                        broker
                            .owner_cancellation_outcome(
                                target
                                    .as_ref()
                                    .map(crate::broker::ExchangeControlSelection::id),
                            )
                            .map_err(transition_failure)?,
                    )
                } else {
                    None
                };
                let TransitionStage::Draining { ownership, .. } =
                    std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                else {
                    unreachable!("local cancellation retained its ownership through commit")
                };
                match prior {
                    Some(prior) => {
                        self.stage = TransitionStage::Settling {
                            target,
                            ownership,
                            prior: Box::new(prior),
                            job: SettlementJob::Advance,
                        }
                    }
                    None => {
                        self.stage = TransitionStage::Ready {
                            _ownership: ownership,
                        };
                        return Ok(TransitionProgress::Finished {
                            outcome: TransitionOutcome::Cancelled(
                                completed
                                    .expect("no prior work has a committed cancellation result"),
                            ),
                            events,
                        });
                    }
                }
                Ok(TransitionProgress::Waiting {
                    events,
                    input: None,
                })
            }
            event @ (TransitionEvent::AdvanceSettlement
            | TransitionEvent::Observed(_)
            | TransitionEvent::Initial(_)
            | TransitionEvent::Final(_)) => {
                let TransitionStage::Settling {
                    target, prior, job, ..
                } = &mut self.stage
                else {
                    return Err(TransitionFailure::Uncertain);
                };
                *job = SettlementJob::Advance;
                let mut events = Vec::new();
                let action = match event {
                    TransitionEvent::Initial(result) => broker
                        .apply_prior_initial(prior, result)
                        .and_then(|()| broker.advance_prior_finalization(prior, &mut events)),
                    TransitionEvent::Final(result) => {
                        broker.apply_prior_final(prior, result.map(|result| *result), &mut events)
                    }
                    TransitionEvent::Observed(result) => {
                        result.and_then(|()| broker.advance_prior_finalization(prior, &mut events))
                    }
                    TransitionEvent::AdvanceSettlement => {
                        broker.advance_prior_finalization(prior, &mut events)
                    }
                    TransitionEvent::Acquired(_)
                    | TransitionEvent::Collected(_)
                    | TransitionEvent::CollectionPending
                    | TransitionEvent::CollectionDeadline
                    | TransitionEvent::ForegroundSettled
                    | TransitionEvent::ForegroundUnavailable => {
                        unreachable!("settlement event was selected exhaustively")
                    }
                };
                let action = match action {
                    Ok(action) => action,
                    Err(error) => return Err(self.abandon_settlement(broker, error)),
                };
                match action {
                    PriorFinalizationAction::Ready => {
                        let outcome = broker
                            .owner_cancellation_outcome(
                                target
                                    .as_ref()
                                    .map(crate::broker::ExchangeControlSelection::id),
                            )
                            .map_err(transition_failure)?;
                        let TransitionStage::Settling { ownership, .. } =
                            std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                        else {
                            unreachable!(
                                "settled cancellation retained its ownership through commit"
                            )
                        };
                        self.stage = TransitionStage::Ready {
                            _ownership: ownership,
                        };
                        Ok(TransitionProgress::Finished {
                            outcome: TransitionOutcome::Cancelled(outcome),
                            events,
                        })
                    }
                    PriorFinalizationAction::Wait(next) => {
                        *job = SettlementJob::Observation(tokio::spawn(next.run()));
                        Ok(TransitionProgress::Waiting {
                            events,
                            input: None,
                        })
                    }
                    PriorFinalizationAction::Initial(next) => {
                        *job = SettlementJob::Initial(tokio::spawn(next.run()));
                        Ok(TransitionProgress::Waiting {
                            events,
                            input: None,
                        })
                    }
                    PriorFinalizationAction::Final(next) => {
                        *job = SettlementJob::Final(tokio::spawn(next.run()));
                        Ok(TransitionProgress::Waiting {
                            events,
                            input: None,
                        })
                    }
                }
            }
        }
    }

    /// End a failed cancellation settlement and classify its failure for the command.
    ///
    /// The settlement holds the host-settlement fence of the exchange it finalizes. Leaving the
    /// fence after the failure made every later admission and cancel wait for a settlement that
    /// no owner would advance again.
    fn abandon_settlement(
        &mut self,
        broker: &mut HarnessBroker,
        error: anyhow::Error,
    ) -> TransitionFailure {
        match self.stage.abandon_settlement(broker, &error) {
            Ok(()) => transition_failure(error),
            Err(release) => TransitionFailure::Domain(error.context(release)),
        }
    }

    /// Transfer overdue collection without aborting native work or retaining a live resolver.
    pub(super) fn into_retained(self) -> RetainedTransition {
        RetainedTransition {
            stage: self.stage,
            intent: self.intent,
            collect_checks: !matches!(self.id, TransitionIdentity::Input(_)),
            child_operation: matches!(self.intent, StopIntent::Cancel(_)).then_some(self.id),
            jobs: Vec::new(),
            compaction_jobs: Vec::new(),
            compactions: Vec::new(),
            lost_compactions: Vec::new(),
            generation: self.generation,
            invocation: self.invocation,
            uncommitted: self.uncommitted,
        }
    }
}

/// Correlate shared collection with its actual owner rather than a fabricated command receipt.
#[derive(Clone)]
enum TransitionIdentity {
    Shutdown(uuid::Uuid),
    Command(CommandId),
    Input(InputRevision),
}

impl TransitionIdentity {
    /// Create a distinct subordinate resource-operation identity for the existing child coordinator.
    fn operation_id(&self) -> String {
        match self {
            Self::Command(id) => id.as_str().into(),
            Self::Shutdown(id) => format!("owner-shutdown:{id}"),
            Self::Input(input) => {
                format!("input-preparation:{}:{}", input.id.as_str(), input.revision)
            }
        }
    }
}

/// Preserve cleanup resources after a failed or timed-out command observation.
pub(super) struct RetainedTransition {
    stage: TransitionStage,
    intent: StopIntent,
    collect_checks: bool,
    child_operation: Option<TransitionIdentity>,
    jobs: Vec<tokio::task::JoinHandle<Result<(), ProviderError>>>,
    generation: Option<ProviderGenerationPermit>,
    invocation: Option<InvocationCollection>,
    compaction_jobs: Vec<(
        super::command::ContextCompactionId,
        tokio::task::JoinHandle<crate::broker::CompactionResult>,
    )>,
    compactions: Vec<crate::broker::CompactionResult>,
    lost_compactions: Vec<super::command::ContextCompactionId>,
    uncommitted: Option<TransitionEvent>,
}

impl RetainedTransition {
    /// Create recovery ownership for settings work that failed before exchange collection.
    pub(super) fn configuration(control: Option<OwnedSemaphorePermit>) -> Self {
        Self {
            stage: TransitionStage::Uncertain {
                _ownership: ControlOwnership {
                    control,
                    children: None,
                },
            },
            intent: StopIntent::Reconfigure {
                children: ChildCollectionPolicy::Preserve,
            },
            collect_checks: true,
            child_operation: None,
            jobs: Vec::new(),
            compaction_jobs: Vec::new(),
            compactions: Vec::new(),
            lost_compactions: Vec::new(),
            generation: None,
            invocation: None,
            uncommitted: None,
        }
    }

    /// Retain settings I/O until its native lane has settled, even after a command timeout.
    pub(super) fn retain_job(&mut self, job: tokio::task::JoinHandle<Result<(), ProviderError>>) {
        self.jobs.push(job);
    }

    /// Retain the actual compaction worker and the attempt whose history it must settle.
    pub(super) fn retain_compaction(
        &mut self,
        id: super::command::ContextCompactionId,
        task: tokio::task::JoinHandle<crate::broker::CompactionResult>,
    ) {
        self.compaction_jobs.push((id, task));
    }

    /// Retain a completed native fact after its host commit could not be certified.
    pub(super) fn retain_compaction_result(&mut self, result: crate::broker::CompactionResult) {
        self.compactions.push(result);
    }

    /// Retain the durable attempt whose worker result was lost.
    pub(super) fn retain_lost_compaction(&mut self, id: super::command::ContextCompactionId) {
        self.lost_compactions.push(id);
    }

    /// Commit late historical facts in the owner while preserving the command's already-recorded failure.
    pub(super) fn apply_compaction_facts(
        &mut self,
        broker: &mut HarnessBroker,
    ) -> Result<Vec<crate::broker::publication::SessionUpdate>, TransitionFailure> {
        let mut updates = Vec::new();
        while let Some(result) = self.compactions.last() {
            let (delivery, _historical_failure, _provenance) = broker
                .commit_owned_compaction(result.clone())
                .map_err(transition_failure)?;
            self.compactions.pop();
            updates.extend(delivery.into_updates());
        }
        while let Some(id) = self.lost_compactions.last() {
            let delivery = broker
                .owner_compaction_observation_lost(id)
                .map_err(transition_failure)?;
            self.lost_compactions.pop();
            updates.extend(delivery.into_updates());
        }
        Ok(updates)
    }

    /// Exclude provider replacement until retained configuration work and cleanup settle.
    pub(super) fn retain_generation(&mut self, generation: Option<ProviderGenerationPermit>) {
        self.generation = generation;
    }

    /// Collect an overdue native job, force-ending the binding when it never returns.
    ///
    /// Collection that is still pending `STOP_COLLECTION_BACKSTOP` after it was retained can no
    /// longer be waiting on a bounded adapter interrupt, so the session would otherwise stay
    /// stopping until the editor restarts. The backstop terminates the root binding's native
    /// process or connection, gives the retained work the same bound again to observe that, and
    /// reports a failed stop either way, which restarts the provider and settles the exchange.
    pub(super) async fn collect(&mut self, services: &OwnerServices) -> Result<(), ProviderError> {
        if let Ok(result) =
            tokio::time::timeout(STOP_COLLECTION_BACKSTOP, self.collect_native(services)).await
        {
            return result;
        }
        let _ = StopBackstop::new(services).terminate().await;
        let _ = tokio::time::timeout(STOP_COLLECTION_BACKSTOP, self.collect_native(services)).await;
        Err(abandoned_collection())
    }

    /// Collect an overdue native job while leaving lifecycle decisions with the driver.
    async fn collect_native(&mut self, services: &OwnerServices) -> Result<(), ProviderError> {
        let mut retained_failure = false;
        // A yielded join result has already consumed its handle; retain its evidence without awaiting it again.
        if let Some(event) = self.uncommitted.take() {
            match event {
                TransitionEvent::Acquired(acquired) => {
                    self.stage = match acquired {
                        Acquisition::Ready(ownership) | Acquisition::Uncertain(ownership) => {
                            TransitionStage::Uncertain {
                                _ownership: ownership,
                            }
                        }
                        Acquisition::Unavailable => TransitionStage::Consumed,
                        Acquisition::ObservationLost => TransitionStage::Uncertain {
                            _ownership: ControlOwnership {
                                control: None,
                                children: match self.intent.children() {
                                    ChildCollectionPolicy::Collect => {
                                        Some(services.children.begin_quiescence())
                                    }
                                    ChildCollectionPolicy::Preserve => None,
                                },
                            },
                        },
                    };
                }
                TransitionEvent::Collected(result) => {
                    retained_failure = result.is_err();
                    if let TransitionStage::Collecting {
                        target, ownership, ..
                    } = std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                    {
                        self.stage = TransitionStage::Draining { target, ownership };
                    }
                }
                event @ (TransitionEvent::Observed(_)
                | TransitionEvent::Initial(_)
                | TransitionEvent::Final(_)) => {
                    if let TransitionStage::Settling { job, .. } = &mut self.stage {
                        *job = SettlementJob::Advance;
                    }
                    self.uncommitted = Some(event);
                }
                TransitionEvent::CollectionPending
                | TransitionEvent::CollectionDeadline
                | TransitionEvent::ForegroundSettled
                | TransitionEvent::ForegroundUnavailable
                | TransitionEvent::AdvanceSettlement => {}
            }
        }
        while let Some(job) = self.jobs.last_mut() {
            let observed = job.await;
            self.jobs.pop();
            if !matches!(observed, Ok(Ok(()))) {
                retained_failure = true;
            }
        }
        while let Some((id, task)) = self.compaction_jobs.last_mut() {
            let result = task.await;
            let id = id.clone();
            self.compaction_jobs.pop();
            match result {
                Ok(result) => self.compactions.push(result),
                Err(_) => {
                    self.lost_compactions.push(id);
                    retained_failure = true;
                }
            }
        }
        if let TransitionStage::Acquiring(task) = &mut self.stage {
            let acquired = task.await.unwrap_or(Acquisition::ObservationLost);
            self.stage = match acquired {
                Acquisition::Ready(ownership) | Acquisition::Uncertain(ownership) => {
                    TransitionStage::Uncertain {
                        _ownership: ownership,
                    }
                }
                Acquisition::Unavailable => TransitionStage::Consumed,
                Acquisition::ObservationLost => TransitionStage::Uncertain {
                    _ownership: ControlOwnership {
                        control: None,
                        children: match self.intent.children() {
                            ChildCollectionPolicy::Collect => {
                                Some(services.children.begin_quiescence())
                            }
                            ChildCollectionPolicy::Preserve => None,
                        },
                    },
                },
            };
        }
        let collected = match &mut self.stage {
            TransitionStage::Collecting { task, .. } => {
                let result = task.await.unwrap_or(Err(ProviderError::DeliveryUncertain));
                let TransitionStage::Collecting {
                    target, ownership, ..
                } = std::mem::replace(&mut self.stage, TransitionStage::Consumed)
                else {
                    unreachable!(
                        "joined collection still retains its original target and ownership"
                    )
                };
                self.stage = TransitionStage::Draining { target, ownership };
                result
            }
            TransitionStage::Uncertain { .. }
            | TransitionStage::Draining { .. }
            | TransitionStage::Ready { .. }
            | TransitionStage::Preparing { .. }
            | TransitionStage::Settling { .. }
            | TransitionStage::Consumed => {
                let check = ExchangeStop::collect_check(services, self.collect_checks)
                    .map_err(collection_failure)?;
                collect_resources(
                    services,
                    self.child_operation.as_ref(),
                    check,
                    self.intent.children(),
                    self.generation.clone(),
                    self.invocation.clone(),
                )
                .await
            }
            TransitionStage::Acquiring(_) => unreachable!("acquisition has already been joined"),
        };
        match (retained_failure, collected) {
            (false, result) => result,
            (true, Ok(())) => Err(ProviderError::DeliveryUncertain),
            (true, Err(error)) => Err(ProviderError::Combined {
                primary: Box::new(ProviderError::DeliveryUncertain),
                secondary: vec![error],
            }),
        }
    }

    /// Report actual retained collection to the same foreground reducer as normal completion.
    pub(super) fn collection_completed_signal(&self) -> crate::broker::ForegroundSignal {
        collection_completed_signal(self.intent, self.invocation.as_ref())
    }

    /// Join already-started checkpoint/observation work after host interruption is requested.
    pub(super) async fn drain_host(&mut self) {
        if let TransitionStage::Settling { job, .. } = &mut self.stage {
            match job {
                SettlementJob::Advance => {}
                SettlementJob::Observation(task) => {
                    let _ = task.await;
                }
                SettlementJob::Initial(task) => {
                    let _ = task.await;
                }
                SettlementJob::Final(task) => {
                    let _ = task.await;
                }
            }
            *job = SettlementJob::Advance;
        }
    }

    /// Release the prior finalization of a settlement that its failed or overdue command left.
    ///
    /// Call this after `drain_host` joined the settlement's job. No owner advances a retained
    /// settlement again, so its host-settlement fence is released here and the exchange stays
    /// finalizing with the recorded error, for the next admission to retry.
    pub(super) fn abandon_settlement(&mut self, broker: &mut HarnessBroker) -> anyhow::Result<()> {
        self.stage.abandon_settlement(
            broker,
            &anyhow::anyhow!("cancellation settlement ended before it finalized the exchange"),
        )
    }

    /// Release compatibility admission and child quiescence after all owned host jobs have joined.
    ///
    /// The driver keeps a retained transition for the rest of the session as evidence, so any
    /// guard left here outlives the stop. A held child quiescence parks every later root turn at
    /// completion behind children that can no longer start. Root execution stays fenced by the
    /// driver's execution state until the stop reopens it or a provider restart replaces it.
    pub(super) fn release_legacy_control(&mut self) {
        self.generation.take();
        let ownership = match &mut self.stage {
            TransitionStage::Preparing { ownership }
            | TransitionStage::Collecting { ownership, .. }
            | TransitionStage::Draining { ownership, .. }
            | TransitionStage::Settling { ownership, .. }
            | TransitionStage::Uncertain {
                _ownership: ownership,
            }
            | TransitionStage::Ready {
                _ownership: ownership,
            } => ownership,
            TransitionStage::Acquiring(_) | TransitionStage::Consumed => return,
        };
        ownership.control.take();
        ownership.children.take();
    }

    /// Apply late collection evidence without rewriting the historical failed command result.
    pub(super) fn apply_collected(
        &self,
        broker: &mut HarnessBroker,
    ) -> Result<Vec<forge_protocol::message::SessionEvent>, TransitionFailure> {
        let target = match &self.stage {
            TransitionStage::Collecting { target, .. }
            | TransitionStage::Draining { target, .. }
            | TransitionStage::Settling { target, .. } => target.as_ref(),
            TransitionStage::Acquiring(_)
            | TransitionStage::Preparing { .. }
            | TransitionStage::Uncertain { .. }
            | TransitionStage::Ready { .. }
            | TransitionStage::Consumed => None,
        };
        let events = broker
            .owner_collection_completed(
                target.map(crate::broker::ExchangeControlSelection::id),
                self.intent.collected_status(false),
            )
            .map_err(transition_failure)?;
        Ok(events)
    }
}

/// Distinguish permission/collection waiting from committed cancellation completion.
pub(super) enum TransitionProgress {
    Waiting {
        events: Vec<forge_protocol::message::SessionEvent>,
        input: Option<Box<crate::input::InputUpdate>>,
    },
    Finished {
        outcome: TransitionOutcome,
        events: Vec<forge_protocol::message::SessionEvent>,
    },
}

/// Preserve collection uncertainty separately from a failure that is safe to record normally.
#[derive(Debug, thiserror::Error)]
pub(super) enum TransitionFailure {
    #[error(transparent)]
    Rejected(CommandFailure),
    #[error("native collection exceeded its deadline and remains owned")]
    Overdue,
    #[error("execution collection remains uncertain")]
    Uncertain,
    #[error("cancellation state could not be committed: {0}")]
    Domain(#[source] anyhow::Error),
}

/// Carry one fact from a retained cancellation effect to the owner update step.
pub(super) enum TransitionEvent {
    /// Outcome of the identified compatibility/child ownership acquisition.
    Acquired(Acquisition),
    /// Completed native, approval, check, and child collection attempt.
    Collected(Result<(), ProviderError>),
    /// Publish one nonterminal notice without releasing cleanup ownership.
    CollectionPending,
    /// The owned job remains retained after its observation deadline elapsed.
    CollectionDeadline,
    /// The driver has no remaining foreground transaction or prepared work.
    ForegroundSettled,
    /// Another failed collection already fenced foreground execution.
    ForegroundUnavailable,
    /// A local finalization stage is ready for the next owner update.
    AdvanceSettlement,
    /// An identified observation wait ended while checkpoint ownership stayed retained.
    Observed(anyhow::Result<()>),
    /// Initial checkpoint work ended without mutating broker state in its worker.
    Initial(anyhow::Result<crate::checkpoint::CheckpointRecord>),
    /// Final checkpoint work ended without mutating broker state in its worker.
    Final(anyhow::Result<Box<crate::broker::FinalCheckpointResult>>),
}

/// Distinguish acquired resource guards from failed prerequisite collection.
pub(super) enum Acquisition {
    /// Compatibility control and child quiescence were acquired successfully.
    Ready(ControlOwnership),
    /// A restart could not collect its children while acquiring control.
    Uncertain(ControlOwnership),
    /// Control admission ended before collection began.
    Unavailable,
    /// The acquisition worker's result was lost.
    ObservationLost,
}

/// Keep one command's response clock separate from the lifetime of its cleanup workers.
struct TransitionReporting {
    admitted_at: tokio::time::Instant,
    policy: Option<forge_provider_api::CleanupPolicy>,
    pending_published: bool,
    response_expired: bool,
}

impl TransitionReporting {
    /// Capture the operation policy once before any acquisition or collection work starts.
    fn new(id: &TransitionIdentity, intent: StopIntent, admitted_at: tokio::time::Instant) -> Self {
        let policy = match (id, intent) {
            (TransitionIdentity::Shutdown(_), _) => Some(forge_provider_api::CleanupPolicy::Close),
            (_, StopIntent::Cancel(_)) => Some(forge_provider_api::CleanupPolicy::Cancellation),
            (_, StopIntent::Reconfigure { .. }) => None,
        };
        Self {
            admitted_at,
            policy,
            pending_published: false,
            response_expired: false,
        }
    }

    /// Get the original response boundary without resetting it at a phase transition.
    fn response_at(&self) -> tokio::time::Instant {
        self.admitted_at
            + self
                .policy
                .map_or(std::time::Duration::from_secs(30), |policy| {
                    policy.response_after()
                })
    }

    /// Get the next unpublished report while preserving the command's original deadline.
    fn next_at(&self) -> tokio::time::Instant {
        match self.policy {
            Some(policy) if !self.pending_published => self.admitted_at + policy.pending_after(),
            Some(_) | None => self.response_at(),
        }
    }

    /// Observe the elapsed reporting boundary without deciding native settlement.
    fn event(&self) -> TransitionEvent {
        if tokio::time::Instant::now() >= self.response_at() {
            return TransitionEvent::CollectionDeadline;
        }
        TransitionEvent::CollectionPending
    }
}

enum TransitionStage {
    Acquiring(tokio::task::JoinHandle<Acquisition>),
    Preparing {
        ownership: ControlOwnership,
    },
    Collecting {
        target: Option<crate::broker::ExchangeControlSelection>,
        ownership: ControlOwnership,
        task: tokio::task::JoinHandle<Result<(), ProviderError>>,
    },
    Draining {
        target: Option<crate::broker::ExchangeControlSelection>,
        ownership: ControlOwnership,
    },
    Settling {
        target: Option<crate::broker::ExchangeControlSelection>,
        ownership: ControlOwnership,
        prior: Box<PriorFinalization>,
        job: SettlementJob,
    },
    Ready {
        _ownership: ControlOwnership,
    },
    Uncertain {
        _ownership: ControlOwnership,
    },
    Consumed,
}

impl TransitionStage {
    /// Release a settlement's prior finalization, keeping its ownership as uncertain evidence.
    ///
    /// Any other stage holds no finalization fence and is left unchanged.
    fn abandon_settlement(
        &mut self,
        broker: &mut HarnessBroker,
        error: &anyhow::Error,
    ) -> anyhow::Result<()> {
        if !matches!(self, Self::Settling { .. }) {
            return Ok(());
        }
        let Self::Settling {
            ownership,
            mut prior,
            ..
        } = std::mem::replace(self, Self::Consumed)
        else {
            unreachable!("the settlement stage was matched above")
        };
        *self = Self::Uncertain {
            _ownership: ownership,
        };
        broker.abort_prior_finalization(&mut prior, error)
    }
}

enum SettlementJob {
    Advance,
    Observation(tokio::task::JoinHandle<anyhow::Result<()>>),
    Initial(tokio::task::JoinHandle<anyhow::Result<crate::checkpoint::CheckpointRecord>>),
    Final(tokio::task::JoinHandle<anyhow::Result<crate::broker::FinalCheckpointResult>>),
}

impl TransitionEvent {
    /// Identify lifecycle evidence independently of its diagnostic label.
    pub(super) fn core_transition(
        &self,
    ) -> Option<(
        super::core_log::CoreTransitionKind,
        super::core_log::CoreOutcome,
    )> {
        use super::core_log::{CoreOutcome, CoreTransitionKind};
        match self {
            Self::Acquired(Acquisition::Ready(_)) => {
                Some((CoreTransitionKind::CollectionStarted, CoreOutcome::Pending))
            }
            Self::Collected(Ok(())) => {
                Some((CoreTransitionKind::Collected, CoreOutcome::Succeeded))
            }
            Self::Collected(Err(_)) | Self::ForegroundUnavailable => Some((
                CoreTransitionKind::CollectionFailed,
                CoreOutcome::ObservationLost,
            )),
            Self::CollectionPending => Some((
                CoreTransitionKind::CancellationPending,
                CoreOutcome::Pending,
            )),
            Self::CollectionDeadline => Some((
                CoreTransitionKind::CancellationTimedOut,
                CoreOutcome::TimedOut,
            )),
            Self::ForegroundSettled => Some((
                CoreTransitionKind::ForegroundSettled,
                CoreOutcome::Succeeded,
            )),
            Self::Acquired(
                Acquisition::Unavailable | Acquisition::ObservationLost | Acquisition::Uncertain(_),
            )
            | Self::AdvanceSettlement
            | Self::Observed(_)
            | Self::Initial(_)
            | Self::Final(_) => None,
        }
    }

    /// Get the finite log label without exposing provider or caller payloads.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Acquired(_) => "ownership_acquired",
            Self::Collected(_) => "native_collected",
            Self::CollectionPending => "collection_pending",
            Self::CollectionDeadline => "collection_deadline",
            Self::ForegroundSettled => "foreground_settled",
            Self::ForegroundUnavailable => "foreground_unavailable",
            Self::AdvanceSettlement => "advance_settlement",
            Self::Observed(_) => "settlement_observed",
            Self::Initial(_) => "initial_checkpoint_collected",
            Self::Final(_) => "final_checkpoint_collected",
        }
    }
}

/// Carry exact native withdrawal evidence with the foreground collection outcome.
fn collection_completed_signal(
    intent: StopIntent,
    invocation: Option<&InvocationCollection>,
) -> crate::broker::ForegroundSignal {
    let withdrawn = invocation.and_then(|invocation| {
        let proof = invocation.collection.lock().ok()?;
        proof.as_ref()?.withdrawn.clone()
    });
    let disposition = match intent {
        StopIntent::Cancel(_) => crate::broker::ForegroundCollection::Cancel(intent.input()),
        StopIntent::Reconfigure { .. } => crate::broker::ForegroundCollection::Restart,
    };
    crate::broker::ForegroundSignal::CollectionCompleted(disposition, withdrawn)
}

/// Collect native callbacks, child work, and the selected check without mutable broker access.
async fn collect_resources(
    services: &OwnerServices,
    child_operation: Option<&TransitionIdentity>,
    check: Option<String>,
    child_policy: ChildCollectionPolicy,
    generation: Option<ProviderGenerationPermit>,
    invocation: Option<InvocationCollection>,
) -> Result<(), ProviderError> {
    let generation = match generation {
        Some(generation) => generation,
        None => services.observe_provider_generation().await,
    };
    let _generation = generation;
    let session = services.observer.session_id();
    let owner = crate::provider::ThreadOwner::root(session);
    let approvals = services
        .permissions
        .cancel_owner(&owner, None)
        .await
        .map_err(collection_failure);
    let native = async {
        let Some(invocation) = invocation else {
            return Ok(());
        };
        let collection = invocation
            .engine
            .collect_exchange(invocation.scope, invocation.input)
            .await?;
        *invocation
            .collection
            .lock()
            .map_err(|_| ProviderError::DeliveryUncertain)? = Some(collection);
        Ok(())
    };
    let checks = async {
        if let Some(check) = check {
            loop {
                let run = services.checks.status(&check, 60_000, true).await?;
                if run.state != crate::checks::CheckState::Running {
                    break;
                }
            }
        }
        Ok::<(), anyhow::Error>(())
    };
    let children = async {
        if let Some(id) = child_operation {
            interrupt_children(&services.children, id).await?;
        } else if matches!(child_policy, ChildCollectionPolicy::Collect) {
            let mut changed = services.children.activity();
            children_settled(&services.children, &mut changed).await?;
        }
        Ok::<(), anyhow::Error>(())
    };
    // Keep the root interrupt independent of the child command lane and check cleanup.
    let (native, checks, children) = tokio::join!(native, checks, children);
    let mut errors = Vec::new();
    for outcome in [
        approvals,
        native,
        checks.map_err(collection_failure),
        children.map_err(collection_failure),
    ] {
        if let Err(error) = outcome {
            errors.push(error);
        }
    }
    let Some(primary) = errors.pop() else {
        return Ok(());
    };
    if errors.is_empty() {
        return Err(primary);
    }
    Err(ProviderError::Combined {
        primary: Box::new(primary),
        secondary: errors,
    })
}

/// Interrupt queued and active child work and wait for the coordinator's settlement evidence.
async fn interrupt_children(
    children: &crate::agent::AgentCoordinator,
    id: &TransitionIdentity,
) -> anyhow::Result<()> {
    if !children.collection_count().has_work() {
        return Ok(());
    }
    let mut changed = children.activity();
    children
        .sender()
        .request(crate::agent::AgentRequest {
            operation_id: format!("{}:collect-children", id.operation_id()),
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::InterruptAll,
        })
        .await?;
    children_settled(children, &mut changed).await
}

/// Wait until every child execution settled, failing once any settled with failed cleanup.
///
/// A child whose cleanup failed keeps its execution slot for the retained cleanup obligation, and
/// no later event removes it, so waiting for the active count to reach zero never ended. Its
/// failure instead makes the collection uncertain, which restarts the provider.
async fn children_settled(
    children: &crate::agent::AgentCoordinator,
    changed: &mut tokio::sync::watch::Receiver<u64>,
) -> anyhow::Result<()> {
    loop {
        let count = children.collection_count();
        if count.running == 0 && count.queued == 0 {
            anyhow::ensure!(
                count.cleanup_failed == 0,
                "{} child agent cleanup failed during collection",
                count.cleanup_failed
            );
            return Ok(());
        }
        changed.changed().await?;
    }
}

/// Normalize an existing host-effect error at the generic collection boundary.
fn collection_failure(error: anyhow::Error) -> ProviderError {
    let error = crate::provider::lifecycle_error::classify(
        forge_provider_api::ProviderOperation::TurnInterrupt,
        error,
    );
    if let ProviderError::Failure(failure) = error {
        return ProviderError::Failure(failure.at_phase(
            forge_provider_api::ProviderPhase::Collection,
            forge_provider_api::FailureDisposition::Uncertain,
        ));
    }
    error
}

/// Keep native observation loss distinct from an uncommitted host-state write.
fn transition_failure(error: anyhow::Error) -> TransitionFailure {
    if error.downcast_ref::<ProviderError>().is_some() {
        TransitionFailure::Uncertain
    } else {
        TransitionFailure::Domain(error)
    }
}

#[cfg(test)]
mod tests {
    use crate::agent::{
        AgentCommit, AgentCoordinator, AgentOperation, AgentRequest, AgentSelection,
        AgentSettlementDisposition, AgentSource, AgentWork, SpawnAgentRequest,
    };

    /// Admit and launch one child task in a coordinator with no other work.
    fn launched_child(coordinator: &AgentCoordinator) -> AgentWork {
        let spawn = SpawnAgentRequest {
            profile: Some("review".into()),
            access: None,
            text: "bounded task".into(),
        };
        let request = AgentRequest {
            operation_id: "collected-child".into(),
            source: AgentSource::User,
            operation: AgentOperation::Spawn(spawn.clone()),
        };
        let selection = AgentSelection {
            profile: "worker".into(),
            instructions: String::new(),
            provider: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            model: "model".into(),
            reasoning_effort: "low".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
        };
        let admission = coordinator
            .reserve_spawn(&request, &spawn, selection, "parent", 1)
            .expect("an idle coordinator admits one child");
        match admission.commit().expect("the child admission commits") {
            AgentCommit::Admitted(work) => work,
            AgentCommit::Existing(_) => panic!("a new request reused an admission"),
        }
    }

    /// End child collection once the only child settled with failed cleanup.
    ///
    /// The failed cleanup keeps the child's execution slot, so a collection that waited for the
    /// active count to reach zero never ended, and a root stop waited on it forever.
    #[tokio::test(start_paused = true)]
    async fn child_collection_ends_when_child_cleanup_failed() {
        let coordinator = AgentCoordinator::new("root".into());
        let child = launched_child(&coordinator);
        assert_eq!(coordinator.active_count(), 1);
        let mut changed = coordinator.activity();
        let collection = super::children_settled(&coordinator, &mut changed);
        tokio::pin!(collection);
        assert!(futures_util::poll!(collection.as_mut()).is_pending());
        coordinator
            .settled(
                &child.agent.id,
                &child.task.id,
                AgentSettlementDisposition::CleanupFailed,
            )
            .expect("the running child settles");
        assert_eq!(
            coordinator.active_count(),
            1,
            "failed cleanup keeps the slot"
        );
        let settled = tokio::time::timeout(std::time::Duration::from_secs(60), collection)
            .await
            .expect("collection waited on a child whose cleanup failed");
        assert!(
            settled.is_err(),
            "failed child cleanup must leave collection uncertain"
        );
    }

    /// Collect a queued launch that holds no execution slot.
    ///
    /// Collection previously skipped the interrupt whenever no slot was occupied, so admitted
    /// work waiting for a launch outlived the cancellation that should have settled it.
    #[tokio::test(start_paused = true)]
    async fn cancel_collects_pending_children_without_execution_slots() {
        let coordinator = AgentCoordinator::new("root".into());
        let spawn = SpawnAgentRequest {
            profile: Some("review".into()),
            access: None,
            text: "bounded task".into(),
        };
        let request = AgentRequest {
            operation_id: "queued-child".into(),
            source: AgentSource::User,
            operation: AgentOperation::Spawn(spawn.clone()),
        };
        let selection = AgentSelection {
            profile: "worker".into(),
            instructions: String::new(),
            provider: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            model: "model".into(),
            reasoning_effort: "low".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
        };
        let admission = coordinator
            .reserve_spawn(&request, &spawn, selection, "parent", 1)
            .expect("an idle coordinator admits one child");
        // A launch fence that begins between reservation and commit queues the task instead.
        let quiescence = coordinator.begin_quiescence();
        let queued = match admission.commit().expect("the child admission commits") {
            AgentCommit::Admitted(work) => work,
            AgentCommit::Existing(_) => panic!("a new request reused an admission"),
        };
        assert!(!queued.launch);
        assert_eq!(coordinator.active_count(), 0);
        assert!(coordinator.collection_count().has_work());
        let mut changed = coordinator.activity();
        let collection = super::children_settled(&coordinator, &mut changed);
        tokio::pin!(collection);
        assert!(
            futures_util::poll!(collection.as_mut()).is_pending(),
            "queued work keeps collection open"
        );
        let cancelled = coordinator
            .interrupt(&queued.agent.id, crate::agent::AgentSource::User)
            .expect("interrupt drains the queued launch");
        assert_eq!(cancelled.len(), 1);
        let mut task = cancelled.into_iter().next().unwrap();
        task.state = crate::exchange::ExchangeState::Cancelled;
        task.completed_at_ms = Some(2);
        coordinator
            .retain(queued.agent.clone(), task)
            .expect("the cancellation is retained");
        tokio::time::timeout(std::time::Duration::from_secs(60), collection)
            .await
            .expect("collection ends once the queued launch is cancelled")
            .expect("cancelled queued work settles cleanly");
        assert!(!coordinator.collection_count().has_work());
        drop(quiescence);
        assert!(coordinator.ready_launch().unwrap().is_none());
    }
}
