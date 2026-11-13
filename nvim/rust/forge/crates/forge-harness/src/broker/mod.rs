use crate::provider::ScopeOwner;
mod continuation_policy;
mod observed_agents;
use crate::provider::session_execution::{InputPresentation, OwnerObservation};
use crate::provider::{ChildWaitEvent, HostEvent, PermissionApprovalEvent};
use forge_provider_api::ProviderEvent;
mod owned_task_checks;
pub(crate) use owned_task_checks::{OwnedTaskCheckRun, TaskCheckEvent, TaskCheckProgress};
mod clarification;
mod rollback_effect;
pub(crate) use rollback_effect::RollbackEffectResult;
mod acceptance;
pub(crate) use acceptance::{PlanExecutionPreparation, ValidatedPlanExecution};
/// Derive the name an unnamed session takes from its first exchange's title.
///
/// A leading slash command is dropped so `/plan Add caching` names the session "Add caching".
pub(crate) fn automatic_session_name(session_title: &str) -> String {
    let words = session_title.split_whitespace().collect::<Vec<_>>();
    let words = if words.first().is_some_and(|word| word.starts_with('/')) {
        &words[1..]
    } else {
        &words[..]
    };
    words.join(" ").chars().take(30).collect()
}

#[cfg(test)]
mod cancellation_test;
#[cfg(test)]
mod change_results_test;
mod checkpoint_settlement;
mod checks;
#[cfg(test)]
mod model_label_test;
pub(crate) use checkpoint_settlement::FinalCheckpointCommit;
mod compaction;
mod configuration;
pub(crate) use compaction::{CompactionEvidenceError, CompactionProvenance, CompactionResult};
pub(crate) use configuration::{
    ConfigurationCandidate, ConfigurationCatalogResult, ConfigurationEffect,
    ConfigurationPreparation, ConfigurationReconcileJob, ConfigurationReconcileResult,
    PendingConfigurationRetry, PolicyBaseline,
};
mod execution;
mod execution_progress;
mod foreground_completion;
mod foreground_failure;
mod foreground_finalization;
mod foreground_goal_effect;
mod foreground_local;
mod foreground_plan_effect;
mod foreground_undo;
mod fork;
mod interaction;
mod live_goal_effect;
mod live_host_effect;
mod prior_finalization;
pub(crate) mod session_message;
pub(crate) use prior_finalization::PriorFinalizationAction;
mod foreground_dispatch;
mod foreground_start;
mod foreground_step;
mod foreground_workspace;
use foreground_dispatch::ForegroundAdmission;
pub(crate) use foreground_dispatch::{DispatchStage, TurnTransaction};
pub mod publication;
use publication::{ConfigurationProgressState, SessionUpdate, TimelineUpdate};
mod check_admission;
mod check_completion;
mod owned_check_completion;
pub(crate) use owned_check_completion::{
    OwnedCheckCompletion, OwnedCheckProgress, OwnedCheckResult,
};
mod new_session;
mod observation;
mod owner_control;
pub(crate) use checkpoint_settlement::FinalCheckpointResult;
pub(crate) use owner_control::{ExchangeControlSelection, ExchangeControlView};
pub(crate) use prior_finalization::PriorFinalization;
mod plan_checks_update;
pub(crate) mod plan_entry;
mod project_checks;
pub(crate) use check_admission::AutomaticCheckAction;
mod host_settlement;
mod review_question;
pub(crate) use observation::BrokerObserver;

pub use fork::{ForkOptions, ForkPreparation, prepare_provider_fork};
pub use new_session::prepare_new_session;

use crate::agent::{Agent, AgentRegistry, AgentState};
use crate::buffer::session::SessionPresentation;
use crate::checkpoint::{GitCheckpoint, checkpoint_diff, checkpoint_diff_for_paths};
use crate::exchange::{
    ActiveWait, ChecklistTracker, Exchange, ExchangeComment, ExchangeNode, ExchangeState,
    ProviderChangeIndex,
};
use crate::goal::{ContinuationDecision, GoalRecord, GoalState};
use crate::permissions::store::PermissionStore;
use crate::plan::state_machine::{PlanEvent, PlanStateMachine};
use crate::plan::{
    ArtifactSummary, PlanElicitation, PlanExecutionLifecycleEvent, PlanExecutionPromptKind,
    PlanExecutionRecord, PlanExecutionState, PlanFileStore, PlanLifecycleKind, PlanLifecycleRecord,
    PlanPrompt, PlanQuestionAnswer, PlanQuestionResponse, PlanQuestionSet, PlanQuestionWithdrawal,
    PlanRecord, PlanReview, PlanState,
};
use crate::protocol::RuntimeMethod;
use crate::provider::approval::{ApprovalRequestView, PermissionCoordinator};
use crate::provider::{
    HarnessEvent, HarnessEventSink, PromptMode, Provider, ProviderCapability,
    ProviderCatalogRequest, ProviderCompletion, ProviderError, ProviderId, ProviderInput,
    ProviderLaunch, ProviderRequest,
};
use crate::session::{
    ApprovalMode, ContextUsage, HarnessPreference, HarnessSession, ModelSetting, PlanExecutor,
    ProviderForkState, SessionStore,
};
use crate::storage::SqliteStore;
use crate::timeline::stream::TimelinePatch;
use crate::timeline::{
    SessionEventKind, SessionEventRecord, TimelineEntry, TimelineProjection, TimelineProjector,
};
use crate::trace::TraceStore;
use crate::workspace::WorkspaceKind;
use anyhow::{Context, Result};
use forge_protocol::message::{Request, Response, SessionEvent};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{
    Arc, RwLock,
    atomic::{AtomicBool, AtomicU64, Ordering},
};
use std::time::{SystemTime, UNIX_EPOCH};
#[cfg(test)]
use tokio::sync::Notify;
use uuid::Uuid;

/// Defines wall-clock access consumed by durable timelines.
pub trait Clock: Send + Sync {
    /// Resolve the current Unix time in milliseconds.
    fn now_ms(&self) -> i64;
}

/// Provides system wall-clock time for the production broker.
pub struct SystemClock;

impl Clock for SystemClock {
    fn now_ms(&self) -> i64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as i64
    }
}

/// Represents initialization state supplied by the Neovim client.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InitializeRequest {
    /// Creates or resumes only a tool-free Background conversation when present.
    #[serde(default)]
    pub background: Option<crate::background::BackgroundIdentity>,
    #[serde(default)]
    /// Resolves the dotfiles-owned global Rulesync catalog independently of workspace.
    pub config_dir: Option<String>,
    pub data_root: String,
    #[serde(default)]
    pub permission_file: Option<String>,
    pub workspace: String,
    pub client_id: String,
    #[serde(alias = "backend")]
    pub provider: ProviderLaunch,
    #[serde(default)]
    /// Supplies explicit launch descriptors for cross-provider children.
    #[serde(alias = "backend_launch_by_provider")]
    pub provider_launch_by_provider: HashMap<ProviderId, ProviderLaunch>,
    #[serde(default = "default_model")]
    pub model: String,
    #[serde(default = "default_effort")]
    pub effort: String,
    pub session_id: Option<String>,
    #[serde(default)]
    pub new_session_name: Option<String>,
    #[serde(default = "default_goal_max_turns")]
    pub goal_max_turns: u32,
    #[serde(default)]
    pub lease_conflict_action: Option<String>,
}

fn agent_summaries(
    runs: &[Agent],
    exchanges: &[Exchange],
) -> std::collections::HashMap<String, Value> {
    runs.iter().map(|run| {
        let owned: Vec<_> = exchanges.iter().filter(|exchange| exchange.agent_id == run.id).collect();
        let tool_count: usize = owned.iter().map(|exchange| exchange.tool_counter.total).sum();
        let failed_count: usize = owned.iter().map(|exchange| exchange.tool_counter.failed).sum();
        let latest = owned.iter().rev().find(|exchange| exchange.state() == ExchangeState::Running)
            .or_else(|| owned.last());
        let target = latest.and_then(|exchange| exchange.turn.iter().rev().find(|turn| matches!(turn.state(),crate::turn::TurnState::Running)))
            .map(|turn| serde_json::to_value(turn.provider()).expect("provider address"));
        (run.id.clone(), json!({"state":latest.map(|exchange| serde_json::to_value(exchange.state()).expect("exchange state")).unwrap_or_else(|| serde_json::to_value(run.state).expect("agent state")),
            "tool_count":tool_count,"failed_count":failed_count,"target":target,
            "exchange_count":owned.len(),"latest_exchange_id":latest.map(|exchange|exchange.id.as_str())}))
    }).collect()
}

fn preview_lines(timeline: &[TimelineEntry], now_ms: i64) -> Result<Vec<String>> {
    let mut display = crate::buffer::identity::DisplayRegistry::default();
    let width = forge_buffer::width::WidthProfile {
        columns: 100,
        ..Default::default()
    };
    let mut lines = Vec::new();
    for (index, entry) in timeline.iter().rev().take(20).rev().enumerate() {
        let projected = crate::buffer::projection::project(
            entry,
            &width,
            now_ms,
            index > 0,
            &Default::default(),
            &Default::default(),
            &mut display,
        )?;
        for block in crate::buffer::sections::preview(projected.entry)?.block {
            for row in block.text.wire_rows() {
                if lines.len() >= 200 {
                    return Ok(lines);
                }
                lines.push(row.chars().take(1000).collect());
            }
        }
    }
    Ok(lines)
}

/// Represents the complete client-visible state for one active session.
#[derive(Clone, Debug, Serialize)]
pub struct BrokerSnapshot {
    /// Latest activity committed by the session owner, independent of request callbacks.
    pub activity: crate::exchange::ActivitySnapshot,
    pub(crate) questions: crate::question::QuestionSnapshot,
    pub ask_model: crate::settings::AskModel,
    /// Saved access choices before provider capability limits are applied.
    pub access_preference: crate::session::AccessPolicy,
    /// Confirmed access label computed by the authoritative filesystem policy.
    pub access_abbreviation: String,
    pub subagent_tools: bool,
    pub lsp_tools: bool,
    pub session_tools: bool,
    pub status_thoughts: bool,
    pub check_editing: bool,
    pub runtime_epoch: String,
    pub snapshot_revision: u64,
    pub checks_running: bool,
    pub check_timing: crate::checks::CheckTiming,
    /// Effective completion-check enablement, independent of manual and plan checks.
    pub automatic_checks: bool,
    pub checks_deferred: bool,
    pub active_check_run: Option<String>,
    pub session: HarnessSession,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
    pub capability: ProviderCapability,
    pub no_checkpoint: bool,
    pub restore_recovery: Option<Value>,
    pub active_plan: Option<PlanRecord>,
    pub active_elicitation: Option<ActiveElicitation>,
    pub artifact: Vec<ArtifactSummary>,
    #[serde(skip)]
    pub timeline: Vec<TimelineEntry>,
    pub timeline_revision: u64,
    pub status: crate::session::state_machine::SessionPhase,
    pub active_wait: Option<ActiveWait>,
    pub prompt_history: Vec<crate::attachment::PromptDraft>,
    pub agent: AgentSnapshot,
    pub approval: Vec<crate::provider::ApprovalRequestPresentation>,
    pub observed_agents: Vec<crate::agent::ObservedAgent>,
}

/// Retains reusable child identities and their durable timelines inside one root.
#[derive(Clone, Debug, Serialize)]
pub struct AgentSnapshot {
    pub run: Vec<Agent>,
    pub summary: std::collections::HashMap<String, Value>,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
}

/// Represents a stored session timeline projected without acquiring its lease.
#[derive(Clone, Debug, Serialize)]
pub struct SessionPreview {
    pub lines: Vec<String>,
    pub session: HarnessSession,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
    #[serde(skip)]
    pub timeline: Vec<TimelineEntry>,
    pub agent: AgentSnapshot,
}

/// Represents the durable owner and question state presented by the Harness question UI.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ElicitationOwner {
    Plan,
    Interaction,
    #[serde(alias = "plan_acceptance")]
    PlanReview,
}

/// Represents the durable owner and question state presented by the Harness question UI.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveElicitation {
    pub owner: ElicitationOwner,
    pub plan_id: Option<String>,
    pub exchange_id: Option<String>,
    pub elicitation: PlanElicitation,
}

/// Preserve update ordering after durable state has been applied.
pub(crate) struct PendingLivePublication {
    /// Notification emitted before the timeline patch.
    pub(crate) update: SessionUpdate,
    /// Timeline patch captured after applying the event.
    pub(crate) timeline: TimelinePatch,
}

/// Separate durable provider event application from ordered publication.
pub(crate) enum AppliedProviderEvent {
    /// State application produced no publication.
    None,
    /// Publish one session update.
    Update(SessionUpdate),
    /// Publish an update followed by its timeline patch.
    Live(PendingLivePublication),
    /// Publish a timeline patch.
    Timeline(TimelinePatch),
    /// Publish multiple already-applied effects in their original order.
    Effects(Vec<AppliedProviderEvent>),
}

/// Own ordered presentation delivery independently of mutable broker state.
pub(crate) struct PendingProviderDelivery {
    sink: Option<publication::SessionUpdateSink>,
    updates: Vec<SessionUpdate>,
    events: Vec<SessionEvent>,
}

impl PendingProviderDelivery {
    /// Deliver queued updates in order and collect events for non-streaming callers.
    pub(crate) async fn deliver(self) -> Result<Vec<SessionEvent>> {
        if let Some(sink) = self.sink {
            for update in self.updates {
                sink.send_wait(update).await?;
            }
        }
        Ok(self.events)
    }

    /// Transfer committed update facts to the session owner instead of a separate forwarder.
    /// Buffered fallback events are derived from these updates and must not be sent a second time.
    pub(crate) fn into_updates(self) -> Vec<SessionUpdate> {
        self.updates
    }

    /// Wait until the external forwarder has consumed every update in this batch.
    pub(crate) async fn deliver_and_flush(self) -> Result<Vec<SessionEvent>> {
        let sink = self.sink.clone();
        let events = self.deliver().await?;
        if let Some(sink) = sink {
            sink.flush().await?;
        }
        Ok(events)
    }
}

pub(crate) struct ProviderSteering {
    pub(crate) active: crate::provider::ActiveSteering,
    pub(crate) deferred: std::collections::VecDeque<crate::provider::SteerCommand>,
    pub(crate) pending_deliveries: Arc<SteeringDeliveries>,
    pub(crate) refresh_readiness: bool,
}

/// Track steering work until its result is applied or its unlaunched owner is dropped.
#[derive(Default)]
pub(crate) struct SteeringDeliveries {
    count: std::sync::atomic::AtomicUsize,
    changed: tokio::sync::Notify,
}

impl SteeringDeliveries {
    /// Retain one effect and its eventual owner-applied result.
    fn retain(self: &Arc<Self>) -> SteeringDeliveryGuard {
        self.count.fetch_add(1, Ordering::AcqRel);
        SteeringDeliveryGuard(self.clone())
    }

    /// Check whether every retained effect has been applied or abandoned.
    fn is_empty(&self) -> bool {
        self.count.load(Ordering::Acquire) == 0
    }
}

/// Release steering ownership on every result, task-failure, and unlaunched-effect path.
pub(crate) struct SteeringDeliveryGuard(Arc<SteeringDeliveries>);

impl Drop for SteeringDeliveryGuard {
    fn drop(&mut self) {
        self.0.count.fetch_sub(1, Ordering::AcqRel);
        self.0.changed.notify_one();
    }
}

/// Own the mutable exchange and its runtime while the service waits for provider work.
pub(crate) struct OwnedForegroundTransaction {
    scope: crate::provider::InvocationScope,
    interaction: Exchange,
    runtime: Option<ExchangeRuntime>,
}

/// Distinguish native completion from a deliberate required-question handoff.
pub(crate) enum ForegroundOutcome {
    NativeReady(Result<crate::provider::ProviderCompletion, crate::provider::ProviderError>),
    RequiredQuestion {
        ready: Result<crate::provider::ProviderCompletion, crate::provider::ProviderError>,
        completion: Box<crate::provider::ProviderCompletion>,
    },
}

/// Own native waiting resources independently of the broker mutex.
pub(crate) struct ForegroundWait<T: Send + 'static> {
    events: crate::provider::events::HarnessEventStream,
    task: tokio::task::JoinSet<Result<T>>,
    pending_submission:
        Option<std::pin::Pin<Box<dyn std::future::Future<Output = Result<T>> + Send>>>,
    result_pending: bool,
    cancellation: Arc<TurnCancellationSignal>,
    watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
    /// Keep timeout collection with this foreground until the wait is dropped.
    _tool_owner: crate::provider::tool_timeout::ForegroundToolOwner,
    stream_open: bool,
}

/// Describe one event returned by the independently owned native wait.
pub(crate) enum NativeForegroundSignal<T> {
    Cancelled,
    ToolExpired(crate::provider::tool_timeout::ToolTimeout),
    ProviderEvent(OwnerObservation),
    NativeReady(T),
}

#[derive(Clone, Copy)]
enum NativeWaitMode {
    Running,
    Collecting,
}

impl<T: Send + 'static> ForegroundWait<T> {
    /// Retain an owned provider future until the owner authorizes its submission.
    pub(crate) fn start(
        events: crate::provider::events::HarnessEventStream,
        submission: impl std::future::Future<Output = Result<T>> + Send + 'static,
        cancellation: Arc<TurnCancellationSignal>,
        watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
    ) -> Result<Self> {
        Ok(Self {
            events,
            task: tokio::task::JoinSet::new(),
            pending_submission: Some(Box::pin(submission)),
            result_pending: false,
            cancellation,
            _tool_owner: crate::provider::tool_timeout::ForegroundToolOwner::attach(
                watchdog.clone(),
            )?,
            watchdog,
            stream_open: true,
        })
    }

    /// Continue the same invocation without replacing its event stream or watchdog.
    pub(crate) fn restart(
        &mut self,
        submission: impl std::future::Future<Output = Result<T>> + Send + 'static,
    ) -> Result<()> {
        anyhow::ensure!(
            self.task.is_empty() && self.pending_submission.is_none(),
            "previous provider attempt is still running or awaiting authorization"
        );
        self.pending_submission = Some(Box::pin(submission));
        self.result_pending = false;
        Ok(())
    }

    /// Retain observation of existing native work even after new submission authority is cancelled.
    pub(crate) fn restart_observation(
        &mut self,
        observation: impl std::future::Future<Output = Result<T>> + Send + 'static,
    ) -> Result<()> {
        anyhow::ensure!(
            self.task.is_empty() && self.pending_submission.is_none(),
            "previous provider attempt is still running or awaiting authorization"
        );
        // This future only observes an admitted invocation. Dropping it at the
        // submission gate would discard the checkout needed to commit cancellation.
        self.task.spawn(observation);
        self.result_pending = false;
        Ok(())
    }

    /// Launch only after the owner has applied this update's command transitions.
    pub(crate) fn authorize_submission(&mut self) {
        if let Some(submission) = self.pending_submission.take()
            && !self
                .cancellation
                .requested
                .load(std::sync::atomic::Ordering::Acquire)
        {
            self.task.spawn(submission);
        }
    }

    /// Report actual native work that must settle before synthesizing an undispatched cancellation.
    pub(crate) fn has_dispatched_task(&self) -> bool {
        !self.task.is_empty() || self.result_pending
    }

    /// Release the result-delivery obligation only when the broker processes the native result.
    pub(crate) fn acknowledge_result(&mut self) {
        self.result_pending = false;
    }

    /// Wait for native progress without retaining broker state or a presentation lock.
    pub(crate) async fn next(&mut self) -> Result<NativeForegroundSignal<T>> {
        self.next_in_mode(NativeWaitMode::Running).await
    }

    /// Drain an owner's interrupted attempt without repeating cancellation or timeout signals.
    pub(crate) async fn next_collected(&mut self) -> Result<NativeForegroundSignal<T>> {
        self.next_in_mode(NativeWaitMode::Collecting).await
    }

    async fn next_in_mode(&mut self, mode: NativeWaitMode) -> Result<NativeForegroundSignal<T>> {
        loop {
            tokio::select! {
                biased;
                () = self.cancellation.cancelled(), if matches!(mode, NativeWaitMode::Running) => return Ok(NativeForegroundSignal::Cancelled),
                expired = self.watchdog.expired(), if matches!(mode, NativeWaitMode::Running) => {
                    return Ok(NativeForegroundSignal::ToolExpired(expired?.context("tool watchdog ended during foreground execution")?));
                }
                event = self.events.recv(), if self.stream_open => match event {
                    Ok(Some(event)) => return Ok(NativeForegroundSignal::ProviderEvent(event)),
                    Ok(None) => self.stream_open = false,
                    Err(error) => return Err(error.into()),
                },
                result = self.task.join_next(), if !self.task.is_empty() => {
                    let result = result.context("provider task ended without an outcome")?.context("provider task failed")??;
                    self.result_pending = true;
                    return Ok(NativeForegroundSignal::NativeReady(result));
                }
            }
        }
    }
}

/// Own all resources polled during one foreground request outside the broker lock.
pub(crate) struct ForegroundTransaction {
    pub(crate) live_host_receiver: tokio::sync::mpsc::Receiver<crate::provider::LiveHostEffect>,
    pub(crate) live_host: Option<live_host_effect::LiveHostState>,
    pub(crate) state: OwnedForegroundTransaction,
    pub(crate) request: ProviderRequest,
    pub(crate) mode: PromptMode,
    pub(crate) settlement: Arc<crate::provider::session_execution::SettlementCell>,
    pub(crate) continuation_count: u32,
    pub(crate) dispatched_task_permit: Option<crate::task::TaskPermit>,
    pub(crate) dispatched_execution_id: Option<String>,
    pub(crate) dispatched_entry_id: Option<crate::task::TaskInputId>,
    pub(crate) _repository_lease: crate::tools::repository::RepositoryToolLease,
    pub(crate) _inspection_lease: crate::tools::session::InspectionLease,
    pub(crate) _lsp_lease: crate::tools::lsp::LspLease,
    pub(crate) native: ForegroundWait<ProviderAttempt>,
    pub(crate) native_open: bool,
    pub(crate) collection: Option<ForegroundCollection>,
    pub(crate) provider_event_sink: HarnessEventSink,
    pub(crate) engine: Arc<crate::provider::session_execution::SessionExecution>,
    pub(crate) steering: ProviderSteering,
    pub(crate) agent_revision: tokio::sync::watch::Receiver<u64>,
    pub(crate) _agent_foreground_claim: crate::agent::AgentForegroundClaim,
    pub(crate) question_changes: tokio::sync::watch::Receiver<u64>,
    pub(crate) execution_receiver:
        tokio::sync::mpsc::Receiver<crate::plan::execution::ExecutionControl>,
    pub(crate) pending_revision: Option<(
        tokio::sync::oneshot::Sender<Result<Value>>,
        crate::plan::revision_review::RevisionReviewWait,
    )>,
    pub(crate) task_authority: Option<crate::tools::TaskAuthority>,
    pub(crate) revision_job_active: bool,
    pub(crate) agent_command_active: bool,
    pub(crate) result_delivery: tokio::task::JoinSet<(crate::agent::AgentTask, Result<()>)>,
    pub(crate) attached_results: Vec<crate::agent::AgentTask>,
    pub(crate) attempted_results: std::collections::HashSet<String>,
    pub(crate) pending_agent_launch: std::collections::VecDeque<PendingAgentLaunch>,
    pub(crate) pending_agent_reply: Option<(tokio::sync::oneshot::Sender<Result<Value>>, Value)>,
    pub(crate) user_delivery: tokio::task::JoinSet<(crate::provider::SteerCommand, Result<()>)>,
    pub(crate) session_events: Arc<crate::provider::session_execution::SessionObservationInbox>,
    pub(crate) delivery_sink: Option<publication::SessionUpdateSink>,
    pub(crate) scan_interval: tokio::time::Interval,
    pub(crate) execution_id: Option<String>,
    pub(crate) question_enabled: bool,
    pub(crate) question_stop_pending: bool,
    pub(crate) question_override: Option<ProviderCompletion>,
    pub(crate) pending_native_ready: Option<Result<ProviderCompletion, ProviderError>>,
    /// Holds a deferred root result while unsettled children own the exchange. Only child
    /// activity, user input, or collection re-exposes it; immediate re-exposure spun the owner.
    pub(crate) native_ready_parked: bool,
    pub(crate) retraction_boundary: Option<(
        crate::provider::InvocationScope,
        forge_provider_api::CheckpointId,
    )>,
    pub(crate) scan_enabled: bool,
    pub(crate) execution_open: bool,
    pub(crate) _agent_lease: crate::agent::AgentToolLease,
    pub(crate) _check_lease: crate::checks::CheckToolLease,
    pub(crate) pending_session_observations: std::collections::VecDeque<OwnerObservation>,
    pub(crate) observation_publication_in_flight: bool,
    pub(crate) finalization: Option<Box<foreground_finalization::ForegroundFinalization>>,
    pub(crate) failure_cleanup_active: bool,
    pub(crate) failure_inbox_open: bool,
    pub(crate) failure: Option<foreground_failure::ForegroundFailure>,
    pub(crate) failure_secondary: Vec<String>,
}

/// Preserve the owner's collection disposition independently of provider cancellation flags.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ForegroundCollection {
    Cancel(crate::exchange::command::CancelledInputPolicy),
    Restart,
}

/// Describe a single owned foreground wait result for a short broker state step.
pub(crate) enum ForegroundSignal {
    LiveHost(Box<crate::provider::LiveHostEffect>),
    CollectionStarted(ForegroundCollection),
    /// The command response timed out; native cleanup retains ownership.
    CollectionTimedOut,
    CollectionCompleted(ForegroundCollection),
    /// The owner's native collection ended without proving the provider idle, so no
    /// `CollectionCompleted` follows and the owner restarts the provider.
    CollectionFailed,
    AdmissionWorkspaceCompleted(Box<foreground_workspace::AdmissionWorkspaceResult>),
    AdmissionRequested,
    AdmissionPrepared,
    FinalizeEffectsRequested,
    PublicationDrained,
    QuestionCatalog(Result<Vec<crate::provider::ProviderModel>>),
    ProviderPrepared(Result<Arc<crate::provider::session_execution::SessionExecution>>),
    Native(NativeForegroundSignal<ProviderAttempt>),
    QuestionChanged,
    QuestionStopped(Box<Result<ProviderCompletion>>),
    Checkpoint(Result<crate::checkpoint::CheckpointRecord>),
    UserInput(crate::provider::SteerCommand),
    UserDelivery((crate::provider::SteerCommand, Result<()>)),
    AgentChanged,
    AgentDelivery(Box<(crate::agent::AgentTask, Result<()>)>),
    AgentResultDelivered {
        /// Defers native completion until the owner records this delivery's acknowledgement.
        flight: SteeringDeliveryGuard,
        task: Box<crate::agent::AgentTask>,
        result: Result<()>,
    },
    AgentApprovalsCancelled {
        completion: Box<AgentCompletion>,
        result: Result<Vec<ApprovalRequestView>>,
    },
    AgentInterrupted(AgentInterrupt),
    AgentSpawnSelected {
        request: OwnedAgentRequest,
        selection: Box<Result<crate::agent::AgentSelection>>,
    },
    AgentMessageSteered {
        request: OwnedAgentRequest,
        registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
        owner: crate::provider::ThreadOwner,
        result: Result<()>,
    },
    ExecutionControl(crate::plan::execution::ExecutionControl),
    ExecutionControlEvidence {
        response: tokio::sync::oneshot::Sender<Result<serde_json::Value>>,
        submission: bool,
        result: Box<Result<execution::ExecutionControlEvidence>>,
    },
    ExecutionControlReport {
        response: tokio::sync::oneshot::Sender<Result<serde_json::Value>>,
        submission: bool,
        result: Box<Result<execution::ExecutionControlReport>>,
    },
    RevisionReview(crate::plan::revision_review::RevisionReviewRequest),
    PlanScan,
    PlanScanned(Box<Result<execution::ExecutionScanResult>>),
    FinalCheckpoint(Result<checkpoint_settlement::FinalCheckpointResult>),
    PlanMutationCompleted(Result<foreground_plan_effect::PlanMutationResult>),
    AutomaticCheckRefreshed(Box<Result<check_admission::AutomaticCheckRefreshResult>>),
    AutomaticCheckCompleted(Box<Result<check_admission::AutomaticCheckResult>>),
    SettlementReady {
        checkout: crate::provider::session_execution::SettlementCheckout,
        ready: Box<Result<crate::provider::session_execution::NativeReady, ProviderError>>,
    },
    SettlementCommitted(Result<(), crate::provider::session_execution::SettlementError>),
    SessionObservationsReady,
    PublicationFailed(std::io::Error),
    EffectFailed(anyhow::Error),
    CleanupCompleted {
        original: anyhow::Error,
        cleanup: Result<(), ProviderError>,
    },
    FailurePublicationCompleted(Result<Vec<SessionEvent>>),
    FailurePublicationNeedsForward {
        events: Vec<SessionEvent>,
    },
    FailureAdmissionEventsNeedForward {
        events: Vec<SessionEvent>,
    },
    FailureAdmissionEventsForwarded(Result<()>),
    AdmissionEventsNeedForward {
        events: Vec<SessionEvent>,
    },
    AdmissionEventsForwarded(Result<()>),
    CommandEventsNeedForward {
        events: Vec<SessionEvent>,
    },
    CommandEventsForwarded(Result<()>),
    AdmissionCleanupCompleted(foreground_failure::AdmissionCleanupResult),
    AdmissionFailureCheckpointCompleted(Result<checkpoint_settlement::FinalCheckpointResult>),
    FailureCheckpointCompleted(Result<checkpoint_settlement::FinalCheckpointResult>),
    FailureBaselineCompleted(foreground_failure::FailureBaselineResult),
    PlanSubmissionCompleted(Result<foreground_plan_effect::PlanSubmissionResult>),
    PlanQuestionCompleted(Result<foreground_plan_effect::PlanQuestionResult>),
    GoalProgressCompleted(Box<Result<foreground_goal_effect::GoalProgressResult>>),
    ExecutionProgressCompleted(Box<Result<foreground_goal_effect::ExecutionProgressResult>>),
    TaskCompacted(Box<Result<crate::provider::Compaction, crate::provider::ProviderError>>),
    CompactionCompleted(Box<compaction::CompactionResult>),
    FinalizeChecksRequested,
    FinalizeCheckContinuationRequested,
    FinalResponseNeedsForward {
        events: Vec<SessionEvent>,
    },
    FinalResponsePublished(Result<Vec<SessionEvent>>),
    ClarificationNeedsForward {
        events: Vec<SessionEvent>,
    },
    ClarificationPublished(Result<Vec<SessionEvent>>),
    CheckRepairNeedsForward {
        events: Vec<SessionEvent>,
    },
    CheckRepairPublished(Result<Vec<SessionEvent>>),
    PermissionNeedsForward {
        events: Vec<SessionEvent>,
    },
    PermissionPublished(Result<Vec<SessionEvent>>),
    CommandNeedsForward {
        events: Vec<SessionEvent>,
    },
    CommandPublished(Result<Vec<SessionEvent>>),
    AutoCompactionNeedsForward {
        events: Vec<SessionEvent>,
    },
    AutoCompactionPublished(Result<Vec<SessionEvent>>),
    ObservationPublicationDelivered(Result<Vec<SessionEvent>>),
    ObservationPublicationNeedsForward {
        events: Vec<SessionEvent>,
    },
    PublicationDelivered(Vec<SessionEvent>),
    PlanTerminalPublished(Result<Vec<SessionEvent>>),
    PlanTerminalNeedsForward {
        events: Vec<SessionEvent>,
        receipts: OwnedEffectReceipts,
    },
    PriorObservationReady(Result<()>),
    PriorInitialCompleted(Result<crate::checkpoint::CheckpointRecord>),
    PriorFinalCompleted(Result<checkpoint_settlement::FinalCheckpointResult>),
    UserDeliverySettled(
        Option<crate::provider::SteerCommand>,
        Option<SteeringDeliveryGuard>,
    ),
}

impl ForegroundSignal {
    /// Static diagnostic label; never includes command or provider payloads.
    pub(crate) fn diagnostic_name(&self) -> &'static str {
        match self {
            Self::LiveHost(..) => "LiveHost",
            Self::CollectionStarted(..) => "CollectionStarted",
            Self::CollectionTimedOut => "CollectionTimedOut",
            Self::CollectionCompleted(..) => "CollectionCompleted",
            Self::CollectionFailed => "CollectionFailed",
            Self::AdmissionWorkspaceCompleted(..) => "AdmissionWorkspaceCompleted",
            Self::AdmissionRequested => "AdmissionRequested",
            Self::AdmissionPrepared => "AdmissionPrepared",
            Self::FinalizeEffectsRequested => "FinalizeEffectsRequested",
            Self::PublicationDrained => "PublicationDrained",
            Self::QuestionCatalog(..) => "QuestionCatalog",
            Self::ProviderPrepared(..) => "ProviderPrepared",
            Self::Native(NativeForegroundSignal::Cancelled) => "NativeCancelled",
            Self::Native(NativeForegroundSignal::ToolExpired(_)) => "NativeToolExpired",
            Self::Native(NativeForegroundSignal::ProviderEvent(_)) => "NativeProviderEvent",
            Self::Native(NativeForegroundSignal::NativeReady(_)) => "NativeReady",
            Self::QuestionChanged => "QuestionChanged",
            Self::QuestionStopped(..) => "QuestionStopped",
            Self::Checkpoint(..) => "Checkpoint",
            Self::UserInput(..) => "UserInput",
            Self::UserDelivery(..) => "UserDelivery",
            Self::AgentChanged => "AgentChanged",
            Self::AgentDelivery(..) => "AgentDelivery",
            Self::AgentResultDelivered { .. } => "AgentResultDelivered",
            Self::AgentApprovalsCancelled { .. } => "AgentApprovalsCancelled",
            Self::AgentInterrupted(..) => "AgentInterrupted",
            Self::AgentSpawnSelected { .. } => "AgentSpawnSelected",
            Self::AgentMessageSteered { .. } => "AgentMessageSteered",
            Self::ExecutionControl(..) => "ExecutionControl",
            Self::ExecutionControlEvidence { .. } => "ExecutionControlEvidence",
            Self::ExecutionControlReport { .. } => "ExecutionControlReport",
            Self::RevisionReview(..) => "RevisionReview",
            Self::PlanScan => "PlanScan",
            Self::PlanScanned(..) => "PlanScanned",
            Self::FinalCheckpoint(..) => "FinalCheckpoint",
            Self::PlanMutationCompleted(..) => "PlanMutationCompleted",
            Self::AutomaticCheckRefreshed(..) => "AutomaticCheckRefreshed",
            Self::AutomaticCheckCompleted(..) => "AutomaticCheckCompleted",
            Self::SettlementReady { .. } => "SettlementReady",
            Self::SettlementCommitted(..) => "SettlementCommitted",
            Self::SessionObservationsReady => "SessionObservationsReady",
            Self::PublicationFailed(..) => "PublicationFailed",
            Self::EffectFailed(..) => "EffectFailed",
            Self::CleanupCompleted { .. } => "CleanupCompleted",
            Self::FailurePublicationCompleted(..) => "FailurePublicationCompleted",
            Self::FailurePublicationNeedsForward { .. } => "FailurePublicationNeedsForward",
            Self::FailureAdmissionEventsNeedForward { .. } => "FailureAdmissionEventsNeedForward",
            Self::FailureAdmissionEventsForwarded(..) => "FailureAdmissionEventsForwarded",
            Self::AdmissionEventsNeedForward { .. } => "AdmissionEventsNeedForward",
            Self::AdmissionEventsForwarded(..) => "AdmissionEventsForwarded",
            Self::CommandEventsNeedForward { .. } => "CommandEventsNeedForward",
            Self::CommandEventsForwarded(..) => "CommandEventsForwarded",
            Self::AdmissionCleanupCompleted(..) => "AdmissionCleanupCompleted",
            Self::AdmissionFailureCheckpointCompleted(..) => "AdmissionFailureCheckpointCompleted",
            Self::FailureCheckpointCompleted(..) => "FailureCheckpointCompleted",
            Self::FailureBaselineCompleted(..) => "FailureBaselineCompleted",
            Self::PlanSubmissionCompleted(..) => "PlanSubmissionCompleted",
            Self::PlanQuestionCompleted(..) => "PlanQuestionCompleted",
            Self::GoalProgressCompleted(..) => "GoalProgressCompleted",
            Self::ExecutionProgressCompleted(..) => "ExecutionProgressCompleted",
            Self::TaskCompacted(..) => "TaskCompacted",
            Self::CompactionCompleted(..) => "CompactionCompleted",
            Self::FinalizeChecksRequested => "FinalizeChecksRequested",
            Self::FinalizeCheckContinuationRequested => "FinalizeCheckContinuationRequested",
            Self::FinalResponseNeedsForward { .. } => "FinalResponseNeedsForward",
            Self::FinalResponsePublished(..) => "FinalResponsePublished",
            Self::ClarificationNeedsForward { .. } => "ClarificationNeedsForward",
            Self::ClarificationPublished(..) => "ClarificationPublished",
            Self::CheckRepairNeedsForward { .. } => "CheckRepairNeedsForward",
            Self::CheckRepairPublished(..) => "CheckRepairPublished",
            Self::PermissionNeedsForward { .. } => "PermissionNeedsForward",
            Self::PermissionPublished(..) => "PermissionPublished",
            Self::CommandNeedsForward { .. } => "CommandNeedsForward",
            Self::CommandPublished(..) => "CommandPublished",
            Self::AutoCompactionNeedsForward { .. } => "AutoCompactionNeedsForward",
            Self::AutoCompactionPublished(..) => "AutoCompactionPublished",
            Self::ObservationPublicationDelivered(..) => "ObservationPublicationDelivered",
            Self::ObservationPublicationNeedsForward { .. } => "ObservationPublicationNeedsForward",
            Self::PublicationDelivered(..) => "PublicationDelivered",
            Self::PlanTerminalPublished(..) => "PlanTerminalPublished",
            Self::PlanTerminalNeedsForward { .. } => "PlanTerminalNeedsForward",
            Self::PriorObservationReady(..) => "PriorObservationReady",
            Self::PriorInitialCompleted(..) => "PriorInitialCompleted",
            Self::PriorFinalCompleted(..) => "PriorFinalCompleted",
            Self::UserDeliverySettled(..) => "UserDeliverySettled",
        }
    }
}

/// Specify whether native settlement exists for the collected invocation.
#[derive(Clone, Copy)]
pub(crate) enum SettlementRequirement {
    /// A dispatched invocation must provide its native settlement ticket.
    Native,
    /// Positive receipt retirement proved collection completed before dispatch.
    CollectedUndispatched,
}

/// Describe the next service action after a short broker state transition.
pub(crate) enum ForegroundAction {
    Wait,
    CancelJobsAndEffect(ForegroundJob),
    WaitForPublicationDrain,
    Effect(ForegroundJob),
    Finish,
    FinishFailure(anyhow::Error),
}

/// Retain effect receipts until a caller has forwarded their session events.
pub(crate) struct OwnedEffectReceipts {
    cell: Arc<crate::provider::session_execution::SettlementCell>,
    ids: Vec<crate::provider::session_execution::HostEffectId>,
}

impl OwnedEffectReceipts {
    /// Mark effects published only after their outbound delivery succeeds.
    pub(crate) fn acknowledge(self) -> Result<()> {
        for id in self.ids {
            self.cell.acknowledge_effect_publication(id)?;
        }
        Ok(())
    }
}

/// Retain a child launch and the response that becomes valid after it starts.
pub(crate) struct PendingAgentLaunch {
    work: crate::agent::AgentWork,
    reply: Option<(tokio::sync::oneshot::Sender<Result<Value>>, Value)>,
}

/// Child completion retained while its approvals are cancelled unlocked.
pub(crate) struct AgentCompletion {
    owner: crate::provider::ThreadOwner,
    task_id: String,
    output: Result<crate::provider::ProviderCompletion>,
    host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
    cleanup_error: Option<String>,
}

/// Retain interruption ownership and its reply while child cleanup runs unlocked.
///
/// Every target is flagged when the interruption is captured, before any cleanup runs, and the
/// reply reports every target's failure together.
pub(crate) struct AgentInterrupt {
    items: Vec<AgentInterruptItem>,
    failures: Vec<anyhow::Error>,
    request: OwnedAgentRequest,
    result: Value,
}

/// Keep child request ownership and reply while provider work runs unlocked.
pub(crate) struct OwnedAgentRequest {
    request: crate::agent::AgentRequest,
    authority: Option<crate::agent::AgentRequestAuthority>,
    target: Option<crate::agent::AgentTarget>,
    response: tokio::sync::oneshot::Sender<Result<Value>>,
    parent_id: String,
    task_admission: Option<tokio::sync::OwnedSemaphorePermit>,
}

struct AgentInterruptItem {
    agent: Agent,
    owner: crate::provider::ThreadOwner,
    cancelled: Vec<crate::agent::AgentTask>,
    cleanup: Option<crate::agent::AgentTask>,
    provider: Option<Arc<Provider>>,
    /// The approvals the unlocked job removed, or `None` when their cancellation failed.
    approvals: Option<Vec<ApprovalRequestView>>,
    /// Whether the unlocked cleanup retry for `cleanup` completed.
    cleanup_succeeded: bool,
}

/// Own provider work that can run without retaining a broker guard.
pub(crate) enum ForegroundJob {
    AdmissionWorkspace(foreground_workspace::AdmissionWorkspaceJob),
    ProviderPrepare(foreground_dispatch::ProviderPrepareJob),
    QuestionCatalog(Box<foreground_dispatch::QuestionModelCatalogJob>),
    Publication(PendingProviderDelivery),
    FailurePublication(PendingProviderDelivery),
    FailureAdmissionEvents(Vec<SessionEvent>),
    AdmissionEvents(Vec<SessionEvent>),
    CommandEvents(Vec<SessionEvent>),
    AdmissionCleanup(foreground_failure::AdmissionCleanupJob),
    AdmissionFailureCheckpoint(Box<checkpoint_settlement::FinalCheckpointJob>),
    FinalResponsePublication(PendingProviderDelivery),
    ClarificationPublication(PendingProviderDelivery),
    CheckRepairPublication(PendingProviderDelivery),
    PermissionPublication(PendingProviderDelivery),
    CommandPublication(PendingProviderDelivery),
    AutoCompactionPublication(PendingProviderDelivery),
    ObservationPublication(PendingProviderDelivery),
    FailureCheckpoint(Box<checkpoint_settlement::FinalCheckpointJob>),
    FailureBaseline(foreground_failure::FailureBaselineJob),
    PlanSubmission(Box<foreground_plan_effect::PlanSubmissionJob>),
    LivePlanSubmission(Box<foreground_plan_effect::PlanSubmissionJob>),
    LivePlanMutation(Box<foreground_plan_effect::PlanMutationJob>),
    PlanQuestion(Box<foreground_plan_effect::PlanQuestionJob>),
    GoalProgress(Box<foreground_goal_effect::GoalProgressJob>),
    ExecutionProgress(Box<foreground_goal_effect::ExecutionProgressJob>),
    TaskCompaction(Box<foreground_dispatch::TaskCompactionJob>),
    Compaction(Box<compaction::CompactionJob>),
    CleanupForeground {
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        native: tokio::task::JoinSet<Result<ProviderAttempt>>,
        original: anyhow::Error,
        trace: Arc<TraceStore>,
        scope: crate::provider::InvocationScope,
    },
    QuestionStop {
        provider: Arc<Provider>,
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        scope: crate::provider::InvocationScope,
        fallback: Option<forge_provider_api::ProviderSessionId>,
        checkpoint: Option<String>,
        capability: ProviderCapability,
    },
    PlanTerminalPublication {
        delivery: PendingProviderDelivery,
        cell: Arc<crate::provider::session_execution::SettlementCell>,
        id: crate::provider::session_execution::HostEffectId,
    },
    PriorObservation(prior_finalization::PriorObservationJob),
    PriorInitial(prior_finalization::PriorInitialJob),
    PriorFinal(Box<prior_finalization::PriorFinalJob>),
    PlanScan(Box<execution::ExecutionScanJob>),
    PlanMutation(Box<foreground_plan_effect::PlanMutationJob>),
    ExecutionControl {
        job: Box<execution::ExecutionControlJob>,
        response: tokio::sync::oneshot::Sender<Result<serde_json::Value>>,
        submission: bool,
    },
    ExecutionControlReport {
        job: Box<execution::ExecutionControlReportJob>,
        response: tokio::sync::oneshot::Sender<Result<serde_json::Value>>,
        submission: bool,
    },
    AgentResult {
        /// Keeps native completion behind a child-result steer whose acceptance is unapplied,
        /// so finalization cannot deliver the same report again as a continuation.
        flight: SteeringDeliveryGuard,
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        scope: crate::provider::InvocationScope,
        task: Box<crate::agent::AgentTask>,
        message: forge_provider_api::forge::AgentResult,
    },
    AgentCancelApprovals {
        coordinator: Arc<PermissionCoordinator>,
        completion: Box<AgentCompletion>,
    },
    AgentInterrupt {
        coordinator: Arc<PermissionCoordinator>,
        operation: AgentInterrupt,
    },
    AgentSpawnSelect {
        factory: crate::agent::AgentProviderFactory,
        root: Box<ProviderRequest>,
        root_provider: crate::provider::ProviderId,
        spawn: crate::agent::SpawnAgentRequest,
        request: OwnedAgentRequest,
    },
    AgentMessageSteer {
        registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
        owner: crate::provider::ThreadOwner,
        message: String,
        request: OwnedAgentRequest,
    },
    AutomaticCheckRefresh(Box<check_admission::AutomaticCheckRefresh>),
    AutomaticCheckRun(Box<check_admission::AutomaticCheckRun>),
    FinalCheckpoint(Box<checkpoint_settlement::FinalCheckpointJob>),
    SettlementRefresh {
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        scope: crate::provider::InvocationScope,
        checkout: crate::provider::session_execution::SettlementCheckout,
    },
    SettlementCommit {
        cell: Arc<crate::provider::session_execution::SettlementCell>,
        requirement: SettlementRequirement,
        undispatched: Vec<crate::provider::SteerCommand>,
    },
    UserSteering {
        flight: SteeringDeliveryGuard,
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        scope: crate::provider::InvocationScope,
        input: crate::provider::SteerCommand,
    },
    SettleUserDelivery {
        input: crate::provider::SteerCommand,
        result: Result<()>,
    },
}

impl ForegroundJob {
    /// Static diagnostic label; never includes command or provider payloads.
    pub(crate) fn diagnostic_name(&self) -> &'static str {
        match self {
            Self::AdmissionWorkspace(..) => "AdmissionWorkspace",
            Self::ProviderPrepare(..) => "ProviderPrepare",
            Self::QuestionCatalog(..) => "QuestionCatalog",
            Self::Publication(..) => "Publication",
            Self::FailurePublication(..) => "FailurePublication",
            Self::FailureAdmissionEvents(..) => "FailureAdmissionEvents",
            Self::AdmissionEvents(..) => "AdmissionEvents",
            Self::CommandEvents(..) => "CommandEvents",
            Self::AdmissionCleanup(..) => "AdmissionCleanup",
            Self::AdmissionFailureCheckpoint(..) => "AdmissionFailureCheckpoint",
            Self::FinalResponsePublication(..) => "FinalResponsePublication",
            Self::ClarificationPublication(..) => "ClarificationPublication",
            Self::CheckRepairPublication(..) => "CheckRepairPublication",
            Self::PermissionPublication(..) => "PermissionPublication",
            Self::CommandPublication(..) => "CommandPublication",
            Self::AutoCompactionPublication(..) => "AutoCompactionPublication",
            Self::ObservationPublication(..) => "ObservationPublication",
            Self::FailureCheckpoint(..) => "FailureCheckpoint",
            Self::FailureBaseline(..) => "FailureBaseline",
            Self::PlanSubmission(..) => "PlanSubmission",
            Self::LivePlanSubmission(..) => "LivePlanSubmission",
            Self::LivePlanMutation(..) => "LivePlanMutation",
            Self::PlanQuestion(..) => "PlanQuestion",
            Self::GoalProgress(..) => "GoalProgress",
            Self::ExecutionProgress(..) => "ExecutionProgress",
            Self::TaskCompaction(..) => "TaskCompaction",
            Self::Compaction(..) => "Compaction",
            Self::CleanupForeground { .. } => "CleanupForeground",
            Self::QuestionStop { .. } => "QuestionStop",
            Self::PlanTerminalPublication { .. } => "PlanTerminalPublication",
            Self::PriorObservation(..) => "PriorObservation",
            Self::PriorInitial(..) => "PriorInitial",
            Self::PriorFinal(..) => "PriorFinal",
            Self::PlanScan(..) => "PlanScan",
            Self::PlanMutation(..) => "PlanMutation",
            Self::ExecutionControl { .. } => "ExecutionControl",
            Self::ExecutionControlReport { .. } => "ExecutionControlReport",
            Self::AgentResult { .. } => "AgentResult",
            Self::AgentCancelApprovals { .. } => "AgentCancelApprovals",
            Self::AgentInterrupt { .. } => "AgentInterrupt",
            Self::AgentSpawnSelect { .. } => "AgentSpawnSelect",
            Self::AgentMessageSteer { .. } => "AgentMessageSteer",
            Self::AutomaticCheckRefresh(..) => "AutomaticCheckRefresh",
            Self::AutomaticCheckRun(..) => "AutomaticCheckRun",
            Self::FinalCheckpoint(..) => "FinalCheckpoint",
            Self::SettlementRefresh { .. } => "SettlementRefresh",
            Self::SettlementCommit { .. } => "SettlementCommit",
            Self::UserSteering { .. } => "UserSteering",
            Self::SettleUserDelivery { .. } => "SettleUserDelivery",
        }
    }

    /// Identify cleanup and settlement work that remains authorized after new invocation effects close.
    pub(crate) fn is_collection(&self) -> bool {
        match self {
            Self::AdmissionCleanup(_)
            | Self::AdmissionFailureCheckpoint(_)
            | Self::FailureCheckpoint(_)
            | Self::FinalCheckpoint(_)
            | Self::FailureBaseline(_)
            | Self::CleanupForeground { .. }
            | Self::QuestionStop { .. }
            | Self::PriorObservation(_)
            | Self::PriorInitial(_)
            | Self::PriorFinal(_)
            | Self::AgentCancelApprovals { .. }
            | Self::AgentInterrupt { .. }
            | Self::SettlementRefresh { .. }
            | Self::SettlementCommit { .. }
            | Self::SettleUserDelivery { .. }
            | Self::GoalProgress(_)
            | Self::ExecutionProgress(_)
            | Self::PlanSubmission(_)
            | Self::PlanQuestion(_)
            | Self::PlanMutation(_) => true,
            Self::AdmissionWorkspace(_)
            | Self::ProviderPrepare(_)
            | Self::QuestionCatalog(_)
            | Self::Publication(_)
            | Self::FailurePublication(_)
            | Self::FailureAdmissionEvents(_)
            | Self::AdmissionEvents(_)
            | Self::CommandEvents(_)
            | Self::FinalResponsePublication(_)
            | Self::ClarificationPublication(_)
            | Self::CheckRepairPublication(_)
            | Self::PermissionPublication(_)
            | Self::CommandPublication(_)
            | Self::AutoCompactionPublication(_)
            | Self::ObservationPublication(_)
            | Self::LivePlanMutation(_)
            | Self::LivePlanSubmission(_)
            | Self::TaskCompaction(_)
            | Self::Compaction(_)
            | Self::PlanTerminalPublication { .. }
            | Self::PlanScan(_)
            | Self::ExecutionControl { .. }
            | Self::ExecutionControlReport { .. }
            | Self::AgentResult { .. }
            | Self::AgentSpawnSelect { .. }
            | Self::AgentMessageSteer { .. }
            | Self::AutomaticCheckRefresh(_)
            | Self::AutomaticCheckRun(_)
            | Self::UserSteering { .. } => false,
        }
    }
    /// Identify presentation batches that must be delivered in session order.
    pub(crate) fn is_publication(&self) -> bool {
        matches!(
            self,
            Self::Publication(_)
                | Self::ObservationPublication(_)
                | Self::FailurePublication(_)
                | Self::FailureAdmissionEvents(_)
                | Self::AdmissionEvents(_)
                | Self::CommandEvents(_)
                | Self::FinalResponsePublication(_)
                | Self::ClarificationPublication(_)
                | Self::CheckRepairPublication(_)
                | Self::PermissionPublication(_)
                | Self::CommandPublication(_)
                | Self::AutoCompactionPublication(_)
                | Self::PlanTerminalPublication { .. }
        )
    }

    /// Run one owned action and return its result to the serialized broker step.
    pub(crate) async fn run(self) -> Result<ForegroundSignal> {
        match self {
            Self::PriorObservation(job) => {
                Ok(ForegroundSignal::PriorObservationReady(job.run().await))
            }
            Self::PriorInitial(job) => Ok(ForegroundSignal::PriorInitialCompleted(job.run().await)),
            Self::PriorFinal(job) => Ok(ForegroundSignal::PriorFinalCompleted(job.run().await)),
            Self::PlanTerminalPublication { delivery, cell, id } => {
                let events = match delivery.deliver_and_flush().await {
                    Ok(events) => events,
                    Err(error) => return Ok(ForegroundSignal::PlanTerminalPublished(Err(error))),
                };
                let receipts = OwnedEffectReceipts {
                    cell,
                    ids: vec![id],
                };
                if !events.is_empty() {
                    return Ok(ForegroundSignal::PlanTerminalNeedsForward { events, receipts });
                }
                Ok(ForegroundSignal::PlanTerminalPublished(
                    receipts.acknowledge().map(|()| events),
                ))
            }
            Self::AdmissionWorkspace(job) => Ok(ForegroundSignal::AdmissionWorkspaceCompleted(
                Box::new(job.run().await),
            )),
            Self::CleanupForeground {
                engine,
                mut native,
                original,
                trace,
                scope,
            } => {
                trace.record(scope.owner().session_id().as_str(), "foreground.cleanup", json!({
                    "exchange_id":scope.exchange_id, "phase":"engine_close_started", "cause":format!("{original:#}"),
                }));
                // Scoped collection keeps the observation stream open until the interrupted
                // turn's terminal event is drained; closing the engine drops it and records the
                // turn as a disconnected failure instead of an interruption.
                let mut cleanup = if HarnessBroker::collects_exchange_scope(&original) {
                    engine.cancel_exchange(scope.clone()).await
                } else {
                    engine.close().await
                };
                trace.record(scope.owner().session_id().as_str(), "foreground.cleanup", json!({
                    "exchange_id":scope.exchange_id, "phase":"engine_closed", "failure":cleanup.as_ref().err().map(ToString::to_string),
                }));
                while let Some(result) = native.join_next().await {
                    if let Err(error) = result
                        && cleanup.is_ok()
                    {
                        cleanup = Err(crate::provider::lifecycle_error::classify_source(
                            forge_provider_api::ProviderOperation::ThreadClose,
                            &error,
                        ));
                    }
                }
                trace.record(scope.owner().session_id().as_str(), "foreground.cleanup", json!({
                    "exchange_id":scope.exchange_id, "phase":"native_joined", "failure":cleanup.as_ref().err().map(ToString::to_string),
                }));
                Ok(ForegroundSignal::CleanupCompleted { original, cleanup })
            }
            Self::FailureBaseline(job) => {
                Ok(ForegroundSignal::FailureBaselineCompleted(job.run().await))
            }
            Self::FailurePublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::FailurePublicationNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::FailurePublicationCompleted(Ok(events))),
                Err(error) => Ok(ForegroundSignal::FailurePublicationCompleted(Err(error))),
            },
            Self::FailureAdmissionEvents(events) => {
                Ok(ForegroundSignal::FailureAdmissionEventsNeedForward { events })
            }
            Self::AdmissionEvents(events) => {
                Ok(ForegroundSignal::AdmissionEventsNeedForward { events })
            }
            Self::CommandEvents(events) => {
                Ok(ForegroundSignal::CommandEventsNeedForward { events })
            }
            Self::AdmissionCleanup(job) => {
                Ok(ForegroundSignal::AdmissionCleanupCompleted(job.run().await))
            }
            Self::AdmissionFailureCheckpoint(job) => Ok(
                ForegroundSignal::AdmissionFailureCheckpointCompleted(job.run().await),
            ),
            Self::ObservationPublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::ObservationPublicationNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::ObservationPublicationDelivered(Ok(
                    events,
                ))),
                Err(error) => Ok(ForegroundSignal::ObservationPublicationDelivered(Err(
                    error,
                ))),
            },
            Self::FinalResponsePublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::FinalResponseNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::FinalResponsePublished(Ok(events))),
                Err(error) => Ok(ForegroundSignal::FinalResponsePublished(Err(error))),
            },
            Self::ClarificationPublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::ClarificationNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::ClarificationPublished(Ok(events))),
                Err(error) => Ok(ForegroundSignal::ClarificationPublished(Err(error))),
            },
            Self::CheckRepairPublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::CheckRepairNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::CheckRepairPublished(Ok(events))),
                Err(error) => Ok(ForegroundSignal::CheckRepairPublished(Err(error))),
            },
            Self::PermissionPublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::PermissionNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::PermissionPublished(Ok(events))),
                Err(error) => Ok(ForegroundSignal::PermissionPublished(Err(error))),
            },
            Self::CommandPublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::CommandNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::CommandPublished(Ok(events))),
                Err(error) => Ok(ForegroundSignal::CommandPublished(Err(error))),
            },
            Self::AutoCompactionPublication(delivery) => match delivery.deliver_and_flush().await {
                Ok(events) if !events.is_empty() => {
                    Ok(ForegroundSignal::AutoCompactionNeedsForward { events })
                }
                Ok(events) => Ok(ForegroundSignal::AutoCompactionPublished(Ok(events))),
                Err(error) => Ok(ForegroundSignal::AutoCompactionPublished(Err(error))),
            },
            Self::FailureCheckpoint(job) => Ok(ForegroundSignal::FailureCheckpointCompleted(
                job.run().await,
            )),
            Self::PlanSubmission(job) | Self::LivePlanSubmission(job) => {
                Ok(ForegroundSignal::PlanSubmissionCompleted(job.run().await))
            }
            Self::PlanQuestion(job) => Ok(ForegroundSignal::PlanQuestionCompleted(job.run().await)),
            Self::GoalProgress(job) => Ok(ForegroundSignal::GoalProgressCompleted(Box::new(
                job.run().await,
            ))),
            Self::ExecutionProgress(job) => Ok(ForegroundSignal::ExecutionProgressCompleted(
                Box::new(job.run().await),
            )),
            Self::TaskCompaction(job) => {
                Ok(ForegroundSignal::TaskCompacted(Box::new(job.run().await)))
            }
            Self::Compaction(job) => Ok(ForegroundSignal::CompactionCompleted(Box::new(
                job.run().await,
            ))),
            Self::ProviderPrepare(job) => Ok(ForegroundSignal::ProviderPrepared(job.run().await)),
            Self::PlanMutation(job) | Self::LivePlanMutation(job) => {
                Ok(ForegroundSignal::PlanMutationCompleted(job.run().await))
            }
            Self::QuestionStop {
                provider,
                engine,
                scope,
                fallback,
                checkpoint,
                capability,
            } => {
                let result = async {
                    let provider_session_id = provider
                        .retained_session_id(&scope.owner())
                        .await
                        .or(fallback);
                    engine
                        .cancel_exchange(scope)
                        .await
                        .context("stop execution for required questions")?;
                    Ok(ProviderCompletion {
                        native: forge_provider_api::completion::ProviderCompletion {
                            provider_session_id,
                            provider_checkpoint_id: checkpoint,
                            capability,
                            ..Default::default()
                        },
                        ..Default::default()
                    })
                }
                .await;
                Ok(ForegroundSignal::QuestionStopped(Box::new(result)))
            }
            Self::QuestionCatalog(job) => Ok(ForegroundSignal::QuestionCatalog(job.run().await)),
            Self::Publication(delivery) => Ok(match delivery.deliver().await {
                Ok(events) => ForegroundSignal::PublicationDelivered(events),
                Err(error) => ForegroundSignal::EffectFailed(error),
            }),
            Self::PlanScan(job) => Ok(ForegroundSignal::PlanScanned(Box::new(job.run().await))),
            Self::ExecutionControl {
                job,
                response,
                submission,
            } => Ok(ForegroundSignal::ExecutionControlEvidence {
                response,
                submission,
                result: Box::new(job.run().await),
            }),
            Self::ExecutionControlReport {
                job,
                response,
                submission,
            } => Ok(ForegroundSignal::ExecutionControlReport {
                response,
                submission,
                result: Box::new(job.run().await),
            }),
            Self::AgentResult {
                flight,
                engine,
                scope,
                task,
                message,
            } => {
                let result = engine
                    .steer(scope, crate::provider::SteeringInput::AgentResult(message))
                    .await
                    .map_err(anyhow::Error::from);
                Ok(ForegroundSignal::AgentResultDelivered {
                    flight,
                    task,
                    result,
                })
            }
            Self::AgentCancelApprovals {
                coordinator,
                completion,
            } => {
                let result = coordinator
                    .cancel_owner_approvals(&completion.owner, None)
                    .await;
                Ok(ForegroundSignal::AgentApprovalsCancelled { completion, result })
            }
            Self::AgentInterrupt {
                coordinator,
                mut operation,
            } => {
                let authority = operation
                    .request
                    .authority
                    .as_ref()
                    .and_then(|authority| authority.task.clone());
                // One failing child never stops the others: each target's approvals and cleanup
                // run independently and their failures are reported together.
                for item in &mut operation.items {
                    let cancelled = match &authority {
                        Some(authority) => {
                            authority.commit(|| coordinator.cancel_owner_immediate(&item.owner))
                        }
                        None => coordinator.cancel_owner_immediate(&item.owner),
                    };
                    match cancelled {
                        Ok(approvals) => item.approvals = Some(approvals),
                        Err(error) => {
                            operation
                                .failures
                                .push(error.context(format!("interrupt child {}", item.agent.id)));
                            continue;
                        }
                    }
                    if let Some(provider) = &item.provider {
                        let cleanup =
                            tokio::time::timeout(std::time::Duration::from_secs(30), async {
                                provider.session_close_owner(&item.owner).await?;
                                provider.shutdown().await
                            })
                            .await
                            .context("child cleanup retry exceeded 30 seconds")
                            .and_then(|result| result.map_err(anyhow::Error::from));
                        match cleanup {
                            Ok(()) => item.cleanup_succeeded = true,
                            Err(error) => operation
                                .failures
                                .push(error.context(format!("collect child {}", item.agent.id))),
                        }
                    }
                }
                Ok(ForegroundSignal::AgentInterrupted(operation))
            }
            Self::AgentSpawnSelect {
                factory,
                root,
                root_provider,
                spawn,
                request,
            } => {
                let selection = factory.resolve(&root, root_provider, &spawn).await;
                Ok(ForegroundSignal::AgentSpawnSelected {
                    request,
                    selection: Box::new(selection),
                })
            }
            Self::AgentMessageSteer {
                registry,
                owner,
                message,
                request,
            } => {
                let authority = request
                    .authority
                    .as_ref()
                    .and_then(|authority| authority.task.clone());
                let result = match registry.owner(owner.clone()).await {
                    Ok(engine) => engine
                        .steer_current_authorized(message.into(), authority)
                        .await
                        .map_err(anyhow::Error::from),
                    Err(error) => Err(error.into()),
                };
                Ok(ForegroundSignal::AgentMessageSteered {
                    request,
                    registry,
                    owner,
                    result,
                })
            }
            Self::AutomaticCheckRefresh(job) => Ok(ForegroundSignal::AutomaticCheckRefreshed(
                Box::new(job.run().await),
            )),
            Self::AutomaticCheckRun(job) => Ok(ForegroundSignal::AutomaticCheckCompleted(
                Box::new(job.run().await),
            )),
            Self::FinalCheckpoint(job) => Ok(ForegroundSignal::FinalCheckpoint(job.run().await)),
            Self::SettlementRefresh {
                engine,
                scope,
                checkout,
            } => Ok(ForegroundSignal::SettlementReady {
                checkout,
                ready: Box::new(engine.wait_ready(scope).await),
            }),
            Self::SettlementCommit {
                cell,
                requirement,
                undispatched,
            } => {
                use crate::provider::session_execution::SettlementError;
                for input in undispatched {
                    if let Err(error) = input.cancel_undispatched().await {
                        return Ok(ForegroundSignal::SettlementCommitted(Err(
                            SettlementError::Provider(crate::provider::lifecycle_error::classify(
                                forge_provider_api::ProviderOperation::TurnStart,
                                error,
                            )),
                        )));
                    }
                }
                if let Err(error) = cell.validate_effects_settled() {
                    return Ok(ForegroundSignal::SettlementCommitted(Err(
                        SettlementError::Provider(error),
                    )));
                }
                let mut checkout = match cell.checkout() {
                    Ok(checkout) => checkout,
                    Err(error) => {
                        return Ok(ForegroundSignal::SettlementCommitted(Err(
                            SettlementError::Provider(error),
                        )));
                    }
                };
                let result = match checkout.ticket() {
                    Some(ticket) => ticket.commit().await,
                    None => match requirement {
                        SettlementRequirement::Native => Err(SettlementError::Unavailable),
                        SettlementRequirement::CollectedUndispatched => Ok(()),
                    },
                };
                if result.is_ok() {
                    checkout.take();
                }
                Ok(ForegroundSignal::SettlementCommitted(result))
            }
            Self::UserSteering {
                flight,
                engine,
                scope,
                input,
            } => {
                let result = engine
                    .steer(scope, input.input())
                    .await
                    .map_err(anyhow::Error::from);
                Ok(match input.settle_delivery(result).await {
                    Ok(input) => ForegroundSignal::UserDeliverySettled(input, Some(flight)),
                    Err(error) => ForegroundSignal::EffectFailed(error),
                })
            }
            Self::SettleUserDelivery { input, result } => {
                Ok(match input.settle_delivery(result).await {
                    Ok(input) => ForegroundSignal::UserDeliverySettled(input, None),
                    Err(error) => ForegroundSignal::EffectFailed(error),
                })
            }
        }
    }
}

impl ForegroundTransaction {
    /// Reject later steering once native completion has been observed or retained.
    pub(crate) fn accepts_steering(&self) -> bool {
        self.native_open && self.pending_native_ready.is_none()
    }

    /// Close native polling when the broker consumes a result that collection may have queued first.
    pub(crate) fn acknowledge_native_result(&mut self) {
        self.native.acknowledge_result();
        self.native_open = self.native.has_dispatched_task();
    }

    /// Wait for one source without retaining the broker or presentation lock.
    pub(crate) async fn next(&mut self) -> Result<ForegroundSignal> {
        loop {
            if self.failure_cleanup_active {
                // Retained events wait for their preceding publication's acknowledgment.
                // Re-selecting a ready failed inbox would starve that publication's job.
                if !self.observation_publication_in_flight
                    && !self.pending_session_observations.is_empty()
                {
                    return Ok(ForegroundSignal::SessionObservationsReady);
                }
                tokio::select! {
                    checkpoint = async {
                        match self.state.runtime.as_mut().and_then(|runtime| runtime.checkpoint.as_mut()) {
                            Some(task) => (&mut task.0).await.context("checkpoint worker failed").and_then(|result| result.map_err(anyhow::Error::new)),
                            None => std::future::pending().await,
                        }
                    } => return Ok(ForegroundSignal::Checkpoint(checkpoint)),
                    event = self.native.events.recv(), if self.native.stream_open && !self.observation_publication_in_flight && self.pending_session_observations.len() < forge_protocol::MAX_PENDING_FRAMES => match event {
                        Ok(Some(event)) => return Ok(ForegroundSignal::Native(NativeForegroundSignal::ProviderEvent(event))),
                        Ok(None) => { self.native.stream_open = false; continue; }
                        Err(error) => { self.native.stream_open = false; return Ok(ForegroundSignal::EffectFailed(error.into())); }
                    },
                    ready = self.session_events.notified(), if self.failure_inbox_open && !self.observation_publication_in_flight && self.pending_session_observations.len() < forge_protocol::MAX_PENDING_FRAMES => match ready {
                        Ok(()) => return Ok(ForegroundSignal::SessionObservationsReady),
                        Err(_) => { self.failure_inbox_open = false; continue; }
                    },
                    else => std::future::pending::<()>().await,
                }
                continue;
            }
            if !self.observation_publication_in_flight
                && !self.pending_session_observations.is_empty()
            {
                return Ok(ForegroundSignal::SessionObservationsReady);
            }
            if !self.observation_publication_in_flight
                && !self.question_stop_pending
                && self.steering.pending_deliveries.is_empty()
                && self.pending_native_ready.is_some()
                && !(self.native_ready_parked && self.collection.is_none())
            {
                // A bounded staging window is not the entire accepted observation prefix.
                // Recheck both upstream owners before exposing a deferred native result.
                if let Some(event) = self.native.events.try_next()? {
                    self.pending_session_observations.push_back(event);
                    return Ok(ForegroundSignal::SessionObservationsReady);
                }
                if self.session_events.has_pending()? {
                    return Ok(ForegroundSignal::SessionObservationsReady);
                }
                let ready = self.pending_native_ready.take().expect(
                    "deferred native result was checked before draining its observation prefix",
                );
                return Ok(ForegroundSignal::Native(
                    NativeForegroundSignal::NativeReady(ProviderAttempt::Ready(Box::new(ready))),
                ));
            }
            let cancellation = self.native.cancellation.clone();
            let watchdog = self.native.watchdog.clone();
            tokio::select! {
                biased;
                request = self.live_host_receiver.recv(), if self.live_host.is_none() && !self.live_host_receiver.is_closed() => {
                    if let Some(request) = request { return Ok(ForegroundSignal::LiveHost(Box::new(request))); }
                }
                () = self.steering.pending_deliveries.changed.notified(), if self.pending_native_ready.is_some() => continue,
                () = cancellation.cancelled(), if self.collection.is_none() => return Ok(ForegroundSignal::Native(NativeForegroundSignal::Cancelled)),
                expired = watchdog.expired(), if self.collection.is_none() => {
                    return Ok(ForegroundSignal::Native(NativeForegroundSignal::ToolExpired(expired?.context("tool watchdog ended during foreground execution")?)));
                }
                changed = self.question_changes.changed(), if self.question_enabled => {
                    changed.context("question controller closed during execution")?;
                    return Ok(ForegroundSignal::QuestionChanged);
                }
                checkpoint = async {
                    match self.state.runtime.as_mut().and_then(|runtime| runtime.checkpoint.as_mut()) {
                        Some(task) => (&mut task.0).await.context("checkpoint worker failed").and_then(|result| result.map_err(anyhow::Error::new)),
                        None => std::future::pending().await,
                    }
                } => return Ok(ForegroundSignal::Checkpoint(checkpoint)),
                input = self.steering.active.receive(), if self.user_delivery.is_empty() && self.steering.deferred.is_empty() && (self.pending_native_ready.is_none() || self.native_ready_parked) => {
                    let input = input.context("parent input receiver closed")?;
                    if self.native_ready_parked {
                        // New user input resumes a parent that waits for its children.
                        self.steering.deferred.push_back(input);
                        self.native_ready_parked = false;
                        continue;
                    }
                    return Ok(ForegroundSignal::UserInput(input));
                }
                delivery = self.user_delivery.join_next(), if !self.user_delivery.is_empty() => {
                    return Ok(ForegroundSignal::UserDelivery(delivery.context("parent input delivery disappeared")?.context("parent input delivery failed")?));
                }
                changed = self.agent_revision.changed(), if !self.agent_command_active => {
                    changed.context("agent coordinator closed during root execution")?;
                    self.native_ready_parked = false;
                    return Ok(ForegroundSignal::AgentChanged);
                }
                delivery = self.result_delivery.join_next(), if !self.result_delivery.is_empty() => {
                    return Ok(ForegroundSignal::AgentDelivery(Box::new(delivery.context("parent result delivery disappeared")?.context("parent result delivery task failed")?)));
                }
                control = self.execution_receiver.recv(), if self.execution_open => match control {
                    Some(control) => return Ok(ForegroundSignal::ExecutionControl(control)),
                    None => self.execution_open = false,
                },
                review = async {
                    match &mut self.pending_revision { Some((_, wait)) => wait.receiver.recv().await, None => std::future::pending().await }
                }, if !self.revision_job_active => return Ok(ForegroundSignal::RevisionReview(review.context("revision review request lane closed")?)),
                _ = self.scan_interval.tick(), if self.scan_enabled && self.pending_revision.is_none() && self.collection.is_none() => return Ok(ForegroundSignal::PlanScan),
                native = async {
                    if self.collection.is_some() { self.native.next_collected().await } else { self.native.next().await }
                }, if self.native_open && !self.observation_publication_in_flight && self.pending_session_observations.len() < forge_protocol::MAX_PENDING_FRAMES => {
                    let native = native?;
                    if matches!(&native, NativeForegroundSignal::NativeReady(_)) { self.native_open = false; }
                    return Ok(ForegroundSignal::Native(native));
                },
                ready = self.session_events.notified(), if !self.observation_publication_in_flight && self.pending_session_observations.len() < forge_protocol::MAX_PENDING_FRAMES => { ready?; return Ok(ForegroundSignal::SessionObservationsReady); }
                error = publication::failed(self.delivery_sink.as_ref()) => return Ok(ForegroundSignal::PublicationFailed(error)),
            }
        }
    }
}

/// Move provider submission and its dependencies out of broker state before polling.
pub(crate) enum ProviderAttempt {
    Ready(Box<Result<ProviderCompletion, ProviderError>>),
    Stale {
        request: Box<ProviderRequest>,
        command: Option<Box<crate::provider::SteerCommand>>,
        retry: crate::provider::retry::RetryState,
    },
}

pub(crate) struct ProviderSubmission {
    request: ProviderRequest,
    engine: Arc<crate::provider::session_execution::SessionExecution>,
    sink: crate::provider::HarnessEventSink,
    trace: Arc<TraceStore>,
    store: Arc<crate::provider::submission::SubmissionStore>,
    continuation: Option<crate::provider::SteerCommand>,
    settlement: Arc<crate::provider::session_execution::SettlementCell>,
    retry: crate::provider::retry::RetryState,
}

impl ProviderSubmission {
    /// Submit one attempt; the normalized callback enforces host checkpoint admission.
    pub(crate) async fn run(self) -> Result<ProviderAttempt> {
        let acceptance_sink = self.sink.clone();
        let response = crate::provider::retry::execute_until_ready(
            self.engine.as_ref(),
            self.request,
            self.sink,
            &self.trace,
            &self.store,
            self.settlement,
            self.retry,
        );
        let (attempt, command) = if let Some(command) = self.continuation {
            command.complete_with(response, acceptance_sink).await
        } else {
            response.await.map(|attempt| (attempt, None))
        }?;
        Ok(match attempt {
            crate::provider::retry::RetryAttempt::Ready(result) => ProviderAttempt::Ready(result),
            crate::provider::retry::RetryAttempt::Stale(request, retry) => ProviderAttempt::Stale {
                request,
                command: command.map(Box::new),
                retry,
            },
        })
    }
}

pub(crate) struct ExchangeRuntime {
    task_permit: Option<crate::task::TaskPermit>,
    checkpoint: Option<crate::checkpoint::CheckpointTask>,
    tool_gate: Option<crate::checkpoint::ToolGate>,
    exchange_id: String,
    checklist: ChecklistTracker,
    retraction_eligibility: RetractionEligibility,
    active_wait: Option<ActiveWait>,
}

impl ExchangeRuntime {
    fn tools_blocked(&self) -> bool {
        self.tool_gate.as_ref().is_some_and(|gate| !gate.ready())
    }
}

/// Track whether a provisional prompt is still output-free, so its cancellation may retract it.
///
/// A provider echo of the submitted prompt is not provider activity: Codex reports the prompt as
/// a native user-message item before any assistant work. Every other item, including reasoning,
/// is visible model work, and checklist, compaction, and steering acceptance also consume it.
#[derive(Debug)]
enum RetractionEligibility {
    /// Retain echoed user-message items, whose later deltas and completions stay output-free.
    OutputFree {
        user_message_items: std::collections::HashSet<forge_provider_api::ItemId>,
    },
    /// Persist cancellation as a partial exchange because the provider produced activity.
    Consumed,
}

impl RetractionEligibility {
    /// Create the eligibility of a prompt that the provider has not yet answered.
    fn output_free() -> Self {
        Self::OutputFree {
            user_message_items: std::collections::HashSet::new(),
        }
    }

    /// Get whether no provider activity has consumed the prompt.
    fn is_output_free(&self) -> bool {
        matches!(self, Self::OutputFree { .. })
    }

    /// Record provider activity that makes history retraction unsafe.
    fn consume(&mut self) {
        *self = Self::Consumed;
    }

    /// Classify one native item event as a prompt echo or as provider activity.
    fn observe_item(
        &mut self,
        item_id: &forge_provider_api::ItemId,
        event: &forge_provider_api::ItemEvent,
    ) {
        let Self::OutputFree { user_message_items } = self else {
            return;
        };
        let echo = match event {
            forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::UserMessage(
                _,
            )) => {
                user_message_items.insert(item_id.clone());
                true
            }
            forge_provider_api::ItemEvent::Started(_)
            | forge_provider_api::ItemEvent::Message { .. }
            | forge_provider_api::ItemEvent::Tool { .. }
            | forge_provider_api::ItemEvent::Completed(_) => user_message_items.contains(item_id),
        };
        if !echo {
            self.consume();
        }
    }
}

impl Drop for ExchangeRuntime {
    fn drop(&mut self) {
        if let Some(gate) = &self.tool_gate {
            gate.complete(Err(crate::checkpoint::CheckpointGateError::OwnerEnded));
        }
    }
}

struct ChildExchangeRuntime {
    exchange: Exchange,
    checklist: ChecklistTracker,
}

/// Represents one request result plus asynchronous events emitted before its response.
pub struct DispatchResult {
    pub response: Response,
    pub event: Vec<SessionEvent>,
}

/// Owns provider, permission, and analysis infrastructure shared by every session controller.
pub struct BrokerRuntime {
    pub(crate) lsp: Arc<crate::tools::lsp::LspService>,
    /// Routes messages between this host's sessions once the owning service binds it.
    pub(crate) session_messages: Arc<session_message::SessionMessageRouter>,
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    workspace_kind: WorkspaceKind,
    provider: Arc<Provider>,
    execution_registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    pub(crate) permission_store: Arc<RwLock<PermissionStore>>,
    pub(crate) profiles: Arc<crate::agent::AgentProfileStore>,
    permission_coordinator: Arc<PermissionCoordinator>,
    trace: Arc<TraceStore>,
}

#[derive(Debug)]
struct RuntimeShutdownFailure {
    fence: Option<ProviderError>,
    native: Option<ProviderError>,
    execution: Option<crate::provider::session_execution::RegistryError>,
}

impl std::fmt::Display for RuntimeShutdownFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "provider shutdown did not fully complete")?;
        if let Some(error) = &self.fence {
            write!(formatter, "; admission fence: {error}")?;
        }
        if let Some(error) = &self.native {
            write!(formatter, "; native shutdown: {error:#}")?;
        }
        if let Some(error) = &self.execution {
            write!(formatter, "; execution collection: {error}")?;
        }
        Ok(())
    }
}

impl std::error::Error for RuntimeShutdownFailure {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.fence
            .as_ref()
            .map(|error| error as &(dyn std::error::Error + 'static))
            .or_else(|| {
                self.native
                    .as_ref()
                    .map(|error| error as &(dyn std::error::Error + 'static))
            })
            .or_else(|| {
                self.execution
                    .as_ref()
                    .map(|error| error as &(dyn std::error::Error + 'static))
            })
    }
}

impl BrokerRuntime {
    /// Builds provider infrastructure for a workspace already resolved by the caller.
    /// Session controllers reuse its workspace, permission scope, and caller-owned analysis engine.
    pub fn initialize(
        request: &InitializeRequest,
        workspace_kind: WorkspaceKind,
        repositories: Arc<forge_git::store::RepositoryStore>,
        diff: Arc<forge_diff::engine::DiffEngine>,
    ) -> Result<Arc<Self>> {
        let data_root = PathBuf::from(&request.data_root);
        let workspace = match &workspace_kind {
            WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                path.to_string_lossy().into_owned()
            }
        };
        let permission_file = request
            .permission_file
            .as_deref()
            .map(PathBuf::from)
            .unwrap_or_else(|| data_root.join("permissions.json"));
        let profiles = Arc::new(crate::agent::AgentProfileStore::new(
            permission_file
                .parent()
                .unwrap_or(&data_root)
                .join("subagents.json"),
        ));
        let permission_store = Arc::new(RwLock::new(PermissionStore::load(
            permission_file,
            &workspace,
        )?));
        let permission_coordinator =
            Arc::new(PermissionCoordinator::new(Arc::clone(&permission_store)));
        let trace = Arc::new(TraceStore::open(&data_root)?);
        let provider = crate::provider::build(
            request.provider.clone(),
            Arc::clone(&permission_coordinator),
            Arc::clone(&trace),
            data_root.clone(),
        )?;
        Ok(Arc::new(Self {
            lsp: Arc::new(Default::default()),
            session_messages: Arc::default(),
            repositories,
            diff,
            workspace_kind,
            execution_registry: Arc::new(
                crate::provider::session_execution::SessionExecutionRegistry::new(
                    provider.clone(),
                    trace.clone(),
                ),
            ),
            provider,
            permission_store,
            profiles,
            permission_coordinator,
            trace,
        }))
    }

    /// Share the process-wide provider host with concurrent session controllers.
    pub fn provider_handle(&self) -> Arc<Provider> {
        Arc::clone(&self.provider)
    }

    /// Fence admission, drain provider shutdown evidence, then collect execution owners.
    pub(crate) async fn shutdown(&self) -> Result<()> {
        let fence = self.execution_registry.begin_orderly_shutdown().await;
        let native = self.provider.shutdown().await;
        let execution = self.execution_registry.close().await;
        let failure = RuntimeShutdownFailure {
            fence: fence.err(),
            native: native.err(),
            execution: execution.err(),
        };
        if failure.fence.is_some() || failure.native.is_some() || failure.execution.is_some() {
            Err(failure.into())
        } else {
            Ok(())
        }
    }

    pub fn trace(&self) -> Arc<TraceStore> {
        Arc::clone(&self.trace)
    }
}

/// Select the current invocation's cancellation signal; workers retain their captured signal.
pub struct TurnCancellation {
    current: std::sync::Mutex<Arc<TurnCancellationSignal>>,
}

/// Interrupt one invocation without allowing a later invocation to reset its signal.
pub(crate) struct TurnCancellationSignal {
    requested: Arc<AtomicBool>,
    submission_cancelled: Arc<AtomicBool>,
    state: tokio::sync::watch::Sender<bool>,
}

impl TurnCancellation {
    fn new() -> Self {
        Self {
            current: std::sync::Mutex::new(Arc::new(TurnCancellationSignal::new())),
        }
    }

    /// Install a new signal without resetting any worker's retained cancellation state.
    pub fn arm(&self) {
        *self
            .current
            .lock()
            .expect("cancellation signal lock poisoned") = Arc::new(TurnCancellationSignal::new());
    }

    /// Capture the signal for work admitted to the current invocation.
    pub(crate) fn capture(&self) -> Arc<TurnCancellationSignal> {
        self.current
            .lock()
            .expect("cancellation signal lock poisoned")
            .clone()
    }

    /// Interrupt the current invocation and close its host callback admission.
    pub fn request(&self) {
        self.capture().request();
    }

    /// Close callback admission while retaining a live native worker for collection.
    pub(crate) fn block_submissions(&self) {
        self.capture()
            .submission_cancelled
            .store(true, Ordering::Release);
    }

    /// Observe interruption of the invocation selected when this subscription is created.
    pub(crate) fn subscribe(&self) -> tokio::sync::watch::Receiver<bool> {
        self.capture().subscribe()
    }

    /// Get the current invocation's synchronous interruption state.
    fn is_requested(&self) -> bool {
        self.capture().requested.load(Ordering::Acquire)
    }

    /// Capture a synchronous interruption flag for a background worker.
    fn requested_flag(&self) -> Arc<AtomicBool> {
        self.capture().requested.clone()
    }

    /// Capture callback admission for one invocation's tool context.
    fn submission_flag(&self) -> Arc<AtomicBool> {
        self.capture().submission_cancelled.clone()
    }

    /// Exercise native collection independently of local worker interruption.
    #[cfg(test)]
    pub(crate) fn request_native_cancel(&self) {
        let signal = self.capture();
        signal.submission_cancelled.store(true, Ordering::Release);
        signal.state.send_replace(true);
    }
}

impl TurnCancellationSignal {
    /// Create a fresh, open invocation signal.
    fn new() -> Self {
        Self {
            requested: Arc::new(AtomicBool::new(false)),
            submission_cancelled: Arc::new(AtomicBool::new(false)),
            state: tokio::sync::watch::channel(false).0,
        }
    }

    /// Close host admission and wake every waiter for this invocation.
    fn request(&self) {
        self.submission_cancelled.store(true, Ordering::Release);
        self.requested.store(true, Ordering::Release);
        self.state.send_replace(true);
    }

    /// Wait for this invocation's signal without observing a replacement invocation.
    async fn cancelled(&self) {
        let mut receiver = self.subscribe();
        while !*receiver.borrow_and_update() {
            if receiver.changed().await.is_err() {
                return;
            }
        }
    }

    /// Observe only this invocation's cancellation state.
    fn subscribe(&self) -> tokio::sync::watch::Receiver<bool> {
        self.state.subscribe()
    }
}

#[derive(Debug)]
/// Rejects a request before it owns or mutates the active execution.
pub struct RequestNotAdmitted(
    /// Describes the unavailable execution or cleanup owner.
    pub &'static str,
);

impl std::fmt::Display for RequestNotAdmitted {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.0)
    }
}

impl std::error::Error for RequestNotAdmitted {}

#[derive(Debug)]
/// Rejects a request naming a durable session that does not exist, which no retry can repair.
pub struct SessionNotFound {
    /// Identifies the requested session.
    pub session_id: String,
}

impl SessionNotFound {
    /// Return the loaded session, or the typed absence that clients treat as terminal.
    pub(crate) fn require<Session>(session: Option<Session>, session_id: &str) -> Result<Session> {
        session.ok_or_else(|| {
            Self {
                session_id: session_id.to_owned(),
            }
            .into()
        })
    }
}

impl std::fmt::Display for SessionNotFound {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "session not found: {}", self.session_id)
    }
}

impl std::error::Error for SessionNotFound {}

/// Classify a request failure into the stable wire code clients use to choose retry behavior.
pub fn request_failure_code(error: &anyhow::Error) -> &'static str {
    if error.is::<RequestNotAdmitted>() {
        "not_admitted"
    } else if error.is::<TurnCancelled>() {
        "turn_cancelled"
    } else if error.is::<crate::provider::tool_timeout::ToolTimeout>() {
        "tool_timeout"
    } else if error.is::<SessionNotFound>() {
        "session_not_found"
    } else {
        "request_failed"
    }
}

#[derive(Debug)]
struct TurnRetracted {
    prompt: String,
}

impl std::fmt::Display for TurnRetracted {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Output-free turn retracted")
    }
}

impl std::error::Error for TurnRetracted {}

struct ExchangeAdmission {
    origin: crate::exchange::ExchangeOrigin,
    check_owner: Option<String>,
    automatic_owner: Option<String>,
    plan_event_id: Option<String>,
    prompt: String,
    agent_run_id: Option<String>,
}

struct PromptControlSnapshot {
    session: HarnessSession,
    execution: Vec<PlanExecutionRecord>,
    plan: Vec<PlanRecord>,
}

/// Retain submission admission and rollback state across independently awaited work.
pub(crate) struct PromptSubmission {
    snapshot: PromptControlSnapshot,
    submitted_prompt: Option<(Option<forge_buffer::identity::DocumentId>, u64)>,
}

impl ExchangeAdmission {
    fn chat(prompt: String) -> Self {
        Self {
            plan_event_id: None,
            check_owner: None,
            automatic_owner: None,
            prompt,
            origin: crate::exchange::ExchangeOrigin::User,
            agent_run_id: None,
        }
    }
}

/// Coordinates provider turns, durable state, plan review, goals, and interaction checkpoints.
pub struct HarnessBroker {
    question_model: Option<crate::settings::AskModel>,
    question_discussion: bool,
    pub(crate) questions: Arc<crate::question::QuestionController>,
    pub(crate) question_events: Option<crate::exchange::QuestionEvents>,
    pub(crate) task_controller: Arc<crate::task::SessionTaskController>,
    context_usage_fresh: bool,
    pub(crate) lsp: Arc<crate::tools::lsp::LspService>,
    /// Routes messages between this host's sessions once the owning service binds it.
    pub(crate) session_messages: Arc<session_message::SessionMessageRouter>,
    checkpoint: Option<GitCheckpoint>,
    active_operation_id: Option<String>,
    observer: Arc<observation::BrokerObserver>,
    repositories: Arc<forge_git::store::RepositoryStore>,
    diff: Arc<forge_diff::engine::DiffEngine>,
    store: SqliteStore,
    pub(crate) submission_store: Arc<crate::provider::submission::SubmissionStore>,
    plan_file: Arc<PlanFileStore>,
    check_runner: Arc<crate::checks::CheckSupervisor>,
    check_setup: bool,
    /// Retain the exchange that an output-free cancellation removed through confirmed retraction,
    /// so the exchange owner distinguishes that removal from lost history.
    retracted_exchange_id: Option<String>,
    /// Shares admitted review documents between view requests and serialized question turns.
    pub(crate) plan_review: Arc<crate::plan::review_document::PlanReviewStore>,
    pub(crate) revision_review: Arc<crate::plan::revision_review::RevisionReview>,
    workspace_kind: WorkspaceKind,
    pub(crate) data_root: PathBuf,
    client_id: String,
    provider_launch: ProviderLaunch,
    provider: Arc<Provider>,
    execution_registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
    session: HarnessSession,
    capability: ProviderCapability,
    latest_manifest: Option<forge_provider_api::CapabilityManifest>,
    configuration_identity: Option<crate::provider::ConfiguredBindingReceipt>,
    pub(crate) goal_max_turns: u32,
    permission_store: Arc<RwLock<PermissionStore>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    pub(crate) trace: Arc<TraceStore>,
    event_sink: Option<publication::SessionUpdateSink>,
    clock: Box<dyn Clock>,
    exchange_runtime: Option<ExchangeRuntime>,
    host_settlement: host_settlement::HostSettlementFence,
    pending_host_settlement: Option<Arc<crate::provider::session_execution::SettlementCell>>,
    settlement_events: Vec<SessionEvent>,
    agent_registry: AgentRegistry,
    native_providers_by_binding: HashMap<forge_provider_api::BindingId, Arc<Provider>>,
    agent_coordinator: Arc<crate::agent::AgentCoordinator>,
    pub(crate) agent_provider_factory: crate::agent::AgentProviderFactory,
    child_provider_by_agent: HashMap<String, Arc<Provider>>,
    child_execution_by_agent:
        HashMap<String, Arc<crate::provider::session_execution::SessionExecutionRegistry>>,
    mcp_notice: HashMap<String, String>,
    child_exchange_runtime_by_agent: HashMap<String, ChildExchangeRuntime>,
    presentation: Arc<std::sync::Mutex<SessionPresentation>>,
    prompt_admission: Option<(Option<forge_buffer::identity::DocumentId>, u64)>,
    pub(crate) input_submission: Option<Arc<crate::provider::submission::Submission>>,
    pub(crate) input_purpose: crate::input::InputPurpose,
    pub(crate) input_wakeup: Option<Arc<tokio::sync::Notify>>,
    submitted_attachments: Vec<crate::attachment::InputAttachment>,
    active_wait_projection: Option<ActiveWait>,
    pub(crate) parent_input: crate::provider::SteeringLane,
    timeline_reconciled_after_dispatch: bool,
    turn_cancellation: Arc<TurnCancellation>,
    pub(crate) startup_timing: std::collections::BTreeMap<&'static str, f64>,
}

impl HarnessBroker {
    /// Install a fixture provider before its execution owner is opened.
    #[cfg(test)]
    pub(crate) fn set_provider_for_test(&mut self, provider: Arc<crate::provider::Provider>) {
        self.provider = provider;
    }

    pub(crate) fn execution_registry(
        &mut self,
    ) -> Arc<crate::provider::session_execution::SessionExecutionRegistry> {
        if !self.execution_registry.matches(&self.provider) {
            self.execution_registry = Arc::new(
                crate::provider::session_execution::SessionExecutionRegistry::with_observer(
                    self.provider.clone(),
                    crate::provider::ThreadOwner::root(&self.session.id),
                    self.observer.session_events(),
                    self.trace.clone(),
                ),
            );
        }
        self.execution_registry.clone()
    }

    /// Get the retained host settlement owner without transferring its ticket.
    pub(crate) fn host_settlement(
        &self,
    ) -> Option<Arc<crate::provider::session_execution::SettlementCell>> {
        self.pending_host_settlement.clone()
    }

    /// Clear an exhausted settlement cell after its ticket has been committed or collected.
    pub(crate) fn clear_host_settlement(
        &mut self,
        settlement: &Arc<crate::provider::session_execution::SettlementCell>,
    ) -> Result<()> {
        anyhow::ensure!(
            self.pending_host_settlement
                .as_ref()
                .is_some_and(|current| Arc::ptr_eq(current, settlement)),
            "host settlement belongs to another transaction"
        );
        anyhow::ensure!(
            settlement.is_empty()?,
            "host settlement ticket is still retained"
        );
        self.pending_host_settlement = None;
        Ok(())
    }

    /// Initialize the broker and acquire one writable session lease.
    pub fn initialize(request: InitializeRequest) -> Result<Self> {
        Self::initialize_with_clock(request, Box::new(SystemClock))
    }

    /// Initialize the broker with an injected clock for deterministic lease tests.
    pub fn initialize_with_clock(
        request: InitializeRequest,
        clock: Box<dyn Clock>,
    ) -> Result<Self> {
        let workspace_kind = crate::workspace::resolve(Path::new(&request.workspace))?;
        let runtime = BrokerRuntime::initialize(
            &request,
            workspace_kind,
            Arc::new(forge_git::store::RepositoryStore::default()),
            forge_diff::engine::DiffEngine::new(forge_diff::cache::CacheLimits::default(), 4),
        )?;
        Self::initialize_with_runtime_and_clock(request, runtime, clock)
    }

    /// Initialize one session controller against shared provider infrastructure.
    pub fn initialize_with_runtime(
        request: InitializeRequest,
        runtime: Arc<BrokerRuntime>,
    ) -> Result<Self> {
        Self::initialize_with_runtime_and_clock(request, runtime, Box::new(SystemClock))
    }

    fn initialize_with_runtime_and_clock(
        request: InitializeRequest,
        runtime: Arc<BrokerRuntime>,
        clock: Box<dyn Clock>,
    ) -> Result<Self> {
        let startup_started = std::time::Instant::now();
        let mut startup_timing = std::collections::BTreeMap::new();
        let data_root = PathBuf::from(&request.data_root);
        #[expect(
            clippy::disallowed_methods,
            reason = "the broker owns its store for the session's lifetime and orders it behind the writer through `OrderedConnection`"
        )]
        let mut store = SqliteStore::open(&data_root)?;
        startup_timing.insert(
            "database_open_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        store.recover_session_deletions()?;
        startup_timing.insert(
            "deletion_recovery_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        let workspace_kind = runtime.workspace_kind.clone();
        let workspace = match &workspace_kind {
            WorkspaceKind::Git(path) | WorkspaceKind::Untracked(path) => {
                path.to_string_lossy().into_owned()
            }
        };
        let now_ms = clock.now_ms();
        let permission_store = Arc::clone(&runtime.permission_store);
        let permission_coordinator = Arc::clone(&runtime.permission_coordinator);
        let provider = Arc::clone(&runtime.provider);
        let trace = runtime.trace();
        let provider_descriptor = provider.descriptor();
        let requested_new_session_name = request.new_session_name.as_deref().map(str::trim);
        let force_new_session = requested_new_session_name.is_some()
            || request.lease_conflict_action.as_deref() == Some("new");
        let mut created_session = false;
        let mut session = match request.session_id.as_deref().filter(|_| !force_new_session) {
            Some(session_id) => {
                let selected =
                    SessionNotFound::require(store.load_session(session_id)?, session_id)?;
                anyhow::ensure!(
                    selected.background.is_some() == request.background.is_some(),
                    "Background and interactive sessions cannot share a provider conversation"
                );
                anyhow::ensure!(
                    crate::workspace::same(&selected.workspace, &workspace),
                    "session uses a different workspace"
                );
                anyhow::ensure!(
                    selected.provider == request.provider.kind.as_str(),
                    "session uses a different configured provider"
                );
                store.acquire_session_lease(session_id, &request.client_id)?
            }
            None => {
                let latest_session = if force_new_session {
                    None
                } else {
                    store
                        .list_session(Some(&workspace))?
                        .into_iter()
                        .find(|session| {
                            session.background.is_none()
                                && session.purpose == crate::session::SessionPurpose::Conversation
                                && session.provider == request.provider.kind.as_str()
                        })
                };
                match latest_session {
                    Some(session) => {
                        store.acquire_session_lease(&session.id, &request.client_id)?
                    }
                    None => {
                        created_session = true;
                        let preference = if request.background.is_some() {
                            None
                        } else {
                            store.load_preference(&workspace, request.provider.kind.as_str())?
                        };
                        let session_id = Uuid::new_v4().to_string();
                        let mut session = HarnessSession {
                            aside: None,
                            purpose: crate::session::SessionPurpose::Conversation,
                            primary_agent_id: HarnessSession::primary_agent_id(&session_id),
                            id: session_id,
                            name: requested_new_session_name.unwrap_or_default().to_owned(),
                            workspace,
                            provider: request.provider.kind.to_string(),
                            provider_session_id: None,
                            provider_checkpoint_id: None,
                            provider_fork_state: ProviderForkState::Ready,
                            model: preference
                                .as_ref()
                                .map_or(request.model, |value| value.model.clone()),
                            model_label: None,
                            provider_label: provider_descriptor.label.clone(),
                            resolved_model: None,
                            effort: preference
                                .as_ref()
                                .map_or(request.effort, |value| value.effort.clone()),
                            plan_executor: preference
                                .as_ref()
                                .map_or_else(Default::default, |value| value.plan_executor.clone()),
                            plan_compact: preference
                                .as_ref()
                                .is_some_and(|value| value.plan_compact),
                            approval_timeout: Default::default(),
                            tool_timeout: Default::default(),
                            context_window: None,
                            service_tier: preference
                                .as_ref()
                                .map(|value| value.service_tier)
                                .unwrap_or_default(),
                            access: preference
                                .as_ref()
                                .map(|value| value.access.clone())
                                .unwrap_or_default(),
                            shell: preference
                                .as_ref()
                                .map_or(Default::default(), |preference| preference.shell),
                            approval_mode: ApprovalMode::Ask,
                            active_task: None,
                            created_at_ms: now_ms,
                            updated_at_ms: now_ms,
                            lease_owner: None,
                            native_fork: false,
                            native_compact: provider_descriptor.capability.native_compact,
                            context_usage: None,
                            background: request.background.clone(),
                            auto_compaction: Default::default(),
                            last_compaction_turn: None,
                        };
                        let model_setting = preference
                            .as_ref()
                            .and_then(|preference| preference.model_setting.get(&session.model));
                        session.context_window =
                            model_setting.and_then(|setting| setting.context_window.clone());
                        session.model_label =
                            model_setting.and_then(|setting| setting.label.clone());
                        session.acquire_lease(&request.client_id)?;
                        session
                    }
                }
            }
        };
        startup_timing.insert(
            "session_selected_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        if !created_session && store.recover_configuration_intent(&data_root, &session.id)? {
            session = store
                .load_session(&session.id)?
                .context("recovered configuration session is missing")?;
        }
        if session.provider_label.is_empty() {
            session.provider_label = provider_descriptor.label.clone();
        }
        session.updated_at_ms = now_ms;
        let shared = crate::settings::SettingsStore::open(&data_root)?.get(
            &session.workspace,
            &session.id,
            &session.provider,
        )?;
        shared
            .desired
            .apply(&mut session, provider_descriptor.capability.native_compact)?;
        if created_session && request.background.is_none() {
            shared.apply_provider_defaults(&mut session)?;
        }
        if request.background.is_some() {
            session.access.write_access = crate::session::WriteScope::Off;
        }
        permission_coordinator.configure_timeout(&session.id, session.approval_timeout)?;
        startup_timing.insert(
            "settings_applied_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        store.lock_session(&session.id)?;
        let submission_store =
            crate::provider::submission::SubmissionStore::open(&data_root, trace.clone())?;
        let exchanges = store.list_exchange(&session.id)?;
        let input_ids: Option<Vec<_>> = exchanges
            .iter()
            .map(|exchange| {
                if exchange.agent_id == session.primary_agent_id
                    && exchange.session_id == session.id
                {
                    exchange
                        .input_message_id
                        .clone()
                        .filter(|id| !id.trim().is_empty())
                } else {
                    None
                }
            })
            .collect();
        // Provider-only preparation is disposable across runtimes. Keep all local metadata,
        // including retained interrupted exchanges, while replacing only an unused provider identity.
        if session.background.is_none()
            && session.purpose == crate::session::SessionPurpose::Conversation
            && session.provider_fork_state == ProviderForkState::Ready
            && session.provider_checkpoint_id.is_none()
            && !store.has_fork_lineage(&session.id)?
            && let Some(input_ids) = input_ids
            && submission_store.proves_session_unconsumed(
                &crate::provider::ThreadOwner::root(&session.id),
                &session.workspace,
                provider_descriptor.kind,
                &input_ids,
            )?
        {
            session.provider_session_id = None;
        }
        store.save_session(&session)?;
        if store
            .list_agent_run(&session.id)?
            .iter()
            .all(|agent| agent.id != session.primary_agent_id)
        {
            store.save_agent_run(&Agent::primary(&session.id, now_ms))?;
        }
        let previous_preference = store.load_preference(&session.workspace, &session.provider)?;
        if previous_preference.is_none() && request.background.is_none() {
            store.save_preference(
                &session.workspace,
                &session.provider,
                &preference_for_session(&session, None),
            )?;
        }
        let mut capability = provider_descriptor.capability;
        capability.native_fork = session.native_fork || capability.native_fork;
        capability.native_compact = session.native_compact || capability.native_compact;
        store.interrupt_detached_execution(&session.id)?;
        session = store
            .load_session(&session.id)?
            .context("session disappeared during recovery")?;
        let mut agent_registry = load_agent_registry(&store, &session.id)?;
        startup_timing.insert(
            "session_recovered_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        let agent_coordinator = crate::agent::AgentCoordinator::new(session.id.clone());
        agent_coordinator.set_enabled(shared.desired.subagent_tools.unwrap_or(false));
        runtime
            .lsp
            .set_enabled(&session.id, shared.desired.lsp_tools.unwrap_or(false));
        let repaired = agent_coordinator.restore(
            store.list_agent_run(&session.id)?,
            store.list_agent_task(&session.id)?,
        )?;
        for agent in repaired {
            store.save_agent_run(&agent)?;
            agent_registry.insert(agent);
        }
        let mut agent_provider_factory = crate::agent::AgentProviderFactory::new(
            request.provider.clone(),
            request.provider_launch_by_provider.clone(),
            permission_coordinator.clone(),
            trace.clone(),
            data_root.clone(),
        );
        agent_provider_factory.profiles = runtime.profiles.clone();
        agent_coordinator.set_profiles(agent_provider_factory.profiles.clone());
        let presentation = Arc::new(std::sync::Mutex::new(SessionPresentation::new(
            session.id.clone(),
        )));
        let check_runner = crate::checks::CheckSupervisor::open(
            crate::checks::CheckSupervisorContext {
                session_id: session.id.clone(),
                data_root: data_root.clone(),
                workspace: PathBuf::from(&session.workspace),
                root: crate::storage::state::directory(&data_root)
                    .join(&session.id)
                    .join("checks"),
            },
            Arc::clone(&presentation),
            store.objects.clone(),
            Arc::clone(&runtime.repositories),
            trace.clone(),
        )?;
        let plan_file = Arc::new(PlanFileStore::new(&data_root, &session.workspace).unwrap());
        let questions = Arc::new(crate::question::QuestionController::open(
            &data_root,
            &session.id,
        )?);
        let input = Arc::new(crate::input::SessionInputQueue::open(
            &data_root,
            &session.id,
            &submission_store,
        )?);
        let observer = Arc::new(observation::BrokerObserver {
            input: input.clone(),
            activity: RwLock::new(Default::default()),
            questions: questions.clone(),
            admission: store.session_admission(&session.id)?,
            input_schedule: tokio::sync::watch::channel(Default::default()).0,
            lsp: runtime.lsp.clone(),
            status_thoughts: AtomicBool::new(shared.desired.status_thoughts.unwrap_or(true)),
            check_editing: Arc::new(AtomicBool::new(
                shared.desired.check_editing.unwrap_or(false),
            )),
            completion_queue: crate::checks::CompletionQueue::new(input),
            session_events: RwLock::new(
                runtime
                    .execution_registry
                    .observer(crate::provider::ThreadOwner::root(&session.id))?,
            ),
            data_root: data_root.clone(),
            session_id: session.id.clone(),
            runtime_epoch: Uuid::new_v4().to_string(),
            session_metadata: store.session_metadata_writer(&session.id, &request.client_id)?,
            snapshot_revision: AtomicU64::new(0),
            metadata: RwLock::new(observation::ObservationMetadata {
                revision: 0,
                capability: capability.clone(),
                manifest: None,
                active_wait: None,
                no_checkpoint: matches!(workspace_kind, WorkspaceKind::Untracked(_)),
            }),
            plan_file: Arc::clone(&plan_file),
            permission_coordinator: Arc::clone(&permission_coordinator),
            permission_store: Arc::clone(&permission_store),
            agent_coordinator: Arc::clone(&agent_coordinator),
            check_runner: Arc::clone(&check_runner),
            trace: Arc::clone(&trace),
        });
        startup_timing.insert(
            "services_created_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        let mut broker = Self {
            question_model: None,
            question_discussion: false,
            question_events: None,
            questions,
            task_controller: Arc::new(crate::task::SessionTaskController::new(
                observer.admission.clone(),
            )?),
            submission_store,
            context_usage_fresh: false,
            lsp: runtime.lsp.clone(),
            session_messages: Arc::clone(&runtime.session_messages),
            active_operation_id: None,
            observer,
            repositories: Arc::clone(&runtime.repositories),
            checkpoint: match &workspace_kind {
                WorkspaceKind::Git(workspace) if request.background.is_none() => {
                    Some(GitCheckpoint::new(workspace))
                }
                _ => None,
            },
            diff: Arc::clone(&runtime.diff),
            store,
            plan_file,
            check_runner,
            check_setup: false,
            retracted_exchange_id: None,
            workspace_kind,
            data_root,
            client_id: request.client_id,
            provider_launch: request.provider,
            execution_registry: runtime.execution_registry.clone(),
            provider,
            session,
            capability,
            latest_manifest: None,
            configuration_identity: None,
            goal_max_turns: request.goal_max_turns,
            permission_store,
            permission_coordinator,
            trace,
            plan_review: Arc::new(crate::plan::review_document::PlanReviewStore::default()),
            revision_review: Arc::new(crate::plan::revision_review::RevisionReview::default()),
            event_sink: None,
            clock,
            exchange_runtime: None,
            host_settlement: Default::default(),
            pending_host_settlement: None,
            settlement_events: Vec::new(),
            agent_registry,
            native_providers_by_binding: HashMap::new(),
            agent_coordinator,
            agent_provider_factory,
            child_provider_by_agent: HashMap::new(),
            child_execution_by_agent: HashMap::new(),
            mcp_notice: HashMap::new(),
            child_exchange_runtime_by_agent: HashMap::new(),
            presentation,
            prompt_admission: None,
            input_submission: None,
            input_purpose: crate::input::InputPurpose::User,
            input_wakeup: None,
            active_wait_projection: None,
            parent_input: Default::default(),
            submitted_attachments: Vec::new(),
            timeline_reconciled_after_dispatch: false,
            turn_cancellation: Arc::new(TurnCancellation::new()),
            startup_timing,
        };
        broker.store.current_exchange_state(&broker.session.id)?;
        broker.refresh_input_schedule()?;
        let initial_timeline = broker.snapshot()?.timeline;
        broker.startup_timing.insert(
            "timeline_loaded_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        broker
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .initialize(initial_timeline)?;
        broker.startup_timing.insert(
            "presentation_initialized_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        Ok(broker)
    }

    /// Share the out-of-band cancellation signal with the broker transport loop.
    pub fn turn_cancellation(&self) -> Arc<TurnCancellation> {
        Arc::clone(&self.turn_cancellation)
    }

    /// Clone the provider control boundary for out-of-band active-turn requests.
    pub fn provider_handle(&self) -> Arc<Provider> {
        Arc::clone(&self.provider)
    }

    pub(crate) fn checks(&self) -> Arc<crate::checks::CheckSupervisor> {
        Arc::clone(&self.check_runner)
    }

    /// Share root-owned admission without requiring the active provider's broker lock.
    pub(crate) fn agent_coordinator(&self) -> Arc<crate::agent::AgentCoordinator> {
        self.agent_coordinator.clone()
    }

    /// Build the stable provider-catalog identity used outside serialized broker dispatch.
    pub fn provider_catalog_request(&self) -> Result<ProviderCatalogRequest> {
        Ok(ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            access: self.session.access.clone(),
            approval_mode: self.session.approval_mode,
            provider_session_id: self
                .session
                .provider_session_id
                .clone()
                .map(TryInto::try_into)
                .transpose()?,
        })
    }

    /// Identify the interactive conversation owner whose native history opens on first use.
    ///
    /// Startup registers this owner so the first lazy open can publish into it. The bridge opens
    /// or resumes the native conversation under input admission, and `retain_session_identity`
    /// commits the identity that the open publishes.
    pub(crate) fn interactive_owner(&self) -> Option<crate::provider::ThreadOwner> {
        (self.session.background.is_none()
            && self.session.purpose == crate::session::SessionPurpose::Conversation
            && self.provider_fork_state() == crate::session::ProviderForkState::Ready)
            .then(|| crate::provider::ThreadOwner::root(&self.session.id))
    }

    fn control_turn_context(&self, mode: PromptMode) -> Option<crate::tools::ControlTurnContext> {
        if mode == PromptMode::Background {
            let mut context = crate::tools::ControlTurnContext::inactive(mode);
            context.task_controller = Some(self.task_controller.clone());
            context.permission_required = true;
            context.permission_snapshot = self.permission_store.read().ok().map(|store| {
                crate::tools::PermissionAuthoritySnapshot::capture(
                    self.permission_store.clone(),
                    &store,
                )
            });
            if let Ok((generation, task)) = self.task_controller.callback_snapshot() {
                context.task_generation = Some(generation);
                context.task_permit = task.map(|task| task.permit());
            }
            return Some(context);
        }
        let plan = self
            .current_plan_id()
            .ok()
            .flatten()
            .as_deref()
            .and_then(|plan_id| self.store.load_plan(plan_id).ok().flatten());
        let document = plan.as_ref().and_then(|plan| {
            self.plan_file
                .read_working_document(&self.session.id, &plan.id)
                .ok()
        });
        let callback_snapshot = self.task_controller.callback_snapshot().ok();
        let permission_snapshot = self.permission_store.read().ok().map(|store| {
            crate::tools::PermissionAuthoritySnapshot::capture(
                self.permission_store.clone(),
                &store,
            )
        });
        Some(crate::tools::ControlTurnContext {
            provider_completion: Default::default(),
            questions: Some(self.questions.clone()),
            question_events: self.question_events.clone(),
            live_host_effects: None,
            observed_file_changes: None,
            question_invocation: None,
            question_exchange_id: self
                .exchange_runtime
                .as_ref()
                .map(|runtime| runtime.exchange_id.clone()),
            question_discussion: self.question_discussion,
            task_controller: Some(self.task_controller.clone()),
            task_start: match &self.input_purpose {
                crate::input::InputPurpose::TaskEntry { start, .. } => start.clone(),
                _ => None,
            },
            task_authority_progress: None,
            task_permit: callback_snapshot
                .as_ref()
                .and_then(|(_, task)| task.as_ref().map(|task| task.permit())),
            task_generation: callback_snapshot.map(|(generation, _)| generation),
            permission_snapshot,
            permission_required: true,
            file_permissions: None,
            submission_cancelled: Some(self.turn_cancellation.submission_flag()),
            checkpoint: self
                .exchange_runtime
                .as_ref()
                .and_then(|runtime| runtime.tool_gate.clone()),
            agent: None,
            checks: None,
            check_shell: self.session.shell,
            mode,
            plan_state: plan.as_ref().map(|plan| plan.state),
            plan_document: document,
            plan_authoring: if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion)
                && !self.question_discussion
                && plan
                    .as_ref()
                    .is_some_and(|plan| plan.state != crate::plan::PlanState::ConfiguringChecks)
            {
                plan.as_ref().map(|plan| {
                    crate::plan::PlanAuthoringScope::new(
                        self.plan_file.clone(),
                        self.session.id.clone(),
                        plan.id.clone(),
                        self.session.access.write_access,
                    )
                })
            } else {
                None
            },
            // Plan, Execute, and Goal turns carry their workflow material as the `<task>` section.
            task_instructions: match mode {
                PromptMode::Plan | PromptMode::PlanDiscussion => {
                    plan.as_ref()
                        .map(|plan| crate::provider::TaskInstructions::Plan {
                            request: plan.request.clone(),
                        })
                }
                PromptMode::ExecutePlan => self.execution_task_instructions().ok().flatten(),
                PromptMode::GoalContinuation => self.current_goal().ok().flatten().map(|goal| {
                    crate::provider::TaskInstructions::Goal(forge_provider_api::forge::GoalTask {
                        objective: goal.objective,
                    })
                }),
                _ => None,
            },
            resolved_question_digest_set: plan
                .as_ref()
                .map(|plan| {
                    plan.question_ledger
                        .resolution
                        .iter()
                        .map(|item| item.content_digest.clone())
                        .collect()
                })
                .unwrap_or_default(),
            has_active_elicitation: plan.as_ref().is_some_and(|plan| plan.elicitation.is_some()),
            has_active_execution: self.current_execution().ok()?.is_some_and(|execution| {
                !matches!(
                    execution.state,
                    PlanExecutionState::Complete | PlanExecutionState::Cancelled
                )
            }),
            workspace_root: Some(PathBuf::from(&self.session.workspace)),
            repository: None,
            harness: None,
            lsp: None,
            aside: crate::tools::AsideToolScope::open(&self.session, &self.session_messages),
            execution: None,
        })
    }

    /// Share the out-of-band permission lane with the broker transport loop.
    pub fn permission_coordinator(&self) -> Arc<PermissionCoordinator> {
        Arc::clone(&self.permission_coordinator)
    }

    /// Build the initial snapshot returned by the initialize response.
    pub fn snapshot(&self) -> Result<BrokerSnapshot> {
        self.observer.publish(self)
    }

    fn session_view(&self) -> Result<HarnessSession> {
        let mut session = self.session.clone();
        let committed = self
            .store
            .load_session(&session.id)?
            .context("observed session is missing")?;
        session.name = committed.name;
        session.active_task = committed.active_task;
        Ok(session)
    }

    /// Generate one tool-free Background exchange using its durable provider conversation.
    pub(crate) async fn generate_background(
        &mut self,
        input: String,
        identity: crate::background::BackgroundIdentity,
        selection: crate::settings::ValidatedModelSelection,
        policy: crate::session::AutoCompaction,
        sink: publication::SessionUpdateSink,
    ) -> Result<String> {
        anyhow::ensure!(
            self.session.background.is_some(),
            "Background request requires a Background session"
        );
        if let Some(previous) = &self.session.background
            && !previous.reason.is_empty()
            && self
                .store
                .current_exchange_state(&self.session.id)?
                .latest
                .is_none()
        {
            self.store
                .save_session_event(&crate::timeline::SessionEventRecord {
                    id: uuid::Uuid::new_v4().to_string(),
                    session_id: self.session.id.clone(),
                    owner: crate::timeline::TimelineOwner::session(&self.session.id),
                    created_at_ms: self.clock.now_ms(),
                    detail: crate::timeline::SessionEventKind::Configuration {
                        operation_id: uuid::Uuid::new_v4().to_string(),
                        state: "completed".into(),
                        field: vec!["background_session".into()],
                        reason: Some(previous.reason.clone()),
                    },
                })?;
        }
        self.session.background = Some(identity);
        selection.apply(&mut self.session);
        self.context_usage_fresh &= self.session.context_usage.is_some();
        self.session.access.write_access = crate::session::WriteScope::Off;
        self.session.auto_compaction = policy;
        self.checkpoint = None;
        self.event_sink = Some(sink);
        self.save_session()?;
        // Background generation is outside the `<forge>` protocol: its input is plain text.
        let result = self
            .run_interaction_input(
                input.clone(),
                PromptMode::Background,
                Some(ExchangeAdmission::chat(input.clone())),
                ProviderInput::kind(forge_provider_api::forge::TurnKind::Background {
                    input: forge_provider_api::PromptInput::Text(input),
                }),
            )
            .await;
        self.event_sink = None;
        result?;
        self.save_session()?;
        let exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("Background exchange is missing")?;
        let text = exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .filter(|message| {
                message.kind() == crate::turn::MessageKind::Assistant
                    && message.delivery() == crate::turn::MessageDelivery::Final
            })
            .map(|message| message.text())
            .collect::<Vec<_>>()
            .join("\n");
        Ok(text)
    }

    /// Retain a Background failure even when no assistant response can be published.
    pub(crate) fn background_failure(&mut self, stage: &str, reason: String) -> Result<()> {
        self.store
            .save_session_event(&crate::timeline::SessionEventRecord {
                id: uuid::Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                owner: crate::timeline::TimelineOwner::session(&self.session.id),
                created_at_ms: self.clock.now_ms(),
                detail: crate::timeline::SessionEventKind::Configuration {
                    operation_id: uuid::Uuid::new_v4().to_string(),
                    state: "failed".into(),
                    field: vec![stage.into()],
                    reason: Some(reason),
                },
            })?;
        Ok(())
    }

    /// Compact once per newly completed provider turn while retaining the full timeline.
    pub(crate) async fn auto_compact(&mut self) -> Result<()> {
        let policy = &self.session.auto_compaction;
        if !policy.enabled || !self.context_usage_fresh {
            return Ok(());
        }
        anyhow::ensure!(
            self.capability.native_compact,
            "This provider does not support automatic compaction"
        );
        policy.validate(self.session.context_usage.as_ref())?;
        if self
            .store
            .list_session_event(&self.session.id)?
            .iter()
            .any(|event| {
                matches!(&event.detail, SessionEventKind::Compaction { compaction }
                if compaction.state == crate::provider::CompactionState::Running
                    && compaction.thread_id == crate::provider::ThreadOwner::root(&self.session.id).thread_id().as_str())
            })
        {
            return Ok(());
        }
        if !policy.due(
            self.session.context_usage.as_ref(),
            self.capability.native_compact,
        ) {
            return Ok(());
        }
        let latest_exchange = self.store.latest_exchange(&self.session.id)?;
        let latest = latest_exchange
            .as_ref()
            .and_then(|exchange| exchange.turn.last())
            .map(|turn| turn.id().to_owned());
        let native_compacted = latest_exchange.as_ref().is_some_and(|exchange| exchange.node_list.iter().any(|node|
            matches!(node, crate::exchange::ExchangeNode::Compaction { compaction, .. }
                if matches!(compaction.state, crate::provider::CompactionState::Completed | crate::provider::CompactionState::Running))));
        if latest.is_none() || latest == self.session.last_compaction_turn {
            return Ok(());
        }
        self.session.last_compaction_turn = latest;
        self.save_session()?;
        if native_compacted {
            return Ok(());
        }
        self.compact_session(false).await?;
        Ok(())
    }

    /// Shares the read-only session boundary with out-of-band controls.
    pub(crate) fn observer(&self) -> Arc<observation::BrokerObserver> {
        Arc::clone(&self.observer)
    }

    /// Route one client request through the owning feature boundary.
    pub async fn dispatch(&mut self, request: Request) -> DispatchResult {
        let id = request.id;
        let method = match RuntimeMethod::decode(&request.method) {
            Ok(method) => method,
            Err(error) => {
                return DispatchResult {
                    response: Response::failure(id, "unknown_method", error.to_string()),
                    event: Vec::new(),
                };
            }
        };
        if method == RuntimeMethod::SessionPromptSubmit {
            return self.dispatch_local_prompt(request).await;
        }
        let edit_trace = matches!(
            method,
            RuntimeMethod::SessionPlanEntityRename
                | RuntimeMethod::SessionPlanTestDelete
                | RuntimeMethod::SessionPlanCheckUpdate
        )
        .then(|| {
            crate::plan::edit_trace::PlanEditTrace::new(
                Arc::clone(&self.trace),
                &self.session.id,
                &request.method,
                &request.params,
            )
        });
        let dispatch_stage = edit_trace
            .as_ref()
            .map(|trace| trace.stage("broker.dispatch"));
        self.timeline_reconciled_after_dispatch = false;
        self.trace.record(
            &self.session.id,
            "lua.rpc.received",
            json!({
                "id": id,
                "method": request.method,
                "params": request.params.clone(),
            }),
        );
        self.record_plan_trace(
            "request_received",
            json!({ "request_id": id, "method": request.method }),
        );
        if method == RuntimeMethod::SessionCommandApply {
            self.active_operation_id = request
                .params
                .get("operation_id")
                .and_then(Value::as_str)
                .map(str::to_owned);
        } else {
            self.active_operation_id = None;
        }
        let mutation_stage = edit_trace
            .as_ref()
            .map(|trace| trace.stage("broker.mutation"));
        let command = (|| -> Result<_> {
            let mut candidate = request.clone();
            if candidate.params.get("operation_id").is_none()
                && let Some(id) = candidate
                    .params
                    .pointer("/_command/id")
                    .and_then(Value::as_str)
                    .map(str::to_owned)
            {
                candidate.params["operation_id"] = json!(id);
            }
            if method == RuntimeMethod::SessionQuestionContinue {
                anyhow::bail!("workflow continuation must route through the session coordinator");
            }
            crate::task::command::SessionCommand::parse_request(&candidate)
        })();
        let mut outcome = match command {
            Ok(Some(command)) => self.commit_session_command(command),
            Err(error) => Err(error),
            Ok(None) => {
                match crate::attachment::resolve_input(&self.data_root, &request.params).await {
                    Ok(attachments) => {
                        self.submitted_attachments = attachments;
                        let outcome = self.dispatch_inner(method, request.params).await;
                        self.submitted_attachments.clear();
                        outcome
                    }
                    Err(error) => Err(error),
                }
            }
        };
        if let Some(stage) = mutation_stage {
            stage.complete(json!({"status":if outcome.is_ok() { "completed" } else { "failed" }}));
        }
        if let Err(error) = self.refresh_input_schedule() {
            outcome = Err(error.context("publish input deadline"));
        }
        match outcome {
            Ok((value, mut event)) => {
                if method.settles_task() {
                    self.active_wait_projection = None;
                }
                let reconcile_stage = edit_trace
                    .as_ref()
                    .map(|trace| trace.stage("broker.timeline_reconcile"));
                let timeline_patch = match self.reconcile_after_dispatch() {
                    Ok(timeline_patch) => timeline_patch,
                    Err(error) => {
                        return DispatchResult {
                            response: Response::failure(
                                id,
                                "timeline_projection_failed",
                                error.to_string(),
                            ),
                            event: Vec::new(),
                        };
                    }
                };
                if let Some(stage) = reconcile_stage {
                    stage.complete(json!({}));
                }
                if !timeline_patch.is_empty() {
                    match self.event("document_changed", timeline_patch.notification()) {
                        Ok(timeline_event) => event.push(timeline_event),
                        Err(error) => {
                            return DispatchResult {
                                response: Response::failure(
                                    id,
                                    "timeline_patch_failed",
                                    error.to_string(),
                                ),
                                event: Vec::new(),
                            };
                        }
                    }
                }
                let response_stage = edit_trace
                    .as_ref()
                    .map(|trace| trace.stage("broker.response_and_trace"));
                match Response::success(id, value) {
                    Ok(response) => {
                        self.trace.record(
                            &self.session.id,
                            "lua.rpc.completed",
                            json!({
                                "id": id,
                                "response": response.clone(),
                                "event": event.clone(),
                            }),
                        );
                        self.record_plan_trace(
                            "request_succeeded",
                            json!({ "request_id": id, "event_count": event.len() }),
                        );
                        if let Some(stage) = response_stage {
                            stage.complete(json!({"count":event.len()}));
                        }
                        if let Some(stage) = dispatch_stage {
                            stage.complete(json!({"status":"completed"}));
                        }
                        DispatchResult { response, event }
                    }
                    Err(error) => DispatchResult {
                        response: Response::failure(id, "encode_response", error.to_string()),
                        event: Vec::new(),
                    },
                }
            }
            Err(error) => {
                if method.settles_task() {
                    self.active_wait_projection = None;
                }
                let mut event = Vec::new();
                if let Ok(timeline_patch) = self.reconcile_after_dispatch()
                    && !timeline_patch.is_empty()
                    && let Ok(timeline_event) =
                        self.event("document_changed", timeline_patch.notification())
                {
                    event.push(timeline_event);
                }
                if let Some(retracted) = error.downcast_ref::<TurnRetracted>() {
                    return DispatchResult {
                        response: Response::failure_with_data(
                            id,
                            "turn_retracted",
                            retracted.to_string(),
                            json!({ "prompt": retracted.prompt }),
                        ),
                        event,
                    };
                }
                let code = request_failure_code(&error);
                self.record_plan_trace("request_failed", json!({ "request_id": id, "code": code }));
                self.trace.record(
                    &self.session.id,
                    "lua.rpc.failed",
                    json!({
                        "id": id,
                        "code": code,
                        "error": format!("{error:#}"),
                        "event": event.clone(),
                    }),
                );
                DispatchResult {
                    response: Response::failure(id, code, crate::storage::error_message(&error)),
                    event,
                }
            }
        }
    }

    /// Route one client request while forwarding provider updates as they arrive.
    pub async fn dispatch_stream(
        &mut self,
        request: Request,
        event_sink: publication::SessionUpdateSink,
    ) -> DispatchResult {
        self.event_sink = Some(event_sink);
        let result = Box::pin(self.dispatch(request)).await;
        self.event_sink = None;
        result
    }

    // Select the handler before polling so unrelated workflow futures do not share a stack frame.
    fn dispatch_inner(
        &mut self,
        method: RuntimeMethod,
        params: Value,
    ) -> futures_util::future::BoxFuture<'_, Result<(Value, Vec<SessionEvent>)>> {
        match method {
            RuntimeMethod::SessionCheckComplete => Box::pin(async move {
                let result = (serde_json::to_value(self.snapshot()?)?, Vec::new());
                self.settle_automatic_checks(result, PromptMode::Chat).await
            }),
            RuntimeMethod::SessionCheckOverride => Box::pin(async {
                anyhow::bail!(
                    "check preferences must be submitted through the session command owner"
                )
            }),
            RuntimeMethod::SessionCheckList | RuntimeMethod::SessionCheckStatus => {
                Box::pin(self.project_checks(method, params))
            }
            RuntimeMethod::SessionCommandApply => self.transition_task(params),
            RuntimeMethod::SessionProviderModelList => Box::pin(self.list_provider_model()),
            RuntimeMethod::SessionAgentStart => Box::pin(self.start_agent(params)),
            RuntimeMethod::SessionPromptSubmit => Box::pin(self.submit_prompt(params)),
            RuntimeMethod::SessionPlanAccept => Box::pin(std::future::ready(
                self.dispatch_session_command("session.plan.accept", params),
            )),
            RuntimeMethod::SessionPlanCheckUpdate => Box::pin(self.update_plan_checks(params)),
            RuntimeMethod::SessionPlanChangesRequest => Box::pin(std::future::ready(
                self.dispatch_session_command("session.plan.changes.request", params),
            )),
            RuntimeMethod::SessionPlanQuestionAnswer => Box::pin(async {
                anyhow::bail!("plan review questions must route through the session coordinator")
            }),
            RuntimeMethod::SessionQuestionContinue => Box::pin(async move {
                if let Some(target) = params.get("target") {
                    let active = self
                        .snapshot()?
                        .active_elicitation
                        .context("question set is no longer active")?;
                    let owner_id = active.exchange_id.as_ref().or(active.plan_id.as_ref());
                    anyhow::ensure!(
                        serde_json::to_value(active.owner)? == target["owner"]
                            && owner_id.map(String::as_str) == target["id"].as_str()
                            && Some(active.elicitation.question_set.id.as_str())
                                == target["set_id"].as_str()
                            && Some(u64::from(active.elicitation.revision))
                                == target["revision"].as_u64(),
                        "question target changed before continuation"
                    );
                    anyhow::ensure!(
                        active
                            .elicitation
                            .question_set
                            .questions
                            .iter()
                            .all(|question| active
                                .elicitation
                                .answer
                                .iter()
                                .any(|answer| answer.question_id == question.id)),
                        "question answers have not all committed"
                    );
                }
                self.continue_question().await
            }),
            RuntimeMethod::SessionGoalSet => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.set", params),
            )),
            RuntimeMethod::SessionGoalPause => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.pause", json!({})),
            )),
            RuntimeMethod::SessionGoalResume => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.resume", json!({})),
            )),
            RuntimeMethod::SessionGoalClear => Box::pin(std::future::ready(
                self.dispatch_session_command("session.goal.clear", json!({})),
            )),
            RuntimeMethod::SessionExchangeRollbackPrepare => {
                Box::pin(self.prepare_rollback(params))
            }
            RuntimeMethod::SessionExchangeRollbackApply => Box::pin(self.apply_rollback(params)),
            RuntimeMethod::SessionExchangeRecovery => Box::pin(self.restore_recovery(params)),
            RuntimeMethod::SessionResume => Box::pin(self.resume_session(params)),
            RuntimeMethod::SessionConfigure => Box::pin(self.configure_session(params)),
            RuntimeMethod::SessionCompact => Box::pin(self.compact_session(true)),
            RuntimeMethod::SessionTurnCancel => Box::pin(self.cancel_settled_execution()),
            _ => Box::pin(self.dispatch_local(method, params)),
        }
    }

    async fn dispatch_local(
        &mut self,
        method: RuntimeMethod,
        params: Value,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if !matches!(
            method,
            RuntimeMethod::SessionStateGet
                | RuntimeMethod::SessionList
                | RuntimeMethod::SessionPreview
        ) {
            self.save_session()?;
        }
        match method {
            RuntimeMethod::SessionCheckEditingSet => {
                anyhow::bail!("check editing policy requires the session exchange owner")
            }
            RuntimeMethod::SessionTaskGet => Ok((
                serde_json::to_value(self.task_controller.current()?)?,
                Vec::new(),
            )),
            RuntimeMethod::SessionLspAuthorize
            | RuntimeMethod::SessionAttachmentImport
            | RuntimeMethod::SessionLspRespond
            | RuntimeMethod::SessionLspAnalyze
            | RuntimeMethod::SessionCommandOperationGet
            | RuntimeMethod::SessionHealthGet => {
                anyhow::bail!("control requires the service coordinator")
            }
            RuntimeMethod::SessionStateGet => {
                Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()))
            }
            RuntimeMethod::HostTraceStatus => {
                Ok((serde_json::to_value(self.trace.status())?, Vec::new()))
            }
            RuntimeMethod::HostTraceConfigure => self.configure_trace(params),
            RuntimeMethod::SessionTraceClear => Ok((
                serde_json::to_value(self.trace.clear_session(&self.session.id)?)?,
                Vec::new(),
            )),
            RuntimeMethod::ProviderModelPinList | RuntimeMethod::ProviderModelPinSet => {
                anyhow::bail!("model pins require the service coordinator")
            }
            RuntimeMethod::SessionAgentMessageSend
            | RuntimeMethod::SessionAgentFollowup
            | RuntimeMethod::SessionAgentInterrupt
            | RuntimeMethod::SessionAgentInterruptAll => {
                let target = || required_text(&params, "target");
                let operation = match method {
                    RuntimeMethod::SessionAgentMessageSend => {
                        crate::agent::AgentOperation::Message {
                            target: target()?,
                            message: required_text(&params, "message")?,
                        }
                    }
                    RuntimeMethod::SessionAgentFollowup => crate::agent::AgentOperation::Followup {
                        target: target()?,
                        message: required_text(&params, "message")?,
                    },
                    RuntimeMethod::SessionAgentInterrupt => {
                        crate::agent::AgentOperation::Interrupt { target: target()? }
                    }
                    _ => crate::agent::AgentOperation::InterruptAll,
                };
                let mut event = Vec::new();
                let result = self
                    .handle_agent_request(
                        crate::agent::AgentRequest {
                            operation_id: Uuid::new_v4().to_string(),
                            source: crate::agent::AgentSource::User,
                            operation,
                        },
                        None,
                        &mut event,
                    )
                    .await?;
                Ok((result, event))
            }
            RuntimeMethod::SessionAgentModelList => {
                let provider =
                    forge_providers::registry::select(&required_text(&params, "provider")?)?;
                let model = self
                    .agent_provider_factory
                    .catalog(
                        provider,
                        self.provider_request(
                            ProviderInput::from_user_text(String::new()),
                            PromptMode::Chat,
                        )?,
                    )
                    .await?;
                Ok((serde_json::to_value(model)?, Vec::new()))
            }
            RuntimeMethod::AgentProfileGet => Ok((
                serde_json::to_value(self.agent_provider_factory.profiles.snapshot()?)?,
                Vec::new(),
            )),
            RuntimeMethod::AgentProfileSave => {
                let revision = required_text(&params, "expected_revision")?;
                let document = serde_json::from_value(params["document"].clone())?;
                Ok((
                    serde_json::to_value(
                        self.agent_provider_factory
                            .profiles
                            .save(&revision, document)?,
                    )?,
                    Vec::new(),
                ))
            }
            RuntimeMethod::PermissionsDocumentOpen => self.observer.open_permission_document(),
            RuntimeMethod::PermissionsDocumentSave => {
                self.observer.save_permission_document(params)
            }
            RuntimeMethod::SessionPermissionSet => {
                let mut action = params;
                action["action"] = json!("permission");
                self.transition_task(action).await
            }
            RuntimeMethod::SessionPromptHistoryRecord => self
                .observer
                .record_prompt_history(params, self.clock.now_ms()),
            RuntimeMethod::SessionPlanEntityRename => self.rename_plan_entity(params),
            RuntimeMethod::SessionPlanTestDelete => self.delete_plan_tests(params),
            RuntimeMethod::SessionPlanCancel => {
                self.dispatch_session_command("session.plan.cancel", params)
            }
            RuntimeMethod::SessionPlanGet => Ok((self.observer.plan(params)?, Vec::new())),
            RuntimeMethod::SessionPlanDeclarationGet => self.observer.read_plan_declaration(params),
            RuntimeMethod::SessionPlanList => self.observer.list_replanning_choices(),
            RuntimeMethod::SessionQuestionAnswer => self.answer_question(params),
            RuntimeMethod::SessionQuestionSkip => self.skip_question(params),
            RuntimeMethod::SessionInputTimeoutConfigure => {
                anyhow::bail!("input timeout settings require the service coordinator")
            }
            RuntimeMethod::SessionExchangeList => Ok((
                serde_json::to_value(self.observer.list_exchange_review()?)?,
                Vec::new(),
            )),
            RuntimeMethod::SessionExchangeCommentSave => {
                self.observer.save_exchange_comment(params)
            }
            RuntimeMethod::SessionExchangeRollbackPreview => Ok((
                self.observer
                    .query(RuntimeMethod::SessionExchangeRollbackPreview, params)?
                    .context("rollback preview observation is unavailable")?,
                Vec::new(),
            )),
            RuntimeMethod::SessionNew => self.new_session(params).await,
            RuntimeMethod::SessionList => self.observer.list_session(params),
            RuntimeMethod::SessionPreview => self.observer.preview_session(params),
            RuntimeMethod::SessionRename => {
                let renamed = self.observer.rename_session(params)?;
                self.session.name = required_text(&renamed.0, "name")?;
                Ok(renamed)
            }
            RuntimeMethod::SessionDelete => self.observer.delete_session(params),
            RuntimeMethod::SessionFork => anyhow::bail!(
                "session.fork must route through the process-wide session coordinator"
            ),
            RuntimeMethod::HostShutdown => {
                self.release_lease()?;
                Ok((json!({ "host.shutdown": true }), Vec::new()))
            }
            RuntimeMethod::SessionInputApply
            | RuntimeMethod::SessionInputGet
            | RuntimeMethod::SessionInputOperationGet
            | RuntimeMethod::SessionTurnSteer
            | RuntimeMethod::SessionApprovalResolve
            | RuntimeMethod::SessionProviderSkillList
            | RuntimeMethod::SettingsSkillSetEnabled
            | RuntimeMethod::SessionProviderMcpList
            | RuntimeMethod::SessionProviderCapabilitiesGet
            | RuntimeMethod::SettingsGet
            | RuntimeMethod::SettingsUpdate
            | RuntimeMethod::SettingsImportPreview
            | RuntimeMethod::SettingsImportApply
            | RuntimeMethod::SettingsMcpSetEnabled => {
                anyhow::bail!("Harness control method requires the process coordinator")
            }
            _ => anyhow::bail!("method requires asynchronous handler routing"),
        }
    }

    fn current_plan_id(&self) -> Result<Option<String>> {
        Ok(self
            .task_controller
            .current()?
            .and_then(|task| match task.workflow.clone() {
                crate::task::WorkflowSnapshot::Plan { plan } => Some(plan.artifact.id),
                crate::task::WorkflowSnapshot::Execute { execution } => {
                    Some(execution.record.plan_id)
                }
                crate::task::WorkflowSnapshot::Goal { .. } => None,
            }))
    }

    fn publish_plan(&mut self, plan: &PlanRecord) -> Result<()> {
        let current = self.task_controller.current()?;
        if let Some(task) = current.filter(|task| {
            matches!(&task.workflow,
            crate::task::WorkflowSnapshot::Plan { plan: active } if active.artifact.id == plan.id)
        }) {
            self.task_controller.apply(
                &mut self.store,
                &task.permit(),
                crate::task::TaskEvent::PlanArtifactUpdated(Box::new(plan.clone())),
                self.clock.now_ms(),
            )
        } else {
            self.store.save_plan(plan)
        }
    }

    fn task_prompt_mode(&self) -> Result<PromptMode> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(PromptMode::Chat);
        };
        if task.status() == crate::task::TaskStatus::Paused || !task.entry_accepted() {
            return Ok(PromptMode::Chat);
        }
        Ok(match task.workflow.clone() {
            crate::task::WorkflowSnapshot::Goal { .. } => PromptMode::GoalContinuation,
            crate::task::WorkflowSnapshot::Execute { .. }
                if task.next_action() == crate::task::TaskNextAction::Checks =>
            {
                PromptMode::Chat
            }
            crate::task::WorkflowSnapshot::Execute { .. } => PromptMode::ExecutePlan,
            crate::task::WorkflowSnapshot::Plan { plan } => {
                if plan.artifact.state == PlanState::AwaitingReview {
                    PromptMode::PlanDiscussion
                } else {
                    PromptMode::Plan
                }
            }
        })
    }

    fn current_execution(&self) -> Result<Option<PlanExecutionRecord>> {
        Ok(self.capture_execution()?.map(|capture| capture.record))
    }

    fn reserve_execution_continuation(&mut self, completed: crate::task::TaskTurnId) -> Result<()> {
        let Some(task) = self.task_controller.current()? else {
            return Ok(());
        };
        let crate::task::WorkflowSnapshot::Execute { .. } = &task.workflow else {
            return Ok(());
        };
        if matches!(
            task.next_action(),
            crate::task::TaskNextAction::Wait | crate::task::TaskNextAction::Finished
        ) {
            return Ok(());
        }
        let now_ms = self.clock.now_ms();
        if task.next_action() == crate::task::TaskNextAction::Checks {
            self.task_controller.reserve_checks(
                &mut self.store,
                &task.permit(),
                completed,
                now_ms,
            )?;
        } else {
            let goal = self.active_goal()?;
            let prompt = self
                .plan_goal_prompt(&goal, PlanExecutionPromptKind::Continue)?
                .context("execution continuation has no accepted plan")?;
            self.task_controller.reserve_continuation(
                &mut self.store,
                &task.permit(),
                completed,
                prompt,
                "The accepted plan requires another execution phase".into(),
                now_ms,
            )?;
        }
        Ok(())
    }

    fn current_task_is_plan(&self) -> Result<bool> {
        Ok(matches!(
            self.task_prompt_mode()?,
            PromptMode::Plan | PromptMode::PlanDiscussion
        ))
    }

    async fn pause_task(&mut self, clear: bool) -> Result<(Value, Vec<SessionEvent>)> {
        self.retry_finalization().await?;
        let current = self.task_controller.current()?;
        if let Some(task) = current {
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                if clear {
                    crate::task::TaskChange::Clear
                } else {
                    crate::task::TaskChange::Pause
                },
                self.clock.now_ms(),
            )?;
        }
        Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()))
    }

    /// Applies a transition only after the service has collected the previous owner.
    /// Apply one task transition without retaining unrelated operation futures while it runs.
    pub(crate) fn transition_task(
        &mut self,
        action: Value,
    ) -> futures_util::future::BoxFuture<'_, Result<(Value, Vec<SessionEvent>)>> {
        let command = match required_text(&action, "action") {
            Ok(command) => command,
            Err(error) => return Box::pin(std::future::ready(Err(error))),
        };
        match command.as_str() {
            "undo" => Box::pin(self.transition_undo(action)),
            "pause" | "clear" => Box::pin(self.transition_pause(command == "clear")),
            "permission" => Box::pin(
                self.apply_configuration(action, configuration::ConfigurationCommand::Permission),
            ),
            "configure" => Box::pin(
                self.apply_configuration(action, configuration::ConfigurationCommand::Configure),
            ),
            "compact" => Box::pin(
                self.apply_configuration(action, configuration::ConfigurationCommand::Compact),
            ),
            "resume" | "plan" | "goal" | "execute" | "attach_plan" | "fork" | "replan"
            | "continue" => Box::pin(std::future::ready(
                self.dispatch_session_command("session.command.apply", action),
            )),
            _ => Box::pin(std::future::ready(Err(anyhow::anyhow!(
                "unknown task action"
            )))),
        }
    }

    async fn transition_undo(&mut self, action: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let rollback = action
            .get("rollback")
            .cloned()
            .context("undo requires rollback selection")?;
        anyhow::ensure!(
            rollback["scope"] == "conversation",
            "undo requires conversation rollback scope"
        );
        self.pause_task(false).await?;
        self.cancel_settled_execution().await?;
        self.rollback_conversation(rollback).await
    }

    async fn transition_pause(&mut self, clear: bool) -> Result<(Value, Vec<SessionEvent>)> {
        self.dispatch_session_command(
            "session.command.apply",
            json!({"action":if clear { "clear" } else { "pause" }}),
        )
    }

    async fn resume_exchange(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let text = required_text(&params, "text")?;
        let interaction = self
            .store
            .latest_exchange(&self.session.id)?
            .context("no Harness interaction is available to resume")?;
        anyhow::ensure!(
            interaction.state() == ExchangeState::Running
                && interaction.execution_started_at_ms().is_none()
                && interaction
                    .turn
                    .iter()
                    .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "the latest Harness exchange is not paused for resumption"
        );
        let pending_plan = if self.active_plan_awaits_input()? {
            Some(self.active_elicitation_plan()?)
        } else {
            None
        };
        let elicitation = pending_plan
            .as_ref()
            .and_then(|plan| plan.elicitation.as_ref())
            .or(interaction.elicitation());
        if let Some(elicitation) = elicitation {
            if !elicitation.clarification_active {
                return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
            }
            let clarification = interaction
                .node_list
                .iter()
                .rev()
                .find_map(|node| match node {
                    ExchangeNode::ExchangeInput { prompt }
                        if prompt.intent == crate::exchange::InputIntent::Clarification
                            && prompt.question.as_ref().is_some_and(|target| {
                                target.set_id == elicitation.question_set.id
                            }) =>
                    {
                        Some(prompt)
                    }
                    _ => None,
                });
            let clarification = clarification
                .context("interrupted clarification has no recorded question input")?;
            // Both are the user's words: the recorded clarification and the resumed reply.
            let follow_up =
                forge_provider_api::PromptInput::Text(format!("{}\n\n{text}", clarification.text));
            let prompt = if pending_plan.is_some() {
                PlanPrompt::clarification(elicitation, follow_up)
            } else {
                PlanPrompt::question_follow_up(elicitation, follow_up)
            };
            let mode = self.task_prompt_mode()?;
            let (value, event) = self
                .run_interaction(ProviderInput::new(prompt), mode, None)
                .await?;
            return if let Some(plan) = pending_plan {
                self.finish_plan_clarification(&plan.id, value, event).await
            } else {
                self.finish_question_clarification(value, event).await
            };
        }
        anyhow::ensure!(
            !interaction.awaiting_input(),
            "the latest Harness exchange is awaiting a user decision"
        );
        let mode = self.task_prompt_mode()?;
        if matches!(mode, PromptMode::Plan | PromptMode::PlanDiscussion) {
            self.run_planning_interaction(ProviderInput::from_user_text(text), None)
                .await
        } else {
            self.run_interaction(ProviderInput::from_user_text(text), mode, None)
                .await
        }
    }

    async fn start_agent(&mut self, mut params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let operation_id = params
            .as_object_mut()
            .and_then(|params| params.remove("operation_id"))
            .and_then(|value| value.as_str().map(str::to_owned))
            .or_else(|| self.active_operation_id.clone())
            .unwrap_or_else(|| Uuid::new_v4().to_string());
        let spawn: crate::agent::SpawnAgentRequest = serde_json::from_value(params)?;
        let request = crate::agent::AgentRequest {
            operation_id,
            source: crate::agent::AgentSource::User,
            operation: crate::agent::AgentOperation::Spawn(spawn),
        };
        let mut event = Vec::new();
        let result = self.handle_agent_request(request, None, &mut event).await?;
        Ok((result, event))
    }

    async fn handle_agent_request(
        &mut self,
        request: crate::agent::AgentRequest,
        parent: Option<&mut Exchange>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<Value> {
        use crate::agent::{AgentOperation, AgentSource};
        anyhow::ensure!(
            request.source != AgentSource::Model || self.agent_coordinator.enabled(),
            "model-facing subagent tools are disabled"
        );
        if let Some(task) = self
            .store
            .list_agent_task(&self.session.id)?
            .into_iter()
            .find(|task| task.operation_id == request.operation_id)
        {
            let agent = self.agent_coordinator.resolve(&task.agent_id)?;
            let same_work = match &request.operation {
                AgentOperation::Spawn(spawn) => {
                    task.ordinal == 1
                        && task.message == spawn.text
                        && spawn
                            .profile
                            .as_deref()
                            .is_none_or(|profile| profile == agent.definition)
                }
                AgentOperation::Followup { target, message } => {
                    task.ordinal > 1
                        && task.message == *message
                        && self.agent_coordinator.resolve(target)?.id == agent.id
                }
                _ => false,
            };
            anyhow::ensure!(
                same_work
                    && task.source == request.source
                    && parent
                        .as_deref()
                        .is_none_or(|parent| task.parent_exchange_id == parent.id),
                "operation identity was reused for different child work"
            );
            return Ok(json!({"agent":agent,"task":task,"status":"admitted","replayed":true}));
        }
        enum ChildWork<'request> {
            Spawn(&'request crate::agent::SpawnAgentRequest),
            Followup {
                target: &'request str,
                message: &'request str,
            },
        }
        let work = match &request.operation {
            AgentOperation::Activity { target, limit } => {
                let limit = limit.unwrap_or(5);
                let (agent, task) = self.agent_coordinator.inspection(target)?;
                let saved;
                let exchange = if let Some(runtime) = self
                    .child_exchange_runtime_by_agent
                    .get(&agent.id)
                    .filter(|runtime| runtime.exchange.id == task.id)
                {
                    &runtime.exchange
                } else {
                    saved = self
                        .store
                        .load_exchange_metadata(&task.id)?
                        .context("child activity exchange missing")?;
                    &saved
                };
                let awaiting_approval =
                    self.permission_coordinator
                        .pending_list()?
                        .iter()
                        .any(|approval| {
                            approval.execution_owner.as_ref().is_some_and(|owner| {
                                owner.agent_id().as_str() == agent.id.as_str()
                                    && *owner.session_id()
                                        == crate::provider::SessionId::from(
                                            self.session.id.as_str(),
                                        )
                            })
                        });
                let result = serde_json::to_value(crate::agent::AgentActivity::capture(
                    &agent,
                    &task,
                    exchange,
                    limit,
                    self.clock.now_ms(),
                    awaiting_approval,
                )?)?;
                return Ok(result);
            }
            AgentOperation::ToolOutput {
                call_ref,
                offset,
                limit,
            } => {
                let reference = crate::agent::AgentToolReference::decode(call_ref)?;
                anyhow::ensure!(
                    reference.session_id == self.session.id,
                    "tool reference belongs to another root session"
                );
                let agent = self.agent_coordinator.resolve(&reference.agent_id)?;
                let task = self
                    .agent_coordinator
                    .task(&reference.exchange_id)
                    .context("tool reference has no admitted child run")?;
                anyhow::ensure!(
                    task.agent_id == agent.id && task.session_id == self.session.id,
                    "tool reference crosses child ownership"
                );
                let exchange = self
                    .store
                    .load_exchange_metadata(&reference.exchange_id)?
                    .context("tool reference exchange missing")?;
                anyhow::ensure!(
                    exchange.agent_id == agent.id && exchange.session_id == self.session.id,
                    "tool reference crosses exchange ownership"
                );
                let tool = exchange
                    .turn
                    .iter()
                    .find(|turn| turn.id() == reference.turn_id)
                    .and_then(|turn| {
                        exchange
                            .tools_for_turn(turn.id())
                            .find(|tool| tool.id == reference.tool_id)
                    })
                    .context("tool reference does not identify a recorded call")?;
                let page = self.store.tool_output_page(
                    &exchange.id,
                    &reference.turn_id,
                    &reference.tool_id,
                    *offset,
                    *limit,
                )?;
                return Ok(
                    json!({"call_ref":call_ref,"status":crate::agent::tool_activity_status(tool),
                    "failed":tool.failed,"output":page.output,"offset":offset,
                    "next_offset":page.next_offset,"total_chars":page.total_chars}),
                );
            }
            AgentOperation::List { prefix } => {
                let mut entry = Vec::new();
                if prefix
                    .as_deref()
                    .is_none_or(|prefix| "/root".starts_with(prefix))
                {
                    entry.push(json!({"agent":Agent::primary(&self.session.id, self.session.created_at_ms),"latest_task":null}));
                }
                entry.extend(
                    self.agent_coordinator
                        .snapshot(prefix.as_deref())
                        .into_iter()
                        .map(|(agent, task)| json!({"agent":agent,"latest_task":task})),
                );
                return Ok(json!({"agents":entry}));
            }
            AgentOperation::Message { target, message } => {
                anyhow::ensure!(
                    !message.trim().is_empty(),
                    "informational message must contain nonempty text"
                );
                let mut agent = self.agent_coordinator.resolve(target)?;
                let id = request.operation_id.clone();
                if let Some(existing) = agent.inbox.iter().find(|message| message.id == id) {
                    anyhow::ensure!(
                        existing.message == *message,
                        "operation identity was reused for a different informational message"
                    );
                    return Ok(serde_json::to_value(existing)?);
                }
                let mut delivery = crate::agent::MessageDelivery::Queued;
                if let Some(registry) = self.child_execution_by_agent.get(&agent.id) {
                    let owner = crate::provider::ThreadOwner::new(
                        self.session.id.clone().into(),
                        crate::provider::AgentId::new(agent.id.clone())?,
                    )
                    .expect("registered session and agent identities are nonempty");
                    let engine = registry.owner(owner).await?;
                    match engine.steer_current(message.clone().into()).await {
                        Ok(()) => delivery = crate::agent::MessageDelivery::Steered,
                        Err(error) => {
                            self.trace.record(&self.session.id, "agent.message_queued", json!({
                                "agent_id":agent.id,"reason":"steering_not_accepted","error":error.to_string()}));
                        }
                    }
                }
                let message = crate::agent::AgentMessage {
                    id,
                    ordinal: agent.inbox.last().map_or(1, |message| message.ordinal + 1),
                    message: message.clone(),
                    delivery,
                    task_id: None,
                    created_at_ms: self.clock.now_ms(),
                };
                agent.inbox.push(message.clone());
                self.store.save_agent_run(&agent)?;
                if let Some(task) = self
                    .agent_coordinator
                    .snapshot(None)
                    .into_iter()
                    .find(|(candidate, _)| candidate.id == agent.id)
                    .and_then(|(_, task)| task)
                {
                    self.agent_coordinator.retain(agent.clone(), task)?;
                }
                self.agent_registry.insert(agent);
                return Ok(serde_json::to_value(message)?);
            }
            AgentOperation::Interrupt { target } => {
                self.interrupt_managed_agent(target, event).await?;
                return Ok(json!({"status":"interrupting","target":target}));
            }
            AgentOperation::InterruptAll => {
                let targets = self
                    .agent_coordinator
                    .snapshot(None)
                    .into_iter()
                    .map(|(agent, _)| agent.id)
                    .collect::<Vec<_>>();
                self.interrupt_managed_agents(&targets, event).await?;
                return Ok(json!({"status":"interrupting"}));
            }
            AgentOperation::Spawn(spawn) => ChildWork::Spawn(spawn),
            AgentOperation::Followup { target, message } => ChildWork::Followup { target, message },
        };
        let mut admitted_parent = if let Some(parent) = parent.as_deref() {
            parent.clone()
        } else {
            Exchange::agent_admission(
                &self.session.id,
                self.store.next_exchange_ordinal(&self.session.id)?,
                "Child task admission".into(),
                self.clock.now_ms(),
            )
        };
        let admission = match work {
            ChildWork::Spawn(spawn) => {
                let mut root = self.provider_request(
                    ProviderInput::from_user_text(String::new()),
                    PromptMode::Chat,
                )?;
                if root.model == "default"
                    && let Some(model) = &self.session.resolved_model
                {
                    root.model = model.clone();
                }
                let selection = self
                    .agent_provider_factory
                    .resolve(
                        &root,
                        forge_providers::registry::select(&self.session.provider)?,
                        spawn,
                    )
                    .await?;
                anyhow::ensure!(
                    request.source != AgentSource::Model || self.agent_coordinator.enabled(),
                    "model-facing subagent tools are disabled"
                );
                self.agent_provider_factory
                    .profiles
                    .resolve(Some(&selection.profile))?;
                self.agent_coordinator.reserve_spawn(
                    &request,
                    spawn,
                    selection,
                    &admitted_parent.id,
                    self.clock.now_ms(),
                )?
            }
            ChildWork::Followup { target, message } => {
                let agent = self.agent_coordinator.resolve(target)?;
                self.agent_provider_factory
                    .profiles
                    .resolve(Some(&agent.definition))?;
                self.agent_coordinator.reserve_followup(
                    &request,
                    target,
                    message,
                    &admitted_parent.id,
                    self.clock.now_ms(),
                )?
            }
        };
        let persisted = (|| -> Result<()> {
            admitted_parent.append_agent_task(&admission.task)?;
            self.store.admit_agent_task(
                &admission.agent,
                &admission.task,
                &admitted_parent,
                admission.continuation_authorization(),
            )
        })();
        if let Err(error) = persisted {
            return Err(match admission.cancel() {
                Ok(()) => error,
                Err(cancellation) => {
                    error.context(format!("release child reservation: {cancellation:#}"))
                }
            });
        }
        let (admission, launch) = match admission.commit()? {
            crate::agent::AgentCommit::Admitted(work) => (work, true),
            crate::agent::AgentCommit::Existing(work) => (work, false),
        };
        self.agent_registry.insert(admission.agent.clone());
        if let Some(parent) = parent {
            *parent = admitted_parent;
        } else {
            admitted_parent.finish(
                crate::exchange::ExchangeOutcome::Complete,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(&admitted_parent)?;
        }
        self.trace.record(&self.session.id,"agent.admitted",json!({"agent_id":admission.agent.id,"exchange_id":admission.task.id,"parent_exchange_id":admission.task.parent_exchange_id,"source":admission.task.source}));
        let warning = if matches!(request.operation, AgentOperation::Spawn(_)) {
            self.agent_provider_factory.profiles.snapshot()?.error
        } else {
            None
        };
        let result = json!({"agent":admission.agent,"task":admission.task,"status":"admitted","warning":warning});
        if launch && admission.launch {
            self.launch_managed_agent(admission, event).await?;
        }
        self.publish_agent_state(event).await?;
        Ok(result)
    }

    async fn publish_agent_state(&mut self, event: &mut Vec<SessionEvent>) -> Result<()> {
        let effects = self.prepare_agent_state()?;
        event.extend(self.prepare_provider_delivery(effects)?.deliver().await?);
        Ok(())
    }

    /// Stage the agent snapshot and timeline in their original delivery order.
    fn prepare_agent_state(&mut self) -> Result<AppliedProviderEvent> {
        let snapshot = self.snapshot()?;
        let update = SessionUpdate::Agent {
            snapshot: snapshot.agent,
        };
        let patch = self
            .presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .reconcile_with_status_thoughts(
                snapshot.timeline,
                self.observer.status_thoughts.load(Ordering::Relaxed),
            )?;
        Ok(AppliedProviderEvent::Effects(vec![
            AppliedProviderEvent::Update(update),
            AppliedProviderEvent::Timeline(patch),
        ]))
    }

    async fn cancel_agent_approvals(
        &mut self,
        owner: &crate::provider::ThreadOwner,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let cancelled = self
            .permission_coordinator
            .cancel_owner_approvals(owner, None)
            .await?;
        for approval in cancelled {
            if let Some(runtime) = self
                .child_exchange_runtime_by_agent
                .get_mut(owner.agent_id().as_str())
            {
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                self.store.save_exchange(&runtime.exchange)?;
            }
            self.emit_session_update(
                SessionUpdate::Approval {
                    execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                    execution_owner: approval
                        .execution_owner
                        .clone()
                        .context("approval lost original owner")?,
                    update: Box::new(approval_cancelled(&approval, self.clock.now_ms())?),
                },
                event,
            )
            .await?;
        }
        Ok(())
    }

    async fn interrupt_managed_agent(
        &mut self,
        target: &str,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        self.interrupt_managed_agents(&[target.to_owned()], event)
            .await
    }

    /// Interrupt every target before retrying any retained process cleanup.
    ///
    /// Each target is flagged first, so one child whose cleanup retry fails cannot leave a later
    /// child running under a cancel that already reported failure. Every failure is reported
    /// together after all targets were attempted, and the agent snapshot is published either way.
    async fn interrupt_managed_agents(
        &mut self,
        targets: &[String],
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let mut failures = Vec::new();
        let mut flagged = Vec::new();
        for target in targets {
            match self.flag_managed_agent_interrupt(target, event).await {
                Ok(agent) => flagged.push(agent),
                Err(error) => failures.push(error.context(format!("interrupt child {target}"))),
            }
        }
        for agent in flagged {
            let agent_id = agent.id.clone();
            if let Err(error) = self.retry_managed_agent_cleanup(agent).await {
                failures.push(error.context(format!("collect child {agent_id}")));
            }
        }
        let published = self.publish_agent_state(event).await;
        aggregate_failures(failures)?;
        published
    }

    /// Cancel one child's queued work and approvals, then persist each cancelled task.
    async fn flag_managed_agent_interrupt(
        &mut self,
        target: &str,
        event: &mut Vec<SessionEvent>,
    ) -> Result<Agent> {
        let agent = self.agent_coordinator.resolve(target)?;
        let owner = crate::provider::ThreadOwner::new(
            (self.session.id.clone()).into(),
            crate::provider::AgentId::new(agent.id.clone())?,
        )
        .expect("registered session and agent identities are nonempty");
        let cancelled = self.agent_coordinator.interrupt(target)?;
        self.cancel_agent_approvals(&owner, event).await?;
        for mut task in cancelled {
            task.state = ExchangeState::Cancelled;
            task.completed_at_ms = Some(self.clock.now_ms());
            if let Some(mut exchange) = self.store.load_exchange(&task.id)? {
                exchange.finish(
                    crate::exchange::ExchangeOutcome::Cancelled,
                    self.clock.now_ms(),
                )?;
                self.store.complete_agent_task(&agent, &task, &exchange)?;
            }
            self.agent_coordinator.retain(agent.clone(), task)?;
        }
        Ok(agent)
    }

    /// Retry a flagged child's retained process cleanup and release its execution on success.
    async fn retry_managed_agent_cleanup(&mut self, agent: Agent) -> Result<()> {
        let owner = crate::provider::ThreadOwner::new(
            (self.session.id.clone()).into(),
            crate::provider::AgentId::new(agent.id.clone())?,
        )
        .expect("registered session and agent identities are nonempty");
        if let Some(mut task) = self.agent_coordinator.cleanup_task(&agent.id) {
            let provider = self
                .child_provider_by_agent
                .get(&agent.id)
                .cloned()
                .context("child cleanup obligation has no retained provider adapter")?;
            tokio::time::timeout(std::time::Duration::from_secs(30), async {
                provider.session_close_owner(&owner).await?;
                provider.shutdown().await
            })
            .await
            .context("child cleanup retry exceeded 30 seconds")??;
            let mut agent = agent;
            agent.state = AgentState::Ready;
            agent.updated_at_ms = self.clock.now_ms();
            task.error = None;
            let mut exchange = self
                .store
                .load_exchange(&task.id)?
                .context("child cleanup exchange missing")?;
            exchange.finalization_error = None;
            self.store.complete_agent_task(&agent, &task, &exchange)?;
            self.agent_registry.insert(agent.clone());
            self.agent_coordinator.retain(agent, task.clone())?;
            self.agent_coordinator.settled(
                owner.agent_id().as_str(),
                &task.id,
                crate::agent::AgentSettlementDisposition::Ready,
            )?;
            self.child_provider_by_agent
                .remove(owner.agent_id().as_str());
            self.child_execution_by_agent
                .remove(owner.agent_id().as_str());
        }
        Ok(())
    }

    async fn launch_managed_agent(
        &mut self,
        admission: crate::agent::AgentWork,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if let Some((owner, task_id, error)) = self.begin_managed_agent(admission)? {
            self.complete_managed_agent(owner, task_id, Err(error), Vec::new(), None, event)
                .await?;
        }
        Ok(())
    }

    /// Transfer admitted child work to its owned runtime without awaiting it.
    fn begin_managed_agent(
        &mut self,
        mut admission: crate::agent::AgentWork,
    ) -> Result<Option<(crate::provider::ThreadOwner, String, anyhow::Error)>> {
        let owner = admission.owner().clone();
        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str()),
            "child execution belongs to another session"
        );
        let prepared = (|| -> Result<_> {
            // Messages the parent queued before this task started travel with it, each as its
            // own `<agent_message type="message">` after the task.
            let mut task = forge_provider_api::forge::AgentTask {
                task_id: admission.task.id.clone(),
                from: forge_provider_api::forge::ROOT_AGENT_PATH.into(),
                text: admission.task.message.clone(),
                messages: Vec::new(),
            };
            for incoming in admission
                .agent
                .inbox
                .iter_mut()
                .filter(|message| message.delivery == crate::agent::MessageDelivery::Queued)
            {
                task.messages.push(parent_message(incoming.message.clone()));
                incoming.delivery = crate::agent::MessageDelivery::Attached;
                incoming.task_id = Some(admission.task.id.clone());
            }
            let mut request = self.agent_provider_factory.request(
                &self.provider_request(
                    ProviderInput::from_user_text(String::new()),
                    PromptMode::Chat,
                )?,
                &admission,
                task,
            )?;
            let provider = self.agent_provider_factory.build(
                admission
                    .agent
                    .selection
                    .as_ref()
                    .context("child selection missing")?
                    .provider
                    .clone(),
            )?;
            let cancellation = self
                .agent_coordinator
                .cancellation(&admission.agent.id, &admission.task.id)?;
            let lsp_lease = if self.lsp.enabled(&self.session.id) {
                let policy = self
                    .permission_store
                    .read()
                    .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                    .compiled()
                    .clone();
                let (scope, lease) = self.lsp.scope(
                    self.session.id.clone(),
                    admission.task.id.clone(),
                    PathBuf::from(&self.session.workspace),
                    policy,
                    cancellation.clone(),
                );
                let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
                context.lsp = Some(scope);
                context.workspace_root = Some(PathBuf::from(&self.session.workspace));
                request.control_context = Some(context);
                Some(lease)
            } else {
                None
            };
            self.store.save_agent_run(&admission.agent)?;
            self.agent_registry.insert(admission.agent.clone());
            self.agent_coordinator
                .retain(admission.agent.clone(), admission.task.clone())?;
            self.child_provider_by_agent
                .insert(admission.agent.id.clone(), provider.clone());
            let execution_registry = Arc::new(
                crate::provider::session_execution::SessionExecutionRegistry::new(
                    provider.clone(),
                    self.trace.clone(),
                ),
            );
            self.child_execution_by_agent
                .insert(admission.agent.id.clone(), execution_registry.clone());
            Ok(crate::agent::AgentRuntime {
                execution_registry,
                provider,
                tool_timeout: crate::settings::SettingsStore::read_only(&self.data_root)?
                    .get(
                        &self.session.workspace,
                        &self.session.id,
                        &self.session.provider,
                    )?
                    .desired
                    .tool_timeout
                    .unwrap_or(self.session.tool_timeout),
                trace: self.trace.clone(),
                submission_store: self.submission_store.clone(),
                permission: self.permission_coordinator.clone(),
                request,
                task_id: admission.task_id().to_string(),
                cancellation,
                sender: self.agent_coordinator.sender(),
                lsp_lease,
            })
        })();
        match prepared {
            Ok(runtime) => {
                let sender = self.agent_coordinator.sender();
                let task_id = admission.task.id;
                tokio::spawn(async move {
                    if let Err(error) = runtime.run().await {
                        let _ = sender
                            .send(crate::agent::AgentCommand::Completed {
                                owner,
                                task_id,
                                output: Box::new(Err(error)),
                                host_file_changes: Vec::new(),
                                cleanup_error: Some(
                                    "child event delivery or cleanup failed".into(),
                                ),
                            })
                            .await;
                    }
                });
            }
            Err(error) => {
                return Ok(Some((owner, admission.task.id, error)));
            }
        }
        Ok(None)
    }

    /// Settle one queued child command, returning `false` once the idle lane is empty.
    ///
    /// The broker lock can be released between calls, so a caller that drains command by command
    /// never holds it across more than one command's provider work. When the lane is empty and no
    /// root turn is active, this call launches the queued children that capacity admits before
    /// returning `false`.
    pub(crate) async fn drain_next_agent_command(
        &mut self,
        parent: Option<&mut Exchange>,
        event: &mut Vec<SessionEvent>,
    ) -> bool {
        let Some(command) = self.agent_coordinator.try_receive_idle() else {
            if parent.is_none() && !self.agent_coordinator.foreground_claimed() {
                let launched = self.launch_ready_agents(event).await;
                self.report_agent_command_failure(launched, event);
            }
            return false;
        };
        let settled = self.apply_agent_command(command, parent, event).await;
        self.report_agent_command_failure(settled, event);
        true
    }

    /// Launch every queued child the coordinator admits now, in queue order.
    async fn launch_ready_agents(&mut self, event: &mut Vec<SessionEvent>) -> Result<()> {
        while let Some(work) = self.agent_coordinator.ready_launch()? {
            Box::pin(self.launch_managed_agent(work, event)).await?;
        }
        Ok(())
    }

    /// Publish one child command failure in band so the batch's other events still reach the
    /// editor.
    fn report_agent_command_failure(&self, result: Result<()>, event: &mut Vec<SessionEvent>) {
        let Err(error) = result else {
            return;
        };
        self.trace.record(
            &self.session.id,
            "agent.command_failed",
            json!({"error": format!("{error:#}")}),
        );
        event.push(SessionEvent {
            session_id: self.session.id.clone(),
            event: "provider_event".into(),
            payload: json!({"kind":"error","text":format!("Agent coordinator failed: {error:#}")}),
        });
    }

    /// Apply one dequeued child command; its failure never discards an earlier command's events.
    async fn apply_agent_command(
        &mut self,
        command: crate::agent::AgentCommand,
        parent: Option<&mut Exchange>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        match command {
            crate::agent::AgentCommand::Request {
                request,
                authority,
                target,
                response,
                task_admission,
            } => {
                if response.is_closed() {
                    return Ok(());
                }
                // The active root turn authenticates a live model request against its own
                // exchange, so a drain without that parent hands the request to the turn.
                if parent.is_none()
                    && request.source == crate::agent::AgentSource::Model
                    && authority
                        .as_ref()
                        .is_some_and(|authority| !*authority.expired.borrow())
                    && self.agent_coordinator.foreground_claimed()
                {
                    self.agent_coordinator.defer_to_foreground(
                        crate::agent::AgentCommand::Request {
                            request,
                            authority,
                            target,
                            response,
                            task_admission,
                        },
                    );
                    return Ok(());
                }
                let authenticated = if request.source == crate::agent::AgentSource::Model {
                    authority.as_ref().is_some_and(|authority| {
                        !*authority.expired.borrow()
                            && authority.owner
                                == crate::provider::ThreadOwner::root(&self.session.id)
                            && parent
                                .as_deref()
                                .is_some_and(|parent| parent.id == authority.parent_exchange_id)
                    })
                } else {
                    authority.is_none()
                };
                let scoped = target
                    .as_ref()
                    .map(|target| self.agent_coordinator.validate_target(target))
                    .transpose();
                let operation = request.operation_id.clone();
                let result = if let Err(error) = scoped {
                    Err(error)
                } else if authenticated {
                    self.handle_agent_request(*request, parent, event).await
                } else {
                    Err(anyhow::anyhow!(
                        "agent request has no active authenticated root authority"
                    ))
                };
                self.agent_coordinator
                    .retain_task_admission(&operation, task_admission);
                let _ = response.send(result);
            }
            crate::agent::AgentCommand::Event {
                owner,
                task_id,
                event: provider_event,
            } => {
                self.observe_managed_agent(owner, &task_id, *provider_event, event)
                    .await?;
            }
            crate::agent::AgentCommand::Completed {
                owner,
                task_id,
                output,
                host_file_changes,
                cleanup_error,
            } => {
                Box::pin(self.complete_managed_agent(
                    owner,
                    task_id,
                    *output,
                    host_file_changes,
                    cleanup_error,
                    event,
                ))
                .await?;
            }
        }
        Ok(())
    }

    async fn observe_managed_agent(
        &mut self,
        owner: crate::provider::ThreadOwner,
        task_id: &str,
        provider_event: OwnerObservation,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let effects =
            self.apply_managed_agent_observation(owner, task_id, provider_event, event)?;
        event.extend(self.prepare_provider_delivery(effects)?.deliver().await?);
        Ok(())
    }

    /// Validate and apply a child observation before staging its publication.
    fn apply_managed_agent_observation(
        &mut self,
        owner: crate::provider::ThreadOwner,
        task_id: &str,
        observation: OwnerObservation,
        _events: &mut Vec<SessionEvent>,
    ) -> Result<AppliedProviderEvent> {
        let observation = match observation {
            OwnerObservation::HostFileChange {
                binding_id,
                scope,
                change,
            } => {
                return self.apply_observed_file_change(&binding_id, &scope, &change);
            }
            OwnerObservation::Event(event) => event,
            OwnerObservation::InputAcceptance {
                scope,
                receipt,
                presentation,
            } => {
                anyhow::ensure!(
                    scope.owner() == owner && scope.exchange_id.as_str() == task_id,
                    "child receipt crosses original task"
                );
                let mut exchange = self
                    .store
                    .load_exchange(task_id)?
                    .context("child receipt has no original exchange")?;
                self.apply_durable_input_acceptance(
                    &mut exchange,
                    &scope,
                    &receipt,
                    &presentation,
                )?;
                if let Some(runtime) = self
                    .child_exchange_runtime_by_agent
                    .get_mut(owner.agent_id().as_str())
                {
                    anyhow::ensure!(
                        runtime.exchange.id == task_id,
                        "child receipt cannot mutate a newer task"
                    );
                    runtime.exchange = exchange;
                }
                return self.prepare_agent_state();
            }
        };

        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str())
                && !owner.is_root(),
            "child event crosses execution ownership"
        );
        if let HarnessEvent::Native {
            source,
            scope,
            event: boxed_native_event,
        } = &observation
            && let ProviderEvent::Agent { agent_id, event } = boxed_native_event.as_ref()
        {
            return self.apply_agent_lifecycle(source, scope, agent_id, event);
        }
        if observation.scope().owner() != owner {
            return self.apply_observed_child(&observation);
        }
        let scope = observation.scope();
        anyhow::ensure!(scope.owner() == owner, "child event crosses original owner");
        if let HarnessEvent::Native { event, .. } = &observation {
            match event.as_ref() {
                ProviderEvent::Thread { thread_id, .. }
                | ProviderEvent::Turn { thread_id, .. }
                | ProviderEvent::Item { thread_id, .. } => {
                    anyhow::ensure!(
                        thread_id == &owner.thread_id(),
                        "child native thread crosses original owner"
                    );
                }
                ProviderEvent::Provider { .. } | ProviderEvent::Agent { .. } => {}
            }
            if let ProviderEvent::Thread {
                event: forge_provider_api::ThreadEvent::IdentityResolved(identity),
                ..
            } = event.as_ref()
            {
                let task = self
                    .agent_coordinator
                    .task(task_id)
                    .context("child identity has no admitted task")?;
                anyhow::ensure!(
                    task.agent_id == owner.agent_id().as_str(),
                    "child identity crosses original task"
                );
                if task.completed_at_ms.is_none() {
                    let mut agent = self.agent_coordinator.resolve(owner.agent_id().as_str())?;
                    agent.provider_thread_id = Some(identity.to_string());
                    self.store.save_agent_run(&agent)?;
                    self.agent_registry.insert(agent.clone());
                    self.agent_coordinator.retain(agent, task)?;
                    return self.prepare_agent_state();
                }
                return Ok(AppliedProviderEvent::None);
            }
        }
        if scope.invocation().is_none() {
            return Ok(match &observation {
                HarnessEvent::Native { event, .. }
                    if matches!(
                        event.as_ref(),
                        ProviderEvent::Thread {
                            event: forge_provider_api::ThreadEvent::TerminalsUpdated(Ok(_)),
                            ..
                        }
                    ) =>
                {
                    AppliedProviderEvent::None
                }
                HarnessEvent::Native { .. } | HarnessEvent::Host { .. } => {
                    SessionUpdate::from_observation(&observation)
                        .map_or(AppliedProviderEvent::None, AppliedProviderEvent::Update)
                }
            });
        }
        let invocation = scope
            .invocation()
            .context("child event requires original invocation")?;
        anyhow::ensure!(
            invocation.exchange_id == forge_provider_api::ExchangeId::from(task_id),
            "child event crosses original task"
        );
        let mut task = self
            .agent_coordinator
            .task(task_id)
            .context("child event has no admitted task")?;
        anyhow::ensure!(
            task.agent_id == owner.agent_id().as_str(),
            "child task has another owner"
        );
        if task.completed_at_ms.is_some() {
            return Ok(AppliedProviderEvent::None);
        }
        let mut runtime = match self
            .child_exchange_runtime_by_agent
            .remove(owner.agent_id().as_str())
        {
            Some(runtime) => {
                anyhow::ensure!(
                    runtime.exchange.id == task_id,
                    "late child event cannot update a newer task"
                );
                runtime
            }
            None => {
                let mut exchange = self
                    .store
                    .load_exchange(task_id)?
                    .context("child task exchange missing")?;
                if exchange.is_finished() {
                    return Ok(AppliedProviderEvent::None);
                }
                exchange.phase = crate::exchange::ExchangePhase::Running;
                exchange.resume(self.clock.now_ms())?;
                ChildExchangeRuntime {
                    exchange,
                    checklist: ChecklistTracker::default(),
                }
            }
        };
        match &observation {
            HarnessEvent::Native { scope, event, .. } => {
                let event = event.as_ref();
                let changed =
                    runtime
                        .exchange
                        .observe_provider_event(scope, event, self.clock.now_ms())?;
                if let ProviderEvent::Turn {
                    event: forge_provider_api::TurnEvent::ChecklistUpdated(update),
                    ..
                } = event
                    && changed
                {
                    runtime.checklist.replace(update);
                    runtime.exchange.checklist = Some(runtime.checklist.snapshot().clone());
                }
            }
            HarnessEvent::Host { scope, event } => {
                runtime
                    .exchange
                    .observe_host_event(scope, event, self.clock.now_ms())?;
                if let HostEvent::PermissionApproval { event, .. } = event {
                    let (id, pending) = match event {
                        PermissionApprovalEvent::Requested(request) => {
                            (request.request_id.as_str(), true)
                        }
                        PermissionApprovalEvent::Resolved(resolution) => {
                            (resolution.request_id.as_str(), false)
                        }
                        PermissionApprovalEvent::Cancelled(cancellation) => {
                            (cancellation.request_id.as_str(), false)
                        }
                    };
                    runtime.exchange.observe_blocker(
                        format!("approval:{id}"),
                        pending,
                        self.clock.now_ms(),
                    );
                }
            }
        }
        match &observation {
            HarnessEvent::Native { event, .. }
                if matches!(
                    event.as_ref(),
                    ProviderEvent::Item {
                        event: forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::Output(
                                forge_provider_api::TextUpdate::Append(_),
                            ),
                        },
                        ..
                    }
                ) =>
            {
                self.store
                    .save_tool_output_delta_behind(&runtime.exchange, event)?
            }
            // Streamed text is shown before it is saved; the writer thread saves it.
            HarnessEvent::Native { event, .. } if event.is_output_delta() => {
                self.store.save_exchange_behind(&runtime.exchange)?
            }
            HarnessEvent::Native { .. } | HarnessEvent::Host { .. } => {
                self.store.save_exchange(&runtime.exchange)?
            }
        }
        if task.state == ExchangeState::Queued {
            task.state = ExchangeState::Running;
        }
        task.last_activity_at_ms = Some(
            task.last_activity_at_ms
                .unwrap_or(i64::MIN)
                .max(self.clock.now_ms()),
        );
        self.store.save_agent_task(&task)?;
        self.agent_coordinator.retain(
            self.agent_coordinator.resolve(owner.agent_id().as_str())?,
            task,
        )?;
        self.child_exchange_runtime_by_agent
            .insert(owner.agent_id().to_string(), runtime);
        let mut effects = Vec::new();
        if let Some(update) = SessionUpdate::from_observation(&observation) {
            effects.push(AppliedProviderEvent::Update(update));
        }
        effects.push(self.prepare_agent_state()?);
        self.agent_coordinator.notify_activity();
        Ok(AppliedProviderEvent::Effects(effects))
    }

    async fn complete_managed_agent(
        &mut self,
        owner: crate::provider::ThreadOwner,
        task_id: String,
        output: Result<crate::provider::ProviderCompletion>,
        host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
        cleanup_error: Option<String>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str())
                && task.agent_id.as_str() == owner.agent_id().as_str(),
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok(());
        }
        // Cancellation removes the child's approvals before any fallible publication, so a
        // failure publishing their terminal events must not also lose the child's completion:
        // an unapplied completion leaves an execution with no runtime that blocks fork and delete.
        let approvals = self.cancel_agent_approvals(&owner, event).await;
        let (effects, next) = self.apply_managed_agent_completion(
            owner,
            task_id,
            output,
            host_file_changes,
            cleanup_error,
        )?;
        event.extend(self.prepare_provider_delivery(effects)?.deliver().await?);
        if let Some(next) = next {
            Box::pin(self.launch_managed_agent(next, event)).await?;
        }
        approvals
    }

    /// Apply a settled child after its pending approvals have been collected.
    fn apply_managed_agent_completion(
        &mut self,
        owner: crate::provider::ThreadOwner,
        task_id: String,
        output: Result<crate::provider::ProviderCompletion>,
        host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
        cleanup_error: Option<String>,
    ) -> Result<(AppliedProviderEvent, Option<crate::agent::AgentWork>)> {
        let mut task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str())
                && task.agent_id.as_str() == owner.agent_id().as_str(),
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok((AppliedProviderEvent::Effects(Vec::new()), None));
        }
        let mut agent = self.agent_coordinator.resolve(owner.agent_id().as_str())?;
        let interrupted = self
            .agent_coordinator
            .cancellation(owner.agent_id().as_str(), &task_id)
            .is_ok_and(|cancel| *cancel.borrow());
        let provider_cancelled = output
            .as_ref()
            .is_err_and(|error| error.is::<TurnCancelled>());
        task.error = cleanup_error
            .clone()
            .or_else(|| output.as_ref().err().map(|error| format!("{error:#}")));
        task.completed_at_ms = Some(self.clock.now_ms());
        if let Ok(output) = &output {
            agent.provider_thread_id = output
                .provider_session_id
                .clone()
                .map(Into::into)
                .or(agent.provider_thread_id);
        }
        let runtime = self
            .child_exchange_runtime_by_agent
            .remove(owner.agent_id().as_str());
        let mut exchange = if let Some(runtime) = runtime {
            runtime.exchange
        } else {
            self.store
                .load_exchange(&task_id)?
                .context("child completion exchange missing")?
        };
        for change in &host_file_changes {
            if change.scope.owner() == owner {
                exchange.record_host_file_change(change)?;
            } else {
                self.record_observed_host_file_change(None, change)?;
            }
        }
        if let Ok(completion) = &output {
            for change in &completion.host_file_changes {
                if change.scope.owner() == owner {
                    exchange.record_host_file_change(change)?;
                } else {
                    self.record_observed_host_file_change(None, change)?;
                }
            }
        }
        task.state = if cleanup_error.is_some() {
            ExchangeState::Failed
        } else if output.is_err() {
            if provider_cancelled || interrupted {
                ExchangeState::Cancelled
            } else {
                ExchangeState::Failed
            }
        } else {
            match exchange.turn.last().map(|turn| turn.state()) {
                Some(crate::turn::TurnState::Finished {
                    outcome:
                        crate::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                }) => ExchangeState::Complete,
                Some(crate::turn::TurnState::Finished {
                    outcome:
                        crate::turn::TurnOutcome::Completed {
                            stop_reason:
                                forge_provider_api::turn::TurnCompletionReason::TokenLimit
                                | forge_provider_api::turn::TurnCompletionReason::RequestLimit
                                | forge_provider_api::turn::TurnCompletionReason::Refusal,
                        }
                        | crate::turn::TurnOutcome::Interrupted,
                }) => ExchangeState::Interrupted,
                Some(crate::turn::TurnState::Finished {
                    outcome: crate::turn::TurnOutcome::Failed { .. },
                }) => ExchangeState::Failed,
                Some(crate::turn::TurnState::Running) | None => {
                    if interrupted {
                        ExchangeState::Cancelled
                    } else {
                        ExchangeState::Failed
                    }
                }
            }
        };
        if task.state == ExchangeState::Complete
            && self.requires_explicit_continuation(&exchange)?
        {
            task.state = ExchangeState::Interrupted;
        }
        exchange.finish(task.state.try_into()?, self.clock.now_ms())?;
        exchange.finalization_error = cleanup_error.clone();
        if exchange.turn.is_empty() {
            for message in agent.inbox.iter_mut().filter(|message| {
                message.task_id.as_deref() == Some(task_id.as_str())
                    && message.delivery == crate::agent::MessageDelivery::Attached
            }) {
                message.delivery = crate::agent::MessageDelivery::Queued;
                message.task_id = None;
            }
        }
        let assistant_message: Vec<_> = exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .filter(|message| message.kind() == crate::turn::MessageKind::Assistant)
            .collect();
        let final_message: Vec<_> = assistant_message
            .iter()
            .filter(|message| message.delivery() == crate::turn::MessageDelivery::Final)
            .collect();
        task.final_response = Some(if final_message.is_empty() {
            assistant_message
                .last()
                .map(|message| message.text().to_owned())
                .unwrap_or_default()
        } else {
            final_message
                .iter()
                .map(|message| message.text())
                .collect::<Vec<_>>()
                .join("\n\n")
        });
        agent.state = if cleanup_error.is_some() {
            AgentState::Closing
        } else {
            AgentState::Ready
        };
        agent.updated_at_ms = self.clock.now_ms();
        let requires_authorization = matches!(
            task.state,
            ExchangeState::Interrupted | ExchangeState::Cancelled | ExchangeState::Failed
        ) || self.requires_explicit_continuation(&exchange)?;
        if requires_authorization {
            agent.continuation = crate::agent::AgentContinuationPolicy::RequiresAuthorization {
                task_id: task.id.clone().into(),
            };
        }
        self.store.complete_agent_task(&agent, &task, &exchange)?;
        self.agent_registry.insert(agent.clone());
        self.agent_coordinator.retain(agent.clone(), task.clone())?;
        let next = self.agent_coordinator.settled(
            owner.agent_id().as_str(),
            &task_id,
            if cleanup_error.is_some() {
                crate::agent::AgentSettlementDisposition::CleanupFailed
            } else if requires_authorization {
                crate::agent::AgentSettlementDisposition::RequiresAuthorization
            } else {
                crate::agent::AgentSettlementDisposition::Ready
            },
        )?;
        if cleanup_error.is_none() {
            self.child_provider_by_agent
                .remove(owner.agent_id().as_str());
            self.child_execution_by_agent
                .remove(owner.agent_id().as_str());
        }
        self.trace.record(&self.session.id,"agent.completed",json!({"agent_id":*owner.agent_id(),"exchange_id":task_id,"state":task.state,"cleanup_error":cleanup_error}));
        Ok((self.prepare_agent_state()?, next))
    }

    async fn list_provider_model(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let context = self
            .provider
            .query_context(&self.provider_catalog_request()?)
            .await?;
        let mut model_list = self.provider.model_list(context).await?;
        let preference = self
            .store
            .load_preference(&self.session.workspace, &self.session.provider)?;
        crate::provider::select_model_settings(&mut model_list, preference.as_ref());
        let event = Vec::new();
        Ok((serde_json::to_value(model_list)?, event))
    }

    async fn submit_prompt(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let result = self
            .dispatch_local_prompt(Request {
                id: 0,
                method: "session.prompt.submit".into(),
                params,
            })
            .await;
        let value = result.response.result().cloned().ok_or_else(|| {
            anyhow::anyhow!("prompt dispatch failed: {:?}", result.response.error())
        })?;
        Ok((value, result.event))
    }

    /// Capture prompt admission and rollback ownership before executing its workflow.
    pub(crate) fn begin_prompt_submission(&mut self, params: &Value) -> Result<PromptSubmission> {
        let snapshot = PromptControlSnapshot {
            session: self.session_view()?,
            execution: self.store.list_plan_execution(&self.session.id)?,
            plan: self.store.list_plan(&self.session.id)?,
        };
        self.prompt_admission = params
            .get("_prompt_admission")
            .map(|value| -> Result<_> {
                Ok((
                    serde_json::from_value(
                        value
                            .get("document")
                            .cloned()
                            .context("prompt admission document missing")?,
                    )?,
                    value
                        .get("token")
                        .and_then(Value::as_u64)
                        .context("prompt admission token missing")?,
                ))
            })
            .transpose()?;
        let submitted_prompt = self.prompt_admission.clone();
        Ok(PromptSubmission {
            snapshot,
            submitted_prompt,
        })
    }

    /// Finalize admission and rollback state after the owned prompt workflow ends.
    fn prepare_retracted_control_restore(&mut self, snapshot: PromptControlSnapshot) -> Result<()> {
        let controller = self.task_controller.clone();
        controller
            .with_task_snapshot(|current| self.restore_retracted_host_snapshot(snapshot, current))
    }

    /// Restore dependent artifacts while the shared task mutation gate excludes new authority.
    fn restore_retracted_host_snapshot(
        &mut self,
        snapshot: PromptControlSnapshot,
        current: Option<crate::task::Task>,
    ) -> Result<()> {
        let unchanged = match (&current, &snapshot.session.active_task) {
            (Some(current), Some(original)) => {
                current.revision == original.revision && current.same_contract(original)
            }
            (None, None) => true,
            _ => false,
        };
        if unchanged {
            for execution in &snapshot.execution {
                self.store.save_plan_execution(execution)?;
            }
            let restored_plan_id = snapshot
                .plan
                .iter()
                .map(|plan| plan.id.as_str())
                .collect::<HashSet<_>>();
            for plan in self.store.list_plan(&self.session.id)? {
                if !restored_plan_id.contains(plan.id.as_str()) {
                    self.store.delete_plan(&plan.id)?;
                    self.plan_file.delete_plan(&self.session.id, &plan.id)?;
                }
            }
            for plan in &snapshot.plan {
                self.store.save_plan(plan)?;
            }
        }
        self.session = snapshot.session;
        self.session.active_task = current;
        self.save_session()?;
        Ok(())
    }

    fn answer_plan_question_choice(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        let question_id = required_text(&params, "question_id")?;
        let response: PlanQuestionResponse = serde_json::from_value(
            params
                .get("response")
                .cloned()
                .context("response is required")?,
        )?;
        plan.elicitation
            .as_mut()
            .context("active plan has no elicitation state")?
            .answer(&question_id, response)?;
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;
        Ok((
            serde_json::to_value(&plan)?,
            vec![self.event("plan_question_updated", json!({ "plan": plan }))?],
        ))
    }

    fn answer_plan_review_choice(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_plan_review()?;
        let question_id = required_text(&params, "question_id")?;
        let response: PlanQuestionResponse = serde_json::from_value(
            params
                .get("response")
                .cloned()
                .context("response is required")?,
        )?;
        plan.review
            .as_mut()
            .context("active plan has no acceptance state")?
            .elicitation
            .answer(&question_id, response)?;
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event(
                "plan_acceptance_updated",
                serde_json::to_value(&snapshot.active_elicitation)?,
            )?],
        ))
    }

    fn answer_question(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        if self.active_plan_has_review_questions()? {
            return self.answer_plan_review_choice(params);
        }
        if self.active_plan_awaits_input()? {
            let (_, event) = self.answer_plan_question_choice(params)?;
            return Ok((serde_json::to_value(self.snapshot()?)?, event));
        }
        let question_id = required_text(&params, "question_id")?;
        let response: PlanQuestionResponse = serde_json::from_value(
            params
                .get("response")
                .cloned()
                .context("response is required")?,
        )?;
        let mut interaction = self.active_interaction_elicitation()?;
        interaction
            .elicitation_mut()
            .context("active interaction has no elicitation state")?
            .answer(&question_id, response)?;
        self.store.save_exchange(&interaction)?;
        let snapshot = self.snapshot()?;
        Ok((
            serde_json::to_value(&snapshot)?,
            vec![self.event(
                "question_updated",
                serde_json::to_value(&snapshot.active_elicitation)?,
            )?],
        ))
    }

    /// Record one explicit conversational answer against the durable elicitation owner.
    fn answer_active_elicitation_from_model(
        &mut self,
        answer: PlanQuestionAnswer,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let has_pending_question;
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let elicitation = plan
                .elicitation
                .as_mut()
                .context("active plan has no elicitation state")?;
            elicitation.answer_from_model(&answer.question_id, answer.response)?;
            has_pending_question = elicitation.current_question().is_some();
            plan.updated_at_ms = self.clock.now_ms();
            self.publish_plan(&plan)?;
        } else {
            let mut interaction = self.active_interaction_elicitation()?;
            let elicitation = interaction
                .elicitation_mut()
                .context("active interaction has no elicitation state")?;
            elicitation.answer_from_model(&answer.question_id, answer.response)?;
            has_pending_question = elicitation.current_question().is_some();
            self.store.save_exchange(&interaction)?;
        }
        if has_pending_question {
            event.push(self.event(
                "question_updated",
                serde_json::to_value(self.snapshot()?.active_elicitation)?,
            )?);
        }
        Ok(())
    }

    /// Remove the durable elicitation only when the provider reports that no decision remains.
    fn withdraw_active_elicitation(
        &mut self,
        withdrawal: PlanQuestionWithdrawal,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let reason = withdrawal.reason.trim();
        anyhow::ensure!(!reason.is_empty(), "question withdrawal reason is required");
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let elicitation = plan
                .elicitation
                .take()
                .context("active plan has no elicitation state")?;
            let now_ms = self.clock.now_ms();
            for question in &elicitation.question_set.questions {
                plan.question_ledger.resolve(question, None, now_ms);
            }
            PlanStateMachine::apply(&mut plan, PlanEvent::FeedbackConsumed, now_ms)?;
            plan.generation.reset_no_progress();
            self.publish_plan(&plan)?;
            let lifecycle = PlanLifecycleRecord {
                title: plan.title.clone(),
                anchor: Some(self.plan_exchange_anchor(&plan.id)?),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                plan_id: plan.id.clone(),
                kind: PlanLifecycleKind::QuestionWithdrawn,
                model_revision: plan.model_revision,
                user_revision: plan.user_revision,
                overall_comment: None,
                annotation: Vec::new(),
                question: Some(elicitation.question_set),
                answer: Some(reason.to_owned()),
                created_at_ms: self.clock.now_ms(),
            };
            self.store.save_plan_lifecycle(&lifecycle)?;
            event.push(self.event(
                "plan_question_withdrawn",
                json!({ "plan": plan, "lifecycle": lifecycle }),
            )?);
            return Ok(());
        }
        let mut interaction = self.active_interaction_elicitation()?;
        interaction.set_awaiting_input(false);
        interaction.set_elicitation(None);
        self.store.save_exchange(&interaction)?;
        event.push(self.event(
            "question_withdrawn",
            json!({ "exchange_id": interaction.id, "reason": reason }),
        )?);
        Ok(())
    }

    /// Replace the pending owner question set when a clarification turn asks again.
    fn replace_active_elicitation(
        &mut self,
        question: PlanQuestionSet,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        if self.active_plan_awaits_input()? {
            let mut plan = self.active_elicitation_plan()?;
            let Some(question) = plan.question_ledger.unresolved(question) else {
                self.record_plan_trace(
                    "resolved_plan_question_suppressed",
                    plan_trace_fields(&plan),
                );
                return Ok(true);
            };
            plan.elicitation
                .as_mut()
                .context("active plan has no elicitation state")?
                .replace_question_set(question.clone());
            plan.updated_at_ms = self.clock.now_ms();
            self.publish_plan(&plan)?;
            let lifecycle = PlanLifecycleRecord {
                title: plan.title.clone(),
                anchor: Some(self.plan_exchange_anchor(&plan.id)?),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                plan_id: plan.id.clone(),
                kind: PlanLifecycleKind::QuestionAsked,
                model_revision: plan.model_revision,
                user_revision: plan.user_revision,
                overall_comment: None,
                annotation: Vec::new(),
                question: Some(question.clone()),
                answer: None,
                created_at_ms: self.clock.now_ms(),
            };
            self.store.save_plan_lifecycle(&lifecycle)?;
            event.push(self.event(
                "plan_question",
                json!({ "plan": plan, "lifecycle": lifecycle, "question": question }),
            )?);
            return Ok(true);
        }
        let Some(mut owner) = self.find_active_interaction_elicitation()? else {
            return Ok(false);
        };
        owner
            .elicitation_mut()
            .context("active interaction has no elicitation state")?
            .replace_question_set(question);
        self.store.save_exchange(&owner)?;
        event.push(self.event(
            "question",
            json!({
                "owner": "interaction",
                "exchange_id": owner.id,
                "elicitation": owner.elicitation(),
            }),
        )?);
        Ok(true)
    }

    fn skip_question(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        self.answer_question(json!({
            "question_id": required_text(&params, "question_id")?,
            "response": { "kind": "skipped" }
        }))
    }

    fn active_plan_awaits_input(&self) -> Result<bool> {
        Ok(self
            .current_plan_id()?
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| {
                plan.state == PlanState::AwaitingInput && plan.elicitation.is_some()
            }))
    }

    fn active_plan_has_review_questions(&self) -> Result<bool> {
        Ok(self
            .current_plan_id()?
            .as_deref()
            .map(|plan_id| self.store.load_plan(plan_id))
            .transpose()?
            .flatten()
            .is_some_and(|plan| plan.review_questions().is_some()))
    }

    fn active_plan_review(&self) -> Result<PlanRecord> {
        let current_plan_id = self.current_plan_id()?;
        let plan_id = current_plan_id.as_deref().context("no active plan")?;
        let plan = self
            .store
            .load_plan(plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.review_questions().is_some(),
            "active plan has no review questions"
        );
        Ok(plan)
    }

    fn active_interaction_elicitation(&self) -> Result<Exchange> {
        self.find_active_interaction_elicitation()?
            .context("no interaction awaits user input")
    }

    fn find_active_interaction_elicitation(&self) -> Result<Option<Exchange>> {
        self.store.pending_exchange(&self.session.id)
    }

    /// Capture the owning planning exchange for a user review or question action.
    fn plan_exchange_anchor(&self, plan_id: &str) -> Result<crate::plan::ExchangeAnchor> {
        if let Some(anchor) = self
            .store
            .list_plan_lifecycle(&self.session.id)?
            .into_iter()
            .rev()
            .filter(|lifecycle| lifecycle.plan_id == plan_id)
            .find_map(|lifecycle| lifecycle.anchor)
        {
            return Ok(anchor);
        }
        let exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("planning event has no owning exchange")?;
        Ok(crate::plan::ExchangeAnchor::capture(&exchange))
    }

    /// Record planning feedback only on the live exchange that owns the plan.
    fn append_plan_feedback(
        &mut self,
        plan_id: &str,
        text: String,
        intent: crate::exchange::InputIntent,
        question: Option<crate::exchange::QuestionInput>,
    ) -> Result<()> {
        let mut exchange = self
            .store
            .latest_exchange(&self.session.id)?
            .context("planning feedback has no owning exchange")?;
        anyhow::ensure!(
            self.current_plan_id()?.as_deref() == Some(plan_id)
                && exchange.state() == ExchangeState::Running,
            "planning feedback does not target the active exchange"
        );
        if let Some(question) = question {
            exchange.append_question_input(intent, text, question, self.clock.now_ms())?;
        } else {
            exchange.append_input(intent, text, self.clock.now_ms())?;
        }
        self.store.save_exchange(&exchange)
    }

    async fn finish_plan_clarification(
        &mut self,
        plan_id: &str,
        mut value: Value,
        mut event: Vec<SessionEvent>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let current_plan = self
            .store
            .load_plan(plan_id)?
            .context("active plan record is missing after clarification")?;
        if current_plan.elicitation.is_none()
            && matches!(
                current_plan.state,
                PlanState::Generating | PlanState::Revising
            )
        {
            let withdrawal = self
                .store
                .list_plan_lifecycle(&self.session.id)?
                .into_iter()
                .rev()
                .find(|lifecycle| {
                    lifecycle.plan_id == plan_id
                        && lifecycle.kind == PlanLifecycleKind::QuestionWithdrawn
                })
                .context("withdrawn planning question has no lifecycle record")?;
            let reason = withdrawal
                .answer
                .as_deref()
                .unwrap_or("no material decision remains");
            let continuation = self
                .run_planning_interaction(ProviderInput::new(PlanPrompt::withdrawn(reason)), None)
                .await;
            match continuation {
                Ok((next_value, mut next_event)) => {
                    value = next_value;
                    event.append(&mut next_event);
                }
                Err(error) => {
                    return Err(error).context("continue planning after question withdrawal");
                }
            }
        } else if current_plan
            .elicitation
            .as_ref()
            .is_some_and(|elicitation| elicitation.current_question().is_none())
        {
            let (next_value, mut next_event) = self.continue_plan_question().await?;
            value = next_value;
            event.append(&mut next_event);
        }
        Ok((value, event))
    }

    async fn finish_question_clarification(
        &mut self,
        mut value: Value,
        mut event: Vec<SessionEvent>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        if self
            .find_active_interaction_elicitation()?
            .and_then(|interaction| interaction.elicitation().cloned())
            .is_some_and(|elicitation| elicitation.current_question().is_none())
        {
            let (next_value, mut next_event) = self.continue_question().await?;
            value = next_value;
            event.append(&mut next_event);
        }
        Ok((value, event))
    }

    async fn continue_plan_question(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut plan = self.active_elicitation_plan()?;
        self.record_plan_trace("feedback_continuation_started", plan_trace_fields(&plan));
        let elicitation = plan
            .elicitation
            .take()
            .context("active plan has no elicitation state")?;
        let answer = elicitation.feedback();
        let question = elicitation.question_set.clone();
        self.append_plan_feedback(
            &plan.id,
            answer.clone(),
            crate::exchange::InputIntent::Answer,
            Some(crate::exchange::QuestionInput {
                set_id: question.id.clone(),
                question_id: None,
                answer: elicitation.answer.clone(),
            }),
        )?;
        let now_ms = self.clock.now_ms();
        for pending_question in &elicitation.question_set.questions {
            let response = Some(
                elicitation
                    .answer
                    .iter()
                    .find(|item| item.question_id == pending_question.id)
                    .map(|item| item.response.clone())
                    .unwrap_or(PlanQuestionResponse::Skipped),
            );
            plan.question_ledger
                .resolve(pending_question, response, now_ms);
        }
        PlanStateMachine::apply(&mut plan, PlanEvent::FeedbackConsumed, now_ms)?;
        plan.generation.reset_no_progress();
        self.publish_plan(&plan)?;
        self.record_plan_trace("feedback_consumed", plan_trace_fields(&plan));
        let lifecycle = PlanLifecycleRecord {
            title: plan.title.clone(),
            anchor: Some(self.plan_exchange_anchor(&plan.id)?),
            id: Uuid::new_v4().to_string(),
            session_id: self.session.id.clone(),
            plan_id: plan.id.clone(),
            kind: PlanLifecycleKind::QuestionAnswered,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: None,
            annotation: Vec::new(),
            question: Some(question.clone()),
            answer: Some(answer.clone()),
            created_at_ms: self.clock.now_ms(),
        };
        self.store.save_plan_lifecycle(&lifecycle)?;
        let mut leading_event = vec![self.event(
            "plan_question_answered",
            json!({ "plan": plan, "lifecycle": lifecycle }),
        )?];
        let prompt = match crate::plan::resolved_questions(&elicitation) {
            Some(answers) => PlanPrompt::feedback(answers),
            None => PlanPrompt::withdrawn("no questions were recorded"),
        };
        let result = self
            .run_planning_interaction(ProviderInput::new(prompt), None)
            .await;
        match result {
            Ok((value, mut event)) => {
                self.record_plan_trace("feedback_continuation_finished", plan_trace_fields(&plan));
                leading_event.append(&mut event);
                Ok((value, leading_event))
            }
            Err(error) => {
                let failed_plan = self
                    .store
                    .load_plan(&plan.id)?
                    .context("active plan record is missing after continuation failure")?;
                self.record_plan_trace(
                    "feedback_continuation_failed",
                    plan_trace_fields(&failed_plan),
                );
                Err(error).context("continue planning after user feedback")
            }
        }
    }

    async fn continue_question(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        if self.active_plan_has_review_questions()? {
            return self.continue_plan_review().await;
        }
        if self.active_plan_awaits_input()? {
            let (_, event) = self.continue_plan_question().await?;
            return Ok((serde_json::to_value(self.snapshot()?)?, event));
        }
        let mut interaction = self.active_interaction_elicitation()?;
        let elicitation = interaction
            .take_elicitation()
            .context("active interaction has no elicitation state")?;
        let feedback = elicitation.feedback();
        if interaction.check_setup.is_some()
            && elicitation.answer.iter().any(|answer| {
                answer.source == crate::session::ResolutionSource::User
                    && !matches!(answer.response, PlanQuestionResponse::Skipped)
            })
        {
            interaction.check_setup = Some(crate::checks::CheckSetupState::Reviewed);
        }
        interaction.set_awaiting_input(false);
        for node in &mut interaction.node_list {
            if let ExchangeNode::QuestionPresented {
                question, answer, ..
            } = node
                && question.id == elicitation.question_set.id
            {
                *answer = Some(feedback.clone());
            }
        }
        interaction.append_question_input(
            crate::exchange::InputIntent::Answer,
            feedback.clone(),
            crate::exchange::QuestionInput {
                set_id: elicitation.question_set.id.clone(),
                question_id: None,
                answer: elicitation.answer.clone(),
            },
            self.clock.now_ms(),
        )?;
        self.store.save_exchange(&interaction)?;
        let prompt = ProviderInput::new(match crate::plan::resolved_questions(&elicitation) {
            Some(answers) => forge_provider_api::forge::TurnInput::new(
                forge_provider_api::forge::TurnKind::WorkflowAnswers { answers },
            ),
            None => crate::plan::task_step(forge_provider_api::forge::TaskStep::AnswersResolved),
        });
        let (value, mut event) = if matches!(
            self.task_prompt_mode()?,
            PromptMode::Plan | PromptMode::PlanDiscussion
        ) {
            self.run_planning_interaction(prompt, None).await?
        } else {
            self.run_interaction(prompt, PromptMode::Chat, None).await?
        };
        event.insert(
            0,
            self.event(
                "question_answered",
                json!({
                    "exchange_id": interaction.id,
                    "question": elicitation.question_set,
                    "answer": elicitation.answer,
                }),
            )?,
        );
        Ok((value, event))
    }

    /// Publish canonical input ownership after a durable state transition.
    pub(crate) fn refresh_input_schedule(&self) -> Result<()> {
        let plan = if self.active_plan_awaits_input()? {
            Some(self.active_elicitation_plan()?)
        } else {
            None
        };
        let interaction = if plan.is_none() {
            self.find_active_interaction_elicitation()?
        } else {
            None
        };
        let input = plan
            .as_ref()
            .and_then(|plan| {
                plan.elicitation
                    .as_ref()
                    .map(|input| (format!("plan:{}", plan.id), input))
            })
            .or_else(|| {
                interaction.as_ref().and_then(|exchange| {
                    exchange
                        .elicitation()
                        .map(|input| (format!("exchange:{}", exchange.id), input))
                })
            });
        let next = crate::session::InputSchedule {
            identity: input
                .as_ref()
                .map(|(owner, input)| format!("{owner}:{}", input.question_set.id)),
            deadline: input.as_ref().and_then(|(_, input)| input.deadline.clone()),
            question_revision: input.as_ref().map(|(_, input)| input.revision),
            approval_allowed: input.is_none() && !self.active_plan_has_review_questions()?,
            ..Default::default()
        };
        self.observer
            .input_schedule
            .send_if_modified(|current| current.update(next, self.clock.now_ms()));
        Ok(())
    }

    /// Revalidate timeout evidence only when the exchange owner considers automatic work eligible.
    pub(crate) fn approval_timeout_ready(&mut self, revision: u64) -> Result<bool> {
        self.refresh_input_schedule()?;
        let current = self.observer.input_schedule.borrow();
        if current.revision != revision
            || !current.approval_allowed
            || self.turn_cancellation.is_requested()
        {
            return Ok(false);
        }
        let owner = crate::provider::ThreadOwner::root(&self.session.id);
        Ok(!self.active_plan_has_review_questions()?
            && self.permission_coordinator.has_timeout_feedback(&owner)?)
    }

    fn active_elicitation_plan(&self) -> Result<PlanRecord> {
        let current_plan_id = self.current_plan_id()?;
        let plan_id = current_plan_id.as_deref().context("no active plan")?;
        let plan = self
            .store
            .load_plan(plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingInput && plan.elicitation.is_some(),
            "active plan is not awaiting planning input"
        );
        Ok(plan)
    }

    async fn run_interaction(
        &mut self,
        input: ProviderInput,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let text = input.text();
        let setup_task = self.task_controller.current()?.filter(|task| {
            matches!(&task.workflow, crate::task::WorkflowSnapshot::Plan { plan }
                if plan.artifact.state == PlanState::ConfiguringChecks)
        });
        let setup_digest = setup_task
            .as_ref()
            .and_then(|_| {
                crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace)).ok()
            })
            .map(|snapshot| snapshot.digest);
        let result = self
            .run_interaction_input(text, mode, admission, input)
            .await?;
        if mode == PromptMode::Chat && !self.question_discussion {
            let result = Box::pin(self.advance_configured_plan(result)).await?;
            if let Some(original) = setup_task
                && let Some(current) = self.task_controller.current()?
                && original
                    .permit()
                    .validate_observation(Some(&current))
                    .is_ok()
                && current.kind() == crate::task::TaskKind::Plan
            {
                let after = crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace))
                    .ok()
                    .map(|snapshot| snapshot.digest);
                let exchange = self
                    .store
                    .latest_exchange(&self.session.id)?
                    .context("check setup source disappeared")?;
                self.task_controller.observe_plan_response(
                    &mut self.store,
                    &current.permit(),
                    &exchange,
                    after != setup_digest,
                    false,
                    self.clock.now_ms(),
                )?;
            }
            Ok(result)
        } else {
            Ok(result)
        }
    }

    async fn run_planning_interaction(
        &mut self,
        prompt: ProviderInput,
        admission: Option<ExchangeAdmission>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        let task = self
            .task_controller
            .current()?
            .context("planning requires an active task")?;
        let crate::task::WorkflowSnapshot::Plan { plan } = &task.workflow else {
            anyhow::bail!("planning requires a Plan task");
        };
        let mode = if plan.artifact.state == PlanState::AwaitingReview {
            PromptMode::PlanDiscussion
        } else {
            PromptMode::Plan
        };
        let result = self.run_interaction(prompt, mode, admission).await;
        if let Err(error) = &result
            && !error.is::<TurnCancelled>()
            && let Some(current) = self.task_controller.current()?
            && task.permit().validate_observation(Some(&current)).is_ok()
            && let Err(persistence) = self.task_controller.apply(
                &mut self.store,
                &current.permit(),
                crate::task::TaskEvent::PlanGenerationFailed,
                self.clock.now_ms(),
            )
            && !persistence.is::<crate::task::StaleTask>()
        {
            return Err(persistence).context("persist planning failure");
        }
        result
    }

    async fn run_interaction_input(
        &mut self,
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
        input: ProviderInput,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        self.context_usage_fresh = false;
        Box::pin(self.run_interaction_input_inner(text, mode, admission, input)).await
    }

    async fn settle_automatic_checks(
        &mut self,
        mut result: (Value, Vec<SessionEvent>),
        mode: PromptMode,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        use crate::checks::{AutomaticChecks, CheckState, RunRequest};
        if !matches!(
            mode,
            PromptMode::Chat | PromptMode::RequestChanges | PromptMode::GoalContinuation
        ) || self.check_setup
            || self.agent_coordinator.active_count() != 0
            || self.find_active_interaction_elicitation()?.is_some()
        {
            return Ok(result);
        }
        let Some(mut interaction) = self.store.latest_exchange(&self.session.id)? else {
            return Ok(result);
        };
        if interaction.state() != ExchangeState::Complete
            || interaction.awaiting_input()
            || interaction.check_setup.is_some()
        {
            return Ok(result);
        }
        if self.task_controller.current()?.is_some() {
            return Ok(result);
        }
        let settings = crate::settings::SettingsStore::open(&self.data_root)?;
        let check_settings = settings
            .get(
                &self.session.workspace,
                &self.session.id,
                &self.session.provider,
            )?
            .desired;
        if !check_settings.automatic_checks_enabled() {
            self.observer.completion_queue.finish()?;
            return Ok(result);
        }
        if crate::checks::ConfigSnapshot::load(Path::new(&self.session.workspace)).is_err() {
            let catalog = self.check_runner.catalog(None)?;
            if catalog["configured"] == false {
                return Ok(result);
            }
            anyhow::bail!(
                "Automatic checks could not load configuration: {}",
                catalog["error"]
            );
        }
        let Some(change_owner) = interaction.check_owner.clone() else {
            return Ok(result);
        };
        let owner = change_owner.clone();
        let mut automatic = AutomaticChecks::open(&self.check_runner.root, &owner)?;
        let timing = check_settings.check_timing.unwrap_or_default();
        if self
            .observer
            .completion_queue
            .defer(timing, &change_owner)?
        {
            automatic.pending = true;
            automatic.save()?;
            result.0["checks_deferred"] = json!(true);
            return Ok(result);
        }
        loop {
            if !settings
                .get(
                    &self.session.workspace,
                    &self.session.id,
                    &self.session.provider,
                )?
                .desired
                .automatic_checks_enabled()
            {
                automatic.pending = false;
                automatic.blocked = None;
                automatic.delivered.clear();
                automatic.repair_prompt = None;
                automatic.repair_exchange = None;
                automatic.save()?;
                self.observer.completion_queue.finish()?;
                return Ok(result);
            }
            if self.turn_cancellation.is_requested() {
                return Err(anyhow::Error::new(TurnCancelled));
            }
            if let Some(id) = self.check_runner.active()? {
                let running = self.check_runner.status(&id, 30_000, false).await?;
                if running.state == CheckState::Running {
                    continue;
                }
            }
            self.check_runner.refresh_changes().await?;
            let mut run = self.check_runner.prepare_request(
                self.store
                    .capture_timeline_owner(&self.session.id, &interaction.id)?,
                RunRequest::default(),
                None,
                self.session.shell,
                "Automatic completion checks".into(),
            )?;
            if !run
                .commands
                .iter()
                .any(|command| command.state != CheckState::Skipped)
            {
                self.observer.completion_queue.finish()?;
                automatic.pending = false;
                automatic.blocked = None;
                automatic.delivered.clear();
                automatic.repair_prompt = None;
                automatic.repair_exchange = None;
                automatic.save()?;
                return Ok(result);
            }
            automatic.pending = true;
            automatic.save()?;
            run.context.automatic = true;
            let check_scope = interaction
                .turn
                .last()
                .map(|turn| turn.provider().scope.clone());
            let run = self
                .check_runner
                .run(
                    run,
                    self.event_sink.clone(),
                    Some(self.turn_cancellation.subscribe()),
                )
                .await?;
            if let Some(scope) = &check_scope {
                self.refresh_settlement(&mut interaction, scope).await?;
            }
            if run.state == CheckState::Interrupted {
                return Err(anyhow::Error::new(TurnCancelled));
            }
            if !settings
                .get(
                    &self.session.workspace,
                    &self.session.id,
                    &self.session.provider,
                )?
                .desired
                .automatic_checks_enabled()
            {
                continue;
            }
            if run.state == CheckState::Stale {
                continue;
            }
            let repair = automatic.observe(&run)?;
            if matches!(run.state, CheckState::Stopped | CheckState::ExecutionError) {
                result.0["automatic_checks"] = json!({"run_id":run.id,"state":run.state});
                return Ok(result);
            }
            if let Some(reason) = &automatic.blocked {
                self.emit_session_update(
                    SessionUpdate::Diagnostic {
                        diagnostic: publication::DiagnosticView {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: forge_provider_api::SanitizedMessage::new(reason.clone())
                                .expect("host diagnostic is nonempty"),
                            severity: publication::DiagnosticSeverity::Warning,
                            scope: crate::provider::EventScope::Binding {
                                thread_id: forge_provider_api::ThreadId::from(
                                    crate::provider::ThreadOwner::root(&self.session.id),
                                ),
                            },
                            native_scope: forge_provider_api::WarningScope::Provider,
                        },
                    },
                    &mut result.1,
                )
                .await?;
                result.0["automatic_checks"] = json!({"run_id":run.id,"blocked":reason});
                return Ok(result);
            }
            let Some(prompt) = repair else {
                self.observer.completion_queue.finish()?;
                result.0["automatic_checks"] = json!({"run_id":run.id,"state":run.state});
                return Ok(result);
            };
            let mut admission = ExchangeAdmission::chat(String::new());
            admission.check_owner = Some(change_owner.clone());
            admission.automatic_owner = Some(owner.clone());
            let input = ProviderInput::kind(forge_provider_api::forge::TurnKind::CheckRepair {
                repair: prompt,
            });
            let repaired = Box::pin(self.run_interaction_input_inner(
                input.text(),
                PromptMode::Chat,
                Some(admission),
                input,
            ))
            .await?;
            result.0 = repaired.0;
            result.1.extend(repaired.1);
            let latest = self
                .store
                .latest_exchange(&self.session.id)?
                .context("repair exchange disappeared")?;
            automatic.repair_exchange = Some(latest.id.clone());
            automatic.save()?;
            if latest.state() != ExchangeState::Complete
                || latest.awaiting_input()
                || self.find_active_interaction_elicitation()?.is_some()
                || self.agent_coordinator.active_count() != 0
            {
                return Ok(result);
            }
        }
    }

    /// Start checkpoint acquisition and tool gating without awaiting the worker.
    fn begin_exchange_runtime(&mut self, interaction: &mut Exchange, now_ms: i64) -> Result<()> {
        self.require_restore_recovered()?;
        let tool_gate = self
            .checkpoint
            .as_ref()
            .map(|_| crate::checkpoint::ToolGate::new());
        let checkpoint = self.checkpoint.clone().map(|checkpoint| {
            let objects = self.store.objects.clone();
            let repositories = Arc::clone(&self.repositories);
            let session_id = self.session.id.clone();
            let trace = self.trace.clone();
            let exchange_id = interaction.id.clone();
            crate::checkpoint::CheckpointTask(tokio::spawn(async move {
                trace
                    .instrument(
                        &session_id,
                        &exchange_id,
                        "checkpoint.capture",
                        checkpoint.capture(&objects, &repositories, &session_id, now_ms),
                    )
                    .await
                    .map_err(
                        |source| crate::checkpoint::CheckpointFailure::SnapshotCapture {
                            source: source.into_boxed_dyn_error(),
                        },
                    )
            }))
        });
        self.active_wait_projection = None;
        self.exchange_runtime = Some(ExchangeRuntime {
            task_permit: self
                .task_controller
                .current()?
                .filter(|task| task.entry_accepted())
                .map(|task| task.permit()),
            checkpoint,
            tool_gate,
            exchange_id: interaction.id.clone(),
            checklist: interaction
                .checklist
                .clone()
                .map(ChecklistTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligibility: RetractionEligibility::output_free(),
            active_wait: None,
        });
        Ok(())
    }

    fn finish_initial_checkpoint(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        result: Result<crate::checkpoint::CheckpointRecord>,
    ) -> Result<()> {
        runtime.checkpoint.take();
        let saved = result.and_then(|checkpoint| {
            let task = self.task_controller.current()?.filter(|task| runtime.task_permit.as_ref()
                .is_some_and(|permit| permit.task_id() == &task.id));
            let execution = task.as_ref().and_then(|task| match &task.workflow {
                crate::task::WorkflowSnapshot::Execute { execution } => Some(execution),
                _ => None,
            });
            let queued_owner = if execution.is_none() {
                self.observer.completion_queue.owner()?
            } else {
                None
            };
            let task_owner = task.as_ref().map(|task| task.id.as_str().to_owned());
            let owner = interaction
                .check_owner
                .as_ref()
                .or(queued_owner.as_ref())
                .or(task_owner.as_ref())
                .unwrap_or(&interaction.id)
                .clone();
            interaction.check_owner = Some(owner.clone());
            self.check_runner.begin_changes(owner, &checkpoint)?;
            interaction.checkpoint_before = Some(checkpoint.id.clone());
            self.store
                .save_checkpoint_exchange(&checkpoint, interaction)?;
            if let Some(task) = task
                && matches!(&task.workflow, crate::task::WorkflowSnapshot::Execute { execution } if execution.record.baseline_checkpoint.is_none())
            {
                self.task_controller.apply(&mut self.store, &task.permit(),
                    crate::task::TaskEvent::ExecutionBaselineCaptured(checkpoint.id), self.clock.now_ms())?;
            }
            Ok(())
        });
        if let Some(gate) = &runtime.tool_gate {
            gate.complete(
                saved
                    .as_ref()
                    .map(|_| ())
                    .map_err(|_| crate::checkpoint::CheckpointGateError::Persistence),
            );
        }
        self.trace.record(
            &self.session.id,
            "turn.dependency",
            json!({
                "exchange_id":interaction.id,"phase":"checkpoint.persist",
                "status":if saved.is_ok() { "ready" } else { "failed" },
            }),
        );
        saved
    }

    async fn collect_initial_checkpoint(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
    ) -> Result<()> {
        let Some(task) = runtime.checkpoint.as_mut() else {
            return Ok(());
        };
        let result = (&mut task.0)
            .await
            .context("checkpoint worker failed")
            .and_then(|result| result.map_err(anyhow::Error::new));
        self.finish_initial_checkpoint(runtime, interaction, result)
    }

    async fn collect_active_initial_checkpoint(
        &mut self,
        interaction: &mut Exchange,
    ) -> Result<()> {
        if !self.exchange_runtime.as_ref().is_some_and(|runtime| {
            runtime.exchange_id == interaction.id && runtime.checkpoint.is_some()
        }) {
            return Ok(());
        }
        let mut runtime = self.exchange_runtime.take().expect("checked runtime");
        let result = self
            .collect_initial_checkpoint(&mut runtime, interaction)
            .await;
        self.exchange_runtime = Some(runtime);
        result
    }

    fn resume_exchange_runtime(&mut self, interaction: &Exchange) -> Result<()> {
        self.require_restore_recovered()?;
        self.exchange_runtime = Some(ExchangeRuntime {
            task_permit: self
                .task_controller
                .current()?
                .filter(|task| task.entry_accepted())
                .map(|task| task.permit()),
            checkpoint: None,
            tool_gate: None,
            exchange_id: interaction.id.clone(),
            checklist: interaction
                .checklist
                .clone()
                .map(ChecklistTracker::from_snapshot)
                .unwrap_or_default(),
            retraction_eligibility: RetractionEligibility::Consumed,
            active_wait: None,
        });
        Ok(())
    }

    /// Describe a settled child task as the `final_answer` `<agent_message>` the parent receives.
    fn agent_result(
        &self,
        task: &crate::agent::AgentTask,
    ) -> Result<forge_provider_api::forge::AgentResult> {
        let agent = self.agent_coordinator.resolve(&task.agent_id)?;
        let status = serde_json::to_value(task.state)?;
        Ok(forge_provider_api::forge::AgentResult {
            agent: agent.task_path.to_string(),
            task_id: task.id.to_string(),
            status: status
                .as_str()
                .map_or_else(|| status.to_string(), str::to_owned),
            response: task.final_response.clone(),
            error: task.error.clone(),
        })
    }

    fn acknowledge_agent_result(&mut self, mut task: crate::agent::AgentTask) -> Result<()> {
        if matches!(
            task.state,
            ExchangeState::Interrupted | ExchangeState::Cancelled | ExchangeState::Failed
        ) {
            return Ok(());
        }
        task.parent_reported = true;
        self.store.save_agent_task(&task)?;
        self.agent_coordinator
            .retain(self.agent_coordinator.resolve(&task.agent_id)?, task)
    }

    async fn notify_unavailable_mcp(
        &mut self,
        request: &ProviderCatalogRequest,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let declaration = self.provider.descriptor();
        let Some(target) = declaration
            .capability
            .integration
            .resource_target
            .as_deref()
        else {
            return Ok(());
        };
        let resources = crate::settings::SessionResources::load(
            &self.data_root,
            request,
            &self.session.provider,
            target,
        )?;
        for (id, message) in &resources.launch_notice {
            if self.mcp_notice.get(id) == Some(message) {
                continue;
            }
            self.trace.record(
                &self.session.id,
                "mcp.start_skipped",
                json!({"resource_id":id,"reason":message}),
            );
            self.emit_session_update(
                SessionUpdate::Diagnostic {
                    diagnostic: publication::DiagnosticView {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new(message.clone())
                            .expect("host diagnostic is nonempty"),
                        severity: publication::DiagnosticSeverity::Warning,
                        scope: crate::provider::EventScope::Binding {
                            thread_id: forge_provider_api::ThreadId::from(
                                crate::provider::ThreadOwner::root(&self.session.id),
                            ),
                        },
                        native_scope: forge_provider_api::WarningScope::Provider,
                    },
                },
                event,
            )
            .await?;
        }
        self.mcp_notice = resources.launch_notice;
        Ok(())
    }

    /// Revoke question delivery before native cancellation can release its waiting tools.
    pub(crate) fn cancel_question_delivery(&self) -> Result<()> {
        let active = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Running)?;
        let active = match active {
            Some(active) => Some(active),
            None => self.store.latest_exchange(&self.session.id)?,
        };
        self.questions
            .cancel_chat(active.as_ref().map(|exchange| exchange.id.as_str()))
    }

    /// Retry finalization without submitting provider input or replaying tool execution.
    async fn cancel_settled_execution(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut event = Vec::new();
        self.cancel_question_delivery()?;
        if self
            .current_goal()?
            .is_some_and(|goal| matches!(goal.state, GoalState::Active | GoalState::Paused))
        {
            let (_, goal_event) = self.dispatch_session_command("session.goal.pause", json!({}))?;
            event.extend(goal_event);
        }
        if let Some(mut exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Running)?
        {
            anyhow::ensure!(
                exchange.execution_started_at_ms().is_none()
                    && exchange
                        .turn
                        .iter()
                        .all(|turn| turn.state() != crate::turn::TurnState::Running),
                "execution must settle before idle cancellation"
            );
            exchange.begin_finalization(
                crate::exchange::ExchangeOutcome::Cancelled,
                self.clock.now_ms(),
            )?;
            self.store.save_exchange(&exchange)?;
        }
        let (result, finalization_event) = self.retry_finalization().await?;
        event.extend(finalization_event);
        Ok((result, event))
    }

    async fn retry_finalization(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let Some(mut exchange) = self
            .store
            .exchange_in_state(&self.session.id, ExchangeState::Finalizing)?
        else {
            return Ok((json!({"cancel_requested": true}), Vec::new()));
        };
        let descendants: Vec<_> = referenced_child_exchange_list(&exchange)
            .into_iter()
            .map(|(_, id)| id)
            .collect();
        if let Err(error) = self
            .store
            .require_settled_exchange_tree(&self.session.id, &descendants)
        {
            exchange.finalization_error = Some(format!("{error:#}"));
            self.store.save_exchange(&exchange)?;
            return Err(error);
        }
        let outcome = exchange
            .finalization_outcome()
            .context("finalization outcome is missing")?;
        self.capture_final_checkpoint(&mut exchange, outcome)
            .await?;
        exchange.finish(outcome, self.clock.now_ms())?;
        self.store.save_exchange(&exchange)?;
        self.exchange_runtime = None;
        self.save_session()?;
        Ok((
            json!({"cancel_requested": true, "finalized_exchange_id": exchange.id}),
            vec![self.event(
                "exchange_complete",
                json!({"id":exchange.id,"state":exchange.state()}),
            )?],
        ))
    }

    async fn capture_final_checkpoint(
        &mut self,
        interaction: &mut Exchange,
        outcome: crate::exchange::ExchangeOutcome,
    ) -> Result<Option<String>> {
        interaction.begin_finalization(outcome, self.clock.now_ms())?;
        self.store.save_exchange(interaction)?;
        let scope = interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone());
        if let Some(scope) = &scope {
            self.host_settlement.begin_settlement(scope);
        }
        let result: Result<Option<String>> = async {
            loop {
                if let Some(scope) = &scope {
                    self.refresh_settlement(interaction, scope).await?;
                }
                let outcome = interaction.finalization_outcome().unwrap_or(outcome);
                let descendants: Vec<_> = referenced_child_exchange_list(interaction)
                    .into_iter()
                    .map(|(_, id)| id)
                    .collect();
                self.store
                    .require_settled_exchange_tree(&self.session.id, &descendants)?;
                if outcome == crate::exchange::ExchangeOutcome::Cancelled
                    && let Some(runtime) = self.exchange_runtime.as_mut().filter(|runtime| {
                        runtime.exchange_id == interaction.id && runtime.tools_blocked()
                    })
                {
                    runtime
                        .tool_gate
                        .as_ref()
                        .expect("blocked tool gate")
                        .complete(Err(crate::checkpoint::CheckpointGateError::OwnerEnded));
                    runtime.checkpoint.take();
                    interaction.finalization_error = None;
                    interaction.finish(outcome, self.clock.now_ms())?;
                    return Ok(None);
                }
                self.collect_active_initial_checkpoint(interaction).await?;
                if self.checkpoint.is_none() {
                    if let Some(scope) = &scope
                        && self.refresh_settlement(interaction, scope).await?
                    {
                        continue;
                    }
                    let mut changes = crate::exchange::ProviderDiffBuilder::default();
                    self.visit_provider_exchanges(interaction, |exchange| {
                        changes.record(exchange)
                    })?;
                    interaction.attributed_diff_text = changes.finish();
                    interaction.attributed_matches_checkpoint = false;
                    interaction.finish(outcome, self.clock.now_ms())?;
                    return Ok(None);
                };
                let capture = self.prepare_final_checkpoint(interaction, outcome)?;
                let captured = capture.run().await?;
                if let Some(scope) = &scope
                    && self.refresh_settlement(interaction, scope).await?
                {
                    continue;
                }
                match self.commit_final_checkpoint(interaction, captured)? {
                    FinalCheckpointCommit::Committed(id) => return Ok(Some(id)),
                    FinalCheckpointCommit::Stale => continue,
                }
            }
        }
        .await;
        if let Some(scope) = &scope {
            self.host_settlement.end_settlement(scope);
        }
        interaction.finalization_error = result.as_ref().err().map(|error| format!("{error:#}"));
        self.store.save_exchange(interaction)?;
        result
    }

    fn interaction_provider_change_index(
        &self,
        interaction: &Exchange,
    ) -> Result<ProviderChangeIndex> {
        let mut index = ProviderChangeIndex::default();
        self.visit_provider_exchanges(interaction, |exchange| index.record(exchange))?;
        Ok(index)
    }

    fn visit_provider_exchanges(
        &self,
        interaction: &Exchange,
        mut visit: impl FnMut(&Exchange),
    ) -> Result<()> {
        visit(interaction);
        let mut pending = referenced_child_exchange_list(interaction);
        let mut visited = HashSet::new();
        visited.insert(interaction.id.clone());
        while let Some((agent_id, exchange_id)) = pending.pop() {
            if !visited.insert(exchange_id.clone()) {
                continue;
            }
            if let Some(record) = self
                .store
                .list_agent_exchange(&agent_id)?
                .into_iter()
                .find(|record| record.id == exchange_id)
            {
                visit(&record);
                pending.extend(referenced_child_exchange_list(&record));
            }
        }
        Ok(())
    }

    /// Transfer one active exchange runtime to a service-owned foreground transaction.
    pub(crate) fn begin_owned_foreground(
        &mut self,
        interaction: Exchange,
        scope: crate::provider::InvocationScope,
    ) -> Result<OwnedForegroundTransaction> {
        anyhow::ensure!(
            scope.owner() == crate::provider::ThreadOwner::root(&self.session.id)
                && scope.exchange_id
                    == forge_provider_api::ExchangeId::from(interaction.id.as_str()),
            "foreground transaction crosses execution ownership"
        );
        let runtime = self
            .exchange_runtime
            .as_ref()
            .context("active interaction timeline is missing")?;
        anyhow::ensure!(
            runtime.exchange_id == interaction.id,
            "foreground runtime belongs to another exchange"
        );
        if let Some(permit) = &runtime.task_permit {
            permit.validate(self.task_controller.current()?.as_ref())?;
        }
        let runtime = self
            .exchange_runtime
            .take()
            .expect("runtime validated above");
        Ok(OwnedForegroundTransaction {
            scope,
            interaction,
            runtime: Some(runtime),
        })
    }

    /// Apply one owned provider event and retain its independent presentation effects.
    pub(crate) fn apply_owned_foreground(
        &mut self,
        transaction: &mut OwnedForegroundTransaction,
        provider_event: HarnessEvent,
    ) -> Result<(AppliedProviderEvent, Vec<SessionEvent>)> {
        if provider_event.scope().owner() != transaction.scope.owner() {
            return Ok((self.apply_observed_child(&provider_event)?, Vec::new()));
        }
        anyhow::ensure!(
            transaction.scope.owner() == crate::provider::ThreadOwner::root(&self.session.id)
                && provider_event.scope().owner() == transaction.scope.owner(),
            "foreground event crosses execution ownership"
        );
        if let Some(scope) = provider_event.scope().invocation() {
            anyhow::ensure!(
                scope == &transaction.scope,
                "foreground event crosses invocation ownership"
            );
        }
        let runtime = transaction
            .runtime
            .as_mut()
            .context("foreground transaction already committed")?;
        // Native facts remain attributable to this exact scope after its task ends.
        // Callback admission and continuation retain their separate authority checks.
        let mut events = Vec::new();
        let effects = self.apply_provider_event(
            runtime,
            &mut transaction.interaction,
            provider_event,
            &mut events,
        )?;
        Ok((effects, events))
    }

    /// Apply one foreground observation and capture its unlocked publication job.
    pub(crate) fn apply_foreground_observation(
        &mut self,
        transaction: &mut ForegroundTransaction,
        observation: OwnerObservation,
    ) -> Result<PendingProviderDelivery> {
        let observation = match observation {
            OwnerObservation::HostFileChange {
                binding_id,
                scope,
                change,
            } => {
                let effect = self.apply_observed_file_change(&binding_id, &scope, &change)?;
                return self.prepare_provider_delivery(effect);
            }
            OwnerObservation::Event(event) => event,
            OwnerObservation::InputAcceptance {
                scope,
                receipt,
                presentation,
            } => {
                anyhow::ensure!(
                    scope == transaction.state.scope,
                    "acceptance receipt crosses foreground invocation"
                );
                self.acknowledge_user_child_reports(&scope, &receipt)?;
                let runtime = transaction
                    .state
                    .runtime
                    .as_mut()
                    .context("foreground runtime missing")?;
                let applied = self.apply_input_acceptance(
                    runtime,
                    &mut transaction.state.interaction,
                    &receipt,
                    &presentation,
                )?;
                return self.prepare_provider_delivery(applied);
            }
        };

        if observation.scope().owner() != transaction.state.scope.owner() {
            let applied = self.apply_observed_child(&observation)?;
            return self.prepare_provider_delivery(applied);
        }
        if let Some(scope) = observation.scope().invocation()
            && scope != &transaction.state.scope
        {
            anyhow::ensure!(
                scope.owner() == transaction.state.scope.owner()
                    && scope.exchange_id == transaction.state.scope.exchange_id
                    && transaction.engine.admitted_invocation(scope)?,
                "foreground successor has no owner-issued invocation admission"
            );
            transaction.state.scope = scope.clone();
        }
        let retraction_boundary = match &observation {
            HarnessEvent::Native { event, .. } => match event.as_ref() {
                ProviderEvent::Turn {
                    event: forge_provider_api::TurnEvent::Completed(completed),
                    ..
                } => completed.result().checkpoint_id().cloned(),
                ProviderEvent::Provider { .. }
                | ProviderEvent::Thread { .. }
                | ProviderEvent::Turn { .. }
                | ProviderEvent::Item { .. }
                | ProviderEvent::Agent { .. } => None,
            },
            HarnessEvent::Host { .. } => None,
        };
        let (applied, mut events) =
            self.apply_owned_foreground(&mut transaction.state, observation)?;
        if let Some(boundary) = retraction_boundary {
            transaction.retraction_boundary = Some((transaction.state.scope.clone(), boundary));
        }
        let mut delivery = self.prepare_provider_delivery(applied)?;
        events.append(&mut delivery.events);
        delivery.events = events;
        Ok(delivery)
    }

    /// Route an admitted user input to an owned native steer or the retained queue.
    pub(crate) fn prepare_foreground_input(
        &self,
        transaction: &mut ForegroundTransaction,
        input: crate::provider::SteerCommand,
    ) -> ForegroundAction {
        if self.capability.native_steer {
            ForegroundAction::Effect(ForegroundJob::UserSteering {
                flight: transaction.steering.pending_deliveries.retain(),
                engine: transaction.engine.clone(),
                scope: transaction.state.scope.clone(),
                input,
            })
        } else {
            transaction.steering.deferred.push_back(input);
            ForegroundAction::Wait
        }
    }

    /// Commit the initial checkpoint without opening a provider-scheduled successor.
    pub(crate) fn apply_foreground_checkpoint(
        &mut self,
        transaction: &mut ForegroundTransaction,
        result: Result<crate::checkpoint::CheckpointRecord>,
    ) -> Result<ForegroundAction> {
        let runtime = transaction
            .state
            .runtime
            .as_mut()
            .context("foreground runtime is missing")?;
        self.finish_initial_checkpoint(runtime, &mut transaction.state.interaction, result)?;
        Ok(ForegroundAction::Wait)
    }

    /// Capture a plan scan as owned work that can run without the broker lock.
    pub(crate) fn apply_foreground_agent_observation(
        &mut self,
        owner: crate::provider::ThreadOwner,
        task_id: &str,
        observation: OwnerObservation,
    ) -> Result<ForegroundAction> {
        let mut events = Vec::new();
        let effects =
            self.apply_managed_agent_observation(owner, task_id, observation, &mut events)?;
        let mut delivery = self.prepare_provider_delivery(effects)?;
        events.append(&mut delivery.events);
        delivery.events = events;
        Ok(ForegroundAction::Effect(ForegroundJob::Publication(
            delivery,
        )))
    }

    /// Capture a child completion and cancel only that owner's approvals unlocked.
    pub(crate) fn prepare_foreground_agent_completion(
        &self,
        owner: crate::provider::ThreadOwner,
        task_id: String,
        output: Result<crate::provider::ProviderCompletion>,
        host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
        cleanup_error: Option<String>,
    ) -> Result<ForegroundAction> {
        let task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str())
                && task.agent_id.as_str() == owner.agent_id().as_str(),
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok(ForegroundAction::Wait);
        }
        Ok(ForegroundAction::Effect(
            ForegroundJob::AgentCancelApprovals {
                coordinator: self.permission_coordinator.clone(),
                completion: Box::new(AgentCompletion {
                    owner,
                    task_id,
                    output,
                    host_file_changes,
                    cleanup_error,
                }),
            },
        ))
    }

    /// Commit child completion only after its approval cancellation has settled.
    ///
    /// `result` carries exactly the approvals the cancellation removed, so each receives one
    /// terminal event and none resolved meanwhile is reported as cancelled.
    pub(crate) fn apply_foreground_agent_completion(
        &mut self,
        transaction: &mut ForegroundTransaction,
        completion: AgentCompletion,
        result: Result<Vec<ApprovalRequestView>>,
    ) -> Result<ForegroundAction> {
        transaction.agent_command_active = false;
        let approvals = match result {
            Ok(approvals) => approvals,
            Err(error) => {
                if let Some((response, _)) = transaction.pending_agent_reply.take() {
                    let _ = response.send(Err(anyhow::anyhow!("{error:#}")));
                }
                return Err(error);
            }
        };
        let AgentCompletion {
            owner,
            task_id,
            output,
            host_file_changes,
            cleanup_error,
        } = completion;
        let task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion disappeared")?;
        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str())
                && task.agent_id.as_str() == owner.agent_id().as_str(),
            "child completion owner changed during approval cancellation"
        );
        if task.completed_at_ms.is_some() {
            return Ok(ForegroundAction::Wait);
        }
        anyhow::ensure!(
            self.child_exchange_runtime_by_agent
                .get(owner.agent_id().as_str())
                .is_none_or(|runtime| runtime.exchange.id == task_id),
            "late child completion cannot settle a newer child exchange"
        );
        let mut effects = Vec::new();
        for approval in approvals {
            if let Some(runtime) = self
                .child_exchange_runtime_by_agent
                .get_mut(owner.agent_id().as_str())
            {
                anyhow::ensure!(
                    runtime.exchange.id == task_id,
                    "child approval belongs to a newer exchange"
                );
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                self.store.save_exchange(&runtime.exchange)?;
            }
            effects.push(AppliedProviderEvent::Update(SessionUpdate::Approval {
                execution_owner: approval
                    .execution_owner
                    .clone()
                    .context("approval lost original owner")?,
                execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                update: Box::new(approval_cancelled(&approval, self.clock.now_ms())?),
            }));
        }
        let (state, next) = self.apply_managed_agent_completion(
            owner,
            task_id,
            output,
            host_file_changes,
            cleanup_error,
        )?;
        effects.push(state);
        if let Some(next) = next {
            transaction
                .pending_agent_launch
                .push_back(PendingAgentLaunch {
                    work: next,
                    reply: None,
                });
        }
        if let Some((response, value)) = transaction.pending_agent_reply.take() {
            let _ = response.send(Ok(value));
        }
        Ok(ForegroundAction::Effect(ForegroundJob::Publication(
            self.prepare_provider_delivery(AppliedProviderEvent::Effects(effects))?,
        )))
    }

    /// Launch a queued child only after the previous completion was applied.
    pub(crate) fn prepare_foreground_agent_launch(
        &mut self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        let ready = match transaction.pending_agent_launch.pop_front() {
            Some(pending) => Some(pending),
            None => self
                .agent_coordinator
                .ready_launch()?
                .map(|work| PendingAgentLaunch { work, reply: None }),
        };
        let Some(PendingAgentLaunch { work, reply }) = ready else {
            return Ok(ForegroundAction::Wait);
        };
        let started = match self.begin_managed_agent(work) {
            Ok(started) => started,
            Err(error) => {
                if let Some((response, _)) = reply {
                    let _ = response.send(Err(error));
                    return Ok(ForegroundAction::Wait);
                }
                return Err(error);
            }
        };
        match started {
            Some((owner, task_id, error)) => {
                transaction.pending_agent_reply = reply;
                let action = self.prepare_foreground_agent_completion(
                    owner,
                    task_id,
                    Err(error),
                    Vec::new(),
                    None,
                )?;
                transaction.agent_command_active = matches!(
                    &action,
                    ForegroundAction::Effect(ForegroundJob::AgentCancelApprovals { .. })
                );
                Ok(action)
            }
            None => {
                if let Some((response, value)) = reply {
                    let _ = response.send(Ok(value));
                }
                Ok(ForegroundAction::Wait)
            }
        }
    }

    /// Capture a plan scan as owned work that can run without the broker lock.
    pub(crate) fn prepare_foreground_agent_result(
        &self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        if !self.capability.native_steer
            || self.requires_explicit_continuation(&transaction.state.interaction)?
        {
            return Ok(ForegroundAction::Wait);
        }
        for task in self.agent_coordinator.unreported_result() {
            if transaction
                .attached_results
                .iter()
                .any(|attached| attached.id == task.id)
                || !transaction.attempted_results.insert(task.id.clone())
            {
                continue;
            }
            anyhow::ensure!(
                task.session_id == self.session.id && task.completed_at_ms.is_some(),
                "child result does not belong to this settled session"
            );
            let message = self.agent_result(&task)?;
            return Ok(ForegroundAction::Effect(ForegroundJob::AgentResult {
                flight: transaction.steering.pending_deliveries.retain(),
                engine: transaction.engine.clone(),
                scope: transaction.state.scope.clone(),
                task: Box::new(task),
                message,
            }));
        }
        Ok(ForegroundAction::Wait)
    }

    /// Flag every target child, then run approval/provider cleanup outside the broker.
    ///
    /// All targets are interrupted here, before any cleanup is attempted, so a failure for one
    /// child cannot leave a later child running. Capture failures travel with the operation and
    /// are reported after every other target was handled.
    pub(crate) fn prepare_foreground_agent_interrupt(
        &mut self,
        targets: Vec<String>,
        request: OwnedAgentRequest,
        result: Value,
    ) -> Result<ForegroundAction> {
        if targets.is_empty() {
            let _ = request.response.send(Ok(result));
            return Ok(ForegroundAction::Wait);
        }
        let mut items = Vec::new();
        let mut failures = Vec::new();
        for target in targets {
            let captured = (|| -> Result<AgentInterruptItem> {
                let agent = self.agent_coordinator.resolve(&target)?;
                let owner = crate::provider::ThreadOwner::new(
                    self.session.id.clone().into(),
                    crate::provider::AgentId::new(agent.id.clone())?,
                )
                .expect("registered session and agent identities are nonempty");
                let cancelled = self.agent_coordinator.interrupt(&target)?;
                let cleanup = self.agent_coordinator.cleanup_task(&agent.id);
                let provider = if cleanup.is_some() {
                    Some(
                        self.child_provider_by_agent
                            .get(&agent.id)
                            .cloned()
                            .context("child cleanup obligation has no retained provider adapter")?,
                    )
                } else {
                    None
                };
                Ok(AgentInterruptItem {
                    agent,
                    owner,
                    cancelled,
                    cleanup,
                    provider,
                    approvals: None,
                    cleanup_succeeded: false,
                })
            })();
            match captured {
                Ok(item) => items.push(item),
                Err(error) => failures.push(error.context(format!("interrupt child {target}"))),
            }
        }
        Ok(ForegroundAction::Effect(ForegroundJob::AgentInterrupt {
            coordinator: self.permission_coordinator.clone(),
            operation: AgentInterrupt {
                items,
                failures,
                request,
                result,
            },
        }))
    }

    fn validate_foreground_agent_request(
        &self,
        transaction: &ForegroundTransaction,
        owned: &OwnedAgentRequest,
    ) -> Result<()> {
        self.validate_foreground_agent_identity(transaction, owned)?;
        if let Some(task) = owned
            .authority
            .as_ref()
            .and_then(|authority| authority.task.as_ref())
        {
            task.validate_scope()?;
        }
        Ok(())
    }

    fn validate_foreground_agent_identity(
        &self,
        transaction: &ForegroundTransaction,
        owned: &OwnedAgentRequest,
    ) -> Result<()> {
        if let Some(target) = &owned.target {
            self.agent_coordinator.validate_target(target)?;
        }
        anyhow::ensure!(
            owned.parent_id == transaction.state.interaction.id,
            "child request belongs to another parent exchange"
        );
        if owned.request.source == crate::agent::AgentSource::Model {
            let authority = owned
                .authority
                .as_ref()
                .context("agent request has no active authenticated root authority")?;
            anyhow::ensure!(
                !*authority.expired.borrow()
                    && authority.owner == crate::provider::ThreadOwner::root(&self.session.id)
                    && authority.parent_exchange_id == owned.parent_id
                    && self.agent_coordinator.enabled(),
                "agent request has no active authenticated root authority"
            );
        } else {
            anyhow::ensure!(
                owned.authority.is_none(),
                "manual agent request has unexpected authority"
            );
        }
        Ok(())
    }

    /// Admit one child command without awaiting provider work while holding the broker.
    pub(crate) fn poll_foreground_agents(
        &mut self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        let Some(command) = self.agent_coordinator.try_receive_foreground() else {
            if transaction.pending_agent_launch.is_empty()
                && let Some(work) = self.agent_coordinator.ready_launch()?
            {
                transaction
                    .pending_agent_launch
                    .push_back(PendingAgentLaunch { work, reply: None });
            }
            if !transaction.pending_agent_launch.is_empty() {
                return self.prepare_foreground_agent_launch(transaction);
            }
            return self.prepare_foreground_agent_result(transaction);
        };
        // Schedule another drain after this owned command has been staged. The revision watch
        // coalesces sends, so a burst of commands would otherwise wake this owner only once.
        self.agent_coordinator.notify_activity();
        transaction.agent_revision.mark_changed();
        let action = match command {
            crate::agent::AgentCommand::Event {
                owner,
                task_id,
                event,
            } => self.apply_foreground_agent_observation(owner, &task_id, *event),
            crate::agent::AgentCommand::Completed {
                owner,
                task_id,
                output,
                host_file_changes,
                cleanup_error,
            } => self.prepare_foreground_agent_completion(
                owner,
                task_id,
                *output,
                host_file_changes,
                cleanup_error,
            ),
            crate::agent::AgentCommand::Request {
                request,
                authority,
                target,
                response,
                task_admission,
            } => {
                if response.is_closed() {
                    return Ok(ForegroundAction::Wait);
                }
                let owned = OwnedAgentRequest {
                    request: *request,
                    authority,
                    target,
                    response,
                    parent_id: transaction.state.interaction.id.clone(),
                    task_admission,
                };
                if let Err(error) = self.validate_foreground_agent_request(transaction, &owned) {
                    let _ = owned.response.send(Err(error));
                    return Ok(ForegroundAction::Wait);
                }
                self.prepare_foreground_agent_request(transaction, owned)
            }
        }?;
        transaction.agent_command_active = matches!(
            &action,
            ForegroundAction::Effect(
                ForegroundJob::AgentCancelApprovals { .. }
                    | ForegroundJob::AgentInterrupt { .. }
                    | ForegroundJob::AgentSpawnSelect { .. }
                    | ForegroundJob::AgentMessageSteer { .. }
            )
        );
        Ok(action)
    }

    fn prepare_foreground_agent_request(
        &mut self,
        transaction: &mut ForegroundTransaction,
        owned: OwnedAgentRequest,
    ) -> Result<ForegroundAction> {
        use crate::agent::AgentOperation;
        let tasks = match self.store.list_agent_task(&self.session.id) {
            Ok(tasks) => tasks,
            Err(error) => {
                let _ = owned.response.send(Err(error));
                return Ok(ForegroundAction::Wait);
            }
        };
        if let Some(task) = tasks
            .into_iter()
            .find(|task| task.operation_id == owned.request.operation_id)
        {
            let replay = (|| {
                let agent = self.agent_coordinator.resolve(&task.agent_id)?;
                let same = match &owned.request.operation {
                    AgentOperation::Spawn(spawn) => {
                        task.ordinal == 1
                            && task.message == spawn.text
                            && spawn
                                .profile
                                .as_deref()
                                .is_none_or(|profile| profile == agent.definition)
                    }
                    AgentOperation::Followup { target, message } => {
                        task.ordinal > 1
                            && task.message == *message
                            && self.agent_coordinator.resolve(target)?.id == agent.id
                    }
                    _ => false,
                };
                anyhow::ensure!(
                    same && task.source == owned.request.source
                        && task.parent_exchange_id == owned.parent_id,
                    "operation identity was reused for different child work"
                );
                Ok(json!({"agent":agent,"task":task,"status":"admitted","replayed":true}))
            })();
            let _ = owned.response.send(replay);
            return Ok(ForegroundAction::Wait);
        }
        match owned.request.operation.clone() {
            AgentOperation::Interrupt { target } => self.prepare_foreground_agent_interrupt(
                vec![target.clone()],
                owned,
                json!({"status":"interrupting","target":target}),
            ),
            AgentOperation::InterruptAll => {
                let targets = self
                    .agent_coordinator
                    .snapshot(None)
                    .into_iter()
                    .map(|(agent, _)| agent.id)
                    .collect();
                self.prepare_foreground_agent_interrupt(
                    targets,
                    owned,
                    json!({"status":"interrupting"}),
                )
            }
            AgentOperation::Spawn(spawn) => {
                let mut root = match self.provider_request(
                    ProviderInput::from_user_text(String::new()),
                    PromptMode::Chat,
                ) {
                    Ok(root) => root,
                    Err(error) => {
                        let _ = owned.response.send(Err(error));
                        return Ok(ForegroundAction::Wait);
                    }
                };
                if root.model == "default"
                    && let Some(model) = &self.session.resolved_model
                {
                    root.model = model.clone();
                }
                let root_provider = match forge_providers::registry::select(&self.session.provider)
                {
                    Ok(provider) => provider,
                    Err(error) => {
                        let _ = owned.response.send(Err(error.into()));
                        return Ok(ForegroundAction::Wait);
                    }
                };
                Ok(ForegroundAction::Effect(ForegroundJob::AgentSpawnSelect {
                    factory: self.agent_provider_factory.clone(),
                    root: Box::new(root),
                    root_provider,
                    spawn,
                    request: owned,
                }))
            }
            AgentOperation::Followup { .. } => {
                self.commit_foreground_agent_admission(transaction, owned, None)
            }
            AgentOperation::Message { target, message } => {
                if message.trim().is_empty() {
                    let _ = owned.response.send(Err(anyhow::anyhow!(
                        "informational message must contain nonempty text"
                    )));
                    return Ok(ForegroundAction::Wait);
                }
                let agent = match self.agent_coordinator.resolve(&target) {
                    Ok(agent) => agent,
                    Err(error) => {
                        let _ = owned.response.send(Err(error));
                        return Ok(ForegroundAction::Wait);
                    }
                };
                if let Some(existing) = agent
                    .inbox
                    .iter()
                    .find(|item| item.id == owned.request.operation_id)
                {
                    let result = if existing.message == message {
                        serde_json::to_value(existing).map_err(anyhow::Error::from)
                    } else {
                        Err(anyhow::anyhow!(
                            "operation identity was reused for a different informational message"
                        ))
                    };
                    let _ = owned.response.send(result);
                    return Ok(ForegroundAction::Wait);
                }
                if let Some(registry) = self.child_execution_by_agent.get(&agent.id) {
                    let owner = crate::provider::ThreadOwner::new(
                        self.session.id.clone().into(),
                        crate::provider::AgentId::new(agent.id.clone())?,
                    )
                    .expect("registered session and agent identities are nonempty");
                    Ok(ForegroundAction::Effect(ForegroundJob::AgentMessageSteer {
                        registry: registry.clone(),
                        owner,
                        message,
                        request: owned,
                    }))
                } else {
                    self.commit_foreground_agent_message(
                        transaction,
                        owned,
                        None,
                        Err(anyhow::anyhow!("child execution is not active")),
                    )
                }
            }
            operation => {
                let result = self.inspect_foreground_agent(operation);
                let _ = owned.response.send(result);
                Ok(ForegroundAction::Wait)
            }
        }
    }

    fn inspect_foreground_agent(&self, operation: crate::agent::AgentOperation) -> Result<Value> {
        use crate::agent::AgentOperation;
        match operation {
            AgentOperation::List { prefix } => {
                let mut entries = Vec::new();
                if prefix
                    .as_deref()
                    .is_none_or(|prefix| "/root".starts_with(prefix))
                {
                    entries.push(json!({"agent":Agent::primary(&self.session.id, self.session.created_at_ms),"latest_task":null}));
                }
                entries.extend(
                    self.agent_coordinator
                        .snapshot(prefix.as_deref())
                        .into_iter()
                        .map(|(agent, task)| json!({"agent":agent,"latest_task":task})),
                );
                Ok(json!({"agents":entries}))
            }
            AgentOperation::Activity { target, limit } => {
                let (agent, task) = self.agent_coordinator.inspection(&target)?;
                let saved;
                let exchange = if let Some(runtime) = self
                    .child_exchange_runtime_by_agent
                    .get(&agent.id)
                    .filter(|runtime| runtime.exchange.id == task.id)
                {
                    &runtime.exchange
                } else {
                    saved = self
                        .store
                        .load_exchange_metadata(&task.id)?
                        .context("child activity exchange missing")?;
                    &saved
                };
                let awaiting = self
                    .permission_coordinator
                    .pending_list()?
                    .iter()
                    .any(|approval| {
                        approval.execution_owner.as_ref().is_some_and(|owner| {
                            owner.agent_id().as_str() == agent.id.as_str()
                                && *owner.session_id()
                                    == crate::provider::SessionId::from(self.session.id.as_str())
                        })
                    });
                Ok(serde_json::to_value(crate::agent::AgentActivity::capture(
                    &agent,
                    &task,
                    exchange,
                    limit.unwrap_or(5),
                    self.clock.now_ms(),
                    awaiting,
                )?)?)
            }
            AgentOperation::ToolOutput {
                call_ref,
                offset,
                limit,
            } => {
                let reference = crate::agent::AgentToolReference::decode(&call_ref)?;
                anyhow::ensure!(
                    reference.session_id == self.session.id,
                    "tool reference belongs to another root session"
                );
                let agent = self.agent_coordinator.resolve(&reference.agent_id)?;
                let task = self
                    .agent_coordinator
                    .task(&reference.exchange_id)
                    .context("tool reference has no admitted child run")?;
                anyhow::ensure!(
                    task.agent_id == agent.id && task.session_id == self.session.id,
                    "tool reference crosses child ownership"
                );
                let exchange = self
                    .store
                    .load_exchange_metadata(&reference.exchange_id)?
                    .context("tool reference exchange missing")?;
                anyhow::ensure!(
                    exchange.agent_id == agent.id && exchange.session_id == self.session.id,
                    "tool reference crosses exchange ownership"
                );
                let tool = exchange
                    .turn
                    .iter()
                    .find(|turn| turn.id() == reference.turn_id)
                    .and_then(|turn| {
                        exchange
                            .tools_for_turn(turn.id())
                            .find(|tool| tool.id == reference.tool_id)
                    })
                    .context("tool reference does not identify a recorded call")?;
                let page = self.store.tool_output_page(
                    &exchange.id,
                    &reference.turn_id,
                    &reference.tool_id,
                    offset,
                    limit,
                )?;
                Ok(
                    json!({"call_ref":call_ref,"status":crate::agent::tool_activity_status(tool),"failed":tool.failed,"output":page.output,"offset":offset,"next_offset":page.next_offset,"total_chars":page.total_chars}),
                )
            }
            _ => anyhow::bail!("agent operation is not an inspection"),
        }
    }

    /// Commit a message only while the original parent authority remains live.
    pub(crate) fn commit_foreground_agent_message(
        &mut self,
        transaction: &mut ForegroundTransaction,
        owned: OwnedAgentRequest,
        captured: Option<(
            crate::provider::ThreadOwner,
            Arc<crate::provider::session_execution::SessionExecutionRegistry>,
        )>,
        result: Result<()>,
    ) -> Result<ForegroundAction> {
        transaction.agent_command_active = false;
        let authority = owned
            .authority
            .as_ref()
            .and_then(|authority| authority.task.clone());
        let apply = || {
            self.validate_foreground_agent_identity(transaction, &owned)?;
            let crate::agent::AgentOperation::Message { target, message } =
                &owned.request.operation
            else {
                anyhow::bail!("child message result has another operation");
            };
            let mut agent = self.agent_coordinator.resolve(target)?;
            if let Some((owner, registry)) = &captured {
                anyhow::ensure!(
                    *owner.session_id()
                        == crate::provider::SessionId::from(self.session.id.as_str())
                        && owner.agent_id().as_str() == agent.id.as_str()
                        && self
                            .child_execution_by_agent
                            .get(&agent.id)
                            .is_some_and(|current| Arc::ptr_eq(current, registry)),
                    "child execution was replaced during message delivery"
                );
            }
            if let Some(existing) = agent
                .inbox
                .iter()
                .find(|item| item.id == owned.request.operation_id)
            {
                anyhow::ensure!(
                    existing.message == *message,
                    "operation identity was reused for a different informational message"
                );
                return Ok(serde_json::to_value(existing)?);
            }
            let delivery = match result {
                Ok(()) => crate::agent::MessageDelivery::Steered,
                Err(error)
                    if error
                        .downcast_ref::<ProviderError>()
                        .is_some_and(|error| matches!(error, ProviderError::DeliveryUncertain)) =>
                {
                    return Err(error.context(
                        "child message delivery is uncertain and was not queued for replay",
                    ));
                }
                Err(error) => {
                    self.trace.record(&self.session.id, "agent.message_queued", json!({"agent_id":agent.id,"reason":"steering_not_accepted","error":error.to_string()}));
                    crate::agent::MessageDelivery::Queued
                }
            };
            let item = crate::agent::AgentMessage {
                id: owned.request.operation_id.clone(),
                ordinal: agent.inbox.last().map_or(1, |item| item.ordinal + 1),
                message: message.clone(),
                delivery,
                task_id: None,
                created_at_ms: self.clock.now_ms(),
            };
            agent.inbox.push(item.clone());
            self.store.save_agent_run(&agent)?;
            if let Some(task) = self
                .agent_coordinator
                .snapshot(None)
                .into_iter()
                .find(|(candidate, _)| candidate.id == agent.id)
                .and_then(|(_, task)| task)
            {
                self.agent_coordinator.retain(agent.clone(), task)?;
            }
            self.agent_registry.insert(agent);
            Ok(serde_json::to_value(item)?)
        };
        let applied = match authority {
            Some(authority) => authority.commit(apply),
            None => apply(),
        };
        let _ = owned.response.send(applied);
        Ok(ForegroundAction::Wait)
    }

    /// Commit a selected child under the captured root authority.
    pub(crate) fn commit_foreground_agent_admission(
        &mut self,
        transaction: &mut ForegroundTransaction,
        mut owned: OwnedAgentRequest,
        selection: Option<crate::agent::AgentSelection>,
    ) -> Result<ForegroundAction> {
        use crate::agent::AgentOperation;
        let authority = owned
            .authority
            .as_ref()
            .and_then(|authority| authority.task.clone());
        let admit = || {
            self.validate_foreground_agent_identity(transaction, &owned)?;
            let mut parent = transaction.state.interaction.clone();
            let admission = match &owned.request.operation {
                AgentOperation::Spawn(spawn) => {
                    let selection = selection.context("child provider selection is missing")?;
                    self.agent_provider_factory
                        .profiles
                        .resolve(Some(&selection.profile))?;
                    self.agent_coordinator.reserve_spawn(
                        &owned.request,
                        spawn,
                        selection,
                        &parent.id,
                        self.clock.now_ms(),
                    )?
                }
                AgentOperation::Followup { target, message } => {
                    let agent = self.agent_coordinator.resolve(target)?;
                    self.agent_provider_factory
                        .profiles
                        .resolve(Some(&agent.definition))?;
                    self.agent_coordinator.reserve_followup(
                        &owned.request,
                        target,
                        message,
                        &parent.id,
                        self.clock.now_ms(),
                    )?
                }
                _ => anyhow::bail!("child admission has another operation"),
            };
            if let Err(error) = parent.append_agent_task(&admission.task).and_then(|_| {
                self.store.admit_agent_task(
                    &admission.agent,
                    &admission.task,
                    &parent,
                    admission.continuation_authorization(),
                )
            }) {
                return Err(match admission.cancel() {
                    Ok(()) => error,
                    Err(cancel) => error.context(format!("release child reservation: {cancel:#}")),
                });
            }
            let (work, launch) = match admission.commit()? {
                crate::agent::AgentCommit::Admitted(work) => (work, true),
                crate::agent::AgentCommit::Existing(work) => (work, false),
            };
            self.agent_registry.insert(work.agent.clone());
            transaction.state.interaction = parent;
            let warning = if matches!(&owned.request.operation, AgentOperation::Spawn(_)) {
                self.agent_provider_factory.profiles.snapshot()?.error
            } else {
                None
            };
            let value =
                json!({"agent":work.agent,"task":work.task,"status":"admitted","warning":warning});
            Ok((
                value,
                if launch && work.launch {
                    Some(work)
                } else {
                    None
                },
            ))
        };
        let admitted = match authority {
            Some(authority) => authority.commit(admit),
            None => admit(),
        };
        self.agent_coordinator
            .retain_task_admission(&owned.request.operation_id, owned.task_admission.take());
        match admitted {
            Ok((value, work)) => {
                if let Some(work) = work {
                    transaction
                        .pending_agent_launch
                        .push_back(PendingAgentLaunch {
                            work,
                            reply: Some((owned.response, value)),
                        });
                } else {
                    let _ = owned.response.send(Ok(value));
                }
                let effects = self.prepare_agent_state()?;
                Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    self.prepare_provider_delivery(effects)?,
                )))
            }
            Err(error) => {
                let _ = owned.response.send(Err(error));
                Ok(ForegroundAction::Wait)
            }
        }
    }

    pub(crate) fn apply_foreground_agent_selection(
        &mut self,
        transaction: &mut ForegroundTransaction,
        owned: OwnedAgentRequest,
        selection: Result<crate::agent::AgentSelection>,
    ) -> Result<ForegroundAction> {
        transaction.agent_command_active = false;
        match selection {
            Ok(selection) => {
                self.commit_foreground_agent_admission(transaction, owned, Some(selection))
            }
            Err(error) => {
                let _ = owned.response.send(Err(error));
                Ok(ForegroundAction::Wait)
            }
        }
    }

    /// Revalidate interrupted tasks before persisting cleanup and notifying the caller.
    pub(crate) fn apply_foreground_agent_interrupt(
        &mut self,
        transaction: &mut ForegroundTransaction,
        operation: AgentInterrupt,
    ) -> Result<ForegroundAction> {
        transaction.agent_command_active = false;
        let AgentInterrupt {
            items,
            mut failures,
            request,
            result: reply,
        } = operation;
        let mut effects = Vec::new();
        for item in items {
            let agent_id = item.agent.id.clone();
            match self.apply_foreground_agent_interrupt_item(item) {
                Ok(item_effects) => effects.extend(item_effects),
                Err(error) => {
                    failures.push(error.context(format!("record interrupted child {agent_id}")))
                }
            }
        }
        let delivery = self.prepare_agent_state().and_then(|state| {
            effects.push(state);
            self.prepare_provider_delivery(AppliedProviderEvent::Effects(effects))
        });
        let _ = request
            .response
            .send(aggregate_failures(failures).map(|()| reply));
        Ok(ForegroundAction::Effect(ForegroundJob::Publication(
            delivery?,
        )))
    }

    /// Record one interrupted child whose approvals were cancelled under live authority.
    ///
    /// The approval events name exactly the requests the unlocked cancellation removed, and the
    /// cleanup obligation is released only when that child's own retry succeeded.
    fn apply_foreground_agent_interrupt_item(
        &mut self,
        item: AgentInterruptItem,
    ) -> Result<Vec<AppliedProviderEvent>> {
        let AgentInterruptItem {
            mut agent,
            owner,
            cancelled,
            cleanup,
            provider,
            approvals,
            cleanup_succeeded,
        } = item;
        let Some(approvals) = approvals else {
            // The unlocked job already reported this child's approval cancellation failure.
            return Ok(Vec::new());
        };
        anyhow::ensure!(
            *owner.session_id() == crate::provider::SessionId::from(self.session.id.as_str())
                && self.agent_coordinator.resolve(&agent.id)?.id == agent.id,
            "interrupted child authority changed during cleanup"
        );
        if let Some(expected) = &provider {
            anyhow::ensure!(
                self.child_provider_by_agent
                    .get(&agent.id)
                    .is_some_and(|current| Arc::ptr_eq(current, expected)),
                "interrupted child provider was replaced during cleanup"
            );
        }
        let mut effects = Vec::new();
        for approval in approvals {
            if let Some(runtime) = self
                .child_exchange_runtime_by_agent
                .get_mut(owner.agent_id().as_str())
            {
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                self.store.save_exchange(&runtime.exchange)?;
            }
            effects.push(AppliedProviderEvent::Update(SessionUpdate::Approval {
                execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                execution_owner: approval
                    .execution_owner
                    .clone()
                    .context("approval lost original owner")?,
                update: Box::new(approval_cancelled(&approval, self.clock.now_ms())?),
            }));
        }
        for mut task in cancelled {
            let current = self
                .agent_coordinator
                .task(&task.id)
                .context("interrupted queued task disappeared")?;
            anyhow::ensure!(
                serde_json::to_value(&current)? == serde_json::to_value(&task)?,
                "interrupted queued task changed during cleanup"
            );
            task.state = ExchangeState::Cancelled;
            task.completed_at_ms = Some(self.clock.now_ms());
            if let Some(mut exchange) = self.store.load_exchange(&task.id)? {
                exchange.finish(
                    crate::exchange::ExchangeOutcome::Cancelled,
                    self.clock.now_ms(),
                )?;
                self.store.complete_agent_task(&agent, &task, &exchange)?;
            }
            self.agent_coordinator.retain(agent.clone(), task)?;
        }
        if let Some(mut task) = cleanup.filter(|_| cleanup_succeeded) {
            let current = self
                .agent_coordinator
                .cleanup_task(&agent.id)
                .context("child cleanup obligation disappeared")?;
            anyhow::ensure!(
                serde_json::to_value(&current)? == serde_json::to_value(&task)?,
                "child cleanup obligation changed during cleanup"
            );
            agent.state = AgentState::Ready;
            agent.updated_at_ms = self.clock.now_ms();
            task.error = None;
            let mut exchange = self
                .store
                .load_exchange(&task.id)?
                .context("child cleanup exchange missing")?;
            exchange.finalization_error = None;
            self.store.complete_agent_task(&agent, &task, &exchange)?;
            self.agent_registry.insert(agent.clone());
            self.agent_coordinator.retain(agent, task.clone())?;
            self.agent_coordinator.settled(
                owner.agent_id().as_str(),
                &task.id,
                crate::agent::AgentSettlementDisposition::Ready,
            )?;
            self.child_provider_by_agent
                .remove(owner.agent_id().as_str());
            self.child_execution_by_agent
                .remove(owner.agent_id().as_str());
        }
        Ok(effects)
    }
    /// Acknowledge only the exact child result sent to this invocation.
    pub(crate) fn apply_foreground_agent_result(
        &mut self,
        transaction: &mut ForegroundTransaction,
        task: crate::agent::AgentTask,
        result: Result<()>,
    ) -> Result<ForegroundAction> {
        if let Err(error) = result {
            self.trace.record(
                &self.session.id,
                "agent.result_queued",
                json!({"task_id": task.id, "error": format!("{error:#}")}),
            );
            return self.prepare_foreground_agent_result(transaction);
        }
        let current = self
            .agent_coordinator
            .task(&task.id)
            .context("delivered child task disappeared")?;
        anyhow::ensure!(
            current.session_id == self.session.id
                && !current.parent_reported
                && serde_json::to_value(&current)? == serde_json::to_value(&task)?,
            "child result changed during delivery"
        );
        self.trace.record(
            &self.session.id,
            "agent.result_delivered",
            json!({"task_id":task.id,"delivery":"steered"}),
        );
        self.acknowledge_agent_result(current)?;
        self.prepare_foreground_agent_result(transaction)
    }

    /// Capture a plan scan as owned work that can run without the broker lock.
    pub(crate) fn prepare_foreground_control(
        &self,
        transaction: &ForegroundTransaction,
        control: crate::plan::execution::ExecutionControl,
    ) -> ForegroundAction {
        let crate::plan::execution::ExecutionControl {
            task_permit,
            execution_id,
            generation,
            operation,
            response,
        } = control;
        let submission = matches!(
            &operation,
            crate::plan::execution::ExecutionOperation::Submit { .. }
        );
        match self.prepare_execution_control(
            &execution_id,
            generation,
            &task_permit,
            operation,
            &transaction.state.interaction,
        ) {
            Ok(job) => ForegroundAction::Effect(ForegroundJob::ExecutionControl {
                job: Box::new(job),
                response,
                submission,
            }),
            Err(error) => {
                let _ = response.send(Err(error));
                ForegroundAction::Wait
            }
        }
    }

    /// Apply evidence under the broker guard and stage a report capture if required.
    pub(crate) fn apply_foreground_control_evidence(
        &mut self,
        transaction: &mut ForegroundTransaction,
        response: tokio::sync::oneshot::Sender<Result<Value>>,
        submission: bool,
        result: Result<execution::ExecutionControlEvidence>,
    ) -> Result<ForegroundAction> {
        let stage = result.and_then(|evidence| {
            self.apply_execution_control(evidence, &mut transaction.state.interaction)
        });
        match stage {
            Ok(execution::ExecutionControlStage::Report(job)) => Ok(ForegroundAction::Effect(
                ForegroundJob::ExecutionControlReport {
                    job,
                    response,
                    submission,
                },
            )),
            Ok(execution::ExecutionControlStage::Complete(value)) => {
                self.finish_foreground_control(transaction, response, submission, Ok(value))
            }
            Err(error) => {
                self.finish_foreground_control(transaction, response, submission, Err(error))
            }
        }
    }

    /// Commit report evidence and deliver the control result or open revision review.
    pub(crate) fn apply_foreground_control_report(
        &mut self,
        transaction: &mut ForegroundTransaction,
        response: tokio::sync::oneshot::Sender<Result<Value>>,
        submission: bool,
        result: Result<execution::ExecutionControlReport>,
    ) -> Result<ForegroundAction> {
        let result = result.and_then(|report| {
            self.commit_execution_control_report(report, &mut transaction.state.interaction)
        });
        self.finish_foreground_control(transaction, response, submission, result)
    }

    fn finish_foreground_control(
        &mut self,
        transaction: &mut ForegroundTransaction,
        response: tokio::sync::oneshot::Sender<Result<Value>>,
        submission: bool,
        result: Result<Value>,
    ) -> Result<ForegroundAction> {
        let committed = result.is_ok();
        if submission
            && result
                .as_ref()
                .is_ok_and(|value| value["status"] == "paused")
        {
            anyhow::ensure!(
                transaction.pending_revision.is_none(),
                "another revision review is already pending"
            );
            let wait = self.revision_review.open()?;
            transaction.state.interaction.pause(self.clock.now_ms());
            transaction.state.interaction.set_awaiting_input(true);
            self.store.save_exchange(&transaction.state.interaction)?;
            transaction.pending_revision = Some((response, wait));
        } else {
            let _ = response.send(result);
        }
        if committed {
            let patch = self.reconcile_timeline()?;
            return Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
            )));
        }
        Ok(ForegroundAction::Wait)
    }

    /// Apply a revision-review request without retaining the broker during check capture.
    pub(crate) fn apply_foreground_revision(
        &mut self,
        transaction: &mut ForegroundTransaction,
        request: crate::plan::revision_review::RevisionReviewRequest,
    ) -> Result<ForegroundAction> {
        use crate::plan::revision_review::RevisionReviewRequest;
        match request {
            RevisionReviewRequest::Capture {
                plan_id,
                digest,
                revision,
                response,
            } => {
                let result = (|| {
                    if let Some(authority) = &transaction.task_authority {
                        authority.validate()?;
                    }
                    anyhow::ensure!(
                        transaction.pending_revision.is_some(),
                        "revision review is no longer active"
                    );
                    anyhow::ensure!(
                        self.current_plan_id()?.as_deref() == Some(plan_id.as_str()),
                        "review belongs to another plan"
                    );
                    if let Some(revision) = revision {
                        let source = self.observer.capture_plan_revision(&plan_id, revision)?;
                        anyhow::ensure!(
                            crate::plan::digest(&serde_json::to_vec(&source.document)?) == digest,
                            "plan revision changed before opening"
                        );
                        Ok(source)
                    } else {
                        self.observer.capture_plan_review(&plan_id, &digest)
                    }
                })();
                let _ = response.send(result);
                Ok(ForegroundAction::Wait)
            }
        }
    }

    /// Capture a plan scan as owned work that can run without the broker lock.
    pub(crate) fn prepare_foreground_scan(
        &self,
        transaction: &ForegroundTransaction,
    ) -> Result<ForegroundJob> {
        let execution_id = transaction
            .execution_id
            .as_deref()
            .context("foreground execution is missing")?;
        Ok(ForegroundJob::PlanScan(Box::new(
            self.prepare_execution_scan(execution_id)?,
        )))
    }

    /// Revalidate a completed scan and stage its ordered live updates.
    pub(crate) fn apply_foreground_scan(
        &mut self,
        transaction: &ForegroundTransaction,
        result: execution::ExecutionScanResult,
    ) -> Result<AppliedProviderEvent> {
        let progress = self.commit_execution_scan(result)?;
        let patch = self.reconcile_live_interaction(Some(&transaction.state.interaction), None)?;
        Ok(AppliedProviderEvent::Effects(vec![
            AppliedProviderEvent::Timeline(patch),
            AppliedProviderEvent::Update(SessionUpdate::PlanProgress {
                progress: serde_json::from_value(progress)?,
            }),
        ]))
    }

    /// Prepare ordered effects while holding broker state; delivery may run after releasing it.
    pub(crate) fn prepare_provider_delivery(
        &self,
        applied: AppliedProviderEvent,
    ) -> Result<PendingProviderDelivery> {
        let mut updates = Vec::new();
        let mut events = Vec::new();
        let mut pending = vec![applied];
        while let Some(effect) = pending.pop() {
            let (update, timeline) = match effect {
                AppliedProviderEvent::None => (None, None),
                AppliedProviderEvent::Update(update) => (Some(update), None),
                AppliedProviderEvent::Live(pending) => {
                    (Some(pending.update), Some(pending.timeline))
                }
                AppliedProviderEvent::Timeline(patch) => (None, Some(patch)),
                AppliedProviderEvent::Effects(effects) => {
                    pending.extend(effects.into_iter().rev());
                    continue;
                }
            };
            if let Some(update) = update {
                if update.wakes_input()
                    && let Some(wakeup) = &self.input_wakeup
                {
                    wakeup.notify_one();
                }
                if self.trace.is_enabled() {
                    self.trace.record(
                        &self.session.id,
                        "lua.event.emitted",
                        publication::encode_session_update(&update)?,
                    );
                }
                if self.event_sink.is_none() {
                    events.push(publication::encode_session_event(
                        &self.session.id,
                        &update,
                    )?);
                }
                updates.push(update);
            }
            if let Some(patch) = timeline
                && !patch.is_empty()
            {
                if self.event_sink.is_none() {
                    events.push(self.event("document_changed", patch.notification())?);
                }
                updates.push(SessionUpdate::DocumentChanged { patch });
            }
        }
        Ok(PendingProviderDelivery {
            sink: self.event_sink.clone(),
            updates,
            events,
        })
    }

    /// Validate ownership and apply supported provider state without awaiting publication.
    pub(crate) fn apply_provider_event(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        observation: HarnessEvent,
        _events: &mut Vec<SessionEvent>,
    ) -> Result<AppliedProviderEvent> {
        let scope = observation.scope();
        if scope.owner() != crate::provider::ThreadOwner::root(&self.session.id) {
            return self.apply_observed_child(&observation);
        }

        anyhow::ensure!(
            scope.owner() == crate::provider::ThreadOwner::root(&self.session.id),
            "root event crosses original owner"
        );
        if let Some(invocation) = scope.invocation() {
            anyhow::ensure!(
                invocation.exchange_id
                    == forge_provider_api::ExchangeId::from(interaction.id.as_str())
                    || interaction
                        .turn
                        .iter()
                        .any(|turn| &turn.provider().scope == invocation),
                "event has no owning exchange"
            );
        }
        match &observation {
            HarnessEvent::Native {
                source,
                scope,
                event,
            } => match event.as_ref() {
                ProviderEvent::Agent { agent_id, event } => {
                    self.apply_agent_lifecycle(source, scope, agent_id, event)
                }
                ProviderEvent::Provider {
                    event: forge_provider_api::ProviderConnectionEvent::Unavailable(_),
                } => self.apply_observed_child(&observation),
                ProviderEvent::Provider {
                    event:
                        forge_provider_api::ProviderConnectionEvent::CapabilitiesChanged(manifest),
                } => {
                    if self.provider.binding_source_status(
                        &source.binding_id,
                        &scope.owner(),
                        None,
                    )? == crate::provider::NativeBindingStatus::Active
                    {
                        self.apply_effective_capabilities(manifest.clone())?;
                    }
                    Ok(AppliedProviderEvent::None)
                }
                ProviderEvent::Thread {
                    thread_id,
                    event: forge_provider_api::ThreadEvent::IdentityResolved(id),
                } => {
                    if self.provider.binding_source_status(
                        &source.binding_id,
                        &scope.owner(),
                        Some(thread_id),
                    )? == crate::provider::NativeBindingStatus::RetiredKnown
                    {
                        return Ok(AppliedProviderEvent::None);
                    }
                    self.retain_session_identity(&scope.owner(), id)?;
                    Ok(AppliedProviderEvent::None)
                }
                ProviderEvent::Thread {
                    event: forge_provider_api::ThreadEvent::CompactionUpdated(compaction),
                    ..
                } => Ok(
                    match self.apply_compaction_observation(
                        runtime,
                        interaction,
                        observation.clone(),
                        (**compaction).clone(),
                    )? {
                        Some(patch) => AppliedProviderEvent::Timeline(patch),
                        None => AppliedProviderEvent::None,
                    },
                ),
                ProviderEvent::Thread {
                    thread_id,
                    event: forge_provider_api::ThreadEvent::ContextUsage(usage),
                } => {
                    if self.provider.binding_source_status(
                        &source.binding_id,
                        &scope.owner(),
                        Some(thread_id),
                    )? == crate::provider::NativeBindingStatus::RetiredKnown
                    {
                        return Ok(AppliedProviderEvent::None);
                    }
                    self.session.context_usage = crate::session::ContextUsage::from_report(usage);
                    self.context_usage_fresh = true;
                    self.apply_exchange_content(runtime, interaction, observation)
                }
                ProviderEvent::Provider { .. }
                | ProviderEvent::Thread { .. }
                | ProviderEvent::Turn { .. }
                | ProviderEvent::Item { .. } => {
                    self.apply_exchange_content(runtime, interaction, observation)
                }
            },
            HarnessEvent::Host { scope, event } => match event {
                HostEvent::CapabilitiesNegotiated {
                    binding_id,
                    manifest,
                    history,
                } => {
                    self.apply_negotiated_capabilities(
                        scope,
                        binding_id,
                        manifest,
                        history.as_ref(),
                    )?;
                    Ok(AppliedProviderEvent::None)
                }
                HostEvent::DirectoryAccessGranted(grant) => Ok(AppliedProviderEvent::Update(
                    self.apply_directory_access(vec![grant.grant.clone()])?,
                )),
                HostEvent::PermissionApproval {
                    execution_kind,
                    event,
                } => Ok(
                    match self.apply_approval_observation(
                        interaction,
                        scope.clone(),
                        *execution_kind,
                        event.clone(),
                    )? {
                        Some(pending) => AppliedProviderEvent::Live(pending),
                        None => AppliedProviderEvent::None,
                    },
                ),
                HostEvent::ChildWait {
                    thread_id,
                    turn_id,
                    event,
                } => {
                    anyhow::ensure!(
                        interaction
                            .turn
                            .iter()
                            .any(|turn| turn.provider().thread_id == *thread_id
                                && turn.provider().turn_id == *turn_id
                                && Some(&turn.provider().scope) == scope.invocation()),
                        "child wait does not address its original parent turn"
                    );
                    Ok(AppliedProviderEvent::Live(self.apply_parent_boundary(
                        runtime,
                        interaction,
                        event.clone(),
                    )?))
                }
                HostEvent::RetryUpdated { .. }
                | HostEvent::SkillRead { .. }
                | HostEvent::Failure { .. } => {
                    self.apply_exchange_content(runtime, interaction, observation)
                }
            },
        }
    }

    /// Persist directory grants and prepare the resulting session update.
    fn apply_directory_access(
        &mut self,
        grants: Vec<crate::permissions::directory::DirectoryGrant>,
    ) -> Result<SessionUpdate> {
        for grant in grants {
            let directories = match grant.operation {
                crate::permissions::directory::DirectoryOperation::Read => {
                    &mut self.session.access.readable_directory
                }
                crate::permissions::directory::DirectoryOperation::Write => {
                    &mut self.session.access.writable_directory
                }
            };
            if !directories.contains(&grant.path) {
                directories.push(grant.path);
            }
        }
        self.save_session()?;
        self.session.active_task = self.task_controller.current()?;
        Ok(SessionUpdate::ExecutionState {
            snapshot: Box::new(self.snapshot()?),
        })
    }

    /// Forget negotiated support only after the prior binding generation has been collected.
    pub(crate) fn clear_effective_capabilities(&mut self) -> Result<()> {
        let capability = self.provider.descriptor().capability;
        let mut published = self
            .observer
            .metadata
            .write()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?;
        published.capability = capability.clone();
        published.manifest = None;
        self.capability = capability;
        self.latest_manifest = None;
        Ok(())
    }

    /// Adopt a bridge-published open or configuration manifest for the root conversation.
    ///
    /// Child bindings negotiate their own manifests, which never change the root session's
    /// persisted fork and compaction support, so this applies only root-owned snapshots. A
    /// manifest queued by a binding that retired before observation is stale, matching the
    /// native capability path's active-binding gate.
    fn apply_negotiated_capabilities(
        &mut self,
        scope: &forge_provider_api::EventScope,
        binding_id: &forge_provider_api::BindingId,
        manifest: &forge_provider_api::CapabilityManifest,
        history: Option<&forge_provider_api::ProviderSessionId>,
    ) -> Result<()> {
        if scope.owner() == crate::provider::ThreadOwner::root(&self.session.id)
            && self
                .provider
                .binding_source_status(binding_id, &scope.owner(), None)?
                == crate::provider::NativeBindingStatus::Active
        {
            self.apply_effective_capabilities(manifest.clone())?;
            // The first input opens the native conversation lazily; retaining its reported
            // history here lets a restart or fork resume that conversation instead of a new one.
            if let Some(history) = history
                && self.session.provider_session_id.as_deref() != Some(history.as_str())
            {
                self.retain_session_identity(&scope.owner(), history)?;
            }
        }
        Ok(())
    }

    /// Apply an ordered negotiated snapshot before later input or configuration admission.
    fn apply_effective_capabilities(
        &mut self,
        manifest: forge_provider_api::CapabilityManifest,
    ) -> Result<()> {
        use forge_provider_api::capability::{CapabilityId, Support};
        anyhow::ensure!(
            manifest.provider == self.provider.descriptor().kind,
            "capability snapshot crosses provider ownership"
        );
        if self
            .latest_manifest
            .as_ref()
            .is_some_and(|current| current.effective_revision > manifest.effective_revision)
        {
            return Ok(());
        }
        let supports = |id| {
            manifest.capability.iter().any(|capability| {
                capability.id == id
                    && matches!(
                        capability.support,
                        Support::Supported | Support::Limited { .. }
                    )
            })
        };
        let mut capability = self.capability.clone();
        capability.native_fork = supports(CapabilityId::NativeFork);
        capability.native_compact = supports(CapabilityId::NativeCompact);
        capability.native_steer = supports(CapabilityId::NativeSteer);
        capability.native_turn_rollback = supports(CapabilityId::NativeTurnRollback);
        capability.native_conversation_rewind = supports(CapabilityId::NativeConversationRewind);
        capability.model_selection = supports(CapabilityId::ModelSelection);
        capability.effort_selection = supports(CapabilityId::EffortSelection);
        capability.fast_mode = supports(CapabilityId::FastMode);
        capability.ultrafast_mode = supports(CapabilityId::UltrafastMode);
        capability.os_sandbox = supports(CapabilityId::OsSandbox);
        capability.background_terminals = supports(CapabilityId::BackgroundTerminals);
        capability.permission_control = supports(CapabilityId::PermissionCallbacks);
        capability.compaction.lifecycle = supports(CapabilityId::CompactionLifecycle);
        capability.compaction.background = supports(CapabilityId::BackgroundCompaction);
        capability.compaction.summary = supports(CapabilityId::CompactionSummary);
        capability.compaction.metrics = supports(CapabilityId::CompactionMetrics);
        capability.catalog.skill = supports(CapabilityId::SkillCatalog);
        capability.catalog.mcp = supports(CapabilityId::McpCatalog);
        let mut published = self
            .observer
            .metadata
            .write()
            .map_err(|_| anyhow::anyhow!("observation metadata lock poisoned"))?;
        published.capability = capability.clone();
        published.manifest = Some(manifest.clone());
        let native_flags_changed = self.session.native_fork != capability.native_fork
            || self.session.native_compact != capability.native_compact;
        self.session.native_fork = capability.native_fork;
        self.session.native_compact = capability.native_compact;
        self.capability = capability;
        self.latest_manifest = Some(manifest);
        drop(published);
        if native_flags_changed {
            self.save_session()?;
        }
        self.observer.admission.changed.notify_one();
        Ok(())
    }

    /// Apply approval ownership and return ordered publication work without awaiting delivery.
    fn apply_approval_observation(
        &mut self,
        interaction: &mut Exchange,
        scope: crate::provider::EventScope,
        execution_kind: crate::provider::ApprovalExecutionKind,
        update: PermissionApprovalEvent,
    ) -> Result<Option<PendingLivePublication>> {
        let (id, pending) = match &update {
            PermissionApprovalEvent::Requested(request) => (request.request_id.as_str(), true),
            PermissionApprovalEvent::Resolved(resolution) => {
                (resolution.request_id.as_str(), false)
            }
            PermissionApprovalEvent::Cancelled(cancellation) => {
                (cancellation.request_id.as_str(), false)
            }
        };
        if pending
            && !self
                .permission_coordinator
                .pending_list()?
                .iter()
                .any(|request| request.id == id)
        {
            return Ok(None);
        }
        if !pending
            && !interaction.observe_host_event(
                &scope,
                &HostEvent::PermissionApproval {
                    execution_kind,
                    event: update.clone(),
                },
                self.clock.now_ms(),
            )?
        {
            return Ok(None);
        }
        interaction.observe_blocker(format!("approval:{id}"), pending, self.clock.now_ms());
        self.store.save_exchange(interaction)?;
        let timeline = self.reconcile_timeline()?;
        Ok(Some(PendingLivePublication {
            update: SessionUpdate::Approval {
                execution_owner: scope.owner().clone(),
                execution_kind,
                update: Box::new(update),
            },
            timeline,
        }))
    }

    /// Publish actual Harness child-wait transitions under their admitted parent turn.
    fn publish_child_wait(
        &mut self,
        transaction: &mut ForegroundTransaction,
        event: ChildWaitEvent,
    ) -> Result<PendingProviderDelivery> {
        let scope = transaction.state.scope.clone();
        let observation = HarnessEvent::Host {
            scope: scope.clone().into(),
            event: HostEvent::ChildWait {
                thread_id: scope.owner().thread_id(),
                turn_id: scope.turn_id()?,
                event,
            },
        };
        self.trace.record(
            &self.session.id,
            "host.child_wait",
            serde_json::to_value(&observation)?,
        );
        let runtime = transaction
            .state
            .runtime
            .as_mut()
            .context("child wait has no retained parent runtime")?;
        let effects = self.apply_provider_event(
            runtime,
            &mut transaction.state.interaction,
            observation,
            &mut Vec::new(),
        )?;
        self.prepare_provider_delivery(effects)
    }

    /// Apply a parent wait transition and prepare its ordered publication.
    fn apply_parent_boundary(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        update: ChildWaitEvent,
    ) -> Result<PendingLivePublication> {
        let now_ms = self.clock.now_ms();
        match update {
            ChildWaitEvent::Started { agent_count } => {
                interaction.observe_blocker("agent_wait".into(), true, now_ms);
                interaction.close_running_messages();
                runtime.active_wait = Some(ActiveWait {
                    exchange_id: interaction.id.clone(),
                    started_at_ms: now_ms,
                    agent_count,
                });
            }
            ChildWaitEvent::Updated { agent_count } => {
                if let Some(wait) = runtime.active_wait.as_mut() {
                    wait.agent_count = agent_count;
                }
            }
            ChildWaitEvent::Finished => {
                interaction.observe_blocker("agent_wait".into(), false, now_ms);
                runtime.active_wait = None;
            }
        }
        self.store.save_exchange(interaction)?;
        self.active_wait_projection = runtime.active_wait.clone();
        let update = SessionUpdate::Timeline {
            update: TimelineUpdate::Wait {
                exchange_id: interaction.id.clone(),
                wait: runtime.active_wait.clone(),
            },
        };
        let timeline = self.reconcile_live_interaction(Some(interaction), None)?;
        Ok(PendingLivePublication { update, timeline })
    }

    /// Apply an already committed acceptance receipt directly to its original session input owner.
    pub(crate) fn apply_input_acceptance(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        receipt: &Arc<crate::provider::submission::Submission>,
        acknowledgement: &InputPresentation,
    ) -> Result<AppliedProviderEvent> {
        let input_id = Some(
            receipt
                .record()?
                .input_message_id
                .unwrap_or_else(|| receipt.receipt_id().as_str().to_owned()),
        );
        let mut effects = Vec::new();
        let now_ms = self.clock.now_ms();
        match acknowledgement {
            // A task entry is the exchange's own prompt, so its acceptance is not provider
            // activity and an output-free cancellation may still retract it.
            InputPresentation::TaskStarted { start, prompt } => {
                let input_id = input_id
                    .as_ref()
                    .context("task entry is missing its input identity")?;
                if self.task_controller.current()?.is_some_and(|task| {
                    task.entry.as_ref().is_some_and(|entry| {
                        entry.id.as_str() == input_id
                            && entry.review_exchange.as_deref() == Some(&interaction.id)
                    })
                }) {
                    interaction.observe_blocker("agent_wait".into(), false, now_ms);
                    runtime.active_wait = None;
                    self.store.save_exchange(interaction)?;
                    return Ok(AppliedProviderEvent::Effects(effects));
                }
                let id = format!("{}:task-start:{input_id}", self.session.id);
                if let Some(existing) = self.store.load_session_event(&self.session.id, &id)? {
                    anyhow::ensure!(
                        matches!(existing.detail,
                                    SessionEventKind::TaskStarted { start: previous, prompt: previous_prompt }
                                    if previous == *start && previous_prompt == *prompt),
                        "task entry identity was reused for different content"
                    );
                    return Ok(AppliedProviderEvent::Effects(effects));
                }
                interaction.observe_blocker("agent_wait".into(), false, now_ms);
                runtime.active_wait = None;
                self.store.save_exchange(interaction)?;
                let task_event = crate::timeline::SessionEventRecord {
                    id,
                    session_id: self.session.id.clone(),
                    owner: self
                        .store
                        .capture_timeline_owner(&self.session.id, &interaction.id)?,
                    created_at_ms: now_ms,
                    detail: SessionEventKind::TaskStarted {
                        start: start.clone(),
                        prompt: prompt.clone(),
                    },
                };
                self.store.save_session_event(&task_event)?;
                let mut displayed = interaction.clone();
                crate::timeline::ownership::insert_event(
                    &mut displayed,
                    crate::timeline::TimelineEvent::Session {
                        event: Box::new(task_event),
                    },
                )?;
                effects.push(self.prepare_live_interaction(
                    SessionUpdate::Timeline {
                        update: TimelineUpdate::Node { prompt: None },
                    },
                    &displayed,
                )?);
            }
            InputPresentation::Steering { text } => {
                // Accepted steering joins the native history, so retraction is no longer safe.
                runtime.retraction_eligibility.consume();
                if input_id.as_ref().is_some_and(|id| interaction.node_list.iter().any(|node|
                                matches!(node, ExchangeNode::ExchangeInput { prompt } if prompt.id == *id))) {
                                return Ok(AppliedProviderEvent::Effects(effects));
                            }
                interaction.observe_blocker("agent_wait".into(), false, now_ms);
                runtime.active_wait = None;
                interaction.append_acknowledged_input(
                    input_id.clone(),
                    Some(receipt.receipt_id().clone()),
                    crate::exchange::InputIntent::Steering,
                    text.clone(),
                    now_ms,
                )?;
                self.store.save_exchange(interaction)?;
                let prompt = interaction.node_list.last().and_then(|node| match node {
                    ExchangeNode::ExchangeInput { prompt } => Some(prompt.clone()),
                    _ => None,
                });
                effects.push(self.prepare_live_interaction(
                    SessionUpdate::Timeline {
                        update: TimelineUpdate::Node { prompt },
                    },
                    interaction,
                )?);
            }
        }
        self.active_wait_projection = runtime.active_wait.clone();
        effects.push(self.prepare_live_interaction(
            SessionUpdate::Timeline {
                update: TimelineUpdate::Wait {
                    exchange_id: interaction.id.clone(),
                    wait: runtime.active_wait.clone(),
                },
            },
            interaction,
        )?);
        Ok(AppliedProviderEvent::Effects(effects))
    }

    /// Persist accepted input once under the journal's original owner, including after UI failure.
    fn apply_durable_input_acceptance(
        &mut self,
        interaction: &mut Exchange,
        scope: &crate::provider::InvocationScope,
        receipt: &Arc<crate::provider::submission::Submission>,
        presentation: &InputPresentation,
    ) -> Result<()> {
        anyhow::ensure!(
            scope.exchange_id.as_str() == interaction.id
                && scope.owner().session_id().as_str() == interaction.session_id
                && scope.owner().agent_id().as_str() == interaction.agent_id,
            "accepted input crosses original ownership"
        );
        self.acknowledge_user_child_reports(scope, receipt)?;
        let id = receipt
            .record()?
            .input_message_id
            .unwrap_or_else(|| receipt.receipt_id().as_str().to_owned());
        let now_ms = self.clock.now_ms();
        match presentation {
            InputPresentation::Steering { text } => {
                if !interaction.node_list.iter().any(|node| matches!(node, ExchangeNode::ExchangeInput { prompt } if prompt.id == id)) {
                    interaction.append_acknowledged_input(Some(id), Some(receipt.receipt_id().clone()), crate::exchange::InputIntent::Steering, text.clone(), now_ms)?;
                }
            }
            InputPresentation::TaskStarted { start, prompt } => {
                let event_id = format!("{}:task-start:{id}", self.session.id);
                if self
                    .store
                    .load_session_event(&self.session.id, &event_id)?
                    .is_none()
                {
                    let record = crate::timeline::SessionEventRecord {
                        id: event_id,
                        session_id: self.session.id.clone(),
                        owner: self
                            .store
                            .capture_timeline_owner(&self.session.id, &interaction.id)?,
                        created_at_ms: now_ms,
                        detail: SessionEventKind::TaskStarted {
                            start: start.clone(),
                            prompt: prompt.clone(),
                        },
                    };
                    self.store.save_session_event(&record)?;
                }
            }
        }
        interaction.observe_blocker("agent_wait".into(), false, now_ms);
        self.store.save_exchange(interaction)
    }

    /// Apply ordinary content and retain ordered publication effects.
    fn apply_exchange_content(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        observation: HarnessEvent,
    ) -> Result<AppliedProviderEvent> {
        self.prepare_provider_observation(interaction, &observation);
        if let Some(routed) = self.apply_settled_parent_event(interaction, &observation)? {
            return Ok(routed);
        }
        let applied = match &observation {
            HarnessEvent::Native { scope, event, .. } => {
                let event = event.as_ref();
                interaction.observe_provider_event(scope, event, self.clock.now_ms())?
            }
            HarnessEvent::Host { scope, event } => {
                interaction.observe_host_event(scope, event, self.clock.now_ms())?
            }
        };
        if !applied {
            return Ok(SessionUpdate::from_observation(&observation)
                .map_or(AppliedProviderEvent::None, AppliedProviderEvent::Update));
        }
        if let HarnessEvent::Native { event, .. } = &observation {
            match event.as_ref() {
                ProviderEvent::Turn {
                    event: forge_provider_api::TurnEvent::ChecklistUpdated(update),
                    ..
                } => {
                    runtime.retraction_eligibility.consume();
                    runtime.checklist.replace(update);
                    interaction.checklist = Some(runtime.checklist.snapshot().clone());
                }
                ProviderEvent::Item { item_id, event, .. } => {
                    runtime.retraction_eligibility.observe_item(item_id, event);
                }
                ProviderEvent::Provider { .. }
                | ProviderEvent::Thread { .. }
                | ProviderEvent::Turn { .. }
                | ProviderEvent::Agent { .. } => {}
            }
        }
        let runtime_update = if let HarnessEvent::Native { scope, event, .. } = &observation
            && let ProviderEvent::Turn {
                thread_id,
                turn_id,
                event:
                    forge_provider_api::TurnEvent::Started(_)
                    | forge_provider_api::TurnEvent::ModelResolved(_),
            } = event.as_ref()
            && scope.owner().is_root()
            && interaction.turn.last().is_some_and(|turn| {
                &turn.provider().thread_id == thread_id && &turn.provider().turn_id == turn_id
            }) {
            // A started turn reports its model later; keep the last confirmed model until then.
            // Configuration changes that select another model clear the confirmation themselves.
            if let Some(model) = interaction
                .turn
                .last()
                .and_then(|turn| turn.resolved_model())
            {
                self.session.resolved_model = Some(model.model_id.as_str().to_owned());
            }
            self.save_session()?;
            Some(SessionUpdate::Runtime {
                session_id: self.session.id.clone(),
                runtime: crate::provider::ProviderRuntime {
                    provider: self.session.provider.clone(),
                    model: self.session.resolved_model.clone(),
                },
            })
        } else {
            None
        };
        attribute_provider_tool(interaction, &mut runtime.checklist, &observation)?;
        match &observation {
            HarnessEvent::Native { event, .. }
                if matches!(
                    event.as_ref(),
                    ProviderEvent::Item {
                        event: forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::Output(
                                forge_provider_api::TextUpdate::Append(_),
                            ),
                        },
                        ..
                    }
                ) =>
            {
                self.store
                    .save_tool_output_delta_behind(interaction, event)?
            }
            // Streamed text is shown before it is saved; the writer thread saves it.
            HarnessEvent::Native { event, .. } if event.is_output_delta() => {
                self.store.save_exchange_behind(interaction)?
            }
            HarnessEvent::Native { .. } | HarnessEvent::Host { .. } => {
                self.store.save_exchange(interaction)?
            }
        }
        let content = self.prepare_live_interaction(observation, interaction)?;
        Ok(match runtime_update {
            Some(update) => {
                AppliedProviderEvent::Effects(vec![AppliedProviderEvent::Update(update), content])
            }
            None => content,
        })
    }

    fn prepare_provider_observation(
        &mut self,
        interaction: &mut Exchange,
        observation: &HarnessEvent,
    ) {
        if !self.capability.compaction.background
            && let HarnessEvent::Native {
                event: boxed_native_event,
                ..
            } = observation
            && let ProviderEvent::Turn {
                thread_id,
                event: forge_provider_api::TurnEvent::Completed(_),
                ..
            } = boxed_native_event.as_ref()
        {
            let now_ms = observation.observed_at_ms(self.clock.now_ms());
            interaction.unconfirm_compaction(thread_id.as_str(), now_ms);
            for child in self.child_exchange_runtime_by_agent.values_mut() {
                child
                    .exchange
                    .unconfirm_compaction(thread_id.as_str(), now_ms);
            }
        }
    }

    /// Apply compaction state and return the timeline publication effect.
    fn apply_compaction_observation(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        provider_event: HarnessEvent,
        mut compaction: crate::provider::Compaction,
    ) -> Result<Option<TimelinePatch>> {
        if compaction.state == crate::provider::CompactionState::Running
            && compaction.thread_id
                == crate::provider::ThreadOwner::root(&self.session.id)
                    .thread_id()
                    .as_str()
        {
            compaction.result.context_tokens_before = compaction
                .result
                .context_tokens_before
                .or_else(|| self.session.context_usage.as_ref().map(|usage| usage.used));
        }
        let now_ms = provider_event.observed_at_ms(self.clock.now_ms());
        let previous_owner = self.compaction_exchange_owner(&compaction)?;
        let owns_other_exchange = previous_owner.as_ref().is_some_and(|owner| {
            owner.id != interaction.id
                && !self
                    .child_exchange_runtime_by_agent
                    .values()
                    .any(|child| child.exchange.id == owner.id)
        });
        let owns_session_event =
            self.store
                .list_session_event(&self.session.id)?
                .iter()
                .any(|record| {
                    matches!(&record.detail, SessionEventKind::Compaction { compaction: existing }
                if existing.id == compaction.id && existing.thread_id == compaction.thread_id)
                });
        if owns_other_exchange || owns_session_event {
            self.retain_external_compaction(compaction, now_ms)?;
            let patch = self.reconcile_timeline()?;
            return Ok(Some(patch));
        }
        let child_id = self
            .child_exchange_runtime_by_agent
            .iter()
            .find(|(_, child)| {
                child
                    .exchange
                    .turn
                    .iter()
                    .any(|turn| turn.provider().thread_id.as_str() == compaction.thread_id)
            })
            .map(|(id, _)| id.clone());
        if let Some(child_id) = child_id {
            let mut child = self
                .child_exchange_runtime_by_agent
                .remove(&child_id)
                .context("compaction owner disappeared")?;
            child.exchange.observe_compaction(compaction, now_ms);
            let result = self
                .store
                .save_exchange(&child.exchange)
                .and_then(|()| self.reconcile_live_interaction(Some(&child.exchange), None));
            self.child_exchange_runtime_by_agent.insert(child_id, child);
            return Ok(Some(result?));
        } else if compaction.thread_id
            == crate::provider::ThreadOwner::root(&self.session.id)
                .thread_id()
                .as_str()
            || interaction
                .turn
                .iter()
                .any(|turn| turn.provider().thread_id.as_str() == compaction.thread_id)
        {
            runtime.retraction_eligibility.consume();
            interaction.observe_compaction(compaction, now_ms);
            self.store.save_exchange(interaction)?;
            return Ok(Some(
                self.reconcile_live_interaction(Some(interaction), None)?,
            ));
        }
        Ok(None)
    }

    /// Apply a late observation to its settled owner and retain its ordered effects.
    fn apply_settled_parent_event(
        &mut self,
        current: &Exchange,
        observation: &HarnessEvent,
    ) -> Result<Option<AppliedProviderEvent>> {
        let Some(address) = native_turn_address(observation) else {
            return Ok(None);
        };
        if current.turn.iter().any(|turn| turn.provider() == &address) {
            return Ok(None);
        }
        let HarnessEvent::Native { scope, event, .. } = observation else {
            return Ok(None);
        };
        let event = event.as_ref();
        let Some(mut owner) = self
            .store
            .load_provider_exchange(&self.session.id, &address)?
        else {
            return Ok(None);
        };
        // The canonical item owner validates positively established background lifetime and late accounting.
        if !owner.observe_provider_event(scope, event, self.clock.now_ms())? {
            return Ok(Some(AppliedProviderEvent::None));
        }
        match event {
            event @ ProviderEvent::Item {
                event:
                    forge_provider_api::ItemEvent::Tool {
                        event:
                            forge_provider_api::ToolItemEvent::Output(
                                forge_provider_api::TextUpdate::Append(_),
                            ),
                    },
                ..
            } => self.store.save_tool_output_delta_behind(&owner, event)?,
            ProviderEvent::Provider { .. }
            | ProviderEvent::Thread { .. }
            | ProviderEvent::Turn { .. }
            | ProviderEvent::Item { .. }
            | ProviderEvent::Agent { .. } => self.store.save_exchange(&owner)?,
        }
        let status = self.live_interaction_status(Some(current))?;
        let incremental = match event {
            event @ ProviderEvent::Item {
                event:
                    forge_provider_api::ItemEvent::Tool {
                        event:
                            forge_provider_api::ToolItemEvent::Output(
                                forge_provider_api::TextUpdate::Append(_),
                            ),
                    },
                ..
            } => self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .append_tool_output(&owner, event)?,
            event @ ProviderEvent::Item {
                event: forge_provider_api::ItemEvent::Message { .. },
                ..
            } => self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .update_message(&owner, event)?,
            ProviderEvent::Provider { .. }
            | ProviderEvent::Thread { .. }
            | ProviderEvent::Turn { .. }
            | ProviderEvent::Item { .. }
            | ProviderEvent::Agent { .. } => None,
        };
        let patch = match incremental {
            Some(patch) => patch,
            None => self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .update_live(
                    Some(&owner),
                    None,
                    status,
                    self.observer.status_thoughts.load(Ordering::Relaxed),
                )?,
        };
        let mut effects = Vec::new();
        if let Some(update) = SessionUpdate::from_observation(observation) {
            effects.push(AppliedProviderEvent::Update(update));
        }
        effects.push(AppliedProviderEvent::Timeline(patch));
        Ok(Some(AppliedProviderEvent::Effects(effects)))
    }

    /// Apply and deliver a settled-owner event in staged-interface tests.
    #[cfg(test)]
    async fn test_apply_settled_parent_event(
        &mut self,
        current: &Exchange,
        provider_event: &HarnessEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let Some(applied) = self.apply_settled_parent_event(current, provider_event)? else {
            return Ok(false);
        };
        let delivery = self.prepare_provider_delivery(applied)?;
        event.extend(delivery.deliver().await?);
        Ok(true)
    }

    /// Capture an ordered update and timeline patch without awaiting a presentation sink.
    fn prepare_live_interaction(
        &mut self,
        update: impl Into<publication::LiveUpdate>,
        interaction: &Exchange,
    ) -> Result<AppliedProviderEvent> {
        let update = update.into();
        let incremental = match &update {
            publication::LiveUpdate::Observation(observation) => match observation.as_ref() {
                HarnessEvent::Native { event, .. }
                    if matches!(
                        event.as_ref(),
                        ProviderEvent::Item {
                            event: forge_provider_api::ItemEvent::Tool {
                                event: forge_provider_api::ToolItemEvent::Output(
                                    forge_provider_api::TextUpdate::Append(_),
                                ),
                            },
                            ..
                        }
                    ) =>
                {
                    self.presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .append_tool_output(interaction, event)?
                }
                HarnessEvent::Native { event, .. }
                    if matches!(
                        event.as_ref(),
                        ProviderEvent::Item {
                            event: forge_provider_api::ItemEvent::Message { .. }
                                | forge_provider_api::ItemEvent::Completed(
                                    forge_provider_api::ItemCompletion::UserMessage(_)
                                        | forge_provider_api::ItemCompletion::AgentMessage(_)
                                        | forge_provider_api::ItemCompletion::Reasoning(_),
                                ),
                            ..
                        }
                    ) =>
                {
                    self.presentation
                        .lock()
                        .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                        .update_message(interaction, event)?
                }
                HarnessEvent::Native { .. } | HarnessEvent::Host { .. } => None,
            },
            publication::LiveUpdate::StateChanged | publication::LiveUpdate::Notification(_) => {
                None
            }
        };
        let update = match update {
            publication::LiveUpdate::StateChanged => None,
            publication::LiveUpdate::Observation(observation) => {
                SessionUpdate::from_observation(&observation)
            }
            publication::LiveUpdate::Notification(update) => Some(*update),
        };
        let patch = match incremental {
            Some(patch) => patch,
            None => self.reconcile_live_interaction(Some(interaction), None)?,
        };
        let mut effects = Vec::new();
        if let Some(update) = update {
            effects.push(AppliedProviderEvent::Update(update));
        }
        effects.push(AppliedProviderEvent::Timeline(patch));
        Ok(AppliedProviderEvent::Effects(effects))
    }

    async fn emit_session_update(
        &mut self,
        update: SessionUpdate,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if update.wakes_input()
            && let Some(wakeup) = &self.input_wakeup
        {
            wakeup.notify_one();
        }
        let payload = publication::encode_session_update(&update)?;
        self.trace
            .record(&self.session.id, "lua.event.emitted", payload.clone());
        if let Some(event_sink) = self.event_sink.as_ref() {
            if let Err(error) = event_sink.send_wait(update).await {
                self.trace.record(
                    &self.session.id,
                    "presentation.delivery_failed",
                    json!({"error":error.to_string()}),
                );
                self.event_sink = None;
                return Err(error.into());
            }
        } else {
            event.push(self.event("provider_event", payload)?);
        }
        Ok(())
    }

    pub(crate) fn presentation(&self) -> Arc<std::sync::Mutex<SessionPresentation>> {
        Arc::clone(&self.presentation)
    }

    /// Reconcile durable state into the next session-local timeline revision.
    fn reconcile_timeline(&mut self) -> Result<TimelinePatch> {
        let timeline = self.snapshot()?.timeline;
        self.presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .reconcile_with_status_thoughts(
                timeline,
                self.observer.status_thoughts.load(Ordering::Relaxed),
            )
    }

    fn reconcile_after_dispatch(&mut self) -> Result<TimelinePatch> {
        if self.timeline_reconciled_after_dispatch {
            self.timeline_reconciled_after_dispatch = false;
            let revision = self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .revision();
            return Ok(TimelinePatch {
                session_id: self.session.id.clone(),
                base_revision: revision,
                revision,
                operation: Vec::new(),
            });
        }
        self.reconcile_timeline()
    }

    fn reconcile_live_interaction(
        &mut self,
        interaction: Option<&Exchange>,
        removed_exchange_id: Option<&str>,
    ) -> Result<TimelinePatch> {
        let status = self.live_interaction_status(interaction)?;
        self.presentation
            .lock()
            .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
            .update_live(
                interaction,
                removed_exchange_id,
                status,
                self.observer.status_thoughts.load(Ordering::Relaxed),
            )
    }

    fn live_interaction_status(
        &self,
        interaction: Option<&Exchange>,
    ) -> Result<crate::session::state_machine::SessionPhase> {
        let runtime = self.store.session_admission(&self.session.id)?.runtime()?;
        let mut status = crate::session::state_machine::SessionPhase::resolve_status(
            runtime.plan.as_ref(),
            self.active_wait_projection.as_ref(),
            interaction,
        );
        if status == crate::session::state_machine::SessionPhase::Idle
            && self.agent_coordinator.active_count() > 0
        {
            status = crate::session::state_machine::SessionPhase::WaitingForAgent {
                agent_count: self.agent_coordinator.active_count(),
            };
        }
        if let Some(task) = &runtime.active_task {
            match &task.workflow {
                crate::task::WorkflowSnapshot::Execute { execution } => {
                    crate::session::state_machine::ExecutionStatusSnapshot::from(&execution.record)
                        .apply(&mut status)
                }
                _ if task.status() == crate::task::TaskStatus::Paused => {
                    status = crate::session::state_machine::SessionPhase::Paused
                }
                _ => {}
            }
        }
        Ok(status)
    }

    #[cfg(test)]
    async fn test_stage_interaction_for_turn(
        &mut self,
        text: &str,
        admit_exchange: bool,
        now_ms: i64,
    ) -> Result<(Exchange, bool, Vec<SessionEvent>)> {
        let (interaction, admitted, mut events, effects) =
            self.stage_interaction_for_turn(text, text, admit_exchange, now_ms, None)?;
        for effect in effects {
            let delivery = self.prepare_provider_delivery(effect)?;
            events.extend(delivery.deliver().await?);
        }
        Ok((interaction, admitted, events))
    }

    /// Admit a turn and stage any previous-exchange presentation work without delivery.
    ///
    /// `session_title` names an unnamed session on its first exchange. Task starts pass their
    /// title rather than the internal instruction that becomes the exchange prompt.
    /// Close an open exchange that no continuation will resume, keeping checkpoint capture owned.
    ///
    /// The outcome follows the provider turns the exchange actually started. A still-running
    /// turn makes it interrupted, and recorded turns complete it. An exchange without any turn
    /// never reached the provider, so it is cancelled rather than reported as completed work.
    /// A captured baseline leaves the exchange finalizing for prior finalization to settle.
    pub(super) fn close_superseded_exchange(
        &mut self,
        exchange: &mut Exchange,
        now_ms: i64,
    ) -> Result<()> {
        let outcome = if exchange
            .turn
            .iter()
            .any(|turn| turn.state() == crate::turn::TurnState::Running)
        {
            crate::exchange::ExchangeOutcome::Interrupted
        } else if exchange.turn.is_empty() {
            crate::exchange::ExchangeOutcome::Cancelled
        } else {
            crate::exchange::ExchangeOutcome::Complete
        };
        if exchange.checkpoint_before.is_some() {
            exchange.begin_finalization(outcome, now_ms)?;
        } else {
            exchange.finish(outcome, now_ms)?;
        }
        self.store.save_exchange(exchange)
    }

    fn stage_interaction_for_turn(
        &mut self,
        text: &str,
        session_title: &str,
        admit_exchange: bool,
        now_ms: i64,
        selected: Option<&crate::provider::ExchangeId>,
    ) -> Result<(Exchange, bool, Vec<SessionEvent>, Vec<AppliedProviderEvent>)> {
        let current = self.store.current_exchange_state(&self.session.id)?;
        if !admit_exchange {
            let mut interaction = match selected {
                Some(selected) => self.store.load_exchange(selected.as_str())?,
                None => self.store.latest_exchange(&self.session.id)?,
            }
            .context("continuation has no originating exchange")?;
            interaction.resume(now_ms)?;
            return Ok((interaction, false, Vec::new(), Vec::new()));
        }
        if current.latest.is_none() && self.session.name.trim().is_empty() {
            let title = automatic_session_name(session_title);
            if !title.is_empty() {
                self.session.name = self
                    .store
                    .initialize_session_name(&self.session.id, &self.client_id, &title)?
                    .name;
                self.save_session()?;
            }
        }
        let event = Vec::new();
        let mut effects = Vec::new();
        if let Some(previous) = current.latest.as_ref()
            && previous.state == ExchangeState::Running
        {
            let mut previous = self
                .store
                .load_exchange(&previous.id)?
                .context("previous running exchange disappeared")?;
            self.close_superseded_exchange(&mut previous, now_ms)?;
            effects.push(
                self.prepare_live_interaction(publication::LiveUpdate::StateChanged, &previous)?,
            );
        }
        Ok((
            Exchange {
                finalization_error: None,
                collection: None,
                check_owner: None,
                output_store: None,
                archived_output: Default::default(),
                input_message_id: None,
                provider_retry: None,
                agent_id: self.session.primary_agent_id.clone(),
                id: Uuid::new_v4().to_string(),
                session_id: self.session.id.clone(),
                ordinal: self.store.next_exchange_ordinal(&self.session.id)?,
                prompt: text.to_owned(),
                origin: crate::exchange::ExchangeOrigin::User,
                approval_mode: Some(self.session.approval_mode),
                phase: crate::exchange::ExchangePhase::Running,
                checkpoint_before: None,
                checkpoint_after: None,
                attributed_diff_text: None,
                checkpoint_diff_text: None,
                attributed_matches_checkpoint: false,
                provider_checkpoint_id: None,
                approval_resolution: Vec::new(),
                host_file_changes: Vec::new(),
                check_setup: None,
                turn: Vec::new(),
                created_at_ms: now_ms,
                tool_counter: Default::default(),
                entry: Vec::new(),
                node_list: Vec::new(),
                metrics: crate::exchange::ExchangeMetrics::default(),
                comment: Vec::new(),
                checklist: None,

                question: crate::exchange::ExchangeQuestion::None,
                clock: crate::exchange::ExchangeClock::Running {
                    elapsed_ms: 0,
                    started_at_ms: now_ms,
                },
            },
            true,
            event,
            effects,
        ))
    }

    fn delete_plan_tests(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let trace = crate::plan::edit_trace::PlanEditTrace::new(
            Arc::clone(&self.trace),
            &self.session.id,
            "session.plan.test.delete",
            &params,
        );
        let mutation = trace.stage("delete.total");
        let plan_id = required_text(&params, "plan_id")?;
        let expected_digest = required_text(&params, "digest")?;
        let expected_version = params
            .get("expected_version")
            .and_then(Value::as_u64)
            .context("test deletion requires the reviewed plan version")?;
        let selected: Vec<crate::plan::DesignTestSelection> = serde_json::from_value(
            params
                .get("tests")
                .cloned()
                .context("test selection is missing")?,
        )?;
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("plan record is missing")?;
        anyhow::ensure!(
            plan.session_id == self.session.id,
            "plan belongs to another session"
        );
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "only plans awaiting review can have tests deleted"
        );
        anyhow::ensure!(
            plan.review_digest.as_deref() == Some(&expected_digest),
            "plan changed before test deletion"
        );
        let (document, rendered, checksum) = self.plan_file.remove_tests(
            &self.session.id,
            &plan.id,
            expected_version,
            &expected_digest,
            &selected,
            Some(&trace),
        )?;
        let records = trace.stage("delete.persist_records");
        plan.document_version = document.version;
        plan.review_digest = Some(checksum);
        plan.review = None;
        plan.manual_changes
            .extend(crate::plan::removed_test_edits(&selected));
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;

        self.save_session()?;
        records.complete(json!({"count":selected.len(), "version":document.version}));
        let payload = json!({ "plan": plan, "content": rendered.markdown, "document": document });
        mutation.complete(json!({"version":document.version}));
        Ok((
            payload.clone(),
            vec![self.event("plan_tests_deleted", payload)?],
        ))
    }

    async fn update_plan_checks(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let result = self.prepare_plan_checks_update(params)?.run().await?;
        self.commit_plan_checks_update(result)
    }

    fn rename_plan_entity(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let trace = crate::plan::edit_trace::PlanEditTrace::new(
            Arc::clone(&self.trace),
            &self.session.id,
            "session.plan.entity.rename",
            &params,
        );
        let mutation = trace.stage("rename.total");
        let preflight = trace.stage("rename.preflight");
        let plan_id = required_text(&params, "plan_id")?;
        let symbol = params.get("symbol").and_then(Value::as_str);
        let entity_name = params
            .get("entity_name")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
        let new_name = required_text(&params, "name")?;
        anyhow::ensure!(!new_name.trim().is_empty(), "entity name cannot be empty");
        let mut plan = self
            .store
            .load_plan(&plan_id)?
            .context("active plan record is missing")?;
        anyhow::ensure!(
            plan.state == PlanState::AwaitingReview,
            "plan does not await review"
        );
        let document = self
            .plan_file
            .read_working_document(&self.session.id, &plan.id)?;
        if let Some(expected_version) = params.get("expected_version").and_then(Value::as_u64) {
            anyhow::ensure!(
                document.version == expected_version,
                "plan version changed before entity rename"
            );
        }
        if let Some(expected) = params.get("digest").and_then(Value::as_str) {
            anyhow::ensure!(
                plan.review_digest.as_deref() == Some(expected),
                "plan changed before rename"
            );
            anyhow::ensure!(
                crate::plan::digest(&serde_json::to_vec(&document)?) == expected,
                "working plan changed before rename"
            );
        }
        preflight.complete(json!({"version":document.version}));
        let (previous_name, document, rendered, digest) = if let Some(identity) = symbol {
            anyhow::ensure!(
                params.get("expected_version").and_then(Value::as_u64) == Some(document.version),
                "rename requires the current plan version"
            );
            self.plan_file.rename_symbol(
                &self.session.id,
                &plan.id,
                document.version,
                identity,
                &new_name,
                Some(&trace),
            )?
        } else {
            let previous = document
                .entity_changes
                .iter()
                .find(|entity| entity.name == entity_name)
                .map(|entity| entity.name.clone())
                .with_context(|| format!("program entity `{entity_name}` does not exist"))?;
            let result = self.plan_file.rename_added_entity(
                &self.session.id,
                &plan.id,
                &entity_name,
                new_name.clone(),
            )?;
            let rendered = self.plan_file.render_review(&result.document)?;
            let digest = crate::plan::digest(&crate::codec::to_vec(&result.document)?);
            (previous, result.document, rendered, digest)
        };
        let records = trace.stage("rename.persist_records");
        let version = document.version;
        plan.document_version = version;
        plan.title.clone_from(&document.title);
        plan.review_digest = Some(digest);
        plan.review = None;
        plan.manual_changes
            .push(forge_provider_api::forge::PlanEdit::Rename {
                from: previous_name.to_string(),
                to: new_name.to_string(),
            });
        plan.updated_at_ms = self.clock.now_ms();
        self.publish_plan(&plan)?;

        self.save_session()?;
        records.complete(json!({"version":version}));
        let payload = json!({
            "plan": &plan,
            "content": &rendered.markdown,
            "document": &document,
            "previous_name": previous_name,
            "name": new_name,
        });
        mutation.complete(json!({"version":version}));
        Ok((
            payload.clone(),
            vec![self.event("plan_entity_renamed", payload)?],
        ))
    }

    async fn continue_plan_review(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let plan = self.active_plan_review()?;
        let acceptance = plan
            .review
            .as_ref()
            .context("active plan has no acceptance state")?;
        let approval_mode = acceptance.approval_mode()?;
        self.dispatch_session_command(
            "session.plan.accept",
            json!({
                "plan_id": plan.id,
                "digest": acceptance.review_digest,
                "saved_source_digest": acceptance.saved_source_digest,
                "approval_mode": approval_mode,
                "comment": acceptance.overall_comment,
            }),
        )
    }

    fn apply_goal_evidence_strict(
        &mut self,
        evidence: crate::goal::TurnEvidence,
        permit: Option<crate::task::TaskPermit>,
        completed_turn: crate::task::TaskTurnId,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let Some(permit) = permit else {
            return Ok(false);
        };
        let task = self
            .task_controller
            .current()?
            .ok_or(crate::task::StaleTask)?;
        if task.kind() == crate::task::TaskKind::Plan {
            return Err(crate::task::StaleTask.into());
        }
        let observed = self.task_controller.observe_goal(
            &mut self.store,
            &permit,
            completed_turn.clone(),
            evidence,
            self.clock.now_ms(),
        );
        let (observed, decision) = observed?;
        let goal = GoalRecord {
            id: task.id.as_str().into(),
            session_id: self.session.id.clone(),
            objective: observed.objective,
            state: observed.state,
            continuation: observed.continuation,
            created_at_ms: task.created_at_ms,
            updated_at_ms: self.clock.now_ms(),
        };
        let continuing = match decision {
            ContinuationDecision::Continue | ContinuationDecision::RetryNoProgress => {
                if task.kind() == crate::task::TaskKind::Execute {
                    self.reserve_execution_continuation(completed_turn)?;
                }
                self.trace.record(
                    &self.session.id,
                    "task.continuation_reserved",
                    json!({"revision":permit.revision()}),
                );
                event.push(self.event(
                    if task.kind() == crate::task::TaskKind::Goal {
                        "goal_changed"
                    } else {
                        "goal_continue_requested"
                    },
                    serde_json::to_value(&goal)?,
                )?);
                true
            }
            ContinuationDecision::Complete
            | ContinuationDecision::Blocked
            | ContinuationDecision::Stalled => {
                event.push(self.event("goal_changed", serde_json::to_value(&goal)?)?);
                false
            }
            ContinuationDecision::Stop => return Ok(false),
        };
        Ok(continuing)
    }

    fn active_goal(&self) -> Result<GoalRecord> {
        self.current_goal()?.context("no active goal")
    }

    fn current_goal(&self) -> Result<Option<GoalRecord>> {
        if let Some(task) = self.task_controller.current()? {
            let (objective, state, continuation) = match &task.workflow {
                crate::task::WorkflowSnapshot::Goal { goal } => (
                    goal.objective.clone(),
                    goal.state,
                    goal.continuation.clone(),
                ),
                crate::task::WorkflowSnapshot::Execute { execution } => (
                    execution.title.clone(),
                    match execution.record.state {
                        PlanExecutionState::Active => GoalState::Active,
                        PlanExecutionState::Paused => GoalState::Paused,
                        PlanExecutionState::Complete => GoalState::Complete,
                        PlanExecutionState::Cancelled => GoalState::Cleared,
                        PlanExecutionState::Stalled => GoalState::Stalled,
                        PlanExecutionState::Blocked => GoalState::Blocked,
                    },
                    execution.continuation.clone(),
                ),
                crate::task::WorkflowSnapshot::Plan { .. } => return Ok(None),
            };
            return Ok(Some(GoalRecord {
                id: task.id.as_str().into(),
                session_id: self.session.id.clone(),
                objective,
                state,
                continuation,
                created_at_ms: task.created_at_ms,
                updated_at_ms: task.updated_at_ms,
            }));
        }
        Ok(None)
    }

    /// Capture the retained preview selected by a transport request.
    pub(crate) fn rollback_token(
        &self,
        preview_id: &str,
    ) -> std::result::Result<
        crate::exchange::rollback::PreparedRollbackToken,
        crate::exchange::rollback::RollbackFailure,
    > {
        let preview = crate::checkpoint::RestorePreview::load(
            &self.store.objects,
            &self.session.id,
            preview_id,
        )
        .map_err(|_| crate::exchange::rollback::RollbackFailure::AuthorityUnavailable)?;
        let mut token = crate::exchange::rollback::PreparedRollbackToken::capture(&preview)?;
        if let Some(original) = &preview.recovery_of {
            let revision = self.restore_revision()?;
            if &revision.preview_id != original {
                return Err(crate::exchange::rollback::RollbackFailure::StaleAuthority);
            }
            if let crate::exchange::rollback::PreparedRollbackToken::Workspace {
                recovery, ..
            } = &mut token
            {
                *recovery = Some(revision);
            }
        }
        Ok(token)
    }

    /// Capture interrupted restore authority for a transport recovery request.
    pub(crate) fn restore_revision(
        &self,
    ) -> std::result::Result<
        crate::exchange::rollback::RestoreRevision,
        crate::exchange::rollback::RollbackFailure,
    > {
        let journal =
            crate::checkpoint::RestoreJournal::load(&self.store.objects, &self.session.id)
                .map_err(|_| crate::exchange::rollback::RollbackFailure::AuthorityUnavailable)?
                .ok_or(crate::exchange::rollback::RollbackFailure::JournalMissing)?;
        crate::exchange::rollback::RestoreRevision::capture(&journal)
    }

    /// Capture file and history authority before a rollback enters the FIFO.
    pub(crate) fn capture_rollback(
        &self,
        command: &crate::exchange::rollback::RollbackCommand,
    ) -> std::result::Result<
        crate::exchange::rollback::RollbackCommand,
        crate::exchange::rollback::RollbackFailure,
    > {
        use crate::exchange::rollback::{PreparedRollbackToken, RollbackCommand, RollbackFailure};
        let mut captured = command.clone();
        match &mut captured {
            RollbackCommand::Prepare(selection)
            | RollbackCommand::Apply(PreparedRollbackToken::Conversation { selection }) => {
                let exchanges = self
                    .store
                    .list_exchange(&self.session.id)
                    .map_err(|_| RollbackFailure::AuthorityUnavailable)?;
                if !exchanges
                    .last()
                    .is_some_and(|exchange| exchange.id == selection.latest_exchange_id)
                {
                    return Err(RollbackFailure::StaleAuthority);
                }
                selection.capture_history(&exchanges)?;
            }
            RollbackCommand::Apply(PreparedRollbackToken::Workspace {
                preview_id,
                digest,
                recovery,
            }) => {
                let preview = crate::checkpoint::RestorePreview::load(
                    &self.store.objects,
                    &self.session.id,
                    preview_id,
                )
                .map_err(|_| RollbackFailure::AuthorityUnavailable)?;
                PreparedRollbackToken::Workspace {
                    preview_id: preview_id.clone(),
                    digest: digest.clone(),
                    recovery: recovery.clone(),
                }
                .validate(&preview)?;
                if let Some(revision) = recovery {
                    let journal = crate::checkpoint::RestoreJournal::load(
                        &self.store.objects,
                        &self.session.id,
                    )
                    .map_err(|_| RollbackFailure::AuthorityUnavailable)?
                    .ok_or(RollbackFailure::JournalMissing)?;
                    revision.validate(&journal)?;
                } else if preview.recovery_of.is_some() {
                    return Err(RollbackFailure::StaleAuthority);
                }
            }
            RollbackCommand::ResumeRestore(revision)
            | RollbackCommand::PrepareUndoRestore(revision) => {
                let journal =
                    crate::checkpoint::RestoreJournal::load(&self.store.objects, &self.session.id)
                        .map_err(|_| RollbackFailure::AuthorityUnavailable)?
                        .ok_or(RollbackFailure::JournalMissing)?;
                revision.validate(&journal)?;
            }
        }
        Ok(captured)
    }

    async fn prepare_rollback(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before workspace rollback"
        );
        let exchange_id = required_text(&params, "exchange_id")?;
        let exchange_id = self
            .store
            .checkpoint_owner(&self.session.id, &exchange_id)?;
        let interaction = self.store.list_exchange(&self.session.id)?;
        let target_index = interaction
            .iter()
            .position(|item| item.id == exchange_id)
            .context("interaction not found")?;
        anyhow::ensure!(
            interaction[target_index].disposition() == crate::exchange::HistoryDisposition::Current,
            "selected exchange no longer belongs to the current workspace history"
        );
        anyhow::ensure!(
            interaction
                .iter()
                .skip(target_index)
                .all(|exchange| exchange.is_finished()),
            "rollback requires settled exchanges"
        );
        self.store.require_settled_exchange_tree(
            &self.session.id,
            &interaction
                .iter()
                .skip(target_index)
                .map(|exchange| exchange.id.clone())
                .collect::<Vec<_>>(),
        )?;
        let expected_id = interaction
            .iter()
            .rev()
            .find(|item| item.disposition() != crate::exchange::HistoryDisposition::Superseded)
            .and_then(|item| match item.disposition() {
                crate::exchange::HistoryDisposition::Current => item.checkpoint_after.as_deref(),
                crate::exchange::HistoryDisposition::RolledBack => {
                    item.checkpoint_before.as_deref()
                }
                crate::exchange::HistoryDisposition::Superseded => None,
            })
            .context("latest interaction has no checkpoint")?;
        let target_id = interaction[target_index]
            .checkpoint_before
            .as_deref()
            .context("selected interaction has no checkpoint")?;
        let expected = self
            .store
            .load_checkpoint(expected_id)?
            .context("latest checkpoint is missing")?;
        let target = self
            .store
            .load_checkpoint(target_id)?
            .context("target checkpoint is missing")?;
        let WorkspaceKind::Git(workspace) = &self.workspace_kind else {
            anyhow::bail!("rollback is unavailable because this session has NO CHECKPOINT")
        };
        self.require_restore_recovered()?;
        let objects = self.store.objects.clone();
        let workspace = workspace.to_owned();
        let exchange_ids = interaction
            .iter()
            .skip(target_index)
            .map(|exchange| exchange.id.clone())
            .collect();
        let preview = tokio::task::spawn_blocking(move || {
            let mut preview = crate::checkpoint::RestorePreview::prepare(
                &objects, &workspace, &expected, &target,
            )?;
            preview.exchange_ids = exchange_ids;
            preview.save(&objects)?;
            Ok::<_, anyhow::Error>(preview)
        })
        .await
        .context("restore preparation worker failed")??;
        Ok((preview.summary(0), Vec::new()))
    }

    fn require_restore_recovered(&self) -> Result<()> {
        if let Some(progress) =
            crate::checkpoint::RestoreJournal::progress(&self.store.objects, &self.session.id)?
        {
            anyhow::ensure!(
                progress.state == crate::checkpoint::RestoreState::Complete,
                "an interrupted restore requires recovery before new workspace work; open Harness undo"
            );
        }
        Ok(())
    }

    async fn apply_rollback(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        match params
            .get("scope")
            .and_then(Value::as_str)
            .unwrap_or("workspace")
        {
            "conversation" => return self.rollback_conversation(params).await,
            "workspace" => {}
            _ => anyhow::bail!("unknown rollback scope"),
        }
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before workspace rollback"
        );
        let preview_id = required_text(&params, "preview_id")?;
        let objects = self.store.objects.clone();
        let session_id = self.session.id.clone();
        let preview = tokio::task::spawn_blocking(move || {
            crate::checkpoint::RestorePreview::load(&objects, &session_id, &preview_id)
        })
        .await
        .context("restore preview read failed")??;
        anyhow::ensure!(
            crate::workspace::same(&preview.workspace, &self.session.workspace),
            "restore preview belongs to another workspace"
        );
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges.iter().all(|exchange| exchange.is_finished()),
            "cancel or finish the active exchange before restoring files"
        );
        if let Some(first) = preview.exchange_ids.first() {
            let position = exchanges
                .iter()
                .position(|exchange| &exchange.id == first)
                .context("restore exchange disappeared")?;
            anyhow::ensure!(
                exchanges[position..]
                    .iter()
                    .map(|exchange| &exchange.id)
                    .eq(preview.exchange_ids.iter())
                    && exchanges[position].disposition()
                        == crate::exchange::HistoryDisposition::Current,
                "restore preview is stale: exchange history changed"
            );
            self.native_rewind(&exchanges, position)?;
            self.store
                .require_settled_exchange_tree(&self.session.id, &preview.exchange_ids)?;
        }
        let journal = GitCheckpoint::new(&preview.workspace)
            .apply_preview(
                Arc::clone(&self.repositories),
                self.store.objects.clone(),
                preview,
                false,
            )
            .await?;
        self.finish_restore(journal).await
    }

    async fn finish_restore(
        &mut self,
        mut journal: crate::checkpoint::RestoreJournal,
    ) -> Result<(Value, Vec<SessionEvent>)> {
        // A host exit after the database commit leaves FilesComplete evidence.
        // Reconcile that commit rather than mutating the task or provider twice.
        let history = self.store.list_exchange(&self.session.id)?;
        let committed = !journal.preview.exchange_ids.is_empty()
            && journal
                .preview
                .exchange_ids
                .iter()
                .enumerate()
                .all(|(index, id)| {
                    let expected = if index == 0 {
                        crate::exchange::HistoryDisposition::RolledBack
                    } else {
                        crate::exchange::HistoryDisposition::Superseded
                    };
                    history
                        .iter()
                        .any(|exchange| &exchange.id == id && exchange.disposition() == expected)
                });
        let events = if committed {
            Vec::new()
        } else {
            self.finish_conversation_rollback(&journal.preview.exchange_ids)
                .await?
        };
        journal.state = crate::checkpoint::RestoreState::Complete;
        journal.save(&self.store.objects)?;
        Ok((json!({"restored":true}), events))
    }

    async fn rollback_conversation(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        anyhow::ensure!(
            self.agent_coordinator.active_count() == 0,
            "child execution must settle before conversation rollback"
        );
        self.require_restore_recovered()?;
        let requested = required_text(&params, "exchange_id")?;
        let target = self.store.checkpoint_owner(&self.session.id, &requested)?;
        let expected_latest = required_text(&params, "latest_exchange_id")?;
        let exchanges = self.store.list_exchange(&self.session.id)?;
        anyhow::ensure!(
            exchanges
                .last()
                .is_some_and(|exchange| exchange.id == expected_latest),
            "conversation changed; select the rollback interaction again"
        );
        let position = exchanges
            .iter()
            .position(|exchange| exchange.id == target)
            .context("rollback interaction disappeared")?;
        anyhow::ensure!(
            exchanges[position].disposition() == crate::exchange::HistoryDisposition::Current,
            "selected interaction has already been rolled back"
        );
        anyhow::ensure!(
            exchanges.iter().all(|exchange| exchange.is_finished()),
            "cancel or finish active work before conversation rollback"
        );
        let ids = exchanges[position..]
            .iter()
            .map(|exchange| exchange.id.clone())
            .collect::<Vec<_>>();
        self.store
            .require_settled_exchange_tree(&self.session.id, &ids)?;
        let events = self.finish_conversation_rollback(&ids).await?;
        Ok((json!({"restored":true,"scope":"conversation"}), events))
    }

    async fn finish_conversation_rollback(
        &mut self,
        exchange_ids: &[String],
    ) -> Result<Vec<SessionEvent>> {
        let Some(first) = exchange_ids.first() else {
            return Ok(Vec::new());
        };
        let exchanges = self.store.list_exchange(&self.session.id)?;
        let position = exchanges
            .iter()
            .position(|exchange| &exchange.id == first)
            .context("rollback interaction disappeared")?;
        let native = self.native_rewind(&exchanges, position)?;
        if let Some(native) = &native {
            self.provider
                .thread_rewind(native.request.clone(), native.after.clone())
                .await
                .map_err(|error| anyhow::anyhow!("provider conversation rewind failed: {error}"))?;
        }
        let mut session = self.session.clone();
        if let Some(task) = self.task_controller.current()? {
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                crate::task::TaskChange::Clear,
                self.clock.now_ms(),
            )?;
            session.active_task = None;
        }
        self.apply_rewound_conversation(&mut session, native.as_ref())?;
        self.store.record_rollback(&session, exchange_ids)?;
        self.session = session;
        self.context_usage_fresh = false;
        self.rolled_back_events(first)
    }

    /// Announce a committed conversation rollback together with the transcript
    /// patch that removes the rolled-back exchanges. Lua treats
    /// `interaction_rolled_back` as a state refresh, which never rewrites the
    /// transcript document, so the patch must travel with the event.
    pub(super) fn rolled_back_events(&mut self, first: &str) -> Result<Vec<SessionEvent>> {
        let exchange = self
            .store
            .load_exchange(first)?
            .filter(|exchange| exchange.session_id == self.session.id)
            .context("rolled-back interaction disappeared")?;
        let mut events =
            vec![self.event("interaction_rolled_back", serde_json::to_value(exchange)?)?];
        let patch = self.reconcile_timeline()?;
        if !patch.is_empty() {
            events.push(self.event("document_changed", patch.notification())?);
        }
        Ok(events)
    }

    async fn restore_recovery(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let action = params
            .get("action")
            .and_then(Value::as_str)
            .unwrap_or("status");
        if action == "status" {
            let progress =
                crate::checkpoint::RestoreJournal::progress(&self.store.objects, &self.session.id)?;
            return Ok((progress.map(|progress| json!({"pending": progress.state != crate::checkpoint::RestoreState::Complete,
                "applied": progress.applied, "files": progress.files})).unwrap_or(Value::Null), Vec::new()));
        }
        enum RecoveryAction {
            Continue,
            Undo,
        }
        let action = match action {
            "continue" => RecoveryAction::Continue,
            "undo" => RecoveryAction::Undo,
            _ => anyhow::bail!("unknown restore recovery action"),
        };
        let objects = self.store.objects.clone();
        let session_id = self.session.id.clone();
        let journal = tokio::task::spawn_blocking(move || {
            crate::checkpoint::RestoreJournal::load(&objects, &session_id)
        })
        .await
        .context("restore journal read failed")??
        .context("restore journal is missing")?;
        match action {
            RecoveryAction::Continue => {
                anyhow::ensure!(
                    journal.state != crate::checkpoint::RestoreState::Complete,
                    "restore is already complete"
                );
                let exchange = self.store.list_exchange(&self.session.id)?;
                anyhow::ensure!(
                    exchange.iter().all(|exchange| exchange.is_finished()),
                    "cancel or finish active work before restore recovery"
                );
                if let Some(position) = journal.preview.exchange_ids.first().and_then(|first| {
                    exchange.iter().position(|candidate| {
                        &candidate.id == first
                            && candidate.disposition()
                                == crate::exchange::HistoryDisposition::Current
                    })
                }) {
                    self.native_rewind(&exchange, position)?;
                }
                self.store.require_settled_exchange_tree(
                    &self.session.id,
                    &exchange
                        .iter()
                        .map(|exchange| exchange.id.clone())
                        .collect::<Vec<_>>(),
                )?;
                let journal = GitCheckpoint::new(&journal.preview.workspace)
                    .apply_preview(
                        Arc::clone(&self.repositories),
                        self.store.objects.clone(),
                        journal.preview,
                        true,
                    )
                    .await?;
                return self.finish_restore(journal).await;
            }
            RecoveryAction::Undo => {
                let objects = self.store.objects.clone();
                let preview = tokio::task::spawn_blocking(move || {
                    let preview = journal.reverse(&objects)?;
                    preview.save(&objects)?;
                    Ok::<_, anyhow::Error>(preview)
                })
                .await
                .context("recovery preparation worker failed")??;
                Ok((preview.summary(0), Vec::new()))
            }
        }
    }
    async fn new_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        self.new_session_now(params)
    }

    fn new_session_now(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let now_ms = self.clock.now_ms();
        let child = prepare_new_session(
            &self.data_root,
            &self.client_id,
            self.provider_launch.kind.as_str(),
            self.capability.native_compact,
            &self.session.id,
            &params,
            now_ms,
        )?;
        let snapshot = self.snapshot_for_session(child.clone())?;
        Ok((
            serde_json::to_value(snapshot)?,
            vec![SessionEvent {
                session_id: child.id.clone(),
                event: "session_created".into(),
                payload: serde_json::to_value(child)?,
            }],
        ))
    }

    async fn resume_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let session_id = required_text(&params, "session_id")?;
        let mut session =
            SessionNotFound::require(self.store.load_session(&session_id)?, &session_id)?;
        anyhow::ensure!(
            crate::workspace::same(&session.workspace, &self.session.workspace),
            "cross-worktree resume requires native fork"
        );
        anyhow::ensure!(
            session.provider == self.provider_launch.kind.as_str(),
            "session uses a different configured provider"
        );
        if session.id == self.session.id {
            return Ok((serde_json::to_value(self.snapshot()?)?, Vec::new()));
        }
        self.pause_current_goal().await?;
        session = self
            .store
            .acquire_session_lease(&session.id, &self.client_id)?;
        self.release_lease()?;
        if session.provider_label.is_empty() {
            session.provider_label = self.provider.descriptor().label;
        }
        self.store.save_session(&session)?;
        self.exchange_runtime = None;
        self.store.interrupt_detached_execution(&session.id)?;
        self.session = session;
        self.agent_registry = load_agent_registry(&self.store, &self.session.id)?;
        self.child_exchange_runtime_by_agent.clear();
        self.capability.native_fork = self.session.native_fork;
        self.capability.native_compact = self.session.native_compact;
        self.capability = self.provider.descriptor().capability;
        self.capability.native_fork = self.session.native_fork || self.capability.native_fork;
        self.capability.native_compact =
            self.session.native_compact || self.capability.native_compact;
        Ok((
            serde_json::to_value(self.snapshot()?)?,
            vec![self.event("session_changed", json!({"snapshot":self.snapshot()?}))?],
        ))
    }

    async fn pause_current_goal(&mut self) -> Result<()> {
        let Some(goal) = self.current_goal()? else {
            return Ok(());
        };
        if goal.state != GoalState::Active {
            return Ok(());
        }
        let task = self
            .task_controller
            .current()?
            .context("task was cleared while pausing")?;
        self.task_controller.change(
            &mut self.store,
            Some(&task.revision),
            crate::task::TaskChange::Pause,
            self.clock.now_ms(),
        )?;
        Ok(())
    }

    async fn configuration_progress(&mut self, state: ConfigurationProgressState) -> Result<()> {
        let Some(operation_id) = self.active_operation_id.clone() else {
            return Ok(());
        };
        self.emit_session_update(
            SessionUpdate::ConfigurationProgress {
                operation_id: operation_id.to_string(),
                state,
            },
            &mut Vec::new(),
        )
        .await
    }

    async fn configure_session(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let preparation = self.prepare_configuration(params)?;
        let models = if preparation.needs_catalog(self) {
            let result = self.prepare_configuration_catalog()?.run().await?;
            self.commit_configuration_catalog(result)?
        } else {
            Vec::new()
        };
        let mut candidate = self.build_configuration_candidate(preparation, &models)?;
        if let Some(job) = self.prepare_candidate_reconcile(&candidate)? {
            let result = job.run().await?;
            self.certify_candidate_reconcile(&mut candidate, result)?;
        }
        let committed = self.commit_configuration_candidate(candidate)?;
        Ok((committed.value, committed.events))
    }

    /// Resolve persisted ownership across primary and child-agent exchanges.
    fn compaction_exchange_owner(
        &self,
        compaction: &crate::provider::Compaction,
    ) -> Result<Option<Exchange>> {
        self.store
            .load_compaction_exchange(&self.session.id, compaction)
    }

    /// Merge active and background notifications into their persisted timeline owner.
    fn retain_external_compaction(
        &mut self,
        mut compaction: crate::provider::Compaction,
        now_ms: i64,
    ) -> Result<()> {
        if compaction.state == crate::provider::CompactionState::Running
            && compaction.thread_id
                == crate::provider::ThreadOwner::root(&self.session.id)
                    .thread_id()
                    .as_str()
        {
            compaction.result.context_tokens_before = compaction
                .result
                .context_tokens_before
                .or_else(|| self.session.context_usage.as_ref().map(|usage| usage.used));
        }
        self.save_session()?;
        if let Some(mut exchange) = self.compaction_exchange_owner(&compaction)? {
            exchange.observe_compaction(compaction, now_ms);
            self.store.save_exchange(&exchange)?;
        } else {
            let existing = self
                .store
                .list_session_event(&self.session.id)?
                .into_iter()
                .find(|record| {
                    matches!(&record.detail, SessionEventKind::Compaction { compaction: existing }
                    if existing.id == compaction.id && existing.thread_id == compaction.thread_id)
                });
            let record = if let Some(mut record) = existing {
                if let SessionEventKind::Compaction {
                    compaction: existing,
                } = &mut record.detail
                {
                    existing.observe(compaction, now_ms);
                }
                record
            } else {
                SessionEventRecord {
                    id: format!(
                        "{}:compaction:{}:{}",
                        self.session.id, compaction.thread_id, compaction.id
                    ),
                    session_id: self.session.id.clone(),
                    owner: crate::timeline::TimelineOwner::session(&self.session.id),
                    created_at_ms: now_ms,
                    detail: SessionEventKind::Compaction {
                        compaction: (compaction.observed(now_ms)).into(),
                    },
                }
            };
            self.store.save_session_event(&record)?;
        }
        Ok(())
    }

    /// Deliver queued session observations in provider order under broker ownership.
    pub(crate) fn drain_session_events(&mut self) -> Result<Vec<SessionEvent>> {
        let mut updates = std::mem::take(&mut self.settlement_events);
        for observation in self.observer.session_events().drain()? {
            updates.extend(self.observe_session_event(observation)?);
        }
        Ok(updates)
    }

    /// Retain session notifications after provider execution has released the broker.
    pub(crate) fn observe_session_event(
        &mut self,
        observation: OwnerObservation,
    ) -> Result<Vec<SessionEvent>> {
        let observation = match observation {
            OwnerObservation::HostFileChange {
                binding_id,
                scope,
                change,
            } => {
                let effect = self.apply_observed_file_change(&binding_id, &scope, &change)?;
                return Ok(self.prepare_provider_delivery(effect)?.events);
            }
            OwnerObservation::Event(event) => event,
            OwnerObservation::InputAcceptance {
                scope,
                receipt,
                presentation,
            } => {
                let mut interaction = self
                    .store
                    .load_exchange(scope.exchange_id.as_str())?
                    .context("accepted receipt has no original exchange")?;
                self.apply_durable_input_acceptance(
                    &mut interaction,
                    &scope,
                    &receipt,
                    &presentation,
                )?;
                let patch = self.reconcile_timeline()?;
                return Ok(if patch.is_empty() {
                    Vec::new()
                } else {
                    vec![self.event("document_changed", patch.notification())?]
                });
            }
        };

        anyhow::ensure!(
            observation.scope().owner().session_id().as_str() == self.session.id,
            "session observation crosses original ownership"
        );
        if observation.scope().owner() != crate::provider::ThreadOwner::root(&self.session.id)
            || matches!(
                &observation,
                HarnessEvent::Native {
                    event: boxed_native_event,
                    ..
                }
             if matches!(boxed_native_event.as_ref(), ProviderEvent::Agent { .. }))
        {
            return self.observe_child_session_event(&observation);
        }
        if matches!(
           &observation,
           HarnessEvent::Native {
               event: boxed_native_event,
               ..
           }
        if matches!(boxed_native_event.as_ref(), ProviderEvent::Provider {
                   event: forge_provider_api::ProviderConnectionEvent::Unavailable(_)
               }))
        {
            return self.observe_child_session_event(&observation);
        }
        if let HarnessEvent::Host {
            scope,
            event:
                HostEvent::CapabilitiesNegotiated {
                    binding_id,
                    manifest,
                    history,
                },
        } = &observation
        {
            self.apply_negotiated_capabilities(scope, binding_id, manifest, history.as_ref())?;
            return Ok(Vec::new());
        }
        let now_ms = observation.observed_at_ms(self.clock.now_ms());
        if let HarnessEvent::Native {
            source,
            scope,
            event,
        } = &observation
        {
            let event = event.as_ref();
            match event {
                ProviderEvent::Provider {
                    event:
                        forge_provider_api::ProviderConnectionEvent::CapabilitiesChanged(manifest),
                } => {
                    if self.provider.binding_source_status(
                        &source.binding_id,
                        &scope.owner(),
                        None,
                    )? == crate::provider::NativeBindingStatus::Active
                    {
                        self.apply_effective_capabilities(manifest.clone())?;
                    }
                    return Ok(Vec::new());
                }
                ProviderEvent::Thread {
                    thread_id,
                    event: forge_provider_api::ThreadEvent::ContextUsage(usage),
                } => {
                    if self.provider.binding_source_status(
                        &source.binding_id,
                        &scope.owner(),
                        Some(thread_id),
                    )? == crate::provider::NativeBindingStatus::RetiredKnown
                    {
                        return Ok(Vec::new());
                    }
                    self.session.context_usage = crate::session::ContextUsage::from_report(usage);
                    self.context_usage_fresh = true;
                    self.save_session()?;
                }
                ProviderEvent::Thread {
                    thread_id,
                    event: forge_provider_api::ThreadEvent::IdentityResolved(id),
                } => {
                    if self.provider.binding_source_status(
                        &source.binding_id,
                        &scope.owner(),
                        Some(thread_id),
                    )? == crate::provider::NativeBindingStatus::RetiredKnown
                    {
                        return Ok(Vec::new());
                    }
                    self.retain_session_identity(&scope.owner(), id)?;
                    return Ok(Vec::new());
                }
                ProviderEvent::Thread {
                    event: forge_provider_api::ThreadEvent::CompactionUpdated(compaction),
                    ..
                } => {
                    self.retain_external_compaction((**compaction).clone(), now_ms)?;
                    let patch = self.reconcile_timeline()?;
                    return Ok(if patch.is_empty() {
                        Vec::new()
                    } else {
                        vec![self.event("document_changed", patch.notification())?]
                    });
                }
                ProviderEvent::Provider { .. }
                | ProviderEvent::Thread { .. }
                | ProviderEvent::Turn { .. }
                | ProviderEvent::Item { .. }
                | ProviderEvent::Agent { .. } => {}
            }
        }
        if let Some(invocation) = observation.scope().invocation()
            && let Some(mut owner) = self.store.load_exchange(invocation.exchange_id.as_str())?
        {
            let changed = match &observation {
                HarnessEvent::Native { scope, event, .. } => {
                    let event = event.as_ref();
                    owner.observe_provider_event(scope, event, now_ms)?
                }
                HarnessEvent::Host { scope, event } => {
                    owner.observe_host_event(scope, event, now_ms)?
                }
            };
            if changed {
                self.store.save_exchange(&owner)?;
                let mut updates = Vec::new();
                if let HarnessEvent::Native {
                    source,
                    scope,
                    event: boxed_native_event,
                } = &observation
                    && let ProviderEvent::Turn {
                        thread_id,
                        turn_id,
                        event: forge_provider_api::TurnEvent::ModelResolved(_),
                    } = boxed_native_event.as_ref()
                {
                    let current_root =
                        self.store
                            .latest_exchange(&self.session.id)?
                            .is_some_and(|current| {
                                current.id == owner.id
                                    && current.turn.last().is_some_and(|turn| {
                                        &turn.provider().scope == invocation
                                            && &turn.provider().thread_id == thread_id
                                            && &turn.provider().turn_id == turn_id
                                    })
                            });
                    let current_binding = self
                        .provider
                        .binding_source_status(&source.binding_id, &scope.owner(), Some(thread_id))
                        .is_ok_and(|status| status == crate::provider::NativeBindingStatus::Active);
                    if current_root && current_binding {
                        if let Some(model) =
                            owner.turn.last().and_then(|turn| turn.resolved_model())
                        {
                            self.session.resolved_model = Some(model.model_id.as_str().to_owned());
                        }
                        self.save_session()?;
                        updates.push(self.event(
                            "provider_event",
                            publication::encode_session_update(&SessionUpdate::Runtime {
                                session_id: self.session.id.clone(),
                                runtime: crate::provider::ProviderRuntime {
                                    provider: self.session.provider.clone(),
                                    model: self.session.resolved_model.clone(),
                                },
                            })?,
                        )?);
                    }
                }
                let patch = self.reconcile_timeline()?;
                if !patch.is_empty() {
                    updates.push(self.event("document_changed", patch.notification())?);
                }
                if !updates.is_empty() {
                    return Ok(updates);
                }
            }
        }
        Ok(match SessionUpdate::from_observation(&observation) {
            Some(update) => vec![self.event(
                "provider_event",
                publication::encode_session_update(&update)?,
            )?],
            None => Vec::new(),
        })
    }

    /// Preserve the local driver's compatibility entry while sharing owned compaction settlement.
    async fn compact_session(&mut self, manual: bool) -> Result<(Value, Vec<SessionEvent>)> {
        // The first input opens the native conversation, so compacting an unused chat would
        // open an empty provider conversation solely to compact it.
        anyhow::ensure!(
            self.session.provider_session_id.is_some()
                || !self.store.list_exchange(&self.session.id)?.is_empty(),
            "Nothing to compact: this conversation has no provider history yet"
        );
        let registry = self.execution_registry();
        registry
            .owner(crate::provider::ThreadOwner::root(&self.session.id))
            .await?;
        let provenance = if manual {
            compaction::CompactionProvenance::Manual
        } else {
            compaction::CompactionProvenance::Automatic {
                turn_id: self
                    .session
                    .last_compaction_turn
                    .clone()
                    .context("automatic compaction requires its authorizing turn")?,
            }
        };
        let (job, started) = self.prepare_owned_compaction(provenance)?;
        let mut events = started.deliver_and_flush().await?;
        let observer = self.observer.session_events();
        let work = job.run();
        tokio::pin!(work);
        let result = loop {
            events.extend(self.drain_session_events()?);
            tokio::select! {
                result = &mut work => break result,
                ready = observer.notified() => ready?,
            }
        };
        events.extend(self.drain_session_events()?);
        let (completed, failure, _) = self.commit_owned_compaction(result)?;
        events.extend(completed.deliver_and_flush().await?);
        if let Some(error) = failure {
            return Err(error);
        }
        Ok((serde_json::to_value(self.snapshot()?)?, events))
    }

    fn snapshot_for_session(&mut self, session: HarnessSession) -> Result<BrokerSnapshot> {
        let parent_session = std::mem::replace(&mut self.session, session);
        let child_workspace = crate::workspace::resolve(Path::new(&self.session.workspace))?;
        let parent_workspace = std::mem::replace(&mut self.workspace_kind, child_workspace);
        let child_registry = load_agent_registry(&self.store, &self.session.id)?;
        let parent_registry = std::mem::replace(&mut self.agent_registry, child_registry);
        let parent_runtime = self.exchange_runtime.take();
        let snapshot = self.snapshot();
        self.session = parent_session;
        self.workspace_kind = parent_workspace;
        self.agent_registry = parent_registry;
        self.exchange_runtime = parent_runtime;
        snapshot
    }

    /// Bind the current foreground's input decorations and tool context to an invocation snapshot.
    pub(crate) fn provider_request(
        &self,
        input: ProviderInput,
        mode: PromptMode,
    ) -> Result<ProviderRequest> {
        let mut request = self.input_submission_request(input, mode)?;
        if !self.submitted_attachments.is_empty() {
            request.input = request
                .input
                .with_attachments(self.submitted_attachments.clone());
        }
        if self.question_discussion {
            request.access.write_access = crate::session::WriteScope::Off;
            request.access.writable_directory.clear();
        }
        request.control_context = self.control_turn_context(request.mode);
        if let Some(authoring) = request
            .control_context
            .as_ref()
            .and_then(|context| context.plan_authoring.as_ref())
        {
            request.access.effective_write_roots = Some(Arc::new(vec![
                authoring.directory().to_string_lossy().into_owned(),
            ]));
            request
                .access
                .readable_directory
                .push(authoring.directory().to_string_lossy().into_owned());
            request.access.readable_directory.push(
                authoring
                    .readable_directory()
                    .to_string_lossy()
                    .into_owned(),
            );
        }
        if let Some(selection) = &self.question_model {
            let model = selection
                .model
                .clone()
                .unwrap_or_else(|| self.session.model.clone());
            if model != request.model {
                request.context_window = None;
            }
            request.model = model;
            request.effort = selection.effort.clone().unwrap_or_else(|| "default".into());
            request.service_tier = selection.service_tier.unwrap_or_default();
        }
        Ok(request)
    }

    fn event(&self, name: &str, payload: Value) -> Result<SessionEvent> {
        Ok(SessionEvent {
            session_id: self.session.id.clone(),
            event: name.into(),
            payload,
        })
    }

    /// Return a session named only by its retracted first prompt to the unnamed state.
    ///
    /// The first exchange names an unnamed session when it starts. Retracting that exchange
    /// restores its prompt to the composer, so the name would otherwise describe a prompt the
    /// transcript no longer contains. A name that differs from the derived one was chosen by the
    /// user or a later exchange and stays.
    pub(crate) fn release_automatic_session_name(&mut self, retracted_prompt: &str) -> Result<()> {
        let derived_name = automatic_session_name(retracted_prompt);
        if self.session.name.is_empty()
            || self.session.name != derived_name
            || !self.store.list_exchange(&self.session.id)?.is_empty()
        {
            return Ok(());
        }
        self.session.name = self
            .store
            .release_session_name(&self.session.id, &self.client_id, &derived_name)?
            .name;
        self.save_session()
    }

    fn save_session(&mut self) -> Result<()> {
        self.session.approval_timeout = self
            .permission_coordinator
            .approval_timeout(&self.session.id)?;
        let now_ms = self.clock.now_ms();
        self.session.acquire_lease(&self.client_id)?;
        self.session.updated_at_ms = now_ms;
        self.store
            .save_owned_session(&self.session, &self.client_id)?;
        self.session.name = self
            .store
            .load_session(&self.session.id)?
            .context("owned session is missing")?
            .name;
        Ok(())
    }

    fn save_preference(&mut self, update_model_preference: bool) -> Result<()> {
        if self.session.background.is_some() {
            return Ok(());
        }
        let previous = self
            .store
            .load_preference(&self.session.workspace, &self.session.provider)?;
        let mut preference = preference_for_session(&self.session, previous.clone());
        if !update_model_preference && let Some(previous) = previous {
            preference.model = previous.model;
            preference.effort = previous.effort;
            preference.model_setting = previous.model_setting;
        }
        self.store
            .save_preference(&self.session.workspace, &self.session.provider, &preference)
    }

    pub(crate) fn release_lease(&mut self) -> Result<()> {
        self.store
            .release_session_lease(&self.session.id, &self.client_id)
    }

    /// Resolve the durable data root for diagnostics and manual verification.
    pub fn data_root(&self) -> &Path {
        &self.data_root
    }

    /// Record plan lifecycle metadata without persisting prompts or plan content.
    fn record_plan_trace(&self, event: &str, fields: Value) {
        self.trace.record(
            &self.session.id,
            event,
            json!({
                "active_plan_id": self.current_plan_id().ok().flatten(),
                "fields": fields,
            }),
        );
    }

    fn configure_trace(&self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let level = serde_json::from_value(
            params
                .get("level")
                .cloned()
                .context("logging level is required")?,
        )?;
        Ok((
            serde_json::to_value(self.trace.configure(level)?)?,
            Vec::new(),
        ))
    }
}

impl Drop for HarnessBroker {
    fn drop(&mut self) {
        let _ = self.release_lease();
    }
}

/// Attribute an identified native tool item to the active checklist step.
fn attribute_provider_tool(
    exchange: &mut Exchange,
    checklist: &mut ChecklistTracker,
    observation: &HarnessEvent,
) -> Result<()> {
    let HarnessEvent::Native { event, .. } = observation else {
        return Ok(());
    };
    let ProviderEvent::Item { item_id, event, .. } = event.as_ref() else {
        return Ok(());
    };
    use forge_provider_api::message::{ItemEvent, ItemStart};
    if !matches!(
        event,
        ItemEvent::Started(
            ItemStart::CommandExecution(_) | ItemStart::FileChange(_) | ItemStart::ToolCall(_)
        ) | ItemEvent::Tool { .. }
    ) {
        return Ok(());
    }
    let Some(address) = native_turn_address(observation) else {
        return Ok(());
    };
    let Some(target) = checklist.attribution_target().map(str::to_owned) else {
        return Ok(());
    };
    let turn_id = exchange.turn_mut(&address)?.id().to_owned();
    if exchange.attribute_tool(&turn_id, item_id.as_str(), &target)? {
        checklist.mark_attributed(&target);
        exchange.checklist = Some(checklist.snapshot().clone());
    }
    Ok(())
}

/// Report every failure of a multi-target operation, keeping a lone failure's original error.
fn aggregate_failures(mut failures: Vec<anyhow::Error>) -> Result<()> {
    match failures.len() {
        0 => Ok(()),
        1 => Err(failures.remove(0)),
        count => {
            let detail = failures
                .iter()
                .map(|failure| format!("{failure:#}"))
                .collect::<Vec<_>>()
                .join("; ");
            Err(anyhow::anyhow!("{count} child operations failed: {detail}"))
        }
    }
}

/// Preserve cancellation ownership when clearing an approval from a child workflow.
fn approval_cancelled(
    view: &crate::provider::approval::ApprovalRequestView,
    now_ms: i64,
) -> Result<PermissionApprovalEvent> {
    let owner = view
        .execution_owner
        .as_ref()
        .context("cancelled approval has no original owner")?;
    let scope = match &view.turn_id {
        Some(turn_id) => crate::provider::ApprovalScope::Turn {
            thread_id: owner.thread_id(),
            turn_id: forge_provider_api::TurnId::new(turn_id.as_str())?,
        },
        None => crate::provider::ApprovalScope::Thread {
            thread_id: owner.thread_id(),
        },
    };
    Ok(PermissionApprovalEvent::Cancelled(
        crate::provider::PermissionApprovalCancellation {
            request_id: crate::provider::ApprovalRequestId::new(view.id.clone())?,
            scope,
            reason: crate::provider::ApprovalCancellationReason::TurnInterrupted,
            cancelled_at_ms: now_ms,
        },
    ))
}

/// Resolve the original typed native turn using the already retained operational owner.
fn native_turn_address(observation: &HarnessEvent) -> Option<crate::provider::TurnAddress> {
    let HarnessEvent::Native { scope, event, .. } = observation else {
        return None;
    };
    match event.as_ref() {
        ProviderEvent::Turn {
            thread_id, turn_id, ..
        }
        | ProviderEvent::Item {
            thread_id, turn_id, ..
        } => Some(crate::provider::TurnAddress {
            scope: scope.invocation()?.clone(),
            thread_id: thread_id.clone(),
            turn_id: turn_id.clone(),
        }),
        ProviderEvent::Provider { .. }
        | ProviderEvent::Thread { .. }
        | ProviderEvent::Agent { .. } => None,
    }
}

/// Collect exact child exchanges for recursive change attribution.
fn referenced_child_exchange_list(interaction: &Exchange) -> Vec<(String, String)> {
    interaction
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::AgentReference { agent } => Some((
                agent.child_agent_id.clone(),
                agent.child_exchange_id.clone(),
            )),
            _ => None,
        })
        .collect()
}

fn load_agent_registry(store: &SqliteStore, session_id: &str) -> Result<AgentRegistry> {
    Ok(AgentRegistry::from_run_list(
        store.list_agent_run(session_id)?,
    ))
}

fn preference_for_session(
    session: &HarnessSession,
    previous: Option<HarnessPreference>,
) -> HarnessPreference {
    let mut model_setting = previous
        .map(|preference| preference.model_setting)
        .unwrap_or_default();
    // A session that never saw the catalog keeps the label an earlier selection learned.
    let label = session.model_label.clone().or_else(|| {
        model_setting
            .get(&session.model)
            .and_then(|setting| setting.label.clone())
    });
    model_setting.insert(
        session.model.clone(),
        ModelSetting {
            reasoning: Some(session.effort.clone()),
            context_window: session.context_window.clone(),
            label,
        },
    );
    HarnessPreference {
        access: session.access.clone(),
        shell: session.shell,

        model: session.model.clone(),
        effort: session.effort.clone(),
        model_setting,
        service_tier: session.service_tier,
        plan_executor: session.plan_executor.clone(),
        plan_compact: session.plan_compact,
        approval_timeout: session.approval_timeout,
        tool_timeout: session.tool_timeout,
    }
}

fn required_text(value: &Value, field: &str) -> Result<String> {
    value
        .get(field)
        .and_then(Value::as_str)
        .map(str::to_owned)
        .with_context(|| format!("{field} is required"))
}

fn plan_trace_fields(plan: &PlanRecord) -> Value {
    json!({
        "plan_id": plan.id,
        "state": format!("{:?}", plan.state),
        "document_version": plan.document_version,
        "submitted_version": plan.submitted_version,
        "model_revision": plan.model_revision,
        "user_revision": plan.user_revision,
        "has_elicitation": plan.elicitation.is_some(),
        "resolved_question_count": plan.question_ledger.resolution.len(),
        "generation_turn": plan.generation.budget.turn_count,
        "generation_max_turn": plan.generation.budget.max_turn_count,
        "generation_no_progress": plan.generation.budget.consecutive_no_progress,
        "generation_max_no_progress": plan.generation.budget.max_consecutive_no_progress,
        "canonical_revision": plan.generation.canonical_revision,
    })
}

fn resolve_fork_name(
    params: &Value,
    source_name: &str,
    existing_name_set: &HashSet<String>,
) -> String {
    params
        .get("name")
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|name| !name.is_empty())
        .map(str::to_owned)
        .unwrap_or_else(|| {
            let base_name = if source_name.is_empty() {
                "fork".to_owned()
            } else {
                format!("{source_name}-fork")
            };
            (0_u64..)
                .map(|ordinal| format!("{base_name}-{ordinal}"))
                .find(|candidate| !existing_name_set.contains(candidate))
                .expect("fork ordinal space is unbounded")
        })
}

fn default_model() -> String {
    "default".into()
}
fn default_effort() -> String {
    "medium".into()
}
fn default_goal_max_turns() -> u32 {
    20
}

/// Address information from the root session to one of its children.
///
/// Every message to a child comes from the root session's agent commands, whether the root's
/// model or its user sent it, so the root's path names the sender.
fn parent_message(text: String) -> forge_provider_api::forge::AgentMessage {
    forge_provider_api::forge::AgentMessage {
        from: forge_provider_api::forge::ROOT_AGENT_PATH.into(),
        text,
    }
}

pub use forge_provider_api::error::TurnCancelled;

#[cfg(test)]
mod test;
