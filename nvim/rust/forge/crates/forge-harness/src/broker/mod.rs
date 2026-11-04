mod owned_task_checks;
pub(crate) use owned_task_checks::{OwnedTaskCheckRun, TaskCheckEvent, TaskCheckProgress};
mod clarification;
mod rollback_effect;
pub(crate) use rollback_effect::RollbackEffectResult;
mod acceptance;
pub(crate) use acceptance::{PlanExecutionPreparation, ValidatedPlanExecution};
#[cfg(test)]
mod cancellation_test;
#[cfg(test)]
mod change_results_test;
mod checkpoint_settlement;
mod checks;
pub(crate) use checkpoint_settlement::FinalCheckpointCommit;
mod compaction;
mod configuration;
pub(crate) use compaction::{CompactionEvidenceError, CompactionProvenance, CompactionResult};
pub(crate) use configuration::{
    ConfigurationCandidate, ConfigurationCatalogResult, ConfigurationEffect,
    ConfigurationPreparation, ConfigurationReconcileResult,
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
pub(crate) use prior_finalization::PriorFinalizationAction;
mod foreground_dispatch;
mod foreground_start;
mod foreground_step;
mod foreground_workspace;
use foreground_dispatch::ForegroundAdmission;
pub(crate) use foreground_dispatch::ForegroundDispatch;
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
pub(crate) use owner_control::{ControlExecution, ExchangeControlSelection, ExchangeControlView};
pub(crate) use prior_finalization::PriorFinalization;
mod plan_checks_update;
pub(crate) mod plan_entry;
mod project_checks;
pub(crate) use check_admission::AutomaticCheckAction;
#[cfg(test)]
pub(crate) use plan_checks_update::{PlanChecksUpdateJob, PlanChecksUpdateResult};
mod reopening;
mod review_question;
pub(crate) use observation::BrokerObserver;

pub use fork::{ForkPreparation, prepare_provider_fork};
pub use new_session::prepare_new_session;

use crate::agent::{Agent, AgentLifecycleEvent, AgentRegistry, AgentState};
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
    PromptMode, Provider, ProviderCapability, ProviderCatalogRequest, ProviderCompletion,
    ProviderError, ProviderEvent, ProviderEventSink, ProviderId, ProviderInput, ProviderLaunch,
    ProviderRequest,
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

fn preview_lines(timeline: &[TimelineEntry]) -> Result<Vec<String>> {
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
    pub task_operation: Option<Value>,
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
    pub approval: Vec<ApprovalRequestView>,
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
    /// Continue an active native goal through its checkpoint transition.
    NativeGoalContinuation(ProviderEvent),
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
    events: crate::provider::events::ProviderEventStream,
    task: tokio::task::JoinSet<Result<T>>,
    pending_submission:
        Option<std::pin::Pin<Box<dyn std::future::Future<Output = Result<T>> + Send>>>,
    result_pending: bool,
    cancellation: Arc<TurnCancellation>,
    watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
    stream_open: bool,
}

/// Describe one event returned by the independently owned native wait.
pub(crate) enum NativeForegroundSignal<T> {
    Cancelled,
    ToolExpired(crate::provider::tool_timeout::ToolTimeout),
    ProviderEvent(ProviderEvent),
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
        events: crate::provider::events::ProviderEventStream,
        submission: impl std::future::Future<Output = Result<T>> + Send + 'static,
        cancellation: Arc<TurnCancellation>,
        watchdog: Arc<crate::provider::tool_timeout::ToolWatchdog>,
    ) -> Self {
        Self {
            events,
            task: tokio::task::JoinSet::new(),
            pending_submission: Some(Box::pin(submission)),
            result_pending: false,
            cancellation,
            watchdog,
            stream_open: true,
        }
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
    pub(crate) idle_native_active: bool,
    pub(crate) provider_event_sink: ProviderEventSink,
    pub(crate) engine: Arc<crate::provider::session_execution::SessionExecution>,
    pub(crate) steering: ProviderSteering,
    pub(crate) agent_revision: tokio::sync::watch::Receiver<u64>,
    pub(crate) question_changes: tokio::sync::watch::Receiver<u64>,
    pub(crate) cleanup: tokio::sync::watch::Receiver<ExecutionCleanup>,
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
    pub(crate) pending_agent_interrupt: Option<(Vec<String>, OwnedAgentRequest, Value)>,
    pub(crate) user_delivery: tokio::task::JoinSet<(crate::provider::SteerCommand, Result<()>)>,
    pub(crate) session_events: Arc<crate::provider::session_execution::SessionObservationInbox>,
    pub(crate) delivery_sink: Option<publication::SessionUpdateSink>,
    pub(crate) scan_interval: tokio::time::Interval,
    pub(crate) execution_id: Option<String>,
    pub(crate) question_enabled: bool,
    pub(crate) question_stop_pending: bool,
    pub(crate) question_override: Option<ProviderCompletion>,
    pub(crate) pending_native_ready: Option<Result<ProviderCompletion, ProviderError>>,
    pub(crate) scan_enabled: bool,
    pub(crate) cleanup_open: bool,
    pub(crate) execution_open: bool,
    pub(crate) _agent_lease: crate::agent::AgentToolLease,
    pub(crate) _check_lease: crate::checks::CheckToolLease,
    pub(crate) native_goal: Option<NativeGoalTransition>,
    pub(crate) pending_goal_effects: Vec<AppliedProviderEvent>,
    pub(crate) pending_goal_events: Vec<SessionEvent>,
    pub(crate) pending_session_observations: std::collections::VecDeque<ProviderEvent>,
    pub(crate) observation_publication_in_flight: bool,
    pub(crate) finalization: Option<Box<foreground_finalization::ForegroundFinalization>>,
    pub(crate) failure_cleanup_active: bool,
    pub(crate) failure_inbox_open: bool,
    pub(crate) failure: Option<foreground_failure::ForegroundFailure>,
    pub(crate) failure_secondary: Vec<String>,
}

/// Track checkpoint phases for an authorized native-goal continuation.
pub(crate) enum NativeGoalTransition {
    Initial {
        observation: ProviderEvent,
        outcome: crate::exchange::ExchangeOutcome,
    },
    Final {
        observation: ProviderEvent,
        outcome: crate::exchange::ExchangeOutcome,
    },
    ContinuationInitial {
        observation: ProviderEvent,
    },
}

/// Preserve the owner's collection disposition independently of provider cancellation flags.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ForegroundCollection {
    Cancel,
    Restart,
}

/// Describe a single owned foreground wait result for a short broker state step.
pub(crate) enum ForegroundSignal {
    LiveHost(Box<crate::provider::LiveHostEffect>),
    CollectionStarted(ForegroundCollection),
    CollectionCompleted(ForegroundCollection),
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
        task: Box<crate::agent::AgentTask>,
        result: Result<()>,
    },
    AgentApprovalsCancelled {
        completion: Box<AgentCompletion>,
        result: Result<()>,
    },
    AgentInterrupted {
        operation: AgentInterrupt,
        completed: usize,
        result: Result<()>,
    },
    AgentSpawnSelected {
        request: OwnedAgentRequest,
        selection: Box<Result<crate::agent::AgentSelection>>,
    },
    AgentMessageSteered {
        request: OwnedAgentRequest,
        registry: Arc<crate::provider::session_execution::SessionExecutionRegistry>,
        owner: crate::provider::ExecutionOwner,
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
    #[cfg(test)]
    RevisionChecksUpdated {
        response: tokio::sync::oneshot::Sender<Result<(Value, Vec<SessionEvent>)>>,
        result: Box<Result<PlanChecksUpdateResult>>,
    },
    PlanScan,
    PlanScanned(Box<Result<execution::ExecutionScanResult>>),
    PlanMutationCompleted(Result<foreground_plan_effect::PlanMutationResult>),
    FinalCheckpoint(Result<checkpoint_settlement::FinalCheckpointResult>),
    AutomaticCheckRefreshed(Box<Result<check_admission::AutomaticCheckRefreshResult>>),
    AutomaticCheckCompleted(Box<Result<check_admission::AutomaticCheckResult>>),
    SettlementReady {
        checkout: crate::provider::session_execution::SettlementCheckout,
        ready: Box<Result<crate::provider::session_execution::NativeReady, ProviderError>>,
    },
    SettlementCommitted(Result<(), crate::provider::session_execution::SettlementError>),
    Cleanup(ExecutionCleanup),
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
    RetractionGoalRestored(Result<()>),
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
            Self::CollectionCompleted(..) => "CollectionCompleted",
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
            Self::AgentInterrupted { .. } => "AgentInterrupted",
            Self::AgentSpawnSelected { .. } => "AgentSpawnSelected",
            Self::AgentMessageSteered { .. } => "AgentMessageSteered",
            Self::ExecutionControl(..) => "ExecutionControl",
            Self::ExecutionControlEvidence { .. } => "ExecutionControlEvidence",
            Self::ExecutionControlReport { .. } => "ExecutionControlReport",
            Self::RevisionReview(..) => "RevisionReview",
            #[cfg(test)]
            Self::RevisionChecksUpdated { .. } => "RevisionChecksUpdated",
            Self::PlanScan => "PlanScan",
            Self::PlanScanned(..) => "PlanScanned",
            Self::PlanMutationCompleted(..) => "PlanMutationCompleted",
            Self::FinalCheckpoint(..) => "FinalCheckpoint",
            Self::AutomaticCheckRefreshed(..) => "AutomaticCheckRefreshed",
            Self::AutomaticCheckCompleted(..) => "AutomaticCheckCompleted",
            Self::SettlementReady { .. } => "SettlementReady",
            Self::SettlementCommitted(..) => "SettlementCommitted",
            Self::Cleanup(..) => "Cleanup",
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
            Self::RetractionGoalRestored(..) => "RetractionGoalRestored",
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

/// Distinguish an applied provider event from a goal checkpoint transition.
pub(crate) enum ForegroundEventApplication {
    Publication(Box<ForegroundJob>),
    NativeGoalContinuation(Box<ProviderEvent>),
}

/// Retain a child launch and the response that becomes valid after it starts.
pub(crate) struct PendingAgentLaunch {
    work: crate::agent::AgentWork,
    reply: Option<(tokio::sync::oneshot::Sender<Result<Value>>, Value)>,
}

/// Child completion and the approvals observed before unlocked cancellation.
pub(crate) struct AgentCompletion {
    owner: crate::provider::ExecutionOwner,
    task_id: String,
    output: Result<crate::provider::ProviderCompletion>,
    cleanup_error: Option<String>,
    approvals: Vec<ApprovalRequestView>,
}

/// Retain interruption ownership and its reply while child cleanup runs unlocked.
pub(crate) struct AgentInterrupt {
    items: Vec<AgentInterruptItem>,
    remaining: Vec<String>,
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
}

struct AgentInterruptItem {
    agent: Agent,
    owner: crate::provider::ExecutionOwner,
    cancelled: Vec<crate::agent::AgentTask>,
    cleanup: Option<crate::agent::AgentTask>,
    provider: Option<Arc<Provider>>,
    approvals: Vec<ApprovalRequestView>,
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
    RetractionGoal(Box<foreground_failure::RetractionGoalJob>),
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
        session_id: String,
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
    #[cfg(test)]
    RevisionChecksUpdate {
        job: Box<PlanChecksUpdateJob>,
        response: tokio::sync::oneshot::Sender<Result<(Value, Vec<SessionEvent>)>>,
    },
    AgentResult {
        engine: Arc<crate::provider::session_execution::SessionExecution>,
        scope: crate::provider::InvocationScope,
        task: Box<crate::agent::AgentTask>,
        message: String,
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
        owner: crate::provider::ExecutionOwner,
        message: String,
        request: OwnedAgentRequest,
    },
    FinalCheckpoint(Box<checkpoint_settlement::FinalCheckpointJob>),
    AutomaticCheckRefresh(Box<check_admission::AutomaticCheckRefresh>),
    AutomaticCheckRun(Box<check_admission::AutomaticCheckRun>),
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
            Self::RetractionGoal(..) => "RetractionGoal",
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
            #[cfg(test)]
            Self::RevisionChecksUpdate { .. } => "RevisionChecksUpdate",
            Self::AgentResult { .. } => "AgentResult",
            Self::AgentCancelApprovals { .. } => "AgentCancelApprovals",
            Self::AgentInterrupt { .. } => "AgentInterrupt",
            Self::AgentSpawnSelect { .. } => "AgentSpawnSelect",
            Self::AgentMessageSteer { .. } => "AgentMessageSteer",
            Self::FinalCheckpoint(..) => "FinalCheckpoint",
            Self::AutomaticCheckRefresh(..) => "AutomaticCheckRefresh",
            Self::AutomaticCheckRun(..) => "AutomaticCheckRun",
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
            | Self::FailureBaseline(_)
            | Self::RetractionGoal(_)
            | Self::CleanupForeground { .. }
            | Self::QuestionStop { .. }
            | Self::PriorObservation(_)
            | Self::PriorInitial(_)
            | Self::PriorFinal(_)
            | Self::AgentCancelApprovals { .. }
            | Self::AgentInterrupt { .. }
            | Self::FinalCheckpoint(_)
            | Self::SettlementRefresh { .. }
            | Self::SettlementCommit { .. }
            | Self::SettleUserDelivery { .. }
            | Self::GoalProgress(_)
            | Self::ExecutionProgress(_)
            | Self::PlanSubmission(_)
            | Self::PlanMutation(_) => true,
            #[cfg(test)]
            Self::RevisionChecksUpdate { .. } => false,
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
            | Self::PlanQuestion(_)
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
                trace.record(scope.owner.session_id.as_str(), "foreground.cleanup", json!({
                    "exchange_id":scope.exchange_id, "phase":"engine_close_started", "cause":format!("{original:#}"),
                }));
                let mut cleanup = if original.is::<TurnCancelled>() {
                    engine
                        .close_with_reason(
                            crate::provider::session_execution::OwnerCloseReason::Cancelled,
                        )
                        .await
                } else {
                    engine.close().await
                };
                trace.record(scope.owner.session_id.as_str(), "foreground.cleanup", json!({
                    "exchange_id":scope.exchange_id, "phase":"engine_closed", "failure":cleanup.as_ref().err().map(ToString::to_string),
                }));
                while let Some(result) = native.join_next().await {
                    if let Err(error) = result
                        && cleanup.is_ok()
                    {
                        cleanup = Err(crate::provider::lifecycle_error::classify_source(
                            forge_provider_api::ProviderOperation::CollectEventReader,
                            &error,
                        ));
                    }
                }
                trace.record(scope.owner.session_id.as_str(), "foreground.cleanup", json!({
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
            Self::RetractionGoal(job) => {
                use futures_util::FutureExt;
                let result = std::panic::AssertUnwindSafe(job.run())
                    .catch_unwind()
                    .await
                    .unwrap_or_else(|_| {
                        Err(anyhow::anyhow!("native retraction goal restore panicked"))
                    });
                Ok(ForegroundSignal::RetractionGoalRestored(result))
            }
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
                session_id,
                fallback,
                checkpoint,
                capability,
            } => {
                let result = async {
                    let provider_session_id = provider
                        .active_session_id_for(&session_id)
                        .await?
                        .or(fallback);
                    provider
                        .cleanup_execution(&session_id)
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
            #[cfg(test)]
            Self::RevisionChecksUpdate { job, response } => {
                Ok(ForegroundSignal::RevisionChecksUpdated {
                    response,
                    result: Box::new(job.run().await),
                })
            }
            Self::AgentResult {
                engine,
                scope,
                task,
                message,
            } => {
                let result = engine
                    .steer(scope, crate::provider::SteeringInput::AgentResult(message))
                    .await
                    .map_err(anyhow::Error::from);
                Ok(ForegroundSignal::AgentResultDelivered { task, result })
            }
            Self::AgentCancelApprovals {
                coordinator,
                completion,
            } => {
                let result = coordinator.cancel_owner(&completion.owner, None).await;
                Ok(ForegroundSignal::AgentApprovalsCancelled { completion, result })
            }
            Self::AgentInterrupt {
                coordinator,
                operation,
            } => {
                let mut completed = 0;
                let authority = operation
                    .request
                    .authority
                    .as_ref()
                    .and_then(|authority| authority.task.clone());
                let result = async {
                    for item in &operation.items {
                        match &authority {
                            Some(authority) => authority
                                .commit(|| coordinator.cancel_owner_immediate(&item.owner))?,
                            None => coordinator.cancel_owner_immediate(&item.owner)?,
                        }
                        completed += 1;
                        if let Some(provider) = &item.provider {
                            tokio::time::timeout(std::time::Duration::from_secs(30), async {
                                provider.cancel_session(&item.owner.runtime_key()).await?;
                                provider
                                    .cleanup_execution(&item.owner.runtime_key())
                                    .await?;
                                provider.shutdown().await
                            })
                            .await
                            .context("child cleanup retry exceeded 30 seconds")??;
                        }
                    }
                    Ok(())
                }
                .await;
                Ok(ForegroundSignal::AgentInterrupted {
                    operation,
                    completed,
                    result,
                })
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
            Self::FinalCheckpoint(job) => Ok(ForegroundSignal::FinalCheckpoint(job.run().await)),
            Self::AutomaticCheckRefresh(job) => Ok(ForegroundSignal::AutomaticCheckRefreshed(
                Box::new(job.run().await),
            )),
            Self::AutomaticCheckRun(job) => Ok(ForegroundSignal::AutomaticCheckCompleted(
                Box::new(job.run().await),
            )),
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
                                forge_provider_api::ProviderOperation::HostEffectPublication,
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
            if self.native_goal.is_none()
                && !self.observation_publication_in_flight
                && !self.pending_session_observations.is_empty()
            {
                return Ok(ForegroundSignal::SessionObservationsReady);
            }
            if self.native_goal.is_none()
                && !self.observation_publication_in_flight
                && !self.question_stop_pending
                && self.steering.pending_deliveries.is_empty()
                && self.pending_native_ready.is_some()
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
                () = cancellation.native_cancel_expired(), if self.collection.is_none() => return Ok(ForegroundSignal::EffectFailed(
                    ProviderError::DeliveryUncertain.into(),
                )),
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
                input = self.steering.active.receive(), if self.user_delivery.is_empty() && self.steering.deferred.is_empty() && self.pending_native_ready.is_none() => {
                    return Ok(ForegroundSignal::UserInput(input.context("parent input receiver closed")?));
                }
                delivery = self.user_delivery.join_next(), if !self.user_delivery.is_empty() => {
                    return Ok(ForegroundSignal::UserDelivery(delivery.context("parent input delivery disappeared")?.context("parent input delivery failed")?));
                }
                changed = self.agent_revision.changed(), if !self.agent_command_active => {
                    changed.context("agent coordinator closed during root execution")?;
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
                _ = self.scan_interval.tick(), if self.scan_enabled && self.pending_revision.is_none() => return Ok(ForegroundSignal::PlanScan),
                changed = self.cleanup.changed(), if self.cleanup_open => {
                    if changed.is_err() { self.cleanup_open = false; continue; }
                    let state = self.cleanup.borrow_and_update().clone();
                    if !matches!(state, ExecutionCleanup::Idle) { return Ok(ForegroundSignal::Cleanup(state)); }
                }
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
    sink: crate::provider::ProviderEventSink,
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
    retraction_eligible: bool,
    active_wait: Option<ActiveWait>,
}

impl ExchangeRuntime {
    fn tools_blocked(&self) -> bool {
        self.tool_gate.as_ref().is_some_and(|gate| !gate.ready())
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

/// Coordinates an out-of-band cancellation request with the active provider turn.
#[derive(Clone, Debug, Default)]
pub(crate) enum ExecutionCleanup {
    #[default]
    Idle,
    Pending,
    #[cfg(test)]
    Failed(String),
}

/// Coordinates execution interruption without discarding pending provider evidence.
pub struct TurnCancellation {
    /// Cancellation revision captured by queued requests before waiting for admission.
    pub(crate) request_revision: AtomicU64,
    requested: Arc<AtomicBool>,
    submission_cancelled: Arc<AtomicBool>,
    restore_prompt: AtomicBool,
    cleanup: tokio::sync::watch::Sender<ExecutionCleanup>,
    retraction_allowed: AtomicBool,
    notify: Notify,
    native_cancel_deadline: std::sync::Mutex<Option<tokio::time::Instant>>,
    native_cancel_notify: Notify,
    state: tokio::sync::watch::Sender<bool>,
}

impl TurnCancellation {
    fn new() -> Self {
        Self {
            request_revision: AtomicU64::new(0),
            requested: Arc::new(AtomicBool::new(false)),
            submission_cancelled: Arc::new(AtomicBool::new(false)),
            restore_prompt: AtomicBool::new(false),
            cleanup: tokio::sync::watch::channel(ExecutionCleanup::Idle).0,
            retraction_allowed: AtomicBool::new(false),
            notify: Notify::new(),
            native_cancel_deadline: std::sync::Mutex::new(None),
            native_cancel_notify: Notify::new(),
            state: tokio::sync::watch::channel(false).0,
        }
    }

    /// Clear cancellation state before dispatching the next broker request.
    pub fn arm(&self, retraction_allowed: bool) {
        self.requested.store(false, Ordering::Release);
        self.submission_cancelled.store(false, Ordering::Release);
        self.restore_prompt.store(false, Ordering::Release);
        self.cleanup.send_replace(ExecutionCleanup::Idle);
        self.retraction_allowed
            .store(retraction_allowed, Ordering::Release);
        *self
            .native_cancel_deadline
            .lock()
            .expect("native cancellation deadline poisoned") = None;
        self.native_cancel_notify.notify_one();
        self.state.send_replace(false);
    }

    /// Request cancellation without waiting for the broker's serialized request lane.
    pub fn request(&self, restore_prompt: bool) {
        self.request_revision.fetch_add(1, Ordering::AcqRel);
        self.submission_cancelled.store(true, Ordering::Release);
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.requested.store(true, Ordering::Release);
        self.state.send_replace(true);
        self.notify.notify_waiters();
    }

    /// Fence queued admission while the native owner settles its cancellation terminal.
    #[cfg(test)]
    pub(crate) fn request_native_cancel(&self, restore_prompt: bool) {
        self.request_revision.fetch_add(1, Ordering::AcqRel);
        self.submission_cancelled.store(true, Ordering::Release);
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.native_cancel_deadline
            .lock()
            .expect("native cancellation deadline poisoned")
            .get_or_insert_with(|| {
                tokio::time::Instant::now() + std::time::Duration::from_secs(30)
            });
        self.native_cancel_notify.notify_one();
        self.state.send_replace(true);
    }

    /// Retire the timeout after the owned native terminal is observed.
    pub(crate) fn native_cancel_settled(&self) {
        *self
            .native_cancel_deadline
            .lock()
            .expect("native cancellation deadline poisoned") = None;
        self.native_cancel_notify.notify_one();
    }

    /// Wait for a native cancellation that never produced a terminal observation.
    async fn native_cancel_expired(&self) {
        loop {
            let changed = self.native_cancel_notify.notified();
            let deadline = *self
                .native_cancel_deadline
                .lock()
                .expect("native cancellation deadline poisoned");
            match deadline {
                Some(deadline) => tokio::select! {
                    () = tokio::time::sleep_until(deadline) => return,
                    () = changed => {}
                },
                None => changed.await,
            }
        }
    }

    /// Retain provider delivery while the owner collects terminal execution evidence.
    pub(crate) fn begin_cleanup(&self, restore_prompt: bool) -> Result<()> {
        self.restore_prompt.store(restore_prompt, Ordering::Release);
        self.submission_cancelled.store(true, Ordering::Release);
        self.cleanup.send_replace(ExecutionCleanup::Pending);
        Ok(())
    }

    /// Publish a retryable cleanup failure without dropping the provider consumer.
    #[cfg(test)]
    pub(crate) fn fail_cleanup(&self, error: &ProviderError) {
        self.cleanup
            .send_replace(ExecutionCleanup::Failed(format!("{error:#}")));
    }

    async fn cancelled(&self) {
        loop {
            let notified = self.notify.notified();
            if self.requested.load(Ordering::Acquire) {
                return;
            }
            notified.await;
        }
    }

    /// Observe cancellation and rearming without acquiring execution ownership.
    pub(crate) fn subscribe(&self) -> tokio::sync::watch::Receiver<bool> {
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
    goal: Option<GoalRecord>,
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
    pub(crate) goal_max_turns: u32,
    permission_store: Arc<RwLock<PermissionStore>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    pub(crate) trace: Arc<TraceStore>,
    event_sink: Option<publication::SessionUpdateSink>,
    clock: Box<dyn Clock>,
    exchange_runtime: Option<ExchangeRuntime>,
    reopened_execution: reopening::ReopenedExecutions,
    retained_foregrounds:
        std::collections::HashMap<crate::provider::InvocationId, reopening::RetainedForeground>,
    pending_host_settlement: Option<Arc<crate::provider::session_execution::SettlementCell>>,
    settlement_events: Vec<SessionEvent>,
    agent_registry: AgentRegistry,
    observed_agent_conversations: HashSet<(
        crate::provider::InvocationId,
        forge_provider_api::ProviderSessionId,
    )>,
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
                    crate::provider::ExecutionOwner::root(&self.session.id),
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
                let selected = store
                    .load_session(session_id)?
                    .context("session not found")?;
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
                        session.context_window = preference.as_ref().and_then(|preference| {
                            preference
                                .model_setting
                                .get(&session.model)
                                .and_then(|setting| setting.context_window.clone())
                        });
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
        if session.resolved_model.is_none() && session.model != "default" {
            session.resolved_model = Some(session.model.clone());
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
        if session.model == "default" && session.resolved_model.is_none() {
            session.resolved_model =
                store.latest_resolved_default_model(&session.workspace, &session.provider)?;
        }
        permission_coordinator.configure_timeout(&session.id, session.approval_timeout)?;
        startup_timing.insert(
            "settings_applied_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        store.lock_session(&session.id)?;
        // Provider-only preparation is disposable across runtimes. Keep all local metadata,
        // including names and settings, while replacing only an unused provider identity.
        if session.background.is_none()
            && session.provider_fork_state == ProviderForkState::Ready
            && session.provider_checkpoint_id.is_none()
            && !store.has_conversation_history(&session.id)?
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
        let agent_registry = load_agent_registry(&store, &session.id)?;
        startup_timing.insert(
            "session_recovered_ms",
            startup_started.elapsed().as_secs_f64() * 1000.0,
        );
        let agent_coordinator = crate::agent::AgentCoordinator::new(session.id.clone());
        agent_coordinator.set_enabled(shared.desired.subagent_tools.unwrap_or(false));
        runtime
            .lsp
            .set_enabled(&session.id, shared.desired.lsp_tools.unwrap_or(false));
        for task in store.list_agent_task(&session.id)? {
            let agent = agent_registry
                .get(&task.agent_id)
                .cloned()
                .context("stored task has no owning child")?;
            agent_coordinator.retain(agent, task)?;
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
                root: data_root.join(&session.id).join("checks"),
            },
            Arc::clone(&presentation),
            store.objects.clone(),
            Arc::clone(&runtime.repositories),
            trace.clone(),
        )?;
        let plan_file = Arc::new(PlanFileStore::new(&data_root, &session.workspace));
        let questions = Arc::new(crate::question::QuestionController::open(
            &data_root,
            &session.id,
        )?);
        let observer = Arc::new(observation::BrokerObserver {
            questions: questions.clone(),
            admission: store.session_admission(&session.id)?,
            input_schedule: tokio::sync::watch::channel(Default::default()).0,
            lsp: runtime.lsp.clone(),
            status_thoughts: AtomicBool::new(shared.desired.status_thoughts.unwrap_or(true)),
            check_editing: Arc::new(AtomicBool::new(
                shared.desired.check_editing.unwrap_or(false),
            )),
            completion_queue: Default::default(),
            session_events: RwLock::new(
                runtime
                    .execution_registry
                    .observer(crate::provider::ExecutionOwner::root(&session.id))?,
            ),
            data_root: data_root.clone(),
            session_id: session.id.clone(),
            runtime_epoch: Uuid::new_v4().to_string(),
            session_metadata: store.session_metadata_writer(&session.id, &request.client_id)?,
            snapshot_revision: AtomicU64::new(0),
            metadata: RwLock::new(observation::ObservationMetadata {
                revision: 0,
                capability: capability.clone(),
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
            submission_store: crate::provider::submission::SubmissionStore::open(
                &data_root,
                trace.clone(),
            )?,
            context_usage_fresh: false,
            lsp: runtime.lsp.clone(),
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
            workspace_kind,
            data_root,
            client_id: request.client_id,
            provider_launch: request.provider,
            execution_registry: runtime.execution_registry.clone(),
            provider,
            session,
            capability,
            goal_max_turns: request.goal_max_turns,
            permission_store,
            permission_coordinator,
            trace,
            plan_review: Arc::new(crate::plan::review_document::PlanReviewStore::default()),
            revision_review: Arc::new(crate::plan::revision_review::RevisionReview::default()),
            event_sink: None,
            clock,
            exchange_runtime: None,
            reopened_execution: Default::default(),
            retained_foregrounds: Default::default(),
            pending_host_settlement: None,
            settlement_events: Vec::new(),
            agent_registry,
            observed_agent_conversations: HashSet::new(),
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

    /// Capture interactive startup settings without creating an exchange or task.
    pub(crate) fn initialization_request(&self) -> Result<Option<ProviderRequest>> {
        (self.session.background.is_none()
            && self.session.purpose == crate::session::SessionPurpose::Conversation
            && self.provider_fork_state() == crate::session::ProviderForkState::Ready)
            .then(|| self.provider_request(ProviderInput::from_text(""), PromptMode::Chat))
            .transpose()
    }

    /// Persist an initialized provider conversation only while its captured settings remain current.
    pub(crate) fn retain_initialized_session(
        &mut self,
        request: &ProviderRequest,
        provider_id: String,
    ) -> Result<Option<BrokerSnapshot>> {
        if self.session.id != request.harness_session_id
            || self.session.provider_session_id.as_deref()
                != request.provider_session_id.as_ref().map(|id| id.as_str())
            || self.session.model != request.model
            || self.session.effort != request.effort
            || self.session.access != request.access
            || self.session.approval_mode != request.approval_mode
        {
            return Ok(None);
        }
        if self.session.provider_session_id.as_ref() != Some(&provider_id) {
            self.session.provider_session_id = Some(provider_id);
            self.save_session()?;
        }
        Ok(Some(self.snapshot()?))
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
            provider_continuation: None,
            questions: Some(self.questions.clone()),
            question_events: self.question_events.clone(),
            live_host_effects: None,
            question_invocation: None,
            question_exchange_id: self
                .exchange_runtime
                .as_ref()
                .map(|runtime| runtime.exchange_id.clone()),
            question_discussion: self.question_discussion,
            task_controller: Some(self.task_controller.clone()),
            native_goal: if mode == PromptMode::GoalContinuation {
                self.task_controller
                    .current()
                    .ok()
                    .flatten()
                    .and_then(|task| match task.workflow.clone() {
                        crate::task::WorkflowSnapshot::Goal { goal } if goal.native => Some(
                            if matches!(
                                &self.input_purpose,
                                crate::input::InputPurpose::TaskContinuation { .. }
                            ) {
                                forge_provider_api::NativeGoalCommand::Resume {
                                    objective: goal.objective,
                                }
                            } else {
                                forge_provider_api::NativeGoalCommand::Set {
                                    objective: goal.objective,
                                }
                            },
                        ),
                        _ => None,
                    })
            } else {
                None
            },
            task_start: match &self.input_purpose {
                crate::input::InputPurpose::TaskEntry { start, .. } => Some(start.clone()),
                _ => None,
            },
            task_authority_progress: None,
            task_permit: callback_snapshot
                .as_ref()
                .and_then(|(_, task)| task.as_ref().map(|task| task.permit())),
            task_generation: callback_snapshot.map(|(generation, _)| generation),
            permission_snapshot,
            permission_required: true,
            submission_cancelled: Some(Arc::clone(&self.turn_cancellation.submission_cancelled)),
            checkpoint: self
                .exchange_runtime
                .as_ref()
                .and_then(|runtime| runtime.tool_gate.clone()),
            agent: None,
            checks: None,
            check_shell: self.session.shell,
            mode,
            planning_feedback: mode == PromptMode::Plan
                && plan
                    .as_ref()
                    .is_some_and(|plan| plan.state == crate::plan::PlanState::AwaitingInput),
            plan_state: plan.as_ref().map(|plan| plan.state),
            plan_document: document,
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
        config: crate::service::TextGenerationConfig,
        policy: crate::session::AutoCompaction,
        sink: publication::SessionUpdateSink,
    ) -> Result<String> {
        let crate::service::TextGenerationConfig {
            model,
            reasoning_effort,
            service_tier,
            ..
        } = config;
        let effort = reasoning_effort.unwrap_or_else(|| "low".into());
        anyhow::ensure!(
            self.session.background.is_some(),
            "Background request requires a Background session"
        );
        service_tier.validate(&self.capability)?;
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
        self.session.model = model;
        self.session.effort = effort;
        self.session.context_window = None;
        self.session.service_tier = service_tier;
        self.session.access.write_access = crate::session::WriteScope::Off;
        self.session.auto_compaction = policy;
        self.checkpoint = None;
        self.event_sink = Some(sink);
        self.save_session()?;
        let result = self
            .run_interaction_input(
                input.clone(),
                PromptMode::Background,
                Some(ExchangeAdmission::chat(input.clone())),
                ProviderInput::from_text(input),
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
                    && Some(&compaction.thread_id) == self.session.provider_session_id.as_ref())
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
                let code = if error.is::<RequestNotAdmitted>() {
                    "not_admitted"
                } else if error.downcast_ref::<TurnCancelled>().is_some() {
                    "turn_cancelled"
                } else if error.is::<crate::provider::tool_timeout::ToolTimeout>() {
                    "tool_timeout"
                } else {
                    "request_failed"
                };
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
                            ProviderInput::from_text(String::new()),
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
            | RuntimeMethod::SessionProviderAuthenticate
            | RuntimeMethod::SessionProviderSetup
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

    /// Drive a test's pending local verification through production check stages.
    #[cfg(test)]
    pub(crate) async fn run_task_checks(&mut self) -> Result<Vec<SessionEvent>> {
        self.run_checks().await
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

    #[cfg(test)]
    fn create_task(
        &mut self,
        kind: crate::task::TaskKind,
        title: String,
        plan_id: Option<String>,
        _goal_id: Option<String>,
    ) -> Result<()> {
        let now_ms = self.clock.now_ms();
        let task = if kind == crate::task::TaskKind::Plan {
            crate::task::Task::plan(
                self.store
                    .load_plan(&plan_id.context("plan is required")?)?
                    .context("plan is missing")?,
                now_ms,
            )?
        } else {
            crate::task::Task::goal(title, Default::default(), false, now_ms)
        };
        let current = self.task_controller.current()?;
        self.task_controller.change(
            &mut self.store,
            current.as_ref().map(|task| &task.revision),
            crate::task::TaskChange::Install(Box::new(task)),
            now_ms,
        )?;
        Ok(())
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
            let follow_up = format!("{}\n\n{text}", clarification.text);
            let elicitation_json = serde_json::to_string_pretty(elicitation)?;
            let prompt = if let Some(plan) = &pending_plan {
                PlanPrompt::clarification(&plan.request, &elicitation_json, &follow_up)
            } else {
                PlanPrompt::question_follow_up(&elicitation_json, &follow_up)
            };
            let mode = self.task_prompt_mode()?;
            let (value, event) = self.run_interaction(prompt, mode, None).await?;
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
            self.run_planning_interaction(text, None).await
        } else {
            self.run_interaction(text, mode, None).await
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
                                owner.agent_id
                                    == forge_provider_api::AgentId::from(agent.id.as_str())
                                    && owner.session_id
                                        == forge_provider_api::SessionId::from(
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
                    let owner = crate::provider::ExecutionOwner {
                        session_id: self.session.id.clone().into(),
                        agent_id: agent.id.clone().into(),
                    };
                    let engine = registry.owner(owner).await?;
                    match engine.steer_current(message.clone().into()).await {
                        Ok(()) => delivery = crate::agent::MessageDelivery::Steered,
                        Err(error) => self.trace.record(&self.session.id, "agent.message_queued", json!({
                            "agent_id":agent.id,"reason":"steering_not_accepted","error":error.to_string()})),
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
                for (agent, _) in self.agent_coordinator.snapshot(None) {
                    self.interrupt_managed_agent(&agent.id, event).await?;
                }
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
                let mut root = self
                    .provider_request(ProviderInput::from_text(String::new()), PromptMode::Chat)?;
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
            self.store
                .admit_agent_task(&admission.agent, &admission.task, &admitted_parent)
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
        owner: &crate::provider::ExecutionOwner,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let pending = self
            .permission_coordinator
            .pending_list()?
            .into_iter()
            .filter(|approval| approval.execution_owner.as_ref() == Some(owner))
            .collect::<Vec<_>>();
        self.permission_coordinator
            .cancel_owner(owner, None)
            .await?;
        for approval in pending {
            if let Some(runtime) = self
                .child_exchange_runtime_by_agent
                .get_mut(owner.agent_id.as_str())
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
                    update: Box::new(crate::provider::ApprovalUpdate::Cancelled(approval.clone())),
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
        let agent = self.agent_coordinator.resolve(target)?;
        let owner = crate::provider::ExecutionOwner {
            session_id: (self.session.id.clone()).into(),
            agent_id: (agent.id.clone()).into(),
        };
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
        if let Some(mut task) = self.agent_coordinator.cleanup_task(&agent.id) {
            let provider = self
                .child_provider_by_agent
                .get(&agent.id)
                .cloned()
                .context("child cleanup obligation has no retained provider adapter")?;
            tokio::time::timeout(std::time::Duration::from_secs(30), async {
                provider.cancel_session(&owner.runtime_key()).await?;
                provider.cleanup_execution(&owner.runtime_key()).await?;
                provider.shutdown().await
            })
            .await
            .context("child cleanup retry exceeded 30 seconds")??;
            let mut agent = agent.clone();
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
            self.agent_coordinator
                .settled(owner.agent_id.as_str(), &task.id, false)?;
            self.child_provider_by_agent.remove(owner.agent_id.as_str());
            self.child_execution_by_agent
                .remove(owner.agent_id.as_str());
        }
        self.publish_agent_state(event).await?;
        Ok(())
    }

    async fn launch_managed_agent(
        &mut self,
        admission: crate::agent::AgentWork,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        if let Some((owner, task_id, error)) = self.begin_managed_agent(admission)? {
            self.complete_managed_agent(owner, task_id, Err(error), None, event)
                .await?;
        }
        Ok(())
    }

    /// Transfer admitted child work to its owned runtime without awaiting it.
    fn begin_managed_agent(
        &mut self,
        mut admission: crate::agent::AgentWork,
    ) -> Result<Option<(crate::provider::ExecutionOwner, String, anyhow::Error)>> {
        let owner = admission.owner().clone();
        anyhow::ensure!(
            owner.session_id == forge_provider_api::SessionId::from(self.session.id.as_str()),
            "child execution belongs to another session"
        );
        let prepared = (|| -> Result<_> {
            let mut message = admission.task.message.clone();
            for incoming in admission
                .agent
                .inbox
                .iter_mut()
                .filter(|message| message.delivery == crate::agent::MessageDelivery::Queued)
            {
                message.push_str(&format!(
                    "\n\nAdditional information:\n{}",
                    incoming.message
                ));
                incoming.delivery = crate::agent::MessageDelivery::Attached;
                incoming.task_id = Some(admission.task.id.clone());
            }
            let mut request = self.agent_provider_factory.request(
                &self
                    .provider_request(ProviderInput::from_text(String::new()), PromptMode::Chat)?,
                &admission,
                message,
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
                    Arc::new(AtomicBool::new(false)),
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

    /// Drain owned mutations through the active prompt or idle controller, never both concurrently.
    pub(crate) async fn drain_agent_commands(
        &mut self,
        mut parent: Option<&mut Exchange>,
    ) -> Result<Vec<SessionEvent>> {
        let mut event = Vec::new();
        while let Some(command) = self.agent_coordinator.try_receive() {
            match command {
                crate::agent::AgentCommand::Request {
                    request,
                    authority,
                    target,
                    response,
                } => {
                    if response.is_closed() {
                        continue;
                    }
                    let authenticated = if request.source == crate::agent::AgentSource::Model {
                        authority.as_ref().is_some_and(|authority| {
                            !*authority.expired.borrow()
                                && authority.owner
                                    == crate::provider::ExecutionOwner::root(&self.session.id)
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
                    let result = if let Err(error) = scoped {
                        Err(error)
                    } else if authenticated {
                        self.handle_agent_request(*request, parent.as_deref_mut(), &mut event)
                            .await
                    } else {
                        Err(anyhow::anyhow!(
                            "agent request has no active authenticated root authority"
                        ))
                    };
                    let _ = response.send(result);
                }
                crate::agent::AgentCommand::Event {
                    owner,
                    task_id,
                    event: provider_event,
                } => {
                    self.observe_managed_agent(owner, &task_id, *provider_event, &mut event)
                        .await?;
                }
                crate::agent::AgentCommand::Completed {
                    owner,
                    task_id,
                    output,
                    cleanup_error,
                } => {
                    Box::pin(self.complete_managed_agent(
                        owner,
                        task_id,
                        *output,
                        cleanup_error,
                        &mut event,
                    ))
                    .await?;
                }
            }
        }
        Ok(event)
    }

    async fn observe_managed_agent(
        &mut self,
        owner: crate::provider::ExecutionOwner,
        task_id: &str,
        provider_event: ProviderEvent,
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
        owner: crate::provider::ExecutionOwner,
        task_id: &str,
        mut provider_event: ProviderEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<AppliedProviderEvent> {
        anyhow::ensure!(
            owner.session_id == forge_provider_api::SessionId::from(self.session.id.as_str())
                && !owner.is_root(),
            "child event crosses execution ownership"
        );
        let binding = provider_event.scope();
        let scope = binding
            .invocation()
            .context("child event requires invocation ownership")?;
        anyhow::ensure!(
            scope.owner == owner
                && scope.exchange_id == forge_provider_api::ExchangeId::from(task_id),
            "child provider event has a different execution owner or task"
        );
        let mut task = self
            .agent_coordinator
            .task(task_id)
            .context("child event has no admitted task")?;
        anyhow::ensure!(
            forge_provider_api::AgentId::from(task.agent_id.as_str()) == owner.agent_id,
            "child event has a different task owner"
        );
        if task.completed_at_ms.is_some() {
            return Ok(AppliedProviderEvent::Effects(Vec::new()));
        }
        if let ProviderEvent::Agent {
            update: crate::provider::AgentUpdate::Lifecycle { event: lifecycle },
            scope,
        } = &provider_event
        {
            self.apply_agent_lifecycle(lifecycle, Some(scope), event)?;
            return Ok(AppliedProviderEvent::Effects(Vec::new()));
        }
        if provider_event.turn_address().is_some_and(|address| {
            self.observed_agent_conversations
                .iter()
                .any(|(invocation, conversation)| {
                    invocation == &address.scope.invocation_id
                        && conversation.as_str() == address.thread_id
                })
        }) {
            return Ok(AppliedProviderEvent::Effects(Vec::new()));
        }
        let provider_thread_id = match &provider_event {
            ProviderEvent::Agent {
                update: crate::provider::AgentUpdate::ConversationResolved { thread_id },
                ..
            } => Some(thread_id.as_str()),
            event => event
                .turn_address()
                .map(|address| address.thread_id.as_str()),
        };
        if let Some(thread_id) = provider_thread_id.filter(|id| !id.is_empty()) {
            let mut agent = self.agent_coordinator.resolve(owner.agent_id.as_str())?;
            if agent.provider_thread_id.as_deref() != Some(thread_id) {
                agent.provider_thread_id = Some(thread_id.to_owned());
                self.store.save_agent_run(&agent)?;
                self.agent_registry.insert(agent.clone());
                self.agent_coordinator.retain(agent, task.clone())?;
            }
        }
        if matches!(&provider_event, ProviderEvent::Agent { .. }) {
            return Ok(AppliedProviderEvent::Effects(Vec::new()));
        }
        if let ProviderEvent::Runtime {
            update: crate::provider::RuntimeUpdate::Resolved { runtime },
            ..
        } = &provider_event
        {
            let mut agent = self.agent_coordinator.resolve(owner.agent_id.as_str())?;
            if let Some(model) = runtime.model.as_ref().filter(|model| !model.is_empty())
                && let Some(selection) = agent.selection.as_mut()
            {
                selection.model = model.clone();
            }
            self.store.save_agent_run(&agent)?;
            self.agent_registry.insert(agent.clone());
            self.agent_coordinator.retain(agent, task)?;
            return self.prepare_agent_state();
        }
        if task.state == ExchangeState::Queued {
            task.state = ExchangeState::Running;
            self.store.save_agent_task(&task)?;
            self.agent_coordinator.retain(
                self.agent_coordinator.resolve(owner.agent_id.as_str())?,
                task.clone(),
            )?;
        }
        anyhow::ensure!(
            self.child_exchange_runtime_by_agent
                .get(owner.agent_id.as_str())
                .is_none_or(|runtime| runtime.exchange.id == task_id),
            "late child event cannot update a newer task"
        );
        let mut runtime = match self
            .child_exchange_runtime_by_agent
            .remove(owner.agent_id.as_str())
        {
            Some(runtime) => runtime,
            None => {
                let mut exchange = self
                    .store
                    .load_exchange(task_id)?
                    .context("child task exchange missing")?;
                if exchange.is_finished() {
                    return Ok(AppliedProviderEvent::Effects(Vec::new()));
                }
                exchange.phase = crate::exchange::ExchangePhase::Running;
                exchange.resume(self.clock.now_ms())?;
                ChildExchangeRuntime {
                    exchange,
                    checklist: ChecklistTracker::default(),
                }
            }
        };
        match &mut provider_event {
            ProviderEvent::Approval {
                update: crate::provider::ApprovalUpdate::Requested(request),
                ..
            } => {
                let turn = runtime
                    .exchange
                    .turn
                    .iter()
                    .rev()
                    .find(|turn| turn.state() == crate::turn::TurnState::Running);
                let Some(approval) = self.permission_coordinator.bind_owner(
                    &request.id,
                    &runtime.exchange.id,
                    turn.map(|turn| turn.id()),
                )?
                else {
                    self.child_exchange_runtime_by_agent
                        .insert(owner.agent_id.to_string(), runtime);
                    return Ok(AppliedProviderEvent::Effects(Vec::new()));
                };
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    true,
                    self.clock.now_ms(),
                );
                *request = approval;
            }
            ProviderEvent::Approval { update, .. } => {
                let approval = match update {
                    crate::provider::ApprovalUpdate::Resolved(approval)
                    | crate::provider::ApprovalUpdate::Cancelled(approval) => approval,
                    crate::provider::ApprovalUpdate::Requested(_) => unreachable!(),
                };
                runtime.exchange.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                runtime.exchange.approval_resolution.push(approval.clone());
            }
            ProviderEvent::Compaction { update, .. } => {
                runtime
                    .exchange
                    .observe_compaction(update.as_ref().clone(), self.clock.now_ms());
            }
            observation => {
                runtime
                    .exchange
                    .observe_turn(observation, self.clock.now_ms())?;
                if let ProviderEvent::Checklist { update, .. } = observation {
                    runtime.checklist.replace(update);
                    runtime.exchange.checklist = Some(runtime.checklist.snapshot().clone());
                }
                attribute_provider_tool(
                    &mut runtime.exchange,
                    &mut runtime.checklist,
                    observation,
                )?;
            }
        }
        if provider_event.tool_output_delta().is_some() {
            self.store
                .save_tool_output_delta(&runtime.exchange, &provider_event)?;
        } else {
            self.store.save_exchange(&runtime.exchange)?;
        }
        task.last_activity_at_ms = Some(
            task.last_activity_at_ms
                .unwrap_or(i64::MIN)
                .max(self.clock.now_ms()),
        );
        self.store.save_agent_task(&task)?;
        self.agent_coordinator.retain(
            self.agent_coordinator.resolve(owner.agent_id.as_str())?,
            task,
        )?;
        self.child_exchange_runtime_by_agent
            .insert(owner.agent_id.to_string(), runtime);
        let mut effects = Vec::new();
        if let Some(update) = SessionUpdate::from_observation(&provider_event) {
            effects.push(AppliedProviderEvent::Update(update));
        }
        effects.push(self.prepare_agent_state()?);
        self.agent_coordinator.notify_activity();
        Ok(AppliedProviderEvent::Effects(effects))
    }

    async fn complete_managed_agent(
        &mut self,
        owner: crate::provider::ExecutionOwner,
        task_id: String,
        output: Result<crate::provider::ProviderCompletion>,
        cleanup_error: Option<String>,
        event: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            owner.session_id == forge_provider_api::SessionId::from(self.session.id.as_str())
                && forge_provider_api::AgentId::from(task.agent_id.as_str()) == owner.agent_id,
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok(());
        }
        self.cancel_agent_approvals(&owner, event).await?;
        let (effects, next) =
            self.apply_managed_agent_completion(owner, task_id, output, cleanup_error)?;
        event.extend(self.prepare_provider_delivery(effects)?.deliver().await?);
        if let Some(next) = next {
            Box::pin(self.launch_managed_agent(next, event)).await?;
        }
        Ok(())
    }

    /// Apply a settled child after its pending approvals have been collected.
    fn apply_managed_agent_completion(
        &mut self,
        owner: crate::provider::ExecutionOwner,
        task_id: String,
        output: Result<crate::provider::ProviderCompletion>,
        cleanup_error: Option<String>,
    ) -> Result<(AppliedProviderEvent, Option<crate::agent::AgentWork>)> {
        let mut task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            owner.session_id == forge_provider_api::SessionId::from(self.session.id.as_str())
                && forge_provider_api::AgentId::from(task.agent_id.as_str()) == owner.agent_id,
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok((AppliedProviderEvent::Effects(Vec::new()), None));
        }
        let mut agent = self.agent_coordinator.resolve(owner.agent_id.as_str())?;
        let interrupted = self
            .agent_coordinator
            .cancellation(owner.agent_id.as_str(), &task_id)
            .is_ok_and(|cancel| *cancel.borrow());
        let provider_cancelled = output
            .as_ref()
            .is_err_and(|error| error.is::<TurnCancelled>());
        task.state = if cleanup_error.is_some() {
            ExchangeState::Failed
        } else if interrupted || provider_cancelled {
            ExchangeState::Cancelled
        } else if output.is_err() {
            ExchangeState::Failed
        } else {
            ExchangeState::Complete
        };
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
            .remove(owner.agent_id.as_str());
        let mut exchange = if let Some(runtime) = runtime {
            runtime.exchange
        } else {
            self.store
                .load_exchange(&task_id)?
                .context("child completion exchange missing")?
        };
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
        self.store.complete_agent_task(&agent, &task, &exchange)?;
        self.agent_registry.insert(agent.clone());
        self.agent_coordinator.retain(agent.clone(), task.clone())?;
        let next = self.agent_coordinator.settled(
            owner.agent_id.as_str(),
            &task_id,
            cleanup_error.is_some(),
        )?;
        if cleanup_error.is_none() {
            self.child_provider_by_agent.remove(owner.agent_id.as_str());
            self.child_execution_by_agent
                .remove(owner.agent_id.as_str());
        }
        self.trace.record(&self.session.id,"agent.completed",json!({"agent_id":owner.agent_id,"exchange_id":task_id,"state":task.state,"cleanup_error":cleanup_error}));
        Ok((self.prepare_agent_state()?, next))
    }

    fn apply_agent_lifecycle(
        &mut self,
        lifecycle: &AgentLifecycleEvent,
        scope: Option<&crate::provider::InvocationScope>,
        _event: &mut Vec<SessionEvent>,
    ) -> Result<Option<Agent>> {
        if let (Some(scope), Some(conversation)) = (scope, &lifecycle.conversation_id) {
            self.observed_agent_conversations
                .insert((scope.invocation_id.clone(), conversation.clone()));
        }
        self.trace.record(
            &self.session.id,
            "agent.lifecycle",
            serde_json::to_value(lifecycle)?,
        );
        Ok(None)
    }

    async fn list_provider_model(&mut self) -> Result<(Value, Vec<SessionEvent>)> {
        let mut model_list = self
            .provider
            .model_list(self.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?)
            .await?;
        let preference = self
            .store
            .load_preference(&self.session.workspace, &self.session.provider)?;
        crate::provider::select_model_settings(&mut model_list, preference.as_ref());
        let event = self.resolve_default_model(&model_list)?;
        Ok((serde_json::to_value(model_list)?, event))
    }

    /// Publishes an unresolved default model without replacing an explicit selection.
    pub(crate) fn resolve_default_model(
        &mut self,
        model_list: &[crate::provider::ProviderModel],
    ) -> Result<Vec<SessionEvent>> {
        if self.session.model == "default"
            && let Some(model) = model_list
                .iter()
                .find(|model| model.is_default)
                .or_else(|| model_list.first())
            && self.session.resolved_model.as_deref() != Some(model.id.as_str())
        {
            self.session.resolved_model = Some(model.id.clone());
            self.save_session()?;
            return Ok(vec![self.event(
                "session_configured",
                serde_json::to_value(self.session_view()?)?,
            )?]);
        }
        Ok(Vec::new())
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
            goal: self.current_goal()?,
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
    fn prepare_retracted_control_restore(
        &mut self,
        snapshot: PromptControlSnapshot,
    ) -> Result<Option<foreground_failure::RetractionGoalJob>> {
        let controller = self.task_controller.clone();
        controller
            .with_task_snapshot(|current| self.restore_retracted_host_snapshot(snapshot, current))
    }

    /// Restore dependent artifacts while the shared task mutation gate excludes new authority.
    fn restore_retracted_host_snapshot(
        &mut self,
        snapshot: PromptControlSnapshot,
        current: Option<crate::task::Task>,
    ) -> Result<Option<foreground_failure::RetractionGoalJob>> {
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
        if unchanged && self.capability.native_goal && self.session.provider_session_id.is_some() {
            let request = self.provider_request(ProviderInput::from_text(""), PromptMode::Chat)?;
            let update = match snapshot.goal {
                Some(goal) => crate::provider::native_goal_update(goal.objective, goal.state),
                None => crate::provider::NativeGoalUpdate::Absent,
            };
            let registry = self.execution_registry();
            return Ok(Some(foreground_failure::RetractionGoalJob::new(
                self,
                request,
                update,
                self.session.active_task.clone(),
                registry,
            )));
        }
        Ok(None)
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
            let feedback = format!(
                "Pending planning questions withdrawn: {}",
                withdrawal
                    .answer
                    .as_deref()
                    .unwrap_or("no material decision remains")
            );
            let continuation = self
                .run_planning_interaction(
                    PlanPrompt::feedback(&current_plan.request, &feedback),
                    None,
                )
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
        let result = self
            .run_planning_interaction(PlanPrompt::feedback(&plan.request, &answer), None)
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
        let prompt = format!(
            "Harness resolved the pending questions and recorded their response sources. Do not call question_answer or question_withdraw for this set. Continue the original request using the responses and current task context.\n\n{feedback}"
        );
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
            || self.turn_cancellation.requested.load(Ordering::Acquire)
        {
            return Ok(false);
        }
        let owner = crate::provider::ExecutionOwner::root(&self.session.id);
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
        text: String,
        mode: PromptMode,
        admission: Option<ExchangeAdmission>,
    ) -> Result<(Value, Vec<SessionEvent>)> {
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
        let input = ProviderInput::from_text(text.clone());
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
                self.task_controller.observe_plan(
                    &mut self.store,
                    &current.permit(),
                    crate::task::TaskTurnId {
                        exchange_ordinal: exchange.ordinal,
                        turn_ordinal: exchange.turn.len(),
                    },
                    after != setup_digest,
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
        prompt: String,
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
            if self.turn_cancellation.requested.load(Ordering::Acquire) {
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
            let execution_revision = check_scope
                .as_ref()
                .map(|scope| self.reopened_execution.revision(scope));
            let run = self
                .check_runner
                .run(
                    run,
                    self.event_sink.clone(),
                    Some(Arc::clone(&self.turn_cancellation.requested)),
                )
                .await?;
            if let Some(scope) = &check_scope {
                self.refresh_settlement(&mut interaction, scope).await?;
                if execution_revision != Some(self.reopened_execution.revision(scope)) {
                    result.1.extend(self.finalize_observed_execution().await?);
                    // Saved results cannot authorize work from a newer native execution.
                    continue;
                }
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
                        diagnostic: crate::provider::ProviderDiagnostic {
                            kind: forge_provider_api::ProviderFailureKind::Unknown,
                            message: forge_provider_api::SanitizedMessage::new(reason.clone())
                                .expect("host diagnostic is nonempty"),
                            fatal: false,
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
            let repaired = Box::pin(self.run_interaction_input_inner(
                prompt.clone(),
                PromptMode::Chat,
                Some(admission),
                ProviderInput::from_text(prompt),
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
            retraction_eligible: true,
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
            retraction_eligible: false,
            active_wait: None,
        });
        Ok(())
    }

    fn agent_result_message(&self, task: &crate::agent::AgentTask) -> Result<String> {
        let agent = self.agent_coordinator.resolve(&task.agent_id)?;
        Ok(format!(
            "Harness child result. This is a report from a child agent, not a new user request.\n{}",
            serde_json::to_string(&json!({
                "agent":agent.task_path,"task_id":task.id,"status":task.state,
                "response":task.final_response,"error":task.error,
            }))?
        ))
    }

    fn acknowledge_agent_result(&mut self, mut task: crate::agent::AgentTask) -> Result<()> {
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
                    diagnostic: crate::provider::ProviderDiagnostic {
                        kind: forge_provider_api::ProviderFailureKind::Unknown,
                        message: forge_provider_api::SanitizedMessage::new(message.clone())
                            .expect("host diagnostic is nonempty"),
                        fatal: false,
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
            self.reopened_execution.begin_settlement(scope);
        }
        let result: Result<Option<String>> = async {
            loop {
                if let Some(scope) = &scope {
                    self.refresh_settlement(interaction, scope).await?;
                }
                let revision = scope
                    .as_ref()
                    .map(|scope| self.reopened_execution.revision(scope));
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
                        && (self.refresh_settlement(interaction, scope).await?
                            || revision != Some(self.reopened_execution.revision(scope)))
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
                    && (self.refresh_settlement(interaction, scope).await?
                        || revision != Some(self.reopened_execution.revision(scope)))
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
            self.reopened_execution.end_settlement(scope);
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

    #[cfg(test)]
    async fn test_apply_provider_event(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        provider_event: ProviderEvent,
        events: &mut Vec<SessionEvent>,
    ) -> Result<()> {
        let applied = self.apply_provider_event(runtime, interaction, provider_event, events)?;
        let delivery = self.prepare_provider_delivery(applied)?;
        events.extend(delivery.deliver().await?);
        Ok(())
    }

    /// Transfer one active exchange runtime to a service-owned foreground transaction.
    pub(crate) fn begin_owned_foreground(
        &mut self,
        interaction: Exchange,
        scope: crate::provider::InvocationScope,
    ) -> Result<OwnedForegroundTransaction> {
        anyhow::ensure!(
            scope.owner == crate::provider::ExecutionOwner::root(&self.session.id)
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
        provider_event: ProviderEvent,
    ) -> Result<(AppliedProviderEvent, Vec<SessionEvent>)> {
        anyhow::ensure!(
            transaction.scope.owner == crate::provider::ExecutionOwner::root(&self.session.id)
                && provider_event.scope().owner() == &transaction.scope.owner,
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
        if let Some(permit) = &runtime.task_permit {
            permit.validate(self.task_controller.current()?.as_ref())?;
        }
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
        observation: ProviderEvent,
    ) -> Result<ForegroundEventApplication> {
        if transaction
            .finalization
            .as_ref()
            .is_some_and(|finalization| finalization.submission.is_none())
            && let ProviderEvent::Execution { observation } = observation
        {
            if let Some(permit) = &transaction.dispatched_task_permit {
                permit.validate_observation(self.task_controller.current()?.as_ref())?;
            }
            let events = self.observe_execution_lifecycle(observation)?;
            transaction.idle_native_active =
                self.reopened_execution.active(&transaction.state.scope);
            transaction.state.interaction = self
                .store
                .load_exchange(&transaction.state.interaction.id)?
                .context("idle continuation exchange disappeared")?;
            let mut delivery = self.prepare_provider_delivery(AppliedProviderEvent::None)?;
            delivery.events = events;
            return Ok(ForegroundEventApplication::Publication(Box::new(
                ForegroundJob::Publication(delivery),
            )));
        }
        let (applied, mut events) =
            self.apply_owned_foreground(&mut transaction.state, observation)?;
        match applied {
            AppliedProviderEvent::NativeGoalContinuation(observation) => {
                transaction.pending_goal_events.extend(events);
                Ok(ForegroundEventApplication::NativeGoalContinuation(
                    Box::new(observation),
                ))
            }
            applied => {
                let mut delivery = self.prepare_provider_delivery(applied)?;
                events.append(&mut delivery.events);
                delivery.events = events;
                Ok(ForegroundEventApplication::Publication(Box::new(
                    ForegroundJob::Publication(delivery),
                )))
            }
        }
    }

    /// Route an admitted user input to an owned native steer or the retained queue.
    pub(crate) fn prepare_foreground_input(
        &self,
        transaction: &mut ForegroundTransaction,
        input: crate::provider::SteerCommand,
    ) -> ForegroundAction {
        if self.provider.descriptor().capability.native_steer {
            transaction.steering.refresh_readiness = true;
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

    /// Start an authorized native-goal continuation without waiting for checkpoint work.
    pub(crate) fn begin_native_goal_transition(
        &mut self,
        transaction: &mut ForegroundTransaction,
        observation: ProviderEvent,
    ) -> Result<ForegroundAction> {
        anyhow::ensure!(
            transaction.native_goal.is_none(),
            "a native-goal transition is already pending"
        );
        let runtime = transaction
            .state
            .runtime
            .as_ref()
            .context("foreground transaction already committed")?;
        anyhow::ensure!(
            self.native_goal_continuation_needed(
                runtime,
                &transaction.state.interaction,
                &observation
            )?,
            "native-goal continuation is no longer authorized"
        );
        let previous = transaction
            .state
            .interaction
            .turn
            .last()
            .context("native goal has no prior turn")?;
        let crate::turn::TurnState::Finished { outcome } = previous.state() else {
            anyhow::bail!("native goal prior turn is not settled");
        };
        let outcome = match outcome {
            crate::turn::TurnOutcome::Completed => crate::exchange::ExchangeOutcome::Complete,
            crate::turn::TurnOutcome::Cancelled => crate::exchange::ExchangeOutcome::Cancelled,
            crate::turn::TurnOutcome::Interrupted => crate::exchange::ExchangeOutcome::Interrupted,
            crate::turn::TurnOutcome::Failed => crate::exchange::ExchangeOutcome::Failed,
        };
        transaction.native_goal = Some(NativeGoalTransition::Initial {
            observation,
            outcome,
        });
        if runtime.checkpoint.is_some() {
            return Ok(ForegroundAction::Wait);
        }
        self.prepare_native_goal_final(transaction)
    }

    /// Commit an initial checkpoint and advance any waiting goal transition.
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
        match &transaction.native_goal {
            Some(NativeGoalTransition::Initial { .. }) => {
                self.prepare_native_goal_final(transaction)
            }
            Some(NativeGoalTransition::ContinuationInitial { .. }) => {
                self.finish_native_goal_exchange(transaction)
            }
            Some(NativeGoalTransition::Final { .. }) => {
                anyhow::bail!("unexpected baseline during final checkpoint capture")
            }
            None => Ok(ForegroundAction::Wait),
        }
    }

    /// Record final checkpoint failure and release goal settlement ownership.
    pub(crate) fn fail_native_goal_checkpoint(
        &mut self,
        transaction: &mut ForegroundTransaction,
        error: anyhow::Error,
    ) -> Result<ForegroundAction> {
        anyhow::ensure!(
            matches!(
                &transaction.native_goal,
                Some(NativeGoalTransition::Final { .. })
            ),
            "native-goal transition is not waiting for final checkpoint evidence"
        );
        transaction.native_goal = None;
        let interaction = &mut transaction.state.interaction;
        if let Some(scope) = interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone())
        {
            self.reopened_execution.end_settlement(&scope);
        }
        interaction.finalization_error = Some(format!("{error:#}"));
        self.store.save_exchange(interaction)?;
        Err(error)
    }

    /// Begin final checkpoint capture after the original baseline is durable.
    fn prepare_native_goal_final(
        &mut self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        let Some(NativeGoalTransition::Initial {
            observation,
            outcome,
        }) = transaction.native_goal.take()
        else {
            anyhow::bail!("native-goal transition is not waiting for its baseline");
        };
        let interaction = &mut transaction.state.interaction;
        interaction.begin_finalization(outcome, self.clock.now_ms())?;
        self.store.save_exchange(interaction)?;
        if let Some(scope) = interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone())
        {
            self.reopened_execution.begin_settlement(&scope);
        }
        if self.checkpoint.is_none() {
            let mut changes = crate::exchange::ProviderDiffBuilder::default();
            self.visit_provider_exchanges(interaction, |exchange| changes.record(exchange))?;
            interaction.attributed_diff_text = changes.finish();
            interaction.attributed_matches_checkpoint = false;
            interaction.finish(outcome, self.clock.now_ms())?;
            self.store.save_exchange(interaction)?;
            return self.start_native_goal_exchange(transaction, observation);
        }
        let job = match self.prepare_final_checkpoint(interaction, outcome) {
            Ok(job) => job,
            Err(error) => {
                if let Some(scope) = interaction
                    .turn
                    .last()
                    .map(|turn| turn.provider().scope.clone())
                {
                    self.reopened_execution.end_settlement(&scope);
                }
                interaction.finalization_error = Some(format!("{error:#}"));
                self.store.save_exchange(interaction)?;
                return Err(error);
            }
        };
        transaction.native_goal = Some(NativeGoalTransition::Final {
            observation,
            outcome,
        });
        Ok(ForegroundAction::Effect(ForegroundJob::FinalCheckpoint(
            Box::new(job),
        )))
    }

    /// Commit native-goal checkpoint evidence or recapture after a stale revision.
    pub(crate) fn complete_native_goal_checkpoint(
        &mut self,
        transaction: &mut ForegroundTransaction,
        result: checkpoint_settlement::FinalCheckpointResult,
    ) -> Result<ForegroundAction> {
        let Some(NativeGoalTransition::Final {
            observation,
            outcome,
        }) = transaction.native_goal.take()
        else {
            anyhow::bail!("native-goal transition is not waiting for final checkpoint evidence");
        };
        let committed =
            match self.commit_final_checkpoint(&mut transaction.state.interaction, result) {
                Ok(committed) => committed,
                Err(error) => {
                    let interaction = &mut transaction.state.interaction;
                    if let Some(scope) = interaction
                        .turn
                        .last()
                        .map(|turn| turn.provider().scope.clone())
                    {
                        self.reopened_execution.end_settlement(&scope);
                    }
                    interaction.finalization_error = Some(format!("{error:#}"));
                    self.store.save_exchange(interaction)?;
                    return Err(error);
                }
            };
        match committed {
            FinalCheckpointCommit::Stale => {
                transaction.native_goal = Some(NativeGoalTransition::Initial {
                    observation,
                    outcome,
                });
                for event in self.observer.session_events().drain()? {
                    transaction
                        .pending_goal_events
                        .extend(self.observe_session_event(event)?);
                }
                transaction.state.interaction = self
                    .store
                    .load_exchange(&transaction.state.interaction.id)?
                    .context("native-goal exchange disappeared during checkpoint capture")?;
                if transaction
                    .state
                    .interaction
                    .turn
                    .last()
                    .is_some_and(|turn| self.reopened_execution.active(&turn.provider().scope))
                {
                    return Ok(ForegroundAction::Wait);
                }
                self.prepare_native_goal_final(transaction)
            }
            FinalCheckpointCommit::Committed(_) => {
                self.start_native_goal_exchange(transaction, observation)
            }
        }
    }

    /// Admit the next goal exchange while its initial checkpoint worker runs independently.
    fn start_native_goal_exchange(
        &mut self,
        transaction: &mut ForegroundTransaction,
        observation: ProviderEvent,
    ) -> Result<ForegroundAction> {
        if let Some(scope) = transaction
            .state
            .interaction
            .turn
            .last()
            .map(|turn| turn.provider().scope.clone())
        {
            self.reopened_execution.end_settlement(&scope);
        }
        let (mut continuation, _, events, effects) =
            self.stage_interaction_for_turn("", true, self.clock.now_ms(), None)?;
        continuation.origin = crate::exchange::ExchangeOrigin::task_continuation(
            "The native goal requires another turn",
        );
        continuation
            .checklist
            .clone_from(&transaction.state.interaction.checklist);
        self.begin_exchange_runtime(&mut continuation, self.clock.now_ms())?;
        let runtime = self
            .exchange_runtime
            .take()
            .context("continuation runtime is missing")?;
        transaction.state.interaction = continuation;
        transaction.state.runtime = Some(runtime);
        transaction.pending_goal_events.extend(events);
        transaction.pending_goal_effects.extend(effects);
        transaction.native_goal = Some(NativeGoalTransition::ContinuationInitial { observation });
        if transaction
            .state
            .runtime
            .as_ref()
            .is_some_and(|runtime| runtime.checkpoint.is_some())
        {
            return Ok(ForegroundAction::Wait);
        }
        self.finish_native_goal_exchange(transaction)
    }

    /// Bind the admitted native turn after the new goal baseline is durable.
    fn finish_native_goal_exchange(
        &mut self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        let Some(NativeGoalTransition::ContinuationInitial { observation }) =
            transaction.native_goal.take()
        else {
            anyhow::bail!("native-goal transition is not waiting for continuation admission");
        };
        let address = observation
            .turn_address()
            .context("native goal start has no turn address")?
            .clone();
        let interaction = &mut transaction.state.interaction;
        interaction
            .inherit_native_turn(address, observation.observed_at_ms(self.clock.now_ms()))?;
        let runtime = transaction
            .state
            .runtime
            .as_mut()
            .context("continuation runtime is missing")?;
        runtime.retraction_eligible = false;
        self.store.save_exchange(interaction)?;
        transaction.pending_goal_effects.push(
            self.prepare_live_interaction(publication::LiveUpdate::StateChanged, interaction)?,
        );
        // The observation that opened the continuation still needs normal content
        // handling after its native turn has been bound to the new exchange.
        transaction
            .pending_goal_effects
            .push(self.apply_exchange_content(runtime, interaction, observation, false)?);
        let effects =
            AppliedProviderEvent::Effects(std::mem::take(&mut transaction.pending_goal_effects));
        let mut delivery = self.prepare_provider_delivery(effects)?;
        let mut events = std::mem::take(&mut transaction.pending_goal_events);
        events.append(&mut delivery.events);
        delivery.events = events;
        Ok(ForegroundAction::Effect(ForegroundJob::Publication(
            delivery,
        )))
    }

    /// Capture a plan scan as owned work that can run without the broker lock.
    pub(crate) fn apply_foreground_agent_observation(
        &mut self,
        owner: crate::provider::ExecutionOwner,
        task_id: &str,
        observation: ProviderEvent,
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
        owner: crate::provider::ExecutionOwner,
        task_id: String,
        output: Result<crate::provider::ProviderCompletion>,
        cleanup_error: Option<String>,
    ) -> Result<ForegroundAction> {
        let task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion has no admitted task")?;
        anyhow::ensure!(
            owner.session_id == forge_provider_api::SessionId::from(self.session.id.as_str())
                && forge_provider_api::AgentId::from(task.agent_id.as_str()) == owner.agent_id,
            "child completion crosses execution ownership"
        );
        if task.completed_at_ms.is_some() {
            return Ok(ForegroundAction::Wait);
        }
        let approvals = self
            .permission_coordinator
            .pending_list()?
            .into_iter()
            .filter(|approval| approval.execution_owner.as_ref() == Some(&owner))
            .collect();
        Ok(ForegroundAction::Effect(
            ForegroundJob::AgentCancelApprovals {
                coordinator: self.permission_coordinator.clone(),
                completion: Box::new(AgentCompletion {
                    owner,
                    task_id,
                    output,
                    cleanup_error,
                    approvals,
                }),
            },
        ))
    }

    /// Commit child completion only after its approval cancellation has settled.
    pub(crate) fn apply_foreground_agent_completion(
        &mut self,
        transaction: &mut ForegroundTransaction,
        completion: AgentCompletion,
        result: Result<()>,
    ) -> Result<ForegroundAction> {
        transaction.agent_command_active = false;
        if let Err(error) = result {
            if let Some((response, _)) = transaction.pending_agent_reply.take() {
                let _ = response.send(Err(anyhow::anyhow!("{error:#}")));
            }
            return Err(error);
        }
        let AgentCompletion {
            owner,
            task_id,
            output,
            cleanup_error,
            approvals,
        } = completion;
        let task = self
            .agent_coordinator
            .task(&task_id)
            .context("child completion disappeared")?;
        anyhow::ensure!(
            owner.session_id == forge_provider_api::SessionId::from(self.session.id.as_str())
                && forge_provider_api::AgentId::from(task.agent_id.as_str()) == owner.agent_id,
            "child completion owner changed during approval cancellation"
        );
        if task.completed_at_ms.is_some() {
            return Ok(ForegroundAction::Wait);
        }
        anyhow::ensure!(
            self.child_exchange_runtime_by_agent
                .get(owner.agent_id.as_str())
                .is_none_or(|runtime| runtime.exchange.id == task_id),
            "late child completion cannot settle a newer child exchange"
        );
        let mut effects = Vec::new();
        for approval in approvals {
            if let Some(runtime) = self
                .child_exchange_runtime_by_agent
                .get_mut(owner.agent_id.as_str())
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
                update: Box::new(crate::provider::ApprovalUpdate::Cancelled(approval)),
            }));
        }
        let (state, next) =
            self.apply_managed_agent_completion(owner, task_id, output, cleanup_error)?;
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
        let Some(PendingAgentLaunch { work, reply }) = transaction.pending_agent_launch.pop_front()
        else {
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
                let action =
                    self.prepare_foreground_agent_completion(owner, task_id, Err(error), None)?;
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
        if !self.provider.descriptor().capability.native_steer {
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
            let message = self.agent_result_message(&task)?;
            return Ok(ForegroundAction::Effect(ForegroundJob::AgentResult {
                engine: transaction.engine.clone(),
                scope: transaction.state.scope.clone(),
                task: Box::new(task),
                message,
            }));
        }
        Ok(ForegroundAction::Wait)
    }

    /// Capture child interruptions and run approval/provider cleanup outside the broker.
    pub(crate) fn prepare_foreground_agent_interrupt(
        &mut self,
        mut targets: Vec<String>,
        request: OwnedAgentRequest,
        result: Value,
    ) -> Result<ForegroundAction> {
        if targets.is_empty() {
            let _ = request.response.send(Ok(result));
            return Ok(ForegroundAction::Wait);
        }
        let target = targets.remove(0);
        let captured = (|| -> Result<Vec<AgentInterruptItem>> {
            let mut items = Vec::new();
            let agent = self.agent_coordinator.resolve(&target)?;
            let owner = crate::provider::ExecutionOwner {
                session_id: self.session.id.clone().into(),
                agent_id: agent.id.clone().into(),
            };
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
            let approvals = self
                .permission_coordinator
                .pending_list()?
                .into_iter()
                .filter(|approval| approval.execution_owner.as_ref() == Some(&owner))
                .collect();
            items.push(AgentInterruptItem {
                agent,
                owner,
                cancelled,
                cleanup,
                provider,
                approvals,
            });
            Ok(items)
        })();
        let items = match captured {
            Ok(items) => items,
            Err(error) => {
                let _ = request.response.send(Err(error));
                return Ok(ForegroundAction::Wait);
            }
        };
        Ok(ForegroundAction::Effect(ForegroundJob::AgentInterrupt {
            coordinator: self.permission_coordinator.clone(),
            operation: AgentInterrupt {
                items,
                remaining: targets,
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
                    && authority.owner == crate::provider::ExecutionOwner::root(&self.session.id)
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
        let Some(command) = self.agent_coordinator.try_receive() else {
            return self.prepare_foreground_agent_result(transaction);
        };
        // Schedule another drain after this owned command has been staged.
        self.agent_coordinator.notify_activity();
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
                cleanup_error,
            } => self.prepare_foreground_agent_completion(owner, task_id, *output, cleanup_error),
            crate::agent::AgentCommand::Request {
                request,
                authority,
                target,
                response,
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
                let mut root = match self
                    .provider_request(ProviderInput::from_text(String::new()), PromptMode::Chat)
                {
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
                    let owner = crate::provider::ExecutionOwner {
                        session_id: self.session.id.clone().into(),
                        agent_id: agent.id.clone().into(),
                    };
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
                            owner.agent_id == forge_provider_api::AgentId::from(agent.id.as_str())
                                && owner.session_id
                                    == forge_provider_api::SessionId::from(self.session.id.as_str())
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
            crate::provider::ExecutionOwner,
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
                    owner.session_id
                        == forge_provider_api::SessionId::from(self.session.id.as_str())
                        && owner.agent_id == forge_provider_api::AgentId::from(agent.id.as_str())
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
        owned: OwnedAgentRequest,
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
                self.store
                    .admit_agent_task(&admission.agent, &admission.task, &parent)
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
        completed: usize,
        result: Result<()>,
    ) -> Result<ForegroundAction> {
        transaction.agent_command_active = false;
        let AgentInterrupt {
            items,
            remaining,
            request,
            result: reply,
        } = operation;
        let cleanup_succeeded = result.is_ok();
        let applied = (|| {
            let mut effects = Vec::new();
            for item in items.into_iter().take(completed) {
                let AgentInterruptItem {
                    mut agent,
                    owner,
                    cancelled,
                    cleanup,
                    provider,
                    approvals,
                } = item;
                anyhow::ensure!(
                    owner.session_id
                        == forge_provider_api::SessionId::from(self.session.id.as_str())
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
                for approval in approvals {
                    if let Some(runtime) = self
                        .child_exchange_runtime_by_agent
                        .get_mut(owner.agent_id.as_str())
                    {
                        runtime.exchange.observe_blocker(
                            format!("approval:{}", approval.id),
                            false,
                            self.clock.now_ms(),
                        );
                        self.store.save_exchange(&runtime.exchange)?;
                    }
                    effects.push(AppliedProviderEvent::Update(SessionUpdate::Approval {
                        update: Box::new(crate::provider::ApprovalUpdate::Cancelled(approval)),
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
                    self.agent_coordinator
                        .settled(owner.agent_id.as_str(), &task.id, false)?;
                    self.child_provider_by_agent.remove(owner.agent_id.as_str());
                    self.child_execution_by_agent
                        .remove(owner.agent_id.as_str());
                }
                effects.push(self.prepare_agent_state()?);
            }
            Ok(AppliedProviderEvent::Effects(effects))
        })();
        match applied {
            Ok(effects) => {
                let delivery = self.prepare_provider_delivery(effects)?;
                match result {
                    Ok(()) if !remaining.is_empty() => {
                        transaction.pending_agent_interrupt = Some((remaining, request, reply));
                        transaction.agent_command_active = true;
                    }
                    result => {
                        let _ = request.response.send(result.map(|()| reply));
                    }
                }
                Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    delivery,
                )))
            }
            Err(error) => {
                let _ = request.response.send(Err(error));
                Ok(ForegroundAction::Wait)
            }
        }
    }

    /// Continue a multi-child interruption after the previous result is published.
    pub(crate) fn continue_foreground_agent_interrupt(
        &mut self,
        transaction: &mut ForegroundTransaction,
    ) -> Result<ForegroundAction> {
        let Some((targets, request, reply)) = transaction.pending_agent_interrupt.take() else {
            return Ok(ForegroundAction::Wait);
        };
        let action = self.prepare_foreground_agent_interrupt(targets, request, reply)?;
        transaction.agent_command_active = matches!(
            &action,
            ForegroundAction::Effect(ForegroundJob::AgentInterrupt { .. })
        );
        Ok(action)
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
            #[cfg(test)]
            RevisionReviewRequest::Decide {
                method,
                params,
                response,
            } => {
                let validate = (|| {
                    if let Some(authority) = &transaction.task_authority {
                        authority.validate()?;
                    }
                    anyhow::ensure!(
                        transaction.pending_revision.is_some(),
                        "revision review is no longer active"
                    );
                    if let Some(id) = params.get("plan_id").and_then(Value::as_str) {
                        anyhow::ensure!(
                            self.current_plan_id()?.as_deref() == Some(id),
                            "review decision belongs to another plan"
                        );
                    }
                    Ok(())
                })();
                if let Err(error) = validate {
                    let _ = response.send(Err(error));
                    return Ok(ForegroundAction::Wait);
                }
                if matches!(method, RuntimeMethod::SessionPlanCheckUpdate) {
                    return match self.prepare_plan_checks_update(params) {
                        Ok(job) => {
                            transaction.revision_job_active = true;
                            Ok(ForegroundAction::Effect(
                                ForegroundJob::RevisionChecksUpdate {
                                    job: Box::new(job),
                                    response,
                                },
                            ))
                        }
                        Err(error) => {
                            let _ = response.send(Err(error));
                            Ok(ForegroundAction::Wait)
                        }
                    };
                }
                let edits_only = matches!(
                    method,
                    RuntimeMethod::SessionPlanEntityRename | RuntimeMethod::SessionPlanTestDelete
                );
                let result = match method {
                    RuntimeMethod::SessionPlanChangesRequest => {
                        self.dispatch_session_command("session.plan.changes.request", params)
                    }
                    RuntimeMethod::SessionPlanCancel => {
                        self.dispatch_session_command("session.plan.cancel", params)
                    }
                    RuntimeMethod::SessionPlanEntityRename => self.rename_plan_entity(params),
                    RuntimeMethod::SessionPlanTestDelete => self.delete_plan_tests(params),
                    _ => Err(anyhow::anyhow!("unsupported revision review decision")),
                };
                if edits_only || result.is_err() {
                    let _ = response.send(result);
                    return Ok(ForegroundAction::Wait);
                }
                let (_, mut events) = result?;
                transaction.state.interaction.set_awaiting_input(false);
                transaction.state.interaction.resume(self.clock.now_ms())?;
                self.store.save_exchange(&transaction.state.interaction)?;
                let execution_id = self
                    .current_execution()?
                    .context("review has no execution")?
                    .id;
                let provider_response = self.revision_review_response(&execution_id)?;
                events.retain(|event| event.event != "goal_continue_requested");
                let patch = self.reconcile_timeline()?;
                let snapshot = serde_json::to_value(self.snapshot()?)?;
                let (provider, wait) = transaction
                    .pending_revision
                    .take()
                    .context("revision review lost its provider wait")?;
                drop(wait);
                let _ = response.send(Ok((snapshot, events)));
                let _ = provider.send(Ok(provider_response));
                Ok(ForegroundAction::Effect(ForegroundJob::Publication(
                    self.prepare_provider_delivery(AppliedProviderEvent::Timeline(patch))?,
                )))
            }
        }
    }

    /// Revalidate and commit a completed revision check edit.
    #[cfg(test)]
    pub(crate) fn apply_foreground_revision_checks(
        &mut self,
        transaction: &mut ForegroundTransaction,
        response: tokio::sync::oneshot::Sender<Result<(Value, Vec<SessionEvent>)>>,
        result: Result<PlanChecksUpdateResult>,
    ) -> ForegroundAction {
        transaction.revision_job_active = false;
        let result = result.and_then(|captured| {
            if let Some(authority) = &transaction.task_authority {
                authority.validate()?;
            }
            anyhow::ensure!(
                transaction.pending_revision.is_some(),
                "revision review is no longer active"
            );
            self.commit_plan_checks_update(captured)
        });
        let _ = response.send(result);
        ForegroundAction::Wait
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

    /// Revalidate and return foreground state to the broker after the owned worker ends.
    #[cfg(test)]
    pub(crate) fn finish_owned_foreground(
        &mut self,
        transaction: &mut OwnedForegroundTransaction,
    ) -> Result<Exchange> {
        anyhow::ensure!(
            transaction.scope.owner == crate::provider::ExecutionOwner::root(&self.session.id),
            "foreground transaction crosses execution ownership"
        );
        anyhow::ensure!(
            self.exchange_runtime.is_none(),
            "another foreground runtime is already installed"
        );
        let runtime = transaction
            .runtime
            .as_ref()
            .context("foreground transaction already committed")?;
        anyhow::ensure!(
            runtime.exchange_id == transaction.interaction.id,
            "foreground runtime belongs to another exchange"
        );
        if let Some(permit) = &runtime.task_permit {
            permit.validate(self.task_controller.current()?.as_ref())?;
        }
        self.exchange_runtime = transaction.runtime.take();
        Ok(transaction.interaction.clone())
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
                AppliedProviderEvent::NativeGoalContinuation(_) => {
                    anyhow::bail!("native goal transition must finish before publication")
                }
            };
            if let Some(update) = update {
                if update.wakes_input()
                    && let Some(wakeup) = &self.input_wakeup
                {
                    wakeup.notify_one();
                }
                let payload = publication::encode_session_update(&update)?;
                self.trace
                    .record(&self.session.id, "lua.event.emitted", payload.clone());
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
        provider_event: ProviderEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<AppliedProviderEvent> {
        let scope = provider_event.scope();
        if scope.owner() != &crate::provider::ExecutionOwner::root(&self.session.id) {
            self.trace.record(
                &self.session.id,
                "provider.event.rejected",
                json!({"code":"foreign_execution_owner","scope":scope}),
            );
            return Ok(AppliedProviderEvent::None);
        }
        if scope.invocation().is_some_and(|scope| {
            scope.exchange_id != forge_provider_api::ExchangeId::from(interaction.id.as_str())
                && !interaction
                    .turn
                    .iter()
                    .any(|turn| &turn.provider().scope == scope)
        }) {
            return Ok(AppliedProviderEvent::None);
        }
        match provider_event {
            ProviderEvent::DirectoryAccess { grant, .. } => Ok(AppliedProviderEvent::Update(
                self.apply_directory_access(grant)?,
            )),
            ProviderEvent::Runtime {
                update: crate::provider::RuntimeUpdate::Resolved { runtime: resolved },
                ..
            } => Ok(AppliedProviderEvent::Update(
                self.apply_resolved_runtime(resolved)?,
            )),
            ProviderEvent::Compaction { scope, update } => {
                let observation = ProviderEvent::Compaction {
                    scope,
                    update: update.clone(),
                };
                Ok(
                    match self.apply_compaction_observation(
                        runtime,
                        interaction,
                        observation,
                        *update,
                    )? {
                        Some(patch) => AppliedProviderEvent::Timeline(patch),
                        None => AppliedProviderEvent::None,
                    },
                )
            }
            ProviderEvent::Approval { update, .. } => Ok(
                match self.apply_approval_observation(interaction, update)? {
                    Some(pending) => AppliedProviderEvent::Live(pending),
                    None => AppliedProviderEvent::None,
                },
            ),
            ProviderEvent::ParentBoundary { update, .. } => Ok(AppliedProviderEvent::Live(
                self.apply_parent_boundary(runtime, interaction, update)?,
            )),
            ProviderEvent::Agent {
                update: crate::provider::AgentUpdate::Lifecycle { event: lifecycle },
                scope,
            } => {
                self.apply_agent_lifecycle(&lifecycle, Some(&scope), event)?;
                Ok(AppliedProviderEvent::None)
            }
            ProviderEvent::Agent { .. } => Ok(AppliedProviderEvent::None),
            ProviderEvent::Diagnostic { diagnostic, .. } => {
                Ok(AppliedProviderEvent::Update(SessionUpdate::Diagnostic {
                    diagnostic,
                }))
            }
            content => self.apply_exchange_content(runtime, interaction, content, true),
        }
    }

    /// Persist directory grants and prepare the resulting session update.
    fn apply_directory_access(
        &mut self,
        grants: Vec<forge_provider_api::access::DirectoryGrant>,
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
            session: Box::new(self.session.clone()),
        })
    }

    /// Persist resolved native identity and prepare its session update.
    fn apply_resolved_runtime(
        &mut self,
        resolved: crate::provider::ProviderRuntime,
    ) -> Result<SessionUpdate> {
        if !resolved.provider.is_empty() {
            self.session.provider_label = resolved.provider;
        }
        if let Some(model) = resolved.model.filter(|model| !model.is_empty()) {
            self.session.resolved_model = Some(model);
        }
        self.save_session()?;
        Ok(SessionUpdate::Runtime {
            session_id: self.session.id.clone(),
            runtime: crate::provider::ProviderRuntime {
                provider: self.session.provider_label.clone(),
                model: self.session.resolved_model.clone(),
            },
        })
    }

    /// Apply approval ownership and return ordered publication work without awaiting delivery.
    fn apply_approval_observation(
        &mut self,
        interaction: &mut Exchange,
        update: crate::provider::ApprovalUpdate,
    ) -> Result<Option<PendingLivePublication>> {
        use crate::provider::ApprovalUpdate;
        let update = match update {
            ApprovalUpdate::Requested(request) => {
                if !self
                    .permission_coordinator
                    .pending_list()?
                    .iter()
                    .any(|approval| approval.id == request.id)
                {
                    return Ok(None);
                }
                let turn = interaction
                    .turn
                    .iter()
                    .rev()
                    .find(|turn| turn.state() == crate::turn::TurnState::Running);
                let Some(approval) = self.permission_coordinator.bind_owner(
                    &request.id,
                    &interaction.id,
                    turn.map(|turn| turn.id()),
                )?
                else {
                    return Ok(None);
                };
                interaction.observe_blocker(
                    format!("approval:{}", approval.id),
                    true,
                    self.clock.now_ms(),
                );
                ApprovalUpdate::Requested(approval)
            }
            update @ (ApprovalUpdate::Resolved(_) | ApprovalUpdate::Cancelled(_)) => {
                let approval = match &update {
                    ApprovalUpdate::Resolved(approval) | ApprovalUpdate::Cancelled(approval) => {
                        approval
                    }
                    ApprovalUpdate::Requested(_) => unreachable!(),
                };
                interaction.observe_blocker(
                    format!("approval:{}", approval.id),
                    false,
                    self.clock.now_ms(),
                );
                interaction.approval_resolution.push(approval.clone());
                update
            }
        };
        self.store.save_exchange(interaction)?;
        let timeline = self.reconcile_timeline()?;
        Ok(Some(PendingLivePublication {
            update: SessionUpdate::Approval {
                update: Box::new(update),
            },
            timeline,
        }))
    }

    /// Apply a parent wait transition and prepare its ordered publication.
    fn apply_parent_boundary(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        update: crate::provider::ParentBoundaryUpdate,
    ) -> Result<PendingLivePublication> {
        let now_ms = self.clock.now_ms();
        match update {
            crate::provider::ParentBoundaryUpdate::WaitStarted { agent_count } => {
                interaction.observe_blocker("agent_wait".into(), true, now_ms);
                interaction.close_running_messages();
                runtime.active_wait = Some(ActiveWait {
                    exchange_id: interaction.id.clone(),
                    started_at_ms: now_ms,
                    agent_count,
                });
            }
            crate::provider::ParentBoundaryUpdate::WaitUpdated { agent_count } => {
                if let Some(wait) = runtime.active_wait.as_mut() {
                    wait.agent_count = agent_count;
                }
            }
            crate::provider::ParentBoundaryUpdate::WaitFinished => {
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

    /// Persist a native turn transition and prepare its publication.
    fn apply_turn_observation(
        &mut self,
        interaction: &Exchange,
        address: &crate::provider::TurnAddress,
        update: &crate::provider::TurnUpdate,
    ) -> Result<Option<SessionUpdate>> {
        if matches!(update, crate::provider::TurnUpdate::Started { .. })
            && self.session.provider_session_id.as_deref() != Some(address.thread_id.as_str())
        {
            self.session.provider_session_id = Some(address.thread_id.clone());
            self.save_session()?;
        }
        self.store.save_exchange(interaction)?;
        Ok(SessionUpdate::from_observation(&ProviderEvent::Turn {
            address: address.clone(),
            update: update.clone(),
        }))
    }

    /// Apply ordinary content and retain ordered publication effects.
    fn apply_exchange_content(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        provider_event: ProviderEvent,
        allow_goal_transition: bool,
    ) -> Result<AppliedProviderEvent> {
        let mut effects = Vec::new();
        self.prepare_provider_observation(interaction, &provider_event);
        if provider_event.turn_address().is_some_and(|address| {
            self.observed_agent_conversations
                .iter()
                .any(|(invocation, conversation)| {
                    invocation == &address.scope.invocation_id
                        && conversation.as_str() == address.thread_id
                })
                || self
                    .agent_registry
                    .get_by_thread(&address.thread_id)
                    .is_some_and(|agent| !agent.is_primary())
        }) {
            return Ok(AppliedProviderEvent::Effects(effects));
        }
        if let Some(routed) = self.apply_settled_parent_event(interaction, &provider_event)? {
            return Ok(routed);
        }
        if allow_goal_transition
            && self.native_goal_continuation_needed(runtime, interaction, &provider_event)?
        {
            return Ok(AppliedProviderEvent::NativeGoalContinuation(provider_event));
        }
        if !interaction.observe_turn(&provider_event, self.clock.now_ms())? {
            self.trace.record(&self.session.id, "provider.event.rejected",
                    json!({"exchange_id":interaction.id,"code":"execution_unknown_or_settled","event":provider_event}));
            return Ok(AppliedProviderEvent::Effects(effects));
        }
        match &provider_event {
            ProviderEvent::Turn { address, update } => {
                let pending = self.apply_turn_observation(interaction, address, update)?;
                if let Some(update) = pending {
                    effects.push(AppliedProviderEvent::Update(update));
                }
                return Ok(AppliedProviderEvent::Effects(effects));
            }
            ProviderEvent::InputAccepted {
                acknowledgement, ..
            } => {
                runtime.retraction_eligible = false;
                let now_ms = self.clock.now_ms();
                match acknowledgement {
                    crate::provider::InputAcknowledgement::TaskStarted {
                        input_id,
                        start,
                        prompt,
                    } => {
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
                        if let Some(existing) =
                            self.store.load_session_event(&self.session.id, &id)?
                        {
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
                    crate::provider::InputAcknowledgement::Steering { input_id, text } => {
                        if input_id.as_ref().is_some_and(|id| interaction.node_list.iter().any(|node|
                                matches!(node, ExchangeNode::ExchangeInput { prompt } if prompt.id == *id))) {
                                return Ok(AppliedProviderEvent::Effects(effects));
                            }
                        interaction.observe_blocker("agent_wait".into(), false, now_ms);
                        runtime.active_wait = None;
                        interaction.append_acknowledged_input(
                            input_id.clone(),
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
                return Ok(AppliedProviderEvent::Effects(effects));
            }
            ProviderEvent::Runtime {
                update: crate::provider::RuntimeUpdate::ContextUsage { usage },
                ..
            } => {
                let thread_id = self.session.provider_session_id.as_deref();
                if let Some(thread_id) = thread_id
                        && interaction.observe_compaction_usage(thread_id, usage.used) {
                        self.store.save_exchange(interaction)?;
                        let patch = self.reconcile_timeline()?;
                        effects.push(AppliedProviderEvent::Timeline(patch));
                    } else if let Some(thread_id) = thread_id
                        && !interaction.node_list.iter().any(|node| matches!(node,
                            ExchangeNode::Compaction { compaction, .. } if compaction.thread_id == thread_id))
                        && let Some(mut record) = self.store.list_session_event(&self.session.id)?.into_iter().rev()
                            .find(|record| matches!(&record.detail, SessionEventKind::Compaction { compaction }
                                if compaction.thread_id == thread_id))
                        && let SessionEventKind::Compaction { compaction } = &mut record.detail
                        && compaction.state == crate::provider::CompactionState::Completed
                        && compaction.result.context_tokens_before.is_some()
                        && compaction.result.context_tokens_after.is_none()
                        && !self.store.list_exchange_metadata(&self.session.id)?.iter().any(|exchange|
                            exchange.node_list.iter().any(|node| matches!(node,
                                ExchangeNode::Compaction { compaction, .. }
                                    if compaction.thread_id == thread_id
                                        && compaction.started_at_ms.unwrap_or(exchange.created_at_ms) >= record.created_at_ms)))
                        && compaction.observe_context_usage(usage.used) {
                        self.store.save_session_event(&record)?;
                        let patch = self.reconcile_timeline()?;
                        effects.push(AppliedProviderEvent::Timeline(patch));
                    }
                self.session.context_usage = Some(usage.clone());
                self.context_usage_fresh = true;
                if let Some(update) = SessionUpdate::from_observation(&provider_event) {
                    effects.push(AppliedProviderEvent::Update(update));
                }
                return Ok(AppliedProviderEvent::Effects(effects));
            }
            ProviderEvent::Checklist { update, .. } => {
                runtime.retraction_eligible = false;
                runtime.checklist.replace(update);
                interaction.checklist = Some(runtime.checklist.snapshot().clone());
                self.store.save_exchange(interaction)?;
                effects.push(self.prepare_live_interaction(
                    publication::LiveUpdate::StateChanged,
                    interaction,
                )?);
            }
            ProviderEvent::Tool { .. } => runtime.retraction_eligible = false,
            ProviderEvent::Message { update, .. }
                if update.text().is_some_and(|text| !text.is_empty()) =>
            {
                runtime.retraction_eligible = false
            }
            _ => {}
        }
        if provider_event.turn_address().is_some() {
            attribute_provider_tool(interaction, &mut runtime.checklist, &provider_event)?;
            if provider_event.tool_output_delta().is_some() {
                self.store
                    .save_tool_output_delta(interaction, &provider_event)?;
            } else {
                self.store.save_exchange(interaction)?;
            }
            effects.push(self.prepare_live_interaction(provider_event, interaction)?);
        } else {
            if let Some(update) = SessionUpdate::from_observation(&provider_event) {
                effects.push(AppliedProviderEvent::Update(update));
            }
        }
        Ok(AppliedProviderEvent::Effects(effects))
    }

    fn prepare_provider_observation(
        &mut self,
        interaction: &mut Exchange,
        provider_event: &ProviderEvent,
    ) {
        if !self.capability.compaction.background
            && let ProviderEvent::Turn {
                address,
                update: crate::provider::TurnUpdate::Finished { .. },
            } = provider_event
        {
            let now_ms = provider_event.observed_at_ms(self.clock.now_ms());
            interaction.unconfirm_compaction(&address.thread_id, now_ms);
            for child in self.child_exchange_runtime_by_agent.values_mut() {
                child
                    .exchange
                    .unconfirm_compaction(&address.thread_id, now_ms);
            }
        }
    }

    /// Apply compaction state and return the timeline publication effect.
    fn apply_compaction_observation(
        &mut self,
        runtime: &mut ExchangeRuntime,
        interaction: &mut Exchange,
        provider_event: ProviderEvent,
        mut compaction: crate::provider::Compaction,
    ) -> Result<Option<TimelinePatch>> {
        if compaction.state == crate::provider::CompactionState::Running
            && self.session.provider_session_id.as_deref() == Some(compaction.thread_id.as_str())
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
            .agent_registry
            .get_by_thread(&compaction.thread_id)
            .map(|agent| agent.id.clone())
            .filter(|id| self.child_exchange_runtime_by_agent.contains_key(id))
            .or_else(|| {
                self.child_exchange_runtime_by_agent
                    .iter()
                    .find(|(_, child)| {
                        child
                            .exchange
                            .turn
                            .iter()
                            .any(|turn| turn.provider().thread_id == compaction.thread_id)
                    })
                    .map(|(id, _)| id.clone())
            });
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
        } else if self.session.provider_session_id.as_deref() == Some(compaction.thread_id.as_str())
            || interaction
                .turn
                .iter()
                .any(|turn| turn.provider().thread_id == compaction.thread_id)
        {
            runtime.retraction_eligible = false;
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
        provider_event: &ProviderEvent,
    ) -> Result<Option<AppliedProviderEvent>> {
        let Some(address) = provider_event.turn_address() else {
            return Ok(None);
        };
        if current.turn.iter().any(|turn| turn.provider() == address)
            || !matches!(
                provider_event,
                ProviderEvent::Usage { .. }
                    | ProviderEvent::Turn { .. }
                    | ProviderEvent::Tool { .. }
            )
        {
            return Ok(None);
        }
        let Some(mut owner) = self
            .store
            .load_provider_exchange(&self.session.id, address)?
        else {
            return Ok(None);
        };
        let mut effects = Vec::new();
        if matches!(provider_event, ProviderEvent::Usage { .. })
            || matches!(provider_event, ProviderEvent::Tool { address, update } if owner.accepts_settled_tool_update(address, update))
        {
            owner.observe_turn(provider_event, self.clock.now_ms())?;
            if provider_event.tool_output_delta().is_some() {
                self.store.save_tool_output_delta(&owner, provider_event)?;
            } else {
                self.store.save_exchange(&owner)?;
            }
            let status = self.live_interaction_status(Some(current))?;
            let patch = self
                .presentation
                .lock()
                .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                .update_live(
                    Some(&owner),
                    None,
                    status,
                    self.observer.status_thoughts.load(Ordering::Relaxed),
                )?;
            if let Some(update) = SessionUpdate::from_observation(provider_event) {
                effects.push(AppliedProviderEvent::Update(update));
            }
            effects.push(AppliedProviderEvent::Timeline(patch));
        }
        Ok(Some(AppliedProviderEvent::Effects(effects)))
    }

    /// Apply and deliver a settled-owner event in staged-interface tests.
    #[cfg(test)]
    async fn test_apply_settled_parent_event(
        &mut self,
        current: &Exchange,
        provider_event: &ProviderEvent,
        event: &mut Vec<SessionEvent>,
    ) -> Result<bool> {
        let Some(applied) = self.apply_settled_parent_event(current, provider_event)? else {
            return Ok(false);
        };
        let delivery = self.prepare_provider_delivery(applied)?;
        event.extend(delivery.deliver().await?);
        Ok(true)
    }

    /// Decide whether an observed start requires a native-goal checkpoint transition.
    fn native_goal_continuation_needed(
        &self,
        runtime: &ExchangeRuntime,
        interaction: &Exchange,
        observation: &ProviderEvent,
    ) -> Result<bool> {
        let ProviderEvent::Turn {
            address,
            update: crate::provider::TurnUpdate::Started { .. },
        } = observation
        else {
            return Ok(false);
        };
        let Some(previous) = interaction.turn.last() else {
            return Ok(false);
        };
        Ok(previous.provider().thread_id == address.thread_id
            && !interaction.turn.iter().any(|turn| {
                turn.provider() == address || turn.state() == crate::turn::TurnState::Running
            })
            && runtime.task_permit.as_ref().is_some_and(|permit| {
                permit
                    .validate(self.task_controller.current().ok().flatten().as_ref())
                    .is_ok()
            })
            && self
                .current_goal()?
                .is_some_and(|goal| goal.native && goal.state == GoalState::Active)
            && matches!(previous.state(), crate::turn::TurnState::Finished { .. }))
    }

    /// Capture an ordered update and timeline patch without awaiting a presentation sink.
    fn prepare_live_interaction(
        &mut self,
        update: impl Into<publication::LiveUpdate>,
        interaction: &Exchange,
    ) -> Result<AppliedProviderEvent> {
        let update = update.into();
        let incremental = if let publication::LiveUpdate::Observation(observation) = &update {
            if observation.tool_output_delta().is_some() {
                self.presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .append_tool_output(interaction, observation)?
            } else if observation.message_delta().is_some() {
                self.presentation
                    .lock()
                    .map_err(|_| anyhow::anyhow!("session presentation lock poisoned"))?
                    .update_message(interaction, observation)?
            } else {
                None
            }
        } else {
            None
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
            self.stage_interaction_for_turn(text, admit_exchange, now_ms, None)?;
        for effect in effects {
            let delivery = self.prepare_provider_delivery(effect)?;
            events.extend(delivery.deliver().await?);
        }
        Ok((interaction, admitted, events))
    }

    /// Admit a turn and stage any previous-exchange presentation work without delivery.
    fn stage_interaction_for_turn(
        &mut self,
        text: &str,
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
            let words = text.split_whitespace().collect::<Vec<_>>();
            let title = if words.first().is_some_and(|word| word.starts_with('/')) {
                words[1..].join(" ")
            } else {
                words.join(" ")
            };
            if !title.is_empty() {
                self.session.name = self
                    .store
                    .initialize_session_name(
                        &self.session.id,
                        &self.client_id,
                        &title.chars().take(30).collect::<String>(),
                    )?
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
            let outcome = if previous
                .turn
                .iter()
                .any(|turn| turn.state() == crate::turn::TurnState::Running)
            {
                crate::exchange::ExchangeOutcome::Interrupted
            } else {
                crate::exchange::ExchangeOutcome::Complete
            };
            if previous.checkpoint_before.is_some() {
                previous.begin_finalization(outcome, now_ms)?;
            } else {
                previous.finish(outcome, now_ms)?;
            }
            self.store.save_exchange(&previous)?;
            effects.push(
                self.prepare_live_interaction(publication::LiveUpdate::StateChanged, &previous)?,
            );
        }
        Ok((
            Exchange {
                finalization_error: None,
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
                approval_resolution: Vec::new(),
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
        let change = format!(
            "Removed planned tests: {}",
            selected
                .iter()
                .map(|test| format!("{}::{}", test.file, test.name))
                .collect::<Vec<_>>()
                .join(", ")
        );
        plan.manual_changes.push(change);
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
        let change = format!("Renamed {previous_name} to {new_name}");
        plan.manual_changes.push(change);
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

    #[cfg(test)]
    fn create_goal(&mut self, objective: String, native: bool) -> Result<GoalRecord> {
        let now_ms = self.clock.now_ms();
        let mut task = crate::task::Task::goal(
            objective,
            crate::session::continuation::ContinuationBudget::new(self.goal_max_turns, 5),
            native,
            now_ms,
        );
        task.request_entry("Continue the active goal".into(), now_ms)?;
        let current = self.task_controller.current()?;
        self.task_controller.change(
            &mut self.store,
            current.as_ref().map(|task| &task.revision),
            crate::task::TaskChange::Install(Box::new(task)),
            now_ms,
        )?;
        self.session.active_task = self.task_controller.current()?;
        self.active_goal()
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
            completed_turn,
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
            native: observed.native,
            created_at_ms: task.created_at_ms,
            updated_at_ms: self.clock.now_ms(),
        };
        let continuing = match decision {
            ContinuationDecision::Continue | ContinuationDecision::RetryNoProgress => {
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
            | ContinuationDecision::Stalled
            | ContinuationDecision::Settled => {
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
            let (objective, state, continuation, native) = match &task.workflow {
                crate::task::WorkflowSnapshot::Goal { goal } => (
                    goal.objective.clone(),
                    goal.state,
                    goal.continuation.clone(),
                    goal.native,
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
                    execution.native,
                ),
                crate::task::WorkflowSnapshot::Plan { .. } => return Ok(None),
            };
            return Ok(Some(GoalRecord {
                id: task.id.as_str().into(),
                session_id: self.session.id.clone(),
                objective,
                state,
                continuation,
                native,
                created_at_ms: task.created_at_ms,
                updated_at_ms: task.updated_at_ms,
            }));
        }
        Ok(None)
    }

    async fn sync_native_goal(&mut self, goal: &GoalRecord, state: GoalState) -> Result<()> {
        if !goal.native || self.session.provider_session_id.is_none() {
            return Ok(());
        }
        let registry = self.execution_registry();
        let owner = crate::provider::ExecutionOwner::root(&self.session.id);
        let expected = self.task_controller.current()?;
        let lane = self.task_controller.native_goal_lane();
        let _lane =
            match tokio::time::timeout(std::time::Duration::from_secs(30), lane.lock()).await {
                Ok(guard) => guard,
                Err(_) => {
                    self.task_controller.mark_native_goal_uncertain();
                    let error = anyhow::Error::new(ProviderError::DeliveryUncertain);
                    return Err(match registry.fence(&owner).await {
                        Ok(()) => error,
                        Err(cleanup) => error.context(cleanup),
                    });
                }
            };
        self.task_controller.ensure_native_goal_certain()?;
        anyhow::ensure!(
            foreground_failure::same_retraction_task(
                self.task_controller.current()?.as_ref(),
                expected.as_ref()
            ),
            "task changed before native goal update"
        );
        let result = match tokio::time::timeout(
            std::time::Duration::from_secs(30),
            self.provider.goal_status(
                self.provider_request(ProviderInput::from_text(""), PromptMode::GoalContinuation)?,
                crate::provider::native_goal_update(goal.objective.clone(), state),
            ),
        )
        .await
        {
            Ok(Ok(())) => Ok(()),
            Ok(Err(error)) => {
                self.task_controller.mark_native_goal_uncertain();
                Err(anyhow::Error::new(error).context(ProviderError::DeliveryUncertain))
            }
            Err(_) => {
                self.task_controller.mark_native_goal_uncertain();
                Err(ProviderError::DeliveryUncertain.into())
            }
        };
        let epoch = match self.task_controller.native_goal_written() {
            Ok(epoch) => epoch,
            Err(error) => {
                drop(_lane);
                return Err(match registry.fence(&owner).await {
                    Ok(()) => error,
                    Err(cleanup) => error.context(cleanup),
                });
            }
        };
        let changed = !foreground_failure::same_retraction_task(
            self.task_controller.current()?.as_ref(),
            expected.as_ref(),
        );
        drop(_lane);
        if let Err(uncertain) = self.task_controller.ensure_native_goal_certain() {
            let error = result.err().unwrap_or(uncertain);
            return Err(match registry.fence(&owner).await {
                Ok(()) => error,
                Err(cleanup) => error.context(cleanup),
            });
        }
        let _ = epoch;
        result?;
        if changed {
            return Err(ProviderError::DeliveryUncertain.into());
        }
        Ok(())
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
        let mut session = self.session.clone();
        if let Some(task) = self.task_controller.current()? {
            if let Some(goal) = self.current_goal()? {
                self.sync_native_goal(&goal, GoalState::Cleared).await?;
            }
            self.task_controller.change(
                &mut self.store,
                Some(&task.revision),
                crate::task::TaskChange::Clear,
                self.clock.now_ms(),
            )?;
            session.active_task = None;
        }
        let replacement = crate::provider::build(
            self.provider_launch.clone(),
            self.permission_coordinator.clone(),
            self.trace.clone(),
            self.data_root.clone(),
        )?;
        let execution_cleanup = self.execution_registry.close().await;
        let native_cleanup = self.provider.shutdown().await;
        execution_cleanup?;
        native_cleanup?;
        self.provider = replacement;
        session.provider_session_id = None;
        session.provider_checkpoint_id = None;
        session.provider_fork_state = ProviderForkState::Ready;
        session.context_usage = None;
        session.last_compaction_turn = None;
        session.acquire_lease(&self.client_id)?;
        session.updated_at_ms = self.clock.now_ms();
        self.store.record_rollback(&session, exchange_ids)?;
        self.session = session;
        self.context_usage_fresh = false;
        let exchange = self
            .store
            .load_exchange(first)?
            .filter(|exchange| exchange.session_id == self.session.id)
            .context("rolled-back interaction disappeared")?;
        Ok(vec![self.event(
            "interaction_rolled_back",
            serde_json::to_value(exchange)?,
        )?])
    }

    fn rollback_context(
        &self,
        current_exchange: &str,
        input: ProviderInput,
    ) -> Result<ProviderInput> {
        let exchanges = self.store.list_exchange(&self.session.id)?;
        if !exchanges
            .iter()
            .any(|exchange| exchange.disposition() != crate::exchange::HistoryDisposition::Current)
        {
            return Ok(input);
        }
        let retained = exchanges
            .iter()
            .filter(|exchange| {
                exchange.id != current_exchange
                    && exchange.disposition() == crate::exchange::HistoryDisposition::Current
            })
            .collect::<Vec<_>>();
        let history = serde_json::to_string(&retained)?;
        let context = format!(
            "The conversation was rolled back. Use only this retained conversation history as prior context. The current workspace files are authoritative.\n{history}\n\nCurrent request:\n"
        );
        Ok(match input {
            ProviderInput::Text { text, attachments } => ProviderInput::Text {
                text: format!("{context}{text}"),
                attachments,
            },
            ProviderInput::Skill {
                name,
                arguments,
                attachments,
            } => ProviderInput::Skill {
                name,
                arguments: format!("{context}{arguments}"),
                attachments,
            },
            ProviderInput::QuestionReply {
                reply,
                context: previous,
                attachments,
            } => ProviderInput::QuestionReply {
                reply,
                context: format!("{context}{previous}"),
                attachments,
            },
        })
    }

    #[cfg(test)]
    async fn rollback_exchange(&mut self, params: Value) -> Result<(Value, Vec<SessionEvent>)> {
        let (preview, _) = self.prepare_rollback(params).await?;
        anyhow::ensure!(
            preview["warning_count"].as_u64() == Some(0),
            "restore requires confirmation of later changes"
        );
        self.apply_rollback(json!({"preview_id":preview["preview_id"]}))
            .await
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
            self.provider.descriptor().capability.native_compact,
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
        let mut session = self
            .store
            .load_session(&session_id)?
            .context("session not found")?;
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
            vec![self.event(
                "session_changed",
                serde_json::to_value(self.session_view()?)?,
            )?],
        ))
    }

    async fn pause_current_goal(&mut self) -> Result<()> {
        let Some(goal) = self.current_goal()? else {
            return Ok(());
        };
        if goal.state != GoalState::Active {
            return Ok(());
        }
        self.sync_native_goal(&goal, GoalState::Paused).await?;
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
        if let Some(job) = committed.cleanup {
            let result = job.run().await?;
            self.commit_configuration_reconcile(result)?;
        }
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
            && self.session.provider_session_id.as_deref() == Some(compaction.thread_id.as_str())
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
        event: ProviderEvent,
    ) -> Result<Vec<SessionEvent>> {
        if let ProviderEvent::Execution { observation } = event {
            return self.observe_execution_lifecycle(observation);
        }
        let now_ms = event.observed_at_ms(self.clock.now_ms());
        if let Some(scope) = event.scope().invocation()
            && (self.reopened_execution.active(scope)
                || matches!(
                    event,
                    ProviderEvent::Usage { .. } | ProviderEvent::Message { .. }
                ))
        {
            anyhow::ensure!(
                scope.owner == crate::provider::ExecutionOwner::root(&self.session.id),
                "late output crosses root ownership"
            );
            if let Some(mut owner) = self.store.load_exchange(scope.exchange_id.as_str())?
                && owner.observe_turn(&event, now_ms)?
            {
                self.store.save_exchange(&owner)?;
                let patch = self.reconcile_timeline()?;
                return Ok(if patch.is_empty() {
                    Vec::new()
                } else {
                    vec![self.event("document_changed", patch.notification())?]
                });
            }
            return Ok(Vec::new());
        }
        if let ProviderEvent::Tool { address, update } = &event {
            anyhow::ensure!(
                address.turn.scope.owner == crate::provider::ExecutionOwner::root(&self.session.id),
                "session tool event crosses execution ownership"
            );
            let Some(mut owner) = self
                .store
                .load_provider_exchange(&self.session.id, &address.turn)?
            else {
                return Ok(Vec::new());
            };
            if !owner.accepts_settled_tool_update(address, update) {
                return Ok(Vec::new());
            }
            if !owner.observe_turn(&event, now_ms)? {
                return Ok(Vec::new());
            }
            if event.tool_output_delta().is_some() {
                self.store.save_tool_output_delta(&owner, &event)?;
            } else {
                self.store.save_exchange(&owner)?;
            }
            let patch = self.reconcile_timeline()?;
            return Ok(if patch.is_empty() {
                Vec::new()
            } else {
                vec![self.event("document_changed", patch.notification())?]
            });
        }
        let ProviderEvent::Compaction {
            scope,
            update: compaction,
        } = event
        else {
            return Ok(match SessionUpdate::from_observation(&event) {
                Some(update) => vec![self.event(
                    "provider_event",
                    publication::encode_session_update(&update)?,
                )?],
                None => Vec::new(),
            });
        };
        anyhow::ensure!(
            scope.owner().session_id
                == forge_provider_api::SessionId::from(self.session.id.as_str()),
            "compaction crosses session ownership"
        );
        self.retain_external_compaction(*compaction, now_ms)?;
        let patch = self.reconcile_timeline()?;
        Ok(if patch.is_empty() {
            Vec::new()
        } else {
            vec![self.event("document_changed", patch.notification())?]
        })
    }

    /// Preserve the local driver's compatibility entry while sharing owned compaction settlement.
    async fn compact_session(&mut self, manual: bool) -> Result<(Value, Vec<SessionEvent>)> {
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
        let result = job.run().await;
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
    fn provider_request(&self, input: ProviderInput, mode: PromptMode) -> Result<ProviderRequest> {
        let mut request = self.input_submission_request(input, mode)?;
        if !self.submitted_attachments.is_empty() {
            request.input = request
                .input
                .with_attachments(self.submitted_attachments.clone());
        }
        if self.question_discussion {
            request.access.sandbox = true;
            request.access.write_access = crate::session::WriteScope::Off;
            request.access.writable_directory.clear();
        }
        request.control_context = self.control_turn_context(request.mode);
        if let Some(selection) = &self.question_model {
            request.model = selection
                .model
                .clone()
                .unwrap_or_else(|| self.session.model.clone());
            request.effort = selection.effort.clone().unwrap_or_default();
            request.service_tier = selection.service_tier.unwrap_or_default();
            request.context_window = None;
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
        let enabled = params
            .get("enabled")
            .and_then(Value::as_bool)
            .context("trace enabled is required")?;
        Ok((
            serde_json::to_value(self.trace.configure(enabled)?)?,
            Vec::new(),
        ))
    }
}

impl Drop for HarnessBroker {
    fn drop(&mut self) {
        let _ = self.release_lease();
    }
}

fn attribute_provider_tool(
    exchange: &mut Exchange,
    checklist: &mut ChecklistTracker,
    event: &ProviderEvent,
) -> Result<()> {
    let ProviderEvent::Tool { address, .. } = event else {
        return Ok(());
    };
    let Some(checklist_item_id) = checklist.attribution_target().map(str::to_owned) else {
        return Ok(());
    };
    let turn_id = exchange.turn_mut(&address.turn)?.id().to_owned();
    if exchange.attribute_tool(&turn_id, address.tool_id.as_str(), &checklist_item_id)? {
        checklist.mark_attributed(&checklist_item_id);
        exchange.checklist = Some(checklist.snapshot().clone());
    }
    Ok(())
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
    model_setting.insert(
        session.model.clone(),
        ModelSetting {
            reasoning: Some(session.effort.clone()),
            context_window: session.context_window.clone(),
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

pub use forge_provider_api::error::TurnCancelled;

#[cfg(test)]
mod test {
    /// A canceled native owner cannot hold admission forever without a terminal.
    #[tokio::test(start_paused = true)]
    async fn native_cancellation_deadline_expires_without_terminal() {
        let cancellation = super::TurnCancellation::new();
        cancellation.request_native_cancel(false);
        let expired = cancellation.native_cancel_expired();
        tokio::pin!(expired);
        assert!(futures_util::poll!(&mut expired).is_pending());
        tokio::time::advance(std::time::Duration::from_secs(30)).await;
        expired.await;
    }

    include!("workflow_test.rs");

    fn completed_message(
        turn: &crate::provider::TurnAddress,
        id: impl Into<String>,
        role: crate::provider::MessageRole,
        text: impl Into<String>,
    ) -> Vec<ProviderEvent> {
        crate::provider::test_support::message(turn, id, Some(role), text)
    }

    fn compaction_event(
        session_id: &str,
        compaction: crate::provider::Compaction,
    ) -> ProviderEvent {
        ProviderEvent::Compaction {
            scope: crate::provider::EventScope::SessionOperation {
                owner: crate::provider::ExecutionOwner::root(session_id),
                operation_id: compaction.id.clone(),
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
        connection
            .execute_batch("ALTER TABLE prompt_history_record RENAME TO unavailable_history")?;
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
        let fixture =
            crate::provider::test_support::EventFixtureProvider::new(
                descriptor.clone(),
                move |request, sink, _cancelled| {
                    let turns = turns.clone();
                    let workspace = workspace.clone();
                    let descriptor = descriptor.clone();
                    async move {
                        let turn = turns.fetch_add(1, Ordering::SeqCst);
                        if turn == 0 || repair {
                            std::fs::write(
                                workspace.join("seed.txt"),
                                if turn > 0 { "fixed" } else { "broken" },
                            )
                            .map_err(|_| {
                                crate::provider::ProviderError::Failure(
                                    forge_provider_api::ProviderFailure::new(
                                        forge_provider_api::ProviderOperation::NativeTransport,
                                        forge_provider_api::ProviderFailureKind::Unknown,
                                    ),
                                )
                            })?;
                        }
                        if turn == 0 && explicit {
                            assert!(request.tools.iter().any(|tool| tool.name == "check_run"));
                            assert!(!request.tools.iter().any(
                                |tool| tool.name.starts_with("check_")
                                    && tool.name != "check_run"
                                    && tool.name != "check_status"
                            ));
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
                                    result:
                                        forge_provider_api::lifecycle::ExecutionResult::Completed {
                                            completion:
                                                forge_provider_api::completion::ProviderCompletion {
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
        broker.observer.completion_queue.update(1, true)?;
        broker
            .run_interaction(
                "Queued edit".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("Queued edit".into())),
            )
            .await?;
        assert!(broker.snapshot()?.checks_deferred);
        broker.observer.completion_queue.update(2, false)?;
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

    fn checkpoint_fixture(
        workspace: PathBuf,
        preparation: Option<Arc<PreparationProbe>>,
        started: Arc<tokio::sync::Notify>,
        tool_ran: Arc<std::sync::atomic::AtomicBool>,
        active: Arc<std::sync::atomic::AtomicUsize>,
        native_rejection: Option<Arc<PreparationProbe>>,
        cleanup_error: bool,
    ) -> Result<crate::provider::test_support::ProviderTestSession> {
        struct ActiveProbe(Arc<std::sync::atomic::AtomicUsize>);
        impl Drop for ActiveProbe {
            fn drop(&mut self) {
                self.0.fetch_sub(1, Ordering::SeqCst);
            }
        }
        let descriptor = crate::provider::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("registry supplies a nonselectable fixture identity"),
            label: "Normalized checkpoint fixture".into(),
            capability: crate::provider::test_support::fixture_capability(),
        };
        let fixture = crate::provider::test_support::EventFixtureProvider::new(
            descriptor.clone(),
            {
                let started = started.clone();
                let tool_ran = tool_ran.clone();
                let active = active.clone();
                move |request, sink, _cancelled| {
                    let started = started.clone();
                    let tool_ran = tool_ran.clone();
                    let active = active.clone();
                    let workspace = workspace.clone();
                    let descriptor = descriptor.clone();
                    async move {
                        active.fetch_add(1, Ordering::SeqCst);
                        let _active = ActiveProbe(active);
                        let scope = request.scope.clone();
                        let address = crate::provider::TurnAddress {
                            scope: scope.clone(),
                            thread_id: "checkpoint-thread".into(),
                            turn_id: "checkpoint-turn".into(),
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
                            update: crate::provider::TurnUpdate::Started { started_at_ms: 0 },
                        })
                        .await?;
                        started.notify_one();
                        if let Err(error) = request.turn_control.before_send(&scope).await {
                            if matches!(error, crate::provider::ProviderError::Cancelled) {
                                sink.send_wait(ProviderEvent::Turn {
                                    address: address.clone(),
                                    update: crate::provider::TurnUpdate::Finished {
                                        outcome: forge_provider_api::turn::TurnOutcome::Cancelled,
                                        completed_at_ms: 1,
                                    },
                                })
                                .await?;
                                sink.send_wait(ProviderEvent::Execution { observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                                scope, forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                                    address, covered_submissions: vec![request.submission_id], completed_at_ms: 1,
                                    result: forge_provider_api::lifecycle::ExecutionResult::Cancelled { completion: forge_provider_api::completion::ProviderCompletion {
                                        capability: descriptor.capability.clone(), ..Default::default()
                                    }.into() },
                                },
                            )? }).await?;
                                return Ok(());
                            }
                            return Err(error);
                        }
                        tool_ran.store(true, Ordering::SeqCst);
                        std::fs::write(workspace.join("seed.txt"), "tool changed workspace\n")
                            .map_err(|_| {
                                crate::provider::ProviderError::Failure(
                                    forge_provider_api::ProviderFailure::new(
                                        forge_provider_api::ProviderOperation::NativeTransport,
                                        forge_provider_api::ProviderFailureKind::Unknown,
                                    ),
                                )
                            })?;
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
                                    result:
                                        forge_provider_api::lifecycle::ExecutionResult::Completed {
                                            completion:
                                                forge_provider_api::completion::ProviderCompletion {
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
            },
        );
        let fixture = fixture.with_pre_dispatch(move |_request| {
            let preparation = preparation.clone();
            let native_rejection = native_rejection.clone();
            async move {
                let (probe, kind) = match (preparation, native_rejection) {
                    (Some(probe), _) => (
                        Some(probe),
                        Some(forge_provider_api::ProviderFailureKind::Runtime),
                    ),
                    (_, Some(probe)) => (
                        Some(probe),
                        Some(forge_provider_api::ProviderFailureKind::Rejected),
                    ),
                    _ => (None, None),
                };
                if let (Some(probe), Some(kind)) = (probe, kind) {
                    probe.started.notify_one();
                    probe.release.notified().await;
                    if probe.fail || kind == forge_provider_api::ProviderFailureKind::Rejected {
                        return Err(crate::provider::ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::AdmitInput,
                                kind,
                            ),
                        ));
                    }
                }
                Ok(())
            }
        });
        let fixture = if cleanup_error {
            fixture.with_cleanup(|_| async {
                Err(crate::provider::ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::CleanupExecution,
                        forge_provider_api::ProviderFailureKind::Transport,
                    ),
                ))
            })
        } else {
            fixture
        };
        crate::provider::test_support::ProviderTestSession::from_native(fixture)
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
            let query =
                broker.provider_request(ProviderInput::from_text(prompt), PromptMode::Chat)?;
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
        cancellation.arm(true);
        let cancel = async {
            started.notified().await;
            cancellation.request(true);
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
            let result = result.context(
                "native cleanup failure left the denied baseline waiting for external Git",
            )?;
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
            if failure.operation() == forge_provider_api::ProviderOperation::CleanupExecution
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
    async fn cancellation_rewinds_before_checkpoint_barrier_without_waiting_for_scan() -> Result<()>
    {
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
            cancellation.arm(true);
            let cancel = async {
                started.notified().await;
                cancellation.request(true);
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
                    cancellation.requested.load(Ordering::Acquire),
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
    async fn native_abort_completion_rewinds_before_pending_checkpoint_finalization() -> Result<()>
    {
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
            cancellation.arm(true);
            let session_id = broker.session.id.clone();
            let cancel = async {
                signals.started.notified().await;
                if cleanup_before_completion {
                    cancellation.begin_cleanup(restore_prompt)?;
                    provider.cleanup_execution(&session_id).await?;
                } else {
                    signals.complete.notify_one();
                    signals.settled.notified().await;
                    tokio::task::yield_now().await;
                    cancellation.begin_cleanup(restore_prompt)?;
                }
                cancellation.request(restore_prompt);
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
                cancellation.requested.load(Ordering::Acquire),
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
        broker
            .store
            .save_task_operation(&mut crate::task::TaskOperation {
            revision: 0,
            id: "pause-before-close".into(),
            session_id: session_id.clone(),
            action: json!({"action":"pause"}),
            permission_selection: None,
            state: "outcome_unknown".into(),
            error: Some(
                "Runtime stopped before the operation settled. Inspect task state before resuming"
                    .into(),
            ),
            exchange_id: Some(interrupted.id.clone()),
            created_at_ms: 3,
        })?;
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
        let pause = reopened
            .store
            .load_task_operation(&session_id, "pause-before-close")?
            .unwrap();
        assert_eq!(pause.state, "cancelled");
        assert!(pause.error.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn initial_checkpoint_failure_can_finalize_without_replaying_provider_work() -> Result<()>
    {
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

    use crate::agent::AgentExecutionState;
    use crate::session::state_machine::SessionPhase;
    include!("agent_test.rs");
    include!("child_cancellation_test.rs");
    include!("child_background_test.rs");
    include!("input_test.rs");
    include!("admission_test.rs");
    async fn settle_managed_children(broker: &mut HarnessBroker) -> Result<Vec<SessionEvent>> {
        let mut revision = broker.agent_coordinator.subscribe();
        let mut event = Vec::new();
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            loop {
                event.extend(broker.drain_agent_commands(None).await?);
                if broker.agent_coordinator.active_count() == 0 {
                    return Ok::<_, anyhow::Error>(());
                }
                revision.changed().await.context("child lane closed")?;
            }
        })
        .await
        .context("managed child did not settle within five seconds")??;
        Ok(event)
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
        let provider =
            crate::provider::test_support::EventFixtureProvider::new(descriptor.clone(), {
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
                                    result:
                                        forge_provider_api::lifecycle::ExecutionResult::Completed {
                                            completion:
                                                forge_provider_api::completion::ProviderCompletion {
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
        let provider = provider.with_active_session_id(|_| async {
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
                        .apply_configuration(
                            action,
                            configuration::ConfigurationCommand::Permission,
                        )
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
        let goal = broker.create_goal("Persist goal".into(), false)?;
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
        let goal = broker.create_goal("Persist goal".into(), false).unwrap();
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
    fn completed_clear_receipt_survives_runtime_recovery() {
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
        let receipt = crate::task::TaskOperation {
            revision: 0,
            id: "clear-before-shutdown".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"clear"}),
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
        };
        let task = broker.task_controller.current().unwrap().unwrap();
        broker
            .task_controller
            .command(
                &mut broker.store,
                &receipt,
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
        let operation = broker
            .store
            .load_task_operation(&broker.session.id, "clear-before-shutdown")
            .unwrap()
            .unwrap();
        assert_eq!(operation.state, "completed");
        assert!(broker.store.list_session_event(&broker.session.id).unwrap().iter().any(|event|
            matches!(&event.detail, SessionEventKind::TaskFinished { outcome } if outcome.title == "Interrupted")));
    }

    #[test]
    fn completed_task_receipt_deduplicates_without_repeating_the_mutation() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker
            .create_task(
                crate::task::TaskKind::Goal,
                "Retain task".into(),
                None,
                None,
            )
            .unwrap();
        let mut receipt = crate::task::TaskOperation {
            revision: 0,
            id: "original".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"pause"}),
            permission_selection: None,
            state: "completed".into(),
            error: None,
            exchange_id: None,
            created_at_ms: 1,
        };
        let admitted = broker
            .task_controller
            .current()
            .unwrap()
            .unwrap()
            .revision
            .clone();
        broker
            .task_controller
            .command(
                &mut broker.store,
                &receipt,
                Some(&admitted),
                crate::task::TaskChange::Pause,
                1,
            )
            .unwrap();
        let revision = broker
            .task_controller
            .current()
            .unwrap()
            .unwrap()
            .revision
            .clone();
        broker
            .task_controller
            .command(
                &mut broker.store,
                &receipt,
                Some(&admitted),
                crate::task::TaskChange::Pause,
                2,
            )
            .unwrap();
        assert_eq!(
            broker.task_controller.current().unwrap().unwrap().revision,
            revision
        );
        receipt.action = json!({"action":"clear"});
        assert!(
            broker
                .task_controller
                .command(
                    &mut broker.store,
                    &receipt,
                    Some(&revision),
                    crate::task::TaskChange::Clear,
                    3
                )
                .is_err()
        );
        let receipt = broker
            .store
            .load_task_operation(&broker.session.id, "original")
            .unwrap()
            .unwrap();
        assert_eq!(receipt.state, "completed");
        assert!(
            broker
                .store
                .list_exchange(&broker.session.id)
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn task_failure_remains_inspectable_without_following_a_new_exchange() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"failed-exchange", "session_id":broker.session.id, "agent_id":broker.session.primary_agent_id, "ordinal":1,
            "prompt":"first", "state":"failed", "created_at_ms":1, "completed_at_ms":2,
            "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        broker.store.save_exchange(&exchange).unwrap();
        let operation = crate::task::TaskOperation {
            revision: 0,
            id: "failed-operation".into(),
            session_id: broker.session.id.clone(),
            action: json!({"action":"plan"}),
            permission_selection: None,
            state: "failed".into(),
            error: Some("provider failed".into()),
            exchange_id: Some(exchange.id.clone()),
            created_at_ms: 1,
        };
        broker.store.admit_task_operation(&operation).unwrap();
        assert_eq!(
            broker.snapshot().unwrap().task_operation.unwrap()["error"],
            "provider failed"
        );
        exchange.id = "next-exchange".into();
        exchange.ordinal = 2;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            ExchangeState::Complete,
            exchange.completed_at_ms(),
            exchange.finalization_outcome(),
        );
        exchange.created_at_ms = 3;
        exchange.phase = crate::exchange::ExchangePhase::fixture(
            exchange.state(),
            Some(4),
            exchange.finalization_outcome(),
        );
        broker.store.save_exchange(&exchange).unwrap();
        assert!(broker.snapshot().unwrap().task_operation.is_none());
        assert_eq!(
            broker
                .store
                .load_task_operation(&broker.session.id, &operation.id)
                .unwrap()
                .unwrap()
                .error,
            Some("provider failed".into())
        );
    }

    use super::*;

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

    async fn check_fixture(
        repository: &Path,
        data: &Path,
        commands: Vec<String>,
        manual: bool,
    ) -> HarnessBroker {
        let mut broker = planning_question_broker(repository, data, false);
        let mut document = crate::plan::test_fixture("check-plan", "Check fixture");
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Check accepted commands".into();
        design.document.background = "A repository fixture".into();
        design.document.design = "Harness owns the command gate".into();
        let config = crate::checks::CheckConfig {
            version: 2,
            definitions: commands
                .into_iter()
                .enumerate()
                .map(|(index, command)| crate::checks::CheckDefinition {
                    auto: true,
                    id: format!("check-{index}"),
                    name: format!("Check {index}"),
                    category: crate::checks::CheckCategory::Check,
                    purpose: crate::checks::CheckPurpose::Verify,
                    enabled: true,
                    area: "Fixture".into(),
                    trigger: crate::checks::CheckTrigger::AnyChange,
                    scope: Default::default(),
                    directory: ".".into(),
                    all: crate::checks::CheckOperation {
                        input: false,
                        args: None,
                        inputs: None,
                        command,
                    },
                    files: None,
                    cases: None,
                    depends_on: Vec::new(),
                })
                .collect(),
        };
        std::fs::write(
            repository.join(".forge.json"),
            serde_json::to_vec(&json!({"checks":config})).unwrap(),
        )
        .unwrap();
        design.document.verification = if manual {
            vec!["Observe manual result".into()]
        } else {
            Vec::new()
        };
        design
            .proposed
            .insert(".gitignore".into(), "/target/\n".into());
        document.design = Some(design);
        let mut plan = broker
            .plan_file
            .prepare_entry(&broker.session.id, "Run checks".into(), Some(document), 1)
            .unwrap()
            .record;
        let (_, _, accepted_digest) = broker
            .plan_file
            .submit_document_revision(&broker.session.id, "check-plan", 1, 1)
            .unwrap();
        plan.state = PlanState::Accepted;
        plan.model_revision = 1;
        plan.accepted_revision = Some(1);
        plan.review_digest = Some(accepted_digest.clone());
        plan.accepted_digest = Some(accepted_digest);
        plan.submitted_version = Some(plan.document_version);
        broker.store.save_plan(&plan).unwrap();
        std::fs::write(repository.join(".gitignore"), "/target/\n").unwrap();
        let goal = GoalRecord {
            id: "check-goal".into(),
            session_id: broker.session.id.clone(),
            objective: "Run checks".into(),
            state: GoalState::Active,
            continuation: Default::default(),
            native: false,
            created_at_ms: 1,
            updated_at_ms: 1,
        };
        broker.save_session().unwrap();
        let baseline = broker.prepare_workspace_capture().run().await.unwrap();
        broker.store.save_checkpoint(&baseline).unwrap();
        std::fs::write(
            repository.join(".gate-input"),
            "Exercise the automated gate\n",
        )
        .unwrap();
        let execution:PlanExecutionRecord=serde_json::from_value(json!({
            "baseline_checkpoint":baseline.id,
            "manual_verification":if manual { vec!["Observe manual result"] } else { Vec::new() },
            "id":"check-execution","session_id":broker.session.id,"plan_id":"check-plan","goal_id":goal.id,
            "state":"active","phase":"checking","original_revision":1,"revision":1,"generation":0,
            "lifecycle":[],"findings":[],"progress":crate::plan::execution::SemanticProgress::default(),
            "verification":[],"check_runs":[],"revision_history":[],"resume_after_review":false,
            "observed_source":{},"observed_document_version":0,"observed_check":[],"created_at_ms":1
        })).unwrap();
        broker.store.save_plan_execution(&execution).unwrap();
        let task = crate::task::Task::execute(
            "Run checks".into(),
            execution,
            crate::session::continuation::ContinuationBudget::new(20, 2),
            false,
            1,
        )
        .unwrap();
        broker
            .task_controller
            .change(
                &mut broker.store,
                None,
                crate::task::TaskChange::Install(Box::new(task)),
                1,
            )
            .unwrap();
        save_check_exchange(&mut broker, "check-exchange", true).unwrap();
        broker
    }

    #[cfg(windows)]
    fn update_check_execution(broker: &mut HarnessBroker, record: PlanExecutionRecord) {
        let task = broker.task_controller.current().unwrap().unwrap();
        broker
            .task_controller
            .update(&mut broker.store, &task.permit(), 100, |task| {
                let crate::task::WorkflowSnapshot::Execute { execution } = &mut task.workflow
                else {
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
                "Write-Output 'accepted stdout'; [Console]::Error.WriteLine('accepted stderr')"
                    .into(),
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
                cancellation.request(false);
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
            broker.turn_cancellation.arm(false);
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
        assert!(
            broker
                .store
                .load_task_operation(&broker.session.id, "retired-cancel")
                .unwrap()
                .is_none()
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
        let selected_task =
            serde_json::to_value(broker.task_controller.current().unwrap()).unwrap();
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
        broker.provider = semantic_execution_provider(
            &broker,
            plan.id.clone(),
            u64::from(plan.model_revision + 1),
        );
        let accepted = broker.dispatch(Request { id: 3, method: "session.plan.accept".into(),
            params: json!({"plan_id":plan.id,"digest":draft.review_digest,"approval_mode":"ask"}) }).await;
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
        native: std::sync::Arc<dyn forge_provider_api::provider::Provider>,
    ) -> Arc<Provider> {
        Arc::new(Provider::new(
            native,
            broker.permission_coordinator.clone(),
            broker.trace.clone(),
            broker.data_root.clone(),
        ))
    }

    fn stopped_execution_provider(broker: &HarnessBroker, cancelled: bool) -> Arc<Provider> {
        let descriptor = forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
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
                let sink = sink.invocation_sink(&request.scope)?;
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
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
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
                    let sink = sink.invocation_sink(&request.scope)?;
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
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
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
                    let sink = sink.invocation_sink(&scope)?;
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
                            .find(|item| {
                                item.get("id").and_then(Value::as_str) == Some(id.as_str())
                            })
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
            label: "Event delivery fixture".into(),
            capability: Default::default(),
        };
        let cleanup_cancelled = cancelled.clone();
        let native = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, sink, mut cancellation| {
                let cancelled = cancelled.clone();
                async move {
                    use forge_provider_api::lifecycle::{ExecutionObservation, ExecutionResult, ExecutionUpdate};
                    use forge_provider_api::message::{MessageClassification, MessageId, MessageRole, MessageUpdate};
                    use forge_provider_api::{MessageAddress, ProviderEvent, TurnAddress};

                    let turn = TurnAddress {
                        scope: request.scope.clone(),
                        thread_id: "delivery-fixture-session".into(),
                        turn_id: "delivery-fixture-turn".into(),
                    };
                    let sink = sink.invocation_sink(&request.scope)?;
                    sink.send_wait(ProviderEvent::Execution {
                        observation: ExecutionObservation::new(
                            request.scope.clone(), ExecutionUpdate::Started { address: turn.clone() },
                        )?,
                    }).await?;
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
                        let event = ProviderEvent::Message { address: address.clone(), update };
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
                        }).await?;
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
                                        completion: forge_provider_api::completion::ProviderCompletion::default().into(),
                                    },
                                },
                            )?,
                        }).await?;
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
            let fixture_provider =
                saturating_provider(&broker, completed, false, cancelled.clone());
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
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
            label: "Cleanup lifecycle fixture".into(),
            capability: Default::default(),
        };
        let signals = Arc::new(CleanupSignals {
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
                    let sink = sink.invocation_sink(&request.scope)?;
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion::default(
                                        )
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
                            forge_provider_api::ProviderOperation::CleanupExecution,
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
            .create_goal("Complete implementation".into(), false)
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
        broker
            .create_goal("Finish implementation".into(), false)
            .unwrap();
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
        let mut exchange =
            completed_interaction("locked-checkpoint", &broker.session.id, Vec::new());
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
    async fn cleanup_failure_retains_provider_delivery_until_retry_settles_execution() {
        let repository = tempfile::tempdir().unwrap();
        let data = tempfile::tempdir().unwrap();
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let (provider, signals) = cleanup_retry_provider(&broker, false, true);
        broker.provider = provider.clone();
        let cancellation = broker.turn_cancellation();
        let session_id = broker.session.id.clone();
        let control = async {
            signals.started.notified().await;
            cancellation.begin_cleanup(false).unwrap();
            let error = provider.cleanup_execution(&session_id).await.unwrap_err();
            cancellation.fail_cleanup(&error);
            tokio::time::timeout(std::time::Duration::from_secs(2), async {
                loop {
                    let store = SqliteStore::open(data.path()).unwrap();
                    let exchange = store.list_exchange(&session_id).unwrap();
                    if exchange[0].state() == ExchangeState::Finalizing
                        && exchange[0].finalization_error.is_some()
                    {
                        assert!(exchange[0].checkpoint_after.is_none());
                        break;
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await
            .expect("cleanup failure should persist without losing provider delivery");
            cancellation.begin_cleanup(false).unwrap();
            provider.cleanup_execution(&session_id).await.unwrap();
            cancellation.request(false);
        };
        let execution = broker.run_interaction(
            "Stop this request".into(),
            PromptMode::Chat,
            Some(ExchangeAdmission::chat("Stop this request".into())),
        );
        let (result, ()) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::join!(execution, control)
        })
        .await
        .expect("retry should settle the original execution");
        assert!(result.is_err());
        let exchange = broker.store.list_exchange(&session_id).unwrap();
        assert_eq!(exchange.len(), 1);
        assert_eq!(exchange[0].state(), ExchangeState::Cancelled);
        assert!(exchange[0].finalization_error.is_none());
        assert_eq!(
            exchange[0].turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
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
    async fn conversation_rollback_preserves_files_and_restarts_with_retained_context() -> Result<()>
    {
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
            !broker.snapshot()?.timeline.iter().any(
                |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == "removed")
            )
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
        assert!(visible.iter().any(
            |entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange_id[0])
        ));
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
                    std::fs::create_dir_all(nested.parent().expect("nested path has parent"))
                        .map_err(|_| {
                            crate::provider::ProviderError::Failure(
                                forge_provider_api::ProviderFailure::new(
                                    forge_provider_api::ProviderOperation::NativeTransport,
                                    forge_provider_api::ProviderFailureKind::Unknown,
                                ),
                            )
                        })?;
                    std::fs::write(&nested, "written before provider failure\n").map_err(|_| {
                        crate::provider::ProviderError::Failure(
                            forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::NativeTransport,
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
                                            capability: descriptor.capability.clone(),
                                            ..Default::default()
                                        }
                                        .into(),
                                    failure:
                                        forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                                            kind:
                                                forge_provider_api::ProviderFailureKind::Transport,
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

    fn invalid_control_output_fixture() -> Result<crate::provider::test_support::ProviderTestSession>
    {
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
                        update: crate::provider::ParentBoundaryUpdate::WaitStarted {
                            agent_count: 1,
                        },
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
                    forge_provider_api::ProviderOperation::AdmitInput,
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
                        forge_provider_api::ProviderOperation::AdmitInput,
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

    fn plan_fixture_provider(
        broker: &HarnessBroker,
        behavior: PlanFixtureBehavior,
    ) -> Arc<Provider> {
        let descriptor = forge_provider_api::ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
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
                    let sink = sink.invocation_sink(&scope)?;
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
        let (_, events) = broker.compact_session(true).await.unwrap();
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

    pub(super) fn repository() -> tempfile::TempDir {
        let temporary = tempfile::tempdir().unwrap();
        git(temporary.path(), &["init", "-q"]);
        git(temporary.path(), &["config", "core.autocrlf", "false"]);
        git(
            temporary.path(),
            &["config", "user.email", "harness@example.invalid"],
        );
        git(temporary.path(), &["config", "user.name", "Harness Test"]);
        std::fs::write(temporary.path().join("seed.txt"), "seed\n").unwrap();
        git(temporary.path(), &["add", "."]);
        git(temporary.path(), &["commit", "-qm", "seed"]);
        temporary
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
        let native: Arc<dyn forge_provider_api::provider::Provider> = mutable_question_fixture();
        broker.provider = Arc::new(crate::provider::Provider::new(
            native,
            broker.permission_coordinator.clone(),
            broker.trace.clone(),
            broker.data_root.clone(),
        ));
        broker
    }

    pub(super) fn planning_question_broker(
        repository: &Path,
        data_root: &Path,
        structured: bool,
    ) -> HarnessBroker {
        crate::checks::configure_fixture(repository);
        let mut broker = HarnessBroker::initialize_with_clock(
            InitializeRequest {
                background: None,
                provider_launch_by_provider: Default::default(),
                config_dir: None,
                data_root: data_root.to_string_lossy().into_owned(),
                permission_file: None,
                workspace: repository.to_string_lossy().into_owned(),
                client_id: "planning-question-client".into(),
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
        broker.provider =
            plan_fixture_provider(&broker, PlanFixtureBehavior::Questions { structured });
        broker
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
        assert!(request.access.sandbox);
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
            parent_agent_id: Some(
                forge_provider_api::agent::AgentId::new("observed-parent").unwrap(),
            ),
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
    async fn background_tool_completes_on_settled_owner_without_touching_current_turn() -> Result<()>
    {
        use crate::provider::{
            TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate,
        };

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
                            message: forge_provider_api::SanitizedMessage::new(
                                "late contradiction"
                            )
                            .expect("host diagnostic is nonempty"),
                        }
                    },
                }
            )
        );
        Ok(())
    }

    #[tokio::test]
    async fn late_tool_title_updates_the_settled_owner_without_touching_current_turn() -> Result<()>
    {
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
        let current_provider =
            crate::turn::test_support::address(&current, "current-provider-turn");
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
        let mut exchange =
            completed_interaction("runtime-identity", &broker.session.id, Vec::new());
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
                crate::provider::ProviderRuntime {
                    provider: "Fixture Provider".into(),
                    model: Some("resolved-model".into()),
                }
                .resolved_event(fixture_scope),
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

    /// A continuation queued during finalization settles the original exchange before checkpoint commit.
    #[tokio::test]
    async fn native_continuation_invalidates_pending_settlement() -> Result<()> {
        use forge_provider_api::lifecycle::{
            ExecutionObservation, ExecutionResult, ExecutionUpdate,
        };
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        broker.checkpoint = None;
        let mut exchange = completed_interaction("continued", &broker.session.id, Vec::new());
        exchange.phase = crate::exchange::ExchangePhase::Running;
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ExecutionOwner::root(&broker.session.id),
            exchange.id.clone(),
        );
        let first = crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "native".into(),
            turn_id: "first".into(),
        };
        exchange.start_turn(first.clone(), 10)?;
        exchange
            .turn_mut(&first)
            .unwrap()
            .finish(crate::turn::TurnOutcome::Completed, 20)?;
        broker.store.save_exchange(&exchange)?;
        let next = crate::provider::TurnAddress {
            scope: scope.clone(),
            thread_id: "native".into(),
            turn_id: "second".into(),
        };
        for update in [
            ExecutionUpdate::Started {
                address: next.clone(),
            },
            ExecutionUpdate::Finished {
                address: next,
                covered_submissions: Vec::new(),
                completed_at_ms: 40,
                result: ExecutionResult::Completed {
                    completion: forge_provider_api::completion::ProviderCompletion::default()
                        .into(),
                },
            },
        ] {
            broker
                .observer
                .session_events()
                .push(ProviderEvent::Execution {
                    observation: ExecutionObservation::new(scope.clone(), update)?,
                })?;
        }
        broker
            .capture_final_checkpoint(&mut exchange, crate::exchange::ExchangeOutcome::Complete)
            .await?;
        assert_eq!(exchange.state(), ExchangeState::Complete);
        assert_eq!(exchange.turn.len(), 2);
        assert_eq!(broker.reopened_execution.revision(&scope), 1);
        assert!(!broker.reopened_execution.active(&scope));
        let saved = broker.store.load_exchange(&exchange.id)?.unwrap();
        assert_eq!(saved.turn.len(), 2);
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
            serde_json::to_value(broker.store.load_exchange(&exchange.id).unwrap().unwrap())
                .unwrap(),
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
                cancelled: Arc::new(AtomicBool::new(false)),
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
                    cancelled: Arc::new(AtomicBool::new(false)),
                    sink: None,
                },
            );
            let response: Value = serde_json::from_str(
                &tokio::time::timeout(
                    std::time::Duration::from_secs(5),
                    scope.invoke("check_run", json!({"check":"configured"})),
                )
                .await??,
            )?;
            assert_eq!(response.as_object().unwrap().len(), 1);
            let run = broker
                .check_runner
                .status(response["run_id"].as_str().unwrap(), 5000, true)
                .await?;
            assert_eq!(run.state, crate::checks::CheckState::Passed);
            assert_eq!(run.commands.len(), 1);
            assert_eq!(run.commands[0].command, "echo configured-check");
            assert_eq!(run.commands[0].exit_code, Some(0));
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
            let running: Value =
                serde_json::from_str(&scope.invoke("check_list", json!({})).await?)?;
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
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100)))
                .unwrap();
        source.release_lease().unwrap();
        request.client_id = "destination-client".into();
        request.new_session_name = Some(String::new());
        let newest =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(110)))
                .unwrap();
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
        let failure =
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(130)))
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

        let resumed =
            HarnessBroker::initialize_with_clock(request, Box::new(FixedClock(140))).unwrap();
        assert_eq!(resumed.session.id, source.session.id);
        assert_eq!(resumed.session.name, "source");
        assert_eq!(resumed.session.approval_mode, ApprovalMode::Ask);
        assert_eq!(resumed.store.list_session(None).unwrap().len(), 2);
    }

    #[tokio::test]
    async fn forks_a_live_session_without_taking_its_lease() {
        let repository = repository();
        let data = tempfile::tempdir().unwrap();
        let initialize =
            |client_id: &str, lease_conflict_action: Option<String>| InitializeRequest {
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
        assert_eq!(
            preparation.provider_request.checkpoint_id.as_deref(),
            Some("mock-checkpoint")
        );
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
            .fork(preparation.provider_request)
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
                TimelineEntry::Inherited { entry, .. } => {
                    Some(serde_json::to_value(entry).unwrap())
                }
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
            HarnessBroker::initialize_with_clock(failed_initialize, Box::new(FixedClock(130)))
                .unwrap();
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
                TimelineEntry::Inherited { entry, .. } => {
                    Some(serde_json::to_value(entry).unwrap())
                }
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
                        ProviderOperation::AdmitInput,
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
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(100)))
                .unwrap();
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
            HarnessBroker::initialize_with_clock(request.clone(), Box::new(FixedClock(200)))
                .unwrap();
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
                params: json!({ "model": "sol", "validate": true }),
            })
            .await;
        assert!(invalid_model.response.error().is_some());
        assert_eq!(broker.session.model, "mock-model");

        let invalid_effort = broker
            .dispatch(Request {
                id: 2,
                method: "session.configure".into(),
                params: json!({ "effort": "xhigh", "validate": true }),
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
                                    completion:
                                        forge_provider_api::completion::ProviderCompletion {
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
        let fixture = fixture.with_goal_status(|_owner, _update| async { Ok(()) });
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
                    false,
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
            let mut provider =
                crate::turn::test_support::address(&exchange, &format!("{id}:provider"));
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
}
