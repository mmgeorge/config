use super::*;
use crate::agent::AgentCapability;
use crate::goal::TurnEvidence;
use crate::plan::{PlanQuestionAnswer, PlanQuestionSet, PlanQuestionWithdrawal};
use crate::session::{ApprovalMode, ContextUsage};
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::time::Duration;

pub(crate) const HARNESS_SYSTEM_MESSAGE: &str = include_str!("../plan/prompts/system.md");

/// Preserve native lifecycle states and suspend Harness-only stalled goals.
pub fn native_goal_update(objective: String, state: crate::goal::GoalState) -> NativeGoalUpdate {
    use crate::goal::GoalState;
    let status = match state {
        GoalState::Active => NativeGoalStatus::Active,
        GoalState::Paused | GoalState::Stalled => NativeGoalStatus::Paused,
        GoalState::UsageLimited => NativeGoalStatus::UsageLimited,
        GoalState::BudgetLimited => NativeGoalStatus::BudgetLimited,
        GoalState::Complete => NativeGoalStatus::Complete,
        GoalState::Blocked => NativeGoalStatus::Blocked,
        GoalState::Cleared => return NativeGoalUpdate::Absent,
    };
    NativeGoalUpdate::Present { objective, status }
}
/// Represents one prompt sent across a provider boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderRequest {
    /// Separates the agent runtime from its visible root session and settings scope.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<ExecutionOwner>,
    pub harness_session_id: String,
    pub workspace: String,
    pub input: ProviderInput,
    pub mode: PromptMode,
    pub model: String,
    pub effort: String,
    pub context_window: Option<String>,
    /// Carries the session tier through explicit and continuation turns.
    pub service_tier: ServiceTier,
    pub access: crate::session::AccessPolicy,
    pub approval_mode: ApprovalMode,
    #[serde(alias = "backend_session_id")]
    pub provider_session_id: Option<forge_provider_api::ProviderSessionId>,
    #[serde(skip, default)]
    /// Durable acknowledgment capability attached before dispatch.
    pub submission: Option<std::sync::Arc<super::submission::Submission>>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Earlier definitely undelivered input, ordered before the current instruction.
    pub recovered_input: Vec<ProviderInput>,
    #[serde(skip, default)]
    pub control_context: Option<crate::tools::ControlTurnContext>,
}

impl ProviderRequest {
    /// Renders missing user input ahead of the newest instruction without re-executing a turn.
    pub(crate) fn recovered_text(&self, current: &str, previous: &[String]) -> String {
        forge_provider_api::input::recovered_text(current, previous)
    }

    /// Retains native attachment references from every recovered input and the newest input.
    pub(crate) fn input_attachments(&self) -> Vec<crate::attachment::InputAttachment> {
        self.recovered_input
            .iter()
            .chain(std::iter::once(&self.input))
            .flat_map(|input| input.attachments().iter().cloned())
            .collect()
    }

    /// Resolve the independent runtime without changing the visible settings scope.
    pub fn runtime_key(&self) -> Result<String> {
        Ok(self.owner()?.runtime_key())
    }
    /// Validate the provider runtime key before admission or cancellation routing.
    pub fn owner(&self) -> Result<ExecutionOwner> {
        ExecutionOwner::resolve(&self.execution_owner, &self.harness_session_id)
    }
}

pub use forge_provider_api::checklist::{
    ChecklistStatus, ProviderChecklistEntry, ProviderChecklistUpdate,
};

pub use forge_provider_api::change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};

/// Routes normalized provider updates to the live interaction reducer.
pub use events::ProviderEventSink;

/// Collected provider events and their completion metadata for non-streaming callers.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProviderOutput {
    pub event: Vec<ProviderEvent>,
    #[serde(flatten)]
    pub completion: ProviderCompletion,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Settled provider metadata and control results, without a second transcript source.
pub struct ProviderCompletion {
    /// Provider-owned observations, separate from host control effects.
    #[serde(flatten)]
    pub native: forge_provider_api::completion::ProviderCompletion,

    #[serde(default)]
    pub plan_edit: Vec<crate::plan::PlanEditRequest>,
    #[serde(default)]
    pub design_patch: Vec<crate::plan::DesignEditRequest>,
    pub plan_submit: Option<PlanSubmitRequest>,
    pub plan_read: Option<String>,
    pub plan_question: Option<PlanQuestionSet>,
    pub question_answer: Option<PlanQuestionAnswer>,
    pub question_withdrawal: Option<PlanQuestionWithdrawal>,
    pub control_error: Option<String>,
    #[serde(skip)]
    pub(crate) goal_control: Option<crate::goal::GoalControl>,
    #[serde(skip)]
    pub(crate) dispatched_task_permit: Option<crate::task::TaskPermit>,
    #[serde(skip)]
    pub(crate) dispatched_execution_id: Option<String>,
    #[serde(skip)]
    pub(crate) dispatched_entry_id: Option<crate::task::TaskInputId>,
    #[serde(skip)]
    pub structured_plan: bool,
}

impl ProviderOutput {
    pub(crate) fn into_completion(self) -> ProviderCompletion {
        self.completion
    }
}

impl ProviderCompletion {
    /// Accumulate host effects from native continuations before the exchange settles.
    pub(crate) fn merge_execution(&mut self, next: Self) {
        let Self {
            native,
            mut plan_edit,
            mut design_patch,
            plan_submit,
            plan_read,
            plan_question,
            question_answer,
            question_withdrawal,
            control_error,
            goal_control,
            dispatched_task_permit,
            dispatched_execution_id,
            dispatched_entry_id,
            structured_plan,
        } = next;
        self.merge_native(native);
        self.plan_edit.append(&mut plan_edit);
        self.design_patch.append(&mut design_patch);
        self.plan_submit = plan_submit.or(self.plan_submit.take());
        self.plan_read = plan_read.or(self.plan_read.take());
        self.plan_question = plan_question.or(self.plan_question.take());
        self.question_answer = question_answer.or(self.question_answer.take());
        self.question_withdrawal = question_withdrawal.or(self.question_withdrawal.take());
        self.control_error = control_error.or(self.control_error.take());
        self.goal_control = goal_control.or(self.goal_control.take());
        self.dispatched_task_permit = dispatched_task_permit.or(self.dispatched_task_permit.take());
        self.dispatched_execution_id =
            dispatched_execution_id.or(self.dispatched_execution_id.take());
        self.dispatched_entry_id = dispatched_entry_id.or(self.dispatched_entry_id.take());
        self.structured_plan |= structured_plan;
    }

    pub(crate) fn merge_native(
        &mut self,
        mut native: forge_provider_api::completion::ProviderCompletion,
    ) {
        native.evidence.tool_called |= self.native.evidence.tool_called;
        native.evidence.workspace_changed |= self.native.evidence.workspace_changed;
        native.evidence.structured_complete |= self.native.evidence.structured_complete;
        native.evidence.structured_blocked |= self.native.evidence.structured_blocked;
        native.evidence.native_state = native
            .evidence
            .native_state
            .or(self.native.evidence.native_state);
        self.native = native;
    }
    pub(crate) fn goal_evidence(&self) -> (TurnEvidence, Option<crate::task::TaskPermit>) {
        let mut evidence = self.evidence;
        let permit = match &self.goal_control {
            Some(control) => {
                evidence.structured_complete =
                    control.termination == crate::goal::GoalTermination::Complete;
                evidence.structured_blocked =
                    control.termination == crate::goal::GoalTermination::Blocked;
                Some(control.permit.clone())
            }
            None => self.dispatched_task_permit.clone(),
        };
        (evidence, permit)
    }
}

/// Identifies the exact canonical plan version entering review.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanSubmitRequest {
    pub plan_id: String,
    pub expected_version: u64,
}

/// Describes a provider fork after the Harness child identity is allocated.
#[derive(Clone, Debug)]
pub struct ProviderForkRequest {
    pub source: ProviderRequest,
    pub target_harness_session_id: String,
    pub checkpoint_id: Option<String>,
}

/// Specialize the shared provider contract with Harness-owned workflow types.
pub struct HarnessProviderHost;

impl forge_provider_api::provider::ProviderHost for HarnessProviderHost {
    type Request = ProviderRequest;
    type Completion = ProviderCompletion;
    type Output = ProviderOutput;
    type ForkRequest = ProviderForkRequest;
    type CompactionRequest = ProviderCompactionRequest;
    type TextGeneration = TextGeneration;
    type SteeringInput = SteeringInput;
    type Settings = crate::settings::PortableSettings;
    type Submission = super::submission::SubmissionRecord;
    type MockResponseControl = super::mock_response::MockResponseControl;
    fn owner(request: &Self::Request) -> Result<ExecutionOwner, ProviderError> {
        Ok(request.owner()?)
    }
    fn output(event: Vec<ProviderEvent>, completion: ProviderCompletion) -> ProviderOutput {
        ProviderOutput { event, completion }
    }
    fn has_resources(settings: &Self::Settings) -> bool {
        !settings.skill.is_empty() || !settings.mcp.is_empty()
    }
}

pub use forge_provider_api::provider::Provider as ProviderAdapter;
pub type Provider = dyn ProviderAdapter<Host = HarnessProviderHost>;

/// Build a provider strategy from an explicit launch descriptor.
pub fn build(
    launch: ProviderLaunch,
    permission_coordinator: std::sync::Arc<approval::PermissionCoordinator>,
    trace: std::sync::Arc<crate::trace::TraceStore>,
    settings_root: std::path::PathBuf,
) -> Result<Box<Provider>> {
    match ProviderKind::parse(&launch.kind)? {
        ProviderKind::Codex => Ok(Box::new(
            codex::CodexProvider::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
                trace,
                Some(settings_root),
            )?,
        )),
        ProviderKind::Copilot => Ok(Box::new(
            copilot::CopilotProvider::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
                trace,
                Some(settings_root),
            )?,
        )),
        kind @ (ProviderKind::Claude | ProviderKind::Antigravity) => Ok(Box::new(
            acp::AcpProvider::new(kind, launch.command, permission_coordinator, settings_root)?,
        )),
        ProviderKind::Mock => Ok(Box::new(MockProvider {
            session_events: Default::default(),
            provider_stream: launch
                .command
                .first()
                .is_some_and(|value| value == "provider-stream"),
            response_control: Default::default(),
            lsp_probe: launch
                .command
                .first()
                .is_some_and(|value| matches!(value.as_str(), "lsp-tools" | "control-tools")),
            delay: match launch.command.first().map(String::as_str) {
                Some("blocking" | "visible-blocking" | "writing-blocking") => {
                    Some(Duration::from_secs(60))
                }
                Some("session-blocking" | "approval-blocking") => Some(Duration::from_secs(3)),
                Some("steering") => Some(Duration::from_millis(500)),
                _ => None,
            },
            emit_before_delay: launch
                .command
                .first()
                .is_some_and(|value| value == "visible-blocking"),
            write_before_delay: launch
                .command
                .first()
                .is_some_and(|value| value == "writing-blocking"),
            fork_delay: launch
                .command
                .first()
                .is_some_and(|value| value == "fork-blocking")
                .then_some(Duration::from_secs(3)),
            steering_by_session: Default::default(),
            permission: launch
                .command
                .first()
                .is_some_and(|value| value == "approval-blocking")
                .then_some(permission_coordinator),
            retained_control: launch
                .command
                .first()
                .is_some_and(|value| value == "retained-control")
                .then(Default::default),
        })),
    }
}

#[derive(Default)]
struct MockProvider {
    session_events: std::sync::Arc<super::session_events::SessionEvents>,
    provider_stream: bool,
    response_control: super::mock_response::MockResponseControl,
    lsp_probe: bool,
    retained_control: Option<
        std::sync::Mutex<std::collections::HashMap<String, crate::tools::ControlTurnContext>>,
    >,
    permission: Option<std::sync::Arc<approval::PermissionCoordinator>>,
    delay: Option<Duration>,
    emit_before_delay: bool,
    write_before_delay: bool,
    fork_delay: Option<Duration>,
    steering_by_session: std::sync::Mutex<
        std::collections::HashMap<String, std::sync::Weak<steering::SteeringLane>>,
    >,
}

impl MockProvider {
    async fn wait_for_response(
        &self,
        hold: Option<&super::mock_response::MockResponseLease<'_>>,
        steering: &mut ActiveSteering,
        steering_text: &mut Vec<String>,
    ) -> Result<()> {
        let ready = async {
            if let Some(hold) = hold {
                hold.released().await?;
            } else if let Some(delay) = self.delay {
                tokio::time::sleep(delay).await;
            }
            Ok::<(), anyhow::Error>(())
        };
        tokio::pin!(ready);
        loop {
            tokio::select! {
                result = &mut ready => return result,
                Some(command) = steering.receive() => {
                    if let Some(submission) = &command.submission {
                        let admitted = submission.sending(super::submission::SubmissionTarget::Untracked)
                            .and_then(|()| submission.accepted("local mock steering admission".into()));
                        if let Err(error) = admitted {
                            command.complete(Err(error)).await;
                            continue;
                        }
                    }
                    steering_text.push(command.text.clone());
                    command.complete(Ok(())).await;
                }
            }
        }
    }

    fn steering_lane(&self, session_id: &str) -> std::sync::Arc<steering::SteeringLane> {
        let mut lane = self
            .steering_by_session
            .lock()
            .expect("mock steering map poisoned");
        lane.retain(|_, lane| lane.strong_count() > 0);
        if let Some(active) = lane.get(session_id).and_then(std::sync::Weak::upgrade) {
            return active;
        }
        let active = std::sync::Arc::new(steering::SteeringLane::default());
        lane.insert(session_id.to_owned(), std::sync::Arc::downgrade(&active));
        active
    }
}

#[async_trait]
impl crate::provider::ProviderAdapter for MockProvider {
    type Host = crate::provider::HarnessProviderHost;
    async fn session_events(
        &self,
        owner: ExecutionOwner,
    ) -> Result<super::events::ProviderEventStream, ProviderError> {
        self.session_events.subscribe(owner)
    }

    async fn session_steer(
        self: std::sync::Arc<Self>,
        owner: &crate::provider::ExecutionOwner,
        scope: crate::provider::InvocationScope,
        input: crate::provider::SteeringInput,
    ) -> Result<(), crate::provider::ProviderError> {
        if scope.owner != *owner {
            return Err(crate::provider::ProviderError::Rejected {
                message: "steering scope belongs to another owner".into(),
            });
        }
        if !self.session_events.running(&scope).await? {
            let request = self.session_events.continuation(&scope, input)?;
            return self.session_prompt(request, scope).await;
        }
        let events = self.session_events.clone();
        events
            .steer(&scope, input, |input| async move {
                self.steer_native(&owner.runtime_key(), input)
                    .await
                    .map_err(crate::provider::ProviderError::from)
            })
            .await
    }

    async fn session_prompt(
        self: std::sync::Arc<Self>,
        request: ProviderRequest,
        scope: InvocationScope,
    ) -> Result<(), ProviderError> {
        if request.owner()? != scope.owner {
            return Err(ProviderError::Rejected {
                message: "mock request belongs to a different owner".into(),
            });
        }
        self.session_events.retain_request(&scope, &request)?;
        let covered = request
            .submission
            .as_ref()
            .map(|receipt| {
                forge_provider_api::lifecycle::SubmissionId::try_from(receipt.id().to_owned())
                    .map_err(|error| ProviderError::Protocol {
                        message: error.to_string(),
                    })
            })
            .transpose()?
            .into_iter()
            .collect();
        self.session_events.sink(&scope)?;
        let acknowledgement =
            super::session_events::Acknowledgement::Local(request.submission.clone());
        let completion = request
            .control_context
            .as_ref()
            .map(|context| context.provider_completion.clone());
        let events = self.session_events.clone();
        events
            .start(
                scope,
                covered,
                acknowledgement,
                completion,
                crate::provider::TerminalBoundary::LocalFixture,
                move |sink| async move { self.run_native(request, sink).await },
            )
            .await
    }

    fn mock_response_control(&self) -> Option<&super::mock_response::MockResponseControl> {
        Some(&self.response_control)
    }

    async fn shutdown(&self) -> Result<()> {
        self.response_control.cancel_session(None);
        self.session_events.shutdown().await?;
        Ok(())
    }

    async fn cancel_session(&self, session_id: &str) -> Result<()> {
        self.cleanup_execution(session_id).await
    }

    async fn has_active_turn(&self, session_id: &str) -> bool {
        self.steering_by_session
            .lock()
            .expect("mock steering map poisoned")
            .get(session_id)
            .and_then(std::sync::Weak::upgrade)
            .is_some_and(|lane| lane.is_active())
    }

    async fn active_session_id_for(
        &self,
        _session_id: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(None)
    }

    async fn background_terminals(
        &self,
        _request: ProviderCatalogRequest,
    ) -> Result<terminal::TerminalSnapshot> {
        Ok(terminal::TerminalSnapshot::default())
    }

    async fn cleanup_execution(&self, session_id: &str) -> Result<()> {
        self.response_control.cancel_session(Some(session_id));
        self.session_events.cancel_runtime(session_id).await?;
        Ok(())
    }
    async fn reset_session(&self, owner: &ExecutionOwner) -> Result<()> {
        self.session_events.reset_owner(owner).await?;
        Ok(())
    }
    async fn fence_session(&self, owner: &ExecutionOwner) -> Result<()> {
        self.cleanup_execution(&owner.runtime_key()).await
    }
    async fn generate_text(
        &self,
        _request: ProviderCatalogRequest,
        purpose: TextGeneration,
        _model: &str,
        _reasoning_effort: Option<&str>,
        _history: &str,
    ) -> Result<String> {
        if let Some(delay) = self.delay {
            tokio::time::sleep(delay).await;
        }
        purpose.validate(match purpose {
            TextGeneration::Recap => {
                "The current request is recorded. Review the next proposed change."
            }
            TextGeneration::SessionName => "Review proposed changes",
            TextGeneration::CommitMessage => "feat: describe repository changes",
            TextGeneration::PullRequestDescription => {
                "Describes the repository changes for reviewers."
            }
        })
    }
    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Mock,
            label: "Mock CLI".into(),
            capability: mock_capability(),
        }
    }

    async fn fork(&self, request: ProviderForkRequest) -> Result<ProviderForkResult> {
        if let Some(delay) = self.fork_delay {
            tokio::time::sleep(delay).await;
        }
        Ok(ProviderForkResult::unprofiled(
            format!(
                "{}-fork",
                request
                    .source
                    .provider_session_id
                    .map(String::from)
                    .unwrap_or_else(|| "mock-session".into())
            )
            .try_into()?,
        ))
    }

    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        Ok(true)
    }

    async fn compact(&self, request: ProviderCompactionRequest) -> Result<ProviderOutput> {
        let scope = EventScope::SessionOperation {
            owner: request.request.owner()?,
            operation_id: request.id.clone(),
        };
        let compaction = Compaction {
            id: request.id,
            thread_id: request
                .request
                .provider_session_id
                .clone()
                .map(String::from)
                .unwrap_or_default(),
            state: CompactionState::Completed,
            manual: true,
            started_at_ms: None,
            completed_at_ms: None,
            result: CompactionResult {
                summary_content: Some(
                    "Mock compacted context preserves the conversation constraints.".into(),
                ),
                pre_compaction_tokens: Some(80_000),
                post_compaction_tokens: Some(20_000),
                tokens_removed: Some(60_000),
                ..Default::default()
            },
        };
        let request = request.request;
        Ok(ProviderOutput {
            event: vec![ProviderEvent::Compaction {
                scope,
                update: Box::new(compaction),
            }],
            completion: crate::provider::ProviderCompletion {
                native: forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: request.provider_session_id,
                    capability: mock_capability(),
                    runtime: ProviderRuntime {
                        provider: "Mock provider".into(),
                        model: Some(request.model),
                    },
                    metrics: TurnMetrics {
                        context_usage: ContextUsage::reported(20_000, 100_000),
                        ..TurnMetrics::default()
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
        })
    }

    async fn model_list(&self, _request: ProviderRequest) -> Result<Vec<ProviderModel>> {
        Ok(vec![ProviderModel {
            service_tiers: vec![
                ServiceTier::Standard,
                ServiceTier::Fast,
                ServiceTier::Ultrafast,
            ],
            id: "mock-model".into(),
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: vec![ProviderContextWindow {
                id: "default".into(),
                token_limit: Some(100_000),
            }],
            default_context_window: Some("default".into()),
            selected_context_window: None,
            vision: false,
            description: Some("Deterministic Harness test model.".into()),
            is_default: true,
        }])
    }
}

impl MockProvider {
    async fn steer_native(&self, session_id: &str, input: SteeringInput) -> Result<()> {
        self.steering_lane(session_id).steer(input).await
    }
}

impl MockProvider {
    /// Drive admitted native work inside the session-owned task.
    async fn run_native(
        &self,
        request: ProviderRequest,
        event_sink: ProviderEventSink,
    ) -> Result<crate::provider::ProviderCompletion, ProviderError> {
        if request
            .control_context
            .as_ref()
            .and_then(|context| context.submission_cancelled.as_ref())
            .is_some_and(|cancelled| cancelled.load(std::sync::atomic::Ordering::Acquire))
        {
            return Err(ProviderError::Cancelled);
        }
        if self.provider_stream {
            let steering = self.steering_lane(&request.runtime_key()?);
            let mut active_steering = steering.activate(Some(event_sink.clone()))?;
            let mut steering_text = Vec::new();
            let response_hold = self.response_control.arrive(&request);
            if let Some(hold) = response_hold.as_ref() {
                self.wait_for_response(Some(hold), &mut active_steering, &mut steering_text)
                    .await?;
            }
            return super::copilot::mock_stream::replay(&request, &event_sink).await;
        }
        let scope = event_sink.invocation_scope()?.clone();
        let turn_address = TurnAddress {
            scope,
            thread_id: request
                .provider_session_id
                .clone()
                .map(String::from)
                .unwrap_or_else(|| "mock-session".into()),
            turn_id: uuid::Uuid::new_v4().to_string().into(),
        };
        event_sink
            .send_wait(ProviderEvent::Execution {
                observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                    turn_address.scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Started {
                        address: turn_address.clone(),
                    },
                )
                .map_err(|error| ProviderError::Protocol {
                    message: error.to_string(),
                })?,
            })
            .await?;
        event_sink
            .send_wait(ProviderEvent::Turn {
                address: turn_address.clone(),
                update: TurnUpdate::Started {
                    started_at_ms: crate::provider::retry::now_ms(),
                },
            })
            .await?;
        let completion_sink = event_sink.clone();
        let output: anyhow::Result<crate::provider::ProviderCompletion> = async {
        let event_sink = Some(event_sink);
        let steering = self.steering_lane(&request.runtime_key()?);
        let mut active_steering = steering.activate(event_sink.clone())?;
        let mut steering_text_list = Vec::new();
        let response_hold = self.response_control.arrive(&request);
        if let Some(hold) = response_hold.as_ref() {
            self.wait_for_response(Some(hold), &mut active_steering, &mut steering_text_list)
                .await?;
        }
        if let Some(retained) = &self.retained_control
            && let Some(context) = &request.control_context
        {
            retained
                .lock()
                .expect("mock control context poisoned")
                .insert(request.runtime_key()?, context.clone());
        }
        let probe_input = self.lsp_probe.then(|| request.input.text());
        if let Some(input) = probe_input.as_deref()
            && let Some(arguments) = input
                .strip_prefix("LSP ")
                .or_else(|| input.strip_prefix("CONTROL "))
        {
            let context = request
                .control_context
                .clone()
                .context("fixture has no control context")?;
            let mut arguments: Value = serde_json::from_str(arguments)?;
            let name = arguments
                .as_object_mut()
                .and_then(|value| value.remove("tool"))
                .and_then(|value| value.as_str().map(str::to_owned))
                .context("LSP fixture requires tool name")?;
            let control_probe = input.starts_with("CONTROL ");
            let collect_pages = arguments
                .as_object_mut()
                .and_then(|arguments| arguments.remove("__collect_pages"))
                .map(serde_json::from_value::<bool>)
                .transpose()?
                .unwrap_or(false);
            let tool_id = uuid::Uuid::new_v4().to_string();
            if control_probe && let Some(sink) = &event_sink {
                let address = ToolAddress { turn: turn_address.clone(), tool_id: tool_id.clone().into() };
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: ToolUpdate::Requested { invocation: ToolInvocation {
                        semantics: Default::default(),
                        kind: ToolActivityKind::ToolCall,
                        name: name.clone(),
                        title: name.clone(),
                        arguments: arguments.clone(),
                    } },
                }).await?;
                sink.send_wait(ProviderEvent::Tool { address, update: ToolUpdate::Started }).await?;
            }
            let text = if control_probe && name == "__catalog" {
                serde_json::to_string(
                    &crate::tools::ControlToolRegistry
                        .definition_list_for_context(Some(&context), request.owner()?.is_root())
                        .into_iter()
                        .map(|tool| tool.name)
                        .collect::<Vec<_>>(),
                )?
            } else {
                let invocation = crate::tools::ControlToolInvocation {
                    task_permit: None,
                    name: name.clone(),
                    arguments: arguments.clone(),
                };
                let executor = crate::tools::ControlToolExecutor::new(context);
                match executor
                    .execute(invocation)
                    .await
                    .and_then(|outcome| Ok(outcome.result?))
                {
                    Ok(result) if collect_pages => {
                        let mut page: Value = serde_json::from_str(&result.message)?;
                        let mut cursor_set = std::collections::HashSet::new();
                        let mut pages = Vec::new();
                        loop {
                            let cursor = page["next_cursor"].as_str().map(str::to_owned);
                            pages.push(page);
                            let Some(cursor) = cursor else {
                                break;
                            };
                            anyhow::ensure!(
                                cursor_set.insert(cursor.clone()),
                                "inspection returned a repeated cursor"
                            );
                            arguments["cursor"] = json!(cursor);
                            let result = executor
                                .execute(crate::tools::ControlToolInvocation {
                                    task_permit: None,
                                    name: name.clone(),
                                    arguments: arguments.clone(),
                                })
                                .await?
                                .result?;
                            page = serde_json::from_str(&result.message)?;
                        }
                        serde_json::to_string(&json!({"pages":pages}))?
                    }
                    Ok(result) => result.message,
                    Err(error) if control_probe => {
                        serde_json::to_string(&json!({"error":format!("{error:#}")}))?
                    }
                    Err(error) => format!("LSP error: {error:#}"),
                }
            };
            if control_probe && let Some(sink) = &event_sink {
                let address = ToolAddress { turn: turn_address.clone(), tool_id: tool_id.into() };
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: ToolUpdate::Output { update: TextUpdate::Replace(text.clone()) },
                }).await?;
                sink.send_wait(ProviderEvent::Tool {
                    address,
                    update: ToolUpdate::Finished { outcome: ToolOutcome::Succeeded },
                }).await?;
            }
            let text = if control_probe {
                    format!("```json\n{text}\n```")
                } else {
                    text
                };
            let address = MessageAddress {
                turn: turn_address.clone(),
                message_id: MessageId::Native(uuid::Uuid::new_v4().to_string()),
            };
            if let Some(sink) = event_sink {
                sink.send_wait(ProviderEvent::Message { address: address.clone(),
                    update: MessageUpdate::Started { classification: MessageClassification::Known(MessageRole::Response), text: text.clone() }
                }).await?;
                sink.send_wait(ProviderEvent::Message { address,
                    update: MessageUpdate::Completed { text }
                }).await?;
            }
            return Ok(ProviderCompletion { native: forge_provider_api::completion::ProviderCompletion {  capability: mock_capability(), ..Default::default() }, ..Default::default() });
        }
        if let Some(context) = request.control_context.as_ref()
            && context
                .checks
                .as_ref()
                .is_some_and(|scope| scope.is_setup())
        {
            let advertised = crate::tools::ControlToolRegistry
                .definition_list_for_context(Some(context), true);
            anyhow::ensure!(
                advertised
                    .iter()
                    .any(|tool| tool.name == "check_definition_list")
                    && advertised
                        .iter()
                        .any(|tool| tool.name == "check_definition_edit"),
                "check setup requires advertised list and configuration tools"
            );
        }
        if let Some(scope) = request
            .control_context
            .as_ref()
            .and_then(|context| context.checks.as_ref())
            && scope.is_setup()
        {
            let catalog: Value = serde_json::from_str(
                &scope
                    .invoke("check_definition_list", serde_json::json!({}))
                    .await?,
            )?;
            let mut config = serde_json::Map::new();
            for definition in catalog["definitions"].as_array().into_iter().flatten() {
                let details: Value = serde_json::from_str(
                    &scope
                        .invoke(
                            "check_definition_details",
                            serde_json::json!({"check_id":definition["check_id"]}),
                        )
                        .await?,
                )?;
                let id = definition["check_id"]
                    .as_str()
                    .context("missing check identity")?;
                config.insert(id.to_owned(), details["definition"].clone());
            }
            scope
                .invoke(
                    "check_definition_edit",
                    serde_json::json!({
                        "expected_revision": catalog["revision"], "config": config,
                    }),
                )
                .await?;
        }
        let mut approval_denial = None;
        if let Some(permission) = &self.permission {
            let resolution = permission
                .authorize(
                    request.approval_mode,
                    crate::permissions::matcher::PermissionRequest {
                        id: "mock-approval".into(),
                        provider: "Mock".into(),
                        reason: None,
                        target_list: vec![crate::permissions::matcher::PermissionTarget::Command {
                            command: "mock-probe".into(),
                            shell: None,
                        }],
                    },
                    Some(turn_address.clone()),
                    event_sink.as_ref(),
                )
                .await?;
            approval_denial = match resolution {
                approval::ApprovalResolution::AllowOnce
                | approval::ApprovalResolution::AllowExact
                | approval::ApprovalResolution::AllowBroad => None,
                approval::ApprovalResolution::Cancel => {
                    return Err(crate::broker::TurnCancelled.into());
                }
                approval::ApprovalResolution::TimedOut => {
                    Some("Automatically rejected because the approval timeout expired.")
                }
                approval::ApprovalResolution::DenyPolicy => {
                    Some("Automatically rejected by saved permission policy.")
                }
                approval::ApprovalResolution::DenyMode => {
                    Some("Automatically rejected in reject-all approval mode.")
                }
                approval::ApprovalResolution::DenyOnce
                | approval::ApprovalResolution::DenyExact
                | approval::ApprovalResolution::DenyBroad => Some("Rejected by the user."),
            };
        }
        let response_text = if let Some(reason) = approval_denial {
                format!(
                    "Mock response: {reason} The requested operation was not executed. Continuing with permitted work."
                )
            } else if request.mode == PromptMode::PlanQuestion {
                "This declaration defines the proposed interface and separates its responsibility from the surrounding code. Implementation details remain outside the declaration plan.".into()
            } else {
                format!("Mock response: {}", request.input.text())
            };
        let message_address = MessageAddress {
            turn: turn_address.clone(),
            message_id: MessageId::Native(uuid::Uuid::new_v4().to_string()),
        };
        let event = ProviderEvent::Message {
            address: message_address.clone(),
            update: MessageUpdate::Started {
                classification: MessageClassification::Known(MessageRole::Response),
                text: response_text.clone(),
            },
        };
        if self.emit_before_delay
            && let Some(event_sink) = event_sink.as_ref()
        {
            event_sink.send_wait(event.clone()).await?;
        }
        if self.write_before_delay {
            std::fs::write(
                std::path::Path::new(&request.workspace).join("mock-provider-change.txt"),
                "changed before cancellation\n",
            )?;
        }
        if response_hold.is_none() {
            self.wait_for_response(None, &mut active_steering, &mut steering_text_list)
                .await?;
        }
        if approval_denial.is_some() {
            let address = ToolAddress { turn: turn_address.clone(), tool_id: "mock-denied".into() };
            if let Some(sink) = &event_sink {
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: ToolUpdate::Requested { invocation: ToolInvocation {
                    semantics: Default::default(),
                    kind: ToolActivityKind::ToolCall,
                    name: "mock-probe".into(),
                    title: "mock-probe".into(),
                    arguments: Value::Null,
                } } }).await?;
                sink.send_wait(ProviderEvent::Tool { address,
                    update: ToolUpdate::Finished { outcome: ToolOutcome::Denied {
                        reason: approval_denial.unwrap_or_default().into(),
                    } }
                }).await?;
                if !self.emit_before_delay { sink.send_wait(event.clone()).await?; }
                sink.send_wait(ProviderEvent::Message { address: message_address,
                    update: MessageUpdate::Completed { text: response_text }
                }).await?;
            }
            self.permission
                .as_ref()
                .expect("mock approval owner")
                .take_timeout_feedback(&request.owner()?)?;
            return Ok(ProviderCompletion { native: forge_provider_api::completion::ProviderCompletion {  capability: mock_capability(), ..Default::default() }, ..Default::default() });
        }
        if request.mode == PromptMode::Plan
            && let Some(document) = request
                .control_context
                .as_ref()
                .and_then(|context| context.plan_document.as_ref())
                .filter(|document| document.design.is_some())
        {
            let path = "src/change.rs";
            let design = document.design.as_ref().unwrap();
            let mut source_digests = std::collections::BTreeMap::new();
            let patch = if design.proposed.contains_key(path)
                || std::path::Path::new(&request.workspace)
                    .join(path)
                    .is_file()
            {
                let inspected =
                    design.inspect(std::path::Path::new(&request.workspace), Some(path), false)?;
                if let Some(digest) = inspected["source_digest"].as_str() {
                    source_digests.insert(path.into(), digest.into());
                }
                let text = inspected["text"]
                    .as_str()
                    .ok_or_else(|| anyhow::anyhow!("mock declaration read has no text"))?;
                let removed = text
                    .lines()
                    .map(|line| format!("-{line}\n"))
                    .collect::<String>();
                let original = design
                    .baseline
                    .get(path)
                    .map(|file| file.text.as_str())
                    .unwrap_or(text);
                let behavior = if original.contains("pub fn reviewed_change(") {
                    "Change\n  Preserve the requested behavior."
                } else {
                    "Algorithm\n  return"
                };
                let behavior = behavior
                    .lines()
                    .map(|line| format!("+{line}\n"))
                    .collect::<String>();
                format!(
                    "*** Begin Patch\n*** Update File: {path}\n@@\n{removed}+pub fn reviewed_change();\n{behavior}*** End Patch"
                )
            } else {
                format!(
                    "*** Begin Patch\n*** Add File: {path}\n+pub fn requested_change();\n+Algorithm\n+  return\n*** End Patch"
                )
            };
            let mut patch = patch;
            let mut configuration = design.proposed.clone();
            for path in [
                "Cargo.toml",
                "package.json",
                "tsconfig.json",
                "ci.yaml",
                "App.csproj",
                ".gitignore",
            ] {
                if !configuration.contains_key(path)
                    && !design.baseline.contains_key(path)
                    && std::path::Path::new(&request.workspace)
                        .join(path)
                        .is_file()
                {
                    let inspected = design.inspect(
                        std::path::Path::new(&request.workspace),
                        Some(path),
                        false,
                    )?;
                    configuration.insert(path.into(), inspected["text"].as_str().unwrap().into());
                    source_digests.insert(
                        path.into(),
                        inspected["source_digest"].as_str().unwrap().into(),
                    );
                }
            }
            for (path, configuration) in &configuration {
                if forge_diff::syntax::ConfigurationFormat::for_path(path).is_none() {
                    continue;
                }
                let updated = configuration.replacen("\"0.1.0\"", "\"0.2.0\"", 1);
                if updated != *configuration {
                    let removed = configuration
                        .lines()
                        .map(|line| format!("-{line}\n"))
                        .collect::<String>();
                    let added = updated
                        .lines()
                        .map(|line| format!("+{line}\n"))
                        .collect::<String>();
                    patch = patch.replace(
                        "*** End Patch",
                        &format!("*** Update File: {path}\n@@\n{removed}{added}*** End Patch"),
                    );
                }
            }
            let metadata = serde_json::to_string_pretty(&design.document)?
                .lines()
                .map(|line| format!("-{line}\n"))
                .collect::<String>();
            let mut revised_metadata = design.document.clone();
            revised_metadata.objective = "Revise the registry interface.".into();
            revised_metadata.background =
                "The registry owns published handles used by its callers.".into();
            revised_metadata.requirements =
                vec!["Preserve the registry ownership boundary.".into()];
            revised_metadata.design =
                "Revise the registry interface while preserving its ownership boundary.".into();
            revised_metadata.verification =
                vec!["Confirm the requested interface behaves as designed.".into()];
            let added = serde_json::to_string_pretty(&revised_metadata)?
                .lines()
                .map(|line| format!("+{line}\n"))
                .collect::<String>();
            let patch = patch.replace(
                "*** End Patch",
                &format!("*** Update File: plan.json\n@@\n{metadata}{added}*** End Patch"),
            );
            let change = crate::plan::DesignPatchRequest {
                plan_id: document.plan_id.clone(),
                expected_version: document.version,
                patch,
                title: Some("Design the requested change".into()),
                source_digests,
            };
            let proposed =
                document.patch_design(std::path::Path::new(&request.workspace), change.clone())?;
            return Ok(ProviderCompletion { native: forge_provider_api::completion::ProviderCompletion {  provider_session_id: request.provider_session_id.or(Some("mock-session".to_owned().try_into().expect("static mock identity is nonempty"))), capability: mock_capability(), ..Default::default() }, design_patch: vec![change.into()], plan_submit: Some(PlanSubmitRequest {
                    plan_id: document.plan_id.clone(),
                    expected_version: proposed.version,
                }), structured_plan: true, ..Default::default() });
        }
        let planning_note = (request.mode == PromptMode::Plan).then(|| {
            let steering = steering_text_list
                .iter()
                .map(|text| format!("- {text}"))
                .collect::<Vec<_>>()
                .join("\n");
            if steering.is_empty() {
                request.input.text().to_owned()
            } else {
                format!(
                    "{}\n\nSteering constraints:\n{steering}",
                    request.input.text()
                )
            }
        });
        let plan_document = planning_note
            .as_deref()
            .and_then(mock_plan_document_from_prompt);
        let mut plan_edit = Vec::new();
        let plan_submit = plan_document.as_ref().map(|document| {
            let mut mutation = crate::plan::PlanMutation {
                plan: Some(crate::plan::PlanFieldPatch {
                    title: Some("Implement the requested change".into()),
                    overview: Some(format!(
                        "{}\n{}",
                        if document.prompt.is_empty() {
                            &document.title
                        } else {
                            &document.prompt
                        },
                        steering_text_list.join("\n")
                    )),
                    ..Default::default()
                }),
                ..Default::default()
            };
            if document.stages.is_empty() {
                mutation.set = Some(crate::plan::PlanResourceSet {
                    entity_changes: Some(vec![crate::plan::ProgramEntityChange {
                            action: crate::plan::EntityChangeAction::Add,
                            kind: crate::plan::EntityKind::Function,
                            renamed_from: None,
                            name: "requested_change".into(),
                            description: "Implement the requested behavior.".into(),
                            path: "src/change.rs".into(),
                            members: Vec::new(),
                            variants: Vec::new(),
                            extends: None,
                            conforms_to: Vec::new(),
                    }]),
                    flows: Some(vec![crate::plan::PlanFlow {
                            title: "Requested change".into(),
                            description: "Start from the requested change and produce its observable result. Keep implementation ownership explicit at the changed entity boundary.".into(),
                            source: crate::plan::EntityReference::PlannedEntity {
                                entity: "requested_change".into(),
                            },
                            edges: vec![crate::plan::PlanFlowEdge {
operation: crate::plan::PlanFlowOperation::Emit("completed change".into()),
target: crate::plan::EntityReference::ExternalEntity {
                                        entity_kind: crate::plan::ReferencedEntityKind::Endpoint,
                                        name: "requested outcome".into(),
                                        dependency: None,
                                    },
expansion: Vec::new(),
branches: Vec::new()
}],
                    }]),
                    ..Default::default()
                });
                mutation.stages = Some(vec![crate::plan::PlanStage { id: "implementation".into(), title: "Implement the change".into(), tasks: vec![crate::plan::PlanTask { id: "requested-change".into(), requires: Vec::new(),
                            title: "Implement the requested change".into(),
                            description: "Connect the requested behavior to its source boundary."
                                .into(),
                            files: vec![crate::plan::PlanFile {
                                change: crate::plan::PlanFileChange::Add {
                                    path: "src/change.rs".into(),
                                },
                                subtasks: vec![
                                    crate::plan::PlanSubtask::Work(
                                        crate::plan::PlanWorkSubtask {
                                            action: crate::plan::SubtaskAction::Create,
                                            description: "Apply the planned behavior at its owner."
                                                .into(),
                                            entities: vec!["requested_change".into()],
                                        },
                                    ),
                                    crate::plan::PlanSubtask::Test(
                                        crate::plan::PlanTestSubtask {
                                            operation: crate::plan::TestSubtaskOperation::Test,
                                            action: crate::plan::ChangeAction::Add,
                                            renamed_from: None,
                                            name: "verify_requested_change".into(),
                                            category: crate::plan::TestCategory::Unit,
                                            behavior:
                                                "The implementation produces the requested result."
                                                    .into(),
                                            covers_entities: vec!["requested_change".into()],
                                        },
                                    ),
                                ],
                            }],
                    }] }]);
            }
            plan_edit.push(crate::plan::PlanEditRequest {
                plan_id: document.plan_id.clone(),
                expected_version: document.version,
                mutation,
            });
            PlanSubmitRequest {
                plan_id: document.plan_id.clone(),
                expected_version: document.version.saturating_add(1),
            }
        });
        let execution_document = matches!(
            request.mode,
            PromptMode::ExecutePlan | PromptMode::GoalContinuation
        )
        .then(|| mock_plan_document_from_prompt(&request.input.text()))
        .flatten();
        let execution_id = request
            .input
            .text()
            .split("Execution ID: ")
            .nth(1)
            .and_then(|tail| tail.split(['.', '\n']).next())
            .map(str::to_owned);
        let plan_task_report = execution_document
            .as_ref()
            .zip(execution_id)
            .and_then(|(document, execution_id)| {
                request
                    .input
                    .text()
                    .split("Active task:\n```json\n")
                    .nth(1)
                    .and_then(|tail| tail.split("\n```").next())
                    .and_then(|value| serde_json::from_str::<serde_json::Value>(value).ok())
                    .and_then(|active| {
                        active["task_id"]
                            .as_str()
                            .and_then(|id| document.task_by_id(id))
                    })
                    .map(|task| (document, task, execution_id))
            })
            .map(|(document, (task_path, task), execution_id)| {
                let task_path = task_path.as_str();
                crate::plan::PlanTaskReport {
                    task_id: task.id.clone(),
                    plan_version: document.version,
                    execution_id,
                    task_path: task_path.into(),
                    state: crate::plan::PlanTaskOutcome::Complete,
                    completed_subtask_paths: task
                        .files
                        .iter()
                        .enumerate()
                        .flat_map(|(file_index, file)| {
                            file.subtasks
                                .iter()
                                .enumerate()
                                .map(move |(subtask_index, _)| {
                                    format!(
                                        "{}/files/{file_index}/subtasks/{subtask_index}",
                                        task_path
                                    )
                                })
                        })
                        .collect(),
                    completed_entity_paths: task
                        .files
                        .iter()
                        .flat_map(|file| file.subtasks.iter())
                        .flat_map(|subtask| subtask.owned_entities().iter())
                        .filter_map(|entity_name| {
                            document
                                .entity_changes
                                .iter()
                                .position(|entity| entity.name == *entity_name)
                        })
                        .map(|entity_index| format!("/entity_changes/{entity_index}"))
                        .collect(),
                    test_results: task
                        .files
                        .iter()
                        .enumerate()
                        .flat_map(|(file_index, file)| {
                            file.subtasks.iter().enumerate().filter_map(
                                move |(subtask_index, subtask)| {
                                    subtask.test().map(|_| crate::plan::PlanTestResult {
                                        test_subtask_path: Some(format!(
                                            "{}/files/{file_index}/subtasks/{subtask_index}",
                                            task_path
                                        )),
                                        status: crate::plan::PlanTestStatus::Passed,
                                        command: Some("mock test".into()),
                                        detail: None,
                                    })
                                },
                            )
                        })
                        .collect(),
                    changed_paths: task
                        .files
                        .iter()
                        .flat_map(|file| {
                            file.change
                                .source_path()
                                .into_iter()
                                .chain(std::iter::once(file.change.path()))
                                .map(str::to_owned)
                        })
                        .collect(),
                    summary: Some("Completed the mock task.".into()),
                    blocking_reason: None,
                }
            });
        let execution_complete = plan_task_report.as_ref().is_some_and(|report| {
            execution_document
                .as_ref()
                .and_then(|document| document.tasks().last())
                .is_some_and(|task| task.id == report.task_id)
        });
        if let Some(event_sink) = event_sink {
            if !self.emit_before_delay { event_sink.send_wait(event).await?; }
            event_sink.send_wait(ProviderEvent::Message {
                address: message_address,
                update: MessageUpdate::Completed { text: response_text },
            }).await?;
        }
        let plan_question: Option<PlanQuestionSet> =
            if request.input.text() == "Ask a fixture question." {
                Some(PlanQuestionSet::freeform(
                    "Which fixture should the operation use?".into(),
                ))
            } else {
                None
            };
        Ok(ProviderCompletion { native: forge_provider_api::completion::ProviderCompletion {  provider_session_id: request
                .provider_session_id
                .or_else(|| Some("mock-session".to_owned().try_into().expect("static mock identity is nonempty"))), provider_checkpoint_id: Some("mock-checkpoint".into()), evidence: TurnEvidence {
                tool_called: request.access.write_access != crate::session::WriteScope::Off,
                structured_complete: execution_complete,
                ..TurnEvidence::default()
            }, capability: mock_capability(), runtime: ProviderRuntime {
                provider: "Mock provider".into(),
                model: Some(request.model),
            }, metrics: TurnMetrics::default() }, plan_edit, design_patch: Vec::new(), plan_submit, plan_read: None, plan_question, question_answer: None, question_withdrawal: None, control_error: None, goal_control: None, dispatched_task_permit: None, dispatched_execution_id: None, dispatched_entry_id: None, structured_plan: request.mode == PromptMode::Plan})

        }.await;
        let outcome = match &output {
            Ok(_) => crate::turn::TurnOutcome::Completed,
            Err(error) if error.is::<crate::broker::TurnCancelled>() => {
                crate::turn::TurnOutcome::Cancelled
            }
            Err(_) => crate::turn::TurnOutcome::Failed,
        };
        completion_sink
            .send_wait(ProviderEvent::Turn {
                address: turn_address,
                update: TurnUpdate::Finished {
                    outcome,
                    completed_at_ms: crate::provider::retry::now_ms(),
                },
            })
            .await?;
        output.map_err(ProviderError::from)
    }
}

pub(crate) fn mock_plan_document_from_prompt(prompt: &str) -> Option<crate::plan::PlanDocument> {
    let mut remaining = prompt;
    while let Some(offset) = remaining.find("```json\n") {
        let json_start = offset + "```json\n".len();
        let relative_end = remaining[json_start..].find("\n```")?;
        let json_end = json_start + relative_end;
        if let Ok(document) = serde_json::from_str(&remaining[json_start..json_end]) {
            return Some(document);
        }
        remaining = &remaining[json_end + "\n```".len()..];
    }
    None
}

/// Describe the synthetic provider used by isolated Harness workflow fixtures.
pub(crate) fn mock_capability() -> ProviderCapability {
    ProviderCapability {
        integration: ProviderIntegration {
            background_generation: true,
            isolated_generation: true,
            terminal_termination: true,
            native_agent_suppression: true,
            mcp_runtime_status: true,
            mcp_tool_catalog: true,
            local_acceptance: true,
            resource_target: Some("mock".into()),
            ..Default::default()
        },
        os_sandbox: false,
        native_fork: true,
        native_compact: true,
        compaction: CompactionCapability {
            lifecycle: true,
            background: false,
            summary: true,
            metrics: true,
        },
        native_steer: true,
        native_turn_rollback: true,
        native_goal: false,
        model_selection: true,
        effort_selection: true,
        fast_mode: true,
        ultrafast_mode: true,
        permission_control: true,
        background_terminals: true,
        approval_mode_list: vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow],
        agent: AgentCapability::default(),
        catalog: CatalogCapability::default(),
    }
}

/// Resolve declared capabilities at the compiled-in provider registration boundary.
pub fn registered_capability(kind: ProviderKind) -> ProviderCapability {
    match kind {
        ProviderKind::Codex => codex::capability(),
        ProviderKind::Copilot => copilot::capability(),
        ProviderKind::Claude | ProviderKind::Antigravity => acp::capability(kind),
        ProviderKind::Mock => mock_capability(),
    }
}

#[cfg(test)]
mod test {
    /// Keep persisted completion fields flat while separating native observations from host effects.
    #[test]
    fn native_completion_preserves_the_flat_host_wire_format() {
        let mut completion = super::ProviderCompletion::default();
        completion.native.provider_session_id =
            Some("native-session".to_owned().try_into().unwrap());
        completion.native.runtime.provider = "test-provider".into();
        completion.native.evidence.tool_called = true;
        completion.plan_read = Some("plan".into());
        let encoded = serde_json::to_value(&completion).unwrap();
        assert!(encoded.get("native").is_none());
        assert_eq!(encoded["provider_session_id"], "native-session");
        assert_eq!(encoded["runtime"]["provider"], "test-provider");
        assert_eq!(encoded["evidence"]["tool_called"], true);
        assert_eq!(encoded["plan_read"], "plan");
        let restored: super::ProviderCompletion = serde_json::from_value(encoded).unwrap();
        assert_eq!(
            restored
                .native
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str()),
            Some("native-session")
        );
        assert_eq!(restored.plan_read.as_deref(), Some("plan"));
        assert!(restored.native.evidence.tool_called);
    }

    #[test]
    fn sandbox_allowlist_preserves_codex_preferences_and_disables_other_providers() {
        for provider in [
            super::ProviderKind::Codex,
            super::ProviderKind::Copilot,
            super::ProviderKind::Claude,
            super::ProviderKind::Antigravity,
            super::ProviderKind::Mock,
        ] {
            let requested = crate::session::AccessPolicy::default();
            let effective =
                crate::provider::registered_capability(provider).resolve_access(requested.clone());
            assert_eq!(
                crate::provider::registered_capability(provider).supports_sandbox(),
                provider == super::ProviderKind::Codex
            );
            assert_eq!(
                effective.sandbox,
                crate::provider::registered_capability(provider).supports_sandbox()
            );
            assert!(requested.sandbox);
            assert_eq!(effective.write_access, requested.write_access);
            assert_eq!(effective.writable_directory, requested.writable_directory);
            assert_eq!(effective.isolation_mode, requested.isolation_mode);
            crate::provider::registered_capability(provider)
                .validate_access(&effective)
                .unwrap();
            let disabled = crate::session::AccessPolicy {
                sandbox: false,
                ..requested
            };
            assert!(
                !crate::provider::registered_capability(provider)
                    .resolve_access(disabled)
                    .sandbox
            );
        }
    }

    #[test]
    fn provider_instructions_describe_staged_edits_and_revision_scoped_reports() {
        let prompt = super::HARNESS_SYSTEM_MESSAGE;
        assert!(prompt.contains("plan_file_edit"));
        assert!(!prompt.contains("harness_plan_task_report"));
        assert!(!prompt.contains("harness_plan_edit"));
    }
    use super::super::mock_response::MockResponseCommand;
    use super::{MockProvider, PromptMode, ProviderAdapter, ProviderRequest};
    use crate::provider::test_support::ProviderTestExt as _;
    use crate::session::ApprovalMode;
    use std::sync::Arc;

    #[tokio::test]
    async fn applies_steering_to_the_same_planning_turn() {
        let directory = tempfile::tempdir().unwrap();
        let store = crate::provider::submission::SubmissionStore::open(
            directory.path(),
            Arc::new(crate::trace::TraceStore::open(directory.path()).unwrap()),
        )
        .unwrap();
        let submission = store
            .prepare_steering(
                &super::ProviderCatalogRequest {
                    execution_owner: None,
                    harness_session_id: "harness-session".into(),
                    workspace: ".".into(),
                    access: Default::default(),
                    approval_mode: ApprovalMode::Ask,
                    provider_session_id: None,
                },
                super::ProviderInput::from_text("And be sure to modify Y"),
                super::ProviderKind::Mock,
            )
            .unwrap();
        let input = super::SteeringInput::User {
            text: "And be sure to modify Y".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        };
        let provider = Arc::new(MockProvider {
            session_events: Default::default(),
            response_control: Default::default(),
            provider_stream: false,
            lsp_probe: false,
            retained_control: None,
            permission: None,
            delay: None,
            emit_before_delay: false,
            write_before_delay: false,
            fork_delay: None,
            steering_by_session: Default::default(),
        });
        let hold = provider
            .response_control
            .dispatch(
                "harness-session",
                MockResponseCommand::Hold {
                    mode: Some(PromptMode::Plan),
                    any_session: false,
                },
            )
            .await
            .unwrap();
        let hold_id = hold["id"].as_str().unwrap().to_owned();
        let test_provider =
            crate::provider::test_support::ProviderTestSession::from_arc(provider.clone()).unwrap();
        let prompt_provider = test_provider.clone();
        let prompt = tokio::spawn(async move {
            prompt_provider
                .session_prompt_and_collect(
                    ProviderRequest {
                        execution_owner: None,
                        harness_session_id: "harness-session".into(),
                        workspace: ".".into(),
                        input: super::ProviderInput::from_text(
                            "Active canonical PlanDocument:\n```json\n{\"schema_version\":5,\"version\":1,\"plan_id\":\"plan\",\"title\":\"Refactor X\",\"overview\":\"Planning\",\"usage\":null,\"entity_changes\":[],\"dependencies\":[],\"flows\":[],\"stages\":[],\"assumptions\":[]}\n```\n\nRefactor X",
                        ),
                        mode: PromptMode::Plan,
                        model: "mock-model".into(),
                        effort: "low".into(),
                        context_window: None,
                        service_tier: crate::provider::ServiceTier::Standard,
                        access: Default::default(),
                        approval_mode: ApprovalMode::Ask,
                        provider_session_id: None,
                        submission: None,
                        recovered_input: Vec::new(),
                        control_context: None,
                    },
                )
                .await
        });
        provider
            .response_control
            .dispatch(
                "harness-session",
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            )
            .await
            .unwrap();
        assert!(provider.has_active_turn("harness-session").await);
        assert!(!provider.has_active_turn("other-session").await);
        test_provider
            .steer_current(
                crate::provider::ExecutionOwner::root("harness-session"),
                input,
            )
            .await
            .unwrap();
        assert!(!prompt.is_finished(), "steering released the held response");
        provider
            .response_control
            .dispatch(
                "harness-session",
                MockResponseCommand::Release { id: hold_id },
            )
            .await
            .unwrap();
        let output = prompt.await.unwrap().unwrap();
        assert!(!provider.has_active_turn("harness-session").await);
        assert!(matches!(
            submission.record().unwrap().delivery,
            crate::provider::submission::SubmissionState::Accepted { .. }
        ));
        let text = output.completion.plan_edit[0]
            .mutation
            .plan
            .as_ref()
            .and_then(|plan| plan.overview.as_deref())
            .expect("mock planning should update the overview");
        assert!(text.contains("Refactor X"));
        assert!(text.contains("And be sure to modify Y"));
        assert!(!text.contains("Active canonical PlanDocument"));
        assert!(!text.contains("schema_version"));
    }
}

#[cfg(test)]
mod session_stream_tests {
    use super::*;
    use crate::provider::mock_response::MockResponseCommand;
    use std::sync::Arc;

    fn request() -> ProviderRequest {
        ProviderRequest {
            execution_owner: None,
            harness_session_id: "persistent-mock".into(),
            workspace: ".".into(),
            input: ProviderInput::from_text("hello"),
            mode: PromptMode::Chat,
            model: "mock-model".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    /// A held native turn reports its real start and a collected cancellation.
    #[tokio::test]
    async fn cancelling_held_response_preserves_native_terminal_and_session_stream()
    -> anyhow::Result<()> {
        use forge_provider_api::lifecycle::{ExecutionResult, ExecutionUpdate};

        let provider = Arc::new(MockProvider::default());
        let owner = request().owner()?;
        let mut stream = provider.session_events(owner.clone()).await?;
        let hold = provider
            .response_control
            .dispatch(
                "persistent-mock",
                MockResponseCommand::Hold {
                    mode: Some(PromptMode::Chat),
                    any_session: false,
                },
            )
            .await?;
        let scope = InvocationScope::new(owner.clone(), "cancelled");
        provider
            .clone()
            .session_prompt(request(), scope.clone())
            .await?;
        provider
            .response_control
            .dispatch(
                "persistent-mock",
                MockResponseCommand::Wait {
                    id: hold["id"].as_str().unwrap().to_owned(),
                },
            )
            .await?;
        let mut started = false;
        while let Some(event) = stream.try_next()? {
            if let ProviderEvent::Execution { observation } = event
                && matches!(observation.update(), ExecutionUpdate::Started { .. })
            {
                assert_eq!(observation.scope(), &scope);
                started = true;
            }
        }
        assert!(
            started,
            "held response must follow an observed native start"
        );
        provider.cancel_session("persistent-mock").await?;
        tokio::time::timeout(Duration::from_secs(1), async {
            loop {
                let event = stream.recv().await?.ok_or_else(|| {
                    anyhow::anyhow!("session stream disconnected during cancellation")
                })?;
                if let ProviderEvent::Execution { observation } = event {
                    assert_eq!(observation.scope(), &scope);
                    match observation.update() {
                        ExecutionUpdate::Finished {
                            result: ExecutionResult::Cancelled { .. },
                            ..
                        } => return Ok::<_, anyhow::Error>(()),
                        ExecutionUpdate::ObservationLost { failure, .. } => {
                            anyhow::bail!("lost cancellation observation: {failure}")
                        }
                        ExecutionUpdate::Finished { result, .. } => {
                            anyhow::bail!("incorrect terminal result: {result:?}")
                        }
                        _ => {}
                    }
                }
            }
        })
        .await??;
        assert!(!provider.has_active_turn("persistent-mock").await);
        let next = InvocationScope::new(owner, "next");
        provider
            .clone()
            .session_prompt(request(), next.clone())
            .await?;
        tokio::time::timeout(Duration::from_secs(1), async {
            loop {
                let event = stream
                    .recv()
                    .await?
                    .ok_or_else(|| anyhow::anyhow!("session stream closed after cancellation"))?;
                if let ProviderEvent::Execution { observation } = event
                    && observation.scope() == &next
                    && matches!(
                        observation.update(),
                        ExecutionUpdate::Finished {
                            result: ExecutionResult::Completed { .. },
                            ..
                        }
                    )
                {
                    return Ok::<_, anyhow::Error>(());
                }
            }
        })
        .await??;
        provider.shutdown().await?;
        Ok(())
    }

    /// Acceptance cannot release a response barrier or end the session's observation channel.
    #[tokio::test]
    async fn acceptance_precedes_completion_and_observation_survives_next_prompt()
    -> anyhow::Result<()> {
        let provider = Arc::new(MockProvider::default());
        let owner = request().owner()?;
        let mut stream = provider.session_events(owner.clone()).await?;
        for exchange in ["first", "second"] {
            let hold = provider
                .response_control
                .dispatch(
                    "persistent-mock",
                    MockResponseCommand::Hold {
                        mode: Some(PromptMode::Chat),
                        any_session: false,
                    },
                )
                .await?;
            let hold_id = hold["id"].as_str().unwrap().to_owned();
            let scope = InvocationScope::new(owner.clone(), exchange);
            tokio::time::timeout(
                Duration::from_secs(1),
                provider.clone().session_prompt(request(), scope.clone()),
            )
            .await??;
            provider
                .response_control
                .dispatch(
                    "persistent-mock",
                    MockResponseCommand::Wait {
                        id: hold_id.clone(),
                    },
                )
                .await?;
            while let Some(event) = stream.try_next()? {
                assert!(!matches!(event, ProviderEvent::Execution { observation }
                    if matches!(observation.update(), forge_provider_api::lifecycle::ExecutionUpdate::Finished { .. })));
            }
            provider
                .response_control
                .dispatch(
                    "persistent-mock",
                    MockResponseCommand::Release { id: hold_id },
                )
                .await?;
            tokio::time::timeout(Duration::from_secs(1), async {
                loop {
                    let event = stream
                        .recv()
                        .await?
                        .ok_or_else(|| anyhow::anyhow!("session stream closed after one prompt"))?;
                    if let ProviderEvent::Execution { observation } = event
                        && matches!(
                            observation.update(),
                            forge_provider_api::lifecycle::ExecutionUpdate::Finished { .. }
                        )
                    {
                        assert_eq!(observation.scope(), &scope);
                        return Ok::<_, anyhow::Error>(());
                    }
                }
            })
            .await??;
        }
        provider.shutdown().await?;
        assert!(stream.recv().await?.is_none());
        Ok(())
    }
}

impl forge_provider_api::progress::TurnProgress for ProviderCompletion {
    fn session_id(&self) -> Option<&forge_provider_api::ProviderSessionId> {
        self.provider_session_id.as_ref()
    }

    fn runtime(&self) -> &ProviderRuntime {
        &self.runtime
    }

    fn set_model(&mut self, model: String) {
        self.runtime.model = Some(model);
    }

    fn set_context_usage(&mut self, usage: ContextUsage) {
        self.metrics.context_usage = Some(usage);
    }

    fn record_tool_call(&mut self) {
        self.evidence.tool_called = true;
    }

    fn record_workspace_change(&mut self, changed: bool) {
        self.evidence.workspace_changed |= changed;
    }
}

impl std::ops::Deref for ProviderCompletion {
    type Target = forge_provider_api::completion::ProviderCompletion;

    fn deref(&self) -> &Self::Target {
        &self.native
    }
}

impl std::ops::DerefMut for ProviderCompletion {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.native
    }
}

impl From<forge_provider_api::completion::ProviderCompletion> for ProviderCompletion {
    fn from(native: forge_provider_api::completion::ProviderCompletion) -> Self {
        Self {
            native,
            ..Self::default()
        }
    }
}
