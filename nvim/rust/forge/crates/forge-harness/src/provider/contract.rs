use super::*;
use crate::goal::TurnEvidence;
use crate::plan::{PlanQuestionAnswer, PlanQuestionSet, PlanQuestionWithdrawal};
use crate::session::ApprovalMode;
use anyhow::Result;
use serde::{Deserialize, Serialize};

pub(crate) const HARNESS_SYSTEM_MESSAGE: &str = include_str!("../plan/prompts/system.md");

/// Represents one prompt sent across a provider boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderRequest {
    /// Observe this live attempt's handoff; restoring saved requests starts a fresh tracker.
    #[serde(skip, default)]
    pub dispatch: super::DispatchTracker,
    /// Separates the agent runtime from its visible root session and settings scope.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<ThreadOwner>,
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
    /// Identify child reports actually carried by this original explicit user input.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub acknowledged_child_reports: Vec<ExchangeId>,
    #[serde(skip, default)]
    pub control_context: Option<crate::tools::ControlTurnContext>,
    /// The child agent this request runs as, delivered as the standing `<agent>` section; root
    /// requests carry none.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub child_agent: Option<forge_provider_api::forge::ChildAgent>,
}

impl ProviderRequest {
    /// Capture persisted invocation values without retaining dispatch, receipt, or tool capabilities.
    pub(crate) fn invocation_snapshot(&self) -> Self {
        let Self {
            dispatch: _,
            execution_owner,
            harness_session_id,
            workspace,
            input,
            mode,
            model,
            effort,
            context_window,
            service_tier,
            access,
            approval_mode,
            provider_session_id,
            submission: _,
            recovered_input,
            acknowledged_child_reports,
            control_context: _,
            child_agent,
        } = self;
        Self {
            dispatch: Default::default(),
            execution_owner: execution_owner.clone(),
            harness_session_id: harness_session_id.clone(),
            workspace: workspace.clone(),
            input: input.clone(),
            mode: *mode,
            model: model.clone(),
            effort: effort.clone(),
            context_window: context_window.clone(),
            service_tier: *service_tier,
            access: access.clone(),
            approval_mode: *approval_mode,
            provider_session_id: provider_session_id.clone(),
            submission: None,
            recovered_input: recovered_input.clone(),
            acknowledged_child_reports: acknowledged_child_reports.clone(),
            control_context: None,
            child_agent: child_agent.clone(),
        }
    }

    /// Resolve the independent runtime without changing the visible settings scope.
    pub fn runtime_key(&self) -> Result<String, crate::provider::ThreadOwnerError> {
        Ok(self.owner()?.runtime_key())
    }
    /// Validate the provider runtime key before admission or cancellation routing.
    pub fn owner(&self) -> Result<ThreadOwner, crate::provider::ThreadOwnerError> {
        ThreadOwner::resolve(&self.execution_owner, &self.harness_session_id)
    }
}

pub use forge_provider_api::checklist::{
    ChecklistStatus, ProviderChecklistEntry, ProviderChecklistUpdate,
};

pub use forge_provider_api::change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};

/// Collected provider events and their completion metadata for non-streaming callers.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProviderOutput {
    pub event: Vec<HarnessEvent>,
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
    pub design_patch: Vec<crate::plan::DesignPatchRequest>,
    pub plan_submit: Option<PlanSubmitRequest>,
    pub plan_read: Option<String>,
    pub plan_question: Option<PlanQuestionSet>,
    pub question_answer: Option<PlanQuestionAnswer>,
    pub question_withdrawal: Option<PlanQuestionWithdrawal>,
    pub control_error: Option<String>,
    /// Prevent replay after independently owned native descendant work was admitted.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub(crate) descendant_work_admitted: bool,
    /// Retain committed host file effects under their exact callback and invocation identities.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub(crate) host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
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
            mut host_file_changes,
            descendant_work_admitted,
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
        self.host_file_changes.append(&mut host_file_changes);
        self.descendant_work_admitted |= descendant_work_admitted;
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
    #[serde(default)]
    pub expected_version: u64,
    /// Identify host-captured planning inputs, never a caller-selected filesystem location.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(crate) authoring_snapshot: Option<String>,
}

/// Describes a provider fork after the Harness child identity is allocated.
#[derive(Clone, Debug)]
pub struct ProviderForkRequest {
    pub source: ProviderRequest,
    pub target_harness_session_id: String,
    pub checkpoint_id: Option<String>,
    /// Distinguishes native attempts for one target. The bridge retains the first request and
    /// outcome under each operation identity, so a later attempt needs an identity of its own.
    pub attempt_id: String,
}

pub type Provider = super::bridge::ProviderBridge;

/// Build a provider strategy from an explicit launch descriptor.
pub fn build(
    launch: ProviderLaunch,
    permission_coordinator: std::sync::Arc<approval::PermissionCoordinator>,
    trace: std::sync::Arc<crate::trace::TraceStore>,
    settings_root: std::path::PathBuf,
) -> Result<std::sync::Arc<Provider>> {
    let native = forge_providers::registry::create(
        launch,
        crate::storage::state::directory(&settings_root),
    )?;
    Ok(std::sync::Arc::new(super::bridge::ProviderBridge::new(
        native,
        permission_coordinator,
        trace,
        settings_root,
    )))
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
