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
            control_context: _,
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
            control_context: None,
        }
    }

    /// Retain admitted attachment references from recovered input and the newest input.
    #[cfg(test)]
    pub(crate) fn input_attachments(&self) -> Vec<crate::attachment::InputAttachment> {
        self.recovered_input
            .iter()
            .chain(std::iter::once(&self.input))
            .flat_map(|input| input.attachments().iter().cloned())
            .collect()
    }

    /// Resolve the independent runtime without changing the visible settings scope.
    pub fn runtime_key(
        &self,
    ) -> Result<String, forge_provider_api::execution::ExecutionOwnerError> {
        Ok(self.owner()?.runtime_key())
    }
    /// Validate the provider runtime key before admission or cancellation routing.
    pub fn owner(
        &self,
    ) -> Result<ExecutionOwner, forge_provider_api::execution::ExecutionOwnerError> {
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
    fn sandbox_resolution_follows_registered_capabilities() {
        for registration in forge_providers::registry::descriptors() {
            let capability = registration.capability;
            let requested = crate::session::AccessPolicy::default();
            let effective = capability.resolve_access(requested.clone());
            assert_eq!(effective.sandbox, capability.supports_sandbox());
            assert!(requested.sandbox);
            assert_eq!(effective.write_access, requested.write_access);
            assert_eq!(effective.writable_directory, requested.writable_directory);
            assert_eq!(effective.isolation_mode, requested.isolation_mode);
            capability.validate_access(&effective).unwrap();
            let disabled = crate::session::AccessPolicy {
                sandbox: false,
                ..requested
            };
            assert!(!capability.resolve_access(disabled).sandbox);
        }
    }

    #[test]
    fn provider_instructions_describe_staged_edits_and_revision_scoped_reports() {
        let prompt = super::HARNESS_SYSTEM_MESSAGE;
        assert!(prompt.contains("plan_file_edit"));
        assert!(!prompt.contains("harness_plan_task_report"));
        assert!(!prompt.contains("harness_plan_edit"));
    }
    use super::{PromptMode, ProviderRequest};
    use crate::provider::mock_response::MockResponseCommand;
    use crate::provider::test_support::{ProviderTestExt as _, ProviderTestSession, build_mock};
    use crate::session::ApprovalMode;
    use anyhow::Context as _;
    use std::sync::Arc;

    #[tokio::test]
    async fn applies_steering_to_the_same_planning_turn() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store =
            crate::provider::submission::SubmissionStore::open(directory.path(), trace.clone())?;
        let provider = build_mock(
            directory.path().to_owned(),
            crate::provider::approval::PermissionCoordinator::transient(directory.path())?,
            trace,
        )?;
        let test_provider = ProviderTestSession::from_dyn(provider.clone())?;
        let submission = store.prepare_steering(
            &super::ProviderCatalogRequest {
                execution_owner: None,
                harness_session_id: "harness-session".into(),
                workspace: directory.path().to_string_lossy().into_owned(),
                access: Default::default(),
                approval_mode: ApprovalMode::Ask,
                provider_session_id: None,
            },
            super::ProviderInput::from_text("And be sure to modify Y"),
            forge_providers::registry::nonselectable_id().expect("registry has a fixture provider"),
        )?;
        let steering = super::SteeringInput::User {
            text: "And be sure to modify Y".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        };
        let control = provider
            .mock_response_control()
            .expect("mock bridge exposes normalized test control");
        let hold = control
            .dispatch(
                "harness-session",
                MockResponseCommand::Hold {
                    mode: Some(PromptMode::Plan),
                    any_session: false,
                },
            )
            .await?;
        let hold_id = hold["id"]
            .as_str()
            .context("mock response hold has no identity")?
            .to_owned();
        let prompt_provider = test_provider.clone();
        let workspace = directory.path().to_string_lossy().into_owned();
        let prompt = tokio::spawn(async move {
            prompt_provider
                .session_prompt_and_collect(ProviderRequest {
                    dispatch: forge_provider_api::submission::DispatchTracker::default(),
                    execution_owner: None,
                    harness_session_id: "harness-session".into(),
                    workspace,
                    input: super::ProviderInput::from_text("Refactor X"),
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
                })
                .await
        });
        control
            .dispatch(
                "harness-session",
                MockResponseCommand::Wait {
                    id: hold_id.clone(),
                },
            )
            .await?;
        let owner = crate::provider::ExecutionOwner::root("harness-session");
        assert!(test_provider.active_invocation_scope(&owner)?.is_some());
        test_provider.steer_current(owner.clone(), steering).await?;
        assert!(
            !prompt.is_finished(),
            "steering must not release the held response"
        );
        control
            .dispatch(
                "harness-session",
                MockResponseCommand::Release { id: hold_id },
            )
            .await?;
        let output = prompt.await??;
        assert!(test_provider.active_invocation_scope(&owner)?.is_none());
        assert!(matches!(
            submission.record()?.delivery,
            crate::provider::submission::SubmissionState::Accepted { .. }
        ));
        let response = output
            .event
            .iter()
            .filter_map(|event| match event {
                crate::provider::ProviderEvent::Message { update, .. } => update.text(),
                _ => None,
            })
            .collect::<Vec<_>>()
            .join("\n");
        assert!(response.contains("Refactor X"));
        assert!(response.contains("And be sure to modify Y"));
        Ok(())
    }
}
