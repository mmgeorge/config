use super::{ControlToolInvocation, apply_invocation};
use crate::plan::{PlanDocument, PlanState};
use crate::provider::{PromptMode, ProviderCompletion};
use crate::tools::ToolResult as Result;
use std::collections::{BTreeMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

/// Carries the broker-owned control state visible to one provider turn.
#[derive(Clone)]
pub struct ControlTurnContext {
    pub(crate) provider_completion: Arc<crate::provider::CompletionSlot>,
    /// Session-owned questions visible through this invocation's control tools.
    pub(crate) questions: Option<Arc<crate::question::QuestionController>>,
    /// Submit scoped question mutations without retaining the session owner through callbacks.
    pub(crate) question_events: Option<crate::exchange::QuestionEvents>,
    /// Commit live callback effects before acknowledging the native tool call.
    pub(crate) live_host_effects: Option<crate::provider::LiveHostEffectSender>,
    /// Retain registered native-child physical facts on its binding-owned observation channel.
    pub(crate) observed_file_changes: Option<crate::provider::HarnessEventSink>,
    pub(crate) question_exchange_id: Option<String>,
    /// Authenticated provider invocation bound by the host control endpoint.
    pub(crate) question_invocation: Option<crate::provider::InvocationScope>,
    pub(crate) question_discussion: bool,
    pub(crate) task_controller: Option<Arc<crate::task::SessionTaskController>>,
    pub(crate) task_permit: Option<crate::task::TaskPermit>,
    pub(crate) task_authority_progress: Option<Arc<super::TaskAuthorityProgress>>,
    pub(crate) task_generation: Option<u64>,
    pub(crate) permission_snapshot: Option<super::authority::PermissionAuthoritySnapshot>,
    pub(crate) permission_required: bool,
    /// Authorize capture's source read and proposal write through the original invocation.
    pub(crate) file_permissions:
        Option<Arc<dyn forge_provider_api::permissions::ProviderPermissions>>,
    pub(crate) task_start: Option<crate::exchange::TaskStart>,
    pub(crate) submission_cancelled: Option<Arc<std::sync::atomic::AtomicBool>>,
    pub checkpoint: Option<crate::checkpoint::ToolGate>,
    pub agent: Option<Arc<crate::agent::AgentToolScope>>,
    pub(crate) checks: Option<Arc<crate::checks::CheckToolScope>>,
    pub mode: PromptMode,
    pub planning_feedback: bool,
    pub check_shell: crate::session::CheckShell,
    pub plan_state: Option<PlanState>,
    pub plan_document: Option<PlanDocument>,
    /// Limit filesystem authoring controls to the active planning directory.
    pub(crate) plan_authoring: Option<crate::plan::PlanAuthoringScope>,
    /// Execute or Goal workflow material the provider prompt delivers as the task section.
    pub(crate) task_instructions: Option<crate::provider::TaskInstructions>,
    pub resolved_question_digest_set: HashSet<String>,
    pub has_active_elicitation: bool,
    pub has_active_execution: bool,
    pub workspace_root: Option<PathBuf>,
    pub repository: Option<Arc<super::repository::RepositoryToolScope>>,
    pub(crate) harness: Option<Arc<super::session::HarnessInspectionScope>>,
    pub(crate) lsp: Option<Arc<super::lsp::LspScope>>,
    pub execution: Option<crate::plan::execution::ExecutionControlSender>,
}

impl std::fmt::Debug for ControlTurnContext {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("ControlTurnContext")
            .field("mode", &self.mode)
            .field("planning_feedback", &self.planning_feedback)
            .field("plan_state", &self.plan_state)
            .field("plan_document", &self.plan_document)
            .field(
                "resolved_question_digest_set",
                &self.resolved_question_digest_set,
            )
            .field("has_active_elicitation", &self.has_active_elicitation)
            .field("has_active_execution", &self.has_active_execution)
            .field("workspace_root", &self.workspace_root)
            .field("repository_available", &self.repository.is_some())
            .finish()
    }
}

pub(crate) struct ProviderExecutionPreview {
    pub revision: u64,
    pub outcome: std::result::Result<ProviderCompletion, crate::provider::ProviderError>,
    pub batches: Vec<crate::provider::HostEffectBatch>,
}

impl ControlTurnContext {
    /// Fork independent native-child tool ownership after exact original registration and sink validation.
    pub(crate) fn fork_observed(
        &self,
        association: &crate::provider::ObservedAgentAssociation,
        sink: crate::provider::HarnessEventSink,
    ) -> std::result::Result<Self, crate::provider::ProviderError> {
        let rejected = || {
            crate::provider::ProviderError::Observation(
                forge_provider_api::ObservationFailure::OwnershipConflict,
            )
        };
        let scope = association.turn().cloned().ok_or_else(rejected)?;
        if self.question_invocation.as_ref() != Some(association.parent())
            || sink.scope().invocation() != Some(&scope)
        {
            return Err(rejected());
        }
        let mut context = self.clone();
        context.provider_completion = Arc::default();
        context.lsp = self
            .lsp
            .as_ref()
            .map(|lsp| lsp.fork_observed(&scope))
            .transpose()
            .map_err(|error| {
                crate::provider::lifecycle_error::classify_source(
                    forge_provider_api::ProviderOperation::TurnStart,
                    error.as_ref(),
                )
            })?;
        context.repository = self
            .repository
            .as_ref()
            .map(|repository| repository.fork_observed(&scope))
            .transpose()
            .map_err(|error| {
                crate::provider::lifecycle_error::classify_source(
                    forge_provider_api::ProviderOperation::TurnStart,
                    error.as_ref(),
                )
            })?;
        context.question_invocation = Some(scope.clone());
        context.question_exchange_id = Some(scope.exchange_id.to_string());
        context.live_host_effects = None;
        context.observed_file_changes = Some(sink);
        context.submission_cancelled = None;
        context.task_permit = self.current_task_permit().map_err(|error| {
            crate::provider::lifecycle_error::classify_source(
                forge_provider_api::ProviderOperation::TurnStart,
                error.as_ref(),
            )
        })?;
        context.task_authority_progress = None;
        context.task_start = None;
        context.agent = None;
        context.questions = None;
        context.question_events = None;
        context.checks = None;
        context.execution = None;
        context.plan_authoring = None;
        context.file_permissions = None;
        Ok(context)
    }

    /// Install one shared revision handle before permission and tool contexts are cloned.
    pub(crate) fn bind_task_authority_progress(&mut self) {
        if self.task_authority_progress.is_none() {
            self.task_authority_progress = self
                .task_permit
                .clone()
                .map(|permit| Arc::new(super::TaskAuthorityProgress::new(permit)));
        }
    }

    pub(crate) fn current_task_permit(&self) -> anyhow::Result<Option<crate::task::TaskPermit>> {
        self.task_authority_progress
            .as_ref()
            .map(|progress| progress.permit())
            .transpose()
            .map(|permit| permit.or_else(|| self.task_permit.clone()))
    }

    #[cfg(test)]
    pub(crate) fn bind_test_task_controller(
        &mut self,
        controller: Arc<crate::task::SessionTaskController>,
    ) {
        let (generation, task) = controller.callback_snapshot().expect("test task snapshot");
        self.task_generation = Some(generation);
        self.task_permit = task.map(|task| task.permit());
        self.task_controller = Some(controller);
    }
    /// Start a new provider observation while retaining this invocation's host authority.
    pub fn begin_provider_execution(
        &mut self,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        self.provider_completion.begin_attempt()
    }

    /// Preview the retained host effects without changing the native turn outcome.
    pub(crate) fn preview_provider_execution(
        &self,
        result: &forge_provider_api::TurnResult,
        native: &forge_provider_api::completion::ProviderCompletion,
        address: &forge_provider_api::TurnAddress,
    ) -> std::result::Result<ProviderExecutionPreview, crate::provider::ProviderError> {
        let preview = self.provider_completion.preview()?;
        let outcome = Self::merge_provider_execution(result, native, address, preview.completion);
        Ok(ProviderExecutionPreview {
            revision: preview.revision,
            outcome,
            batches: preview.batches,
        })
    }

    /// Consume effects only when the reviewed host revision remains current.
    pub(crate) fn commit_provider_execution(
        &self,
        revision: u64,
    ) -> std::result::Result<bool, crate::provider::ProviderError> {
        self.provider_completion.take_revision(revision)
    }

    /// Preserve native failure and explicit retry permission independently of host effects.
    fn merge_provider_execution(
        result: &forge_provider_api::TurnResult,
        native: &forge_provider_api::completion::ProviderCompletion,
        address: &forge_provider_api::TurnAddress,
        effects: Option<ProviderCompletion>,
    ) -> std::result::Result<ProviderCompletion, crate::provider::ProviderError> {
        use crate::provider::ProviderError;
        use forge_provider_api::turn::{RetryDecision, TurnOutcome};
        match result.outcome() {
            TurnOutcome::Completed { .. } => {
                let mut output = effects.unwrap_or_default();
                output.merge_native(native.clone());
                output.native.provider_checkpoint_id =
                    result.checkpoint_id().map(|id| id.as_str().to_owned());
                Ok(output)
            }
            TurnOutcome::Interrupted => Err(ProviderError::Cancelled),
            TurnOutcome::Failed {
                error,
                retry: RetryDecision::NotAllowed,
            } => Err(ProviderError::Failure(error)),
            TurnOutcome::Failed {
                error,
                retry: RetryDecision::Allowed { .. },
            } => {
                let mut retained = effects.unwrap_or_default();
                retained.merge_native(native.clone());
                if !crate::provider::retry::replay_safe(&retained) {
                    return Err(ProviderError::Failure(error));
                }
                let category =
                    if error.kind() == forge_provider_api::ProviderFailureKind::RateLimited {
                        forge_provider_api::retry::RetryCategory::RateLimited
                    } else if error.kind() == forge_provider_api::ProviderFailureKind::Transport {
                        forge_provider_api::retry::RetryCategory::TemporaryTransport
                    } else {
                        forge_provider_api::retry::RetryCategory::TemporaryCapacity
                    };
                Err(ProviderError::Retryable(Box::new(
                    forge_provider_api::retry::RetryableFailure::new(
                        address.clone(),
                        result,
                        category,
                    )?,
                )))
            }
        }
    }
    /// Build an inert context for provider calls without broker state.
    pub fn inactive(mode: PromptMode) -> Self {
        Self {
            provider_completion: Default::default(),
            questions: None,
            question_events: None,
            live_host_effects: None,
            observed_file_changes: None,
            question_exchange_id: None,
            question_invocation: None,
            question_discussion: false,
            task_controller: None,
            task_permit: None,
            task_authority_progress: None,
            task_generation: None,
            permission_snapshot: None,
            permission_required: false,
            file_permissions: None,
            task_start: None,
            submission_cancelled: None,
            checkpoint: None,
            agent: None,
            checks: None,
            mode,
            planning_feedback: false,
            check_shell: Default::default(),
            plan_state: None,
            plan_document: None,
            plan_authoring: None,
            task_instructions: None,
            resolved_question_digest_set: HashSet::new(),
            has_active_elicitation: false,
            has_active_execution: false,
            workspace_root: None,
            repository: None,
            harness: None,
            lsp: None,
            execution: None,
        }
    }
}

/// Owns provider-visible control validation for one bounded provider turn.
pub struct ControlToolRuntime {
    pub(super) lsp_mutations: Arc<super::lsp::MutationWork>,
    pub(super) context: ControlTurnContext,
    pub(super) plan_document: Option<PlanDocument>,
    pub(super) terminal: bool,
    pub(super) completion: Option<(String, String)>,
    pub(super) inspected: BTreeMap<String, String>,
}

/// Returns one accepted invocation or an idempotent provider-visible result.
#[derive(Debug)]
pub struct ControlToolResult {
    pub invocation: Option<ControlToolInvocation>,
    pub message: String,
}

impl ControlToolRuntime {
    /// Build a runtime from the broker snapshot captured at turn start.
    pub fn new(context: ControlTurnContext) -> Self {
        Self {
            lsp_mutations: Arc::default(),
            plan_document: context.plan_document.clone(),
            context,
            terminal: false,
            completion: None,
            inspected: BTreeMap::new(),
        }
    }

    /// Validate a control invocation and await broker persistence for execution controls.
    pub async fn invoke(&mut self, invocation: ControlToolInvocation) -> Result<ControlToolResult> {
        self.ensure_submission_active()?;
        if self.context.plan_authoring.is_some() {
            crate::tools::error::ensure!(
                !matches!(invocation.name.as_str(), "plan_read" | "plan_edit"),
                "{}",
                super::unavailable_tool_reason(&invocation.name)
            );
        }
        if self.context.observed_file_changes.is_some()
            && !(super::lsp::is_tool(&invocation.name)
                || super::repository::is_tool(&invocation.name))
        {
            return Err(crate::tools::ToolError::Rejected(
                "Native child context does not authorize parent workflow controls.".into(),
            ));
        }
        self.context.task_permit = self.context.current_task_permit()?;
        if let Some(authority) = super::TaskAuthority::capture(&self.context) {
            authority.validate_scope()?;
        }
        if invocation.name == "question_ask" && invocation.arguments["wait"].as_bool() == Some(true)
        {
            crate::tools::error::ensure!(
                self.context.questions.is_some(),
                "waiting questions require the session question controller"
            );
        }
        if self.context.question_discussion {
            crate::tools::error::ensure!(
                matches!(
                    invocation.name.as_str(),
                    "question_ask"
                        | "question_reply"
                        | "question_revise"
                        | "question_withdraw"
                        | "plan_read"
                        | "plan_status"
                ) || (super::session::is_tool(&invocation.name)
                    || invocation.name == super::config::NAME)
                    || (super::lsp::is_tool(&invocation.name)
                        && !super::lsp::is_mutation(&invocation.name)),
                "question discussion cannot change task state or workspace content"
            );
        }
        if let Some(controller) = self.context.questions.as_ref()
            && controller.snapshot()?.required_pending()
            && !self.context.question_discussion
        {
            crate::tools::error::ensure!(
                matches!(
                    invocation.name.as_str(),
                    "question_reply" | "question_revise" | "question_withdraw"
                ),
                "required questions must be resolved before continuing task work"
            );
        }
        if super::questions::is_tool(&invocation.name) && self.context.questions.is_some() {
            super::validate_arguments(&invocation)?;
            return super::questions::execute(self, invocation, ProviderCompletion::default())
                .await;
        }
        let mut output = ProviderCompletion::default();
        apply_invocation(&invocation, &mut output)?;
        self.invoke_decoded(invocation, output).await
    }

    /// Execute one control invocation from arguments decoded by the provider boundary.
    pub(crate) async fn invoke_decoded(
        &mut self,
        mut invocation: ControlToolInvocation,
        output: ProviderCompletion,
    ) -> Result<ControlToolResult> {
        if self.context.plan_authoring.is_some() {
            crate::tools::error::ensure!(
                !matches!(invocation.name.as_str(), "plan_read" | "plan_edit"),
                "{}",
                super::unavailable_tool_reason(&invocation.name)
            );
        }
        self.ensure_submission_active()?;
        self.context.task_permit = self.context.current_task_permit()?;
        if let Some(authority) = super::TaskAuthority::capture(&self.context) {
            authority.validate_scope()?;
        }
        invocation.admit_turn_control(
            self.context.task_controller.as_deref(),
            self.context.task_permit.as_ref(),
            self.context.mode,
        )?;
        if let Some(checkpoint) = &self.context.checkpoint {
            checkpoint.wait().await?;
        }
        if let Some(authority) = super::TaskAuthority::capture(&self.context) {
            authority.validate_scope()?;
        }
        self.ensure_submission_active()?;
        invocation.admit_turn_control(
            self.context.task_controller.as_deref(),
            self.context.task_permit.as_ref(),
            self.context.mode,
        )?;
        let identity = serde_json::to_string(&invocation)?;
        if let Some((completed, message)) = &self.completion
            && completed == &identity
        {
            return Ok(ControlToolResult {
                invocation: None,
                message: message.clone(),
            });
        }
        crate::tools::error::ensure!(
            !self.terminal,
            "this provider turn already reached a terminal control action"
        );
        if let Some(execution) = &self.context.execution {
            match invocation.name.as_str() {
                "plan_edit" => execution.require_edit_phase()?,
                "plan_submit" => execution.require_revision_phase()?,
                _ => {}
            }
        }
        match invocation.name.as_str() {
            name if super::agents::is_tool(name) => {
                super::agents::execute(self, invocation, output).await
            }
            name if super::repository::is_tool(name) => {
                super::repository::execute(self, invocation, output).await
            }
            name if super::plan::is_tool(name) => {
                super::plan::execute(self, invocation, output).await
            }
            name if super::questions::is_tool(name) => {
                super::questions::execute(self, invocation, output).await
            }
            name if super::goals::is_tool(name) => {
                super::goals::execute(self, invocation, output).await
            }
            name if super::checks::is_tool(name) => {
                super::checks::execute(self, invocation, output).await
            }
            name if super::lsp::is_tool(name) => {
                super::lsp::execute(self, invocation, output).await
            }
            name if super::session::is_tool(name) => {
                super::session::execute(self, invocation, output).await
            }
            super::config::NAME => super::config::execute(self, invocation, output).await,
            name => crate::tools::error::bail!("unknown Harness control tool: {name}"),
        }
    }

    /// Reject callbacks whose submission was cancelled before host execution.
    fn ensure_submission_active(&self) -> Result<()> {
        if self
            .context
            .submission_cancelled
            .as_ref()
            .is_some_and(|cancelled| cancelled.load(std::sync::atomic::Ordering::Acquire))
        {
            return Err(crate::broker::TurnCancelled.into());
        }
        Ok(())
    }

    /// Indicates that the current provider turn must stop after delivering its execution result.
    pub(crate) fn execution_finished(&self) -> bool {
        self.completion.is_some()
    }

    /// Return the staged canonical document after accepted plan edits.
    pub fn plan_document(&self) -> Option<&PlanDocument> {
        self.plan_document.as_ref()
    }

    pub(super) fn require_editable_plan(&self) -> Result<()> {
        self.require_readable_plan()?;
        crate::tools::error::ensure!(
            matches!(
                self.context.plan_state,
                Some(PlanState::Generating | PlanState::Revising | PlanState::AwaitingReview)
            ),
            "plan submission requires a draft or reviewable plan"
        );
        Ok(())
    }

    pub(super) fn require_readable_plan(&self) -> Result<()> {
        if self.context.mode == PromptMode::ExecutePlan && self.context.execution.is_some() {
            return Ok(());
        }
        crate::tools::error::ensure!(
            matches!(
                self.context.mode,
                PromptMode::Plan | PromptMode::PlanDiscussion
            ),
            "plan controls require Harness Plan mode"
        );
        crate::tools::error::ensure!(
            matches!(
                self.context.plan_state,
                Some(PlanState::Generating | PlanState::Revising | PlanState::AwaitingReview)
            ),
            "plan controls require an active draft or submitted plan"
        );
        Ok(())
    }

    pub(super) fn plan_version_message(&self, prefix: &str) -> String {
        format!(
            "{prefix}. Active canonical version is {}.",
            self.plan_document
                .as_ref()
                .map(|document| document.version)
                .unwrap_or_default()
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::provider::ScopeOwner;

    enum CapturePermission {
        Allow,
        DenyRead,
    }

    #[async_trait::async_trait]
    impl forge_provider_api::permissions::ProviderPermissions for CapturePermission {
        /// Apply source-read authorization at the established permission callback boundary.
        async fn authorize(
            &self,
            _scope: &forge_provider_api::InvocationScope,
            request: forge_provider_api::permissions::PermissionRequest,
        ) -> forge_provider_api::permissions::PermissionResponse {
            use forge_provider_api::permissions::{PermissionResponse, PermissionTarget};
            if matches!(self, Self::DenyRead)
                && request
                    .target_list
                    .iter()
                    .any(|target| matches!(target, PermissionTarget::Read { .. }))
            {
                PermissionResponse::Deny {
                    reason: Some("Source reads are denied".into()),
                }
            } else {
                PermissionResponse::Allow
            }
        }

        /// Admit the fixture's already-ready checkpoint boundary.
        async fn before_tool(
            &self,
            _scope: &forge_provider_api::InvocationScope,
        ) -> std::result::Result<(), crate::provider::ProviderError> {
            Ok(())
        }
    }

    /// Exercise capture authorization, its fixed destination, and the filesystem-only planning catalog.
    #[tokio::test]
    async fn filesystem_capture_preserves_resource_authority_and_fixed_destination()
    -> anyhow::Result<()> {
        let temporary = tempfile::tempdir()?;
        crate::checks::configure_fixture(temporary.path());
        std::fs::write(temporary.path().join("source.rs"), "pub fn existing() {}\n")?;
        let files = Arc::new(crate::plan::PlanFileStore::new(
            temporary.path().join("storage"),
            temporary.path(),
        )?);
        let prepared = files.prepare_entry("session", "Revise a callable.".into(), None, 1)?;
        let plan_id = prepared.record.id;
        let directory = files.authoring_directory("session", &plan_id);
        let mut context = ControlTurnContext::inactive(PromptMode::Plan);
        context.plan_state = Some(PlanState::Generating);
        context.plan_document = Some(files.read_working_document("session", &plan_id)?);
        context.workspace_root = Some(temporary.path().into());
        context.plan_authoring = Some(crate::plan::PlanAuthoringScope::new(
            files.clone(),
            "session".into(),
            plan_id.clone(),
            crate::session::WriteScope::Workspace,
        ));
        context.question_invocation = Some(crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root("session"),
            "capture",
        ));
        context.file_permissions = Some(Arc::new(CapturePermission::DenyRead));
        let catalog =
            super::super::ControlToolRegistry.definition_list_for_context(Some(&context), true);
        assert!(catalog.iter().any(|tool| tool.name == "plan_capture_file"));
        assert!(
            !catalog
                .iter()
                .any(|tool| matches!(tool.name, "plan_read" | "plan_edit"))
        );
        let submit = catalog
            .iter()
            .find(|tool| tool.name == "plan_submit")
            .unwrap();
        assert!(
            submit.input_schema["properties"]
                .get("expected_version")
                .is_none()
        );
        assert!(
            submit.input_schema["properties"]
                .get("authoring_snapshot")
                .is_none()
        );
        assert!(submit.input_schema["properties"].get("reason").is_none());
        let failed_submit = ControlToolInvocation {
            call_id: None,
            name: "plan_submit".into(),
            arguments: serde_json::json!({"plan_id": plan_id}),
            task_permit: None,
        };
        let failure = crate::tools::control_tool_failure_text(
            &failed_submit,
            &crate::tools::ToolError::Rejected("A required artifact is missing".into()),
            context.plan_document.as_ref(),
            Some(&directory),
        );
        assert!(!failure.contains("expected_version"));
        assert!(failure.contains("planning directory"));
        let invocation = |arguments| ControlToolInvocation {
            call_id: None,
            name: "plan_capture_file".into(),
            arguments,
            task_permit: None,
        };
        let mut runtime = ControlToolRuntime::new(context);
        assert!(
            runtime
                .invoke(invocation(serde_json::json!({"path":"source.rs"})))
                .await
                .is_err()
        );
        assert!(!directory.join("files/source.rs").exists());
        runtime.context.file_permissions = Some(Arc::new(CapturePermission::Allow));
        assert!(
            runtime
                .invoke(invocation(
                    serde_json::json!({"path":"source.rs","destination":"elsewhere.rs"})
                ))
                .await
                .is_err()
        );
        let result = runtime
            .invoke(invocation(serde_json::json!({"path":"source.rs"})))
            .await?;
        assert!(
            result.message.contains(
                &directory
                    .join("files")
                    .join("source.rs")
                    .display()
                    .to_string()
            )
        );
        assert!(
            std::fs::read_to_string(directory.join("files/source.rs"))?
                .contains("pub fn existing();")
        );
        std::fs::write(directory.join("files/source.rs"), "pub fn edited();\n")?;
        runtime
            .invoke(invocation(serde_json::json!({"path":"source.rs"})))
            .await?;
        assert_eq!(
            std::fs::read_to_string(directory.join("files/source.rs"))?,
            "pub fn edited();\n"
        );
        runtime.context.plan_authoring = Some(crate::plan::PlanAuthoringScope::new(
            files,
            "session".into(),
            plan_id,
            crate::session::WriteScope::Off,
        ));
        assert!(
            runtime
                .invoke(invocation(serde_json::json!({"path":"source.rs"})))
                .await
                .is_err()
        );
        Ok(())
    }

    /// Explicit retry permission still cannot replay a completed native or host effect.
    #[test]
    fn retry_requires_explicit_permission_and_safe_completed_effects() {
        use forge_provider_api::turn::{RetryDecision, TurnOutcome};
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root("retry"),
            "exchange",
        );
        let address = forge_provider_api::TurnAddress {
            thread_id: scope.owner().thread_id(),
            turn_id: scope.turn_id().unwrap(),
            scope,
        };
        let failure = forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::TurnStart,
            forge_provider_api::ProviderFailureKind::RateLimited,
        );
        for (retry, native_tool, host_edit, allowed) in [
            (RetryDecision::NotAllowed, false, false, false),
            (
                RetryDecision::Allowed { retry_after: None },
                false,
                false,
                true,
            ),
            (
                RetryDecision::Allowed { retry_after: None },
                true,
                false,
                false,
            ),
            (
                RetryDecision::Allowed { retry_after: None },
                false,
                true,
                false,
            ),
        ] {
            let result = forge_provider_api::TurnResult::new(
                TurnOutcome::Failed {
                    error: failure,
                    retry,
                },
                1,
                None,
                vec![],
                vec![],
                None,
            )
            .unwrap();
            let mut native = forge_provider_api::completion::ProviderCompletion::default();
            native.evidence.tool_called = native_tool;
            let mut effects = ProviderCompletion::default();
            effects.evidence.workspace_changed = host_edit;
            let outcome = ControlTurnContext::merge_provider_execution(
                &result,
                &native,
                &address,
                Some(effects),
            );
            assert_eq!(
                matches!(outcome, Err(crate::provider::ProviderError::Retryable(_))),
                allowed
            );
        }
    }

    use serde_json::json;

    #[tokio::test]
    async fn goal_controls_follow_bound_session_authority_and_reject_stale_results() {
        use crate::task::{SessionTaskController, Task, TaskChange};
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let initial = controller.current().unwrap().unwrap();
        controller
            .change(&mut store, Some(&initial.revision), TaskChange::Clear, 2)
            .unwrap();
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.bind_test_task_controller(controller.clone());
        let mut runtime = ControlToolRuntime::new(context);
        let invocation = |name: &str| ControlToolInvocation {
            call_id: None,
            name: name.into(),
            arguments: json!({"summary":"Finished the requested work"}),
            task_permit: None,
        };
        assert!(runtime.invoke(invocation("goal_complete")).await.is_err());
        let goal = controller
            .change(
                &mut store,
                None,
                TaskChange::Install(Box::new(Task::goal(
                    "Promoted goal".into(),
                    Default::default(),
                    3,
                ))),
                3,
            )
            .unwrap()
            .unwrap();
        // Callback authority is the task snapshot captured when the turn binds, so a turn bound
        // before the goal existed cannot complete it; a turn bound after installation can.
        let stale = runtime
            .invoke(invocation("goal_complete"))
            .await
            .unwrap_err();
        assert!(format!("{stale:?}").contains("StaleTask"), "{stale:?}");
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.bind_test_task_controller(controller.clone());
        let mut runtime = ControlToolRuntime::new(context);
        let accepted = runtime
            .invoke(invocation("goal_complete"))
            .await
            .unwrap()
            .invocation
            .unwrap();
        let serialized = serde_json::to_value(&accepted).unwrap();
        assert!(serialized.get("task_permit").is_none());
        let mut output = ProviderCompletion::default();
        apply_invocation(&accepted, &mut output).unwrap();
        let permit = output.goal_control.unwrap().permit;
        permit.validate(Some(&goal)).unwrap();
        let replacement = controller
            .change(
                &mut store,
                Some(&goal.revision),
                TaskChange::Install(Box::new(Task::goal(
                    "Replacement goal".into(),
                    Default::default(),
                    4,
                ))),
                4,
            )
            .unwrap()
            .unwrap();
        let error = controller
            .observe_goal(
                &mut store,
                &permit,
                crate::task::TaskTurnId {
                    exchange_ordinal: 1,
                    turn_ordinal: 1,
                },
                output.native.evidence,
                5,
            )
            .unwrap_err();
        assert!(error.is::<crate::task::StaleTask>());
        assert_eq!(
            controller.current().unwrap().unwrap().revision,
            replacement.revision
        );
        let decoded: ControlToolInvocation = serde_json::from_value(json!({
            "name":"goal_complete", "arguments":{"summary":"untrusted"},
            "task_permit":{"revision":replacement.revision},
            "call_id":"forged-model-identity",
        }))
        .unwrap();
        assert!(decoded.task_permit.is_none());
        assert!(
            decoded.call_id.is_none(),
            "model JSON forged an authenticated tool-call identity"
        );
    }

    #[tokio::test]
    async fn uncaptured_reads_guard_first_edits_and_preserve_broker_replay_identity() {
        let mut context = planning_context();
        let workspace = context.workspace_root.clone().unwrap();
        std::fs::write(workspace.join("Foo.rs"), "pub struct Foo;\n").unwrap();
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::open(&workspace).unwrap());
        let initial = context.plan_document.clone().unwrap();
        let mut runtime = ControlToolRuntime::new(context);
        let read = ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "plan_read".into(),
            arguments: json!({"plan_id":"plan","path":"Foo.rs"}),
        };
        let inspected = runtime.invoke(read.clone()).await.unwrap();
        assert!(inspected.message.contains("Source digest:"));
        assert_eq!(runtime.plan_document(), Some(&initial));
        let edit = ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "plan_edit".into(),
            arguments: json!({"plan_id":"plan","expected_version":1,"patch":"*** Begin Patch\n*** Update File: Foo.rs\n@@\n-pub struct Foo;\n+pub struct Revised;\n*** End Patch"}),
        };
        std::fs::write(
            workspace.join("Foo.rs"),
            "pub struct Foo;\n// Concurrent implementation edit\n",
        )
        .unwrap();
        assert!(
            runtime
                .invoke(edit.clone())
                .await
                .unwrap_err()
                .to_string()
                .contains("changed since inspection")
        );
        assert_eq!(runtime.plan_document(), Some(&initial));
        runtime.invoke(read).await.unwrap();
        let accepted = runtime.invoke(edit).await.unwrap().invocation.unwrap();
        let mut output = ProviderCompletion::default();
        apply_invocation(&accepted, &mut output).unwrap();
        let request = output.design_patch.pop().unwrap();
        assert!(request.source_digests.contains_key("Foo.rs"));
        let replayed = initial.patch_design(&workspace, request.clone()).unwrap();
        assert_eq!(runtime.plan_document(), Some(&replayed));
        std::fs::write(
            workspace.join("Foo.rs"),
            "pub struct Foo;\n// Changed after acknowledgement\n",
        )
        .unwrap();
        assert!(initial.patch_design(&workspace, request).is_err());
    }

    /// The plan is authored as files: artifact edits store text as written, plan_submit reports
    /// every artifact problem by line in one rejection, and Revert File withdraws proposals.
    #[tokio::test]
    async fn plan_artifacts_patch_status_and_revert_like_files() {
        let mut context = planning_context();
        let workspace = context.workspace_root.clone().unwrap();
        std::fs::write(
            workspace.join(".forge.json"),
            json!({"checks":{
                "build":{"trigger":{"auto":true,"paths":["**"]},
                    "action":{"command":["build"],"label":"Build","category":"build"}},
                "unit":{"trigger":{"auto":false,"paths":["**"]},
                    "action":{"command":["unit"],"label":"Unit","category":"test","after":["build"]}}
            }})
            .to_string(),
        )
        .unwrap();
        std::fs::write(workspace.join("Foo.rs"), "pub struct Foo;\n").unwrap();
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::open(&workspace).unwrap());
        let mut runtime = ControlToolRuntime::new(context);
        let call = |name: &str, arguments: serde_json::Value| ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: name.into(),
            arguments,
        };
        let edit = |version: u64, patch: &str| {
            call(
                "plan_edit",
                json!({"plan_id":"plan","expected_version":version,"patch":patch}),
            )
        };

        let overview = runtime
            .invoke(call(
                "plan_read",
                json!({"plan_id":"plan","path":"plan:overview.md"}),
            ))
            .await
            .unwrap();
        assert!(
            overview.message.contains("1: # Structured plan"),
            "{}",
            overview.message
        );
        assert!(overview.message.contains("## Manual checks"));

        let accepted = runtime
            .invoke(edit(
                1,
                "*** Begin Patch\n*** Update File: plan:overview.md\n@@\n-# Structured plan\n+# Shipped game\n@@\n ## Objective\n+Ship the game.\n+## Goals\n*** End Patch",
            ))
            .await
            .unwrap();
        assert!(
            accepted.message.contains("version is 2"),
            "{}",
            accepted.message
        );
        assert!(accepted.message.contains("Changed: plan:overview.md"));
        let document = runtime.plan_document().unwrap();
        let design = document.design.as_ref().unwrap();
        assert!(design.artifact_text["plan:overview.md"].contains("Ship the game.\n## Goals"));
        assert!(design.document.objective.is_empty());
        assert_eq!(document.title, "Shipped game");

        let checks = runtime
            .invoke(call(
                "plan_read",
                json!({"plan_id":"plan","path":"plan:checks.md"}),
            ))
            .await
            .unwrap();
        assert!(
            !checks.message.contains("unit"),
            "an unselected plan lists no checks: {}",
            checks.message
        );
        runtime
            .invoke(edit(
                2,
                "*** Begin Patch\n*** Add File: plan:checks.md\n+unit\n*** End Patch",
            ))
            .await
            .unwrap();
        runtime
            .invoke(edit(
                3,
                "*** Begin Patch\n*** Update File: Foo.rs\n@@\n-pub struct Foo;\n+pub struct Revised;\n*** End Patch",
            ))
            .await
            .unwrap();

        let submit = call(
            "plan_submit",
            json!({"plan_id":"plan","expected_version":4}),
        );
        let error = runtime.invoke(submit.clone()).await.unwrap_err();
        let failure =
            crate::tools::control_tool_failure_text(&submit, &error, runtime.plan_document(), None);
        assert!(
            failure.starts_with("plan_submit rejected version 4 with "),
            "{failure}"
        );
        for expected in [
            "error[invalid_plan_artifact]: `## Goals`: unknown section",
            "error[invalid_plan_content]: `## Background`: the section is required",
            "Fix them and retry with expected_version 4.",
        ] {
            assert!(
                failure.contains(expected),
                "missing {expected:?} in {failure}"
            );
        }
        assert!(
            !failure.contains("plan:checks.md"),
            "an unlisted prerequisite is added, not reported: {failure}"
        );

        let status = runtime
            .invoke(call("plan_status", json!({"plan_id":"plan"})))
            .await
            .unwrap();
        assert!(
            status.message.contains("planning · version 4"),
            "{}",
            status.message
        );
        assert!(status.message.contains("plan:overview.md  written"));
        assert!(status.message.contains("unit selected"));
        assert!(status.message.contains("M Foo.rs"));

        runtime
            .invoke(edit(
                4,
                "*** Begin Patch\n*** Revert File: Foo.rs\n*** Revert File: plan:overview.md\n*** End Patch",
            ))
            .await
            .unwrap();
        let document = runtime.plan_document().unwrap();
        let design = document.design.as_ref().unwrap();
        assert!(design.proposed_source.is_empty() && design.baseline.is_empty());
        assert!(!design.artifact_text.contains_key("plan:overview.md"));
        let status = runtime
            .invoke(call("plan_status", json!({"plan_id":"plan"})))
            .await
            .unwrap();
        assert!(
            status.message.contains("Files\n  (none)"),
            "{}",
            status.message
        );
        assert!(status.message.contains("plan:overview.md  empty"));
    }

    /// Source edits keep the agent's text exactly: every effective edit reports the file as
    /// changed even while its structure does not parse, a rejected submission changes nothing,
    /// a chunk copied at the wrong base indentation still applies with its nesting, an accepted
    /// submission parses the text without rewriting it, and a mismatch names its line without
    /// changing the plan.
    #[tokio::test]
    async fn source_edits_store_authored_text_and_submit_parses_it_without_changes() {
        let mut context = planning_context();
        let workspace = context.workspace_root.clone().unwrap();
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::open(&workspace).unwrap());
        let mut runtime = ControlToolRuntime::new(context);
        let call = |name: &str, arguments: serde_json::Value| ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: name.into(),
            arguments,
        };
        let edit = |version: u64, patch: &str| {
            call(
                "plan_edit",
                json!({"plan_id":"plan","expected_version":version,"patch":patch}),
            )
        };
        let read = |runtime: &ControlToolRuntime| {
            runtime
                .plan_document()
                .unwrap()
                .design
                .as_ref()
                .unwrap()
                .clone()
        };
        let authored = "/// Play every round.\npub fn play(rounds: u32);\nAlgorithm\nfor each round in rounds\n    score(round)\n";

        let overview = runtime
            .invoke(edit(
                1,
                "*** Begin Patch\n*** Add File: plan:overview.md\n+# Rounds\n+\n+## Objective\n+Play every round.\n+\n+## Background\n+Rounds are scored one at a time.\n+\n+## Design\n+One function plays each round.\n*** End Patch",
            ))
            .await
            .unwrap();
        assert!(
            overview.message.contains("version is 2"),
            "{}",
            overview.message
        );
        runtime
            .invoke(edit(
                2,
                "*** Begin Patch\n*** Add File: Game.rs\n+/// Play every round.\n+pub fn play(rounds: u32);\n+Algorithm\n+for each round in rounds\n+    score(round)\n*** End Patch",
            ))
            .await
            .unwrap();
        let design = read(&runtime);
        assert_eq!(design.proposed_source["Game.rs"], authored);
        assert!(design.parsed.is_none());

        let second_block = runtime
            .invoke(edit(
                3,
                "*** Begin Patch\n*** Update File: Game.rs\n@@\n     score(round)\n+Change\n+  Also record the round.\n*** End Patch",
            ))
            .await
            .unwrap();
        assert!(
            second_block.message.contains("version is 4")
                && second_block.message.contains("Changed: Game.rs"),
            "{}",
            second_block.message
        );
        let shown = runtime
            .invoke(call(
                "plan_read",
                json!({"plan_id":"plan","path":"Game.rs"}),
            ))
            .await
            .unwrap();
        assert!(shown.message.contains("6: Change"), "{}", shown.message);

        let another_block = runtime
            .invoke(edit(
                4,
                "*** Begin Patch\n*** Update File: Game.rs\n@@\n   Also record the round.\n+Change\n+  Record it twice.\n*** End Patch",
            ))
            .await
            .unwrap();
        assert!(
            another_block.message.contains("version is 5")
                && another_block.message.contains("Changed: Game.rs"),
            "an edit of text that does not parse still reports its file: {}",
            another_block.message
        );

        let before_rejection = crate::codec::to_vec(runtime.plan_document().unwrap()).unwrap();
        let submit = call(
            "plan_submit",
            json!({"plan_id":"plan","expected_version":5}),
        );
        let error = runtime.invoke(submit.clone()).await.unwrap_err();
        let failure =
            crate::tools::control_tool_failure_text(&submit, &error, runtime.plan_document(), None);
        for expected in [
            "Game.rs:6:1: error[invalid_declaration_structure]:",
            "already has a behavior block",
            "Fix them and retry with expected_version 5.",
        ] {
            assert!(
                failure.contains(expected),
                "missing {expected:?} in {failure}"
            );
        }
        assert_eq!(
            crate::codec::to_vec(runtime.plan_document().unwrap()).unwrap(),
            before_rejection,
            "a rejected submission leaves the plan byte-identical"
        );

        runtime
            .invoke(edit(
                5,
                "*** Begin Patch\n*** Update File: Game.rs\n@@\n-Change\n-  Also record the round.\n-Change\n-  Record it twice.\n*** End Patch",
            ))
            .await
            .unwrap();
        assert_eq!(read(&runtime).proposed_source["Game.rs"], authored);

        runtime
            .invoke(edit(
                6,
                "*** Begin Patch\n*** Update File: Game.rs\n@@\n   for each round in rounds\n       score(round)\n+      finish(round)\n*** End Patch",
            ))
            .await
            .unwrap();
        let draft = read(&runtime);
        assert!(
            draft.proposed_source["Game.rs"]
                .ends_with("for each round in rounds\n    score(round)\n    finish(round)\n"),
            "{}",
            draft.proposed_source["Game.rs"]
        );

        let accepted = runtime
            .invoke(call(
                "plan_submit",
                json!({"plan_id":"plan","expected_version":7}),
            ))
            .await
            .unwrap();
        assert!(
            accepted.message.contains("submitted for review"),
            "{}",
            accepted.message
        );
        let submitted = read(&runtime);
        assert_eq!(
            submitted.proposed_source, draft.proposed_source,
            "an accepted submission leaves the proposed source byte-identical"
        );
        assert_eq!(submitted.artifact_text, draft.artifact_text);
        assert_eq!(
            submitted.parsed().unwrap().proposed_calls["Game.rs"][0]
                .behavior
                .as_ref()
                .unwrap()
                .text(),
            "for each round in rounds\n    score(round)\n    finish(round)"
        );

        let mismatch = edit(
            7,
            "*** Begin Patch\n*** Update File: Game.rs\n@@\n-  for each turn in turns\n+  for each turn in rounds\n*** End Patch",
        );
        let error = runtime.invoke(mismatch.clone()).await.unwrap_err();
        let failure = crate::tools::control_tool_failure_text(
            &mismatch,
            &error,
            runtime.plan_document(),
            None,
        );
        for expected in [
            "Game.rs: chunk 1 does not match: its first line `  for each turn in turns` is not in the file",
            "No changes were applied. Fix them and retry with expected_version 7.",
        ] {
            assert!(
                failure.contains(expected),
                "missing {expected:?} in {failure}"
            );
        }
        assert_eq!(runtime.plan_document().unwrap().version, 7);
        assert!(
            read(&runtime).parsed.is_some(),
            "a failed edit keeps the submitted structure"
        );
    }

    /// A source file holds only behavior blocks: `plan_read` shows no reference sections, and a
    /// written reference section is rejected at submission while an Algorithm-only file submits.
    #[tokio::test]
    async fn source_files_hold_only_behavior_blocks() {
        let mut context = planning_context();
        let workspace = context.workspace_root.clone().unwrap();
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::open(&workspace).unwrap());
        let mut runtime = ControlToolRuntime::new(context);
        let call = |name: &str, arguments: serde_json::Value| ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: name.into(),
            arguments,
        };
        let edit = |version: u64, patch: &str| {
            call(
                "plan_edit",
                json!({"plan_id":"plan","expected_version":version,"patch":patch}),
            )
        };
        runtime
            .invoke(edit(
                1,
                "*** Begin Patch\n*** Add File: plan:overview.md\n+# Rounds\n+\n+## Objective\n+Score every round.\n+\n+## Background\n+Rounds are scored by a scorer.\n+\n+## Design\n+Play passes the scorer to each round.\n*** End Patch",
            ))
            .await
            .unwrap();
        runtime
            .invoke(edit(
                2,
                "*** Begin Patch\n*** Add File: Rounds.rs\n+/// Score one round.\n+pub fn score(round: u32) -> u32;\n+Algorithm\n+  return round\n+\n+/// Play every round with the scorer.\n+pub fn play(rounds: u32);\n+Algorithm\n+  for each round in rounds\n+    apply(score, round)\n+Accesses\n+  score\n*** End Patch",
            ))
            .await
            .unwrap();
        let rejected = runtime
            .invoke(call(
                "plan_submit",
                json!({"plan_id":"plan","expected_version":3}),
            ))
            .await
            .unwrap_err();
        assert!(
            format!("{rejected:#}").contains("Accesses sections are not written in plans"),
            "{rejected:#}"
        );

        runtime
            .invoke(edit(
                3,
                "*** Begin Patch\n*** Update File: Rounds.rs\n@@\n     apply(score, round)\n-Accesses\n-  score\n*** End Patch",
            ))
            .await
            .unwrap();
        let accepted = runtime
            .invoke(call(
                "plan_submit",
                json!({"plan_id":"plan","expected_version":4}),
            ))
            .await
            .unwrap();
        assert!(
            accepted.message.contains("submitted for review"),
            "{}",
            accepted.message
        );
        let design = runtime.plan_document().unwrap().design.clone().unwrap();
        assert!(
            design.parsed().unwrap().proposed_calls["Rounds.rs"]
                .iter()
                .all(|body| body.behavior.is_some())
        );
    }

    fn planning_context() -> ControlTurnContext {
        let workspace = tempfile::tempdir().unwrap().keep();
        crate::checks::configure_fixture(&workspace);
        ControlTurnContext {
            provider_completion: Default::default(),
            questions: None,
            question_events: None,
            live_host_effects: None,
            observed_file_changes: None,
            question_exchange_id: None,
            question_invocation: None,
            question_discussion: false,
            task_start: None,
            submission_cancelled: None,
            checkpoint: None,
            agent: None,
            checks: None,
            mode: PromptMode::Plan,
            planning_feedback: false,
            check_shell: Default::default(),
            plan_state: Some(PlanState::Generating),
            plan_document: Some(crate::plan::test_fixture("plan", "Initial")),
            plan_authoring: None,
            task_instructions: None,
            resolved_question_digest_set: HashSet::new(),
            has_active_elicitation: false,
            has_active_execution: false,
            task_controller: None,
            workspace_root: Some(workspace),
            task_permit: None,
            task_authority_progress: None,
            task_generation: None,
            permission_snapshot: None,
            permission_required: false,
            file_permissions: None,
            repository: None,
            harness: None,
            lsp: None,
            execution: None,
        }
    }
}
