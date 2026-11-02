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
    pub(crate) questions: Option<Arc<crate::question::QuestionController>>,
    pub(crate) question_exchange_id: Option<String>,
    pub(crate) question_discussion: bool,
    pub(crate) task_controller: Option<Arc<crate::task::SessionTaskController>>,
    pub(crate) task_permit: Option<crate::task::TaskPermit>,
    pub(crate) native_goal: Option<String>,
    pub(crate) submission_cancelled: Option<Arc<std::sync::atomic::AtomicBool>>,
    pub checkpoint: Option<crate::checkpoint::ToolGate>,
    pub agent: Option<Arc<crate::agent::AgentToolScope>>,
    pub(crate) checks: Option<Arc<crate::checks::CheckToolScope>>,
    pub mode: PromptMode,
    pub planning_feedback: bool,
    pub check_shell: crate::session::CheckShell,
    pub plan_state: Option<PlanState>,
    pub plan_document: Option<PlanDocument>,
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
            .field("native_goal", &self.native_goal)
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

impl ControlTurnContext {
    /// Build an inert context for provider calls without broker state.
    pub fn inactive(mode: PromptMode) -> Self {
        Self {
            questions: None,
            question_exchange_id: None,
            question_discussion: false,
            task_controller: None,
            task_permit: None,
            native_goal: None,
            submission_cancelled: None,
            checkpoint: None,
            agent: None,
            checks: None,
            mode,
            planning_feedback: false,
            check_shell: Default::default(),
            plan_state: None,
            plan_document: None,
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
    pub(super) context: ControlTurnContext,
    pub(super) plan_document: Option<PlanDocument>,
    pub(super) terminal: bool,
    pub(super) completion: Option<(String, String)>,
    pub(super) inspected: BTreeMap<String, String>,
    pub(super) patch_source_digests: BTreeMap<u64, BTreeMap<String, String>>,
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
            plan_document: context.plan_document.clone(),
            context,
            terminal: false,
            completion: None,
            inspected: BTreeMap::new(),
            patch_source_digests: BTreeMap::new(),
        }
    }

    /// Validate a control invocation and await broker persistence for execution controls.
    pub async fn invoke(&mut self, invocation: ControlToolInvocation) -> Result<ControlToolResult> {
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
                        | "plan_file_read"
                        | "plan_overview_read"
                ) || (super::session::is_tool(&invocation.name)
                    || invocation.name == super::config::NAME)
                    || super::lsp::is_tool(&invocation.name),
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
        invocation.admit_turn_control(
            self.context.task_controller.as_deref(),
            self.context.task_permit.as_ref(),
            self.context.mode,
        )?;
        if let Some(checkpoint) = &self.context.checkpoint {
            checkpoint.wait().await?;
        }
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
        if matches!(
            invocation.name.as_str(),
            "plan_file_edit" | "plan_overview_edit" | "plan_submit"
        ) && let Some(execution) = &self.context.execution
        {
            execution.require_revision_phase()?;
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

    /// Indicates that the current provider turn must stop after delivering its execution result.
    pub(crate) fn execution_finished(&self) -> bool {
        self.completion.is_some()
    }

    /// Return the staged canonical document after accepted plan edits.
    pub fn plan_document(&self) -> Option<&PlanDocument> {
        self.plan_document.as_ref()
    }

    /// Retain first-capture identities when transports collect a patch for broker replay.
    pub(crate) fn patch_source_digests(&self, version: u64) -> Option<&BTreeMap<String, String>> {
        self.patch_source_digests.get(&version)
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

    #[tokio::test]
    async fn persistent_questions_append_without_terminating_each_workflow() -> Result<()> {
        for mode in [
            PromptMode::Chat,
            PromptMode::Plan,
            PromptMode::ExecutePlan,
            PromptMode::GoalContinuation,
        ] {
            let (root, _store, _tasks) = crate::task::SessionTaskController::test_goal();
            let controller = Arc::new(crate::question::QuestionController::open(
                root.path(),
                "session",
            )?);
            let mut context = ControlTurnContext::inactive(mode);
            context.questions = Some(controller.clone());
            context.question_exchange_id = Some("exchange".into());
            let mut runtime = ControlToolRuntime::new(context);
            let mut returned_ids = Vec::new();
            for _ in 0..2 {
                let result = runtime.invoke(ControlToolInvocation { task_permit: None, name: "question_ask".into(), arguments: serde_json::json!({
                    "questions": [{"header":"Layout", "question":"Choose a layout", "options":[]}]
                }) }).await?;
                let result: serde_json::Value = serde_json::from_str(&result.message)?;
                returned_ids.push(result["question_ids"][0].as_str().unwrap().to_owned());
                assert!(!runtime.terminal);
            }
            assert_eq!(controller.snapshot()?.questions.len(), 2);
            assert_ne!(returned_ids[0], returned_ids[1]);
            let mut pending = Box::pin(runtime.invoke(ControlToolInvocation { task_permit: None, name: "question_ask".into(), arguments: serde_json::json!({
                "wait":true, "questions":[{"header":"Target", "question":"Choose the target", "options":[]}]
            }) }));
            assert!(futures_util::poll!(pending.as_mut()).is_pending());
            assert!(controller.snapshot()?.required_pending());
            assert!(!controller.snapshot()?.required_in_exchange("exchange"));
            let id = controller.snapshot()?.questions[2].id.clone();
            controller.apply(crate::question::QuestionCommand {
                operation_id: "answer".into(),
                question_id: id,
                revision: 1,
                action: crate::question::QuestionAction::Answer {
                    answer: crate::question::QuestionAnswer::Other {
                        text: "Desktop".into(),
                    },
                },
            })?;
            let result: serde_json::Value = serde_json::from_str(&pending.await?.message)?;
            assert_eq!(
                result["answers"][0]["resolution"]["answer"]["text"],
                "Desktop"
            );
            assert!(controller.outbox()?.is_empty());
            assert!(!controller.snapshot()?.required_pending());
            assert!(!runtime.terminal);
        }
        Ok(())
    }
    use serde_json::json;

    #[tokio::test]
    async fn goal_controls_follow_live_session_authority_and_reject_stale_results() {
        use crate::task::{SessionTaskController, Task, TaskChange};
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let initial = controller.current().unwrap().unwrap();
        controller
            .change(&mut store, Some(&initial.revision), TaskChange::Clear, 2)
            .unwrap();
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.task_controller = Some(controller.clone());
        let mut runtime = ControlToolRuntime::new(context);
        let invocation = |name: &str| ControlToolInvocation {
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
                    false,
                    3,
                ))),
                3,
            )
            .unwrap()
            .unwrap();
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
                    false,
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
        }))
        .unwrap();
        assert!(decoded.task_permit.is_none());
    }

    #[tokio::test]
    async fn selected_plan_checks_are_versioned_and_replayed_without_changing_configuration() {
        let mut context = planning_context();
        let workspace = context.workspace_root.clone().unwrap();
        let settings = json!({"checks":{"manual":{"trigger":{"auto":false},"action":{"command":"print selected","label":"Manual","category":"test"}}}});
        std::fs::write(workspace.join(".forge.json"), settings.to_string()).unwrap();
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::default());
        let initial = context.plan_document.clone().unwrap();
        let mut runtime = ControlToolRuntime::new(context);
        let invocation = |version, ids| ControlToolInvocation {
            task_permit: None,
            name: "plan_overview_edit".into(),
            arguments: json!({"plan_id":"plan","expected_version":version,"checks":ids}),
        };
        let stale = runtime
            .invoke(invocation(99, vec!["manual"]))
            .await
            .unwrap_err();
        assert!(
            stale.to_string().contains("design version conflict"),
            "{stale:#}"
        );
        let missing = runtime
            .invoke(invocation(1, vec!["missing"]))
            .await
            .unwrap_err();
        assert!(
            missing
                .to_string()
                .contains("unknown planned check: missing"),
            "{missing:#}"
        );
        let accepted = runtime
            .invoke(invocation(1, vec!["manual", "manual"]))
            .await
            .unwrap();
        assert!(accepted.message.contains("Check preview"));
        let staged = runtime.plan_document().unwrap().clone();
        assert_eq!(
            staged.design.as_ref().unwrap().document.checks,
            vec!["manual"]
        );
        let mut output = ProviderCompletion::default();
        apply_invocation(&accepted.invocation.unwrap(), &mut output).unwrap();
        assert_eq!(
            initial
                .patch_design(&workspace, output.design_patch.pop().unwrap())
                .unwrap(),
            staged
        );
        let duplicate = runtime
            .invoke(invocation(staged.version, vec!["manual"]))
            .await
            .unwrap();
        assert!(duplicate.invocation.is_none());
        assert_eq!(runtime.plan_document().unwrap().version, staged.version);
        let cleared = runtime
            .invoke(invocation(staged.version, Vec::<&str>::new()))
            .await
            .unwrap();
        let after_clear = runtime.plan_document().unwrap();
        assert!(
            after_clear
                .design
                .as_ref()
                .unwrap()
                .document
                .checks
                .is_empty()
        );
        assert_eq!(after_clear.version, staged.version + 1);
        let mut replay = ProviderCompletion::default();
        apply_invocation(&cleared.invocation.unwrap(), &mut replay).unwrap();
        assert_eq!(
            staged
                .patch_design(&workspace, replay.design_patch.pop().unwrap())
                .unwrap(),
            *after_clear
        );
        assert_eq!(
            std::fs::read_to_string(workspace.join(".forge.json")).unwrap(),
            settings.to_string()
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
            task_permit: None,
            name: "plan_file_read".into(),
            arguments: json!({"plan_id":"plan","path":"Foo.rs"}),
        };
        let inspected = runtime.invoke(read.clone()).await.unwrap();
        assert!(inspected.message.contains("Source digest:"));
        assert_eq!(runtime.plan_document(), Some(&initial));
        let edit = ControlToolInvocation {
            task_permit: None,
            name: "plan_file_edit".into(),
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
        let crate::plan::DesignEditRequest::File(file) = &request else {
            panic!("expected file edit")
        };
        assert!(file.source_digests.contains_key("Foo.rs"));
        let replayed = initial.patch_design(&workspace, request.clone()).unwrap();
        assert_eq!(runtime.plan_document(), Some(&replayed));
        std::fs::write(
            workspace.join("Foo.rs"),
            "pub struct Foo;\n// Changed after acknowledgement\n",
        )
        .unwrap();
        assert!(initial.patch_design(&workspace, request).is_err());
    }

    #[tokio::test]
    async fn overview_edits_preserve_files_checks_and_replay_partial_fields() {
        let mut context = planning_context();
        let workspace = context.workspace_root.clone().unwrap();
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Original objective".into();
        design.document.background = "Existing behavior".into();
        design.document.design = "Keep ownership local".into();
        design.document.usage = Some("Old example".into());
        design.document.requirements = vec!["Old requirement".into()];
        design.document.checks = vec!["manual".into()];
        design.document.disabled_checks = vec!["optional".into()];
        design
            .proposed
            .insert("owner.rs".into(), "pub struct Owner;\n".into());
        context.plan_document.as_mut().unwrap().design = Some(design);
        let original = context.plan_document.clone().unwrap();
        let mut runtime = ControlToolRuntime::new(context);
        let edited = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_edit".into(),
                arguments: json!({"plan_id":"plan", "expected_version":1,
                "title":"Reviewed design", "objective":"Revised objective",
                "usage":null, "requirements":[]}),
            })
            .await
            .unwrap();
        let saved = runtime.plan_document().unwrap().clone();
        let saved_design = saved.design.as_ref().unwrap();
        assert_eq!(saved.version, 2);
        assert_eq!(saved.title, "Reviewed design");
        assert_eq!(saved_design.document.objective, "Revised objective");
        assert_eq!(saved_design.document.background, "Existing behavior");
        assert!(saved_design.document.usage.is_none());
        assert!(saved_design.document.requirements.is_empty());
        assert_eq!(
            saved_design.proposed,
            original.design.as_ref().unwrap().proposed
        );
        assert_eq!(saved_design.document.checks, ["manual"]);
        assert_eq!(saved_design.document.disabled_checks, ["optional"]);
        let mut replay = ProviderCompletion::default();
        apply_invocation(&edited.invocation.unwrap(), &mut replay).unwrap();
        assert_eq!(
            original
                .patch_design(&workspace, replay.design_patch.pop().unwrap())
                .unwrap(),
            saved
        );

        let read = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_read".into(),
                arguments: json!({"plan_id":"plan"}),
            })
            .await
            .unwrap();
        let overview: serde_json::Value = serde_json::from_str(&read.message).unwrap();
        assert_eq!(overview["overview"]["objective"], "Revised objective");
        assert_eq!(overview["files"]["paths"], json!(["owner.rs"]));
        assert!(!read.message.contains("plan.json"));

        for invocation in [
            ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_edit".into(),
                arguments: json!({"plan_id":"plan","expected_version":1,"objective":"Stale"}),
            },
            ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_edit".into(),
                arguments: json!({"plan_id":"plan","expected_version":2,"title":""}),
            },
            ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_edit".into(),
                arguments: json!({"plan_id":"plan","expected_version":2,"patch":"ignored"}),
            },
            ControlToolInvocation {
                task_permit: None,
                name: "plan_file_read".into(),
                arguments: json!({"plan_id":"plan","path":"plan.json"}),
            },
            ControlToolInvocation {
                task_permit: None,
                name: "plan_file_edit".into(),
                arguments: json!({"plan_id":"plan","expected_version":2,"patch":"*** Begin Patch\n*** Add File: other.rs\n+pub struct Other;\n*** Update File: plan.json\n@@\n*** End Patch"}),
            },
        ] {
            assert!(runtime.invoke(invocation).await.is_err());
            assert_eq!(runtime.plan_document(), Some(&saved));
        }
        let unchanged = runtime.invoke(ControlToolInvocation {
            task_permit: None, name:"plan_overview_edit".into(),
            arguments:json!({"plan_id":"plan","expected_version":2,"objective":"Revised objective"}),
        }).await.unwrap();
        assert!(unchanged.invocation.is_none());
        assert_eq!(runtime.plan_document(), Some(&saved));
        assert!(!workspace.join("owner.rs").exists());
        assert!(!workspace.join("other.rs").exists());
    }

    fn planning_context() -> ControlTurnContext {
        let workspace = tempfile::tempdir().unwrap().keep();
        crate::checks::configure_fixture(&workspace);
        ControlTurnContext {
            questions: None,
            question_exchange_id: None,
            question_discussion: false,
            native_goal: None,
            submission_cancelled: None,
            checkpoint: None,
            agent: None,
            checks: None,
            mode: PromptMode::Plan,
            planning_feedback: false,
            check_shell: Default::default(),
            plan_state: Some(PlanState::Generating),
            plan_document: Some(crate::plan::test_fixture("plan", "Initial")),
            resolved_question_digest_set: HashSet::new(),
            has_active_elicitation: false,
            has_active_execution: false,
            task_controller: None,
            workspace_root: Some(workspace),
            task_permit: None,
            repository: None,
            harness: None,
            lsp: None,
            execution: None,
        }
    }

    #[tokio::test]
    async fn declaration_tools_stage_versions_and_recover_atomic_failures() {
        let mut context = planning_context();
        context.mode = PromptMode::PlanDiscussion;
        context.plan_state = Some(PlanState::AwaitingReview);
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::default());
        context
            .plan_document
            .as_mut()
            .unwrap()
            .design
            .as_mut()
            .unwrap()
            .document
            .objective = "Own private state.".into();
        context
            .plan_document
            .as_mut()
            .unwrap()
            .design
            .as_mut()
            .unwrap()
            .document
            .background = "The fixture contains the declarations under review.".into();
        context
            .plan_document
            .as_mut()
            .unwrap()
            .design
            .as_mut()
            .unwrap()
            .document
            .requirements = vec!["Preserve the declared behavior and ownership.".into()];
        let mut runtime = ControlToolRuntime::new(context);
        let patch = "*** Begin Patch\n*** Add File: src/lib.rs\n+pub struct Owner;\n*** End Patch";
        assert!(
            runtime
                .invoke(ControlToolInvocation {
                    task_permit: None,
                    name: "plan_submit".into(),
                    arguments: json!({"plan_id":"plan","expected_version":1})
                })
                .await
                .is_err()
        );
        let change = |version, patch: &str| ControlToolInvocation {
            task_permit: None,
            name: "plan_file_edit".into(),
            arguments: json!({"plan_id":"plan","expected_version":version,"patch":patch}),
        };
        assert!(runtime.invoke(change(99, patch)).await.is_err());
        assert!(
            runtime
                .invoke(change(
                    1,
                    "*** Begin Patch\n*** Add File: src/lib.rs\n+fn bad() {}\n*** End Patch"
                ))
                .await
                .is_err()
        );
        assert_eq!(runtime.plan_document().unwrap().version, 1);
        let inventory = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_read".into(),
                arguments: json!({"plan_id":"plan"}),
            })
            .await
            .unwrap();
        assert!(!inventory.message.contains("plan.json"));
        runtime.invoke(change(1, patch)).await.unwrap();
        let overview_edit = |version| ControlToolInvocation {
            task_permit: None,
            name: "plan_overview_edit".into(),
            arguments: json!({"plan_id":"plan","expected_version":version,"design":"Introduce an owner for private state."}),
        };
        runtime.invoke(overview_edit(2)).await.unwrap();
        let metadata = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_read".into(),
                arguments: json!({"plan_id":"plan"}),
            })
            .await
            .unwrap();
        assert!(metadata.message.contains("Introduce an owner"));
        runtime.invoke(overview_edit(3)).await.unwrap();
        assert_eq!(runtime.plan_document().unwrap().version, 3);
        runtime.invoke(change(3,"*** Begin Patch\n*** Update File: src/lib.rs\n@@\n-pub struct Owner;\n+pub struct Owner { private: u64 }\n*** End Patch")).await.unwrap();
        let read = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_file_read".into(),
                arguments: json!({"plan_id":"plan","path":"src/lib.rs"}),
            })
            .await
            .unwrap();
        assert!(read.message.contains("private: u64"));
        let draft = runtime.plan_document().unwrap().clone();
        let error = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_submit".into(),
                arguments: json!({"plan_id":"plan","expected_version":4}),
            })
            .await
            .unwrap_err();
        assert!(error.to_string().contains("private"));
        assert_eq!(runtime.plan_document(), Some(&draft));
        let declaration = &runtime
            .plan_document()
            .unwrap()
            .design
            .as_ref()
            .unwrap()
            .proposed["src/lib.rs"];
        let repair_patch = format!(
            "*** Begin Patch\n*** Update File: src/lib.rs\n@@\n {}\n+\n+pub fn inspect(owner: &Owner);\n+Algorithm\n+  inspect_value(owner.private)\n+Accesses\n+  Owner::private\n*** End Patch",
            declaration.lines().last().unwrap()
        );
        runtime.invoke(change(4, &repair_patch)).await.unwrap();
        runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_submit".into(),
                arguments: json!({"plan_id":"plan","expected_version":5}),
            })
            .await
            .unwrap();
        assert!(!runtime.terminal);
        runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_overview_read".into(),
                arguments: json!({"plan_id":"plan"}),
            })
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn submitted_algorithms_remain_editable_and_only_the_latest_submission_is_replayed() {
        let mut context = planning_context();
        context.plan_state = Some(PlanState::AwaitingReview);
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Return the current value.".into();
        design.document.background = "The value is exposed through a public function.".into();
        design.document.design = "Keep the public function and revise its returned value.".into();
        design
            .proposed
            .insert("api.rs".into(), "pub fn value() -> u32;\n".into());
        design.proposed_calls.insert(
            "api.rs".into(),
            vec![crate::plan::FunctionBody {
                owner: "value".into(),
                evidence: None,
                call: Some(Vec::new()),
                behavior: Some(crate::plan::FunctionBehavior::Algorithm("return 0".into())),
            }],
        );
        context.plan_document.as_mut().unwrap().design = Some(design);
        let initial = context.plan_document.clone().unwrap();
        let workspace = context.workspace_root.clone().unwrap();
        let mut runtime = ControlToolRuntime::new(context);
        let mut output = ProviderCompletion::default();
        let submit = |version| ControlToolInvocation {
            task_permit: None,
            name: "plan_submit".into(),
            arguments: json!({"plan_id":"plan","expected_version":version}),
        };
        for _ in 0..2 {
            let accepted = runtime.invoke(submit(1)).await.unwrap();
            apply_invocation(&accepted.invocation.unwrap(), &mut output).unwrap();
        }
        let read = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_file_read".into(),
                arguments: json!({"plan_id":"plan","path":"api.rs"}),
            })
            .await
            .unwrap();
        assert!(read.message.contains("return 0"));
        let accepted = runtime.invoke(ControlToolInvocation { task_permit: None,
            name: "plan_file_edit".into(), arguments: json!({"plan_id":"plan","expected_version":1,
                "patch":"*** Begin Patch\n*** Update File: api.rs\n@@\n-  return 0\n+  return 1\n*** End Patch"}),
        }).await.unwrap();
        apply_invocation(&accepted.invocation.unwrap(), &mut output).unwrap();
        assert!(
            output.plan_submit.is_none(),
            "an edit retained the older accepted submission"
        );
        assert!(
            runtime.invoke(submit(1)).await.is_err(),
            "a stale version was resubmitted"
        );
        let accepted = runtime.invoke(submit(2)).await.unwrap();
        apply_invocation(&accepted.invocation.unwrap(), &mut output).unwrap();
        let submission = output.plan_submit.unwrap();
        assert_eq!(submission.expected_version, 2);
        assert!(!runtime.terminal);
        let mut replayed = initial;
        for patch in output.design_patch {
            replayed = replayed.patch_design(&workspace, patch).unwrap();
        }
        let files = crate::plan::PlanFileStore::new(workspace.join("data"), &workspace);
        files
            .write_working_document("session", "plan", &replayed)
            .unwrap();
        let (submitted, _, _) = files
            .submit_document_revision("session", "plan", 1, submission.expected_version)
            .unwrap();
        assert_eq!(submitted.version, 2);
        assert_eq!(
            submitted.design.unwrap().proposed_calls["api.rs"][0].behavior,
            Some(crate::plan::FunctionBehavior::Algorithm("return 1".into()))
        );
    }

    #[tokio::test]
    async fn focused_declaration_reads_and_patches_return_exact_context() {
        let mut context = planning_context();
        let source = "/// Describes an arena.\npub struct Arena;\n\n/// Returns the number of targets.\npub fn collectible_count() -> u32;\n";
        let workspace = context.workspace_root.as_ref().unwrap();
        std::fs::create_dir_all(workspace.join("src")).unwrap();
        std::fs::write(workspace.join("src/lib.rs"), source).unwrap();
        let mut design = crate::plan::DeclarationDesign::default();
        design.proposed.insert("src/lib.rs".into(), source.into());
        design.baseline.insert(
            "src/lib.rs".into(),
            crate::plan::DeclarationFile {
                text: source.into(),
                source_digest: crate::plan::digest(source.as_bytes()),
            },
        );
        design.document.objective = "Rename the count accessor.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Preserve the target count while renaming its accessor.".into();
        context.plan_document.as_mut().unwrap().design = Some(design);
        let mut runtime = ControlToolRuntime::new(context);
        let read = |arguments| ControlToolInvocation {
            task_permit: None,
            name: "plan_file_read".into(),
            arguments,
        };
        let result = runtime
            .invoke(read(
                json!({"plan_id":"plan", "path":"src/lib.rs", "start_line":4, "end_line":5}),
            ))
            .await
            .unwrap();
        assert!(result.message.contains("lines 4-5 of 5"));
        assert!(result.message.contains(
            "4: /// Returns the number of targets.\n5: pub fn collectible_count() -> u32;\n"
        ));
        assert!(!result.message.contains("pub struct Arena") && !result.message.contains("\\n"));
        for arguments in [
            json!({"plan_id":"plan", "start_line":1}),
            json!({"plan_id":"plan", "path":"src/lib.rs", "start_line":6}),
            json!({"plan_id":"plan", "path":"src/lib.rs", "start_line":5, "end_line":4}),
        ] {
            assert!(runtime.invoke(read(arguments)).await.is_err());
        }
        let result = runtime.invoke(ControlToolInvocation { task_permit: None, name:"plan_file_edit".into(), arguments:json!({"plan_id":"plan", "expected_version":1, "patch":"*** Begin Patch\n*** Update File: src/lib.rs\n@@\n-pub fn collectible_count() -> u32;\n+pub fn get_collectable_count() -> u32;\n+Algorithm\n+  return count(targets)\n*** End Patch"}) }).await.unwrap();
        assert!(
            result.message.contains("version is 2")
                && result.message.contains(
                    "-pub fn collectible_count() -> u32;\n+pub fn get_collectable_count() -> u32;"
                )
        );
        let baseline = runtime.invoke(read(json!({"plan_id":"plan", "path":"src/lib.rs", "baseline":true, "start_line":5, "end_line":99}))).await.unwrap();
        assert!(
            baseline.message.contains("(baseline) · lines 5-5 of 5")
                && baseline.message.contains("collectible_count()")
        );
        let submitted = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_submit".into(),
                arguments: json!({"plan_id":"plan", "expected_version":2}),
            })
            .await
            .unwrap();
        assert_eq!(
            submitted.message,
            "Declaration design submitted for review. Active canonical version is 2."
        );
        assert!(
            runtime
                .plan_document()
                .unwrap()
                .design
                .as_ref()
                .unwrap()
                .validation
                .is_some()
        );
    }

    #[tokio::test]
    async fn declaration_submission_keeps_unverified_evidence_out_of_tool_output() {
        let mut context = planning_context();
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Expose a generated interface.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design =
            "Keep generated declarations outside the hand-authored API.".into();
        design.proposed.insert("src/lib.rs".into(), "/// Declares generated types supplied during implementation.\npub mod generated;\nuse generated::*;\n/// Accepts the generated request.\npub fn inspect(value: Generated);\n".into());
        design.proposed_calls.insert(
            "src/lib.rs".into(),
            vec![crate::plan::FunctionBody {
                owner: "inspect".into(),
                evidence: None,
                call: None,
                behavior: Some(crate::plan::FunctionBehavior::Algorithm(
                    "inspect_value(value)".into(),
                )),
            }],
        );
        context.plan_document.as_mut().unwrap().design = Some(design);
        let mut runtime = ControlToolRuntime::new(context);
        let result = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_submit".into(),
                arguments: json!({"plan_id":"plan","expected_version":1}),
            })
            .await
            .unwrap();
        assert_eq!(
            result.message,
            "Declaration design submitted for review. Active canonical version is 1."
        );
        let validation = runtime
            .plan_document()
            .unwrap()
            .design
            .as_ref()
            .unwrap()
            .validation
            .as_ref()
            .unwrap();
        assert!(
            !validation.warnings().is_empty(),
            "unverified evidence was discarded"
        );
    }

    #[tokio::test]
    async fn large_patch_confirmation_preserves_the_complete_applied_design() {
        let mut context = planning_context();
        context.plan_document.as_mut().unwrap().design =
            Some(crate::plan::DeclarationDesign::default());
        let mut declaration =
            "/// Describes the public response.\npub struct Response {\n".to_owned();
        for index in 0..500 {
            declaration.push_str(&format!("  /// Retains the café response value for slot {index}.\n  pub response_{index}: u32,\n"));
        }
        declaration.push_str("}\n");
        let added = declaration
            .lines()
            .map(|line| format!("+{line}\n"))
            .collect::<String>();
        let mut runtime = ControlToolRuntime::new(context);
        let confirmed = runtime.invoke(ControlToolInvocation { task_permit: None, name:"plan_file_edit".into(), arguments:json!({"plan_id":"plan","expected_version":1,"patch":format!("*** Begin Patch\n*** Add File: src/response.rs\n{added}*** End Patch")}) }).await.unwrap();
        assert!(!confirmed.message.contains("Applied diff truncated"));
        assert!(confirmed.message.len() > 17 * 1024);
        assert!(confirmed.message.contains("response_499"));
        assert_eq!(
            runtime
                .plan_document()
                .unwrap()
                .design
                .as_ref()
                .unwrap()
                .proposed["src/response.rs"],
            declaration
        );
    }

    #[tokio::test]
    async fn declaration_submit_returns_reference_errors_and_allows_repair() {
        let mut context = planning_context();
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.objective = "Define the public API.".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Expose a typed API.".into();
        design.proposed.insert(
            "tsconfig.json".into(),
            "{\"compilerOptions\":{\"noLib\":true}}".into(),
        );
        design.proposed.insert(
            "api.ts".into(),
            "import { Missing } from './model';\nexport interface Api { item: Missing; }\n".into(),
        );
        design
            .proposed
            .insert("model.ts".into(), "export interface Item {}\n".into());
        context.plan_document.as_mut().unwrap().design = Some(design);
        let mut runtime = ControlToolRuntime::new(context);
        let submit = |version| ControlToolInvocation {
            task_permit: None,
            name: "plan_submit".into(),
            arguments: serde_json::json!({"plan_id":"plan","expected_version":version}),
        };
        let error = runtime.invoke(submit(1)).await.unwrap_err();
        let failure: serde_json::Value = serde_json::from_str(
            &crate::tools::control_tool_failure_json(&submit(1), &error, runtime.plan_document()),
        )
        .unwrap();
        assert_eq!(failure["code"], "declaration_validation_failed");
        assert!(
            failure["violation"][0]["path"]
                .as_str()
                .unwrap()
                .starts_with("api.ts:")
        );
        runtime.invoke(ControlToolInvocation { task_permit: None, name:"plan_file_edit".into(),arguments:serde_json::json!({"plan_id":"plan","expected_version":1,"patch":"*** Begin Patch\n*** Update File: api.ts\n@@\n-import { Missing } from './model';\n-export interface Api { item: Missing; }\n+import { Item } from './model';\n+export interface Api { item: Item; }\n*** End Patch"}) }).await.unwrap();
        runtime.invoke(submit(2)).await.unwrap();
        assert!(
            runtime
                .plan_document()
                .unwrap()
                .design
                .as_ref()
                .unwrap()
                .validation
                .is_some()
        );
    }

    #[tokio::test]
    async fn external_api_validation_does_not_block_submission() {
        let mut context = planning_context();
        context.plan_document.as_mut().unwrap().dependencies.push(
            crate::plan::PlanDependencyChange {
                action: crate::plan::ChangeAction::Add,
                name: "datafusion".into(),
                version: "not a Cargo requirement".into(),
                resolved_version: None,
                manifest: "Cargo.toml".into(),
                license: Some("Apache-2.0".into()),
                justification: "Runs relational queries. The standard library has no query engine."
                    .into(),
            },
        );
        context.plan_document.as_mut().unwrap().stages[0].tasks[0]
            .files
            .push(
                serde_json::from_value(json!({
                    "action": "modify",
                        "path": "Cargo.toml",
                        "subtasks": [{
                            "operation": "configure",
                        "description": "the invalid dependency requirement.",
                        "entities": []
                    }]
                }))
                .unwrap(),
            );
        let mut runtime = ControlToolRuntime::new(context);

        let result = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_submit".into(),
                arguments: json!({ "plan_id": "plan", "expected_version": 1 }),
            })
            .await
            .unwrap();
        assert!(
            result
                .message
                .contains("passed canonical submission validation")
        );
        assert!(!runtime.terminal);
    }

    #[tokio::test]
    async fn external_dependency_submission_needs_no_analysis_service() {
        let mut context = planning_context();
        context.plan_document.as_mut().unwrap().dependencies.push(
            crate::plan::PlanDependencyChange {
                action: crate::plan::ChangeAction::Add,
                name: "datafusion".into(),
                version: "54".into(),
                resolved_version: None,
                manifest: "Cargo.toml".into(),
                license: Some("Apache-2.0".into()),
                justification: "Runs relational queries. The standard library has no query engine."
                    .into(),
            },
        );
        context.plan_document.as_mut().unwrap().stages[0].tasks[0]
            .files
            .push(
                serde_json::from_value(json!({
                    "action": "modify",
                    "path": "Cargo.toml",
                    "subtasks": [{
                        "operation": "configure",
                        "description": "the DataFusion dependency.",
                        "entities": []
                    }]
                }))
                .unwrap(),
            );
        let mut runtime = ControlToolRuntime::new(context);

        let result = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "plan_submit".into(),
                arguments: json!({ "plan_id": "plan", "expected_version": 1 }),
            })
            .await
            .unwrap();

        assert!(
            result
                .message
                .contains("passed canonical submission validation")
        );
        assert!(!result.message.contains("warning"));
        assert!(
            runtime.plan_document().unwrap().dependencies[0]
                .resolved_version
                .is_none()
        );
    }

    #[tokio::test]
    async fn consumes_a_repeated_question_by_content_digest() {
        let question = crate::plan::PlanQuestion {
            id: "first_id".into(),
            header: "Scope".into(),
            question: "Which scope?".into(),
            options: vec![
                crate::plan::PlanQuestionOption {
                    recommended: false,
                    label: "Narrow".into(),
                    description: "Keep it narrow.".into(),
                },
                crate::plan::PlanQuestionOption {
                    recommended: false,
                    label: "Broad".into(),
                    description: "Expand it.".into(),
                },
            ],
            allow_freeform: false,
        };
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context
            .resolved_question_digest_set
            .insert(question.content_digest());
        let mut runtime = ControlToolRuntime::new(context);
        let result = runtime
            .invoke(ControlToolInvocation {
                task_permit: None,
                name: "question_ask".into(),
                arguments: json!({
                    "questions": [{
                    "header": "Scope", "question": "Which scope?",
                    "allow_freeform": false,
                    "options": [
                        { "label": "Narrow", "description": "Keep it narrow." },
                        { "label": "Broad", "description": "Expand it." }
                    ]
                    }]
                }),
            })
            .await
            .unwrap();
        assert!(result.invocation.is_none());
        assert!(result.message.contains("already consumed"));
    }
}
