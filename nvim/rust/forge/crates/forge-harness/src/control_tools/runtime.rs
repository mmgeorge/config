use super::{ControlToolInvocation, apply_invocation};
use crate::backend::{BackendOutput, PromptMode};
use crate::plan::{
    PlanDocument, PlanState, apply_plan_edit, render_plan, validate_workspace_references,
};
use crate::rustdoc::{RustdocResolver, validate_plan_rust_api};
use anyhow::{Context, Result};
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;

/// Carries the broker-owned control state visible to one provider turn.
#[derive(Clone)]
pub struct ControlTurnContext {
    pub mode: PromptMode,
    pub planning_feedback: bool,
    pub plan_state: Option<PlanState>,
    pub plan_document: Option<PlanDocument>,
    pub resolved_question_digest_set: HashSet<String>,
    pub has_active_elicitation: bool,
    pub has_active_execution: bool,
    pub has_active_goal: bool,
    pub workspace_root: Option<PathBuf>,
    pub rustdoc: Option<Arc<RustdocResolver>>,
    pub repository: Option<Arc<super::repository::RepositoryToolScope>>,
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
            .field("has_active_goal", &self.has_active_goal)
            .field("workspace_root", &self.workspace_root)
            .field("rustdoc_available", &self.rustdoc.is_some())
            .field("repository_available", &self.repository.is_some())
            .finish()
    }
}

impl ControlTurnContext {
    /// Build an inert context for backend calls without broker state.
    pub fn inactive(mode: PromptMode) -> Self {
        Self {
            mode,
            planning_feedback: false,
            plan_state: None,
            plan_document: None,
            resolved_question_digest_set: HashSet::new(),
            has_active_elicitation: false,
            has_active_execution: false,
            has_active_goal: false,
            workspace_root: None,
            rustdoc: None,
            repository: None,
        }
    }
}

/// Owns provider-visible control validation for one bounded backend turn.
pub struct ControlToolRuntime {
    context: ControlTurnContext,
    plan_document: Option<PlanDocument>,
    terminal: bool,
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
        }
    }

    /// Validate and stage one control invocation without persisting broker state.
    pub async fn invoke(&mut self, invocation: ControlToolInvocation) -> Result<ControlToolResult> {
        let mut output = BackendOutput::default();
        apply_invocation(&invocation, &mut output)?;
        self.invoke_decoded(invocation, output).await
    }

    /// Execute one control invocation from arguments decoded by the provider boundary.
    pub(crate) async fn invoke_decoded(
        &mut self,
        invocation: ControlToolInvocation,
        mut output: BackendOutput,
    ) -> Result<ControlToolResult> {
        anyhow::ensure!(
            !self.terminal,
            "this provider turn already reached a terminal control action"
        );
        match invocation.name.as_str() {
            "harness_design_apply_patch" => {
                self.require_readable_plan()?;
                anyhow::ensure!(!self.context.has_active_elicitation, "resolve pending Harness questions before editing the design");
                let request = output.design_patch.pop().context("design patch has no request")?;
                let document = self.plan_document.as_ref().context("design patch has no active design")?;
                self.plan_document = Some(document.patch_design(request)?);
                if self.context.plan_state == Some(PlanState::AwaitingReview) { self.context.plan_state = Some(PlanState::Revising); }
                Ok(ControlToolResult { invocation:Some(invocation), message:self.plan_version_message("Declaration patch accepted") })
            }
            "harness_repository_status"
            | "harness_repository_changed_paths"
            | "harness_repository_diff"
            | "harness_repository_file_diff" => {
                let scope = self.context.repository.as_ref().context(
                    "repository inspection requires an authenticated active interaction",
                )?;
                let message = scope.invoke(&invocation.name, invocation.arguments).await?;
                Ok(ControlToolResult {
                    invocation: None,
                    message,
                })
            }
            "harness_plan_edit" => {
                self.require_readable_plan()?;
                anyhow::ensure!(!self.context.has_active_elicitation,
                    "resolve the pending Harness questions before editing the plan");
                let request = output
                    .plan_edit
                    .pop()
                    .context("plan edit did not produce an edit request")?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan edit has no active canonical document")?;
                self.plan_document = Some(apply_plan_edit(document, request)?.document);
                if self.context.plan_state == Some(PlanState::AwaitingReview) {
                    self.context.plan_state = Some(PlanState::Revising);
                }
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: format!(
                        "{} Submission validation has not run.",
                        self.plan_version_message("Plan edit accepted")
                    ),
                })
            }
            "harness_plan_read" => {
                self.require_readable_plan()?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan read has no active canonical document")?;
                anyhow::ensure!(
                    output.plan_read.as_deref() == Some(&document.plan_id),
                    "requested plan id does not match the active plan"
                );
                let message = if let Some(design) = &document.design {
                        serde_json::to_string(&serde_json::json!({"plan_id":document.plan_id,"version":document.version,"file":design.read(invocation.arguments.get("path").and_then(serde_json::Value::as_str),invocation.arguments.get("baseline").and_then(serde_json::Value::as_bool).unwrap_or(false))?}))?
                    } else { document.model_json()? };
                Ok(ControlToolResult { invocation: Some(invocation), message })
            }
            "harness_plan_submit" => {
                self.require_editable_plan()?;
                let submission = output
                    .plan_submit
                    .as_ref()
                    .context("plan submit did not produce a submission")?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan submit has no active canonical document")?;
                anyhow::ensure!(
                    submission.plan_id == document.plan_id,
                    "submitted plan id does not match the active plan"
                );
                anyhow::ensure!(
                    submission.expected_version == document.version,
                    "submitted version does not match active version"
                );
                document.validate_for_submission()?;
                if let Some(design) = &document.design {
                    let mut design = design.formatted()?;
                    let workspace = self.context.workspace_root.as_deref().context("design submission has no workspace root")?;
                    design.check_workspace(workspace)?;
                    let mut resolver = crate::declaration::DeclarationResolver::prepare(workspace, &design, false).await?;
                    let report = resolver.validate(&design);
                    report.ensure_valid()?;
                    let warning = report.warnings();
                    design.validation = Some(report);
                    let mut submitted = document.clone();
                    submitted.design = Some(design);
                    self.plan_document = Some(submitted);
                    self.terminal = true;
                    return Ok(ControlToolResult { invocation:Some(invocation), message:format!("{}\n{}", self.plan_version_message("Declaration design submitted for review"), serde_json::to_string(&warning)?) });
                }
                let workspace_root = self
                    .context
                    .workspace_root
                    .as_deref()
                    .context("plan submission has no workspace root")?;
                validate_workspace_references(document, workspace_root)?;
                render_plan(document)?;
                let resolver = self
                    .context
                    .rustdoc
                    .as_ref()
                    .context("plan submission has no Rust API validation service")?;
                let mut validated_document = document.clone();
                let report = validate_plan_rust_api(resolver, &mut validated_document).await?;
                self.plan_document = Some(validated_document);
                self.terminal = true;
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: if report.warning.is_empty() {
                        self.plan_version_message("Plan passed canonical submission validation")
                    } else {
                        format!(
                            "{} {} Rust API validation warning(s): {}",
                            self.plan_version_message(
                                "Plan passed canonical submission validation"
                            ),
                            report.warning.len(),
                            report
                                .warning
                                .iter()
                                .map(|warning| format!("{}: {}", warning.path, warning.message))
                                .collect::<Vec<_>>()
                                .join("; ")
                        )
                    },
                })
            }
            "harness_question_ask" => {
                let question = output
                    .plan_question
                    .as_ref()
                    .context("question ask did not produce questions")?;
                if question.questions.iter().all(|item| {
                    self.context
                        .resolved_question_digest_set
                        .contains(&item.content_digest())
                }) {
                    self.terminal = true;
                    return Ok(ControlToolResult { invocation: None, message: "Those questions were already consumed. Continue planning without reopening feedback.".into() });
                }
                anyhow::ensure!(
                    !self.context.has_active_elicitation
                        || (matches!(self.context.mode, PromptMode::Chat | PromptMode::PlanDiscussion)
                            && !self.context.planning_feedback),
                    "a Harness question set is already pending"
                );
                self.terminal = true;
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: "Question set accepted".into(),
                })
            }
            "harness_question_answer" | "harness_question_withdraw" => {
                anyhow::ensure!(
                    matches!(self.context.mode, PromptMode::Chat | PromptMode::PlanDiscussion)
                        && !self.context.planning_feedback,
                    "question resolution is unavailable during planning feedback"
                );
                anyhow::ensure!(
                    self.context.has_active_elicitation,
                    "no Harness question set is pending"
                );
                self.terminal = true;
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: "Question resolution accepted".into(),
                })
            }
            "harness_plan_deviation" | "harness_plan_task_report" => {
                anyhow::ensure!(
                    self.context.mode == PromptMode::ExecutePlan
                        && self.context.has_active_execution,
                    "plan execution controls require an active accepted-plan task"
                );
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: "Execution control accepted".into(),
                })
            }
            "harness_goal_complete" | "harness_goal_blocked" | "harness_goal_status" => {
                anyhow::ensure!(
                    self.context.has_active_goal,
                    "goal control requires an active nonterminal goal"
                );
                if invocation.name != "harness_goal_status" {
                    self.terminal = true;
                }
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: "Goal control accepted".into(),
                })
            }
            name => anyhow::bail!("unknown Harness control tool: {name}"),
        }
    }

    /// Return the staged canonical document after accepted plan edits.
    pub fn plan_document(&self) -> Option<&PlanDocument> {
        self.plan_document.as_ref()
    }

    fn require_editable_plan(&self) -> Result<()> {
        self.require_readable_plan()?;
        anyhow::ensure!(
            matches!(self.context.plan_state, Some(PlanState::Generating | PlanState::Revising)),
            "plan submission requires a generating or revising plan"
        );
        Ok(())
    }

    fn require_readable_plan(&self) -> Result<()> {
        anyhow::ensure!(
            matches!(self.context.mode, PromptMode::Plan | PromptMode::PlanDiscussion),
            "plan controls require Harness Plan mode"
        );
        anyhow::ensure!(
            matches!(
                self.context.plan_state,
                Some(PlanState::Generating | PlanState::Revising | PlanState::AwaitingReview)
            ),
            "plan controls require an active draft or submitted plan"
        );
        Ok(())
    }

    fn plan_version_message(&self, prefix: &str) -> String {
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
    use serde_json::json;

    fn planning_context() -> ControlTurnContext {
        let cache_dir = tempfile::tempdir().unwrap().keep();
        ControlTurnContext {
            mode: PromptMode::Plan,
            planning_feedback: false,
            plan_state: Some(PlanState::Generating),
            plan_document: Some(crate::plan::test_fixture("plan", "Initial")),
            resolved_question_digest_set: HashSet::new(),
            has_active_elicitation: false,
            has_active_execution: false,
            has_active_goal: false,
            workspace_root: Some(tempfile::tempdir().unwrap().keep()),
            repository: None,
            rustdoc: Some(Arc::new(
                RustdocResolver::new(crate::rustdoc::RustdocResolverConfig {
                    crates_io_base: "http://127.0.0.1:9".into(),
                    cache_dir,
                    cargo_source: crate::rustdoc::CargoSourceResolverConfig {
                        cargo_executable: PathBuf::from("missing-cargo"),
                        cargo_home: tempfile::tempdir().unwrap().keep(),
                    },
                })
                .unwrap(),
            )),
        }
    }

    #[tokio::test]
    async fn declaration_tools_stage_versions_and_recover_atomic_failures() {
        let mut context = planning_context();
        context.mode = PromptMode::PlanDiscussion;
        context.plan_state = Some(PlanState::AwaitingReview);
        context.plan_document.as_mut().unwrap().design = Some(crate::plan::DeclarationDesign::default());
        context.plan_document.as_mut().unwrap().design.as_mut().unwrap().document.task = "Own private state.".into();
        let mut runtime = ControlToolRuntime::new(context);
        let patch = "*** Begin Patch\n*** Add File: src/lib.rs\n+pub struct Owner;\n*** Update File: plan.json\n@@\n-  \"description\": \"\"\n+  \"description\": \"Introduce an owner for private state.\"\n*** End Patch";
        assert!(runtime.invoke(ControlToolInvocation { name:"harness_plan_submit".into(),arguments:json!({"plan_id":"plan","expected_version":1}) }).await.is_err());
        let change = |version,patch:&str| ControlToolInvocation { name:"harness_design_apply_patch".into(),arguments:json!({"plan_id":"plan","expected_version":version,"patch":patch}) };
        assert!(runtime.invoke(change(99,patch)).await.is_err());
        assert!(runtime.invoke(change(1,"*** Begin Patch\n*** Add File: src/lib.rs\n+fn bad() {}\n*** End Patch")).await.is_err());
        assert_eq!(runtime.plan_document().unwrap().version,1);
        let inventory = runtime.invoke(ControlToolInvocation { name:"harness_plan_read".into(),arguments:json!({"plan_id":"plan"}) }).await.unwrap();
        assert!(inventory.message.contains("plan.json"));
        runtime.invoke(change(1,patch)).await.unwrap();
        let metadata = runtime.invoke(ControlToolInvocation { name:"harness_plan_read".into(),arguments:json!({"plan_id":"plan","path":"plan.json"}) }).await.unwrap();
        assert!(metadata.message.contains("Introduce an owner"));
        runtime.invoke(change(2,"*** Begin Patch\n*** Update File: plan.json\n@@\n   \"description\": \"Introduce an owner for private state.\"\n*** End Patch")).await.unwrap();
        assert_eq!(runtime.plan_document().unwrap().version, 2);
        runtime.invoke(change(2,"*** Begin Patch\n*** Update File: src/lib.rs\n@@\n-pub struct Owner;\n+pub struct Owner { private: u64 }\n*** End Patch")).await.unwrap();
        let read = runtime.invoke(ControlToolInvocation { name:"harness_plan_read".into(),arguments:json!({"plan_id":"plan","path":"src/lib.rs"}) }).await.unwrap();
        assert!(read.message.contains("private: u64"));
        runtime.invoke(ControlToolInvocation { name:"harness_plan_submit".into(),arguments:json!({"plan_id":"plan","expected_version":3}) }).await.unwrap();
        assert!(runtime.invoke(change(3,patch)).await.is_err());
    }




    #[tokio::test]
    async fn declaration_submit_returns_reference_errors_and_allows_repair() {
        let mut context = planning_context();
        let mut design = crate::plan::DeclarationDesign::default();
        design.document.task = "Define the public API.".into();
        design.document.description = "Expose a typed API.".into();
        design.proposed.insert("tsconfig.json".into(), "{\"compilerOptions\":{\"noLib\":true}}".into());
        design.proposed.insert("api.ts".into(), "export interface Api { item: Missing; }\n".into());
        context.plan_document.as_mut().unwrap().design = Some(design);
        let mut runtime = ControlToolRuntime::new(context);
        let submit = |version| ControlToolInvocation { name:"harness_plan_submit".into(),arguments:serde_json::json!({"plan_id":"plan","expected_version":version}) };
        let error = runtime.invoke(submit(1)).await.unwrap_err();
        let failure:serde_json::Value = serde_json::from_str(&crate::control_tools::control_tool_failure_json(&submit(1), &error, runtime.plan_document())).unwrap();
        assert_eq!(failure["code"], "declaration_validation_failed");
        assert!(failure["violation"][0]["path"].as_str().unwrap().starts_with("api.ts:1:"));
        runtime.invoke(ControlToolInvocation { name:"harness_design_apply_patch".into(),arguments:serde_json::json!({"plan_id":"plan","expected_version":1,"patch":"*** Begin Patch\n*** Update File: api.ts\n@@\n-export interface Api { item: Missing; }\n+export interface Api { item: string; }\n*** End Patch"}) }).await.unwrap();
        runtime.invoke(submit(2)).await.unwrap();
        assert!(runtime.plan_document().unwrap().design.as_ref().unwrap().validation.is_some());
    }

    #[tokio::test]
    async fn returns_rust_api_violations_to_the_submit_tool_and_keeps_editing_open() {
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

        let error = runtime
            .invoke(ControlToolInvocation {
                name: "harness_plan_submit".into(),
                arguments: json!({ "plan_id": "plan", "expected_version": 1 }),
            })
            .await
            .unwrap_err()
            .to_string();

        assert!(error.contains("dependencies.0.version"), "{error}");
        assert!(
            error.contains("invalid Cargo version requirement"),
            "{error}"
        );
        assert!(
            runtime
                .invoke(ControlToolInvocation {
                    name: "harness_plan_read".into(),
                    arguments: json!({ "plan_id": "plan" }),
                })
                .await
                .is_ok()
        );
    }

    #[tokio::test]
    async fn returns_network_validation_warnings_without_rejecting_submission() {
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
                name: "harness_plan_submit".into(),
                arguments: json!({ "plan_id": "plan", "expected_version": 1 }),
            })
            .await
            .unwrap();

        assert!(
            result
                .message
                .contains("passed canonical submission validation")
        );
        assert!(result.message.contains("Rust API validation warning"));
        assert!(result.message.contains("partially skipped"));
        assert!(result.message.contains("datafusion"));
    }

    #[tokio::test]
    async fn consumes_a_repeated_question_by_content_digest() {
        let question = crate::plan::PlanQuestion {
            id: "first_id".into(),
            header: "Scope".into(),
            question: "Which scope?".into(),
            options: vec![
                crate::plan::PlanQuestionOption {
                    label: "Narrow".into(),
                    description: "Keep it narrow.".into(),
                },
                crate::plan::PlanQuestionOption {
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
                name: "harness_question_ask".into(),
                arguments: json!({
                    "questions": [{
                    "id": "provider_changed_id", "header": "Scope", "question": "Which scope?",
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
