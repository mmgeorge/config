use super::{ControlToolInvocation, apply_invocation};
use crate::backend::{BackendOutput, PromptMode};
use crate::plan::{PlanDocument, PlanState, apply_plan_edit, render_plan};
use anyhow::{Context, Result};
use std::collections::HashSet;

/// Carries the broker-owned control state visible to one provider turn.
#[derive(Clone, Debug)]
pub struct ControlTurnContext {
    pub mode: PromptMode,
    pub planning_feedback: bool,
    pub plan_state: Option<PlanState>,
    pub plan_document: Option<PlanDocument>,
    pub resolved_question_digest_set: HashSet<String>,
    pub has_active_elicitation: bool,
    pub has_active_execution: bool,
    pub has_active_goal: bool,
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
    pub fn invoke(&mut self, invocation: ControlToolInvocation) -> Result<ControlToolResult> {
        let mut output = BackendOutput::default();
        apply_invocation(&invocation, &mut output)?;
        self.invoke_decoded(invocation, output)
    }

    /// Execute one control invocation from arguments decoded by the provider boundary.
    pub(crate) fn invoke_decoded(
        &mut self,
        invocation: ControlToolInvocation,
        mut output: BackendOutput,
    ) -> Result<ControlToolResult> {
        anyhow::ensure!(
            !self.terminal,
            "this provider turn already reached a terminal control action"
        );
        match invocation.name.as_str() {
            "harness_plan_edit" => {
                self.require_editable_plan()?;
                let request = output
                    .plan_edit
                    .pop()
                    .context("plan edit did not produce an edit request")?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan edit has no active canonical document")?;
                self.plan_document = Some(apply_plan_edit(document, request)?.document);
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: self.plan_version_message("Plan edit accepted"),
                })
            }
            "harness_plan_read" => {
                self.require_editable_plan()?;
                let document = self
                    .plan_document
                    .as_ref()
                    .context("plan read has no active canonical document")?;
                anyhow::ensure!(
                    output.plan_read.as_deref() == Some(&document.plan_id),
                    "requested plan id does not match the active plan"
                );
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: document.model_json()?,
                })
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
                render_plan(document)?;
                self.terminal = true;
                Ok(ControlToolResult {
                    invocation: Some(invocation),
                    message: self.plan_version_message("Plan passed submission validation"),
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
                    !self.context.has_active_elicitation,
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
                    self.context.mode == PromptMode::Chat && !self.context.planning_feedback,
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
        anyhow::ensure!(
            self.context.mode == PromptMode::Plan,
            "plan controls require Harness Plan mode"
        );
        anyhow::ensure!(
            matches!(
                self.context.plan_state,
                Some(PlanState::Generating | PlanState::Revising)
            ),
            "plan controls require a generating or revising plan"
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
        ControlTurnContext {
            mode: PromptMode::Plan,
            planning_feedback: false,
            plan_state: Some(PlanState::Generating),
            plan_document: Some(crate::plan::test_fixture("plan", "Initial")),
            resolved_question_digest_set: HashSet::new(),
            has_active_elicitation: false,
            has_active_execution: false,
            has_active_goal: false,
        }
    }

    #[test]
    fn stages_edits_and_closes_the_turn_after_submission() {
        let mut runtime = ControlToolRuntime::new(planning_context());
        runtime
            .invoke(ControlToolInvocation {
                name: "harness_plan_edit".into(),
                arguments: json!({
                    "plan_id": "plan", "expected_version": 1,
                    "plan": { "modify": { "overview": "Changed" } }
                }),
            })
            .unwrap();
        runtime
            .invoke(ControlToolInvocation {
                name: "harness_plan_submit".into(),
                arguments: json!({ "plan_id": "plan", "expected_version": 2 }),
            })
            .unwrap();
        assert!(
            runtime
                .invoke(ControlToolInvocation {
                    name: "harness_plan_read".into(),
                    arguments: json!({ "plan_id": "plan" }),
                })
                .is_err()
        );
    }

    #[test]
    fn keeps_the_draft_editable_after_submission_validation_fails() {
        let mut runtime = ControlToolRuntime::new(planning_context());
        runtime
            .invoke(ControlToolInvocation {
                name: "harness_plan_edit".into(),
                arguments: json!({
                    "plan_id": "plan",
                    "expected_version": 1,
                    "entity_changes": {
                        "add": [{
                            "action": "add",
                            "kind": "struct",
                            "name": "InspectionService",
                            "description": "Own inspection.",
                            "path": "src/inspection.rs",
                            "members": [],
                            "extends": null,
                            "conforms_to": []
                        }]
                    }
                }),
            })
            .unwrap();
        let submission_error = match runtime.invoke(ControlToolInvocation {
            name: "harness_plan_submit".into(),
            arguments: json!({ "plan_id": "plan", "expected_version": 2 }),
        }) {
            Ok(_) => panic!("incomplete plan submission should fail"),
            Err(error) => error.to_string(),
        };
        assert!(submission_error.contains("must belong to exactly one subtask"));

        runtime
            .invoke(ControlToolInvocation {
                name: "harness_plan_edit".into(),
                arguments: json!({
                    "plan_id": "plan",
                    "expected_version": 2,
                    "entity_changes": {
                        "modify": [{
                            "entity": "InspectionService",
                            "members": {
                                "add": [{
                                "action": "add",
                                "kind": "method",
                                "name": "inspect",
                                "description": "Inspect input.",
                                "visibility": "public",
                                "type": null,
                                "parameters": [],
                                "return_type": "InspectionReport"
                            }]
                            }
                        }]
                    },
                    "tasks": {
                        "modify": [{
                            "task": "Create plan state",
                            "files": {
                                "add": [{
                                    "path": "src/inspection.rs",
                                    "action": "add",
                                    "subtasks": [{
                                        "operation": "create",
                                        "description": "the inspection owner.",
                                        "entities": ["InspectionService"]
                                    }]
                                }]
                            }
                        }]
                    }
                }),
            })
            .unwrap();
        runtime
            .invoke(ControlToolInvocation {
                name: "harness_plan_submit".into(),
                arguments: json!({ "plan_id": "plan", "expected_version": 3 }),
            })
            .unwrap();
    }

    #[test]
    fn consumes_a_repeated_question_by_content_digest() {
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
            .unwrap();
        assert!(result.invocation.is_none());
        assert!(result.message.contains("already consumed"));
    }
}
