use super::{BackendRequest, HARNESS_SYSTEM_MESSAGE, PromptMode};

impl BackendRequest {
    /// Bind shared provider instructions to the permission and purpose of this turn.
    pub(crate) fn system_message(&self) -> String {
        format!(
            "{HARNESS_SYSTEM_MESSAGE}\n\n{}",
            self.interaction_contract()
        )
    }

    /// Supply current interaction context even when a provider reuses a live session.
    pub(crate) fn interaction_contract(&self) -> String {
        let purpose = match self.mode {
            PromptMode::Chat => {
                "Answer the current user request. This interaction does not itself start, clear, or complete a Harness task."
            }
            PromptMode::Plan => {
                "Create or revise the supplied canonical Plan task. Edit virtual proposals only, then submit for review."
            }
            PromptMode::PlanDiscussion => {
                "Discuss the active plan. Edit virtual proposals only when the user requests a revision. Do not implement it."
            }
            PromptMode::PlanQuestion => {
                "Answer the anchored plan review question only. Do not edit, submit, implement, or ask follow-up questions."
            }
            PromptMode::ExecutePlan => {
                "Continue the Execute task at its supplied phase and accepted revision. Use harness_plan_phase_done, not goal completion, to advance execution."
            }
            PromptMode::GoalContinuation => {
                "Continue the active Goal task toward its objective. Report completion only when all required work is finished, or a concrete blocker when no authorized progress remains."
            }
            PromptMode::RequestChanges => {
                "Address the supplied review against the current workspace. Preserve unrelated work and verify the affected behavior."
            }
        };
        let question = if self
            .control_context
            .as_ref()
            .is_some_and(|context| context.planning_feedback)
        {
            "Harness already recorded and consumed the supplied planning answers. Do not call harness_question_answer or harness_question_withdraw for them."
        } else {
            "Use harness_question_answer only for an explicit answer to a currently pending question, and harness_question_withdraw only when that decision is no longer needed."
        };
        let shell = self.control_context.as_ref().map_or(Default::default(), |context|context.check_shell);
        let interpreter = match shell {
            crate::session::CheckShell::Nushell => "Nushell",
            crate::session::CheckShell::System if cfg!(windows) => "PowerShell 7 (pwsh)",
            crate::session::CheckShell::System => "the system login shell",
        };
        format!(
            "Current Harness interaction\nEffective permission: {}.\nAccess configuration: {}.\nPurpose: {purpose}\nAccepted checks run in {interpreter}. Write exact check commands for that interpreter.\n{question}",
            self.execution_mode.label(), serde_json::to_string(&self.access).expect("access policy serializes")
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::backend::{BackendInput, ServiceTier};
    use crate::control_tools::ControlTurnContext;
    use crate::session::PermissionMode;

    #[test]
    fn current_context_replaces_previous_permission_and_does_not_infer_answers_from_text() {
        let mut request = BackendRequest {
            harness_session_id: "session".into(),
            workspace: ".".into(),
            input: BackendInput::from_text("Explain the phrase Planning feedback:"),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: ServiceTier::Standard,
            access: Default::default(),
            execution_mode: PermissionMode::Read,
            backend_session_id: Some("existing".into()),
            control_context: Some(ControlTurnContext::inactive(PromptMode::Chat)),
        };
        for permission in [
            PermissionMode::Read,
            PermissionMode::Write,
            PermissionMode::Yolo,
        ] {
            request.execution_mode = permission;
            let contract = request.interaction_contract();
            assert!(contract.contains(&format!("Effective permission: {}.", permission.label())));
            assert!(contract.contains("currently pending question"));
            assert!(!contract.contains("already recorded"));
        }
        request.mode = PromptMode::Plan;
        request.control_context.as_mut().unwrap().planning_feedback = true;
        assert!(
            request
                .interaction_contract()
                .contains("already recorded and consumed")
        );
        assert!(
            !request
                .interaction_contract()
                .contains("currently pending question")
        );
        for (mode, expected) in [
            (PromptMode::Plan, "virtual proposals only"),
            (
                PromptMode::PlanDiscussion,
                "when the user requests a revision",
            ),
            (
                PromptMode::PlanQuestion,
                "anchored plan review question only",
            ),
            (PromptMode::ExecutePlan, "harness_plan_phase_done"),
            (PromptMode::GoalContinuation, "active Goal task"),
            (PromptMode::RequestChanges, "supplied review"),
        ] {
            request.mode = mode;
            assert!(request.system_message().contains(expected));
        }
    }
}
