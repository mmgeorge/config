use super::{BackendRequest, HARNESS_SYSTEM_MESSAGE, PromptMode};

impl BackendRequest {
    /// Bind shared provider instructions to the permission and purpose of this turn.
    pub(crate) fn system_message(&self) -> String {
        if self.mode == PromptMode::Background { return super::BACKGROUND_SYSTEM_MESSAGE.to_owned(); }
        format!(
            "{HARNESS_SYSTEM_MESSAGE}\n\n{}",
            self.interaction_contract()
        )
    }

    /// Supply current interaction context even when a provider reuses a live session.
    pub(crate) fn interaction_contract(&self) -> String {
        let purpose = match self.mode {
            PromptMode::Background => return super::BACKGROUND_SYSTEM_MESSAGE.to_owned(),
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
                "Continue the Execute task at its supplied phase and accepted revision. Use plan_phase_done, not goal completion, to advance execution."
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
            "Harness already recorded and consumed the supplied planning answers. Do not call question_answer or question_withdraw for them."
        } else {
            "Use question_answer only for an explicit answer to a currently pending question, and question_withdraw only when that decision is no longer needed."
        };
        let shell = self.control_context.as_ref().map_or(Default::default(), |context|context.check_shell);
        let interpreter = match shell {
            crate::session::CheckShell::Nushell => "Nushell",
            crate::session::CheckShell::System if cfg!(windows) => "PowerShell 7 (pwsh)",
            crate::session::CheckShell::System => "the system login shell",
        };
        let lsp = if self
            .control_context
            .as_ref()
            .and_then(|context| context.lsp.as_ref())
            .is_some_and(|scope| scope.available())
        {
            "\nLSP exploration is enabled. Prefer lsp_overview for a public directory map, lsp_symbols to find names and exact positions, and lsp_context for declarations, documentation, and source bodies. Target lsp_definition (definition, type, or implementation), lsp_context, lsp_references, and lsp_calls with a short or qualified symbol, optionally a file, or an exact path/line/column. A path/line/symbol selects an occurrence. Ambiguity returns candidates; select one rather than guessing. Use lsp_paths with from/to selectors to find bounded static call chains between known endpoints. Positions are one-based Unicode code points. Matching excerpt lines use >, while declaration headers carry reusable columns. Keep token_budget small and follow next_offset or next_line only when needed. Queries share editor language servers and wait for initialization. Use lsp_diagnostics after edits, but stale or unknown diagnostics do not prove a successful typecheck. lsp_status is for readiness or capability issues, not a prerequisite to each query. Use text search for literals or when semantic queries are unavailable."
        } else {
            ""
        };
        let check_editing = if self.control_context.as_ref().and_then(|context| context.checks.as_ref())
            .is_some_and(|scope| scope.editing_enabled()) {
            "enabled. Use checks_configure for authorized edits. Guided setup still requires answers before saving."
        } else {
            "disabled. The user can enable it with /checks setup on or start guided /checks setup."
        };
        format!(
            "Current Harness interaction\nEffective permission: {}.\nAccess configuration: {}.\nPurpose: {purpose}\nCheck editing permission: {check_editing}\nSaved project checks run in {interpreter}. Setup commands must target that interpreter. Checks run automatically at the configured queue boundary, after the queue by default. Manual runs are generally unnecessary. You may optionally call checks_run for earlier feedback. Read recorded results through checks_status. During automatic repair, fix the failures and finish the turn so Harness checks again. Never run validation through terminal commands or subprocesses. During plan execution, finish the phase and let Harness Checking run checks. Delegated agents without check tools must leave validation to their parent's completion gate.\n{question}{lsp}",
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
    fn background_profile_excludes_workflow_instructions_and_tools() {
        let context = ControlTurnContext::inactive(PromptMode::Background);
        assert!(crate::control_tools::ControlToolRegistry.definition_list_for_context(Some(&context),true).is_empty());
        assert!(crate::control_tools::ControlToolRegistry.definition_list_for_context(Some(&context),false).is_empty());
    }

    #[test]
    fn current_context_replaces_previous_permission_and_does_not_infer_answers_from_text() {
        let mut request = BackendRequest {
            execution_owner: None,
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
            (PromptMode::ExecutePlan, "plan_phase_done"),
            (PromptMode::GoalContinuation, "active Goal task"),
            (PromptMode::RequestChanges, "supplied review"),
        ] {
            request.mode = mode;
            assert!(request.system_message().contains(expected));
        }
        request.mode = PromptMode::Background;
        assert_eq!(request.system_message(), super::super::BACKGROUND_SYSTEM_MESSAGE);
        assert_eq!(request.interaction_contract(), super::super::BACKGROUND_SYSTEM_MESSAGE);
        assert!(!request.system_message().contains("Conventional Commit"));
    }
}
