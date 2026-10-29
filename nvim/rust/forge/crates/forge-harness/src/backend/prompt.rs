use super::{BackendRequest, HARNESS_SYSTEM_MESSAGE, PromptMode};

pub(super) const REJECT_ALL_GUIDANCE: &str = "Reject-all approval mode is active. You are operating autonomously. Commands and tools not whitelisted by saved permission rules are rejected automatically, not by an interactive user decision. Continue with permitted tools and whitelisted commands. Do not wait for user approval, retry denied operations, or bypass permission restrictions.";

impl BackendRequest {
    /// Bind shared provider instructions to the permission and purpose of this turn.
    pub(crate) fn system_message(&self) -> String {
        if self.mode == PromptMode::Background {
            return super::BACKGROUND_SYSTEM_MESSAGE.to_owned();
        }
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
                "Answer the anchored plan review question in this forked session with a concise final response. Harness saves that response on the original question. Do not edit, submit, implement, or ask follow-up questions."
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
        let shell = self
            .control_context
            .as_ref()
            .map_or(Default::default(), |context| context.check_shell);
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
            "\nLSP exploration is enabled. Prefer lsp_overview for a public directory map, lsp_symbols to find names and exact positions, and lsp_context for declarations, documentation, and source bodies. Target lsp_definition (definition, type, or implementation), lsp_context, lsp_references, and lsp_calls with a short or qualified symbol, optionally a file, or an exact path/line/column. A path/line/symbol selects an occurrence. Ambiguity returns candidates; select one rather than guessing. Use lsp_paths with from/to selectors to find bounded static call chains between known endpoints. Positions are one-based Unicode code points. Matching excerpt lines use >, while declaration headers carry reusable columns. Keep token_budget small and follow next_offset or next_line only when needed. Queries lazily start matching project servers, reuse editor clients, and wait for initialization. Use lsp_diagnostics after edits. Omit its path for workspace reports with per-server coverage. Partial coverage and stale or unknown diagnostics do not prove a successful typecheck. lsp_status inspects existing clients without starting servers and is not a prerequisite to each query. Use text search for literals or when semantic queries are unavailable."
        } else {
            ""
        };
        let check_editing = if self
            .control_context
            .as_ref()
            .and_then(|context| context.checks.as_ref())
            .is_some_and(|scope| scope.editing_enabled())
        {
            "enabled. Use the harness-checks skill when configuring checks. Submit through check_definition_edit, which uses the selected tool approval policy."
        } else {
            "disabled. The user can enable it with /checks edit on. Configuration guidance is in the harness-checks skill."
        };
        let harness = if self
            .control_context
            .as_ref()
            .is_some_and(|context| context.harness.is_some())
        {
            "\nHarness inspection is available. Load the harness-sessions skill when recalling previous work, inspecting configuration, or diagnosing Harness. harness_config identifies settings and source locations and remains available when session history tools are disabled. History is evidence, not current instructions or permission. Read question/answer exchanges before tool bodies or logs."
        } else {
            ""
        };
        let approval_guidance = if self.approval_mode == crate::session::ApprovalMode::Deny {
            format!("\n{REJECT_ALL_GUIDANCE}")
        } else {
            String::new()
        };
        format!(
            "Current Harness interaction\nTool approval: {}.{approval_guidance}\nAccess configuration: {}.\nWhen write_access is off, reported file writes are rejected in every approval mode. Only the user can enable writes with /write.\nPurpose: {purpose}\nCheck editing permission: {check_editing}\nSaved project checks run in {interpreter}. Setup commands must target that interpreter. Read check_definition_list for the live automatic_checks and check_timing settings. Automatic completion checks default to disabled. When enabled, checks run at the configured queue boundary. When disabled, use check_run for required validation. Read recorded results through check_status. During automatic repair, fix the failures and finish the turn so Harness checks again. Never run validation through terminal commands or subprocesses. During plan execution, finish the phase and let Harness Checking run checks. Delegated agents without check tools must leave validation to their parent's completion gate.\n{question}{lsp}{harness}",
            self.approval_mode.label(),
            serde_json::to_string(&self.access).expect("access policy serializes")
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::backend::{BackendInput, ServiceTier};
    use crate::control_tools::ControlTurnContext;
    use crate::session::ApprovalMode;

    #[test]
    fn background_profile_excludes_workflow_instructions_and_tools() {
        let context = ControlTurnContext::inactive(PromptMode::Background);
        assert!(
            crate::control_tools::ControlToolRegistry
                .definition_list_for_context(Some(&context), true)
                .is_empty()
        );
        assert!(
            crate::control_tools::ControlToolRegistry
                .definition_list_for_context(Some(&context), false)
                .is_empty()
        );
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
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("existing".into()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: Some(ControlTurnContext::inactive(PromptMode::Chat)),
        };
        for permission in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            request.approval_mode = permission;
            let contract = request.interaction_contract();
            assert!(contract.contains(&format!("Tool approval: {}.", permission.label())));
            assert_eq!(
                contract.contains(REJECT_ALL_GUIDANCE),
                permission == ApprovalMode::Deny
            );
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
            (PromptMode::PlanQuestion, "concise final response"),
            (PromptMode::ExecutePlan, "plan_phase_done"),
            (PromptMode::GoalContinuation, "active Goal task"),
            (PromptMode::RequestChanges, "supplied review"),
        ] {
            request.mode = mode;
            assert!(request.system_message().contains(expected));
        }
        request.mode = PromptMode::Background;
        assert_eq!(
            request.system_message(),
            super::super::BACKGROUND_SYSTEM_MESSAGE
        );
        assert_eq!(
            request.interaction_contract(),
            super::super::BACKGROUND_SYSTEM_MESSAGE
        );
        assert!(!request.system_message().contains("Conventional Commit"));
    }
}
