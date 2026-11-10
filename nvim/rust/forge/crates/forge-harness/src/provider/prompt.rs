use super::{HARNESS_SYSTEM_MESSAGE, PromptMode, ProviderRequest};

use super::context_delivery::StandingSections;
use forge_provider_api::forge::{
    ExecuteTask, GoalTask, Permissions, PlanTask, ReadAccess, Task, WriteAccess,
};

/// Workflow material for an active task, delivered as the `<task>` section.
///
/// The broker resolves it from durable task state when it builds a request, because the prompt
/// builder cannot read the plan or goal stores itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum TaskInstructions {
    /// A Plan task's original request; the authoring scope supplies its paths.
    Plan { request: String },
    /// An Execute task's phase instructions and accepted design.
    Execute(ExecuteTask),
    /// A Goal task's objective.
    Goal(GoalTask),
}

impl ProviderRequest {
    /// Build the instructions a conversation receives once, when it opens or resumes.
    ///
    /// They hold only text that stays true for the whole conversation: the Harness overview, check
    /// and LSP guidance, the check shell, and the `<forge>` protocol worded for the provider's
    /// context `channel`. Everything that can change travels as standing sections from
    /// [`Self::standing_sections`].
    pub(crate) fn open_instructions(
        &self,
        channel: forge_provider_api::forge::ContextChannel,
    ) -> String {
        if self.mode == PromptMode::Background {
            return super::BACKGROUND_SYSTEM_MESSAGE.to_owned();
        }
        let shell = self
            .control_context
            .as_ref()
            .map_or(Default::default(), |context| context.check_shell);
        let interpreter = match shell {
            crate::session::CheckShell::Nushell => "Nushell",
            crate::session::CheckShell::System if cfg!(windows) => "PowerShell 7 (pwsh)",
            crate::session::CheckShell::System => "the system login shell",
        };
        format!(
            "{}\n\nSaved project checks run in {interpreter}, so setup commands must target that shell.\n\n{}\n\n{}",
            HARNESS_SYSTEM_MESSAGE.trim_end(),
            include_str!("../prompts/lsp.md").trim_end(),
            forge_provider_api::forge::overview(channel)
        )
    }

    /// Build the current standing sections for this turn.
    ///
    /// Each turn's host tools reach the model through native tool definitions, so no section
    /// lists them. Background generation receives no sections because its open-time
    /// instructions are complete.
    pub(crate) fn standing_sections(&self) -> StandingSections {
        if self.mode == PromptMode::Background {
            return StandingSections::default();
        }
        StandingSections {
            permissions: Some(self.permissions()),
            task: Some(self.task()),
        }
    }

    /// Describe the active task, or the absence of a task.
    ///
    /// The turn's interaction mode selects the task: planning and plan discussion use the
    /// editable planning directory and contract, Execute and Goal turns use the instructions the
    /// broker resolved, and every other turn has no task.
    fn task(&self) -> Task {
        let context = self.control_context.as_ref();
        let instructions = context.and_then(|context| context.task_instructions.as_ref());
        if matches!(self.mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            && let Some(scope) = context.and_then(|context| context.plan_authoring.as_ref())
        {
            let request = match instructions {
                Some(TaskInstructions::Plan { request }) => request.clone(),
                _ => String::new(),
            };
            return Task::Plan(PlanTask {
                plan_id: scope.plan_id().to_owned(),
                directory: scope.directory().display().to_string(),
                authoring_guide: scope.authoring_guide().display().to_string(),
                request,
                contract: crate::plan::PlanPrompt::planning_contract().to_owned(),
            });
        }
        match (self.mode, instructions) {
            (PromptMode::ExecutePlan, Some(TaskInstructions::Execute(task))) => {
                Task::Execute(task.clone())
            }
            (PromptMode::GoalContinuation, Some(TaskInstructions::Goal(task))) => {
                Task::Goal(task.clone())
            }
            _ => Task::None,
        }
    }

    /// Describe the approval mode, filesystem access, and toggleable toolsets Harness enforces
    /// for this turn.
    ///
    /// An invocation-only write root such as the planning directory replaces the workspace.
    fn permissions(&self) -> Permissions {
        use forge_provider_api::access::{DirectoryScope, WriteScope};
        let read = match self.access.read_access {
            DirectoryScope::Full => ReadAccess::Full,
            DirectoryScope::Workspace => ReadAccess::Workspace {
                directories: self.access.readable_directory.clone(),
            },
        };
        let write = match (
            self.access.write_access,
            self.access.effective_write_roots.as_deref(),
        ) {
            (WriteScope::Off, _) => WriteAccess::Off,
            (_, Some(roots)) => WriteAccess::PlanningDirectory {
                directories: roots.clone(),
            },
            (WriteScope::Full, None) => WriteAccess::Full,
            (WriteScope::Workspace, None) => WriteAccess::Workspace {
                directories: self.access.writable_directory.clone(),
            },
        };
        Permissions {
            approval: self.approval_mode,
            read,
            write,
            toolsets: crate::tools::toolsets(self.control_context.as_ref()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_provider_api::forge::{ContextChannel, TurnContext, TurnInput, render};

    fn request(mode: PromptMode) -> ProviderRequest {
        ProviderRequest {
            dispatch: forge_provider_api::submission::DispatchTracker::default(),
            execution_owner: None,
            harness_session_id: "prompt-test-session".into(),
            workspace: ".".into(),
            input: super::super::ProviderInput::from_text("hello"),
            mode,
            model: "default".into(),
            effort: "default".into(),
            context_window: None,
            service_tier: Default::default(),
            access: Default::default(),
            approval_mode: Default::default(),
            provider_session_id: None,
            submission: None,
            acknowledged_child_reports: Vec::new(),
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    /// Render a request's standing sections the way an adapter would.
    fn rendered(request: &ProviderRequest) -> String {
        let sections = request.standing_sections();
        render(
            &TurnContext {
                record: String::new(),
                permissions: sections.permissions,
                task: sections.task,
            },
            &TurnInput::user(forge_provider_api::PromptInput::Text(String::new())),
            ContextChannel::UserMessage,
        )
        .lead
    }

    /// Keep check and LSP guidance in open-time text and only changeable settings in sections.
    #[test]
    fn open_instructions_hold_static_guidance_and_sections_hold_settings() {
        let request = request(PromptMode::Chat);
        let open = request.open_instructions(ContextChannel::UserMessage);
        assert!(open.starts_with(HARNESS_SYSTEM_MESSAGE.trim_end()));
        assert!(open.contains("Saved project checks run in"));
        assert!(open.contains("prefer them over text search"));
        assert!(open.contains("When the harness_session_* toolset is on"));
        assert!(open.contains("at the start of a user message, before"));
        assert!(!open.contains("Tool approval:"));
        assert!(
            request
                .open_instructions(ContextChannel::DeveloperMessage)
                .contains("as a developer message")
        );
        let sections = request.standing_sections();
        assert_eq!(sections.task, Some(Task::None));
        assert_eq!(
            rendered(&request),
            "<forge>\n<permissions>\nTool approval: ask.\nRead access: full.\nWrite access: workspace.\nWrite directories: \".\".\nToolsets: lsp_* off, harness_session_* off, agent_* off, check_definition_edit off.\n</permissions>\n<task kind=\"none\">\nNo Harness task is active for this turn. Do not start, clear, or complete a task, and earlier task instructions no longer apply.\n</task>\n</forge>"
        );
    }

    /// Show the planning root in place of the workspace and list read directories when limited.
    #[test]
    fn permissions_name_the_planning_root_and_limited_reads() {
        let mut request = request(PromptMode::Plan);
        request.access.read_access = forge_provider_api::access::DirectoryScope::Workspace;
        request.access.readable_directory = vec!["D:/plan-data".into()];
        request.access.effective_write_roots =
            Some(std::sync::Arc::new(vec!["D:/plan-workspace".into()]));
        let text = rendered(&request);
        assert!(
            text.contains("Read access: workspace.\nRead directories: \".\", \"D:/plan-data\".")
        );
        assert!(text.contains(
            "Write access: planning directory only.\nWrite directories: \"D:/plan-workspace\"."
        ));
        request.access.write_access = forge_provider_api::access::WriteScope::Off;
        assert!(rendered(&request).contains("Write access: off.\nToolsets:"));
    }

    /// Use broker-resolved Execute and Goal instructions, and no task without them.
    #[test]
    fn execute_and_goal_turns_carry_their_task_instructions() {
        let mut goal = request(PromptMode::GoalContinuation);
        let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::GoalContinuation);
        context.task_instructions = Some(TaskInstructions::Goal(GoalTask {
            objective: "Ship the feature.".into(),
        }));
        goal.control_context = Some(context);
        assert_eq!(
            goal.standing_sections().task,
            Some(Task::Goal(GoalTask {
                objective: "Ship the feature.".into()
            }))
        );
        assert_eq!(
            request(PromptMode::GoalContinuation)
                .standing_sections()
                .task,
            Some(Task::None),
            "a turn without resolved instructions has no task"
        );
    }

    /// Give background generation its own open-time instructions without standing sections.
    #[test]
    fn background_generation_keeps_its_own_instructions() {
        let request = request(PromptMode::Background);
        assert_eq!(request.standing_sections(), StandingSections::default());
        assert_eq!(
            request.open_instructions(Default::default()),
            super::super::BACKGROUND_SYSTEM_MESSAGE
        );
    }
}
