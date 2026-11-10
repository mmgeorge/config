use super::{HARNESS_SYSTEM_MESSAGE, PromptMode, ProviderRequest};

use crate::permissions::provider::REJECT_ALL_GUIDANCE;
use forge_provider_api::turn_context::{ContextPart, ContextSection};

/// Workflow material for an active Execute or Goal task, delivered as the `task` section.
///
/// The broker resolves it from durable task state when it builds a request, because the prompt
/// builder cannot read the plan or goal stores itself.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum TaskInstructions {
    /// An Execute task's phase instructions and accepted design.
    Execute {
        /// Lowercase phase name, such as `implement` or `verify`.
        phase: String,
        /// Accepted plan revision the execution implements.
        revision: u64,
        /// Execution identity, phase instructions, findings, comments, and accepted design.
        text: String,
    },
    /// A Goal task's objective.
    Goal {
        /// Objective the user supplied when starting the goal.
        objective: String,
    },
}

impl ProviderRequest {
    /// Build the instructions a conversation receives once, when it opens or resumes.
    ///
    /// They hold only text that stays true for the whole conversation: the Harness overview, check
    /// and LSP guidance, the check shell, and the `<forge>` protocol worded for the provider's
    /// context `channel`. Everything that can change travels as tracked sections from
    /// [`Self::context_sections`].
    pub(crate) fn open_instructions(
        &self,
        channel: forge_provider_api::turn_context::ContextChannel,
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
            forge_provider_api::turn_context::overview(channel)
        )
    }

    /// Build every tracked context section for this turn, in delivery order.
    ///
    /// Each turn's host tools reach the model through native tool definitions, so no section
    /// lists them. Background generation receives no sections because its open-time
    /// instructions are complete.
    pub(crate) fn context_sections(&self) -> Vec<ContextPart> {
        if self.mode == PromptMode::Background {
            return Vec::new();
        }
        vec![
            ContextPart::new(ContextSection::Permissions, self.permissions_text()),
            self.task_part(),
        ]
    }

    /// Build workflow instructions that belong to this input only and are not tracked.
    pub(crate) fn input_instructions(&self) -> String {
        let additional = self.input.additional_instructions();
        let mut parts = Vec::new();
        if self
            .control_context
            .as_ref()
            .is_some_and(|context| context.planning_feedback)
        {
            parts.push(
                "Harness has already recorded the supplied planning answers. Apply those answers without reopening their questions.",
            );
        }
        if !additional.is_empty() {
            parts.push(additional);
        }
        parts.join("\n")
    }

    /// Describe the active task and its workflow instructions, or the absence of a task.
    ///
    /// The turn's interaction mode selects the task: planning and plan discussion use the
    /// editable planning directory and contract, Execute and Goal turns use the instructions the
    /// broker resolved, and every other turn has no task.
    fn task_part(&self) -> ContextPart {
        let context = self.control_context.as_ref();
        if matches!(self.mode, PromptMode::Plan | PromptMode::PlanDiscussion)
            && let Some(scope) = context.and_then(|context| context.plan_authoring.as_ref())
        {
            return ContextPart::new(
                ContextSection::Task,
                format!(
                    "{}\nTreat each user message during this task as input to the plan: the request on the first turn, and feedback after that.\n\n{}",
                    scope.prompt(),
                    crate::plan::PlanPrompt::planning_contract()
                ),
            )
            .with_attribute("kind", "plan");
        }
        let instructions = context.and_then(|context| context.task_instructions.as_ref());
        match (self.mode, instructions) {
            (
                PromptMode::ExecutePlan,
                Some(TaskInstructions::Execute {
                    phase,
                    revision,
                    text,
                }),
            ) => ContextPart::new(ContextSection::Task, text.clone())
                .with_attribute("kind", "execute")
                .with_attribute("phase", phase.clone())
                .with_attribute("revision", revision.to_string()),
            (PromptMode::GoalContinuation, Some(TaskInstructions::Goal { objective })) => {
                ContextPart::new(
                    ContextSection::Task,
                    format!(
                        "Objective: {objective}\nContinue working until the goal is complete. Report completion with goal_complete only when all required work is done, or a concrete blocker with goal_blocked when no allowed progress remains."
                    ),
                )
                .with_attribute("kind", "goal")
            }
            _ => ContextPart::new(
                ContextSection::Task,
                "No Harness task is active for this turn. Do not start, clear, or complete a task, and earlier task instructions no longer apply.",
            )
            .with_attribute("kind", "none"),
        }
    }

    /// Describe the approval mode, filesystem access, and toggleable toolsets Harness enforces
    /// for this turn.
    ///
    /// `"."` names the workspace. Directory lists appear only where they limit access: reads
    /// under workspace scope, and writes under workspace scope or an invocation-only root such as
    /// the planning directory, which replaces the workspace.
    fn permissions_text(&self) -> String {
        use forge_provider_api::access::{DirectoryScope, WriteScope};
        let quoted = |directories: &mut dyn Iterator<Item = &str>| {
            directories
                .map(|directory| format!("\"{directory}\""))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let mut lines = vec![format!("Tool approval: {}.", self.approval_mode.label())];
        if self.approval_mode == crate::session::ApprovalMode::Deny {
            lines.push(REJECT_ALL_GUIDANCE.to_owned());
        }
        match self.access.read_access {
            DirectoryScope::Full => lines.push("Read access: full.".into()),
            DirectoryScope::Workspace => {
                lines.push("Read access: workspace.".into());
                let mut directories = std::iter::once(".")
                    .chain(self.access.readable_directory.iter().map(String::as_str));
                lines.push(format!("Read directories: {}.", quoted(&mut directories)));
            }
        }
        match (
            self.access.write_access,
            self.access.effective_write_roots.as_deref(),
        ) {
            (WriteScope::Off, _) => lines.push("Write access: off.".into()),
            (_, Some(roots)) => {
                lines.push("Write access: planning directory only.".into());
                lines.push(format!(
                    "Write directories: {}.",
                    quoted(&mut roots.iter().map(String::as_str))
                ));
            }
            (WriteScope::Full, None) => lines.push("Write access: full.".into()),
            (WriteScope::Workspace, None) => {
                lines.push("Write access: workspace.".into());
                let mut directories = std::iter::once(".")
                    .chain(self.access.writable_directory.iter().map(String::as_str));
                lines.push(format!("Write directories: {}.", quoted(&mut directories)));
            }
        }
        lines.push(crate::tools::toolset_line(self.control_context.as_ref()));
        lines.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    fn task(request: &ProviderRequest) -> ContextPart {
        request.context_sections().remove(1)
    }

    fn permissions(request: &ProviderRequest) -> String {
        request.context_sections().remove(0).text
    }

    /// Keep check and LSP guidance in open-time text and only changeable settings in sections.
    #[test]
    fn open_instructions_hold_static_guidance_and_sections_hold_settings() {
        use forge_provider_api::turn_context::ContextChannel;
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
        let sections = request.context_sections();
        assert_eq!(
            sections.iter().map(|part| part.section).collect::<Vec<_>>(),
            ContextSection::ALL
        );
        assert_eq!(
            sections[1].attributes,
            [("kind".to_owned(), "none".to_owned())]
        );
        assert_eq!(
            sections[0].text,
            "Tool approval: ask.\nRead access: full.\nWrite access: workspace.\nWrite directories: \".\".\nToolsets: lsp_* off, harness_session_* off, agent_* off, check_definition_edit off."
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
        let text = permissions(&request);
        assert!(
            text.contains("Read access: workspace.\nRead directories: \".\", \"D:/plan-data\".")
        );
        assert!(text.contains(
            "Write access: planning directory only.\nWrite directories: \"D:/plan-workspace\"."
        ));
        request.access.write_access = forge_provider_api::access::WriteScope::Off;
        assert!(permissions(&request).contains("Write access: off.\nToolsets:"));
    }

    /// Render Execute and Goal turns from broker-resolved instructions with identifying attributes.
    #[test]
    fn execute_and_goal_turns_carry_their_task_instructions() {
        let mut execute = request(PromptMode::ExecutePlan);
        let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::ExecutePlan);
        context.task_instructions = Some(TaskInstructions::Execute {
            phase: "verify".into(),
            revision: 3,
            text: "Accepted semantic design: {}".into(),
        });
        execute.control_context = Some(context);
        let part = task(&execute);
        assert_eq!(
            part.element(),
            "<task kind=\"execute\" phase=\"verify\" revision=\"3\">\nAccepted semantic design: {}\n</task>"
        );
        let mut goal = request(PromptMode::GoalContinuation);
        let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::GoalContinuation);
        context.task_instructions = Some(TaskInstructions::Goal {
            objective: "Ship the feature.".into(),
        });
        goal.control_context = Some(context);
        let part = task(&goal);
        assert_eq!(part.attributes, [("kind".to_owned(), "goal".to_owned())]);
        assert!(part.text.starts_with("Objective: Ship the feature."));
        assert_eq!(
            task(&request(PromptMode::GoalContinuation)).attributes,
            [("kind".to_owned(), "none".to_owned())],
            "a turn without resolved instructions has no task"
        );
    }

    /// Give background generation its own open-time instructions without tracked sections.
    #[test]
    fn background_generation_keeps_its_own_instructions() {
        let request = request(PromptMode::Background);
        assert!(request.context_sections().is_empty());
        assert_eq!(
            request.open_instructions(Default::default()),
            super::super::BACKGROUND_SYSTEM_MESSAGE
        );
        assert!(
            request.input_instructions().is_empty(),
            "the Background rules are sent once, at open, not with every request"
        );
    }
}
