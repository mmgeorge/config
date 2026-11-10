//! Standing sections: context that stays in effect until Harness sends a newer copy.
//!
//! Harness tracks each section per native conversation and sends it again only when its value
//! changes or the conversation may have lost it, so these types derive `PartialEq` for change
//! detection.

use serde::{Deserialize, Serialize};

use super::writer::Writer;
use crate::approval::ApprovalMode;

/// Guidance shown under the reject-all approval mode, where no user reviews requests.
pub const REJECT_ALL_GUIDANCE: &str = "Tool approval is set to reject all, and no user is reviewing requests. Commands and tools that saved permission rules do not allow are rejected automatically. Continue with the allowed tools and commands. Do not wait for approval, retry rejected operations, or work around the restrictions.";

/// Tool approval, filesystem access, and toggleable toolsets that Harness enforces for a turn.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Permissions {
    /// Approval policy for tool and command requests.
    pub approval: ApprovalMode,
    /// Directories the model may read.
    pub read: ReadAccess,
    /// Directories the model may write.
    pub write: WriteAccess,
    /// Toolsets that can change during a conversation, in display order.
    pub toolsets: Vec<Toolset>,
}

/// Readable directories. `"."` always names the workspace.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "scope", rename_all = "snake_case")]
pub enum ReadAccess {
    /// Every path is readable.
    Full,
    /// The workspace and these additional directories are readable.
    Workspace { directories: Vec<String> },
}

/// Writable directories. `"."` always names the workspace.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "scope", rename_all = "snake_case")]
pub enum WriteAccess {
    /// No path is writable.
    Off,
    /// Every path is writable.
    Full,
    /// The workspace and these additional directories are writable.
    Workspace { directories: Vec<String> },
    /// Only these invocation roots, such as the planning directory, are writable.
    PlanningDirectory { directories: Vec<String> },
}

/// One toolset that can be turned on or off during a conversation.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Toolset {
    /// Prefix the toolset's tools share, such as `lsp_*`, or its single tool name.
    pub name: String,
    /// Whether the toolset's tools are available in this turn.
    pub enabled: bool,
}

impl Permissions {
    pub(crate) fn write(&self, writer: &mut Writer) {
        let quoted = |directories: &mut dyn Iterator<Item = &str>| {
            directories
                .map(|directory| format!("\"{directory}\""))
                .collect::<Vec<_>>()
                .join(", ")
        };
        let mut lines = vec![format!("Tool approval: {}.", self.approval.label())];
        if self.approval == ApprovalMode::Deny {
            lines.push(REJECT_ALL_GUIDANCE.to_owned());
        }
        match &self.read {
            ReadAccess::Full => lines.push("Read access: full.".into()),
            ReadAccess::Workspace { directories } => {
                lines.push("Read access: workspace.".into());
                let mut all = std::iter::once(".").chain(directories.iter().map(String::as_str));
                lines.push(format!("Read directories: {}.", quoted(&mut all)));
            }
        }
        match &self.write {
            WriteAccess::Off => lines.push("Write access: off.".into()),
            WriteAccess::Full => lines.push("Write access: full.".into()),
            WriteAccess::Workspace { directories } => {
                lines.push("Write access: workspace.".into());
                let mut all = std::iter::once(".").chain(directories.iter().map(String::as_str));
                lines.push(format!("Write directories: {}.", quoted(&mut all)));
            }
            WriteAccess::PlanningDirectory { directories } => {
                lines.push("Write access: planning directory only.".into());
                lines.push(format!(
                    "Write directories: {}.",
                    quoted(&mut directories.iter().map(String::as_str))
                ));
            }
        }
        if !self.toolsets.is_empty() {
            lines.push(format!(
                "Toolsets: {}.",
                self.toolsets
                    .iter()
                    .map(|toolset| format!(
                        "{} {}",
                        toolset.name,
                        if toolset.enabled { "on" } else { "off" }
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        writer.element("permissions", &[], &lines.join("\n"));
    }
}

/// The active Harness task and its standing workflow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Task {
    /// No task is active; earlier task instructions no longer apply.
    None,
    /// A Plan task is drafting or discussing a plan.
    Plan(PlanTask),
    /// An Execute task is implementing an accepted plan.
    Execute(ExecuteTask),
    /// A Goal task is pursuing an objective.
    Goal(GoalTask),
}

/// A Plan task's editable directory, its guide, the user's request, and the planning contract.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTask {
    pub plan_id: String,
    /// Absolute path of the editable planning directory.
    pub directory: String,
    /// Absolute path of the read-only authoring guide.
    pub authoring_guide: String,
    /// The user's original planning request.
    pub request: String,
    /// The planning workflow contract.
    pub contract: String,
}

/// An Execute task's identity, phase instructions, findings, and accepted design.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ExecuteTask {
    pub execution_id: String,
    pub plan_id: String,
    /// Lowercase phase name, such as `implement` or `verify`.
    pub phase: String,
    /// Accepted plan revision the execution implements.
    pub revision: u64,
    /// Instructions for the current phase.
    pub phase_instructions: String,
    /// Findings that remain unresolved.
    pub findings: Vec<String>,
    /// Reviewer approval comments, which are context rather than plan edits.
    pub approval_comments: Vec<String>,
    /// The accepted semantic design.
    pub design: String,
}

/// A Goal task's objective.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct GoalTask {
    pub objective: String,
}

/// Sentence that makes every user message during planning input to the plan.
const PLAN_INPUT_RULE: &str = "Treat each user message during this task as input to the plan: the request on the first turn, and feedback after that.";

/// Completion rule for a Goal task.
const GOAL_RULE: &str = "Continue working until the goal is complete. Report completion with goal_complete only when all required work is done, or a concrete blocker with goal_blocked when no allowed progress remains.";

/// Text for a turn without an active task.
const NO_TASK: &str = "No Harness task is active for this turn. Do not start, clear, or complete a task, and earlier task instructions no longer apply.";

impl Task {
    pub(crate) fn write(&self, writer: &mut Writer) {
        match self {
            Self::None => {
                writer.element("task", &[("kind", "none".into())], NO_TASK);
            }
            Self::Plan(plan) => {
                writer.open(
                    "task",
                    &[("kind", "plan".into()), ("plan_id", plan.plan_id.clone())],
                );
                writer.text(&format!(
                    "Planning directory: {}\nAuthoring guide: {}\n{PLAN_INPUT_RULE}",
                    plan.directory, plan.authoring_guide
                ));
                writer.element("request", &[], &plan.request);
                writer.text(&plan.contract);
                writer.close();
            }
            Self::Execute(execute) => {
                writer.open(
                    "task",
                    &[
                        ("kind", "execute".into()),
                        ("execution_id", execute.execution_id.clone()),
                        ("plan_id", execute.plan_id.clone()),
                        ("phase", execute.phase.clone()),
                        ("revision", execute.revision.to_string()),
                    ],
                );
                writer.element("phase_instructions", &[], &execute.phase_instructions);
                writer.element("findings", &[], &execute.findings.join("\n"));
                writer.element(
                    "approval_comments",
                    &[],
                    &execute.approval_comments.join("\n"),
                );
                writer.element("design", &[], &execute.design);
                writer.close();
            }
            Self::Goal(goal) => {
                writer.open("task", &[("kind", "goal".into())]);
                writer.element("objective", &[], &goal.objective);
                writer.text(GOAL_RULE);
                writer.close();
            }
        }
    }

    /// Plan ID the task names, for adapters and tests that act on the active plan.
    pub fn plan_id(&self) -> Option<&str> {
        match self {
            Self::Plan(plan) => Some(&plan.plan_id),
            Self::Execute(execute) => Some(&execute.plan_id),
            Self::None | Self::Goal(_) => None,
        }
    }
}
