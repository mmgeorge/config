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

/// The origin of an aside session: a fork the user opened to ask about a session that keeps
/// running.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Aside {
    /// Harness session the aside was forked from.
    pub origin_session_id: String,
    /// The origin's display name when the aside was opened; empty when it had none.
    pub origin_session_name: String,
}

/// Role of an aside session and the tools that reach its origin.
const ASIDE_RULE: &str = "This session is an aside: a fork of the origin session named above, which the user opened with /btw to ask about that session without interrupting it. The origin continues its own work in another tab. This conversation holds a copy of the origin's history up to the fork, without any exchange that was still running then.\n\nThe user will ask questions about the origin session. Answer from the copied history when it is enough. To see what the origin did after the fork or is doing now, inspect it with the harness_session_* tools, passing the origin session ID as session_id: harness_session_exchange_list lists its exchanges, harness_session_exchange shows one exchange's messages, tool calls, and progress so far, harness_session_tool reads one tool's output, and harness_session_status reports its task and access. The origin keeps changing, so read it again instead of relying on an earlier read.\n\nDo not continue the origin's task here unless the user asks. This is a full session: when the user gives you other work, do it here.\n\norigin_message sends the origin session a message. It arrives there in a <session_message> element that names this session, and starts a turn or waits in the queue behind running work. Send one only when the user asks, or to report a result the user asked you to send back.";

impl Aside {
    pub(crate) fn write(&self, writer: &mut Writer) {
        let mut attributes = vec![("origin_session_id", self.origin_session_id.clone())];
        if !self.origin_session_name.is_empty() {
            attributes.push(("origin_session_name", self.origin_session_name.clone()));
        }
        writer.element("aside", &attributes, ASIDE_RULE);
    }
}

/// The child agent a conversation belongs to: its path, its parent, and the user's profile
/// instructions for it.
///
/// The section stands rather than travelling with each task so that compaction, which clears the
/// delivery record, cannot summarize the profile instructions away.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ChildAgent {
    /// The agent's own path, such as `/root/explore-1`.
    pub path: String,
    /// Path of the agent that assigns its tasks and receives its answers.
    pub parent: String,
    /// Instructions from the agent's profile; empty when the profile has none.
    pub profile_instructions: String,
}

/// Role of a child agent, written after the section names its path and parent.
const CHILD_AGENT_RULE: &str = "You are a child agent. Your parent assigns each task in an <agent_message type=\"new_task\"> and may send more information about it in an <agent_message type=\"message\">. Harness delivers your final response to each task to your parent as your answer, so make that response a complete report of what you did and found.";

impl ChildAgent {
    pub(crate) fn write(&self, writer: &mut Writer) {
        writer.open(
            "agent",
            &[("path", self.path.clone()), ("parent", self.parent.clone())],
        );
        writer.text(CHILD_AGENT_RULE);
        if !self.profile_instructions.trim().is_empty() {
            writer.element("profile_instructions", &[], &self.profile_instructions);
        }
        writer.close();
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
