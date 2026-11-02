use serde::{Deserialize, Serialize};

/// Defines the approval policy selected for one Harness session.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalMode {
    #[default]
    /// Apply rules and request a decision for remaining tool requests.
    Ask,
    /// Reject tool requests that still require a decision.
    Deny,
    /// Approve remaining tool requests once, preserving explicit denials.
    Allow,
}

impl ApprovalMode {
    /// Return the stable user-facing label for this approval policy.
    pub const fn label(self) -> &'static str {
        match self {
            Self::Ask => "ask",
            Self::Deny => "deny",
            Self::Allow => "allow",
        }
    }
}

use crate::access::DirectoryGrant;
use crate::deadline::{InputDeadline, ResolutionSource};
use forge_shell::shell::{CommandHighlight, CommandShell};
use std::ops::Range;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalResolution {
    AllowOnce,
    AllowExact,
    AllowBroad,
    DenyOnce,
    DenyExact,
    DenyBroad,
    /// Automatic denial by saved permissions or filesystem access policy.
    DenyPolicy,
    /// Automatic denial because approval prompts are disabled.
    DenyMode,
    /// Rejects an expired request without recording a persistent rule.
    TimedOut,
    Cancel,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalChoice {
    pub id: String,
    pub label: String,
    pub resolution: ApprovalResolution,
    #[serde(default)]
    pub rule_list: Vec<(String, String)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directory: Option<DirectoryGrant>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalRequestView {
    pub id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<crate::ExecutionOwner>,
    pub exchange_id: Option<String>,
    pub turn_id: Option<String>,
    pub provider: String,
    pub reason: Option<String>,
    pub item_list: Vec<ApprovalItem>,
    /// Captures the original deadline independently of the active window.
    #[serde(default)]
    pub deadline: Option<InputDeadline>,
    /// Distinguishes automatic rejection from an explicit user decision.
    #[serde(default)]
    pub resolution_source: ResolutionSource,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// One unapproved target with choices scoped to that target.
pub struct ApprovalItem {
    pub id: String,
    pub title: String,
    pub detail: String,
    /// Full source commands containing this target, with all matching executable occurrences.
    pub command_list: Vec<ApprovalCommand>,
    pub choice_list: Vec<ApprovalChoice>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Preserves command context and source coordinates for formatting and scoped approval markers.
pub struct ApprovalCommand {
    /// Retains the complete script payload with shell launchers removed for presentation.
    pub source: String,
    /// Selects the language of the retained source.
    pub shell: CommandShell,
    /// Locates only executable occurrences covered by this item's choices.
    pub focus_range_list: Vec<Range<usize>>,
    /// Colors the original source independently of its approval markers.
    pub highlight_list: Vec<CommandHighlight>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// An explicit choice for one item in a pending approval request.
pub struct ApprovalAnswer {
    pub item_id: String,
    pub choice_id: String,
}
