//! Harness approval presentation and policy-choice records.
//!
//! Native adapters translate permission requests into semantic targets and receive only the
//! normalized `PermissionResponse`. Titles, offered rule choices, command focus ranges, syntax
//! highlights, deadlines, and resolution provenance are Harness presentation and policy state,
//! so they live here rather than in the provider API.
use crate::permissions::directory::DirectoryGrant;
use crate::session::{InputDeadline, ResolutionSource};
use forge_shell::shell::{CommandHighlight, CommandShell};
use serde::{Deserialize, Serialize};
use std::ops::Range;

/// Record the host decision selected for one approval item.
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

/// Offer one host policy decision, with the rules or grant it would persist.
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

/// Present one pending host approval with its owner and item choices.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalRequestView {
    pub id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<crate::provider::ThreadOwner>,
    pub exchange_id: Option<String>,
    pub turn_id: Option<forge_provider_api::TurnId>,
    pub provider: Option<forge_provider_api::ProviderId>,
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
