use crate::session::{AccessPolicy, ApprovalMode, DirectoryScope, WriteScope};
use serde::{Deserialize, Serialize};

mod active;
pub(crate) mod command;
mod controller;
mod event;
mod execution;
mod installation;
pub(crate) use execution::{ExecutionSettlement, ExecutionSettlementKind};
pub(crate) use installation::{TaskInstallation, TaskInstallationWork};
mod state;
pub(crate) use active::StaleTask;
pub use active::{
    ExecutionEntry, ExecutionSnapshot, GoalSnapshot, PlanExecutionChoice, PlanSnapshot, Task,
    TaskContinuation, TaskDecision, TaskEntry, TaskId, TaskInputDelivery, TaskInputId,
    TaskInputIdentityError, TaskOutcome, TaskOutcomeState, TaskPermit, TaskRevision, TaskTurn,
    TaskTurnId, TaskWork, WorkflowSnapshot,
};
pub(crate) use active::{TaskReceiptInput, TaskReceiptPermit, TaskSnapshot};
pub(crate) use controller::TaskChange;
pub(crate) use controller::{
    OwnerTaskCommit, SessionTaskController, latest_turn_completed_normally,
};
pub(crate) use event::TaskEvent;
pub(crate) use state::TaskNextAction;
pub use state::{
    ExecutionContract, ExecutionState, GoalState, PlanningState, SuspensionReason, TaskLifecycle,
    TaskPhase, WorkflowState,
};

impl PermissionChange {
    /// Apply this intent to candidate state before validation and persistence.
    pub fn apply(
        self,
        access: &mut AccessPolicy,
        approval_mode: &mut ApprovalMode,
        sandbox_supported: bool,
    ) {
        match self {
            Self::ToggleReadScope => {
                access.read_access = match access.read_access {
                    DirectoryScope::Workspace => DirectoryScope::Full,
                    DirectoryScope::Full => DirectoryScope::Workspace,
                }
            }
            Self::ToggleWriteEnabled => {
                access.write_access = match access.write_access {
                    WriteScope::Off => WriteScope::Workspace,
                    WriteScope::Workspace | WriteScope::Full => WriteScope::Off,
                }
            }
            Self::ToggleWriteScope => {
                access.write_access = match access.write_access {
                    WriteScope::Off | WriteScope::Workspace => WriteScope::Full,
                    WriteScope::Full => WriteScope::Workspace,
                }
            }
            Self::SetApproval { mode } => *approval_mode = mode,
            Self::ToggleYolo => {
                let was_yolo = access.read_access == DirectoryScope::Full
                    && access.write_access == WriteScope::Full
                    && *approval_mode == ApprovalMode::Allow
                    && !access.sandbox;
                access.sandbox = was_yolo && sandbox_supported;
                access.read_access = DirectoryScope::Full;
                access.write_access = WriteScope::Full;
                *approval_mode = if was_yolo {
                    ApprovalMode::Ask
                } else {
                    ApprovalMode::Allow
                };
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
/// Retains the cumulative policy selected by admitted permission intents.
pub struct PermissionSelection {
    /// Selected provider sandbox enablement.
    pub sandbox: bool,
    /// Selected read boundary, independent of directory grants.
    pub read_access: DirectoryScope,
    /// Selected write boundary, independent of directory grants.
    pub write_access: WriteScope,
    /// Selected approval behavior.
    pub approval_mode: ApprovalMode,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
/// Records whether a workflow can admit another attempt.
pub enum TaskStatus {
    /// Persisted before its first attempt starts.
    Ready,
    /// Owns an admitted provider attempt.
    Running,
    /// Collects an interrupted attempt before another can start.
    Stopping,
    /// Requires an explicit resume intent.
    Paused,
    /// Requires an answer or plan review.
    Waiting,
    /// Cannot advance without resolving a recorded constraint.
    Blocked,
    /// Satisfied its completion gates and cannot resume.
    Completed,
    /// Was cleared and cannot resume.
    Cancelled,
    /// Retains a failure for inspection before explicit retry.
    Failed,
}

impl TaskStatus {
    /// Terminal tasks retain history but cannot admit new attempts.
    pub fn terminal(self) -> bool {
        matches!(self, Self::Completed | Self::Cancelled)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn yolo_toggles_only_from_full_unsandboxed_allow() {
        for sandbox_supported in [false, true] {
            for sandbox in [false, true] {
                for read in [DirectoryScope::Workspace, DirectoryScope::Full] {
                    for write in [WriteScope::Off, WriteScope::Workspace, WriteScope::Full] {
                        for approval in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow]
                        {
                            let mut access = AccessPolicy {
                                sandbox,
                                read_access: read,
                                write_access: write,
                                readable_directory: vec!["read-grant".into()],
                                writable_directory: vec!["write-grant".into()],
                                ..Default::default()
                            };
                            let original = access.clone();
                            let mut mode = approval;
                            let was_yolo = !sandbox
                                && read == DirectoryScope::Full
                                && write == WriteScope::Full
                                && approval == ApprovalMode::Allow;
                            PermissionChange::ToggleYolo.apply(
                                &mut access,
                                &mut mode,
                                sandbox_supported,
                            );
                            assert_eq!(access.abbreviation(), "rxwx");
                            assert_eq!(
                                mode,
                                if was_yolo {
                                    ApprovalMode::Ask
                                } else {
                                    ApprovalMode::Allow
                                }
                            );
                            assert_eq!(access.sandbox, was_yolo && sandbox_supported);
                            assert_eq!(access.readable_directory, original.readable_directory);
                            assert_eq!(access.writable_directory, original.writable_directory);
                            assert_eq!(access.isolation_mode, original.isolation_mode);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn policy_transitions_preserve_independent_dimensions() {
        for approval in [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow] {
            for read in [DirectoryScope::Workspace, DirectoryScope::Full] {
                for write in [WriteScope::Off, WriteScope::Workspace, WriteScope::Full] {
                    let original = AccessPolicy {
                        read_access: read,
                        write_access: write,
                        ..Default::default()
                    };
                    for next_approval in
                        [ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow]
                    {
                        let mut access = original.clone();
                        let mut mode = approval;
                        PermissionChange::SetApproval {
                            mode: next_approval,
                        }
                        .apply(&mut access, &mut mode, true);
                        assert_eq!(access, original);
                        assert_eq!(mode, next_approval);
                    }
                    let mut access = original.clone();
                    let mut mode = approval;
                    PermissionChange::ToggleReadScope.apply(&mut access, &mut mode, true);
                    assert_ne!(access.read_access, read);
                    assert_eq!(access.write_access, write);
                    assert_eq!(mode, approval);
                    PermissionChange::ToggleReadScope.apply(&mut access, &mut mode, true);
                    assert_eq!(access, original);

                    PermissionChange::ToggleWriteEnabled.apply(&mut access, &mut mode, true);
                    assert_eq!(access.read_access, read);
                    assert_eq!(
                        access.write_access,
                        if write == WriteScope::Off {
                            WriteScope::Workspace
                        } else {
                            WriteScope::Off
                        }
                    );
                    PermissionChange::ToggleWriteEnabled.apply(&mut access, &mut mode, true);
                    assert_eq!(
                        access.write_access,
                        if write == WriteScope::Off {
                            WriteScope::Off
                        } else {
                            WriteScope::Workspace
                        }
                    );
                    assert_eq!(mode, approval);

                    access = original.clone();
                    PermissionChange::ToggleWriteScope.apply(&mut access, &mut mode, true);
                    assert_eq!(access.read_access, read);
                    assert_eq!(
                        access.write_access,
                        if write == WriteScope::Full {
                            WriteScope::Workspace
                        } else {
                            WriteScope::Full
                        }
                    );
                    assert_eq!(mode, approval);
                    PermissionChange::ToggleYolo.apply(&mut access, &mut mode, true);
                    assert_eq!(access.abbreviation(), "rxwx");
                    assert_eq!(mode, ApprovalMode::Allow);
                    assert!(!access.sandbox);
                    assert_eq!(access.isolation_mode, original.isolation_mode);
                }
            }
        }
    }
}

pub use forge_provider_api::input::TaskKind;

#[cfg(test)]
mod integration_test;

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
/// Captures a policy intent resolved once against authoritative session state.
pub enum PermissionChange {
    /// Toggle workspace and full reads without changing writes or approval.
    ToggleReadScope,
    /// Toggle writes between off and workspace without remembering full scope.
    ToggleWriteEnabled,
    /// Toggle enabled writes between workspace and full, enabling full from off.
    ToggleWriteScope,
    /// Change only the tool approval selection.
    SetApproval { mode: ApprovalMode },
    #[serde(rename = "enable_yolo")]
    /// Toggle full unsandboxed Allow to full-access Ask with supported sandboxing.
    ToggleYolo,
}
