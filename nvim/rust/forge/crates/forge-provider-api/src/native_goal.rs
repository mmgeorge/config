/// Select the native goal action for one admitted invocation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NativeGoalCommand {
    Set { objective: String },
    Resume { objective: String },
}

/// Select the complete provider-owned goal state without confusing suspension with absence.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NativeGoalUpdate {
    /// Remove a native goal whose task no longer exists.
    Absent,
    /// Retain the canonical objective and its current lifecycle state.
    Present {
        /// Canonical task objective, preserved across suspension and resumption.
        objective: String,
        /// Lifecycle state the provider must persist.
        status: NativeGoalStatus,
    },
}

/// Describe the lifecycle states accepted by native goal synchronization.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeGoalStatus {
    /// Permit native goal continuation.
    Active,
    /// Retain the goal while suspending continuation.
    Paused,
    /// Retain the successfully completed outcome.
    Complete,
    /// Retain a blocker that requires resolution before continuation.
    Blocked,
    /// Preserve the provider's usage-limit stop state.
    UsageLimited,
    /// Preserve the provider's budget-limit stop state.
    BudgetLimited,
}
