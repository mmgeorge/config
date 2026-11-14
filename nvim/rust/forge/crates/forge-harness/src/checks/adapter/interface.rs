use super::{
    AdapterError, AdapterProcesses, DiscoverRequest, Discovery, PathSelection, RunExit,
    TestSelection,
    events::{DiagnosticSink, FormatSink, TestEventSink},
};
use std::path::Path;

/// Supply the execution resources owned by one supervised check command.
pub struct AdapterContext<'a> {
    /// Repository root against which all protocol paths are interpreted.
    pub workspace: &'a Path,
    /// Validated check directory inside the repository.
    pub directory: &'a Path,
    /// Launcher carrying this command's cancellation, deadline, and log.
    pub processes: &'a AdapterProcesses,
    /// Private command directory for protocol files.
    pub scratch: &'a Path,
}

/// Describe the tool-specific identity shared by every category adapter.
pub trait CheckAdapter: Send + Sync {
    /// Get the configuration key used in the adapter catalog.
    fn kind(&self) -> &'static str;

    /// Get the configured inputs that must participate in evidence invalidation.
    fn evidence_identity(&self) -> serde_json::Value;
}

/// Discover and run test cases while leaving process ownership to Harness.
#[async_trait::async_trait]
pub trait TestAdapter: CheckAdapter {
    /// Discover a validated tree without running cases; static mode cannot build or invoke binaries.
    async fn discover(
        &self,
        context: &AdapterContext<'_>,
        request: &DiscoverRequest,
    ) -> Result<Discovery, AdapterError>;

    /// Run the admitted selection and deliver every normalized event to its validating sink.
    async fn run(
        &self,
        context: &AdapterContext<'_>,
        selection: &TestSelection,
        events: &mut TestEventSink,
    ) -> Result<RunExit, AdapterError>;
}

/// Report type errors through the shared diagnostic contract.
#[async_trait::async_trait]
pub trait TypeCheckAdapter: CheckAdapter {
    /// Check selected paths and report all diagnostics without modifying source.
    async fn typecheck(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError>;
}

/// Report lint diagnostics and optionally apply the tool's own fixes.
#[async_trait::async_trait]
pub trait LintAdapter: CheckAdapter {
    /// Check selected paths without modifying source.
    async fn lint(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError>;

    /// Apply supported fixes and report diagnostics remaining afterwards.
    async fn fix(
        &self,
        _context: &AdapterContext<'_>,
        _selection: &PathSelection,
        _events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError> {
        Err(AdapterError::Unsupported("lint fix".into()))
    }
}

/// Build selected targets and report diagnostics plus produced artifacts.
#[async_trait::async_trait]
pub trait BuildAdapter: CheckAdapter {
    /// Build selected paths under the supervisor's process ownership.
    async fn build(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut DiagnosticSink,
    ) -> Result<RunExit, AdapterError>;
}

/// Verify formatting and apply formatter-owned source changes.
#[async_trait::async_trait]
pub trait FormatAdapter: CheckAdapter {
    /// Report formatting outcomes without modifying source.
    async fn check(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut FormatSink,
    ) -> Result<RunExit, AdapterError>;

    /// Format the admitted selection and report changed files.
    async fn apply(
        &self,
        context: &AdapterContext<'_>,
        selection: &PathSelection,
        events: &mut FormatSink,
    ) -> Result<RunExit, AdapterError>;
}
