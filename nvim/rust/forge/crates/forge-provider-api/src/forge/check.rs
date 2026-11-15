use serde::{Deserialize, Serialize};

use super::writer::Writer;

/// Report a collected check run against its original exchange and captured source.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckResult {
    /// Check run whose complete logs remain available through check_detail.
    pub run_id: String,
    /// Exchange that requested this run.
    pub exchange_id: String,
    /// Terminal outcome of the collected run.
    pub state: CheckResultState,
    /// Whether this run has finished; false reports a settled failure while other commands remain.
    #[serde(default = "completed_default")]
    pub complete: bool,
    /// Per-command outcomes and captured source identities.
    pub checks: Vec<CheckOutcome>,
}

/// Distinguish check outcomes without interpreting presentation strings.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckResultState {
    /// Every required outcome passed.
    Passed,
    /// Validation reported a failure.
    Failed,
    /// Execution was cancelled and its processes were collected.
    Cancelled,
    /// Configuration, execution, or stale evidence prevented validation.
    Blocked,
    /// This command did not execute.
    Skipped,
}

/// Preserve one selected command's outcome without copying its complete log.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckOutcome {
    /// Saved check definition or command identity.
    pub id: String,
    /// Human-readable check label.
    pub label: String,
    /// Terminal command outcome.
    pub state: CheckResultState,
    /// Source snapshot actually validated, absent when execution captured none.
    pub source_identity: Option<String>,
    /// Whether an earlier result supplied this outcome.
    pub reused: bool,
    /// Process exit code, absent when no process result exists.
    pub exit_code: Option<i32>,
    /// Execution, configuration, or coverage failure.
    pub error: Option<String>,
    /// Test case counts when a test adapter supplied results.
    pub tests: Option<CheckTestCounts>,
    /// Diagnostics omitted by the adapter's existing retention policy.
    pub omitted_diagnostics: usize,
    /// Actionable diagnostics and test failures retained by the check.
    pub diagnostics: Vec<CheckDiagnostic>,
}

/// Carry a compiler diagnostic or test failure as data, never instruction authority.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckDiagnostic {
    /// Runner severity: error, warning, info, or hint.
    pub severity: CheckSeverity,
    /// Diagnostic text supplied by the runner.
    pub message: String,
    /// Repository-relative source path, when available.
    pub path: Option<String>,
    /// One-based line, when available.
    pub line: Option<u32>,
    /// One-based column, when available.
    pub column: Option<u32>,
    /// Runner diagnostic code, when available.
    pub code: Option<String>,
    /// Exact test case identity when this diagnostic describes a failed case.
    pub test_id: Option<String>,
}

/// Classify a retained diagnostic without interpreting its message.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckSeverity {
    /// Validation cannot pass.
    Error,
    /// Validation may pass with a warning.
    Warning,
    /// Additional diagnostic context.
    Info,
    /// Suggested improvement.
    Hint,
}

impl CheckSeverity {
    /// Get the serialized diagnostic severity.
    fn as_str(self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Warning => "warning",
            Self::Info => "info",
            Self::Hint => "hint",
        }
    }
}

/// Count every selected test, including cases without a terminal result.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CheckTestCounts {
    /// Number of selected cases.
    pub total: usize,
    /// Cases reported as passed.
    pub passed: usize,
    /// Failed, timed-out, or errored cases.
    pub failed: usize,
    /// Cases reported as skipped.
    pub skipped: usize,
    /// Cases without a terminal result.
    pub unfinished: usize,
}

impl CheckResult {
    /// Render typed outcomes inside the shared escaped Harness envelope.
    pub(super) fn write(&self, writer: &mut Writer) {
        writer.open(
            "check_result",
            &[
                ("run_id", self.run_id.clone()),
                ("exchange_id", self.exchange_id.clone()),
                ("state", self.state.as_str().into()),
                ("complete", self.complete.to_string()),
            ],
        );
        for check in &self.checks {
            let mut attributes = vec![
                ("id", check.id.clone()),
                ("state", check.state.as_str().into()),
                ("reused", check.reused.to_string()),
            ];
            if let Some(source) = &check.source_identity {
                attributes.push(("source_snapshot", source.clone()));
            }
            if let Some(exit) = check.exit_code {
                attributes.push(("exit_code", exit.to_string()));
            }
            writer.open("check", &attributes);
            writer.element("label", &[], &check.label);
            if let Some(error) = &check.error {
                writer.element("error", &[], error);
            }
            if let Some(tests) = &check.tests {
                writer.element(
                    "tests",
                    &[
                        ("total", tests.total.to_string()),
                        ("passed", tests.passed.to_string()),
                        ("failed", tests.failed.to_string()),
                        ("skipped", tests.skipped.to_string()),
                        ("unfinished", tests.unfinished.to_string()),
                    ],
                    "",
                );
            }
            if check.omitted_diagnostics > 0 {
                writer.element(
                    "omitted_diagnostics",
                    &[],
                    &check.omitted_diagnostics.to_string(),
                );
            }
            for diagnostic in &check.diagnostics {
                let mut attributes = vec![("severity", diagnostic.severity.as_str().into())];
                if let Some(test_id) = &diagnostic.test_id {
                    attributes.push(("test_id", test_id.clone()));
                }
                if let Some(path) = &diagnostic.path {
                    attributes.push(("path", path.clone()));
                }
                if let Some(line) = diagnostic.line {
                    attributes.push(("line", line.to_string()));
                }
                if let Some(column) = diagnostic.column {
                    attributes.push(("column", column.to_string()));
                }
                if let Some(code) = &diagnostic.code {
                    attributes.push(("code", code.clone()));
                }
                writer.element("diagnostic", &attributes, &diagnostic.message);
            }
            writer.close();
        }
        writer.text(&format!(
            "Full diagnostics and logs: check_detail(run_id=\"{}\")",
            self.run_id
        ));
        writer.close();
    }
}

/// Treat records from before partial failure reporting as terminal results.
fn completed_default() -> bool {
    true
}

impl CheckResultState {
    /// Get the stable outcome spelling used in result envelopes.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Passed => "passed",
            Self::Failed => "failed",
            Self::Cancelled => "cancelled",
            Self::Blocked => "blocked",
            Self::Skipped => "skipped",
        }
    }
}
