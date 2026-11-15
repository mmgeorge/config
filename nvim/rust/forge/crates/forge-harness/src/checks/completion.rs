use super::adapter::events::{CaseStatus, Severity};
use super::{CheckRun, CheckState};
use forge_provider_api::forge::{
    CheckDiagnostic, CheckOutcome, CheckResult, CheckResultState, CheckSeverity, CheckTestCounts,
};

/// Project collected evidence without claiming it validates a later workspace snapshot.
pub(crate) fn result(run: &CheckRun, exchange_id: &str) -> CheckResult {
    CheckResult {
        run_id: run.id.clone(),
        exchange_id: exchange_id.into(),
        state: state(run.state),
        complete: run.completed_at_ms.is_some(),
        checks: run
            .commands
            .iter()
            .filter(|command| command.selected)
            .map(|command| {
                let mut diagnostics = Vec::new();
                let mut tests = None;
                let mut omitted_diagnostics = 0;
                let mut error = command.error.clone();
                match &command.result {
                    Some(super::adapter::AdapterResult::Diagnostics(result)) => {
                        error = error
                            .or_else(|| result.error.clone())
                            .or_else(|| result.protocol_error.clone());
                        omitted_diagnostics = result.truncated.values().sum();
                        diagnostics.extend(result.diagnostics.iter().map(|diagnostic| {
                            CheckDiagnostic {
                                severity: match diagnostic.severity {
                                    Severity::Error => CheckSeverity::Error,
                                    Severity::Warning => CheckSeverity::Warning,
                                    Severity::Info => CheckSeverity::Info,
                                    Severity::Hint => CheckSeverity::Hint,
                                },
                                message: diagnostic.message.clone(),
                                path: diagnostic.path.clone(),
                                line: diagnostic.range.map(|range| range.start().line()),
                                column: diagnostic.range.map(|range| range.start().column()),
                                code: diagnostic.code.clone(),
                                test_id: None,
                            }
                        }));
                    }
                    Some(super::adapter::AdapterResult::Tests(result)) => {
                        error = error
                            .or_else(|| result.error.clone())
                            .or_else(|| result.protocol_error.clone());
                        let mut counts = CheckTestCounts {
                            total: result.cases.len(),
                            passed: 0,
                            failed: 0,
                            skipped: 0,
                            unfinished: 0,
                        };
                        for case in &result.cases {
                            match case.status {
                                Some(CaseStatus::Passed) => counts.passed += 1,
                                Some(CaseStatus::Skipped) => counts.skipped += 1,
                                Some(
                                    CaseStatus::Failed | CaseStatus::TimedOut | CaseStatus::Errored,
                                ) => counts.failed += 1,
                                None => counts.unfinished += 1,
                            }
                            diagnostics.extend(case.failures.iter().map(|failure| {
                                CheckDiagnostic {
                                    severity: CheckSeverity::Error,
                                    message: failure.message.clone(),
                                    path: failure.path.clone(),
                                    line: failure.line,
                                    column: None,
                                    code: None,
                                    test_id: Some(case.id.as_str().to_owned()),
                                }
                            }));
                        }
                        tests = Some(counts);
                    }
                    Some(super::adapter::AdapterResult::Format(result)) => {
                        error = error
                            .or_else(|| result.error.clone())
                            .or_else(|| result.protocol_error.clone());
                        diagnostics.extend(
                            result
                                .files
                                .iter()
                                .filter(|file| {
                                    matches!(
                                        file.status,
                                        None | Some(
                                            super::adapter::events::FormatStatus::Unformatted
                                                | super::adapter::events::FormatStatus::Failed
                                        )
                                    )
                                })
                                .map(|file| CheckDiagnostic {
                                    severity: CheckSeverity::Error,
                                    message: file.message.clone().unwrap_or_else(|| {
                                        "File was not successfully formatted".into()
                                    }),
                                    path: Some(file.path.clone()),
                                    line: None,
                                    column: None,
                                    code: None,
                                    test_id: None,
                                }),
                        );
                    }
                    None => {}
                }
                CheckOutcome {
                    id: command.context.as_ref().map_or_else(
                        || command.id.clone(),
                        |context| context.definition_id.clone(),
                    ),
                    label: command
                        .context
                        .as_ref()
                        .map_or_else(|| command.command.clone(), |context| context.label()),
                    state: state(command.state),
                    source_identity: command
                        .evidence
                        .as_ref()
                        .map(|evidence| evidence.source_identity.clone()),
                    reused: command.reused_from.is_some(),
                    exit_code: command.exit_code,
                    error,
                    tests,
                    omitted_diagnostics,
                    diagnostics,
                }
            })
            .collect(),
    }
}

/// Map execution states to terminal result vocabulary.
fn state(state: CheckState) -> CheckResultState {
    match state {
        CheckState::Passed => CheckResultState::Passed,
        CheckState::Failed => CheckResultState::Failed,
        CheckState::Interrupted | CheckState::Stopped => CheckResultState::Cancelled,
        CheckState::Skipped => CheckResultState::Skipped,
        CheckState::Pending
        | CheckState::Running
        | CheckState::Blocked
        | CheckState::ConfigurationError
        | CheckState::ExecutionError
        | CheckState::Stale => CheckResultState::Blocked,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Keep failure locations, severities, incomplete cases, and reused identity in the notification.
    #[test]
    fn completion_preserves_diagnostics_and_test_coverage() {
        let run: CheckRun = serde_json::from_value(json!({
            "id":"run","owner":{"kind":"session","session_id":"session"},"revision":0,
            "shell":"system","executable":null,"workspace":"fixture","environment_digest":"fixture",
            "started_at_ms":1,"completed_at_ms":2,"state":"failed",
            "progress":crate::plan::execution::SemanticProgress::default(),"workspace_identity":null,"workspace_digest":{},
            "commands":[
                {"id":"diagnostics","command":"compiler","selected":true,"state":"failed",
                 "started_at_ms":1,"completed_at_ms":2,"exit_code":1,"error":null,
                 "stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
                 "reused_from":["earlier","diagnostics"],
                 "result":{"category":"diagnostics","selection":{"paths":[]},"widened":null,"targets":[],
                   "diagnostics":[{"severity":"warning","message":"warning text","path":"src/lib.rs",
                     "range":{"start":{"line":3,"column":4},"end":{"line":3,"column":5}},"code":"W1","source":null,"target":null,"rendered":null,"fixes":[]}],
                   "truncated":{"error":2},"truncated_bytes":0,"artifacts":[],"error":null,
                   "protocol_error":"missing completion","exit":1,"cancelled":false,"unsupported":false}},
                {"id":"tests","command":"tests","selected":true,"state":"failed",
                 "started_at_ms":1,"completed_at_ms":2,"exit_code":1,"error":null,
                 "stdout":"unused","stderr":"unused","output":"unused","output_bytes":0,"preview":"",
                 "result":{"category":"tests","selection":{"ids":[],"affected":[]},"items":[],"widened":null,
                   "cases":[
                     {"id":"passed","status":"passed","duration_ms":1,"failures":[],"item":null,"output":null,"output_truncated_bytes":0},
                     {"id":"failed","status":"failed","duration_ms":1,"failures":[{"message":"assertion failed","path":"src/test.rs","line":9}],"item":null,"output":null,"output_truncated_bytes":0},
                     {"id":"missing","status":null,"duration_ms":null,"failures":[],"item":null,"output":null,"output_truncated_bytes":0}],
                   "not_found":[],"error":null,"protocol_error":null,"exit":1,"cancelled":false,"unsupported":false,"output_truncated_bytes":0}}
            ]
        })).expect("valid check fixture");
        let result = result(&run, "exchange");
        assert_eq!(result.state, CheckResultState::Failed);
        let compiler = &result.checks[0];
        assert!(compiler.reused);
        assert_eq!(compiler.source_identity, None);
        assert_eq!(compiler.omitted_diagnostics, 2);
        assert_eq!(compiler.error.as_deref(), Some("missing completion"));
        assert_eq!(compiler.diagnostics[0].severity, CheckSeverity::Warning);
        assert_eq!(compiler.diagnostics[0].line, Some(3));
        assert_eq!(compiler.diagnostics[0].column, Some(4));
        let tests = &result.checks[1];
        assert_eq!(
            tests.tests,
            Some(CheckTestCounts {
                total: 3,
                passed: 1,
                failed: 1,
                skipped: 0,
                unfinished: 1
            })
        );
        assert_eq!(tests.diagnostics[0].test_id.as_deref(), Some("failed"));
        assert_eq!(tests.diagnostics[0].path.as_deref(), Some("src/test.rs"));
        assert_eq!(tests.diagnostics[0].line, Some(9));
    }
}
