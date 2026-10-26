use super::{CheckCategory, CheckState, CommandCheck};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::io::{BufRead, BufReader};

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Retains runner-reported file outcomes separately from the enclosing check result.
pub struct CheckReport {
    /// Completed fixture executions in the runner's original order.
    pub result: Vec<CheckResult>,
    /// Report decoding failure, with the original command output retained for inspection.
    pub error: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Describes one reported fixture execution without inferring individual assertions.
pub struct CheckResult {
    /// Repository-relative source file reported by the runner.
    pub file: String,
    /// Recorded process outcome for this fixture.
    pub state: CheckState,
    /// Fixture duration when the runner supplies a recognized duration.
    pub duration_ms: Option<u64>,
    /// Actual fixture command retained for expanded details.
    pub command: String,
    /// Complete captured fixture diagnostics, separate from runner metadata.
    pub output: String,
}

#[derive(Deserialize)]
struct RunnerHeader {
    suite: String,
    fixtures: Vec<String>,
}

#[derive(Deserialize)]
struct FixtureRecord {
    test: String,
    command: Vec<String>,
    elapsed: String,
    exit_status: i32,
    stdout: String,
    stderr: String,
}

#[derive(Deserialize)]
struct RunnerSummary {
    passed: usize,
    failed: usize,
}

/// Collects the existing Lua runner's file reports without changing command success or raw logs.
pub(super) fn collect(command: &CommandCheck) -> Option<CheckReport> {
    let context = command.context.as_ref()?;
    if context.category != CheckCategory::Test
        || !command
            .command
            .replace('\\', "/")
            .contains(".forge/checks/lua-tests.nu")
        || command.started_at_ms.is_none()
        || matches!(command.state, CheckState::Pending | CheckState::Running)
    {
        return None;
    }
    Some(
        match std::fs::File::open(&command.stdout)
            .with_context(|| format!("open fixture report {}", command.stdout.display()))
            .and_then(|file| parse(BufReader::new(file)))
        {
            Ok(result) => CheckReport {
                result,
                error: None,
            },
            Err(error) => CheckReport {
                result: Vec::new(),
                error: Some(format!("{error:#}")),
            },
        },
    )
}

/// Validates all reported files and totals before publishing any fixture outcomes.
fn parse(reader: impl BufRead) -> Result<Vec<CheckResult>> {
    let mut lines = reader.lines();
    let header: RunnerHeader =
        serde_json::from_str(&lines.next().context("fixture report is empty")??)
            .context("decode fixture report header")?;
    ensure!(!header.suite.is_empty(), "fixture report has no suite");
    let mut expected = BTreeSet::new();
    for file in header.fixtures {
        let file = file.replace('\\', "/");
        super::config::relative_path(&file)?;
        ensure!(
            expected.insert(file),
            "fixture report repeats a selected file"
        );
    }
    let mut result = Vec::new();
    let mut summary = None;
    for line in lines {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        ensure!(
            summary.is_none(),
            "fixture report continues after its totals"
        );
        let value: serde_json::Value =
            serde_json::from_str(&line).context("decode fixture result")?;
        if value.get("test").is_none() {
            summary = Some(
                serde_json::from_value::<RunnerSummary>(value).context("decode fixture totals")?,
            );
            continue;
        }
        let record: FixtureRecord =
            serde_json::from_value(value).context("decode fixture fields")?;
        let file = record.test.replace('\\', "/");
        ensure!(
            expected.remove(&file),
            "fixture report contains an unexpected or repeated file: {file}"
        );
        ensure!(!record.command.is_empty(), "fixture command is missing");
        let mut output = record.stdout;
        if !output.is_empty() && !record.stderr.is_empty() && !output.ends_with('\n') {
            output.push('\n');
        }
        output.push_str(&record.stderr);
        result.push(CheckResult {
            file,
            state: if record.exit_status == 0 {
                CheckState::Passed
            } else {
                CheckState::Failed
            },
            duration_ms: duration_ms(&record.elapsed),
            command: super::invocation::display(&record.command[0], &record.command[1..]),
            output,
        });
    }
    let summary = summary.context("fixture report has no totals")?;
    let passed = result
        .iter()
        .filter(|result| result.state == CheckState::Passed)
        .count();
    ensure!(
        expected.is_empty(),
        "fixture report is missing selected files"
    );
    ensure!(
        summary.passed == passed && summary.failed == result.len() - passed,
        "fixture totals disagree with the reported outcomes"
    );
    Ok(result)
}

/// Converts the runner's compound duration into milliseconds without accepting non-finite values.
fn duration_ms(text: &str) -> Option<u64> {
    let mut total = 0.0;
    for part in text.split_whitespace() {
        let boundary =
            part.find(|character: char| !character.is_ascii_digit() && character != '.')?;
        let value: f64 = part[..boundary].parse().ok()?;
        let scale = match &part[boundary..] {
            "ns" => 0.000001,
            "µs" | "us" => 0.001,
            "ms" => 1.0,
            "sec" => 1000.0,
            "min" => 60_000.0,
            "hr" => 3_600_000.0,
            "day" => 86_400_000.0,
            _ => return None,
        };
        total += value * scale;
    }
    (total.is_finite() && total >= 0.0 && total <= u64::MAX as f64 && !text.is_empty())
        .then_some(total.round() as u64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_reports_retain_outcomes_diagnostics_and_complete_content() -> Result<()> {
        let diagnostic = "assertion failed\n".repeat(100_000);
        let source = [
            serde_json::json!({"suite":"affected","fixtures":["tests/pass.lua","tests/fail.lua"]}),
            serde_json::json!({"test":"tests/pass.lua","command":["nvim","-l","tests/pass.lua"],
                "elapsed":"2sec 273ms 682µs 800ns","exit_status":0,"stdout":"","stderr":"passed"}),
            serde_json::json!({"test":"tests/fail.lua","command":["nvim","-l","tests/fail.lua"],
                "elapsed":"102ms","exit_status":1,"stdout":"context","stderr":diagnostic}),
            serde_json::json!({"passed":1,"failed":1}),
        ]
        .into_iter()
        .map(|value| value.to_string())
        .collect::<Vec<_>>()
        .join("\n");
        let result = parse(std::io::Cursor::new(source))?;
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].duration_ms, Some(2274));
        assert_eq!(result[0].state, CheckState::Passed);
        assert_eq!(result[1].state, CheckState::Failed);
        assert_eq!(result[1].output, format!("context\n{diagnostic}"));
        Ok(())
    }

    #[test]
    fn incomplete_or_inconsistent_reports_never_publish_partial_success() {
        let header = r#"{"suite":"affected","fixtures":["tests/pass.lua"]}"#;
        assert!(parse(std::io::Cursor::new(header)).is_err());
        assert!(
            parse(std::io::Cursor::new(format!(
                "{header}\n{{\"passed\":1,\"failed\":0}}"
            )))
            .is_err()
        );
        assert!(parse(std::io::Cursor::new("not JSON")).is_err());
        assert_eq!(duration_ms("NaNsec"), None);
    }
}
