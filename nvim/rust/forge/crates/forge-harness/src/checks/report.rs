use super::{CheckState, CommandCheck};
use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct CheckReport {
    pub result: Vec<CheckResult>,
    pub error: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CheckResult {
    pub file: String,
    pub state: CheckResultState,
    pub duration_ms: Option<u64>,
    pub command: String,
    pub output: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CheckResultState {
    Passed,
    Failed,
    Skipped,
    Interrupted,
    Blocked,
    ExecutionError,
}

impl From<CheckResultState> for CheckState {
    fn from(state: CheckResultState) -> Self {
        match state {
            CheckResultState::Passed => Self::Passed,
            CheckResultState::Failed => Self::Failed,
            CheckResultState::Skipped => Self::Skipped,
            CheckResultState::Interrupted => Self::Interrupted,
            CheckResultState::Blocked => Self::Blocked,
            CheckResultState::ExecutionError => Self::ExecutionError,
        }
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReportDocument {
    version: u32,
    result: Vec<CheckResult>,
}

pub(super) fn collect(command: &CommandCheck) -> Option<CheckReport> {
    let context = command.context.as_ref()?;
    if !context
        .operation
        .as_ref()
        .is_some_and(|operation| operation.input)
        || command.started_at_ms.is_none()
        || matches!(command.state, CheckState::Pending | CheckState::Running)
    {
        return None;
    }
    let path = context.receipt.with_extension("report.json");
    let file = match std::fs::File::open(&path) {
        Ok(file) => file,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return None,
        Err(error) => {
            return Some(CheckReport {
                result: Vec::new(),
                error: Some(format!("read check report {}: {error}", path.display())),
            });
        }
    };
    Some(match parse(std::io::BufReader::new(file)) {
        Ok(result) => CheckReport {
            result,
            error: None,
        },
        Err(error) => CheckReport {
            result: Vec::new(),
            error: Some(format!("{error:#}")),
        },
    })
}

fn parse(reader: impl std::io::Read) -> Result<Vec<CheckResult>> {
    let document: ReportDocument =
        serde_json::from_reader(reader).context("decode check report")?;
    ensure!(document.version == 1, "unsupported check report version");
    for result in &document.result {
        super::config::relative_path(&result.file)?;
        ensure!(
            !result.command.trim().is_empty(),
            "reported command is empty"
        );
    }
    Ok(document.result)
}
