use super::super::contract::{ITEM_LIMIT, validate_path};
use super::super::{AdapterError, PathSelection, RunExit, TestItemId};
use super::TestPhase;
use super::common::text_retain;
use crate::checks::CheckState;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, BTreeSet},
    num::NonZeroU32,
};

const DIAGNOSTIC_LIMIT: usize = 5_000;
const TEXT_LIMIT: usize = 16 * 1024 * 1024;

/// Identify a diagnostic target independently of test item identities.
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct TargetId(TestItemId);

impl TargetId {
    /// Get the adapter-defined target name for display and transport.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl TryFrom<String> for TargetId {
    type Error = AdapterError;

    /// Validate the target identifier at its input boundary.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        TestItemId::try_from(value).map(Self)
    }
}

/// Restrict diagnostics to the categories that implement this contract.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticCategory {
    TypeCheck,
    Lint,
    Build,
}

/// Classify one completed target without inferring severity from its log.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TargetStatus {
    Passed,
    Failed,
    Skipped,
}

/// Classify a diagnostic independently of the tool's process exit.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Severity {
    Error,
    Warning,
    Info,
    Hint,
}

/// Locate a source point with one-based line and UTF-8 byte column values.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct SourcePosition {
    line: NonZeroU32,
    column: NonZeroU32,
}

impl SourcePosition {
    /// Validate a source position without accepting zero-based coordinates.
    pub fn new(line: u32, column: u32) -> Result<Self, AdapterError> {
        Ok(Self {
            line: NonZeroU32::new(line)
                .ok_or_else(|| AdapterError::Protocol("source line must be one-based".into()))?,
            column: NonZeroU32::new(column)
                .ok_or_else(|| AdapterError::Protocol("source column must be one-based".into()))?,
        })
    }

    /// Get the one-based source line.
    pub fn line(self) -> u32 {
        self.line.get()
    }

    /// Get the one-based UTF-8 byte column.
    pub fn column(self) -> u32 {
        self.column.get()
    }
}

/// Retain an ordered, end-exclusive source range or a single source point.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "RawSourceRange")]
pub struct SourceRange {
    start: SourcePosition,
    end: Option<SourcePosition>,
}

#[derive(Deserialize)]
struct RawSourceRange {
    start: SourcePosition,
    #[serde(default)]
    end: Option<SourcePosition>,
}

impl SourceRange {
    /// Validate source ordering once, including ranges crossing lines.
    pub fn new(start: SourcePosition, end: Option<SourcePosition>) -> Result<Self, AdapterError> {
        if end.is_some_and(|end| end < start) {
            return Err(AdapterError::Protocol(
                "source range ends before it starts".into(),
            ));
        }
        Ok(Self { start, end })
    }

    /// Get the first included source position.
    pub fn start(self) -> SourcePosition {
        self.start
    }

    /// Get the exclusive end, when the diagnostic describes a range.
    pub fn end(self) -> Option<SourcePosition> {
        self.end
    }
}

impl TryFrom<RawSourceRange> for SourceRange {
    type Error = AdapterError;

    /// Apply range validation to loaded wire and history values.
    fn try_from(value: RawSourceRange) -> Result<Self, Self::Error> {
        Self::new(value.start, value.end)
    }
}

/// Record a tool-suggested source edit without applying it.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TextEdit {
    pub path: String,
    pub range: SourceRange,
    pub replacement: String,
}

/// Keep a named set of suggested edits as diagnostic data.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SuggestedFix {
    pub message: String,
    pub edits: Vec<TextEdit>,
}

/// Preserve a tool diagnostic and its optional repository location.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub range: Option<SourceRange>,
    #[serde(default)]
    pub code: Option<String>,
    #[serde(default)]
    pub source: Option<String>,
    #[serde(default)]
    pub target: Option<TargetId>,
    #[serde(default)]
    pub rendered: Option<String>,
    #[serde(default)]
    pub fixes: Vec<SuggestedFix>,
}

/// Describe the typed event stream shared by typecheck, lint, and build.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum DiagnosticEvent {
    Phase {
        name: TestPhase,
        #[serde(default)]
        message: Option<String>,
    },
    Plan {
        #[serde(default)]
        targets: Option<Vec<TargetId>>,
        #[serde(default)]
        widened: Option<String>,
    },
    TargetStarted {
        id: TargetId,
    },
    TargetFinished {
        id: TargetId,
        status: TargetStatus,
        #[serde(default)]
        duration_ms: Option<u64>,
    },
    Diagnostic(Diagnostic),
    Artifact {
        path: String,
        #[serde(default)]
        kind: Option<String>,
    },
    Output {
        text: String,
    },
    Error {
        message: String,
    },
    Complete,
    Unsupported,
    #[serde(other)]
    Unknown,
}

/// Preserve target coverage even when execution ends before a terminal event.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TargetResult {
    pub id: TargetId,
    pub status: Option<TargetStatus>,
    pub duration_ms: Option<u64>,
}

/// Publish observed diagnostic and target counts, without inventing a processed set.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct DiagnosticProgress {
    pub phase: Option<TestPhase>,
    pub targets_total: Option<usize>,
    pub targets_finished: usize,
    pub errors: usize,
    pub warnings: usize,
}

/// Retain structured results, truncation counts, and the actual termination cause.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DiagnosticResult {
    pub selection: PathSelection,
    pub widened: Option<String>,
    pub targets: Vec<TargetResult>,
    pub diagnostics: Vec<Diagnostic>,
    pub truncated: BTreeMap<Severity, usize>,
    pub truncated_bytes: u64,
    pub artifacts: Vec<(String, Option<String>)>,
    pub error: Option<String>,
    pub protocol_error: Option<String>,
    pub exit: Option<i32>,
    pub cancelled: bool,
    pub unsupported: bool,
}

impl DiagnosticResult {
    /// Map independent process, coverage, and severity failures to the existing check states.
    pub fn state(&self) -> CheckState {
        if self.cancelled {
            return CheckState::Interrupted;
        }
        if self.protocol_error.is_some() || self.unsupported {
            return CheckState::ConfigurationError;
        }
        if self.error.is_some()
            || self.exit.is_some_and(|code| code != 0)
            || self.targets.iter().any(|target| {
                target
                    .status
                    .is_none_or(|status| status == TargetStatus::Failed)
            })
            || self
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.severity == Severity::Error)
            || self
                .truncated
                .get(&Severity::Error)
                .copied()
                .unwrap_or_default()
                > 0
        {
            return CheckState::Failed;
        }
        CheckState::Passed
    }
}

/// Validate diagnostic streams and retain bounded results while counting all severities.
pub struct DiagnosticSink {
    category: DiagnosticCategory,
    selection: PathSelection,
    targets: BTreeMap<TargetId, TargetResult>,
    started: BTreeSet<TargetId>,
    declared_targets: bool,
    plan_seen: bool,
    target_seen: bool,
    complete: bool,
    unsupported: bool,
    record_count: usize,
    phase: Option<TestPhase>,
    widened: Option<String>,
    diagnostics: Vec<Diagnostic>,
    counts: BTreeMap<Severity, usize>,
    truncated: BTreeMap<Severity, usize>,
    retained_bytes: usize,
    truncated_bytes: u64,
    artifacts: Vec<(String, Option<String>)>,
    error: Option<String>,
    protocol_error: Option<String>,
    progress_sender: Option<tokio::sync::watch::Sender<DiagnosticProgress>>,
}

impl DiagnosticSink {
    /// Create a sink for one category and normalized repository path selection.
    pub fn new(
        category: DiagnosticCategory,
        mut selection: PathSelection,
    ) -> Result<Self, AdapterError> {
        selection.normalize()?;
        Ok(Self {
            category,
            selection,
            targets: BTreeMap::new(),
            started: BTreeSet::new(),
            declared_targets: false,
            plan_seen: false,
            target_seen: false,
            complete: false,
            unsupported: false,
            record_count: 0,
            phase: None,
            widened: None,
            diagnostics: Vec::new(),
            counts: BTreeMap::new(),
            truncated: BTreeMap::new(),
            retained_bytes: 0,
            truncated_bytes: 0,
            artifacts: Vec::new(),
            error: None,
            protocol_error: None,
            progress_sender: None,
        })
    }

    /// Validate an event and latch failures before publishing resulting progress.
    pub fn accept(&mut self, event: DiagnosticEvent) -> Result<(), AdapterError> {
        let result = self.event_accept(event);
        if let Err(error) = &result {
            self.protocol_error.get_or_insert_with(|| error.to_string());
        }
        if let Some(sender) = &self.progress_sender {
            sender.send_replace(self.progress());
        }
        result
    }

    /// Decode a complete JSON record through the shared bounded transport parser.
    pub fn accept_line(&mut self, line: &str) -> Result<(), AdapterError> {
        match super::common::decode(line.as_bytes()) {
            Ok(event) => self.accept(event),
            Err(error) => {
                self.failure(&error);
                Err(error)
            }
        }
    }

    /// Subscribe to accepted progress without exposing mutable sink state.
    pub fn progress_subscribe(&mut self) -> tokio::sync::watch::Receiver<DiagnosticProgress> {
        let (sender, receiver) = tokio::sync::watch::channel(self.progress());
        self.progress_sender = Some(sender);
        receiver
    }

    /// Get exact counts, including diagnostics omitted from retained history.
    pub fn progress(&self) -> DiagnosticProgress {
        DiagnosticProgress {
            phase: self.phase,
            targets_total: self.declared_targets.then_some(self.targets.len()),
            targets_finished: self
                .targets
                .values()
                .filter(|target| target.status.is_some())
                .count(),
            errors: self
                .counts
                .get(&Severity::Error)
                .copied()
                .unwrap_or_default(),
            warnings: self
                .counts
                .get(&Severity::Warning)
                .copied()
                .unwrap_or_default(),
        }
    }

    /// Retain an owned process or protocol failure without discarding earlier diagnostics.
    pub fn failure(&mut self, error: &AdapterError) {
        match error {
            AdapterError::Protocol(message) => {
                self.protocol_error.get_or_insert_with(|| message.clone());
            }
            AdapterError::Unsupported(_) => {
                let _ = self.accept(DiagnosticEvent::Unsupported);
            }
            AdapterError::Cancelled => {}
            AdapterError::Launch(_)
            | AdapterError::ToolLaunch { .. }
            | AdapterError::Command { .. }
            | AdapterError::Selection(_)
            | AdapterError::Storage(_)
            | AdapterError::Process(_)
            | AdapterError::Timeout => {
                self.error.get_or_insert_with(|| error.to_string());
            }
        }
    }

    /// Reconcile completion and exit independently from diagnostic severity and target coverage.
    pub fn finish(mut self, exit: Option<RunExit>, cancelled: bool) -> DiagnosticResult {
        if !cancelled && !self.unsupported {
            if !self.complete && self.error.is_none() {
                self.protocol_error
                    .get_or_insert_with(|| "diagnostic stream is missing complete".into());
            }
            match exit {
                Some(RunExit {
                    success: false,
                    code,
                }) if self.error.is_none()
                    && self
                        .counts
                        .get(&Severity::Error)
                        .copied()
                        .unwrap_or_default()
                        == 0 =>
                {
                    self.error = Some(format!(
                        "the tool exited with status {code:?} without reporting an error"
                    ));
                }
                None if self.error.is_none() => {
                    self.error = Some("diagnostic process did not return an exit status".into());
                }
                Some(_) | None => {}
            }
        }
        DiagnosticResult {
            selection: self.selection,
            widened: self.widened,
            targets: self.targets.into_values().collect(),
            diagnostics: self.diagnostics,
            truncated: self.truncated,
            truncated_bytes: self.truncated_bytes,
            artifacts: self.artifacts,
            error: self.error,
            protocol_error: self.protocol_error,
            exit: exit.and_then(|exit| exit.code),
            cancelled,
            unsupported: self.unsupported,
        }
    }

    /// Enforce ordering and validate each record before retaining its effect.
    fn event_accept(&mut self, event: DiagnosticEvent) -> Result<(), AdapterError> {
        if self.complete || self.unsupported || self.protocol_error.is_some() {
            return Err(AdapterError::Protocol(
                "event received after terminal record or protocol failure".into(),
            ));
        }
        self.record_count += 1;
        match event {
            DiagnosticEvent::Unknown | DiagnosticEvent::Output { .. } => {}
            DiagnosticEvent::Phase { name, .. } => self.phase = Some(name),
            DiagnosticEvent::Plan { targets, widened } => {
                if self.plan_seen || self.target_seen {
                    return Err(AdapterError::Protocol(
                        "duplicate or late diagnostic plan".into(),
                    ));
                }
                let mut declared = BTreeMap::new();
                if let Some(targets) = &targets {
                    if targets.len() > ITEM_LIMIT {
                        return Err(AdapterError::Protocol(
                            "diagnostic target limit exceeded".into(),
                        ));
                    }
                    for id in targets {
                        if declared
                            .insert(
                                id.clone(),
                                TargetResult {
                                    id: id.clone(),
                                    status: None,
                                    duration_ms: None,
                                },
                            )
                            .is_some()
                        {
                            return Err(AdapterError::Protocol(
                                "duplicate planned diagnostic target".into(),
                            ));
                        }
                    }
                }
                self.targets = declared;
                self.declared_targets = targets.is_some();
                self.plan_seen = true;
                self.widened = widened;
            }
            DiagnosticEvent::TargetStarted { id } => {
                let target = self.target_get(&id)?;
                if target.status.is_some() || !self.started.insert(id) {
                    return Err(AdapterError::Protocol(
                        "duplicate or late target start".into(),
                    ));
                }
                self.target_seen = true;
            }
            DiagnosticEvent::TargetFinished {
                id,
                status,
                duration_ms,
            } => {
                let target = self.target_get(&id)?;
                if target.status.is_some() {
                    return Err(AdapterError::Protocol("duplicate target finish".into()));
                }
                target.status = Some(status);
                target.duration_ms = duration_ms;
                self.target_seen = true;
            }
            DiagnosticEvent::Diagnostic(mut diagnostic) => {
                diagnostic_validate(&diagnostic)?;
                if self.declared_targets
                    && diagnostic
                        .target
                        .as_ref()
                        .is_some_and(|id| !self.targets.contains_key(id))
                {
                    return Err(AdapterError::Protocol(
                        "diagnostic references an unplanned target".into(),
                    ));
                }
                *self.counts.entry(diagnostic.severity).or_default() += 1;
                let keep = self.diagnostics.len() < DIAGNOSTIC_LIMIT;
                let mut remaining = if keep {
                    TEXT_LIMIT.saturating_sub(self.retained_bytes)
                } else {
                    0
                };
                let before = remaining;
                self.truncated_bytes += diagnostic_text_retain(&mut diagnostic, &mut remaining);
                self.retained_bytes += before - remaining;
                if keep {
                    self.diagnostics.push(diagnostic);
                } else {
                    *self.truncated.entry(diagnostic.severity).or_default() += 1;
                }
            }
            DiagnosticEvent::Artifact { path, kind } => {
                if self.category != DiagnosticCategory::Build {
                    return Err(AdapterError::Protocol(
                        "artifacts require the build category".into(),
                    ));
                }
                validate_path(&path)?;
                if self.artifacts.len() >= ITEM_LIMIT {
                    return Err(AdapterError::Protocol(
                        "build artifact limit exceeded".into(),
                    ));
                }
                self.artifacts.push((path, kind));
            }
            DiagnosticEvent::Error { message } => {
                self.error.get_or_insert(message);
            }
            DiagnosticEvent::Complete => self.complete = true,
            DiagnosticEvent::Unsupported => {
                if self.record_count != 1 {
                    return Err(AdapterError::Protocol(
                        "unsupported must be the only event".into(),
                    ));
                }
                self.unsupported = true;
            }
        }
        Ok(())
    }

    /// Admit an observed target only when it agrees with a declared target set.
    fn target_get(&mut self, id: &TargetId) -> Result<&mut TargetResult, AdapterError> {
        if !self.targets.contains_key(id) {
            if self.declared_targets {
                return Err(AdapterError::Protocol(format!(
                    "unplanned diagnostic target: {}",
                    id.as_str()
                )));
            }
            if self.targets.len() >= ITEM_LIMIT {
                return Err(AdapterError::Protocol(
                    "diagnostic target limit exceeded".into(),
                ));
            }
        }
        Ok(self
            .targets
            .entry(id.clone())
            .or_insert_with(|| TargetResult {
                id: id.clone(),
                status: None,
                duration_ms: None,
            }))
    }
}

/// Validate location ownership before truncation can remove diagnostic text.
fn diagnostic_validate(diagnostic: &Diagnostic) -> Result<(), AdapterError> {
    if let Some(path) = &diagnostic.path {
        validate_path(path)?;
    }
    if diagnostic.range.is_some() && diagnostic.path.is_none() {
        return Err(AdapterError::Protocol(
            "diagnostic range requires a repository path".into(),
        ));
    }
    for fix in &diagnostic.fixes {
        for edit in &fix.edits {
            validate_path(&edit.path)?;
        }
    }
    Ok(())
}

/// Bound retained text on UTF-8 boundaries while retaining severity and location metadata.
fn diagnostic_text_retain(diagnostic: &mut Diagnostic, remaining: &mut usize) -> u64 {
    let mut omitted = text_retain(&mut diagnostic.message, remaining);
    for text in [
        &mut diagnostic.code,
        &mut diagnostic.source,
        &mut diagnostic.rendered,
    ]
    .into_iter()
    .flatten()
    {
        omitted += text_retain(text, remaining);
    }
    for fix in &mut diagnostic.fixes {
        omitted += text_retain(&mut fix.message, remaining);
        for edit in &mut fix.edits {
            omitted += text_retain(&mut edit.replacement, remaining);
        }
    }
    omitted
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Supply a diagnostic payload through the same decoder used for script output.
    fn diagnostic(severity: Severity, message: String) -> Diagnostic {
        serde_json::from_value(serde_json::json!({"severity":severity,"message":message})).unwrap()
    }

    /// Reconcile fixture records while preserving the first protocol failure.
    fn result(lines: &[&str], code: i32) -> DiagnosticResult {
        let mut sink =
            DiagnosticSink::new(DiagnosticCategory::TypeCheck, PathSelection::default()).unwrap();
        for line in lines {
            let _ = sink.accept_line(line);
        }
        sink.finish(
            Some(RunExit {
                code: Some(code),
                success: code == 0,
            }),
            false,
        )
    }

    /// Independent severity, process, and coverage failures all prevent a passing result.
    #[test]
    fn completion_and_failures_follow_the_diagnostic_contract() {
        let complete = r#"{"event":"complete"}"#;
        let warning = r#"{"event":"diagnostic","severity":"warning","message":"warning"}"#;
        let error = r#"{"event":"diagnostic","severity":"error","message":"type mismatch"}"#;
        let target = r#"{"event":"plan","targets":["crate"]}"#;
        for (lines, code, expected) in [
            (vec![complete], 0, CheckState::Passed),
            (vec![warning, complete], 0, CheckState::Passed),
            (vec![error, complete], 0, CheckState::Failed),
            (vec![warning, complete], 7, CheckState::Failed),
            (vec![error, complete], 7, CheckState::Failed),
            (vec![], 0, CheckState::ConfigurationError),
            (vec![warning], 0, CheckState::ConfigurationError),
            (vec![target, complete], 0, CheckState::Failed),
            (
                vec![r#"{"event":"target_started","id":"crate"}"#, complete],
                0,
                CheckState::Failed,
            ),
            (
                vec![r#"{"event":"error","message":"compiler unavailable"}"#],
                1,
                CheckState::Failed,
            ),
        ] {
            assert_eq!(result(&lines, code).state(), expected, "{lines:?}");
        }
        assert!(
            result(&[warning, complete], 7)
                .error
                .unwrap()
                .contains("without reporting an error")
        );
        let run = result(&[target, complete], 0);
        assert_eq!(run.targets.len(), 1);
        assert_eq!(run.targets[0].status, None);
    }

    /// Reject illegal event transitions even if the caller ignores an earlier parser error.
    #[test]
    fn malformed_order_and_invalid_locations_are_latched() {
        let complete = r#"{"event":"complete"}"#;
        let plan = r#"{"event":"plan","targets":["crate"]}"#;
        let started = r#"{"event":"target_started","id":"crate"}"#;
        let finished = r#"{"event":"target_finished","id":"crate","status":"passed"}"#;
        for lines in [
            vec![plan, plan],
            vec![started, plan],
            vec![plan, r#"{"event":"target_started","id":"other"}"#],
            vec![started, started],
            vec![finished, finished],
            vec![finished, started],
            vec![complete, complete],
            vec![complete, r#"{"event":"future"}"#],
            vec![
                r#"{"event":"phase","name":"build"}"#,
                r#"{"event":"unsupported"}"#,
            ],
            vec![
                r#"{"event":"diagnostic","severity":"error","message":"x","path":"../escape.rs"}"#,
            ],
            vec![r#"{"event":"diagnostic","severity":"error","message":"x","path":"/outside.rs"}"#],
            vec![
                r#"{"event":"diagnostic","severity":"error","message":"x","path":"src/x.rs","range":{"start":{"line":0,"column":1}}}"#,
            ],
            vec![
                r#"{"event":"diagnostic","severity":"error","message":"x","range":{"start":{"line":1,"column":1}}}"#,
            ],
            vec![
                r#"{"event":"diagnostic","severity":"error","message":"x","path":"src/x.rs","range":{"start":{"line":2,"column":1},"end":{"line":1,"column":2}}}"#,
            ],
            vec![
                r#"{"event":"diagnostic","severity":"error","message":"x","fixes":[{"message":"fix","edits":[{"path":"../escape","range":{"start":{"line":1,"column":1}},"replacement":"x"}]}]}"#,
            ],
            vec![r#"{"event":"plan","targets":["crate","crate"]}"#],
        ] {
            let mut records = lines.clone();
            records.push(complete);
            let run = result(&records, 0);
            assert_eq!(run.state(), CheckState::ConfigurationError, "{lines:?}");
            assert!(run.protocol_error.is_some());
        }
        assert!(serde_json::from_str::<SourcePosition>(r#"{"line":1,"column":0}"#).is_err());
        let range: SourceRange =
            serde_json::from_str(r#"{"start":{"line":1,"column":3},"end":{"line":2,"column":1}}"#)
                .unwrap();
        assert_eq!(range.start().column(), 3);
        assert_eq!(range.end().unwrap().line(), 2);
    }

    /// Preserve exact severity counts and failure status after both retention limits.
    #[test]
    fn truncation_never_hides_error_diagnostics() {
        let mut sink =
            DiagnosticSink::new(DiagnosticCategory::Lint, PathSelection::default()).unwrap();
        for _ in 0..DIAGNOSTIC_LIMIT {
            sink.accept(DiagnosticEvent::Diagnostic(diagnostic(
                Severity::Warning,
                "warning".into(),
            )))
            .unwrap();
        }
        sink.accept(DiagnosticEvent::Diagnostic(diagnostic(
            Severity::Error,
            "hidden error".into(),
        )))
        .unwrap();
        assert_eq!(sink.progress().errors, 1);
        assert_eq!(sink.progress().warnings, DIAGNOSTIC_LIMIT);
        sink.accept(DiagnosticEvent::Complete).unwrap();
        let run = sink.finish(
            Some(RunExit {
                code: Some(0),
                success: true,
            }),
            false,
        );
        assert_eq!(run.diagnostics.len(), DIAGNOSTIC_LIMIT);
        assert_eq!(run.truncated[&Severity::Error], 1);
        assert_eq!(run.state(), CheckState::Failed);

        let mut sink =
            DiagnosticSink::new(DiagnosticCategory::Lint, PathSelection::default()).unwrap();
        for _ in 0..3 {
            sink.accept(DiagnosticEvent::Diagnostic(diagnostic(
                Severity::Error,
                "é".repeat(3 * 1024 * 1024),
            )))
            .unwrap();
        }
        sink.accept(DiagnosticEvent::Complete).unwrap();
        let run = sink.finish(
            Some(RunExit {
                code: Some(0),
                success: true,
            }),
            false,
        );
        assert_eq!(
            run.diagnostics
                .iter()
                .map(|item| item.message.len())
                .sum::<usize>(),
            TEXT_LIMIT
        );
        assert_eq!(run.truncated_bytes, 2 * 1024 * 1024);
        assert_eq!(run.state(), CheckState::Failed);
    }

    /// Reject returned unsupported errors after an adapter has already emitted events.
    #[test]
    fn returned_unsupported_cannot_hide_prior_diagnostic_events() {
        let mut sink =
            DiagnosticSink::new(DiagnosticCategory::Lint, PathSelection::default()).unwrap();
        sink.accept_line(r#"{"event":"output","text":"already working"}"#)
            .unwrap();
        sink.failure(&AdapterError::Unsupported("fix".into()));
        assert!(sink.finish(None, false).protocol_error.is_some());
    }

    /// Keep cancellation and timeout causes while preserving already reported work.
    #[test]
    fn progress_and_interrupted_results_preserve_observations() {
        let mut sink =
            DiagnosticSink::new(DiagnosticCategory::Build, PathSelection::default()).unwrap();
        let progress = sink.progress_subscribe();
        for line in [
            r#"{"event":"phase","name":"build"}"#,
            r#"{"event":"plan","targets":["crate","next"]}"#,
            r#"{"event":"target_finished","id":"crate","status":"passed","duration_ms":5}"#,
            r#"{"event":"diagnostic","severity":"warning","message":"careful"}"#,
            r#"{"event":"artifact","path":"target/program","kind":"binary"}"#,
        ] {
            sink.accept_line(line).unwrap();
        }
        assert_eq!(progress.borrow().targets_total, Some(2));
        assert_eq!(progress.borrow().targets_finished, 1);
        assert_eq!(progress.borrow().warnings, 1);
        let run = sink.finish(None, true);
        assert_eq!(run.state(), CheckState::Interrupted);
        assert_eq!(run.diagnostics.len(), 1);
        assert_eq!(run.targets[0].duration_ms, Some(5));
        assert_eq!(run.artifacts.len(), 1);
        let mut sink =
            DiagnosticSink::new(DiagnosticCategory::TypeCheck, PathSelection::default()).unwrap();
        sink.failure(&AdapterError::Timeout);
        let run = sink.finish(None, false);
        assert_eq!(run.state(), CheckState::Failed);
        assert!(run.error.unwrap().contains("deadline"));
        assert_eq!(
            result(&[r#"{"event":"artifact","path":"target/program"}"#], 0).state(),
            CheckState::ConfigurationError
        );
    }
}
