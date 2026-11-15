use super::super::contract::{ITEM_LIMIT, validate_path, validate_tree};
use super::super::{AdapterError, RunExit, TestItem, TestItemId, TestSelection};
use crate::checks::CheckState;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
    path::{Path, PathBuf},
};

const OUTPUT_LIMIT: usize = 16 * 1024 * 1024;
const CASE_OUTPUT_LIMIT: usize = 1024 * 1024;

/// Describe a test run event independently of its originating tool.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum TestEvent {
    Phase {
        name: TestPhase,
        #[serde(default)]
        message: Option<String>,
    },
    Plan {
        cases: Vec<TestItemId>,
        #[serde(default)]
        widened: Option<String>,
        #[serde(default)]
        items: Vec<TestItem>,
    },
    Started {
        id: TestItemId,
    },
    Output {
        #[serde(default)]
        id: Option<TestItemId>,
        text: String,
    },
    Finished {
        id: TestItemId,
        status: CaseStatus,
        #[serde(default)]
        duration_ms: Option<u64>,
        #[serde(default)]
        failures: Vec<TestFailure>,
    },
    Error {
        message: String,
    },
    Complete,
    Unsupported,
    #[serde(other)]
    Unknown,
}

/// Identify pre-run or execution work for live status.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestPhase {
    Build,
    Discover,
    Run,
}

/// Classify a terminal test outcome without treating skipped cases as failures.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CaseStatus {
    Passed,
    Failed,
    Skipped,
    TimedOut,
    Errored,
}

impl CaseStatus {
    /// Determine whether this terminal status prevents passing a check.
    fn is_failure(self) -> bool {
        match self {
            Self::Passed | Self::Skipped => false,
            Self::Failed | Self::TimedOut | Self::Errored => true,
        }
    }
}

/// Preserve an assertion failure and its optional repository source location.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TestFailure {
    pub message: String,
    #[serde(default)]
    pub path: Option<String>,
    #[serde(default)]
    pub line: Option<u32>,
}

/// Publish progress derived only from accepted events.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TestProgress {
    pub phase: Option<TestPhase>,
    /// Latest detail for the current phase, cleared when the phase changes.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    pub total: Option<usize>,
    pub finished: usize,
    pub failed: usize,
    pub running: Vec<TestItemId>,
}

/// Retain one planned case even when it never receives a terminal event.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CaseResult {
    pub id: TestItemId,
    pub status: Option<CaseStatus>,
    pub duration_ms: Option<u64>,
    pub failures: Vec<TestFailure>,
    pub item: Option<TestItem>,
    pub output: Option<PathBuf>,
    pub output_truncated_bytes: u64,
}

/// Preserve the result and failure causes of one adapter test run.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TestRunResult {
    pub selection: TestSelection,
    /// Preserve the discovered hierarchy used to group cases without interpreting IDs.
    pub items: Vec<TestItem>,
    pub widened: Option<String>,
    pub cases: Vec<CaseResult>,
    pub not_found: Vec<TestItemId>,
    pub error: Option<String>,
    pub protocol_error: Option<String>,
    pub exit: Option<i32>,
    pub cancelled: bool,
    pub unsupported: bool,
    pub output_truncated_bytes: u64,
}

impl TestRunResult {
    /// Map validated coverage and process termination to the existing check state.
    pub fn state(&self) -> CheckState {
        if self.cancelled {
            return CheckState::Interrupted;
        }
        if self.protocol_error.is_some() || self.unsupported {
            return CheckState::ConfigurationError;
        }
        if self.error.is_some()
            || !self.not_found.is_empty()
            || self
                .cases
                .iter()
                .any(|case| case.status.is_none_or(CaseStatus::is_failure))
        {
            return CheckState::Failed;
        }
        if self.cases.is_empty() {
            return if self.selection.ids.is_empty() && !self.selection.affected.is_empty() {
                CheckState::Skipped
            } else {
                CheckState::Failed
            };
        }
        CheckState::Passed
    }
}

/// Validate the event lifecycle and retain bounded output for one test run.
pub struct TestEventSink {
    selection: TestSelection,
    output_directory: PathBuf,
    cases: BTreeMap<TestItemId, CaseResult>,
    items: BTreeMap<TestItemId, TestItem>,
    started: BTreeSet<TestItemId>,
    planned: bool,
    complete: bool,
    unsupported: bool,
    record_count: usize,
    widened: Option<String>,
    phase: Option<TestPhase>,
    phase_message: Option<String>,
    error: Option<String>,
    protocol_error: Option<String>,
    retained_bytes: usize,
    case_bytes: BTreeMap<TestItemId, usize>,
    output_truncated_bytes: u64,
    progress_sender: Option<tokio::sync::watch::Sender<TestProgress>>,
}

impl TestEventSink {
    /// Create a sink for an admitted selection and private command output directory.
    pub fn new(
        mut selection: TestSelection,
        output_directory: &Path,
    ) -> Result<Self, AdapterError> {
        selection.normalize()?;
        Ok(Self {
            selection,
            output_directory: output_directory.to_owned(),
            cases: BTreeMap::new(),
            items: BTreeMap::new(),
            started: BTreeSet::new(),
            planned: false,
            complete: false,
            unsupported: false,
            record_count: 0,
            widened: None,
            phase: None,
            phase_message: None,
            error: None,
            protocol_error: None,
            retained_bytes: 0,
            case_bytes: BTreeMap::new(),
            output_truncated_bytes: 0,
            progress_sender: None,
        })
    }

    /// Accept a typed event, latching any protocol failure for final reconciliation.
    pub fn accept(&mut self, event: TestEvent) -> Result<(), AdapterError> {
        let result = self.event_accept(event);
        if let Err(error) = &result {
            self.protocol_error.get_or_insert_with(|| error.to_string());
        }
        if let Some(sender) = &self.progress_sender {
            sender.send_replace(self.progress());
        }
        result
    }

    /// Subscribe to accepted progress without sharing mutable event state.
    pub fn progress_subscribe(&mut self) -> tokio::sync::watch::Receiver<TestProgress> {
        let (sender, receiver) = tokio::sync::watch::channel(self.progress());
        self.progress_sender = Some(sender);
        receiver
    }

    /// Retain an adapter failure before reconciling its partial results.
    pub fn failure(&mut self, error: &AdapterError) {
        match error {
            AdapterError::Protocol(message) => {
                self.protocol_error.get_or_insert_with(|| message.clone());
            }
            AdapterError::Cancelled => {}
            AdapterError::Unsupported(_) => {
                self.unsupported = true;
            }
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

    /// Decode one complete JSON object and accept its typed event.
    pub fn accept_line(&mut self, line: &str) -> Result<(), AdapterError> {
        match super::common::decode(line.as_bytes()) {
            Ok(event) => self.accept(event),
            Err(error) => {
                self.protocol_error.get_or_insert_with(|| error.to_string());
                Err(error)
            }
        }
    }

    /// Get live counts without inferring a plan from observed case output.
    pub fn progress(&self) -> TestProgress {
        TestProgress {
            phase: self.phase,
            message: self.phase_message.clone(),
            total: self.planned.then_some(self.cases.len()),
            finished: self
                .cases
                .values()
                .filter(|case| case.status.is_some())
                .count(),
            failed: self
                .cases
                .values()
                .filter(|case| case.status.is_some_and(CaseStatus::is_failure))
                .count(),
            running: self
                .started
                .iter()
                .filter(|id| {
                    self.cases
                        .get(*id)
                        .is_some_and(|case| case.status.is_none())
                })
                .cloned()
                .collect(),
        }
    }

    /// Reconcile completion, requested coverage, and process exit while retaining partial results.
    pub fn finish(mut self, exit: Option<RunExit>, cancelled: bool) -> TestRunResult {
        let covered = self.covered_items();
        let not_found = self
            .selection
            .ids
            .iter()
            .filter(|requested| !covered.contains(*requested))
            .cloned()
            .collect();
        if !cancelled && !self.unsupported {
            if self.error.is_none() && (!self.planned || !self.complete) {
                self.protocol_error
                    .get_or_insert_with(|| "test stream is missing plan or complete".into());
            }
            let failed = self
                .cases
                .values()
                .any(|case| case.status.is_none_or(CaseStatus::is_failure));
            match exit {
                Some(RunExit { success: true, .. }) if failed && self.error.is_none() => {
                    self.protocol_error.get_or_insert_with(|| {
                        "successful exit with failed or unfinished cases".into()
                    });
                }
                Some(RunExit {
                    success: false,
                    code,
                }) if self.error.is_none() => {
                    self.error = Some(format!("test process exited unsuccessfully ({code:?})"));
                }
                None if self.error.is_none() => {
                    self.error = Some("test process did not return an exit status".into());
                }
                Some(_) | None => {}
            }
        }
        TestRunResult {
            items: self.items.into_values().collect(),
            selection: self.selection,
            widened: self.widened,
            cases: self.cases.into_values().collect(),
            not_found,
            error: self.error,
            protocol_error: self.protocol_error,
            exit: exit.and_then(|exit| exit.code),
            cancelled,
            unsupported: self.unsupported,
            output_truncated_bytes: self.output_truncated_bytes,
        }
    }

    /// Enforce ordering and validate an event before publishing any resulting progress.
    fn event_accept(&mut self, event: TestEvent) -> Result<(), AdapterError> {
        if self.complete || self.unsupported || self.protocol_error.is_some() {
            return Err(AdapterError::Protocol(
                "event received after terminal record or protocol failure".into(),
            ));
        }
        self.record_count += 1;
        match event {
            TestEvent::Unknown => Ok(()),
            TestEvent::Phase { name, message } => {
                self.phase = Some(name);
                self.phase_message = message;
                Ok(())
            }
            TestEvent::Plan {
                cases,
                widened,
                items,
            } => self.plan_accept(cases, widened, items),
            TestEvent::Started { id } => {
                self.case_get(&id)?;
                if self.cases[&id].status.is_some() || !self.started.insert(id) {
                    return Err(AdapterError::Protocol(
                        "duplicate or late case start".into(),
                    ));
                }
                Ok(())
            }
            TestEvent::Output { id, text } => self.output_accept(id, text),
            TestEvent::Finished {
                id,
                status,
                duration_ms,
                failures,
            } => {
                self.case_get(&id)?;
                if self.cases[&id].status.is_some() {
                    return Err(AdapterError::Protocol("duplicate case result".into()));
                }
                for failure in &failures {
                    if let Some(path) = &failure.path {
                        validate_path(path)?;
                    }
                    if failure.line == Some(0) || (failure.line.is_some() && failure.path.is_none())
                    {
                        return Err(AdapterError::Protocol(
                            "invalid test failure location".into(),
                        ));
                    }
                }
                let failures = failures
                    .into_iter()
                    .map(|mut failure| {
                        let retained = self.text_retain(&failure.message, None);
                        failure.message.truncate(retained);
                        failure
                    })
                    .collect();
                let case = self.cases.get_mut(&id).expect("case was validated above");
                case.status = Some(status);
                case.duration_ms = duration_ms;
                case.failures = failures;
                Ok(())
            }
            TestEvent::Error { message } => {
                self.error.get_or_insert(message);
                Ok(())
            }
            TestEvent::Complete => {
                if (!self.planned && self.error.is_none())
                    || (self.error.is_none()
                        && self.cases.values().any(|case| case.status.is_none()))
                {
                    return Err(AdapterError::Protocol(
                        "completion before plan or terminal case results".into(),
                    ));
                }
                self.complete = true;
                Ok(())
            }
            TestEvent::Unsupported => {
                if self.record_count != 1 {
                    return Err(AdapterError::Protocol(
                        "unsupported must be the sole event".into(),
                    ));
                }
                self.unsupported = true;
                Ok(())
            }
        }
    }

    /// Validate a complete plan before replacing the sink's coverage state.
    fn plan_accept(
        &mut self,
        cases: Vec<TestItemId>,
        widened: Option<String>,
        items: Vec<TestItem>,
    ) -> Result<(), AdapterError> {
        if self.planned {
            return Err(AdapterError::Protocol("duplicate test plan".into()));
        }
        if cases.len() > ITEM_LIMIT {
            return Err(AdapterError::Protocol(
                "test plan exceeds 100000 cases".into(),
            ));
        }
        validate_tree(&items)?;
        let items: BTreeMap<_, _> = items
            .into_iter()
            .map(|item| (item.id.clone(), item))
            .collect();
        let mut results = BTreeMap::new();
        for id in cases {
            let item = items.get(&id).cloned();
            if item.as_ref().is_some_and(|item| !item.case) {
                return Err(AdapterError::Protocol(
                    "planned case is marked as a group".into(),
                ));
            }
            let result = CaseResult {
                id: id.clone(),
                status: None,
                duration_ms: None,
                failures: Vec::new(),
                item,
                output: None,
                output_truncated_bytes: 0,
            };
            if results.insert(id, result).is_some() {
                return Err(AdapterError::Protocol("duplicate planned case".into()));
            }
        }
        self.cases = results;
        self.items = items;
        self.widened = widened;
        self.planned = true;
        Ok(())
    }

    /// Require a previously announced case for case-specific events.
    fn case_get(&self, id: &TestItemId) -> Result<(), AdapterError> {
        if !self.planned || !self.cases.contains_key(id) {
            return Err(AdapterError::Protocol(format!(
                "unplanned case {}",
                id.as_str()
            )));
        }
        Ok(())
    }

    /// Retain bounded case output under generated filenames, never adapter IDs.
    fn output_accept(&mut self, id: Option<TestItemId>, text: String) -> Result<(), AdapterError> {
        if let Some(id) = &id {
            self.case_get(id)?;
        }
        let retained = self.text_retain(&text, id.as_ref());
        if retained == 0 {
            return Ok(());
        }
        let filename = match &id {
            Some(id) => format!("case-{:x}.log", Sha256::digest(id.as_str().as_bytes())),
            None => "run.log".into(),
        };
        let path = self.output_directory.join(filename);
        std::fs::create_dir_all(&self.output_directory).map_err(AdapterError::Storage)?;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&path)
            .map_err(AdapterError::Storage)?;
        file.write_all(&text.as_bytes()[..retained])
            .map_err(AdapterError::Storage)?;
        if let Some(id) = id {
            self.cases.get_mut(&id).expect("case is in the plan").output = Some(path);
        }
        Ok(())
    }

    /// Bound retained UTF-8 text while counting every omitted byte.
    fn text_retain(&mut self, text: &str, id: Option<&TestItemId>) -> usize {
        let available = OUTPUT_LIMIT.saturating_sub(self.retained_bytes);
        let available = match id {
            Some(id) => available
                .min(CASE_OUTPUT_LIMIT.saturating_sub(*self.case_bytes.get(id).unwrap_or(&0))),
            None => available,
        };
        let mut retained = available.min(text.len());
        while !text.is_char_boundary(retained) {
            retained -= 1;
        }
        let omitted = u64::try_from(text.len() - retained).expect("text length fits u64");
        self.retained_bytes += retained;
        self.output_truncated_bytes += omitted;
        if let Some(id) = id {
            *self.case_bytes.entry(id.clone()).or_default() += retained;
            self.cases
                .get_mut(id)
                .expect("case is in the plan")
                .output_truncated_bytes += omitted;
        }
        retained
    }

    /// Collect covered ancestors once, without interpreting adapter ID syntax.
    fn covered_items(&self) -> BTreeSet<&TestItemId> {
        let mut covered = BTreeSet::new();
        for case in self.cases.keys() {
            let mut current = case;
            while covered.insert(current) {
                match self
                    .items
                    .get(current)
                    .and_then(|item| item.parent.as_ref())
                {
                    Some(parent) => current = parent,
                    None => break,
                }
            }
        }
        covered
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Create an all-tests sink without producing output files.
    fn sink() -> TestEventSink {
        TestEventSink::new(TestSelection::default(), Path::new("unused-test-output")).unwrap()
    }

    /// Keep older progress readable and omit absent details from the saved representation.
    #[test]
    fn progress_without_phase_message_remains_compatible() {
        let original = serde_json::json!({
            "phase": "build", "total": null, "finished": 0, "failed": 0, "running": []
        });
        let progress: TestProgress = serde_json::from_value(original.clone()).unwrap();
        assert_eq!(progress.message, None);
        assert_eq!(serde_json::to_value(progress).unwrap(), original);
    }

    /// Deliver fixture JSON through the real decoder and lifecycle validator.
    fn events(sink: &mut TestEventSink, records: &[&str]) {
        for record in records {
            sink.accept_line(record).unwrap();
        }
    }

    /// Reconcile a sink with an ordinary successful process exit.
    fn finish(sink: TestEventSink) -> TestRunResult {
        sink.finish(
            Some(RunExit {
                code: Some(0),
                success: true,
            }),
            false,
        )
    }

    #[test]
    fn complete_case_results_and_opaque_parent_coverage_pass() {
        let selection = TestSelection {
            ids: vec![TestItemId::try_from("group!".to_owned()).unwrap()],
            affected: vec![],
        };
        let mut sink = TestEventSink::new(selection, Path::new("unused-test-output")).unwrap();
        events(
            &mut sink,
            &[
                r#"{"event":"phase","name":"build"}"#,
                r#"{"event":"plan","cases":["opaque"],"items":[{"id":"group!","label":"group"},{"id":"opaque","parent":"group!","label":"case","case":true}]}"#,
                r#"{"event":"started","id":"opaque"}"#,
            ],
        );
        assert_eq!(sink.progress().running.len(), 1);
        events(
            &mut sink,
            &[
                r#"{"event":"finished","id":"opaque","status":"passed"}"#,
                r#"{"event":"complete"}"#,
            ],
        );
        let result = finish(sink);
        assert!(result.not_found.is_empty());
        assert_eq!(result.state(), CheckState::Passed);
    }

    #[test]
    fn incomplete_success_cannot_pass() {
        for records in [
            vec![],
            vec![r#"{"event":"plan","cases":[]}"#],
            vec![r#"{"event":"plan","cases":["case"]}"#],
            vec![
                r#"{"event":"plan","cases":["case"]}"#,
                r#"{"event":"finished","id":"case","status":"failed"}"#,
                r#"{"event":"complete"}"#,
            ],
        ] {
            let mut sink = sink();
            events(&mut sink, &records);
            assert_eq!(finish(sink).state(), CheckState::ConfigurationError);
        }
    }

    #[test]
    fn empty_affected_selection_skips_but_missing_explicit_ids_fail() {
        for (selection, expected) in [
            (TestSelection::default(), CheckState::Failed),
            (
                TestSelection {
                    ids: vec![],
                    affected: vec!["src/lib.rs".into()],
                },
                CheckState::Skipped,
            ),
            (
                TestSelection {
                    ids: vec![TestItemId::try_from("missing".to_owned()).unwrap()],
                    affected: vec![],
                },
                CheckState::Failed,
            ),
        ] {
            let mut sink = TestEventSink::new(selection, Path::new("unused-test-output")).unwrap();
            events(
                &mut sink,
                &[r#"{"event":"plan","cases":[]}"#, r#"{"event":"complete"}"#],
            );
            assert_eq!(finish(sink).state(), expected);
        }
    }

    #[test]
    fn invalid_event_order_is_latched_even_if_caller_ignores_error() {
        for (prefix, invalid) in [
            (vec![], r#"{"event":"started","id":"case"}"#),
            (
                vec![r#"{"event":"plan","cases":[]}"#],
                r#"{"event":"plan","cases":[]}"#,
            ),
            (
                vec![r#"{"event":"plan","cases":[]}"#],
                r#"{"event":"finished","id":"unknown","status":"passed"}"#,
            ),
            (
                vec![r#"{"event":"plan","cases":[]}"#, r#"{"event":"complete"}"#],
                r#"{"event":"future_event"}"#,
            ),
            (
                vec![r#"{"event":"unsupported"}"#],
                r#"{"event":"complete"}"#,
            ),
            (
                vec![r#"{"event":"future_event"}"#],
                r#"{"event":"unsupported"}"#,
            ),
            (
                vec![r#"{"event":"plan","cases":["case"]}"#],
                r#"{"event":"complete"}"#,
            ),
        ] {
            let mut sink = sink();
            events(&mut sink, &prefix);
            assert!(sink.accept_line(invalid).is_err());
            assert_eq!(finish(sink).state(), CheckState::ConfigurationError);
        }
    }

    #[test]
    fn cancellation_and_run_errors_preserve_completed_cases() {
        for cancelled in [false, true] {
            let mut sink = sink();
            events(
                &mut sink,
                &[
                    r#"{"event":"plan","cases":["done","pending"]}"#,
                    r#"{"event":"finished","id":"done","status":"passed"}"#,
                    r#"{"event":"error","message":"worker died"}"#,
                ],
            );
            let result = sink.finish(None, cancelled);
            assert_eq!(
                result.state(),
                if cancelled {
                    CheckState::Interrupted
                } else {
                    CheckState::Failed
                }
            );
            assert_eq!(result.cases[0].status, Some(CaseStatus::Passed));
            assert_eq!(result.cases[1].status, None);
        }
    }

    #[test]
    fn startup_error_may_complete_without_a_plan() {
        let mut sink = sink();
        events(
            &mut sink,
            &[
                r#"{"event":"error","message":"compiler unavailable"}"#,
                r#"{"event":"complete"}"#,
            ],
        );
        let result = finish(sink);
        assert_eq!(result.state(), CheckState::Failed);
        assert!(result.protocol_error.is_none());
    }

    #[test]
    fn duplicate_case_transitions_and_invalid_plans_are_rejected() {
        for records in [
            vec![r#"{"event":"plan","cases":["a","a"]}"#],
            vec![r#"{"event":"plan","cases":["a"],"items":[{"id":"a","label":"group"}]}"#],
            vec![
                r#"{"event":"plan","cases":["a"]}"#,
                r#"{"event":"started","id":"a"}"#,
                r#"{"event":"started","id":"a"}"#,
            ],
            vec![
                r#"{"event":"plan","cases":["a"]}"#,
                r#"{"event":"finished","id":"a","status":"passed"}"#,
                r#"{"event":"finished","id":"a","status":"passed"}"#,
            ],
            vec![
                r#"{"event":"plan","cases":["a"]}"#,
                r#"{"event":"finished","id":"a","status":"passed"}"#,
                r#"{"event":"started","id":"a"}"#,
            ],
        ] {
            let mut sink = sink();
            let (last, prefix) = records.split_last().unwrap();
            events(&mut sink, prefix);
            assert!(sink.accept_line(last).is_err());
            assert_eq!(finish(sink).state(), CheckState::ConfigurationError);
        }
    }

    #[test]
    fn exit_failure_cannot_be_hidden_by_passing_cases() {
        let mut sink = sink();
        events(
            &mut sink,
            &[
                r#"{"event":"plan","cases":["case"],"future_field":true}"#,
                r#"{"event":"future_event","payload":[]}"#,
                r#"{"event":"finished","id":"case","status":"passed"}"#,
                r#"{"event":"complete"}"#,
            ],
        );
        let result = sink.finish(
            Some(RunExit {
                code: Some(7),
                success: false,
            }),
            false,
        );
        assert_eq!(result.state(), CheckState::Failed);
        assert!(result.error.unwrap().contains('7'));
    }

    #[test]
    fn output_is_bounded_on_utf8_boundaries_without_hiding_failure() {
        let directory = tempfile::tempdir().unwrap();
        let mut sink = TestEventSink::new(TestSelection::default(), directory.path()).unwrap();
        events(&mut sink, &[r#"{"event":"plan","cases":["../../unsafe"]}"#]);
        sink.accept(TestEvent::Output {
            id: Some(TestItemId::try_from("../../unsafe".to_owned()).unwrap()),
            text: "é".repeat(CASE_OUTPUT_LIMIT),
        })
        .unwrap();
        events(
            &mut sink,
            &[
                r#"{"event":"finished","id":"../../unsafe","status":"failed"}"#,
                r#"{"event":"complete"}"#,
            ],
        );
        let result = sink.finish(
            Some(RunExit {
                code: Some(1),
                success: false,
            }),
            false,
        );
        assert_eq!(result.state(), CheckState::Failed);
        let case = &result.cases[0];
        assert_eq!(
            case.output_truncated_bytes,
            u64::try_from(CASE_OUTPUT_LIMIT).unwrap()
        );
        let path = case.output.as_ref().unwrap();
        assert_eq!(path.parent(), Some(directory.path()));
        assert_eq!(
            std::fs::read_to_string(path).unwrap().len(),
            CASE_OUTPUT_LIMIT
        );
    }
}
