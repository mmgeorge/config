use super::super::contract::{ITEM_LIMIT, validate_path};
use super::super::{AdapterError, PathSelection, RunExit};
use super::TestPhase;
use crate::checks::CheckState;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

const TEXT_LIMIT: usize = 16 * 1024 * 1024;

/// Distinguish read-only formatting verification from source mutation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FormatOperation {
    Check,
    Apply,
}

/// Describe one terminal file outcome under the selected formatting operation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FormatStatus {
    Formatted,
    Unformatted,
    Changed,
    Failed,
}

/// Describe a formatter's optional plan and reported per-file outcomes.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum FormatEvent {
    Phase {
        name: TestPhase,
        #[serde(default)]
        message: Option<String>,
    },
    Plan {
        #[serde(default)]
        files: Option<Vec<String>>,
        #[serde(default)]
        widened: Option<String>,
    },
    File {
        path: String,
        status: FormatStatus,
        #[serde(default)]
        message: Option<String>,
        #[serde(default)]
        diff: Option<String>,
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

/// Preserve a declared file even when the formatter never reports its outcome.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct FormatFileResult {
    pub path: String,
    pub status: Option<FormatStatus>,
    pub message: Option<String>,
    pub diff: Option<String>,
}

/// Count reported outcomes without claiming an unknown processed-file total.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FormatProgress {
    pub phase: Option<TestPhase>,
    pub total: Option<usize>,
    pub reported: usize,
    pub unformatted: usize,
}

/// Retain formatting coverage, bounded detail text, and independent failure causes.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct FormatResult {
    pub operation: FormatOperation,
    pub selection: PathSelection,
    pub widened: Option<String>,
    pub files: Vec<FormatFileResult>,
    pub truncated_bytes: u64,
    pub error: Option<String>,
    pub protocol_error: Option<String>,
    pub exit: Option<i32>,
    pub cancelled: bool,
    pub unsupported: bool,
}

impl FormatResult {
    /// Require complete file coverage and successful process exit before passing.
    pub fn state(&self) -> CheckState {
        if self.cancelled {
            return CheckState::Interrupted;
        }
        if self.protocol_error.is_some() || self.unsupported {
            return CheckState::ConfigurationError;
        }
        if self.error.is_some()
            || self.exit.is_some_and(|code| code != 0)
            || self.files.iter().any(|file| match file.status {
                None | Some(FormatStatus::Failed) => true,
                Some(FormatStatus::Unformatted) => self.operation == FormatOperation::Check,
                Some(FormatStatus::Formatted | FormatStatus::Changed) => false,
            })
        {
            return CheckState::Failed;
        }
        CheckState::Passed
    }
}

/// Validate formatting streams while retaining all file statuses independently of text limits.
pub struct FormatSink {
    operation: FormatOperation,
    selection: PathSelection,
    files: BTreeMap<String, FormatFileResult>,
    plan_seen: bool,
    declared_files: bool,
    file_seen: bool,
    complete: bool,
    unsupported: bool,
    record_count: usize,
    phase: Option<TestPhase>,
    widened: Option<String>,
    remaining_text: usize,
    truncated_bytes: u64,
    error: Option<String>,
    protocol_error: Option<String>,
    progress_sender: Option<tokio::sync::watch::Sender<FormatProgress>>,
}

impl FormatSink {
    /// Bind the operation and normalize paths before accepting formatter output.
    pub fn new(
        operation: FormatOperation,
        mut selection: PathSelection,
    ) -> Result<Self, AdapterError> {
        selection.normalize()?;
        Ok(Self {
            operation,
            selection,
            files: BTreeMap::new(),
            plan_seen: false,
            declared_files: false,
            file_seen: false,
            complete: false,
            unsupported: false,
            record_count: 0,
            phase: None,
            widened: None,
            remaining_text: TEXT_LIMIT,
            truncated_bytes: 0,
            error: None,
            protocol_error: None,
            progress_sender: None,
        })
    }

    /// Latch protocol failures before publishing progress from accepted records.
    pub fn accept(&mut self, event: FormatEvent) -> Result<(), AdapterError> {
        let accepted = self.event_accept(event);
        if let Err(error) = &accepted {
            self.protocol_error.get_or_insert_with(|| error.to_string());
        }
        if let Some(sender) = &self.progress_sender {
            sender.send_replace(self.progress());
        }
        accepted
    }

    /// Decode a complete formatter record through the common bounded parser.
    pub fn accept_line(&mut self, line: &str) -> Result<(), AdapterError> {
        match super::common::decode(line.as_bytes()) {
            Ok(event) => self.accept(event),
            Err(error) => {
                self.failure(&error);
                Err(error)
            }
        }
    }

    /// Subscribe to owned progress without sharing mutable file results.
    pub fn progress_subscribe(&mut self) -> tokio::sync::watch::Receiver<FormatProgress> {
        let (sender, receiver) = tokio::sync::watch::channel(self.progress());
        self.progress_sender = Some(sender);
        receiver
    }

    /// Get reported outcomes and an optional formatter-declared total.
    pub fn progress(&self) -> FormatProgress {
        FormatProgress {
            phase: self.phase,
            total: self.declared_files.then_some(self.files.len()),
            reported: self
                .files
                .values()
                .filter(|file| file.status.is_some())
                .count(),
            unformatted: self
                .files
                .values()
                .filter(|file| file.status == Some(FormatStatus::Unformatted))
                .count(),
        }
    }

    /// Preserve an owned execution failure alongside any completed file outcomes.
    pub fn failure(&mut self, error: &AdapterError) {
        match error {
            AdapterError::Protocol(message) => {
                self.protocol_error.get_or_insert_with(|| message.clone());
            }
            AdapterError::Unsupported(_) => {
                let _ = self.accept(FormatEvent::Unsupported);
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

    /// Reconcile normal completion and process exit without discarding interrupted results.
    pub fn finish(mut self, exit: Option<RunExit>, cancelled: bool) -> FormatResult {
        if !cancelled && !self.unsupported {
            if !self.complete && self.error.is_none() {
                self.protocol_error
                    .get_or_insert_with(|| "format stream is missing complete".into());
            }
            match exit {
                Some(RunExit {
                    success: false,
                    code,
                }) if self.error.is_none() => {
                    self.error = Some(format!("formatter exited unsuccessfully ({code:?})"));
                }
                None if self.error.is_none() => {
                    self.error = Some("formatter did not return an exit status".into());
                }
                Some(_) | None => {}
            }
        }
        FormatResult {
            operation: self.operation,
            selection: self.selection,
            widened: self.widened,
            files: self.files.into_values().collect(),
            truncated_bytes: self.truncated_bytes,
            error: self.error,
            protocol_error: self.protocol_error,
            exit: exit.and_then(|exit| exit.code),
            cancelled,
            unsupported: self.unsupported,
        }
    }

    /// Validate category-specific ordering, coverage, and operation permissions before mutation.
    fn event_accept(&mut self, event: FormatEvent) -> Result<(), AdapterError> {
        if self.complete || self.unsupported || self.protocol_error.is_some() {
            return Err(AdapterError::Protocol(
                "event received after terminal record or protocol failure".into(),
            ));
        }
        self.record_count += 1;
        match event {
            FormatEvent::Unknown | FormatEvent::Output { .. } => {}
            FormatEvent::Phase { name, .. } => self.phase = Some(name),
            FormatEvent::Plan { files, widened } => {
                if self.plan_seen || self.file_seen {
                    return Err(AdapterError::Protocol(
                        "duplicate or late format plan".into(),
                    ));
                }
                let mut declared = BTreeMap::new();
                if let Some(files) = &files {
                    if files.len() > ITEM_LIMIT {
                        return Err(AdapterError::Protocol("format file limit exceeded".into()));
                    }
                    for path in files {
                        validate_path(path)?;
                        if declared
                            .insert(
                                path.clone(),
                                FormatFileResult {
                                    path: path.clone(),
                                    status: None,
                                    message: None,
                                    diff: None,
                                },
                            )
                            .is_some()
                        {
                            return Err(AdapterError::Protocol(
                                "duplicate planned format file".into(),
                            ));
                        }
                    }
                }
                self.files = declared;
                self.declared_files = files.is_some();
                self.plan_seen = true;
                self.widened = widened;
            }
            FormatEvent::File {
                path,
                status,
                mut message,
                mut diff,
            } => {
                validate_path(&path)?;
                if self
                    .files
                    .get(&path)
                    .is_some_and(|file| file.status.is_some())
                {
                    return Err(AdapterError::Protocol(
                        "duplicate format file result".into(),
                    ));
                }
                if self.declared_files && !self.files.contains_key(&path) {
                    return Err(AdapterError::Protocol("unplanned format file".into()));
                }
                if !self.files.contains_key(&path) && self.files.len() >= ITEM_LIMIT {
                    return Err(AdapterError::Protocol("format file limit exceeded".into()));
                }
                if matches!(
                    (self.operation, status),
                    (FormatOperation::Check, FormatStatus::Changed)
                        | (FormatOperation::Apply, FormatStatus::Unformatted)
                ) {
                    return Err(AdapterError::Protocol(
                        "format status is invalid for the requested operation".into(),
                    ));
                }
                if self.operation == FormatOperation::Apply && diff.is_some() {
                    return Err(AdapterError::Protocol(
                        "format diffs are only valid for check operations".into(),
                    ));
                }
                for text in [&mut message, &mut diff].into_iter().flatten() {
                    self.truncated_bytes +=
                        super::common::text_retain(text, &mut self.remaining_text);
                }
                self.files.insert(
                    path.clone(),
                    FormatFileResult {
                        path,
                        status: Some(status),
                        message,
                        diff,
                    },
                );
                self.file_seen = true;
            }
            FormatEvent::Error { message } => {
                self.error.get_or_insert(message);
            }
            FormatEvent::Complete => self.complete = true,
            FormatEvent::Unsupported => {
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
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reconcile a formatter fixture while preserving the first invalid record.
    fn result(
        operation: FormatOperation,
        lines: &[&str],
        exit: i32,
        cancelled: bool,
    ) -> FormatResult {
        let mut sink = FormatSink::new(operation, PathSelection::default()).unwrap();
        for line in lines {
            let _ = sink.accept_line(line);
        }
        sink.finish(
            Some(RunExit {
                code: Some(exit),
                success: exit == 0,
            }),
            cancelled,
        )
    }

    /// Enforce check/apply status semantics, completion, exit status, and declared coverage.
    #[test]
    fn operation_and_coverage_determine_format_outcomes() {
        let complete = r#"{"event":"complete"}"#;
        let clean = r#"{"event":"file","path":"src/a.rs","status":"formatted"}"#;
        let dirty = r#"{"event":"file","path":"src/a.rs","status":"unformatted"}"#;
        let changed = r#"{"event":"file","path":"src/a.rs","status":"changed"}"#;
        let plan = r#"{"event":"plan","files":["src/a.rs","src/b.rs"]}"#;
        for (operation, lines, exit, expected) in [
            (
                FormatOperation::Check,
                vec![clean, complete],
                0,
                CheckState::Passed,
            ),
            (
                FormatOperation::Apply,
                vec![changed, complete],
                0,
                CheckState::Passed,
            ),
            (
                FormatOperation::Check,
                vec![dirty, complete],
                0,
                CheckState::Failed,
            ),
            (
                FormatOperation::Check,
                vec![clean, complete],
                1,
                CheckState::Failed,
            ),
            (
                FormatOperation::Check,
                vec![plan, clean, complete],
                0,
                CheckState::Failed,
            ),
            (
                FormatOperation::Check,
                vec![changed, complete],
                0,
                CheckState::ConfigurationError,
            ),
            (
                FormatOperation::Apply,
                vec![dirty, complete],
                0,
                CheckState::ConfigurationError,
            ),
            (
                FormatOperation::Check,
                vec![clean],
                0,
                CheckState::ConfigurationError,
            ),
            (
                FormatOperation::Check,
                vec![],
                0,
                CheckState::ConfigurationError,
            ),
        ] {
            assert_eq!(
                result(operation, &lines, exit, false).state(),
                expected,
                "{operation:?} {lines:?}"
            );
        }
        let interrupted = result(FormatOperation::Check, &[plan, clean], 1, true);
        assert_eq!(interrupted.state(), CheckState::Interrupted);
        assert_eq!(interrupted.files[0].status, Some(FormatStatus::Formatted));
        assert_eq!(interrupted.files[1].status, None);
    }

    /// Reject duplicate, out-of-scope, late, and operation-incompatible records.
    #[test]
    fn invalid_format_records_cannot_be_hidden_by_later_completion() {
        let clean = r#"{"event":"file","path":"src/a.rs","status":"formatted"}"#;
        let plan = r#"{"event":"plan","files":["src/a.rs"]}"#;
        for lines in [
            vec![plan, plan],
            vec![clean, plan],
            vec![clean, clean],
            vec![r#"{"event":"plan","files":["src/a.rs","src/a.rs"]}"#],
            vec![r#"{"event":"plan","files":[]}"#, clean],
            vec![r#"{"event":"file","path":"../escape.rs","status":"formatted"}"#],
            vec![r#"{"event":"complete"}"#, clean],
            vec![plan, r#"{"event":"unsupported"}"#],
        ] {
            assert_eq!(
                result(FormatOperation::Check, &lines, 0, false).state(),
                CheckState::ConfigurationError
            );
        }
        let lines = [
            r#"{"event":"file","path":"src/a.rs","status":"changed","diff":"edit"}"#,
            r#"{"event":"complete"}"#,
        ];
        assert_eq!(
            result(FormatOperation::Apply, &lines, 0, false).state(),
            CheckState::ConfigurationError
        );
    }

    /// Reject returned unsupported errors after an adapter has already emitted events.
    #[test]
    fn returned_unsupported_cannot_hide_prior_format_events() {
        let mut sink = FormatSink::new(FormatOperation::Apply, PathSelection::default()).unwrap();
        sink.accept_line(r#"{"event":"output","text":"already working"}"#)
            .unwrap();
        sink.failure(&AdapterError::Unsupported("apply".into()));
        assert!(sink.finish(None, false).protocol_error.is_some());
    }

    /// Unknown totals remain absent and text truncation preserves dirty-file status.
    #[test]
    fn unknown_coverage_and_bounded_diffs_preserve_observed_results() {
        let mut sink = FormatSink::new(FormatOperation::Check, PathSelection::default()).unwrap();
        let progress = sink.progress_subscribe();
        sink.accept_line(r#"{"event":"plan","widened":"whole workspace"}"#)
            .unwrap();
        for index in 0..3 {
            sink.accept(FormatEvent::File {
                path: format!("src/{index}.rs"),
                status: FormatStatus::Unformatted,
                message: None,
                diff: Some("é".repeat(3 * 1024 * 1024)),
            })
            .unwrap();
        }
        assert_eq!(progress.borrow().total, None);
        assert_eq!(progress.borrow().reported, 3);
        assert_eq!(progress.borrow().unformatted, 3);
        sink.accept(FormatEvent::Complete).unwrap();
        let result = sink.finish(
            Some(RunExit {
                code: Some(0),
                success: true,
            }),
            false,
        );
        assert_eq!(result.state(), CheckState::Failed);
        assert_eq!(result.truncated_bytes, 2 * 1024 * 1024);
        assert_eq!(
            result
                .files
                .iter()
                .map(|file| file.diff.as_ref().unwrap().len())
                .sum::<usize>(),
            TEXT_LIMIT
        );
    }
}
