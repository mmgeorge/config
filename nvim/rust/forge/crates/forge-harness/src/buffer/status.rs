use anyhow::{Result, ensure};
use forge_buffer::block::{
    BufferBlock, Decoration, StatusHint, StatusPresentation, TargetRange, TextPosition, TextRange,
};
use forge_buffer::document::BufferDocument;
use forge_buffer::identity::{BlockId, DocumentId, DocumentRevision, TargetId};
use forge_buffer::patch::BufferSnapshot;
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::projection::TranscriptAction;
use super::transcript::TranscriptRenderer;
use crate::checks::{CheckActivity, CheckState};
use crate::session::state_machine::{ExecutionStatus, SessionPhase, WorkflowActivity};

/// Complete status replacement, source removal, or an unrelated transcript update.
#[derive(Default)]
pub(super) enum StatusProjection {
    #[default]
    Unchanged,
    Replace {
        source: String,
        rendered: Box<RenderedStatus>,
    },
    Remove {
        source: String,
    },
}

impl StatusProjection {
    pub fn render(
        entry: &crate::timeline::TimelineEntry,
        width: &WidthProfile,
        now_ms: i64,
    ) -> Result<Self> {
        let crate::timeline::TimelineEntry::Status {
            id,
            status,
            agent_count,
            status_thoughts,
            checks,
            ..
        } = entry
        else {
            return Ok(Self::Unchanged);
        };
        Ok(Self::Replace {
            source: id.clone(),
            rendered: Box::new(
                StatusView {
                    id,
                    status,
                    agent_count: *agent_count,
                    status_thoughts: *status_thoughts,
                    checks: checks.as_ref(),
                }
                .render(width, now_ms)?,
            ),
        })
    }
}

/// Native status and action targets retired with their parent presentation.
pub(super) struct StatusDocument {
    id: DocumentId,
    document: BufferDocument,
    source: Option<String>,
    action: Option<(TargetId, TranscriptAction)>,
    content: StatusContent,
}

#[derive(Clone, Default)]
struct StatusContent {
    phase: Option<BufferBlock>,
    staging: Option<BufferBlock>,
}

impl StatusDocument {
    pub fn new(parent: &DocumentId) -> Result<Self> {
        let id = DocumentId(format!("{}:status", parent.0));
        Ok(Self {
            document: BufferDocument::new(id.clone(), Vec::new())?,
            id,
            source: None,
            action: None,
            content: StatusContent::default(),
        })
    }

    pub fn snapshot(&self) -> BufferSnapshot {
        self.document.snapshot()
    }

    pub fn id(&self) -> &DocumentId {
        &self.id
    }

    pub fn apply(&mut self, projection: StatusProjection) -> Result<()> {
        let (source, rendered) = match projection {
            StatusProjection::Unchanged => return Ok(()),
            StatusProjection::Remove { source } => return self.remove(&source),
            StatusProjection::Replace { source, rendered } => (source, rendered),
        };
        self.publish(StatusContent {
            phase: rendered.block,
            ..self.content.clone()
        })?;
        self.source = Some(source);
        self.action = rendered.action;
        Ok(())
    }

    pub fn clear(&mut self) -> Result<()> {
        self.publish(StatusContent {
            phase: None,
            ..self.content.clone()
        })?;
        self.source = None;
        self.action = None;
        Ok(())
    }

    pub fn set_staging(&mut self, staging: Option<BufferBlock>) -> Result<()> {
        self.publish(StatusContent {
            staging,
            ..self.content.clone()
        })
    }

    fn publish(&mut self, content: StatusContent) -> Result<()> {
        let mut blocks: Vec<_> = content.phase.iter().cloned().collect();
        if content.phase.is_some() && content.staging.is_some() {
            blocks.push(BufferBlock {
                id: BlockId(format!("{}:staging-gap", self.id.0)),
                text: BufferText::from_rows(vec![String::new()])?,
                metadata: Default::default(),
            });
        }
        blocks.extend(content.staging.iter().cloned());
        self.document.edit(0..self.document.block_count(), blocks)?;
        self.content = content;
        Ok(())
    }

    pub fn remove(&mut self, source: &str) -> Result<()> {
        if self.source.as_deref() == Some(source) {
            self.clear()?;
        }
        Ok(())
    }

    pub fn action(
        &self,
        document: &DocumentId,
        revision: DocumentRevision,
        target: &TargetId,
    ) -> Result<TranscriptAction> {
        ensure!(
            *document == self.id && revision == self.document.revision(),
            "status document revision changed"
        );
        let (current, action) = self
            .action
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("status is not actionable"))?;
        ensure!(current == target, "status action changed");
        Ok(action.clone())
    }
}

/// Main workflow status and independent check progress for one live session.
pub(super) struct StatusView<'source> {
    /// Stable status block identity in the session presentation.
    pub id: &'source str,
    /// Provider or plan phase, unaffected by background check updates.
    pub status: &'source SessionPhase,
    /// Active child executions across this session.
    pub agent_count: usize,
    /// Whether the current reasoning preview is visible.
    pub status_thoughts: bool,
    /// Current check run without its retained output or historical events.
    pub checks: Option<&'source CheckActivity>,
}

impl StatusView<'_> {
    /// Render status and side activity in one block so timers and layout commit atomically.
    pub fn render(&self, width: &WidthProfile, now_ms: i64) -> Result<RenderedStatus> {
        let Self {
            id,
            status,
            agent_count,
            status_thoughts,
            checks,
        } = *self;
        if matches!(status, SessionPhase::Idle) && checks.is_none() && agent_count == 0 {
            return Ok(RenderedStatus {
                block: None,
                action: None,
            });
        }
        let checking = checks.is_some() && matches!(status, SessionPhase::Idle);
        let text = if checking {
            check_heading(checks.expect("checking requires check activity"), now_ms)
        } else {
            primary_text(status, now_ms)
        };
        let text = if agent_count > 0 && !matches!(status, SessionPhase::WaitingForAgent { .. }) {
            format!(
                "{text} · {agent_count} agent{} running",
                if agent_count == 1 { "" } else { "s" }
            )
        } else {
            text
        };
        let mut block =
            TranscriptRenderer::new(width)?.literal(BlockId(id.to_owned()), &text, 0)?;
        block.metadata.status = Some(StatusPresentation {
            row: 0,
            animated: checking || status.animated(),
            hint: if checking {
                Some(StatusHint::Working)
            } else {
                match status {
                    SessionPhase::Working { .. }
                    | SessionPhase::RetryingPlanGeneration { .. }
                    | SessionPhase::RetryingProvider { .. }
                    | SessionPhase::WaitingForAgent { .. } => Some(StatusHint::Working),
                    SessionPhase::AwaitingInput { .. } => Some(StatusHint::Question),
                    SessionPhase::AwaitingApproval { .. } => Some(StatusHint::Permission),
                    SessionPhase::AwaitingPlanReview { .. } => Some(StatusHint::Review),
                    _ => None,
                }
            },
        });
        let action = bind_action(&mut block, id, status);
        if let Some(checks) = checks {
            append_checks(&mut block, width, checks, status, now_ms)?;
        }
        if status_thoughts
            && let SessionPhase::Working {
                reasoning_summary: Some(summary),
                ..
            } = status
        {
            append_reasoning_preview(&mut block, width, summary)?;
        }
        Ok(RenderedStatus {
            block: Some(block),
            action,
        })
    }
}

/// Status rows and their optional plan-review action.
pub(super) struct RenderedStatus {
    /// Complete status block, including independent check progress.
    pub block: Option<BufferBlock>,
    /// Action owned by the primary status line.
    pub action: Option<(TargetId, TranscriptAction)>,
}

fn primary_text(status: &SessionPhase, now_ms: i64) -> String {
    match status {
        SessionPhase::Idle => "Idle".into(),
        SessionPhase::Paused => "Paused".into(),
        SessionPhase::Stopping { collection } => collection.label().into(),
        SessionPhase::Compacting { .. } => {
            let elapsed_seconds = status.elapsed_ms(now_ms) / 1_000;
            format!("Compacting · {elapsed_seconds}s")
        }
        SessionPhase::Finalizing { outcome, error, .. } => {
            let label = finalization_label(*outcome);
            error
                .as_ref()
                .map_or_else(|| label.into(), |error| format!("{label} failed · {error}"))
        }
        SessionPhase::Working {
            activity,
            execution,
            ..
        } => {
            let elapsed_seconds = status.elapsed_ms(now_ms) / 1_000;
            working_status_text(elapsed_seconds, *activity, execution.as_ref())
        }
        SessionPhase::AwaitingInput { .. } => "Waiting for your answer".into(),
        SessionPhase::AwaitingApproval { .. } => "Waiting for your approval".into(),
        SessionPhase::AwaitingPlanReview { revision, .. } => {
            format!("Waiting for plan review · revision {revision}")
        }
        SessionPhase::RetryingPlanGeneration { activity, .. } => {
            let elapsed = status.elapsed_ms(now_ms) / 1_000;
            format!("{} · {elapsed}s", activity.active_label())
        }
        SessionPhase::RetryingProvider { retry, activity } => {
            let label = format!(
                "{} · retry {}/{}",
                activity.active_label(),
                retry.attempt,
                retry.max_attempts
            );
            match retry.retry_at_ms {
                Some(deadline) => format!(
                    "{label} · retrying in {}s",
                    (deadline.saturating_sub(now_ms).max(0) as u64).div_ceil(1000)
                ),
                None => label,
            }
        }
        SessionPhase::PlanningFailed { reason, .. } => {
            format!("Planning stopped · {reason}")
        }
        SessionPhase::WaitingForAgent { agent_count: 0 } => "Waiting for agents".into(),
        SessionPhase::WaitingForAgent { agent_count } => {
            format!("Waiting for agents · {agent_count} running")
        }
    }
}

fn bind_action(
    block: &mut BufferBlock,
    id: &str,
    status: &SessionPhase,
) -> Option<(TargetId, TranscriptAction)> {
    let mut action = None;
    if let SessionPhase::AwaitingPlanReview { plan_id, .. } = status {
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            capture: "ForgeTimelineStatus".into(),
            priority: 100,
        });
        let target = TargetId(format!("{id}:review-plan"));
        block.metadata.target.push(TargetRange {
            id: target.clone(),
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
        });
        action = Some((
            target,
            TranscriptAction::Plan {
                plan_id: plan_id.clone(),
                revision: None,
            },
        ));
    } else if matches!(
        status,
        SessionPhase::Working { .. } | SessionPhase::AwaitingInput { .. }
    ) {
        let role = if matches!(status, SessionPhase::Working { .. }) {
            "working"
        } else {
            "question"
        };
        block.metadata.target.push(TargetRange {
            id: TargetId(format!("{id}:{role}")),
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
        });
    }
    action
}

/// Describe the complete selected run independently of command progress.
fn check_heading(checks: &CheckActivity, now_ms: i64) -> String {
    format!(
        "Checking · {}/{} finished · {}s",
        checks.completed_count,
        checks.total_count,
        checks.elapsed_ms(now_ms) / 1000
    )
}

/// Render every selected command, expanding live progress and retaining failure causes.
fn append_checks(
    block: &mut BufferBlock,
    width: &WidthProfile,
    checks: &CheckActivity,
    status: &SessionPhase,
    now_ms: i64,
) -> Result<()> {
    let nested = !matches!(status, SessionPhase::Idle);
    let root = if nested { "   " } else { "" };
    let mut lines = Vec::new();
    let mut running_rows = Vec::new();
    if nested {
        lines.push(format!("└─ {}", check_heading(checks, now_ms)));
    }
    for (command_index, command) in checks.command_list.iter().enumerate() {
        let last = command_index + 1 == checks.command_list.len();
        let branch = if last { "└─" } else { "├─" };
        let continuation = if last { "   " } else { "│  " };
        let (marker, outcome) = match command.state {
            CheckState::Pending => ("○", "queued"),
            CheckState::Running => ("⠋", "running"),
            CheckState::Passed => ("✓", "passed"),
            CheckState::Failed => ("✗", "failed"),
            CheckState::Blocked => ("!", "blocked"),
            CheckState::Interrupted => ("!", "interrupted"),
            CheckState::Skipped => ("−", "skipped"),
            CheckState::ConfigurationError => ("✗", "configuration error"),
            CheckState::ExecutionError => ("✗", "execution error"),
            CheckState::Stale => ("!", "stale"),
            CheckState::Stopped => ("!", "stopped"),
        };
        let elapsed = command
            .elapsed_ms(now_ms)
            .map_or_else(String::new, |elapsed| format!(" · {}s", elapsed / 1000));
        if command.state == CheckState::Running {
            running_rows.push(lines.len());
        }
        lines.push(format!(
            "{root}{branch} {marker} {} · {outcome}{elapsed}",
            command.title
        ));
        let mut details = Vec::new();
        if command.state == CheckState::Running {
            if let Some(crate::checks::adapter::AdapterProgress::Tests(tests)) = &command.progress {
                let detail = if let Some(total) = tests.total.filter(|_| {
                    tests.phase != Some(crate::checks::adapter::events::TestPhase::Build)
                }) {
                    format!(
                        "{}/{} tests · {} failed",
                        tests.finished, total, tests.failed
                    )
                } else {
                    match tests.phase {
                        Some(crate::checks::adapter::events::TestPhase::Build) => "Building".into(),
                        Some(crate::checks::adapter::events::TestPhase::Discover) => {
                            "Discovering tests".into()
                        }
                        Some(crate::checks::adapter::events::TestPhase::Run) => {
                            "Running tests".into()
                        }
                        None => "Preparing tests".into(),
                    }
                };
                let message = tests
                    .message
                    .as_deref()
                    .map(|message| message.split_whitespace().collect::<Vec<_>>().join(" "));
                if let Some(message) = message.filter(|message| !message.is_empty()) {
                    details.push(format!("{detail} · {message}"));
                } else {
                    details.push(detail);
                }
            }
            if let Some(crate::checks::adapter::AdapterProgress::Diagnostics(diagnostics)) =
                &command.progress
            {
                let targets = diagnostics.targets_total.map_or_else(
                    || format!("{} targets", diagnostics.targets_finished),
                    |total| format!("{}/{total} targets", diagnostics.targets_finished),
                );
                details.push(format!(
                    "{targets} · {} errors · {} warnings",
                    diagnostics.errors, diagnostics.warnings
                ));
            }
            if let Some(crate::checks::adapter::AdapterProgress::Format(format)) = &command.progress
                && (format.total.is_some() || format.reported > 0)
            {
                let files = format.total.map_or_else(
                    || format!("{} file outcomes", format.reported),
                    |total| format!("{}/{total} files", format.reported),
                );
                details.push(format!("{files} · {} unformatted", format.unformatted));
            }
        }
        if !matches!(
            command.state,
            CheckState::Pending | CheckState::Passed | CheckState::Skipped
        ) && let Some(output_line) = &command.output_line
            && !details.iter().any(|detail| {
                detail == output_line || detail.ends_with(&format!(" · {output_line}"))
            })
        {
            details.push(output_line.clone());
        }
        for (detail_index, detail) in details.iter().enumerate() {
            let detail_branch = if detail_index + 1 == details.len() {
                "└─"
            } else {
                "├─"
            };
            lines.push(format!("{root}{continuation}{detail_branch} {detail}"));
        }
    }
    let rendered = TranscriptRenderer::new(width)?.literal(
        BlockId(format!("{}:checks", block.id.0)),
        &lines.join("\n"),
        0,
    )?;
    let start = block.text.row_count();
    let mut rendered_row = start;
    for (source_row, line) in lines.iter().enumerate() {
        let wrapped = width.wrap_plain(line, 0)?;
        if running_rows.contains(&source_row) {
            for (offset, row) in wrapped.iter().enumerate() {
                if let Some(column) = row.find('⠋') {
                    block.metadata.decoration.push(Decoration {
                        range: TextRange {
                            start: TextPosition {
                                row: rendered_row + offset,
                                column,
                            },
                            end: TextPosition {
                                row: rendered_row + offset,
                                column: column + "⠋".len(),
                            },
                        },
                        capture: "ForgeTimelineStatusSpinner".into(),
                        priority: 110,
                    });
                    break;
                }
            }
        }
        rendered_row += wrapped.len();
    }
    let mut rows = block.text.wire_rows();
    rows.extend(rendered.text.wire_rows());
    block.text = BufferText::from_rows(rows)?;
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition {
                row: if nested { start } else { 0 },
                column: 0,
            },
            end: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        },
        capture: "ForgeStatusHint".into(),
        priority: 100,
    });
    Ok(())
}

fn finalization_label(exchange_outcome: Option<crate::exchange::ExchangeOutcome>) -> &'static str {
    if exchange_outcome == Some(crate::exchange::ExchangeOutcome::Cancelled) {
        "Stopping"
    } else {
        "Saving exchange"
    }
}

fn append_reasoning_preview(
    block: &mut BufferBlock,
    width: &WidthProfile,
    summary: &str,
) -> Result<()> {
    let normalized = summary.split_whitespace().collect::<Vec<_>>().join(" ");
    if normalized.is_empty() {
        return Ok(());
    }
    let rendered = forge_buffer::markdown::MarkdownRenderer::render(
        BlockId(format!("{}:reasoning", block.id.0)),
        &normalized,
        width,
    )?;
    let preview = rendered.block.text.wire_rows().join(" ");
    let start = block.text.row_count();
    let mut rows = block.text.wire_rows();
    rows.push(&preview);
    block.text = forge_buffer::text::BufferText::from_rows(rows)?;
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition {
                row: start,
                column: 0,
            },
            end: TextPosition {
                row: block.text.row_count(),
                column: 0,
            },
        },
        capture: "ForgeHarnessCommentary".into(),
        priority: 100,
    });
    Ok(())
}

fn working_status_text(
    elapsed_seconds: u64,
    activity: WorkflowActivity,
    execution: Option<&ExecutionStatus>,
) -> String {
    let phase = activity.active_label();
    if let Some(execution) = execution.filter(|execution| {
        WorkflowActivity::from(execution.phase) == activity && execution.has_comparison()
    }) {
        let progress = &execution.progress;
        let count = progress
            .missing
            .iter()
            .chain(&progress.different)
            .chain(&progress.unverified)
            .map(|finding| {
                finding
                    .split_once(": ")
                    .map_or(finding.as_str(), |(path, _)| path)
            })
            .collect::<std::collections::BTreeSet<_>>()
            .len();
        let attention = if count == 1 {
            "1 file needs attention".into()
        } else if count > 1 {
            format!("{count} files need attention")
        } else {
            "all files matched".into()
        };
        return format!("{phase} · {elapsed_seconds}s · {attention}");
    }
    format!("{phase} · {elapsed_seconds}s")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checks::{
        CheckCommandActivity,
        adapter::{
            AdapterProgress, TestSelection,
            events::{TestEvent, TestEventSink, TestPhase},
        },
    };
    use std::path::Path;

    /// Omit unknown formatting coverage until the adapter reports a file outcome.
    #[test]
    fn format_details_require_reported_progress() {
        for (total, reported, expected) in [
            (None, 0, None),
            (Some(40), 0, Some("   └─ 0/40 files · 0 unformatted")),
            (Some(40), 12, Some("   └─ 12/40 files · 0 unformatted")),
            (None, 12, Some("   └─ 12 file outcomes · 0 unformatted")),
        ] {
            let checks = CheckActivity {
                run_id: "run".into(),
                duration_ms: 0,
                started_at_ms: Some(0),
                completed_count: 0,
                total_count: 1,
                command_list: vec![CheckCommandActivity {
                    title: "Formatting".into(),
                    started_at_ms: Some(0),
                    completed_at_ms: None,
                    state: CheckState::Running,
                    output_line: None,
                    progress: Some(AdapterProgress::Format(
                        crate::checks::adapter::events::FormatProgress {
                            phase: None,
                            total,
                            reported,
                            unformatted: 0,
                        },
                    )),
                }],
            };
            let block = StatusView {
                id: "status",
                status: &SessionPhase::Idle,
                agent_count: 0,
                status_thoughts: false,
                checks: Some(&checks),
            }
            .render(&WidthProfile::default(), 2_000)
            .unwrap()
            .block
            .unwrap();
            let mut expected_rows = vec![
                "Checking · 0/1 finished · 2s",
                "└─ ⠋ Formatting · running · 2s",
            ];
            expected_rows.extend(expected);
            assert_eq!(block.text.wire_rows(), expected_rows);
        }
    }

    /// Keep build progress inline and replace it with counts when execution begins.
    #[test]
    fn build_message_renders_inline_until_test_execution() {
        let mut events =
            TestEventSink::new(TestSelection::default(), Path::new("unused-output")).unwrap();
        let mut checks = CheckActivity {
            run_id: "run".into(),
            duration_ms: 0,
            started_at_ms: Some(0),
            completed_count: 0,
            total_count: 1,
            command_list: vec![CheckCommandActivity {
                title: "Run Rust tests".into(),
                started_at_ms: Some(0),
                completed_at_ms: None,
                state: CheckState::Running,
                output_line: None,
                progress: None,
            }],
        };
        for (event, expected) in [
            (
                TestEvent::Phase {
                    name: TestPhase::Build,
                    message: None,
                },
                "   └─ Building",
            ),
            (
                TestEvent::Phase {
                    name: TestPhase::Build,
                    message: Some("Built forge-diff".into()),
                },
                "   └─ Building · Built forge-diff",
            ),
            (
                TestEvent::Plan {
                    cases: Vec::new(),
                    widened: None,
                    items: Vec::new(),
                },
                "   └─ Building · Built forge-diff",
            ),
            (
                TestEvent::Phase {
                    name: TestPhase::Run,
                    message: None,
                },
                "   └─ 0/0 tests · 0 failed",
            ),
        ] {
            events.accept(event).unwrap();
            checks.command_list[0].progress = Some(AdapterProgress::Tests(events.progress()));
            let rendered = StatusView {
                id: "status",
                status: &SessionPhase::Idle,
                agent_count: 0,
                status_thoughts: false,
                checks: Some(&checks),
            }
            .render(&WidthProfile::default(), 27_000)
            .unwrap();
            assert_eq!(
                rendered.block.unwrap().text.wire_rows(),
                [
                    "Checking · 0/1 finished · 27s",
                    "└─ ⠋ Run Rust tests · running · 27s",
                    expected,
                ]
            );
        }
    }

    /// Preserve each selected lifecycle and record the host payload consumed by the editor.
    #[test]
    fn selected_check_tree_matches_editor_recording() -> Result<()> {
        let mut run: crate::checks::CheckRun = serde_json::from_value(serde_json::json!({
            "id": "run", "owner": {"kind": "session", "session_id": "session"}, "revision": 0,
            "shell": "system", "executable": null, "workspace": ".", "environment_digest": "fixture",
            "started_at_ms": 0, "completed_at_ms": null, "state": "running",
            "progress": crate::plan::execution::SemanticProgress::default(),
            "workspace_identity": null, "workspace_digest": {},
            "commands": [
                {"id":"format", "command":"format", "state":"passed", "selected":true,
                 "started_at_ms":0, "completed_at_ms":1000, "exit_code":0, "error":null,
                 "stdout":"unused", "stderr":"unused", "output":"unused", "output_bytes":0, "preview":"formatted"},
                {"id":"lint", "command":"lint", "state":"failed", "selected":true,
                 "started_at_ms":1000, "completed_at_ms":3000, "exit_code":1, "error":"unknown field",
                 "stdout":"unused", "stderr":"unused", "output":"unused", "output_bytes":0, "preview":""},
                {"id":"tests", "command":"tests", "state":"running", "selected":true,
                 "started_at_ms":3000, "completed_at_ms":null, "exit_code":null, "error":null,
                 "stdout":"unused", "stderr":"unused", "output":"unused", "output_bytes":0, "preview":"Running session tests"},
                {"id":"build", "command":"build", "state":"pending", "selected":true,
                 "started_at_ms":null, "completed_at_ms":null, "exit_code":null, "error":null,
                 "stdout":"unused", "stderr":"unused", "output":"unused", "output_bytes":0, "preview":""},
                {"id":"excluded", "command":"excluded", "state":"skipped", "selected":false,
                 "started_at_ms":null, "completed_at_ms":null, "exit_code":null, "error":null,
                 "stdout":"unused", "stderr":"unused", "output":"unused", "output_bytes":0, "preview":""}
            ]
        }))?;
        let mut events = TestEventSink::new(TestSelection::default(), Path::new("unused-output"))?;
        events.accept(TestEvent::Phase {
            name: TestPhase::Build,
            message: Some("Built test targets".into()),
        })?;
        run.commands[2].adapter_progress = Some(AdapterProgress::Tests(events.progress()));
        let checks = CheckActivity::from_run(&run).unwrap();
        assert_eq!(checks.total_count, 4);
        assert_eq!(checks.completed_count, 2);
        assert_eq!(checks.command_list.len(), 4);
        assert_eq!(checks.command_list[0].elapsed_ms(18_000), Some(1000));
        let status = SessionPhase::Working {
            duration_ms: 0,
            started_at_ms: Some(0),
            activity: WorkflowActivity::Working,
            reasoning_summary: Some(
                "Reviewing the remaining diagnostics and checking 界界界 for a complete result"
                    .into(),
            ),
            execution: None,
        };
        let block = StatusView {
            id: "status",
            status: &status,
            agent_count: 0,
            status_thoughts: true,
            checks: Some(&checks),
        }
        .render(
            &WidthProfile {
                columns: 120,
                ..WidthProfile::default()
            },
            18_000,
        )?
        .block
        .unwrap();
        assert_eq!(
            &block.text.wire_rows()[..8],
            &[
                "Working · 18s",
                "└─ Checking · 2/4 finished · 18s",
                "   ├─ ✓ format · passed · 1s",
                "   ├─ ✗ lint · failed · 2s",
                "   │  └─ unknown field",
                "   ├─ ⠋ tests · running · 15s",
                "   │  ├─ Building · Built test targets",
                "   │  └─ Running session tests",
            ]
        );
        let document = BufferDocument::new(DocumentId("status-layout:status".into()), vec![block])?;
        forge_recorded_fixtures::assert_matches_recording(
            "status-layout.json",
            &serde_json::to_value(document.snapshot())?,
        );

        run.commands[3].state = CheckState::Running;
        run.commands[3].started_at_ms = Some(15_000);
        run.commands[3].preview = "Compiling forge".into();
        let checks = CheckActivity::from_run(&run).unwrap();
        let block = StatusView {
            id: "status",
            status: &SessionPhase::Idle,
            agent_count: 0,
            status_thoughts: false,
            checks: Some(&checks),
        }
        .render(&WidthProfile::default(), 18_000)?
        .block
        .unwrap();
        let rows = block.text.wire_rows();
        assert!(rows.contains(&"│  └─ Running session tests"));
        assert!(rows.contains(&"   └─ Compiling forge"));
        let markers = block
            .metadata
            .decoration
            .iter()
            .filter(|decoration| decoration.capture == "ForgeTimelineStatusSpinner")
            .collect::<Vec<_>>();
        assert_eq!(markers.len(), 2);
        for marker in markers {
            assert_eq!(
                &rows[marker.range.start.row][marker.range.start.column..marker.range.end.column],
                "⠋"
            );
        }
        Ok(())
    }
}
