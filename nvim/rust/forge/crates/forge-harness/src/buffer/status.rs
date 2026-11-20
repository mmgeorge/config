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
use crate::timeline::StatusAgent;

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
            agents,
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
                    agents,
                    status_thoughts: *status_thoughts,
                    checks: checks.as_ref(),
                }
                .render(width, now_ms)?,
            ),
        })
    }
}

impl StatusProjection {
    /// Record when a running label of a status entry next shows a new value.
    pub fn label_due(
        entry: &crate::timeline::TimelineEntry,
        now_ms: i64,
        due: &mut super::duration::LabelDue,
    ) {
        if let crate::timeline::TimelineEntry::Status {
            id,
            status,
            agents,
            status_thoughts,
            checks,
            ..
        } = entry
        {
            StatusView {
                id,
                status,
                agents,
                status_thoughts: *status_thoughts,
                checks: checks.as_ref(),
            }
            .label_due(now_ms, due);
        }
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
    /// Unfinished managed child exchanges across this session, in delegation order.
    pub agents: &'source [StatusAgent],
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
            agents,
            status_thoughts,
            checks,
        } = *self;
        let idle = matches!(status, SessionPhase::Idle);
        if idle && checks.is_none() && agents.is_empty() {
            return Ok(RenderedStatus {
                block: None,
                action: None,
            });
        }
        let checking = checks.is_some() && idle;
        // An idle root with only running children has no phase of its own to report, so the
        // agents heading takes the primary row, as the check heading does for idle checks.
        let delegating = idle && checks.is_none();
        let text = if checking {
            check_heading(checks.expect("checking requires check activity"), now_ms)
        } else if delegating {
            agents_heading(agents)
        } else {
            primary_text(status, now_ms)
        };
        let mut block =
            TranscriptRenderer::new(width)?.literal(BlockId(id.to_owned()), &text, 0)?;
        block.metadata.status = Some(StatusPresentation {
            row: 0,
            animated: checking || delegating || status.animated(),
            handoff: matches!(
                status,
                SessionPhase::Working {
                    activity: WorkflowActivity::HandingOff,
                    ..
                }
            ),
            hint: if checking || delegating {
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
            append_checks(&mut block, width, checks, status, agents.is_empty(), now_ms)?;
        }
        if !agents.is_empty() {
            // The primary row already names running agents when the root is idle or waiting for
            // them, so only a root with other work introduces the branch with its own heading.
            let heading = !delegating
                && (checks.is_some() || !matches!(status, SessionPhase::WaitingForAgent { .. }));
            append_agents(&mut block, agents, heading, now_ms)?;
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

impl StatusView<'_> {
    /// Record the next change of each running clock that `render` formats.
    ///
    /// Each clause names the clock of one label: the primary phase row, the provider retry
    /// countdown, the check heading and running commands, and each child agent with its tools.
    fn label_due(&self, now_ms: i64, due: &mut super::duration::LabelDue) {
        match self.status {
            SessionPhase::Working {
                started_at_ms: Some(_),
                ..
            }
            | SessionPhase::RetryingPlanGeneration {
                started_at_ms: Some(_),
                ..
            }
            | SessionPhase::Compacting { .. } => {
                due.elapsed(now_ms, self.status.elapsed_ms(now_ms));
            }
            SessionPhase::RetryingProvider {
                retry:
                    crate::provider::retry::ProviderRetry {
                        retry_at_ms: Some(deadline),
                        ..
                    },
                ..
            } => due.countdown(now_ms, *deadline),
            _ => {}
        }
        if let Some(checks) = self.checks {
            if checks.started_at_ms.is_some() {
                due.elapsed(now_ms, checks.elapsed_ms(now_ms));
            }
            for command in &checks.command_list {
                if command.state == CheckState::Running
                    && let Some(elapsed) = command.elapsed_ms(now_ms)
                {
                    due.elapsed(now_ms, elapsed);
                }
            }
        }
        for agent in self.agents {
            if agent.starting {
                continue;
            }
            due.elapsed(now_ms, agent.elapsed_ms(now_ms));
            if let Some(tool_ms) = agent.tool_ms(now_ms) {
                due.elapsed(now_ms, tool_ms);
            }
        }
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
        SessionPhase::Paused { waiting: None } => "Paused".into(),
        SessionPhase::Paused {
            waiting: Some(crate::session::state_machine::PausedWait::PlanReview { .. }),
        } => "Paused · plan review waits · /task resume".into(),
        SessionPhase::Blocked { reason } => {
            format!("Blocked · {reason} · /task resume · /task clear")
        }
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
        // The hand-off has no clock of its own; its successor publishes the first one.
        SessionPhase::Working {
            activity: WorkflowActivity::HandingOff,
            ..
        } => WorkflowActivity::HandingOff.active_label().into(),
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

/// Render one row per selected command, with live progress and failure causes on that row.
///
/// The run selects its commands when it starts, so the check tree reaches its height in one
/// step and each command changes its text in place as it starts, reports progress, and finishes.
/// Detail rows below each command once appeared and disappeared with every start and pass, and
/// the status split changed height several times a second, scrolling the transcript beside it.
/// A row wider than the window still wraps, so no detail is lost.
fn append_checks(
    block: &mut BufferBlock,
    width: &WidthProfile,
    checks: &CheckActivity,
    status: &SessionPhase,
    last_branch: bool,
    now_ms: i64,
) -> Result<()> {
    let nested = !matches!(status, SessionPhase::Idle);
    // A following agents branch keeps the stem open beside the check commands.
    let root = match (nested, last_branch) {
        (false, _) => "",
        (true, true) => "   ",
        (true, false) => "│  ",
    };
    let mut lines = Vec::new();
    let mut running_rows = Vec::new();
    if nested {
        let branch = if last_branch { "└─" } else { "├─" };
        lines.push(format!("{branch} {}", check_heading(checks, now_ms)));
    }
    for (command_index, command) in checks.command_list.iter().enumerate() {
        let last = command_index + 1 == checks.command_list.len();
        let branch = if last { "└─" } else { "├─" };
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
        let mut details = Vec::new();
        if let Some(selection) = &command.selection {
            details.push(selection.clone());
        }
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
        let details: String = details
            .iter()
            .map(|detail| format!(" · {detail}"))
            .collect();
        lines.push(format!(
            "{root}{branch} {marker} {} · {outcome}{elapsed}{details}",
            command.title
        ));
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

/// Count running children for the agents branch heading.
fn agents_heading(agents: &[StatusAgent]) -> String {
    format!("Agents · {} running", agents.len())
}

/// Render one row per unfinished child: elapsed time, identity, tool progress, and reasoning.
///
/// Rows stay unwrapped so the spinner and reasoning columns keep exact byte positions; the
/// editor clips each row carrying a reasoning segment to its window. The reasoning segment uses
/// its own capture so the editor never merges a child's thought into the root preview.
fn append_agents(
    block: &mut BufferBlock,
    agents: &[StatusAgent],
    heading: bool,
    now_ms: i64,
) -> Result<()> {
    let mut rows = block
        .text
        .wire_rows()
        .into_iter()
        .map(str::to_owned)
        .collect::<Vec<_>>();
    let start = rows.len();
    let root = if heading {
        rows.push(format!("└─ {}", agents_heading(agents)));
        "   "
    } else {
        ""
    };
    for (index, agent) in agents.iter().enumerate() {
        let last = agents[index + 1..]
            .iter()
            .take_while(|later| later.depth >= agent.depth)
            .all(|later| later.depth > agent.depth);
        let branch = if last { "└─" } else { "├─" };
        let indent = "   ".repeat(agent.depth);
        let elapsed = if agent.starting {
            "starting".to_owned()
        } else {
            format!("{}s", agent.elapsed_ms(now_ms) / 1000)
        };
        let prefix = format!("{root}{indent}{branch} ");
        let mut row = format!(
            "{prefix}⠋ {elapsed} {}{}",
            agent.label, agent.selection_label
        );
        if let Some(tools) = super::projection::tool_count_label(
            super::projection::ToolTally {
                successful: agent.tool_completed,
                denied: agent.tool_denied,
                total: agent.tool_total,
            },
            agent.tool_ms(now_ms),
        ) {
            row.push_str(&format!(" │ {tools}"));
        }
        let spinner = prefix.len();
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition {
                    row: rows.len(),
                    column: spinner,
                },
                end: TextPosition {
                    row: rows.len(),
                    column: spinner + "⠋".len(),
                },
            },
            capture: "ForgeTimelineStatusSpinner".into(),
            priority: 110,
        });
        let thought = agent
            .reasoning_summary
            .as_deref()
            .map(|summary| summary.split_whitespace().collect::<Vec<_>>().join(" "))
            .filter(|summary| !summary.is_empty());
        if let Some(thought) = thought {
            row.push_str(" │ ");
            let column = row.len();
            row.push_str(&thought);
            block.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition {
                        row: rows.len(),
                        column,
                    },
                    end: TextPosition {
                        row: rows.len(),
                        column: row.len(),
                    },
                },
                capture: "ForgeHarnessAgentThought".into(),
                priority: 105,
            });
        }
        rows.push(row);
    }
    block.metadata.decoration.push(Decoration {
        range: TextRange {
            start: TextPosition {
                row: start,
                column: 0,
            },
            end: TextPosition {
                row: rows.len(),
                column: 0,
            },
        },
        capture: "ForgeStatusHint".into(),
        priority: 100,
    });
    block.text = BufferText::from_rows(rows)?;
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

    /// Keep the check tree at one height while its commands start, report progress, and finish.
    ///
    /// Detail rows under each command appeared when it started and disappeared when it passed,
    /// so the status split grew and shrank several times within a second of checks starting.
    #[test]
    fn check_tree_height_is_fixed_by_its_selected_commands() -> Result<()> {
        let command = |title: &str, state: CheckState| CheckCommandActivity {
            title: title.into(),
            selection: None,
            started_at_ms: (state != CheckState::Pending).then_some(0),
            completed_at_ms: matches!(state, CheckState::Passed | CheckState::Failed)
                .then_some(1_000),
            state,
            output_line: None,
            progress: None,
        };
        let diagnostics =
            AdapterProgress::Diagnostics(serde_json::from_value(serde_json::json!({
                "targets_finished": 0, "targets_total": null, "errors": 0, "warnings": 0
            }))?);
        let mut typecheck_running = command("Type check", CheckState::Running);
        typecheck_running.progress = Some(diagnostics);
        let mut tests_queued = command("Tests", CheckState::Pending);
        tests_queued.selection = Some("2 tests selected".into());
        let mut tests_running = command("Tests", CheckState::Running);
        tests_running.selection = Some("2 tests selected".into());
        let mut events = TestEventSink::new(TestSelection::default(), Path::new("unused-output"))?;
        events.accept(TestEvent::Phase {
            name: TestPhase::Run,
            message: None,
        })?;
        tests_running.progress = Some(AdapterProgress::Tests(events.progress()));
        let mut tests_passed = command("Tests", CheckState::Passed);
        tests_passed.selection = Some("2 tests selected".into());
        let mut lint_failed = command("Lint", CheckState::Failed);
        lint_failed.output_line = Some("unknown field".into());
        let stages = [
            vec![
                command("Type check", CheckState::Pending),
                command("Tests", CheckState::Pending),
                command("Lint", CheckState::Pending),
            ],
            vec![
                typecheck_running,
                tests_queued.clone(),
                command("Lint", CheckState::Pending),
            ],
            vec![
                command("Type check", CheckState::Passed),
                tests_running,
                command("Lint", CheckState::Pending),
            ],
            vec![
                command("Type check", CheckState::Passed),
                tests_passed.clone(),
                command("Lint", CheckState::Running),
            ],
            vec![
                command("Type check", CheckState::Passed),
                tests_passed,
                lint_failed,
            ],
        ];
        let working = SessionPhase::Working {
            duration_ms: 0,
            started_at_ms: Some(0),
            activity: WorkflowActivity::Checking,
            reasoning_summary: None,
            execution: None,
        };
        for status in [&SessionPhase::Idle, &working] {
            let mut heights = Vec::new();
            for command_list in &stages {
                let checks = CheckActivity {
                    run_id: "run".into(),
                    duration_ms: 0,
                    started_at_ms: Some(0),
                    completed_count: 0,
                    total_count: command_list.len(),
                    command_list: command_list.clone(),
                };
                let block = StatusView {
                    id: "status",
                    status,
                    agents: &[],
                    status_thoughts: false,
                    checks: Some(&checks),
                }
                .render(&WidthProfile::default(), 2_000)?
                .block
                .unwrap();
                heights.push(block.text.row_count());
            }
            assert!(
                heights.iter().all(|height| *height == heights[0]),
                "the check tree changed height as its commands progressed: {heights:?}"
            );
        }
        Ok(())
    }

    /// A blocked task's status names the cause and both commands that can follow it, and it
    /// keeps no animation or key hint, as nothing runs until the user acts.
    #[test]
    fn blocked_status_names_its_cause_and_commands() -> Result<()> {
        use anyhow::Context as _;
        let status = SessionPhase::Blocked {
            reason: "5 consecutive turns changed no files".into(),
        };
        let block = StatusView {
            id: "status",
            status: &status,
            agents: &[],
            status_thoughts: false,
            checks: None,
        }
        .render(&WidthProfile::default(), 0)?
        .block
        .context("a blocked task rendered no status")?;
        assert_eq!(
            block.text.wire_rows().first().copied(),
            Some("Blocked · 5 consecutive turns changed no files · /task resume · /task clear")
        );
        let presentation = block.metadata.status.context("status row metadata")?;
        assert!(!presentation.animated);
        assert!(presentation.hint.is_none());
        Ok(())
    }

    /// Omit unknown formatting coverage until the adapter reports a file outcome.
    #[test]
    fn format_details_require_reported_progress() {
        for (total, reported, expected) in [
            (None, 0, ""),
            (Some(40), 0, " · 0/40 files · 0 unformatted"),
            (Some(40), 12, " · 12/40 files · 0 unformatted"),
            (None, 12, " · 12 file outcomes · 0 unformatted"),
        ] {
            let checks = CheckActivity {
                run_id: "run".into(),
                duration_ms: 0,
                started_at_ms: Some(0),
                completed_count: 0,
                total_count: 1,
                command_list: vec![CheckCommandActivity {
                    title: "Formatting".into(),
                    selection: None,
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
                agents: &[],
                status_thoughts: false,
                checks: Some(&checks),
            }
            .render(&WidthProfile::default(), 2_000)
            .unwrap()
            .block
            .unwrap();
            assert_eq!(
                block.text.wire_rows(),
                [
                    "Checking · 0/1 finished · 2s".to_owned(),
                    format!("└─ ⠋ Formatting · running · 2s{expected}"),
                ]
            );
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
                selection: None,
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
                " · Building",
            ),
            (
                TestEvent::Phase {
                    name: TestPhase::Build,
                    message: Some("Built forge-diff".into()),
                },
                " · Building · Built forge-diff",
            ),
            (
                TestEvent::Plan {
                    cases: Vec::new(),
                    widened: None,
                    items: Vec::new(),
                },
                " · Building · Built forge-diff",
            ),
            (
                TestEvent::Phase {
                    name: TestPhase::Run,
                    message: None,
                },
                " · 0/0 tests · 0 failed",
            ),
        ] {
            events.accept(event).unwrap();
            checks.command_list[0].progress = Some(AdapterProgress::Tests(events.progress()));
            let rendered = StatusView {
                id: "status",
                status: &SessionPhase::Idle,
                agents: &[],
                status_thoughts: false,
                checks: Some(&checks),
            }
            .render(&WidthProfile::default(), 27_000)
            .unwrap();
            assert_eq!(
                rendered.block.unwrap().text.wire_rows(),
                [
                    "Checking · 0/1 finished · 27s".to_owned(),
                    format!("└─ ⠋ Run Rust tests · running · 27s{expected}"),
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
        run.commands[3].context = Some(serde_json::from_value(serde_json::json!({
            "purpose":"verify", "definition_id":"lua-host-test", "name":"Lua host tests",
            "area":"tests", "reason":"explicit selection", "matched_files":[],
            "category":"test", "directory":".", "depends_on":[], "resolved":null,
            "selection":{"kind":"tests", "tests":{"ids":[
                "nvim/lua/forge/tests/integration/views/harness/harness.test.lua::skill_marker_aligns_with_commentary"
            ], "affected":[]}}
        }))?);
        let checks = CheckActivity::from_run(&run).unwrap();
        assert_eq!(checks.total_count, 4);
        assert_eq!(checks.completed_count, 2);
        assert_eq!(checks.command_list.len(), 4);
        assert_eq!(checks.command_list[3].title, "Lua host tests");
        assert_eq!(
            checks.command_list[3].selection.as_deref(),
            Some("skill_marker_aligns_with_commentary")
        );
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
            agents: &[],
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
            &block.text.wire_rows()[..6],
            &[
                "Working · 18s",
                "└─ Checking · 2/4 finished · 18s",
                "   ├─ ✓ format · passed · 1s",
                "   ├─ ✗ lint · failed · 2s · unknown field",
                "   ├─ ⠋ tests · running · 15s · Building · Built test targets · Running session tests",
                "   └─ ○ Lua host tests · queued · skill_marker_aligns_with_commentary",
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
            agents: &[],
            status_thoughts: false,
            checks: Some(&checks),
        }
        .render(&WidthProfile::default(), 18_000)?
        .block
        .unwrap();
        let rows = block.text.wire_rows();
        let text = rows.join(
            "
",
        );
        assert!(
            text.contains(
                "└─ ⠋ Lua host tests · running · 3s · skill_marker_aligns_with_commentary"
            ),
            "{rows:#?}"
        );
        assert!(text.contains("Compiling forge"), "{rows:#?}");
        assert!(
            text.contains("├─ ⠋ tests · running · 15s · Building · Built test targets"),
            "{rows:#?}"
        );
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
        let context = run.commands[3].context.as_mut().unwrap();
        let crate::checks::CheckSelection::Tests { tests } = &mut context.selection else {
            unreachable!("fixture selects tests")
        };
        tests
            .ids
            .push("other.test.lua::another_case".to_owned().try_into()?);
        let checks = CheckActivity::from_run(&run).unwrap();
        assert_eq!(checks.command_list[3].title, "Lua host tests");
        assert_eq!(
            checks.command_list[3].selection.as_deref(),
            Some("2 tests selected")
        );
        Ok(())
    }

    fn running_agent(
        label: &str,
        depth: usize,
        tools: (usize, usize),
        reasoning: Option<&str>,
    ) -> StatusAgent {
        let mut metrics = crate::exchange::ExchangeMetrics::default();
        metrics.timing_complete = true;
        metrics.tool_duration_ms = 2_400;
        StatusAgent {
            exchange_id: format!("{label}:exchange"),
            depth,
            label: label.into(),
            selection_label: " · codex · gpt-6-luna +".into(),
            starting: false,
            clock: crate::exchange::ExchangeClock::Running {
                elapsed_ms: 0,
                started_at_ms: 1_000,
            },
            metrics,
            tool_completed: tools.0,
            tool_denied: 0,
            tool_total: tools.1,
            reasoning_summary: reasoning.map(str::to_owned),
        }
    }

    /// Render running children beside checks with their tool summary and reasoning only.
    #[test]
    fn running_agents_render_tool_summary_and_reasoning() -> Result<()> {
        let agents = vec![
            running_agent(
                "code-explorer-1",
                0,
                (6, 6),
                Some("Tracing   reservation admission"),
            ),
            running_agent("code-explorer-2", 0, (12, 13), None),
            running_agent("nested-reviewer", 1, (0, 0), None),
        ];
        let status = SessionPhase::Working {
            duration_ms: 0,
            started_at_ms: Some(0),
            activity: WorkflowActivity::Working,
            reasoning_summary: Some("Waiting on the explorers".into()),
            execution: None,
        };
        let block = StatusView {
            id: "status",
            status: &status,
            agents: &agents,
            status_thoughts: true,
            checks: None,
        }
        .render(&WidthProfile::default(), 44_000)?
        .block
        .unwrap();
        let rows = block.text.wire_rows();
        assert_eq!(
            &rows[..5],
            &[
                "Working · 44s",
                "└─ Agents · 3 running",
                "   ├─ ⠋ 43s code-explorer-1 · codex · gpt-6-luna + │ 6/6 tools · 2.4s │ Tracing reservation admission",
                "   └─ ⠋ 43s code-explorer-2 · codex · gpt-6-luna + │ 12/13 tools · 2.4s",
                "      └─ ⠋ 43s nested-reviewer · codex · gpt-6-luna +",
            ]
        );
        let thoughts = block
            .metadata
            .decoration
            .iter()
            .filter(|decoration| decoration.capture == "ForgeHarnessAgentThought")
            .collect::<Vec<_>>();
        assert_eq!(thoughts.len(), 1);
        let thought = thoughts[0];
        assert_eq!(
            &rows[thought.range.start.row][thought.range.start.column..thought.range.end.column],
            "Tracing reservation admission"
        );
        let spinners = block
            .metadata
            .decoration
            .iter()
            .filter(|decoration| decoration.capture == "ForgeTimelineStatusSpinner")
            .collect::<Vec<_>>();
        assert_eq!(spinners.len(), 3);
        for spinner in spinners {
            assert_eq!(
                &rows[spinner.range.start.row]
                    [spinner.range.start.column..spinner.range.end.column],
                "⠋"
            );
        }
        let document = BufferDocument::new(DocumentId("status-agents:status".into()), vec![block])?;
        forge_recorded_fixtures::assert_matches_recording(
            "status-agents.json",
            &serde_json::to_value(document.snapshot())?,
        );
        Ok(())
    }

    /// Keep the check stem open when an agents branch follows it.
    #[test]
    fn agents_branch_follows_an_open_check_stem() -> Result<()> {
        let checks = CheckActivity {
            run_id: "run".into(),
            duration_ms: 0,
            started_at_ms: Some(0),
            completed_count: 0,
            total_count: 1,
            command_list: vec![CheckCommandActivity {
                title: "format".into(),
                selection: None,
                started_at_ms: Some(0),
                completed_at_ms: None,
                state: CheckState::Running,
                output_line: None,
                progress: None,
            }],
        };
        let agents = vec![running_agent("explorer", 0, (1, 1), None)];
        let status = SessionPhase::Working {
            duration_ms: 0,
            started_at_ms: Some(0),
            activity: WorkflowActivity::Working,
            reasoning_summary: None,
            execution: None,
        };
        let block = StatusView {
            id: "status",
            status: &status,
            agents: &agents,
            status_thoughts: false,
            checks: Some(&checks),
        }
        .render(&WidthProfile::default(), 3_000)?
        .block
        .unwrap();
        assert_eq!(
            block.text.wire_rows(),
            [
                "Working · 3s",
                "├─ Checking · 0/1 finished · 3s",
                "│  └─ ⠋ format · running · 3s",
                "└─ Agents · 1 running",
                "   └─ ⠋ 2s explorer · codex · gpt-6-luna + │ 1/1 tools · 2.4s",
            ]
        );
        Ok(())
    }

    /// Let the primary row name the agents when the root has no phase of its own to show.
    #[test]
    fn idle_and_waiting_roots_list_agents_without_a_second_heading() -> Result<()> {
        let agents = vec![running_agent("explorer", 0, (0, 0), None)];
        for (status, heading) in [
            (SessionPhase::Idle, "Agents · 1 running"),
            (
                SessionPhase::WaitingForAgent { agent_count: 1 },
                "Waiting for agents · 1 running",
            ),
        ] {
            let block = StatusView {
                id: "status",
                status: &status,
                agents: &agents,
                status_thoughts: false,
                checks: None,
            }
            .render(&WidthProfile::default(), 3_000)?
            .block
            .unwrap();
            assert_eq!(
                block.text.wire_rows(),
                [heading, "└─ ⠋ 2s explorer · codex · gpt-6-luna +"]
            );
            assert!(block.metadata.status.as_ref().unwrap().animated);
        }
        Ok(())
    }

    /// Keep one animated row from the interrupt until settlement.
    ///
    /// Native collection usually ends within a few hundred milliseconds, so a row that renamed
    /// itself and dropped its spinner when host settlement began changed twice under the reader.
    /// A cleanup that outlives its response deadline is the only state that reads differently.
    #[test]
    fn stopping_reads_the_same_through_collection_and_settlement() -> Result<()> {
        use crate::exchange::ExchangeCollection;
        for (collection, text, animated) in [
            (ExchangeCollection::Pending, "Stopping", true),
            (ExchangeCollection::Settling, "Stopping", true),
            (
                ExchangeCollection::TimedOut,
                "Cleanup timed out - provider state uncertain",
                false,
            ),
        ] {
            let status = SessionPhase::Stopping { collection };
            let block = StatusView {
                id: "status",
                status: &status,
                agents: &[],
                status_thoughts: false,
                checks: None,
            }
            .render(&WidthProfile::default(), 3_000)?
            .block
            .unwrap();
            assert_eq!(block.text.wire_rows(), [text]);
            assert_eq!(block.metadata.status.as_ref().unwrap().animated, animated);
        }
        Ok(())
    }

    /// Mark the predicted hand-off so the editor can tell it from ordinary work.
    ///
    /// The hand-off reads `Working` with no clock: its successor may be a new exchange whose
    /// clock starts at zero or an open exchange that continues its own, as a plan revision
    /// does, and a guessed `0s` jumped to the continued exchange's total a moment later. The
    /// flag tells the editor that the exchange that just settled is followed by a queued turn,
    /// which keeps the editor from revealing the finished exchange a moment before the
    /// successor's output replaces it.
    #[test]
    fn hand_off_reads_working_without_a_clock_and_carries_the_hand_off_flag() -> Result<()> {
        let render = |activity| -> Result<BufferBlock> {
            let status = SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: None,
                activity,
                reasoning_summary: None,
                execution: None,
            };
            Ok(StatusView {
                id: "status",
                status: &status,
                agents: &[],
                status_thoughts: false,
                checks: None,
            }
            .render(&WidthProfile::default(), 3_000)?
            .block
            .unwrap())
        };
        let hand_off = render(WorkflowActivity::HandingOff)?;
        let working = render(WorkflowActivity::Working)?;
        assert_eq!(hand_off.text.wire_rows(), ["Working"]);
        assert_eq!(working.text.wire_rows(), ["Working · 0s"]);
        assert!(hand_off.metadata.status.as_ref().unwrap().handoff);
        assert!(!working.metadata.status.as_ref().unwrap().handoff);
        let document =
            BufferDocument::new(DocumentId("status-handoff:status".into()), vec![hand_off])?;
        forge_recorded_fixtures::assert_matches_recording(
            "status-handoff.json",
            &serde_json::to_value(document.snapshot())?,
        );
        Ok(())
    }
}
