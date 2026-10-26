use anyhow::Result;
use forge_buffer::block::{
    BufferBlock, Decoration, StatusHint, StatusPresentation, TargetRange, TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, TargetId};
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::projection::TranscriptAction;
use super::transcript::TranscriptRenderer;
use crate::checks::CheckActivity;
use crate::session::state_machine::{ExecutionStatus, SessionPhase, WorkflowActivity};

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
        let checking = checks.is_some() && matches!(status, SessionPhase::Idle);
        let text = if checking {
            "Checking".into()
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
        let text = if text.is_empty() {
            text
        } else {
            format!("\n{text}")
        };
        let mut block =
            TranscriptRenderer::new(width)?.literal(BlockId(id.to_owned()), &text, 0)?;
        block.metadata.status = Some(StatusPresentation {
            row: 1,
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
        let action = bind_primary_action(&mut block, id, status);
        if let Some(checks) = checks {
            append_checks(&mut block, width, checks, now_ms)?;
        }
        if status_thoughts
            && let SessionPhase::Working {
                reasoning_summary: Some(summary),
                ..
            } = status
        {
            append_reasoning_preview(&mut block, width, summary)?;
        }
        Ok(RenderedStatus { block, action })
    }
}

/// Status rows and their optional plan-review action.
pub(super) struct RenderedStatus {
    /// Complete status block, including independent check progress.
    pub block: BufferBlock,
    /// Action owned by the primary status line.
    pub action: Option<(TargetId, TranscriptAction)>,
}

fn primary_text(status: &SessionPhase, now_ms: i64) -> String {
    match status {
        SessionPhase::Idle => "Idle".into(),
        SessionPhase::Paused => "Paused".into(),
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

#[cfg(test)]
mod retry_tests {
    use super::*;

    #[test]
    fn retry_status_counts_down_then_keeps_attempt_visible() {
        let mut retry = crate::backend::retry::ProviderRetry {
            attempt: 2,
            max_attempts: 5,
            retry_at_ms: Some(6000),
        };
        let status = SessionPhase::RetryingProvider {
            retry: retry.clone(),
            activity: WorkflowActivity::Working,
        };
        assert_eq!(
            primary_text(&status, 1001),
            "Working · retry 2/5 · retrying in 5s"
        );
        assert!(status.animated());
        assert!(status.ticking());
        retry.retry_at_ms = None;
        let status = SessionPhase::RetryingProvider {
            retry,
            activity: WorkflowActivity::Planning,
        };
        assert_eq!(primary_text(&status, 6000), "Planning · retry 2/5");
        assert!(!status.ticking());
    }
}

fn bind_primary_action(
    block: &mut BufferBlock,
    id: &str,
    status: &SessionPhase,
) -> Option<(TargetId, TranscriptAction)> {
    let mut action = None;
    if let SessionPhase::AwaitingPlanReview { plan_id, .. } = status {
        block.metadata.decoration.push(Decoration {
            range: TextRange {
                start: TextPosition { row: 1, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
            capture: "ForgeHarnessPlan".into(),
            priority: 100,
        });
        let target = TargetId(format!("{id}:review-plan"));
        block.metadata.target.push(TargetRange {
            id: target.clone(),
            range: TextRange {
                start: TextPosition { row: 1, column: 0 },
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
                start: TextPosition { row: 1, column: 0 },
                end: TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                },
            },
        });
    }
    action
}

fn append_checks(
    block: &mut BufferBlock,
    width: &WidthProfile,
    checks: &CheckActivity,
    now_ms: i64,
) -> Result<()> {
    let mut lines = vec![format!(
        "Checks running · {}/{} finished · {}s",
        checks.completed_count,
        checks.total_count,
        checks.elapsed_ms(now_ms) / 1000
    )];
    for command in &checks.command_list {
        let elapsed = command.elapsed_ms(now_ms).map_or_else(
            || "starting".to_owned(),
            |elapsed| format!("{}s", elapsed / 1000),
        );
        lines.push(format!("  {} · {elapsed}", command.title));
        if let Some(output_line) = &command.output_line {
            lines.push(format!("    {output_line}"));
        }
    }
    let rendered = TranscriptRenderer::new(width)?.literal(
        BlockId(format!("{}:checks", block.id.0)),
        &lines.join("\n"),
        0,
    )?;
    let start = block.text.row_count();
    let mut rows = block.text.wire_rows();
    rows.extend(rendered.text.wire_rows());
    block.text = BufferText::from_rows(rows)?;
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
        capture: "ForgeStatusHint".into(),
        priority: 100,
    });
    Ok(())
}

fn finalization_label(exchange_outcome: Option<crate::exchange::ExchangeState>) -> &'static str {
    if exchange_outcome == Some(crate::exchange::ExchangeState::Cancelled) {
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
    let first = rendered.block.text.row(0).unwrap_or("");
    let preview = if rendered.block.text.row_count() > 1 || width.cells(first, 0)? > width.columns {
        let suffix = ["...", "..", ".", ""]
            .into_iter()
            .find(|suffix| {
                width
                    .cells(suffix, 0)
                    .is_ok_and(|cells| cells <= width.columns)
            })
            .unwrap_or("");
        let available = width.columns.saturating_sub(width.cells(suffix, 0)?);
        let prefix = if available == 0 {
            ""
        } else {
            let mut preview_width = width.clone();
            preview_width.columns = available;
            let (end, _) = preview_width.wrapped_prefix(first, 1, 0)?;
            let prefix = &first[..end];
            if width.cells(prefix, 0)? <= available {
                prefix.trim_end()
            } else {
                ""
            }
        };
        format!("{prefix}{suffix}")
    } else {
        first.to_owned()
    };
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
