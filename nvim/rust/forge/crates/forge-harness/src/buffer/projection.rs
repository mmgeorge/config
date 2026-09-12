use std::collections::HashMap;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BufferBlock, Decoration, FoldRange, Gutter, TargetRange, TextChunk, TextPosition,
    TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::width::WidthProfile;
use serde::Serialize;

use crate::interaction::{
    InteractionKind, InteractionNode, InteractionRecord, InteractionState, MainSegment,
    SegmentState,
};
use crate::session::state_machine::SessionPhase;
use crate::timeline::{PlanExecutionTimelineItem, SessionEventKind, TimelineEntry};

use super::document::TranscriptEntry;
use super::tool::ToolOutputView;
use super::transcript::TranscriptRenderer;

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TranscriptAction {
    Tool { call_id: String },
    Diff { text: String },
    Plan { plan_id: String },
    Agent { run_id: String },
    Session { session_id: String },
    Url { url: String },
}

pub struct ProjectedEntry {
    pub entry: TranscriptEntry,
    pub prompt: Vec<BlockId>,
    pub action: HashMap<TargetId, TranscriptAction>,
    pub tool: HashMap<String, ToolOutputView>,
}

impl ProjectedEntry {
    pub fn retained_bytes(&self) -> usize {
        self.entry
            .block
            .iter()
            .map(|block| block.text.byte_count())
            .sum::<usize>()
            + self
                .prompt
                .iter()
                .map(|identity| identity.0.len() + 32)
                .sum::<usize>()
            + self
                .tool
                .values()
                .map(ToolOutputView::retained_bytes)
                .sum::<usize>()
            + self
                .action
                .values()
                .map(|action| match action {
                    TranscriptAction::Diff { text } => text.len(),
                    _ => 512,
                })
                .sum::<usize>()
    }
}

struct Projection<'profile> {
    renderer: TranscriptRenderer<'profile>,
    now_ms: i64,
    block: Vec<BufferBlock>,
    prompt: Vec<BlockId>,
    action: HashMap<TargetId, TranscriptAction>,
    tool: HashMap<String, ToolOutputView>,
    bytes: usize,
    leading_separator: bool,
}

pub fn project(
    entry: &TimelineEntry,
    width: &WidthProfile,
    leading_separator: bool,
) -> Result<ProjectedEntry> {
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| {
            duration.as_millis().min(i64::MAX as u128) as i64
        });
    project_at_with_separator(entry, width, now_ms, leading_separator)
}

#[cfg(test)]
fn project_at(entry: &TimelineEntry, width: &WidthProfile, now_ms: i64) -> Result<ProjectedEntry> {
    project_at_with_separator(entry, width, now_ms, false)
}

fn project_at_with_separator(
    entry: &TimelineEntry,
    width: &WidthProfile,
    now_ms: i64,
    leading_separator: bool,
) -> Result<ProjectedEntry> {
    let mut projection = Projection {
        renderer: TranscriptRenderer::new(width)?,
        now_ms,
        block: Vec::new(),
        prompt: Vec::new(),
        action: HashMap::new(),
        tool: HashMap::new(),
        bytes: 0,
        leading_separator,
    };
    projection.entry(entry, 0)?;
    if projection.block.is_empty() {
        projection.literal(&format!("{}:empty", entry.id()), "", None)?;
    }
    Ok(ProjectedEntry {
        entry: TranscriptEntry {
            id: entry.id(),
            block: projection.block,
        },
        action: projection.action,
        prompt: projection.prompt,
        tool: projection.tool,
    })
}

impl Projection<'_> {
    fn entry(&mut self, entry: &TimelineEntry, depth: usize) -> Result<()> {
        ensure!(depth <= 16, "agent transcript nesting exceeds 16 levels");
        match entry {
            TimelineEntry::Interaction {
                interaction,
                agent_by_id,
                ..
            } => self.interaction(interaction, agent_by_id, depth)?,
            TimelineEntry::SessionEvent { id, event, .. } => {
                let text = format!("  {}", session_event_text(&event.detail));
                let action = match &event.detail {
                    SessionEventKind::Forked {
                        source_session_id, ..
                    } => Some(TranscriptAction::Session {
                        session_id: source_session_id.clone(),
                    }),
                    _ => None,
                };
                self.literal(id, &text, action)?;
            }
            TimelineEntry::Status { id, status, .. } => {
                let text = match status {
                    SessionPhase::Idle => String::new(),
                    SessionPhase::Working { started_at_ms, .. } => {
                        let elapsed_seconds =
                            self.now_ms.saturating_sub(*started_at_ms).max(0) / 1_000;
                        format!("Working ({elapsed_seconds}s)")
                    }
                    SessionPhase::AwaitingInput { .. } => "Awaiting input".into(),
                    SessionPhase::AwaitingPlanReview { revision, .. } => {
                        format!("Awaiting plan review · revision {revision}")
                    }
                    SessionPhase::RetryingPlanGeneration { turn, max_turn, .. } => {
                        format!("Revising plan · attempt {turn}/{max_turn}")
                    }
                    SessionPhase::PlanningFailed { turn_count, .. } => {
                        format!("Plan generation failed after {turn_count} attempts")
                    }
                    SessionPhase::WaitingForAgent { agent_count } => {
                        format!("Waiting for {agent_count} agents")
                    }
                };
                let text = if text.is_empty() {
                    text
                } else {
                    format!("  {text}")
                };
                self.literal(id, &text, None)?;
            }
            TimelineEntry::AgentLifecycle {
                id,
                run,
                interaction,
                agent,
                ..
            } => {
                let heading_start = self.block.len();
                self.literal(
                    id,
                    &agent_summary(run, self.now_ms),
                    Some(TranscriptAction::Agent {
                        run_id: run.id.clone(),
                    }),
                )?;
                self.indent_range(heading_start, depth.saturating_sub(1));
                let task_start = self.block.len();
                self.literal(&format!("{id}:task"), &format!("  ↳ {}", run.task), None)?;
                self.indent_range(task_start, depth.saturating_sub(1));
                for interaction in interaction {
                    let interaction_start = self.block.len();
                    self.interaction(interaction, &HashMap::new(), depth + 1)?;
                    self.indent_range(interaction_start, depth);
                }
                for agent in agent {
                    self.entry(agent, depth + 1)?;
                }
            }
            TimelineEntry::PlanLifecycle {
                id,
                plan,
                lifecycle,
                ..
            } => {
                self.literal(
                    id,
                    &format!(
                        "{} · {:?} · revision {}",
                        plan.title, lifecycle.kind, lifecycle.model_revision
                    ),
                    Some(TranscriptAction::Plan {
                        plan_id: plan.id.clone(),
                    }),
                )?;
                if let Some(comment) = &lifecycle.overall_comment {
                    self.markdown(&format!("{id}:comment"), comment)?;
                }
                if let Some(question) = &lifecycle.question {
                    for (index, question) in question.questions.iter().enumerate() {
                        self.markdown(&format!("{id}:question:{index}"), &question.question)?;
                        for (option, value) in question.options.iter().enumerate() {
                            self.literal(
                                &format!("{id}:question:{index}:option:{option}"),
                                &format!("{}: {}", value.label, value.description),
                                None,
                            )?;
                        }
                    }
                }
                if let Some(answer) = &lifecycle.answer {
                    self.markdown(&format!("{id}:answer"), answer)?;
                }
                for (index, annotation) in lifecycle.annotation.iter().enumerate() {
                    self.markdown(
                        &format!("{id}:annotation:{index}"),
                        &format!("{}\n{}", annotation.label, annotation.body),
                    )?;
                }
            }
            TimelineEntry::PlanExecution { id, plan, item, .. } => {
                self.literal(
                    id,
                    &plan.title,
                    Some(TranscriptAction::Plan {
                        plan_id: plan.id.clone(),
                    }),
                )?;
                for (index, item) in item.iter().enumerate() {
                    match item {
                        PlanExecutionTimelineItem::Interaction { interaction } => {
                            self.interaction(interaction, &HashMap::new(), depth)?
                        }
                        PlanExecutionTimelineItem::TaskStarted {
                            ordinal,
                            total,
                            title,
                            ..
                        } => self.literal(
                            &format!("{id}:task:{index}"),
                            &format!("Task {ordinal}/{total}: {title}"),
                            None,
                        )?,
                        PlanExecutionTimelineItem::TaskCompleted {
                            ordinal,
                            total,
                            title,
                            elapsed_ms,
                            ..
                        } => self.literal(
                            &format!("{id}:task:{index}"),
                            &format!("Completed {ordinal}/{total}: {title} · {} ms", elapsed_ms),
                            None,
                        )?,
                        PlanExecutionTimelineItem::DeviationRecorded { summary, .. } => {
                            self.markdown(&format!("{id}:deviation:{index}"), summary)?
                        }
                    }
                }
            }
            TimelineEntry::PlanResolution {
                id,
                resolution,
                deviation,
                audit,
                ..
            } => {
                let tasks = &resolution.task_summary;
                let tests = &resolution.test_summary;
                self.literal(
                    id,
                    &format!(
                        "Plan {:?} · {}/{} tasks completed · {} blocked",
                        resolution.kind, tasks.completed, tasks.total, tasks.blocked
                    ),
                    Some(TranscriptAction::Plan {
                        plan_id: resolution.plan_id.clone(),
                    }),
                )?;
                self.literal(
                    &format!("{id}:tests"),
                    &format!(
                        "Tests: {} passed, {} failed, {} skipped, {} not run",
                        tests.passed, tests.failed, tests.skipped, tests.not_run
                    ),
                    None,
                )?;
                for deviation in deviation {
                    self.markdown(
                        &format!("{id}:deviation:{}", deviation.id),
                        &format!("{}\n{}", deviation.summary, deviation.reason),
                    )?;
                }
                if let Some(audit) = audit {
                    self.literal(
                        &format!("{id}:audit"),
                        &format!(
                            "Audit: {} unplanned paths, {} unchanged planned paths",
                            audit.unplanned_paths.len(),
                            audit.unchanged_planned_paths.len()
                        ),
                        None,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn interaction(
        &mut self,
        interaction: &InteractionRecord,
        agents: &HashMap<String, TimelineEntry>,
        depth: usize,
    ) -> Result<()> {
        if depth == 0 && self.leading_separator {
            self.literal(&format!("{}:separator", interaction.id), "", None)?;
        }
        let prompt = self.renderer.prompt(
            BlockId(format!("{}:prompt", interaction.id)),
            &interaction.prompt,
        )?;
        self.prompt.push(prompt.id.clone());
        self.push(prompt)?;
        let mut response = Vec::new();
        for node in &interaction.node_list {
            match node {
                InteractionNode::MainSegment { segment } => {
                    let segment_start = self.block.len();
                    let summary = segment_summary(interaction, segment, self.now_ms);
                    let mut heading = self.renderer.literal(
                        BlockId(format!("{}:summary", segment.id)),
                        &summary,
                        2,
                    )?;
                    heading.metadata.decoration.push(Decoration {
                        range: TextRange {
                            start: TextPosition { row: 0, column: 0 },
                            end: TextPosition {
                                row: heading.text.row_count(),
                                column: 0,
                            },
                        },
                        capture: if segment.state == SegmentState::Complete {
                            "ForgeHarnessThought".into()
                        } else {
                            "ForgeHarnessThinking".into()
                        },
                        priority: 100,
                    });
                    self.push(heading)?;
                    for thought in &segment.thought {
                        let start = self.block.len();
                        let commentary = self.renderer.commentary(
                            BlockId(format!("{}:thought", thought.id)),
                            &thought.text,
                        )?;
                        self.push(commentary)?;
                        if !thought.tool.is_empty() {
                            let tool_start = self.block.len();
                            let failed = thought.tool.iter().filter(|tool| tool.failed).count();
                            let count = thought.tool.len();
                            let mut summary = format!(
                                "  ▸ Ran {count} {}",
                                if count == 1 { "tool" } else { "tools" }
                            );
                            if failed > 0 {
                                summary.push_str(&format!(" ({failed} failed)"));
                            }
                            self.literal(&format!("{}:tools", thought.id), &summary, None)?;
                            for tool in &thought.tool {
                                let tool_start = self.block.len();
                                self.tool(&interaction.id, tool)?;
                                self.fold(
                                    tool_start,
                                    &format!("{}:tool:{}", thought.id, tool.id),
                                    true,
                                );
                            }
                            self.fold(tool_start, &format!("{}:tools", thought.id), true);
                        }
                        if let Some(diff) = &thought.diff_text {
                            self.diff(&format!("{}:diff", thought.id), "Thought changes", diff)?;
                        }
                        self.fold(start, &format!("{}:thought", thought.id), false);
                    }
                    if let Some(active) = &segment.active {
                        let commentary = self
                            .renderer
                            .commentary(BlockId(format!("{}:active", segment.id)), &active.text)?;
                        self.push(commentary)?;
                        if let Some(tool) = &active.latest_tool {
                            let preview = self.renderer.active_tool_preview(
                                BlockId(format!("{}:tool", segment.id)),
                                &tool.kind,
                                &tool.status,
                                tool.failed,
                                &tool.title,
                                &tool.output,
                            )?;
                            self.push(preview)?;
                        }
                    }
                    self.fold(
                        segment_start,
                        &format!("{}:segment", segment.id),
                        segment.state == SegmentState::Complete,
                    );
                    if let Some(text) = &segment.response {
                        response.push((format!("{}:response", segment.id), text));
                    }
                }
                InteractionNode::SteeringPrompt { prompt } => {
                    let block = self
                        .renderer
                        .prompt(BlockId(format!("{}:prompt", prompt.id)), &prompt.text)?;
                    self.prompt.push(block.id.clone());
                    self.push(block)?;
                }
                InteractionNode::AgentReference { agent } => {
                    if let Some(entry) = agents.get(&agent.agent_run_id) {
                        self.entry(entry, depth + 1)?;
                    } else {
                        self.literal(
                            &agent.id,
                            "Agent",
                            Some(TranscriptAction::Agent {
                                run_id: agent.agent_run_id.clone(),
                            }),
                        )?;
                    }
                }
                InteractionNode::PlanCommentResolution { resolution } => {
                    for (index, annotation) in resolution.annotation.iter().enumerate() {
                        self.markdown(
                            &format!("{}:comment:{index}", resolution.id),
                            &format!("Resolved {}\n{}", annotation.label, annotation.body),
                        )?;
                    }
                }
                InteractionNode::ArtifactChange { change } => {
                    self.diff(&change.id, &change.path, &change.diff_text)?
                }
            }
        }
        if let Some(diff) = &interaction.attributed_diff_text {
            self.diff(
                &format!("{}:changes", interaction.id),
                "Interaction changes",
                diff,
            )?;
        }
        if !interaction.attributed_matches_checkpoint {
            if let Some(diff) = &interaction.checkpoint_diff_text {
                self.diff(
                    &format!("{}:checkpoint", interaction.id),
                    "Checkpoint changes",
                    diff,
                )?;
            }
        }
        for (id, text) in response {
            self.markdown(&id, text)?;
        }
        Ok(())
    }

    fn tool(&mut self, interaction: &str, tool: &crate::interaction::CompletedTool) -> Result<()> {
        let call_id = format!("{interaction}:{}", tool.id);
        ensure!(
            !self.tool.contains_key(&call_id),
            "duplicate tool identity in interaction"
        );
        self.bytes = self
            .bytes
            .checked_add(tool.output.len())
            .context("transcript output capacity overflow")?;
        ensure!(
            self.bytes <= 32 * 1024 * 1024,
            "transcript entry exceeds 32 MiB"
        );
        let output = ToolOutputView::new(call_id.clone(), Arc::from(tool.output.as_str()))?;
        let id = format!("{call_id}:tool");
        let target = TargetId(id.clone());
        let label = tool.title.clone();
        let block = self.renderer.tool_preview(
            BlockId(id),
            target.clone(),
            &tool.kind,
            &tool.status,
            tool.failed,
            &label,
            &output.collapsed(),
        )?;
        self.action.insert(
            target,
            TranscriptAction::Tool {
                call_id: call_id.clone(),
            },
        );
        self.tool.insert(call_id, output);
        self.push(block)
    }

    fn diff(&mut self, id: &str, label: &str, text: &str) -> Result<()> {
        if text.is_empty() {
            return Ok(());
        }
        ensure!(
            text.len() <= 8 * 1024 * 1024,
            "transcript diff exceeds 8 MiB"
        );
        self.bytes += text.len();
        self.literal(
            id,
            label,
            Some(TranscriptAction::Diff { text: text.into() }),
        )
    }

    fn markdown(&mut self, id: &str, text: &str) -> Result<()> {
        let rendered = self.renderer.response(BlockId(id.into()), text)?;
        for link in rendered.link {
            self.action.insert(
                link.target,
                TranscriptAction::Url {
                    url: link.destination,
                },
            );
        }
        self.push(rendered.block)
    }

    fn literal(&mut self, id: &str, text: &str, action: Option<TranscriptAction>) -> Result<()> {
        let mut block = self.renderer.literal(BlockId(id.into()), text, 2)?;
        if let Some(action) = action {
            let target = TargetId(id.into());
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
            self.action.insert(target, action);
        }
        self.push(block)
    }

    fn push(&mut self, block: BufferBlock) -> Result<()> {
        self.bytes += block.text.byte_count();
        ensure!(
            self.bytes <= 32 * 1024 * 1024 && self.block.len() < 65536,
            "transcript entry exceeds native capacity"
        );
        self.block.push(block);
        Ok(())
    }

    fn indent_range(&mut self, start: usize, depth: usize) {
        let prefix = "  ".repeat(depth);
        if prefix.is_empty() {
            return;
        }
        for block in &mut self.block[start..] {
            for row in 0..block.text.row_count() {
                if let Some(gutter) = block
                    .metadata
                    .gutter
                    .iter_mut()
                    .find(|gutter| gutter.position.row == row && gutter.position.column == 0)
                {
                    if let Some(chunk) = gutter.chunk.first_mut() {
                        chunk.text.insert_str(0, &prefix);
                    } else {
                        gutter.chunk.push(TextChunk {
                            text: prefix.clone(),
                            capture: "Normal".into(),
                        });
                    }
                } else {
                    block.metadata.gutter.push(Gutter {
                        position: TextPosition { row, column: 0 },
                        chunk: vec![TextChunk {
                            text: prefix.clone(),
                            capture: "Normal".into(),
                        }],
                        priority: 200,
                    });
                }
            }
        }
    }

    fn fold(&mut self, start: usize, identity: &str, closed: bool) {
        let selected = &self.block[start..];
        if selected
            .iter()
            .map(|block| block.text.row_count())
            .sum::<usize>()
            < 2
        {
            return;
        }
        let end = selected.last().expect("nonempty folded block range");
        let anchor = BlockAnchor {
            block: end.id.clone(),
            position: TextPosition {
                row: end.text.row_count(),
                column: 0,
            },
        };
        self.block[start].metadata.fold.push(FoldRange {
            id: FoldId(identity.into()),
            start: TextPosition { row: 0, column: 0 },
            end: anchor,
            closed,
        });
    }
}

fn agent_summary(run: &crate::agent::AgentRun, now_ms: i64) -> String {
    use crate::agent::AgentRunStatus;

    let elapsed_seconds = now_ms
        .min(run.updated_at_ms.max(run.created_at_ms))
        .saturating_sub(run.created_at_ms)
        .max(0)
        / 1_000;
    let lifecycle = match run.status {
        AgentRunStatus::Starting => format!("starting for {elapsed_seconds}s"),
        AgentRunStatus::Running => {
            let elapsed_seconds = now_ms.saturating_sub(run.created_at_ms).max(0) / 1_000;
            format!("running for {elapsed_seconds}s")
        }
        AgentRunStatus::Waiting => format!("waiting for {elapsed_seconds}s"),
        AgentRunStatus::Completed => format!("completed in {elapsed_seconds}s"),
        AgentRunStatus::Failed => format!("failed after {elapsed_seconds}s"),
        AgentRunStatus::Interrupted => format!("interrupted after {elapsed_seconds}s"),
        AgentRunStatus::Closed => format!("closed after {elapsed_seconds}s"),
    };
    format!("▸ Agent {} {lifecycle}", run.label())
}

fn session_event_text(event: &SessionEventKind) -> String {
    match event {
        SessionEventKind::Renamed { name } if name.is_empty() => "Session name cleared".into(),
        SessionEventKind::Renamed { name } => format!("Session renamed to {name}"),
        SessionEventKind::Forked {
            source_session_id,
            source_session_name,
        } => {
            let mut text = format!("Forked from {source_session_id}");
            if !source_session_name.is_empty() {
                text.push_str(&format!(" ({source_session_name})"));
            }
            text
        }
    }
}

fn segment_summary(interaction: &InteractionRecord, segment: &MainSegment, now_ms: i64) -> String {
    let complete = segment.state == SegmentState::Complete;
    let duration = if complete {
        segment.duration_ms
    } else {
        segment
            .duration_ms
            .max(now_ms.saturating_sub(segment.started_at_ms).max(0) as u64)
    } / 1000;
    let activity = if interaction.state == InteractionState::Cancelled {
        "Cancelled after"
    } else {
        match interaction.kind {
            InteractionKind::PlanDraft | InteractionKind::PlanRevision
                if complete && interaction.awaiting_input =>
            {
                "Planning paused after"
            }
            InteractionKind::PlanDraft | InteractionKind::PlanRevision if complete => "Planned for",
            InteractionKind::PlanDraft | InteractionKind::PlanRevision => "Planning for",
            InteractionKind::PlanExecution if complete => "Executed plan for",
            InteractionKind::PlanExecution => "Executing plan for",
            InteractionKind::Chat if complete => "Thought for",
            InteractionKind::Chat => "Thinking for",
        }
    };
    let mut summary = format!("▸ {activity} {duration}s");
    if complete {
        if let Some(tokens) = segment.token_count {
            let display = if tokens >= 1000 {
                format!("{:.1}k", tokens as f64 / 1000.0)
            } else {
                tokens.to_string()
            };
            summary.push_str(&format!(", {display} tokens"));
        }
    }
    let count = segment
        .thought
        .iter()
        .map(|thought| thought.tool.len())
        .sum::<usize>()
        + segment
            .active
            .as_ref()
            .map_or(0, |active| active.tool_count);
    let failed = segment
        .thought
        .iter()
        .flat_map(|thought| &thought.tool)
        .filter(|tool| tool.failed)
        .count()
        + segment
            .active
            .as_ref()
            .map_or(0, |active| active.failed_count);
    if count > 0 {
        summary.push_str(&format!(
            ", {count} {} called",
            if count == 1 { "tool" } else { "tools" }
        ));
        if failed > 0 {
            summary.push_str(&format!(" ({failed} failed)"));
        }
    }
    if segment.spawned_agent_count > 0 {
        let count = segment.spawned_agent_count;
        summary.push_str(&format!(
            ", {count} {} spawned",
            if count == 1 { "agent" } else { "agents" }
        ));
    }
    summary
}

#[cfg(test)]
mod tests {
    use super::{project_at, project_at_with_separator, session_event_text};
    use forge_buffer::width::WidthProfile;
    use serde_json::json;
    use std::collections::HashMap;

    use crate::{
        interaction::InteractionRecord,
        session::state_machine::{SessionPhase, WorkflowActivity},
        timeline::{SessionEventKind, TimelineEntry},
    };

    #[test]
    fn session_event_text_preserves_clear_and_fork_lineage() {
        assert_eq!(
            session_event_text(&SessionEventKind::Renamed {
                name: String::new()
            }),
            "Session name cleared"
        );
        assert_eq!(
            session_event_text(&SessionEventKind::Forked {
                source_session_id: "session-1".into(),
                source_session_name: "Architecture review".into(),
            }),
            "Forked from session-1 (Architecture review)"
        );
    }

    #[test]
    fn nested_agent_uses_legacy_lifecycle_task_and_child_indentation() {
        use crate::agent::{AgentRun, AgentRunStatus};

        let child_interaction = serde_json::from_value(json!({
            "id":"child", "session_id":"session", "ordinal":1, "prompt":"Inspect child",
            "kind":"chat", "state":"complete", "created_at_ms":1000, "completed_at_ms":2000,
            "attributed_matches_checkpoint":false, "node_list":[{"kind":"main_segment","segment":{
                "id":"child-segment", "state":"complete", "started_at_ms":1000,
                "completed_at_ms":2000, "duration_ms":1000, "spawned_agent_count":0,
                "thought":[], "response":"Child result."
            }}]
        }))
        .unwrap();
        let agent = TimelineEntry::AgentLifecycle {
            id: "agent-run".into(),
            created_at_ms: 1_000,
            run: AgentRun {
                id: "agent-run".into(),
                session_id: "session".into(),
                parent_interaction_id: Some("parent".into()),
                parent_thread_id: None,
                provider_thread_id: None,
                active_turn_id: None,
                definition: "explorer".into(),
                nickname: None,
                task: "Trace ownership.".into(),
                status: AgentRunStatus::Completed,
                created_at_ms: 1_000,
                updated_at_ms: 2_000,
            },
            interaction: vec![child_interaction],
            agent: Vec::new(),
        };
        let parent_interaction = serde_json::from_value(json!({
            "id":"parent", "session_id":"session", "ordinal":1, "prompt":"Delegate",
            "kind":"chat", "state":"complete", "created_at_ms":1000, "completed_at_ms":3000,
            "attributed_matches_checkpoint":false,
            "node_list":[{"kind":"agent_reference","agent":{
                "id":"agent-reference", "agent_run_id":"agent-run", "created_at_ms":1000
            }}]
        }))
        .unwrap();
        let projection = project_at(
            &TimelineEntry::Interaction {
                id: "parent".into(),
                created_at_ms: 1_000,
                interaction: parent_interaction,
                agent_by_id: HashMap::from([("agent-run".into(), agent)]),
            },
            &WidthProfile::default(),
            3_000,
        )
        .unwrap();

        let heading = projection
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "agent-run")
            .unwrap();
        assert_eq!(
            heading.text.row(0),
            Some("▸ Agent explorer completed in 1s")
        );
        let task = projection
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "agent-run:task")
            .unwrap();
        assert_eq!(task.text.row(0), Some("  ↳ Trace ownership."));
        let response = projection
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "child-segment:response")
            .unwrap();
        assert_eq!(response.metadata.gutter[0].chunk[0].text, "  ▸ ");
    }

    #[test]
    fn working_status_reports_elapsed_seconds_without_persisting_a_timer_value() {
        let projection = project_at(
            &TimelineEntry::Status {
                id: "session:status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    started_at_ms: 1_000,
                    activity: WorkflowActivity::Working,
                },
            },
            &WidthProfile::default(),
            4_999,
        )
        .unwrap();

        assert_eq!(
            projection.entry.block[0].text.wire_rows(),
            vec!["  Working (3s)"]
        );
    }

    #[test]
    fn only_noninitial_top_level_interactions_own_a_separator() {
        let interaction: InteractionRecord = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "ordinal":1, "prompt":"Inspect parser",
            "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let first = project_at(
            &TimelineEntry::Interaction {
                id: "interaction".into(),
                created_at_ms: 1_000,
                interaction: interaction.clone(),
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1_000,
        )
        .unwrap();
        assert_eq!(first.entry.block.len(), 1);
        assert_eq!(first.entry.block[0].id.0, "interaction:prompt");

        let projection = project_at_with_separator(
            &TimelineEntry::Interaction {
                id: "interaction".into(),
                created_at_ms: 1_000,
                interaction,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1_000,
            true,
        )
        .unwrap();

        assert_eq!(projection.entry.block.len(), 2);
        assert_eq!(projection.entry.block[0].id.0, "interaction:separator");
        assert_eq!(projection.entry.block[0].text.wire_rows(), vec![""]);
        assert!(projection.entry.block[0].metadata.fold.is_empty());
        assert_eq!(projection.entry.block[1].id.0, "interaction:prompt");
    }

    #[test]
    fn completed_segments_keep_summary_thought_tool_folds_and_response_outside() {
        let interaction = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "ordinal":1, "prompt":"Inspect parser",
            "kind":"chat", "state":"complete", "created_at_ms":1000, "completed_at_ms":3500,
            "attributed_matches_checkpoint":false, "node_list":[{"kind":"main_segment","segment":{
                "id":"segment", "state":"complete", "started_at_ms":1000,"completed_at_ms":3500,
                "duration_ms":2500,"token_count":128,"spawned_agent_count":0,
                "thought":[{"id":"thought","text":"Inspect the boundary", "synthetic":false,
                    "started_at_ms":1000,"completed_at_ms":2000,
                    "tool":[{"id":"tool","kind":"command","title":"cargo test parser",
                        "output":"first\nmiddle\nlast", "status":"completed","failed":false}]}],
                "response":"The parser is correct.\n\n- Source remains available."
            }}]
        }))
        .unwrap();
        let entry = TimelineEntry::Interaction {
            id: "interaction".into(),
            created_at_ms: 1000,
            interaction,
            agent_by_id: HashMap::new(),
        };
        for columns in [1, 20, 100, 160] {
            let projection = project_at(
                &entry,
                &WidthProfile {
                    columns,
                    ..WidthProfile::default()
                },
                3500,
            )
            .unwrap();
            for block in &projection.entry.block {
                block.validate().unwrap();
            }
            let summary = projection
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == "segment:summary")
                .unwrap();
            if columns >= 100 {
                assert_eq!(
                    summary.text.row(0),
                    Some("▸ Thought for 2s, 128 tokens, 1 tool called")
                );
            }
            let fold = &summary.metadata.fold[0];
            assert!(fold.closed);
            assert_eq!(fold.end.block.0, "interaction:tool:tool");
            let thought = projection
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == "thought:thought")
                .unwrap();
            assert!(!thought.metadata.fold[0].closed);
            let tools = projection
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == "thought:tools")
                .unwrap();
            assert!(tools.metadata.fold[0].closed);
            let tool = projection
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == "interaction:tool:tool")
                .unwrap();
            assert!(tool.metadata.fold[0].closed);
            assert_eq!(tool.metadata.fold[0].id.0, "thought:tool:tool");
            if columns >= 100 {
                assert_eq!(tool.text.row(0), Some("  • Ran cargo test parser"));
            }
            assert!(
                tool.metadata
                    .decoration
                    .iter()
                    .any(|decoration| decoration.capture == "ForgeHarnessCommand")
            );
            let response = projection.entry.block.last().unwrap();
            assert_eq!(response.id.0, "segment:response");
            assert!(response.metadata.fold.is_empty());
            assert!(
                projection
                    .action
                    .keys()
                    .any(|target| target.0 == "interaction:tool:tool")
            );
        }
    }

    #[test]
    fn running_segment_summary_tracks_duration_and_completion_metadata() {
        use crate::interaction::{InteractionKind, InteractionRecord, MainSegment};
        let mut interaction: InteractionRecord = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "ordinal":1, "prompt":"Plan parser",
            "kind":"plan_draft", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false,"node_list":[]
        }))
        .unwrap();
        let mut segment = MainSegment::running("segment".into(), 1000);
        assert_eq!(
            super::segment_summary(&interaction, &segment, 4200),
            "▸ Planning for 3s"
        );
        segment.complete(5500);
        segment.token_count = Some(1280);
        interaction.awaiting_input = true;
        assert_eq!(
            super::segment_summary(&interaction, &segment, 20000),
            "▸ Planning paused after 4s, 1.3k tokens"
        );
        interaction.kind = InteractionKind::Chat;
        assert_eq!(
            super::segment_summary(&interaction, &segment, 20000),
            "▸ Thought for 4s, 1.3k tokens"
        );
    }

    #[test]
    fn active_tool_renders_live_heading_state_and_four_output_rows() {
        let interaction = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "ordinal":1, "prompt":"Inspect parser",
            "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[{"kind":"main_segment","segment":{
                "id":"segment", "state":"running", "started_at_ms":1000,
                "duration_ms":0,"spawned_agent_count":0,"thought":[],
                "active": {"interaction_id":"interaction", "thought_id":"thought", "text":"Inspecting",
                    "synthetic":false,"tool_count":1,"failed_count":0,"revision":1,
                    "latest_tool":{"id":"tool","kind":"command","title":"cargo test --lib parser",
                        "output":"one\ntwo\nthree\nfour\nfive", "status":"in_progress","failed":false}}
            }}]
        }))
        .unwrap();
        let projection = project_at(
            &TimelineEntry::Interaction {
                id: "interaction".into(),
                created_at_ms: 1_000,
                interaction,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            2_000,
        )
        .unwrap();
        let tool = projection
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "segment:tool")
            .unwrap();

        assert_eq!(
            tool.text.wire_rows(),
            vec![
                "  • Ran cargo test --lib parser",
                "    └ one",
                "      two",
                "      three",
                "      four"
            ]
        );
        assert!(tool.metadata.fold.is_empty());
        assert!(
            tool.metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessCommand")
        );
        assert!(
            tool.metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessOutput")
        );
    }
}
