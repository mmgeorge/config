use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BufferBlock, ContentLayout, Decoration, FoldRange, TargetRange, TextChunk,
    TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::node::{NodeDisplay, NodeKind, NodeLifecycle, NodeState};
use forge_buffer::width::WidthProfile;
use serde::Serialize;

use crate::exchange::{Exchange, ExchangeActivity, ExchangeKind, ExchangeNode, ExchangeState};
use crate::session::state_machine::{ExecutionStatus, SessionPhase, WorkflowActivity};
use crate::timeline::{SessionEventKind, TimelineEntry};

use super::document::TranscriptEntry;
use super::tool::ToolOutputView;
use super::transcript::TranscriptRenderer;

#[path = "projection/checks.rs"]
mod checks;

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TranscriptAction {
    Question {
        question_set_id: String,
    },
    Tool {
        call_id: String,
    },
    Diff {
        text: String,
        #[serde(skip)]
        syntax: bool,
    },
    File {
        path: String,
        line: usize,
    },
    Declaration {
        plan_id: String,
        revision: u32,
        path: String,
        baseline: bool,
        document: bool,
        line: usize,
    },
    Plan {
        plan_id: String,
        revision: Option<u32>,
    },
    Agent {
        run_id: String,
    },
    Session {
        session_id: String,
    },
    Url {
        url: String,
    },
}

pub struct ProjectedEntry {
    pub entry: TranscriptEntry,
    pub prompt: Vec<BlockId>,
    pub action: HashMap<TargetId, TranscriptAction>,
    pub tool: HashMap<String, ToolOutputView>,
    pub(crate) syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
}

struct TimelineRenderer<'profile> {
    width: &'profile WidthProfile,
    margin: usize,
    now_ms: i64,
    block: Vec<BufferBlock>,
    prompt: Vec<BlockId>,
    action: HashMap<TargetId, TranscriptAction>,
    tool: HashMap<String, ToolOutputView>,
    syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
    leading_separator: bool,
    expansion: &'profile HashMap<String, bool>,
    tool_limit: &'profile HashMap<String, usize>,
}

#[derive(Clone, Copy)]
enum MarkdownRole {
    Response,
    Commentary,
    Message,
    Detail,
    Comment,
}

#[must_use]
struct Section {
    start: usize,
    margin: usize,
    child_indent: usize,
}

#[derive(Clone, Copy)]
pub enum ProjectionSource<'source> {
    Entry(&'source TimelineEntry),
    Exchange(&'source Exchange),
}

impl<'source> From<&'source TimelineEntry> for ProjectionSource<'source> {
    fn from(entry: &'source TimelineEntry) -> Self {
        Self::Entry(entry)
    }
}

impl ProjectionSource<'_> {
    fn id(self) -> String {
        match self {
            Self::Entry(entry) => entry.id(),
            Self::Exchange(exchange) => exchange.id.clone(),
        }
    }
}

pub fn project<'source>(
    entry: impl Into<ProjectionSource<'source>>,
    width: &WidthProfile,
    leading_separator: bool,
    expansion: &HashMap<String, bool>,
    tool_limit: &HashMap<String, usize>,
) -> Result<ProjectedEntry> {
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| {
            duration.as_millis().min(i64::MAX as u128) as i64
        });
    project_at_with_separator(
        entry.into(),
        width,
        now_ms,
        leading_separator,
        expansion,
        tool_limit,
    )
}

fn tool_group_label<'tool>(
    calls: impl IntoIterator<Item = &'tool crate::turn::ToolCall>,
    lifecycle: NodeLifecycle,
    now_ms: i64,
) -> String {
    let mut count = 0;
    let mut failed = 0;
    let mut elapsed = Some(0_u64);
    for tool in calls {
        count += 1;
        failed += usize::from(tool.failed);
        elapsed = elapsed
            .zip(tool.elapsed_ms(now_ms))
            .map(|(total, duration)| total.saturating_add(duration));
    }
    let mut label = format!(
        "▸ {} {count} {}",
        if lifecycle == NodeLifecycle::Live {
            "Running"
        } else {
            "Ran"
        },
        if count == 1 { "tool" } else { "tools" }
    );
    if failed > 0 {
        label.push_str(&format!(" ({failed} failed)"));
    }
    if let Some(elapsed) = elapsed {
        label.push_str(&format!(
            " · {}",
            super::duration::tool_duration(Some(elapsed)).trim()
        ));
    }
    label
}

pub(super) fn timing_blocks(
    entry: &TimelineEntry,
    width: &WidthProfile,
    document: &forge_buffer::document::BufferDocument,
    expanded: &HashMap<String, bool>,
    tools: &HashMap<String, ToolOutputView>,
) -> Result<Vec<BufferBlock>> {
    fn exchanges<'source>(
        entry: &'source TimelineEntry,
        result: &mut Vec<&'source Exchange>,
        headings: &mut Vec<(
            &'source str,
            &'source crate::agent::Agent,
            Option<&'source Exchange>,
        )>,
    ) {
        match entry {
            TimelineEntry::Exchange {
                exchange,
                agent_by_id,
                ..
            } => {
                result.push(exchange);
                for node in &exchange.node_list {
                    if let ExchangeNode::AgentReference { agent } = node
                        && let Some(TimelineEntry::AgentLifecycle { run, exchange, .. }) =
                            agent_by_id
                                .get(&agent.id)
                                .or_else(|| agent_by_id.get(&agent.child_agent_id))
                    {
                        headings.push((
                            &agent.id,
                            run,
                            exchange
                                .iter()
                                .find(|exchange| exchange.id == agent.child_exchange_id),
                        ));
                    }
                }
                for agent in agent_by_id.values() {
                    exchanges(agent, result, headings);
                }
            }
            TimelineEntry::AgentLifecycle {
                id,
                run,
                exchange,
                agent,
                ..
            } => {
                headings.push((id, run, None));
                result.extend(exchange);
                for source in exchange {
                    for node in &source.node_list {
                        if let ExchangeNode::AgentReference { agent: reference } = node
                            && let Some(TimelineEntry::AgentLifecycle { run, exchange, .. }) =
                                agent.iter().find(|agent| {
                                    agent.id() == reference.id
                                        || agent.id() == reference.child_agent_id
                                })
                        {
                            headings.push((
                                &reference.id,
                                run,
                                exchange
                                    .iter()
                                    .find(|exchange| exchange.id == reference.child_exchange_id),
                            ));
                        }
                    }
                }
                for agent in agent {
                    exchanges(agent, result, headings);
                }
            }
            _ => {}
        }
    }
    if matches!(entry, TimelineEntry::Status { .. }) {
        return Ok(project(
            ProjectionSource::Entry(entry),
            width,
            false,
            expanded,
            &HashMap::new(),
        )?
        .entry
        .block);
    }
    let now_ms = SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis() as i64;
    if matches!(entry, TimelineEntry::Checks { .. }) {
        return Ok(Vec::new());
    }
    let mut source = Vec::new();
    let mut headings = Vec::new();
    exchanges(entry, &mut source, &mut headings);
    let mut blocks = Vec::new();
    let mut rendered = std::collections::HashSet::new();
    for (id, run, exchange) in headings {
        if !rendered.insert(id) {
            continue;
        }
        let Some(previous) = document.block(&BlockId(id.into())) else {
            continue;
        };
        let mut profile = width.clone();
        profile.columns = profile
            .columns
            .saturating_sub(
                previous
                    .metadata
                    .layout
                    .as_ref()
                    .map_or(0, |layout| layout.indent.saturating_sub(2)),
            )
            .max(1);
        let mut block = TranscriptRenderer::new(&profile)?.literal(
            previous.id.clone(),
            &agent_summary(run, exchange, now_ms),
            2,
        )?;
        block.metadata = previous.metadata.clone();
        for span in &mut block.metadata.decoration {
            if span.range.end.row == previous.text.row_count() {
                span.range.end.row = block.text.row_count();
            }
        }
        TranscriptRenderer::resolve_marker(&mut block)?;
        blocks.push(block);
    }
    for exchange in source {
        if exchange.completed_at_ms.is_some() || exchange.state != ExchangeState::Running {
            continue;
        }
        if exchange.execution_started_at_ms.is_some()
            && let Some(previous) = document.block(&BlockId(format!("{}:summary", exchange.id)))
        {
            let mut profile = width.clone();
            profile.columns = profile
                .columns
                .saturating_sub(
                    previous
                        .metadata
                        .layout
                        .as_ref()
                        .map_or(0, |layout| layout.indent.saturating_sub(2)),
                )
                .max(1);
            let mut block = TranscriptRenderer::new(&profile)?.literal(
                previous.id.clone(),
                &exchange_activity_summary(exchange, now_ms),
                2,
            )?;
            block.metadata = previous.metadata.clone();
            for span in &mut block.metadata.decoration {
                if span.range.end.row == previous.text.row_count() {
                    span.range.end.row = block.text.row_count();
                }
            }
            TranscriptRenderer::resolve_marker(&mut block)?;
            blocks.push(block);
        }
        let mut groups: BTreeMap<&str, Vec<_>> = BTreeMap::new();
        for turn in &exchange.turn {
            for tool in turn.tools() {
                let call_id = format!("{}:{}", turn.id(), tool.id);
                if let Some(output) = tools.get(&call_id) {
                    if output.group.is_empty() {
                        if tool.state() != crate::turn::ToolState::Running {
                            continue;
                        }
                        if let Some(previous) = document.block(&BlockId(format!("{call_id}:tool")))
                        {
                            let mut profile = width.clone();
                            profile.columns = profile
                                .columns
                                .saturating_sub(
                                    previous
                                        .metadata
                                        .layout
                                        .as_ref()
                                        .map_or(0, |layout| layout.indent.saturating_sub(2)),
                                )
                                .max(1);
                            blocks.push(TranscriptRenderer::new(&profile)?.refresh_tool_heading(
                                previous,
                                &tool.kind,
                                tool.elapsed_ms(now_ms),
                                tool.failed,
                                &tool.title,
                                false,
                            )?);
                        }
                    } else {
                        groups.entry(&output.group).or_default().push((turn, tool));
                    }
                }
            }
        }
        for (id, calls) in groups {
            if !calls
                .iter()
                .any(|(_, tool)| tool.state() == crate::turn::ToolState::Running)
            {
                continue;
            }
            for (turn, tool) in &calls {
                let call_id = format!("{}:{}", turn.id(), tool.id);
                let Some(previous) = document.block(&BlockId(format!("{call_id}:tool"))) else {
                    continue;
                };
                let mut profile = width.clone();
                profile.columns = profile
                    .columns
                    .saturating_sub(
                        previous
                            .metadata
                            .layout
                            .as_ref()
                            .map_or(0, |layout| layout.indent.saturating_sub(2)),
                    )
                    .max(1);
                blocks.push(TranscriptRenderer::new(&profile)?.refresh_tool_heading(
                    previous,
                    &tool.kind,
                    tool.elapsed_ms(now_ms),
                    tool.failed,
                    &tool.title,
                    expanded.get(&format!("{call_id}:tool")) == Some(&true),
                )?);
            }
            let Some(previous) = document.block(&BlockId(id.into())) else {
                continue;
            };
            let lifecycle = previous
                .metadata
                .node
                .as_ref()
                .context("tool group lifecycle is missing")?
                .lifecycle;
            let mut profile = width.clone();
            profile.columns = profile
                .columns
                .saturating_sub(
                    previous
                        .metadata
                        .layout
                        .as_ref()
                        .map_or(0, |layout| layout.indent.saturating_sub(2)),
                )
                .max(1);
            let mut block = TranscriptRenderer::new(&profile)?.literal(
                previous.id.clone(),
                &tool_group_label(calls.iter().map(|(_, tool)| *tool), lifecycle, now_ms),
                2,
            )?;
            block.metadata = previous.metadata.clone();
            for span in &mut block.metadata.decoration {
                if span.range.end.row == previous.text.row_count() {
                    span.range.end.row = block.text.row_count();
                }
            }
            blocks.push(block);
        }
    }
    Ok(blocks)
}

pub(super) fn message_block(
    previous: &BufferBlock,
    message: &crate::turn::Message,
    width: &WidthProfile,
) -> Result<ProjectedEntry> {
    let indent = previous
        .metadata
        .layout
        .as_ref()
        .map_or(2, |layout| layout.indent);
    let mut content_width = width.clone();
    content_width.columns = content_width
        .columns
        .saturating_sub(indent.saturating_sub(2))
        .max(1);
    let mut renderer = TimelineRenderer {
        width: &content_width,
        margin: 0,
        now_ms: 0,
        block: Vec::new(),
        prompt: Vec::new(),
        action: HashMap::new(),
        tool: HashMap::new(),
        syntax: HashMap::new(),
        leading_separator: false,
        expansion: &HashMap::new(),
        tool_limit: &HashMap::new(),
    };
    renderer.markdown(
        &previous.id.0,
        message.text(),
        if message.delivery() == crate::turn::MessageDelivery::Final {
            if previous
                .metadata
                .layout
                .as_ref()
                .is_some_and(|layout| layout.marker.is_none())
            {
                MarkdownRole::Message
            } else {
                MarkdownRole::Response
            }
        } else {
            MarkdownRole::Commentary
        },
    )?;
    let block = &mut renderer.block[0];
    block.metadata.layout = previous.metadata.layout.clone();
    if let Some(layout) = &mut block.metadata.layout {
        layout.source_indent = 0;
    }
    super::layout::materialize(block)?;
    block.metadata.node = previous.metadata.node.clone();
    block.metadata.content_node = previous.metadata.content_node.clone();
    if previous
        .metadata
        .node
        .as_ref()
        .is_some_and(|node| node.kind == NodeKind::Thought)
    {
        block.metadata.fold = previous.metadata.fold.clone();
        for fold in &mut block.metadata.fold {
            if fold.end.block == previous.id {
                fold.end.position = TextPosition {
                    row: block.text.row_count(),
                    column: 0,
                };
            }
        }
    }
    if message.delivery() == crate::turn::MessageDelivery::Final
        && previous
            .metadata
            .layout
            .as_ref()
            .and_then(|layout| layout.marker.as_ref())
            .is_some_and(|marker| marker.capture == "ForgeHarnessCommentary")
        && let Some(node) = &mut block.metadata.node
    {
        node.lifecycle = NodeLifecycle::Live;
        node.default_display = NodeDisplay::Full;
    }
    super::message::prepare(block, width)?;
    Ok(ProjectedEntry {
        entry: TranscriptEntry {
            id: previous.id.0.clone(),
            block: renderer.block,
        },
        prompt: renderer.prompt,
        action: renderer.action,
        tool: renderer.tool,
        syntax: renderer.syntax,
    })
}

#[cfg(test)]
fn project_at(entry: &TimelineEntry, width: &WidthProfile, now_ms: i64) -> Result<ProjectedEntry> {
    project_at_with_separator(
        ProjectionSource::Entry(entry),
        width,
        now_ms,
        false,
        &HashMap::new(),
        &HashMap::new(),
    )
}

fn project_at_with_separator(
    entry: ProjectionSource<'_>,
    width: &WidthProfile,
    now_ms: i64,
    leading_separator: bool,
    expansion: &HashMap<String, bool>,
    tool_limit: &HashMap<String, usize>,
) -> Result<ProjectedEntry> {
    let mut projection = TimelineRenderer {
        width,
        margin: 0,
        now_ms,
        block: Vec::new(),
        prompt: Vec::new(),
        action: HashMap::new(),
        tool: HashMap::new(),
        syntax: HashMap::new(),
        leading_separator,
        expansion,
        tool_limit,
    };
    match entry {
        ProjectionSource::Entry(entry) => projection.entry(entry, 0)?,
        ProjectionSource::Exchange(exchange) => {
            projection.interaction(exchange, &HashMap::new(), 0)?
        }
    }
    super::nodes::link(&mut projection.block);
    for block in &mut projection.block {
        TranscriptRenderer::resolve_marker(block)?;
        super::layout::materialize(block)?;
        super::message::prepare(block, width)?;
    }
    ensure!(projection.margin == 0, "timeline section was not finished");
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
        syntax: projection.syntax,
    })
}

impl TimelineRenderer<'_> {
    fn entry(&mut self, entry: &TimelineEntry, depth: usize) -> Result<()> {
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || self.entry_content(entry, depth))
    }

    fn entry_content(&mut self, entry: &TimelineEntry, depth: usize) -> Result<()> {
        match entry {
            TimelineEntry::Inherited {
                captured_at_ms,
                entry,
            } => {
                let previous_time = self.now_ms;
                let first_block = self.block.len();
                let previous_action = self
                    .action
                    .keys()
                    .cloned()
                    .collect::<std::collections::HashSet<_>>();
                self.now_ms = *captured_at_ms;
                let result = self.entry(entry, depth);
                self.now_ms = previous_time;
                result?;
                self.action.retain(|target, action| {
                    previous_action.contains(target)
                        || matches!(
                            action,
                            TranscriptAction::Tool { .. }
                                | TranscriptAction::Diff { .. }
                                | TranscriptAction::File { .. }
                                | TranscriptAction::Url { .. }
                                | TranscriptAction::Session { .. }
                        )
                });
                for block in &mut self.block[first_block..] {
                    block
                        .metadata
                        .target
                        .retain(|target| self.action.contains_key(&target.id));
                    if let Some(node) = &mut block.metadata.node {
                        node.lifecycle = NodeLifecycle::Settled;
                    }
                }
            }
            TimelineEntry::Exchange {
                exchange: interaction,
                agent_by_id,
                ..
            } => {
                let agents = agent_by_id
                    .iter()
                    .map(|(id, entry)| (id.as_str(), entry))
                    .collect();
                self.interaction(interaction, &agents, depth)?;
            }
            TimelineEntry::Checks { run, .. } => self.checks(run, true, None)?,
            TimelineEntry::SessionEvent { event, .. } => self.session_event(event)?,
            TimelineEntry::Status {
                id,
                status,
                agent_count,
                status_thoughts,
                checks,
                ..
            } => {
                let rendered = super::status::StatusView {
                    id,
                    status,
                    agent_count: *agent_count,
                    status_thoughts: *status_thoughts,
                    checks: checks.as_ref(),
                }
                .render(&self.content_width(), self.now_ms)?;
                if let Some((target, action)) = rendered.action {
                    self.action.insert(target, action);
                }
                self.push(rendered.block)?;
                if let SessionPhase::Working {
                    execution: Some(execution),
                    activity,
                    ..
                } = status
                    && WorkflowActivity::from(execution.phase) == *activity
                    && execution.has_comparison()
                {
                    self.declaration_comparison(id, execution)?;
                }
            }
            TimelineEntry::AgentLifecycle {
                id,
                run,
                exchange: interaction,
                agent,
                ..
            } => {
                if interaction.is_empty() {
                    self.agent_entry(id, run, "", interaction, agent, None, depth)?;
                } else {
                    let children = agent
                        .iter()
                        .filter_map(|entry| match entry {
                            TimelineEntry::AgentLifecycle { id, .. } => Some((id.as_str(), entry)),
                            _ => None,
                        })
                        .collect();
                    for exchange in interaction {
                        self.interaction(exchange, &children, depth)?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Render a persisted plan settlement at its causal position in the execution.
    fn plan_resolution(
        &mut self,
        resolution: &crate::plan::PlanResolutionRecord,
        deviation: &[crate::plan::PlanDeviation],
        audit: Option<&crate::plan::PlanAudit>,
    ) -> Result<()> {
        let id = &resolution.id;
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
                revision: None,
            }),
        )?;
        let section = self.begin_section(2);
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
                MarkdownRole::Detail,
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
        self.finish_section(section, id, true);
        Ok(())
    }

    /// Render planning evidence at its owning exchange position.
    fn plan_event(&mut self, event: &crate::plan::ExchangePlanEvent) -> Result<()> {
        use crate::plan::{PlanEventContent, PlanExecutionLifecycleEvent, PlanLifecycleKind};
        match &event.content {
            PlanEventContent::Lifecycle { title, lifecycle } => {
                if lifecycle.kind == PlanLifecycleKind::QuestionAsked {
                    if let Some(question) = &lifecycle.question {
                        return self.question(&event.id, question, lifecycle.answer.as_deref());
                    }
                }
                let label = match lifecycle.kind {
                    PlanLifecycleKind::QuestionAsked => "Clarification requested",
                    PlanLifecycleKind::QuestionAnswered => "Clarification answered",
                    PlanLifecycleKind::QuestionWithdrawn => "Clarification withdrawn",
                    PlanLifecycleKind::Created => "Plan submitted",
                    PlanLifecycleKind::RevisionCreated => "Plan submitted",
                    PlanLifecycleKind::ChangesRequested => "Plan revision requested",
                    PlanLifecycleKind::Accepted => "Plan accepted",
                    PlanLifecycleKind::Cancelled => "Plan rejected",
                };
                let question = matches!(
                    lifecycle.kind,
                    PlanLifecycleKind::QuestionAsked
                        | PlanLifecycleKind::QuestionAnswered
                        | PlanLifecycleKind::QuestionWithdrawn
                );
                let summary = if lifecycle.kind == PlanLifecycleKind::QuestionAnswered {
                    let answer = lifecycle
                        .answer
                        .as_deref()
                        .unwrap_or("")
                        .trim()
                        .strip_prefix("Planning feedback:")
                        .unwrap_or(lifecycle.answer.as_deref().unwrap_or(""))
                        .trim()
                        .trim_start_matches("- ")
                        .replace("\n- ", ", ");
                    if answer.contains("[automatic timeout]") {
                        format!("● Automatic timeout: {answer}")
                    } else {
                        format!("● You answered: {answer}")
                    }
                } else if question {
                    format!("▸ {label}")
                } else {
                    format!("▸ {label}: {title} · revision {}", lifecycle.model_revision)
                };
                self.literal(
                    &event.id,
                    &summary,
                    (!question).then(|| TranscriptAction::Plan {
                        plan_id: lifecycle.plan_id.clone(),
                        revision: Some(lifecycle.model_revision),
                    }),
                )?;
                let section = self.begin_section(2);
                if lifecycle.kind == PlanLifecycleKind::QuestionAnswered {
                    self.prompt.push(BlockId(event.id.clone()));
                }
                if let Some(question) = lifecycle.question.as_ref().filter(|_| {
                    !matches!(
                        lifecycle.kind,
                        PlanLifecycleKind::QuestionAnswered | PlanLifecycleKind::QuestionWithdrawn
                    )
                }) {
                    for (index, question) in question.questions.iter().enumerate() {
                        self.markdown(
                            &format!("{}:question:{index}", event.id),
                            &question.question,
                            MarkdownRole::Detail,
                        )?;
                        for (option, value) in question.options.iter().enumerate() {
                            self.literal(
                                &format!("{}:question:{index}:option:{option}", event.id),
                                &format!("{}: {}", value.label, value.description),
                                None,
                            )?;
                        }
                    }
                }
                if let Some(comment) = lifecycle
                    .overall_comment
                    .as_ref()
                    .filter(|_| lifecycle.kind != PlanLifecycleKind::ChangesRequested)
                {
                    self.markdown(
                        &format!("{}:comment", event.id),
                        comment,
                        MarkdownRole::Detail,
                    )?;
                }
                if let Some(answer) = &lifecycle.answer {
                    self.markdown(
                        &format!("{}:answer", event.id),
                        answer,
                        MarkdownRole::Detail,
                    )?;
                }
                for (index, annotation) in lifecycle.annotation.iter().enumerate() {
                    self.markdown(
                        &format!("{}:annotation:{index}", event.id),
                        &format!("{}\n{}", annotation.label, annotation.body),
                        MarkdownRole::Comment,
                    )?;
                }
                self.finish_section(section, &event.id, true);
            }
            PlanEventContent::Execution { event: lifecycle } => {
                let label = match lifecycle {
                    PlanExecutionLifecycleEvent::Phase {
                        title, revision, ..
                    } => format!("{title} · revision {revision}"),
                    PlanExecutionLifecycleEvent::Completed { title } => {
                        format!("Plan complete: {title}")
                    }
                    PlanExecutionLifecycleEvent::Failed { title, reason } => format!(
                        "Plan failed: {title} — {}",
                        reason.replace(['\n', '\r'], " ")
                    ),
                };
                self.literal(&event.id, &format!("◇ {label}"), None)?;
            }
            PlanEventContent::Resolution {
                resolution,
                deviation,
                audit,
            } => {
                self.plan_resolution(resolution, deviation, audit.as_ref())?;
            }
        }
        Ok(())
    }

    fn agent_entry(
        &mut self,
        id: &str,
        run: &crate::agent::Agent,
        _task: &str,
        interaction: &[Exchange],
        agent: &[TimelineEntry],
        exchange_id: Option<&str>,
        depth: usize,
    ) -> Result<()> {
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            self.agent_content(id, run, interaction, agent, exchange_id, depth)
        })
    }

    fn agent_content(
        &mut self,
        id: &str,
        run: &crate::agent::Agent,
        interaction: &[Exchange],
        agent: &[TimelineEntry],
        exchange_id: Option<&str>,
        depth: usize,
    ) -> Result<()> {
        let exchange = exchange_id
            .and_then(|id| interaction.iter().find(|exchange| exchange.id == id))
            .or_else(|| interaction.last());
        let summary = agent_summary(run, exchange, self.now_ms);
        self.literal(
            id,
            &summary,
            Some(TranscriptAction::Agent {
                run_id: run.id.clone(),
            }),
        )?;
        let section = self.begin_section(2);
        let completed = exchange.is_some_and(|exchange| exchange.completed_at_ms.is_some());
        if let Some(exchange) = exchange {
            if completed {
                let agents = agent
                    .iter()
                    .filter_map(|entry| match entry {
                        TimelineEntry::AgentLifecycle { id, .. } => Some((id.as_str(), entry)),
                        _ => None,
                    })
                    .collect();
                self.interaction(exchange, &agents, depth)?;
            } else {
                if let Some(message) = exchange.turn.iter().rev().find_map(|turn| {
                    turn.messages().iter().rev().find(|message| {
                        !message.text().is_empty()
                            && message.delivery() != crate::turn::MessageDelivery::Final
                            && message.kind() == crate::turn::MessageKind::Assistant
                    })
                }) {
                    self.markdown(
                        &format!("{id}:thought"),
                        message.text(),
                        MarkdownRole::Commentary,
                    )?;
                }
                if let Some((turn, tool)) = exchange
                    .turn
                    .iter()
                    .rev()
                    .find_map(|turn| turn.tools().last().map(|tool| (turn, tool)))
                {
                    self.tool(turn.id(), tool, "", false)?;
                }
            }
        }
        let heading = section.start;
        self.finish_section(section, id, completed);
        let heading = &mut self.block[heading];
        if let Some(node) = heading.metadata.node.as_mut() {
            node.kind = NodeKind::Exchange;
            node.lifecycle = if completed {
                NodeLifecycle::Settled
            } else {
                NodeLifecycle::Live
            };
        }
        Ok(())
    }

    fn question(
        &mut self,
        id: &str,
        question: &crate::plan::PlanQuestionSet,
        answer: Option<&str>,
    ) -> Result<()> {
        let header = question
            .questions
            .iter()
            .map(|question| question.header.as_str())
            .collect::<Vec<_>>()
            .join(", ");
        self.literal(
            id,
            &format!("▸ Question presented: {header}"),
            Some(TranscriptAction::Question {
                question_set_id: question.id.clone(),
            }),
        )?;
        let section = self.begin_section(2);
        for (index, question) in question.questions.iter().enumerate() {
            self.markdown(
                &format!("{id}:question:{index}"),
                &question.question,
                MarkdownRole::Detail,
            )?;
            for (option, value) in question.options.iter().enumerate() {
                self.literal(
                    &format!("{id}:question:{index}:option:{option}"),
                    &format!("{}: {}", value.label, value.description),
                    None,
                )?;
            }
        }
        if let Some(answer) = answer {
            self.markdown(&format!("{id}:answer"), answer, MarkdownRole::Detail)?;
        }
        self.finish_section(section, id, true);
        Ok(())
    }

    fn declaration_comparison(
        &mut self,
        status_id: &str,
        execution: &ExecutionStatus,
    ) -> Result<()> {
        let identity = format!("{status_id}:implementation:{}", execution.id);
        self.literal(&identity, "▸ Declaration comparison", None)?;
        let section = self.begin_section(2);
        let progress = &execution.progress;
        let mut file = BTreeMap::<&str, Vec<(&str, &str)>>::new();
        for (state, findings) in [
            ("missing", &progress.missing),
            ("different", &progress.different),
            ("unverified", &progress.unverified),
        ] {
            for finding in findings {
                let (path, detail) = finding.split_once(": ").unwrap_or((finding, ""));
                file.entry(path).or_default().push((state, detail));
            }
        }
        for (path, findings) in &file {
            let file_id = format!("{identity}:{}", crate::plan::digest(path.as_bytes()));
            let state = findings
                .iter()
                .map(|(state, _)| *state)
                .collect::<std::collections::BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
                .join(" · ");
            self.literal(&file_id, &format!("▸ {path} · {state}"), None)?;
            let detail_section = self.begin_section(2);
            for (index, (state, detail)) in findings.iter().enumerate() {
                self.literal(
                    &format!("{file_id}:finding:{index}"),
                    if detail.is_empty() { state } else { detail },
                    None,
                )?;
            }
            self.finish_section(detail_section, &file_id, true);
        }
        if !progress.matched.is_empty() {
            let matched_id = format!("{identity}:matched");
            self.literal(
                &matched_id,
                &format!("▸ Matched files · {}", progress.matched.len()),
                None,
            )?;
            let matched_section = self.begin_section(2);
            for path in &progress.matched {
                self.literal(
                    &format!("{identity}:{}", crate::plan::digest(path.as_bytes())),
                    path,
                    None,
                )?;
            }
            self.finish_section(matched_section, &matched_id, true);
        }
        self.finish_section(section, &identity, true);
        Ok(())
    }

    fn interaction(
        &mut self,
        interaction: &Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
    ) -> Result<()> {
        if interaction.lifecycle.as_deref() == Some("agent_admission") {
            return self.content(
                interaction,
                agents,
                depth,
                &interaction.node_list.iter().collect::<Vec<_>>(),
                None,
                MarkdownRole::Response,
            );
        }
        if depth == 0 && self.leading_separator {
            self.literal(&format!("{}:separator", interaction.id), "", None)?;
        }
        let layout = super::layout::ExchangeLayout::new(interaction)?;
        self.content(
            interaction,
            agents,
            depth,
            &layout.opening,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        if let Some(lifecycle) = &interaction.lifecycle {
            self.literal(
                &format!("{}:lifecycle", interaction.id),
                &format!("◇ {lifecycle}"),
                None,
            )?;
        } else if !interaction.prompt.is_empty() {
            let prompt = TranscriptRenderer::new(&self.content_width())?.prompt(
                BlockId(format!("{}:prompt", interaction.id)),
                &interaction.prompt,
            )?;
            self.prompt.push(prompt.id.clone());
            self.push(prompt)?;
        }
        let summary = exchange_activity_summary(interaction, self.now_ms);
        let mut heading = TranscriptRenderer::new(&self.content_width())?.literal(
            BlockId(format!("{}:summary", interaction.id)),
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
            capture: if matches!(
                interaction.kind,
                ExchangeKind::PlanDraft | ExchangeKind::PlanRevision
            ) {
                "ForgeHarnessPlan".into()
            } else if let Some(mode) = interaction.mode {
                format!(
                    "ForgeHarness{}",
                    match mode {
                        crate::session::PermissionMode::Yolo => "Yolo",
                        mode => mode.label(),
                    }
                )
            } else if interaction.completed_at_ms.is_some() {
                "ForgeHarnessThought".into()
            } else {
                "ForgeHarnessThinking".into()
            },
            priority: 100,
        });
        let capture = heading.metadata.decoration[0].capture.clone();
        TranscriptRenderer::heading_marker(&mut heading, &capture);
        self.push(heading)?;
        let section = self.begin_section(2);
        self.content(
            interaction,
            agents,
            depth,
            &layout.activity,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        let exchange_start = section.start;
        self.finish_section(
            section,
            &format!("{}:exchange", interaction.id),
            interaction.completed_at_ms.is_some(),
        );
        if let Some(node) = &mut self.block[exchange_start].metadata.node {
            node.kind = NodeKind::Exchange;
            node.lifecycle = if interaction.completed_at_ms.is_some() {
                NodeLifecycle::Settled
            } else {
                NodeLifecycle::Live
            };
        }
        self.interaction_results(interaction)?;
        self.content(
            interaction,
            agents,
            depth,
            &layout.results,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        self.verification_result(interaction)?;
        self.content(
            interaction,
            agents,
            depth,
            &layout.conclusion,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        let mut continuation = layout.continuation.as_slice();
        while let Some(node) = continuation.first() {
            let response = super::layout::is_final_response(interaction, node)?;
            let mut count = 1;
            while count < continuation.len()
                && super::layout::is_final_response(interaction, continuation[count])? == response
            {
                count += 1;
            }
            let (content, remaining) = continuation.split_at(count);
            let start = self.block.len();
            let indent = if response { 0 } else { 2 };
            self.margin += indent;
            self.content(
                interaction,
                agents,
                depth,
                content,
                Some(&layout.questions),
                MarkdownRole::Response,
            )?;
            self.margin -= indent;
            self.offset_layout(start, indent / 2);
            continuation = remaining;
        }
        Ok(())
    }

    /// Project stable interaction summaries while retaining each thought's original activity.
    fn interaction_results(&mut self, interaction: &Exchange) -> Result<()> {
        let nodes = interaction.node_list.iter().collect::<Vec<_>>();
        let content = interaction.thoughts(&nodes)?;
        let writes = content
            .thought
            .iter()
            .flat_map(|thought| thought.writes.call.iter().map(|call| call.tool))
            .collect::<Vec<_>>();
        let identity = format!("{}:results", interaction.id);
        if !writes.is_empty() {
            self.file_change_group(&identity, &writes)?;
        }
        let runs = interaction
            .node_list
            .iter()
            .filter_map(|node| match node {
                ExchangeNode::TimelineEvent { event } => match event.as_ref() {
                    crate::timeline::TimelineEvent::Check { run }
                        if run.state != crate::checks::CheckState::Running =>
                    {
                        Some(run.as_ref())
                    }
                    _ => None,
                },
                _ => None,
            })
            .collect::<Vec<_>>();
        let Some(latest) = runs.last() else {
            return Ok(());
        };
        let mut combined = (**latest).clone();
        combined.id = identity;
        combined.completed_plan = None;
        combined.commands = runs
            .iter()
            .flat_map(|run| run.commands.iter())
            .filter(|command| {
                !matches!(
                    command.state,
                    crate::checks::CheckState::Pending | crate::checks::CheckState::Skipped
                )
            })
            .map(|command| {
                let mut command = command.clone();
                command.id = format!("{}:command:{}", combined.id, command.id);
                command
            })
            .collect();
        let heading = format!(
            "Checks finished · {} · {:.1}s",
            combined.summary(),
            combined.duration_ms() as f64 / 1000.0
        );
        self.checks(&combined, false, Some(&heading))
    }

    fn verification_result(&mut self, interaction: &Exchange) -> Result<()> {
        use crate::plan::{PlanPhase, execution::VerificationOutcome};
        let Some(phase) = interaction
            .execution_phase
            .as_ref()
            .filter(|phase| phase.phase == PlanPhase::Verify)
        else {
            return Ok(());
        };
        let Some(outcome) = phase.outcome else {
            return Ok(());
        };
        let identity = format!("{}:verification-result", interaction.id);
        if outcome == VerificationOutcome::Passed
            && phase
                .verification
                .as_ref()
                .is_none_or(|report| report.checks.is_empty())
        {
            let summary = phase
                .summary
                .as_deref()
                .unwrap_or("")
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ");
            return self.literal(
                &identity,
                &format!(
                    "◇ Verification passed{}",
                    if summary.is_empty() {
                        String::new()
                    } else {
                        format!(" · {summary}")
                    }
                ),
                None,
            );
        }
        let label = match outcome {
            VerificationOutcome::Failed => "Verification failed",
            VerificationOutcome::Blocked => "Verification blocked",
            VerificationOutcome::Passed => "Verification passed",
        };
        let checks = phase
            .verification
            .as_ref()
            .map_or(&[][..], |report| report.checks.as_slice());
        let failed = checks
            .iter()
            .filter(|check| check.outcome == VerificationOutcome::Failed)
            .count();
        let blocked = checks
            .iter()
            .filter(|check| check.outcome == VerificationOutcome::Blocked)
            .count();
        let mut totals = Vec::new();
        if failed > 0 {
            totals.push(format!(
                "{failed} failed check{}",
                if failed == 1 { "" } else { "s" }
            ));
        }
        if blocked > 0 {
            totals.push(format!(
                "{blocked} blocked check{}",
                if blocked == 1 { "" } else { "s" }
            ));
        }
        if !phase.declaration_findings.is_empty() {
            totals.push(format!(
                "{} declaration mismatch{}",
                phase.declaration_findings.len(),
                if phase.declaration_findings.len() == 1 {
                    ""
                } else {
                    "es"
                }
            ));
        }
        let suffix = if totals.is_empty() {
            String::new()
        } else {
            format!(" · {}", totals.join(", "))
        };
        self.literal(&identity, &format!("▸ {label}{suffix}"), None)?;
        let section = self.begin_section(2);
        for (category, label) in [
            (
                crate::plan::execution::VerificationCategory::Automated,
                "Automated",
            ),
            (
                crate::plan::execution::VerificationCategory::Manual,
                "Manual",
            ),
        ] {
            let group_checks = checks
                .iter()
                .filter(|check| check.category == category)
                .collect::<Vec<_>>();
            if group_checks.is_empty() {
                continue;
            }
            let group_id = format!("{identity}:{label}");
            let counts = [
                (VerificationOutcome::Passed, "passed"),
                (VerificationOutcome::Failed, "failed"),
                (VerificationOutcome::Blocked, "blocked"),
            ]
            .into_iter()
            .filter_map(|(outcome, label)| {
                let count = group_checks
                    .iter()
                    .filter(|check| check.outcome == outcome)
                    .count();
                (count > 0).then(|| format!("{count} {label}"))
            })
            .collect::<Vec<_>>()
            .join(", ");
            self.literal(&group_id, &format!("▸ {label} · {counts}"), None)?;
            let group = self.begin_section(2);
            for (index, check) in group_checks.into_iter().enumerate() {
                let check_id = format!("{group_id}:{index}");
                let outcome = match check.outcome {
                    VerificationOutcome::Passed => "passed",
                    VerificationOutcome::Failed => "failed",
                    VerificationOutcome::Blocked => "blocked",
                };
                self.literal(
                    &check_id,
                    &format!("▸ {} · {outcome}", check.label.replace(['\r', '\n'], " ")),
                    None,
                )?;
                let check_section = self.begin_section(2);
                self.markdown(
                    &format!("{check_id}:summary"),
                    check.summary.trim_end(),
                    MarkdownRole::Detail,
                )?;
                for (evidence_index, evidence) in check.evidence.iter().enumerate() {
                    let call_id = phase
                        .verification_tools
                        .get(evidence)
                        .cloned()
                        .unwrap_or_else(|| format!("{}:{evidence}", interaction.id));
                    self.literal(
                        &format!("{check_id}:evidence:{evidence_index}"),
                        "Command output",
                        Some(TranscriptAction::Tool { call_id }),
                    )?;
                }
                self.finish_section(check_section, &check_id, true);
            }
            self.finish_section(group, &group_id, true);
        }
        if !phase.declaration_findings.is_empty() || !phase.declaration_changes.is_empty() {
            let declaration_id = format!("{identity}:declarations");
            let count = phase
                .declaration_findings
                .len()
                .max(phase.declaration_changes.len());
            self.literal(
                &declaration_id,
                &format!(
                    "▸ Declarations · {count} mismatch{}",
                    if count == 1 { "" } else { "es" }
                ),
                None,
            )?;
            let declarations = self.begin_section(2);
            let mut files = std::collections::BTreeMap::<
                &str,
                Vec<&crate::plan::execution::DeclarationMismatch>,
            >::new();
            for change in &phase.declaration_changes {
                files.entry(&change.path).or_default().push(change);
            }
            for (file_index, (path, changes)) in files.into_iter().enumerate() {
                let file_id = format!("{declaration_id}:{file_index}");
                self.literal(
                    &file_id,
                    &format!(
                        "▸ {path} · {} mismatch{}",
                        changes.len(),
                        if changes.len() == 1 { "" } else { "es" }
                    ),
                    None,
                )?;
                let file = self.begin_section(2);
                for (index, change) in changes.iter().enumerate() {
                    let tree = super::changes::ChangeTree::declaration(
                        &TranscriptRenderer::new(&self.content_width())?,
                        &format!("{file_id}:{index}"),
                        &change.name,
                        &change.diff,
                    )?;
                    self.action.extend(tree.action);
                    for block in tree.block {
                        self.push(block)?;
                    }
                }
                self.finish_section(file, &file_id, true);
            }
            for (index, finding) in phase.declaration_findings.iter().enumerate() {
                if phase
                    .declaration_changes
                    .iter()
                    .any(|change| finding == &change.finding)
                {
                    continue;
                }
                self.markdown(
                    &format!("{declaration_id}:finding:{index}"),
                    finding,
                    MarkdownRole::Detail,
                )?;
            }
            self.finish_section(declarations, &declaration_id, true);
        }
        let findings = phase
            .verification
            .as_ref()
            .map_or(phase.findings.as_slice(), |report| {
                report.findings.as_slice()
            });
        if !findings.is_empty() {
            let finding_id = format!("{identity}:findings");
            self.literal(
                &finding_id,
                &format!("▸ Findings · {}", findings.len()),
                None,
            )?;
            let finding_section = self.begin_section(2);
            for (index, finding) in findings.iter().enumerate() {
                self.markdown(
                    &format!("{finding_id}:{index}"),
                    finding,
                    MarkdownRole::Detail,
                )?;
            }
            self.finish_section(finding_section, &finding_id, true);
        }
        if checks.is_empty() && findings.is_empty() && phase.declaration_findings.is_empty() {
            if let Some(reason) = phase
                .verification
                .as_ref()
                .and_then(|report| report.reason.as_deref())
                .or(phase.summary.as_deref())
            {
                self.markdown(&format!("{identity}:reason"), reason, MarkdownRole::Detail)?;
            }
        }
        self.finish_section(section, &identity, true);
        Ok(())
    }

    fn content<'exchange>(
        &mut self,
        interaction: &'exchange Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
        visible: &[&'exchange ExchangeNode],
        questions: Option<&super::question::QuestionHistory<'exchange>>,
        response_role: MarkdownRole,
    ) -> Result<()> {
        let content = interaction.thoughts(visible)?;
        for part in &content.part {
            match part {
                crate::exchange::ThoughtPart::Boundary(nodes) => {
                    self.content_nodes(interaction, agents, depth, nodes, questions)?;
                }
                crate::exchange::ThoughtPart::Thought(index) => {
                    self.thought(
                        interaction,
                        agents,
                        depth,
                        &content.thought[*index],
                        questions,
                        response_role,
                    )?;
                }
            }
        }
        Ok(())
    }

    fn thought<'exchange>(
        &mut self,
        interaction: &'exchange Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
        thought: &crate::exchange::ExchangeThought<'exchange>,
        questions: Option<&super::question::QuestionHistory<'exchange>>,
        response_role: MarkdownRole,
    ) -> Result<()> {
        let section = if let Some(message) = thought.message {
            if message.delivery() == crate::turn::MessageDelivery::Final {
                self.markdown(thought.id, message.text(), response_role)?;
                None
            } else {
                self.markdown(thought.id, message.text(), MarkdownRole::Commentary)?;
                thought.has_activity().then(|| self.begin_section(2))
            }
        } else {
            None
        };
        self.content_nodes(interaction, agents, depth, &thought.events, questions)?;
        self.tool_group(&thought.tools)?;
        if let Some(first) = thought.writes.call.first() {
            let calls = thought
                .writes
                .call
                .iter()
                .map(|call| call.tool)
                .collect::<Vec<_>>();
            self.file_change_group(first.id, &calls)?;
        }
        if let Some(section) = section {
            let start = section.start;
            self.finish_section(section, thought.id, true);
            self.block[start]
                .metadata
                .node
                .as_mut()
                .expect("thought node")
                .kind = NodeKind::Thought;
        }
        Ok(())
    }

    fn tool_group(&mut self, group: &crate::exchange::ThoughtGroup<'_>) -> Result<()> {
        let Some(first) = group.call.first() else {
            return Ok(());
        };
        let start = self.block.len();
        let settled = !group.live;
        let lifecycle = if settled {
            NodeLifecycle::Settled
        } else {
            NodeLifecycle::Live
        };
        let group_id = format!("{}:tools", first.id);
        let label = tool_group_label(
            group.call.iter().map(|call| call.tool),
            lifecycle,
            self.now_ms,
        );
        self.literal(&group_id, &label, None)?;
        for (index, call) in group.call.iter().enumerate() {
            let tool_start = self.block.len();
            self.tool(
                call.turn.id(),
                call.tool,
                &group_id,
                !settled && index + 1 == group.call.len(),
            )?;
            let node_id = format!("{}:{}:tool", call.turn.id(), call.tool.id);
            self.fold(
                tool_start,
                &node_id,
                settled || index + 1 < group.call.len(),
                NodeKind::Tool,
            );
            if let Some(node) = &mut self.block[tool_start].metadata.node {
                node.lifecycle = if call.tool.state() == crate::turn::ToolState::Running {
                    NodeLifecycle::Live
                } else {
                    NodeLifecycle::Settled
                };
                if !settled && index + 1 == group.call.len() {
                    node.default_display = NodeDisplay::Preview;
                }
                if self.expansion.get(&node_id) == Some(&true) {
                    let output = &self.tool[&format!("{}:{}", call.turn.id(), call.tool.id)];
                    let limit = self
                        .tool_limit
                        .get(&node_id)
                        .copied()
                        .unwrap_or(super::tool::INITIAL_INLINE_BYTES);
                    node.more = output.inline_prefix_chunks(limit)? < output.inline_chunks()?;
                }
                node.resolve(self.expansion.get(&node_id).copied());
            }
        }
        self.fold(start, &group_id, settled, NodeKind::ToolGroup);
        let node = self.block[start]
            .metadata
            .node
            .as_mut()
            .expect("tool group node");
        node.lifecycle = lifecycle;
        node.default_display = if settled {
            NodeDisplay::Heading
        } else {
            NodeDisplay::Preview
        };
        node.resolve(self.expansion.get(&group_id).copied());
        Ok(())
    }

    fn content_nodes<'exchange>(
        &mut self,
        interaction: &'exchange Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
        visible: &[&'exchange ExchangeNode],
        questions: Option<&super::question::QuestionHistory<'exchange>>,
    ) -> Result<()> {
        for (position, node) in visible.iter().enumerate() {
            if let Some(group) = questions.and_then(|history| history.group.get(node.id())) {
                self.question_group(group, interaction, agents, depth)?;
                continue;
            }
            match node {
                ExchangeNode::TimelineEvent { event } => match event.as_ref() {
                    crate::timeline::TimelineEvent::Check { run } => {
                        self.checks(run, false, None)?
                    }
                    crate::timeline::TimelineEvent::Session { event } => {
                        self.session_event(event)?
                    }
                },
                ExchangeNode::SkillRead { id, read, .. } => {
                    self.literal(
                        id,
                        &format!("◇ Read skill · {}", read.name),
                        Some(TranscriptAction::File {
                            path: read.path.clone(),
                            line: 1,
                        }),
                    )?;
                }
                ExchangeNode::Compaction { id, compaction } => self.compaction(id, compaction)?,
                ExchangeNode::TurnContent { .. } => {
                    anyhow::bail!("provider content must be rendered through its thought owner");
                }
                ExchangeNode::PlanEvent { event } => {
                    self.plan_event(event)?;
                }
                ExchangeNode::ExchangeInput { prompt } => {
                    if matches!(prompt.intent, crate::exchange::InputIntent::Clarification | crate::exchange::InputIntent::Answer)
                        && interaction.node_list.iter().any(|node| matches!(node,
                            ExchangeNode::PlanEvent { event } if matches!(&event.content,
                                crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                                if lifecycle.kind == crate::plan::PlanLifecycleKind::QuestionAnswered
                                    && lifecycle.answer.as_deref() == Some(prompt.text.as_str())))) {
                        continue;
                    }
                    let label = match prompt.intent {
                        crate::exchange::InputIntent::Steering => "Steering",
                        crate::exchange::InputIntent::Clarification => "You asked",
                        crate::exchange::InputIntent::Answer => "You answered",
                    };
                    let text = if prompt.intent == crate::exchange::InputIntent::Answer {
                        prompt
                            .text
                            .trim()
                            .strip_prefix("Planning feedback:")
                            .unwrap_or(&prompt.text)
                            .trim()
                            .trim_start_matches("- ")
                            .replace("\n- ", ", ")
                    } else {
                        prompt.text.clone()
                    };
                    let width = self.content_width();
                    let renderer = TranscriptRenderer::new(&width)?;
                    let block = renderer.prompt(
                        BlockId(format!("{}:prompt", prompt.id)),
                        &format!("{label}: {text}"),
                    )?;
                    self.prompt.push(block.id.clone());
                    self.push(block)?;
                }
                ExchangeNode::AgentReference { agent } => {
                    if let Some(entry) = agents
                        .get(agent.id.as_str())
                        .or_else(|| agents.get(agent.child_agent_id.as_str()))
                        .copied()
                    {
                        if let TimelineEntry::AgentLifecycle {
                            id: _,
                            run,
                            exchange: interaction,
                            agent: child,
                            ..
                        } = entry
                        {
                            self.agent_entry(
                                &agent.id,
                                run,
                                &agent.task,
                                interaction,
                                child,
                                Some(&agent.child_exchange_id),
                                depth + 1,
                            )?;
                        }
                    } else {
                        self.literal(
                            &agent.id,
                            "Agent",
                            Some(TranscriptAction::Agent {
                                run_id: agent.child_agent_id.clone(),
                            }),
                        )?;
                    }
                }
                ExchangeNode::QuestionPresented {
                    id,
                    question,
                    answer,
                } => {
                    self.question(id, question, answer.as_deref())?;
                }
                ExchangeNode::PlanCommentResolution { resolution } => {
                    for (index, annotation) in resolution.annotation.iter().enumerate() {
                        let identity = format!("{}:comment:{index}", resolution.id);
                        self.literal(&identity, &format!("▸ Resolved {}", annotation.label), None)?;
                        let section = self.begin_section(2);
                        self.markdown(
                            &format!("{identity}:body"),
                            &annotation.body,
                            MarkdownRole::Detail,
                        )?;
                        self.finish_section(section, &identity, true);
                    }
                }
                ExchangeNode::ImplementationReport {
                    id,
                    reference,
                    report,
                } => {
                    self.literal(id, &format!("▸ {}", reference.summary()), None)?;
                    let section = self.begin_section(2);
                    if let Some(report) = report {
                        self.markdown(
                            &format!("{id}:revisions"),
                            &format!(
                                "Original revision {} · accepted revision {}\n{}",
                                report.original_revision,
                                report.accepted_revision,
                                report
                                    .revisions
                                    .iter()
                                    .map(|revision| format!(
                                        "- Revision {} ({}): {}",
                                        revision.revision, revision.approval, revision.reason
                                    ))
                                    .collect::<Vec<_>>()
                                    .join("\n")
                            ),
                            MarkdownRole::Detail,
                        )?;
                        for (index, file) in report.file.iter().enumerate() {
                            let file_id = format!("{id}:file:{index}");
                            self.literal(&file_id, &format!("▸ {}", file.path), None)?;
                            let file_section = self.begin_section(2);
                            let mut lines = Vec::new();
                            for detail in &file.contract {
                                lines.push(format!("- Contract: {detail}"));
                            }
                            for helper in &file.internal {
                                lines.push(format!("- Internal addition: `{helper}`"));
                            }
                            for difference in &file.references {
                                lines.push(format!(
                                    "- `{}` — {}{}",
                                    difference.owner,
                                    difference.category,
                                    if difference.expected.is_none() {
                                        " (unspecified in plan)"
                                    } else {
                                        ""
                                    }
                                ));
                                if let Some(expected) = &difference.expected {
                                    lines.push(format!("  Planned: {}", expected.join(", ")));
                                }
                                lines.push(format!(
                                    "  Observed: {}",
                                    difference.observed.join(", ")
                                ));
                            }
                            self.markdown(
                                &format!("{file_id}:details"),
                                &lines.join("\n"),
                                MarkdownRole::Detail,
                            )?;
                            self.finish_section(file_section, &file_id, true);
                        }
                        for (index, unavailable) in report.unavailable.iter().enumerate() {
                            self.markdown(
                                &format!("{id}:unavailable:{index}"),
                                &format!("Unavailable: {unavailable}"),
                                MarkdownRole::Detail,
                            )?;
                        }
                    } else {
                        self.markdown(
                            &format!("{id}:unavailable"),
                            "Report content is unavailable. The saved summary is retained.",
                            MarkdownRole::Detail,
                        )?;
                    }
                    self.finish_section(section, id, true);
                }
                ExchangeNode::ArtifactChange { change } => {
                    if let Some(declaration) = &change.declaration {
                        let revision_request = interaction.kind == ExchangeKind::PlanExecution;
                        let revision_section = if revision_request {
                            self.literal(
                                &format!("{}:request", change.id),
                                &format!(
                                    "▸ Plan revision requested · revision {}",
                                    declaration.revision
                                ),
                                None,
                            )?;
                            Some(self.begin_section(2))
                        } else {
                            None
                        };
                        self.diff(
                            &change.id,
                            if revision_request {
                                "Changes"
                            } else {
                                "Proposed changes"
                            },
                            &change.diff_text,
                            "",
                            None,
                            Some((declaration, false)),
                        )?;
                        self.diff(
                            &format!("{}:document", change.id),
                            "Plan overview",
                            &declaration.document_diff,
                            "",
                            None,
                            Some((declaration, true)),
                        )?;
                        if let Some(section) = revision_section {
                            self.finish_section(section, &format!("{}:request", change.id), true);
                        }
                        continue;
                    }
                    let label = visible
                        .iter()
                        .skip(position + 1)
                        .take_while(|node| !matches!(node, ExchangeNode::ArtifactChange { .. }))
                        .find_map(|node| match node {
                            ExchangeNode::PlanEvent { event } => match &event.content {
                                crate::plan::PlanEventContent::Lifecycle { title, lifecycle }
                                    if matches!(
                                        lifecycle.kind,
                                        crate::plan::PlanLifecycleKind::Created
                                            | crate::plan::PlanLifecycleKind::RevisionCreated
                                    ) =>
                                {
                                    Some(format!(
                                        "Artifact: {title} (revision {})",
                                        lifecycle.model_revision
                                    ))
                                }
                                _ => None,
                            },
                            _ => None,
                        });
                    self.diff(
                        &change.id,
                        "Changed",
                        &change.diff_text,
                        "",
                        label.as_deref().map(|label| (change.path.as_str(), label)),
                        None,
                    )?
                }
            }
        }
        Ok(())
    }

    fn compaction(&mut self, id: &str, compaction: &crate::backend::Compaction) -> Result<()> {
        use crate::backend::CompactionState;
        let label = match compaction.state {
            CompactionState::Running => return Ok(()),
            CompactionState::Completed => "Context compacted",
            CompactionState::Failed => "Context compaction failed",
            CompactionState::Unconfirmed => "Context compaction completion unconfirmed",
        };
        let mut heading = format!("◇ {label}");
        let context_tokens = compaction
            .result
            .context_tokens_before
            .zip(compaction.result.context_tokens_after)
            .or_else(|| {
                compaction
                    .result
                    .before_context
                    .as_ref()
                    .and_then(|context| context.current_tokens)
                    .zip(
                        compaction
                            .result
                            .context_window
                            .as_ref()
                            .and_then(|context| context.current_tokens),
                    )
            });
        if let Some((before, after)) = context_tokens {
            heading.push_str(&format!(
                " · {} → {} tokens",
                token_display(before),
                token_display(after)
            ));
        }
        if let (Some(started), Some(completed)) =
            (compaction.started_at_ms, compaction.completed_at_ms)
        {
            heading.push_str(&format!(
                " · {}s",
                completed.saturating_sub(started).max(0) / 1_000
            ));
        }
        self.literal(id, &heading, None)?;
        let mut details = Vec::new();
        if let Some(count) = compaction.result.pre_compaction_tokens {
            details.push(format!("Conversation tokens before compaction: {count}"));
        }
        if let Some(count) = compaction.result.post_compaction_tokens {
            details.push(format!("Conversation tokens after compaction: {count}"));
        }
        if let Some(model) = &compaction.result.model {
            details.push(format!("Compaction model: `{model}`"));
        }
        if let Some(instructions) = &compaction.result.custom_instructions {
            details.push(format!("Focus instructions: {instructions}"));
        }
        if let Some(status) = compaction.result.status_code {
            details.push(format!("HTTP status: {status}"));
        }
        if let Some(request) = &compaction.result.request_id {
            details.push(format!("Request ID: `{request}`"));
        }
        if let Some(request) = &compaction.result.service_request_id {
            details.push(format!("Service request ID: `{request}`"));
        }
        if let Some(error) = &compaction.result.error {
            let label = if compaction.state == CompactionState::Failed {
                "Failure"
            } else {
                "Explanation"
            };
            details.push(format!("{label}: {error}"));
        }
        if let Some(count) = compaction.result.tokens_removed {
            details.push(format!("Tokens removed: {count}"));
        }
        if let Some(count) = compaction.result.messages_removed {
            details.push(format!("Messages removed: {count}"));
        }
        if let Some(count) = compaction.result.pre_compaction_messages_length {
            details.push(format!("Messages before compaction: {count}"));
        }
        if let Some(count) = compaction.result.checkpoint_number {
            details.push(format!("Checkpoint: {count}"));
        }
        if let Some(path) = &compaction.result.checkpoint_path {
            details.push(format!("Checkpoint path: `{path}`"));
        }
        if let Some(usage) = &compaction.result.compaction_tokens_used {
            if let Some(cost) = &usage.billing {
                if let Some(total) = cost.total_nano_aiu {
                    details.push(format!("Provider cost: {total} nano-AI units"));
                }
                for category in cost.token_details.as_deref().unwrap_or_default() {
                    let name = category.token_type.as_deref().unwrap_or("unspecified");
                    if let Some(count) = category.token_count {
                        details.push(format!("Billed {name} tokens: {count}"));
                    }
                    if let Some(count) = category.batch_size {
                        details.push(format!("Billing {name} batch size: {count}"));
                    }
                    if let Some(count) = category.cost_per_batch {
                        details.push(format!("Billing {name} cost per batch: {count}"));
                    }
                }
            }
            if let Some(model) = &usage.model {
                details.push(format!("Compaction request model: `{model}`"));
            }
            if let Some(duration) = usage.duration_ms {
                details.push(format!("Compaction request duration: {duration} ms"));
            }
            for (name, count) in [
                ("Compaction input tokens", usage.input),
                ("Compaction output tokens", usage.output),
                ("Compaction cached input tokens", usage.cached_input),
            ] {
                if let Some(count) = count {
                    details.push(format!("{name}: {count}"));
                }
            }
            if let Some(count) = usage.cache_write_tokens {
                details.push(format!("Compaction cache write tokens: {count}"));
            }
        }
        if let Some(context) = &compaction.result.before_context {
            for (name, count) in [
                (
                    "Conversation tokens before compaction",
                    context.conversation_tokens,
                ),
                ("System tokens before compaction", context.system_tokens),
                (
                    "Tool definition tokens before compaction",
                    context.tool_definitions_tokens,
                ),
            ] {
                if let Some(count) = count {
                    details.push(format!("{name}: {count}"));
                }
            }
        }
        let context = compaction.result.context_window.as_ref();
        for (name, count) in [
            (
                "Conversation tokens",
                compaction
                    .result
                    .conversation_tokens
                    .or_else(|| context.and_then(|context| context.conversation_tokens)),
            ),
            (
                "System tokens",
                compaction
                    .result
                    .system_tokens
                    .or_else(|| context.and_then(|context| context.system_tokens)),
            ),
            (
                "Tool definition tokens",
                compaction
                    .result
                    .tool_definitions_tokens
                    .or_else(|| context.and_then(|context| context.tool_definitions_tokens)),
            ),
            (
                "Context tokens",
                context.and_then(|context| context.current_tokens),
            ),
            (
                "Context capacity",
                context.and_then(|context| context.token_limit),
            ),
            (
                "Retained messages",
                context.and_then(|context| context.messages_length),
            ),
        ] {
            if let Some(count) = count {
                details.push(format!("{name}: {count}"));
            }
        }
        let summary = compaction
            .result
            .summary_content
            .as_deref()
            .filter(|summary| !summary.trim().is_empty());
        if details.is_empty() && summary.is_none() {
            return Ok(());
        }
        let section = self.begin_section(2);
        if !details.is_empty() {
            self.markdown(
                &format!("{id}:metrics"),
                &details.join("\n\n"),
                MarkdownRole::Detail,
            )?;
        }
        if let Some(summary) = summary {
            self.markdown(&format!("{id}:summary"), summary, MarkdownRole::Detail)?;
        }
        self.finish_section(section, id, true);
        Ok(())
    }

    fn question_group(
        &mut self,
        group: &super::question::QuestionGroup<'_>,
        exchange: &Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
    ) -> Result<()> {
        let cancelled =
            exchange.state == crate::exchange::ExchangeState::Cancelled && !group.resolved;
        let settled = group.resolved || cancelled;
        let answered = group
            .branch
            .iter()
            .filter(|branch| branch.answer.is_some())
            .count();
        let label = if group.withdrawal.is_some() {
            "▸ Questions · Withdrawn".into()
        } else if cancelled {
            format!(
                "▸ Questions · Cancelled · {answered}/{} answered",
                group.branch.len()
            )
        } else {
            format!("▸ Questions · {answered}/{} answered", group.branch.len())
        };
        self.literal(
            group.id,
            &label,
            (!cancelled).then(|| TranscriptAction::Question {
                question_set_id: group.set.id.clone(),
            }),
        )?;
        let section = self.begin_section(2);
        for branch in &group.branch {
            let id = format!("{}:question:{}", group.id, branch.question.id);
            let status = branch
                .answer
                .as_deref()
                .unwrap_or(if group.withdrawal.is_some() {
                    "Withdrawn"
                } else if cancelled {
                    "Cancelled"
                } else if group.resolved {
                    "Resolved"
                } else {
                    "Awaiting answer"
                });
            self.literal(
                &id,
                &format!("▸ {} · {status}", branch.question.question),
                (!cancelled).then(|| TranscriptAction::Question {
                    question_set_id: group.set.id.clone(),
                }),
            )?;
            let question_section = self.begin_section(2);
            for (index, option) in branch.question.options.iter().enumerate() {
                self.literal(
                    &format!("{id}:option:{index}"),
                    &format!("○ {}: {}", option.label, option.description),
                    None,
                )?;
            }
            for (index, clarification) in branch.clarification.iter().enumerate() {
                self.clarification(
                    clarification,
                    exchange,
                    agents,
                    depth,
                    settled || index + 1 < branch.clarification.len(),
                )?;
            }
            if let Some(answer) = &branch.answer {
                let label = if branch.source == crate::session::ResolutionSource::Timeout {
                    "Automatic timeout"
                } else {
                    "You answered"
                };
                let mut block = TranscriptRenderer::new(&self.content_width())?.literal(
                    BlockId(format!("{id}:answer")),
                    &format!("● {label}: {answer}"),
                    2,
                )?;
                TranscriptRenderer::heading_marker(&mut block, "ForgeHarnessPrompt");
                block.metadata.decoration.push(Decoration {
                    range: TextRange {
                        start: TextPosition { row: 0, column: 0 },
                        end: TextPosition {
                            row: block.text.row_count(),
                            column: 0,
                        },
                    },
                    capture: "ForgeHarnessPrompt".into(),
                    priority: 100,
                });
                self.prompt.push(block.id.clone());
                self.push(block)?;
            }
            self.finish_section(question_section, &id, settled);
        }
        for clarification in &group.clarification {
            self.clarification(clarification, exchange, agents, depth, settled)?;
        }
        if let Some(reason) = group.withdrawal {
            self.markdown(
                &format!("{}:withdrawal", group.id),
                &format!("Clarification withdrawn: {reason}"),
                MarkdownRole::Detail,
            )?;
        }
        self.finish_section(section, group.id, settled);
        Ok(())
    }

    fn clarification(
        &mut self,
        clarification: &super::question::Clarification<'_>,
        exchange: &Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
        closed: bool,
    ) -> Result<()> {
        let id = format!("{}:prompt", clarification.prompt.id);
        let block = TranscriptRenderer::new(&self.content_width())?.prompt(
            BlockId(id.clone()),
            &format!("You asked: {}", clarification.prompt.text),
        )?;
        self.prompt.push(block.id.clone());
        self.push(block)?;
        let section = self.begin_section(2);
        self.content(
            exchange,
            agents,
            depth,
            &clarification.content,
            None,
            MarkdownRole::Message,
        )?;
        self.finish_section(section, &id, closed);
        Ok(())
    }

    fn file_change_group(&mut self, id: &str, calls: &[&crate::exchange::ToolCall]) -> Result<()> {
        use crate::turn::ToolState;
        let identity = format!("{id}:changes");
        let start = self.block.len();
        let (diff, warning) = crate::exchange::ProviderDiffBuilder::build_group(calls);
        let pending: std::collections::HashSet<_> = calls
            .iter()
            .filter(|tool| tool.state() == crate::turn::ToolState::Running)
            .flat_map(|tool| tool.change.file.iter())
            .map(|file| file.move_path.as_ref().unwrap_or(&file.path))
            .collect();
        let running = calls
            .iter()
            .any(|tool| tool.state() == crate::turn::ToolState::Running);
        let unsuccessful: Vec<_> = calls
            .iter()
            .enumerate()
            .filter(|(_, tool)| {
                tool.state() != ToolState::Running
                    && (tool.failed || tool.state() != ToolState::Completed)
            })
            .collect();
        let unavailable = calls.iter().any(|tool| {
            tool.state() == ToolState::Completed && !tool.failed && tool.change.file.is_empty()
        });
        let mut outcome = String::new();
        if !unsuccessful.is_empty() {
            outcome.push_str(&format!(
                " · {} unsuccessful {}",
                unsuccessful.len(),
                if unsuccessful.len() == 1 {
                    "write"
                } else {
                    "writes"
                }
            ));
        }
        if unavailable {
            outcome.push_str(" · edit details unavailable");
        }
        if warning.is_some() {
            outcome.push_str(" · net diff unavailable");
        }
        if let Some(diff) = diff {
            let mut suffix = if running {
                if pending.is_empty() {
                    " · changing files".to_owned()
                } else {
                    format!(
                        " · changing {} {}",
                        pending.len(),
                        if pending.len() == 1 { "file" } else { "files" }
                    )
                }
            } else {
                String::new()
            };
            suffix.push_str(&outcome);
            self.diff(&identity, "Changed", &diff, &suffix, None, None)?;
        } else {
            let label = if running {
                if pending.is_empty() {
                    "Changing files".to_owned()
                } else {
                    format!(
                        "Changing {} {}",
                        pending.len(),
                        if pending.len() == 1 { "file" } else { "files" }
                    )
                }
            } else if unsuccessful.len() == calls.len() {
                "File changes".to_owned()
            } else if !unavailable && warning.is_none() {
                "No net file changes".to_owned()
            } else {
                "Changed files · details unavailable".to_owned()
            };
            self.literal(&identity, &format!("◇ {label}{outcome}"), None)?;
        }
        let details_start = self.block.len();
        self.margin += 2;
        if let Some(warning) = warning {
            self.literal(
                &format!("{identity}:warning"),
                &format!("◇ {warning}"),
                None,
            )?;
        }
        for (index, tool) in unsuccessful {
            self.file_change(&format!("{id}:write:{index}"), tool)?;
        }
        self.margin -= 2;
        if self.block.len() > details_start {
            self.offset_layout(details_start, 1);
            self.block[start].metadata.fold.clear();
            self.fold(start, &identity, true, NodeKind::Changes);
        }
        Ok(())
    }

    fn file_change(&mut self, id: &str, tool: &crate::exchange::ToolCall) -> Result<()> {
        use crate::turn::ToolState;
        let identity = format!("{id}:changes");
        if let Some(diff) = crate::exchange::ProviderDiffBuilder::build(std::slice::from_ref(tool))
        {
            return self.diff(&identity, "Changed", &diff, "", None, None);
        }
        let label = match tool.state() {
            ToolState::Running => {
                let count = tool.change.file.len();
                if count == 0 {
                    "Changing files".into()
                } else {
                    format!(
                        "Changing {count} {}",
                        if count == 1 { "file" } else { "files" }
                    )
                }
            }
            ToolState::Completed if !tool.failed => "Changed files · details unavailable".into(),
            ToolState::Completed | ToolState::Failed => "File changes failed".into(),
            ToolState::Cancelled => "File changes cancelled".into(),
            ToolState::Denied => "File changes denied".into(),
            ToolState::Interrupted => "File changes interrupted".into(),
        };
        let details = tool.state() != ToolState::Running && !tool.output.trim().is_empty();
        self.literal(
            &identity,
            &format!("{} {label}", if details { "▸" } else { "◇" }),
            None,
        )?;
        if details {
            let section = self.begin_section(2);
            self.markdown(
                &format!("{identity}:details"),
                &tool.output,
                MarkdownRole::Detail,
            )?;
            self.finish_section(section, &identity, true);
        }
        Ok(())
    }

    fn session_event(&mut self, event: &crate::timeline::SessionEventRecord) -> Result<()> {
        if let SessionEventKind::Compaction { compaction } = &event.detail {
            return self.compaction(&event.id, compaction);
        }
        let action = match &event.detail {
            SessionEventKind::Forked {
                source_session_id, ..
            } => Some(TranscriptAction::Session {
                session_id: source_session_id.clone(),
            }),
            _ => None,
        };
        self.literal(
            &event.id,
            &format!("◇ {}", session_event_text(&event.detail)),
            action,
        )
    }

    fn tool(
        &mut self,
        interaction: &str,
        tool: &crate::exchange::ToolCall,
        group: &str,
        preview: bool,
    ) -> Result<()> {
        let call_id = format!("{interaction}:{}", tool.id);
        ensure!(
            !self.tool.contains_key(&call_id),
            "duplicate tool identity in interaction"
        );
        let mut output = ToolOutputView::new(call_id.clone(), &tool.output)?;
        output.heading(tool);
        output.group = group.to_owned();
        output.live_preview = preview;
        let id = format!("{call_id}:tool");
        let target = TargetId(id.clone());
        let label = tool.title.clone();
        let width = self.content_width();
        let renderer = TranscriptRenderer::new(&width)?;
        let choice = self.expansion.get(&format!("{call_id}:tool")).copied();
        let expanded = !group.is_empty() && choice == Some(true);
        let preview = preview && choice != Some(false);
        let block = renderer.tool_header(
            BlockId(id),
            target.clone(),
            &tool.kind,
            tool.elapsed_ms(self.now_ms),
            tool.failed,
            &label,
            expanded,
        )?;
        self.action.insert(
            target,
            TranscriptAction::Tool {
                call_id: call_id.clone(),
            },
        );
        self.push(block)?;
        if expanded || preview {
            let limit = self
                .tool_limit
                .get(&format!("{call_id}:tool"))
                .copied()
                .unwrap_or(super::tool::INITIAL_INLINE_BYTES);
            for chunk in 0..if expanded {
                output.inline_prefix_chunks(limit)?
            } else {
                1
            } {
                let block = renderer.tool_body(&call_id, &output, expanded, chunk)?;
                for target in &block.metadata.target {
                    self.action.insert(
                        target.id.clone(),
                        TranscriptAction::Tool {
                            call_id: call_id.clone(),
                        },
                    );
                }
                self.push(block)?;
            }
            let hidden = renderer.tool_hidden(&call_id, &output, expanded)?;
            for target in &hidden.metadata.target {
                self.action.insert(
                    target.id.clone(),
                    TranscriptAction::Tool {
                        call_id: call_id.clone(),
                    },
                );
            }
            self.push(hidden)?;
        } else {
            for suffix in ["preview", "hidden"] {
                self.push(BufferBlock {
                    id: BlockId(format!("{call_id}:{suffix}")),
                    text: Default::default(),
                    metadata: Default::default(),
                })?;
            }
        }
        self.tool.insert(call_id, output);
        Ok(())
    }

    fn diff(
        &mut self,
        id: &str,
        label: &str,
        text: &str,
        suffix: &str,
        file_label: Option<(&str, &str)>,
        declaration: Option<(&crate::exchange::DeclarationRevision, bool)>,
    ) -> Result<()> {
        if text.is_empty() {
            return Ok(());
        }
        let tree = super::changes::ChangeTree::render(
            &TranscriptRenderer::new(&self.content_width())?,
            id,
            label,
            suffix,
            text,
            file_label,
            declaration,
        )?;
        self.action.extend(tree.action);
        for block in tree.block {
            self.push(block)?;
        }
        Ok(())
    }

    fn markdown(&mut self, id: &str, text: &str, role: MarkdownRole) -> Result<()> {
        let mut width = self.content_width();
        let comment = matches!(role, MarkdownRole::Comment);
        if comment {
            width.columns = width.columns.saturating_sub(2).max(1);
        }
        let mut rendered = match role {
            MarkdownRole::Response => {
                TranscriptRenderer::new(&width)?.response(BlockId(id.into()), text)?
            }
            MarkdownRole::Commentary => {
                TranscriptRenderer::new(&width)?.commentary(BlockId(id.into()), text)?
            }
            MarkdownRole::Message => {
                let mut rendered = forge_buffer::markdown::MarkdownRenderer::source(
                    BlockId(id.into()),
                    text,
                    &width,
                )?;
                rendered.block.metadata.markdown = true;
                rendered
            }
            MarkdownRole::Detail | MarkdownRole::Comment => {
                forge_buffer::markdown::MarkdownRenderer::render(BlockId(id.into()), text, &width)?
            }
        };
        if rendered.block.metadata.markdown && !matches!(role, MarkdownRole::Commentary) {
            rendered.block.metadata.decoration.clear();
        }
        if comment {
            rendered.block.metadata.layout = Some(ContentLayout {
                indent: 4,
                marker: Some(TextChunk {
                    text: "◦".into(),
                    capture: "Normal".into(),
                }),
                source_indent: 0,
            });
        }
        rendered
            .code
            .retain(|code| forge_diff::syntax::SyntaxLanguage::from_name(&code.language).is_some());
        if !rendered.code.is_empty() {
            let target = TargetId(format!("{id}:markdown-syntax"));
            self.syntax.insert(
                target.clone(),
                super::syntax::MarkdownSyntax {
                    target,
                    block: rendered.block.id.clone(),
                    text: rendered.block.text.clone(),
                    code: Arc::new(rendered.code),
                },
            );
        }
        rendered.block.metadata.node = Some(NodeState::new(
            FoldId(id.into()),
            if matches!(role, MarkdownRole::Commentary) {
                NodeKind::Thought
            } else {
                NodeKind::Message
            },
            NodeDisplay::Full,
        ));
        if matches!(role, MarkdownRole::Response) {
            rendered
                .block
                .metadata
                .node
                .as_mut()
                .expect("message node")
                .lifecycle = NodeLifecycle::Live;
        }
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
        let mut block =
            TranscriptRenderer::new(&self.content_width())?.literal(BlockId(id.into()), text, 2)?;
        TranscriptRenderer::heading_marker(&mut block, "Normal");
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

    fn push(&mut self, mut block: BufferBlock) -> Result<()> {
        block.metadata.layout.get_or_insert(ContentLayout {
            indent: 2,
            marker: None,
            source_indent: 0,
        });
        self.block.push(block);
        Ok(())
    }

    fn offset_layout(&mut self, start: usize, depth: usize) {
        for block in &mut self.block[start..] {
            block
                .metadata
                .layout
                .as_mut()
                .expect("projected block layout")
                .indent += depth * 2;
        }
    }

    fn content_width(&self) -> WidthProfile {
        let mut width = self.width.clone();
        width.columns = width.columns.saturating_sub(self.margin).max(1);
        width
    }

    fn begin_section(&mut self, child_indent: usize) -> Section {
        let section = Section {
            start: self.block.len() - 1,
            margin: self.margin,
            child_indent,
        };
        self.margin += child_indent;
        section
    }

    fn finish_section(&mut self, section: Section, identity: &str, closed: bool) {
        self.margin = section.margin;
        self.offset_layout(section.start + 1, section.child_indent / 2);
        self.fold(section.start, identity, closed, NodeKind::Group);
    }

    fn fold(&mut self, start: usize, identity: &str, closed: bool, kind: NodeKind) {
        self.block[start].metadata.node = Some(NodeState::new(
            FoldId(identity.into()),
            kind,
            if closed {
                NodeDisplay::Heading
            } else {
                NodeDisplay::Full
            },
        ));
        let selected = &self.block[start..];
        if selected
            .iter()
            .map(|block| block.text.row_count())
            .sum::<usize>()
            < 2
            && kind != NodeKind::Tool
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
            collapsed_suffix: None,
            heading_start: None,
            collapse_children: false,
            expand_children: false,
            id: FoldId(identity.into()),
            start: TextPosition { row: 0, column: 0 },
            end: anchor,
            closed,
        });
        TranscriptRenderer::fold_marker(&mut self.block[start]);
    }
}

fn agent_summary(run: &crate::agent::Agent, exchange: Option<&Exchange>, now_ms: i64) -> String {
    if let Some(exchange) = exchange {
        let lifecycle = if run.state == crate::agent::AgentState::Closing {
            "Closing"
        } else {
            match exchange.state {
                ExchangeState::Queued => "Starting",
                ExchangeState::Running
                    if exchange.activity() == ExchangeActivity::AwaitingApproval =>
                {
                    "Waiting for approval"
                }
                ExchangeState::Running
                    if exchange.activity() == ExchangeActivity::AwaitingInput =>
                {
                    "Waiting for your answer"
                }
                ExchangeState::Running if exchange.activity() == ExchangeActivity::Paused => {
                    "Paused"
                }
                ExchangeState::Running
                    if exchange.activity() == ExchangeActivity::WaitingForAgent =>
                {
                    "Waiting for agents"
                }
                ExchangeState::Running
                    if matches!(exchange.activity(), ExchangeActivity::Compacting { .. }) =>
                {
                    "Compacting"
                }
                ExchangeState::Running => "Running",
                ExchangeState::Finalizing => finalization_label(exchange.finalization_outcome),
                ExchangeState::Complete => "Ran",
                ExchangeState::Failed => "Failed",
                ExchangeState::Cancelled => "Cancelled",
                ExchangeState::Interrupted => "Interrupted",
            }
        };
        let selection = run
            .selection
            .as_ref()
            .map(|selection| {
                let speed = match selection.service_tier {
                    crate::backend::ServiceTier::Standard => "",
                    crate::backend::ServiceTier::Fast => " +",
                    crate::backend::ServiceTier::Ultrafast => " ++",
                };
                format!(
                    " · {} · {}{speed}",
                    crate::agent::provider_name(selection.provider),
                    selection.model
                )
            })
            .unwrap_or_default();
        let heading = format!(
            "● {lifecycle} {}s · {}{selection}",
            exchange.elapsed(now_ms) / 1000,
            run.label()
        );
        return exchange_metrics_summary(exchange, now_ms, heading);
    }

    use crate::agent::AgentState;

    let elapsed_seconds = now_ms
        .min(run.updated_at_ms.max(run.created_at_ms))
        .saturating_sub(run.created_at_ms)
        .max(0)
        / 1_000;
    let lifecycle = match run.state {
        AgentState::Starting => format!(
            "Starting {}s",
            now_ms.saturating_sub(run.created_at_ms).max(0) / 1_000
        ),
        AgentState::Ready => "Ready".into(),
        AgentState::Closing => format!(
            "Closing {}s",
            now_ms.saturating_sub(run.updated_at_ms).max(0) / 1_000
        ),
        AgentState::Closed => format!("Closed {elapsed_seconds}s"),
    };
    format!("● {lifecycle} · {}", run.label())
}

fn session_event_text(event: &SessionEventKind) -> String {
    match event {
        SessionEventKind::Configuration {
            state,
            field,
            reason,
            ..
        } => {
            let mut text = format!("Settings {state}: {}", field.join(", "));
            if let Some(reason) = reason {
                text.push_str(&format!(" — {reason}"));
            }
            text
        }
        SessionEventKind::Compaction { .. } => "Context compaction".into(),
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

/// Distinguish workspace history from the preserved execution outcome.
fn history_prefix(disposition: crate::exchange::HistoryDisposition) -> &'static str {
    match disposition {
        crate::exchange::HistoryDisposition::Current => "",
        crate::exchange::HistoryDisposition::RolledBack => "Rolled back · ",
        crate::exchange::HistoryDisposition::Superseded => "Superseded · ",
    }
}

fn token_display(tokens: u64) -> String {
    match tokens {
        tokens if tokens >= 1000 => format!("{:.1}k", tokens as f64 / 1000.0),
        tokens => tokens.to_string(),
    }
}

fn finalization_label(outcome: Option<ExchangeState>) -> &'static str {
    if outcome == Some(ExchangeState::Cancelled) {
        "Stopping"
    } else {
        "Saving exchange"
    }
}

fn exchange_activity_summary(interaction: &Exchange, now_ms: i64) -> String {
    let complete = interaction.completed_at_ms.is_some();
    let status = interaction.activity();
    let paused = status == ExchangeActivity::Paused;
    let duration = interaction.elapsed(now_ms) / 1000;
    let activity = if interaction.state == ExchangeState::Cancelled {
        "Cancelled"
    } else if interaction.state == ExchangeState::Finalizing {
        finalization_label(interaction.finalization_outcome)
    } else if interaction.state == ExchangeState::Interrupted {
        "Interrupted"
    } else if interaction.state == ExchangeState::Failed {
        "Failed"
    } else if interaction.state == ExchangeState::Queued {
        "Starting"
    } else if status == ExchangeActivity::AwaitingApproval {
        "Waiting for your approval"
    } else if status == ExchangeActivity::AwaitingInput {
        "Waiting for your answer"
    } else if status == ExchangeActivity::WaitingForAgent {
        "Waiting for agents"
    } else if matches!(status, ExchangeActivity::Compacting { .. }) {
        "Compacting"
    } else {
        let activity = WorkflowActivity::from_exchange(interaction);
        match interaction.kind {
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision if paused => {
                if activity == WorkflowActivity::Revising {
                    "Revising plan paused"
                } else {
                    "Planning paused"
                }
            }
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision if complete => {
                activity.completed_label()
            }
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision => activity.active_label(),
            ExchangeKind::PlanExecution
                if interaction
                    .execution_phase
                    .as_ref()
                    .is_some_and(|phase| phase.outcome.is_some()) =>
            {
                activity.completed_label()
            }
            ExchangeKind::PlanExecution if complete => match activity {
                WorkflowActivity::Verifying => "Verification stopped",
                WorkflowActivity::Resolving => "Resolution stopped",
                _ => "Implementation stopped",
            },
            ExchangeKind::PlanExecution if paused => match activity {
                WorkflowActivity::Verifying => "Verifying paused",
                WorkflowActivity::Resolving => "Resolving paused",
                _ => "Implementing paused",
            },
            ExchangeKind::PlanExecution => activity.active_label(),
            ExchangeKind::Chat if complete => activity.completed_label(),
            ExchangeKind::Chat if paused => "Paused",
            ExchangeKind::Chat => activity.active_label(),
        }
    };
    let summary = format!(
        "● {}{activity} {duration}s",
        history_prefix(interaction.disposition)
    );
    exchange_metrics_summary(interaction, now_ms, summary)
}

fn exchange_metrics_summary(interaction: &Exchange, now_ms: i64, summary: String) -> String {
    let count: usize = interaction
        .turn
        .iter()
        .map(|turn| turn.tools().count())
        .sum();
    let mut sections = vec![summary];
    let compaction: Vec<_> = interaction
        .node_list
        .iter()
        .filter_map(|node| {
            if let ExchangeNode::Compaction { compaction, .. } = node {
                Some(compaction)
            } else {
                None
            }
        })
        .collect();
    if !compaction.is_empty() {
        let mut label = format!("Compactions {}", compaction.len());
        for (state, name) in [
            (crate::backend::CompactionState::Running, "active"),
            (crate::backend::CompactionState::Failed, "failed"),
            (crate::backend::CompactionState::Unconfirmed, "unconfirmed"),
        ] {
            let count = compaction
                .iter()
                .filter(|compaction| compaction.state == state)
                .count();
            if count > 0 {
                label.push_str(&format!(" · {count} {name}"));
            }
        }
        sections.push(label);
    }
    if count > 0 {
        let passed = interaction
            .turn
            .iter()
            .flat_map(|turn| turn.tools())
            .filter(|tool| tool.state() == crate::turn::ToolState::Completed && !tool.failed)
            .count();
        let duration = interaction
            .metrics
            .tool_ms(interaction.elapsed(now_ms))
            .map(|duration| {
                let label = super::duration::duration_label(duration);
                let value =
                    label.trim_end_matches(|character: char| character.is_ascii_alphabetic());
                format!(" · {value} {}", &label[value.len()..])
            })
            .unwrap_or_default();
        sections.push(format!("{passed}/{count} tools{duration}"));
    }
    let usage = interaction.usage();
    let mut tokens = Vec::new();
    let input = usage.input.map(|input| {
        let mut label = token_display(input);
        if let Some(percent) = usage.cached_percent() {
            label.push_str(&format!(" ({percent}%)"));
        }
        label
    });
    match (input, usage.output) {
        (Some(input), Some(output)) => tokens.push(format!("{input} -> {}", token_display(output))),
        (Some(input), None) => tokens.push(format!("in {input}")),
        (None, Some(output)) => tokens.push(format!("out {}", token_display(output))),
        (None, None) => {}
    }
    if let Some((output, duration)) = interaction
        .metrics
        .reported_output_tokens
        .zip(interaction.metrics.reported_response_ms)
        .filter(|(output, duration)| {
            *output > 0 && *duration > 0 && interaction.metrics.timing_complete
        })
    {
        tokens.push(format!(
            "{:.0} tps",
            output as f64 * 1000.0 / duration as f64
        ));
    }
    if !tokens.is_empty() {
        sections.push(tokens.join(" · "));
    }
    let mut summary = sections.join(" │ ");
    if let Some(error) = &interaction.finalization_error {
        summary.push_str(&format!(" — {}", error.replace(['\n', '\r'], " ")));
    }
    summary
}

#[cfg(test)]
mod tests {
    #[test]
    fn compaction_context_usage_survives_event_order_and_reload() -> anyhow::Result<()> {
        use super::*;
        use crate::backend::{Compaction, CompactionResult, CompactionState};
        for usage_before_completion in [false, true] {
            let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
                "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Work", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            let running = Compaction {
                id: "operation".into(),
                thread_id: "provider".into(),
                state: CompactionState::Running,
                manual: false,
                started_at_ms: None,
                completed_at_ms: None,
                result: CompactionResult {
                    context_tokens_before: Some(120_000),
                    ..Default::default()
                },
            };
            exchange.observe_compaction(running.clone(), 1_000);
            assert!(!exchange.observe_compaction_usage("different-provider", 900_000));
            if usage_before_completion {
                assert!(!exchange.observe_compaction_usage("provider", 125_000));
            }
            let mut completed = running.clone();
            completed.state = CompactionState::Completed;
            completed.result = CompactionResult::default();
            exchange.observe_compaction(completed, 16_000);
            let waiting = project_at(
                &TimelineEntry::Exchange {
                    id: "exchange".into(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                16_000,
            )?;
            assert!(
                !waiting
                    .entry
                    .block
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .any(|row| row.contains("→"))
            );
            assert!(exchange.observe_compaction_usage("provider", 32_000));
            assert!(!exchange.observe_compaction_usage("provider", 64_000));
            exchange.observe_compaction(running, 20_000);
            let restored: Exchange = serde_json::from_value(serde_json::to_value(exchange)?)?;
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: "exchange".into(),
                    created_at_ms: 0,
                    exchange: restored,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                20_000,
            )?;
            let header = projected
                .entry
                .block
                .iter()
                .find(|block| {
                    block
                        .text
                        .row(0)
                        .is_some_and(|row| row.contains("Context compacted"))
                })
                .unwrap();
            assert!(
                header
                    .text
                    .row(0)
                    .unwrap()
                    .contains("Context compacted · 120.0k → 32.0k tokens · 15s")
            );
            assert!(header.metadata.fold.is_empty());
            assert!(header.metadata.node.is_none());
            assert_eq!(
                header
                    .metadata
                    .layout
                    .as_ref()
                    .unwrap()
                    .marker
                    .as_ref()
                    .unwrap()
                    .text,
                "◇"
            );
        }
        Ok(())
    }

    #[test]
    fn uncertain_compaction_retains_explanation_without_failure_wording() -> anyhow::Result<()> {
        use super::*;
        use crate::backend::{Compaction, CompactionResult, CompactionState};
        for state in [CompactionState::Unconfirmed, CompactionState::Failed] {
            let entry = TimelineEntry::SessionEvent {
                id: "compact".into(),
                created_at_ms: 1,
                event: crate::timeline::SessionEventRecord {
                    id: "compact".into(),
                    session_id: "session".into(),
                    owner: crate::timeline::TimelineOwner::session(&"session"),
                    created_at_ms: 1,
                    detail: SessionEventKind::Compaction {
                        compaction: Compaction {
                            id: "operation".into(),
                            thread_id: "provider".into(),
                            state,
                            manual: true,
                            started_at_ms: Some(1),
                            completed_at_ms: Some(2),
                            result: CompactionResult {
                                error: Some("Runtime closed".into()),
                                ..Default::default()
                            },
                        },
                    },
                },
            };
            let projected = project_at(&entry, &WidthProfile::default(), 2)?;
            let text = projected
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n");
            assert_eq!(text.contains("Failure:"), state == CompactionState::Failed);
            assert_eq!(
                text.contains("Explanation:"),
                state == CompactionState::Unconfirmed
            );
        }
        Ok(())
    }

    #[test]
    fn compaction_fold_requires_reported_content() -> anyhow::Result<()> {
        use super::*;
        use crate::backend::{Compaction, CompactionResult, CompactionState};
        for summary in [None, Some(""), Some(" \n\t")] {
            for messages_removed in [None, Some(0)] {
                let entry = TimelineEntry::SessionEvent {
                    id: "compaction".into(),
                    created_at_ms: 1_000,
                    event: crate::timeline::SessionEventRecord {
                        id: "compaction".into(),
                        session_id: "session".into(),
                        owner: crate::timeline::TimelineOwner::session(&"session"),
                        created_at_ms: 1_000,
                        detail: SessionEventKind::Compaction {
                            compaction: Compaction {
                                id: "operation".into(),
                                thread_id: "provider".into(),
                                state: CompactionState::Completed,
                                manual: true,
                                started_at_ms: Some(1_000),
                                completed_at_ms: Some(16_000),
                                result: CompactionResult {
                                    summary_content: summary.map(str::to_owned),
                                    messages_removed,
                                    ..Default::default()
                                },
                            },
                        },
                    },
                };
                let projected = project_at(&entry, &WidthProfile::default(), 16_000)?;
                let header = &projected.entry.block[0];
                assert!(
                    header
                        .text
                        .row(0)
                        .unwrap()
                        .contains("Context compacted · 15s")
                );
                assert!(!header.text.row(0).unwrap().contains("manual"));
                assert!(!header.text.row(0).unwrap().contains("automatic"));
                assert_eq!(header.metadata.fold.is_empty(), messages_removed.is_none());
                if messages_removed.is_none() {
                    assert_eq!(projected.entry.block.len(), 1);
                    assert!(header.metadata.node.is_none());
                    assert_eq!(
                        header
                            .metadata
                            .layout
                            .as_ref()
                            .unwrap()
                            .marker
                            .as_ref()
                            .unwrap()
                            .text,
                        "◇"
                    );
                }
                assert!(
                    !projected
                        .entry
                        .block
                        .iter()
                        .any(|block| block.id.0 == "compaction:summary")
                );
            }
        }
        Ok(())
    }

    #[test]
    fn compaction_keeps_its_position_summary_and_fold_after_reload() -> anyhow::Result<()> {
        use super::*;
        use crate::backend::{Compaction, CompactionResult, CompactionState};
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        let compaction = Compaction {
            id: "operation".into(),
            thread_id: "provider".into(),
            state: CompactionState::Running,
            manual: false,
            started_at_ms: None,
            completed_at_ms: None,
            result: CompactionResult::default(),
        };
        exchange.observe_compaction(compaction.clone(), 1000);
        let id = exchange.node_list[0].id().to_owned();
        let running = TimelineEntry::Exchange {
            id: "exchange".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let projected = project_at(&running, &WidthProfile::default(), 1500)?;
        assert!(!projected.entry.block.iter().any(|block| block.id.0 == id));
        exchange.append_input(
            crate::exchange::InputIntent::Steering,
            "Continue".into(),
            1500,
        )?;
        let mut completed = compaction.clone();
        completed.state = CompactionState::Completed;
        completed.result.summary_content =
            Some("Preserve **the API contract**.\n\nKeep the regression test.".into());
        completed.result.pre_compaction_tokens = Some(1000);
        completed.result.post_compaction_tokens = Some(400);
        completed.result.messages_removed = Some(0);
        exchange.observe_compaction(completed, 3000);
        exchange.observe_compaction(compaction, 4000);
        let restored: Exchange = serde_json::from_value(serde_json::to_value(exchange)?)?;
        assert_eq!(restored.node_list.len(), 2);
        assert_eq!(restored.node_list[0].id(), id);
        let entry = TimelineEntry::Exchange {
            id: "exchange".into(),
            created_at_ms: 0,
            exchange: restored,
            agent_by_id: HashMap::new(),
        };
        let projected = project_at(&entry, &WidthProfile::default(), 5000)?;
        let activity = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "exchange:summary")
            .unwrap();
        assert!(activity.text.row(0).unwrap().contains("Compactions 1"));
        let header = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == id)
            .unwrap();
        assert!(header.text.row(0).unwrap().contains("Context compacted"));
        assert!(header.text.row(0).unwrap().contains("2s"));
        assert!(header.metadata.status.is_none());
        assert!(
            header
                .metadata
                .node
                .as_ref()
                .is_some_and(|node| node.default_display == NodeDisplay::Heading)
        );
        let summary = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == format!("{id}:summary"))
            .unwrap();
        assert!(
            summary
                .text
                .wire_rows()
                .join("\n")
                .contains("Keep the regression test.")
        );
        let metrics = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == format!("{id}:metrics"))
            .unwrap();
        assert!(
            metrics
                .text
                .wire_rows()
                .join("\n")
                .contains("Messages removed: 0")
        );
        Ok(())
    }

    #[test]
    fn verification_groups_keep_checks_and_declaration_diffs_separate() -> anyhow::Result<()> {
        use super::*;
        use serde_json::json;
        let exchange: Exchange = serde_json::from_value(json!({
            "id":"verify", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"", "kind":"plan_execution", "state":"complete", "created_at_ms":0,
            "completed_at_ms":1000, "attributed_matches_checkpoint":true, "node_list":[],
            "execution_phase":{"phase":"verify","outcome":"failed", "findings":[],
                "verification":{"outcome":"failed", "checks":[
                    {"category":"automated","label":"cargo test","outcome":"passed","summary":"8 tests passed","evidence":["test-tool"]},
                    {"category":"automated","label":"cargo check","outcome":"failed","summary":"Expected FontSize","evidence":[]},
                    {"category":"manual","label":"Initial display","outcome":"blocked","summary":"Compilation failed","evidence":[]}
                ]},
                "verification_tools":{"test-tool":"earlier-exchange:test-tool"},
                "declaration_findings":["src/lib.rs: function update: changed"],
                "declaration_changes":[{"path":"src/lib.rs","name":"function update",
                    "finding":"src/lib.rs: function update: changed",
                    "diff":"diff --git a/src/lib.rs b/src/lib.rs\n--- a/src/lib.rs\n+++ b/src/lib.rs\n@@ -1 +1 @@\n-fn update(input: State);\n+fn update(mut input: State);\n"}]
            }
        }))?;
        let exchange = serde_json::from_slice(&serde_json::to_vec(&exchange)?)?;
        let projection = project_at(
            &TimelineEntry::Exchange {
                id: "verify".into(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1000,
        )?;
        let blocks = &projection.entry.block;
        let rows = blocks
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>();
        let text = rows.join("\n");
        assert!(text.contains(
            "Verification failed · 1 failed check, 1 blocked check, 1 declaration mismatch"
        ));
        assert!(text.contains("Automated · 1 passed, 1 failed"));
        assert!(text.contains("Manual · 1 blocked"));
        assert!(text.contains("Declarations · 1 mismatch"));
        assert!(text.contains("fn update(mut input: State);"));
        assert_eq!(text.matches("Expected FontSize").count(), 1);
        assert!(!text.contains("src/lib.rs: function update: changed"));
        assert!(!rows.iter().any(|row| row.is_empty()));
        for id in [
            "verify:verification-result:Automated",
            "verify:verification-result:Manual",
            "verify:verification-result:declarations",
            "verify:verification-result:Automated:0",
        ] {
            let block = blocks.iter().find(|block| block.id.0 == id).unwrap();
            assert!(block.metadata.fold[0].closed);
        }
        assert!(projection.action.values().any(|action| matches!(action,
            TranscriptAction::Tool { call_id } if call_id == "earlier-exchange:test-tool")));
        assert!(
            !projection
                .action
                .values()
                .any(|action| matches!(action, TranscriptAction::File { .. }))
        );
        Ok(())
    }

    #[test]
    fn verification_result_retains_findings_outside_the_activity_fold() -> anyhow::Result<()> {
        use super::*;
        use serde_json::json;
        for outcome in ["failed", "blocked", "passed"] {
            let findings = if outcome == "passed" {
                vec![]
            } else {
                vec![
                    "restart_resets_round: expected score 0, got 3",
                    "pause_freezes_timer: remaining time changed",
                ]
            };
            let exchange: Exchange = serde_json::from_value(json!({
                "id":"verify-result", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"", "kind":"plan_execution", "state":"complete", "created_at_ms":0,
                "completed_at_ms":18000, "duration_ms":18000, "attributed_matches_checkpoint":true,
                "execution_phase":{"phase":"verify","outcome":outcome,
                    "summary":"Ran eight gameplay tests.", "findings":findings},
                "node_list":[{"kind":"artifact_change","change":{
                    "id":"inspected-change", "path":"game.rs", "created_at_ms":1,
                    "diff_text":"diff --git a/game.rs b/game.rs\n--- a/game.rs\n+++ b/game.rs\n@@ -1 +1 @@\n-old\n+new\n"
                }}]
            }))?;
            let exchange = serde_json::from_slice(&serde_json::to_vec(&exchange)?)?;
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: "verify-result".into(),
                    created_at_ms: 0,
                    exchange,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                18000,
            )?;
            let blocks = &projected.entry.block;
            let result_index = blocks
                .iter()
                .position(|block| block.id.0 == "verify-result:verification-result")
                .unwrap();
            let result = &blocks[result_index];
            assert!(
                blocks[0]
                    .text
                    .wire_rows()
                    .join("\n")
                    .contains("Verified 18s")
            );
            assert!(
                blocks[0].metadata.fold.is_empty(),
                "result-only exchanges have no activity to fold"
            );
            assert!(blocks[0].metadata.fold.iter().all(|fold| {
                !blocks[result_index..]
                    .iter()
                    .any(|block| block.id == fold.end.block)
            }));
            let text = result.text.wire_rows().join("\n");
            if outcome == "passed" {
                assert!(text.contains("◇ Verification passed · Ran eight gameplay tests."));
                assert!(result.metadata.fold.is_empty());
            } else {
                assert!(!text.contains("expected score"));
                assert!(text.contains(if outcome == "failed" {
                    "Verification failed"
                } else {
                    "Verification blocked"
                }));
                assert!(result.metadata.fold[0].closed);
                assert!(blocks.iter().any(|block| {
                    block
                        .text
                        .wire_rows()
                        .join("\n")
                        .contains("pause_freezes_timer: remaining time changed")
                }));
            }
        }
        Ok(())
    }

    #[test]
    fn execution_phase_headings_and_lifecycle_events_survive_serialization() {
        use super::*;
        use serde_json::json;
        let source = json!({
            "id":"phase", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"", "kind":"plan_execution", "state":"running", "created_at_ms":0,
            "lifecycle":"Plan implementation started",
            "execution_started_at_ms":0, "attributed_matches_checkpoint":false,
            "execution_phase":{"phase":"implement","outcome":null},
            "node_list":[
                {"kind":"artifact_change","change":{"id":"files","path":"lib.rs",
                    "diff_text":"diff --git a/lib.rs b/lib.rs\n--- a/lib.rs\n+++ b/lib.rs\n@@ -1 +1 @@\n-old\n+new\n",
                    "created_at_ms":1}},
                {"kind":"plan_event","event":{"id":"done","node_count":1,"content":{
                    "kind":"execution","event":{"kind":"completed","title":"Collection game"}
                }}}
            ]
        });
        for (phase, outcome, state, expected) in [
            ("implement", None, "running", "Implementing"),
            ("verify", None, "running", "Verifying"),
            ("resolve", None, "running", "Resolving"),
            ("implement", Some("passed"), "complete", "Implemented"),
            ("verify", Some("failed"), "complete", "Verified"),
            ("verify", Some("blocked"), "complete", "Verified"),
            ("resolve", Some("passed"), "complete", "Resolved"),
            ("verify", Some("passed"), "complete", "Verified"),
            ("implement", None, "complete", "Implementation stopped"),
            ("verify", None, "complete", "Verification stopped"),
            ("resolve", None, "complete", "Resolution stopped"),
            ("implement", None, "cancelled", "Cancelled"),
            ("implement", None, "interrupted", "Interrupted"),
            ("implement", None, "failed", "Failed"),
        ] {
            let mut value = source.clone();
            value["execution_phase"] = json!({"phase":phase,"outcome":outcome});
            value["state"] = json!(state);
            value["completed_at_ms"] = if state == "running" {
                json!(null)
            } else {
                json!(1000)
            };
            let exchange: Exchange = serde_json::from_value(value).unwrap();
            let exchange = serde_json::from_slice(&serde_json::to_vec(&exchange).unwrap()).unwrap();
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: "phase".into(),
                    created_at_ms: 0,
                    exchange,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                1000,
            )
            .unwrap();
            let blocks = &projected.entry.block;
            assert_eq!(blocks[0].id.0, "phase:lifecycle");
            assert_eq!(
                blocks[0].text.wire_rows().join("\n"),
                "◇ Plan implementation started"
            );
            let summary = blocks
                .iter()
                .find(|block| block.id.0 == "phase:summary")
                .unwrap();
            assert!(summary.text.wire_rows().join("\n").contains(expected));
            assert_eq!(blocks.last().unwrap().id.0, "done");
            assert!(
                summary.metadata.fold.is_empty(),
                "artifacts and outcomes stay outside activity"
            );
            assert!(!blocks.iter().any(|block| block.id.0 == "phase:prompt"));
        }
    }

    #[test]
    fn timer_updates_agent_lifecycle_without_replacing_its_task_body() -> anyhow::Result<()> {
        let mut run = crate::agent::Agent::pending("session", "Worker", "Implement", 0);
        run.state = crate::agent::AgentState::Ready;
        let entry = crate::timeline::TimelineEntry::AgentLifecycle {
            id: "worker-heading".into(),
            created_at_ms: 0,
            run,
            exchange: vec![],
            agent: vec![],
        };
        let width = WidthProfile::default();
        let projected = super::project_at(&entry, &width, 0)?;
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("timer-agent".into()),
            projected.entry.block,
        )?;
        let refreshed = super::timing_blocks(
            &entry,
            &width,
            &document,
            &Default::default(),
            &projected.tool,
        )?;
        assert_eq!(refreshed.len(), 1);
        assert_eq!(refreshed[0].id.0, "worker-heading");
        assert!(
            !refreshed[0]
                .text
                .wire_rows()
                .join("\n")
                .contains("ready for 0s")
        );
        Ok(())
    }
    use super::{project_at, project_at_with_separator, session_event_text};
    use forge_buffer::node::{NodeDisplay, NodeLifecycle};
    use forge_buffer::width::WidthProfile;
    use serde_json::{Value, json};
    use std::collections::HashMap;

    use crate::{
        exchange::Exchange,
        session::state_machine::{SessionPhase, WorkflowActivity},
        timeline::{SessionEventKind, TimelineEntry},
    };

    #[test]
    fn plan_annotations_have_neutral_hollow_bullets_and_hanging_continuations() {
        let exchange = serde_json::from_value(json!({
            "id":"review", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[{
                "kind":"plan_event", "event": {
                    "id":"feedback", "node_count":0, "content": {
                        "kind":"lifecycle", "title":"Migration", "lifecycle": {
                            "id":"feedback", "session_id":"session", "plan_id":"plan",
                            "kind":"changes_requested", "model_revision":3, "user_revision":1,
                            "created_at_ms":0, "annotation":[
                                {"subject":[], "label":"TelemetrySink", "body":"rename to TelemetryDongle and preserve all of the existing methods"},
                                {"subject":[], "label":"Configuration", "body":"Keep the existing defaults"}
                            ]
                        }
                    }
                }
            }]
        })).unwrap();
        let rendered = project_at(
            &TimelineEntry::Exchange {
                id: "review".into(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile {
                columns: 40,
                ..WidthProfile::default()
            },
            1000,
        )
        .unwrap();
        for index in 0..2 {
            let block = rendered
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == format!("feedback:annotation:{index}"))
                .unwrap();
            assert!(
                block.metadata.fold.is_empty(),
                "comment bullet introduced a nested fold"
            );
            let layout = block.metadata.layout.as_ref().unwrap();
            assert_eq!(layout.indent, 6);
            assert_eq!(layout.marker.as_ref().unwrap().text, "◦");
            assert!(block.metadata.gutter.is_empty());
            assert!(block.text.row_count() > 1, "fixture must exercise wrapping");
        }
    }

    #[test]
    fn activity_summary_uses_permission_and_separate_planning_kind() {
        for (mode, capture) in [
            ("read", "ForgeHarnessRead"),
            ("write", "ForgeHarnessWrite"),
            ("yolo", "ForgeHarnessYolo"),
        ] {
            for completed in [false, true] {
                let exchange: Exchange = serde_json::from_value(json!({
                    "id":"colored", "session_id":"session", "agent_id":"primary", "ordinal":1,
                    "prompt":"Inspect", "kind":"chat", "mode":mode, "state": if completed { "complete" } else { "running" },
                    "created_at_ms":0, "completed_at_ms": if completed { Some(1000) } else { None },
                    "attributed_matches_checkpoint":false, "node_list":[]
                })).unwrap();
                let restored: Exchange =
                    serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
                assert_eq!(restored.mode, exchange.mode);
                let rendered = project_at(
                    &TimelineEntry::Exchange {
                        id: "colored".into(),
                        created_at_ms: 0,
                        exchange: restored,
                        agent_by_id: HashMap::new(),
                    },
                    &WidthProfile::default(),
                    2000,
                )
                .unwrap();
                let summary = rendered
                    .entry
                    .block
                    .iter()
                    .find(|block| block.id.0 == "colored:summary")
                    .unwrap();
                assert!(summary.metadata.fold.is_empty());
                assert_eq!(
                    summary
                        .metadata
                        .layout
                        .as_ref()
                        .unwrap()
                        .marker
                        .as_ref()
                        .unwrap()
                        .text,
                    "●"
                );
                assert!(summary.text.row(0).unwrap().starts_with("● "));
                assert!(
                    summary
                        .metadata
                        .decoration
                        .iter()
                        .any(|decoration| decoration.capture == capture)
                );
            }
        }
        for kind in ["plan_draft", "plan_revision"] {
            let exchange = serde_json::from_value(json!({
                "id":"legacy", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Plan", "kind":kind, "state":"complete", "created_at_ms":0,
                "completed_at_ms":1000, "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            let rendered = project_at(
                &TimelineEntry::Exchange {
                    id: "legacy".into(),
                    created_at_ms: 0,
                    exchange,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                2000,
            )
            .unwrap();
            let summary = rendered
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == "legacy:summary")
                .unwrap();
            assert!(
                summary
                    .metadata
                    .decoration
                    .iter()
                    .any(|decoration| decoration.capture == "ForgeHarnessPlan")
            );
        }
    }

    #[test]
    fn outer_responses_keep_order_across_answered_questions_and_continued_turns() {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        use crate::exchange::{
            ExchangeKind, ExchangeNode, ExchangeState, InputIntent, QuestionInput,
        };

        for planning_question in [None, Some(false), Some(true)] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"ordered", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Plan a game", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            exchange.resume(0).unwrap();
            let mut event = BackendEvent {
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "initial".into(),
                }),
                turn_boundary: Some(TurnBoundary::Started),
                kind: "turn_started".into(),
                text: None,
                data: serde_json::Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            exchange.observe_turn(&event, 0).unwrap();
            event.turn_boundary = None;
            event.kind = "assistant_message".into();
            event.text = Some("Inspecting the repository".into());
            event.data = json!({"phase":"commentary"});
            exchange.observe_turn(&event, 1).unwrap();
            event.text =
                Some("The repository contains only .git. Three choices are pending.".into());
            event.data = json!({"phase":"final_answer"});
            exchange.observe_turn(&event, 2).unwrap();
            event.text = None;
            event.turn_boundary = Some(TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            exchange.observe_turn(&event, 3).unwrap();

            if let Some(planning) = planning_question {
                let question: crate::plan::PlanQuestionSet = serde_json::from_value(json!({
                    "id":"format", "questions":[{"id":"game", "header":"Game format",
                        "question":"Which game format?", "options":[
                            {"label":"2D arena survival", "description":"Top-down combat"}
                        ], "allow_freeform":true}]
                }))
                .unwrap();
                exchange.node_list.push(if planning {
                    exchange.kind = ExchangeKind::PlanDraft;
                    serde_json::from_value(json!({"kind":"plan_event", "event":{
                        "id":"presented", "node_count":exchange.node_list.len(),
                        "content":{"kind":"lifecycle", "title":"Game", "lifecycle":{
                            "id":"presented", "session_id":"session", "plan_id":"plan",
                            "kind":"question_asked", "model_revision":0, "user_revision":0,
                            "question":question, "created_at_ms":3
                        }}
                    }}))
                    .unwrap()
                } else {
                    ExchangeNode::QuestionPresented {
                        id: "presented".into(),
                        question,
                        answer: None,
                    }
                });
                exchange
                    .append_question_input(
                        InputIntent::Answer,
                        "Game format: 2D arena survival".into(),
                        QuestionInput {
                            set_id: "format".into(),
                            question_id: None,
                            answer: vec![crate::plan::PlanQuestionAnswer {
                                source: crate::session::ResolutionSource::User,
                                question_id: "game".into(),
                                response: crate::plan::PlanQuestionResponse::Selected {
                                    option: "2D arena survival".into(),
                                    feedback: None,
                                },
                            }],
                        },
                        4,
                    )
                    .unwrap();
            }
            event.address.as_mut().unwrap().turn_id = "continuation".into();
            event.turn_boundary = Some(TurnBoundary::Started);
            exchange.observe_turn(&event, 5).unwrap();
            event.turn_boundary = None;
            event.text = Some("The design will use a 2D arena survival loop.".into());
            event.data = json!({"phase":"commentary"});
            exchange.observe_turn(&event, 6).unwrap();

            let render = |exchange: &Exchange| {
                project_at(
                    &TimelineEntry::Exchange {
                        id: exchange.id.clone(),
                        created_at_ms: 0,
                        exchange: exchange.clone(),
                        agent_by_id: HashMap::new(),
                    },
                    &WidthProfile::default(),
                    10,
                )
                .unwrap()
            };
            let initial = render(&exchange);
            let response_position = initial
                .entry
                .block
                .iter()
                .position(|block| {
                    block
                        .text
                        .wire_rows()
                        .join("\n")
                        .contains("Three choices are pending")
                })
                .unwrap();
            let prefix = initial.entry.block[..=response_position]
                .iter()
                .map(|block| block.id.clone())
                .collect::<Vec<_>>();

            event.text = None;
            event.kind = "tool".into();
            for tool_count in 1..=3 {
                event.activity = Some(ToolActivity {
                    id: format!("lookup-{tool_count}"),
                    kind: ToolActivityKind::Command,
                    title: format!("lookup_{tool_count}"),
                    output: Some("found".into()),
                    output_delta: false,
                    status: Some("completed".into()),
                    change: Default::default(),
                });
                exchange.observe_turn(&event, 6 + tool_count).unwrap();
                let projected = render(&exchange);
                let blocks = &projected.entry.block;
                assert_eq!(
                    blocks[..=response_position]
                        .iter()
                        .map(|block| block.id.clone())
                        .collect::<Vec<_>>(),
                    prefix,
                    "later tools moved an earlier response"
                );
                let text = blocks
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .collect::<Vec<_>>()
                    .join("\n");
                let response = text.find("Three choices are pending").unwrap();
                let continuation = text.find("The design will use").unwrap();
                assert!(response < continuation && continuation < text.find("lookup_1").unwrap());
                let commentary = blocks
                    .iter()
                    .find(|block| {
                        block
                            .text
                            .wire_rows()
                            .join("\n")
                            .contains("The design will use")
                    })
                    .unwrap();
                assert_eq!(
                    commentary.metadata.layout.as_ref().unwrap().indent,
                    4,
                    "continued thoughts must stay one level inside the exchange"
                );
                let tools = blocks
                    .iter()
                    .find(|block| block.id.0.ends_with(":tools"))
                    .unwrap();
                assert_eq!(
                    tools.metadata.layout.as_ref().unwrap().indent,
                    6,
                    "continued tool groups must nest inside their thought"
                );
                if planning_question.is_some() {
                    let question = text.find("Questions · 1/1 answered").unwrap();
                    assert!(response < question && question < continuation);
                    assert_eq!(text.matches("You answered: 2D arena survival").count(), 1);
                }
                let summary = blocks
                    .iter()
                    .find(|block| block.id.0 == "ordered:summary")
                    .unwrap();
                assert_eq!(
                    summary.metadata.fold[0].end.block,
                    blocks[response_position - 1].id,
                    "activity fold hid an outer response or later turn"
                );
                if planning_question == Some(true) && tool_count == 3 {
                    println!(
                        "ORDERED_RESPONSE_FIXTURE:{}",
                        serde_json::to_string(blocks).unwrap()
                    );
                }
            }
            event.activity = None;
            event.kind = "assistant_message".into();
            event.text = Some("The design is ready for review.".into());
            event.data = json!({"phase":"final_answer"});
            exchange.observe_turn(&event, 10).unwrap();
            event.text = None;
            event.turn_boundary = Some(TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            exchange.observe_turn(&event, 11).unwrap();
            exchange.finish(ExchangeState::Complete, 11).unwrap();
            let completed = render(&exchange);
            let restored: Exchange =
                serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
            assert_eq!(completed.entry.block, render(&restored).entry.block);
            assert_eq!(
                completed.entry.block[..=response_position]
                    .iter()
                    .map(|block| block.id.clone())
                    .collect::<Vec<_>>(),
                prefix
            );
            let text = completed
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n");
            assert!(text.find("lookup_3").unwrap() < text.find("The design is ready").unwrap());
            assert_eq!(text.matches("Three choices are pending").count(), 1);
        }
    }

    #[test]
    fn inherited_history_freezes_time_and_disables_question_controls() {
        use crate::exchange::ExchangeNode;
        let question: crate::plan::PlanQuestionSet = serde_json::from_value(json!({
            "id":"source-question", "questions":[{
                "id":"scope", "header":"Scope", "question":"Which scope?",
                "options":[{"label":"All","description":"All files"}], "allow_freeform":true
            }]
        }))
        .unwrap();
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"source-exchange", "session_id":"source", "agent_id":"primary", "ordinal":1,
            "prompt":"Keep this visible", "state":"running", "created_at_ms":0,
            "execution_started_at_ms":1,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.node_list.push(ExchangeNode::QuestionPresented {
            id: "source-presented".into(),
            question: question.clone(),
            answer: None,
        });
        exchange.elicitation = Some(crate::plan::PlanElicitation::new(question));
        exchange.awaiting_input = true;
        let source = TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 0,
            exchange,
            agent_by_id: HashMap::new(),
        };
        assert!(
            project_at(&source, &WidthProfile::default(), 10)
                .unwrap()
                .action
                .values()
                .any(|action| matches!(action, super::TranscriptAction::Question { .. }))
        );
        let inherited = TimelineEntry::Inherited {
            captured_at_ms: 10,
            entry: Box::new(source),
        };
        let inherited: TimelineEntry =
            serde_json::from_value(serde_json::to_value(inherited).unwrap()).unwrap();
        let first = project_at(&inherited, &WidthProfile::default(), 20).unwrap();
        let later = project_at(&inherited, &WidthProfile::default(), 90_000).unwrap();
        assert_eq!(
            serde_json::to_value(&first.entry.block).unwrap(),
            serde_json::to_value(&later.entry.block).unwrap()
        );
        assert!(
            first
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .any(|row| row.contains("Which scope?"))
        );
        assert!(!first.action.values().any(|action| matches!(
            action,
            super::TranscriptAction::Question { .. }
                | super::TranscriptAction::Plan { .. }
                | super::TranscriptAction::Agent { .. }
                | super::TranscriptAction::Declaration { .. }
        )));
        assert!(
            first
                .entry
                .block
                .iter()
                .filter_map(|block| block.metadata.node.as_ref())
                .all(|node| node.lifecycle == NodeLifecycle::Settled)
        );
    }

    #[test]
    fn question_branches_keep_targets_responses_and_folds_across_reload() {
        use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
        use crate::exchange::{
            ExchangeKind, ExchangeNode, ExchangeState, InputIntent, QuestionInput,
        };
        use forge_buffer::block::BufferBlock;
        for planning in [false, true] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"nested", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Plan a replacement", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            let set: crate::plan::PlanQuestionSet = serde_json::from_value(json!({
                "id":"set", "questions":[
                    {"id":"scope", "header":"Scope", "question":"What should replace the repository?",
                        "options":[{"label":"Rust CLI","description":"Preserve the demo"}], "allow_freeform":true},
                    {"id":"azure", "header":"Azure", "question":"Include real Azure services?",
                        "options":[{"label":"Simulation","description":"Keep everything local"}], "allow_freeform":true}
                ]
            })).unwrap();
            exchange.node_list.push(if planning {
                exchange.kind = ExchangeKind::PlanDraft;
                serde_json::from_value(json!({"kind":"plan_event", "event":{
                    "id":"presented", "node_count":0, "content":{"kind":"lifecycle", "title":"Replacement", "lifecycle":{
                        "id":"presented", "session_id":"session", "plan_id":"plan", "kind":"question_asked",
                        "model_revision":0,"user_revision":0,"annotation":[],"question":set,"created_at_ms":0
                    }}
                }})).unwrap()
            } else {
                ExchangeNode::QuestionPresented { id:"presented".into(), question:set.clone(), answer:None }
            });
            for (index, (target, prompt, reply)) in [
                (
                    "scope",
                    "What does Rust include?",
                    "It preserves the simulation.",
                ),
                (
                    "azure",
                    "Would this require credentials?",
                    "Azure connections require credentials.",
                ),
                (
                    "scope",
                    "Can I retain the same workflow?",
                    "Yes, the workflow remains the same.",
                ),
            ]
            .into_iter()
            .enumerate()
            {
                exchange
                    .append_question_input(
                        InputIntent::Clarification,
                        prompt.into(),
                        QuestionInput {
                            set_id: "set".into(),
                            question_id: Some(target.into()),
                            answer: Vec::new(),
                        },
                        index as i64,
                    )
                    .unwrap();
                let mut event = BackendEvent {
                    received_at_ms: None,
                    address: Some(ProviderAddress {
                        owner: None,
                        thread_id: "thread".into(),
                        turn_id: format!("turn-{index}"),
                    }),
                    turn_boundary: Some(TurnBoundary::Started),
                    kind: "turn_started".into(),
                    text: None,
                    data: serde_json::Value::Null,
                    activity: None,
                    summary: None,
                    task_update: None,
                };
                exchange.observe_turn(&event, index as i64).unwrap();
                event.turn_boundary = None;
                event.kind = "assistant_message".into();
                event.text = Some(reply.into());
                event.data = json!({"phase":"final_answer"});
                exchange.observe_turn(&event, index as i64).unwrap();
                event.text = None;
                event.turn_boundary = Some(TurnBoundary::Finished {
                    outcome: crate::turn::TurnOutcome::Completed,
                });
                exchange.observe_turn(&event, index as i64).unwrap();
            }
            let selected = crate::plan::PlanQuestionAnswer {
                source: crate::session::ResolutionSource::User,
                question_id: "scope".into(),
                response: crate::plan::PlanQuestionResponse::Selected {
                    option: "Rust CLI".into(),
                    feedback: None,
                },
            };
            let mut elicitation = crate::plan::PlanElicitation::new(set);
            elicitation.answer.push(selected.clone());
            exchange.elicitation = Some(elicitation);
            exchange.awaiting_input = true;
            exchange.pause(3);
            let render = |exchange: Exchange| {
                project_at(
                    &TimelineEntry::Exchange {
                        id: exchange.id.clone(),
                        created_at_ms: 0,
                        exchange,
                        agent_by_id: HashMap::new(),
                    },
                    &WidthProfile::default(),
                    4,
                )
                .unwrap()
            };
            let restored =
                serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
            let projected = render(restored);
            let blocks = &projected.entry.block;
            let text = blocks
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n");
            assert!(text.contains("Questions · 1/2 answered"));
            let position = |id: &str| blocks.iter().position(|block| block.id.0 == id).unwrap();
            let scope = position("presented:question:scope");
            let azure = position("presented:question:azure");
            for (input, start, end) in [
                (1, scope, azure),
                (2, azure, blocks.len()),
                (3, scope, azure),
            ] {
                let heading = position(&format!("nested:input:{input}:prompt"));
                let block = &blocks[heading];
                assert!(start < heading && heading < end);
                assert!(
                    block
                        .metadata
                        .decoration
                        .iter()
                        .any(|decoration| decoration.capture == "ForgeHarnessPrompt")
                );
                let fold = &block.metadata.fold[0];
                let reply = position(&fold.end.block.0);
                assert_eq!(reply, heading + 1, "clarification folded a sibling input");
                assert!(reply < end);
                assert!(
                    !blocks[reply]
                        .metadata
                        .gutter
                        .iter()
                        .flat_map(|gutter| &gutter.chunk)
                        .any(|chunk| chunk.text.contains('↳'))
                );
                let indent = |block: &BufferBlock| block.metadata.layout.as_ref().unwrap().indent;
                assert_eq!(indent(&blocks[reply]), indent(block) + 2);
            }
            if planning {
                println!(
                    "NESTED_QUESTION_FIXTURE:{}",
                    serde_json::to_string(blocks).unwrap()
                );
            }
            exchange
                .append_question_input(
                    InputIntent::Answer,
                    "Scope: Rust CLI".into(),
                    QuestionInput {
                        set_id: "set".into(),
                        question_id: None,
                        answer: vec![selected],
                    },
                    4,
                )
                .unwrap();
            exchange.finish(ExchangeState::Cancelled, 5).unwrap();
            let cancelled = render(exchange);
            assert!(
                cancelled
                    .entry
                    .block
                    .iter()
                    .find(|block| block.id.0 == "presented")
                    .unwrap()
                    .metadata
                    .fold[0]
                    .closed
            );
            let text = cancelled
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n");
            assert_eq!(
                text.matches("Azure connections require credentials.")
                    .count(),
                1
            );
            assert_eq!(
                text.matches("Yes, the workflow remains the same.").count(),
                1
            );
        }
    }

    #[test]
    fn question_clarification_preserves_turn_delivery_and_history() {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        use crate::exchange::{ExchangeNode, ExchangeState};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"question-flow", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Plan out a new one", "kind":"chat", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "question-turn".into(),
            }),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 0).unwrap();
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("The scope determines the implementation.".into());
        event.data = json!({"phase":"commentary"});
        exchange.observe_turn(&event, 1).unwrap();
        event.kind = "tool".into();
        event.text = None;
        for (id, status) in [
            ("failed-question", "failed"),
            ("accepted-question", "completed"),
        ] {
            event.activity = Some(ToolActivity {
                id: id.into(),
                kind: ToolActivityKind::Command,
                title: "question_ask".into(),
                output: Some(
                    if status == "failed" {
                        "Invalid options"
                    } else {
                        "Question set accepted"
                    }
                    .into(),
                ),
                output_delta: false,
                status: Some(status.into()),
                change: Default::default(),
            });
            exchange.observe_turn(&event, 2).unwrap();
        }
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 3).unwrap();
        let question = crate::plan::PlanQuestionSet {
            id: "scope-set".into(),
            questions: vec![crate::plan::PlanQuestion {
                id: "scope".into(),
                header: "Migration scope".into(),
                question: "What should the replacement demonstrate?".into(),
                options: vec![crate::plan::PlanQuestionOption {
                    recommended: false,
                    label: "Rust CLI".into(),
                    description: "Replace the demo".into(),
                }],
                allow_freeform: true,
            }],
        };
        exchange.node_list.push(ExchangeNode::QuestionPresented {
            id: "presented".into(),
            question: question.clone(),
            answer: None,
        });
        exchange.elicitation = Some(crate::plan::PlanElicitation::new(question));
        exchange.awaiting_input = true;
        exchange
            .append_input(
                crate::exchange::InputIntent::Clarification,
                "What do you mean?".into(),
                4,
            )
            .unwrap();
        exchange.node_list.push(ExchangeNode::SkillRead {
            id: "clarification-skill".into(),
            turn_id: "explanation-turn".into(),
            read: crate::skill::SkillRead {
                resource_id: "migration-guide".into(),
                name: "migration-guide".into(),
                path: "/skills/migration-guide/SKILL.md".into(),
                tool_id: "read-guide".into(),
            },
        });
        exchange.node_list.push(ExchangeNode::Compaction {
            id: "clarification-compaction".into(),
            compaction: crate::backend::Compaction {
                id: "compaction".into(),
                thread_id: "thread".into(),
                state: crate::backend::CompactionState::Completed,
                manual: false,
                started_at_ms: Some(4),
                completed_at_ms: Some(4),
                result: Default::default(),
            },
        });
        event.address.as_mut().unwrap().turn_id = "explanation-turn".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        exchange.observe_turn(&event, 4).unwrap();
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("I mean a Rust CLI or a redesigned TypeScript demo.".into());
        event.data = json!({"phase":"final_answer"});
        exchange.observe_turn(&event, 5).unwrap();
        event.text = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 6).unwrap();
        exchange.pause(6);
        let restored: Exchange =
            serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: restored.id.clone(),
                created_at_ms: 0,
                exchange: restored,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            6,
        )
        .unwrap();
        let visible = super::super::sections::preview(super::super::document::TranscriptEntry {
            id: projected.entry.id.clone(),
            block: projected.entry.block.clone(),
        })
        .unwrap();
        let text = visible
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("▸ You asked: What do you mean?"));
        assert_eq!(text.matches("▸ Questions ·").count(), 1);
        assert!(!text.contains("Question set accepted"));
        assert!(
            !text.contains("Invalid options"),
            "settled tool output stays deferred"
        );
        assert!(
            projected
                .tool
                .values()
                .any(|tool| tool.preview(true).is_ok_and(|preview| preview
                    .row
                    .iter()
                    .any(|row| row.contains("Invalid options"))))
        );
        assert_eq!(
            projected.tool.len(),
            1,
            "successful question must not remain a raw tool"
        );
        assert!(text.find("▸ Questions ·").unwrap() < text.find("You asked:").unwrap());
        assert!(text.find("You asked:").unwrap() < text.find("I mean a Rust CLI").unwrap());
        let answer = projected
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .join("\n")
                    .contains("I mean a Rust CLI")
            })
            .unwrap();
        assert!(
            !answer
                .metadata
                .gutter
                .iter()
                .flat_map(|gutter| &gutter.chunk)
                .any(|chunk| chunk.text.contains('↳'))
        );
        assert!(projected.action.values().any(|action| matches!(action, super::TranscriptAction::Question { question_set_id } if question_set_id == "scope-set")));
        let question_block = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "presented")
            .unwrap();
        assert!(!question_block.metadata.fold[0].closed);
        assert_eq!(
            question_block.metadata.fold[0].end.block, answer.id,
            "question owns the clarification response"
        );
        let clarification_history = super::super::question::QuestionHistory::new(&exchange);
        assert!(clarification_history.nested.contains("clarification-skill"));
        assert!(
            clarification_history
                .nested
                .contains("clarification-compaction")
        );
        assert!(clarification_history.nested.contains(answer.id.0.as_str()));
        let message = exchange
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .find(|message| message.text() == "I mean a Rust CLI or a redesigned TypeScript demo.")
            .unwrap();
        let mut updated = message.clone();
        updated.text =
            "I mean a Rust CLI or a redesigned TypeScript demo.\n[Guide](/migration.md)".into();
        let incremental = super::message_block(answer, &updated, &WidthProfile::default()).unwrap();
        let mut saved = serde_json::to_value(&exchange).unwrap();
        let reloaded_message = saved["turn"]
            .as_array_mut()
            .unwrap()
            .iter_mut()
            .flat_map(|turn| turn["message"].as_array_mut().unwrap())
            .find(|candidate| candidate["id"] == message.id())
            .unwrap();
        *reloaded_message = serde_json::to_value(updated).unwrap();
        let reloaded: Exchange = serde_json::from_value(saved).unwrap();
        let reloaded = project_at(
            &TimelineEntry::Exchange {
                id: reloaded.id.clone(),
                created_at_ms: 0,
                exchange: reloaded,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            6,
        )
        .unwrap();
        let rebuilt = reloaded
            .entry
            .block
            .iter()
            .find(|block| block.id == answer.id)
            .unwrap();
        assert_eq!(incremental.entry.block[0].text, rebuilt.text);
        assert_eq!(
            incremental.entry.block[0].metadata.conceal,
            rebuilt.metadata.conceal
        );
        assert_eq!(
            incremental.entry.block[0].metadata.node,
            rebuilt.metadata.node
        );
        assert!(exchange.completed_at_ms.is_none() && exchange.checkpoint_after.is_none());
        println!(
            "QUESTION_FIXTURE:{}",
            serde_json::to_string(&projected.entry.block).unwrap()
        );
        exchange
            .append_input(
                crate::exchange::InputIntent::Answer,
                "Planning feedback:\n- Scope: Rust CLI".into(),
                7,
            )
            .unwrap();
        let mut cancelled = exchange.clone();
        cancelled.finish(ExchangeState::Cancelled, 7).unwrap();
        let cancelled = project_at(
            &TimelineEntry::Exchange {
                id: cancelled.id.clone(),
                created_at_ms: 0,
                exchange: cancelled,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            7,
        )
        .unwrap();
        let text = cancelled
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.find("I mean a Rust CLI").unwrap()
                < text.find("You answered: Scope: Rust CLI").unwrap()
        );
        exchange.resume(7).unwrap();
        event.address.as_mut().unwrap().turn_id = "continuation-turn".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        exchange.observe_turn(&event, 7).unwrap();
        event.turn_boundary = None;
        event.text = Some("Proceeding with Rust".into());
        exchange.observe_turn(&event, 8).unwrap();
        event.text = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 9).unwrap();
        exchange.finish(ExchangeState::Complete, 9).unwrap();
        let completed = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            9,
        )
        .unwrap();
        let text = completed
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.find("I mean a Rust CLI").unwrap()
                < text.find("You answered: Scope: Rust CLI").unwrap()
        );
        assert!(
            text.find("You answered: Scope: Rust CLI").unwrap()
                < text.find("Proceeding with Rust").unwrap()
        );
    }

    #[test]
    fn commentary_closes_groups_and_unsuccessful_writes_remain_nested() -> anyhow::Result<()> {
        use crate::backend::{
            BackendEvent, ProviderAddress, ProviderChangeKind, ProviderChangeSet,
            ProviderFileChange, ToolActivity, ToolActivityKind,
        };
        for boundary in ["commentary", "failed", "cancelled", "denied", "interrupted"] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"edits", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Write", "kind":"chat", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            exchange.resume(0)?;
            let address = ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            };
            exchange.start_turn(address.clone(), 0)?;
            let mut event = BackendEvent {
                received_at_ms: None,
                address: Some(address),
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: serde_json::Value::Null,
                summary: None,
                task_update: None,
                activity: None,
            };
            for (ordinal, name) in ["first", boundary, "last"].into_iter().enumerate() {
                if name == "commentary" {
                    event.kind = "assistant_message".into();
                    event.text = Some("Continue after this message".into());
                    event.data = json!({"message_id":"message", "phase":"commentary"});
                    event.activity = None;
                } else {
                    event.kind = "tool".into();
                    event.text = None;
                    event.data = serde_json::Value::Null;
                    event.activity = Some(ToolActivity {
                        id: name.into(),
                        title: name.into(),
                        output: Some(format!("{name} details")),
                        output_delta: false,
                        kind: if name == "command" {
                            ToolActivityKind::Command
                        } else {
                            ToolActivityKind::FileChange
                        },
                        status: Some(
                            if ordinal == 1 && name != "command" {
                                name
                            } else {
                                "completed"
                            }
                            .into(),
                        ),
                        change: ProviderChangeSet {
                            file: vec![ProviderFileChange {
                                path: format!("{name}.txt"),
                                move_path: None,
                                kind: ProviderChangeKind::Add,
                                diff: "content\n".into(),
                            }],
                        },
                    });
                }
                exchange.observe_turn(&event, ordinal as i64 + 1)?;
            }
            let rendered = project_at(
                &TimelineEntry::Exchange {
                    id: "edits".into(),
                    created_at_ms: 0,
                    exchange,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                10,
            )?;
            let heading: Vec<_> = rendered
                .entry
                .block
                .iter()
                .filter(|block| {
                    block.id.0.ends_with(":changes") && !block.id.0.starts_with("edits:results:")
                })
                .map(|block| block.text.wire_rows().join(""))
                .collect();
            assert_eq!(
                heading
                    .iter()
                    .filter(|text| text.contains(if boundary == "commentary" {
                        "Changed 1 file"
                    } else {
                        "Changed 2 files"
                    }))
                    .count(),
                if boundary == "commentary" { 2 } else { 1 },
                "{boundary}: {heading:?}"
            );
            if !matches!(boundary, "command" | "commentary") {
                assert!(
                    heading
                        .iter()
                        .any(|text| text.contains(&format!("File changes {boundary}"))),
                    "{heading:?}"
                );
                assert!(rendered.entry.block.iter().any(|block| {
                    block
                        .text
                        .wire_rows()
                        .join("")
                        .contains(&format!("{boundary} details"))
                }));
            }
        }
        Ok(())
    }

    #[test]
    fn alternating_tools_and_writes_restore_activity_without_exchange_summaries()
    -> anyhow::Result<()> {
        use crate::backend::{
            BackendEvent, ProviderAddress, ProviderChangeKind, ProviderChangeSet,
            ProviderFileChange, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"edits", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Implement", "kind":"plan_execution", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 0)?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        for (index, (id, kind, status, diff)) in [
            ("inspect", ToolActivityKind::Command, "completed", ""),
            (
                "add",
                ToolActivityKind::FileChange,
                "completed",
                "@@ -0,0 +1 @@\n+first",
            ),
            ("read", ToolActivityKind::Command, "completed", ""),
            (
                "edit",
                ToolActivityKind::FileChange,
                "completed",
                "@@ -1 +1 @@\n-first\n+second",
            ),
            (
                "rejected",
                ToolActivityKind::FileChange,
                "failed",
                "@@ -1 +1 @@\n-second\n+wrong",
            ),
            ("check", ToolActivityKind::Command, "completed", ""),
        ]
        .into_iter()
        .enumerate()
        {
            let file_change = kind == ToolActivityKind::FileChange;
            event.activity = Some(ToolActivity {
                id: id.into(),
                kind,
                title: if file_change {
                    "file changes".into()
                } else {
                    id.into()
                },
                output: None,
                output_delta: false,
                status: Some("running".into()),
                change: ProviderChangeSet {
                    file: if file_change {
                        vec![ProviderFileChange {
                            path: "src/lib.rs".into(),
                            move_path: None,
                            kind: if id == "add" {
                                ProviderChangeKind::Add
                            } else {
                                ProviderChangeKind::Update
                            },
                            diff: diff.into(),
                        }]
                    } else {
                        Vec::new()
                    },
                },
            });
            exchange.observe_turn(&event, index as i64 * 10 + 1)?;
            if file_change {
                let projected = project_at(
                    &TimelineEntry::Exchange {
                        id: "edits".into(),
                        created_at_ms: 0,
                        exchange: exchange.clone(),
                        agent_by_id: HashMap::new(),
                    },
                    &WidthProfile::default(),
                    100,
                )?;
                assert!(projected.entry.block.iter().any(|block| {
                    block
                        .text
                        .wire_rows()
                        .join("")
                        .to_lowercase()
                        .contains("changing 1 file")
                }));
            }
            event.activity.as_mut().unwrap().status = Some(status.into());
            exchange.observe_turn(&event, index as i64 * 10 + 2)?;
        }
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 70)?;
        exchange.finish(crate::exchange::ExchangeState::Complete, 70)?;
        let restored = serde_json::from_value(serde_json::to_value(&exchange)?)?;
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: "edits".into(),
                created_at_ms: 0,
                exchange: restored,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            100,
        )?;
        let blocks = &projected.entry.block;
        let groups: Vec<_> = blocks
            .iter()
            .filter(|block| block.id.0.ends_with(":tools"))
            .collect();
        assert_eq!(groups.len(), 1);
        assert!(groups[0].text.wire_rows().join("").contains("Ran 3 tools"));
        let headings: Vec<_> = blocks
            .iter()
            .filter(|block| {
                block.id.0.ends_with(":changes") && !block.id.0.starts_with("edits:results:")
            })
            .map(|block| block.text.wire_rows().join(""))
            .collect();
        assert_eq!(headings.len(), 2);
        assert!(headings[0].contains("Changed 1 file"));
        assert!(headings[1].contains("File changes failed"));
        assert_eq!(
            projected.tool.len(),
            3,
            "file edits must not register generic output views"
        );
        assert!(
            !blocks
                .iter()
                .any(|block| block.text.wire_rows().join("").contains("file changes"))
        );
        assert!(!blocks.iter().any(|block| block.id.0 == "edits:changes"));
        assert!(
            !projected
                .action
                .contains_key(&super::TargetId("edits:changes".into()))
        );
        let reported = "--- a/src/lib.rs\n+++ b/src/lib.rs\n@@ -0,0 +1 @@\n+second\n";
        let checkpoint = "--- a/generated.rs\n+++ b/generated.rs\n@@ -0,0 +1 @@\n+generated\n";
        for (checkpoint_after, attributed, total, matches) in [
            (None, Some(reported), None, false),
            (Some("checkpoint"), Some(reported), Some(reported), true),
            (Some("checkpoint"), Some(reported), Some(checkpoint), false),
            (Some("checkpoint"), None, Some(checkpoint), false),
        ] {
            exchange.checkpoint_after = checkpoint_after.map(str::to_owned);
            exchange.attributed_diff_text = attributed.map(str::to_owned);
            exchange.checkpoint_diff_text = total.map(str::to_owned);
            exchange.attributed_matches_checkpoint = matches;
            let restored = serde_json::from_value(serde_json::to_value(&exchange)?)?;
            let with_checkpoint = project_at(
                &TimelineEntry::Exchange {
                    id: "edits".into(),
                    created_at_ms: 0,
                    exchange: restored,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                100,
            )?;
            assert_eq!(with_checkpoint.entry.block, *blocks);
            assert!(
                !with_checkpoint
                    .action
                    .contains_key(&super::TargetId("edits:changes".into()))
            );
            assert!(
                !with_checkpoint
                    .action
                    .contains_key(&super::TargetId("edits:checkpoint".into()))
            );
        }
        Ok(())
    }

    #[test]
    fn tool_groups_align_mixed_durations_as_running_timers_change() {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"aligned", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 0).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        let mut clock = 1;
        for (index, duration) in [Some(4), Some(21), Some(1000), Some(626), None]
            .into_iter()
            .enumerate()
        {
            event.activity = Some(ToolActivity {
                id: format!("tool-{index}"),
                kind: if index % 2 == 0 {
                    ToolActivityKind::Command
                } else {
                    ToolActivityKind::ToolCall
                },
                title: format!("inspect_{index}"),
                output: None,
                output_delta: false,
                status: Some("running".into()),
                change: Default::default(),
            });
            exchange.observe_turn(&event, clock).unwrap();
            if let Some(duration) = duration {
                clock += duration;
                event.activity.as_mut().unwrap().status = Some("completed".into());
                exchange.observe_turn(&event, clock).unwrap();
            }
            clock += 1;
        }
        for now in [clock, clock + 10_000_000] {
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                now,
            )
            .unwrap();
            let headings = projected
                .entry
                .block
                .iter()
                .filter_map(|block| block.text.row(0))
                .filter(|row| row.contains("• "))
                .collect::<Vec<_>>();
            assert_eq!(headings.len(), 5);
            let column = headings[0].find("inspect_").unwrap();
            assert!(
                headings
                    .iter()
                    .all(|row| row.find("inspect_") == Some(column)),
                "{headings:?}"
            );
            assert!(headings[0].contains("   0s inspect_0"));
            assert!(headings[2].contains("   1s inspect_2"));
            let group = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":tools"))
                .unwrap();
            assert_eq!(
                group.text.row(0),
                Some(
                    format!(
                        "▸ Running 5 tools · {}",
                        super::super::duration::tool_duration(Some(1652 + (now - clock) as u64))
                            .trim()
                    )
                    .as_str()
                )
            );
            let document = forge_buffer::document::BufferDocument::new(
                forge_buffer::identity::DocumentId("group-timer".into()),
                projected.entry.block.clone(),
            )
            .unwrap();
            let refreshed = super::timing_blocks(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                &document,
                &Default::default(),
                &projected.tool,
            )
            .unwrap();
            let refreshed_group = refreshed.iter().find(|block| block.id == group.id).unwrap();
            assert!(
                refreshed_group
                    .text
                    .row(0)
                    .unwrap()
                    .starts_with("▸ Running 5 tools · ")
            );
            assert_ne!(refreshed_group.text, group.text);
            assert_eq!(refreshed_group.metadata.layout, group.metadata.layout);
            assert!(
                refreshed
                    .iter()
                    .all(|block| !block.id.0.ends_with(":preview")),
                "clock ticks must not replace tool output bodies"
            );
            let refreshed_headings = refreshed
                .iter()
                .filter_map(|block| block.text.row(0))
                .filter(|row| row.contains("inspect_"))
                .collect::<Vec<_>>();
            assert_eq!(refreshed_headings.len(), 5);
            let refreshed_column = refreshed_headings[0].find("inspect_").unwrap();
            assert!(
                refreshed_headings
                    .iter()
                    .all(|row| row.find("inspect_") == Some(refreshed_column))
            );
            for block in projected
                .entry
                .block
                .iter()
                .filter(|block| block.text.row(0).is_some_and(|row| row.contains("• ")))
            {
                assert!(block.text.row(0).unwrap().starts_with('•'));
                assert_eq!(
                    block.metadata.layout.as_ref().unwrap().indent,
                    4,
                    "tool rows inherit the group indent without adding a second text indent"
                );
                assert!(
                    block
                        .metadata
                        .decoration
                        .iter()
                        .filter(|decoration| matches!(
                            decoration.capture.as_str(),
                            "ForgeHarnessCommand" | "ForgeHarnessMcpName"
                        ))
                        .all(|decoration| decoration.range.start.column == column)
                );
            }
        }
    }

    #[test]
    fn tool_group_totals_sum_parallel_calls_and_require_complete_timing() {
        let mut first: crate::turn::ToolCall = serde_json::from_value(json!({
            "id":"first", "kind":"command", "title":"first", "output":"",
            "status":"completed", "failed":false, "started_at_ms":0, "completed_at_ms":1050
        }))
        .unwrap();
        let mut second = first.clone();
        second.id = "second".into();
        second.completed_at_ms = None;
        second.status = "running".into();
        assert_eq!(
            super::tool_group_label([&first, &second], NodeLifecycle::Live, 1500),
            "▸ Running 2 tools · 2.6s"
        );
        second.completed_at_ms = Some(2000);
        second.status = "failed".into();
        second.failed = true;
        assert_eq!(
            super::tool_group_label([&first, &second], NodeLifecycle::Settled, 90_000),
            "▸ Ran 2 tools (1 failed) · 3.1s"
        );
        first.started_at_ms = None;
        assert_eq!(
            super::tool_group_label([&first, &second], NodeLifecycle::Settled, 90_000),
            "▸ Ran 2 tools (1 failed)"
        );
    }

    #[test]
    fn adjacent_tools_share_one_group_across_turns_and_hidden_reasoning() -> anyhow::Result<()> {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        use crate::exchange::ExchangeState;
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"cross-turn", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect the repository", "kind":"chat", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn-1".into(),
            }),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        let render = |exchange: &Exchange| {
            project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                100,
            )
        };
        let mut first_group_id = None;
        for ordinal in 1..=3 {
            event.address.as_mut().unwrap().turn_id = format!("turn-{ordinal}");
            event.turn_boundary = Some(TurnBoundary::Started);
            event.activity = None;
            event.text = None;
            exchange.observe_turn(&event, ordinal * 10)?;
            event.turn_boundary = None;
            for kind in ["reasoning", "reasoning_summary"] {
                event.kind = kind.into();
                event.text = Some("Thoughts must not become commentary".into());
                event.data = json!({"message_id":format!("{kind}-{ordinal}")});
                exchange.observe_turn(&event, ordinal * 10 + 1)?;
            }
            event.kind = "tool".into();
            event.text = None;
            event.data = Value::Null;
            event.activity = Some(ToolActivity {
                id: "shared-call".into(),
                kind: ToolActivityKind::Command,
                title: format!("Inspect file {ordinal}"),
                output: Some(format!("output-{ordinal}\n")),
                output_delta: false,
                status: Some("in_progress".into()),
                change: Default::default(),
            });
            exchange.observe_turn(&event, ordinal * 10 + 2)?;
            let projected = render(&exchange)?;
            assert!(!projected.entry.block.iter().any(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("Thoughts must not"))
            }));
            let groups = projected
                .entry
                .block
                .iter()
                .filter(|block| block.id.0.ends_with(":tools"))
                .collect::<Vec<_>>();
            assert_eq!(
                groups.len(),
                1,
                "hidden reasoning or provider turns split adjacent tools"
            );
            let retained_id = first_group_id.get_or_insert_with(|| groups[0].id.clone());
            assert_eq!(&groups[0].id, &*retained_id);
            assert!(
                !groups[0].metadata.fold[0].closed,
                "the latest running tool lost its preview"
            );
            for owner in 1..=ordinal {
                assert!(
                    projected.tool.contains_key(&format!(
                        "{}:shared-call",
                        exchange.turn[owner as usize - 1].id()
                    )),
                    "grouping replaced a tool's provider-turn identity"
                );
            }
            let document = forge_buffer::document::BufferDocument::new(
                forge_buffer::identity::DocumentId("cross-turn-clock".into()),
                projected.entry.block.clone(),
            )?;
            let refreshed = super::timing_blocks(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                &document,
                &Default::default(),
                &projected.tool,
            )?;
            let refreshed_group = refreshed
                .iter()
                .find(|block| block.id == groups[0].id)
                .unwrap();
            assert!(
                refreshed_group
                    .text
                    .row(0)
                    .unwrap()
                    .starts_with(&format!("▸ Running {ordinal} tool")),
                "timer refresh counted only the newest provider turn"
            );
            assert_eq!(refreshed_group.metadata.node, groups[0].metadata.node);
            event.activity.as_mut().unwrap().status =
                Some(if ordinal == 2 { "failed" } else { "completed" }.into());
            exchange.observe_turn(&event, ordinal * 10 + 3)?;
            let completed_call = render(&exchange)?;
            let group = completed_call
                .entry
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":tools"))
                .unwrap();
            assert!(
                group
                    .text
                    .row(0)
                    .unwrap()
                    .starts_with(&format!("▸ Running {ordinal} tool")),
                "individual tool completion settled an open group"
            );
            assert_eq!(
                group.metadata.node.as_ref().unwrap().lifecycle,
                NodeLifecycle::Live
            );
            event.activity = None;
            event.turn_boundary = Some(TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            exchange.observe_turn(&event, ordinal * 10 + 4)?;
            let completed_turn = render(&exchange)?;
            let group = completed_turn
                .entry
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":tools"))
                .unwrap();
            assert!(
                group
                    .text
                    .row(0)
                    .unwrap()
                    .starts_with(&format!("▸ Running {ordinal} tool"))
            );
            assert!(!group.metadata.fold[0].closed);
        }
        let projected = render(&exchange)?;
        let group = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0.ends_with(":tools"))
            .unwrap();
        assert!(group.text.row(0).unwrap().contains("Running 3 tools"));
        assert!(
            !group.metadata.fold[0].closed,
            "provider turn completion closed a group that can receive more tools"
        );

        event.address.as_mut().unwrap().turn_id = "turn-4".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        exchange.observe_turn(&event, 40)?;
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("Now inspect the final file".into());
        event.data = json!({"message_id":"commentary","phase":"commentary"});
        exchange.observe_turn(&event, 41)?;
        event.kind = "tool".into();
        event.text = None;
        event.data = Value::Null;
        event.activity = Some(ToolActivity {
            id: "shared-call".into(),
            kind: ToolActivityKind::Command,
            title: "Inspect final file".into(),
            output: Some("final-output\n".into()),
            output_delta: false,
            status: Some("completed".into()),
            change: Default::default(),
        });
        exchange.observe_turn(&event, 42)?;
        let projected = render(&exchange)?;
        assert_eq!(
            projected
                .entry
                .block
                .iter()
                .filter(|block| block.id.0.ends_with(":tools"))
                .count(),
            2,
            "visible commentary must separate tool groups"
        );
        let groups = projected
            .entry
            .block
            .iter()
            .filter(|block| block.id.0.ends_with(":tools"))
            .collect::<Vec<_>>();
        assert!(groups[0].text.row(0).unwrap().starts_with("▸ Ran 3 tools"));
        assert!(
            groups[1]
                .text
                .row(0)
                .unwrap()
                .starts_with("▸ Running 1 tool")
        );
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 43)?;
        for outcome in [
            ExchangeState::Complete,
            ExchangeState::Cancelled,
            ExchangeState::Interrupted,
            ExchangeState::Failed,
        ] {
            let mut finished = exchange.clone();
            finished.finish(outcome, 50)?;
            let projected = render(&finished)?;
            for group in projected
                .entry
                .block
                .iter()
                .filter(|block| block.id.0.ends_with(":tools"))
            {
                assert!(group.text.row(0).unwrap().starts_with("▸ Ran "));
                assert_eq!(
                    group.metadata.node.as_ref().unwrap().lifecycle,
                    NodeLifecycle::Settled
                );
                assert!(group.metadata.fold[0].closed);
            }
        }
        Ok(())
    }

    #[test]
    fn sequential_tools_keep_group_open_and_only_latest_preview_expanded() {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"sequence", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect the repository", "kind":"chat", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            }),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 0).unwrap();
        event.turn_boundary = None;
        event.kind = "tool".into();
        let render = |exchange: &Exchange| {
            project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                10,
            )
            .unwrap()
        };
        for count in 1..=4 {
            for status in ["in_progress", "completed"] {
                event.activity = Some(ToolActivity {
                    id: format!("call-{count}"),
                    kind: ToolActivityKind::Command,
                    title: format!("Inspect repository file {count}"),
                    output: Some(format!("first-{count}\nmiddle\nlast-{count}\n")),
                    output_delta: false,
                    status: Some(status.into()),
                    change: Default::default(),
                });
                exchange.observe_turn(&event, count).unwrap();
                let projected = render(&exchange);
                let group = projected
                    .entry
                    .block
                    .iter()
                    .find(|block| block.id.0.ends_with(":tools"))
                    .unwrap();
                assert!(
                    !group.metadata.fold[0].closed,
                    "group collapsed between sequential calls"
                );
                for block in &projected.entry.block {
                    if block.text.row(0).is_some_and(|row| row.starts_with("  └ ")) {
                        assert_eq!(
                            block.metadata.layout.as_ref().unwrap().indent,
                            group.metadata.layout.as_ref().unwrap().indent
                        );
                    }
                }
                let tool_folds: Vec<_> = projected
                    .entry
                    .block
                    .iter()
                    .flat_map(|block| &block.metadata.fold)
                    .filter(|fold| {
                        fold.id != group.metadata.fold[0].id && !fold.id.0.ends_with(":exchange")
                    })
                    .collect();
                for (index, fold) in tool_folds.iter().enumerate() {
                    assert_eq!(
                        fold.closed,
                        index + 1 < count as usize,
                        "only a completed tool with a successor should collapse"
                    );
                }
                assert_eq!(
                    projected.tool.len(),
                    count as usize,
                    "hidden tools must retain full sources"
                );
                assert_eq!(
                    group.metadata.node.as_ref().unwrap().default_display,
                    NodeDisplay::Preview
                );
                if status == "completed" {
                    assert_eq!(tool_folds.len(), count as usize);
                    let text = projected
                        .entry
                        .block
                        .iter()
                        .flat_map(|block| block.text.wire_rows())
                        .collect::<Vec<_>>()
                        .join("\n");
                    assert!(text.contains(&format!("first-{count}")));
                    assert!(text.contains(&format!("last-{count}")));
                }
            }
        }
        let mut finished = exchange.clone();
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        finished.observe_turn(&event, 11).unwrap();
        assert!(
            !render(&finished)
                .entry
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":tools"))
                .unwrap()
                .metadata
                .fold[0]
                .closed,
            "turn completion must keep the final group open for more provider calls"
        );
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("Repository inspection complete".into());
        event.data = json!({"phase":"commentary"});
        exchange.observe_turn(&event, 11).unwrap();
        assert!(
            render(&exchange)
                .entry
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":tools"))
                .unwrap()
                .metadata
                .fold[0]
                .closed,
            "following commentary must close the group"
        );
    }

    #[test]
    fn saved_plan_artifacts_use_their_revision_label_and_keep_real_file_targets() {
        let path = "D:/forge/plans/session-guid/plan-guid/working.md";
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"revision", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        for revision in [1, 2] {
            exchange
                .node_list
                .push(crate::exchange::ExchangeNode::ArtifactChange {
                    change: crate::exchange::ArtifactChange {
                        id: format!("artifact-{revision}"),
                        declaration: None,
                        path: path.into(),
                        created_at_ms: revision,
                        diff_text: format!("--- {path}\n+++ {path}\n@@ -1 +1 @@\n-old\n+new\n"),
                    },
                });
            let lifecycle = serde_json::from_value(json!({
                "id":format!("event-{revision}"), "session_id":"session", "plan_id":"plan",
                "title":"Migrate cloud diagnostics to Rust", "kind":"revision_created",
                "model_revision":revision, "user_revision":0, "created_at_ms":revision
            }))
            .unwrap();
            exchange
                .node_list
                .push(crate::exchange::ExchangeNode::PlanEvent {
                    event: Box::new(crate::plan::ExchangePlanEvent {
                        id: format!("event-{revision}"),
                        node_count: exchange.node_list.len(),
                        content: crate::plan::PlanEventContent::Lifecycle {
                            title: "Migrate cloud diagnostics to Rust".into(),
                            lifecycle,
                        },
                    }),
                });
        }
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            10,
        )
        .unwrap();
        for revision in [1, 2] {
            let block = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == format!("artifact-{revision}:file:0"))
                .unwrap();
            assert_eq!(block.text.row(0), Some(format!(
                "Modified Artifact: Migrate cloud diagnostics to Rust (revision {revision}) +1 -1"
            ).as_str()));
            assert!(
                block
                    .metadata
                    .decoration
                    .iter()
                    .any(|span| span.capture == "ForgeStatusPath")
            );
        }
        assert!(projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::File { path: target, line:1 } if target == path)));
        assert!(projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::Diff { text, .. } if text.contains(path))));
    }

    #[test]
    fn declaration_revision_projects_file_deltas_and_overview_as_separate_trees() {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"revision", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let mut response = crate::backend::BackendEvent {
            received_at_ms: None,
            address: Some(crate::backend::ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            }),
            turn_boundary: Some(crate::backend::TurnBoundary::Started),
            kind: "assistant_message".into(),
            text: Some("Submitted the revised design.".into()),
            data: json!({"phase":"final_answer"}),
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange
            .start_turn(response.address.clone().unwrap(), 0)
            .unwrap();
        response.turn_boundary = None;
        exchange.observe_turn(&response, 1).unwrap();
        response.turn_boundary = Some(crate::backend::TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        response.text = None;
        exchange.observe_turn(&response, 2).unwrap();
        exchange.node_list.push(crate::exchange::ExchangeNode::ArtifactChange {
            change: crate::exchange::ArtifactChange {
                id: "artifact".into(), path: "working.md".into(), created_at_ms: 1,
                diff_text: "--- a/src/lib.rs\n+++ b/src/lib.rs\n@@ -1 +1 @@\n-pub struct Old;\n+pub struct New;\n".into(),
                declaration: Some(crate::exchange::DeclarationRevision {
                    plan_id: "plan".into(), revision: 3,
                    baseline_paths: Default::default(),
                    document_diff: "--- a/Description\n+++ b/Description\n@@ -1 +1 @@\n-old overview\n+new overview\n".into(),
                }),
            },
        });
        let exchange: Exchange =
            serde_json::from_slice(&serde_json::to_vec(&exchange).unwrap()).unwrap();
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            10,
        )
        .unwrap();
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.find("Proposed changes").unwrap()
                < text.find("Submitted the revised design.").unwrap()
        );
        assert!(
            text.find("Plan overview").unwrap()
                < text.find("Submitted the revised design.").unwrap()
        );
        assert!(text.contains("Proposed changes 1 file +1 -1"));
        assert!(text.contains("Modified src/lib.rs +1 -1"));
        assert!(text.contains("Plan overview 1 section +1 -1"));
        assert!(text.contains("Modified Description +1 -1"));
        assert!(!text.contains("Artifact:") && !text.contains("working.md"));
        assert!(projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::Declaration { path, revision: 3, document: true, .. } if path == "Description")));
    }

    #[test]
    fn agent_history_renders_beyond_former_nesting_limit() -> anyhow::Result<()> {
        let mut child: Option<TimelineEntry> = None;
        for index in (0..32).rev() {
            let id = format!("agent-{index}");
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":id,"session_id":"session","agent_id":id,"ordinal":1,
                "prompt":format!("Request {index}"),"state":"complete",
                "created_at_ms":0,"completed_at_ms":1,
                "attributed_matches_checkpoint":true,"node_list":[]
            }))?;
            if let Some(child) = &child {
                exchange
                    .node_list
                    .push(crate::exchange::ExchangeNode::AgentReference {
                        agent: crate::agent::Delegation {
                            id: child.id(),
                            parent_exchange_id: id.clone(),
                            parent_turn_id: None,
                            child_agent_id: child.id(),
                            child_exchange_id: child.id(),
                            task: "Inspect nested work".into(),
                            created_at_ms: 0,
                        },
                    });
            }
            let mut run = crate::agent::Agent::pending("session", &id, "Inspect", 0);
            run.id = id.clone();
            child = Some(TimelineEntry::AgentLifecycle {
                id,
                created_at_ms: 0,
                run,
                exchange: vec![exchange],
                agent: child.into_iter().collect(),
            });
        }
        let entry = child.unwrap();
        let projected = project_at(&entry, &WidthProfile::default(), 1)?;
        assert_eq!(projected.prompt.len(), 32);
        assert!(
            projected
                .entry
                .block
                .iter()
                .any(|block| block.id.0 == "agent-31:prompt")
        );
        Ok(())
    }

    #[test]
    fn child_preview_nests_live_activity_and_completed_task_history() {
        use crate::exchange::ExchangeState;
        let mut nested: Exchange = serde_json::from_value(json!({
            "id":"nested", "session_id":"session", "agent_id":"nested-agent", "ordinal":1,
            "prompt":"Inspect nested work", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        nested.resume(0).unwrap();
        let mut update = crate::backend::BackendEvent {
            received_at_ms: None,
            address: Some(crate::backend::ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            }),
            turn_boundary: Some(crate::backend::TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        nested.observe_turn(&update, 0).unwrap();
        update.turn_boundary = None;
        update.kind = "assistant_message".into();
        update.data = json!({"phase":"commentary", "provider_message_id":"earlier"});
        update.text = Some("Earlier child thought".into());
        nested.observe_turn(&update, 1).unwrap();
        update.data = json!({"phase":"commentary", "provider_message_id":"latest"});
        update.text = Some("Latest child thought".into());
        nested.observe_turn(&update, 2).unwrap();
        update.data = json!({"phase":"final_answer"});
        update.text = Some("Child final response".into());
        nested.observe_turn(&update, 3).unwrap();
        update.text = None;
        update.activity = Some(crate::backend::ToolActivity {
            id: "call".into(),
            kind: crate::backend::ToolActivityKind::ToolCall,
            title: "Child tool".into(),
            output: Some("Child tool result".into()),
            output_delta: false,
            status: Some("completed".into()),
            change: Default::default(),
        });
        nested.observe_turn(&update, 3).unwrap();
        let mut parent = nested.clone();
        parent.id = "parent".into();
        parent.turn.clear();
        parent.node_list.clear();
        update.activity = None;
        update.address.as_mut().unwrap().turn_id = "root-turn".into();
        update.turn_boundary = Some(crate::backend::TurnBoundary::Started);
        parent.observe_turn(&update, 0).unwrap();
        update.turn_boundary = None;
        update.activity = Some(crate::backend::ToolActivity {
            id: "spawn-call".into(),
            kind: crate::backend::ToolActivityKind::ToolCall,
            title: "agent_spawn(task_name=Nested)".into(),
            output: Some("SPAWN_PAYLOAD".into()),
            output_delta: false,
            status: Some("completed".into()),
            change: Default::default(),
        });
        parent.observe_turn(&update, 1).unwrap();
        parent
            .observe_turn(
                &crate::backend::BackendEvent {
                    activity: None,
                    text: Some("Parent final response".into()),
                    ..update.clone()
                },
                2,
            )
            .unwrap();
        parent
            .node_list
            .push(crate::exchange::ExchangeNode::AgentReference {
                agent: crate::agent::Delegation {
                    id: "delegation".into(),
                    parent_exchange_id: parent.id.clone(),
                    parent_turn_id: Some(parent.turn.last().unwrap().id().to_owned()),
                    child_agent_id: "nested-agent".into(),
                    child_exchange_id: nested.id.clone(),
                    task: "Inspect nested work".into(),
                    created_at_ms: 0,
                },
            });
        let entry = TimelineEntry::Exchange {
            id: "parent".into(),
            created_at_ms: 0,
            exchange: parent,
            agent_by_id: HashMap::from([(
                "nested-agent".into(),
                TimelineEntry::AgentLifecycle {
                    id: "nested-agent".into(),
                    created_at_ms: 0,
                    run: crate::agent::Agent::pending("session", "Nested", "Inspect", 0),
                    exchange: vec![nested.clone()],
                    agent: vec![],
                },
            )]),
        };
        let projected = project_at(&entry, &WidthProfile::default(), 0).unwrap();
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Latest child thought"));
        assert!(!text.contains("Earlier child thought"));
        assert!(!text.contains("Child final response"));
        assert!(text.contains("Child tool"));
        assert!(!text.contains("Child tool result"));
        assert!(!text.contains("agent_spawn"));
        assert!(!text.contains("SPAWN_PAYLOAD"));
        let thought = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "delegation:thought")
            .unwrap();
        assert!(thought.metadata.layout.as_ref().unwrap().indent > 2);
        let heading = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "delegation")
            .unwrap();
        assert!(heading.text.wire_rows().join("\n").contains("tools"));
        assert_eq!(
            heading.metadata.node.as_ref().unwrap().kind,
            forge_buffer::node::NodeKind::Exchange
        );
        assert!(
            heading
                .text
                .wire_rows()
                .join("\n")
                .contains("Running 0s · Nested")
        );
        assert_eq!(
            heading
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .marker
                .as_ref()
                .unwrap()
                .text,
            "▸"
        );
        assert!(!heading.metadata.fold[0].closed);
        assert_eq!(
            heading
                .metadata
                .node
                .as_ref()
                .unwrap()
                .parent
                .as_ref()
                .unwrap()
                .0,
            "parent:exchange"
        );
        let action = heading.metadata.target.first().unwrap();
        assert!(
            matches!(&projected.action[&action.id], super::TranscriptAction::Agent { run_id } if !run_id.is_empty())
        );
        let child_position = projected
            .entry
            .block
            .iter()
            .position(|block| block.id.0 == "delegation")
            .unwrap();
        let response_position = projected
            .entry
            .block
            .iter()
            .position(|block| {
                block
                    .text
                    .wire_rows()
                    .join("\n")
                    .contains("Parent final response")
            })
            .unwrap();
        assert!(
            child_position < response_position,
            "late child references belong inside parent activity"
        );
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("child-preview".into()),
            projected.entry.block.clone(),
        )
        .unwrap();
        let refreshed = super::timing_blocks(
            &entry,
            &WidthProfile::default(),
            &document,
            &Default::default(),
            &projected.tool,
        )
        .unwrap();
        let refreshed_heading = refreshed
            .iter()
            .find(|block| block.id.0 == "delegation")
            .unwrap();
        assert!(
            refreshed_heading
                .text
                .wire_rows()
                .join("\n")
                .contains("s · Nested")
        );
        assert_eq!(
            refreshed_heading
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .marker
                .as_ref()
                .unwrap()
                .text,
            "▸"
        );
        assert!(
            !refreshed
                .iter()
                .any(|block| block.id.0 == "delegation:thought"),
            "timers must not replace the retained child thought"
        );
        for (service_tier, suffix) in [
            (crate::backend::ServiceTier::Standard, ""),
            (crate::backend::ServiceTier::Fast, " +"),
            (crate::backend::ServiceTier::Ultrafast, " ++"),
        ] {
            for (state, lifecycle) in [
                (ExchangeState::Running, "Running"),
                (ExchangeState::Complete, "Ran"),
            ] {
                let mut selected_entry = entry.clone();
                if let TimelineEntry::Exchange { agent_by_id, .. } = &mut selected_entry {
                    let Some(TimelineEntry::AgentLifecycle { run, exchange, .. }) =
                        agent_by_id.get_mut("nested-agent")
                    else {
                        panic!("missing child fixture");
                    };
                    run.selection = Some(crate::agent::AgentSelection {
                        profile: "remote-explorer".into(),
                        instructions: String::new(),
                        provider: crate::backend::BackendKind::Codex,
                        model: "gpt-6.1-sol".into(),
                        reasoning_effort: "high".into(),
                        context_window: None,
                        service_tier,
                        access: Default::default(),
                        execution_mode: crate::session::PermissionMode::Write,
                    });
                    exchange[0].state = state;
                    exchange[0].completed_at_ms =
                        (state == ExchangeState::Complete).then_some(7000);
                }
                let selected = project_at(&selected_entry, &WidthProfile::default(), 7000).unwrap();
                let expected = format!("s · Nested · codex · gpt-6.1-sol{suffix}");
                let heading = selected
                    .entry
                    .block
                    .iter()
                    .find(|block| block.id.0 == "delegation")
                    .unwrap();
                assert!(
                    heading
                        .text
                        .wire_rows()
                        .join("\n")
                        .contains(&format!("{lifecycle} "))
                );
                assert!(heading.text.wire_rows().join("\n").contains(&expected));
                assert_eq!(
                    heading.metadata.fold[0].closed,
                    state == ExchangeState::Complete
                );
                let contents = selected
                    .entry
                    .block
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .collect::<Vec<_>>()
                    .join("\n");
                if state == ExchangeState::Complete {
                    assert!(
                        contents.contains("Earlier child thought")
                            && contents.contains("Child final response")
                    );
                    assert!(
                        contents.contains("Child tool") && contents.contains("Inspect nested work")
                    );
                    assert!(selected.tool.values().any(|output| {
                        output
                            .preview(true)
                            .unwrap()
                            .row
                            .contains(&"Child tool result")
                    }));
                    let mut nodes = super::super::nodes::NodeMap::default();
                    let live_heading = projected
                        .entry
                        .block
                        .iter()
                        .find(|block| block.id.0 == "delegation")
                        .unwrap();
                    nodes.register(live_heading).unwrap();
                    nodes.choice.insert("delegation".into(), true);
                    nodes.register(heading).unwrap();
                    assert!(
                        !nodes.choice.contains_key("delegation"),
                        "completion resets a live expansion"
                    );
                    nodes.choice.insert("delegation".into(), true);
                    nodes.register(heading).unwrap();
                    assert_eq!(
                        nodes.choice.get("delegation"),
                        Some(&true),
                        "completed history stays manually expandable"
                    );
                } else {
                    assert!(
                        !contents.contains("Child final response")
                            && !contents.contains("Child tool result")
                    );
                }
                let document = forge_buffer::document::BufferDocument::new(
                    forge_buffer::identity::DocumentId("speed-preview".into()),
                    selected.entry.block,
                )
                .unwrap();
                let refreshed = super::timing_blocks(
                    &selected_entry,
                    &WidthProfile::default(),
                    &document,
                    &Default::default(),
                    &selected.tool,
                )
                .unwrap();
                let heading = refreshed
                    .iter()
                    .find(|block| block.id.0 == "delegation")
                    .unwrap();
                assert!(
                    heading
                        .text
                        .wire_rows()
                        .join("\n")
                        .contains(&format!("{lifecycle} "))
                );
                assert!(heading.text.wire_rows().join("\n").contains(&expected));
            }
        }
        let mut failed_entry = entry.clone();
        if let TimelineEntry::Exchange { exchange, .. } = &mut failed_entry {
            update.activity.as_mut().unwrap().id = "failed-spawn-call".into();
            update.activity.as_mut().unwrap().status = Some("failed".into());
            exchange.observe_turn(&update, 2).unwrap();
        }
        let failed = project_at(&failed_entry, &WidthProfile::default(), 2).unwrap();
        assert!(
            failed
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .any(|row| row.contains("agent_spawn")),
            "failed spawn errors must remain inspectable"
        );
        let inspection = project_at(
            &TimelineEntry::AgentLifecycle {
                id: "nested-agent".into(),
                created_at_ms: 0,
                run: crate::agent::Agent::pending("session", "Nested", "Inspect", 0),
                exchange: vec![nested],
                agent: vec![],
            },
            &WidthProfile::default(),
            3,
        )
        .unwrap();
        let full = inspection
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(full.contains("Earlier child thought") && full.contains("Latest child thought"));
        assert!(full.contains("Child tool result"));
    }

    #[test]
    fn plan_revision_details_share_their_fold_ownership_and_indentation() {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"revision", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        for (index, kind) in [
            "changes_requested",
            "question_asked",
            "question_answered",
            "question_withdrawn",
            "created",
            "revision_created",
            "accepted",
            "cancelled",
        ]
        .into_iter()
        .enumerate()
        {
            let identity = format!("event-{index}");
            let lifecycle = serde_json::from_value(json!({
                "id":identity, "session_id":"session", "plan_id":"plan", "title":"Migration",
                "kind":kind, "model_revision":1, "user_revision":0, "created_at_ms":index,
                "overall_comment":format!("Rename CloudServiceSettings\n\n{}", "Preserve the configuration role. ".repeat(10))
            })).unwrap();
            exchange
                .node_list
                .push(crate::exchange::ExchangeNode::PlanEvent {
                    event: Box::new(crate::plan::ExchangePlanEvent {
                        id: identity,
                        node_count: index,
                        content: crate::plan::PlanEventContent::Lifecycle {
                            title: "Migration".into(),
                            lifecycle,
                        },
                    }),
                });
        }
        for columns in [40, 80, 120] {
            let width = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &width,
                21_000,
            )
            .unwrap();
            let blocks = &projected.entry.block;
            let indentation = |block: &forge_buffer::block::BufferBlock, _row| {
                " ".repeat(block.metadata.layout.as_ref().unwrap().indent - 2)
            };
            for index in 1..8 {
                let heading = blocks
                    .iter()
                    .find(|block| block.id.0 == format!("event-{index}"))
                    .unwrap();
                let detail = blocks
                    .iter()
                    .find(|block| block.id.0 == format!("event-{index}:comment"))
                    .unwrap();
                let activity_indent = if [4, 5].contains(&index) { "" } else { "  " };
                assert_eq!(indentation(heading, 0), activity_indent);
                for row in 0..detail.text.row_count() {
                    assert_eq!(
                        indentation(detail, row),
                        format!("{activity_indent}  "),
                        "detail acquired a response marker"
                    );
                }
                assert_eq!(heading.metadata.fold.len(), 1);
                if ![1, 2, 3].contains(&index) {
                    assert!(matches!(
                        projected
                            .action
                            .get(&forge_buffer::identity::TargetId(format!("event-{index}"))),
                        Some(super::TranscriptAction::Plan {
                            revision: Some(1),
                            ..
                        })
                    ));
                }
                assert_eq!(heading.metadata.fold[0].end.block, detail.id);
                assert_eq!(
                    heading.metadata.fold[0].end.position.row,
                    detail.text.row_count()
                );
            }
            let summary = blocks
                .iter()
                .find(|block| block.id.0 == "revision:summary")
                .unwrap();
            assert_eq!(
                summary.metadata.fold[0].end.block,
                forge_buffer::identity::BlockId("event-7:comment".into())
            );
            assert_eq!(indentation(summary, 0), "");
            for block in blocks {
                for row in 0..block.text.row_count() {
                    let rendered = format!(
                        "{}{}",
                        indentation(block, row),
                        block.text.row(row).unwrap()
                    );
                    assert!(
                        width.cells(&rendered, 0).unwrap() <= columns,
                        "nested content exceeded {columns} columns: {rendered}"
                    );
                }
            }
        }
    }

    #[test]
    fn native_turn_content_renders_in_exchange_order_without_segment_content() {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"native", "session_id":"session", "agent_id":"primary", "ordinal":1, "prompt":"Do the work",
            "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            }),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 0).unwrap();
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("Inspecting ownership".into());
        event.data = serde_json::json!({ "phase": "commentary" });
        exchange.observe_turn(&event, 1).unwrap();
        event.text = None;
        event.kind = "tool".into();
        event.activity = Some(ToolActivity {
            id: "call".into(),
            kind: ToolActivityKind::FileChange,
            title: "inspect".into(),
            output: Some("result".into()),
            output_delta: false,
            status: Some("completed".into()),
            change: crate::backend::ProviderChangeSet {
                file: vec![crate::backend::ProviderFileChange {
                    path: "owned.rs".into(),
                    move_path: None,
                    kind: crate::backend::ProviderChangeKind::Add,
                    diff: "@@ -0,0 +1 @@\n+owned\n".into(),
                }],
            },
        });
        exchange.observe_turn(&event, 2).unwrap();
        exchange
            .append_input(
                crate::exchange::InputIntent::Steering,
                "Also check tests".into(),
                3,
            )
            .unwrap();
        event.activity = None;
        event.kind = "assistant_message".into();
        event.text = Some("Finished the work".into());
        event.data = serde_json::json!({ "phase": "final_answer" });
        exchange.observe_turn(&event, 4).unwrap();
        exchange
            .node_list
            .push(crate::exchange::ExchangeNode::SkillRead {
                id: "late-skill".into(),
                turn_id: exchange.turn[0].id().to_owned(),
                read: crate::skill::SkillRead {
                    resource_id: "fixture".into(),
                    name: "fixture".into(),
                    path: "/skills/fixture/SKILL.md".into(),
                    tool_id: "read-skill".into(),
                },
            });
        event.text = None;
        event.kind = "tool".into();
        event.data = serde_json::Value::Null;
        event.activity = Some(ToolActivity {
            id: "interrupted-edit".into(),
            kind: ToolActivityKind::FileChange,
            title: "file changes".into(),
            output: None,
            output_delta: false,
            status: Some("interrupted".into()),
            change: Default::default(),
        });
        exchange.observe_turn(&event, 4).unwrap();
        let streaming = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange: exchange.clone(),
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            4,
        )
        .unwrap();
        let streaming_response = streaming
            .entry
            .block
            .iter()
            .find(|block| block.text.row(0) == Some("Finished the work"))
            .unwrap();
        let streaming_commentary = streaming
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .row(0)
                    .is_some_and(|text| text == "  Inspecting ownership")
            })
            .unwrap();
        assert_eq!(
            streaming_commentary
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .indent,
            4,
            "commentary must nest one level below the exchange summary"
        );
        assert!(
            streaming_commentary.metadata.gutter.is_empty(),
            "resolved content layout must not acquire another gutter"
        );
        assert_eq!(
            streaming_response.metadata.layout.as_ref().unwrap().indent,
            2,
            "streaming final response must not inherit activity indentation"
        );
        event.text = None;
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 5).unwrap();
        exchange
            .finish(crate::exchange::ExchangeState::Complete, 5)
            .unwrap();
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange: exchange.clone(),
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            5,
        )
        .unwrap();
        let restored: Exchange =
            serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
        let reloaded = project_at(
            &TimelineEntry::Exchange {
                id: restored.id.clone(),
                created_at_ms: 0,
                exchange: restored,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            5,
        )
        .unwrap();
        for projection in [&streaming, &projected, &reloaded] {
            let blocks = &projection.entry.block;
            let summary = blocks
                .iter()
                .find(|block| block.id.0 == "native:summary")
                .unwrap();
            let activity_end = &summary.metadata.fold[0].end.block;
            let end = blocks
                .iter()
                .position(|block| &block.id == activity_end)
                .unwrap();
            let response = blocks
                .iter()
                .position(|block| block.text.row(0) == Some("Finished the work"))
                .unwrap();
            for label in ["Read skill", "File changes interrupted", "Changed 1 file"] {
                let position = blocks
                    .iter()
                    .position(|block| block.text.wire_rows().join("\n").contains(label))
                    .unwrap();
                assert!(
                    position <= end && end < response,
                    "{label} escaped the activity fold"
                );
                assert_eq!(
                    blocks[position].metadata.layout.as_ref().unwrap().indent,
                    if label == "Read skill" { 4 } else { 6 },
                    "{label}"
                );
            }
        }
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).filter_map(|row| block.text.row(row)))
            .collect::<Vec<_>>()
            .join("\n");
        let completed_response = projected
            .entry
            .block
            .iter()
            .find(|block| block.text.row(0) == Some("Finished the work"))
            .unwrap();
        assert_eq!(
            streaming_response.metadata.layout, completed_response.metadata.layout,
            "completion must not change final response indentation"
        );
        let commentary = text.find("Inspecting ownership").unwrap();
        let changes = text.find("Changed 1 file").unwrap();
        let steering = text.find("Also check tests").unwrap();
        let response = text.find("Finished the work").unwrap();
        assert!(commentary < changes && changes < steering && steering < response);
        assert_eq!(text.matches("Finished the work").count(), 1);
        assert!(
            projected.tool.is_empty(),
            "file edits must not create generic tool output"
        );
        assert!(
            projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::Diff { text, .. } if text.contains("owned.rs"))),
            "file edit lost its derived diff"
        );
        let summary = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "native:summary")
            .unwrap();
        assert!(summary.metadata.fold[0].closed);
        let last = projected.entry.block.last().unwrap();
        assert_ne!(summary.metadata.fold[0].end.block, last.id);
        assert!(
            last.metadata.fold.is_empty(),
            "final response was folded with running activity"
        );
        let lifecycle = serde_json::from_value(serde_json::json!({
            "id":"cancel", "session_id":"session", "plan_id":"plan", "title":"Migration",
            "kind":"cancelled", "model_revision":1, "user_revision":0, "created_at_ms":6
        }))
        .unwrap();
        let anchor = crate::plan::ExchangeAnchor::capture(&exchange);
        crate::plan::event::insert_events(
            &mut exchange,
            vec![crate::plan::ExchangePlanEvent {
                id: "cancel".into(),
                node_count: anchor.node_count,
                content: crate::plan::PlanEventContent::Lifecycle {
                    title: "Migration".into(),
                    lifecycle,
                },
            }],
        );
        let rendered = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            6,
        )
        .unwrap();
        let text = rendered
            .entry
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.find("Finished the work").unwrap() < text.find("Plan rejected").unwrap(),
            "review actions after completion must not precede the final answer"
        );
    }

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
    fn implementation_status_keeps_file_findings_in_stable_nested_folds() {
        let mut execution = crate::session::state_machine::ExecutionStatus {
            id: "execution".into(),
            phase: crate::plan::PlanPhase::Implement,
            progress: crate::plan::execution::SemanticProgress {
                matched: vec!["Cargo.toml".into()],
                missing: vec!["src/ui.rs".into()],
                different: vec![
                    "src/player.rs: planned call relationships differ for move_player".into(),
                ],
                unverified: vec!["src/player.rs: unresolved reference".into()],
                ..Default::default()
            },
        };
        let render = |execution: crate::session::state_machine::ExecutionStatus| {
            project_at(
                &TimelineEntry::Status {
                    checks: None,
                    status_thoughts: true,
                    agent_count: 0,
                    id: "session:status".into(),
                    created_at_ms: 0,
                    status: SessionPhase::Working {
                        duration_ms: 0,
                        started_at_ms: Some(0),
                        activity: execution.phase.into(),
                        reasoning_summary: Some("Checking files".into()),
                        execution: Some(execution),
                    },
                },
                &WidthProfile::default(),
                133_000,
            )
            .unwrap()
        };
        let initial = render(execution.clone());
        assert_eq!(
            initial.entry.block[0].text.wire_rows(),
            vec![
                "",
                "Implementing · 133s · 2 files need attention",
                "Checking files"
            ]
        );
        assert!(
            initial.entry.block[0].metadata.target[0]
                .id
                .0
                .ends_with(":working")
        );
        assert!(initial.entry.block[1].metadata.fold[0].closed);
        let file_id = format!(
            "session:status:implementation:execution:{}",
            crate::plan::digest(b"src/player.rs")
        );
        let file = initial
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == file_id)
            .unwrap();
        assert!(file.metadata.fold[0].closed);
        assert!(
            initial.entry.block.iter().any(|block| block
                .text
                .wire_rows()
                .join(" ")
                .contains("move_player"))
        );
        let repeated = render(execution.clone());
        assert_eq!(
            initial.entry.block[1].metadata.fold,
            repeated.entry.block[1].metadata.fold
        );
        println!(
            "IMPLEMENTATION_STATUS_FIXTURE:{}",
            serde_json::to_string(&initial.entry.block).unwrap()
        );
        execution.phase = crate::plan::PlanPhase::Verify;
        execution.progress.missing.clear();
        execution.progress.different.clear();
        execution.progress.unverified.clear();
        execution
            .progress
            .matched
            .extend(["src/player.rs".into(), "src/ui.rs".into()]);
        let matched = render(execution);
        assert_eq!(
            matched.entry.block[0].text.wire_rows(),
            vec!["", "Verifying · 133s · all files matched", "Checking files"]
        );
        assert!(
            matched
                .entry
                .block
                .iter()
                .any(|block| block.id.0 == file_id)
        );
    }

    #[test]
    fn status_text_and_animation_are_committed_together() {
        use forge_buffer::block::StatusHint;
        let cases = [
            (SessionPhase::Idle, "Idle", false, None),
            (SessionPhase::Paused, "Paused", false, None),
            (
                SessionPhase::AwaitingApproval {
                    exchange_id: "exchange".into(),
                },
                "Waiting for your approval",
                false,
                Some(StatusHint::Permission),
            ),
            (
                SessionPhase::Compacting {
                    started_at_ms: 1_000,
                },
                "Compacting · 2s",
                true,
                None,
            ),
            (
                SessionPhase::Compacting {
                    started_at_ms: 4_000,
                },
                "Compacting · 0s",
                true,
                None,
            ),
            (
                SessionPhase::Finalizing {
                    exchange_id: "exchange".into(),
                    outcome: Some(crate::exchange::ExchangeState::Complete),
                    error: None,
                },
                "Saving exchange",
                true,
                None,
            ),
            (
                SessionPhase::Finalizing {
                    exchange_id: "exchange".into(),
                    outcome: Some(crate::exchange::ExchangeState::Complete),
                    error: Some("locked".into()),
                },
                "Saving exchange failed · locked",
                false,
                None,
            ),
            (
                SessionPhase::WaitingForAgent { agent_count: 2 },
                "Waiting for agents · 2 running",
                true,
                Some(StatusHint::Working),
            ),
            (
                SessionPhase::RetryingPlanGeneration {
                    plan_id: "plan".into(),
                    turn: 2,
                    max_turn: 20,
                    duration_ms: 0,
                    started_at_ms: Some(0),
                    activity: WorkflowActivity::Planning,
                },
                "Planning · 3s",
                true,
                Some(StatusHint::Working),
            ),
            (
                SessionPhase::RetryingPlanGeneration {
                    plan_id: "plan".into(),
                    turn: 3,
                    max_turn: 20,
                    duration_ms: 0,
                    started_at_ms: Some(0),
                    activity: WorkflowActivity::Revising,
                },
                "Revising plan · 3s",
                true,
                Some(StatusHint::Working),
            ),
            (
                SessionPhase::PlanningFailed {
                    plan_id: "plan".into(),
                    turn_count: 2,
                    reason: "no progress limit reached".into(),
                },
                "Planning stopped · no progress limit reached",
                false,
                None,
            ),
        ];
        for (phase, expected, animated, hint) in cases {
            let projection = project_at(
                &TimelineEntry::Status {
                    checks: None,
                    status_thoughts: true,
                    agent_count: 0,
                    id: "status".into(),
                    created_at_ms: 0,
                    status: phase,
                },
                &WidthProfile::default(),
                3000,
            )
            .unwrap();
            let block = &projection.entry.block[0];
            assert_eq!(block.text.wire_rows(), vec!["", expected]);
            assert_eq!(
                block.metadata.status,
                Some(forge_buffer::block::StatusPresentation {
                    row: 1,
                    animated,
                    hint
                })
            );
            block.validate().unwrap();
            let encoded = serde_json::to_vec(block).unwrap();
            let reopened: forge_buffer::block::BufferBlock =
                serde_json::from_slice(&encoded).unwrap();
            assert_eq!(&reopened, block);
        }
    }

    #[test]
    fn working_status_reports_elapsed_seconds_without_persisting_a_timer_value() {
        let projection = project_at(
            &TimelineEntry::Status {
                checks: None,
                status_thoughts: true,
                agent_count: 0,
                id: "session:status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    duration_ms: 0,
                    started_at_ms: Some(1_000),
                    activity: WorkflowActivity::Working,
                    execution: None,
                    reasoning_summary: None,
                },
            },
            &WidthProfile::default(),
            4_999,
        )
        .unwrap();

        assert_eq!(
            projection.entry.block[0].text.wire_rows(),
            vec!["", "Working · 3s"]
        );
        let status = &projection.entry.block[0].metadata;
        assert!(
            status.gutter.is_empty(),
            "status text must not be split into the sign column"
        );
        assert!(
            status.conceal.is_empty(),
            "status text must remain intact for inline hints"
        );
    }

    #[test]
    fn working_status_keeps_reasoning_below_the_heading_in_italics() {
        for activity in [
            WorkflowActivity::Working,
            WorkflowActivity::Planning,
            WorkflowActivity::Revising,
            WorkflowActivity::Implementing,
            WorkflowActivity::Checking,
            WorkflowActivity::Verifying,
            WorkflowActivity::Resolving,
            WorkflowActivity::Goal,
        ] {
            let projection = project_at(
                &TimelineEntry::Status { checks: None, status_thoughts: true, agent_count: 0,
                    id: "session:status".into(),
                    created_at_ms: 0,
                    status: SessionPhase::Working {
                        duration_ms: 0, started_at_ms: Some(1_000),
                        activity,
                        execution: None,
                        reasoning_summary: Some(
                            "**Considering virtual program implementation**\nI'm thinking about changes".into(),
                        ),
                    },
                },
                &WidthProfile::default(),
                4_999,
            )
            .unwrap();

            let block = &projection.entry.block[0];
            let rows = block.text.wire_rows();
            assert_eq!(rows[0], "");
            assert_eq!(rows[1], format!("{} · 3s", activity.active_label()));
            assert_eq!(
                rows[2..].join(" ").trim(),
                "Considering virtual program implementation I'm thinking about changes"
            );
            assert!(rows[2..].iter().all(|row| !row.starts_with(' ')));
            assert!(block.metadata.status.as_ref().unwrap().animated);
            assert_eq!(block.metadata.status.as_ref().unwrap().row, 1);
            assert!(
                block
                    .metadata
                    .decoration
                    .iter()
                    .any(|decoration| decoration.capture == "ForgeHarnessCommentary"
                        && decoration.range.start.row == 2
                        && decoration.range.start.column == 0
                        && decoration.range.end.row <= rows.len())
            );
        }
    }

    #[test]
    fn working_status_ellipsizes_reasoning_and_omits_empty_previews() {
        for (summary, columns, expected) in [
            ("", 32, None),
            (" \n\t ", 32, None),
            ("short\nthought", 32, Some("short thought")),
            ("**short**", 5, Some("short")),
            ("12345678", 8, Some("12345678")),
            ("123456789", 8, Some("12345...")),
            ("界界界界界", 8, Some("界界...")),
            (
                "e\u{301}e\u{301}e\u{301}e\u{301}e\u{301}e\u{301}e\u{301}e\u{301}e\u{301}",
                8,
                Some("e\u{301}e\u{301}e\u{301}e\u{301}e\u{301}..."),
            ),
            ("👩‍💻👩‍💻👩‍💻", 5, Some("👩‍💻...")),
            ("overflow", 1, Some(".")),
            ("overflow", 2, Some("..")),
        ] {
            let width = WidthProfile {
                columns,
                ..Default::default()
            };
            let projection = project_at(
                &TimelineEntry::Status {
                    checks: None,
                    id: "session:status".into(),
                    created_at_ms: 0,
                    status_thoughts: true,
                    agent_count: 0,
                    status: SessionPhase::Working {
                        duration_ms: 37_000,
                        started_at_ms: None,
                        activity: WorkflowActivity::Planning,
                        execution: None,
                        reasoning_summary: Some(summary.into()),
                    },
                },
                &width,
                0,
            )
            .unwrap();
            let block = &projection.entry.block[0];
            let rows = block.text.wire_rows();
            let preview_start = block
                .metadata
                .decoration
                .iter()
                .find(|decoration| decoration.capture == "ForgeHarnessCommentary")
                .map(|decoration| decoration.range.start.row);
            if let Some(expected) = expected {
                let start = preview_start.expect("missing thought preview");
                assert_eq!(rows.len(), start + 1, "{summary:?} at {columns} columns");
                assert_eq!(rows[start], expected);
                assert!(width.cells(rows[start], 0).unwrap() <= columns);
            } else {
                assert!(preview_start.is_none());
                assert_eq!(rows, vec!["", "Planning · 37s"]);
            }
        }
    }

    #[test]
    fn status_thoughts_can_be_hidden_without_hiding_the_status() {
        for status_thoughts in [false, true] {
            let projected = project_at(
                &TimelineEntry::Status {
                    checks: None,
                    id: "session:status".into(),
                    created_at_ms: 0,
                    agent_count: 0,
                    status_thoughts,
                    status: SessionPhase::Working {
                        duration_ms: 3_000,
                        started_at_ms: None,
                        activity: WorkflowActivity::Working,
                        execution: None,
                        reasoning_summary: Some("Inspecting the selected files".into()),
                    },
                },
                &WidthProfile::default(),
                0,
            )
            .unwrap();
            let block = &projected.entry.block[0];
            let rows = block.text.wire_rows();
            assert_eq!(rows[1], "Working · 3s");
            assert_eq!(
                rows.iter()
                    .any(|row| row.contains("Inspecting the selected files")),
                status_thoughts
            );
            assert_eq!(
                block
                    .metadata
                    .decoration
                    .iter()
                    .any(|decoration| decoration.capture == "ForgeHarnessCommentary"),
                status_thoughts
            );
            assert!(block.metadata.status.as_ref().unwrap().animated);
        }
    }

    #[test]
    fn awaiting_input_status_exposes_question_hint_target() {
        for owner in [
            crate::broker::ElicitationOwner::Plan,
            crate::broker::ElicitationOwner::Interaction,
            crate::broker::ElicitationOwner::PlanAcceptance,
        ] {
            let projected = project_at(
                &TimelineEntry::Status {
                    checks: None,
                    status_thoughts: true,
                    agent_count: 0,
                    id: "session:status".into(),
                    created_at_ms: 0,
                    status: SessionPhase::AwaitingInput {
                        owner,
                        plan_id: None,
                        exchange_id: None,
                    },
                },
                &WidthProfile::default(),
                0,
            )
            .unwrap();
            let block = &projected.entry.block[0];
            assert_eq!(block.text.wire_rows(), vec!["", "Waiting for your answer"]);
            assert_eq!(block.metadata.target[0].id.0, "session:status:question");
            assert_eq!(block.metadata.target[0].range.start.row, 1);
            assert!(block.metadata.fold.is_empty());
            assert!(projected.action.is_empty());
        }
    }

    #[test]
    fn review_status_has_a_separate_unindented_action_row() {
        let projected = project_at(
            &TimelineEntry::Status {
                checks: None,
                status_thoughts: true,
                agent_count: 0,
                id: "session:status".into(),
                created_at_ms: 0,
                status: SessionPhase::AwaitingPlanReview {
                    plan_id: "pending-plan".into(),
                    revision: 1,
                },
            },
            &WidthProfile::default(),
            0,
        )
        .unwrap();
        let block = &projected.entry.block[0];
        assert_eq!(
            block.text.wire_rows(),
            vec!["", "Waiting for plan review · revision 1"]
        );
        let target = &block.metadata.target[0];
        assert_eq!(target.range.start.row, 1);
        assert_eq!(target.range.start.column, 0);
        assert!(
            block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessPlan"
                    && decoration.range.start.row == 1)
        );
        assert!(matches!(projected.action.get(&target.id),
            Some(super::TranscriptAction::Plan { plan_id, .. }) if plan_id == "pending-plan"));
        assert!(block.metadata.fold.is_empty());
    }

    #[test]
    fn lifecycle_resumption_does_not_render_as_user_input() {
        let interaction: Exchange = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"", "lifecycle":"Plan implementation resumed · implement",
            "kind":"plan_execution", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: "interaction".into(),
                created_at_ms: 1_000,
                exchange: interaction,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1_000,
        )
        .unwrap();
        assert_eq!(projected.entry.block[0].id.0, "interaction:lifecycle");
        assert!(
            projected.entry.block[0]
                .text
                .wire_rows()
                .join("\n")
                .contains("Plan implementation resumed · implement")
        );
        assert!(
            !projected
                .entry
                .block
                .iter()
                .any(|block| block.id.0 == "interaction:prompt")
        );
    }

    #[test]
    fn only_noninitial_top_level_interactions_own_a_separator() {
        let interaction: Exchange = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "agent_id":"primary", "ordinal":1, "prompt":"Inspect parser",
            "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let first = project_at(
            &TimelineEntry::Exchange {
                id: "interaction".into(),
                created_at_ms: 1_000,
                exchange: interaction.clone(),
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1_000,
        )
        .unwrap();
        assert_eq!(first.entry.block.len(), 2);
        assert_eq!(first.entry.block[0].id.0, "interaction:prompt");

        let projection = project_at_with_separator(
            super::ProjectionSource::Entry(&TimelineEntry::Exchange {
                id: "interaction".into(),
                created_at_ms: 1_000,
                exchange: interaction,
                agent_by_id: HashMap::new(),
            }),
            &WidthProfile::default(),
            1_000,
            true,
            &HashMap::new(),
            &HashMap::new(),
        )
        .unwrap();

        assert_eq!(projection.entry.block.len(), 3);
        assert_eq!(projection.entry.block[0].id.0, "interaction:separator");
        assert_eq!(projection.entry.block[0].text.wire_rows(), vec![""]);
        assert!(projection.entry.block[0].metadata.fold.is_empty());
        assert_eq!(projection.entry.block[1].id.0, "interaction:prompt");
    }

    #[test]
    fn unsuccessful_exchange_summaries_preserve_outcome_and_frozen_duration() {
        for (state, label) in [
            (crate::exchange::ExchangeState::Interrupted, "Interrupted"),
            (crate::exchange::ExchangeState::Failed, "Failed"),
            (crate::exchange::ExchangeState::Cancelled, "Cancelled"),
        ] {
            for kind in ["chat", "plan_draft", "plan_execution"] {
                let mut exchange: Exchange = serde_json::from_value(json!({
                    "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1, "prompt":"Work",
                    "kind":kind, "state":"running", "created_at_ms":1000,
                    "attributed_matches_checkpoint":false, "node_list":[]
                }))
                .unwrap();
                exchange.resume(1000).unwrap();
                exchange.finish(state, 4000).unwrap();
                assert_eq!(
                    super::exchange_activity_summary(&exchange, 90_000),
                    format!("● {label} 3s")
                );
            }
        }
    }

    #[test]
    fn exchange_and_session_activity_share_waits_and_execution_time() -> anyhow::Result<()> {
        for (kind, phase) in [
            ("chat", None),
            ("plan_draft", None),
            ("plan_revision", None),
            ("plan_execution", Some("implement")),
            ("plan_execution", Some("verify")),
            ("plan_execution", Some("resolve")),
        ] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "kind":kind, "state":"running", "prompt":"Work", "created_at_ms":1000,
                "node_list":[], "attributed_matches_checkpoint":false,
                "execution_phase":phase.map(|phase|json!({"phase":phase})),
            }))?;
            let activity = WorkflowActivity::from_exchange(&exchange);
            let assert_working = |exchange: &Exchange, now_ms, seconds| -> anyhow::Result<()> {
                let status = SessionPhase::resolve(None, None, Some(exchange));
                assert!(matches!(status, SessionPhase::Working { .. }));
                let projection = project_at(
                    &TimelineEntry::Status {
                        checks: None,
                        id: "status".into(),
                        created_at_ms: 1000,
                        status,
                        status_thoughts: true,
                        agent_count: 0,
                    },
                    &WidthProfile::default(),
                    now_ms,
                )?;
                assert_eq!(
                    projection.entry.block[0].text.row(1),
                    Some(format!("{} · {seconds}s", activity.active_label()).as_str())
                );
                assert_eq!(
                    super::exchange_activity_summary(exchange, now_ms),
                    format!("● {} {seconds}s", activity.active_label())
                );
                Ok(())
            };
            exchange.resume(1000)?;
            assert_working(&exchange, 4000, 3)?;
            exchange.pause(4000);
            assert_working(&exchange, 90_000, 3)?;
            exchange.resume(4000)?;
            exchange.observe_blocker("approval:command".into(), true, 5000);
            assert!(matches!(
                SessionPhase::resolve(None, None, Some(&exchange)),
                SessionPhase::AwaitingApproval { .. }
            ));
            assert_eq!(
                super::exchange_activity_summary(&exchange, 90_000),
                "● Waiting for your approval 4s"
            );
            exchange.observe_blocker("approval:command".into(), false, 90_000);
            assert_working(&exchange, 92_000, 6)?;
            exchange.awaiting_input = true;
            exchange.pause(93_000);
            assert!(matches!(
                SessionPhase::resolve(None, None, Some(&exchange)),
                SessionPhase::AwaitingInput { .. }
            ));
            assert_eq!(
                super::exchange_activity_summary(&exchange, 100_000),
                "● Waiting for your answer 7s"
            );
            exchange.resume(100_000)?;
            exchange.start_turn(
                crate::backend::ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                100_000,
            )?;
            exchange.pause_for_restart(101_000)?;
            assert_eq!(
                SessionPhase::resolve(None, None, Some(&exchange)),
                SessionPhase::Paused
            );
            assert!(!SessionPhase::resolve(None, None, Some(&exchange)).animated());
            exchange.resume(200_000)?;
            assert_working(&exchange, 201_000, 9)?;
            if let Some(phase) = &mut exchange.execution_phase {
                phase.outcome = Some(crate::plan::execution::VerificationOutcome::Passed);
                assert_eq!(
                    SessionPhase::resolve(None, None, Some(&exchange)),
                    SessionPhase::Idle
                );
                assert!(
                    super::exchange_activity_summary(&exchange, 201_000)
                        .contains(activity.completed_label())
                );
            }
        }
        Ok(())
    }

    #[test]
    fn thought_activity_preserves_text_ownership_and_streamed_fold() -> anyhow::Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};

        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"thought-layout", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Run exact Rust tests", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 0)?;
        let text = "I’ll run the two Rust regression tests individually.\nThe next line retains its own source position.";
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some(text.into()),
            data: json!({"provider_message_id":"initial", "phase":"commentary"}),
            summary: None,
            task_update: None,
            activity: None,
        };
        exchange.observe_turn(&event, 1)?;
        let thought_id = exchange.node_list.last().unwrap().id().to_owned();
        for index in 0..3 {
            event.kind = "tool".into();
            event.text = None;
            event.data = serde_json::Value::Null;
            event.activity = Some(ToolActivity {
                id: format!("exact-{index}"),
                kind: ToolActivityKind::Command,
                title: format!("checks_run exact test {index}"),
                output: Some("retained test output".into()),
                output_delta: false,
                status: Some(if index == 2 { "running" } else { "completed" }.into()),
                change: Default::default(),
            });
            exchange.observe_turn(&event, index + 2)?;
        }
        for columns in [40, 124, 180] {
            let width = WidthProfile {
                columns,
                ..Default::default()
            };
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                &width,
                10,
            )?;
            let heading = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == thought_id)
                .unwrap();
            assert_eq!(
                heading
                    .text
                    .wire_rows()
                    .iter()
                    .map(|row| row.trim_start())
                    .collect::<Vec<_>>()
                    .join("\n"),
                text,
                "activity counts must not be appended to commentary source"
            );
            let group =
                projected
                    .entry
                    .block
                    .iter()
                    .find(|block| {
                        block.metadata.node.as_ref().is_some_and(|node| {
                            node.kind == forge_buffer::node::NodeKind::ToolGroup
                        })
                    })
                    .unwrap();
            assert_eq!(
                group
                    .metadata
                    .node
                    .as_ref()
                    .unwrap()
                    .parent
                    .as_ref()
                    .unwrap()
                    .0,
                thought_id
            );
            assert!(
                group.metadata.layout.as_ref().unwrap().indent
                    > heading.metadata.layout.as_ref().unwrap().indent
            );
            let message = exchange.turn[0]
                .messages()
                .iter()
                .find(|message| message.kind() == crate::turn::MessageKind::Assistant)
                .unwrap();
            let streamed = super::message_block(heading, message, &width)?;
            let replacement = &streamed.entry.block[0];
            assert_eq!(
                replacement.metadata.fold, heading.metadata.fold,
                "a streamed thought update must retain its activity fold endpoints"
            );
            assert_eq!(replacement.metadata.node, heading.metadata.node);
        }
        Ok(())
    }

    #[test]
    fn interaction_results_render_created_then_deleted_files_without_preview_errors()
    -> anyhow::Result<()> {
        use crate::backend::{
            BackendEvent, ProviderAddress, ProviderChangeKind, ProviderChangeSet,
            ProviderFileChange, ToolActivity, ToolActivityKind,
        };
        for persistent in [false, true] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"transient", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Exercise changes", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            exchange.resume(0)?;
            let address = ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            };
            exchange.start_turn(address.clone(), 0)?;
            for (index, (kind, diff)) in [
                (ProviderChangeKind::Add, "@@ -0,0 +1 @@\n+first"),
                (ProviderChangeKind::Update, "@@ -1 +1 @@\n-first\n+second"),
                (ProviderChangeKind::Update, "@@ -1 +1 @@\n-second\n+third"),
                (ProviderChangeKind::Delete, "@@ -1 +0,0 @@\n-third"),
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = BackendEvent {
                    received_at_ms: None,
                    address: Some(address.clone()),
                    turn_boundary: None,
                    kind: "assistant_message".into(),
                    text: Some(format!("Step {index}")),
                    data: json!({"message_id":format!("step-{index}"), "phase":"commentary"}),
                    activity: None,
                    summary: None,
                    task_update: None,
                };
                exchange.observe_turn(&event, index as i64 * 10 + 1)?;
                let mut file = vec![ProviderFileChange {
                    path: "temporary.lua".into(),
                    move_path: None,
                    kind,
                    diff: diff.into(),
                }];
                if persistent && index == 3 {
                    file.push(ProviderFileChange {
                        path: "existing.lua".into(),
                        move_path: None,
                        kind: ProviderChangeKind::Update,
                        diff: "@@ -1 +1 @@\n-before\n+after".into(),
                    });
                }
                event.kind = "tool".into();
                event.text = None;
                event.data = serde_json::Value::Null;
                event.activity = Some(ToolActivity {
                    id: format!("write-{index}"),
                    kind: ToolActivityKind::FileChange,
                    title: "file changes".into(),
                    output: None,
                    output_delta: false,
                    status: Some("completed".into()),
                    change: ProviderChangeSet { file },
                });
                exchange.observe_turn(&event, index as i64 * 10 + 2)?;
            }
            for complete in [false, true] {
                if complete {
                    exchange
                        .turn
                        .last_mut()
                        .unwrap()
                        .finish(crate::turn::TurnOutcome::Completed, 49)?;
                    exchange.finish(crate::exchange::ExchangeState::Complete, 50)?;
                }
                let restored = serde_json::from_value(serde_json::to_value(&exchange)?)?;
                let projected = project_at(
                    &TimelineEntry::Exchange {
                        id: "transient".into(),
                        created_at_ms: 0,
                        exchange: restored,
                        agent_by_id: HashMap::new(),
                    },
                    &WidthProfile::default(),
                    60,
                )?;
                let blocks = &projected.entry.block;
                assert!(!blocks.iter().any(|block| {
                    block
                        .text
                        .wire_rows()
                        .join("")
                        .contains("preview unavailable")
                }));
                let summary = blocks
                    .iter()
                    .find(|block| block.id.0 == "transient:results:changes")
                    .unwrap();
                assert!(summary.text.wire_rows().join("").contains(if persistent {
                    "Changed 1 file"
                } else {
                    "No net file changes"
                }));
                assert_eq!(
                    blocks
                        .iter()
                        .filter(|block| {
                            block.id.0.ends_with(":changes")
                                && block.id.0 != "transient:results:changes"
                        })
                        .count(),
                    4
                );
            }
        }
        Ok(())
    }

    #[test]
    fn interaction_results_combine_checks_and_files_above_their_thought_details()
    -> anyhow::Result<()> {
        use crate::backend::{
            BackendEvent, ProviderAddress, ProviderChangeKind, ProviderChangeSet,
            ProviderFileChange, ToolActivity, ToolActivityKind,
        };
        use crate::exchange::ExchangeNode;
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"combined", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Implement and check", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 0)?;
        let mut thought_ids = Vec::new();
        for index in 0..2 {
            let mut event = BackendEvent {
                received_at_ms: None,
                address: Some(address.clone()),
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some(format!("Thought {index}")),
                data: json!({"message_id":format!("thought-{index}"), "phase":"commentary"}),
                activity: None,
                summary: None,
                task_update: None,
            };
            exchange.observe_turn(&event, index * 10 + 1)?;
            thought_ids.push(exchange.node_list.last().unwrap().id().to_owned());
            event.kind = "tool".into();
            event.text = None;
            event.data = serde_json::Value::Null;
            event.activity = Some(ToolActivity {
                id: format!("write-{index}"),
                kind: ToolActivityKind::FileChange,
                title: "file changes".into(),
                output: None,
                output_delta: false,
                status: Some("completed".into()),
                change: ProviderChangeSet {
                    file: vec![ProviderFileChange {
                        path: "result.txt".into(),
                        move_path: None,
                        kind: if index == 0 {
                            ProviderChangeKind::Add
                        } else {
                            ProviderChangeKind::Update
                        },
                        diff: if index == 0 {
                            "@@ -0,0 +1 @@\n+first"
                        } else {
                            "@@ -1 +1 @@\n-first\n+second"
                        }
                        .into(),
                    }],
                },
            });
            exchange.observe_turn(&event, index * 10 + 2)?;
            let run: crate::checks::CheckRun = serde_json::from_value(json!({
                "id":format!("run-{index}"), "owner":{"kind":"exchange","session_id":"session",
                    "exchange_id":"combined","node_count":exchange.node_list.len()},
                "revision":1,"completed_plan":null,"shell":"system","executable":null,
                "workspace":".","environment_digest":"environment", "started_at_ms":index * 10 + 3,
                "completed_at_ms":index * 10 + 5,"state":if index == 0 { "failed" } else { "passed" },
                "progress":crate::plan::execution::SemanticProgress::default(),
                "workspace_identity":null,"workspace_digest":{},"commands":[{
                    "id":format!("run-{index}:command"),"command":"check result","state":if index == 0 { "failed" } else { "passed" },
                    "started_at_ms":index * 10 + 3,"completed_at_ms":index * 10 + 5,"exit_code":if index == 0 {1} else {0},
                    "error":null,"stdout":"","stderr":"","output":"","output_bytes":0,"preview":""
                }]
            }))?;
            exchange.node_list.push(ExchangeNode::TimelineEvent {
                event: Box::new(crate::timeline::TimelineEvent::Check { run: Box::new(run) }),
            });
            let restored: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: "combined".into(),
                    created_at_ms: 0,
                    exchange: restored,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                30,
            )?;
            let blocks = &projected.entry.block;
            let ids = blocks
                .iter()
                .map(|block| &block.id)
                .collect::<std::collections::HashSet<_>>();
            assert_eq!(
                ids.len(),
                blocks.len(),
                "aggregate and detail blocks reused identities"
            );
            for identity in ["combined:results:checks", "combined:results:changes"] {
                let heading = blocks.iter().find(|block| block.id.0 == identity).unwrap();
                assert!(
                    heading.metadata.node.as_ref().unwrap().parent.is_none(),
                    "interaction summary is hidden inside Thought"
                );
                assert_eq!(heading.metadata.layout.as_ref().unwrap().indent, 2);
            }
            let summary = blocks
                .iter()
                .find(|block| block.id.0 == "combined:results:checks")
                .unwrap();
            assert!(summary.text.wire_rows().join("").contains(if index == 0 {
                "0 passed · 1 failed"
            } else {
                "1 passed · 1 failed"
            }));
            for (thought_index, thought_id) in thought_ids.iter().enumerate() {
                let run = blocks
                    .iter()
                    .find(|block| block.id.0 == format!("run-{thought_index}:checks"))
                    .unwrap();
                assert_eq!(
                    run.metadata
                        .node
                        .as_ref()
                        .unwrap()
                        .parent
                        .as_ref()
                        .unwrap()
                        .0,
                    *thought_id,
                    "individual check run lost its subthought owner"
                );
            }
            let changes = blocks
                .iter()
                .find(|block| block.id.0 == "combined:results:changes")
                .unwrap();
            assert!(changes.text.wire_rows().join("").contains("Changed 1 file"));
        }
        Ok(())
    }

    #[test]
    fn owned_check_results_keep_exchange_ownership_before_the_final_response() -> anyhow::Result<()>
    {
        use crate::backend::ProviderAddress;
        use crate::turn::{MessageDelivery, MessageKind, Turn, TurnOutcome};

        let mut turn = Turn::new(
            "turn".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            0,
        );
        turn.record_provider_text(
            MessageKind::Assistant,
            "Work completed",
            None,
            MessageDelivery::Final,
            false,
        )?;
        turn.finish(TurnOutcome::Completed, 20)?;
        let exchange: Exchange = serde_json::from_value(json!({
            "id":"work", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Check the change", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":true, "turn":[turn],
            "node_list":[{"kind":"turn_content", "id":"final", "turn_id":"turn",
                "item":{"kind":"message", "id":"turn:message:1"}}]
        }))?;
        let run: crate::checks::CheckRun = serde_json::from_value(json!({
            "id":"checks", "owner":{"kind":"exchange","session_id":"session","exchange_id":"work","node_count":0}, "revision":1, "completed_plan":null,
            "shell":"system", "executable":null, "workspace":".", "environment_digest":"environment",
            "started_at_ms":5, "completed_at_ms":10, "state":"passed",
            "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null, "workspace_digest":{}, "commands":[]
        }))?;
        for completed in [false, true] {
            let mut exchange = exchange.clone();
            if completed {
                exchange.finish(crate::exchange::ExchangeState::Complete, 20)?;
            }
            let entry = TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            };
            let entry =
                crate::timeline::TimelineProjector::with_checks(vec![entry], vec![run.clone()])?
                    .remove(0);
            let entry = serde_json::from_str(&serde_json::to_string(&entry)?)?;
            let projected = project_at(&entry, &WidthProfile::default(), 20)?;
            let blocks = &projected.entry.block;
            let checks = blocks
                .iter()
                .position(|block| block.id.0 == "checks:checks")
                .unwrap();
            let response = blocks
                .iter()
                .position(|block| block.id.0 == "final")
                .unwrap();
            assert!(
                checks < response,
                "check results followed the final response"
            );
            assert_eq!(
                blocks[checks]
                    .metadata
                    .node
                    .as_ref()
                    .unwrap()
                    .parent
                    .as_ref(),
                Some(&forge_buffer::identity::FoldId("work:exchange".into())),
                "check results must retain their exchange owner without an assistant thought"
            );
            assert_eq!(blocks[checks].metadata.layout.as_ref().unwrap().indent, 4);
        }
        Ok(())
    }

    #[test]
    fn running_checks_are_status_activity_until_result_published() -> anyhow::Result<()> {
        let commands = ["passed", "running", "pending", "skipped", "failed", "configuration_error"]
            .into_iter().map(|state| json!({
                "id": state, "command": format!("cargo test {state}"), "state": state, "selected": true,
                "started_at_ms": if matches!(state, "pending" | "configuration_error") { None } else { Some(0) }, "completed_at_ms": null, "exit_code": null,
                "error": null, "stdout": "absent", "stderr": "absent",
                "output": "absent", "output_bytes": 100,
                "preview": "older output\n\u{1b}[32mCompiling forge-harness\u{1b}[0m\r\n\n"
            })).collect::<Vec<_>>();
        let mut run: crate::checks::CheckRun = serde_json::from_value(json!({
            "id": "check-run", "revision": 1, "completed_plan": null,
            "shell": "system", "executable": null, "workspace": ".",
            "owner":{"kind":"session","session_id":"session"}, "environment_digest": "environment", "started_at_ms": 0,
            "completed_at_ms": null, "state": "running",
            "progress": crate::plan::execution::SemanticProgress::default(),
            "workspace_identity": null, "workspace_digest": {}, "commands": commands,
            "lifecycle": [{"id": "old-event", "created_at_ms": 0,
                "kind": "check_skipped", "check_id": "skipped", "text": "Previous check skipped"}]
        }))?;
        let history =
            crate::timeline::TimelineProjector::with_checks(Vec::new(), vec![run.clone()])?;
        assert!(
            history.is_empty(),
            "live checks entered durable timeline history"
        );
        let status = TimelineEntry::Status {
            id: "status".into(),
            created_at_ms: 0,
            agent_count: 0,
            status_thoughts: true,
            status: SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: Some(0),
                activity: WorkflowActivity::Working,
                reasoning_summary: Some("Independent provider work".into()),
                execution: None,
            },
            checks: crate::checks::CheckActivity::from_run(&run),
        };
        let projected = project_at(&status, &WidthProfile::default(), 1000)?;
        let rows = projected.entry.block[0].text.wire_rows();
        assert_eq!(rows[1], "Working · 1s");
        assert!(rows[2].starts_with("Checks running"));
        assert!(rows.iter().any(|row| row.contains("cargo test running")));
        assert!(
            rows.iter()
                .any(|row| row.contains("Independent provider work"))
        );
        assert!(rows.iter().any(|row| *row == "    Compiling forge-harness"));
        assert!(!rows.iter().any(|row| row.contains("cargo test pending")
            || row.contains("older output")
            || row.contains("Previous check")
            || row.contains('\u{1b}')));
        assert!(projected.tool.is_empty());
        run.state = crate::checks::CheckState::Failed;
        let history = crate::timeline::TimelineProjector::with_checks(Vec::new(), vec![run])?;
        assert_eq!(history.len(), 1);
        let finished = project_at(&history[0], &WidthProfile::default(), 2000)?;
        assert_eq!(finished.tool.len(), 4);
        for id in ["passed", "running", "failed", "configuration_error"] {
            assert!(finished.tool.contains_key(id));
        }
        for id in ["pending", "skipped"] {
            assert!(!finished.tool.contains_key(id));
            assert!(
                finished
                    .entry
                    .block
                    .iter()
                    .all(|block| !block.id.0.starts_with(id))
            );
        }
        assert!(
            !finished
                .entry
                .block
                .iter()
                .any(|block| block.id.0 == "old-event")
        );
        Ok(())
    }

    #[test]
    fn check_activity_clocks_do_not_read_output_or_tick_finished_history() -> anyhow::Result<()> {
        let mut run: crate::checks::CheckRun = serde_json::from_value(json!({
            "id":"check-run","revision":1,"completed_plan":null,"shell":"system","executable":null,
            "workspace":".","owner":{"kind":"session","session_id":"session"}, "environment_digest":"environment","started_at_ms":0,"completed_at_ms":null,
            "state":"running","progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},"commands":[{
                "id":"check-command","command":"cargo test --locked","state":"running","started_at_ms":0,
                "completed_at_ms":null,"exit_code":null,"error":null,"stdout":"absent","stderr":"absent",
                "output":"absent","output_bytes":100,"preview":"unread output"
            }]
        }))?;
        let entry = TimelineEntry::Status {
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::Idle,
            status_thoughts: false,
            agent_count: 0,
            checks: crate::checks::CheckActivity::from_run(&run),
        };
        let profile = WidthProfile::default();
        let projected = project_at(&entry, &profile, 1000)?;
        assert_eq!(projected.entry.block[0].text.row(1), Some("Checking"));
        assert!(
            projected.entry.block[0]
                .metadata
                .status
                .as_ref()
                .unwrap()
                .animated
        );
        assert!(
            projected.entry.block[0]
                .text
                .wire_rows()
                .iter()
                .any(|row| row.starts_with("Checks running · 0/0 finished · "))
        );
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("check-clock".into()),
            projected.entry.block,
        )?;
        let refreshed = super::timing_blocks(
            &entry,
            &profile,
            &document,
            &Default::default(),
            &projected.tool,
        )?;
        assert_eq!(refreshed.len(), 1);
        assert_eq!(refreshed[0].id.0, "status");
        assert_ne!(
            refreshed[0].text,
            document.block(&refreshed[0].id).unwrap().text
        );
        assert!(projected.tool.is_empty());
        run.commands[0].state = crate::checks::CheckState::Passed;
        run.commands[0].completed_at_ms = Some(2000);
        let mut previous = run.commands[0].clone();
        previous.id = "previous-command".into();
        run.commands.insert(0, previous);
        run.commands[1].state = crate::checks::CheckState::Interrupted;
        assert_eq!(
            crate::checks::CheckActivity::from_run(&run)
                .unwrap()
                .elapsed_ms(90_000),
            2000
        );
        run.state = crate::checks::CheckState::Interrupted;
        assert!(crate::checks::CheckActivity::from_run(&run).is_none());
        run.state = crate::checks::CheckState::Running;
        run.commands[1].state = crate::checks::CheckState::Running;
        run.commands[1].started_at_ms = Some(90_000);
        run.commands[1].completed_at_ms = None;
        assert_eq!(
            crate::checks::CheckActivity::from_run(&run)
                .unwrap()
                .elapsed_ms(93_000),
            5000
        );
        run.state = crate::checks::CheckState::Passed;
        let finished = TimelineEntry::Checks {
            id: run.id.clone(),
            created_at_ms: 0,
            run,
        };
        assert!(
            super::timing_blocks(
                &finished,
                &profile,
                &document,
                &Default::default(),
                &projected.tool
            )?
            .is_empty()
        );
        Ok(())
    }

    #[test]
    fn empty_or_stale_comparison_does_not_claim_live_work() -> anyhow::Result<()> {
        for phase in [
            crate::plan::PlanPhase::Implement,
            crate::plan::PlanPhase::Verify,
        ] {
            let projection = project_at(
                &TimelineEntry::Status {
                    checks: None,
                    id: "status".into(),
                    created_at_ms: 1000,
                    status_thoughts: true,
                    agent_count: 0,
                    status: SessionPhase::Working {
                        duration_ms: 3000,
                        started_at_ms: None,
                        activity: WorkflowActivity::Implementing,
                        reasoning_summary: None,
                        execution: Some(crate::session::state_machine::ExecutionStatus {
                            id: "execution".into(),
                            phase,
                            progress: crate::plan::execution::SemanticProgress {
                                matched: if phase == crate::plan::PlanPhase::Verify {
                                    vec!["file.rs".into()]
                                } else {
                                    vec![]
                                },
                                ..Default::default()
                            },
                        }),
                    },
                },
                &WidthProfile::default(),
                90_000,
            )?;
            assert_eq!(projection.entry.block.len(), 1);
            assert_eq!(
                projection.entry.block[0].text.row(1),
                Some("Implementing · 3s")
            );
        }
        Ok(())
    }

    #[test]
    fn paused_exchange_activity_summary_freezes_until_execution_resumes() {
        for (kind, paused, running) in [
            ("chat", "Paused", "Working"),
            ("plan_draft", "Planning paused", "Planning"),
            ("plan_revision", "Revising plan paused", "Revising plan"),
            ("plan_execution", "Implementing paused", "Implementing"),
        ] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"exchange", "session_id":"session", "agent_id":"primary",
                "ordinal":1, "prompt":"Work", "kind":kind, "state":"running",
                "created_at_ms":1000, "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            exchange.resume(1000).unwrap();
            exchange
                .start_turn(
                    crate::backend::ProviderAddress {
                        owner: None,
                        thread_id: "thread".into(),
                        turn_id: "turn".into(),
                    },
                    1000,
                )
                .unwrap();
            exchange.pause_for_restart(4000).unwrap();
            for now in [4000, 90_000] {
                assert_eq!(
                    super::exchange_activity_summary(&exchange, now),
                    format!("● {paused} 3s")
                );
            }
            exchange.resume(90_000).unwrap();
            assert_eq!(
                super::exchange_activity_summary(&exchange, 92_000),
                format!("● {running} 5s")
            );
        }
    }

    #[test]
    fn restored_history_is_visible_without_replacing_execution_outcomes() {
        use crate::exchange::{ExchangeState, HistoryDisposition};
        for (disposition, marker) in [
            (HistoryDisposition::RolledBack, "Rolled back"),
            (HistoryDisposition::Superseded, "Superseded"),
        ] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"history", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Work", "kind":"chat", "state":"running", "created_at_ms":1000,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            exchange.resume(1000).unwrap();
            exchange.finish(ExchangeState::Failed, 4000).unwrap();
            exchange.set_disposition(disposition).unwrap();
            let entry = TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 1000,
                exchange: exchange.clone(),
                agent_by_id: HashMap::new(),
            };
            let projected = project_at(&entry, &WidthProfile::default(), 90_000).unwrap();
            let summary = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == "history:summary")
                .unwrap();
            assert_eq!(
                summary
                    .text
                    .wire_rows()
                    .join("")
                    .split_whitespace()
                    .collect::<Vec<_>>()
                    .join(" "),
                format!("● {marker} · Failed 3s")
            );
            assert_eq!(exchange.state, ExchangeState::Failed);
            assert_eq!(exchange.elapsed(90_000), 3000);
        }
    }

    #[test]
    fn compact_header_reports_successful_tools_and_inclusive_output_tokens() {
        let mut calls = serde_json::Map::new();
        let order = (1..=20).map(|index| index.to_string()).collect::<Vec<_>>();
        for id in &order {
            calls.insert(
                id.clone(),
                json!({"id":id,"kind":"tool_call","title":"inspect",
                "output":"", "status":if id == "20" { "failed" } else { "completed" },
                "failed":id == "20", "started_at_ms":0,"completed_at_ms":200}),
            );
        }
        let mut value = json!({
            "id":"header","session_id":"session","agent_id":"primary","ordinal":1,
            "kind":"plan_draft","state":"complete","prompt":"Plan","created_at_ms":0,
            "completed_at_ms":44000,"duration_ms":44000,"attributed_matches_checkpoint":true,"node_list":[],
            "metrics":{"timing_complete":true,"tool_duration_ms":4000,
                "reported_output_tokens":7906,"reported_response_ms":40132},
            "turn":[{"id":"turn","provider":{"thread_id":"thread","turn_id":"turn"},
                "state":{"kind":"finished","outcome":"completed"},"started_at_ms":0,"completed_at_ms":44000,
                "usage":{"input":855100,"output":7906,"reasoning":906},
                "tool":{"order":order,"item":calls},"message":[],"item":[],"current_message":null}]
        });
        let exchange: Exchange = serde_json::from_value(value.clone()).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&exchange, 44000),
            "● Planned 44s │ 19/20 tools · 4 s │ 855.1k -> 7.9k · 197 tps"
        );
        for status in ["running", "cancelled", "interrupted"] {
            value["turn"][0]["tool"]["item"]["1"]["status"] = json!(status);
            let exchange: Exchange = serde_json::from_value(value.clone()).unwrap();
            assert!(super::exchange_activity_summary(&exchange, 44000).contains("18/20 tools"));
        }
        value["kind"] = json!("chat");
        value["duration_ms"] = json!(31000);
        value["metrics"] = json!({"timing_complete":true,"tool_duration_ms":29,
            "reported_output_tokens":846,"reported_response_ms":9097});
        value["turn"][0]["usage"] = json!({"input":77000,"cached_input":49280,"output":846});
        value["turn"][0]["tool"] = json!({"order":["first","second"],"item":{
            "first":{"id":"first","kind":"tool_call","title":"inspect","status":"completed","output":"",
                "failed":false,"started_at_ms":0,"completed_at_ms":29},
            "second":{"id":"second","kind":"tool_call","title":"inspect","status":"completed","output":"",
                "failed":false,"started_at_ms":0,"completed_at_ms":29}
        }});
        let exchange: Exchange = serde_json::from_value(value).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&exchange, 44000),
            "● Thought 31s │ 2/2 tools · 29 ms │ 77.0k (64%) -> 846 · 93 tps"
        );
        let child = crate::agent::Agent::pending("session", "Child", "Inspect", 0);
        assert!(
            super::agent_summary(&child, Some(&exchange), 44000)
                .ends_with("│ 2/2 tools · 29 ms │ 77.0k (64%) -> 846 · 93 tps")
        );
    }

    #[test]
    fn compact_summary_counts_native_usage_and_total_time_including_tools() {
        use crate::backend::usage::{TokenUsage, UsageUpdate};
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(1000).unwrap();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 1000).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: json!({}),
            activity: Some(ToolActivity {
                id: "tool".into(),
                kind: ToolActivityKind::ToolCall,
                title: "cargo test".into(),
                output: None,
                status: Some("running".into()),
                change: Default::default(),
                output_delta: false,
            }),
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 2000).unwrap();
        for (now_ms, duration, aggregate) in [
            (2439, "0.4s", "439 ms"),
            (2500, "0.5s", "500 ms"),
            (3000, "1s", "1 s"),
            (4500, "2.5s", "2.5 s"),
        ] {
            let entry = crate::timeline::TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 1000,
                exchange: exchange.clone(),
                agent_by_id: HashMap::new(),
            };
            let rendered = project_at(&entry, &WidthProfile::default(), now_ms).unwrap();
            assert!(rendered.entry.block.iter().any(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains(&format!("• {duration:>5} cargo test")))
            }));
            assert!(
                super::exchange_activity_summary(&exchange, now_ms)
                    .contains(&format!("0/1 tools · {aggregate}"))
            );
        }
        let mut permission_wait = exchange.clone();
        permission_wait.kind = crate::exchange::ExchangeKind::PlanDraft;
        permission_wait.observe_blocker("approval:command".into(), true, 4500);
        for now_ms in [90_000, 900_000] {
            assert_eq!(
                super::exchange_activity_summary(&permission_wait, now_ms),
                "● Waiting for your approval 3s │ 0/1 tools · 2.5 s"
            );
        }
        permission_wait.observe_blocker("approval:command".into(), false, 900_000);
        assert_eq!(
            super::exchange_activity_summary(&permission_wait, 902_000),
            "● Planning 5s │ 0/1 tools · 4.5 s"
        );
        event.activity.as_mut().unwrap().status = Some("failed".into());
        exchange.observe_turn(&event, 5000).unwrap();
        let entry = crate::timeline::TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 1000,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let rendered = project_at(&entry, &WidthProfile::default(), 90_000).unwrap();
        assert!(rendered.entry.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("•    3s cargo test"))
        }));
        event.kind = "turn_completed".into();
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        exchange.observe_turn(&event, 7000).unwrap();
        exchange
            .finish(crate::exchange::ExchangeState::Complete, 7000)
            .unwrap();
        event.kind = "usage".into();
        event.turn_boundary = None;
        event.data = serde_json::to_value(UsageUpdate {
            id: "call".into(),
            usage: TokenUsage {
                input: Some(76000),
                cached_input: Some(68400),
                reasoning: Some(3000),
                output: Some(4200),
            },
            cumulative: None,
            cumulative_total: None,
        })
        .unwrap();
        exchange.observe_turn(&event, 8000).unwrap();
        exchange.observe_turn(&event, 9000).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&exchange, 90_000),
            "● Thought 6s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1400 tps"
        );
        event.address.as_mut().unwrap().thread_id = "unowned-child".into();
        assert!(!exchange.observe_turn(&event, 90_000).unwrap());
        assert_eq!(exchange.usage().input, Some(76000));
        for duration_ms in [6500, 3500, 3000] {
            exchange.duration_ms = duration_ms;
            assert_eq!(
                super::exchange_activity_summary(&exchange, 90_000),
                format!(
                    "● Thought {}s │ 0/1 tools · 3 s │ 76.0k (90%) -> 4.2k · 1400 tps",
                    duration_ms / 1000
                )
            );
        }
        exchange.metrics.timing_complete = false;
        assert_eq!(
            super::exchange_activity_summary(&exchange, 90_000),
            "● Thought 3s │ 0/1 tools │ 76.0k (90%) -> 4.2k"
        );
    }

    #[test]
    fn active_usage_updates_header_without_waiting_for_exchange_completion() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"live", "session_id":"session", "agent_id":"primary", "ordinal":1, "prompt":"Work",
            "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(1000).unwrap();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 1000).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "usage".into(),
            text: None,
            data: json!({"id":"first", "usage":{"input":1000,"cached_input":900,"reasoning":40,"output":100},
                "cumulative":null,"cumulative_total":null}),
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 3000).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&exchange, 4000),
            "● Working 3s │ 1.0k (90%) -> 100 · 50 tps"
        );
        exchange.observe_turn(&event, 6000).unwrap();
        assert!(super::exchange_activity_summary(&exchange, 6000).contains("50 tps"));
        event.data["id"] = json!("second");
        exchange.observe_turn(&event, 6000).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&exchange, 9000),
            "● Working 8s │ 2.0k (90%) -> 200 · 40 tps"
        );
    }

    #[test]
    fn summary_omits_missing_usage_and_preserves_reported_zeroes() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut original: Exchange = serde_json::from_value(json!({
            "id":"partial", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        original.resume(1000).unwrap();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        original.start_turn(address.clone(), 1000).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&original, 4000),
            "● Working 3s"
        );
        for (usage, expected) in [
            (json!({}), "● Working 3s"),
            (
                json!({"input":1000,"output":100}),
                "● Working 3s │ 1.0k -> 100 · 50 tps",
            ),
            (
                json!({"input":1000,"cached_input":876}),
                "● Working 3s │ in 1.0k (88%)",
            ),
            (json!({"input":0,"cached_input":0}), "● Working 3s │ in 0"),
            (
                json!({"input":1000,"cached_input":1001}),
                "● Working 3s │ in 1.0k",
            ),
            (json!({"reasoning":23}), "● Working 3s"),
            (
                json!({"input":1000,"cached_input":0,"reasoning":0,"output":0}),
                "● Working 3s │ 1.0k (0%) -> 0",
            ),
        ] {
            let mut exchange = original.clone();
            let event = BackendEvent {
                received_at_ms: None,
                address: Some(address.clone()),
                turn_boundary: None,
                kind: "usage".into(),
                text: None,
                data: json!({"id":"first", "usage":usage, "cumulative":null,"cumulative_total":null}),
                activity: None,
                summary: None,
                task_update: None,
            };
            exchange.observe_turn(&event, 3000).unwrap();
            assert_eq!(super::exchange_activity_summary(&exchange, 4000), expected);
        }
    }

    #[test]
    fn running_exchange_activity_summary_tracks_duration_and_completion_metadata() {
        use crate::exchange::{Exchange, ExchangeKind};
        let mut interaction: Exchange = serde_json::from_value(json!({
            "id":"interaction", "session_id":"session", "agent_id":"primary", "ordinal":1, "prompt":"Plan parser",
            "kind":"plan_draft", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false,"node_list":[]
        }))
        .unwrap();
        interaction.resume(1000).unwrap();
        assert_eq!(
            super::exchange_activity_summary(&interaction, 4200),
            "● Planning 3s"
        );
        interaction.pause(5500);
        interaction
            .start_turn(
                crate::backend::ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                1000,
            )
            .unwrap();
        interaction
            .turn
            .last_mut()
            .unwrap()
            .record_usage(crate::backend::usage::TokenUsage {
                input: Some(1000),
                cached_input: Some(900),
                reasoning: Some(200),
                output: Some(280),
            });
        interaction
            .turn
            .last_mut()
            .unwrap()
            .finish(crate::turn::TurnOutcome::Completed, 5500)
            .unwrap();
        interaction.awaiting_input = true;
        assert_eq!(
            super::exchange_activity_summary(&interaction, 20000),
            "● Waiting for your answer 4s │ 1.0k (90%) -> 280"
        );
        interaction.kind = ExchangeKind::Chat;
        interaction
            .finish(crate::exchange::ExchangeState::Complete, 5500)
            .unwrap();
        assert_eq!(
            super::exchange_activity_summary(&interaction, 20000),
            "● Thought 4s │ 1.0k (90%) -> 280"
        );
    }
    #[test]
    fn implementation_report_has_closed_file_sections_and_visible_missing_evidence() {
        use crate::plan::implementation_report::{
            ImplementationFile, ImplementationReport, ImplementationReportRef,
        };
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"report-exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Implement", "kind":"plan_execution", "state":"complete", "created_at_ms":0,
            "attributed_matches_checkpoint":false,"node_list":[]
        }))
        .unwrap();
        let report = ImplementationReport {
            original_revision: 1,
            accepted_revision: 2,
            revisions: Vec::new(),
            file: vec![ImplementationFile {
                path: "src/lib.rs".into(),
                internal: vec!["function helper".into()],
                contract: Vec::new(),
                references: Vec::new(),
            }],
            unavailable: Vec::new(),
        };
        exchange
            .node_list
            .push(crate::exchange::ExchangeNode::ImplementationReport {
                id: "report".into(),
                reference: ImplementationReportRef {
                    object_id: "saved".into(),
                    files: 1,
                    helpers: 1,
                    relationships: 0,
                    incomplete: false,
                },
                report: Some(std::sync::Arc::new(report)),
            });
        let render = |exchange| {
            project_at(
                &TimelineEntry::Exchange {
                    id: "report-exchange".into(),
                    created_at_ms: 0,
                    exchange,
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                1,
            )
            .unwrap()
        };
        let projected = render(exchange.clone());
        for identity in ["report", "report:file:0"] {
            let block = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == identity)
                .unwrap();
            assert!(block.metadata.fold[0].closed);
        }
        let crate::exchange::ExchangeNode::ImplementationReport { report, .. } =
            &mut exchange.node_list[0]
        else {
            unreachable!()
        };
        *report = None;
        let projected = render(exchange);
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap()))
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Report content is unavailable"));
        assert!(text.contains("Implementation differences"));
    }
}

#[cfg(test)]
#[test]
fn agent_counts_preserve_the_primary_status_and_spinner() -> anyhow::Result<()> {
    for (status, label, animated) in [
        (
            SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: Some(0),
                reasoning_summary: None,
                activity: WorkflowActivity::Working,
                execution: None,
            },
            "Working",
            true,
        ),
        (
            SessionPhase::Compacting { started_at_ms: 0 },
            "Compacting",
            true,
        ),
        (SessionPhase::Paused, "Paused", false),
        (
            SessionPhase::AwaitingPlanReview {
                plan_id: "plan".into(),
                revision: 1,
            },
            "Waiting for plan review",
            false,
        ),
    ] {
        let projection = project_at(
            &TimelineEntry::Status {
                checks: None,
                status_thoughts: true,
                id: "status".into(),
                created_at_ms: 0,
                status,
                agent_count: 2,
            },
            &WidthProfile::default(),
            1000,
        )?;
        let text = projection
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.contains(label) && text.contains("2 agents running"),
            "{text}"
        );
        assert_eq!(
            projection
                .entry
                .block
                .iter()
                .find_map(|block| block.metadata.status.as_ref())
                .unwrap()
                .animated,
            animated
        );
    }
    Ok(())
}
