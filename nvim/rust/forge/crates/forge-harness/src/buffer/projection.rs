use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BufferBlock, ContentLayout, Decoration, FoldRange, TargetRange, TextChunk,
    TextPosition, TextRange,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::node::{NodeDisplay, NodeKind, NodeLifecycle, NodeState};
use forge_buffer::width::WidthProfile;
use serde::Serialize;

use crate::exchange::{Exchange, ExchangeActivity, ExchangeNode, ExchangeState};
use crate::session::state_machine::{ExecutionStatus, SessionPhase, WorkflowActivity};
use crate::timeline::{SessionEventKind, TimelineEntry};

use super::document::TranscriptEntry;
use super::tool::ToolOutputView;
use super::transcript::TranscriptRenderer;

mod checks;

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TranscriptAction {
    Question {
        question_set_id: String,
    },
    Tool {
        call_id: super::identity::DisplayId,
    },
    Diff {
        text: String,
        #[serde(skip)]
        syntax: bool,
        #[serde(skip)]
        checkpoint: Option<SavedCheckpoint>,
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SavedCheckpoint {
    pub session_id: String,
    pub before: String,
    pub after: String,
}

pub struct ProjectedEntry {
    pub(super) status: super::status::StatusProjection,
    pub(crate) workspace_error: Option<String>,
    pub entry: TranscriptEntry,
    pub prompt: Vec<BlockId>,
    pub action: HashMap<TargetId, TranscriptAction>,
    pub tool: HashMap<super::identity::DisplayId, ToolOutputView>,
    pub(crate) syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
}

impl ProjectedEntry {
    pub(super) fn require_workspace_preview(&self) -> Result<()> {
        if let Some(error) = &self.workspace_error {
            anyhow::bail!(
                "Workspace changes preview failed, retaining the previous document: {error}"
            );
        }
        Ok(())
    }
}

pub(super) struct ProjectedMessage {
    pub block: BufferBlock,
    pub action: HashMap<TargetId, TranscriptAction>,
    pub syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
}

struct TimelineRenderer<'profile> {
    workspace_error: Option<String>,
    display: super::identity::DisplayProjection<'profile>,
    width: &'profile WidthProfile,
    margin: RenderMargin,
    now_ms: i64,
    block: Vec<RenderBlock>,
    prompt: Vec<BlockId>,
    action: HashMap<TargetId, TranscriptAction>,
    tool: HashMap<super::identity::DisplayId, ToolOutputView>,
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
    margin: RenderMarginScope,
    child_indent: usize,
}

#[derive(Default)]
struct RenderMargin(std::rc::Rc<std::cell::Cell<usize>>);

struct RenderMarginScope {
    margin: std::rc::Rc<std::cell::Cell<usize>>,
    previous: usize,
}

impl RenderMargin {
    fn enter(&self, indent: usize) -> RenderMarginScope {
        let previous = self.0.get();
        self.0.set(previous + indent);
        RenderMarginScope {
            margin: self.0.clone(),
            previous,
        }
    }
}

impl Drop for RenderMarginScope {
    fn drop(&mut self) {
        self.margin.set(self.previous);
    }
}

struct RenderBlock {
    content: BufferBlock,
    layout: ContentLayout,
}

impl RenderBlock {
    fn new(mut content: BufferBlock) -> Self {
        let layout = content.metadata.layout.take().unwrap_or(ContentLayout {
            indent: 2,
            marker: None,
            source_indent: 0,
        });
        Self { content, layout }
    }

    fn into_block(mut self) -> BufferBlock {
        self.content.metadata.layout = Some(self.layout);
        self.content
    }
}

impl std::ops::Deref for RenderBlock {
    type Target = BufferBlock;

    fn deref(&self) -> &Self::Target {
        &self.content
    }
}

impl std::ops::DerefMut for RenderBlock {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.content
    }
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

/// Places projection-test time after every fixture timestamp, as the system clock does.
#[cfg(test)]
pub(crate) const FIXTURE_NOW_MS: i64 = 1_800_000_000_000;

/// Project one entry, rendering elapsed labels against the caller's presentation time.
pub fn project<'source>(
    entry: impl Into<ProjectionSource<'source>>,
    width: &WidthProfile,
    now_ms: i64,
    leading_separator: bool,
    expansion: &HashMap<String, bool>,
    tool_limit: &HashMap<String, usize>,
    display: &mut crate::buffer::identity::DisplayRegistry,
) -> Result<ProjectedEntry> {
    project_at_with_separator(
        entry.into(),
        width,
        now_ms,
        leading_separator,
        expansion,
        tool_limit,
        display,
    )
}

/// Identify the provider turn that produced a content part, so a turn restarted after
/// an interruption (for example by a mid-turn access or thinking change) can be
/// separated from the partial output it replaces.
fn part_turn_id<'source>(
    interaction: &'source Exchange,
    content: &crate::exchange::ExchangeContentView<'source>,
    part: &crate::exchange::EntryPart<'source>,
) -> Option<&'source str> {
    match part {
        crate::exchange::EntryPart::Event(nodes) => nodes.iter().find_map(|node| match node {
            ExchangeNode::TurnContent { turn_id, .. } => Some(turn_id.as_str()),
            _ => None,
        }),
        crate::exchange::EntryPart::Commentary(index) => {
            let commentary = content.commentary.get(*index)?;
            commentary
                .message
                .and_then(|message| {
                    interaction
                        .turn
                        .iter()
                        .find(|turn| turn.messages().iter().any(|item| item.id() == message.id()))
                        .map(|turn| turn.id())
                })
                .or_else(|| {
                    commentary
                        .tools
                        .call
                        .iter()
                        .chain(&commentary.writes.call)
                        .map(|call| call.turn.id())
                        .next()
                })
        }
    }
}

fn tool_group_label<'tool>(
    calls: impl IntoIterator<Item = &'tool crate::turn::ToolCall>,
    lifecycle: NodeLifecycle,
    now: crate::turn::ToolInstant,
) -> String {
    let mut count = 0;
    let mut failed = 0;
    let mut elapsed = Some(0_u64);
    for tool in calls {
        count += 1;
        failed += usize::from(tool.failed);
        elapsed = elapsed
            .zip(tool.elapsed_ms(now))
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
    now_ms: i64,
    document: &forge_buffer::document::BufferDocument,
    expanded: &HashMap<String, bool>,
    tools: &HashMap<super::identity::DisplayId, ToolOutputView>,
    display: &crate::buffer::identity::DisplayRegistry,
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
            TimelineEntry::ObservedAgent {
                agent, children, ..
            } => {
                if agent.availability() == crate::agent::ObservedAgentAvailability::Available {
                    result.extend(agent.exchanges());
                }
                for child in children {
                    exchanges(child, result, headings);
                }
            }
            _ => {}
        }
    }
    if matches!(entry, TimelineEntry::Status { .. }) {
        return Ok(Vec::new());
    }
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
        if exchange.is_finished() || exchange.state() != ExchangeState::Running {
            continue;
        }
        if exchange.execution_started_at_ms().is_some()
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
            for tool in exchange.tools_for_turn(turn.id()) {
                let Some(call_id) = display.tool(
                    &entry.id(),
                    &crate::turn::ToolAddress {
                        turn: turn.provider().clone(),
                        item_id: forge_provider_api::ItemId::new(tool.id.clone())
                            .expect("retained tool has a validated item identity"),
                    },
                ) else {
                    continue;
                };

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
                                crate::buffer::transcript::ToolHeading {
                                    kind: &tool.kind,
                                    elapsed_ms: tool.elapsed_ms(exchange.tool_instant(now_ms)),
                                    failed: tool.failed,
                                    title: &tool.title,
                                    expanded: false,
                                    shell: tool.command_shell.as_deref(),
                                },
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
                let Some(call_id) = display.tool(
                    &entry.id(),
                    &crate::turn::ToolAddress {
                        turn: turn.provider().clone(),
                        item_id: forge_provider_api::ItemId::new(tool.id.clone())
                            .expect("retained tool has a validated item identity"),
                    },
                ) else {
                    continue;
                };

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
                    crate::buffer::transcript::ToolHeading {
                        kind: &tool.kind,
                        elapsed_ms: tool.elapsed_ms(exchange.tool_instant(now_ms)),
                        failed: tool.failed,
                        title: &tool.title,
                        expanded: expanded.get(&format!("{call_id}:tool")) == Some(&true),
                        shell: tool.command_shell.as_deref(),
                    },
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
                &tool_group_label(
                    calls.iter().map(|(_, tool)| *tool),
                    lifecycle,
                    exchange.tool_instant(now_ms),
                ),
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
) -> Result<ProjectedMessage> {
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
    let mut display = crate::buffer::identity::DisplayRegistry::default();
    let mut renderer = TimelineRenderer {
        workspace_error: None,
        display: display.project(previous.id.0.clone()),
        width: &content_width,
        margin: RenderMargin::default(),
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
    let mut block = if message.kind() == crate::turn::MessageKind::User {
        TranscriptRenderer::new(&renderer.content_width())?
            .prompt(previous.id.clone(), message.text())?
    } else {
        renderer.markdown_block(
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
        )?
    };
    block.metadata.layout = previous.metadata.layout.clone();
    if let Some(layout) = &mut block.metadata.layout {
        layout.source_indent = 0;
    }
    super::layout::materialize(&mut block)?;
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
    super::message::prepare(&mut block, width)?;
    Ok(ProjectedMessage {
        block,
        action: renderer.action,
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
        &mut crate::buffer::identity::DisplayRegistry::default(),
    )
}

fn project_at_with_separator(
    entry: ProjectionSource<'_>,
    width: &WidthProfile,
    now_ms: i64,
    leading_separator: bool,
    expansion: &HashMap<String, bool>,
    tool_limit: &HashMap<String, usize>,
    display: &mut crate::buffer::identity::DisplayRegistry,
) -> Result<ProjectedEntry> {
    let mut projection = TimelineRenderer {
        workspace_error: None,
        display: display.project(entry.id()),
        width,
        margin: RenderMargin::default(),
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
    if projection.block.is_empty() {
        if matches!(entry, ProjectionSource::Entry(TimelineEntry::Status { .. })) {
            projection.push(BufferBlock {
                id: BlockId(format!("{}:empty", entry.id())),
                text: Default::default(),
                metadata: Default::default(),
            })?;
        } else {
            projection.literal(&format!("{}:empty", entry.id()), "", None)?;
        }
    }
    let mut blocks: Vec<_> = projection
        .block
        .into_iter()
        .map(RenderBlock::into_block)
        .collect();
    for block in &mut blocks {
        TranscriptRenderer::prepare_event(block);
    }
    super::nodes::link(&mut blocks);
    for block in &mut blocks {
        TranscriptRenderer::resolve_marker(block)?;
        super::layout::materialize(block)?;
        super::message::prepare(block, width)?;
    }
    Ok(ProjectedEntry {
        status: match entry {
            ProjectionSource::Entry(entry) => {
                super::status::StatusProjection::render(entry, width, now_ms)?
            }
            ProjectionSource::Exchange(_) => super::status::StatusProjection::Unchanged,
        },
        workspace_error: projection.workspace_error,
        entry: TranscriptEntry {
            id: entry.id(),
            block: blocks,
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
            TimelineEntry::Status { id, status, .. } => {
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
            TimelineEntry::ObservedAgent {
                id,
                agent,
                children,
                ..
            } => {
                self.observed_agent(id, agent, children, depth)?;
            }
            TimelineEntry::AgentLifecycle {
                id,
                run,
                exchange: interaction,
                agent,
                ..
            } => {
                if interaction.is_empty() {
                    self.agent_entry(id, run, interaction, agent, None, depth)?;
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
                if lifecycle.kind == PlanLifecycleKind::QuestionAsked
                    && let Some(question) = &lifecycle.question
                {
                    return self.question(&event.id, question, lifecycle.answer.as_deref());
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

    /// Freeze unavailable native-child display clocks without inventing a native completion.
    fn observed_agent(
        &mut self,
        id: &str,
        agent: &crate::agent::ObservedAgent,
        children: &[TimelineEntry],
        depth: usize,
    ) -> Result<()> {
        let original_now = self.now_ms;
        if agent.availability() == crate::agent::ObservedAgentAvailability::Unavailable {
            self.now_ms = agent.observation_boundary_ms();
        }
        let rendered = stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            self.observed_agent_content(id, agent, children, depth)
        });
        self.now_ms = original_now;
        rendered
    }

    /// Present native child history without managed-agent action or task controls.
    fn observed_agent_content(
        &mut self,
        id: &str,
        agent: &crate::agent::ObservedAgent,
        children: &[TimelineEntry],
        depth: usize,
    ) -> Result<()> {
        use crate::agent::ObservedAgentState;
        use forge_provider_api::agent::{AgentActiveState, AgentOutcome};
        let description = agent.description();
        let label = description
            .nickname
            .as_deref()
            .filter(|label| !label.trim().is_empty())
            .or_else(|| {
                description
                    .definition
                    .as_deref()
                    .filter(|label| !label.trim().is_empty())
            })
            .unwrap_or("child");
        let state = match agent.state() {
            ObservedAgentState::NotObserved => "observed",
            ObservedAgentState::Active(AgentActiveState::Starting) => "starting",
            ObservedAgentState::Active(AgentActiveState::Running) => "running",
            ObservedAgentState::Active(AgentActiveState::Waiting) => "waiting",
            ObservedAgentState::Finished(AgentOutcome::Completed) => "completed",
            ObservedAgentState::Finished(AgentOutcome::Failed(_)) => "failed",
            ObservedAgentState::Finished(AgentOutcome::Interrupted) => "interrupted",
            ObservedAgentState::Finished(AgentOutcome::Closed) => "closed",
        };
        let unavailable =
            agent.availability() == crate::agent::ObservedAgentAvailability::Unavailable;
        self.literal(
            id,
            &format!(
                "Native agent {label} · {}",
                if unavailable {
                    "observation unavailable"
                } else {
                    state
                }
            ),
            None,
        )?;
        let section = self.begin_section(2);
        if unavailable {
            self.literal(
                &format!("{id}:last-state"),
                &format!("Last native state: {state}"),
                None,
            )?;
        }
        if let Some(task) = description
            .task
            .as_deref()
            .filter(|task| !task.trim().is_empty())
        {
            self.markdown(&format!("{id}:task"), task, MarkdownRole::Detail)?;
        }
        if let ObservedAgentState::Finished(AgentOutcome::Failed(failure)) = agent.state() {
            self.literal(&format!("{id}:failure"), &failure.to_string(), None)?;
        }
        let no_managed_agents = HashMap::new();
        self.host_file_change_records(agent.pending_file_changes())?;
        for exchange in agent.exchanges() {
            self.interaction(exchange, &no_managed_agents, depth + 1)?;
        }
        for child in children {
            self.entry(child, depth + 1)?;
        }
        let finished = unavailable || matches!(agent.state(), ObservedAgentState::Finished(_));
        let node = self.finish_section(section, id, finished);
        node.lifecycle = if finished {
            NodeLifecycle::Settled
        } else {
            NodeLifecycle::Live
        };
        Ok(())
    }

    fn agent_entry(
        &mut self,
        id: &str,
        run: &crate::agent::Agent,
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
        let completed = exchange.is_some_and(|exchange| exchange.is_finished());
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
                if let Some((turn, tool)) = exchange.turn.iter().rev().find_map(|turn| {
                    exchange
                        .tools_for_turn(turn.id())
                        .last()
                        .map(|tool| (turn, tool))
                }) {
                    let call = crate::exchange::ThoughtCall {
                        id: &tool.id,
                        turn,
                        tool,
                    };
                    self.tool(&call, None, "", false, exchange)?;
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
        if interaction
            .node_list
            .iter()
            .any(|node| matches!(node, ExchangeNode::AgentReference { .. }))
            && interaction.turn.is_empty()
        {
            return self.content(
                interaction,
                agents,
                depth,
                interaction.content(&interaction.node_list.iter().collect::<Vec<_>>(), true)?,
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
            interaction.content(&layout.opening, false)?,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        if let crate::exchange::ExchangeOrigin::TaskStarted { start } = &interaction.origin {
            self.task_notice(
                &format!("{}:start", interaction.id),
                &start.heading(),
                &interaction.prompt,
            )?;
        } else if let crate::exchange::ExchangeOrigin::TaskContinuation { reason } =
            &interaction.origin
        {
            self.task_notice(
                &format!("{}:origin", interaction.id),
                reason,
                &interaction.prompt,
            )?;
        } else if !matches!(
            interaction.origin,
            crate::exchange::ExchangeOrigin::NativeAgent
        ) && !interaction.prompt.is_empty()
        {
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
            capture: if interaction.approval_mode.is_some() {
                "ForgeHarnessRead".into()
            } else if interaction.is_finished() {
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
            interaction.content(&layout.activity, true)?,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        let exchange_start = section.start;
        self.finish_section(
            section,
            &format!("{}:exchange", interaction.id),
            interaction.is_finished(),
        );
        if let Some(node) = &mut self.block[exchange_start].metadata.node {
            node.kind = NodeKind::Exchange;
            node.lifecycle = if interaction.is_finished() {
                NodeLifecycle::Settled
            } else {
                NodeLifecycle::Live
            };
        }
        self.provider_checklist(interaction)?;
        self.host_file_changes(interaction)?;
        self.interaction_results(interaction)?;
        self.content(
            interaction,
            agents,
            depth,
            interaction.content(&layout.results, false)?,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        self.content(
            interaction,
            agents,
            depth,
            interaction.content(&layout.conclusion, false)?,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        let mut continuation = layout.continuation.as_slice();
        while let Some(node) = continuation.first() {
            let response = super::layout::is_outer_message(interaction, node)?;
            let mut count = 1;
            while count < continuation.len()
                && super::layout::is_outer_message(interaction, continuation[count])? == response
            {
                count += 1;
            }
            let (content, remaining) = continuation.split_at(count);
            let start = self.block.len();
            let indent = if response { 0 } else { 2 };
            let margin = self.margin.enter(indent);
            self.content(
                interaction,
                agents,
                depth,
                interaction.content(content, false)?,
                Some(&layout.questions),
                MarkdownRole::Response,
            )?;
            drop(margin);
            self.offset_layout(start, indent / 2);
            continuation = remaining;
        }
        for turn in &interaction.turn {
            for (index, error) in turn.errors().iter().enumerate() {
                self.literal(
                    &format!("{}:turn:{}:error:{index}", interaction.id, turn.id()),
                    &format!("Provider turn error: {error}"),
                    None,
                )?;
            }
            if let crate::turn::TurnState::Finished {
                outcome:
                    crate::turn::TurnOutcome::Completed {
                        stop_reason: reason,
                    },
            } = turn.state()
            {
                use forge_provider_api::turn::TurnCompletionReason;
                let notice = match reason {
                    TurnCompletionReason::Normal => None,
                    TurnCompletionReason::Refusal => Some("The provider refused to continue."),
                    TurnCompletionReason::TokenLimit => {
                        Some("The provider stopped because it reached its token limit.")
                    }
                    TurnCompletionReason::RequestLimit => Some(
                        "The provider stopped because this turn reached its model-request limit.",
                    ),
                };
                if let Some(notice) = notice {
                    self.literal(
                        &format!("{}:turn:{}:stop-reason", interaction.id, turn.id()),
                        notice,
                        None,
                    )?;
                }
            }
        }
        Ok(())
    }

    /// Project stable interaction summaries while retaining each thought's original activity.
    fn interaction_results(&mut self, interaction: &Exchange) -> Result<()> {
        let identity = format!("{}:results", interaction.id);
        self.workspace_changes(&format!("{identity}:changes"), interaction)?;
        self.check_group(&identity, interaction.checks())
    }

    /// Combines attempts under a stable owner while preserving each command's output source.
    fn check_group<'run>(
        &mut self,
        identity: &str,
        runs: impl Iterator<Item = &'run crate::checks::CheckRun>,
    ) -> Result<()> {
        let mut runs: Vec<_> = runs
            .filter(|run| run.state != crate::checks::CheckState::Running)
            .collect();
        runs.sort_by(|left, right| {
            left.started_at_ms
                .cmp(&right.started_at_ms)
                .then_with(|| left.id.cmp(&right.id))
        });
        let Some(latest) = runs.last() else {
            return Ok(());
        };
        let mut combined = (**latest).clone();
        combined.id = identity.to_owned();
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
            checks::check_summary(&combined),
            runs.iter().map(|run| run.duration_ms()).sum::<u64>() as f64 / 1000.0
        );
        self.checks(&combined, false, Some(&heading))
    }

    fn verification_result(
        &mut self,
        _exchange_id: &str,
        id: &str,
        result: &crate::exchange::VerificationResult,
    ) -> Result<()> {
        use crate::plan::execution::VerificationOutcome;
        let outcome = result.outcome;
        let identity = id.to_owned();
        if outcome == VerificationOutcome::Passed
            && result
                .verification
                .as_ref()
                .is_none_or(|report| report.checks.is_empty())
        {
            let summary = result
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
        let checks = result
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
        if !result.declaration_findings.is_empty() {
            totals.push(format!(
                "{} declaration mismatch{}",
                result.declaration_findings.len(),
                if result.declaration_findings.len() == 1 {
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
                    let action = result
                        .verification_tools
                        .get(evidence)
                        .and_then(|source| self.display.source_tool(source))
                        .map(|call_id| TranscriptAction::Tool { call_id });
                    self.literal(
                        &format!("{check_id}:evidence:{evidence_index}"),
                        "Command output",
                        action,
                    )?;
                }
                self.finish_section(check_section, &check_id, true);
            }
            self.finish_section(group, &group_id, true);
        }
        if !result.declaration_findings.is_empty() || !result.declaration_changes.is_empty() {
            let declaration_id = format!("{identity}:declarations");
            let count = result
                .declaration_findings
                .len()
                .max(result.declaration_changes.len());
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
            for change in &result.declaration_changes {
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
            for (index, finding) in result.declaration_findings.iter().enumerate() {
                if result
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
        let findings = result
            .verification
            .as_ref()
            .map_or(result.findings.as_slice(), |report| {
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
        if checks.is_empty()
            && findings.is_empty()
            && result.declaration_findings.is_empty()
            && let Some(reason) = result
                .verification
                .as_ref()
                .and_then(|report| report.reason.as_deref())
                .or(result.summary.as_deref())
        {
            self.markdown(&format!("{identity}:reason"), reason, MarkdownRole::Detail)?;
        }
        self.finish_section(section, &identity, true);
        Ok(())
    }

    fn content<'exchange>(
        &mut self,
        interaction: &'exchange Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
        content: crate::exchange::ExchangeContentView<'exchange>,
        questions: Option<&super::question::QuestionHistory<'exchange>>,
        response_role: MarkdownRole,
    ) -> Result<()> {
        let mut previous_turn: Option<&str> = None;
        for part in &content.part {
            if let Some(turn_id) = part_turn_id(interaction, &content, part) {
                if let Some(previous) = previous_turn
                    && previous != turn_id
                    && interaction.turn.iter().any(|turn| {
                        turn.id() == previous
                            && matches!(
                                turn.state(),
                                crate::turn::TurnState::Finished {
                                    outcome: crate::turn::TurnOutcome::Interrupted
                                }
                            )
                    })
                {
                    self.literal(&format!("{turn_id}:resumed"), "◇ Resumed", None)?;
                }
                previous_turn = Some(turn_id);
            }
            match part {
                crate::exchange::EntryPart::Event(nodes) => {
                    if let [
                        ExchangeNode::TurnContent {
                            turn_id,
                            item: crate::turn::TurnItem::Message { id },
                            ..
                        },
                    ] = nodes.as_slice()
                    {
                        let message = interaction
                            .turn
                            .iter()
                            .find(|turn| turn.id() == turn_id)
                            .and_then(|turn| {
                                turn.messages().iter().find(|message| message.id() == id)
                            })
                            .context("response references a missing provider message")?;
                        if message.kind() == crate::turn::MessageKind::User {
                            self.native_user_message(nodes[0].id(), message.text())?;
                        } else {
                            self.markdown(
                                nodes[0].id(),
                                message.text(),
                                if message.role().is_none() {
                                    MarkdownRole::Message
                                } else {
                                    response_role
                                },
                            )?;
                        }
                    } else {
                        self.content_nodes(interaction, agents, depth, nodes, questions)?;
                    }
                }
                crate::exchange::EntryPart::Commentary(index) => {
                    self.commentary(interaction, &content.commentary[*index], response_role)?;
                }
            }
        }
        Ok(())
    }

    /// Keep active commentary open so its existing latest-tool preview remains visible.
    fn commentary<'exchange>(
        &mut self,
        interaction: &'exchange Exchange,
        thought: &crate::exchange::CommentaryView<'exchange>,
        response_role: MarkdownRole,
    ) -> Result<()> {
        let section = if let Some(message) = thought.message {
            if message.delivery() == crate::turn::MessageDelivery::Final {
                self.markdown(&thought.id, message.text(), response_role)?;
                None
            } else {
                self.markdown(&thought.id, message.text(), MarkdownRole::Commentary)?;
                thought.has_activity().then(|| self.begin_section(2))
            }
        } else {
            self.markdown(
                &thought.id,
                "Considering initial steps",
                MarkdownRole::Commentary,
            )?;
            Some(self.begin_section(2))
        };
        self.tool_group(&thought.tools, interaction)?;
        if let Some(first) = thought.writes.call.first() {
            let calls = thought.writes.call.iter().collect::<Vec<_>>();
            self.file_change_group(first.id, &calls, interaction)?;
        }
        self.check_group(&format!("{}:results", thought.id), thought.checks.iter())?;
        if let Some(section) = section {
            let live = thought.tools.live || thought.writes.live;
            let node = self.finish_section(section, &thought.id, !live);
            node.kind = NodeKind::Thought;
            node.lifecycle = if live {
                NodeLifecycle::Live
            } else {
                NodeLifecycle::Settled
            };
        }
        Ok(())
    }

    fn tool_group(
        &mut self,
        group: &crate::exchange::ThoughtGroup<'_>,
        exchange: &Exchange,
    ) -> Result<()> {
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
            exchange.tool_instant(self.now_ms),
        );
        self.literal(&group_id, &label, None)?;
        for (index, call) in group.call.iter().enumerate() {
            let tool_start = self.block.len();
            let (call_id, more) = self.tool(
                call,
                None,
                &group_id,
                !settled && index + 1 == group.call.len(),
                exchange,
            )?;
            let node_id = format!("{call_id}:tool");
            let expansion = self.expansion.get(&node_id).copied();
            let node = self.fold(
                tool_start,
                &node_id,
                settled || index + 1 < group.call.len(),
                NodeKind::Tool,
            );
            node.lifecycle = if call.tool.state() == crate::turn::ToolState::Running {
                NodeLifecycle::Live
            } else {
                NodeLifecycle::Settled
            };
            if !settled && index + 1 == group.call.len() {
                node.default_display = NodeDisplay::Preview;
            }
            if expansion == Some(true) {
                node.more = more;
            }
            node.resolve(expansion);
        }
        let expansion = self.expansion.get(&group_id).copied();
        let node = self.fold(start, &group_id, settled, NodeKind::ToolGroup);
        node.lifecycle = lifecycle;
        node.default_display = if settled {
            NodeDisplay::Heading
        } else {
            NodeDisplay::Preview
        };
        node.resolve(expansion);
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
                    crate::timeline::TimelineEvent::Check { .. } => {
                        anyhow::bail!("exchange checks must belong to commentary")
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
                ExchangeNode::TurnContent {
                    turn_id,
                    item: crate::turn::TurnItem::Message { id },
                    ..
                } => {
                    let message = interaction
                        .turn
                        .iter()
                        .find(|turn| turn.id() == turn_id)
                        .and_then(|turn| turn.messages().iter().find(|message| message.id() == id))
                        .context("response references a missing provider message")?;
                    if message.kind() == crate::turn::MessageKind::User {
                        self.native_user_message(node.id(), message.text())?;
                    } else {
                        self.markdown(
                            node.id(),
                            message.text(),
                            if message.role().is_none() {
                                MarkdownRole::Message
                            } else {
                                MarkdownRole::Response
                            },
                        )?;
                    }
                }
                ExchangeNode::TurnContent { .. } => {
                    anyhow::bail!("tool content requires a commentary owner")
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
                ExchangeNode::QuestionWithdrawn { .. } => {}
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
                ExchangeNode::VerificationResult { id, result } => {
                    self.verification_result(&interaction.id, id, result)?;
                }
                ExchangeNode::ArtifactChange { change } => {
                    if let Some(declaration) = &change.declaration {
                        let revision_request = change.revision_request;
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

    fn compaction(&mut self, id: &str, compaction: &crate::provider::Compaction) -> Result<()> {
        use crate::provider::CompactionState;
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
        if let Some(diagnostic) = &compaction.result.error {
            let label = if compaction.state == CompactionState::Failed {
                "Failure"
            } else {
                "Explanation"
            };
            details.push(format!("{label}: {} ({:?})", diagnostic, diagnostic.kind()));
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
        if let Some(usage) = &compaction.result.compaction_tokens_used {
            if let Some(cost) = &usage.billing {
                for measure in &cost.measurements {
                    details.push(format!(
                        "{}: {} {}",
                        measure.label, measure.value, measure.unit
                    ));
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
            exchange.state() == crate::exchange::ExchangeState::Cancelled && !group.resolved;
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
            exchange.content(&clarification.content, false)?,
            None,
            MarkdownRole::Message,
        )?;
        self.finish_section(section, &id, closed);
        Ok(())
    }

    fn workspace_changes(&mut self, identity: &str, exchange: &Exchange) -> Result<()> {
        use crate::exchange::ExchangeChangeResult;
        match exchange.change_result() {
            ExchangeChangeResult::NotStarted => Ok(()),
            ExchangeChangeResult::Captured(comparison) => {
                self.workspace_comparison(identity, comparison, "")
            }
            ExchangeChangeResult::Pending {
                previous: Some(comparison),
            } => self.workspace_comparison(identity, comparison, " · previous capture · pending"),
            ExchangeChangeResult::Pending { previous: None } => Ok(()),
            ExchangeChangeResult::Unavailable {
                previous: Some(comparison),
                reason,
            } => self.workspace_comparison(
                identity,
                comparison,
                &format!(" · previous capture · unavailable: {reason}"),
            ),
            ExchangeChangeResult::Unavailable {
                previous: None,
                reason,
            } => self.literal(
                identity,
                &format!("◇ Workspace changes unavailable: {reason}"),
                None,
            ),
        }
    }

    /// Render committed host mutations independently of provider-native final snapshots.
    fn host_file_changes(&mut self, exchange: &Exchange) -> Result<()> {
        self.host_file_change_records(&exchange.host_file_changes)
    }

    /// Show the retained provider checklist as reported progress, never as Harness lifecycle.
    ///
    /// The exchange owns the canonical tracker snapshot, so a refresh rebuilds the same block
    /// identities from it. Superseded entries stay in the snapshot and appear only as a count,
    /// and a missing or empty checklist adds no section rather than implying finished work.
    fn provider_checklist(&mut self, exchange: &Exchange) -> Result<()> {
        use crate::provider::ChecklistStatus;
        let Some(checklist) = &exchange.checklist else {
            return Ok(());
        };
        if checklist.current.is_empty() {
            return Ok(());
        }
        let identity = format!("{}:provider-checklist", exchange.id);
        let completed = checklist
            .current
            .iter()
            .filter(|item| item.status == ChecklistStatus::Completed)
            .count();
        let mut heading = format!(
            "Provider-reported checklist · {completed}/{} completed",
            checklist.current.len()
        );
        if let Some(name) = checklist.name.as_deref().map(str::trim)
            && !name.is_empty()
        {
            heading = format!("{heading} · {name}");
        }
        if !checklist.superseded.is_empty() {
            heading = format!("{heading} · {} superseded", checklist.superseded.len());
        }
        self.literal(&identity, &heading, None)?;
        let section = self.begin_section(2);
        for item in &checklist.current {
            let marker = match item.status {
                ChecklistStatus::Pending => "[ ]",
                ChecklistStatus::InProgress => "[>]",
                ChecklistStatus::Completed => "[x]",
                ChecklistStatus::Superseded => "[-]",
            };
            let suffix = match item.status {
                ChecklistStatus::InProgress => " · in progress",
                ChecklistStatus::Superseded => " · superseded",
                ChecklistStatus::Pending | ChecklistStatus::Completed => "",
            };
            self.literal(
                &format!("{identity}:{}", item.id),
                &format!("{marker} {}{suffix}", item.title.trim()),
                None,
            )?;
        }
        self.finish_section(section, &identity, exchange.is_finished());
        Ok(())
    }

    /// Keep each committed host callback's diff stable before and after native transcript admission.
    fn host_file_change_records(
        &mut self,
        effects: &[crate::tools::lsp::HostFileChange],
    ) -> Result<()> {
        for effect in effects {
            let Some(diff) = crate::exchange::ProviderDiffBuilder::build_changes(&effect.changes)
            else {
                continue;
            };
            let call = serde_json::to_vec(&(&effect.scope, &effect.call_id))?;
            let identity = format!(
                "{}:host-file-changes:{}",
                effect.scope.exchange_id,
                crate::plan::digest(&call)
            );
            self.diff(&identity, "Host file changes", &diff, "", None, None)?;
        }
        Ok(())
    }

    fn workspace_comparison(
        &mut self,
        identity: &str,
        comparison: crate::exchange::CheckpointComparison<'_>,
        suffix: &str,
    ) -> Result<()> {
        if comparison.text().is_empty() {
            return Ok(());
        }
        let mut tree = super::changes::ChangeTree::render(
            &TranscriptRenderer::new(&self.content_width())?,
            identity,
            "Workspace changes",
            suffix,
            comparison.text(),
            None,
            None,
        )?;
        if let Some(error) = tree.preview_error {
            self.workspace_error.get_or_insert(error);
        }
        if tree.block.is_empty() {
            let error = "saved comparison has no file sections";
            self.workspace_error.get_or_insert(error.into());
            return self.literal(
                identity,
                &format!("◇ Workspace changes: preview unavailable ({error}){suffix}"),
                Some(TranscriptAction::Diff {
                    text: comparison.text().into(),
                    syntax: false,
                    checkpoint: Some(SavedCheckpoint {
                        session_id: comparison.session_id.into(),
                        before: comparison.before.into(),
                        after: comparison.after.into(),
                    }),
                }),
            );
        }
        for action in tree.action.values_mut() {
            if let TranscriptAction::Diff { checkpoint, .. } = action {
                *checkpoint = Some(SavedCheckpoint {
                    session_id: comparison.session_id.into(),
                    before: comparison.before.into(),
                    after: comparison.after.into(),
                });
            }
        }
        self.action.extend(tree.action);
        for block in tree.block {
            self.push(block)?;
        }
        Ok(())
    }

    fn file_change_group(
        &mut self,
        id: &str,
        calls: &[&crate::exchange::ThoughtCall<'_>],
        exchange: &Exchange,
    ) -> Result<()> {
        use crate::turn::ToolState;
        let identity = format!("{id}:changes");
        let running = calls
            .iter()
            .filter(|call| call.tool.state() == ToolState::Running)
            .count();
        let unsuccessful = calls
            .iter()
            .filter(|call| {
                call.tool.state() != ToolState::Running
                    && (call.tool.failed || call.tool.state() != ToolState::Completed)
            })
            .count();
        let mut label = format!(
            "▸ Reported edits · {} {}",
            calls.len(),
            if calls.len() == 1 {
                "operation"
            } else {
                "operations"
            }
        );
        if running > 0 {
            label.push_str(&format!(" · {running} running"));
        }
        if unsuccessful > 0 {
            label.push_str(&format!(" · {unsuccessful} unsuccessful"));
        }
        self.literal(&identity, &label, None)?;
        let section = self.begin_section(2);
        for call in calls {
            if exchange.output_store.is_some()
                && call.tool.state() != ToolState::Running
                && (call.tool.failed
                    || call.tool.state() != ToolState::Completed
                    || call.tool.change.file.is_empty())
            {
                let tool_start = self.block.len();
                let (call_id, _) = self.tool(call, Some(&identity), &identity, false, exchange)?;
                self.fold(tool_start, &format!("{call_id}:tool"), true, NodeKind::Tool);
            } else {
                self.file_change(&format!("{}:operation", call.id), call.tool)?;
            }
        }
        self.finish_section(section, &identity, true).kind = NodeKind::Changes;
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
        if let SessionEventKind::TaskStarted { start, prompt } = &event.detail {
            return self.task_notice(&format!("{}:start", event.id), &start.heading(), prompt);
        }
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

    /// Render a task start or continuation as one notice whose Harness-authored input stays folded.
    ///
    /// The provider receives the input, but the user reads only the notice heading; a multi-line
    /// literal starts as a closed fold, so the complete instruction remains inspectable without
    /// filling the transcript.
    fn task_notice(&mut self, identity: &str, heading: &str, input: &str) -> Result<()> {
        let mut text = format!("◇ {heading}");
        if !input.is_empty() {
            text.push('\n');
            text.push_str(input);
        }
        self.literal(identity, &text, None)?;
        self.prompt.push(BlockId(identity.to_owned()));
        Ok(())
    }

    fn tool(
        &mut self,
        call: &crate::exchange::ThoughtCall<'_>,
        scope: Option<&str>,
        group: &str,
        preview: bool,
        exchange: &Exchange,
    ) -> Result<(super::identity::DisplayId, bool)> {
        let interaction = call.turn.id();
        let tool = call.tool;
        let display = self.display.tool(
            crate::turn::ToolAddress {
                turn: call.turn.provider().clone(),
                item_id: forge_provider_api::ItemId::new(tool.id.clone())
                    .expect("retained tool has a validated item identity"),
            },
            scope,
        );
        let call_id = display;
        let mut output = if let Some((root, object)) = exchange.output_store.as_ref().zip(
            exchange
                .archived_output
                .get(&format!("{interaction}:{}", tool.id)),
        ) {
            let objects = crate::storage::objects::ObjectStore::open(root)?;
            let path = objects.path(object)?;
            let bytes = std::fs::metadata(&path)?.len();
            ToolOutputView::file(call_id, path, bytes)
        } else if let Some(root) = &exchange.output_store {
            ToolOutputView::stored(call_id, root, &exchange.id, interaction, &tool.id)
        } else {
            ToolOutputView::new(call_id, &tool.output)
        };
        output.heading(tool, exchange.tool_instant(self.now_ms));
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
            crate::buffer::transcript::ToolHeading {
                shell: tool.command_shell.as_deref(),
                kind: &tool.kind,
                elapsed_ms: tool.elapsed_ms(exchange.tool_instant(self.now_ms)),
                failed: tool.failed,
                title: &label,
                expanded,
            },
        )?;
        self.action
            .insert(target, TranscriptAction::Tool { call_id });
        self.push(block)?;
        if expanded || preview {
            let limit = self
                .tool_limit
                .get(&format!("{call_id}:tool"))
                .copied()
                .unwrap_or(super::tool::INITIAL_INLINE_BYTES);
            output.load_prefix(limit)?;
            for chunk in 0..if expanded {
                output.inline_prefix_chunks(limit)?
            } else {
                1
            } {
                let block = renderer.tool_body(&call_id, &output, expanded, chunk)?;
                for target in &block.metadata.target {
                    self.action
                        .insert(target.id.clone(), TranscriptAction::Tool { call_id });
                }
                self.push(block)?;
            }
            let hidden = renderer.tool_hidden(&call_id, &output, expanded)?;
            for target in &hidden.metadata.target {
                self.action
                    .insert(target.id.clone(), TranscriptAction::Tool { call_id });
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
        let limit = self
            .tool_limit
            .get(&format!("{call_id}:tool"))
            .copied()
            .unwrap_or(super::tool::INITIAL_INLINE_BYTES);
        let more = if choice == Some(true) {
            output.inline_prefix_chunks(limit)? < output.inline_chunks()?
        } else {
            false
        };
        self.tool.insert(call_id, output);
        Ok((call_id, more))
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
        let block = self.markdown_block(id, text, role)?;
        self.push(block)
    }

    /// Render positive native user content with the same sender treatment as submitted prompts.
    fn native_user_message(&mut self, id: &str, text: &str) -> Result<()> {
        let block =
            TranscriptRenderer::new(&self.content_width())?.prompt(BlockId(id.into()), text)?;
        self.prompt.push(block.id.clone());
        self.push(block)
    }

    fn markdown_block(&mut self, id: &str, text: &str, role: MarkdownRole) -> Result<BufferBlock> {
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
        let mut node = NodeState::new(
            FoldId(id.into()),
            if matches!(role, MarkdownRole::Commentary) {
                NodeKind::Thought
            } else {
                NodeKind::Message
            },
            NodeDisplay::Full,
        );
        if matches!(role, MarkdownRole::Response) {
            node.lifecycle = NodeLifecycle::Live;
        }
        rendered.block.metadata.node = Some(node);
        for link in rendered.link {
            self.action.insert(
                link.target,
                TranscriptAction::Url {
                    url: link.destination,
                },
            );
        }
        Ok(rendered.block)
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

    fn push(&mut self, block: BufferBlock) -> Result<()> {
        self.block.push(RenderBlock::new(block));
        Ok(())
    }

    fn offset_layout(&mut self, start: usize, depth: usize) {
        for block in &mut self.block[start..] {
            block.layout.indent += depth * 2;
        }
    }

    fn content_width(&self) -> WidthProfile {
        let mut width = self.width.clone();
        width.columns = width.columns.saturating_sub(self.margin.0.get()).max(1);
        width
    }

    fn begin_section(&mut self, child_indent: usize) -> Section {
        Section {
            start: self.block.len() - 1,
            margin: self.margin.enter(child_indent),
            child_indent,
        }
    }

    fn finish_section(&mut self, section: Section, identity: &str, closed: bool) -> &mut NodeState {
        drop(section.margin);
        self.offset_layout(section.start + 1, section.child_indent / 2);
        self.fold(section.start, identity, closed, NodeKind::Group)
    }

    fn fold(
        &mut self,
        start: usize,
        identity: &str,
        closed: bool,
        kind: NodeKind,
    ) -> &mut NodeState {
        let node = NodeState::new(
            FoldId(identity.into()),
            kind,
            if closed {
                NodeDisplay::Heading
            } else {
                NodeDisplay::Full
            },
        );
        let selected = &self.block[start..];
        if selected
            .iter()
            .map(|block| block.text.row_count())
            .sum::<usize>()
            < 2
            && kind != NodeKind::Tool
        {
            return self.block[start].metadata.node.insert(node);
        }
        let heading = &self.block[start];
        let end = self.block[start + 1..].last().unwrap_or(heading);
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
            heading_marker: false,
            id: FoldId(identity.into()),
            start: TextPosition { row: 0, column: 0 },
            end: anchor,
            closed,
        });
        let heading = &mut self.block[start];
        TranscriptRenderer::fold_layout_marker(&heading.content.text, &mut heading.layout);
        heading.metadata.node.insert(node)
    }
}

fn agent_summary(run: &crate::agent::Agent, exchange: Option<&Exchange>, now_ms: i64) -> String {
    if let Some(exchange) = exchange {
        let lifecycle = if run.state == crate::agent::AgentState::Closing {
            "Closing"
        } else {
            match exchange.state() {
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
                ExchangeState::Finalizing => finalization_label(exchange.finalization_outcome()),
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
                    crate::provider::ServiceTier::Standard => "",
                    crate::provider::ServiceTier::Fast => " +",
                    crate::provider::ServiceTier::Ultrafast => " ++",
                };
                format!(
                    " · {} · {}{speed}",
                    selection.provider.as_str(),
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
        SessionEventKind::TaskStarted { start, .. } => start.heading(),
        SessionEventKind::TaskFinished { outcome } => {
            let label = match outcome.kind {
                crate::task::TaskKind::Goal => "Goal",
                crate::task::TaskKind::Plan => "Plan",
                crate::task::TaskKind::Execute => "Execution",
            };
            let state = match outcome.state {
                crate::task::TaskOutcomeState::Complete => "completed",
                crate::task::TaskOutcomeState::Cancelled => "cleared",
            };
            let summary = if outcome.summary.is_empty() {
                String::new()
            } else {
                format!("\n{}", outcome.summary)
            };
            format!("{label} {state}: {}{summary}", outcome.title)
        }
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

fn finalization_label(outcome: Option<crate::exchange::ExchangeOutcome>) -> &'static str {
    if outcome == Some(crate::exchange::ExchangeOutcome::Cancelled) {
        "Stopping"
    } else {
        "Saving exchange"
    }
}

fn exchange_activity_summary(interaction: &Exchange, now_ms: i64) -> String {
    let complete = interaction.is_finished();
    let status = interaction.activity();
    let paused = status == ExchangeActivity::Paused;
    let duration = interaction.elapsed(now_ms) / 1000;
    let activity = if interaction.state() == ExchangeState::Cancelled {
        "Cancelled"
    } else if interaction.state() == ExchangeState::Finalizing {
        finalization_label(interaction.finalization_outcome())
    } else if interaction.state() == ExchangeState::Interrupted {
        "Interrupted"
    } else if interaction.state() == ExchangeState::Failed {
        "Failed"
    } else if interaction.state() == ExchangeState::Queued {
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
        if complete {
            "Thought"
        } else if paused {
            "Paused"
        } else {
            "Thinking"
        }
    };
    let summary = format!(
        "● {}{activity} {duration}s",
        history_prefix(interaction.disposition())
    );
    exchange_metrics_summary(interaction, now_ms, summary)
}

fn exchange_metrics_summary(interaction: &Exchange, now_ms: i64, summary: String) -> String {
    let count = interaction.tool_counter.total;
    let mut sections = vec![summary];
    if count > 0 {
        let passed = interaction.tool_counter.completed;
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
        // Input sums every request in the exchange, so a bare percentage beside it reads
        // as context fill; name it as the cached share of that input.
        if let Some(percent) = usage.cached_percent() {
            label.push_str(&format!(" ({percent}% cached)"));
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
    let mut changes = crate::exchange::ProviderChangeIndex::default();
    changes.record(interaction);
    if !changes.is_empty() {
        let files = changes.paths().len();
        sections.push(format!(
            "{files} file{} changed",
            if files == 1 { "" } else { "s" }
        ));
    }
    let mut summary = sections.join(" │ ");
    if let Some(error) = &interaction.finalization_error {
        summary.push_str(&format!(" — {}", error.replace(['\n', '\r'], " ")));
    }
    summary
}

#[cfg(test)]
mod tests {

    use super::project_at;

    use forge_buffer::width::WidthProfile;
    use serde_json::json;
    use std::collections::HashMap;

    use crate::{exchange::Exchange, timeline::TimelineEntry};

    #[test]
    fn outer_responses_keep_order_across_answered_questions_and_continued_turns() {
        use crate::exchange::{ExchangeNode, InputIntent, QuestionInput};
        use crate::provider::{TextUpdate, ToolOutcome};

        for planning_question in [None, Some(false), Some(true)] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"ordered", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Plan a game", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            exchange.resume(0).unwrap();
            let mut address = crate::turn::test_support::address(&exchange, "initial");
            exchange.start_turn(address.clone(), 0).unwrap();
            let event = crate::turn::test_support::message(
                &address,
                "inspection",
                Some(forge_provider_api::AgentMessageRole::Commentary),
                "Inspecting the repository",
            );
            exchange.observe_event(&event, 1).unwrap();
            let event = crate::turn::test_support::message(
                &address,
                "response",
                Some(forge_provider_api::AgentMessageRole::Response),
                "The repository contains only .git. Three choices are pending.",
            );
            exchange.observe_event(&event, 2).unwrap();
            let event = crate::turn::test_support::completed_event(
                &(address.clone()),
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                3,
            );
            exchange.observe_event(&event, 3).unwrap();

            if let Some(planning) = planning_question {
                let question: crate::plan::PlanQuestionSet = serde_json::from_value(json!({
                    "id":"format", "questions":[{"id":"game", "header":"Game format",
                        "question":"Which game format?", "options":[
                            {"label":"2D arena survival", "description":"Top-down combat"}
                        ], "allow_freeform":true}]
                }))
                .unwrap();
                exchange.node_list.push(if planning {
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
            address = crate::turn::test_support::address(&exchange, "continuation");
            exchange.start_turn(address.clone(), 5).unwrap();
            let event = crate::turn::test_support::message(
                &address,
                "continuation",
                Some(forge_provider_api::AgentMessageRole::Commentary),
                "The design will use a 2D arena survival loop.",
            );
            exchange.observe_event(&event, 6).unwrap();

            let render = |exchange: &Exchange| {
                project_at(
                    &TimelineEntry::Exchange {
                        id: exchange.id.clone(),

                        created_at_ms: 0,
                        exchange: (exchange.clone()).into(),

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

            for tool_count in 1..=3 {
                crate::turn::test_support::tool(
                    &mut exchange,
                    &address,
                    &format!("lookup-{tool_count}"),
                    forge_provider_api::ItemStart::CommandExecution(
                        forge_provider_api::ToolDescriptor {
                            semantics: Default::default(),
                            name: "lookup".into(),
                            title: format!("lookup_{tool_count}"),
                            arguments: forge_provider_api::InspectableArguments::new(json!({})),
                        },
                    ),
                    [
                        forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::ExecutionStarted,
                        },
                        forge_provider_api::ItemEvent::Tool {
                            event: forge_provider_api::ToolItemEvent::Output(TextUpdate::Replace(
                                "found".into(),
                            )),
                        },
                        forge_provider_api::ItemEvent::Completed(
                            forge_provider_api::ItemCompletion::CommandExecution(
                                forge_provider_api::ToolCompletion {
                                    outcome: ToolOutcome::Succeeded,
                                    descriptor: None,
                                    output: None,
                                    changes: Default::default(),
                                },
                            ),
                        ),
                    ],
                    6 + tool_count,
                )
                .unwrap();
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
                let activity_end = blocks
                    .iter()
                    .position(|block| block.id == summary.metadata.fold[0].end.block)
                    .unwrap();
                assert!(
                    activity_end < response_position,
                    "activity fold hid an outer response or later turn"
                );
                assert!(
                    blocks[activity_end + 1..response_position]
                        .iter()
                        .all(|block| block.id.0 == "ordered:results:changes")
                );
                if planning_question == Some(true) && tool_count == 3 {
                    println!(
                        "ORDERED_RESPONSE_FIXTURE:{}",
                        serde_json::to_string(blocks).unwrap()
                    );
                }
            }
            let event = crate::turn::test_support::message(
                &address,
                "response",
                Some(forge_provider_api::AgentMessageRole::Response),
                "The design is ready for review.",
            );
            exchange.observe_event(&event, 10).unwrap();
            let event = crate::turn::test_support::completed_event(
                &(address),
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                11,
            );
            exchange.observe_event(&event, 11).unwrap();
            exchange
                .finish(crate::exchange::ExchangeOutcome::Complete, 11)
                .unwrap();
            let completed = render(&exchange);
            let restored: Exchange =
                serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
            assert_eq!(completed.entry.block, render(&restored).entry.block);
            assert_eq!(
                completed
                    .entry
                    .block
                    .iter()
                    .filter(|block| !block.id.0.contains(":results:"))
                    .take(response_position + 1)
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

    /// A native user item echoing a Forge submission must not repeat the admitted prompt row.
    #[test]
    fn native_echo_of_a_submission_renders_one_prompt_row() -> anyhow::Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"echo", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"What is this repo?", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        for event in [
            forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::UserMessage(
                forge_provider_api::UserMessageStart {
                    text: "What is this repo?".into(),
                    submission_id: Some(forge_provider_api::SubmissionId::try_from(
                        "submission".to_owned(),
                    )?),
                },
            )),
            forge_provider_api::ItemEvent::Completed(
                forge_provider_api::ItemCompletion::UserMessage(
                    forge_provider_api::MessageCompletion {
                        text: "What is this repo?".into(),
                        outcome: forge_provider_api::MessageOutcome::Completed,
                    },
                ),
            ),
        ] {
            exchange.observe_event(
                &crate::turn::test_support::item_event(&address, "native-echo", event),
                1,
            )?;
        }
        assert_eq!(exchange.turn[0].messages().len(), 1);
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange: exchange.into(),
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            2,
        )?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert_eq!(text.matches("What is this repo?").count(), 1, "{text}");
        Ok(())
    }
}
