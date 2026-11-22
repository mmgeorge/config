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
        #[serde(skip_serializing_if = "Option::is_none")]
        column: Option<usize>,
    },
    Quickfix {
        title: String,
        items: Vec<QuickfixItem>,
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

/// Preserve diagnostic locations and severity for the editor's quickfix list.
#[derive(Clone, Debug, Serialize)]
pub struct QuickfixItem {
    pub path: String,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub message: String,
    pub severity: crate::checks::adapter::events::Severity,
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
            anyhow::bail!("Edits preview failed, retaining the previous document: {error}");
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
    /// Rows of the heading a closed event fold must leave visible; the rest is its body.
    heading_rows: usize,
}

impl RenderBlock {
    fn new(mut content: BufferBlock) -> Self {
        let layout = content.metadata.layout.take().unwrap_or(ContentLayout {
            indent: 2,
            marker: None,
            source_indent: 0,
        });
        Self {
            content,
            layout,
            heading_rows: 1,
        }
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

/// Render a group's tool count and occupied execution time without counting overlaps twice.
fn tool_group_label<'tool>(
    calls: impl IntoIterator<Item = &'tool crate::turn::ToolCall>,
    now: crate::turn::ToolInstant,
) -> String {
    let calls: Vec<_> = calls.into_iter().collect();
    let tally = ToolTally::from(crate::exchange::ToolCounter::of(calls.iter().copied()));
    format!(
        "▸ Tools · {}",
        tool_tally_label(tally, tool_group_elapsed(&calls, now), "")
    )
}

/// Get occupied group time on a shared active clock, falling back to a shared wall clock.
fn tool_group_elapsed(
    calls: &[&crate::turn::ToolCall],
    now: crate::turn::ToolInstant,
) -> Option<u64> {
    // Announced tools have no execution interval yet; they cannot invalidate known timing.
    let calls: Vec<_> = calls
        .iter()
        .copied()
        .filter(|tool| {
            tool.state() != crate::turn::ToolState::Denied
                && (tool.started_active_ms.is_some() || tool.started_at_ms.is_some())
        })
        .collect();
    if calls.is_empty() {
        return None;
    }
    let active_intervals = calls
        .iter()
        .map(|tool| {
            let start = tool.started_active_ms?;
            let end = tool.completed_active_ms.or_else(|| {
                (tool.state() == crate::turn::ToolState::Running)
                    .then_some(now.active_ms)
                    .flatten()
            })?;
            Some((i128::from(start), i128::from(end)))
        })
        .collect::<Option<Vec<_>>>();
    let mut intervals = active_intervals.or_else(|| {
        calls
            .iter()
            .map(|tool| {
                let start = tool.started_at_ms?;
                let end = tool.completed_at_ms.or_else(|| {
                    (tool.state() == crate::turn::ToolState::Running).then_some(now.wall_ms)
                })?;
                Some((i128::from(start), i128::from(end)))
            })
            .collect::<Option<Vec<_>>>()
    })?;
    intervals.sort_unstable();
    let mut elapsed = 0_u64;
    let mut previous_end = None;
    for (start, end) in intervals {
        let uncovered_start = previous_end.map_or(start, |previous: i128| previous.max(start));
        elapsed = elapsed.saturating_add(u64::try_from((end - uncovered_start).max(0)).ok()?);
        previous_end = Some(previous_end.map_or(end, |previous| previous.max(end)));
    }
    Some(elapsed)
}

/// Collect every exchange whose running labels an entry displays, including child agents.
fn timing_exchanges<'source>(entry: &'source TimelineEntry, result: &mut Vec<&'source Exchange>) {
    match entry {
        TimelineEntry::Exchange {
            exchange,
            agent_by_id,
            ..
        } => {
            result.push(exchange);
            for agent in agent_by_id.values() {
                timing_exchanges(agent, result);
            }
        }
        TimelineEntry::AgentLifecycle {
            exchange, agent, ..
        } => {
            result.extend(exchange);
            for agent in agent {
                timing_exchanges(agent, result);
            }
        }
        TimelineEntry::ObservedAgent {
            agent, children, ..
        } => {
            if agent.availability() == crate::agent::ObservedAgentAvailability::Available {
                result.extend(agent.exchanges());
            }
            for child in children {
                timing_exchanges(child, result);
            }
        }
        _ => {}
    }
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
    if matches!(entry, TimelineEntry::Status { .. }) {
        return Ok(Vec::new());
    }
    if matches!(entry, TimelineEntry::Checks { .. }) {
        return Ok(Vec::new());
    }
    let mut source = Vec::new();
    timing_exchanges(entry, &mut source);
    let mut blocks = Vec::new();
    for exchange in source {
        if exchange.is_finished() || exchange.state() != ExchangeState::Running {
            continue;
        }
        if heading_clock_runs(exchange)
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
                            let mut heading = crate::buffer::transcript::ToolHeading::of(
                                tool,
                                exchange.tool_instant(now_ms),
                                false,
                            );
                            heading.state =
                                super::tool_outcome::heading_state(exchange, turn, tool);
                            blocks.push(
                                TranscriptRenderer::new(&profile)?
                                    .refresh_tool_heading(previous, heading)?,
                            );
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
                let mut heading = crate::buffer::transcript::ToolHeading::of(
                    tool,
                    exchange.tool_instant(now_ms),
                    expanded.get(&format!("{call_id}:tool")) == Some(&true),
                );
                heading.state = super::tool_outcome::heading_state(exchange, turn, tool);
                blocks.push(
                    TranscriptRenderer::new(&profile)?.refresh_tool_heading(previous, heading)?,
                );
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
                &tool_group_label(
                    calls.iter().map(|(_, tool)| *tool),
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

/// Record when a running label that `timing_blocks` refreshes next shows a new value.
///
/// It reads the same clocks the refreshed labels format: the exchange heading's elapsed and
/// tool time, each running tool heading, and each tool group with a running call.
pub(super) fn timing_label_due(
    entry: &TimelineEntry,
    now_ms: i64,
    tools: &HashMap<super::identity::DisplayId, ToolOutputView>,
    display: &crate::buffer::identity::DisplayRegistry,
    due: &mut super::duration::LabelDue,
) {
    let mut source = Vec::new();
    timing_exchanges(entry, &mut source);
    for exchange in source {
        if exchange.is_finished() || exchange.state() != ExchangeState::Running {
            continue;
        }
        let instant = exchange.tool_instant(now_ms);
        let mut tool_running = false;
        let mut groups: BTreeMap<&str, Vec<&crate::turn::ToolCall>> = BTreeMap::new();
        for turn in &exchange.turn {
            for tool in exchange.tools_for_turn(turn.id()) {
                if tool.state() == crate::turn::ToolState::Running {
                    tool_running = true;
                    if let Some(elapsed) = tool.elapsed_ms(instant) {
                        due.elapsed(now_ms, elapsed);
                    }
                }
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
                if let Some(output) = tools.get(&call_id)
                    && !output.group.is_empty()
                {
                    groups.entry(&output.group).or_default().push(tool);
                }
            }
        }
        for calls in groups.values() {
            if calls
                .iter()
                .any(|tool| tool.state() == crate::turn::ToolState::Running)
                && let Some(elapsed) = tool_group_elapsed(calls, instant)
            {
                due.elapsed(now_ms, elapsed);
            }
        }
        if exchange.execution_started_at_ms().is_some() {
            let elapsed = exchange.elapsed(now_ms);
            due.elapsed(now_ms, elapsed);
            if tool_running && let Some(tool_ms) = exchange.metrics.tool_ms(elapsed) {
                due.elapsed(now_ms, tool_ms);
            }
        }
        if let Some(started_at_ms) = exchange.compaction_started_at_ms() {
            due.elapsed(now_ms, now_ms.saturating_sub(started_at_ms).max(0) as u64);
        }
    }
}

/// Report whether the exchange heading's figure advances with the clock.
///
/// The heading counts the execution clock while a turn runs and the compaction's own interval
/// while a compaction runs; a compaction stops the turn it interrupts, so either clock alone
/// can drive it. The timeline's periodic refresh and `timing_blocks` both read this, so a
/// heading that counts is redrawn every second without a host event.
pub(crate) fn heading_clock_runs(exchange: &Exchange) -> bool {
    exchange.execution_started_at_ms().is_some() || exchange.compaction_started_at_ms().is_some()
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
        .map(|block| {
            let heading_rows = block.heading_rows;
            let mut block = block.into_block();
            TranscriptRenderer::prepare_event(&mut block, heading_rows);
            block
        })
        .collect();
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
                                | TranscriptAction::Quickfix { .. }
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
                    self.literal(
                        id,
                        &agent_summary(run, None, self.now_ms),
                        Some(TranscriptAction::Agent {
                            run_id: run.id.clone(),
                        }),
                    )?;
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
                    PlanLifecycleKind::RevisionStopped => "Plan revision stopped",
                    PlanLifecycleKind::DraftWithdrawn => "Plan draft changed after submission",
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
                } else if lifecycle.kind == PlanLifecycleKind::DraftWithdrawn {
                    format!(
                        "▸ {label} · revision {} withdrawn",
                        lifecycle.model_revision
                    )
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
                self.event_row(&event.id, &label, "", None)?;
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
            ObservedAgentState::Finished(AgentOutcome::Interrupted) => "cancelled",
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

    /// Summarize the children this exchange delegated once each reaches a terminal state.
    ///
    /// Running children belong to the status area, so the root lists only finished children and
    /// its heading counts them against every delegation. Leaves keep the agent action, which
    /// opens the child's own transcript, but never expand inline.
    fn agent_results(
        &mut self,
        exchange: &Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
    ) -> Result<()> {
        let delegations = exchange
            .node_list
            .iter()
            .filter_map(|node| match node {
                ExchangeNode::AgentReference { agent } => Some(agent),
                _ => None,
            })
            .filter_map(|delegation| {
                agents
                    .get(delegation.id.as_str())
                    .or_else(|| agents.get(delegation.child_agent_id.as_str()))
                    .and_then(|entry| DelegatedAgent::resolve(delegation, entry))
            })
            .collect::<Vec<_>>();
        // A stop cancels every running child while the status area still lists them, and the
        // status closes only once the exchange settles. Until then the summary leaves out the
        // children the stop cancelled, so they join it in the frame in which the status closes,
        // and the transcript above moves once instead of once per step. Only the user's stop
        // settles the exchange: a restart for a configuration change also collects the turn and
        // its children, but the exchange runs on, so the children it cancelled are drawn at once.
        // A child cancelled before the stopped turn started, by a restart or by the user, is
        // already drawn, so the summary keeps it; withholding it emptied the summary mid-stop.
        let stop_started_ms = (!exchange.is_finished()
            && (exchange.cancellation_requested
                || exchange.finalization_outcome()
                    == Some(crate::exchange::ExchangeOutcome::Cancelled)))
        .then(|| {
            exchange
                .turn
                .last()
                .map_or(i64::MIN, |turn| turn.started_at_ms())
        });
        let identity = format!("{}:agents", exchange.id);
        self.agent_group(
            &identity,
            &delegations,
            exchange.is_finished(),
            stop_started_ms,
        )
    }

    /// Summarize the latest run of each delegated child that has finished.
    ///
    /// `stop_started_ms` withholds the children that ended cancelled at or after that time,
    /// which a stop of their parent cancelled, until that parent settles.
    fn agent_group(
        &mut self,
        identity: &str,
        delegations: &[DelegatedAgent<'_>],
        closed: bool,
        stop_started_ms: Option<i64>,
    ) -> Result<()> {
        // A parent can run one child several times in an exchange, through a model follow-up or a
        // user's, and each run is its own delegation. The summary names each child once, by its
        // latest run, so it never lists the same child twice with different results.
        let mut latest_runs: Vec<&DelegatedAgent<'_>> = Vec::new();
        for delegated in delegations {
            match latest_runs
                .iter_mut()
                .find(|latest| latest.run.id == delegated.run.id)
            {
                Some(latest)
                    if delegated.exchange.created_at_ms >= latest.exchange.created_at_ms =>
                {
                    *latest = delegated;
                }
                Some(_) => {}
                None => latest_runs.push(delegated),
            }
        }
        let finished = latest_runs
            .iter()
            .copied()
            .filter(|delegated| {
                let cancelled_by_stop = stop_started_ms.is_some_and(|stop_started_ms| {
                    delegated.exchange.state() == ExchangeState::Cancelled
                        && delegated
                            .exchange
                            .completed_at_ms()
                            .is_none_or(|completed_at_ms| completed_at_ms >= stop_started_ms)
                });
                delegated.exchange.is_finished() && !cancelled_by_stop
            })
            .collect::<Vec<_>>();
        let Some((first, rest)) = finished.split_first() else {
            return Ok(());
        };
        let completed = finished
            .iter()
            .filter(|delegated| delegated.exchange.state() == ExchangeState::Complete)
            .count();
        // Children run concurrently, so the heading reports the span from the first child's
        // creation to the last child's settlement rather than the sum of their durations.
        let first_created_ms = finished
            .iter()
            .map(|delegated| delegated.exchange.created_at_ms)
            .min();
        let last_completed_ms = finished
            .iter()
            .filter_map(|delegated| delegated.exchange.completed_at_ms())
            .max();
        let longest_ms = finished
            .iter()
            .map(|delegated| delegated.exchange.elapsed(self.now_ms))
            .max()
            .unwrap_or_default();
        let wall_ms = match (first_created_ms, last_completed_ms) {
            (Some(created), Some(completed)) => u64::try_from(completed - created)
                .unwrap_or_default()
                .max(longest_ms),
            _ => longest_ms,
        };
        let mut sections = vec![format!(
            "Agents · {completed}/{} completed · {}s",
            latest_runs.len(),
            wall_ms / 1000
        )];
        let tally = finished
            .iter()
            .fold(ToolTally::default(), |tally, delegated| ToolTally {
                successful: tally.successful + delegated.exchange.tool_counter.completed,
                denied: tally.denied + delegated.exchange.tool_counter.denied,
                total: tally.total + delegated.exchange.tool_counter.total,
            });
        let tool_ms = finished
            .iter()
            .map(|delegated| {
                delegated
                    .exchange
                    .metrics
                    .tool_ms(delegated.exchange.elapsed(self.now_ms))
            })
            .sum::<Option<u64>>();
        if let Some(tools) = tool_count_label(tally, tool_ms) {
            sections.push(tools);
        }
        let mut usage = first.exchange.usage();
        for delegated in rest {
            usage.accumulate(&delegated.exchange.usage());
        }
        if let Some(tokens) = token_usage_label(&usage) {
            sections.push(tokens);
        }
        self.literal(identity, &sections.join(" │ "), None)?;
        let section = self.begin_section(2);
        for delegated in finished {
            self.agent_result(identity, delegated)?;
        }
        self.finish_section(section, identity, closed);
        Ok(())
    }

    fn agent_result(&mut self, identity: &str, delegated: &DelegatedAgent<'_>) -> Result<()> {
        stacker::maybe_grow(64 * 1024, 1024 * 1024, || {
            let exchange = delegated.exchange;
            let (marker, outcome) = match exchange.state() {
                ExchangeState::Complete => ("✓", "Ran"),
                ExchangeState::Cancelled => ("✗", "Cancelled"),
                ExchangeState::Interrupted => ("✗", "Interrupted"),
                _ => ("✗", "Failed"),
            };
            let heading = format!(
                "{marker} {}{} · {outcome} {}s",
                delegated.run.label(),
                agent_selection_label(delegated.run),
                exchange.elapsed(self.now_ms) / 1000,
            );
            let leaf = format!("{identity}:{}", delegated.delegation.id);
            self.literal(
                &leaf,
                &exchange_metrics_summary(exchange, self.now_ms, heading),
                Some(TranscriptAction::Agent {
                    run_id: delegated.run.id.clone(),
                }),
            )?;
            let nested = exchange
                .node_list
                .iter()
                .filter_map(|node| match node {
                    ExchangeNode::AgentReference { agent } => Some(agent),
                    _ => None,
                })
                .filter_map(|delegation| {
                    delegated
                        .children
                        .iter()
                        .find(|entry| {
                            entry.id() == delegation.id || entry.id() == delegation.child_agent_id
                        })
                        .and_then(|entry| DelegatedAgent::resolve(delegation, entry))
                })
                .collect::<Vec<_>>();
            if nested.iter().any(|child| child.exchange.is_finished()) {
                let section = self.begin_section(2);
                self.agent_group(&format!("{leaf}:agents"), &nested, true, None)?;
                self.finish_section(section, &leaf, false);
            }
            Ok(())
        })
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
            self.content(
                interaction,
                agents,
                depth,
                interaction.content(&interaction.node_list.iter().collect::<Vec<_>>(), true)?,
                None,
                MarkdownRole::Response,
            )?;
            return self.agent_results(interaction, agents);
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
        if let crate::exchange::ExchangeOpening::User { content } = &interaction.opening {
            let id = format!("{}:prompt", interaction.id);
            if let Some(group) = super::question::QuestionGroup::from_content(&id, content) {
                self.question_group(&group, interaction, agents, depth)?;
            } else {
                let text = content.label();
                if !text.is_empty() {
                    let prompt = TranscriptRenderer::new(&self.content_width())?
                        .prompt(BlockId(id), &text)?;
                    self.prompt.push(prompt.id.clone());
                    self.push(prompt)?;
                }
            }
        } else if let crate::exchange::ExchangeOpening::TaskStarted { start } = &interaction.opening
        {
            self.task_notice(
                &format!("{}:start", interaction.id),
                &start.heading(),
                &interaction.prompt,
            )?;
        } else if let crate::exchange::ExchangeOpening::TaskContinuation { reason } =
            &interaction.opening
        {
            self.task_notice(
                &format!("{}:origin", interaction.id),
                reason,
                &interaction.prompt,
            )?;
        }
        // The first turn's check runs are drawn once, as the exchange's results beneath the
        // Thought, so the commentary that owns them inside the Thought does not draw them too.
        // Later turns' commentary draws its own runs where they happened.
        let mut activity = interaction.content(&layout.activity, true)?;
        let activity_checks = activity.take_checks();
        // A task notice whose exchange settled with nothing beneath it stands alone: a heading
        // there would summarize no work, as a goal paused before its first answer shows.
        if !(agents.is_empty() && interaction.is_settled_output_free_task_notice()) {
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
                capture: if interaction.is_finished() {
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
                activity,
                Some(&layout.questions),
                MarkdownRole::Response,
            )?;
            // A native child the provider ran inside this turn stays inside its Thought, where it
            // ran, before the answer that followed it, whether the turn is running or settled.
            let mut observed = agents
                .values()
                .filter_map(|entry| match entry {
                    TimelineEntry::ObservedAgent {
                        id, created_at_ms, ..
                    } => Some(((*created_at_ms, id.as_str()), *entry)),
                    _ => None,
                })
                .collect::<Vec<_>>();
            observed.sort_by_key(|(order, _)| *order);
            for (_, entry) in observed {
                self.entry(entry, depth + 1)?;
            }
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
        }
        self.content(
            interaction,
            agents,
            depth,
            interaction.content(&layout.steering, false)?,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        self.provider_checklist(interaction)?;
        self.host_file_changes(interaction)?;
        self.agent_results(interaction, agents)?;
        self.interaction_results(interaction, activity_checks)?;
        self.content(
            interaction,
            agents,
            depth,
            interaction.content(&layout.results, false)?,
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
        // An execution's completion or failure concludes the exchange, so it follows the response.
        self.content(
            interaction,
            agents,
            depth,
            interaction.content(&layout.conclusion, false)?,
            Some(&layout.questions),
            MarkdownRole::Response,
        )?;
        for (index, record) in interaction.tool_timeout.iter().enumerate() {
            self.literal(
                &format!("{}:tool-timeout:{index}", interaction.id),
                &super::tool_outcome::timeout_notice(interaction, record),
                None,
            )?;
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

    /// Project the exchange's file changes and the check runs of its first turn.
    fn interaction_results(
        &mut self,
        interaction: &Exchange,
        checks: Vec<&crate::checks::CheckRun>,
    ) -> Result<()> {
        let identity = format!("{}:results", interaction.id);
        self.workspace_changes(&format!("{identity}:changes"), interaction)?;
        self.check_group(&identity, checks.into_iter())
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
        // The latest run carries the completion title and its selected skips, which explain a
        // run that started no check; earlier runs contribute only commands that executed.
        let mut combined = (**latest).clone();
        combined.id = identity.to_owned();
        combined.commands = runs
            .iter()
            .flat_map(|run| {
                let latest = std::ptr::eq(*run, *latest);
                run.commands
                    .iter()
                    .filter(move |command| match command.state {
                        crate::checks::CheckState::Pending => false,
                        crate::checks::CheckState::Skipped => latest && command.selected,
                        _ => true,
                    })
            })
            .map(|command| {
                let mut command = command.clone();
                command.id = format!("{}:command:{}", combined.id, command.id);
                command
            })
            .collect();
        let heading = format!(
            "Checks · {} · {:.1}s",
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
        use crate::plan::execution::VerificationStatus;
        let status = result.status;
        let identity = id.to_owned();
        if status == VerificationStatus::Passed
            && result
                .report
                .as_ref()
                .is_none_or(|report| report.manual_checks.is_empty())
        {
            let summary = result
                .phase_summary
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
        let label = match status {
            VerificationStatus::Failed => "Verification failed",
            VerificationStatus::Blocked => "Verification blocked",
            VerificationStatus::Passed => "Verification passed",
        };
        let manual_checks = result
            .report
            .as_ref()
            .map_or(&[][..], |report| report.manual_checks.as_slice());
        let failed = manual_checks
            .iter()
            .filter(|check| check.status == VerificationStatus::Failed)
            .count();
        let blocked = manual_checks
            .iter()
            .filter(|check| check.status == VerificationStatus::Blocked)
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
        if !manual_checks.is_empty() {
            let group_id = format!("{identity}:Manual");
            let counts = [
                (VerificationStatus::Passed, "passed"),
                (VerificationStatus::Failed, "failed"),
                (VerificationStatus::Blocked, "blocked"),
            ]
            .into_iter()
            .filter_map(|(status, label)| {
                let count = manual_checks
                    .iter()
                    .filter(|check| check.status == status)
                    .count();
                (count > 0).then(|| format!("{count} {label}"))
            })
            .collect::<Vec<_>>()
            .join(", ");
            self.literal(&group_id, &format!("▸ Manual checks · {counts}"), None)?;
            let group = self.begin_section(2);
            for (index, check) in manual_checks.iter().enumerate() {
                let check_id = format!("{group_id}:{index}");
                let status = match check.status {
                    VerificationStatus::Passed => "passed",
                    VerificationStatus::Failed => "failed",
                    VerificationStatus::Blocked => "blocked",
                };
                self.literal(
                    &check_id,
                    &format!("▸ {} · {status}", check.label.replace(['\r', '\n'], " ")),
                    None,
                )?;
                let check_section = self.begin_section(2);
                self.markdown(
                    &format!("{check_id}:note"),
                    check.note.trim_end(),
                    MarkdownRole::Detail,
                )?;
                for (source_index, evidence_id) in check.observed_in.iter().enumerate() {
                    let action = result
                        .evidence_sources
                        .get(evidence_id)
                        .and_then(|source| self.display.source_tool(source))
                        .map(|call_id| TranscriptAction::Tool { call_id });
                    self.literal(
                        &format!("{check_id}:observed_in:{source_index}"),
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
            .report
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
        if manual_checks.is_empty()
            && findings.is_empty()
            && result.declaration_findings.is_empty()
            && let Some(reason) = result
                .report
                .as_ref()
                .and_then(|report| report.reason.as_deref())
                .or(result.phase_summary.as_deref())
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
                                } else if message.delivery() == crate::turn::MessageDelivery::Final
                                    && super::layout::response_continues_with_checks(
                                        interaction,
                                        nodes[0],
                                    )
                                {
                                    MarkdownRole::Commentary
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

    /// Keep active commentary open so the latest tool heading remains visible.
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
        self.approval_timeouts(&thought.tools, interaction)?;
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

    /// Record each approval that expired for a call in this group directly beneath the group.
    ///
    /// The expiry belongs with the request that asked for it. A continuation turn, when one
    /// follows, opens later with the same notice; when the turn completed normally there is no
    /// continuation, and this row is the transcript's only record of the expiry.
    fn approval_timeouts(
        &mut self,
        group: &crate::exchange::ThoughtGroup<'_>,
        exchange: &Exchange,
    ) -> Result<()> {
        for call in &group.call {
            for event in &exchange.approval_resolution {
                let crate::provider::PermissionApprovalEvent::Resolved(resolution) = event else {
                    continue;
                };
                let crate::provider::ApprovalScope::Item {
                    thread_id,
                    turn_id,
                    item_id,
                } = &resolution.scope
                else {
                    continue;
                };
                if thread_id != &call.turn.provider().thread_id
                    || turn_id != &call.turn.provider().turn_id
                    || item_id.as_str() != call.tool.id
                    || !resolution.targets.iter().any(|target| {
                        matches!(
                            target.decision,
                            crate::provider::PermissionDecision::TimedOut
                        )
                    })
                {
                    continue;
                }
                let identity = format!("{}:approval-timeout", resolution.request_id.as_str());
                let mut detail = format!(
                    "Request
  {}

Action",
                    call.tool.title
                );
                if let Some(execution) = super::tool_outcome::execution(exchange, call) {
                    detail.push_str(&crate::provider::approval_timeout::execution_display(
                        execution,
                    )?);
                }
                detail.push_str(
                    "

",
                );
                detail.push_str(crate::provider::approval_timeout::TIMEOUT_OUTCOME);
                // An event row with a body folds closed on its own, as task notices do.
                self.literal(
                    &identity,
                    &format!(
                        "◇ An approval timed out
{detail}"
                    ),
                    None,
                )?;
            }
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
            exchange.tool_instant(self.now_ms),
        );
        self.literal(&group_id, &label, None)?;
        for call in &group.call {
            let tool_start = self.block.len();
            let (call_id, more) = self.tool(call, None, &group_id, exchange)?;
            let node_id = format!("{call_id}:tool");
            let expansion = self.expansion.get(&node_id).copied();
            let node = self.fold(tool_start, &node_id, true, NodeKind::Tool);
            node.lifecycle = if call.tool.state() == crate::turn::ToolState::Running {
                NodeLifecycle::Live
            } else {
                NodeLifecycle::Settled
            };
            if expansion == Some(true) {
                node.more = more;
            }
            node.resolve(expansion);
        }
        let expansion = self.expansion.get(&group_id).copied();
        let node = self.fold(start, &group_id, true, NodeKind::ToolGroup);
        node.lifecycle = lifecycle;
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
                        &format!("≡ Skill · {}", read.name),
                        Some(TranscriptAction::File {
                            path: read.path.clone(),
                            line: 1,
                            column: None,
                        }),
                    )?;
                }
                ExchangeNode::InstructionRead { id, read, .. } => {
                    self.literal(
                        id,
                        &format!("≡ Instruction · {}", instruction_display_path(&read.path)),
                        Some(TranscriptAction::File {
                            path: read.path.clone(),
                            line: 1,
                            column: None,
                        }),
                    )?;
                }
                ExchangeNode::Compaction { id, compaction } => self.compaction(
                    id,
                    compaction,
                    compaction_restarted(interaction, compaction),
                )?,
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
                    // An unconfirmed steer reached the provider, but no report shows the model read
                    // it, so its row keeps the text and states that limit.
                    let unconfirmed =
                        prompt.consumption == crate::exchange::InputConsumption::Unconfirmed;
                    let label = match &prompt.intent {
                        crate::exchange::InputIntent::User { content } => match content {
                            crate::input::UserInputContent::UserText { .. } if unconfirmed => {
                                Some("Steering (not confirmed read)")
                            }
                            crate::input::UserInputContent::UserText { .. } => None,
                            crate::input::UserInputContent::QuestionResponse { .. } => {
                                Some("Question response")
                            }
                            crate::input::UserInputContent::CheckResult { .. } => {
                                Some("Check result")
                            }
                        },
                        crate::exchange::InputIntent::Clarification => Some("You asked"),
                        crate::exchange::InputIntent::Answer => Some("You answered"),
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
                    let id = BlockId(format!("{}:prompt", prompt.id));
                    let text = match label {
                        Some(label) => format!("{label}: {text}"),
                        None => text,
                    };
                    let block = if unconfirmed {
                        renderer.unconfirmed_prompt(id, &text)?
                    } else {
                        renderer.prompt(id, &text)?
                    };
                    self.prompt.push(block.id.clone());
                    self.push(block)?;
                }
                ExchangeNode::AgentReference { agent } => {
                    let run = agents
                        .get(agent.id.as_str())
                        .or_else(|| agents.get(agent.child_agent_id.as_str()))
                        .and_then(|entry| match entry {
                            TimelineEntry::AgentLifecycle { run, .. } => Some(run.as_ref()),
                            _ => None,
                        });
                    let text = match run {
                        Some(run) => {
                            format!("◇ Spawned {}{}", run.label(), agent_selection_label(run))
                        }
                        None => "◇ Spawned agent".into(),
                    };
                    self.literal(
                        &agent.id,
                        &text,
                        Some(TranscriptAction::Agent {
                            run_id: run
                                .map_or_else(|| agent.child_agent_id.clone(), |run| run.id.clone()),
                        }),
                    )?;
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

    /// Draw one compaction row; `restarted` adds that the turn it stopped ran again afterwards.
    fn compaction(
        &mut self,
        id: &str,
        compaction: &crate::provider::Compaction,
        restarted: bool,
    ) -> Result<()> {
        use crate::provider::CompactionState;
        let explanation = compaction.result.provider_explanation.as_ref();
        let label = match compaction.state {
            CompactionState::Running => return Ok(()),
            CompactionState::Completed => "Context compacted",
            CompactionState::Failed if explanation.is_some() => "Context not compacted",
            CompactionState::Failed => "Context compaction failed",
            CompactionState::Unconfirmed => "Context compaction completion unconfirmed",
        };
        let mut heading = format!("◇ {label}");
        if let Some(explanation) = explanation {
            heading.push_str(&format!(" · {explanation}"));
        }
        // The two figures must be one measure taken around the compaction. The provider's own
        // counts come first: the context meter Harness captured at the start predates the
        // context of a turn the compaction stopped, so beside the provider's post-compaction
        // count it reads as a compaction that grew the context.
        let result = &compaction.result;
        let context_tokens = result
            .pre_compaction_tokens
            .zip(result.post_compaction_tokens)
            .or_else(|| {
                result
                    .before_context
                    .as_ref()
                    .and_then(|context| context.current_tokens)
                    .zip(
                        result
                            .context_window
                            .as_ref()
                            .and_then(|context| context.current_tokens),
                    )
            })
            .or_else(|| {
                result
                    .context_tokens_before
                    .zip(result.context_tokens_after)
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
        if restarted {
            heading.push_str(" · turn ran again");
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
        } else if group.resolved && answered > 0 {
            format!(
                "▸ Answered {answered} {}",
                if answered == 1 {
                    "question"
                } else {
                    "questions"
                }
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
        if group.resolved && answered > 0 {
            let heading = self
                .block
                .last_mut()
                .expect("question heading was just rendered");
            if let Some(marker) = &mut heading.layout.marker {
                marker.capture = "ForgeHarnessPrompt".into();
            }
            let row_count = heading.text.row_count();
            heading.metadata.decoration.push(Decoration {
                range: TextRange {
                    start: TextPosition { row: 0, column: 0 },
                    end: TextPosition {
                        row: row_count,
                        column: 0,
                    },
                },
                capture: "ForgeHarnessPrompt".into(),
                priority: 100,
            });
            self.prompt.push(heading.id.clone());
        }
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
                &if group.resolved && branch.answer.is_some() {
                    branch.question.question.clone()
                } else {
                    format!("▸ {} · {status}", branch.question.question)
                },
                (!cancelled).then(|| TranscriptAction::Question {
                    question_set_id: branch.question_set_id.to_owned(),
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
            self.finish_section(question_section, &id, settled && !group.resolved);
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
            ExchangeChangeResult::NotStarted | ExchangeChangeResult::Delegated => Ok(()),
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
            } => self.literal(identity, &format!("◇ Edits unavailable: {reason}"), None),
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
            "Edits",
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
                &format!("◇ Edits: preview unavailable ({error}){suffix}"),
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
        // A change refused by a permission decision never ran, so it is denied, not unsuccessful.
        let denied = calls
            .iter()
            .filter(|call| call.tool.state() == ToolState::Denied)
            .count();
        let unsuccessful = calls
            .iter()
            .filter(|call| {
                !matches!(call.tool.state(), ToolState::Running | ToolState::Denied)
                    && (call.tool.failed || call.tool.state() != ToolState::Completed)
            })
            .count();
        let mut label = format!(
            "▸ Edits · {} {}",
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
        if denied > 0 {
            label.push_str(&format!(" · {denied} denied"));
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
                let (call_id, _) = self.tool(call, Some(&identity), &identity, exchange)?;
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
            return self.compaction(&event.id, compaction, false);
        }
        let action = match &event.detail {
            SessionEventKind::Forked {
                source_session_id, ..
            } => Some(TranscriptAction::Session {
                session_id: source_session_id.clone(),
            }),
            _ => None,
        };
        let text = session_event_text(&event.detail);
        let (heading, body) = text.split_once('\n').unwrap_or((&text, ""));
        self.event_row(&event.id, heading, body, action)
    }

    /// Render an event row whose heading stays whole, so only its body starts folded.
    ///
    /// Task rows name their task by title, and a closed fold over the heading's own wrapped rows
    /// would cut that title mid-phrase. The heading wraps in full instead, as `Plan submitted`
    /// does, and the fold covers only the lines after it.
    fn event_row(
        &mut self,
        identity: &str,
        heading: &str,
        body: &str,
        action: Option<TranscriptAction>,
    ) -> Result<()> {
        let heading = format!("◇ {heading}");
        let heading_rows = TranscriptRenderer::new(&self.content_width())?
            .literal(BlockId(identity.into()), &heading, 2)?
            .text
            .row_count();
        let text = if body.is_empty() {
            heading
        } else {
            format!("{heading}\n{body}")
        };
        self.literal(identity, &text, action)?;
        if let Some(block) = self.block.last_mut() {
            block.heading_rows = heading_rows;
        }
        Ok(())
    }

    /// Render a task start or continuation as one notice whose Harness-authored input stays folded.
    ///
    /// The provider receives the input, but the user reads only the notice heading; a multi-line
    /// literal starts as a closed fold, so the complete instruction remains inspectable without
    /// filling the transcript.
    fn task_notice(&mut self, identity: &str, heading: &str, input: &str) -> Result<()> {
        self.event_row(identity, heading, input, None)?;
        self.prompt.push(BlockId(identity.to_owned()));
        Ok(())
    }

    fn tool(
        &mut self,
        call: &crate::exchange::ThoughtCall<'_>,
        scope: Option<&str>,
        group: &str,
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
        let outcome = super::tool_outcome::outcome_line(exchange, call);
        let timed_out = super::tool_outcome::timed_out(exchange, call).is_some();
        let mut output = if tool.state() == crate::turn::ToolState::Denied {
            ToolOutputView::new(call_id, outcome.as_deref().unwrap_or("Permission denied"))
        } else if let Some((root, object)) = exchange.output_store.as_ref().zip(
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
        // A denial already shows its reason as the body; any other unsuccessful end keeps its
        // output and states how the call ended beneath it. The provider reports a call its
        // deadline stopped as interrupted; Harness knows it failed.
        output.conclude(
            outcome.filter(|_| tool.state() != crate::turn::ToolState::Denied),
            timed_out.then_some(crate::turn::ToolState::Failed),
        );
        output.group = group.to_owned();
        let id = format!("{call_id}:tool");
        let target = TargetId(id.clone());
        let width = self.content_width();
        let renderer = TranscriptRenderer::new(&width)?;
        let choice = self.expansion.get(&format!("{call_id}:tool")).copied();
        let expanded = !group.is_empty() && choice == Some(true);
        let mut heading = crate::buffer::transcript::ToolHeading::of(
            tool,
            exchange.tool_instant(self.now_ms),
            expanded,
        );
        if let Some(state) = output.heading_state() {
            heading.state = state;
        }
        let block = renderer.tool_header(BlockId(id), target.clone(), heading)?;
        self.action
            .insert(target, TranscriptAction::Tool { call_id });
        self.push(block)?;
        if expanded {
            let limit = self
                .tool_limit
                .get(&format!("{call_id}:tool"))
                .copied()
                .unwrap_or(super::tool::INITIAL_INLINE_BYTES);
            output.load_prefix(limit)?;
            for chunk in 0..output.inline_prefix_chunks(limit)? {
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

/// A delegation resolved to its child agent, the exchange it delegated, and the child's own
/// delegated children.
struct DelegatedAgent<'source> {
    delegation: &'source crate::agent::Delegation,
    run: &'source crate::agent::Agent,
    exchange: &'source Exchange,
    children: &'source [TimelineEntry],
}

impl<'source> DelegatedAgent<'source> {
    fn resolve(
        delegation: &'source crate::agent::Delegation,
        entry: &'source TimelineEntry,
    ) -> Option<Self> {
        let TimelineEntry::AgentLifecycle {
            run,
            exchange,
            agent,
            ..
        } = entry
        else {
            return None;
        };
        let exchange = exchange
            .iter()
            .find(|exchange| exchange.id == delegation.child_exchange_id)?;
        Some(Self {
            delegation,
            run,
            exchange,
            children: agent,
        })
    }
}

/// Format a child's resolved provider, model, and speed as ` · provider · model +`.
pub(crate) fn agent_selection_label(run: &crate::agent::Agent) -> String {
    run.selection
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
        .unwrap_or_default()
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
        let selection = agent_selection_label(run);
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
        SessionEventKind::TaskAdvancement { message } => message.clone(),
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
            unfinished_exchange_omitted,
        } => {
            // The omission leads the line: a session ID and name fill most of a transcript row,
            // and a wrapped event folds every row after its first.
            let mut text = if *unfinished_exchange_omitted {
                format!("Forked before the unfinished exchange of {source_session_id}")
            } else {
                format!("Forked from {source_session_id}")
            };
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

/// Report whether a manual compaction stopped this exchange's turn, which then ran again.
///
/// `/compact` during a turn records the attempt inside the exchange it stopped; every turn that
/// started after the compaction began is the replay on the compacted context.
fn compaction_restarted(exchange: &Exchange, compaction: &crate::provider::Compaction) -> bool {
    compaction.manual
        && compaction.state == crate::provider::CompactionState::Completed
        && compaction.started_at_ms.is_some_and(|started_at_ms| {
            exchange
                .turn
                .iter()
                .any(|turn| turn.started_at_ms() >= started_at_ms)
        })
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
    let mut duration = interaction.elapsed(now_ms) / 1000;
    let activity = if let Some(collection) = interaction.collection {
        collection.label()
    } else if interaction.state() == ExchangeState::Cancelled {
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
    } else if let Some((started_at_ms, completed_at_ms)) = heading_compaction(interaction, status) {
        // A compaction stops the turn it interrupts, so the exchange has no live execution and
        // reads as paused; the status row names the compaction and counts it from its start,
        // and so does the heading, until the stopped turn runs again.
        duration = completed_at_ms
            .unwrap_or(now_ms)
            .saturating_sub(started_at_ms)
            .max(0) as u64
            / 1000;
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

/// Find the compaction that holds the exchange heading, as its start and, once it has ended,
/// its end.
///
/// A running compaction holds it, as it holds the status row. A manual compaction that stopped
/// the turn and has completed keeps holding it while the exchange waits, paused, for that turn
/// to run again; the first turn started after the compaction began releases it.
fn heading_compaction(
    interaction: &Exchange,
    status: ExchangeActivity,
) -> Option<(i64, Option<i64>)> {
    if let ExchangeActivity::Compacting { started_at_ms } = status {
        return Some((started_at_ms, None));
    }
    if status != ExchangeActivity::Paused {
        return None;
    }
    if let Some(started_at_ms) = interaction.compaction_started_at_ms() {
        return Some((started_at_ms, None));
    }
    interaction.node_list.iter().rev().find_map(|node| {
        let ExchangeNode::Compaction { compaction, .. } = node else {
            return None;
        };
        let started_at_ms = compaction.started_at_ms?;
        (compaction.manual
            && compaction.state == crate::provider::CompactionState::Completed
            && interaction
                .turn
                .iter()
                .all(|turn| turn.started_at_ms() < started_at_ms))
        .then_some((started_at_ms, compaction.completed_at_ms))
    })
}

fn exchange_metrics_summary(interaction: &Exchange, now_ms: i64, summary: String) -> String {
    let mut sections = vec![summary];
    if let Some(tools) = exchange_tool_label(interaction, now_ms) {
        sections.push(tools);
    }
    let mut tokens = Vec::new();
    // The input sums every turn, and the output is that of the turns that produced the answer,
    // the same turns the speed divides by their time.
    let mut usage = interaction.usage();
    usage.output = interaction.metrics.reported_output_tokens;
    if let Some(usage) = token_usage_label(&usage) {
        tokens.push(usage);
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
    // A planner's design and proposed files live in its authoring tree, not the workspace.
    let files = changes
        .paths()
        .iter()
        .filter(|path| !crate::plan::is_authoring_path(path))
        .count();
    if files > 0 {
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

/// Format an exchange's successful and total tool calls with their union execution time.
///
/// The Thought heading, the Agents summary, and running-agent status rows share this label so a
/// child's tool progress reads the same wherever it appears.
pub(super) fn exchange_tool_label(interaction: &Exchange, now_ms: i64) -> Option<String> {
    // The heading measures the same occupied intervals its Tools groups show, so the two agree.
    let calls = interaction
        .tools()
        .map(|(_, tool)| tool)
        .collect::<Vec<_>>();
    tool_count_label(
        ToolTally::from(interaction.tool_counter.clone()),
        tool_group_elapsed(&calls, interaction.tool_instant(now_ms)),
    )
}

/// Count a set of tool calls the way every tool summary reports them.
#[derive(Clone, Copy, Debug, Default)]
pub(crate) struct ToolTally {
    pub(crate) successful: usize,
    pub(crate) denied: usize,
    pub(crate) total: usize,
}

impl From<crate::exchange::ToolCounter> for ToolTally {
    fn from(counter: crate::exchange::ToolCounter) -> Self {
        Self {
            successful: counter.completed,
            denied: counter.denied,
            total: counter.total,
        }
    }
}

/// Format `successful/total tools`, the denied count when any, and the tool duration when known.
///
/// Durations use the same formatter as the Tools group label, so a heading and its group never
/// disagree on units or spacing.
pub(crate) fn tool_count_label(tally: ToolTally, tool_ms: Option<u64>) -> Option<String> {
    (tally.total > 0).then(|| tool_tally_label(tally, tool_ms, " tools"))
}

/// The one formatter behind every tool summary: `successful/total{noun}`, then the denied
/// count when any, then the occupied duration when known. The Thought heading and its Tools
/// group differ only in `noun`, so their counts and durations cannot drift apart.
fn tool_tally_label(tally: ToolTally, tool_ms: Option<u64>, noun: &str) -> String {
    let mut label = format!("{}/{}{noun}", tally.successful, tally.total);
    if tally.denied > 0 {
        label.push_str(&format!(" · ({} denied)", tally.denied));
    }
    if let Some(duration) = tool_ms {
        label.push_str(&format!(" · {}", super::duration::duration_label(duration)));
    }
    label
}

/// Format token expenditure as `input (N% cached) -> output`, omitting unavailable counts.
fn token_usage_label(usage: &crate::provider::usage::TokenUsage) -> Option<String> {
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
        (Some(input), Some(output)) => Some(format!("{input} -> {}", token_display(output))),
        (Some(input), None) => Some(format!("in {input}")),
        (None, Some(output)) => Some(format!("out {}", token_display(output))),
        (None, None) => None,
    }
}

fn instruction_display_path(path: &str) -> String {
    use std::path::Path;

    let path = Path::new(path);
    if let Ok(working_directory) = std::env::current_dir()
        && let Ok(relative_path) = path.strip_prefix(&working_directory)
    {
        return relative_path.display().to_string();
    }
    for home_directory in ["HOME", "USERPROFILE"]
        .into_iter()
        .filter_map(std::env::var_os)
        .map(std::path::PathBuf::from)
    {
        if let Ok(relative_path) = path.strip_prefix(home_directory) {
            return format!("~{}{}", std::path::MAIN_SEPARATOR, relative_path.display());
        }
    }
    path.display().to_string()
}

#[cfg(test)]
pub(crate) mod tests {

    use super::project_at;

    use forge_buffer::width::WidthProfile;
    use serde_json::json;
    use std::collections::HashMap;

    use crate::{exchange::Exchange, timeline::TimelineEntry};

    /// Persist one instruction row per turn and show its full navigation path.
    #[test]
    fn instruction_reads_render_paths_and_preserve_navigation_after_reload() -> anyhow::Result<()> {
        let mut exchange = crate::storage::test::running_exchange("instruction-timeline");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let read = forge_provider_api::InstructionRead {
            path: "/fixture/nested/AGENTS.md".into(),
            file_name: "AGENTS.md".into(),
        };
        let event = crate::provider::test_support::native_event(
            crate::provider::EventScope::Invocation {
                invocation: address.scope.clone(),
            },
            forge_provider_api::ProviderEvent::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event: forge_provider_api::TurnEvent::InstructionRead(read),
            },
        );
        assert!(exchange.observe_event(&event, 1)?);
        assert!(!exchange.observe_event(&event, 1)?);
        let exchange: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
        let mut display = crate::buffer::identity::DisplayRegistry::default();
        let projected = super::project(
            super::ProjectionSource::Exchange(&exchange),
            &WidthProfile::default(),
            2,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut display,
        )?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.contains(&format!(
                "Instruction · {}",
                super::instruction_display_path("/fixture/nested/AGENTS.md")
            )),
            "{text}"
        );
        assert_eq!(
            exchange
                .node_list
                .iter()
                .filter(|node| matches!(
                    node,
                    crate::exchange::ExchangeNode::InstructionRead { .. }
                ))
                .count(),
            1
        );
        assert!(projected.action.values().any(|action| matches!(action, super::TranscriptAction::File { path, .. } if path == "/fixture/nested/AGENTS.md")));
        Ok(())
    }

    /// A planning exchange's heading counts the workspace files it changed, not its plan drafts.
    ///
    /// The planner writes its design and proposed files in the plan's authoring tree under the
    /// Harness state directory, so a plan draft read `3 files changed` while the workspace had
    /// no change at all.
    #[test]
    fn exchange_heading_counts_workspace_files_only() -> anyhow::Result<()> {
        use forge_provider_api::{
            InspectableArguments, ItemCompletion, ItemEvent, ItemStart, ToolCompletion,
            ToolDescriptor, ToolItemEvent,
        };
        let mut exchange = crate::storage::test::running_exchange("plan-draft");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let state = crate::storage::state::directory(std::path::Path::new("D:/forge-data"));
        let authoring = state
            .join("plan-workspaces")
            .join(exchange.session_id.as_str())
            .join("plan");
        let paths = [
            authoring.join("docs").join("overview.md"),
            authoring.join("files").join("src").join("order.ts"),
            std::path::PathBuf::from("src/order.ts"),
        ];
        for (index, path) in paths.iter().enumerate() {
            crate::turn::test_support::tool(
                &mut exchange,
                &address,
                &format!("change-{index}"),
                ItemStart::FileChange(ToolDescriptor {
                    semantics: Default::default(),
                    name: "edit".into(),
                    title: "edit".into(),
                    arguments: InspectableArguments::new(json!({})),
                }),
                [
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                    ItemEvent::Tool {
                        event: ToolItemEvent::Changes(crate::provider::ProviderChangeSet {
                            file: vec![crate::provider::ProviderFileChange {
                                path: path.display().to_string(),
                                move_path: None,
                                kind: crate::provider::ProviderChangeKind::Update,
                                diff: "+changed".into(),
                            }],
                        }),
                    },
                    ItemEvent::Completed(ItemCompletion::FileChange(ToolCompletion {
                        outcome: forge_provider_api::ToolOutcome::Succeeded,
                        descriptor: None,
                        output: None,
                        changes: Default::default(),
                    })),
                ],
                1,
            )?;
        }
        let heading = super::exchange_metrics_summary(&exchange, 2, "● Thought 0s".into());
        assert!(heading.contains("│ 1 file changed"), "{heading}");
        Ok(())
    }

    /// Retain internal evidence for replay without duplicating the visible check categories.
    #[test]
    fn check_result_inputs_do_not_add_timeline_rows() -> anyhow::Result<()> {
        let mut exchange = crate::storage::test::running_exchange("check-timeline");
        let result = forge_provider_api::forge::CheckResult {
            run_id: "run".into(),
            exchange_id: exchange.id.to_string(),
            state: forge_provider_api::forge::CheckResultState::Passed,
            complete: true,
            checks: Vec::new(),
        };
        exchange.append_input(
            crate::exchange::InputIntent::User {
                content: crate::input::UserInputContent::CheckResult {
                    result: Box::new(result),
                    additional_results: Vec::new(),
                },
            },
            "Check run passed".into(),
            1,
        )?;
        exchange.append_input(
            crate::exchange::InputIntent::user_text("Keep this steering message".into()),
            "Keep this steering message".into(),
            2,
        )?;
        let mut display = crate::buffer::identity::DisplayRegistry::default();
        let projected = super::project(
            super::ProjectionSource::Exchange(&exchange),
            &WidthProfile::default(),
            3,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut display,
        )?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(!text.contains("Check run passed"), "{text}");
        assert!(text.contains("Keep this steering message"), "{text}");
        assert_eq!(
            exchange
                .node_list
                .iter()
                .filter(|node| matches!(node, crate::exchange::ExchangeNode::ExchangeInput { .. }))
                .count(),
            2
        );
        Ok(())
    }

    /// Keep live output behind each tool's expansion across updates and reflow.
    #[test]
    fn live_tool_output_requires_individual_expansion() -> anyhow::Result<()> {
        use forge_buffer::node::{NodeDisplay, NodeKind};
        use forge_provider_api::{ItemEvent, ItemStart, TextUpdate, ToolItemEvent};

        for agent in ["primary", "child"] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"live-output", "session_id":"session", "agent_id":agent, "ordinal":1,
                "prompt":"Inspect", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            exchange.resume(0)?;
            let address = crate::turn::test_support::address(&exchange, "turn");
            exchange.start_turn(address.clone(), 0)?;
            for id in ["first", "latest"] {
                crate::turn::test_support::tool(
                    &mut exchange,
                    &address,
                    id,
                    ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
                        semantics: Default::default(),
                        name: "command".into(),
                        title: format!("Run {id}"),
                        arguments: forge_provider_api::InspectableArguments::new(json!({})),
                    }),
                    [
                        ItemEvent::Tool {
                            event: ToolItemEvent::ExecutionStarted,
                        },
                        ItemEvent::Tool {
                            event: ToolItemEvent::Output(TextUpdate::Replace(format!(
                                "{id}-one\n{id}-two\n{id}-three\n{id}-four\n{id}-five"
                            ))),
                        },
                    ],
                    1,
                )?;
            }
            let mut display = crate::buffer::identity::DisplayRegistry::default();
            let mut expansion = HashMap::new();
            for columns in [80, 24] {
                let width = WidthProfile {
                    columns,
                    ..WidthProfile::default()
                };
                let collapsed = super::project(
                    super::ProjectionSource::Exchange(&exchange),
                    &width,
                    2,
                    false,
                    &expansion,
                    &HashMap::new(),
                    &mut display,
                )?;
                let tools = collapsed
                    .entry
                    .block
                    .iter()
                    .filter_map(|block| {
                        block
                            .metadata
                            .node
                            .as_ref()
                            .filter(|node| node.kind == NodeKind::Tool)
                    })
                    .collect::<Vec<_>>();
                assert_eq!(tools.len(), 2);
                assert!(
                    tools
                        .iter()
                        .all(|node| node.display == NodeDisplay::Heading)
                );
                assert!(collapsed.entry.block.iter().all(|block| {
                    !block
                        .text
                        .wire_rows()
                        .iter()
                        .any(|row| row.contains("-one") || row.contains("-five"))
                }));
                let latest = tools.last().unwrap().id.0.clone();
                let group = collapsed.tool.values().next().unwrap().group.clone();
                let group_node = collapsed
                    .entry
                    .block
                    .iter()
                    .filter_map(|block| block.metadata.node.as_ref())
                    .find(|node| node.kind == NodeKind::ToolGroup)
                    .unwrap();
                assert_eq!(group_node.display, NodeDisplay::Heading);
                let visible = crate::buffer::sections::preview(collapsed.entry)?;
                let headings = visible
                    .block
                    .iter()
                    .filter_map(|block| {
                        block
                            .metadata
                            .node
                            .as_ref()
                            .filter(|node| node.kind == NodeKind::Tool)
                    })
                    .collect::<Vec<_>>();
                assert_eq!(
                    headings.len(),
                    0,
                    "the live group must keep its tool headings folded"
                );
                expansion.insert(group.clone(), true);
                expansion.insert(latest.clone(), true);
                let expanded = super::project(
                    super::ProjectionSource::Exchange(&exchange),
                    &width,
                    2,
                    false,
                    &expansion,
                    &HashMap::new(),
                    &mut display,
                )?;
                let text = expanded
                    .entry
                    .block
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .collect::<Vec<_>>()
                    .join("\n");
                assert!(
                    text.contains("latest-one") && text.contains("latest-five"),
                    "{text}"
                );
                assert!(
                    !text.contains("first-one"),
                    "expanding one tool exposed another: {text}"
                );
                let renderer = super::TranscriptRenderer::new(&width)?;
                for (call_id, output) in &expanded.tool {
                    assert_eq!(
                        renderer
                            .tool_body(call_id, output, false, 0)?
                            .text
                            .row_count(),
                        0
                    );
                    assert_eq!(
                        renderer
                            .tool_hidden(call_id, output, false)?
                            .text
                            .row_count(),
                        0
                    );
                }
                expansion.insert(latest, false);
                expansion.remove(&group);
            }
            let update = crate::turn::test_support::item_event(
                &address,
                "latest",
                ItemEvent::Tool {
                    event: ToolItemEvent::Output(TextUpdate::Append("\nnew-output".into())),
                },
            );
            exchange.observe_event(&update, 3)?;
            let collapsed = super::project(
                super::ProjectionSource::Exchange(&exchange),
                &WidthProfile::default(),
                3,
                false,
                &expansion,
                &HashMap::new(),
                &mut display,
            )?;
            assert!(collapsed.entry.block.iter().all(|block| {
                !block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("new-output") || row.contains("latest-five"))
            }));
        }
        Ok(())
    }

    /// A call that never executed shows no duration on its row or in its group, on every
    /// provider.
    ///
    /// Codex announces a command as running before it asks to run it, so a denied Codex call
    /// carried an interval (`0.1s`) while the same denial on Claude Code, which never starts the
    /// call, showed `—`.
    #[test]
    fn denied_calls_have_no_duration() {
        use crate::turn::{ToolCall, ToolInstant, ToolState};
        let denied = ToolCall::projection(
            "denied".into(),
            "command".into(),
            "sleep 4sec".into(),
            ToolState::Denied,
            Some(0),
            Some(92),
        );
        assert_eq!(denied.elapsed_ms(ToolInstant::wall(1_000)), None);
        assert_eq!(
            super::tool_group_label([&denied], ToolInstant::wall(1_000)),
            "▸ Tools · 0/1 · (1 denied)",
        );
    }

    /// Count concurrent and sequential tool intervals without charging overlaps or idle gaps.
    #[test]
    fn tool_group_duration_counts_occupied_time_once() {
        use crate::turn::{ToolCall, ToolInstant, ToolState};

        for (intervals, expected) in [
            (vec![(0, 1_000), (0, 2_000), (0, 3_000)], "3s"),
            (vec![(0, 1_000), (1_000, 2_000), (2_000, 3_000)], "3s"),
            (vec![(0, 1_000), (3_000, 4_000)], "2s"),
            (vec![(2_000, 4_000), (0, 3_000), (1_000, 2_000)], "4s"),
        ] {
            let calls: Vec<_> = intervals
                .into_iter()
                .enumerate()
                .map(|(id, (start, end))| {
                    ToolCall::projection(
                        id.to_string(),
                        "command".into(),
                        "Inspect".into(),
                        ToolState::Completed,
                        Some(start),
                        Some(end),
                    )
                })
                .collect();
            assert_eq!(
                super::tool_group_label(&calls, ToolInstant::wall(10_000),),
                format!("▸ Tools · {0}/{0} · {expected}", calls.len()),
            );
        }
    }

    #[test]
    fn tool_group_summary_distinguishes_failed_and_denied() {
        use crate::turn::{ToolCall, ToolInstant, ToolState};
        let calls: Vec<_> = [ToolState::Completed, ToolState::Failed, ToolState::Denied]
            .into_iter()
            .enumerate()
            .map(|(index, state)| {
                ToolCall::projection(
                    index.to_string(),
                    "command".into(),
                    "Inspect".into(),
                    state,
                    None,
                    None,
                )
            })
            .collect();
        assert_eq!(
            super::tool_group_label(&calls, ToolInstant::wall(0)),
            "▸ Tools · 1/3 · (1 denied)",
        );
        assert!(calls[1].failed);
        assert_eq!(calls[2].state(), ToolState::Denied);
        assert!(!calls[0].failed);
    }

    /// Explain each stopped call on its own row, and count a denial the same way in the Thought
    /// heading and its Tools group.
    ///
    /// A call the user cancelled used to show only an interrupted marker with no line saying the
    /// turn stopped, and a call interrupted before it finished had no line at all. The heading
    /// counted denials as unsuccessful calls while the group named them, so the two disagreed.
    #[test]
    fn stopped_and_denied_calls_explain_themselves_and_agree_in_every_summary() -> anyhow::Result<()>
    {
        use crate::provider::ToolStatus;
        use forge_provider_api::ItemStart;

        let outcomes = [
            ToolStatus::Succeeded,
            ToolStatus::Denied,
            ToolStatus::Interrupted,
            ToolStatus::Cancelled,
        ];
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"stopped-calls", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        for (index, status) in outcomes.iter().enumerate() {
            let start = ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "command".into(),
                title: format!("Run {index}"),
                arguments: forge_provider_api::InspectableArguments::new(json!({})),
            });
            let updates =
                crate::turn::test_support::tool_updates(&start, *status, None, Default::default());
            crate::turn::test_support::tool(
                &mut exchange,
                &address,
                &index.to_string(),
                start,
                updates,
                1,
            )?;
        }
        exchange
            .approval_resolution
            .push(crate::provider::PermissionApprovalEvent::Cancelled(
                crate::provider::PermissionApprovalCancellation {
                    request_id: crate::provider::ApprovalRequestId::new("cancelled")?,
                    scope: crate::provider::ApprovalScope::Item {
                        thread_id: address.thread_id.clone(),
                        turn_id: address.turn_id.clone(),
                        item_id: forge_provider_api::ItemId::new("3")?,
                    },
                    reason: crate::provider::ApprovalCancellationReason::UserCancelled,
                    cancelled_at_ms: 1,
                },
            ));
        exchange.rebuild_tool_counter();
        let project = |expansion: &HashMap<String, bool>| {
            super::project(
                super::ProjectionSource::Exchange(&exchange),
                &WidthProfile::default(),
                2,
                false,
                expansion,
                &HashMap::new(),
                &mut crate::buffer::identity::DisplayRegistry::default(),
            )
        };
        let folded = project(&HashMap::new())?;
        let expansion = folded
            .entry
            .block
            .iter()
            .filter_map(|block| block.metadata.node.as_ref())
            .map(|node| (node.id.0.clone(), true))
            .collect::<HashMap<_, _>>();
        let rows = project(&expansion)?
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .map(str::to_owned)
            .collect::<Vec<_>>();
        for line in [
            "Interrupted before it finished.",
            "Cancelled by you; the turn stopped.",
        ] {
            assert!(
                rows.iter().any(|row| row.contains(line)),
                "missing {line:?}: {rows:#?}"
            );
        }
        let heading = super::exchange_tool_label(&exchange, 2).expect("tools are counted");
        assert!(heading.starts_with("1/4 tools · (1 denied)"), "{heading}");
        let group = rows
            .iter()
            .find(|row| row.contains("Tools ·"))
            .expect("the group has a heading");
        assert!(group.contains("Tools · 1/4 · (1 denied)"), "{group}");
        Ok(())
    }

    /// A file change that policy refused counts as denied in its Edits group, not unsuccessful.
    ///
    /// A refused change never ran, and the Edits group used to count it with failed changes, so a
    /// Harness policy denial read as an edit that had broken.
    #[test]
    fn denied_file_change_counts_as_denied_in_its_edits_group() -> anyhow::Result<()> {
        use crate::provider::ToolStatus;
        use forge_provider_api::ItemStart;

        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"denied-change", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Edit", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let start = ItemStart::FileChange(forge_provider_api::ToolDescriptor {
            semantics: Default::default(),
            name: "apply_patch".into(),
            title: "Edit notes.txt".into(),
            arguments: forge_provider_api::InspectableArguments::new(json!({})),
        });
        let updates = crate::turn::test_support::tool_updates(
            &start,
            ToolStatus::Denied,
            None,
            Default::default(),
        );
        crate::turn::test_support::tool(&mut exchange, &address, "change", start, updates, 1)?;
        exchange.rebuild_tool_counter();
        let rows = project_rows(&crate::timeline::TimelineEntry::Exchange {
            id: "denied-change".into(),
            created_at_ms: 0,
            exchange: Box::new(exchange),
            agent_by_id: HashMap::new(),
        })?;
        let edits = rows
            .iter()
            .find(|row| row.contains("Edits ·"))
            .unwrap_or_else(|| panic!("no Edits group: {rows:#?}"));
        assert!(edits.contains("1 denied"), "{edits}");
        assert!(!edits.contains("unsuccessful"), "{edits}");
        Ok(())
    }

    /// Mark each tool row by its own outcome, so a stopped call never reads as a success.
    #[test]
    fn tool_rows_mark_each_terminal_outcome() -> anyhow::Result<()> {
        use crate::provider::ToolStatus;
        use forge_buffer::node::NodeKind;
        use forge_provider_api::ItemStart;

        let outcomes = [
            (ToolStatus::Succeeded, "ForgeHarnessToolSuccess"),
            (ToolStatus::Failed, "ForgeHarnessToolFailure"),
            (ToolStatus::Denied, "ForgeHarnessToolDenied"),
            (ToolStatus::Cancelled, "ForgeHarnessToolInterrupted"),
            (ToolStatus::Interrupted, "ForgeHarnessToolInterrupted"),
            // The provider reports a call its tool deadline stopped as interrupted; the host's
            // timeout record marks it failed.
            (ToolStatus::Interrupted, "ForgeHarnessToolFailure"),
        ];
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"tool-outcomes", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        for (index, (status, _)) in outcomes.iter().enumerate() {
            let start = ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "command".into(),
                title: format!("Run {index}"),
                arguments: forge_provider_api::InspectableArguments::new(json!({})),
            });
            let updates =
                crate::turn::test_support::tool_updates(&start, *status, None, Default::default());
            crate::turn::test_support::tool(
                &mut exchange,
                &address,
                &index.to_string(),
                start,
                updates,
                1,
            )?;
        }
        exchange
            .tool_timeout
            .push(crate::provider::tool_timeout::ToolTimeoutRecord {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                item_id: forge_provider_api::ItemId::new((outcomes.len() - 1).to_string())?,
                title: format!("Run {}", outcomes.len() - 1),
                timeout_ms: 600_000,
            });
        let projected = super::project(
            super::ProjectionSource::Exchange(&exchange),
            &WidthProfile::default(),
            2,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
        )?;
        let markers = projected
            .entry
            .block
            .iter()
            .filter(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == NodeKind::Tool)
            })
            .map(|block| {
                block
                    .metadata
                    .decoration
                    .iter()
                    .find(|decoration| decoration.capture.starts_with("ForgeHarnessTool"))
                    .map(|decoration| decoration.capture.as_str())
            })
            .collect::<Vec<_>>();
        assert_eq!(
            markers,
            outcomes
                .iter()
                .map(|(_, capture)| Some(*capture))
                .collect::<Vec<_>>()
        );
        Ok(())
    }

    /// Freeze completed intervals and measure live overlap on the approval-paused clock.
    #[test]
    fn tool_group_duration_uses_active_clock_for_live_and_completed_calls() {
        use crate::turn::{ToolCall, ToolInstant, ToolState};

        let mut completed = ToolCall::projection(
            "completed".into(),
            "command".into(),
            "Inspect".into(),
            ToolState::Completed,
            Some(1_000),
            Some(10_000),
        );
        completed.started_active_ms = Some(0);
        completed.completed_active_ms = Some(1_000);
        let mut running = ToolCall::projection(
            "running".into(),
            "command".into(),
            "Inspect".into(),
            ToolState::Running,
            Some(1_500),
            None,
        );
        running.started_active_ms = Some(500);
        let calls = [completed, running];
        assert_eq!(
            super::tool_group_label(
                &calls,
                ToolInstant {
                    wall_ms: 20_000,
                    active_ms: Some(2_000),
                },
            ),
            "▸ Tools · 1/2 · 2s",
        );
    }

    /// Use one wall clock for mixed records without adding failure labels.
    #[test]
    fn tool_group_duration_handles_wall_fallback_and_missing_endpoints() {
        use crate::turn::{ToolCall, ToolInstant, ToolState};

        let mut active = ToolCall::projection(
            "active".into(),
            "command".into(),
            "Inspect".into(),
            ToolState::Completed,
            Some(1_000),
            Some(3_000),
        );
        active.started_active_ms = Some(0);
        active.completed_active_ms = Some(100);
        let wall = ToolCall::projection(
            "wall".into(),
            "command".into(),
            "Inspect".into(),
            ToolState::Failed,
            Some(2_000),
            Some(4_000),
        );
        let mut calls = [active, wall];
        let now = ToolInstant {
            wall_ms: 10_000,
            active_ms: Some(5_000),
        };
        assert_eq!(super::tool_group_label(&calls, now), "▸ Tools · 1/2 · 3s",);
        calls[1].started_at_ms = None;
        assert_eq!(
            super::tool_group_label(&calls, now),
            "▸ Tools · 1/2 · 100ms",
        );
    }

    /// Announcing a tool cannot erase elapsed execution from the rest of its group.
    #[test]
    fn tool_group_duration_survives_unstarted_calls() {
        use crate::turn::{ToolCall, ToolInstant, ToolState};
        let finished = ToolCall::projection(
            "finished".into(),
            "tool".into(),
            "Inspect".into(),
            ToolState::Completed,
            Some(100),
            Some(1_100),
        );
        let waiting = ToolCall::projection(
            "waiting".into(),
            "tool".into(),
            "Inspect".into(),
            ToolState::Running,
            None,
            None,
        );
        assert_eq!(
            super::tool_group_label([&finished, &waiting], ToolInstant::wall(2_000)),
            "▸ Tools · 1/2 · 1s"
        );
        assert_eq!(
            super::tool_group_label([&waiting], ToolInstant::wall(2_000)),
            "▸ Tools · 0/1"
        );
    }

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
            let event = crate::turn::test_support::message_completed(
                &address,
                "response",
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
                    let question = text.find("Answered 1 question").unwrap();
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
            let event = crate::turn::test_support::message_completed(
                &address,
                "response",
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

    /// Show the output and speed of the turn that ran again when the turn it replaced was stopped
    /// before its output count was known.
    ///
    /// A compaction or a configuration change stops a turn and runs it again in the same
    /// exchange. The stopped turn's unknown output made the exchange total unknown, so the
    /// heading dropped the rerun's output and speed although the rerun reported both.
    #[test]
    fn a_turn_that_ran_again_shows_the_output_of_its_rerun() -> anyhow::Result<()> {
        use forge_provider_api::{TurnUsageReport, UsageReportId};
        let mut exchange = crate::storage::test::running_exchange("rerun");
        let stopped = crate::turn::test_support::address(&exchange, "stopped");
        let rerun = crate::turn::test_support::address(&exchange, "rerun");
        for (address, report_id, observed_at_ms, output, duration_ms) in [
            (&stopped, "stopped-call", 1_000, None, 4_000),
            (&rerun, "rerun-call", 6_000, Some(40), 2_000),
        ] {
            exchange.start_turn(address.clone(), observed_at_ms - 1)?;
            exchange.observe_event(
                &crate::provider::test_support::native_event(
                    crate::provider::EventScope::Invocation {
                        invocation: address.scope.clone(),
                    },
                    forge_provider_api::ProviderEvent::Turn {
                        thread_id: address.thread_id.clone(),
                        turn_id: address.turn_id.clone(),
                        event: forge_provider_api::TurnEvent::Usage(TurnUsageReport::accounting(
                            UsageReportId::new(report_id)?,
                            observed_at_ms,
                            Some(forge_provider_api::usage::TokenUsage {
                                input: Some(150),
                                output,
                                ..Default::default()
                            }),
                            Some(duration_ms),
                        )?),
                    },
                ),
                observed_at_ms,
            )?;
            if output.is_none() {
                exchange.settle_running_turns(
                    crate::exchange::ExchangeOutcome::Interrupted,
                    observed_at_ms,
                )?;
            }
        }

        let heading = project_rows(&TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 0,
            exchange: exchange.clone().into(),
            agent_by_id: HashMap::new(),
        })?
        .join("\n");
        assert!(heading.contains("300 -> 40 · 20 tps"), "{heading}");
        Ok(())
    }

    /// Keep a call's throughput when its end arrives as an endpoint no call identity claims.
    ///
    /// Copilot reports a model call's tokens and duration under the call's identity, then ends
    /// the call with an event that carries neither. Counting that endpoint as a second call
    /// without timing erased the first call's duration, so the heading lost its rate.
    #[test]
    fn throughput_survives_a_call_end_without_call_identity() -> anyhow::Result<()> {
        use forge_provider_api::{ModelCallId, TurnUsageReport, UsageReportId};
        let mut exchange = crate::storage::test::running_exchange("throughput");
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let usage = |report| {
            crate::provider::test_support::native_event(
                crate::provider::EventScope::Invocation {
                    invocation: address.scope.clone(),
                },
                forge_provider_api::ProviderEvent::Turn {
                    thread_id: address.thread_id.clone(),
                    turn_id: address.turn_id.clone(),
                    event: forge_provider_api::TurnEvent::Usage(report),
                },
            )
        };
        let heading = |exchange: &Exchange| -> anyhow::Result<String> {
            Ok(project_rows(&TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange: exchange.clone().into(),
                agent_by_id: HashMap::new(),
            })?
            .join("\n"))
        };
        exchange.observe_event(
            &usage(TurnUsageReport::call(
                UsageReportId::new("call-1")?,
                Some(ModelCallId::new("call-1")?),
                1_000,
                Some(forge_provider_api::usage::TokenUsage {
                    output: Some(6),
                    ..Default::default()
                }),
                Some(2_000),
            )?),
            1_000,
        )?;
        let reported = heading(&exchange)?;
        assert!(reported.contains("· 3 tps"), "{reported}");
        exchange.observe_event(
            &usage(TurnUsageReport::interval(
                UsageReportId::new("copilot:call-end:event")?,
                None,
                1_060,
                None,
                Some(1_060),
            )?),
            1_060,
        )?;
        let ended = heading(&exchange)?;
        assert!(ended.contains("· 3 tps"), "{ended}");
        Ok(())
    }

    /// A native user item echoing a Forge submission must not repeat the admitted prompt row.
    #[test]
    fn native_echo_of_a_submission_renders_one_prompt_row() -> anyhow::Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"echo", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"What is this repo?", "opening":{"kind":"user","content":{"kind":"user_text","text":"What is this repo?"}}, "state":"running", "created_at_ms":0,
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

    /// Steering waits outside the transcript, anchors never render, and unconfirmed rows keep
    /// their text without the marker that response reveal anchors on.
    #[test]
    fn steering_rows_follow_their_consumption_state() -> anyhow::Result<()> {
        use anyhow::Context as _;
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"steered", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Summarize the repo", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let early = forge_provider_api::SubmissionId::try_from("early".to_owned())?;
        exchange.observe_event(
            &crate::turn::test_support::item_event(
                &address,
                "early-echo",
                forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::UserMessage(
                    forge_provider_api::UserMessageStart {
                        text: "Anchored before acknowledgement".into(),
                        submission_id: Some(early),
                    },
                )),
            ),
            1,
        )?;
        exchange.append_acknowledged_input(
            &address.scope,
            Some("waiting".into()),
            Some(forge_provider_api::SubmissionId::try_from(
                "waiting".to_owned(),
            )?),
            crate::exchange::InputIntent::user_text("Prefer the short form".into()),
            "Prefer the short form".into(),
            2,
        )?;
        let render = |exchange: &Exchange| -> anyhow::Result<super::ProjectedEntry> {
            project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: 0,
                    exchange: exchange.clone().into(),
                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                3,
            )
        };
        let text = |projected: &super::ProjectedEntry| {
            projected
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n")
        };
        let live = render(&exchange)?;
        assert!(
            !text(&live).contains("Prefer the short form"),
            "{}",
            text(&live)
        );
        assert!(!text(&live).contains("Anchored before"), "{}", text(&live));

        exchange.append_input(
            crate::exchange::InputIntent::user_text("Use a short title".into()),
            "Use a short title".into(),
            3,
        )?;
        let consumed = render(&exchange)?;
        assert!(text(&consumed).contains("Use a short title"));
        assert!(!text(&consumed).contains("Steering: Use a short title"));
        let row = consumed
            .entry
            .block
            .iter()
            .find(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("Use a short title"))
            })
            .context("consumed steering row")?;
        assert!(
            row.metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessPrompt")
        );

        exchange.finish(crate::exchange::ExchangeOutcome::Interrupted, 4)?;
        let settled = render(&exchange)?;
        assert!(
            text(&settled).contains("Steering (not confirmed read): Prefer the short form"),
            "{}",
            text(&settled)
        );
        assert!(
            !text(&settled).contains("Anchored before"),
            "{}",
            text(&settled)
        );
        let row = settled
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "waiting:prompt")
            .context("unconfirmed row")?;
        let marker = row
            .metadata
            .layout
            .as_ref()
            .and_then(|layout| layout.marker.as_ref())
            .context("unconfirmed row marker")?;
        assert_eq!(marker.capture, "ForgeHarnessUnconfirmedPrompt");
        assert!(
            row.metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "ForgeHarnessPrompt"),
            "unconfirmed input lost its prompt styling"
        );
        assert!(
            settled.prompt.contains(&row.id),
            "prompt navigation skipped the row"
        );
        Ok(())
    }

    /// Project one timeline entry and return its transcript rows.
    pub(crate) fn project_rows(entry: &TimelineEntry) -> anyhow::Result<Vec<String>> {
        Ok(project_at(entry, &WidthProfile::default(), 6_000)?
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .map(str::to_owned)
            .collect())
    }

    /// Project one timeline entry with every fold open, as a reader who expanded each row sees it.
    pub(crate) fn project_rows_expanded(entry: &TimelineEntry) -> anyhow::Result<Vec<String>> {
        let mut expansion = HashMap::new();
        loop {
            let projected = super::project_at_with_separator(
                super::ProjectionSource::Entry(entry),
                &WidthProfile::default(),
                6_000,
                false,
                &expansion,
                &HashMap::new(),
                &mut crate::buffer::identity::DisplayRegistry::default(),
            )?;
            let opened = expansion.len();
            for node in projected
                .entry
                .block
                .iter()
                .filter_map(|block| block.metadata.node.as_ref())
            {
                expansion.insert(node.id.0.clone(), true);
            }
            // Opening a fold can reveal folds nested inside it, so stop once nothing new appears.
            if expansion.len() == opened {
                return Ok(projected
                    .entry
                    .block
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .map(str::to_owned)
                    .collect());
            }
        }
    }

    /// Build a delegated child record for agent presentation fixtures.
    pub(crate) fn child_agent(id: &str, task_name: &str) -> crate::agent::Agent {
        serde_json::from_value(json!({
            "id": id,
            "session_id": "session",
            "provider_thread_id": null,
            "definition": "code-explorer",
            "nickname": null,
            "task_name": task_name,
            "task_path": format!("/root/{task_name}"),
            "source": "model",
            "selection": null,
            "inbox": [],
            "state": "ready",
            "created_at_ms": 10,
            "updated_at_ms": 10,
        }))
        .expect("child agent fixture")
    }

    /// Build a parent exchange that delegated one child per task name, returning the timeline
    /// entry and each child's exchange identity in delegation order.
    pub(crate) fn delegating_exchange(
        task_names: &[&str],
        child_outcome: &[Option<crate::exchange::ExchangeOutcome>],
    ) -> anyhow::Result<TimelineEntry> {
        let mut parent = crate::storage::test::running_exchange("parent");
        let address = crate::turn::test_support::address(&parent, "turn");
        parent.start_turn(address, 0)?;
        let mut agent_by_id = HashMap::new();
        for (index, (task_name, outcome)) in task_names.iter().zip(child_outcome).enumerate() {
            let run_id = format!("child-{index}");
            assert!(parent.append_delegation(&run_id, &format!("call-{index}"), "explore", 40)?);
            let Some(crate::exchange::ExchangeNode::AgentReference { agent: delegation }) =
                parent.node_list.last()
            else {
                unreachable!("delegation appends a reference")
            };
            let mut child = crate::storage::test::running_exchange(&delegation.child_exchange_id);
            child.created_at_ms = 50 + index as i64;
            if let Some(outcome) = outcome {
                child.finish(*outcome, 5_000)?;
            }
            agent_by_id.insert(
                run_id.clone(),
                TimelineEntry::AgentLifecycle {
                    id: run_id.clone(),
                    created_at_ms: 50 + index as i64,
                    run: Box::new(child_agent(&run_id, task_name)),
                    exchange: vec![child],
                    agent: Vec::new(),
                },
            );
        }
        Ok(TimelineEntry::Exchange {
            id: parent.id.clone(),
            created_at_ms: parent.created_at_ms,
            exchange: parent.into(),
            agent_by_id,
        })
    }

    /// Record each delegation as one static event and summarize only finished children.
    #[test]
    fn delegations_render_spawn_events_and_summarize_finished_children() -> anyhow::Result<()> {
        use crate::exchange::ExchangeOutcome;
        let entry = delegating_exchange(
            &["explorer-done", "explorer-running", "explorer-cancelled"],
            &[
                Some(ExchangeOutcome::Complete),
                None,
                Some(ExchangeOutcome::Cancelled),
            ],
        )?;
        let projected = project_at(&entry, &WidthProfile::default(), 6_000)?;
        let rows = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>();
        for task_name in ["explorer-done", "explorer-running", "explorer-cancelled"] {
            assert!(
                rows.contains(&format!("◇ Spawned {task_name}").as_str()),
                "{rows:#?}"
            );
        }
        assert!(rows.contains(&"Agents · 1/3 completed · 4s"), "{rows:#?}");
        assert!(
            rows.iter()
                .any(|row| row.starts_with("✓ explorer-done · Ran "))
        );
        assert!(
            rows.iter()
                .any(|row| row.starts_with("✗ explorer-cancelled · Cancelled "))
        );
        assert!(
            !rows
                .iter()
                .any(|row| row.contains("explorer-running") && !row.starts_with("◇ Spawned")),
            "a running child appears only as its spawn event: {rows:#?}"
        );
        let spawn = rows
            .iter()
            .position(|row| *row == "◇ Spawned explorer-done")
            .unwrap();
        let summary = rows
            .iter()
            .position(|row| *row == "Agents · 1/3 completed · 4s")
            .unwrap();
        assert!(spawn < summary, "spawn events precede the exchange summary");
        let agent_block = projected
            .entry
            .block
            .iter()
            .find(|block| block.id.0 == "parent:agents")
            .expect("agents root block");
        let node = agent_block
            .metadata
            .node
            .as_ref()
            .expect("agents root node");
        assert_eq!(node.id.0, "parent:agents");
        let opened = projected
            .action
            .values()
            .filter_map(|action| match action {
                super::TranscriptAction::Agent { run_id } => Some(run_id.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();
        for run_id in ["child-0", "child-1", "child-2"] {
            assert!(opened.contains(&run_id), "{opened:?}");
        }
        Ok(())
    }

    /// A child the parent ran twice in one exchange is summarized once, by its latest run. The
    /// summary once listed it twice with the two runs' different results.
    #[test]
    fn agents_summary_names_a_child_run_twice_once() -> anyhow::Result<()> {
        use crate::exchange::ExchangeOutcome;
        let mut parent = crate::storage::test::running_exchange("parent");
        let address = crate::turn::test_support::address(&parent, "turn");
        parent.start_turn(address, 0)?;
        let mut child_exchange = Vec::new();
        for (index, completed_at_ms) in [(0, 2_000), (1, 5_000)] {
            assert!(parent.append_delegation(
                "child-0",
                &format!("call-{index}"),
                "explore",
                40
            )?);
            let Some(crate::exchange::ExchangeNode::AgentReference { agent: delegation }) =
                parent.node_list.last()
            else {
                unreachable!("delegation appends a reference")
            };
            let mut child = crate::storage::test::running_exchange(&delegation.child_exchange_id);
            child.created_at_ms = 50 + index;
            child.finish(ExchangeOutcome::Complete, completed_at_ms)?;
            child_exchange.push(child);
        }
        let entry = TimelineEntry::Exchange {
            id: parent.id.clone(),
            created_at_ms: parent.created_at_ms,
            agent_by_id: HashMap::from([(
                "child-0".to_owned(),
                TimelineEntry::AgentLifecycle {
                    id: "child-0".into(),
                    created_at_ms: 50,
                    run: Box::new(child_agent("child-0", "explorer")),
                    exchange: child_exchange,
                    agent: Vec::new(),
                },
            )]),
            exchange: parent.into(),
        };
        let rows = project_at(&entry, &WidthProfile::default(), 6_000)?
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .map(str::to_owned)
            .collect::<Vec<_>>();
        assert!(
            rows.iter()
                .any(|row| row.starts_with("Agents · 1/1 completed")),
            "{rows:#?}"
        );
        let leaves = rows
            .iter()
            .filter(|row| row.starts_with("✓ explorer · Ran "))
            .collect::<Vec<_>>();
        assert_eq!(leaves.len(), 1, "{rows:#?}");
        assert!(leaves[0].starts_with("✓ explorer · Ran 4s"), "{rows:#?}");
        Ok(())
    }

    /// A stop cancels every child of the exchange it stops while the status area still lists
    /// them, and that area closes only when the exchange settles. The Agents summary once gained
    /// the cancelled children as soon as the last of them settled, a frame before the status
    /// closed, so the transcript above moved twice. The cancelled children now join the summary
    /// when the exchange settles, and a child that ended before the stop stays as it was.
    #[test]
    fn a_stopping_exchange_summarizes_its_cancelled_children_once_it_settles() -> anyhow::Result<()>
    {
        use crate::exchange::ExchangeOutcome;
        enum Parent {
            Running,
            Stopping,
            Cancelled,
        }
        let agent_rows = |outcomes: &[Option<ExchangeOutcome>],
                          parent: Parent|
         -> anyhow::Result<Vec<String>> {
            let mut entry = delegating_exchange(&["explorer-first", "explorer-second"], outcomes)?;
            let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
                unreachable!("the fixture is an exchange entry")
            };
            match parent {
                Parent::Running => {}
                Parent::Stopping => {
                    exchange.collection = Some(crate::exchange::ExchangeCollection::Pending);
                    exchange.cancellation_requested = true;
                    exchange.pause(5_500);
                }
                Parent::Cancelled => exchange.finish(ExchangeOutcome::Cancelled, 5_500)?,
            }
            let projected = project_at(&entry, &WidthProfile::default(), 6_000)?;
            Ok(projected
                .entry
                .block
                .iter()
                .filter(|block| block.id.0.starts_with("parent:agents"))
                .flat_map(|block| block.text.wire_rows())
                .map(str::to_owned)
                .collect())
        };
        let partly = agent_rows(&[Some(ExchangeOutcome::Cancelled), None], Parent::Stopping)?;
        assert!(
            partly.is_empty(),
            "the summary showed a stop's first cancelled child: {partly:#?}"
        );
        let all_cancelled = [
            Some(ExchangeOutcome::Cancelled),
            Some(ExchangeOutcome::Cancelled),
        ];
        let collected = agent_rows(&all_cancelled, Parent::Stopping)?;
        assert!(
            collected.is_empty(),
            "the summary gained the cancelled children before the stop settled: {collected:#?}"
        );
        let settled = agent_rows(&all_cancelled, Parent::Cancelled)?;
        assert!(
            settled
                .first()
                .is_some_and(|row| row.starts_with("Agents · 0/2 completed")),
            "{settled:#?}"
        );
        assert_eq!(
            agent_rows(
                &[
                    Some(ExchangeOutcome::Complete),
                    Some(ExchangeOutcome::Cancelled)
                ],
                Parent::Stopping
            )?,
            agent_rows(&[Some(ExchangeOutcome::Complete), None], Parent::Running)?,
            "a child that finished before the stop must keep the summary it had"
        );
        Ok(())
    }

    /// A configuration change restarts the delegating turn and cancels its children, and the
    /// restarted turn may delegate again before the user stops it. The Agents summary withheld
    /// every cancelled child while any collection ran, so it blanked during the restart, and the
    /// later stop removed the children the restart had cancelled and shortened the heading's
    /// span. Each frame now keeps every row the one before it showed, and only the children the
    /// stop itself cancels wait for the exchange to settle.
    #[test]
    fn the_agents_summary_keeps_every_row_it_showed_through_a_restart_and_a_stop()
    -> anyhow::Result<()> {
        use crate::exchange::{ExchangeCollection, ExchangeOutcome};
        fn delegate(parent: &mut Exchange, run_id: &str, at_ms: i64) -> anyhow::Result<Exchange> {
            assert!(parent.append_delegation(
                run_id,
                &format!("call-{run_id}"),
                "explore",
                at_ms
            )?);
            let Some(crate::exchange::ExchangeNode::AgentReference { agent: delegation }) =
                parent.node_list.last()
            else {
                unreachable!("delegation appends a reference")
            };
            let mut child = crate::storage::test::running_exchange(&delegation.child_exchange_id);
            child.created_at_ms = at_ms;
            Ok(child)
        }
        fn frame(parent: &Exchange, children: &[(&str, &Exchange)], now_ms: i64) -> Vec<String> {
            let entry = TimelineEntry::Exchange {
                id: parent.id.clone(),
                created_at_ms: parent.created_at_ms,
                exchange: parent.clone().into(),
                agent_by_id: children
                    .iter()
                    .map(|(run_id, child)| {
                        (
                            (*run_id).to_owned(),
                            TimelineEntry::AgentLifecycle {
                                id: (*run_id).to_owned(),
                                created_at_ms: child.created_at_ms,
                                run: Box::new(child_agent(run_id, run_id)),
                                exchange: vec![(*child).clone()],
                                agent: Vec::new(),
                            },
                        )
                    })
                    .collect(),
            };
            project_at(&entry, &WidthProfile::default(), now_ms)
                .expect("the parent projects")
                .entry
                .block
                .iter()
                .filter(|block| block.id.0.starts_with("parent:agents"))
                .flat_map(|block| block.text.wire_rows())
                .map(str::to_owned)
                .collect()
        }
        let leaf_names = |rows: &[String]| -> Vec<String> {
            rows.iter()
                .filter(|row| row.starts_with("✓ ") || row.starts_with("✗ "))
                .filter_map(|row| row[4..].split(" · ").next().map(str::to_owned))
                .collect()
        };
        let heading_seconds = |rows: &[String]| -> u64 {
            rows.first()
                .and_then(|heading| heading.split(" │ ").next())
                .and_then(|heading| heading.rsplit(" · ").next())
                .and_then(|span| span.trim_end_matches('s').parse().ok())
                .unwrap_or_default()
        };

        let mut parent = crate::storage::test::running_exchange("parent");
        parent.start_turn(crate::turn::test_support::address(&parent, "first"), 0)?;
        let mut first = delegate(&mut parent, "explorer-1", 100)?;
        let mut second = delegate(&mut parent, "explorer-2", 200)?;
        // The restart cancels the first turn's children before it marks the exchange stopping.
        first.finish(ExchangeOutcome::Cancelled, 900)?;
        second.finish(ExchangeOutcome::Cancelled, 950)?;
        let mut frames = vec![(
            "the restart cancelled the children",
            frame(
                &parent,
                &[("explorer-1", &first), ("explorer-2", &second)],
                960,
            ),
        )];
        parent.collection = Some(ExchangeCollection::Pending);
        parent.pause_for_restart(1_000)?;
        frames.push((
            "the restart is collecting",
            frame(
                &parent,
                &[("explorer-1", &first), ("explorer-2", &second)],
                1_100,
            ),
        ));
        parent.collection = None;
        parent.start_turn(crate::turn::test_support::address(&parent, "second"), 2_000)?;
        let mut third = delegate(&mut parent, "explorer-3", 2_100)?;
        let mut fourth = delegate(&mut parent, "explorer-4", 2_200)?;
        fourth.finish(ExchangeOutcome::Complete, 4_000)?;
        frames.push((
            "the restarted turn runs",
            frame(
                &parent,
                &[
                    ("explorer-1", &first),
                    ("explorer-2", &second),
                    ("explorer-3", &third),
                    ("explorer-4", &fourth),
                ],
                5_000,
            ),
        ));
        parent.collection = Some(ExchangeCollection::Pending);
        parent.cancellation_requested = true;
        parent.pause(5_100);
        frames.push((
            "the user stopped the turn",
            frame(
                &parent,
                &[
                    ("explorer-1", &first),
                    ("explorer-2", &second),
                    ("explorer-3", &third),
                    ("explorer-4", &fourth),
                ],
                5_200,
            ),
        ));
        third.finish(ExchangeOutcome::Cancelled, 5_300)?;
        let stopping = frame(
            &parent,
            &[
                ("explorer-1", &first),
                ("explorer-2", &second),
                ("explorer-3", &third),
                ("explorer-4", &fourth),
            ],
            5_400,
        );
        assert_eq!(
            leaf_names(&stopping),
            leaf_names(&frames[frames.len() - 1].1),
            "the child the stop cancelled joined the summary before the stop settled"
        );
        frames.push(("the stop cancelled the last child", stopping));
        parent.finish(ExchangeOutcome::Cancelled, 5_500)?;
        let settled = frame(
            &parent,
            &[
                ("explorer-1", &first),
                ("explorer-2", &second),
                ("explorer-3", &third),
                ("explorer-4", &fourth),
            ],
            5_600,
        );
        assert_eq!(
            leaf_names(&settled),
            ["explorer-1", "explorer-2", "explorer-3", "explorer-4"],
            "{settled:#?}"
        );
        frames.push(("the stop settled", settled));
        for pair in frames.windows(2) {
            let ((earlier_name, earlier), (later_name, later)) = (&pair[0], &pair[1]);
            let shown = leaf_names(later);
            for name in leaf_names(earlier) {
                assert!(
                    shown.contains(&name),
                    "{name} left the summary between \"{earlier_name}\" and \"{later_name}\": \
                     {earlier:#?} then {later:#?}"
                );
            }
            assert!(
                heading_seconds(later) >= heading_seconds(earlier),
                "the summary's span fell between \"{earlier_name}\" and \"{later_name}\": \
                 {earlier:#?} then {later:#?}"
            );
        }
        Ok(())
    }

    /// A stop pauses the exchange clock, yet the newest thought it interrupts keeps its open fold
    /// until the exchange settles, so the rows the stop leaves change in place.
    #[test]
    fn a_stop_leaves_the_newest_thought_open_until_the_exchange_settles() -> anyhow::Result<()> {
        use crate::provider::ToolStatus;
        use forge_provider_api::ItemStart;
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"stopping", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        exchange.observe_event(
            &crate::turn::test_support::message(
                &address,
                "commentary",
                Some(forge_provider_api::AgentMessageRole::Commentary),
                "Reading the files",
            ),
            1,
        )?;
        let start = ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
            semantics: Default::default(),
            name: "command".into(),
            title: "Run".into(),
            arguments: forge_provider_api::InspectableArguments::new(json!({})),
        });
        let updates = crate::turn::test_support::tool_updates(
            &start,
            ToolStatus::Succeeded,
            None,
            Default::default(),
        );
        crate::turn::test_support::tool(&mut exchange, &address, "read", start, updates, 2)?;
        let thought_display =
            |exchange: &Exchange| -> anyhow::Result<forge_buffer::node::NodeDisplay> {
                let projected = super::project(
                    super::ProjectionSource::Exchange(exchange),
                    &WidthProfile::default(),
                    3,
                    false,
                    &HashMap::new(),
                    &HashMap::new(),
                    &mut crate::buffer::identity::DisplayRegistry::default(),
                )?;
                projected
                    .entry
                    .block
                    .iter()
                    .filter_map(|block| block.metadata.node.as_ref())
                    .find(|node| node.kind == forge_buffer::node::NodeKind::Thought)
                    .map(|node| node.default_display)
                    .ok_or_else(|| anyhow::anyhow!("the commentary has no thought node"))
            };
        assert_eq!(
            thought_display(&exchange)?,
            forge_buffer::node::NodeDisplay::Full
        );
        exchange.collection = Some(crate::exchange::ExchangeCollection::Pending);
        exchange.pause(3);
        assert_eq!(
            thought_display(&exchange)?,
            forge_buffer::node::NodeDisplay::Full,
            "the stop folded the thought it interrupted"
        );
        Ok(())
    }

    /// Keep the Agents root absent until a delegated child reaches a terminal state.
    #[test]
    fn agents_root_waits_for_a_finished_child() -> anyhow::Result<()> {
        let entry = delegating_exchange(&["explorer"], &[None])?;
        let projected = project_at(&entry, &WidthProfile::default(), 6_000)?;
        assert!(
            projected
                .entry
                .block
                .iter()
                .all(|block| block.id.0 != "parent:agents")
        );
        Ok(())
    }

    /// Every task row keeps its whole title visible, as `Plan submitted` does: a closed fold may
    /// hide the row's body, never the rest of a wrapped title.
    #[test]
    fn task_rows_keep_their_whole_title_visible() -> anyhow::Result<()> {
        let title = "Add one Vitest case to tests/tax.test.ts that checks taxCompute with a zero                      amount, then call check_run with apply set to true for the format check";
        let event = |id: &str, mut detail: serde_json::Value| -> anyhow::Result<TimelineEntry> {
            detail["id"] = id.into();
            detail["session_id"] = "session".into();
            detail["owner"] =
                serde_json::to_value(crate::timeline::TimelineOwner::session("session"))?;
            detail["created_at_ms"] = 1.into();
            Ok(TimelineEntry::SessionEvent {
                id: id.into(),
                created_at_ms: 1,
                event: serde_json::from_value(detail)?,
            })
        };
        let started = event(
            "started",
            json!({"kind":"task_started","start":{"kind":"plan","title":title},
                "prompt":"Harness instruction that stays folded"}),
        )?;
        let cleared = event(
            "cleared",
            json!({"kind":"task_finished","outcome":{"id":"task","kind":"plan","title":title,
                "state":"cancelled","summary":"Task cleared"}}),
        )?;
        for entry in [started, cleared] {
            let projected = project_at(&entry, &WidthProfile::default(), 6_000)?;
            let block = &projected.entry.block[0];
            let fold_start = block
                .metadata
                .fold
                .first()
                .map_or(block.text.row_count(), |fold| fold.start.row);
            let visible = block.text.wire_rows()[..fold_start].join(" ");
            assert!(
                visible.contains("for the format check"),
                "the title was folded: {visible:?}"
            );
            assert!(!visible.contains("Task cleared") && !visible.contains("Harness instruction"));
        }
        Ok(())
    }

    /// A task start paused before its provider showed anything, and closed when the task resumed
    /// in a new exchange, keeps its start notice but draws no empty heading beneath it. The same
    /// exchange still draws its heading while it is paused.
    #[test]
    fn closed_output_free_task_start_draws_no_empty_heading() -> anyhow::Result<()> {
        let mut exchange = crate::storage::test::running_exchange("goal-start");
        exchange.opening = crate::exchange::ExchangeOpening::TaskStarted {
            start: crate::exchange::TaskStart {
                kind: crate::task::TaskKind::Goal,
                title: "Create notes/d.txt".into(),
            },
        };
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address, 0)?;
        exchange.pause_for_restart(1)?;
        let rows = |exchange: &Exchange| -> anyhow::Result<Vec<String>> {
            let mut display = crate::buffer::identity::DisplayRegistry::default();
            let projected = super::project(
                super::ProjectionSource::Exchange(exchange),
                &WidthProfile::default(),
                2,
                false,
                &HashMap::new(),
                &HashMap::new(),
                &mut display,
            )?;
            Ok(projected
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .map(str::to_owned)
                .collect())
        };
        let paused = rows(&exchange)?;
        assert!(
            paused.iter().any(|row| row.contains("Paused 0s")),
            "{paused:#?}"
        );
        exchange.finish(crate::exchange::ExchangeOutcome::Complete, 2)?;
        let closed = rows(&exchange)?;
        assert!(
            closed
                .iter()
                .any(|row| row.contains("Goal started: Create notes/d.txt")),
            "{closed:#?}"
        );
        assert!(
            closed.iter().all(|row| !row.contains("Thought")),
            "{closed:#?}"
        );
        Ok(())
    }

    /// Plan execution rows name their plan by title too, so `Plan complete` and `Plan failed`
    /// wrap the whole title instead of folding its wrapped rows away.
    #[test]
    fn plan_execution_rows_keep_their_whole_title_visible() -> anyhow::Result<()> {
        let title = "Add a function moneyAllocate(amount, shares) to src/money.ts that splits an                 amount across equal shares and gives the remainder to the first share";
        let mut exchange = crate::storage::test::running_exchange("plan-execution-title");
        for (id, event) in [
            ("complete", json!({"kind":"completed","title":title})),
            (
                "failed",
                json!({"kind":"failed","title":title,"reason":"checks failed"}),
            ),
        ] {
            exchange.node_list.push(serde_json::from_value(json!({
                "kind":"plan_event","event":{
                    "id":id,"node_count":exchange.node_list.len(),
                    "content":{"kind":"execution","event":event}
                }
            }))?);
        }
        let mut display = crate::buffer::identity::DisplayRegistry::default();
        let projected = super::project(
            super::ProjectionSource::Exchange(&exchange),
            &WidthProfile::default(),
            1,
            false,
            &HashMap::new(),
            &HashMap::new(),
            &mut display,
        )?;
        for id in ["complete", "failed"] {
            let block = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == id)
                .expect("plan execution row");
            let fold_start = block
                .metadata
                .fold
                .first()
                .map_or(block.text.row_count(), |fold| fold.start.row);
            let visible = block.text.wire_rows()[..fold_start].join(" ");
            assert!(
                visible.contains("to the first share"),
                "the title was folded: {visible:?}"
            );
        }
        Ok(())
    }
}
