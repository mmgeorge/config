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

#[path = "projection/checks.rs"]
mod checks;

#[cfg(test)]
#[path = "projection/change_results_test.rs"]
mod change_results_test;

#[cfg(test)]
#[path = "projection/task_start_test.rs"]
mod task_start_test;

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
    #[cfg(test)]
    display: crate::buffer::identity::DisplayRegistry,
    pub entry: TranscriptEntry,
    pub prompt: Vec<BlockId>,
    pub action: HashMap<TargetId, TranscriptAction>,
    pub tool: HashMap<super::identity::DisplayId, ToolOutputView>,
    pub(crate) syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
}

impl ProjectedEntry {
    #[cfg(test)]
    fn status_block(&self) -> &BufferBlock {
        let super::status::StatusProjection::Replace { rendered, .. } = &self.status else {
            panic!("status replacement missing")
        };
        rendered.block.as_ref().expect("visible status missing")
    }

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
        #[cfg(test)]
        display: projection.display.snapshot(),
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
    #[test]
    fn compaction_context_usage_survives_event_order_and_reload() -> anyhow::Result<()> {
        use super::*;
        use crate::provider::{Compaction, CompactionResult, CompactionState};
        for metrics_on_completion in [false, true] {
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
            let apply = |exchange: &mut Exchange, compaction: Compaction, now| {
                let event =
                    forge_provider_api::ThreadEvent::CompactionUpdated(Box::new(compaction));
                let forge_provider_api::ThreadEvent::CompactionUpdated(compaction) = event else {
                    unreachable!("fixture carries positively identified compaction metadata")
                };
                exchange.observe_compaction(*compaction, now);
            };
            apply(&mut exchange, running.clone(), 1_000);
            let mut unrelated = running.clone();
            unrelated.thread_id = "different-provider".into();
            unrelated.state = CompactionState::Completed;
            unrelated.result.context_tokens_after = Some(900_000);
            apply(&mut exchange, unrelated, 2_000);
            let mut completed = running.clone();
            completed.state = CompactionState::Completed;
            completed.result = CompactionResult::default();
            completed.result.context_tokens_after = metrics_on_completion.then_some(32_000);
            apply(&mut exchange, completed.clone(), 16_000);
            let waiting = project_at(
                &TimelineEntry::Exchange {
                    id: "exchange".into(),

                    created_at_ms: 0,
                    exchange: (exchange.clone()).into(),

                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                16_000,
            )?;
            let original_header = waiting
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .find(|row| row.contains("120.0k"))
                .unwrap_or_default();
            assert_eq!(original_header.contains("→ 32.0k"), metrics_on_completion);
            completed.result.context_tokens_after = Some(32_000);
            apply(&mut exchange, completed.clone(), 17_000);
            apply(&mut exchange, completed.clone(), 18_000);
            completed.result.context_tokens_after = Some(64_000);
            apply(&mut exchange, completed, 19_000);
            apply(&mut exchange, running, 20_000);
            let restored: Exchange = serde_json::from_value(serde_json::to_value(exchange)?)?;
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: "exchange".into(),

                    created_at_ms: 0,
                    exchange: (restored).into(),

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
    fn task_replacement_withdraws_question_without_losing_its_content() -> anyhow::Result<()> {
        let question = serde_json::json!({ "id":"set", "questions":[{
            "id":"scope", "header":"Scope", "question":"Which files should change?",
            "options":[{"label":"Source", "description":"Update source files", "recommended":false}],
            "allow_freeform":true
        }] });
        let exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"withdrawn", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Plan the change", "state":"complete", "created_at_ms":0,
            "completed_at_ms":1, "attributed_matches_checkpoint":false,
            "node_list":[
                {"kind":"question_presented", "id":"presented", "question":question, "answer":null},
                {"kind":"question_withdrawn", "id":"withdrawal", "question":question, "reason":"task changed",
                    "answer":[{"question_id":"scope","response":{"kind":"selected","option":"Source","feedback":null}}]}
            ]
        }))?;
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),

                created_at_ms: 0,
                exchange: exchange.into(),

                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1,
        )?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Questions · Withdrawn"));
        assert_eq!(text.matches("Which files should change?").count(), 1);
        assert!(text.contains("Update source files"));
        assert!(text.contains("task changed"));
        assert!(text.contains("You answered: Source"));
        Ok(())
    }

    #[test]
    fn task_finished_event_keeps_outcome_without_a_task_owner() {
        let event = SessionEventKind::TaskFinished {
            outcome: crate::task::TaskOutcome {
                id: crate::task::TaskId::fresh(),
                kind: crate::task::TaskKind::Goal,
                title: "Reduce startup time".into(),
                state: crate::task::TaskOutcomeState::Complete,
                summary: "Startup now takes 80 ms.".into(),
            },
        };
        assert_eq!(
            super::session_event_text(&event),
            "Goal completed: Reduce startup time\nStartup now takes 80 ms."
        );
        let restored: SessionEventKind =
            serde_json::from_value(serde_json::to_value(event).unwrap()).unwrap();
        assert_eq!(
            super::session_event_text(&restored),
            "Goal completed: Reduce startup time\nStartup now takes 80 ms."
        );
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
                "node_list":[{"kind":"verification_result","id":"verify-result:verification-result","result":{
                    "outcome":outcome,"summary":"Ran eight gameplay tests.", "findings":findings}},
                    {"kind":"artifact_change","change":{
                    "id":"inspected-change", "path":"game.rs", "created_at_ms":1,
                    "diff_text":"diff --git a/game.rs b/game.rs\n--- a/game.rs\n+++ b/game.rs\n@@ -1 +1 @@\n-old\n+new\n"
                }}]
            }))?;
            let exchange: Exchange = serde_json::from_slice(&serde_json::to_vec(&exchange)?)?;
            let projected = project_at(
                &TimelineEntry::Exchange {
                    id: "verify-result".into(),

                    created_at_ms: 0,
                    exchange: exchange.into(),

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
                    .contains("Thought 18s")
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

    use super::{project_at, project_at_with_separator};
    use forge_buffer::node::{NodeDisplay, NodeLifecycle};
    use forge_buffer::width::WidthProfile;
    use serde_json::json;
    use std::collections::HashMap;

    use crate::{
        exchange::Exchange,
        session::state_machine::{SessionPhase, WorkflowActivity},
        timeline::{SessionEventKind, TimelineEntry},
    };

    #[test]
    fn activity_summary_preserves_permission_capture() {
        for (mode, capture) in [
            ("ask", "ForgeHarnessRead"),
            ("deny", "ForgeHarnessRead"),
            ("allow", "ForgeHarnessRead"),
        ] {
            for completed in [false, true] {
                let exchange: Exchange = serde_json::from_value(json!({
                    "id":"colored", "session_id":"session", "agent_id":"primary", "ordinal":1,
                    "prompt":"Inspect", "kind":"chat", "approval_mode":mode, "state": if completed { "complete" } else { "running" },
                    "created_at_ms":0, "completed_at_ms": if completed { Some(1000) } else { None },
                    "attributed_matches_checkpoint":false, "node_list":[]
                })).unwrap();
                let restored: Exchange =
                    serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
                assert_eq!(restored.approval_mode, exchange.approval_mode);
                let rendered = project_at(
                    &TimelineEntry::Exchange {
                        id: "colored".into(),

                        created_at_ms: 0,
                        exchange: (restored).into(),

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
        exchange.set_elicitation(Some(crate::plan::PlanElicitation::new(question)));
        exchange.set_awaiting_input(true);
        let source = TimelineEntry::Exchange {
            id: exchange.id.clone(),

            created_at_ms: 0,
            exchange: exchange.into(),

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
        use crate::exchange::{ExchangeNode, InputIntent, QuestionInput};

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
                let address =
                    crate::turn::test_support::address(&exchange, &format!("turn-{index}"));
                exchange.start_turn(address.clone(), index as i64).unwrap();
                let event = crate::turn::test_support::message(
                    &address,
                    "response",
                    Some(forge_provider_api::AgentMessageRole::Response),
                    reply,
                );
                exchange.observe_event(&event, index as i64).unwrap();
                let event = crate::turn::test_support::completed_event(
                    &(address),
                    crate::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                    index as i64,
                );
                exchange.observe_event(&event, index as i64).unwrap();
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
            exchange.set_elicitation(Some(elicitation));
            exchange.set_awaiting_input(true);
            exchange.pause(3);
            let render = |exchange: Exchange| {
                project_at(
                    &TimelineEntry::Exchange {
                        id: exchange.id.clone(),

                        created_at_ms: 0,
                        exchange: exchange.into(),

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
            exchange
                .finish(crate::exchange::ExchangeOutcome::Cancelled, 5)
                .unwrap();
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
    fn adjacent_tools_share_one_group_across_turns_and_hidden_reasoning() -> anyhow::Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"cross-turn", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect the repository", "kind":"chat", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let render = |exchange: &Exchange| {
            project_at(
                &TimelineEntry::Exchange {
                    id: exchange.id.clone(),

                    created_at_ms: 0,
                    exchange: (exchange.clone()).into(),

                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                100,
            )
        };
        let mut first_group_id = None;
        for ordinal in 1..=3 {
            let address = crate::turn::test_support::address(&exchange, &format!("turn-{ordinal}"));
            exchange.start_turn(address.clone(), ordinal * 10)?;
            for kind in ["reasoning", "reasoning_summary"] {
                let event = crate::turn::test_support::reasoning(
                    &address,
                    format!("{kind}-{ordinal}"),
                    if kind == "reasoning" {
                        forge_provider_api::ReasoningKind::Full
                    } else {
                        forge_provider_api::ReasoningKind::Summary
                    },
                    "Thoughts must not become commentary",
                );
                exchange.observe_event(&event, ordinal * 10 + 1)?;
            }

            let fixture_start = forge_provider_api::ItemStart::CommandExecution(
                forge_provider_api::ToolDescriptor {
                    semantics: Default::default(),
                    name: "inspect".into(),
                    arguments: forge_provider_api::InspectableArguments::new(json!({})),
                    title: format!("Inspect file {ordinal}"),
                },
            );
            let fixture_updates = crate::turn::test_support::tool_updates(
                &fixture_start,
                crate::provider::ToolStatus::Running,
                Some(format!("output-{ordinal}\n")),
                Default::default(),
            );
            crate::turn::test_support::tool(
                &mut exchange,
                &address,
                "shared-call",
                fixture_start.clone(),
                fixture_updates,
                ordinal * 10 + 2,
            )?;
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
                    projected
                        .display
                        .tool(
                            &projected.entry.id,
                            &crate::turn::ToolAddress {
                                turn: exchange.turn[owner as usize - 1].provider().clone(),
                                item_id: forge_provider_api::ItemId::new("shared-call").unwrap(),
                            }
                        )
                        .is_some_and(|id| projected.tool.contains_key(&id)),
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
                    exchange: (exchange.clone()).into(),

                    agent_by_id: HashMap::new(),
                },
                &WidthProfile::default(),
                crate::buffer::projection::FIXTURE_NOW_MS,
                &document,
                &Default::default(),
                &projected.tool,
                &projected.display,
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
            for update in crate::turn::test_support::tool_updates(
                &fixture_start,
                if ordinal == 2 {
                    crate::provider::ToolStatus::Failed
                } else {
                    crate::provider::ToolStatus::Succeeded
                },
                None,
                Default::default(),
            ) {
                exchange.observe_event(
                    &crate::turn::test_support::item_event(
                        &(address.clone()),
                        "shared-call",
                        update,
                    ),
                    ordinal * 10 + 3,
                )?;
            }
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
            let event = crate::turn::test_support::completed_event(
                &(address),
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                ordinal * 10 + 4,
            );
            exchange.observe_event(&event, ordinal * 10 + 4)?;
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

        let address = crate::turn::test_support::address(&exchange, "turn-4");
        exchange.start_turn(address.clone(), 40)?;
        let event = crate::turn::test_support::message(
            &address,
            "commentary",
            Some(forge_provider_api::AgentMessageRole::Commentary),
            "Now inspect the final file",
        );
        exchange.observe_event(&event, 41)?;

        let fixture_start =
            forge_provider_api::ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "inspect".into(),
                arguments: forge_provider_api::InspectableArguments::new(json!({})),
                title: "Inspect final file".into(),
            });
        let fixture_updates = crate::turn::test_support::tool_updates(
            &fixture_start,
            crate::provider::ToolStatus::Succeeded,
            Some("final-output\n".into()),
            Default::default(),
        );
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            "shared-call",
            fixture_start.clone(),
            fixture_updates,
            42,
        )?;
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
        let event = crate::turn::test_support::completed_event(
            &(address),
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            43,
        );
        exchange.observe_event(&event, 43)?;
        for outcome in [
            crate::exchange::ExchangeOutcome::Complete,
            crate::exchange::ExchangeOutcome::Cancelled,
            crate::exchange::ExchangeOutcome::Interrupted,
            crate::exchange::ExchangeOutcome::Failed,
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
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"sequence", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect the repository", "kind":"chat", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0).unwrap();
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
        for count in 1..=4 {
            for status in ["running", "completed"] {
                let fixture_start = forge_provider_api::ItemStart::CommandExecution(
                    forge_provider_api::ToolDescriptor {
                        semantics: Default::default(),
                        name: "inspect".into(),
                        arguments: forge_provider_api::InspectableArguments::new(json!({})),
                        title: format!("Inspect repository file {count}"),
                    },
                );
                let fixture_updates = crate::turn::test_support::tool_updates(
                    &fixture_start,
                    crate::provider::ToolStatus::parse(status).unwrap(),
                    Some(format!("first-{count}\nmiddle\nlast-{count}\n")),
                    Default::default(),
                );
                crate::turn::test_support::tool(
                    &mut exchange,
                    &address,
                    &format!("call-{count}"),
                    fixture_start.clone(),
                    fixture_updates,
                    count,
                )
                .unwrap();
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
                    .filter(|fold| fold.id.0.ends_with(":tool"))
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
        let event = crate::turn::test_support::completed_event(
            &(address.clone()),
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            11,
        );
        finished.observe_event(&event, 11).unwrap();
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
        let event = crate::turn::test_support::message(
            &address,
            "inspection",
            Some(forge_provider_api::AgentMessageRole::Commentary),
            "Repository inspection complete",
        );
        exchange.observe_event(&event, 11).unwrap();
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
    fn later_turn_results_follow_the_earlier_response_in_source_order() {
        let artifact = |id: &str, path: &str, created_at_ms: i64| {
            crate::exchange::ExchangeNode::ArtifactChange {
                change: crate::exchange::ArtifactChange {
                    revision_request: false,
                    id: id.into(),
                    declaration: None,
                    path: path.into(),
                    created_at_ms,
                    diff_text: format!("--- {path}\n+++ {path}\n@@ -1 +1 @@\n-old\n+new\n"),
                },
            }
        };
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"revision", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        exchange.resume(0).unwrap();
        let address = crate::turn::test_support::address(&exchange, "first");
        exchange.start_turn(address.clone(), 0).unwrap();
        exchange.node_list.push(artifact("early", "early.md", 1));
        let event = crate::turn::test_support::message(
            &address,
            "first-response",
            Some(forge_provider_api::AgentMessageRole::Response),
            "First revision is ready.",
        );
        exchange.observe_event(&event, 2).unwrap();
        let event = crate::turn::test_support::completed_event(
            &address,
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            3,
        );
        exchange.observe_event(&event, 3).unwrap();
        let address = crate::turn::test_support::address(&exchange, "second");
        exchange.start_turn(address.clone(), 4).unwrap();
        let event = crate::turn::test_support::message(
            &address,
            "second-commentary",
            Some(forge_provider_api::AgentMessageRole::Commentary),
            "Applying the requested changes.",
        );
        exchange.observe_event(&event, 4).unwrap();
        exchange.node_list.push(artifact("late", "late.md", 4));
        let lifecycle = serde_json::from_value(json!({
            "id":"event-2", "session_id":"session", "plan_id":"plan",
            "title":"Second plan", "kind":"revision_created",
            "model_revision":2, "user_revision":0, "created_at_ms":4
        }))
        .unwrap();
        exchange
            .node_list
            .push(crate::exchange::ExchangeNode::PlanEvent {
                event: Box::new(crate::plan::ExchangePlanEvent {
                    id: "event-2".into(),
                    node_count: exchange.node_list.len(),
                    content: crate::plan::PlanEventContent::Lifecycle {
                        title: "Second plan".into(),
                        lifecycle,
                    },
                }),
            });
        let event = crate::turn::test_support::message(
            &address,
            "second-response",
            Some(forge_provider_api::AgentMessageRole::Response),
            "Second revision is ready.",
        );
        exchange.observe_event(&event, 6).unwrap();
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange: exchange.into(),
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            10,
        )
        .unwrap();
        let rows = projected
            .entry
            .block
            .iter()
            .map(|block| block.text.wire_rows().join("\n"))
            .collect::<Vec<_>>();
        let position = |needle: &str| {
            rows.iter()
                .position(|text| text.contains(needle))
                .unwrap_or_else(|| panic!("{needle} missing from {rows:#?}"))
        };
        let early = position("early.md");
        let first = position("First revision is ready.");
        let late = position("late.md");
        let second = position("Second revision is ready.");
        assert!(early < first, "{rows:#?}");
        assert!(first < late, "{rows:#?}");
        assert!(late < second, "{rows:#?}");
    }

    #[test]
    fn declaration_revision_projects_file_deltas_and_overview_as_separate_trees() {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"revision", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0).unwrap();
        let response = crate::turn::test_support::message(
            &address,
            "response",
            Some(forge_provider_api::AgentMessageRole::Response),
            "Submitted the revised design.",
        );
        exchange.observe_event(&response, 1).unwrap();
        let response = crate::turn::test_support::completed_event(
            &(address),
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            2,
        );
        exchange.observe_event(&response, 2).unwrap();
        exchange.node_list.push(crate::exchange::ExchangeNode::ArtifactChange {
            change: crate::exchange::ArtifactChange {
                        revision_request: false,
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
                exchange: exchange.into(),

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
    fn child_preview_nests_live_activity_and_completed_task_history() {
        use crate::exchange::ExchangeState;
        use crate::provider::ToolStatus;
        let mut nested: Exchange = serde_json::from_value(json!({
            "id":"nested", "session_id":"session", "agent_id":"nested-agent", "ordinal":1,
            "prompt":"Inspect nested work", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        nested.resume(0).unwrap();
        let address = crate::turn::test_support::address(&nested, "turn");
        nested.start_turn(address.clone(), 0).unwrap();
        let update = crate::turn::test_support::message(
            &address,
            "earlier",
            Some(forge_provider_api::AgentMessageRole::Commentary),
            "Earlier child thought",
        );
        nested.observe_event(&update, 1).unwrap();
        let update = crate::turn::test_support::message(
            &address,
            "latest",
            Some(forge_provider_api::AgentMessageRole::Commentary),
            "Latest child thought",
        );
        nested.observe_event(&update, 2).unwrap();
        let update = crate::turn::test_support::message(
            &address,
            "response",
            Some(forge_provider_api::AgentMessageRole::Response),
            "Child final response",
        );
        nested.observe_event(&update, 3).unwrap();

        let fixture_start =
            forge_provider_api::ItemStart::ToolCall(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "child".into(),
                arguments: forge_provider_api::InspectableArguments::new(json!({})),
                title: "Child tool".into(),
            });
        let fixture_updates = crate::turn::test_support::tool_updates(
            &fixture_start,
            ToolStatus::Succeeded,
            Some("Child tool result".into()),
            Default::default(),
        );
        crate::turn::test_support::tool(
            &mut nested,
            &address,
            "call",
            fixture_start.clone(),
            fixture_updates,
            3,
        )
        .unwrap();
        let mut parent = nested.clone();
        parent.id = "parent".into();
        parent.turn.clear();
        parent.node_list.clear();
        let address = crate::turn::test_support::address(&parent, "root-turn");
        parent.start_turn(address.clone(), 0).unwrap();

        let fixture_start =
            forge_provider_api::ItemStart::ToolCall(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "agent_spawn".into(),
                arguments: forge_provider_api::InspectableArguments::new(json!({})),
                title: "agent_spawn(task_name=Nested)".into(),
            });
        let fixture_updates = crate::turn::test_support::tool_updates(
            &fixture_start,
            ToolStatus::Succeeded,
            Some("SPAWN_PAYLOAD".into()),
            Default::default(),
        );
        crate::turn::test_support::tool(
            &mut parent,
            &address,
            "spawn-call",
            fixture_start.clone(),
            fixture_updates,
            1,
        )
        .unwrap();
        parent
            .observe_event(
                &crate::turn::test_support::message(
                    &address,
                    "parent-response",
                    Some(forge_provider_api::AgentMessageRole::Response),
                    "Parent final response",
                ),
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
            exchange: (parent).into(),

            agent_by_id: HashMap::from([(
                "nested-agent".into(),
                TimelineEntry::AgentLifecycle {
                    id: "nested-agent".into(),

                    created_at_ms: 0,
                    run: (crate::agent::Agent::pending("session", "Nested", "Inspect", 0)).into(),

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
            crate::buffer::projection::FIXTURE_NOW_MS,
            &document,
            &Default::default(),
            &projected.tool,
            &projected.display,
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
            (crate::provider::ServiceTier::Standard, ""),
            (crate::provider::ServiceTier::Fast, " +"),
            (crate::provider::ServiceTier::Ultrafast, " ++"),
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
                        provider: forge_providers::registry::default_id(),
                        model: "gpt-6.1-sol".into(),
                        reasoning_effort: "high".into(),
                        context_window: None,
                        service_tier,
                        access: Default::default(),
                        approval_mode: crate::session::ApprovalMode::Ask,
                    });
                    exchange[0].phase = crate::exchange::ExchangePhase::fixture(
                        state,
                        exchange[0].completed_at_ms(),
                        exchange[0].finalization_outcome(),
                    );
                    exchange[0].phase = crate::exchange::ExchangePhase::fixture(
                        exchange[0].state(),
                        (state == ExchangeState::Complete).then_some(7000),
                        exchange[0].finalization_outcome(),
                    );
                }
                let selected = project_at(&selected_entry, &WidthProfile::default(), 7000).unwrap();
                let expected = format!(
                    "s · Nested · {} · gpt-6.1-sol{suffix}",
                    forge_providers::registry::default_id()
                );
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
                    crate::buffer::projection::FIXTURE_NOW_MS,
                    &document,
                    &Default::default(),
                    &selected.tool,
                    &selected.display,
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
            let fixture_start =
                forge_provider_api::ItemStart::ToolCall(forge_provider_api::ToolDescriptor {
                    semantics: Default::default(),
                    name: "agent_spawn".into(),
                    arguments: forge_provider_api::InspectableArguments::new(json!({})),
                    title: "agent_spawn(task_name=Nested)".into(),
                });
            let fixture_updates = crate::turn::test_support::tool_updates(
                &fixture_start,
                ToolStatus::Failed,
                None,
                Default::default(),
            );
            crate::turn::test_support::tool(
                exchange,
                &address,
                "failed-spawn-call",
                fixture_start.clone(),
                fixture_updates,
                2,
            )
            .unwrap();
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
                run: (crate::agent::Agent::pending("session", "Nested", "Inspect", 0)).into(),

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
            projection.status_block().text.wire_rows(),
            vec!["Working · 3s"]
        );
        let status = &projection.status_block().metadata;
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
    fn awaiting_input_status_exposes_question_hint_target() {
        for owner in [
            crate::broker::ElicitationOwner::Plan,
            crate::broker::ElicitationOwner::Interaction,
            crate::broker::ElicitationOwner::PlanReview,
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
            let block = projected.status_block();
            assert_eq!(block.text.wire_rows(), vec!["Waiting for your answer"]);
            assert_eq!(block.metadata.target[0].id.0, "session:status:question");
            assert_eq!(block.metadata.target[0].range.start.row, 0);
            assert!(block.metadata.fold.is_empty());
            assert!(projected.action.is_empty());
        }
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
                exchange: (interaction.clone()).into(),

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
                exchange: (interaction).into(),

                agent_by_id: HashMap::new(),
            }),
            &WidthProfile::default(),
            1_000,
            true,
            &HashMap::new(),
            &HashMap::new(),
            &mut crate::buffer::identity::DisplayRegistry::default(),
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
            (crate::exchange::ExchangeOutcome::Interrupted, "Interrupted"),
            (crate::exchange::ExchangeOutcome::Failed, "Failed"),
            (crate::exchange::ExchangeOutcome::Cancelled, "Cancelled"),
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
    fn interaction_results_render_created_then_deleted_files_without_preview_errors()
    -> anyhow::Result<()> {
        use crate::provider::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
        for persistent in [false, true] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"transient", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Exercise changes", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            exchange.resume(0)?;
            let address = crate::turn::test_support::address(&exchange, "turn");
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
                let event = crate::turn::test_support::message(
                    &address,
                    format!("step-{index}"),
                    Some(forge_provider_api::AgentMessageRole::Commentary),
                    format!("Step {index}"),
                );
                exchange.observe_event(&event, index as i64 * 10 + 1)?;
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

                let fixture_start =
                    forge_provider_api::ItemStart::FileChange(forge_provider_api::ToolDescriptor {
                        semantics: Default::default(),
                        title: "file changes".into(),
                        name: "edit".into(),
                        arguments: forge_provider_api::InspectableArguments::new(json!({})),
                    });
                let fixture_updates = crate::turn::test_support::tool_updates(
                    &fixture_start,
                    crate::provider::ToolStatus::Succeeded,
                    None,
                    ProviderChangeSet { file },
                );
                crate::turn::test_support::tool(
                    &mut exchange,
                    &address,
                    &format!("write-{index}"),
                    fixture_start.clone(),
                    fixture_updates,
                    index as i64 * 10 + 2,
                )?;
            }
            for complete in [false, true] {
                if complete {
                    exchange.turn.last_mut().unwrap().finish(
                        crate::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                        49,
                    )?;
                    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 50)?;
                    exchange.checkpoint_before = Some("before".into());
                    exchange.checkpoint_after = Some("after".into());
                    exchange.checkpoint_diff_text = Some(if persistent {
                        "--- a/existing.lua\n+++ b/existing.lua\n@@ -1 +1 @@\n-before\n+after\n"
                            .into()
                    } else {
                        String::new()
                    });
                }
                let restored: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
                let projected = project_at(
                    &TimelineEntry::Exchange {
                        id: "transient".into(),

                        created_at_ms: 0,
                        exchange: (restored).into(),

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
                    .find(|block| block.id.0 == "transient:results:changes");
                if !complete || !persistent {
                    assert!(summary.is_none());
                } else {
                    assert!(
                        summary
                            .unwrap()
                            .text
                            .wire_rows()
                            .join("")
                            .contains("Workspace changes 1 file")
                    );
                }
                assert_eq!(
                    blocks
                        .iter()
                        .filter(|block| {
                            block.id.0.ends_with(":changes")
                                && block.id.0 != "transient:results:changes"
                        })
                        .count(),
                    8
                );
            }
        }
        Ok(())
    }

    /// A turn-scoped error remains visible once while its native work continues.
    #[test]
    fn turn_error_notices_survive_reload_without_completing_work() -> anyhow::Result<()> {
        let mut turn = crate::turn::Turn::new(
            "turn".into(),
            crate::provider::test_support::address(&crate::provider::InvocationScope::new(
                crate::provider::ThreadOwner::root("session"),
                "work",
            )),
            0,
        );
        let error = forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::TurnStart,
            forge_provider_api::ProviderFailureKind::RateLimited,
        );
        assert!(turn.record_error(error));
        assert!(!turn.record_error(error));
        assert_eq!(turn.state(), crate::turn::TurnState::Running);
        let exchange: Exchange = serde_json::from_value(json!({
            "id":"work", "session_id":"session", "agent_id":"session:agent:primary", "ordinal":1,
            "prompt":"Answer", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":true, "turn":[turn], "node_list":[]
        }))?;
        let entry = TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 0,
            exchange: exchange.into(),
            agent_by_id: HashMap::new(),
        };
        let loaded = serde_json::from_str(&serde_json::to_string(&entry)?)?;
        for source in [&entry, &loaded, &loaded] {
            let projected = project_at(source, &WidthProfile::default(), 20)?;
            let notices = projected
                .entry
                .block
                .iter()
                .filter(|block| block.id.0 == "work:turn:turn:error:0")
                .collect::<Vec<_>>();
            assert_eq!(notices.len(), 1);
            assert!(
                notices[0]
                    .text
                    .wire_rows()
                    .join("")
                    .contains("Provider turn error:")
            );
        }
        Ok(())
    }

    /// Empty native stops retain exactly one visible notice after history reload and reprojection.
    #[test]
    fn native_stop_notices_survive_reload_without_duplication() -> anyhow::Result<()> {
        use forge_provider_api::turn::TurnCompletionReason;
        for (reason, notice) in [
            (
                TurnCompletionReason::Refusal,
                "The provider refused to continue.",
            ),
            (
                TurnCompletionReason::TokenLimit,
                "The provider stopped because it reached its token limit.",
            ),
            (
                TurnCompletionReason::RequestLimit,
                "The provider stopped because this turn reached its model-request limit.",
            ),
        ] {
            let mut turn = crate::turn::Turn::new(
                "turn".into(),
                crate::provider::test_support::address(&crate::provider::InvocationScope::new(
                    crate::provider::ThreadOwner::root("session"),
                    "work",
                )),
                0,
            );
            turn.finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: reason,
                },
                20,
            )?;
            let exchange: Exchange = serde_json::from_value(json!({
                "id":"work", "session_id":"session", "agent_id":"session:agent:primary", "ordinal":1,
                "prompt":"Answer", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":true, "turn":[turn], "node_list":[]
            }))?;
            let entry = TimelineEntry::Exchange {
                id: exchange.id.clone(),
                created_at_ms: 0,
                exchange: exchange.into(),
                agent_by_id: HashMap::new(),
            };
            let reloaded = serde_json::from_str(&serde_json::to_string(&entry)?)?;
            for source in [&entry, &reloaded, &reloaded] {
                let projected = project_at(source, &WidthProfile::default(), 20)?;
                let notices = projected
                    .entry
                    .block
                    .iter()
                    .filter(|block| block.id.0 == "work:turn:turn:stop-reason")
                    .collect::<Vec<_>>();
                assert_eq!(notices.len(), 1);
                assert!(notices[0].text.wire_rows().join("").contains(notice));
            }
        }
        Ok(())
    }

    #[test]
    fn owned_check_results_keep_exchange_ownership_before_the_final_response() -> anyhow::Result<()>
    {
        use crate::provider::{InvocationScope, ThreadOwner};
        use crate::turn::{MessageDelivery, MessageKind, Turn, TurnOutcome};

        let mut turn = Turn::new(
            "turn".into(),
            crate::provider::test_support::address(&InvocationScope {
                thread_id: forge_provider_api::ThreadId::from(
                    ThreadOwner::new(
                        "session".into(),
                        forge_provider_api::agent::AgentId::new("primary").unwrap(),
                    )
                    .expect("registered session and agent identities are nonempty"),
                ),
                exchange_id: "work".into(),
                invocation_id: "invocation".into(),
            }),
            0,
        );
        turn.record_provider_text(
            MessageKind::Assistant,
            "Work completed",
            None,
            MessageDelivery::Final,
            false,
        )?;
        turn.finish(
            TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            20,
        )?;
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
                exchange.finish(crate::exchange::ExchangeOutcome::Complete, 20)?;
            }
            let entry = TimelineEntry::Exchange {
                id: exchange.id.clone(),

                created_at_ms: 0,
                exchange: exchange.into(),

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
                .position(|block| block.id.0 == "work:initial:results:checks")
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
                Some(&forge_buffer::identity::FoldId("work:initial".into())),
                "checks without an assistant message belong to initial commentary"
            );
            assert_eq!(blocks[checks].metadata.layout.as_ref().unwrap().indent, 6);
        }
        Ok(())
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
            exchange
                .finish(crate::exchange::ExchangeOutcome::Failed, 4000)
                .unwrap();
            exchange.set_disposition(disposition).unwrap();
            let entry = TimelineEntry::Exchange {
                id: exchange.id.clone(),

                created_at_ms: 1000,
                exchange: (exchange.clone()).into(),

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
            assert_eq!(exchange.state(), ExchangeState::Failed);
            assert_eq!(exchange.elapsed(90_000), 3000);
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

    /// Steering a provider consumes after its current answer renders between the two answers.
    ///
    /// Copilot can hold steering until a model call ends: the input is acknowledged before the
    /// first answer streams, and its native echo arrives after that answer. The echo, not the
    /// acknowledgement, marks where the model read the input, so the steering row moves there and
    /// the reply to it no longer runs on from the earlier answer.
    #[test]
    fn steering_consumed_after_an_answer_renders_between_the_answers() -> anyhow::Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"late-steering", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Write an essay", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let submission = forge_provider_api::SubmissionId::try_from("steering-submission")?;
        exchange.append_acknowledged_input(
            Some("steering-input".into()),
            Some(submission.clone()),
            crate::exchange::InputIntent::Steering,
            "Reply with BETA.".into(),
            1,
        )?;
        let message = |id: &str, item: forge_provider_api::ItemStart| {
            crate::turn::test_support::item_event(
                &address,
                id,
                forge_provider_api::ItemEvent::Started(item),
            )
        };
        let agent = |text: &str| {
            forge_provider_api::ItemStart::AgentMessage(forge_provider_api::AgentMessageStart {
                text: text.into(),
                classification: forge_provider_api::MessageClassification::Unclassified,
            })
        };
        exchange.observe_event(&message("essay", agent("Bridges are old.")), 2)?;
        exchange.observe_event(
            &message(
                "steering-echo",
                forge_provider_api::ItemStart::UserMessage(forge_provider_api::UserMessageStart {
                    text: "Reply with BETA.".into(),
                    submission_id: Some(submission),
                }),
            ),
            3,
        )?;
        exchange.observe_event(&message("reply", agent("BETA")), 4)?;
        let order = exchange
            .node_list
            .iter()
            .map(|node| match node {
                crate::exchange::ExchangeNode::ExchangeInput { prompt } => prompt.text.clone(),
                crate::exchange::ExchangeNode::TurnContent { id, .. } => id.clone(),
                _ => String::new(),
            })
            .collect::<Vec<_>>();
        // The essay, the echo, and the reply are the turn's first three messages; the echo
        // renders no row of its own.
        assert!(
            order.len() == 3
                && order[0].ends_with(":message:1")
                && order[1] == "Reply with BETA."
                && order[2].ends_with(":message:3"),
            "steering did not move to where the provider consumed it: {order:?}"
        );
        Ok(())
    }

    /// A native echo of a Harness child-result report is input evidence, not a user prompt row.
    #[test]
    fn native_echo_of_a_child_report_renders_no_prompt_row() -> anyhow::Result<()> {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"child-echo", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Delegate the work", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let report = format!(
            "{}\n{{\"task_id\":\"child-task\"}}",
            crate::agent::report::CHILD_RESULT_PREAMBLE
        );
        exchange.observe_event(
            &crate::turn::test_support::item_event(
                &address,
                "child-report-echo",
                forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::UserMessage(
                    forge_provider_api::UserMessageStart {
                        text: report,
                        submission_id: None,
                    },
                )),
            ),
            1,
        )?;
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
        assert!(!text.contains("Harness child result"), "{text}");
        assert!(!text.contains("child-task"), "{text}");
        Ok(())
    }

    /// Native child history remains visible without managed actions or a fabricated user submission.
    #[test]
    fn observed_native_history_is_read_only_and_unavailable_is_explicit() -> anyhow::Result<()> {
        use crate::provider::{InvocationScope, ScopeOwner, ThreadOwner, TurnAddress};
        use forge_provider_api::agent::{
            AgentActiveState, AgentDelegation, AgentDescription, AgentEvent, AgentId,
            AgentLifecycleEvent,
        };
        let root = InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(ThreadOwner::root("session")),
            exchange_id: "root".into(),
            invocation_id: "root-turn".into(),
        };
        let owner = ThreadOwner::new(
            root.owner().session_id().clone(),
            AgentId::new("execution-owner")?,
        )?;
        let scope = InvocationScope {
            thread_id: owner.thread_id(),
            exchange_id: root.exchange_id.clone(),
            invocation_id: "child-turn".into(),
        };
        let address = TurnAddress {
            thread_id: scope.owner().thread_id(),
            turn_id: scope.turn_id()?,
            scope,
        };
        let public = AgentId::new("public-child")?;
        let description = AgentDescription {
            parent_agent_id: Some(root.owner().agent_id().clone()),
            delegation: Some(AgentDelegation {
                parent_thread_id: root.owner().thread_id(),
                parent_turn_id: root.turn_id()?,
                parent_item_id: None,
            }),
            thread_id: Some(address.thread_id.clone()),
            definition: None,
            nickname: Some("Inspect".into()),
            task: None,
        };
        let proof = crate::provider::test_support::observed_agent_fixture(
            forge_provider_api::BindingId::new("binding")?,
            root,
            vec![(public.clone(), description.clone(), Some(address.clone()))],
            public,
            Some(address.turn_id.clone()),
        )?;
        let source = |sequence| crate::provider::events::NativeEventSource {
            binding_id: proof.binding_id().clone(),
            sequence: forge_provider_api::EventSequence::new(sequence),
        };
        let mut child = crate::agent::ObservedAgent::registered(&proof, 1)?;
        child.observe_agent(
            &proof,
            &source(1),
            &AgentEvent::Lifecycle {
                event: AgentLifecycleEvent::Started {
                    agent: description,
                    state: AgentActiveState::Running,
                },
            },
            1,
        )?;
        let native_scope = crate::provider::EventScope::from(address.scope.clone());
        child.observe_event(
            &proof,
            &source(2),
            &native_scope,
            &forge_provider_api::ProviderEvent::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event: forge_provider_api::TurnEvent::Started(
                    forge_provider_api::turn::TurnStarted { started_at_ms: 2 },
                ),
            },
            2,
        )?;
        child.observe_event(
            &proof,
            &source(3),
            &native_scope,
            &forge_provider_api::ProviderEvent::Item {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                item_id: forge_provider_api::ItemId::new("history-user")?,
                event: forge_provider_api::ItemEvent::Started(
                    forge_provider_api::ItemStart::UserMessage(
                        forge_provider_api::UserMessageStart {
                            text: "Positive native child history".into(),
                            submission_id: None,
                        },
                    ),
                ),
            },
            3,
        )?;
        child.mark_unavailable();
        let id = child.storage_id();
        let entries =
            crate::timeline::TimelineProjector::with_observed_agents(vec![], vec![child])?;
        let projected = project_at(&entries[0], &WidthProfile::default(), 90_000)?;
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Native agent Inspect · observation unavailable"));
        assert!(text.contains("Last native state: running"));
        assert!(text.contains("Positive native child history"));
        assert!(
            !projected
                .action
                .values()
                .any(|action| matches!(action, super::TranscriptAction::Agent { .. }))
        );
        assert_eq!(
            projected
                .entry
                .block
                .iter()
                .filter(|block| block
                    .metadata
                    .decoration
                    .iter()
                    .any(|decoration| decoration.capture == "ForgeHarnessPrompt"))
                .count(),
            1
        );
        assert_eq!(
            projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == id)
                .unwrap()
                .metadata
                .node
                .as_ref()
                .unwrap()
                .lifecycle,
            NodeLifecycle::Settled
        );
        Ok(())
    }
}
