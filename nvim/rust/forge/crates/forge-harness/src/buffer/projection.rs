use std::collections::{BTreeMap, HashMap};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{
    BlockAnchor, BufferBlock, ContentLayout, Decoration, FoldRange, TargetRange, TextChunk, TextPosition,
    TextRange, StatusPresentation, StatusHint,
};
use forge_buffer::identity::{BlockId, FoldId, TargetId};
use forge_buffer::width::WidthProfile;
use forge_buffer::node::{NodeState, NodeKind, NodeDisplay, NodeLifecycle};
use serde::Serialize;

use crate::exchange::{Exchange, ExchangeKind, ExchangeNode, ExchangeState};
use crate::session::state_machine::{ExecutionStatus, SessionPhase, WorkflowActivity};
use crate::timeline::{SessionEventKind, TimelineEntry};

use super::document::TranscriptEntry;
use super::tool::ToolOutputView;
use super::transcript::TranscriptRenderer;

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
    project_at_with_separator(entry.into(), width, now_ms, leading_separator, expansion, tool_limit)
}

fn reported_changes(exchange: &Exchange, agents: &HashMap<&str, &TimelineEntry>) -> Option<String> {
    fn record(
        builder: &mut crate::exchange::ProviderDiffBuilder,
        exchange: &Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        visited: &mut std::collections::HashSet<String>,
    ) {
        if !visited.insert(exchange.id.clone()) { return; }
        builder.record(exchange);
        for node in &exchange.node_list {
            let ExchangeNode::AgentReference { agent } = node else { continue };
            let Some(TimelineEntry::AgentLifecycle { exchange, agent: children, .. }) = agents
                .get(agent.id.as_str()).or_else(|| agents.get(agent.child_agent_id.as_str())).copied()
            else { continue };
            let Some(exchange) = exchange.iter().find(|exchange| exchange.id == agent.child_exchange_id)
            else { continue };
            let children = children.iter().filter_map(|entry| match entry {
                TimelineEntry::AgentLifecycle { id, .. } => Some((id.as_str(), entry)),
                _ => None,
            }).collect();
            record(builder, exchange, &children, visited);
        }
    }
    let mut builder = crate::exchange::ProviderDiffBuilder::default();
    record(&mut builder, exchange, agents, &mut std::collections::HashSet::new());
    builder.finish()
}

fn tool_group_label<'tool>(calls: impl IntoIterator<Item = &'tool crate::turn::ToolCall>,
    settled: bool, now_ms: i64) -> String {
    let mut count = 0;
    let mut failed = 0;
    let mut elapsed = Some(0_u64);
    for tool in calls {
        count += 1;
        failed += usize::from(tool.failed);
        elapsed = elapsed.zip(tool.elapsed_ms(now_ms)).map(|(total, duration)| total.saturating_add(duration));
    }
    let mut label = format!("▸ {} {count} {}", if settled { "Ran" } else { "Running" },
        if count == 1 { "tool" } else { "tools" });
    if failed > 0 { label.push_str(&format!(" ({failed} failed)")); }
    if let Some(elapsed) = elapsed {
        label.push_str(&format!(" · {}", super::duration::tool_duration(Some(elapsed)).trim()));
    }
    label
}

pub(super) fn timing_blocks(entry: &TimelineEntry, width: &WidthProfile,
    document: &forge_buffer::document::BufferDocument,
    expanded: &HashMap<String, bool>,
    tools: &HashMap<String, ToolOutputView>) -> Result<Vec<BufferBlock>> {
    fn exchanges<'source>(entry: &'source TimelineEntry, result: &mut Vec<&'source Exchange>,
        headings: &mut Vec<(&'source str, &'source crate::agent::Agent, Option<&'source Exchange>)>) {
        match entry {
            TimelineEntry::Exchange { exchange,agent_by_id,.. } => {
                result.push(exchange);
                for node in &exchange.node_list {
                    if let ExchangeNode::AgentReference { agent } = node
                        && let Some(TimelineEntry::AgentLifecycle { run, exchange, .. }) = agent_by_id
                            .get(&agent.id).or_else(|| agent_by_id.get(&agent.child_agent_id)) {
                        headings.push((&agent.id, run, exchange.iter().find(|exchange|exchange.id == agent.child_exchange_id)));
                    }
                }
                for agent in agent_by_id.values() { exchanges(agent,result,headings); }
            }
            TimelineEntry::AgentLifecycle { id,run,exchange,agent,.. } => {
                headings.push((id, run, None));
                result.extend(exchange);
                for source in exchange {
                    for node in &source.node_list {
                        if let ExchangeNode::AgentReference { agent: reference } = node
                            && let Some(TimelineEntry::AgentLifecycle { run,exchange,.. }) = agent.iter().find(|agent|
                                agent.id() == reference.id || agent.id() == reference.child_agent_id) {
                            headings.push((&reference.id,run,exchange.iter().find(|exchange|exchange.id == reference.child_exchange_id)));
                        }
                    }
                }
                for agent in agent { exchanges(agent,result,headings); }
            }
            _ => {}
        }
    }
    if matches!(entry,TimelineEntry::Status { .. }) {
        return Ok(project(ProjectionSource::Entry(entry),width,false,expanded,&HashMap::new())?.entry.block);
    }
    let now_ms = SystemTime::now().duration_since(UNIX_EPOCH)?.as_millis() as i64;
    let mut source = Vec::new();
    let mut headings = Vec::new();
    exchanges(entry,&mut source,&mut headings);
    let mut blocks = Vec::new();
    let mut rendered = std::collections::HashSet::new();
    for (id,run,exchange) in headings {
        if !rendered.insert(id) { continue; }
        let Some(previous) = document.block(&BlockId(id.into())) else { continue; };
        let mut profile = width.clone();
        profile.columns = profile.columns.saturating_sub(previous.metadata.layout.as_ref()
            .map_or(0, |layout|layout.indent.saturating_sub(2))).max(1);
        let mut block = TranscriptRenderer::new(&profile)?.literal(previous.id.clone(),
            &agent_summary(run,exchange,now_ms),2)?;
        block.metadata = previous.metadata.clone();
        for span in &mut block.metadata.decoration {
            if span.range.end.row == previous.text.row_count() { span.range.end.row = block.text.row_count(); }
        }
        TranscriptRenderer::resolve_marker(&mut block)?;
        blocks.push(block);
    }
    for exchange in source {
        if exchange.completed_at_ms.is_some() || exchange.execution_started_at_ms.is_none() { continue; }
        if let Some(previous) = document.block(&BlockId(format!("{}:summary",exchange.id))) {
            let mut profile = width.clone();
            profile.columns = profile.columns.saturating_sub(previous.metadata.layout.as_ref()
                .map_or(0, |layout|layout.indent.saturating_sub(2))).max(1);
            let mut block = TranscriptRenderer::new(&profile)?.literal(previous.id.clone(),
                &exchange_activity_summary(exchange,now_ms),2)?;
            block.metadata = previous.metadata.clone();
            for span in &mut block.metadata.decoration {
                if span.range.end.row == previous.text.row_count() { span.range.end.row = block.text.row_count(); }
            }
            TranscriptRenderer::resolve_marker(&mut block)?;
            blocks.push(block);
        }
        for turn in exchange.turn.iter().filter(|turn|turn.state() == crate::turn::TurnState::Running) {
            let mut groups: BTreeMap<&str, Vec<&crate::turn::ToolCall>> = BTreeMap::new();
            for tool in turn.tools() {
                let call_id = format!("{}:{}",turn.id(),tool.id);
                if let Some(output) = tools.get(&call_id).filter(|output| !output.group.is_empty()) {
                    groups.entry(&output.group).or_default().push(tool);
                }
            }
            for (id, calls) in groups {
                if !calls.iter().any(|tool| tool.state() == crate::turn::ToolState::Running) { continue; }
                for tool in &calls {
                    let call_id = format!("{}:{}", turn.id(), tool.id);
                    let Some(previous) = document.block(&BlockId(format!("{call_id}:tool"))) else { continue; };
                    let mut profile = width.clone();
                    profile.columns = profile.columns.saturating_sub(previous.metadata.layout.as_ref()
                        .map_or(0, |layout|layout.indent.saturating_sub(2))).max(1);
                    blocks.push(TranscriptRenderer::new(&profile)?.refresh_tool_heading(
                        previous, &tool.kind, tool.elapsed_ms(now_ms), tool.failed, &tool.title, expanded.get(&format!("{call_id}:tool")) == Some(&true))?);
                }
                let Some(previous) = document.block(&BlockId(id.into())) else { continue; };
                let mut profile = width.clone();
                profile.columns = profile.columns.saturating_sub(previous.metadata.layout.as_ref()
                    .map_or(0, |layout| layout.indent.saturating_sub(2))).max(1);
                let mut block = TranscriptRenderer::new(&profile)?.literal(previous.id.clone(),
                    &tool_group_label(calls.iter().copied(), false, now_ms), 2)?;
                block.metadata = previous.metadata.clone();
                for span in &mut block.metadata.decoration {
                    if span.range.end.row == previous.text.row_count() { span.range.end.row = block.text.row_count(); }
                }
                blocks.push(block);
            }
        }
    }
    Ok(blocks)
}

pub(super) fn message_block(previous: &BufferBlock, message: &crate::turn::Message,
    width: &WidthProfile) -> Result<ProjectedEntry> {
    let indent = previous.metadata.layout.as_ref().map_or(2, |layout|layout.indent);
    let mut content_width = width.clone();
    content_width.columns = content_width.columns.saturating_sub(indent.saturating_sub(2)).max(1);
    let mut renderer = TimelineRenderer { width:&content_width,margin:0,now_ms:0,block:Vec::new(),
        prompt:Vec::new(),action:HashMap::new(),tool:HashMap::new(),syntax:HashMap::new(),
        leading_separator:false,expansion:&HashMap::new(),tool_limit:&HashMap::new() };
    renderer.markdown(&previous.id.0,message.text(),
        if message.delivery() == crate::turn::MessageDelivery::Final { MarkdownRole::Response }
        else { MarkdownRole::Commentary })?;
    let block = &mut renderer.block[0];
    block.metadata.layout = previous.metadata.layout.clone();
    if let Some(layout) = &mut block.metadata.layout { layout.source_indent = 0; }
    super::layout::materialize(block)?;
    block.metadata.fold = previous.metadata.fold.clone();
    block.metadata.node = previous.metadata.node.clone();
    for fold in &mut block.metadata.fold {
        if fold.end.block == block.id && fold.end.position.row == previous.text.row_count() {
            fold.end.position.row = block.text.row_count();
        }
    }
    Ok(ProjectedEntry { entry:TranscriptEntry { id:previous.id.0.clone(),block:renderer.block },
        prompt:renderer.prompt,action:renderer.action,tool:renderer.tool,syntax:renderer.syntax })
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
        ProjectionSource::Exchange(exchange) => projection.interaction(exchange, &HashMap::new(), 0)?,
    }
    super::nodes::link(&mut projection.block);
    for block in &mut projection.block {
        TranscriptRenderer::resolve_marker(block)?;
        super::layout::materialize(block)?;
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
        ensure!(depth <= 16, "agent transcript nesting exceeds 16 levels");
        match entry {
            TimelineEntry::Exchange {
                exchange: interaction,
                agent_by_id,
                ..
            } => {
                let agents = agent_by_id.iter().map(|(id, entry)| (id.as_str(), entry)).collect();
                self.interaction(interaction, &agents, depth)?;
            }
            TimelineEntry::Checks { run, .. } => self.checks(run)?,
            TimelineEntry::SessionEvent { id, event, .. } => {
                let text = format!("◇ {}", session_event_text(&event.detail));
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
                    SessionPhase::Paused => "Paused".into(),
                    SessionPhase::Finalizing { error, .. } => error.as_ref().map_or_else(
                        || "Saving exchange".into(),
                        |error| format!("Saving exchange failed · {error}"),
                    ),
                    SessionPhase::Working {
                        started_at_ms,
                        reasoning_summary,
                        activity,
                        execution,
                    } => {
                        let elapsed_seconds =
                            self.now_ms.saturating_sub(*started_at_ms).max(0) / 1_000;
                        working_status_text(
                            self.width,
                            elapsed_seconds,
                            reasoning_summary.as_deref(),
                            *activity,
                            execution.as_ref(),
                        )?
                    }
                    SessionPhase::AwaitingInput { .. } => "Waiting for your answer".into(),
                    SessionPhase::AwaitingPlanReview { revision, .. } => {
                        format!("Waiting for plan review · revision {revision}")
                    }
                    SessionPhase::RetryingPlanGeneration { activity, started_at_ms, .. } => {
                        let elapsed = self.now_ms.saturating_sub(*started_at_ms).max(0) / 1_000;
                        format!("{} · {elapsed}s", activity.active_label())
                    }
                    SessionPhase::PlanningFailed { reason, .. } => {
                        format!("Planning stopped · {reason}")
                    }
                    SessionPhase::WaitingForAgent { agent_count } => {
                        format!("Waiting for {agent_count} agents")
                    }
                };
                let text = if text.is_empty() {
                    text
                } else {
                    format!("\n{text}")
                };
                let mut block = TranscriptRenderer::new(&self.content_width())?.literal(
                    BlockId(id.clone()),
                    &text,
                    0,
                )?;
                if status.visible() {
                    block.metadata.status = Some(StatusPresentation {
                        row: 1,
                        animated: matches!(status, SessionPhase::Working { .. }
                            | SessionPhase::RetryingPlanGeneration { .. } | SessionPhase::WaitingForAgent { .. }
                            | SessionPhase::Finalizing { error: None, .. }),
                        hint: match status {
                            SessionPhase::Working { .. } | SessionPhase::RetryingPlanGeneration { .. }
                                | SessionPhase::WaitingForAgent { .. } => Some(StatusHint::Working),
                            SessionPhase::AwaitingInput { .. } => Some(StatusHint::Question),
                            SessionPhase::AwaitingPlanReview { .. } => Some(StatusHint::Review),
                            _ => None,
                        },
                    });
                }
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
                    self.action.insert(
                        target,
                        TranscriptAction::Plan {
                            plan_id: plan_id.clone(),
                            revision: None,
                        },
                    );
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
                self.push(block)?;
                if let SessionPhase::Working { execution: Some(execution), .. } = status {
                    self.implementation_details(id, execution)?;
                }
            }
            TimelineEntry::AgentLifecycle {
                id,
                run,
                exchange: interaction,
                agent,
                ..
            } => {
                let task = interaction
                    .first()
                    .map(|exchange| exchange.prompt.as_str())
                    .unwrap_or_default();
                self.agent_entry(id, run, task, interaction, agent, None, depth)?;
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
                    format!("● You answered: {answer}")
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
                if let Some(question) = lifecycle.question.as_ref().filter(|_| !matches!(
                    lifecycle.kind, PlanLifecycleKind::QuestionAnswered | PlanLifecycleKind::QuestionWithdrawn
                )) {
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
                if let Some(comment) = lifecycle.overall_comment.as_ref().filter(|_|
                    lifecycle.kind != PlanLifecycleKind::ChangesRequested) {
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
                    PlanExecutionLifecycleEvent::Phase { title, revision, .. } => format!("{title} · revision {revision}"),
                    PlanExecutionLifecycleEvent::Completed { title } => format!("Plan complete: {title}"),
                    PlanExecutionLifecycleEvent::Failed { title, reason } => format!("Plan failed: {title} — {}", reason.replace(['\n', '\r'], " ")),
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
        task: &str,
        interaction: &[Exchange],
        agent: &[TimelineEntry],
        exchange_id: Option<&str>,
        depth: usize,
    ) -> Result<()> {
        ensure!(depth <= 16, "agent transcript nesting exceeds 16 levels");
        self.literal(
            id,
            &agent_summary(
                run,
                exchange_id.and_then(|id| interaction.iter().find(|exchange| exchange.id == id)),
                self.now_ms,
            ),
            Some(TranscriptAction::Agent {
                run_id: run.id.clone(),
            }),
        )?;
        let section = self.begin_section(2);
        self.literal(&format!("{id}:task"), &format!("↳ {}", task), None)?;
        let children: HashMap<_, _> = agent
            .iter()
            .filter_map(|entry| match entry {
                TimelineEntry::AgentLifecycle { id, .. } => Some((id.as_str(), entry)),
                _ => None,
            })
            .collect();
        for interaction in interaction
            .iter()
            .filter(|interaction| exchange_id.is_none_or(|id| interaction.id == id))
        {
            self.interaction(interaction, &children, depth + 1)?;
        }
        self.finish_section(section, id, false);
        Ok(())
    }

    fn question(&mut self, id: &str, question: &crate::plan::PlanQuestionSet, answer: Option<&str>) -> Result<()> {
        let header = question.questions.iter().map(|question| question.header.as_str()).collect::<Vec<_>>().join(", ");
        self.literal(id, &format!("▸ Question presented: {header}"), Some(TranscriptAction::Question {
            question_set_id: question.id.clone(),
        }))?;
        let section = self.begin_section(2);
        for (index, question) in question.questions.iter().enumerate() {
            self.markdown(&format!("{id}:question:{index}"), &question.question, MarkdownRole::Detail)?;
            for (option, value) in question.options.iter().enumerate() {
                self.literal(&format!("{id}:question:{index}:option:{option}"),
                    &format!("{}: {}", value.label, value.description), None)?;
            }
        }
        if let Some(answer) = answer {
            self.markdown(&format!("{id}:answer"), answer, MarkdownRole::Detail)?;
        }
        self.finish_section(section, id, true);
        Ok(())
    }

    fn implementation_details(&mut self, status_id: &str, execution: &ExecutionStatus) -> Result<()> {
        let identity = format!("{status_id}:implementation:{}", execution.id);
        self.literal(&identity, "▸ Implementation details", None)?;
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
            let state = findings.iter().map(|(state, _)| *state).collect::<std::collections::BTreeSet<_>>()
                .into_iter().collect::<Vec<_>>().join(" · ");
            self.literal(&file_id, &format!("▸ {path} · {state}"), None)?;
            let detail_section = self.begin_section(2);
            for (index, (state, detail)) in findings.iter().enumerate() {
                self.literal(&format!("{file_id}:finding:{index}"),
                    if detail.is_empty() { state } else { detail }, None)?;
            }
            self.finish_section(detail_section, &file_id, true);
        }
        if !progress.matched.is_empty() {
            let matched_id = format!("{identity}:matched");
            self.literal(&matched_id, &format!("▸ Matched files · {}", progress.matched.len()), None)?;
            let matched_section = self.begin_section(2);
            for path in &progress.matched {
                self.literal(&format!("{identity}:{}", crate::plan::digest(path.as_bytes())), path, None)?;
            }
            self.finish_section(matched_section, &matched_id, true);
        }
        if file.is_empty() && progress.matched.is_empty() {
            self.literal(&format!("{identity}:pending"), "Waiting for file comparison", None)?;
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
        if depth == 0 && self.leading_separator {
            self.literal(&format!("{}:separator", interaction.id), "", None)?;
        }
        let layout = super::layout::ExchangeLayout::new(interaction)?;
        self.content(interaction, agents, depth, &layout.opening, Some(&layout.questions), MarkdownRole::Response)?;
        if let Some(lifecycle) = &interaction.lifecycle {
            self.literal(&format!("{}:lifecycle", interaction.id), &format!("◇ {lifecycle}"), None)?;
        } else if !interaction.prompt.is_empty() {
            let prompt = TranscriptRenderer::new(&self.content_width())?.prompt(
                BlockId(format!("{}:prompt", interaction.id)), &interaction.prompt,
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
        let section = self.begin_section(0);
        self.content(interaction, agents, depth, &layout.activity, Some(&layout.questions), MarkdownRole::Response)?;
        let exchange_start = section.start;
        self.finish_section(
            section,
            &format!("{}:exchange", interaction.id),
            interaction.completed_at_ms.is_some(),
        );
        if let Some(node) = &mut self.block[exchange_start].metadata.node {
            node.kind = NodeKind::Exchange;
            node.lifecycle = if interaction.completed_at_ms.is_some() { NodeLifecycle::Settled } else { NodeLifecycle::Live };
        }
        self.content(interaction, agents, depth, &layout.results, Some(&layout.questions), MarkdownRole::Response)?;
        self.verification_result(interaction)?;
        let reported = if interaction.completed_at_ms.is_some()
            && interaction.checkpoint_after.is_none() && interaction.attributed_diff_text.is_none()
        {
            reported_changes(interaction, agents)
        } else { None };
        if let Some(diff) = interaction.attributed_diff_text.as_ref().or(reported.as_ref()) {
            self.diff(
                &format!("{}:changes", interaction.id),
                "Changed",
                diff,
                if interaction.checkpoint_after.is_none() {
                    " · reported edits"
                } else if interaction.attributed_matches_checkpoint {
                    " · checkpoint matched"
                } else {
                    ""
                },
                None,
                None,
            )?;
        }
        if !interaction.attributed_matches_checkpoint {
            if let Some(diff) = &interaction.checkpoint_diff_text {
                self.diff(
                    &format!("{}:checkpoint", interaction.id),
                    "Checkpoint total:",
                    diff,
                    "",
                    None,
                    None,
                )?;
            }
        }
        self.content(interaction, agents, depth, &layout.conclusion, Some(&layout.questions), MarkdownRole::Response)?;
        self.content(interaction, agents, depth, &layout.continuation, Some(&layout.questions), MarkdownRole::Response)?;
        Ok(())
    }

    fn verification_result(&mut self, interaction: &Exchange) -> Result<()> {
        use crate::plan::{PlanPhase, execution::VerificationOutcome};
        let Some(phase) = interaction.execution_phase.as_ref()
            .filter(|phase| phase.phase == PlanPhase::Verify) else { return Ok(()); };
        let Some(outcome) = phase.outcome else { return Ok(()); };
        let identity = format!("{}:verification-result", interaction.id);
        if outcome == VerificationOutcome::Passed && phase.verification.as_ref().is_none_or(|report| report.checks.is_empty()) {
            let summary = phase.summary.as_deref().unwrap_or("")
                .split_whitespace().collect::<Vec<_>>().join(" ");
            return self.literal(&identity, &format!("◇ Verification passed{}",
                if summary.is_empty() { String::new() } else { format!(" · {summary}") }), None);
        }
        let label = match outcome { VerificationOutcome::Failed => "Verification failed",
            VerificationOutcome::Blocked => "Verification blocked", VerificationOutcome::Passed => "Verification passed" };
        let checks = phase.verification.as_ref().map_or(&[][..], |report| report.checks.as_slice());
        let failed = checks.iter().filter(|check| check.outcome == VerificationOutcome::Failed).count();
        let blocked = checks.iter().filter(|check| check.outcome == VerificationOutcome::Blocked).count();
        let mut totals = Vec::new();
        if failed > 0 { totals.push(format!("{failed} failed check{}", if failed == 1 { "" } else { "s" })); }
        if blocked > 0 { totals.push(format!("{blocked} blocked check{}", if blocked == 1 { "" } else { "s" })); }
        if !phase.declaration_findings.is_empty() { totals.push(format!("{} declaration mismatch{}", phase.declaration_findings.len(), if phase.declaration_findings.len() == 1 { "" } else { "es" })); }
        let suffix = if totals.is_empty() { String::new() } else { format!(" · {}", totals.join(", ")) };
        self.literal(&identity, &format!("▸ {label}{suffix}"), None)?;
        let section = self.begin_section(2);
        for (category, label) in [(crate::plan::execution::VerificationCategory::Automated, "Automated"),
            (crate::plan::execution::VerificationCategory::Manual, "Manual")] {
            let group_checks = checks.iter().filter(|check| check.category == category).collect::<Vec<_>>();
            if group_checks.is_empty() { continue; }
            let group_id = format!("{identity}:{label}");
            let counts = [(VerificationOutcome::Passed, "passed"), (VerificationOutcome::Failed, "failed"), (VerificationOutcome::Blocked, "blocked")]
                .into_iter().filter_map(|(outcome, label)| {
                    let count = group_checks.iter().filter(|check| check.outcome == outcome).count();
                    (count > 0).then(|| format!("{count} {label}"))
                }).collect::<Vec<_>>().join(", ");
            self.literal(&group_id, &format!("▸ {label} · {counts}"), None)?;
            let group = self.begin_section(2);
            for (index, check) in group_checks.into_iter().enumerate() {
                let check_id = format!("{group_id}:{index}");
                let outcome = match check.outcome { VerificationOutcome::Passed => "passed", VerificationOutcome::Failed => "failed", VerificationOutcome::Blocked => "blocked" };
                self.literal(&check_id, &format!("▸ {} · {outcome}", check.label.replace(['\r', '\n'], " ")), None)?;
                let check_section = self.begin_section(2);
                self.markdown(&format!("{check_id}:summary"), check.summary.trim_end(), MarkdownRole::Detail)?;
                for (evidence_index, evidence) in check.evidence.iter().enumerate() {
                    let call_id = phase.verification_tools.get(evidence).cloned()
                        .unwrap_or_else(|| format!("{}:{evidence}", interaction.id));
                    self.literal(&format!("{check_id}:evidence:{evidence_index}"), "Command output",
                        Some(TranscriptAction::Tool { call_id }))?;
                }
                self.finish_section(check_section, &check_id, true);
            }
            self.finish_section(group, &group_id, true);
        }
        if !phase.declaration_findings.is_empty() || !phase.declaration_changes.is_empty() {
            let declaration_id = format!("{identity}:declarations");
            let count = phase.declaration_findings.len().max(phase.declaration_changes.len());
            self.literal(&declaration_id, &format!("▸ Declarations · {count} mismatch{}", if count == 1 { "" } else { "es" }), None)?;
            let declarations = self.begin_section(2);
            let mut files = std::collections::BTreeMap::<&str, Vec<&crate::plan::execution::DeclarationMismatch>>::new();
            for change in &phase.declaration_changes { files.entry(&change.path).or_default().push(change); }
            for (file_index, (path, changes)) in files.into_iter().enumerate() {
                let file_id = format!("{declaration_id}:{file_index}");
                self.literal(&file_id, &format!("▸ {path} · {} mismatch{}", changes.len(), if changes.len() == 1 { "" } else { "es" }), None)?;
                let file = self.begin_section(2);
                for (index, change) in changes.iter().enumerate() {
                    let tree = super::changes::ChangeTree::declaration(&TranscriptRenderer::new(&self.content_width())?,
                        &format!("{file_id}:{index}"), &change.name, &change.diff)?;
                    self.action.extend(tree.action);
                    for block in tree.block { self.push(block)?; }
                }
                self.finish_section(file, &file_id, true);
            }
            for (index, finding) in phase.declaration_findings.iter().enumerate() {
                if phase.declaration_changes.iter().any(|change| finding == &change.finding) { continue; }
                self.markdown(&format!("{declaration_id}:finding:{index}"), finding, MarkdownRole::Detail)?;
            }
            self.finish_section(declarations, &declaration_id, true);
        }
        let findings = phase.verification.as_ref().map_or(phase.findings.as_slice(), |report| report.findings.as_slice());
        if !findings.is_empty() {
            let finding_id = format!("{identity}:findings");
            self.literal(&finding_id, &format!("▸ Findings · {}", findings.len()), None)?;
            let finding_section = self.begin_section(2);
            for (index, finding) in findings.iter().enumerate() {
                self.markdown(&format!("{finding_id}:{index}"), finding, MarkdownRole::Detail)?;
            }
            self.finish_section(finding_section, &finding_id, true);
        }
        if checks.is_empty() && findings.is_empty() && phase.declaration_findings.is_empty() {
            if let Some(reason) = phase.verification.as_ref().and_then(|report| report.reason.as_deref())
                .or(phase.summary.as_deref()) {
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
        let mut question_turn = std::collections::HashSet::new();
        let mut preceding_turn = None;
        for node in interaction.node_list.iter() {
            if let ExchangeNode::TurnContent { turn_id, .. } = node {
                preceding_turn = Some(turn_id.as_str());
            }
            let question = match node {
                ExchangeNode::QuestionPresented { .. } => true,
                ExchangeNode::PlanEvent { event } => matches!(&event.content,
                    crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                        if lifecycle.kind == crate::plan::PlanLifecycleKind::QuestionAsked),
                _ => false,
            };
            if question {
                if let Some(turn_id) = preceding_turn { question_turn.insert(turn_id); }
            }
        }
        let mut rendered_tool = std::collections::HashSet::new();
        for (position, node) in visible.iter().enumerate() {
            if let Some(group) = questions.and_then(|history| history.group.get(node.id())) {
                self.question_group(group, interaction, agents, depth)?;
                continue;
            }
            match node {
                ExchangeNode::TurnContent { id, turn_id, item } => {
                    let turn = interaction
                        .turn
                        .iter()
                        .find(|turn| turn.id() == turn_id)
                        .context("timeline content references a missing provider turn")?;
                    match item {
                        crate::turn::TurnItem::Message { id: message_id } => {
                            let message = turn
                                .messages()
                                .iter()
                                .find(|message| message.id() == message_id)
                                .context("timeline content references a missing message")?;
                            if matches!(
                                message.kind(),
                                crate::turn::MessageKind::Reasoning
                                    | crate::turn::MessageKind::ReasoningSummary
                            ) {
                                continue;
                            }
                            let final_message = message.delivery() == crate::turn::MessageDelivery::Final;
                            if final_message {
                                self.markdown(id, message.text(), response_role)?;
                            } else {
                                let start = self.block.len();
                                self.margin += 2;
                                self.markdown(id, message.text(), MarkdownRole::Commentary)?;
                                self.margin -= 2;
                                self.offset_layout(start, 1);
                            }
                        }
                        crate::turn::TurnItem::Tool { id: tool_id } => {
                            if rendered_tool.contains(id) {
                                continue;
                            }
                            let tool = turn.tools().find(|tool| tool.id == *tool_id)
                                .context("timeline content references a missing tool")?;
                            if tool.kind == "file_change" {
                                let start = self.block.len();
                                self.margin += 2;
                                self.file_change(id, tool)?;
                                self.margin -= 2;
                                self.offset_layout(start, 1);
                                continue;
                            }
                            let group = visible.iter().skip(position).take_while(|node| match node {
                                ExchangeNode::TurnContent { turn_id: owner, item: crate::turn::TurnItem::Tool { id }, .. }
                                    if owner == turn_id => turn.tools().find(|tool| tool.id == *id)
                                        .is_some_and(|tool| tool.kind != "file_change"),
                                _ => false,
                            });
                            let mut calls = Vec::new();
                            for node in group {
                                let ExchangeNode::TurnContent {
                                    id,
                                    item: crate::turn::TurnItem::Tool { id: tool_id },
                                    ..
                                } = node
                                else {
                                    unreachable!()
                                };
                                rendered_tool.insert(id.clone());
                                let tool = turn
                                    .tools()
                                    .find(|tool| tool.id == *tool_id)
                                    .context("timeline content references a missing tool")?;
                                let question_control = tool.title.split('(').next().unwrap_or("").trim()
                                    .ends_with("harness_question_ask");
                                if !(question_control && question_turn.contains(turn_id.as_str()) && !tool.failed
                                    && tool.state() == crate::turn::ToolState::Completed) {
                                    calls.push((id, tool));
                                }
                            }
                            if calls.is_empty() { continue; }
                            let start = self.block.len();
                            self.margin += 2;
                            let count = calls.len();
                            let last_tool_id = &calls.last().expect("nonempty tool group").1.id;
                            let followed_in_turn = turn.items().iter().rposition(|item| matches!(item,
                                crate::turn::TurnItem::Tool { id } if id == last_tool_id
                            )).is_some_and(|position| position + 1 < turn.items().len());
                            let settled = interaction.completed_at_ms.is_some() || calls
                                .iter()
                                .all(|(_, tool)| tool.state() != crate::turn::ToolState::Running)
                                && (followed_in_turn
                                    || turn.state() != crate::turn::TurnState::Running);
                            let group_id = format!("{id}:tools");
                            let label = tool_group_label(calls.iter().map(|(_, tool)| *tool), settled, self.now_ms);
                            self.literal(&group_id, &label, None)?;
                            for (index, (_, tool)) in calls.into_iter().enumerate() {
                                let tool_start = self.block.len();
                                self.tool(turn.id(), tool, &group_id, !settled && index + 1 == count)?;
                                let node = format!("{}:{}:tool", turn.id(), tool.id);
                                self.fold(tool_start, &node, settled || index + 1 < count, NodeKind::Tool);
                                if let Some(node) = &mut self.block[tool_start].metadata.node {
                                    node.lifecycle = if tool.state() == crate::turn::ToolState::Running { NodeLifecycle::Live } else { NodeLifecycle::Settled };
                                    if !settled && index + 1 == count { node.default_display = NodeDisplay::Preview; }
                                    if self.expansion.get(&node.id.0) == Some(&true) {
                                        let output = &self.tool[&format!("{}:{}", turn.id(), tool.id)];
                                        let limit = self.tool_limit.get(&node.id.0).copied().unwrap_or(super::tool::INITIAL_INLINE_BYTES);
                                        node.more = output.inline_prefix_chunks(limit)? < output.inline_chunks()?;
                                    }
                                    node.resolve(self.expansion.get(&node.id.0).copied());
                                }
                            }
                            self.fold(start, &format!("{id}:tools"), settled, NodeKind::ToolGroup);
                            self.margin -= 2;
                            self.offset_layout(start, 1);
                        }
                    }
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
                        prompt.text.trim().strip_prefix("Planning feedback:").unwrap_or(&prompt.text)
                            .trim().trim_start_matches("- ").replace("\n- ", ", ")
                    } else { prompt.text.clone() };
                    let width = self.content_width();
                    let renderer = TranscriptRenderer::new(&width)?;
                    let block = renderer.prompt(BlockId(format!("{}:prompt", prompt.id)), &format!("{label}: {text}"))?;
                    self.prompt.push(block.id.clone());
                    self.push(block)?;
                }
                ExchangeNode::AgentReference { agent } => {
                    if let Some(entry) = agents
                        .get(agent.id.as_str())
                        .or_else(|| agents.get(agent.child_agent_id.as_str())).copied()
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
                ExchangeNode::QuestionPresented { id, question, answer } => {
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
                ExchangeNode::ImplementationReport { id, reference, report } => {
                    self.literal(id, &format!("▸ {}", reference.summary()), None)?;
                    let section = self.begin_section(2);
                    if let Some(report) = report {
                        self.markdown(&format!("{id}:revisions"), &format!("Original revision {} · accepted revision {}\n{}",
                            report.original_revision, report.accepted_revision,
                            report.revisions.iter().map(|revision| format!("- Revision {} ({}): {}", revision.revision, revision.approval, revision.reason)).collect::<Vec<_>>().join("\n")), MarkdownRole::Detail)?;
                        for (index, file) in report.file.iter().enumerate() {
                            let file_id = format!("{id}:file:{index}");
                            self.literal(&file_id, &format!("▸ {}", file.path), None)?;
                            let file_section = self.begin_section(2);
                            let mut lines = Vec::new();
                            for detail in &file.contract { lines.push(format!("- Contract: {detail}")); }
                            for helper in &file.internal { lines.push(format!("- Internal addition: `{helper}`")); }
                            for difference in &file.references {
                                lines.push(format!("- `{}` — {}{}", difference.owner, difference.category,
                                    if difference.expected.is_none() { " (unspecified in plan)" } else { "" }));
                                if let Some(expected) = &difference.expected { lines.push(format!("  Planned: {}", expected.join(", "))); }
                                lines.push(format!("  Observed: {}", difference.observed.join(", ")));
                            }
                            self.markdown(&format!("{file_id}:details"), &lines.join("\n"), MarkdownRole::Detail)?;
                            self.finish_section(file_section, &file_id, true);
                        }
                        for (index, unavailable) in report.unavailable.iter().enumerate() {
                            self.markdown(&format!("{id}:unavailable:{index}"), &format!("Unavailable: {unavailable}"), MarkdownRole::Detail)?;
                        }
                    } else {
                        self.markdown(&format!("{id}:unavailable"), "Report content is unavailable. The saved summary is retained.", MarkdownRole::Detail)?;
                    }
                    self.finish_section(section, id, true);
                }
                ExchangeNode::ArtifactChange { change } => {
                    if let Some(declaration) = &change.declaration {
                        let revision_request = interaction.kind == ExchangeKind::PlanExecution;
                        let revision_section = if revision_request {
                            self.literal(&format!("{}:request", change.id),
                                &format!("▸ Plan revision requested · revision {}", declaration.revision), None)?;
                            Some(self.begin_section(2))
                        } else { None };
                        self.diff(&change.id, if revision_request { "Changes" } else { "Proposed changes" }, &change.diff_text, "", None, Some((declaration, false)))?;
                        self.diff(&format!("{}:document", change.id), "Plan overview", &declaration.document_diff, "",
                            None, Some((declaration, true)))?;
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

    fn question_group(
        &mut self,
        group: &super::question::QuestionGroup<'_>,
        exchange: &Exchange,
        agents: &HashMap<&str, &TimelineEntry>,
        depth: usize,
    ) -> Result<()> {
        let answered = group.branch.iter().filter(|branch| branch.answer.is_some()).count();
        let label = if group.withdrawal.is_some() { "▸ Questions · Withdrawn".into() }
            else { format!("▸ Questions · {answered}/{} answered", group.branch.len()) };
        self.literal(group.id, &label,
            Some(TranscriptAction::Question { question_set_id: group.set.id.clone() }))?;
        let section = self.begin_section(2);
        for branch in &group.branch {
            let id = format!("{}:question:{}", group.id, branch.question.id);
            let status = branch.answer.as_deref().unwrap_or(if group.withdrawal.is_some() { "Withdrawn" }
                else if group.resolved { "Resolved" } else { "Awaiting answer" });
            self.literal(&id, &format!("▸ {} · {status}", branch.question.question),
                Some(TranscriptAction::Question { question_set_id: group.set.id.clone() }))?;
            let question_section = self.begin_section(2);
            for (index, option) in branch.question.options.iter().enumerate() {
                self.literal(&format!("{id}:option:{index}"), &format!("○ {}: {}", option.label, option.description), None)?;
            }
            for (index, clarification) in branch.clarification.iter().enumerate() {
                self.clarification(clarification, exchange, agents, depth,
                    group.resolved || index + 1 < branch.clarification.len())?;
            }
            if let Some(answer) = &branch.answer {
                let mut block = TranscriptRenderer::new(&self.content_width())?.literal(
                    BlockId(format!("{id}:answer")), &format!("● You answered: {answer}"), 2)?;
                TranscriptRenderer::heading_marker(&mut block, "ForgeHarnessPrompt");
                block.metadata.decoration.push(Decoration {
                    range: TextRange { start: TextPosition { row: 0, column: 0 },
                        end: TextPosition { row: block.text.row_count(), column: 0 } },
                    capture: "ForgeHarnessPrompt".into(), priority: 100,
                });
                self.prompt.push(block.id.clone());
                self.push(block)?;
            }
            self.finish_section(question_section, &id, group.resolved);
        }
        for clarification in &group.clarification {
            self.clarification(clarification, exchange, agents, depth, group.resolved)?;
        }
        if let Some(reason) = group.withdrawal {
            self.markdown(&format!("{}:withdrawal", group.id),
                &format!("Clarification withdrawn: {reason}"), MarkdownRole::Detail)?;
        }
        self.finish_section(section, group.id, group.resolved);
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
            BlockId(id.clone()), &format!("You asked: {}", clarification.prompt.text))?;
        self.prompt.push(block.id.clone());
        self.push(block)?;
        let section = self.begin_section(2);
        self.content(exchange, agents, depth, &clarification.content, None, MarkdownRole::Message)?;
        self.finish_section(section, &id, closed);
        Ok(())
    }

    fn file_change(&mut self, id: &str, tool: &crate::exchange::ToolCall) -> Result<()> {
        use crate::turn::ToolState;
        let identity = format!("{id}:changes");
        if let Some(diff) = crate::exchange::ProviderDiffBuilder::build(std::slice::from_ref(tool)) {
            return self.diff(&identity, "Changed", &diff, "", None, None);
        }
        let label = match tool.state() {
            ToolState::Running => {
                let count = tool.change.file.len();
                if count == 0 { "Changing files".into() }
                else { format!("Changing {count} {}", if count == 1 { "file" } else { "files" }) }
            }
            ToolState::Completed if !tool.failed => "Changed files · details unavailable".into(),
            ToolState::Completed | ToolState::Failed => "File changes failed".into(),
            ToolState::Cancelled => "File changes cancelled".into(),
            ToolState::Interrupted => "File changes interrupted".into(),
        };
        let details = tool.state() != ToolState::Running && !tool.output.trim().is_empty();
        self.literal(&identity, &format!("{} {label}", if details { "▸" } else { "◇" }), None)?;
        if details {
            let section = self.begin_section(2);
            self.markdown(&format!("{identity}:details"), &tool.output, MarkdownRole::Detail)?;
            self.finish_section(section, &identity, true);
        }
        Ok(())
    }

    fn checks(&mut self, run: &crate::plan::checks::CheckRun) -> Result<()> {
        let start = self.block.len();
        self.literal(&format!("{}:checks", run.id), &format!("Checks · {:?}", run.state), None)?;
        let declarations_start=self.block.len();
        self.literal(&format!("{}:declarations", run.id),
            &format!("Declarations · {}", if run.progress.conforms() { "match" } else { "differ" }), None)?;
        for (index, finding) in run.progress.findings().iter().enumerate() {
            self.literal(&format!("{}:finding:{index}",run.id), finding, None)?;
        }
        for (index,change) in run.progress.declaration_changes.iter().enumerate() {
            self.diff(&format!("{}:declaration:{index}",run.id),&change.name,&change.diff,"",Some((&change.path,&change.name)),None)?;
        }
        self.fold(declarations_start,&format!("{}:declarations",run.id),true,NodeKind::Group);
        for check in &run.commands {
            let command_start = self.block.len();
            let call_id = check.id.clone();
            let heading = crate::turn::ToolCall {
                id:call_id.clone(), kind:"check".into(), title:format!("{} · {:?}",
                    crate::permissions::command::display_command(&check.command,
                        crate::permissions::shell::CommandShell::default()).source,check.state),
                started_at_ms:check.started_at_ms, completed_at_ms:check.completed_at_ms,
                task_id:None,output:String::new(),status:String::new(),failed:matches!(check.state,crate::plan::checks::CheckState::Failed),change:Default::default(),
            };
            let mut output = ToolOutputView::file(call_id.clone(),check.output.clone(),check.output_bytes)?;
            output.heading(&heading);
            output.group = format!("{}:checks", run.id);
            let expanded = self.expansion.get(&format!("{call_id}:tool")).copied()==Some(true);
            let limit=self.tool_limit.get(&format!("{call_id}:tool")).copied().unwrap_or(super::tool::INITIAL_INLINE_BYTES);
            if expanded { output.load_prefix(limit)?; }
            let width=self.content_width();
            let renderer=TranscriptRenderer::new(&width)?;
            self.push(renderer.tool_header(BlockId(format!("{call_id}:tool")),TargetId(format!("{call_id}:tool")),
                "check",heading.elapsed_ms(self.now_ms),heading.failed,&heading.title,expanded)?)?;
            if expanded {
                for chunk in 0..output.inline_prefix_chunks(limit)? { self.push(renderer.tool_body(&call_id,&output,true,chunk)?)?; }
                self.push(renderer.tool_hidden(&call_id,&output,true)?)?;
            } else {
                let preview=run.state==crate::plan::checks::CheckState::Running && check.state==crate::plan::checks::CheckState::Running
                    && self.expansion.get(&format!("{call_id}:tool"))!=Some(&false);
                if preview {
                    let visible=ToolOutputView::new(call_id.clone(),&check.preview)?;
                    self.push(renderer.tool_body(&call_id,&visible,false,0)?)?;
                } else { self.push(BufferBlock { id:BlockId(format!("{call_id}:preview")),text:Default::default(),metadata:Default::default() })?; }
                self.push(BufferBlock { id:BlockId(format!("{call_id}:hidden")),text:Default::default(),metadata:Default::default() })?;
            }
            if let Some(error)=&check.error { self.literal(&format!("{call_id}:error"),error,None)?; }
            self.fold(command_start,&format!("{call_id}:tool"),true,NodeKind::Tool);
            if let Some(node)=&mut self.block[command_start].metadata.node { node.more=expanded && output.has_unloaded_output();
                if check.state==crate::plan::checks::CheckState::Running {
                    node.lifecycle=NodeLifecycle::Live;
                    node.default_display=NodeDisplay::Preview;
                    node.resolve(self.expansion.get(&node.id.0).copied());
                } }
            self.tool.insert(call_id,output);
        }
        self.fold(start,&format!("{}:checks",run.id),run.state != crate::plan::checks::CheckState::Running,NodeKind::Group);
        if let Some(title)=&run.completed_plan { self.literal(&format!("{}:complete",run.id), &format!("◇ Plan complete: {title}"),None)?; }
        Ok(())
    }

    fn tool(&mut self, interaction: &str, tool: &crate::exchange::ToolCall, group: &str, preview: bool) -> Result<()> {
        let call_id = format!("{interaction}:{}", tool.id);
        ensure!(
            !self.tool.contains_key(&call_id),
            "duplicate tool identity in interaction"
        );
        let mut output = ToolOutputView::new(call_id.clone(), &tool.output)?;
        output.heading(tool);
        output.group = group.to_owned();
        let id = format!("{call_id}:tool");
        let target = TargetId(id.clone());
        let label = tool.title.clone();
        let width = self.content_width();
        let renderer = TranscriptRenderer::new(&width)?;
        let choice = self.expansion.get(&format!("{call_id}:tool")).copied();
        let expanded = choice == Some(true);
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
            let limit = self.tool_limit.get(&format!("{call_id}:tool")).copied().unwrap_or(super::tool::INITIAL_INLINE_BYTES);
            for chunk in 0..if expanded { output.inline_prefix_chunks(limit)? } else { 1 } {
                let block = renderer.tool_body(&call_id, &output, expanded, chunk)?;
                for target in &block.metadata.target { self.action.insert(target.id.clone(), TranscriptAction::Tool { call_id: call_id.clone() }); }
                self.push(block)?;
            }
            let hidden = renderer.tool_hidden(&call_id, &output, expanded)?;
            for target in &hidden.metadata.target { self.action.insert(target.id.clone(), TranscriptAction::Tool { call_id: call_id.clone() }); }
            self.push(hidden)?;
        } else {
            for suffix in ["preview", "hidden"] {
                self.push(BufferBlock {
                    id: BlockId(format!("{call_id}:{suffix}")), text: Default::default(), metadata: Default::default(),
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
                let mut rendered = forge_buffer::markdown::MarkdownRenderer::source(BlockId(id.into()), text, &width)?;
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
                indent: 4, marker: Some(TextChunk { text: "◦".into(), capture: "Normal".into() }), source_indent: 0,
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
        rendered.block.metadata.node = Some(NodeState::new(FoldId(id.into()), NodeKind::Message, NodeDisplay::Full));
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
        block.metadata.layout.get_or_insert(ContentLayout { indent: 2, marker: None, source_indent: 0 });
        self.block.push(block);
        Ok(())
    }

    fn offset_layout(&mut self, start: usize, depth: usize) {
        for block in &mut self.block[start..] {
            block.metadata.layout.as_mut().expect("projected block layout").indent += depth * 2;
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
            FoldId(identity.into()), kind, if closed { NodeDisplay::Heading } else { NodeDisplay::Full }));
        let selected = &self.block[start..];
        if selected
            .iter()
            .map(|block| block.text.row_count())
            .sum::<usize>()
            < 2 && kind != NodeKind::Tool
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
            collapse_children: false, expand_children: false,
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
        use crate::exchange::ExchangeState;
        let seconds = exchange.elapsed(now_ms) / 1_000;
        let lifecycle = match exchange.state {
            ExchangeState::Queued => "queued for",
            ExchangeState::Running => {
                if exchange.awaiting_input {
                    "waiting for"
                } else {
                    "running for"
                }
            }
            ExchangeState::Finalizing => "finalizing after",
            ExchangeState::Complete => "completed in",
            ExchangeState::Failed => "failed after",
            ExchangeState::Cancelled => "cancelled after",
            ExchangeState::Interrupted => "interrupted after",
        };
        return format!(
            "▸ {}Agent {} {lifecycle} {seconds}s",
            history_prefix(exchange.disposition),
            run.label()
        );
    }

    use crate::agent::AgentState;

    let elapsed_seconds = now_ms
        .min(run.updated_at_ms.max(run.created_at_ms))
        .saturating_sub(run.created_at_ms)
        .max(0)
        / 1_000;
    let lifecycle = match run.state {
        AgentState::Starting => format!("starting for {elapsed_seconds}s"),
        AgentState::Ready => {
            let elapsed_seconds = now_ms.saturating_sub(run.created_at_ms).max(0) / 1_000;
            format!("ready for {elapsed_seconds}s")
        }
        AgentState::Closing => format!("closing after {elapsed_seconds}s"),
        AgentState::Closed => format!("closed after {elapsed_seconds}s"),
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

/// Distinguish workspace history from the preserved execution outcome.
fn history_prefix(disposition: crate::exchange::HistoryDisposition) -> &'static str {
    match disposition {
        crate::exchange::HistoryDisposition::Current => "",
        crate::exchange::HistoryDisposition::RolledBack => "Rolled back · ",
        crate::exchange::HistoryDisposition::Superseded => "Superseded · ",
    }
}

const WORKING_STATUS_RESERVED_CELLS: usize = 28;

fn working_status_text(
    width: &WidthProfile,
    elapsed_seconds: i64,
    reasoning_summary: Option<&str>,
    activity: WorkflowActivity,
    execution: Option<&ExecutionStatus>,
) -> Result<String> {
    if let Some(execution) = execution {
        let phase = WorkflowActivity::from(execution.phase).active_label();
        let progress = &execution.progress;
        let count = progress.missing.iter().chain(&progress.different).chain(&progress.unverified)
            .map(|finding| finding.split_once(": ").map_or(finding.as_str(), |(path, _)| path))
            .collect::<std::collections::BTreeSet<_>>().len();
        let attention = if count == 1 { "1 file needs attention".into() }
            else if count > 1 { format!("{count} files need attention") }
            else if progress.matched.is_empty() { "comparing files".into() }
            else { "all files matched".into() };
        return Ok(format!("{phase} · {elapsed_seconds}s · {attention}"));
    }
    let phase = activity.active_label();
    let prefix = format!("{phase} · {elapsed_seconds}s");
    let Some(reasoning_summary) = reasoning_summary else {
        return Ok(prefix);
    };
    let bounded = reasoning_summary.chars().take(4_096).collect::<String>();
    let normalized = bounded.split_whitespace().collect::<Vec<_>>().join(" ");
    if normalized.is_empty() {
        return Ok(prefix);
    }
    let occupied = width.cells(&prefix, 0)? + 4 + WORKING_STATUS_RESERVED_CELLS;
    let available = width.columns.saturating_sub(occupied);
    if available < 4 {
        return Ok(prefix);
    }
    let summary = truncate_to_cells(width, &normalized, available)?;
    Ok(format!("{prefix} · {summary}"))
}

fn truncate_to_cells(width: &WidthProfile, text: &str, max_cells: usize) -> Result<String> {
    if width.cells(text, 0)? <= max_cells {
        return Ok(text.to_owned());
    }
    let mut truncated = String::new();
    for character in text.chars() {
        truncated.push(character);
        if width.cells(&format!("{truncated}…"), 0)? > max_cells {
            truncated.pop();
            break;
        }
    }
    while !truncated.is_empty() && width.cells(&format!("{truncated}…"), 0)? > max_cells {
        truncated.pop();
    }
    truncated.push('…');
    Ok(truncated)
}

fn token_display(tokens: u64) -> String {
    match tokens {
        tokens if tokens >= 1000 => format!("{:.1}k", tokens as f64 / 1000.0),
        tokens => tokens.to_string(),
    }
}

fn exchange_activity_summary(interaction: &Exchange, now_ms: i64) -> String {
    let complete = interaction.completed_at_ms.is_some();
    let paused = interaction.state == ExchangeState::Running
        && !complete
        && interaction.execution_started_at_ms.is_none();
    let duration = interaction.elapsed(now_ms) / 1000;
    let activity = if interaction.state == ExchangeState::Cancelled {
        "Cancelled"
    } else if interaction.state == ExchangeState::Finalizing {
        "Saving exchange"
    } else if interaction.state == ExchangeState::Interrupted {
        "Interrupted"
    } else if interaction.state == ExchangeState::Failed {
        "Failed"
    } else {
        let activity = WorkflowActivity::from_exchange(interaction);
        match interaction.kind {
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision if interaction.awaiting_input || paused =>
                if activity == WorkflowActivity::Revising { "Revising plan paused" } else { "Planning paused" },
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision if complete => activity.completed_label(),
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision => activity.active_label(),
            ExchangeKind::PlanExecution if interaction.execution_phase.as_ref().is_some_and(|phase| phase.outcome.is_some()) => activity.completed_label(),
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
    let count: usize = interaction.turn.iter().map(|turn| turn.tools().count()).sum();
    let mut sections = vec![summary];
    if count > 0 {
        let passed = interaction.turn.iter().flat_map(|turn| turn.tools())
            .filter(|tool| tool.state() == crate::turn::ToolState::Completed && !tool.failed).count();
        let duration = interaction.metrics.tool_ms(interaction.elapsed(now_ms))
            .map(|duration| format!("{} · ", super::duration::duration_label(duration))).unwrap_or_default();
        sections.push(format!("Tools {duration}{passed}/{count} pass"));
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
        (Some(input), Some(output)) => tokens.push(format!("Tokens {input} -> {}", token_display(output))),
        (Some(input), None) => tokens.push(format!("Tokens in {input}")),
        (None, Some(output)) => tokens.push(format!("Tokens out {}", token_display(output))),
        (None, None) => {}
    }
    if let Some((output, duration)) = interaction.metrics.reported_output_tokens
        .zip(interaction.metrics.reported_response_ms)
        .filter(|(output, duration)| *output > 0 && *duration > 0 && interaction.metrics.timing_complete) {
        tokens.push(format!("{:.0} tps", output as f64 * 1000.0 / duration as f64));
    }
    if !tokens.is_empty() { sections.push(tokens.join(" · ")); }
    let mut summary = sections.join(" │ ");
    if let Some(error) = &interaction.finalization_error {
        summary.push_str(&format!(" — {}", error.replace(['\n', '\r'], " ")));
    }
    summary
}

#[cfg(test)]
mod tests {
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
        let projection = project_at(&TimelineEntry::Exchange { id:"verify".into(), created_at_ms:0,
            exchange, agent_by_id:HashMap::new() }, &WidthProfile::default(), 1000)?;
        let blocks = &projection.entry.block;
        let rows = blocks.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>();
        let text = rows.join("\n");
        assert!(text.contains("Verification failed · 1 failed check, 1 blocked check, 1 declaration mismatch"));
        assert!(text.contains("Automated · 1 passed, 1 failed"));
        assert!(text.contains("Manual · 1 blocked"));
        assert!(text.contains("Declarations · 1 mismatch"));
        assert!(text.contains("fn update(mut input: State);"));
        assert_eq!(text.matches("Expected FontSize").count(), 1);
        assert!(!text.contains("src/lib.rs: function update: changed"));
        assert!(!rows.iter().any(|row| row.is_empty()));
        for id in ["verify:verification-result:Automated", "verify:verification-result:Manual",
            "verify:verification-result:declarations", "verify:verification-result:Automated:0"] {
            let block = blocks.iter().find(|block| block.id.0 == id).unwrap();
            assert!(block.metadata.fold[0].closed);
        }
        assert!(projection.action.values().any(|action| matches!(action,
            TranscriptAction::Tool { call_id } if call_id == "earlier-exchange:test-tool")));
        assert!(!projection.action.values().any(|action| matches!(action, TranscriptAction::File { .. })));
        Ok(())
    }

    #[test]
    fn verification_result_retains_findings_outside_the_activity_fold() -> anyhow::Result<()> {
        use super::*;
        use serde_json::json;
        for outcome in ["failed", "blocked", "passed"] {
            let findings = if outcome == "passed" { vec![] } else {
                vec!["restart_resets_round: expected score 0, got 3", "pause_freezes_timer: remaining time changed"]
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
            let projected = project_at(&TimelineEntry::Exchange {
                id:"verify-result".into(), created_at_ms:0, exchange, agent_by_id:HashMap::new(),
            }, &WidthProfile::default(), 18000)?;
            let blocks = &projected.entry.block;
            let result_index = blocks.iter().position(|block| block.id.0 == "verify-result:verification-result").unwrap();
            let result = &blocks[result_index];
            assert!(blocks[0].text.wire_rows().join("\n").contains("Verified 18s"));
            assert!(blocks[0].metadata.fold.is_empty(), "result-only exchanges have no activity to fold");
            assert!(blocks[0].metadata.fold.iter().all(|fold| !blocks[result_index..].iter()
                .any(|block| block.id == fold.end.block)));
            let text = result.text.wire_rows().join("\n");
            if outcome == "passed" {
                assert!(text.contains("◇ Verification passed · Ran eight gameplay tests."));
                assert!(result.metadata.fold.is_empty());
            } else {
                assert!(!text.contains("expected score"));
                assert!(text.contains(if outcome == "failed" { "Verification failed" } else { "Verification blocked" }));
                assert!(result.metadata.fold[0].closed);
                assert!(blocks.iter().any(|block| block.text.wire_rows().join("\n")
                    .contains("pause_freezes_timer: remaining time changed")));
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
            value["completed_at_ms"] = if state == "running" { json!(null) } else { json!(1000) };
            let exchange: Exchange = serde_json::from_value(value).unwrap();
            let exchange = serde_json::from_slice(&serde_json::to_vec(&exchange).unwrap()).unwrap();
            let projected = project_at(&TimelineEntry::Exchange {
                id:"phase".into(), created_at_ms:0, exchange, agent_by_id:HashMap::new(),
            }, &WidthProfile::default(), 1000).unwrap();
            let blocks = &projected.entry.block;
            assert_eq!(blocks[0].id.0, "phase:lifecycle");
            assert_eq!(blocks[0].text.wire_rows().join("\n"), "◇ Plan implementation started");
            let summary = blocks.iter().find(|block| block.id.0 == "phase:summary").unwrap();
            assert!(summary.text.wire_rows().join("\n").contains(expected));
            assert_eq!(blocks.last().unwrap().id.0, "done");
            assert!(summary.metadata.fold.is_empty(), "artifacts and outcomes stay outside activity");
            assert!(!blocks.iter().any(|block| block.id.0 == "phase:prompt"));
        }
    }

    #[test]
    fn timer_updates_agent_lifecycle_without_replacing_its_task_body() -> anyhow::Result<()> {
        let mut run = crate::agent::Agent::pending("session", "Worker", "Implement", 0);
        run.state = crate::agent::AgentState::Ready;
        let entry = crate::timeline::TimelineEntry::AgentLifecycle {
            id: "worker-heading".into(), created_at_ms: 0, run,
            exchange: vec![], agent: vec![],
        };
        let width = WidthProfile::default();
        let projected = super::project_at(&entry, &width, 0)?;
        let document = forge_buffer::document::BufferDocument::new(
            forge_buffer::identity::DocumentId("timer-agent".into()), projected.entry.block)?;
        let refreshed = super::timing_blocks(&entry, &width, &document, &Default::default(), &projected.tool)?;
        assert_eq!(refreshed.len(), 1);
        assert_eq!(refreshed[0].id.0, "worker-heading");
        assert!(!refreshed[0].text.wire_rows().join("\n").contains("ready for 0s"));
        Ok(())
    }
    use super::{
        project_at, project_at_with_separator, session_event_text,
    };
    use forge_buffer::width::WidthProfile;
    use serde_json::json;
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
                assert_eq!(summary.metadata.layout.as_ref().unwrap().marker.as_ref().unwrap().text, "●");
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
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary};
        use crate::exchange::{ExchangeKind, ExchangeNode, ExchangeState, InputIntent, QuestionInput};

        for planning_question in [None, Some(false), Some(true)] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"ordered", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Plan a game", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            })).unwrap();
            exchange.resume(0).unwrap();
            let mut event = BackendEvent {
            received_at_ms: None,
                address: Some(ProviderAddress { thread_id:"thread".into(), turn_id:"initial".into() }),
                turn_boundary: Some(TurnBoundary::Started), kind:"turn_started".into(), text:None,
                data:serde_json::Value::Null, activity:None, summary:None, task_update:None,
            };
            exchange.observe_turn(&event, 0).unwrap();
            event.turn_boundary = None;
            event.kind = "assistant_message".into();
            event.text = Some("Inspecting the repository".into());
            event.data = json!({"phase":"commentary"});
            exchange.observe_turn(&event, 1).unwrap();
            event.text = Some("The repository contains only .git. Three choices are pending.".into());
            event.data = json!({"phase":"final_answer"});
            exchange.observe_turn(&event, 2).unwrap();
            event.text = None;
            event.turn_boundary = Some(TurnBoundary::Finished { outcome:crate::turn::TurnOutcome::Completed });
            exchange.observe_turn(&event, 3).unwrap();

            if let Some(planning) = planning_question {
                let question: crate::plan::PlanQuestionSet = serde_json::from_value(json!({
                    "id":"format", "questions":[{"id":"game", "header":"Game format",
                        "question":"Which game format?", "options":[
                            {"label":"2D arena survival", "description":"Top-down combat"}
                        ], "allow_freeform":true}]
                })).unwrap();
                exchange.node_list.push(if planning {
                    exchange.kind = ExchangeKind::PlanDraft;
                    serde_json::from_value(json!({"kind":"plan_event", "event":{
                        "id":"presented", "node_count":exchange.node_list.len(),
                        "content":{"kind":"lifecycle", "title":"Game", "lifecycle":{
                            "id":"presented", "session_id":"session", "plan_id":"plan",
                            "kind":"question_asked", "model_revision":0, "user_revision":0,
                            "question":question, "created_at_ms":3
                        }}
                    }})).unwrap()
                } else {
                    ExchangeNode::QuestionPresented { id:"presented".into(), question, answer:None }
                });
                exchange.append_question_input(InputIntent::Answer, "Game format: 2D arena survival".into(), QuestionInput {
                    set_id:"format".into(), question_id:None, answer:vec![crate::plan::PlanQuestionAnswer {
                        question_id:"game".into(), response:crate::plan::PlanQuestionResponse::Selected {
                            option:"2D arena survival".into(), feedback:None,
                        },
                    }],
                }, 4).unwrap();
            }
            event.address.as_mut().unwrap().turn_id = "continuation".into();
            event.turn_boundary = Some(TurnBoundary::Started);
            exchange.observe_turn(&event, 5).unwrap();
            event.turn_boundary = None;
            event.text = Some("The design will use a 2D arena survival loop.".into());
            event.data = json!({"phase":"commentary"});
            exchange.observe_turn(&event, 6).unwrap();

            let render = |exchange: &Exchange| project_at(&TimelineEntry::Exchange {
                id:exchange.id.clone(), created_at_ms:0, exchange:exchange.clone(), agent_by_id:HashMap::new(),
            }, &WidthProfile::default(), 10).unwrap();
            let initial = render(&exchange);
            let response_position = initial.entry.block.iter().position(|block|
                block.text.wire_rows().join("\n").contains("Three choices are pending")
            ).unwrap();
            let prefix = initial.entry.block[..=response_position].iter()
                .map(|block| block.id.clone()).collect::<Vec<_>>();

            event.text = None;
            event.kind = "tool".into();
            for tool_count in 1..=3 {
                event.activity = Some(ToolActivity {
                    id:format!("lookup-{tool_count}"), kind:ToolActivityKind::Command,
                    title:format!("lookup_{tool_count}"), output:Some("found".into()),
                    output_delta:false, status:Some("completed".into()), change:Default::default(),
                });
                exchange.observe_turn(&event, 6 + tool_count).unwrap();
                let projected = render(&exchange);
                let blocks = &projected.entry.block;
                assert_eq!(blocks[..=response_position].iter().map(|block| block.id.clone()).collect::<Vec<_>>(), prefix,
                    "later tools moved an earlier response");
                let text = blocks.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
                let response = text.find("Three choices are pending").unwrap();
                let continuation = text.find("The design will use").unwrap();
                assert!(response < continuation && continuation < text.find("lookup_1").unwrap());
                let commentary = blocks.iter().find(|block|
                    block.text.wire_rows().join("\n").contains("The design will use")).unwrap();
                assert_eq!(commentary.metadata.layout.as_ref().unwrap().indent, 4,
                    "continued thoughts must stay one level inside the exchange");
                let tools = blocks.iter().find(|block| block.id.0.ends_with(":tools")).unwrap();
                assert_eq!(tools.metadata.layout.as_ref().unwrap().indent, 4,
                    "continued tool groups must stay one level inside the exchange");
                if planning_question.is_some() {
                    let question = text.find("Questions · 1/1 answered").unwrap();
                    assert!(response < question && question < continuation);
                    assert_eq!(text.matches("You answered: 2D arena survival").count(), 1);
                }
                let summary = blocks.iter().find(|block| block.id.0 == "ordered:summary").unwrap();
                assert_eq!(summary.metadata.fold[0].end.block, blocks[response_position - 1].id,
                    "activity fold hid an outer response or later turn");
                if planning_question == Some(true) && tool_count == 3 {
                    println!("ORDERED_RESPONSE_FIXTURE:{}", serde_json::to_string(blocks).unwrap());
                }
            }
            event.activity = None;
            event.kind = "assistant_message".into();
            event.text = Some("The design is ready for review.".into());
            event.data = json!({"phase":"final_answer"});
            exchange.observe_turn(&event, 10).unwrap();
            event.text = None;
            event.turn_boundary = Some(TurnBoundary::Finished { outcome:crate::turn::TurnOutcome::Completed });
            exchange.observe_turn(&event, 11).unwrap();
            exchange.finish(ExchangeState::Complete, 11).unwrap();
            let completed = render(&exchange);
            let restored: Exchange = serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
            assert_eq!(completed.entry.block, render(&restored).entry.block);
            assert_eq!(completed.entry.block[..=response_position].iter().map(|block| block.id.clone()).collect::<Vec<_>>(), prefix);
            let text = completed.entry.block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert!(text.find("lookup_3").unwrap() < text.find("The design is ready").unwrap());
            assert_eq!(text.matches("Three choices are pending").count(), 1);
        }
    }

    #[test]
    fn question_branches_keep_targets_responses_and_folds_across_reload() {
        use forge_buffer::block::BufferBlock;
        use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
        use crate::exchange::{ExchangeKind, ExchangeNode, ExchangeState, InputIntent, QuestionInput};
        for planning in [false, true] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"nested", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Plan a replacement", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            })).unwrap();
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
                ("scope", "What does Rust include?", "It preserves the simulation."),
                ("azure", "Would this require credentials?", "Azure connections require credentials."),
                ("scope", "Can I retain the same workflow?", "Yes, the workflow remains the same."),
            ].into_iter().enumerate() {
                exchange.append_question_input(InputIntent::Clarification, prompt.into(), QuestionInput {
                    set_id: "set".into(), question_id: Some(target.into()), answer:Vec::new(),
                }, index as i64).unwrap();
                let mut event = BackendEvent {
            received_at_ms: None,
                    address:Some(ProviderAddress { thread_id:"thread".into(),turn_id:format!("turn-{index}") }),
                    turn_boundary:Some(TurnBoundary::Started), kind:"turn_started".into(), text:None,
                    data:serde_json::Value::Null, activity:None,summary:None,task_update:None,
                };
                exchange.observe_turn(&event,index as i64).unwrap();
                event.turn_boundary=None;
                event.kind="assistant_message".into();
                event.text=Some(reply.into());
                event.data=json!({"phase":"final_answer"});
                exchange.observe_turn(&event,index as i64).unwrap();
                event.text=None;
                event.turn_boundary=Some(TurnBoundary::Finished {outcome:crate::turn::TurnOutcome::Completed});
                exchange.observe_turn(&event,index as i64).unwrap();
            }
            let selected = crate::plan::PlanQuestionAnswer {
                question_id:"scope".into(), response:crate::plan::PlanQuestionResponse::Selected {
                    option:"Rust CLI".into(),feedback:None,
                },
            };
            let mut elicitation = crate::plan::PlanElicitation::new(set);
            elicitation.answer.push(selected.clone());
            exchange.elicitation=Some(elicitation);
            exchange.awaiting_input=true;
            exchange.pause(3);
            let render = |exchange: Exchange| project_at(&TimelineEntry::Exchange {
                id:exchange.id.clone(),created_at_ms:0,exchange,agent_by_id:HashMap::new(),
            }, &WidthProfile::default(), 4).unwrap();
            let restored = serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
            let projected = render(restored);
            let blocks = &projected.entry.block;
            let text = blocks.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert!(text.contains("Questions · 1/2 answered"));
            let position = |id: &str| blocks.iter().position(|block| block.id.0 == id).unwrap();
            let scope = position("presented:question:scope");
            let azure = position("presented:question:azure");
            for (input, start, end) in [(1,scope,azure),(2,azure,blocks.len()),(3,scope,azure)] {
                let heading = position(&format!("nested:input:{input}:prompt"));
                let block = &blocks[heading];
                assert!(start < heading && heading < end);
                assert!(block.metadata.decoration.iter().any(|decoration| decoration.capture == "ForgeHarnessPrompt"));
                let fold = &block.metadata.fold[0];
                let reply = position(&fold.end.block.0);
                assert_eq!(reply, heading + 1, "clarification folded a sibling input");
                assert!(reply < end);
                assert!(!blocks[reply].metadata.gutter.iter().flat_map(|gutter| &gutter.chunk)
                    .any(|chunk| chunk.text.contains('↳')));
                let indent = |block: &BufferBlock| block.metadata.layout.as_ref().unwrap().indent;
                assert_eq!(indent(&blocks[reply]),indent(block)+2);
            }
            if planning { println!("NESTED_QUESTION_FIXTURE:{}",serde_json::to_string(blocks).unwrap()); }
            exchange.append_question_input(InputIntent::Answer,"Scope: Rust CLI".into(),QuestionInput {
                set_id:"set".into(),question_id:None,answer:vec![selected],
            },4).unwrap();
            exchange.finish(ExchangeState::Cancelled,5).unwrap();
            let cancelled = render(exchange);
            assert!(cancelled.entry.block.iter().find(|block| block.id.0=="presented").unwrap().metadata.fold[0].closed);
            let text = cancelled.entry.block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
            assert_eq!(text.matches("Azure connections require credentials.").count(),1);
            assert_eq!(text.matches("Yes, the workflow remains the same.").count(),1);
        }
    }

    #[test]
    fn question_clarification_preserves_turn_delivery_and_history() {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary};
        use crate::exchange::{ExchangeNode, ExchangeState};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"question-flow", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Plan out a new one", "kind":"chat", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        exchange.resume(0).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress { thread_id: "thread".into(), turn_id: "question-turn".into() }),
            turn_boundary: Some(TurnBoundary::Started), kind: "turn_started".into(), text: None,
            data: serde_json::Value::Null, activity: None, summary: None, task_update: None,
        };
        exchange.observe_turn(&event, 0).unwrap();
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("The scope determines the implementation.".into());
        event.data = json!({"phase":"commentary"});
        exchange.observe_turn(&event, 1).unwrap();
        event.kind = "tool".into();
        event.text = None;
        for (id, status) in [("failed-question", "failed"), ("accepted-question", "completed")] {
            event.activity = Some(ToolActivity {
                id: id.into(), kind: ToolActivityKind::Command, title: "harness_question_ask".into(),
                output: Some(if status == "failed" { "Invalid options" } else { "Question set accepted" }.into()),
                output_delta: false, status: Some(status.into()), change: Default::default(),
            });
            exchange.observe_turn(&event, 2).unwrap();
        }
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished { outcome: crate::turn::TurnOutcome::Completed });
        exchange.observe_turn(&event, 3).unwrap();
        let question = crate::plan::PlanQuestionSet {
            id: "scope-set".into(), questions: vec![crate::plan::PlanQuestion {
                id: "scope".into(), header: "Migration scope".into(), question: "What should the replacement demonstrate?".into(),
                options: vec![crate::plan::PlanQuestionOption { label: "Rust CLI".into(), description: "Replace the demo".into() }],
                allow_freeform: true,
            }],
        };
        exchange.node_list.push(ExchangeNode::QuestionPresented { id: "presented".into(), question: question.clone(), answer: None });
        exchange.elicitation = Some(crate::plan::PlanElicitation::new(question));
        exchange.awaiting_input = true;
        exchange.append_input(crate::exchange::InputIntent::Clarification, "What do you mean?".into(), 4).unwrap();
        event.address.as_mut().unwrap().turn_id = "explanation-turn".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        exchange.observe_turn(&event, 4).unwrap();
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("I mean a Rust CLI or a redesigned TypeScript demo.".into());
        event.data = json!({"phase":"final_answer"});
        exchange.observe_turn(&event, 5).unwrap();
        event.text = None;
        event.turn_boundary = Some(TurnBoundary::Finished { outcome: crate::turn::TurnOutcome::Completed });
        exchange.observe_turn(&event, 6).unwrap();
        exchange.pause(6);
        let restored: Exchange = serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
        let projected = project_at(&TimelineEntry::Exchange {
            id: restored.id.clone(), created_at_ms: 0, exchange: restored, agent_by_id: HashMap::new(),
        }, &WidthProfile::default(), 6).unwrap();
        let text = projected.entry.block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
        assert!(text.contains("▸ You asked: What do you mean?"));
        assert_eq!(text.matches("▸ Questions ·").count(), 1);
        assert!(!text.contains("Question set accepted"));
        assert!(!text.contains("Invalid options"), "settled tool output stays deferred");
        assert!(projected.tool.values().any(|tool| tool.preview(true).is_ok_and(|preview|
            preview.row.iter().any(|row| row.contains("Invalid options")))));
        assert_eq!(projected.tool.len(), 1, "successful question must not remain a raw tool");
        assert!(text.find("▸ Questions ·").unwrap() < text.find("You asked:").unwrap());
        assert!(text.find("You asked:").unwrap() < text.find("I mean a Rust CLI").unwrap());
        let answer = projected.entry.block.iter().find(|block| block.text.wire_rows().join("\n").contains("I mean a Rust CLI")).unwrap();
        assert!(!answer.metadata.gutter.iter().flat_map(|gutter| &gutter.chunk).any(|chunk| chunk.text.contains('↳')));
        assert!(projected.action.values().any(|action| matches!(action, super::TranscriptAction::Question { question_set_id } if question_set_id == "scope-set")));
        let question_block = projected.entry.block.iter().find(|block| block.id.0 == "presented").unwrap();
        assert!(!question_block.metadata.fold[0].closed);
        assert_eq!(question_block.metadata.fold[0].end.block, answer.id, "question owns the clarification response");
        assert!(exchange.completed_at_ms.is_none() && exchange.checkpoint_after.is_none());
        println!("QUESTION_FIXTURE:{}", serde_json::to_string(&projected.entry.block).unwrap());
        exchange.append_input(crate::exchange::InputIntent::Answer, "Planning feedback:\n- Scope: Rust CLI".into(), 7).unwrap();
        let mut cancelled = exchange.clone();
        cancelled.finish(ExchangeState::Cancelled, 7).unwrap();
        let cancelled = project_at(&TimelineEntry::Exchange {
            id: cancelled.id.clone(), created_at_ms: 0, exchange: cancelled, agent_by_id: HashMap::new(),
        }, &WidthProfile::default(), 7).unwrap();
        let text = cancelled.entry.block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
        assert!(text.find("I mean a Rust CLI").unwrap() < text.find("You answered: Scope: Rust CLI").unwrap());
        exchange.resume(7).unwrap();
        event.address.as_mut().unwrap().turn_id = "continuation-turn".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        exchange.observe_turn(&event, 7).unwrap();
        event.turn_boundary = None;
        event.text = Some("Proceeding with Rust".into());
        exchange.observe_turn(&event, 8).unwrap();
        event.text = None;
        event.turn_boundary = Some(TurnBoundary::Finished { outcome: crate::turn::TurnOutcome::Completed });
        exchange.observe_turn(&event, 9).unwrap();
        exchange.finish(ExchangeState::Complete, 9).unwrap();
        let completed = project_at(&TimelineEntry::Exchange {
            id: exchange.id.clone(), created_at_ms: 0, exchange, agent_by_id: HashMap::new(),
        }, &WidthProfile::default(), 9).unwrap();
        let text = completed.entry.block.iter().flat_map(|block| block.text.wire_rows()).collect::<Vec<_>>().join("\n");
        assert!(text.find("I mean a Rust CLI").unwrap() < text.find("You answered: Scope: Rust CLI").unwrap());
        assert!(text.find("You answered: Scope: Rust CLI").unwrap() < text.find("Proceeding with Rust").unwrap());
    }

    #[test]
    fn file_changes_split_tool_groups_and_restore_non_git_summary() -> anyhow::Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress, ProviderChangeKind, ProviderChangeSet,
            ProviderFileChange, ToolActivity, ToolActivityKind, TurnBoundary};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"edits", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Implement", "kind":"plan_execution", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = ProviderAddress { thread_id:"thread".into(), turn_id:"turn".into() };
        exchange.start_turn(address.clone(), 0)?;
        let mut event = BackendEvent {
            received_at_ms:None, address:Some(address), turn_boundary:None, kind:"tool".into(),
            text:None, data:serde_json::Value::Null, activity:None, summary:None, task_update:None,
        };
        for (index, (id, kind, status, diff)) in [
            ("inspect", ToolActivityKind::Command, "completed", ""),
            ("add", ToolActivityKind::FileChange, "completed", "@@ -0,0 +1 @@\n+first"),
            ("read", ToolActivityKind::Command, "completed", ""),
            ("edit", ToolActivityKind::FileChange, "completed", "@@ -1 +1 @@\n-first\n+second"),
            ("rejected", ToolActivityKind::FileChange, "failed", "@@ -1 +1 @@\n-second\n+wrong"),
            ("check", ToolActivityKind::Command, "completed", ""),
        ].into_iter().enumerate() {
            let file_change = kind == ToolActivityKind::FileChange;
            event.activity = Some(ToolActivity {
                id:id.into(), kind, title:if file_change { "file changes".into() } else { id.into() },
                output:None, output_delta:false, status:Some("running".into()),
                change:ProviderChangeSet { file:if file_change { vec![ProviderFileChange {
                    path:"src/lib.rs".into(), move_path:None,
                    kind:if id == "add" { ProviderChangeKind::Add } else { ProviderChangeKind::Update },
                    diff:diff.into(),
                }] } else { Vec::new() } },
            });
            exchange.observe_turn(&event, index as i64 * 10 + 1)?;
            if file_change {
                let projected = project_at(&TimelineEntry::Exchange {
                    id:"edits".into(), created_at_ms:0, exchange:exchange.clone(), agent_by_id:HashMap::new(),
                }, &WidthProfile::default(), 100)?;
                assert!(projected.entry.block.iter().any(|block| block.text.wire_rows().join("").contains("Changing 1 file")));
            }
            event.activity.as_mut().unwrap().status = Some(status.into());
            exchange.observe_turn(&event, index as i64 * 10 + 2)?;
        }
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished { outcome:crate::turn::TurnOutcome::Completed });
        exchange.observe_turn(&event, 70)?;
        exchange.finish(crate::exchange::ExchangeState::Complete, 70)?;
        let restored = serde_json::from_value(serde_json::to_value(exchange)?)?;
        let projected = project_at(&TimelineEntry::Exchange {
            id:"edits".into(), created_at_ms:0, exchange:restored, agent_by_id:HashMap::new(),
        }, &WidthProfile::default(), 100)?;
        let blocks = &projected.entry.block;
        let groups: Vec<_> = blocks.iter().filter(|block| block.id.0.ends_with(":tools")).collect();
        assert_eq!(groups.len(), 3);
        assert!(groups.iter().all(|block| block.text.wire_rows().join("").contains("Ran 1 tool")));
        let headings: Vec<_> = blocks.iter().filter(|block| block.id.0.ends_with(":changes"))
            .map(|block| block.text.wire_rows().join("")).collect();
        assert_eq!(headings.len(), 4);
        assert!(headings[0].contains("Changed 1 file") && headings[1].contains("Changed 1 file"));
        assert!(headings[2].contains("File changes failed"));
        assert!(headings[3].contains("Changed 1 file") && headings[3].contains("reported edits"));
        assert_eq!(projected.tool.len(), 3, "file edits must not register generic output views");
        assert!(!blocks.iter().any(|block| block.text.wire_rows().join("").contains("file changes")));
        let summary = blocks.iter().position(|block| block.id.0 == "edits:changes").unwrap();
        let activity = blocks.iter().find(|block| block.metadata.fold.iter().any(|fold| fold.id.0 == "edits:exchange")).unwrap();
        let endpoint = &activity.metadata.fold.iter().find(|fold| fold.id.0 == "edits:exchange").unwrap().end.block;
        assert!(blocks.iter().position(|block| &block.id == endpoint).unwrap() < summary);
        let aggregate = match projected.action.get(&super::TargetId("edits:changes".into())).unwrap() {
            super::TranscriptAction::Diff { text } => text,
            _ => panic!("aggregate must expose its reported patch"),
        };
        assert!(aggregate.contains("+first") && aggregate.contains("+second") && !aggregate.contains("+wrong"));
        Ok(())
    }

    #[test]
    fn tool_groups_align_mixed_durations_as_running_timers_change() {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"aligned", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        exchange.resume(0).unwrap();
        let address = ProviderAddress { thread_id: "thread".into(), turn_id: "turn".into() };
        exchange.start_turn(address.clone(), 0).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address), turn_boundary: None, kind: "tool".into(), text: None,
            data: serde_json::Value::Null, activity: None, summary: None, task_update: None,
        };
        let mut clock = 1;
        for (index, duration) in [Some(4), Some(21), Some(1000), Some(626), None].into_iter().enumerate() {
            event.activity = Some(ToolActivity {
                id: format!("tool-{index}"), kind: if index % 2 == 0 { ToolActivityKind::Command } else { ToolActivityKind::ToolCall },
                title: format!("inspect_{index}"), output: None, output_delta: false,
                status: Some("running".into()), change: Default::default(),
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
            let projected = project_at(&TimelineEntry::Exchange {
                id: exchange.id.clone(), created_at_ms: 0, exchange: exchange.clone(), agent_by_id: HashMap::new(),
            }, &WidthProfile::default(), now).unwrap();
            let headings = projected.entry.block.iter().filter_map(|block| block.text.row(0))
                .filter(|row| row.contains("• ")).collect::<Vec<_>>();
            assert_eq!(headings.len(), 5);
            let column = headings[0].find("inspect_").unwrap();
            assert!(headings.iter().all(|row| row.find("inspect_") == Some(column)), "{headings:?}");
            assert!(headings[0].contains("   0s inspect_0"));
            assert!(headings[2].contains("   1s inspect_2"));
            let group = projected.entry.block.iter().find(|block| block.id.0.ends_with(":tools")).unwrap();
            assert_eq!(group.text.row(0), Some(format!("▸ Running 5 tools · {}",
                super::super::duration::tool_duration(Some(1652 + (now - clock) as u64)).trim()).as_str()));
            let document = forge_buffer::document::BufferDocument::new(
                forge_buffer::identity::DocumentId("group-timer".into()), projected.entry.block.clone()).unwrap();
            let refreshed = super::timing_blocks(&TimelineEntry::Exchange {
                id: exchange.id.clone(), created_at_ms: 0, exchange: exchange.clone(), agent_by_id: HashMap::new(),
            }, &WidthProfile::default(), &document, &Default::default(), &projected.tool).unwrap();
            let refreshed_group = refreshed.iter().find(|block| block.id == group.id).unwrap();
            assert!(refreshed_group.text.row(0).unwrap().starts_with("▸ Running 5 tools · "));
            assert_ne!(refreshed_group.text, group.text);
            assert_eq!(refreshed_group.metadata.layout, group.metadata.layout);
            assert!(refreshed.iter().all(|block| !block.id.0.ends_with(":preview")),
                "clock ticks must not replace tool output bodies");
            let refreshed_headings = refreshed.iter().filter_map(|block| block.text.row(0))
                .filter(|row| row.contains("inspect_")).collect::<Vec<_>>();
            assert_eq!(refreshed_headings.len(), 5);
            let refreshed_column = refreshed_headings[0].find("inspect_").unwrap();
            assert!(refreshed_headings.iter().all(|row| row.find("inspect_") == Some(refreshed_column)));
            for block in projected.entry.block.iter().filter(|block| block.text.row(0).is_some_and(|row| row.contains("• "))) {
                assert!(block.text.row(0).unwrap().starts_with('•'));
                assert_eq!(block.metadata.layout.as_ref().unwrap().indent, 4,
                    "tool rows inherit the group indent without adding a second text indent");
                assert!(block.metadata.decoration.iter().filter(|decoration| matches!(decoration.capture.as_str(), "ForgeHarnessCommand" | "ForgeHarnessMcpName"))
                    .all(|decoration| decoration.range.start.column == column));
            }
        }
    }

    #[test]
    fn tool_group_totals_sum_parallel_calls_and_require_complete_timing() {
        let mut first: crate::turn::ToolCall = serde_json::from_value(json!({
            "id":"first", "kind":"command", "title":"first", "output":"",
            "status":"completed", "failed":false, "started_at_ms":0, "completed_at_ms":1050
        })).unwrap();
        let mut second = first.clone();
        second.id = "second".into();
        second.completed_at_ms = None;
        second.status = "running".into();
        assert_eq!(super::tool_group_label([&first, &second], false, 1500),
            "▸ Running 2 tools · 2.6s");
        second.completed_at_ms = Some(2000);
        second.status = "failed".into();
        second.failed = true;
        assert_eq!(super::tool_group_label([&first, &second], true, 90_000),
            "▸ Ran 2 tools (1 failed) · 3.1s");
        first.started_at_ms = None;
        assert_eq!(super::tool_group_label([&first, &second], true, 90_000),
            "▸ Ran 2 tools (1 failed)");
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
                        assert_eq!(block.metadata.layout.as_ref().unwrap().indent,
                            group.metadata.layout.as_ref().unwrap().indent);
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
            render(&finished)
                .entry
                .block
                .iter()
                .find(|block| block.id.0.ends_with(":tools"))
                .unwrap()
                .metadata
                .fold[0]
                .closed,
            "turn completion must close the final group"
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
            super::TranscriptAction::Diff { text } if text.contains(path))));
    }

    #[test]
    fn declaration_revision_projects_file_deltas_and_overview_as_separate_trees() {
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"revision", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Request plan changes", "kind":"plan_revision", "state":"running",
            "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        let mut response = crate::backend::BackendEvent {
            received_at_ms: None,
            address: Some(crate::backend::ProviderAddress {
                thread_id: "thread".into(), turn_id: "turn".into(),
            }),
            turn_boundary: Some(crate::backend::TurnBoundary::Started),
            kind: "assistant_message".into(), text: Some("Submitted the revised design.".into()),
            data: json!({"phase":"final_answer"}), activity: None, summary: None, task_update: None,
        };
        exchange.start_turn(response.address.clone().unwrap(), 0).unwrap();
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
        let exchange: Exchange = serde_json::from_slice(&serde_json::to_vec(&exchange).unwrap()).unwrap();
        let projected = project_at(&TimelineEntry::Exchange {
            id: exchange.id.clone(), created_at_ms: 0, exchange, agent_by_id: HashMap::new(),
        }, &WidthProfile::default(), 10).unwrap();
        let text = projected.entry.block.iter().flat_map(|block|
            (0..block.text.row_count()).map(|row| block.text.row(row).unwrap())).collect::<Vec<_>>().join("\n");
        assert!(text.find("Proposed changes").unwrap() < text.find("Submitted the revised design.").unwrap());
        assert!(text.find("Plan overview").unwrap() < text.find("Submitted the revised design.").unwrap());
        assert!(text.contains("Proposed changes 1 file +1 -1"));
        assert!(text.contains("Modified src/lib.rs +1 -1"));
        assert!(text.contains("Plan overview 1 section +1 -1"));
        assert!(text.contains("Modified Description +1 -1"));
        assert!(!text.contains("Artifact:") && !text.contains("working.md"));
        assert!(projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::Declaration { path, revision: 3, document: true, .. } if path == "Description")));
    }

    #[test]
    fn nested_agents_indent_each_ownership_boundary_once() {
        let nested: Exchange = serde_json::from_value(json!({
            "id":"nested", "session_id":"session", "agent_id":"nested-agent", "ordinal":1,
            "prompt":"Inspect nested work", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))
        .unwrap();
        let mut parent = nested.clone();
        parent.id = "parent".into();
        parent
            .node_list
            .push(crate::exchange::ExchangeNode::AgentReference {
                agent: crate::agent::Delegation {
                    id: "delegation".into(),
                    parent_exchange_id: parent.id.clone(),
                    parent_turn_id: None,
                    child_agent_id: "nested-agent".into(),
                    child_exchange_id: nested.id.clone(),
                    task: "Inspect nested work".into(),
                    created_at_ms: 0,
                },
            });
        let projected = project_at(
            &TimelineEntry::AgentLifecycle {
                id: "parent-agent".into(),
                created_at_ms: 0,
                run: crate::agent::Agent::pending("session", "Parent", "Inspect", 0),
                exchange: vec![parent],
                agent: vec![TimelineEntry::AgentLifecycle {
                    id: "nested-agent".into(),
                    created_at_ms: 0,
                    run: crate::agent::Agent::pending("session", "Nested", "Inspect", 0),
                    exchange: vec![nested],
                    agent: vec![],
                }],
            },
            &WidthProfile::default(),
            0,
        )
        .unwrap();
        for (identity, spaces) in [
            ("parent-agent", 0),
            ("parent:prompt", 2),
            ("parent:summary", 2),
            ("delegation", 2),
            ("nested:prompt", 4),
            ("nested:summary", 4),
        ] {
            let block = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == identity)
                .unwrap();
            let prefix = " ".repeat(block.metadata.layout.as_ref().unwrap().indent - 2);
            assert_eq!(
                prefix,
                " ".repeat(spaces),
                "incorrect nesting for {identity}"
            );
        }
        let parent = &projected.entry.block[0];
        assert_eq!(parent.metadata.fold[0].end.block.0, "nested:summary");
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
                assert_eq!(indentation(heading, 0), "");
                for row in 0..detail.text.row_count() {
                    assert_eq!(
                        indentation(detail, row),
                        "  ",
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
        let streaming = project_at(
            &TimelineEntry::Exchange {
                id: exchange.id.clone(), created_at_ms: 0,
                exchange: exchange.clone(), agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(), 4,
        ).unwrap();
        let streaming_response = streaming.entry.block.iter()
            .find(|block| block.text.row(0) == Some("Finished the work")).unwrap();
        let streaming_commentary = streaming.entry.block.iter()
            .find(|block| block.text.row(0) == Some("  Inspecting ownership")).unwrap();
        assert_eq!(streaming_commentary.metadata.layout.as_ref().unwrap().indent, 4,
            "commentary must nest one level below the exchange summary");
        assert!(streaming_commentary.metadata.gutter.is_empty(),
            "resolved content layout must not acquire another gutter");
        assert_eq!(streaming_response.metadata.layout.as_ref().unwrap().indent, 2,
            "streaming final response must not inherit activity indentation");
        event.text = None;
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
        let text = projected
            .entry
            .block
            .iter()
            .flat_map(|block| (0..block.text.row_count()).filter_map(|row| block.text.row(row)))
            .collect::<Vec<_>>()
            .join("\n");
        let completed_response = projected.entry.block.iter()
            .find(|block| block.text.row(0) == Some("Finished the work")).unwrap();
        assert_eq!(streaming_response.metadata.layout, completed_response.metadata.layout,
            "completion must not change final response indentation");
        let commentary = text.find("Inspecting ownership").unwrap();
        let changes = text.find("Changed 1 file").unwrap();
        let steering = text.find("Also check tests").unwrap();
        let response = text.find("Finished the work").unwrap();
        assert!(commentary < changes && changes < steering && steering < response);
        assert_eq!(text.matches("Finished the work").count(), 1);
        assert!(projected.tool.is_empty(), "file edits must not create generic tool output");
        assert!(
            projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::Diff { text } if text.contains("owned.rs"))),
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
            id: "execution".into(), phase: crate::plan::PlanPhase::Implement,
            progress: crate::plan::execution::SemanticProgress {
                matched: vec!["Cargo.toml".into()],
                missing: vec!["src/ui.rs".into()],
                different: vec!["src/player.rs: planned call relationships differ for move_player".into()],
                unverified: vec!["src/player.rs: unresolved reference".into()],
                ..Default::default()
            },
        };
        let render = |execution| project_at(&TimelineEntry::Status {
            id: "session:status".into(), created_at_ms: 0,
            status: SessionPhase::Working {
                started_at_ms: 0, activity: WorkflowActivity::Working,
                reasoning_summary: Some("Checking files".into()), execution: Some(execution),
            },
        }, &WidthProfile::default(), 133_000).unwrap();
        let initial = render(execution.clone());
        assert_eq!(initial.entry.block[0].text.wire_rows(), vec!["", "Implementing · 133s · 2 files need attention"]);
        assert!(initial.entry.block[0].metadata.target[0].id.0.ends_with(":working"));
        assert!(initial.entry.block[1].metadata.fold[0].closed);
        let file_id = format!("session:status:implementation:execution:{}", crate::plan::digest(b"src/player.rs"));
        let file = initial.entry.block.iter().find(|block| block.id.0 == file_id).unwrap();
        assert!(file.metadata.fold[0].closed);
        assert!(initial.entry.block.iter().any(|block| block.text.wire_rows().join(" ").contains("move_player")));
        let repeated = render(execution.clone());
        assert_eq!(initial.entry.block[1].metadata.fold, repeated.entry.block[1].metadata.fold);
        println!("IMPLEMENTATION_STATUS_FIXTURE:{}", serde_json::to_string(&initial.entry.block).unwrap());
        execution.phase = crate::plan::PlanPhase::Verify;
        execution.progress.missing.clear();
        execution.progress.different.clear();
        execution.progress.unverified.clear();
        execution.progress.matched.extend(["src/player.rs".into(), "src/ui.rs".into()]);
        let matched = render(execution);
        assert_eq!(matched.entry.block[0].text.wire_rows(), vec!["", "Verifying · 133s · all files matched"]);
        assert!(matched.entry.block.iter().any(|block| block.id.0 == file_id));
    }

    #[test]
    fn status_text_and_animation_are_committed_together() {
        use forge_buffer::block::StatusHint;
        let cases = [
            (SessionPhase::Paused, "Paused", false, None),
            (SessionPhase::Finalizing { exchange_id: "exchange".into(), error: None }, "Saving exchange", true, None),
            (SessionPhase::Finalizing { exchange_id: "exchange".into(), error: Some("locked".into()) }, "Saving exchange failed · locked", false, None),
            (SessionPhase::WaitingForAgent { agent_count: 2 }, "Waiting for 2 agents", true, Some(StatusHint::Working)),
            (SessionPhase::RetryingPlanGeneration { plan_id: "plan".into(), turn: 2, max_turn: 20, started_at_ms: 0, activity: WorkflowActivity::Planning }, "Planning · 3s", true, Some(StatusHint::Working)),
            (SessionPhase::RetryingPlanGeneration { plan_id: "plan".into(), turn: 3, max_turn: 20, started_at_ms: 0, activity: WorkflowActivity::Revising }, "Revising plan · 3s", true, Some(StatusHint::Working)),
            (SessionPhase::PlanningFailed { plan_id: "plan".into(), turn_count: 2, reason: "no progress limit reached".into() }, "Planning stopped · no progress limit reached", false, None),
        ];
        for (phase, expected, animated, hint) in cases {
            let projection = project_at(&TimelineEntry::Status { id: "status".into(), created_at_ms: 0, status: phase }, &WidthProfile::default(), 3000).unwrap();
            let block = &projection.entry.block[0];
            assert_eq!(block.text.wire_rows(), vec!["", expected]);
            assert_eq!(block.metadata.status, Some(forge_buffer::block::StatusPresentation { row: 1, animated, hint }));
            block.validate().unwrap();
            let encoded = serde_json::to_vec(block).unwrap();
            let reopened: forge_buffer::block::BufferBlock = serde_json::from_slice(&encoded).unwrap();
            assert_eq!(&reopened, block);
        }
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
        assert!(status.gutter.is_empty(), "status text must not be split into the sign column");
        assert!(status.conceal.is_empty(), "status text must remain intact for inline hints");
    }

    #[test]
    fn working_status_shows_one_normalized_reasoning_summary_line() {
        let projection = project_at(
            &TimelineEntry::Status {
                id: "session:status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    started_at_ms: 1_000,
                    activity: WorkflowActivity::Working,
                    execution: None,
                    reasoning_summary: Some(
                        "Inspecting\n repository   structure before making changes".into(),
                    ),
                },
            },
            &WidthProfile::default(),
            4_999,
        )
        .unwrap();

        assert_eq!(projection.entry.block[0].text.row_count(), 2);
        assert_eq!(
            projection.entry.block[0].text.wire_rows(),
            vec!["", "Working · 3s · Inspecting repository structure bef…"]
        );
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
        })).unwrap();
        let projected = project_at(
            &TimelineEntry::Exchange {
                id: "interaction".into(),
                created_at_ms: 1_000,
                exchange: interaction,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1_000,
        ).unwrap();
        assert_eq!(projected.entry.block[0].id.0, "interaction:lifecycle");
        assert!(projected.entry.block[0].text.wire_rows().join("\n").contains("Plan implementation resumed · implement"));
        assert!(!projected.entry.block.iter().any(|block| block.id.0 == "interaction:prompt"));
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
    fn paused_exchange_activity_summary_freezes_until_execution_resumes() {
        for (kind, paused, running) in [
            ("chat", "Paused", "Working"),
            ("plan_draft", "Planning paused", "Planning"),
            ("plan_revision", "Revising plan paused", "Revising plan"),
            (
                "plan_execution",
                "Implementing paused",
                "Implementing",
            ),
        ] {
            let mut exchange: Exchange = serde_json::from_value(json!({
                "id":"exchange", "session_id":"session", "agent_id":"primary",
                "ordinal":1, "prompt":"Work", "kind":kind, "state":"running",
                "created_at_ms":1000, "attributed_matches_checkpoint":false, "node_list":[]
            }))
            .unwrap();
            exchange.resume(1000).unwrap();
            exchange.pause(4000);
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
                summary.text.wire_rows().join("").split_whitespace().collect::<Vec<_>>().join(" "),
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
            calls.insert(id.clone(), json!({"id":id,"kind":"tool_call","title":"inspect",
                "output":"", "status":if id == "20" { "failed" } else { "completed" },
                "failed":id == "20", "started_at_ms":0,"completed_at_ms":200}));
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
        assert_eq!(super::exchange_activity_summary(&exchange, 44000),
            "● Planned 44s │ Tools 4s · 19/20 pass │ Tokens 855.1k -> 7.9k · 197 tps");
        for status in ["running", "cancelled", "interrupted"] {
            value["turn"][0]["tool"]["item"]["1"]["status"] = json!(status);
            let exchange: Exchange = serde_json::from_value(value.clone()).unwrap();
            assert!(super::exchange_activity_summary(&exchange, 44000).contains("18/20 pass"));
        }
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
        for (now_ms, duration, aggregate) in [(2439, "0.4s", "439ms"), (2500, "0.5s", "500ms"),
            (3000, "1s", "1s"), (4500, "2.5s", "2.5s")] {
            let entry = crate::timeline::TimelineEntry::Exchange {
                id: exchange.id.clone(), created_at_ms: 1000, exchange: exchange.clone(),
                agent_by_id: HashMap::new(),
            };
            let rendered = project_at(&entry, &WidthProfile::default(), now_ms).unwrap();
            assert!(rendered.entry.block.iter().any(|block| block.text.wire_rows().iter()
                .any(|row| row.contains(&format!("• {duration:>5} cargo test")))));
            assert!(super::exchange_activity_summary(&exchange, now_ms)
                .contains(&format!("Tools {aggregate} · 0/1 pass")));
        }
        let mut permission_wait = exchange.clone();
        permission_wait.kind = crate::exchange::ExchangeKind::PlanDraft;
        permission_wait.observe_blocker("approval:command".into(), true, 4500);
        for now_ms in [90_000, 900_000] {
            assert_eq!(super::exchange_activity_summary(&permission_wait, now_ms),
                "● Planning paused 3s │ Tools 2.5s · 0/1 pass");
        }
        permission_wait.observe_blocker("approval:command".into(), false, 900_000);
        assert_eq!(super::exchange_activity_summary(&permission_wait, 902_000),
            "● Planning 5s │ Tools 4.5s · 0/1 pass");
        event.activity.as_mut().unwrap().status = Some("failed".into());
        exchange.observe_turn(&event, 5000).unwrap();
        let entry = crate::timeline::TimelineEntry::Exchange {
            id: exchange.id.clone(), created_at_ms: 1000, exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let rendered = project_at(&entry, &WidthProfile::default(), 90_000).unwrap();
        assert!(rendered.entry.block.iter().any(|block| block.text.wire_rows().iter()
            .any(|row| row.contains("•    3s cargo test"))));
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
            "● Thought 6s │ Tools 3s · 0/1 pass │ Tokens 76.0k (90%) -> 4.2k · 1400 tps"
        );
        event.address.as_mut().unwrap().thread_id = "unowned-child".into();
        assert!(!exchange.observe_turn(&event, 90_000).unwrap());
        assert_eq!(exchange.usage().input, Some(76000));
        for duration_ms in [6500, 3500, 3000] {
            exchange.duration_ms = duration_ms;
            assert_eq!(
                super::exchange_activity_summary(&exchange, 90_000),
                format!("● Thought {}s │ Tools 3s · 0/1 pass │ Tokens 76.0k (90%) -> 4.2k · 1400 tps", duration_ms / 1000)
            );
        }
        exchange.metrics.timing_complete = false;
        assert_eq!(
            super::exchange_activity_summary(&exchange, 90_000),
            "● Thought 3s │ Tools 0/1 pass │ Tokens 76.0k (90%) -> 4.2k"
        );
    }

    #[test]
    fn active_usage_updates_header_without_waiting_for_exchange_completion() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"live", "session_id":"session", "agent_id":"primary", "ordinal":1, "prompt":"Work",
            "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        exchange.resume(1000).unwrap();
        let address = ProviderAddress { thread_id: "parent".into(), turn_id: "turn".into() };
        exchange.start_turn(address.clone(), 1000).unwrap();
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address), turn_boundary: None, kind: "usage".into(), text: None,
            data: json!({"id":"first", "usage":{"input":1000,"cached_input":900,"reasoning":40,"output":100},
                "cumulative":null,"cumulative_total":null}),
            activity: None, summary: None, task_update: None,
        };
        exchange.observe_turn(&event, 3000).unwrap();
        assert_eq!(super::exchange_activity_summary(&exchange, 4000),
            "● Working 3s │ Tokens 1.0k (90%) -> 100 · 50 tps");
        exchange.observe_turn(&event, 6000).unwrap();
        assert!(super::exchange_activity_summary(&exchange, 6000).contains("50 tps"));
        event.data["id"] = json!("second");
        exchange.observe_turn(&event, 6000).unwrap();
        assert_eq!(super::exchange_activity_summary(&exchange, 9000),
            "● Working 8s │ Tokens 2.0k (90%) -> 200 · 40 tps");
    }

    #[test]
    fn summary_omits_missing_usage_and_preserves_reported_zeroes() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut original: Exchange = serde_json::from_value(json!({
            "id":"partial", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Work", "kind":"chat", "state":"running", "created_at_ms":1000,
            "attributed_matches_checkpoint":false, "node_list":[]
        })).unwrap();
        original.resume(1000).unwrap();
        let address = ProviderAddress { thread_id: "parent".into(), turn_id: "turn".into() };
        original.start_turn(address.clone(), 1000).unwrap();
        assert_eq!(super::exchange_activity_summary(&original, 4000), "● Working 3s");
        for (usage, expected) in [
            (json!({}), "● Working 3s"),
            (json!({"input":1000,"output":100}), "● Working 3s │ Tokens 1.0k -> 100 · 50 tps"),
            (json!({"input":1000,"cached_input":876}), "● Working 3s │ Tokens in 1.0k (88%)"),
            (json!({"input":0,"cached_input":0}), "● Working 3s │ Tokens in 0"),
            (json!({"input":1000,"cached_input":1001}), "● Working 3s │ Tokens in 1.0k"),
            (json!({"reasoning":23}), "● Working 3s"),
            (json!({"input":1000,"cached_input":0,"reasoning":0,"output":0}),
                "● Working 3s │ Tokens 1.0k (0%) -> 0"),
        ] {
            let mut exchange = original.clone();
            let event = BackendEvent {
            received_at_ms: None,
                address: Some(address.clone()), turn_boundary: None, kind: "usage".into(), text: None,
                data: json!({"id":"first", "usage":usage, "cumulative":null,"cumulative_total":null}),
                activity: None, summary: None, task_update: None,
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
            "● Planning paused 4s │ Tokens 1.0k (90%) -> 280"
        );
        interaction.kind = ExchangeKind::Chat;
        interaction
            .finish(crate::exchange::ExchangeState::Complete, 5500)
            .unwrap();
        assert_eq!(
            super::exchange_activity_summary(&interaction, 20000),
            "● Thought 4s │ Tokens 1.0k (90%) -> 280"
        );
    }
    #[test]
    fn implementation_report_has_closed_file_sections_and_visible_missing_evidence() {
        use crate::plan::implementation_report::{ImplementationReport, ImplementationReportRef, ImplementationFile};
        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"report-exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Implement", "kind":"plan_execution", "state":"complete", "created_at_ms":0,
            "attributed_matches_checkpoint":false,"node_list":[]
        })).unwrap();
        let report = ImplementationReport { original_revision:1, accepted_revision:2, revisions:Vec::new(),
            file:vec![ImplementationFile { path:"src/lib.rs".into(), internal:vec!["function helper".into()],
                contract:Vec::new(), references:Vec::new() }], unavailable:Vec::new() };
        exchange.node_list.push(crate::exchange::ExchangeNode::ImplementationReport {
            id:"report".into(), reference:ImplementationReportRef { object_id:"saved".into(), files:1,helpers:1,relationships:0,incomplete:false },
            report:Some(std::sync::Arc::new(report)),
        });
        let render = |exchange| project_at(&TimelineEntry::Exchange {
            id:"report-exchange".into(), created_at_ms:0, exchange, agent_by_id:HashMap::new(),
        }, &WidthProfile::default(), 1).unwrap();
        let projected = render(exchange.clone());
        for identity in ["report", "report:file:0"] {
            let block = projected.entry.block.iter().find(|block| block.id.0 == identity).unwrap();
            assert!(block.metadata.fold[0].closed);
        }
        let crate::exchange::ExchangeNode::ImplementationReport { report, .. } = &mut exchange.node_list[0] else { unreachable!() };
        *report = None;
        let projected = render(exchange);
        let text = projected.entry.block.iter().flat_map(|block| (0..block.text.row_count()).map(|row| block.text.row(row).unwrap())).collect::<Vec<_>>().join("\n");
        assert!(text.contains("Report content is unavailable"));
        assert!(text.contains("Implementation differences"));
    }

}
