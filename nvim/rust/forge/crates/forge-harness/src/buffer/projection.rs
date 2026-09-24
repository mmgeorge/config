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

use crate::exchange::{Exchange, ExchangeKind, ExchangeNode, ExchangeState};
use crate::session::state_machine::SessionPhase;
use crate::timeline::{SessionEventKind, TimelineEntry};

use super::document::TranscriptEntry;
use super::tool::ToolOutputView;
use super::transcript::TranscriptRenderer;

#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TranscriptAction {
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

impl ProjectedEntry {
    pub fn retained_bytes(&self) -> usize {
        self.entry
            .block
            .iter()
            .map(BufferBlock::retained_bytes)
            .sum::<usize>()
            + self
                .syntax
                .values()
                .map(super::syntax::MarkdownSyntax::retained_bytes)
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

struct TimelineRenderer<'profile> {
    width: &'profile WidthProfile,
    margin: usize,
    now_ms: i64,
    block: Vec<BufferBlock>,
    prompt: Vec<BlockId>,
    action: HashMap<TargetId, TranscriptAction>,
    tool: HashMap<String, ToolOutputView>,
    syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
    bytes: usize,
    leading_separator: bool,
    expanded_tool: &'profile std::collections::HashSet<String>,
}

enum MarkdownRole {
    Response,
    Detail,
    Comment,
}

#[must_use]
struct Section {
    start: usize,
    margin: usize,
}

pub fn project(
    entry: &TimelineEntry,
    width: &WidthProfile,
    leading_separator: bool,
    expanded_tool: &std::collections::HashSet<String>,
) -> Result<ProjectedEntry> {
    let now_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |duration| {
            duration.as_millis().min(i64::MAX as u128) as i64
        });
    project_at_with_separator(entry, width, now_ms, leading_separator, expanded_tool)
}

#[cfg(test)]
fn project_at(entry: &TimelineEntry, width: &WidthProfile, now_ms: i64) -> Result<ProjectedEntry> {
    project_at_with_separator(
        entry,
        width,
        now_ms,
        false,
        &std::collections::HashSet::new(),
    )
}

fn project_at_with_separator(
    entry: &TimelineEntry,
    width: &WidthProfile,
    now_ms: i64,
    leading_separator: bool,
    expanded_tool: &std::collections::HashSet<String>,
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
        bytes: 0,
        leading_separator,
        expanded_tool,
    };
    projection.entry(entry, 0)?;
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
                    SessionPhase::Finalizing { error, .. } => error.as_ref().map_or_else(
                        || "Finalizing".into(),
                        |error| format!("Finalization failed: {error}"),
                    ),
                    SessionPhase::Working {
                        started_at_ms,
                        reasoning_summary,
                        ..
                    } => {
                        let elapsed_seconds =
                            self.now_ms.saturating_sub(*started_at_ms).max(0) / 1_000;
                        working_status_text(
                            self.width,
                            elapsed_seconds,
                            reasoning_summary.as_deref(),
                        )?
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
                    format!("\n{text}")
                };
                let mut block = TranscriptRenderer::new(&self.content_width())?.literal(
                    BlockId(id.clone()),
                    &text,
                    0,
                )?;
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
                } else if matches!(status, SessionPhase::Working { .. }) {
                    block.metadata.target.push(TargetRange {
                        id: TargetId(format!("{id}:working")),
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
        let section = self.begin_section();
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
                let label = match lifecycle.kind {
                    PlanLifecycleKind::QuestionAsked => "Clarification requested",
                    PlanLifecycleKind::QuestionAnswered => "Clarification answered",
                    PlanLifecycleKind::QuestionWithdrawn => "Clarification withdrawn",
                    PlanLifecycleKind::Created => "Plan submitted",
                    PlanLifecycleKind::RevisionCreated => "Plan revised",
                    PlanLifecycleKind::ChangesRequested => "Plan changes requested",
                    PlanLifecycleKind::Accepted => "Plan accepted",
                    PlanLifecycleKind::Cancelled => "Plan cancelled",
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
                    format!("▸ Clarification: {answer}")
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
                let section = self.begin_section();
                if lifecycle.kind == PlanLifecycleKind::QuestionAnswered {
                    self.prompt.push(BlockId(event.id.clone()));
                }
                if let Some(question) = &lifecycle.question {
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
                if let Some(comment) = &lifecycle.overall_comment {
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
                    PlanExecutionLifecycleEvent::TaskStarted {
                        ordinal,
                        total,
                        title,
                        ..
                    } => format!("Task {ordinal}/{total}: {title}"),
                    PlanExecutionLifecycleEvent::TaskCompleted {
                        ordinal,
                        total,
                        title,
                        elapsed_ms,
                        ..
                    } => format!("Completed {ordinal}/{total}: {title} · {elapsed_ms} ms"),
                    PlanExecutionLifecycleEvent::DeviationRecorded { summary, .. } => {
                        format!("Deviation: {summary}")
                    }
                };
                self.literal(&event.id, &label, None)?;
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
        let section = self.begin_section();
        self.literal(&format!("{id}:task"), &format!("↳ {}", task), None)?;
        let children: HashMap<_, _> = agent
            .iter()
            .filter_map(|entry| match entry {
                TimelineEntry::AgentLifecycle { id, .. } => Some((id.clone(), entry.clone())),
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

    fn interaction(
        &mut self,
        interaction: &Exchange,
        agents: &HashMap<String, TimelineEntry>,
        depth: usize,
    ) -> Result<()> {
        if depth == 0 && self.leading_separator {
            self.literal(&format!("{}:separator", interaction.id), "", None)?;
        }
        let prompt = TranscriptRenderer::new(&self.content_width())?.prompt(
            BlockId(format!("{}:prompt", interaction.id)),
            &interaction.prompt,
        )?;
        self.prompt.push(prompt.id.clone());
        self.push(prompt)?;
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
                        crate::session::HarnessMode::Yolo => "Yolo",
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
        self.push(heading)?;
        let section = self.begin_section();
        let mut response = Vec::new();
        let visible = interaction.node_list.iter().collect::<Vec<_>>();
        let final_position = interaction.completed_at_ms.and_then(|_| {
            visible.iter().rposition(|node| {
                if let ExchangeNode::TurnContent {
                    turn_id,
                    item: crate::turn::TurnItem::Message { id },
                    ..
                } = node
                {
                    interaction
                        .turn
                        .iter()
                        .find(|turn| turn.id() == turn_id)
                        .and_then(|turn| turn.messages().iter().find(|message| message.id() == id))
                        .is_some_and(|message| {
                            message.delivery() == crate::turn::MessageDelivery::Final
                        })
                } else {
                    false
                }
            })
        });
        let mut trailing_plan = Vec::new();
        let mut rendered_tool = std::collections::HashSet::new();
        for (position, node) in visible.iter().enumerate() {
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
                            let final_message = interaction.completed_at_ms.is_some()
                                && message.delivery() == crate::turn::MessageDelivery::Final;
                            if final_message {
                                response.push((id.clone(), message.text()));
                            } else {
                                let commentary = TranscriptRenderer::new(&self.content_width())?
                                    .commentary(BlockId(id.clone()), message.text())?;
                                self.push(commentary)?;
                            }
                        }
                        crate::turn::TurnItem::Tool { .. } => {
                            if rendered_tool.contains(id) {
                                continue;
                            }
                            let group = visible.iter().skip(position).take_while(|node| matches!(node,
                                ExchangeNode::TurnContent { turn_id: owner, item: crate::turn::TurnItem::Tool { .. }, .. } if owner == turn_id));
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
                                calls.push((id, tool));
                            }
                            let start = self.block.len();
                            let count = calls.len();
                            let settled = calls
                                .iter()
                                .all(|(_, tool)| tool.state() != crate::turn::ToolState::Running)
                                && (position + count < visible.len()
                                    || turn.state() != crate::turn::TurnState::Running);
                            let failed = calls.iter().filter(|(_, tool)| tool.failed).count();
                            let mut label = format!(
                                "▸ {} {count} {}",
                                if settled { "Ran" } else { "Running" },
                                if count == 1 { "tool" } else { "tools" }
                            );
                            if failed > 0 {
                                label.push_str(&format!(" ({failed} failed)"));
                            }
                            self.literal(&format!("{id}:tools"), &label, None)?;
                            for (index, (id, tool)) in calls.into_iter().enumerate() {
                                if tool.state() == crate::turn::ToolState::Running {
                                    self.tool(turn.id(), tool)?;
                                } else {
                                    let tool_start = self.block.len();
                                    self.tool(turn.id(), tool)?;
                                    if let Some(diff) = crate::exchange::ProviderDiffBuilder::build(
                                        std::slice::from_ref(tool),
                                    ) {
                                        let diff_start = self.block.len();
                                        self.diff(
                                            &format!("{id}:changes"),
                                            "Changed",
                                            &diff,
                                            "",
                                            None,
                                        )?;
                                        self.indent_range(diff_start, 2);
                                    }
                                    self.fold(tool_start, id, index + 1 < count);
                                }
                            }
                            self.fold(start, &format!("{id}:tools"), settled);
                        }
                    }
                }
                ExchangeNode::PlanEvent { event } => {
                    if final_position.is_some_and(|final_position| position > final_position) {
                        trailing_plan.push(event);
                    } else {
                        self.plan_event(event)?;
                    }
                }
                ExchangeNode::ExchangeInput { prompt } => {
                    if prompt.intent == crate::exchange::InputIntent::Clarification
                        && interaction.node_list.iter().any(|node| matches!(node,
                            ExchangeNode::PlanEvent { event } if matches!(&event.content,
                                crate::plan::PlanEventContent::Lifecycle { lifecycle, .. }
                                if lifecycle.kind == crate::plan::PlanLifecycleKind::QuestionAnswered
                                    && lifecycle.answer.as_deref() == Some(prompt.text.as_str())))) {
                        continue;
                    }
                    let label = match prompt.intent {
                        crate::exchange::InputIntent::Steering => "Steering",
                        crate::exchange::InputIntent::Clarification => "Clarification",
                    };
                    let block = TranscriptRenderer::new(&self.content_width())?.prompt(
                        BlockId(format!("{}:prompt", prompt.id)),
                        &format!("{label}: {}", prompt.text),
                    )?;
                    self.prompt.push(block.id.clone());
                    self.push(block)?;
                }
                ExchangeNode::AgentReference { agent } => {
                    if let Some(entry) = agents
                        .get(&agent.id)
                        .or_else(|| agents.get(&agent.child_agent_id))
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
                ExchangeNode::PlanCommentResolution { resolution } => {
                    for (index, annotation) in resolution.annotation.iter().enumerate() {
                        let identity = format!("{}:comment:{index}", resolution.id);
                        self.literal(&identity, &format!("▸ Resolved {}", annotation.label), None)?;
                        let section = self.begin_section();
                        self.markdown(
                            &format!("{identity}:body"),
                            &annotation.body,
                            MarkdownRole::Detail,
                        )?;
                        self.finish_section(section, &identity, true);
                    }
                }
                ExchangeNode::ArtifactChange { change } => {
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
                    )?
                }
            }
        }
        self.finish_section(
            section,
            &format!("{}:exchange", interaction.id),
            interaction.completed_at_ms.is_some(),
        );
        if let Some(diff) = &interaction.attributed_diff_text {
            self.diff(
                &format!("{}:changes", interaction.id),
                "Changed",
                diff,
                if interaction.attributed_matches_checkpoint {
                    " · checkpoint matched"
                } else {
                    ""
                },
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
                )?;
            }
        }
        for (id, text) in response {
            self.markdown(&id, text, MarkdownRole::Response)?;
        }
        for event in trailing_plan {
            self.plan_event(event)?;
        }
        Ok(())
    }

    fn tool(&mut self, interaction: &str, tool: &crate::exchange::ToolCall) -> Result<()> {
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
        let block = TranscriptRenderer::new(&self.content_width())?.tool_preview(
            BlockId(id),
            target.clone(),
            &tool.kind,
            &tool.status,
            tool.failed,
            &label,
            &output.preview(self.expanded_tool.contains(&call_id)),
            self.expanded_tool.contains(&call_id),
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

    fn diff(
        &mut self,
        id: &str,
        label: &str,
        text: &str,
        suffix: &str,
        file_label: Option<(&str, &str)>,
    ) -> Result<()> {
        if text.is_empty() {
            return Ok(());
        }
        ensure!(
            text.len() <= 8 * 1024 * 1024,
            "transcript diff exceeds 8 MiB"
        );
        self.bytes += text.len();
        let tree = super::changes::ChangeTree::render(
            &TranscriptRenderer::new(&self.content_width())?,
            id,
            label,
            suffix,
            text,
            file_label,
        )?;
        self.bytes += tree.bytes;
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
            MarkdownRole::Detail | MarkdownRole::Comment => {
                forge_buffer::markdown::MarkdownRenderer::render(BlockId(id.into()), text, &width)?
            }
        };
        if comment {
            for row in 0..rendered.block.text.row_count() {
                rendered.block.metadata.gutter.insert(
                    row,
                    Gutter {
                        position: TextPosition { row, column: 0 },
                        chunk: vec![TextChunk {
                            text: if row == 0 { "◦ " } else { "  " }.into(),
                            capture: "Normal".into(),
                        }],
                        priority: 200,
                    },
                );
            }
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
                    code: rendered.code,
                },
            );
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

    fn content_width(&self) -> WidthProfile {
        let mut width = self.width.clone();
        width.columns = width.columns.saturating_sub(self.margin).max(1);
        width
    }

    fn begin_section(&mut self) -> Section {
        let section = Section {
            start: self.block.len() - 1,
            margin: self.margin,
        };
        self.margin += 2;
        section
    }

    fn finish_section(&mut self, section: Section, identity: &str, closed: bool) {
        self.margin = section.margin;
        self.indent_range(section.start + 1, 1);
        self.fold(section.start, identity, closed);
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
            collapse_children: false,
            id: FoldId(identity.into()),
            start: TextPosition { row: 0, column: 0 },
            end: anchor,
            closed,
        });
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
) -> Result<String> {
    let prefix = format!("Working ({elapsed_seconds}s");
    let Some(reasoning_summary) = reasoning_summary else {
        return Ok(format!("{prefix})"));
    };
    let bounded = reasoning_summary.chars().take(4_096).collect::<String>();
    let normalized = bounded.split_whitespace().collect::<Vec<_>>().join(" ");
    if normalized.is_empty() {
        return Ok(format!("{prefix})"));
    }
    let occupied = width.cells(&prefix, 0)? + 4 + WORKING_STATUS_RESERVED_CELLS;
    let available = width.columns.saturating_sub(occupied);
    if available < 4 {
        return Ok(format!("{prefix})"));
    }
    let summary = truncate_to_cells(width, &normalized, available)?;
    Ok(format!("{prefix} · {summary})"))
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

fn exchange_activity_summary(interaction: &Exchange, now_ms: i64) -> String {
    let complete = interaction.completed_at_ms.is_some();
    let paused = interaction.state == ExchangeState::Running
        && !complete
        && interaction.execution_started_at_ms.is_none();
    let duration = interaction.elapsed(now_ms) / 1000;
    let activity = if interaction.state == ExchangeState::Cancelled {
        "Cancelled after"
    } else if interaction.state == ExchangeState::Finalizing {
        "Finalizing after"
    } else if interaction.state == ExchangeState::Interrupted {
        "Interrupted after"
    } else if interaction.state == ExchangeState::Failed {
        "Failed after"
    } else {
        match interaction.kind {
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision
                if interaction.awaiting_input || paused =>
            {
                "Planning paused after"
            }
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision if complete => "Planned for",
            ExchangeKind::PlanDraft | ExchangeKind::PlanRevision => "Planning for",
            ExchangeKind::PlanExecution if complete => "Executed plan for",
            ExchangeKind::PlanExecution if paused => "Plan execution paused after",
            ExchangeKind::PlanExecution => "Executing plan for",
            ExchangeKind::Chat if complete => "Thought for",
            ExchangeKind::Chat if paused => "Paused after",
            ExchangeKind::Chat => "Thinking for",
        }
    };
    let mut summary = format!(
        "▸ {}{activity} {duration}s",
        history_prefix(interaction.disposition)
    );
    if complete || interaction.awaiting_input || paused {
        if let Some(tokens) = interaction.token_count {
            let display = if tokens >= 1000 {
                format!("{:.1}k", tokens as f64 / 1000.0)
            } else {
                tokens.to_string()
            };
            summary.push_str(&format!(", {display} tokens"));
        }
    }
    let spawned = interaction
        .node_list
        .iter()
        .filter_map(|node| match node {
            ExchangeNode::AgentReference { agent } => Some(agent.child_agent_id.as_str()),
            _ => None,
        })
        .collect::<std::collections::HashSet<_>>()
        .len() as u64;
    let count: usize = interaction
        .turn
        .iter()
        .map(|turn| turn.tools().count())
        .sum();
    let failed = interaction
        .turn
        .iter()
        .flat_map(|turn| turn.tools())
        .filter(|tool| tool.failed)
        .count();
    if count > 0 {
        summary.push_str(&format!(
            ", {count} {} called",
            if count == 1 { "tool" } else { "tools" }
        ));
        if failed > 0 {
            summary.push_str(&format!(" ({failed} failed)"));
        }
    }
    if spawned > 0 {
        let count = spawned;
        summary.push_str(&format!(
            ", {count} {} spawned",
            if count == 1 { "agent" } else { "agents" }
        ));
    }
    if let Some(error) = &interaction.finalization_error {
        summary.push_str(&format!(" — {}", error.replace(['\n', '\r'], " ")));
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
            for row in 0..block.text.row_count() {
                let gutter = block
                    .metadata
                    .gutter
                    .iter()
                    .find(|gutter| gutter.position.row == row)
                    .unwrap();
                assert_eq!(
                    gutter.chunk[0].text,
                    if row == 0 { "    ◦ " } else { "      " }
                );
                assert_eq!(gutter.chunk[0].capture, "Normal");
            }
            assert!(block.text.row_count() > 1, "fixture must exercise wrapping");
        }
    }

    #[test]
    fn activity_summary_uses_retained_mode_and_recognizes_legacy_planning() {
        for (mode, capture) in [
            ("read", "ForgeHarnessRead"),
            ("write", "ForgeHarnessWrite"),
            ("full", "ForgeHarnessFull"),
            ("yolo", "ForgeHarnessYolo"),
            ("plan", "ForgeHarnessPlan"),
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
            ("delegation", 4),
            ("nested:prompt", 6),
            ("nested:summary", 6),
        ] {
            let block = projected
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == identity)
                .unwrap();
            let prefix = block
                .metadata
                .gutter
                .iter()
                .filter(|gutter| gutter.position.row == 0 && gutter.position.column == 0)
                .flat_map(|gutter| &gutter.chunk)
                .map(|chunk| chunk.text.as_str())
                .collect::<String>();
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
            let indentation = |block: &forge_buffer::block::BufferBlock, row| {
                block
                    .metadata
                    .gutter
                    .iter()
                    .filter(|gutter| gutter.position.row == row && gutter.position.column == 0)
                    .flat_map(|gutter| &gutter.chunk)
                    .map(|chunk| chunk.text.as_str())
                    .collect::<String>()
            };
            for index in 0..8 {
                let heading = blocks
                    .iter()
                    .find(|block| block.id.0 == format!("event-{index}"))
                    .unwrap();
                let detail = blocks
                    .iter()
                    .find(|block| block.id.0 == format!("event-{index}:comment"))
                    .unwrap();
                assert_eq!(indentation(heading, 0), "  ");
                for row in 0..detail.text.row_count() {
                    assert_eq!(
                        indentation(detail, row),
                        "    ",
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
                blocks.last().unwrap().id
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
        let commentary = text.find("Inspecting ownership").unwrap();
        let tools = text.find("Ran 1 tool").unwrap();
        let steering = text.find("Also check tests").unwrap();
        let response = text.find("Finished the work").unwrap();
        assert!(commentary < tools && tools < steering && steering < response);
        assert_eq!(text.matches("Finished the work").count(), 1);
        assert_eq!(projected.tool.len(), 1);
        assert!(
            projected.action.values().any(|action| matches!(action,
            super::TranscriptAction::Diff { text } if text.contains("owned.rs"))),
            "native tool lost its derived file diff"
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
            text.find("Finished the work").unwrap() < text.find("Plan cancelled").unwrap(),
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
    fn working_status_reports_elapsed_seconds_without_persisting_a_timer_value() {
        let projection = project_at(
            &TimelineEntry::Status {
                id: "session:status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    started_at_ms: 1_000,
                    activity: WorkflowActivity::Working,
                    reasoning_summary: None,
                },
            },
            &WidthProfile::default(),
            4_999,
        )
        .unwrap();

        assert_eq!(
            projection.entry.block[0].text.wire_rows(),
            vec!["", "Working (3s)"]
        );
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
            vec!["", "Working (3s · Inspecting repository structure befo…)"]
        );
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
            vec!["", "Awaiting plan review · revision 1"]
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
            &TimelineEntry::Exchange {
                id: "interaction".into(),
                created_at_ms: 1_000,
                exchange: interaction,
                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            1_000,
            true,
            &std::collections::HashSet::new(),
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
                    format!("▸ {label} after 3s")
                );
            }
        }
    }

    #[test]
    fn paused_exchange_activity_summary_freezes_until_execution_resumes() {
        for (kind, paused, running) in [
            ("chat", "Paused after", "Thinking for"),
            ("plan_draft", "Planning paused after", "Planning for"),
            ("plan_revision", "Planning paused after", "Planning for"),
            (
                "plan_execution",
                "Plan execution paused after",
                "Executing plan for",
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
                    format!("▸ {paused} 3s")
                );
            }
            exchange.resume(90_000).unwrap();
            assert_eq!(
                super::exchange_activity_summary(&exchange, 92_000),
                format!("▸ {running} 5s")
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
                summary.text.wire_rows().join(""),
                format!("▸ {marker} · Failed after 3s")
            );
            assert_eq!(exchange.state, ExchangeState::Failed);
            assert_eq!(exchange.elapsed(90_000), 3000);
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
            "▸ Planning for 3s"
        );
        interaction.pause(5500);
        interaction.token_count = Some(1280);
        interaction.awaiting_input = true;
        assert_eq!(
            super::exchange_activity_summary(&interaction, 20000),
            "▸ Planning paused after 4s, 1.3k tokens"
        );
        interaction.kind = ExchangeKind::Chat;
        interaction
            .finish(crate::exchange::ExchangeState::Complete, 5500)
            .unwrap();
        assert_eq!(
            super::exchange_activity_summary(&interaction, 20000),
            "▸ Thought for 4s, 1.3k tokens"
        );
    }
}
