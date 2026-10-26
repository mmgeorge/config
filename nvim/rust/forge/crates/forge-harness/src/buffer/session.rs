use std::collections::{HashMap, HashSet, VecDeque};

use anyhow::{Context, Result, ensure};
use forge_buffer::identity::{DocumentId, DocumentRevision, InputSequence, TargetId, ViewId};
use forge_buffer::input::DocumentInput;
use forge_buffer::patch::{BufferPatch, BufferSnapshot};
use forge_buffer::width::WidthProfile;
use serde::{Deserialize, Serialize};

use crate::exchange::Exchange;
use crate::session::state_machine::SessionPhase;
use crate::timeline::{
    TimelineEntry,
    stream::{TimelineOperation, TimelinePatch, TimelineStream},
};

use super::document::{TranscriptChange, TranscriptDocument, TranscriptEntry};
use super::output::OutputDocument;
use super::projection::{ProjectedEntry, ProjectionSource, TranscriptAction, project};

const MAX_PENDING_BYTES: usize = 2 * 1024 * 1024;

pub struct SessionPresentation {
    session_id: String,
    timeline: TimelineStream,
    open: Option<OpenPresentation>,
    activity_updated: std::time::Instant,
}

struct OpenPresentation {
    unsettled_exchange: Vec<String>,
    syntax_done: HashSet<TargetId>,
    syntax: HashMap<TargetId, super::syntax::MarkdownSyntax>,
    agent_scope: Option<String>,
    last_width: WidthProfile,
    transcript: TranscriptDocument,
    sections: super::sections::SectionProjection,
    transcript_id: DocumentId,
    submission_sequence: u64,
    pending_submission: Option<u64>,
    accepted_submission: Option<u64>,
    staging: Option<StagingContent>,
    staging_sequence: u64,
    selection_sequence: u64,
    entry: HashMap<String, EntryPresentation>,
    action: HashMap<TargetId, TranscriptAction>,
    output: HashMap<DocumentId, OutputDocument>,
    pending: VecDeque<(BufferPatch, usize)>,
    pending_bytes: usize,
    input: HashMap<ViewId, InputSequence>,
    failure: Option<String>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct StagingContent {
    queue: Vec<String>,
    steering: Vec<String>,
}

struct EntryPresentation {
    prompt: Vec<forge_buffer::identity::BlockId>,
    response: Vec<forge_buffer::identity::BlockId>,
    target: Vec<TargetId>,
    syntax: Vec<TargetId>,
    tool: Vec<String>,
}

#[derive(Serialize)]
pub struct PresentationOpen {
    pub syntax_pending: bool,
    pub transcript: BufferSnapshot,
}

#[derive(Serialize)]
pub struct PresentationSync {
    pub syntax_pending: bool,
    pub patch: Vec<BufferPatch>,
    pub snapshot: Option<BufferSnapshot>,
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case")]
pub enum PresentationRequest {
    Node {
        #[serde(flatten)]
        request: super::nodes::NodeRequest,
    },
    BackgroundTerminals,
    Recap {
        model: String,
    },
    SessionName {
        model: String,
    },
    TerminateTerminal {
        id: String,
    },
    Highlight {
        document: DocumentId,
    },
    PlanOpen {
        document: DocumentId,
        view: ViewId,
        plan_id: String,
        digest: String,
        revision: Option<u32>,
        width: WidthProfile,
        saved_source_digest: Option<String>,
        focused_annotation: Option<String>,
    },
    PlanAction {
        input: DocumentInput,
    },
    /// Complete an on-demand project-LSP lookup against the originating review action.
    PlanDefinition {
        input: DocumentInput,
        source: crate::plan::WorkspaceDefinition,
        target: crate::plan::DefinitionLocation,
    },
    PlanSaveAnnotations {
        document: DocumentId,
        saved_source_digest: String,
        annotation: Vec<crate::plan::ReviewAnnotation>,
    },
    PlanView {
        document: DocumentId,
        view: ViewId,
        width: Option<WidthProfile>,
    },
    PlanClose {
        document: DocumentId,
    },
    Open {
        document: DocumentId,
        view: ViewId,
        width: WidthProfile,
    },
    Sync {
        document: DocumentId,
        revision: DocumentRevision,
    },
    Staging {
        document: DocumentId,
        sequence: u64,
        #[serde(default)]
        queue: Vec<String>,
        #[serde(default)]
        steering: Vec<String>,
    },
    Snapshot {
        document: DocumentId,
    },
    Input {
        input: DocumentInput,
    },
    NavigatePrompt {
        input: DocumentInput,
        previous: bool,
    },
    SelectAgent {
        document: DocumentId,
        sequence: u64,
        run_id: Option<String>,
    },
    Close {
        document: DocumentId,
    },
    Resize {
        document: DocumentId,
        view: ViewId,
        width: WidthProfile,
    },
    CloseView {
        document: DocumentId,
        view: ViewId,
    },
    OpenView {
        document: DocumentId,
        view: ViewId,
        width: WidthProfile,
    },
    ToolOpen {
        input: DocumentInput,
        document: DocumentId,
    },
    ToolDemand {
        document: DocumentId,
        revision: DocumentRevision,
    },
    ToolExport {
        document: DocumentId,
    },
}

impl SessionPresentation {
    pub(crate) fn append_tool_output(
        &mut self,
        exchange: &Exchange,
        event: &crate::backend::BackendEvent,
    ) -> Result<Option<TimelinePatch>> {
        let patch = self.timeline.append_tool_output(exchange, event)?;
        if let Some(patch) = &patch {
            self.project_patch(patch);
        }
        Ok(patch)
    }

    pub(crate) fn update_message(
        &mut self,
        exchange: &Exchange,
        event: &crate::backend::BackendEvent,
    ) -> Result<Option<TimelinePatch>> {
        let patch = self.timeline.update_message(exchange, event)?;
        if let Some(patch) = &patch {
            self.project_patch(patch);
        }
        Ok(patch)
    }

    /// Capture visible context without acquiring the active prompt broker.
    pub(crate) fn conversation_history(&self) -> Result<String> {
        crate::backend::text_generation::history(self.timeline.entry_list())
    }
    pub fn dispatch(&mut self, request: PresentationRequest) -> Result<serde_json::Value> {
        use serde_json::{json, to_value};
        match request {
            PresentationRequest::Node { request } => {
                use super::nodes::NodeAction;
                self.document(&request.document)?;
                let open = self.open.as_mut().expect("validated presentation");
                if !open.sections.admit(
                    &request.view,
                    request.sequence,
                    &request.node,
                    request.generation,
                )? {
                    return Ok(json!({}));
                }
                let (expanded, more) = match request.action {
                    NodeAction::SetExpansion { expanded } => (expanded, false),
                    NodeAction::LoadMore => (true, true),
                    NodeAction::RetryLoading => (true, false),
                };
                ensure!(
                    (1..=8192).contains(&request.rows),
                    "node row budget exceeds layout limits"
                );
                request.width.validate()?;
                if let Some(call) = request.node.strip_suffix(":tool")
                    && open.sections.nodes.tool.contains_key(call)
                    && (more || open.sections.expansion(&request.node) != Some(expanded))
                {
                    let limit = if more {
                        open.sections
                            .nodes
                            .tool_limit
                            .get(&request.node)
                            .copied()
                            .unwrap_or(super::tool::INITIAL_INLINE_BYTES)
                            .saturating_add(super::tool::INITIAL_INLINE_BYTES)
                    } else {
                        super::tool::INITIAL_INLINE_BYTES
                    };
                    toggle_inline_output(open, call, expanded, limit)?;
                }
                open.sections.set(
                    &mut open.transcript,
                    request.view,
                    request.sequence,
                    &request.node,
                    expanded,
                    more,
                    Some((request.rows, request.width)),
                )?;
                retain_patches(open, Vec::new())?;
                Ok(json!({}))
            }
            PresentationRequest::Highlight { .. } => {
                anyhow::bail!("syntax requires the asynchronous service owner")
            }
            PresentationRequest::PlanOpen { .. }
            | PresentationRequest::PlanAction { .. }
            | PresentationRequest::PlanDefinition { .. }
            | PresentationRequest::PlanSaveAnnotations { .. }
            | PresentationRequest::PlanView { .. }
            | PresentationRequest::PlanClose { .. } => {
                anyhow::bail!("plan review requires the physical storage owner")
            }
            PresentationRequest::OpenView {
                document,
                view,
                width,
            } => {
                self.document(&document)?;
                let open = self.open.as_mut().expect("validated open presentation");
                if open.transcript.views.open(view.clone(), width)? {
                    reflow(open, self.timeline.entry_list(), self.timeline.revision())?;
                }
                open.sections.register(view.clone());
                open.input.entry(view).or_insert(InputSequence(0));
                Ok(json!({}))
            }
            PresentationRequest::SelectAgent {
                document,
                sequence,
                run_id,
            } => {
                if let Some(run_id) = &run_id {
                    ensure!(
                        !run_id.is_empty() && run_id.len() <= 256,
                        "invalid agent identity"
                    );
                    ensure!(
                        find_agent(self.timeline.entry_list(), run_id, 0).is_some(),
                        "agent history is unavailable"
                    );
                }
                let open = self
                    .open
                    .as_mut()
                    .context("session presentation is not open")?;
                ensure!(
                    open.transcript_id == document,
                    "transcript document lifetime changed"
                );
                if sequence <= open.selection_sequence {
                    return Ok(json!({}));
                }
                let previous = std::mem::replace(&mut open.agent_scope, run_id);
                if let Err(failure) =
                    reflow(open, self.timeline.entry_list(), self.timeline.revision())
                {
                    open.agent_scope = previous;
                    return Err(failure);
                }
                open.selection_sequence = sequence;
                Ok(json!({}))
            }
            PresentationRequest::NavigatePrompt { input, previous } => {
                self.navigate_prompt(input, previous)
            }
            PresentationRequest::ToolOpen { input, document } => {
                document.validate()?;
                let TranscriptAction::Tool { call_id } = self.action(input)? else {
                    anyhow::bail!("transcript target is not a tool output");
                };
                let open = self
                    .open
                    .as_mut()
                    .context("session presentation is not open")?;
                ensure!(
                    !open.output.contains_key(&document) && document != open.transcript_id,
                    "tool document identity is already used"
                );
                let tool = open
                    .sections
                    .nodes
                    .tool
                    .get(&call_id)
                    .context("saved tool output is unavailable")?;
                let source = tool.snapshot()?;
                let output = OutputDocument::new(document.clone(), source)?;
                let snapshot = output.snapshot();
                open.output.insert(document, output);
                Ok(
                    json!({"snapshot":snapshot,"more":true,"title":format!("Tool output {call_id}")}),
                )
            }
            PresentationRequest::ToolDemand { document, revision } => {
                let open = self
                    .open
                    .as_mut()
                    .context("session presentation is not open")?;
                let output = open
                    .output
                    .get_mut(&document)
                    .context("unknown tool output document")?;
                Ok(to_value(output.demand(revision)?)?)
            }
            PresentationRequest::ToolExport { .. } => {
                anyhow::bail!("tool export requires the initialized storage owner")
            }
            PresentationRequest::BackgroundTerminals
            | PresentationRequest::TerminateTerminal { .. }
            | PresentationRequest::Recap { .. }
            | PresentationRequest::SessionName { .. } => {
                anyhow::bail!("provider operation requires the provider owner")
            }
            PresentationRequest::Open {
                document,
                view,
                width,
            } => Ok(to_value(self.open(document, view, width)?)?),
            PresentationRequest::Sync { document, revision } => {
                Ok(to_value(self.sync(&document, revision)?)?)
            }
            PresentationRequest::Staging {
                document,
                sequence,
                queue,
                steering,
            } => {
                self.set_staging(&document, sequence, queue, steering)?;
                Ok(json!({"updated":true}))
            }
            PresentationRequest::Snapshot { document } => Ok(to_value(self.snapshot(&document)?)?),
            PresentationRequest::Input { input } => match self.action(input)? {
                TranscriptAction::Diff { .. } => Ok(json!({"kind":"diff"})),
                action => Ok(to_value(action)?),
            },
            PresentationRequest::Close { document } => {
                self.close(&document)?;
                Ok(json!({"closed":true}))
            }
            PresentationRequest::Resize {
                document,
                view,
                width,
            } => {
                self.document(&document)?;
                let open = self.open.as_mut().expect("validated open presentation");
                if open.transcript.views.resize(&view, width)? {
                    reflow(open, self.timeline.entry_list(), self.timeline.revision())?;
                }
                Ok(json!({"updated":true}))
            }
            PresentationRequest::CloseView { document, view } => {
                self.document(&document)?;
                let open = self.open.as_mut().expect("validated open presentation");
                open.input.remove(&view);
                open.sections.close(&mut open.transcript, &view);
                retain_patches(open, Vec::new())?;
                if open.transcript.views.close(&view) && open.transcript.views.profile().is_some() {
                    reflow(open, self.timeline.entry_list(), self.timeline.revision())?;
                }
                Ok(json!({"closed":true}))
            }
        }
    }
    pub fn new(session_id: String) -> Self {
        Self {
            timeline: TimelineStream::new(session_id.clone()),
            session_id,
            open: None,
            activity_updated: std::time::Instant::now(),
        }
    }

    pub fn initialize(&mut self, entry: Vec<TimelineEntry>) -> Result<()> {
        self.timeline.initialize(entry)
    }

    pub fn revision(&self) -> u64 {
        self.timeline.revision()
    }

    pub(crate) fn update_checks(&mut self, run: &crate::checks::CheckRun) -> Result<TimelinePatch> {
        let patch = self.timeline.update_checks(run)?;
        self.project_patch(&patch);
        Ok(patch)
    }

    pub fn reconcile(&mut self, entry: Vec<TimelineEntry>) -> Result<TimelinePatch> {
        let patch = self.timeline.reconcile(entry)?;
        self.project_patch(&patch);
        Ok(patch)
    }

    /// Applies the current visibility preference to snapshots captured before a toggle.
    pub(crate) fn reconcile_with_status_thoughts(
        &mut self,
        mut entry: Vec<TimelineEntry>,
        enabled: bool,
    ) -> Result<TimelinePatch> {
        for entry in &mut entry {
            if let TimelineEntry::Status {
                status_thoughts, ..
            } = entry
            {
                *status_thoughts = enabled;
            }
        }
        self.reconcile(entry)
    }

    pub(crate) fn set_status_thoughts(&mut self, enabled: bool) -> Result<TimelinePatch> {
        let patch = self.timeline.set_status_thoughts(enabled)?;
        self.project_patch(&patch);
        Ok(patch)
    }

    pub fn update_live(
        &mut self,
        interaction: Option<&Exchange>,
        removed: Option<&str>,
        status: SessionPhase,
        status_thoughts: bool,
    ) -> Result<TimelinePatch> {
        let patch = self
            .timeline
            .update_live(interaction, removed, status, status_thoughts)?;
        self.project_patch(&patch);
        Ok(patch)
    }

    pub fn open(
        &mut self,
        document: DocumentId,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<PresentationOpen> {
        document.validate()?;
        if let Some(open) = self.open.as_mut() {
            ensure!(
                open.transcript_id == document,
                "session presentation is already owned by another document lifetime"
            );
            if open.transcript.views.open(view.clone(), width)? {
                reflow(open, self.timeline.entry_list(), self.timeline.revision())?;
            }
            open.sections.register(view.clone());
            open.input.entry(view).or_insert(InputSequence(0));
            return Ok(PresentationOpen {
                syntax_pending: syntax_pending(open),
                transcript: open.sections.document.snapshot()?,
            });
        }
        let mut projected = Vec::new();
        let unsettled = unsettled_exchanges(
            self.timeline
                .entry_list()
                .iter()
                .map(ProjectionSource::Entry),
        );
        for (index, entry) in self.timeline.entry_list().iter().enumerate() {
            let mut entry = project(
                ProjectionSource::Entry(entry),
                &width,
                index > 0,
                &HashMap::new(),
                &HashMap::new(),
            )?;
            if !unsettled.contains(&entry.entry.id) {
                settle_responses(&mut entry);
            }
            projected.push(entry);
        }
        let mut entry = HashMap::new();
        let mut action = HashMap::new();
        let mut syntax = HashMap::new();
        let mut tool = HashMap::new();
        let mut block = Vec::new();
        for mut projected in projected {
            for tool in projected.tool.values_mut() {
                tool.own(&projected.entry.id);
            }
            entry.insert(projected.entry.id.clone(), entry_presentation(&projected));
            block.push(projected.entry);
            action.extend(projected.action);
            syntax.extend(projected.syntax);
            tool.extend(projected.tool);
        }
        let mut transcript = TranscriptDocument::initialize(
            document.clone(),
            self.session_id.clone(),
            self.timeline.revision(),
            block,
        )?;
        transcript.views.open(view.clone(), width.clone())?;
        let mut sections =
            super::sections::SectionProjection::new(&mut transcript, document.clone())?;
        sections.register(view.clone());
        sections.nodes.tool = tool;
        let opened = PresentationOpen {
            syntax_pending: !syntax.is_empty()
                || action
                    .values()
                    .any(|action| matches!(action, TranscriptAction::Diff { syntax: true, .. })),
            transcript: sections.document.snapshot()?,
        };
        self.open = Some(OpenPresentation {
            unsettled_exchange: unsettled,
            syntax_done: HashSet::new(),
            syntax,
            agent_scope: None,
            last_width: width,
            transcript,
            sections,
            transcript_id: document,
            submission_sequence: 0,
            staging_sequence: 0,
            selection_sequence: 0,
            pending_submission: None,
            accepted_submission: None,
            staging: None,
            entry,
            action,
            output: HashMap::new(),
            pending: VecDeque::new(),
            pending_bytes: 0,
            input: HashMap::from([(view, InputSequence(0))]),
            failure: None,
        });
        Ok(opened)
    }

    pub fn sync(
        &mut self,
        document: &DocumentId,
        revision: DocumentRevision,
    ) -> Result<PresentationSync> {
        if self.activity_updated.elapsed() >= std::time::Duration::from_secs(1)
            && self.timeline.ticking_entries().next().is_some()
        {
            let open = self
                .open
                .as_mut()
                .context("session presentation is not open")?;
            refresh_activity(open, self.timeline.ticking_entries())?;
            self.activity_updated = std::time::Instant::now();
        }
        let open = self.document(document)?;
        if let Some(failure) = &open.failure {
            anyhow::bail!("transcript projection requires reopening: {failure}");
        }
        let current = open.sections.document.document.revision();
        ensure!(
            revision <= current,
            "transcript revision is ahead of its native owner"
        );
        let mut cursor = revision;
        let mut patch = Vec::new();
        for (candidate, _) in &open.pending {
            if candidate.next <= cursor {
                continue;
            }
            if candidate.base != cursor {
                break;
            }
            cursor = candidate.next;
            patch.push(candidate.clone());
        }
        if cursor == current {
            Ok(PresentationSync {
                syntax_pending: syntax_pending(open),
                patch,
                snapshot: None,
            })
        } else {
            Ok(PresentationSync {
                syntax_pending: syntax_pending(open),
                patch: Vec::new(),
                snapshot: Some(open.sections.document.snapshot()?),
            })
        }
    }

    fn set_staging(
        &mut self,
        document: &DocumentId,
        sequence: u64,
        queue: Vec<String>,
        steering: Vec<String>,
    ) -> Result<()> {
        let open = self
            .open
            .as_mut()
            .context("session presentation is not open")?;
        ensure!(
            open.transcript_id == *document,
            "transcript document identity changed"
        );
        if sequence <= open.staging_sequence {
            return Ok(());
        }
        let staging = StagingContent { queue, steering };
        if open.staging.as_ref() == Some(&staging) {
            open.staging_sequence = sequence;
            return Ok(());
        }
        let previous = open.staging.clone();
        open.staging = if staging.queue.is_empty() && staging.steering.is_empty() {
            None
        } else {
            Some(staging)
        };
        if let Err(error) = update_staging_block(open) {
            open.staging = previous;
            return Err(error);
        }
        open.staging_sequence = sequence;
        Ok(())
    }

    /// Capture one syntax source while keeping asynchronous work outside the presentation lock.
    pub(crate) fn capture_syntax(
        &mut self,
        document: &DocumentId,
    ) -> Result<Option<super::syntax::TranscriptSyntax>> {
        self.document(document)?;
        let open = self.open.as_mut().expect("validated document");
        let selected = open
            .action
            .iter()
            .find_map(|(target, action)| match action {
                TranscriptAction::Diff { text, syntax: true }
                    if !open.syntax_done.contains(target) =>
                {
                    Some(super::syntax::TranscriptSyntax::Diff(
                        super::syntax::SavedDiffSyntax {
                            target: target.clone(),
                            text: text.clone(),
                        },
                    ))
                }
                _ => None,
            })
            .or_else(|| {
                open.syntax.iter().find_map(|(target, job)| {
                    (!open.syntax_done.contains(target))
                        .then(|| super::syntax::TranscriptSyntax::Markdown(job.clone()))
                })
            });
        if let Some(job) = &selected {
            open.syntax_done.insert(job.target().clone());
        }
        Ok(selected)
    }

    /// Publish highlight metadata only into the document and saved source that admitted analysis.
    pub(crate) fn apply_syntax(
        &mut self,
        document: &DocumentId,
        job: &super::syntax::TranscriptSyntax,
        highlighted: Vec<forge_buffer::block::BufferBlock>,
    ) -> Result<()> {
        let Some(open) = self
            .open
            .as_mut()
            .filter(|open| &open.transcript_id == document)
        else {
            return Ok(());
        };
        let current_source = match job {
            super::syntax::TranscriptSyntax::Diff(job) => {
                matches!(open.action.get(&job.target), Some(TranscriptAction::Diff { text, syntax: true }) if text == &job.text)
            }
            super::syntax::TranscriptSyntax::Markdown(job) => {
                open.syntax.get(&job.target) == Some(job)
            }
        };
        if !current_source {
            return Ok(());
        }
        let mut edits = Vec::new();
        for syntax in highlighted {
            let Some(current) = open.transcript.document.block(&syntax.id) else {
                return Ok(());
            };
            if current.text.row_count() != syntax.text.row_count() {
                return Ok(());
            }
            let mut replacement = current.clone();
            for mut span in syntax.metadata.visible_decoration {
                let row = span.range.start.row;
                let source = syntax.text.row(row).expect("syntax row");
                let displayed = current.text.row(row).expect("retained row");
                let Some(prefix) = displayed.strip_suffix(source) else {
                    return Ok(());
                };
                span.range.start.column += prefix.len();
                span.range.end.column += prefix.len();
                replacement.metadata.visible_decoration.push(span);
            }
            let index = open
                .transcript
                .document
                .block_index(&syntax.id)
                .expect("retained block");
            open.transcript.mark_block_dirty(&syntax.id);
            edits.push(forge_buffer::sequence::SequenceEdit {
                range: index..index + 1,
                block: vec![replacement],
            });
        }
        if let Some(patch) = open.transcript.document.edit_many(edits)? {
            retain_patches(open, vec![patch])?;
        }
        open.syntax_done.insert(job.target().clone());
        Ok(())
    }

    pub fn snapshot(&self, document: &DocumentId) -> Result<BufferSnapshot> {
        let open = self
            .open
            .as_ref()
            .context("session presentation is not open")?;
        if let Some(output) = open.output.get(document) {
            Ok(output.snapshot())
        } else {
            self.document(document)?.sections.document.snapshot()
        }
    }

    pub fn action(&mut self, input: DocumentInput) -> Result<TranscriptAction> {
        let open = self.validate_input(&input)?;
        open.action
            .get(
                input
                    .target
                    .as_ref()
                    .context("transcript input has no action")?,
            )
            .cloned()
            .context("transcript target is not actionable")
    }

    fn validate_input(&mut self, input: &DocumentInput) -> Result<&mut OpenPresentation> {
        input.validate()?;
        let open = self
            .open
            .as_mut()
            .context("session presentation is not open")?;
        ensure!(
            input.document == open.transcript_id
                && input.revision <= open.sections.document.document.revision(),
            "transcript input revision changed"
        );
        let sequence = open
            .input
            .get_mut(&input.view)
            .context("unknown transcript view")?;
        ensure!(
            input.sequence > *sequence,
            "stale transcript input sequence"
        );
        let block = open
            .sections
            .document
            .document
            .block(&input.block)
            .context("transcript input block disappeared")?;
        let stable_tool = input.action == "activate"
            && input.target.as_ref().is_some_and(|target| {
                matches!(open.action.get(target), Some(TranscriptAction::Tool { .. }))
                    && block
                        .metadata
                        .target
                        .iter()
                        .any(|candidate| &candidate.id == target)
            });
        if input.revision == open.sections.document.document.revision() {
            let row = block
                .text
                .row(input.position.row)
                .context("transcript input row disappeared")?;
            ensure!(
                row.is_char_boundary(input.position.column),
                "transcript input column changed"
            );
            let target = block.target_at(input.position);
            ensure!(
                input.target.as_ref() == target,
                "transcript input target changed"
            );
        } else {
            ensure!(stable_tool, "transcript input revision changed");
        }
        *sequence = input.sequence;
        Ok(open)
    }

    fn navigate_prompt(
        &mut self,
        input: DocumentInput,
        previous: bool,
    ) -> Result<serde_json::Value> {
        let open = self.validate_input(&input)?;
        let current = open
            .transcript
            .document
            .block_index(&input.block)
            .context("transcript block disappeared")?;
        let mut prompt = open
            .entry
            .values()
            .flat_map(|entry| &entry.prompt)
            .filter_map(|id| {
                open.transcript
                    .document
                    .block_index(id)
                    .map(|index| (index, id))
            })
            .collect::<Vec<_>>();
        prompt.sort_by_key(|(index, _)| *index);
        let selected = if previous {
            prompt
                .iter()
                .rev()
                .find(|(index, _)| *index < current)
                .or_else(|| prompt.first())
        } else {
            prompt
                .iter()
                .find(|(index, _)| *index > current)
                .or_else(|| prompt.last())
        };
        Ok(
            serde_json::json!({"input":input,"anchor":selected.map(|(_, id)| forge_buffer::block::BlockAnchor {
                block:(*id).clone(),position:forge_buffer::block::TextPosition {row:0,column:0}
            })}),
        )
    }

    pub fn begin_submission(
        &mut self,
        document: &DocumentId,
        token: u64,
        text: &str,
    ) -> Result<()> {
        self.document(document)?;
        ensure!(!text.trim().is_empty(), "prompt requires nonempty text");
        let open = self.open.as_mut().expect("validated presentation");
        ensure!(
            open.pending_submission.is_none(),
            "prompt submission is already pending"
        );
        ensure!(
            token > open.submission_sequence && token <= forge_buffer::MAX_COUNTER,
            "prompt submission token is stale or invalid"
        );
        open.submission_sequence = token;
        open.pending_submission = Some(token);
        open.accepted_submission = None;
        Ok(())
    }

    pub fn complete_submission(
        &mut self,
        document: &DocumentId,
        token: u64,
        admitted: bool,
    ) -> Result<Option<serde_json::Value>> {
        let Some(open) = self.open.as_mut() else {
            return Ok(None);
        };
        if &open.transcript_id != document || open.pending_submission != Some(token) {
            return Ok(None);
        }
        open.pending_submission = None;
        if !admitted {
            return Ok(None);
        }
        open.accepted_submission = Some(token);
        Ok(Some(
            serde_json::json!({"document":document,"token":token,"state":"accepted"}),
        ))
    }

    pub fn retract_submission(
        &mut self,
        document: &DocumentId,
        token: u64,
    ) -> Result<Option<serde_json::Value>> {
        let Some(open) = self.open.as_mut() else {
            return Ok(None);
        };
        if &open.transcript_id != document || open.accepted_submission != Some(token) {
            return Ok(None);
        }
        open.accepted_submission = None;
        Ok(Some(
            serde_json::json!({"document":document,"token":token,"state":"retracted"}),
        ))
    }

    pub fn close(&mut self, document: &DocumentId) -> Result<()> {
        if let Some(open) = self.open.as_mut() {
            if let Some(output) = open.output.get_mut(document) {
                output.close()?;
                open.output.remove(document);
                return Ok(());
            }
        }
        self.document(document)?;
        for output in self
            .open
            .as_mut()
            .expect("validated presentation")
            .output
            .values_mut()
        {
            output.close()?;
        }
        self.open = None;
        Ok(())
    }

    pub fn close_all(&mut self) -> Result<()> {
        if let Some(open) = &self.open {
            self.close(&open.transcript_id.clone())?;
        }
        Ok(())
    }

    pub fn export_tool(
        &mut self,
        document: &DocumentId,
        directory: &std::path::Path,
    ) -> Result<std::path::PathBuf> {
        self.open
            .as_mut()
            .context("session presentation is not open")?
            .output
            .get_mut(document)
            .context("unknown tool output document")?
            .export(directory)
    }

    fn document(&self, document: &DocumentId) -> Result<&OpenPresentation> {
        let open = self
            .open
            .as_ref()
            .context("session presentation is not open")?;
        ensure!(
            &open.transcript_id == document,
            "transcript document identity changed"
        );
        Ok(open)
    }

    fn project_patch(&mut self, patch: &TimelinePatch) {
        if patch.is_empty() {
            return;
        }
        let Some(open) = self.open.as_mut() else {
            return;
        };
        if open.failure.is_some() {
            return;
        }
        let unsettled = if let Some(run_id) = open.agent_scope.as_deref() {
            unsettled_exchanges(
                find_agent(self.timeline.entry_list(), run_id, 0)
                    .into_iter()
                    .flat_map(|entry| match entry {
                        TimelineEntry::AgentLifecycle { exchange, .. } => exchange.iter(),
                        _ => unreachable!("agent lookup returns an agent lifecycle"),
                    })
                    .map(ProjectionSource::Exchange),
            )
        } else {
            unsettled_exchanges(
                self.timeline
                    .entry_list()
                    .iter()
                    .map(ProjectionSource::Entry),
            )
        };
        let applied = if open.agent_scope.is_some() {
            if patch.operation.iter().all(|operation| {
                matches!(
                    operation,
                    TimelineOperation::ToolOutput { .. } | TimelineOperation::Message { .. }
                ) || matches!(
                    operation,
                    TimelineOperation::Replace {
                        entry: TimelineEntry::Status { .. },
                        ..
                    }
                )
            }) {
                let filtered: Vec<_> = patch
                    .operation
                    .iter()
                    .filter(|operation| match operation {
                        TimelineOperation::ToolOutput { call_id, .. } => {
                            open.sections.nodes.tool.contains_key(call_id)
                        }
                        TimelineOperation::Message { block_id, .. } => open
                            .transcript
                            .document
                            .block(&forge_buffer::identity::BlockId(block_id.clone()))
                            .is_some(),
                        _ => false,
                    })
                    .collect();
                apply_projection(open, patch, filtered.iter().copied(), &unsettled)
            } else {
                reflow(open, self.timeline.entry_list(), self.timeline.revision())
            }
        } else if patch.operation.iter().any(|operation| match operation {
            TimelineOperation::ToolOutput { call_id, .. } => {
                !open.sections.nodes.tool.contains_key(call_id)
            }
            TimelineOperation::Message { block_id, .. } => open
                .transcript
                .document
                .block(&forge_buffer::identity::BlockId(block_id.clone()))
                .is_none(),
            _ => false,
        }) {
            reflow(open, self.timeline.entry_list(), self.timeline.revision())
        } else {
            apply_projection(open, patch, patch.operation.iter(), &unsettled)
        };
        if let Err(error) = applied {
            open.failure = Some(format!("{error:#}"));
            open.pending.clear();
            open.pending_bytes = 0;
        }
    }
}

/// Identify unanalysed sources without inspecting their potentially large bodies.
fn syntax_pending(open: &OpenPresentation) -> bool {
    open.action.iter().any(|(target, action)| {
        matches!(action, TranscriptAction::Diff { syntax: true, .. })
            && !open.syntax_done.contains(target)
    }) || open
        .syntax
        .keys()
        .any(|target| !open.syntax_done.contains(target))
}

fn entry_presentation(projected: &ProjectedEntry) -> EntryPresentation {
    EntryPresentation {
        prompt: projected.prompt.clone(),
        response: projected
            .entry
            .block
            .iter()
            .filter(|block| {
                block.metadata.node.as_ref().is_some_and(|node| {
                    node.kind == forge_buffer::node::NodeKind::Message
                        && node.lifecycle == forge_buffer::node::NodeLifecycle::Live
                })
            })
            .map(|block| block.id.clone())
            .collect(),
        target: projected.action.keys().cloned().collect(),
        syntax: projected.syntax.keys().cloned().collect(),
        tool: projected.tool.keys().cloned().collect(),
    }
}

/// Keeps the current exchange and the two most recent preceding responses outside automatic folding.
fn unsettled_exchanges<'source>(
    source: impl DoubleEndedIterator<Item = ProjectionSource<'source>>,
) -> Vec<String> {
    let mut exchanges = source.filter_map(|entry| match entry {
        ProjectionSource::Entry(TimelineEntry::Exchange { id, exchange, .. }) => {
            Some((id, exchange))
        }
        ProjectionSource::Exchange(exchange) => Some((&exchange.id, exchange)),
        _ => None,
    });
    let mut unsettled = Vec::new();
    if let Some((id, _)) = exchanges.next_back() {
        unsettled.push(id.clone());
    }
    unsettled.extend(
        exchanges
            .rev()
            .filter(|(_, exchange)| {
                exchange.turn.iter().any(|turn| {
                    turn.messages().iter().any(|message| {
                        message.kind() == crate::turn::MessageKind::Assistant
                            && message.delivery() == crate::turn::MessageDelivery::Final
                    })
                })
            })
            .take(2)
            .map(|(id, _)| id.clone()),
    );
    unsettled
}

fn settle_responses(projected: &mut ProjectedEntry) {
    for block in &mut projected.entry.block {
        super::message::settle(block);
    }
}

fn find_agent<'source>(
    source: &'source [TimelineEntry],
    run_id: &str,
    depth: usize,
) -> Option<&'source TimelineEntry> {
    if depth > 16 {
        return None;
    }
    for entry in source {
        match entry {
            TimelineEntry::AgentLifecycle { run, agent, .. } => {
                if run.id == run_id {
                    return Some(entry);
                }
                if let Some(found) = find_agent(agent, run_id, depth + 1) {
                    return Some(found);
                }
            }
            TimelineEntry::Exchange { agent_by_id, .. } => {
                for attached in agent_by_id.values() {
                    if let Some(found) =
                        find_agent(std::slice::from_ref(attached), run_id, depth + 1)
                    {
                        return Some(found);
                    }
                }
            }
            _ => {}
        }
    }
    None
}

fn refresh_activity<'source>(
    open: &mut OpenPresentation,
    source: impl IntoIterator<Item = &'source TimelineEntry>,
) -> Result<()> {
    let width = open
        .transcript
        .views
        .profile()
        .unwrap_or(&open.last_width)
        .clone();
    let mut blocks = Vec::new();
    for entry in source {
        for block in super::projection::timing_blocks(
            entry,
            &width,
            &open.transcript.document,
            &open.sections.nodes.choice,
            &open.sections.nodes.tool,
        )? {
            blocks.push(block);
        }
    }
    let mut patches = Vec::new();
    for block in blocks {
        if let Some(patch) = open.transcript.append(block)? {
            patches.push(patch);
        }
    }
    retain_patches(open, patches)
}

fn toggle_inline_output(
    open: &mut OpenPresentation,
    call_id: &str,
    expanded: bool,
    limit: usize,
) -> Result<()> {
    use forge_buffer::identity::BlockId;
    let source = open
        .sections
        .nodes
        .tool
        .get_mut(call_id)
        .context("inline tool source is missing")?;
    if expanded {
        source.load_prefix(limit)?;
    }
    let owner = source.owner().to_owned();
    let call = source
        .header()
        .context("inline tool heading source is missing")?;
    let heading_id = BlockId(format!("{call_id}:tool"));
    let previous_heading = open
        .transcript
        .document
        .block(&heading_id)
        .context("inline heading is missing")?;
    let was_expanded = previous_heading
        .metadata
        .node
        .as_ref()
        .is_some_and(|node| node.expansion == Some(true));
    let layout = previous_heading.metadata.layout.clone();
    let mut width = open
        .transcript
        .views
        .profile()
        .unwrap_or(&open.last_width)
        .clone();
    width.columns = width
        .columns
        .saturating_sub(
            layout
                .as_ref()
                .map_or(0, |layout| layout.indent.saturating_sub(2)),
        )
        .max(1);
    let renderer = super::transcript::TranscriptRenderer::new(&width)?;
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)?
        .as_millis() as i64;
    let mut heading = renderer.tool_header(
        heading_id,
        previous_heading.metadata.target[0].id.clone(),
        &call.kind,
        call.elapsed_ms(now),
        call.failed,
        &call.title,
        expanded,
    )?;
    heading.metadata.layout = layout.clone();
    heading.metadata.fold = previous_heading.metadata.fold.clone();
    heading.metadata.node = previous_heading.metadata.node.clone();
    let body_layout = layout.map(|mut layout| {
        layout.marker = None;
        layout
    });
    let chunks = if expanded {
        source.inline_prefix_chunks(limit)?
    } else {
        1
    };
    if let Some(node) = &mut heading.metadata.node {
        node.more = expanded && (chunks < source.inline_chunks()? || source.has_unloaded_output());
        node.resolve(Some(expanded));
    }
    let identity = |chunk| {
        BlockId(if chunk == 0 {
            format!("{call_id}:preview")
        } else {
            format!("{call_id}:output:{chunk}")
        })
    };
    let hidden_id = BlockId(format!("{call_id}:hidden"));
    let removed_chunks = open
        .transcript
        .document
        .block_index(&hidden_id)
        .context("inline hidden anchor is missing")?
        - open
            .transcript
            .document
            .block_index(&identity(0))
            .context("inline body anchor is missing")?;
    let first_chunk = if expanded && was_expanded {
        removed_chunks.min(chunks)
    } else {
        0
    };
    let mut retired_target = HashSet::new();
    for chunk in first_chunk..removed_chunks {
        let block = open
            .transcript
            .document
            .block(&identity(chunk))
            .context("inline body is missing")?;
        retired_target.extend(block.metadata.target.iter().map(|target| target.id.clone()));
    }
    let previous_hidden = open
        .transcript
        .document
        .block(&hidden_id)
        .context("inline hidden counter is missing")?;
    retired_target.extend(
        previous_hidden
            .metadata
            .target
            .iter()
            .map(|target| target.id.clone()),
    );
    let mut body = Vec::new();
    for chunk in first_chunk..chunks {
        let mut block = if !expanded && call.kind == "check" {
            forge_buffer::block::BufferBlock {
                id: identity(chunk),
                text: Default::default(),
                metadata: Default::default(),
            }
        } else {
            renderer.tool_body(call_id, source, expanded, chunk)?
        };
        block.metadata.layout = body_layout.clone();
        body.push(block);
    }
    let mut hidden = if !expanded && call.kind == "check" {
        forge_buffer::block::BufferBlock {
            id: BlockId(format!("{call_id}:hidden")),
            text: Default::default(),
            metadata: Default::default(),
        }
    } else {
        renderer.tool_hidden(call_id, source, expanded)?
    };
    hidden.metadata.layout = body_layout;
    let target = body
        .iter()
        .chain(std::iter::once(&hidden))
        .flat_map(|block| block.metadata.target.iter().map(|target| target.id.clone()))
        .collect::<Vec<_>>();
    let mut changes = vec![TranscriptChange::Block { block: heading }];
    if !body.is_empty() || first_chunk < removed_chunks {
        changes.push(TranscriptChange::ToolBody {
            owner: owner.clone(),
            anchor: if first_chunk < removed_chunks {
                identity(first_chunk)
            } else {
                hidden_id
            },
            removed: removed_chunks - first_chunk,
            block: body,
        });
    }
    changes.push(TranscriptChange::Block { block: hidden });
    let patches = match open.transcript.tool_layout(changes) {
        Ok(patches) => patches,
        Err(failure) => {
            open.failure = Some(format!("{failure:#}"));
            return Err(failure);
        }
    };
    for target in &retired_target {
        open.action.remove(target);
    }
    for target in &target {
        open.action.insert(
            target.clone(),
            TranscriptAction::Tool {
                call_id: call_id.to_owned(),
            },
        );
    }
    let entry = open
        .entry
        .get_mut(&owner)
        .context("inline entry owner is missing")?;
    entry
        .target
        .retain(|target| !retired_target.contains(target));
    entry.target.extend(target);
    let node = format!("{call_id}:tool");
    if expanded {
        open.sections.nodes.tool_limit.insert(node, limit);
    } else {
        open.sections.nodes.tool_limit.remove(&node);
    }
    let _ = patches;
    Ok(())
}

fn reflow(
    open: &mut OpenPresentation,
    source: &[TimelineEntry],
    timeline_revision: u64,
) -> Result<()> {
    let source = match open.agent_scope.as_deref() {
        None => source
            .iter()
            .map(ProjectionSource::Entry)
            .collect::<Vec<_>>(),
        Some(run_id) => find_agent(source, run_id, 0)
            .into_iter()
            .flat_map(|entry| match entry {
                TimelineEntry::AgentLifecycle { exchange, .. } => exchange.iter(),
                _ => unreachable!("agent lookup returns an agent lifecycle"),
            })
            .map(ProjectionSource::Exchange)
            .collect::<Vec<_>>(),
    };
    let width = open
        .transcript
        .views
        .profile()
        .unwrap_or(&open.last_width)
        .clone();
    let mut projected = Vec::new();
    let unsettled = unsettled_exchanges(source.iter().copied());
    for (index, entry) in source.into_iter().enumerate() {
        let mut entry = project(
            entry,
            &width,
            index > 0,
            &open.sections.nodes.choice,
            &open.sections.nodes.tool_limit,
        )?;
        if !unsettled.contains(&entry.entry.id) {
            settle_responses(&mut entry);
        }
        projected.push(entry);
    }
    let mut entry = HashMap::new();
    let mut action = HashMap::new();
    let mut syntax = HashMap::new();
    let mut tool = HashMap::new();
    let mut blocks = Vec::new();
    for mut projection in projected {
        for tool in projection.tool.values_mut() {
            tool.own(&projection.entry.id);
        }
        entry.insert(projection.entry.id.clone(), entry_presentation(&projection));
        action.extend(projection.action);
        syntax.extend(projection.syntax);
        tool.extend(projection.tool);
        blocks.push(projection.entry);
    }
    let patch = open.transcript.replace_scope(blocks, timeline_revision)?;
    for (id, output) in &mut tool {
        if let Some(mut previous) = open.sections.nodes.tool.remove(id) {
            if previous.retain(output) {
                *output = previous;
            }
        }
    }
    open.syntax_done.clear();
    open.entry = entry;
    open.action = action;
    open.syntax = syntax;
    open.sections.nodes.tool = tool;
    open.last_width = width;
    open.unsettled_exchange = unsettled;
    retain_patches(open, patch)
}

fn apply_projection<'operation>(
    open: &mut OpenPresentation,
    patch: &TimelinePatch,
    operations: impl Iterator<Item = &'operation TimelineOperation> + Clone,
    unsettled: &[String],
) -> Result<()> {
    let width = open.transcript.views.profile().unwrap_or(&open.last_width);
    let mut projected = Vec::new();
    let mut changed = Vec::new();
    if open.unsettled_exchange != unsettled {
        for id in open
            .unsettled_exchange
            .iter()
            .filter(|id| !unsettled.contains(id))
        {
            let Some(entry) = open.entry.get_mut(id) else {
                continue;
            };
            for response in entry.response.drain(..) {
                if let Some(previous) = open.transcript.document.block(&response) {
                    let mut block = previous.clone();
                    super::message::settle(&mut block);
                    changed.push(TranscriptChange::Block { block });
                }
            }
        }
        open.unsettled_exchange = unsettled.to_vec();
    }
    for operation in operations.clone() {
        match operation {
            TimelineOperation::Message {
                entry_id,
                exchange_id,
                block_id,
                message,
                ..
            } => {
                if message.kind() != crate::turn::MessageKind::Assistant {
                    continue;
                }
                let block_id = forge_buffer::identity::BlockId(block_id.clone());
                let previous = open
                    .transcript
                    .document
                    .block(&block_id)
                    .context("streamed message block is missing")?;
                let mut replacement = super::projection::message_block(previous, message, width)?;
                let owner = if open.agent_scope.is_some() {
                    exchange_id
                } else {
                    entry_id
                };
                if !unsettled.contains(owner) {
                    settle_responses(&mut replacement);
                }
                let previous_targets = previous
                    .metadata
                    .target
                    .iter()
                    .map(|target| target.id.clone())
                    .collect::<Vec<_>>();
                for target in &previous_targets {
                    open.action.remove(target);
                    open.syntax_done.remove(target);
                }
                let syntax_target = TargetId(format!("{}:markdown-syntax", block_id.0));
                open.syntax.remove(&syntax_target);
                open.syntax_done.remove(&syntax_target);
                if let Some(entry) = open.entry.get_mut(owner) {
                    entry
                        .target
                        .retain(|target| !previous_targets.contains(target));
                    entry.target.extend(replacement.action.keys().cloned());
                    entry.syntax.retain(|target| target != &syntax_target);
                    entry.syntax.extend(replacement.syntax.keys().cloned());
                    entry.response.retain(|response| response != &block_id);
                    entry
                        .response
                        .extend(entry_presentation(&replacement).response);
                }
                open.action.extend(replacement.action);
                open.syntax.extend(replacement.syntax);
                changed.push(TranscriptChange::Block {
                    block: replacement
                        .entry
                        .block
                        .into_iter()
                        .next()
                        .expect("message block"),
                });
            }
            TimelineOperation::ToolOutput {
                entry_id,
                exchange_id,
                call_id,
                delta,
                ..
            } => {
                let node_id = format!("{call_id}:tool");
                let expanded = open.sections.expansion(&node_id) == Some(true);
                let limit = open
                    .sections
                    .nodes
                    .tool_limit
                    .get(&node_id)
                    .copied()
                    .unwrap_or(super::tool::INITIAL_INLINE_BYTES);
                let source = open
                    .sections
                    .nodes
                    .tool
                    .get_mut(call_id)
                    .context("streamed tool cache is missing")?;
                let old_chunks = if expanded {
                    source.inline_prefix_chunks(limit)?
                } else {
                    1
                };
                let first_chunk = if expanded { source.inline_tail()? } else { 0 };
                source.append(delta)?;
                if source.group.is_empty() {
                    continue;
                }
                let block_id = forge_buffer::identity::BlockId(format!("{call_id}:tool"));
                let previous = open
                    .transcript
                    .document
                    .block(&block_id)
                    .context("streamed tool block is missing")?;
                let layout = previous.metadata.layout.clone().map(|mut layout| {
                    layout.marker = None;
                    layout
                });
                let mut content_width = width.clone();
                content_width.columns = content_width
                    .columns
                    .saturating_sub(
                        previous
                            .metadata
                            .layout
                            .as_ref()
                            .map_or(0, |layout| layout.indent.saturating_sub(2)),
                    )
                    .max(1);
                let renderer = super::transcript::TranscriptRenderer::new(&content_width)?;
                let owner = if open.agent_scope.is_some() {
                    exchange_id
                } else {
                    entry_id
                };
                let chunks = if expanded {
                    source.inline_prefix_chunks(limit)?
                } else {
                    1
                };
                if expanded && let Some(node) = &previous.metadata.node {
                    let more = chunks < source.inline_chunks()?;
                    if node.more != more {
                        let mut heading = previous.clone();
                        heading.metadata.node.as_mut().expect("tool node").more = more;
                        changed.push(TranscriptChange::Block { block: heading });
                    }
                }
                let identity = |chunk| {
                    forge_buffer::identity::BlockId(if chunk == 0 {
                        format!("{call_id}:preview")
                    } else {
                        format!("{call_id}:output:{chunk}")
                    })
                };
                let mut block = Vec::new();
                for chunk in first_chunk..chunks {
                    let mut replacement = renderer.tool_body(call_id, source, expanded, chunk)?;
                    replacement.metadata.layout = layout.clone();
                    if let Some(layout) = &mut replacement.metadata.layout {
                        layout.source_indent = 0;
                    }
                    super::layout::materialize(&mut replacement)?;
                    let previous = open.transcript.document.block(&replacement.id);
                    let new_target =
                        previous.is_none_or(|previous| previous.metadata.target.is_empty());
                    if let Some(previous) = previous {
                        for target in &previous.metadata.target {
                            open.action.remove(&target.id);
                        }
                    }
                    for target in &replacement.metadata.target {
                        open.action.insert(
                            target.id.clone(),
                            TranscriptAction::Tool {
                                call_id: call_id.clone(),
                            },
                        );
                        if let Some(entry) = open.entry.get_mut(owner) {
                            if new_target {
                                entry.target.push(target.id.clone());
                            }
                        }
                    }
                    block.push(replacement);
                }
                let hidden_id = forge_buffer::identity::BlockId(format!("{call_id}:hidden"));
                let mut hidden = renderer.tool_hidden(call_id, source, expanded)?;
                hidden.metadata.layout = layout;
                if let Some(layout) = &mut hidden.metadata.layout {
                    layout.source_indent = 0;
                }
                super::layout::materialize(&mut hidden)?;
                let previous_hidden = open
                    .transcript
                    .document
                    .block(&hidden_id)
                    .context("tool hidden counter is missing")?;
                let new_hidden_target = previous_hidden.metadata.target.is_empty();
                for target in &previous_hidden.metadata.target {
                    open.action.remove(&target.id);
                }
                for target in &hidden.metadata.target {
                    open.action.insert(
                        target.id.clone(),
                        TranscriptAction::Tool {
                            call_id: call_id.clone(),
                        },
                    );
                    if let Some(entry) = open.entry.get_mut(owner) {
                        if new_hidden_target {
                            entry.target.push(target.id.clone());
                        }
                    }
                }
                if first_chunk < chunks {
                    changed.push(TranscriptChange::ToolBody {
                        owner: owner.clone(),
                        anchor: if first_chunk < old_chunks {
                            identity(first_chunk)
                        } else {
                            hidden_id
                        },
                        removed: old_chunks.saturating_sub(first_chunk),
                        block,
                    });
                }
                changed.push(TranscriptChange::Block { block: hidden });
            }
            TimelineOperation::Insert { index, entry }
            | TimelineOperation::Replace { index, entry } => {
                let mut entry = project(
                    ProjectionSource::Entry(entry),
                    width,
                    *index > 0,
                    &open.sections.nodes.choice,
                    &open.sections.nodes.tool_limit,
                )?;
                if !unsettled.contains(&entry.entry.id) {
                    settle_responses(&mut entry);
                }
                let info = entry_presentation(&entry);
                projected.push((
                    entry.entry.id.clone(),
                    info,
                    entry.action,
                    entry.tool,
                    entry.syntax,
                ));
                changed.push(if matches!(operation, TimelineOperation::Insert { .. }) {
                    TranscriptChange::Insert {
                        index: *index,
                        entry: entry.entry,
                    }
                } else {
                    TranscriptChange::Replace {
                        index: *index,
                        entry: entry.entry,
                    }
                });
            }
            TimelineOperation::Remove { index, id } => {
                changed.push(TranscriptChange::Remove {
                    index: *index,
                    id: id.clone(),
                });
            }
        }
    }
    let patches = open.transcript.apply_event(
        &patch.session_id,
        patch.base_revision,
        patch.revision,
        changed,
    )?;
    let mut retained_tool = HashMap::new();
    for operation in operations {
        let id = match operation {
            TimelineOperation::ToolOutput { .. } | TimelineOperation::Message { .. } => continue,
            TimelineOperation::Insert { entry, .. } | TimelineOperation::Replace { entry, .. } => {
                entry.id()
            }
            TimelineOperation::Remove { id, .. } => {
                open.sections.retire_entry(id);
                id.clone()
            }
        };
        if let Some(previous) = open.entry.remove(&id) {
            for target in previous.target {
                open.syntax_done.remove(&target);
                open.action.remove(&target);
            }
            for call in previous.tool {
                if let Some(tool) = open.sections.nodes.tool.remove(&call) {
                    retained_tool.insert(call, tool);
                }
            }
            for target in previous.syntax {
                open.syntax_done.remove(&target);
                open.syntax.remove(&target);
            }
        }
    }
    for (id, entry, action, mut tool, syntax) in projected {
        for (call, output) in &mut tool {
            output.own(&id);
            if let Some(mut previous) = retained_tool.remove(call) {
                if previous.retain(output) {
                    *output = previous;
                }
            }
        }
        open.entry.insert(id, entry);
        open.action.extend(action);
        open.sections.nodes.tool.extend(tool);
        open.syntax.extend(syntax);
    }
    retain_patches(open, patches)
}

fn update_staging_block(open: &mut OpenPresentation) -> Result<()> {
    use forge_buffer::identity::BlockId;

    let block_id = BlockId("harness:staging".into());
    let mut patch = None;
    let entry_ids = open.sections.document.entry_ids();
    let existing = entry_ids.iter().position(|id| id == "harness:staging");
    if let Some(staging) = &open.staging {
        let width = open.transcript.views.profile().unwrap_or(&open.last_width);
        let mut rows = Vec::new();
        append_staging_section(&mut rows, "Steering active turn", &staging.steering, width)?;
        append_staging_section(&mut rows, "Queued follow-up inputs", &staging.queue, width)?;
        let mut block = forge_buffer::block::BufferBlock {
            id: block_id.clone(),
            text: forge_buffer::text::BufferText::from_rows(rows)?,
            metadata: Default::default(),
        };
        block
            .metadata
            .decoration
            .push(forge_buffer::block::Decoration {
                range: forge_buffer::block::TextRange {
                    start: forge_buffer::block::TextPosition { row: 0, column: 0 },
                    end: forge_buffer::block::TextPosition {
                        row: block.text.row_count(),
                        column: 0,
                    },
                },
                capture: "ForgeStatusHint".into(),
                priority: 100,
            });
        let entry = TranscriptEntry {
            id: "harness:staging".into(),
            block: vec![block],
        };
        patch = if let Some(index) = existing {
            open.sections
                .document
                .tool_layout(vec![TranscriptChange::Replace { index, entry }])?
                .into_iter()
                .last()
        } else {
            open.sections
                .document
                .tool_layout(vec![TranscriptChange::Insert {
                    index: entry_ids.len(),
                    entry,
                }])?
                .into_iter()
                .last()
        };
    } else if let Some(index) = existing {
        patch = open
            .sections
            .document
            .tool_layout(vec![TranscriptChange::Remove {
                index,
                id: "harness:staging".into(),
            }])?
            .into_iter()
            .last();
    }
    if let Some(patch) = patch {
        retain_patch(open, patch)?;
    }
    Ok(())
}

fn append_staging_section(
    rows: &mut Vec<String>,
    label: &str,
    values: &[String],
    width: &WidthProfile,
) -> Result<()> {
    if values.is_empty() {
        return Ok(());
    }
    rows.push(String::new());
    rows.extend(width.wrap_plain(label, 0)?);
    for value in values {
        rows.extend(width.wrap_plain(
            &format!("• {value}"),
            2.min(width.columns.saturating_sub(1)),
        )?);
    }
    Ok(())
}

fn retain_patch(open: &mut OpenPresentation, patch: BufferPatch) -> Result<()> {
    let bytes = crate::limits::serialized_size(&patch)?;
    while open.pending.len() >= 64 || open.pending_bytes + bytes > MAX_PENDING_BYTES {
        let Some((_, removed)) = open.pending.pop_front() else {
            break;
        };
        open.pending_bytes -= removed;
    }
    if bytes <= MAX_PENDING_BYTES {
        open.pending_bytes += bytes;
        open.pending.push_back((patch, bytes));
    } else {
        open.pending.clear();
        open.pending_bytes = 0;
    }
    Ok(())
}

fn retain_patches(open: &mut OpenPresentation, _source_patches: Vec<BufferPatch>) -> Result<()> {
    if let Some(failure) = &open.failure {
        anyhow::bail!("transcript projection requires reopening: {failure}");
    }
    let patches = match open.sections.refresh(&mut open.transcript) {
        Ok(patches) => patches,
        Err(failure) => {
            open.failure = Some(format!("loaded section projection: {failure:#}"));
            return Err(failure.context("loaded section projection"));
        }
    };
    for patch in patches {
        retain_patch(open, patch)?;
    }
    update_staging_block(open)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn long_messages_keep_two_previous_responses_and_reopen_after_resize() -> anyhow::Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
        use forge_buffer::node::{NodeDisplay, NodeLifecycle};
        let mut entry = interaction_entry("first");
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        exchange.prompt = "first\nsecond\nthird\nfourth\nfifth".into();
        exchange.state = crate::exchange::ExchangeState::Running;
        exchange.completed_at_ms = None;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "reply".into(),
            }),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 2)?;
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text =
            Some("response one\nresponse two\nresponse three\nresponse four\nresponse five".into());
        event.data = serde_json::json!({"phase":"final_answer", "provider_message_id":"reply"});
        exchange.observe_turn(&event, 3)?;
        let mut owner = super::SessionPresentation::new("session".into());
        owner.initialize(vec![entry.clone()])?;
        let document = forge_buffer::identity::DocumentId("message-folds".into());
        let view = forge_buffer::identity::ViewId("view".into());
        owner.open(
            document.clone(),
            view.clone(),
            forge_buffer::width::WidthProfile::default(),
        )?;
        let response = owner.open.as_ref().unwrap().entry["first"].response[0].clone();
        let snapshot = owner.snapshot(&document)?;
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id.0 == "first:prompt")
                .unwrap()
                .text
                .row_count(),
            3
        );
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let expand =
            |owner: &mut super::SessionPresentation, id: &str, sequence| -> anyhow::Result<()> {
                let generation = owner
                    .open
                    .as_ref()
                    .unwrap()
                    .sections
                    .nodes
                    .generation(id)
                    .unwrap();
                owner.dispatch(super::PresentationRequest::Node {
                    request: NodeRequest {
                        document: document.clone(),
                        view: view.clone(),
                        sequence,
                        node: id.into(),
                        generation,
                        action: NodeAction::SetExpansion { expanded: true },
                        rows: 48,
                        width: Default::default(),
                    },
                })?;
                Ok(())
            };
        expand(&mut owner, "first:prompt", 10)?;
        expand(&mut owner, &response.0, 11)?;
        let mut second = interaction_entry("second");
        owner.reconcile(vec![entry.clone(), second.clone()])?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        owner.dispatch(super::PresentationRequest::Resize {
            document: document.clone(),
            view: view.clone(),
            width: forge_buffer::width::WidthProfile {
                columns: 70,
                ..Default::default()
            },
        })?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let without_response = vec![entry.clone(), second.clone(), interaction_entry("empty")];
        owner.reconcile(without_response)?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let TimelineEntry::Exchange { exchange, .. } = &mut second else {
            unreachable!()
        };
        exchange.state = crate::exchange::ExchangeState::Running;
        exchange.completed_at_ms = None;
        event.address.as_mut().unwrap().turn_id = "second-reply".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        event.kind = "turn_started".into();
        event.text = None;
        event.data = serde_json::Value::Null;
        exchange.observe_turn(&event, 4)?;
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("next one\nnext two\nnext three\nnext four\nnext five".into());
        event.data =
            serde_json::json!({"phase":"final_answer", "provider_message_id":"second-reply"});
        exchange.observe_turn(&event, 5)?;
        owner.reconcile(vec![entry.clone(), second.clone()])?;
        let second_response = owner.open.as_ref().unwrap().entry["second"].response[0].clone();
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let mut third = interaction_entry("third");
        owner.reconcile(vec![entry.clone(), second.clone(), third.clone()])?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let TimelineEntry::Exchange { exchange, .. } = &mut third else {
            unreachable!()
        };
        exchange.state = crate::exchange::ExchangeState::Running;
        exchange.completed_at_ms = None;
        event.address.as_mut().unwrap().turn_id = "third-reply".into();
        event.turn_boundary = Some(TurnBoundary::Started);
        event.kind = "turn_started".into();
        event.text = None;
        event.data = serde_json::Value::Null;
        exchange.observe_turn(&event, 6)?;
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("third one\nthird two\nthird three\nthird four\nthird five".into());
        event.data =
            serde_json::json!({"phase":"final_answer", "provider_message_id":"third-reply"});
        exchange.observe_turn(&event, 7)?;
        owner.reconcile(vec![entry.clone(), second.clone(), third.clone()])?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let third_response = owner.open.as_ref().unwrap().entry["third"].response[0].clone();
        let history = vec![entry.clone(), second, third, interaction_entry("fourth")];
        owner.reconcile(history.clone())?;
        let snapshot = owner.snapshot(&document)?;
        let collapsed = snapshot
            .block
            .iter()
            .find(|block| block.id == response)
            .unwrap();
        assert_eq!(collapsed.text.row_count(), 3);
        assert_eq!(
            collapsed.metadata.node.as_ref().unwrap().display,
            NodeDisplay::Preview
        );
        assert_eq!(
            collapsed.metadata.node.as_ref().unwrap().lifecycle,
            NodeLifecycle::Settled
        );
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id == second_response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id == third_response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        let mut reopened = super::SessionPresentation::new("session".into());
        reopened.initialize(history.clone())?;
        reopened.open(document.clone(), view.clone(), Default::default())?;
        let restored = reopened.snapshot(&document)?;
        assert_eq!(
            restored
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            3
        );
        assert_eq!(
            restored
                .block
                .iter()
                .find(|block| block.id == second_response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        assert_eq!(
            restored
                .block
                .iter()
                .find(|block| block.id == third_response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        assert_eq!(
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .document
                .block(&response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        expand(&mut owner, &response.0, 12)?;
        let mut later = history;
        later.push(interaction_entry("fifth"));
        owner.reconcile(later)?;
        owner.dispatch(super::PresentationRequest::Resize {
            document: document.clone(),
            view: view.clone(),
            width: forge_buffer::width::WidthProfile {
                columns: 30,
                ..Default::default()
            },
        })?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id == response)
                .unwrap()
                .text
                .row_count(),
            5
        );
        assert!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id.0 == "first:prompt")
                .unwrap()
                .text
                .row_count()
                > 3
        );
        Ok(())
    }

    use super::*;

    #[test]
    fn staging_uses_a_stable_physical_block_and_clears_without_entering_timeline() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::Idle,
        }])?;
        let document = DocumentId("staging".into());
        let opened = owner.open(
            document.clone(),
            ViewId("staging-view".into()),
            WidthProfile::default(),
        )?;
        let request: PresentationRequest = serde_json::from_value(serde_json::json!({
            "operation": "staging",
            "sequence": 1,
            "document": document,
            "queue": ["queued first", "queued second"],
            "steering": ["active direction"]
        }))?;
        assert_eq!(
            owner.dispatch(request)?,
            serde_json::json!({"updated":true})
        );
        let snapshot = owner.snapshot(&document)?;
        let staging_index = snapshot
            .block
            .iter()
            .position(|block| block.id.0 == "harness:staging")
            .unwrap();
        assert_eq!(snapshot.block[staging_index - 1].id.0, "status");
        let rows = snapshot.block[staging_index].text.wire_rows().join("\n");
        assert!(rows.contains("Steering active turn"));
        assert!(rows.contains("active direction"));
        assert!(rows.starts_with("\nSteering active turn\n"));
        assert!(rows.contains("\n\nQueued follow-up inputs\n"));
        assert!(rows.find("queued first").unwrap() < rows.find("queued second").unwrap());
        assert!(!owner.timeline.entry_list().iter().any(
            |entry| matches!(entry, TimelineEntry::Status { id, .. } if id == "harness:staging")
        ));

        owner.dispatch(PresentationRequest::Staging {
            document: document.clone(),
            sequence: 1,
            queue: vec!["late retry".into()],
            steering: Vec::new(),
        })?;
        assert_eq!(
            owner.snapshot(&document)?.block[staging_index]
                .text
                .wire_rows()
                .join("\n"),
            rows,
            "a repeated staging identity replaced newer presentation state"
        );

        owner.dispatch(PresentationRequest::Staging {
            sequence: owner.open.as_ref().unwrap().staging_sequence + 1,
            document: document.clone(),
            queue: vec!["queued only".into()],
            steering: Vec::new(),
        })?;
        assert_eq!(
            owner.snapshot(&document)?.block[staging_index]
                .text
                .wire_rows(),
            ["", "Queued follow-up inputs", "• queued only"],
            "the first queued input must be separated from the preceding status"
        );
        owner.dispatch(PresentationRequest::Staging {
            sequence: owner.open.as_ref().unwrap().staging_sequence + 1,
            document: document.clone(),
            queue: Vec::new(),
            steering: Vec::new(),
        })?;
        assert!(
            !owner
                .snapshot(&document)?
                .block
                .iter()
                .any(|block| block.id.0 == "harness:staging")
        );
        assert!(
            !owner
                .sync(&document, opened.transcript.revision)?
                .patch
                .is_empty()
        );
        Ok(())
    }

    #[test]
    fn staged_prompts_keep_hanging_indentation_after_resize() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(Vec::new())?;
        let document = DocumentId("aligned-staging".into());
        let view = ViewId("aligned-staging-view".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let prompt = "a Harness execution request is already running: no permits available\nwhen I entered this prompt it failed\n    never lose this indentation 界";
        owner.dispatch(PresentationRequest::Staging {
            sequence: owner.open.as_ref().unwrap().staging_sequence + 1,
            document: document.clone(),
            queue: vec![prompt.into()],
            steering: vec![
                "Keep steering text aligned when it wraps across several display rows 界".into(),
            ],
        })?;
        for columns in [32, 54] {
            let width = WidthProfile {
                columns,
                ..WidthProfile::default()
            };
            owner.dispatch(PresentationRequest::Resize {
                document: document.clone(),
                view: view.clone(),
                width: width.clone(),
            })?;
            let snapshot = owner.snapshot(&document)?;
            let rows = snapshot.block.last().unwrap().text.wire_rows();
            let mut in_prompt = false;
            for row in rows {
                assert!(width.cells(row, 0)? <= columns);
                if row.starts_with("• ") {
                    in_prompt = true;
                } else if row.is_empty() || row == "Queued follow-up inputs" {
                    in_prompt = false;
                } else if in_prompt {
                    assert!(row.starts_with("  "), "unaligned continuation: {row:?}");
                }
            }
            assert!(
                snapshot
                    .block
                    .last()
                    .unwrap()
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.starts_with("      never"))
            );
        }
        assert_eq!(
            owner.open.as_ref().unwrap().staging.as_ref().unwrap().queue,
            vec![prompt.to_owned()]
        );
        Ok(())
    }

    #[test]
    fn staging_preserves_complete_large_payloads_across_reflow() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("main")])?;
        let document = DocumentId("large-staging".into());
        let view = ViewId("large-staging-view".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let queue: Vec<_> = (0..1200)
            .map(|index| format!("queue-{index}-{}", "x".repeat(96)))
            .collect();
        let last = queue.last().unwrap().clone();
        let steering = "steering-λ".repeat(256);
        owner.dispatch(PresentationRequest::Staging {
            sequence: owner.open.as_ref().unwrap().staging_sequence + 1,
            document: document.clone(),
            queue,
            steering: vec![steering.clone()],
        })?;
        let rows = owner
            .snapshot(&document)?
            .block
            .iter()
            .find(|block| block.id.0 == "harness:staging")
            .unwrap()
            .text
            .wire_rows()
            .join("\n");
        assert!(rows.contains("queue-0-"));
        let unwrapped = rows.lines().map(str::trim_start).collect::<String>();
        assert!(unwrapped.contains(&last));
        assert!(unwrapped.find("queue-0-").unwrap() < unwrapped.find(&last).unwrap());
        assert!(unwrapped.contains(&steering));
        owner.reconcile(vec![
            interaction_entry("main"),
            interaction_entry("followup"),
            TimelineEntry::Status {
                checks: None,
                status_thoughts: true,
                agent_count: 0,
                id: "status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    duration_ms: 0,
                    started_at_ms: Some(0),
                    activity: crate::session::state_machine::WorkflowActivity::Working,
                    execution: None,
                    reasoning_summary: None,
                },
            },
        ])?;
        assert_eq!(
            owner.snapshot(&document)?.block.last().unwrap().id.0,
            "harness:staging"
        );
        owner.dispatch(PresentationRequest::Staging {
            sequence: owner.open.as_ref().unwrap().staging_sequence + 1,
            document: document.clone(),
            queue: vec![last.clone()],
            steering: vec![steering.clone()],
        })?;
        assert_eq!(
            owner.snapshot(&document)?.block.last().unwrap().id.0,
            "harness:staging"
        );
        owner.dispatch(PresentationRequest::Resize {
            document: document.clone(),
            view,
            width: WidthProfile {
                columns: 32,
                ..WidthProfile::default()
            },
        })?;
        let rows = owner
            .snapshot(&document)?
            .block
            .iter()
            .find(|block| block.id.0 == "harness:staging")
            .unwrap()
            .text
            .wire_rows()
            .join("\n");
        assert!(
            rows.lines()
                .map(str::trim_start)
                .collect::<String>()
                .contains(&last)
        );
        assert_eq!(
            owner
                .open
                .as_ref()
                .unwrap()
                .staging
                .as_ref()
                .unwrap()
                .queue
                .len(),
            1
        );
        Ok(())
    }

    #[test]
    fn expanded_empty_check_has_only_a_heading_marker() -> Result<()> {
        use forge_buffer::identity::BlockId;

        let directory = tempfile::tempdir()?;
        let output = directory.path().join("command.output");
        std::fs::write(&output, "")?;
        let run = serde_json::from_value(serde_json::json!({
            "id":"gate","owner":{"kind":"session","session_id":"session"},
            "revision":1,"completed_plan":null,"shell":"system","executable":null,
            "workspace":".","environment_digest":"environment","started_at_ms":1,"completed_at_ms":2,
            "state":"passed","progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},"commands":[{
                "id":"gate:0","command":"accepted command","state":"passed","started_at_ms":1,
                "completed_at_ms":2,"exit_code":0,"error":null,"stdout":output,"stderr":output,
                "output":output,"output_bytes":0,"preview":""
            }]
        }))?;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(crate::timeline::TimelineProjector::with_checks(
            Vec::new(),
            vec![run],
        )?)?;
        let document = DocumentId("empty-check-output".into());
        owner.open(
            document.clone(),
            ViewId("view".into()),
            WidthProfile::default(),
        )?;
        expand_sections(&mut owner, "view")?;
        owner
            .open
            .as_mut()
            .unwrap()
            .sections
            .nodes
            .choice
            .insert("gate:0:tool".into(), true);
        toggle_inline_output(
            owner.open.as_mut().unwrap(),
            "gate:0",
            true,
            super::super::tool::INITIAL_INLINE_BYTES,
        )?;
        expand_sections(&mut owner, "view")?;
        let open = owner.open.as_ref().unwrap();
        let heading = open
            .transcript
            .document
            .block(&BlockId("gate:0:tool".into()))
            .unwrap();
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
        for identity in ["gate:0:preview", "gate:0:hidden"] {
            let block = open
                .transcript
                .document
                .block(&BlockId(identity.into()))
                .unwrap();
            assert!(block.metadata.layout.as_ref().unwrap().marker.is_none());
        }
        let body = open
            .transcript
            .document
            .block(&BlockId("gate:0:preview".into()))
            .unwrap();
        assert!(
            body.text
                .wire_rows()
                .iter()
                .any(|row| row.trim_start() == "└  no output")
        );
        Ok(())
    }

    #[test]
    fn attached_check_output_is_lazy_paged_and_collapses_without_a_preview() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let output = directory.path().join("command.output");
        let text = "saved command output\n".repeat(15000);
        std::fs::write(&output, &text)?;
        let run: crate::checks::CheckRun = serde_json::from_value(serde_json::json!({
            "id":"gate","owner":{"kind":"exchange","session_id":"session","exchange_id":"implementation","node_count":0},"revision":1,"completed_plan":null,"shell":"system","executable":null,
            "workspace":".","environment_digest":"environment","started_at_ms":1,"completed_at_ms":2,
            "state":"passed","progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null,"workspace_digest":{},"commands":[{
                "id":"gate:0","command":"accepted command","state":"passed","started_at_ms":1,
                "completed_at_ms":2,"exit_code":0,"error":null,"stdout":output,"stderr":output,
                "output":output,"output_bytes":text.len(),"preview":"must not render"
            }]
        }))?;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(crate::timeline::TimelineProjector::with_checks(
            vec![interaction_entry("implementation")],
            vec![run],
        )?)?;
        let document = DocumentId("check-output".into());
        owner.open(
            document.clone(),
            ViewId("view".into()),
            WidthProfile::default(),
        )?;
        assert_eq!(
            owner.open.as_ref().unwrap().sections.nodes.tool["gate:0"]
                .snapshot()?
                .retained_bytes(),
            0
        );
        expand_sections(&mut owner, "view")?;
        let collapsed = owner.snapshot(&document)?;
        let checks_position = collapsed
            .block
            .iter()
            .position(|block| block.id.0 == "gate:checks")
            .unwrap();
        assert!(
            !collapsed
                .block
                .iter()
                .any(|block| block.id.0 == "gate:separator")
        );
        let checks = &collapsed.block[checks_position];
        assert_eq!(
            checks
                .metadata
                .node
                .as_ref()
                .unwrap()
                .parent
                .as_ref()
                .unwrap()
                .0,
            "implementation:exchange"
        );
        for identity in ["gate:declarations", "gate:0:tool"] {
            let child = collapsed
                .block
                .iter()
                .find(|block| block.id.0 == identity)
                .unwrap();
            assert_eq!(
                child
                    .metadata
                    .node
                    .as_ref()
                    .unwrap()
                    .parent
                    .as_ref()
                    .unwrap()
                    .0,
                if identity == "gate:0:tool" {
                    "gate:category:check"
                } else {
                    "gate:checks"
                }
            );
            assert_eq!(
                child.metadata.layout.as_ref().unwrap().indent,
                checks.metadata.layout.as_ref().unwrap().indent
                    + if identity == "gate:0:tool" { 4 } else { 2 }
            );
        }
        {
            let open = owner.open.as_mut().unwrap();
            open.sections.set(
                &mut open.transcript,
                ViewId("view".into()),
                99,
                "implementation:exchange",
                false,
                false,
                None,
            )?;
            retain_patches(open, Vec::new())?;
        }
        assert!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .all(|block| block.id.0 != "gate:0:tool"),
            "collapsing implementation activity must hide its owned check results"
        );
        assert!(!collapsed.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("saved command output") || row.contains("must not render"))
        }));
        {
            let open = owner.open.as_mut().unwrap();
            open.sections.set(
                &mut open.transcript,
                ViewId("view".into()),
                100,
                "implementation:exchange",
                true,
                false,
                None,
            )?;
            retain_patches(open, Vec::new())?;
        }
        for (sequence, action) in [
            (
                101,
                super::super::nodes::NodeAction::SetExpansion { expanded: true },
            ),
            (102, super::super::nodes::NodeAction::LoadMore),
            (
                103,
                super::super::nodes::NodeAction::SetExpansion { expanded: false },
            ),
        ] {
            let generation = owner
                .snapshot(&document)?
                .block
                .iter()
                .find_map(|block| {
                    block
                        .metadata
                        .node
                        .as_ref()
                        .filter(|node| node.id.0 == "gate:0:tool")
                        .map(|node| node.generation)
                })
                .unwrap();
            owner.dispatch(PresentationRequest::Node {
                request: super::super::nodes::NodeRequest {
                    document: document.clone(),
                    view: ViewId("view".into()),
                    sequence,
                    node: "gate:0:tool".into(),
                    generation,
                    action,
                    rows: 100,
                    width: WidthProfile::default(),
                },
            })?;
            let retained = owner.open.as_ref().unwrap().sections.nodes.tool["gate:0"]
                .snapshot()?
                .retained_bytes();
            assert!(
                retained > 0
                    && owner.open.as_ref().unwrap().sections.nodes.tool["gate:0"]
                        .has_unloaded_output(),
                "explicit opening must read a page, not the entire saved output"
            );
        }
        assert!(!owner.snapshot(&document)?.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("saved command output"))
        }));
        Ok(())
    }

    #[tokio::test]
    async fn large_history_retains_parsers_and_syntax_across_refreshes_without_an_aggregate_limit()
    -> Result<()> {
        use forge_diff::syntax::SyntaxEngine;
        use forge_diff::workers::{AnalysisPool, PoolLimits};
        use std::sync::Arc;

        let output = format!("{}\n", "x".repeat(511)).repeat(16_384);
        let mut entries = (0..9)
            .map(|index| {
                let mut entry = interaction_entry(&format!("large-{index}"));
                let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
                    unreachable!()
                };
                let mut value = serde_json::to_value(&*exchange)?;
                value["turn"][0]["tool"]["item"]["tool"]["output"] = serde_json::json!(output);
                *exchange = serde_json::from_value(value)?;
                Ok(entry)
            })
            .collect::<Result<Vec<TimelineEntry>>>()?;
        let TimelineEntry::Exchange { exchange, .. } = entries.last_mut().unwrap() else {
            unreachable!()
        };
        exchange.node_list.push(crate::exchange::ExchangeNode::ArtifactChange {
            change: crate::exchange::ArtifactChange {
                id: "saved-diff".into(),
                path: "test.mjs".into(),
                diff_text: "--- a/test.mjs\n+++ b/test.mjs\n@@ -1 +1 @@\n-const old = 1;\n+const next = 2;\n".into(),
                created_at_ms: 1,
                declaration: None,
            },
        });
        assert!(output.len() * entries.len() > 64 * 1024 * 1024);
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(entries.clone())?;
        let document = DocumentId("large-history".into());
        let view = ViewId("large-history-view".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let snapshots = owner
            .open
            .as_ref()
            .unwrap()
            .sections
            .nodes
            .tool
            .values()
            .map(|tool| tool.snapshot())
            .collect::<Result<Vec<_>>>()?;
        assert!(
            snapshots
                .iter()
                .map(|output| output.retained_bytes())
                .sum::<usize>()
                > 128 * 1024 * 1024
        );

        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
        })));
        let job = owner.capture_syntax(&document)?.expect("saved diff syntax");
        owner.apply_syntax(&document, &job, job.analyze(&engine).await?)?;
        for iteration in 0..3 {
            let TimelineEntry::Exchange { exchange, .. } = entries.last_mut().unwrap() else {
                unreachable!()
            };
            exchange.prompt = format!("Updated prompt {iteration}");
            owner.reconcile(entries.clone())?;
            refresh_activity(owner.open.as_mut().unwrap(), &entries)?;
            owner.dispatch(PresentationRequest::Resize {
                document: document.clone(),
                view: view.clone(),
                width: WidthProfile {
                    columns: 70 + iteration,
                    ..WidthProfile::default()
                },
            })?;
            let snapshot = owner.snapshot(&document)?;
            assert!(
                snapshot
                    .block
                    .iter()
                    .map(|block| block.text.byte_count())
                    .sum::<usize>()
                    < 1024 * 1024,
                "collapsed history materialized tool output"
            );
            owner.sync(&document, snapshot.revision)?;
        }
        Ok(())
    }

    fn expand_sections(owner: &mut SessionPresentation, view: &str) -> Result<()> {
        let open = owner.open.as_mut().unwrap();
        let ids: Vec<_> = open
            .transcript
            .snapshot()?
            .block
            .iter()
            .filter(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .is_none_or(|node| node.kind != forge_buffer::node::NodeKind::Tool)
            })
            .flat_map(|block| block.metadata.fold.iter().map(|fold| fold.id.0.clone()))
            .collect();
        for (index, id) in ids.iter().enumerate() {
            open.sections.set(
                &mut open.transcript,
                ViewId(view.into()),
                index as u64 + 1,
                id,
                true,
                false,
                None,
            )?;
            open.sections.quota_for_test(id, 16 * 1024 * 1024);
        }
        retain_patches(open, Vec::new())
    }

    #[test]
    fn expanded_streaming_retains_settled_chunks_and_patches_only_the_mutable_tail() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        use forge_buffer::identity::BlockId;
        let output = (0..30000)
            .map(|row| format!("row {row} λ\n"))
            .collect::<String>();
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["output"] = serde_json::json!(output);
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["status"] =
            serde_json::json!("inProgress");
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] =
            serde_json::Value::Null;
        let mut exchange: Exchange = serde_json::from_value(active["exchange"].take())?;
        let entry = TimelineEntry::Exchange {
            id: "active".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry])?;
        let document = DocumentId("expanded-stream".into());
        owner.open(
            document.clone(),
            ViewId("view".into()),
            WidthProfile::default(),
        )?;
        owner
            .open
            .as_mut()
            .unwrap()
            .sections
            .nodes
            .choice
            .insert("active:turn:1:tool:tool".into(), true);
        toggle_inline_output(
            owner.open.as_mut().unwrap(),
            "active:turn:1:tool",
            true,
            16 * 1024 * 1024,
        )?;
        expand_sections(&mut owner, "view")?;
        {
            let open = owner.open.as_mut().unwrap();
            open.sections
                .quota_for_test("active:turn:1:tool:tool", 16 * 1024 * 1024);
            open.transcript.dirty.insert("active".into());
            retain_patches(open, Vec::new())?;
        }
        let opened = owner.snapshot(&document)?;
        assert!(
            opened
                .block
                .iter()
                .map(|block| block.text.row_count())
                .sum::<usize>()
                >= 30000
        );
        let heading = BlockId("active:turn:1:tool:tool".into());
        let settled = BlockId("active:turn:1:tool:preview".into());
        let pointer = |owner: &SessionPresentation, id: &BlockId| {
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .document
                .block(id)
                .unwrap()
                .text
                .row(0)
                .unwrap()
                .as_ptr() as usize
        };
        let heading_pointer = pointer(&owner, &heading);
        let settled_pointer = pointer(&owner, &settled);
        let mut revision = opened.revision;
        let projected_entries = owner.open.as_ref().unwrap().sections.projected_entries;
        for iteration in 0..100 {
            let event = BackendEvent {
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "active".into(),
                }),
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: serde_json::json!({"emittedAtMs":10+iteration}),
                summary: None,
                task_update: None,
                activity: Some(ToolActivity {
                    id: "tool".into(),
                    kind: ToolActivityKind::Command,
                    title: "Read output".into(),
                    output: Some(format!("new {iteration} 🦀\n")),
                    status: Some("inProgress".into()),
                    output_delta: true,
                    change: Default::default(),
                }),
            };
            exchange.observe_turn(&event, 10 + iteration)?;
            owner.append_tool_output(&exchange, &event)?;
            let sync = owner.sync(&document, revision)?;
            assert!(sync.snapshot.is_none());
            for patch in sync.patch {
                assert!(patch.metadata_edit.iter().all(|edit| edit.block != settled));
                assert!(
                    patch
                        .text_edit
                        .iter()
                        .map(|edit| edit.text.byte_count())
                        .sum::<usize>()
                        < 20000
                );
                revision = patch.next;
            }
            assert_eq!(pointer(&owner, &heading), heading_pointer);
            assert_eq!(pointer(&owner, &settled), settled_pointer);
            assert_eq!(
                owner.open.as_ref().unwrap().sections.projected_entries,
                projected_entries,
                "a streamed tail rebuilt the containing exchange"
            );
        }
        assert!(owner.snapshot(&document)?.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("new 99 🦀"))
        }));
        refresh_activity(
            owner.open.as_mut().unwrap(),
            &[TimelineEntry::Exchange {
                id: "active".into(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            }],
        )?;
        let timed_heading = owner
            .open
            .as_ref()
            .unwrap()
            .transcript
            .document
            .block(&heading)
            .unwrap()
            .text
            .row(0)
            .unwrap()
            .to_owned();
        assert_eq!(pointer(&owner, &settled), settled_pointer);
        toggle_inline_output(
            owner.open.as_mut().unwrap(),
            "active:turn:1:tool",
            false,
            super::super::tool::INITIAL_INLINE_BYTES,
        )?;
        let collapsed_heading = owner
            .open
            .as_ref()
            .unwrap()
            .transcript
            .document
            .block(&heading)
            .unwrap()
            .text
            .row(0)
            .unwrap();
        let column = |text: &str| {
            WidthProfile::default()
                .cells(&text[..text.find("Read output").unwrap()], 0)
                .unwrap()
        };
        assert_eq!(
            column(&timed_heading),
            column(collapsed_heading),
            "timer updates and activation must keep the command column fixed"
        );
        Ok(())
    }

    #[test]
    fn streamed_message_reparses_only_its_owned_block_and_retains_tools() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        let mut exchange: Exchange = serde_json::from_value(active["exchange"].take())?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "active".into(),
            }),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("First [link](https://example.com)".into()),
            data: serde_json::json!({"provider_message_id":"message"}),
            summary: None,
            task_update: None,
            activity: None,
        };
        exchange.observe_turn(&event, 2)?;
        let entry = TimelineEntry::Exchange {
            id: "active".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("settled"), entry])?;
        let document = DocumentId("streamed-message".into());
        let opened = owner.open(
            document.clone(),
            ViewId("view".into()),
            WidthProfile::default(),
        )?;
        let projected_entries = owner.open.as_ref().unwrap().sections.projected_entries;
        let tool = owner.open.as_ref().unwrap().sections.nodes.tool["active:turn:1:tool"]
            .preview(true)?
            .row
            .into_iter()
            .map(str::to_owned)
            .collect::<Vec<_>>();
        event.text = Some("\n\nMore **text** with λ.".into());
        exchange.observe_turn(&event, 3)?;
        let patch = owner
            .update_message(&exchange, &event)?
            .expect("owned message");
        assert!(matches!(
            &patch.operation[..],
            [TimelineOperation::Message { index: 1, .. }]
        ));
        let sync = owner.sync(&document, opened.transcript.revision)?;
        assert!(sync.snapshot.is_none() && !sync.patch.is_empty());
        assert_eq!(
            owner.open.as_ref().unwrap().sections.projected_entries,
            projected_entries
        );
        assert!(sync.patch.iter().all(|patch| {
            patch.block_edit.is_empty()
                && patch.metadata_edit.iter().all(|edit| {
                    !edit.block.0.contains("settled") && !edit.block.0.ends_with(":tool")
                })
        }));
        assert_eq!(
            owner.open.as_ref().unwrap().sections.nodes.tool["active:turn:1:tool"]
                .preview(true)?
                .row,
            tool
        );
        assert!(owner.snapshot(&document)?.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("More **text**"))
        }));
        Ok(())
    }

    #[test]
    fn streamed_thought_retains_owned_tools_and_expansion() -> Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        use forge_buffer::node::NodeKind;
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        let mut exchange: Exchange = serde_json::from_value(active["exchange"].take())?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "active".into(),
            }),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("First [link](https://example.com)".into()),
            data: serde_json::json!({"provider_message_id":"message","phase":"commentary"}),
            summary: None,
            task_update: None,
            activity: None,
        };
        exchange.observe_turn(&event, 2)?;
        let thought_id = exchange.node_list.last().unwrap().id().to_owned();
        let mut tool_event = event.clone();
        tool_event.kind = "tool".into();
        tool_event.text = None;
        tool_event.data = serde_json::Value::Null;
        tool_event.activity = Some(ToolActivity {
            id: "owned-tool".into(),
            kind: ToolActivityKind::Command,
            title: "Read owned output".into(),
            output: Some("Thought-owned output".into()),
            output_delta: false,
            status: Some("running".into()),
            change: Default::default(),
        });
        exchange.observe_turn(&tool_event, 2)?;
        let entry = TimelineEntry::Exchange {
            id: "active".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("settled"), entry])?;
        let document = DocumentId("streamed-message".into());
        owner.open(
            document.clone(),
            ViewId("view".into()),
            WidthProfile::default(),
        )?;
        let snapshot = owner.snapshot(&document)?;
        let thought = snapshot
            .block
            .iter()
            .find(|block| block.id.0 == thought_id)
            .unwrap();
        let node = thought.metadata.node.as_ref().unwrap();
        assert_eq!(node.kind, NodeKind::Thought);
        owner.dispatch(PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: ViewId("view".into()),
                sequence: 1,
                node: node.id.0.clone(),
                generation: node.generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 40,
                width: WidthProfile::default(),
            },
        })?;
        let snapshot = owner.snapshot(&document)?;
        let thought_fold = snapshot
            .block
            .iter()
            .find(|block| block.id.0 == thought_id)
            .unwrap()
            .metadata
            .fold
            .clone();
        assert!(!thought_fold.is_empty());
        let revision = snapshot.revision;
        let projected_entries = owner.open.as_ref().unwrap().sections.projected_entries;
        let tool = owner.open.as_ref().unwrap().sections.nodes.tool["active:turn:1:tool"]
            .preview(true)?
            .row
            .into_iter()
            .map(str::to_owned)
            .collect::<Vec<_>>();
        event.text = Some("\n\nMore **text** with λ.".into());
        exchange.observe_turn(&event, 3)?;
        let patch = owner
            .update_message(&exchange, &event)?
            .expect("owned message");
        assert!(matches!(
            &patch.operation[..],
            [TimelineOperation::Message { index: 1, .. }]
        ));
        let sync = owner.sync(&document, revision)?;
        assert!(sync.snapshot.is_none() && !sync.patch.is_empty());
        assert_eq!(
            owner.open.as_ref().unwrap().sections.projected_entries,
            projected_entries
        );
        assert!(sync.patch.iter().all(|patch| {
            patch.block_edit.is_empty()
                && patch.metadata_edit.iter().all(|edit| {
                    !edit.block.0.contains("settled") && !edit.block.0.ends_with(":tool")
                })
        }));
        assert_eq!(
            owner.open.as_ref().unwrap().sections.nodes.tool["active:turn:1:tool"]
                .preview(true)?
                .row,
            tool
        );
        assert!(owner.snapshot(&document)?.block.iter().any(|block| {
            block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("More **text**"))
        }));
        let snapshot = owner.snapshot(&document)?;
        let thought = snapshot
            .block
            .iter()
            .find(|block| block.id.0 == thought_id)
            .unwrap();
        assert_eq!(thought.metadata.fold, thought_fold);
        let group = snapshot
            .block
            .iter()
            .find(|block| block.id.0.contains("owned-tool") && block.id.0.ends_with(":tools"))
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
        tool_event.activity.as_mut().unwrap().status = Some("completed".into());
        exchange.observe_turn(&tool_event, 4)?;
        owner.reconcile(vec![
            interaction_entry("settled"),
            TimelineEntry::Exchange {
                id: "active".into(),
                created_at_ms: 0,
                exchange,
                agent_by_id: HashMap::new(),
            },
        ])?;
        let snapshot = owner.snapshot(&document)?;
        let thought = snapshot
            .block
            .iter()
            .find(|block| block.id.0 == thought_id)
            .unwrap();
        assert_eq!(
            thought.metadata.node.as_ref().unwrap().kind,
            NodeKind::Thought
        );
        assert_eq!(
            thought.metadata.node.as_ref().unwrap().display,
            forge_buffer::node::NodeDisplay::Full
        );
        Ok(())
    }

    #[test]
    fn streamed_thoughts_appear_only_in_status_when_enabled() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress};
        for status_thoughts in [false, true] {
            let mut active = serde_json::to_value(interaction_entry("active"))?;
            active["exchange"]["state"] = serde_json::json!("running");
            active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
            active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
            active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
            let mut exchange: Exchange = serde_json::from_value(active["exchange"].take())?;
            let mut event = BackendEvent {
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "active".into(),
                }),
                turn_boundary: None,
                kind: "reasoning_summary".into(),
                text: Some("Inspect settings.".into()),
                data: serde_json::json!({"provider_message_id":"thought"}),
                summary: None,
                task_update: None,
                activity: None,
            };
            exchange.observe_turn(&event, 2)?;
            let mut owner = SessionPresentation::new("session".into());
            owner.initialize(vec![
                TimelineEntry::Exchange {
                    id: "active".into(),
                    created_at_ms: 0,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                },
                TimelineEntry::Status {
                    checks: None,
                    id: "session:status".into(),
                    created_at_ms: 0,
                    agent_count: 0,
                    status_thoughts,
                    status: SessionPhase::Working {
                        duration_ms: 0,
                        started_at_ms: Some(1),
                        activity: crate::session::state_machine::WorkflowActivity::Working,
                        execution: None,
                        reasoning_summary: exchange.latest_reasoning_summary().map(str::to_owned),
                    },
                },
            ])?;
            let document = DocumentId("streamed-thought".into());
            owner.open(
                document.clone(),
                ViewId("view".into()),
                WidthProfile::default(),
            )?;
            event.text = Some(" Next step.".into());
            exchange.observe_turn(&event, 3)?;
            let patch = owner
                .update_message(&exchange, &event)?
                .expect("owned thought");
            assert!(matches!(
                patch.operation.first(),
                Some(TimelineOperation::Message { .. })
            ));
            assert_eq!(patch.operation.len(), if status_thoughts { 2 } else { 1 });
            let snapshot = owner.snapshot(&document)?;
            assert!(
                !snapshot
                    .block
                    .iter()
                    .filter(|block| block.id.0 != "session:status")
                    .any(|block| block
                        .text
                        .wire_rows()
                        .iter()
                        .any(|row| row.contains("Next step.")))
            );
            let status = snapshot
                .block
                .iter()
                .find(|block| block.id.0 == "session:status")
                .unwrap();
            assert_eq!(
                status
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("Next step.")),
                status_thoughts
            );
            let toggle = owner.set_status_thoughts(!status_thoughts)?;
            assert!(matches!(
                toggle.operation.as_slice(),
                [TimelineOperation::Replace {
                    entry: TimelineEntry::Status { .. },
                    ..
                }]
            ));
            let toggled = owner.snapshot(&document)?;
            let status = toggled
                .block
                .iter()
                .find(|block| block.id.0 == "session:status")
                .unwrap();
            assert_eq!(
                status
                    .text
                    .wire_rows()
                    .iter()
                    .any(|row| row.contains("Next step.")),
                !status_thoughts
            );
            assert!(
                !toggled
                    .block
                    .iter()
                    .filter(|block| block.id.0 != "session:status")
                    .any(|block| block
                        .text
                        .wire_rows()
                        .iter()
                        .any(|row| row.contains("Next step.")))
            );
            let mut stale = owner.timeline.entry_list().to_vec();
            for entry in &mut stale {
                if let TimelineEntry::Status {
                    status_thoughts: preference,
                    ..
                } = entry
                {
                    *preference = status_thoughts;
                }
            }
            assert!(
                owner
                    .reconcile_with_status_thoughts(stale, !status_thoughts)?
                    .is_empty()
            );
        }
        Ok(())
    }

    #[test]
    fn streamed_output_changes_only_the_tool_and_its_fold_endpoints() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["status"] =
            serde_json::json!("inProgress");
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] =
            serde_json::Value::Null;
        let active = TimelineEntry::Exchange {
            id: "active".into(),
            created_at_ms: 0,
            exchange: serde_json::from_value(active["exchange"].take())?,
            agent_by_id: HashMap::new(),
        };
        let TimelineEntry::Exchange { mut exchange, .. } = active.clone() else {
            unreachable!()
        };
        let mut entries: Vec<_> = (0..3000)
            .map(|index| interaction_entry(&format!("history-{index}")))
            .collect();
        entries.push(active);
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(entries)?;
        let document = DocumentId("streamed-output".into());
        let opened = owner.open(
            document.clone(),
            ViewId("view".into()),
            WidthProfile::default(),
        )?;
        assert!(
            opened
                .transcript
                .block
                .iter()
                .map(|block| block.text.row_count())
                .sum::<usize>()
                < 30000
        );
        assert!(
            !opened
                .transcript
                .block
                .iter()
                .any(|block| block.id.0 == "history-0:turn:1:tool:preview")
        );
        let event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "active".into(),
            }),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::json!({"emittedAtMs":10}),
            summary: None,
            task_update: None,
            activity: Some(ToolActivity {
                id: "tool".into(),
                kind: ToolActivityKind::Command,
                title: "Read output".into(),
                output: Some("\nseventh\n".into()),
                status: Some("inProgress".into()),
                output_delta: true,
                change: Default::default(),
            }),
        };
        exchange.observe_turn(&event, 10)?;
        let patch = owner
            .append_tool_output(&exchange, &event)?
            .expect("incremental output");
        assert!(matches!(
            &patch.operation[..],
            [TimelineOperation::ToolOutput { index: 3000, .. }]
        ));
        let sync = owner.sync(&document, opened.transcript.revision)?;
        assert!(sync.snapshot.is_none());
        assert!(!sync.patch.is_empty());
        for patch in &sync.patch {
            assert!(patch.block_edit.is_empty());
            assert!(
                patch
                    .metadata_edit
                    .iter()
                    .all(|metadata| !metadata.block.0.contains("history-"))
            );
            assert!(
                patch.text_edit.iter().all(|edit| edit.removed_rows <= 4),
                "stream updates replace only the four-row preview or its count"
            );
        }
        assert!(
            owner.open.as_ref().unwrap().sections.nodes.tool["active:turn:1:tool"]
                .preview(true)?
                .row
                .contains(&"seventh")
        );
        let snapshot = owner.snapshot(&document)?;
        let preview = snapshot
            .block
            .iter()
            .find(|block| block.id.0 == "active:turn:1:tool:preview")
            .unwrap();
        assert!(
            preview
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("seventh"))
        );
        assert_eq!(preview.text.row_count(), 4);
        assert_eq!(
            snapshot
                .block
                .iter()
                .find(|block| block.id.0 == "active:turn:1:tool:hidden")
                .unwrap()
                .text
                .row_count(),
            0
        );
        Ok(())
    }

    #[test]
    fn consecutive_file_writes_update_one_expanded_entry_across_turns_and_reload() -> Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use crate::backend::{
            BackendEvent, ProviderAddress, ProviderChangeKind, ProviderChangeSet,
            ProviderFileChange, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        use forge_buffer::node::{NodeDisplay, NodeKind};
        use serde_json::{Value, json};

        let mut exchange: Exchange = serde_json::from_value(json!({
            "id":"writes", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Write files", "kind":"chat", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let entry = |exchange: &Exchange| TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry(&exchange)])?;
        let document = DocumentId("consecutive-writes".into());
        let view = ViewId("write-view".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let mut group_id = None;
        let mut group_position = None;
        let mut first_file_id = None;
        let mut thought_id = None;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: Value::Null,
            summary: None,
            task_update: None,
            activity: None,
        };
        for (batch, (range, count)) in [(0..2, 2), (2..5, 5), (5..6, 6), (0..1, 6)]
            .into_iter()
            .enumerate()
        {
            let address = ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: format!("turn-{batch}"),
            };
            exchange.start_turn(address.clone(), batch as i64 * 10)?;
            event.address = Some(address);
            event.turn_boundary = None;
            if batch == 0 {
                event.kind = "assistant_message".into();
                event.text = Some("Write the requested files.\nPreserve their content.".into());
                event.data = json!({"params":{"itemId":"thought","phase":"commentary"}});
                exchange.observe_turn(&event, 0)?;
                owner.reconcile(vec![entry(&exchange)])?;
                let snapshot = owner.snapshot(&document)?;
                let thought = snapshot
                    .block
                    .iter()
                    .find(|block| {
                        block
                            .text
                            .wire_rows()
                            .join("")
                            .contains("Write the requested files.")
                    })
                    .unwrap();
                thought_id = Some(thought.id.clone());
                assert_eq!(
                    thought
                        .metadata
                        .layout
                        .as_ref()
                        .unwrap()
                        .marker
                        .as_ref()
                        .unwrap()
                        .text,
                    "○"
                );
                assert!(thought.metadata.fold.is_empty());
            }
            event.kind = "reasoning_summary".into();
            event.text = Some("Continue writing".into());
            event.data = json!({"provider_message_id":format!("reasoning-{batch}")});
            exchange.observe_turn(&event, batch as i64 * 10 + 1)?;
            event.kind = "tool".into();
            event.text = None;
            event.data = Value::Null;
            event.activity = Some(ToolActivity {
                id: format!("write-{batch}"),
                kind: ToolActivityKind::FileChange,
                title: "file changes".into(),
                output: None,
                output_delta: false,
                status: Some("running".into()),
                change: ProviderChangeSet {
                    file: range
                        .map(|index| ProviderFileChange {
                            path: format!("file-{index}.txt"),
                            move_path: None,
                            kind: if batch == 3 {
                                ProviderChangeKind::Update
                            } else {
                                ProviderChangeKind::Add
                            },
                            diff: if batch == 3 {
                                "@@ -1 +1 @@\n-initial\n+final".into()
                            } else {
                                "initial\n".into()
                            },
                        })
                        .collect(),
                },
            });
            exchange.observe_turn(&event, batch as i64 * 10 + 2)?;
            owner.reconcile(vec![entry(&exchange)])?;
            if batch > 0 {
                let snapshot = owner.snapshot(&document)?;
                assert_eq!(
                    snapshot
                        .block
                        .iter()
                        .filter(
                            |block| block.metadata.node.as_ref().is_some_and(|node| node.kind
                                == NodeKind::Changes
                                && node.parent.as_ref().is_some_and(|parent| thought_id
                                    .as_ref()
                                    .is_some_and(|thought| parent.0 == thought.0)))
                        )
                        .count(),
                    1
                );
                let group = snapshot
                    .block
                    .iter()
                    .find(|block| Some(&block.id) == group_id.as_ref())
                    .unwrap();
                assert!(group.text.wire_rows().join("").contains("changing"));
            }
            event.activity.as_mut().unwrap().status = Some("completed".into());
            exchange.observe_turn(&event, batch as i64 * 10 + 3)?;
            exchange.observe_turn(&event, batch as i64 * 10 + 3)?;
            if batch == 0 {
                exchange
                    .node_list
                    .push(crate::exchange::ExchangeNode::Compaction {
                        id: "compaction".into(),
                        compaction: crate::backend::Compaction {
                            id: "compaction".into(),
                            thread_id: "thread".into(),
                            state: crate::backend::CompactionState::Completed,
                            manual: false,
                            started_at_ms: Some(2),
                            completed_at_ms: Some(3),
                            result: Default::default(),
                        },
                    });
            }
            event.activity = Some(ToolActivity {
                id: format!("inspect-{batch}"),
                kind: ToolActivityKind::Command,
                title: "Inspect file".into(),
                output: Some("File inspected".into()),
                output_delta: false,
                status: Some("completed".into()),
                change: ProviderChangeSet::default(),
            });
            exchange.observe_turn(&event, batch as i64 * 10 + 4)?;
            event.activity = None;
            event.turn_boundary = Some(TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            exchange.observe_turn(&event, batch as i64 * 10 + 5)?;
            exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
            owner.reconcile(vec![entry(&exchange)])?;
            let snapshot = owner.snapshot(&document)?;
            let thought = snapshot
                .block
                .iter()
                .find(|block| Some(&block.id) == thought_id.as_ref())
                .unwrap();
            assert_eq!(
                thought
                    .text
                    .wire_rows()
                    .iter()
                    .map(|row| row.trim_start())
                    .collect::<Vec<_>>()
                    .join("\n"),
                "Write the requested files.\nPreserve their content."
            );
            let thought_node = thought.metadata.node.as_ref().unwrap();
            assert_eq!(
                thought_node.display,
                if batch == 0 {
                    NodeDisplay::Heading
                } else {
                    NodeDisplay::Full
                }
            );
            if batch == 0 {
                owner.dispatch(PresentationRequest::Node {
                    request: NodeRequest {
                        document: document.clone(),
                        view: view.clone(),
                        sequence: 1,
                        node: thought_node.id.0.clone(),
                        generation: thought_node.generation,
                        action: NodeAction::SetExpansion { expanded: true },
                        rows: 40,
                        width: WidthProfile::default(),
                    },
                })?;
            }
            let snapshot = owner.snapshot(&document)?;
            let groups: Vec<_> = snapshot
                .block
                .iter()
                .enumerate()
                .filter(|(_, block)| {
                    block.metadata.node.as_ref().is_some_and(|node| {
                        node.kind == NodeKind::Changes
                            && node.parent.as_ref().is_some_and(|parent| {
                                thought_id
                                    .as_ref()
                                    .is_some_and(|thought| parent.0 == thought.0)
                            })
                    })
                })
                .collect();
            assert_eq!(groups.len(), 1);
            let summary = snapshot
                .block
                .iter()
                .find(|block| block.id.0 == "writes:results:changes")
                .unwrap();
            assert!(summary.metadata.node.as_ref().unwrap().parent.is_none());
            assert!(
                summary
                    .text
                    .wire_rows()
                    .join("")
                    .contains(&format!("Changed {count} files"))
            );
            let (position, group) = groups[0];
            assert!(
                group
                    .text
                    .wire_rows()
                    .join("")
                    .contains(&format!("Changed {count} files +{count} -0"))
            );
            if let Some(identity) = &group_id {
                assert_eq!(&group.id, identity);
                assert_eq!(Some(position), group_position);
                assert_eq!(
                    group.metadata.node.as_ref().unwrap().display,
                    NodeDisplay::Full
                );
                assert!(
                    snapshot
                        .block
                        .iter()
                        .any(|block| Some(&block.id) == first_file_id.as_ref())
                );
            } else {
                group_id = Some(group.id.clone());
                group_position = Some(position);
                let node = group.metadata.node.as_ref().unwrap();
                owner.dispatch(PresentationRequest::Node {
                    request: NodeRequest {
                        document: document.clone(),
                        view: view.clone(),
                        sequence: 2,
                        node: node.id.0.clone(),
                        generation: node.generation,
                        action: NodeAction::SetExpansion { expanded: true },
                        rows: 40,
                        width: WidthProfile::default(),
                    },
                })?;
                first_file_id = owner
                    .snapshot(&document)?
                    .block
                    .iter()
                    .find(|block| {
                        block
                            .metadata
                            .node
                            .as_ref()
                            .is_some_and(|node| node.kind == NodeKind::File)
                    })
                    .map(|block| block.id.clone());
                assert!(first_file_id.is_some());
            }
        }
        let open = owner.open.as_ref().unwrap();
        let identity = &group_id.unwrap().0;
        let action = open
            .action
            .get(&forge_buffer::identity::TargetId(identity.clone()))
            .unwrap();
        let super::super::projection::TranscriptAction::Diff { text, .. } = action else {
            panic!("missing grouped diff")
        };
        assert!(text.contains("+final"));
        assert!(!text.contains("-initial"));
        assert_eq!(text.matches("diff --git").count(), 6);
        let snapshot = owner.snapshot(&document)?;
        let thought_position = snapshot
            .block
            .iter()
            .position(|block| Some(&block.id) == thought_id.as_ref())
            .unwrap();
        let event_position = snapshot
            .block
            .iter()
            .position(|block| block.id.0 == "compaction")
            .unwrap();
        let tools_position = snapshot
            .block
            .iter()
            .position(|block| block.id.0.ends_with(":tools"))
            .unwrap();
        let changes_position = snapshot
            .block
            .iter()
            .position(|block| &block.id.0 == identity)
            .unwrap();
        assert!(
            thought_position < event_position
                && event_position < tools_position
                && tools_position < changes_position
        );
        assert_eq!(
            snapshot.block[event_position]
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .indent,
            snapshot.block[thought_position]
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .indent
                + 2
        );
        let tools: Vec<_> = snapshot
            .block
            .iter()
            .filter(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == NodeKind::ToolGroup)
            })
            .collect();
        assert_eq!(tools.len(), 1);
        assert!(
            tools[0]
                .text
                .wire_rows()
                .join("")
                .contains("Running 4 tools")
        );
        Ok(())
    }

    #[test]
    fn hidden_parallel_tool_output_stays_cached_until_exchange_completion() -> Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        use serde_json::{Value, json};
        let mut value = serde_json::to_value(interaction_entry("parallel"))?;
        value["exchange"]["state"] = json!("running");
        value["exchange"]["completed_at_ms"] = Value::Null;
        value["exchange"]["execution_started_at_ms"] = json!(1);
        value["exchange"]["turn"][0]["state"] = json!({"kind":"running"});
        value["exchange"]["turn"][0]["tool"]["item"]["tool"]["status"] = json!("inProgress");
        value["exchange"]["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] = Value::Null;
        let mut exchange: Exchange = serde_json::from_value(value["exchange"].take())?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "parallel".into(),
            }),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: Value::Null,
            summary: None,
            task_update: None,
            activity: Some(ToolActivity {
                id: "latest".into(),
                kind: ToolActivityKind::Command,
                title: "Latest parallel tool".into(),
                output: Some("Latest output".into()),
                status: Some("inProgress".into()),
                output_delta: false,
                change: Default::default(),
            }),
        };
        exchange.observe_turn(&event, 2)?;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Exchange {
            id: "parallel".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        }])?;
        let document = DocumentId("hidden-parallel".into());
        let view = ViewId("hidden-parallel-view".into());
        let opened = owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let before = owner.snapshot(&document)?;
        assert!(
            !before
                .block
                .iter()
                .any(|block| block.id.0 == "parallel:turn:1:tool:tool")
        );
        assert!(
            before
                .block
                .iter()
                .any(|block| block.id.0 == "parallel:turn:1:latest:tool")
        );
        assert_eq!(
            before
                .block
                .iter()
                .find(|block| block.id.0 == "parallel:turn:1:latest:preview")
                .unwrap()
                .text
                .row_count(),
            4
        );
        let activity = event.activity.as_mut().unwrap();
        activity.id = "tool".into();
        activity.title = "Read output".into();
        activity.output = Some("\nLate older output".into());
        activity.output_delta = true;
        exchange.observe_turn(&event, 3)?;
        owner
            .append_tool_output(&exchange, &event)?
            .expect("hidden tool output delta");
        owner.sync(&document, opened.transcript.revision)?;
        let after = owner.snapshot(&document)?;
        assert_eq!(
            serde_json::to_value(&before.block)?,
            serde_json::to_value(&after.block)?,
            "an older tool's output must not move or replace the live preview"
        );
        let group = after
            .block
            .iter()
            .find_map(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .filter(|node| node.kind == forge_buffer::node::NodeKind::ToolGroup)
            })
            .unwrap();
        owner.dispatch(PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: view.clone(),
                sequence: 100,
                node: group.id.0.clone(),
                generation: group.generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 40,
                width: WidthProfile::default(),
            },
        })?;
        let expanded = owner.snapshot(&document)?;
        assert!(
            !expanded
                .block
                .iter()
                .any(|block| block.id.0 == "parallel:turn:1:tool:tool"),
            "expanding a live group must keep older tools hidden"
        );
        assert!(
            expanded
                .block
                .iter()
                .any(|block| block.id.0 == "parallel:turn:1:latest:tool")
        );
        let source = &owner.open.as_ref().unwrap().sections.nodes.tool["parallel:turn:1:tool"];
        assert!(source.preview(true)?.row.contains(&"Late older output"));
        event.activity = Some(ToolActivity {
            id: "third".into(),
            kind: ToolActivityKind::Command,
            title: "Third parallel tool".into(),
            output: Some("Third output".into()),
            status: Some("inProgress".into()),
            output_delta: false,
            change: Default::default(),
        });
        exchange.observe_turn(&event, 4)?;
        owner.reconcile(vec![TimelineEntry::Exchange {
            id: "parallel".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        }])?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .filter(|block| block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::Tool))
                .count(),
            1,
            "new calls must replace the preview even after explicit group expansion"
        );
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .find(|block| block.id.0 == "parallel:turn:1:third:preview")
                .unwrap()
                .text
                .row_count(),
            4
        );
        for id in ["tool", "latest", "third"] {
            let activity = event.activity.as_mut().unwrap();
            activity.id = id.into();
            activity.status = Some("completed".into());
            activity.output = None;
            exchange.observe_turn(&event, 5)?;
        }
        event.activity = None;
        event.kind = "assistant_message".into();
        event.text = Some("Tool group complete, continuing the exchange.".into());
        event.data = json!({ "phase": "commentary" });
        exchange.observe_turn(&event, 6)?;
        owner.reconcile(vec![TimelineEntry::Exchange {
            id: "parallel".into(),
            created_at_ms: 0,
            exchange: exchange.clone(),
            agent_by_id: HashMap::new(),
        }])?;
        let settled = owner.snapshot(&document)?;
        let group = settled
            .block
            .iter()
            .find(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::ToolGroup)
            })
            .unwrap();
        assert!(group.text.wire_rows().join("\n").contains("Ran 3 tools"));
        let group_node = group.metadata.node.as_ref().unwrap();
        assert_eq!(group_node.display, forge_buffer::node::NodeDisplay::Heading);
        assert!(
            !settled.block.iter().any(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::Tool)
            }),
            "Ran groups must collapse even after explicit live expansion"
        );
        owner.dispatch(PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: view.clone(),
                sequence: 101,
                node: group_node.id.0.clone(),
                generation: group_node.generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 40,
                width: WidthProfile::default(),
            },
        })?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .filter(|block| block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::Tool))
                .count(),
            3,
            "Ran groups must expose every call before exchange completion"
        );
        exchange.state = crate::exchange::ExchangeState::Complete;
        exchange.completed_at_ms = Some(7);
        owner.reconcile(vec![TimelineEntry::Exchange {
            id: "parallel".into(),
            created_at_ms: 0,
            exchange,
            agent_by_id: HashMap::new(),
        }])?;
        let activity = "parallel:exchange";
        let generation = owner
            .open
            .as_ref()
            .unwrap()
            .sections
            .nodes
            .generation(activity)
            .unwrap();
        owner.dispatch(PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view,
                sequence: 102,
                node: activity.into(),
                generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 40,
                width: WidthProfile::default(),
            },
        })?;
        assert_eq!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .filter(|block| block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::Tool))
                .count(),
            3,
            "completed exchanges must allow the full tool history to expand"
        );
        Ok(())
    }

    #[tokio::test]
    async fn markdown_responses_keep_source_without_native_syntax_jobs() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
        let entry_for = |body: &str| {
            let mut entry = interaction_entry("markdown");
            let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
                unreachable!()
            };
            exchange.turn.clear();
            exchange.node_list.clear();
            exchange.state = crate::exchange::ExchangeState::Running;
            exchange.completed_at_ms = None;
            exchange.execution_started_at_ms = Some(1);
            let mut event = BackendEvent {
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "markdown".into(),
                }),
                turn_boundary: Some(TurnBoundary::Started),
                kind: "turn_started".into(),
                text: None,
                data: serde_json::Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            exchange.observe_turn(&event, 1).unwrap();
            event.turn_boundary = None;
            event.kind = "assistant_message".into();
            event.text = Some(body.into());
            event.data = serde_json::json!({"phase":"final_answer"});
            exchange.observe_turn(&event, 2).unwrap();
            event.text = None;
            event.turn_boundary = Some(TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Completed,
            });
            exchange.observe_turn(&event, 3).unwrap();
            exchange
                .finish(crate::exchange::ExchangeState::Complete, 3)
                .unwrap();
            entry
        };
        let entry = entry_for("```ts\nconst value: number = 1;\n```");
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry.clone()])?;
        let document = DocumentId("transcript:markdown".into());
        let opened = owner.open(
            document.clone(),
            ViewId("markdown".into()),
            WidthProfile::default(),
        )?;
        assert!(!opened.syntax_pending);
        assert!(owner.capture_syntax(&document)?.is_none());
        let before = owner.snapshot(&document)?;
        assert!(
            before
                .block
                .iter()
                .any(|block| block.metadata.markdown && block.text.wire_rows().contains(&"```ts"))
        );
        owner.reconcile(vec![entry_for("```js\nconst value: number = 1;\n```")])?;
        let after = owner.snapshot(&document)?;
        assert!(
            after
                .block
                .iter()
                .any(|block| block.metadata.markdown && block.text.wire_rows().contains(&"```js"))
        );
        assert!(owner.capture_syntax(&document)?.is_none());
        owner.close(&document)?;
        assert!(owner.open.is_none());
        Ok(())
    }

    #[test]
    fn malformed_saved_patch_retains_one_diagnostic_without_syntax_retries() -> Result<()> {
        let mut entry = interaction_entry("invalid-diff");
        let source = "--- a/test.rs\n+++ b/test.rs\n@@ -1 +1 @@\n-before\n+after\n+outside\n";
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        exchange
            .node_list
            .push(crate::exchange::ExchangeNode::ArtifactChange {
                change: crate::exchange::ArtifactChange {
                    id: "saved-diff".into(),
                    path: "test.rs".into(),
                    diff_text: source.into(),
                    created_at_ms: 1,
                    declaration: None,
                },
            });
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry.clone()])?;
        let document = DocumentId("transcript:invalid-diff".into());
        let opened = owner.open(
            document.clone(),
            ViewId("invalid-diff".into()),
            WidthProfile::default(),
        )?;
        assert!(!opened.syntax_pending);
        for _ in 0..3 {
            owner.reconcile(vec![entry.clone()])?;
            assert!(owner.capture_syntax(&document)?.is_none());
            let snapshot = owner.snapshot(&document)?;
            let diagnostics = snapshot
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .filter(|row| row.contains("preview unavailable"))
                .count();
            assert_eq!(diagnostics, 1);
        }
        assert!(
            owner
                .open
                .as_ref()
                .unwrap()
                .action
                .values()
                .any(|action| matches!(
                    action, TranscriptAction::Diff { text, syntax: false } if text == source
                ))
        );
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        let Some(crate::exchange::ExchangeNode::ArtifactChange { change }) =
            exchange.node_list.last_mut()
        else {
            unreachable!()
        };
        change.diff_text = source.replace("+outside\n", "");
        owner.reconcile(vec![entry])?;
        assert!(owner.capture_syntax(&document)?.is_some());
        Ok(())
    }

    #[tokio::test]
    async fn saved_syntax_rejects_stale_and_closed_documents_and_survives_timer_refresh()
    -> Result<()> {
        use forge_diff::syntax::SyntaxEngine;
        use forge_diff::workers::{AnalysisPool, PoolLimits};
        use std::sync::Arc;
        let engine = SyntaxEngine::new(Arc::new(AnalysisPool::new(PoolLimits {
            workers: 1,
            jobs: 2,
        })));
        let mut entry = interaction_entry("syntax");
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        exchange.node_list.push(crate::exchange::ExchangeNode::ArtifactChange {
            change: crate::exchange::ArtifactChange {
                id: "saved-diff".into(),
                path: "test.mjs".into(),
                diff_text: "--- a/test.mjs\n+++ b/test.mjs\n@@ -1 +1 @@\n-const old = 1;\n+const next = 2;\n".into(),
                created_at_ms: 1,
                declaration: None,
            },
        });
        exchange.completed_at_ms = None;
        exchange.execution_started_at_ms = Some(1);
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry.clone()])?;
        let document = DocumentId("transcript:syntax".into());
        owner.open(
            document.clone(),
            ViewId("syntax".into()),
            WidthProfile::default(),
        )?;
        let job = owner.capture_syntax(&document)?.expect("saved diff work");
        let blocks = job.analyze(&engine).await?;
        let block_id = blocks[0].id.clone();
        owner.apply_syntax(&document, &job, blocks.clone())?;
        let highlighted = owner
            .open
            .as_ref()
            .unwrap()
            .transcript
            .document
            .block(&block_id)
            .unwrap()
            .clone();
        assert!(!highlighted.metadata.visible_decoration.is_empty());
        refresh_activity(owner.open.as_mut().unwrap(), &[entry.clone()])?;
        assert_eq!(
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .document
                .block(&block_id)
                .unwrap()
                .metadata,
            highlighted.metadata
        );
        assert!(
            owner.capture_syntax(&document)?.is_none(),
            "timer must not restart parsing"
        );
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        let Some(crate::exchange::ExchangeNode::ArtifactChange { change }) =
            exchange.node_list.last_mut()
        else {
            unreachable!()
        };
        change.diff_text.push('\n');
        owner.reconcile(vec![entry])?;
        let before = owner.snapshot(&document)?;
        owner.apply_syntax(&document, &job, blocks.clone())?;
        assert_eq!(
            owner.snapshot(&document)?,
            before,
            "stale completion changed the document"
        );
        owner.close(&document)?;
        owner.apply_syntax(&document, &job, blocks)?;
        assert!(owner.open.is_none());
        Ok(())
    }

    fn interaction_entry(identity: &str) -> TimelineEntry {
        use crate::backend::{
            BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind, TurnBoundary,
        };
        use crate::exchange::{ExchangeKind, ExchangeState};
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: identity.into(),
        };
        let mut interaction = Exchange {
            lifecycle: None,
            finalization_error: None,
            check_owner: None,
            finalization_outcome: None,
            provider_retry: None,
            agent_id: "primary".into(),
            id: identity.into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: format!("Prompt {identity}"),
            kind: ExchangeKind::Chat,
            mode: None,
            plan_id: None,
            execution_id: None,
            execution_phase: None,
            goal_id: None,
            state: ExchangeState::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            approval_resolution: Vec::new(),
            check_setup: None,
            disposition: crate::exchange::HistoryDisposition::Current,
            turn: Vec::new(),
            created_at_ms: 0,
            completed_at_ms: None,
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            execution_started_at_ms: None,
            metrics: crate::exchange::ExchangeMetrics::default(),
            comment: Vec::new(),
            task: None,
        };
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(address),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        interaction.observe_turn(&event, 0).unwrap();
        event.turn_boundary = None;
        event.kind = "reasoning".into();
        event.text = Some("Completed thought\nwith details".into());
        interaction.observe_turn(&event, 0).unwrap();
        event.text = None;
        event.kind = "tool".into();
        event.activity = Some(ToolActivity {
            id: "tool".into(),
            kind: ToolActivityKind::Command,
            title: "Read output".into(),
            output: Some("first\nsecond\nthird\nfourth\nfifth\nsixth".into()),
            output_delta: false,
            status: Some("completed".into()),
            change: Default::default(),
        });
        interaction.observe_turn(&event, 1).unwrap();
        event.activity = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        interaction.observe_turn(&event, 1).unwrap();
        interaction.finish(ExchangeState::Complete, 1).unwrap();
        TimelineEntry::Exchange {
            id: identity.into(),
            created_at_ms: 0,
            agent_by_id: HashMap::new(),
            exchange: interaction,
        }
    }

    #[test]
    fn tool_actions_survive_unrelated_revisions_without_retargeting_or_replay() -> Result<()> {
        use forge_buffer::{block::TextPosition, identity::BlockId};
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![
            interaction_entry("first"),
            interaction_entry("second"),
        ])?;
        let document = DocumentId("transcript:stable-tool".into());
        let view = ViewId("view:stable-tool".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        expand_sections(&mut owner, "view:stable-tool")?;
        let captured = owner.snapshot(&document)?;
        let input = DocumentInput {
            document: document.clone(),
            revision: captured.revision,
            view: view.clone(),
            sequence: InputSequence(1),
            action: "activate".into(),
            block: BlockId("first:turn:1:tool:tool".into()),
            position: TextPosition { row: 0, column: 8 },
            target: Some(TargetId("first:turn:1:tool:tool".into())),
        };
        owner.dispatch(PresentationRequest::Resize {
            document: document.clone(),
            view,
            width: WidthProfile {
                columns: 65,
                ..WidthProfile::default()
            },
        })?;
        assert!(owner.snapshot(&document)?.revision > captured.revision);
        let mut changed = input.clone();
        changed.action = "navigate_prompt".into();
        assert!(
            owner
                .dispatch(PresentationRequest::NavigatePrompt {
                    input: changed,
                    previous: true,
                })
                .is_err()
        );
        let mut changed = input.clone();
        changed.target = Some(TargetId("second:turn:1:tool:tool".into()));
        assert!(owner.action(changed).is_err());
        let mut changed = input.clone();
        changed.document = DocumentId("transcript:other".into());
        assert!(owner.action(changed).is_err());
        let mut changed = input.clone();
        changed.revision =
            forge_buffer::identity::DocumentRevision(owner.snapshot(&document)?.revision.0 + 1);
        assert!(owner.action(changed).is_err());
        assert!(matches!(
            owner.action(input.clone())?,
            TranscriptAction::Tool { .. }
        ));
        assert!(owner.action(input.clone()).is_err());
        let mut next = input.clone();
        next.sequence = InputSequence(2);
        owner.dispatch(PresentationRequest::ToolOpen {
            input: next.clone(),
            document: DocumentId("output:stable-tool".into()),
        })?;
        owner.reconcile(vec![interaction_entry("second")])?;
        next.sequence = InputSequence(3);
        assert!(owner.action(next).is_err());
        Ok(())
    }

    #[test]
    fn streaming_pages_publish_continuations_without_exceeding_the_loaded_row_budget() -> Result<()>
    {
        use super::super::nodes::{NodeAction, NodeRequest};
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        for (rows, initial, delta) in [
            (
                4096,
                format!("{}\n", "x".repeat(1500)).repeat(40),
                format!("{}\n", "y".repeat(1500)).repeat(10),
            ),
            (8, "initial\n".repeat(6), "appended\n".repeat(40)),
        ] {
            let mut value = serde_json::to_value(interaction_entry("active"))?;
            value["exchange"]["state"] = serde_json::json!("running");
            value["exchange"]["completed_at_ms"] = serde_json::Value::Null;
            value["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
            value["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
            value["exchange"]["turn"][0]["tool"]["item"]["tool"]["output"] =
                serde_json::json!(initial);
            value["exchange"]["turn"][0]["tool"]["item"]["tool"]["status"] =
                serde_json::json!("inProgress");
            value["exchange"]["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] =
                serde_json::Value::Null;
            let mut exchange: Exchange = serde_json::from_value(value["exchange"].take())?;
            let mut owner = SessionPresentation::new("session".into());
            owner.initialize(vec![TimelineEntry::Exchange {
                id: "active".into(),
                created_at_ms: 0,
                exchange: exchange.clone(),
                agent_by_id: HashMap::new(),
            }])?;
            let document = DocumentId("stream-page".into());
            let view = ViewId("view".into());
            owner.open(document.clone(), view.clone(), WidthProfile::default())?;
            expand_sections(&mut owner, "view")?;
            let node = "active:turn:1:tool:tool";
            let generation = owner
                .open
                .as_ref()
                .unwrap()
                .sections
                .nodes
                .generation(node)
                .unwrap();
            owner.dispatch(PresentationRequest::Node {
                request: NodeRequest {
                    document: document.clone(),
                    view,
                    sequence: 100,
                    node: node.into(),
                    generation,
                    action: NodeAction::SetExpansion { expanded: true },
                    rows,
                    width: WidthProfile::default(),
                },
            })?;
            let projected = owner.open.as_ref().unwrap().sections.projected_entries;
            let event = BackendEvent {
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "active".into(),
                }),
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: serde_json::json!({"emittedAtMs":10}),
                summary: None,
                task_update: None,
                activity: Some(ToolActivity {
                    id: "tool".into(),
                    kind: ToolActivityKind::Command,
                    title: "Read output".into(),
                    output: Some(delta),
                    status: Some("inProgress".into()),
                    output_delta: true,
                    change: Default::default(),
                }),
            };
            exchange.observe_turn(&event, 10)?;
            owner.append_tool_output(&exchange, &event)?;
            let snapshot = owner.snapshot(&document)?;
            let state = snapshot
                .block
                .iter()
                .filter_map(|block| block.metadata.node.as_ref())
                .find(|state| state.id.0 == node)
                .unwrap();
            assert!(state.more, "stream growth lost its continuation");
            assert!(
                state.loaded_rows <= rows + 1,
                "stream growth exceeded the page row budget"
            );
            assert_eq!(
                owner.open.as_ref().unwrap().sections.projected_entries,
                projected,
                "stream growth rebuilt the exchange instead of the tool subtree"
            );
        }
        Ok(())
    }

    #[test]
    fn tool_pages_format_only_the_requested_prefix_and_retain_previous_chunks() -> Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use forge_buffer::identity::BlockId;
        let output = (0..512)
            .map(|row| format!("row {row:03} {}\n", "x".repeat(2000)))
            .collect::<String>();
        let mut entry = serde_json::to_value(interaction_entry("paged"))?;
        entry["exchange"]["turn"][0]["tool"]["item"]["tool"]["output"] = serde_json::json!(output);
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Exchange {
            id: "paged".into(),
            created_at_ms: 0,
            exchange: serde_json::from_value(entry["exchange"].take())?,
            agent_by_id: HashMap::new(),
        }])?;
        let document = DocumentId("paged-tools".into());
        let view = ViewId("view:paged".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        expand_sections(&mut owner, "view:paged")?;
        let node = "paged:turn:1:tool:tool";
        let generation = owner
            .open
            .as_ref()
            .unwrap()
            .sections
            .nodes
            .generation(node)
            .unwrap();
        let request = |action, sequence| PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: view.clone(),
                sequence,
                node: node.into(),
                generation,
                action,
                rows: 4096,
                width: WidthProfile {
                    columns: 256,
                    ..WidthProfile::default()
                },
            },
        };
        owner.dispatch(request(NodeAction::SetExpansion { expanded: true }, 100))?;
        let first = BlockId("paged:turn:1:tool:preview".into());
        let later = BlockId("paged:turn:1:tool:output:4".into());
        let pointer = |owner: &SessionPresentation| {
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .document
                .block(&first)
                .unwrap()
                .text
                .row(0)
                .unwrap()
                .as_ptr()
        };
        let retained = pointer(&owner);
        assert!(
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .document
                .block(&later)
                .is_none(),
            "initial expansion formatted beyond its 64 KiB source prefix"
        );
        let mut sequence = 101;
        while owner.snapshot(&document)?.block.iter().any(|block| {
            block
                .metadata
                .node
                .as_ref()
                .is_some_and(|state| state.id.0 == node && state.more)
        }) {
            assert!(sequence < 140, "tool paging made no progress");
            owner.dispatch(request(NodeAction::LoadMore, sequence))?;
            assert_eq!(
                pointer(&owner),
                retained,
                "paging reformatted the retained prefix"
            );
            sequence += 1;
        }
        assert!(
            owner.snapshot(&document)?.block.iter().any(|block| block
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("row 511"))),
            "last tool row was lost during paging"
        );
        owner.dispatch(request(
            NodeAction::SetExpansion { expanded: false },
            sequence,
        ))?;
        owner.dispatch(request(
            NodeAction::SetExpansion { expanded: true },
            sequence + 1,
        ))?;
        assert!(
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .document
                .block(&later)
                .is_none(),
            "reopening retained obsolete source pages"
        );
        Ok(())
    }

    #[test]
    fn node_expansion_owns_full_output_and_survives_reflow() -> Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use forge_buffer::identity::BlockId;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("first")])?;
        let document = DocumentId("transcript:nodes".into());
        let view = ViewId("view:nodes".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        expand_sections(&mut owner, "view:nodes")?;
        let projected_entries = owner.open.as_ref().unwrap().sections.projected_entries;
        let node = "first:turn:1:tool:tool";
        let generation = owner
            .open
            .as_ref()
            .unwrap()
            .sections
            .nodes
            .generation(node)
            .unwrap();
        let request = |expanded, sequence, generation| PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: view.clone(),
                sequence,
                node: node.into(),
                generation,
                action: NodeAction::SetExpansion { expanded },
                rows: 48,
                width: WidthProfile::default(),
            },
        };
        owner.dispatch(request(true, 100, generation))?;
        let text = |owner: &SessionPresentation| -> Result<Vec<String>> {
            Ok(owner
                .snapshot(&document)?
                .block
                .into_iter()
                .flat_map(|block| {
                    block
                        .text
                        .wire_rows()
                        .into_iter()
                        .map(str::to_owned)
                        .collect::<Vec<_>>()
                })
                .collect())
        };
        assert!(text(&owner)?.iter().any(|row| row.contains("sixth")));
        let invalid = NodeRequest {
            document: document.clone(),
            view: view.clone(),
            sequence: 101,
            node: node.into(),
            generation,
            action: NodeAction::SetExpansion { expanded: false },
            rows: 0,
            width: WidthProfile::default(),
        };
        assert!(
            owner
                .dispatch(PresentationRequest::Node { request: invalid })
                .is_err()
        );
        assert!(
            text(&owner)?.iter().any(|row| row.contains("sixth")),
            "invalid request changed the committed frame"
        );
        owner.dispatch(request(false, 99, generation))?;
        assert!(
            text(&owner)?.iter().any(|row| row.contains("sixth")),
            "stale action changed expansion"
        );
        assert!(owner.dispatch(request(false, 101, generation + 1)).is_err());
        owner.dispatch(PresentationRequest::Resize {
            document: document.clone(),
            view: view.clone(),
            width: WidthProfile {
                columns: 65,
                ..WidthProfile::default()
            },
        })?;
        assert!(text(&owner)?.iter().any(|row| row.contains("sixth")));
        owner.dispatch(request(false, 102, generation))?;
        let snapshot = owner.snapshot(&document)?;
        assert!(
            snapshot
                .block
                .iter()
                .all(|block| block.id != BlockId("first:turn:1:tool:preview".into()))
        );
        assert!(
            !text(&owner)?
                .iter()
                .any(|row| row.contains("hidden") || row.contains("sixth"))
        );
        owner.dispatch(request(true, 103, generation))?;
        assert!(text(&owner)?.iter().any(|row| row.contains("sixth")));
        // Resizing reflows once. Node toggles use only their indexed subtree.
        assert_eq!(
            owner.open.as_ref().unwrap().sections.projected_entries,
            projected_entries + 1
        );
        owner.reconcile(Vec::new())?;
        owner.reconcile(vec![interaction_entry("first")])?;
        assert!(
            owner
                .open
                .as_ref()
                .unwrap()
                .sections
                .nodes
                .generation(node)
                .unwrap()
                > generation
        );
        assert!(
            owner.dispatch(request(true, 104, generation)).is_err(),
            "retired node accepted an old action"
        );
        Ok(())
    }

    #[test]
    fn prompt_navigation_and_tool_open_validate_the_same_input_lifetime() -> Result<()> {
        use forge_buffer::{block::TextPosition, identity::BlockId};
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![
            interaction_entry("first"),
            interaction_entry("second"),
        ])?;
        let document = DocumentId("transcript:actions".into());
        let view = ViewId("view:actions".into());
        let opened = owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        assert!(
            opened
                .transcript
                .block
                .iter()
                .any(|block| block.metadata.node.is_some())
        );
        let mut input = DocumentInput {
            document: document.clone(),
            revision: opened.transcript.revision,
            view,
            sequence: InputSequence(1),
            action: "navigate_prompt".into(),
            block: BlockId("first:prompt".into()),
            position: TextPosition { row: 0, column: 0 },
            target: None,
        };
        let moved = owner.navigate_prompt(input.clone(), false)?;
        assert_eq!(moved["anchor"]["block"], "second:prompt");
        assert!(owner.navigate_prompt(input.clone(), false).is_err());
        expand_sections(&mut owner, "view:actions")?;
        input.revision = owner.snapshot(&document)?.revision;
        input.sequence = InputSequence(2);
        input.block = BlockId("first:turn:1:tool:tool".into());
        input.target = Some(TargetId("first:turn:1:tool:tool".into()));
        input.action = "activate".into();
        let output = DocumentId("tool:actions".into());
        owner.dispatch(PresentationRequest::ToolOpen {
            input,
            document: output.clone(),
        })?;
        let delivery = owner.dispatch(PresentationRequest::ToolDemand {
            document: output.clone(),
            revision: DocumentRevision(0),
        })?;
        assert!(delivery["patch"].is_object());
        owner.close(&output)?;
        assert!(owner.snapshot(&output).is_err());
        assert!(owner.snapshot(&document).is_ok());
        Ok(())
    }

    #[test]
    fn continued_exchanges_keep_their_plan_and_agent_document_owner() -> Result<()> {
        use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
        use crate::exchange::ExchangeState;

        for nesting in ["plan", "agent", "nested-agent"] {
            let TimelineEntry::Exchange { mut exchange, .. } = interaction_entry("continued")
            else {
                unreachable!()
            };
            exchange.state = ExchangeState::Running;
            exchange.completed_at_ms = None;
            let agent_entry =
                |identity: &str, exchange: Vec<Exchange>, agent: Vec<TimelineEntry>| {
                    TimelineEntry::AgentLifecycle {
                        id: identity.into(),
                        created_at_ms: 0,
                        run: crate::agent::Agent {
                            id: identity.into(),
                            session_id: "session".into(),
                            provider_thread_id: Some(identity.into()),
                            definition: "reviewer".into(),
                            task_name: "reviewer".into(),
                            task_path: "/root/reviewer".into(),
                            source: crate::agent::AgentSource::Model,
                            selection: None,
                            inbox: Vec::new(),
                            nickname: None,
                            state: crate::agent::AgentState::Ready,
                            created_at_ms: 0,
                            updated_at_ms: 0,
                        },
                        exchange,
                        agent,
                    }
                };
            let entry = if nesting == "plan" {
                TimelineEntry::Exchange {
                    id: exchange.id.clone(),
                    created_at_ms: exchange.created_at_ms,
                    exchange: exchange.clone(),
                    agent_by_id: HashMap::new(),
                }
            } else {
                let child = agent_entry("child", vec![exchange.clone()], Vec::new());
                if nesting == "agent" {
                    child
                } else {
                    let mut parent = interaction_entry("parent");
                    let TimelineEntry::Exchange { agent_by_id, .. } = &mut parent else {
                        unreachable!()
                    };
                    agent_by_id.insert("root".into(), agent_entry("root", Vec::new(), vec![child]));
                    parent
                }
            };
            let owner_id = entry.id();
            let mut owner = SessionPresentation::new("session".into());
            owner.initialize(vec![entry])?;
            let document = DocumentId(format!("transcript:{nesting}"));
            let opened = owner.open(
                document.clone(),
                ViewId(format!("view:{nesting}")),
                WidthProfile::default(),
            )?;
            let mut event = BackendEvent {
                received_at_ms: None,
                address: Some(ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "second".into(),
                }),
                turn_boundary: Some(TurnBoundary::Started),
                kind: "turn_started".into(),
                text: None,
                data: serde_json::Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            exchange.observe_turn(&event, 2)?;
            event.turn_boundary = None;
            event.kind = "assistant_message".into();
            event.text = Some("Second task is running".into());
            exchange.observe_turn(&event, 3)?;
            let patch = owner.update_live(Some(&exchange), None, SessionPhase::Idle, true)?;
            assert_eq!(
                owner
                    .timeline
                    .entry_list()
                    .iter()
                    .filter(|entry| !matches!(entry, TimelineEntry::Status { .. }))
                    .count(),
                1,
                "{nesting} duplicated its exchange"
            );
            assert!(
                patch.operation.iter().filter(|operation| !matches!(operation,
                    TimelineOperation::Insert { entry: TimelineEntry::Status { .. }, .. }
                    | TimelineOperation::Replace { entry: TimelineEntry::Status { .. }, .. }))
                    .count() == 1 && patch.operation.iter().any(|operation|
                        matches!(operation, TimelineOperation::Replace { entry, .. } if entry.id() == owner_id))
            );
            owner.sync(&document, opened.transcript.revision)?;
            let snapshot = owner.snapshot(&document)?;
            let mut identities = std::collections::HashSet::new();
            assert!(
                snapshot
                    .block
                    .iter()
                    .all(|block| identities.insert(block.id.clone()))
            );
            assert!(
                serde_json::to_string(owner.timeline.entry_list())?
                    .contains("Second task is running")
            );
            if nesting != "nested-agent" {
                assert!(
                    snapshot
                        .block
                        .iter()
                        .any(|block| (0..block.text.row_count()).any(|row| block
                            .text
                            .row(row)
                            .unwrap()
                            .contains("Second task is running")))
                );
            }

            owner.update_live(None, Some(&exchange.id), SessionPhase::Idle, true)?;
            owner.sync(&document, snapshot.revision)?;
            assert_eq!(
                owner
                    .timeline
                    .entry_list()
                    .iter()
                    .filter(|entry| !matches!(entry, TimelineEntry::Status { .. }))
                    .count(),
                if nesting == "plan" { 0 } else { 1 }
            );
            assert!(
                !serde_json::to_string(owner.timeline.entry_list())?
                    .contains("Second task is running")
            );
            if nesting != "plan" {
                assert_eq!(owner.timeline.entry_list()[0].id(), owner_id);
            }
        }
        Ok(())
    }

    #[test]
    fn child_fold_streams_headers_then_collapses_completed_history() -> Result<()> {
        use super::super::nodes::{NodeAction, NodeRequest};
        use crate::backend::{BackendEvent, ToolActivity, ToolActivityKind, TurnBoundary};
        use crate::exchange::{ExchangeNode, ExchangeState};
        let live = |identity: &str| -> Result<Exchange> {
            let TimelineEntry::Exchange { exchange, .. } = interaction_entry(identity) else {
                unreachable!()
            };
            let mut raw = serde_json::to_value(exchange)?;
            raw["state"] = serde_json::json!("running");
            raw["completed_at_ms"] = serde_json::Value::Null;
            raw["execution_started_at_ms"] = serde_json::json!(1);
            raw["turn"][0]["state"] = serde_json::json!({"kind":"running"});
            raw["turn"][0]["tool"]["item"]["tool"]["status"] = serde_json::json!("inProgress");
            raw["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] = serde_json::Value::Null;
            Ok(serde_json::from_value(raw)?)
        };
        let mut child = live("child")?;
        let mut parent = live("parent")?;
        parent.turn.clear();
        parent.node_list.clear();
        let mut run = crate::agent::Agent::pending("session", "explorer", "Inspect", 0);
        run.id = "child-run".into();
        parent.node_list.push(ExchangeNode::AgentReference {
            agent: crate::agent::Delegation {
                id: "delegation".into(),
                parent_exchange_id: parent.id.clone(),
                parent_turn_id: None,
                child_agent_id: run.id.clone(),
                child_exchange_id: child.id.clone(),
                task: child.prompt.clone(),
                created_at_ms: 0,
            },
        });
        let entry = |child: Exchange| TimelineEntry::Exchange {
            id: parent.id.clone(),
            created_at_ms: 0,
            exchange: parent.clone(),
            agent_by_id: HashMap::from([(
                run.id.clone(),
                TimelineEntry::AgentLifecycle {
                    id: run.id.clone(),
                    created_at_ms: 0,
                    run: run.clone(),
                    exchange: vec![child],
                    agent: Vec::new(),
                },
            )]),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry(child.clone())])?;
        let document = DocumentId("transcript:child-fold".into());
        let view = ViewId("view:child-fold".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let contents = |owner: &SessionPresentation| -> Result<String> {
            Ok(owner
                .snapshot(&document)?
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n"))
        };
        assert!(contents(&owner)?.contains("Read output"));
        assert!(!contents(&owner)?.contains("sixth"));
        let generation = owner
            .open
            .as_ref()
            .unwrap()
            .sections
            .nodes
            .generation("delegation")
            .unwrap();
        let expand = |sequence| PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: view.clone(),
                sequence,
                node: "delegation".into(),
                generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 48,
                width: WidthProfile::default(),
            },
        };
        owner.dispatch(expand(1))?;
        owner.dispatch(PresentationRequest::SelectAgent {
            sequence: owner.open.as_ref().unwrap().selection_sequence + 1,
            document: document.clone(),
            run_id: Some(run.id.clone()),
        })?;
        assert!(
            contents(&owner)?.contains("Prompt child")
                && !contents(&owner)?.contains("Prompt parent")
        );
        owner.dispatch(PresentationRequest::SelectAgent {
            sequence: owner.open.as_ref().unwrap().selection_sequence + 1,
            document: document.clone(),
            run_id: None,
        })?;
        let mut event = BackendEvent {
            received_at_ms: None,
            address: Some(child.turn[0].provider().clone()),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: Some(ToolActivity {
                id: "tool".into(),
                kind: ToolActivityKind::Command,
                title: "Read output".into(),
                output: Some("\nSTREAMED_CHILD_OUTPUT".into()),
                output_delta: true,
                status: Some("inProgress".into()),
                change: Default::default(),
            }),
            summary: None,
            task_update: None,
        };
        child.observe_turn(&event, 2)?;
        owner
            .append_tool_output(&child, &event)?
            .expect("child output patch");
        assert!(!contents(&owner)?.contains("STREAMED_CHILD_OUTPUT"));
        event.activity = None;
        event.kind = "assistant_message".into();
        event.text = Some("CHILD_FINAL_RESPONSE".into());
        event.data = serde_json::json!({"phase":"final_answer"});
        child.observe_turn(&event, 3)?;
        owner.reconcile(vec![entry(child.clone())])?;
        assert!(!contents(&owner)?.contains("CHILD_FINAL_RESPONSE"));
        event.text = None;
        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: crate::turn::TurnOutcome::Completed,
        });
        child.observe_turn(&event, 4)?;
        child.finish(ExchangeState::Complete, 4)?;
        owner.reconcile(vec![entry(child)])?;
        let completed = owner.snapshot(&document)?;
        let heading = completed
            .block
            .iter()
            .find(|block| block.id.0 == "delegation")
            .unwrap();
        assert_eq!(
            heading.metadata.node.as_ref().unwrap().display,
            forge_buffer::node::NodeDisplay::Heading
        );
        assert!(!contents(&owner)?.contains("CHILD_FINAL_RESPONSE"));
        owner.dispatch(expand(2))?;
        assert!(
            contents(&owner)?.contains("Prompt child")
                && contents(&owner)?.contains("CHILD_FINAL_RESPONSE")
        );
        expand_sections(&mut owner, "view:child-fold")?;
        let tool_node = "child:turn:1:tool:tool";
        let tool_generation = owner
            .open
            .as_ref()
            .unwrap()
            .sections
            .nodes
            .generation(tool_node)
            .unwrap();
        owner.dispatch(PresentationRequest::Node {
            request: NodeRequest {
                document: document.clone(),
                view: view.clone(),
                sequence: 100,
                node: tool_node.into(),
                generation: tool_generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 48,
                width: WidthProfile::default(),
            },
        })?;
        assert!(contents(&owner)?.contains("STREAMED_CHILD_OUTPUT"));
        owner.dispatch(PresentationRequest::SelectAgent {
            sequence: owner.open.as_ref().unwrap().selection_sequence + 1,
            document: document.clone(),
            run_id: Some(run.id),
        })?;
        assert!(
            contents(&owner)?.contains("Prompt child")
                && !contents(&owner)?.contains("Prompt parent")
        );
        Ok(())
    }

    #[test]
    fn selected_agent_updates_keep_the_submission_and_document_lifetime() -> Result<()> {
        use crate::agent::{Agent, AgentState};
        let TimelineEntry::Exchange {
            exchange: interaction,
            ..
        } = interaction_entry("child")
        else {
            unreachable!()
        };
        let mut raw = serde_json::to_value(interaction)?;
        raw["state"] = serde_json::json!("running");
        raw["completed_at_ms"] = serde_json::Value::Null;
        raw["execution_started_at_ms"] = serde_json::json!(1);
        raw["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        raw["turn"][0]["tool"]["item"]["tool"]["status"] = serde_json::json!("inProgress");
        raw["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] = serde_json::Value::Null;
        let mut interaction: Exchange = serde_json::from_value(raw)?;
        let agent = TimelineEntry::AgentLifecycle {
            id: "agent:child".into(),
            created_at_ms: 0,
            run: Agent {
                id: "child-run".into(),
                session_id: "session".into(),
                provider_thread_id: None,
                definition: "explorer".into(),
                task_name: "explorer".into(),
                task_path: "/root/explorer".into(),
                source: crate::agent::AgentSource::Model,
                selection: None,
                inbox: Vec::new(),
                nickname: None,
                state: AgentState::Ready,
                created_at_ms: 0,
                updated_at_ms: 1,
            },
            exchange: vec![interaction.clone()],
            agent: Vec::new(),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("main"), agent.clone()])?;
        let document = DocumentId("transcript:scope".into());
        let initial = owner.open(
            document.clone(),
            ViewId("view:scope".into()),
            WidthProfile::default(),
        )?;
        owner.begin_submission(&document, 1, "retained submission")?;
        owner.dispatch(PresentationRequest::Staging {
            sequence: owner.open.as_ref().unwrap().staging_sequence + 1,
            document: document.clone(),
            queue: vec!["queued while selecting agent".into()],
            steering: vec!["keep active guidance".into()],
        })?;
        owner.dispatch(PresentationRequest::SelectAgent {
            sequence: owner.open.as_ref().unwrap().selection_sequence + 1,
            document: document.clone(),
            run_id: Some("child-run".into()),
        })?;
        let selected = owner.snapshot(&document)?;
        assert!(selected.revision > initial.transcript.revision);
        assert!(
            selected
                .block
                .iter()
                .any(|block| block.id.0 == "child:prompt")
        );
        assert!(
            !selected
                .block
                .iter()
                .any(|block| block.id.0 == "main:prompt")
        );
        assert_eq!(selected.block.last().unwrap().id.0, "harness:staging");
        assert!(
            selected
                .block
                .last()
                .unwrap()
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("queued while selecting agent"))
        );
        let event = crate::backend::BackendEvent {
            received_at_ms: None,
            address: Some(interaction.turn[0].provider().clone()),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::json!({"emittedAtMs":2}),
            activity: Some(crate::backend::ToolActivity {
                id: "tool".into(),
                kind: crate::backend::ToolActivityKind::Command,
                title: "Read output".into(),
                output: Some("\nscoped output".into()),
                status: Some("inProgress".into()),
                output_delta: true,
                change: Default::default(),
            }),
            summary: None,
            task_update: None,
        };
        interaction.observe_turn(&event, 2)?;
        owner
            .append_tool_output(&interaction, &event)?
            .expect("scoped output");
        let update = owner.sync(&document, selected.revision)?;
        assert!(
            update.snapshot.is_none()
                && update.patch.iter().all(|patch| patch.block_edit.is_empty())
        );
        assert!(
            owner.open.as_ref().unwrap().sections.nodes.tool["child:turn:1:tool"]
                .preview(true)?
                .row
                .contains(&"scoped output")
        );
        owner.reconcile(vec![interaction_entry("other-main"), agent])?;
        assert_eq!(owner.open.as_ref().unwrap().pending_submission, Some(1));
        owner.dispatch(PresentationRequest::SelectAgent {
            sequence: owner.open.as_ref().unwrap().selection_sequence + 1,
            document: document.clone(),
            run_id: None,
        })?;
        assert!(
            owner
                .snapshot(&document)?
                .block
                .iter()
                .any(|block| block.id.0 == "other-main:prompt")
        );
        owner.close_all()?;
        assert!(owner.snapshot(&document).is_err());
        Ok(())
    }

    #[test]
    fn compaction_counter_refreshes_without_provider_events() -> Result<()> {
        let started_at_ms = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)?
            .as_millis() as i64;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "session:status".into(),
            created_at_ms: 0,
            status: SessionPhase::Compacting { started_at_ms },
        }])?;
        let document = DocumentId("transcript:compaction-timer".into());
        let opened = owner.open(
            document.clone(),
            ViewId("view:compaction-timer".into()),
            WidthProfile::default(),
        )?;
        let before = opened.transcript.block[0]
            .text
            .row(1)
            .unwrap()
            .strip_prefix("Compacting · ")
            .unwrap()
            .trim_end_matches('s')
            .parse::<u64>()?;
        let timeline_revision = owner.revision();
        std::thread::sleep(std::time::Duration::from_millis(1_100));
        let update = owner.sync(&document, opened.transcript.revision)?;
        assert!(
            !update.patch.is_empty(),
            "elapsed time did not produce a document patch"
        );
        let refreshed = owner.snapshot(&document)?;
        let after = refreshed.block[0]
            .text
            .row(1)
            .unwrap()
            .strip_prefix("Compacting · ")
            .unwrap()
            .trim_end_matches('s')
            .parse::<u64>()?;
        assert!(after > before, "compaction counter did not advance");
        assert_eq!(
            owner.revision(),
            timeline_revision,
            "timer changed durable timeline state"
        );
        assert!(
            refreshed.block[0]
                .metadata
                .status
                .as_ref()
                .unwrap()
                .animated
        );
        owner.update_live(None, None, SessionPhase::Idle, true)?;
        let idle = owner.snapshot(&document)?;
        let status = idle
            .block
            .iter()
            .find(|block| block.id.0 == "session:status")
            .expect("idle status remains visible");
        assert!(!status.metadata.status.as_ref().unwrap().animated);
        assert!(
            status
                .text
                .wire_rows()
                .iter()
                .any(|row| row.contains("Idle"))
        );
        Ok(())
    }

    #[test]
    fn timer_refresh_retains_completed_entry_allocations() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![
            interaction_entry("completed"),
            TimelineEntry::Status {
                checks: None,
                status_thoughts: true,
                agent_count: 0,
                id: "status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    duration_ms: 0,
                    started_at_ms: Some(0),
                    activity: crate::session::state_machine::WorkflowActivity::Working,
                    execution: None,
                    reasoning_summary: None,
                },
            },
        ])?;
        let document = DocumentId("transcript:timer".into());
        let opened = owner.open(
            document.clone(),
            ViewId("view:timer".into()),
            WidthProfile::default(),
        )?;
        let open = owner.open.as_ref().unwrap();
        let (entry_id, pointer) = open
            .entry
            .iter()
            .find(|(_, entry)| !entry.prompt.is_empty())
            .map(|(id, entry)| (id.clone(), entry.prompt.as_ptr()))
            .unwrap();
        let revision = owner.revision();
        owner.sync(&document, opened.transcript.revision)?;
        assert_eq!(owner.revision(), revision);
        assert_eq!(
            owner.open.as_ref().unwrap().entry[&entry_id]
                .prompt
                .as_ptr(),
            pointer
        );
        Ok(())
    }

    #[test]
    fn submission_admission_rejects_stale_tokens_and_closed_lifetimes() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        let document = DocumentId("transcript:admission".into());
        owner.open(
            document.clone(),
            ViewId("view:admission".into()),
            WidthProfile::default(),
        )?;
        assert!(owner.begin_submission(&document, 0, "draft").is_err());
        assert!(owner.begin_submission(&document, 1, " ").is_err());
        assert!(
            owner
                .begin_submission(&document, 1, &"\n".repeat(4096))
                .is_err()
        );
        owner.begin_submission(&document, 1, &"complete λ\n\ndraft\n".repeat(8193))?;
        assert!(
            owner
                .begin_submission(&document, 2, "duplicate pending")
                .is_err()
        );
        assert!(owner.complete_submission(&document, 2, true)?.is_none());
        assert!(owner.complete_submission(&document, 1, false)?.is_none());
        assert!(owner.begin_submission(&document, 1, "stale token").is_err());
        owner.begin_submission(&document, 2, "next draft")?;
        assert_eq!(
            owner.complete_submission(&document, 2, true)?,
            Some(serde_json::json!({"document":document,"token":2,"state":"accepted"}))
        );
        assert!(owner.complete_submission(&document, 2, true)?.is_none());
        assert_eq!(
            owner.retract_submission(&document, 2)?,
            Some(serde_json::json!({"document":document,"token":2,"state":"retracted"}))
        );
        assert!(owner.retract_submission(&document, 2)?.is_none());
        owner.begin_submission(&document, 3, "pending at close")?;
        owner.close(&document)?;
        assert!(owner.begin_submission(&document, 4, "late prompt").is_err());
        assert!(owner.complete_submission(&document, 3, true)?.is_none());
        let replacement = DocumentId("transcript:replacement".into());
        owner.open(
            replacement.clone(),
            ViewId("view:replacement".into()),
            WidthProfile::default(),
        )?;
        owner.begin_submission(&replacement, 1, "new lifetime")?;
        assert!(owner.complete_submission(&document, 3, true)?.is_none());
        assert!(owner.retract_submission(&document, 2)?.is_none());
        assert!(owner.complete_submission(&replacement, 1, true)?.is_some());
        Ok(())
    }

    #[test]
    fn active_projection_preserves_pending_submission() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::Idle,
        }])?;
        let document = DocumentId("transcript:test".into());
        let opened = owner.open(
            document.clone(),
            ViewId("view:test".into()),
            WidthProfile::default(),
        )?;
        owner.begin_submission(&document, 1, "draft")?;
        owner.reconcile(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::WaitingForAgent { agent_count: 2 },
        }])?;
        let synced = owner.sync(&document, opened.transcript.revision)?;
        assert!(!synced.patch.is_empty());
        assert!(synced.snapshot.is_none());
        assert_eq!(owner.open.as_ref().unwrap().pending_submission, Some(1));
        assert!(
            owner
                .sync(&document, DocumentRevision(forge_buffer::MAX_COUNTER))
                .is_err()
        );
        Ok(())
    }

    #[test]
    fn hidden_transcript_keeps_streaming_until_a_new_width_owner_attaches() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::WaitingForAgent { agent_count: 2 },
        }])?;
        let document = DocumentId("transcript:hidden".into());
        let opened = owner.open(
            document.clone(),
            ViewId("view:old".into()),
            WidthProfile::default(),
        )?;
        owner.dispatch(PresentationRequest::CloseView {
            document: document.clone(),
            view: ViewId("view:old".into()),
        })?;
        owner.reconcile(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::WaitingForAgent { agent_count: 3 },
        }])?;
        assert!(owner.snapshot(&document)?.revision > opened.transcript.revision);
        owner.dispatch(PresentationRequest::OpenView {
            document: document.clone(),
            view: ViewId("view:new".into()),
            width: WidthProfile {
                columns: 6,
                ..WidthProfile::default()
            },
        })?;
        assert_eq!(owner.open.as_ref().unwrap().last_width.columns, 6);

        assert!(
            !owner
                .sync(&document, opened.transcript.revision)?
                .patch
                .is_empty()
        );
        Ok(())
    }

    #[test]
    fn transcript_rejects_another_document_lifetime() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(Vec::new())?;
        owner.open(
            DocumentId("transcript:first".into()),
            ViewId("view:first".into()),
            WidthProfile::default(),
        )?;
        assert!(
            owner
                .open(
                    DocumentId("transcript:other".into()),
                    ViewId("view:other".into()),
                    WidthProfile::default(),
                )
                .is_err()
        );
        Ok(())
    }

    #[test]
    fn layout_follows_the_oldest_live_view_without_changing_timeline_revision() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Status {
            checks: None,
            status_thoughts: true,
            agent_count: 0,
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::WaitingForAgent { agent_count: 2 },
        }])?;
        let document = DocumentId("transcript:width".into());
        let first = ViewId("view:first".into());
        let second = ViewId("view:second".into());
        owner.open(document.clone(), first.clone(), WidthProfile::default())?;
        let timeline_revision = owner.revision();
        let initial = owner.snapshot(&document)?.revision;
        owner.open(
            document.clone(),
            second.clone(),
            WidthProfile {
                columns: 4,
                ..WidthProfile::default()
            },
        )?;
        assert_eq!(owner.snapshot(&document)?.revision, initial);
        owner.dispatch(PresentationRequest::Resize {
            document: document.clone(),
            view: second.clone(),
            width: WidthProfile {
                columns: 6,
                ..WidthProfile::default()
            },
        })?;
        assert_eq!(owner.snapshot(&document)?.revision, initial);
        owner.dispatch(PresentationRequest::CloseView {
            document: document.clone(),
            view: first,
        })?;
        assert_eq!(
            owner
                .open
                .as_ref()
                .unwrap()
                .transcript
                .views
                .profile()
                .unwrap()
                .columns,
            6
        );
        assert!(owner.snapshot(&document)?.revision > initial);
        assert_eq!(owner.revision(), timeline_revision);
        assert!(!owner.sync(&document, initial)?.patch.is_empty());
        Ok(())
    }
}
