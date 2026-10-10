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

use super::document::{TranscriptChange, TranscriptDocument};
use super::output::OutputDocument;
use super::projection::{ProjectedEntry, TranscriptAction, project};
use super::tool::ToolOutputView;

const MAX_PROJECTED_BYTES: usize = 64 * 1024 * 1024;
const MAX_PENDING_BYTES: usize = 2 * 1024 * 1024;
const MAX_OUTPUT_VIEW_BYTES: usize = 96 * 1024 * 1024;

pub struct SessionPresentation {
    session_id: String,
    timeline: TimelineStream,
    open: Option<OpenPresentation>,
    activity_updated: std::time::Instant,
}

struct OpenPresentation {
    expanded_tool: HashSet<String>,
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
    entry: HashMap<String, EntryPresentation>,
    action: HashMap<TargetId, TranscriptAction>,
    tool: HashMap<String, ToolOutputView>,
    output: HashMap<DocumentId, OutputDocument>,
    pending: VecDeque<(BufferPatch, usize)>,
    pending_bytes: usize,
    retained_bytes: usize,
    input: HashMap<ViewId, InputSequence>,
    failure: Option<String>,
}

struct EntryPresentation {
    prompt: Vec<forge_buffer::identity::BlockId>,
    target: Vec<TargetId>,
    syntax: Vec<TargetId>,
    tool: Vec<String>,
    bytes: usize,
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
    SectionExpansion {
        document: DocumentId,
        view: ViewId,
        sequence: u64,
        section: String,
        expanded: bool,
        #[serde(default)]
        more: bool,
        rows: usize,
        width: WidthProfile,
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
    ToggleTool {
        input: DocumentInput,
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
    pub(crate) fn append_tool_output(&mut self, exchange: &Exchange,
        event: &crate::backend::BackendEvent) -> Result<Option<TimelinePatch>> {
        let patch = self.timeline.append_tool_output(exchange,event)?;
        if let Some(patch) = &patch { self.project_patch(patch); }
        Ok(patch)
    }

    pub(crate) fn update_message(&mut self, exchange: &Exchange,
        event: &crate::backend::BackendEvent) -> Result<Option<TimelinePatch>> {
        let patch = self.timeline.update_message(exchange,event)?;
        if let Some(patch) = &patch { self.project_patch(patch); }
        Ok(patch)
    }

    /// Capture visible context without acquiring the active prompt broker.
    pub(crate) fn conversation_history(&self) -> Result<String> {
        crate::backend::text_generation::history(self.timeline.entry_list())
    }
    pub fn dispatch(&mut self, request: PresentationRequest) -> Result<serde_json::Value> {
        use serde_json::{json, to_value};
        match request {
            PresentationRequest::SectionExpansion { document, view, sequence, section, expanded, more, rows, width } => {
                self.document(&document)?;
                let open = self.open.as_mut().expect("validated presentation");
                open.sections.set(&mut open.transcript, view, sequence, &section, expanded, more, Some((rows, width)))?;
                retain_patches(open, Vec::new())?;
                Ok(json!({}))
            }
            PresentationRequest::Highlight { .. } => {
                anyhow::bail!("syntax requires the asynchronous service owner")
            }
            PresentationRequest::PlanOpen { .. }
            | PresentationRequest::PlanAction { .. }
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
            PresentationRequest::SelectAgent { document, run_id } => {
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
                let previous = std::mem::replace(&mut open.agent_scope, run_id);
                if let Err(failure) =
                    reflow(open, self.timeline.entry_list(), self.timeline.revision())
                {
                    open.agent_scope = previous;
                    return Err(failure);
                }
                Ok(json!({}))
            }
            PresentationRequest::NavigatePrompt { input, previous } => {
                self.navigate_prompt(input, previous)
            }
            PresentationRequest::ToggleTool { input } => {
                let TranscriptAction::Tool { call_id } = self.action(input)? else {
                    anyhow::bail!("transcript target is not a tool output");
                };
                let open = self
                    .open
                    .as_mut()
                    .context("session presentation is not open")?;
                let expanded = !open.expanded_tool.remove(&call_id);
                if expanded {
                    open.expanded_tool.insert(call_id.clone());
                }
                if let Err(error) = toggle_inline_output(open,&call_id,expanded) {
                    if expanded {
                        open.expanded_tool.remove(&call_id);
                    } else {
                        open.expanded_tool.insert(call_id);
                    }
                    return Err(error);
                }
                Ok(json!({"expanded": expanded}))
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
                    open.output.len() < 8
                        && !open.output.contains_key(&document)
                        && document != open.transcript_id,
                    "tool document admission is full or identity is already used"
                );
                let source = open
                    .tool
                    .get(&call_id)
                    .context("saved tool output is unavailable")?
                    .clone();
                let output = OutputDocument::new(document.clone(), source)?;
                ensure!(
                    open.output
                        .values()
                        .map(OutputDocument::retained_bytes)
                        .sum::<usize>()
                        + output.retained_bytes()
                        <= MAX_OUTPUT_VIEW_BYTES,
                    "tool source admission exceeds 96 MiB"
                );
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
                let retained = open
                    .output
                    .values()
                    .map(OutputDocument::retained_bytes)
                    .sum::<usize>();
                let output = open
                    .output
                    .get_mut(&document)
                    .context("unknown tool output document")?;
                Ok(to_value(output.demand(
                    revision,
                    MAX_OUTPUT_VIEW_BYTES.saturating_sub(retained),
                )?)?)
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

    pub fn reconcile(&mut self, entry: Vec<TimelineEntry>) -> Result<TimelinePatch> {
        let patch = self.timeline.reconcile(entry)?;
        self.project_patch(&patch);
        Ok(patch)
    }

    pub fn update_live(
        &mut self,
        interaction: Option<&Exchange>,
        removed: Option<&str>,
        status: SessionPhase,
    ) -> Result<TimelinePatch> {
        let patch = self.timeline.update_live(interaction, removed, status)?;
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
        let mut retained_bytes = 0;
        for (index, entry) in self.timeline.entry_list().iter().enumerate() {
            let entry = project(entry, &width, index > 0, &HashSet::new())?;
            retained_bytes += entry.retained_bytes();
            ensure!(
                retained_bytes <= MAX_PROJECTED_BYTES,
                "session projection exceeds 64 MiB"
            );
            projected.push(entry);
        }
        let mut entry = HashMap::new();
        let mut action = HashMap::new();
        let mut syntax = HashMap::new();
        let mut tool = HashMap::new();
        let mut block = Vec::new();
        for mut projected in projected {
            for tool in projected.tool.values_mut() { tool.own(&projected.entry.id); }
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
        let mut sections = super::sections::SectionProjection::new(&mut transcript, document.clone())?;
        sections.register(view.clone());
        let opened = PresentationOpen {
            syntax_pending: !syntax.is_empty()
                || action
                    .values()
                    .any(|action| matches!(action, TranscriptAction::Diff { .. })),
            transcript: sections.document.snapshot()?,
        };
        self.open = Some(OpenPresentation {
            expanded_tool: HashSet::new(),
            syntax_done: HashSet::new(),
            syntax,
            agent_scope: None,
            last_width: width,
            transcript,
            sections,
            transcript_id: document,
            submission_sequence: 0,
            pending_submission: None,
            accepted_submission: None,
            entry,
            action,
            tool,
            output: HashMap::new(),
            pending: VecDeque::new(),
            pending_bytes: 0,
            retained_bytes,
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
            && self.timeline.ticking_entries().next().is_some() {
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
                TranscriptAction::Diff { text } if !open.syntax_done.contains(target) => Some(
                    super::syntax::TranscriptSyntax::Diff(super::syntax::SavedDiffSyntax {
                        target: target.clone(),
                        text: text.clone(),
                    }),
                ),
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
                matches!(open.action.get(&job.target), Some(TranscriptAction::Diff { text }) if text == &job.text)
            }
            super::syntax::TranscriptSyntax::Markdown(job) => {
                open.syntax.get(&job.target) == Some(job)
            }
        };
        if !current_source {
            return Ok(());
        }
        let mut edits = Vec::new();
        let mut additional = 0;
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
            additional += replacement
                .retained_bytes()
                .saturating_sub(current.retained_bytes());
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
        ensure!(
            open.retained_bytes + additional <= MAX_PROJECTED_BYTES,
            "session syntax projection exceeds 64 MiB"
        );
        if let Some(patch) = open.transcript.document.edit_many(edits)? {
            open.retained_bytes += additional;
            if let Some(entry) = open.entry.values_mut().find(|entry| {
                entry.target.contains(job.target()) || entry.syntax.contains(job.target())
            }) {
                entry.bytes += additional;
            }
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
            .sections.document
            .document
            .block(&input.block)
            .context("transcript input block disappeared")?;
        let stable_tool = input.action == "activate"
            && input.target.as_ref().is_some_and(|target| {
                matches!(open.action.get(target), Some(TranscriptAction::Tool { .. }))
                    && block.metadata.target.iter().any(|candidate| &candidate.id == target)
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
        ensure!(
            text.len() <= 65536 && text.split('\n').count() <= 4096 && !text.trim().is_empty(),
            "prompt requires nonempty text within 64 KiB and 4096 rows"
        );
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
        let applied = if open.agent_scope.is_some() {
            if patch.operation.iter().all(|operation|matches!(operation,
                TimelineOperation::ToolOutput { .. } | TimelineOperation::Message { .. })
                || matches!(operation,TimelineOperation::Replace { entry:TimelineEntry::Status { .. },.. })) {
                let filtered = TimelinePatch { operation:patch.operation.iter().filter(|operation|match operation {
                    TimelineOperation::ToolOutput { call_id,.. } => open.tool.contains_key(call_id),
                    TimelineOperation::Message { block_id,message,.. } => message.kind() != crate::turn::MessageKind::Assistant
                        || open.transcript.document.block(&forge_buffer::identity::BlockId(block_id.clone())).is_some(),
                    _ => false,
                }).cloned().collect(),..patch.clone() };
                apply_projection(open,&filtered)
            } else { reflow(open, self.timeline.entry_list(), self.timeline.revision()) }
        } else if patch.operation.iter().any(|operation|match operation {
            TimelineOperation::ToolOutput { call_id,.. } => !open.tool.contains_key(call_id),
            TimelineOperation::Message { block_id,message,.. } => message.kind() == crate::turn::MessageKind::Assistant
                && open.transcript.document.block(&forge_buffer::identity::BlockId(block_id.clone())).is_none(),
            _ => false,
        }) {
            reflow(open,self.timeline.entry_list(),self.timeline.revision())
        } else {
            apply_projection(open, patch)
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
        matches!(action, TranscriptAction::Diff { .. }) && !open.syntax_done.contains(target)
    }) || open
        .syntax
        .keys()
        .any(|target| !open.syntax_done.contains(target))
}

fn entry_presentation(projected: &ProjectedEntry) -> EntryPresentation {
    EntryPresentation {
        prompt: projected.prompt.clone(),
        target: projected.action.keys().cloned().collect(),
        syntax: projected.syntax.keys().cloned().collect(),
        tool: projected.tool.keys().cloned().collect(),
        bytes: projected.retained_bytes(),
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

fn refresh_activity<'source>(open: &mut OpenPresentation,
    source: impl IntoIterator<Item=&'source TimelineEntry>) -> Result<()> {
    let width = open.transcript.views.profile().unwrap_or(&open.last_width).clone();
    let mut blocks = Vec::new();
    let mut retained = open.retained_bytes;
    for entry in source {
        for block in super::projection::timing_blocks(entry,&width,
            &open.transcript.document,&open.expanded_tool,&open.tool)? {
            let previous = open.transcript.document.block(&block.id)
                .context("timing block is missing")?.retained_bytes();
            retained = retained.saturating_sub(previous) + block.retained_bytes();
            ensure!(retained <= MAX_PROJECTED_BYTES, "session projection exceeds 64 MiB");
            blocks.push((entry.id(), previous, block));
        }
    }
    let mut patches = Vec::new();
    for (entry_id, previous, block) in blocks {
        let added = block.retained_bytes();
        if let Some(patch) = open.transcript.append(block)? { patches.push(patch); }
        if let Some(entry) = open.entry.get_mut(&entry_id) {
            entry.bytes = entry.bytes.saturating_sub(previous) + added;
        }
    }
    open.retained_bytes = retained;
    retain_patches(open,patches)
}

fn toggle_inline_output(open: &mut OpenPresentation, call_id: &str, expanded: bool) -> Result<()> {
    use forge_buffer::identity::BlockId;
    let source = open.tool.get(call_id).context("inline tool source is missing")?;
    let owner = source.owner().to_owned();
    let call = source.header().context("inline tool heading source is missing")?;
    let heading_id = BlockId(format!("{call_id}:tool"));
    let previous_heading = open.transcript.document.block(&heading_id).context("inline heading is missing")?;
    let layout = previous_heading.metadata.layout.clone();
    let mut width = open.transcript.views.profile().unwrap_or(&open.last_width).clone();
    width.columns = width.columns.saturating_sub(layout.as_ref().map_or(0, |layout|layout.indent.saturating_sub(2))).max(1);
    let renderer = super::transcript::TranscriptRenderer::new(&width)?;
    let now = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH)?.as_millis() as i64;
    let mut heading = renderer.tool_header(heading_id,previous_heading.metadata.target[0].id.clone(),
        &call.kind,call.elapsed_ms(now),call.failed,&call.title,expanded)?;
    heading.metadata.layout = layout.clone();
    heading.metadata.fold = previous_heading.metadata.fold.clone();
    let chunks = if expanded { source.inline_chunks() } else { 1 };
    let removed_chunks = if expanded { 1 } else { source.inline_chunks() };
    let identity = |chunk| BlockId(if chunk == 0 { format!("{call_id}:preview") } else { format!("{call_id}:output:{chunk}") });
    let mut removed = previous_heading.retained_bytes();
    let mut retired_target = HashSet::new();
    for chunk in 0..removed_chunks {
        let block = open.transcript.document.block(&identity(chunk)).context("inline body is missing")?;
        removed += block.retained_bytes();
        retired_target.extend(block.metadata.target.iter().map(|target|target.id.clone()));
    }
    let hidden_id = BlockId(format!("{call_id}:hidden"));
    let previous_hidden = open.transcript.document.block(&hidden_id).context("inline hidden counter is missing")?;
    removed += previous_hidden.retained_bytes();
    retired_target.extend(previous_hidden.metadata.target.iter().map(|target|target.id.clone()));
    let mut body = Vec::new();
    for chunk in 0..chunks {
        let mut block = renderer.tool_body(call_id,source,expanded,chunk)?;
        block.metadata.layout = layout.clone();
        body.push(block);
    }
    let mut hidden = renderer.tool_hidden(call_id,source,expanded)?;
    hidden.metadata.layout = layout;
    let added = heading.retained_bytes() + hidden.retained_bytes() + body.iter().map(forge_buffer::block::BufferBlock::retained_bytes).sum::<usize>();
    let retained = open.retained_bytes.saturating_sub(removed) + added;
    ensure!(retained <= MAX_PROJECTED_BYTES, "inline output exceeds projection capacity");
    let target = body.iter().chain(std::iter::once(&hidden)).flat_map(|block|block.metadata.target.iter().map(|target|target.id.clone())).collect::<Vec<_>>();
    let changes = vec![TranscriptChange::Block { block:heading },
        TranscriptChange::ToolBody { owner:owner.clone(),anchor:identity(0),removed:removed_chunks,block:body },
        TranscriptChange::Block { block:hidden }];
    let patches = match open.transcript.tool_layout(changes) {
        Ok(patches) => patches,
        Err(failure) => { open.failure = Some(format!("{failure:#}")); return Err(failure); }
    };
    for target in &retired_target { open.action.remove(target); }
    for target in &target { open.action.insert(target.clone(),TranscriptAction::Tool { call_id:call_id.to_owned() }); }
    let entry = open.entry.get_mut(&owner).context("inline entry owner is missing")?;
    entry.target.retain(|target|!retired_target.contains(target));
    entry.target.extend(target);
    entry.bytes = entry.bytes.saturating_sub(removed) + added;
    open.retained_bytes = retained;
    retain_patches(open,patches)
}

fn reflow(
    open: &mut OpenPresentation,
    source: &[TimelineEntry],
    timeline_revision: u64,
) -> Result<()> {
    let source = match open.agent_scope.as_deref() {
        None => std::borrow::Cow::Borrowed(source),
        Some(run_id) => std::borrow::Cow::Owned(
            find_agent(source, run_id, 0)
                .into_iter()
                .flat_map(|entry| match entry {
                    TimelineEntry::AgentLifecycle { exchange, .. } => exchange.iter(),
                    _ => unreachable!("agent lookup returns an agent lifecycle"),
                })
                .map(|interaction| TimelineEntry::Exchange {
                    id: interaction.id.clone(),
                    created_at_ms: interaction.created_at_ms,
                    exchange: interaction.clone(),
                    agent_by_id: HashMap::new(),
                })
                .collect::<Vec<_>>(),
        ),
    };
    let width = open
        .transcript
        .views
        .profile()
        .unwrap_or(&open.last_width)
        .clone();
    let mut projected = Vec::new();
    let mut retained = 0;
    for (index, entry) in source.iter().enumerate() {
        let entry = project(entry, &width, index > 0, &open.expanded_tool)?;
        retained += entry.retained_bytes();
        ensure!(
            retained <= MAX_PROJECTED_BYTES,
            "session projection exceeds 64 MiB"
        );
        projected.push(entry);
    }
    let mut entry = HashMap::new();
    let mut action = HashMap::new();
    let mut syntax = HashMap::new();
    let mut tool = HashMap::new();
    let mut blocks = Vec::new();
    for mut projection in projected {
        for tool in projection.tool.values_mut() { tool.own(&projection.entry.id); }
        entry.insert(projection.entry.id.clone(), entry_presentation(&projection));
        action.extend(projection.action);
        syntax.extend(projection.syntax);
        tool.extend(projection.tool);
        blocks.push(projection.entry);
    }
    let patch = open.transcript.replace_scope(blocks, timeline_revision)?;
    for (id, output) in &mut tool {
        if let Some(mut previous) = open.tool.remove(id) {
            previous.retain(output);
            *output = previous;
        }
    }
    open.syntax_done.clear();
    open.entry = entry;
    open.action = action;
    open.syntax = syntax;
    open.tool = tool;
    open.retained_bytes = retained;
    open.last_width = width;
    retain_patches(open, patch)
}

fn apply_projection(open: &mut OpenPresentation, patch: &TimelinePatch) -> Result<()> {
    let width = open.transcript.views.profile().unwrap_or(&open.last_width);
    let mut projected = Vec::new();
    let mut changed = Vec::new();
    let mut retained = open.retained_bytes;
    for operation in &patch.operation {
        match operation {
            TimelineOperation::Message { entry_id, exchange_id, block_id, message, .. } => {
                if message.kind() != crate::turn::MessageKind::Assistant { continue; }
                let block_id = forge_buffer::identity::BlockId(block_id.clone());
                let previous = open.transcript.document.block(&block_id).context("streamed message block is missing")?;
                let replacement = super::projection::message_block(previous,message,width)?;
                let mut removed = previous.retained_bytes();
                let previous_targets = previous.metadata.target.iter().map(|target|target.id.clone()).collect::<Vec<_>>();
                for target in &previous_targets {
                    if let Some(action) = open.action.remove(target) {
                        removed += match action { TranscriptAction::Diff { text } => text.len(), _ => 512 };
                    }
                    open.syntax_done.remove(target);
                }
                let syntax_target = TargetId(format!("{}:markdown-syntax",block_id.0));
                if let Some(syntax) = open.syntax.remove(&syntax_target) { removed += syntax.retained_bytes(); }
                open.syntax_done.remove(&syntax_target);
                let added = replacement.retained_bytes();
                retained = retained.saturating_sub(removed) + added;
                ensure!(retained <= MAX_PROJECTED_BYTES, "session projection exceeds 64 MiB");
                let owner = if open.agent_scope.is_some() { exchange_id } else { entry_id };
                if let Some(entry) = open.entry.get_mut(owner) {
                    entry.bytes = entry.bytes.saturating_sub(removed) + added;
                    entry.target.retain(|target|!previous_targets.contains(target));
                    entry.target.extend(replacement.action.keys().cloned());
                    entry.syntax.retain(|target|target != &syntax_target);
                    entry.syntax.extend(replacement.syntax.keys().cloned());
                }
                open.action.extend(replacement.action);
                open.syntax.extend(replacement.syntax);
                changed.push(TranscriptChange::Block { block:replacement.entry.block.into_iter().next().expect("message block") });
            }
            TimelineOperation::ToolOutput { entry_id, exchange_id, call_id, delta, .. } => {
                let source = open.tool.get_mut(call_id).context("streamed tool cache is missing")?;
                let before = source.retained_bytes();
                let expanded = open.expanded_tool.contains(call_id);
                let old_chunks = if expanded { source.inline_chunks() } else { 1 };
                let first_chunk = if expanded { source.inline_tail() } else { 0 };
                source.append(delta)?;
                let block_id = forge_buffer::identity::BlockId(format!("{call_id}:tool"));
                let previous = open.transcript.document.block(&block_id).context("streamed tool block is missing")?;
                let layout = previous.metadata.layout.clone();
                let mut content_width = width.clone();
                content_width.columns = content_width.columns.saturating_sub(
                    previous.metadata.layout.as_ref().map_or(0, |layout| layout.indent.saturating_sub(2))
                ).max(1);
                let renderer = super::transcript::TranscriptRenderer::new(&content_width)?;
                let owner = if open.agent_scope.is_some() { exchange_id } else { entry_id };
                let chunks = if expanded { source.inline_chunks() } else { 1 };
                let identity = |chunk| forge_buffer::identity::BlockId(if chunk == 0 { format!("{call_id}:preview") }
                    else { format!("{call_id}:output:{chunk}") });
                let mut removed = before;
                let mut block = Vec::new();
                for chunk in first_chunk..chunks {
                    let mut replacement = renderer.tool_body(call_id,source,expanded,chunk)?;
                    replacement.metadata.layout = layout.clone();
                    if let Some(layout) = &mut replacement.metadata.layout { layout.source_indent = 0; }
                    super::layout::materialize(&mut replacement)?;
                    let previous = open.transcript.document.block(&replacement.id);
                    let new_target = previous.is_none_or(|previous|previous.metadata.target.is_empty());
                    if let Some(previous) = previous {
                        removed += previous.retained_bytes();
                        for target in &previous.metadata.target { open.action.remove(&target.id); }
                    }
                    for target in &replacement.metadata.target {
                        open.action.insert(target.id.clone(),TranscriptAction::Tool { call_id:call_id.clone() });
                        if let Some(entry) = open.entry.get_mut(owner) {
                            if new_target { entry.target.push(target.id.clone()); }
                        }
                    }
                    block.push(replacement);
                }
                let added_body = block.iter().map(forge_buffer::block::BufferBlock::retained_bytes).sum::<usize>();
                let hidden_id = forge_buffer::identity::BlockId(format!("{call_id}:hidden"));
                let mut hidden = renderer.tool_hidden(call_id,source,expanded)?;
                hidden.metadata.layout = layout;
                if let Some(layout) = &mut hidden.metadata.layout { layout.source_indent = 0; }
                super::layout::materialize(&mut hidden)?;
                let previous_hidden = open.transcript.document.block(&hidden_id).context("tool hidden counter is missing")?;
                let new_hidden_target = previous_hidden.metadata.target.is_empty();
                removed += previous_hidden.retained_bytes();
                for target in &previous_hidden.metadata.target { open.action.remove(&target.id); }
                for target in &hidden.metadata.target {
                    open.action.insert(target.id.clone(),TranscriptAction::Tool { call_id:call_id.clone() });
                    if let Some(entry) = open.entry.get_mut(owner) {
                        if new_hidden_target { entry.target.push(target.id.clone()); }
                    }
                }
                let added = source.retained_bytes() + added_body + hidden.retained_bytes();
                retained = retained.saturating_sub(removed) + added;
                ensure!(retained <= MAX_PROJECTED_BYTES, "session projection exceeds 64 MiB");
                if let Some(entry) = open.entry.get_mut(owner) { entry.bytes = entry.bytes.saturating_sub(removed) + added; }
                if first_chunk < chunks {
                    changed.push(TranscriptChange::ToolBody { owner:owner.clone(),
                        anchor:if first_chunk < old_chunks { identity(first_chunk) } else { hidden_id },
                        removed:old_chunks.saturating_sub(first_chunk),block });
                }
                changed.push(TranscriptChange::Block { block:hidden });
            }
            TimelineOperation::Insert { index, entry }
            | TimelineOperation::Replace { index, entry } => {
                let entry = project(entry, width, *index > 0, &open.expanded_tool)?;
                retained = retained.saturating_sub(
                    open.entry
                        .get(&entry.entry.id)
                        .map_or(0, |entry| entry.bytes),
                ) + entry.retained_bytes();
                ensure!(
                    retained <= MAX_PROJECTED_BYTES,
                    "session projection exceeds 64 MiB"
                );
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
                retained =
                    retained.saturating_sub(open.entry.get(id).map_or(0, |entry| entry.bytes));
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
    for operation in &patch.operation {
        let id = match operation {
            TimelineOperation::ToolOutput { .. } | TimelineOperation::Message { .. } => continue,
            TimelineOperation::Insert { entry, .. } | TimelineOperation::Replace { entry, .. } => {
                entry.id()
            }
            TimelineOperation::Remove { id, .. } => id.clone(),
        };
        if let Some(previous) = open.entry.remove(&id) {
            for target in previous.target {
                open.syntax_done.remove(&target);
                open.action.remove(&target);
            }
            for call in previous.tool {
                open.tool.remove(&call);
            }
            for target in previous.syntax {
                open.syntax_done.remove(&target);
                open.syntax.remove(&target);
            }
        }
    }
    for (id, entry, action, mut tool, syntax) in projected {
        for tool in tool.values_mut() { tool.own(&id); }
        open.entry.insert(id, entry);
        open.action.extend(action);
        open.tool.extend(tool);
        open.syntax.extend(syntax);
    }
    open.retained_bytes = retained;
    retain_patches(open, patches)
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
        let bytes = serde_json::to_vec(&patch)?.len();
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
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn expand_sections(owner: &mut SessionPresentation, view: &str) -> Result<()> {
        let open = owner.open.as_mut().unwrap();
        let ids: Vec<_> = open.transcript.snapshot()?.block.iter()
            .flat_map(|block| block.metadata.fold.iter().map(|fold| fold.id.0.clone())).collect();
        for (index, id) in ids.iter().enumerate() {
            open.sections.set(&mut open.transcript, ViewId(view.into()), index as u64 + 1, id, true, false, None)?;
            open.sections.quota_for_test(id, 16 * 1024 * 1024);
        }
        retain_patches(open, Vec::new())
    }

    #[test]
    fn expanded_streaming_retains_settled_chunks_and_patches_only_the_mutable_tail() -> Result<()> {
        use crate::backend::{BackendEvent,ProviderAddress,ToolActivity,ToolActivityKind};
        use forge_buffer::identity::BlockId;
        let output = (0..30000).map(|row|format!("row {row} λ\n")).collect::<String>();
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["output"] = serde_json::json!(output);
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["status"] = serde_json::json!("inProgress");
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] = serde_json::Value::Null;
        let mut exchange:Exchange = serde_json::from_value(active["exchange"].take())?;
        let entry = TimelineEntry::Exchange { id:"active".into(),created_at_ms:0,exchange:exchange.clone(),agent_by_id:HashMap::new() };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry])?;
        let document = DocumentId("expanded-stream".into());
        owner.open(document.clone(),ViewId("view".into()),WidthProfile::default())?;
        owner.open.as_mut().unwrap().expanded_tool.insert("active:turn:1:tool".into());
        toggle_inline_output(owner.open.as_mut().unwrap(),"active:turn:1:tool",true)?;
        expand_sections(&mut owner, "view")?;
        let opened = owner.snapshot(&document)?;
        assert!(opened.block.iter().map(|block|block.text.row_count()).sum::<usize>() >= 30000);
        let heading = BlockId("active:turn:1:tool:tool".into());
        let settled = BlockId("active:turn:1:tool:preview".into());
        let pointer = |owner:&SessionPresentation,id:&BlockId|owner.open.as_ref().unwrap()
            .transcript.document.block(id).unwrap().text.row(0).unwrap().as_ptr() as usize;
        let heading_pointer = pointer(&owner,&heading);
        let settled_pointer = pointer(&owner,&settled);
        let mut revision = opened.revision;
        for iteration in 0..100 {
            let event = BackendEvent {
            received_at_ms: None, address:Some(ProviderAddress { thread_id:"thread".into(),turn_id:"active".into() }),
                turn_boundary:None,kind:"tool".into(),text:None,data:serde_json::json!({"emittedAtMs":10+iteration}),
                summary:None,task_update:None,activity:Some(ToolActivity { id:"tool".into(),kind:ToolActivityKind::Command,
                    title:"Read output".into(),output:Some(format!("new {iteration} 🦀\n")),status:Some("inProgress".into()),
                    output_delta:true,change:Default::default() }) };
            exchange.observe_turn(&event,10+iteration)?;
            owner.append_tool_output(&exchange,&event)?;
            let sync = owner.sync(&document,revision)?;
            assert!(sync.snapshot.is_none());
            for patch in sync.patch {
                assert!(patch.metadata_edit.iter().all(|edit|edit.block != heading && edit.block != settled));
                assert!(patch.text_edit.iter().map(|edit|edit.text.byte_count()).sum::<usize>() < 20000);
                revision = patch.next;
            }
            assert_eq!(pointer(&owner,&heading),heading_pointer);
            assert_eq!(pointer(&owner,&settled),settled_pointer);
        }
        assert!(owner.snapshot(&document)?.block.iter().any(|block|block.text.wire_rows().iter().any(|row|row.contains("new 99 🦀"))));
        refresh_activity(owner.open.as_mut().unwrap(), &[TimelineEntry::Exchange {
            id:"active".into(), created_at_ms:0, exchange, agent_by_id:HashMap::new(),
        }])?;
        let timed_heading = owner.open.as_ref().unwrap().transcript.document.block(&heading).unwrap().text.row(0).unwrap().to_owned();
        assert_eq!(pointer(&owner,&settled),settled_pointer);
        toggle_inline_output(owner.open.as_mut().unwrap(),"active:turn:1:tool",false)?;
        let collapsed_heading = owner.open.as_ref().unwrap().transcript.document.block(&heading).unwrap().text.row(0).unwrap();
        let column = |text: &str| WidthProfile::default().cells(&text[..text.find("Read output").unwrap()], 0).unwrap();
        assert_eq!(column(&timed_heading), column(collapsed_heading),
            "timer updates and activation must keep the command column fixed");
        Ok(())
    }

    #[test]
    fn streamed_message_reparses_only_its_owned_block_and_retains_tools() -> Result<()> {
        use crate::backend::{BackendEvent,ProviderAddress};
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        let mut exchange: Exchange = serde_json::from_value(active["exchange"].take())?;
        let mut event = BackendEvent {
            received_at_ms: None, address:Some(ProviderAddress { thread_id:"thread".into(),turn_id:"active".into() }),
            turn_boundary:None,kind:"assistant_message".into(),text:Some("First [link](https://example.com)".into()),
            data:serde_json::json!({"provider_message_id":"message"}),summary:None,task_update:None,activity:None };
        exchange.observe_turn(&event,2)?;
        let entry = TimelineEntry::Exchange { id:"active".into(),created_at_ms:0,exchange:exchange.clone(),agent_by_id:HashMap::new() };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("settled"),entry])?;
        let document = DocumentId("streamed-message".into());
        let opened = owner.open(document.clone(),ViewId("view".into()),WidthProfile::default())?;
        let tool = owner.open.as_ref().unwrap().tool["active:turn:1:tool"].preview(true).row.into_iter().map(str::to_owned).collect::<Vec<_>>();
        event.text = Some("\n\nMore **text** with λ.".into());
        exchange.observe_turn(&event,3)?;
        let patch = owner.update_message(&exchange,&event)?.expect("owned message");
        assert!(matches!(&patch.operation[..],[TimelineOperation::Message { index:1,.. }]));
        let sync = owner.sync(&document,opened.transcript.revision)?;
        assert!(sync.snapshot.is_none() && !sync.patch.is_empty());
        assert!(sync.patch.iter().all(|patch|patch.block_edit.is_empty()
            && patch.metadata_edit.iter().all(|edit|!edit.block.0.contains("settled") && !edit.block.0.ends_with(":tool"))));
        assert_eq!(owner.open.as_ref().unwrap().tool["active:turn:1:tool"].preview(true).row,tool);
        assert!(owner.snapshot(&document)?.block.iter().any(|block|block.text.wire_rows().iter().any(|row|row.contains("More **text**"))));
        Ok(())
    }

    #[test]
    fn streamed_output_changes_only_the_tool_and_its_fold_endpoints() -> Result<()> {
        use crate::backend::{BackendEvent,ProviderAddress,ToolActivity,ToolActivityKind};
        let mut active = serde_json::to_value(interaction_entry("active"))?;
        active["exchange"]["state"] = serde_json::json!("running");
        active["exchange"]["completed_at_ms"] = serde_json::Value::Null;
        active["exchange"]["execution_started_at_ms"] = serde_json::json!(1);
        active["exchange"]["turn"][0]["state"] = serde_json::json!({"kind":"running"});
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["status"] = serde_json::json!("inProgress");
        active["exchange"]["turn"][0]["tool"]["item"]["tool"]["completed_at_ms"] = serde_json::Value::Null;
        let active = TimelineEntry::Exchange { id:"active".into(),created_at_ms:0,
            exchange:serde_json::from_value(active["exchange"].take())?,agent_by_id:HashMap::new() };
        let TimelineEntry::Exchange { mut exchange, .. } = active.clone() else { unreachable!() };
        let mut entries: Vec<_> = (0..3000).map(|index| interaction_entry(&format!("history-{index}"))).collect();
        entries.push(active);
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(entries)?;
        let document = DocumentId("streamed-output".into());
        let opened = owner.open(document.clone(),ViewId("view".into()),WidthProfile::default())?;
        assert!(opened.transcript.block.iter().map(|block|block.text.row_count()).sum::<usize>() < 30000);
        assert!(!opened.transcript.block.iter().any(|block| block.id.0 == "history-0:turn:1:tool:preview"));
        let event = BackendEvent {
            received_at_ms: None, address:Some(ProviderAddress { thread_id:"thread".into(),turn_id:"active".into() }),
            turn_boundary:None,kind:"tool".into(),text:None,data:serde_json::json!({"emittedAtMs":10}),
            summary:None,task_update:None,activity:Some(ToolActivity { id:"tool".into(),
                kind:ToolActivityKind::Command,title:"Read output".into(),output:Some("\nseventh\n".into()),
                status:Some("inProgress".into()),output_delta:true,change:Default::default() }) };
        exchange.observe_turn(&event,10)?;
        let patch = owner.append_tool_output(&exchange,&event)?.expect("incremental output");
        assert!(matches!(&patch.operation[..],[TimelineOperation::ToolOutput { index:3000,.. }]));
        let sync = owner.sync(&document,opened.transcript.revision)?;
        assert!(sync.snapshot.is_none());
        assert!(!sync.patch.is_empty());
        for patch in &sync.patch {
            assert!(patch.block_edit.is_empty());
            assert!(patch.metadata_edit.iter().all(|metadata| !metadata.block.0.contains("history-")));
            assert!(patch.text_edit.iter().all(|edit| edit.removed_rows <= 1));
        }
        assert!(owner.open.as_ref().unwrap().tool["active:turn:1:tool"].preview(true).row.contains(&"seventh"));
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

    #[tokio::test]
    async fn saved_syntax_rejects_stale_and_closed_documents_and_survives_timer_refresh()
    -> Result<()> {
        use forge_diff::syntax::{SyntaxEngine};
        use forge_diff::workers::{AnalysisPool, PoolLimits};
        use std::sync::Arc;
        let engine = SyntaxEngine::new(
            Arc::new(AnalysisPool::new(PoolLimits {
                workers: 1,
                jobs: 2,
                input_bytes: 1024 * 1024,
            })),
        );
        let mut entry = interaction_entry("syntax");
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        exchange.attributed_diff_text = Some(
            "--- a/test.mjs\n+++ b/test.mjs\n@@ -1 +1 @@\n-const old = 1;\n+const next = 2;\n"
                .into(),
        );
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
        exchange.attributed_diff_text.as_mut().unwrap().push('\n');
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
            thread_id: "thread".into(),
            turn_id: identity.into(),
        };
        let mut interaction = Exchange {
            lifecycle: None,
            finalization_error: None,
            finalization_outcome: None,
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
        owner.initialize(vec![interaction_entry("first"), interaction_entry("second")])?;
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
            block: BlockId("first:turn:1:tool:preview".into()),
            position: TextPosition { row: 0, column: 8 },
            target: Some(TargetId("first:turn:1:tool:preview:tool".into())),
        };
        owner.dispatch(PresentationRequest::Resize {
            document: document.clone(),
            view,
            width: WidthProfile { columns: 65, ..WidthProfile::default() },
        })?;
        assert!(owner.snapshot(&document)?.revision > captured.revision);
        let mut changed = input.clone();
        changed.action = "navigate_prompt".into();
        assert!(owner.dispatch(PresentationRequest::NavigatePrompt {
            input: changed, previous: true,
        }).is_err());
        let mut changed = input.clone();
        changed.target = Some(TargetId("second:turn:1:tool:preview:tool".into()));
        assert!(owner.dispatch(PresentationRequest::ToggleTool { input: changed }).is_err());
        let mut changed = input.clone();
        changed.document = DocumentId("transcript:other".into());
        assert!(owner.dispatch(PresentationRequest::ToggleTool { input: changed }).is_err());
        let mut changed = input.clone();
        changed.revision = forge_buffer::identity::DocumentRevision(
            owner.snapshot(&document)?.revision.0 + 1,
        );
        assert!(owner.dispatch(PresentationRequest::ToggleTool { input: changed }).is_err());
        assert_eq!(owner.dispatch(PresentationRequest::ToggleTool {
            input: input.clone(),
        })?["expanded"], true);
        assert!(owner.dispatch(PresentationRequest::ToggleTool { input: input.clone() }).is_err());
        let mut next = input.clone();
        next.sequence = InputSequence(2);
        owner.dispatch(PresentationRequest::ToolOpen {
            input: next.clone(), document: DocumentId("output:stable-tool".into()),
        })?;
        owner.reconcile(vec![interaction_entry("second")])?;
        next.sequence = InputSequence(3);
        assert!(owner.dispatch(PresentationRequest::ToggleTool { input: next }).is_err());
        Ok(())
    }

    #[test]
    fn tool_preview_expands_from_output_and_survives_reflow() -> Result<()> {
        use forge_buffer::{block::TextPosition, identity::BlockId};
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("first")])?;
        let document = DocumentId("transcript:toggle".into());
        let view = ViewId("view:toggle".into());
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        expand_sections(&mut owner, "view:toggle")?;
        let opened = PresentationOpen { syntax_pending: false, transcript: owner.snapshot(&document)? };
        let block = BlockId("first:turn:1:tool:preview".into());
        let mut input = DocumentInput {
            document: document.clone(),
            revision: opened.transcript.revision,
            view: view.clone(),
            sequence: InputSequence(1),
            action: "activate".into(),
            block: block.clone(),
            position: TextPosition { row: 0, column: 8 },
            target: Some(TargetId(format!("{}:tool",block.0))),
        };
        assert_eq!(
            owner.dispatch(PresentationRequest::ToggleTool {
                input: input.clone()
            })?["expanded"],
            true
        );
        assert!(
            owner
                .dispatch(PresentationRequest::ToggleTool {
                    input: input.clone()
                })
                .is_err()
        );
        owner.dispatch(PresentationRequest::Resize {
            document: document.clone(),
            view,
            width: WidthProfile {
                columns: 65,
                ..WidthProfile::default()
            },
        })?;
        let snapshot = owner.snapshot(&document)?;
        let expanded = snapshot
            .block
            .iter()
            .find(|candidate| candidate.id == block)
            .unwrap();
        assert!(expanded.text.wire_rows().contains(&"    sixth"));
        input.revision = snapshot.revision;
        input.sequence = InputSequence(2);
        assert_eq!(
            owner.dispatch(PresentationRequest::ToggleTool { input })?["expanded"],
            false
        );
        let snapshot = owner.snapshot(&document)?;
        let collapsed = snapshot
            .block
            .iter()
            .find(|candidate| candidate.id == block)
            .unwrap();
        assert!(snapshot.block.iter().find(|block|block.id.0 == "first:turn:1:tool:hidden")
            .unwrap().text.wire_rows().contains(&"    …(2 hidden)"));
        assert!(!collapsed.text.wire_rows().contains(&"    sixth"));
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
                .any(|block| !block.metadata.fold.is_empty())
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
            let patch = owner.update_live(Some(&exchange), None, SessionPhase::Idle)?;
            assert_eq!(
                owner.timeline.entry_list().len(),
                1,
                "{nesting} duplicated its exchange"
            );
            assert!(
                matches!(&patch.operation[..], [TimelineOperation::Replace { entry, .. }] if entry.id() == owner_id)
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

            owner.update_live(None, Some(&exchange.id), SessionPhase::Idle)?;
            owner.sync(&document, snapshot.revision)?;
            assert_eq!(
                owner.timeline.entry_list().len(),
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
        owner.dispatch(PresentationRequest::SelectAgent {
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
        let event = crate::backend::BackendEvent {
            received_at_ms: None, address:Some(interaction.turn[0].provider().clone()),
            turn_boundary:None,kind:"tool".into(),text:None,data:serde_json::json!({"emittedAtMs":2}),
            activity:Some(crate::backend::ToolActivity { id:"tool".into(),kind:crate::backend::ToolActivityKind::Command,
                title:"Read output".into(),output:Some("\nscoped output".into()),status:Some("inProgress".into()),
                output_delta:true,change:Default::default() }),summary:None,task_update:None };
        interaction.observe_turn(&event,2)?;
        owner.append_tool_output(&interaction,&event)?.expect("scoped output");
        let update = owner.sync(&document,selected.revision)?;
        assert!(update.snapshot.is_none() && update.patch.iter().all(|patch|patch.block_edit.is_empty()));
        assert!(owner.open.as_ref().unwrap().tool["child:turn:1:tool"].preview(true).row.contains(&"scoped output"));
        owner.reconcile(vec![interaction_entry("other-main"), agent])?;
        assert_eq!(owner.open.as_ref().unwrap().pending_submission, Some(1));
        owner.dispatch(PresentationRequest::SelectAgent {
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
    fn timer_refresh_retains_completed_entry_allocations() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![
            interaction_entry("completed"),
            TimelineEntry::Status {
                id: "status".into(),
                created_at_ms: 0,
                status: SessionPhase::Working {
                    started_at_ms: 0,
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
                .begin_submission(&document, 1, &"x".repeat(65537))
                .is_err()
        );
        assert!(
            owner
                .begin_submission(&document, 1, &"\n".repeat(4096))
                .is_err()
        );
        owner.begin_submission(&document, 1, "complete λ\n\ndraft")?;
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
