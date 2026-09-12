use std::collections::{HashMap, VecDeque};

use anyhow::{Context, Result, ensure};
use forge_buffer::editable::{LocalEdit, LocalEditResult};
use forge_buffer::identity::{
    DocumentId, DocumentRevision, InputSequence, RegionRevision, TargetId, ViewId,
};
use forge_buffer::input::DocumentInput;
use forge_buffer::patch::{BufferPatch, BufferSnapshot};
use forge_buffer::width::WidthProfile;
use serde::{Deserialize, Serialize};

use crate::interaction::InteractionRecord;
use crate::session::state_machine::SessionPhase;
use crate::timeline::{
    TimelineEntry,
    stream::{TimelineOperation, TimelinePatch, TimelineStream},
};

use super::composer::{ComposerDocument, ComposerSubmission};
use super::document::{HarnessDocument, TranscriptChange};
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
}

struct OpenPresentation {
    agent_scope: Option<String>,
    last_width: WidthProfile,
    transcript: HarnessDocument,
    transcript_id: DocumentId,
    composer: ComposerDocument,
    composer_id: DocumentId,
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
    tool: Vec<String>,
    bytes: usize,
}

#[derive(Serialize)]
pub struct PresentationOpen {
    pub transcript: BufferSnapshot,
    pub composer: BufferSnapshot,
}

#[derive(Serialize)]
pub struct PresentationSync {
    pub patch: Vec<BufferPatch>,
    pub snapshot: Option<BufferSnapshot>,
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case")]
pub enum PresentationRequest {
    PlanOpen {
        document: DocumentId,
        view: ViewId,
        plan_id: String,
        digest: String,
        width: WidthProfile,
        saved_source_digest: Option<String>,
    },
    PlanAction {
        input: DocumentInput,
    },
    PlanAddAnnotation {
        input: DocumentInput,
    },
    PlanEdit {
        edit: LocalEdit,
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
        composer: DocumentId,
        view: ViewId,
        width: WidthProfile,
        initial: forge_buffer::text::BufferText,
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
    EditComposer {
        edit: LocalEdit,
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
    pub fn dispatch(&mut self, request: PresentationRequest) -> Result<serde_json::Value> {
        use serde_json::{json, to_value};
        match request {
            PresentationRequest::PlanOpen { .. }
            | PresentationRequest::PlanAction { .. }
            | PresentationRequest::PlanAddAnnotation { .. }
            | PresentationRequest::PlanEdit { .. }
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
                        && document != open.transcript_id
                        && document != open.composer_id,
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
            PresentationRequest::Open {
                document,
                composer,
                view,
                width,
                initial,
            } => Ok(to_value(
                self.open(document, composer, view, width, initial)?,
            )?),
            PresentationRequest::Sync { document, revision } => {
                Ok(to_value(self.sync(&document, revision)?)?)
            }
            PresentationRequest::Snapshot { document } => Ok(to_value(self.snapshot(&document)?)?),
            PresentationRequest::Input { input } => match self.action(input)? {
                TranscriptAction::Diff { .. } => Ok(json!({"kind":"diff"})),
                action => Ok(to_value(action)?),
            },
            PresentationRequest::EditComposer { edit } => Ok(match self.edit_composer(edit)? {
                LocalEditResult::Accepted {
                    acknowledgement,
                    patch,
                } => json!({"accepted":true,"acknowledgement":acknowledgement,"patch":patch}),
                LocalEditResult::Conflict { current } => {
                    json!({"accepted":false,"reason":"conflict","current":current})
                }
                LocalEditResult::UnknownRegion => {
                    json!({"accepted":false,"reason":"unknown_region"})
                }
                LocalEditResult::StaleSequence => {
                    json!({"accepted":false,"reason":"stale_sequence"})
                }
            }),
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
        interaction: Option<&InteractionRecord>,
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
        composer: DocumentId,
        view: ViewId,
        width: WidthProfile,
        initial: forge_buffer::text::BufferText,
    ) -> Result<PresentationOpen> {
        document.validate()?;
        composer.validate()?;
        ensure!(
            document != composer,
            "transcript and composer require independent document identities"
        );
        if let Some(open) = self.open.as_mut() {
            ensure!(
                open.transcript_id == document && open.composer_id == composer,
                "session presentation is already owned by another document lifetime"
            );
            if open.transcript.views.open(view.clone(), width)? {
                reflow(open, self.timeline.entry_list(), self.timeline.revision())?;
            }
            open.input.entry(view).or_insert(InputSequence(0));
            return Ok(PresentationOpen {
                transcript: open.transcript.snapshot()?,
                composer: open.composer.snapshot(),
            });
        }
        let composer_document = ComposerDocument::new(composer.clone(), initial)?;
        let mut projected = Vec::new();
        let mut retained_bytes = 0;
        for (index, entry) in self.timeline.entry_list().iter().enumerate() {
            let entry = project(entry, &width, index > 0)?;
            retained_bytes += entry.retained_bytes();
            ensure!(
                retained_bytes <= MAX_PROJECTED_BYTES,
                "session projection exceeds 64 MiB"
            );
            projected.push(entry);
        }
        let mut entry = HashMap::new();
        let mut action = HashMap::new();
        let mut tool = HashMap::new();
        let mut block = Vec::new();
        for projected in projected {
            entry.insert(projected.entry.id.clone(), entry_presentation(&projected));
            block.push(projected.entry);
            action.extend(projected.action);
            tool.extend(projected.tool);
        }
        let mut transcript = HarnessDocument::initialize(
            document.clone(),
            self.session_id.clone(),
            self.timeline.revision(),
            block,
        )?;
        transcript.views.open(view.clone(), width.clone())?;
        let opened = PresentationOpen {
            transcript: transcript.snapshot()?,
            composer: composer_document.snapshot(),
        };
        self.open = Some(OpenPresentation {
            agent_scope: None,
            last_width: width,
            transcript,
            transcript_id: document,
            composer: composer_document,
            composer_id: composer,
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
        if self.timeline.entry_list().iter().any(|entry| {
            matches!(
                entry,
                TimelineEntry::Status {
                    status: SessionPhase::Working { .. },
                    ..
                }
            )
        }) {
            let timeline = self.timeline.entry_list().to_vec();
            let timeline_revision = self.timeline.revision();
            let open = self
                .open
                .as_mut()
                .context("session presentation is not open")?;
            reflow(open, &timeline, timeline_revision)?;
        }
        let open = self.document(document)?;
        if let Some(failure) = &open.failure {
            anyhow::bail!("transcript projection requires reopening: {failure}");
        }
        let current = open.transcript.document.revision();
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
                patch,
                snapshot: None,
            })
        } else {
            Ok(PresentationSync {
                patch: Vec::new(),
                snapshot: Some(open.transcript.snapshot()?),
            })
        }
    }

    pub fn snapshot(&self, document: &DocumentId) -> Result<BufferSnapshot> {
        let open = self
            .open
            .as_ref()
            .context("session presentation is not open")?;
        if let Some(output) = open.output.get(document) {
            Ok(output.snapshot())
        } else if document == &open.composer_id {
            Ok(open.composer.snapshot())
        } else {
            self.document(document)?.transcript.snapshot()
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
                && input.revision == open.transcript.document.revision(),
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
            .transcript
            .document
            .block(&input.block)
            .context("transcript input block disappeared")?;
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

    pub fn edit_composer(&mut self, edit: LocalEdit) -> Result<LocalEditResult> {
        let open = self
            .open
            .as_mut()
            .context("session presentation is not open")?;
        ensure!(
            edit.document == open.composer_id,
            "composer edit belongs to another document"
        );
        open.composer.edit(edit)
    }

    pub fn begin_submission(
        &mut self,
        composer: &DocumentId,
        revision: RegionRevision,
    ) -> Result<ComposerSubmission> {
        let open = self
            .open
            .as_mut()
            .context("session presentation is not open")?;
        ensure!(
            &open.composer_id == composer,
            "composer submission belongs to another document"
        );
        open.composer.begin_submission(revision)
    }

    pub fn settle_submission(
        &mut self,
        composer: &DocumentId,
        token: u64,
        admitted: bool,
    ) -> Result<Option<BufferPatch>> {
        let open = self
            .open
            .as_mut()
            .context("session presentation is not open")?;
        ensure!(
            &open.composer_id == composer,
            "composer acknowledgement belongs to another document"
        );
        open.composer.settle_submission(token, admitted)
    }

    pub fn complete_submission(
        &mut self,
        composer: &DocumentId,
        token: u64,
        admitted: bool,
    ) -> Result<Option<BufferPatch>> {
        let Some(open) = self.open.as_mut() else {
            return Ok(None);
        };
        if &open.composer_id != composer || open.composer.pending_token() != Some(token) {
            return Ok(None);
        }
        open.composer.settle_submission(token, admitted)
    }

    pub fn retract_submission(
        &mut self,
        composer: &DocumentId,
        token: u64,
    ) -> Result<Option<BufferPatch>> {
        let Some(open) = self.open.as_mut() else {
            return Ok(None);
        };
        if &open.composer_id != composer {
            return Ok(None);
        }
        open.composer.retract_submission(token)
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
            reflow(open, self.timeline.entry_list(), self.timeline.revision())
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

fn entry_presentation(projected: &ProjectedEntry) -> EntryPresentation {
    EntryPresentation {
        prompt: projected.prompt.clone(),
        target: projected.action.keys().cloned().collect(),
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
            TimelineEntry::Interaction { agent_by_id, .. } => {
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
                    TimelineEntry::AgentLifecycle { interaction, .. } => interaction.iter(),
                    _ => unreachable!("agent lookup returns an agent lifecycle"),
                })
                .map(|interaction| TimelineEntry::Interaction {
                    id: interaction.id.clone(),
                    created_at_ms: interaction.created_at_ms,
                    interaction: interaction.clone(),
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
        let entry = project(entry, &width, index > 0)?;
        retained += entry.retained_bytes();
        ensure!(
            retained <= MAX_PROJECTED_BYTES,
            "session projection exceeds 64 MiB"
        );
        projected.push(entry);
    }
    let mut entry = HashMap::new();
    let mut action = HashMap::new();
    let mut tool = HashMap::new();
    let mut blocks = Vec::new();
    for projection in projected {
        entry.insert(projection.entry.id.clone(), entry_presentation(&projection));
        action.extend(projection.action);
        tool.extend(projection.tool);
        blocks.push(projection.entry);
    }
    let patch = open.transcript.replace_scope(blocks, timeline_revision)?;
    for (id, output) in &mut tool {
        if let Some(previous) = open.tool.remove(id) {
            *output = previous;
        }
    }
    open.entry = entry;
    open.action = action;
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
            TimelineOperation::Insert { index, entry }
            | TimelineOperation::Replace { index, entry } => {
                let entry = project(entry, width, *index > 0)?;
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
                projected.push((entry.entry.id.clone(), info, entry.action, entry.tool));
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
            TimelineOperation::Insert { entry, .. } | TimelineOperation::Replace { entry, .. } => {
                entry.id()
            }
            TimelineOperation::Remove { id, .. } => id.clone(),
        };
        if let Some(previous) = open.entry.remove(&id) {
            for target in previous.target {
                open.action.remove(&target);
            }
            for call in previous.tool {
                open.tool.remove(&call);
            }
        }
    }
    for (id, entry, action, tool) in projected {
        open.entry.insert(id, entry);
        open.action.extend(action);
        open.tool.extend(tool);
    }
    open.retained_bytes = retained;
    retain_patches(open, patches)
}

fn retain_patches(open: &mut OpenPresentation, patches: Vec<BufferPatch>) -> Result<()> {
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
    use forge_buffer::text::BufferText;

    fn interaction_entry(identity: &str) -> TimelineEntry {
        use crate::interaction::{
            CompletedThought, CompletedTool, InteractionKind, InteractionNode, InteractionState,
            MainSegment,
        };
        let mut segment = MainSegment::running(format!("{identity}:segment"), 0);
        segment.thought.push(CompletedThought {
            id: format!("{identity}:thought"),
            text: "Completed thought\nwith details".into(),
            synthetic: false,
            tool: vec![CompletedTool {
                id: "tool".into(),
                kind: "command".into(),
                title: "Read output".into(),
                output: "first\nsecond\nthird".into(),
                status: "completed".into(),
                failed: false,
                change: Default::default(),
            }],
            started_at_ms: 0,
            completed_at_ms: 1,
            diff_text: None,
            task_id: None,
        });
        TimelineEntry::Interaction {
            id: identity.into(),
            created_at_ms: 0,
            agent_by_id: HashMap::new(),
            interaction: InteractionRecord {
                id: identity.into(),
                session_id: "session".into(),
                ordinal: 1,
                prompt: format!("Prompt {identity}"),
                kind: InteractionKind::Chat,
                plan_id: None,
                execution_id: None,
                state: InteractionState::Complete,
                checkpoint_before: None,
                checkpoint_after: None,
                attributed_diff_text: None,
                checkpoint_diff_text: None,
                attributed_matches_checkpoint: false,
                created_at_ms: 0,
                completed_at_ms: Some(1),
                node_list: vec![InteractionNode::MainSegment {
                    segment: Box::new(segment),
                }],
                awaiting_input: false,
                elicitation: None,
                duration_ms: 1,
                token_count: None,
                comment: Vec::new(),
                task: None,
            },
        }
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
        let opened = owner.open(
            document.clone(),
            DocumentId("composer:actions".into()),
            view.clone(),
            WidthProfile::default(),
            BufferText::from_rows([""])?,
        )?;
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
        input.sequence = InputSequence(2);
        input.block = BlockId("first:tool:tool".into());
        input.target = Some(TargetId("first:tool:tool".into()));
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
    fn selected_agent_updates_keep_the_composer_and_document_lifetime() -> Result<()> {
        use crate::agent::{AgentRun, AgentRunStatus};
        let TimelineEntry::Interaction { interaction, .. } = interaction_entry("child") else {
            unreachable!()
        };
        let agent = TimelineEntry::AgentLifecycle {
            id: "agent:child".into(),
            created_at_ms: 0,
            run: AgentRun {
                id: "child-run".into(),
                session_id: "session".into(),
                parent_interaction_id: None,
                parent_thread_id: None,
                provider_thread_id: None,
                active_turn_id: None,
                definition: "explorer".into(),
                nickname: None,
                task: "Inspect".into(),
                status: AgentRunStatus::Completed,
                created_at_ms: 0,
                updated_at_ms: 1,
            },
            interaction: vec![interaction],
            agent: Vec::new(),
        };
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("main"), agent.clone()])?;
        let document = DocumentId("transcript:scope".into());
        let composer = DocumentId("composer:scope".into());
        let initial = owner.open(
            document.clone(),
            composer.clone(),
            ViewId("view:scope".into()),
            WidthProfile::default(),
            BufferText::from_rows(["retained draft"])?,
        )?;
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
        owner.reconcile(vec![interaction_entry("other-main"), agent])?;
        assert_eq!(owner.snapshot(&composer)?, initial.composer);
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
    fn active_projection_and_composer_have_independent_revisions() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Status {
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::Idle,
        }])?;
        let document = DocumentId("transcript:test".into());
        let composer = DocumentId("composer:test".into());
        let opened = owner.open(
            document.clone(),
            composer.clone(),
            ViewId("view:test".into()),
            WidthProfile::default(),
            BufferText::from_rows(["draft"])?,
        )?;
        let composer_before = serde_json::to_value(&opened.composer)?;
        owner.reconcile(vec![TimelineEntry::Status {
            id: "status".into(),
            created_at_ms: 0,
            status: SessionPhase::WaitingForAgent { agent_count: 2 },
        }])?;
        let synced = owner.sync(&document, opened.transcript.revision)?;
        assert!(!synced.patch.is_empty());
        assert!(synced.snapshot.is_none());
        assert_eq!(
            serde_json::to_value(owner.snapshot(&composer)?)?,
            composer_before
        );
        assert_eq!(
            owner.begin_submission(&composer, RegionRevision(0))?.text,
            "draft"
        );
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
        let composer = DocumentId("composer:hidden".into());
        let opened = owner.open(
            document.clone(),
            composer.clone(),
            ViewId("view:old".into()),
            WidthProfile::default(),
            BufferText::from_rows(["draft"])?,
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
        assert_eq!(owner.snapshot(&composer)?, opened.composer);
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
            DocumentId("composer:first".into()),
            ViewId("view:first".into()),
            WidthProfile::default(),
            BufferText::from_rows([""])?,
        )?;
        assert!(
            owner
                .open(
                    DocumentId("transcript:other".into()),
                    DocumentId("composer:other".into()),
                    ViewId("view:other".into()),
                    WidthProfile::default(),
                    BufferText::from_rows(["newer draft"])?
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
        let composer = DocumentId("composer:width".into());
        let first = ViewId("view:first".into());
        let second = ViewId("view:second".into());
        owner.open(
            document.clone(),
            composer.clone(),
            first.clone(),
            WidthProfile::default(),
            BufferText::from_rows([""])?,
        )?;
        let timeline_revision = owner.revision();
        let initial = owner.snapshot(&document)?.revision;
        owner.open(
            document.clone(),
            composer,
            second.clone(),
            WidthProfile {
                columns: 4,
                ..WidthProfile::default()
            },
            BufferText::from_rows([""])?,
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
