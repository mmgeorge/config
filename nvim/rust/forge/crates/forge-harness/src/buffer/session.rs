use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;

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
use super::projection::{ProjectedEntry, ProjectionSource, TranscriptAction, project};

const MAX_PENDING_BYTES: usize = 2 * 1024 * 1024;
/// Elapsed labels show whole seconds, so a faster refresh would publish identical text.
/// Elapsed labels show whole seconds, so no label waits longer than this for its next value.
const ACTIVITY_LABEL_INTERVAL_MS: i64 = 1_000;

pub(crate) mod diff_view;

pub struct SessionPresentation {
    session_id: String,
    timeline: TimelineStream,
    open: Option<OpenPresentation>,
    /// Supplies one presentation time to elapsed labels and the activity refresh cadence.
    clock: Arc<dyn crate::broker::Clock>,
    /// The instant at which a running elapsed label next shows a new value, measured at each
    /// synchronization; `None` while no label clock runs.
    activity_due_ms: Option<i64>,
}

struct OpenPresentation {
    diff: diff_view::TimelineDiffStore,
    status: super::status::StatusDocument,
    display: super::identity::DisplayRegistry,
    history_input_id: HashSet<String>,
    /// Acknowledged steering whose provider has not reported consumption, in timeline order.
    awaiting_steering: Vec<PresentedInput>,
    unsettled_exchange: Vec<String>,
    syntax_done: HashSet<TargetId>,
    /// Targets captured for analysis whose highlight has not been applied yet.
    syntax_in_flight: HashSet<TargetId>,
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
    queue: Vec<PresentedInput>,
    steering: Vec<PresentedInput>,
    /// Resume hint for the queue heading while delivery is paused.
    queue_paused: Option<String>,
}

/// Keep staging semantics identical to admitted input after decoding the editor payload.
#[derive(Clone, Debug, PartialEq, Eq)]
struct PresentedInput {
    id: Option<String>,
    text: String,
    failed: bool,
    failure_notice: Option<String>,
    content: crate::input::UserInputContent,
}

impl PresentedInput {
    fn resolve(input: StagedInput, questions: &crate::question::QuestionSnapshot) -> Result<Self> {
        match input {
            StagedInput::Text(text) => Ok(Self {
                id: None,
                content: crate::input::UserInputContent::UserText { text: text.clone() },
                text,
                failed: false,
                failure_notice: None,
            }),
            StagedInput::Message {
                id,
                text,
                failed,
                failure_notice,
                content,
            } => {
                let content = match content {
                    StagedContent::Message => {
                        crate::input::UserInputContent::UserText { text: text.clone() }
                    }
                    StagedContent::QuestionResponse { responses } => {
                        crate::question::input_content(
                            &forge_provider_api::forge::TurnInput::new(
                                forge_provider_api::forge::TurnKind::QuestionBatch { responses },
                            ),
                            &text,
                            questions,
                        )?
                    }
                };
                Ok(Self {
                    id: Some(id),
                    text,
                    failed,
                    failure_notice,
                    content,
                })
            }
        }
    }
}

/// Preserve input meaning separately from queue or steering delivery.
#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum StagedContent {
    Message,
    QuestionResponse {
        responses: Vec<forge_provider_api::QuestionReply>,
    },
}

#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum StagedInput {
    Text(String),
    Message {
        id: String,
        text: String,
        content: StagedContent,
        #[serde(default)]
        failed: bool,
        failure_notice: Option<String>,
    },
}

impl From<&str> for StagedInput {
    fn from(text: &str) -> Self {
        Self::Text(text.into())
    }
}

impl From<String> for StagedInput {
    fn from(text: String) -> Self {
        Self::Text(text)
    }
}

struct EntryPresentation {
    prompt: Vec<forge_buffer::identity::BlockId>,
    response: Vec<forge_buffer::identity::BlockId>,
    target: Vec<TargetId>,
    syntax: Vec<TargetId>,
    tool: Vec<super::identity::DisplayId>,
}

#[derive(Serialize)]
pub struct PresentationOpen {
    pub diff_view: forge_diff::view::DiffViewOptions,
    pub status: BufferSnapshot,
    pub syntax_pending: bool,
    pub transcript: BufferSnapshot,
    pub startup_timing: std::collections::BTreeMap<&'static str, f64>,
    /// Milliseconds until a running elapsed label changes, as in `PresentationSync`.
    pub activity_due_in_ms: Option<u64>,
}

#[derive(Serialize)]
pub struct PresentationSync {
    pub diff_view: forge_diff::view::DiffViewOptions,
    pub status: BufferSnapshot,
    pub syntax_pending: bool,
    pub patch: Vec<BufferPatch>,
    pub snapshot: Option<BufferSnapshot>,
    /// Milliseconds until a running elapsed label changes, absent while no clock runs. The
    /// editor synchronizes again after this delay, so a silent turn still counts every second.
    pub activity_due_in_ms: Option<u64>,
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case")]
pub enum PresentationRequest {
    DiffInput {
        input: DocumentInput,
    },
    StatusAction {
        document: DocumentId,
        status: DocumentId,
        revision: DocumentRevision,
        view: ViewId,
        sequence: InputSequence,
        target: TargetId,
    },
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
        queue: Vec<StagedInput>,
        #[serde(default)]
        steering: Vec<StagedInput>,
        /// Resume hint the editor shows with the queue heading while delivery is paused,
        /// empty when no key resumes it; absent while delivery runs.
        #[serde(default)]
        queue_paused: Option<String>,
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

impl OpenPresentation {
    fn tool_display(
        &self,
        entry: &str,
        exchange: &str,
        source: &crate::turn::ToolAddress,
    ) -> Option<super::identity::DisplayId> {
        self.display.tool(
            if self.agent_scope.is_some() {
                exchange
            } else {
                entry
            },
            source,
        )
    }

    fn document_mut<'presentation>(
        slot: &'presentation mut Option<Self>,
        document: &DocumentId,
    ) -> Result<&'presentation mut Self> {
        let open = slot.as_mut().context("session presentation is not open")?;
        ensure!(
            &open.transcript_id == document,
            "transcript document identity changed"
        );
        Ok(open)
    }
}
impl SessionPresentation {
    pub(crate) fn append_tool_output(
        &mut self,
        exchange: &Exchange,
        event: &crate::provider::ProviderEvent,
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
        event: &crate::provider::ProviderEvent,
    ) -> Result<Option<TimelinePatch>> {
        let patch = self.timeline.update_message(exchange, event)?;
        if let Some(patch) = &patch {
            self.project_patch(patch);
        }
        Ok(patch)
    }

    /// Capture visible context without acquiring the active prompt broker.
    pub(crate) fn conversation_history(&self) -> Result<String> {
        crate::provider::text_generation::history(self.timeline.entry_list())
    }
    pub(crate) fn dispatch(
        &mut self,
        request: PresentationRequest,
        questions: &crate::question::QuestionSnapshot,
    ) -> Result<serde_json::Value> {
        use serde_json::{json, to_value};
        let now_ms = self.clock.now_ms();
        match request {
            PresentationRequest::DiffInput { .. } => {
                anyhow::bail!("timeline diff controls require the asynchronous service owner")
            }
            PresentationRequest::StatusAction {
                document,
                status,
                revision,
                view,
                sequence,
                target,
            } => {
                let open = OpenPresentation::document_mut(&mut self.open, &document)?;
                let previous = open
                    .input
                    .get_mut(&view)
                    .context("unknown status action view")?;
                ensure!(sequence > *previous, "stale status action sequence");
                let action = open.status.action(&status, revision, &target)?;
                *previous = sequence;
                Ok(to_value(action)?)
            }
            PresentationRequest::Node { request } => {
                use super::nodes::NodeAction;
                let open = OpenPresentation::document_mut(&mut self.open, &request.document)?;
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
                if let Some(call) = super::identity::DisplayId::from_node(&request.node)
                    && open.sections.nodes.tool.contains_key(&call)
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
                    toggle_inline_output(open, &call, expanded, limit, now_ms)?;
                }
                open.sections.set(
                    &mut open.transcript,
                    crate::buffer::sections::SectionRequest {
                        view: request.view,
                        sequence: request.sequence,
                        id: &request.node,
                        expanded,
                        more,
                        page: Some((request.rows, request.width)),
                    },
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
                let open = OpenPresentation::document_mut(&mut self.open, &document)?;
                if open.transcript.views.open(view.clone(), width)? {
                    reflow(
                        open,
                        self.timeline.entry_list(),
                        self.timeline.revision(),
                        now_ms,
                    )?;
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
                    ensure!(!run_id.is_empty(), "invalid agent identity");
                    ensure!(
                        find_agent_exchanges(self.timeline.entry_list(), run_id).is_some(),
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
                if let Err(failure) = reflow(
                    open,
                    self.timeline.entry_list(),
                    self.timeline.revision(),
                    now_ms,
                ) {
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
                let source = tool.complete_snapshot()?;
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
                queue_paused,
            } => {
                self.set_staging(
                    &document,
                    sequence,
                    queue,
                    steering,
                    queue_paused,
                    questions,
                )?;
                Ok(json!({"updated":true}))
            }
            PresentationRequest::Snapshot { document } => {
                let mut snapshot = to_value(self.snapshot(&document)?)?;
                if let Some(open) = &self.open
                    && open.transcript_id == document
                {
                    snapshot["diff_view"] = to_value(open.diff.view.committed())?;
                }
                Ok(snapshot)
            }
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
                let open = OpenPresentation::document_mut(&mut self.open, &document)?;
                if open.transcript.views.resize(&view, width)? {
                    reflow(
                        open,
                        self.timeline.entry_list(),
                        self.timeline.revision(),
                        now_ms,
                    )?;
                }
                Ok(json!({"updated":true}))
            }
            PresentationRequest::CloseView { document, view } => {
                let open = OpenPresentation::document_mut(&mut self.open, &document)?;
                open.input.remove(&view);
                open.sections.close(&mut open.transcript, &view);
                retain_patches(open, Vec::new())?;
                if open.transcript.views.close(&view) && open.transcript.views.profile().is_some() {
                    reflow(
                        open,
                        self.timeline.entry_list(),
                        self.timeline.revision(),
                        now_ms,
                    )?;
                }
                Ok(json!({"closed":true}))
            }
        }
    }
    pub fn new(session_id: String) -> Self {
        Self::with_clock(session_id, Arc::new(crate::broker::SystemClock))
    }

    /// Render elapsed labels and pace activity refreshes from an injected clock.
    pub(crate) fn with_clock(session_id: String, clock: Arc<dyn crate::broker::Clock>) -> Self {
        Self {
            timeline: TimelineStream::new(session_id.clone()),
            session_id,
            open: None,
            clock,
            activity_due_ms: None,
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

    /// Replace one settled exchange inside the entry that owns it, keeping the session status.
    pub(crate) fn settle_owned_exchange(
        &mut self,
        exchange: &Exchange,
        status_thoughts: bool,
    ) -> Result<TimelinePatch> {
        let patch = self
            .timeline
            .settle_owned_exchange(exchange, status_thoughts)?;
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

    /// Get the newest presented exchange that has not finished, as the transcript draws it.
    pub(crate) fn unfinished_exchange(&self) -> Option<Exchange> {
        self.timeline
            .entry_list()
            .iter()
            .rev()
            .find_map(|entry| match entry {
                TimelineEntry::Exchange { exchange, .. } if !exchange.is_finished() => {
                    Some(exchange.as_ref().clone())
                }
                _ => None,
            })
    }

    pub fn open(
        &mut self,
        document: DocumentId,
        view: ViewId,
        width: WidthProfile,
    ) -> Result<PresentationOpen> {
        let now_ms = self.clock.now_ms();
        let started = std::time::Instant::now();
        let mut startup_timing = std::collections::BTreeMap::new();
        document.validate()?;
        if let Some(open) = self.open.as_mut() {
            ensure!(
                open.transcript_id == document,
                "session presentation is already owned by another document lifetime"
            );
            if open.transcript.views.open(view.clone(), width)? {
                reflow(
                    open,
                    self.timeline.entry_list(),
                    self.timeline.revision(),
                    now_ms,
                )?;
            }
            open.sections.register(view.clone());
            open.input.entry(view).or_insert(InputSequence(0));
            return Ok(PresentationOpen {
                diff_view: open.diff.view.committed(),
                status: open.status.snapshot(),
                syntax_pending: syntax_pending(open),
                transcript: open.sections.document.snapshot()?,
                startup_timing: std::collections::BTreeMap::from([(
                    "reuse_ms",
                    started.elapsed().as_secs_f64() * 1000.0,
                )]),
                activity_due_in_ms: self
                    .activity_due_ms
                    .map(|due_ms| due_ms.saturating_sub(now_ms).max(0) as u64),
            });
        }
        let mut projected = Vec::new();
        let mut status = super::status::StatusDocument::new(&document)?;
        let mut display = super::identity::DisplayRegistry::default();
        let unsettled = unsettled_exchanges(
            self.timeline
                .entry_list()
                .iter()
                .map(ProjectionSource::Entry),
        );
        let mut visible_entry = false;
        for entry in self.timeline.entry_list() {
            let mut entry = project(
                ProjectionSource::Entry(entry),
                &width,
                now_ms,
                visible_entry,
                &HashMap::new(),
                &HashMap::new(),
                &mut display,
            )?;
            if !unsettled.contains(&entry.entry.id) {
                settle_responses(&mut entry);
            }
            visible_entry |= entry
                .entry
                .block
                .iter()
                .any(|block| block.text.row_count() > 0);
            projected.push(entry);
        }
        startup_timing.insert(
            "entries_projected_ms",
            started.elapsed().as_secs_f64() * 1000.0,
        );
        let mut entry = HashMap::new();
        let mut action = HashMap::new();
        let mut syntax = HashMap::new();
        let mut tool = HashMap::new();
        let mut block = Vec::new();
        for mut projected in projected {
            status.apply(std::mem::take(&mut projected.status))?;
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
        startup_timing.insert(
            "transcript_created_ms",
            started.elapsed().as_secs_f64() * 1000.0,
        );
        let mut sections =
            super::sections::SectionProjection::new(&mut transcript, document.clone())?;
        sections.register(view.clone());
        sections.nodes.tool = tool;
        startup_timing.insert(
            "sections_created_ms",
            started.elapsed().as_secs_f64() * 1000.0,
        );
        let snapshot = sections.document.snapshot()?;
        startup_timing.insert(
            "snapshot_created_ms",
            started.elapsed().as_secs_f64() * 1000.0,
        );
        let syntax_pending = !syntax.is_empty();
        let mut open = OpenPresentation {
            diff: diff_view::TimelineDiffStore::default(),
            status,
            display,
            history_input_id: history_input_ids(self.timeline.entry_list()),
            awaiting_steering: awaiting_steering(self.timeline.entry_list()),
            unsettled_exchange: unsettled,
            syntax_done: HashSet::new(),
            syntax_in_flight: HashSet::new(),
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
        };
        // A reopened presentation shows steering still waiting for its provider, which no
        // editor staging describes anymore.
        update_staging_block(&mut open)?;
        self.activity_due_ms = activity_label_due(&open, self.timeline.ticking_entries(), now_ms);
        let opened = PresentationOpen {
            diff_view: forge_diff::view::DiffViewOptions::default(),
            status: open.status.snapshot(),
            syntax_pending,
            transcript: snapshot,
            startup_timing,
            activity_due_in_ms: self
                .activity_due_ms
                .map(|due_ms| due_ms.saturating_sub(now_ms).max(0) as u64),
        };
        self.open = Some(open);
        Ok(opened)
    }

    pub fn sync(
        &mut self,
        document: &DocumentId,
        revision: DocumentRevision,
    ) -> Result<PresentationSync> {
        if let Some(failure) = &self.document(document)?.failure {
            return Err(super::TranscriptProjectionFailure::new(failure.clone()).into());
        }
        let now_ms = self.clock.now_ms();
        let open = self
            .open
            .as_mut()
            .context("session presentation is not open")?;
        // Labels rendered with their entries are current until the measured instant. A clock set
        // backward cannot postpone the refresh past one label interval.
        if self.activity_due_ms.is_some_and(|due_ms| {
            now_ms >= due_ms || due_ms.saturating_sub(now_ms) > ACTIVITY_LABEL_INTERVAL_MS
        }) {
            refresh_activity(open, self.timeline.ticking_entries(), now_ms)?;
        }
        self.activity_due_ms = activity_label_due(open, self.timeline.ticking_entries(), now_ms);
        let activity_due_in_ms = self
            .activity_due_ms
            .map(|due_ms| due_ms.saturating_sub(now_ms).max(0) as u64);
        let open = self.document(document)?;
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
                diff_view: open.diff.view.committed(),
                status: open.status.snapshot(),
                syntax_pending: syntax_pending(open),
                patch,
                snapshot: None,
                activity_due_in_ms,
            })
        } else {
            Ok(PresentationSync {
                diff_view: open.diff.view.committed(),
                status: open.status.snapshot(),
                syntax_pending: syntax_pending(open),
                patch: Vec::new(),
                snapshot: Some(open.sections.document.snapshot()?),
                activity_due_in_ms,
            })
        }
    }

    fn set_staging(
        &mut self,
        document: &DocumentId,
        sequence: u64,
        queue: Vec<StagedInput>,
        steering: Vec<StagedInput>,
        queue_paused: Option<String>,
        questions: &crate::question::QuestionSnapshot,
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
        let queue = queue
            .into_iter()
            .map(|input| PresentedInput::resolve(input, questions))
            .collect::<Result<Vec<_>>>()?;
        let steering = steering
            .into_iter()
            .map(|input| PresentedInput::resolve(input, questions))
            .collect::<Result<Vec<_>>>()?;
        let staging = StagingContent {
            queue,
            steering,
            queue_paused,
        };
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
        let open = OpenPresentation::document_mut(&mut self.open, document)?;
        // Prefer targets no earlier capture has taken. A captured target stays unfinished until
        // `apply_syntax` settles it, so a failed analysis is retried by a later request once no
        // untaken work remains, without starving the targets behind it.
        let select = |include_in_flight: bool| {
            let eligible = |target: &TargetId| {
                !open.syntax_done.contains(target)
                    && (include_in_flight || !open.syntax_in_flight.contains(target))
            };
            open.action
                .iter()
                .find_map(|(target, action)| match action {
                    TranscriptAction::Diff {
                        text, syntax: true, ..
                    } if eligible(target)
                        && !open.diff.root.contains_key(target)
                        && open.sections.expansion(&target.0) == Some(true) =>
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
                        eligible(target)
                            .then(|| super::syntax::TranscriptSyntax::Markdown(job.clone()))
                    })
                })
        };
        let selected = select(false).or_else(|| select(true));
        if let Some(job) = &selected {
            open.syntax_in_flight.insert(job.target().clone());
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
                matches!(open.action.get(&job.target), Some(TranscriptAction::Diff { text, syntax: true, .. }) if text == &job.text)
            }
            super::syntax::TranscriptSyntax::Markdown(job) => {
                open.syntax.get(&job.target) == Some(job)
            }
        };
        if !current_source {
            return Ok(());
        }
        // A highlight that no longer lines up with the displayed blocks is settled as done, as an
        // applied one is; only a failure before this point leaves the target retryable.
        'apply: {
            let mut edits = Vec::new();
            for syntax in highlighted {
                let Some(index) = open.transcript.document.block_index(&syntax.id) else {
                    break 'apply;
                };
                let current = open
                    .transcript
                    .document
                    .blocks(open.transcript.document.revision(), index..index + 1)?
                    .next()
                    .context("highlighted block is unavailable")?;
                if current.text.row_count() != syntax.text.row_count() {
                    break 'apply;
                }
                let mut replacement = current.clone();
                for mut span in syntax.metadata.visible_decoration {
                    let row = span.range.start.row;
                    let Some((source, displayed)) = syntax.text.row(row).zip(current.text.row(row))
                    else {
                        break 'apply;
                    };
                    let Some(prefix) = displayed.strip_suffix(source) else {
                        break 'apply;
                    };
                    span.range.start.column += prefix.len();
                    span.range.end.column += prefix.len();
                    replacement.metadata.visible_decoration.push(span);
                }
                open.transcript.mark_block_dirty(&syntax.id);
                edits.push(forge_buffer::sequence::SequenceEdit {
                    range: index..index + 1,
                    block: vec![replacement],
                });
            }
            if let Some(patch) = open.transcript.document.edit_many(edits)? {
                retain_patches(open, vec![patch])?;
            }
        }
        open.syntax_in_flight.remove(job.target());
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
        } else if open.status.id() == document {
            Ok(open.status.snapshot())
        } else {
            self.document(document)?.sections.document.snapshot()
        }
    }

    pub fn action(&mut self, input: DocumentInput) -> Result<TranscriptAction> {
        let open = self.validate_input(&input)?;
        let target = input
            .target
            .as_ref()
            .context("transcript input has no action")?;
        let action = open
            .diff
            .action(target)
            .or_else(|| open.action.get(target).cloned())
            .context("transcript target is not actionable")?;
        if matches!(action, TranscriptAction::File { .. })
            && let Some(captured) = diff_view::captured_file_action(open, &input.block)
        {
            return Ok(captured);
        }
        Ok(action)
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
        // A tool row and a spawn row keep their target across revisions: the block that carries
        // the target still names the same call or child, so activating it from an older
        // revision opens what the user saw. Running children revise the transcript several
        // times a second, so a spawn row's revision was often stale by the time the input arrived.
        let stable_target = input.action == "activate"
            && input.target.as_ref().is_some_and(|target| {
                matches!(
                    open.action.get(target),
                    Some(TranscriptAction::Tool { .. } | TranscriptAction::Agent { .. })
                ) && block
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
            ensure!(stable_target, "transcript input revision changed");
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
        ensure!(!text.trim().is_empty(), "prompt requires nonempty text");
        let open = OpenPresentation::document_mut(&mut self.open, document)?;
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
        if let Some(open) = self.open.as_mut()
            && let Some(output) = open.output.get_mut(document)
        {
            output.close()?;
            open.output.remove(document);
            return Ok(());
        }
        let open = OpenPresentation::document_mut(&mut self.open, document)?;
        for output in open.output.values_mut() {
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
        trace: std::sync::Arc<crate::trace::TraceStore>,
        session_id: &str,
    ) -> Result<std::path::PathBuf> {
        self.open
            .as_mut()
            .context("session presentation is not open")?
            .output
            .get_mut(document)
            .context("unknown tool output document")?
            .export(directory, trace, session_id)
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
        let now_ms = self.clock.now_ms();
        let Some(open) = self.open.as_mut() else {
            return;
        };
        if open.failure.is_some() {
            return;
        }
        if patch
            .operation
            .iter()
            .any(|operation| matches!(operation, TimelineOperation::Remove { .. }))
        {
            open.history_input_id = history_input_ids(self.timeline.entry_list());
        } else {
            for operation in &patch.operation {
                if let TimelineOperation::Insert { entry, .. }
                | TimelineOperation::Replace { entry, .. } = operation
                {
                    open.history_input_id
                        .extend(history_input_ids(std::slice::from_ref(entry)));
                }
            }
        }
        open.awaiting_steering = awaiting_steering(self.timeline.entry_list());
        let unsettled = if let Some(run_id) = open.agent_scope.as_deref() {
            unsettled_exchanges(
                find_agent_exchanges(self.timeline.entry_list(), run_id)
                    .into_iter()
                    .flatten()
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
                        TimelineOperation::ToolOutput {
                            entry_id,
                            exchange_id,
                            call,
                            ..
                        } => open
                            .tool_display(entry_id, exchange_id, call)
                            .is_some_and(|id| open.sections.nodes.tool.contains_key(&id)),
                        TimelineOperation::Message { block_id, .. } => open
                            .transcript
                            .document
                            .block(&forge_buffer::identity::BlockId(block_id.clone()))
                            .is_some(),
                        _ => false,
                    })
                    .collect();
                apply_projection(open, patch, filtered.iter().copied(), &unsettled, now_ms)
            } else {
                reflow(
                    open,
                    self.timeline.entry_list(),
                    self.timeline.revision(),
                    now_ms,
                )
            }
        } else if patch.operation.iter().any(|operation| match operation {
            TimelineOperation::ToolOutput {
                entry_id,
                exchange_id,
                call,
                ..
            } => open
                .tool_display(entry_id, exchange_id, call)
                .is_none_or(|id| !open.sections.nodes.tool.contains_key(&id)),
            // Reasoning has no transcript block: its summary reaches the status row through its
            // own `Replace` operation. Only prose whose block is missing needs the reflow that
            // creates it, so a reasoning delta must not re-project every entry of the session.
            TimelineOperation::Message {
                block_id, message, ..
            } => {
                streams_into_transcript(message)
                    && open
                        .transcript
                        .document
                        .block(&forge_buffer::identity::BlockId(block_id.clone()))
                        .is_none()
            }
            _ => false,
        }) {
            reflow(
                open,
                self.timeline.entry_list(),
                self.timeline.revision(),
                now_ms,
            )
        } else {
            apply_projection(open, patch, patch.operation.iter(), &unsettled, now_ms)
        };
        if let Err(error) = applied {
            open.failure = Some(format!("{error:#}"));
            open.pending.clear();
            open.pending_bytes = 0;
        }
    }
}

/// Whether each delta of `message` updates a transcript block.
///
/// A provider that cannot tell commentary from the response until the turn ends streams its prose
/// unclassified. That prose is as visible as classified prose, so both kinds update their block;
/// excluding it froze the transcript at the first delta until the turn completed. Reasoning never
/// has a block of its own.
fn streams_into_transcript(message: &crate::turn::Message) -> bool {
    matches!(
        message.kind(),
        crate::turn::MessageKind::Assistant | crate::turn::MessageKind::Unknown
    )
}

/// Identify unanalysed sources without inspecting their potentially large bodies.
fn syntax_pending(open: &OpenPresentation) -> bool {
    open.action.iter().any(|(target, action)| {
        matches!(action, TranscriptAction::Diff { syntax: true, .. })
            && !open.syntax_done.contains(target)
            && !open.diff.root.contains_key(target)
            && open.sections.expansion(&target.0) == Some(true)
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
            Some((id, exchange.as_ref()))
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

fn find_agent_exchanges<'source>(
    source: &'source [TimelineEntry],
    run_id: &str,
) -> Option<&'source [Exchange]> {
    let mut pending = source.iter().rev().collect::<Vec<_>>();
    while let Some(entry) = pending.pop() {
        match entry {
            TimelineEntry::AgentLifecycle {
                run,
                exchange,
                agent,
                ..
            } => {
                if run.id == run_id {
                    return Some(exchange);
                }
                pending.extend(agent.iter().rev());
            }
            TimelineEntry::Exchange { agent_by_id, .. } => {
                pending.extend(agent_by_id.values().collect::<Vec<_>>().into_iter().rev());
            }
            _ => {}
        }
    }
    None
}

/// Find when a running elapsed label of `source` next shows a new value.
fn activity_label_due<'source>(
    open: &OpenPresentation,
    source: impl IntoIterator<Item = &'source TimelineEntry>,
    now_ms: i64,
) -> Option<i64> {
    let mut due = super::duration::LabelDue::default();
    for entry in source {
        if open.agent_scope.is_none() {
            super::status::StatusProjection::label_due(entry, now_ms, &mut due);
        }
        super::projection::timing_label_due(
            entry,
            now_ms,
            &open.sections.nodes.tool,
            &open.display,
            &mut due,
        );
    }
    due.due_ms()
}

fn refresh_activity<'source>(
    open: &mut OpenPresentation,
    source: impl IntoIterator<Item = &'source TimelineEntry>,
    now_ms: i64,
) -> Result<()> {
    let width = open
        .transcript
        .views
        .profile()
        .unwrap_or(&open.last_width)
        .clone();
    let mut blocks = Vec::new();
    let mut status = Vec::new();
    for entry in source {
        if open.agent_scope.is_none() {
            status.push(super::status::StatusProjection::render(
                entry, &width, now_ms,
            )?);
        }
        for block in super::projection::timing_blocks(
            entry,
            &width,
            now_ms,
            &open.transcript.document,
            &open.sections.nodes.choice,
            &open.sections.nodes.tool,
            &open.display,
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
    retain_patches(open, patches)?;
    for update in status {
        open.status.apply(update)?;
    }
    Ok(())
}

fn toggle_inline_output(
    open: &mut OpenPresentation,
    call_id: &super::identity::DisplayId,
    expanded: bool,
    limit: usize,
    now_ms: i64,
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
    let (call, captured) = source
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
    let mut tool_heading = crate::buffer::transcript::ToolHeading::of(
        call,
        crate::turn::ToolInstant {
            wall_ms: now_ms,
            active_ms: captured.active_ms,
        },
        expanded,
    );
    if let Some(state) = source.heading_state() {
        tool_heading.state = state;
    }
    let mut heading = renderer.tool_header(
        heading_id,
        previous_heading.metadata.target[0].id.clone(),
        tool_heading,
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
    let mut changes = vec![TranscriptChange::Block {
        block: (heading).into(),
    }];
    if !body.is_empty() || first_chunk < removed_chunks {
        changes.push(TranscriptChange::ToolBody {
            owner: owner.clone(),
            node: forge_buffer::identity::FoldId(format!("{call_id}:tool")),
            anchor: if first_chunk < removed_chunks {
                identity(first_chunk)
            } else {
                hidden_id
            },
            removed: removed_chunks - first_chunk,
            block: body,
        });
    }
    changes.push(TranscriptChange::Block {
        block: (hidden).into(),
    });
    let patches = match open.transcript.tool_layout(changes) {
        Ok(patches) => patches,
        Err(failure) => {
            let diagnostic = format!("{failure:#}");
            open.failure = Some(diagnostic.clone());
            return Err(super::TranscriptProjectionFailure::new(diagnostic).into());
        }
    };
    for target in &retired_target {
        open.action.remove(target);
    }
    for target in &target {
        open.action
            .insert(target.clone(), TranscriptAction::Tool { call_id: *call_id });
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
    now_ms: i64,
) -> Result<()> {
    let source = match open.agent_scope.as_deref() {
        None => source
            .iter()
            .map(ProjectionSource::Entry)
            .collect::<Vec<_>>(),
        Some(run_id) => find_agent_exchanges(source, run_id)
            .into_iter()
            .flatten()
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
    let mut status = super::status::StatusProjection::Unchanged;
    let unsettled = unsettled_exchanges(source.iter().copied());
    let mut visible_entry = false;
    for entry in source {
        let mut entry = project(
            entry,
            &width,
            now_ms,
            visible_entry,
            &open.sections.nodes.choice,
            &open.sections.nodes.tool_limit,
            &mut open.display,
        )?;
        if open.entry.contains_key(&entry.entry.id) {
            entry.require_workspace_preview()?;
        }
        open.diff.overlay(&mut entry)?;
        if !unsettled.contains(&entry.entry.id) {
            settle_responses(&mut entry);
        }
        visible_entry |= entry
            .entry
            .block
            .iter()
            .any(|block| block.text.row_count() > 0);
        projected.push(entry);
    }
    let mut entry = HashMap::new();
    let mut action = HashMap::new();
    let mut syntax = HashMap::new();
    let mut tool = HashMap::new();
    let mut blocks = Vec::new();
    for mut projection in projected {
        if !matches!(
            projection.status,
            super::status::StatusProjection::Unchanged
        ) {
            status = std::mem::take(&mut projection.status);
        }
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
        if let Some(mut previous) = open.sections.nodes.tool.remove(id)
            && previous.retain(output)
        {
            *output = previous;
        }
    }
    open.syntax_done.clear();
    open.syntax_in_flight.clear();
    open.entry = entry;
    open.action = action;
    open.syntax = syntax;
    open.sections.nodes.tool = tool;
    open.last_width = width;
    open.unsettled_exchange = unsettled;
    retain_patches(open, patch)?;
    if matches!(status, super::status::StatusProjection::Unchanged) {
        open.status.clear()?;
    } else {
        open.status.apply(status)?;
    }
    Ok(())
}

fn apply_projection<'operation>(
    open: &mut OpenPresentation,
    patch: &TimelinePatch,
    operations: impl Iterator<Item = &'operation TimelineOperation> + Clone,
    unsettled: &[String],
    now_ms: i64,
) -> Result<()> {
    let width = open.transcript.views.profile().unwrap_or(&open.last_width);
    let mut projected = Vec::new();
    let mut status = Vec::new();
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
                    changed.push(TranscriptChange::Block {
                        block: block.into(),
                    });
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
                if !streams_into_transcript(message) {
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
                    super::message::settle(&mut replacement.block);
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
                    open.syntax_in_flight.remove(target);
                }
                let syntax_target = TargetId(format!("{}:markdown-syntax", block_id.0));
                open.syntax.remove(&syntax_target);
                open.syntax_done.remove(&syntax_target);
                open.syntax_in_flight.remove(&syntax_target);
                if let Some(entry) = open.entry.get_mut(owner) {
                    entry
                        .target
                        .retain(|target| !previous_targets.contains(target));
                    entry.target.extend(replacement.action.keys().cloned());
                    entry.syntax.retain(|target| target != &syntax_target);
                    entry.syntax.extend(replacement.syntax.keys().cloned());
                    entry.response.retain(|response| response != &block_id);
                    if replacement
                        .block
                        .metadata
                        .node
                        .as_ref()
                        .is_some_and(|node| {
                            node.kind == forge_buffer::node::NodeKind::Message
                                && node.lifecycle == forge_buffer::node::NodeLifecycle::Live
                        })
                    {
                        entry.response.push(replacement.block.id.clone());
                    }
                }
                open.action.extend(replacement.action);
                open.syntax.extend(replacement.syntax);
                changed.push(TranscriptChange::Block {
                    block: (replacement.block).into(),
                });
            }
            TimelineOperation::ToolOutput {
                entry_id,
                exchange_id,
                call,
                delta,
                ..
            } => {
                let call_id = open
                    .tool_display(entry_id, exchange_id, call)
                    .context("streamed tool display is unavailable")?;
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
                    .get_mut(&call_id)
                    .context("streamed tool cache is missing")?;
                let old_chunks = if expanded {
                    source.inline_prefix_chunks(limit)?
                } else {
                    1
                };
                let first_chunk = if expanded { source.inline_tail()? } else { 0 };
                source.append(delta)?;
                if source.group.is_empty() || !expanded {
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
                        let mut node = node.clone();
                        node.more = more;
                        heading.metadata.node = Some(node);
                        changed.push(TranscriptChange::Block {
                            block: (heading).into(),
                        });
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
                    let mut replacement = renderer.tool_body(&call_id, source, expanded, chunk)?;
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
                        open.action
                            .insert(target.id.clone(), TranscriptAction::Tool { call_id });
                        if let Some(entry) = open.entry.get_mut(owner)
                            && new_target
                        {
                            entry.target.push(target.id.clone());
                        }
                    }
                    block.push(replacement);
                }
                let hidden_id = forge_buffer::identity::BlockId(format!("{call_id}:hidden"));
                let mut hidden = renderer.tool_hidden(&call_id, source, expanded)?;
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
                    open.action
                        .insert(target.id.clone(), TranscriptAction::Tool { call_id });
                    if let Some(entry) = open.entry.get_mut(owner)
                        && new_hidden_target
                    {
                        entry.target.push(target.id.clone());
                    }
                }
                if first_chunk < chunks {
                    changed.push(TranscriptChange::ToolBody {
                        owner: owner.clone(),
                        node: forge_buffer::identity::FoldId(format!("{call_id}:tool")),
                        anchor: if first_chunk < old_chunks {
                            identity(first_chunk)
                        } else {
                            hidden_id
                        },
                        removed: old_chunks.saturating_sub(first_chunk),
                        block,
                    });
                }
                changed.push(TranscriptChange::Block {
                    block: (hidden).into(),
                });
            }
            TimelineOperation::Insert { index, entry }
            | TimelineOperation::Replace { index, entry } => {
                let mut entry = project(
                    ProjectionSource::Entry(entry),
                    width,
                    now_ms,
                    *index > 0,
                    &open.sections.nodes.choice,
                    &open.sections.nodes.tool_limit,
                    &mut open.display,
                )?;
                if open.entry.contains_key(&entry.entry.id) {
                    entry.require_workspace_preview()?;
                }
                open.diff.overlay(&mut entry)?;
                if !unsettled.contains(&entry.entry.id) {
                    settle_responses(&mut entry);
                }
                status.push(std::mem::take(&mut entry.status));
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
                status.push(super::status::StatusProjection::Remove { source: id.clone() });
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
                open.syntax_in_flight.remove(&target);
                open.action.remove(&target);
            }
            for call in previous.tool {
                if let Some(tool) = open.sections.nodes.tool.remove(&call) {
                    retained_tool.insert(call, tool);
                }
            }
            for target in previous.syntax {
                open.syntax_done.remove(&target);
                open.syntax_in_flight.remove(&target);
                open.syntax.remove(&target);
            }
        }
    }
    for (id, entry, action, mut tool, syntax) in projected {
        for (call, output) in &mut tool {
            output.own(&id);
            if let Some(mut previous) = retained_tool.remove(call)
                && previous.retain(output)
            {
                *output = previous;
            }
        }
        open.entry.insert(id, entry);
        open.action.extend(action);
        open.sections.nodes.tool.extend(tool);
        open.syntax.extend(syntax);
    }
    retain_patches(open, patches)?;
    for update in status {
        open.status.apply(update)?;
    }
    Ok(())
}

fn history_input_ids(entries: &[TimelineEntry]) -> HashSet<String> {
    let mut ids = HashSet::new();
    for entry in entries {
        if let TimelineEntry::Exchange { exchange, .. } = entry {
            ids.extend(exchange.input_message_id.iter().cloned());
            for node in &exchange.node_list {
                if let crate::exchange::ExchangeNode::ExchangeInput { prompt } = node {
                    ids.insert(prompt.id.clone());
                }
            }
        }
    }
    ids
}

/// Collect acknowledged steering that waits for consumption in open top-level exchanges.
fn awaiting_steering(entries: &[TimelineEntry]) -> Vec<PresentedInput> {
    entries
        .iter()
        .filter_map(|entry| match entry {
            TimelineEntry::Exchange { exchange, .. } if !exchange.is_finished() => Some(exchange),
            _ => None,
        })
        .flat_map(|exchange| exchange.awaiting_inputs())
        .filter(|prompt| {
            !matches!(
                &prompt.intent,
                crate::exchange::InputIntent::User { content }
                    if content.is_question_reply() || matches!(content, crate::input::UserInputContent::CheckResult { .. })
            )
        })
        .map(|prompt| PresentedInput {
            content: crate::input::UserInputContent::UserText {
                text: prompt.text.clone(),
            },
            id: Some(prompt.id.clone()),
            text: prompt.text.clone(),
            failed: false,
            failure_notice: None,
        })
        .collect()
}

/// Build the status staging block from durable waiting steering, then editor staging.
///
/// The status section lists each steering message in one place. Acknowledgement creates the
/// durable copy under the staged message's ID, which removes the staged copy in the same
/// publication; consumption then moves the message from this section into the transcript.
fn update_staging_block(open: &mut OpenPresentation) -> Result<()> {
    use forge_buffer::identity::BlockId;
    let block_id = BlockId("harness:staging".into());
    let staged = |values: &[PresentedInput]| {
        values
            .iter()
            .filter_map(|value| {
                let in_history = value
                    .id
                    .as_ref()
                    .is_some_and(|id| open.history_input_id.contains(id));
                if !in_history {
                    return Some(value.clone());
                }
                // The transcript owns the prompt, but its delivery can still hold the queue.
                value.failed.then(|| {
                    let mut notice = value.clone();
                    notice.text = value.failure_notice.as_deref().map_or_else(
                        || "Input needs recovery".to_owned(),
                        |reason| format!("Input needs recovery · {reason}"),
                    );
                    notice
                })
            })
            .collect::<Vec<_>>()
    };
    let (mut staged_steering, mut staged_queue) = open
        .staging
        .as_ref()
        .map(|staging| (staged(&staging.steering), staged(&staging.queue)))
        .unwrap_or_default();
    let queue_heading = queue_heading(
        open.staging
            .as_ref()
            .and_then(|staging| staging.queue_paused.as_deref()),
    );
    let questions = staged_steering
        .iter()
        .chain(staged_queue.iter())
        .filter(|value| value.content.is_question_reply())
        .cloned()
        .collect::<Vec<_>>();
    staged_steering.retain(|value| !value.content.is_question_reply());
    staged_queue.retain(|value| !value.content.is_question_reply());
    let steering = open
        .awaiting_steering
        .iter()
        .cloned()
        .chain(staged_steering)
        .collect::<Vec<_>>();
    if !steering.is_empty() || !staged_queue.is_empty() || !questions.is_empty() {
        let width = open.transcript.views.profile().unwrap_or(&open.last_width);
        let mut rows = Vec::new();
        let mut decoration = Vec::new();
        append_staging_section(
            &mut rows,
            "Steering active turn",
            &steering,
            width,
            false,
            &mut decoration,
        )?;
        append_staging_section(
            &mut rows,
            &queue_heading,
            &staged_queue,
            width,
            true,
            &mut decoration,
        )?;
        append_staging_section(
            &mut rows,
            "Question replies",
            &questions,
            width,
            false,
            &mut decoration,
        )?;
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
        block.metadata.decoration.extend(decoration);
        let block = (block.text.row_count() > 0).then_some(block);
        open.status.set_staging(block)
    } else {
        open.status.set_staging(None)
    }
}

/// Name the queued follow-up section, marking a paused queue with the key that resumes it.
fn queue_heading(paused: Option<&str>) -> String {
    match paused {
        None => "Queued follow-up inputs".into(),
        Some("") => "Queued follow-up inputs · paused".into(),
        Some(hint) => format!("Queued follow-up inputs · paused · {hint}"),
    }
}

fn append_staging_section(
    rows: &mut Vec<String>,
    label: &str,
    values: &[PresentedInput],
    width: &WidthProfile,
    preview: bool,
    decoration: &mut Vec<forge_buffer::block::Decoration>,
) -> Result<()> {
    if values.is_empty() {
        return Ok(());
    }
    if !rows.is_empty() {
        rows.push(String::new());
    }
    if !label.is_empty() {
        rows.extend(width.wrap_plain(label, 0)?);
    }
    for value in values {
        let failed = value.failed;
        let value = &value.text;
        let start = rows.len();
        if preview {
            let first = value.split(['\r', '\n']).next().unwrap_or_default();
            rows.push(width.clip_plain(&format!("• {first}"), "…")?);
        } else {
            rows.extend(width.wrap_plain(
                &format!("• {value}"),
                2.min(width.columns.saturating_sub(1)),
            )?);
        }
        if failed {
            decoration.push(forge_buffer::block::Decoration {
                range: forge_buffer::block::TextRange {
                    start: forge_buffer::block::TextPosition {
                        row: start,
                        column: 0,
                    },
                    end: forge_buffer::block::TextPosition {
                        row: rows.len(),
                        column: 0,
                    },
                },
                capture: "DiagnosticError".into(),
                priority: 110,
            });
        }
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
        return Err(super::TranscriptProjectionFailure::new(failure.clone()).into());
    }
    let patches = match open.sections.refresh(&mut open.transcript) {
        Ok(patches) => patches,
        Err(failure) => {
            let diagnostic = format!("loaded section projection: {failure:#}");
            open.failure = Some(diagnostic.clone());
            return Err(super::TranscriptProjectionFailure::new(diagnostic).into());
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

    use super::*;

    #[tokio::test]
    async fn timeline_semantic_controls_retain_captured_sources_and_lazy_native_nodes() -> Result<()>
    {
        use super::super::nodes::{NodeAction, NodeRequest};
        use forge_diff::{engine::DiffEngine, syntax::SyntaxEngine};
        let data = tempfile::tempdir()?;
        let workspace = tempfile::tempdir()?;
        let before = "pub fn visible() {\n    previous();\n}\nfn hidden() { old_private(); }\n";
        let after = before
            .replace("previous", "updated")
            .replace("old_private", "new_private");
        let saved_after = after.clone();
        let saved_workspace = workspace.path().to_owned();
        let saved_data = data.path().to_owned();
        tokio::task::spawn_blocking(move || -> Result<()> {
            forge_git::test_support::init_repository(&saved_workspace);
            let mut store = crate::storage::SqliteStore::open(&saved_data)?;
            let connection = crate::storage::test::raw_connection(&saved_data)?;
            connection.execute("INSERT INTO session_record(id, workspace, updated_at_ms, payload) VALUES('session', ?1, 0, '{}')", [saved_workspace.to_string_lossy().as_ref()])?;
            for (id, text) in [("before", before), ("after", saved_after.as_str())] {
                store.save_checkpoint(&crate::checkpoint::CheckpointRecord {
                    id: id.into(), session_id: "session".into(), workspace: saved_workspace.to_string_lossy().into_owned(), head: "UNBORN".into(), tree: None,
                    file: vec![crate::checkpoint::CheckpointFile { path: "source.rs".into(), object_id: store.objects.put(text.as_bytes())?, mode: 0o100644 }],
                    deleted: Vec::new(), checkout: Default::default(), excluded: Default::default(), created_at_ms: 0,
                })?;
            }
            Ok(())
        }).await??;
        let patch = format!(
            "--- a/source.rs\n+++ b/source.rs\n@@ -1,4 +1,4 @@\n{}{}",
            before
                .lines()
                .map(|row| format!("-{row}\n"))
                .collect::<String>(),
            after
                .lines()
                .map(|row| format!("+{row}\n"))
                .collect::<String>()
        );
        let mut entry = interaction_entry("semantic-inline");
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        exchange.checkpoint_before = Some("before".into());
        exchange.checkpoint_after = Some("after".into());
        exchange.checkpoint_diff_text = Some(patch);
        let document = DocumentId("timeline:semantic".into());
        let view = ViewId("timeline:semantic:view".into());
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry.clone()])?;
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let presentation = std::sync::Arc::new(std::sync::Mutex::new(owner));
        let diff = DiffEngine::new(Default::default(), 2);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let mut sequence = 0;
        for selector in [
            "changes", "file", "semantic", "public", "visible", "full", "all",
        ] {
            sequence += 1;
            let frame = presentation.lock().unwrap().snapshot(&document)?;
            let request = if selector == "semantic"
                || selector == "public"
                || selector == "full"
                || selector == "all"
            {
                let block = frame
                    .block
                    .iter()
                    .find(|block| {
                        block
                            .metadata
                            .node
                            .as_ref()
                            .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::File)
                    })
                    .context("missing native file heading")?;
                PresentationRequest::DiffInput {
                    input: DocumentInput {
                        document: document.clone(),
                        revision: frame.revision,
                        view: view.clone(),
                        sequence: InputSequence(sequence),
                        action: if selector == "semantic" || selector == "full" {
                            "toggle_presentation".into()
                        } else {
                            "toggle_visibility".into()
                        },
                        block: block.id.clone(),
                        position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                        target: block
                            .metadata
                            .target
                            .first()
                            .map(|target| target.id.clone()),
                    },
                }
            } else {
                let node = frame
                    .block
                    .iter()
                    .find_map(|block| {
                        block.metadata.node.as_ref().filter(|node| match selector {
                            "changes" => node.kind == forge_buffer::node::NodeKind::Changes,
                            "file" => node.kind == forge_buffer::node::NodeKind::File,
                            "visible" => block
                                .text
                                .row(0)
                                .is_some_and(|text| text.contains("pub fn visible()")),
                            _ => false,
                        })
                    })
                    .context("missing native expansion node")?;
                PresentationRequest::Node {
                    request: NodeRequest {
                        document: document.clone(),
                        view: view.clone(),
                        sequence,
                        node: node.id.0.clone(),
                        generation: node.generation,
                        action: NodeAction::SetExpansion { expanded: true },
                        rows: 80,
                        width: WidthProfile::default(),
                    },
                }
            };
            let revision = frame.revision;
            diff_view::update(
                presentation.clone(),
                &request,
                data.path().to_owned(),
                diff.clone(),
                syntax.clone(),
            )
            .await?
            .context("timeline diff input was not handled")?;
            let frame = presentation.lock().unwrap().snapshot(&document)?;
            let text = frame
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n");
            if selector == "changes" {
                assert!(!text.contains("updated();"));
                assert_eq!(syntax.usage().active_jobs, 0);
                std::fs::write(
                    workspace.path().join("source.rs"),
                    "unrelated later worktree content\n",
                )?;
            }
            if selector == "semantic" {
                assert!(text.contains("pub fn visible()"));
                assert!(!text.contains("updated();"));
            }
            if selector == "public" || selector == "visible" || selector == "full" {
                assert!(!text.contains("hidden"));
            }
            if selector == "visible" || selector == "full" || selector == "all" {
                assert!(text.contains("updated();"));
            }
            if selector == "all" {
                assert!(text.contains("new_private();"));
            }
            assert!(!text.contains("unrelated later worktree"));
            presentation.lock().unwrap().sync(&document, revision)?;
        }
        {
            let mut owner = presentation.lock().unwrap();
            owner.snapshot(&document)?;
            owner.dispatch(
                PresentationRequest::Resize {
                    document: document.clone(),
                    view: view.clone(),
                    width: WidthProfile {
                        columns: 120,
                        ..Default::default()
                    },
                },
                &Default::default(),
            )?;
            assert_eq!(
                owner.open.as_ref().unwrap().diff.view.committed(),
                forge_diff::view::DiffViewOptions::default()
            );
            let resized = owner.snapshot(&document)?;
            assert!(resized.block.iter().any(|block| {
                block
                    .text
                    .row(0)
                    .is_some_and(|text| text.contains("updated();"))
            }));
            let recovered = owner.dispatch(
                PresentationRequest::Snapshot { document },
                &Default::default(),
            )?;
            assert_eq!(
                recovered["diff_view"],
                serde_json::json!({"presentation":"full","visibility":"all"})
            );
        }
        let frame = presentation
            .lock()
            .unwrap()
            .snapshot(&DocumentId("timeline:semantic".into()))?;
        let heading = frame
            .block
            .iter()
            .find(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::File)
            })
            .unwrap();
        let public = PresentationRequest::DiffInput {
            input: DocumentInput {
                document: frame.document.clone(),
                revision: frame.revision,
                view: ViewId("timeline:semantic:view".into()),
                sequence: InputSequence(8),
                action: "toggle_visibility".into(),
                block: heading.id.clone(),
                position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                target: heading
                    .metadata
                    .target
                    .first()
                    .map(|target| target.id.clone()),
            },
        };
        diff_view::update(
            presentation.clone(),
            &public,
            data.path().to_owned(),
            diff.clone(),
            syntax.clone(),
        )
        .await?;
        let later = format!(
            "pub fn visible() {{\n{}}}\nfn hidden() {{ later_private(); }}\n",
            (0..700)
                .map(|index| format!("    later_public_{index}();\n"))
                .collect::<String>()
        );
        let mut store = crate::storage::SqliteStore::open(data.path())?;
        store.save_checkpoint(&crate::checkpoint::CheckpointRecord {
            id: "after-next".into(),
            session_id: "session".into(),
            workspace: workspace.path().to_string_lossy().into_owned(),
            head: "UNBORN".into(),
            tree: None,
            file: vec![crate::checkpoint::CheckpointFile {
                path: "source.rs".into(),
                object_id: store.objects.put(later.as_bytes())?,
                mode: 0o100644,
            }],
            deleted: Vec::new(),
            checkout: Default::default(),
            excluded: Default::default(),
            created_at_ms: 0,
        })?;
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        exchange.checkpoint_after = Some("after-next".into());
        exchange.checkpoint_diff_text = Some(format!(
            "--- a/source.rs\n+++ b/source.rs\n@@ -1,4 +1,{} @@\n{}{}",
            later.lines().count(),
            before
                .lines()
                .map(|row| format!("-{row}\n"))
                .collect::<String>(),
            later
                .lines()
                .map(|row| format!("+{row}\n"))
                .collect::<String>()
        ));
        presentation.lock().unwrap().reconcile(vec![entry])?;
        let frame = presentation
            .lock()
            .unwrap()
            .snapshot(&DocumentId("timeline:semantic".into()))?;
        assert!(
            frame
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .any(|text| text.contains("updated();"))
        );
        assert!(
            diff_view::update(
                presentation.clone(),
                &PresentationRequest::Sync {
                    document: frame.document.clone(),
                    revision: frame.revision
                },
                data.path().to_owned(),
                diff.clone(),
                syntax.clone()
            )
            .await?
            .is_none()
        );
        let frame = presentation
            .lock()
            .unwrap()
            .snapshot(&DocumentId("timeline:semantic".into()))?;
        let node = frame
            .block
            .iter()
            .find_map(|block| {
                block
                    .metadata
                    .node
                    .as_ref()
                    .filter(|node| node.kind == forge_buffer::node::NodeKind::File)
            })
            .unwrap();
        let expansion = PresentationRequest::Node {
            request: NodeRequest {
                document: frame.document,
                view: ViewId("timeline:semantic:view".into()),
                sequence: 9,
                node: node.id.0.clone(),
                generation: node.generation,
                action: NodeAction::SetExpansion { expanded: true },
                rows: 80,
                width: WidthProfile::default(),
            },
        };
        diff_view::update(
            presentation.clone(),
            &expansion,
            data.path().to_owned(),
            diff.clone(),
            syntax.clone(),
        )
        .await?;
        let frame = presentation
            .lock()
            .unwrap()
            .snapshot(&DocumentId("timeline:semantic".into()))?;
        let text = frame
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("later_public"));
        assert!(!text.contains("later_private"));
        let mut sequence = 9;
        for semantic in [false, true] {
            if semantic {
                let frame = presentation
                    .lock()
                    .unwrap()
                    .snapshot(&DocumentId("timeline:semantic".into()))?;
                let heading = frame
                    .block
                    .iter()
                    .find(|block| {
                        block
                            .metadata
                            .node
                            .as_ref()
                            .is_some_and(|node| node.kind == forge_buffer::node::NodeKind::File)
                    })
                    .unwrap();
                sequence += 1;
                diff_view::update(
                    presentation.clone(),
                    &PresentationRequest::DiffInput {
                        input: DocumentInput {
                            document: frame.document,
                            revision: frame.revision,
                            view: ViewId("timeline:semantic:view".into()),
                            sequence: InputSequence(sequence),
                            action: "toggle_presentation".into(),
                            block: heading.id.clone(),
                            position: forge_buffer::block::TextPosition { row: 0, column: 0 },
                            target: heading
                                .metadata
                                .target
                                .first()
                                .map(|target| target.id.clone()),
                        },
                    },
                    data.path().to_owned(),
                    diff.clone(),
                    syntax.clone(),
                )
                .await?;
            }
            let mut previous = String::new();
            loop {
                let frame = presentation
                    .lock()
                    .unwrap()
                    .snapshot(&DocumentId("timeline:semantic".into()))?;
                let text = frame
                    .block
                    .iter()
                    .flat_map(|block| block.text.wire_rows())
                    .collect::<Vec<_>>()
                    .join("\n");
                assert!(!text.contains("later_private"));
                if text.contains("later_public_699();") {
                    break;
                }
                ensure!(text != previous, "timeline source paging did not advance");
                previous = text;
                let node = frame
                    .block
                    .iter()
                    .filter_map(|block| block.metadata.node.as_ref())
                    .find(|node| {
                        node.more
                            && (node.kind == forge_buffer::node::NodeKind::File
                                || semantic && node.kind == forge_buffer::node::NodeKind::Group)
                    })
                    .context("source continuation is missing")?;
                sequence += 1;
                let action = if node.display == forge_buffer::node::NodeDisplay::Heading {
                    NodeAction::SetExpansion { expanded: true }
                } else {
                    NodeAction::LoadMore
                };
                diff_view::update(
                    presentation.clone(),
                    &PresentationRequest::Node {
                        request: NodeRequest {
                            document: frame.document,
                            view: ViewId("timeline:semantic:view".into()),
                            sequence,
                            node: node.id.0.clone(),
                            generation: node.generation,
                            action,
                            rows: 80,
                            width: WidthProfile::default(),
                        },
                    },
                    data.path().to_owned(),
                    diff.clone(),
                    syntax.clone(),
                )
                .await
                .with_context(|| {
                    format!(
                        "timeline paging semantic={semantic} sequence={sequence} node={} rows={}",
                        node.id.0,
                        frame.block.len()
                    )
                })?;
            }
        }
        Ok(())
    }

    /// A captured highlight whose analysis never applies stays retryable, behind untaken work,
    /// instead of being marked done before analysis ran.
    #[test]
    fn failed_syntax_analysis_is_retried_after_untaken_targets() -> Result<()> {
        let document = DocumentId("timeline:syntax".into());
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![interaction_entry("syntax-retry")])?;
        owner.open(
            document.clone(),
            ViewId("timeline:syntax:view".into()),
            WidthProfile::default(),
        )?;
        let open = owner.open.as_mut().context("presentation is open")?;
        open.syntax.clear();
        let job = |name: &str| super::super::syntax::MarkdownSyntax {
            target: TargetId(format!("{name}:markdown-syntax")),
            block: forge_buffer::identity::BlockId(name.into()),
            text: Default::default(),
            code: Default::default(),
        };
        for name in ["first", "second"] {
            open.syntax.insert(job(name).target.clone(), job(name));
        }
        let captured = |owner: &mut SessionPresentation| -> Result<Option<String>> {
            Ok(owner
                .capture_syntax(&document)?
                .map(|job| job.target().0.clone()))
        };

        let failed = captured(&mut owner)?.context("first capture")?;
        let untaken = captured(&mut owner)?.context("second capture")?;
        assert_ne!(
            failed, untaken,
            "a taken target was preferred over untaken work"
        );
        let retried = owner
            .capture_syntax(&document)?
            .context("failed analysis was never retried")?;
        assert_eq!(retried.target().0, failed);
        owner.apply_syntax(&document, &retried, Vec::new())?;
        assert_eq!(captured(&mut owner)?, Some(untaken));
        Ok(())
    }

    /// A tool row shows how it ended whether it was opened before or after the call stopped.
    ///
    /// Opening a tool loaded only its output body; the outcome line beneath it comes from the
    /// entry's projection, which the tool toggle did not redo. A tool opened after its deadline
    /// stopped it showed `no output` alone until a later turn re-projected the entry, and on Claude
    /// Code, with no later turn, never showed `Failed: timed out` at all.
    #[test]
    fn expanded_tool_shows_its_timeout_when_the_exchange_records_it() -> Result<()> {
        use forge_provider_api::ItemStart;
        // The running tool, then the same exchange once its deadline stopped it.
        let exchange = |timed_out: bool, stopped: bool| -> Result<Exchange> {
            let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
                "id":"held", "session_id":"session", "agent_id":"primary", "ordinal":1,
                "prompt":"Run the held command", "state":"running", "created_at_ms":0,
                "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            exchange.resume(0)?;
            let address = crate::turn::test_support::address(&exchange, "turn");
            exchange.start_turn(address.clone(), 0)?;
            let start = ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "shell".into(),
                title: "sleep 45sec".into(),
                arguments: forge_provider_api::InspectableArguments::new(serde_json::json!({})),
            });
            let updates = if stopped {
                crate::turn::test_support::tool_updates(
                    &start,
                    crate::provider::ToolStatus::Interrupted,
                    None,
                    Default::default(),
                )
            } else {
                Vec::new()
            };
            crate::turn::test_support::tool(&mut exchange, &address, "held", start, updates, 1)?;
            if timed_out {
                exchange
                    .tool_timeout
                    .push(crate::provider::tool_timeout::ToolTimeoutRecord {
                        thread_id: address.thread_id.clone(),
                        turn_id: address.turn_id.clone(),
                        item_id: forge_provider_api::ItemId::new("held")?,
                        title: "sleep 45sec".into(),
                        timeout_ms: 15_000,
                    });
            }
            Ok(exchange)
        };
        let document = DocumentId("timeline:timeout".into());
        let view = ViewId("timeline:timeout:view".into());
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(Vec::new())?;
        owner.update_live(
            Some(&exchange(false, false)?),
            None,
            crate::session::state_machine::SessionPhase::Idle,
            false,
        )?;
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let text = |owner: &SessionPresentation| -> Result<String> {
            Ok(owner
                .snapshot(&document)?
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join(
                    "
",
                ))
        };
        // Open every fold, as a reader expanding the transcript does.
        let mut sequence = 0;
        let mut expand_all = |owner: &mut SessionPresentation| -> Result<()> {
            let mut opened = std::collections::HashSet::new();
            loop {
                let folded = owner
                    .snapshot(&document)?
                    .block
                    .iter()
                    .filter_map(|block| block.metadata.node.clone())
                    .filter(|node| {
                        node.display != forge_buffer::node::NodeDisplay::Full
                            && opened.insert(node.id.0.clone())
                    })
                    .collect::<Vec<_>>();
                if folded.is_empty() {
                    return Ok(());
                }
                for node in folded {
                    sequence += 1;
                    owner.dispatch(
                        PresentationRequest::Node {
                            request: super::super::nodes::NodeRequest {
                                document: document.clone(),
                                view: view.clone(),
                                sequence,
                                node: node.id.0.clone(),
                                generation: node.generation,
                                action: super::super::nodes::NodeAction::SetExpansion {
                                    expanded: true,
                                },
                                rows: 80,
                                width: WidthProfile::default(),
                            },
                        },
                        &Default::default(),
                    )?;
                }
                ensure!(sequence < 64, "expansion never settled");
            }
        };
        expand_all(&mut owner)?;
        let before = text(&owner)?;
        ensure!(
            before.contains("sleep 45sec") && !before.contains("Failed: timed out"),
            "{before}"
        );
        // The broker records the expiry before the provider reports the call it interrupted.
        for stopped in [false, true] {
            owner.update_live(
                Some(&exchange(true, stopped)?),
                None,
                crate::session::state_machine::SessionPhase::Idle,
                false,
            )?;
            let during = text(&owner)?;
            ensure!(
                during.contains("Failed: timed out after 15s; execution stopped."),
                "the open tool kept its earlier body (stopped: {stopped}): {during}"
            );
        }
        // A reader who opens the tool only after the call stopped sees the same line.
        let mut reopened = SessionPresentation::new("session".into());
        reopened.initialize(Vec::new())?;
        reopened.update_live(
            Some(&exchange(true, true)?),
            None,
            crate::session::state_machine::SessionPhase::Idle,
            false,
        )?;
        reopened.open(document.clone(), view.clone(), WidthProfile::default())?;
        expand_all(&mut reopened)?;
        let late = text(&reopened)?;
        ensure!(
            late.contains("Failed: timed out after 15s; execution stopped."),
            "a tool opened after it stopped showed no outcome: {late}"
        );
        Ok(())
    }

    /// Presentation time the test sets explicitly.
    struct ManualClock(std::sync::atomic::AtomicI64);
    impl crate::broker::Clock for ManualClock {
        fn now_ms(&self) -> i64 {
            self.0.load(std::sync::atomic::Ordering::Relaxed)
        }
    }

    /// Change each elapsed label at its own second boundary, whenever the editor asks.
    ///
    /// The host refreshed labels only one second after its previous refresh, and the editor
    /// asked once a second on its own schedule. Two independent one-second gates sample the
    /// clock every 1.04 to 1.08 seconds, so a displayed second was skipped whenever the drift
    /// crossed a boundary. The host now refreshes as soon as any label is due and tells the
    /// editor when the next one is.
    #[test]
    fn elapsed_labels_change_at_every_second_boundary() -> Result<()> {
        let clock = Arc::new(ManualClock(std::sync::atomic::AtomicI64::new(0)));
        let mut owner = SessionPresentation::with_clock("session".into(), clock.clone());
        owner.initialize(vec![TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: crate::session::state_machine::SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: Some(0),
                activity: crate::session::state_machine::WorkflowActivity::Working,
                reasoning_summary: None,
                execution: None,
            },
            status_thoughts: false,
            agents: Vec::new(),
            checks: None,
        }])?;
        let document = DocumentId("elapsed-labels".into());
        owner.open(
            document.clone(),
            ViewId("elapsed-view".into()),
            WidthProfile::default(),
        )?;
        let mut samples = Vec::new();
        for now_ms in [990, 1_010, 2_005, 2_990, 3_000] {
            clock.0.store(now_ms, std::sync::atomic::Ordering::Relaxed);
            let revision = owner
                .document(&document)?
                .sections
                .document
                .document
                .revision();
            let synced = owner.sync(&document, revision)?;
            let label = synced.status.block[0].text.wire_rows()[0].to_owned();
            samples.push((now_ms, label, synced.activity_due_in_ms));
        }
        assert_eq!(
            samples,
            [
                (990, "Working · 0s".to_owned(), Some(10)),
                (1_010, "Working · 1s".to_owned(), Some(990)),
                (2_005, "Working · 2s".to_owned(), Some(995)),
                (2_990, "Working · 2s".to_owned(), Some(10)),
                (3_000, "Working · 3s".to_owned(), Some(1_000)),
            ]
        );
        Ok(())
    }

    /// Advance the check heading and a running check row at each second while checks run.
    ///
    /// During a Check phase every timed value in the status split stayed fixed for six seconds
    /// although the editor kept synchronizing, so each sync must render a new status revision.
    #[test]
    fn running_check_labels_change_at_every_second_boundary() -> Result<()> {
        use crate::checks::{CheckActivity, CheckCommandActivity, CheckState};
        let command = |title: &str, state, started_at_ms, completed_at_ms| CheckCommandActivity {
            title: title.into(),
            selection: None,
            started_at_ms: Some(started_at_ms),
            completed_at_ms,
            state,
            output_line: None,
            progress: None,
        };
        let clock = Arc::new(ManualClock(std::sync::atomic::AtomicI64::new(0)));
        let mut owner = SessionPresentation::with_clock("session".into(), clock.clone());
        owner.initialize(vec![TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: crate::session::state_machine::SessionPhase::Idle,
            status_thoughts: false,
            agents: Vec::new(),
            checks: Some(CheckActivity {
                run_id: "run".into(),
                duration_ms: 400,
                started_at_ms: Some(500),
                completed_count: 1,
                total_count: 2,
                command_list: vec![
                    command("Type check", CheckState::Passed, 0, Some(400)),
                    command("ESLint", CheckState::Running, 500, None),
                ],
            }),
        }])?;
        let document = DocumentId("check-labels".into());
        owner.open(
            document.clone(),
            ViewId("check-view".into()),
            WidthProfile::default(),
        )?;
        let mut samples = Vec::new();
        let mut revisions = Vec::new();
        for now_ms in [600, 1_100, 2_100, 3_100] {
            clock.0.store(now_ms, std::sync::atomic::Ordering::Relaxed);
            let revision = owner
                .document(&document)?
                .sections
                .document
                .document
                .revision();
            let synced = owner.sync(&document, revision)?;
            let rows = synced
                .status
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .map(str::to_owned)
                .collect::<Vec<_>>();
            let heading = rows.first().cloned().unwrap_or_default();
            let running = rows
                .iter()
                .find(|row| row.contains("ESLint"))
                .cloned()
                .unwrap_or_default();
            samples.push((heading, running.trim().to_owned()));
            revisions.push(synced.status.revision);
        }
        assert_eq!(
            samples,
            [
                (
                    "Working · 0s · 1/2 checks finished".to_owned(),
                    "└─ ⠋ ESLint · running · 0s".to_owned()
                ),
                (
                    "Working · 1s · 1/2 checks finished".to_owned(),
                    "└─ ⠋ ESLint · running · 0s".to_owned()
                ),
                (
                    "Working · 2s · 1/2 checks finished".to_owned(),
                    "└─ ⠋ ESLint · running · 1s".to_owned()
                ),
                (
                    "Working · 3s · 1/2 checks finished".to_owned(),
                    "└─ ⠋ ESLint · running · 2s".to_owned()
                ),
            ]
        );
        assert!(
            revisions.windows(2).all(|pair| pair[0] < pair[1]),
            "a changed label kept its status revision: {revisions:?}"
        );
        Ok(())
    }

    /// Use the verification run's clock for the Working heading during the Checking phase.
    #[test]
    fn checking_status_timer_advances_with_its_run() -> Result<()> {
        use crate::session::state_machine::{SessionPhase, WorkflowActivity};
        let clock = Arc::new(ManualClock(std::sync::atomic::AtomicI64::new(0)));
        let mut owner = SessionPresentation::with_clock("session".into(), clock.clone());
        owner.initialize(vec![TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: None,
                activity: WorkflowActivity::Checking,
                reasoning_summary: None,
                execution: None,
            },
            status_thoughts: false,
            agents: Vec::new(),
            checks: None,
        }])?;
        let document = DocumentId("checking-clock".into());
        owner.open(
            document.clone(),
            ViewId("checking-clock-view".into()),
            WidthProfile::default(),
        )?;
        let run: crate::checks::CheckRun = serde_json::from_value(serde_json::json!({
            "id":"run", "owner":{"kind":"session","session_id":"session"},
            "revision":0, "shell":"system", "executable":null, "workspace":".",
            "environment_digest":"fixture", "started_at_ms":1_000, "completed_at_ms":null,
            "state":"running", "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null, "workspace_digest":{}, "commands":[]
        }))?;
        clock.0.store(1_000, std::sync::atomic::Ordering::Relaxed);
        owner.update_checks(&run)?;
        let mut headings = Vec::new();
        for now_ms in [1_000, 2_000, 4_500] {
            clock.0.store(now_ms, std::sync::atomic::Ordering::Relaxed);
            let revision = owner
                .document(&document)?
                .sections
                .document
                .document
                .revision();
            let synced = owner.sync(&document, revision)?;
            headings.push(
                synced
                    .status
                    .block
                    .first()
                    .and_then(|block| block.text.wire_rows().first().map(|row| (*row).to_owned()))
                    .unwrap_or_default(),
            );
        }
        assert!(
            headings == ["Working · 0s", "Working · 1s", "Working · 3s"],
            "the Working heading did not follow the check run: {headings:?}"
        );
        Ok(())
    }

    /// Enter Check with one change of the status height.
    ///
    /// Keep the Working heading during the hand-off and publish the check count and command
    /// rows together once the run starts.
    #[test]
    fn entering_check_changes_the_status_height_once() -> Result<()> {
        use crate::session::state_machine::{SessionPhase, WorkflowActivity};
        let clock = Arc::new(ManualClock(std::sync::atomic::AtomicI64::new(1_000)));
        let mut owner = SessionPresentation::with_clock("session".into(), clock.clone());
        owner.initialize(vec![TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: SessionPhase::Working {
                duration_ms: 0,
                started_at_ms: None,
                activity: WorkflowActivity::ChecksStarting,
                reasoning_summary: None,
                execution: None,
            },
            status_thoughts: false,
            agents: Vec::new(),
            checks: None,
        }])?;
        let document = DocumentId("entering-check".into());
        owner.open(
            document.clone(),
            ViewId("entering-check-view".into()),
            WidthProfile::default(),
        )?;
        let status_rows = |owner: &mut SessionPresentation| -> Result<Vec<String>> {
            let revision = owner
                .document(&document)?
                .sections
                .document
                .document
                .revision();
            let synced = owner.sync(&document, revision)?;
            Ok(synced
                .status
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .map(str::to_owned)
                .collect())
        };
        let pending = status_rows(&mut owner)?;
        assert_eq!(
            pending,
            ["Working · 0s"],
            "the pending run changed the status heading"
        );
        let command = |id: &str| {
            serde_json::json!({"id":id, "command":id, "state":"pending", "selected":true,
                "started_at_ms":null, "completed_at_ms":null, "exit_code":null, "error":null,
                "stdout":"unused", "stderr":"unused", "output":"unused", "output_bytes":0,
                "preview":""})
        };
        let run: crate::checks::CheckRun = serde_json::from_value(serde_json::json!({
            "id":"run", "owner":{"kind":"session","session_id":"session"},
            "revision":0, "shell":"system", "executable":null, "workspace":".",
            "environment_digest":"fixture", "started_at_ms":1_000, "completed_at_ms":null,
            "state":"running", "progress":crate::plan::execution::SemanticProgress::default(),
            "workspace_identity":null, "workspace_digest":{},
            "commands":[command("typecheck"), command("lint")]
        }))?;
        owner.update_checks(&run)?;
        let checking = status_rows(&mut owner)?;
        assert!(
            checking[0] == "Working · 0s · 0/2 checks finished" && checking.len() == 3,
            "the run's first record did not publish its count with its rows: {checking:?}"
        );
        Ok(())
    }

    fn interaction_entry(identity: &str) -> TimelineEntry {
        let mut interaction = Exchange {
            finalization_error: None,
            collection: None,
            cancellation_requested: false,
            check_owner: None,
            output_store: None,
            archived_output: Default::default(),
            input_message_id: None,
            provider_retry: None,
            agent_id: "primary".into(),
            id: identity.into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: format!("Prompt {identity}"),
            opening: crate::exchange::ExchangeOpening::user_text(format!("Prompt {identity}")),

            approval_mode: None,

            phase: crate::exchange::ExchangePhase::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            provider_checkpoint_id: None,
            approval_resolution: Vec::new(),
            tool_timeout: Vec::new(),
            host_file_changes: Vec::new(),
            check_setup: None,
            turn: Vec::new(),
            created_at_ms: 0,
            entry: Vec::new(),
            tool_counter: Default::default(),
            node_list: Vec::new(),
            metrics: crate::exchange::ExchangeMetrics::default(),
            comment: Vec::new(),
            checklist: None,
            closed_input_consumption: Vec::new(),

            question: crate::exchange::ExchangeQuestion::None,
            clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 1 },
        };
        let address = crate::turn::test_support::address(&interaction, identity);
        interaction.start_turn(address.clone(), 0).unwrap();
        let event = crate::turn::test_support::reasoning(
            &address,
            "thought",
            forge_provider_api::ReasoningKind::Full,
            "Completed thought\nwith details",
        );
        interaction.observe_event(&event, 0).unwrap();
        crate::turn::test_support::tool(
            &mut interaction,
            &address,
            "tool",
            forge_provider_api::ItemStart::CommandExecution(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                title: "Read output".into(),
                name: "read".into(),
                arguments: forge_provider_api::InspectableArguments::new(serde_json::json!({})),
            }),
            crate::turn::test_support::tool_updates(
                &(forge_provider_api::ItemStart::CommandExecution(
                    forge_provider_api::ToolDescriptor {
                        semantics: Default::default(),
                        title: "Read output".into(),
                        name: "read".into(),
                        arguments: forge_provider_api::InspectableArguments::new(
                            serde_json::json!({}),
                        ),
                    },
                )),
                crate::provider::ToolStatus::Succeeded,
                Some("first\nsecond\nthird\nfourth\nfifth\nsixth".into()),
                Default::default(),
            ),
            1,
        )
        .unwrap();
        let event = crate::turn::test_support::completed_event(
            &(address),
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            1,
        );
        interaction.observe_event(&event, 1).unwrap();
        interaction
            .finish(crate::exchange::ExchangeOutcome::Complete, 1)
            .unwrap();
        TimelineEntry::Exchange {
            id: identity.into(),

            created_at_ms: 0,

            agent_by_id: HashMap::new(),
            exchange: (interaction).into(),
        }
    }

    /// `Enter` on a spawn row opens its child even when the transcript changed after the editor
    /// captured the row. While children run, their events revise the transcript several times a
    /// second, so the captured revision is usually older than the current one; the input was then
    /// refused as "transcript input revision changed" and nothing opened.
    #[test]
    fn spawn_row_opens_its_child_after_the_transcript_changes() -> Result<()> {
        let mut entry = interaction_entry("parent");
        let TimelineEntry::Exchange { exchange, .. } = &mut entry else {
            unreachable!()
        };
        let delegation = crate::agent::Delegation {
            id: "delegation".into(),
            parent_exchange_id: "parent".into(),
            parent_turn_id: None,
            child_agent_id: "child-agent".into(),
            child_exchange_id: "child-exchange".into(),
            task: "Inspect the parser".into(),
            created_at_ms: 0,
        };
        // Children run while their parent waits for them, so the parent's thought stays open.
        exchange.phase = crate::exchange::ExchangePhase::Running;
        exchange
            .node_list
            .push(crate::exchange::ExchangeNode::AgentReference { agent: delegation });
        let document = DocumentId("timeline:spawn".into());
        let view = ViewId("timeline:spawn:view".into());
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![entry.clone()])?;
        owner.open(document.clone(), view.clone(), WidthProfile::default())?;
        let captured = owner.snapshot(&document)?;
        let spawn = captured
            .block
            .iter()
            .find(|block| block.text.wire_rows()[0].starts_with("◇ Spawned"))
            .context("spawn row is missing")?;
        let target = spawn
            .metadata
            .target
            .first()
            .map(|target| target.id.clone())
            .context("spawn row has no action target")?;
        owner.reconcile(vec![entry, interaction_entry("later")])?;
        let current = owner.snapshot(&document)?;
        assert!(current.revision > captured.revision);
        let action = owner.action(DocumentInput {
            document: document.clone(),
            revision: captured.revision,
            view,
            sequence: InputSequence(1),
            action: "activate".into(),
            block: spawn.id.clone(),
            position: forge_buffer::block::TextPosition { row: 0, column: 0 },
            target: Some(target),
        })?;
        assert!(
            matches!(&action, TranscriptAction::Agent { run_id } if run_id == "child-agent"),
            "{action:?}"
        );
        Ok(())
    }

    /// Keep a failed queue entry actionable after the transcript adopts its text.
    #[test]
    fn unresolved_input_keeps_a_status_notice_without_repeating_its_text() -> Result<()> {
        let prompt = "Prompt awaiting delivery evidence";
        let exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id": "exchange", "session_id": "session", "agent_id": "primary", "ordinal": 1,
            "prompt": prompt, "state": "failed", "created_at_ms": 0, "input_message_id": "failed",
            "attributed_matches_checkpoint": false, "node_list": []
        }))?;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 0,
            agent_by_id: HashMap::new(),
            exchange: exchange.into(),
        }])?;
        let document = DocumentId("timeline:unresolved-input".into());
        owner.open(
            document.clone(),
            ViewId("timeline:unresolved-input:view".into()),
            WidthProfile::default(),
        )?;
        owner.set_staging(
            &document,
            1,
            vec![StagedInput::Message {
                id: "failed".into(),
                content: StagedContent::Message,
                text: prompt.into(),
                failed: true,
                failure_notice: None,
            }],
            Vec::new(),
            None,
            &Default::default(),
        )?;
        let status =
            serde_json::to_string(&owner.open.as_ref().context("open")?.status.snapshot())?;
        assert!(status.contains("Queued follow-up inputs"), "{status}");
        assert!(status.contains("Input needs recovery"), "{status}");
        assert!(!status.contains(prompt), "{status}");
        Ok(())
    }

    /// The status section shows each steering message once, from staging through acknowledgement,
    /// without Lua staging, until consumption moves it into the transcript.
    #[test]
    fn steering_keeps_one_visible_copy_through_consumption() -> Result<()> {
        let steer = "keep it short";
        let mut exchange: Exchange = serde_json::from_value(serde_json::json!({
            "id":"steered", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Summarize", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        exchange.resume(0)?;
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 0)?;
        let mut owner = SessionPresentation::new("session".into());
        owner.initialize(vec![TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: 0,
            agent_by_id: HashMap::new(),
            exchange: exchange.clone().into(),
        }])?;
        let document = DocumentId("timeline:steering".into());
        owner.open(
            document.clone(),
            ViewId("timeline:steering:view".into()),
            WidthProfile::default(),
        )?;
        let text = |snapshot: BufferSnapshot| {
            snapshot
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n")
        };
        let visible = |owner: &SessionPresentation| -> Result<(usize, usize)> {
            let status = text(owner.open.as_ref().context("open")?.status.snapshot());
            let transcript = text(owner.snapshot(&document)?);
            Ok((
                status.matches(steer).count(),
                transcript.matches(steer).count(),
            ))
        };
        let staged = || {
            vec![StagedInput::Message {
                id: "steer".into(),
                content: StagedContent::Message,
                text: steer.into(),
                failed: false,
                failure_notice: None,
            }]
        };

        owner.set_staging(
            &document,
            1,
            Vec::new(),
            staged(),
            None,
            &Default::default(),
        )?;
        assert_eq!(visible(&owner)?, (1, 0));
        let submission = forge_provider_api::SubmissionId::try_from("steer-submission".to_owned())?;
        exchange.append_acknowledged_input(
            &address.scope,
            Some("steer".into()),
            Some(submission.clone()),
            crate::exchange::InputIntent::user_text(steer.into()),
            steer.into(),
            1,
        )?;
        owner.update_live(Some(&exchange), None, SessionPhase::Idle, false)?;
        assert_eq!(
            visible(&owner)?,
            (1, 0),
            "acknowledgement duplicated or dropped"
        );
        owner.set_staging(
            &document,
            2,
            Vec::new(),
            Vec::new(),
            None,
            &Default::default(),
        )?;
        assert_eq!(
            visible(&owner)?,
            (1, 0),
            "durable waiting needed Lua staging"
        );
        let status = text(owner.open.as_ref().context("open")?.status.snapshot());
        assert!(status.contains("Steering active turn"), "{status}");

        // A presentation opened while the steer waits lists it with no editor staging.
        owner.close(&document)?;
        let reopened = DocumentId("timeline:steering:reopened".into());
        let opened = owner.open(
            reopened.clone(),
            ViewId("timeline:steering:reopened:view".into()),
            WidthProfile::default(),
        )?;
        assert_eq!(
            serde_json::to_string(&opened.status)?
                .matches(steer)
                .count(),
            1
        );
        let document = reopened;

        exchange.observe_event(
            &crate::turn::test_support::item_event(
                &address,
                "echo",
                forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::UserMessage(
                    forge_provider_api::UserMessageStart {
                        text: steer.into(),
                        submission_id: Some(submission),
                    },
                )),
            ),
            2,
        )?;
        owner.update_live(Some(&exchange), None, SessionPhase::Idle, false)?;
        let status = text(owner.open.as_ref().context("open")?.status.snapshot());
        let transcript = text(owner.snapshot(&document)?);
        assert_eq!(status.matches(steer).count(), 0, "{status}");
        assert_eq!(transcript.matches(steer).count(), 1, "{transcript}");
        Ok(())
    }

    /// Mark a paused queue in its heading with the key that resumes it.
    ///
    /// An interrupt pauses delivery and leaves follow-ups looking like ordinary queued rows, so
    /// nothing told the user the queue would not run or how to resume it.
    #[test]
    fn paused_queue_heading_names_the_resume_hint() -> Result<()> {
        let mut owner = SessionPresentation::new("session".into());
        let document = DocumentId("paused-staging".into());
        owner.open(
            document.clone(),
            ViewId("paused-view".into()),
            WidthProfile::default(),
        )?;
        let queued = || -> Result<Vec<StagedInput>> {
            Ok(vec![serde_json::from_value(serde_json::json!({
                "id":"queued", "text":"Follow-up", "failed":false, "content":{"kind":"message"},
            }))?])
        };
        let status = |owner: &SessionPresentation| -> Result<String> {
            Ok(owner
                .open
                .as_ref()
                .context("open")?
                .status
                .snapshot()
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join(
                    "
",
                ))
        };
        owner.set_staging(
            &document,
            1,
            queued()?,
            Vec::new(),
            Some("<M-r> resume".into()),
            &Default::default(),
        )?;
        let paused = status(&owner)?;
        assert!(
            paused.contains("Queued follow-up inputs · paused · <M-r> resume"),
            "{paused}"
        );
        owner.set_staging(
            &document,
            2,
            queued()?,
            Vec::new(),
            None,
            &Default::default(),
        )?;
        let running = status(&owner)?;
        assert!(running.contains("Queued follow-up inputs"), "{running}");
        assert!(!running.contains("paused"), "{running}");
        Ok(())
    }

    #[test]
    fn question_staging_requires_category_and_preserves_failed_delivery() -> Result<()> {
        assert!(
            serde_json::from_value::<StagedInput>(serde_json::json!({
                "id":"answer", "text":"Tabs", "failed":false,
            }))
            .is_err()
        );
        assert!(
            serde_json::from_value::<StagedInput>(serde_json::json!({
                "id":"answer", "text":"Tabs", "content":{"kind":"question_response"},
            }))
            .is_err()
        );
        let mut owner = SessionPresentation::new("session".into());
        let document = DocumentId("question-staging".into());
        owner.open(
            document.clone(),
            ViewId("question-view".into()),
            WidthProfile::default(),
        )?;
        let staged: StagedInput = serde_json::from_value(serde_json::json!({
            "id":"answer", "text":"Delivery failed: Tabs", "failed":true,
            "content":{"kind":"question_response", "responses":[{
                "question_id":"layout", "revision":1, "message_id":"answer-layout", "question":"Which layout?",
                "response":{"kind":"answer", "answer":"Tabs", "required_questions_remain":false}
            }]}
        }))?;
        let questions = serde_json::from_value(serde_json::json!({
            "revision":1, "questions":[{
                "id":"layout", "exchange_id":"source", "task_id":null, "phase":null,
                "requirement":"optional", "content":{"revision":1,"header":"Layout","question":"Which layout?","options":[]},
                "previous":[], "resolution":{"kind":"pending"}, "discussion":[]
            }]
        }))?;
        owner.set_staging(&document, 1, Vec::new(), vec![staged], None, &questions)?;
        let snapshot = owner.open.as_ref().context("open")?.status.snapshot();
        let text = snapshot
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(text.contains("Question replies"), "{text}");
        assert!(text.contains("Delivery failed: Tabs"), "{text}");
        assert!(!text.contains("Steering active turn"), "{text}");
        assert!(snapshot.block.iter().any(|block| {
            block
                .metadata
                .decoration
                .iter()
                .any(|decoration| decoration.capture == "DiagnosticError")
        }));
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
}
