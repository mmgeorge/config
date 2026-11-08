use crate::provider::ScopeOwner;
use crate::provider::{ProviderChangeSet, ToolOutcome, TurnAddress};
use forge_provider_api::{
    ItemCompletion, ItemEvent, ItemId, ItemStart, TextUpdate, ToolCompletion, ToolDescriptor,
    ToolInputUpdate, ToolItemEvent,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Preserve native tool lifecycle independently of display text.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ToolState {
    Running,
    Completed,
    Failed,
    Denied,
    Cancelled,
    Interrupted,
}

/// Read one instant on both clocks a tool timer can use.
///
/// `active_ms` reads the owning exchange's tool clock, which pauses with the Working timer
/// during approval and input waits. It is absent for host-generated tools that no exchange
/// clock owns, and those tools, like records saved before active readings existed, time
/// themselves by `wall_ms`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ToolInstant {
    pub wall_ms: i64,
    pub active_ms: Option<u64>,
}

impl ToolInstant {
    /// Create an instant that only a wall clock measures.
    pub fn wall(wall_ms: i64) -> Self {
        Self {
            wall_ms,
            active_ms: None,
        }
    }
}

/// Own the display projection of one provider operation.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "ToolCallWire")]
pub struct ToolCall {
    pub started_at_ms: Option<i64>,
    pub completed_at_ms: Option<i64>,
    /// Exchange tool-clock reading at the first execution start.
    pub started_active_ms: Option<u64>,
    /// Exchange tool-clock reading at the first terminal observation.
    pub completed_active_ms: Option<u64>,
    pub checklist_item_id: Option<String>,
    pub id: String,
    pub kind: String,
    pub title: String,
    pub command_shell: Option<String>,
    pub output: super::ToolOutputText,
    pub status: String,
    pub failed: bool,
    pub change: ProviderChangeSet,
    state: ToolState,
}

#[derive(Deserialize)]
struct ToolCallWire {
    started_at_ms: Option<i64>,
    completed_at_ms: Option<i64>,
    #[serde(default)]
    started_active_ms: Option<u64>,
    #[serde(default)]
    completed_active_ms: Option<u64>,
    checklist_item_id: Option<String>,
    id: String,
    kind: String,
    title: String,
    command_shell: Option<String>,
    output: super::ToolOutputText,
    status: String,
    failed: bool,
    change: ProviderChangeSet,
    state: ToolState,
}

impl TryFrom<ToolCallWire> for ToolCall {
    type Error = ToolStoreLoadError;

    /// Validate the typed lifecycle projection independently of its serialized display strings.
    fn try_from(wire: ToolCallWire) -> Result<Self, Self::Error> {
        let ToolCallWire {
            started_at_ms,
            completed_at_ms,
            started_active_ms,
            completed_active_ms,
            checklist_item_id,
            id,
            kind,
            title,
            command_shell,
            output,
            status,
            failed,
            change,
            state,
        } = wire;
        if id.trim().is_empty()
            || status != state_label(state)
            || failed != (state == ToolState::Failed)
            || state == ToolState::Running && completed_at_ms.is_some()
            || completed_at_ms
                .zip(started_at_ms)
                .is_some_and(|(end, start)| end < start)
            || completed_active_ms
                .zip(started_active_ms)
                .is_some_and(|(end, start)| end < start)
        {
            return Err(ToolStoreLoadError::Lifecycle);
        }
        Ok(Self {
            started_at_ms,
            completed_at_ms,
            started_active_ms,
            completed_active_ms,
            checklist_item_id,
            id,
            kind,
            title,
            command_shell,
            output,
            status,
            failed,
            change,
            state,
        })
    }
}

impl ToolCall {
    /// Create a host-generated tool display record from an explicit typed lifecycle.
    pub(crate) fn projection(
        id: String,
        kind: String,
        title: String,
        state: ToolState,
        started_at_ms: Option<i64>,
        completed_at_ms: Option<i64>,
    ) -> Self {
        Self {
            started_at_ms,
            completed_at_ms,
            started_active_ms: None,
            completed_active_ms: None,
            checklist_item_id: None,
            id,
            kind,
            title,
            command_shell: None,
            output: Default::default(),
            status: state_label(state).into(),
            failed: state == ToolState::Failed,
            change: Default::default(),
            state,
        }
    }
    /// Copy heading metadata without retaining output or file-change payloads.
    pub(crate) fn heading(&self) -> Self {
        let mut heading = self.clone();
        heading.output = Default::default();
        heading.change = Default::default();
        heading.checklist_item_id = None;
        heading
    }
    /// Get observed execution time, frozen at the first terminal observation.
    ///
    /// A tool started on its exchange's tool clock excludes the approval and input waits
    /// that pause that clock; other tools fall back to wall time.
    pub fn elapsed_ms(&self, now: ToolInstant) -> Option<u64> {
        if let Some(start) = self.started_active_ms
            && let Some(end) = self.completed_active_ms.or_else(|| {
                (self.state == ToolState::Running)
                    .then_some(now.active_ms)
                    .flatten()
            })
        {
            return Some(end.saturating_sub(start));
        }
        let start = self.started_at_ms?;
        let end = self
            .completed_at_ms
            .or_else(|| (self.state == ToolState::Running).then_some(now.wall_ms))?;
        u64::try_from(end.saturating_sub(start).max(0)).ok()
    }
    /// Get the typed lifecycle without interpreting the display projection.
    pub fn state(&self) -> ToolState {
        self.state
    }
}

/// Store tool items in admission order under checked normalized identities.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(try_from = "ToolStoreWire")]
pub struct ToolStore {
    order: Vec<ItemId>,
    item: HashMap<ItemId, ToolCall>,
    execution: HashMap<ItemId, ToolExecution>,
}

/// Validate persisted identities and native outcomes before restoring a tool owner.
#[derive(Debug, thiserror::Error)]
enum ToolStoreLoadError {
    #[error("tool store contains duplicate, missing, or conflicting item identities")]
    Identity,
    #[error("tool store changes an item's original normalized turn owner")]
    Owner,
    #[error("tool store changes an item's immutable native type")]
    ItemType,
    #[error("tool store contains contradictory lifecycle or timing observations")]
    Lifecycle,
    #[error("tool store contains a conflicting retained native final snapshot")]
    FinalSnapshot,
}

#[derive(Deserialize)]
struct ToolStoreWire {
    order: Vec<ItemId>,
    item: HashMap<ItemId, ToolCall>,
    execution: HashMap<ItemId, ToolExecution>,
}

impl TryFrom<ToolStoreWire> for ToolStore {
    type Error = ToolStoreLoadError;

    /// Validate all related maps before publishing a restored native tool store.
    fn try_from(wire: ToolStoreWire) -> Result<Self, Self::Error> {
        let ToolStoreWire {
            order,
            item,
            execution,
        } = wire;
        if order.len() != item.len() || item.len() != execution.len() {
            return Err(ToolStoreLoadError::Identity);
        }
        let mut seen = std::collections::HashSet::new();
        let mut original_owner = None;
        for id in &order {
            if !seen.insert(id) {
                return Err(ToolStoreLoadError::Identity);
            }
            let tool = item.get(id).ok_or(ToolStoreLoadError::Identity)?;
            let native = execution.get(id).ok_or(ToolStoreLoadError::Identity)?;
            if tool.id != id.as_str() || native.item_id != *id {
                return Err(ToolStoreLoadError::Identity);
            }
            if native.address.thread_id != native.address.scope.owner().thread_id()
                || native.address.scope.turn_id().as_ref() != Ok(&native.address.turn_id)
                || original_owner.is_some_and(|owner| owner != &native.address)
            {
                return Err(ToolStoreLoadError::Owner);
            }
            original_owner = Some(&native.address);
            if tool.kind != kind_label(native.kind) {
                return Err(ToolStoreLoadError::ItemType);
            }
            if tool.status != state_label(tool.state)
                || tool.failed != (tool.state == ToolState::Failed)
                || tool.title != native.descriptor.title
                || tool.command_shell != command_shell(&native.descriptor)
                || tool
                    .completed_at_ms
                    .zip(tool.started_at_ms)
                    .is_some_and(|(end, start)| end < start)
            {
                return Err(ToolStoreLoadError::Lifecycle);
            }
            let expected = match &native.outcome {
                None => ToolState::Running,
                Some(ToolOutcome::Succeeded) => ToolState::Completed,
                Some(ToolOutcome::Failed { .. }) => ToolState::Failed,
                Some(ToolOutcome::Denied { .. }) => ToolState::Denied,
                Some(ToolOutcome::Cancelled) => ToolState::Cancelled,
                Some(ToolOutcome::Interrupted) => ToolState::Interrupted,
            };
            if tool.state != expected
                || (tool.state == ToolState::Running) != tool.completed_at_ms.is_none()
                || native.lifetime == ToolExecutionLifetime::Background
                    && (native.kind != ToolKind::CommandExecution || tool.started_at_ms.is_none())
            {
                return Err(ToolStoreLoadError::Lifecycle);
            }
            if let Some(snapshot) = &native.final_snapshot
                && native.outcome.as_ref() != Some(&snapshot.outcome)
            {
                return Err(ToolStoreLoadError::FinalSnapshot);
            }
        }
        Ok(Self {
            order,
            item,
            execution,
        })
    }
}

/// Preserve the immutable tool item type admitted by its start event.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum ToolKind {
    CommandExecution,
    FileChange,
    ToolCall,
}

/// Preserve provider-confirmed work lifetime independently of turn status.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
enum ToolExecutionLifetime {
    Foreground,
    Background,
}

/// Retain one item's original owner, descriptor, arguments, and native outcome.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "ToolExecutionWire")]
pub struct ToolExecution {
    pub address: TurnAddress,
    pub item_id: ItemId,
    pub descriptor: ToolDescriptor,
    pub input: String,
    pub outcome: Option<ToolOutcome>,
    kind: ToolKind,
    initial_descriptor: ToolDescriptor,
    final_snapshot: Option<ToolCompletion>,
    lifetime: ToolExecutionLifetime,
}

#[derive(Deserialize)]
struct ToolExecutionWire {
    address: TurnAddress,
    item_id: ItemId,
    descriptor: ToolDescriptor,
    input: String,
    outcome: Option<ToolOutcome>,
    kind: ToolKind,
    initial_descriptor: ToolDescriptor,
    final_snapshot: Option<ToolCompletion>,
    lifetime: ToolExecutionLifetime,
}

impl TryFrom<ToolExecutionWire> for ToolExecution {
    type Error = ToolStoreLoadError;

    /// Validate original normalized ownership and retained native terminal evidence on load.
    fn try_from(wire: ToolExecutionWire) -> Result<Self, Self::Error> {
        let ToolExecutionWire {
            address,
            item_id,
            descriptor,
            input,
            outcome,
            kind,
            initial_descriptor,
            final_snapshot,
            lifetime,
        } = wire;
        if address.thread_id != address.scope.owner().thread_id()
            || address.scope.turn_id().as_ref() != Ok(&address.turn_id)
        {
            return Err(ToolStoreLoadError::Owner);
        }
        if lifetime == ToolExecutionLifetime::Background && kind != ToolKind::CommandExecution {
            return Err(ToolStoreLoadError::ItemType);
        }
        if let Some(snapshot) = &final_snapshot
            && outcome.as_ref() != Some(&snapshot.outcome)
        {
            return Err(ToolStoreLoadError::FinalSnapshot);
        }
        Ok(Self {
            address,
            item_id,
            descriptor,
            input,
            outcome,
            kind,
            initial_descriptor,
            final_snapshot,
            lifetime,
        })
    }
}

impl ToolStore {
    /// Verify every admitted item retains the original turn selected by its containing exchange entry.
    pub(crate) fn belongs_to(&self, address: &TurnAddress) -> bool {
        self.execution
            .values()
            .all(|execution| &execution.address == address)
    }

    /// Remove retained output from an intentionally compacted display snapshot.
    pub(crate) fn omit_output(&mut self) {
        for tool in self.item.values_mut() {
            tool.output = super::ToolOutputText::default();
        }
    }
    /// Visit tool projections in their native admission order.
    pub(crate) fn iter(&self) -> impl Iterator<Item = &ToolCall> {
        self.order.iter().filter_map(|id| self.item.get(id))
    }
    /// Close unfinished foreground work without inventing tool success.
    pub(crate) fn finish(&mut self, outcome: super::TurnOutcome, now: ToolInstant) {
        for (id, execution) in &mut self.execution {
            if matches!(outcome, super::TurnOutcome::Completed { .. })
                && execution.lifetime == ToolExecutionLifetime::Background
            {
                continue;
            }
            let tool = self
                .item
                .get_mut(id)
                .expect("each admitted execution owns a projection");
            if tool.state != ToolState::Running {
                continue;
            }
            let terminal = match outcome {
                super::TurnOutcome::Failed { error, .. } => ToolOutcome::Failed {
                    diagnostic: crate::provider::ToolDiagnostic {
                        kind: error.kind(),
                        message: forge_provider_api::SanitizedMessage::new(
                            "Provider execution failed before tool completion",
                        )
                        .expect("static diagnostic is nonempty"),
                    },
                },
                super::TurnOutcome::Completed { .. } | super::TurnOutcome::Interrupted => {
                    ToolOutcome::Interrupted
                }
            };
            settle(tool, execution, terminal, now);
        }
    }
    /// Interrupt proven background work when exclusive recovery finds no native runtime owner.
    pub(crate) fn interrupt_detached_background(
        &mut self,
        observed_at: ToolInstant,
    ) -> Result<bool, crate::provider::ProviderError> {
        let mut changed = false;
        for (id, execution) in &mut self.execution {
            let tool = self
                .item
                .get_mut(id)
                .expect("each admitted execution owns a projection");
            if execution.lifetime == ToolExecutionLifetime::Background
                && tool.state == ToolState::Running
            {
                settle(tool, execution, ToolOutcome::Interrupted, observed_at);
                changed = true;
            }
        }
        Ok(changed)
    }
    /// Attribute a call once without moving historical ownership.
    pub(crate) fn attribute(&mut self, id: &str, checklist_item_id: &str) -> bool {
        let Some(tool) = self
            .item
            .iter_mut()
            .find_map(|(key, value)| (key.as_str() == id).then_some(value))
        else {
            return false;
        };
        if tool.checklist_item_id.is_some() {
            return false;
        }
        tool.checklist_item_id = Some(checklist_item_id.to_owned());
        true
    }
    /// Apply one typed item event atomically under its original turn and immutable item type.
    pub(crate) fn record_item(
        &mut self,
        address: &TurnAddress,
        item_id: &ItemId,
        event: &ItemEvent,
        now: ToolInstant,
    ) -> Result<(), crate::provider::ProviderError> {
        let existing = self.execution.get(item_id);
        let (mut execution, mut tool, admitted) = match existing {
            Some(execution) => {
                if execution.address != *address {
                    return Err(protocol());
                }
                (
                    execution.clone(),
                    self.item
                        .get(item_id)
                        .expect("each execution owns a projection")
                        .clone(),
                    false,
                )
            }
            None => {
                let ItemEvent::Started(start) = event else {
                    return Err(protocol());
                };
                let (kind, descriptor) = start_tool(start).ok_or_else(protocol)?;
                let execution = ToolExecution {
                    address: address.clone(),
                    item_id: item_id.clone(),
                    descriptor: descriptor.clone(),
                    input: String::new(),
                    outcome: None,
                    kind,
                    initial_descriptor: descriptor.clone(),
                    final_snapshot: None,
                    lifetime: ToolExecutionLifetime::Foreground,
                };
                let tool = ToolCall {
                    started_at_ms: None,
                    completed_at_ms: None,
                    started_active_ms: None,
                    completed_active_ms: None,
                    checklist_item_id: None,
                    id: item_id.to_string(),
                    kind: kind_label(kind).into(),
                    title: descriptor.title.clone(),
                    command_shell: command_shell(descriptor),
                    output: Default::default(),
                    status: "running".into(),
                    failed: false,
                    change: Default::default(),
                    state: ToolState::Running,
                };
                (execution, tool, true)
            }
        };
        match event {
            ItemEvent::Started(start) => {
                let (kind, descriptor) = start_tool(start).ok_or_else(protocol)?;
                if kind != execution.kind || *descriptor != execution.initial_descriptor {
                    return Err(protocol());
                }
            }
            ItemEvent::Tool { event } => match event {
                ToolItemEvent::DescriptorUpdated(descriptor) => {
                    execution.descriptor = descriptor.clone();
                    tool.title.clone_from(&descriptor.title);
                    tool.command_shell = command_shell(descriptor);
                }
                ToolItemEvent::TitleChanged(title) => {
                    tool.title.clone_from(title);
                    execution.descriptor.title.clone_from(title);
                }
                ToolItemEvent::Input(update) => match update {
                    ToolInputUpdate::Text(TextUpdate::Append(text)) => {
                        execution.input.push_str(text)
                    }
                    ToolInputUpdate::Text(TextUpdate::Replace(text)) => {
                        execution.input.clone_from(text)
                    }
                    ToolInputUpdate::Arguments(arguments) => {
                        execution.descriptor.arguments = arguments.clone()
                    }
                },
                ToolItemEvent::ExecutionStarted => {
                    if tool.state != ToolState::Running {
                        return Err(protocol());
                    }
                    if tool.started_at_ms.is_none() {
                        tool.started_at_ms = Some(now.wall_ms);
                        tool.started_active_ms = now.active_ms;
                    }
                }
                ToolItemEvent::Backgrounded => {
                    if execution.kind != ToolKind::CommandExecution
                        || tool.state != ToolState::Running
                        || tool.started_at_ms.is_none()
                    {
                        return Err(protocol());
                    }
                    execution.lifetime = ToolExecutionLifetime::Background;
                }
                ToolItemEvent::Output(TextUpdate::Append(text)) => tool.output.push_str(text),
                ToolItemEvent::Output(TextUpdate::Replace(text)) => tool.output.replace(text),
                ToolItemEvent::Changes(changes) => tool.change.clone_from(changes),
            },
            ItemEvent::Completed(completion) => {
                let (kind, completion) = finish_tool(completion).ok_or_else(protocol)?;
                if kind != execution.kind {
                    return Err(protocol());
                }
                if let Some(previous) = &execution.final_snapshot {
                    if previous != completion {
                        return Err(protocol());
                    }
                } else {
                    if execution
                        .outcome
                        .as_ref()
                        .is_some_and(|outcome| *outcome != completion.outcome)
                    {
                        return Err(protocol());
                    }
                    if let Some(descriptor) = &completion.descriptor {
                        execution.descriptor = descriptor.clone();
                        tool.title.clone_from(&descriptor.title);
                        tool.command_shell = command_shell(descriptor);
                    }
                    if let Some(output) = &completion.output {
                        tool.output.replace(output);
                    }
                    if !completion.changes.is_empty() {
                        tool.change.clone_from(&completion.changes);
                    }
                    settle(&mut tool, &mut execution, completion.outcome.clone(), now);
                    execution.final_snapshot = Some(completion.clone());
                }
            }
            ItemEvent::Message { .. } => return Err(protocol()),
        }
        if admitted {
            self.order.push(item_id.clone());
        }
        self.execution.insert(item_id.clone(), execution);
        self.item.insert(item_id.clone(), tool);
        Ok(())
    }
    /// Get a display projection by its normalized identity spelling.
    pub(crate) fn get(&self, id: &str) -> Option<&ToolCall> {
        self.item
            .iter()
            .find_map(|(key, value)| (key.as_str() == id).then_some(value))
    }
    /// Get retained execution metadata by its normalized identity spelling.
    pub fn execution(&self, id: &str) -> Option<&ToolExecution> {
        self.execution
            .iter()
            .find_map(|(key, value)| (key.as_str() == id).then_some(value))
    }
    /// Permit late completion only for the original running background item.
    pub(crate) fn accepts_background_completion(
        &self,
        address: &TurnAddress,
        item_id: &ItemId,
    ) -> bool {
        self.execution.get(item_id).is_some_and(|execution| {
            execution.address == *address
                && execution.lifetime == ToolExecutionLifetime::Background
                && self
                    .item
                    .get(item_id)
                    .is_some_and(|tool| tool.state == ToolState::Running)
        })
    }
}

/// Resolve only immutable tool starts; message items cannot enter this store.
fn start_tool(start: &ItemStart) -> Option<(ToolKind, &ToolDescriptor)> {
    match start {
        ItemStart::CommandExecution(descriptor) => Some((ToolKind::CommandExecution, descriptor)),
        ItemStart::FileChange(descriptor) => Some((ToolKind::FileChange, descriptor)),
        ItemStart::ToolCall(descriptor) => Some((ToolKind::ToolCall, descriptor)),
        ItemStart::UserMessage(_) | ItemStart::AgentMessage(_) | ItemStart::Reasoning(_) => None,
    }
}
/// Resolve only the matching tool completion alternatives.
fn finish_tool(completion: &ItemCompletion) -> Option<(ToolKind, &ToolCompletion)> {
    match completion {
        ItemCompletion::CommandExecution(completion) => {
            Some((ToolKind::CommandExecution, completion))
        }
        ItemCompletion::FileChange(completion) => Some((ToolKind::FileChange, completion)),
        ItemCompletion::ToolCall(completion) => Some((ToolKind::ToolCall, completion)),
        ItemCompletion::UserMessage(_)
        | ItemCompletion::AgentMessage(_)
        | ItemCompletion::Reasoning(_) => None,
    }
}
/// Project the immutable operation type at the display boundary.
fn kind_label(kind: ToolKind) -> &'static str {
    match kind {
        ToolKind::CommandExecution => "command",
        ToolKind::FileChange => "file_change",
        ToolKind::ToolCall => "tool_call",
    }
}
/// Get the adapter-normalized shell without parsing raw arguments.
fn command_shell(descriptor: &ToolDescriptor) -> Option<String> {
    match &descriptor.semantics {
        forge_provider_api::tool::ToolSemantics::Command { shell, .. } => shell.clone(),
        forge_provider_api::tool::ToolSemantics::Read { .. }
        | forge_provider_api::tool::ToolSemantics::Other => None,
    }
}
/// Freeze the first terminal timestamp and update its display projection.
fn settle(
    tool: &mut ToolCall,
    execution: &mut ToolExecution,
    outcome: ToolOutcome,
    now: ToolInstant,
) {
    let (state, status) = match &outcome {
        ToolOutcome::Succeeded => (ToolState::Completed, "completed"),
        ToolOutcome::Failed { .. } => (ToolState::Failed, "failed"),
        ToolOutcome::Denied { .. } => (ToolState::Denied, "denied"),
        ToolOutcome::Cancelled => (ToolState::Cancelled, "cancelled"),
        ToolOutcome::Interrupted => (ToolState::Interrupted, "interrupted"),
    };
    tool.state = state;
    tool.status = status.into();
    tool.failed = state == ToolState::Failed;
    if tool.completed_at_ms.is_none() {
        tool.completed_at_ms = Some(now.wall_ms.max(tool.started_at_ms.unwrap_or(now.wall_ms)));
        tool.completed_active_ms = tool
            .started_active_ms
            .zip(now.active_ms)
            .map(|(start, end)| end.max(start));
    }
    execution.outcome = Some(outcome);
}
/// Project an explicit lifecycle to its canonical display spelling.
fn state_label(state: ToolState) -> &'static str {
    match state {
        ToolState::Running => "running",
        ToolState::Completed => "completed",
        ToolState::Failed => "failed",
        ToolState::Denied => "denied",
        ToolState::Cancelled => "cancelled",
        ToolState::Interrupted => "interrupted",
    }
}
/// Classify a contradictory normalized item observation without retaining native payloads.
fn protocol() -> crate::provider::ProviderError {
    crate::provider::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::TurnStart,
        forge_provider_api::ProviderFailureKind::Protocol,
    ))
}
