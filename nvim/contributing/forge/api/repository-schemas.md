# Repository result and action schemas

The declarations below define repository document identities, captured actions, and native result shapes. A `Box` does not change JSON encoding. ID types encode as strings, revisions and counters as integers, and paths marked as repository paths use standard base64 to preserve arbitrary path bytes. Other local workspace paths use ordinary strings.

Optional output fields are null unless a `skip_serializing_if` attribute omits them. The Lua decoder removes null object members. A buffer patch applies only when its document identity and `base` revision match the replica. A status inventory revision, file generation, and body revision are separate fences. Keep the prior usable content if a response is stale or fails validation.

## StatusSnapshot

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, PartialEq, Serialize)]
/// Complete status inventory. Lua projects headers independently of native diff bodies.
pub struct StatusSnapshot {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: StatusView,
    pub diff_view: forge_diff::view::DiffViewOptions,
    pub head: StatusHead,
    pub context: Option<StatusContext>,
    pub section: Vec<StatusSectionRecord>,
    pub file: Vec<StatusFile>,
    pub pending: Vec<u64>,
}
```

## StatusView

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StatusView {
    Status,
    Local {
        path: String,
    },
    Comparison {
        title: String,
        path: Option<String>,
        worktree: bool,
    },
}
```

## StatusHead

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusHead {
    pub state: &'static str,
    pub reference: Option<String>,
    pub object: Option<String>,
}
```

## StatusSectionRecord

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusSectionRecord {
    pub kind: StatusSection,
    pub file: Vec<u64>,
}
```

## StatusFile

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusFile {
    pub header: Vec<forge_buffer::block::TextChunk>,
    pub id: u64,
    pub section: StatusSection,
    pub change: &'static str,
    pub path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
    pub untracked: bool,
    pub stats: StatusStatistics,
    pub generation: u64,
}
```

## StatusStatistics

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum StatusStatistics {
    Unknown,
    Exact { added: u64, deleted: u64 },
}
```

## StatusDelta

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, Serialize)]
/// Contiguous inventory change. Body revisions advance independently of this revision.
pub struct StatusDelta {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub document: DocumentId,
    pub base: DocumentRevision,
    pub next: DocumentRevision,
    pub removed: Vec<u64>,
    pub file: Vec<StatusFile>,
    pub section: Vec<StatusSectionRecord>,
    pub pending: Vec<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub head: Option<StatusHead>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<StatusContext>,
}
```

## StatusInput

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
/// Captured action with native identities instead of Lua header layout coordinates.
pub struct StatusInput {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: ViewId,
    pub sequence: InputSequence,
    pub action: String,
    pub location: StatusLocation,
}
```

## StatusLocation

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum StatusLocation {
    Boundary {
        after_files: bool,
    },
    File {
        id: u64,
    },
    Section {
        section: StatusSection,
    },
    Context {
        role: String,
    },
    Body {
        file: u64,
        generation: u64,
        revision: DocumentRevision,
        block: BlockId,
        position: TextPosition,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        target: Option<TargetId>,
    },
}
```

## StatusSelection

Source: [nvim/rust/forge/crates/forge-status/src/protocol.rs](../../../rust/forge/crates/forge-status/src/protocol.rs).

```rust
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StatusSelection {
    pub target: Vec<StatusLocation>,
}
```

## SourceOpen

Source: [nvim/rust/forge/crates/forge-status/src/source_document.rs](../../../rust/forge/crates/forge-status/src/source_document.rs).

```rust
#[derive(Debug, Serialize)]
pub struct SourceOpen {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub snapshot: BufferSnapshot,
    pub state: BodyState,
    pub more: bool,
    pub title: String,
    pub object: String,
    pub revision: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub syntax_diagnostic: Option<String>,
}
```

## SourceDelivery

Source: [nvim/rust/forge/crates/forge-status/src/source_document.rs](../../../rust/forge/crates/forge-status/src/source_document.rs).

```rust
#[derive(Debug, Serialize)]
pub struct SourceDelivery {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub patch: Option<BufferPatch>,
    pub state: BodyState,
    pub more: bool,
}
```

## SourceSnapshot

Source recovery returns its snapshot, committed controls, and continuation state
from the same document revision. Non-diff source documents omit `diff_view`.
Source: [source_document.rs](../../../rust/forge/crates/forge-status/src/source_document.rs).

```rust
#[derive(Debug, Serialize)]
pub struct SourceSnapshot {
    #[serde(flatten)]
    pub snapshot: BufferSnapshot,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub more: bool,
}
```

## BodyState

Source: [nvim/rust/forge/crates/forge-status/src/service.rs](../../../rust/forge/crates/forge-status/src/service.rs).

```rust
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(tag = "state", content = "diagnostic", rename_all = "snake_case")]
pub enum BodyState {
    Deferred,
    Loading,
    Partial,
    Ready,
    Unavailable(String),
    Failed(String),
}
```

## BodyDelivery

Source: [nvim/rust/forge/crates/forge-status/src/service.rs](../../../rust/forge/crates/forge-status/src/service.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct BodyDelivery {
    pub patch: Option<BufferPatch>,
    pub document: DocumentId,
    pub file: u64,
    pub generation: u64,
    pub snapshot: Option<BufferSnapshot>,
    pub state: BodyState,
    pub more: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub syntax_diagnostic: Option<String>,
}
```

## StatusOpenTarget

Source: [nvim/rust/forge/crates/forge-status/src/service.rs](../../../rust/forge/crates/forge-status/src/service.rs).

```rust
#[derive(Debug, Serialize)]
pub struct StatusOpenTarget {
    #[serde(flatten)]
    pub input: StatusInput,
    pub id: String,
    pub kind: &'static str,
    pub path: String,
    pub row: usize,
    pub column: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_revision: Option<String>,
}
```

## NavigationDelivery

Source: [nvim/rust/forge/crates/forge-status/src/service.rs](../../../rust/forge/crates/forge-status/src/service.rs).

```rust
#[derive(Serialize)]
pub struct NavigationDelivery {
    pub effect: Option<NavigationEffect>,
    pub body: Option<BodyDelivery>,
    pub more: bool,
}
```

## NavigationEffect

Source: [nvim/rust/forge/crates/forge-status/src/service.rs](../../../rust/forge/crates/forge-status/src/service.rs).

```rust
#[derive(Serialize)]
pub struct NavigationEffect {
    pub document: DocumentId,
    pub revision: forge_buffer::identity::DocumentRevision,
    pub view: forge_buffer::identity::ViewId,
    pub sequence: forge_buffer::identity::InputSequence,
    pub id: String,
    pub kind: &'static str,
    pub location: StatusLocation,
}
```

## ContextAction

Source: [nvim/rust/forge/crates/forge-status/src/context.rs](../../../rust/forge/crates/forge-status/src/context.rs).

```rust
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ContextAction {
    Commit { oid: String },
    PullRequest,
    About,
    Issues { text: String },
}
```

## BufferSnapshot

Source: [nvim/rust/forge/crates/forge-buffer/src/patch.rs](../../../rust/forge/crates/forge-buffer/src/patch.rs).

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BufferSnapshot {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub block: Vec<BufferBlock>,
}
```

## BufferPatch

Source: [nvim/rust/forge/crates/forge-buffer/src/patch.rs](../../../rust/forge/crates/forge-buffer/src/patch.rs).

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BufferPatch {
    pub document: DocumentId,
    pub base: DocumentRevision,
    pub next: DocumentRevision,
    pub base_rows: usize,
    pub next_rows: usize,
    pub base_blocks: usize,
    pub next_blocks: usize,
    pub text_edit: Vec<TextEdit>,
    pub metadata_edit: Vec<MetadataEdit>,
    pub removed_block: Vec<BlockId>,
    pub block_edit: Vec<BlockOrderEdit>,
}
```

## DocumentInput

Source: [nvim/rust/forge/crates/forge-buffer/src/input.rs](../../../rust/forge/crates/forge-buffer/src/input.rs).

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DocumentInput {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: ViewId,
    pub sequence: InputSequence,
    pub action: String,
    pub block: BlockId,
    pub position: TextPosition,
    pub target: Option<TargetId>,
}
```

## WriteRequest

Source: [nvim/rust/forge/src/repository_write.rs](../../../rust/forge/src/repository_write.rs).

```rust
#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum WriteRequest {
    Trace {
        after: Option<u64>,
    },
    Prepare {
        workspace: PathBuf,
        action: WriteAction,
    },
    Submit {
        intent: String,
    },
    Acknowledge {
        operation_id: u64,
    },
    Cancel {
        intent: Option<String>,
        operation_id: Option<u64>,
    },
}
```

## WriteAction

Source: [nvim/rust/forge/src/repository_write.rs](../../../rust/forge/src/repository_write.rs).

```rust
#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub(crate) enum WriteAction {
    CreateBranch {
        name: String,
    },
    CommitEditor {
        command: String,
        nvim_server: String,
        #[serde(default)]
        amend: bool,
    },
    Push,
    PublishBranch {
        name: String,
        head: Option<String>,
    },
    Pull,
}
```

## WalkthroughSource

Source: [nvim/rust/forge/src/walkthrough_routes.rs](../../../rust/forge/src/walkthrough_routes.rs).

```rust
#[derive(Serialize)]
struct WalkthroughSource {
    #[serde(flatten)]
    source: forge_status::source_document::SourceOpen,
    annotation: forge_buffer::patch::BufferSnapshot,
    source_row: usize,
    stale: bool,
    review: bool,
}
```

