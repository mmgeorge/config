# Forge migration design and acceptance requirements

This is the retained target specification. Proposed types, files, and flows are requirements,
not claims that they already exist. Update requirements only when a design decision changes.
Current status belongs in [impl-status.md](../../../impl-status.md). Do not append execution history here.

# Overview

Forge moves the former DiffReview plugin's repository work, diff analysis, syntax analysis, feature state, and buffer
row generation into one Rust executable shared by ForgeStatus and Harness. The current implementation
already uses asynchronous processes, native folds, lazy file bodies, and selective line edits, but
Lua still rebuilds feature models and performs expensive analysis on Neovim's main thread.
The migration gives Rust ownership of computation and document state while Lua owns Neovim resources,
native editing, and the mechanical application of versioned buffer changes.

# Usage

The existing command opens the same ForgeStatus surface. File headers appear before optional counts
and previews finish, and opening one file inserts only that file's requested rows.

~~~text
:ForgeStatus
<Tab> on an unstaged file
~~~

Expected interaction:

~~~text
The status buffer contains branch metadata, sections, and file headers.
The selected file shows its first bounded set of change rows.
Other files keep their text, marks, and loaded state.
Scrolling within expanded content automatically loads rows for the viewport plus lookahead.
Multiple loaded batches remain visible together without another expansion action.
Closing and reopening a loaded fold does not reload its source.
~~~

Harness uses the same running Forge backend. Its typed repository tools operate on a repository and
comparison request without opening ForgeStatus or simulating cursor input.

~~~text
:ForgeHarness
~~~

Expected interaction:

~~~text
The existing session, transcript, composer, permissions, and plan-review workflow remain available.
A Harness repository inspection shares Forge's repository handles and immutable diff results.
Provider processes remain separate children where their existing adapters require them.
~~~

These rules are deliberate behavior changes or preserved requirements, not measured results:

- The first ForgeStatus response contains no collapsed file body. Exact counts can remain pending.
- Added and deleted previews use content directly, without a line-diff calculation.
- Deleted files never trigger speculative body or syntax work from cursor movement.
- A collapsed file qualifies for speculative work only with an already known delta below 100 lines.
  Unknown counts do not authorize speculative computation to discover eligibility.
- Full added or deleted previews above 1,000 source lines show an unavailable row. Whole-file actions
  remain available where they are valid, even when a preview is unavailable.
- The base status buffer publishes all section and file headers together, without pagination.
- Expanded diff content follows visible demand plus lookahead. Each internal delivery batch admits
  at most 256 body rows, further limited by bytes and decorations. A batch is not a user-visible page
  or a hunk boundary. Loading continues automatically across batches until demand is covered.
- Revision completion returns cached candidates immediately and refreshes asynchronously when stale.
- All nine currently registered Forge commands retain their current interface. The two commands
  that accept file plus revision currently use nargs="+" and consume the first two arguments.
  Tightening them to exactly two arguments is outside this migration.
- Harness tool expansion preserves the current request to reveal complete output. Large output
  arrives incrementally after that one request. It does not acquire a new manual pagination command.

Implementation is authorized. The root impl-status.md records current status and remaining work.
This document specifies the accepted target design, not the current implementation.
A renamed module does not establish Rust ownership.

# Diagrams

Feature crates own their domain state and use a common buffer contract. RepositoryStore shares
repository resources, DiffEngine owns immutable analysis results, and ForgeRuntime composes these
owners without adding another service layer or another independently deployed binary.

## Object model and ownership

A block is an independently editable range of generated buffer text with an opaque identity.
BufferDocument understands block ordering and coordinates, while feature documents interpret
opaque action targets as files, hunks, comments, or Harness entries.

The two columns have independent rows. An unqualified type means owned state, @Type means retained
shared ownership, &Type means a retained borrow, Type? means optional state, and Type[] means a
collection. Indented child declarations are intentional private implementation details.

~~~text
Contracts                                   Concrete

*trait RepositoryReader                     *struct ForgeRuntime
  [nvim/rust/forge/crates/forge-git/            [nvim/rust/forge/src/runtime.rs]
   src/reader.rs]                             - repositories: @RepositoryStore
  + observe(request): RepositoryObservation    - analysis: @DiffEngine
  + content(request): ContentResult            - status: StatusService
  + revisions(request): RevisionCandidates     - review: ReviewService
                                              - github: @GithubService
*trait RepositoryWriter                       - harness: HarnessService
  [nvim/rust/forge/crates/forge-git/            + dispatch(request): Response
   src/writer.rs]
  + execute(request): WriteOutcome           *struct RepositoryStore
                                              [nvim/rust/forge/crates/forge-git/
*trait GithubRemote                            src/store.rs]
  [nvim/rust/forge/crates/forge-github/         - worktree_map: @RepositoryState[]
   src/remote.rs]                             - writes: MutationCoordinator
  + read(request): RemoteResult               + open(path): @RepositoryState
  + mutate(request): RemoteOutcome
                                            *struct RepositoryState
*trait Backend                                [nvim/rust/forge/crates/forge-git/
  [nvim/rust/forge/crates/forge-harness/         src/repository.rs]
   src/backend/mod.rs]                        - identity: RepositoryIdentity
  + descriptor(): BackendDescriptor           - reader: GitReader
  + prompt_stream(request, events):           - observation: RepositoryObservation?
    Result<BackendOutput>
                                              - revision_candidates: RevisionCandidates?

                                            *struct GitReader: RepositoryReader
                                              [nvim/rust/forge/crates/forge-git/
                                               src/reader.rs]
                                              - repository: gix::ThreadSafeRepository
                                              - capabilities: ReadCapabilities
                                              - compatibility: GitReadProcess

                                            *struct GitWriter: RepositoryWriter
                                              [nvim/rust/forge/crates/forge-git/
                                               src/writer.rs]
                                              - executable: ExecutablePath

                                            *struct DiffEngine
                                              [nvim/rust/forge/crates/forge-diff/
                                               src/engine.rs]
                                              - cache: AnalysisCache
                                              - workers: AnalysisPool
                                              + compare(request): DiffResult
                                              + syntax(request): SyntaxResult

                                            *struct BufferDocument
                                              [nvim/rust/forge/crates/forge-buffer/
                                               src/document.rs]
                                              - id: DocumentId
                                              - revision: DocumentRevision
                                              - blocks: BlockSequence
                                              + edit(change): BufferPatch
                                              + locate(row): BlockPosition
                                              + snapshot(): BufferSnapshot

                                              *struct BlockSequence
                                                [nvim/rust/forge/crates/forge-buffer/
                                                 src/sequence.rs]
                                                - root: SequenceNode?
                                                - locator: BlockLocator
                                                + splice(change): SequenceChange
                                                + locate(row): BlockPosition

                                            *struct BufferBlock
                                              [nvim/rust/forge/crates/forge-buffer/
                                               src/block.rs]
                                              - id: BlockId
                                              - text: BufferText
                                              - decorations: Decoration[]
                                              - targets: TargetRange[]
                                              - regions: EditableRegion[]

                                            *struct StatusDocument
                                              [nvim/rust/forge/crates/forge-status/
                                               src/document.rs]
                                              - repository: @RepositoryState
                                              - confirmed: RepositoryObservation
                                              - journal: OperationJournal
                                              - files: StatusFile[]
                                              - demand: ViewDemand[]
                                              - buffer: BufferDocument
                                              + command(request): StatusResult
                                              + visible_changed(demand): StatusResult

                                            *struct ViewDemand
                                              [nvim/rust/forge/crates/forge-buffer/
                                               src/demand.rs]
                                              - view_id: ViewId
                                              - revision: DocumentRevision
                                              - visible: BlockRange[]
                                              - expanded: FoldId[]
                                              - height: ScreenRows

                                            *struct GithubService
                                              [nvim/rust/forge/crates/forge-github/
                                               src/service.rs]
                                              - remote: @GhClient
                                              - issue_store: IssueStore
                                              - mutation_queue: RemoteQueue

                                            *struct GhClient: GithubRemote
                                              [nvim/rust/forge/crates/forge-github/
                                               src/remote.rs]
                                              - executable: ExecutablePath

                                            *struct ReviewDocument
                                              [nvim/rust/forge/crates/forge-review/
                                               src/review.rs]
                                              - remote: PullRequest
                                              - edits: EditStore
                                              - comments: CommentStore
                                              - buffer: BufferDocument

                                            *struct HarnessService
                                              [nvim/rust/forge/crates/forge-harness/
                                               src/service.rs]
                                              - sessions: SessionControllerRegistry
                                              - repositories: @RepositoryStore
                                              - analysis: @DiffEngine
                                              + dispatch(request): HarnessResult
                                              + repository_tools(): RepositoryTools

                                            *struct HarnessDocument
                                              [nvim/rust/forge/crates/forge-harness/
                                               src/buffer/mod.rs]
                                              - session_id: SessionId
                                              - timeline_revision: TimelineRevision
                                              - buffer: BufferDocument

                                            *struct BufferSession
                                              [nvim/lua/forge/buffer.lua]
                                              - document_id: DocumentId
                                              - applied_revision: DocumentRevision
                                              - buffer: NeovimBuffer
                                              - windows: WindowState[]
                                              - local_edits: PendingEdit[]
                                              + apply(patch): ApplyResult

                                            *enum ApplyResult
                                              [nvim/lua/forge/buffer.lua]
                                              Applied
                                                + revision: DocumentRevision
                                              Deferred
                                                + edit_sequence: EditSequence
                                              Desynchronized
                                                + diagnostic: String
~~~

Backend keeps its complete existing contract during relocation. The diagram shows only its
provider-description and event-stream operations, not a replacement interface. BufferSession is the
Lua-owned runtime record, not a Rust type or a Neovim buffer implementation.

## Code flow: input, analysis, and buffer changes

The input identifies the displayed revision and an opaque target or position. Rust resolves that
input against the same revision, then commits a result only if its source and document still match.
Viewport reports follow the same route. StatusDocument resolves generic visible ranges into expanded
file demand and continues bounded delivery automatically, without opening collapsed files.

~~~text
Input                  Resolve                  Read and analyze
[forge/input.lua] ----> [StatusDocument] -------> [RepositoryStore + DiffEngine]
     command or ViewDemand    target + source identities             |
                                                                    | result
Apply                  Validate patch           Commit current result|
[Neovim APIs] <-------- [forge/buffer.lua] <----- [StatusDocument] <---+
              edits + metadata           BufferPatch

Recover                Replace synchronized state
[forge/client.lua] --> [BufferDocument.snapshot] --> [forge/buffer.lua]
             desynchronized revision          complete snapshot
~~~

## Code flow: repository write and verification

Admission belongs to the repository mutation coordinator. A cancelled caller cannot release the
write permit while Git still runs or cause the client to repeat an uncertain write.

~~~text
Stage input             Admit                     Execute
[StatusDocument] -----> [MutationCoordinator] ----> [GitWriter]
                 intent + preconditions       permit + validated patch
                              |                       |
                              | admitted              | WriteOutcome
                              v                       v
Optimistic rows         Reconcile                 Verify after settle
[StatusDocument] <----- [OperationJournal] <------ [RepositoryStore]
                 BufferPatch or unchanged     observed affected paths
~~~

## Code flow: Harness events and direct repository tools

Harness retains session and provider ownership. Its repository tools call the shared Rust services,
and its transcript consumes ordered lifecycle events without routing through a ForgeStatus document.

~~~text
Submit                   Run existing session       Call repository tool
[Composer adapter] ----> [HarnessService] ----------> [RepositoryTools]
                    prompt + session       typed read request |
                               |                              | direct Rust call
                               | provider events              v
                               |                    [RepositoryStore + DiffEngine]
                               v
Reduce and persist       Render changed blocks      Apply ordered updates
[Harness feature state] -> [HarnessDocument] ------> [forge/buffer.lua]
                  timeline change             BufferPatch
~~~

# Tasks

Tasks are ownership changes, not estimates of implementation effort. Each task ends with its named
tests and evidence. The execution gates in the next section define the actual dependency order and
the points where production entrypoints switch.

Every new public type and operation specified below belongs to a concrete file group. Existing
Harness modules move mechanically with their full contents and tests before semantic edits.
Task 1 records that relocation inventory so a renamed package cannot silently omit a private module,
test helper, asset, schema, or build input.

1. Establish a verified migration baseline and the Forge workspace. The baseline records current
   behavior and separates deliberate changes from accidental regressions. The workspace uses one
   root package plus eight library crates, with no app directory.

file nvim/rust/forge/Cargo.toml
└─ Create the package and workspace as a modular monolith.
   ├─ Add config package forge with one executable at src/main.rs
   ├─ Add config workspace members forge-buffer, forge-diff, forge-git, forge-github,
   │  forge-status, forge-review, forge-harness, and forge-protocol
   ├─ Add config shared dependency versions and inherited lints
   ├─ Add config edition 2024 and an MSRV validated against the selected dependency graph
   ├─ Add config test and benchmark targets without additional service binaries
   └─ Add config release symbols for profiling and crash diagnosis

file nvim/rust/forge/Cargo.lock
└─ Configure reproducible dependencies without upgrading providers during relocation.
   ├─ Add config exact resolved dependencies for the complete workspace
   ├─ Modify config duplicate dependencies only after API and provider tests pass
   └─ Add config locked-build enforcement in the verification workflow

file nvim/rust/forge/migration.toml
└─ Create a machine-checkable baseline and cutover inventory.
   ├─ Add config source revision and hashes for modified or untracked baseline files
   ├─ Add config public commands, argument behavior, keymaps, facade APIs, and configuration keys
   ├─ Add config current source files, exported symbols, test cases, assets, and destination owner
   ├─ Add config current durable paths, format versions, schemas, and external executable contracts
   ├─ Add config grammar inventory, query origins, licenses, and filename aliases
   └─ Add config intentional behavior changes and evidence required for each cutover

file nvim/rust/forge/tests/migration_inventory.rs
└─ Create completeness checks independent of rendered UI snapshots.
   ├─ Add test every_baseline_file_and_test_has_a_destination_or_explicit_removal
   ├─ Add test every_public_command_retains_its_recorded_contract
   ├─ Add test every_durable_path_has_one_declared_owner
   └─ Add test every_removed_export_has_zero_remaining_production_callers

2. Move Harness into a library and compose one owning runtime. Preserve the existing session,
   provider, permission, storage, checkpoint, plan, goal, and tool contracts during the package move.
   Opening ForgeStatus must not initialize a provider, acquire a Harness session lease, or open every
   durable store.

file nvim/rust/forge/crates/forge-harness/Cargo.toml
└─ Move the existing Harness package into a library workspace member.
   ├─ Modify config package name from diff-review-harness to forge-harness
   ├─ Modify config dependencies to inherit versions without changing provider capabilities
   ├─ Remove config standalone binary target from the library package
   └─ Modify config fixture and asset paths relative to the new package location

file nvim/rust/forge/crates/forge-harness/src/lib.rs
└─ Move every existing feature module and its private descendants intact.
   ├─ Modify module agent, backend, broker, checkpoint, control_tools, goal, and interaction paths
   ├─ Modify module permissions, plan, protocol, rustdoc, session, storage, timeline, trace,
   │  and workspace paths
   ├─ Add module service for session-controller ownership extracted from the old executable
   └─ Modify config include paths for schemas, permission fixtures, and other compile-time assets

file nvim/rust/forge/crates/forge-harness/src/service.rs
└─ Extract session routing without duplicating the existing broker state.
   ├─ Modify struct SessionController and struct SessionControllerRegistry from the old main.rs
   ├─ Modify struct ProviderForkGate with its existing capability and completion behavior
   ├─ Add struct HarnessService with lazy provider runtime and session registry
   ├─ Add fn HarnessService::open_session to resolve the exact workspace and current-format record
   ├─ Add fn HarnessService::dispatch to route session operations through the existing broker
   ├─ Add fn HarnessService::cancel to retain the independent active-turn control lane
   └─ Add fn HarnessService::shutdown to stop turns and release leases after their owners finish

file nvim/rust/forge/src/runtime.rs
└─ Compose feature owners without a global lock around requests.
   ├─ Add struct ForgeRuntime with shared RepositoryStore and DiffEngine
   ├─ Add field status, review, github, and harness with lazy initialization of external resources
   ├─ Add fn ForgeRuntime::new to construct owners without opening a provider or repository
   ├─ Add fn ForgeRuntime::dispatch to route typed requests to their owning service
   └─ Add fn ForgeRuntime::shutdown to coordinate feature-specific termination

file nvim/rust/forge/src/main.rs
└─ Create one executable with explicit process roles.
   ├─ Add enum ForgeMode with Nvim and ControlMcpAdapter variants
   ├─ Add fn main to initialize Tokio and select the requested role
   ├─ Add fn run_nvim to own the only feature runtime for one Neovim instance
   └─ Add fn run_control_mcp_adapter to retain the stateless existing adapter behavior

file nvim/rust/forge/tests/harness_broker_stdio.rs
└─ Move the complete existing broker stdio suite to the executable package.
   ├─ Modify test process fixtures to launch the Forge executable
   ├─ Modify test streams_mock_backend_events_before_the_jsonl_response
   ├─ Modify test opens_a_fork_before_provider_preparation_and_queues_only_the_child_prompt
   ├─ Modify test creates_a_new_session_while_the_source_turn_continues
   ├─ Modify test cancels_a_running_turn_through_the_out_of_band_request_lane
   ├─ Modify test restarts_a_running_turn_in_write_mode_without_creating_another_interaction
   ├─ Modify test retracts_an_output_free_planning_turn_and_restores_control_state
   ├─ Modify test visible_output_prevents_cancelled_turn_retraction
   ├─ Modify test workspace_changes_prevent_cancelled_turn_retraction
   ├─ Modify test persists_acknowledged_steering_on_the_active_interaction
   └─ Modify test reports_structured_lease_recovery_and_allows_a_new_session

file nvim/rust/forge/crates/forge-harness/tests/permission_document.rs
└─ Move permission fixtures without changing the schema or evaluator.
   ├─ Modify fn broker_fixture for the relocated package
   ├─ Modify test repository_permission_document_matches_the_runtime_schema
   ├─ Modify test opens_validates_and_atomically_replaces_the_permission_document
   └─ Modify test cycles_explicit_modes_without_changing_mode_during_plan_control

file nvim/rust/forge/crates/forge-harness/tests/codex_cli.rs
└─ Move the complete installed-Codex suite with its existing ignored annotations.
   ├─ Modify fn git and fn request for the relocated package
   ├─ Modify test lists_backend_owned_codex_model_metadata
   ├─ Modify test lists_provider_skills_and_complete_mcp_rows
   ├─ Modify test plans_without_writing_then_executes_and_forks_in_a_temporary_repository
   ├─ Modify test streams_structured_child_agent_lifecycle
   └─ Modify test broker_runs_the_configured_local_code_explorer_to_parent_completion

file nvim/rust/forge/crates/forge-harness/tests/codex_real.rs
└─ Move authenticated Codex planning tests with their original prerequisites.
   ├─ Modify fn initialize_git_workspace, fn trace_tail, and fn initialize_real_codex_broker
   ├─ Modify test asks_for_feedback_then_creates_a_plan_without_native_collaboration_mode
   └─ Modify test geoparquet_prompt_reaches_plan_review_after_scope_feedback

file nvim/rust/forge/crates/forge-harness/tests/copilot_real.rs
└─ Move authenticated Copilot tests with their original prerequisites.
   ├─ Modify test lists_native_copilot_skills_and_mcp_rows
   └─ Modify test streams_one_native_copilot_sdk_turn_through_the_broker

3. Define a feature-neutral buffer document with honest update costs. Feature owners produce rows
   and interpret target identities. The shared buffer crate does not contain enums for Git files,
   PR comments, or Harness lifecycle states.

file nvim/rust/forge/crates/forge-buffer/Cargo.toml
└─ Create a synchronous library for text, coordinates, and presentation records.
   ├─ Add config serialization, error, and property-test dependencies
   └─ Add config prohibition on Git, providers, Neovim handles, and runtime dependencies

file nvim/rust/forge/crates/forge-buffer/src/id.rs
└─ Create identities whose lifetime and coordinate domain are explicit.
   ├─ Add struct DocumentId and struct DocumentRevision
   ├─ Add struct BlockId, struct TargetId, struct RegionId, and struct FoldId
   ├─ Add struct ViewId, struct InputSequence, and struct EffectId
   └─ Add struct EditSequence and struct RegionRevision

file nvim/rust/forge/crates/forge-buffer/src/text.rs
└─ Encapsulate generated rows without conflating them with source-file newline metadata.
   ├─ Add struct BufferText with UTF-8 text and internal row offsets
   ├─ Add fn BufferText::from_rows to preserve empty rows exactly
   ├─ Add fn BufferText::row and fn BufferText::slice to borrow text ranges
   └─ Add fn BufferText::wire_rows to expose exact rows without serializing internal offsets

file nvim/rust/forge/crates/forge-buffer/src/block.rs
└─ Create independently editable blocks without embedding domain state.
   ├─ Add struct BufferBlock with id, text, decorations, targets, and editable regions
   ├─ Add struct TargetRange with an opaque TargetId and relative byte coordinates
   ├─ Add struct BlockPosition with BlockId and relative row and byte column
   ├─ Add fn BufferBlock::validate to reject out-of-range metadata
   └─ Add fn BufferBlock::target_at to return an opaque target without interpreting its meaning

file nvim/rust/forge/crates/forge-buffer/src/sequence.rs
└─ Encapsulate a dynamic order-statistic sequence behind one private boundary.
   ├─ Add struct BlockSequence with chunked balanced nodes and subtree row and byte totals
   ├─ Add struct SequenceNode and struct BlockLocator as private implementation state
   ├─ Add struct SequenceChange with inserted, removed, and retained block identities
   ├─ Add fn BlockSequence::splice for insertions, removals, and replacements
   ├─ Add fn BlockSequence::locate to map a row to one block
   └─ Add fn BlockSequence::position to map one retained block to its current row

file nvim/rust/forge/crates/forge-buffer/src/patch.rs
└─ Create text edits and metadata changes against one exact base revision.
   ├─ Add struct BufferSnapshot with complete document text, metadata, and revision
   ├─ Add struct BufferPatch with document, base, next, text edits, and metadata edits
   ├─ Add struct TextEdit with base start row, removed row count, and replacement rows
   ├─ Add struct MetadataEdit with stable block identity and replacement metadata
   ├─ Add fn BufferPatch::validate to reject overlapping edits and inconsistent row counts
   └─ Add fn apply_reference to test the same edit contract without Neovim

file nvim/rust/forge/crates/forge-buffer/src/document.rs
└─ Centralize generated document revisions and local reconciliation.
   ├─ Add struct BufferDocument with identity, revision, and BlockSequence
   ├─ Add fn BufferDocument::edit to emit disjoint edits only for changed blocks
   ├─ Add fn BufferDocument::locate to resolve a position at a retained revision
   ├─ Add fn BufferDocument::snapshot to emit the current complete generated state
   └─ Add fn BufferDocument::accept_local_edit to advance acknowledged editable-region state

file nvim/rust/forge/crates/forge-buffer/src/presentation.rs
└─ Create reusable Neovim presentation instructions without direct API calls.
   ├─ Add struct Decoration with relative rows, byte ranges, capture name, priority, and flags
   ├─ Add struct Gutter with inline virtual-text segments
   ├─ Add struct FoldSpec with stable identity, range, label, and default intent
   ├─ Add struct EditableRegion with identity, revision, anchors, and edit policy
   ├─ Add struct WidthProfile with layout width, tab policy, and display-cell conventions
   ├─ Add fn coalesce_spans to merge only identical adjacent styles
   └─ Add fn validate_presentation to check ranges, nesting, and byte boundaries

file nvim/rust/forge/crates/forge-buffer/src/effect.rs
└─ Create finite editor effects with explicit lifetime checks.
   ├─ Add enum ClientEffect for navigation, file open, URL open, chooser, prompt, notice, and focus
   ├─ Add struct EffectContext with EffectId, document, revision, ViewId, and InputSequence
   └─ Add fn validate_effect to distinguish stale navigation from durable operation results

file nvim/rust/forge/crates/forge-buffer/tests/document.rs
└─ Create a reference-model suite for the buffer contract.
   ├─ Add test randomized_splices_match_the_reference_row_vector
   ├─ Add test dynamic_insertions_preserve_block_identity_and_row_lookup
   ├─ Add test metadata_only_changes_do_not_emit_text
   ├─ Add test append_to_a_large_block_does_not_resend_its_prefix
   ├─ Add test zero_rows_empty_rows_and_trailing_empty_rows_are_distinct
   └─ Add test invalid_patch_validation_leaves_the_reference_document_unchanged

4. Establish a bounded transport that preserves ordering and reports uncertain outcomes. One host
   owns stdin and stdout, but each feature serializes its own state changes. The transport must
   stop memory growth on both sides of the pipe and cannot treat a response timeout as proof that
   a write did not happen.

file nvim/rust/forge/crates/forge-protocol/Cargo.toml
└─ Create the shared wire schema without importing feature implementations.
   ├─ Add config serde, serde_json, thiserror, and forge-buffer dependencies
   └─ Add config protocol fixtures checked against the Lua decoder

file nvim/rust/forge/crates/forge-protocol/src/message.rs
└─ Create one versioned JSONL connection for the Neovim host.
   ├─ Add struct Hello with protocol version, client identity, build expectation, and limits
   ├─ Add struct Welcome with server identity, selected limits, and feature capabilities
   ├─ Add struct Request with request ID, service, method, and typed payload discriminator
   ├─ Add config visible_changed request payload using the generic ViewDemand contract
   ├─ Add struct Response with request ID and exactly one result or structured error
   ├─ Add struct DocumentEvent with document, base revision, next revision, and payload
   ├─ Add enum ControlMessage for cancel, acknowledge, suspend, resume, and shutdown
   └─ Add struct OperationReceipt with operation ID and pending, completed, or unknown outcome

file nvim/rust/forge/crates/forge-protocol/src/codec.rs
└─ Configure framing before decoding or retaining complete messages.
   ├─ Add struct JsonLineDecoder with a bounded partial-frame buffer
   ├─ Add fn JsonLineDecoder::push to handle split UTF-8 and multiple frames in one read
   ├─ Add fn encode_frame to enforce encoded byte limits before enqueueing
   ├─ Add fn decode_frame to validate shape, nesting, and collection counts
   ├─ Add struct SnapshotPart with transfer identity, sequence, and bounded payload
   ├─ Add struct SnapshotEnd with total part count, bytes, and document revision
   └─ Add fn split_snapshot to transfer complete bounded parts with an explicit final marker

file nvim/rust/forge/crates/forge-protocol/src/limits.rs
└─ Centralize negotiated resource limits and saturation results.
   ├─ Add struct TransportLimits with frame, queue, in-flight, snapshot, and receive-credit bounds
   ├─ Add enum CapacityError with busy, oversized, and slow-consumer outcomes
   └─ Add fn TransportLimits::validate to reject incompatible or zero progress limits

file nvim/rust/forge/src/host.rs
└─ Encapsulate transport progress from feature and worker execution.
   ├─ Add struct ConnectionHost with one writer and a bounded request registry
   ├─ Add fn read_requests to validate before dispatch
   ├─ Add fn write_messages to preserve ordering within each document stream
   ├─ Add fn grant_credit to track acknowledged client consumption
   ├─ Add fn cancel_request to signal the operation owner without abandoning its permit
   └─ Add fn shutdown to stop admission and join owners within the shutdown policy

file nvim/rust/forge/src/router.rs
└─ Route wire requests into typed feature calls at the executable boundary.
   ├─ Add enum RoutedRequest for lifecycle, status, review, GitHub, and Harness requests
   ├─ Add fn decode_request to reject unknown methods before touching feature state
   ├─ Add fn route_request to attach cancellation, provenance, and operation receipts
   └─ Add fn encode_result to preserve structured errors and terminal-response ordering

file nvim/lua/forge/client.lua
└─ Create one lazy Forge client per Neovim instance.
   ├─ Add class ForgeClient with process generation, bounded receive queue, and request map
   ├─ Add fn M.start to launch and handshake once for concurrent callers
   ├─ Add fn M.request to register a callback before writing the request
   ├─ Add fn M.consume to parse bounded chunks outside the rendering callback
   ├─ Add fn M.drain to schedule bounded batches and return receive credit
   ├─ Add fn M.recover_document to request a current snapshot without replaying mutations
   ├─ Add fn M.reset_process to fail callbacks and invalidate old-generation state
   └─ Add fn M.stop to close the owned process and report incomplete operations

file nvim/lua/forge/protocol.lua
└─ Resolve the same wire contract without decoding domain models in Lua.
   ├─ Add fn M.encode_request and fn M.decode_frame
   ├─ Add fn M.validate_patch and fn M.validate_snapshot_part
   └─ Add fn M.finish_snapshot to expose only complete validated transfers

file nvim/rust/forge/tests/protocol_stdio.rs
└─ Create real-pipe tests with slow readers and fragmented writes.
   ├─ Add test fragmented_frames_decode_without_unbounded_fragments
   ├─ Add test control_traffic_does_not_overtake_its_own_document_dependencies
   ├─ Add test slow_client_exhausts_credit_before_memory_growth
   ├─ Add test partial_snapshot_never_changes_applied_revision
   ├─ Add test timeout_reports_unknown_write_outcome_without_retry
   └─ Add test old_process_frames_cannot_complete_new_process_requests

5. Keep Lua mechanically aware of buffer structure and native editing. Lua needs block anchors,
   editable ranges, fold ranges, and current revisions to operate synchronously. It does not need
   a second ForgeStatus file tree or a second Harness timeline reducer.

file nvim/lua/forge/buffer.lua
└─ Create a revision-checked physical buffer adapter.
   ├─ Add class BufferSession with document identity, applied revision, and native handles
   ├─ Add enum ApplyResult with Applied, Deferred, and Desynchronized outcomes
   ├─ Add fn M.open to create the correct filetype and buffer lifecycle
   ├─ Add fn M.preflight to validate all edits, metadata, handles, and changedtick
   ├─ Add fn M.apply_patch to perform bottom-up edits in one scheduled callback
   ├─ Add fn M.apply_snapshot to recover a read-only document without claiming rollback
   ├─ Add fn M.fail_apply to invalidate the session after any partial API failure
   └─ Add fn M.close to detach callbacks, streams, namespaces, and per-window state

file nvim/lua/forge/input.lua
└─ Route commands through opaque targets with freshness and window identity.
   ├─ Add fn M.command to attach DocumentRevision, ViewId, and InputSequence
   ├─ Add fn M.selection to capture characterwise, linewise, and blockwise selections
   ├─ Add fn M.target_anchor to retain a generic BlockId and local position
   ├─ Add fn M.cursor_changed to coalesce obsolete prewarm requests
   ├─ Add fn M.visible_changed to report revision, view, generic ranges, height, and fold intent
   ├─ Add fn M.schedule_visible to coalesce scroll, resize, fold, and post-apply reports outside redraw
   └─ Add fn M.install to reuse the current configurable command specification

file nvim/lua/forge/decorations.lua
└─ Encapsulate unchanged metadata stable when rows move.
   ├─ Add class DecorationCache keyed by BlockId with relative style spans
   ├─ Add fn M.replace_block to update only the block's changed records
   ├─ Add fn M.visible_rows to resolve the visible range through generic block anchors
   ├─ Add fn M.emit to install cached ephemeral highlights during redraw
   ├─ Add fn M.gutters to maintain persistent inline virtual text
   └─ Add fn M.colorscheme_changed to relink capture names without reparsing source

file nvim/lua/forge/folds.lua
└─ Reuse native fold intent separately for every window.
   ├─ Add class WindowFoldState keyed by ViewId and FoldId
   ├─ Add fn M.apply_specs to install changed structural fold ranges
   ├─ Add fn M.toggle_loaded to operate on known native fold ranges synchronously
   ├─ Add fn M.request_unloaded to ask Forge for absent rows
   ├─ Add fn M.capture_changed_subtree to avoid scanning all folds on every text edit
   └─ Add fn M.restore_changed_subtree to preserve unrelated window-local intent

file nvim/lua/forge/editable.lua
└─ Split native editing from generated-text synchronization.
   ├─ Add class PendingEdit with RegionId, RegionRevision, EditSequence, and current text
   ├─ Add fn M.attach to observe exact changed ranges through native buffer callbacks
   ├─ Add fn M.capture to retain the newest full region text and increment EditSequence
   ├─ Add fn M.suspend_generated_text before applying any patch over unacknowledged local edits
   ├─ Add fn M.flush to send current regions before save, submit, or semantic selection
   ├─ Add fn M.acknowledge to clear only acknowledged edit sequences
   ├─ Add fn M.recover to rebase generated state without erasing newer local text
   └─ Add fn M.guard_region to detect edits that cross read-only boundaries

file nvim/lua/forge/effects.lua
└─ Route finite editor effects only to their intended live resources.
   ├─ Add fn M.apply to dispatch allowlisted native effects
   ├─ Add fn M.navigate to reject obsolete cursor and focus effects
   ├─ Add fn M.open_file to preserve real-file and historical-buffer behavior
   ├─ Add fn M.choose to return a stable choice ID with the originating request context
   ├─ Add fn M.notify to surface operation failures even after the originating view closes
   └─ Add fn M.complete to deduplicate repeated effect delivery within one process generation

file nvim/lua/forge/completion.lua
└─ Encapsulate synchronous completion bounded and independent of repository I/O.
   ├─ Add class CompletionCache with list identity, generation, and sorted opaque values
   ├─ Add fn M.replace to publish one validated bounded candidate snapshot
   ├─ Add fn M.values to binary-search a prefix and return at most 200 values
   ├─ Add fn M.refresh to coalesce cold or stale requests outside the completion callback
   └─ Add fn M.clear_generation to reject old-host candidates

file nvim/tests/forge/forge_completion.lua
└─ Create synchronous candidates without hiding asynchronous refresh failures.
   ├─ Add test cold_cache_returns_immediately_and_starts_one_refresh
   ├─ Add test warm_cache_filters_without_RPC_or_filesystem_access
   ├─ Add test candidate_count_bytes_and_returned_results_are_bounded
   └─ Add test refresh_error_notifies_without_replacing_valid_candidates_with_empty_success

file nvim/tests/forge/forge_buffer.lua
└─ Create real-Neovim tests for the generic adapter.
   ├─ Add test one_file_expansion_preserves_unrelated_text_and_extmarks
   ├─ Add test scrolling_expanded_content_reports_demand_without_another_keypress
   ├─ Add test post_apply_reports_fill_a_viewport_larger_than_one_batch
   ├─ Add test visible_collapsed_headers_never_request_body_expansion
   ├─ Add test unrelated_views_and_loaded_prefixes_survive_automatic_continuation
   ├─ Add test a_later_API_failure_marks_the_document_desynchronized
   ├─ Add test inserted_local_rows_block_even_nonoverlapping_absolute_row_patches
   ├─ Add test newer_local_edits_survive_an_older_acknowledgement
   ├─ Add test two_windows_keep_independent_native_fold_intent
   ├─ Add test delayed_navigation_never_moves_a_newly_selected_window
   ├─ Add test virtual_gutters_never_enter_yanked_source_text
   └─ Add test disabled_keymaps_disappear_from_maps_hints_and_help

6. Own repository identity, observations, and source bytes in forge-git. HEAD, index, worktree,
   common Git storage, and remote repository identities have different lifetimes and lock scopes.
   Rust obtains exact source objects instead of reconstructing old text from the current worktree.

file nvim/rust/forge/crates/forge-git/Cargo.toml
└─ Create the repository library with explicit read capabilities.
   ├─ Add config gix with only the features required by compiled parity probes
   ├─ Add config Tokio process and bounded blocking-work dependencies
   ├─ Add config byte-preserving path and hash dependencies
   └─ Add config real-Git fixture dependencies

file nvim/rust/forge/crates/forge-git/src/identity.rs
└─ Split worktree state from shared repository storage.
   ├─ Add struct WorktreeId, struct GitStorageId, and struct RepositoryIdentity
   ├─ Add struct RepositoryPath with raw path identity and a separate display label
   ├─ Add fn discover_identity to resolve the worktree, git directory, common directory, and index
   ├─ Add fn validate_path to reject escaping paths without following tracked symlink contents
   └─ Add fn resolve_argument to retain exact file and revision argument boundaries

file nvim/rust/forge/crates/forge-git/src/reader.rs
└─ Expose repository reads through one implementation-selected capability boundary.
   ├─ Add trait RepositoryReader with observe, content, and revisions operations
   ├─ Add struct GitReader with shared gix handle and per-operation ReadCapabilities
   ├─ Add struct ReadCapabilities with explicit implementation selection and unavailable reasons
   ├─ Add struct GitReadProcess for explicit Git-compatible read operations
   ├─ Add fn GitReader::thread_local to create worker-local gix state
   ├─ Add fn GitReader::observe to combine tree-index and index-worktree observations
   ├─ Add fn GitReader::content to load immutable objects or validated worktree bytes
   └─ Add fn GitReader::revisions to resolve refs and retain command completion behavior

file nvim/rust/forge/crates/forge-git/src/snapshot.rs
└─ Expose an observation from a filesystem transaction.
   ├─ Add struct RepositoryObservation with observation ID, HEAD, index stamp, and path records
   ├─ Add struct FileChange with independent change kind, modes, and content classification
   ├─ Add enum LineStats with Unknown, Exact, and ExceedsLimit variants
   ├─ Add struct ObservationScope with full or explicit affected-path scope
   ├─ Add fn collect_observation to reject known changes during collection
   ├─ Add fn verify_preconditions to recheck exact affected sources before a write
   └─ Add fn merge_path_observation to preserve unaffected records and later pending work

file nvim/rust/forge/crates/forge-git/src/content.rs
└─ Reuse source identity and enforce source acquisition limits.
   ├─ Add enum ContentSource for immutable object, index stage, worktree, and supplied bytes
   ├─ Add struct ContentRequest with source identity, path, and byte and line limits
   ├─ Add struct FileContent with source bytes, newline metadata, representation, and hash
   ├─ Add enum ContentResult for Ready, Binary, TooLarge, Missing, Unavailable, and Failed
   ├─ Add fn read_worktree to validate before-and-after stamps and hash accepted bytes
   ├─ Add fn read_object to check advertised size before decoding a blob
   ├─ Add fn text_representation to record raw, canonical, or display-only conversion
   └─ Add fn count_bounded_lines to distinguish an exact count from a lower bound

file nvim/rust/forge/crates/forge-git/src/repository.rs
└─ Centralize refresh generations and per-worktree state behind a narrow handle.
   ├─ Add struct RepositoryState with identity, reader, current observation, and revision candidates
   ├─ Add fn RepositoryState::observe to run reads without holding the state lock
   ├─ Add fn RepositoryState::adopt to reject superseded observations
   ├─ Add fn RepositoryState::invalidate to invalidate mutable aliases without deleting immutable blobs
   └─ Add fn RepositoryState::refresh_revisions to publish current local and remote branch candidates

file nvim/rust/forge/crates/forge-git/src/store.rs
└─ Reuse repository handles across feature owners.
   ├─ Add struct RepositoryStore with worktree handles, BlockingReadPool, and MutationCoordinator
   ├─ Add fn RepositoryStore::open to deduplicate discovered worktree identity
   ├─ Add fn RepositoryStore::release to remove a client reference without closing active work
   ├─ Add fn RepositoryStore::evict_idle to bound unreferenced handles
   └─ Add fn RepositoryStore::invalidate_storage to refresh linked-worktree ref consumers

file nvim/rust/forge/crates/forge-git/src/completion.rs
└─ Centralize bounded repository revision candidates without buffer-crate dependencies.
   ├─ Add struct RevisionCandidates with revision, sorted values, and truncated state
   ├─ Add fn build_candidates to preserve current local and remote branch completion
   ├─ Add fn invalidate_candidates to reject stale ref and repository generations
   └─ Add fn limit_candidates to enforce both item and byte limits

file nvim/rust/forge/crates/forge-git/src/read_pool.rs
└─ Configure blocking repository reads independently of CPU analysis.
   ├─ Add struct BlockingReadPool with queue, input-memory admission, and per-job cancellation
   ├─ Add fn BlockingReadPool::submit to retain at most the configured read concurrency
   ├─ Add fn BlockingReadPool::finish to release capacity only after actual worker completion
   └─ Add fn BlockingReadPool::shutdown to report unfinished native reads

file nvim/rust/forge/crates/forge-git/tests/reader.rs
└─ Create Git parity and external-change fixtures.
   ├─ Add test staged_and_unstaged_sources_remain_distinct
   ├─ Add test linked_worktrees_have_distinct_indexes_and_shared_ref_identity
   ├─ Add test mode_only_empty_symlink_submodule_and_conflict_changes_remain_visible
   ├─ Add test rename_scope_includes_origin_but_copy_scope_does_not
   ├─ Add test invalid_utf8_paths_keep_identity_separate_from_display
   ├─ Add test worktree_change_during_read_rejects_the_result
   ├─ Add test oversized_packed_blob_does_not_enter_diff_or_syntax
   └─ Add test unborn_detached_sparse_and_partial_repository_capabilities_are_explicit

7. Compute canonical changes and lazy display rows in forge-diff. File display limits do not
   define action targets. Exact hunks and source identities remain available independently from
   the currently loaded display rows, with mutation eligibility determined separately.

file nvim/rust/forge/crates/forge-diff/Cargo.toml
└─ Create the diff and source-analysis library.
   ├─ Add config a single measured histogram implementation and its locked version
   ├─ Add config byte hashing, source indexing, bounded caches, and property tests
   └─ Add config no dependency on ForgeStatus, Harness, Neovim, or forge-git

file nvim/rust/forge/crates/forge-diff/src/source.rs
└─ Expose immutable analysis inputs without knowing how Git loaded them.
   ├─ Add struct SourceVersion with content identity, representation, and newline metadata
   ├─ Add struct SourcePair with old and new immutable bytes
   ├─ Add struct SourceCoordinate with side, line, and byte position
   └─ Add fn validate_source_pair to reject unsupported encodings and inconsistent metadata

file nvim/rust/forge/crates/forge-diff/src/raw.rs
└─ Encapsulate exact changed ranges independent of display grouping.
   ├─ Add struct RawHunkId and struct RawHunk with old and new ranges
   ├─ Add struct PatchBody with canonical changed lines and final-newline markers
   ├─ Add fn compute_hunks to compare a validated pair
   ├─ Add fn synthetic_hunk to describe added or deleted content without comparison
   └─ Add fn patch_body to generate untruncated changed bytes for eligible writes

file nvim/rust/forge/crates/forge-diff/src/display.rs
└─ Create local display chunks without changing raw action identity.
   ├─ Add struct DisplayHunk with contributing raw IDs and bounded context windows
   ├─ Add struct DisplayRow with code-only text, source coordinates, gutter, and styles
   ├─ Add struct DisplayCursor tied to the exact source pair and display options
   ├─ Add fn group_hunks to merge touching context windows without duplicate source rows
   ├─ Add fn next_rows to split even one oversized display hunk at the row and byte limits
   └─ Add fn target_ranges to map each emitted row back to its exact source or raw action

file nvim/rust/forge/crates/forge-diff/src/body.rs
└─ Centralize user-requested preview and speculative-work limits.
   ├─ Add struct BodyPolicy with explicit expansion, prewarm, row, byte, and span bounds
   ├─ Add fn may_prewarm to require nondeleted kind and an already known delta below 100
   ├─ Add fn may_preview_full_file to permit 1000 lines and reject 1001
   ├─ Add fn unavailable_reason to distinguish binary, size, encoding, missing, and actual errors
   └─ Add fn limit_display_chunk to bound rows, text bytes, metadata, and one progress sentinel

file nvim/rust/forge/crates/forge-diff/src/intraline.rs
└─ Configure intraline work independently of source length.
   ├─ Add struct IntralinePolicy with line-pair, byte, and output-span limits
   ├─ Add fn compare_replacement to produce byte-correct old and new emphasis
   └─ Add fn fallback_line_style to retain readable diff backgrounds when work is rejected

file nvim/rust/forge/crates/forge-diff/src/engine.rs
└─ Reuse immutable analysis while feature documents own demand.
   ├─ Add struct DiffResult with exact source pair, raw hunks, and availability
   ├─ Add struct DiffEngine with AnalysisCache and AnalysisPool
   ├─ Add fn DiffEngine::compare to coalesce identical immutable source requests
   ├─ Add fn DiffEngine::added and fn DiffEngine::deleted to bypass line comparison
   ├─ Add fn DiffEngine::display to generate only the next requested chunk
   ├─ Add fn DiffEngine::syntax to schedule shared source parsing and capture extraction
   └─ Add fn DiffEngine::release_interest to cancel work only after its final consumer leaves

file nvim/rust/forge/crates/forge-diff/src/cache.rs
└─ Configure retained data by bytes as well as entry count.
   ├─ Add struct AnalysisCache with independent source, diff, tree, and capture accounting
   ├─ Add struct AnalysisKey with source hashes, representation, options, and grammar/query version
   ├─ Add fn AnalysisCache::reserve to charge memory before accepting work
   ├─ Add fn AnalysisCache::insert to retain immutable results under the byte budget
   └─ Add fn AnalysisCache::evict to release unused entries without claiming active references vanished

file nvim/rust/forge/crates/forge-diff/tests/diff.rs
└─ Create patch correctness and lazy rendering separately.
   ├─ Add test applying_raw_hunks_reconstructs_the_exact_new_canonical_bytes
   ├─ Add test reversing_raw_hunks_reconstructs_the_exact_old_canonical_bytes
   ├─ Add test display_context_merges_without_duplicate_lines
   ├─ Add test added_and_deleted_previews_never_call_the_line_diff_algorithm
   ├─ Add test unknown_or_100_line_delta_never_enters_prewarm
   ├─ Add test first_giant_hunk_obeys_the_same_chunk_limit_as_later_hunks
   ├─ Add test one_very_long_line_hits_the_byte_gate_before_serialization
   └─ Add test cancelled_consumer_does_not_cancel_another_consumer_of_the_same_analysis

8. Move syntax and context analysis into a bounded Rust worker pool. A small file delta does not
   imply a small source file or a cheap parse. Source bytes, tree allocation, query work, and result
   spans each need their own limit.

file nvim/rust/forge/crates/forge-diff/languages.toml
└─ Configure the supported grammar and query set explicitly.
   ├─ Add config entries for the configured parser inventory and filetype aliases
   ├─ Add config pinned grammar, ABI, highlight, injection, local-variable, and context query sources
   ├─ Add config translated predicate and directive behavior for Neovim-specific queries
   ├─ Add config license and update metadata for every vendored grammar and query
   └─ Add config explicit Unsupported results for languages outside the accepted manifest

file nvim/rust/forge/crates/forge-diff/build.rs
└─ Register build inputs without downloading grammars during a build.
   ├─ Add fn load_manifest to validate unique names and aliases
   ├─ Add fn compile_vendored_grammars to use pinned local generated sources
   ├─ Add fn generate_registry to expose the accepted language set
   └─ Add fn emit_inputs to fingerprint grammar sources, scanners, queries, and the manifest

file nvim/rust/forge/crates/forge-diff/src/language.rs
└─ Encapsulate language selection and query capabilities.
   ├─ Add struct LanguageRegistry and struct LanguageDefinition
   ├─ Add struct QueryCapabilities with supported predicates and directives
   ├─ Add fn LanguageRegistry::detect to use path, alias, and bounded shebang data
   ├─ Add fn LanguageRegistry::load_queries to reject unsupported required semantics
   └─ Add fn LanguageRegistry::capture_palette to preserve language-qualified capture names

file nvim/rust/forge/crates/forge-diff/src/workers.rs
└─ Centralize CPU scheduling, memory admission, and cooperative cancellation.
   ├─ Add struct AnalysisPool with bounded queues and dedicated worker-local parser state
   ├─ Add enum WorkPriority for foreground, visible enrichment, and speculative work
   ├─ Add struct WorkBudget with bytes, deadline, and cancellation signal
   ├─ Add fn AnalysisPool::submit to acquire capacity before retaining large inputs
   ├─ Add fn AnalysisPool::cancel to stop queued work and signal active cooperative checks
   └─ Add fn AnalysisPool::shutdown to join completed work and report unfinished native work

file nvim/rust/forge/crates/forge-diff/src/syntax.rs
└─ Reuse parsed source between syntax captures and semantic context.
   ├─ Add struct SyntaxRequest with exact source, requested ranges, and work budget
   ├─ Add struct ParsedSource with exact source identity, tree, language, and query version
   ├─ Add struct SyntaxResult with bounded capture spans and explicit availability
   ├─ Add fn parse_source to create one tree per exact source side
   ├─ Add fn query_highlights to extract supported capture semantics from that tree
   ├─ Add fn query_context to reuse the same tree for hunk context
   ├─ Add fn query_injections with explicit depth, byte, and language limits
   └─ Add fn map_spans_to_rows to emit only requested loaded ranges

file nvim/lua/plugins/treesitter.lua
└─ Disable automatic parsing only for buffers explicitly owned by Forge.
   ├─ Modify fn FileType callback to honor the Forge buffer ownership marker
   └─ Modify config parser attachment tests to preserve ordinary source-buffer behavior

file nvim/rust/forge/crates/forge-diff/tests/syntax.rs
└─ Create grammar, coordinate, cancellation, and resource tests.
   ├─ Add test every_manifest_language_constructs_its_accepted_queries
   ├─ Add test old_and_new_sources_never_share_a_tree_without_equal_identity
   ├─ Add test context_and_highlights_reuse_one_parsed_source
   ├─ Add test unsupported_predicates_fail_the_manifest_gate
   ├─ Add test injection_depth_and_capture_counts_remain_bounded
   └─ Add test parser_cancellation_does_not_release_capacity_before_the_worker_finishes

file nvim/tests/forge/forge_syntax.lua
└─ Create actual Neovim styling and parser ownership.
   ├─ Add test Forge_buffers_do_not_attach_the_global_Tree_sitter_autocmd
   ├─ Add test capture_names_preserve_theme_links_and_colorscheme_changes
   ├─ Add test diff_backgrounds_do_not_replace_syntax_foregrounds
   ├─ Add test multibyte_and_CRLF_coordinates_match_visible_cells
   └─ Add test unsupported_languages_keep_source_text_and_diff_backgrounds

9. Serialize Forge-owned writes through repository-scoped admission. Shared Rust services prevent
   their own overlapping writes, while Git's locks and source preconditions remain necessary for
   other Neovim instances, shells, hooks, and provider tools.

file nvim/rust/forge/crates/forge-git/src/mutation.rs
└─ Create write intent independently from the visible rows.
   ├─ Add struct OperationId and struct MutationIntent
   ├─ Add enum MutationTarget for whole path, canonical patch, and explicit worktree operation
   ├─ Add struct WritePreconditions with expected HEAD, index entries, modes, and source identities
   ├─ Add struct WriteOutcome with completed, failed, cancelled-before-start, and uncertain targets
   ├─ Add enum MutationScope for worktree index, worktree files, and shared refs
   └─ Add fn validate_intent to reject display-only, truncated, ambiguous, or overlapping targets

file nvim/rust/forge/crates/forge-git/src/coordinator.rs
└─ Centralize admission and operation lifetime across all Forge callers.
   ├─ Add struct MutationCoordinator with per-scope queues and operation receipts
   ├─ Add struct AdmissionGuard that remains owned by a running operation
   ├─ Add fn MutationCoordinator::admit to reserve scopes before asynchronous preparation
   ├─ Add fn MutationCoordinator::start to validate preconditions immediately before Git
   ├─ Add fn MutationCoordinator::cancel to distinguish queued and running work
   ├─ Add fn MutationCoordinator::finish to record exact outcomes before releasing admission
   ├─ Add fn MutationCoordinator::settle to collect one affected-path union per quiet burst
   └─ Add fn MutationCoordinator::recover to perform one verification retry and publish truth

file nvim/rust/forge/crates/forge-git/src/writer.rs
└─ Encapsulate Git as the production mutation implementation.
   ├─ Add trait RepositoryWriter with execute returning WriteOutcome
   ├─ Add struct GitWriter with an explicit executable and argument-vector construction
   ├─ Add fn stage_patch and fn unstage_patch to apply validated canonical patch bytes
   ├─ Add fn stage_path and fn unstage_path to preserve filters, modes, and unborn HEAD behavior
   ├─ Add fn write_patch_headers to encode exact Git paths without shell interpolation
   ├─ Add fn execute_worktree_operation for explicitly requested discard, branch, and restore work
   └─ Add fn terminate_and_reap to retain the operation permit until process outcome is known

file nvim/rust/forge/crates/forge-status/src/journal.rs
└─ Compose optimistic UI state from coordinator facts without owning a second writer queue.
   ├─ Add struct OperationJournal with confirmed baseline and ordered optimistic layers
   ├─ Add struct JournalLayer keyed by OperationId and immutable action intent
   ├─ Add fn OperationJournal::admitted to create one immediate local state change
   ├─ Add fn OperationJournal::completed to retain only known successful writes
   ├─ Add fn OperationJournal::verify to retire semantic matches without text edits
   └─ Add fn OperationJournal::recover to merge observed paths and replay later valid layers

file nvim/rust/forge/crates/forge-git/tests/mutations.rs
└─ Create real-Git semantics and ownership under contention.
   ├─ Add test all_Forge_views_share_one_index_queue
   ├─ Add test linked_worktree_ref_operations_use_the_shared_storage_scope
   ├─ Add test cancelling_a_running_child_does_not_release_admission_early
   ├─ Add test stale_patch_preconditions_refuse_the_write_without_retargeting
   ├─ Add test external_index_changes_are_detected_before_cached_patch_application
   ├─ Add test first_failure_preserves_completed_targets_and_cancels_the_remaining_batch
   ├─ Add test quiet_verification_retries_once_without_an_intermediate_notification
   ├─ Add test CRLF_clean_filters_and_missing_newline_patches_preserve_exact_index_bytes
   └─ Add test rename_copy_empty_file_mode_and_unborn_HEAD_actions_match_Git

10. Own ForgeStatus and local Git documents through forge-status. A StatusDocument owns semantic
    file and hunk state, while a separate BufferDocument owns the generated rows. Expansion,
    counts, syntax, and refresh publish independent changes instead of rebuilding every file.

file nvim/rust/forge/crates/forge-status/Cargo.toml
└─ Create the local Git feature owner.
   ├─ Add config dependencies on forge-buffer, forge-git, and forge-diff
   └─ Add config no dependency on Harness or GitHub implementation state

file nvim/rust/forge/crates/forge-status/src/file.rs
└─ Split file identity, body demand, and native fold intent.
   ├─ Add struct StatusFileId stable within one document and source-side occurrence
   ├─ Add struct StatusFile with source identities, line stats, and BodyState
   ├─ Add enum BodyState with Deferred, Loading, Ready, Partial, Unavailable, and Failed variants
   ├─ Add fn StatusFile::request_body to coalesce identical expansion requests
   └─ Add fn StatusFile::invalidate to retain file identity while replacing obsolete body state

file nvim/rust/forge/crates/forge-buffer/src/demand.rs
└─ Expose generic view demand without interpreting files or hunks.
   ├─ Add struct ViewDemand with view identity, document revision, visible ranges, and screen height
   ├─ Add field ViewDemand::expanded for the view's currently expanded fold identities
   └─ Add struct BlockRange with stable block identity and local row bounds

file nvim/rust/forge/crates/forge-status/src/document.rs
└─ Centralize status commands and incremental row changes.
   ├─ Add struct StatusDocument with repository, confirmed observation, journal, files, and buffer
   ├─ Add enum StatusTarget for sections, file occurrences, raw hunks, source lines, and actions
   ├─ Add enum StatusCommand from the complete current command-spec inventory
   ├─ Add fn StatusDocument::open to publish all base headers together before optional enrichment
   ├─ Add fn StatusDocument::command to resolve a validated revision and target
   ├─ Add field StatusDocument::demand to retain the latest accepted demand per live view
   ├─ Add fn StatusDocument::expand to admit initial body work without defining a page boundary
   ├─ Add fn StatusDocument::visible_changed to resolve expanded regions and coalesce view demand
   ├─ Add fn StatusDocument::deliver_body to advance the source-bound display cursor under credit
   ├─ Add fn StatusDocument::release_view to remove demand without cancelling other consumers
   ├─ Add fn StatusDocument::prewarm to obey known-count and resource gates
   ├─ Add fn StatusDocument::refresh to adopt only current observations
   └─ Add fn StatusDocument::reconcile to edit changed blocks and their affected summaries

file nvim/rust/forge/crates/forge-status/src/render.rs
└─ Create exact status rows and generic presentation records.
   ├─ Add fn render_header for branch, HEAD, upstream, merge, push, and remote metadata
   ├─ Add fn render_section and fn render_file for counts, names, and stable target IDs
   ├─ Add fn render_body for lazy diff chunks and source coordinates
   ├─ Add fn render_unavailable for explicit size, binary, encoding, and read failures
   └─ Add fn render_commands for configured hints and help from one command specification

file nvim/rust/forge/crates/forge-status/src/service.rs
└─ Reuse repository observations across independently owned status documents.
   ├─ Add struct StatusService with document registry and shared repository and analysis handles
   ├─ Add fn StatusService::open, fn StatusService::command, and fn StatusService::close
   ├─ Add fn StatusService::repository_changed to notify interested documents
   ├─ Add fn StatusService::remote_metadata to accept normalized values from ForgeRuntime
   └─ Add fn StatusService::resolve_view to reject a stale document or view lifetime

file nvim/rust/forge/crates/forge-status/src/revisions.rs
└─ Reuse the same source and buffer contracts for branch and historical-file views.
   ├─ Add struct BranchDocument with resolved base and current comparison mode
   ├─ Add struct RevisionDocument with source identity and historical buffer metadata
   ├─ Add fn open_branch to preserve working-tree-against-revision semantics
   ├─ Add fn open_revision to preserve staged HEAD, unstaged index, and branch-base line origins
   └─ Add fn resolve_old_line to return an exact source navigation effect

file nvim/rust/forge/crates/forge-status/src/compact.rs
└─ Reuse compact preview behavior through a separate non-writeable result type.
   ├─ Add struct CompactPreview with text, limits, and omission metrics
   ├─ Add fn compact_diff to preserve file and hunk headers with explicit omission markers
   └─ Add fn open_compact_preview to retain the staged bang and current buffer naming

file nvim/rust/forge/crates/forge-status/src/config.rs
└─ Move status-owned configuration and virtual ignored-path state.
   ├─ Add struct RepositoryConfig with current branch-prefix behavior
   ├─ Add struct IgnoredPathStore with existing worktree-scoped marker semantics
   ├─ Add fn load_repository_config to preserve .forge.json precedence
   ├─ Add fn update_ignored_paths to retain pending stage and recovery behavior
   └─ Add fn persist_ignored_paths to replace only the exact owned current-format file

file nvim/lua/forge/status.lua
└─ Route public status entrypoints into Forge and apply native view setup.
   ├─ Add fn M.open, fn M.open_branch_diff, and fn M.open_file_revision
   ├─ Add fn M.open_compact_preview
   ├─ Add fn M.refresh and fn M.close
   └─ Add fn M.setup_window to preserve existing filetype, buffer, winbar, and cursor behavior

file nvim/lua/plugins/forge.lua
└─ Reuse command registration while replacing the execution and completion owners.
   ├─ Modify command ForgeStatus, ForgeBranchDiff, ForgeBranchDiffFile, and ForgeFileRevision
   ├─ Modify command ForgeDiffCompactPreview with its existing bang behavior
   ├─ Modify fn complete_branches to use asynchronously refreshed revision candidates
   └─ Modify config lazy command list only when a recorded current command requires it

file nvim/lua/forge/views/commands.lua
└─ Route the current facade to Forge documents.
   ├─ Modify fn M.open, fn M.open_branch_diff, and fn M.open_file_revision
   ├─ Modify fn M.open_compact_preview
   └─ Modify fn M._walkthrough_host to expose native effects and canonical root identity

file nvim/rust/forge/crates/forge-status/tests/status.rs
└─ Create local updates and action identity under changing observations.
   ├─ Add test first_status_has_no_collapsed_body_or_eager_modified_file_diff
   ├─ Add test one_expansion_changes_its_body_and_only_required_parent_metadata
   ├─ Add test count_and_syntax_updates_do_not_replace_source_text
   ├─ Add test repeated_Tab_while_loading_preserves_the_latest_window_intent
   ├─ Add test hunk_action_uses_raw_source_identity_across_delivery_batches
   ├─ Add test all_base_headers_publish_together_without_body_batch_limits
   ├─ Add test visible_demand_continues_across_multiple_batches_without_user_input
   ├─ Add test multiple_expanded_files_and_windows_share_bounded_delivery_fairly
   ├─ Add test collapse_or_close_removes_only_that_views_demand
   ├─ Add test stale_viewport_reports_cannot_restart_obsolete_source_delivery
   ├─ Add test successful_verification_retires_layers_without_a_buffer_write
   └─ Add test large_preview_refusal_does_not_disable_valid_whole_file_actions

11. Own GitHub requests and issue storage through forge-github. Forge and shared issue
    completion use the same normalized records. The existing standalone GitHub plugin keeps its
    unrelated commands, while issue sync and repository cache deletion use the Forge store owner.

file nvim/rust/forge/crates/forge-github/Cargo.toml
└─ Create the remote data library without a second executable.
   ├─ Add config current redb version and serialization dependencies
   ├─ Add config Tokio process and bounded retry dependencies
   └─ Add config no dependency on status, review, or Harness feature state

file nvim/rust/forge/crates/forge-github/src/model.rs
└─ Split remote identity from local checkout identity.
   ├─ Add struct GithubRepositoryId with hostname, owner, and name
   ├─ Add struct PullRequest, struct ReviewComment, and struct ConversationComment
   ├─ Add struct IssueRecord, struct NotificationRecord, and struct RemoteFailure
   └─ Add fn validate_record to reject missing identity and invalid source anchors

file nvim/rust/forge/crates/forge-github/src/remote.rs
└─ Encapsulate gh execution with injectable request results.
   ├─ Add trait GithubRemote with typed read and mutate requests
   ├─ Add struct GhClient with executable, authentication context, and hostname
   ├─ Add fn read to decode successful empty results separately from failures
   ├─ Add fn mutate to return confirmed or uncertain remote outcomes
   └─ Add fn classify_failure to preserve rate limits, stderr, and API diagnostics

file nvim/rust/forge/crates/forge-github/src/queue.rs
└─ Centralize remote mutations by the remote resource they change.
   ├─ Add struct RemoteQueue keyed by hostname, repository, and PR or issue identity
   ├─ Add fn RemoteQueue::enqueue and fn RemoteQueue::complete
   ├─ Add fn transition_pr_state to preserve reopen-then-draft-or-ready sequencing
   └─ Add fn reconcile_uncertain to reload remote truth without automatically reposting a mutation

file nvim/rust/forge/crates/forge-github/src/issue_store.rs
└─ Move the current redb schema and storage functions into one library owner.
   ├─ Add struct IssueStore with existing issue, detail, term, label, and sync tables
   ├─ Modify fn upsert_page, fn read_state, fn read_detail, fn read_details, and fn upsert_detail
   ├─ Modify fn snapshot, fn remove_issue_indexes, and fn write_issue_indexes
   ├─ Modify fn open_database_with_timeout to distinguish lock contention from corruption
   ├─ Add fn publish_snapshot to atomically replace the current JSON completion snapshot
   └─ Add fn delete_repository_cache to acquire deletion ownership and close all local handles

file nvim/rust/forge/crates/forge-github/src/service.rs
└─ Centralize background sync and shared remote metadata.
   ├─ Add struct GithubService with remote client, IssueStore, and RemoteQueue
   ├─ Add fn ensure_fresh to preserve the current ten-minute refresh behavior
   ├─ Add fn sync to preserve open and all scopes and page-level high-water updates
   ├─ Add fn pause_for_rate_limit to use a bounded retry timer
   ├─ Add fn detail_batch to avoid one request per completion candidate
   └─ Add fn delete_cache to coordinate active sync, short-lived database handles, and publication

file nvim/lua/github/issue_index.lua
└─ Encapsulate completion synchronous and move remote and storage work into Forge.
   ├─ Modify fn M.list and fn M.search to use a preloaded immutable Lua snapshot
   ├─ Modify fn M.detail_async, fn M.prefetch_details, and fn M.store_detail_async
   ├─ Modify fn M.sync_repo, fn M.ensure_repo, fn M.ensure_for_buffer, and fn M.ensure_current
   ├─ Modify fn M.sync_current to retain the current open and all request behavior
   ├─ Remove fn run_sidecar, fn run_sidecar_binary, and fn run_sidecar_json
   └─ Add fn M.delete_repo to request coordinated deletion through Forge

file nvim/lua/github/repo_cache.lua
└─ Reuse the shared data-directory contract during store relocation.
   ├─ Modify fn M.delete_repo and fn M.delete_current to coordinate Forge-owned issue data
   ├─ Modify fn M.repo_dir and fn M.remember_cwd_repo only for explicitly versioned identity changes
   └─ Modify config cache identity to prevent cross-host aliasing without silently moving old data

file nvim/lua/plugins/github.lua
└─ Route shared-store commands without migrating unrelated standalone commands.
   ├─ Modify command ForgeGithubIssueSync to preserve its current arguments and completion
   └─ Modify command ForgeGithubDeleteRepoCache to wait for coordinated deletion completion

file nvim/rust/forge/crates/forge-github/tests/issues.rs
└─ Reuse existing redb tests and add multiple-process coverage.
   ├─ Modify test upsert_page_and_snapshot_open_issues
   ├─ Modify test upsert_replaces_old_indexes_without_losing_issue
   ├─ Modify test upsert_and_read_issue_detail
   ├─ Modify test read_multiple_issue_details_preserves_requested_order
   ├─ Modify test open_database_waits_for_existing_lock
   ├─ Modify test archive_corrupt_database_moves_only_database_file
   ├─ Add test second_Forge_process_can_access_the_store_between_short_transactions
   ├─ Add test deletion_blocks_new_sync_and_never_archives_a_locked_database
   └─ Add test database_and_JSON_snapshot_recover_after_interruption_between_commits

12. Move PR and review state into feature documents while preserving native editing. Rust owns
    comment identity, dirty state, viewed moves, remote queues, and exact generated borders. Lua
    retains raw editable text and acknowledges its local edit sequence before accepting new rows.

file nvim/rust/forge/crates/forge-review/Cargo.toml
└─ Create the review feature library.
   ├─ Add config forge-buffer, forge-diff, forge-git, and forge-github dependencies
   └─ Add config Markdown and display-width dependencies validated against Neovim fixtures

file nvim/rust/forge/crates/forge-review/src/edit.rs
└─ Encapsulate field revisions separate from document layout revisions.
   ├─ Add struct EditStore and struct EditableField
   ├─ Add struct RegionEdit with region revision, edit sequence, and full native text
   ├─ Add fn EditStore::accept to retain newer text even when unrelated layout changed
   ├─ Add fn EditStore::begin_save to capture one submitted text and edit sequence
   └─ Add fn EditStore::complete_save to clear dirty state only for the submitted content

file nvim/rust/forge/crates/forge-review/src/comments.rs
└─ Centralize comment and occurrence identity independently of physical rows.
   ├─ Add struct CommentStore, struct CommentId, and struct CommentOccurrenceId
   ├─ Add struct CommentAnchor with revision, path, side, and source range
   ├─ Add fn CommentStore::focus to expand exactly one occurrence
   ├─ Add fn CommentStore::merge to preserve dirty viewer-authored content and reply drafts
   ├─ Add fn CommentStore::save to retain explicit create, edit, reply, and delete outcomes
   └─ Add fn CommentStore::browser_target to distinguish comment anchors from adjacent code

file nvim/rust/forge/crates/forge-review/src/review.rs
└─ Centralize PR overview and batched review lifecycles.
   ├─ Add struct ReviewDocument with PR, viewed state, EditStore, CommentStore, and BufferDocument
   ├─ Add enum ReviewMode for overview and batched review
   ├─ Add fn ReviewDocument::open to preserve current defaults and bounded body loading
   ├─ Add fn ReviewDocument::command to interpret validated opaque targets
   ├─ Add fn ReviewDocument::move_viewed to mirror status section-move behavior
   ├─ Add fn ReviewDocument::submit to send one verdict, summary, commit, and comment batch
   └─ Add fn ReviewDocument::refresh to merge remote truth without replacing local drafts

file nvim/rust/forge/crates/forge-review/src/render.rs
└─ Create Markdown, comments, and issue rows in Rust.
   ├─ Add struct MarkdownRenderer with explicit WidthProfile
   ├─ Add fn render_pr_header, fn render_description, and fn render_conversation
   ├─ Add fn render_comment_box and fn render_comment_editor
   ├─ Add fn render_issue and fn render_notifications
   ├─ Add fn render_conventional_subject and fn render_relative_time
   └─ Add fn rewrap_changed_blocks to preserve editable source text exactly

file nvim/rust/forge/crates/forge-review/src/service.rs
└─ Centralize feature documents and remote subscriptions without a second remote store.
   ├─ Add struct ReviewService with document registry, GithubService, and shared analysis
   ├─ Add fn ReviewService::open_pr, fn ReviewService::open_review, and fn ReviewService::open_issue
   ├─ Add fn ReviewService::open_notifications and fn ReviewService::region_edit
   ├─ Add fn ReviewService::save and fn ReviewService::close
   └─ Add fn ReviewService::layout_changed to use one layout policy per physical buffer

file nvim/lua/forge/review.lua
└─ Reuse PR windows, native editors, and keyboard choosers.
   ├─ Add fn M.open_pr, fn M.open_review, fn M.open_issue, and fn M.open_notifications
   ├─ Add fn M.attach_editing to install generic region anchors and native change callbacks
   ├─ Add fn M.save to flush local text before the semantic save request
   └─ Add fn M.close to release view resources without losing pending operation notices

file nvim/rust/forge/crates/forge-review/tests/review.rs
└─ Create draft preservation and remote mutation semantics.
   ├─ Add test local_text_survives_unrelated_remote_and_layout_changes
   ├─ Add test older_save_completion_does_not_clear_newer_dirty_text
   ├─ Add test compact_and_focused_occurrences_share_the_same_comment_owner
   ├─ Add test conversation_comments_expand_only_on_explicit_open
   ├─ Add test inline_replies_exist_only_in_overview_mode
   ├─ Add test review_submission_uses_one_request_and_no_automatic_repost
   └─ Add test partial_PR_state_transition_displays_the_confirmed_intermediate_state

13. Move walkthrough semantics while retaining the physical plan-review contract. Walkthrough
    artifacts and Harness PlanDocument remain different domain formats. Shared buffer primitives
    can render their rows without creating a common task model that forces either schema to change.

file nvim/rust/forge/crates/forge-review/src/walkthrough.rs
└─ Centralize walkthrough validation and source navigation.
   ├─ Add struct WalkthroughDocument with current schema, source revision, tasks, and BufferDocument
   ├─ Add fn WalkthroughDocument::load from the resolved worktree root
   ├─ Add fn WalkthroughDocument::validate against the exact current artifact schema
   ├─ Add fn WalkthroughDocument::render to preserve complete task and subtask headings
   ├─ Add fn WalkthroughDocument::resolve_change to distinguish exact and explicitly stale targets
   └─ Add fn WalkthroughDocument::annotations to emit read-only comment presentation records

file nvim/rust/forge/crates/forge-review/src/inventory.rs
└─ Reuse Sem as the only walkthrough inventory provider.
   ├─ Add struct SemInventory with canonical paths and bounded entity records
   ├─ Add fn SemInventory::collect to run tracked inventory and one batched untracked request
   ├─ Add fn SemInventory::normalize to derive paths from exact absolute filenames
   └─ Add fn SemInventory::cancel to discard superseded results without inventing a fallback

file nvim/lua/forge/walkthrough.lua
└─ Reuse native walkthrough effects over opaque Rust-owned targets.
   ├─ Add fn M.start to request the artifact for the current canonical root
   ├─ Add fn M.show_change to apply source navigation and read-only annotation effects
   └─ Add fn M.close to clear view-owned extmarks and subscriptions

file nvim/rust/forge/crates/forge-review/tests/walkthrough.rs
└─ Create artifact and navigation parity tests.
   ├─ Add test startup_from_a_subdirectory_uses_the_repository_artifact
   ├─ Add test missing_or_failed_Sem_never_selects_another_inventory_engine
   ├─ Add test folded_tasks_keep_all_heading_rows_visible
   ├─ Add test task_and_subtask_initial_fold_states_match_the_current_contract
   └─ Add test stale_source_navigation_is_explicit_and_never_becomes_a_write_target

14. Generate Harness buffers and expose typed repository tools through the existing session owner.
    Provider events retain their ordering and semantic identity. New repository tools call the
    shared Rust libraries and inherit the active session's workspace and permission context.

file nvim/rust/forge/crates/forge-harness/src/repository_tools.rs
└─ Expose structured repository inspection without UI dependencies.
   ├─ Add struct RepositoryTools with shared RepositoryStore and DiffEngine
   ├─ Add struct RepositoryToolContext with session, interaction, workspace, and active policy
   ├─ Add fn RepositoryTools::status to return typed observations
   ├─ Add fn RepositoryTools::diff to compare exact requested sides
   ├─ Add fn RepositoryTools::file_diff to return bounded hunks and source coordinates
   └─ Add fn RepositoryTools::changed_paths to reuse repository truth for context and evidence

file nvim/rust/forge/crates/forge-harness/src/control_tools/runtime.rs
└─ Attach direct repository calls without creating another session runtime.
   ├─ Modify struct ControlToolRuntime to retain optional authorized RepositoryTools access
   ├─ Modify fn invoke_decoded to route implemented read-only repository methods
   └─ Modify fn control context validation to reject wrong workspace or inactive interaction

file nvim/rust/forge/crates/forge-harness/src/control_tools/mod.rs
└─ Reuse structured tool normalization and current external adapter semantics.
   ├─ Modify struct ControlToolRegistry to advertise only implemented repository inspection tools
   ├─ Modify fn run_stdio to retain the stateless ControlMcpAdapter role
   └─ Modify fn apply_invocation to keep lifecycle replay deduplication separate from execution

file nvim/rust/forge/crates/forge-harness/src/buffer/mod.rs
└─ Centralize transcript rows without taking ownership of provider reduction.
   ├─ Add struct HarnessDocument with session, timeline revision, and BufferDocument
   ├─ Add fn HarnessDocument::initialize to render the current accepted session state
   ├─ Add fn HarnessDocument::apply_event to retain every distinct provider lifecycle event
   ├─ Add fn HarnessDocument::append to edit only the active block suffix
   └─ Add fn HarnessDocument::layout_changed to rewrap only affected display blocks

file nvim/rust/forge/crates/forge-harness/src/buffer/transcript.rs
└─ Move transcript formatting into Rust.
   ├─ Add fn render_prompt to preserve greater-than prefixes and aligned continuation rows
   ├─ Add fn render_thought to preserve final duration and token metadata
   ├─ Add fn render_response, fn render_agent_tree, and fn render_session_status
   ├─ Add fn render_working to produce the transient one-second timer row
   └─ Add fn remove_working to avoid persisting presentation-only state

file nvim/rust/forge/crates/forge-harness/src/buffer/tool.rs
└─ Reuse full tool output while transferring bounded row batches.
   ├─ Add struct ToolOutputView with stable call identity and loaded output cursor
   ├─ Add fn ToolOutputView::collapsed to show command, first row, count, and final row
   ├─ Add fn ToolOutputView::expand to request complete output through bounded delivery
   ├─ Add fn ToolOutputView::next_batch to respect receive credit and row and byte limits
   ├─ Add fn ToolOutputView::collapse to stop pending delivery without discarding durable output
   └─ Add fn ToolOutputView::export_saved_output to write a complete read-only output artifact

file nvim/rust/forge/crates/forge-harness/src/buffer/history.rs
└─ Create interaction and session surfaces without changing their lifecycle.
   ├─ Add fn render_interactions and fn render_sessions
   ├─ Add fn render_prompt_history to return bounded composer replacement effects
   └─ Add fn resolve_history_target to keep stable selection across list refreshes

file nvim/rust/forge/crates/forge-harness/src/buffer/plan.rs
└─ Reuse canonical PlanDocument validation and rendering.
   ├─ Add fn render_plan_review to preserve the existing physical editable artifact
   ├─ Add fn accept_saved_plan to call the current exact-digest acceptance boundary
   ├─ Add fn request_plan_revision to preserve edited text, model diff, and annotations
   └─ Add fn render_plan_status to keep lifecycle rows distinct from the plan document

file nvim/lua/forge/harness.lua
└─ Reuse composer, transcript, permission, and plan windows over the shared connection.
   ├─ Add fn M.open, fn M.new_session, fn M.open_interactions, and fn M.open_sessions
   ├─ Add fn M.submit to flush composer text and preserve one admitted user action
   ├─ Add fn M.present_approval and fn M.present_question
   ├─ Add fn M.open_permissions and fn M.save_permissions
   ├─ Add fn M.open_plan_review to retain native file editing
   └─ Add fn M.close to release views without stopping ForgeStatus

file nvim/lua/forge/views/harness/controller.lua
└─ Route semantic timeline rendering with generic buffer and effect application.
   ├─ Modify fn on_event, fn M.submit, and fn M.attach
   ├─ Modify fn M.present_approval, fn M.present_plan_question, and fn M.toggle_mode
   ├─ Remove fn M.render and fn schedule_render after the transcript cutover
   └─ Remove fn synchronize_state after the Rust-owned event reducer remains authoritative

file nvim/lua/plugins/forge.lua
└─ Route the existing Harness commands through the shared Forge connection.
   ├─ Modify command Harness and command ForgeHarnessNew
   ├─ Modify command ForgeHarnessLog with unchanged optional arguments and static completion
   └─ Modify command Permissions with unchanged native document behavior

file nvim/rust/forge/crates/forge-harness/tests/buffer.rs
└─ Create transcript preservation and direct repository integration.
   ├─ Add test each_distinct_provider_event_reaches_the_adapter_in_order
   ├─ Add test one_tool_expansion_eventually_reveals_all_output_under_flow_control
   ├─ Add test oversized_tool_output_opens_a_complete_read_only_export
   ├─ Add test tool_lifecycle_updates_share_call_identity_without_dropping_events
   ├─ Add test repository_tools_share_the_status_repository_and_analysis_handles
   ├─ Add test repository_tools_do_not_require_a_GitStatus_document
   ├─ Add test source_buffer_open_does_not_acquire_a_Harness_session_lease
   └─ Add test plan_review_preserves_exact_saved_digest_and_current_acceptance_semantics

15. Route commit, branch, and network Git operations through the same admission owner. Commit
    editor waits must leave the protocol and control lanes responsive. AI message generation uses
    an exact comparison identity and must not overwrite text the user has already edited. The About
    summary keeps its HEAD comparison, while commit population requests staged context.

file nvim/rust/forge/crates/forge-status/src/commit.rs
└─ Centralize commit admission and process lifetime.
   ├─ Add struct CommitSession with OperationId, admission, process, and editor request state
   ├─ Add enum CommitEvent for editor request, stdout, stderr, and exit outcome
   ├─ Add fn begin_commit to reject conflicting pending, active, settling, or recovering writes
   ├─ Add fn run_commit to preserve the existing fake-editor callback contract
   ├─ Add fn finish_commit to observe the actual repository result before releasing admission
   └─ Add fn cancel_commit to terminate and reap without claiming the commit was rolled back

file nvim/rust/forge/crates/forge-status/src/repository_commands.rs
└─ Route the complete existing branch and network command inventory.
   ├─ Add enum RepositoryCommand for the recorded branch, push, pull, and refresh operations
   ├─ Add fn execute_repository_command to acquire appropriate index, worktree, and ref scopes
   ├─ Add fn stream_process_output to preserve order within each stdout or stderr stream
   └─ Add fn reconcile_repository_command to publish changed worktree and shared-ref observations

file nvim/rust/forge/crates/forge-harness/src/commit_message.rs
└─ Create bounded AI commit context through direct shared-library calls.
   ├─ Add enum MessagePurpose with About and Commit variants
   ├─ Add struct CommitContext with comparison identity, ignored markers, and compact diff
   ├─ Add struct CommitMessageResult with subject, body, purpose, and exact source identities
   ├─ Add fn build_context to load the requested HEAD or staged comparison without a status buffer
   ├─ Add fn generate_message to preserve the existing configured provider contract
   └─ Add fn validate_result to reject stale comparison context before publication

file nvim/rust/forge/src/messages.rs
└─ Route message requests between feature owners without a status-to-Harness dependency.
   ├─ Add struct MessageRequestStore with per-repository purpose, request, and source identities
   ├─ Add fn request_message to coalesce identical context and honor explicit regeneration
   ├─ Add fn publish_message to deliver only the current request to its surviving consumer
   └─ Add fn cancel_message to detach closed consumers and stop unneeded generation

file nvim/rust/forge/crates/forge-status/src/about.rs
└─ Encapsulate About summary state without moving provider ownership into ForgeStatus.
   ├─ Add enum AboutState with None, Pending, Generating, Ready, and Failed variants
   ├─ Add struct AboutSummary with request identity, comparison identity, and message state
   ├─ Add fn schedule_about to honor about_auto_generate and about_auto_generate_delay_ms
   ├─ Add fn accept_about to reject stale repository and source results
   └─ Add fn open_about to request generation or open the complete read-only message

file nvim/lua/forge/views/status/pr_state.lua
└─ Route About lifecycle ownership to the status document.
   └─ Modify fn status_ensure_about_state to use Forge until the legacy view is retired

file nvim/lua/forge/views/status/commit_view.lua
└─ Reuse the native About window without owning AI generation state.
   └─ Modify fn status_open_about to consume the read-only message editor effect

file nvim/lua/forge/integrations/commit.lua
└─ Reuse native editor and console windows while Forge owns Git execution.
   ├─ Modify fn M.commit to request admission through Forge
   ├─ Modify fn M.editor to complete the current editor request
   ├─ Modify fn append_text to consume explicit stdout and stderr events
   ├─ Modify fn M._finish to restore the existing Neovim window state
   └─ Remove field M._admission_pending only after all commit callers use Forge admission

file nvim/lua/forge/integrations/ai_commit.lua
└─ Reuse native text ownership while replacing context computation.
   ├─ Modify fn M.ensure to preserve the requested comparison, default HEAD, and force behavior
   ├─ Modify fn M.populate_commit_buffer_when_ready to check changedtick and user text
   ├─ Remove fn changes_fingerprint_async and fn build_commit_context_async
   └─ Remove fn generate_async, fn system_text_async, and fn systemlist_async after cutover

file nvim/rust/forge/crates/forge-status/tests/commit.rs
└─ Create commit exclusion without blocking control operations.
   ├─ Add test editor_wait_keeps_cancel_and_permission_responses_live
   ├─ Add test commit_refuses_pending_mutation_admission
   ├─ Add test hook_failure_and_cancellation_preserve_the_actual_repository_outcome
   ├─ Add test linked_worktree_ref_changes_invalidate_other_consumers
   └─ Add test delayed_AI_result_never_overwrites_user_commit_text

file nvim/rust/forge/tests/messages.rs
└─ Create coverage for About and staged commit context across real feature owners.
   ├─ Add test About_uses_HEAD_context_and_commit_uses_staged_context
   ├─ Add test same_stats_with_different_bytes_does_not_reuse_a_message
   ├─ Add test disabled_automatic_About_still_allows_explicit_generation
   ├─ Add test delayed_result_cannot_cross_repository_or_request_identity
   └─ Add test message_generation_does_not_open_or_mutate_an_interactive_Harness_session

16. Bound observability and measure the entire Rust-to-Neovim path. Logging remains controlled
    by diff_logging and harness_logging. Budgets cover JSON decoding, Lua allocations, API calls,
    fold maintenance, redraw, and Rust work separately.

file nvim/rust/forge/src/logging.rs
└─ Create bounded scope-aware diagnostic output.
   ├─ Add enum LoggingScope with Diff and Harness variants
   ├─ Add struct LoggingConfig with enabled state, rotation, and maximum record size
   ├─ Add fn configure_logging to preserve the current runtime settings
   ├─ Add fn record to carry originating scope through shared Git and diff work
   ├─ Add fn rotate to bound total retained bytes across files owned by each scope
   └─ Add fn clear_scope to clear only validated owned paths after closing their writers

file nvim/rust/forge/src/metrics.rs
└─ Split CPU, transport, editor application, and visible completion timings.
   ├─ Add struct RequestMetrics with queue, read, analysis, encode, and completion timestamps
   ├─ Add struct BufferMetrics with changed rows, bytes, blocks, spans, and API call counts
   ├─ Add struct MemoryMetrics with cache, in-flight, pinned, queue, and process measurements
   ├─ Add fn record_latency to retain p50, p95, p99, and maximum samples
   └─ Add fn record_cancellation to distinguish requested, observed, and finished times

file nvim/lua/forge/infra/perf.lua
└─ Reuse the two public logging scopes and measure native editor work.
   ├─ Modify fn M.configure_from_forge_options
   ├─ Modify fn M.span to time only Lua and Neovim work
   ├─ Modify fn M.clear to coordinate the selected Rust and Lua diagnostic files
   └─ Add fn M.record_apply to separate decode, line conversion, metadata, API, and redraw costs

file nvim/rust/forge/benches/serialization.rs
└─ Configure real payload representations before freezing the wire version.
   ├─ Add fn benchmark_line_arrays and fn benchmark_text_blobs
   ├─ Add fn benchmark_compact_metadata
   ├─ Add fn benchmark_total_pipeline to include the matching Lua consumer
   └─ Add fn report_results to record fixtures, environment, bytes, allocations, and latency

file nvim/rust/forge/benches/analysis.rs
└─ Configure repository, diff, syntax, and cancellation costs independently.
   ├─ Add fn benchmark_status_first_result and fn benchmark_status_enrichment
   ├─ Add fn benchmark_small_delta_large_source and fn benchmark_giant_single_hunk
   ├─ Add fn benchmark_cold_and_cached_syntax
   └─ Add fn benchmark_cancelled_backlog to expose stale work and nested parallelism

file nvim/tests/forge/forge_perf.lua
└─ Configure the actual native editor work for status and Harness.
   ├─ Add test small_patch_cost_does_not_scale_with_unrelated_document_rows
   ├─ Add test metadata_only_patch_does_not_write_buffer_text
   ├─ Add test large_snapshot_recovery_yields_between_bounded_preparation_batches
   ├─ Add test slow_consumer_never_accumulates_unbounded_scheduled_callbacks
   ├─ Add test completion_callback_reads_only_cached_values
   └─ Add test mixed_status_and_Harness_soak_records_peak_and_retained_memory

17. Deploy one immutable binary and preserve recoverable process lifecycles. The binary contains
    all feature crates, but feature resources initialize on demand. A running generation stays
    paired with its matching Lua protocol until an explicit restart.

file nvim/lua/forge/builder.lua
└─ Reuse the existing sidecar build and deployment owner.
   ├─ Add fn M.ensure to build the workspace root and return its immutable executable
   ├─ Add fn M.subscribe to share build progress among concurrent callers
   ├─ Add fn M.cancel_wait to detach a caller without killing another caller's build
   └─ Add fn M.shutdown to finish or terminate the builder-owned process according to policy

file nvim/lua/rust_sidecar/init.lua
└─ Generalize the existing deployment fingerprint to complete workspace inputs.
   ├─ Modify class RustSidecarOptions for a workspace manifest and one binary target
   ├─ Modify fn M.new to include member manifests, lockfile, source, queries, grammars, and build scripts
   ├─ Modify fn deploy to publish a completed immutable generation
   └─ Add fn prune_deployments to preserve paths leased by any live supported Neovim instance

file nvim/rust/forge/src/shutdown.rs
└─ Centralize distinct read, write, provider, and storage shutdown behavior.
   ├─ Add enum ShutdownState with Running, Draining, and Stopped variants
   ├─ Add fn begin_shutdown to reject new work and cancel speculative reads
   ├─ Add fn drain_operations to retain write and lease ownership until children are reaped
   ├─ Add fn finish_shutdown to flush terminal outcomes and release durable handles
   └─ Add fn report_unfinished to identify uncertain operations after a bounded shutdown wait

file nvim/lua/forge/infra/config.lua
└─ Expose only operator-relevant Forge configuration.
   ├─ Modify config initialization to pass current preview, prewarm, keymap, provider, and log options
   ├─ Add config Forge process and resource options with validated lower and upper bounds
   ├─ Add config explicit unsupported-preview behavior and capability reporting
   └─ Remove config obsolete engine options only when their final consumer is removed

file nvim/tests/forge/forge_sidecar.lua
└─ Create multiple clients, platform behavior, and failure recovery.
   ├─ Add test concurrent_commands_share_one_build_and_one_host
   ├─ Add test GitStatus_open_does_not_initialize_Harness_provider_resources
   ├─ Add test running_Windows_executable_is_not_overwritten_or_pruned
   ├─ Add test another_Neovim_instance_can_keep_its_older_leased_generation
   ├─ Add test restart_preserves_local_edit_buffers_and_reports_uncertain_writes
   └─ Add test failed_build_does_not_replace_the_last_completed_deployment

18. Switch owners in complete dependency groups and remove obsolete implementations. A production
    cutover includes entrypoints, all callers of the changed owner, regression tests, and source
    documentation. Old modules shared with a not-yet-migrated surface remain until that consumer
    switches, rather than being deleted according to the earlier feature's schedule.

file nvim/lua/forge/init.lua
└─ Reuse the public facade while deleting semantic dispatch through injected module fields.
   ├─ Modify fn M.setup and the recorded public open, refresh, and Harness functions
   ├─ Modify config command-spec composition to feed Forge and native keymaps once
   └─ Remove field exports for deleted semantic implementations after caller checks pass

file nvim/lua/forge/git/mutation_coordinator.lua
└─ Route every current Lua writer through Forge before switching status rendering.
   ├─ Modify fn M.enqueue to send typed write intent to the sole Forge coordinator
   ├─ Modify fn pending to read published admission state for current Lua callers
   └─ Remove fn local Git write execution when every writer uses Forge admission

file nvim/lua/forge/git/index_mutation.lua
└─ Destroy independent execution after all callers share Forge.
   ├─ Modify fn M.execute_async to preserve its caller result while routing through Forge
   └─ Remove fn direct process mutation helpers after caller migration completes

file nvim/lua/forge/harness/client.lua
└─ Route the old process owner with the shared current-protocol client.
   ├─ Modify fn request to map the current Harness operation contract to Forge
   └─ Remove fn standalone process launch and shutdown after all Harness consumers switch

file nvim/lua/github/issue_index.lua
└─ Destroy obsolete sidecar and GitHub sync state after shared-store parity.
   ├─ Remove fn old process, page-fetch, and retry helpers listed in migration.toml
   └─ Remove field duplicate sync and database ownership state

file nvim/rust/diff-review-harness/Cargo.toml
└─ Destroy the old package only after the relocation inventory is complete.
   ├─ Remove config standalone package and binary declarations
   └─ Remove config old test targets after their Forge destinations compile and retain every case

file nvim/rust/github-issue-index/Cargo.toml
└─ Destroy the storage-only package after its functions and tests move into forge-github.
   ├─ Remove config standalone package and binary declarations
   └─ Remove config obsolete build and launch references

file nvim/rust/forge/migration.toml
└─ Resolve every old source file and test before deleting its implementation.
   ├─ Modify config disposition to record destination, removal gate, and replacement tests
   ├─ Modify config shared-consumer list to delay deletion until its last production caller switches
   └─ Modify config completion evidence with exact test commands and results

file nvim/lua/forge/docs/architecture.md
└─ Route current ownership documentation at the corresponding implementation gate.
   ├─ Modify docs process, repository, buffer, syntax, review, Harness, and recovery contracts
   ├─ Modify docs native editing, folding, completion, and startup behavior
   └─ Remove docs obsolete Lua semantic ownership only after the matching code disappears

file .rulesync/rules/forge.md
└─ Configure source rules with the implemented Forge boundaries.
   ├─ Modify config architecture and verification guidance after the production cutover
   └─ Remove config obsolete fixed process counts and standalone package paths

file .rulesync/rules/nvim.md
└─ Reuse data and permission rules while assigning issue storage to Forge.
   ├─ Modify config sidecar ownership and current verification commands
   └─ Modify config generated-provider synchronization guidance through the normal Rulesync workflow

file nvim/rust/forge/tests/full_stack.rs
└─ Create final acceptance tests across real owning modules.
   ├─ Add test status_review_Harness_and_issues_use_one_owning_runtime
   ├─ Add test all_known_Git_writers_share_admission_before_any_render_cutover
   ├─ Add test closing_one_surface_keeps_other_surfaces_live
   ├─ Add test malformed_or_panicking_analysis_cannot_publish_partial_feature_state
   ├─ Add test restart_never_automatically_replays_a_repository_or_remote_write
   └─ Add test no_old_service_executable_is_required_after_final_cutover

# Modularity, testability, and plan validation

The migration is complete when each domain has one owner and Neovim applies its output without
recomputing domain meaning. Moving code across the process boundary does not establish bounded
latency, transactional writes, or safe cancellation by itself. The contracts below define those
properties and the limits of the guarantees.

## Verified baseline and corrections

The source inspection on 2026-09-05 establishes these facts. Source paths are relative to
nvim/lua/forge unless another repository path is shown:

| Current boundary | Source evidence | Consequence for the migration |
|---|---|---|
| Status reconciliation | status_render.lua, status_reconcile_buffer_lines | Current code already computes histogram line edits and applies them bottom to top |
| Status row generation | status_render.lua, status_render_loaded | Current code reconstructs row and metadata collections, then clears and reinstalls the status namespace |
| File expansion | status_render.lua, status_render_file | A never-opened collapsed file omits its body, while previously loaded rows remain available for native folding |
| Display pagination | size_gate.lua, _status_size_gate_should_defer | Current logic always admits the first hunk, so a single giant first hunk can exceed the nominal budget |
| Status collection | status_snapshot.lua, snapshot_command_by_source | Current collection uses five commands, including two MRC diffs and two added-file numstat queries |
| Old-side syntax | syntax_engine.lua, old_file_syntax_source_lines | Current code can copy live source and rewrite it backward with hunks before parsing |
| Harness process host | nvim/rust/diff-review-harness/src/main.rs, run_broker | The current output channel and per-request task spawning need explicit capacity limits |
| Harness MCP role | nvim/rust/diff-review-harness/src/control_tools/mod.rs, run_stdio | The current standalone mode advertises tools and returns structured invocation data rather than owning a session runtime |
| Native provider tools | nvim/rust/diff-review-harness/src/backend/codex/json_rpc.rs and backend/copilot/mod.rs | ControlToolRuntime already participates in provider adapters inside the Harness process |
| Public commands | nvim/lua/plugins/forge.lua | Nine commands are currently registered, with permissive two-position argument consumers |
| Global syntax attachment | nvim/lua/plugins/treesitter.lua | A FileType autocmd attempts native parser attachment to every buffer |

The earlier draft treated several intended outcomes as established guarantees. This revision
corrects them:

- A Fenwick tree supports prefix sums over an indexed sequence but does not make arbitrary sequence
  insertion logarithmic. BlockSequence uses a dynamic sequence and an explicit complexity contract.
- A Neovim apply callback is not a rollback-capable transaction. A partial API failure invalidates
  the replica and starts recovery.
- Row limits do not bound bytes, decoded object size, parser allocation, or decoration work.
- A local generation counter cannot prove that an external process did not change the index.
- Killing a Git child does not prove that its mutation never committed.
- A new process launched from the same executable does not share Rust objects with the host.
- A generic buffer crate should not own Git or Harness target enums.
- A text blob is not inherently faster than a JSON string array once Lua decoding and splitting
  are included.
- Replacing an entire file block on every delivery batch would resend the loaded prefix.
  Stable blocks enable bounded edits, but the patch algorithm must actually emit those edits.
- Exact syntax compatibility includes queries, predicates, injections, aliases, and capture names.
  Linking a grammar alone does not establish that compatibility.
- Existing provider tests that require authentication stay opt-in. Their absence from an offline
  run must not prevent verifying the move or be reported as a successful real-provider test.
- A 10-second LSP notification timeout from the earlier conversation is not a Forge startup or
  provider execution requirement.

## Crates and dependency direction

The root Cargo package lives directly under nvim/rust/forge and produces forge. Library crates live
under nvim/rust/forge/crates. There is no app directory, feature service executable, generic
application crate, or common domain model crate.

The crate dependency graph is acyclic. These are allowed dependencies between Forge packages,
not lists of all third-party libraries.

| Crate | Owns | Allowed Forge dependencies |
|---|---|---|
| forge-buffer | Generic generated text, block ordering, patches, presentation, editable-region records | None |
| forge-diff | Source-pair analysis, raw hunks, display windows, intraline changes, syntax, analysis cache | None |
| forge-git | Repository identity, content acquisition, observations, refs, known write admission | None |
| forge-github | Remote GitHub records, gh requests, remote mutation ordering, issue storage | None |
| forge-status | ForgeStatus, branch and revision views, local Git commands, optimistic UI state | buffer, diff, git |
| forge-review | PR, review, issue, notification, and walkthrough documents | buffer, diff, git, github |
| forge-harness | Existing Harness domains, typed repository tools, transcript and plan surfaces | buffer, diff, git |
| forge-protocol | Shared wire records, validation, frame limits, buffer transport | buffer |
| forge root | Process host, typed routing, resource composition, deployment-facing modes, diagnostics | All eight libraries |

Repository paths inside forge-github identify remote files as bytes or validated remote strings.
They do not need a dependency on local Git repository discovery. The root resolves the local-to-remote
association and passes normalized metadata into status and review.

forge-git owns RevisionCandidates as repository data. The root maps those values into a generic
completion response. Returning forge-buffer completion types from forge-git would violate the
dependency boundary.

Backend remains an actual provider contract. GithubRemote represents the external gh/API boundary.
RepositoryReader and RepositoryWriter isolate real repository operations for deterministic failure
injection. Additional interfaces need multiple behaviors or a concrete testing boundary, not merely
one class with one caller.

A crate boundary does not itself guarantee faster compilation. Record incremental and clean build
times before and after relocation. Avoid highly generic exported APIs and dependency upgrades that
force every feature crate to rebuild.

## One executable and process ownership

One Neovim instance starts one owning Forge host lazily. ForgeStatus, review, issue infrastructure,
and Harness share that host. Another Neovim instance can start its own host, so filesystem locks,
session leases, immutable deployment leases, and short redb ownership remain necessary.

Provider CLIs, Git, gh, Sem, and the existing fake-editor helper remain external processes where
required. The design does not replace them with Rust implementations merely to claim a single
operating-system process.

The ControlMcpAdapter mode is a stateless adapter in the same executable. It must not initialize
ForgeRuntime, open a second session store, acquire a session lease, or advertise live access to the
host's caches. Native provider tools use RepositoryTools directly inside the host.

External clients that need to execute repository operations against the live host require an
explicit authenticated local connection design. That listener is not part of this migration.
The current stateless adapter must not pretend it executed a read-only tool when it only encoded an
invocation. Such tools are advertised only on the provider path that can execute them.

## Repository identity and truth

RepositoryIdentity contains separate identities for the worktree, actual Git directory, common Git
storage, and actual index path. Two linked worktrees have distinct index and worktree state but can
share refs and object storage.

Git paths remain byte-preserving domain values. Display labels can escape invalid UTF-8, newlines,
tabs, or other control bytes. UI labels never become path arguments or action identities. Git
commands receive an argument vector and explicit path boundaries, with literal pathspec handling
where the command requires it.

Content classification is separate from change kind. A file can be a binary rename, a mode-only
modification, an empty addition, a symlink change, or an unresolved conflict. One enum containing
both Added and Binary cannot describe those combinations correctly.

An observation records what the reader saw, not a transaction over the whole filesystem. Forge
checks HEAD and index stamps before and after collection and verifies changed source bytes. It
rejects observations superseded by a known internal write or observed external change. It does not
claim those checks defeat arbitrary same-time external edits.

Every write validates its affected source identities again immediately before invoking Git. Git's
own index and ref locks remain authoritative for external contention. A detected mismatch reports
a stale target and refreshes the relevant state. Forge never relocates a destructive target to a
nearby line automatically.

Mutable worktree content uses accepted bytes and a content hash. A timestamp or file size is an
invalidation hint, not a correctness identity. HEAD, index, config, attributes, excludes, and remote
changes invalidate the relevant mutable aliases. Immutable object and analysis cache entries stay
valid when their complete keys remain equal.

File notifications, BufWritePost, FocusGained, command completion, and explicit refresh can trigger
coalesced invalidation. Notifications improve freshness but are not the write precondition.

## About summaries and commit messages

The current About summary and commit editor share ai_commit.lua but use different comparisons.
M.ensure defaults to HEAD, while commit population requests staged content. Preserve that distinction
instead of turning every message request into staged-only generation.

The root message router invokes the Harness-owned configured generation service and delivers a
passive result to the status owner. forge-status does not depend on forge-harness. Generating a
message may initialize the selected provider transport, but it does not create an interactive
Harness session, change its selected model, acquire its session lease, or inject a transcript turn.
Preserve the current ai.adapters commit-model selection through an explicit configuration value.

The cache key includes repository identity, purpose, exact old/new source identities, excluded paths,
prompt version, and provider/model configuration. Current stat-summary fingerprints can collide for
different text and are not correctness keys. Reuse a HEAD result for staged context only when the
complete accepted comparison content and generation configuration match. Automatic generation
retains its configured delay and disable switch. Explicit opening or regeneration remains available.

A delayed completion must match the outstanding request and its source before changing the About
row. Commit population also checks the native buffer's changedtick and text so a generated message
cannot overwrite user edits. A rejected result is not retried as a user action without a new request.

## Git reads, counts, and source representations

gix is the preferred source for discovery, refs, tree and index access, objects, and proven status
operations. A capability probe must compile and exercise each selected API before the feature
cutover. A high-level status method is not assumed to include both staged and unstaged comparisons
or every Git command option.

Use GitReadProcess for an operation whose required semantics are not demonstrated in gix. The
selection is explicit, internal, covered by parity fixtures, and visible in metrics. It is not a
silent fallback after arbitrary errors or a user-selectable duplicate engine. Git remains the
writer throughout this migration.

ReadCapabilities records status, rename/copy detection, attributes and filters, submodule handling,
revision syntax, sparse index, partial-clone missing objects, and hash-format support. Unsupported
repositories or operations produce a precise capability result. Unexpected read failures notify
instead of selecting another implementation without explanation.

The first status response enumerates changes and available metadata. It does not compute every
modified file's hunks merely to show exact line counts. Counts have explicit Unknown, Exact, and
ExceedsLimit states. Background counts are lower priority than explicit file expansion, and their
work is bounded and cancelled when no view needs them.

A count request for a modified file can require a diff. That cost belongs to optional enrichment,
not the claim that status enumeration is metadata-only. Status classification and rename detection
can also read content. The guarantee is no eager display-hunk computation or body transfer for
every collapsed file, not zero filesystem work.

The source pairs are exact:

| Comparison | Old source | New source |
|---|---|---|
| Unstaged tracked change | Current accepted index entry | Accepted worktree representation |
| Staged tracked change | Resolved HEAD tree, or empty tree for unborn HEAD | Current accepted index entry |
| Branch/revision comparison | Explicitly resolved requested tree | Existing command's worktree comparison semantics |
| Added file | Empty source | Index or worktree source for the selected side |
| Deleted file | HEAD, index, or comparison-tree source for the selected side | Empty source |

Renames and copies can contain modified content and therefore can require comparison. The direct
added/deleted optimization does not apply merely because one path changed.

Raw disk bytes, Git-canonical bytes, and display-only transformed bytes are distinct representations.
Line ending normalization, working-tree encoding, clean filters, and textconv can change content or
coordinates. Forge records the representation in source and analysis identities.

Only canonical, untruncated content can generate write patches. Display-only transformations retain
navigation metadata where it is known and explicitly disable ambiguous hunk writes. Whole-path
staging delegates to Git so filters, modes, symlinks, and empty files keep Git semantics.

A 1,001-line probe can stop scanning a decoded buffer early, but it cannot promise that a packed
Git blob was only partially decoded. Before object decode, inspect available size metadata and
reject oversized objects. Packed delta bases, decoder caches, and library allocations remain part
of measured memory consumption. Do not describe the line gate as a streaming-object guarantee.

## Lazy content and bounded progress

The base status buffer contains all section and file headers in one logical update and one initial
text application. It has no header pagination or viewport-dependent header loading. Optional counts
and remote metadata can arrive later as targeted edits. The expanded-body batch limits do not apply
to base headers. Measure header startup directly before introducing additional delivery mechanisms.

Three independent units govern expanded diffs. The file comparison determines exact changed ranges
from the admitted source pair. View demand determines which expanded content needs display rows.
The delivery batch bounds one row-generation and editor-update operation, not a fold, hunk, visible
page, or total loaded region. Comparing a modified file can still require the complete source pair.
Retain that comparison while subsequent batches generate only additional display rows.

The file-body state machine is independent from the window's native fold state:

1. A new file occurrence starts Deferred with source identity and available counts.
2. Explicit expansion enters Loading and assigns a demand generation.
3. Source acquisition and analysis return Ready, Partial, Unavailable, or Failed.
4. Only a result with the same source identity and demand generation can change the document.
5. Visible demand advances a display cursor tied to the same source pair automatically, continuing
   across batches until the viewport plus lookahead is covered or the content ends.
6. Collapse changes native visibility and stops unwanted future delivery without deleting loaded
   physical rows.
7. A source change invalidates the body cursor and replaces the obsolete body under one new revision.

A body chunk admits at most 256 source display rows, 128 KiB of generated text, and 8,192 decoration
records as initial engineering limits. The chunk also permits at most 16 structural rows for hunk
headers, loading state, or a sentinel. Limits apply to the first hunk as well as later hunks.
One hunk can span multiple batches and one batch can contain multiple hunks. A viewport can display
any number of loaded batches. Loading indicators describe pending work, not a required load-more
action. Failed reads expose explicit retry, while unavailable previews retain their stated limits.

Lua reports ViewDemand after scroll, resize, fold changes, and applied text changes, outside redraw
callbacks. Reports carry ViewId, DocumentRevision, generic visible block ranges, expanded fold IDs,
and window height. Lua does not identify files or select hunks. Rust resolves those ranges through
the current document and retains only the latest valid report for each view.

Use one window-height of additional display rows as the initial forward-lookahead target. Actual
visible ranges take priority, including wrapped content. A newly expanded region starts with an
initial batch, then post-apply visibility reports drive continuation. Inserting rows shifts the
loaded boundary, so continuation reevaluates coverage instead of reusing an obsolete absolute row.
Already loaded text remains in place. Context expansion reads the accepted sources without changing
raw hunk identity or requiring a new comparison.

Combine demand from all live windows and schedule visible expanded regions before lookahead, with
at most one pending delivery per file occurrence and fair rotation among ready regions. Receive
credit and the shared worker limits bound concurrent work. One window's collapse removes only its
demand. Stop unneeded work when no consumer remains, reject stale source results, and retain loaded
rows. Never expand a collapsed file merely because its header enters the viewport. A loading marker
entering view continues its already expanded region without another keypress. Source limits and
document memory limits still stop delivery with an explicit reason rather than retrying indefinitely.

One source line above the chunk byte limit is not split into fake source rows. Forge reports that
the inline preview is unavailable and retains exact whole-file and source-open actions. Binary,
invalid text encoding, unsupported filters, and large source results remain distinct from errors.

The 1,000-line added/deleted gate counts source lines, excluding display headers. Empty content has
zero source lines. A final newline does not add a synthetic extra source line. Preserve final-newline
metadata separately for Git patches.

Prewarm requires a known changed-line total below 100, nondeleted kind, source-size admission, and
available speculative capacity. Modified-file delta means added plus removed source lines. Added
file eligibility uses its known new-line count. A 99-line delta in a huge file still fails the
source-byte gate. Deleted files never qualify.

Loaded text remains real Neovim buffer text. Search, selection, marks, and yanks work over loaded
rows. Search cannot find deferred rows that have never entered the buffer, and the UI must not imply
it searched the whole repository. Scrolling does not replace physical text with a moving viewport.
Native search covers loaded rows only. Automatic continuation is not a promise to search unloaded
content or to retain only one viewport of text.

## Buffer identity, cost, and patch contract

DocumentId lasts for one open Forge document in one server instance. BlockId survives unrelated
edits and movement within that document. Feature identities such as file occurrences and comments
can survive many document revisions, but raw hunk identity changes when its exact source pair or
changed range changes.

DocumentRevision orders generated document states. Repository observation IDs, region revisions,
timeline revisions, and input sequences have separate purposes and cannot be substituted for one
another. IDs travel as opaque strings. Numeric counters are validated within Lua's exactly
representable integer range, with lifetime replacement before overflow.

BlockSequence is a dynamic balanced sequence with aggregate row and byte counts. Its target cost
is O(log B) for lookup and O(log B + K) for a splice affecting K stored blocks, excluding allocation
and text processing for the changed content. Initial construction and full snapshots are O(B + R)
for B blocks and R rows. A flat vector is acceptable for the first isolated prototype only if its
linear insertion cost is documented and the large-document gate still passes.

A logical file can own a header block, several loaded body blocks, and one sentinel. Appending rows
changes the sentinel and new tail, not the entire loaded prefix. A streaming Harness response can
replace its final incomplete line and append new complete lines without resending earlier text.

All TextEdit coordinates refer to the patch's base revision. Edits are disjoint and applied in
descending row order. Metadata uses stable block identities and block-relative positions. Moving
one block does not require recomputing every later absolute-row entry in Lua.

A change can legitimately affect more than one block. Expanding a file changes its body and parent
fold metadata. Staging can change two sections, their counts, a file occurrence, and command hints.
The acceptance condition is proportional work over affected blocks, not literally one block for
every user action.

Generic structural metadata is allowed in Lua. Lua can find a BlockId, native fold, or RegionId
synchronously. It cannot infer file or hunk semantics from line prefixes or build a second domain
tree. A local block index or extmark anchor structure must itself avoid O(total rows) maintenance.

## Protocol and serialization

The initial protocol candidate is JSONL because the existing integration already uses it and both
runtimes have mature JSON support. A compatible build uses one exact schema version. This personal
plugin does not need a range of historical protocol decoders or a general capability negotiation
framework for obsolete builds.

BufferText uses a compact internal string and row offsets in Rust. The initial wire candidate uses
a JSON array of strings because Neovim consumes that shape directly. Benchmark a newline-delimited
blob against it before freezing version one. A blob is selected only when total decode, split,
metadata, and apply time improves on the supported machines without increasing peak memory beyond
the admission budget.

Do not serialize offsets merely because Rust has them. They add wire values and still require
constructing Lua strings for nvim_buf_set_lines. An offset consumer must demonstrate a concrete
benefit before that representation enters the protocol.

Generated buffer rows and source-file bytes use different newline contracts. For a blob encoding,
row_count resolves zero rows versus one empty row and preserves trailing empty rows. Source
final-newline state belongs to SourceVersion, not the physical line list of a mixed ForgeStatus buffer.

Use compact arrays for repeated span records only after documenting each field and validating
fixtures. Preserve capture names through an initialization dictionary so themes can resolve
@function, @function.rust, and other accepted captures. Internal line offsets stay internal.

Initial transport limits are proposals to validate, not benchmark results:

| Resource | Initial limit | Saturation behavior |
|---|---:|---|
| Encoded frame | 512 KiB | Split a supported transfer or reject an oversized request |
| Active requests per client | 64 | Return Busy before retaining the payload |
| Host queued encoded bytes | 8 MiB | Stop producing optional output and apply receive credit |
| Lua pending received bytes | 8 MiB | Pause consumption credit and retain one scheduled drain |
| Pending frame count | 128 | Reject further admission or suspend the affected stream |
| Control records reserved | 32 | Preserve cancellation, failure, and shutdown progress |
| Snapshot part | 256 KiB | Validate sequence and total size before applying |
| Complete snapshot transfer | 16 MiB per document | Use a deliberate bounded document-load policy |
| Incomplete snapshot transfers | 1 per document | Cancel obsolete transfer before accepting another |

A bytes-only limit does not bound a JSON document containing millions of tiny arrays. Validation
also caps rows, spans, nesting, strings, and edit counts before allocating feature objects.
These are transport safety limits, not header pagination. A base snapshot that needs wire framing
is assembled and validated before its single initial buffer application. Do not apply expanded-body
row budgets to base snapshots or introduce incremental header presentation.

One writer preserves document event order. Reserved control capacity does not permit a terminal
message that depends on revision N to overtake the publication of N. Different document streams
can interleave while retaining independent revision sequences.

Receive credit counts work acknowledged by the Lua consumer, not bytes accepted by the operating
system pipe. Lua schedules one drain callback and processes bounded batches. This prevents a fast
host from accumulating an unbounded number of vim.schedule closures.

The first implementation recovers a revision gap with a current snapshot. A retained patch replay
log is not required unless measurements show snapshot recovery is a real cost. Adding replay
introduces retention, acknowledgement, and duplicate-effect state and needs its own tested owner.

Request IDs correlate transport responses. OperationId identifies an admitted mutation across
request cancellation within the live host. A lost connection marks an outstanding write outcome
unknown. On restart, inspect repository or remote truth and report the result. Never resubmit the
write merely because its request ID was lost.

## Applying patches and recovering from API failures

Lua preflights buffer validity, the applied revision, changedtick, row counts, metadata bounds, and
every referenced native resource before changing text. It prepares line arrays and metadata outside
the final mutation callback.

One scheduled callback performs the accepted edits and publishes the new applied revision only
after all required text and metadata operations succeed. This avoids yielding between the edits
owned by the adapter. It does not provide rollback or prevent every other plugin from observing
intermediate buffer events.

If an API call fails after earlier edits succeeded, BufferSession becomes Desynchronized. The
adapter stops semantic input, suppresses its own further decoration application, reports the
specific failure, and requests recovery. It does not acknowledge the target revision.

For generated read-only documents, recovery replaces the current text and metadata from a validated
snapshot and restores only still-valid native resources. For editable documents, recovery preserves
the local edit store and follows the edit protocol below.

Full snapshot preparation can be incremental, but a final physical buffer replacement can still
cost O(document rows). Large-document recovery must be measured separately from a normal patch.
If it exceeds the UI budget, build a replacement read-only buffer incrementally and publish it
through an explicit replacement operation. That operation must document buffer-number, mark, and
window implications instead of claiming transparent atomic recovery.

## Input freshness and immediate interaction

Every semantic command carries document identity, the displayed revision, ViewId, InputSequence,
and a generic target anchor or physical selection. Feature logic resolves file or hunk meaning.

A result computed from revision N must not interpret a row against revision N+1. The service can
resolve a retained TargetId only when its semantic identity and source preconditions still match.
Otherwise it rejects the stale action and refreshes. Read-only navigation can return an explicit
stale result, but destructive actions never silently choose a nearest target.

Rapid sequential input needs ordered admission. Per-document command dispatch resolves each input
before launching expensive work and preserves input order. Worker completion is asynchronous and
commits only against its captured generation.

Local native operations remain immediate. Moving a cursor, opening an already loaded native fold,
changing a window, and checking generic edit bounds do not wait for Forge. Opening absent content
does require a request. Repeated toggle input while Loading updates the latest requested fold intent
and does not start duplicate source jobs.

Navigation and focus effects carry the originating ViewId and InputSequence. Lua ignores an effect
when the user has since moved to another target or closed the window. Operation failure notices
remain visible even when their original view disappears.

Characterwise, linewise, and blockwise selections have distinct shape. Lua captures native selection
semantics, including byte versus display-cell coordinates. Rust resolves those generic ranges into
exact raw targets. Virtual gutter text never becomes source text or part of a Git patch.

## Editable regions and local change ordering

Neovim owns unsaved native text until Forge acknowledges it. RegionRevision identifies the accepted
content of one editable region, while EditSequence identifies successive local changes. Neither
depends solely on the generated document's layout revision.

A local insertion shifts every later physical row. Therefore it is insufficient to block only a
server patch whose old range directly overlaps the edited region. While any local edit is
unacknowledged, the adapter defers all generated text patches for that physical document.

The normal lifecycle is:

1. A native buffer callback records the changed region's full text, RegionRevision, and a new
   EditSequence before any later scheduled generated patch can apply.
2. The adapter suspends generated-text application for that document. It continues processing
   acknowledgements, notices, and independent documents.
3. A bounded debounce sends current region text. Save and submit flush it immediately.
4. Rust accepts the edit against its region revision even if unrelated generated layout advanced.
   A conflicting remote change produces an explicit conflict result rather than overwriting text.
5. The acknowledgement names the exact accepted EditSequence and resulting region revision.
6. Lua clears only edits up to that sequence. Newer local typing remains pending.
7. Once all local edits are acknowledged, the adapter discards deferred stale patches and requests
   a synchronized document state that includes the accepted region text.
8. Rust and Lua resume normal generated patches from that agreed document revision.

The first implementation favors this suspension protocol over a general text transformation
algorithm. Review editing can defer unrelated generated rows while typing. The Harness composer is
a separate native buffer, so composer typing does not suspend transcript streaming.

On process restart, native editable buffers remain available with their unsent content. Forge
reopens the current domain document, then reconciles each preserved region explicitly. An unknown
region is retained as recoverable local text and is not silently discarded.

Cursor-controlled modifiable is a UI convenience, not enforcement that an arbitrary native edit
stays within a region. Normal and visual commands can cross region boundaries. The adapter validates
actual changed ranges, retains the affected text before rejecting an invalid edit, and restores or
reports the invalid operation without losing valid user content.

Save has independent local and remote stages. An accepted region edit is not a completed GitHub
mutation. The remote request captures the submitted text and sequence. Success clears dirty state
only when the current content still equals that submitted content. Failure restores the marker and
underlying error while retaining newer local text.

PlanReview remains a physical artifact. Canonical PlanDocument, saved digest, annotations, and
acceptance stay in the current Harness plan implementation. The generic buffer protocol must not
introduce a second plan acceptance state machine or turn an unsaved buffer snapshot into approval.

## Folding, widths, and decoration ownership

Forge owns fold ranges, labels, and semantic load requirements. Neovim owns each window's open or
closed fold intent. A fold hides existing rows and cannot create missing source content.

Text splices let Neovim move persistent marks. The adapter replaces fold definitions only for a
changed structural subtree where feasible. If a native fold operation requires rebuilding a larger
range, measure that work explicitly. A signature containing absolute rows must not invalidate every
later fold merely because rows were inserted above it.

A physical buffer has one set of real lines even when two windows show it at different widths.
Forge therefore assigns one layout width per document. Use the initiating view as layout owner,
retain that choice until it closes, and select another live view then. Other windows use native
soft wrapping. Do not let two alternating resize events repeatedly reflow the same shared text.

Read-only comment boxes and transcript text can be wrapped in Rust. Editable Markdown bodies remain
raw text with native soft wrap. Borders use WidthProfile and a measured cell-width policy that
covers tabs, combining marks, East Asian width, emoji, and relevant Neovim options. Preserve source
bytes independently from display widths.

Inline virtual gutters remain persistent because they affect layout. Syntax and intraline spans
are cached records emitted only for visible rows. The decoration callback performs no parsing,
filesystem access, RPC, or mutable feature-state reduction.

Persistent metadata uses block anchors so insertion does not require O(total document rows) Lua
reindexing. The adapter maintains a generic ordered block index or an equivalent bounded anchor
lookup. Choose the measured representation during the buffer prototype.

Capture names and language suffixes survive the Rust analysis boundary. Neovim resolves theme
groups, foreground, background, bold, italic, and priorities. Background-only diff styles do not
supply a foreground. Adjacent different tokens remain different spans.

Colorscheme changes relink the palette and invalidate local decoration presentation, not source
analysis. The global Tree-sitter FileType autocmd skips buffers marked as Forge-generated before
FileType fires. Ordinary source buffers keep their native highlighting and language integrations.

## Syntax compatibility and worker scheduling

The configured parser inventory currently includes rust, typescript, tsx, lua, vim, vimdoc, json,
query, javascript, css, html, wgsl, glsl, c_sharp, toml, slang, yaml, nu, markdown, markdown_inline,
latex, and cue. Preserve frag and vert aliases for glsl and wgslx for wgsl. The implementation
inventory captures any changes before grammar cutover.

Cargo does not supply a complete uniform grammar catalog. Each language entry records a compatible
grammar crate or pinned generated source, external scanner requirements, ABI, license, query source,
and accepted predicates and directives.

Neovim query files can contain behavior beyond Tree-sitter's core query syntax. Query compilation
alone is insufficient. The registry validates predicates, local-variable semantics, injection
handling, capture priorities, and supported directives. Required unsupported behavior blocks that
language's cutover or receives a documented equivalent translation with fixture evidence.

Use one parsed tree per exact source side and share it between context and capture extraction.
Direct Parser and QueryCursor integration is the initial design because context needs tree access.
If tree-sitter-highlight is selected instead, demonstrate tree reuse or explicitly measure and
budget the additional parse. Do not run two independent parser pipelines while claiming one parse.

Different old and new sources normally require two parses. Identical source hashes, representation,
language, and query versions can share results. Diff rows are not a syntactically complete source
and must not be parsed as if they were the file.

AnalysisPool schedules explicit expansion before visible enrichment and speculative prewarm.
Acquire job and input-memory admission before retaining large request data. Keep at most one
speculative job per current cursor target and remove obsolete queued jobs.

Native parsers and synchronous diff algorithms run outside Tokio's I/O workers. Cancellation is
cooperative. A deadline stops waiting for the result and prevents publication, but it cannot prove
an already running native call stopped. Worker capacity remains charged until that call returns.

Start with at most min(4, max(1, logical CPUs minus 1)) analysis workers and a separately bounded
repository-read pool. Limit gix internal parallelism so repository jobs do not each consume a full
additional CPU pool. Record total runnable native work under mixed load.

Tokio explicitly documents that a started spawn_blocking task cannot be aborted. Forge must use
cooperative checks and real owner completion instead of treating JoinHandle::abort or a timeout as
thread termination. [Tokio blocking-task contract](https://docs.rs/tokio/latest/tokio/task/fn.spawn_blocking.html)

gix's shared repository handle creates worker-local repository state and needs the parallel feature
for Send when defaults are disabled. Its caches and configuration still need an explicit refresh
policy. [gix shared repository contract](https://docs.rs/gix/0.87.1/gix/struct.ThreadSafeRepository.html)

Rust removes classes of ownership and memory-access errors in Forge code. Native grammar code,
unsafe dependencies, allocator exhaustion, aborts, and process termination remain shared-process
failure boundaries. An isolated worker result can be discarded after a panic only if it did not
mutate shared feature state. A panic while changing shared state quarantines that owner or
terminates the host for recovery. Catching an unwind is not proof of valid shared state.

## Memory ownership and large outputs

Cached entries, active jobs, visible documents, pending output, and Lua replicas all consume memory.
Evicting an LRU entry does not free an allocation still retained by a document or worker.

Initial accounting targets are:

| Owner | Initial target | Required accounting |
|---|---:|---|
| Source inputs | 8 MiB per source side | Raw and converted bytes, before and after decompression where measurable |
| Source cache | 128 MiB | Referenced buffers and unreferenced cache entries reported separately |
| Diff result cache | 64 MiB and 512 entries | Hunk arrays, line indexes, and retained patch data |
| Tree and capture cache | 64 MiB accounted target | Tree estimates, capture arrays, query and injection overhead |
| Generated document | 16 MiB serialized snapshot limit | Text, metadata, loaded body ranges, and retained edit state |
| Idle repository handles | 16 | Active handles remain separately counted |
| Revision completion | 2 MiB or 20,000 values | Loaded candidate strings and lookup index |
| Visible completion results | 200 | Returned string values only |
| Logs per public scope | 30 MiB total | Active and rotated files across Lua and Rust writers |

These are admission and retention limits for owned representations, not a hard bound on operating
system resident memory. Native parser allocation, gix object caches, SQLite/redb mappings, allocator
overhead, and the provider SDK can exceed estimated charges. Measure process memory during the soak
and reject new optional work when the configured high-water target is reached.

Do not silently remove loaded physical rows to satisfy a cache budget. Prefer releasing redundant
analysis, stopping speculation, or refusing additional expansion with a visible resource limit.
Closing a document releases its pinned data. A full-file open or explicit export remains available
where existing source access supports it.

Harness's current durable record format can require decoding a complete interaction before slicing
tool output. Moving its rendering to Rust does not make that decoder incremental. Preserve the
current format initially, measure this allocation, and avoid duplicating the same output into
multiple rendered strings.

Complete tool output remains in durable Harness state. One expand request streams it into the
buffer under receive credit, pausing between batches. The display exposes progress while rows
arrive, and native search covers loaded rows. If the generated-document limit is reached, preserve
the saved output and expose an Open saved output row. The existing open action on that row asks
ToolOutputView::export_saved_output to write the complete saved bytes to a uniquely named artifact
under the Harness-owned data root. Lua opens that artifact as a read-only ordinary buffer, outside
the managed-document row cap. The export adds no manual pagination to normal tool expansion. Its
path, cleanup lifetime, and open action enter the Task 1 inventory, and cleanup retains an artifact
while a view still owns it.

## Mutation admission, cancellation, and external writers

The coordinator's guarantee covers known Forge-owned operations: status and diff staging,
unstaging, branch and network operations, commit, explicit discard, and Harness checkpoints or
repository tools. It cannot serialize arbitrary Git commands executed by a user shell, another
Neovim process, a hook, or a provider's general-purpose shell tool.

Provider workspace changes trigger invalidation and source-precondition checks. The plan does not
claim that safe Rust turns all external tools into cooperative writers.

Known operations reserve all required scopes in a deterministic order. Index-only operations use
the actual index identity. Worktree changes also reserve the worktree scope. Ref changes reserve
shared storage so linked worktrees cannot race through distinct per-worktree queues.

Admission requires resolved identity. A command from an existing document already carries its
RepositoryIdentity. A command issued before discovery registers pending admission at the host,
then promotes it to canonical scopes before preparation. Do not claim a root-specific reservation
exists before the root has been identified.

Cancellation has three distinct outcomes:

- Queued work is removed before Git starts and reports CancelledBeforeStart.
- Active work receives a cancellation request, but keeps its permit until the child is terminated,
  reaped, and its outcome classified.
- A process or connection failure after possible mutation reports OutcomeUnknown and requires
  observation before any subsequent dependent action.

The existing 120 ms quiet window and one verification retry remain behavior contracts for index
bursts. Verification compares semantic truth from affected paths. A match retires optimistic
layers without buffer edits. A mismatch changes affected documents and replays later valid layers.
After two failed reads, preserve known completed outcomes, reverse failed and cancelled optimistic
layers, mark verification stale, and notify once.

A batch stops at its first failed target and retains completed writes. Git commits, multi-command
branch transitions, and remote API requests are not generally rollback-capable transactions.
Do not implement automatic write retries or delete lockfiles merely because a timeout occurred.

## Harness preservation and direct calls

The package relocation preserves all existing feature directories and their private tests. It does
not combine provider adapters, replace Backend with an invented reduced interface, or rewrite the
session state machine as part of buffer work.

Session controllers retain out-of-band cancellation, approval delivery, capability-gated forks,
lease ownership, interaction identity, the 20-turn goal bound, and the two-consecutive-no-progress
guard. Current-format sessions reopen from their existing data root. Other private formats remain
hidden. No historical migration or alternate decoder is introduced.

PlanDocument validation, exact saved digest acceptance, task reporting, scope deviations, and goal
continuation remain in the current Harness implementation. Rendered plan rows do not authorize
execution or replace canonical plan state.

RepositoryTools provides typed reads inside the host. It accepts explicit session workspace and
policy context, never infers the active repository from an arbitrary visible window. Future write
tools must enter MutationCoordinator and the active permission evaluator.

Provider events are reduced in their original order. Each explicit assistant delta and tool
lifecycle update reaches the Lua adapter in that order during a healthy connection. There is no
timer that replaces several distinct events with the latest text. Neovim can still perform one
screen redraw after processing multiple callbacks, which is different from dropping events.

Backpressure can delay event delivery but must not deadlock approval responses or cancellation.
The provider pump separates control from presentation persistence. If a provider cannot pause and
the bounded event retention is exhausted, report saturation and stop the affected turn through its
defined failure path. Do not promise lossless presentation under unlimited provider production
and finite memory.

A connection failure can recover the current durable transcript state. That does not prove every
intermediate visual event was displayed before the failure. Persistence boundaries and acknowledged
events must be measured and documented during the Harness move rather than strengthened to one
database commit per token.

## GitHub storage and review consistency

GitHub identity includes hostname. Local worktree identity is not a unique key for a remote PR,
and owner/name alone can collide across GitHub hosts. Existing cache paths stay current-format
paths until an explicitly versioned path change is part of implementation.

One Forge process does not establish exclusive machine-wide ownership of issues.redb. Prefer
short-lived open/use/close transactions under a process-local queue, with a shared advisory
operation/deletion lease for cooperating Forge instances. Do not hold redb's exclusive file lock
for the entire Neovim session.

Cache deletion first stops or rejects new local sync, acquires the repository deletion lease,
closes local handles, and then deletes only the resolved repository cache directory. Another
process holding the database or an incompatible old client causes a visible Busy failure. A lock
error is not evidence of corruption.

A redb transaction and replacement of open-snapshot.json are separate commits. If the process dies
between them, the database can be newer than the completion snapshot. Store a revision in both,
detect the mismatch, and regenerate the snapshot. Do not claim cross-file transactional atomicity.

Completion callbacks read an already loaded Lua snapshot. Disk reading and JSON parsing happen
asynchronously before replacement. A missing snapshot yields no current candidates and starts one
coalesced refresh. It does not run a live GitHub query per keystroke.

Remote mutations retain resource-specific ordering. A timeout after sending a request can mean the
remote mutation succeeded. Reload remote truth or report an unresolved outcome. Never retry comment
creation or review submission automatically without a demonstrated idempotency contract.

## Logging, lifecycle, and fault reporting

diff_logging covers non-Harness work. harness_logging covers Harness-originated work, including
shared Git or diff calls made by a Harness action. Diagnostic component names are fields within a
scope, not additional operator toggles.

The checked-in plugin settings currently enable diff_logging and disable harness_logging. The
migration preserves the configured values. It does not reapply older default values from memory.

Use metadata-only logs by default: durations, bytes, counts, identities, queue depth, and outcomes.
Do not copy full source buffers, prompts, provider payloads, or complete buffer snapshots into
routine performance records. Cap record size before formatting or enqueueing it.

The 30 MiB scope budget includes all files owned by that scope, including Lua and Rust diagnostics.
Two independent writers each retaining 30 MiB would violate that total. Either one owner writes
the scope or the two writers receive explicit portions of the budget.

Failure notification remains independent of logging. Disabled logs do not suppress Git, provider,
decode, parser, permission, or persistence errors. Normal cancellation and unsupported preview
capabilities use their documented nonfailure UI states.

Forge builds asynchronously through the existing immutable sidecar deployment. Build progress,
read deadlines, provider turn deadlines, and cancellation waits have distinct settings. The prior
10-second LSP notification request does not set all of them to ten seconds.

A deployed generation is identified by every relevant workspace input, including grammar sources
and queries. A newer build does not replace an executable in use. Multiple Neovim instances retain
leases for their actual executable paths. Cleanup removes only unleased completed generations.

On shutdown, stop admission, cancel optional reads, request provider cancellation, finish or
terminate and reap owned children, flush known terminal outcomes, and release session leases.
A shutdown timeout reports unfinished operations. The client must not tell the user every write
was cancelled merely because the host process exited.

## Execution gates and rollback

Ownership order and execution order are related but not identical. In particular, logging and
measurement begin with the prototype, and all known writers switch admission before a new Rust
status path can issue mutations.

| Gate | Required tasks | Evidence before advancing | Production effect |
|---|---|---|---|
| Baseline and dependency probes | 1, isolated portions of 6 and 8, metrics from 16 | Current command and test inventory, gix capability probes, parser/query build probes, baseline latency | None |
| Shared process and buffer prototype | 2 through 5, deployment from 17 | Relocated Harness deterministic tests, real pipes, native patch and edit races, wire benchmark | None until host and all clients agree on the current protocol |
| Shared mutation ownership | 6, 7, 9, admission parts of 15 and 18 | Known writer inventory, real-Git tests, old Lua callers routed through Forge, linked-worktree scopes | One coordinator for all known writers while existing rendering can remain |
| Local Git document cutover | 8, 10, affected portions of 16 and 17 | Status and diff parity, lazy body limits, capture compatibility, actual Neovim latency | Switch local Git documents and remove their unused semantic code |
| Remote and review cutover | 11 through 13 | Shared-store contention, dirty-edit races, PR/review/walkthrough parity | Switch remote documents and shared issue infrastructure |
| Harness buffer and commit cutover | 14 and 15 | Ordered event delivery, current data reopen, provider mocks, editor/commit races | Switch generated Harness rows and complete shared commit integration |
| Final retirement | 18 and complete verification | Every inventory disposition resolved, old launchers absent, Windows and macOS evidence | Remove obsolete packages and remaining unused semantic modules |

The temporary Lua writer route is a migration of current callers to the current Forge API. It is
not a compatibility decoder for old stored formats. It owns no second write queue and is removed
when its final caller migrates.

Shared Lua render modules cannot be removed at the ForgeStatus cutover if review or walkthrough still
uses them. The removal gate belongs to the last consumer. The migration inventory proves this with
caller evidence instead of assuming every feature has separate implementation files.

Each production cutover is a coherent source change and matching immutable build. Rollback reverts
that cutover's source and restarts the matching client and binary. Retaining an older executable
alone is insufficient if its Lua protocol or private storage format no longer matches.

Keep Harness storage path and exact current format unchanged through relocation. A later format
change requires a new exact version and the repository's hide-noncurrent behavior. It cannot be
combined with a claim that every older session still reopens without a decoder.

No new service automatically receives traffic while its gate is incomplete. Internal fixtures can
compare old and new outputs without adding a permanent runtime engine selector.

## Completion ownership

Revision candidates belong to RepositoryState and carry their accepted ref generation. Lua's
CompletionCache stores sorted opaque values and uses binary search to return at most 200 matches.
The callback does no repository discovery, Git execution, or JSON file decoding.

A cold callback returns immediately and schedules one asynchronous refresh. A stale callback can
return its last known candidates with a stale generation while refresh runs. A failed refresh
notifies and does not replace a valid cache with an empty successful result.

The 20,000-item and 2 MiB cache limits can omit candidates. The snapshot carries a truncated flag,
and completion must expose that limitation without rejecting a manually typed valid revision.
The first implementation does not promise exhaustive completion in repositories above those limits.

Native file completion for the first argument of ForgeBranchDiffFile and ForgeFileRevision retains the
current Neovim path completion behavior. Its native filesystem cost is measured separately and is
not covered by the cached revision callback's 1 ms target.

Harness model, skill, and command candidates keep their existing provider capability source.
Workspace-file and issue candidates refresh outside completion callbacks and keep their own list
identity, freshness, and size bounds. The common Lua cache does not merge domain lists into a
single repository-dependent model.

## Source and test retirement inventory

The implementation must populate exact file and symbol entries in migration.toml. These current
source families establish the required destinations and prevent broad deletion based on a filename
pattern.

| Current source ownership | Forge destination | Retirement condition |
|---|---|---|
| git/git_backend.lua, status_snapshot.lua, file_body.lua, and repository reads in git_data.lua | forge-git and forge-diff | All current read consumers use the shared contracts |
| git/index_mutation.lua and mutation_coordinator.lua | forge-git writer and coordinator | Every known Lua and Rust caller shares admission |
| views/status/operation_journal.lua and status_sync.lua | forge-status journal and reconciliation | Real-Git partial-failure and stale-verification parity |
| views/status/status_render.lua, section_builder.lua, section_map.lua, and diff_source_state.lua | forge-status documents and render | Status, PR, and review consumers have migrated |
| render/diff_parse.lua, hunk_model.lua, hunk_index.lua, intraline_diff.lua, and syntax_engine.lua | forge-diff | Diff, syntax, context, and action-target parity |
| render/row_tree.lua, layout.lua, text_snapshot.lua, source.lua, and source_loader.lua | forge-buffer or source owners | No unconverted feature retains their semantic state |
| render/decoration.lua, row_emitter.lua, region.lua, and native status window/fold helpers | Generic Lua Forge adapters | Required Neovim behavior moved and tested before removal |
| integrations/gh.lua and remote payload normalization | forge-github | Every Forge remote consumer switches |
| views/pr/* and render/comment_* plus annotations.lua | forge-review and generic Lua editing | All dirty-edit and occurrence identity fixtures pass |
| views/walkthrough.lua and infra/inventory.lua | forge-review walkthrough and Sem inventory | Artifact, fold, inventory, and stale navigation parity |
| render/harness/* and views/harness/timeline_cache.lua | forge-harness buffer and current Rust timeline | Native event and interaction fixtures pass |
| views/harness/controller.lua, layout.lua, prompt_history.lua, and completion/* | Rust feature state plus retained Lua input/layout adapters | Native behavior is preserved while semantic reduction disappears |
| harness/client.lua, builder.lua, and protocol.lua | Shared Forge client, builder, and wire schema | No standalone Harness launch remains |
| rust/diff-review-harness/src/* | forge-harness source plus Forge host routing | Whole-module relocation inventory and deterministic suite pass |
| rust/diff-review-harness/tests/broker_stdio.rs | forge/tests/harness_broker_stdio.rs | Every existing case preserved against the executable |
| rust/diff-review-harness/tests/permission_document.rs | forge-harness/tests/permission_document.rs | All current schema and mode cases pass |
| rust/diff-review-harness/tests/codex_cli.rs, codex_real.rs, and copilot_real.rs | Same named tests in forge-harness | All cases compile with their existing ignored annotations |
| rust/github-issue-index/src/main.rs and inline tests | forge-github issue store and tests | Current schema, lock, index, and snapshot behavior preserved |
| integrations/commit.lua and ai_commit.lua | Retained native editor plus Rust commit owners | Admission, callback, cancellation, and user-text races pass |

Paths in this table are relative to nvim/lua/forge unless they begin with rust/, which is
relative to nvim/. The canonical task groups above use complete repository-relative paths.
The inventory itself must use complete repository-relative paths everywhere.

Migrating a Lua domain unit test does not mean replacing its Git mock with a fake Forge response and
declaring the original behavior covered. Move the domain assertions to Rust, keep native assertions
in Neovim, and add a real integration test that connects the two.

The migration inventory records each existing test's disposition as retained, ported, replaced with
named equivalent coverage, or removed with a specific obsolete behavior. A whole-suite deletion
without case mapping fails the retirement gate.

## Decision rationale and validation limits

The selected design is a modular monolith because status, review, and Harness share repository
identity, analysis results, and lifecycle rules. Direct Rust calls avoid serializing those internal
values and permit tests at each ownership boundary.

A Rust helper that only runs Git leaves row rebuilding and state races in Lua. A domain-aware Lua
client retains two sources of file and hunk truth. A completely structure-free Lua client makes
native editing, fold interaction, and stale-coordinate handling depend on avoidable round trips.
The chosen boundary keeps generic buffer structure local and domain meaning in Rust.

A separate process per feature adds failure isolation but also duplicates deployment, caches,
connections, and cross-feature coordination. That is not the selected architecture. One process
accepts a shared failure boundary and needs honest recovery rather than claims that Rust prevents
all process crashes.

The plan does not claim the new implementation is already faster. The architectural expectation is
less Neovim-thread computation and work proportional to changed blocks. End-to-end benchmarks,
native redraw inspection, and memory measurements determine whether those expectations hold.

The UML uses shared ownership only for genuinely shared services, keeps domain target types outside
forge-buffer, and gives the private BlockSequence one parent. The three diagrams cover input and
recovery, mutation verification, and Harness event/tool flow. Editable-region and multi-process
storage lifecycles have explicit transition rules rather than being hidden inside a generic
buffer-update step.

Neovim byte coordinates and native update callbacks are verified against its API contract.
Implementation validation must use the installed supported Neovim version, not assume every
development-version API exists. [Neovim buffer and coordinate API](https://neovim.io/doc/user/api/)

The gix 0.87.1 repository API and feature set are research inputs, not proof of complete Git
compatibility. The implementation locks versions only after the probes compile and fixtures pass.
[Repository read APIs](https://docs.rs/gix/0.87.1/gix/struct.Repository.html)

## Definition of done

Completion requires all of these observable outcomes:

- The public Forge and shared issue-index command inventory passes through the intended owner.
- One Forge host serves ForgeStatus and Harness in a Neovim instance, with lazy provider startup.
- Harness repository tools call shared Rust services without opening a status buffer.
- The first ForgeStatus response omits collapsed body text and eager modified-file hunk computation.
- Added/deleted preview, prewarm, first-hunk, byte, span, and source-size bounds are enforced.
- Rust owns expensive diff, syntax, Markdown, row, and domain-state work for migrated surfaces.
- Lua stores generic structural metadata and performs native resource, editing, and effect work.
- Stale inputs cannot silently target a newer file, hunk, comment, or window.
- Unacknowledged local edits survive patches, remote refreshes, process failure, and older saves.
- All known Forge Git writers share admission, while external-write conflicts remain checked.
- Uncertain writes are never automatically replayed.
- Existing Harness domain tests and all five standalone test targets retain their coverage.
- Current-format Harness data reopens, noncurrent formats remain hidden, and issue-store contention
  works across multiple Forge processes.
- diff_logging and harness_logging preserve current settings and enforce one total retention budget
  per scope.
- Native buffer, fold, decoration, completion, and command behavior passes on Windows and macOS.
- Every retired source and test has recorded replacement evidence, and architecture rules and source
  documentation describe the implemented boundaries.

# Test plan

Tests verify contracts at their actual ownership boundary. Rust domain tests exercise real domain
modules, Neovim tests exercise native editor behavior, and executable integration tests connect both.
Recorded commands below are implementation verification targets, not commands executed while
writing this document.

## Unit tests

The unit suite uses mocks only for external operations, time, controlled failure, or provider input.

| Owner | Exercise directly | Replace for failure injection | Required assertion |
|---|---|---|---|
| BufferDocument | Dynamic sequence, text, patches, metadata | None | Random operations equal a reference document and retain exact coordinates |
| GitReader | Identity and typed observation logic | Process results or filesystem race fixture | Stage, path, mode, representation, and capability distinctions survive |
| MutationCoordinator | Admission, scope ordering, receipts, settle/recovery | Clock and RepositoryWriter | No early permit release, hidden retry, or false rollback |
| DiffEngine | Raw diff, display grouping, intraline, body policy | Source acquisition already supplied | Exact patch reconstruction and bounded display work |
| AnalysisPool | Queueing, cancellation, consumer sharing | Cooperative fake workload and clock | Admission remains charged until work actually ends |
| Syntax engine | Pinned real grammars and accepted queries | Only cancellation and unsupported-language fixtures | Exact side mapping and bounded capture output |
| StatusDocument | Current observation plus journal and buffer | Reader/writer failures | Only affected blocks change and stale actions are refused |
| ReviewDocument | EditStore, CommentStore, submission state | GithubRemote | No local text loss or duplicate uncertain mutation |
| GithubService | Page reduction, snapshot revisions, queue policy | gh/API results and clock | Correct high-water state, rate waits, and uncertainty handling |
| Harness | Existing session, plan, goal, permission, and event reducer | Existing mock Backend | Relocation preserves current behavior and independent control delivery |
| Lua adapter | Real headless Neovim APIs | Fake Forge transport only | Native text, extmarks, folds, editing, and failure recovery obey the wire contract |

Property tests generate insertions, deletions, block moves, Unicode text, empty lines, metadata-only
updates, and local region changes. Compare every accepted operation with a simple reference row
vector. Validate metadata positions and identities as well as final text.

Do not assert that two histogram implementations produce identical hunk boundaries for every
repeated-line input. Assert exact reconstruction, stage/unstage behavior, intended context policy,
and chosen golden cases. A deliberate hunk-policy difference needs documented review evidence.

## Integration tests with real Git and storage

The integration fixtures create temporary repositories and data roots. They do not mutate the
user's active repository, provider configuration, or operational logs.

1. Open a repository with staged, unstaged, partially staged, untracked, deleted, renamed, copied,
   mode-only, empty, binary, symlink, submodule, and conflict changes. Compare both observation sides
   with Git and verify the first status response has no body text.
2. Expand added, deleted, and modified files. Instrument content reads, comparison calls, source
   bytes, parser calls, and transmitted rows. Assert direct added/deleted body construction.
3. Exercise 99, 100, 1,000, and 1,001-line boundaries, unknown counts, a tiny delta in a huge file,
   one giant first hunk, and a single very long line.
4. Stage and unstage whole paths and individual raw hunks under CRLF, attributes, clean filters,
   missing final newline, sparse index, and unborn HEAD fixtures.
5. Issue writes from two views, a commit action, and a Harness checkpoint. Assert admission
   ordering, operation receipts, and exact completed-target preservation after a partial failure.
6. Modify the index and worktree from an external process between read and write. Assert stale
   preconditions fail without selecting another target or deleting a lock.
7. Exercise two linked worktrees. Verify independent index operations, shared-ref exclusion, and
   invalidation of both views after a shared branch change.
8. Cancel before start, during preparation, during Git execution, and after possible completion.
   Assert distinct outcomes and no automatic write replay after a host restart.
9. Open current-format Harness data at the existing path and verify session lease behavior from
   two processes. Verify noncurrent records remain hidden and preferences follow their current
   schema.
10. Run two issue-store clients with short transactions. Interrupt between redb commit and JSON
    snapshot replacement, then verify recovery. Exercise coordinated cache deletion under contention.
11. Drive remote review mutation with deterministic HTTP/gh fixtures. Verify an uncertain create
    or submit result reloads truth instead of posting a duplicate.
12. Call RepositoryTools from an active Harness interaction with ForgeStatus both open and closed.
    Verify shared handles and exact workspace and permission context.
13. Start the ControlMcpAdapter role and assert it owns no feature runtime or durable session store.

## Neovim integration and interaction tests

A fake Forge server is appropriate for transport faults and deliberate malformed patches. Real
Forge must also run behind the editor for feature acceptance.

- Keep the same buffer number and unchanged extmarks during normal one-file expansion.
- Publish all base headers together, with no collapsed body and no header pagination.
- Scroll through expanded content without further activation. Verify viewport coverage plus
  lookahead, including a viewport larger than one delivery batch and multiple expanded files.
- Split one raw hunk across delivery batches and verify selection and staging retain its identity.
- Collapse or close one of two views during delivery. Verify stale demand stops while the other
  view continues, and no loaded prefix is resent or removed.
- Insert new rows near the start of a 10,000-file document and verify metadata does not require
  rewriting every later row record.
- Apply multiple disjoint edits and inject an API failure after the first. Verify desynchronization,
  no false revision acknowledgement, an actionable notice, and successful recovery.
- Type into a description, append at its lower boundary, and paste across a protected border.
  Verify native edit capture, region bounds, and preservation of recoverable user text.
- Delay a region acknowledgement, type newer text, then deliver a remote refresh and an old save
  response. Verify the newest text remains present and dirty.
- Insert local rows above a server patch's target. Verify the patch is deferred even if its original
  interval does not overlap the editable region.
- Send two rapid stage or toggle actions while analysis and buffer updates are delayed. Verify
  ordered admission and no stale row retargeting.
- Open one buffer in two windows, choose different folds, resize both, and close the layout owner.
  Verify independent fold intent and one documented layout-width policy.
- Exercise native characterwise, linewise, and blockwise selection with tabs, combining characters,
  non-ASCII paths, and inline gutters.
- Apply a colorscheme change and inspect visible cells. Verify capture names, diff backgrounds,
  intraline emphasis, and unsupported-language rendering.
- Expand a large Harness tool once. Verify bounded batches eventually expose complete output
  within the admitted document size and collapse stops further delivery.
- Keep typing in the composer while transcript deltas arrive. Verify composer editing does not
  pause transcript application.
- Verify all nine command registrations, exact current argument handling, bang behavior, usage
  notices, keymap overrides, disabled mappings, hints, and help.
- Verify ForgeGithubIssueSync and ForgeGithubDeleteRepoCache through the shared issue owner, while unrelated
  standalone GitHub command entrypoints remain outside this migration.
- Open a real source file and a historical revision. Verify expected LSP, filetype, winbar, hidden
  buffer, and close behavior without duplicate parser attachment to mixed Forge buffers.

Run terminal inspection at 160 by 48 and 100 by 30 for status, branch diff, historical revision, PR,
review, walkthrough, transcript, composer, plan review, interactions, sessions, permissions, and
commit editor/console. Headless text snapshots do not prove highlight precedence, wrapping, cursor
position, or fold rendering.

## Existing test preservation

All Rust unit tests inside the relocated Harness source move with their owning module. The five
standalone targets keep explicit destinations:

| Existing target | Destination | Deterministic execution |
|---|---|---|
| broker_stdio | Root Forge harness_broker_stdio target | Required |
| permission_document | forge-harness permission_document target | Required |
| codex_cli | forge-harness codex_cli target | Compile and retain ignored tests |
| codex_real | forge-harness codex_real target | Compile and retain ignored tests |
| copilot_real | forge-harness copilot_real target | Compile and retain ignored tests |

Authenticated provider execution is separate evidence. Run it when its prerequisites exist and
report skips explicitly. Deterministic migration approval requires preservation and compilation of
those tests, not network access or a fabricated passing provider result.

Port Lua domain assertions from status_snapshot, operation_journal, mutation_coordinator,
index_mutation, intraline_diff, section_map_reducer, inventory, file_body, and related cases into
their Rust owners. Preserve native tests such as visual_selection, diff_buffer_action_cursor,
empty_diff_cursor, editable_region_append, comment_box, and native fold/syntax cases in Neovim.

Retain Harness plan-question, plan-task, plan-UML, plan-file, annotation, rustdoc, session-picker,
agent-picker, and permission tests at their actual UI or domain boundary. The package move does not
authorize deleting them because the new transcript renderer has its own smaller test suite.

## Performance acceptance

Measure the existing implementation and Forge on the same machine, Neovim build, terminal, theme,
repository fixture, window size, and warm/cold state. Logging is disabled unless the benchmark
specifically measures logging overhead.

Report queue wait, source acquisition, diff, parser/query, row generation, serialization, Lua
decode, row conversion, metadata maintenance, native API calls, and visible redraw independently.
Record p50, p95, p99, maximum, encoded bytes, total spans, and peak process memory.

The following are target budgets for the prototype and cutover, not claims of current measurements:

| Interaction | Initial target | Fixture and failure response |
|---|---:|---|
| Immediate native input or cached completion | 1 ms p95 | Full configured cache and two visible windows |
| Normal patch decode through native application | 8 ms p95 | At most 256 body rows within byte/span limits |
| Automatic expanded-body continuation | Visible demand before lookahead | Multiple files and windows, no per-batch keypress |
| Base header publication | Measure one complete application | Thousands of headers without body pagination limits |
| One scheduled apply turn | 16 ms p99 | Split preparation or reduce admitted batch if exceeded |
| Warm expansion of already loaded fold | No repository or syntax work | Native fold cost reported separately |
| Small patch with unrelated rows added | Cost tracks changed data | Compare 100, 1,000, and 10,000 file documents |
| Full snapshot/recovery | Measured separately | No normal-patch budget claim for a full replacement |
| First status response | Improve versus recorded baseline | Count optional enrichment separately |
| Cancellation | Bound queue removal and stale-result rejection | Measure active native work until actual completion |
| Memory and logs | Remain inside owned admission/retention limits | Report RSS and native allocations separately |

The encoding benchmark compares JSON line arrays, a single text blob, and compact metadata using the
real Lua implementation. Selection uses total client-visible work and peak memory. A smaller encoded
payload alone is insufficient evidence.

Use a mixed 30-minute soak with status refresh, file expansion/collapse, cancellation, review edits,
tool streaming, and multiple repositories. Long-running checks are split or run with an explicitly
approved timeout beyond the repository's command limit. Assert bounded queues, no scheduled-callback
accumulation, stable retained memory after closing views, and no lost operation outcomes.

A failing performance target changes batch sizes, metadata layout, worker admission, or the chosen
representation before cutover. Record the adjustment and repeat the affected benchmark. Do not
hard-code a test that asserts a performance claim without recording its test machine and fixture.

## Verification commands and reporting

Commands run from the repository root. Cold dependency builds and runtime tests are recorded
separately so compilation time is not mistaken for a test stall. Use the repository's timeout
runner or equivalent tool-enforced timeout, starting at the targeted values below.

Compile the foundational packages with a 120-second command limit before executing their targeted
tests. A timeout produces diagnostic evidence and a narrower follow-up, not an automatic escalation.

~~~text
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-buffer --no-run
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-git --no-run
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-diff --no-run
~~~

After compilation, run each relevant package or test target with a 60-second limit.

~~~text
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-buffer
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-git --test mutations
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-diff --test diff
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-diff --test syntax
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-status --test status
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-review --test review
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-github --test issues
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-harness --test permission_document
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge --test harness_broker_stdio
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge --test protocol_stdio
~~~

Compile all migrated external-provider tests without running ignored cases. Use a 120-second limit
for compilation, then report ignored-case counts explicitly.

~~~text
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge-harness --tests --no-run
~~~

Run native adapter tests with a 30-second limit each. The tests must wait for their specific async
completion event before asserting or exiting.

~~~text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -S nvim/tests/forge/forge_buffer.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -S nvim/tests/forge/forge_syntax.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -S nvim/tests/forge/forge_sidecar.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -S nvim/tests/forge/status_flows.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -S nvim/tests/forge/pr_review.lua
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -S nvim/tests/forge/harness.lua
~~~

Run formatting and linting after the targeted tests pass. Use a 120-second limit and narrow any
timeout before extending it.

~~~text
cargo fmt --manifest-path nvim/rust/forge/Cargo.toml --all -- --check
cargo clippy --manifest-path nvim/rust/forge/Cargo.toml --locked --workspace --all-targets -- -D warnings
lua-language-server --check nvim/lua/forge --checklevel=Warning --logpath <external-scratch-path>
~~~

Run the deterministic workspace and full executable integration gate after targeted success. Use
a 120-second limit per command and report any external-provider exclusions.

~~~text
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked --workspace
cargo test --manifest-path nvim/rust/forge/Cargo.toml --locked -p forge --test full_stack
~~~

At implementation cutover, validate Rulesync source changes through its configured dry run before
regenerating provider files. This planning rewrite does not modify those rules or generated files.

~~~text
rulesync generate --dry-run
git diff --check
~~~

Every execution report records command, configured timeout, elapsed time, exit status, failing
assertion or diagnostic artifact, and skipped prerequisites. Windows and macOS require independent
evidence. Tests must remove only their own verified temporary files and must never delete the user's
existing diagnostic logs as routine cleanup.


