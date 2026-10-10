# Forge — Architecture Reference

On Windows, the initialize handshake loads a versioned Git system-prefix location
cache from `stdpath("cache")/rust-sidecar/forge/git-config-location.json` before
repository reads. `forge-git::config_location` validates the bounded record and
existing absolute directory, then seeds gix through the local `gix-path` patch.
A miss uses gix's default discovery and atomically persists the prefix. A hit avoids
the `git --exec-path` subprocess. Configuration and attribute contents remain live.
Non-Windows hosts and explicit `EXEPATH`, `GIT_EXEC_PATH`, `GIT_CONFIG_SYSTEM`,
`GIT_CONFIG_NOSYSTEM`, or `GIT_ATTR_NOSYSTEM` overrides bypass persistence.
Cache I/O failures report a warning and retain normal discovery without rejecting
startup. `:ForgeGitConfigCacheReset` clears the file. Restart Neovim afterward to
replace gix's process-local prefix, including when switching between installations
whose old directories still exist. Startup diagnostics record the cache state.

The `forge` package builds a library and a thin executable. `src/main.rs` parses
arguments and calls the synchronous, non-inlined `forge::run` entry point.
`src/lib.rs` owns the host, router, runtime, and shutdown modules, keeping their
generated async code in the library compilation unit. Both targets use the same
release profile with incremental compilation, no debug symbols, and LTO disabled.

Forge uses one Rust host for repository operations, GitHub operations, generated
documents, analysis, and Harness sessions. Lua owns Neovim buffers, windows, editing
callbacks, and editor effects. Public Status, branch comparisons, local previews,
source revisions, Walkthrough, and Harness use native document services. PR/review,
issue, and notifications use native document services through Lua editor adapters.
Public PR overview and review routes open one native ReviewDocument. The overview
loads overview, file, check, and conversation sections. The review route then enters
batched mode. Notification PR subjects preserve repository, number, and workspace
identity through that same public route, while browser fallback remains available.
PlanReview retains its Lua controller and projects physical source rows through a native document.
Each test-inventory source row carries its file and case identity. Visual `j` collects
distinct cases across the selected rows and opens the shared confirmation dialog.
Confirmation validates the captured view and source revision before `plan.tests.delete`
creates a new reviewed plan revision. The broker removes only selected inventory entries,
prunes empty file groups, and rejects stale confirmations. Project files remain unchanged.
Harness analyzes the exact saved Markdown with the shared syntax engine before opening that
document, after releasing the broker lock. The document retains the syntax handle across
annotation insertions and view resizes. Native highlights include fenced language injections.
Block-relative conceal ranges hide Markdown delimiters without modifying source bytes.
Block-relative source overlays replace display cells such as unordered list markers.
Source highlights apply inline-code and heading backgrounds independently from syntax captures.
Heading overlays add one display cell of trailing padding at the source line endpoint.
Generated PlanReview section headings use one highlighted space on each side of the title,
omit the colon, and share the black-on-white `RenderMarkdownH1` presentation.
The viewport provider reveals the original source on the cursor or selection rows according
to the window's conceal policy. Overlays preserve source coordinates and rebase with edits.
The Neovim replica applies those ranges. PlanReview hides editor line numbers and the status
column and uses Markdown conceal level 3, restoring the inherited settings when released.
Source-line numbers remain in declaration diff gutters. Draft comments preserve their owning
view's column settings instead of changing editor line-number options.
Wrapped source lines retain the invoking window's continuation indentation and indentation
options. The input view owns those settings through window re-entry and restores them on release.
The statusline resolves the source type from physical native-buffer paths without changing
the buffer's parser-admission filetype. Generated URI buffers retain their declared type.
Collapsed fold labels use the same syntax and conceal metadata as expanded source rows.
Markdown inline regions retain independent parse trees so delimiters cannot cross paragraph
boundaries. Syntax analysis accepts at most 10,000 source lines, counting a trailing
newline as the end of the last line rather than an extra line. Longer files retain
their source and diff content without trees, captures, or an error notification.
Injected languages use the same source and stop repeated language/range cycles.

`crates/forge-buffer` owns block documents, revisions, editable regions, targets,
fold metadata, display width, and Markdown rendering. Its Markdown source map relates
every physical source row to generated rows after wrapping or syntax removal.
Accepted local edits rebase fold anchors within the edited block and in externally
owned headers. The endpoint index selects only affected owners. Their metadata and
the editable body commit in one validated revision, so growth and deletion preserve
fold boundaries without copying unrelated document blocks.
`crates/forge-protocol` owns bounded frames, outgoing queues, and receive credit.
`ConnectionHost` retains request and shutdown ownership through collection.
`ForgeRuntime` composes the shared repository, analysis, GitHub, and document services.
Opening a non-Harness view does not create a Harness session.

`forge.buffer` applies native patches to physical buffers. Neovim owns unsent PR and issue
field text. Explicit saves capture region bodies and revisions together. A queued save retains
that capture rather than reading newer unsaved text when dispatch starts. Completion advances
accepted baselines without replacing newer typing. Generated text updates defer while native
drafts remain, while source decorations resolve their shifted physical rows.
Ordinary PR and issue close hides retained drafts. The eight-document PR cache evicts only
clean hidden buffers and retains additional documents when every candidate contains a draft.
After host generation collection,
explicit invalidation revokes the old replica and preserves physical text for replacement
admission. Stale patches cannot address the replacement owner.
Each replica installs its buffer-wipe invalidation before feature-owner cleanup handlers.
External wipes revoke the replica while retaining Neovim's ownership of the active deletion.
Ordinary close marks the replica closed and removes that lifecycle callback before deleting
an owned buffer, so reentrant owner cleanup cannot attempt a second deletion.

`forge.draft_comments` owns local compact and full-width comment presentation. PlanReview
uses it for creation, focus, collapse, raw body editing, and full-collection captures without
editing requests to Rust. Source extmarks retain semantic identities independently from
physical comment rows. Providers may supply sparse source identities. Read-only comments
reject focus promotion and deletion, while an owner callback admits editable source fields.
The renderer forces status-column layout before measuring rule width so line-number growth
cannot wrap the heading label. `forge.review_comments` uses the same renderer for inline PR
draft creation, focus, collapse, and local deletion. Native field extmarks retain other unsaved
text through those transitions. `forge.draft_source` caches source identities once per buffer
change or local render generation and maps navigation, syntax, and fold boundaries independently
from inserted comment rows. Materialization supplies immutable diff anchors for local creation.
Conversation and reply drafts use the same local creation path. Reply drafts retain the remote
parent identity and source anchor, reuse one unsent body per parent, and remain unavailable in
batched mode. Compact read-only comments expose their identity for reply selection without
admitting body edits. Existing conversation entries retain their explicit-open lifecycle.

Plan document recovery retains current local annotations and the latest queued explicit
operation independently. After replacement admission validates the source digest, the queued
operation keeps its original annotation capture. A queued submission receives the replacement
document, revision, view, and sequence identities before dispatch. Completion closes the current
review only when its physical buffer and plan identity match the originating submission.
An operation already dispatched to the obsolete host is not replayed by this recovery path.
Explicit PR comment saves capture only the selected body,
and batched review submission captures the summary and comments without accepting PR fields.
Generated patches defer while a local PR projection owns inserted comment rows. The adapter
coalesces those updates, detaches the clean local projection, and adopts a full host snapshot
before rebuilding source mappings. Pending native text prevents that replacement.
Review capture admission accepts client-owned comment region identities, immutable source
anchors, and optional reply parents with their bodies. `CommentStore` stages numeric identities
and validates reply ownership before `EditStore` reserves every field body. Both stores commit
only after the complete capture passes. New bodies retain an empty saved baseline. Repeated
captures preserve accepted identities and revisions. Comment save can resolve a captured region
to its numeric identity, and restoration retains that region when reopening saved draft state.
Materialization returns comment body snapshots and anchors for local presentation ownership.

`forge.status`, `forge.local_diff`, `forge.source_document`, and `forge.walkthrough`
bind native services to editor views. The Harness controller binds a native transcript document and a Lua-owned
composer buffer. Shared document commands resolve configured bindings,
help, selections, and gutters from native metadata. Physical source rows contain source
text, while decorations provide gutters and selection presentation without changing text.

`forge.window_presentation` retains the ordinary source-window options before a native
view applies document settings. Native documents use a fixed one-cell margin, while
Status and Harness request zero cells. Width capture stays exact before Neovim refreshes its
cached status-column layout. Historical source views capture that baseline before
switching buffers, preserve native line, sign, and fold columns, and opt out of mixed
document folds. Closing a view releases its exact window owner and restores only options
that still match its applied values. Complete historical Markdown source buffers permit
`render-markdown` presentation through its viewport parser, while Rust retains source
text, line anchors, and syntax identity. This exception does not admit the editor-wide
syntax highlighter, Otter, or Markdown parsing of mixed native documents.

`forge-diff` projects side-specific background and gutter groups and retains each syntax
capture's parser language, including injected languages. Projection preserves tree and query
precedence when overlapping captures share a priority, and Lua retains that order during
viewport indexing and redraw. Lua resolves those groups through the active theme. Gutter
widths follow the complete display group's line ranges, so chunks of one hunk keep aligned
columns without adding gutter bytes to the source text.

`forge.folds` owns native manual fold ranges and renders collapsed labels from header text
and semantic decorations. Each attached window retains its own open or closed preference.
Ordinary patches remove and recreate only affected fold subtrees. Unchanged native ranges
survive timer updates. One-line replacements use `nvim_buf_set_text` to preserve the line
and its fold membership. `nvim_buf_set_lines` can shorten a manual fold when replacing its
last line, even when the row count stays unchanged. Row insertions and deletions remove
affected folds before the edit and recreate them afterward. Harness and Status share the
text writer and `folds.prepare_records` invalidation path. Before either view changes its
row index, the fold engine queries the old sequence for boundaries touching each edit,
including an insertion exactly at an exclusive endpoint or a zero-row marker. This prevents
the logical endpoint advancing while Neovim leaves the native range behind. Indexed range
queries visit only intersecting blocks and the affected fold descendants, preserving
unrelated native folds. Status delegates those queries through its file/body sequence.
Body patch validation and text preparation finish before the synchronous commit removes
old folds, adopts the fragment, writes text, and restores folds. Subtree deletion selects the
complete native range before removing its descendants, including folds that share a header row. Creation temporarily opens
containing folds so Neovim does not widen a child range to a closed ancestor. Both operations
restore retained ancestor preferences within the atomic buffer update. Attaching a window
or replacing an authoritative snapshot rebuilds the native ranges without replacing retained
window preferences. Fold actions capture the
affected window's choices immediately. Detaching retains those choices even after its buffer
has changed, and a replacement window inherits the document's most recently detached view.
An existing window restores its own choices. Native ranges are rebuilt once from those
preferences, without a second fold-open/close replay. Closing a window drops its retained
entry, while the last detached view remains available until the document closes.
Native fold preparation and publication failures mark the replica desynchronized, report
the diagnostic, and request an authoritative snapshot through the document's recovery path.
Before native mutation, Rust and Lua validate node identity, parent existence, parent-before-child
ordering, and containment within ancestor folds. Node-owner and direct-child indexes select the
affected relationships when an edit changes a parent, child, or fold endpoint. Rejected updates
retain the last published text and revision. This validation does not rebuild or scan the entire
timeline for a streaming update.
Fold views record native ownership only after creation succeeds. Deletion restores temporary
window options even when an editor command fails.
Status demand skips closed folds,
and expanding a file admits its source through the existing bounded demand path.

`forge.cooperative` prepares Harness document updates in slices targeting four milliseconds
or 1024 work units. Preparation retains the committed text, folds, and decorations. A completed
update publishes text, metadata, folds, and view restoration in one non-yielding callback.
Each text replacement uses one buffer API call without first deleting the live range.
Document actions reject pending updates while cursor movement remains available during preparation.
Each resume checks the document owner, host generation, buffer validity, and changedtick.
Cancellation before publication preserves the prior frame and reports recovery through the
existing document failure path. Once publication starts, cancellation takes effect afterward.
Profiling records slice counts, maximum callback duration, maximum preparation duration, and
maximum non-yielding commit duration in `ui.document.slices`. Large initial snapshots can exceed
the preparation time budget during their atomic commit, so commit timing is tracked separately.

Each synchronous commit captures every attached window's current cursor identity and viewport
after preparation, immediately before changing native text or folds. The same non-yielding
commit restores those anchors against the new document. Movement during preparation is therefore
included in the capture and never suppresses restoration. Cursor snapshots do not cross
asynchronous preparation boundaries.

The replica's `before_commit` callback runs at that same boundary. Timeline switches use it
to save the outgoing view after backend requests and local preparation finish. Declaration
expansion captures its viewport after the response, immediately before publication. Rejected
editable-text changes capture cursors inside the scheduled rollback, not when queuing it.
Action-target snapshots remain separate and only validate whether a response still applies.

`NodeMap` owns the shared expansion choices, node incarnations, and tool output sources for
one open Harness presentation. Emitted nodes identify exchanges, messages, tool groups,
tools, changes, files, and hunks. Each node separates its source lifecycle, automatic display,
explicit expansion override, and loaded extent. `NodeState` travels inside block metadata,
so the same atomic publication changes the text, node state, and native fold range.
Content blocks carry the owning node identity, allowing Tab on output rows to address the
same node as its heading. The protocol version changes with this shared contract.

Lua sends `node` actions with a view sequence and node generation. `set_expansion` changes
explicit intent, `load_more` extends a loaded page, and `retry_loading` retries after a
reported failure. Rust rejects a retired generation before materializing content. All windows
showing one presentation share the choice. Closing a parent preserves child choices, and
closing a single window preserves the choices of the remaining windows. A per-node intent
sequence also rejects an older request from another attached view. The last window
releases the presentation's expansion state. Deleting a canonical entry retires its nodes,
while scope switching retains their choices for a return to that scope.

Each tool retains independent heading and output blocks. Explicit expansion materializes
its output through stable 16 KiB source chunks and a bounded loaded page. The initial
formatted source prefix is at most 64 KiB before wrapping. Later page requests extend that
prefix without reformatting existing chunks. Raw output beyond the prefix stays retained
without formatted blocks, including subsequent streamed tails. Reflow preserves the
requested source extent. Closing and reopening the tool resets it to the initial prefix.
Tool and file
pages start at two viewport heights and extend when their boundary approaches the viewport. The latest tool in
an active group receives the automatic four-row preview. Settled groups default to headings,
and an explicit closure suppresses the preview. Closing a node removes its materialized body
without inserting a blank placeholder. Parent closure also drops descendant page quotas
while retaining their explicit choices. The heading's node metadata remains actionable even
when no native fold exists. Both top-level and nested caret markers read that node state,
so an unloaded body does not remove its expansion affordance.

Ordinary message updates replace their indexed block. Streamed tool tails splice only the
changed output chunks and update the owning node metadata. Hidden chunks produce no body
patch. `SectionProjection` retains a complete-block index so these updates do not reproject
the containing exchange. Structural changes and partially loaded blocks use the subtree
projection path. Expansion and paging project only the indexed node subtree. Pending parent
and child actions coalesce at the outermost affected node and resolve the latest choices
before projection. Subtree replacement and enclosing fold endpoint rebasing share one
validated document edit. An enclosing fold ending at the replaced subtree's boundary moves
to the replacement's last block, even when its former endpoint survives as a heading.
Retaining that heading must not leave newly loaded children outside their parent fold.
Native folds follow the published node display rather than keeping a
second Harness expansion preference. After a body splice repairs fold endpoints, subsequent
heading updates preserve the current loaded ranges at application time. They cannot restore
fold metadata captured before the splice.

Tool activation binds to the captured document, view, block, and target identity. A later
transcript revision can still activate that same retained tool target, so streaming output
or timer updates cannot invalidate an unrelated click. Future revisions, removed targets,
cross-block targets, and replayed input sequences remain invalid. Position-dependent
navigation continues to require the captured revision and coordinates.

Lazy section projection preserves zero-row blocks as empty anchors. An empty hidden-count
block consumes no page budget and cannot mark a tool group as truncated. This prevents
spurious blank rows and repeated page requests after all source content has loaded.

Section expansion validates its next node choice, quota, and rendered prefix together.
A failed preparation restores only that node's previous choice and quota. A continuation requires an existing continuation marker and must advance the final
loaded block or finish the section. Failed preparation preserves the previous expansion state.
Projection refresh retains dirty entries until publication succeeds. A publication failure
invalidates the native presentation and requires reopening rather than serving a partial frame.

Timeline projection borrows canonical blocks and agent exchanges. Opening a parent does not
copy closed descendant text into a temporary exchange. It constructs only the requested
visible prefix, copying only metadata that belongs to that prefix. Source indexing still
visits the owning exchange's block metadata. Full-size borrowed indexes contain references,
not duplicated text.

`BufferText` shares immutable packed rows and their row index through `Arc`. Block snapshots,
pending patches, fold metadata replacements, and Markdown jobs retain that storage without
copying its bytes. New text creates new storage, so an older published snapshot cannot change.
Admission accounting conservatively charges the full retained storage to each owner.
Serialization visits rows directly without allocating an intermediate row vector. Pending
patch sizing uses a counting writer instead of retaining another encoded copy.

Closed historical tool sources retain raw output without constructing a parser or row index.
The first preview, expansion, or separate output view initializes the incremental parser.
Subsequent deltas reuse it, including partial ANSI sequences. Heading replacements retain the
parser when the saved source version is unchanged.

`ToolOutputSnapshot` shares raw output, normalized display text, and row offsets with the live
tool view. It owns an independent pagination cursor and cannot append. Creating an output
window neither copies nor reparses the saved output. A subsequent live append copies shared
storage on write, preserving the output window's captured version. The streaming parser
remains exclusively owned by the live view, including incomplete ANSI sequences.

The timeline copy audit retains these ownership boundaries:

| Boundary | Retained copying |
| --- | --- |
| Canonical events and timeline state | Changed exchanges and deltas own their mutable strings independently. Patches own changed entries until delivery. Settled history is borrowed during reconciliation. |
| Reconciliation comparison cache | Serialized values support exact content comparison. The working reconciliation uses references to those values and entries, preserving rollback without another history copy. |
| Rendered text and Markdown jobs | Text and fenced-code maps share immutable storage. Metadata remains independently owned where projection rewrites coordinates or fold endpoints. |
| Node preparation | One previous expansion choice and page quota support rollback. Streaming splices retain block identities and share immutable text, without copying the complete choice map or exchange body. |
| Native Lua adoption | Lua copies changed metadata tables because normalization and decoration preparation mutate them. Lua strings remain shared, and patch adoption does not deep-copy transcript text or the full block sequence. |
| Transport | Encoding creates the wire payload. Decoding creates the receiving process's strings. These process-boundary copies remain necessary. |

Lua tracks each section request by view and request sequence. An acknowledgement permits
opening only after the matching content has been adopted. Superseded requests cannot clear
another view's loading state. Automatic pagination compares the final loaded block and row,
so unrelated transcript revisions cannot restart the same continuation. Failed sections retain
one diagnostic and stop automatic demand until an explicit reopen.

The presentation request queue checks host, document, view, and timeline lifetimes before
dispatch and delivery. Timeline selection retires pending section and action callbacks while
retaining view registration and cleanup operations. Requests settle once. A 30-second timeout
reports an unknown outcome and stops presentation work until reopening. Callback exceptions
follow the same failure path, preserving committed buffer text and replacing the working hint
with a persistent presentation error. These errors do not mark the underlying task complete.

Standalone tool output stops demand after delivery errors or pages that add no rows while
claiming more content. Explicit retry first adopts the native snapshot before requesting another
page, recovering from responses that advanced the native cursor without reaching the buffer.
Closed output views and replaced hosts cannot adopt late responses.

Harness Markdown resolves visible message owners through the weighted sequence and skips closed
native folds. The parser includes complete visible messages across all attached windows, retaining
delimiter context without scanning off-screen history. Per-block versions suppress renders after
unrelated tool and timer patches. Parsing uses a completion callback guarded by the render lifetime.
Leaving all Markdown regions stops their highlighter. Cursor reveal checks use the cursor range,
and display and inline math share asynchronous conversion with at most four active processes.
Conversion failures retain source text and report the underlying error. A converter has a
30-second process deadline, and completion only refreshes surviving buffer/window owners.

Lua timeline replicas retain tool deltas as immutable append chunks. Consumers materialize the full
string on demand, while ordinary output events copy only their ancestor records. Timeline tracing
uses the buffered asynchronous performance logger rather than writing a file in the update callback.

SQLite stores output append chunks separately from exchange metadata. The indexed tool-owner table
validates delta admission without parsing exchange JSON. Initialization migrates legacy inline output
and ownership in one transaction. Lifecycle barriers synchronize metadata and chunks atomically,
and publication follows a successful commit. Adapters stamp the first receipt time on `BackendEvent`
before waiting for output capacity, preserving provider timestamps and distinguishing broker delay
from tool execution time.

`StatusDocument` retains the displayed HEAD and each file's index state, worktree stamp,
and rename-origin stamp. The private wire protocol is version 4. Status, local previews,
and comparisons publish `StatusSnapshot`, containing a document revision, view kind,
repository context, ordered section membership, and semantic file records. A file record
contains a numeric handle, source generation, section, change kind, display path, optional
rename origin, untracked flag, and statistics state. Initial messages contain no rendered
blocks, row or column ranges, highlights, or diff bodies. Rust performs Git classification
and retains raw repository paths. Lua never derives action paths from displayed text.

`forge.status_render` owns every status header, including HEAD, upstream, push, PR, About,
Issues, and recent commits. It formats real buffer text and cached highlight chunks. The
viewport decoration provider draws visible header chunks and fold labels directly. The
initial weighted sequence builds in O(files + context rows) time, with cooperative
preparation checkpoints targeting four milliseconds between yields. Each file node owns
its header and the row weight of its independent body sequence. Body growth updates that
weight in O(log files) time without rebasing every later file. Neovim retains native folds,
window settings, selections, search, and yank behavior. The first attached window owns
context wrapping width, and ownership transfers when that view closes.

Expanding a file sends a typed file handle through `StatusInput`. Rust lazily acquires and
validates its source, then returns `BodyDelivery` with the file handle, source generation,
and first body `BufferSnapshot`. Continuations contain body-relative `BufferPatch` values.
Each body has its own document identity and revision. Rust still produces source text,
hunk targets, source coordinates, syntax, gutters, and fold metadata inside those bodies.
Lua validates the fragment through shared `forge.buffer` primitives and splices only that
file's rows. Expanding one file does not serialize or validate unrelated headers or bodies.
Body delivery does not advance the inventory revision.
Read-only demand accepts an older inventory revision for a still-live file handle and
serves its current generation. Future revisions, forged handles, and replayed input
sequences remain invalid. This prevents unrelated context and mutation updates from
failing a visible file read.

Refresh returns `StatusDelta` with changed file records, retired handles, changed section
membership, and changed native context. Stable source identities retain their loaded body.
A changed source increments the file generation and retires that body's targets before
replacement. Handles never address a newly created file after retirement. Lua rejects late
body deliveries from another generation. Header-only statistics changes preserve loaded
bodies. An inventory revision mismatch requests the semantic snapshot, while a body revision
mismatch requests only that file's body snapshot. Recovery does not retry an invalid
snapshot indefinitely. PR and About producer results update context rows locally, retaining
the file index and avoiding presentation round trips through Rust.
Inventory construction sorts raw paths within each section, including mutation projections
collected from unordered maps. When a text edit removes or relocates retained body rows,
the renderer reinstalls that body's inline metadata at its new position. Snapshot recovery
clears the namespace before rebuilding metadata so orphan gutters cannot survive.
Status retains a one-cell display margin on both initial and wrapped source rows,
matching the pre-migration diff view. Neovim owns soft word wrapping of intact
source lines. Inline gutter text consumes width only on the first display row.
The margin participates in width capture and follows each attached window through
buffer return and splits.

Before status text edits, Lua captures fold states by fold identity for each
attached window and restores surviving identities afterward. Capture temporarily
opens ancestors to distinguish hidden child states, then restores the ancestors
and window view before returning. Buffer reattachment uses the same capture path.
Restoration refreshes the native fold-expression cache and closes only rows with
a native fold, so removed, one-line, and suspended folds cannot cause E490. This
prevents row movement from opening unrelated files or closing expanded neighbors.

Repository collection reuses gix analysis for tracked modifications and byte line counts
for complete added or deleted sources. Cached worktree statistics use file metadata rather
than hashing every file. The existing line-count skip flag remains a profiling option.
Mutation updates retain the document's filename counts in memory. Whole-file
stage and unstage transfer the selected side's counts to the destination, including
queued action replay. Settlement verifies Git status and source metadata without
reading content for statistics or running a count comparison. Settled headers retain
the visible counts until a normal refresh or body demand supplies current counts.
These labels can therefore be stale when changes combine or an external edit intervenes.
They never serve as content identity, write preconditions, or entries in the source-keyed
count cache. Expanded hunk updates obtain counts from the diff analysis already required
to rebuild their bodies, rather than running a separate count operation.
Unknown and unavailable counts remain distinct from exact zero counts. Untracked files
retain their distinct mutation identity inside the Unstaged section. Section actions select
both tracked and untracked members. Recent commits retain exact native commit targets.

Header input carries a file handle, section, or context role, without Lua layout coordinates.
Body input additionally carries its generation, revision, block, position, and optional
target. Rust verifies those coordinates against that body's source metadata. Navigation
from a non-actionable title uses a before/after-files boundary, which cannot select a Git
mutation. Lua resolves visual ranges to semantic target lists and captures the first
selected target even when the cursor ends on a collapsed fold's hidden blank row. Rust
validates each target before preparing the action. Status inventory and source validation remain native.
Whole-file staging writes current worktree contents when the native queue executes it.
It does not capture or hash worktree contents or require the displayed index entry to
remain unchanged. The same policy applies to whole-file targets inside a batch.
The writer groups adjacent compatible targets into commands of at most 256 paths,
including selections smaller than 256. Stage, unstage, and tracked discard send literal
NUL-delimited pathspecs through stdin. Selected untracked deletions form separate groups
and do not spawn Git. Mixed action lists retain their original order and merge only
adjacent groups with the same operation and source behavior.
Patch stage, unstage, and worktree discard combine distinct-file patches with the same
direction into one `git apply` input, bounded to 256 targets and 16 MiB. Staged-hunk
discard and compound rollback retain their dependent preflight and mutation steps.
Before each group, the writer verifies every selected source. A failed command marks
every target in that group as uncertain, preserves completed earlier groups, and leaves
later groups unstarted. Settlement observes every affected path before queue handoff.
Whole-file staging retains an index scan to reject directory targets but skips HEAD-tree
membership and repeated index-content comparisons that its current-content policy does
not require.
Hunk staging checks the displayed worktree metadata against execution-time metadata,
then checks it again before the target write. It retains exact index-source validation
for patch application. A changed source rejects the hunk without applying its patch.
Native settlement observes affected paths and publishes a settled delta that removes
the optimistic layer, invalidates stale bodies, and lets Lua demand current content.
Index-only unstage excludes worktree-only drift. Destructive discard retains content
fingerprinting and selected-source checks. The fingerprint streams through an 8 KiB
buffer without imposing an 8 MiB file-size cap. All policies retain cancellation,
execution deadlines, and HEAD validation. Metadata sampling does not detect edits
that preserve every sampled field or lock out external writes between check and use.
Discard confirmation retains its captured target, source revision, and selection, renews
only the submission sequence after background demand, and rejects changed replica, view,
or host ownership.

Forge Ignore on a staged file submits a whole-file unstage through the native
writer, then persists its private ignore marker only after that target completes.
Renames include both endpoints. The working tree remains unchanged, and a failed
unstage leaves the file staged and visible. Ignoring unstaged or untracked files
only updates the private marker store. Ignored rows use the resulting worktree
diff, and Unstage on an Ignored row removes its marker.

Stage and unstage use a worktree-scoped native mutation journal. The journal holds one
confirmed observation and an ordered predicted index projection shared by every open
Status document for that worktree. Admission returns an `accepted` `StatusDelta` before
Git execution, so Lua updates only the changed headers and bodies without reconstructing
the status layout. Untracked and ignored records without a sampled worktree mode use a
provisional regular-file mode in the predicted index. Unknown mode is not absence:
these files move to Staged in the same accepted update as tracked files, without
waiting for source loading or Git completion. Settlement replaces the provisional
mode with Git's actual mode. Tracked deletions continue to predict an absent entry.
A zero worktree mode from Git also denotes absence. Unstage preserves the Deleted
classification for that mode instead of predicting Modified. A second action resolves
against the predicted index. Closing a view
unsubscribes its document but does not release an admitted write or its settlement receipt.

Hunk actions capture immutable byte edits from the source comparison instead of display
rows. A completed write observes the repository, adopts that observation as confirmed,
then replays each later exact edit through known byte transitions. An overlapping or
unavailable later edit is cancelled with a diagnostic while independent targets continue.
An unverified write outcome quarantines conflicting writes until a read-only repository
observation reconciles the coordinator. A HEAD change cancels later queued mutations
because their captured source comparison no longer has a valid base.

Body delivery initially uses the available raw diff context. Native Tree-sitter analysis
updates structural hunk context asynchronously and applies only when the file source and
body generation still match. Rust sends semantic `StatusDelta` and `BodyDelivery` data,
while Lua formats text, highlights, decorations, folds, and cursor preservation. Large
document events use bounded multipart transport and Lua publishes the update only after
the complete event has been assembled and validated.

Native syntax analysis applies the 10,000-line cutoff independently to each source
version. It has no source-byte reservation, retained-memory rejection, capture-count
limit, injection-count limit, or decoration-count limit. The shared worker pool still
queues work and propagates cancellation. Its byte admission excludes syntax sources,
which are already retained by their owners. The cache retains up to 256 results and
evicts old entries without invalidating document handles or rejecting new analysis.

Startup logging separates repository acquisition, serialization, transport decoding,
semantic formatting, index construction, buffer writes, post-application setup, and the
first displayed redraw. Semantic application records callback count, total work, and the
longest renderer callback. File expansion records demand, body application, and the first
body redraw. Measurements use the manually built release executable in a fresh host.
Existing hosts retain their process-owned executable until shutdown.

The detailed legacy rendering sections below describe isolated compatibility modules,
not public Status, PR, review, source, Walkthrough, or Harness command paths. The
repository's `impl-status.md` records current migration boundaries.
`nvim/rust/forge/MIGRATION_DESIGN.md` retains the accepted requirements and completion gates.

A deep reference for the `forge` Neovim plugin: a self-contained, in-editor
Git review UI. It renders staged/unstaged diffs, single-file diffs, branch diffs,
historical file revisions, GitHub pull requests, a batched PR review mode, and an
LLM-authored guided walkthrough — all as native Neovim buffers with tree-sitter
syntax, intraline highlights, and folding.

This document is the map a new developer should read before touching any subsystem.
It covers the directory layout, the architectural seams that hold it together, every
buffer type, the render engine internals, and the end-to-end data flows. Pair it with
`.rulesync/rules/forge.md` (testing, linting, the live-nvim debugging recipe)
and the repo-root `architecture.md` (the diff-render decoration-provider design notes).

---

## 1. The shape of the system

The plugin is a **layered package** with one stable public entry point —
`require("forge")` — and a strict dependency direction:

```
user command / github integration
        │
        ▼
   init.lua  ─────────────────────────────┐  the "star hub" facade
        │                                  │  (re-exports everything,
        ▼                                  │   owns shared mutable state)
   views/  ──────────────┐                 │
   (status, pr,          │                 │
    diff_buffer)         ▼                 ▼
    diff_buffer)     render/           shared/   infra/
        │            (diff engine)    (plumbing) (config, perf,
        ▼                │                         highlights, paths)
     git/  ◄─────────────┘
   (data layer)
        │
        ▼
  integrations/  (gh CLI, ai, commit bridge)
```

The golden rule: **everything depends inward toward `git/` and `render/`, never the
reverse.** A render module never reaches up into a view. A view reaches the data layer
through the facade, never the other way around.

Two design constraints shaped the layout and explain almost every "why" in the code.

**Constraint one — Lua's 200-local-per-chunk limit.** `init.lua` was once a single
20,000-line file. It now imports ~60 modules. If each were a `local`, the main chunk
would blow past Lua's hard cap of 200 locals. So every submodule is attached as a
**field on the module table** (`M._git_data = require(...)`), never as a bare local.

**Constraint two — circular requires.** Views, render, and git all need shared state
(the current status buffer, the diff caches). That state lives in a dedicated
**`session.lua`** store that `require`s nothing, so every layer imports it directly with
no cycle. A narrower need remains for shared *functions*: `init.lua` re-exports the
package's functions and requires those same modules at load time, so they cannot
`require` it back at the top level without a cycle. The **`dr()` seam** (Section 3)
resolves that lazily.

---

## 2. Directory layout

```
forge/
├── init.lua                  Thin public facade: exposes setup/get and user-facing open* entry points
├── session.lua               Shared mutable state: status registries, diff caches, Harness presentation handles
├── types.lua                 Shared LuaCATS (---@class/---@alias) catalog — annotation only, never required at runtime
├── query_runtime.lua         Puts the plugin root on the runtimepath so bundled queries resolve
├── docs/architecture.md      This document
├── walkthrough.lua           Native walkthrough document adapter and view lifecycle
│
├── views/                    Buffer-producing views (one boundary per user-facing surface)
│   ├── commands.lua          Public open* entry points: open / open_pr / open_review / open_branch_diff / open_file_revision / open_compact_preview
│   ├── diff_buffer.lua       The standalone single-file diff:// buffer with hunk-level stage/unstage and folds
│   ├── branch_diff.lua       Working-tree-vs-branch diff rendered into a status buffer
│   ├── file_revision.lua     Read-only view of a file at a historical revision
│   ├── harness/              Dedicated interaction-tree/composer tab and prompt queue controller
│   ├── plan_review/          Canonical plan review with semantic task rows, annotations, folds, accept, and revision
│   │   ├── init.lua           View lifecycle and composition of model, renderer, comments, and folds
│   │   ├── task_model.lua     Non-rendering working.json/index adapter with canonical source anchors
│   │   ├── entity_info.lua    LSP-style entity-description hover resolved from canonical plan data
│   │   ├── entity_navigation.lua Planned, workspace, and exact-version external entity navigation
│   │   ├── entity_rename.lua  Added-entity rename prompt and client-side eligibility boundary
│   │   ├── comment.lua        Display-row projection plus editable source-anchored comments
│   │   ├── fold.lua           Plan task native-fold state capture and application
│   │   └── schema.lua         Read-only working.json view with return-to-review lifecycle
│   ├── sessions/             Current-worktree and all-worktree durable session browser
│   ├── pr/
│   │   ├── pr_overview.lua    PR metadata, checks, review summaries, inline comments
│   │   ├── pr_edit.lua        In-place edit of PR title/body/reviewers/milestone with queued mutations
│   │   ├── pr_view.lua        Thin external-caller wrapper around open_pr
│   │   ├── review.lua         Batched review mode: draft comment CRUD, viewed-state, verdict, submission
│   │   └── reviewer_source.lua  blink.cmp @mention completion source
│   └── status/               The :ForgeStatus view, decomposed into one responsibility per file
│       ├── state.lua           State lifecycle + per-buffer autocmd state machine + perf wrappers
│       ├── status_buffer.lua   Accumulates lines/highlights/extmarks/folds into a per-buffer state
│       ├── comment_box_rows.lua Owns compact comments as real status-buffer rows and resize records
│       ├── status_render.lua   Full render pass plus vim.diff-driven minimal buffer-line reconciliation
│       ├── render_orchestrator.lua  Async git-root + load pipeline, PR-detail/PR-diff render passes
│       ├── status_head.lua     Head/about lines (HEAD/merge/push rows, PR summary, section headings)
│       ├── status_keys.lua     Stable identity keys for sections/files/hunks (fold + cache + action index)
│       ├── status_helpers.lua  Shared helpers: notifications, git command building, branch creation, popups
│       ├── status_debug.lua    Dev-only event log plus row/extmark/syntax inspection dump
│       ├── status_issues.lua   `#`-issue completion + issue integration in the status buffer
│       ├── section_map.lua     Pure section projection, path replacement, and semantic equivalence
│       ├── operation_journal.lua Confirmed section baseline plus ordered optimistic mutation layers
│       ├── ignored_path_store.lua Durable worktree-scoped virtual ignore markers and stage suppressions
│       ├── status_sync.lua     Optimistic cache projection and path-scoped authoritative synchronization
│       ├── section_builder.lua Build sections/files from diff text, attach review comments
│       ├── fold_state.lua      Per-key fold map, native fold application, foldtext, resize refresh
│       ├── size_gate.lua       Estimate render cost, decide which big files defer their body render
│       ├── diff_source_state.lua  Per-file diff-source state bridging status entries to the render engine
│       ├── entry_nav.lua       Cursor/entry navigation, action-target resolution, decoration prewarm
│       ├── commit_view.lua     Commit-message editor, About view, create-PR/verdict/help popups, push/pull
│       ├── window_options.lua  Window-local option overrides (number/fold/conceal/wrap) with restore
│       ├── pr_state.lua        Async PR + AI-about lookup lifecycle with request-id race guards
│       └── actions.lua         Stage/unstage dispatch into the optimistic journal and index coordinator
│
├── render/                   The shared diff render engine (pure + async, view-agnostic)
│   ├── source.lua             Diff-source data model: registry, per-source/per-file state, lazy text loaders
│   ├── source_loader.lua      One-call resolve of registry/source/file + replace a file's hunks
│   ├── diff_parse.lua         Pure unified-diff parser: text → blocks → hunks → gutter-annotated lines
│   ├── hunk_model.lua         Pure hunk model: change regions, context scopes, padding, coordinate maps
│   ├── hunk_index.lua         Index a hunk into sections + body rows for lazy chunked rendering
│   ├── intraline_diff.lua     Compact similar -/+ line pairs into one row with character-level highlights
│   ├── syntax_engine.lua      Async tree-sitter syntax + hunk-context producer with three caches + prewarm
│   ├── syntax_context.lua     Per-file tree-sitter parse state (snapshot/parser/tree/query) → highlight spans
│   ├── diff_render.lua        Build fancy-diff rows (gutter/boundary/body) and apply them as buffer extmarks
│   ├── diff_component.lua     Shared file headers, hunk rows, and status-buffer accumulation
│   ├── row_emitter.lua        Emit shared diff-row spans for ForgeStatus, PR review, and Harness
│   ├── diff_tree.lua           Compose file headers and fancy hunks into an indentable fold tree
│   ├── display_text.lua       Shared display-cell wrapping with semantic first/continuation prefixes
│   ├── task_tree.lua          View-independent semantic task tree → wrapped rows + fold metadata
│   ├── task_tree_style.lua    Shared task/action/kind/target highlight segments
│   ├── fold_presentation.lua  Shared native-fold labels, filler, and folded-row window chrome
│   ├── comment_box.lua        Pure compact comment-box wrapping and segmented row layout
│   ├── comment_editor.lua     Shared full-width comment rules and editable-body line normalization
│   ├── layout.lua             Fenwick (binary-indexed) tree mapping items → buffer rows in O(log n)
│   ├── row_tree.lua           Logical node tree (hunks/padding/annotations) kept in row-sync via layout
│   ├── region.lua             Extmark-anchored buffer region with dirty tracking
│   ├── annotations.lua        Review-comment model: by-anchor index + sync state machine + serial sync queue
│   ├── decoration.lua         Decoration-provider cache for ephemeral per-row syntax highlights
│   ├── text_snapshot.lua      Immutable byte-indexed text snapshot (line spans without copying lines)
│   └── harness/              Interaction tree, tool, Markdown, queue, and node-local transaction renderers
│
├── git/                      The Git data layer
│   ├── git_backend.lua        Pluggable async process runner (vim.system or an injected test backend)
│   ├── git_data.lua           Diff parsing, snapshot integration, session caching, async syntax compute
│   ├── status_snapshot.lua     Atomic status, filtered-patch, and added-file metadata snapshot
│   ├── file_body.lua           Lazy added/deleted preview loader using worktree bytes or Git blobs
│   ├── index_mutation.lua      One semantic stage/unstage mutation with partial-success reporting
│   ├── mutation_coordinator.lua Repository-root FIFO, burst debounce, settle, and failure recovery
│   └── repo_config.lua        Per-repo .forge.json reader (branch_prefix) behind a test seam
│
├── integrations/            External services
│   ├── gh.lua                 GitHub CLI / API bridge: PRs, checks, review comments, submissions
│   ├── ai_commit.lua          LLM commit-message generation with diff context + fingerprint dedup
│   ├── commit.lua             Fake-editor commit bridge: headless nvim as GIT_EDITOR over RPC
│   ├── conventional_commit.lua  Parse the conventional-commit prefix into colored segments
│   └── datetime.lua           Relative/absolute date formatting + date highlight ranges
│
├── harness/                 Thin Neovim client for the Rust Harness broker
│   ├── builder.lua            Manual build paths and process-owned executable copies
│   ├── client.lua             Long-lived JSONL process, request correlation, events, generation guards
│   ├── protocol.lua           JSONL request/message codec
│   └── backends/              Lua launch descriptors only: Codex app-server, Copilot SDK, and test mock
│
├── infra/                    Cross-cutting leaves
│   ├── config.lua             Config schema + defaults + setup merge (keymaps, perf, lookup mode)
│   ├── choice_popup.lua       Shared keyboard chooser for lifecycle, closed-PR, and verdict menus
│   ├── popup_window.lua       Shared float construction plus origin window/mode restoration
│   ├── highlights.lua         Define every Forge highlight group from the active colorscheme
│   ├── notifications.lua      Centralized error + git-failure notifications
│   ├── perf.lua               JSON event/span profiler gated by config
│   ├── paths.lua              Path normalization + repo-relative resolution (Windows-aware)
│   └── util.lua               Leaf utilities: stat counting, buffer lookup, NUL detection, filetype
│
├── shared/                   View plumbing shared across all views
│   ├── keymaps.lua            Install per-view buffer keymaps + the sticky hint-bar winbar
│   ├── command_specs.lua      Pure data: the command vocabulary (id/label/views/hint order)
│   ├── view_controller.lua    Registry of per-view-kind controllers (render/action hooks)
│   └── view_command_set.lua   Per-buffer action registry with enabled() guards
│
└── queries/                  Bundled tree-sitter queries (discovered via query_runtime)
    ├── <lang>/diff_context.scm    Capture the named scope (fn/struct/class/...) around a changed line
    └── <lang>/diff_inventory.scm  Extract all named symbols + line ranges for the change inventory
```

Contributor-facing assistant rules live at the repo root in
`.rulesync/rules/forge.md`. Keep that rule focused on working practices and keep
this document focused on architecture.

### Harness timeline ownership

Timeline prompts and final-response bodies use `●`, thoughts use `○`, non-foldable response summaries use `●`, and lifecycle rows use `◇`. Only fold headings use
`▸` when closed and `▾` when open. Rust assigns markers from actual fold ownership, including
file headings. Response summaries retain `●` until they own foldable content. Tool calls
retain `•`, and hunk headers retain their ForgeStatus presentation without arrows.
Neovim renders fold direction per window through its status column and visible-row
decorations. Each marked heading has one arrow derived from native fold state. The status
column does not use multi-level `%C` rendering, which can display multiple arrows when nested
fold ranges start on the same row. Native fold commands and split windows update indicators without rewriting the
shared buffer. Collapsed labels retain the same indentation and marker column as open headings.
Tool groups inherit the same layout depth as commentary. Their rows start with the `•` marker
without additional leading spaces. Output branches add two columns relative to that marker.
Tool timings round to the nearest tenth in a fixed five-cell column, accommodating `99.9s`.
Rounded values reaching 100 advance to the next unit, starting with minutes, so timer
updates never shift command columns.
Tool group headings sum the observed runtimes of their calls, including overlapping calls.
An unavailable call duration suppresses the total. Each output view retains its group identity
so timer updates can refresh the group heading without scanning or replacing output bodies.
Pending approvals and user questions suspend provider-inactivity notices. Approval waits use
informational styling and remain pending when their picker closes. Controller renders and
presentation callbacks share status-hint selection so updates cannot alternate between a
wait notice and the underlying activity. Connection and execution failures retain precedence.

Transcript presentation follows the final row after updates whenever the current buffer is not
the transcript. Focus, rather than the previous cursor row or row-count growth, controls automatic
following. Leaving the transcript resumes following immediately. Focused transcript readers retain
their cursor and viewport through the shared buffer view preservation, including snapshot recovery.
Tail positioning reveals the end of wrapped final lines in every attached transcript window without
changing focus. Explicit prompt submission and agent actions can request tail positioning directly.


`client.lua` owns one persistent Forge JSONL process. Repository requests do not require Harness
initialization. Harness requests and streamed events carry a durable session id, so the process routes
independent turns without treating one session as globally active.
`session.lua` mirrors that boundary through `harness_by_id`: each live session owns a transcript buffer, composer
buffer, windows, queue, busy state, and subscriptions inside one real Neovim tab.

Lua owns the complete unsent Harness draft. Opening a transcript and typing in HarnessInput never
transmit draft text or local edit revisions to Rust. Ctrl-s captures at most 64 KiB and 4096 rows,
then sends one `prompt.submit` request with complete text and a monotonically increasing token
bound to the transcript document lifetime. Rust tracks submission admission without retaining a
composer document. A `prompt_submission` acceptance clears only the unchanged captured draft.
Retraction restores its exact rows only while the cleared buffer retains its changedtick.
Newer typing survives both transitions. Rejection retains the draft, and closing or replacing
the presentation invalidates late transitions. Plan preparation and Git checkpoint capture still
precede acceptance.

Rust projects the semantic timeline and its synthetic bottom status from the same session-scoped durable owners.
`ExchangeLayout` preserves the exchange's ordered nodes while assigning question clarifications to their branches.
The activity summary fold ends before the first outer final response. That response and later question groups,
messages, tools, and plan events retain their relative order outside the summary fold. The transcript renderer
consumes those ordered ranges directly without deferring responses to the end of the exchange. Clarification
responses remain inside their question branch folds. Streaming, completion, and reload use the same layout.
Thoughts and tool groups nest one two-column level below their exchange summary, including
continued activity after an intermediate final answer. Tool calls and output retain their
additional nesting. Final answers stay at the response level in their original chronological position.
`PlanStateMachine` validates question, feedback, submission, review, acceptance, cancellation, and failure
transitions. `PlanQuestionLedger` records answers, skips, and withdrawals before generation resumes. It matches both
the provider's logical id and a canonical digest of the user-visible content, so a resolved decision cannot become
pending again when a provider retries with a different id. The transient `PlanElicitation` therefore contains only
unresolved questions and never acts as the durable decision owner.

The broker owns plan generation as one bounded continuation loop across every backend. Each provider turn first
applies plan creation and edits to the canonical document, then the broker accepts only three forward outcomes: a
submitted plan, a genuinely new unresolved question, or another bounded generation turn. `ContinuationBudget`
centralizes the total-turn and consecutive-no-progress mechanics shared by goals and plan generation.
`PlanGeneration` adds canonical document revision evidence, permits 20 total turns, and stops after two consecutive
turns without canonical document progress. `/plan retry` resets only that budget while retaining the plan document
and question ledger. Provider adapters stream one turn and never decide whether planning should continue.
An automatic planning retry admits a new exchange with an empty user prompt and the persisted
`Planning resumed` lifecycle label. The preceding exchange completes before the retry starts,
so its answer, activity fold, duration, and usage remain separate after reload. Admission
publishes the preceding exchange's terminal state before the new exchange appears. The task
and canonical plan retain their identities across these exchange boundaries.
Goal continuations and execution retries also admit new exchanges without synthetic user
prompts. Their lifecycle rows identify `Goal continued` or the current execution phase as
started or continued. These boundaries preserve the prior attempt's metrics and outcome even
when the same task, goal, and provider thread continue.
Native goal streams use the next parent turn's start as the exchange boundary without sending
another prompt. Replayed boundaries cannot reopen old exchanges, and late usage stays attached
to its original provider turn. Child turns retain their own delegation exchanges.

`SessionPhase` projects exactly one visible workflow phase from that control state. Plan review and planning failure
preempt retained transport activity. Active retries expose `RetryingPlanGeneration`, while exhausted or failed turns
expose `PlanningFailed` with `/plan retry` and `/plan cancel` as the only recovery actions. A submitted plan therefore
cannot reopen a stale question picker, and a failed generation turn cannot roll consumed feedback backward.

Harness response text is published before fenced-code syntax analysis. `MarkdownRenderer` retains
code languages, literal rows, and their rendered byte positions alongside the Markdown projection.
`SessionPresentation` owns these sources independently from navigation actions. Its `TranscriptSyntax`
jobs analyze saved diffs and Markdown code through the shared syntax engine outside the presentation
lock. Completion must match the open document and retained source before adding viewport decorations.
Replacement, reflow, and close invalidate obsolete work. Unknown fence languages retain plain text.
The timeline has no aggregate byte, row, or block-count limit. Retained tool parsers and syntax
metadata do not consume a session-wide admission budget. Each syntax job has a 10-second deadline
and uses the shared 10,000-line source cutoff. Syntax results have no separate byte or span-count
rejection. Content loading has no total byte-size admission limit. Pages and transport parts stay
bounded, and their consumers request subsequent batches. Tool display stops after 262,144 source
rows with an explicit truncation notice, while export retains the complete original response.
A tool row longer than one output-page budget is abbreviated in that page with an export hint.
Shared Markdown projection and literal wrapping also accept content without byte, row, or
decoration-count admission limits. They retain source and navigation metadata for later paging.

Each Rust session controller owns one `TimelineStream`. The stream compares stable top-level entry identities, advances
its own monotonic revision, and emits ordered `insert`, `replace`, `remove`, `tool_output`, and `message` operations. Provider lifecycle events
remain transport evidence for approvals, context metadata, and diagnostics. They no longer mutate transcript records
inside Lua. Initial load, explicit reconciliation, resume, and preview carry full snapshots. Normal streaming carries
only the changed item. Tool-output batches append to the indexed canonical call without
cloning or serializing its exchange. Existing message updates replace one message and parse
only its Markdown block. Message creation, snapshots, and lifecycle changes reconcile the
owning entry. Lua stages patches atomically and copies only the path to the changed call or
message. A rejected owner or revision leaves the prior cache intact.

Provider delivery coalesces adjacent deltas for the same owner, call or message, and phase
for at most 16 milliseconds or 64 KiB. Lifecycle, snapshot, and owner changes end a batch.
The bounded queue waits asynchronously for capacity. A cancelled receive retains its partial
batch for the final drain. Tool timers prefer provider timestamps and reported duration,
then adapter receipt time, so broker queue delay does not become tool runtime.
The `broker.provider.dequeued` trace records receipt, provider time, and queue delay
separately from execution duration. Forwarding retains the original receipt timestamp.

SQLite stores tool bytes in ordered `tool_output_chunk` records keyed by exchange, turn,
call, and byte offset. An append validates the last committed offset and commits before
publication. Completion commits metadata and authoritative output in one transaction.
Loading hydrates the committed chunks. Opening the store transactionally moves existing
inline output into that table. A storage failure propagates through the broker's visible
failure boundary rather than publishing uncommitted text. A crash preserves committed
output, while lifecycle and metrics metadata retain their last committed barrier.

`ToolOutputView` retains the ANSI parser across deltas and indexes new line boundaries.
Collapsed previews render at most four wrapped output rows plus a hidden-row counter.
Full output remains available through the output document. One-second sync ticks use an
index of active entries and regenerate only timing headings, preserving message syntax
and tool bodies. Stable block identities and indexed fold endpoints constrain a patch to
the changed block and its owning fold metadata.

`forge.editable_shadow` stores the editable callback's generated-text baseline in indexed
64-row chunks. An ordinary splice replaces boundary chunks and inserted rows without
shifting the remaining transcript. Full restoration materializes the shadow only at the
recovery boundary. `ui.buffer.patch` records total buffer API time separately from
`editable_callback_ms` and its callback count, alongside preflight, metadata, folds, and
view restoration. `tests/forge/streaming_performance.lua` exercises 100 updates over
30,006 rows and reports p95, maximum callback time, and sequence visits.

Lua's `snapshot.lua` projects JSON nulls to absent Lua values before assigning Harness state.
Startup, session activation, and state reconciliation share this boundary. Nested optional fields
receive the same conversion without mutating the transport response or other protocols' null semantics.

The projector consumes `QuestionAnswered` as transition evidence instead of publishing a second feedback row because
the continuation interaction owns that visible user action. It also nests durable child-agent turns under their
provider and spawning-interaction identities before projection. Rust retains complete exchanges and
source output. `state.get` carries compact status and agent summaries, while `document_changed`
announces the current source revision without duplicating transcript content in Lua.

The session presentation owns a separate loaded document. Closed folds retain their heading and
an empty body anchor. A section request records expansion intent per view and publishes the union
needed by attached windows. An unloaded fold stays closed with a loading label until its first
page is installed. The same publication opens it, and a second toggle cancels that pending open.
Failed requests leave the fold closed and retryable. Closing the last requesting view releases its body. Nested closed
sections remain deferred. Expanded change files own their hunk bodies and start with two
window heights of display rows. Each continuation adds two current window heights when its
end boundary is within one screen below the viewport. File headings never request more pages.
A page also has a 64 KiB byte budget to bound unusually long lines, and larger hunks continue
in the next batch. Loaded rows remain stable across resizing, and each file admits one outstanding
request until its response is published. Other section bodies grow in 64 KiB pages without a
total loaded-body byte limit. Nested sections outside a file own independent page
budgets, so an expanded parent cannot prevent a child page from making progress. The viewport
admits at most eight section requests per pass and suspends prefetch while another buffer has
focus. A failed section reports its error once and stops automatic requests until the user
explicitly retries or closes and reopens it.
A failed request produces a visible notice and remains retryable. Section sequences reject delayed requests, and document identities reject
requests from a previous presentation lifetime. Neovim's native search covers loaded text.

The existing 67 ms publication interval applies to demand responses and streaming changes. Each
buffer commit installs text and metadata atomically. Internal events retain the 512 KiB frame limit
and use ordered JSON parts for larger payloads without an aggregate byte limit. Transport shutdown
or an incomplete multipart event reports an explicit delivery failure.

`Idle` remains a structural Rust phase but produces no status entry. `Working`, `RetryingPlanGeneration`,
`AwaitingInput`, `AwaitingPlanReview`, `PlanningFailed`, and `WaitingForAgent` occupy the final timeline position.
Rust derives the transient `Working · Ns` or `Planning · Ns` row from the retained Rust start timestamp during each document sync.
Lua supplies the one-second sync cadence but does not own the status text or persist elapsed time. Lua never
reopens a question after a continuation error. It renders the next Rust patch, which either contains a new
`AwaitingInput` owner or a terminal failure. The timeline and status debug logs record session id, base revision,
resulting revision, operation count, and rendered status, which makes transport divergence observable without
creating a second state owner.

`/fork` creates that tab before provider work finishes. The child receives only the source's last completed provider
turn. A fork during the source's first in-progress turn therefore starts with an empty provider thread, rather than
copying an unfinished prompt. `/new [name]` follows the same immediate-tab path without provider history. `/sessions`
focuses an already-open timeline, opens in the current tab by default, and toggles new-tab opening with `<Tab>`.
`:ForgeHarnessNew` also creates its provisional timeline before broker work. On a cold client it carries the new-session
request through initialization so the broker creates that exact session without first resuming or persisting another one.
On a warm client the process-wide session coordinator clones provider-neutral settings from the durable source without
entering its controller, so an active source turn or catalog lookup cannot delay the new timeline.
Controller registration clears one-shot initialization fields before opening that durable child and rejects any
resolved controller whose session id differs from the requested id. This keeps every later event, state request, and
question picker bound to the timeline that created it.

The Rust broker keeps one process-wide provider runtime and a serialized controller per Harness session. Codex lazily
launches one app-server with a loopback WebSocket listener, then gives each session an independent JSON-RPC connection
and provider thread through that host. Catalog discovery, `/new`, `/fork`, resumed sessions, prompts, configuration,
and compaction all reuse the same operating-system process. Copilot follows the same ownership rule through one SDK
`Client`, while Harness-keyed session slots retain independent SDK sessions and turn-local control lanes. Each slot
serializes only its own create or resume path, so one slow session bootstrap cannot block another timeline.

Codex owns its launcher and descendants through a Windows job object or a Unix process group. Windows assigns the
suspended launcher before resuming it and closes the job when the Forge host exits, including abrupt termination.
Shutdown and owner drop terminate the owned tree on Windows and the process group on Unix. Unix process groups do
not guarantee descendant cleanup after an uncatchable host kill. Provider stderr uses a private pipe with 4 KiB reads
and a 16 KiB retained tail for startup failures. It cannot retain the editor's host stderr pipe. Runtime teardown
aborts the stderr collector after initiating process termination.

Fork creation crosses two lifecycle boundaries. The broker first reads the source's last persisted provider session id
and completed-turn checkpoint, writes a child with `provider_fork_state = preparing`, preserves inherited context usage,
and returns its snapshot immediately. Provider preparation then runs outside the parent controller lock. Codex caches
the prepared WebSocket connection under the child id, while Copilot caches the prepared SDK session there. A prompt
submitted before preparation finishes waits on that child's gate only. Completion persists `ready` or `failed` before
publishing `session_fork_ready` or `session_fork_failed`, so unrelated timelines continue and failure remains explicit.
If the broker restarts with a child still marked `preparing`, provider-bound work reports the interrupted preparation
instead of guessing at provider history.

This split preserves independent cancellation, approvals, queues, and modes while preventing session creation from
multiplying provider hosts. The user-visible result combines immediate tabs with real concurrent turns and one stable
provider process per Harness broker.

---

## 3. Shared state (`session.lua`) and the `dr()` seam

Cross-cutting mutable state — the active status state, the per-buffer registry, and the
per-session diff caches — lives in **`session.lua`**, a store that `require`s nothing.
Because it has no dependencies, every layer imports it directly, with no cycle risk:

```lua
local session = require("forge.session")
session.status              -- the active status state
session.states[buf]         -- the per-buffer state registry
session.file_diffs          -- per-file diff cache (git layer writes, views read)
```

`session` is the **single explicit owner of shared session state**. The git layer
produces it, views and render consume it, and the status teardown resets it — all through
one table, instead of state hidden on the facade. Its fields are fully typed, so reads are
statically checked (unlike the `dr()` function seams below).

**The `dr()` seam** solves the second, narrower problem: reaching init-owned *functions*.
`init.lua` re-exports the package's functions under flat `_x` names, and it requires those
modules at load time, so they cannot `require` it back at the top level without a cycle.
The lazy back-reference resolves the fully-wired facade at call time instead:

```lua
local function dr() return require("forge") end
dr()._collect_items_from_git(cwd, cb)   -- a function re-exported on the facade
dr()._status_stage_entries(...)
```

Because `require` is memoized, `dr()` is a cheap table lookup after first load. `init.lua`
is now purely the **function re-export point**, not a state owner. Three wiring idioms
appear in it:

- **Module attach:** `M._git_data = require("forge.git.git_data")` — a submodule
  table parked on a field.
- **Function re-export loop:** `for name, fn in pairs(mod) do M["_" .. name] = fn end`
  — flattens a module's functions onto `M` under their old names so existing `dr()._x`
  callsites keep working after an extraction.
- **Selective re-export:** `M._setup_bg_highlights = status_helpers.setup_bg_highlights`
  — lifts one function onto the facade under its canonical name.

**The one rule that matters for correctness:** if a function can be overridden by a test
through `dr()._x` (a test seam), intra-module callers must invoke it through `dr()._x`,
not the module-local copy. Calling the local copy bypasses the override. This is the
single most common bug introduced when extracting code into a module.

A consequence for tooling: the re-export loops are invisible to static analysis, so
`lua-language-server` would flag hundreds of `undefined-field` reads on `dr()._x`. The
`ForgeModule` class in `init.lua` carries an `---@field [string] any` catch-all to
silence that architectural noise without losing typing on the explicit seams. See
`.rulesync/rules/forge.md` -> Linting.

---

## 4. Buffer types

The plugin renders into several distinct buffer kinds. Most user-facing views share the
**`ForgeStatus` filetype** and the same status-state machinery — they differ by a
`view_kind` discriminator (`"status" | "pr" | "review" | "diff"`) rather than by buffer
type. The standalone diff, file-revision, and preview buffers are separate.

| Buffer | Filetype | `view_kind` | Created by | Purpose |
|---|---|---|---|---|
| Main status | `ForgeStatus` | `status` | `views/commands.open` (`:ForgeStatus`) | Stage/unstage/discard, inline diffs, commit, push/pull, branch ops |
| PR overview | `ForgeStatus` | `pr` | `open_pr` | PR metadata, checks, review summaries, editable body, inline comments |
| PR review | `ForgeStatus` | `review` | `open_review` | Batched review: per-hunk comments, viewed-state, verdict, submission |
| Branch diff | `ForgeStatus` | `diff` | `open_branch_diff` (`:ForgeBranchDiff`) | Read-only working-tree-vs-branch diff |
| Single-file diff | (set by render) | — | `diff_buffer.open_diff_buffer` | One file's hunks with `S`/`U` hunk staging and folds |
| File revision | auto-detected | — | `file_revision.open` (`:ForgeFileRevision`) | A file as it existed at a revision, read-only, red winbar |
| Compact preview | `diff` | — | `open_compact_preview` (`:ForgeDiffCompactPreview`) | Raw compacted git diff |
| Commit message | (commit ft) | — | `commit.editor` (borrowed window) | The `git commit` message buffer, AI-prefilled |
| Commit console | (scratch) | — | `commit.commit` | Streamed pre-commit hook + git output |
| Harness interaction tree | `Harness` | — | `:ForgeHarness` | Prompt, thought, tool, diff, plan-progress, response, and elapsed-work nodes with stable native folds |
| Harness composer | `HarnessInput` | — | `:ForgeHarness` | Auto-growing multiline prompt input, active-turn steering, FIFO queue editing, and global prompt-history recall |
| Harness picker | `ForgePicker` | — | Harness selectors and requests | Bottom-anchored search, pages, two-column choices, attached input, and final review |
| Picker search | `ForgePickerSearch` | — | Session and other searchable selectors | Focused one-line fuzzy input with dynamically assigned result keys |
| Picker input | `ForgePickerInput` | — | `views/picker/input.lua` | Multiline feedback, Other answers, Ask prompts, custom models, and agent tasks |
| Plan review | `markdown` | — | `/plan <request>` | Physical editable plan, line annotations, accept or request changes |

Harness prompt windows use native `winfixheight` so closing another horizontal pane or
equalizing the layout leaves prompt height unchanged. Timeline windows explicitly clear
`winfixheight`, including when opened from a prompt window, and receive the available height.
Draft and queue changes still resize the prompt within its configured minimum and maximum.

**Why one filetype for four views.** Folding, keymaps, highlight groups, and the
hint-bar winbar are all keyed on the `ForgeStatus` filetype. Sharing it means the status,
PR, PR-review, and branch-diff buffers reuse the entire status rendering and interaction
stack. The `view_kind` field on the state selects which command set, head builders, and
sections apply, dispatched through the **view-controller registry** (`shared/view_controller.lua`).

### Shared Harness picker

`views/picker/` owns every compact Harness decision surface. Pure state and layout modules own
single or multiple selection, pages, responsive label/detail rows, and reserved input geometry.
The renderer applies one frame with extmarks, while the window owner anchors a focusable float to the
bottom of the union of the Harness transcript and composer windows.

The picker buffer owns modal navigation and globally hides its focused cursor through Neovim's
blend-100 TUI cursor contract. An optional `ForgePickerSearch` child owns fuzzy filtering,
while `ForgePickerInput` owns multiline feedback and returns to the picker through `go`.
`shared/input_gutter.lua` gives both picker input and HarnessInput the same two-cell window gutter
without inserting prompt text into either buffer. The picker captures the opening window, mode,
and cursor configuration once, then restores them after the complete lifecycle ends. An empty
cursor configuration restores as an equivalent explicit all-mode block cursor because Neovim
otherwise leaves the terminal in its last hidden TUI state. This global transition is intentional:
mapping only the picker window's `Cursor` highlight does not hide the focused terminal cursor.
Do not replace the paired `guicursor` transition with window-local highlighting, and do not restore
an empty cursor option directly after hiding it. Either change leaves cursor visibility stuck in the
terminal's previous state. Models, effort, service tier, artifacts, agents, interaction rollback,
approvals, lease conflicts, execution confirmations, and planning questions therefore share the
same geometry and focus contract without duplicating popup mechanics.

Configuration pickers never depend on the serialized provider-turn request lane to become visible.
Harness resolves and caches backend model metadata when the view activates, then `/model` opens from
that presentation cache even while a provider turn runs. Model, effort, service-tier, backend, and
execution-mode selections continue through their normal mutation owners. Configuration and backend
changes queue for the next safe boundary, while execution mode retains its active-turn restart
contract.

`/fast` and `/ultrafast` select mutually exclusive service tiers. Repeating the selected command
returns to standard processing. The session, workspace preference, fork, and backend request share
one typed `service_tier` value. Completion and configuration admission use backend capability flags.
Codex advertises both tiers and Copilot advertises neither. Codex receives the selected tier on thread
creation, resume, explicit turns, and Harness-driven continuations. Disabling acceleration sends
`default` explicitly because a null tier can inherit the thread's previous selection. Provider errors
retain their normal failure path when a model or account cannot use the requested tier.

`views/harness/provider_catalog.lua` owns per-session caches for backend skills and MCP definitions.
`/skills` toggles future skill eligibility and inserts enabled `$skill` selectors, while the completion
source advertises only enabled user-invocable skills at the first non-whitespace token.

`/mcp` renders each complete provider definition with transport, token attribution, lifecycle state,
and its cached tools. Copilot applies MCP changes to its live SDK session. Codex interrupts an active
turn, writes effective app-server config, confirms refreshed status, then resumes that interaction.

The `/undo` specialization loads the active session's durable interaction records and lists
checkpointed complete, failed, or cancelled interactions newest first. Selecting a record opens a
second picker with `Cancel` selected before the destructive choice. A successful broker rollback
restores the selected interaction's exact multiline prompt to `HarnessInput`, moves the cursor to
its end, and enters Insert mode so the reverted request can be edited or resubmitted.

The `/sessions` specialization defaults to the current repository and toggles all repositories with
`C-o`. Moving the result selection requests a read-only `session.preview` projection and temporarily
swaps only the Harness transcript window to a scratch timeline rendered by the regular Harness tree.
The active transcript buffer keeps receiving background renders off-screen and returns unchanged
when the picker closes. `C-j` deletes the selected inactive session, while Enter resumes a compatible
same-repository session. Preview requests never acquire or alter a durable session lease.

### User command surface

Registered in `nvim/lua/plugins/forge.lua`:

```vim
:ForgeStatus                            " forge.open()
:ForgeBranchDiff <branch>               " open_branch_diff(branch)
:ForgeBranchDiffFile <file> <branch>    " open_branch_diff(branch, { file = file })
:ForgeFileRevision <file> <commit>      " open_file_revision(file, commit)
:ForgeDiffCompactPreview[!]             " open_compact_preview({ staged = bang })
:ForgeHarness                              " open_harness()
:ForgeHarnessNew                           " new_harness_session()
```

Inside Harness, `/sessions` opens durable session search and `/undo` opens checkpoint rollback.

PR and review buffers are opened programmatically by the `github` integration
(`nvim/lua/github/open_pr.lua`, pickers) through `open_pr` / `open_review`, not by a
direct user command.

---

## 5. The state model

Each view owns a **`ForgeStatusState`** table. The same shape backs status, PR,
review, and branch-diff buffers — `view_kind` is the only structural discriminator.
Core fields:

- `buf`, `cwd`, `view_kind` — identity.
- `lines` — the rendered text lines.
- `sections` — the ordered top-level sections (unstaged, staged, unviewed, ...).
- `entries` — per-row metadata: which file/hunk/diff-line/kind a buffer row maps to.
- `folds` — `{ key → folded }` over the stable keys from `status_keys.lua`.
- `highlights`, `extmarks` — accumulated decoration, applied after the lines are written.
- view-specific state — PR data, review comments, walkthrough mode, diff source handles.

The Neovim-local Harness presentation state lives under `session.harness`. It holds
buffer/window handles, pending steering indicators, the local FIFO prompt queue, process generation, capability flags,
and a presentation copy of the current interaction list. Durable sessions, plans, goals,
interaction timelines, and checkpoints never live in Lua. The Rust broker owns those
records, then reconstructs Lua presentation state after restart. Raw transcript events no
longer form part of the storage or rendering model.

The active-state pointers live in **`session.lua`** (Section 3):

- `session.status` — the **active** state (the one the current buffer renders into).
- `session.states` — the registry `{ [buf] → state }` so multiple status-like buffers
  coexist.
- `session.main_status` — the primary `:ForgeStatus` buffer.

**The autocmd state machine (`views/status/state.lua`).** A single `session.status`
pointer is convenient but dangerous when several status buffers are open.
`attach_status_state(buf, state)` registers the state in `session.states[buf]` and installs
a `BufEnter/BufWinEnter/CursorMoved/ModeChanged/BufWipeout` autocmd group that **swaps
`session.status = session.states[buf]` whenever the buffer becomes current.** This way
render, navigation, and action code can read a single `session.status` and always get the
right buffer's state. On teardown it calls `diff_buffer.cleanup_diff_buffers()` — the diff
buffer owns its per-buffer caches (`_buf_hunks`, saved cursors, the `diff://` registry) and
clears them itself.

---

## 6. The render engine (`render/`)

This is the most intricate subsystem and the one most worth understanding deeply. It is
**view-agnostic**: it knows nothing about status buffers or PRs. It turns *diff text +
source files* into *buffer rows + extmarks*. Most of it is pure functions, with a single
async island for tree-sitter.

### 6.1 The data model: sources and files (`source.lua`)

Everything hangs off a **`ForgeDiffSourceRegistry`**. A **source** represents one
diff context — working-tree unstaged, working-tree staged, a commit, a PR, a review, a
branch, or a walkthrough — keyed by `id` and `kind`. Each source owns a
**`ForgeDiffFileState`** per file, which holds:

- `hunks[]` and `hunk_index_by_id` — the parsed hunks plus their lazy-chunk index.
- `old_text` / `new_text` — **text snapshots** (Section 6.7), loaded lazily.
- `text_loader` — a per-side async callback that fetches the file body only when needed.
- `syntax_context` — the file's tree-sitter parse state (Section 6.5).
- `annotations[]` — review comments anchored to this file.
- `layout` / `body_layout` — the row-tree mapping used for navigation.
- staleness flags for invalidation.

**Lazy loading is the core performance idea.** Opening a status with 50 changed files
must not parse 50 files. `set_text_loader` registers a fetcher, `ensure_text(file, side,
done)` runs it only on first demand, and `invalidate_paths` / `reload_paths` re-diff only
what actually changed. `source_loader.lua` is the thin convenience facade
(`ensure` → `ensure_file` → `replace_file_hunks`) so callers do not thread three handles
around.

Whole-file additions and deletions add a harder data boundary. Startup retains their porcelain
metadata and line counts without constructing full patches. Walkthrough mode hydrates an expanded
added file automatically, while ordinary status interaction hydrates it on explicit expansion.
When either side exceeds `status_file_preview_line_limit` (default 1,000), the file entry renders
`Diff omitted — file has N lines (limit 1000)` and never constructs hunks, builds syntax, or
prewarms. Deleted files also avoid reading their source blob after the path-scoped numstat query
crosses the limit. Smaller staged deletions read the porcelain HEAD object ID, while smaller
unstaged deletions read the porcelain index object ID through `git cat-file blob`.

Cursor-driven syntax prewarm applies a separate hard boundary before it constructs file-level
highlight state. Deleted files never prewarm. A collapsed non-deleted file prewarms only when its
known `added + removed` delta stays below 100 lines, including new files whose delta comes entirely
from additions. Files at or above 100 lines, or files without known stats, must expand before
file-level prewarm runs. Hunk rows already imply an expanded file and follow the same deleted-file
exclusion.

### 6.2 Parsing (`diff_parse.lua`)

A pure state machine over unified-diff text. It produces `ForgeParsedBlock[]` →
hunks → `ForgeParsedHunkLine[]`, tracking old/new line counters per `-`/`+`/` `
prefix and computing gutter widths from the maximum line number. No state, no buffers.

### 6.3 The hunk model (`hunk_model.lua`)

Pure logic that turns parsed hunks into **renderable regions**. The central concept is
the **`ForgeHunkChangeRegion`**: a contiguous run of changed lines plus the
tree-sitter context scope it belongs to. `change_regions(...)` segments render items into
regions and **merges adjacent regions that share a context scope**, so a function with
three edits renders as one labeled block rather than three. It also computes context
padding (the unchanged lines shown around a change), boundary markers (the scope's
opening/closing lines), and coordinate maps (`old_line_for_new_line`) for navigation.

### 6.4 Intraline diffing (`intraline_diff.lua`)

When a `-`/`+` pair is *almost* the same line, showing both is noise. `compact_pair`
detects a shared prefix and suffix (≥3 common chars, length ≤400, one side a small
edit) and collapses the pair into a single **replacement row** with `inline_spans`
marking the changed bytes. `compact_hunk_lines` walks a hunk body, groups consecutive
deletions then additions (≤8 per group), and emits replacement items. The result reads
like a word-level diff inside one line.

### 6.5 Tree-sitter syntax (`syntax_engine.lua` + `syntax_context.lua`)

This is the only async part of the engine. Diff bodies are not real buffers, so they
have no syntax. To color them, the engine parses the underlying file (or the diff's
synthetic body) with tree-sitter off the main path and caches the result.

`syntax_engine.lua` owns four caches as **module-local tables** (private to the render
engine) that survive across renders:

- `ts_source_bufs` — transient, hidden scratch buffers used to host a parse.
- `ts_syntax_cache` — per-file syntax keyed by filename.
- `ts_diff_syntax_cache` — per-diff-side syntax keyed by `filename:side:sha256(diff)`.
- `ts_context_cache` — per-line scope context (the "in function `foo`" label).

The three callers outside the engine (the git refresh, the row builder, the debug dump)
reach these only through a narrow read/clear API — `clear_context_cache()`,
`context_cache_entry(key)`, `file_syntax_cache_entry(filename)` — never the raw tables, so
the caches stay encapsulated.

The async pattern is uniform: on a cache miss the caller schedules
`dr().compute_*_async(...)`, gets back a *pending* marker, and re-renders when the
callback fills the cache. `prewarm_diff_syntax` preloads syntax for hunks near the
cursor under a budget, so scrolling feels instant without parsing everything up front.
The viewport prewarmer only schedules syntax for rendered hunk rows. Collapsed file rows
stay cold until the cursor rests on them, preventing status open from parsing every
changed file at once.

`syntax_context.lua` is the per-file holder: for each side it stores the source
snapshot, parser, parsed tree, and highlight query, and resolves
`highlights(side, first_row, last_row)` into per-row `(col_start, col_end, hl_group)`
spans. It **drops the cached tree whenever the snapshot changes**, so highlights never
paint against stale text.

### 6.6 Row building and buffer application (`diff_render.lua`)

The orchestrator that ties parsing, the hunk model, intraline compaction, and syntax
together. `build_fancy_diff_rows(diff_text, ...)` produces an array of **rows**, where
each row is a list of chunks — either `{ text, hl_group }` or a virtual-text gutter
chunk. For each change region it emits boundary rows (scope start/end), context-padding
rows, and body rows with their syntax segments and inline spans.

`render_highlight_rows(buf, rows, ns)` then **flattens rows into buffer lines and
extmarks**: it clears the namespace, sets the lines, and adds highlight, line-background,
and virtual-text extmarks at fixed priorities.

**Why the diff background uses `hl_eol`, not padding.** The Forge windows enable soft
word wrap (`wrap` + `linebreak`, set in `window_options.apply`), so a long diff line wraps
instead of running off-screen. `breakindent` stays off on purpose — its wrapped-continuation
indent is virtual whitespace no character-range highlight can paint, so an indented
continuation would show an unpainted notch under the gutter on `+`/`-` rows. The `+`/`-` line background therefore fills to the
window edge with `hl_eol` on a char-range span at priority 60 — *below* the inline word-diff
highlights — instead of padding the buffer line with trailing spaces. Padding (the old
`_diff_pad_highlighted_line` to ~160 cols) would spill a blank highlighted tail onto every
wrapped continuation row and leak into yanks, whereas `hl_eol` keeps the band full-width on each
display row while the buffer line stays pure code. Emitted in `status_render`'s ephemeral
decoration provider (status views) and `diff_render`'s extmark pass (standalone diff buffers).

**Why the gutter is virtual text, not buffer content.** Line numbers and the `+`/`-`
sign live as *inline virtual text*, not as characters in the buffer. So a visual
selection, yank, search, or `gd` operates on the real code only — the gutter never
pollutes the register or a search hit. Cursor-normalization helpers in `diff_buffer.lua`
keep the cursor from stepping into that virtual gutter.

### 6.7 Supporting structures

- **`text_snapshot.lua`** — an immutable snapshot of a file body with byte-indexed line
  spans. `line_text(n)` slices a substring instead of holding a Lua table of every line,
  which keeps large files cheap.
- **`hunk_index.lua`** — splits a hunk into sections and body rows so a huge hunk can be
  rendered in **chunks on demand** rather than all at once (the lazy-render path the size
  gate triggers).
- **`layout.lua`** — a **Fenwick / binary-indexed tree** of cumulative row heights.
  `item_at_row(row)` answers "which item owns this buffer row" in O(log n), which is what
  makes navigation and re-render over thousands of rows fast.
- **`row_tree.lua`** — the logical tree of nodes (hunks, padding, annotations) layered on
  top of `layout`, keeping each node's row span in sync as content changes.
- **`region.lua`** — a buffer range anchored by two extmarks with dirty tracking, used
  for editable comment regions that must survive surrounding edits.
- **`comment_box.lua`** — builds every compact inline PR, submitted-review,
  batched-review, and walkthrough comment through one segmented box primitive.
  `views/status/comment_box_rows.lua` emits those segments as real status-buffer rows, which
  lets normal cursor movement enter every box. Walkthrough annotations use the same rows
  with readonly descriptors instead of a separate virtual-line transport.
  `section_builder.emit_anchored_comments` dispatches the cursor-selected occurrence to
  full-width editable rows only while its stable diff-entry ID owns focus, so duplicate
  appearances under Reviews and Changes cannot both become editable. The primitive wraps
  by display cells, splits unbroken text, preserves same-anchor order, and requests a
  status re-render on resize. Save preserves focus. The shared review cursor lifecycle
  clears focus and restores the compact rows only after the cursor leaves the selected
  comment's header, body, replies, and footer.
- **`annotations.lua`** — the review-comment model: a by-anchor index, a per-comment sync
  state machine (`new`/`dirty`/`clean`/`deleted`/`conflict`), and a **serial sync queue**
  that drains dirty comments to the remote one at a time, rejecting stale completions by
  operation id.
- **`decoration.lua`** — a `nvim_set_decoration_provider` cache for **ephemeral** per-row
  highlights computed at draw time (the alternative to baking every highlight into static
  extmarks — see the repo-root `architecture.md` for the full design).

### 6.8 The pipeline, end to end

```
diff text ─► diff_parse ─► hunk_model.change_regions ─┐
                              intraline_diff.compact ──┤
file body ─► text_snapshot ─► syntax_context ◄─ syntax_engine (async, cached)
                                                       │
                                                       ▼
                                          diff_render.build_fancy_diff_rows
                                                       │  rows = [{text,hl}|{virt_text}]
                                                       ▼
                                          diff_render.render_highlight_rows
                                                       │  lines + extmarks
                                                       ▼
                                          buffer  +  layout / row_tree (navigation)
```

---

## 7. The status view (`views/status/`)

`:ForgeStatus` is assembled from single-responsibility modules. The flow:

**1. Collect.** `views/commands.open` creates or reuses the `ForgeStatus` buffer, attaches
a state through `state.attach_status_state`, and invokes the `status_snapshot.lua`
collector through `git_data.lua`. One snapshot runs exactly five commands in parallel:

- porcelain-v2 status with NUL records and all untracked files
- zero-context unstaged and staged diffs filtered to modified, renamed, and copied paths
- unstaged and staged NUL-delimited numstat queries filtered to added paths

A full load passes an empty path list to cover the repository. Mutation verification
passes only the affected root-relative paths through the same seam. The collector parses
the five outputs into canonical sections, source records, compact line metadata, and
staged/unstaged diff caches without starting a second Git reload. Added and deleted bodies remain
unloaded. Untracked files pass through a 16-read asynchronous libuv pool after the Git fan-in only
to classify binary content and count lines, without retaining synthetic patches.

Collection stays pure with respect to `session.file_diffs`, `session.file_hunk_staged`,
and `session.untracked`. `render_orchestrator.lua` adopts a full snapshot only after the
request ID, buffer validity, and pending-mutation gates accept that load. A pre-action
full load can therefore finish late without overwriting the optimistic cache projection.

**2. Build sections.** `section_builder.lua` turns diff text into the section/file/hunk
tree. `section_map.lua` owns pure projection, path replacement, and semantic equivalence.
`operation_journal.lua` holds one confirmed section baseline plus ordered optimistic
layers, which lets a new stage or unstage project over synchronization already in flight.
`ignored_path_store.lua` then projects repository-relative markers over that Git model,
moving matching Unstaged files into a folded **Ignored changes** section. It persists one
versioned JSON document per normalized worktree root under Neovim's data directory, so
branch switches share markers while unrelated worktrees remain isolated. Startup loads
the Git snapshot and marker document concurrently and renders only after both complete,
which prevents an Unstaged-to-Ignored flash without serializing the two reads.
`status_keys.lua` assigns each section/file/hunk a **stable identity key** so fold state,
caches, and actions all index the same canonical key across renders.

**3. Render.** `status_render.lua` runs the full pass: `status_head.lua` builds the
head/about lines, the sections render their files and hunks, and `status_buffer.lua`
accumulates lines, highlights, extmarks, and folds. Buffer text reconciliation asks
`vim.diff` for histogram indices, then applies disjoint edits from bottom to top so an
unchanged prefix never gets rewritten. Extmarks and the decoration provider complete the
pass. `render_orchestrator.lua` wraps the async git-root load and PR-specific render
passes.

**4. Fold and gate.** `fold_state.lua` owns the per-key fold map, native fold ranges,
foldtext, materialized-entry state, and resize refresh. Initially collapsed files omit
their bodies. The first expansion materializes the file and hunk rows once, after which
collapse and expansion use native folds without rebuilding the status buffer. `size_gate.lua`
estimates how many rows a file's hunks and comments will occupy and **defers the body
render of files over budget**, so opening a status with a 20,000-line diff stays responsive.

**5. Bridge to the engine.** `diff_source_state.lua` is the seam between status entries
and the render engine: it owns the per-file diff-source registry, commit source handles,
git text loaders, and the layout build that `diff_render` consumes.

**6. Navigate and act.** `entry_nav.lua` resolves the entry under the cursor, parent/file/
hunk relationships, visual-selection entry sets, action targets, and decoration prewarm.
Before dispatch, it expands selected sections into files and removes hunk targets already
covered by a selected file in the same status section. This non-overlapping action set
prevents one visual range from mutating a whole file and then mutating its former hunks.
Visual line mutations capture the next surviving semantic sibling before changing the
component model, falling back to the previous sibling when the selection reaches the
end. The originating action render consumes that target once. Normal-mode actions,
other open buffers, deferred Git synchronization, recovery, and enrichment renders
never receive it, which prevents delayed work from steering the user's cursor.
For stage and unstage, `actions.lua` immediately appends an optimistic journal layer,
projects the section and diff caches, and renders that projection. It then submits the
Git index mutation to `mutation_coordinator.lua`, whose repository-root FIFO prevents
`.git/index.lock` races across every status and diff buffer for that repository.
Commit admission resolves the repository root and waits asynchronously while that FIFO
contains an active, queued, settling, or recovering mutation burst. The coordinator
resumes admission once all bursts finish, including their authoritative snapshots.
A mutation or verification failure cancels the waiting commit with an error notification.
Once the commit
session becomes active, `session.suspend_preview` rejects new stage and unstage actions.
The admission reservation spans root lookup and queue settlement through active-session installation, and the
active session remains exclusive through process exit, so repeated commit keys cannot
start concurrent Git writers or editor clients.

Commit diagnostics retain one request ID from the user action through native write
preparation. Successful preparation returns microsecond timings for repository
opening, admission, write-queue waiting, read-worker dispatch, and individual
HEAD/index/source capture and recheck stages. Lua records these as
`git.write.prepare.native.*` events in `forge/diff-perf.log`, converted to
milliseconds. `capture_total` includes its individual capture stages and must not
be added to them. Failed preparation retains the existing overall duration and
error result without reporting incomplete stage timings as successful work.

The Ignored section never enters the Git journal. `I` moves whole Unstaged files into
the virtual marker set, while `U` removes those markers without invoking Git. `S`
temporarily suppresses selected markers and submits ordinary whole-file stage targets,
so the existing optimistic journal moves them directly to Staged. Successful targets
delete their markers after Git completes. Failed or cancelled targets retain their
markers and reappear through the normal recovery snapshot, including partial batches.
AI commit generation loads the same effective marker set and adds Git exclude pathspecs
to both its context diff and fingerprint diff. Ignored-only edits therefore neither
influence the generated message nor invalidate an otherwise reusable message.

After the FIFO drains, a **120 ms quiet window** closes the burst and `status_sync.lua`
runs one path-scoped five-command snapshot for the union of affected paths. A matching
snapshot retires the resolved journal layers without rendering the status buffer or
writing an open diff buffer. A real mismatch replaces those paths from Git truth and
performs one corrective render. Later optimistic layers replay over the new confirmed
baseline, so verification never erases actions accepted during an earlier synchronization.
If the authoritative read itself fails, synchronization retries that five-command
attempt once after 120 ms before marking the projection stale. The normal path still
runs one attempt, and a successful retry does not add a render when truth matches.

Git-generated tracked hunk patches already express the canonical diff, including when
the worktree uses CRLF or an idempotent clean filter, so stage and reverse-unstage leave
worktree bytes untouched and normally match the projection. Whole-file `git add` for an
untracked file crosses a different boundary. Git may normalize EOLs or run a clean filter
while writing the index, which can make the authoritative staged diff differ from the raw
synthetic untracked patch. That case receives the same single correction as a hunk split
or merge. It does not count as a mutation failure.

The first failed mutation cancels the queued tasks from that burst and notifies
immediately. Successful Git writes that completed before the failure remain intact. One
path snapshot then rebuilds actual Git truth and forces one recovery render. Stage and
unstage only restore the one target captured by an originating visual bulk action during
its optimistic render. Normal actions, corrective renders, and recovery renders never
move the cursor. If both bounded snapshot attempts fail, recovery marks verification stale,
reverses the failed and cancelled projections, and retains only targets Git already
reported complete. Discard follows its separate destructive flow and retains an explicit
target.

**Supporting:** `commit_view.lua` (commit editor, About view, create-PR/verdict/help
popups, push/pull), `status_issues.lua` (`#`-issue completion), `status_debug.lua`
(dev-only diagnostics), `window_options.lua` (window option overrides with exact
restore), `pr_state.lua` (async PR + AI-about lookup with request-id race guards).

---

## 8. PR, review, and walkthrough views

### PR overview (`views/pr/pr_overview.lua`)

Renders a PR into a `ForgeStatus` buffer (`view_kind = "pr"`): the metadata header,
foldable checks section (with status icons), submitted-review summaries
(approved/changes-requested/commented), regular issue comments laid out by
`github.comment_rows`, and inline review comments dispatched through the shared anchored
comment-box renderer. Expanding a submitted review reuses the same box path as Changes.
Entering a compact box row promotes only that occurrence to editable buffer rows. `C`
creates a new comment instead of selecting an existing one. Viewer-authored snapshots resolve through the PR-owned editable
store before either render mode. GitHub update responses preserve replies they do not
include, while deletion removes the identity from the editable store, flattened code
comments, and nested submitted-review comments before re-rendering. It loads comments and
checks asynchronously through `gh`. Existing replies render as internal divided blocks
inside one outer comment box. In this PR surface only, `R` stays bound across compact and
focused remote comment modes and creates a PR-owned inline reply draft below the selected
thread. `section_builder.emit_anchored_comments` expands that occurrence through the same
full-width comment renderer while keeping the parent and posted replies readonly. The reply
body alone receives an editable extmark region. `<C-s>` posts it through GitHub's
review-comment reply endpoint, `J` discards it, and leaving the body collapses a nonempty
draft back into the merged compact thread. The same key falls back to PR refresh away from
an inline comment.

The status row models the PR lifecycle as `DRAFT`, `OPEN`, or `CLOSED`.
Activating it opens `infra.choice_popup` with exactly the other two states.
`pr_edit` sends the chosen transition through `github.pull_request` to Rust.
The host reads remote truth and retains one resource owner across close,
reopen, ready, or convert-to-draft GraphQL mutations. A proven second-step
rejection returns the confirmed intermediate state. An uncertain result
triggers a read-only reconciliation and failure notification without
automatically repeating the mutation.

ForgeStatus resolves `ogp` through a branch-wide PR list. `pr_state` sorts the
results by descending PR number, selects the newest active PR across ready and
draft states, and opens it directly. Without an active PR, it retains the
newest closed candidate and uses the shared chooser to open that PR or start a
new draft PR. Merged PRs never become closed fallbacks.

Top-level issue comments under `Comments` remain a separate foldable-list flow. They
start as one metadata/preview row and cursor movement never opens them. The status-family
open command (`o`, `<CR>`, or `.` by default) expands the selected comment, while
`<Tab>` collapses it. Expanded comments retain their fold state when the cursor leaves.
Viewer-authored bodies become editable through their rendered body regions without
entering the inline annotation focus lifecycle.

### PR edit (`views/pr/pr_edit.lua`)

Makes the PR title, description, reviewers, and milestone **editable in place**. It tracks
each field with extmark markers, queues GitHub mutations as the user types, and scopes
`render-markdown.nvim` to the description region only (by sandboxing the markdown
tree-sitter parser to that range) so the title and reviewer rows are not re-styled.

### PR review (`views/pr/review.lua`)

The batched review mode (`view_kind = "review"`). It loads a local draft from disk and
the remote pending review from GitHub, **merges them with conflict detection**, and lets
the reviewer comment per hunk, mark files viewed/unviewed, pick a verdict, and submit. A
review loads GitHub's net PR diff rather than its per-commit patch series, so changes
introduced and reverted within the PR never appear as commentable rows. A
comment carries `body` (current), `base_body` (last synced), and `remote_body` (on
conflict) plus a `local_state` machine. Mutations flow through the `annotations` serial
sync queue, and the draft is persisted to the `github.repo_cache` so a closed buffer
never loses pending comments. Unfocused comments use the shared compact-row renderer.
Cursor entry promotes the selected box to full-width editable rows and tracks the selected
diff entry by stable ID so re-rendering can collapse a previous editor without losing
focus. `C` creates a new comment from a changed line. `J` resolves the exact comment row
under the cursor, while `y`/`z` navigate across both representations.

### Reviewer source (`views/pr/reviewer_source.lua`)

A `blink.cmp` completion source that autocompletes `@mentions` from the repo's cached
contributors, triggered on `@` in any PR/review text field.

### Walkthrough (`walkthrough.lua` and `forge-review`)

`ow` opens a read-only native `ForgeWalkthrough` document. The Rust service resolves the
repository root, reads and validates `.walkthrough.json` through its Rust schema, and renders
the flow, task tree, annotations, and native fold metadata. Lua owns visible windows and input
capture, while Rust owns artifact state, target identity, inventory, and source resolution.

Each visible walkthrough window registers a `DocumentView`. The first view profile controls
wrapping until that view closes, which prevents a second split from changing another window's
projection. Closing the public document releases every view before collecting the Rust document.
Opening a change creates one composite review beside the outline. Rust projects the nearest
changed hunk with source coordinates, syntax colors, and intraline emphasis, followed by the
bordered annotation. Each excerpt admits at most 256 diff rows and 128 KiB of diff text. When the
anchor falls outside that delivery, the review shows seven captured source rows around it instead.
Stale artifacts and unchanged files also use the captured-source excerpt. The retained review
including annotation text and decorations must fit 16 MiB.

`o` on a source target opens the exact captured source in the review window. Rust validates the
review revision, view, input sequence, and source coordinate before Lua switches buffers. `q`
returns from source to review, then closes the review and returns focus to the outline. Closing
an owned window releases its review and source. Separate reviews retain separate lifecycles even
when they reference identical bytes. The main document admits at most eight reviews. Each review
owns its Markdown annotation and `DocumentViews` width profile. Resize and width-owner transfer
reflow the annotation without reconstructing source rows. Closing the outline collects every
remaining review and source.

The artifact commit identifies its HEAD baseline. For a current artifact, Add and Modify
annotations on paths changed when the walkthrough opens resolve bounded, immutable worktree
content through GitService. Acquisition must match the captured worktree stamp. Later source
drift rejects navigation until the walkthrough reopens. Remove annotations and paths unchanged
at opening resolve the baseline blob. An artifact whose commit differs from HEAD always resolves
its recorded commit and displays an explicit stale-source warning. Every source and annotation
remains read-only, including fresh worktree snapshots.

The `walkthrough_inventory` option accepts `"sem"` or `false`. The Sem path runs one tracked
diff request and one batched untracked entity request. Missing Sem, command failures, invalid
JSON, and unsupported file types produce an unavailable inventory with the Sem diagnostic and no
fallback provider. Lua reports that diagnostic through the walkthrough error notifier. `false`
omits inventory collection and its document projection.

---

## 9. The Git data layer (`git/`)

### `git_backend.lua`

A **pluggable async process runner**. By default it spawns through `vim.system`. Tests
inject a `ForgeGitBackend` via `set_backend(...)` so they run without touching a real
repo. It exposes async primitives (`system_text_async`, `system_text_stream_async`,
`systemlist_async`), git-root discovery (`git_root_async` from the process directory and
`git_root_at_async` from an explicit working directory), and high-level runners
(`run_git_async` that resolves the root first).
The streaming variant feeds an `on_line` callback for progress, then a final callback on
exit — this is how large diffs and long-running commands report incrementally.

### `git_data.lua`

The diff-specific layer on top of the backend, and the busiest data module:

- **Parse:** `parse_diff(output, staged)` → hunks with positions, context, counts.
  `order_file_hunks` sorts hunks so staging/unstaging folds in place.
- **Integrate snapshots:** `collect_status_snapshot_async(cwd, cb)` returns the typed canonical
  snapshot without changing session caches. `section_map.sections_from_snapshot` drives both first
  paint and reconciliation, while the accepted status orchestrator owns full-cache adoption.
- **Materialize deferred bodies:** `file_body.lua` synthesizes canonical full-file hunks only after
  explicit expansion. Added files come from worktree bytes or the porcelain index object ID.
  Deleted files use numstat before reading the HEAD or index blob.
- **Compute syntax (async):** `compute_file_syntax_async` / `compute_diff_syntax_async` /
  `compute_hunk_context_async` create scratch buffers, parse with tree-sitter, and return
  `{ buf, tree, highlight_query }` — the producers behind the `syntax_engine` caches.

### Status snapshots and index mutations

`status_snapshot.lua` owns the authoritative read seam. Each invocation starts exactly
one porcelain-v2 status command, modified/renamed/copied zero-context diffs for unstaged and staged
state, and added-file numstat queries for unstaged and staged state. A nonempty path list appends one
shared pathspec to all five commands. An
empty list produces the full status snapshot used for first load and explicit refresh.
After those commands finish, bounded asynchronous filesystem reads collect compact untracked-file
metadata without another Git process. The collector preserves porcelain modes and object IDs so
later body reads avoid path-revision ambiguity. Added and deleted paths never request unified Git
diffs. Lazy synthesized patches retain binary, CRLF, and missing-final-newline behavior.

Rename and copy origins drive different mutation scopes. A rename includes both paths
because the source disappears. A copy mutates and replaces only the destination because
the source remains an independent tracked path. The section model retains that kind so
unstaging a copy cannot unstage unrelated source changes.

`index_mutation.lua` translates one semantic hunk, tracked-file, untracked-file, or
added-file action into ordered Git commands. It stops at the first failure and reports
the completed targets, which preserves partial success instead of pretending an entire
batch rolled back.

`mutation_coordinator.lua` serializes index writes per repository root, groups accepted
tasks into quiet-window bursts, and delegates successful settle or failed recovery to
`views/status/status_sync.lua`. Different repositories retain independent queues.

### `repo_config.lua`

Reads `<repo root>/.forge.json` (currently just `branch_prefix`, used by the `bc`
branch-create action) behind a reader seam so tests never hit the filesystem.

---

## 10. Integrations (`integrations/`)

- **`gh.lua`** — the GitHub bridge (CLI by default, injectable backend for tests). Builds
  GraphQL/REST queries and parses responses for PR details, checks, review comments, and
  review submission. Consumed by the PR/review views.
- **`ai_commit.lua`** — starts one approximate About draft when Status opens,
  describing net HEAD changes from staged, unstaged, and untracked files, excluding
  Git ignores and the Status Ignore category. Rust enumerates changes once and
  builds bounded diff context without comparison fingerprints or before/after
  repository snapshots. Status retains the HEAD object and nonignored file
  generations used for that draft. On refocus, Status refreshes its native
  snapshot and regenerates once when those sources changed. A clean snapshot
  clears About. A successful commit clears the displayed draft and shared
  commit-editor cache before returning to Status. Failed and aborted commits
  retain the draft. File acquisition retains its local read-safety checks.
  The commit editor reuses the ready or pending About draft without inspecting
  repository state or automatically generating a replacement. Opening without an
  About draft leaves the message unchanged. Ctrl-A in normal or insert mode
  explicitly generates from staged files, including staged paths in Status Ignore,
  because those paths will be committed. The editor winbar advertises regeneration.
  Regeneration preserves Git comments and existing text while waiting. Buffer
  edits, newer regeneration requests, submission, and closure prevent late results
  from overwriting newer editor state. Provider failures preserve the message.
  Each explicit regeneration calls the provider without a message-cache check.
- **`commit.lua`** — the **fake-editor commit bridge**. Because `git commit` needs an editor,
  this spawns a headless `nvim --clean` as `GIT_EDITOR`, which connects back to the parent
  over RPC (`$NVIM`) and asks it to open the real `COMMIT_EDITMSG` in a borrowed
  diff-preview window. `<C-c><C-c>` commits, `<C-q>` aborts, and pre-commit hook output
  streams into a console buffer. The parent retains the exit-notification RPC channel
  until the helper closes it, preventing queued exit messages from being discarded. Commit admission remains exclusive from repository-root
  lookup through process exit and rejects startup while the repository mutation
  coordinator is pending. This lets the running editor provide AI prefill and live hook
  output without overlapping Git index writers.
  Native commit commands drain stdout and stderr through process exit. Each stream
  publishes at most 64 KiB, including a truncation notice, and retains a bounded tail
  for failure diagnostics. Output truncation never changes Git's exit status or
  terminates a commit. Commands whose output is parsed retain strict capture limits.
  The 2 KiB mutation diagnostic preserves its prefix and tail at UTF-8 boundaries.
  Lua anchors pending-line matching so newline-free progress does not trigger a
  quadratic pattern search.
  With `diff_logging` enabled, `commit.*` and `git.write.*` events share a request ID
  and record root lookup, native preparation/submission, first stream output,
  editor opening, message-writing time, editor signalling, receipt, cleanup, and
  preview restoration. `commit.finished.ms` measures receipt latency after editor
  submission, excluding time spent editing the message. Native submission includes
  Git hooks and receipt settlement, not only the Git process. Separate
  `status.refresh.*` events identify the Status document and measure request,
  recovery snapshot, and patch application through refresh completion. These events
  use the bounded asynchronous `forge/diff-perf.log` writer and contain timing and
  identity metadata, never commit messages, paths, or command output.
  The native mutation coordinator retains the latest 1,024 metadata-only lifecycle
  records. During a logged Git write, Lua polls `repository.write` with the `trace`
  operation every 250 ms, with one request in flight and at most 128 records per
  response. It also drains completion and acknowledgement records. A host identity
  and monotonic sequence distinguish restarts and report overwritten records.
  `git.write.native.blocked` identifies the waiting operation and its blocker,
  including the blocker's operation type and phase. Other native records separate
  preparation, execution, affected-path settlement, consumer settlement, completion,
  and acknowledgement. Each carries total age, elapsed time in the preceding phase,
  selected file count, and actual Git command count. Queue ownership still extends
  through settlement. Logging never releases or delays that ownership for I/O.
- **`conventional_commit.lua`** — parses the `type(scope)!:` prefix of a subject into
  colored segments for consistent highlighting across commit rows.
- **`datetime.lua`** — formats epochs/ISO timestamps into relative ("2 hours ago") or
  absolute dates and returns highlight ranges for date spans, with an overridable `now()`
  for tests.

---

## 11. Infra (`infra/`)

- **`config.lua`** — the `ForgeConfig` schema, defaults, and setup merge. Owns
  buffer names, Harness launch descriptors, execution defaults, perf options, and the full
  keymap config. List values replace defaults atomically, so a configured key list never
  inherits trailing default entries.
- **`confirm.lua`** — owns the centered `y` / `n` confirmation dialog. Discard
  displays the captured file path, hunk label, or selected-file count before sending
  a mutation. `n`, `q`, Escape, and leaving the dialog cancel without dispatch.
  Missing-PR creation uses the same dialog with the Status title.
- **`views/status/dialogs.lua`** — preserves the single-line New branch popup and
  derives discard descriptions from the captured semantic selection. These dialogs
  do not use `vim.ui.select` or `vim.ui.input`.
- **`views/status/issues_editor.lua`** — keeps Issues editable on its existing
  Status row. Lua retains the local draft across presentation updates and refreshes.
  `:write` sends the current text to the Rust Issues write route. Failed saves and
  edits made during an outstanding save retain the draft. Other rows remain read-only.
- **`choice_popup.lua`** — renders a small keyboard-driven chooser from typed options,
  centralizing option keys, `q`/`<Esc>` cancellation, popup sizing, and callback cleanup.
  PR lifecycle changes, closed-PR resolution, review verdict selection, and explicit
  GitHub mutation recovery reuse it. Lifecycle selection opens from the PR Status
  field. The normal `l` key retains cursor movement.
- **`popup_window.lua`** — exclusively owns Forge float construction and closure.
  Popup buffers and window options are installed before focus enters the float,
  preventing a one-line prompt from inheriting a Status winbar during `WinEnter`.
  Every custom popup, help window, chooser, and attached input leaves Insert mode while
  visible, then restores the originating window and its Normal, Insert, Replace, or Visual
  mode. Its `select` and `input` wrappers apply the same lifecycle to Snacks-backed
  `vim.ui` pickers without duplicating window geometry.
  Custom popup closure marks the origin transition so Status does not refresh on
  that `BufEnter` before dispatching the user's confirmed action. Actual repository
  refreshes still invalidate previously captured discard inputs.
- **`highlights.lua`** — defines every Forge highlight group at setup, deriving
  backgrounds from the active colorscheme so the diff colors track the theme.
- **`notifications.lua`** — centralized `error` and `git_failures` notifications.
- **`perf.lua`** — two independently gated JSON event/span profilers, batched and flushed on a
  timer. `diff_logging` records ForgeStatus, diffs, PRs, and shared UI work in
  `forge/diff-perf.log`. `harness_logging` records Harness lifecycle and provider timings in
  `forge/harness-perf.log`.
  Harness UI traces record correlated begin/end boundaries for response callbacks,
  transcript application, Markdown parser/render work, and approval display. Each record
  includes the process ID and monotonic timestamp. A 250 ms heartbeat records main-loop
  delays of at least 100 ms while Harness logging is enabled. Writes remain asynchronous
  and bounded, and payloads contain counts and identities rather than conversation content.
- **`paths.lua`** — path normalization and repo-relative resolution, case-insensitive on
  Windows.
- **`util.lua`** — leaf helpers: diff stat counting, loaded-buffer lookup, NUL-byte
  (binary) detection, filetype resolution.

---

## 12. Shared plumbing (`shared/`)

These four modules implement the **strategy/registry pattern** that lets one filetype
back four view kinds.

- **`command_specs.lua`** — pure data declaring the command vocabulary: each spec has an
  id, label, the views it applies to, and its hint-bar order. No behavior.
- **`view_command_set.lua`** — a per-buffer registry mapping command ids to `{ run,
  enabled }` actions, dispatched with an `enabled()` guard.
- **`view_controller.lua`** — a registry of one controller per `view_kind`, each
  optionally supplying `sources`, `head_rows`, `sections`, `command_set`, and
  `after_render` hooks. `run_hook(view_kind, hook, state)` is how `status_render` stays
  view-agnostic — it asks the registered controller what to do.
- **`keymaps.lua`** — installs status-family and generic command-set keymaps, then renders
  their sticky **hint-bar winbars** and help popups from the same configured keys.
  Capability-gated commands never enter a command set, so unsupported actions disappear
  from mappings, hints, and help together.

Forge restores loaded mini.clue buffer triggers after installing or rebinding document
and shared-view mappings. This preserves prefix help when asynchronous view setup
runs after mini.clue's buffer autocmd. Forge does not load mini.clue itself.

Native Status, comparison, walkthrough-list, and review replicas use `document_commands.lua` for mappings, help,
and sticky winbar hints. Hints select available bindings in `command_specs.lua` order and display each command's
first resolved key. Disabled bindings disappear from both maps and hints. Narrow windows retain close and help.
Document and shared-view mappings use Neovim's normal prefix resolution. They do not set `nowait`, so
`o` cannot consume the start of `opp`, `opP`, `ogp`, or configured longer mappings. A standalone ambiguous
key resolves after the configured mapping timeout.
Each visible window owns its prior winbar value, which is restored on departure unless another component replaced
the hint. Historical walkthrough sources disable this hint ownership to preserve their explicit source header.
Dropbar excludes native documents, and Harness retains its independent transcript winbar.

Push and pull keep the Status buffer visible throughout the native Git operation. The context owner
publishes Git progress into Push or Merge, preferring the latest percentage-bearing phase over a
trailing packing summary. It retains object counts and transfer rate when available. Percentages
describe individual Git phases, not overall operation completion. The writer normalizes carriage-return progress before
delivery. Header text retains at most 1,024 characters. The renderer updates existing context rows
without rebuilding file bodies, and preserves expanded bodies and folds when inserting or removing
a temporary remote row. Completion clears progress and refreshes repository state. Failure also
reports an error notification. Neither path opens a remote-operation console.

---

## 13. Bundled tree-sitter queries (`queries/`)

The plugin ships its own queries instead of relying on the shared `nvim/queries/` tree, so
they travel with the plugin. `query_runtime.lua` registers them by computing the plugin
root from `debug.getinfo` and appending it to the runtimepath — once, and from every entry
path (`init.lua`, `git_data.lua`) so the queries resolve no matter which
module loads first.

- **`diff_context.scm`** captures the named scope (function, struct, class, trait,
  interface, impl, module, ...) enclosing a changed line. The capture is labeled `@scope`
  with a `@scope.name` child. `git_data` + `syntax_engine` use it to label hunk boundaries
  ("in `fn foo`") and to merge change regions by scope. Languages: rust, typescript, tsx,
  javascript, python, slang.

`vim.treesitter.query.get(lang, "diff_context")` resolves these through the runtimepath —
which is why `query_runtime` must run before any consumer.

---

## 14. End-to-end flows

**Open the status view**

```
:ForgeStatus
  └─ views/commands.open
       ├─ create/reuse ForgeStatus buf, state.attach_status_state(buf, state)
       ├─ status_snapshot.collect_async(cwd, {})
       │    ├─ porcelain-v2 status -z --untracked-files=all
       │    ├─ zero-context unstaged/staged MRC diffs
       │    └─ unstaged/staged added-file numstat
       ├─ git_data + section_map                       (cache + canonical section tree)
       ├─ operation_journal.reset                      (confirmed baseline)
       └─ status_render                                (head + sections → lines → extmarks)
            ├─ size_gate defers oversized file bodies
            ├─ fold_state applies native folds
            ├─ vim.diff returns disjoint line indices applied bottom-up
            └─ diff_source_state + render/* paint expanded hunks
```

**Stage a hunk**

```
cursor on a hunk, press the stage key
  └─ keymaps dispatch → actions.status_stage_entries
       ├─ operation_journal.append + cache projection
       │    ├─ render the staged projection immediately
       │    └─ never restore or move the cursor
       ├─ mutation_coordinator.enqueue(root, task)
       │    └─ repository FIFO → index_mutation → git apply --cached
       └─ after FIFO idle + 120 ms quiet
            └─ one path-scoped status_snapshot
                 ├─ match → retire layer, perform no status or diff-buffer write
                 ├─ mismatch → replace Git truth for the path, render once
                 └─ failure → notify, cancel queued burst tasks, preserve completed
                              Git writes, snapshot truth, force one recovery render

Later accepted journal layers replay over any confirmed snapshot before the next task
runs, so rapid stage and unstage actions never expose an intermediate backend frame.
```

**Stage a visual file selection**

```
visual line range, press the stage key
  └─ entry_nav captures the next surviving file id, otherwise the previous file id
       ├─ keymaps exits visual mode
       ├─ entry_nav expands sections and lets selected files dominate their hunks
       ├─ actions projects the accepted files into Staged
       ├─ originating status render restores the captured id once
       └─ queued Git work and every deferred synchronization render stay cursor-neutral
```

**Open a PR review**

```
open_review(pr)
  └─ views/commands.open_review (view_kind = "review")
       ├─ review.load_draft (disk)  +  review.load_remote_before_open (GitHub)
       ├─ review.merge (conflict detection)
       └─ review.render → Unviewed/Viewed sections with anchored comment boxes
            enter box → selected occurrence becomes full-width editable rows
            press C on changed line → create and focus a new comment
            press cc → comment input → annotations sync queue → gh review comment
            submit  → flush sync queue → gh submit review (verdict)
```

**Commit**

```
press the commit key
  └─ commit.commit
       ├─ reserve exclusive commit admission
       ├─ resolve repository root
       ├─ wait for mutation_coordinator.when_idle(root), cancelling on failure
       ├─ spawn headless nvim as GIT_EDITOR
       │     └─ client connects back over RPC → commit.editor
       │           ├─ open COMMIT_EDITMSG in the borrowed preview window
       │           └─ ai_commit.populate_commit_buffer_when_ready (AI prefill)
       ├─ <C-c><C-c> writes + commits, <C-q> aborts
       └─ pre-commit hook output streams into the console buffer
```

**Design and review through Harness**

Planning captures an immutable declaration baseline. Approval records the reviewed design, creates its execution goal, and starts the Implement phase.

Each Implement, Verify, and Resolve attempt owns a persisted exchange and independent metrics.
Acceptance appears once before the first implementation heading, without a synthetic user prompt.
Phase completion commits the exchange's phase outcome together with the execution and goal state.
The broker finishes that exchange after the provider turn settles and admits the next phase into a
new exchange. Failed verification enters Resolve. Only successful verification emits Plan complete,
outside the final phase fold. A provider failure emits Plan failed with its reason, while cancellation
and blocked verification retain their distinct outcomes. Pauses and interruptions retain the execution phase. Recovery resumes
that saved phase without repeating acceptance or adding tokens and tools to a completed phase.

```
:ForgeHarness -> multiline composer -> /plan <request>
  -> broker captures Rust, TypeScript, and Lua declaration overviews
  -> selected backend receives the declaration planning prompt in Read authorization
  -> harness_plan_read exposes the virtual file inventory and individual overviews
  -> harness_design_apply_patch atomically edits proposed declarations with optimistic versions
  -> harness_plan_submit verifies workspace digests and freezes the reviewed revision
  -> PlanReview displays the shared native file and hunk diff
       -> Enter opens a declaration snapshot and C adds a line comment
       -> oN sends inline comments and overall feedback for revision
       -> oY approves the saved design and selects execution authorization
       -> Implement -> Verify -> completion, or Verify -> Resolve -> Verify
```

**Persist goals without hiding user prompts**

Codex goal notifications use one owning-thread state decoder for both reader lifetime and
`TurnEvidence`. Native goals preserve complete, paused, blocked, usage-limited, budget-limited,
and cleared outcomes before considering tool activity or continuation budgets. Suspended goals
require explicit resume. Pause acknowledgment accepts an already-paused goal.
Native resume activates the goal through the exchange's provider reader and adopts the turn
that Codex starts automatically. It does not also send `turn/start`. Preparation traffic cannot
publish exchange activity. Explicit turn admission waits for the returned provider turn ID,
filters prior main-turn events, and retains ongoing child activity within the same exchange.

```
backend turn completes
  └─ GoalRecord observes { tool call, workspace change, structured/native terminal state }
       ├─ queued user prompt exists → admit it before any continuation
       ├─ progress → request another continuation at idle
       ├─ first no-progress turn → one retry
       ├─ second consecutive no-progress turn → stalled
       └─ 20 total goal turns → stalled until explicit /goal resume resets the budget
```

**Roll back and revise an interaction**

```
/undo → recovery or newest-first exchange picker → saved preview → confirmation
  └─ exchange.rollback.apply revalidates HEAD and affected file contents
       ├─ stale preview → no mutation, refresh and confirm again
       ├─ saved recovery journal → replace files and persist per-file progress
       ├─ interrupted restore → persistent recovery state, block new workspace work
       └─ completed restore → mark exchange history, refresh, return the original prompt
```

### Harness broker boundary

The feature-first Rust crate lives at `nvim/rust/forge/crates/forge-harness`. Its directories
name capabilities rather than layers: `broker`, `session`, `plan`, `goal`, `exchange`, `turn`, `timeline`,
`checkpoint`, `backend`, `storage`, `workspace`, `protocol`, and `control_tools`. `Backend` defines
the complete provider contract once, while `CodexBackend` and `CopilotBackend` own their private
transports and expose capability values that drive broker and editor behavior. `CodexJsonRpc`
remains private to the Codex implementation. `CopilotEventDecoder` remains private to the native
Copilot SDK implementation. Consumer-owned traits stay beside the feature that consumes them.

`BackendModel` forms the provider-neutral model-picker contract. Each backend supplies the model
identifier, ordered reasoning choices, context-window tiers, vision capability, and optional
description. It never supplies a display label because the model identifier already owns that
identity in the UI. The broker overlays durable reasoning and context selections per model before
Lua receives the catalog. `views/harness/model_picker.lua` therefore owns only picker interaction
state, field cycling, and presentation order. Copilot maps the selected context tier into native
session creation and `set_model`, while Codex exposes only the controls returned by app-server.

`BackendInput` distinguishes ordinary text from explicit skill invocation before either provider
sees the prompt. `SkillDefinition` normalizes discovery and enabled state, while each `McpDefinition`
owns its tool inventory so callers never issue a second tool-list operation against the backend.

The broker runs once per Neovim process over JSONL stdio. An `Agent` owns persistent provider
identity, an `Exchange` owns one admitted request through resolution, and each actual provider
invocation creates a `Turn` inside that exchange. `ExchangeNode` preserves the order of turn
content, delegations, acknowledged clarification or steering, plan resolution, and artifact changes.
Session creation commits the primary Agent together with the session. A fork assigns a new primary
Agent identity. Delegation admission similarly commits the parent reference and queued child Exchange
together, before provider start evidence activates the child. A Delegation always projects its named
child Exchange, so later reuse of the Agent cannot change historical status.

Mode restart interrupts provider Turns and pauses the existing Exchange. Resumption preserves its
request, plan and goal associations, and initial checkpoint. Cancellation retains provider delivery
while cleanup awaits terminal evidence. A 30-second cleanup failure remains retryable, with the
Exchange finalizing and new request admission blocked. The terminal checkpoint and Exchange outcome
commit together. Rollback changes history disposition while preserving execution outcomes.
Checkpoint capture errors include the source path and retain the finalizing exchange. Ctrl-C
retries finalization even when no provider request is active. Goal resume retries finalization
before activating the goal, so a failed retry leaves the goal paused and admits no new work.
Execution admission retains one permit per session from request preparation through response
delivery. A second execution cannot reset the first request's cancellation state. Cleanup owns
a separate control permit that blocks new execution admission until cleanup returns. Status
reads and approval responses remain available. Ctrl-C during a mode restart suppresses its
automatic resumption and waits for the existing cleanup acknowledgement and execution result.
It then cancels the settled exchange and pauses its goal durably, preventing a reconnect from
resuming work that the user stopped. Repeated idle cancellation leaves terminal history intact.
The shared provider instructions treat `.gitignore` as ordinary model-owned project configuration.
When a change introduces generated files, the model inspects existing ignore rules and proposes
needed additions or updates during planning. It edits the actual file during implementation before
generating those files, preserving comments, negations, unrelated rules, source, and required
lockfiles. Harness neither creates the file automatically nor requires an empty file when no rules
are needed. Resumption rechecks current contents before edits. These workflow instructions remain
outside product requirements. Incompatible workspace changes use plan revision.

Stable tool identities merge start, output, and completion events into one canonical tool record.
Codex `mcpToolCall` items follow that same path. `CodexJsonRpc` converts their app-server
`server`, `tool`, and compact JSON arguments into one `server.tool(arguments)` title. Before that
title enters durable timeline state, the transport recursively replaces values named `token`,
`api_key`, `key`, `secret`, `password`, `passphrase`, `authorization`, `bearer`, `cookie`,
`session`, `credential`, `access_token`, `refresh_token`, `client_secret`, or `private_key` with
`[REDACTED]`. It does not guess from value shape, preserving useful hashes and identifiers.
`item/mcpToolCall/progress` replaces mutable previews, while completed result JSON is pretty
printed before rendering. MCP payload details stop at the Codex transport boundary, so
`ToolActivity`, `Turn`, and the Rust timeline projection represent every provider action as a
generic tool.

`trace::TraceStore` owns opt-in protocol diagnostics independently from timeline persistence.
The persisted global preference overrides the initial `harness_logging` default. `/log on`,
`/log off`, and the `/config` Logging toggle use the same service routes and also set Lua
performance logging. Trace controls bypass the broker's active-turn lock.
The aggregate `harness-trace.jsonl` retains bounded metadata. Detailed `logs/<session-id>.jsonl`
files retain request and response payloads with named credential fields redacted, including
Codex protocol frames and Copilot session configuration, prompt, event, and isolated-generation
records. Each file rotates at 15 MiB and retains three older segments. Oversized individual
records contain an explicit omission with their encoded byte count. Session-independent events
use the global log. `/log`, `/log open`, and `:ForgeHarnessLog` render the current session file
as timestamped events with indented JSON payloads in a reusable read-only tab. `R` refreshes
the view, and editor focus, buffer entry, and idle events refresh it when the file changes.
`/log clear` empties only the current session file and removes its three rotated segments.
It leaves the global trace, other session logs, and logging preference unchanged.
The Configuration picker displays the provider CLI and exposes an explicit provider-switch action.

The Codex `CodexTurnCoordinator` treats one user request as an exchange that may outlive
its first parent app-server turn. A `turn/start` acknowledgement admits its returned turn before
content reaches the exchange. This also covers input attached to an already-running native-goal
turn, where no second `turn/started` notification arrives. Admission excludes unrelated turn
events so cancelled activity cannot populate a later exchange. Cancellation pauses an observed
provider goal even when Harness did not create that goal, then waits for interruption evidence
and cleanup acknowledgements. The coordinator
retains the session's JSON-RPC connection while descendant threads remain active, accepts steering
as another parent turn on the same thread, and starts a bounded synthesis turn after the final child
completes. Child lifecycle updates change the child agent's exchange behind the existing
`AgentReference`, so they never move the child row. Codex `subAgentActivity` values enter that lifecycle only for explicit
start or terminal states. Directed `interacted` activity carries messages between agents without
creating a run, and the descendant tracker rejects the parent thread identity unconditionally.
`ActiveWait` drives only the current
Timeline Status at the end of the Main timeline. The status uses an animated spinner followed by
`Waiting for N subagents` and carries no duration because it represents current control state, not
history. A submitted plan in `AwaitingReview` renders `Waiting for plan review` with the configured
artifact key until the reviewer opens PlanReview. Clearing `ActiveWait` removes the status without
creating a timeline node. A normal
Harness submit while this status remains active uses the parent steering lane immediately. Ctrl-q
retains its explicit steering shortcut, and child timelines never project Main control state.

`turn.cancel` bypasses the broker's serialized request queue so Ctrl-c can interrupt an active
provider turn instead of waiting behind it. The broker drops the prompt future, asks the backend
to release any retained transport, then either retracts or cancels the interaction. A newly
submitted `prompt.submit` remains retractable only while the provider has produced no assistant
message, tool activity, task update, or workspace delta. Context usage and reasoning status alone
do not consume that eligibility. Codex maps an eligible retraction to app-server
`thread/rollback`, the broker deletes the provisional interaction and restores pre-submit plan or
goal control state, and Lua returns the exact prompt to HarnessInput. Copilot advertises no turn
rollback capability, so it always follows ordinary cancellation. Once visible activity or a file
change occurs, the broker finalizes partial timeline state and persists the interaction as
cancelled. This split prevents the composer from presenting text that still exists in provider
history while preserving the quick-regret workflow for a genuinely output-free turn.

`turn.restart` uses that same out-of-band lane for an execution-mode change. Shift-Tab records the
target mode in the Harness winbar, interrupts the active provider turn, persists the mode after
both cancellation completion and restart acknowledgement arrive, then resumes the cancelled interaction on the retained provider
conversation. The interrupted turn remains durable and the resumed provider work appends another
`Turn` to the same `Exchange`, so one user action keeps one checkpoint and one rollback boundary.
Codex exposes the submitted app-server thread for resumption, while Copilot exposes its retained
SDK session. A restart failure preserves the partial transcript, keeps the selected execution mode,
and notifies the user instead of silently replaying the original prompt.

All execution callbacks share mode-restart completion handling, including plan acceptance,
planning clarification, goal continuation, compaction, child-agent requests, and resumed turns.
The selected child timeline does not defer a session-wide mode change. A newer mode selection
replaces the pending target while cancellation or mode application completes. The restart marker
clears before resumed work begins, so another mode selection can interrupt that work immediately.
Cleanup or mode-application failure stops the sequence and reports the error without resuming.
Interrupted exchanges without an active provider turn project a paused footer even while their
retained exchange remains resumable. Host exit broadcasts a local stop event before failing pending
requests. The controller clears transient busy, cancellation, and restart ownership, stops timers,
and overlays a reconnect error on the retained transcript. Partial output and queued prompts remain
available. Passive refreshes and failure callbacks cannot start a replacement host or resume work.
Reopening Harness explicitly reconnects the presentation.

Session events above the 512 KiB frame limit use session-scoped, transfer-identified parts and a
completion record. Lua validates and assembles the complete event before delivering it to any
subscriber. Event transfers share two concurrent transfer slots with response transfers. Additional
senders wait for a slot. Aggregate content size does not reject a transfer.

`forge/protocol_contract.json` defines the versioned event vocabulary, routing policy, required
payload fields, and node enum spellings. Rust embeds it at compile time and Lua loads it from the
runtime directory. The broker forwards only the normalized backend events declared there. Raw
provider events remain in Rust, where document projection consumes them. Serialization rejects
undeclared UI events or missing required fields before transmission.

Direct frames and reassembled messages use the same Lua envelope and event validator. Responses
carry exactly one result or structured error. Unknown variants, conflicting identities, and invalid
counters terminate the faulty connection through the existing visible host-stop path. Null object
fields become absent Lua fields, while null array entries retain their position. Snapshot transfers
use the same null policy. Explicit null response results remain successful responses. Contract
changes require a wire-version change and rebuilding the host before reconnecting.

`turn.steer` uses the same out-of-band broker lane without creating another interaction. Ctrl-q
clears HarnessInput only after admitting the text into a pending steering record, then the backend
delivers it to the active provider turn and acknowledges the request. The Codex backend maps that
operation to app-server `turn/steer` with the active thread and expected turn IDs. A provider
acknowledgement closes the current thought boundary and appends a durable `SteeringPrompt` node to
the owning interaction. The timeline renders that child with the same yellow prompt treatment and
prompt-navigation index as ordinary user input. Failed or late steering creates no timeline child
and moves its text into the follow-up queue. The shared
steering lane activates before transport startup, so input submitted during connection or
`turn/start` setup waits for that same turn. Codex releases buffered input after the matching
`turn/started` notification or item activity confirms that the acknowledged turn is running.
The acknowledgement alone admits timeline ownership without asserting steering readiness.
Copilot maps the same lane to the SDK's immediate
delivery mode on its active session, then publishes the canonical acknowledgement through a
turn-scoped event sink only after the SDK accepts the message. Prompt mode does not gate the lane. Chat, `/plan`, goals, and
accepted-plan execution therefore share one steering contract.

The broker keeps an active dispatch response pending until every admitted steering request reaches
a terminal result. If the provider turn completes first, the backend rejects the unacknowledged
steering request and Lua moves its text into the ordinary FIFO follow-up queue. Unsupported or idle
steering leaves the composer untouched. This boundary prevents a completion race from dropping a
planning constraint while preserving the existing interaction semantics.

Provider checklist events normalize into complete `ProviderTaskUpdate` replacements regardless
of whether they arrive during `/plan`, accepted-plan execution, or ordinary chat. `TaskTracker`
owns stable Harness task identities, current provider order, and superseded history. Explicit
provider IDs match first, exact normalized titles match second, and an unambiguous ordinal slot
allows a provider rename. Removed tasks disappear unless a completed thought references them.
When one task is in progress at thought completion, the broker freezes that task ID onto the
thought. Later checklist rewrites never reparent historical work.

The live protocol publishes `ActiveThoughtUpdate` counters plus one replaceable latest-tool
record while a thought remains mutable. The Harness tree shows `Running N tools`, the latest
tool heading, and at most four wrapped output rows plus a hidden-count indicator. Native
previews apply this limit after display-cell wrapping, including when one source line spans
multiple rows. Explicit expansion retains the complete output. The live preview does not
expose a fold whose contents could change while open. Each lifecycle event replaces that preview, so a newly started tool displaces the
previous tool instead of accumulating mutable rows. When the next thought or turn boundary
closes that thought, the broker merges successful completed provider file-change items in their
first-seen order and publishes one immutable `CompletedThought`. The UI then changes
`Running` to `Ran` atomically and enables semantic expansion nodes for that thought, its tool
list, each tool result, and its changes. The final assistant message becomes the Markdown response instead of another thought
when it contains no tools.

Codex `fileChange` items carry path, operation kind, move destination, textual diff, and final
status. The backend replaces provisional patch revisions by provider item ID, while the timeline
retains first-seen tool order. Only completed successful file-change items contribute to a
thought diff. Commands, formatters, generators, failed patches, and declined patches therefore
remain visible as tools without being misattributed as authored edits. Backends that do not
publish structured file changes omit the thought-level Changed node instead of inferring it from
the filesystem or command output.

Every Git interaction captures a baseline before its first provider turn and a terminal
checkpoint when it completes, fails, or cancels. The first baseline is checkpoint zero,
including pre-existing staged and unstaged content. Steering within an exchange retains
its baseline. Each later exchange captures the current workspace before starting.

`CheckpointRecord` stores an existing Git tree ID, complete content overrides, deletions,
file modes, and sampled checkout conversion rules. Capture never creates commits, changes
refs, stages files, or writes the user's index. `forge_git::checkpoint` enumerates the tree,
index, and nonignored worktree entries through gix. Matching non-racy index metadata avoids
reading clean files. A bounded process-local cache reuses unchanged dirty file identities.
New or uncertain content streams through a 64 KiB buffer and is published only when it
actually differs from the Git baseline. A second metadata observation rejects captures
that observe concurrent changes. This is optimistic validation, not a filesystem snapshot.

Clean files resolve through the recorded tree, independently of the current branch.
Checkout rules are sampled with Git attribute precedence. External filters and encodings
use captured raw overrides rather than running commands during restoration. Individual
source reads traverse only the requested tree path. Large baseline acquisition streams
through a temporary file using a bounded, 30-second `git cat-file` operation. Missing
baseline objects produce an explicit error before mutation. No checkpoint refs are added
to protect objects from a later external prune.

At terminal capture, `ProviderChangeIndex` collects normalized paths from successful structured
file changes in the main timeline and every referenced child-agent turn. The checkpoint comparer
then produces an attributed diff restricted to those paths and a checkpoint diff across every
path. Because both use the same baseline-to-terminal content comparison, repeated edits,
overlapping thoughts, and reversions resolve to one final canonical patch instead of summed
provider hunks. Equal patches render one interaction-level `Changed … · checkpoint matched`
node. Divergent patches render independent `Changed …` and `Checkpoint total: …` nodes. The
per-thought provider trees remain available at their original timeline positions.

The checkpoint total remains the rollback and cancellation-divergence authority. It intentionally
includes command, formatter, and external-process effects that do not belong to a structured
provider file change. A concurrent unattributed edit to a provider-reported path cannot be
separated without operating-system provenance, so that path remains attributed while the complete
checkpoint still preserves the exact rollback boundary.

The Rust `TimelineProjector` combines interactions, plan lifecycle records, and accepted-plan
executions into one ordered presentation. The Lua controller renders that projection instead of
replaying raw provider events or maintaining a tail-only checklist. `interaction_tree.lua`
coordinates the high-level projection, `task_tree.lua` owns provider tasks, and `plan_event.lua`
owns artifact lifecycle rows. Each completed node owns a stable expansion key, and the renderer materializes
only the children selected by `session.harness.activity_expanded`. This projection avoids
overlapping native ranges, which cannot reliably represent wrapped thought and command headings.
When a planning-feedback lifecycle answer exactly matches its durable continuation prompt, the
projection keeps the interaction as the single visible owner and omits the redundant lifecycle row.
`display_text.lua` wraps prompts and thoughts into real rows using rendered-cell width before row
ownership, highlights, and folds are assigned. Continuation rows therefore preserve the tree's
two-column indent without depending on window-local soft-wrap behavior.
Markdown responses retain each source line and rely on Neovim soft-wrap so window resizing can
reflow prose without rebuilding the timeline. Assistant responses and commentary project local
file links as their labels, with the original destinations retained in navigation metadata.
This removes hidden path bytes from Neovim's wrap calculation. Web links and code examples retain
their Markdown source. The projection preserves physical rows and remaps byte-column targets,
while durable responses retain the original Markdown. Their two-column structural indentation lives in
the real buffer text because `breakindent` cannot measure inline virtual text. The first response
row overlays `▸ ` onto those two spaces, preserving the timeline marker without changing layout.
Keep structural whitespace real and reserve extmarks for overlay markers and highlighting, or
wrapped response continuations will regress to column zero.
`markdown.lua` registers `Harness` as a Markdown Tree-sitter filetype so render-markdown's default
parser lookup resolves the same parser, then restricts that parser to the response ranges emitted
by the timeline. A render with no response ranges clears the included regions and render-markdown
namespace, preventing Markdown captures from leaking into prompts, tools, or shared diff rows.
`transaction.lua` compares stable node blocks, applies changed blocks from bottom to top, and
preserves semantic cursor identity, viewport position, expansion state, and settled prefix extmarks.
It validates semantic row indexes against the post-mutation buffer before restoring the cursor, so
switching between timelines with different lengths cannot address a row from the previous projection.
It mutates a hidden transcript buffer without applying window-local cursor, view, or fold state when
that window currently displays Permissions or another view. Returning to Harness then rebuilds folds.
It rebuilds native folds only when fold topology changes, which prevents timer-driven streaming
frames from repeatedly closing and reopening unchanged folds.
Active thoughts never expose expansion keys. Completed nodes stay immutable, so an expanded
command or diff never changes while the user reads it.
Prompt submission does not create a Lua-owned interaction. Broker admission emits the first
revisioned patch with the durable interaction and a Rust `Working` status, so the first visible
segment already comes from canonical state. Later node and completion patches replace that stable
interaction id without a presentation-side merge clock or optimistic shadow record. Lua animates
the Rust `started_at_ms` timestamp but never creates, extends, or completes an interaction.

Each stored session uses one versioned envelope. Session loading and listing accept only the exact
current format, leaving older rows invisible without migration or partial decoding. Runtime code
therefore reads only the current ordered interaction model.

ForgeStatus, PR review, and the Harness tree route file headers and hunk bodies
through `diff_component.lua`. It owns the call into `diff_render.build_fancy_diff_rows` plus the
`status_buffer.add_fancy_row` and `status_buffer.add_segment_line` accumulation used by every
consumer. `diff_tree.lua` supplies checkpoint paths, expansion keys, caller-selected indentation,
and interaction metadata without recomposing any visual row. The result retains the native
`diff_row_spans` state shape, and `row_emitter.lua` applies that same state in ForgeStatus, PR review,
and Harness. No Harness code reconstructs background or syntax ranges. The shared path keeps Tree-sitter syntax, file
labels, stats, gutters, row backgrounds, intraline replacements,
diff-line identities, and annotation coordinates consistent across live work and later review.
Harness materializes file and hunk children from semantic expansion state. Other consumers may
request the fully materialized tree and retain their existing view controller.

SQLite uses WAL mode under
`stdpath("data")/forge/harness/harness.sqlite3`. File content lives in a SHA-256
object store, while plans retain canonical JSON and generated Markdown projections under `plans/`. A session
lease grants one live Neovim write control and leaves other instances free to browse.
Immediate SQLite transactions arbitrate acquisition, a ten-second heartbeat protects long
turns, and owner-checked saves prevent a stale broker from overwriting a replacement owner.
An initialization collision returns structured lease metadata instead of a terminal string
failure. Neovim offers Retry and Start New Session for every collision, and adds Fork Session
only when the persisted backend capability advertises native fork. Fork recovery initializes
an independently leased broker and forks the provider conversation without taking or releasing the
source lease. The child deliberately starts with no copied Harness interactions, comments, plans,
executions, goals, or agent runs because the backend-owned conversation already carries the inherited
model context. Its local timeline begins with one durable typed `Forked` event containing the source
session ID and display name. Explicit `/fork <name>` uses that child name. Bare `/fork` derives
`<source-name> (fork)`, or `(fork)` when the source remains unnamed, without enforcing uniqueness.
Codex forks request `excludeTurns` because Harness does not render provider-returned history. The broker
copies the source session's last durable `ContextUsage` into the child before returning its snapshot, so
the winbar immediately shows inherited context pressure while the provider retains the full model history.
Neovim opens a provisional child timeline before issuing `session.fork` and renders `Forking from ...`
without inventing a durable session identity. A successful response promotes that same buffer to the
provider-backed child. A failure remains visible in the provisional timeline instead of restoring silence.
Fork responses carry transient performance diagnostics for provider process startup, initialization,
native fork work, broker persistence, and snapshot projection. Neovim records those phases alongside
client-observed completion latency in the `forge/harness-perf.log` JSONL stream when
`harness_logging` is enabled.

Transcript synchronization coalesces event bursts with a 67-millisecond timer, limiting normal
refresh requests to 15 per second. Events arriving during an active update retain one trailing
refresh. Each response publishes its ordered batch of at most 64 patches without yielding
between revisions. Queued document requests wait until publication completes. Closing the view
cancels its pending refresh timer, and host replacement cancels preparation before publication.
Validation failures release the queue and report the recovery failure.

Status hints read the last committed frame during preparation. They cache block-relative targets
and resolve physical rows on each render, including after same-revision snapshot replacement.
Invalid committed targets trigger diagnostic recovery instead of indexing a missing text row.
Commands continue in the provider process throughout these editor updates.

Row-preserving single-line edits and metadata-only updates retain identical fold definitions.
Structural edits rebuild native folds and restore each window's fold preferences. With
Harness logging enabled, `ui.buffer.patch` records validation, view capture, fold capture,
sequence updates, buffer text writes, metadata installation, editable-region attachment,
decorations, fold refresh, and view restoration separately in milliseconds.

Broker initialization selects the most recently updated session for the resolved repository
and configured backend, then restores its interaction timeline, plan, goal, model controls,
and provider session identity. Independent model, effort, and service-tier preferences remain available
when older sessions become invisible. `:ForgeHarness` therefore resumes current repository-local work across Neovim restarts,
while `/clear` remains the explicit boundary for creating a new session. Resumed sessions retain
their persisted execution mode. New and forked sessions establish a fresh Read boundary.

The Provider row in `/config` and `/backend` open the same shared picker over selectable providers.
Selecting another provider requires an explicit New chat or Resume session destination. The resume
picker searches and previews only that provider's sessions in the current workspace. Switching is
unavailable during active turns, queued prompts or steering, pending configuration, and another switch.
Cancellation before startup preserves the active chat. Confirmation stops the host and initializes the
selected provider directly into a fresh session or the exact selected session, displayed in the current
tab. Rust validates the selected workspace and provider before acquiring its lease. Sessions retain
their own conversations, plans, goals, and settings without a cross-provider fork or context transfer.
An already-open destination moves its existing buffers into the current tab, preserving its unsent
draft and closing its previous tab. Switching back reuses retained session buffers without creating
duplicate buffer identities. Presentation callbacks reject obsolete session and host generations,
and shutdown events cannot start state reconciliation while a provider switch owns the host.
Neovim stores the last successful selection in
`stdpath("data")/forge/harness/backend.json` before the next editor launch. An explicit
`setup({ harness = { backend = ... } })` remains authoritative, and failed switches restore the
exact previous session and backend rather than persisting a broken default. Cancelling lease-conflict
recovery also restores that source session. Ordinary Harness startup still resumes the latest
same-workspace session for its configured provider.

Declaration design records use schema 6. Older plan records are excluded from storage reads,
and older document schemas fail validation. Planning no longer authors entities, stages,
or implementation tasks.

Harness captures nonignored Git working-tree Rust, TypeScript, TSX, Lua, JSON/JSONC, TOML, YAML, and XML files. Tree-sitter
language adapters extract immutable declaration baselines, complete signatures, fields, visibility,
generics, imports, and documentation. They omit executable bodies and value initializers. Private
members remain visible when changed. Lua function headers omit the closing `end`. TypeScript arrow
bindings stop at `=>`. These overview files describe interfaces and do not claim compilation validity.
Configuration retains complete values. Unsupported extensions remain outside this design representation.

`declaration` owns Rust and TypeScript name resolution for submission validation and `.` navigation.
The shared Tree-sitter `DeclarationIndex` extracts scopes, symbols, imports, exports, generics, and
signature references without examining implementation bodies. Proposed declarations replace their
baseline files. Deleted lines use a separate baseline graph, and deleted files cannot reappear from
disk. Lexical bindings take precedence over implicit library names. Primitive types use language
rules. Library types resolve through real declarations rather than a name allowlist.

Cargo metadata runs against isolated manifests and target stubs under Cargo's cache. It acquires
dependency sources through Cargo's normal registry and Git caches, preserving the reviewed checkout.
The plan retains captured Cargo lockfiles as generated, immutable metadata. Manifest and captured
lockfile identities select reusable package graphs, while source-content hashes select
parsed declarations. Workspace dependency aliases, library roots, modules, and re-exports retain
their package identities. External module sources load when reference resolution reaches them,
so opening a plan does not parse every dependency or standard-library module.
The selected workspace toolchain supplies `rust-src` and edition-specific
`std` or `core` preludes. Missing `rust-src` produces a warning with installation instructions and
disables standard-library and prelude checks. It does not disable project or dependency checks.

TypeScript resolution reads configured libraries from the installed TypeScript package, follows
library references, and indexes included project globals and ambient type packages. It respects
explicit `types`, version-dependent defaults, type roots, path aliases, relative modules, and package
declaration entry points. Exports do not become globals. Type and value namespaces remain distinct.
Unsupported package selection, incomplete syntax, generated declarations, conditional compilation,
and unavailable sources produce unverified evidence rather than invented missing-import errors.

The submit tool runs reference validation before becoming terminal, so the provider can repair a
rejected design in the same turn. The broker repeats validation on the formatted snapshot before
freezing it. `DeclarationDesign.validation` stores generated location-bearing diagnostics and its
snapshot fingerprint. Patches invalidate that evidence. Review displays a collapsible Validation
section. Proven invalid or ambiguous references reject submission. Unverified references warn.

`.` resolves the selected token through saved Tree-sitter token coordinates. It jumps to a visible
plan declaration and opens containing folds, opens a declaration snapshot when the target is hidden
or outside the diff, or opens the exact existing project/dependency/standard-library source location.
Re-export navigation follows the defining declaration. Filtering remains unchanged. Native input
revision, view, and cursor guards discard stale responses. Lua owns key binding and presentation,
while Rust owns resolution and source coordinates.

`DeclarationDesign` persists baseline text, original source digests, proposed text, and explicit moves
inside Harness-owned `plans/<session>/<plan>/working.json`. New plans read `.forge.json` settings
and start with no captured source files. They perform no repository file discovery or extraction.
The provider receives the plan identity,
version, request, and path inventory. `harness_plan_read` reads one virtual file or its baseline as
numbered plain text. Optional 1-based inclusive line bounds require a file path. The response retains
the active version, selected side, actual range, and total line count. An uncaptured-path read extracts
only that workspace file and returns its full-source digest without changing the saved design or version.
Inventory reads remain structured and list captured and proposed paths rather than workspace files.
`harness_design_apply_patch` applies familiar Add File, Update File, Delete File, Move to, and
context chunks atomically. The first Update, Delete, or Move captures that path's baseline and source
digest in the patch candidate. Add and Move reject occupied workspace destinations. Captured removals
and moves never reload their original source. The control runtime retains inspected digests within a
turn, and optional `source_digests` preserves read identities across turns. Provider transports retain
first-capture digests for broker replay, so a source change after acknowledgement also rejects the patch.
Invalid syntax, paths, bodies, stale versions, source identities, or unmatched context change nothing.
Every patch that changes the design increments the version. Patch confirmation includes
the applied declaration and overview deltas with three context lines, capped at 16 KiB with explicit
truncation. The agent can confirm focused edits without a second read. The baseline remains immutable.
Successful submission retains inconclusive reference warnings in the saved validation evidence and
returns only acceptance and version to the provider. Proven-invalid references still return actionable errors.
Reference validation reads relevant ancestor manifests, lockfiles, and untouched modules on demand
without adding those source files to the proposal. Lockfile evidence remains separate from editable files.
Files first captured in a later submitted revision compare against their captured source baseline,
and removed-row navigation opens that baseline in the current revision rather than a missing prior proposal.
`plan/prompts/system.md` defines stable model-facing Harness conventions. It separates permission
presets from Plan, Execute, and Goal tasks, preserves task identity across interruption, and explains
control transitions, questions, evidence, and failures. `backend/prompt.rs` composes current effective
permission and interaction purpose from the admitted request. Recorded-answer guidance comes from
control context rather than a phrase found in user text. Codex receives the shared instructions and
current context through `developerInstructions` on both thread start and resume, including native goal
resumption. Copilot appends shared instructions when creating or resuming a session and sends current
interaction context with every message, including messages to an already-live session.

Plan metadata stores data flows separately from the Design Markdown. `design_flows.rs` owns
the recursive `text`, optional incoming `via`, and `children` node schema, validates its size and
depth, and generates code-fenced diagrams. The planning prompt follows values, requests, events,
records, and artifacts through producers, transformations, stores, and consumers. Short labels name
one semantic role each. Paired JSON and rendered examples demonstrate transformations, shared
consumers, and alternative results. Scheduling rules and lifecycle guarantees stay in descriptions.
Short chains share a line, longer chains continue with arrows at the same
indentation, and only actual splits create indented branches. Branches retain their incoming
transfer or condition labels and continuation rails. The shared section
projection places Flows after Design and omits an empty inventory. Section reads, review output,
and revision diffs use the same generated Markdown without changing the stored JSON.

Plan review places Tests after proposed declaration changes and before Verification. Each section
retains its own fold boundary and navigation targets in both full and public-only views.
The blank row before Tests stays outside the Changes fold in both views.
Public filtering preserves that unhighlighted section spacer even after an empty diff row.

`plan/prompts/planning.md` owns the virtual file procedure, metadata roles, declaration syntax,
validation contracts, and patch examples. `PlanPrompt` embeds it in draft, feedback, revision, and
editable discussion requests. Execution prompts retain execution ID, plan ID, accepted revision,
phase, findings, and the accepted document. Start, continuation, and interruption resume have distinct
guidance. Resume requires checking current files, tool results, and background processes before
repeating work. Successful phase completion ends the model turn so Harness owns the next phase
transition. Execution revision submission retains the provider turn while the broker commits the
revision and returns the decision. Ordinary pending-question follow-ups do not claim to be Plan tasks.

`harness_question_ask` pauses either backend on one to three structured decisions while
`harness_plan_submit` alone creates a review artifact. `ControlToolRegistry` owns every Harness
tool schema once, then Codex projects it into app-server dynamic tools while Copilot projects it
into SDK `Tool` handlers. Ordinary prose remains an ordinary planning response until the agent
invokes a structured control tool.
`ControlToolRuntime` owns the provider-visible state machine for one bounded turn. The broker
captures the active plan state, canonical document, resolved question digests, elicitation,
execution, and goal state, then both adapters feed every control invocation through that Rust
runtime before forwarding it. Versioned patches atomically mutate virtual files and plan metadata,
so both providers share the same authoring and validation boundary. The runtime terminalizes question asks and plan
submissions, rejects controls outside their state, and treats an already-consumed question digest
as an idempotent success rather than creating another picker. The broker still replays accepted
operations against durable state with optimistic version checks, which keeps the session-scoped
broker document authoritative. Adapters export a control invocation into aggregate
`BackendOutput` only after its provider request returns success. Rejected invocations remain
visible as failed timeline activities but never enter durable replay, preventing a corrected plan
from being poisoned by an earlier validation failure. Started and completed lifecycle
notifications update the rendered tool activity only. They never authorize a control mutation,
because those notifications can precede validation or replay a rejected call after its response.
A separate accepted-request identity set keeps successful provider request replays idempotent
without allowing rejected requests to reserve that identity.
The question tool also works during ordinary chat, goal, and execution turns. Those questions
persist on their owning `Exchange`, while planning questions remain on `PlanRecord`.
After submission, a normal prompt associated with the selected waiting Plan task creates a plan discussion exchange.
`PromptMode::PlanDiscussion` supplies the canonical document and instructions to answer questions
without editing or resubmitting. Discussion questions remain on that exchange, and clarification
and answer continuations retain its planning context. Reading leaves `AwaitingReview` unchanged.
A successful version-checked edit applies `ChangesRequested`, changes the exchange to
`PlanRevision`, and clears pending acceptance. The planning loop then requires submission or a
new question before completion. A rejected edit does not start a revision. Tool authorization
allows discussion reads and edits but rejects resubmission without an edit and rejects editing
while a question remains unresolved. These transitions retain the selected execution authorization.
`BrokerSnapshot.active_elicitation` projects either owner through one question UI contract, so
answer, skip, Ask, and continue reuse the shared bottom picker without creating a plan artifact.
Submitting the review page consumes that question set before the provider continuation starts.
The continuation prompt and control-tool descriptions therefore forbid `harness_question_answer`
and `harness_question_withdraw` for the embedded planning feedback. The Codex request responder
enforces the same boundary by returning an unsuccessful tool result and omitting the rejected call
from normalized control output. If Codex edits the plan but ends without submitting or asking a new
question, the backend starts one bounded corrective provider turn inside the same Harness
interaction and requires the missing terminal control action. Codex app-server can expose one
dynamic control invocation through both lifecycle and request messages. The JSON-RPC transport
normalizes that invocation once across the full Harness request, including its corrective turn.
This keeps one reviewed answer set attached to one planning interaction instead of reopening the
same questions or applying one plan edit twice.
Ordinary prose never counts as a submitted question set. Copilot `session.todos_changed` and Codex
`turn/plan/updated` events feed the generic task tracker without submitting a plan. Names printed
in prose or command output never count as control calls. Codex uses its app-server protocol
directly, including `model/list`, dynamic Harness control tools, `thread/goal/set`, and
`thread/fork`, but it does not set `collaborationMode` for Harness planning. Copilot uses the SDK
model catalog, native permission callbacks, immediate steering, cancellation, and streamed
subagent observation. Native fork enters the command set only after the backend advertises it.
No transcript-copy fallback exists.

Provider token-usage notifications normalize into durable `ContextUsage` session state. The
Harness winbar renders the remaining percentage and total window at its right edge without
adding timeline entries. Codex uses app-server `thread/compact/start` for `/compact`. Copilot's
SDK compacts automatically but exposes no manual compaction request, so its capability omits
`/compact`. Compaction never creates a user interaction, and unsupported backends omit the command
instead of receiving a synthetic summarization prompt.

Codex publishes every tool lifecycle update to the owning turn. Tool records merge only by
provider address and call ID, never by tool name or arguments. Repeating a control call with
the same arguments creates a separate lifetime. Replayed updates retain the original start
and completion timestamps. Accepted control effects deduplicate by provider call identity.
Catalog and background-terminal connections observe shared app-server broadcasts without
answering provider requests. Only the connection owning the active turn responds to controls
and approvals. An observer must not reject a broadcast, because its response can win the race
against the owning connection. Phase completion without an execution control context fails.

Exchange summaries retain native input, cached input, reasoning, and inclusive output counts
on each owning `Turn`. Codex cumulative snapshots recover call increments after the first
report and exclude repeated notifications and synthetic context-window adjustments. Copilot
reports every `assistant.usage` call with its native call identity. Rust persists report identities
and cumulative cursors inside `ExchangeMetrics`, so streamed events and terminal replay count
each report once. Parent and child exchanges keep separate usage. A late child report updates
its settled owning exchange without reopening execution. Missing categories remain unavailable.
`ExchangeMetrics.request_count` counts admitted model-usage reports once. Codex uses distinct
cumulative updates and Copilot uses native call identities. Duplicate refreshes, replay, and
synthetic context adjustments do not increment the count. A cumulative jump can recover token
totals without recovering how many reports were missed. Requests without usage reports, including
failed attempts and calls still in progress, remain outside this observed completion count.

Running, paused, and completed headers share the layout
`Planning 120s (12s tools) │ ~65 tok/s │ I 820.0k (94%) · R 2.8k · O 4.2k │ 8 req · 21 tools`.
The outer duration includes tools, approvals, and delegated waits within active execution and
freezes across explicit pauses. The parenthetical counts tool-only wall time, taking the union
of overlapping calls and capping outstanding intervals at interruption or completion.
Tool time uses milliseconds below one second and rounded tenths of a second thereafter,
omitting a zero fractional digit. The outer duration remains whole seconds.

Each distinct admitted usage report refreshes cumulative input, cache percentage, reasoning,
non-reasoning output, and request count immediately. Turns that have not reported usage do not
erase previously reported totals. Missing categories within reported usage remain unavailable.
Input includes cached tokens and cache percentage divides cumulative cached input by cumulative
input. Inclusive generated tokens include reasoning and tool arguments.

The same report captures cumulative generated tokens and active elapsed time minus the union
of tool, approval, and delegated waits. Their quotient supplies approximate effective throughput,
rounded to whole tokens per second. Both operands stay fixed until the next report, so ongoing
requests cannot lower the displayed rate before reporting their tokens. Provider and transport
overhead remain in this estimate. Throughput appears only when generated tokens and a valid,
positive duration are available. Elapsed and active tool time tick independently. Tool time appears
after a tool is recorded and only while its timing is complete.
Both renderers omit unavailable token categories, unknown cache percentages, and zero request or
tool counts. Reported zero token counts and zero cache-hit percentages remain visible. Separators
join populated sections only, including tool failures and spawned agents when present.
Private session format 36 hides earlier sessions without migration.


Each canonical `ToolCall` retains its first observed running timestamp and first terminal
timestamp. Tool headings show `• 2s command`, updating through the existing one-second activity
refresh. Parallel calls measure their own observed wall-clock intervals. Repeated progress never
resets the start, and repeated completion never extends the end. Turn completion or interruption
settles any outstanding call at that boundary. Completed timestamps survive session reopening.
Calls first observed at completion have unavailable duration (`—`). Subsecond calls display their measured milliseconds, such as `439ms`.
Longer calls display rounded tenths, such as `2.5s`. These labels do not round the stored
timestamps or throughput operands.
Every tool heading reserves six display cells for its duration, keeping command and MCP names
in one column across timer updates, tool groups, and expansion. Labels that exceed six cells
use compact minutes, hours, or days. Timer updates do not scan peer tools to size this column.
These durations measure Harness lifecycle observations rather than provider CPU execution time.

`/plan` uses the same `Thinking` and `Thought` interaction summaries as every model turn. A
question-only turn saves `PlanElicitation` under the active `AwaitingInput` plan, renders its
choices in the timeline, and opens the shared picker across the bottom of the complete Harness
transcript-and-composer surface. The border shows the question title and `N/M` progress. The
picker maps provider choices and Other through configurable `picker.choice_keys`, reserves `a`
for Ask, and reserves `o` for Other. Provider choices
default to `n`, `e`, `i`, `l`, `u`, and `y`. Arrow keys plus `s`/`t` change the light-blue selected
row without submitting it. The compact footer exposes only question navigation and Tab feedback.
Enter records an ordinary choice or opens the
attached editor for Other and Ask. Tab opens that same editor for optional choice feedback.
Ctrl-s belongs only to the attached input window, where it records the selected answer plus text
and advances to the next unanswered question. The input renders as an independent borderless child
inside rows reserved by the parent picker and uses the shared window-local prompt gutter, so buffer
columns contain only submitted text. Reserving those rows keeps the parent top edge stable while
the input opens. Opening the picker focuses its read-only navigation buffer with the cursor hidden.
Opening an attached input restores the configured cursor and enters Insert mode. Tab returns to
the cursorless picker without closing the child, so its text
follows whichever option the user selects next. Tab or `go` can focus that existing child again,
while Ctrl-c clears its draft, closes it, shrinks the picker, and restores option focus.

After the final answer, the float becomes an explicit review page that lists every question,
selected answer, and additional input in question order. `y` closes the elicitation and resumes
the provider. `n` returns to the first question while preserving the broker-backed answer set,
including feedback and Other text, so each answer can be inspected or replaced before submission.
The main question float never maps Ctrl-s and cannot bypass this review boundary.

Answer and skip requests update only the durable elicitation record. Ask and subsequent ordinary
Harness prompts run read-only follow-up interactions against a mutable decision set. The provider
must preserve the set without a control call when it remains valid, may replace the complete set
through `harness_question_ask`, may record only an explicit user answer through
`harness_question_answer`, and may remove the boundary through `harness_question_withdraw` only
when user direction or repository evidence proves that no material decision remains. A model
recommendation never qualifies as an answer.
Closing the float leaves a Timeline Status reading `Waiting for input (press oe)`. Clarification
chat can continue without reopening that unchanged question set, while `oe` or `/questions`
restores it explicitly. A provider replacement increments the elicitation revision, preserves only
answers that remain valid against the new schema, and presents the revised questions once. A
conversational answer also increments the elicitation revision so the next unresolved decision
reopens. Withdrawing an ordinary question clears its status after the response. Withdrawing a
planning question records `Question withdrawn: <reason>` in plan history and starts a new planning
turn automatically. Failed planning continuation restores the original elicitation and removes the
provisional withdrawal lifecycle, preserving the review boundary. The
status takes precedence over a concurrent subagent wait because user input blocks provider
continuation. The broker serializes missing answers as intentional best-judgment decisions only
when the user explicitly continues. A provisional `QuestionAnswered` lifecycle record is removed if that
continuation turn fails, so the timeline never claims feedback was consumed while the elicitation
has been restored.
Elicitation revisions prevent a restored session from repeatedly presenting the same picker, while
the transient Timeline Status exposes the pending decision after dismissal or restart.
Plan edit calls persist the structurally valid JSON draft without rendering its intermediate
cross-section state. `harness_plan_submit` validates ownership coverage and renderability against
the complete staged document. A rejected submission returns those violations to the provider and
leaves the draft editable, while a successful submission atomically refreshes `working.md`, its
navigation index, and the immutable submitted revision.
Successful submission appends one collapsed artifact delta to the planning interaction that
produced it and increments the artifact count in the winbar. The delta uses the shared changed-file
tree while labeling the canonical plan as an artifact, so revision history stays attached to its
causal turn instead of creating a sibling `Plan revision created` event or dumping the complete plan
into the timeline. Harness never opens PlanReview automatically. `op` selects a session artifact
through the floating no-preview picker, marks it active, and opens its physical working copy.
`C` creates an inline annotation at the current source line. Cursor focus expands its editable body,
while leaving the annotation collapses it into a compact rendered box. Request changes records
line/body input, then Rust resolves every line to its exact semantic target before starting another
read-only revision turn. That revision interaction names the overall comment when present and otherwise stays
`Request plan changes`. The durable `ChangesRequested` record remains state-machine history but
does not create a duplicate timeline node. PlanReview admits only one acceptance or revision request
at a time. Once the reviewer commits `oY` or confirms an `oN` comment, it destroys the physical
Markdown buffer immediately and restores the originating Harness window while the broker request
continues. Failed revision requests preserve the Lua annotation cache so `op` can rebuild the review
surface. Acceptance creates durable `PlanAcceptance` input in Rust and reuses the Harness multi-page
question picker for provider-context reuse and execution authorization. Dismissing that picker
cancels pending acceptance and restores plan-review status. Rust attaches one collapsed
`Resolved N comments` node before the revision artifact delta only after the model submits the
replacement plan. Acceptance prevents an accepted write plan from inheriting Read authorization and
blocking its first file change. It then emits
`Plan accepted` inside the admitted execution exchange and creates the guarded
`Complete accepted plan: <title>` goal. Phase transitions, revision decisions, and suspension events
retain an `ExchangeAnchor` containing the owning exchange ID and a boundary in its append-only
source node sequence. `TimelineProjector` inserts their content as `ExchangeNode::PlanEvent`
inside that exchange. The timeline has no separate plan lifecycle, execution, or resolution
container. Live provider replacement preserves these derived nodes at their captured positions.
Question answers and withdrawals retain the matching question identity and place its resolved
details at the response boundary.
The collapsed clarification row retains the question, options, and response, without duplicating
the user input row. Review feedback retains annotations before revision execution, including
failed revisions. Direct review renames remain visible as exchange-owned revision events.
Records saved before anchors existed use a compatibility path scoped to the matching plan or
execution. That path uses clarification input boundaries when available, but old provider content
without item timestamps cannot recover every historical intra-turn position exactly. Cancellation
pauses execution without changing its phase. `/goal resume` uses the effective canonical design,
retained findings, and phase instructions while preserving completed workspace work.

Codex background-terminal requests resume their owning thread on an independent connection.
A newly created thread can become visible before its rollout metadata is written. For the
specific `-32603` empty-rollout response naming that thread, the backend retries resume at
500 ms intervals, up to four retries and two seconds of added delay. Exhausted retries preserve
the provider error for the terminal observer to report. Other provider and transport failures
propagate immediately. Recovery never substitutes an empty terminal inventory for a failure.

Harness defaults to the direct Codex app-server backend. Set `harness.backend = "copilot"` to use
the native Copilot SDK. An empty `harness.backends.copilot.command` delegates CLI discovery and
startup to the SDK. A nonempty command selects an explicit Copilot CLI executable and prefix
arguments. Persisted provider session IDs resume through the same backend, and backend mismatches
remain invalid session transitions.

The Rust `PermissionStore` loads one validated Rulesync-shaped document from
`stdpath("config")/forge/permissions.json`, compiles command and resource matchers once, and
gives every surfaced provider request to one `PermissionCoordinator`. `allow` proceeds immediately,
`deny` rejects immediately, and `ask` blocks the provider response on the Harness approval float.
Persistent approval choices atomically replace exact or broad JSON rules with allow or deny. The
permission document remains outside provider write authority, including shell commands that name it.
In `permission.bash`, `"*"` supplies the lowest-priority decision for a recognized command.
More specific command rules override it. Without a matching rule, commands ask for approval.
Empty or ambiguous command text also asks, even when `"*"` allows other commands.

Approval requests bypass the timeline reducer and stream directly into the controller. Resolution
and cancellation emit matching lifecycle events keyed by approval ID, so the controller removes the
request, closes its float, clears the winbar status, and presents the next queued request atomically.
Cancelling a turn drains the coordinator before provider teardown, preventing abandoned requests
from reappearing through a later state snapshot.

Read, Write, and YOLO select approval handling. Read asks before edits and untrusted
commands, Write applies saved permission rules, and YOLO skips approval prompts. These
settings do not select a filesystem scope or disable the sandbox. `/mode` and its direct
commands change only approvals through the durable task transition coordinator.

`session.access` stores sandbox enablement, workspace/full write scope, additional writable
directories, and the Windows sandbox backend. `/config` saves these settings per workspace
and provider. The directory picker adds or edits existing directories using completion,
confirms deletion, and marks covered paths. Full scope and disabled sandbox retain the list
but make it inactive. Disabling the sandbox removes OS write restrictions, so the write-scope
selector remains stored but inactive. Copilot does not expose these isolation controls.

Configuration uses the same interrupt/finalize/resume transition as approval changes. The
broker validates and canonicalizes directory paths before committing settings. Provider
failures remain visible through the existing failed-operation state. Updated session and
preference envelopes hide incompatible earlier formats without deleting their records.
The broker publishes applied settings before resuming provider execution. That event updates
the terminal catalog policy and completes the configuration picker while the task continues.

`CodexSecurity` sends `sandbox` plus per-thread configuration to thread start, resume, and
fork, and sends `sandboxPolicy` to every turn start. Read maps to native `untrusted`, Write
to `on-request`, and YOLO to `never`. Workspace writes include the workspace and configured
directories. Full scope uses mounted filesystem roots while retaining OS isolation. Windows
backend selection is independent of approvals. Background terminal thread resumes receive
the same settings, so polling cannot restore an older policy.

Native Codex commands and patches run under its filesystem sandbox. Explicit provider
requests are evaluated by `PermissionCoordinator`, which no longer imposes an implicit
workspace ceiling based on approval mode. Copilot routes SDK approval callbacks through
the same coordinator. Provider-private operations without callbacks remain outside that
approval boundary. MCP servers retain their own process permissions.

The global Rulesync config omits the `permissions` feature only for `codexcli`. Codex therefore
cannot apply a generated exec-policy denial before Harness evaluates the request. Direct Codex CLI
sessions retain Codex's native sandbox and approval policy, while the other provider targets keep
their existing generated permission outputs.

Only the transcript window owns the Harness winbar. The composer clears its window-local
winbar so the split presents session identity once. The transcript winbar begins directly
with the active execution mode and resolved runtime model. `/config` displays
the underlying CLI provider.
The model picker and `/config` share `views/picker/field.lua` for field rendering.
The active field uses arrows, while inactive fields retain equal-width padding.
`/config` stores Logging preferences through the shared field picker.
Codex resolves the configured `default` sentinel through the `isDefault` entry from
`model/list`, then caches and persists that model on the Harness session. Copilot maps the SDK
model catalog into the same picker and applies supported reasoning effort when it creates,
resumes, or reconfigures the active session. Before resolution, the winbar says `resolving model`
instead of presenting `default` as though it were a real model ID.
Codex publishes `runtime_resolved` immediately after selecting its model, before starting
the provider turn. Copilot publishes the same event when the root session reports its model.
The broker persists this identity and forwards it directly to the winbar without waiting for
turn completion or a model-picker catalog request. Runtime identity events have no turn
address or transcript content, and child-agent model reports cannot replace the session model.
Catalog lookups only fill an unresolved default and never overwrite an observed runtime model.

`/rename <name>` routes directly to the broker's durable `session.rename` request rather
than entering the model transcript. The broker retains rename metadata without displaying a
timeline message. The first prompt names an unnamed session from its first 30 characters after
removing a leading slash command such as `/plan`. The Harness tab displays up to 30 characters of
the current session name. Bare `/rename` shows only
`Generating session name…` until the name is persisted. It captures visible conversation history and generates a 2-6 word name
in an isolated provider conversation using the selected model. Naming selects the lowest advertised
reasoning effort for explicit models. Copilot Auto retains provider-managed reasoning. The request
does not change the main conversation or its settings. Shared `TextGeneration` defines naming and recap
instructions and output validation. Provider adapters own temporary conversation lifecycles. Generated names
must fit 60 characters. The current name remains until generation and persistence succeed. Lua
rejects superseded replies, and the broker checks the captured name before saving a generated result.
`SessionEventKind` distinguishes
rename events from fork lineage without inferring behavior from optional fields. The `/sessions`
picker searches that name and substitutes `[unnamed]` for empty names,
keeping storage semantics separate from presentation fallback text.

Each visited Harness session owns a distinct nofile transcript buffer inside the active Harness tab.
`session_navigation.lua` maps those buffers to durable session IDs and re-enters the broker through
`session.resume` on `BufEnter`. Enter or `.` on a `Forked` row switches to the source buffer through
Neovim's normal buffer command, which records a native jump location. The existing `,` jump-back
mapping can therefore return to the child buffer, whose `BufEnter` activation restores the child
lease and snapshot without a parallel Lua navigation stack.

Non-Git WRITE requires an explicit confirmation and permanently displays `NO CHECKPOINT`
for that session. Git checkpoints include tracked and nonignored untracked files, exclude
ignored files, and never mutate the index or history during rollback.

Harness subagents follow the same Rust-state and Lua-presentation split. `AgentRegistry` owns
Codex definition discovery, persistent agent identities, provider thread identity, lifecycle state,
and transient execution addresses. `Delegation` owns the immutable parent exchange and turn link,
the task, and the child exchange identity. Every parent or child exchange owns one Git baseline and
terminal snapshot. Child turns reuse `Exchange`, `Turn`, and the shared Rust timeline projection.
Definition discovery merges built-ins, `CODEX_HOME/agents`, and workspace `.codex/agents` with
workspace definitions taking precedence. The home-directory `.codex/agents` path supplies the
personal fallback when `CODEX_HOME` is unavailable.

Codex collaboration tool status describes the parent tool call, not the child. The JSON-RPC
normalizer therefore emits one lifecycle update per `agentsStates` entry and uses each child's
reported status. Only spawn and concrete subagent-activity events may create runs. Wait, input, and
close events update an already indexed provider thread and cannot manufacture `default` agents.
Provider spawn events may precede the child thread identifier. `AgentRegistry` resolves that
two-phase handshake through one unambiguous active run with the same parent interaction, parent
thread, and turn. It refuses ambiguous sibling matches. Parent thread ownership becomes immutable
after binding, so later wait events cannot reparent a child or create a cycle. The Codex backend
also waits for the original parent thread and turn to complete. A child `turn/completed` closes only
the child timeline and never terminates the parent request.

`/agent` owns timeline navigation only. Its shared bottom picker presents Main, Running, and Done
sections, derives each child's elapsed time and tool/failure totals from the same summary component
as the parent timeline, and refreshes that presentation while it remains open. `/agent main` always
returns to the parent conversation. Zero-based numeric selectors and alphabetic aliases select only
running children in label order, so `/agent a` and `/agent 0` target the same first running child
without letting completed history shift those positions. Choosing any picker row switches timelines
and restores the window that opened the picker.

`/spawn` owns child creation. The no-argument form opens the shared bottom picker with a focused
fuzzy-search input over provider definitions. Choosing a definition transforms the same picker into
an attached multiline task input without resizing the Harness split. `/spawn <definition> <task>`
asks the parent Codex thread to spawn the selected definition because
app-server exposes child lifecycle events but no direct client spawn RPC. The parent instruction
requests exactly one spawn with the selected definition and rejects default intermediary agents.
The Codex launch strategy translates Harness definition names into native identifier form, such as
`local-code-explorer` to `local_code_explorer`, while preserving the catalog name on the durable run.
The main timeline nests each run under its spawning interaction, derives `Waiting on N subagents`
only while the parent has no active thought, and replaces that state when parent work resumes.
Child runtime, tool totals, failures, and completion remain visible in the collapsed node. Expanding
the node reveals its immutable thought and response timeline. Provider-nested children follow
`parent_thread_id`, preserving the actual hierarchy instead of flattening every run beside the parent.
Selecting a child changes
both the rendered timeline and composer target. Codex child prompts use the reported thread id,
while steering and interruption target its active turn id. Copilot exposes streamed child
lifecycle and timeline observation, but it omits a Harness-owned catalog and direct child control.
The picker therefore shows observed Copilot children without advertising unsupported spawn or
interrupt actions.

### Deferred Harness work

- Add task-level diff annotations and feedback cycles now that stable task identity and shared
  diff rendering exist in the timeline.
- Audit provider-local hooks and command policies that can still reject a tool under Harness
  WRITE, because backend-native policy layers remain authoritative outside the Harness protocol.
- Move the per-Neovim broker to a shared daemon only if cross-editor live control becomes
  valuable enough to justify process discovery and stronger lease coordination.
- Add provider strategies only through the typed `Backend` contract. Do not scrape PTYs.

---

## 15. Conventions and invariants

- **Async work surfaces failure loudly.** Every external/git/GitHub/AI request reports
  nonzero exits, invalid JSON, and stale-operation errors through `vim.notify` or the
  notification wrapper. "Zero results" must stay distinct from "request failed" — never
  collapse a failure into an empty list or a stuck loading state. The failure path is
  tested.
- **Request-id race guards.** Async lookups (PR detail, about summary, branch diff) stamp
  a request id and discard results that arrive after a newer request, so a slow response
  never overwrites fresh state.
- **Test seams go through `dr()`.** Any function a test overrides via `dr()._x` must be
  called through `dr()._x` everywhere, never the module-local copy.
- **The gutter is virtual text.** Line numbers and signs are inline virtual text, so
  selections, yanks, and searches see only real code.
- **`.lua` files are LF.** Enforced by `.gitattributes` (`*.lua text eol=lf`). Tools that
  write CRLF (e.g. `Set-Content`) corrupt the files — use LF-preserving edits.
- **No plural type names.** Type names name one role — `TaskStore`, not `Tasks`. Use
  singular collection roles for many-valued types.
- **Naming.** No single-letter variables. Long type names shorten to a clear word
  (`FoundationalVectorStore` → `store`), never a letter.

---

## 16. Testing and linting

The headless suite lives in `nvim/tests/forge/` and runs via
`nu nvim/tools/run_tests.nu`. Every test has a 30-second deadline by default. The runner starts
each fixture through an owned child Nushell process, records the fixture's real exit status, and
terminates the owned process tree when the deadline expires. A fixture that prints success but
remains alive reports exit status 124 rather than a passing result. The runner returns a failing
exit status for test or whitespace failures. It preserves production logs.
`tests/forge/mock_backend.lua` injects a fake git backend so
tests never touch a real repo. `diff_architecture.lua` guards the render-engine extraction
boundaries. The native Harness fixtures isolate host lifecycle and recovery, document adoption and
submission acceptance and draft preservation, controller transitions, transport ordering, session creation, provider
catalogues, tool output, saved diffs, and wire-version rejection.

The Rust suite runs with `cargo +1.94.0 test --locked --workspace --manifest-path
nvim/rust/forge/Cargo.toml`. It isolates plan revisions, goal guards,
SQLite current-version session filters, leases, ordered provider file-change attribution,
Gitignored interaction boundaries, divergence refusal, unborn Git worktrees,
failed-provider goal pausing with final checkpoints, provider task normalization,
exact control tools, backend/session compatibility, output-free cancellation retraction,
visible-output and workspace-change retraction guards, and worktree-only rollback. The ignored
`tests/codex_cli.rs` integration uses the installed
authenticated Codex CLI with `gpt-5.6-terra` at low effort and fast mode in a temporary Git repository.
Run it explicitly with `cargo test --manifest-path nvim/rust/forge/Cargo.toml
-p forge-harness --test codex_cli -- --ignored --nocapture`. It verifies model discovery, plan steering, no writes
before plan acceptance, execution, structured goal completion, resume, and native fork without
touching this dotfiles worktree.

The ignored `tests/copilot_real.rs` integration starts the native Copilot SDK in a temporary Git
repository, discovers the authenticated model catalog, chooses a small or fast model when
available, and streams one exact read-only response through the complete broker. Run it explicitly
with `cargo test --manifest-path nvim/rust/forge/Cargo.toml -p forge-harness --test copilot_real --
--ignored --nocapture`.

After automated tests, use Terminal MCP to open `:ForgeHarness` and PlanReview, then exercise `/undo`
and `/sessions` at both 160x48 and 100x30. Inspect rollback confirmation, prompt restoration,
fuzzy filtering, preview replacement,
scope toggling, deletion, resume, focus restoration, winbars, folds, prompt navigation, composer
growth, queue editing, and capability-gated actions in a real PTY.

Diagnostics come from `lua-language-server` (`--check`, configured by `nvim/.luarc.json`).
Most `undefined-field` / `inject-field` volume is the dynamic `dr()` seam, not real bugs.
After a `git mv`-heavy refactor the editor's lua-ls holds **stale old paths** and reports
phantom `duplicate-*` warnings — run `:LspRestart` to clear them (a fresh CLI `--check`
never shows them). See `.rulesync/rules/forge.md` -> Linting for the full triage.

---

## 17. Where to start reading

- **Adding a status feature?** Start at `views/status/status_render.lua` and
  `section_map.lua`, then `actions.lua` for mutations.
- **Touching how diffs look?** Start at `render/diff_render.lua`, then `hunk_model.lua`
  and `syntax_engine.lua`.
- **Working on PRs/reviews?** Start at `views/pr/review.lua` and `pr_overview.lua`, with
  `integrations/gh.lua` for the data.
- **Changing git behavior?** Start at `git/git_data.lua` over `git/git_backend.lua`.
- **Changing Harness behavior?** Start at `client.lua` and the matching `views/`
  directory, then follow the JSONL method into `nvim/rust/forge/crates/forge-harness/src/broker`.
- **Anything cross-cutting?** Shared state lives in `session.lua`. A module's own caches
  live in that module, and `init.lua` only re-exports functions reached through the `dr()` seam.


## 18. Migration editable-region ordering

`forge/editable.lua` owns native region anchors and locally pending full text. Each
physical document owns one monotonic sequence. Each region owns its accepted revision
and latest pending value. Native `on_bytes` validates the changed range, shifts later
anchors, captures the changed field, and suspends generated text synchronously. It
never sends an edit request and owns no debounce timer or acknowledgement state.

Explicit save and submit actions call `capture_draft` to obtain immutable full-text
captures. Captures include document identity, region identity, accepted base revision,
sequence, and exact text. Selection limits an operation to its intended fields.
`saved_capture` advances accepted revisions and clears only pending values whose
sequence matches the completed capture. Newer typing remains pending and dirty.
Rust validates complete capture sets atomically before adopting submitted text.

`forge/draft_comments.lua` owns local comment creation, deletion, focus, collapse,
and the transition between compact boxes and full-width raw editable bodies. Plan
and PR comments share this engine. Source extmarks preserve navigation identities
through local row changes. `forge/draft_source.lua` maps generated source coordinates
to physical rows for syntax, folds, navigation, and selections. The native snapshot
installs diff gutters once. The plan comment projection must not reinstall them.
The plan adapter captures native fold preferences before local comment rendering
and restores them after source rows move. Expanding or collapsing a comment must
preserve folds opened by navigation as well as folds changed through Tab.
Only rows with plan comment targets anchor annotations. Each source identity
receives one comment editor even when several projected rows share its line
number, preventing an empty duplicate editor from overwriting the draft body.
Plan comment bodies attach the shared native editable-region guard. Local
presentation runs with that guard suspended and reanchors the body ranges after
rendering. Counted deletions, joins, and other edits crossing a header, footer, or
source boundary restore the accepted buffer and preserve draft bytes. Plan windows
use the native number column rather than inheriting the transcript's sign-only
status column when a dirty buffer is reopened.

Document adapters serialize explicit operations and retain the latest queued
explicit capture. They must dispatch that retained value without recapturing newer
unsaved typing. Save completion updates accepted baselines without overwriting draft
text. Dirty close hides and retains the buffer. Conflicts preserve the draft for
explicit recovery. Remote operations retain the host's durable mutation queue and
uncertain-outcome recovery rather than replaying publication automatically.

Plan acceptance and revision requests hide the review tab as soon as the explicit
submission is queued and return focus to Harness. The document owner retains the
captured comments until the response arrives. Success releases the owner. Failure
reopens the retained review without discarding unsaved feedback.

Revision requests retain structured comment anchors in the lifecycle record. The
planning prompt presents those comments beside file excerpts from the saved
baseline and proposal. Excerpts include three diff rows before and after each
selected row and merge overlapping context within a file. Each row has one line
number, with `-` for baseline removals and `+` for proposed additions. Unchanged
rows use proposed line numbers. Comments follow as `8: body` or `8–12: body`.
Section and file comments retain their bodies without inventing source lines.
Feedback formatting completes before the broker changes the plan state, so an
invalid saved anchor cannot leave a revision request without its comments.

PlanReview uses `A` to create a `Plan question` through the same draft editor as
`C`. `Ctrl-S` saves captured feedback and submits nonempty unanswered questions.
The shared draft-comment component supports per-item headings and multiple
read-only replies. Focusing any member expands the entire thread to full width.
Answers retain read-only ranges outside the parent's guarded editing region.
Reply updates preserve local draft text and cursor position. The document owner
retains pending and completed replies across source projection refreshes.
`C` and `A` on a message or answer inherit its source range and record a parent
message identity. The core groups those entries into one conversation and expands
all members when any member has focus. Each editable entry keeps its own guarded
body. Linked entries share heading dividers in the focused view and a single
outer border in the compact view. Shared divider hit testing belongs to the
following message, while its predecessor's editable region ends at that divider.
Parent deletion requires removing its follow-ups first.

`plan.questions.answer` captures native review identity and semantic anchors,
then admits a read-only discussion turn in the main provider conversation through
the broker's existing serialized interaction and timeline pipeline. The timeline
shows the user's question and the provider's answer. Its instruction requests one
or two sentences using conversation history, the saved declaration design, and
the selected source excerpt. It leaves plan state and source unchanged. Questions
and answers also persist in the annotation store, with
each answer bound to the question's exact body. Answers retain the exchange's
measured execution duration. The answer divider
renders `Thought for N seconds`, with no invented timing for answers without
recorded duration.
Saving unchanged answered questions makes no new model request. Editing a question invalidates its answer,
and a late response cannot attach to different text. Ordinary revision requests
include only revision comments as requested changes. Follow-up questions and
revision comments also carry their entire saved conversation, including model
answers, alongside the original anchored source excerpt. The annotation store
rejects missing or forward parents and mismatched inherited source ranges.
Row-only annotation migration is unsupported.

Submitted declaration plans retain a file delta between consecutive proposed
snapshots with three surrounding context lines per hunk. Distant edits retain
separate hunks. The first submission compares source declarations with the proposal.
Timeline file headings request recursive expansion through the shared fold
metadata. Opening a file reveals all of its hunks. Opening a change summary
retains collapsed file headings, and individual hunks remain independently foldable.
The transcript renders these deltas through the shared change tree, independently
of the Task, Description, and Validation delta. Metadata revisions compare the plain text
of each section, omit unchanged sections, and retain paragraph breaks. Their
navigation opens that section's immutable text rather than internal JSON.
PlanReview continues to compare source with
the complete proposal. Transcript declaration targets retain the plan identity,
submitted revision, path, and side. `plan.declaration` reads that immutable
snapshot, and navigation opens a read-only declaration buffer. Removed rows open
the previous proposal, or the source baseline on the first submission. Navigation
never substitutes the current working-tree file for a saved declaration.

## 19. Physical buffer replica

`forge/buffer.lua` owns the physical buffer, metadata namespace, applied revision,
changedtick, block sequence, and native editable owner. Feature adapters own their
domain state and explicit operation lifecycle.

Patch preflight checks base revision and counts, changedtick, decoration handles,
disjoint text and block edits, retirement, resulting row coverage, and metadata byte
boundaries. Clean incremental patches share unchanged metadata and avoid copying
unrelated source rows. Application runs synchronously on the main loop. Native
extmarks move with unchanged text. Revision publication follows successful text,
metadata, and row-count checks. Partial API failure marks the session desynchronized
and preserves diagnostic and recovery state without claiming rollback.

Pending local drafts defer generated patches and snapshots. Locally projected
comment buffers also defer ordinary patches until their adapter can rebuild a clean
projection. Adapters coalesce refresh demand rather than mixing local edit patches
with generated patches. Snapshot recovery validates the complete candidate before
replacing text and metadata. Generated mutation detaches native edit callbacks and
reattaches them with the resulting region coordinates. The replica preserves logical
zero-row documents despite Neovim's required physical empty row.

## 20. Host output admission during migration

The root Forge executable now sends broker responses and events through `forge-protocol::outbound`.
Admission reserves storage before JSON encoding. Ordinary frames have a 512 KiB encoded limit.
Compact control records have a 4 KiB limit. One FIFO queue permits 128 outstanding frames with
32 records and 128 KiB reserved for control admission. The 8 MiB encoded-byte budget includes
frames currently being serialized and the frame held by the stdout writer. Dropping that frame
releases its reservation only after writing completes or fails. Reservation uses the maximum
frame size before serialization and refunds unused bytes afterward.

Serialization failure, output saturation, or a closed output channel poisons the connection.
The root input loop observes that failure independently of the writer, including while stdout
is blocked. It stops the connection instead of continuing after a missing event. Reserved control
records remain in the same FIFO and cannot overtake earlier document events. This is a hard
admission boundary, not the final consumer-credit protocol. Large transfers still require framing
and snapshot assembly before they can pass this boundary.

Request dispatch now admits at most 64 tasks per connection. Ordinary requests can occupy 63
slots. One slot remains available for cancellation, restart, approval resolution, or shutdown.
Saturation returns a correlated `busy` response before spawning or retaining another request task.
This bound does not yet cover provider-internal event queues or detached provider preparation.
The backend event channel and consumer receive-credit integration remain migration work.


## 21. Migration snapshot transfers

`forge-protocol::snapshot` produces bounded transfer parts from one encoded snapshot. The native
replica consumes them through `apply_snapshot_part` and `forge/snapshot.lua`. Document identity,
revision, transfer number, contiguous part sequence, part count, and total bytes form the transfer
contract. Each document retains one incomplete transfer. A newer transfer cancels its predecessor,
and late predecessor parts are ignored. Invalid metadata discards partial assembly.

Only a completely assembled and decoded snapshot reaches `apply_snapshot`. Partial delivery does
not mutate the physical buffer or advance its revision. Existing snapshot validation and local-edit
suspension still apply after assembly. Encoded transfers have no aggregate byte cap. Lua concatenation
and decoded structures add transient memory beyond the encoded payload and remain part of
the performance acceptance work. The live host and client transport do not route these records yet.


## 22. Native receive ownership

The live Harness client owns one `forge/receive.lua` instance per process generation. Stdout
callbacks admit bounded encoded frames and retain partial data. They do not decode domain objects
or create per-message scheduled closures. A single scheduled drain decodes and dispatches FIFO
batches of at most 16 frames, normally bounded at 256 KiB. An individual larger frame is consumed
alone. Limits before decode are 512 KiB per frame, 8 MiB pending encoded data, and 128 complete
pending frames. Partial fragments compact after 128 segments.

Consumption accounting occurs after dispatch, allowing future host credit to describe completed
client work rather than bytes accepted by the pipe. The live client now sends cumulative byte and frame consumption through `transport.consumed`.
Overflow or malformed framing stops the connection. Process exit waits behind all admitted complete
frames, so terminal responses cannot be overtaken by readiness cleanup. Unterminated final data
fails the connection. Closing the receive owner discards queued old-generation callbacks. Stderr
retention is capped separately at its most recent 64 KiB.


## 23. Consumer credit and process shutdown

`forge-protocol::credit::ReceiveCredit` gates the live stdout writer before every frame. Its window
contains at most 128 published but unconsumed frame lengths and 8 MiB of published bytes. Lua sends
cumulative byte and frame counts after each receive batch successfully dispatches. Rust verifies
that the new counts acknowledge an exact FIFO prefix. Identical cumulative acknowledgements are
idempotent. Regressing counters, excess frames, and mismatched byte totals reject the connection.
Credit records produce no response and bypass ordinary request admission.

The reader continues accepting consumption records while shutdown output drains. The shutdown
response is an internal terminal frame in the same FIFO as earlier output. The writer flushes it
before returning. This avoids relying on all background producer handles being dropped before the
writer can finish. Outstanding work beyond that explicit shutdown boundary is not replayed.

Process stdin uses a dedicated blocking reader thread feeding a two-slot channel of at most 64 KiB
per chunk. Unlike Tokio's stdin blocking-pool task, this thread cannot keep runtime teardown waiting
on an idle pipe. The thread remains blocked until input or process exit if the OS read cannot be
cancelled. Queue, current-read, and consumer chunks remain bounded. Existing JSONL frame limits
still apply above this input adapter.

Producer-side output admission remains a separate hard bound. A producer can still exhaust its
encoded queue while the credit-gated writer waits. Pausing optional generation and replacing the
provider-internal unbounded event channel remain necessary for complete backpressure.


## 24. Exact wire-version handshake

The client includes `protocol_version` in initialization parameters. The root host requires the
current `forge_protocol::WIRE_VERSION` before feature initialization or provider construction.
A mismatch returns `protocol_mismatch` and exits. Successful initialization includes the same
version in the initial result. Lua compares it with `forge.protocol.VERSION` before readiness and
follow-up requests. Wire version 2 separates the host handshake from `harness.initialize`.

The transport accepts one exact version. Missing, older, newer, and nonnumeric versions do not
select an alternative decoder. Rebuild and restart are the recovery action when local executable
and Lua transport code disagree. Existing Harness feature methods remain unchanged after the
successful handshake.


## 25. Automatic builds and executable ownership

The agent or developer editing compiled Forge inputs rebuilds an existing executable
before runtime verification. When the selected executable is missing, Forge startup
runs Cargo asynchronously and shares the build result with pending callers. Startup
does not scan source inputs, hash executables, or compare build receipts.
`forge.builder.build_command()` describes the Cargo invocation, and `binary_path()` selects its output under
`stdpath("cache")/rust-sidecar/forge/build`.

`:ForgeBuild` is registered before the plugin loads and calls `forge.builder.build()`
to run Cargo even when the executable already exists. Concurrent explicit builds and
startup requests share one build result. Cargo runs from the crate directory to select
its Rust toolchain. Build failures report the complete compiler diagnostic. A successful
build reports the artifact path and requests a Neovim restart to load the rebuilt host.

Forge defaults to the optimized Cargo `release` profile for development and profiling.
Release builds retain level-one debug information. Set `vim.g.forge_build_profile` to `"dev"`
or `"release"` before loading Forge to select the corresponding `debug` or `release`
artifact. Startup builds a missing artifact with the selected profile and `--locked`,
then verifies that the executable exists before launching. Missing Cargo, spawn errors,
compiler failures, and absent build output fail startup with a diagnostic. A later load
can retry a failed build. Protocol handshake errors remain startup
failures and never trigger compilation.

Cargo builds update one notification with an animated spinner, elapsed seconds,
and the latest compiler output line, limited to 240 bytes. A one-second timer keeps
the notification visible while Cargo is quiet. Completion stops the timer and replaces
progress with a success or failure status. Startup errors retain the full compiler output.

Each host asynchronously copies the selected executable into its own directory under
`rust-sidecar/forge/leases`. Running the copy allows Cargo to replace its output on
Windows while existing hosts keep their original executable. The host removes its
copy after process exit or spawn failure. If startup is cancelled during copying,
the completion callback removes the unused copy without launching a process.
Existing hosts adopt a rebuild only after they stop and a new host starts.

`:ForgeStartupLog` opens `stdpath("cache")/rust-sidecar/forge/startup.log`. Diagnostics
record executable selection, copy duration, host spawn and initialization, request
dispatch, and status response/application timings. Native observation timing splits
line-statistics source acquisition into staged and worktree microseconds. It also
records batch preparation, verification, and diff computation, plus source reads,
compared pairs, fast-count pairs, cache hits before acquisition, count-cache metadata time,
analysis-cache hits, retained and skipped analyses, and
total input bytes. Unchanged sides perform no source acquisition or comparison.
These are aggregate counters with no per-file log writes.
Every entry records a monotonic microsecond timestamp and cumulative synchronous
logging time. Document identifiers connect command/open, request dispatch, native
timing, snapshot application, and readiness. Diagnostic writes disable per-entry
`fsync` so tracing does not force a disk flush on the startup path. Request identifiers connect outbound
requests to frame reception and assembled JSON decoding. Receive summaries report
bytes, frames, total frame decoding time, and maximum frame queue wait. Queue waits
overlap across frames and must not be summed as elapsed startup time.
`status.first_redraw` records command-entry-to-redraw and ready-to-redraw durations
after a decoration provider observes the populated buffer in a displayed window.
It splits the latter into `ready_to_redraw_start_us` and `redraw_us`, separating
work before the provider's redraw-start callback from the measured redraw cycle.
`status.callback.finished` marks completion of the initial snapshot callback and
request-queue advancement, including their combined elapsed time.
This boundary measures Neovim's redraw completion, not physical terminal display.
Hidden test buffers produce no redraw marker. The eagerly registered `ForgeStatus`
command captures entry before explicitly loading the lazy plugin. Its timestamp
passes into the open handler, so the total includes first-use plugin loading.
Direct Lua `require("forge").open()` calls begin timing in the open handler.

For diagnostic comparisons, set `vim.g.forge_skip_line_stats = true` before the
host starts. The client passes `--diagnostic-skip-line-stats` to the executable.
Display observations then leave counts unknown and skip count-cache lookup, source
acquisition, and prepared analysis retention. Native timing records the bypass and
eligible/skipped file sides. Expansion still acquires its required content, and
verified mutation settlement does not use this bypass. The default is false.
Changing this flag requires a fresh host. Close Forge views and stop the client,
or restart Neovim, before comparing modes.
An unresolved status open emits
a waiting entry every five seconds until completion or closure. Entries identify
the Neovim process and exclude request bodies. The log resets at 2 MiB, bounds
individual entries, and cannot fail startup if writing fails. Opening status buffers
display `Loading Forge status…` until their initial request settles.

## 26. Bounded normalized provider event delivery

`forge_harness::backend::events` owns normalized event delivery from providers to primary and
child reducers and from reducers to the root transport forwarder. The queues use the protocol's
encoded admission mechanism. Each queue admits at most 96 ordinary records with a 512 KiB
individual encoded limit and an 8 MiB budget that retains the existing 128 KiB control reserve.
Serialization reserves capacity before allocating the encoded record. Decoding occurs at the
consumer, so queued records do not retain arbitrary provider object graphs.

Send operations remain synchronous and never wait for presentation capacity. Exhaustion poisons
the affected queue and wakes its consumer. This failure remains observable when a provider
callback discards the send result. Healthy delivery preserves FIFO order without replacing deltas.
Saturation terminates delivery explicitly rather than promising lossless retention under unlimited
production. Approval responses and cancellation do not wait for room in these event queues.

Primary and child reducers observe delivery errors and stop the affected provider transport.
The primary reducer uses its existing failed-interaction and paused-goal persistence path.
After a provider returns, the reducer checks queue failure again before accepting completion,
including failure encountered while draining the remaining events. The root forwarder propagates
both receive and transport-send errors. Timeline emission also propagates admission failure.

These limits cover normalized event queues, not the entire provider runtime. Provider SDK buffers,
retained transcript/output collections, steering and Copilot control-tool queues still need their
own bounds. Credit-driven suspension of optional output generation, shared-host feature routing,
and complete snapshot-transfer routing remain separate work.

## 27. Bounded active-turn control admission

`SteeringLane` admits 32 ordinary requests and reserves one additional request for a targeted
interrupt. A command retains its permit after dequeue until provider acknowledgement or command
drop. Both operation classes share one FIFO receiver. Ordinary saturation cannot consume the
interrupt permit. Excess requests fail immediately, without creating queue-capacity waiters.
Steering text is limited to 64 KiB and each target identifier to 4 KiB. Closing the active receiver
fails queued acknowledgements and releases its generation's admission state. A dequeued command
retains its acknowledgement owner until completion or drop.

The Copilot control router uses a 32-slot bounded channel. It reserves a slot before validating
the invocation's encoded size or invoking `ControlToolRuntime`. The invocation must encode within
256 KiB. Reservation failure therefore precedes any plan or terminal-state mutation. Validation
failures and calls without a routed result release their reservations. Accepted invocations retain
their order in the receiver. These limits count queued and reserved invocations. A dequeued
invocation and the runtime's retained plan state have separate lifetimes.

No unbounded Tokio channels remain in the root or Harness source. This inventory does not prove
bounded total memory. Provider SDK buffering, accumulated transcript/output collections, decoded
object overhead, and retained runtime documents still require separate resource accounting.

## 28. MCP process framing

`forge mcp` reads through `ThreadInput` and `JsonLineReader`, matching the root host's 512 KiB
input frame limit before JSON decoding. Fragmented input remains in one reader until complete.
Oversized input terminates the process even when its parent keeps stdin open. Truncated JSON at
EOF fails decoding without writing a response fragment. A complete final JSON value at EOF keeps
the shared reader's existing behavior.

The stateless MCP loop processes one request at a time and encodes each complete response through
the bounded output encoder before writing. If a result exceeds 512 KiB including its newline, the
loop emits an error with the original request ID and remains available for subsequent requests.
If that error cannot fit, including an oversized request ID, the process fails without writing a
partial frame. Response construction still materializes JSON values before encoding. The input
bound limits the echoed arguments but does not eliminate those intermediate copies.

This entry point remains the existing stateless control-tool transport. It does not create a
Harness session or launch a provider. Notification handling and normal tool response shapes remain
unchanged.

## 29. Harness representation and evidence boundaries

Submitted Codex turns own their request through a box, and plan-execution timeline interactions
own their interaction record through a box. Inactive enum variants no longer reserve storage for
the largest payload. Serialization continues to emit the underlying request and interaction data
without an additional wire object.

Plan mutation uses a `ResourceSchema` descriptor for collection identity, semantic keys, renaming,
and preparation. The descriptor keeps each resource's naming and identity rules together while
the mutation algorithm still validates and applies the same ordered edits. Semantic execution
accepts phase completion through its broker-owned control channel. Completion requires a fresh
semantic comparison and a verification assessment with retained tool evidence.

Wrapped plan lines accept their first-line and continuation prefixes as a pair. Entity rendering
derives the entity from its document index instead of accepting a second independently supplied
entity reference. Cargo source resolution names its package/version lock map without changing
lock ownership or retention.

## 30. Shared Git repository identity

`forge-git` now owns discovery of worktree roots, private Git directories, common Git storage,
and index paths. `WorktreeId` and `GitStorageId` are distinct opaque types. Linked worktrees share
storage identity while retaining separate worktree identities and index paths. Bare repositories
have storage identity without a worktree. Unborn repositories do not need an existing index.

Harness workspace discovery calls this library directly instead of launching `git rev-parse`.
No repository produces an untracked workspace. Configuration and I/O errors propagate rather than
being converted to an untracked workspace. `gix` 0.87.1 uses disabled default features with SHA-1
and SHA-256 enabled. The path/discovery probe compiles with that configuration. `dunce` preserves
canonical path identity while avoiding unnecessary Windows verbatim prefixes in workspace paths.

`RepositoryPath` retains raw Git bytes independently of its display label. Empty, NUL-containing,
absolute, and dot-component paths are rejected. Native argument conversion preserves exact bytes
on Unix and rejects unrepresentable or ambiguous Windows paths. It returns one argument without
shell quoting. Future Git command owners must still disable pathspec interpretation explicitly.
Parent validation detects existing paths that escape the root without following the leaf symlink's
contents. This check does not replace mutation admission or eliminate filesystem races.

The new library does not yet own status observations, source acquisition, completion, a shared
repository store, or Git writes. Lua status and mutation implementations remain active until those
services and their callers migrate. Unix byte-path and symlink fixtures require macOS or Unix
execution before cross-platform acceptance.

## 31. Blocking repository read ownership

`forge_git::read_pool::BlockingReadPool` bounds admitted jobs and their declared retained input
bytes. Admission counts queued jobs, running jobs, and completed results awaiting collection.
Over-capacity requests return typed errors immediately, without accumulating capacity waiters.
Each worker owns a reservation that survives caller cancellation, waiter drop, and timeout.
Successful and failed results carry their reservation until collection or disposal. Abandoned
results drop before their reservation releases. Native panics release admission during unwinding.

`ReadTask` owns result delivery and requests cooperative cancellation when dropped. Workers check
cancellation before calling the operation and after it returns. Result collection checks it again,
so cancellation can discard a completed but uncollected result. Source readers still need to check
the token between interruptible acquisition stages. This cannot stop a native call already blocked
inside the operating system.

Shutdown closes admission, requests cancellation, and waits for admission release until its
deadline. It returns identities of retained jobs, including completed but uncollected results.
A later shutdown call can observe worker exit and result disposal. It does not terminate Tokio
blocking threads or discard a result still owned by its waiter.

The repository store and Harness checkpoint source reader use this pool. Input accounting relies
on the caller's declared retained size. Source allocation and output byte limits remain the
operation's responsibility. Result count remains bounded through collection, while unified source,
cache, and output byte accounting remains acceptance work.

## 32. Shared repository handle lifetime

`RepositoryStore` retains at most its configured number of repository handles. Discovery runs on
the bounded read pool and returns canonical identity with the same opened gix handle. Concurrent
discoveries deduplicate by worktree identity. Bare repositories deduplicate by storage identity.
Linked worktrees retain separate handles and index scope while sharing storage invalidation.

Clients hold `Arc<RepositoryState>` leases. At capacity, admission evicts the least recently used
entry that has no external lease. If every entry is leased, admission returns an error. Each native
read retains its own lease until its blocking closure exits, even after caller cancellation.
Discovery uses the same read reservation through result collection. It does not maintain a
separate semaphore. Temporary discovery handles remain bounded after native execution ends.

The gix `parallel` feature permits shared thread-safe handles. A read creates worker-local gix
state inside its blocking closure. Results carry the repository generation, and collection rejects
results superseded by an observed invalidation. Feature owners must check the generation again at
final adoption. Storage invalidation advances every retained linked-worktree generation without
invalidating unrelated storage. This does not yet detect external Git changes automatically.

Shutdown closes admission and delegates cancellation and unfinished-worker reporting to the read
pool. Idle eviction never removes leased entries. The host uses the store for startup discovery,
and Harness checkpoint reads share its read admission. Handle count and declared input budgets do
not bound gix internal caches or source bytes. Operation-specific source limits remain necessary.
Observation caches and complete mutation integration remain incomplete.

## 33. Host repository ownership

`ForgeRuntime` constructs one shared repository store without opening a repository or provider.
The current host defaults allow 32 retained repository handles, four admitted native reads, and
32 MiB of declared retained read inputs, including supplied source allocations. These are admission settings, not measured cache
or process-memory bounds.

After wire-version validation, the Neovim host discovers its initial workspace through this store
and retains the repository lease for the connection lifetime. Harness receives the resolved
workspace in its shared `BrokerRuntime`, so controller initialization reuses that workspace and
permission scope. Standalone Harness constructors still resolve their workspace synchronously.
Session listing and child snapshot discovery also remain separate migration work.

Discovery or provider initialization failures produce a correlated initialization response.
Repository discovery fails before provider and session storage initialization. Every normal or
error return from the connection loop invokes repository shutdown, which closes admission and
waits up to two seconds before reporting unfinished native reads as a host error. This is not yet
the full provider, write, storage, and client lifecycle shutdown policy.

The `mcp` entry point remains separate and does not construct `ForgeRuntime`. Diff, status, review,
and GitHub owners still need to join the runtime, and Harness repository tools still need shared
repository and analysis services.

## 34. Executor termination boundary

Both executable entry points run through `shutdown::run`. Service cleanup returns its result
before executor teardown begins. The executor waits up to 100 milliseconds using Tokio's
`shutdown_timeout`, then releases the calling thread even if native blocking work has not returned.
Normal errors propagate unchanged. Panic unwinding also tears down the executor with this bound
before resuming the panic.

The timeout does not terminate native work. Such workers can run until the process exits, so
feature owners must report unfinished operations and persist required outcomes before returning.
The existing repository shutdown still has its separate two-second reporting deadline. Provider,
write, and durable-storage ownership remain incomplete.

A subprocess regression starts a native worker that never returns and verifies process exit
within five seconds with the reported error preserved. Its child fixture is excluded from direct
suite execution and invoked by the parent test, so an unbounded teardown regression cannot hang
the main test process.

## 35. Mutation admission and owner lifetime

Each `RepositoryStore` owns one `MutationCoordinator`. Index and worktree-file scopes use
`WorktreeId`, while shared-reference scopes use `GitStorageId`. Admission reserves queue and
receipt capacity before asynchronous preparation. The current store limit is 64 retained
operations, each with at most 64 distinct scopes, and 16 MiB of declared retained preparation inputs.
Completed receipts and their input reservations count toward capacity until their consumer
acknowledges them with `take_receipt`.

An operation can start only after every earlier operation with an intersecting scope finishes.
An unrelated scope can proceed independently. This ordering includes earlier queued operations,
so a later operation cannot bypass one waiting to acquire several scopes together.

Queued cancellation records `CancelledBeforeStart`. Running cancellation sets a request flag
without releasing scopes. The process owner retains `AdmissionGuard` until termination is known
and records completion before release. Dropping a running guard marks the operation abandoned and
quarantines its scopes for the remainder of this coordinator's lifetime. Shutdown reports those
identities instead of claiming completion. No automatic recovery releases abandoned scopes yet.

The coordinator currently records operation-level termination, not per-target write facts.
Production Git writers, exact source preconditions, durable receipts, and settle/recovery still
need implementation and integration.
Harness checkpoint rollback enters this coordinator. Other Harness and Lua writers still need routing.

`wait_start` registers one queue owner synchronously and returns a `Send` future. Dropping that
future before or after polling cancels its queued operation and retains a cancellation receipt.
Duplicate waiters and synchronous starts against a registered waiter are rejected. A watch channel
coalesces state-change notifications, so completion, cancellation, and shutdown wake waiters without
a notification queue or polling loop. Every wake rechecks scope ordering under the state lock.

The future transfers ownership directly into `AdmissionGuard` when scopes become available.
Running owners can await `cancelled` to receive cancellation without polling, but still must
terminate and reap their process before recording completion. Watch notification does not release
execution ownership.

Mutation admission checks operation count and declared input bytes independently under one lock.
A rejected byte reservation consumes neither an identity nor a queue slot. Charges remain through
queued cancellation, running cancellation, process completion, and abandoned-owner quarantine.
Only adoption of a terminal receipt releases its input charge. `usage` reports both retained
operation count and input bytes from one locked snapshot. Writers must reserve an upper bound before
retaining patch, path, and precondition inputs. This declared accounting does not measure native
Git allocations, allocator overhead, or memory retained after receipt adoption.

## 36. Harness checkpoint rollback admission

The host passes its repository store into the shared Harness runtime, and every session controller
retains that same store. Standalone Harness construction creates its own store with the centralized
default limits. `exchange.rollback` now acquires the admitted worktree's index and file scopes
plus its shared-reference scope before executing checkpoint source validation or filesystem writes.

`CheckpointRestore` retains the exact expected and target checkpoint records, repository lease,
operation identity, and running guard. It charges their retained metadata capacities during
admission. A cancelled queued restore adopts its cancellation receipt, releasing its slot and
input reservation. Dropping an admitted restore before execution records failure without claiming
a write occurred. Panic during execution leaves the guard to quarantine the operation.

After admission, rollback rechecks HEAD, the index digest, and expected worktree content. It also
checks that the checkpoint workspace resolves to the admitted worktree. An external edit while
queued therefore refuses restoration after handoff. Completion invalidates mutable repository
reads before releasing scopes. A restore error remains uncertain because filesystem writes can
have partially completed. Existing interaction-state updates run only after restoration succeeds.

Checkpoint rollback now runs its file acquisition, Git subprocess calls, and restoration on a
blocking worker. Other checkpoint capture and diff callers still execute synchronously. Temporary
content buffers and SQLite decoding are not covered by the metadata reservation.
Per-target durable outcomes, permission integration, process termination, and recovery remain
incomplete. This first writer integration does not establish shared ownership for all writers.

## 37. Checkpoint worker and object-store ownership

`ObjectStore` owns the content-addressed checkpoint directory independently of SQLite. `SqliteStore`
retains a cloneable object-store handle, and checkpoint capture, restore, and diff APIs consume
that handle directly. A restore worker never opens another SQLite connection or borrows one from
its calling controller.

Object publication writes and syncs a temporary file in the destination directory, then persists
it without replacing an existing identity. Concurrent publishers verify the winning file's hash.
Reads verify content hashes before returning bytes. Existing-object verification streams through
a 64 KiB buffer. Directory-entry durability across power loss and total object-read allocation
bounds remain separate storage requirements.

`CheckpointRestore` owns its expected and target records and moves them, its repository lease,
and its admission guard into `spawn_blocking`. Dropping the waiting future requests cancellation
without aborting the worker or releasing its scopes. Cancellation before worker execution skips
restoration. After filesystem work starts, the worker finishes that operation and reports its
outcome. Panic quarantines the abandoned mutation through guard drop.

Normal result collection acknowledges the receipt. If completed restoration loses its waiter,
the terminal receipt and its conservative input charge remain in the coordinator for reconciliation.
Automatic reconciliation and durable per-target outcomes are still incomplete. Tokio blocking
work remains bounded by mutation admission, not by the repository read pool. Dedicated write-worker
concurrency and subprocess termination policy still need implementation.

## 38. Streamed checkpoint source acquisition

Checkpoint capture calls `ObjectStore::put_file` instead of allocating each complete file. The
source reader hashes through a 64 KiB buffer and reads at most the initial file length plus one
byte. Shorter or longer input fails acquisition. Before and after stamps compare length, modification
time, available creation time, and read-only status. Unix stamps also compare device, inode, and
mode. These checks reject observed changes and do not provide an atomic filesystem snapshot.

An already stored hash is verified without rewriting its object. A new object uses a second bounded
source pass into a temporary file. Both byte hashes and final source stamps must agree before
publication. Temporary files are removed on failure. New-object acquisition uses source-sized disk
space, but does not allocate a source-sized byte vector.

Object retrieval for diff still allocates complete objects. Git listing output,
checkpoint metadata, and other synchronous capture work retain their existing bounds or limitations.
This change establishes a per-file streaming acquisition buffer, not a bound on total checkpoint
memory, disk usage, or end-to-end latency.

## 39. Streamed checkpoint restoration

`ObjectStore::restore_file` verifies stored bytes into a private temporary file, then
replaces each destination from a same-directory temporary file. The transfer buffer is
64 KiB. This prevents an interrupted write from leaving a truncated destination. The batch
remains interruptible between files and retains durable progress rather than claiming
multi-file atomicity.

## 40. Checkpoint restore preview and recovery

`exchange.rollback.prepare` computes only paths changed between the expected and target
checkpoints. Unrelated later files remain untouched. It acquires recovery copies of affected
current files, resolves target objects, and saves an immutable preview outside the worktree.
A changed HEAD or later edit adds an overwrite warning rather than prohibiting restoration.
The confirmation has Cancel selected by default and pages warning lists in groups of 50.
The apply request carries only the preview ID.

`exchange.rollback.apply` revalidates the affected paths and HEAD under repository mutation
ownership. Staging changes do not invalidate a preview because restoration never writes the
index. Changed files or HEAD require a new preview and confirmation. Unsafe paths, symlink
ancestors, missing objects, and unsettled exchanges reject before mutation.

A saved `RestorePreview` contains original and target identities. A separate constant-size
progress journal records Applying, FilesComplete, or Complete. Both publish with file sync
and atomic replacement. Each file operation compares its actual state with original and
target content, so restart handles a crash between replacement and progress publication.
A concurrent third state stops with a visible error. Empty directories are removed only
when required to restore a file and only when empty. Symlink targets are captured as data.

New workspace work is blocked until a pending restore is resolved. Harness undo offers
Continue interrupted restore and Restore pre-restore files. A completed restore also offers
Undo last restore. Recovery does not resurrect a task or provider execution. It restores
file content, while exchange history retains the completed rollback disposition. A failed
history or provider finalization leaves FilesComplete recoverable and does not repeat file
writes unnecessarily. Confirmation after later edits warns again. Git's index, branches,
commits, and ignored untracked output remain outside the mutation scope.

The checkpoint format intentionally has no legacy decoder. Opening the new store removes
sessions owning old complete-file checkpoint records through SQLite foreign-key cascades.
Preferences, prompt history, and provider-owned sessions remain intact. Immutable object
cleanup is separate from format reset.

## 41. Checkpoint diff source admission

Checkpoint diff generation admits at most 8 MiB of content per source side, matching the migration
source-input limit. `ObjectStore::get` requires an explicit byte limit and returns an unavailable
result before reading or allocating content when the stored length exceeds it. Accepted reads
reserve the initial length, use the bounded reader, verify the digest, and reject observed
descriptor metadata changes. Empty objects are valid with a zero-byte limit.

An oversized source produces a `Diff unavailable` entry for that file while other changed files
continue to generate diffs. Capture and streamed restoration retain the complete object, so preview
availability does not disable rollback. Oversized objects are not hash-verified by the preview read.

This bounds retained source bytes to 16 MiB per file pair. It does not bound the diff algorithm's
line indexes, working memory, computation time, accumulated output, or concurrent callers. Shared
diff-engine admission and typed availability delivery remain migration work.

## 42. Shared immutable diff sources

`forge-diff` owns source contracts without dependencies on Git, Harness, status, or Neovim.
`SourceVersion` retains immutable bytes, a lazy SHA-256 content identity, a representation, and derived
newline metadata. Raw, Git-canonical, and display-only representations produce distinct analysis
identities even when their byte hashes match. Clones share the original allocation and one
thread-safe identity cell. The first identity request computes its hash once.

Construction admits byte length and retained vector capacity against the 8 MiB limit without
hashing. NUL-containing bytes and invalid UTF-8 produce distinct rejection results. Metadata counts
LF and CRLF without normalizing bytes, treating bare CR as content. Empty files contain zero source
lines, and a final newline does not create another line. `from_declared` verifies acquisition claims
against both the content hash and derived newline metadata.

Harness checkpoint diffs now consume these sources and use the shared source limit. Binary sources
retain a binary-difference entry, while unsupported encodings receive an explicit unavailable entry.
Source validation does not authorize writes or canonicalize checkpoint bytes.

The crate currently establishes the source boundary. Histogram comparison, raw action hunks, lazy
display rows, shared caches, worker admission, and syntax analysis remain unimplemented. Harness
continues to use its existing comparison algorithm after shared source validation.

## 43. Exact raw changes

`forge-diff::raw` uses pinned `imara-diff` 0.2.0 histogram comparison and its line postprocessing.
Line tokens retain newline separators. `RawDiff` owns the immutable source pair and ordered raw
hunks with half-open line and byte ranges. Hunk identities include both source hashes,
representations, and exact byte ranges, independently of display context or preview limits.

Empty-side additions and deletions take the synthetic path before tokenization or comparison.
`patch_body` accepts only a hunk retained by the owning result and only Git-canonical source pairs.
It emits all changed bytes with removal/addition prefixes and explicit missing-final-newline markers.
This does not authorize a Git write. The repository owner must still verify paths, modes, attributes,
source generations, and mutation preconditions.

The initial Windows release measurements cover 25 samples each of three synthetic 5,000-line
comparisons. Median/p95 times were 339/397 microseconds for one unique-line change, 217/252 for one
repeated-line change, and 196/256 for a complete repeated-line replacement. Measurements include
validation, temporary line indexes, interning, comparison, postprocessing, and hunk construction,
but exclude source construction and destruction of the returned result. They do not establish
end-to-end latency, peak memory, or cross-platform performance acceptance.

Shared analysis admission, cancellation, retained-result accounting, lazy display, and live Harness
comparison cutover remain incomplete. Temporary indexes, interning, and patch output require their
own memory accounting beyond the source-byte limit.

## 44. Bounded display cursors

`DisplayCursor` owns one immutable raw result and one fixed context setting. Context grouping merges
touching windows while retaining contributing raw identities. Status uses the compact cursor mode,
which retains context rows between nearby raw hunks in one action group but omits unchanged rows at
the leading and trailing edges of each group. A cursor tracks source line and byte
positions monotonically, skipping hidden gaps without constructing an all-file display-row array or
another complete source-line index.

Each delivery caps source rows at 256 and generated text at 128 KiB, counting one row separator byte
per emitted row. The decoration budget reserves one slot per row for its base diff style. Additional
syntax or intraline spans are not yet emitted. Zero budgets preserve delivery state. A row that fits
the total byte limit but not the remaining batch budget remains pending for the next call.

An oversized source line terminates delivery with its source coordinate and byte length before
allocating a display string. Lines are never split into artificial source rows. Returned text omits
the diff gutter and LF/CRLF terminator. Bare CR content remains intact. Rows carry old/new source
coordinates and the exact raw identity for added or removed content.

Structural rows, full-file preview policy, syntax and intraline spans, document memory accounting,
wire delivery, and live viewport demand integration remain incomplete. The row and text bounds here
do not establish a bound on group metadata, shared analysis memory, or serialized transport bytes.

## 45. Full-file preview and speculative eligibility

Display cursor construction now requires the caller's explicit `BodyKind`. Repository ownership
supplies that classification, since empty bytes alone do not distinguish deletion from an emptied
tracked file. Added/deleted previews count source lines and reject counts above 1,000 before group
metadata or display rows are allocated. Modified-file expansion remains independent of this gate.
An unavailable preview retains the raw analysis, source identities, and canonical patch bodies.

`BodyPolicy::may_prewarm` requires a nondeleted kind, a known changed-line total below 100, known
byte sizes within the source limit on both sides, and available speculative capacity. It does not
acquire sources or compute missing counts. Policy overrides can lower the thresholds but cannot
raise them above the migration limits. Eligibility does not reserve capacity, so the scheduler must
still perform atomic admission before starting work.

Typed unavailable reasons distinguish a full-file line limit from a single-line byte limit.
The pure speculative eligibility function is not yet connected to the live feature scheduler.
Editor delivery and runtime body-kind classification remain part of feature integration.

## 46. Bounded intraline analysis

`compare_replacement` admits at most 64 positionally paired replacement lines, 16 KiB of combined
UTF-8 input, and 256 output spans. Policy values can lower those limits. Unequal line counts fall
back to base added/removed line styles instead of guessing correspondence. Admission precedes
character indexing and comparison, and embedded LF or NUL bytes reject the line input.

The pinned histogram implementation compares Unicode scalar values. Character offsets map results
back to exact UTF-8 byte ranges. Insertions and deletions emit spans only on the side containing
changed bytes. If span capacity is exhausted, the result discards all earlier emphasis in that
replacement and explicitly requests line-style fallback. Base diff backgrounds remain independent
of whether intraline work succeeds.

The analysis API is not yet connected to display rows or decoration delivery. Scalar boundaries
do not establish grapheme-cluster grouping, and bounded input does not establish a wall-clock
deadline. Shared scheduling, cancellation, and integration with per-chunk decoration accounting
remain required before editor cutover.

## 47. Display emphasis and decoration admission

Display rows now carry intraline byte ranges and an explicit fallback reason independently of
their base added/removed kind. The cursor computes emphasis on first demand for a raw hunk and
retains only that hunk's bounded result across delivery batches. It checks raw pair counts and
combined byte lengths before allocating line-reference arrays or invoking character comparison.

Each row charges one base-style decoration plus its emphasis span count. A row that exceeds the
remaining batch capacity stays pending. If its full emphasis cannot fit the total decoration budget,
the cursor emits its base style with a decoration-limit fallback, avoiding indefinite retry.
Returned chunks report their actual decoration charge.

CRLF terminators are removed consistently before comparison and display, so emphasis offsets
address displayed UTF-8 bytes. Oversized or unpaired replacements retain full display content with
base styles. The cursor does not cache all file emphasis or rerun accepted analysis merely because
a replacement crosses a batch boundary.

These records still need wire serialization and Neovim decoration integration. Shared worker
scheduling, cancellation, syntax spans, and retained-memory accounting remain incomplete.

## 48. Analysis cache ownership and reservations

`AnalysisCache` reserves source capacity, result capacity, and one entry before work is admitted.
Defaults are 128 MiB of sources, 64 MiB of results, and 512 admitted entries. Reservations cover
pending work and remain attached to published consumer handles. Cache eviction does not release a
charge until the last such handle drops. Admission evicts least-recently-used entries only when
the cache is their sole owner, preserving reuse of results with live consumers.

Publication rejects reservations from another cache, mismatched source identities, and retained
capacities above the reservation. Raw result accounting includes the result structure and hunk
vector capacity. Source accounting uses vector capacity and charges a shared old/new allocation
once within a result. Separate results conservatively charge shared source storage again.
Reservation excess remains charged until release.

This cache currently covers immutable raw comparisons. It does not account for temporary algorithm
allocations, allocator overhead, syntax trees, captures, patches created after publication, or
source/result clones made outside charged handles. The engine must preserve handle ownership across
consumers and reserve worker memory separately. Cache integration with engine jobs and display
cursors, source-level deduplication, and separate tree/capture accounting remain incomplete.

## 49. Display cursors retain charged analysis handles

`DisplayCursor` now requires an `AnalysisHandle` instead of an uncharged `Arc<RawDiff>`. The handle
provides immutable access to the raw result while retaining its reservation for the full cursor
lifetime. Finishing delivery does not release the result while the cursor remains open. Dropping
the last cursor releases its charge only when no cache or other consumer still owns the handle.

Display and body-policy fixtures now publish their analysis through the cache before opening a
cursor. A lifecycle regression opens two cursors, evicts their shared cache entry, and verifies
that both can render while new admission remains saturated until the last cursor drops.

This closes the cursor's direct-reference accounting bypass. Generated row ownership, per-cursor
group and emphasis storage, independent source/result clones, worker memory, and live feature
integration still require their own accounting and lifecycle work.

## 50. Shared asynchronous raw comparison engine

`ForgeRuntime` owns one `DiffEngine` with four active-job slots. Construction opens no repository
and requires no executor. `compare` checks the cache and joins matching in-flight source identities
before reserving another job. New work reserves source retention and a conservative maximum hunk
capacity before reserving dedicated pool admission. Returned handles retain cache reservations.

Each waiting caller owns an interest token. Dropping one waiter leaves other consumers active.
One analysis admits at most 64 waiting callers. The next caller receives `ConsumerLimit` without
subscribing or retaining its duplicate source. Releasing one interest permits another caller while
at least one existing consumer remains. Once the count reaches zero, the job retires and matching
requests wait until retirement releases the slot. A retiring job cannot gain new
consumers after its worker has committed to cancellation.

Diff and syntax callers register a capacity notification before attempting admission. Temporary
job-slot pressure suspends both callers, while input-byte pressure applies only to diff work.
Completion wakes the caller to recheck cache coalescing and admission. Closing either analysis
owner wakes its pending callers without closing the other owner's shared pool. Syntax deadlines
also bound admission waits. Dropping a waiting future cancels that caller without starting native
work. Disabled workers and closed owners remain explicit errors. Diff admission also rejects
oversized input and retained-memory exhaustion. Caller-owned sources waiting for capacity remain
outside pool input accounting.

After the final waiter drops, queued work is removed and its inputs are released. Running work
remains charged until native execution exits. An already running comparison
finishes, and its unneeded result is discarded. Native panic releases admission and reports a
worker failure. Closing the engine rejects new requests while accepted work remains owned.

The native work item owns its completion guard and reservation. Successful return publishes under
the engine lock. Panic unwinding reports `WorkerFailed`. Cancelled or expired unstarted work reports
its stop reason. All retirement paths release input bytes before their reservations. This cleanup
does not depend on an async completion task. Native execution and bookkeeping remain available
after the caller's Tokio executor has been destroyed.

Host shutdown closes analysis admission and drains repository reads and analyses concurrently under
the two-second shutdown deadline. Completion notifications wake drain waiters. Deadline expiry
reports remaining jobs and unjoined worker threads without releasing native work or its reservations.
The reservation uses a conservative hunk-capacity estimate based on source line counts, so it can
reject a large source with few actual changes. Temporary algorithm allocations, caller-owned input
acquisition, independent clones, and per-consumer metadata need separate accounting. This is not a
hard process-memory bound or preemptive cancellation of the native histogram algorithm.

Harness checkpoint comparisons use the shared host engine. Status and other feature comparisons
have not yet migrated to that owner.
Syntax scheduling, feature-level priority selection, source deduplication, and accounting for
completed-but-unconsumed responses remain incomplete.

## 51. Dedicated analysis worker ownership

`AnalysisPool` owns lazily started `forge-analysis-*` threads. Pool limits bound worker count, total
reserved/queued/running jobs, and declared input bytes. `reserve` obtains admission before a caller
transfers its input closure. A dropped permit returns admission. An accepted permit can submit after
admission closes, and shutdown retains workers while such reservations remain outstanding.

Foreground, visible-enrichment, and speculative queues each preserve FIFO order. Workers select
foreground before visible work and visible work before speculation. Priority does not interrupt
running native code. `DiffRequest` carries an explicit priority to `DiffEngine::compare`, with four
job slots and at most four dedicated threads in the host. Live feature demand has not yet migrated
to this request interface.

Cancellation tickets belong to one pool and one submission. Cancelling queued work removes its
closure outside the pool lock, allowing feature cleanup to reenter its owner. Cancelling running
work sets its shared signal without releasing its job or input charge. `WorkBudget::check` exposes
cancellation and an optional deadline at cooperative checkpoints. Expired work is rejected before
admission or discarded before execution. The raw histogram operation still has no internal
checkpoint and cannot be interrupted during comparison.

Native execution and input disposal run inside the worker's panic boundary. A panic releases the
job lease and leaves the thread available for subsequent work. Pool shutdown closes admission,
drains accepted jobs, and joins finished threads within the remaining deadline. Its report separates
job/input usage from the number of unjoined workers. The host rejects a clean-shutdown result when
either analyses or analysis threads remain unfinished.

Worker-local parsers, syntax scheduling, algorithm temporary-memory accounting,
and live feature routing remain incomplete. Declared input bytes do not bound arbitrary closure
captures or temporary native allocations.

## 52. Priority promotion for shared comparisons

`DiffRequest` owns a source pair and the caller's foreground, visible, or speculative priority.
Priority affects scheduling without changing immutable analysis identity. Cache lookup and
in-flight coalescing therefore reuse the same result across callers with different priorities.

An analysis records the highest priority observed from an admitted consumer. A later foreground
consumer raises matching queued speculation through `AnalysisPool::promote`, retaining its original
job slot and byte reservations. Promotion appends to the destination queue after existing work at
that priority. Equal or lower requests do not reorder the queue. Foreign, stopped, running, and
retired submissions cannot be promoted.

Scheduling state retains a promotion that arrives before the initial pool ticket is published.
The ticket publisher applies the recorded priority after enqueueing. Scheduling updates occur
outside the engine lock because queued cancellation can reenter engine retirement. Priority is
monotonic for one accepted job, including after the consumer that raised it leaves. Cancellation
still removes work only after its final consumer leaves.

Promotion does not interrupt native comparison or bypass admission limits. Live status/review
demand, speculative eligibility checks, worker-local syntax, and temporary allocation accounting
remain incomplete.

## 53. Shared Harness checkpoint comparisons

`ForgeRuntime` passes its `DiffEngine` into `BrokerRuntime`, which shares that owner with every
session controller. Checkpoint previews submit exact raw source pairs at foreground priority.
Checkpoint and provider-attributed previews reuse cached analysis for equal source identities.
They no longer run a separate `similar::TextDiff` comparison.

Interaction finalization awaits shared comparison through checkpoint population and turn setup.
Captured checkpoint records are saved before the analysis await, preserving durable checkpoint
identity across cancellation or comparison failure. Expected binary, encoding, and source-size
unavailability remain distinct. Shared-engine admission and execution errors propagate to the
existing broker failure path without a fallback comparison engine.

The unified writer consumes raw hunks and shared context grouping, writing source lines directly
to an `io::Write` destination. It preserves CRLF, missing-final-newline markers, empty-side ranges,
and complete changed content beyond the preview line gate. Writer errors stop serialization.
Repository-owned file modes and action eligibility remain outside this raw preview serializer.

Unified headings quote path whitespace, control characters, quotes, and backslashes. The Lua diff
parser decodes quoted headings and source headers, including octal UTF-8 bytes, while preserving
separate file blocks and `/dev/null` identity. Mixed quoted/unquoted headings remain supported.

Checkpoint source acquisition uses the host repository store's blocking read pool. Each job reads
at most two 8 MiB objects, verifies stored digests, validates encoding, and computes source identity
and newline metadata outside the async executor. Cancellation checks separate those stages. The
default four read slots bound running and uncollected checkpoint source pairs to 64 MiB of source
payload, excluding allocator overhead and separately retained analysis results. Object IDs consume
the pool's declared input budget. Busy and closed admission propagate through the broker error path.

Preview serialization still executes synchronously in the broker future. Persisted previews
still accumulate into strings. Unified source/cache byte accounting, output retention and
transport delivery, status/review routing, and syntax scheduling remain incomplete. This integration
does not complete the Harness migration gate.

## 54. Checkpoint capture admission

Every broker capture path awaits `GitCheckpoint::capture` on the host's shared blocking read pool.
This includes initial and final captures for normal interactions and child-agent turns. Git queries,
workspace file acquisition, object publication, and checkpoint manifest hashing execute outside
the broker executor. Discovery resolves the canonical worktree before capture admits repository
scopes. The request charges its retained workspace path and session identity before acquisition
starts. Completed records retain the read slot until collection or disposal.

Capture checks cancellation during enumeration, between 64 KiB file reads, and before
manifest publication. Dropping a waiter retains native ownership until the worker exits.
Published but unreferenced immutable objects are harmless and may remain after cancellation.
Source metadata includes file identity and change time where the operating system provides
it. File replacement invalidates cached identities. The content and baseline caches retain
at most 65,536 entries each. Reopening a host rebuilds those caches from current metadata.

Initial interaction startup persists its checkpoint before publishing the provider turn.
Admission and acquisition failures leave a visible finalization error. Capture diagnostics
report candidate and override counts, cache hits, file bytes read, and elapsed milliseconds.

## 55. Bounded checkpoint command output

`forge_git::command::read_command` owns one direct child and two scoped output readers. The caller
supplies job admission, independent stdout/stderr byte limits, a deadline, and cancellation checks.
Readers drain both pipes concurrently, preserving binary bytes and nonzero exit status for caller
classification. Each reader uses an 8 KiB scratch buffer and grows retained output geometrically
within its stream capacity. An extra byte beyond the limit produces an error without publishing
truncated output as success.

Cancellation, timeout, output overflow, and reader errors request direct-child termination. The
owner waits for the child and joins both readers before returning. Callback unwind uses the same
child cleanup ownership. The deadline is a termination deadline rather than a guaranteed return
time. Operating-system waits and inherited pipes held by descendants can outlive it. Process-tree
containment remains incomplete.

Checkpoint discovery reads Git metadata through gix rather than shell file lists.
`file_command` sends large immutable blob output directly to an owned temporary file,
retains at most 64 KiB of stderr, and applies a 30-second deadline. It shares process-group
or Windows Job Object ownership and cancellation cleanup with the bounded command runner.

## 56. Capture and rollback share repository scopes

Capture and rollback derive index, worktree-file, and shared-ref scopes from one canonical
repository identity. Capture waits for intersecting predecessor operations before reading the
workspace. A capture waiting for coordinator scopes holds no repository-read slot. Capturing from
a nested directory resolves to the entire canonical worktree rather than a partial subtree.

`CheckpointCapture` retains its repository lease, copied request inputs, generation, and admission
owner. It samples the generation after scope handoff and checks both cancellation sources and
observed invalidation during acquisition and before accepting the result. A cancelled queued
capture removes its admission. A cancelled native worker retains scopes until capture exits.
Immutable object publication can leave unreferenced objects, but does not mutate workspace files,
the index, or refs, so abandoned capture can finish as failed without quarantining those scopes.

Successful native completion releases the scopes while retaining the receipt and request-byte
charge with the result. Result collection or disposal drops copied inputs before acknowledging
the receipt. Rollback keeps its separate uncertain-outcome recovery contract for workspace writes.

This serializes capture against operations already routed through the coordinator, including
Harness rollback. Legacy Lua mutation paths and external writers remain outside that guarantee
until their cutover or explicit observation. Generation checks do not discover external changes
automatically. The saved restore journal provides restart recovery. External writers remain outside
coordinator ownership and are checked through file preconditions. Aggregate disk admission
and external-writer isolation remain separate constraints.

## 57. Rust status metadata enumeration

`forge_git::reader::StatusReader::status` uses the worker-local gix repository through the shared
read pool. The `Gix` backend combines HEAD-to-index and index-to-worktree events, requests every
untracked path, evaluates configured submodule state, and enables staged rename tracking. Scoped
reads disable staged rename tracking. It never writes gix's optional index-refresh suggestions.

The reader caps the collected status map at 65,536 paths, restores byte-wise path ordering, and
checks cancellation between emitted events. `gix` performs its own parallel directory, worktree,
and tree-index traversal. The iterator must finish before Forge accepts its result.

`PathRecord` preserves raw repository path bytes, independent staged and unstaged change kinds,
HEAD/index objects and modes, worktree modes, rename/copy origin and similarity, all three conflict
stages, and submodule dirty flags. `affects` includes the removed origin of a rename. A copy affects
its destination without including its unchanged origin. Content classification and both line-count
states remain `Unknown` because metadata enumeration performs no diff-body acquisition or analysis.

Cancellation and store-generation invalidation reject results. This API returns `StatusEnumeration`,
not a stable repository observation. It does not yet establish HEAD/index stamps, external-change
detection, source identities, scoped observation merging, or mutation preconditions. Lua status and
review views still own their current read paths. Native status parity, live feature cutover, and
the repository-reader acceptance gates remain incomplete.

## 58. Repository observation collection and adoption

`RepositoryState::observe` serializes collection and adoption across documents sharing one
repository, so concurrent opens and refreshes cannot supersede each other's observation requests.
Each admitted collection assigns a per-worktree request sequence, reads through
`RepositoryStore::read`, and adopts only the latest requested result from the same worktree and
generation. Detached `snapshot` reads remain independent and do not advance that request sequence.
Collection performs no I/O under the observation-state mutex. Invalidation advances the generation
and clears the adopted reference under that mutex, rejecting results collected before invalidation.
Existing client leases retain their historical observation. Replaced observations drop after the
state mutex is released.

The collector reads HEAD and an index stamp and obtains sorted status metadata. A second
status enumeration must match the first. The collector verifies the index fingerprint and HEAD again.
Detected mismatches reject the result. HEAD preserves the symbolic reference and target object,
including unborn and detached states. Linked worktrees retain independent observations.

Display collection performs no separate worktree-stamp passes. Paths start as `Unobserved`, which
is distinct from `Missing`. Statistics reuse metadata already obtained during bounded content
acquisition. Counts describe the bytes encountered and may become stale during collection.
Selected-path settlement retains its verified collection mode. The removed broad display scans
have no worker pool. Expansion verifies cached source provenance. Writes use the action-specific
preconditions described above. Whole-file staging accepts current content, hunk staging rejects
changed captured metadata, and discard validates captured content. An unobserved display stamp
imposes no earlier metadata precondition.

Status retains its visible counts while editor saves within the workspace, re-entry, refocusing,
or an explicit refresh trigger asynchronous recomputation. Repeated requests coalesce into one
active refresh and one latest pending refresh. Lua rejects superseded results and recovers a full
snapshot when skipped patches advanced the native revision. Native refresh advances the repository
generation and verifies the collected generation before reconciliation. Closing the document
removes its autocmds and completes pending refresh callbacks with a closed-document error.

Index stamps read at most the final 32 bytes of the index and each bounded
`sharedindex.<hash>` file, retaining Git's existing checksum and the file length.
They do not hash or scan index contents. If Git omitted its checksum, the stamp uses
modification time instead. A present checksum ignores timestamp-only shared-index
refreshes. At most 256 shared-index files are admitted per directory.

Worktree stamps use file kind, size, modification time, available creation time, readonly state,
and symlink target text. Unix builds also record device, inode, and change time. Symlink contents
are not opened. These are metadata observations. Changes that preserve sampled metadata, changes
outside the observed path set after enumeration, and changes after the final checks require later
source verification. Exact byte identities are acquired through the content API. Mutation
preconditions remain separate work.

Collection checks cancellation, generation invalidation, and a 30-second cooperative deadline.
Full collection runs two native status enumerations and two index-stamp passes. It derives
line counts from Forge raw hunks using the same canonical source pairs used for body rendering.
Binary, filtered, unsupported, and oversized sources retain unknown counts. The count total can
differ from Git's default numstat when the two diff algorithms choose different repeated-line hunks.

## 59. Bounded repository content acquisition

`RepositoryState::content` admits object, index-stage, worktree, or supplied-source requests on the
shared read pool. Request accounting includes retained path capacity and supplied source capacity.
The default input budget is now 32 MiB so an 8 MiB supplied source can enter the pool. The four-job
limit remains unchanged. Admission, cancellation, and generation invalidation fail the outer read.
Native acquisition returns distinct ready, binary, oversized, missing, unavailable, and failed
outcomes. Actual acquisition errors retain their error chains.

`FileContent` reuses `forge_diff::source::SourceVersion` for immutable bytes, lazy SHA-256 content identity,
representation, and newline metadata. Acquisition provenance records the immutable Git object,
selected index stage and mode, or worktree path and stamp. Git object and index bytes are canonical.
Worktree requests explicitly select raw or canonical bytes, and supplied sources retain their
declared representation. The Git
crate depends on the repository-independent source contract without invoking diff analysis.

Native gix reads check object hash format, existence, blob kind, and advertised size before decoding.
They disable replacement-ref substitution. Exact content requests verify decoded bytes against the
requested Git object ID. Informational statistics use the existing object ID without rehashing the
blob. Exact mutation acquisition retains checksum verification. A real packed-object fixture
corrupts an oversized blob's compressed payload and still
obtains a size rejection, proving that this path does not decode the payload before admission.
Native gix delta bases, decoder scratch buffers, and aggregate result leases still need memory
admission before the resource acceptance gates can pass.

Index-stage selection uses bounded `git ls-files --stage -z` output with literal pathspecs and
64 KiB limits for each output stream. It validates exact path identity, unique stage records,
mode, and object hash through the shared object-state parser. It checks the selected stage again
after reading the object. Base, ours, and theirs remain separate sources. Gitlinks report an
unavailable content kind rather than entering text analysis.

Worktree acquisition checks metadata before opening, from the opened file, after streaming, and
at the path after acquisition. It rejects growth and shrinkage without returning a partial source.
Reads use an 8 KiB scratch buffer and reserve only the admitted file length. Leaf opens use Unix
`O_NOFOLLOW` or Windows `FILE_FLAG_OPEN_REPARSE_POINT`. Symlink content is its target text, without
opening the target. Directory and special-file requests remain explicit unavailable outcomes.

Callers can lower the 8 MiB byte ceiling and impose a line limit. Line counts preserve CRLF and
terminal-newline behavior, stopping at a lower bound when a line limit is exceeded. An optional
expected source identity rejects changed bytes or representation. Successful empty sources remain
distinct from missing content. Complete canonical conversion, external filters, textconv, atomic write
preconditions, aggregate memory admission, and the live feature cutovers remain incomplete. The
Unix symlink fixture is present but was not executed by the Windows validation run.

## 60. Canonical worktree conversion

Worktree content requests select `WorktreeConversion::Raw` or `GitCanonical`. Canonical regular-file
reads use the pinned native gix conversion pipeline after bounded raw acquisition and before UTF-8
validation. This ordering permits UTF-16 input without admitting it as a UTF-8 source. Symlink target
text is canonical without applying regular-file filters. Object and index content remain canonical.

Successful conversion records the effective configuration digest, selected conversion-attribute
digest, and index stamp in shared immutable provenance. It does not hash raw worktree bytes
for conversion provenance. The
reader strictly reopens repository configuration, validates worktree/index locations, and compares
fresh configuration, attributes, and index state after conversion. Invalid configuration is an
acquisition failure. These sampled checks do not establish atomic mutation preconditions.

Status opens and refreshes share one conversion session across changed paths. The session
prepares configuration, index, native pipeline, attribute stack, and bounded info-attribute
input once. A fresh verification session compares configuration and index, then resolves every
used path's attributes before the batch can publish. Single-file content acquisition uses the
same preparation and verification boundaries around one file. Existing HEAD, status, and
selected-path metadata checks still surround verified settlement collection. Display collection
omits those broad checks. Concurrent conversion-input
changes enter the repository's three-attempt retry policy.

Statistics acquire only changed sides, retaining a path's index source across its staged and
unstaged comparisons. Source newline metadata uses `memchr` and preserves unterminated final
lines. Added, deleted, and untracked text uses that metadata without tokenizing a comparison.
Modified text uses the pinned `gix-imara-diff` Histogram implementation and line postprocessing.
Counts and display hunks derive from the same comparison. Binary, oversized, conflicted,
submodule, and unsupported sources retain unavailable counts.

Each repository owns a count cache with at most 8,192 entries and 4 MiB of charged entry data.
The cache stores comparison object IDs and modes, counts, and optional prepared-analysis keys.
Worktree entries also store file kind, modification time, and byte size. A matching immutable
comparison reuses its counts before object acquisition. A worktree cache candidate requires one
metadata read before reuse, without opening or hashing its content. Cache misses reuse metadata
from content acquisition rather than adding a preliminary metadata scan. Changes preserving time
and size may retain stale displayed counts. Explicit repository invalidation expires worktree
entries, preserves immutable entries, and prevents older collections from republishing counts.
New entries publish after conversion-batch verification. Pending entries have the same count and
charged-byte limits as the cache. Cache entries never authorize mutations or bypass lazy
prepared-analysis validation when a file expands.

`RepositoryStore` and the runtime's `DiffEngine` share one synchronized `AnalysisStore`, bounded
to 512 entries, 128 MiB of retained sources, and 64 MiB of results. Statistics reserve retention
before constructing cached hunks, prioritize unstaged then staged path order, and
never evict existing analyses. Saturation keeps exact counts but omits retention. Count-only
comparisons omit hunk byte ranges and content hashes. Retained full analyses request lazy source
identities for their exact cache keys and hunk IDs. Acquisition that requests an exact identity
reuses that same hash, including the identity returned with binary or unsupported content.
Cache locks never span file I/O, native
comparison, or asynchronous waits.

Completed snapshots retain `PreparedAnalysis` keys and source provenance rather than analysis
handles. A file-body demand checks the captured generation, worktree metadata, and fresh
conversion inputs before reusing retained sources and hunks. Eviction or changed inputs uses
fresh acquisition. Syntax remains demand-driven. Mutation paths always acquire their exact
source preconditions independently of these display-cache associations.

The native pipeline handles CRLF normalization, text attributes, ident contraction, and the admitted
encoding subset. UTF-8, UTF-16LE, and UTF-16BE are admitted. Explicit UTF-16 endianness rejects byte
order marks. Decoder capacity is checked against the caller's byte limit before conversion. Other
encoding labels return an explicit unsupported-encoding outcome until their Git parity is proven.
Configured clean/process filters or required filters return an explicit external-filter unavailable
outcome. All native external driver options are cleared before conversion, so no filter process can
start through this path. Textconv remains separate work.

Attribute resolution corrects the pinned gix-worktree 0.56 precedence behavior by applying
`$GIT_COMMON_DIR/info/attributes` after nested worktree rules. The override uses collected macro
definitions and preserves explicit unspecified/unset states. A validated resolved selection drives
both native conversion and final context verification. Each additional info-attribute read permits
1 MiB. Selected-state digest input permits 64 KiB and serialized effective configuration permits
1 MiB. Native configuration/attribute parsing, decoder scratch, and aggregate result memory still
require complete admission accounting before resource acceptance gates can pass.

The CRLF history callback reads only an unconflicted index entry, checks blob kind and the 8 MiB
header limit before decoding, then verifies retained capacity and object checksum. Tests compare
canonical bytes with real Git staging for an existing CRLF index entry. Other fixtures cover native
Git object identity, configuration refresh and invalidation, attribute precedence/macros, explicit
unset/unspecified states, index identity changes, external-filter exclusion, and encoding limits.

Enabling gix's attributes feature retains gix 0.87.1 and adds locked transitive packages
`gix-submodule 0.34.0`, `dashmap 6.2.1`, and `hashbrown 0.14.5`. The stale local registry index needed
an explicit package metadata refresh before the new graph could resolve. Subsequent checks use the
locked offline dependency graph. Live status/review read cutovers and all migration acceptance
gates remain incomplete.

## 61. Revision candidates and synchronous completion cache

`RepositoryState::refresh_revisions` collects local and remote branch arguments through the shared
blocking read pool. The `Gix` backend enumerates references, resolves symbolic remote references,
and preserves Git-compatible short names, including ambiguous local branches.
The result retains raw argument bytes, a sorted candidate list, request revision, repository
generation, shared-storage identity, and a digest of the enumerated full refs and object IDs.

Each enumeration retains at most 20,001 candidate records and 16 MiB of encoded reference data.
Accepted candidate storage permits 20,000 values and 2 MiB of actual vector capacities,
including the candidate index. Callers can lower both limits. The extra record proves truncation
when the count ceiling is reached. Byte saturation keeps an admitted prefix and marks truncation.
Malformed framing, unsorted full refs, duplicate short arguments, and wrong object hash formats
reject the result. Invalid UTF-8 arguments retain their original bytes.

Two equal enumerations fence detected ref changes. Collection checks cancellation and the
30-second cooperative deadline. This is sampled consistency without an atomic ref transaction.
A newer request, duplicate adoption, foreign storage, or repository invalidation rejects publication.
Failed refreshes preserve the accepted snapshot. Invalidation acquires observation then revision
state locks, advances generation, and clears both current snapshots before releasing those locks.
Previous snapshots are dropped outside the locks. Linked-worktree storage invalidation clears each
worktree's candidate cache. Readers retaining an old Arc keep an explicitly historical snapshot.

`forge/completion.lua` owns one completion list independently from repository discovery and I/O.
Its synchronous lookup uses binary search and returns at most 200 values. A cold or stale lookup
schedules one refresh, and warm lookups schedule none. Callback delivery is accepted once. Failed
requests notify, preserve valid candidates, clear pending state, and delay retry for one second.
Repository invalidation rejects an in-flight result, while a host-generation change drops the list
and rejects old-host responses. Successful empty lists remain distinct from failed requests.

Lua publication validates identity, generation, revision, strict byte ordering, dense-list shape,
20,000 values, and a 2 MiB accounting quota before copying the list. The accounting quota includes
4 KiB per snapshot and 64 bytes per value in addition to string bytes. It is an admission policy,
not a measured bound on Lua VM allocation or total host memory. Candidate snapshots expose their
truncated state. The shared-host page encoder applies this same quota before wire delivery and
uses base64 to preserve raw ref bytes. Section 62 describes the live command integration.

Peak-memory and cross-platform measurements remain required before the completion acceptance gate
can pass. The accounting quota is not a measurement of total Lua VM memory.

## 62. Shared-host revision completion cutover

`builder.lua`, `client.lua`, and `protocol.lua` own build launch, the shared process, and the wire
contract at the Forge root.
Their old Harness paths are removed, with callers and migration destinations updated at source.
`initialize` accepts only the exact host wire version. Version 2 rejects older clients without a
compatibility decoder. Host startup constructs shared repository/diff owners but does not discover
a repository, open Harness stores, acquire a Harness session lease, or construct a provider.

`router.rs::HostRouter` handles `repository.revisions` directly through `RepositoryStore`.
`harness.initialize` lazily constructs the existing Harness runtime and session-controller registry.
Harness methods reject requests before that initialization. Opening Harness after repository reads
reuses the same process. Existing control admission, consumption credit, session routing, and
terminal shutdown responses remain active. Invalid host handshake parameters produce a correlated
error before feature initialization. Complete typed routing for other services remains unfinished.

Revision refresh publishes one accepted Rust snapshot, then returns a page of NUL-framed raw
arguments encoded as base64. Pages contain at most 128 KiB of decoded payload. Subsequent requests
carry the repository identity, ref digest, snapshot revision, and byte offset. They read the accepted
snapshot without rerunning Git enumeration and reject a changed repository, invalidated snapshot,
changed source digest, superseded revision, or out-of-range offset. The encoder applies Lua's
4 KiB plus 64-bytes-per-value accounting quota before selecting the exposed prefix. Further
omissions set the existing truncated flag. Individual arguments may span page boundaries.

`revisions.lua` captures working-directory context at setup and on command-line/directory events.
The synchronous command callback reads that context and cached strings without repository discovery,
Git execution, or JSON decoding. Cold calls return immediately and schedule one host start/refresh.
The adapter assembles bounded pages asynchronously and publishes only after identity, offsets,
length, count, framing, and cache validation succeed. Ten-second stale refreshes retain the previous
list while running. Failures notify and preserve valid data. A truncated result produces a warning
that manually entered revisions remain available.

`ForgeBranchDiff`, the revision argument of `ForgeBranchDiffFile`, and the revision argument of
`ForgeFileRevision` now use that adapter. Native file completion for the first file argument remains
unchanged. One active workspace cache bounds retained list ownership. Host generations invalidate
old lists. Transport callbacks are admitted before startup or writes, with at most 60 ordinary and
64 total pending requests, including reserved initialization/control traffic. Startup waiter lists
are also bounded. Stop fails queued requests once and rejects a late build result. Warm lease
recovery creates, retries, or forks through the initialized host instead of restarting it.

The full Windows Rust workspace passed 526 tests. The full Neovim run passed 88 files, followed by
an additional real-host command test and focused admission/version/completion checks. The actual
Neovim-to-Forge-to-Git fixture completes a Unicode branch name without loading a provider descriptor
and verifies clean host shutdown. Pipe fixtures cover 5,000 refs across multiple pages and rejection
of a superseded snapshot. A warm command-completion probe over 20,000 values measured 0.0629 ms p95
and 0.1572 ms maximum across 500 calls on this Windows host. These measurements do not establish
macOS behavior, cold-start latency, or peak-memory acceptance.

The shared client still contains Harness snapshot reduction and initialization helpers that need
further extraction. The shared builder has moved to `forge/builder.lua`, and every caller uses that path.
Live status/review/GitHub cutovers, complete service routing, deployment
acceptance, and global memory/cancellation/soak gates remain incomplete.

## 63. Method validation before Harness state access

`HarnessMethod` defines the 60 accepted Harness wire names. The shared router decodes a method
before resolving a session or arming provider cancellation, then carries the classified method
into asynchronous routing. Unknown names return a correlated failure before lazy Harness state is
accessed. Host initialization, repository revisions, and shutdown retain separate routes.

Direct Harness dispatch also decodes before trace recording, lease refresh, or timeline reconciliation.
Its feature match is exhaustive over the enum. Process-owned controls return an explicit coordinator
error if incorrectly submitted directly to the broker. Provider-fork readiness classification belongs
to the same type and is reused by preparation and session routing.

This boundary validates method names. Harness parameter decoding and complete host-task shutdown
remain separate work. It does not complete typed request schemas for the remaining services or the
migration acceptance gates.

## 64. Shared wire-record ownership

`forge-protocol::message` owns `Request`, `Response`, `ProtocolError`, `SessionEvent`, and `Message`.
The executable and Harness import these records directly. Harness retains its feature-specific method
decoder. The old Broker-prefixed records and their exports are removed, and the baseline protocol
source now maps to the shared message module in the conservation inventory.

`Response` owns a private `Result<Value, ProtocolError>`. Callers cannot construct both outcomes or
omit an outcome. Serialization emits the request ID and exactly one result or error. A successful
JSON null remains an explicit result. Deserialization rejects missing IDs, missing outcomes, duplicate
IDs or outcomes, malformed errors, unknown fields, and records mixing response and event fields.
Session events retain their existing session, event-name, and payload shape.

The wire JSON shape and version remain unchanged. The Lua decoder still has its existing validation
behavior. Negotiated handshake records, typed service payload discriminators, document-event wire
records, operation receipts, and complete cross-language shape validation remain open.

## 65. Harness service ownership and quiescent lease release

`ForgeRuntime` constructs a lazy `HarnessService` alongside the shared repository store and diff
engine. Construction opens no provider, repository, or durable store. The service owns initialization,
session controllers, provider-fork gates, lease heartbeats, and independent control routing. The root
router delegates Harness work through the service and keeps host handshake and repository routing.
The executable no longer contains session-controller or provider-operation implementations.

Service admission uses shared activity guards, so independent sessions and control requests do not
acquire one exclusive feature lock. Host preparation arms provider cancellation in input order before
concurrent dispatch. Initialization still publishes exactly one registry and preserves structured
lease-conflict recovery. Failed initialization leaves the service available for retry.

Shutdown first closes admission and signals every current controller. It waits for active dispatches
and provider-fork tasks, then explicitly releases every controller lease and drops the registry.
A timeout retains task and registry ownership and allows a later shutdown call to finish. The wire
shutdown response follows Harness quiescence. Runtime teardown also invokes service shutdown before
closing repository/diff resources, sharing its existing two-second teardown budget across those steps.

Provider forks use a retained JoinSet with at most 64 pending or uncollected tasks. Admission reaps
completed tasks and rejects saturation before preparing durable child state. Shutdown waits for these
tasks without aborting them at the deadline. The mock backend now scopes steering to Harness session
IDs so integration tests can run independent active sessions. Weak lane references are pruned as
new lanes are resolved, avoiding retention of one lane for every historical mock session.

Provider subprocess cleanup still uses each backend's existing destruction behavior. Complete host
request joining, native provider-process exit verification, global resource accounting, and shutdown
under all transport/worker failure modes remain incomplete.

The connection host now starts teardown before writer drainage on EOF. Section 66 describes the
request-task and output-drain boundary added after this service extraction.

## 66. Connection ownership, EOF cancellation, and request drainage

`host.rs::ConnectionHost` owns the shared handshake, input loop, output task, receive credit, and
connection shutdown. `main.rs` selects the process role and delegates Neovim transport to `run_nvim`.
The router retains method classification and feature dispatch. Wire shutdown is coordinated by the
connection host rather than dispatched as one ordinary Harness task.

`RequestTaskStore` records request IDs alongside a bounded JoinSet. Its 64-entry ceiling matches
request admission, and duplicate IDs are rejected while their tasks remain registered. Completion
and panic both remove the matching identity. Request tasks retain their router/runtime and admission
permit while running. The runtime owns the task store, so a drain timeout does not drop the set and
abort a feature future. A delayed task releases that ownership only when it actually finishes.
This registry tracks task completion, not confirmation that the client consumed its response.

EOF, transport failure, and explicit shutdown close request admission and start runtime cancellation
before output drainage. During explicit shutdown, the input loop continues accepting consumption
credit. The host joins pending requests and waits for feature shutdown before enqueueing the terminal
response. Successful shutdown uses the same `shutdown: true` result for a cold or initialized host.
Response order remains the existing outbound FIFO order.

Connection drainage uses a two-second cooperative deadline. A deadline failure reports unfinished
request IDs and whether feature shutdown finished. The output task receives a stop signal and has a
further 100 ms cancellation deadline. Only the writer can be aborted at that boundary. Request tasks
remain retained. Main's existing runtime teardown still runs after the connection returns, so the
connection deadline is not a hard bound on total process exit time or native system calls.

Pipe tests cover EOF with an active mock turn, exhaustion of receive credit, and duplicate IDs during
an active request. Duplex tests cover a writer blocked inside its output write and a terminal response
held until unrelated registered work completes. Registry tests cover panic cleanup and retention of
runtime/admission ownership across a timeout.

Negotiated limits, per-operation cancellation and receipts, acknowledgement-bound request identity,
provider-process exit verification, complete global memory accounting, and all-platform shutdown
acceptance remain incomplete.


## 67. Issue storage library and atomic completion publication

`forge-github::issue_store::IssueStore` owns the existing redb issue, detail, term, label, and sync
schema. Remote and stored records live in `forge-github::model`. The crate has no Forge feature or
protocol dependencies and pins redb 2.6.3, matching the previous executable lockfile. The standalone
storage package has been removed. Sections 68 and 69 describe host routing and the Lua consumer
cutover. Remote sync ownership remains incomplete.

An IssueStore contains a database path, normalized owner/name, and lock retry timeout. Construction
validates the name without opening a database, creating directories, or requiring an executor. Every
operation opens its own redb handle and releases that handle before returning, including on errors.
Async host consumers must place these blocking operations in retained blocking workers. This
library supplies their admission and shutdown ownership through GithubService, described in section 68.

Page updates retain one transaction for issue rows, term and label replacement, and synchronization
cursor/high-water state. Detail lookups preserve request order and distinguish missing entries from
database or decode failures. Completion snapshots omit issue bodies and retain the existing update
ordering. Database commit errors require callers to reload before assuming rollback.

Snapshot publication retains the database handle through serialization, file synchronization, and
atomic replacement. A temporary file resides in the output directory, so replacement does not cross
filesystems. Publication refuses the database itself as its output. Serialization and replacement
errors preserve the previous snapshot. Database commit and JSON publication remain separate
operations, and crash recovery between them is not yet coordinated by a GithubService. Directory
entry durability across power loss is not proven by file synchronization alone.

Database opening no longer changes the process-wide panic hook. A caught panic returns its payload
and retains the database instead of renaming it after losing redb ownership. Lock contention retries
only through the supplied timeout and never triggers archiving. The historical archive helper and
its original test remain test-only preservation evidence. They do not establish a production
corruption recovery path. Exclusive recovery and cache deletion ownership remain pending.

The library retains seven original storage tests and adds four API lifecycle tests. A separate CLI
fixture verifies page, state, snapshot, and detail contracts across successive executable processes.
Those fixtures establish transaction-bound handle release. They do not establish the planned
multiple-Forge-process or deletion-versus-sync acceptance gates. Snapshot memory limits, normalized
hostname identity, bounded remote retries, and host-owned issue completion remain incomplete.


## 68. Host-owned issue storage workers

ForgeRuntime now constructs a lazy GithubService and routes `github.issues` to its typed
IssueOperation dispatcher. Requests supply `database`, normalized `repo` input, and a nested
`request` containing the operation discriminator and operation-specific fields. Supported operations
are upsert_page, state, detail, details, upsert_detail, and publish_snapshot. Host and operation
schemas reject unknown fields. The parameterless state operation uses an empty struct variant
because the serializer's unit variant otherwise accepted unexpected fields.

GithubService admits at most eight queued, running, or completed-but-uncollected blocking jobs.
Page and detail-batch requests accept at most 100 issues. Saturation and closed admission fail before
work starts. Each worker owns its input, a reference to the task store, and the response sender.
Dropping a caller or the last service handle does not release native work ownership. A dropped result
receiver discards the completed result, so an unobserved mutation still requires a later truth read.
Operation receipts and retry classification remain incomplete.

Runtime shutdown closes issue storage admission before waiting for Harness teardown. It then joins
storage completion alongside repository and diff shutdown using the remaining runtime deadline.
A timeout retains the JoinSet and reports unfinished jobs. Subsequent shutdown attempts can collect
those jobs after native completion. Concurrent shutdown collectors serialize through a separate
async mutex within their deadlines. Native calls are not forcibly interrupted. The shutdown report
counts task join failures separately from ordinary operation errors delivered to callers.

Host-created IssueStore instances use a one-second database lock retry timeout. Before admitting an
issue response to the outbound queue, the router checks its encoded size against the shared frame
limit. An oversized result returns a correlated result_too_large error with operation_completed
set to true and leaves the connection usable. The caller must not treat a delivery-size error as
proof that a successful database operation needs to be repeated. This prevents output queue poisoning, but does not bound the earlier redb decode, response
object construction, or snapshot allocation. Global memory accounting and segmented detail delivery
remain pending. Publication returns the issue count and keeps the full snapshot on disk.

Five service tests cover saturation, dropped result receivers, shutdown timeout/retry, ownership
after service drop, panic reporting, typed real-store operations, and rejection before disk access.
Executable tests verify storage without Harness initialization, strict operation decoding, and
connection survival after an oversized cached detail. Runtime tests verify that shutdown rejects
storage without creating its database. Lua consumers now use the host as described in section 69.
Remote sync, deletion ownership, hostname identity, and snapshot memory bounds remain incomplete.


## 69. Shared issue consumers and standalone package removal

`github.issue_index` sends page, state, detail, detail-batch, detail-update, and snapshot-publication
requests through `forge.client.request_host("github.issues", ...)`. The request contains the existing
repo-cache database path, normalized repository name, and typed operation parameters. Host startup
and process reuse share the same client as Harness and revision completion. Issue-only startup does
not initialize Harness. Lua no longer builds CLI arguments, serializes storage input separately,
decodes storage process output, resolves a storage binary, or launches a storage process.

The request adapter preserves the existing success/failure callback convention. It delivers at most
one callback, returns startup/transport errors to the owning consumer, rejects non-table results,
and reports synchronous request exceptions. Sync-state failure notifies once and releases its
repository sync lock. Detail-prefetch failure remains an error and emits the underlying failure
message rather than appearing as a successful empty batch. Full operation-specific response-shape
validation and completion snapshots preloaded entirely outside synchronous callbacks remain pending.

The standalone storage Cargo manifest, lockfile, source, test adapter, Lua builder, and VimEnter
build plugin have been removed. Prior generated Cargo output remains ignored at its old location.
The existing seven redb tests live in forge-github. The executable contract fixture now uses two
independent Forge hosts against one database, covering page updates, state, atomic publication,
detail writes, ordered detail batches, and single detail reads between short transactions. Generic
automatic builds, executable selection, and process-copy ownership are verified by `tests/forge/sidecar_manual.lua`.

The real Neovim issue fixture runs a mocked two-page remote sync with the actual Forge host, verifies
three persisted completion records, confirms body omission, and checks clean process shutdown. The
root GitHub, repo-cache, and Forge integration fixtures use typed storage mocks. New cases cover
host failures, malformed results, synchronous request exceptions, duplicate callbacks, lock release,
and detail-prefetch notifications. LuaLS reports no diagnostics in issue_index.lua after replacing
parameter reassignment with normalized local bindings and correcting the removed cwd annotation.

Lua still owns remote GitHub requests, sync progression, rate-limit retries, and repository cache
deletion. Deletion coordination with active host storage, normalized hostname identity, bounded
snapshot/decoded-record memory, and Rust review/status document ownership remain incomplete. The
shared-host storage cutover does not complete the full remote migration gate.

## 70. Completion snapshot preload

Completion queries must not read files or decode JSON. `github.issue_snapshot` owns the asynchronous
open, stat, chunked read, validation, and close lifecycle. `github.issue_index.list` and `search` use
only retained records and return independent label tables. Cold queries return no records until
preload completes. Sync startup requests preload, and successful host publication awaits a forced
reload before completing the sync callback.

Directory watchers observe atomic snapshot replacement and request reloads. A failed reload retains
the last valid records. Each file is limited to 16 MiB, read in 64 KiB chunks. Four active requests
share a sixteen-request queue. The cache retains at most sixteen repositories and 32 MiB of encoded
bytes, evicting inactive entries by last access. This accounting excludes decoded Lua allocation.
JSON decoding runs in a scheduled callback, outside the synchronous completion query path.

Invalidation removes cached records and closes the watcher immediately. A pending native read keeps
its descriptor until its callback finishes, then closes it without publishing. Repository cache
deletion invokes this invalidation before removing files. It does not coordinate deletion with host
database jobs. Repeated watcher events coalesce with at most 64 waiting callbacks and three attempts
per request. Capacity failures and changing-file exhaustion report errors without replacing records.

## 71. Repository storage and deletion leases

`forge-github::RepositoryLease` uses shared filesystem locks for issue operations and an exclusive
lock for deletion. The lock file is a persistent sibling of the repository cache directory, so
removing and recreating that directory does not let cooperating hosts lock different files. Each
IssueStore operation retains its lease until the database and any publication temporary file close.
The lease API reports Busy immediately when incompatible ownership exists.

`GithubService` reserves repository admission before submitting a blocking job. Deletion admission
requires no earlier admitted operation for that directory and rejects new operations until its
worker finishes. The worker owns admission independently of the request receiver. Cross-process
ownership is enforced separately by the filesystem lease.

The typed `delete_cache` storage operation accepts the current `issues/issues.redb` layout. It
resolves the repository directory, rejects redirected issue/database paths and existing sync locks,
checks that redb can open, closes the database, and removes the resolved directory under its deletion
lease. A held legacy redb handle produces an error without archiving or deleting the database.
Removal failures report the error and can leave partial deletion. No automatic retry occurs.

Lua deletion commands have not switched to this operation. Lua remote sync does not yet retain a
host lease across remote requests, and the legacy sync-lock existence check does not atomically
exclude a concurrently starting legacy client. That lifecycle cutover remains required before the
user command can claim coordinated deletion. Repository hostname and canonical admission identity
also remain acceptance work.

## 72. Owned issue synchronization

`GithubService::sync` retains one shared repository operation lease and one exclusive sync lease
across remote reads, retry delays, page commits, and snapshot publication. Both lease files remain
outside the repository cache directory. The service admits at most two async sync jobs separately
from its eight blocking storage jobs. Remote waits retain no redb handle, so state and detail
transactions can proceed while deletion and another cooperating sync remain excluded.

Initial history resumes the persisted cursor. Once history is complete, refresh starts at the newest
page and includes closed issues for both open and all scopes. The fixed pre-refresh high-water mark
stops incremental pagination. Each page commits its cursor and high-water state, then publishes an
open completion snapshot. A failed later page preserves earlier committed progress. A repeated
cursor fails before committing that page. A non-manual refresh skips remote reads only when its
scope history is complete, its snapshot exists, and its last check is less than ten minutes old.

The loop admits at most 100 records per page and 10,000 pages, with nonempty cursors limited to
512 bytes. Rate-limit read failures receive at most three retries with sixty-second timers. Normal
page spacing is 150 milliseconds. Exhausted page rate budgets wait sixty seconds. Each remote read
has a 120-second deadline. Close notification takes priority over read or timer progression.

Dropping the caller leaves admitted sync work owned by the service. Shutdown closes admission,
interrupts remote waits and timers, and drains both task collections under the existing deadline.
Blocking commits keep independent operation ownership. A remote implementation must retain native
process cleanup independently of a dropped read future.

`GithubRepositoryId` validates and normalizes hostname, owner, and name without changing existing
cache paths. Sync validates issue identity, state, URL host/path, update presence and ordering, and
page cursor progression before commit. This is not complete remote schema or global allocation
validation. `GithubRemote` is currently an injectable issue-read contract. The concrete gh process
client, host sync route, Lua consumer cutover, progress events, and coordinated deletion command
remain pending. Production remote sync still uses the Lua implementation until those paths switch.

## 73. Bounded gh issue reads

`GhClient` supplies the concrete `GithubRemote` issue-page implementation. It invokes gh directly
with explicit hostname routing, inherited authentication, and JSON request variables stored in an
owned temporary input file. Initial open scope requests open issues. All scope and incremental
refresh request open and closed issues. The query preserves the existing hundred-issue page and
fifty-label limits and excludes issue bodies.

The client reuses forge-git's retained blocking read pool and bounded command runner. Four admitted
native requests share a 64 KiB input budget. Each process accepts at most 8 MiB stdout and 64 KiB
stderr, with a 120-second execution deadline. A blocking worker drives Tokio process I/O through
the existing runtime. `process-wrap` contains each command in a Windows Job Object or Unix process
group. Cancellation and deadlines terminate that job or group, await its exit, and collect both
bounded streams before releasing admission. Windows `KillOnDrop` also terminates the job if the
Rust host exits unexpectedly. Unix process groups need a surviving owner to receive cancellation,
so forced host death has no equivalent guarantee. Git can leave a repository lock after forced
termination, which requires separate recovery.

GraphQL decoding bounds issue, label, and error collections while consuming the response. An empty
issue connection succeeds. Missing fields, malformed JSON, invalid pagination, GraphQL errors, and
command failures remain errors. Error categories preserve rate limits, missing repositories,
authentication failures, and local Busy admission. Diagnostics preserve UTF-8 within a 64 KiB limit
and mark truncation. Decoded collection limits do not establish global allocation accounting.

Native tests compile a local Rust gh fixture and execute the real client against it. They verify
arguments, request-file cleanup, Unicode records, cancellation and child reaping, admission overflow,
oversized output, stderr classification, and retained descendant pipes. The fixture performs no
network requests. Unit tests verify empty results, missing fields, API failures, collection limits,
and diagnostic truncation. The client is not yet registered with host runtime ownership or exposed
through a sync route, so production Lua consumers have not switched.

## 74. Host sync dispatch and request progress

ForgeRuntime now retains the concrete GhClient and includes native gh ownership in shutdown
reporting. Checkout contexts share that single four-request pool instead of allocating independent
admission budgets. Context creation binds the working directory without launching a process.

The `github.sync` host method accepts a database path, checkout directory, typed SyncRequest, and
optional progress flag. It routes through GithubService without initializing Harness. Unknown
request fields and malformed remote identity fail decoding. The final response reports freshness,
fetched records, and published page count. Successful sync releases its repository ownership before
a later delete-cache request runs.

Opt-in progress uses RequestEvent with request_id, event, and payload fields. Sync reports reading,
indexing, publishing, rate-limit waits, completion, and freshness through a watch channel holding
only the latest value. Slow observers can miss intermediate phases. The correlated final response
remains authoritative, and no progress is emitted after it. Request events do not require or imply
a Harness session identity.

The Lua host client accepts an optional progress callback for request_host. It delivers matching
events only while that request remains pending. Unknown, mismatched, and late request events do not
reach Harness subscribers. Invalid request-event shapes fail decoding. GitHub host failures no
longer invalidate unrelated Harness state. Callback failures are reported without completing or
removing the pending request.

Executable tests use the actual Forge host and a local native gh fixture. They verify sync,
freshness, progress correlation, strict parameters, deletion after sync, and shutdown while a remote
read is blocked. The terminal shutdown response follows direct-child reaping. Library tests verify
that separate checkout contexts cannot exceed the shared native admission budget. Production
issue_index.sync_repo still uses its Lua orchestration pending the consumer cutover. Complete
descendant termination and global allocation accounting remain open.

## 75. Repository cache deletion through the host

Repository deletion now uses the shared host's `github.issues` delete_cache operation. Rust acquires
exclusive repository ownership and closes database handles before removing repository data. Lua
retains the loaded issue snapshot while the request is pending or rejected. A successful host result
invalidates the corresponding local snapshot, including an already-absent repository result.

repo_cache.delete_current sequences the requested repository and any distinct mapped repository.
Its callback reports the completed deletion count and an optional failure. The first failure stops
the sequence and preserves the cwd mapping. The command displays success only after the callback.
If an earlier repository was already removed before a later failure, the returned count preserves
that partial completion. Cwd removal errors notify and return failure instead of reporting success.

The deletion sequence captures its cwd and cache namespace. Before advancing, it checks that the
namespace and cwd mapping still match. A changed context stops further deletion and preserves the
new mapping. Direct repository deletion invalidates the snapshot only if the current namespace
still matches the path sent to Rust. The stale-repository sync failure path also uses host deletion.

Lua fixtures verify delayed completion, Busy rejection, unchanged path routing, snapshot retention,
successful invalidation, and a changed cwd mapping. The issue-index fixture exercises the real host
against a real database. An active Lua sync lock rejects deletion without clearing local state.
After lock release, the same API removes repository data and the cwd mapping, invalidates completion
records, and shuts down cleanly. Production issue sync, metadata writers, and other GitHub consumers
have not yet all moved behind Rust ownership, so the complete GitHub migration gate remains open.

## 76. Production issue sync consumer cutover

issue_index.sync_repo now submits one typed github.sync request to the shared host. Lua no longer
owns the issue-page query, GraphQL response parsing, pagination, high-water traversal, freshness
policy, rate-limit delays, page persistence, or filesystem sync locks. GithubService owns those
transitions and retains them through caller cancellation and runtime shutdown.

Lua keys pending sync context by the database path. Duplicate requests in that context do not reach
the host. Each context retains its checkout directory, repository, snapshot path, progress state,
and optional completion callback. The final callback accepts a successful SyncOutcome only after
the snapshot preload completes. A host error, malformed outcome, failed preload, or changed cache
context produces a failure result and notification. Late host responses and progress cannot finish
the same context twice. Test reset retires local context without claiming native cancellation.

Manual sync starts progress immediately. Automatic sync starts progress when the host reports work
or returns a refreshed outcome. A ten-minute freshness skip stays silent. Coalesced progress maps
native phases and counts to the existing notification UI. Invalid progress reports a diagnostic
without replacing the final host response. A failing notifier cannot prevent request completion.

Hostname resolution preserves the explicit issue-index override and Git remote lookup. Lookup
failure remains a failure. Before dispatch, the resolved host must equal repo_cache.hostname(),
which owns the current cache namespace. A mismatch fails before any remote request or database
write. This protects the current path contract while complete repository identity propagation
through other GitHub consumers remains unfinished.

The issue-index fixture runs the actual Forge host with a compiled local gh fixture in its child
search path. It verifies native issue sync, durable snapshot publication, synchronous completion
search after preload, automatic freshness, deletion ownership, and clean shutdown without Harness
initialization. Mocked boundaries cover scopes, duplicate admission, malformed and failed responses,
exceptions, missing repositories, progress phases, notifier failure, hostname lookup failure,
host/cache mismatch, and cache-context changes. Other UI fixtures now inject the host sync result
instead of recreating a Lua GraphQL backend. Detail fetching and metadata writers remain separate
Lua consumers awaiting their service cutovers.

## 77. Native issue details and retained persistence

The native issue adapter gives its retained buffer a `forge://issue/` name and uses `acwrite` ownership.
Neovim therefore routes `:write` through BufWriteCmd into an explicit immutable field capture. Cursor
and insert transitions enable editing only inside a native editable region. Projection delivery
rechecks that boundary, so generated headings and comment presentation stay protected while issue
fields remain editable. Typing sends no host request. Dirty close hides the buffer without saving,
and reopening the same issue restores that buffer with its current unsaved text.

GithubRemote now exposes typed IssueDetailRequest and IssueDetail results. GhClient routes issue
detail reads through `gh issue view` with an explicit hostname-qualified repository. Detail and page
requests share one native command runner and the same four-request admission pool. Process output
limits remain 8 MiB stdout and 64 KiB stderr, with a 120-second execution deadline. Cancellation
retains admission until child exit, pipe drainage, decoding, and result collection finish.

The detail decoder consumes the CLI's current exported schema. It bounds returned comments to 1000
and labels, assignees, and project items to 100 each. Exceeding a bound fails the read. Missing
required fields, mismatched repository or issue URLs, unsupported states, and invalid comment
anchors fail decoding. Empty or null CLI collections remain valid empty collections. Normalization
preserves bodies, metadata, project status, and comment source URLs. CLI issue comments do not
export an update timestamp, so the normalized updated_at remains empty, matching the former Lua
representation. Complete timestamp validation and global allocation accounting remain open.

GithubService.fetch_detail shares the sixteen-job asynchronous admission bound with sync and review reads. The owned job
acquires a repository operation lease before reading remotely and retains it through detail
persistence. Remote waits hold no database handle. Dropping a caller leaves admitted work owned.
Shutdown cancels pending remote reads and drains retained jobs. Service repository admission is
released before delivering the terminal result, allowing a subsequent deletion to observe completed
ownership.

The github.detail route works before Harness initialization. It returns a normalized DetailRecord
after persistence. The route shares the completed-storage response encoder with github.issues.
Section 78 defines segmented delivery for results above the transport frame limit.

Lua detail fetching now calls github.detail and does not repeat remote execution or persistence.
Cached detail display and stale refresh remain available. Memory keys include the database path to
isolate cache namespaces. Matching requests share one pending fetch. Invalid, failed, or stale
responses notify and fail their waiters. A throwing waiter does not prevent other callbacks from
completing. Successful repository deletion invalidates both snapshot and detail memory and retires
local pending detail callbacks without claiming cancellation of already-owned native work.

Tests cover normalized fields, empty collections, collection overflow, source anchors, native
argument routing, shared page/detail admission, cancellation and child reaping, retained service
ownership after caller drop, shutdown, persistence, and oversized host responses. The Lua native
fixture proves remote fetch, persisted reload after memory eviction, and detail invalidation after
deletion. Metadata writers, remote mutations, complete identity propagation, descendant process
termination, and the broader Forge migration gates remain unfinished.

## 78. Segmented completed results

The github.detail and github.issues routes deliver responses above 512 KiB through request-correlated
result.part events followed by one result.complete event. Each transfer retains one encoded response
without a total byte cap. Two permits shared by all sender clones bound concurrent encodings by
count, with additional senders waiting for capacity. This does not bound source values, encoded
response sizes, or JSON decoding allocations. The output queue remains separately bounded.

JsonTransfer splits the encoded response at UTF-8 boundaries into payloads of at most 128 KiB.
SnapshotTransfer uses the same splitter while retaining its document and revision identity. The
sender produces one part at a time and waits for output capacity. EncodedFrame releases capacity
only when its writer drops it. Queue saturation therefore delays these responses instead of poisoning
the connection. Receiver closure and connection failure terminate the wait. Completion follows every
part in the same FIFO queue before the transfer permit is released.

Lua reserves the declared byte total for at most two active results. Its shared JSON assembler
validates exact sequence, matching totals, bounded payloads, and complete byte counts. Result
completion additionally validates request identity and the response envelope before invoking the
pending callback. Parts never reach Harness event subscribers or success callbacks. Malformed
transfers fail the connection, and shutdown clears retained parts and declared byte reservations.
An explicit error response can terminate a partial transfer. An ordinary success cannot replace it.
Late events for requests that already completed are ignored.

Waiting for an encoding permit ends when capacity becomes available or the connection closes.
Serialization failure returns result_encoding_failed with operation_completed true because storage
has already finished. That error does not claim that a committed operation was rolled back.
Disconnect during delivery does not undo service persistence.

Protocol tests exercise FIFO capacity waits, retained writer reservations, receiver closure, and
shared transfer admission. Client fixtures cover large Unicode and escaped content, delayed
completion, malformed sequences and totals, invalid JSON and identity, three-transfer overflow,
explicit failure, and disconnect cleanup. Native host tests deliver a 9 MiB cached detail under
credit control and deliver an encoding above 16 MiB while keeping the host usable. The Neovim native
fixture verifies full remote and persisted detail bodies above the single-frame limit.

## 79. Repository user metadata ownership

The github.metadata route owns contributor and collaborator reads and metadata.json publication
without initializing Harness. It uses the same sixteen-job service admission bound as issue sync and
detail fetches. Native commands share the existing four-request pool. Explicit hostname arguments
select the API host independently of the checkout's default repository.

GhClient requests contributors and collaborators with per_page=100, --paginate, and --slurp. The
typed decoder accepts at most 1000 pages of 100 users per source. The existing native output limit
remains 8 MiB. User logins are nonempty, at most 256 bytes, and contain no whitespace or controls.
Optional names are at most 1024 bytes. A sorted, case-insensitive map merges duplicate logins and
fills a missing name from the other source, retaining at most 100,000 unique users. Empty pages are
successful empty results. Malformed records and exceeded bounds are failures. A successful source
can supply metadata when the other fails, with the failed source's diagnostic retained in the
metadata failure list. Failure of both sources returns an error and preserves previous metadata.
Serialization omits absent names so cached Lua completion retains its repository-text fallback.

MetadataStore validates the existing hostname/repos/owner/name path suffix and the record identity.
It does not rename or relocate legacy data. Legacy records without a hostname remain readable under
that validated path. A ten-minute cache hit returns without issuing a remote request. Future-dated
records are not fresh. Invalid cached JSON or identity returns a visible failure rather than an
empty success or an implicit destructive repair.

One retained metadata job owns both shared repository operation admission and an exclusive metadata
refresh lease. Another metadata refresh receives Busy, while issue sync and detail reads can use
their own leases. Deletion cannot proceed until remote work and publication release ownership.
Dropping a request receiver leaves admitted work owned. Shutdown cancels remote waits and drains
retained blocking publication. No database handle spans the remote wait.

Publication validates the result, streams JSON through a 64 KiB buffer with a 16 MiB output limit,
flushes and syncs the temporary file, and atomically replaces metadata.json. Serialization, byte
limit, or flush failure leaves the prior file unchanged and removes temporary output. The file and
record limits do not establish global process allocation bounds. The host uses segmented result
delivery if the completed response exceeds one transport frame.

Lua repo_users now adapts requests to the host. The two former gh contributor methods and Lua user
pagination, merge, and metadata-write implementations are removed. Status, issue, and notification
consumers use repo_cache.ensure_metadata. Cached reads retain their current interface and cwd
mapping behavior. Pending requests coalesce by cache directory. Callback validation rejects a
changed cache context or foreign identity, and repository deletion retires pending local metadata
context. Error paths release local admission and report refresh failure. Lua does not write the
host's metadata result back to disk.

Tests cover source merging, partial and total failures, invalid and oversized pages, retained
ownership after caller drop, shutdown, old-format cache reuse, identity rejection, publication
overflow, and temporary-file cleanup. The native Neovim fixture fetches merged users through the
actual host, observes persisted metadata before callback completion, and removes metadata through
coordinated repository deletion. Remote mutations, other feature services, process-tree cleanup,
global memory accounting, and the remaining migration gates are still open.


## 80. Completion snapshot revision reconciliation

A process can exit after a page commits and before its completion snapshot is published. Each
page transaction now advances an exact wire integer revision with its issue and sync records.
Snapshot construction reads that revision and its rows in one read transaction. Detail-only
writes do not advance it. Revision exhaustion rejects the entire page transaction.

IssueStore.reconcile_snapshot retains the repository operation lease and database handle while
checking the published repository, filter, revision, and row count. A missing or mismatched file
is rebuilt from committed records. A database without a committed page reports ready=false and
does not publish an empty success. Fresh automatic sync repairs the file before freshness can
skip remote work. The shared publisher streams JSON through a 64 KiB buffer, enforces 16 MiB,
flushes and syncs the temporary file, and replaces the destination atomically.

Production Lua preload and watcher reload reconcile inside the existing four-active-request
admission bound. Concurrent callers coalesce, and callback identity checks reject changed cache
contexts. Lua verifies the decoded revision before adopting records. Concurrent file replacement
uses the existing three-attempt bound. Failures preserve prior records and report failure to the
caller. Synchronous completion performs no storage request or disk read.

The two-process fixture kills the writer after its database commit and observes repair by the
surviving host. A second reconciliation leaves the repaired file unchanged. The native Neovim
fixture verifies recovery without another gh request. Unit fixtures cover revision exhaustion,
invalid publication headers, invalidated callbacks, and revision changes during preload.

Database commit and file publication remain separate operations. Recovery establishes consistency
at its observation point, and a later writer can advance the database again. Header validation
counts rows without retaining their contents and does not validate every issue field. These
checks do not establish global allocation bounds, macOS behavior, or power-loss durability.


## 81. PR lifecycle mutation ownership

Concurrent PR lifecycle requests can interleave reopen and draft conversion, and a failed response
can follow a successful remote write. The github.pull_request route owns both sequencing and
outcome classification. Each transition binds normalized hostname, owner, repository, number,
and expected node identity. Rust reads current remote truth before selecting at most two writes.
Merged PRs reject lifecycle changes, and an already satisfied target performs no write.

RemoteQueue admits at most 64 operations and retains at most 64 resource identities. Clones share
FIFO admission by hostname, repository, and issue/PR number. Distinct hosts and numbers proceed
independently. A running operation retains ownership across reopen and subsequent draft or ready.
Dropping a queued operation removes it without releasing the active owner. Dropping a write owner
after begin_mutation marks its resource uncertain. Unknown completion rejects waiting mutations
and retains the resource slot until a successful reconciliation. Existing uncertain resources can
still reconcile when all 64 resource slots are occupied.

GithubService retains mutation jobs independently of request receivers. Closure rejects waiting
work and drains active work without dropping a possible native write. Native reads and writes
share the four-job gh process pool. Each command retains the existing 120-second deadline,
8 MiB stdout bound, and 64 KiB diagnostic bound. The worker collects process termination and pipe
drainage before the mutation future completes. Native admission rejection proves no write began.
Other native failures conservatively report OutcomeUnknown, including malformed successful output.
No state mutation retries automatically. A proven second-step rejection returns the last confirmed
remote state, while unknown results expose no claimed current state.

Lua github.pull_request adapts host results and guards callback delivery and hostname changes.
After an unknown result, host transport failure, or malformed response it requests exactly one
read-only reconciliation, reports the original
failure, and supplies the newly observed state when available. Reconciliation failure preserves
uncertainty and does not fabricate a state. A changed hostname prevents automatic reconciliation
against a different namespace. pr_edit checks that the buffer still owns the same
PR before applying the callback. The previous Lua lifecycle GraphQL helpers are removed. The
existing chooser and local edit queue remain presentation consumers.

Queue fixtures cover FIFO ordering, independent resource identities, cancellation, closure,
capacity recovery, and uncertain-resource retention. State-machine fixtures cover merged and
foreign records, no-op transitions, reopen ordering, and confirmed partial failure. Native gh
fixtures persist a changed PR before returning malformed output, demonstrate explicit recovery,
and retain a blocked writer after caller cancellation and shutdown. The stdio fixture exercises
this route without initializing Harness, and Lua fixtures preserve chooser, cache, notification,
and no-repost assertions.

Ordering is process-local. Other Lua remote writers and other Forge processes remain outside this
queue. Uncertainty is not persisted across host restart, although every lifecycle transition reads
remote truth before writing. Cross-process mutation receipts, comment/review submission ownership,
complete descendant termination, and global allocation acceptance remain open.


## 82. PR text edits and submitted baselines

PR title and description writes share the Rust resource owner used by lifecycle transitions.
The github.pull_request edit operation captures optional title and body fields. Omitted fields
remain unchanged, and an empty body clears the description. PullRequestEdit rejects an empty
submission, blank or multiline titles, NUL bytes, titles above 4 KiB, and bodies above 256 KiB.
The existing transport frame limit also applies to the encoded request. These are Forge admission
limits, not claims about GitHub's full API limits.

Rust reads PR identity and editable text before choosing whether a write is needed. Matching
submitted fields return a confirmed no-op. A changed field uses one updatePullRequest mutation,
and the response must confirm both identity and the submitted values. A mismatched or malformed
response is uncertain and never triggers another write. ReconcileEdit reads current text and
returns matches_submission without returning or reposting the body. General PR reconciliation
also reads editable text so it cannot release an uncertain text edit using lifecycle data alone.

RemoteQueue retains at most 16 MiB of request allocation through queue waiting and completion,
in addition to its 64-operation and 64-resource bounds. The service charges submitted string
capacity before spawning a job. The native four-process pool has a separate 16 MiB input budget
for encoded queries and launch context. Temporary encoding, decoded observations, Lua copies,
and other services remain outside a complete global allocation proof.

Lua github.pull_request.request_async copies the submitted request before dispatch and adapts
both lifecycle and edit operations. Uncertain edits request one read-only comparison against that
captured text. A matching reconciliation advances the submitted baseline while still reporting
the original delivery failure. A mismatching or failed reconciliation leaves the text unconfirmed.
The old Lua gh.update_pr_async helper and its subprocess construction are removed.

The PR editor updates only fields from the completed submission. It recomputes dirty markers and
the modified flag from the current native buffer before deciding whether to render. If a newer
title, description, reviewer, or milestone edit exists, the save completion preserves those rows.
A changed document identity rejects the callback. Failed remote operations still notify after
the originating buffer closes. The UI regression fixture submits one title, types a newer title
and description while the callback is held, and verifies that the older
completion updates its baseline without replacing either newer field. The next save commits
those fields and clears their markers.

Native tests verify Unicode, quotes, indentation, trailing newlines, omitted fields, empty-body
clearing, no-op writes, shared ordering with lifecycle changes, and text recovery after a write
persists but its response is malformed. The host fixture verifies the edit receipt over stdio.
These changes do not implement the planned Rust ReviewDocument or EditStore. Their document,
region-revision, comment, reviewer, and milestone ownership work remains open.

## 83. Review edit store and save capture lifetimes

The forge-review crate owns accepted field text and saved baselines independently of generated
document layout. RegionEdit carries document identity, region identity, region revision, a positive
edit sequence, and exact UTF-8 text. Stale sequences, conflicting revisions, invalid text, exhausted
counters, and failed admission leave the field unchanged. The buffer integration test inserts
generated rows before accepting a Unicode multiline field edit and verifies matching region
acknowledgements from EditStore and BufferDocument despite their different layout revisions.

prepare_capture validates all fields and reserves their text without changing the store. Its proposed
snapshots let a document prepare its buffer projection before committing either state. Dropping the
prepared capture releases its reservations. Issue captures validate the proposed title and assignees
before projection preparation, so scalar validation and missing projection blocks cannot partially
adopt another field in the same capture.

begin_save retains immutable submitted text, revision, and sequence. Confirmation advances only
that submitted baseline, so newer local text remains dirty. A pending write followed by a local
revert still admits a compensating submission. Callers must execute writes in submission order.
Completion delivery may arrive out of order without replacing a newer confirmed baseline.
Rejected writes preserve the baseline. Uncertain writes retain their capture and prevent further
save admission until an explicit remote observation reconciles the field. Reconciliation preserves
local text, and a superseded observation cannot replace a newer confirmed baseline.

The default per-document bounds are 256 active fields, 4,096 region lifetimes, 64 pending saves,
and 256 KiB per field. Retired region identities cannot be reused during the document lifetime.
A caller-supplied shared EditBudget charges retained UTF-8 text bytes across stores and submissions.
Current text, baseline text, and captures share allocations when their content matches an existing
current or baseline value. A retained submission keeps its charge after its store closes. Admission
reserves new text before changing field state, including the transient overlap with replaced text.
This bound excludes allocator overhead and metadata and does not establish global memory acceptance.

Production ReviewDocument uses EditStore to retain explicitly captured text and confirmed baselines.
Neovim owns unsaved typing. Save, comment publication, and batched submission carry immutable captures
into the admitted service task. The router does not adopt captures separately, and rejected concurrent
operations leave the document unchanged. No per-keystroke acknowledgment participates in editing.

## 84. Host-owned review field lifecycle

ForgeRuntime owns ReviewService alongside GithubService. The review service holds a registry of
at most 64 PR documents, admits at most 32 asynchronous jobs, and shares a 16 MiB EditBudget across
retained field text. Open, save, and reconciliation use the existing GithubService resource queue.
They do not create another GitHub process pool or mutation queue. Completed job handles remain
owned until collection. Runtime shutdown closes review admission and joins its jobs with the
remaining shared owners.

review.open_pr accepts a directory and validated PR target. It reads identity and editable text
through GitHub reconciliation before allocating a document identity. The initial title and body
baselines come from that remote observation. A failed or abandoned open releases its document
admission. Document identities do not repeat within a host lifetime. The current ReviewDocument
owns title and body fields and their save batch. PR rendering, comment ownership, and its planned
BufferDocument composition remain migration boundaries.

Explicit review saves carry immutable full-field captures with document identity, region revision,
and sequence. Typing remains in Neovim and sends no per-edit request. The host validates the capture
before submitting title and body in one updatePullRequest mutation. Omitted clean fields remain
unchanged. Confirmation advances captured baselines. A remote panic or delivery error retains
explicit uncertainty rather than implicitly replaying the mutation.

review.reconcile reads current remote text through the same resource owner and updates unresolved
baselines without replacing local text or reposting the write. A failed observation retains the
unresolved capture. If text admission fails partway through a multi-field reconciliation, settled
fields retain the observed baseline and remaining captures stay unresolved for a later observation.
review.snapshot reports current text, baseline, revision, sequence, dirty flags, and operation state.
review.close removes the view's registry entry. An admitted job retains its document and permit
until it settles, so closing a buffer or dropping a request receiver cannot cancel an active write.

GitHub reconciliation receipts now include observed title and body. Completed review and GitHub
responses use the same bounded, request-correlated result transfer when encoded output exceeds a
single frame. The host integration fixture transfers a 200 KiB body of quote characters with exact
current and baseline text. Review failures reach their Lua request callbacks without invalidating
Harness state. The existing PR editor still uses its previous rendering and baseline adapter until
the native review consumer adopts these host documents. No review UI parity or migration gate is
established by the host API alone.

## 85. Native PR field consumer cutover

The PR view binds title and description regions through forge.review to one ReviewService document.
The initial host observation supplies saved baselines. If native text still matches the initial
rendered field, the view adopts current remote text. If the user typed while the read was pending,
the view preserves that text as a local draft. The adapter validates document identity,
field revisions, edit sequences, field bounds, and dirty-state consistency before adopting results.

Text-change events retain native values and capture sequences locally. An explicit save captures
title, description, and reviewer text before dispatching review.save. A queued save retains that
capture while later typing continues. Settlement advances the captured region revisions and adopts
host baselines while preserving newer pending text. Lua compares native values with confirmed or
explicitly queued values for presentation. It does not publish newer typing implicitly.
github.pull_request.transition_async now handles lifecycle transitions only.

Failed saves observe the host snapshot and reconcile an uncertain result without reposting. Failure
notifications remain visible even after the originating buffer closes. A failed acknowledgement or
lost host preserves native text and stops operation dispatch against the obsolete identity. Reopening
the retained PR validates a replacement host snapshot and restores local fields and comment boxes
into the same buffer. Baseline or inline-revision conflicts retain the original document. Host
document identities include a service UUID, so an old document request cannot resolve to a new
host's document with the same ordinal. Closing during open releases the late document, and closing
before save dispatch completes the caller without sending a mutation.

Empty descriptions contain one empty editable row. The renderer preserves CR characters and
trailing empty rows instead of normalizing the source or inserting an editable placeholder. A
folded description has no native editable region and retains its existing field value. Generated
check rows below the folded section cannot become description text. Clean views remain interactive
while the initial host read runs. Dirty fields and pending save captures prevent replacement renders.

PR title labels occupy a native gutter, leaving the editable title at source column zero. Native
file headers own collapsed folds, semantic change counts, and opaque diff targets. Opening a fold
submits an `expand` action that Rust restricts to retained file diffs. Overview and batched review
modes rebind their command specifications without changing editable-region ownership. The overview
preserves metadata order, compact SHA labels, section spacing, and Checks before Changes. Activating
Status returns only the native lifecycle choices available for its retained observation. The shared
Lua chooser displays those choices and sends a fresh captured action, which Rust validates against
the same status target before admitting a transition. Full and incremental comment projection share
one label and fold constructor, so unchanged rematerialization publishes no patch.

The native UI fixtures cover held acknowledgements, newer typing, initial remote adoption, folded
descriptions, exact empty/trailing rows, invalid responses, late opens, close-before-save, and
read-only recovery. A real Forge executable plus native gh fixture verifies confirmed and uncertain
save flows through the Lua adapter with exactly two mutation attempts. PR Markdown layout, comment
ownership, reviewer and milestone mutation consumers, and the planned full ReviewDocument renderer
remain separate migration boundaries. No complete review parity or global allocation gate closes.


## 86. Comment ownership and remote merge conflicts

CommentStore assigns one stable CommentId and EditStore region to each remote comment or local
draft. Conversation and inline presentations receive independent CommentOccurrenceId values.
Removing an occurrence releases presentation state without removing its body or reply draft.
Focus selects exactly one occurrence. Cursor promotion requires an inline, viewer-authored comment,
while an explicit open can expand a read-only or conversation occurrence. Overview mode retains
one inline reply draft per remote parent. Batched review mode rejects that editor.

ReviewDocument contains the comment index alongside its existing shared EditStore. Comment bodies
use the same retained-text budget as title and description. The index validates document identity,
remote node and database identity, PR-scoped browser URLs, and revision-bound source anchors before
admitting text. Default comment admission permits 256 owners, 1,024 simultaneous occurrences, and
4,096 occurrence lifetimes. The shared 256-field default also includes title and description.
Occurrence identities are never reused. Failed admission does not consume a comment identity.

A clean remote update advances the region revision. A remote value matching current local text
advances the baseline without changing the local revision or sequence. A divergent remote value
retains a conflict candidate alongside local text and the previous baseline. All three values share
the existing byte budget. Allocation failure preserves the previous candidate and body. Pending
saves reject refreshes before metadata changes, and unresolved conflicts reject new saves.

Conflict resolution validates the current region revision. KeepLocal adopts the observed remote
baseline and preserves the local draft. TakeRemote replaces current text, advances the revision,
and releases obsolete retained values. Typing the candidate value also resolves the conflict.
Counter exhaustion rejects a revision-changing resolution without losing any version. A remote
reversion to the saved baseline clears the candidate while retaining local edits.

Browser actions resolve an explicit comment occurrence or source-code anchor. Adjacent code does
not inherit a nearby comment URL, and an unsaved draft has no remote comment URL. These ownership
and merge contracts are tested directly in Rust. Remote comment ingestion, create/edit/reply/delete
receipts, host routes, Lua consumer adoption, and full ReviewDocument rendering remain open. This
foundation does not establish comment UI parity or close a migration gate.


## 87. Comment mutation capture and settlement

CommentStore.save captures explicit create, edit, reply, or delete intent with the owner's current
body. One pending operation per comment prevents repeated posting while a receipt remains unsettled.
The receipt retains its shared EditStore text charge after the document closes. Deletes capture even
a clean body. Save rejects empty bodies, read-only owners, deleted owners, and unresolved conflicts.

Settlement checks receipt identity against the pending operation. Rejection releases admission without
advancing the baseline. Other outcomes first retain an uncertain capture. A matching confirmation
then reconciles the baseline while preserving newer native text. Invalid text, identity, anchor, or
resource evidence leaves the operation uncertain. Observation allocation failure also preserves its
capture, body, baseline, and retry prohibition. Edit and delete observations require the exact existing
remote node and database identity. Creation observations must refer to a new identity with the expected
source anchor and viewer ownership. The native caller must supply evidence correlated with the actual
creation operation. An arbitrary matching comment is insufficient evidence for that caller.

Unknown creation cannot be resolved by an absent result. The owner cannot prove that a lost mutation
response means no comment was created. Native routing must obtain positively correlated evidence or
keep that operation unresolved. No automatic mutation retry occurs in this lifecycle. A confirmed reply
adopts its remote identity and frees the parent's draft slot, allowing a subsequent distinct reply.

Confirmed deletion and observed absence of an existing remote target mark the owner deleted and
adopt an empty remote baseline. Retained native text remains available for recovery. Deleted owners
reject new mutations, stale refreshes, and new replies. An uncertain delete that observes the original
remote comment settles without reissuing deletion. Tombstone presentation and explicit draft recovery
remain part of the pending UI integration.

This implementation supplies in-memory operation ownership and settlement contracts. It does not yet
execute comment requests, persist receipts across host restarts, expose host comment routes, or replace
Lua mutation consumers. Remote fetching, correlated creation evidence, native cancellation/reaping,
and consumer cutover remain required before comment mutation parity is established.


## 88. Scoped remote reconciliation and shared mutation execution

A PR title/body observation cannot prove the outcome of a comment mutation. RemoteQueue now keeps
an evidence scope alongside uncertainty while preserving one FIFO owner for the whole repository
resource number. PullRequest, Comment(node identity), and Creation(operation identity) scopes share
write ordering. Any unresolved scope blocks all subsequent writes to that resource. Reconciliation
clears uncertainty only when its scope matches. Unrelated observations remain read-only and leave
the original uncertain scope intact, including queued mutations admitted before the failure.
ReviewService leaves comment-only uncertainty with its comment owner. An explicitly closed unknown
PR operation adopts the subsequently observed title/body baseline while retaining local edits.
Confirmed recovery still requires exact equality with every captured field. A failure before draft
publication and remote dispatch releases its in-memory save capture without retrying closed storage,
and preserves `MutationNotStarted` evidence for the caller.

Scope identities contain at most 256 ASCII graphic bytes. Admission charges their text alongside the
existing 16 MiB retained input bound and rejects overflow before acquiring an operation slot. At most
64 uncertain resources retain one bounded scope each after request completion. A read-only owner
cannot create or replace mutation uncertainty. Creation scopes identify individual operations, so an
observation for a different creation cannot enable reposting of the unresolved operation.

GithubService.run_mutation centralizes queue admission, task retention, shutdown selection, and result
delivery for typed mutation adapters. The existing pull_request path uses this executor with the PR
scope. The executor retains active operations independently of their response receivers and drains
them during shutdown. Native comment adapters must use this same executor with the matching evidence
scope. This change adds no alternate process pool and does not yet execute native comment requests.

Queue regressions verify cross-scope FIFO ordering, queued-write rejection after an unknown result,
unrelated observation isolation, exact-scope recovery, identity limits, and byte overflow. Service
regressions verify the same uncertainty boundary and retention of an active comment-scoped operation
after caller cancellation through a shutdown deadline. Native comment request construction, correlated
creation receipts, host routing, and Lua consumer adoption remain open migration boundaries.


## 89. Native existing-comment operations

forge-github defines typed existing-comment targets, observations, edit/delete requests, and result
outcomes. GithubService.comment validates requests before queue admission and reads the remote owner
before a write. Comment kind, node identity, database identity, and the PR-scoped URL must match.
Read-only owners and missing comments reject mutations. An already-matching body skips the write.
The service uses the existing comment evidence scope and shared mutation executor. A malformed or
mismatched write confirmation retains uncertainty until explicit read-only reconciliation.

GithubCommentRemote separates the comment capability from issue synchronization methods while GhClient
and its directory-bound view share the existing native process owner. Comment reads, edits, and deletes
use GraphQL variables encoded into temporary input files. Conversation edits select updateIssueComment,
and review-comment edits select updatePullRequestReviewComment with pullRequestReviewCommentId. Deletes
select the matching delete mutation. The client requests fullDatabaseId and accepts exact integer or
decimal-string representations, rejecting lossy values. The existing cross-language identity bound
remains 2^53 - 1.

Mutation requests include a bounded clientMutationId receipt. A response must echo that receipt before
the native client accepts its result. This correlation check does not provide server-side idempotency.
Only proven pre-execution admission failure is classified as rejected after mutation dispatch. Process
failure, GraphQL errors, malformed JSON, missing fields, or foreign receipts remain uncertain. Native
work is not reported complete until the shared process owner has collected it. Reads distinguish an
explicit null node from missing data and reject GraphQL errors before interpreting absence.

Deterministic native fixtures exercise both comment kinds, correct GraphQL identity input keys,
64-bit identities, raw Unicode and trailing rows, confirmed edits and deletion, lost responses, and
foreign receipts. Unknown results reconcile with one mutation attempt. Isolated service tests reject
foreign observations, read-only owners, invalid bodies, and mismatched confirmations. These tests do
not perform live GitHub writes. Schema references are GitHub's Issues and Pull requests GraphQL pages:
https://docs.github.com/en/graphql/reference/issues and https://docs.github.com/en/graphql/reference/pulls.

Native create/reply operations, remote comment-list loading and source-anchor reconstruction,
ReviewService receipt adoption, host routes, persistent receipts, and Lua consumer cutover remain
open. Existing Lua comment mutation consumers remain active until that integration is complete.


## 90. Host routing for existing-comment operations

GhClient.for_directory now returns a concrete shared GhDirectory handle. The handle exposes both
GithubRemote and GithubCommentRemote without allocating another process pool. Existing callers
coerce the same handle to their required capability. Directory validation and native admission remain
centralized in the original client.

The github.comment host route accepts strict directory, target, and operation parameters and dispatches
through GithubService.comment. It runs independently of Harness initialization. Confirmed, rejected,
uncertain, and reconciled results retain their typed outcomes. Completed responses use the existing
bounded result-transfer mechanism when JSON exceeds one frame. Invalid requests and uncertain-resource
errors return through the caller's response channel. Lua does not invalidate Harness state for these
host comment failures.

Host integration tests connect the actual Forge process to a deterministic native gh fixture. They
verify checkout context, exact Unicode text, lost responses, prevention of repeated mutation, explicit
reconciliation, and unknown-field rejection. A separate 200 KiB quote-heavy comment observation crosses
multiple response frames and is assembled under the original request identity. The Lua transport fixture
verifies comment failure isolation. These fixtures issue no live GitHub writes.

The route exposes existing-comment operations. It does not replace the ReviewDocument saved-baseline
owner or complete the PR comment consumer cutover. ReviewService comment ownership integration,
creation/reply requests, comment-list loading, persistent receipts, and native UI adoption remain open.


## 91. Review-owned comment baselines and native settlement

ReviewDocument now owns native comment targets alongside CommentStore and the shared EditStore.
ReviewService retains a combined ReviewRemote capability for PR and comment operations. Loading an
existing comment validates its target against the document's repository, PR number, and PR node
identity, then obtains the body and saved baseline from GithubService's native observation. Caller
anchor hints are validated as source coordinates but are not yet reconstructed from native comment
loading. Canonical comment-list and source-anchor loading remain separate migration work.

The review.comment command supports load, snapshot, save, reconcile, and conflict resolution. Snapshot
returns stable comment and region identities, region revision, edit sequence, current text, saved
baseline, conflict candidate, viewer ownership, deletion state, and save uncertainty. Native region
edits route through CommentStore's viewer-ownership check. Snapshot and region edits remain available
while a remote comment operation runs.

One document-level remote guard serializes PR and comment operations. Saving captures existing-comment
edit or delete intent from the shared EditStore and generates a UUID receipt for the native request.
Settlement advances only the submitted baseline, preserving newer native text. Unknown results retain
the pending capture and block subsequent saves. A PR reconciliation cannot settle a pending comment.
Comment reconciliation uses its matching native target and adopts observed truth without reposting.
Refreshed remote conflicts require a matching region revision before KeepLocal or TakeRemote resolution.

The guard retains an active native operation after its request receiver or document view closes. On
panic or an abandoned active save, it conservatively records uncertainty and releases the active flag.
The service retains submitted mutations and uncertain outcomes through the durable recovery store.
Replacement hosts restore those records and require explicit reconciliation before another publication.
Deleted comments retain native text for the pending recovery presentation.

The real Forge host fixture verifies native loading, captured text, confirmed baseline adoption,
lost responses, newer text, and reconciliation with exactly two mutation attempts. Isolated service
tests cover held writes, snapshot access during save, PR/comment exclusion, closure through shutdown,
remote panic, foreign document targets, and revision-checked refresh conflict resolution. Native create
and reply operations use client-owned draft definitions carried by explicit captures.


## 92. Native conversation-comment creation

GithubService validates a conversation submission before admitting it to the shared PR resource queue.
The operation reads the addressed repository and PR number and verifies the supplied PR node before
beginning the mutation. A foreign parent observation therefore fails before any write. The creation
uses the existing GhClient or GhDirectory process owner and the common bounded native executor.

AddCommentInput carries the parent node, exact body, and caller receipt. Confirmation requires the
returned clientMutationId and subject node to match that submission. The returned node must be an
IssueComment with valid node and database identities, a URL bound to the target PR, viewer ownership,
and the exact submitted body. Unicode, indentation, and trailing rows retain their original bytes.
The input schema follows [GitHub's Issues GraphQL reference](https://docs.github.com/en/graphql/reference/issues).

A malformed, lost, or mismatched confirmation retains uncertainty under the creation receipt. The
resource queue blocks subsequent writes without retrying addComment. The receipt correlates the
immediate response and does not establish server-side idempotency. Creation recovery cannot use the
absence of a comment or an arbitrary body match as proof of failure or ownership. Persistent creation
receipts and positively correlated recovery remain unfinished.

The native fixture verifies successful creation, foreign receipt, foreign subject, changed body,
foreign author, wrong comment kind, response loss after persistence, and foreign-parent preflight.
Each dispatched case records exactly one mutation, and uncertain cases reject another attempt.
ReviewDocument draft adoption, host command exposure, inline creation, replies, pending reviews, and
Lua comment consumer migration remain separate unfinished boundaries.


## 93. Review-owned conversation drafts and creation adoption

Neovim allocates a stable local comment identity and body region without contacting the host.
Explicit publication sends the draft definition and exact body text together. The admitted service
task adopts them into CommentStore and EditStore, generates a native receipt, and dispatches
GithubService creation through the document guard. Empty bodies fail before remote dispatch.

Confirmed creation adopts the returned remote identity into the same comment owner. Subsequent save
and delete commands use the existing-comment path. Saved-baseline settlement uses the captured body,
so newer local typing remains dirty against the submitted baseline. The comment and
region identities remain stable across creation and later mutations.

A failed PR identity preflight returns a proven rejection before mutation admission begins. A proven
rejection releases the capture and permits an explicit retry. Unknown creation retains the
capture and draft without adopting a target. PR reconciliation cannot clear this uncertainty, and
existing-comment reconciliation cannot substitute an arbitrary observed node. Creation recovery still
requires positively correlated evidence and persistent receipt storage. Active creation survives
caller cancellation and document removal through service shutdown. Replacement hosts restore uncertain
creation records without reposting the submitted body.

Five service tests cover creation followed by edit and deletion, newer text during a held creation,
rejection and failed preflight followed by explicit retry, uncertainty isolation, and caller/document closure during an
active write. A native Forge host fixture verifies draft capture, exact text, GraphQL
creation, receipt correlation, unchanged local identities, and exact saved-baseline adoption. These
tests issue no live GitHub mutations. Service tests also cover inline creation, replies, batched reviews,
and uncertain creation recovery after host replacement.


## PR buffer loading

Editable PR descriptions retain their exact Markdown source rows. Source-preserving Markdown metadata supplies links and structural formatting, and the shared Rust syntax engine supplies Markdown captures and fenced-language injections before publication. The review window conceals presentation markers outside the active cursor line. Accepted description edits request a fresh projection so removed fences and stale syntax decorations do not persist.

Opening a PR retains the current buffer until its title, metadata, and description are ready. Initial loading reads overview, editable fields, and requested reviewers concurrently by repository and PR number. `review.header` consumes the retained overview and reviewer responses without repeating either request. The first visible native presentation contains the complete header and description, with normal heading highlights and dark gray `Loading...` rows in pending sections. Leaving the originating buffer before presentation prevents a late response from replacing the newer buffer.

The Lua adapter retains up to eight PR documents and their native buffers for the Neovim session, keyed by workspace, hostname, repository, and PR number. Reopening a retained PR displays its existing text and loaded sections immediately, then refreshes header and secondary sections concurrently. Clean title, description, and requested-reviewer fields adopt remote changes. Local edits and pending saves remain intact. Explicit buffer deletion releases the retained document.

Title, description, and requested reviewers have native editable regions. Cursor entry unlocks the selected region and restores native editing bindings. Unsaved fields display `*`. Both `<C-s>` and `:w` immediately capture native values as a local save intent and clear their markers and the buffer's modified flag. Lua keeps this presentation baseline separate from the host's confirmed baseline. Typing after the local save restores the affected field's marker.

Explicit save and submission actions preserve the exact native field text, including CR characters, Unicode, internal line breaks, and trailing empty rows. Capture does not rewrite the buffer. Native anchors follow local edits, and immutable captures cross the host boundary only for explicit operations.

The PR and review `b` command remains available over editable fields in normal and visual mode. It sends a distinct `browse` action with the captured document, view, revision, and position. Rust preserves explicit browser targets and otherwise opens the PR page from its captured host, repository, and number. Browsing cannot activate lifecycle changes, file expansion, or comment editing. Issue browsing uses its source target or issue URL. Notification browsing uses the selected notification URL and reports missing URLs and launch failures.

PR file headers own working-file opening through the `open` action and diff loading through fold expansion. No separate working-file action row appears in the document. A stable empty trailing block bounds each file fold while incremental diff rows arrive. Status snapshots normalize an empty `headRepository.nameWithOwner` from the separately supplied head owner login and repository name, preserving fork identity for immutable source reads.

Editable regions protect their surrounding rows and headings independently of cursor-based `modifiable` settings. The shared Lua guard retains the last permitted buffer text, updating only affected rows for accepted edits and generated patches. A cross-boundary edit captures extmarks before Neovim moves them, suspends projection effects, and restores the retained text and marks on the next main-loop callback. Rejection leaves accepted field edits and native region anchors intact, sends no invalid edit to Rust, and keeps the fields editable. Only a failed restoration faults the document.

Lua retains one active publication operation and at most one queued explicit action across field saves, comment publication, batched submission, and lifecycle changes. Each action captures its input when invoked. A newer queued action replaces the previous queued action and reports supersession to its callback. Completion dispatches the retained action without recapturing later typing. Unresolved submission recovery holds publication until its outcome is settled. Rust saves title and description together, then applies reviewer additions and removals through the durable mutation queue. Completion adopts confirmed baselines without replacing native text. The queued capture continues to control markers while the earlier request settles. Presentation refresh waits while local drafts remain pending. A deferred refresh does not mark reconciliation as active, and a refresh started before a save completes cannot restore older field baselines.

Rejected and missing save results retain later explicit captures, restore markers against confirmed baselines, and notify the failure. Uncertain field saves start `review.reconcile` without reposting mutations and hold later publication until recovery settles. Rust reads current remote fields or the reviewer set before settling the durable capture. Matching title/body observations verify the submitted operation. Reviewer changes close the unknown operation after observing the current reviewer set because GitHub provides no reviewer-operation identity to link. Differing title/body observations also close the unknown operation. Recovery establishes the observed baseline without claiming that a closed-unknown request succeeded and preserves all locally owned text. Failed observations retain captures, keep native editing enabled, and expose `gR` to retry recovery. An explicit save during recovery retains its capture for later dispatch.

Uncertain comment snapshots hold later publication without replacing its queued capture. The `gR` recovery picker offers remote observation, a linked confirmed comment ID, not-dispatched rejection, and close-unknown resolution. Cancellation sends no request. Comment recovery bypasses only the comment-uncertainty guard and retains the queued publication action. Successful settlement resumes that action, while failed recovery keeps it held. The same picker presents batched submission resolution through `gB`.

Reviewer completion excludes the authenticated user only in the reviewer input. Ordinary `@` mentions still include that user. Lua validates manually entered reviewer names case-insensitively before dispatch. Rust repeats validation before any field write and again before the reviewer phase, so a self-review request cannot produce a partial title/body save or bypass validation by changing during an earlier write. The buffer enables the shared `@` username and `#` issue completion sources, with metadata refreshed at open and issue queries served from the local index.

After the first presentation, `review.load` starts file, check, conversation, and commit reads concurrently in Rust. Lua requests one presentation refresh after the batch settles. Successful sections survive failures in other sections, and failed sections display diagnostics. The shared GitHub service admits at most sixteen remote jobs, independently of storage-worker admission. Native `gh` execution runs at most four subprocesses concurrently. Additional admitted requests wait asynchronously for native job and input-byte capacity, and shutdown wakes waiting requests with a closed-pool error. Section updates serialize against presentation capture without blocking editable-field acceptance during analysis.

## 98. Native batched-review ownership

ReviewDocument owns an explicit `Overview` or `Batched` mode. Batched mode adds an independently editable review-summary region and a repository-relative viewed-file set. Both values are written into the PR-scoped durable review draft, then restored before the document is published. A failed draft write reports the request failure and prevents Lua from adopting the returned mode state.

A batched submission captures the summary and every local inline draft body with its immutable revision, path, line range, and side. The captured `ReviewMutation::ReviewSubmit` contains those comments in the single review endpoint request. The operation ID and complete mutation capture reach durable draft storage before dispatch. Confirmation advances the summary baseline and hides submitted local drafts. Rejection releases the capture without changing local text. An unknown outcome retains the operation ID and immutable capture across reopen, blocks another submission, and requires an explicit recovery action instead of automatic replay. Recovery validates the retained mutation against the recovery journal before it accepts a linked confirmed review ID, an explicit not-dispatched rejection, or close-unknown. A missing journal cannot establish confirmation.

Lua obtains mode, viewed state, and submission outcomes only through `review.begin_batched`, `review.set_viewed`, and `review.submit_batched`. It opens a compact comment fold before focusing the corresponding editable occurrence. Command specifications retain ownership of the configured review bindings.
### Commit-scoped diff expansion

Commit detail reads resolve one immutable full commit identity to its first parent and subject through `GithubReviewRemote`. Expansion compares that parent SHA to the selected commit SHA. A commit with no parents produces an explicit root-commit unavailable state because no comparison base exists.

`forge-review::commit_diff::CommitDiffStore` owns expanded commit rows separately from pull-request file state. Each record uses the full commit SHA and repository-relative path as its key, retains the subject target across expansion, and admits at most 64 records and 16 MiB. Failed admission preserves every existing record. Path-only pull-request `RetainedFile` entries never satisfy or replace a commit-scoped lookup.


## Harness task ownership and recovery

The broker owns task transitions. Neovim presents task history and submits durable intents.
Command completion keeps subcommands within their typed parent command. Opening `/task new`
retains the conversation and current execution until a replacement task is submitted.
Session listing and resumption compare existing workspace paths after filesystem resolution,
so alternate separators do not hide a session from its worktree.
`TaskRecord` identifies a conversation-scoped Plan, Execute, or Goal workflow, its saved
permission, phase, lifecycle status, generation, and attempt identity. Plan and goal records
retain their workflow-specific evidence. `current_task_id` selects the task independently of
the conversation permission. Clearing a task retains its history and artifacts.

`/task` lists the current task first and previous tasks by activity. `/task new` offers the
three workflow kinds. `/task resume`, `/task pause`, and `/task clear` operate on the selected
task. `/execute` selects a submitted plan and `/execute last` selects the most recently
submitted plan in this conversation. Selection accepts the exact revision and transitions its
Plan task into Execute. Terminal executions require an explicit fork. Forking a plan uses its
latest saved design and reassesses the current repository without copying execution evidence.

The default writable permission is Write and applies to new Goal tasks and accepted plans.
The Plan permission defaults to Keep current. Both preferences live in `/config`.
Explicit resume restores saved permission. Selecting Read pauses Execute and Goal tasks.
Interrupting retains the selected task. The next ordinary message resumes a paused Plan,
Execute, or Goal task with its saved context and the message as additional instructions.
Control-triggered resumption records a lifecycle title on its exchange instead of a user
prompt. The title identifies the task and any permission or configuration change. Backend
continuation instructions remain separate, including the native goal activation command.
Clearing, switching, or completing the task ends that message association. A resumed plan
awaiting review still requires explicit acceptance before execution.
Changing permission while paused does not restart execution. Model changes during execution
use the same stop-and-resume coordinator. MCP configuration changes require a paused task.

`task.transition` persists an operation ID on the ordered host input lane before concurrent
dispatch or acknowledgement. A compare-and-set claim permits one execution of each intent.
Pending pause or clear intent takes precedence over stale Running records when settings change.
Settings received before a new task acquires its identity reject visibly without displacing
that task. The coordinator
closes conflicting execution admission, collects the previous provider attempt, finishes
checkpoint finalization, and applies the latest intent. Superseded intents cannot start a
provider attempt. Ctrl-C submits a pause intent. Backend goal continuation stays inside the
service-owned execution lifetime rather than relying on Lua to submit the next turn.
The execution permit, rather than a contended broker mutex, identifies provider work that needs
cleanup. Catalog and snapshot readers cannot cause an idle permission change to interrupt the
provider. Cleanup after the steering receiver has settled is a no-op, while steering input still
requires an active receiver. The execution permit remains held through finalization and delivery.
`task.operation` queries the original ID after uncertain acknowledgement and never replays it.
User cancellation remains a cancelled operation across broker and service error boundaries.
The coordinator associates each settled operation with its exchange while holding execution
admission. Snapshots show terminal operation status only while that exchange is current,
while direct operation queries retain the historical outcome. Lua clears a superseded
operation notice without clearing a newer unrelated diagnostic.
Failed status reads preserve the admitted operation state and retry with a visible diagnostic.
Resuming an execution with a pending plan revision restores its review wait without starting
the provider or treating the unmet review prerequisite as an execution failure.
An explicit pause clears review-triggered continuation even when review already paused the
goal. Approval can continue a terminal exchange only by admitting a new exchange within the
same execution. Completed, cancelled, interrupted, and failed exchanges remain immutable.
A successful read clears that diagnostic. Local admission rejection carries `not_admitted`,
so the UI does not report an unknown write outcome for a request that never left Neovim.
Startup configuration, task transitions, status queries, and health checks share reserved
client capacity that ordinary presentation requests cannot exhaust.
The host reserves four of its 64 request slots for the same control methods. Concurrent
health, operation, cancellation, and approval requests can use those slots and borrow unused
ordinary capacity. Ordinary requests cannot consume the four reserved slots.

SQLite uses WAL and FULL synchronization. Acceptance commits the task, goal, execution,
accepted revision, and acceptance event together. Session ownership includes an operating-system
file lock, so an expired timestamp cannot admit a second live runtime. Restart recovery retains
committed phase and completion evidence, suspends unfinished attempts, and marks unsettled
operations outcome-unknown. No recovered task starts a provider automatically.
Exchange recovery retains the last persisted active elapsed coordinate. It excludes the
offline interval and may omit activity after the last saved provider event.
The approval picker shares the configured Harness interrupt key. Interrupt closes the picker
and pauses the task without approving the pending tool. Closing with `q` leaves approval pending.
Review UI starts execution only when acceptance creates an initial acceptance record. An execution
revision emits a foldable `Plan revision requested` record followed by `Plan revision accepted (auto)`
when automatic approval is enabled. Manual review retains the submission tool response and pauses
exchange timing without cancelling the provider. A bounded review channel serves plan capture, review
edits, acceptance, requested changes, and rejection through the broker that owns the running exchange,
without acquiring its execution mutex. A stale review fails without releasing the pending tool.
Acceptance, requested changes, or rejection returns the canonical document, current generation, and
feedback to the same control runtime before its next tool call. These decisions never request another
exchange while the provider is waiting. Cancellation drops the review channel and leaves the persisted
revision pending for recovery. Approval after interruption starts a new exchange through the existing
resume path. Acceptance retains its
anchor at the end of the planning exchange. The first execution exchange starts with
`Plan implementation started` and never takes ownership of the acceptance event. Each subsequent
exchange identifies the phase as `Plan implementation`, `Plan verification`, or `Plan resolution`,
including when it starts, continues, pauses, or resumes. An exchange that ends without a phase
result reports that phase as stopped. Only recorded phase results claim phase completion.

Activity headings use `Implementing`, `Verifying`, and `Resolving` while a phase runs,
then `Implemented`, `Verified`, and `Resolved` when its completion is recorded.
Verification outcomes remain separate from the activity fold. A passing result is a diamond
event. Failed and blocked results show the first finding in a collapsed heading, with the phase
summary and all findings inside its lazy-loaded section. The broker persists the effective
outcome, summary, and findings on that exchange before transitioning to the next phase, so later
resolution and restarts cannot replace an earlier verification's reason. Structural findings
remain visible even when the agent reports that its behavioral checks passed.

Health requests use reserved admission and output scheduling independently of the broker
execution lock. The UI sends at most one outstanding health request per conversation. Ten
seconds without a response shows connection uncertainty. Thirty seconds without provider
activity shows an explicit wait without failing or repeating work. Bulk output remains bounded,
and control frames receive priority between complete output frames.

Snapshots are scoped to their originating conversation, host generation, runtime epoch, and
monotonic snapshot revision. An older snapshot cannot replace newer task state. A new runtime
clears obsolete UI operation ownership while retaining an uncertain durable operation outcome.
Task status responses omit the original prompt and bound error text to 4096 characters.
Transition admission and finalization waits stop after ten seconds with a visible diagnostic.
Failed state
refreshes remain visible and retry after one, two, and four seconds. `/task refresh` requests a
new authoritative snapshot. Failure, disconnect, and unknown operation outcomes suppress
automatic queue draining. Queued input retains task identity and is not retargeted by a switch.


## Plan implementation and conformance

Implementation cannot be interrupted by declaration reconciliation. Implement writes the accepted
behavior and tests, permits justified source deviations, and defers builds and checks to Verify.
The tool catalog omits plan mutation controls in Implement and Verify. The control runtime and
broker independently reject those mutations, including calls from stale provider sessions.

Verify compares body-free declaration structure and configuration values against the current
accepted revision. Required declarations, signatures, ownership, and exposed API remain binding.
Additional internal helpers are permitted. Calls and Accesses are descriptive evidence, never
completion gates. Verify collects structural and behavioral findings together. Resolve fixes the
workspace or submits a consolidated contract revision, then returns to Verify. Plan revisions
retain the existing review-wait protocol and never replace the original accepted baseline.

At completion or a blocked result, the broker records an implementation-differences report against
the original accepted revision. Failure, cancellation, and continuation exhaustion retain an
incomplete report. Reports compare accepted paths and checkpoint changes, including removed files.
They distinguish unspecified reference categories from explicitly empty Calls or Accesses and show
internal additions, contract differences, approved revisions, and unavailable evidence.

The report is an immutable content-addressed object. The execution record and timeline node retain
its identity and counts, not its full text. The timeline shares cached report data and exposes its
file details through the existing collapsed-section loader. Reopening a session preserves the same
report. A missing report object shows unavailable content rather than silently dropping the summary.
Report publication checks source identities before committing a successful phase result. A changed
workspace rejects that result for a fresh verification attempt, rather than pairing stale evidence
with a newer report.
