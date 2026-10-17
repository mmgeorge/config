# Git repositories, status, and checkpoints

Rust owns repository identity, observation, source acquisition, mutation admission, and checkpoint content. Consumers retain immutable source identities so review and restoration use captured bytes rather than the current worktree by accident.

Buffer publication is described in [Buffers and rendering](buffers-and-rendering.md). Host workers and cancellation are described in [Host and protocol](host-and-protocol.md).

## Windows Git configuration location

On Windows, the initialize handshake loads a versioned Git system-prefix location
cache from `stdpath("cache")/rust-sidecar/forge/git-config-location.json` before
repository reads. `forge-git::config_location` validates the complete record and
existing absolute directory, then seeds gix through the local `gix-path` patch.
A miss uses gix's default discovery and atomically persists the prefix. A hit avoids
the `git --exec-path` subprocess. Configuration and attribute contents remain live.
Non-Windows hosts and explicit `EXEPATH`, `GIT_EXEC_PATH`, `GIT_CONFIG_SYSTEM`,
`GIT_CONFIG_NOSYSTEM`, or `GIT_ATTR_NOSYSTEM` overrides bypass persistence.
Cache I/O failures report a warning and retain normal discovery without rejecting
startup. `:ForgeGitConfigCacheReset` clears the file. Restart Neovim afterward to
replace gix's process-local prefix, including when switching between installations
whose old directories still exist. Startup diagnostics record the cache state.

## Native status observation and mutation

`StatusDocument` retains the displayed HEAD and each file's index state, worktree stamp,
and rename-origin stamp. The private wire protocol uses the exact current version in `forge-protocol` and `protocol_contract.json`. Status, local previews,
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
Native syntax adoption merges adjacent display groups when their scopes are equal or nested
and the gap between their padded windows is at most six lines on both source sides.
Each group retains the union of its raw hunk identities, so actions on bridge context stage
or unstage exactly those edits. Semantic bridging leaves larger gaps and unrelated sibling
scopes separate. Hunk headers contain change counts. Enclosing declarations appear as
syntax-highlighted source rows, with their exact old and new coordinates. Unchanged scope openers appear in the first containing group and closers
in the last, with each source coordinate emitted once across the file's groups. Syntax
adoption replaces the body generation before publishing the regrouped targets and rows.
Omitted ranges inside a group render an indented `...` row without a source coordinate
or mutation target. A single omitted unchanged line renders its actual source text.
These markers preserve chunk boundaries without consuming a source line. Staging a raw
hunk recomputes groups and source context independently for the index and worktree.
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
direction into one `git apply` input per group of at most 256 targets. This grouping
bounds each command's target count, not the complete selection or patch bytes. Staged-hunk
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
The marker store keys worktrees by their canonical root, so symlinked directory
names share the same state and errors. Local-file views resolve the parent
directory without requiring the selected file to exist, preserving deleted-file views.

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

## Commit-message context

`CommitMessageService` owns prompt preparation for automatic About drafts and explicit
commit-message regeneration. HEAD includes tracked worktree changes and untracked files,
staged generation compares the index with HEAD, and amend generation reads the latest
commit's immutable old and new objects. All three routes use the same native diff and
excerpt formatter. Ignored paths remain excluded. Binary and unsupported sources retain
an inventory entry without a text excerpt.

The constructed system instructions and user prompt together contain at most 500,000
UTF-8 bytes. This application limit counts text before JSON escaping, not model tokens
or provider-added context. The prompt starts with changed paths and full addition/deletion
counts, followed by excerpts in path order. Paths whose filename contains `test` or `spec`,
or which lie under a test/spec directory, follow other paths. No lockfile classification
or dependency summarization runs. Inventory emission stops if the inventory itself reaches
the byte limit.

The shared syntax engine supplies declaration extents, signature rows, and recognized
import ranges from exact old and new sources. The formatter assigns a source row to its
innermost declaration and emits it once. It preserves complete signatures and selects
the first 20 changed body rows per declaration, counting additions and removals together.
Two unchanged rows surround selected changes, overlapping context merges, and omitted
ranges use `... N lines omitted (+A -D)`. Gutter prefixes are `+ `, `- `, and two spaces.
Imports remain in inventory counts but are excluded from excerpts. Fully omitted files
receive no detail heading. The global limit stops subsequent emission rather than
reducing the per-declaration allowance or redistributing it to later files.

Markdown and plain-text paths (`.md` and `.txt`, case-insensitive) retain every changed
row with two context rows and have no 20-row limit. Unavailable grammars and malformed
syntax use ordinary diff excerpts with the same byte limit. Python currently follows
that fallback because this host does not bundle a Python grammar. Optional syntax work
has a shared two-second deadline, after which remaining files use the diff fallback.
Repository acquisition and comparison failures remain request failures rather than
being converted into an empty change set.

## Exchange checkpoints and change attribution

Every Git interaction captures a baseline before its first provider turn and a terminal
checkpoint when it completes, fails, or cancels. The first baseline is checkpoint zero,
including pre-existing staged and unstaged content. Steering within an exchange retains
its baseline. Each later exchange captures the current workspace before starting.

`CheckpointRecord` stores an existing Git tree ID, complete content overrides, deletions,
file modes, sampled checkout conversion rules, and explicit Git LFS exclusions. Capture never
creates commits, changes refs, stages files, or writes the user's index. Native Git status uses
the index to select changed files, while a concurrent `git ls-files` supplies tracked and
nonignored untracked names. Status refreshes a private temporary copy of the index through
`GIT_INDEX_FILE`, so its physical file-size observations never alter the user's staging.
The temporary directory owns that copy and removes it after capture, including failed captures.
Both bounded commands retain repository admission until both children exit. Attribute
resolution shares one gix attribute stack and reads selected values
directly, without rebuilding a content-conversion pipeline for every path.

Git LFS files remain outside checkpoint ownership. Status excludes `filter=lfs` paths before
content comparison, and capture never opens their contents, runs their filters, or copies
their objects. Saved exclusions include untracked LFS names. Reads, comparison, and restore
exclude the union of saved and current LFS paths, including overrides from older records.
Changing attributes later cannot make a saved exclusion restorable. A pending restore journal
rejects a path that becomes LFS before reading or writing its contents.

Clean ordinary files come from existing tree metadata, not a second walk of worktree files.
Only changed files, unsupported non-LFS conversions, and platform-specific symlinks require
content acquisition. Conversion-sensitive clean files use the refreshed index sizes and Git
object headers to preserve their physical line endings without a second worktree metadata scan.
A process-local cache compares status, index identity, path names, configuration, selected attributes,
refreshed checkout sizes, and captured source metadata before reuse.

The initial status inventory defines the candidate boundary. An ordinary clean file edited
after that inventory belongs to the next checkpoint. Each candidate retains the bytes acquired
during its read, and a later source change prevents reuse of that acquisition. Capture does not
rescan every clean file or run status a second time. This preserves per-file capture semantics,
not an atomic filesystem snapshot.

Checkpoint capture and SQLite persistence must finish in under one second on the large WebGIS
worktree with its normal small set of non-LFS edits. The `checkpoint_latency` release example
measures fresh capture, reuse, and a new session with temporary durable storage, and fails if
any iteration reaches one second. Fresh capture means empty Harness caches, not flushed operating
system filesystem caches. Repositories with large changed non-LFS files still require reading
and durably storing those bytes.

Clean files resolve through the recorded tree, independently of the current branch.
Checkout rules are sampled with Git attribute precedence. Non-LFS external filters and encodings
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
per-operation provider trees remain available at their original timeline positions.

Non-Git exchanges retain an aggregate of successful provider-reported edits at finalization,
including edits from referenced child exchanges. That summary appears outside the activity node
and is labeled `reported edits`. It groups repeated paths but preserves individual edit hunks,
so its counts describe reported operations rather than a net workspace comparison. Reopened
history without a saved aggregate derives the same summary from its retained edit records.
No repository is initialized, and no checkpoint or rollback capability is implied.

The checkpoint total remains the rollback and cancellation-divergence authority. It intentionally
includes command, formatter, and external-process effects that do not belong to a structured
provider file change. A concurrent unattributed edit to a provider-reported path cannot be
separated without operating-system provenance, so that path remains attributed while the complete
checkpoint still preserves the exact rollback boundary.

## Shared Git repository identity

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

## Blocking repository read ownership

`forge_git::read_pool::BlockingReadPool` bounds admitted jobs and tracks their declared retained
input bytes without a byte admission ceiling. Admission counts queued jobs, running jobs, and
completed results awaiting collection.
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
on the caller's declared retained size. Operations preserve complete source and output content,
report actual acquisition failures, and check cancellation between interruptible stages.
Job-count admission bounds concurrent ownership through result collection, not total content size.

## Shared repository handle lifetime

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
and Harness checkpoint reads share its read admission. Handle count and declared input accounting
do not bound gix internal caches or source bytes. Streaming, cancellation, and cache eviction
control work without rejecting a source for its size.
Observation caches and complete mutation integration remain incomplete.

## Host repository ownership

`ForgeRuntime` constructs one shared repository store without opening a repository or provider.
The current host defaults allow 32 retained repository handles and four admitted native reads.
Declared retained read inputs, including supplied source allocations, remain accounted without a
byte admission ceiling. Handle and job counts do not establish process-memory bounds.

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

## Mutation admission and owner lifetime

Each `RepositoryStore` owns one `MutationCoordinator`. Index and worktree-file scopes use
`WorktreeId`, while shared-reference scopes use `GitStorageId`. Admission reserves queue and
receipt capacity before asynchronous preparation. The current store limit is 64 retained
operations. Scope validation requires distinct, nonempty scopes, while retained preparation-input
accounting imposes no byte admission ceiling.
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

## Harness checkpoint rollback admission

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
blocking worker. Checkpoint capture and diff callers use the shared read pool described below. Temporary
content buffers and SQLite decoding are not covered by the metadata reservation.
Per-target durable outcomes, permission integration, process termination, and recovery remain
incomplete. This first writer integration does not establish shared ownership for all writers.

## Checkpoint worker and object-store ownership

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

## Streamed checkpoint source acquisition

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

## Streamed checkpoint restoration

`ObjectStore::restore_file` verifies stored bytes into a private temporary file, then
replaces each destination from a same-directory temporary file. The transfer buffer is
64 KiB. This prevents an interrupted write from leaving a truncated destination. The batch
remains interruptible between files and retains durable progress rather than claiming
multi-file atomicity.

## Checkpoint restore preview and recovery

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

## Complete checkpoint diff sources

Checkpoint diff generation reads each complete source side. `ObjectStore::get` returns the exact
stored bytes, verifies their digest, and rejects observed descriptor metadata changes. It checks
addressability and allocation failures rather than imposing a content-size threshold. Empty
objects remain valid.

Capture, diffing, and streamed restoration share complete object identities. Binary content and
unsupported encodings retain distinct preview outcomes. A file's size does not turn an otherwise
valid preview into `Diff unavailable`. Restoration verifies bytes before replacing a destination
and preserves existing regular-file permissions.

Concurrency, cache eviction, cancellation, and bounded I/O chunks control resource ownership.
They do not impose a total file-size limit. Comparison still requires memory for line indexes,
working state, and output, and actual allocation or I/O failures propagate to the requesting owner.

## Shared immutable diff sources

`forge-diff` owns source contracts without dependencies on Git, Harness, status, or Neovim.
`SourceVersion` retains immutable bytes, a lazy SHA-256 content identity, a representation, and derived
newline metadata. Raw, Git-canonical, and display-only representations produce distinct analysis
identities even when their byte hashes match. Clones share the original allocation and one
thread-safe identity cell. The first identity request computes its hash once.

Construction retains complete source bytes without an arbitrary byte or line-count limit.
NUL-containing bytes and invalid UTF-8 produce distinct rejection results. Metadata counts
LF and CRLF without normalizing bytes, treating bare CR as content. Empty files contain zero source
lines, and a final newline does not create another line. `from_declared` verifies acquisition claims
against both the content hash and derived newline metadata.

Harness checkpoint diffs consume these complete sources. Binary sources
retain a binary-difference entry, while unsupported encodings receive an explicit unavailable entry.
Source validation does not authorize writes or canonicalize checkpoint bytes.

The crate currently establishes the source boundary. Histogram comparison, raw action hunks, lazy
display rows, shared caches, worker admission, and syntax analysis remain unimplemented. Harness
continues to use its existing comparison algorithm after shared source validation.

## Exact raw changes

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
own memory accounting independently of source acceptance.

## Shared Harness checkpoint comparisons

`ForgeRuntime` passes its `DiffEngine` into `BrokerRuntime`, which shares that owner with every
session controller. Checkpoint previews submit exact raw source pairs at foreground priority.
Checkpoint and provider-attributed previews reuse cached analysis for equal source identities.
They no longer run a separate `similar::TextDiff` comparison.

Interaction finalization awaits shared comparison through checkpoint population and turn setup.
Captured checkpoint records are saved before the analysis await, preserving durable checkpoint
identity across cancellation or comparison failure. Expected binary and encoding
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
the complete source pair, verifies stored digests, validates encoding, and computes source identity
and newline metadata outside the async executor. Cancellation checks separate those stages.
The default four read slots bound concurrent jobs, not the size of an individual source pair.
Input-byte statistics describe retained work without rejecting large jobs. Busy and closed
admission propagate through the broker error path.

Preview serialization still executes synchronously in the broker future. Persisted previews
still accumulate into strings. Unified source/cache byte accounting, output retention and
transport delivery, status/review routing, and syntax scheduling remain incomplete. This integration
does not complete the Harness migration gate.

## Checkpoint capture admission

Every broker capture path runs `GitCheckpoint::capture` on the host's shared blocking read pool.
Initial acquisition belongs to the exchange runtime and runs concurrently with provider reasoning.
Final capture awaits that baseline before comparing normal interactions and child-agent turns. Git queries,
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

Initial interaction startup publishes prompt acceptance and the timeline before checkpoint completion.
The broker persists the baseline before releasing the exchange's tool barrier. Codex uses a temporary
PreToolUse command that invokes `forge checkpoint-wait`, while Copilot uses its session pre-tool hook.
Harness control tools await the same barrier. Providers without a pre-tool hook wait before the model
request starts, after prompt acceptance. A resumed Codex thread with existing background terminals
also waits before inference because terminal stdin bypasses its pre-tool hook. Missing barrier files, acquisition failures, and a 25-second
barrier deadline deny tool execution. Cancellation keeps a failed barrier closed. Finalization also
collects acquisition when the model finishes without calling a tool.
If initial acquisition fails, finalization can retry after the obstruction clears. The closed
barrier prevents provider edits, so recovery records the current workspace as both boundaries
without replaying the provider request.

Each broker retains one checkpoint cache across exchanges. A bounded native Git porcelain status
checks HEAD, index identity, and tracked or untracked changes. Metadata and file identities detect
repeated edits to already-dirty files. A matching observation reuses the previous durable record
without tree enumeration, file copying, or manifest reconstruction. Cache misses perform and verify
the complete capture. Ignored file edits do not invalidate reuse. Repository coordination and native
command cancellation apply to both paths. Capture diagnostics report reuse or candidate and override
counts, cache hits, file bytes read, and elapsed milliseconds.

## Complete checkpoint command output

`forge_git::command::read_command` owns one direct child and two scoped output readers. The caller
supplies job admission, a deadline, and cancellation checks.
Readers drain both pipes concurrently, preserving binary bytes and nonzero exit status for caller
classification. Each reader uses an 8 KiB scratch buffer and grows retained output as needed,
preserving the complete stream. Allocation and I/O failures remain explicit errors.

Cancellation, timeout, and reader errors request direct-child termination. The
owner waits for the child and joins both readers before returning. Callback unwind uses the same
child cleanup ownership. The deadline is a termination deadline rather than a guaranteed return
time. Operating-system waits and inherited pipes held by descendants can outlive it. Process-tree
containment remains incomplete.

Checkpoint discovery reads Git metadata through gix rather than shell file lists.
`file_command` sends large immutable blob output directly to an owned temporary file,
retains complete stderr, and applies a 30-second deadline. It shares process-group
or Windows Job Object ownership and cancellation cleanup with the command runner.
Only explicit diagnostic-command calls retain bounded output tails. Those diagnostic budgets
do not restrict acquired file content or ordinary command streams.

## Capture and rollback share repository scopes

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

## Rust status metadata enumeration

`forge_git::reader::StatusReader::status` uses the worker-local gix repository through the shared
read pool. The `Gix` backend combines HEAD-to-index and index-to-worktree events, requests every
untracked path, evaluates configured submodule state, and enables staged rename tracking. Scoped
reads disable staged rename tracking. It never writes gix's optional index-refresh suggestions.

The reader retains the complete status map, restores byte-wise path ordering, and
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

## Repository observation collection and adoption

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
is distinct from `Missing`. Statistics reuse metadata already obtained during complete content
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

Index stamps read at most the final 32 bytes of the index and each
`sharedindex.<hash>` file, retaining Git's existing checksum and the file length.
They do not hash or scan index contents. If Git omitted its checksum, the stamp uses
modification time instead. A present checksum ignores timestamp-only shared-index
refreshes. Every matching shared-index file participates, without a directory-level count ceiling.

Worktree stamps use file kind, size, modification time, available creation time, readonly state,
and symlink target text. Unix builds also record device, inode, and change time. Symlink contents
are not opened. These are metadata observations. Changes that preserve sampled metadata, changes
outside the observed path set after enumeration, and changes after the final checks require later
source verification. Exact byte identities are acquired through the content API. Mutation
preconditions remain separate work.

Collection checks cancellation, generation invalidation, and a 30-second cooperative deadline.
Full collection runs two native status enumerations and two index-stamp passes. It derives
line counts from Forge raw hunks using the same canonical source pairs used for body rendering.
Binary, filtered, and unsupported sources retain unknown counts. Source size alone does not
disable statistics. The count total can differ from Git's default numstat when the two diff
algorithms choose different repeated-line hunks.

## Complete repository content acquisition

`RepositoryState::content` admits object, index-stage, worktree, or supplied-source requests on the
shared read pool. Request accounting includes retained path capacity and supplied source capacity.
The read pool admits four jobs without a byte admission ceiling. Admission, cancellation, and
generation invalidation fail the outer read.
Native acquisition returns distinct ready, binary, missing, unavailable, and failed
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
blob. Exact mutation acquisition retains checksum verification. Packed-blob and worktree fixtures
verify complete bytes beyond the former source-size threshold. Native gix delta bases,
decoder scratch buffers, and aggregate result leases still consume memory independently of
the retained source accounting.

Index-stage selection reads the requested stage from the native gix index. It retains exact
path identity, stage, mode, and object hash. It checks the selected stage again
after reading the object. Base, ours, and theirs remain separate sources. Gitlinks report an
unavailable content kind rather than entering text analysis.

Worktree acquisition checks metadata before opening, from the opened file, after streaming, and
at the path after acquisition. It rejects growth and shrinkage without returning a partial source.
Reads use an 8 KiB scratch buffer and reserve the observed file length without a size ceiling.
Leaf opens use Unix `O_NOFOLLOW` or Windows `FILE_FLAG_OPEN_REPARSE_POINT`. Symlink content is its target text, without
opening the target. Directory and special-file requests remain explicit unavailable outcomes.

Content requests carry no byte or line-count limits. Complete newline metadata preserves CRLF and
terminal-newline behavior. An optional expected source identity rejects changed bytes or
representation. Successful empty sources remain distinct from missing content. Complete canonical
conversion, external filters, textconv, atomic write preconditions, and the live feature cutovers remain incomplete. The
Unix symlink fixture is present but was not executed by the Windows validation run.

## Canonical worktree conversion

Worktree content requests select `WorktreeConversion::Raw` or `GitCanonical`. Canonical regular-file
reads use the pinned native gix conversion pipeline after complete raw acquisition and before UTF-8
validation. This ordering permits UTF-16 input without admitting it as a UTF-8 source. Symlink target
text is canonical without applying regular-file filters. Object and index content remain canonical.

Successful conversion records the effective configuration digest, selected conversion-attribute
digest, and index stamp in shared immutable provenance. It does not hash raw worktree bytes
for conversion provenance. The
reader strictly reopens repository configuration, validates worktree/index locations, and compares
fresh configuration, attributes, and index state after conversion. Invalid configuration is an
acquisition failure. These sampled checks do not establish atomic mutation preconditions.

Status opens and refreshes share one conversion session across changed paths. The session
prepares configuration, index, native pipeline, attribute stack, and complete info-attribute
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
Counts and display hunks derive from the same comparison. Binary, conflicted,
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

`RepositoryStore` and the runtime's `DiffEngine` share one synchronized `AnalysisStore`, with cache
retention budgets of 512 entries, 128 MiB of sources, and 64 MiB of results. Explicit analysis
can exceed these budgets and returns a live handle without retaining a cache reference.
Statistics reserve optional retention
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
order marks. Decoder capacity calculations retain overflow checks without imposing a byte ceiling. Other
encoding labels return an explicit unsupported-encoding outcome until their Git parity is proven.
Configured clean/process filters or required filters return an explicit external-filter unavailable
outcome. All native external driver options are cleared before conversion, so no filter process can
start through this path. Textconv remains separate work.

Attribute resolution corrects the pinned gix-worktree 0.56 precedence behavior by applying
`$GIT_COMMON_DIR/info/attributes` after nested worktree rules. The override uses collected macro
definitions and preserves explicit unspecified/unset states. A validated resolved selection drives
both native conversion and final context verification. Info-attribute reads, selected-state digests,
and serialized effective configuration preserve their complete input without byte ceilings.
Native configuration/attribute parsing, decoder scratch, and aggregate results retain their
actual memory costs.

The CRLF history callback reads only an unconflicted index entry, checks blob kind, decodes the
complete object, and verifies advertised length and object checksum. Tests compare
canonical bytes with real Git staging for an existing CRLF index entry. Other fixtures cover native
Git object identity, configuration refresh and invalidation, attribute precedence/macros, explicit
unset/unspecified states, index identity changes, external-filter exclusion, and encoding validation.

Enabling gix's attributes feature retains gix 0.87.1 and adds locked transitive packages
`gix-submodule 0.34.0`, `dashmap 6.2.1`, and `hashbrown 0.14.5`. The stale local registry index needed
an explicit package metadata refresh before the new graph could resolve. Subsequent checks use the
locked offline dependency graph. Live status/review read cutovers and all migration acceptance
gates remain incomplete.

## Revision candidates and synchronous completion cache

`RepositoryState::refresh_revisions` collects local and remote branch arguments through the shared
blocking read pool. The `Gix` backend enumerates references, resolves symbolic remote references,
and preserves Git-compatible short names, including ambiguous local branches.
The result retains raw argument bytes, a sorted candidate list, request revision, repository
generation, shared-storage identity, and a digest of the enumerated full refs and object IDs.

Each enumeration retains the complete candidate collection and encoded reference data without a
count or byte ceiling. Accepted snapshots preserve every argument rather than an admitted prefix.
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
Its synchronous lookup uses binary search and returns every prefix match. A cold or stale lookup
schedules one refresh, and warm lookups schedule none. Callback delivery is accepted once. Failed
requests notify, preserve valid candidates, clear pending state, and delay retry for one second.
Repository invalidation rejects an in-flight result, while a host-generation change drops the list
and rejects old-host responses. Successful empty lists remain distinct from failed requests.

Lua publication validates identity, generation, revision, strict byte ordering, and dense-list shape
before copying the complete list. Accounting includes 4 KiB per snapshot and 64 bytes per value in
addition to string bytes, without rejecting snapshots for their accounted size. The shared-host
page encoder preserves every argument and uses base64 to preserve raw ref bytes.

Peak-memory and cross-platform measurements remain required before the completion acceptance gate
can pass. Snapshot accounting is not a measurement of total Lua VM memory.

## Shared-host revision completion cutover

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
changed source digest, superseded revision, or out-of-range offset. Pagination covers the complete
snapshot without selecting a size-limited prefix. Individual arguments may span page boundaries.

`revisions.lua` captures working-directory context at setup and on command-line/directory events.
The synchronous command callback reads that context and cached strings without repository discovery,
Git execution, or JSON decoding. Cold calls return immediately and schedule one host start/refresh.
The adapter assembles bounded pages asynchronously and publishes only after identity, offsets,
length, count, framing, and cache validation succeed. Ten-second stale refreshes retain the previous
list while running. Failures notify and preserve valid data.

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
