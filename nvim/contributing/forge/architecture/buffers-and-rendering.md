# Buffers and rendering

Rust owns canonical document state and render projections. Lua owns Neovim buffers, windows, unsent editing, and application of admitted updates. Revision, source, and view identities prevent stale work from overwriting a newer presentation.

Public command behavior is defined by the native services and their Lua adapters. Legacy compatibility modules are not an alternative source of ownership authority. Read [Host and protocol](host-and-protocol.md) and [Git and checkpoints](git-and-checkpoints.md) for the service boundaries.

## Shared document ownership

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

## Native views and atomic publication

`forge.status`, `forge.local_diff`, `forge.source_document`, and `forge.walkthrough`
bind native services to editor views. The Harness controller binds a native transcript document and a Lua-owned
composer buffer. Shared document commands resolve configured bindings,
help, selections, and gutters from native metadata. Physical source rows contain source
text, while decorations provide gutters and selection presentation without changing text.

Composer resizing captures the current cursor and viewport immediately before changing
window height. If the complete draft fits after accounting for wrapping and the winbar,
the viewport starts at the first line. Otherwise it retains the current
scroll position. The height query stops after the visible-row budget, without limiting
draft content. This prevents Insert-mode scrolling before window growth from hiding
the first line of a short draft.

Queued prompts and pending steering are physical, read-only rows below status in
the timeline. Lua coalesces changes through the presentation's `staging` operation.
Rust keeps them in a presentation-local block, separate from durable exchange
history. These rows use normal timeline scrolling and never enter composer text
or its height calculation. Refreshing staging preserves intentional transcript browsing.

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

Forge documents keep collapsed descendants out of the Neovim buffer. `forge.nodes`
indexes source ranges and resolves expansion actions. `forge.node_projection` retains
source blocks and explicit expansion choices, then projects only visible rows for Status
and review documents. It reuses unchanged visible blocks and skips complete hidden
ranges. Expansion choices belong to the document, so windows displaying that document
share the same visible rows. Ordinary source buffers retain their own folding settings.
Forge disables native folding in generated views and does not create native fold ranges.

Harness retains its Rust-owned node projection and incremental IPC patches. Lua applies
that projection directly, without projecting or scanning the retained transcript again.
The node's display state and the rows published in the same revision must agree.

Harness Markdown handlers retain only decorations inside the current projected message
ranges and clip spans at their ends before render-markdown caches them. Incremental parser
trees can retain deleted list items at a collapsed boundary. Filtering before caching prevents
cursor refreshes from restoring their bullets on a separator or another transcript entry.

Expanding an unloaded node keeps its heading visible while the existing asynchronous content request
runs. A successful response inserts content. Closing during loading cancels expansion
intent, and a late response cannot reopen the node. Errors clear loading state and remain
visible through the existing notification and recovery paths.

Status retains file bodies independently of visible rows. Opening an unloaded file requests
its body before expanding. The shared projector handles file, hunk, and section expansion,
while the Git service remains responsible for paging and semantic actions. If a generation
change discards a body delivery, the Status renderer requests a complete body snapshot before
accepting a patch whose base is missing or ahead of the retained body revision. Demand responses
and context events can cross in transit, so deliveries already covered by a retained body do not
replace it. Snapshot recovery retains the prior usable view and expansion intent, admits recovery
from a desynchronized body, and reports request failures through the existing notification path.
Review buffers retain canonical source rows and draft comments separately. A collapsed source row is
omitted from presentation without deleting its comment or changing its source anchor.

Status applies the configured `status_open_target` to admitted file-opening effects.
The default `tab` preserves the Status window, while `buffer` reuses it. Historical
source creates its destination tab only after source loading and view validation
succeed. Its close action removes an owned single-window tab while preserving other tabs.

Updates validate source identities, ranges, and revisions before publication. The buffer
writer captures every displayed window's cursor immediately before its synchronous commit.
It restores surviving block identities afterward, or selects the enclosing heading when
collapse removes the selected body. No cursor snapshot crosses asynchronous preparation.
Text, decorations, visible indexes, and cursor restoration publish without yielding.
Each changed span replaces its prior rows directly, without clearing the buffer first.

`forge.cooperative` prepares Harness document updates in slices targeting four milliseconds
or 1024 work units. Preparation retains the committed text, node ranges, and decorations. A completed
update publishes text, metadata, node ranges, and view restoration in one non-yielding callback.
Each text replacement uses one buffer API call without first deleting the live range.
Document actions reject pending updates while cursor movement remains available during preparation.
Each resume checks the document owner, host generation, buffer validity, and changedtick.
Cancellation before publication preserves the prior frame and reports recovery through the
existing document failure path. Once publication starts, cancellation takes effect afterward.
Profiling records slice counts, maximum callback duration, maximum preparation duration, and
maximum non-yielding commit duration in `ui.document.slices`. Large initial snapshots can exceed
the preparation time budget during their atomic commit, so commit timing is tracked separately.

Each synchronous commit captures every attached window's current cursor identity and viewport
after preparation, immediately before changing visible text and node metadata. The same non-yielding
commit restores those anchors against the new document. Movement during preparation is therefore
included in the capture and never suppresses restoration. Cursor snapshots do not cross
asynchronous preparation boundaries.

The replica's `before_commit` callback runs at that same boundary. Timeline switches use it
to save the outgoing view after backend requests and local preparation finish. Its optional
`after_commit` callback runs after viewport restoration in the same atomic patch or snapshot commit.
Harness uses these callbacks to capture bottom visibility and apply per-window tail following
without retaining stale scroll decisions across asynchronous preparation. Declaration
expansion captures its viewport after the response, immediately before publication. Rejected
editable-text changes capture cursors inside the scheduled rollback, not when queuing it.
Action-target snapshots remain separate and only validate whether a response still applies.

## Timeline node projection

The timeline projects ordered exchange items into separate message, tool-group, and change
nodes. Consecutive ordinary tools share a group. Commentary, steering, or a file edit ends that
group. While its owning exchange remains active, the latest tool can show a preview of its last
four display rows. Explicit expansion loads the complete output through the existing paging
path. A settled group defaults to collapsed tools without previews. Exchange completion also
collapses the activity node and leaves its aggregate changes and final response outside it.

Tool output deltas update the existing output cache. File edits have no generic tool-output
cache, so their deltas reconcile the change node instead. Both paths retain the same provider
item identity and saved output. A failed edit therefore exposes its diagnostic through its own
change node without creating a generic tool row.

Codex `fileChange` items carry path, operation kind, move destination, textual diff, and final
status. The backend replaces provisional patch revisions by provider item ID, while the timeline
retains first-seen order. Each successful edit operation renders a `Changed N files` node at its
original position, outside generic tool groups. An edit ends the preceding tool group. Its node
opens directly into files and hunks, without a generic tool heading or empty output preview.
Running edits show `Changing N files`. Failed, cancelled, and interrupted edits retain their
outcome and available diagnostic text but never contribute to the successful change summary.
Commands, formatters, and generators remain ordinary tools. Their filesystem effects require a
checkpoint comparison rather than inference from command output.

## Shared diff integration

`forge-diff::presentation::DiffPresenter` owns file and hunk headings, source-row
styling, gutter widths, and fold boundaries for status, PlanReview, PR and commit
reviews, walkthrough excerpts, and Harness change trees. Consumers supply source
rows, stable block identities, and domain targets. The presenter never performs
repository mutations or resolves plan and comment actions.

Presentation options reuse `ContentLayout` for nesting and marker columns.
Count headings serve status and PlanReview. Source-range headings serve PR and
walkthrough views. Saved timeline headings retain their exact text, including
suffixes, and saved patch rows retain their existing context. The shared patch
adapter normalizes those rows without recomputing the diff. Timeline summaries,
indentation, expansion policies, and immutable declaration navigation remain
owned by Harness.

`DisplayCursor::new` provides the single compact policy for generated diffs.
It retains changed rows, context between grouped edits, and enclosing structural
boundaries. It omits ordinary unchanged context outside each group. Plan visibility
filtering removes empty hunks and their omission markers without discarding saved
annotation anchors.

Local comment projection reanchors layout through `forge.buffer` and the shared
`content_layout` installer after source rows move. Hidden rows receive no gutters.
The adapter replaces owned layout marks rather than relying on their gravity
through ambiguous edits involving identical omission rows.

## Refresh cadence and decoration updates

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
Installing decoration metadata invalidates cached screen rows for the affected buffer without
flushing an intermediate frame. The next redraw applies viewport highlights in every attached
window, including when a patch changes only highlight metadata and the cursor stays still.
PlanReview's annotation repaint and viewport decoration share `decorations.project_row` to map
source spans onto physical rows. Added and removed diff backgrounds retain end-of-line fill,
including blank rows. Syntax and intraline spans retain their byte ranges. Annotation insertion
and folding must preserve this distinction without padding source text to the window width.
Structural edits rebuild native folds and restore each window's fold preferences. With
Harness logging enabled, `ui.buffer.patch` records validation, view capture, fold capture,
sequence updates, buffer text writes, metadata installation, editable-region attachment,
decorations, fold refresh, and view restoration separately in milliseconds.

## Conventions and invariants

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

## Editable-region ordering

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
PlanReview compares source with the complete proposal and uses the shared diff
context rules on declaration rows. Each change retains compact changed rows
and enclosing declaration boundaries. Related scopes merge when their padded
windows are at most six lines apart. Shared opening rows appear once in the first
group and closing rows once in the last. Distant edits retain separate groups.
Explicit reference navigation can reveal declarations outside those regions through
three-line excerpts and enclosing boundaries, without expanding the complete file.
Initial PlanReview delivery contains compact blocks once. Updates carry disjoint
patches and changed source anchors, retaining unchanged text and highlighting in
the Lua fragment. The comment adapter rebuilds its physical projection locally.
Enclosing declarations retain normal syntax highlighting. Omitted ranges render
`...` without declaration or edit targets, while a single omitted line retains its
source text and coordinates. Shared diff delivery preserves these rows across chunks.
Transcript declaration targets retain the plan identity,
submitted revision, path, and side. `plan.declaration` reads that immutable
snapshot, and navigation opens a read-only declaration buffer. Removed rows open
the previous proposal, or the source baseline on the first submission. Navigation
never substitutes the current working-tree file for a saved declaration.

## Physical buffer replica

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

## Bounded display cursors

`DisplayCursor` owns one immutable raw result and one fixed context setting. Context grouping merges
touching windows while retaining contributing raw identities. Status uses the compact cursor mode,
which retains context rows between nearby raw hunks in one action group but omits unchanged rows at
the leading and trailing edges of each group. A cursor tracks source line and byte
positions monotonically, skipping hidden gaps without constructing an all-file display-row array or
another complete source-line index.

Each delivery targets 256 source rows and 128 KiB of generated text, counting one row separator byte
per emitted row. The decoration budget reserves one slot per row for its base diff style. Additional
syntax or intraline spans are not yet emitted. Zero budgets preserve delivery state. A row that fits
the byte target but not the remaining batch budget remains pending for the next call.

The first complete source line always makes progress even when it exceeds the chunk's byte target.
Lines are never split into artificial source rows. Returned text omits
the diff gutter and LF/CRLF terminator. Bare CR content remains intact. Rows carry old/new source
coordinates and the exact raw identity for added or removed content.

Structural rows, full-file preview policy, syntax and intraline spans, document memory accounting,
wire delivery, and live viewport demand integration remain incomplete. The row and text targets here
do not establish a bound on group metadata, shared analysis memory, or serialized transport bytes.

## Full-file preview and speculative eligibility

Display cursor construction takes an immutable analysis and context setting. Added, deleted,
and modified previews retain every source line and use successive
display chunks to deliver the complete content. File size and line count do not disable expansion.

`BodyPolicy::may_prewarm` requires a nondeleted kind, a known changed-line total below 100, known
byte sizes on both sides, and available speculative capacity. It does not
acquire sources or compute missing counts. Policy overrides can lower the thresholds but cannot
raise them above the migration limits. Eligibility does not reserve capacity, so the scheduler must
still perform atomic admission before starting work.

Explicit display requests have no full-file line or single-line byte admission limit.
The pure speculative eligibility function is not yet connected to the live feature scheduler.
Editor delivery and runtime body-kind classification remain part of feature integration.

## Bounded intraline analysis

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

## Display emphasis and decoration admission

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

## Shared asynchronous raw comparison engine

`ForgeRuntime` owns one `DiffEngine` with four active-job slots. Construction opens no repository
and requires no executor. `compare` checks the cache and joins matching in-flight source identities
before reserving another job. New work accounts for source retention and a conservative maximum
hunk capacity before reserving dedicated pool admission. Returned handles retain their accounting
reservations even when cache budgets require an uncached result.

Each waiting caller owns an interest token. Dropping one waiter leaves other consumers active.
One analysis admits at most 64 waiting callers. The next caller receives `ConsumerLimit` without
subscribing or retaining its duplicate source. Releasing one interest permits another caller while
at least one existing consumer remains. Once the count reaches zero, the job retires and matching
requests wait until retirement releases the slot. A retiring job cannot gain new
consumers after its worker has committed to cancellation.

Diff and syntax callers register a capacity notification before attempting admission. Temporary
job-slot pressure suspends both callers without imposing an input-byte admission ceiling.
Completion wakes the caller to recheck cache coalescing and admission. Closing either analysis
owner wakes its pending callers without closing the other owner's shared pool. Syntax deadlines
also bound admission waits. Dropping a waiting future cancels that caller without starting native
work. Disabled workers and closed owners remain explicit errors. Content size and cache-budget
exhaustion do not reject explicit diff analysis. Caller-owned sources waiting for capacity remain
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
The reservation uses a conservative hunk-capacity estimate based on source line counts for
accounting, not source rejection. Temporary algorithm allocations, caller-owned input
acquisition, independent clones, and per-consumer metadata need separate accounting. This is not a
hard process-memory bound or preemptive cancellation of the native histogram algorithm.

Harness checkpoint comparisons use the shared host engine. Status and other feature comparisons
have not yet migrated to that owner.
Syntax scheduling, feature-level priority selection, source deduplication, and accounting for
completed-but-unconsumed responses remain incomplete.

## Dedicated analysis worker ownership

`AnalysisPool` owns lazily started `forge-analysis-*` threads. Pool limits bound worker count, total
reserved/queued/running jobs. Declared input bytes remain accounted without an admission ceiling.
`reserve` obtains admission before a caller
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

## Priority promotion for shared comparisons

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
