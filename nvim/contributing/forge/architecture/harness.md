# Harness sessions, tasks, and permissions

Harness owns durable sessions, exchanges, tasks, goals, child agents, approvals, and their recovery state in Rust. Lua presents that state and captures explicit user input without independently inferring lifecycle transitions.

Provider-specific transport behavior is described in [Providers](providers.md). Plan lifecycle and checking gates are described in [Planning](planning.md).

Provider polling runs in an owned Tokio task. The broker pins the provider lifecycle,
event loop, and event-processing futures separately so their state does not accumulate
inline along a permission-change resume and automatic repair. Compaction processing
has its own handler so ordinary streamed messages do not reserve its temporary storage.
The resumed-repair regression runs with a 1 MiB worker stack to leave room for the
enclosing host request lifecycle on the normal worker stack.

## Timeline storage and incremental projection

Portable settings retain automatic completion-check enablement independently of
queue timing. Enablement defaults on. The completion gate reads the effective
setting before loading check configuration. Live changes use the settings route
without acquiring provider execution ownership. Disabling the gate releases its
deferred queue obligation and leaves explicit runs and plan Checking available.
The provider `check_definition_list` tool and public checks catalog read effective settings
on each request and expose `automatic_checks` and `check_timing`. Existing tool
scopes observe live changes without restarting the provider. Agents use this
policy to choose between the completion gate and explicit verification.

The shared picker owns its single-key modal bindings and disables `mini.clue`
triggers only in its display buffer. Leader-key trigger refreshes cannot replace
Space toggles in `/config`, including automatic-check enablement. Source buffers
retain their normal clue configuration.

The status entry combines the primary `SessionPhase` with an optional `CheckActivity`.
`buffer/status.rs` owns their layout. The primary label and action keep their own
row, and check progress appears directly below it before the reasoning preview.
Check updates preserve an active provider phase or user-decision wait. When the
provider is idle and checks remain active, the primary label is Checking and the
check row names its trigger. Rust owns both timers, while Lua animates only the
committed primary status marker.

Active checks never become historical entries. `TimelineStream::update_checks`
publishes a lightweight status replacement during execution and atomically removes
the matching activity when it inserts the final result. Normal status updates retain
independent check activity. Full snapshot reconstruction applies the same boundary.

Check runs and retained session events require a `TimelineOwner` at creation.
Session ownership names the session explicitly. Exchange ownership names the
session, exchange, and a position in its source node sequence. Provider-requested
checks capture the invoking exchange before execution. Standalone user checks
explicitly belong to the session, while execution gates belong to their execution
exchange. Ownership cannot change when an operation completes or resumes.

Completed checks and visible exchange events enter `ExchangeNode::TimelineEvent`
at their captured position. Check summaries render as exchange results outside
the activity fold, before the outer final response, so completing an exchange
does not hide its verification results. Each check group retains its own collapsed
command details. Other exchange events render inside the activity fold.
The projector and live stream share event insertion,
identity replacement, and standalone ordering. Source updates preserve the same
owned nodes, including nodes in child exchanges. Missing owners, cross-session
owners, and invalid source positions fail explicitly. Deliberately retired
exchange history is excluded separately while its check records remain stored.
Routine settings progress stays hidden, and configuration failures remain visible.
Expanded check groups contain attempted commands and failures, including failures
before process startup. Skipped and pending selections remain in check records
without creating transcript rows or output nodes.

Completed results retain one Checks root. Its children group executions by
Formatting, Lint, Validation, Tests, and Builds, then by a known source file or
package/application target. Exact test names nest below their known source or
target. Categories start collapsed and show their latest passing ratio, maximum attempt
count, and total command duration across all attempts. Explicit category expansion persists
when the Checks root is closed and reopened.
Repeated executions retain independent command identities and chronological
Run rows, including earlier failures. Grouped file and target headings summarize their
leaf outcomes and sum their recorded durations. They omit the total duration if any leaf
has no recorded duration. Run rows show only their number, outcome, and duration.
The root summary counts the latest outcome for each check definition and known file or
test target. It shows passed results over the total and the largest attempt count across
those identities. Earlier failures remain in Run rows but do not count as current failures
after a successful retry. The root duration retains the time spent across all attempts.

The check supervisor reads an optional generic JSON report at the explicit path
provided to callbacks. Repository adapters translate runner-specific formats into
that report. Harness does not detect command names or parse runner stdout.
Malformed reports fail explicitly, while complete raw logs remain available.
The transcript consumes reported file outcomes, durations, commands, and diagnostics
without parsing logs during rendering.

Timeline ownership has no event-count or history-length limit. Recursive agent
rendering grows stack segments instead of rejecting a fixed nesting depth, and
persisted JSON uses the shared stack-growing codec. Output paging controls the
materialized view without discarding the underlying content.
Agent selection and refresh locate nested runs through an explicit work list,
including runs attached to exchange entries, without a separate lookup-depth limit.

Automatic repair reopens the completed owning exchange through a dedicated check
transition and preserves its prompt, usage, and original checkpoint. Ordinary
continuation still rejects completed exchanges.

`CheckSupervisor` owns run admission, cancellation, records, and worker lifetimes.
Admission and the corresponding record share one state lock, as do terminal
publication and release of active ownership. Observer snapshots read both together.
The active run also owns the service's source-work permit, which is released when
its processes are collected rather than when the UI drains its event stream.
Dropping a caller's wait does not abandon the owned worker. Process cancellation
uses an independent task that owns the process tree, so blocked output writing,
record persistence, or UI delivery cannot delay the termination request.

`CheckPublication` coalesces intermediate snapshots per run in a separate persistence
worker and flushes the terminal snapshot before the run result returns. Notification
delivery coalesces view invalidations separately from persistence. Complete process
output stays in its stdout, stderr, and combined files. Persistence failures become
explicit execution errors in the live record, with the previous disk record
available for recovery. The UI does not own execution or infer completion.

Transcript reads retain the committed view while retrying transient transport
and admission failures. State reads that predate displayed revisions coalesce
another read and retain waiting callbacks. Queue publication owns a separate
diagnostic that clears only after its matching successful retry. Diagnostic
severity belongs to the owning operation, not to its message text. Section
loading failures remain attached to their section after reconciling the current
node generation and page boundary.

Staging writes carry a monotonic sequence within the document lifetime. Retrying
the same sequence cannot replace newer staging. Task controls retain their
operation identity while polling an uncertain acknowledgement. Waiting for
execution ownership includes checkpoint finalization and has no ten-second
failure boundary. Explicit shutdown remains a neutral lifecycle notice.


`NodeMap` owns the shared expansion choices, node incarnations, and tool output sources for
one open Harness presentation. Emitted nodes identify exchanges, messages, thoughts, tool groups,
tools, changes, files, and hunks. Each node separates its source lifecycle, automatic display,
explicit expansion override, and loaded extent. `NodeState` travels inside block metadata,
so the same atomic publication changes the text, node state, and visible source ranges.
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
an active group receives the automatic four-row preview. `Running` groups expose only that latest
call, including after explicit expansion. When a group becomes `Ran`, it collapses automatically,
clears its live expansion override, and exposes every call when reopened, even while the exchange
continues. Settled groups default to headings,
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
The visible row projection follows the published node display without a second
Harness expansion preference. After a body splice repairs fold endpoints, subsequent
heading updates preserve the current loaded ranges at application time. They cannot restore
fold metadata captured before the splice.

Prompts and final responses longer than three display rows support message folds. Prompts
start folded when submitted. The two latest preceding responses stay expanded throughout the current
exchange. Starting another exchange folds the now third-to-last response, even after an explicit
expansion. Exchanges without a final response do not displace the retained response. Older explicit choices
remain stable. Folded messages show their first three display rows with a right-pointing
caret, while expanded messages retain their complete Markdown, links, and text. The canonical
block keeps the full message and the preview boundary. Resize recomputes that boundary without
changing the expansion choice. Assistant commentary retains its separate presentation.
Reasoning messages and summaries remain in durable turn history but do not render as
timeline commentary, including in active child previews and reopened history.

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

Harness Markdown resolves visible message owners through the weighted sequence. Collapsed
descendants have no buffer rows and never enter Markdown parsing. The parser includes complete visible messages across all attached windows, retaining
delimiter context without scanning off-screen history. The presentation caches message versions,
window identities, and the buffer changedtick. Text publications restore the intended parser
boundaries even when no Markdown owner changed, because Tree-sitter can extend a message's
included region into rows appended at its end. Metadata-only and timer updates retain the
cached presentation. Parsing uses a completion callback guarded by the render lifetime.
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

### Harness timeline ownership

The transcript presentation owns a shared gutter selection controller. `W` enters linewise
Visual mode and highlights complete display rows, including the left status column,
timeline markers, indentation, and inline diff gutters. `<Space>l` copies those rows
with their displayed markers and gutters without changing buffer text. Copying resolves
expansion markers from the same layout owner as rendering and removes concealed source
markers so they do not appear twice.
Selection exit, buffer leave, rejected startup, and presentation close release selection
ownership and restore the prior clipboard mapping. Ordinary timeline cursor movement retains
its existing behavior. Harness keymap configuration and help expose this command only in the
transcript, while the composer retains its normal `W` behavior.

The composer shares session commands with the transcript in Normal mode, including
question and approval reopening, model selection, artifacts, help, and closing.
Only timeline row activation and gutter selection are excluded from its command
set. Prompt navigation and activity toggling focus the transcript before acting.
Insert-mode typing and prompt-history navigation retain their own bindings.

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
The exchange activity section owns the shared child indentation for all event kinds,
including skill reads and file-change failures or interruptions. Late skill and tool
events from the response's own turn remain inside its activity fold. Later turns
retain their chronological position and nest their activity between outer responses.
Copilot assistant deltas render as commentary until the completed message supplies its tool
requests. Messages that request tools retain commentary styling. Completed messages without
tool requests become final responses. Completion replaces the streamed message by provider
identity, preserving its position without appending duplicate text.
Tool timings round to the nearest tenth in a fixed five-cell column, accommodating `99.9s`.
Rounded values reaching 100 advance to the next unit, starting with minutes, so timer
updates never shift command columns.
Tool group headings sum the observed runtimes of their calls, including overlapping calls.
An unavailable call duration suppresses the total. Each output view retains its group identity
so timer updates can refresh the group heading without scanning or replacing output bodies.
Provider silence does not replace the native activity or waiting status. Approval waits use
informational styling and remain pending when their picker closes. Dismissing an approval keeps
it closed across snapshots until the user reopens it or that request resolves. Approval lifecycle
events invalidate any snapshot already in flight before it can overwrite newer pending state.
Controller renders and
presentation callbacks share status-hint selection so updates cannot alternate between a
wait notice and the underlying activity. Connection and execution failures retain precedence.

Transcript updates preserve each attached window's cursor and viewport through the shared
document commit, including when thought text beneath Working grows or wraps. Bottom visibility,
elapsed time, and composer focus never authorize automatic cursor movement. Explicit prompt
submission and agent actions can position the transcript at its tail once. Subsequent updates
preserve that position. Snapshot recovery retains reading positions, and timeline switches
restore the selected timeline's saved view.


`client.lua` owns one persistent Forge JSONL process. Repository requests do not require Harness
initialization. Harness requests and streamed events carry a durable session id, so the process routes
independent turns without treating one session as globally active.
`session.lua` mirrors that boundary through `harness_by_id`: each live session owns a transcript buffer, composer
buffer, windows, queue, busy state, and subscriptions inside one real Neovim tab.

Lua owns the complete unsent Harness draft. Opening a transcript and typing in HarnessInput never
transmit draft text or local edit revisions to Rust. Explicit file-path pastes import the selected
file through `attachment.import`, independently of prompt submission. Ctrl-s captures the complete draft,
then sends one `prompt.submit` request with separate text and typed attachment fields and a monotonically increasing session token
paired with the captured transcript document. Rust tracks submission admission without retaining a
composer document. A `prompt_submission` acceptance clears only the unchanged captured draft.
Retraction restores its exact rows only while the cleared buffer retains its changedtick.
Newer typing survives both transitions. Rejection retains the draft, and closing or replacing
the presentation invalidates late draft edits. `views/harness/prompt_submission.lua` owns the
request in session state independently of that presentation. Every transport completion releases
its matching request and reaches the controller, including errors and cancellation after view
closure. Duplicate completion cannot release a newer request. The controller clears busy state
and refreshes authoritative task state before draining queued follow-ups. Plan preparation precedes acceptance. Git checkpoint
acquisition starts asynchronously when the exchange is admitted and does not delay acceptance or
the initial timeline projection.

Queued prompts retain their entry, attachments, task identity, and configuration until
the matching `prompt_submission` acceptance. Queue acceptance uses the session submission
token independently of any composer document. Admission failures leave the entry in place,
retraction restores the original entry once, and failures after acceptance do not replay
already-admitted work. An in-flight queue entry cannot be edited or submitted again.
Queued compaction retains its entry until the task transition succeeds.

`views/harness/attachment.lua` scopes terminal and register-paste handling to composer
buffers. It retains the pasted path during asynchronous capture and tracks that exact
range with extmarks. Edits before or after the range move its anchors. A replaced or
deleted range invalidates the response instead of overwriting newer text. Imports run
one at a time per composer through the shared native read pool, without owning the
active provider broker. Pending imports block submission and queued-edit navigation.

`attachment.rs` captures complete files in the existing content-addressed object store
and publishes a named copy and metadata under the data root's `attachments` directory.
The import returns a typed object with its retained identity, filename, media type,
and visible label. The composer keeps these objects separately from its plain text and
anchors their label spans with extmarks. Text never contains a hidden attachment URL.
Native text selections and register metadata preserve the objects when copied or moved.
Queue drafts and prompt history retain the text and attachment list together.
Register metadata is captured by `TextYankPost`, without replacing the user's normal-mode
delete, change, insert, or paste mappings. Register pastes are observed after keymap resolution,
so user-defined paste direction and bindings remain native editor operations.
Typing a label or an attachment-shaped string creates ordinary text.

The composer binds only submission, queueing, steering, cancellation, mode, effort, and prompt
history controls. It inherits ordinary text editing and navigation mappings. Timeline navigation,
folding, help, and close shortcuts belong to the transcript. Up starts prompt history at the
first draft line and otherwise moves within the draft. Down advances through recalled prompts
and restores the unsent draft with its attachments. Alt-Up and Alt-Down remain aliases.
Snapshot refreshes and history-record responses preserve the selected occurrence and unsent
draft while adopting new history entries. Each composer owns its history-edit listener.
The shared history store retains all submitted entries across sessions and host restarts.

Command admission and queue classification share the same leading-token parser in
`completion/command_source.lua`. Unknown and incomplete slash commands retain the
draft and notify. A filename extension on the leading token or the complete first
line identifies a path prompt instead. Exact supported commands take priority over
this extension check. Submission, queueing, and queued editing share that classification.

Request admission resolves only the explicit attachment list. `BackendInput` carries
the resolved objects through provider requests and user steering. Provider adapters
do not scan text for paths or references. Generated Background prompts and child-result
text have no attachments unless a caller explicitly supplies an attachment object.

Rust projects the semantic timeline and its synthetic bottom status from the same session-scoped durable owners.
`ExchangeLayout` keeps provider activity in the summary fold and renders generated artifacts,
plan submission events, verification results, and checkpoint changes before the outer final response.
It partitions references to durable nodes without cloning response or diff bodies. Review acceptance
and requests for revision belong to the newly admitted implementation or revision exchange and render
before its lifecycle heading, prompt, and metrics. Rejection without revision remains a terminal event.
Question clarifications retain their branch ownership across skill reads and provider compaction.
Those events do not end a clarification. The next user input or question lifecycle boundary does.
Incremental message rendering preserves the nested reply's Markdown role, matching reopened history.
Legacy activity following an intermediate final
answer retains its relative order. Streaming, completion, and reload use the same layout and node IDs.
Thoughts and tool groups nest one two-column level below their exchange summary. Tool calls and output
retain their additional nesting. Adjacent visible tools share one group across provider-turn boundaries.
Hidden reasoning and empty messages do not split that group. Visible commentary, file changes,
questions, and other timeline content retain their boundaries. Each grouped tool keeps its original
turn identity for output lookup, actions, and expansion. Final answers remain outside the activity fold. Tool invocation headings retain one display row in both collapsed and expanded states. The shared width-bounded title formatting preserves the duration column and truncates the displayed command or arguments. Full provider titles remain in durable tool state. Active tool previews display the latest four wrapped rows from the incremental output index. New output replaces only the preview body and earlier-line count at the existing presentation cadence. Explicit expansion still reads the full output from its beginning, and settled exchanges remove the automatic preview.
`PlanStateMachine` validates question, feedback, submission, review, acceptance, cancellation, and failure
transitions. `PlanQuestionLedger` records answers, skips, and withdrawals before generation resumes. It matches both
the provider's logical id and a canonical digest of the user-visible content, so a resolved decision cannot become
pending again when a provider retries with a different id. The transient `PlanElicitation` therefore contains only
unresolved questions and never acts as the durable decision owner.

The shared broker input boundary allocates the interaction future on the heap. Task transitions, planning
continuations, and direct prompts therefore avoid embedding the full provider interaction state in every outer
async frame.

The broker owns plan generation as one bounded continuation loop across every backend. Each provider turn first
applies plan creation and edits to the canonical document, then the broker accepts only three forward outcomes: a
submitted plan, a genuinely new unresolved question, or another bounded generation turn. `ContinuationBudget`
centralizes the total-turn and consecutive-no-progress mechanics shared by goals and plan generation.
`PlanGeneration` adds canonical document revision evidence, permits 20 total turns, and stops after two consecutive
turns without canonical document progress. `/plan retry` resets only that budget while retaining the plan document
and question ledger. Provider adapters stream one turn and never decide whether planning should continue.
An automatic planning retry admits a new exchange with an empty user prompt and the persisted
`Planning continued` lifecycle label. The preceding exchange completes before the retry starts,
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
and analyzes the complete source without a file-size or line-count cutoff. Syntax results have no separate byte or span-count
rejection. Content loading has no total byte-size admission limit. Pages and transport parts stay
bounded, and their consumers request subsequent batches. Tool display and export retain every
source row. An output page admits its first complete row even when that row exceeds the page's
byte target.
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
needed by attached windows. An unloaded fold keeps its existing collapsed heading without a loading
label until its first page is installed. The same publication opens it, and a second toggle cancels that pending open.
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

The 60 Hz publication target uses a 16.67 ms interval rounded up to a 17 ms timer for demand
responses and streaming changes. Each
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

### Shared Harness picker

`views/picker/` owns every compact Harness decision surface. Pure state and layout modules own
single or multiple selection, pages, responsive label/detail rows, and reserved input geometry.
The renderer applies one frame with extmarks, while the window owner anchors a focusable float to the
bottom of the union of the Harness transcript and composer windows.

Question pages preserve separate Answer and Description columns. The shared layout's
`wrap_column` selects a column for word wrapping, and `flexible_column` gives that column
the available width after the other columns. Each option retains one selection identity
across its rendered lines. The viewport keeps the selected option fully visible when it
fits, retains the column headings and controls, and permits Page Up/Page Down within an
option taller than the available body. Resizing reflows the rows without changing the
selected answer or attached input.

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

Structured settings rows use `field_navigation` with `option.fields`. Every row and
field has a stable ID. `picker/state.lua` retains field focus per row across selection,
search, and asynchronous spec replacement. `picker/field.lua` renders choice, toggle,
input, and action fields. Tab and Shift-Tab move focus, arrows change choices or numeric
values, Space toggles booleans, and Enter edits inputs or opens actions. The view receives
`on_field_action(context, action, direction)` and owns persistence. Disabled fields retain
focus and show their reason, but the picker rejects their actions.

A field can set `join_previous` to share the preceding field's column while retaining
its own focus, as timeout units do beside their amounts. Single-toggle rows can set
`inline_fields` to render their icon and label together in the first column. The layout wraps every
structured field without dropping value text, carries its
highlight spans through wrapping, and pins the focused field's explanation below the
scrolling rows. Section headings remain outside option selection. A `fit_content` view
may grow past the default picker height within its host bounds and optional height ratio.
Other pickers retain their configured maximum height.

`/config` groups all controls into Models, Compaction, Checks, Planning, Execute, Permissions,
Tools, Display, Provider, and Debug. Main and Background model controls retain separate
mutation owners. Planning owns the plan approval and revision Auto-approve controls.
Execute exposes the Ask/Deny/Allow approval default without a Defaults row.
The check-editing toggle uses the independent `checks.editing` control.
Related values share rows, and the selected field replaces the permanent description
column. Tools contains Timeouts and separate single-toggle rows for Subagents and LSP, Display owns Thoughts, and the final
Debug category owns Logging. Background preference publication preserves focus and never changes chat settings.

Boolean row enablement uses a leading `●` for enabled and `○` for disabled, toggled
with Space. Keep that state out of editable text columns. Tab selects the next
editable field, such as Scope or Auto, and the left/right arrows change its value.
The checks picker follows this pattern alongside the MCP picker's Space toggle.

Configuration pickers never depend on the serialized provider-turn request lane to become visible.
Harness resolves and caches backend model metadata when the view activates, then `/model` refreshes
the catalog when it opens so persisted model and effort selections remain current during provider work.
Model, effort, service-tier, backend, and
access and approval selections continue through their normal mutation owners. Configuration changes
that affect execution always use `task.transition`, including when Lua currently reports idle.
The host determines whether execution needs collection, so a delayed busy snapshot cannot route
a thinking or model change through the settled-session guard. Presentation-only status thoughts
and input timeouts retain their independent live-update paths. Automatic check timing
also updates independently and applies at the next queue completion decision. Configuration changes
interrupt active work immediately, collect the current execution, apply and verify the requested
revision, and resume the same exchange when its task remains active. Durable timeline events report
interruption, application, verification, and resume. Provider changes require idle work and an
explicit destination chat. Failed provider startup restores the source backend and session.

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

`/mcp` renders each provider definition with transport, reported token attribution, lifecycle state,
and cached tools. Resource toggles identify a canonical shared source and persist a revision before
task transition admission. The client waits for the applied revision instead of treating admission as
successful application. Unknown native resources require explicit import before mutation. Resource
configuration does not write native provider settings. ACP exposes configured selection separately
from server readiness because the protocol does not provide a server-status inventory.

Copilot terminal status reads the session binding without waiting for initialization to release
its lock. A busy binding returns a starting snapshot, which the footer renders as
`Terminal status: starting`. Subsequent polls observe the ready session and its terminal inventory.
Status polling never starts or resumes a provider session and never waits for the first inventory
refresh. A pending first refresh returns a starting snapshot. An unattached restored conversation
returns an empty snapshot, since a persisted provider identity does not mean startup is running.
An initialization delay therefore does not consume the terminal query's eight-second deadline.

The `/undo` specialization opens the active session's settled current interactions newest first.
Selecting a record offers conversation-only rollback or conversation with file restoration when
a checkpoint exists. Conversation-only rollback leaves files and Git's index untouched and works
without checkpoints. Its confirmed `task.transition` action `undo` validates the selected history,
collects execution through the normal cancellation and child-quiescence path, and leaves the task
paused before applying the conversation rollback. The broker rechecks the history after collection.
File restoration retains its preview and explicit confirmation. Only an
unfinished file restore interrupts entry into the history list with recovery choices. A successful broker rollback
restores the selected interaction's exact multiline prompt to `HarnessInput`, moves the cursor to
its end, and enters Insert mode so the reverted request can be edited or resubmitted.
The timeline omits rolled-back and superseded exchanges instead of showing a rollback entry.
Durable records retain their disposition and execution outcome for checkpoint history and recovery.
The same visibility rule applies when the session reopens and to delegated exchange history.
Both scopes replace the provider conversation and atomically persist the cleared provider identity
with the reverted history. The next prompt supplies the retained current exchanges as context to
the new provider conversation, excluding the rolled-back and superseded messages.

The `/sessions` specialization defaults to the current repository and toggles all repositories with
`C-o`. Moving the result selection requests a read-only `session.preview` projection and temporarily
swaps only the Harness transcript window to a scratch timeline rendered by the regular Harness tree.
The active transcript buffer keeps receiving background renders off-screen and returns unchanged
when the picker closes. `C-j` deletes the selected inactive session, while Enter resumes a compatible
same-repository session. Preview requests never acquire or alter a durable session lease.

## Check setup admission

Check setup uses ordinary provider execution admission, not the installation/authentication cleanup
lock. Its model turn therefore remains steerable and interruptible while preserving exclusive execution
and finalization. Ctrl-C submits a pause intent with output-free prompt retraction enabled,
regardless of composer content. It never submits or steers the draft, commits a queued edit,
or releases queued follow-ups. The composer and pending input retain their existing ownership.
When a child timeline is selected, Ctrl-C interrupts that child without sending the composer.
Explicit pause actions retain their existing pause-only behavior.
The presentation state retains `queue_paused` independently of host task records, so
taskless chat cancellation cannot release queued input when the pause acknowledgement settles.
A replacement prompt waiting for check cancellation moves into the queue before its callback
can deliver it. An explicit new prompt or execution task action releases the local pause.

## Exchange and restart ownership

The broker runs once per Neovim process over JSONL stdio. An `Agent` owns persistent provider
identity, an `Exchange` owns one admitted request through resolution, and each actual provider
invocation creates a `Turn` inside that exchange. `ExchangeNode` preserves the order of turn
content, delegations, acknowledged clarification or steering, plan resolution, and artifact changes.
Session creation commits the primary Agent together with the session. A fork assigns a new primary
Agent identity. Delegation admission similarly commits the parent reference and queued child Exchange
together, before provider start evidence activates the child. A Delegation always projects its named
child Exchange, so later reuse of the Agent cannot change historical status.

Provider execution runs as one task owned by the broker's `JoinSet` on the existing
Tokio executor. The broker observes its result alongside checkpoint, control, and event
lanes. Every exit aborts and joins any remaining provider task before transport cleanup,
so cancellation cannot detach work. This task boundary keeps provider polling outside
the host dispatch stack. Task transitions select a separate operation future before
polling, so unrelated actions do not enlarge the running action's state machine.
Development and release use the executor's default worker stack settings.

Mode restart interrupts provider Turns and pauses the existing Exchange. Resumption preserves its
request, plan and goal associations, and initial checkpoint. If baseline acquisition remains pending,
the broker collects its existing worker and durably stores the baseline before parking the exchange
and releasing its runtime. A failed capture retains a retryable finalizing exchange and propagates
without releasing the tool barrier. Resumption
reuses the stored baseline rather than capturing the changed workspace as a new starting point.
Cancellation retains provider delivery
while cleanup awaits terminal evidence. Copilot fences submission before attempting an abort.
A turn still preparing its session has no provider execution to collect, so cleanup returns
immediately and the broker drops preparation. A submitted turn retains its event reader until
native idle evidence arrives, with an eight-second bound on abort and collection.
A cleanup failure remains retryable, with the Exchange finalizing and new request admission blocked.
The terminal checkpoint and Exchange outcome
commit together. Rollback changes history disposition while preserving execution outcomes.
Cancelled finalization displays Stopping across status, exchange, and agent headings. Saving
exchange describes finalization for other outcomes.
Checkpoint capture errors include the source path and retain the finalizing exchange. Ctrl-C
retries finalization even when no provider request is active. New prompt admission and goal
resume also retry finalization before starting provider work. A failed retry retains the
previous exchange and its error without admitting a new exchange.
Explicit pause and clear also finalize an exchange waiting for user input. Closed exchanges
retain their recorded questions in history but cannot expose them as active questions.
Unanswered question rows in a cancelled exchange display Cancelled and cannot reopen a prompt.
Internal pauses for configuration changes preserve waiting questions until work resumes.
Restarting an interrupted clarification rebuilds its pending-question context from the saved owner
and admitted input, then appends the configuration notice. It retains the input identity, question
target, exchange, and checkpoint without submitting the input twice. Planning clarifications remain
clarification turns rather than resuming plan generation. An idle question wait starts no provider
work when permission changes without a resume intent.
Execution admission retains one permit per session from request preparation through provider
and exchange finalization. The host transfers that permit into service dispatch, which releases
it and the composer submission ownership before publishing the completion response. Response
backpressure cannot make a finished request reject the next prompt. A second execution cannot
reset an active request's cancellation state. Cleanup owns
a separate control permit that blocks new execution admission until cleanup returns. Status
reads and approval responses remain available. Ctrl-C during a mode restart suppresses its
automatic resumption and waits for the existing cleanup acknowledgement and execution result.
It then cancels the settled exchange and pauses its goal durably, preventing a reconnect from
resuming work that the user stopped. Repeated idle cancellation leaves terminal history intact.
Queued prompt admission waits inside its request task, leaving the host input lane available
for cancellation and status requests. It holds neither execution nor cleanup ownership while
waiting for the other. Admission rechecks the captured task and cancellation revision before
starting work, so a pause, cancellation, or task switch cannot silently resume queued input.
The shared provider instructions treat `.gitignore` as ordinary model-owned project configuration.
When a change introduces generated files, the model inspects existing ignore rules and proposes
needed additions or updates during planning. It edits the actual file during implementation before
generating those files, preserving comments, negations, unrelated rules, source, and required
lockfiles. Harness neither creates the file automatically nor requires an empty file when no rules
are needed. Resumption rechecks current contents before edits. These workflow instructions remain
outside product requirements. Incompatible workspace changes use plan revision.

## Cancellation and mode restart

`turn.cancel` bypasses the broker's serialized request queue so Ctrl-c can interrupt an active
provider turn instead of waiting behind it. Cleanup retains the provider reader until native
execution is collected, then releases the prompt and either retracts or cancels the interaction.
Submitting a replacement prompt during checks uses the same ownership order. The editor sends
`checks.cancel` with `interrupt_execution`, and the service acknowledges it only after native
cleanup. Direct prompt admission and check cancellation during a provider turn enforce the
same boundary. Cancelling a manual run without an active provider affects only that run.
A pause cancels any active check before waiting for its execution permit, including
manual checks without a provider cancellation token. If a replacement prompt arrives while
that pause is pending, Lua retains it until check cleanup and the task operation both settle.
A newly submitted `prompt.submit` remains retractable only while the provider has produced no assistant
message, tool activity, task update, or workspace delta. Context usage and reasoning status alone
do not consume that eligibility. Codex maps an eligible retraction to app-server
`thread/rollback`. Copilot uses the pinned SDK's typed `session.history.truncate` API,
targeting the first actual root `user.message` event from that submission, not the send-response ID,
a subagent event, or a later reminder. This rewinds conversation history without executing
`/rewind` as model input. A pre-submit native tail cursor bounds history verification when abort
precedes the user-event notification. Successful empty history after that cursor proves a queued
message never entered conversation history. Expired cursors and failed reads remain explicit errors.
The broker deletes the provisional interaction and restores pre-submit plan or
goal control state, and Lua returns the exact prompt to HarnessInput without overwriting newer typing.
If cancellation arrives while the initial checkpoint barrier still denies tools, the broker
can prove the turn never gained workspace mutation access without comparing workspace snapshots.
An otherwise output-free native rewind then cancels pending baseline acquisition and removes the
provisional exchange immediately. It does not wait for the repository scan or capture a final
checkpoint. Once the barrier releases tools, normal checkpoint comparison remains required.
Native abort may complete the provider reader successfully before the coordinator publishes
its cancellation request. The broker resolves a pending cleanup intent to cancellation before
collecting the initial checkpoint, rather than interpreting the native success as ordinary
completion. If cancellation arrives during that collection, a still-closed tool barrier also
allows cancellation to interrupt the wait. This admission proof remains separate from conversation
retraction eligibility. If an assistant has already spoken or a tool has appeared while waiting at
the barrier, cancellation preserves that partial conversation but still cancels baseline acquisition.
The exchange settles without workspace snapshots because its tools never received mutation access.
It does not invent a rollback snapshot or restore files. Once tool admission succeeds, cancellation
retains the usual before/after checkpoint requirements.
Each broker invocation clears stale native rewind identity before preparation. The broker's
submission-cancellation flag also fences turns that have not activated their provider reader yet.
Copilot serializes both initial and steering sends against stop admission, so an in-flight native
send must settle before abort, while a canceled preparation cannot send afterward.
Native rewind failures preserve the exchange and notify instead of pretending history was removed.
Once visible activity or a file
change occurs, the broker finalizes partial timeline state and persists the interaction as
cancelled. This split prevents the composer from presenting text that still exists in provider
history while preserving the quick-regret workflow for a genuinely output-free turn.

Policy changes use the durable `task.transition` lane. Shift-Tab submits a
write-toggle intent, then the coordinator interrupts and collects active provider
work before applying the new access policy. The winbar retains confirmed access
and approval while that operation is pending. A resumed exchange retains its
checkpoint and completed tool results. Codex resumes its app-server thread, and
Copilot resumes its retained SDK session.

Plan acceptance, planning clarification, goal continuation, compaction, and child
execution retain their existing ownership boundaries. A newer policy intent can
supersede a pending operation without applying its toggle twice. A validation or
collection failure retains the last confirmed policy and reports the failure.
Resumption failures preserve the policy that already committed.
Interrupted exchanges without an active provider turn project a paused footer even while their
retained exchange remains resumable. Host exit broadcasts a local stop event before failing pending
requests. The controller clears transient busy, cancellation, and restart ownership, stops timers,
and overlays a reconnect error on the retained transcript. Partial output and queued prompts remain
available. Passive refreshes and failure callbacks cannot start a replacement host or resume work.
Reopening Harness explicitly reconnects the presentation.

## Steering and task tracking

Queued editing retains entry identity, task ownership, and configuration while the
composer displays the draft. `queue_editor.lua` owns the reservation and drafts retained
when navigating backward. Drain may consume earlier entries but stops at the reserved
entry, including before queued model configuration. Saving validates every retained draft
before changing the queue, then commits edits without reordering entries, removes cleared entries,
and releases the reservation. A validation failure focuses the invalid draft while the earliest
unfinished edit remains reserved. While editing, submit,
queue, and steering bindings save the reserved message instead of sending it out of order.
The timeline marks the active reservation with `[editing]`. Same-session snapshots and
composer recreation retain the edit. Changing sessions clears it with the queue.
The native staging projection limits each queued preview to the first logical line
and one display row while retaining the complete draft in staging state. Steering
entries retain their full text. Explicit queue navigation resizes the composer
before positioning the transcript tail, so the status and queue use the resulting
pane height. Passive staging refreshes preserve the reader's position.

`turn.steer` uses the same out-of-band broker lane without creating another interaction. Explicit steering
preserves queued follow-ups and delegates active-turn admission to the backend even while a task
transition is pending. It clears HarnessInput only after admitting the text into a pending steering record, then the backend
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

## Timeline and session envelopes

The Rust `TimelineProjector` combines interactions, plan lifecycle records, and accepted-plan
executions into one ordered presentation. The Lua controller applies native blocks instead of
replaying provider events or deriving workflow state. Rust retains node identities and expansion
choices and publishes only their visible rows. The retained Lua `interaction_tree.lua` renderer
supports fixture validation rather than live Harness presentation. Native node projection avoids
overlapping editor fold ranges, which cannot reliably represent wrapped thought and command headings.
When a planning-feedback lifecycle answer exactly matches its durable continuation prompt, the
projection keeps the interaction as the single visible owner and omits the redundant lifecycle row.
The native transcript renderer wraps prompts into real rows before row ownership, highlights,
and folds are assigned. Prompt continuations add no text padding because the concealed first-row
marker appears in the sign column, which supplies the same margin on every row. Explicit source
indentation remains intact. Nested structural indentation applies uniformly to the complete block.
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
`buffer.lua` applies validated node projections incrementally and preserves semantic cursor
identity, viewport position, and unchanged prefix extmarks. The synchronous commit captures
only windows displaying the affected buffer. Returning to Harness attaches the document's
current projection without recreating editor folds. Timer updates retain existing node choices.
Active thoughts never expose expansion keys. Completed nodes stay immutable, so an expanded
command or diff never changes while the user reads it.
Prompt submission does not create a Lua-owned interaction. Broker admission emits the first
revisioned patch with the durable interaction and a Rust `Working` status, so the first visible
segment already comes from canonical state. Later node and completion patches replace that stable
interaction id without a presentation-side merge clock or optimistic shadow record. Lua animates
the published status metadata but never creates, extends, or completes an interaction.

Each stored session uses one versioned envelope. Session loading and listing accept only the exact
current format, leaving older rows invisible without migration or partial decoding. Runtime code
therefore reads only the current ordered interaction model.

## Durable storage

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
source lease. The child retains a fork-time snapshot of the source's visible history before a durable
typed `Forked` event containing the source session ID and display name. SQLite stores these inherited
entries separately from executable exchanges, plans, goals, and agents. The child therefore displays
previous prompts, responses, tools, and nested history without inheriting tasks or pending questions.
Inherited clocks remain frozen, and question, plan, and agent controls are inactive. Tool output,
diffs, file links, and source-session navigation remain available. Reloads and session previews read
the same immutable snapshot. Later source changes do not alter it, and subsequent forks preserve
each inherited entry once. The child, its primary agent, snapshot, and lineage event commit together
before provider preparation, so failed preparation still retains the copied history.
Explicit `/fork <name>` uses that child name. Bare `/fork` derives
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

## Context usage and compaction

`AutoCompaction` is a portable per-session policy with an enabled flag and a positive token
threshold. Ordinary sessions default to disabled. Background sessions default to enabled at
128,000 tokens. The policy does not change the provider's context window and never measures
billing totals or estimates bytes as tokens. It runs after completed provider work, serializes
with subsequent input, and records its last attempted turn to prevent repeated idle retries.
Provider-initiated compaction remains authoritative. Unsupported compaction and invalid known
capacity produce explicit diagnostics. Missing usage remains unverified until the provider
reports it. Full exchanges and timeline records survive model-context compaction. On restart, an operation
that never confirmed completion becomes unconfirmed rather than retaining a running indicator.


Provider token-usage notifications normalize into durable `ContextUsage` session state. The
Harness winbar renders the remaining percentage and total window at its right edge without
adding timeline entries for usage changes. `Compaction` defines provider context replacement as a
separate lifecycle with an operation identity, conversation identity, observed start and completion,
terminal outcome, and optional `CompactionResult`. `CompactionCapability` describes lifecycle,
background execution, summary, and metrics availability independently of `native_compact`, which
gates manual requests.
Codex normalizes `contextCompaction` item start and completion without inventing summary text or
token counts. Copilot normalizes `session.compaction_start` and `session.compaction_complete`,
including supplied summary text, token and message counts, checkpoint metadata, and compaction
model usage, duration, context composition, request identities, and optional billing evidence.
The generated summary is distinct from the provider's complete retained context.

Each automatic operation owns one `ExchangeNode::Compaction` at its first observed position.
Completion replaces that node's state without moving it or creating assistant text or a tool call.
Terminal replays cannot reopen the record. Later notifications can fill missing result fields
without replacing confirmed evidence. A foreground provider turn ending before compaction
confirmation records an unconfirmed outcome rather than leaving an active indicator indefinitely.
While compaction runs, the shared renderer shows `Compacting · Ns` with the existing status spinner
and suppresses its history row. The shared activity timer refreshes elapsed whole seconds from
the recorded compaction start. A terminal outcome restores the ordinary workflow status and
projects a compaction event. Events with a summary or reported metrics have a collapsed detail fold.
Events without details use a plain diamond marker and no fold. Event labels omit manual or automatic origin.
The broker captures context-meter usage at compaction start, preferring the provider's complete
context composition over the last meter reading. It accepts the after reading only after the
provider confirms completion. Usage received while compaction runs still updates the live
meter, but cannot become the post-compaction value. Completed headings show the before/after token counts
only when both readings exist. These readings survive lifecycle replays and session reloads,
remain separate from provider conversation-token metrics, and stop changing once settled.
Codex discards resume-time usage before requesting compaction so stale counts cannot become
the after reading. Usage notifications retain their provider thread address.
Absent provider fields remain absent
without a placeholder summary. Compaction status and results remain in their timeline
entries. Exchange headers omit compaction counts.

Copilot's session subscription remains alive after prompt completion through `Backend::compaction_stream`.
That subscription owns normalization across turns and SDK session rebinding. It delivers active
notifications through the turn's existing event sink and also retains the session delivery lane.
Foreground prompt readers skip these raw notifications to prevent independent operation identities.
The session controller serializes those notifications through the broker and updates the original
exchange record when one exists. Notifications with no exchange owner use a durable session event.
The controller emits document revisions through the normal atomic presentation boundary. Provider
shutdown ends the subscription, and controller disposal cancels its consumer task.

Codex uses app-server `thread/compact/start` for `/compact`. Copilot uses the installed SDK's
experimental `session.history.compact` RPC. Manual requests persist one `SessionEventKind::Compaction`
before provider admission, retain the result or failure afterward, and never create a user exchange.
Compaction updates used tokens while preserving the session's established context limit. The RPC's
raw context-window report remains in the compaction details, and normal usage events own limit changes.
`BackendCompactionRequest` carries the manual row identity into the provider adapter. Copilot's
monitor correlates the native lifecycle with that identity and enriches the same row when native
notifications provide more detail than the RPC result. Unsupported backends omit the command instead of receiving a synthetic
summarization prompt.

Codex publishes every tool lifecycle update to the owning turn. Tool records merge only by
provider address and call ID, never by tool name or arguments. Repeating a control call with
the same arguments creates a separate lifetime. Replayed updates retain the original start
and completion timestamps. Accepted control effects deduplicate by provider call identity.
MCP and dynamic calls include deterministically ordered, credential-redacted arguments in
their titles. Dynamic call completion retains every text output block and treats a false
success flag as failure even when the provider lifecycle status is completed.
Copilot tool calls use the same argument formatter for their native start events. Completion
and progress events without arguments retain the start title, so collapsed rows and expanded
call bodies preserve the invocation through the complete lifecycle.
Expanded tool bodies include the complete call with wrapped arguments independently of
the single-line heading's width limit.
Thread start and resume request automatic reasoning summaries in native configuration,
and every explicit turn start requests the same summary mode. Readable summary deltas
retain item and section identities. Completed reasoning items replace those sections
with authoritative snapshots, including sections delivered without deltas. Summary part
boundaries create no content, and raw reasoning remains separate from readable summaries.
Catalog and background-terminal connections observe shared app-server broadcasts without
answering provider requests. Only the connection owning the active turn responds to controls
and approvals. An observer must not reject a broadcast, because its response can win the race
against the owning connection. Phase completion without an execution control context fails.

## Usage and duration accounting

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
`Planning 120s │ 20/21 tools · 12 s │ 820.0k -> 7.0k · 65 tps`.
The outer duration includes tools and delegated waits within active execution and freezes
across user-input and permission waits. The first pending permission freezes its owning
exchange's execution clock. The last decision resumes it only if execution remains eligible.
Closing the permission window leaves the clock frozen. Explicit pauses, finalization, and
host restarts discard the live coordinator's resume intent. The tool summary counts tool
time in that same active clock, taking the union of overlapping calls and capping outstanding
intervals at interruption or completion.
Tool time uses milliseconds below one second and rounded tenths of a second thereafter,
omitting a zero fractional digit. The outer duration remains whole seconds. The pass numerator
counts successful completed calls only. Running, failed, interrupted, and cancelled calls remain
in the denominator without counting as passed.
Headers append nonzero distinct changed-file totals after usage. The successful/total
tool ratio accounts for unsuccessful calls without a separate failure count.
Changed-file counts use successful provider file changes, normalize paths, and
deduplicate repeated edits. Failed and unfinished edits do not increase the file total.

Each distinct admitted usage report refreshes cumulative input, cache percentage, reasoning,
non-reasoning output, and request count immediately. Turns that have not reported usage do not
erase previously reported totals. Missing categories within reported usage remain unavailable.
Input includes cached tokens and cache percentage divides cumulative cached input by cumulative
input. The compact header shows input and inclusive generated tokens, which include reasoning
and tool arguments. Cache, reasoning, and request counts remain recorded but are omitted from
the header. Unavailable categories remain hidden, with partial token counts labeled in or out.

The same report captures cumulative generated tokens and active elapsed time minus the union
of tool, approval, and delegated waits. Their quotient supplies approximate effective throughput,
rounded to whole tokens per second. Both operands stay fixed until the next report, so ongoing
requests cannot lower the displayed rate before reporting their tokens. Provider and transport
overhead remain in this estimate. Throughput appears only when generated tokens and a valid,
positive duration are available. Elapsed and active tool time both freeze during permission waits. Tool time appears
after a tool is recorded and only while its timing is complete.
Both renderers omit unavailable token categories, unknown cache percentages, and zero request or
tool counts. Reported zero token counts and zero cache-hit percentages remain visible. Separators
join populated sections only, including tool failures and spawned agents when present.
The current private session format hides incompatible earlier sessions without migration.

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

## Permissions, sandboxing, and request deadlines

Initial interactive sessions and `session.new` use full reads, workspace writes,
and the configured approval default, which is Ask. Resumption retains both stored
selections. Background generation disables writes explicitly. Provider forks copy
the source session's access and approval.

The Rust `PermissionStore` loads one validated Rulesync-shaped document from
`stdpath("config")/forge/permissions.json`, compiles command and resource matchers once, and
gives every surfaced provider request to one `PermissionCoordinator`. `allow` proceeds immediately,
`deny` rejects immediately, and `ask` blocks the provider response on the Harness approval float.
Reported writes are rejected while writing is off, including requests without a
path. Enabling writes requires an explicit `/write` or `/write full` command.
Child requests retain the root's filesystem policy and a no-less-restrictive
approval selection.
Persistent approval choices atomically replace exact or broad JSON rules with allow or deny. The
permission document remains outside provider write authority, including shell commands that name it.
The command permission category displays as `shell` while retaining the Rulesync `bash` storage key.
The matcher splits recognized compound shell requests into distinct command targets and omits targets
already allowed by policy. Each remaining target owns one picker page with its own exact and broad
choices. Failed command analysis never opens the picker, executes a command, or records
a permission rule. Malformed transport and shell syntax return `shell_command_parse_error`.
Valid constructs outside the analyzer's supported subset return `shell_command_unsupported`
with the construct name. Interrupted parsing, exhausted analysis budgets, and source-mapping
failures return `shell_command_analysis_failed` with the underlying reason. Partial command
results never authorize execution or supply persistent approval choices.
Each command approval item retains its complete script payload, shell identity, syntax captures,
and byte ranges for the executable occurrences covered by its choices. Quoted copies of a command
do not contribute approval ranges. Nested launcher decoding maps those ranges back through quoting
and escapes into the script payload. Approval details, timeline headings, folds, check labels, and
background-terminal labels omit the shell executable and invocation flags. The popup omits the
redundant command-action subtitle.
The display formatter indents blocks while preserving a source-to-display byte map. The shared
picker maps syntax captures and approval underlines through formatting, UTF-8 wrapping, and its
viewport. The underline adds no foreground color, so shell syntax remains visible. Long details
initially reveal the current approval target. None of these display transformations changes the
executed command or stored permission pattern.
PageUp and PageDown scroll long details independently of the decision list. The visible line range
indicates remaining content. Exact choices refer to the highlighted command instead of repeating it.
Shell analysis uses pinned PowerShell, bash, zsh, and Nushell syntax trees without executing source.
PowerShell `Where-Object` analysis binds script-block and property parameter sets, including
the `where` and `?` aliases and the module-qualified cmdlet name. Property-only filters,
`-Not`, implicit equality, case-sensitive comparisons, explicit `I` comparison aliases,
and `-Is`/`-IsNot` type predicates retain their native parameter roles. Property and value
arguments can be positional or named. `-InputObject`, common parameters and their aliases,
unambiguous parameter abbreviations, inline parameter values, and the `--` end-of-parameters
marker are recognized. Named arguments bind before positional arguments, so reordering them
does not change which operand is the property or comparison value.
Filter scripts must be literal script blocks. Dynamic script references and mixed parameter
sets fail analysis without opening approval. Argument expressions remain visible to the normal
command traversal, including expressions in properties, values, common parameters, and explicit
input. Type literals are permitted only as operands of `-Is`/`-IsNot` within the filter. The
structural filter retains explicit deny rules, and every nested executable keeps its own
permission decision. The parameter contract follows the
[PowerShell cmdlet reference](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/where-object)
and [common parameter reference](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_commonparameters).
Static `[Math]::Min(...)` and `[System.Math]::Min(...)` calls with exactly two arguments are
expressions rather than executable permission targets. Both arguments remain subject to
normal command traversal. Other .NET methods, dynamic receivers, and member-invoking
`ForEach-Object` parameter sets remain unsupported. Recognition uses syntax nodes without
evaluating expressions, and preserves the original source and executable coordinates.
Codex command approval text encodes an argument vector with POSIX `shlex` quoting on every platform.
The provider boundary decodes that transport with `shlex`, retaining structured argument vectors
when available. A recognized launcher supplies its exact script argument and shell identity to
analysis. Direct executable arguments are quoted as POSIX literals for analysis without execution.
Malformed transport returns the same parse error as malformed shell syntax. Display summaries and
suggested policy prefixes never supply executable source. This boundary applies identically on
Windows, Linux, and macOS, including Windows paths and Unix shell executable paths.
The transport format follows Codex's `shlex_join` in
[`bespoke_event_handling.rs`](https://github.com/openai/codex/blob/main/codex-rs/app-server/src/bespoke_event_handling.rs),
using the matching [`shlex` codec](https://docs.rs/shlex/2.0.1/shlex/) rather than host-shell escaping.
Provider shell metadata and explicit shell launchers select the grammar. Unqualified commands use
PowerShell on Windows and the configured login shell on macOS, with zsh as the platform default.
Recognized callback builtins such as PowerShell `ForEach-Object` and `Where-Object`, and Nushell
`each`, `par-each`, `where`, `filter`, `reduce`, `any`, and `all`, require every nested command to pass
policy. An explicit denial of a callback builtin still rejects the request. Control-flow bodies,
conditions, substitutions, and static nested shell invocations contribute their own command targets.
Dynamic command names, code evaluation, unrecognized callback consumers, definitions, and
redirections return `shell_command_unsupported`. Invalid syntax returns `shell_command_parse_error`.
Both failures reach the provider without creating an approval. The error directs the agent to
rewrite the unsupported construct or split independent operations while keeping
pipelines intact. This validation also applies to YOLO and mandatory provider callbacks.
Nushell script-file calls retain their full executable, launcher flags, filename, and script
arguments as one permission target. The analyzer recognizes `--no-config-file` (`-n`),
`--no-history`, `--no-std-lib`, `--stdin`, and `--login` (`-l`) before the filename.
After the filename, `-c` and `--commands` remain script arguments rather than inline code.
Inline `nu -c` and `nu --commands` calls still contribute their nested command targets.
Unknown launcher flags remain unsupported, and pipeline commands retain independent decisions.
Unsupported string forms such as ANSI-C quoting, PowerShell here-strings, and Nushell raw strings
return errors. Escaped newlines in Zsh also return errors because the pinned grammar can turn the
continued word into another executable. These limitations never produce partial approval targets.
The PowerShell grammar is a pinned local Cargo patch under `vendor/tree-sitter-powershell`.
It parses command argument arrays, numeric option names, attached option values, adjacent
parenthesized expressions, and escaped argument punctuation. Array operands retain nested
execution nodes. The matcher accepts only complete syntax trees and never suppresses missing
or error nodes. Original command bytes and ranges remain authoritative for permission rules.
Range trimming preserves escaped trailing whitespace, including literal carriage returns,
instead of turning a valid argument into an incomplete escape.
The patch, upstream revision, generator version, and asset hashes are recorded with the vendored
grammar. The regression corpus exercises quoting, comments, arrays, pipelines, control flow,
closures, substitutions, nested shells, and each discovered executable's independent policy
decision across all four shells. Analysis retains complete command text and stops at 8,192 visited nodes, 64 tree
levels, 16 nested shell invocations, or the parser's 25 ms progress deadline and returns the same error.
The picker stages each decision locally and advances to the next unanswered item. Confirming the
last unanswered item submits all decisions immediately, without a separate review page. Submission
requires one answer for every item and atomically persists the selected rules before releasing the
original provider request. Pending submission blocks duplicate confirmations. A failed submission
keeps the decision pages open for correction or retry.
Any denial rejects the complete provider operation. Reject all discards staged choices. Conflicting
rules or incomplete, duplicate, or unknown answers leave the request pending without changing policy.
In `permission.bash`, `"*"` supplies the lowest-priority decision for a recognized command.
More specific command rules override it. Without a matching rule, commands ask for approval.
Empty or ambiguous command text returns a parse error, even when `"*"` allows other commands.

Approval requests enter the owning exchange's reducer before reaching the controller. The permission
coordinator retains the request's event stream and queues manual resolution there before releasing
the provider callback. Root and child exchanges therefore clear their approval blocker and resume
timing through the same lifecycle path. The controller consumes events keyed by approval ID to remove
the request, close its float, and present the next queued request. Cancellation emits a matching event.
Cancelling a turn drains the coordinator before provider teardown, preventing abandoned requests
from reappearing through a later state snapshot.

`/config` exposes independent approval and answer timeouts, each defaulting to ten minutes.
Each duration retains an amount and a seconds, minutes, or hours unit. Zero disables expiry.
The `session.input_timeouts` route persists global portable settings and updates the shared
session settings without interrupting provider execution. Existing deadlines retain their
original duration. Session and workspace overrides retain their normal precedence.

Rust creates an approval deadline when the permission coordinator registers the request.
The waiting provider future owns the timer, so a hidden or closed picker cannot suspend it.
Expiry removes the whole request atomically, returns a native one-time denial, and records
`resolution_source: timeout` without writing any staged permission rule. The owning provider
receives conservative continuation guidance. Copilot also receives the explanation in its
denial response. Other adapters receive it at their next serialized continuation boundary.
Child continuations retain their original execution owner and cancellation channel.

Question sets retain one deadline across navigation, confirmed answers, and clarification.
The broker publishes input ownership and deadlines through a watched schedule after durable
transitions. One session worker sleeps until the captured monotonic deadline or a schedule,
approval-feedback, or cancellation change. Idle sessions perform no periodic timeout checks
and acquire no execution permit. An expired deadline waits asynchronously for execution and
cleanup admission, then revalidates its schedule revision before resuming through streamed
dispatch. Replaced or cancelled waits release admission without resolving stale questions.
Approval feedback wakes only its owning session and remains pending while a question or plan
acceptance blocks continuation. The worker preserves confirmed
answers, selects each unanswered question's first recommended option or first supplied option,
and skips questions without options. Structured recommendation metadata and the existing
`(Recommended)` label convention normalize to the same choice. Automatic answers retain their
source in history and model feedback. Plan acceptance and execution-access selection never
receive an answer deadline. Recovery restores the remaining interval from the stored expiry
timestamp, then uses monotonic time while the host remains alive.

Both pickers use the shared header countdown. A picker-owned timer updates only its title,
leaving the input buffer, cursor, selection, and scroll position intact. Closing the picker
releases this display timer without changing the Rust deadline. Session shutdown cancels the
controller task before releasing provider and lease ownership.
Harness picker hosts supply the configured interrupt actions to every hosted picker in normal
and insert mode, including MCP, configuration, and questions. Interruption closes the picker
and pauses its owning task without submitting an answer. Question freeform editors retain
this binding. Shared picker actions explicitly opt into freeform input, override conflicting
editor commands, and supply the corresponding footer.

`HarnessSession` owns `AccessPolicy` and `ApprovalMode` independently. Approval
is Ask, Deny, or Allow, defaulting to Ask. Read scope is Workspace or Full. Write
scope is Off, Workspace, or Full. New interactive sessions use Full reads and
Workspace writes. Additional readable/writable directories remain separate lists.

`PermissionChange` resolves `/read full`, `/write`, `/write full`, `/ask`, `/deny`,
`/allow`, and `/yolo` once against authoritative candidate state. The existing task
transition coordinator interrupts and collects active work, validates the candidate,
persists both selections, and resumes only when the prior lifecycle requires it.
`/yolo` selects Full reads, Full writes, Allow, and sandbox off in one operation.
When those values are already selected, it keeps Full reads/writes, changes approval
to Ask, and enables sandboxing only for providers that support it. Admission resolves
this toggle against the latest pending permission selection, so rapid toggles compose
before their superseded operations run.
Lua submits intents and renders confirmed snapshots. Rust publishes the access
filesystem abbreviation. Lua appends `dx` for Deny, `ax` for Allow, and nothing for Ask
to form one gray winbar code. The independent policy values remain unchanged.
The optional red YOLO prefix appears only for full reads/writes, Allow, and sandbox off,
rendering `YOLO · rxwxax`. Enabled sandbox status precedes the model.
Exchange headings and planning/review status text retain their own highlights.

`CompiledPermissionDocument` evaluates saved rules without receiving approval mode.
`DirectoryPolicy` evaluates canonical and supplied paths, retained directory grants,
and filesystem scope. Write-off denies known and unknown-path writes even when a
saved rule or directory grant allows them. Explicit denies and protected targets
remain authoritative. Out-of-scope requests produce Ask unless a specific saved
grant covers them. Wildcard allow rules do not remove the scope check.

`PermissionCoordinator` resolves the final Ask decision with a popup under Ask,
RejectOnce under Deny, or AllowOnce under Allow. Automatic decisions do not persist
rules or directories. Forced ordinary provider callbacks use this same decision
path. Proposal reviews, plan acceptance, and user questions keep their own lifecycle.

`/config` exposes both scopes and directory lists for Copilot, Codex, and ACP, independently
of sandbox support. Configuration saves global defaults through the existing settings store,
which also supports workspace and session overrides. The shared directory editor adds or
edits existing directories using completion, confirms deletion, and marks canonical overlaps.
Full scope retains its list for later use. Disabling the sandbox does not disable either
authorization scope. Only OS-isolation controls depend on native provider support.
The shared provider access validator rejects sandbox-enabled Copilot and ACP sessions before
creating or reusing a provider session. Root configuration and child admission use the same
validator. The user must explicitly disable sandbox before connecting these providers.

Other configuration uses the same interrupt/finalize/resume transition as approval changes. The
broker validates and canonicalizes directory paths before committing settings. Provider
failures remain visible through the existing failed-operation state. Updated session and
preference envelopes hide incompatible earlier formats without deleting their records.
The broker publishes applied settings before resuming provider execution. That event updates
the terminal catalog policy and completes the configuration picker while the task continues.

`CodexSecurity` sends `sandbox` plus per-thread configuration to thread start, resume, and
fork, and sends `sandboxPolicy` to every turn start. All approval selections use
native `untrusted` routing so Harness receives ordinary decisions. Write-off selects
native read-only isolation when sandboxing is enabled. Workspace writes include the workspace and configured
directories. Full scope uses mounted filesystem roots while retaining OS isolation. Windows
backend selection is independent of approvals. Background terminal thread resumes receive
the same settings, so polling cannot restore an older policy.

Native Codex commands and patches run under its filesystem sandbox. Explicit provider
requests are evaluated by `PermissionCoordinator`, which no longer imposes an implicit
workspace ceiling based on approval mode. Copilot routes SDK approval callbacks through
the same coordinator. Provider-private operations without callbacks remain outside that
approval boundary. MCP servers retain their own process permissions.

`PermissionCoordinator` binds a `DirectoryPolicy` to each provider execution through an
execution-owned guard. All three adapters register before starting provider work and release
the registration when that work settles or is cancelled. File authorization resolves relative
paths from the execution workspace, follows existing symlinks before processing later `..`
components, and resolves existing ancestors for new files. Containment compares path
components, not string prefixes. Both the supplied and canonical path participate in explicit
denial checks. Selected-skill read grants retain their existing execution lifetime and cannot
override those denials.
Permission-evaluation failures publish an error event before rejecting the provider callback,
so unresolved paths and malformed commands reach the editor's notification boundary.

The existing permission popup includes “Allow and add to global readable directories” or
the writable equivalent for reported file targets. The server mints the canonical directory
in each choice. Read grants use the file's existing parent or the nearest existing directory
for a new nested path. Write grants discover the containing Git worktree from that directory
and use its root, including linked worktrees and nested repositories. Non-Git writes retain
the nearest existing directory. Discovery failures reach the permission-error boundary
instead of producing a broader grant. The popup displays the exact directory before approval. Directory choices
carry no client-selected path. `SettingsStore::grant_directories` merges accepted choices
into global settings in one immediate SQLite transaction, retaining unrelated preferences
and concurrent grants. Persistence completes before the provider receives approval. A
successful grant refreshes the active session's displayed access lists through the existing
execution-state event, without interrupting the execution waiting for that approval.
Subsequent authorization reads current stored lists, including global grants alongside
workspace/session lists, so grants survive restarts and configured removals take effect.
Exact and broad permission-file choices remain available. Write-directory choices and
external read-directory choices name the displayed canonical directory. Write targets inside
the execution workspace also offer "Always allow writes in this workspace", which persists
an absolute workspace edit rule without enabling filesystem-wide writes or changing approval
mode. When the directory and workspace coincide, this choice replaces the duplicate broad
allow choice. External targets do not offer an unrelated workspace grant. Explicit denials,
protected targets, and write-off remain authoritative.

These scopes authorize reported file operations, not every filesystem access performed by
a provider, shell command, or MCP server. Copilot and ACP do not acquire an OS filesystem
sandbox from these controls. Codex's native sandbox separately constrains writes, but Forge
does not translate Read scope into native read isolation. A file callback without a path
requires a one-request decision, resolved by Ask/Deny/Allow, because directory limits
and protected paths cannot be verified. Unknown writes are rejected while writes are off. It never substitutes the workspace or offers a persistent directory grant.

The global Rulesync config omits the `permissions` feature only for `codexcli`. Codex therefore
cannot apply a generated exec-policy denial before Harness evaluates the request. Direct Codex CLI
sessions retain Codex's native sandbox and approval policy, while the other provider targets keep
their existing generated permission outputs.

## Presentation and session navigation

Only the transcript window owns the Harness winbar. The composer clears its window-local
winbar so the split presents session identity once. The transcript winbar begins directly
with confirmed access and approval, followed by the resolved runtime model. Its right side shows remaining
context followed by the sole command hint, `? help`, using the configured help binding.
Every field separator uses the shared small gray `·` segment from `shared/keymaps`.
Separators retain their own highlight independently of permission, lifecycle, and error text.
The composer displays no key hints. Its bindings cover submission, queue editing,
cancellation, write toggling, reasoning adjustment, prompt history, and shared
Normal-mode session commands. Help and closing work in either pane independently
of filesystem permissions. Timeline row activation and gutter selection remain
transcript-only so the composer retains its native Enter, `.`, and `W` behavior.
`/config` displays the underlying CLI provider.
The winbar prefixes the model and thinking level with a gray outlined lightning glyph
(`󱐌`, U+F140C) for Fast or a filled glyph (`󱐋`, U+F140B) in YOLO's neon red for Ultrafast,
including before the model catalog loads. Standard has no glyph. The icon and gray
model text share a field without an intervening separator. Model, thinking, and speed
changes share one trailing `*` after thinking only when they interrupt existing work
and any selected value still differs from the applied session. Configuration captures
this distinction before the coordinator marks itself busy and retains it across
superseding operations. Idle and paused changes show no marker. The marker clears
once all values agree. Configuration admission validates tier support.
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
Startup suppresses session fields and retained transcript rendering until the first
snapshot applies. The snapshot publishes access and model identity together. Startup
failure or dismissed session selection clears that pending state.
For an unresolved `default`, broker initialization reads the latest resolved default
from current-format sessions in the same workspace and backend before publishing the
snapshot. The storage query selects only the model string. It does not load transcript
history. Explicit model selections remain unchanged. The asynchronous catalog refresh
updates a default session when its default model changes, persists the result, and emits
`session_configured`. An unchanged catalog emits no redundant configuration event.

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

## Child-agent ownership

Harness owns child conversations through `AgentCoordinator`. Each root session owns unique task
names and canonical `/root/<name>` paths, durable `AgentTask` records, informational inboxes,
follow-up queues, and cancellation signals. `ExecutionOwner` binds every child runtime to its
Harness session and agent. Provider connection keys include both identifiers, while portable
settings retain the root session scope. Children use the shared workspace and start with fresh
provider history. Follow-ups resume only the selected child's native conversation.

Admission reserves one of eight active child slots before committing the agent identity, parent
`Delegation`, queued child `Exchange`, and task in one SQLite transaction. An operation receipt
returns the admitted identity on repeated delivery. Rejected admission leaves no orphaned child.
Each child runs at most one task at a time and retains at most 32 pending follow-ups. Informational
messages remain in a durable inbox and never start an idle child. An active adapter can steer the
message into its current turn, otherwise the next follow-up receives the retained message.

The coordinator's bounded request channel has 64 entries. The active root prompt drains it while
retaining the broker lock, and the idle session controller drains it between prompts. Provider
tool callbacks use that channel rather than acquiring the broker lock. Model tool authority is
bound to a live root turn and parent exchange, expires when the turn ends, and is checked again
at admission. Children receive no delegation tools or root planning authority.

The portable `subagent_tools` setting defaults to false. Enabling it exposes exactly
`agent_spawn`, `agent_send`, `agent_followup`, `agent_interrupt`,
`agent_list`, `agent_activity`, and `agent_tool_output` to the root model. Manual spawning remains available
when the setting is disabled. Completion enters a durable queue of unreported child results.
The broker sends each result as an attributed parent message through supported active steering,
or includes it at the next input boundary. Unfinished model delegation can continue the same
exchange after a provider turn ends. Manual completion never starts idle root work. No model-facing
wait tool is exposed. Main yields when only delegated work remains, and Harness resumes it
after the relevant children settle or new user input arrives.

`agent_activity` selects the child's active run before queued follow-ups, or its latest
run when idle. It returns the most recent entries in timeline order, elapsed milliseconds,
last accepted provider activity, and approval/input state. Tool entries contain lifecycle,
failure status, elapsed time, and an opaque call reference. They contain no output. Thought and
commentary entries retain up to 4000 Unicode characters with an explicit truncation flag. Final
answers remain on the existing completion-delivery path. The tool defaults to five entries,
with per-call overrides bounded to 1–20.

`agent_tool_output` resolves a reference containing the session, child, exchange,
turn, and call identities. The broker validates every ownership boundary before reading.
References survive child completion and follow-ups. Output reads use zero-based Unicode
character offsets, default to 8000 characters, and reject limits above 32000. SQLite reads
bounded UTF-8 byte windows without hydrating unrelated output. Each result reports the current
call state and next offset. Running calls return available text without waiting or re-execution.
Metadata-only inspection does not advance the child's last-activity timestamp.

`/agents on` and `/agents off` submit explicit `subagent_tools` values through the same
configuration path as `/config`. Both commands remain local during active work or child
timeline selection and never become child prompts. Command completion offers both forms.
The current setting gates model requests at invocation and admission, including requests queued
before a disable operation. Reenabling restores admission in the same session. Already-admitted
child tasks continue, and explicit user spawning remains available while model delegation is off.

`/agents spawn <profile> <text>` admits a fresh child using the named enabled profile. `/agents spawn`
opens the profile picker and then the task composer. `/agents spawn <profile>` opens that composer's
task entry directly. Completion lists enabled profile names. Repeated spawns allocate distinct
instance names such as `reviewer-1` and `reviewer-2`. `/agents` presents Main, Running, and Done
sections, includes a Profile column and resolved provider/model/thinking/speed, and preserves
selection across live updates. Its existing message, follow-up, interruption, and navigation
actions operate on the instance identity.

`AgentProfileStore` owns `stdpath("config")/forge/subagents.json`, beside permissions.json.
The file is the global source of truth across workspaces and providers. `/agents edit` opens a profile
picker over `agent.profiles.get`, and `/agents edit <profile>` opens the named profile directly.
The edit picker includes disabled profiles.
Every fresh spawn and follow-up checks the current profile enablement before admission, including
follow-ups to existing children. Disabling a profile blocks new tasks without cancelling admitted
work. Reenabling it permits new tasks in the same session. Retained children keep their captured
provider selection, instructions, and conversation history.
`oc` closes the picker and opens the global configuration file in a new tab.
Tab selects Enable, Provider, Model, Thinking, or Speed on the selected profile. Left/Right
cycles that field through a revision-checked save.
Inherited fields display only `inherit`. Editable column headers reserve the same arrow
padding as their values, and the Purpose column shrinks first when space is limited.
Columns fit their longest header or value with compact arrow gutters. Additional window
width does not spread the columns.
The full-page profile form keeps its title in the winbar and starts its body at Name.
Field rows are consecutive, with white bold labels and multiline instruction editing.
Picker value controls share `keymaps.picker.previous_value` (Left and `r`) and
`keymaps.picker.next_value` (Right and `h`). Settings, profile toggles, model values, and plan
revisions declare these commands instead of literal keys. Form controls retain their separate
bindings, and picker aliases apply only where a value action is declared.
The picker updates in place after success, retaining selection. Pending saves block further
mutations, reload, and form entry. Failed saves retain the displayed value and report the error,
including attempts to disable the default profile.
`subagent_selection.lua` owns the tuning choices shared by the picker and full-page editor.
`agent.models` returns `{models, capability}` from one child adapter, preserving provider model
order. Each editor caches successful catalogs by provider until reload or close. A provider change
selects its first advertised model. Provider and model changes retain compatible thinking and
speed, otherwise resetting the field to `default`. Thinking choices follow the selected model,
and speed choices follow the provider's `fast_mode` and `ultrafast_mode` capabilities. Inherit
choices remain available when the current parent value is compatible, with that value displayed
beside the sentinel. Spawn-time validation still applies when parent settings or providers change.
Catalog failures and empty catalogs leave all profile values unchanged. The picker saves the
complete dependent change once and blocks other mutations until it finishes. The shared form
accepts an asynchronous field patch only while the form, field, generation, and captured draft
still match. Save, reload, close, and newer edits invalidate late patches.
Selection opens a dedicated full-page form through `shared/form.lua`, which reuses `editable.lua`
for protected native text regions, `picker/field.lua` for choice values, and the shared command
specification for save, reload, help, and close. Text and choice edits remain local until one
`agent.profiles.save` submits the complete form. Confirmed saves clear only the submitted values,
so edits made while a save is running stay dirty. Failed saves retain all drafts. Reload explicitly
discards local edits, and closing a dirty form leaves it open. `/config` keeps only the tool toggle.
Saves validate the complete document,
check its content revision under a writer lock, then atomically replace the file. External edits
reload on access. Invalid JSON retains the host's last valid catalog and surfaces a diagnostic.
A host with no valid catalog rejects spawning until the file is repaired.

Profiles contain a purpose, child instructions, enabled state, provider, model, reasoning effort,
service tier, and permission mode. The document selects an enabled default profile. Provider
accepts `inherit` or an explicit provider. Model, thinking, and speed distinguish `inherit` from
`default`: inheritance copies the root's selection at admission, while default selects the target
provider's default. Explicit and cross-provider model/effort combinations are checked against the
provider catalog. Unsupported combinations fail before child identity or capacity is committed.
Profiles do not infer a substitute model when an inherited model is unavailable on another provider.

The spawn tool accepts only `profile` and `text`, with the configured default used when profile is
omitted. Its advertised schema includes enabled profiles and their purposes. Provider adapters
refresh changed tool schemas at the next root turn, retaining the provider conversation. Each
child persists its resolved profile name, tuning, instructions, and permission ceiling. Editing
or deleting a profile cannot retune an existing child. Follow-ups reuse this snapshot and the
child's own history. Effective permissions are the stricter of the snapshot and current root
mode, and the current root OS access policy remains mandatory. Profiles inherit provider tools
subject to the existing child exclusion of delegation and root workflow tools.

Child events use the shared `Exchange`, `Turn`, tool-output storage, and incremental document
pipeline. The broker publishes registered `agent_updated` events for live picker summaries
and timeline patches for transcript changes. Queued follow-ups cannot replace the active
exchange's state or control target in the picker.
Main nests each delegated child fold inside its owning exchange's Thought fold, including
references received after the parent final answer. A running child shows its latest non-final
thought with the existing `○` renderer and its latest tool heading with the existing tool renderer.
Tool output stays in the child timeline. Completion replaces the preview with the delegated
exchange's complete timeline and collapses the child fold. Later explicit expansion persists.
The fold heading includes the child's name, provider, model, speed suffix, lifecycle, elapsed
time, and metrics. Timer patches preserve fold metadata. Enter on that first line opens the
owned child timeline while running or completed, independently of inline fold controls.
Successful spawn calls use the agent entry as their presentation, while failed calls retain
their tool error rows. Parent final answers remain outside the parent Thought fold.

Task completion commits the exchange outcome and agent state atomically. Interruption
cancels active and queued tasks and retains history and inbox content. Cleanup failure retains the
child's capacity slot and adapter for an explicit retry. Runtime cleanup has a 30-second bound.
Root cancellation, clearing, and access-policy transitions collect child execution before proceeding.
Admission remains closed until collection finishes and the transition acquires exclusive root
execution ownership. The service then releases its collection guard before dispatching the transition,
so resumed provider work can admit children and finish its parent-result drain. Exclusive execution
ownership prevents another root from starting during policy application. Keeping the collection guard
through resumed generation would deadlock that drain after the provider becomes idle. A later explicit
follow-up retains its provider and model while inheriting the root's current access policy.
Tool visibility changes preserve existing child conversations and resume interrupted root work
within the same exchange. Plan phase completion and automatic checks reject active child execution,
including retained cleanup failures, before changing verification state or finalizing the goal.
Lua adopts the tool toggle after a successful configuration response or its matching active
configuration event, before refreshing the picker. Rejected and unrelated operations leave the
displayed value unchanged.
Completed child tasks ignore replayed completion and late content. The broker delivers each
child report as a compact parent message containing the child path, task ID, status, response,
and error. Supported active steering runs concurrently with provider polling and suppresses
user-prompt rendering for child results. Provider acceptance acknowledges delivery. Rejected
or unsupported delivery remains pending for the next input boundary, including continuation
of an unfinished delegated exchange. Idle roots wait for explicit input.
Harness owns waiting when a provider turn ends with unfinished model-delegated children.
The exchange stays open while provider generation is suspended. After all children owned by
that exchange settle, Harness starts one continuation with their pending reports. New user
input resumes the same exchange immediately and leaves children running, including when the
provider lacks native steering. The root input lane persists across provider-turn boundaries,
admits at most 32 ordered inputs, and retains each input's complete text. Active native steering
retains its existing behavior. Unsupported or rejected delivery waits for the next turn boundary.
Explicit pause, cancellation, and unresolved user decisions prevent automatic continuation.

The root exposes seven management tools without a wait tool. Main retains its existing Working,
Planning, Implementing, and other activity labels with an independent running-agent count.
When Main has no active work, the existing spinner shows `Waiting for agents`. Manual children
can produce that idle status but never schedule a root turn on completion. Child lifecycle and
transcript events continue to own timeline updates independently.

Child approval events retain their execution owner through persistence and transport. The shared
permission popup adds `Agent <name> · <provider>` above the existing request details and choices.
Its interrupt action targets the requesting child independently of the selected timeline.
Requests queue in the existing popup. Resolution removes that child's blocker, while interruption
or provider failure cancels only that owner's pending requests. Sibling requests remain pending.
The broker retains provider conversation identity from addressed events and the adapter's final
identity lookup, even when the child turn fails or is interrupted.

## Session and preference compatibility

Session format 51 hides incompatible session records without deleting them. Preference format 41
remains independent of session compatibility. Portable settings layers, application snapshots,
and replay receipts use their own exact versioned envelope in `settings.db`. Unversioned or
incompatible records remain stored but do not participate in inheritance, replay, or forking.
Current-format decoding failures remain errors. Provider model preferences, imported resources,
and catalog bindings stay independent of that envelope. Reopening retains child identities, completed
outcomes, and provider history, marks unfinished child tasks interrupted, and never replays them
automatically. Root forking does not transfer live child execution ownership.

## Unsupported Harness behavior

- Add task-level diff annotations and feedback cycles now that stable task identity and shared
  diff rendering exist in the timeline.
- Audit provider-local hooks and command policies that can still reject a tool under Harness
  Allow, because backend-native policy layers remain authoritative outside the Harness protocol.
- Move the per-Neovim broker to a shared daemon only if cross-editor live control becomes
  valuable enough to justify process discovery and stronger lease coordination.
- Add provider strategies only through the typed `Backend` contract. Do not scrape PTYs.

---

## Bounded active-turn control admission

`SteeringLane` admits 32 ordinary requests and reserves one additional request for a targeted
interrupt. A command retains its permit after dequeue until provider acknowledgement or command
drop. Both operation classes share one FIFO receiver. Ordinary saturation cannot consume the
interrupt permit. Excess requests fail immediately, without creating queue-capacity waiters.
Steering retains complete text and validates target identity. Closing the active receiver
fails queued acknowledgements and releases its generation's admission state. A dequeued command
retains its acknowledgement owner until completion or drop.

The Copilot control router uses a 32-slot bounded channel. It reserves a slot before validating
the invocation or entering the shared `ControlToolExecutor`. Each invocation retains its complete arguments.
Reservation failure therefore precedes any plan or terminal-state mutation. Validation
failures and calls without a routed result release their reservations. Accepted invocations retain
their order in the receiver. These limits count queued and reserved invocations. A dequeued
invocation and the runtime's retained plan state have separate lifetimes.
Synthetic turn ownership is retained only after the exchange admits that turn, so a rejected
admission cannot replace the original failure with an unknown-turn error during cleanup.

No unbounded Tokio channels remain in the root or Harness source. This inventory does not prove
bounded total memory. Provider SDK buffering, accumulated transcript/output collections, decoded
object overhead, and retained runtime documents still require separate resource accounting.

## Harness task ownership and recovery

The broker owns task transitions. Neovim presents task history and submits durable intents.
Command completion keeps subcommands within their typed parent command. Opening `/task new`
retains the conversation and current execution until a replacement task is submitted.
Session listing and resumption compare existing workspace paths after filesystem resolution,
so alternate separators do not hide a session from its worktree.
`TaskRecord` identifies a conversation-scoped Plan, Execute, or Goal workflow, its saved
access and approval selections, phase, lifecycle status, generation, and attempt identity. Plan and goal records
retain their workflow-specific evidence. `current_task_id` selects the task independently of
the conversation permission. The selected task owns the current plan through `TaskRecord.plan_id`.
`HarnessSession` does not store a second plan selection. Pausing a task retains its plan association and hides planning
input until explicit resumption. Clearing a task retains its history and artifacts.

Read requests and independent metadata writes do not settle tasks or clear active waits. The
observer serves state, session, task, plan, and exchange queries without selecting an execution
owner. Lookup failures remain request errors. Only explicit workflow operations reconcile task
outcomes. Review annotations and questions retain their document's plan identity even while
another task is selected. Requesting revisions or accepting a reviewed plan selects its owning
task explicitly after validation. Cancelling a review addresses that plan's task directly.

`/task` lists the current task first and previous tasks by activity. `/task new` offers the
three workflow kinds. `/task resume`, `/task pause`, and `/task clear` operate on the selected
task. `/execute` selects a submitted plan and `/execute last` selects the most recently
submitted plan in this conversation. Selection accepts the exact revision and transitions its
Plan task into Execute. Terminal executions require an explicit fork. Forking a plan uses its
latest saved design and reassesses the current repository without copying execution evidence.

The default approval is Ask and applies to new Goal tasks and accepted plans.
The Plan approval defaults to Keep current. Both preferences live in `/config`.
Explicit resume restores the task's saved access and approval. Workflow changes never
enable writing or expand filesystem scope.
Interrupting retains the selected task. The next ordinary message resumes a paused Plan,
Execute, or Goal task with its saved context and the message as additional instructions.
Control-triggered resumption records a lifecycle title on its exchange instead of a user
prompt. The title identifies the task and any permission or configuration change. Backend
continuation instructions remain separate, including the native goal activation command.
Clearing, switching, or completing the task ends that message association. A resumed plan
awaiting review still requires explicit acceptance before execution.
Changing permission while paused does not restart execution. Model changes during execution
use the same stop-and-resume coordinator. Shared MCP and skill selection changes immediately
stop active work, apply the selected revision, and resume through that coordinator.
Permission-driven resumption supplies the previous and effective approval modes to the model,
preserves the original request, and directs it to inspect interrupted operations before retrying.
The notice does not widen filesystem access or bypass saved approval rules. Task resumptions
keep this notice separate from user-authored text and retain their lifecycle heading. Native
goal resumption delivers the notice to the resumed provider turn without starting another goal.

`task.transition` persists an operation ID on the ordered host input lane before concurrent
dispatch or acknowledgement. A compare-and-set claim permits one execution of each intent.
Pending pause or clear intent takes precedence over stale Running records when settings change.
Settings received before a new task acquires its identity reject visibly without displacing
that task. The coordinator
closes conflicting execution admission, collects the previous provider attempt, finishes
checkpoint finalization, and applies the latest intent. Superseded intents cannot start a
provider attempt. Permission admission resolves each toggle against the preceding pending
permission selection, or the confirmed session when no selection is pending. The latest
operation retains that cumulative selection without publishing it before application, so
back-to-back access and approval commands preserve both changes even if an earlier operation
is superseded. The cumulative selection includes sandbox enablement, so a pending
`/yolo` cannot lose sandbox-off when another permission command supersedes it.
Directory grants remain owned by the confirmed session.
The shared coordinator also carries configuration and compaction operations, which do not
create tasks. The winbar uses the operation's action to reserve task-control progress for
workflow actions. Configuration keeps confirmed policy and the existing workflow visible
without a task-submission label. Configuration failures retain their normal diagnostics.
Ctrl-C submits a pause intent. Backend goal continuation stays inside the
service-owned execution lifetime rather than relying on Lua to submit the next turn.
The execution permit, rather than a contended broker mutex, identifies provider work that needs
cleanup. Admitted execution waits up to ten seconds for transient broker ownership. Settled
mutations retain an execution permit while waiting for that mutex and through response delivery.
Catalog readers and background subscriptions cannot reject admitted work or cause idle goal
controls to interrupt the provider. Cleanup after the steering receiver has settled is a no-op, while steering input still
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
Pause operations recover as cancelled without an unknown-write warning. A stopped runtime
cannot continue execution, and recovery preserves paused tasks and interrupted check results.
The same recovery repairs pause records previously marked outcome-unknown by runtime shutdown.
Exchange recovery retains the last persisted active elapsed coordinate. It excludes the
offline interval and may omit activity after the last saved provider event.
Recovery also settles unfinished provider turns in a finalizing exchange. A pending success
becomes interrupted when those turns lack completion evidence, while checkpoint finalization
remains pending for the normal retry path.
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
anchor at the start of the admitted implementation exchange. That exchange starts with acceptance followed by
`Plan implementation started`. The previous planning exchange retains its submission and final response. Each subsequent
exchange identifies the phase as `Plan implementation`, `Plan verification`, or `Plan resolution`,
including when it starts, continues, pauses, or resumes. An exchange that ends without a phase
result reports that phase as stopped. Only recorded phase results claim phase completion.

Activity headings use `Implementing`, `Verifying`, and `Resolving` while a phase runs,
then `Implemented`, `Verified`, and `Resolved` when its completion is recorded.
`Exchange::activity` distinguishes active processing, interruption, approval, input, compaction,
and delegated waits without treating a stopped accounting clock as a task pause. Session status,
exchange headings, and child headings consume that lifecycle. Terminal exchanges release approval
and phase activity. Background compaction retains its separate session lifecycle after its owning
exchange completes, without changing that historical exchange's outcome. Working and planning retry rows
retain accumulated execution time plus the current optional interval, matching the exchange clock
across waits and resumption. Rust publishes animation and wait hints with the native row. Lua
shows permission hints and a stationary wait marker without replacing the phase from an approval list.
Periodic refresh includes planning retries and live tool clocks, and skips frozen work and idle agents.
Tool group labels report running calls independently from the group's retained expansion choice.
Declaration comparison details require recorded evidence for the current phase. Empty progress
does not claim that comparison is running or that files match.
Checking results remain separate from model activity folds. A blank row separates a noninitial
Checks section from preceding activity. Declaration findings and accepted commands nest beneath its
heading, and collapsing the implementation exchange leaves the Checks section visible.
Harness records each accepted command's
exit status and saves stdout, stderr, and combined output. Fold expansion reads a bounded page from
that saved file, without provider tool IDs or model-written command summaries. Checking also records
workspace-wide declaration conformance before manual verification becomes eligible.
Manual verification exchanges report each accepted action and link completed execution evidence.
The broker validates manual action identities and evidence references before persisting the report.
Declaration comparison captures readable accepted-to-observed patches for each changed symbol.
The Declarations group nests files and symbol diffs using the existing diff projection, folds, and
intraline highlighting. Captured patches never depend on later workspace contents, and their
synthetic declaration line numbers do not navigate to implementation source. The broker persists
the effective outcome, report, and declaration findings before transitioning, so resolution and
restarts cannot replace an earlier verification result.

Health requests use reserved admission and output scheduling independently of the broker
execution lock. The UI sends at most one outstanding health request per conversation. Ten
seconds without a response shows connection uncertainty. Provider inactivity adds no replacement
status or elapsed counter. Bulk output remains bounded,
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
Paused, blocked, failed, and stopping tasks retain queued input without admitting another turn.
An explicit resume permits draining again. `/clear` and `/new` create another session even while
the source is busy, leaving the source independently accessible instead of steering command text.
Goal control admission requires an active goal, matching the goal observer's mutation boundary.
Controls for paused, blocked, stalled, or completed goals reject rather than reporting success
for a state transition that the observer will discard.

## Check-editing permission

`checks.editing` persists `check_editing` in the session settings layer and updates
shared live authority without acquiring provider execution admission. New conversations
default to off. The broker snapshot supplies the permission for the Harness winbar.
The winbar appends `ce` after the access and approval suffix in the same gray field,
such as `rxwxce` or `rxwxaxce`, and omits it when editing is disabled.
`/checks edit on` and `/checks edit off` send this control request without creating
an exchange, interrupting active work, or issuing a model prompt.

Configuration requests use ordinary chat, not a dedicated setup operation.
The global Rulesync `harness-checks` skill owns discovery, configuration schema,
and scoped-helper instructions. `ResourceCatalog` discovers its canonical source
and provider resource profiles bind it like other selected skills. The per-turn
contract references the skill without repeating its body. Automatic planning
prerequisites also reference it and retain the original plan request.
Questions are optional and never authorize publication. Every `check_definition_edit` invocation
reviews the exact configuration and managed helper changes through the shared
approval coordinator in Ask mode, regardless of saved allow rules.
Deny rejects this review and Allow approves it once. Write-off still rejects the writes. Review choices apply only to that submission and do not
create permission rules. Check tools read the
shared permission at invocation and before publication, so revocation also applies to
already-open tool scopes. Configuration still validates revisions, managed helpers,
write approval, and active-agent exclusion. The permission does not enable check runs
or change the automatic completion boundary.

## Harness status presentation and lifecycle vocabulary

`WorkflowActivity` owns active, completed, and lifecycle labels. The broker builds
persisted lifecycle text from that activity and `LifecycleAction`: Started enters
a task or phase, Continued starts another automatic exchange for unfinished work,
and Resumed restarts paused or interrupted work. Each exchange retains its own
metrics and task ownership. Planning continuation does not imply a user revision.
Its turn budget stays internal and exhausted limits surface their actual cause.

The Rust status projection commits text with `BlockMetadata.status`, containing
its block-relative row, animation flag, and optional Working, Question, or Review
hint context. The buffer contract validates that the row exists. Lua reads this
metadata from the committed sequence, never from label text or target suffixes.
Active labels are Working, Planning, Revising plan, Compacting, Implementing, Verifying,
Resolving, Stopping, and Saving exchange. Completed headings retain Planned, Implemented,
Verified, and Resolved. Idle remains the primary label when only independent checks run.
Active status headings show the phase and elapsed time. The portable
`status_thoughts` preference controls whether the latest reasoning preview appears
below independent check progress in italics, aligned with the phase label and
wrapped within the content width. It defaults to false. Reasoning messages never render
as timeline commentary regardless of this preference. Copilot's concise reasoning deltas
and completed snapshots normalize as readable reasoning summaries for this status path.
A presentation-only `session.configure` request persists the
preference and updates the observer and status entry without acquiring the provider
execution owner or interrupting its response. The preview renders at most two
rows of the complete normalized summary without retaining its emphasis markers. Execution comparisons may
add their file-attention summary. Lua appends the interrupt hint to the heading.

`health.notice` resolves local failures before user approval waits and otherwise preserves the
native status. User decisions display a static `◷` sign for review, approval, and
answer waits. Failures and pauses do not animate. The timeline has one marker column. It selects native status signs only on the
committed status row and window-local timeline markers on other rows. The sign
column stays enabled without rendering a second copy of prompt or event markers. Provider/tool waits,
agent waits, planning retries, saving, and ordinary work animate. The status hint
renderer owns one timer per transcript. Ticks update only the spinner extmark.
They do not read history, clear other decorations, mutate buffer text, or capture
or restore a cursor. A committed update, closure, or session replacement stops
the old timer. New status metadata supplies the next animation policy.
