# Host, protocol, and execution ownership

One local Rust host serves Forge capabilities. Typed requests carry document and operation identity across the Lua boundary, and worker lifetimes retain ownership until their results or cancellations settle.

Read [Building and testing](../building-and-testing.md) for executable selection and verification. Protocol version constants and the checked-in contract are authoritative.

## Host executable and service ownership

The `forge` package builds a library and a thin executable. `src/main.rs` parses
arguments and calls the synchronous, non-inlined `forge::run` entry point.
`src/lib.rs` owns the host, router, runtime, and shutdown modules, keeping their
generated async code in the library compilation unit. Both targets use the same
development profile with incremental compilation, limited debug information, and
LTO disabled. Workspace code remains unoptimized. External packages use optimization
level 3 without debug information, and routine tests inherit the development profile.
Release compilation is an explicit build and verification choice.

Executable acquisition depends on the host OS. Windows launches an immutable cached copy
for each build identity so Cargo can replace the build output while an editor runs. macOS and Linux launch
the cached build output directly, retaining the old executable for running processes
when Cargo replaces its path. Unix release leaves that shared file intact. Reusing
the executable avoids the first-execution delay measured for each fresh copy on macOS.

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
Confirmation validates the captured view and source version before `plan.tests.delete`
updates the saved review draft. The broker removes only selected inventory entries,
prunes empty file groups, and rejects stale confirmations. Project files remain unchanged.
Renames and test deletions return an updated projection to the existing native document.
The document retains analysis for unchanged declarations, input views, visibility filters,
and declaration folds. Changed declaration sides receive fresh syntax analysis. Lua applies
changed text ranges in the same buffer without closing the review or displaying a loading page.
Lua retains every semantic rename preview span after confirmation until the authoritative
projection commits. Failed requests remove the preview and restore the original conceal settings.
Confirmed test deletion immediately projects the remaining rows, count summary, and file headings
from the captured inventory. The saved revision and source identities remain unchanged until Rust
accepts the edit. Failure restores those rows through the live comment projection, preserving newer
feedback and the current cursor. Success replaces the temporary projection directly without
reintroducing deleted rows. Source projection actions wait while test deletion is pending.
Test-only edits replace the test inventory, recompute its count summary, remove empty
file headings, and shift later navigation anchors while retaining every declaration block.
Semantic renames preserve canonical formatting and validate only changed declaration and call
data. A content-keyed cache reuses parsed call presentations for unchanged file sides.
The file store retains one validated canonical revision and its rendered source, avoiding
repeated validation and rendering during persistence and review recapture. Every cache hit
requires identical canonical content, and disk reads still enforce the saved revision checks.
Late draft edits reanchor to surviving source blocks, and queued submissions use the new document version.
The plan persists an ordered `manual_changes` history with each rename and each removed test identity.
Acceptance, rejection, revision feedback, and resumed execution expose that history to the agent.
Each manual edit saves `working.json`, `working.md`, and its navigation index while advancing
the document version and review digest. Submitted snapshots, the submitted revision counter,
and revision lifecycle records remain unchanged during editing. Reopening or restarting restores
the draft through its checked working files. Acceptance or request changes freezes one snapshot
of the complete edited draft and records one revision before the decision continues. Opening or
cancelling acceptance options does not finalize the draft. Invalid decisions preserve both the
draft and the existing revision history. Model submissions retain their own revision boundary.
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
boundaries. Syntax analysis processes complete source without a byte or line-count
admission cutoff. Cache eviction controls retained analysis references without
disabling parsing for larger files. Cancellation, deadlines, and parser failures
remain explicit outcomes. Injected languages use the same source and stop repeated
language/range cycles.

## Startup measurement

Startup logging separates repository acquisition, serialization, transport decoding,
semantic formatting, index construction, buffer writes, post-application setup, and the
first displayed redraw. Semantic application records callback count, total work, and the
longest renderer callback. File expansion records demand, body application, and the first
body redraw. Measurements use the manually built release executable in a fresh host.
Existing hosts retain their process-owned executable until shutdown.

## Protocol tracing

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
Plan renames and test deletions carry one `operation_id` through `ui.plan.edit` in
`harness-perf.log` and `plan.edit` in the session trace. Stage start and completion
records separate queue and broker waits, semantic edits, declaration formatting,
individual revision writes, task and transcript reconciliation, review recapture,
per-file syntax work, projection, response delivery, and the final buffer update.
Failed and interrupted stages retain elapsed times. `ui.plan.rename_preview` and
`plan.rename.preview` measure preview preparation separately from confirmed edits.
These timing records contain identities, work counts, and durations, without plan
text or comment bodies. `/log on` enables both trace sources before reproduction.
The Configuration picker displays the provider CLI and exposes an explicit provider-switch action.

## Event transfer and validation

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

## Host output admission during migration

The root Forge executable now sends broker responses and events through `forge-protocol::outbound`.
Admission reserves storage before JSON encoding. Ordinary frames have a 512 KiB encoded limit.
Compact control records have a 4 KiB limit. One FIFO queue permits 128 outstanding frames with
32 records and 128 KiB reserved for control admission. The 8 MiB encoded-byte budget includes
frames currently being serialized and the frame held by the stdout writer. Dropping that frame
releases its reservation only after writing completes or fails. Reservation uses the maximum
frame size before serialization and refunds unused bytes afterward.

Serialization failure, output saturation, or a closed output channel poisons the connection.
The first producer failure remains the diagnostic returned to later senders and receivers.
Receivers recheck that failure after channel wakeup so producer shutdown cannot replace it
with clean EOF. Admission and encoding diagnostics include the observed queue or frame sizes.
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

## Snapshot transfers

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

## Native receive ownership

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

## Consumer credit and process shutdown

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
cancelled. Queue and read chunks remain bounded. `JsonLineReader` accumulates one complete
JSONL request above this adapter without an aggregate byte cap, preserving request content
regardless of the number of read chunks. Lua likewise sends the complete encoded request
without applying the host's outbound frame limit.

Producer-side output admission remains a separate hard bound. A producer can still exhaust its
encoded queue while the credit-gated writer waits. Pausing optional generation and replacing the
provider-internal unbounded event channel remain necessary for complete backpressure.

## Exact wire-version handshake

The client includes `protocol_version` in initialization parameters. The root host requires the
current `forge_protocol::WIRE_VERSION` before feature initialization or provider construction.
A mismatch returns `protocol_mismatch` and exits. Successful initialization includes the same
version in the initial result. Lua compares it with `forge.protocol.VERSION` before readiness and
follow-up requests. Wire version 2 separates the host handshake from `harness.initialize`.

The transport accepts one exact version. Missing, older, newer, and nonnumeric versions do not
select an alternative decoder. Rebuild and restart are the recovery action when local executable
and Lua transport code disagree. Existing Harness feature methods remain unchanged after the
successful handshake.

## Executable ownership

`forge.builder` delegates to independent `rust_sidecar` builders for development
and release profiles. Cargo output stays in the crate's default `target` directory,
separate from the runtime cache used for executable leases and diagnostics.
The default runtime uses `target/debug`, and `:ForgeBuild`
always builds that development executable. `:ForgeBuildRelease` explicitly builds
`target/release` without changing runtime selection. Setting
`vim.g.forge_build_profile = "release"` before loading Forge selects that runtime.
`for_profile()` returns a stable builder for explicit profile operations, so
concurrent requests for different profiles retain separate completion callbacks
and artifact paths. Contributors rebuild the selected executable before runtime
verification. The current `ensure()` implementation reuses an existing artifact
and invokes `build()` when it is absent. No source-freshness or
executable-fingerprint check selects a build.

Windows hosts share immutable images under `executables/<source>/<identity>/` in the sidecar cache.
The identity hashes the canonical source path and file metadata, without reading the executable
contents. Cache misses copy to a temporary file and publish atomically after checking that the
source did not change. Concurrent callers share publication. Each host pins the image with a
Windows read handle that denies writes and deletion until process exit. Releasing a host keeps
the current image available for the next editor. Deferred cleanup removes older images only
when Windows permits deletion, preserving running hosts and acquisitions holding a pin.
Other platforms launch the selected artifact directly. See
[Building and testing](../building-and-testing.md) for the contributor workflow.

Reusing the image avoids the repeated first-launch cost of per-process copies. A newly built
image can still incur Windows first-execution overhead. Cache-hit startup and first use of a
new build are measured separately.

`:ForgeStartupLog harness` opens the current editor's
`stdpath("cache")/rust-sidecar/forge/harness-startup-<pid>.jsonl`. Each Harness open
has a correlation identity, monotonic elapsed time, and time since the previous event through
host readiness, session initialization, snapshot application, native document
adoption, and the first displayed redraw. Rust initialization reports cumulative
milestones for repository discovery, provider infrastructure, SQLite opening,
session recovery, timeline loading, and presentation initialization. Native document
opening also separates entry projection, transcript construction, section projection,
and snapshot creation. These are
observations of existing work, without additional database reads. The dedicated
file uses asynchronous ordered writes and remains separate from high-frequency
RPC logs. Executable selection, copying, and process creation records are also
retained in that file. Acquisition records include cache hit, build identity, image size,
metadata lookup, copy, and image-pin times. The first host stdout event distinguishes process
launch return from protocol responsiveness. Startup failures and closure before the first redraw end the trace
explicitly. These lifecycle diagnostics remain available independently of provider
logging and exclude prompts and source content.

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

## MCP process framing

`forge mcp` shares the root host's `ThreadInput` and `JsonLineReader`. The input adapter
reads bounded chunks, and the JSONL reader retains fragments until a complete request arrives,
without a total request-size admission limit. Truncated JSON at EOF fails decoding without
writing a response fragment. A complete final JSON value at EOF receives normal processing.

The stateless MCP loop processes one request at a time and serializes its complete response,
including the original request ID, before writing the newline-terminated record. Response
size does not replace a result with an error or terminate the process. JSON serialization
and I/O failures propagate normally. Response values and encoded bytes remain in memory
during delivery. This MCP endpoint does not use Neovim's chunked response or receive-credit
protocol, whose per-frame and queue bounds remain separate.

This entry point remains the existing stateless control-tool transport. It does not create a
Harness session or launch a provider. Notification handling and normal tool response shapes remain
unchanged.

## Executor termination boundary

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

## Method validation before Harness state access

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

## Shared wire-record ownership

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

## Harness service ownership and quiescent lease release

`ForgeRuntime` constructs a lazy `HarnessService` alongside the shared repository store and diff
engine. Construction opens no provider, repository, or durable store. The service owns initialization,
session controllers, provider-fork gates, session ownership, and independent control routing. The root
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

## Connection ownership, EOF cancellation, and request drainage

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

`HostRouter` retains the status-update subscription and its output sender. After request and feature
producers quiesce, `ConnectionHost` signals that subscription to drain already-published updates and
joins its task before closing output admission. The subscription does not wait for the runtime-owned
broadcast source to disappear. Delivery and task failures participate in the connection failure result.

Connection drainage uses a two-second cooperative deadline. A deadline failure reports unfinished
request IDs and whether feature shutdown finished. The output task receives a stop signal and has a
further 100 ms cancellation deadline. The status forwarder can also be aborted after that connection
deadline because it owns no native work. Request tasks
remain retained. Main's existing runtime teardown still runs after the connection returns, so the
connection deadline is not a hard bound on total process exit time or native system calls.

Pipe tests cover EOF with an active mock turn, exhaustion of receive credit, and duplicate IDs during
an active request. Duplex tests cover a writer blocked inside its output write and a terminal response
held until unrelated registered work completes. Registry tests cover panic cleanup and retention of
runtime/admission ownership across a timeout.

Negotiated limits, per-operation cancellation and receipts, acknowledgement-bound request identity,
provider-process exit verification, complete global memory accounting, and all-platform shutdown
acceptance remain incomplete.

## Segmented completed results

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
