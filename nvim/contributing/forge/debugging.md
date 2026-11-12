# Debugging Forge

Forge records evidence ahead of time, because a failed session or a vanished editor leaves nothing to inspect afterward. This guide covers diagnostic and performance logs, their contents, and their retention boundaries.

| Question | Log |
| --- | --- |
| What did the host and provider exchange? | [Protocol trace](#protocol-trace) |
| Why was startup slow? | [Startup log](#startup-log) |
| Which operation blocked the UI or host? | [Performance logs](#performance-logs) |
| Why did the editor close? | [Session log](#session-log) |

## Protocol trace

Rust `tracing` supplies instrumentation; `trace::TraceStore` retains the existing shared writer, state lease, ordered shutdown, and JSONL files separately from timeline persistence. `/log` opens the level picker, also available through `/config` Logging. Levels are cumulative: None disables recording, Error records errors, Warn adds warnings, Info adds major IPC and provider lifecycle operations, and Trace adds verbose events and Lua Harness performance logging. New configurations default to Info.

Info records method, request and operation identities, outcome, and duration rather than per-frame activity. The subscriber filters events before formatting their fields; dependency Info and Debug events require Trace. Async correlation spans retain session and operation identities across provider workers. Local diagnostic records can include native error messages, cause chains, source locations, and span context. Credential patterns are redacted, and request/response bodies and unknown native fields are excluded; native messages can nevertheless contain sensitive context, so inspect logs before sharing them. Normalized provider errors still control lifecycle and retry policy independently of diagnostics.

Info, Warn, and Error serialize enqueue operations through a short in-memory critical section; file I/O remains on the writer thread. Trace producers may yield under contention, and all levels remain best-effort when the existing bounded queue saturates. The log viewer exposes the rejected-record count rather than treating missing diagnostics as execution failure.

The aggregate `harness-trace.jsonl` and per-session `logs/<session-id>.jsonl` files keep one logical JSON record per line, with `timestamp_ms`, `session_id`, `event`, and `payload`. Severity and diagnostic context live in the payload. Session files rotate at 100 MiB and retain ten older segments; large logical records use the existing chunk envelopes across segments. The
`harness_session_log` tool exposes only allowlisted diagnostic metadata and reads
up to 1,000 complete entries per page by default, newest-first, with a positive
`limit` override. Log pages are not split by the response byte budget. Its cursor keeps
the captured file extents and excludes later appends. Rotation renames the retained
files so an open cursor can finish reading its original segments. The aggregate
file starts over at its 100 MiB threshold without keeping an older segment.

`/log open` and `:ForgeHarnessLog` show the current session file in a read-only tab. `/log clear` empties only that file and its segments. `/log none|error|warn|info|trace` sets the level directly. The saved setting takes precedence over the Lua `log_level` default. Legacy `enabled: true` and `false` settings migrate to Trace and None respectively; malformed or unsupported settings fail explicitly without replacing the file. Plan edits carry one `operation_id` across `harness-perf.log` and the session trace, and their timing records never contain plan text.

## Startup log

`:ForgeStartupLog` opens `stdpath("cache")/rust-sidecar/forge/startup-<pid>.jsonl`, and `:ForgeStartupLog harness` opens `harness-startup-<pid>.jsonl`. They record executable selection, host spawn, request dispatch, snapshot application, and the first redraw. Entries use monotonic microsecond timestamps and exclude prompts and source content. Queue waits overlap across frames, so do not add them together. The per-request events `host.request.send`, `host.response.received`, and `host.result.decoded` are written only at the Trace logging level, because a streaming session issues several requests per rendered frame; lifecycle events are written at every level.

Both writers rotate at a 2 MiB threshold and retain the previous segment as `.1`.
The general startup stream also includes host stderr excerpts. Rotation happens
between write batches and preserves complete records, so a large batch can exceed
the threshold.

Setting `vim.g.forge_skip_line_stats = true` before the host starts leaves display counts unknown, which is useful for comparison runs.

## Performance logs

The repository's plugin configuration selects `log_level = "info"` and enables
`diff_logging`. Detailed Harness performance logging runs at Trace; Info retains
sparse slow-UI and queue-edit diagnostics. Diff performance logging remains
independent. The former `harness_logging` Lua boolean migrates to
Trace or None when `log_level` is absent. Lua writes
`stdpath("cache")/forge/harness-perf.log` and `diff-perf.log`; each rotates at
100 MiB and retains ten older segments.

These streams include render and patch timings, request/queue metadata, and
event-loop delay observations. Trace spans record begin/end boundaries; timed
spans record durations, with the default sample rate retaining fast spans too.
The host's `future.started` and `future.finished` trace records include elapsed
time, time spent polling, poll count, phase, and completion status when tracing is
enabled. These are operation timings, not a CPU profile or a stack trace of a stall.

The existing metadata collectors bound field counts and strings. The Lua collector
also drops records beyond its record or pending-queue budgets. Those limits mean
these performance logs are diagnostic samples, not a lossless execution history.
Info records renders and buffer patches taking at least 100 ms, event-loop delay
of at least 250 ms, and queue reservation failures, pending-removal rejections,
and missing-entry draft recovery. These editor-local records use the same
`harness-perf.log`, independently of host responsiveness; `/log open` continues
to show the host log. The loop watcher runs only while a Harness transcript is
displayed. Changing `/log` updates both detailed and sparse recording.

The host reports its own scheduling delay. `src/runtime_lag.rs` asks the Tokio
runtime for a wakeup every 100 ms and logs `runtime.scheduling_lag` at Warn, at most
once per second, when a wakeup arrives 50 ms or more late. A record means a worker
thread blocked on a disk, a lock, or a long computation while other tasks waited.
The alarm is coarse: another worker can run the timer on time, so a short stall of
one worker passes unseen.

Each of the five fixed event categories emits immediately, then at most once per
ten seconds per editor. The next emitted record includes the suppressed count
and maximum duration from the preceding interval; a final quiet interval has no
forced summary. Suppressed counts can span sessions; the emitted record's IDs
identify that individual occurrence. Metadata is allowlisted and strings are
bounded to 256 bytes; drafts, attachments, and raw error text are excluded.
Pending writes are bounded by 4,096 records and 1 MiB per scope, with drop counts
available as `rejected_record_count`. Detailed Trace timing replaces the sparse
render, patch, and lag records rather than duplicating them.

Queue saturation is checked before metadata collection and JSON encoding, so
discarded records do not pay that serialization cost. Trace still records every
begin/end boundary and can produce substantial traffic; use Info for routine work
and enable Trace for a bounded reproduction. Timing records identify the slow
boundary but do not identify native buffer callbacks, CPU stacks, or lock owners.

## Streaming cadence

Choppy streamed text has three possible sources: the provider delivering in bursts, the host's 16 ms delta window, or slow editor work per update. Telling them apart needs one Trace capture. Run `/log trace`, stream a long response, then restore the previous level.

The host's `provider.event.consumed` record carries `bytes`, the UTF-8 length of an output delta batch, and never its text. The `sequence` gap between consecutive delta records counts the native positions that one batch absorbed, including any interleaved non-delta events. `timestamp_ms` is wall-clock time with millisecond resolution. The following Nushell pipeline prints each batch's gap, position count, and size from the session file that `/log open` shows:

```nu
open --raw $log
| lines
| each { from json }
| where {|record| $record.event? == "provider.event.consumed" and $record.payload?.bytes? != null }
| each {|record| { time: $record.timestamp_ms, sequence: $record.payload.sequence, bytes: $record.payload.bytes } }
| window 2
| each {|pair| { gap_ms: ($pair.1.time - $pair.0.time), positions: ($pair.1.sequence - $pair.0.sequence), bytes: $pair.1.bytes } }
```

Batches that absorb many positions and arrive well over 16 ms apart point upstream. A median gap near 31 ms between dense batches means the host window rounds up to the Windows tick. Editor cost appears in `harness-perf.log` as `ui.request` gaps for the `sync` operation, `ui.buffer.patch` phase timings, and `ui.render`. A `ui.buffer.patch` time that rises over the course of one long response means that per-update cost grows with message length.

## Streaming investigation, 2026-11-12

One Trace capture of a Codex response measured the path after the editor stopped delaying displayed transcripts. The response streamed 18,467 bytes in 363 host batches over 27.4 seconds and filled 262 transcript rows. No Claude response was captured.

Codex delivers in bursts. The median gap between host batches was 77 ms (p10 21 ms, p90 126 ms), and 196 of 362 gaps were 60 ms or longer. A median batch absorbed 9 native positions and 47 bytes. A host window that merges nine positions and then waits 77 ms for the next delta is idle, not limiting, so the gaps originate before Forge: in the Codex CLI or its service.

The host window does not consistently round to the Windows tick. No gap fell below 15 ms, 28 fell between 15 and 19 ms, and 46 between 30 and 35 ms, so the 16 ms timeout often fires on time.

The editor adds little. Each `sync` round trip took a median 0.80 ms (p90 1.14 ms), a patch 0.48 ms, and `ui.render` 0.34 ms. Patch time did not grow over the response: its median was 0.54 ms in the first quarter and 0.46 ms in the last. Neither a transcript-only render path nor narrower message patches would change what a reader sees at this size.

Smoother display therefore needs an editor-side reveal that spreads each burst over the following gap, which trades some latency for continuity. These figures come from one response from one provider and do not establish how other providers or longer messages behave.

## Queue-edit investigation, 2026-11-10

The retained foreground session `8dcba1ee-f6df-49cd-b6e4-48990555e14b`
in state version 17 records a successful removal at input revision 481, followed
by a reservation for the same removed entry at revision 482 (`input_missing`).
The editor allowed reopening the entry while its removal was in flight; the
deferred reservation then ran after deletion. The queue editor now blocks that
reopening and converts a missing edit target to a normal draft, preserving text
and attachments and requiring a separate submit to send it.

The removal RPC took 5.64 seconds and the stale reservation took 10.60 seconds.
For the reservation, 7.53 seconds elapsed before owner intake, 2.21 seconds before
command execution, and 0.86 seconds during execution. Publication continued in
the interval. These timings establish latency, but do not identify a SQLite lock
owner or prove database contention.

Editor performance logging was already enabled. It recorded buffer patches of
1.53, 1.80, and 2.37 seconds, almost entirely inside the native buffer API, with
zero editable-callback time for those patches. Markdown parse time in the
surrounding 50-second window totaled approximately 75 ms. A later read-only
inspection found 1,001 undo entries in the transcript buffer; this is a lead,
not evidence that undo caused the earlier stalls. Native stack profiling remains
necessary to attribute that time. The available Samply installation could not
record because Windows Performance Toolkit/xperf was unavailable.

The `exchange_profile` example accepts a read-only database path and exchange ID,
then measures serialization, fresh temporary-store writes, and timeline updates
without resuming the session. Replaying the retained 5,196,912-byte exchange in
the dev profile measured saves at 271–278 ms before the question-merge fast path
and 141–146 ms afterward. This microbenchmark isolates repeated whole-record JSON
work; it is not an end-to-end latency guarantee. Stored answers and withdrawals
still use the merge path and have regression coverage.

## Session log

An editor that vanishes leaves no error to read. The Neovim configuration therefore installs `session_log` after startup, and each process writes a timestamped JSONL file under `stdpath("log")/sessions`. Logs survive restarts, and concurrent editors write separate files. Print the current path with `:lua print(require("session_log").path())`.

The log records Normal-mode input and its effective mapping, command names without arguments, error notifications, changes to `v:errmsg`, one-second heartbeats, UI attachment events, and `QuitPre`, `ExitPre`, `VimLeavePre`, and `VimLeave`. Heartbeats include the current Harness session and its busy state when available. It does not record prompt text or Insert-mode input.

The writer rotates before a record crosses 2 MiB and retains one previous segment
as `.1`; a single oversized diagnostic remains intact. Writes stay synchronous so
exit observers do not depend on a later event-loop callback to save their evidence.
Rotation bounds each stream, not the whole directory: separate session IDs and
editor process IDs retain separate files, with no age-based directory cleanup.

Reproduce the failure in a fresh editor, and keep that process's file before starting another investigation.

An exit-event sequence shows an orderly shutdown. A file that ends without exit events shows an abrupt stop, but it cannot tell a native crash from a forced termination or a power loss. Compare it with the operating system's crash reports and the shell's exit status. The logger does not cancel exits or recover from native faults.
