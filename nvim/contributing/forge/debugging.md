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

`:ForgeStartupLog` opens `stdpath("cache")/rust-sidecar/forge/startup-<pid>.jsonl`, and `:ForgeStartupLog harness` opens `harness-startup-<pid>.jsonl`. They record executable selection, host spawn, request dispatch, snapshot application, and the first redraw. Entries use monotonic microsecond timestamps and exclude prompts and source content. Queue waits overlap across frames, so do not add them together.

Both writers rotate at a 2 MiB threshold and retain the previous segment as `.1`.
The general startup stream also includes host stderr excerpts. Rotation happens
between write batches and preserves complete records, so a large batch can exceed
the threshold.

Setting `vim.g.forge_skip_line_stats = true` before the host starts leaves display counts unknown, which is useful for comparison runs.

## Performance logs

The repository's plugin configuration selects `log_level = "info"` and enables
`diff_logging`. Harness performance logging runs only at Trace; diff performance
logging remains independent. The former `harness_logging` Lua boolean migrates to
Trace or None when `log_level` is absent. Lua writes
`stdpath("cache")/forge/harness-perf.log` and `diff-perf.log`; each starts over at
15 MiB without retaining a previous segment.

These streams include render and patch timings, request/queue metadata, and
event-loop delay observations. Trace spans record begin/end boundaries; timed
spans record durations, with the default sample rate retaining fast spans too.
The host's `future.started` and `future.finished` trace records include elapsed
time, time spent polling, poll count, phase, and completion status when tracing is
enabled. These are operation timings, not a CPU profile or a stack trace of a stall.

The existing metadata collectors bound field counts and strings. The Lua collector
also drops records beyond its record or pending-queue budgets. Those limits mean
these performance logs are diagnostic samples, not a lossless execution history.

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
