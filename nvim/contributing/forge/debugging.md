# Debugging Forge

Forge records evidence ahead of time, because a failed session or a vanished editor leaves nothing to inspect afterward. This guide covers the three logs and when to use each.

| Question | Log |
| --- | --- |
| What did the host and provider exchange? | [Protocol trace](#protocol-trace) |
| Why was startup slow? | [Startup log](#startup-log) |
| Why did the editor close? | [Session log](#session-log) |

## Protocol trace

`trace::TraceStore` owns opt-in protocol diagnostics, separately from timeline persistence. `/log on`, `/log off`, and the `/config` Logging toggle control it together with Lua performance logging. The aggregate `harness-trace.jsonl` keeps bounded metadata. Per-session `logs/<session-id>.jsonl` files keep request and response payloads with credential fields redacted, and rotate at 15 MiB with three older segments.

`/log`, `/log open`, and `:ForgeHarnessLog` show the current session file in a read-only tab. `/log clear` empties only the current session file and its segments. Plan edits carry one `operation_id` across `harness-perf.log` and the session trace, and their timing records never contain plan text.

## Startup log

`:ForgeStartupLog` opens `stdpath("cache")/rust-sidecar/forge/startup-<pid>.jsonl`, and `:ForgeStartupLog harness` opens `harness-startup-<pid>.jsonl`. They record executable selection, host spawn, request dispatch, snapshot application, and the first redraw. Entries use monotonic microsecond timestamps and exclude prompts and source content. Queue waits overlap across frames, so do not add them together.

Setting `vim.g.forge_skip_line_stats = true` before the host starts leaves display counts unknown, which is useful for comparison runs.

## Session log

An editor that vanishes leaves no error to read. The Neovim configuration therefore installs `session_log` after startup, and each process writes a timestamped JSONL file under `stdpath("log")/sessions`. Logs survive restarts, and concurrent editors write separate files. Print the current path with `:lua print(require("session_log").path())`.

The log records Normal-mode input and its effective mapping, command names without arguments, error notifications, changes to `v:errmsg`, one-second heartbeats, UI attachment events, and `QuitPre`, `ExitPre`, `VimLeavePre`, and `VimLeave`. Heartbeats include the current Harness session and its busy state when available. It does not record prompt text or Insert-mode input.

Reproduce the failure in a fresh editor, and keep that process's file before starting another investigation.

An exit-event sequence shows an orderly shutdown. A file that ends without exit events shows an abrupt stop, but it cannot tell a native crash from a forced termination or a power loss. Compare it with the operating system's crash reports and the shell's exit status. The logger does not cancel exits or recover from native faults.
