# Host, protocol, and execution ownership

An editor that blocks on its host, or a host that keeps running after its editor disappears, loses work in both directions. Forge therefore runs one local Rust host per editor process, carries document and operation identity on every request, and keeps each worker's ownership until its result or cancellation settles.

Read [Building and testing](../building-and-testing.md) for executable selection and verification. The protocol version constants and the checked-in `forge/protocol_contract.json` are authoritative.

## Ownership split

The Rust host serves repository operations, GitHub operations, generated documents, analysis, and Harness sessions. Lua owns Neovim buffers, windows, editing callbacks, and editor effects. Status, branch comparisons, local previews, source revisions, Walkthrough, Harness, and PlanReview use native document services, and PR, review, issue, and notification views use them through Lua editor adapters.

The [Lua client API](../api/lua-client.md) has one request-object entry point, and each request carries explicit session context according to the shared method policy. Shared-service initialization opens no interactive session. The host reader admits each request into its own tracked task before waiting on normalization or provider preparation, so a waiting request never blocks independent requests or transport credit.

## Executable

The `forge` package builds a library and a thin executable: `src/main.rs` parses arguments and calls the non-inlined `forge::run`, and `src/lib.rs` owns the host, router, runtime, and shutdown modules. `forge.builder` exposes independent development and release builders. The default runtime uses `target/debug`, `:ForgeBuild` builds it, and `:ForgeBuildRelease` builds `target/release` without changing which executable runs. Setting `vim.g.forge_build_profile = "release"` before Forge loads selects the release runtime. `ensure()` builds only when the artifact is missing, and no freshness or fingerprint check ever triggers a build.

Windows launches an immutable cached copy under `executables/<source>/<identity>/` in the sidecar cache, so Cargo can replace the build output while an editor runs. The identity hashes the canonical source path and file metadata. A miss copies to a temporary file and publishes it atomically, and each host pins its image with a read handle that denies writes and deletion until exit. Deferred cleanup removes older images only when Windows permits it. macOS and Linux launch the build output directly, because running processes keep the replaced file. A running host never adopts a rebuild, so verification needs a fresh host.

## Handshake and method validation

The client sends `protocol_version` during initialization, and the host requires the exact current `forge_protocol::WIRE_VERSION` before feature initialization or provider construction. Any other value returns `protocol_mismatch` and exits, because no alternative decoder exists. Rebuild and restart are the only recovery. The host handshake is separate from `session.initialize`, which lazily constructs Harness.

`HarnessMethod` enumerates every accepted Harness wire name. The router decodes the method before resolving a session, recording a trace, or arming provider cancellation, so an unknown name fails without touching Harness state. Host initialization, repository revisions, and shutdown have separate routes.

## Wire records and validation

`forge-protocol::message` owns `Request`, `Response`, `ProtocolError`, `SessionEvent`, and `Message`. A `Response` holds exactly one result or error, and a successful JSON null stays an explicit result. Deserialization rejects missing or duplicate IDs and outcomes, unknown fields, and records that mix response and event fields.

`forge/protocol_contract.json` defines the event vocabulary, routing policy, required payload fields, and node enum spellings. Rust embeds it at compile time, and Lua loads it at runtime. The broker consumes a typed provider event enum and publishes through a separate typed session-update boundary, so raw provider envelopes never reach document rendering. Serialization rejects undeclared events and missing fields. Lua validates direct frames and reassembled messages through the same validator, and unknown variants, conflicting identities, or invalid counters stop the connection. Null object fields become absent Lua fields, while null array entries keep their position. Any contract change requires a wire-version change.

Irrecoverable transcript publication failures carry `buffer::TranscriptProjectionFailure`, which `ConnectionHost` classifies as `invalid_state` regardless of its text. Lua then stops automatic retries, keeps the last applied transcript, and reports that reopening is required. Admission failures report `not_admitted`, and other failures report `request_failed`.

## Output admission and receive credit

All host output passes through `forge-protocol::outbound`, which reserves storage before encoding. Ordinary frames are limited to 512 KiB encoded and control records to 4 KiB. One FIFO queue admits 128 frames and 8 MiB of encoded bytes, with 32 records and 128 KiB reserved for control traffic. Control records never overtake earlier events. Serialization failure, saturation, or a closed channel poisons the connection, and the first failure stays the diagnostic for every later sender.

`ReceiveCredit` gates the stdout writer: at most 128 published but unconsumed frames and 8 MiB of bytes may be outstanding. `forge/receive.lua` buffers stdout callbacks without decoding them and drains complete frames in FIFO batches of at most 16 frames, usually within 256 KiB, on one scheduled callback. After each dispatched batch it sends cumulative byte and frame counts through `host.transport.consumed`, so credit describes completed client work rather than bytes accepted by the pipe. Rust requires each acknowledgement to cover an exact FIFO prefix, and regressing or mismatched counts reject the connection. Malformed framing, unterminated final data, or overflow stops the connection. Process exit waits behind admitted frames, so a terminal response is never overtaken.

Process stdin uses a dedicated blocking reader thread with a two-slot channel of 64 KiB chunks, so an idle pipe cannot stall runtime teardown. `JsonLineReader` assembles each complete request without an aggregate byte cap.

## Large transfers

A session event above the 512 KiB frame limit travels as transfer-identified parts plus a completion record. `forge-protocol::snapshot` splits document snapshots the same way, keyed by document identity, revision, transfer number, part sequence, and total bytes. Each document keeps one incomplete transfer, a newer transfer cancels its predecessor, and only a completely assembled and decoded snapshot reaches `apply_snapshot`.

Completed responses above 512 KiB, such as `github.detail` and `github.issues`, use request-correlated `result.part` events and one `result.complete` event. `JsonTransfer` splits encoded output at UTF-8 boundaries into payloads of at most 128 KiB and produces one part at a time, waiting for output capacity rather than poisoning the connection. Two shared permits bound concurrent encodings. Lua reserves the declared total for at most two active results and validates sequence, totals, and request identity before invoking the callback. Parts never reach subscribers, and an ordinary success cannot replace a partial transfer. A serialization failure after storage completes returns `result_encoding_failed` with `operation_completed: true`, because nothing was rolled back. No transfer has an aggregate size cap.

## Request tasks and shutdown

Each connection admits at most 64 request tasks. Ordinary requests can occupy 63, leaving one slot for cancellation, restart, approval resolution, or shutdown, and saturation returns a correlated `busy` response. `RequestTaskStore` records request IDs beside a bounded JoinSet and rejects duplicate IDs while their tasks run. The runtime owns the store, so a drain timeout never aborts a feature future.

`ConnectionHost` owns the handshake, input loop, output task, receive credit, and shutdown. EOF, transport failure, or explicit shutdown closes admission and starts runtime cancellation before output drains. During explicit shutdown the input loop keeps accepting credit, the host joins pending requests and waits for feature shutdown, and only then enqueues the terminal response in the same FIFO. Draining has a two-second cooperative deadline that reports unfinished request IDs, followed by a 100 ms output deadline.

`HarnessService` is constructed lazily and owns initialization, session controllers, provider-fork gates, and session ownership. Independent sessions share activity guards rather than one exclusive lock. Provider forks run in a retained JoinSet of at most 64 tasks. Shutdown closes admission, signals controllers, waits for dispatches and forks, then releases every controller lease. Both executable entry points finish through `shutdown::run`, which bounds executor teardown to 100 ms with Tokio's `shutdown_timeout`. That timeout releases the thread but cannot stop native work, so feature owners must persist required outcomes before returning.

## MCP endpoint

`forge mcp` is the stateless control-tool transport. It shares `ThreadInput` and `JsonLineReader`, processes one request at a time, and writes each complete response as one newline-terminated record without a size limit. Truncated JSON at EOF fails without writing a partial response. The endpoint creates no Harness session, launches no provider, and does not use Neovim's chunked transfer or receive credit.

## Diagnostics

`trace::TraceStore` owns opt-in protocol diagnostics independently of timeline persistence. `/log on`, `/log off`, and the `/config` Logging toggle control it and Lua performance logging together, bypassing the broker's turn lock. The aggregate `harness-trace.jsonl` keeps bounded metadata. Per-session `logs/<session-id>.jsonl` files keep request and response payloads with named credential fields redacted, rotate at 15 MiB with three older segments, and replace oversized records with an explicit omission. `/log`, `/log open`, and `:ForgeHarnessLog` render the current session file in a reusable read-only tab that `R` and focus events refresh. `/log clear` empties only the current session file and its rotated segments. Plan edits carry one `operation_id` across `harness-perf.log` and the session trace, and their timing records contain identities, counts, and durations, never plan text.

`:ForgeStartupLog` opens `stdpath("cache")/rust-sidecar/forge/startup-<pid>.jsonl`, and `:ForgeStartupLog harness` opens `harness-startup-<pid>.jsonl`. Together they record executable selection and copying, host spawn and initialization, request dispatch, native observation counters, snapshot application, and `status.first_redraw` when a decoration provider sees the populated buffer. Harness opens add correlated milestones through session initialization and the first redraw. Entries use monotonic microsecond timestamps, exclude prompts and source content, and go through one asynchronous ordered writer that rotates at 2 MiB without blocking editor callbacks. Queue waits overlap across frames and must not be summed. Setting `vim.g.forge_skip_line_stats = true` before the host starts passes `--diagnostic-skip-line-stats`, which leaves display counts unknown for comparison runs. Expansion and mutation settlement still acquire their content.

An opening keeps the existing buffer visible until a populated snapshot is ready, then replaces it only if the window still shows the original buffer. No loading placeholder replaces usable content.
