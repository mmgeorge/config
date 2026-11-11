# Host and protocol

An editor that blocks on its host, or a host that keeps running after its editor disappears, loses work in both directions. Forge therefore runs one local Rust host per editor process, carries document and operation identity on every request, and keeps each worker's ownership until its result or cancellation settles.

The protocol version constants and the checked-in `forge/protocol_contract.json` are authoritative. [Building and testing](../building-and-testing.md) covers executable selection, and [Debugging](../debugging.md) covers logs.

## Ownership split

The Rust host serves repository operations, GitHub operations, generated documents, analysis, and Harness sessions. Lua owns Neovim buffers, windows, editing callbacks, and editor effects. The [Lua client API](../api/lua-client.md) has one request-object entry point, and each request carries explicit session context. The host reader admits each request into its own tracked task before waiting on normalization or provider preparation, so a waiting request never blocks other requests or transport credit.

## Executable

The `forge` package builds a library and a thin executable. `src/main.rs` parses arguments and calls `forge::run`, and `src/lib.rs` owns the host, router, runtime, and shutdown modules. The entry point installs one Rust `tracing` subscriber; it routes diagnostics to the live Harness data-root writer rather than standard output. Major IPC calls emit Info start/completion events with method, request identity, and elapsed time; document polling and transport-credit traffic remain Trace-only. No freshness or fingerprint check ever triggers a build. Tokio worker threads use an 8 MiB stack so foreground provider settlement fits within the Windows host runtime.

Windows launches an immutable cached copy under `executables/<source>/<identity>/` in the sidecar cache, so Cargo can replace the build output while an editor runs. Each host pins its image with a read handle until exit. macOS and Linux launch the build output directly, because running processes keep the replaced file. A running host never adopts a rebuild, so verification needs a fresh host. Disposable Harness execution state uses its own versioned directory (`state/v12/` for this contract), and each host retains the lease for its selected version. New hosts leave older leased state intact. Persistent settings and permissions remain outside that versioned history; an older executable must keep its own compatible state version.

## Handshake and validation

The client sends `protocol_version` during initialization, and the host requires the exact current `forge_protocol::WIRE_VERSION` before feature initialization. Any other value returns `protocol_mismatch` and exits, because no alternative decoder exists. Rebuilding and restarting is the only recovery. This handshake is separate from `session.initialize`, which lazily constructs Harness.

`HarnessMethod` enumerates every accepted Harness wire name. The router decodes the method before resolving a session, so an unknown name fails without touching Harness state.

## Wire records

`forge-protocol::message` owns `Request`, `Response`, `ProtocolError`, `SessionEvent`, and `Message`. A `Response` holds exactly one result or error, and a successful JSON null stays an explicit result. Deserialization rejects missing or duplicate IDs, unknown fields, and records that mix response and event fields.

`forge/protocol_contract.json` defines the event vocabulary, routing policy, required payload fields, and node enum spellings. Rust embeds it at compile time, and Lua loads it at runtime. Serialization rejects undeclared events and missing fields. Lua validates frames through the same contract, and unknown variants, conflicting identities, or invalid counters stop the connection. Any contract change requires a wire-version change. Provider events remain an internal typed boundary: the host resolves them to durable session/timeline records before translating them into this editor protocol. Host permission and retry events never become adapter publication authority. The level-based logging contract uses wire version 41; running components with an older contract reject the handshake before feature payload decoding.

Irrecoverable transcript publication failures are classified as `invalid_state`. Lua then stops automatic retries, keeps the last applied transcript, and reports that reopening is required. Admission failures report `not_admitted`. A request naming a durable session that does not exist reports `session_not_found`; Lua treats it as terminal, stops retrying, and marks the view because a deleted session never becomes readable again. Other failures report `request_failed`. `forge_harness::broker::request_failure_code` owns this classification for both the host and broker responses.

## Flow control

All host output passes through `forge-protocol::outbound`, which reserves storage before encoding. Ordinary frames are chunked to at most 512 KiB encoded and control records to 4 KiB. Queue admission allows 4096 frames, with 32 records reserved for control traffic and no aggregate byte ceiling. Serialization failure or a closed channel poisons the connection, and the first failure stays the diagnostic for every later sender. Saturation is not a failure: the non-waiting `send` returns `WouldBlock` and leaves the connection open, the waiting senders wait for frame capacity, and `send_progress` discards an advisory progress frame. Request tasks therefore send responses and lossless events through the waiting senders, so an editor that stops draining delays them rather than ending the host.

The request reader cannot wait for output, because it also reads the `host.transport.consumed` acknowledgements that let the writer drain. Failures the reader produces itself, such as undecodable requests, `busy`, and generation admission rejections, use the reserved control lane and queue in order while that lane is full; the reader admits them as capacity returns, and shutdown delivers the remainder before its terminal response. A request whose response cannot be delivered is logged and affects only that request, and a request task that panics is answered with a failure response instead of closing the connection.

`ReceiveCredit` gates the stdout writer, so at most 4096 unconsumed frames may be outstanding. The host and Lua receiver impose no aggregate byte ceiling. `forge/receive.lua` drains complete frames in FIFO batches on one scheduled callback, then sends cumulative counts through `host.transport.consumed`, so credit describes completed client work rather than bytes accepted by the pipe. Regressing or mismatched counts reject the connection.

A receiver failure while the process is running, such as an undecodable frame, stops the host through `host_stop`. A failure after the process has exited, such as stdout that ends inside a frame, passes the exit result to the client, which finishes the exit directly: pending callbacks fail, subscribers receive `host_stopped`, and the next start launches a new host. Draining a process that has already exited would wait on a forced collection that cannot succeed.

Process stdin uses a dedicated reader thread, so an idle pipe cannot stall teardown. `JsonLineReader` assembles each complete request without an aggregate size cap.

## Large transfers

A session event above the 512 KiB frame limit travels as parts plus a completion record. `forge-protocol::snapshot` splits document snapshots the same way. Each document keeps one incomplete transfer, a newer transfer cancels its predecessor, and only a completely assembled snapshot reaches `apply_snapshot`.

Responses above 512 KiB use request-correlated `result.part` events and one `result.complete` event. `JsonTransfer` splits encoded output into payloads of at most 128 KiB and waits for capacity rather than poisoning the connection. Parts never reach subscribers. A serialization failure after storage completes returns `result_encoding_failed` with `operation_completed: true`, because nothing was rolled back. No transfer has an aggregate size cap.

## Request tasks and shutdown

Each connection admits at most 64 request tasks. Ordinary requests can occupy 63, leaving one slot for cancellation, restart, approval resolution, or shutdown, and saturation returns a correlated `busy` response. `RequestTaskStore` rejects duplicate IDs while their tasks run.

`ConnectionHost` owns the handshake, input loop, output task, receive credit, and shutdown. EOF, transport failure, or explicit shutdown closes admission and starts cancellation before output drains. During explicit shutdown, the host joins pending requests and waits for feature shutdown, and only then enqueues the terminal response. Draining has a two-second deadline that cancels and reports unfinished request IDs. A 500 ms cancellation deadline follows it, after which the host stops waiting for cancelled requests or an undrained terminal response and reports the stall, then a 100 ms writer deadline releases stdio. The whole sequence ends before the editor's three-second kill.

`HarnessService` is constructed lazily and owns session controllers and fork gates. Shutdown closes admission, signals controllers, waits for dispatches and forks, then releases every lease. Background generation shuts down concurrently under the same deadline, so a slow Background task reports its own timeout without skipping the session owners. Both executable entry points finish through `shutdown::run`, which bounds executor teardown to 100 ms. That timeout releases the thread but cannot stop native work, so feature owners must persist required outcomes before returning.

## MCP endpoint

`forge mcp` is the stateless control-tool transport. It processes one request at a time and writes each complete response as one newline-terminated record. It creates no Harness session, launches no provider, and does not use Neovim's chunked transfer or receive credit.

## Opening a view

An opening keeps the existing buffer visible until a populated snapshot is ready, then replaces it only if the window still shows the original buffer. No loading placeholder replaces usable content.
