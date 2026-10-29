# Host methods

The native connection owns these methods. None invokes a model, performs a remote mutation, or selects an active session. The [usage ledger](../../../../md/rename-usage-ledger.json) records their callers, and the shared contract supplies their transport policy.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `host.initialize`

Negotiate the client/host protocol and initialize host-owned storage paths.

- **Context:** Host connection.
- **Input:** protocol_version plus optional recovery_directory, status_ignored_directory, and git_config_cache_path.
- **Result and effects:** Returns the negotiated version and startup/cache information.
- **Boundary:** A version mismatch prevents feature dispatch.
- **Inspected source:** [`nvim/rust/forge/src/host.rs`](../../../rust/forge/src/host.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `control`. Recovery: `none`.

**Parameter schema:** [HostInitialize](native-schemas.md#hostinitialize).

**Current caller:** [nvim/lua/forge/client.lua:570](../../../../nvim/lua/forge/client.lua).

The first frame on a new connection must be this request. The decoder rejects unknown parameter fields.

| Field | Type and default | Meaning |
| --- | --- | --- |
| `protocol_version` | Required unsigned 32-bit integer | Must equal `29`. |
| `recovery_directory` | Optional path string, omitted/null means unconfigured | Directory for GitHub mutation recovery records. |
| `status_ignored_directory` | Optional path string, omitted/null means unconfigured | Directory for persisted Status ignore state. |
| `git_config_cache_path` | Optional absolute path string, omitted/null disables this initialization step | Git configuration-location cache file. A relative path fails validation. |

Success returns `protocol_version`. When a Git cache path is supplied, `git_config_cache` also reports its initialization `state`. A cache failure returns `state: "error"` and `warning` without failing the handshake. Invalid recovery or ignore configuration returns `invalid_request`. A version mismatch returns `protocol_mismatch` with `data.expected_version`, and the host closes instead of admitting feature work. No progress events accompany the handshake. Wait for its response before sending other requests. Reconnecting starts a new handshake and does not recover unknown mutation outcomes by itself.

The minimal exchange is exercised by [the host handshake fixture](../../../../nvim/tests/forge/host_initialize.lua):

```json
{"id":1,"method":"host.initialize","params":{"protocol_version":29}}
```

```json
{"id":1,"result":{"protocol_version":29}}
```

## `host.shutdown`

Stop accepting work and drain the shared host.

- **Context:** Host connection.
- **Input:** An empty parameter object.
- **Result and effects:** Returns shutdown acknowledgement after the host's shutdown path settles.
- **Boundary:** A lost connection does not establish the outcome of an unfinished mutation.
- **Inspected source:** [`nvim/rust/forge/src/host.rs`](../../../rust/forge/src/host.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `control`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:1060](../../../../nvim/lua/forge/client.lua).

There are no parameter fields or nested operations. The host stops admission, collects outstanding work and producers, and sends the terminal `shutdown: true` response before the connection exits. It may still require receive-credit frames while draining buffered output. Sending ordinary work after shutdown starts is invalid. A broken pipe or collection failure can prevent the acknowledgement, so the caller must also observe process exit and retain unresolved mutation receipts. The request emits no progress and does not delete session storage. It is a connection lifecycle operation, not a replayable service mutation.

[The same handshake fixture](../../../../nvim/tests/forge/host_initialize.lua) sends shutdown and verifies process collection:

```json
{"id":2,"method":"host.shutdown","params":{}}
```

```json
{"id":2,"result":{"shutdown":true}}
```

## `host.trace.configure`

Set whether tracing is enabled.

- **Context:** Host tracing.
- **Input:** enabled, with optional default_only for initial defaults.
- **Result and effects:** Persists/applies trace configuration and returns status.
- **Boundary:** default_only must not overwrite an established preference.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `control`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:527](../../../../nvim/lua/forge/client.lua).

Without session context, `path` identifies `harness-trace.jsonl`. With explicit context, it identifies that session's log. Configuration still changes the shared tracing setting. An explicit unknown session fails validation rather than selecting another log.

| Field | Type and default | Meaning |
| --- | --- | --- |
| `enabled` | Required boolean | Desired shared tracing state. |
| `default_only` | Optional boolean, default `false` | Apply the preference only if no persisted trace configuration exists. |

Success returns `{enabled: boolean, configured: boolean, path: string}`. `configured` reports whether the preference file exists. The returned path is a location, not a promise that a log file already contains events. Applying an unchanged setting is valid. Configuration writes local trace preferences and may open the local log, with I/O failures returned to this request. It has no progress stream or provider/network work. Repeating the same desired state is safe. Concurrent callers share one trace store, so a later successful write can replace the earlier setting. Applying the returned status to UI still requires the caller's captured view/session to remain current.

With the runtime data directory `/tmp/forge`, a successful example is:

```json
{"id":3,"method":"host.trace.configure","params":{"enabled":true}}
```

```json
{"id":3,"result":{"enabled":true,"configured":true,"path":"/tmp/forge/harness-trace.jsonl"}}
```

[The trace control regression](../../../rust/forge/crates/forge-harness/src/service.rs) exercises this operation while a provider owns the session broker. [`shared::trace`](../../../rust/forge/crates/forge-harness/src/service/shared.rs) owns its decoding and response.

## `host.trace.status`

Read tracing status and the selected log information.

- **Context:** Host tracing.
- **Input:** No mutation fields.
- **Result and effects:** Returns current trace configuration/status.
- **Boundary:** Reading status does not enable tracing.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/settings.lua:163](../../../../nvim/lua/forge/views/harness/settings.lua).

Without session context, `path` identifies `harness-trace.jsonl`. With explicit context, it identifies that session's log. An explicit unknown session fails validation rather than selecting another log.

There are no parameter fields or nested operations. The result has the same three fields as `host.trace.configure`. This read does not write preferences, invoke a provider, or emit progress. The local preference can change immediately after the response, so it is an observation rather than a lock. Retrying a failed read is safe. Keep the captured UI owner when displaying the path.

For an unconfigured runtime rooted at `/tmp/forge`:

```json
{"id":4,"method":"host.trace.status","params":{}}
```

```json
{"id":4,"result":{"enabled":false,"configured":false,"path":"/tmp/forge/harness-trace.jsonl"}}
```

The [shared-runtime regression](../../../rust/forge/crates/forge-harness/src/service.rs) checks this read without an interactive session. The [trace handler](../../../rust/forge/crates/forge-harness/src/service/shared.rs) also validates explicit context.

## `host.transport.consumed`

Return receive credit for data already processed by Lua.

- **Context:** Host connection.
- **Input:** Cumulative bytes and frames counters.
- **Result and effects:** Updates outbound flow control without a normal feature result.
- **Boundary:** Counters belong to the current connection and cannot be reused after replacement.
- **Inspected source:** [`nvim/rust/forge/src/host.rs`](../../../rust/forge/src/host.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/client.lua:685](../../../../nvim/lua/forge/client.lua).

| Field | Type | Meaning |
| --- | --- | --- |
| `bytes` | Required unsigned integer | Cumulative bytes in the processed prefix of output frames, including newline delimiters. |
| `frames` | Required unsigned integer | Number of frames in that same prefix. |

Both counters start at zero on each connection. They must be nondecreasing and identify an exact published prefix. Duplicate acknowledgements are valid. An unknown field, a counter beyond the safe integer domain, a regression, or mismatched byte/frame totals is a connection protocol error. Credit never acknowledges business-operation completion and has no durable state, provider invocation, or network activity.

This frame has **no response or progress event**. The Lua transport owns it and does not register a normal request callback. The zero-prefix example is valid immediately after handshake and grants no new credit:

```json
{"id":0,"method":"host.transport.consumed","params":{"bytes":0,"frames":0}}
```

[Receive-credit validation](../../../rust/forge/crates/forge-protocol/src/credit.rs) verifies exact prefixes, and [the host fixture](../../../../nvim/tests/forge/host_initialize.lua) grants the real handshake byte count before shutdown. The [host reader](../../../rust/forge/src/host.rs) intercepts this frame even while ordinary requests drain.
