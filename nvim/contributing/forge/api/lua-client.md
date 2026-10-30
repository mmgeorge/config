# Lua client

`forge.client` owns one native host connection. Requests carry their own session context. Changing the active Harness view does not change an admitted request's target.

## `request(spec, callback, progress)`

Send one asynchronous operation. Requests without session-command recovery return their transport correlation ID after admission. Validation or admission failure, and session commands managed by recovery, return nil. The function supplies no generic cancellation handle.

| Argument | Contract |
| --- | --- |
| `spec.method` | Required exact method name from `lua/forge/protocol_contract.json`. |
| `spec.params` | Optional object with string field names. Omission sends an empty object. Method handlers own field validation. |
| `spec.session_id` | Nonempty string when supplied. The method policy requires, permits, or forbids this field. |
| `callback` | Optional `function(result, failure, detail)`. Omission discards the result. |
| `progress` | Optional `function(payload)` for the selected method's request-correlated progress. Progress does not complete the request. |

Additional specification fields, unknown methods, array parameters, and invalid session context fail before host startup. The callback receives `nil`, an error message, and `{ code = "invalid_request" }`.

Lua allocates a correlation ID, reserves admission capacity, ensures the required service is ready, and sends the envelope. The wire contains `id`, `method`, `params`, and optional `session_id`. Lua functions never appear in the envelope. The pending record is removed before a terminal callback runs. Responses can complete in a different order from submission. Callback exceptions produce diagnostics without preventing another request's completion.

Host readiness starts the process and negotiates wire version 29. Runtime readiness additionally opens shared storage and services through `session.initialize` with `services_only = true`. Shared initialization creates no session and selects no active session. A mismatched host fails startup and drains the connection. Build, launch, serialization, initialization, and admission failures reach the callback.

The client admits at most 64 pending requests, with four slots reserved from ordinary requests for control traffic. This limits concurrent work, not valid content size. Admission failure reports `not_admitted`. Actual native admission can also reject a request.

On success, `callback(result, nil, nil)` receives the method's result. A service failure supplies `callback(nil, message, detail)`, where `detail` preserves the native error code and fields. A method error completes its own request. A malformed transport message or lost process affects the connection. Replies from a replaced host generation cannot complete requests in the new generation.

Empty parameters encode as `{}`, including an explicitly supplied empty Lua table. The decoder removes JSON null object members, retains null array entries as `vim.NIL`, and delivers a top-level null result as nil without an error. Wire examples preserve null fields even when the corresponding Lua result omits them.

### Session-independent requests

Repository operations omit `session_id`. Keep the workspace captured from the initiating action rather than rereading it in a delayed callback.

```lua
local client = require("forge.client")
local workspace = vim.fn.getcwd()
client.request({
  method = "repository.revision.list",
  params = { workspace = workspace },
}, function(result, failure)
  if failure then
    vim.notify(failure, vim.log.levels.ERROR)
    return
  end
  vim.print(result)
end)
```

The result is owned by `repository.revision.list`. It is not a session snapshot. Shared operations such as `permissions.document.open` also omit session context and do not need an interactive Harness session.

### Existing-session requests

Capture the ID from the view that owns the action. Session-prefixed names do not by themselves determine context requirements. Creation and discovery explicitly permit calls without an existing ID.

```lua
local client = require("forge.client")
local owner_session_id = owner.session.id
vim.schedule(function()
  client.request({
    method = "session.state.get",
    session_id = owner_session_id,
  }, function(snapshot, failure)
    if failure then
      vim.notify(failure, vim.log.levels.ERROR)
      return
    end
    assert(snapshot.session.id == owner_session_id)
    owner.snapshot = snapshot
  end)
end)
```

When the target has a registered Harness presentation, the callback runs in that presentation's context and then restores the previous active presentation. Unregistered targets still retain the explicit wire ID. The transport never substitutes the active presentation's ID.

### Creation and discovery

Discovery has no target. Creation returns the new ID. Omitting `source_session_id` creates from configured defaults rather than inheriting from the active view.

```lua
local client = require("forge.client")
client.request({ method = "session.list", params = { scope = "all" } }, function(result, failure)
  if failure then
    vim.notify(failure, vim.log.levels.ERROR)
    return
  end
  vim.print(result)
end)
client.session_create({ name = "Investigation" }, function(snapshot, failure)
  if failure then
    vim.notify(failure, vim.log.levels.ERROR)
    return
  end
  client.request({ method = "session.state.get", session_id = snapshot.session.id }, vim.print)
end)
```

### Progress and cancellation

`generation_request` consumes `repository.generation.progress` and translates model/status events into `on_model` calls. A direct request can use its third argument to receive the same method-owned progress payload. Request progress is correlated by request ID. Session and document events use separate subscriptions below.

There is no transport-wide cancel handle. Cancellation uses a documented operation such as `session.turn.cancel` or a generation operation's cancel action. Cancellation is itself asynchronous. A request that reports cancellation can still have committed effects documented by its service.

### Mutation outcomes

Methods with `session_command` recovery and an explicit session ID preserve one command identity across reconciliation. The client checks `session.command.status.get` after an uncertain result. It replays only when the coordinator reports `not_admitted`. A host-generation change reports `outcome_unknown` and does not replay the command into the new host.

Repository, GitHub, and shared-service mutations retain their own operation identities and recovery contracts. A transport request ID is not a durable operation ID. Callers must not interpret connection loss as proof that a mutation failed to commit.

## Lifecycle and accessors

### `host_start(callback)`

Start or reuse the native host without opening a session. The optional callback receives `{ generation = integer }` or a failure. Concurrent startup callers share startup. This function can build/acquire the executable and launch the process. A draining process rejects startup. Wire-version mismatch, unavailable executable, or replaced generation fails explicitly.

### `host_stop(reason, on_stopped)`

Drain the shared host and settle pending callbacks. `reason` is an optional diagnostic string. `on_stopped` is an optional no-argument callback after process collection. Repeated calls share an ongoing drain. The function closes the LSP bridge and can force collection after the graceful deadline. Unknown mutation outcomes remain unknown.

### `host_get_generation()`

Return the current local integer generation without I/O. A new process advances the generation. The value guards delayed replies and is not a durable session identity.

### `host_is_accepting()`

Return whether a process exists and is not draining. This predicate does not promise initialized services or spare admission capacity.

### `session_initialize(callback, options, startup)`

Ensure the runtime and requested initial-session workflow. `callback` is optional. `options` is optional and accepts `services_only`, `session_id`, `new_session_name`, or `lease_conflict_action = "new"`. `startup` is an optional timing observer used by the Harness view.

`services_only = true` returns `{ initialized = true }` without a session. Otherwise the callback receives a session snapshot. An explicit `session_id` resumes that ID, including when another session is already initialized. An explicit new-session choice creates a new session. Concurrent callers retain their own choices. No explicit-target failure selects another session.

The first interactive initialization with no options uses the configured initial-session workflow. This convenience belongs to opening the Harness UI. General requests use service-only initialization and cannot invoke it as a side effect.

### `session_create(options, callback)`

Create a persisted session and return its snapshot asynchronously. Optional `options.name` names the session. Optional `options.source_session_id` selects inherited context. Omission does not read the active session. Read the new identity from `result.session.id`. Invalid source IDs and storage/provider initialization failures reach the callback.

### `session_resolve_lease_conflict(action, conflict, callback)`

Resolve an initial-session lease conflict. `action` is `retry`, `new`, or `fork`, and `conflict.session_id` identifies the conflicted session. `retry` retains that target. `new` creates another session. `fork` forks the explicit target after startup. The optional callback receives the resulting snapshot or error.

## Event subscriptions

### `session_events_subscribe(callback)`

Register `function(event, payload, session_id)` and return a no-argument unsubscribe function. Registration performs no host request. The stream includes multiple sessions, so subscribers filter using their owned session IDs. Unsubscribe when disposing the owner. Subscriber failures are isolated from other delivery.

### `document_events_subscribe(document, callback)`

Register `function(event, update, generation)` for the supplied document identity and return an unsubscribe function. Registration does not initialize the host. The view validates document revision and generation before applying delayed updates, then unsubscribes when disposed.

## Background generation

### `generation_get_config(selection)`

Resolve backend launch data, model, reasoning effort, service tier, compaction preferences, and storage path without launching a provider. An explicit `selection` accepts `model`, optional `reasoning_effort`, optional `service_tier` (`default`, `fast`, or `ultrafast`), and optional `auto_compaction`. Omission uses configured background-generation preferences and the initiating UI's selected backend. Return values are `config` on success or `nil, failure` for an unknown backend or invalid model selection.

### `generation_request(params, callback, on_model, selection)`

Resolve generation settings and send `repository.generation`. `params` retains the generation operation's fields. The helper adds `generation`, calls optional `on_model(text)` before dispatch and on model/status progress, and delivers the terminal outcome to `callback`. Configuration failure reaches `callback` without launching a provider.

The returned idempotent function cancels only this request. Before dispatch, cancellation removes the queued request and calls `callback` with a cancellation error. After dispatch, it sends the original request ID to the generation cancellation operation. The original callback reports the eventual terminal outcome, and further progress is suppressed. Cancellation does not stop the shared host. Provider invocation and local writes depend on the selected generation operation.

The source contract is in [client.lua](../../../lua/forge/client.lua), with context policy in [protocol_contract.json](../../../lua/forge/protocol_contract.json). Focused regressions cover [request ownership](../../../tests/forge/client_request_contract.lua), [callback isolation](../../../tests/forge/client_callback_isolation.lua), and [receive-credit ordering](../../../tests/forge/forge_client.lua).
