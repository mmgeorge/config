# Session command configuration

`session.command.apply` carries both typed task actions and configuration actions. All require an explicit envelope `session_id`. Callers should supply a nonempty `operation_id` so a lost response can be recovered. Typed task actions generate an ID when it is omitted. Reusing an operation ID with a different action fails. The command receipt and later `task_operation` event describe admission and settlement separately. Recover the receipt through `session.command.operation.get` before retrying an uncertain mutation.

Task actions use [SessionAction](native-schemas.md#sessionaction). The additional configuration actions below are decoded by [admit_task_intent](../../../rust/forge/crates/forge-harness/src/service.rs) and executed by [apply_configuration](../../../rust/forge/crates/forge-harness/src/broker/configuration.rs). They do not create a task.

| Action | Required fields | Optional fields | Effect |
| --- | --- | --- | --- |
| `permission` | `change`: [PermissionChange](native-schemas.md#permissionchange) | `resume`: boolean, default false | Applies the selected access or approval change to the session. Admission captures the resolved permission selection. |
| `configure` | `config`: configuration object | `resume`: boolean, default false | Validates and applies the requested session and provider settings. |
| `compact` | None | `resume`: boolean, default false | Requests native context compaction. Unsupported providers fail explicitly. |
| `undo` | `rollback.scope`: `conversation`, `rollback.exchange_id`: existing exchange ID, `rollback.latest_exchange_id`: captured latest exchange ID | None | Applies a conversation rollback only while the captured latest exchange remains current. |

The configuration object accepts the fields below. Omitted fields retain their current setting. Typed deserialization rejects null where the type does not allow it. String fields read through `as_str` and boolean fields read through `as_bool` ignore values of the wrong type unless the handler explicitly validates their presence. Callers should always send the documented type.

| Field | Type and validation |
| --- | --- |
| `settings_revision` | Unsigned integer matching the current settings revision. Loads the desired settings before applying them and checks the revision again before confirmation. |
| `access` | [AccessPolicy](native-schemas.md#accesspolicy), validated against the provider's supported sandbox. |
| `shell` | Supported shell selection. The executable must resolve. |
| `approval_mode`, `default_approval` | Approval-mode enum. |
| `plan_approval` | Optional approval-mode enum. Explicit null clears this override. |
| `approval_timeout`, `answer_timeout` | Validated input-timeout objects. |
| `auto_compaction` | [AutoCompaction](native-schemas.md#autocompaction). Enabling requires native compaction support. |
| `check_timing` | `after_queue` or `between_work`. |
| `automatic_checks` | Boolean. |
| `model`, `effort` | Nonempty strings. With `validate = true`, selection must appear in the provider's model catalog. |
| `context_window` | String or null. Null clears the selection. Catalog validation applies when requested. |
| `service_tier` | `default`, `fast`, or `ultrafast`. Provider and selected model must support the tier. |
| `validate` | Boolean, default false. Requests catalog validation of model, effort, and context-window selections. |
| `plan_executor_enabled` | Boolean. Enabling requires model-selection support. |
| `plan_executor_model`, `plan_executor_effort` | Nonempty strings validated against the provider catalog. |
| `plan_compact` | Boolean. Enabling requires native compaction support. |
| `plan_auto_approve_revisions` | Boolean. |
| `subagent_tools`, `lsp_tools`, `session_tools`, `status_thoughts` | Boolean feature selections. Tool-registry changes can clean up provider execution. |
| `session_policy` | Boolean. True chooses the session settings layer for directly persisted configuration. Auto-compaction changes also use that layer. Other direct portable settings use the global layer. |

Configuration can persist preferences and settings, query a provider's model catalog, reconcile provider resources, and clean up execution. It is not a single transaction across the settings database and an external provider. A failure after persistence requires receipt and current-state reconciliation. A successful admission response alone does not establish that application completed.

For a permission change, the caller captures the session and uses a new durable operation ID:

```lua
client.request({
  method = "session.command.apply",
  session_id = captured_session_id,
  params = {
    operation_id = operation_id,
    action = "permission",
    change = { kind = "toggle_write_enabled" },
  },
}, function(receipt, failure)
  if failure then return report_failure(failure) end
  retain_receipt(receipt)
end)
```

The caller must use the discriminator declared on [PermissionChange](native-schemas.md#permissionchange). The [permission integration fixture](../../../tests/forge/harness_access_host.lua) and [settings fixture](../../../tests/forge/harness_settings.lua) exercise these flows with explicit ownership.
