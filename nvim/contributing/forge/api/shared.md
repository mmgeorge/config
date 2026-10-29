# Shared methods

These methods own configuration shared across conversations. Optional envelope context validates an existing session and selects its workspace/settings layer where stated. Omission never selects the active conversation. The [usage ledger](../../../../md/rename-usage-ledger.json) records each caller.

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## Shared result and document schemas

All methods in this chapter use runtime readiness and emit no request-progress stream. None starts a model turn or performs a remote network mutation. Settings writes with explicit session context can reconfigure that session's provider and emit `task_operation` and session-configuration events. Without context, they persist shared settings without creating a conversation. Keep the captured session/view owner when adopting any asynchronous result.

**Profile snapshots** contain `path` (the local JSON file), `revision` (an opaque string used for conditional save), `document`, and optional `error`. A missing file supplies the built-in defaults and revision `missing`. A malformed or unreadable file returns an error-bearing snapshot with the last validated document or defaults. `unreadable` is not a safe revision for overwriting an unobserved file. Profile saves validate the document, take the file lock, verify the exact revision, and atomically replace the file. Lock contention, invalid content, stale revision, and I/O failures reject the save. Reread after an unknown outcome.

A profile document has required `default_profile: string` and `profiles: object`, keyed by profile name. The default must name an enabled entry. Names contain 1–64 ASCII letters, digits, `_`, or `-`, and cannot be `root`, `Main`, or `primary`. Each profile accepts:

| Field | Type and default | Domain |
| --- | --- | --- |
| `enabled` | Boolean, `true` | Enables selection of this profile. |
| `description` | String, empty | UI description. |
| `instructions` | String, empty | Persistent child instructions. |
| `provider` | String, `inherit` | `inherit`, `codex`, `copilot`, `claude`, or `antigravity`. |
| `model` | String, `inherit` | Nonblank model choice or inheritance value. |
| `reasoning_effort` | String, `inherit` | Nonblank effort choice or inheritance value. |
| `service_tier` | String, `inherit` | `inherit`, `default`, `fast`, or `ultrafast`. |
| `approval_mode` | String, `inherit` | `inherit`, `ask`, `deny`, or `allow`. |

Unknown profile fields fail decoding. These are reusable child definitions, not a list of running agents. The [profile store](../../../rust/forge/crates/forge-harness/src/agent/profile.rs) owns validation, file locking, and fallback reads.

**Permission document results** contain `path` and `source`, both strings. `source` is canonical pretty JSON with a trailing LF. Its top-level `permission` object is required, and `$schema` is an optional string. `permission` maps categories to pattern/decision objects. Categories are `bash`, `edit`, `elevate`, `mcp`, `read`, `tool`, and `webfetch`. Patterns must be nonblank. Decisions are `allow`, `ask`, or `deny`. Unknown top-level fields and unsupported categories fail validation. Saving compiles and atomically writes the new policy before replacing the usable in-memory document. Invalid JSON, invalid rules, and I/O failures leave the prior usable policy. Saves have no expected-revision field, so callers must reread rather than assume concurrent edits are merged. The [permission store](../../../rust/forge/crates/forge-harness/src/permissions/store.rs) owns this lifecycle.

**Model-pin results** are arrays of model-identifier strings sorted by identifier. Backend and model parameters must be nonblank. A pin can reference a temporarily unavailable model and does not require querying a provider catalog. Membership writes are idempotent SQLite transactions that preserve other models and backends. A repeated unpin returns the unchanged list. Storage failures affect only the originating request. The [model-pin store](../../../rust/forge/crates/forge-harness/src/storage/model_pin.rs) owns concurrency and persistence.

**Settings snapshots** contain `revision` (unsigned integer), `desired` ([PortableSettings](native-schemas.md#portablesettings)), `catalog` with `mcp` and `skill` arrays, `layers` with `global`, `workspace`, and `session` PortableSettings objects, and `application` status. Optional settings fields are absent when not selected. Each PortableSettings object retains `skill` and `mcp` maps even when empty. Omitted workspace uses the initialized workspace; explicit session context always selects the session's workspace. Workspace resolution and storage errors reject the read rather than retargeting it.

Catalog MCP entries contain string `id`, `name`, and `source`, optional `scope`, an array of provider `targets`, and `transport` (`stdio`, `http`, or `sse`). Catalog skill entries contain string `id`, `name`, `description`, and `source`, provider `targets`, and boolean `user_invocable`. Catalogs omit executable definitions, secrets, and skill prompt bodies.

`application` contains unsigned `desired_revision` and `generation`, string `state`, and optional `operation_id`, `applied_revision`, and `effective` PortableSettings. It starts as `unverified`, records application work separately from persistence, and retains the last effective settings after a failed application. A successful provider application records `applied` and advances the generation. Failure states follow the corresponding command receipt.

**Import previews** contain `source` (canonical path), `fingerprint` (opaque string), `revision` (settings revision), and `candidate` (array). Each candidate contains `id`, `name`, `kind` (`skill` or `mcp`), `source`, optional/null `transport` and `blocked`, and a `conflict` array of strings. A blocked candidate cannot be applied. Preview supports `kind: "mcp"` for JSON/TOML server configuration and `kind: "skills"` for native skill directories. Import scope is `global` or `workspace`, defaulting to `workspace`; `session` scope is rejected. A relative source path resolves against the selected workspace. It reads local files without importing credentials or launching resources. The [import implementation](../../../rust/forge/crates/forge-harness/src/settings/import.rs) owns fingerprint, provenance, and conflict validation. Apply rereads the source, rejects changed fingerprints/revisions and invalid selections, and saves selected resources disabled.

Examples below come from [the isolated shared-API fixture](../../../../nvim/tests/forge/shared_api_examples.lua), which exercises every method without creating an interactive session. `/fixture` replaces its temporary root, and correlation IDs are renumbered for readability. Opaque IDs and fingerprints are retained from the corresponding responses and reused by later requests.

## `agent.profile.get`

Read the reusable agent-profile document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** Empty object. Optional envelope session context validates that the session exists, but does not scope profile storage.
- **Result and effects:** Returns profiles and their revision.
- **Boundary:** Profiles are shared configuration rather than one session's running agents.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/controller.lua:1441](../../../../nvim/lua/forge/views/harness/controller.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 4,
  "method": "agent.profile.get",
  "params": {}
}
```

```json
{
  "id": 4,
  "result": {
    "error": null,
    "path": "/fixture/nvim.0/mc53HA/0/data/forge\\subagents.json",
    "revision": "missing",
    "document": {
      "profiles": {
        "reviewer": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Review changes for defects",
          "reasoning_effort": "inherit",
          "instructions": "Review the assigned changes for correctness and regressions. Report findings with file references. Do not modify files.",
          "model": "inherit",
          "service_tier": "inherit"
        },
        "remote-explorer": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Research external code and documentation",
          "reasoning_effort": "inherit",
          "instructions": "Research the assigned external code or documentation. Prefer primary sources and cite them. Do not modify local files.",
          "model": "inherit",
          "service_tier": "inherit"
        },
        "code-explorer": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Explore the local codebase",
          "reasoning_effort": "inherit",
          "instructions": "Inspect the local codebase and answer the assigned question with source references. Do not modify files.",
          "model": "inherit",
          "service_tier": "inherit"
        },
        "worker": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Implement a scoped task",
          "reasoning_effort": "inherit",
          "instructions": "Complete the assigned task and report the result and verification.",
          "model": "inherit",
          "service_tier": "inherit"
        }
      },
      "default_profile": "worker"
    }
  }
}
```

<!-- /captured-example -->

## `agent.profile.save`

Save a reusable agent-profile document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `expected_revision`: required string from the previous snapshot. `document`: required profile document. The store validates the document and revision before saving.
- **Result and effects:** Returns the saved profile snapshot.
- **Boundary:** A stale revision must not overwrite concurrent profile changes.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/subagent_profiles.lua:83](../../../../nvim/lua/forge/views/harness/subagent_profiles.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 5,
  "method": "agent.profile.save",
  "params": {
    "expected_revision": "missing",
    "document": {
      "default_profile": "worker",
      "profiles": {
        "reviewer": {
          "enabled": true,
          "approval_mode": "inherit",
          "model": "inherit",
          "description": "Review changes for defects",
          "instructions": "Review the assigned changes for correctness and regressions. Report findings with file references. Do not modify files.",
          "reasoning_effort": "inherit",
          "provider": "inherit",
          "service_tier": "inherit"
        },
        "remote-explorer": {
          "enabled": true,
          "approval_mode": "inherit",
          "model": "inherit",
          "description": "Research external code and documentation",
          "instructions": "Research the assigned external code or documentation. Prefer primary sources and cite them. Do not modify local files.",
          "reasoning_effort": "inherit",
          "provider": "inherit",
          "service_tier": "inherit"
        },
        "code-explorer": {
          "enabled": true,
          "approval_mode": "inherit",
          "model": "inherit",
          "description": "Explore the local codebase",
          "instructions": "Inspect the local codebase and answer the assigned question with source references. Do not modify files.",
          "reasoning_effort": "inherit",
          "provider": "inherit",
          "service_tier": "inherit"
        },
        "worker": {
          "enabled": true,
          "approval_mode": "inherit",
          "model": "inherit",
          "description": "Implement a scoped task",
          "instructions": "Complete the assigned task and report the result and verification.",
          "reasoning_effort": "inherit",
          "provider": "inherit",
          "service_tier": "inherit"
        }
      }
    }
  }
}
```

```json
{
  "id": 5,
  "result": {
    "error": null,
    "path": "/fixture/nvim.0/mc53HA/0/data/forge\\subagents.json",
    "revision": "148fe9d7fe851ba896b28e11e329b8477c6da1119073baf980bb2c910a32e5e8",
    "document": {
      "profiles": {
        "reviewer": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Review changes for defects",
          "reasoning_effort": "inherit",
          "instructions": "Review the assigned changes for correctness and regressions. Report findings with file references. Do not modify files.",
          "model": "inherit",
          "service_tier": "inherit"
        },
        "remote-explorer": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Research external code and documentation",
          "reasoning_effort": "inherit",
          "instructions": "Research the assigned external code or documentation. Prefer primary sources and cite them. Do not modify local files.",
          "model": "inherit",
          "service_tier": "inherit"
        },
        "code-explorer": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Explore the local codebase",
          "reasoning_effort": "inherit",
          "instructions": "Inspect the local codebase and answer the assigned question with source references. Do not modify files.",
          "model": "inherit",
          "service_tier": "inherit"
        },
        "worker": {
          "enabled": true,
          "approval_mode": "inherit",
          "provider": "inherit",
          "description": "Implement a scoped task",
          "reasoning_effort": "inherit",
          "instructions": "Complete the assigned task and report the result and verification.",
          "model": "inherit",
          "service_tier": "inherit"
        }
      },
      "default_profile": "worker"
    }
  }
}
```

<!-- /captured-example -->

## `permissions.document.open`

Open the editable permission document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** Empty object. The runtime owns the permission-store path. The caller does not supply another path.
- **Result and effects:** Returns the permission document representation.
- **Boundary:** Opening the document does not save edits.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/permissions.lua:57](../../../../nvim/lua/forge/views/permissions.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 6,
  "method": "permissions.document.open",
  "params": {}
}
```

```json
{
  "id": 6,
  "result": {
    "source": "{\n  \"$schema\": \"https://github.com/dyoshikawa/rulesync/releases/latest/download/permissions-schema.json\",\n  \"permission\": {\n    \"edit\": {\n      \".\": \"allow\"\n    },\n    \"mcp\": {\n      \"*\": \"ask\"\n    },\n    \"read\": {\n      \"*\": \"allow\"\n    },\n    \"webfetch\": {\n      \"*\": \"allow\"\n    }\n  }\n}\n",
    "path": "/fixture/nvim.0/mc53HA/0/data/forge/permissions.json"
  }
}
```

<!-- /captured-example -->

## `permissions.document.save`

Validate and save the edited permission document.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `source`: required UTF-8 string containing the complete edited permission document. Returns `{ path, source }` from the saved store.
- **Result and effects:** Returns the save outcome/document state.
- **Boundary:** Invalid permission content must not silently replace the prior usable rules.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/permissions.lua:35](../../../../nvim/lua/forge/views/permissions.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 7,
  "method": "permissions.document.save",
  "params": {
    "source": "{\n  \"$schema\": \"https://github.com/dyoshikawa/rulesync/releases/latest/download/permissions-schema.json\",\n  \"permission\": {\n    \"edit\": {\n      \".\": \"allow\"\n    },\n    \"mcp\": {\n      \"*\": \"ask\"\n    },\n    \"read\": {\n      \"*\": \"allow\"\n    },\n    \"webfetch\": {\n      \"*\": \"allow\"\n    }\n  }\n}\n"
  }
}
```

```json
{
  "id": 7,
  "result": {
    "source": "{\n  \"$schema\": \"https://github.com/dyoshikawa/rulesync/releases/latest/download/permissions-schema.json\",\n  \"permission\": {\n    \"edit\": {\n      \".\": \"allow\"\n    },\n    \"mcp\": {\n      \"*\": \"ask\"\n    },\n    \"read\": {\n      \"*\": \"allow\"\n    },\n    \"webfetch\": {\n      \"*\": \"allow\"\n    }\n  }\n}\n",
    "path": "/fixture/nvim.0/mc53HA/0/data/forge/permissions.json"
  }
}
```

<!-- /captured-example -->

## `provider.model.pin.list`

List pinned model identifiers for a provider.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `backend`: required provider identifier string. Returns an array of pinned model identifier strings.
- **Result and effects:** Returns the provider's pinned model IDs.
- **Boundary:** This reads shared model-pin storage.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/model_pins.lua:31](../../../../nvim/lua/forge/views/harness/model_pins.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 8,
  "method": "provider.model.pin.list",
  "params": {
    "backend": "codex"
  }
}
```

```json
{
  "id": 8,
  "result": []
}
```

<!-- /captured-example -->

## `provider.model.pin.set`

Set whether a provider model is pinned.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `backend`: required provider identifier string. `model`: required model identifier string. `pinned`: required boolean. Returns the resulting array of pinned model identifiers.
- **Result and effects:** Returns the resulting pinned model IDs.
- **Boundary:** The change is persisted independently of a conversation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/model_pins.lua:47](../../../../nvim/lua/forge/views/harness/model_pins.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 9,
  "method": "provider.model.pin.set",
  "params": {
    "pinned": true,
    "model": "example-model",
    "backend": "codex"
  }
}
```

```json
{
  "id": 9,
  "result": [
    "example-model"
  ]
}
```

<!-- /captured-example -->

## `settings.get`

Read resolved settings and the available catalogs/layers.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `workspace`: optional path string, used only without explicit session context and defaulting to the initialized workspace. With a session, its workspace and session layer are used. Returns the settings snapshot plus `catalog`, `layers` (`global`, `workspace`, `session`), and `application` status.
- **Result and effects:** Returns effective settings, catalogs, layer settings, and application status.
- **Boundary:** Global, workspace, and session layers are distinct.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/provider_catalog.lua:133](../../../../nvim/lua/forge/views/harness/provider_catalog.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [request_independence_host.lua](../../../../nvim/tests/forge/request_independence_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 69,
  "method": "settings.get",
  "params": {}
}
```

```json
{
  "id": 69,
  "result": {
    "layers": {
      "session": {
        "answer_timeout": null,
        "access": null,
        "auto_compaction": null,
        "skill": {},
        "mcp": {},
        "subagent_tools": null,
        "lsp_tools": null,
        "session_tools": null,
        "status_thoughts": null,
        "check_editing": null,
        "check_timing": null,
        "automatic_checks": null,
        "plan_compact": null,
        "shell": null,
        "approval_timeout": null,
        "plan_auto_approve_revisions": null,
        "plan_approval": null,
        "default_approval": null
      },
      "workspace": {
        "answer_timeout": null,
        "access": null,
        "auto_compaction": null,
        "skill": {},
        "mcp": {},
        "subagent_tools": null,
        "lsp_tools": null,
        "session_tools": null,
        "status_thoughts": null,
        "check_editing": null,
        "check_timing": null,
        "automatic_checks": null,
        "plan_compact": null,
        "shell": null,
        "approval_timeout": null,
        "plan_auto_approve_revisions": null,
        "plan_approval": null,
        "default_approval": null
      },
      "global": {
        "answer_timeout": null,
        "access": null,
        "auto_compaction": null,
        "skill": {},
        "mcp": {},
        "subagent_tools": null,
        "lsp_tools": null,
        "session_tools": null,
        "status_thoughts": null,
        "check_editing": null,
        "check_timing": null,
        "automatic_checks": null,
        "plan_compact": null,
        "shell": null,
        "approval_timeout": null,
        "plan_auto_approve_revisions": null,
        "plan_approval": null,
        "default_approval": null
      }
    },
    "application": {
      "state": "unverified",
      "generation": 0,
      "applied_revision": null,
      "effective": null,
      "operation_id": null,
      "desired_revision": 0
    },
    "provider": null,
    "revision": 0,
    "catalog": {
      "mcp": [
        {
          "id": "rulesync:f2a04cad06bbe5ece8d0d84c3ad80c672c0e83228a10a200c6802d6b73918225",
          "name": "microsoft-learn",
          "scope": "global",
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "http"
        },
        {
          "id": "rulesync:06486c18c4444d96d98026c38b04067c6d909b4bfa4c8033d7c6911249426a3b",
          "name": "docs",
          "scope": "global",
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "stdio"
        },
        {
          "id": "rulesync:1302343119c9002f2fd90b7761d5e9c9935feb8c18469fafef543fca892fffdd",
          "name": "sem",
          "scope": "global",
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "stdio"
        },
        {
          "id": "rulesync:932c2c9804c769a2eb9d3ea59448be877dba6671446217e2da6533e289a26a60",
          "name": "google-dev-knowledge",
          "scope": "global",
          "targets": [
            "claudecode",
            "codexcli",
            "antigravity-cli",
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "http"
        },
        {
          "id": "rulesync:6657177c1874594eb8e36973fcfb954fe7ed1272c8f2edad5e0334c95a199bed",
          "name": "chrome-devtools",
          "scope": "global",
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "stdio"
        },
        {
          "id": "rulesync:7e8278b6b703eacd655ac6da9a144056a3ff28c1110d836d2c7bc71002e8ec5a",
          "name": "github-enterprise",
          "scope": "global",
          "targets": [
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "stdio"
        },
        {
          "id": "rulesync:e38da7d45b35c82d8d4259d4ee07a343babfe18240f250c52af7a581f673bd20",
          "name": "github",
          "scope": "global",
          "targets": [
            "claudecode",
            "codexcli",
            "antigravity-cli",
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "http"
        },
        {
          "id": "rulesync:2b895cd82186354573ec084c810e52f7b8ebceb4fb1335a130e5dc4731acf01a",
          "name": "openaiDeveloperDocs",
          "scope": "global",
          "targets": [
            "codexcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "http"
        },
        {
          "id": "rulesync:61d1970f2055277a3e4d5e9a64dd5a71abf4730cfb2736db27047c66d22a282d",
          "name": "play-mcp",
          "scope": "global",
          "targets": [
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "stdio"
        },
        {
          "id": "rulesync:67c7c74bb2ccbebe9042dd607309a5f6babc482ca2dc3123cebc4706126da501",
          "name": "terminal-mcp",
          "scope": "global",
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\mcp.json",
          "transport": "stdio"
        }
      ],
      "skill": [
        {
          "id": "rulesync:0396b426cd46171d5ae4ce0685d7c33b58fc0e253e5817b6f714a1f689d9ba30",
          "name": "academic-pseudocode",
          "user_invocable": true,
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\academic-pseudocode\\SKILL.md",
          "description": "Write and revise high-level, compact academic pseudocode. Use when authoring Algorithm blocks in implementation plans or when the user requests academic pseudocode. Do not use for native implementations or prose-only Change blocks."
        },
        {
          "id": "rulesync:78b289086d1a58391dc2228ef87a2bec6282dea8545d5fa495a0cfcaf9ebb343",
          "name": "authoring-skills",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\authoring-skills\\SKILL.md",
          "description": "Author and refactor Agent Skills (a SKILL.md plus bundled references, scripts, and assets) for reliable discovery and execution. Use when creating a new skill, restructuring an existing skill, defining frontmatter descriptions, organizing progressive-disclosure reference files, or evaluating skill quality."
        },
        {
          "id": "rulesync:b2b6ff5c27a5bd9ec654e1dbc966ce404ddde2638dabd32a2aac59f08a8534b9",
          "name": "fn-walk",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\fn-walk\\SKILL.md",
          "description": "Generates a detailed, section-by-section code walkthrough with code blocks and in-line invariant comments. Use when asked \"/fn-walk\", \"walk through this code\", \"detailed walkthrough\", \"step by step walkthrough\", or when a deep, section-by-section code breakdown is requested."
        },
        {
          "id": "rulesync:d91f2b37134193b63d934fba09b7f57a8a0169da9d3acbef176e587599a52548",
          "name": "fn-why",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\fn-why\\SKILL.md",
          "description": "Explains why a function, method, or subsystem exists, its controlling invariant, caller contract, and lifecycle flow. Use when asked \"/fn-why\", \"why does this function exist\", \"explain this function\", \"what does this function do\", or when walking through a function's caller contract and failure boundaries."
        },
        {
          "id": "rulesync:e7db0769918277a3dd9565eb93b4ef9fb1404db0ababb121221688c3f0eecdaf",
          "name": "harness-checks",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\harness-checks\\SKILL.md",
          "description": "Configure Forge Harness project checks, automatic triggers, typed command inputs, and repository-owned callbacks through check_definition_edit. Use when adding, editing, or repairing .forge.json checks or managed .forge/checks/ helpers. Do not use for merely running existing checks or changing task-specific check selection."
        },
        {
          "id": "rulesync:7ca0fd038896f63a235e9683d09e0d8ab0c4d2d3fb26ffbe793086053e50ac6e",
          "name": "harness-sessions",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\harness-sessions\\SKILL.md",
          "description": "Recall previous work and diagnose Forge Harness sessions, tool results, permissions, configuration, and logs with harness_config and harness_session tools. Use when searching prior conversations, explaining Harness behavior, or locating Harness source and settings. Do not use for ordinary application debugging unrelated to Harness."
        },
        {
          "id": "rulesync:3bcc024cce184ee6eadde98ce632beaf780cfbd233746f909b4beb7592ff84e0",
          "name": "maps-sdk",
          "user_invocable": true,
          "targets": [
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\maps-sdk\\SKILL.md",
          "description": "Builds and debugs ArcGIS Maps SDK for JavaScript applications and ArcGIS Play documents using map components, FeatureLayer, ParquetLayer, renderers, popups, layer views, and view lifecycle. Use when creating, inspecting, editing, running, or troubleshooting a JSAPI or Maps SDK app, including an ArcGIS Play document through play-mcp. Consults the local WebGIS SDK source before web documentation. Use the portal skill for Portal item discovery and management, and the parquet skill for Parquet publishing or dataset management."
        },
        {
          "id": "rulesync:89b31474bec26a7448d8987800e5020684a1d8a0bb7fad25dd8d4e2c4517f045",
          "name": "nushell",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\nushell\\SKILL.md",
          "description": "Author, inspect, validate, and debug Nushell scripts, modules, pipelines, commands, configuration, and nu-check syntax assertions. Use when working with Nushell code, converting shell scripts to Nushell, or validating script syntax."
        },
        {
          "id": "rulesync:c5a34c667debee661e03649652a9bbc0ce3c818f9da19f658bb700a2e2e60131",
          "name": "parquet",
          "user_invocable": true,
          "targets": [
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\parquet\\SKILL.md",
          "description": "Inspects, filters, subsets, publishes, updates, shares, and visualizes Parquet datasets with the portal CLI, including ArcGIS Parquet Feature Layer items, Azure Blob or ADLS storage, _spatial.json metadata, and ArcGIS Play apps. Use when a request names portal tools parquet, portal item parquet, an ArcGIS Parquet Feature Layer, _spatial.json, or ArcGIS Play delivery generated by the portal CLI from Azure or Portal Parquet content. Use the maps-sdk skill for hand-authored application code that consumes an existing Parquet item through ParquetLayer. Do not use for unrelated Portal CLI administration, PyArrow, Spark, DuckDB, or generic Azure file-transfer work."
        },
        {
          "id": "rulesync:a87c1f5381ac2ff962fad0a638143f4174fdd87b6f5e935c3e081e0469cee7c7",
          "name": "portal",
          "user_invocable": true,
          "targets": [
            "copilotcli"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\portal\\SKILL.md",
          "description": "Configures and operates the portal CLI for ArcGIS Online and Enterprise portals, including authentication, item discovery, generic resources, Feature Layers, Web Maps, exports, tracked jobs, cache settings, multiscale geometry, sharing, and ArcGIS Play previews. Use when a request names the portal CLI, portal config, portal item, portal job, Feature Layer items, Web Map items, or ArcGIS Portal item management. Use the parquet skill instead for portal tools parquet, portal item parquet, or Azure Parquet delivery. Use the maps-sdk skill for hand-authored application code that consumes Portal content."
        },
        {
          "id": "rulesync:23a2983808b0fdc5f5d3c6188e814a79f608fbf25afb4ba8539bb9a50a62eeee",
          "name": "rulesync",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\rulesync\\SKILL.md",
          "description": "Configure Rulesync target tools, feature selection, and output scope in rulesync.jsonc and rulesync.local.jsonc, and synchronize assistant rules, permissions, MCP servers, skills, and other assets from .rulesync/ sources. Use when editing Rulesync JSONC configuration, changing which providers or features are targeted, running generation or imports, or diagnosing missing, stale, or unexpectedly unchanged generated files."
        },
        {
          "id": "rulesync:f206729498807fa8db5c0e973a6ad9f611447390c76f68ed231a305116568a94",
          "name": "technical-writing",
          "user_invocable": true,
          "targets": [],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\technical-writing\\SKILL.md",
          "description": "Author and revise technical documents, architectural overviews, RFCs, reference docs, READMEs, API docstrings, rustdoc, and source code comments. Use when creating or editing standalone documentation files, docstrings, or in-code comments."
        },
        {
          "id": "rulesync:50f77bad4c48a2a301b3dff0c2c2d36c2feaf334148bcb8fc722fc9ba81de224",
          "name": "walkthrough",
          "user_invocable": true,
          "targets": [
            "*"
          ],
          "source": "D:\\config\\rulesync-global\\.rulesync\\skills\\walkthrough\\SKILL.md",
          "description": "Generate, regenerate, validate, or repair a Forge `.walkthrough.json` artifact directly from repository changes. Use when creating review walkthroughs, guided code review artifacts, or `.walkthrough.json` files after local modifications."
        }
      ]
    },
    "desired": {
      "answer_timeout": null,
      "access": null,
      "auto_compaction": null,
      "skill": {},
      "mcp": {},
      "subagent_tools": null,
      "lsp_tools": null,
      "session_tools": null,
      "status_thoughts": null,
      "check_editing": null,
      "check_timing": null,
      "automatic_checks": null,
      "plan_compact": null,
      "shell": null,
      "approval_timeout": null,
      "plan_auto_approve_revisions": null,
      "plan_approval": null,
      "default_approval": null
    }
  }
}
```

<!-- /captured-example -->

## `settings.import.apply`

Apply a reviewed settings import.

- **Context:** Shared service with explicit evaluation context.
- **Input:** All preview fields, plus `expected_revision`: required unsigned integer, `fingerprint`: required reviewed source fingerprint string, `selected`: required array of entry identifier strings, and optional `operation_id`. Re-reads the source and validates the fingerprint before persistence.
- **Result and effects:** Returns the import and application outcome.
- **Boundary:** Validate the captured source/revision before persistence.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/native_import.lua:94](../../../../nvim/lua/forge/views/harness/native_import.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_native_import_host.lua](../../../../nvim/tests/forge/harness_native_import_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 14,
  "method": "settings.import.apply",
  "params": {
    "kind": "mcp",
    "source": "/fixture/nvim.0\\o3tBDD\\1\\native.json",
    "action": "configure",
    "scope": "workspace",
    "selected": [
      "import:82151f6a5ad9948d7b3cb04babc95caa2c63e3ebefa5330917ef03556e6a8276"
    ],
    "fingerprint": "44ff8497afe2563da0a688e2aac59808445d9a2e69f68bea37761dc2f9af4263",
    "expected_revision": 0,
    "operation_id": "20356:47079410679100:2",
    "_command": {
      "id": "20356:47079410679100:2",
      "runtime_epoch": "622cec20-aea6-4b05-b70e-2f76278f5a58"
    }
  },
  "session_id": "8fe7fcf9-64c4-49a0-b80f-813a4cd60e87"
}
```

```json
{
  "result": {
    "state": "accepted",
    "revision": 1,
    "error": null,
    "action": "configure",
    "id": "20356:47079410679100:2"
  },
  "id": 14
}
```

<!-- /captured-example -->

## `settings.import.preview`

Preview importing native provider settings.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `kind`: required native import kind. `source`: required local source-path string. `scope`: setting scope, default `workspace`. `workspace`: optional evaluation path without a session. Returns a reviewed source fingerprint and selectable import entries without persisting them.
- **Result and effects:** Returns the import preview without applying it.
- **Boundary:** A preview is not approval or proof that a later import has committed.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `none`.

**Current caller:** [nvim/lua/forge/views/harness/native_import.lua:105](../../../../nvim/lua/forge/views/harness/native_import.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [harness_native_import_host.lua](../../../../nvim/tests/forge/harness_native_import_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 6,
  "method": "settings.import.preview",
  "params": {
    "scope": "workspace",
    "kind": "mcp",
    "source": "/fixture/nvim.0\\o3tBDD\\1/native.json"
  },
  "session_id": "8fe7fcf9-64c4-49a0-b80f-813a4cd60e87"
}
```

```json
{
  "result": {
    "revision": 0,
    "candidate": [
      {
        "blocked": null,
        "kind": "mcp",
        "source": "/fixture/nvim.0\\o3tBDD\\1\\native.json",
        "name": "portable",
        "conflict": [],
        "transport": "stdio",
        "id": "import:82151f6a5ad9948d7b3cb04babc95caa2c63e3ebefa5330917ef03556e6a8276"
      },
      {
        "blocked": "MCP options cannot transfer without changing behavior: disabled_tools",
        "kind": "mcp",
        "source": "/fixture/nvim.0\\o3tBDD\\1\\native.json",
        "name": "restricted",
        "conflict": [],
        "transport": null,
        "id": "import:2b6d780f153d42f363eed40b52299fe4164a6d9fdb85e8e6ff9787af48366111"
      }
    ],
    "source": "/fixture/nvim.0\\o3tBDD\\1\\native.json",
    "fingerprint": "2b55d472ce3d41ee875f86e6b9336c85dd6b184d5514c52338e200e7483495cb"
  },
  "id": 6
}
```

<!-- /captured-example -->

## `settings.mcp.set_enabled`

Change an MCP server's enabled setting.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `name`: required resource-name string. `enabled`: required boolean. `resource_id`: optional canonical identifier string, required to disambiguate duplicate names. `expected_revision`: optional unsigned integer, otherwise reads the current revision. `scope`: setting scope, default `global`. `workspace`: optional evaluation path without a session. `operation_id`: optional durable mutation identifier string. Exactly one canonical resource must match.
- **Result and effects:** Returns the settings mutation/application result.
- **Boundary:** This is settings persistence and is not equivalent to a tool invocation.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/provider_catalog.lua:175](../../../../nvim/lua/forge/views/harness/provider_catalog.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 14,
  "method": "settings.mcp.set_enabled",
  "params": {
    "enabled": true,
    "workspace": "/fixture/nvim.0\\mc53HA\\0/workspace",
    "operation_id": "example-mcp",
    "expected_revision": 2,
    "scope": "workspace",
    "name": "example",
    "resource_id": "import:51227d97b863aa9760c205524d112ff485d850b9e867a401519d53e065c606e0"
  }
}
```

```json
{
  "id": 14,
  "result": {
    "id": "example-mcp",
    "state": "saved",
    "revision": 3,
    "restart_required": false
  }
}
```

<!-- /captured-example -->

## `settings.skill.set_enabled`

Change a skill's enabled setting.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `name`: required resource-name string. `enabled`: required boolean. `resource_id`: optional canonical identifier string, required to disambiguate duplicate names. `expected_revision`: optional unsigned integer, otherwise reads the current revision. `scope`: setting scope, default `global`. `workspace`: optional evaluation path without a session. `operation_id`: optional durable mutation identifier string. Exactly one canonical resource must match.
- **Result and effects:** Returns the settings mutation/application result.
- **Boundary:** This is settings persistence and can affect more than one session.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/lua/forge/views/harness/provider_catalog.lua:167](../../../../nvim/lua/forge/views/harness/provider_catalog.lua).

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 17,
  "method": "settings.skill.set_enabled",
  "params": {
    "enabled": true,
    "workspace": "/fixture/nvim.0\\mc53HA\\0/workspace",
    "operation_id": "example-skill",
    "expected_revision": 4,
    "scope": "workspace",
    "name": "example-skill",
    "resource_id": "import:fc4ab5397162f688ac4baf3e94e33349dd1385907a875954fbb662fca25f606a"
  }
}
```

```json
{
  "id": 17,
  "result": {
    "id": "example-skill",
    "state": "saved",
    "revision": 5,
    "restart_required": false
  }
}
```

<!-- /captured-example -->

## `settings.update`

Apply an explicit settings mutation.

- **Context:** Shared service with explicit evaluation context.
- **Input:** `expected_revision`: required unsigned integer. `desired`: required [PortableSettings](native-schemas.md#portablesettings) object. `scope`: `global`, `workspace`, or `session`, default `global`. `workspace`: optional evaluation path without a session. `operation_id`: optional string, generated when omitted. Callers that need mutation recovery must retain their own ID. Session scope requires an explicit envelope session ID.
- **Result and effects:** Returns mutation/application information.
- **Boundary:** Persisted intent and runtime application can have separate outcomes.
- **Inspected source:** [`nvim/rust/forge/crates/forge-harness/src/service.rs`](../../../rust/forge/crates/forge-harness/src/service/shared.rs).

**Transport:** Optional explicit session ID. Readiness: `runtime`. Admission: `ordinary`. Recovery: `session_command`.

**Current caller:** [nvim/tests/forge/request_independence_host.lua:67](../../../../nvim/tests/forge/request_independence_host.lua). Live integration-control use: Verify same-revision concurrent settings mutations reject a conflicting write.

<!-- captured-example -->
### Verified example

Wire frames captured from [request_independence_host.lua](../../../../nvim/tests/forge/request_independence_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 70,
  "method": "settings.update",
  "params": {
    "scope": "global",
    "expected_revision": 0,
    "desired": {
      "plan_compact": true
    }
  }
}
```

```json
{
  "id": 70,
  "result": {
    "id": "424d6670-435a-46eb-902a-7c971b2012e8",
    "restart_required": false,
    "revision": 1,
    "state": "saved"
  }
}
```

<!-- /captured-example -->


## Shared mutation settlement

Without a session, a settings mutation returns `{ id, state = "saved", revision, restart_required = false }`. This records durable settings without creating a session or invoking its provider. With explicit session context, the mutation is saved first, then `session.command.apply` applies the captured settings revision. Its receipt can initially be accepted and later settle through `task_operation`. A later application failure does not imply that settings persistence failed. Reconcile both the receipt and the current settings snapshot.

These methods use the runtime readiness policy and normal admission. They emit no request-progress stream. Settings application with a session can emit session configuration and task-operation events. Profile, permission-document, and pin writes use their own store semantics rather than the session command receipt protocol. A transport failure after such a write requires rereading its current document or list.

The [shared-service regression](../../../rust/forge/crates/forge-harness/src/service/shared.rs) exercises these services before any session exists. [request_independence_host.lua](../../../tests/forge/request_independence_host.lua) submits two settings updates at the same revision and asserts exactly one commit. [permissions.lua](../../../tests/forge/permissions.lua) and [subagent_profiles.lua](../../../tests/forge/subagent_profiles.lua) cover document consumers.
