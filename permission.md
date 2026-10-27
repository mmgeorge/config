# Harness permissions: agent handoff

The requested change separates filesystem access from tool approval. The winbar
must describe both independently instead of replacing Read/Write with YOLO.
This document records the request and investigation, not implemented behavior.
No permission-model implementation changes were made during this investigation.

## Requested user-facing model

Filesystem access has two independent dimensions: read scope and write scope.
Read scope is workspace or full. Write scope is off, workspace, or full.
Tool approval is a separate selection: `ask`, `deny`, or `allow`.

The winbar shows the access abbreviation first and the approval mode immediately
after it, for example `rxw ask` or `rxwx allow`. Other winbar information can follow.
Do not substitute `YOLO` for the access abbreviation.

Remove the existing Write- and YOLO-specific coloring. Display the access
abbreviation and approval mode in white for every combination, with no color
change when either selection changes.

### Access abbreviations

The suffix `x` means full filesystem scope for the preceding operation. It is
not a Unix executable permission.

| Display | Read scope | Write scope |
| --- | --- | --- |
| `r` | Workspace | Off |
| `rx` | Full | Off |
| `rw` | Workspace | Workspace |
| `rxw` | Full | Workspace |
| `rwx` | Workspace | Full |
| `rxwx` | Full | Full |

Read and write scopes must not be inferred from approval mode. In particular,
`allow` must not silently enable writing or widen either filesystem scope.

### Tool approval

The user requested `ask` as the default approval mode. A permission rule can
already approve or deny a request independently of whether a popup is needed.

| Mode | Requested behavior |
| --- | --- |
| `ask` | Apply permission rules and ask for requests not already approved or denied. |
| `deny` | Deny requests not already approved rather than opening an approval popup. |
| `allow` | Allow tool requests without ordinary approval prompts, corresponding to YOLO. |

Whether `allow` overrides explicit saved deny rules was raised as a clarification.
The conservative provisional answer is to honor those denies. Its response source
was an automatic question timeout, not an explicit user answer.

## Requested commands

Except for `/yolo`, the access commands change only filesystem access and preserve
the current tool approval mode.

| Command | Access transition | Approval transition |
| --- | --- | --- |
| `/read all` | Toggle read scope between workspace and full, preserving write state. | Unchanged |
| `/write all` | Toggle enabled writing between workspace and full, preserving read scope. | Unchanged |
| `/write` | Toggle writing between workspace-enabled and off, preserving read scope. | Unchanged |
| `/yolo` | Set full read and full write access. | Set `allow` |

Concrete transitions establish the distinction between changing scope and turning
writing off:

| Starting display | Command | Result |
| --- | --- | --- |
| `r ask` | `/read all` | `rx ask` |
| `rx ask` | `/read all` | `r ask` |
| `rw deny` | `/read all` | `rxw deny` |
| `rxwx allow` | `/read all` | `rwx allow` |
| `rw ask` | `/write all` | `rwx ask` |
| `rwx ask` | `/write all` | `rw ask` |
| `rxw deny` | `/write all` | `rxwx deny` |
| `rxwx allow` | `/write` | `rx allow` |
| `rxw ask` | `/write` | `rx ask` |
| `rw ask` | `/write` | `r ask` |
| `rwx ask` | `/write` | `r ask` |
| `r ask` | `/write` | `rw ask` |
| `rx deny` | `/write` | `rxw deny` |
| `r deny` | `/yolo` | `rxwx allow` |

For `/write all` while writing is off, the provisional interpretation is to enable
full writes immediately: `r` becomes `rwx`, and `rx` becomes `rxwx`. This also came
from an automatic question timeout, not explicit user confirmation.

The user did not specify a new command for selecting `ask`, `deny`, or `allow`,
the future meaning of bare `/read`, or whether existing `/mode` forms remain.
Resolve those caller-facing details without presenting unimplemented aliases as
available commands.

## Implementation boundary

The user explicitly requested that the code reflect this model, not merely change
labels. Persisted state, authorization, provider configuration, and presentation
must agree on the selected scopes and approval mode.

Rust owns authoritative session state, task transitions, permission evaluation,
and provider policy configuration. Lua captures commands and displays confirmed
state. Access and approval changes must retain the existing ownership and
interruption boundaries rather than create an editor-only permission state.

The requested `/yolo` change updates access and approval together. It does not
request disabling sandboxing or changing platform-specific sandbox settings.
Distinguish reported file authorization from actual OS confinement, particularly
for shell commands and providers without equivalent sandbox capabilities.

### Starting conditions observed in source

- `PermissionMode` currently has `Read`, `Write`, and `Yolo` variants. Approval
  handling and task defaults refer to that type.
- `AccessPolicy` already stores separate `read_access` and `write_access` values.
  Both use `DirectoryScope`, whose variants are `Workspace` and `Full`. The schema
  currently cannot represent write access being off.
- `AccessPolicy::default()` currently selects full reads and workspace writes.
  Preserving those scope defaults would display `rxw`, not `rw`. The user requested
  default `ask` approval but did not explicitly change the initial scope defaults.
- Lua's `M.set_mode()` currently accepts `read`, `write`, or `yolo` and submits a
  permission action through the task transition coordinator.
- Permission evaluation, directory authorization, provider adapters, task
  restoration, settings, and approval UI all consume the existing model. Updating
  the winbar alone does not implement the request.

### Source entry points

Paths are relative to the repository root. Reinspect current contents before
editing because this checkout has concurrent work.

| Area | Source |
| --- | --- |
| Session types and approval enum | `nvim/rust/forge/crates/forge-harness/src/session/mod.rs` |
| Filesystem scope model | `nvim/rust/forge/crates/forge-harness/src/session/access.rs` |
| Rule evaluation | `nvim/rust/forge/crates/forge-harness/src/permissions/matcher.rs` |
| Directory authorization | `nvim/rust/forge/crates/forge-harness/src/permissions/directory.rs` |
| Approval coordination | `nvim/rust/forge/crates/forge-harness/src/backend/approval.rs` |
| Task state | `nvim/rust/forge/crates/forge-harness/src/task.rs` |
| Durable transition handling | `nvim/rust/forge/crates/forge-harness/src/broker/mod.rs` |
| Portable settings | `nvim/rust/forge/crates/forge-harness/src/settings/settings.rs` |
| Codex policy configuration | `nvim/rust/forge/crates/forge-harness/src/backend/codex/security.rs` |
| Copilot adapter | `nvim/rust/forge/crates/forge-harness/src/backend/copilot/mod.rs` |
| ACP adapters | `nvim/rust/forge/crates/forge-harness/src/backend/acp/` |
| Commands and winbar | `nvim/lua/forge/views/harness/controller.lua` |
| Settings picker | `nvim/lua/forge/views/harness/settings.lua` |
| Command help | `nvim/lua/forge/shared/command_specs.lua` |
| User documentation | `nvim/docs/forge/commands.md` |
| Architecture documentation | `nvim/contributing/forge/architecture/harness.md` |

## Acceptance evidence needed

Exercise all six access states independently of all three approval modes.
Cover the command transitions above, including repeated toggles and `/yolo`
from write-off and deny states.

Authorization coverage must distinguish allowed, denied, and unmatched rules.
Include explicit saved denies under `allow` using the provisional conservative
interpretation. Verify that approval selection cannot restore disabled writing
or silently widen filesystem scope.

Verify session reopen, task pause/resume, workflow changes, settings inheritance,
and child execution without losing either dimension. Check provider requests as
well as UI state, and surface unsupported provider restrictions explicitly.

Manually observe the actual winbar and commands in an isolated Terminal MCP editor,
including a change while work is active. Do not treat a display-only update as
evidence that enforcement changed.
