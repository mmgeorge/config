# 1. Overview

Separate Harness filesystem access from tool-approval handling so commands, durable state,
authorization, and presentation describe the same selections. Rust owns independent read scope,
write scope, and approval mode, while Lua submits changes through the existing task-transition
coordinator and displays confirmed state. The transcript winbar shows white access and approval
text separated by dots and prefixes red `YOLO` only for full access with Allow:
`YOLO · rxwx · allow`. Remove special Write, Yolo, and planning-task coloring from timeline
headings and lifecycle/status text, preserving unrelated content styling.

The current `PermissionMode::{Read, Write, Yolo}` couples rule evaluation, writable workflow
defaults, provider-native approval settings, and presentation. `AccessPolicy` already separates
workspace/full reads and writes, but cannot disable writes. This change replaces that coupled
model, not just its labels.

## Confirmed scope and decisions

The conversation supersedes the provisional choices in repository-root `permission.md`.

- New interactive sessions default to full reads, workspace writes, and `ask`: `rxw ask`.
- Read scope is `Workspace` or `Full`. Write scope is `Off`, `Workspace`, or `Full`.
- `/read full` toggles read scope and preserves writes and approval.
- `/write full` toggles workspace/full writes and preserves reads and approval. From write-off,
  it enables full writes.
- `/write` and Shift-Tab toggle writing between off and workspace, preserving reads and approval.
  Disabling full writes does not retain a hidden full-write state for the next enable.
- `/ask`, `/deny`, and `/allow` change only Harness-owned tool-approval handling.
- `/yolo` atomically selects full reads, full writes, and `allow`. It does not disable sandboxing.
- Remove bare `/read`, `/mode`, and the `/read all` and `/write all` forms. Do not retain aliases.
- Preserve explicitly selected access across Plan, Execute, Goal, pause/resume, and continuation.
  Starting execution must not silently restore writes.
- `ask` retains existing once-only and permanent grant choices. `deny` automatically rejects
  requests that otherwise require a permission popup. `allow` automatically approves those
  requests without persisting a rule or a directory grant.
- Saved deny rules, protected targets, malformed-command rejection, and write-off remain
  authoritative in every approval mode.
- An out-of-scope file request can receive a once-only or saved grant under `ask`, an automatic
  rejection under `deny`, or an automatic once-only approval under `allow`. Those outcomes do
  not silently change the selected scopes.
- All three approval selections use the same provider-native callback-routing configuration.
  Harness makes the ask/deny/allow decision. Access changes still update supported native
  filesystem controls.
- The winbar uses `YOLO · rxwx · allow` only for that exact combination. `YOLO` appears first
  in the existing shared neon red, `require("theme.palette").red` (`#ef3573`), with dot separators
  between segments. Access and approval remain white. Other combinations display
  `<access> · <approval>` without YOLO or a leading separator.
- Remove Write-specific orange, Yolo-specific purple, and planning-task purple from timeline
  headings and lifecycle/status text, including planning, revision, and plan-review status.
  These labels use existing white captures regardless of access, approval, or task kind.
  Preserve unrelated tool, diff, diagnostic, and spinner colors. The red YOLO winbar indicator
  remains a separate explicit exception. This latest scope supersedes the earlier instruction
  to preserve planning/review accent colors.

## Verified starting points

| Owner | Current behavior and required boundary |
| --- | --- |
| `session/mod.rs`, `session/access.rs` | `PermissionMode` has Read/Write/Yolo. Both access fields use `DirectoryScope`. Defaults are full reads and workspace writes. |
| `permissions/matcher.rs` | Yolo bypasses rules before explicit denies are considered. Read converts allowed writes into Ask. |
| `permissions/directory.rs` | Canonical and supplied paths participate in denial checks. Out-of-scope requests ask, and specific saved grants can authorize them. |
| `backend/approval.rs` | `PermissionCoordinator` owns popup admission, saved choices, directory grants, deadlines, and callback replies. Read-mode writes currently require a mode-switch interaction. |
| `broker/mod.rs`, `task.rs` | Permission changes use durable interruption and resumption. Tasks retain `permission` and `last_write_permission`, and workflow entry can select a writable mode. |
| `backend/codex/security.rs` | Read/Write/Yolo select native untrusted/on-request/never independently of the access sandbox fields. |
| `backend/acp/backend.rs` | Yolo changes Claude native ask rules, ACP native modes, and session-reuse decisions. |
| `controller.lua`, `status_hint.lua` | Winbar and live animated status choose permission-dependent highlight groups. |
| `buffer/projection.rs`, `buffer/status.rs` | Rust emits permission-dependent exchange headings and purple planning/review status captures. Both need neutral white lifecycle styling. |
| `storage/mod.rs` | Exact-version session and preference envelopes already hide incompatible stored state. |

Paths in the table are relative to
`nvim/rust/forge/crates/forge-harness/src/` for Rust and
`nvim/lua/forge/views/harness/` for Lua.

Repository instructions prohibit private-format migration and generated-provider-file edits.
The existing worktree also contains unrelated changes, including check configuration. Reinspect
affected files before implementation and preserve that work.

# 2. Usage

Commands operate in the Harness composer, not as shell commands. Each displayed result follows
successful Rust confirmation. The example starts from the new-session default.

```text
Initial display       rxw · ask
/write                rx · ask
/read full            r · ask
/write full           rwx · ask
/allow                rwx · allow
/read full            YOLO · rxwx · allow
/deny                 rxwx · deny
/write full           rxw · deny
/write                rx · deny
/yolo                 YOLO · rxwx · allow
```

Access and approval text remain white. Only the optional leading YOLO label uses the existing
shared red (`palette.red`), with dots between segments. Selecting Write or
Allow must not apply orange or purple permission styling to Working or exchange headings.
Planning, revision, and plan-review lifecycle text also use white rather than the existing
purple task accent. A pending or failed transition must not present an unconfirmed policy
as applied.

## Access transitions

Every cell preserves approval. The independent toggles intentionally permit `rwx`, meaning
workspace reads and full writes.

| Initial access | `/read full` | `/write` or Shift-Tab | `/write full` |
| --- | --- | --- | --- |
| `r` | `rx` | `rw` | `rwx` |
| `rx` | `r` | `rxw` | `rxwx` |
| `rw` | `rxw` | `r` | `rwx` |
| `rxw` | `rw` | `rx` | `rxwx` |
| `rwx` | `rxwx` | `r` | `rw` |
| `rxwx` | `rwx` | `rx` | `rxw` |

`/ask`, `/deny`, and `/allow` preserve all six access states. `/yolo` maps every state to
`rxwx allow` in one durable operation.

## Approval outcomes

Rule evaluation and access evaluation produce a decision before popup handling runs.
Existing best-match rule precedence remains unchanged.

| Evaluated request | `ask` | `deny` | `allow` |
| --- | --- | --- | --- |
| Already authorized | Approve | Approve | Approve |
| Matched saved deny or protected target | Reject | Reject | Reject |
| Reported write with writes off | Reject | Reject | Reject |
| Unmatched or explicitly asks | Existing popup | Reject automatically | Approve once automatically |
| Out-of-scope file, writes enabled if writing | Existing grant popup | Reject automatically | Approve once automatically |

Automatic outcomes never create permanent grants. A permanent grant is an explicit popup
selection persisted through the current rule or directory-setting owner.

# 3. Diagrams

`HarnessSession` owns the selected `AccessPolicy` and `ApprovalMode`. `TaskRecord` retains the
task's confirmed selections for recovery, and `BackendRequest` captures the policy for one
provider execution. `DirectoryPolicy` evaluates reported file access and
`PermissionCoordinator` resolves tool approval without owning editor state.

## Object model and ownership

The new `WriteScope` adds write-off without making an invalid read-off state representable.
Existing structs stay in their feature owners. No generic permission service layer is added.

```text
Contracts                                 Concrete

<none>                                    *enum ApprovalMode
                                            [nvim/rust/forge/crates/forge-harness/src/session/mod.rs]
                                            Ask
                                            Deny
                                            Allow

                                          ~enum PermissionMode
                                            [nvim/rust/forge/crates/forge-harness/src/session/mod.rs]

                                          *enum WriteScope
                                            [nvim/rust/forge/crates/forge-harness/src/session/access.rs]
                                            Off
                                            Workspace
                                            Full

                                          *struct AccessPolicy
                                            [nvim/rust/forge/crates/forge-harness/src/session/access.rs]
                                            - read_access: DirectoryScope
                                            - write_access: WriteScope
                                            - readable_directory: String[]
                                            - writable_directory: String[]
                                            - sandbox: Boolean
                                            - windows_sandbox: WindowsSandbox
                                            + validate(): AccessPolicy
                                            + writable_roots(workspace): String[]
                                            + abbreviation(): String

                                          *struct HarnessSession
                                            [nvim/rust/forge/crates/forge-harness/src/session/mod.rs]
                                            - access: AccessPolicy
                                            - approval_mode: ApprovalMode
                                            - default_approval: ApprovalMode
                                            - plan_approval: ApprovalMode?

                                          *struct TaskRecord
                                            [nvim/rust/forge/crates/forge-harness/src/task.rs]
                                            - access: AccessPolicy
                                            - approval_mode: ApprovalMode
                                            + new(session_id, title, kind, access, approval_mode, now)

                                          *enum PermissionChange
                                            [nvim/rust/forge/crates/forge-harness/src/task.rs]
                                            ToggleReadScope
                                            ToggleWriteEnabled
                                            ToggleWriteScope
                                            SetApproval
                                              + mode: ApprovalMode
                                            EnableYolo
                                            + apply(access, approval_mode)

                                          *struct BackendRequest
                                            [nvim/rust/forge/crates/forge-harness/src/backend/backend.rs]
                                            - access: AccessPolicy
                                            - approval_mode: ApprovalMode

                                          *struct DirectoryPolicy
                                            [nvim/rust/forge/crates/forge-harness/src/permissions/directory.rs]
                                            - access: AccessPolicy
                                            + evaluate(compiled, request): PermissionEvaluation

                                          *struct PermissionCoordinator
                                            [nvim/rust/forge/crates/forge-harness/src/backend/approval.rs]
                                            - store: @RwLock<PermissionStore>
                                            + authorize(approval_mode, request, address, sink):
                                              ApprovalResolution
```

`default_approval` and optional `plan_approval` replace the existing workflow approval
preferences, not filesystem permissions. The default is Ask. Planning can keep the current
approval selection or apply an explicitly configured approval preference. Execute and Goal
use their explicit default approval setting without changing access.

## Code flow: policy transition

Lua sends an intent rather than calculating toggles from a potentially stale snapshot. Rust
resolves that intent against authoritative state and applies `/yolo` as one combined change.
The locations below name logical subsystems to keep the flow readable.

```text
Capture command       Admit transition        Settle active work
Harness composer ─intent▶ Task coordinator ─interrupt▶ Provider execution

Apply policy          Persist confirmation       Display confirmed policy
HarnessBroker ─state▶ Session/task storage ─snapshot▶ Transcript winbar
```

A failed validation or provider reconfiguration retains the last confirmed policy, reports the
operation failure, and leaves work in an explicit stopped or recoverable state.

## Code flow: callback authorization

Rule matching does not know approval mode. The coordinator applies ask/deny/allow only after
canonical path checks, protected-target checks, write-off enforcement, and rule decisions.

```text
Receive callback       Evaluate targets          Resolve ordinary approval
Provider adapter ─request▶ Rules + DirectoryPolicy ─decision▶ PermissionCoordinator

Resolve ordinary approval         Reply to provider
PermissionCoordinator ─once-only result▶ Native callback
```

Ask branches to the existing approval UI and persistence path before replying. Deny and Allow
return the corresponding once-only result without creating a pending popup. Explicit plan
acceptance, question answers, authentication, and destructive user confirmations remain separate
interactions, not automatic tool approvals.

## Code flow: lifecycle presentation

Both Rust-owned decorations and Lua's live status overlay must stop selecting Write/Yolo
permission colors and purple planning/review task accents. Diagnostic and content styling
remain independent.

```text
Project exchange heading      Publish canonical frame       Render permission-neutral heading
Rust buffer projector ─decorations▶ Transcript document ─frame▶ Existing white capture

Animate current status       Preserve independent colors
Lua status_hint.render ─spinner-only update▶ Status text + spinner
```

# 4. Tasks

1. **Represent access and approval independently in session-owned state.**
   Replace the coupled mode at its source and remove write-restoration semantics. These types
   establish the contract consumed by every subsequent task.

   file nvim/rust/forge/crates/forge-harness/src/session/mod.rs
   - Split filesystem state from approval state.
     - Remove enum `PermissionMode` and its `label` and `is_write_default` methods.
     - Add enum `ApprovalMode` with Ask, Deny, Allow, and default Ask.
     - Add method `ApprovalMode::label` for lowercase user-facing approval labels.
     - Modify struct `HarnessSession` to replace `execution_mode`, `default_write_permission`,
       and `plan_permission` with `approval_mode`, `default_approval`, and `plan_approval`.
     - Modify struct `HarnessPreference` to use the same approval preference names.
     - Remove fn `default_write_permission`.

   file nvim/rust/forge/crates/forge-harness/src/session/access.rs
   - Encapsulate valid write states and access formatting.
     - Add enum `WriteScope` with Off, Workspace, and Full.
     - Modify field `AccessPolicy::write_access` to use `WriteScope`.
     - Modify method `AccessPolicy::writable_roots` to return no writable roots for Off.
     - Add method `AccessPolicy::abbreviation` for exactly the six confirmed strings.
     - Modify method `AccessPolicy::validate` without weakening canonical directory validation.
     - Preserve config `AccessPolicy::default`: full reads, workspace writes, existing sandbox
       and Windows selections.

   file nvim/rust/forge/crates/forge-harness/src/task.rs
   - Encapsulate confirmed task policy for recovery.
     - Add enum `PermissionChange` for read-scope, write-enable, write-scope, approval-selection,
       and yolo intents.
     - Add method `PermissionChange::apply` as the single pure implementation of the confirmed
       transition table. Apply it to candidate access/approval values before validation and
       persistence, not directly to live session state.
     - Modify struct `TaskRecord` and fn `TaskRecord::new` to retain access and approval.
     - Remove fields `permission` and `last_write_permission`.
     - Preserve task identity, generation fences, interruption, and terminal-state rules.

   file nvim/rust/forge/crates/forge-harness/src/exchange/exchange.rs
   - Expose approval metadata without inferring filesystem access.
     - Modify the exchange's existing mode metadata to carry `ApprovalMode` under an
       approval-specific name, or remove it where presentation was its sole consumer.
       Do not retain historical color selection as a reason to preserve coupled metadata.

2. **Resolve tool approvals through the shared coordinator after access evaluation.**
   Preserve current best-match rules, shell parsing, saved grants, skill grants, and protected
   targets. Automatic popup handling must not bypass explicit rejection or enable disabled writes.

   file nvim/rust/forge/crates/forge-harness/src/permissions/matcher.rs
   - Split rule evaluation from popup selection.
     - Modify methods `CompiledPermissionDocument::evaluate` and `evaluate_command` to remove
       approval-mode arguments and Read/Yolo special cases.
     - Preserve command normalization, source ranges, compound-command aggregation, unknown
       file targets, and explicit Deny precedence across request targets.
     - Modify directly affected rule-evaluation tests.

   file nvim/rust/forge/crates/forge-harness/src/permissions/directory.rs
   - Centralize reported-file policy before approval handling.
     - Modify method `DirectoryPolicy::evaluate` to evaluate scopes independently of approval.
     - Deny both known-path writes and UnknownFile Write requests when writes are Off.
     - Preserve supplied/canonical denial checks, symlink handling, specific grants, additional
       directories, and the existing out-of-scope Ask result.
     - Preserve methods `grant`, `save`, and `effective_access` unless changed types require
       direct adjustments. Neither saved directories nor specific allow rules override Off.
     - Modify directory tests for write-off, scope exceptions, and path identity.

   file nvim/rust/forge/crates/forge-harness/src/backend/approval.rs
   - Route ordinary tool approval through one final mode decision.
     - Modify methods `PermissionCoordinator::authorize` and `authorize_callback` to accept
       `ApprovalMode` and resolve Ask results as popup, DenyOnce, or AllowOnce.
     - Apply the mode decision after protected-target, write-off, explicit-deny, and selected
       skill checks, including callbacks previously marked forced.
     - Remove field `ApprovalRequestView::required_mode` and the Read-to-Write popup branch.
       A write-off rejection explains that `/write` enables writes, without enabling it.
     - Preserve popup choice identity, deadline handling, cancellation, source-address ownership,
       once-only replies, and persistence-before-approval for permanent choices.
     - Preserve separate explicit proposal reviews unless the caller identifies them as ordinary
       tool-permission decisions.
     - Modify approval tests for all three modes, forced callbacks, and no-popup auto outcomes.

   file nvim/rust/forge/crates/forge-harness/src/control_tools/repository.rs
   - Reuse mode-independent rule evaluation.
     - Modify struct `RepositoryToolScope`, method `open`, and method `authorize` to remove the
       coupled mode argument while preserving their existing read-only authorization boundary.

   file nvim/rust/forge/crates/forge-harness/src/control_tools/lsp.rs
   - Reuse mode-independent read authorization.
     - Modify existing matcher call sites and affected permission fixtures.

   file nvim/rust/forge/crates/forge-harness/src/control_tools/executor.rs
   - Reuse the shared rule contract in control-tool scope construction.
     - Modify affected matcher calls and test setup without granting tools access from approval.

   file nvim/rust/forge/crates/forge-harness/src/checks/tools.rs
   - Route check-file writes through the same write-off boundary.
     - Modify the `checks_configure` branch and its authorization call to remove the Read-to-Write
       promotion. Keep check-editing enablement and child-exclusion requirements authoritative.
     - Apply Ask/Deny/Allow to the existing tool-approval review without changing check-editing
       feature enablement. Reject rather than display its approval popup in Deny.

3. **Persist policy changes through task and settings ownership.**
   Resolve toggle intents in Rust and retain the existing interrupt/finalize/apply/confirm/resume
   sequence. Access changes and approval changes must survive retry and recovery independently.

   file nvim/rust/forge/crates/forge-harness/src/broker/mod.rs
   - Centralize policy intents in durable transitions.
     - Modify methods `transition_settings`, `select_execution_mode`, and associated transition
       dispatch. Replace `select_execution_mode` with approval-specific selection and add access
       intent handling for read-full, write-toggle, write-full, and atomic yolo.
     - Decode commands into `PermissionChange` and invoke its pure transition logic exactly once
       against authoritative state. Reuse the existing validated configuration/application path
       for native access changes rather than creating a second policy persistence path.
     - Modify method `submit_prompt_inner` to recognize only the confirmed command vocabulary.
       Route direct broker commands through the same transitions as Lua commands.
     - Modify session creation, `configure_session`, task admission/resumption, plan acceptance,
       goal entry, execution entry, and session snapshots to carry the independent fields.
     - Remove workflow calls that restore writable permission or reset selected access.
     - Update the existing task's stored access and approval after every confirmed policy change.
       A task resume restores its confirmed policy without consulting obsolete write history.
     - Preserve durable operation IDs, uncertain-acknowledgement recovery, revision checks,
       checkpoint finalization, pending user decisions, and existing active-child admission rules.
     - Publish the access abbreviation with confirmed session state for Lua consumers.

   file nvim/rust/forge/crates/forge-harness/src/broker/fork.rs
   - Preserve source access for native forks.
     - Modify forked-session policy initialization to copy explicit access and independent approval
       rather than resetting to the legacy Read mode.

   file nvim/rust/forge/crates/forge-harness/src/broker/new_session.rs
   - Reuse the independent new-session defaults.
     - Modify affected session construction and preference application.

   file nvim/rust/forge/crates/forge-harness/src/settings/settings.rs
   - Expose approval defaults independently of filesystem preferences.
     - Modify struct `PortableSettings` and methods `inherit`, `apply`, and `write_layer` for
       `default_approval` and `plan_approval`.
     - Remove the Write-or-Yolo-only validation.
     - Preserve global/workspace/session inheritance, directory-grant transactions, operation
       receipts, and desired/applied revision separation.

   file nvim/rust/forge/crates/forge-harness/src/storage/mod.rs
   - Version changed durable payloads without migration.
     - Modify constants `SESSION_FORMAT_VERSION` and `PREFERENCE_FORMAT_VERSION`.
     - Modify current-format session, preference, and task fixtures for the independent fields.
     - Use exact-current-format handling for affected stored setting payloads. Report incompatible
       preferences rather than silently treating a decode failure as an empty layer.
     - Preserve unrelated independently decodable settings and resources. Hide incompatible
       sessions without rewriting or deleting their records.

   file nvim/rust/forge/crates/forge-harness/src/protocol/method.rs
   - Expose one current policy command contract.
     - Modify the legacy session-mode routing to use explicit access and approval operations.
       Remove obsolete mode aliases rather than adding private-protocol compatibility.

   file nvim/rust/forge/crates/forge-harness/src/plan/mod.rs
   - Expose approval choices without writable-mode semantics.
     - Modify struct `PlanAcceptance`, its constructor and selected-value method, and the current
       option label/description functions to use Ask/Deny/Allow.
     - Preserve exact plan/digest acceptance. Acceptance never enables writes or widens access.

4. **Carry independent policy through provider and child execution.**
   Provider adapters receive the same Harness policy, but native callback setup must not vary
   with Ask/Deny/Allow. Provider limitations remain visible rather than being mistaken for OS
   confinement or full callback coverage.

   file nvim/rust/forge/crates/forge-harness/src/backend/backend.rs
   - Expose execution policy through the shared request contract.
     - Modify structs `BackendRequest` and `BackendCapabilities` to replace execution-mode fields
       with approval-specific state and capability descriptions.
     - Modify mock-backend behavior that currently infers writability from `is_write_default`.
       Gate writes on access, not approval.

   file nvim/rust/forge/crates/forge-harness/src/backend/catalog.rs
   - Reuse policy fields during discovery.
     - Modify struct `BackendCatalogRequest` to carry independent approval state.

   file nvim/rust/forge/crates/forge-harness/src/backend/codex/security.rs
   - Split native callback setup from access projection.
     - Modify struct `CodexSecurity`, constructor, `approval_policy`, `apply_thread`, and
       `apply_turn` so approval-mode changes produce identical native approval configuration.
     - Use one supported callback-routing setup for Harness-owned decisions, not native
       never/YOLO for `/allow` or `/yolo`.
     - Project write-off through supported native read-only controls, not workspace-write with
       an empty extra-roots list, which can still authorize workspace writes.
     - Preserve sandbox enablement, mounted-root handling, Windows backend, and unrelated config.
       Do not claim native read confinement that the installed Codex contract does not provide.

   file nvim/rust/forge/crates/forge-harness/src/backend/codex/request_execution.rs
   - Route native decisions through the shared coordinator.
     - Modify request state and method `authorize_provider_request` to carry `ApprovalMode`.

   file nvim/rust/forge/crates/forge-harness/src/backend/codex/mod.rs
   - Reuse independent policy for native lifecycle operations.
     - Modify `secure_thread`, `apply_security`, descriptors, and affected request construction.
       Cover start, resume, fork, and turn setup with the same callback/access separation.

   file nvim/rust/forge/crates/forge-harness/src/backend/copilot/mod.rs
   - Route SDK callbacks through Harness-owned approval handling.
     - Modify permission-handler state, descriptor approval metadata, callback evaluation, and
       affected request construction. Keep one native callback registration for all modes.
     - Preserve protected-target checks and once-only native decisions.

   file nvim/rust/forge/crates/forge-harness/src/backend/acp/backend.rs
   - Keep native permission routing invariant across approval selections.
     - Modify Claude session parameters, native-mode selection, session `native_ask` reuse logic,
       capability metadata, and affected request construction.
     - Keep Claude native ask rules installed and stop selecting bypassPermissions/yolo for
       Harness Allow. Preserve separately advertised plan/architect workflow selection.

   file nvim/rust/forge/crates/forge-harness/src/backend/acp/session.rs
   - Resolve every ordinary ACP permission callback in Harness.
     - Modify struct `ActiveTurn` and its callback path to carry independent approval state.
     - Remove the Yolo-derived forced-popup branch. Map automatic responses to native once-only
       options and surface missing native allow/reject options explicitly.

   file nvim/rust/forge/crates/forge-harness/src/backend/prompt.rs
   - Expose the actual execution contract in provider instructions.
     - Modify permission-context generation to name access and approval separately and explain
       write-off without suggesting an implicit mode change.

   file nvim/rust/forge/crates/forge-harness/src/agent/selection.rs
   - Retain child policy independently.
     - Modify struct `AgentSelection` to capture `approval_mode` alongside access.

   file nvim/rust/forge/crates/forge-harness/src/agent/profile.rs
   - Expose approval-only child overrides.
     - Modify field `AgentProfile::permission_mode` to `approval_mode` and validation to accept
       inherit/ask/deny/allow. Reject obsolete profile values explicitly, without migration.

   file nvim/rust/forge/crates/forge-harness/src/agent/backend.rs
   - Resolve child approval without widening root access.
     - Modify methods `resolve` and `request` to carry the independent state.
     - Replace fn `narrow_permission` with approval-only narrowing, ordered Deny, Ask, Allow
       from most restrictive to least. Preserve root access for each request and provider-specific
       effective sandbox resolution. A child never restores write access disabled by its root.

   file nvim/rust/forge/crates/forge-harness/src/service.rs
   - Reuse policy contracts for service-created sessions and capabilities.
     - Modify affected session/request constructors and capability validation. Background
       generation keeps its read-only intent through WriteScope::Off, not legacy Read approval.

   file nvim/rust/forge/crates/forge-harness/src/backend/text_generation.rs
   - Preserve background access explicitly.
     - Modify affected request construction to use independent approval and write-off.

   file nvim/rust/forge/crates/forge-harness/src/backend/codex/text_generation.rs
   - Reuse callback and access projection for background Codex requests.
     - Modify affected native security construction and fixtures.

   The enum replacement also requires mechanical request/fixture updates at existing consumers
   in `backend/mcp.rs`, `backend/retry.rs`, `backend/steering.rs`, `backend/submission.rs`,
   `backend/codex/json_rpc.rs`, `backend/codex/runtime.rs`, `backend/codex/resources.rs`,
   `backend/codex/turn_coordinator.rs`, `settings/import.rs`, and `settings/resources.rs`, all
   under `nvim/rust/forge/crates/forge-harness/src/`. These edits only propagate the new shared
   field/type and remove obsolete matcher arguments. They must not introduce new policy logic.

   Before finalizing native policy fields, verify the installed provider schema and existing
   adapter fixtures. Native tools that do not issue callbacks remain outside Harness's approval
   boundary. Codex read restrictions and Antigravity terminal callbacks already have gaps.
   Surface those limits through existing capabilities and documentation, and fail unsupported
   native policy application explicitly. Do not broaden access or disable isolation as fallback.

5. **Expose confirmed policy through composer commands and settings.**
   Lua sends Rust-owned intents so concurrent windows and pending operations cannot calculate
   different toggle results. Existing shared picker behavior remains the interaction contract.

   file nvim/lua/forge/views/harness/controller.lua
   - Route commands to authoritative policy transitions.
     - Modify `M.submit` for `/read full`, `/write`, `/write full`, `/ask`, `/deny`, `/allow`,
       and `/yolo`.
     - Remove `M.set_mode`, `M.select_mode`, and the `/mode` parser.
     - Replace `M.toggle_mode` with the write-toggle action used by Shift-Tab.
     - Modify `M.present_approval` to remove obsolete required-mode switching.
     - Modify `M.task_transition`, pending state, event handling, and configuration plumbing
       only where the policy contract changes.
     - Reject removed commands and invalid arguments with actionable errors instead of sending
       them to the model as ordinary prompts.

   file nvim/lua/forge/views/harness/access.lua
   - Expose write-off in the existing access model.
     - Modify annotation `ForgeDirectoryPolicy` and `M.policy` for Off/Workspace/Full writes.
     - Modify directory-editor descriptions to distinguish retained grants from disabled writes.

   file nvim/lua/forge/views/harness/settings.lua
   - Reuse shared fields for independent selections.
     - Modify read/write scope controls, adding Off to write choices.
     - Modify planning and execution approval controls to Keep/Ask/Deny/Allow where applicable.
     - Replace the old Write-or-Yolo execution default with default Ask and approval-only choices.
     - Preserve sandbox controls, directory lists, inherited setting scope, and applied-state
       confirmation behavior.

   file nvim/lua/forge/shared/command_specs.lua
   - Expose the current write-toggle shortcut.
     - Modify the former `toggle_mode` spec, descriptions, and consumers to identify write
       enablement rather than approval.

   file nvim/lua/forge/views/harness/completion/command_source.lua
   - Expose only supported composer forms.
     - Modify command entries for the confirmed vocabulary.
     - Remove `/mode` value completion and bare `/read`.
     - Add full-scope completion without restoring all aliases.

   file nvim/lua/forge/views/harness/init.lua
   - Preserve independent state during provider changes.
     - Modify provider-switch policy capture/restoration to carry access and approval rather
       than restoring a non-Read legacy mode through `controller.set_mode`.
     - Preserve an explicitly resumed destination session's own stored selections.

   file nvim/lua/forge/views/harness/subagent_profiles.lua
   - Expose the new approval-only profile field.
     - Modify profile picker/editing values and labels to inherit/ask/deny/allow.

6. **Remove permission and planning accents from lifecycle presentation.**
   Keep the winbar factual and make timeline headings and lifecycle/status labels white across
   ordinary work, planning, revision, and review. Preserve unrelated content highlights in live
   and restored views.

   file nvim/lua/forge/views/harness/controller.lua
   - Compose separately styled winbar segments from confirmed state.
     - Modify local fn `status_text` to display Rust's access abbreviation followed by approval,
       separated by ` · `.
     - Prefix red YOLO only when read Full, write Full, and approval Allow all hold, producing
       `YOLO · rxwx · allow`. Keep access and approval white and omit the leading label and its
       separator for every other combination.
     - Preserve model, effort, task, queue, and existing operation-status information.

   file nvim/lua/forge/infra/highlights.lua
   - Reuse the existing white permission capture.
     - Add highlight group `ForgeHarnessYoloIndicator` using the existing `palette.red`
       (`#ef3573`) for the winbar YOLO segment only. Do not introduce a new red constant or apply
       this group to timeline text.
     - Remove fn `M.harness_mode` after replacing its permission/task-dependent consumers.
     - Remove obsolete `ForgeHarnessWrite`, `ForgeHarnessYolo`, and `ForgeHarnessPlan`
       definitions after replacing their lifecycle consumers with existing white captures,
       such as `ForgeTimelineStatus` and `ForgeHarnessRead`.
     - Preserve spinner/tool/diff/diagnostic groups and every other unrelated highlight.
       Do not add a new color abstraction.

   file nvim/rust/forge/crates/forge-harness/src/buffer/projection.rs
   - Remove permission-based and planning-task exchange-heading colors.
     - Modify the PermissionMode-derived exchange-summary capture and the preceding
       PlanDraft/PlanRevision `ForgeHarnessPlan` branch to use existing white captures.
     - Preserve the existing white Thought/Thinking fallback styling.
     - Preserve heading-marker structure, action targets, and unrelated lifecycle behavior.
     - Modify existing Write/Yolo/Plan decoration assertions to require white lifecycle text.

   file nvim/rust/forge/crates/forge-harness/src/buffer/status.rs
   - Preserve plan-review interaction without the purple status accent.
     - Modify fn `bind_primary_action` to use the existing white lifecycle capture instead
       of `ForgeHarnessPlan`, preserving the PlanReview action target and metadata.
     - Keep status text, timers, check activity, and animation semantics unchanged.

   file nvim/lua/forge/views/harness/status_hint.lua
   - Remove permission colors from the live animation overlay.
     - Modify `M.render` to stop deriving status text capture from session approval.
     - Preserve `animate` cadence, spinner colors, waiting signs, notices, and existing
       diagnostic severity colors. Use white text for planning/review lifecycle labels too.

7. **Prove enforcement, recovery, and rendering through focused regressions.**
   Extend existing fixtures rather than adding another test framework. Use the six-access by
   three-approval matrix where the combination changes behavior, plus focused failure cases.

   file nvim/rust/forge/crates/forge-harness/tests/permission_document.rs
   - Expose rule and coordinator outcomes independently.
     - Modify permission integration tests and add explicit-deny-under-Allow regressions.

   file nvim/rust/forge/crates/forge-harness/src/permissions/shell_tests.rs
   - Reuse approval-independent shell evaluation.
     - Modify existing evaluator calls while retaining malformed/unsupported shell regressions.

   file nvim/tests/forge/harness_access.lua
   - Expose all six access states in existing Lua fixtures.
     - Modify access annotations/fixtures and add retained-directory behavior while writes are Off.

   file nvim/tests/forge/harness_access_host.lua
   - Verify durable policy transitions against the real host.
     - Modify the active-goal scenario to cover access-only, approval-only, atomic yolo, pause,
       restart, and session reopen without task replacement or write restoration.

   file nvim/tests/forge/harness_mode_restart.lua
   - Verify current-policy restart semantics.
     - Modify the legacy mode scenario for independent policy intents and failed transitions.

   file nvim/tests/forge/harness_tasks_host.lua
   - Verify workflow transitions retain explicit access.
     - Modify Plan/Execute/Goal and resume assertions to reject automatic write enablement.

   file nvim/tests/forge/harness_settings.lua
   - Verify independent settings controls.
     - Modify picker assertions for approval values, write-off, and existing scope inheritance.

   file nvim/tests/forge/harness_completion.lua
   - Verify command discovery matches parsing.
     - Modify command and value completion assertions and reject removed aliases.

   file nvim/tests/forge/harness_composer_keymaps.lua
   - Verify Shift-Tab changes only write enablement.
     - Modify shortcut assertions for normal and insert mode, including write-full to Off.

   file nvim/tests/forge/harness_status_hint.lua
   - Verify animated text no longer uses Write/Yolo/planning colors.
     - Modify permission/task-dependent overlay assertions while preserving spinner behavior.

   file nvim/tests/forge/harness_status_hint_async.lua
   - Verify asynchronous status updates cannot restore Write/Yolo/planning colors.
     - Modify affected permission/task captures and retain stale-update coverage.

   file nvim/tests/forge/timeline_status.lua
   - Verify white lifecycle text across permission and task kinds.
     - Modify assertions directly affected by removing Write/Yolo/Plan captures, retaining
       unrelated content, diagnostic, and spinner styling.

   file nvim/tests/forge/subagent_profiles.lua
   - Verify child profile approval values and invalid legacy values.
     - Modify profile schema and editing fixtures.

   Existing Rust unit modules in session/access, matcher, directory, approval, broker, storage,
   settings, Codex security, ACP session/backend, and agent backend own their focused behavioral
   tests. Existing provider fixtures, including `backend/copilot/turn_test.rs`,
   `backend/copilot/mcp/test.rs`, `backend/acp/submission_test.rs`, and
   `broker/agent_test.rs`, must propagate the current request schema. Live targets
   `tests/codex_cli.rs`, `tests/codex_real.rs`, and `tests/copilot_real.rs` must compile against
   the new schema without being run automatically.

8. **Align documentation and source guidance with the independent policy contract.**
   Remove contradictory operational guidance so future changes do not restore the coupled mode
   model. Do not hand-edit generated provider instructions.

   file nvim/docs/forge/commands.md
   - Expose the confirmed commands, defaults, grant behavior, winbar predicate, and removal of
     special Write/Yolo/planning lifecycle coloring. Remove Read-mode write-switch instructions
     and `/mode`.

   file nvim/contributing/forge/architecture/harness.md
   - Centralize the independent state, evaluation order, transitions, persistence, task recovery,
     child restrictions, and neutral permission/planning lifecycle styling.

   file nvim/contributing/forge/architecture/providers.md
   - Expose invariant native callback setup, access projection, and actual provider capability
     limits without promising universal filesystem confinement.

   file .rulesync/rules/nvim.md
   - Modify permission guidance that currently mandates fixed Read/Write/Full/YOLO modes.

   file .rulesync/rules/forge.md
   - Modify winbar, Shift-Tab, permission, and native-policy guidance for the confirmed model.
     Preserve unrelated checks, build, rendering, and ownership rules.

   Run `rulesync generate --dry-run` before authorized local regeneration. Inspect generated
   output only after source changes. This implementation plan does not authorize broad global
   Rulesync changes or unrelated documentation corrections.

## Dependencies

Task 1 precedes Tasks 2 and 3. Task 4 depends on Tasks 1 and 2, and integrates with Task 3's
confirmed transition state. Task 5 depends on Tasks 1 and 3. Task 6 uses Task 5's current UI
contract. Task 7 is developed alongside the owning tasks and its final verification depends on
Tasks 2 through 6. Task 8 records the completed contracts and accompanies final verification.
These boundaries guide review and do not authorize parallel agents.

# 5. Modularity, testability, and plan validation

`AccessPolicy` owns filesystem selections and formatting. `CompiledPermissionDocument` owns rule
matching. `DirectoryPolicy` adds canonical file-access evaluation. `PermissionCoordinator` alone
decides whether an ordinary tool approval becomes a popup or an automatic once-only reply.
The broker alone commits policy transitions, and Lua never owns an alternative permission state.

The proposed enum replacement propagates through existing requests, settings, child selections,
and fixtures. Mechanical propagation must not add policy branches in provider adapters. Keep
native capability differences in their existing capability contract, not in inconsistent
implementations of Ask/Deny/Allow.

## Architectural constraints

Implement the independent model at its owners, not as display aliases around legacy
Read/Write/Yolo behavior.

- `PermissionChange` owns the command transition table once. Lua captures intent, the broker
  applies it to authoritative candidate state, and tests call that same pure logic. Do not
  duplicate toggle calculations in Lua, direct broker commands, settings callbacks, or providers.
- `ApprovalMode` contains only popup-handling semantics. Remove old writable-mode predicates
  and restoration fields rather than retaining them behind renamed labels or conversion tables.
- `CompiledPermissionDocument` evaluates rules independently of approval. `DirectoryPolicy`
  adds reported-file access decisions. `PermissionCoordinator` applies popup handling once.
  Do not scatter Ask/Deny/Allow branches across matchers, adapters, or UI callbacks.
- The broker applies both dimensions through the existing durable operation and settings
  application lifecycle. `/yolo` is one typed intent and one confirmation, not sequential
  full-read, full-write, and approval requests with observable intermediate policies.
- Provider adapters translate native contracts and invoke the shared coordinator. They do not
  maintain a second rule engine, approval preference store, or provider-specific meaning of
  Ask/Deny/Allow.
- Rust publishes the access abbreviation. Lua composes independently styled winbar segments
  from confirmed fields. The YOLO indicator is a derived conjunction, not a fourth permission
  mode or a persisted flag that can disagree with access and approval.
- Reuse existing feature modules, transition admission, settings revisions, directory grants,
  shared picker controls, and highlight palette. Add no generic permission framework, extra
  service layer, forwarding wrappers, legacy shims, or speculative configuration.
- Keep failures explicit and state adoption ordered. A rejected candidate must not mutate
  durable or displayed policy, and a failed continuation must not undo a policy already
  confirmed by the existing application lifecycle.

Review the implementation against these ownership boundaries before final verification.
Reject a change that passes display tests while retaining legacy coupling, duplicated transition
logic, provider-native bypass selection, or an editor-only permission state.

All six access states, the three approval values, and the exact YOLO predicate appear in the
usage contract, owning types, transition tasks, and tests. The three flow diagrams separately
cover mutation/recovery, authorization, and presentation. UML paths follow declarations and the
Contracts/Concrete anchors remain fixed. Flow lines remain within 100 characters.

The implementation must not add file-size, document-size, occurrence-count, or history-length
limits. Existing unrelated limits are outside this task.

## Error and compatibility boundaries

- Preserve the last confirmed UI state when policy validation, persistence, provider application,
  or resumption fails. Report the actual failure through existing task-operation and notification
  paths.
- Do not broaden filesystem access when a provider cannot enforce requested native restrictions.
  Preserve existing reported-operation versus OS-confinement distinctions.
- Provider-private operations without callbacks cannot be auto-approved or auto-denied by
  Harness. Verify and document this boundary, including shell and external MCP execution.
- Write-off rejects reported writes even with saved allow rules, directory grants, Allow mode,
  or child profiles. Native filesystem enforcement depends on supported sandbox capabilities.
- Reject incompatible private formats using exact-current-version handling. Do not migrate,
  partially decode, silently reset, or delete old sessions.
- Keep `permissions.json` as the validated Rust-owned rule document and retain its valid current
  schema. This change does not move policy into provider-specific persistence.
- Existing build documentation and generated instructions disagree about profile selection.
  Resolve the artifact through the builder and apply the governing release-build requirements.
  Do not silently switch profiles or repair unrelated build documentation in this task.

## Completion criteria

The new model is complete only when no execution path infers write access from approval, no
workflow silently restores writes, automatic tool decisions remain in Harness, and the running
editor displays the confirmed state without Write/Yolo permission coloring or purple
planning/review lifecycle accents. Unrelated content styling must remain intact. A winbar-only change, a
passing unit test with stale provider configuration, or an unrestarted old host is insufficient.

# 6. Test plan

## Unit tests

Use existing test modules with mocked native requests, event sinks, and persistence failures.

1. Test each command intent from all six access states under all three approval values. Assert
   exact resulting policy, preserved dimensions, and atomic yolo behavior through
   `PermissionChange::apply`. Use transport tests separately to prove all entry points reach
   that owner rather than testing duplicate implementations of the table.
2. Test rule Allow/Deny/Ask and unmatched requests separately from approval handling. Include
   explicit saved denies under Allow, compound shell commands with a denied occurrence, invalid
   shell syntax, and supplied/canonical path denial.
3. Test write-off for known writes, unknown-path writes, additional writable directories,
   specific saved edit allows, protected targets, and child requests. All reject without popup.
4. Test out-of-scope read/write requests with enabled writes. Ask retains once/permanent grant
   choices, Deny returns DenyOnce, and Allow returns AllowOnce without modifying persisted rules
   or directories.
5. Test forced native callbacks with all modes. Ask remains answerable and Deny/Allow return
   native once-only results. Do not let a forced flag bypass hard rejection.
6. Test identical native approval setup across modes and different native access projection
   across write Off/Workspace/Full. Assert `/yolo` preserves sandbox and Windows settings.
7. Test current-format session/task round trips, old-version invisibility, inheritance,
   preference errors, and preservation of unrelated independent preferences.
8. Test child approval narrowing and unchanged inherited access. Child Allow cannot override root
   Deny, a root deny rule, or root write-off.
9. Test all 18 access/approval display combinations. Only Full/Full/Allow displays
   `YOLO · rxwx · allow`, with the leading YOLO segment in the existing shared red and access
   and approval in white. Assert dot placement and no leading separator when YOLO is absent.
10. Test Rust summary/status decorations and Lua animated overlays for removal of Write/Yolo
    coloring and purple planning accents. Assert that PlanDraft/PlanRevision headings,
    Working/Planning labels, and plan-review status use white captures, while action targets,
    spinner identity/color, diagnostic/content styling, status timers, and restored-history
    behavior remain intact.

## Integration tests

Use real Harness modules, an isolated workspace/data root, and mock provider transports.

1. Start at `rxw ask`, execute the Usage sequence, and compare the confirmed session/task
   snapshot, persisted state, callback outcome, and rendered indicators after every command.
2. Change access and approval while a Goal or Execute task is active. Verify interruption and
   checkpoint finalization precede policy application, task identity remains stable, and resumed
   provider requests receive the confirmed policy.
3. Inject configuration failure and uncertain acknowledgement. Confirm retry deduplication,
   retained prior state, actionable notifications, and no duplicate continuation.
4. Pause, restart the host, reopen the session, resume the task, switch workflows, and create a
   child. Verify both dimensions survive and no path restores disabled writes.
5. Under Ask, save an exact rule and a canonical directory grant, reopen, and exercise both.
   Switch to Deny and Allow without rewriting those grants. Confirm saved denies still win.
6. Exercise Codex thread/turn/resume/fork security fixtures, Copilot callbacks, and ACP
   session-new/load and permission replies. Approval-mode changes must not select native
   bypass modes or remove callback-routing rules.
7. Verify check-file editing cannot bypass write-off, while separate check-editing enablement
   remains required.
8. Exercise command completion and parsing together. Removed `/read`, `/mode`, and all forms
   produce errors and are absent from suggestions.

## Validation execution

No implementation tests or builds run during this planning-only request.

During implementation, use the existing configured checks when executing inside Harness:
select the affected Lua checks, permission tests, `forge-rust-format-fix`,
`forge-rust-clippy`, `forge-rust-tests`, and the required release executable build. Inspect
check definitions before selecting them because the current `forge-build` configuration and
documentation do not uniformly use the governing release artifact. Resolve command and artifact
through `require("forge.builder").build_command()` and `.binary_path()`, retain one target
directory and release settings, and obtain authorized check configuration if coverage is missing.
Do not bypass recorded checks with ad hoc terminal validation.

Rust tests use `--release --locked` and the same target directory as the release executable.
Run the targeted permission, access, transition, provider-security, and projection cases first.
Broaden package coverage after those pass because the request types and enums are shared.
Build the actual Forge executable with `--bin forge --locked` before host integration or manual
verification. Allow 300 seconds for compilation/builds and at most 120 seconds for other
commands unless separately approved. Record command, timeout, exit status, relevant errors,
and rebuilt artifact path.

Run the selected existing Lua fixtures in fresh isolated Neovim hosts after the artifact build.
Run the existing LuaLS/formatting checks for changed Lua and `git diff --check` for changed
files. Do not install new testing tooling or invoke paid/live-provider tests automatically.

Finally inspect an isolated rendered terminal editor at 160x48 and 100x30 using the terminal
debugging skill and its available terminal-driving tools. Verify the Usage sequence, active-work
policy changes, the exact `YOLO · rxwx · allow` predicate and segment colors, removal of
Write/Yolo colors from ordinary status text, white planning/revision/review lifecycle text,
unchanged spinner/tool/diff/diagnostic colors, and session reopen. If terminal
tools or a provider are unavailable, record the specific blocked evidence rather than claiming
manual or native-provider verification.
