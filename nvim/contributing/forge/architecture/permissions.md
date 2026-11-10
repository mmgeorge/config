# Harness permissions

A model that can write outside its workspace, or bypass a saved denial by rephrasing a shell command, cannot be left to run unattended. Harness therefore evaluates every reported operation against saved rules and access scopes before the provider receives an answer. Saved denials, protected targets, and write-off always win, whichever approval mode is selected.

## Limits of these controls

These scopes authorize the operations a provider reports. They do not sandbox everything a provider, shell, or MCP server does, and only Codex adds operating-system isolation, through `CodexSecurity`. Provider-local hooks and command policies can also reject a tool that Harness allowed.

## Access and approval

`HarnessSession` keeps two independent settings. `AccessPolicy` holds the read scope (Workspace or Full), the write scope (Off, Workspace, or Full), and extra directory lists. `ApprovalMode` is Ask, Deny, or Allow. New interactive sessions use full reads, workspace writes, and Ask. Resumption restores the stored selections, Background generation disables writes, and forks copy the source's access.

Filesystem planning adds an invocation-only `effective_write_roots` restriction for its authoring directory, without changing saved user access preferences. Directory evaluation rejects reported writes outside that root even under Full or Allow, and write-off still rejects capture and ordinary file writes. Baselines and host snapshots remain outside the writable root; read access does not make them editable.

`PermissionChange` resolves the permission commands through the task transition coordinator. `/yolo` selects full reads and writes, Allow, and sandbox off together, and repeating it switches back to Ask and re-enables the sandbox where the provider supports it.

## Evaluation order

Each operation passes through these stages in order:

1. `CompiledPermissionDocument` evaluates saved rules, without seeing the approval mode.
2. `DirectoryPolicy` evaluates canonical paths, grants, and scope.
3. `PermissionCoordinator` resolves anything still undecided: a popup under Ask, a one-time rejection under Deny, or a one-time approval under Allow.

Write-off denies every write, including writes to unknown paths, regardless of rules or grants. Explicit denials and protected targets always win, and providers can never write the permission document itself. Automatic decisions never persist rules.

Before any of these stages, `ProviderPermissionContext` rejects a command containing a PowerShell file writer (`Set-Content`, `Add-Content`, or `Out-File`, found by `command::shell_file_writer` in any pipeline, sequence, or nested launcher) and tells the model to use its file-editing tool. A shell write reaches the approval layer only as an opaque command, so it can never be matched against write roots or path rules; a file-editing tool reports a concrete path that directory evaluation can authorize, including planning's authoring root. Saved allow rules and Allow mode do not lift this rejection.

`PermissionStore` loads one validated document from `stdpath("config")/forge/permissions.json`, edited through `:ForgePermissions`, and compiles its matchers once.

Built-in host tools default to Allow at the tool-approval layer. Explicit saved tool rules still apply, including Ask and Deny. Resource permissions, directory scopes, write-off, protected paths, and tool-specific admission remain independent. External provider and MCP tools continue to require approval when no saved rule matches.

## Reported paths

Adapters reconstruct the affected paths before the coordinator sees a request. Codex resolves item approvals from the matching file proposal, and ACP combines partial callbacks with its tool snapshot. Missing targets stay unresolved, and a malformed target list fails as a whole.

Path authorization resolves relative paths from the execution workspace, follows symlinks before later `..` components, and compares path components rather than string prefixes. Comparing components stops `/repo-other` from matching a grant for `/repo`. The approval popup can add a reported file's directory to the global readable or writable lists. A write grant uses the containing Git worktree root, and accepted grants persist before the provider receives approval.

## Shell command analysis

A rule that matches `git status` is worthless if `git status; rm -rf .` also passes. Harness parses shell commands with pinned PowerShell, Bash, Zsh, and Nushell grammars, without running them, and splits compound commands into separate targets, each with its own approval decision. Targets that are already allowed are skipped. Callback builtins such as `ForEach-Object`, `Where-Object`, and Nushell's `each` and `where` pass only if every nested command passes.

Analysis fails closed, with a distinct error for each cause:

| Cause | Error |
| --- | --- |
| Dynamic command names, code evaluation, definitions, or redirections | `shell_command_unsupported` |
| Invalid syntax | `shell_command_parse_error` |
| Exhausted analysis budget (8,192 nodes, 64 tree levels, 16 nested shells, or a 25 ms parse deadline) | `shell_command_analysis_failed` |

Failed analysis never opens approval or authorizes part of a command, even under YOLO. Codex command text uses POSIX `shlex` quoting on every platform. In `permission.bash`, `"*"` is the lowest-priority decision.

## Approval decisions

The approval picker stages one offered choice per independently identified target and submits them together when the last is confirmed. Checked request, target, and choice IDs bind that selection to the original `ApprovalScope` (item, turn, thread, or operation). Each choice contains its concrete decision, saved-rule effects, and directory grants. The coordinator validates those effects rather than inferring authority from a display label or a provider-specific choice string. Any denial rejects the whole operation, and rules persist atomically before the provider request is released.

`PermissionApprovalEvent::{Requested, Resolved, Cancelled}` is host-owned. Resolution retains each target's decision and source, committed rules/grants, and timestamp. Cancellation retains its own reason and never fabricates a user denial. Timeout is an explicit one-time decision. `DirectoryAccessGranted` describes a persisted grant under that same original scope; a late grant or resolution cannot authorize another pending request. Native adapter writers cannot publish any of these host events. Approval presentation carries an explicit execution kind: `Managed` or `NativeObserved`. A native child request inherits policy only through its trusted original association and a positively registered child owner. Resolving or dismissing it operates on that approval; it never invokes a managed `AgentTask` cancellation path.

`/config` sets the approval and answer timeouts, ten minutes each by default, and zero disables expiry. The waiting provider future owns the approval deadline, so closing the picker cannot pause it. Expiry returns a one-time denial recorded as `resolution_source: timeout`. An expired question set answers each open question with its recommended or first option. Plan acceptance never times out.

Adapters receive only a semantic `PermissionResponse` (`Allow`, `Deny`, or `Cancelled`) and the configured `ApprovalMode`, so they cannot depend on picker layout or Harness timing. Harness owns the presentation types in `forge-harness/src/provider/approval_view.rs`, including `ApprovalRequestView`, `ApprovalItem`, `ApprovalChoice`, `ApprovalCommand`, and `ApprovalAnswer`. It also owns the decision deadlines in `forge-harness/src/session/timeout.rs`, including `InputTimeout`, `InputDeadline`, and `ResolutionSource`. Each adapter maps the semantic response to its native wire answer. Codex, for example, sends accept, decline, or cancel.

## Check editing

`session.check.editing.set` controls the per-session `check_editing` permission, which is off by default. Every `check_definition_edit` call goes through the approval coordinator in Ask mode, regardless of saved allow rules, and write-off still rejects it. The `harness-checks` skill owns the configuration instructions. `check_definition_list` reports the live value as `check_editing`, and the interaction contract points the model there instead of stating the value. The value can change during a turn, after the contract was delivered, so the contract never embeds it. The contract's tool approval, access configuration, and purpose do reach a Codex model at every change: Codex replaces per-turn collaboration-mode instructions with its model catalog's preset, so the adapter injects the contract into thread history before `turn/start` whenever it differs from the last injection, as [Providers](providers.md#sessions) describes.
