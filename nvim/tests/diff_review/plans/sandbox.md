# Harness Sandbox and Permission Model

## Status

Design decision recorded on 2026-07-14. The Codex adapter now implements the four-mode contract and has passed a
real app-server planning and workspace-write probe against Codex CLI 0.144.3. The remaining provider findings reflect
the documentation and source available on that date and still require implementation-specific capability probes.

This document replaces the named trust-profile design. Harness will expose one permission document and four
execution modes rather than treating command rules as a filesystem sandbox.

## Objective

Harness should present **Read**, **Write**, **Full**, and **YOLO** as one stable editor contract while delegating
enforcement to each provider's strongest native mechanisms. The UI must describe the authority the launched process
actually received, not the authority Harness merely requested.

Three independent mechanisms produce that result:

1. **Execution mode** sets the maximum local capability the provider process may exercise.
2. **Provider security adapter** translates that ceiling into native sandbox, approval, and tool settings.
3. **Harness permissions** decide approval requests that the provider sends to Harness.

Keeping these mechanisms separate matters because an approval callback can deny a surfaced operation, but it cannot
contain a command that the provider executes without asking. A native or OS sandbox supplies the hard boundary.

## Settled design

- Remove named trust profiles and their profile-selection UI.
- Remove `:TrustCreate`.
- Rename `:Trust` to `:Permissions`.
- Store one Harness-owned JSON permission document in the same structural format as
  `rulesync-global/.rulesync/permissions.json`.
- Keep **Read**, **Write**, **Full**, and **YOLO** as the only user-facing execution modes.
- Render Read in white, Write in orange, Full in red, and YOLO in purple.
- Use native provider sandbox and approval controls whenever they can enforce the requested mode.
- Use provider-specific startup overrides when no typed live setting exists.
- Use each provider's native YOLO mechanism. Harness auto-approval remains a fallback only for approval requests that
  still reach it.
- Allow network access in every mode, including Read.
- Fail closed when Harness cannot prove that the current provider, version, and operating system enforce a mode.
- Apply the same or a narrower effective mode to every child agent. A child may never widen its parent's authority.

## Mode contract

| Mode | Local filesystem ceiling | Network | Approval behavior | Harness permissions |
| --- | --- | --- | --- | --- |
| **Read** | Reads may succeed. Local writes must fail. | Enabled | Retained | Evaluates surfaced requests |
| **Write** | Writes may succeed only inside the workspace. | Enabled | Retained | Evaluates surfaced requests |
| **Full** | Local writes may succeed anywhere the process identity permits. | Enabled | Retained | Evaluates surfaced requests |
| **YOLO** | Local writes may succeed anywhere the process identity permits. | Enabled | Provider-native bypass | Bypassed for authorization |

The mode defines a **ceiling**, not a promise that every permitted operation will succeed. Managed policy, operating
system access control, provider hard denials, and the current process identity may narrow it.

Read guarantees local filesystem immutability only when the selected backend can enforce that boundary. Read does not
mean offline. Full network access permits remote mutation through HTTP APIs, Git hosting, cloud CLIs, MCP servers, and
browser tools. It also permits exfiltration of any secret the process can read. The UI and documentation must state
that distinction directly.

Full and YOLO differ only in their approval path. Full keeps provider approvals active and routes surfaced requests
through Harness. YOLO selects the provider's native unrestricted and non-interactive mode. A Full session may still
run operations without asking when the provider already classifies them as allowed.

## Permission document

`:Permissions` opens one JSON document shaped like the Rulesync permission source:

```json
{
  "$schema": "https://github.com/dyoshikawa/rulesync/releases/latest/download/permissions-schema.json",
  "permission": {
    "edit": {
      ".": "allow"
    },
    "bash": {
      "git commit": "deny",
      "git commit *": "deny"
    },
    "webfetch": {
      "*": "allow"
    }
  }
}
```

Harness should load and compile this document once, then replace the compiled evaluator atomically after a valid save.
Approval routing must never scan or parse the source file per request.

The evaluator has three outcomes:

- `allow` approves a matching request without opening the approval float.
- `deny` rejects a matching request without opening the approval float.
- no match opens the approval float and lets the user choose once, always allow, or always deny.

Choosing always allow or always deny writes the narrowest provider-independent rule that represents the request, then
recompiles the document. The approval event records the matched rule and effective mode.

An `allow` rule cannot widen the current execution mode. For example, an edit allow in Read still fails at the native
sandbox. YOLO bypasses this document because applying a deny rule in YOLO would turn the mode back into a variant of
Full. Provider-managed policy and operating system denial still take precedence.

The global Rulesync config uses per-target feature sets and omits `permissions` for `codexcli`. This prevents a
generated Codex exec policy from rejecting a command before the Harness approval and sandbox boundary sees it.
Direct Codex CLI sessions therefore rely on Codex's native sandbox and approval policy. Other providers retain their
existing generated permissions until their Harness adapters own equivalent startup and approval contracts.

## Ownership model

The Rust broker should own security state because it launches providers, receives approval requests, persists session
configuration, and feeds the timeline. Lua should request a mode change and render the acknowledged effective state.

```text
Harness UI ──requests──▶ ModePolicy ──projects──▶ BackendSecurityAdapter ──launches──▶ Provider
                              │                            │
                              │                            └──reports──▶ SecurityCapability
                              │
                              └──constrains──▶ ApprovalRouter ──evaluates──▶ PermissionDocument
                                                                    │
                                                                    └──records──▶ SecurityAudit
```

The provider adapter must report which guarantees it can prove for the current executable version and operating
system. The UI derives its mode list from that report. It must never display a selectable Full or Write mode merely
because another backend supports it.

## Mode activation and transitions

Use a typed live provider setting when one exists. Otherwise, wait for a safe turn boundary, restart the provider with
the new launch projection, and resume the session when the provider supports resumption.

The transition follows this order:

1. Resolve the requested mode against provider, version, operating system, and managed-policy capabilities.
2. Build the complete native sandbox, permission, and approval projection.
3. Apply it live or restart the provider process.
4. Read back effective settings when the provider exposes them.
5. Run or consult the backend's capability proof.
6. Acknowledge the new mode and update the winbar.

Until step six succeeds, Harness keeps the previous mode visible and active. If restart would discard context, Harness
must warn before changing the mode. If the provider cannot prove the requested ceiling, the option stays hidden rather
than degrading silently.

Mode prompts still matter. After activation, the next model input should state the new behavioral constraint so the
model plans appropriately. That prompt improves behavior but never substitutes for enforcement.

## Approval and audit flow

For Read, Write, and Full, Harness processes approval requests in this order:

1. A provider hard denial may reject the operation before Harness sees it.
2. The native sandbox prevents operations outside the active mode ceiling.
3. The provider emits an approval request for an undecided operation.
4. `PermissionDocument` returns allow, deny, or prompt.
5. The float records a one-time choice or persists an always rule.
6. `SecurityAudit` records the request, rule, choice, provider, mode, session, and child-agent identity.

YOLO uses the provider's native bypass so provider semantics remain intact. Audit events distinguish observation from
authorization:

- `mode.entered_yolo` and `mode.left_yolo` record mode transitions.
- `approval.auto_allowed` records an approval request that Harness actually approved.
- `tool.executed_under_yolo` records observed execution when native YOLO suppressed the approval request.

Harness must not claim that it approved an operation when no approval request existed.

## Generic ACP findings

[ACP defines a client-agent protocol](https://agentclientprotocol.com/get-started/architecture), not a portable
sandbox. The client launches an agent subprocess, supplies MCP configuration, receives events, and can answer
permission requests. Those mechanisms do not constrain direct filesystem, shell, or network access performed inside
the agent process.

[ACP session configuration](https://agentclientprotocol.com/rfds/session-config-options) lets an agent advertise
arbitrary options. Semantic categories improve UI presentation but remain optional and cannot carry security meaning
without a provider-specific contract. Harness must never infer Read or Write from an option label alone.

[ACP additional roots](https://agentclientprotocol.com/rfds/additional-directories) identify paths available to the
session. They do not create a sandbox. Harness can enforce boundaries for client-mediated filesystem requests, but
direct provider access requires the provider's native sandbox or a Harness-owned operating system wrapper.

The ACP architecture therefore splits into two layers:

- The generic ACP driver owns streaming, sessions, MCP handoff, and permission responses.
- A known-agent launch adapter maps Harness modes to that agent's startup flags, config, and native sandbox.

Unknown ACP agents should expose only modes whose semantics their advertised capability and a runtime proof establish.
When no such proof exists, Harness should hide the unsupported mode rather than treat permission callbacks as complete
containment.

## Codex app-server findings

Codex provides the strongest typed mode integration among the evaluated backends.
[The app-server protocol](https://github.com/openai/codex/blob/main/codex-rs/app-server/README.md) accepts sandbox and
approval settings on `turn/start`, including `readOnly`, `workspaceWrite`, and `dangerFullAccess`. Turn overrides become
sticky defaults, and queued thread-setting updates can target the next turn.

Recommended projection:

| Harness mode | Codex projection |
| --- | --- |
| Read | `readOnly` with network enabled, approval policy retained |
| Write | `workspaceWrite` with workspace roots and network enabled, approval policy retained |
| Full | `dangerFullAccess` with approval policy retained |
| YOLO | `dangerFullAccess` with approval policy `never` |

Codex separates the sandbox policy from the approval policy. Harness can therefore change the filesystem ceiling
without conflating it with non-interactive execution. Codex app-server also emits structured command, file-change,
MCP, and permission events, which gives Harness a reliable approval and audit boundary.

Codex documents platform-native enforcement through macOS Seatbelt, Linux or WSL2 bubblewrap, and a native Windows
sandbox. Linux depends on user namespaces, bubblewrap availability, and compatible AppArmor policy. Native Windows
behavior remains provider-specific rather than equivalent to the Unix implementations.

The app-server schema carries a network-access flag, but released builds have reported operating-system-specific
regressions. Relevant reports include
[Linux workspace-write networking](https://github.com/openai/codex/issues/18337),
[macOS networking](https://github.com/openai/codex/issues/10390), and the earlier
[filesystem-restricted network request](https://github.com/openai/codex/issues/13361). Harness must verify Read plus
network and Write plus network against the launched build instead of trusting schema support alone.

Managed policy can narrow sandbox and approval choices. Harness cannot override those restrictions. Codex-native child
agents generally inherit the parent runtime boundary, but Harness should not expose a child-mode selector until the
launched version proves inheritance or supports an explicit narrower child policy.

## GitHub Copilot CLI findings

[Copilot CLI tool permissions](https://docs.github.com/en/copilot/how-tos/copilot-cli/use-copilot-cli/allowing-tools)
provide `--available-tools`, `--excluded-tools`, `--allow-tool`, `--deny-tool`, `--allow-all-tools`, and `--yolo`.
Denials win over allows, and managed settings may disable bypass. Native YOLO covers tools, paths, and URLs, but it does
not independently disable the local sandbox.

[Copilot's local sandbox](https://docs.github.com/en/copilot/how-tos/cloud-and-local-sandboxes/configuring-local-sandbox-settings)
can assign read-only or read-write filesystem paths and control outbound or local network access. The feature remains
preview-grade. Native Windows support requires an eligible Windows Insider build. Per-host filtering carries explicit
reliability limits on macOS and Linux, so Harness should request unrestricted network rather than claim precise host
filtering.

Recommended projection:

| Harness mode | Copilot projection |
| --- | --- |
| Read | Sandbox enabled, workspace read-only, network enabled, write tools excluded or denied |
| Write | Sandbox enabled, workspace read-write, outside paths read-only or denied, network enabled |
| Full | Sandbox disabled, standard tool approvals retained |
| YOLO | Sandbox disabled plus native `--yolo` |

User, repository, and managed Copilot denials may still narrow YOLO. Persistent allow rules may also broaden ordinary
modes if startup overrides do not replace them completely. Harness needs an isolated or fully projected launch
configuration and an effective-policy probe before it claims strict Read or Write.

### Copilot over ACP

[Copilot's ACP server](https://docs.github.com/en/copilot/reference/copilot-cli-reference/acp-server) supports core
chat, streaming, tool activity, permission requests, MCP configuration, slash commands, and session resumption.
`session/new` carries only a limited configuration set. Tool filters and effort are fixed when the ACP server starts and
apply to every session in that process.

Harness already controls one provider process per Harness session, so initial mode projection remains feasible. A live
change to effort, tool availability, or a startup-only sandbox control requires restart and resume. Some model and mode
changes may work through advertised slash commands, but those commands provide weaker typed acknowledgement than a
native RPC.

### Copilot SDK

[Copilot SDK hooks](https://docs.github.com/en/copilot/how-tos/copilot-sdk/hooks/hooks-overview) can observe or deny
every tool before execution. Its
[streaming events](https://docs.github.com/en/copilot/how-tos/copilot-sdk/features/streaming-events) expose richer
partial output, tool detail, usage, compaction, background activity, and parent subagent identifiers. The SDK also
supports custom tools, custom agents, skills, plugin directories, model changes, mode changes, and resume-time
reconfiguration.

That surface gives a future native backend stronger auditing and subagent integration than ACP. It still does not expose
one Codex-style per-turn configuration envelope. Model and mode can change live, while effort and tool availability
generally remain create-time or resume-time settings. The Rust SDK remains technical preview, so ACP should remain the
initial backend unless a feature requires native interception.

## Claude Code findings

[Claude Code permission modes](https://code.claude.com/docs/en/permission-modes) include `plan`, `default`,
`acceptEdits`, `dontAsk`, `auto`, and `bypassPermissions`.
[Permission rules](https://code.claude.com/docs/en/permissions) apply across tools, while
[the sandbox](https://code.claude.com/docs/en/sandboxing) contains Bash and its child processes. Built-in Read, Edit,
and Write tools use the permission system rather than the Bash sandbox.

Strict modes therefore require both layers:

| Harness mode | Claude projection |
| --- | --- |
| Read | `plan` or `dontAsk`, deny Edit and Write, sandbox Bash without writes, allow network |
| Write | `acceptEdits`, permit workspace file tools, sandbox Bash to workspace writes, allow network |
| Full | Disable filesystem sandbox restrictions, retain normal permission prompts |
| YOLO | Disable sandbox restrictions and use `bypassPermissions` |

Claude may auto-allow Bash when the command runs inside its sandbox. Explicit deny rules still take precedence.
`allowUnsandboxedCommands` must remain disabled for Read and Write or individual commands can escape the intended
ceiling.

The sandbox uses macOS Seatbelt and Linux or WSL2 bubblewrap. WSL1 lacks the required primitives. Native Windows
sandboxing remains unsupported. WSL2 sandboxing can block Windows executables reached through mounted Windows paths.
If sandbox setup fails, Claude may warn and continue unsandboxed unless `sandbox.failIfUnavailable` forces failure.
Harness must force fail-closed behavior for Read and Write.

Claude routes network access through a domain-aware proxy without TLS inspection. Broad domain access permits
exfiltration and remote mutation. Domain fronting and allowed Unix sockets can bypass assumptions, and access to a
Docker socket effectively grants host-level authority.

`bypassPermissions` may be disabled by administrators and carries additional restrictions when Claude runs as root or
through sudo. Mode changes can occur during a session, but bypass capability may need startup authorization. Harness
should restart and resume when it cannot prove a live transition.

## Antigravity CLI findings

[Antigravity permissions](https://www.antigravity.google/docs/cli-permissions) define action rules for file reads,
file writes, commands, URLs, unsandboxed execution, and MCP. Deny outranks Ask, which outranks Allow. Workspace reads
and writes may start auto-allowed, so Read must explicitly replace that default rather than layering a prompt on top.

[The CLI reference](https://antigravity.google/docs/cli-reference) exposes `toolPermission` modes such as
`request-review`, `proceed-in-sandbox`, `always-proceed`, and `strict`, plus non-workspace access and terminal-sandbox
settings. Command-line overrides remain active for the launched process, which makes per-session startup projection
preferable to mutating shared user settings.

Recommended projection:

| Harness mode | Antigravity projection |
| --- | --- |
| Read | Strict file-write denial, terminal sandbox enabled, network actions allowed |
| Write | Workspace file writes allowed, non-workspace writes denied, sandbox enabled, network allowed |
| Full | Sandbox disabled, `request-review` approvals retained |
| YOLO | Sandbox disabled plus native always-proceed or dangerous-skip-permissions behavior |

[Antigravity's sandbox page](https://antigravity.google/docs/cli-sandbox) describes Linux nsjail, macOS sandbox-exec,
and Windows AppContainer. Other current Antigravity permission and IDE documentation describes terminal sandboxing as
available on macOS and Linux with Windows support still pending. This official contradiction blocks any strict Windows
claim. Harness must probe the launched binary and hide Read or Write when it cannot prove AppContainer enforcement.

Antigravity can enforce native file, command, network, unsandboxed, and MCP rules. Generic ACP does not guarantee that
every native prompt becomes an ACP permission request, so Harness must use native launch settings for the ceiling and
native dangerous-skip behavior for YOLO. Approval routing can govern only requests that the backend actually forwards.

## Provider comparison

| Capability | Codex app-server | Copilot ACP | Copilot SDK | Claude Code | Antigravity CLI |
| --- | --- | --- | --- | --- | --- |
| Typed sandbox policy | Yes | Startup/config projection | Native SDK/config | Split sandbox and permissions | Native config/flags |
| Typed per-turn security update | Strong | Limited | Partial | Mode-dependent | Not established |
| Structured approval requests | Yes | ACP request | Hook and events | Native prompt integration needed | Transport-dependent |
| Native YOLO | `dangerFullAccess` + `never` | `--yolo` plus sandbox off | CLI/runtime equivalent | `bypassPermissions` | always-proceed/dangerous skip |
| Reliable native Windows sandbox | Available, version-specific | Insider preview only | Same CLI runtime | No | Official docs conflict |
| Best initial Harness path | Native app-server | ACP | Future advanced backend | Native CLI adapter | Native CLI adapter |

## Operating-system enforcement limits

### macOS

Seatbelt or sandbox-exec can enforce process filesystem boundaries when the provider launches every tool beneath the
sandbox. Provider network proxies may enforce domains separately. Host filters can remain coarse or unreliable, so a
mode should claim network enabled rather than claim an exact domain firewall unless a provider proves it.

### Linux

Bubblewrap and nsjail depend on kernel namespaces, installed helpers, mount behavior, and local security policy. Some
AppArmor configurations disable unprivileged user namespaces. Containers and nested sandboxes may block setup or alter
the resulting boundary. Read and Write must fail closed when setup cannot complete.

### WSL

WSL2 can use Linux sandbox primitives when its kernel and distribution permit them. WSL1 cannot. A WSL2 sandbox may
block launching Windows binaries through `/mnt/c`, so strict containment can trade away host-tool interoperability.

### Windows

No common mechanism spans the evaluated providers. Codex ships a native Windows sandbox. Copilot requires preview
Windows builds for its local sandbox. Claude supports WSL2 rather than native Windows sandboxing. Antigravity's current
documentation conflicts about AppContainer availability.

A Harness-level command allow list cannot fill that gap. Process names do not describe filesystem effects, shell
commands can invoke arbitrary programs, and PowerShell pipelines compound multiple operations. Without a provider or
OS sandbox, Harness can offer approval mediation but cannot promise strict Read or Write.

## Limits of the approach

### Approval visibility

Harness controls only approval requests the provider sends. Native built-in tools, preapproved commands, provider
automation, hooks, and background work may execute without a request. A permission document cannot govern invisible
operations.

### Persistent and managed configuration

Provider-global, repository-local, enterprise, and managed policies may change effective behavior. Hard denials safely
narrow a mode. Persistent allows can weaken Read or Write if the provider does not let Harness replace them. Harness
should use isolated config roots or complete startup overrides where supported, then verify effective behavior.

Managed policy always wins. Harness cannot turn an administrator-denied operation into an allowed one, including in
YOLO.

### Network side effects

Read prevents local writes, not remote writes. Full network can create issues, push API mutations, send messages, alter
cloud resources, or exfiltrate readable credentials. Host allow lists cannot classify whether an HTTP request reads or
mutates state.

### MCP and external services

MCP servers run as separate processes or remote services and may hold authority beyond the provider sandbox. A provider
may sandbox its MCP children, pass them through an external host, or leave them outside the boundary. Harness permissions
can deny a surfaced MCP request, but strict containment requires a verified provider guarantee or a Harness-owned MCP
process sandbox. See `mcp.md` for the separate MCP profile design.

### Browser, computer-use, and GUI tools

Browser automation and computer-use tools can mutate remote services or local applications outside a shell sandbox.
They require their own permission categories and capability declarations. Filesystem Read alone does not constrain
them.

### Paths and process escape

Symlinks, junctions, mount points, device paths, Unix sockets, Docker sockets, inherited handles, daemons, and external
terminals can cross a lexical workspace boundary. Harness string matching cannot provide canonical containment. The
native sandbox must resolve and enforce the real resource boundary.

### Git semantics

Workspace write access also permits changing the Git index, refs, hooks, and repository metadata unless the native
sandbox or permission rules distinguish them. Read, Write, Full, and YOLO describe filesystem scope. They do not by
themselves define a separate Git-history policy.

### Version drift

Several relevant controls remain previews. Flags, config fields, defaults, and event coverage can change between CLI
versions. Every backend adapter must declare a supported version range and reject unknown security projections until
tests prove them.

## Subagent inheritance

Every child agent receives the parent's effective mode as its maximum authority. Harness may assign a narrower mode to
a Harness-owned child, but never a wider one. Provider-native children remain available only when the backend proves
that the provider propagates the parent sandbox and approval boundary.

Child tool requests must include the child identity in approval and audit events. When a provider cannot direct child
approvals through Harness or prove native inheritance, the backend should disable child spawning in Read and Write or
report the limitation explicitly. The orchestration tradeoffs remain documented in `harness-owned-agents.md`.

## Failure behavior

- Hide a mode when the backend cannot construct and prove its ceiling.
- Abort Read or Write activation when native sandbox setup fails.
- Preserve the previous mode until a live update or restart succeeds.
- Warn before a restart that cannot preserve the session.
- Reject invalid permission documents and retain the last compiled evaluator.
- Treat an unknown approval category as unmatched and prompt outside YOLO.
- Record provider hard denials separately from Harness permission denials.
- Surface managed-policy conflicts in the winbar or mode picker rather than silently narrowing behavior.
- Never label a session YOLO until both unrestricted capability and native approval bypass are active.

## Verification contract

Unit tests should isolate mode resolution, provider projection, permission compilation, precedence, approval routing,
audit classification, and child-mode narrowing. Mocked providers should cover missing capabilities, failed restarts,
unknown versions, managed denials, and events that arrive without approval requests.

Real integration tests should launch each supported CLI with a fast model in disposable repositories. Each provider and
operating-system combination needs these canaries:

| Mode | Workspace write | Outside write | Network request | Expected prompt |
| --- | --- | --- | --- | --- |
| Read | Fails | Fails | Succeeds | Provider-dependent read/tool prompts only |
| Write | Succeeds | Fails | Succeeds | Undecided operations reach Harness |
| Full | Succeeds | Succeeds after native OS access | Succeeds | Surfaced approvals remain active |
| YOLO | Succeeds | Succeeds after native OS access | Succeeds | No Harness approval prompt |

The integration fixture should also verify:

- An always-allow choice updates the JSON document and applies on the next matching request.
- An always-deny choice updates the JSON document and rejects the next matching request.
- An allow rule cannot bypass the Read or Write ceiling.
- YOLO bypasses Harness permission evaluation while retaining provider-managed hard denials.
- MCP, browser, and child-agent capabilities never inherit stronger claims than their verified boundary.
- Restart-based transitions preserve the session or warn before discarding it.
- The winbar changes only after the backend acknowledges the effective mode.
- Security audit events distinguish requests, decisions, hard denials, and observations under YOLO.

Terminal MCP should drive the real Neovim flow for each supported backend. Verification must inspect the mode picker,
winbar acknowledgement, approval float, permission-file mutation, provider restart, session resumption, and timeline
audit events. Headless mocks cannot prove those interaction boundaries or operating-system enforcement.

## Implementation recommendation

Implement Codex first because app-server exposes typed sandbox and approval policies. Add Copilot ACP next with explicit
restart boundaries and operating-system capability gating. Treat a native Copilot SDK backend as a later upgrade for
tool interception, richer audit events, and subagent control. Add Claude and Antigravity only after their launch adapters
can prove strict Read and Write on the current operating system.

The macro result should remain conservative: Harness offers fewer modes when enforcement is incomplete, but every mode
it displays carries a testable meaning. That avoids a security-themed UI whose labels exceed the guarantees of the
provider process underneath it.
