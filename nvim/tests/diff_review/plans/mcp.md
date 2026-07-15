# Harness MCP Profiles

## Status

Parked. Do not implement MCP profiles until the provider-isolation questions in this document have been
revalidated against the exact Codex, Copilot, and ACP versions that Harness launches.

This document records research and candidate architecture. It does not authorize implementation and does not
claim that every provider can currently enforce an exact MCP profile.

## Objective

Harness should expose named MCP environments such as `Personal` and `Work` without coupling server definitions
to a provider name. A profile selects which MCP servers an agent can see. The same semantic profile should work
through Codex app-server, Copilot over ACP, and future backends whenever their protocols can enforce the selected
set.

The selected profile applies only to runtimes launched by Harness. Standalone CLI configuration remains owned by
Rulesync. Harness should remember the last selected profile for the computer, use it for new sessions, and keep
the profile recorded by an existing session when that session resumes.

## Decisions already made

- `rulesync-global/.rulesync/mcp.json` remains the single catalog of reusable MCP server definitions.
- Named Harness profiles contain server-name sets rather than duplicate complete server definitions.
- Existing Rulesync `targets` remain for standalone CLI generation. Harness ignores `targets` when resolving its
  provider-neutral profiles.
- A profile describes the exact effective MCP server set visible to the agent, not merely servers added by
  Harness.
- A backend that cannot prove the exact effective set must reject profile activation rather than silently expose
  extra native servers.
- Harness's private ACP control server remains an internal exception. It carries Harness control tools and does
  not appear in the user-authored profile.
- Profile changes during an active turn queue until the turn reaches a safe boundary.
- The active profile always appears on the right side of the Harness winbar beside context usage. A queued change
  displays a pending marker until it becomes active.
- New sessions inherit the computer-wide last selection. Resumed sessions retain their persisted profile.
- If an ACP provider cannot reload an existing session with a different MCP set, Harness asks before starting a
  fresh provider session and losing provider context.

## Availability and authorization are separate

An MCP profile controls **availability**. A trust profile controls **authorization**.

```text
MCP profile ──selects──▶ servers exposed to the provider
Trust profile ──decides──▶ whether a reported server/tool request may execute
```

Keeping these responsibilities separate prevents an availability picker from becoming a second permission
language. Excluding a server before launch provides a reliable server-wide denial. Tool-specific enforcement
still depends on the provider emitting an approval request that Harness can observe.

Provider-private MCP execution that never reaches Harness cannot receive strict per-tool trust enforcement. A
future Harness-owned MCP proxy could close that gap, but proxying tools, resources, prompts, OAuth, elicitation,
and transport lifecycles would create a separate project.

## Proposed profile model

The Rulesync catalog retains complete server definitions:

```json
{
  "mcpServers": {
    "github": {
      "command": "github-mcp-server",
      "args": ["stdio"],
      "envVars": ["GITHUB_PERSONAL_ACCESS_TOKEN"],
      "targets": ["claudecode", "codexcli", "antigravity-cli"]
    }
  }
}
```

Harness profiles reference catalog keys through a small editable document:

```text
Name: Personal

Servers:
github
openaiDeveloperDocs
chrome-devtools
terminal-mcp
sem
```

The candidate initial sets inferred from the current catalog are:

```text
Personal
├─ microsoft-learn
├─ docs-mcp
├─ sem
├─ google-dev-knowledge
├─ chrome-devtools
├─ github
├─ openaiDeveloperDocs
└─ terminal-mcp

Work
├─ microsoft-learn
├─ docs-mcp
├─ sem
├─ chrome-devtools
├─ github-enterprise
└─ terminal-mcp
```

These memberships need confirmation before implementation. The catalog remains authoritative for commands,
URLs, arguments, environment references, headers, and transport settings.

## Candidate ownership model

Rust should own catalog parsing, profile compilation, persistence, backend projection, and capability checks.
Lua should own commands, completion, the profile editor, the picker, and winbar presentation.

```text
Rulesync MCP catalog ──read by──▶ McpCatalog
Profile documents ──owned by──▶ McpProfileRegistry

McpProfileRegistry ──resolves──▶ ResolvedMcpProfile + digest
HarnessSession ──records──▶ profile name + digest
MachinePreference ──records──▶ last selected profile

BackendRequest ──carries──▶ ResolvedMcpProfile
CodexBackend ──projects──▶ Codex effective configuration
AcpBackend ──projects──▶ ACP session mcpServers
```

The existing trust implementation provides the closest local pattern:

- `TrustProfileDocument` parses editable text.
- `TrustProfileRegistry` validates, compiles, atomically saves, and caches documents.
- `PermissionCoordinator` consumes compiled state without reparsing on every request.
- `views/trust_policy.lua` implements the `acwrite` editing workflow.

MCP profiles should reuse the lifecycle pattern, not the trust matcher. Server selection and permission matching
represent different domains.

## Current Harness integration points

The current broker initialization receives the workspace, backend launch command, model, effort, trust profile,
and data directories. It does not receive an MCP catalog path or profile directory.

`HarnessPreference` stores model, effort, and fast mode by workspace and backend. That key cannot represent a
computer-wide MCP default. The candidate design needs a distinct machine-preference record plus profile state on
each `HarnessSession`.

The session format should increment when MCP fields become persistent. Existing session policy already hides
outdated formats instead of migrating them, so implementation should preserve that rule.

The proposed broker surface was:

```text
mcp.list
mcp.create
mcp.open
mcp.save
mcp.select
```

The proposed initialization and snapshot additions were:

```text
catalog path
profile directory
seed profile
active profile
pending profile
available profile names
resolved profile digest
backend isolation capability
```

`mcp.select` should validate and resolve the complete profile before changing either session state or the machine
default. A failed save or resolution must retain the last valid compiled profile.

## Codex app-server findings

### Process lifecycle

The current `CodexBackend` starts a fresh `codex app-server` process for each backend operation and resumes the
stored Codex thread. A profile switch therefore does not need to terminate a persistent Codex process. The next
request starts a new process and can project a different environment before resuming the thread.

### MCP configuration cannot be replaced with an empty override

Codex loads MCP servers from layered configuration and active plugins. Command-line `-c` values form another
high-precedence layer, but table values merge recursively. The following does not clear lower-layer servers:

```text
codex -c 'mcp_servers={}' app-server
```

The relevant source is `codex-rs/config/src/merge.rs`, where `merge_toml_values` recursively merges table keys.
`Config::to_mcp_config` also incorporates plugin MCP definitions after ordinary configuration resolution.

Consequences:

- Adding selected profile definitions can create duplicates with native configuration.
- Disabling only known catalog entries does not remove unknown plugin-contributed servers.
- A profile cannot claim exactness until Harness verifies the effective server list before starting a turn.
- One-off `mcp_servers.<name>.enabled=false` overrides require care because partial definitions have produced
  invalid-transport failures in released Codex versions.

Primary evidence:

- [Codex config merge implementation](https://github.com/openai/codex/blob/main/codex-rs/config/src/merge.rs)
- [Codex app-server protocol](https://github.com/openai/codex/blob/main/codex-rs/app-server/README.md)
- [Codex advanced configuration](https://learn.chatgpt.com/docs/config-file/config-advanced)
- [Empty MCP table override issue](https://github.com/openai/codex/issues/16045)

### App-server discovery APIs

A normally launched discovery process can ask Codex to resolve its own configuration instead of reimplementing
Codex precedence:

```text
config/read
skills/list
plugin/list
hooks/list
mcpServerStatus/list
```

`config/read` returns effective layered configuration. `mcpServerStatus/list` returns configured MCP servers and
their tools, authentication state, server information, resources, and templates. These calls can establish the
runtime truth before Harness starts or resumes a thread.

### Skills

App-server provides first-class skill operations:

```text
skills/list
skills/extraRoots/set
skills/config/write
skills/changed
```

`skills/extraRoots/set` registers additional standalone skill roots for the current app-server process. The
registration disappears when the process exits, so the current per-operation Codex lifecycle would require
Harness to register roots on every process before starting a thread or turn.

`turn/start` can also carry an explicit `skill` input item. That gives Harness a precise invocation path after it
has resolved a selected skill.

### Agents

App-server has no symmetric `agent/list`, `agents/extraRoots/set`, or `agent/add` API. Codex discovers standalone
custom agent TOML files from:

```text
$CODEX_HOME/agents/*.toml
$REPO_ROOT/.codex/agents/*.toml
...
$CWD/.codex/agents/*.toml
```

Each definition supplies `name`, `description`, and `developer_instructions`. Optional fields can override model,
reasoning effort, sandbox mode, MCP servers, and skill configuration.

App-server can list spawned child threads and expose their role and nickname. That surface describes agent runs,
not the available definition catalog.

### Source `CODEX_HOME` discovery

If Harness uses an isolated Codex environment, it first needs the source home that the ordinary Codex process
would use. The proposed resolution order is:

1. Explicit Harness backend configuration.
2. The broker process's inherited `CODEX_HOME` value.
3. The platform home directory joined with `.codex`.

Codex documents `CODEX_HOME` as the root for configuration, authentication, logs, sessions, skills, and package
metadata. The directory defaults to `~/.codex` and must exist when explicitly set.

Finding the directory does not establish effective configuration. Project `.codex/config.toml`, project agents,
repository skills, managed settings, plugins, and command-line layers can add runtime behavior outside the source
home. The discovery app-server must remain the source of truth for effective configuration.

### Candidate Codex isolation strategies

#### Probe, disable, and verify

1. Launch app-server under the normal Codex environment.
2. Read effective configuration and enumerate effective MCP servers.
3. Build complete overrides that enable selected servers and disable every discovered unselected server.
4. Launch the runtime app-server with those overrides.
5. Call `mcpServerStatus/list` before starting or resuming a thread.
6. Abort if the effective list differs from the profile.

This approach preserves normal authentication, skills, agents, model providers, features, and plugins. It may
still fail when plugin injection cannot be disabled cleanly. Discovery also adds process startup work unless the
result can be cached against reliable configuration fingerprints.

#### Isolated `CODEX_HOME`

1. Discover the normal effective environment.
2. Build a Harness-owned Codex home containing only projected configuration.
3. Reattach skill roots with `skills/extraRoots/set`.
4. Mirror personal agent definitions and retain project-agent discovery through the workspace.
5. Bridge authentication without copying credentials into session records or logs.
6. Start app-server with the isolated home and verify the effective MCP list.

This approach creates the strongest process boundary. It also creates a configuration-virtualization layer that
must preserve model providers, feature flags, managed requirements, authentication, agents, skills, and any
other non-MCP behavior Harness promises to retain.

No isolation strategy has been selected. That decision caused this work to be parked.

## ACP findings

ACP assigns MCP configuration during session setup. `session/new`, `session/load`, and `session/resume` accept an
`mcpServers` list supplied by the client. The current Harness ACP backend supplies only its private control
server.

The candidate flow for a profile is:

```text
ResolvedMcpProfile
        │
        ├─ project supported transports
        ├─ append private Harness control server
        └─ send through session/new or session/load
```

ACP requires STDIO support. HTTP and SSE depend on capabilities advertised by the connected agent. Harness must
reject a profile that contains an unsupported transport and identify the server and transport. Silently dropping
the server would violate exact-profile semantics.

The current `AcpBackend` holds one persistent connection. A profile digest change therefore requires closing the
connection, launching a new ACP process, and loading the provider session with the new server list.

If the provider does not support session loading, preserving provider context and changing the MCP set cannot
both succeed. The settled UX is to explain the context loss and request confirmation before creating a fresh
provider session.

ACP only standardizes the servers supplied by the client. It does not guarantee that an implementation has no
provider-native servers of its own. Exact-profile support therefore needs an adapter-specific isolation
capability rather than an assumption applied to every ACP agent.

Primary evidence:

- [ACP session setup](https://agentclientprotocol.com/protocol/session-setup)
- [ACP protocol repository](https://github.com/agentclientprotocol/agent-client-protocol)

## Copilot findings

Copilot exposes several MCP entry points with different ownership:

- Copilot over ACP accepts client-provided session MCP servers through ACP.
- Copilot CLI accepts `--additional-mcp-config`. It augments the discovered registry and overrides only entries
  with the same server name.
- Copilot SDK sessions accept MCP server configuration programmatically, but trusted-workspace discovery still
  runs for SDK server-mode sessions.
- Resolution includes additional CLI config, plugin definitions, workspace `.mcp.json` or `.github/mcp.json`, and
  user `~/.copilot/mcp-config.json`.
- Native Copilot configuration and installed extensions can therefore contribute servers outside the supplied
  session set.

Current Copilot CLI builds also expose `--disable-builtin-mcps` and repeatable
`--disable-mcp-server=<name>` flags. The experimental SDK protocol includes MCP discovery, listing, and disable
operations such as `server.rpc.mcp.discover`, `server.rpc.mcp.config.disable`, `session.rpc.mcp.list`, and
`session.rpc.mcp.disable`.

That produces a candidate exact-isolation flow:

1. Disable built-in MCPs at launch.
2. Create the session with selected profile definitions.
3. List the complete effective session registry.
4. Disable every server absent from the selected profile.
5. List again and compare exact names before sending the first prompt.

This path must remain capability-gated because the session list and disable RPCs are experimental. Older builds
can use repeatable launch flags only when Harness can discover every native server before launch.

The work profile primarily matters because the user runs Copilot at work. A future implementation must test the
exact Copilot build used there and prove whether SDK or ACP session configuration replaces native discovery or
extends it. Until then, Copilot cannot advertise exact-profile capability.

Harness should not infer exactness merely because it successfully injected the selected servers. It needs a
provider-reported effective server inventory or a launch mode that disables native discovery.

Primary evidence:

- [Copilot CLI command reference](https://docs.github.com/en/copilot/reference/copilot-cli-reference/cli-command-reference)
- [Copilot SDK MCP documentation](https://docs.github.com/en/copilot/how-tos/copilot-sdk/features/mcp)
- [Copilot SDK generated MCP disable type](https://github.github.com/copilot-sdk-java/snapshot/apidocs/com.github.copilot.java/com/github/copilot/generated/rpc/McpConfigDisableParams.html)

## Agy and Antigravity findings

The installed Agy CLI discovers MCP configuration from global `~/.gemini/config/mcp_config.json`, workspace
`.agents/mcp_config.json`, and plugin locations. Its `/mcp` command manages status, reload, and logs. Individual
entries can declare `disabled` and `disabledTools`.

No public CLI flag currently supplies an additional per-session registry, replaces native discovery, or selects
an alternate config root. The public repository contains packaging, documentation, and changelog material rather
than the CLI implementation, so its internal merge behavior cannot be source-verified.

Antigravity's programmatic SDK may accept MCP configuration, but that does not prove the Agy CLI can isolate a
profile. A future Agy backend should remain capability-gated until its launched version can either suppress
native discovery or report an effective inventory that Harness can verify.

The likely choices are:

- implement an SDK-backed Antigravity adapter with explicit session configuration,
- materialize a provider-native profile before launch and restore it afterward, or
- report MCP profiles as unsupported for Agy.

Mutating the user's native configuration around every Harness session introduces races across terminals and
Neovim instances, so it should not become the default approach.

Primary evidence:

- [Antigravity MCP documentation](https://antigravity.google/docs/mcp)
- [Antigravity CLI reference](https://antigravity.google/docs/cli-reference)
- [Antigravity CLI repository](https://github.com/google-antigravity/antigravity-cli)
- [Antigravity CLI changelog](https://github.com/google-antigravity/antigravity-cli/blob/main/CHANGELOG.md)

## UI findings

The candidate user-facing surface is:

```text
/mcp                 open profile picker
/mcp <profile>       select by name
:McpCreate <name>    create an editable profile
:McpOpen <name>      open an existing profile
```

The picker should reuse the existing no-preview floating selection behavior used for model and effort choices.
It should not resize `HarnessInput`.

The right side of the winbar should render:

```text
MCP Personal  92% context left (258K)
```

While a turn is active and the user selects another profile:

```text
MCP Personal → Work pending  92% context left (258K)
```

The broker applies the queued profile only after the active turn completes. The UI must not claim the new
profile before the backend rebind and exact-set verification succeed.

No dedicated keybinding was selected. `Shift-Tab` remains reserved for cycling trust profiles.

## Failure behavior

- Missing catalog file: block backend launch and show the expected path.
- Invalid catalog definition: report the server and invalid field without resolving secrets into the error.
- Missing profile reference: reject the profile save and keep the last valid compiled version.
- Missing resumed profile: block the next turn and open profile selection instead of silently substituting one.
- Unsupported transport: name the server, transport, and backend capability.
- Extra effective server: abort before sending the prompt and show the unexpected server names.
- Backend rebind failure: keep the previous profile active when its process remains usable.
- ACP context reset: require confirmation and record that a fresh provider session was created.
- Catalog or profile change during a turn: defer reload until the turn ends.
- Secret resolution failure: identify only the missing environment-variable name.

The catalog and profile directory must count as protected Harness configuration. A model that can modify either
can change which executables launch on the next turn.

## Verification required before implementation

### Rust unit tests

- Parse every field currently used by the Rulesync catalog.
- Ignore `targets` during Harness resolution while preserving them for Rulesync generation.
- Reject duplicate names, malformed transports, invalid URLs, and unresolved profile references.
- Compile stable digests independent of JSON object ordering.
- Include secret reference names in digests without storing secret values.
- Create, save, reload, and atomically replace profile documents.
- Preserve the last valid compiled profile after an invalid save.
- Persist machine default and per-session profile through separate storage keys.
- Hide sessions from older format versions without migrations.
- Protect catalog and profile paths from provider writes.

### Backend contract tests

- ACP fake agent captures the exact `mcpServers` list on `session/new` and `session/load`.
- ACP profile changes close one connection and load the same provider session exactly once.
- ACP rejects unsupported HTTP or SSE servers without omission.
- The private Harness control server remains available for every ACP profile.
- Codex fake app-server captures projected launch configuration.
- Codex discovery identifies native and plugin MCP servers.
- Codex verification rejects one unexpected effective server before `thread/start` or `thread/resume`.
- Codex preserves the provider thread ID after a successful profile change.
- Skill roots register before a turn starts on every fresh Codex process.
- Agent discovery covers personal and every applicable project directory.

### Broker and Neovim integration tests

- A new session chooses the machine default.
- `/clear` creates a session from the current machine default.
- A resumed session keeps its persisted profile after the machine default changes.
- A profile selected during a turn appears as pending and applies once at the safe boundary.
- Failed backend verification leaves the old profile displayed and active.
- `/mcp` and `/mcp <prefix>` completion use the broker-provided profile list.
- The picker floats over Harness without changing split heights.
- Winbar text updates atomically without displaying a temporary false profile.
- Snapshot and session-switch synchronization preserve active and pending state.

### Real integration tests

- Run a cheap read-only Codex turn against a temporary STDIO MCP server.
- Switch to a profile that excludes that server, resume the same thread, and prove the server is absent.
- Add an unexpected native or plugin server and prove Harness stops before the next prompt reaches the model.
- Run Copilot over ACP with two temporary profiles and inspect the exact session setup payloads.
- Exercise the no-load ACP confirmation path and verify provider context resets only after approval.
- Drive `/mcp`, the picker, the winbar, a queued switch, and session resume through a real Neovim instance using
  Terminal MCP.
- Run the Rulesync global dry run because the catalog remains a Rulesync source file.

Mocks alone are insufficient. Exact MCP isolation depends on process startup, configuration layering, transport
capabilities, and provider-specific discovery that unit doubles cannot reproduce.

## Questions to revisit

- Should Codex use probe-disable-verify or an isolated `CODEX_HOME`?
- Can the current Codex app-server disable every plugin-contributed MCP without disabling unrelated plugin
  skills, hooks, or apps?
- How should an isolated Codex home bridge file-based authentication without duplicating credentials?
- Which non-MCP Codex settings must Harness preserve exactly when virtualizing configuration?
- Does the supported Copilot ACP or SDK version replace native MCP discovery or extend it?
- Can Copilot report its complete effective MCP inventory before the first turn?
- Has Agy added an exact per-session MCP configuration or a native-discovery disable flag?
- Should `Personal` and `Work` ship as seeded profile documents or be created from an interactive first-run flow?
- Should profile edits trigger immediate validation only, or also queue activation when the edited profile is
  currently selected?
- Can resolved configuration discovery be cached without missing changes to project config, plugins, agent
  definitions, or managed settings?

## Current recommendation

Keep the feature parked until Codex isolation and Copilot exactness have direct runtime proofs. The profile,
persistence, and UI model are straightforward. The unresolved work sits at the process boundary where provider
configuration layers can expose servers that Harness did not select.

Implementing the picker before that boundary is solved would produce a misleading security control. Proving the
effective tool environment first makes the profile meaningful and prevents Personal and Work from becoming
cosmetic labels over provider-dependent behavior.
