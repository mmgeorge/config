# Neovim ACP Extensions

This document summarizes what we learned about ACP (Agent Client Protocol), ACP-oriented Neovim integrations, one-shot vs persistent sessions, existing plugins, and how a Neovim ACP plugin could be built in practice.

## Executive summary

ACP is a strong fit for building Neovim agent integrations, but the current Neovim ACP ecosystem is still young.

The most important takeaways are:

- ACP supports multiple sessions over a single connection.
- ACP does **not** standardize a true “ephemeral/no-persist” session mode.
- ACP does **not** standardize session naming in the core protocol.
- Existing Neovim tools are uneven:
  - `sidekick.nvim` is session/terminal oriented, not a one-shot ACP automation layer.
  - `agentic.nvim` is the closest thing to a thin ACP-style client in Neovim.
- If you want a robust ACP-based Neovim plugin, a custom implementation is very feasible.
- Rust is a very good fit for the ACP core.
- `nvim-oxi` plus a Rust ACP client plus Tokio background work is a promising architecture.

## What ACP is

ACP is a JSON-RPC based protocol for communication between code editors/clients and coding agents.

The core mental model is:

- an editor/client connects to an agent
- one connection can carry multiple sessions
- a session is where prompt/response state lives
- updates are streamed back over notifications or events

## Core ACP session semantics

### Multiple sessions over one connection

Yes: ACP supports multiple sessions over a single agent connection/process.

This is an important point because it means an editor does not need a brand-new agent process for every prompt.

### Session persistence

Persistence is **not guaranteed by ACP itself**.

What ACP standardizes is the session lifecycle and message flow. Whether sessions are resumable or stored on disk is provider-specific.

The protocol can expose session loading/resume-related capabilities, but persistence behavior is not a universal ACP guarantee.

### Throwaway / ephemeral sessions

ACP does **not** define a standard “ephemeral” or “do not persist this session” field in the baseline protocol.

That means:

- a client can create a fresh session and choose never to resume it
- a provider may still persist it internally
- true no-persist behavior is a provider feature, not a baseline ACP feature

Practical implication:

- “fresh session for commit generation” is easy
- “guaranteed non-persisted session” is not something ACP core promises

### Naming sessions

ACP core does not appear to standardize a human-readable session title/name field.

So if a UI or client supports session names, that is typically a client/provider feature, not a protocol-level primitive.

## Existing Neovim ACP-related options

## `sidekick.nvim`

### What it is

`folke/sidekick.nvim` is primarily:

- a Copilot/AI CLI session wrapper
- a terminal/session management layer
- a UI around long-lived interactive CLI sessions

It is **not** primarily a generic ACP protocol client library.

### How Sidekick sessions work

From the code we inspected:

- Sidekick keys sessions by tool + cwd
- with tmux/zellij mux enabled, sessions are persistent/reusable
- without a multiplexer, the session is basically a regular Neovim terminal/job, so effectively ephemeral

### Extension model

Sidekick is customizable through:

- `cli.tools.<name>`
- `cli.prompts.<name>`
- `cli.context.<name>`
- window/mux config
- runtime tool definitions under `sk/cli/*.lua`

### Why it is not a great fit for one-shot commit prompts

The important limitation is that Sidekick is built around attached sessions and terminal interaction.

It does not present a clean “send one prompt, capture structured result, exit” API the way your Goose-based workflow does.

So for one-shot commit generation, Sidekick is not a natural drop-in replacement.

## `agentic.nvim`

### Why it matters

`agentic.nvim` is the closest thing we found to a thin ACP client in Neovim.

### What we found in the source

Its public top-level API is UI/session oriented, but internally it contains a real ACP client abstraction.

Important internal capabilities include methods equivalent to:

- create session
- load session
- list sessions
- send prompt
- set mode
- set model
- cancel session
- stop generation

It also has structured handler hooks for:

- session updates
- permission requests
- tool calls
- tool-call updates
- errors

### Caveat

This low-level ACP client appears to be internal/undocumented rather than a committed stable public API.

That makes it valuable as:

- inspiration
- source to vendor or copy from
- proof that ACP-in-Neovim is practical

But risky as a stable dependency boundary.

## Is there a good standalone Lua ACP library?

We did **not** find a broadly adopted standalone general-purpose Lua ACP client library.

That means if you want ACP in Neovim today, the realistic choices are:

- vendor/adapt logic from `agentic.nvim`
- build your own small ACP client
- implement the ACP core in another language (for example Rust) and expose only a thin Neovim layer

## Rust ACP ecosystem

Rust looks significantly stronger than Lua for ACP client implementation.

### Official Rust SDK

We found the official ACP Rust crate:

- `agent-client-protocol`

We also found that the Rust SDK workspace contains Tokio-specific pieces, including:

- `agent-client-protocol-tokio`

This strongly suggests the official Rust ACP stack is Tokio-first or at least Tokio-friendly.

### Other Rust ACP-related crates

We found a few relevant crates/projects, including:

- `agent-client-protocol`
- `agent-client-protocol-schema`
- `acpx`
- `vtcode-acp-client`

#### `agent-client-protocol`

This is the official SDK/types layer from the ACP project.

Best use:

- if you want to build your own client cleanly on top of official protocol crates

#### `acpx`

This is a thin helper library built for launching ACP-compatible local subprocess agents over stdio.

It looks especially relevant if you want to launch agents like `copilot --acp`.

#### `vtcode-acp-client`

This looks like a fuller ACP client implementation with more opinionated higher-level pieces.

Useful, but more app-derived than the official SDK.

## Is Rust a good fit for an ACP client?

Yes — very much so.

ACP is not “just a bit of I/O.” It is a strongly Rust-shaped problem:

- async transport management
- stdio/socket handling
- JSON-RPC framing
- protocol typing
- session state machine logic
- event streaming
- cancellation
- robust error handling
- concurrency around UI-facing and agent-facing state

This plays well to Rust's strengths.

For a serious ACP-backed Neovim plugin, Rust is arguably a better fit than Lua for the ACP core.

## ACP and Neovim architecture recommendation

A strong architecture for a serious Neovim ACP plugin would be:

- UI layer: `nvim-oxi`
- ACP core: Rust
- async runtime: Tokio in worker thread(s)
- scheduling back into Neovim: libuv timers/callbacks or scheduled Neovim callbacks
- user-facing commands/config surface: direct Rust-exposed Lua module APIs, optionally with small Lua wrappers

This gives:

- low-overhead editor integration
- robust typed protocol handling
- clean separation of UI thread and background I/O

## Copilot CLI as an ACP-adjacent backend

We spent significant time investigating Copilot CLI because it is a likely backend for ACP/agent workflows.

## One-shot prompt mode

Copilot CLI supports programmatic prompt execution with:

```bash
copilot -p "your prompt"
```

This executes the prompt and exits.

Useful related flags:

- `-s` / `--silent`: only output the agent response, useful for scripting
- `--output-format=json`: stream JSON events instead of plain text
- `--model <model>`: select model

### What `-s` means

`-s` is `--silent`, which makes output more scripting-friendly by suppressing extra stats/output and leaving just the agent response.

## Why `--output-format=json` shows so many events

That JSON output is an event stream, not just a final payload.

You can expect events like:

- `session.mcp_servers_loaded`
- `session.tools_updated`
- `user.message`
- `assistant.turn_start`
- `assistant.message_delta`
- `assistant.message`
- `assistant.turn_end`
- `result`

If you only want the answer, use text mode or parse only the `assistant.message` event.

## MCP/tool disabling in Copilot CLI

We discovered that simply pointing Copilot CLI at a clean `--config-dir` does **not** disable built-in MCPs.

Important distinction:

- config dir changes which saved config and MCP config files are loaded
- built-in MCPs still exist unless explicitly disabled

Relevant flags include:

- `--config-dir <dir>`
- `--disable-builtin-mcps`
- `--disable-mcp-server <name>`
- `--available-tools`
- `--excluded-tools`

This is especially relevant if you want one-shot commit generation with minimal startup overhead and minimal tool set.

## Separate config dirs for throwaway workflows

One effective strategy for keeping “commit generation” separate from normal Copilot CLI usage is:

- point commit-generation runs at a separate `--config-dir`

This causes session state, logs, and config to live under a different tree.

That does not change ACP semantics, but it is a very practical way to avoid polluting your main Copilot CLI environment.

## Copilot CLI session persistence and storage

### Where sessions are stored

We confirmed Copilot CLI stores local session state under:

```bash
~/.copilot/session-state/
```

with per-session directories like:

```bash
~/.copilot/session-state/<session-id>/
```

These directories can contain things like:

- `events.jsonl`
- `workspace.yaml`
- `files/`
- `checkpoints/`
- `session.db`
- `workspace.yaml`

### Are those directory names hashes of the working directory?

No.

Those directory names are session IDs, not directory hashes.

To find which sessions correspond to a given working directory, inspect each session's `workspace.yaml`.

Example pattern:

```bash
find ~/.copilot/session-state -name workspace.yaml \
  -exec grep -H '/Users/matt9222/config' {} \;
```

### Can sessions be named?

Copilot CLI itself supports renaming sessions (for example via `/rename`), but this is a CLI/session-manager feature, not evidence of a core ACP session-name primitive.

### Can ACP delete or hide sessions?

ACP itself does not define a standard delete/hide session operation.

Practical options when using Copilot CLI as a backend:

- use a separate `--config-dir` for throwaway workflows so those sessions are isolated
- manually delete local session-state folders if you want to remove them from the local store

But there is no standard ACP “delete session” primitive.

## Reusing sessions vs one-shot sessions

For very small prompts, reusing a session can reduce startup overhead because you skip repeated process/session/tool bootstrap.

However:

- the model call still costs time
- once prompts get larger, startup overhead matters less

So the tradeoff is:

- `copilot -p`: simpler, more self-contained, more startup overhead
- reused session: lower latency for repeated tiny tasks, but more state contamination and persistence

For commit-message generation, whether reuse is worthwhile depends on whether you value:

- clean one-shot behavior
- or latency over repeated prompts

## Throwaway semantics across coding CLIs

We compared Copilot CLI with other coding CLIs.

### Codex

Codex has the clearest explicit ephemeral mode:

- `codex exec --ephemeral ...`

This is the strongest example of clear throwaway semantics.

### Claude Code

Claude supports one-shot invocations and has settings that can disable persistence globally, but we did not find a clean documented per-run ephemeral flag equivalent to Codex's.

### Copilot CLI

Copilot CLI supports one-shot prompting (`-p`) and resumable sessions, but we did not find a documented explicit no-persist / ephemeral flag.

So Copilot CLI can behave like a one-shot tool operationally, but not with the same strong explicit throwaway semantics as Codex.

## Existing local Neovim commit generation workflow

We inspected your current Neovim config and found that your commit-message flow currently uses Goose in a very one-shot-friendly way.

Specifically, in `nvim/lua/plugins/git.lua`, the commit buffer automation runs a command like:

```bash
goose run --recipe commit --output-format json --no-session -q
```

and conditionally adds:

```bash
--provider github_copilot --model gpt-4.1
```

This is a very good fit for commit-message generation because it is:

- one-shot
- structured (`json`)
- explicitly `--no-session`

Compared to this, Sidekick/Copilot-CLI session-oriented flows are more awkward if the goal is only “generate commit text and insert it into `COMMIT_EDITMSG`.”

## What a Neovim ACP plugin would need

A useful ACP plugin for Neovim would need to handle at least:

- agent discovery/config
- transport startup (likely stdio for CLI-based agents)
- initialize/authenticate
- create/load/list sessions
- send prompts
- handle streamed updates
- permission requests / tool calls
- cancellation
- mapping ACP state back to Neovim buffers/windows/UI

For a polished UX, you probably also want:

- commit/throwaway session mode
- persistent session mode
- model selection
- mode selection
- separation of project sessions and utility sessions

## Feasibility of writing a minimal ACP layer

Writing a minimal ACP abstraction for Neovim is very feasible if scoped correctly.

A small MVP would likely include:

- stdio transport
- JSON-RPC framing
- `initialize`
- `session/new`
- `session/prompt`
- update stream handling
- cancel/close behavior
- some minimal event routing to buffers/UI

This is not trivial, but it is very manageable — especially in Rust.

## ACP + Neovim + Rust recommended design

A practical design for a serious ACP-based Neovim plugin looks like:

1. `nvim-oxi` provides the plugin/module surface.
2. ACP client is implemented in Rust.
3. Tokio runtime runs in background worker thread(s).
4. ACP worker emits strongly typed events such as:
   - session created
   - planning started
   - plan finished
   - permission requested
   - tool invoked
   - turn completed
5. Main Neovim thread consumes those events via nonblocking polling or scheduled callbacks.
6. UI surfaces include:
   - scratch buffers
   - side panes
   - inline updates
   - command output
   - commit-message insertion

This keeps the hard protocol logic out of Lua while still making the plugin feel native inside Neovim.

## Buffer and UI integration with `nvim-oxi`

`nvim-oxi` can do the editor-facing side required for agent UIs.

Capabilities include:

- create buffers
- set buffer contents
- open/switch buffers/windows
- attach to buffer change notifications with `nvim_buf_attach()` wrappers

This makes it possible to build interfaces like:

- chat/session transcript buffers
- plan buffers
- diff or review buffers
- prompt input buffers
- scratch result buffers

It also means a user-editable ACP buffer can be watched for changes and synchronized with agent/plugin state.

## Main-thread vs background-thread model

A very important design constraint is that Neovim UI work should stay on the main thread.

### Typical event flow

1. User triggers `prompt()` in Neovim.
2. Main thread validates input and enqueues ACP request.
3. Tokio worker performs asynchronous ACP transport work.
4. Worker sends `UiEvent` back to main thread through a channel.
5. Main thread handles the event in a libuv callback or scheduled callback.
6. Buffer/window/UI is updated.

This model avoids blocking the editor while still allowing robust async ACP integration.

## Testing ACP-backed Neovim plugins

For the Neovim-facing layer, `nvim-oxi` has a good testing story.

This means you can write tests that verify things like:

- a command opens the expected buffer
- a prompt action creates the expected UI state
- a mock worker event updates the visible buffer

For the ACP core, Rust unit tests and protocol-level tests can cover:

- framing
- session state transitions
- event handling
- resume/new-session behavior
- provider-specific quirks

This separation is valuable:

- ACP core tested as normal Rust logic
- Neovim integration tested inside actual Neovim via `nvim-oxi` test support

## Overall recommendation

If your goal is a serious Neovim ACP plugin, the current best path appears to be:

- build the ACP core in Rust
- expose the Neovim surface through `nvim-oxi`
- use Tokio in worker threads for the ACP runtime
- keep UI updates on the Neovim main thread
- treat one-shot “commit” sessions as a separate workflow/config/store from persistent editor sessions

In other words:

- do not try to force everything through Sidekick
- borrow ideas from `agentic.nvim`
- implement the ACP heart in Rust where protocol/state/concurrency are easier to reason about

That gives the cleanest path to a robust, testable, and performant Neovim ACP integration.
