# Forge architecture

Forge provides Git review, GitHub review, generated documents, and an AI Harness inside Neovim. Lua owns editor integration. A shared Rust executable owns native services and durable Harness state. Existing project language servers supply source-backed definition navigation when a plan reviewer requests it.

## Ownership boundaries

| Component | Owns | Does not own |
| --- | --- | --- |
| Neovim Lua | Windows, buffers, cursor and viewport state, unsent editing, user input, native effects | Durable task transitions or repository mutation authority |
| Shared Rust host | Typed service requests, canonical documents, repository work, admission, persistence, execution lifetimes | Direct Neovim API calls |
| Harness | Sessions, exchanges, plans, tasks, goals, permissions, provider coordination | An independent editor-side lifecycle model |
| Tree-sitter integration | Parsing, syntax captures, declaration projection, structural indexes, token mapping | Full program typechecking |
| Existing project LSP | Definition lookup against real workspace source and available dependencies | Generated plan stubs, plan submission validation, or dependency installation |

## Request and publication lifecycle

1. Lua captures an explicit action with its document, view, revision, and target identity.
2. The owning Rust service validates admission and performs the operation under its resource lifetime.
3. Rust publishes a canonical result or a structured failure.
4. Lua verifies that the receiving view is current before applying text, metadata, navigation, or another editor effect.

Long-running work must not publish into retired views or release resource ownership before its worker settles. Source identities prevent stale analysis from being applied to new text. These invariants also apply when a provider restarts or a user switches Harness sessions.

## Subsystem documents

| Document | Scope |
| --- | --- |
| [Buffers and rendering](buffers-and-rendering.md) | Physical documents, atomic publication, editable regions, and folds |
| [Diff presentation](diff-presentation.md) | Shared diff rows, compact views, and the analysis engine |
| [Languages](languages.md) | Tree-sitter assets, syntax analysis, declaration extraction, and source coordinates |
| [Agent LSP tools](lsp.md) | Semantic queries, authorization, and result coverage |
| [Host and protocol](host-and-protocol.md) | Executable ownership, transport, admission, cancellation, and worker shutdown |
| [Harness](harness.md) | Sessions, exchanges, input delivery, tasks, recovery, and compaction |
| [Permissions](permissions.md) | Access scopes, approval modes, saved rules, and shell command analysis |
| [Child agents](child-agents.md) | Delegated conversations, profiles, and session inspection tools |
| [Timeline](timeline.md) | Transcript publication, paging, check results, status, and metrics |
| [Providers](providers.md) | Provider adapters, capabilities, normalized activity, and cancellation |
| [Provider configuration](provider-configuration.md) | Settings, MCP servers, skills, and models |
| [Planning](planning.md) | Plan documents, control tools, validation, acceptance, and conformance |
| [Plan review](plan-review.md) | Review document, navigation, edits, annotations, and decisions |
| [Project checks](project-checks.md) | Check definitions, selection, execution, evidence, and repair |
| [Git repositories](git-repositories.md) | Repository identity, read admission, observation, and source content |
| [Status](status.md) | Status documents, index writes, and mutation admission |
| [Checkpoints](checkpoints.md) | Capture, change attribution, the object store, and restoration |
| [Commit messages](commit-messages.md) | Commit context, excerpts, and Background conversations |
| [GitHub services](github-and-review.md) | Native `gh` execution, storage, issues, and the remote queue |
| [Pull request review](pull-request-review.md) | PR creation, review documents, comments, and batched review |

## Source entry points

The plugin entry is `nvim/lua/forge/init.lua`, and command registration lives in `nvim/lua/plugins/forge.lua`. The shared host starts in `nvim/rust/forge/src/main.rs` and delegates to the library entry in `src/lib.rs`. Feature crates live under `nvim/rust/forge/crates/`. The editor transport and document adapters live under `nvim/lua/forge/`.

Start a code investigation at the owning feature rather than traversing every module. Legacy compatibility modules do not define current public command behavior. Follow the native service boundary and its active Lua adapter.

For change procedures and verification, return to [Contributing](../README.md).
