# Agent LSP tools

A stale or partial language-server answer can mislead an agent into a wrong edit, and a query that reads unauthorized files would bypass Harness permissions. Rust therefore authorizes every target before the editor reads it, and every result reports its coverage, so an incomplete answer never reads as a clean one.

The [tool reference](../../../docs/forge/lsp-tools.md) defines arguments and formatting. This chapter covers how the tools work.

## Request flow

Nine read-only `lsp_*` tools expose source overview, symbol discovery, navigation, context, references, calls, call paths, diagnostics, and status. `SourceDeclaration` extraction for Rust, TypeScript, JavaScript, and Lua supplies source ranges, signatures, nesting, and visibility, separately from plan projection.

Rust owns one interaction scope per root or child turn, with the read policy and setting generation. It admits at most four requests per host, each with a 60-second startup wait and a ten-second operation deadline, or 30 seconds for path searches. Correlated `lsp.request` events reach the editor regardless of the selected view, and `session.lsp.authorize`, `session.lsp.respond`, and `session.lsp.analyze` bypass the busy broker lock. Target authorization precedes every editor read, and Rust checks returned paths again before publishing text. Cancellation sends `lsp.cancel`, and late replies cannot revive the request.

## Editor adapter

The shared Lua adapter owns client selection, hidden source buffers, and reference-counted leases. Agent queries read live editor contents, while plan navigation rejects source that differs from its captured digest. Unused hidden buffers expire 60 seconds after their last request.

`forge.lsp.startup` starts servers only for requests that need them. Overview, source-scoped symbol listings, and context with a local target parse authorized source directly. Context adds optional hover only from an already-ready server. Semantic operations own readiness checks, scoped by path and supported method; a request remembers ready clients across target resolution and subsequent lookups. Each new request checks readiness again. Query deadlines include the 60-second startup allowance.

Discovery finds JavaScript and TypeScript projects by `package.json` and Rust projects by `Cargo.toml`, and keeps every distinct root. Scoped directory requests reuse completed parent discovery, inspect ancestor manifests, and exclude vendored and build directories. File renames discover from their source path and contact matching providers for that path. Clients already serving a root are reused, and Harness never stops shared servers.

Root candidates resolve concurrently. A `root_dir` callback that does not answer within two seconds is skipped without reporting a server failure, and a late callback cannot start its server. The separate 60-second initialization allowance begins when a server is launched or reused.

## Result accuracy

`forge.lsp.target` resolves names and occurrences, failing on ambiguity or incomplete search before the semantic operation starts. `forge.lsp.paths` walks outgoing call hierarchy edges with cycle checks, and captures every visited file so any change invalidates the result.

`forge.lsp.diagnostics` reuses a pull report only while its buffer tick and client generation match, and reports push-only diagnostics with unknown freshness. Workspace diagnostics and symbols keep per-server coverage. Empty results, incomplete coverage, unsupported capabilities, startup failure, and disconnected servers each produce a distinct message.

## Rename ownership

`lsp_rename_symbol` and `lsp_rename_file` use the dedicated `lsp.mutation` event. Mutation replies transfer their complete JSON in ordered chunks through `session.lsp.respond`; list pagination never limits a workspace edit. Rust retains correlation, turn authority, directory permissions, and a host-wide mutation gate. Lua resolves targets, requests server edits, validates buffer versions, and holds affected clean buffers until Rust releases preparation.

Rust stages the complete filesystem result and its original bytes and modes before committing. Every affected resource is authorized using the invocation's directory policy and captured permission rules. Renames require existing permission and never request approval. Commit owns recovery until it settles even if the provider waiter is cancelled. Lua refreshes buffers and sends post-operation notifications only after commit. Native tool addresses receive the normalized change set through the existing provider event stream, preserving timeline and checkpoint attribution.

Read-only interaction modes exclude both tools at advertisement and execution. Every conversation's open-time instructions include `src/prompts/lsp.md`, which tells the model to prefer LSP tools over text search when they are available. The guidance is the same whether or not the tools are enabled; each turn's tool definitions show their availability ([Turn context](providers.md#turn-context)).
