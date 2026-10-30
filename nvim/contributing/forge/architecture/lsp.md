# Agent LSP tools

A stale or partial language-server answer can mislead an agent into a wrong edit, and a query that reads unauthorized files would bypass Harness permissions. Rust therefore authorizes every target before the editor reads it, and every result reports its coverage, so an incomplete answer never reads as a clean one.

The [tool reference](../../../docs/forge/lsp-tools.md) defines arguments and formatting. This chapter covers how the tools work.

## Request flow

Nine `lsp_*` tools expose source overview, symbol discovery, navigation, context, references, calls, call paths, diagnostics, and status. `SourceDeclaration` extraction for Rust, TypeScript, JavaScript, and Lua supplies source ranges, signatures, nesting, and visibility, separately from plan projection.

Rust owns one interaction scope per root or child turn, with the read policy and setting generation. It admits at most four requests per host, each with a 60-second startup wait and a ten-second operation deadline, or 30 seconds for path searches. Correlated `lsp.request` events reach the editor regardless of the selected view, and `session.lsp.authorize`, `session.lsp.respond`, and `session.lsp.analyze` bypass the busy broker lock. Target authorization precedes every editor read, and Rust checks returned paths again before publishing text. Cancellation sends `lsp.cancel`, and late replies cannot revive the request.

## Editor adapter

The shared Lua adapter owns client selection, hidden source buffers, and reference-counted leases. Agent queries read live editor contents, while plan navigation rejects source that differs from its captured digest. Unused hidden buffers expire 60 seconds after their last request.

`forge.lsp.startup` starts servers only for an explicit request. Discovery finds JavaScript and TypeScript projects by `package.json` and Rust projects by `Cargo.toml`, and keeps every distinct root. Clients already serving a root are reused, and Harness never stops shared servers.

## Result accuracy

`forge.lsp.target` resolves names and occurrences, failing on ambiguity or incomplete search before the semantic operation starts. `forge.lsp.paths` walks outgoing call hierarchy edges with cycle checks, and captures every visited file so any change invalidates the result.

`forge.lsp.diagnostics` reuses a pull report only while its buffer tick and client generation match, and reports push-only diagnostics with unknown freshness. Workspace diagnostics and symbols keep per-server coverage. Empty results, incomplete coverage, unsupported capabilities, startup failure, and disconnected servers each produce a distinct message.
