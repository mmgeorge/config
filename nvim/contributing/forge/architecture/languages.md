# Languages, syntax, and source mapping

A language feature can work in one consumer and silently fail in another, because Forge's language support is several independent capabilities rather than one switch. Tree-sitter supplies syntax analysis, declaration projection, token coordinates, and highlighting. Plan navigation and agent queries may ask an existing project LSP about real source, but no LSP parses or validates the virtual plan.

Read [Adding a language](../adding-language.md), [Updating Tree-sitter](../updating-tree-sitter.md), and [Planning](planning.md) before changing these boundaries.

## Independent capabilities

| Capability | Source of truth | Result |
| --- | --- | --- |
| Native syntax rendering | `forge-diff/src/syntax/assets.rs`, Cargo grammar dependencies, and local asset manifests | Highlight and injection captures for real source |
| Declaration planning | `DeclarationOverview::language` and the declaration adapters | Body-free interfaces, imports, fields, documentation, and formatting |
| Structural references | `declaration_index.rs`, `declaration_calls.rs`, and Harness plan modules | Scopes, callable owners, reference expressions, and source coordinates |
| Commit-message excerpts | `SyntaxHandle::outline` and `src/commit_message/excerpt.rs` | Signatures and bounded changed-body excerpts, with ordinary diff fallback |
| Workspace definitions and agent queries | The shared Lua LSP adapter and the running project server | Definitions, references, calls, symbols, and diagnostics for real source |
| Check invocation | Harness typed inputs and literal argv substitution | One configured opaque command with recorded input identity |

The native syntax registry supports more languages than declaration planning, so each extension and alias must be added to the registry for every promised capability. Rust, TypeScript, TSX, and Lua have plan adapters, and configuration formats keep their values. Python (`.py`, `.pyi`, `.pyw`), CUE, C, and C++ have native syntax and commit excerpts but no plan adapters. `.c` and `.h` select C. `.cpp`, `.cc`, `.cxx`, `.C`, `.c++`, `.hpp`, `.hh`, `.hxx`, `.h++`, `.ipp`, and `.tpp` select C++. `.scm` selects the Tree-sitter query grammar, not Scheme. None of these require editor parsers or language servers.

Commit excerpts select syntax from the file path, and signature and import support must be verified separately from highlighting. Parse errors disable only the affected declarations, so a recovered C or C++ header still produces excerpts. Coverage requirements are in [Commit-message excerpt coverage](../adding-language.md#commit-message-excerpt-coverage). The check executor has no package-manager or test-runner adapters, so adding syntax support never implies a source-to-test mapping. See [Extending project checks](../extending-project-checks.md) and [Planning](planning.md#project-checks).

## Parser and query ownership

`forge-diff/Cargo.toml` selects upstream grammar crates at exact Git revisions, `Cargo.lock` pins them, and `SyntaxLanguage::grammar` consumes each crate's language entry point. Prefer this path for a new language. WGSL, CUE, and LaTeX keep local parser sources, recorded with file hashes in `forge-diff/grammar/manifest.json` and compiled by `forge-diff/build.rs`, because their upstream Rust bindings require older Tree-sitter runtimes. The patched PowerShell grammar is a Cargo path patch under `forge/vendor/`. Harness consumes Bash, Zsh, and Nushell grammars for shell approval analysis, and its classifier must keep rejecting unresolved execution and environment changes after parser updates.

The repository-owned `algorithm` grammar, also compiled by `forge-diff/build.rs`, parses academic pseudocode for plan Algorithm blocks. It is registered under the `algorithm` and `pseudocode` aliases and highlighted by `query/local/algorithm/highlights.scm`.

`forge-diff/query/manifest.json` records native query provenance, and `QUERY_ASSET` embeds the query text. Neovim runtime overrides in `nvim/queries/` and the plugin's bundled `nvim/lua/forge/queries/` are separate consumers, so updating one never updates another. Query node names, fields, predicates, capture ordering, and injection languages must match the selected grammar. The bundled `diff_context.scm` queries capture the named scope enclosing a changed line as `@scope` and `@scope.name` for Rust, TypeScript, TSX, JavaScript, Python, and Slang. `query_runtime.lua` appends the plugin root to the runtimepath from every entry module before any consumer calls `vim.treesitter.query.get(lang, "diff_context")`.

## Declarations and coordinates

`DeclarationOverview` compiles each declaration query once per grammar, with separate slots for TypeScript and TSX. Parsers, trees, cursors, and capture maps stay local to each operation. Extraction preserves interfaces and source identity without claiming that an overview compiles. Formatting can move rows, so source-token maps stay distinct from displayed coordinates. Comment scans, literal scans, callable extraction, and structural indexing use work lists instead of recursion and traverse complete trees without a depth limit.

The Rust navigation boundary uses one-based lines and zero-based UTF-8 byte columns. The Lua adapter converts columns to each client's position encoding before a request and back afterward, and source digests guard both halves. An LSP result describes current project source and cannot prove that a changed virtual signature typechecks.

## Syntax analysis

Native rendering and structural queries analyze complete sources without byte, line, capture, injection, or decoration limits. The shared worker pool bounds concurrent jobs and propagates cancellation and deadlines, and the syntax cache keeps up to 256 results, evicting old entries without invalidating live handles. `AnalysisCache` keeps raw comparisons within 512 entries, 128 MiB of sources, and 64 MiB of results. A reservation stays charged until the last consumer handle drops, including a `DisplayCursor`, which holds an `AnalysisHandle` for its lifetime. Explicit work still returns an uncached handle when those budgets are full.

## Agent LSP queries

Nine `lsp_*` tools expose source overview, symbol discovery, navigation, context, references, calls, call paths, diagnostics, and status. The [tool reference](../../../docs/forge/lsp-tools.md) defines arguments, formatting, and limits. `SourceDeclaration` extraction for Rust, TypeScript/TSX, JavaScript/JSX, and Lua supplies source ranges, identifier positions, signatures, nesting, and visibility, separately from plan projection.

Rust owns one interaction scope per root or child turn, along with the read policy and setting generation. It admits at most four requests per host, each with a 60-second startup wait and a ten-second operation deadline (30 seconds for path searches). A transport watchdog allows 75 seconds for ordinary queries and 105 for path searches. Correlated `lsp.request` events reach the editor regardless of the selected view, and `session.lsp.authorize`, `session.lsp.respond`, and `session.lsp.analyze` bypass the busy broker lock. Target authorization precedes every editor read, and Rust checks returned paths again before publishing text. Cancellation sends `lsp.cancel`, and late replies cannot revive the request. `session.lsp.analyze` accepts ordered UTF-8 source chunks, so large files fit within the transport frame limit.

The shared Lua adapter owns client selection, hidden source buffers, and reference-counted leases. Agent queries read live editor contents, while plan navigation rejects source that differs from its captured digest. `forge.lsp.startup` starts servers only for an explicit request. Discovery finds JavaScript and TypeScript projects by `package.json` and Rust projects by `Cargo.toml`, visits nested directories in batches without total limits, and keeps every distinct root. Clients already serving a root are reused, and Harness never stops shared servers. Root-resolution callbacks are dispatched onto the main loop before client startup. Unused owned hidden buffers expire 60 seconds after their last request.

`forge.lsp.target` resolves declaration names and exact occurrences for position-based tools, failing on ambiguity or incomplete search before the semantic operation starts. `forge.lsp.paths` walks outgoing call hierarchy edges under one request budget with cycle checks, and captures every visited file so any change invalidates the result. `forge.lsp.diagnostics` reuses a pull report only while its buffer tick and client invalidation generation match, coalesces concurrent pulls, and reports push-only diagnostics with unknown freshness. Workspace diagnostics and symbols keep per-server coverage, so partial coverage never becomes a clean-workspace claim. Empty results, incomplete coverage, unsupported capabilities, startup failure, and disconnected servers produce distinct messages.
