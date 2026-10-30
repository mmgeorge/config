# Languages and syntax

A language feature can work in one consumer and silently fail in another, because Forge's language support is several independent capabilities rather than one switch. Tree-sitter supplies syntax analysis, declaration projection, token coordinates, and highlighting. No language server parses or validates a virtual plan.

[Adding a language](../adding-language.md) and [Updating Tree-sitter](../updating-tree-sitter.md) are the procedures, and [Agent LSP tools](lsp.md) covers semantic queries.

## Capabilities

| Capability | Source of truth | Result |
| --- | --- | --- |
| Native syntax rendering | `forge-diff/src/syntax/assets.rs` and grammar dependencies | Highlight and injection captures |
| Declaration planning | `DeclarationOverview::language` and its adapters | Body-free interfaces, imports, fields, and documentation |
| Structural references | `declaration_index.rs`, `declaration_calls.rs`, and Harness plan modules | Scopes, callable owners, and source coordinates |
| Commit-message excerpts | `SyntaxHandle::outline` and `src/commit_message/excerpt.rs` | Signatures and changed-body excerpts, with plain diff fallback |
| Workspace definitions and agent queries | The shared Lua LSP adapter and the running project server | Definitions, references, and diagnostics for real source |
| Check invocation | Harness typed inputs and literal argv substitution | One configured command with recorded inputs |

The syntax registry supports more languages than planning, so add each extension to every capability you promise. Rust, TypeScript, TSX, and Lua have plan adapters, and configuration formats keep their values. Python, CUE, C, and C++ have syntax and commit excerpts but no plan adapters. `.scm` selects the Tree-sitter query grammar, not Scheme. The check executor has no test-runner adapters, so adding syntax support never implies a source-to-test mapping.

Parse errors disable only the affected declarations, so a recovered C or C++ header still produces excerpts.

## Parsers and queries

`forge-diff/Cargo.toml` selects upstream grammar crates at exact Git revisions, and `SyntaxLanguage::grammar` consumes each crate's language entry point. Prefer this path for a new language. WGSL, CUE, and LaTeX keep local parser sources, recorded in `forge-diff/grammar/manifest.json` and compiled by `forge-diff/build.rs`, because their upstream bindings need older Tree-sitter runtimes. The patched PowerShell grammar is a Cargo path patch under `forge/vendor/`.

Harness uses Bash, Zsh, and Nushell grammars for [shell approval analysis](permissions.md#shell-command-analysis), and its classifier must keep rejecting unresolved execution after parser updates. The repository-owned `algorithm` grammar parses plan Algorithm blocks.

`forge-diff/query/manifest.json` records query provenance, and `QUERY_ASSET` embeds the query text. Neovim runtime overrides in `nvim/queries/` and the bundled `nvim/lua/forge/queries/` are separate consumers, so updating one never updates another. Query node names, fields, predicates, and capture order must match the selected grammar. `query_runtime.lua` adds the plugin root to the runtimepath before any consumer calls `vim.treesitter.query.get(lang, "diff_context")`.

## Declarations and coordinates

`DeclarationOverview` compiles each declaration query once per grammar. Parsers, trees, and cursors stay local to each operation. Formatting can move rows, so source-token maps stay separate from displayed coordinates. Comment scans, callable extraction, and structural indexing use work lists instead of recursion, and traverse complete trees without a depth limit.

The Rust navigation boundary uses one-based lines and zero-based UTF-8 byte columns. The Lua adapter converts columns to each client's position encoding, and source digests guard both directions. An LSP result describes current project source, and cannot prove that a changed virtual signature type-checks.

## Syntax analysis

Native rendering and structural queries analyze complete sources without byte, line, capture, or decoration limits. The shared worker pool bounds concurrent jobs and propagates cancellation and deadlines. The syntax cache keeps up to 256 results, evicting old entries without invalidating live handles. See [Git repositories](git-repositories.md#statistics-and-caches) for the shared analysis cache budgets.
