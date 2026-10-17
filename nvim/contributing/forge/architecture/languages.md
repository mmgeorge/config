# Languages, syntax, and source mapping

Tree-sitter supplies syntax analysis, declaration projection, token coordinates, and highlighting. Plan navigation may ask an existing project LSP for a definition in real source, but the LSP does not parse or validate the virtual plan.

Read [Adding a language](../adding-language.md), [Updating Tree-sitter](../updating-tree-sitter.md), and [Planning](planning.md) before changing these boundaries.

## Independent language capabilities

| Capability | Source of truth | Result |
| --- | --- | --- |
| Native syntax rendering | `forge-diff/src/syntax/assets.rs`, Cargo grammar dependencies, and local asset manifests | Highlight and injection captures for real source |
| Declaration planning | `DeclarationOverview::language` and the declaration adapters | Body-free interfaces, imports, fields, documentation, and formatting |
| Structural references | `declaration_index.rs`, `declaration_calls.rs`, and Harness plan modules | Scopes, callable owners, reference expressions, and source coordinates |
| Commit-message excerpts | `SyntaxHandle::outline` and `src/commit_message/excerpt.rs` | Signatures and bounded changed-body excerpts, with ordinary diff fallback |
| Workspace definitions | Shared Lua LSP adapter plus the running project server | Definition locations for captured real-source occurrences |
| Package ownership | Harness `checks/packages.rs` and package-manager metadata | Cargo, npm, and pnpm workspace members owning changed files |
| Test selection | Harness check selections, invocation resolver, and runner | Package checks or exact planned cases, with execution evidence |

The native syntax registry supports more languages than the declaration-planning
registry. Extensions and aliases must be added to the correct registry for each
promised capability. Rust, TypeScript, TSX, and Lua have plan-specific adapters.
Configuration formats retain their values rather than using executable-language
body projection.

Native syntax includes Python (`.py`, `.pyi`, `.pyw`) and CUE (`.cue`) without
installing editor parsers or language servers. Python uses a pinned upstream
grammar crate and the local query assets. Its context queries
recognize decorated functions and classes once, preserving their outer scope.
Python and CUE do not have declaration-planning adapters.

Native C and C++ syntax uses upstream grammar crates and the existing query assets.
`.c` and `.h` select C. `.cpp`, `.cc`, `.cxx`, `.C`, `.c++`, `.hpp`, `.hh`,
`.hxx`, `.h++`, `.ipp`, and `.tpp` select C++. The `c++` language alias selects
the `cpp` parser. `.scm` and the `scm` alias select the Tree-sitter query grammar,
not Scheme. These languages require no editor parser installation and do not
add declaration-planning or agent structural-query adapters.

Commit excerpts select syntax automatically from the file path. They require no
user configuration or language server. A language's signature and import support
must be verified separately from highlighting. The current regression matrix
covers Rust, TypeScript, JavaScript, Lua, Python, C, and C++ signatures,
JSON/TOML/YAML/CUE/query diffs, unknown-language fallback, and complete
Markdown/plain-text changes. C and C++ extraction follows function declarators,
retains template headers, recognizes bound lambdas, and excludes `#include` rows.
Parse errors disable only the affected declarations or imports. Recovered valid
subtrees remain eligible for excerpts, so conditional C/C++ linkage blocks do
not force an entire shared header into ordinary diff fallback.
Contributor acceptance requirements are in
[Commit-message excerpt coverage](../adding-language.md#commit-message-excerpt-coverage).

## Agent LSP queries

Nine focused `lsp_*` tools expose source overview, symbol discovery, navigation,
context, references, calls, call paths, diagnostics, and status independently of plan navigation.
The enabled-only model guidance and explorer profiles recommend overview/symbol
lookup followed by shared name-or-position selectors. The [tool reference](../../../docs/forge/lsp-tools.md)
defines arguments, formatting, and limits.

`SourceDeclaration` extraction operates on the native `SyntaxHandle` for Rust,
TypeScript/TSX, JavaScript/JSX, and Lua. It retains source ranges, exact identifier
positions, signatures, nesting, and declared visibility. It does not alter the
plan-specific declaration projection. Native parsing uses the shared syntax
workers. Extracted declaration lists and query results remain request-scoped.
The existing exact-source syntax cache is unchanged.

Lua captures authorized live snapshots and calls the correlated `lsp.analyze`
route. Rust validates the active request scope before native analysis, then checks
it throughout the await. This route bypasses the busy broker lock, like
`lsp.authorize` and `lsp.respond`. Rust validates returned paths and renders one
shared location gutter with budgeted source excerpts, or aligned call trees. Snapshot guards cover every
contributing buffer and disk source, including semantic destinations.

The shared Lua LSP source adapter owns client selection, hidden source buffers,
and reference-counted buffer leases. Agent queries use live editor contents.
Plan navigation retains its captured digest and rejects changed or unsaved source
that differs from that capture. Opening Harness warms enabled editor LSP
configurations against real project files. `forge.lsp.startup` uses Neovim's
configuration and client reuse, preserving the editor's server ownership. It
dispatches root-resolution callbacks onto Neovim's main loop before client
startup or buffer cleanup, because server configurations can resolve roots from
libuv process callbacks. Queued callbacks recheck workspace closure before acting.
It releases hidden buffer leases after initialization and never stops shared
servers when Harness closes. Agent queries also prepare explicit project files
and await initialization and reported work-done progress before selecting a client. No generated project or
source is required.

Startup considers one source per directory and enabled configuration within its
bounded scan. It resolves candidates using the original server root callback,
preferring a root containing the repository before falling back to the closest
nested workspace. Deeper independent workspaces are skipped once identified.
Discovery and a concurrent first query share pending startup work. Explicit
queries into separate workspaces retain separate editor-owned clients.

Rust owns one interaction scope per root or child turn, the read policy, and the
setting generation. It admits at most four requests per host, each with a
60-second startup wait and a separate ten-second operation deadline. Path searches
allow 30 seconds after startup. The Rust transport watchdog allows 75 seconds for
ordinary queries and 105 seconds for path searches, including startup and delivery. Correlated
`lsp.request` events reach an editor adapter
independently of the selected view. The `lsp.authorize` and `lsp.respond` routes
bypass the busy broker lock. Target authorization precedes editor reads, and Rust
checks returned paths again before publishing bounded text. Cancellation drops
the pending identity and sends `lsp.cancel`. Late replies cannot revive it.

The adapter uses pull diagnostics when supported and otherwise reads Neovim's
diagnostic store. Unknown freshness remains explicit. Harness retains no query
result cache and creates no dependency projects.

Package discovery uses manifests and workspace membership rather than syntax or
LSP results. The check configuration chooses the package manager explicitly.
Cargo supports native exact-case execution through its standard test harness.
npm and pnpm provide package ownership, while runner-specific helpers retain
responsibility for case selection and receipts. Adding syntax support therefore
does not imply a source-to-test mapping. See
[Extending project checks](../extending-project-checks.md) for the integration
boundary and [Planning](planning.md#project-checks-and-the-harness-checking-gate)
for selection, preview, and execution ownership.

## Parser and query ownership

`forge-diff/Cargo.toml` selects upstream grammar crates and exact Git revisions.
`Cargo.lock` pins their resolved packages. Each grammar dependency compiles its
own generated parser and scanner, and `SyntaxLanguage::grammar` consumes its Rust
language entry point. Prefer this path when adding a language.

WGSL, CUE, and LaTeX retain local parser sources because their selected upstream
Rust bindings require older Tree-sitter runtimes. `forge-diff/grammar/manifest.json`
records local grammar sources and their exact file hashes. `forge-diff/build.rs`
compiles their parsers and scanners. The separate patched PowerShell grammar
remains a Cargo path patch under `forge/vendor/`. Bash and Zsh already use upstream
Cargo grammar dependencies in `forge-harness` for shell approval analysis.
Harness also consumes the shared Nushell grammar. Its approval classifier must
continue rejecting unresolved execution and environment changes after parser updates.

The repository-owned `algorithm` grammar is also compiled by `forge-diff/build.rs`.
It parses academic pseudocode statements independently of a declaration's native language.
The native syntax registry exposes `algorithm` and `pseudocode` aliases, and Algorithm review
rows use the shared syntax worker pool and `query/local/algorithm/highlights.scm`. The grammar
manifest records its source and generated asset hashes and its Tree-sitter CLI/ABI command.
The submission validator adds indentation and control-flow nesting checks to the parsed statements.

The pinned `tree-sitter` and `tree-sitter-language` dependencies define the runtime
and grammar binding boundary. A compatible generated-parser ABI does not remove
an upstream Rust crate's dependency constraints.

`forge-diff/query/manifest.json` records native query provenance. `QUERY_ASSET`
embeds and orders query text for the native service. Neovim runtime overrides in
`nvim/queries/` are a separate consumer. Updating one consumer does not update the
other. Query node names, fields, predicates, capture ordering, and injection
languages must match the selected grammar.

## Declarations and source coordinates

Declaration extraction preserves interfaces and source identity without pretending
that an overview file is a compilable implementation. Formatting and presentation
can move rows, so source-token maps remain distinct from displayed coordinates.
Callable references retain their exact source expression even when inferred
receiver resolution is unavailable.

The Rust navigation boundary uses one-based lines and zero-based UTF-8 byte
columns. The Lua adapter converts byte columns to the selected client's position
encoding before a request, then converts returned locations back to byte columns.
Source digests and captured document identities guard both halves of the request.
The detailed request and proposal-mapping contract is owned by
[Planning](planning.md#source-backed-lsp-navigation).

An LSP result describes the current project source. It cannot establish that a
changed virtual signature or a new dependency in a plan typechecks. Saving and
submission retain their independent structural and internal-reference checks.

## Native syntax analysis and cache retention

Native rendering and explicit structural queries analyze complete source versions without
byte, line-count, capture-count, injection-count, or decoration-count admission ceilings.
The shared worker pool bounds concurrent jobs and propagates cancellation and deadlines.
Syntax sources remain retained by their existing owners. The cache retains up to 256 results and
evicts old entries without invalidating document handles or rejecting new analysis.

## Bundled tree-sitter queries (`queries/`)

The plugin ships its own queries instead of relying on the shared `nvim/queries/` tree, so
they travel with the plugin. `query_runtime.lua` registers them by computing the plugin
root from `debug.getinfo` and appending it to the runtimepath — once, and from every entry
path (`init.lua`, `git_data.lua`) so the queries resolve no matter which
module loads first.

- **`diff_context.scm`** captures the named scope (function, struct, class, trait,
  interface, impl, module, ...) enclosing a changed line. The capture is labeled `@scope`
  with a `@scope.name` child. `git_data` + `syntax_engine` use it to label hunk boundaries
  ("in `fn foo`") and to merge change regions by scope. Languages: rust, typescript, tsx,
  javascript, python, slang.

`vim.treesitter.query.get(lang, "diff_context")` resolves these through the runtimepath —
which is why `query_runtime` must run before any consumer.

---

## Analysis cache ownership and reservations

`AnalysisCache` tracks source capacity, result capacity, and one entry for each live analysis.
Cache retention budgets default to 128 MiB of sources, 64 MiB of results, and 512 entries.
Reservations cover pending work and remain attached to published consumer handles. Explicit work
evicts least-recently-used cache references as needed and still returns an uncached handle when
live allocations exceed those budgets. Eviction does not release a charge until the last consumer
handle drops. Optional speculative retention can decline a cache reservation without disabling
explicit analysis or losing its source content.

Publication rejects reservations from another cache, mismatched source identities, and retained
capacities above the reservation. Raw result accounting includes the result structure and hunk
vector capacity. Source accounting uses vector capacity and charges a shared old/new allocation
once within a result. Separate results conservatively charge shared source storage again.
Reservation excess remains charged until release.

This cache currently covers immutable raw comparisons. It does not account for temporary algorithm
allocations, allocator overhead, syntax trees, captures, patches created after publication, or
source/result clones made outside charged handles. The engine must preserve handle ownership across
consumers and reserve worker memory separately. Cache integration with engine jobs and display
cursors, source-level deduplication, and separate tree/capture accounting remain incomplete.

## Display cursors retain charged analysis handles

`DisplayCursor` now requires an `AnalysisHandle` instead of an uncharged `Arc<RawDiff>`. The handle
provides immutable access to the raw result while retaining its reservation for the full cursor
lifetime. Finishing delivery does not release the result while the cursor remains open. Dropping
the last cursor releases its charge only when no cache or other consumer still owns the handle.

Display and body-policy fixtures now publish their analysis through the cache before opening a
cursor. A lifecycle regression opens two cursors, evicts their shared cache entry, and verifies
that both can render while new admission remains saturated until the last cursor drops.

This closes the cursor's direct-reference accounting bypass. Generated row ownership, per-cursor
group and emphasis storage, independent source/result clones, worker memory, and live feature
integration still require their own accounting and lifecycle work.


### Agent target resolution and path search

`forge.lsp.target` resolves declaration names and exact source occurrences for
all position-based tools. File lookup uses the captured structural snapshot.
Workspace lookup asks existing language servers for bounded candidates and
validates exact qualified names against authorized snapshots. Ambiguous and
incomplete searches fail before the requested semantic operation begins.
Explicit file and directory symbol searches filter structural declarations before
pagination. Unscoped symbol searches use workspace symbols, with rust-analyzer's
all-symbol query mode and optional dependency scope. Server search limits cannot
truncate a file-scoped inventory.

`forge.lsp.paths` traverses outgoing call hierarchy edges after preparing both
endpoints. The query owns its request budget, cycle checks, route queue, and
captured source identities. It retains no cross-query results. All native sends,
including retries, consume the shared request budget. Every visited source file
is captured so a changed buffer or disk snapshot invalidates the whole result.
Rust renders only complete paths within the response budget.

Explicit agent structural queries use `SyntaxEngine::analyze_full` without a
source-byte or line cutoff. Rendering continues to use `analyze` with its existing
line cutoff. Cache keys separate cutoff results from full parsing for large sources.
The `lsp.analyze` route accepts ordered UTF-8 source chunks, validates the active
request and read authority for every chunk, and assembles one source under the
pending request. Cancellation releases unfinished transfer state. Chunking preserves
the transport frame limit without rejecting large source files.

List arguments accept any positive result limit. Lua bounds transport batches,
and Rust applies the response budget with continuation positions. Structured
visibility omissions carry names, positions, and total count through authorization
and rendering. Optional structural or semantic enrichment failures retain valid
primary results. Source changes, revoked authority, and cancelled requests remain
fatal. Empty results, incomplete coverage, unsupported capabilities, startup
failure, and disconnected servers have distinct output messages without retry
instructions.
