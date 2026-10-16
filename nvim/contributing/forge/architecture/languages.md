# Languages, syntax, and source mapping

Tree-sitter supplies syntax analysis, declaration projection, token coordinates, and highlighting. Plan navigation may ask an existing project LSP for a definition in real source, but the LSP does not parse or validate the virtual plan.

Read [Adding a language](../adding-language.md), [Updating Tree-sitter](../updating-tree-sitter.md), and [Planning](planning.md) before changing these boundaries.

## Independent language capabilities

| Capability | Source of truth | Result |
| --- | --- | --- |
| Native syntax rendering | `forge-diff/src/syntax/assets.rs`, bundled grammar and query manifests | Highlight and injection captures for real source |
| Declaration planning | `DeclarationOverview::language` and the declaration adapters | Body-free interfaces, imports, fields, documentation, and formatting |
| Structural references | `declaration_index.rs`, `declaration_calls.rs`, and Harness plan modules | Scopes, callable owners, reference expressions, and source coordinates |
| Workspace definitions | Shared Lua LSP adapter plus the running project server | Definition locations for captured real-source occurrences |
| Package ownership | Harness `checks/packages.rs` and package-manager metadata | Cargo, npm, and pnpm workspace members owning changed files |
| Test selection | Harness check selections, invocation resolver, and runner | Package checks or exact planned cases, with execution evidence |

The native syntax registry supports more languages than the declaration-planning
registry. Extensions and aliases must be added to the correct registry for each
promised capability. Rust, TypeScript, TSX, and Lua have plan-specific adapters.
Configuration formats retain their values rather than using executable-language
body projection.

Package discovery uses manifests and workspace membership rather than syntax or
LSP results. The check configuration chooses the package manager explicitly.
Cargo supports native exact-case execution through its standard test harness.
npm and pnpm provide package ownership, while runner-specific helpers retain
responsibility for case selection and receipts. Adding syntax support therefore
does not imply a source-to-test mapping. See
[Extending project checks](../extending-project-checks.md) for the integration
boundary and [Planning](planning.md#project-checks-and-the-harness-checking-gate)
for selection, preview, and execution ownership.

## Bundled parser and query ownership

`forge-diff/grammar/manifest.json` pins parser origin, revision, source subdirectory,
and exact vendored file hashes. `forge-diff/build.rs` compiles the C parser and an
optional scanner. `SyntaxLanguage::grammar` binds the exported entry point.
The pinned `tree-sitter` and `tree-sitter-language` dependencies define the runtime
ABI used by those grammars.

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

## Native syntax analysis bounds

Native syntax analysis applies the 10,000-line cutoff independently to each source
version. It has no source-byte reservation, retained-memory rejection, capture-count
limit, injection-count limit, or decoration-count limit. The shared worker pool still
queues work and propagates cancellation. Its byte admission excludes syntax sources,
which are already retained by their owners. The cache retains up to 256 results and
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

`AnalysisCache` reserves source capacity, result capacity, and one entry before work is admitted.
Defaults are 128 MiB of sources, 64 MiB of results, and 512 admitted entries. Reservations cover
pending work and remain attached to published consumer handles. Cache eviction does not release a
charge until the last such handle drops. Admission evicts least-recently-used entries only when
the cache is their sole owner, preserving reuse of results with live consumers.

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
