# Updating Tree-sitter

A grammar update must preserve both syntax rendering and the structural data used
by planning. A parser that builds successfully can still rename nodes, invalidate
queries, move token positions, or change declaration extraction.

Read [Languages](architecture/languages.md) and use the release workflow in
[Building and testing](building-and-testing.md). Add a new language through the
broader [language integration procedure](adding-language.md).

## Asset ownership

| Path | Owns |
| --- | --- |
| `nvim/rust/forge/crates/forge-diff/Cargo.toml` and `Cargo.lock` | Upstream grammar dependencies and resolved versions or Git revisions |
| `nvim/rust/forge/crates/forge-diff/grammar/manifest.json` | Remaining local grammar exceptions, revisions, file inventory, and hashes |
| `nvim/rust/forge/crates/forge-diff/grammar/<language>/` | Exception-only vendored parser, scanner, headers, metadata, and license |
| `nvim/rust/forge/crates/forge-diff/build.rs` | Compilation of the remaining local parsers/scanners |
| `nvim/rust/forge/crates/forge-diff/query/manifest.json` | Vendored query provenance and hashes |
| `nvim/rust/forge/crates/forge-diff/query/` | Queries embedded by the native syntax service |
| `nvim/rust/forge/crates/forge-diff/src/syntax/assets.rs` | Grammar entry points, language mapping, and embedded query selection |
| `nvim/queries/` | Neovim runtime query overrides, separate from the Rust query inventory |

Prefer an upstream Cargo grammar crate over copying parser sources. Use a pinned
upstream Git dependency when the required compatible binding is not published.
Keep local sources only for a necessary patch or incompatible upstream binding.
WGSL, CUE, and LaTeX currently retain local bindings for runtime compatibility.
The repository-owned Algorithm grammar has no upstream dependency. Its `grammar.js` and
generated assets live under `grammar/algorithm/`. Regenerate it with the CLI version and ABI
recorded in its README and manifest, then refresh its grammar and highlight-query hashes.
Do not update only Neovim's parser installation and assume the native parser
changed. For local exceptions, never replace generated `parser.c` without its
required scanner, headers, node metadata, and provenance.

Harness's PowerShell grammar uses the Cargo patch under
`nvim/rust/forge/vendor/tree-sitter-powershell/`. Its `FORGE-PATCH.md` and
`grammar.patch` record the upstream base and local source changes, and
`nvim/rust/forge/vendor/grammar-manifest.json` records all vendored bytes.
Regenerate with the recorded CLI and ABI before refreshing those hashes.
The remaining shell grammars retain their Cargo or native grammar manifest pins.

## Update sequence

1. **Record the existing asset selection.** Identify the Cargo package or local
   manifest entry, exact revision, and every query consumer for the language.

2. **Update the upstream dependency.** Change the grammar crate version or Git
   revision and refresh its lockfile entry. For a required local exception, copy
   only the manifest-listed files and license from one pinned revision. If parser
   generation is necessary, use its supported generator and declared ABI. Do not
   hand-edit generated parser tables.

3. **Reconcile the build and runtime mapping.** Verify the dependency's Rust language
   entry point and `SyntaxLanguage` mapping. Check C exports, scanner requirements,
   and `GRAMMAR` source locations only for local exceptions. Update the Tree-sitter
   runtime only when required, then verify every configured grammar against it.

4. **Reconcile query and extraction behavior.** Compare `node-types.json` and grammar
   fields with native queries, Neovim query overrides, and language branches under
   `src/syntax/`. Review highlighting, injections, captures, function owners,
   declaration roles, visibility, and member-token coordinates.

5. **Refresh local asset hashes and verify provenance.** Update only entries whose bytes
   changed. From the repository root, run the existing inventory check:

   ```nu
   nu nvim/tools/check_forge_migration.nu
   ```

   A passing result reports verified assets. A digest mismatch identifies the
   exact vendored file that differs. This check also validates the repository's
   migration inventory, so investigate that diagnostic separately if an unrelated
   source-ownership record fails.

6. **Run targeted native regression tests.** Use the release target from the build
   guide. Exercise syntax, injection/highlight cases, extraction, declaration
   formatting, and source mapping for the affected language. Add a regression
   fixture for every intentional query or node-shape change.
   Nushell also supplies Harness shell parsing, so run its `permissions::` tests
   and preserve rejection of unresolved commands and environment mutations.

7. **Rebuild Forge and verify editor behavior.** Check rendered highlighting and
   plan navigation in a fresh Neovim host. Confirm that existing project LSP
   definitions still receive the correct member-token position.

8. **Update the documentation.** Record changed language capabilities or integration
   steps in the language architecture and contribution guide. Keep version-specific
   facts in manifests and lockfiles instead of duplicating a second version list.

## Failure recovery

| Failure | Response |
| --- | --- |
| Query compilation rejects a node or field | Update the query for the pinned grammar and add a focused regression |
| Missing parser/scanner symbol at link time | Check `build.rs`, the vendored file inventory, and grammar entry-point mapping |
| A manifest hash differs | Verify upstream provenance before changing the recorded hash |
| The migration check reports `Source has no owner` | Resolve the named migration-inventory entry separately from parser/query hash failures |
| Declarations save but navigation shifts columns | Compare byte positions, multiline syntax, and injected-language offsets |
| Highlighting changes unexpectedly | Check query ordering, priorities, injections, and Neovim/native asset differences |
| The editor uses old behavior after a successful build | Start a fresh Forge host or restart Neovim |
