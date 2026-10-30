# Updating Tree-sitter

A parser that builds can still rename nodes, invalidate queries, move token positions, or change declaration extraction, so a successful compile proves little. This guide takes a grammar update through each consumer. It assumes familiarity with [Languages](architecture/languages.md). To add a language, see [Adding language support](adding-language.md).

Paths are relative to `nvim/rust/forge/crates/forge-diff/` unless stated otherwise.

## Asset ownership

| Path | Owns |
| --- | --- |
| `Cargo.toml` and the workspace `Cargo.lock` | Upstream grammar dependencies and pinned revisions |
| `grammar/manifest.json` and `grammar/<language>/` | Local grammar exceptions, file inventory, and hashes |
| `build.rs` | Compilation of local parsers and scanners |
| `query/manifest.json` and `query/` | Embedded queries and their provenance |
| `src/syntax/assets.rs` | Grammar entry points, language mapping, and query selection |
| `nvim/queries/` (repository root) | Neovim runtime query overrides, separate from the Rust queries |

Prefer an upstream Cargo grammar crate. Keep local sources only for a required patch or an incompatible binding, as WGSL, CUE, and LaTeX do. Updating Neovim's parser installation does not change the native parser.

Two grammars have their own process. The repository-owned Algorithm grammar lives under `grammar/algorithm/`, so regenerate it with the CLI version and ABI in its README, then refresh its hashes. The PowerShell grammar is a Cargo patch under `nvim/rust/forge/vendor/tree-sitter-powershell/`, where `FORGE-PATCH.md` records the upstream base, and `vendor/grammar-manifest.json` records its bytes.

## Update steps

1. **Record the current selection.** Note the Cargo package or manifest entry, its revision, and every query consumer.
2. **Update the dependency.** Change the crate version or Git revision and refresh the lockfile. For a local exception, copy only the manifest-listed files and license from one pinned revision. Never hand-edit generated parser tables.
3. **Check the runtime mapping.** Verify the Rust language entry point and `SyntaxLanguage` mapping. Update the Tree-sitter runtime only when required, then verify every grammar against it.
4. **Reconcile queries and extraction.** Compare `node-types.json` and grammar fields with native queries, Neovim overrides, and the language branches under `src/syntax/`.
5. **Refresh hashes.** Update only entries whose bytes changed, then run the inventory check from the repository root. A digest mismatch names the exact file. A `Source has no owner` diagnostic is an unrelated inventory entry, so resolve it separately.

   ```nu
   nu nvim/tools/check_forge_migration.nu
   ```

6. **Run targeted tests.** Exercise syntax, injections, extraction, formatting, and source mapping for the language, and add a regression fixture for each intentional change. Nushell, Bash, Zsh, and PowerShell also supply shell approval analysis, so run the `permissions::` tests and confirm unresolved commands stay rejected.
7. **Verify in the editor.** Rebuild, then check highlighting and plan navigation in a fresh host, and confirm project LSP definitions still get the right member-token position.
8. **Update documentation.** Keep version facts in manifests and lockfiles, not in prose.

## Failure recovery

| Failure | Response |
| --- | --- |
| A query rejects a node or field | Update the query for the pinned grammar, and add a regression. |
| A parser or scanner symbol is missing at link time | Check `build.rs`, the file inventory, and the entry-point mapping. |
| A manifest hash differs | Verify upstream provenance before changing the recorded hash. |
| Navigation columns shift | Compare byte positions, multiline syntax, and injected-language offsets. |
| Highlighting changes unexpectedly | Check query order, priorities, and injections. |
| The editor shows old behavior after a build | Start a fresh Forge host. |
