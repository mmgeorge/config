# Rust workspace diagnostics

Expose Rust Analyzer's published project diagnostics through Forge's
`lsp_diagnostics` tool without mistaking an incomplete snapshot for a clean
workspace. Reuse the server's existing diagnostics; never emulate a workspace
query by opening files or requesting diagnostics from each file.

## Current failure

`nvim/lua/forge/lsp/semantic.lua` requires
`diagnosticProvider.workspaceDiagnostics` for workspace diagnostic queries.
Rust Analyzer advertises document pull diagnostics but sets workspace pull
support to false, so Forge rejects the query before collecting any published
diagnostics. Rust Analyzer can nevertheless publish compiler diagnostics for
files across a project, including unopened files.

The local configuration in `nvim/lua/plugins/lsp.lua` sets
`rust-analyzer.check.workspace = false`. That setting requests package-scoped
checks where applicable, so accumulated diagnostics must not imply that every
workspace member has been checked.

## Required behavior

Choose the diagnostic acquisition method from the selected server's
capabilities. Keep one server and its resolved workspace root throughout the
request, including when the caller selects a nested Cargo manifest.

- Use `workspace/diagnostic` when the server advertises workspace pull support.
- Otherwise return the selected server's published diagnostic snapshot held by
  Neovim, including reports associated with unopened files.
- Restrict results to the resolved workspace root and apply the existing read
  authorization before exposing diagnostic messages or source excerpts.
- Preserve severity filtering, pagination, and source-position conversion.
- Keep document diagnostic queries on their existing acquisition path.
- Report unsupported acquisition, unavailable servers, and empty snapshots as
  distinct outcomes.
- Label published snapshots with partial coverage and unknown freshness unless
  existing evidence establishes a stronger result. Zero reported diagnostics
  must not imply that the workspace is clean.

No request may enumerate source files to synthesize workspace diagnostics,
open files to provoke diagnostics, or issue per-file diagnostic requests.
Reading the diagnostic records already held by Neovim is collection of existing
results, not per-file analysis.

## Implementation

Extend the existing diagnostics owner rather than adding Rust-specific branches
throughout the tool handler. Keep acquisition and result presentation separate
so future servers can use the same published-diagnostic path.

1. Add workspace acquisition to `nvim/lua/forge/lsp/diagnostics.lua`. Reuse
   Neovim's diagnostic namespace and replacement semantics instead of storing a
   duplicate diagnostic cache. Select the namespace belonging to the chosen
   client, and exclude document-pull namespaces from the published snapshot.
2. Route workspace diagnostic queries through that owner from
   `nvim/lua/forge/lsp/semantic.lua`. Retain the native workspace-pull path and
   its existing response validation.
3. Carry coverage and freshness through the existing result contract. Inspect
   its actual states before changing it; if additional states are required,
   update the typed host contract, renderer, tests, and affected protocol
   recordings together.
4. Preserve replacement and clearing behavior for published reports. Exclude
   reports from stopped or replaced clients, and do not promote unknown source
   versions to current merely because acquisition succeeded.
5. Update `nvim/docs/forge/lsp-tools.md` and
   `nvim/contributing/forge/architecture/lsp.md` to explain native workspace
   reports versus published snapshots, including the meaning of empty output.

Preserve the user's `check.workspace = false` setting. Changing compiler-check
scope is a separate policy decision and is not necessary to expose available
diagnostics.

## Refresh boundary

This change reads existing diagnostic state. It does not send
`rust-analyzer/runFlycheck`, run Cargo, or introduce implicit compilation into a
read-only query. Rust Analyzer's explicit refresh notification is a possible
separate feature; triggering a check and tracking its completion require their
own execution and lifecycle contract.

## Regression coverage

Use mocked clients and diagnostic storage at existing production boundaries.
Add cases to the existing mirrored unit test files, and keep each case focused
on the acquisition or presentation behavior it owns.

- In `nvim/lua/forge/tests/unit/lsp/diagnostics.test.lua`, verify native workspace
  pull preference, published reports for unopened files, report replacement,
  empty-publication clearing, and exclusion of stopped or replaced clients.
- In `nvim/lua/forge/tests/unit/lsp/semantic.test.lua`, verify selected-client and
  workspace isolation, nested-manifest root handling, authorization, severity
  filtering, pagination, and partial/unknown metadata for published snapshots.
- Verify that acquisition sends no per-file requests, opens no source buffers
  to generate diagnostics, and starts no compiler or refresh process.
- Add a focused case to
  `nvim/lua/forge/tests/integration/lsp/lsp.test.lua` that exercises the public
  workspace query with a mocked publishing server and verifies its coverage,
  freshness, and empty-result presentation.

Use the repository's mini.test isolation and shared mock helpers. Drive
publication and invalidation directly; do not wait on real clocks or create
real compiler failures to reach these states.

## Validation

Read the Forge architecture and editor-test procedures before implementation.
Run the focused Lua coverage through the configured `lua-test` check and finish
the change with a passing `rust-build` after the final edits, including for a
Lua-only implementation. If the host result contract changes, run the affected
Rust checks and recorded-payload consumers as well.

Exercise the public tool against a nested Forge crate and the independent
`zellij/sessions` project. Confirm server isolation and useful snapshot output;
an empty live snapshot alone cannot validate coverage. Review the final diff
for accidental configuration changes, per-file emulation, and implicit checks.

## References

These upstream sources explain the protocol boundary and the check-scope
constraint. Recheck them against the installed server when implementing the
change because capabilities can evolve.

- [Rust Analyzer capability declaration](https://github.com/rust-lang/rust-analyzer/blob/master/crates/rust-analyzer/src/lsp/capabilities.rs)
  declares workspace diagnostic pull support.
- [Rust Analyzer check configuration](https://rust-analyzer.github.io/book/configuration.html#rust-analyzer.check.workspace)
  describes workspace versus package-scoped checks.
- [Rust Analyzer flycheck control](https://rust-analyzer.github.io/book/contributing/lsp-extensions.html#controlling-flycheck)
  describes explicit refresh notifications.
