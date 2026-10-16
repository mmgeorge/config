# Adding language support

A language is supported only for the capabilities implemented and tested for it.
Syntax highlighting, declaration planning, source-backed LSP navigation, package
discovery, and test selection are separate capabilities. Installing a language
server does not add a grammar, declaration projection, or plan validation rules.
Package discovery does not require a parser or language server. Follow
[Extending project checks](extending-project-checks.md) for package ownership and
test-runner integration. Cargo, npm, and pnpm currently provide package discovery,
while native exact-case execution supports Cargo's standard test harness.

Read [Languages](architecture/languages.md) and [Planning](architecture/planning.md)
before changing the boundary. Use [Building and testing](building-and-testing.md)
for release artifact selection and test execution.

## Current capability boundaries

The current `DeclarationOverview::language` recognizes Rust, TypeScript
(`.ts`, `.mts`, `.cts`), TSX, and Lua, alongside supported configuration formats.
The broader `SyntaxLanguage` registry also includes languages used only for
syntax rendering. JavaScript rendering support is not proof of JavaScript plan support.

Rust and TypeScript have saved-plan declaration resolution. Lua has its own plan
reference handling. The generic LSP adapter queries real source only when a
captured occurrence still maps to that source. It reuses a running project client,
adds no jump-result cache, and never installs dependencies or creates plan stubs.

## Prerequisites

- Use the Rust toolchain pinned by `nvim/rust/forge/rust-toolchain.toml`, a C compiler for bundled grammars, Neovim, and Nushell.
- Choose a pinned grammar revision and preserve the upstream license and source provenance.
- Install and configure the language server separately if source navigation is in scope.
- Use a small fixture project with relative local dependencies. Do not add a large framework just to exercise a definition jump.

## Integration sequence

1. **Declare the supported capabilities.** Specify extensions, Neovim filetypes,
   declaration forms, visibility, namespaces, and which references remain unchecked.
   Keep unsupported typechecking or external dependency validation explicit.

2. **Register the parser and syntax assets.** Add the grammar to
   `crates/forge-diff/grammar/manifest.json`, its vendored source directory, and
   `crates/forge-diff/build.rs`. Extend `SyntaxLanguage` in
   `crates/forge-diff/src/syntax/assets.rs`, including its names, aliases, path
   selection, grammar function, and applicable `QUERY_ASSET` entries. Follow
   [Updating Tree-sitter](updating-tree-sitter.md) for query provenance and digests.

3. **Implement declaration projection.** Extend the language selection and adapters
   under `crates/forge-diff/src/syntax/`. Preserve signatures, fields, imports,
   documentation, scopes, and visibility while omitting executable bodies according
   to the language's plan format. Cover formatting, folding, declaration contracts,
   and implementation-conformance extraction. Do not assume a grammar's node names
   or field names match another language.

4. **Implement reference extraction and source mapping.** Extend
   `declaration_calls.rs` and `declaration_index.rs` as required. Preserve each
   reference's original expression separately from its inferred name. Capture the
   referenced member token, not merely its receiver. Use one-based source lines
   and zero-based UTF-8 byte columns at the Rust boundary. Test repeated occurrences,
   ambiguous receivers, optional chains where applicable, and non-ASCII text.

5. **Integrate saved-plan behavior.** Review
   `crates/forge-harness/src/declaration/` and `src/plan/`, including `calls.rs`,
   `references.rs`, `workspace_definition.rs`, and `conformance.rs`. Decide which
   internal references can be validated and which must remain unverified. Preserve
   malformed-target rejection, round trips, deletion handling, incoming-use checks,
   and proposal-only navigation without consulting the current workspace as proof
   that a changed proposal is valid.

6. **Verify project LSP selection.** Check the filetype produced by
   `DeclarationOverview::filetype` against the configured client's `filetypes` and
   workspace root. The shared adapter is
   `nvim/lua/forge/views/plan_review/workspace_definition.lua`. Extend the language
   mapping rather than creating another per-language request/cache implementation.
   Preserve UTF position conversion, Location/LocationLink handling, cancellation,
   digest checks, and the no-autostart rule for hidden source buffers.

7. **Add the fixture and regression cases.** Add a project under
   `nvim/tests/forge/fixtures/plan_navigation/` and extend the fixture generator in
   `crates/forge-harness/examples/plan_navigation_fixture.rs` and the real-host test
   `nvim/tests/forge/plan_navigation_languages_host.lua`. Include a local declaration,
   a relative dependency, and a language-specific member-reference case. Keep the
   fixture's dependency graph local and small.

8. **Rebuild and verify the complete path.** Run targeted Rust extraction and plan
   tests first, then the adapter tests and the real Neovim/Harness/LSP fixture.
   Confirm the editor selects the actual definition token and can return to the
   plan. Update the capability descriptions and relevant architecture chapter.

All `crates/...` paths above are relative to `nvim/rust/forge/`.

## Acceptance checks

| Boundary | Required evidence |
| --- | --- |
| Parsing and projection | Valid source extracts, formats, saves, reopens, and retains source coordinates |
| Plan-only behavior | New declarations navigate internally without an LSP, and invalid internal references remain rejected |
| Captured references | Source expressions survive extraction and editing without being rewritten into guessed identities |
| Existing source | A real project server resolves both local and relative-dependency definitions |
| Unavailable analysis | Missing clients, empty results, timeouts, and unavailable external dependencies do not block plan saving |
| Stale results | A changed source, plan revision, view, or client cannot apply an obsolete definition result |
| Editor behavior | Navigation selects the expected file/token or declaration popup and preserves return navigation |

## Troubleshooting

| Symptom | Check |
| --- | --- |
| Highlighting works but plan capture skips the file | The declaration extension mapping is separate from the syntax registry |
| A query no longer matches | Compare the pinned grammar's `node-types.json`, field names, and query predicates |
| A jump selects a receiver instead of a member | Check extraction byte columns and LSP encoding conversion |
| No running client matches | Check the configured filetype, project root, initialization, and definition capability |
| Only proposed references fail | Check saved-plan resolution rather than adding generated source or querying an unrelated workspace |
| A language fixture downloads many dependencies | Replace those dependencies with tiny relative local packages |

## Agent source context

Extend native `SourceDeclaration` extraction when adding a language to the focused
LSP tools. Cover identifier coordinates, declaration/body/documentation ranges,
nesting, and declared visibility. Unknown visibility must remain explicit. Add
fixture assertions for overview, context, and semantic destinations without
changing the independent plan-projection adapter. Verify the editor's configured
server through the real Harness host fixture and confirm client reuse.
