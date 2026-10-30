# Adding language support

Installing a language server does not add a grammar, declaration projection, or plan validation, so a language is supported only for the capabilities implemented and tested for it. This guide takes a language through each capability and shows what proves it works. It assumes familiarity with Tree-sitter and the [Languages](architecture/languages.md) chapter.

Syntax highlighting, declaration planning, LSP navigation, commit-message excerpts, and test selection are separate. Do only the steps for the capabilities you are adding. Repository check callbacks need no parser, so use [Extending project checks](extending-project-checks.md) for test-runner integration.

All `crates/...` paths are relative to `nvim/rust/forge/`.

## Current support

`DeclarationOverview::language` recognizes Rust, TypeScript (`.ts`, `.mts`, `.cts`), TSX, and Lua, alongside configuration formats. The broader `SyntaxLanguage` registry includes languages used only for rendering, so JavaScript rendering does not mean JavaScript plans work. Rust and TypeScript have saved-plan declaration resolution, and Lua has its own reference handling.

## Prerequisites

- Use the Rust toolchain pinned by `nvim/rust/forge/rust-toolchain.toml`, a C compiler, Neovim, and Nushell.
- Choose a pinned grammar revision, and keep the upstream license and source provenance.
- Install the language server separately if source navigation is in scope.
- Use a small fixture project with relative local dependencies.

## Steps

1. **Declare the capabilities.** List the extensions, Neovim filetypes, declaration forms, visibility rules, and which references stay unchecked.
2. **Register the parser.** Add the upstream grammar crate to `crates/forge-diff/Cargo.toml`, at an exact revision if needed. Vendor only when a required patch or incompatible binding prevents using it directly, and record that in `crates/forge-diff/grammar/manifest.json`. Extend `SyntaxLanguage` in `crates/forge-diff/src/syntax/assets.rs` with names, aliases, path selection, grammar function, and `QUERY_ASSET` entries. [Updating Tree-sitter](updating-tree-sitter.md) covers query provenance.
3. **Implement declaration projection.** Extend the adapters under `crates/forge-diff/src/syntax/`. Keep signatures, fields, imports, documentation, scopes, and visibility, and omit executable bodies. Do not assume a grammar's node names match another language's.
4. **Implement reference extraction.** Extend `declaration_calls.rs` and `declaration_index.rs`. Keep each reference's original expression separate from its inferred name, and capture the referenced member token, not its receiver. Use one-based lines and zero-based UTF-8 byte columns. Test repeated occurrences, ambiguous receivers, and non-ASCII text.
5. **Integrate saved-plan behavior.** Review `crates/forge-harness/src/declaration/` and `src/plan/`, including `calls.rs`, `references.rs`, `workspace_definition.rs`, and `conformance.rs`. Decide which internal references can be validated and which stay unverified. Do not use the current workspace as proof that a changed proposal is valid.
6. **Verify LSP selection.** Check `DeclarationOverview::filetype` against the client's `filetypes` and workspace root. Extend the mapping in `nvim/lua/forge/views/plan_review/workspace_definition.lua` rather than adding another per-language adapter.
7. **Add fixtures.** Add a project under `nvim/tests/forge/fixtures/plan_navigation/`, and extend `crates/forge-harness/examples/plan_navigation_fixture.rs` and `nvim/tests/forge/plan_navigation_languages_host.lua`. Include a local declaration, a relative dependency, and a member-reference case.
8. **Verify commit excerpts.** Exercise `SyntaxHandle::outline` against real source, and extend `src/commit_message/excerpt_tests.rs`. See [Commit-message excerpts](#commit-message-excerpts).
9. **Verify the complete path.** Run the targeted Rust tests, then the adapter tests and the real host fixture. Confirm the editor selects the actual definition token and can return to the plan. Update the relevant architecture chapter.

To add a language to the agent LSP tools, also extend `SourceDeclaration` extraction with identifier coordinates, declaration and body ranges, nesting, and visibility, and leave unknown visibility explicit. Verify with [the host fixture](verification-by-area.md#agent-lsp-tools).

## Acceptance checks

| Boundary | Required evidence |
| --- | --- |
| Parsing and projection | Valid source extracts, formats, saves, reopens, and keeps source coordinates. |
| Commit excerpts | Complete signatures and the first 20 changed body lines survive, and omissions keep exact change counts. |
| Plan-only behavior | New declarations navigate internally without an LSP, and invalid internal references stay rejected. |
| Existing source | A real project server resolves local and relative-dependency definitions. |
| Unavailable analysis | Missing clients, empty results, and timeouts do not block plan saving. |
| Stale results | A changed source, plan revision, view, or client cannot apply an old definition result. |
| Editor behavior | Navigation selects the expected token or source buffer, without a popup, and return navigation works. |

## Commit-message excerpts

Commit generation uses `SyntaxLanguage::for_path` and the shared syntax engine, independently of planning and LSP navigation. Registering a grammar does not establish signature-aware excerpts, so verify these behaviors:

- Declaration extents, multiline signatures, closing delimiters, nested declarations, and imports are recognized.
- Inline callbacks stay in source order, and bound functions keep their binding.
- Same-named methods and overloads keep separate budgets.
- A byte cutoff keeps both old and new signatures, or neither.
- Added and removed callables keep only their signatures, including inline bodies.
- Plain diff fallback works when no grammar is available or a declaration cannot be parsed.

Signature-aware coverage currently tests Rust, TypeScript, JavaScript, Lua, Python, C, and C++. JSON, TOML, YAML, and CUE have diff-excerpt coverage. Keep the language matrix in `src/commit_message/excerpt_tests.rs` current. `excerpt_regression_tests.rs` also checks source order, callback ownership, and import filtering across languages. `nvim/tests/forge/language_syntax_host.lua` verifies rendered highlight colors in the rebuilt host.

## Troubleshooting

| Symptom | Check |
| --- | --- |
| Highlighting works, but plan capture skips the file | The declaration extension mapping is separate from the syntax registry. |
| Excerpts omit signatures or include body text | Check `SyntaxHandle::outline` and the grammar's body and name fields. |
| A query no longer matches | Compare the grammar's `node-types.json`, field names, and predicates. |
| A jump selects a receiver instead of a member | Check extraction byte columns and LSP encoding conversion. |
| No running client matches | Check the configured filetype, project root, and definition capability. |
| Only proposed references fail | Check saved-plan resolution. Do not add generated source. |
| A fixture downloads many dependencies | Replace them with tiny relative local packages. |
