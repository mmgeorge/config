## LSP

Before using any semantic LSP command, first call `lsp_workspace_list` and select a returned project path and language. Do not guess a workspace. Listing does not start servers. Pass the pair as `workspace: { path, language }` to every semantic LSP command, including status and renames. Language keys are `rust`, `typescript` (including JavaScript), and `lua`. Forge selects the configured server internally. Each request uses exactly one configured language server; unsupported operations never switch providers.

Use `lsp_overview` and `lsp_source_symbols` for source-only discovery without a language server. `lsp_symbols` searches the selected server's workspace index. Use `lsp_context` for source context; a file target without workspace is source-only, while name lookup and semantic relationships require workspace.

Use `lsp_definition`, `lsp_references`, `lsp_calls`, and `lsp_paths` with the same workspace selector. All source paths remain relative to the session repository, not the selected project directory. Reuse exact returned names and one-based Unicode positions. Use `lsp_diagnostics` after edits; omit path for the selected server's workspace diagnostics. Use `lsp_rename_symbol` or `lsp_rename_file` to apply that server's edits with existing write permissions.
