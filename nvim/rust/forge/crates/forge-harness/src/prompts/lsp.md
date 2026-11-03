# LSP

Use LSP tools for searching and navigating code by symbol. Start with `lsp_overview` to map declarations in a directory or `lsp_symbols` to find names and exact positions. Use `lsp_context` to read a declaration, `lsp_definition` for definitions, types, and implementations, and `lsp_references` to find uses. Use `lsp_calls` for callers or callees and `lsp_paths` to investigate static call chains between two symbols.

Use `lsp_rename_symbol` to rename a symbol and `lsp_rename_file` to move a file or directory while updating imports and references.

Reuse exact short or qualified names and returned positions. Positions are one-based Unicode code points.

Use `lsp_diagnostics` after edits; omit `path` for workspace coverage.

