# PowerShell command grammar corrections

Harness must distinguish literal command arguments from executable expressions before
applying permission rules. Parse errors reject the complete command, including in YOLO.

This package preserves `tree-sitter-powershell` 0.26.4 from upstream revision
`d398441825243b00e317e87e1829b9d6a3e54ce0`. The upstream project is
[airbus-cert/tree-sitter-powershell](https://github.com/airbus-cert/tree-sitter-powershell).
`grammar.patch` records the source changes. The MIT license and original Rust binding
and build remain included.

## Grammar changes

- Parse bare, quoted, and expression operands in command argument arrays. Commas
  accept horizontal whitespace before them and ordinary grammar whitespace or comments
  after them. Keep nested expressions as syntax nodes.
- Keep escaped punctuation inside generic arguments. An escaped semicolon or comma
  cannot introduce a second command or array operand. An escaped CR followed by LF
  inside a bare argument still leaves the next command visible.
- Parse digit-bearing command options, `--`, and attached `=` values. Attached quoted
  values and substitutions retain their own syntax nodes.
- Parse parenthesized expressions after variable arguments independently from method
  argument lists. Actual method calls retain their original invocation nodes.
- Give expandable-string content priority over comment tokens after interpolation.

The related upstream reports include
[command parameters](https://github.com/airbus-cert/tree-sitter-powershell/issues/59),
[adjacent expressions](https://github.com/airbus-cert/tree-sitter-powershell/issues/48),
and [interpolation followed by `#`](https://github.com/airbus-cert/tree-sitter-powershell/issues/58).

## Regeneration and verification

Generated files use Tree-sitter CLI 0.26.8 and language ABI 15. Run from this directory:

```nu
tree-sitter generate --abi 15
```

Refresh this package's hashes in `../grammar-manifest.json`. The repository asset
check verifies that inventory. Run Harness's `permissions::` and `backend::approval::`
tests with the release profile and shared target directory, then rebuild Forge and
exercise a fresh host. `permissions/shell_tests.rs` covers accepted syntax, complete
executable discovery, exact original byte ranges, independent policy decisions,
and rejected syntax across PowerShell, Bash, Zsh, and Nushell.
