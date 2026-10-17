# Forge Algorithm grammar

This repository owns the statement grammar in `grammar.js`. Tree-sitter builds typed
statements and expression tokens for academic pseudocode. Indentation remains in the
source coordinates. `syntax/algorithm.rs` validates block nesting, branch placement,
and loop control at plan submission, while the syntax service can highlight incomplete drafts.

Invocation nodes preserve qualified targets and distinguish named fields from positional
arguments. Write construction as `Projectile(damage = 1, previous_position = position)`
or `CombatEntity()` for a marker. The plan validator matches each `Constructs` target
against these nodes and requires named fields for constructor arguments. Nested calls
remain visible to validation. Ordinary helper calls can use positional arguments.

Regenerate from this directory with Tree-sitter CLI 0.26.8:

```text
tree-sitter generate --abi 14
```

Commit the grammar, generated parser, node metadata, and headers together. Refresh the
`algorithm` entry in `../manifest.json` and the native query manifest after changing assets.
The runtime uses the generated C parser directly and does not require a Neovim parser install.
