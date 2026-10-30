# Forge Algorithm grammar

This repository owns the statement grammar in `grammar.js`. Tree-sitter builds typed
statements and structured expressions for academic pseudocode. Indentation remains in the
source coordinates. `syntax/algorithm.rs` validates block nesting, branch placement,
and loop control at plan submission, while the syntax service can highlight incomplete drafts.

Invocation nodes preserve qualified targets and distinguish named fields from positional
arguments. Write construction as `Projectile(damage = 1, previous_position = position)`
or `CombatEntity()` for a marker. The plan validator matches each `Constructs` target
against these nodes and requires named fields for constructor arguments. Nested calls
remain visible to validation. Ordinary helper calls can use positional arguments.

Statements are assignments to references or indexed elements, calls, control flow, returns,
and loop control. Expressions combine references, calls, literals, tuples, lists, and indexing
with unary or binary operators. Separate tuple, list, and argument elements with commas.
Each complete statement occupies one line. Free-form prose is invalid, including inside
conditions and argument values. Put explanations in standalone or trailing `#` comments.
Quoted strings remain literal data. Lowercase abstract operation calls do not require
repository declarations or implicit `Calls` entries.

Regenerate from this directory with Tree-sitter CLI 0.27.0, retaining ABI 14:

```text
tree-sitter generate --abi 14
```

Commit the grammar, generated parser, node metadata, and headers together. Refresh the
`algorithm` entry in `../manifest.json` and the native query manifest after changing assets.
The runtime uses the generated C parser directly and does not require a Neovim parser install.
