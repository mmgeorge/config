---
name: academic-pseudocode
description: >-
  Write and revise high-level, compact academic pseudocode. Use when authoring
  Algorithm blocks in implementation plans or when the user requests academic
  pseudocode. Do not use for native implementations or prose-only Change blocks.
---
# Academic Pseudocode

Express the complete algorithm at the highest useful level and as compactly as
possible. Preserve behavior, not implementation mechanics.

## Writing rules

These rules adapt Brown CS16's pseudocode guidance into a self-contained local
reference. Apply them directly without fetching an external guide.

- Combine assignments and control flow with concise descriptive operations.
  Prefer one clear operation over several mechanical steps.
- Use `←` for assignment and `=` for equality consistently. Indent the bodies of
  `if`, `otherwise`, `for each`, and `while`. Use `return` and `continue` when needed.
- Write language-independent operations. Omit native-language braces, semicolons,
  redundant types, allocation syntax, and other incidental implementation details.
- Prefer `for each` over indexed iteration unless the index affects behavior.
- Use abstract collections and standard operations without implementing their
  internals. State ordering, uniqueness, or multiple-value properties when needed.
- Summarize routine work in words. Expose decisions, state transitions, bounds,
  and failure paths whenever abstraction would make the result ambiguous.
- Remove redundant passes and intermediate structures. Prefer the shorter correct
  algorithm, but never shorten it by hiding required behavior.

## Explicit construction

Write each constructed value as `Type(field = value, ...)`, using the declared type
name and explicit field values. Write zero-field markers as `Type()`. Use the exact
qualified factory name for factory construction, such as `Store::new(config = config)`.
Keep construction separate from operations that enqueue, spawn, insert, or publish
the value. Do not hide construction inside phrases such as "queue a shot with damage".

When an Algorithm has a `Constructs` list, give every listed target an explicit
construction site with the same name. Ordinary helper calls may use positional
arguments. Reusing an existing value or shared asset does not construct it again.
Keep Change blocks focused on their behavioral delta rather than repeating a full algorithm.

The values and the operation that consumes them remain separately visible:

```text
projectile ← Projectile(damage = 1, previous_position = spawn_position)
velocity ← Velocity(linear = 560 × direction)
lifetime ← Lifetime(remaining = 1.2)
combat_entity ← CombatEntity()
queue entity at spawn_position with projectile, velocity, lifetime, combat_entity
```

## Contract and review

Preserve meaningful domain identifiers, numeric limits, tie-breaking, saturation,
ownership, and deferred effects. Do not repeat inputs or results already declared
by an enclosing signature. Keep standalone input and output descriptions only
when needed to understand the algorithm.

Check every branch and state change against the requested behavior, then remove
details that do not affect correctness. Do not translate native code line by line.

The example makes ordering and the empty case explicit while abstracting selection:

```text
candidate ← earliest eligible item, breaking ties by identifier
if candidate = none
    return
process candidate
```
