---
name: academic-pseudocode
description: >-
  Write and revise high-level, compact academic pseudocode. Use when authoring
  Algorithm blocks in implementation plans or when the user requests academic
  pseudocode. Do not use for native implementations or prose-only Change blocks.
---
# Academic Pseudocode

Express the complete algorithm at the highest useful level and as compactly as possible. Preserve behavior, not implementation mechanics.

## Writing rules

Use structured, language-independent notation. Free-form prose is not valid in statements, conditions, arguments, or assignment values. Keep explanations in `#` comments or declaration descriptions.

- Combine assignments and control flow with named abstract operations such as `spawn_square(...)` or `select_earliest(...)`. Prefer one clear operation over several mechanical steps.
- Use `←` for assignment and `=` for equality consistently. Indent the bodies of `if`, `otherwise`, `for each`, and `while`. Use `return` and `continue` when needed.
- Write language-independent operations. Omit native-language braces, semicolons, redundant types, allocation syntax, and other incidental implementation details.
- Prefer `for each` over indexed iteration unless the index affects behavior.
- Use abstract collections and standard operations without implementing their internals. State ordering, uniqueness, or multiple-value properties when needed.
- Summarize routine work through abstract operation calls. Expose decisions, state transitions, bounds, and failure paths whenever abstraction would make the result ambiguous.
- Remove redundant passes and intermediate structures. Prefer the shorter correct algorithm, but never shorten it by hiding required behavior.

## Structured notation

Put each complete statement on one line. Use assignments, operation calls, indented control flow, `return`, `break`, and `continue`. Use `break` and `continue` only inside loops. Bare identifiers and prose such as `process candidate` are not operation statements: write `process(candidate)`.

Expressions contain identifiers, qualified references (`player.position`, `Store::new`), calls, numeric and quoted string literals, `none`, `true`, `false`, or `null`. Combine them with arithmetic, comparison, logical (`and`, `or`, `not`), membership (`in`, `∈`, `∉`), or set (`∪`, `∩`) operators. Use comma-separated tuples `(x, y)`, lists `[player, speed]`, and indexing `items[index]`. Assign only to a reference or indexed element. Write loop bindings as `for each item in items` or `for each (key, value) in entries`.

Abstract operations describe behavior without requiring repository functions. Use lowercase descriptive names, and do not invent declarations or add them to `Calls` merely to make pseudocode look executable. Concrete references belong in `Calls`, `Constructs`, and `Accesses` when known. Preserve selection criteria and failure behavior in explicit arguments or control flow rather than hiding them behind vague operation names.

## Explicit construction

Write each constructed value as `Type(field = value, ...)`, using the declared type name and explicit field values. Write zero-field markers as `Type()`. Use the exact qualified factory name for factory construction, such as `Store::new(config = config)`. Keep construction separate from operations that enqueue, spawn, insert, or publish the value. Do not hide construction inside phrases such as "queue a shot with damage".

When an Algorithm has a `Constructs` list, give every listed target an explicit construction site with the same name. Ordinary helper calls may use positional arguments. Reusing an existing value or shared asset does not construct it again. Keep Change blocks focused on their behavioral delta rather than repeating a full algorithm.

The values and the operation that consumes them remain separately visible:

```text
projectile ← Projectile(damage = 1, previous_position = spawn_position)
velocity ← Velocity(linear = 560 × direction)
lifetime ← Lifetime(remaining = 1.2)
combat_entity ← CombatEntity()
queue_entity(position = spawn_position, components = [projectile, velocity, lifetime, combat_entity])
```

## Contract and review

Preserve meaningful domain identifiers, numeric limits, tie-breaking, saturation, ownership, and deferred effects. Do not repeat inputs or results already declared by an enclosing signature. Keep standalone input and output descriptions only when needed to understand the algorithm.

Check every branch and state change against the requested behavior, then remove details that do not affect correctness. Do not translate native code line by line.

The example makes ordering and the empty case explicit while abstracting selection:

```text
candidate ← select_earliest(items, eligible = is_eligible, tie_break = identifier)
if candidate = none
    return
process(candidate)
```
