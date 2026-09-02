# Code Comments Profile

*Read when authoring, reviewing, or revising inline or block comments within source code.*

## Controlling Claim

Establish why a non-obvious local constraint, algorithmic invariant, or safety precondition exists so future maintainers do not introduce regressions during refactoring.

## Triggering Criteria: When to Comment

Prefer semantic types, clear variable names, and explicit control flow over comments. Add a source comment only when correctness depends on context the syntax cannot convey:

- **Non-Obvious Domain Rules:** Business logic, tax calculations, or domain invariants not evident from the algorithm.
- **Safety and Memory Bounds:** Explicit preconditions, alignment assumptions, unsafe blocks, or pointer arithmetic constraints.
- **Concurrency Protocols:** Lock ordering, memory barrier requirements, atomic ordering invariants, or thread ownership transfers.
- **Hardware and External API Quirks:** Workarounds for vendor bugs, non-standard HTTP responses, or platform-specific driver behaviors.
- **Performance Tradeoffs:** Non-intuitive data structures or bitwise manipulations chosen to satisfy critical latency or memory bounds.

## Composition Rules

- **Placement:** Place the comment immediately above the statement or branch that enforces the constraint.
- **Invariant and Failure Mode:** State the exact invariant preserved and the specific failure mode prevented (such as `// Must flush write buffer before releasing lock to prevent stale reads by worker threads.`).
- **Omit Syntax Narration:** Never restate what the syntax expresses (avoid `// Increment counter by 1`).
- **Structure:** Use single-line comments for single-constraint explanations. Use block comments only when documenting multi-precondition safety rules or complex synchronization sequences.

## Maintenance and Refactoring

- Delete or update the comment immediately when code refactoring makes the constraint explicit via the type system or API boundaries.
- Re-verify comment accuracy after modifying nearby branches, variables, or error handling.

## Completion Check

A maintainer reading the comment should immediately understand what failure occurs if the enforcing code is modified, reordered, or deleted.
