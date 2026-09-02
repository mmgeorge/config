# Reference Profile

*Read when authoring or revising comprehensive lookup documentation for APIs, schemas, CLI commands, configuration surfaces, protocols, or error catalogs.*

## Controlling Claim

Establish that the document describes its declared interface completely, accurately, and in a predictable hierarchy that practitioners can rapidly navigate during implementation and debugging.

## Structural Obligations

Reference documentation must follow the structural organization of the underlying interface:

### 1. Scope, Version, and Authoritative Source
- State the exact software version, package, schema version, or RFC standard covered.
- State the authoritative source of truth (such as a source crate, OpenAPI spec, or protobuf definition).

### 2. Namespace and Module Hierarchy
- Mirror the module structure, namespaces, or command groups directly in heading levels.
- Provide a summary table or index at the top of each major category for rapid lookup.

### 3. Exhaustive Member Specification
For every declared member (function, field, configuration key, flag, or error code):
- **Canonical Identifier:** Full symbol path or command syntax.
- **Type and Constraints:** Data type, accepted value range, string encoding, or regex pattern.
- **Optionality and Defaults:** Explicitly mark as `Required`, `Optional (Default: <value>)`, or `Deprecated (Use: <alternative>)`.
- **Behavior Contract:** Concrete description of runtime effects, side effects, and state mutations.
- **Errors and Exit Codes:** Exhaustive enumeration of possible error returns, exceptions, or failure codes.

### 4. Verified Minimal Examples
- Provide a minimal, syntax-accurate example for each member or configuration block.
- Verify examples against the same authoritative compiler, schema, or test suite that produces the reference.

## Exclude by Default

- Do not include narrative tutorials, design history, or persuasive arguments. Link the relevant how-to guide, explanation, or RFC.
- Do not omit obscure or advanced members. Reference documentation must provide exhaustive coverage within its declared scope.

## Completion Check

A practitioner should be able to locate any declared member immediately, identify its accepted inputs, return guarantees, default values, and failure modes, and distinguish documented contract facts from optional examples.
