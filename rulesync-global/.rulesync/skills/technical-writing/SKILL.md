---
name: technical-writing
description: >-
  Write and revise architecture docs, proposals and RFCs, tutorials and
  runbooks, specifications, READMEs, API docs, docstrings, and rustdoc. Use
  when creating or editing standalone documentation files or interface
  docstrings.
---
# Technical Writing

Write for an engineer who is new to this codebase. Build intuition for how the system works: ground every claim in the implementation, explain what the system does and why its boundaries exist, and connect paragraphs by cause and consequence.
## Support Claims with Evidence

Replace vague self-praise (*"clean separation"*, *"seamless"*, *"robust"*) and unsupported claims about performance, simplicity, or tradeoffs with the concrete mechanism, evidence, and explicit limits. Treat words like `stable`, `persistent`, `durable`, `safely`, and `reliably` as guarantees, stating where each stops holding (for example, *"survives process restarts, resets on schema migration"*).

| Unsupported claim or praise                 | Grounded prose                                                                                                                    |
| ------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| *"Batching improves performance."*          | *"Batching packs multiple events into a single disk flush, amortizing sync overhead."*                                            |
| *"Generating bindings reduces complexity."* | *"Generating bindings from the schema removes hand-maintained mappings, and allows for catching field mismatches at build time."* |
| *"The draft journal is robust."*            | *"Unconfirmed drafts survive tab crashes by replaying from local storage."*                                                       |
| *"WebAssembly best fits plugins."*          | *"WebAssembly isolates plugin memory inside the server process without spawning an OS process per request."*                      |
| *"The gateway offers a clean separation."*  | *"The gateway terminates TLS and enforces rate limits while forwarding decoded requests to stateless workers."*                   |
## Write Clear, Causal Prose

- **Write in a calm, developer-to-developer voice.** Explain constraints matter-of-factly as operational realities; skip meta-signposting (*"This page documents..."*), slogans, and obvious payoffs.
- **Keep paragraphs focused and prose-first.** Split heavy clauses into short sentences, and explain related components in subsequent paragraphs—using bullets or tables for parallel items, ordered steps, or comparisons.
- **Prefer active voice.** Name the acting component (*"The draft store invalidates the cache on save"*) rather than hiding ownership in passive voice (*"The cache is invalidated on save"*).
- **Prefer present indicative for the design, and conditional mood for counterfactuals.** Describe implemented and target architecture with definite subjects in the present indicative (`The scheduler runs...`). Use conditional mood (`would`, `could`) for rejected alternatives or unmitigated failure scenarios.

**Choppy (disconnected facts):**

> `BuildRunner` compiles into `StagingDir`. Then it calls `Path::rename`. `DevHost` loads the binary from `ActiveDir`.

**Readable (causal flow in plain terms):**

> The build runner compiles each update into a staging directory and renames it into place once compilation finishes. That atomic swap keeps the running host from opening a half-written binary.

## Workflow

1. **Survey docs and source.** Locate pages that already cover or link to the topic so you update or consolidate them, and inspect the relevant implementations, schemas, defaults, error paths, and tests.
2. **Select a profile.** Match the reader's goal to a profile below and read its template before outlining. Read [tables](references/tables.md) before adding a table and [diagrams](references/diagrams.md) before adding a diagram.
3. **Draft and verify.** Follow the selected profile and the principles above, then verify every symbol, file path, snippet, and cross-link against the repository.

## Profiles

| Reader goal                                             | Profile                                     |
| ------------------------------------------------------- | ------------------------------------------- |
| Understand a system or subsystem architecture           | [Architecture](templates/architecture.md)   |
| Propose or evaluate a design change or strategy         | [Proposal](templates/proposal.md)           |
| Follow a procedure or task to reach one result          | [Tutorial](templates/tutorial.md)           |
| Define a format, protocol, or interoperability contract | [Specification](templates/specification.md) |
| Document an API, CLI, config, docstring, or endpoint    | [API](templates/api.md)                     |
