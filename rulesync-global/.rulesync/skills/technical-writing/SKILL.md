---
name: technical-writing
description: >-
  Author and revise technical documents, architectural overviews, RFCs,
  specifications, reference docs, READMEs, API docstrings, and rustdoc. Use when
  creating or editing standalone documentation files or docstrings. Source code
  comments follow the active language skill.
---
# Technical Writing

Write for a technical reader who knows general systems engineering but does not know this codebase. Good technical writing is both **verifiable** and **readable**: it opens with the real constraint or failure mode the design solves, builds the mental model before dumping types or syntax, supports every claim with concrete evidence, and connects its paragraphs so the reader always knows *why* a boundary exists.

Terse is not the same as clear. Stripping away causal explanations, motivating constraints, and synthesis sentences turns documentation into an obtuse checklist. Aim for precision and flow together.

## Lead with the Constraint, Not an Inventory

Readers remember a design when they see what breaks without it. Open architectural and explanatory documents with the conflicting requirements or failure modes that force the structure, then show the mechanism and named components that resolve them. Compare against a realistic naive approach—such as autosaving straight from memory—rather than an unusable strawman no engineer would build, and avoid opening with a feature laundry list, a bare inventory of classes, or meta signposting ("This page documents...").

**Obtuse (front-loaded types, no failure mode):**

> `IngestPipeline` connects `SocketReader` to `BatchWriter` through a bounded `ChunkChannel`.

**Readable (failure mode first, then the same named objects):**

> When storage stalls, an unbounded queue between network ingestion and disk writes exhausts process memory before backpressure reaches the client. Placing a bounded `ChunkChannel` between the `SocketReader` and the `BatchWriter` inside the `IngestPipeline` pauses socket reads automatically once memory reaches capacity.

## Build Concepts Before Identifiers

Introduce roles, lifetimes, modes, and ownership boundaries before showing raw structs, flags, or code signatures. When readers understand the shape of the system first, the code confirms their mental model instead of forcing them to reverse-engineer it.

- **State ownership and lifetimes, then show the boundary type.** Say what each component owns and how long it lives—not just what actions it performs—before pasting a config struct or wire schema.
- **Pair diagrams with the point they prove.** State the takeaway in the paragraph above a diagram, and show how secondary paths (such as retries, failover, or reloads) reuse the same contract when relevant.
- **Order for a cold reader.** Readers often arrive from search or a cross-link rather than the previous page. Define unfamiliar domain terms on first use, place prerequisites before steps that depend on them, and put each mechanism beside its failure behavior.
- **Give each topic one home.** Link to canonical pages instead of duplicating a contract in two places; split a page only when readers look up, maintain, or link its parts independently.

## Support Claims with Evidence and Explicit Limits

Technical documents ask readers to trust conclusions about how a system behaves, why a design satisfies its requirements, and what happens under failure. Back every conclusion—stated or implied—with verifiable evidence from source code, schemas, tests, traces, or measurements, and attach limits directly to the claim instead of appending vague disclaimers.

- **Scope to one main claim:** Cut background facts or sections that do not help establish the document's core conclusion.
- **State non-obvious links:** When evidence does not obviously prove a conclusion, spell out why it holds.
- **Audit omissions:** Omitting a cost makes a recommendation read stronger than the evidence supports; omitting a failure condition makes a guarantee look unconditional.

| Promotional or unsupported claim | Grounded evidence |
| --- | --- |
| *"This change improves performance."* | Compare latency or throughput before and after the change and state the workload. |
| *"This change reduces complexity."* | Name the coordination steps, lock scopes, or background tasks it removes. |
| *"The architecture is robust."* | Describe the recovery path, how tests verify it, and which failures still lose data. |
| *"This design best fits the requirements."* | Compare the alternatives against the same criteria, including what the chosen option costs. |

Verify every symbol name, file path, flag, default, and error behavior against the implementation before writing it down. For API docs, inspect implementations, callers, and tests, and document only the caller contract.

## Write Causal, Active Prose

Connect sentences and paragraphs by cause, contrast, or consequence. Open each paragraph with its point, substantiate it in the middle, and close on the invariant or transition that leads into the next paragraph.

**Choppy (disconnected facts):**

> `AccountWriter` updates the row in `PostgresStore`. Then it calls `RedisCache::delete`. Readers query `PostgresStore` on a miss.

**Readable (causal flow with the same named objects):**

> The `AccountWriter` calls `RedisCache::delete` only *after* committing to the `PostgresStore`, so a concurrent reader cannot repopulate the cache with stale values before the commit becomes visible.

### Common Prose Fixes

| Pattern to avoid | Before | After |
| --- | --- | --- |
| **Meta signposting** | *"This page documents the caching layer and breaks it into three sections."* | *"The response cache stores compiled query results in memory for 60 seconds."* |
| **Over-generalized subject** | *"A browser-based document editor updates memory on every keystroke."* | *"The document editor updates its in-memory state on every keystroke."* |
| **Abstract gerund solution** | *"Persisting drafts locally closes that gap."* | *"To close that gap, the editor persists drafts locally."* |
| **Abstract framing** | *"Every edit crosses two durability boundaries in strict order."* | *"Saving happens in two stages: a local journal write, followed by an asynchronous upload."* |
| **Anthropomorphic phrasing** | *"The view and worker never talk; they meet through the journal."* | *"Local persistence and background upload communicate strictly through the `DraftJournal`."* |
| **Project-name filler** | *"Acme uses PostgreSQL for persistence and Redis for rate limiting."* | *"Account records live in PostgreSQL; rate limits track counters in Redis."* |
| **Passive hiding the owner** | *"The cache is invalidated on save."* | *"`DraftStore::save` invalidates the cache entry for that document."* |
| **Difficulty tiers** | *"This advanced guide is not for trivial setups."* | *"Assumes familiarity with async Rust and Redis."* |
| **Generic nouns** | *"Coordinate two important things across the boundary."* | *"Coordinate retry budgets and circuit-breaker state across the gateway."* |
| **Aphorisms and slogans** | *"Fail fast."* | *"Validate config at startup so a typo aborts launch instead of failing mid-request."* |
| **Figurative verbs / buzzwords** | *"Our battle-tested pipeline unlocks seamless workflows."* | *"The queue retains unfinished jobs and reassigns them when a worker disconnects."* |

Additional composition rules:

- **Prose over component lists:** Describe collaborating components and ownership in connected paragraphs rather than stacked bullet lists; reserve bullets for parallel items, ordered steps, or quick comparisons.
- **Natural articles with type names:** Write `"through the DraftJournal"` or `"a sequenced DraftRecord"` rather than dropping articles as if code types were people's names (`"through DraftJournal"`), and use plain nouns (`"the journal"`, `"the worker"`) on nearby repeats.
- **No inline struct braces in prose:** Describe fields in prose or a fenced code block instead of embedding `Record { a, b }` inside a sentence.
- **One governing tense:** Describe implemented and target architecture in the present tense. Avoid "intended," "proposed," or "future" except for an active migration that changes how callers use the system today.
- **No decoration:** Omit emoji in headings and checklists. Use the serial comma.
- **Subject near verb:** Keep subjects close to verbs, modifiers beside what they modify, and pronouns near unambiguous antecedents. Put new or emphatic information at the end of the sentence.

### Headings

Write headings that capture **the point of the section** in plain domain terms so a reader scanning the outline understands the design without opening every paragraph.

- **State the design point, not the template slot or class list:** Prefer `## Splitting Local Edits from Background Sync` over generic buckets (`## Components and Ownership`) or bare object lists (`## EditorView, DraftJournal, and SyncWorker`).
- **Use plain words over mechanical or theatrical phrasing:** Prefer `## Recording and Syncing a Draft` over `## Journal Write and Sync Flow` or `## From Keystroke to Server Confirmation`, and `## Outages and Storage Quotas` over `## Failure Boundaries`.
- **Never number headings:** Omit `## 1. ...` prefixes so reordering sections never breaks numbering or clutters tables of contents.

## Terminology, Identifiers, and Guarantees

- **Prefer established terms from the code.** Define a specialized term once where it first appears—what kind of entity it is, what it owns, and how it differs from similar concepts.
- **Avoid vague placeholders.** Reserve `projection` for coordinate spaces. Replace `materialization` and `actor` with exact roles (`thread`, `task`, `worker`, `system`, `process`, `pipeline`, `load`, `populate`).
- **Specify identifier contracts.** For every public or persistent ID, key, hash, handle, URI, or path, state what it identifies, who creates and resolves it, how long it stays valid, and what invalidates it.
- **Qualify durability words.** Treat `stable`, `persistent`, `durable`, and `permanent` as guarantees and state the exact boundary where each stops holding (for example, "survives process restarts, resets on schema migration").

## Workflow

1. **Survey existing docs.** Locate pages that already cover the topic or link to it. Update or consolidate affected pages instead of duplicating contracts across files.
2. **Select a document profile.** Match the reader's objective to a profile below and read its template before outlining. Read [tables](references/tables.md) before adding a table and [diagrams](references/diagrams.md) before adding a diagram.
3. **Verify against source.** Check symbols, schemas, config defaults, error paths, and tests in the codebase so every claim rests on live behavior.
4. **Draft for a cold reader.** Lead with constraints and mental models, order concepts before raw identifiers, and illustrate non-obvious contracts with minimal working examples.
5. **Proofread in two passes.**
   - *Technical pass:* Verify every claim, qualification, identifier, code snippet, and cross-link. Check whether omitting a cost or failure mode makes a guarantee look unconditional.
   - *Readability pass:* Read the opening and transitions aloud. Cut meta signposting, project-name filler, choppy stacked sentences, passive voice that hides the owner, and front-loaded type dumps.

## Document Profiles

| Reader objective | Profile |
| --- | --- |
| Understand a system or subsystem architecture | [Architectural overview](templates/architectural-overview.md) |
| Propose or evaluate a design change or strategy | [RFC and proposal](templates/rfc.md) |
| Record a significant design decision and its tradeoffs | [Architecture decision record](templates/adr.md) |
| Understand causal mechanics or why a behavior occurs | [Technical explanation](templates/explanation.md) |
| Execute a procedure or runbook to reach one result | [How-to guide](templates/how-to.md) |
| Look up an exhaustive API, CLI, schema, or error catalog | [Reference](templates/reference.md) |
| Define or relax an interoperability or format contract | [Specification](templates/specification.md) |
| Learn a technology by building one working project | [Tutorial](templates/tutorial.md) |
| Predict how a public API, docstring, or endpoint behaves | [API documentation](templates/api-documentation.md) |
