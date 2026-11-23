---
name: technical-writing
description: >-
  Author and revise technical documents, architectural overviews, RFCs,
  specifications, reference docs, READMEs, API docstrings, and rustdoc. Use when
  creating or editing standalone documentation files or docstrings. Source code
  comments follow the active language skill.
---
# Technical Writing

Write for a technical reader who knows general systems engineering but does not know this codebase. Good technical writing is both **verifiable** and **readable**: it grounds the reader in what the system does and why its boundaries exist before showing raw types or syntax, backs claims with evidence from the implementation, and connects paragraphs by cause and consequence rather than stacking disconnected facts.

## Start with Context and Constraints, Not an Inventory

Open architectural and explanatory documents by grounding the first sentence in the user action or calling subsystem that needs this component (the "so what"—for example, an asset browser displaying preview grids or admins uploading moderation rules), naming the system in the next sentence if it is not already the subject. Match the next sentences to the reader's first question: lead with the architecture and what it buys when the design is straightforward or multi-part, or introduce the single tempting shortcut or concrete operating constraint it addresses when the machinery would otherwise look unnecessary. Never stack multiple rejected designs back-to-back before describing the real system. Describe the architectural split in plain behavioral terms and defer named mechanisms and component types until the sections that define them. Avoid opening with meta signposting ("This page documents..."), a feature laundry list, or a bare inventory of classes.

**Obtuse (bare type inventory):**

> `IngestPipeline` connects `SocketReader` to `BatchWriter` through a bounded `ChunkChannel`.

**Readable (role and operating constraint, then the named objects):**

> The `IngestPipeline` streams incoming socket payloads to batched disk storage within a fixed memory budget. A bounded `ChunkChannel` between the `SocketReader` and the `BatchWriter` absorbs brief write stalls and throttles socket reads once the buffer fills.

## Build Concepts Before Identifiers

Introduce roles, modes, and ownership boundaries before showing raw structs, flags, or code signatures. When readers understand the shape of the system first, the code confirms their mental model instead of forcing them to reverse-engineer it.

- **State ownership before boundary types.** Say what each component owns—not just what actions it performs—before pasting a config struct or wire schema.
- **Pair diagrams with the point they prove.** State the takeaway in the paragraph above a diagram, and show how secondary paths (such as retries, failover, or reloads) reuse the same contract when relevant.
- **Order for a cold reader.** Readers often arrive from search or a cross-link rather than the previous page. Define unfamiliar domain terms on first use, place prerequisites before steps that depend on them, and put each mechanism beside its failure behavior.
- **Give each topic one home.** Link to canonical pages instead of duplicating a contract in two places; split a page only when readers look up, maintain, or link its parts independently.

## Support Claims with Evidence and Explicit Limits

Verify every symbol name, file path, flag, default, and error behavior against the implementation before writing it down. For API docs, inspect implementations, callers, and tests, and document only the caller contract.

Attach costs, prerequisites, and failure conditions directly to the claims they qualify instead of writing promotional language or appending vague disclaimers:

| Unsupported claim | Grounded evidence |
| --- | --- |
| *"This change improves performance."* | Compare latency or throughput before and after the change and state the workload. |
| *"This change reduces complexity."* | Name the coordination steps, lock scopes, or background tasks it removes. |
| *"The architecture is robust."* | Describe the recovery path, how tests verify it, and which failures still lose data. |
| *"This design best fits the requirements."* | Compare the alternatives against the same criteria, including what the chosen option costs. |

## Write Clear, Causal Prose

Connect sentences and paragraphs by cause, contrast, or consequence rather than stacking disconnected facts. Vary how you link cause and effect—split them across two sentences, use an active causal verb (`keeps`, `prevents`, `lets`), or lead with the goal—instead of ending every sentence with `, so ...`.

**Choppy (disconnected facts):**

> `AccountWriter` updates the row in `PostgresStore`. Then it calls `RedisCache::delete`. Readers query `PostgresStore` on a miss.

**Readable (causal flow with the same named objects):**

> The `AccountWriter` calls `RedisCache::delete` only *after* committing to the `PostgresStore`, so a concurrent reader cannot repopulate the cache with stale values before the commit becomes visible.

Follow these baseline conventions:

- **Name the owner in active voice.** Write `` `DraftStore::save` invalidates the cache `` instead of *"The cache is invalidated on save."*
- **Cut filler, slogans, and vague praise or performance adjectives.** Omit meta signposting (*"This page documents..."*), project-name filler (*"Acme uses PostgreSQL..."*), promotional buzzwords, aphorisms (*"Fail fast."*), and vague design praise (*"clean separation"*, *"clean architecture"*, *"seamless"*, *"elegant"*)—state the actual boundary or contract instead. State the concrete work a path avoids (blocking I/O, network round-trips, full-file buffering) instead of writing *"stays fast"* or *"keeps X responsive."*
- **Prefer prose over lists.** Explain collaborating components and ownership in connected paragraphs; reserve bullets and tables for parallel items, ordered steps, or comparisons.
- **Keep types natural in prose.** Use normal articles with type names (`the DraftJournal`, `a sequenced DraftRecord`) and describe struct fields in prose or code blocks rather than inline braces (`Record { a, b }`).
- **Use present indicative for the design and conditional mood for counterfactuals.** Describe implemented and target architecture with definite subjects in the present indicative (`The scheduler runs...`, not `A scheduler runs...`), avoiding generic textbook definitions and words like "intended," "proposed," or "future" except for an active migration affecting callers today. Use conditional mood (`would`, `could`) for rejected alternatives or unmitigated failure scenarios (`a disk failure could lose acknowledged writes`) so they read as counterfactuals rather than live behavior.
- **Write in a calm, developer-to-developer voice.** Explain constraints matter-of-factly as operational realities encountered in practice (`we` is fine when explaining why the design is shaped a certain way). Prefer plain transitions (`and`, `but`, `because`, `instead`) over formal conjunctions (`whereas`, `therefore`, `moreover`), and split clauses into separate sentences only when combining them makes the sentence heavy to read. Avoid theatrical failure triplets ("go down, hang, or return 500s"), invented worst-case numbers, and stacked periodic clauses ("To survive X without Y—and without Z—...").
- **Prefer short, plain sentences and trust the reader.** Stop once the mechanism is clear (`frees the instance and its memory`) instead of restating obvious payoffs (`prevents state from leaking between requests`).
- **Write plain, domain-named headings.** State the point of each section (`## Splitting Local Edits from Background Sync`) rather than generic template slots (`## Components and Ownership`) or class lists. Omit heading numbers and emoji.

## Terminology, Identifiers, and Guarantees

- **Prefer established terms from the code.** Define a specialized term once where it first appears—what kind of entity it is, what it owns, and how it differs from similar concepts.
- **Describe mechanics plainly before naming jargon.** Only introduce vendor or subsystem jargon (such as `visibility lease`, `fencing token`, or `tombstone`) when the codebase defines it or the document reuses it, and state what it actually does on first use (*"temporarily hides the job from other workers until a timeout expires"*) rather than assuming every reader knows that subsystem's vocabulary.
- **Use exact runtime roles.** Prefer concrete roles (`thread`, `task`, `worker`, `system`, `process`, `pipeline`) over vague placeholders (`actor`, `materialization`) unless the codebase itself defines that term.
- **Specify non-obvious identifier contracts.** When an ID, key, hash, handle, URI, or path has a non-obvious scope or invalidation rule (such as a generation ID, lease token, or cache key), state what creates it and what invalidates it. Skip boilerplate for self-explanatory IDs.
- **Qualify durability and safety words.** Treat `stable`, `persistent`, `durable`, `permanent`, and adverbs like `durably`, `safely`, or `reliably` as guarantees, and state the exact boundary where each stops holding (for example, "survives process restarts, resets on schema migration").

## Workflow

1. **Survey existing docs.** Locate pages that already cover the topic or link to it. Update or consolidate affected pages instead of duplicating contracts across files.
2. **Select a document profile.** Match the reader's objective to a profile below and read its template before outlining. Read [tables](references/tables.md) before adding a table and [diagrams](references/diagrams.md) before adding a diagram.
3. **Verify against source.** Check symbols, schemas, config defaults, error paths, and tests in the codebase so every claim rests on live behavior.
4. **Draft for a cold reader.** Open with what the system does and the constraints that shape it, order concepts before raw identifiers, and illustrate non-obvious contracts with minimal working examples.
5. **Proofread in two passes.**
   - *Technical pass:* Verify every claim, qualification, identifier, code snippet, and cross-link. Check whether omitting a cost or failure mode makes a guarantee look unconditional.
   - *Readability pass:* Read the opening and transitions aloud. Cut strawman foils, meta signposting, project-name filler, vague design praise (*"clean separation"*, *"seamless"*), redundant modifiers (*"live consumer delivery"* → *"delivery"*, *"replication consensus"* → *"replication"*), synonym rotation (*"consumers"* → *"subscribers"*), choppy stacked sentences, passive voice that hides the owner, and front-loaded type dumps.

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
