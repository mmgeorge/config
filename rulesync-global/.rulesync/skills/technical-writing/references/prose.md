# Prose

*Read when drafting technical prose or when existing prose feels vague, repetitive, indirect, or assembled through local edits.*

## Contents

- Plan the passage privately
- Test contribution before placement
- Preserve explicit goals
- State the necessary why
- Use hidden conclusions to select evidence
- Lead with the operative mechanism
- Expose concrete mechanics
- Complete relational predicates
- Keep parallel structures on one axis
- Make topic sentences concrete
- Name concepts before qualifying them
- Attach properties to their source
- Match verbs to responsibility
- Name implementations only when relevant
- Audit complete sentence semantics
- Match voice to the relationship
- Match specificity to the claim
- Use domain language before internal abstractions
- Control references
- Prove non-obvious representations
- Keep planning structure out of the prose
- Preserve paragraph continuity
- Reflow after every edit
- Rewrite the whole paragraph

## Plan the Passage Privately

Use only the fields needed for the passage:

```text
Point: What the passage establishes
Contribution: How it advances the controlling claim or a required dependency
Pressure or requirement: What selects or requires this behavior
Mechanism and evidence: What concretely happens
Warrant: Why the mechanism establishes the point
Consequence: What follows
Hidden conclusion: What qualitative judgment the evidence should earn, if any
```

The point must state a fact about the subject. “The API records commands before workers execute them” gives the paragraph a point. “The boundary is strict” merely supplies a conclusion without evidence.

Keep planning language out of the document. Do not write “the key point is,” “the important distinction,” or “it is worth noting.” Write the architecture that follows those phrases.

Useful explicit points define a requirement, record a decision, explain a mechanism, identify an owner, establish a constraint, supply evidence and its warrant, or name a concrete consequence.

## Test Contribution Before Placement

Before rewriting or relocating a sentence, state what required information it contributes. Count a definition, decision, mechanism, premise, evidence, warrant, boundary, or consequence as information. Broad topical relevance does not count.

Delete the sentence when removing it loses nothing the reader needs. Only after it passes this test should placement follow from the section whose point depends on it. Do not move redundant prose into a more plausible section and preserve it by inertia.

For example, a validation section does not need “Workers later execute accepted jobs” when a later execution section already establishes that fact. If validation must complete before execution, state the actual invariant: “The gateway records each accepted job before a worker may claim it.”

## Preserve Explicit Goals

A goal states what the system must enable. Do not replace it with lower-level mechanics.

- Goal only: “Users and automated jobs must be able to submit the same import through every ingestion client.”
- Mechanism only: “The web application, CLI, and scheduler submit the same manifest.”
- Combined: “Users and automated jobs must be able to submit the same import through the web application, CLI, or scheduler. All three paths therefore produce the same validated manifest.”

The combined version tells readers what success means and how the design reaches it. The mechanism supports the goal rather than making the reader reconstruct it.

## State the Necessary Why

Evidence does not explain itself. State why a mechanism satisfies its pressure or why an example supports the point when that relationship carries meaningful design information.

- Evidence without warrant: “The editor, CLI, and automation modify the same source file.”
- Evidence with warrant: “The editor, CLI, and automation modify the same source declarations, so a schema migration updates one authoritative representation rather than three synchronized models.”

The second sentence still lets readers infer maintainability. It does not force them to invent the causal relationship.

## Use Hidden Conclusions to Select Evidence

- Telling: “The boundary between the API and workers is strict.”
- Showing: “The API validates and records commands. Workers execute only commands already committed to the queue.”

The second version exposes the exclusive responsibilities that create the boundary. Apply the same test to other qualities:

- Show reliability through failure and recovery behavior.
- Show flexibility through the dimensions that can vary.
- Show simplicity through state, coordination, or steps removed.
- Show performance through work avoided or measured latency.
- Show usability through the concrete workflow and feedback loop.

Use the intended hidden conclusion to decide which evidence belongs in the passage. Do not append “this improves flexibility,” “this keeps the system simple,” or another generic evaluation merely to make the paragraph sound complete.

## Lead with the Operative Mechanism

State how the design works before describing behavior it avoids.

- Indirect: “The editor never guesses which file owns a resolved property.”
- Direct: “The asset catalog locates the package export, while evaluator provenance locates each contributing declaration and source span.”

Use negative framing only when the prohibition defines a contract, invariant, safety boundary, or relevant comparison.

## Expose Concrete Mechanics

- Low value: “The scheduler combines priorities and limits to make dispatch decisions.”
- Specific: “The scheduler orders ready jobs by tenant priority, then rejects dispatches that would exceed that tenant's concurrency limit.”

- Low value: “The compiler stores that result for later use.”
- Specific: “The compiler serializes each verified query plan into a cache entry keyed by schema revision.”

Name the actor, operation, inputs, failure condition, and output when they matter to the point. Put the responsible actor in the subject position, its operation in the verb, and the affected value or subsystem in the object.

## Complete Relational Predicates

Treat `agrees`, `matches`, `supports`, `handles`, `resolves`, `preserves`, `accepts`, `compatible`, `valid`, and similar words as compressed claims. Expand the relation until the reader can identify the participants, condition or operation, evaluator, result, and relevant failure behavior.

- Compressed: “The policies must agree before deployment.”
- Complete: “The deployment validator rejects a request when its selected region does not appear in the tenant's allowed-region set.”

When explaining a downstream requirement, name the operation that consumes the information rather than a broad area such as UI, runtime, administration, or tooling.

## Keep Parallel Structures on One Axis

Make every item in a list, coordinated clause, table row, or sibling phrase answer the same governing question at the same architectural level. Keep grammar parallel because grammatical symmetry often reveals conceptual symmetry, but do not accept matching grammar as proof that the concepts belong together.

Move a policy, mechanism, consumer, or consequence into its own sentence when it governs the other members rather than belonging beside them.

- Mixed: “The platform manages invoices, receipts, stored documents, and retry policies.”
- Parallel: “The platform manages invoices, receipts, and stored documents. Retry policies govern failed deliveries.”

## Make Topic Sentences Concrete

A topic sentence must remain understandable before the details that follow it. Name the real subject and the operation, distinction, or relationship the paragraph establishes. Do not compress the conclusion into abstractions that later sentences must decode.

Check whether the subject actually performs the verb, whether a concrete category can replace an abstract proxy, and whether the sentence tells readers why the paragraph exists.

- Compressed: “Compaction executes at the lifecycle stage that owns its segments.”
- Concrete: “Log compaction combines several immutable segments into fewer segments while preserving the latest value for each key.”

## Name Concepts Before Qualifying Them

Make the head noun identify the concept before adding modifiers. A modifier may distinguish a variant, identify a meaningful state, or state a required property, but it must not supply meaning that the noun itself lacks.

Inspect phrases built around broad nouns such as `result`, `data`, `model`, `state`, `representation`, `document`, `system`, or `process`. These words remain valid when the category itself matters or one immediate antecedent identifies the referent. Replace them when readers need the modifiers to infer what the phrase names.

Apply three checks:

1. **Identity:** Does the head noun identify what the value or subsystem represents?
2. **Attachment:** Does each modifier logically describe that noun?
3. **Contribution:** Does each modifier change a claim the document needs?

## Attach Properties to Their Source

Attach a property to the concept, operation, or boundary that provides it. Do not describe an input as `lossless`, `safe`, `durable`, `validated`, or `consistent` when the intended claim concerns how another operation preserves, verifies, stores, or coordinates that input.

Move a preservation or quality guarantee onto the mechanism that enforces it. Name the preserved information, rejected condition, storage boundary, or coordination rule when the guarantee matters to the argument.

## Match Verbs to Responsibility

Treat operational verbs as architectural claims. Verbs such as `owns`, `keeps`, `stores`, `caches`, `loads`, `evaluates`, `resolves`, `writes`, and `mutates` assign work or lifecycle responsibility to their subject. Verify that responsibility against the design before using the verb.

When a subsystem only consumes or projects another subsystem's output, use verbs such as `displays`, `reads`, `receives`, or `uses`, and name the producer or owner when that boundary matters.

Map these roles privately for disputed sentences:

```text
Producer: Which subsystem creates the value
Owner: Which subsystem controls its lifecycle
Consumer: Which subsystem reads or displays it
Mutation authority: Which subsystem may change the authoritative form
Lifetime: How long the value survives
```

## Name Implementations Only When Relevant

Name a language, framework, format, vendor, or execution environment only when the claim depends on a capability specific to it, crosses its boundary, requires the reader to operate it, or uses it as evidence. Once the document establishes implementation context, use the architectural actor for ordinary behavior.

- Incidental: “The Python delivery worker retries failed messages.”
- Relevant boundary: “The schema generator reads Python type annotations and emits TypeScript client bindings.”

Removing an implementation name should not change an implementation-independent claim.

## Audit Complete Sentence Semantics

Review nouns, modifiers, and predicates together. A sentence can use recognized terms and remain architecturally wrong because its modifier attaches a guarantee to the wrong concept or its verb assigns work to the wrong subsystem.

Before:

> The dashboard stores each lossless log document beside the temporary result generated from its matching events.

After:

> The dashboard displays each log document beside the incident timeline produced by the analysis service. Timeline exports preserve event ordering and annotations.

The revision replaces `temporary result` with the value's domain name, assigns analysis to its producer, and attaches the preservation guarantee to export behavior.

Before:

> The API retains the validated configuration model, the ephemeral result of combining every inherited setting.

After:

> The configuration service combines inherited settings into the effective deployment configuration. The API returns that configuration to clients.

The revision separates configuration ownership from API delivery and replaces lifecycle modifiers with the concept the service produces.

## Match Voice to the Relationship

Prefer active voice when a sentence describes an operation. Use linking verbs when a sentence defines identity, classification, composition, or stable state.

- Definition: “A queue is an ordered collection of pending work.”
- Operation: “The scheduler removes the next job from the queue.”
- Avoidable passive: “The next job is removed from the queue by the scheduler.”

Before replacing `is` or `are`, identify what the sentence expresses. Preserve the linking verb when the sentence defines what something is. Name the actor and use active voice when the sentence explains what something does. Do not force an action verb when it invents agency or obscures the underlying relationship.

## Match Specificity to the Claim

Precision means choosing the exact abstraction the claim needs, not naming the lowest-level detail available. Use stable domain categories for implementation choices that can change without altering the decision, mechanism, or contract.

Name a format, library, protocol, encoding, version, tool, or output shape only when the design depends on its behavior, the reader must act on it, or it provides evidence required by the point. Introduce the term before use when the intended audience may not recognize it. Move operational commands and replaceable implementation choices into implementation plans, how-to guides, or reference documentation.

## Use Domain Language Before Internal Abstractions

Establish scope with terms the reader already understands:

> Users can submit videos from the web application, mobile clients, or a bulk-import tool.

Introduce terms such as `ingest manifest`, `transcode job`, or `media artifact` only when the architecture depends on their specific meaning. Apply [`terms.md`](terms.md) to admit and define first-class vocabulary, test category coverage, and decide whether a term needs its own section.

## Control References

Use `this`, `that`, `it`, and `result` only when one immediate noun can serve as the antecedent. If several transformations precede a sentence, name the exact source document, resolved value, artifact, resource, or instance.

Apply the terminology and emphasis rules in [`terms.md`](terms.md) when a passage introduces, renames, bolds, or reuses a first-class term.

## Prove Non-Obvious Representations

When prose claims that a representation supports composition, identity, overrides, cross-object references, validation, or another unfamiliar relationship, provide a minimal concrete example. Explain which field, expression, or transition demonstrates the claim.

Do not add an example merely because a format appears in the document. The example must carry evidence the prose needs.

## Keep Planning Structure out of the Prose

Do not expose planning labels as `Goal:`, `Decision:`, `Mechanism:`, `Warrant:`, or `Consequence:` in ordinary paragraphs. Connect the ideas with verbs, conjunctions, and causal transitions.

Use a colon for a genuine list, definition, example, or expansion. Do not compress planning categories into a colon-led inventory when a natural sentence would express the relationship more clearly.

## Preserve Paragraph Continuity

For each pair of neighboring paragraphs, complete this sentence privately:

```text
The second paragraph follows from the first because ______.
```

The answer must identify a dependency, continuation, contrast, consequence, or required example. Broad topical similarity does not create a sequence. Relocate the second paragraph when its strongest connection belongs to another section.

Check the paragraph against the section spine as well as its immediate neighbors. A paragraph that reads well in isolation still weakens the document when it interrupts the section's ordered claim.

## Reflow After Every Edit

Treat an edit as a change to the passage's reasoning, not an isolated replacement. Even a stronger sentence can duplicate a later explanation, remove a prerequisite, strand a transition, or move a consequence ahead of its cause.

Review the edit at widening scopes:

1. Within the paragraph, verify that the opening states its point and each later sentence receives the context it needs from the sentences before it.
2. Between paragraphs, verify that the changed paragraph follows from its predecessor, enables its successor, and still belongs to the section that contains it.
3. Across the document, verify that the edit did not move a concept, decision, transformation, or consequence ahead of the material it assumes.

Rewrite, split, combine, move, or remove surrounding material whenever those checks fail. Preserving untouched wording carries less value than preserving the document's reasoning path.

## Rewrite the Whole Paragraph

The opening sentence should usually establish the point. The remaining sentences provide only the requirement, mechanism, evidence, warrant, constraint, example, or consequence needed to support it.

Read the paragraph without its final sentence. If no information disappears, delete that sentence. If local edits create duplicated subjects, stale transitions, or mixed levels of abstraction, discard the accumulated wording and rewrite the paragraph from its point, explicit reasoning, hidden conclusion, and evidence.
