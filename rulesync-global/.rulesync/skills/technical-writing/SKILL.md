---
name: technical-writing
description: >-
  Writes and revises architectural overviews, design documents, RFCs, ADRs,
  implementation plans, technical explanations, how-to guides, reference
  documentation, tutorials, and READMEs. Use when technical prose must select
  the right document form, build one controlling claim, connect evidence through
  explicit reasoning, introduce unfamiliar concepts, preserve a consistent
  architectural model, and earn qualitative conclusions through concrete
  mechanisms.
---

# Technical Writing

Build every document around one controlling claim about what the reader should understand, trust, decide, or accomplish. Select the document contract first because each form proves its claim differently. Preserve one coherent line from the reader's need through the evidence, reasoning, and result.

State goals, requirements, decisions, warrants, and concrete consequences directly. Use hidden conclusions to organize the evidence that should earn qualitative judgments such as maintainability, coherence, reliability, performance, usability, or automation readiness.

Treat the reading path as part of the document's correctness. Orient readers with the outcome, system, task, decision, or interface that gives the details meaning. Then develop the formal explanation through dependency, causality, and transformation, returning to the whole when recombining its parts reveals behavior the opening could not establish.

Define every unfamiliar or project-specific term when it first appears. Establish its kind, role, and distinguishing boundary with enough precision to support the current passage. When a term names a category, define the shared membership boundary that covers every known subtype used in the document. Later sections may expand mechanics, constraints, and lifecycle, but they must not supply basic meaning omitted at first use.

Treat every definition as an architectural claim. Verify that it identifies what kind of thing the subject actually names rather than one input, output, state, dependency, identifier, storage representation, or implementation target associated with it. Test every asserted property against every known member of the category.

Ground every first-class term in the system or the design. Identify whether it names an existing implementation or domain concept, an explicitly proposed architectural concept, or only a descriptive relationship. Promote only the first two into controlled vocabulary. Keep descriptive relationships in ordinary prose rather than turning them into bold definitions, conceptual headings, inventory members, diagram nodes, or type-like names.

Resolve concept dependencies before introducing specializations, operations, or lifecycle mechanics. Decompose compound terms into the concepts they assume, assign each first-class concept one canonical definition location, and establish every prerequisite before its first load-bearing use. A definition of a specialized term does not define its base concept.

Apply deliberate sequence at every scale. Within a paragraph, lead with its point and arrange supporting sentences in the order needed to understand it. Between paragraphs, make each passage continue, support, qualify, or follow from the previous one. Across the complete document, select an explanatory arc that gives readers context before detail and preserves prerequisites within the detailed reasoning.

After every edit, reassess all three scales. Reorder or rewrite the changed paragraph when its sentence logic no longer holds, inspect its neighboring paragraphs for broken transitions or ownership, then recheck the section and document outline when the edit changes a prerequisite, concept, decision, or consequence. Reflow the surrounding prose instead of preserving an obsolete sequence around a locally correct change.

## Select the Document Contract

Identify the document's purpose before designing its layout. When a request asks for a system or feature design without naming another form, default to an **architectural overview**.

Read the matching profile before drafting or restructuring:

| Document contract | Read this profile |
| --- | --- |
| Unspecified system or feature design, architecture overview | [`templates/architectural-overview.md`](templates/architectural-overview.md) |
| RFC, proposal, or option evaluation | [`templates/rfc.md`](templates/rfc.md) |
| Architecture decision record | [`templates/adr.md`](templates/adr.md) |
| Implementation or migration plan | [`templates/implementation-plan.md`](templates/implementation-plan.md) |
| Conceptual or causal explanation | [`templates/explanation.md`](templates/explanation.md) |
| Task instructions or troubleshooting guide | [`templates/how-to.md`](templates/how-to.md) |
| API, schema, command, or system reference | [`templates/reference.md`](templates/reference.md) |
| Guided learning experience | [`templates/tutorial.md`](templates/tutorial.md) |

Treat each profile as a reasoning contract, not a mandatory heading list. Adapt headings to the subject and omit stages that carry no necessary part of the claim. A README names a location rather than a document contract, so select its contract from the job it performs.

For an architectural overview, present the golden operating model and its established decisions as fully realized architecture. Do not add RFC-style feasibility analysis, rejected alternatives, generic open questions, implementation sequencing, backlog, or implementation-status qualifications unless the request or existing document makes them part of the purpose. Attach unresolved design uncertainty to the claim it qualifies.

Read [`references/design-documents.md`](references/design-documents.md) for rules shared by architectural overviews, RFCs, ADRs, and implementation plans.

## Build the Controlling Argument

Use these terms while planning:

- A **reader outcome** states what the reader should understand, trust, decide, or accomplish.
- A **controlling claim** states the central proposition the document must support.
- A **goal** states what the system or workflow must enable.
- A **topic** names a subject without making a claim about it.
- A **point** states what a paragraph or section establishes about its topic.
- A **decision pressure** names the goal, constraint, failure mode, or tradeoff that selects a choice.
- A **decision** records the choice made under that pressure.
- A **mechanism** explains how the choice operates.
- **Grounds or evidence** make the claim verifiable through ownership, data flow, representations, constraints, measurements, examples, or failure behavior.
- A **warrant** explains why the evidence supports the claim or why the mechanism satisfies the pressure.
- A **qualifier** states the conditions or limits under which the claim holds.
- A **counterargument or alternative** presents a credible competing claim, mechanism, or interpretation.
- A **rebuttal** explains why the selected claim still holds under the stated pressures.
- A **consequence** states what the mechanism produces, prevents, requires, or costs.
- A **hidden conclusion** names a qualitative judgment the evidence should earn.

These terms form a private reasoning model, not a prose template. A reference page and an RFC both need a controlling claim, but their profiles require different evidence and reasoning.

Use this private map when the structure remains unclear:

```text
Document contract: Which form fits the reader's job
Audience: What the reader already knows and needs
Reader outcome: What reading should enable
Controlling claim: What the document must establish
Goals and pressures: What selects or constrains the design
Decisions or method: What the document asks the reader to accept or do
Evidence: Which mechanics, examples, facts, or checks support the claim
Warrants: Why that evidence proves the relevant point
Consequences and limits: What follows and where the claim stops
Hidden conclusions: Which qualitative judgments the evidence should earn
```

Before drafting or revising:

1. Identify the document contract, audience, reader outcome, and controlling claim.
2. Inspect the available evidence, repository terminology, requested tone, and surrounding document.
3. Record the goals, decision pressures, decisions or method, warrants, consequences, and meaningful limits.
4. Identify, ground, and admit the concepts and architectural inventories the reader must understand.
5. For every multi-section document, create a maximal private outline containing every material concept, relationship, owner, lifecycle stage, transformation, and boundary relevant to its contract.
6. Identify the orienting whole, select the dominant document arc, assign each high-load section a local concept layout, and mark where the document defines, decomposes, reconstructs, projects, or verifies its subject.
7. Assign containment and dependencies, merge overlaps, relocate misplaced concepts, and compress the maximal outline into independently useful claim clusters.
8. Build an ordered claim spine for each section, assign every first-class concept a canonical definition location, and place each definition before the specializations or operations that depend on it.
9. When revising, map each changed passage back onto its paragraph, section spine, and document dependencies, then reflow every affected scope.
10. Match status treatment to the document contract. Write settled architectural-overview claims as fully realized operating behavior regardless of implementation progress. Mark unresolved design decisions, accepted decisions, and implementation state only when the selected contract requires those distinctions.
11. Select hidden conclusions at the document, section, or paragraph level only where an evaluative takeaway matters.

Read [`references/argumentation.md`](references/argumentation.md) when the claim, warrant, qualification, comparison, or relationship between explicit and hidden conclusions remains unclear.

## Organize with Hidden Conclusions

Use hidden conclusions as an organizing lens at the document, section, and paragraph levels. They identify the qualitative judgment that the evidence should earn and help select relevant mechanisms, examples, consequences, and failure behavior.

Keep the reasoning a reader must evaluate explicit. Never hide a goal, requirement, decision, warrant, compatibility promise, latency limit, security boundary, or concrete consequence. A hidden conclusion may shape the argument, but it cannot carry a logical step that connects evidence to the claim.

Read [`references/hidden-conclusions.md`](references/hidden-conclusions.md) when selecting an evaluative takeaway or deciding which evidence can earn it.

## Introduce Terms Deliberately

Before introducing an unfamiliar or specialized term, determine whether the controlling claim depends on its specific meaning and what authority makes it part of the model. Ground an existing term in code, a schema, an API, a format contract, or established domain language. Introduce a proposed term only when the design deliberately adds a necessary concept with defined ownership, boundaries, representation, and relationships. Otherwise replace the phrase with the stable domain category, keep it descriptive, or omit it.

Define an admitted term at first use through its kind, role, and distinguishing boundary. Explain every behavior the current passage requires, then defer unrelated internals until later reasoning needs them.

Decompose specialized and compound terms into their conceptual prerequisites. Define the base concept before the specialization carries load-bearing reasoning, and record one canonical definition location for each first-class concept. Do not treat a definition of a specialization as a definition of the category it specializes.

Treat file extensions, tool and library names, protocols, encodings, vendor products, and project-specific terminology as unfamiliar unless the audience definition establishes otherwise. Use the most precise stable abstraction the claim requires rather than the most concrete detail available. Name an implementation detail only when the decision depends on it, the reader must act on it, or it provides necessary evidence.

Identify each concept's **explanatory load**. Give a concept a dedicated explanation when the audience may not know it, the project gives it a specialized meaning, its behavior differs materially from familiar systems, several later decisions depend on it, or a compact example explains its role better than another abstraction.

Give a high-load concept its own section. Explain what it does in this design, why the architecture uses it, and the smallest example that demonstrates its relevant behavior. Define familiar or narrowly used concepts inline.

Define relationship terms such as `composes`, `specializes`, `inherits`, `adapts`, `implements`, and `resolves` through their operation. Name the contributors, combination or resolution rule, resulting concept, and relevant conflict or failure behavior. A relationship label cannot substitute for the mechanism it summarizes.

Do not introduce internal taxonomies before the reader understands the problem they solve. Use the reader's domain language first, then establish the project term through its operation and relationships.

For a term that names a category, test its definition against every known subtype or member used elsewhere in the document. State the property those members share rather than describing one representative example. Broaden the definition, split the category, or reclassify a member when the membership test fails.

Bold the first defining occurrence of a first-class term when doing so helps readers locate the document's vocabulary. Bolding does not make a phrase a term and never substitutes for its definition. During review, treat every bolded noun phrase, conceptual heading, type-like code span, inventory label, and labeled diagram node as a claim that the named concept belongs to the architecture. Verify its authority and status, then remove or demote unsupported terminology.

Read [`references/terms.md`](references/terms.md) when admitting or defining terms, testing category coverage, deciding explanatory load, maintaining a terminology ledger, recording identifier scope and stability, or applying bold emphasis to first-class vocabulary.

## Design the Reading Path

Before drafting or restructuring any multi-section document, create a maximal private outline of its subject. Keep that conceptual model separate from the reading path because the system's containment hierarchy, the reader's knowledge dependencies, and the visible section order need not match.

Make three ordering decisions at different scales. The document contract establishes the reasoning obligations. A dominant document arc organizes the complete reading path. Each high-load section uses a local concept layout selected by the relationship it must explain.

Choose the dominant arc from the document contract and the subject. For an architectural overview, use whole-part-whole when readers can recognize a meaningful whole before learning its internals, that whole explains why several parts matter, and the later synthesis reveals how those parts produce its behavior. Use bottom-up composition, top-down decomposition, a linear transformation, or another contract-specific arc when those conditions do not hold.

Apply the same selection recursively. A section may use its own whole-part-whole arc when its concept independently passes the whole, motivation, parts, and synthesis tests. Other sections may use a compact definition, linear transformation, lifecycle, taxonomy, contract, decision argument, or feedback loop. Use conceptual priority and established domain sequence to order peer concepts, not as universal substitutes for a local layout. Move a supporting concept earlier only when the primary concept cannot receive an accurate minimal definition without it.

Compress the maximal outline into independently useful claim clusters. Use headings to express the subject's actual containment, while section order carries the selected explanatory arc. An orienting whole can appear in the introduction and return later as a formal synthesis without creating duplicate headings.

Read [`references/document-structure.md`](references/document-structure.md) for the complete maximal-outline, explanatory-arc, whole-part-whole, containment, compression, ordering, and structural-reflow rules, plus an annotated worked example.

## Keep the Architectural Model Closed

Treat every table, list, union, tree, diagram, or parallel series of type-like noun phrases that enumerates architectural members as a declared inventory. Every member must answer the same governing question at the same abstraction level. When later prose introduces another member of that category, add it to the inventory, define it as a composition of existing members, or change the terminology so it no longer claims membership.

When a declaration, template, schema, command, or build input produces another representation, name the input concept, output concept, transformation, and cardinality. Distinguish zero-or-one, one-to-one, one-to-many, many-to-one, and many-to-many mappings. Ground every type-like output in code, a schema, a format contract, or an explicit design decision before naming it. Use one term for both sides only when identity, lifecycle, meaning, and cardinality remain invariant.

Treat every ID, key, hash, handle, URI, and path as scoped to the lifecycle stages where its producer creates it and its resolver can interpret it. State its representation, producer, resolver, scope, and replacement behavior. Treat `stable`, `persistent`, `durable`, and `permanent` as lifecycle guarantees that require a stated stability scope and the changes that preserve or replace the reference.

Give every table one comparison or lookup job. Include a column only when it answers a distinct reader question across most rows and the document needs that answer at this point. Remove columns that repeat another column, remain mostly constant, contain mostly empty values, or introduce a different architectural concern. Name each column with the shortest specific noun phrase that remains unambiguous.

Treat a column header as shared context for every cell beneath it. Read the header and row label as one composite label, remove category words that merely repeat the header, and keep sibling labels parallel. If some rows still require the repeated word for disambiguation, revise the header, retain it consistently, or split the category rather than mixing naming conventions.

Use heading depth to express conceptual containment. Nest a section beneath another when it classifies, decomposes, or specializes the parent concept. Reserve sibling headings for concepts operating at the same architectural level.

Treat the document contract, controlling claim, and reader outcome as the root of the document. Every major section must either advance that claim directly or establish a concept, decision, mechanism, warrant, consequence, prerequisite, or verification required by another section that does.

Apply these private tests:

```text
Contribution:
The document needs this section because it establishes ______
needed to understand, evaluate, trust, or accomplish ______.

Dependency:
This section assumes ______ has already been established
and enables the later discussion of ______.
```

After an orienting view, order formal explanations after the definitions and mechanics they assume and before the interactions, projections, transformations, or conclusions that depend on them. Define the complete model before explaining an editor, compiler, cooker, API, or other consumer that projects or transforms it.

The introduction should establish the controlling claim, the meaningful whole or outcome, and enough of the operating model to explain why the details matter. Define every unfamiliar term it introduces without expanding internals that later sections own.

Use headings to expose conceptual containment and keep siblings at one abstraction level. Name each section with the shortest domain term that remains distinct, adding modifiers only when they change its architectural meaning.

Read [`references/document-structure.md`](references/document-structure.md) when deciding whether a concept needs a section, naming or nesting headings, or ordering sections. Read [`references/terms.md`](references/terms.md) when defining or auditing first-class vocabulary. Read [`references/consistency.md`](references/consistency.md) when reconciling declared categories, tracing representations and identities across lifecycle boundaries, or proving a non-obvious representation. Read [`references/tables.md`](references/tables.md) when designing or auditing a table.

## Make Each Section and Paragraph Establish a Point

Plan difficult passages with this private card:

```text
Point: What the passage establishes
Contribution: How it advances the controlling claim or a required dependency
Pressure or requirement: What selects or requires this behavior
Mechanism and evidence: What concretely happens
Warrant: Why the mechanism establishes the point
Consequence: What follows
Hidden conclusion: What qualitative judgment the evidence should earn, if any
```

Use only the fields the passage needs. Keep every planning label out of the final prose.

Test contribution before placement or phrasing. State what required definition, decision, mechanism, evidence, boundary, or consequence the passage adds. Delete it when removing it loses nothing required. Only then assign it to the section whose point depends on it. Do not relocate filler.

When several architectural concepts form one contract or workflow, state the capability they enable or the invalid state they prevent before explaining their individual structure. A structural role explains what a concept contains, declares, or references. It does not replace the design purpose that justifies the concept.

Lead with the operative mechanism. Do not frame the design through behavior it avoids, such as “never guesses” or “does not rely on,” unless the prohibition defines a contractual boundary or directly distinguishes a considered alternative.

Prefer active voice when a sentence describes an operation. Put the responsible actor in the subject position, its operation in the verb, and the affected value or subsystem in the object. Use linking verbs when a sentence defines identity, classification, composition, or stable state. Do not force an action verb when it invents agency or obscures the domain relationship.

Use the remaining sentences only to supply a requirement, mechanism, evidence, warrant, constraint, example, or consequence needed to establish the point. Remove a sentence when its absence loses nothing required by the passage.

Build a private spine for each section by listing the ordered claims required to establish its point. Every paragraph must advance that spine, support one of its claims, or explain a direct consequence. Define an unfamiliar term when it first appears, then place its detailed mechanics after their prerequisites and near the first claim or example that requires them.

For every pair of neighboring paragraphs, state why the second follows from the first. Relocate a paragraph when the connection rests only on broad topical similarity or when another section uses its point more directly.

After changing any passage, repeat the sentence-order, paragraph-adjacency, section-spine, and document-dependency checks. A local edit completes only after the surrounding sequence still carries the reader from prerequisites through consequences.

When a design claims that a representation supports composition, identity, overrides, references, validation, or another non-obvious relationship, provide the smallest concrete example that exercises that relationship. Explicitly bind the explanation to the example, identify the exact field, expression, transition, or edge that provides the evidence, and state how it proves the claim. Include every premise required by the conclusion. Do not add examples that merely decorate familiar syntax or replace the relevant operation with an ellipsis.

Read [`references/prose.md`](references/prose.md) when drafting or repairing individual paragraphs. Read [`references/examples.md`](references/examples.md) when prose states a quality without earning it, presents evidence without a warrant, explains mechanics without their goal, or asserts a representation without demonstrating it.

## Keep Goals, Warrants, and Requirements Explicit

State required capabilities, compatibility promises, latency limits, security boundaries, and other success conditions directly. Name the users, workflows, operating conditions, or measurable boundary the system must support.

Do not replace a goal with implementation mechanics. State the capability, then explain the decision and mechanism that provide it.

Apply this rule locally as well as globally. When a subsystem or workflow exists to satisfy a distinct operational need, open its section with that capability and the concrete pressure behind it before describing triggers, caches, protocols, stages, or optimizations.

Do not leave the reader to invent the relationship between evidence and a claim. State the warrant whenever the causal connection, selection pressure, or underlying assumption carries meaningful design information or could reasonably be disputed.

## Compare Only When the Document Contract Requires It

Use an intrinsic hidden conclusion when the document explains one design. Do not introduce an alternative merely to make that design look favorable.

Use a comparison when an RFC evaluates alternatives, an ADR records competing pressures, a design replaces an existing mechanism, or the controlling claim depends on a tradeoff. Establish one shared requirement, describe each mechanism at the same level of detail, and show the concrete work, coupling, state, cost, or failure behavior each creates.

Audit `rather than`, `instead of`, `unlike`, `without`, `never`, `does not`, `avoids`, and `only`. Keep the contrast only when the document has established the alternative as current behavior, a considered proposal, a common misunderstanding created by the text, or a contractual failure mode. Otherwise state the selected mechanism directly.

Never weaken an alternative through loaded adjectives, missing context, or asymmetric detail. Remove a comparison when the evidence cannot support it.

## Write Natural Technical Prose

Make the opening begin the actual claim, problem, or operating model. Do not let a generic document announcement, agenda, or status disclaimer displace the design. Use scope or audience metadata when it helps readers without consuming the opening argument.

Do not announce “the key point,” “the important distinction,” or another description of the prose. Write the architecture, action, or causal relationship itself.

Use a colon when it introduces a real list, definition, example, or expansion. Do not compress planning categories into colon-led prose that would read more naturally with verbs and conjunctions.

Use one stable term for each first-class concept, and reserve each defined domain term for one concept. Apply this invariant to types, assets, artifacts, roles, states, boundaries, interfaces, and lifecycle stages rather than ordinary vocabulary. Replace vague references such as “the result,” “the setup,” or “the data” with the specific value, subsystem, or artifact when several antecedents exist.

Treat every noun phrase and predicate as a semantic claim about the architecture. Make the head noun identify the concept before modifiers qualify it. Attach each property to the concept, operation, or boundary that actually provides it, and use responsibility-bearing verbs only when the subject performs or owns the stated work.

Keep coordinated clauses and list members semantically parallel. Each must answer the same governing question, occupy the same architectural level, and perform the same grammatical role. Move a policy, mechanism, consumer, or consequence into its own sentence when it governs the other members rather than belonging beside them.

Name a language, framework, format, vendor, or implementation environment only when the claim depends on a capability specific to it, crosses its boundary, requires the reader to operate it, or uses it as evidence. After establishing implementation context, name the architectural actor for ordinary behavior.

Read [`references/terms.md`](references/terms.md) when defining first-class terms, testing definitions against category members, or deciding which terms receive bold emphasis. Read [`references/prose.md`](references/prose.md) when repairing sentence mechanics, references, or paragraph flow.

Prefer short paragraphs. Use a list for a genuine sequence or parallel set, a table for repeated mappings, and a Mermaid diagram when ownership, state, hierarchy, or a multi-stage flow becomes clearer spatially than verbally.

Keep flow diagrams focused on the stages needed to establish their point. Limit the longest directed path to six nodes and prefer labels of one to three words. Use noun phrases for actors, inputs, outputs, states, stores, and artifacts. Use active verb phrases for operations and transformations. Mixing nouns and verbs clarifies a flow when each label reflects the node's actual role. Label a bidirectional edge when both directions represent one symmetric relationship. Split it into two directed, labeled edges when each direction performs a different operation.

Read [`references/diagrams.md`](references/diagrams.md) when selecting diagram stages, labels, edges, or boundaries.

## Keep Status Attached to the Claim

An architectural overview defines the **golden design**: the complete operating model readers should build toward and preserve. Write every settled part in present tense as fully realized behavior, even when the repository implements only part of it. Do not label the overview or its settled claims as `intended`, `proposed`, `aspirational`, or `not yet implemented`, and do not add implementation-status disclaimers merely because code lags behind the design.

Mark unresolved design decisions where they occur. Describe current implementation behavior only when the overview's controlling claim specifically depends on a migration boundary or an incompatibility between the current system and the golden design. Keep that implementation evidence subordinate to the operating contract rather than using it to qualify the architecture.

Use document metadata for lifecycle status when the document contract requires it, such as an ADR marked proposed, accepted, deprecated, or superseded. Architectural overviews do not receive lifecycle metadata solely because implementation remains incomplete. Verify any necessary claims about current behavior against code or authoritative sources.

## Revise Through Mandatory Gates

Apply these gates in order. Repair a failed gate before polishing later prose because later checks depend on the model established earlier.

1. **Contract and claim:** Confirm that the selected profile matches the reader's job. State the reader outcome and controlling claim, then trace goals or pressures through decisions, evidence, warrants, consequences, limits, and status.
2. **Contribution and purpose:** State what each section and paragraph adds. Delete material whose removal loses no required information before deciding where it belongs. Confirm that every subsystem, workflow, and concept cluster receives its necessary operational goal before internal machinery.
3. **Concepts and definitions:** Ground every first-class term in code, a schema, an established domain concept, or an explicit design decision. Define kind, role, and boundary at first use. Test every category predicate against every member, distinguish concepts from associated representations or targets, and define relationship terms through their mechanics.
4. **Dependencies and structure:** Build the maximal private outline, choose the document arc and local layouts, then compress it. Place definitions before load-bearing uses, parent models before optimizations, inputs before transformations, and purposes before stages. Reconcile heading containment, tables, diagrams, and every explicit or implicit inventory.
5. **Mechanisms and semantics:** Name the actor, operation, inputs, conditions, outputs, and failure behavior required by each point. Verify noun identity, modifier attachment, verb responsibility, topic-sentence clarity, semantic parallelism, reference control, and implementation specificity.
6. **Evidence and examples:** Demonstrate every non-obvious relationship with the smallest claim-shaped example. Bind its explanation explicitly, identify the evidence, include every premise, state the warrant, and remove ellipses that hide the operation under discussion.
7. **Representations and consistency:** Trace every input to its grounded output with explicit cardinality. Record each identifier's representation, producer, resolver, lifecycle scope, and replacement behavior. Prove every stability or preservation guarantee and keep one term per concept across prose, schemas, tables, diagrams, and examples.
8. **Comparisons and inference:** Keep only alternatives that the document has established and compare them symmetrically. Replace unsupported qualitative claims with mechanisms and consequences, then confirm that the evidence earns each intended hidden conclusion.
9. **Reflow and finish:** Recheck sentence order, paragraph adjacency, section spines, and the complete reading path after every repair. Revisit the opening, synthesis, terminology, headings, tables, diagrams, and status claims whenever an edit changes their prerequisites or promises.

Preserve stable terminology, useful examples, factual qualifications, and the author's voice where they support the document. Intentional structure matters more than applying a uniform house style.

## Work Within the Request

Follow repository instructions and preserve unrelated changes when editing a file. Verify claims about implementation state only when the selected document contract requires them.

If the user requests an edit, make the edit and report the changed document. If the user requests advice or review, remain read-only and provide concrete findings or replacement language.
