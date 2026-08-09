# Document Structure

*Read before drafting or restructuring a multi-section document, choosing its explanatory arc, naming or nesting headings, or ordering sections.*

## Contents

- Build the maximal outline
- Choose the explanatory arc
- Select local concept layouts
- Orient high-load sections
- Establish transformation freedom before optimizations
- Compress the visible outline
- Build section spines
- Express containment through headings
- Name headings by function
- Order the detailed explanation
- Reflow structural edits
- Worked example

## Build the Maximal Outline

Create a maximal private outline before drafting or restructuring every multi-section document. Give each material concept, relationship, owner, lifecycle stage, transformation, and boundary a candidate heading. Add a one-sentence point when the heading alone cannot express its contribution.

Separate three structures while planning:

- The **subject model** records how the real concepts contain, depend on, and interact with one another.
- The **knowledge dependencies** record which definitions or mechanics readers need before they can evaluate a later claim.
- The **reading path** determines how the document orients readers, develops those dependencies, and completes its promised explanation.

Do not force the reading path to mirror the subject model. A document may orient readers with a complete system before explaining its parts, even though those parts remain conceptually nested inside that system.

Expand the subject according to the selected document contract:

- An architectural overview expands domain concepts, ownership boundaries, representations, relationships, transformations, and lifecycle stages.
- An RFC or ADR also expands decision pressures, alternatives, evidence, qualifications, and consequences.
- An implementation plan or how-to guide expands prerequisites, stages, actions, verification, recovery, and completion.
- A reference document expands the interface's actual categories and member groups.
- An explanation or tutorial expands causal concepts, learning stages, exercises, observations, and their dependencies.

Annotate difficult candidates privately:

```text
Point: What this material establishes
Contains: Which concepts belong inside it
Assumes: What readers must already understand
Enables: Which later reasoning depends on it
Reading role: Orientation, definition, mechanism, synthesis, projection, or verification
Local layout: Which selected layout organizes the claims at this scope
```

## Choose the Explanatory Arc

Choose the dominant reading path after modeling the complete subject. Start with the outcome, system, task, decision, or interface that gives the details meaning, then select the arc that fits the reader's job.

Use these defaults:

- Use **whole-part-whole** when a recognizable whole provides the details' purpose, several parts need separate explanation, and recombining those parts reveals behavior the opening cannot establish.
- Use **bottom-up composition** when readers cannot understand the result until they know its primitives.
- Use **top-down decomposition** when readers already recognize the whole and only need its internal structure.
- Use a **linear transformation** when data or state passes through an inherently sequential pipeline.
- Let decision documents follow pressures, choices, evidence, and consequences. Let procedures follow prerequisites, actions, and verification. Let reference documents mirror the interface readers need to search.

Apply this private test before selecting whole-part-whole:

```text
Whole: Can readers recognize the system or outcome before learning its internals?
Motivation: Does that whole explain why the parts matter?
Parts: Do several parts require independent explanation?
Synthesis: Will recombining them reveal behavior the opening could not explain?
```

Use whole-part-whole only when every answer contributes real content. Avoid it when the subject has one simple mechanism, follows a strictly linear transformation, serves lookup rather than explanation, or would end by merely repeating the opening.

### Structure Whole-Part-Whole

Use the opening whole to establish the operational result, its boundary, why it matters, and the roles of its major parts. Define every unfamiliar or project-specific term when it first appears. Prefer familiar domain language when introducing a specialized term would overload the opening.

Decompose only as far as the later explanation requires. Explain each part's identity and mechanics in dependency order, then build upward through the relationships and transformations that connect them.

Return to the whole with greater explanatory resolution. Show how the established parts produce its behavior, satisfy its pressures, or create its consequences. Do not repeat the opening description with more words.

The opening whole and final synthesis do not require duplicate headings. An introduction can define the whole at operational scale, while a later domain section explains its complete construction.

## Select Local Concept Layouts

Select a local layout for every surviving section after choosing the dominant document arc. The dominant arc governs the complete reading path, while the local layout governs the claims inside one section. Apply the selection recursively when a subsection carries enough explanatory load to need its own structure.

Use a compact definition by default. Choose a larger layout only when the concept's actual relationships require it.

| Layout | Use when | Claim sequence |
| --- | --- | --- |
| Compact definition | The concept has little internal structure. | Kind, role, boundary, relevant behavior |
| Whole-part-whole | Several parts cooperate to produce behavior that no part explains alone. | Operational whole, parts, interaction, reconstructed whole |
| Bottom-up composition | The result cannot receive an accurate operational definition before its primitives. | Primitives, relationships, composed result |
| Top-down decomposition | Readers recognize the whole and need only its internal structure. | Whole, boundaries, parts, responsibilities |
| Linear transformation | Input or state passes through an inherently ordered process. | Input, operations, output, consequence |
| Lifecycle | One identity persists through meaningful state transitions. | Creation, active state, transitions, termination or recovery |
| Taxonomy | A category contains members selected by meaningful differences. | Membership boundary, members, differences, selection rule |
| Contract | Separate owners coordinate through an interface or invariant. | Consumer need, contract, provider behavior, failure boundary |
| Decision argument | A pressure selects one mechanism or policy. | Pressure, choice, mechanism, consequence |
| Feedback loop | An observed result changes the next operation. | Input, operation, observation, correction, stable condition |
| Lookup hierarchy | Readers navigate named members rather than follow a linear argument. | Parent category, member groups, individual members |

Apply these private tests before assigning a local layout:

```text
Scope: Does this concept carry enough explanatory load to need more than a compact definition?
Composite: Do several parts interact to produce behavior worth reconstructing?
Primitive: Would an operational definition become false, circular, or unintelligible without earlier primitives?
Transformation: Does one representation or state become another through ordered operations?
Identity: Does one object retain identity while its state changes?
Category: Does the concept define a membership boundary and distinguish several members?
Ownership: Does the concept coordinate separate owners through a contract?
Decision: Does a requirement or pressure select the mechanism being explained?
Feedback: Does an observation alter the next operation?
Lookup: Will readers navigate members independently rather than read them in sequence?
Reconnection: Does the local explanation return naturally to the parent section's claim?
```

Whole-part-whole may recur at the document, section, and subsection levels, but each occurrence must pass the composite and synthesis tests at its own scope. Do not repeat an orientation or synthesis merely to make the structures symmetrical.

When several peer concepts remain after layout selection, identify the primary concept that organizes the reader's model and place it before its attributes, policies, storage, or implementation machinery. Preserve an established domain sequence when that sequence expresses the same conceptual priority.

Use this minimal-definition test before moving a supporting concept earlier:

```text
Can the primary concept receive an accurate one-sentence definition while the
supporting concept receives a short gloss?

Yes: Keep the primary concept first and defer the supporting mechanics.
No: Establish the supporting concept first because it is a hard prerequisite.
```

Reader purpose, safety constraints, and hard causal dependencies override conceptual priority. Causal order governs transformations, containment governs heading depth, and domain convention resolves genuine ties rather than replacing these tests.

## Orient High-Load Sections

Before discussing a high-load concept's stages, variants, implementation, constraints, or edge cases, establish the concept itself. Its opening passage should explain the operation or relationship it names, the relevant inputs and outputs, why the architecture uses it, and the scope needed by the section.

When a subsystem or workflow exists to satisfy a distinct user or operational need, state that capability and its concrete pressure before describing triggers, caches, protocols, stages, or optimizations. Structural mechanics do not make readers reconstruct the purpose that selected them.

Make the topic sentence understandable before the details that follow. A section may expand the concept later, but its opening cannot depend on later paragraphs to decode an abstract summary.

## Establish Transformation Freedom Before Optimizations

Before describing partitioning, compression, indexing, reordering, caching, specialization, or another representation-level optimization, establish:

- The source representation and its priorities
- The produced representation and its consumers
- The pressure that motivates a different layout
- The transformations the responsible subsystem may perform
- The semantics, references, or behavior those transformations must preserve
- The output cardinality the transformation permits

Place the optimization beneath that parent model. A detail such as splitting one input into several loadable outputs cannot introduce the compilation model that makes the split possible.

## Compress the Visible Outline

Transform the maximal outline into independently useful claim clusters. A heading survives only when readers may need to navigate directly to the point it establishes.

Assign every first-class concept one canonical definition location before compression. Preserve that definition when later sections depend on it. Merge or remove its candidate heading only when an earlier surviving passage assumes full responsibility for establishing the concept's kind, role, and boundary.

Apply these compression rules:

- Merge sections that explain one mechanism or repeat one claim.
- Nest specializations beneath the concept or lifecycle they belong to.
- Relocate cross-cutting material to the boundary that governs it.
- Collapse examples, qualifications, table members, and short supporting points into prose.
- Remove headings that exist only to balance sibling counts.

A single child beneath a parent should trigger review rather than automatic removal. The parent may establish a shared model while the child explains one substantial specialization.

Do not expose the explanatory arc as generic headings such as `Whole`, `Parts`, or `Synthesis`. Name the actual domain concepts that perform those roles.

## Build Section Spines

State each section's point, then list the ordered claims required to establish it. Every paragraph must advance one claim in that spine, supply evidence or a warrant required by it, or explain a direct consequence.

Broad topical similarity does not establish paragraph ownership. Move a paragraph when it develops a sibling concept, mechanism, or lifecycle stage that another section uses more directly. A paragraph can remain locally clear and factually correct while still interrupting the section's reasoning.

Apply this ownership test to every paragraph:

```text
Section point: What the complete section establishes
Paragraph role: Which claim, evidence, warrant, or consequence it contributes
Predecessor: What the previous paragraph establishes for this one
Successor: What this paragraph enables next
Destination: Which section needs it most if either connection fails
```

Define every unfamiliar term at first appearance with enough precision to identify its kind, role, and boundary. A later section may expand its representation, mechanics, constraints, and lifecycle, but it must not supply the basic meaning omitted earlier.

Within the detailed explanation, place each mechanism after its prerequisites and as close as possible to the first claim or example that requires it. Do not place a definition early merely because it relates to the section's general topic.

## Express Containment Through Headings

Use heading depth to express the subject model rather than the order in which readers happened to discover it.

- Nest a concept when it represents a part, type, stage, specialization, or projection of its parent.
- Keep a concept top-level when it establishes an independent boundary or crosses several parent concepts.
- Place a cross-cutting concept under the mechanism that governs it rather than the section where it first appears.
- Introduce shared representations before the specialized objects that use them in load-bearing reasoning.
- Keep sibling headings at the same abstraction level and make them answer the same kind of question.

For example, `Failure Recovery` belongs beneath `Job Execution` when it explains one execution stage. It belongs beside that section only when recovery crosses admission, execution, storage, and delivery as an independent architectural boundary.

## Name Headings by Function

Name the concept, boundary, operation, stage, or outcome that the section establishes. Use the shortest domain term that uniquely identifies the section. Add a modifier only when it distinguishes the subject from sibling concepts or names a real architectural boundary. Remove modifiers that merely describe how the document discusses the subject.

Treat a conceptual heading as a claim that its subject belongs to the architecture. Ground that subject in an existing implementation or domain concept, or in an explicit design decision that introduces it. When a phrase merely describes the shape or relationship of established concepts, keep that description in the section body and name the heading after the established subject.

Use an operation, transformation, or lifecycle stage as a heading only after the document has defined the subject it acts upon. When one section must introduce both the subject and its operations, name the section for the subject and nest the operations beneath it.

For example, `Cache Eviction` can follow an established definition of a cache. If the section must first explain what a cache stores and who owns it, use `Caches` and nest `Eviction` beneath it. The heading then reflects the section's definition ownership while preserving the operation as a navigable specialization.

Apply this modifier test to every heading:

1. Remove each modifier from the heading.
2. Check whether the remaining term still identifies the section and distinguishes it from siblings.
3. Restore only modifiers that change the architectural meaning.

Choose grammar according to the document's job:

- Use compact noun phrases such as `Request Admission`, `Job Execution`, and `Failure Recovery` for architecture, explanation, and reference topics.
- Use parallel imperative phrases for procedures, plans, and task instructions.
- Reserve question headings for tutorials, FAQs, and troubleshooting documents where the reader arrives with that question.

Prefer the shortest specific phrase that remains unambiguous. A one-word heading works when the surrounding hierarchy supplies its scope. Keep sibling headings parallel in grammar, capitalization, and abstraction level.

Follow the repository's heading convention. Default to title case when none exists. Avoid headings that merely announce an overview, discussion, notes, details, or the section's intention when a domain term can state its point.

## Order the Detailed Explanation

After the orienting view, follow the dominant document arc and the local layout assigned to each section. Within those layouts, preserve hard dependencies and causal order. Place definitions before load-bearing uses, inputs before transformations, representations before projections, and initial-state construction before runtime mutation, persistence, or synchronization.

Order peer domain concepts by conceptual priority. Introduce the object that organizes the reader's model before its attributes, policies, storage, or implementation machinery when the minimal-definition test permits it. Use established domain order as evidence of that priority and justify departures when reader purpose, safety, or a hard prerequisite requires another sequence.

Define the complete domain model before explaining the editor, compiler, cooker, API, or runtime that projects or transforms it. Keep failure behavior beside the mechanism that handles it.

Apply these tests to each section:

```text
Contribution:
The document needs this section because it establishes ______
needed to understand, evaluate, trust, or accomplish ______.

Dependency:
This section assumes ______ has already been established
and enables the later discussion of ______.

Local layout:
This section uses ______ because the relationship it must explain is ______.

Reconnection:
This section returns to the document spine by establishing ______.
```

Remove or combine a section when it makes no precise contribution. Reorder it when its detailed reasoning relies on a concept whose basic meaning or required mechanics appear later.

## Reflow Structural Edits

After adding, removing, splitting, merging, or moving a section, reassess the reading path at widening scopes:

1. Rebuild the changed section's claim spine and reorder its paragraphs.
2. Recheck the neighboring sections for broken transitions, duplicated ownership, and prerequisites that moved.
3. Recheck the complete explanatory arc, including the orienting whole, final synthesis, declared inventories, and every later section that depends on the changed concept.

Rewrite transitions, relocate supporting material, and revise the opening or synthesis when the structural edit changes what they promise or establish. A structural edit completes only after the revised document still carries readers from orientation through detailed reasoning to its intended result.

## Worked Example

Consider an architectural overview for a distributed job service. The document must explain how the service accepts a job, gives one worker authority to run it, recovers from worker failure, and publishes one durable result.

### Maximal Private Outline

Begin with a conceptually exhaustive outline rather than the headings expected in the final document:

```text
Reliable Job Execution
├── Client Submission
│   ├── Authentication
│   ├── Request Shape
│   └── Idempotency Key
├── Admission
│   ├── Validation
│   ├── Job Identity
│   ├── Durable Record
│   └── Rejection
├── Scheduling
│   ├── Ready Queue
│   ├── Priority
│   ├── Tenant Capacity
│   └── Dispatch
├── Work Ownership
│   ├── Lease Acquisition
│   ├── Lease Renewal
│   ├── Lease Expiration
│   └── Fencing Token
├── Worker Execution
│   ├── Input Loading
│   ├── External Side Effects
│   ├── Result Staging
│   └── Result Commit
├── Failure Recovery
│   ├── Worker Loss
│   ├── Retry
│   ├── Retry Limit
│   └── Terminal Failure
├── Status Publication
├── Operator Metrics
└── Execution Guarantees
```

This outline models the complete subject. It does not claim that every candidate deserves a heading or that the conceptual containment already provides the best reading order.

### Explanatory Arc

Choose whole-part-whole because the subject satisfies all four conditions:

- **Whole:** Readers can recognize a job moving from submission to one durable result before learning the internal coordination protocol.
- **Motivation:** That complete operation explains why admission, ownership, execution, and recovery matter.
- **Parts:** Each boundary carries independent identity, state, and failure mechanics.
- **Synthesis:** Only the final combination explains how the service tolerates duplicate execution without accepting conflicting results.

The introduction can define the whole without prematurely naming its internal mechanisms:

> A **job** is a durable unit of work submitted by a client and completed when a worker commits its result. The service validates each job, schedules it for execution, assigns one worker authority to publish the result, and recovers unfinished work when that worker fails.

This passage defines `job` at first use and gives every later section a visible role. It defers terms such as `lease` and `fencing token` until the sections that can define them through their operation.

### Compression Decisions

Convert the maximal outline into a visible structure by making each heading earn independent navigation:

- Merge client submission, validation, identity, durable recording, and rejection under `Request Admission` because they establish one acceptance boundary.
- Keep `Scheduling` separate because ordering ready work and enforcing tenant capacity require a distinct mechanism.
- Nest leases and fencing tokens under `Work Ownership` because both establish which worker may act for a job.
- Keep `Failure Recovery` top-level because worker loss crosses ownership, execution, and result publication rather than specializing one of them.
- Collapse status publication into the result-commit prose because it adds one consequence rather than an independent architectural boundary.
- Omit operator metrics because the controlling claim does not depend on observability design.
- Preserve `Execution Guarantees` as the synthesis because it combines evidence established across every preceding section.

### Visible Outline

The compressed document uses domain headings and explicitly assigns each passage a point and reading role:

```text
# Reliable Job Execution

Introduction
  Point: Define a job and show its complete path from submission to one
         durable result.
  Role: Orienting whole.
  Enables: Every later mechanism has a visible purpose.

## Request Admission
  Point: Convert an accepted client request into one durable job identity
         before execution can begin.
  Role: First domain boundary.
  Assumes: Only the opening definition of a job.
  Enables: Scheduling can operate on recorded jobs rather than requests.

  ### Validation
    Point: Reject requests whose fields or permissions cannot produce a
           runnable job.

  ### Durable Records
    Point: Assign one identity and persist the accepted job before reporting
           success to the client.

## Scheduling
  Point: Select which recorded job may consume worker capacity next.
  Role: Transformation from accepted work to ready work.
  Assumes: Request Admission has produced durable jobs.
  Enables: Work Ownership can assign a selected job.

  ### Priority
    Point: Order ready jobs by tenant policy and submission time.

  ### Capacity
    Point: Prevent dispatch from exceeding tenant and worker limits.

## Work Ownership
  Point: Give one worker temporary authority to act for a selected job.
  Role: Coordination boundary.
  Assumes: Scheduling has selected ready work.
  Enables: Worker Execution can distinguish authorized work from stale work.

  ### Leases
    Point: Define a lease as a time-limited ownership grant that another
           worker may acquire after expiration.

  ### Fencing Tokens
    Point: Attach an increasing ownership generation to each lease so storage
           can reject writes from an earlier owner.

## Worker Execution
  Point: Perform the job and commit its result only while the worker retains
         current ownership.
  Role: Primary execution mechanism.
  Assumes: Work Ownership has established leases and fencing tokens.
  Enables: Failure Recovery can distinguish incomplete work from committed work.

  ### Side Effects
    Point: Give retried external operations stable operation identities so a
           retry can recognize work already performed.

  ### Result Commit
    Point: Publish a staged result only when its fencing token still matches
           the current ownership generation.

## Failure Recovery
  Point: Return unfinished jobs to schedulable state without replacing a
         result that already committed.
  Role: Cross-cutting failure mechanism.
  Assumes: Ownership expiration and result commit have been defined.
  Enables: The synthesis can explain behavior under worker loss.

  ### Lease Expiration
    Point: Make abandoned work eligible for another ownership grant.

  ### Retry Limits
    Point: Convert repeatedly failing work into a terminal state with a
           recorded reason.

## Execution Guarantees
  Point: Show how durable admission, current ownership, fenced commits, and
         recovery allow duplicate computation while preserving one accepted
         result.
  Role: Reconstructed whole and architectural warrant.
  Assumes: Every preceding domain mechanism.
  Enables: Readers can evaluate the complete operating claim.
```

The heading hierarchy records conceptual containment. The section order records the knowledge path from accepted work through scheduling, ownership, execution, recovery, and the final guarantee. The introduction and `Execution Guarantees` both discuss the complete system, but the latter adds the causal explanation that the opening could not yet provide.

### Local Layout Reasoning

The document uses whole-part-whole as its dominant arc without forcing every section to repeat that pattern. Each major section selects the smallest layout that matches the relationship it must establish:

| Section | Local layout | Reason |
| --- | --- | --- |
| Introduction | Top-down decomposition | The complete path from submission to one durable result introduces the whole and names the boundaries that later sections explain. |
| Request Admission | Linear transformation | The section converts a client request into a validated, durable job identity. |
| Scheduling | Decision argument | Priority and capacity pressures select which recorded job may consume worker capacity next. |
| Work Ownership | Whole-part-whole | Temporary authority forms the local whole, while leases and fencing tokens supply independent parts whose interaction creates enforceable ownership. |
| Worker Execution | Linear transformation | The worker loads input, performs work, stages a result, and attempts an ownership-checked commit in causal order. |
| Failure Recovery | Lifecycle | The same durable job moves from abandoned work through reassignment, retry, and possible terminal failure. |
| Execution Guarantees | Bottom-up composition | The section recombines admission, ownership, execution, and recovery to explain why duplicate computation still yields one accepted result. |

`Work Ownership` demonstrates recursive whole-part-whole. Its opening defines temporary authority at an operational level, its subsections explain the lease and fencing mechanisms, and its final claim reconstructs those parts as an enforceable ownership boundary. `Request Admission` does not copy that structure because its point concerns an ordered transformation rather than emergent behavior among parts.

### Section Spine

The `Work Ownership` section can use this private paragraph spine:

```text
1. A scheduled job needs one current owner before a worker may act for it.
2. A lease grants that authority for a bounded time.
3. Expiration permits reassignment after worker loss.
4. Expiration alone cannot stop a delayed former owner from writing.
5. A fencing token lets storage reject that stale write.
6. Worker Execution can now rely on an enforceable ownership boundary.
```

Each paragraph receives its premise from the paragraph before it and enables the next. Moving fencing tokens into `Worker Execution` would break that flow because execution already depends on the ownership boundary they create.

### Structural Reflow

Suppose the design moves result commitment into an independent storage service. Adding a `Result Storage` section requires more than moving the `Result Commit` subsection:

1. Revise `Worker Execution` so its point ends at staging a result rather than committing one.
2. Place `Result Storage` after execution because it consumes staged results and current fencing tokens.
3. Revise `Failure Recovery` because it now distinguishes staged, committed, and abandoned results through the storage boundary.
4. Revise `Execution Guarantees` so its synthesis attributes the single accepted result to storage rather than the worker.
5. Recheck the introduction if the storage boundary changes the operational description promised there.

The local move changes section ownership, downstream prerequisites, and the final warrant. Reflowing all three levels preserves the document's reasoning rather than leaving surrounding prose attached to the former architecture.
