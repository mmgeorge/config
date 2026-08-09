# Design Documents

*Read before drafting or restructuring an architectural overview, RFC, ADR, or implementation plan. Read the matching document profile first.*

## Establish the Design Through a Supported Claim

A design document makes a claim about an operating model, proposed change, recorded decision, or implementation path. Connect the relevant goal and decision pressures to architectural choices, explain the model and mechanisms those choices create, and state why the evidence supports them.

Goals name required capabilities or outcomes. Topics only name subjects. `Job Execution` can serve as a topic, while “reports must survive worker termination” supplies a goal or requirement. The section beneath `Job Execution` should establish how the design satisfies that requirement.

Use hidden conclusions to decide which evidence the design must expose. Readers may infer that the job system remains reliable or easy to operate, but the document must explicitly state the survival requirement, failure behavior, warrant, and concrete consequence.

## Establish the Design Immediately

Open with the problem's scope, controlling claim, and central architectural decision. Follow with enough of the operating model for the reader to understand how the choice addresses the goal.

Do not spend the opening on the document itself:

> This document describes the proposed job-processing architecture. The service does not yet implement the complete design.

Start with the architecture:

> To keep interactive requests responsive while large reports compile, the service separates request admission from report generation. The API records an immutable job and returns its ID immediately, while workers claim queued jobs and publish completed reports to object storage.

The first sentence states the goal and decision. The second explains the resulting execution model. The reader can infer responsiveness, while the immediate response and durable job ownership provide the explicit reasoning needed to evaluate it.

Do not replace the first sentence with the second. The goal tells readers why the API returns before compilation. The mechanism shows that the decision can satisfy it.

Avoid openings that announce only a topic, lead with a generic status disclaimer, present an agenda, or list favorable qualities without evidence. Use scope, audience, or lifecycle metadata when readers need it without displacing the opening claim.

## Connect Sections Through Contribution and Dependency

The controlling claim and selected document contract govern the body. A section may define a concept, record a decision, explain an ownership boundary, detail a representation, supply evidence and its warrant, trace a transformation, describe an interaction, or follow a consequence into another subsystem.

Test each major section privately:

```text
Contribution:
The document needs this section because it establishes ______
needed to understand, evaluate, trust, or accomplish ______.

Dependency:
This section assumes ______ has already been established
and enables the later discussion of ______.
```

When the contribution fails, remove or combine the section. When the dependency points to a concept established later, reorder the document. Do not expand the introduction into an index merely to justify every supporting section.

Headings name topics. Section openings make points about those topics. A document-level claim can support many sections, so do not force each subsection to invent another goal.

## Order Claims by Dependency

Develop the body in the order the reader needs to reason about the design. Common claims include:

1. **Goal and authority** establish what the system must accomplish and which source or subsystem owns each fact.
2. **Central concepts** establish unfamiliar or specialized semantics needed by later decisions.
3. **Model and representation** define the objects, identities, relationships, inventories, and invariants used throughout the design.
4. **Interaction** shows how users, editors, APIs, or neighboring systems observe and change the model.
5. **Transformation** explains how accepted inputs become resolved values, artifacts, or runtime state.
6. **Lifecycle** connects creation, validation, loading, mutation, persistence, recovery, and removal.
7. **Failure behavior** identifies which boundary rejects invalid state and how valid work continues.
8. **Decision pressure** records the tradeoff that selected the design and the evidence that could overturn it when the document contract requires evaluation.

This sequence supplies reasoning order rather than mandatory headings. Combine claims that form one point, omit stages the selected profile does not need, and define the complete model before its projections and transformations.

## Record Why Decisions Hold

Connect each major decision to the objective or pressure that selected it, the mechanism that implements it, and the concrete consequence it creates.

> The render graph assigns every transient texture a lifetime interval. Textures with non-overlapping intervals share GPU memory, reducing peak allocation without changing which pass owns each resource.

The lifetime condition warrants the aliasing decision and identifies when it remains valid. A future reader can reevaluate the decision if resource lifetimes or ownership rules change.

## Match Uncertainty to the Document Contract

An architectural overview presents the golden operating model as fully realized architecture and keeps unresolved design qualifications beside affected claims. An RFC exposes alternatives, risks, validation criteria, and acceptance-relevant open questions. An ADR records one decision's status and consequences. An implementation plan sequences work against an already accepted target.

Do not add generic feasibility, goals, non-goals, alternatives, risks, open questions, or implementation sections automatically. Add the reasoning required by the selected profile and actual claim.

## Keep Status Local and Durable

An architectural overview records the golden design as an operating contract. Write settled architecture as fully realized behavior even when implementation remains incomplete. Do not label the overview or its settled claims as intended, proposed, aspirational, or not yet implemented merely to reflect repository progress.

Mark unresolved design decisions where they occur. Describe current implementation behavior only when the controlling claim depends on a migration boundary or an incompatibility with the golden design, and keep that evidence subordinate to the operating contract.

Verify necessary claims about current behavior against code or authoritative documentation. Attach `accepted` or `open` to the decision it qualifies when the contract requires that distinction. Use document metadata for lifecycle status when the contract requires it, including ADR status and supersession. Do not add lifecycle metadata to an architectural overview solely because implementation lags behind the design.

## Use Structure Only When It Carries Information

Use the visible hierarchy to expose architectural boundaries and dependency order. Read [`document-structure.md`](document-structure.md) for heading selection, naming, containment, and compression.

Use Mermaid for ownership, state transitions, hierarchy, branching, or multi-stage flows. Use tables for repeated mappings such as authored asset type → cooked artifact → runtime consumer.

Use a concrete representation example when the design claims support for non-obvious composition, identity, constraints, references, or overrides. Explain what the example proves rather than leaving the relationship implicit.
