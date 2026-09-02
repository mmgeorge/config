---
name: technical-writing
description: >-
  Writes and revises technical documents and code documentation, including
  architectural overviews, RFCs, reference docs, READMEs, API docs, docstrings,
  rustdoc, and code comments. Use when prose must explain a system, define a
  caller contract, or preserve a non-obvious code constraint.
---
# Technical Writing

Write for technically proficient readers. When audience context is unstated, address a developer who understands general software engineering but does not know this specific repository. Keep prose direct, accessible, precise, and concise.

A document contract connects what the reader must accomplish with what the document type must prove. Establish one controlling claim and include only the definitions, evidence, reasoning, and consequences required to support it.

Match document depth to the target format: multi-section designs require a domain model and reading path, API docs require caller contracts, and code comments require local constraint rationale.

## Engineering Writing Sequence

1. **Select Profile and Define Contract:** Identify reader intent, technical background, operating environment, target outcome, document profile, and controlling claim.
2. **Inspect Authoritative Sources:** Verify facts against implementation source code, schemas, tests, traces, or runtime behavior before asserting system behavior.
3. **Model the Subject and Dependencies:** Outline the argument from prerequisites, supporting claims, evidence, warrants, boundary conditions, and consequences. Order prerequisites before dependent claims and mechanisms before consequences.
4. **Draft with Functional Prose:** Draft each section, API item, or comment around one primary point. State goals, decisions, requirements, mechanisms, limits, and concrete effects directly using semantic identifiers.
5. **Verify and Audit:** Test promised execution outcomes, then run the [Finish and Audit Pass](#finish-and-audit-pass).

Ground audience assumptions in the task prompt, existing documentation, or repository context. State unverified audience assumptions explicitly rather than inventing fictional user personas.

For API documentation and code comments, inspect implementations, callers, and tests. Focus the prose strictly on the caller contract or local code constraint.

## Document Profile Selection

Read the matching profile before drafting or restructuring:

| Reader Objective | Document Profile |
| --- | --- |
| Understand a system or subsystem architecture | [Architectural overview](templates/architectural-overview.md) |
| Propose or evaluate a technical change or strategy | [RFC and proposal](templates/rfc.md) |
| Record an architecturally significant decision | [Architecture decision record](templates/adr.md) |
| Sequence accepted designs into engineering tasks | [Implementation plan](templates/implementation-plan.md) |
| Understand causal mechanics, mental models, or concepts | [Technical explanation](templates/explanation.md) |
| Execute an operational procedure or runbook | [How-to guide](templates/how-to.md) |
| Look up an exhaustive interface, CLI, schema, or error catalog | [Reference](templates/reference.md) |
| Learn a technology by building an end-to-end project | [Tutorial](templates/tutorial.md) |
| Predict how a public API, docstring, or endpoint behaves | [API documentation](templates/api-documentation.md) |
| Preserve a non-obvious local code invariant | [Code comment](templates/code-comments.md) |

A README is a file container rather than a distinct document type. Select a profile based on what the reader must accomplish. Default unspecified system or feature designs to an architectural overview.

An architectural overview describes the target architecture in the present tense as fully operational behavior. Qualify only unresolved claims or temporary migration boundaries. Keep RFC deliberation, task checklists, and backlog out unless the prompt explicitly requests them.

## Write for Fast Understanding

Apply these composition rules to keep technical explanations direct, scannable, and precise:

- **Direct Openings:** Begin with the behavior, problem, decision, or result that gives the subject meaning. Sustain attention through technical consequences, concrete mechanisms, verifiable examples, and visible progress. Omit conversational filler, marketing adjectives, contrived anecdotes, and delayed conclusions.
- **Paragraph Unity and Transitions:** Make each paragraph establish one point. Open each expository paragraph with that point, use the body to define or support it, and end on its strongest consequence or required transition. Connect sequential paragraphs with explicit transitions: continuation, qualification, contrast, or causal consequence.
- **Active Voice and Agency:** Prefer active voice for operations, with the responsible subsystem or component as the subject. Use passive voice when the receiver, result, or unknown entity carries the topic. State claims directly and concretely. Remove filler words, nominalizations, passive abstractions, and introductory wind-ups.
- **Coordinate Parallelism and Modifier Placement:** Vary sentence form to expose real relationships instead of repeating loose chains joined by `and`, `but`, or `so`. Express coordinate ideas in parallel grammatical form. Keep subjects near their verbs, modifiers beside what they modify, and pronouns near clear antecedents. Make every introductory phrase modify the grammatical subject that follows.
- **Tense and Emphatic Endings:** Keep a summary in one governing tense. Put new or emphatic information near the end of the sentence, paragraph, or section unless the beginning needs deliberate emphasis.
- **Punctuation Standards:**
  - Form singular possessives with `'s` unless house style requires another form. Keep possessive pronouns free of apostrophes.
  - Use the serial comma by default.
  - Enclose parenthetical material with both delimiters. Distinguish nonrestrictive clauses from restrictive clauses.
  - Put a comma before a coordinating conjunction that joins independent clauses. Otherwise use a period. Do not create comma splices.
  - Use fragments only for deliberate, unmistakable emphasis.

## Keep Prose Clear and Functional

Technical documentation must remain concrete, actionable, and free of literary decoration. Apply these rules across all sections:

- **Ban literary and mechanical metaphors:** Use software architecture constructs (pipelines, state machines, lifecycles, and contracts) instead of storytelling or structural metaphors (arcs, spines, load-bearing relationships, or machinery).
- **Ban anthropomorphism and theatrical personas:** Do not attribute cognitive traits to models (*"the agent is already smart"*), assign theatrical roleplay personas (*"act as a ruthless QA tester"*), or use physical journey metaphors (*"robot on a path"*, *"narrow bridge with cliffs"*). Frame instructions around operation fragility, context budgets, and input/output contracts.
- **Ban developer slang and informal shorthand:** Eliminate colloquial idioms (*"loud and obvious"*, *"gotchas"*, *"hand-roll"*, *"voodoo constants"*, *"scripts that punt"*, *"eat your own dog food"*). Use formal systems architecture terms: explicit error notification routing, unvalidated edge cases, unexplained magic constants, and unhandled process failures.
- **Ban provider and interface leakage in shared skills:** Do not hardcode specific provider brand names (*"when Codex works with..."*) or chat UI slash commands (`/gpu-debug`) in portable skills. Write third-person functional triggers (`"Use when authoring, modifying, or debugging..."`) and reference underlying skills by name.
- **Ban promotional marketing and decorative styling:** Do not use marketing qualifiers (*"20+ supported tools"*, *"battle-tested"*, *"lightning-fast"*, *"Why [Product]?"*) or emoji decorations (`✅`, `🚀`, `⚠️`) in technical checklists and headings. Present capabilities as factual, third-person technical inventories.
- **Replace aphorisms with mapping rules:** State explicit conditions and consequences rather than high-level slogans.
- **Eliminate subjective thresholds:** Replace vague sufficiency words (`enough`, `materially`, `meaningful`, `practical`) and soft hedges (`mostly`, `usually`, `several`, `likely`) with explicit bounds, triggers, or verification environments.
- **Ban subjective difficulty tiers:** Do not classify tasks or audiences with labels like `basic`, `advanced`, `trivial`, or `complex`. State the exact prerequisite tools, environment versions, and permissions instead.
- **Ban announcement prose:** Omit mechanical transition sentences that merely introduce an upcoming artifact (such as *"The following table lists..."* or *"Below is an example of..."*). State the invariant, constraint, or takeaway in the preceding prose.
- **Ground generic placeholders:** Replace catch-all words (`things`, `details`, `another mechanism`) and vague importance modifiers (`important`, `critical`, `major`, `credible`) with exact domain entities and attributes.
- **Demonstrate qualitative claims:** Do not assert properties such as simplicity, reliability, or maintainability directly. Prove them with error recovery paths, resource bounds, reduced coordination, or isolated change boundaries.
- **Use precise stylistic terms:** Replace editorial idioms (throat-clearing, circle back, loading the description) with direct grammatical directives.

### Clear and Functional Replacements

| Category | Non-Functional Phrasing | Clear and Functional Replacement |
| --- | --- | --- |
| **Metaphors** | *"Build a claim spine along the dominant arc."* | *"Outline the argument using a pipeline, state machine, or component-synthesis structure."* |
| **Anthropomorphism** | *"The agent model is already smart, so act as a ruthless QA tester."* | *"Define explicit input schemas and evaluate edge cases with malformed payloads."* |
| **Physical journey metaphor** | *"Think of the agent as a robot on a path with cliffs."* | *"Match instruction specificity to operation fragility (open guidelines vs exact commands)."* |
| **Developer slang** | *"Make error handling loud and obvious without voodoo constants."* | *"Report caught errors via user notifications and document numeric parameter derivations."* |
| **Restricted domain term** | *"Identify every concurrent actor and the state each actor modifies."* | *"Identify every concurrent thread or worker and the state each modifies."* |
| **Provider / UI leakage** | *"Use when Codex needs to debug, then escalate to /gpu-debug."* | *"Use when debugging running engine sessions (use the gpu-debug skill for shader capture)."* |
| **Promotional marketing** | *"Generates configs across 20+ tools — battle-tested and lightning-fast."* | *"Generate configurations across supported target assistants from a single .rulesync/ source."* |
| **Aphorisms** | *"Scale the work to the output."* | *"Match document depth to format: multi-section designs require a domain model, API docs require caller contracts."* |
| **Subjective thresholds** | *"Keep the flow short, usually no more than a few nodes when practical."* | *"Limit the longest directed path to six nodes or fewer."* |
| **Difficulty tiers** | *"This advanced tutorial is not for trivial tasks."* | *"Prerequisites: Node.js 20+, Docker daemon running, and write access to the deployment registry."* |
| **Announcement prose** | *"The following code snippet shows how to configure the client:"* | *"Configure the client with an explicit retry policy to prevent connection exhaustion:"* |
| **Circular triggers** | *"Qualify only the migration boundary that needs qualification."* | *"Qualify only claims with unverified dependencies or temporary migration boundaries."* |
| **Generic nouns** | *"Show interactions between two important things."* | *"Show interactions between two components or stores."* |
| **Asserted quality** | *"The cache provides a simple, robust, and highly reliable architecture."* | *"The cache eliminates distributed locks by isolating write mutations to a single worker queue."* |
| **Editorial idioms** | *"Circle back after clearing the throat."* | *"Revisit earlier assumptions after removing introductory filler."* |

## Structure Arguments with Demonstrable Proof

A claim states what the reader should accept or accomplish. Evidence supplies facts, runtime behavior, data schemas, code examples, or benchmark measurements. A warrant explains why that evidence supports the claim. State the warrant when the causal link is non-obvious or carries architectural rationale.

State why a capability matters, why recorded constraints require a decision, why a mechanism satisfies a requirement, what consequences follow, and where the claim stops holding. Attach qualifications to the affected claim instead of weakening the document with a generic disclaimer.

Compare alternatives only when the document profile or decision requires it. Apply one shared set of criteria, describe competing mechanisms at the same level of detail, preserve real operational costs, and explain why recorded requirements select the chosen outcome.

Do not assert qualitative claims (such as simplicity, reliability, or maintainability) directly. Demonstrate them through concrete mechanisms:
- Prove **reliability** with explicit failure recovery paths and fallback states.
- Prove **performance** with measured resource bounds or reduced execution steps.
- Prove **simplicity** with eliminated coordination or state synchronization.
- Prove **maintainability** with isolated change boundaries and modular ownership.

## Control Terminology and Domain Models

Introduce a specialized term only when its exact meaning carries an architectural claim or the reader must use it. Ground controlled vocabulary in code, a schema, an API, an established industry concept, or an explicit design decision. Keep descriptive phrases in ordinary prose.

Reserve `projection` strictly for coordinate spaces. Replace `materialization` and `actor` with the exact domain role (`thread`, `task`, `worker`, `system`, `process`, or `pipeline`).

Define an unfamiliar term at first use through its kind, role, and distinguishing boundary. Give each concept one stable name and one canonical definition location. Place the definition before the first claim that depends on it. Define critical system relationships by specifying their participating components, interaction protocol, output state, and failure modes.

Treat every parallel list, table, union, tree, diagram, and type-like series as a declared inventory. Give every member the same governing question and abstraction level. Reconcile any later member by adding it, defining it as a composition or specialization, narrowing the inventory, or changing its name.

For every data transformation, name the input, output, responsible subsystem, cardinality, and surviving identity. Use one name across the boundary only when meaning, identity, lifecycle, and cardinality remain unchanged.

For every public or persistent ID, key, hash, handle, URI, or path, state what it identifies, who creates and resolves it, where it remains valid, and which change replaces it. Treat `stable`, `persistent`, `durable`, and `permanent` as guarantees with explicit lifecycle boundaries.

Use a private terminology ledger when overlapping terms risk drifting across sections. Audit in both directions by merging duplicate names for one concept and splitting names with multiple meanings.

## Build the Reading Path

Model the subject before choosing headings. Keep three structures distinct:

- The **subject model** records concepts, owners, boundaries, representations, transformations, lifecycles, and failures.
- The **knowledge dependencies** record what readers must understand first.
- The **reading path** orders orientation, definitions, evidence, reasoning, and consequences for the reader's objective.

Assume every page may be the reader's entry point. Provide local context to establish scope, prerequisites, result, and next destination before detailing mechanics.

Choose a primary organizational structure:

- **Component-synthesis:** Explain how individual subsystems assemble to produce overall system behavior.
- **Bottom-up:** Define core data structures, primitives, and storage engines before detailing high-level workflows.
- **Top-down:** Start from public interfaces and caller boundaries before detailing internal subsystems.
- **Linear:** Trace sequential pipelines, state machine lifecycles, and ordered data transformations.
- **Causal:** Diagnose observed runtime behaviors or trace failure recovery mechanisms.
- **Decision-oriented:** Structure content around requirements, evaluated alternatives, decisions, and consequences.
- **Procedural:** Guide readers through step-by-step operational workflows with checkpoints.
- **Categorical lookup:** Organize exhaustive APIs, configuration keys, or error codes for rapid indexing.

Within each section, organize information by the relationship being explained: definition, composition, data transformation, lifecycle, taxonomy, contract, or decision tradeoff. State a subsystem's purpose before its internal implementation. Put inputs before transformations, representations before projections, initial state before mutation, and mechanisms beside their failure behavior.

Keep a heading only when readers navigate directly to its point. Use heading depth for conceptual containment, keep siblings at one abstraction level, and name domain concepts rather than planning stages. Merge repeated claims.

Organize multi-document sets as a linear series for sequential workflows, a tree hierarchy for nested subsystems, or a cross-linked graph for modular guides. Give each topic one primary page with cross-links. Split a page when readers need to retrieve, update, link, or verify its topic independently.

After structural edits, verify introductory scope, transitions between sections, prerequisite ordering, complete item inventories, and concluding summaries. Rewrite surrounding prose when the model changes.

## Supporting Engineering Artifacts

Use minimal code or data examples that demonstrate the specific constraint or interaction. Include all necessary input setup, highlight active fields or lines, and omit non-functional boilerplate.

For code, commands, configuration, input, or output:

- Decide whether the sample must run or only explain. Label incomplete fragments and pseudocode explicitly.
- Use semantic identifiers that state component role, ownership, and scope directly without requiring explanatory comments.
- State the purpose, starting state, versions, dependencies, permissions, services, inputs, replacement values, and expected result that affect use.
- Provide deterministic verification checkpoints: specify the exact command or test, expected exit status or output string, and actionable error symptom on failure.
- Keep input, code, and output synchronized. Preserve exact searchable error messages.
- Do not use an ellipsis to hide state, ordering, cleanup, error handling, or any step that changes the result.
- Run executable samples through the documented sequence during drafting. State the unverified boundary when execution is blocked by missing environment dependencies or credentials.

Read [Technical Tables Guide](references/tables.md) when deciding whether to use a table or reviewing column criteria, headers, and rows. Read [Engineering Diagrams Guide](references/diagrams.md) when deciding whether to use a visual or reviewing path depth, accessibility, node semantics, and edge labels.

## Finish and Audit Pass

Verify the promised result before polishing the prose. Check factual claims, terms, definitions, inventories, transformations, identifiers, qualifications, examples, links, and cross-references.

Then run a clear-and-functional review pass:
1. Strip out lingering metaphors, slogans, conversational idioms, and developer slang.
2. Remove anthropomorphic cognitive claims, theatrical roleplay, and physical terrain metaphors.
3. Replace subjective thresholds (`enough`, `materially`, `practical`) and difficulty tiers (`basic`, `advanced`, `trivial`) with concrete triggers, tool versions, or bounds.
4. Eliminate promotional marketing claims, superlatives, and decorative emojis in checklists.
5. Eliminate announcement prose preceding tables, code blocks, and diagrams.
6. Replace restricted domain words (`actor`/`materialization`) with concrete system roles (`thread`, `task`, `worker`, `system`, `process`, or `pipeline`).
7. Ground generic nouns (`things`, `details`) and importance modifiers (`important`, `critical`, `major`) in specific domain constructs.
8. Repair paragraph openings, adjacency, endings, sentence emphasis, parallelism, modifier attachment, pronoun reference, tense, punctuation, and needless words.

A polished document still fails when readers cannot achieve its promised result. Preserve the author's intentional tone and the repository's house style. If the user asked for an edit, make it. If the user asked for review or advice, remain read-only.

