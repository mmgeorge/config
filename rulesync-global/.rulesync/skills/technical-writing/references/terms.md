# Terms

*Read when deciding whether a term belongs in the document, defining first-class vocabulary, testing whether a category definition covers its members, deciding how much explanation a term needs, applying bold emphasis, auditing terminology across representations, or distinguishing concepts and identifiers used at different lifecycle stages.*

## Admit Terms Deliberately

Introduce an unfamiliar or specialized term only when the controlling claim depends on its specific meaning. Replace it with a stable domain category when the reasoning remains unchanged, and omit it when the reader does not need it to understand or use the design.

Apply this admission check:

1. Identify the claim, decision, mechanism, or constraint that requires the term.
2. Replace the term with its stable domain category and test whether the reasoning changes.
3. Keep the specialized term when the design depends on its behavior or the reader must act on it.
4. Decide whether the intended audience will recognize the meaning used here.
5. Define a necessary unfamiliar term inline at first use, then give it a dedicated section when its explanatory load requires one.

Treat file extensions, tool and library names, protocols, encodings, vendor products, and project-specific terms as unfamiliar unless the audience definition establishes otherwise. Recognition does not establish relevance. A familiar implementation detail still belongs outside the document when changing it would leave the argument and architecture unchanged.

Use familiar domain language in an orienting passage when a specialized term would require a distracting explanation. Introduce the project term when the document can define it through the work it performs.

For example, a paragraph that names a compression codec without relying on its behavior should say `compressed payload`. The codec name adds no evidence and can change independently of the design. By contrast, if failure recovery depends on a **fencing token**, define it on first use as a monotonically increasing ownership generation that storage uses to reject stale writers. A later section can explain how writers acquire and persist that token.

## Ground Terms in the Model

Before promoting a noun phrase into first-class vocabulary, identify the authority that makes the concept part of the document's model. Use one of these statuses:

- **Existing:** Code, a schema, an API, a file-format contract, or established domain language already defines the concept.
- **Proposed:** The design deliberately introduces a necessary concept and defines its owner, boundary, representation, and relationships.
- **Descriptive:** The phrase describes the shape, behavior, or relationship of other concepts without naming an independent architectural subject.

Existing and proposed concepts may become controlled vocabulary. Keep descriptive phrases in ordinary prose. Do not promote them into bold definitions, conceptual headings, architectural inventories, type-like code spans, or labeled diagram nodes.

A precise description does not establish a new concept. Several queues may form a dependency graph, for example, but that relationship does not create a first-class `DependencyGraph` unless the implementation or design gives it distinct ownership, representation, or behavior. Name the section after the established subject and explain the graph relationship inside it.

When a genuinely necessary term lacks an existing authority, make the design decision explicit before using the term. Define what owns the concept, what it contains or controls, how other parts address it, and which neighboring concept it does not replace.

## Define Terms at First Use

Define every unfamiliar or project-specific term when it first appears. Establish what kind of thing it names, what role it plays in the current design, and which boundary distinguishes it from adjacent concepts. Every claim in the first passage must remain understandable from that initial definition.

Define now and expand later. A later section may add representation, mechanics, constraints, examples, and lifecycle behavior that the first passage does not need. It must not supply the basic meaning omitted at first use.

Do not lead with an encyclopedia definition. Explain the term through the work its concept performs in the design.

## Test Definition Integrity

Treat every definition as a claim about category membership. Establish the subject's kind, role, and distinguishing boundary, then verify that every asserted property belongs to the concept itself.

Separate the concept from its identifier, storage representation, referenced object, implementation target, inputs, outputs, dependencies, and states. When a sentence uses `X is Y`, confirm that `Y` identifies what kind of thing `X` actually names. If `Y` describes only a relationship, rewrite the sentence as that relationship.

- False definition: “A subscription is an email address stored by a campaign.”
- Corrected: “A subscription records a recipient's consent to messages from a sender. An email address provides one possible delivery destination.”

Test every predicate in a category definition against every known member. Keep only universal membership properties in the parent definition. Move common but optional properties into subtype-specific prose.

## Resolve Concept Dependencies

Decompose every specialized or compound term into the concepts its meaning assumes. Defining a specialization does not define its base concept. Establish each prerequisite before the specialized term carries load-bearing reasoning.

Assign every first-class concept one canonical definition location. Record its first load-bearing use and the concepts that use it as a prerequisite. An earlier orienting passage may provide the canonical definition when it establishes the concept's kind, role, and boundary. A later section can expand that definition without taking ownership retroactively.

When a section must define a concept and then explain an operation performed on it, name the section for the concept and nest the operation beneath it. An operation can lead the section only after an earlier passage has established its subject.

For example, `Cache Eviction` works as a section after the document has defined what a cache stores, who owns it, and which reads it serves. If the same section must introduce caches before comparing eviction policies, use `Caches` as the section and place `Eviction` beneath it. Defining an eviction policy does not define the cache that applies it.

## Define Relationships Operationally

Treat terms such as `composes`, `specializes`, `inherits`, `derives`, `adapts`, `implements`, and `resolves` as load-bearing concepts when later reasoning depends on their exact behavior. Define what each participant contributes, which rule combines or relates them, what concept results, and what happens when the relationship cannot be satisfied.

Do not promote a relationship into a first-class type merely because it needs explanation. Keep it descriptive unless code, a schema, established domain language, or an explicit design decision gives it independent ownership and representation.

- Labeled but unexplained: “The regional policy specializes the base policy.”
- Operational: “The regional policy imports the base fields, narrows the allowed storage locations, and adds a retention limit. The evaluator rejects the result when the restrictions leave no permitted location.”

## Cover the Category

When a term names a category, identify every known subtype or member used in the document before finalizing its definition. State the shared membership criterion rather than the characteristics of one representative example, then test every known member against that criterion.

If a member fails the test, broaden the definition, split the category, or reclassify the member. Do not silently preserve a definition that describes only the most familiar subtype.

Use examples from materially different subtypes when they clarify the category's range. Do not enumerate every subtype unless the list forms a closed architectural inventory.

For example, if a document classifies messages, files, and streams as inputs, defining an input as “a file submitted for processing” covers only one member. Define the shared relationship instead, such as “data supplied to a process,” then describe file-specific behavior where the design requires it.

Apply the same membership test to examples joined by `including`, `such as`, or a coordinated list. A policy that governs several members does not belong in the list of members merely because the document discusses both under one topic.

## Ground Identifiers and Guarantees

Do not introduce an ID, key, hash, handle, URI, path, or other reference until the design establishes its role. Do not introduce a record, descriptor, entry, model, or result as a transformation output until the design establishes that representation. Record:

- What it identifies or represents
- Which subsystem creates it
- Where it appears
- Which subsystem resolves or consumes it
- Which lifecycle stages may use it
- Which changes preserve or replace it

Do not infer an identifier because a later stage needs to refer to something. The actual reference may be a path, map key, content hash, database key, generated handle, or another representation with different scope and stability.

Treat `stable`, `persistent`, `durable`, and `permanent` as lifecycle guarantees. State the scope of the guarantee and the edits, rebuilds, releases, restarts, or migrations that preserve or replace the reference.

- Unsupported: “The compiler preserves stable job IDs.”
- Grounded: “The scheduler assigns an ID when it creates the queue entry. Retries preserve that ID until the entry reaches a terminal state, while a new submission receives another ID.”

## Measure Explanatory Load

A term carries high explanatory load when several later claims depend on understanding its behavior. Give it a dedicated introduction when one or more of these conditions apply:

- The intended audience may not recognize it.
- The project assigns it a specialized meaning.
- Its behavior differs materially from a familiar system.
- Several later sections rely on its semantics.
- A central architectural decision depends on capabilities unique to it.
- A compact example reveals relationships that prose would leave abstract.

Define a high-load term initially through its kind, role, and boundary. Expand it in a dedicated section by explaining what it does in this architecture, why the design uses it, which downstream concepts depend on it, and the smallest example that demonstrates its relevant behavior.

## Reserve One Term per Concept

Maintain a one-to-one mapping between first-class concepts and their names. Use one stable term for each concept, and reserve each defined term for that concept within the document.

Apply this invariant to terms that name types, assets, artifacts, roles, states, boundaries, interfaces, and lifecycle stages. Do not apply it mechanically to ordinary verbs, adjectives, or incidental prose. When established domain language already overloads a term, qualify every ambiguous use or choose a more specific term.

Build a private terminology ledger when a document introduces several related concepts:

```text
Term: The name used throughout the document
Meaning: The one concept it identifies
Status: Existing, proposed, or descriptive
Authority: The code, schema, API, format contract, domain standard, or design decision that establishes it
Anchor: The exact symbol, field, section, or decision where that authority appears
Definition location: The passage that first establishes its kind, role, and boundary
First load-bearing use: The first claim that depends on its specific meaning
Prerequisites: The concepts readers must already understand
Scope: Where the definition applies
Valid stages: Where the term or identifier may appear
Resolver: Which subsystem interprets the identifier
Stability: Which changes preserve or replace the identifier
Produced concept: Which representation, if any, this concept creates or becomes
Cardinality: How many outputs each input may produce
Excluded meanings: Nearby concepts that require different names
```

Audit terminology in both directions:

1. Find concepts that receive several names and select one term.
2. Find terms that carry several meanings and separate those concepts.
3. Apply each correction across prose, headings, tables, diagrams, schemas, and examples.
4. Recheck qualifiers and abbreviations so they preserve rather than blur the distinction.

## Emphasize First-Class Terms

Bold the first defining occurrence of a first-class term when visual emphasis helps readers locate the document's vocabulary. Do not bold every occurrence, familiar terminology, or words that merely seem important. Bolding never substitutes for a definition.

Audit emphasis in both directions. Treat each bolded noun phrase as a candidate first-class term and confirm that its authority, status, definition, and usage support controlled vocabulary. Remove the bolding when the phrase remains descriptive or lacks an architectural anchor. Then inspect the terminology ledger and bold any first defining occurrence whose absence makes the vocabulary materially harder to locate.
