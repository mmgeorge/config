# Consistency

*Read when enumerating architectural members across prose, tables, diagrams, directory trees, schemas, or examples, tracing representations or identifiers across lifecycle boundaries, or claiming support for a non-obvious relationship.*

Use [`terms.md`](terms.md) to admit, define, emphasize, and maintain first-class vocabulary. This reference owns agreement among the inventories and representations that use those terms.

## Reconcile Declared Categories

A table of asset types, a union of command variants, a diagram of processing stages, a list of system actors, and a parallel series of type-like nouns in prose all declare architectural inventories. Readers reasonably treat them as complete within the document's stated scope.

Before accepting a parallel series, state the governing question privately and verify that every member answers it at the same abstraction level. A policy, operation, consumer, or consequence that governs the members does not belong beside them as another member.

Scan later sections for nouns claiming membership in each inventory. For every new member, choose one action:

- Add it to the defining inventory.
- Define it as a composition or specialization of existing members.
- Narrow the inventory's stated scope.
- Change the later term so it no longer claims membership.

Do not let a later section introduce a “world-composition asset” after an earlier table claims to enumerate authoring assets without reconciling the two.

Apply the same check across representations. A diagram, table, prose list, schema union, and directory tree describing the same category must agree on its members and terminology.

## Trace Representations Across Transformations

When a declaration, template, schema, command, build input, or other source representation produces another representation, identify the concepts on both sides of the transformation. Use one term for both sides only when identity, lifecycle, meaning, and cardinality remain invariant across every supported case.

For each transformation, establish:

- The input concept
- The output concept
- The subsystem that performs the transformation
- Whether one input produces zero or one, exactly one, many, combines with other inputs into one output, or participates in a many-to-many relationship
- Which identity, if any, survives the transformation

Ground every output concept in code, a schema, a format contract, established domain language, or an explicit design decision. Generic suffixes such as `record`, `model`, `descriptor`, `entry`, and `result` do not establish a representation by themselves. Define what contains, resolves, and consumes the output before using it as a transformation target.

Audit `one`, `single`, `each`, `the`, `instance`, and `realization` as cardinality claims. Qualify instance terms by the concept instantiated when several lifecycles could fit the unqualified word.

Give declarations, templates, specifications, produced instances, and loaded objects separate terms when they differ in lifecycle, identity, or cardinality.

When data crosses authoring, build, storage, network, process, or runtime boundaries, identify the reference used at every stage. Do not carry an identifier into another stage merely because it names a related representation.

For each ID, key, hash, or handle, establish:

- What it identifies
- Which subsystem creates it
- Which subsystem resolves it
- Which lifecycle stages may use it
- Whether edits, rebuilds, releases, or process restarts change it
- Whether it names a logical object, immutable version, storage location, or loaded instance
- Which transformation converts it into the next stage's reference

Treat `stable`, `persistent`, `durable`, and `permanent` as guarantees rather than decorative modifiers. State which lifecycle boundary the guarantee crosses and which edits, rebuilds, migrations, releases, or restarts preserve or replace the reference.

For every claim that cooking, compilation, export, migration, or another transformation preserves or removes information, name the source information, the resulting representation, the selection rule, and the later operation that requires the surviving information. Supply both a retained and removed example when the filtering boundary would otherwise remain abstract.

Audit examples from the consumer's boundary backward. A stored field, API parameter, schema property, or code sample must use the concept and reference valid for its consumer's stage, state how many outputs the source may produce, and name any conversion required to reach that stage.

## Prove Non-Obvious Representations

Use a concrete example when prose claims that a representation supports a relationship the reader cannot verify from familiar syntax alone. Common triggers include:

- Composition or inheritance
- Stable identity across files or instances
- Local overrides and inherited values
- Cross-object references
- Validation across fields
- Attachment or compatibility contracts
- State transitions or recovery

Use the smallest example that exercises the relationship. Follow it with a sentence identifying the fields, operations, or transitions that establish the claim.

Do not add a code block merely because the document names a format. Familiar syntax without a non-obvious relationship carries no architectural evidence.
