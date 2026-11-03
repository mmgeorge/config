# Specification Profile

Read before authoring, reviewing, or relaxing a format, protocol, schema, or conformance specification. Specify the constraints that independent implementations need to interoperate; do not restate every possibility already supplied by the underlying format.

## Main Claim

Independent implementations can determine conformance from the specification's definitions and necessary constraints. Behavior already defined by referenced formats remains with those formats unless this specification explicitly changes it.

## Relax by removing restrictions

When asked to relax a specification, remove or narrow the restrictive wording first. Do not automatically replace a prohibition with an affirmative permission or a catalogue of alternatives.

For example, change:

> Absolute path of the non-nullable point x-coordinate column. A missing geometry must use `NaN`.

to:

> Absolute path of the point x-coordinate column.

Do not replace it with:

> The column may be nullable or non-nullable, and `NaN` may also represent an indeterminate value. Neither representation is mandatory.

The replacement enumerates possibilities instead of defining a necessary constraint. If the underlying format already governs nullability, validity, and numeric representation, leave those mechanics there unless this specification intentionally changes them.

## State only contract-relevant distinctions

Keep a sentence when it defines meaning, imposes an interoperability constraint, or resolves an ambiguity that the referenced standards leave open. Remove sentences that merely exhaust the possibilities, such as a value being null or non-null, without changing how a reader or writer must behave.

- Describe a field's role directly instead of appending every representation it could use.
- State a conditional requirement once, where it belongs. For example, an extent requirement that applies only when at least one geometry has a determinate location is a real constraint.
- Do not repeat parent/child validity rules, primitive type behavior, or other inherited storage semantics unless an extension changes their interpretation. Link to the owning standard when a reference is needed.
- Do not add a sentinel, default, fallback, or validation requirement merely because the implementation currently uses it.

Omission and explicit null are not interchangeable in every format. Explain their distinction only when the contract needs it; do not assume that deleting prose overrides a normative schema or an inherited restriction.

## Review the resulting contract

Read the edited section as a complete contract, not as a record of the discussion. Remove stale cross-references, duplicated qualifications, and examples that reintroduce a deleted restriction.

Before finishing, check:

1. What requirement does each remaining sentence add? If none, remove it unless readers need it to interpret the contract.
2. Did relaxing one constraint introduce new rules elsewhere? Remove those additions unless the request requires them.
3. Do normative schemas and examples still impose the removed restriction? Update directly affected declarations rather than compensating with contradictory prose.

A prose cleanup is not automatically a request to change validators, writers, or public APIs. Distinguish an editorial removal of redundant wording from a behavioral contract change, and make implementation changes only when requested or required by that agreed change.
