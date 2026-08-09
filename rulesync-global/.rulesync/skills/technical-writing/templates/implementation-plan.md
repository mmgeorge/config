# Implementation Plan Profile

*Read when sequencing an accepted design into concrete engineering work, migration, validation, and rollout.*

## Controlling Claim

Establish that a dependency-ordered sequence can move the implementation from its verified current state to the accepted target architecture without losing required behavior or validation coverage.

## Reasoning Obligations

- State the accepted target and the current implementation gap.
- Identify owners, affected boundaries, dependencies, and invariants.
- Order work so each phase produces the prerequisites consumed by the next.
- Separate structural changes from migrations, compatibility work, and cleanup.
- Define focused tests, integration checks, generated-artifact validation, and runtime verification.
- Include rollout, recovery, or fallback only when operational state changes require them.
- State completion criteria that prove the target architecture exists.

## Default Progression

Move from current and target states through affected boundaries, dependency-ordered work, migrations, validation, rollout where needed, and completion criteria.

## Exclude by Default

Do not reopen accepted architectural choices inside task sequencing. Link the RFC, ADR, or overview when rationale matters.

## Completion Check

An implementer should know what to change, in which order, why the order matters, how each boundary will be verified, and what evidence marks the work complete.
