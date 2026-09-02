# Technical Explanation Profile

*Read when authoring deep dives, conceptual overviews, mechanism analyses, or mental model explanations.*

## Controlling Claim

Establish a causal and conceptual model that explains why a system behaves as observed, enabling readers to predict outcomes when inputs, configurations, or failure conditions change.

## Structural Obligations

An explanation must progress through the following sections:

### 1. Observed Behavior or Core Question
- State the specific runtime phenomenon, system behavior, or concept interaction being explained.
- Establish the observable symptoms, performance characteristics, or architectural invariants under discussion.

### 2. Governing Concepts and Dependency Order
- Define prerequisite concepts in strict dependency order before explaining their interactions.
- Anchor unfamiliar abstractions to established software concepts (such as mapping a custom consensus protocol to standard Raft concepts).

### 3. Execution Mechanics and Causal Tracing
- Trace causes, transformations, state mutations, and consequences explicitly.
- Detail the exact step-by-step mechanism (such as how a distributed transaction coordinator handles network partition recovery).
- Connect intermediate data structures to the resulting system output.

### 4. Empirical Evidence and Counterexamples
- Provide compact code traces, log output, or benchmark measurements that validate the causal model.
- Include a concrete counterexample showing what happens when a boundary condition is violated (such as demonstrating cache stampede behavior when lock acquisition fails).

### 5. Boundary Conditions and Non-Applicability
- Explicitly state the boundaries where the model no longer applies.
- Identify edge conditions, scale thresholds, or alternate subsystem fallbacks that take over outside these boundaries.

## Exclude by Default

- Do not turn an explanation into a step-by-step operational runbook. Link a dedicated how-to guide for procedural workflows.
- Do not list exhaustive API reference members. Link the reference documentation.
- Do not include proposal justification or roadmap planning.

## Completion Check

A reader should be able to explain the underlying mechanism in their own words, predict system behavior when inputs or environmental conditions change, and recognize boundaries where the explanation stops applying.
