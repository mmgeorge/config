# Architecture Decision Record Profile

*Read when recording an architecturally significant decision for future maintainers.*

## Controlling Claim

Establish that one architectural decision directly responds to documented technical constraints and requirements, defining explicit consequences, tradeoffs, and system boundaries.

## Structural Obligations

Every Architecture Decision Record (ADR) must contain the following sections in order:

### 1. Title and Metadata
- Use a short, noun-based title identifying the decision (such as `ADR-004: In-Memory Draft Buffer for Offline Sync`).
- Record current lifecycle status (`Proposed`, `Accepted`, `Rejected`, `Superseded`, or `Deprecated`).
- Record date, authors, reviewers, and any prior ADR superseded by this record.

### 2. Context and Problem Statement
- Describe the current system baseline and the specific technical or organizational problem being solved.
- State competing constraints, performance requirements, and operational boundaries objectively without biasing the description toward the chosen solution.

### 3. Decision
- State the chosen architectural outcome in active, present-tense language.
- Define exact ownership boundaries, affected modules, state representations, and communication protocols.

### 4. Considered Alternatives and Rejection Rationale
- Enumerate viable alternative approaches evaluated against the same criteria.
- State why each rejected alternative failed the requirements or imposed unacceptable operational costs.

### 5. Architectural Consequences
- **Positive:** Capabilities enabled, performance improvements, and reduced complexity.
- **Negative:** Accepted operational overhead, latency costs, migration burdens, or new failure modes.
- **Neutral:** Secondary requirements or architectural patterns required by this decision.

### 6. Validation and Invariants
- State how the decision will be verified (such as unit tests, load tests, static linting, or architectural fitness functions).

## Exclude by Default

- Do not expand an ADR into an exhaustive system overview, API manual, or task-level implementation plan. Link those documents when they exist.
- Do not reopen settled decisions without presenting new technical constraints or measured failures.

## Completion Check

A future maintainer reading this ADR should understand what changed, why the decision was selected over alternatives under the recorded constraints, what operational costs were accepted, and how the decision is verified.
