# RFC and Proposal Profile

*Read when proposing a major architecture change, technology adoption, API redesign, or cross-cutting engineering strategy.*

## Controlling Claim

Establish that the proposed technical design best satisfies stated goals and constraints when evaluated against viable alternatives, recorded drawbacks, system risks, and prototype evidence.

## Structural Obligations

An RFC must contain the following sections:

### 1. Motivation and Problem Definition
- Describe the concrete use cases, system bottlenecks, or user friction prompting the proposal.
- Document the operational cost and technical limitations of maintaining the status quo.

### 2. Goals, Non-Goals, and Evaluation Criteria
- **Goals:** Measurable capabilities or invariants the proposal must deliver.
- **Non-Goals:** Explicitly out-of-scope capabilities to prevent scope creep.
- **Evaluation Criteria:** Weighted dimensions (such as Latency, Durability, Backward Compatibility, Developer Velocity) used to judge solutions.

### 3. Proposed Solution
- **User-Facing Surface:** Show concrete CLI invocations, API code samples, or configuration examples before describing internal machinery.
- **Reference-Level Architecture:** Detail component ownership, state machines, data schemas, protocol definitions, and concurrency models.
- **State Invariants:** Explicitly define invariants preserved across failure and recovery paths.

### 4. Evaluated Alternatives and Tradeoffs
- Enumerate viable alternative approaches evaluated against the shared criteria.
- Provide a side-by-side comparison table showing where each alternative excels and why it failed the primary requirements.

### 5. Drawbacks, Security Risks, and Mitigations
- Document new failure modes, resource overhead, operational complexities, or security attack surfaces introduced by this design.
- Define explicit mitigation strategies for every identified risk.

### 6. Migration and Compatibility Strategy
- Document backward compatibility guarantees, schema evolution rules, and deprecation schedules.
- Detail zero-downtime data migration and fallback rollback mechanisms.

### 7. Unresolved Questions and Validation Plan
- State explicit open questions that affect reviewer acceptance.
- Define the proof-of-concept prototype, benchmark, or experiment required to validate uncertain claims before implementation.

## Exclude by Default

- Do not include task-level Jira tickets or engineering checklists. Those belong in an implementation plan after RFC acceptance.
- Do not make unsupported assertions (such as *"This approach is vastly superior"*). Connect every claim to measurable evaluation criteria.

## Completion Check

A reviewer should be able to identify the exact decision requested, test the proposal against explicit criteria, understand its mechanics and costs, compare it fairly with alternatives, and identify what evidence remains required before approval.
