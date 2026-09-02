# Architectural Overview Profile

*Read when authoring or revising a system architecture overview, subsystem design document, or feature architecture specification.*

## Controlling Claim

Establish that one complete operating model explains how the system satisfies its requirements, divides subsystem authority, represents domain entities, and moves state through its lifecycle.

Describe the target architecture in the present tense as fully operational behavior. Do not qualify the model with aspirational disclaimers (`intended`, `proposed`, `future`) unless describing a temporary migration boundary that directly affects current caller behavior.

## Structural Obligations

An architectural overview progresses through the following sections:

### 1. System Scope and Public Boundary
- State what capability the system provides at its user or caller boundary.
- Enumerate the participating primary subsystems and external dependencies.
- Define unfamiliar domain concepts immediately when introduced.

### 2. Domain Model and Entity Ownership
- Define core domain entities, their persistent identifiers, and state invariants.
- Explicitly define ownership boundaries: which subsystem creates, mutates, and deletes each entity.
- Detail data representations, serialization formats, and durability guarantees.

### 3. Subsystem Architecture and Storage
- Describe each subsystem by its primary role, public interface, and internal storage.
- Document storage engines, database schemas, in-memory caches, and isolation mechanisms.
- Explain the warrants behind core storage and boundary decisions.

### 4. Runtime Lifecycles and Data Pipelines
- Trace end-to-end dataflow from ingestion or caller request through transformations to persistence and response.
- Document state machine transitions, event notifications, and background worker queues.
- Detail synchronization protocols, cache invalidation rules, and consistency guarantees.

### 5. Failure Modes and Concurrency Boundaries
- Define the concurrency model (such as actor isolation, thread pools, async event loops, or mutex hierarchies).
- Document partial failure handling, network timeouts, retry policies, and automated recovery paths.
- Define resource limits, queue capacity bounds, and backpressure mechanisms.

### 6. End-to-End Workflow Synthesis
- Show how the individual subsystems and pipelines compose to fulfill the overarching system contract.

## Organizational Patterns

- **Component-Synthesis:** Open with the complete system boundary, decompose into participating subsystems, and synthesize their interactions.
- **Bottom-Up:** Define primitive entities, storage models, and protocols before detailing higher-level APIs and workflows.
- **Pipeline:** Sequence sections to follow the physical or logical dataflow from ingest to output.

## Exclude by Default

- Do not include proposal history, rejected alternatives, feasibility discussions, implementation roadmaps, rollout checklists, or backlog items. Those belong in an RFC or implementation plan.
- Do not expose low-level function signatures or variable names unless they define a public architectural boundary.

## Completion Check

A reader should understand what the system accomplishes, which subsystem owns each responsibility, how central entities relate, how inputs become durable runtime state, how failures are contained, and why the boundaries satisfy system requirements.
