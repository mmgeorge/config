# Implementation Plan Profile

*Read when sequencing an accepted design into concrete, dependency-ordered engineering tasks, migration steps, validation matrices, and rollout procedures.*

## Controlling Claim

Establish that a dependency-ordered sequence of verifiable tasks transitions the system from its verified current state to the accepted target architecture without regressions, lost data, or unmonitored failures.

## Structural Obligations

An implementation plan must progress through the following sections:

### 1. Architectural Scope and Delta
- State the accepted target architecture and reference the originating design document, RFC, or ADR.
- Explicitly define the gap between the current codebase state and the target architecture.

### 2. Affected Boundaries and Modules
- Enumerate the specific files, modules, packages, database schemas, or RPC contracts being modified.
- Identify data owners, module dependencies, and state invariants that must be preserved during the transition.

### 3. Phased Execution Sequence
Structure the work into strict dependency-ordered phases:

- **Phase 1: Domain Entities and Data Structures:** Add or modify types, enums, schemas, and invariants before changing consumer logic.
- **Phase 2: Core Engine and Storage:** Implement storage engines, state mutation logic, serialization, and background workers.
- **Phase 3: Public Interfaces and Call Sites:** Update caller-facing APIs, CLI commands, HTTP routes, and UI components.
- **Phase 4: Migration and Compatibility:** Execute database migrations, data backfills, and deploy temporary backward-compatibility adapters.
- **Phase 5: Cleanup and Deprecation:** Remove deprecated interfaces, dead codepaths, and temporary migration adapters after verification.

### 4. Verification Matrix per Phase
Define the exact verification steps required to declare each phase complete:
- **Unit Tests:** Specific tests and mocks that validate isolated components.
- **Integration Tests:** End-to-end boundary tests with real dependencies.
- **Schema and Artifact Validation:** Lint checks, generated schema comparisons, or binary build artifacts.
- **Performance Criteria:** Latency, memory, or throughput benchmarks when critical.

### 5. Deployment, Rollout, and Rollback
- Document feature flag gates, canary deployments, or phased traffic ramping.
- Define observability metrics, alerts, and log events that confirm operational health.
- Define unambiguous rollback criteria and the exact rollback procedure if errors exceed thresholds.

### 6. Definition of Done
- Enumerate the explicit observable conditions that mark the entire plan complete.

## Exclude by Default

- Do not reopen accepted design decisions or debate alternative architectures. Link the originating RFC or ADR for architectural rationale.
- Do not leave tasks as vague directives (avoid `Refactor error handling`). Specify the exact files, functions, and invariants changed.

## Completion Check

An engineering team should know what code to modify, the exact execution order, why that order is required, how each phase is tested, and how to verify complete delivery.
