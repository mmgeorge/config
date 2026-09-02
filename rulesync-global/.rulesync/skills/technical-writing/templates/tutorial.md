# Tutorial Profile

*Read when authoring guided tutorials where a learner builds a runnable project to master a specific technology or capability.*

## Controlling Claim

Establish that building one runnable, end-to-end project from scratch teaches the learner how central concepts interact and gives them a reusable engineering capability.

## Structural Obligations

A tutorial must progress through the following milestones:

### 1. Target Project and Acquired Capability
- State the concrete functional artifact the learner will build (such as `Build an event-driven task queue worker with Redis and Rust.`).
- Define the reusable engineering capability acquired upon completion.

### 2. Pinned Prerequisites and Environment
- Specify exact software versions, required compilers, CLIs, Docker images, and access credentials.
- State assumed background knowledge without using subjective labels (such as `Assumes familiarity with async Rust syntax and basic Redis commands.`).

### 3. Progressive Milestones
Structure the tutorial into small, verifiable milestones:

- **Milestone 1: Minimal Runnable Scaffold:** Initialize the repository, dependencies, and configuration. Provide an immediate verification command that proves the basic environment runs.
- **Milestones 2 through N: Feature Construction:**
  - Introduce core concepts strictly at the milestone where the learner implements them.
  - Provide complete, copyable code files or diffs with semantic identifiers.
  - End each milestone with a deterministic verification checkpoint (such as running a test or sending a curl request).

### 4. End-to-End System Verification
- Provide the final command that exercises the complete application from start to finish.
- Display the exact expected terminal output, log traces, or UI result that confirms success.

### 5. Capability Mapping and Next Steps
- Explain how the patterns implemented in the tutorial translate to production systems.
- Link relevant architecture overviews, explanations, and reference documents for advanced configuration.

### 6. Author Verification Requirement
- Execute the entire tutorial from a clean directory in an isolated environment before publishing to verify that every command and code snippet succeeds.

## Exclude by Default

- Do not provide multiple alternative implementation paths or configuration forks. Provide the single simplest working path to the target artifact.
- Do not interrupt the build flow with exhaustive API listings or deep theoretical proofs.

## Completion Check

A learner following the tutorial should successfully build the functional artifact, understand the purpose of each introduced concept, and possess a working template they can adapt to production projects.
