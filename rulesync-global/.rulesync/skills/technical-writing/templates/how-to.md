# How-To Guide Profile

*Read when authoring operational runbooks, incident response procedures, migration instructions, or task-oriented guides.*

## Controlling Claim

Establish that following the documented sequence deterministically achieves one specific operational result under defined prerequisites and environmental constraints.

## Structural Obligations

Every how-to guide must progress through the following sections:

### 1. Target Outcome
- Name the exact resulting state or artifact produced by completing the guide (such as `Rotates the production database credentials and updates the secret manager with zero downtime.`).

### 2. Prerequisites and Environment
- List required tooling, exact version constraints, and required environment variables.
- List required permissions, IAM roles, or credential scopes.
- Highlight any destructive operations or irreversible side effects in an explicit warning block before proceeding.

### 3. Execution Sequence
- **Step Independence:** Sequence steps in strict execution dependency order.
- **One Observable Action per Step:** Give each numbered step exactly one primary action.
- **Actionable Formatting:** Begin each step with an active imperative verb (such as `Deploy`, `Export`, `Configure`, `Verify`).
- **Commands and Code:** Provide exact copyable commands, file paths, and configuration snippets. Place optional parameters and explanations directly beneath the main command.

### 4. Checkpoints and State Verification
- Provide expected stdout, HTTP status codes, or file system artifacts after critical actions.
- Include a specific verification command that proves the step succeeded before the reader advances to the next step.

### 5. Conditional Branches
- Address environment or configuration divergences (such as macOS vs Linux paths) at the exact step where the branch occurs, not in separate disconnected sections.

### 6. Failure Modes and Recovery
- Provide a troubleshooting table or section covering expected failure states:
  - **Symptom / Error Text:** The exact searchable error string or status code.
  - **Root Cause:** The underlying trigger (such as expired token or port collision).
  - **Remediation Action:** The exact recovery command to restore operational state.

### 7. Author Verification Requirement
- Execute the complete guide from the declared starting state in a clean, isolated environment before publishing.

## Exclude by Default

- Do not include theoretical deep dives, architectural debates, or historical design rationale in the workflow. Link a dedicated explanation document instead.
- Do not provide a broad menu of alternative approaches. Document the single standard, verified procedure.

## Completion Check

A practitioner with the stated prerequisites should be able to execute the steps without ambiguity, verify intermediate checkpoints, reach the target state, and recover from documented error conditions.
