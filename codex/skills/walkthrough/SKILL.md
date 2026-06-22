---
name: walkthrough
description: Use when Codex needs to generate, regenerate, validate, or repair a DiffReview `.walkthrough.json` artifact by delegating to the `walkthrough-writer` custom agent. Trigger when the user asks for a walkthrough, review walkthrough, DiffReview walkthrough, guided review artifact, or `.walkthrough.json` after local code changes.
---

# Walkthrough

Delegate walkthrough creation to the `walkthrough-writer` custom agent. The skill exists to keep the main thread focused on implementation and verification while the specialized agent gathers semantic change context, writes `.walkthrough.json`, validates it, and fixes validation failures.

## Workflow

1. Identify the target repository root and commit before spawning the agent:
   - Run `git rev-parse --show-toplevel` to get the repository root where `.walkthrough.json` must be written.
   - Run `git rev-parse HEAD` to get the full 40-character sha for `commit`.
   - Run `git ls-files --others --exclude-standard` to get untracked paths for the handoff because `sem_diff` excludes untracked files.
2. Prepare a compact handoff prompt for `walkthrough-writer` with:
   - The target repository root.
   - The full HEAD commit sha.
   - The untracked paths list, or `none`.
   - A concise recap of the completed task or change being reviewed.
   - Any plan file or design note associated with the change.
   - Any explicit user constraints for the walkthrough.
3. Spawn `walkthrough-writer` as a standalone task. Do not use a full-history fork for this role. Mention that the agent should use `sem`
4. Wait for the agent result before the final response.
5. Report whether `.walkthrough.json` was written and validated. Include any blocker exactly if the agent could not complete the artifact.

## Handoff Prompt Template

Use this shape for the standalone agent prompt:

```text
Generate a DiffReview walkthrough for this completed change.

Repository root: <absolute path>

Commit: <full 40-character HEAD sha>

Untracked paths:
<repo-relative paths or "none">

Task recap:
<compact recap of the change or review target>

Associated plan or notes:
<path or "none">

Constraints:
<user constraints or "none">

Use the walkthrough-writer's semantic workflow. Treat MCP sem_diff as tracked-only and use the parent-provided untracked paths for untracked files. Write .walkthrough.json at the repository root, validate it, and fix validation failures before returning.
```

## Failure Handling

If `walkthrough-writer` is unavailable, report that the custom agent is missing and do not silently produce a manual walkthrough. If `sem` is unavailable or semantic change inventory fails, report that semantic change inventory could not be gathered and include the agent's blocker.
