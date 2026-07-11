---
name: walkthrough
description: Use when Codex needs to generate, regenerate, validate, or repair a DiffReview `.walkthrough.json` artifact directly from the current repository changes. Trigger when the user asks for a walkthrough, review walkthrough, DiffReview walkthrough, guided review artifact, or `.walkthrough.json` after local code changes.
---

# Walkthrough

Dispatch the complete DiffReview walkthrough workflow to the `walkthrough-writer` custom agent so semantic discovery, raw patch evidence, artifact construction, and validation stay outside the parent context.

## Dispatch

1. Resolve the target repository or working directory from the request and current session.
2. Resolve the absolute path to `walkthrough.schema.json` beside this `SKILL.md` without reading the schema into the parent context.
3. Spawn exactly one `walkthrough-writer` custom agent. Give it:
   - the target repository or working directory
   - the original user request
   - the absolute schema path
   - ownership of semantic discovery, `.walkthrough.json` writing, validation, repair, and final verification
4. Wait for the writer to finish. If it reports a repairable failure, send the repair request back to the same writer so its accumulated repository context remains reusable.
5. Report the writer's compact completion status to the user.

Do not run `sem_diff`, inspect repository files, read raw patches, draft walkthrough content, or validate the artifact in the parent context. If the `walkthrough-writer` custom agent cannot be spawned, report that exact blocker instead of silently running the generation workflow in the parent.
