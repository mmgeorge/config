---
name: walkthrough
description: >-
  Generate, regenerate, validate, or repair a DiffReview `.walkthrough.json` artifact
  directly from repository changes. Use when creating review walkthroughs, guided
  code review artifacts, or `.walkthrough.json` files after local modifications.
targets:
  - '*'
---

# Walkthrough

Dispatch the complete DiffReview walkthrough generation workflow to the `walkthrough-writer` subagent to isolate semantic discovery, raw patch extraction, artifact construction, and schema validation from the parent context.

## Dispatch Workflow

1. Resolve the target repository path and working directory from the request.
2. Locate `walkthrough.schema.json` adjacent to this `SKILL.md` without loading schema contents into the parent context.
3. Spawn one `walkthrough-writer` subagent with:
   - Target repository or working directory path
   - User review request
   - Absolute path to `walkthrough.schema.json`
   - Complete ownership of semantic discovery, artifact creation, schema validation, and error repair
4. Await subagent completion. If validation reports repairable schema errors, route the repair request back to the existing subagent instance to preserve cached repository context.
5. Report the completion status to the user.

Do not run `sem_diff`, inspect raw diffs, draft JSON artifacts, or run schema validation in the parent conversation context. If subagent invocation fails, report the blocking failure explicitly.
