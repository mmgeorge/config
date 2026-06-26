<!--
COPY THIS to <new-skill>/SKILL.md and fill it in. Delete this comment and every
<placeholder>. Keep the body under ~500 lines; push depth into references/*.md.
This template is itself a minimal example of the structure in ../SKILL.md.
-->
---
name: <gerund-name>            # lowercase-hyphen, ≤64 chars, matches the directory, no "anthropic"/"claude"
description: >-
  <What it does, third person, with concrete trigger terms.> Use when <the specific
  situations/keywords that should activate this skill>; not for <look-alike cases, if any>.
---
# <Skill Title>

<One or two sentences: what an agent gains from this skill, and the single most important
thing to get right. State only what the model wouldn't already know.>

## <Core procedure>

<The main workflow as numbered, imperative steps. Provide a default approach, not a menu.
Match detail to fragility: exact commands where a misstep is costly, prose where many
paths work. For a long multi-step process, give a copyable checklist:>

- [ ] 1. <step>
- [ ] 2. <step — if it forks, say "If X → …; otherwise → …">
- [ ] 3. <validation/verify step; on failure, return to step N>

## <Key rules / gotchas>

<The non-obvious, project-specific things the agent gets wrong without you. One term per
concept. No time-sensitive facts.>

## Reference map

<Only if the body would exceed ~500 lines. One topic per file, one level deep, each with a
"when to read this" trigger.>

| Read this… | …when you are |
| --- | --- |
| **`references/<topic>.md`** | <doing the specific thing this file covers>. |

<!-- Optional bundled dirs:
  references/<topic>.md   detail loaded on demand (start each with a *When to read this:* line)
  scripts/<tool>.py       deterministic CLI the agent RUNS (say "run" vs "read")
  assets/<file>           a template/schema the agent COPIES into its output
-->
