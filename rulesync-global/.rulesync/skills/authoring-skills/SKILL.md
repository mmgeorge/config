---
name: authoring-skills
description: >-
  Author and refactor Agent Skills (a SKILL.md plus its bundled
  references/scripts/assets) that an AI agent can reliably discover and use. Use
  when creating a new skill, restructuring an existing one, writing or fixing a
  SKILL.md description or frontmatter, splitting a skill into progressive-disclosure
  reference files, or evaluating a skill's quality. Encodes the official Anthropic
  Agent Skills best practices plus a concrete draft → disclose → adversarially
  validate → iterate workflow.
targets:
  - '*'
---
# Authoring Agent Skills

A skill teaches an agent a *reusable procedure* — it is written **for an agent, not a
human**. The agent model already knows the SKILL.md format and is already smart; this
skill's value is the **discipline**: build from a real gap (not an imagined one), keep
the body lean via progressive disclosure, and **adversarially validate** before shipping.
Most failed skills fail on two things — a vague `description` (so it never triggers) and
a bloated body (so the agent can't extract what matters). The workflow below fixes both.

Read the matching `references/` file before the step that needs it (see the map). The
references are themselves a worked example of one-topic-per-file progressive disclosure.

---

## The workflow

Follow these in order. Do not skip step 1 — skills written to document imagined problems
are the most common waste.

**1. Find the real gap (evals before prose).**
Run the agent on 1–3 representative tasks **without** the skill. Write down the *specific*
failures or context you had to supply by hand. Those failures are the skill's reason to
exist and its first evaluations. If the agent already succeeds unaided, **don't write the
skill** — delete the instinct. (See `evaluation-and-iteration.md` → "Evals first".)

**2. Draft the minimal SKILL.md.**
Write the frontmatter (`name`, `description`) and the *smallest* set of instructions that
closes the gaps from step 1. Challenge every sentence: "would the agent get this wrong
without it?" If no, cut it. Nail the `description` now — it is the only thing the agent
sees before triggering, and it must say **what** the skill does **and when** to use it,
in the third person. (See `structure-and-frontmatter.md`.)

**3. Apply progressive disclosure.**
Keep the SKILL.md body a lean table of contents (**under ~500 lines**). Move bulky rules,
long examples, schemas, and domain detail into `references/*.md` linked **one level deep**
from SKILL.md, with a *when to read this* trigger on each. Put copyable output templates
and data in `assets/`; put deterministic, reusable code in `scripts/`. (See
`structure-and-frontmatter.md` and, for code skills, `executable-scripts.md`.)

**4. Adversarially validate (the part everyone skips).**
Stress the draft in a fresh context, in three passes — discovery (does it trigger on the
right prompts and *not* on look-alikes?), logic (simulate execution step-by-step; where is
the agent forced to guess?), and edge cases (act as a ruthless QA tester trying to break
it). Then refine. The exact prompts are in `evaluation-and-iteration.md` → "The four-pass
validation". Run `skillgrade` if available.

**5. Iterate A/B on real usage.**
Use the skill in a **fresh session** on real tasks. Watch how the agent navigates it —
unexpected read order, missed reference links, ignored files, a rule it skipped. Bring
each observation back and tighten the SKILL.md (often: make a rule more prominent or move
it into the body). Repeat. Test with every model you'll run it on.

**Refactoring an existing skill** (splitting a bloated body, fixing a description) instead
of authoring fresh? Same workflow, but the baseline in step 1 is the *current* skill plus
any evals you have — the job is to split/prune the body and sharpen the description
**without regressing**. Run step 4's validation **before and after** so you can prove the
refactor didn't lose behavior, then remove any now-stale generated copies (rulesync section
below).

---

## Core principles (always apply)

- **Concise is key — the agent is already smart.** Add only what the model lacks: your
  conventions, non-obvious gotchas, which tool to default to. Don't explain what a PDF is.
- **The `description` is the product.** Third person — "Extract…" or "Extracts…", never
  "I can…"/"you can…" — specific, with key terms and concrete *when-to-use* triggers. It's
  how the agent picks this skill out of 100+. Vague descriptions are the #1 reason a good
  skill never fires.
- **Progressive disclosure, one hop from SKILL.md.** Every reference is linked *directly*
  from SKILL.md, so no file is reachable *only* by chaining through another (SKILL.md →
  a.md → b.md, where b.md is linked nowhere else — the agent previews such buried files
  with `head` and reads them incompletely). A short "see also" between two references that
  are both linked from SKILL.md is fine.
- **Provide a default, not a menu.** "Use pdfplumber. For scanned PDFs, use pdf2image."
  Not "you can use pypdf, or pdfplumber, or PyMuPDF, or…". Match freedom to fragility (see
  `writing-and-content.md` → "Degrees of freedom").
- **One term per concept; no time-sensitive facts.** Pick "field" (not field/box/element);
  put deprecated guidance in an "old patterns" `<details>` block, not inline.
- **Forward slashes always** (`references/x.md`), even on Windows. Name files by content
  (`form-validation.md`, not `doc2.md`).
- **Ground in reality.** Write the skill against the actual code/API/tool it covers and
  verify the claims; never document a remembered or assumed interface.
- **Eat your own dog food.** A skill-authoring output should itself pass this skill's
  checklist.

---

## Reference map

Read the file that matches the step you're on:

| Read this… | …when you are |
| --- | --- |
| **`references/structure-and-frontmatter.md`** | Choosing the name/description, the directory layout (SKILL.md / references / scripts / assets), the progressive-disclosure pattern, MCP tool names, or declaring dependencies. |
| **`references/writing-and-content.md`** | Writing the body: tone, degrees of freedom, workflows + checklists + feedback loops, templates, examples, terminology, avoiding time-sensitive content. |
| **`references/evaluation-and-iteration.md`** | Validating the skill: evals-first, the four-pass adversarial validation prompts, the Claude-A/Claude-B iteration loop, skillgrade/SkillsBench. |
| **`references/executable-scripts.md`** | The skill bundles `scripts/`: solve-don't-punt error handling, no voodoo constants, execute-vs-read intent, plan-validate-execute. |
| **`references/checklist-and-anti-patterns.md`** | About to ship — the final scan: the "ready" checklist and the anti-patterns to delete. |

Templates to copy, not describe: **`assets/SKILL.template.md`** (a skeleton skill) and
**`assets/evaluation.example.json`** (the eval rubric shape).

## Syncing skills in this repo (rulesync)

Skills here are **rulesync-managed**: author under `.rulesync/skills/<name>/` (locally) or
`rulesync-global/.rulesync/skills/<name>/` (global), then run `rulesync generate`
(dry-run first). Never hand-edit the generated `.claude/`, `.codex/`, `.agents/`, etc.
copies — rulesync overwrites them. `rulesync generate` does **not** prune deleted files,
so remove stale generated copies by hand after deleting a source reference. See the
`rulesync` skill for the commands.
