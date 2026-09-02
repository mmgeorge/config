---
name: authoring-skills
description: >-
  Author and refactor Agent Skills (a SKILL.md plus bundled references, scripts, and
  assets) for reliable discovery and execution. Use when creating a new skill,
  restructuring an existing skill, defining frontmatter descriptions, organizing
  progressive-disclosure reference files, or evaluating skill quality.
targets:
  - '*'
---

# Authoring Agent Skills

An agent skill defines a reusable procedure and domain context. High-quality skills provide precise discovery triggers, minimize context overhead through progressive disclosure, and validate deterministic execution before deployment.

Read the corresponding reference file before performing each workflow step.

---

## Workflow

Follow these steps in sequential order:

1. **Identify Functional Gaps with Baselines:**
   Execute 1–3 representative tasks without the skill. Record specific failures, unverified assumptions, or domain context that required manual intervention. These recorded gaps define the scope and evaluation criteria for the skill. If baseline execution succeeds consistently without guidance, do not create the skill.

2. **Draft Minimal `SKILL.md`:**
   Define frontmatter (`name`, `description`) and the minimal instructions necessary to resolve the recorded gaps. State **what** capability the skill provides and **when** to trigger it using third-person declarative prose. Verify that every instruction addresses a specific operational failure mode.

3. **Apply Progressive Disclosure Based on Conditional Access:**
   Keep the `SKILL.md` body under 500 lines as a structural overview. Extract content into `references/*.md` only when an agent can execute a distinct subset of tasks without loading the other parts (such as specialized data schemas, alternative format decoders, or rare error codes). If all rules and guidelines form an essential baseline required on every invocation, keep them unified in `SKILL.md` rather than fragmenting them across reference files. Place output templates in `assets/` and deterministic executables in `scripts/`.

4. **Validate Skill Execution:**
   Evaluate the draft in an isolated session across three dimensions:
   - **Discovery:** Verify the skill triggers on intended requests and remains inactive on related but distinct tasks.
   - **Deterministic Logic:** Trace execution step by step to identify ambiguous branching, missing parameters, or unspecified dependencies.
   - **Edge Cases:** Test boundary conditions, malformed inputs, missing tools, and execution failures.

5. **Iterate on Real Workflows:**
   Test the skill on production tasks across all targeted models. Monitor navigation behavior, referenced file loading order, and command execution. Promote frequently needed reference rules into `SKILL.md` and prune unused reference files.

---

## Core Authoring Principles

- **Minimize Context Window Overhead:** Document only non-standard project conventions, domain invariants, edge cases, and preferred tool configurations. Do not explain standard programming languages or well-known file formats.
- **Conditional Reference Partitioning:** Extract material to `references/` only when tasks require a fraction of the total guidance. Avoid artificial fragmentation of cohesive baseline standards that must apply universally across every skill invocation.
- **Precise Frontmatter Discovery:** Use third-person declarative phrasing in the `description` field with an imperative or infinitive opening verb. Include concrete keywords, file extensions, and explicit trigger conditions.
- **Direct Single-Hop Reference Linking:** Link every reference file directly from `SKILL.md`. Do not create deep reference dependency chains (such as `SKILL.md -> a.md -> b.md` where `b.md` is unlinked in `SKILL.md`).
- **Deterministic Defaults:** Provide a single recommended tool or command path by default. Document secondary alternatives only when required by specific technical constraints.
- **Consistent Domain Terminology:** Use one unambiguous term per concept across all files in the skill. Place historical or deprecated patterns in explicit collapsed blocks.
- **Cross-Platform Paths:** Use forward slashes (`/`) for all file paths, including Windows environments.
- **Verified Technical Interfaces:** Validate all documented CLI commands, API endpoints, schema structures, and parameters against live implementations.

---

## Reference Map

| Reference File | Purpose |
| --- | --- |
| **`references/structure-and-frontmatter.md`** | Directory structure, frontmatter schemas, description triggers, progressive disclosure hierarchy, and tool dependencies. |
| **`references/writing-and-content.md`** | Instructional phrasing, specification levels, sequential workflows, verification checkpoints, templates, and domain terminology. |
| **`references/evaluation-and-iteration.md`** | Baseline evaluations, multi-phase validation, execution tracing, and iterative refinement across models. |
| **`references/executable-scripts.md`** | Bundled scripts, execution vs reading semantics, explicit parameter justification, and plan-validate-execute workflows. |
| **`references/checklist-and-anti-patterns.md`** | Pre-deployment checklist and non-functional anti-patterns to remove. |

Templates:
- `assets/SKILL.template.md` (structural skill template)
- `assets/evaluation.example.json` (evaluation schema template)

## Synchronizing Skills

Skills in this repository are managed by Rulesync:
- Local skills: `.rulesync/skills/<name>/`
- Global skills: `rulesync-global/.rulesync/skills/<name>/`

Synchronize with `rulesync generate` (run with `--dry-run` first). When deleting reference files, manually remove obsolete generated copies from target provider directories.
