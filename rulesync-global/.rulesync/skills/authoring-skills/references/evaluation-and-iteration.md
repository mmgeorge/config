# Evaluation & Iteration

*When to read this:* before writing prose (to build evals first) and after a draft exists
(to adversarially validate it and iterate). This is the step that separates a skill that
*looks* good from one that *works*.

## Evals first — build them before the documentation

Create evaluations **before** writing extensive instructions, so the skill solves a real
problem instead of an imagined one. Evaluation-driven development:

1. **Identify gaps.** Run the agent on representative tasks **without** the skill. Record the
   *specific* failures or context you had to supply by hand.
2. **Create ~3 scenarios** that test those gaps.
3. **Establish a baseline** — measure performance with no skill.
4. **Write minimal instructions** — just enough to address the gaps and pass the evals.
5. **Iterate** — run evals, compare to baseline, refine.

An evaluation is a task + a behavioral rubric (see `assets/evaluation.example.json`):

```json
{
  "skills": ["processing-pdfs"],
  "query": "Extract all text from this PDF and save it to output.txt",
  "files": ["test-files/document.pdf"],
  "expected_behavior": [
    "Reads the PDF with an appropriate library or CLI tool",
    "Extracts text from ALL pages without skipping any",
    "Saves readable text to a file named output.txt"
  ]
}
```

There is no built-in runner — the evals are *your* source of truth; run them by hand or with
a small harness. If a gap disappears without the skill, the skill isn't needed.

## The four-pass adversarial validation

After a draft exists, stress it in a **fresh context** (no authoring history) — paste the
frontmatter / SKILL.md / file tree into a clean conversation and run these passes. Gather
problems first; fix after.

**Pass 1 — Discovery (does it trigger correctly?).** Give the agent only the
`name` + `description` and ask it to produce:
- 3 realistic prompts that **should** trigger this skill, and
- 3 prompts that sound similar but should **NOT** trigger it.
Then: "Is this description too broad or too narrow? Rewrite it optimally." If the
should-NOT prompts trigger, or real prompts don't, the description is wrong — fix it first;
nothing else matters if the skill never fires.

**Pass 2 — Logic (where is the agent forced to guess?).** Give it the full SKILL.md + tree
and ask it to **simulate execution step-by-step** on a realistic request, narrating an
internal monologue for each step — what it's doing, which file it reads or runs. Then:
"Flag the exact line where you are forced to guess, hallucinate, or invent a value." Every
flagged line is a missing instruction, an undefined term, or a dangling reference.

**Pass 3 — Edge cases (try to break it).** "Act as a ruthless QA tester whose goal is to
break this skill. Ask 3–5 specific questions about edge cases, failure states, and missing
fallbacks." Push on: unsupported configs, legacy/missing dependencies, ambiguous inputs,
implicit assumptions, what happens when a script or API fails.

**Pass 4 — Refine.** Rewrite from the answers: tighten the description, add the missing
steps, add an **error-handling section**, and *enforce progressive disclosure* — move any
dense rules, long templates, or schemas that crept into the body out to `references/` /
`assets/`, replaced by a "read this file when X" pointer.

## Iterate A/B on real usage

The best refinement loop uses two agent roles. **Author A** helps design and tighten the
skill; **Tester B** is a *fresh* session with the skill loaded, doing **real** tasks (not
test prompts). The loop:

1. Tester B performs an actual task with the skill.
2. **Observe B's navigation**, not just its answer — watch for:
   - **Unexpected read order** → your structure isn't as intuitive as you thought.
   - **Missed reference links** → links need to be more explicit/prominent.
   - **Overreliance on one file** → that content probably belongs in the SKILL.md body.
   - **Ignored files** → unnecessary, or poorly signposted.
   - **A skipped rule** → make it more prominent or use stronger wording ("MUST", not
     "always").
3. Bring the specific observation to Author A ("B forgot to filter test accounts even though
   the skill mentions it — is it prominent enough?") and apply the fix.
4. Repeat on new scenarios. Refine from observed behavior, not assumptions.

Test with **every model** you'll run the skill on — Haiku may need more guidance than Opus;
aim for instructions that work across all of them.

## Tooling

- **skillgrade** (`github.com/mgechev/skillgrade`, install `npm i -g skillgrade`; needs an
  LLM API key) — scores skill quality and guards against regressions; run it before shipping
  or in CI.
- **SkillsBench** (arXiv 2602.12670) — a benchmark to borrow evaluation ideas from.

Final gate: `references/checklist-and-anti-patterns.md`.
