# Ship Checklist & Anti-Patterns

*When to read this:* right before you ship a skill — a fast scan for "is it ready" and "did I
do any of the known-bad things".

## Ready-to-ship checklist

**Discovery & structure**
- [ ] `description` is third-person, specific, and includes concrete trigger terms + a
      "use when…" clause.
- [ ] `name` is lowercase-hyphen, ≤ 64 chars, matches the directory, no reserved words.
- [ ] SKILL.md body is under ~500 lines and reads like a table of contents.
- [ ] Bulky detail is in `references/` / `assets/`, linked **one level deep**, each with a
      "when to read this" trigger.
- [ ] Every reference link resolves to a file that exists (no dangling/typo'd links).
- [ ] Reference files > 100 lines have a table of contents.
- [ ] All paths use forward slashes.

**Content**
- [ ] Nothing the model already knows is explained (no "what a PDF is").
- [ ] One default per task, not a menu of options.
- [ ] Consistent terminology (one term per concept).
- [ ] No time-sensitive facts (deprecated guidance in an "old patterns" block).
- [ ] Examples are concrete (input→output), not abstract descriptions.
- [ ] Multi-step procedures are numbered; complex ones have a copyable checklist.

**Code (if `scripts/`)**
- [ ] Scripts handle their own errors with specific, actionable messages.
- [ ] No voodoo constants — every value is justified.
- [ ] Required packages are listed; nothing assumes a binary is installed.
- [ ] Execute-vs-read intent is explicit for each script.
- [ ] Validation / verification step exists for fragile or destructive operations.

**Testing**
- [ ] At least three evaluations exist and the skill passes them.
- [ ] The four-pass adversarial validation was run (discovery, logic, edge cases, refine).
- [ ] Tested in a **fresh session** on a real task, with every model you'll run it on.

## Anti-patterns — delete these on sight

- **Vague description / name.** `Helps with documents`, `helper`, `utils` — the skill will
  never trigger reliably. The single highest-leverage thing to get right.
- **A bloated SKILL.md.** Dense rules, full schemas, or long templates inline. Move them out;
  the body is navigation, not the encyclopedia.
- **Buried references** — a file reachable *only* by chaining (SKILL.md → a.md → b.md, with
  b.md linked nowhere else). The agent previews such files and reads them incompletely. Link
  every reference directly from SKILL.md. (A "see also" between two files both linked from
  SKILL.md is fine — each is still one hop.)
- **A menu of options.** "You can use pypdf, or pdfplumber, or PyMuPDF…". Give one default +
  an escape hatch.
- **Explaining what the model already knows.** Over-explaining wastes context and buries the
  3 lines that actually matter. Ask "would the agent get this wrong without it?".
- **Time-sensitive instructions** ("before August 2025…"). They rot. Use an old-patterns block.
- **Windows-style paths** (`scripts\x.py`). Forward slashes only.
- **Human-facing meta files inside the skill** — no `README.md`, `CHANGELOG.md`,
  `INSTALLATION.md`. A skill is for an agent; SKILL.md *is* the doc.
- **Redundant logic.** If the agent already does the task reliably, the instruction is noise —
  delete it (and reconsider whether the skill should exist at all).
- **Library code in `scripts/`.** Scripts are tiny single-purpose CLIs, not a vendored lib.
- **Scripts that punt** — failing and leaving the agent to "figure it out" instead of handling
  the error.
- **Voodoo constants** — unexplained magic numbers an agent can't reason about.
- **Inventing a skill from a prompt with no real task behind it.** Skills come from extracting
  a pattern you actually used, not from imagining one (see `references/evaluation-and-iteration.md`).
- **Over-narrow or over-broad scope.** Too narrow → many skills load for one task and conflict;
  too broad → triggers on the wrong requests. Encapsulate one coherent unit of work.
