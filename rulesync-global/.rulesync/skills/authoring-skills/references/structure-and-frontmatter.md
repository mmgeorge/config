# Structure, Frontmatter & Progressive Disclosure

*When to read this:* before naming the skill, writing the `description`, laying out the
directory, deciding what goes in the body vs a reference, or wiring MCP tools / dependencies.

## Contents
- Directory layout
- Frontmatter (the two required fields)
- Writing the description (discovery)
- Progressive disclosure (body as table of contents)
- MCP tools and dependencies

## Directory layout

A skill is a directory whose name **exactly matches** the `name` field:

```
skill-name/
├── SKILL.md          # metadata + the core procedure (loaded when triggered)
├── references/       # supplementary context loaded on demand (schemas, deep guides)
│   ├── topic-a.md
│   └── topic-b.md
├── scripts/          # deterministic executables the agent RUNS (not read for context)
│   └── validate.py
└── assets/           # templates/data the agent COPIES into its output
    └── report.template.md
```

`references/`, `scripts/`, and `assets/` are all optional. The simplest valid skill is a
single `SKILL.md`. Keep every subdirectory **exactly one level deep**.

## Frontmatter (only two fields matter)

```yaml
---
name: processing-pdfs
description: >-
  Extract text and tables from PDF files, fill forms, and merge documents. Use when
  working with PDFs or when the user mentions PDFs, forms, or document extraction.
---
```

- **`name`** — ≤ 64 chars, **lowercase letters / numbers / hyphens only**, no XML tags, and
  **must not contain the reserved words `anthropic` or `claude`**. Must equal the directory
  name. Prefer **gerund form** (verb + -ing): `processing-pdfs`, `analyzing-spreadsheets`,
  `writing-documentation`. Acceptable: noun phrases (`pdf-processing`) or action verbs
  (`process-pdfs`). Avoid vague names: `helper`, `utils`, `tools`, `data`, `files`.
- **`description`** — ≤ 1024 chars, non-empty, no XML tags. This is the **only** thing the
  agent sees before it decides to trigger the skill, so it carries the whole discovery
  burden (see below). Anything beyond `name`/`description` is tool-specific (`targets`,
  `allowed-tools`, `metadata`) — keep the two required fields perfect first.

## Writing the description (this makes or breaks discovery)

The agent chooses among potentially 100+ skills using `description` alone. It must state
**what** the skill does **and when** to use it, in the **third person**.

- ✅ `Analyze Excel spreadsheets, create pivot tables, generate charts. Use when analyzing Excel files, spreadsheets, tabular data, or .xlsx files.`
- ✅ `Generate descriptive commit messages by analyzing git diffs. Use when the user asks for help writing commit messages or reviewing staged changes.`
- ❌ `Helps with documents` / `Processes data` / `Does stuff with files` (no trigger surface).
- ❌ `I can help you process Excel files` / `You can use this to…` — never first/second
  person; the description is injected into the system prompt and POV drift breaks matching.

Rules of thumb:
- **Front-load concrete trigger terms** the user would actually type (`.xlsx`, "pivot table",
  "commit message", "PR review"). The model keyword-matches against these.
- **Add negative triggers** when a look-alike skill exists ("…for PDFs, *not* image OCR").
- One sentence of *what* + one sentence of *when* is usually enough. Don't pad to 1024.

## Progressive disclosure (the body is a table of contents)

Three loading tiers: **metadata** (name+description, always in context) → **SKILL.md body**
(read when triggered; keep it **under ~500 lines**) → **references/scripts/assets** (read or
executed only when the body points to them). Bundling a 2,000-line API doc costs **zero**
context until the agent actually reads it — so push depth out and keep the body lean.

Pick the pattern that fits:

- **High-level guide + references** — body has quick-start + a list of links:
  ```markdown
  ## Advanced features
  **Form filling**: see [forms.md](forms.md)
  **API reference**: see [reference.md](reference.md)
  ```
- **Domain organization** — one reference per independent domain, so a sales question never
  loads finance context: `reference/{finance,sales,product,marketing}.md`.
- **Conditional details** — show the common path inline, link the rare path:
  `For tracked changes: see [redlining.md](redlining.md)`.

Hard rules:
- **One hop from SKILL.md.** Every reference links *directly* from SKILL.md, so no file is
  reachable *only* via another (SKILL.md → a.md → b.md, where b.md is linked nowhere else) —
  the agent previews such buried files with `head -100` and misses content. A "see also"
  between two references that are *both* linked from SKILL.md is fine (they're each one hop).
  A reference may live at the skill root *or* under `references/`; either is one hop.
- **TOC for any reference > 100 lines**, so a partial preview still reveals the full scope.
- **Tell the agent *when* to read each file**, not just "see references/". Conditional
  language: "*If the API returns non-200*, read [errors.md](errors.md)." Put a one-line
  *When to read this* trigger at the top of each reference (as this file does).
- **Name files by content** (`form-validation-rules.md`), never `doc2.md`/`file1.md`.

## MCP tools and dependencies

- **MCP tools:** always use the fully-qualified `ServerName:tool_name` (e.g.
  `GitHub:create_issue`, `BigQuery:bigquery_schema`). A bare tool name fails to resolve when
  multiple servers are loaded.
- **Dependencies:** never assume a package/binary is installed. List required packages in
  the body and show the install (`pip install pypdf`) before the usage. Some runtimes (the
  Claude API code tool) have no network/install — call that out if your skill needs it.

For tone, freedom, and content patterns, see `references/writing-and-content.md`. Before shipping, run
the scan in `references/checklist-and-anti-patterns.md`.
