# Writing & Content

*When to read this:* while writing the SKILL.md body or a reference — choosing tone, how
much to spell out, how to structure a multi-step procedure, and which patterns (template,
examples, workflow, feedback loop) to use.

## Contents
- Concise — the agent is already smart
- Tone: third-person imperative
- Degrees of freedom (match specificity to fragility)
- Default, not a menu
- Consistent terminology, no time-sensitive facts
- Structure multi-step work as a workflow
- Feedback loops (validator → fix → repeat)
- Templates and examples

## Concise — the agent is already smart

The context window is a shared public good. Add only what the model **doesn't already
know**: your project's conventions, non-obvious gotchas, the specific tool to default to,
the field-name mismatch that bites. Challenge every line: *"would the agent get this wrong
without it?"* If no, delete it.

````markdown
<!-- Good (~50 tokens): assumes the model knows what a PDF is -->
## Extract PDF text
Use pdfplumber:
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```

<!-- Bad (~150 tokens): explains PDFs, libraries, pip — all known -->
## Extract PDF text
PDF (Portable Document Format) files are a common format that contains text… there are
many libraries… pdfplumber is recommended because… first install it with pip…
````

## Tone: third-person imperative

Write the body as instructions to the agent: "**Extract** the table. **Check** for empty
pages. **If** the form is scanned, **run** OCR." Avoid "I will…" and "you should…" and avoid
narrating *why* at length — state the rule and move on. (The `description` is third-person
declarative; the body is third-person imperative.)

## Degrees of freedom — match specificity to fragility

Think of the agent as a robot on a path. Give exact guardrails only where a misstep is
costly; give direction and trust it where many paths succeed.

- **High freedom (prose steps)** — many valid approaches, context decides. *e.g.* "Review
  the code for bugs, readability, and adherence to project conventions." Open field.
- **Medium freedom (parameterized pattern/pseudocode)** — a preferred pattern exists, some
  variation is fine. *e.g.* a `generate_report(data, format="markdown")` template to adapt.
- **Low freedom (exact command, no deviation)** — fragile, consistency-critical, must run in
  sequence. *e.g.* "Run exactly `python scripts/migrate.py --verify --backup`. Do not add
  flags." Narrow bridge with cliffs.

Over-constraining an open task wastes tokens and brittle-fails; under-constraining a fragile
one invites the agent to improvise into a wall.

## Default, not a menu

Presenting many options makes the agent try them all. Give one default plus an escape hatch:

```markdown
Use pdfplumber for text extraction. For scanned PDFs requiring OCR, use pdf2image + pytesseract.
```

Not: "you can use pypdf, or pdfplumber, or PyMuPDF, or pdf2image, or…".

## Consistent terminology, no time-sensitive facts

- Pick **one** term per concept and use it everywhere: always "field" (never field / box /
  element / control), always "extract" (never pull / get / retrieve). Drift confuses the
  agent.
- Don't write "before August 2025 use the old API". Put deprecated guidance in a collapsed
  **old-patterns** block so the main flow stays current:
  ```markdown
  ## Current method
  Use the v2 endpoint: `api.example.com/v2/messages`
  <details><summary>Legacy v1 (deprecated 2025-08)</summary>
  v1 used `api.example.com/v1/messages` — no longer supported.
  </details>
  ```

## Structure multi-step work as a workflow

Break complex operations into numbered, chronological steps. For long procedures, give a
**copyable checklist** the agent ticks off — it prevents skipped validation:

```markdown
## Form-filling workflow
Copy this and check off as you go:
- [ ] 1. Analyze the form (run analyze_form.py)
- [ ] 2. Create field mapping (edit fields.json)
- [ ] 3. Validate mapping (run validate_fields.py)
- [ ] 4. Fill the form (run fill_form.py)
- [ ] 5. Verify output — if it fails, return to step 2
```

Use **conditional/decision-tree** steps when the path forks ("Creating new content? → …;
Editing existing? → …"). If a workflow grows large, push it into its own reference file.

## Feedback loops — validator → fix → repeat

For quality-critical output, build the check into the procedure. The "validator" can be a
script *or* a reference doc the agent compares against:

```markdown
1. Draft content per STYLE_GUIDE.md
2. Review against the checklist (terminology, example format, required sections)
3. If issues: note each with a section reference, revise, re-review
4. Only proceed when all requirements are met
```

## Templates and examples

- **Template pattern** — give the exact output shape. Mark strictness: "ALWAYS use this exact
  template" for data formats vs "a sensible default; adapt as needed" for flexible output.
  Prefer a copyable template in `assets/` over prose describing the format.
- **Examples pattern** — when quality depends on style, show input→output pairs (just like
  few-shot prompting). Three concrete commit-message examples teach the format better than a
  paragraph describing it. **Concrete beats abstract** every time.

Before shipping, scan `references/checklist-and-anti-patterns.md`. For code-bearing skills, see
`references/executable-scripts.md`.
