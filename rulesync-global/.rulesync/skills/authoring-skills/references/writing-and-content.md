# Writing and Content Guidelines

*When to read this:* When authoring the `SKILL.md` body or reference files to choose instructional phrasing, levels of instruction specificity, sequential workflows, verification checkpoints, templates, and domain terminology.

## Contents
- Context Budget and Information Density
- Instructional Tone and Phrasing
- Instruction Specificity by Operational Fragility
- Default Tool Selection
- Consistent Domain Terminology
- Sequential Workflows
- Verification Loops
- Templates and Concrete Examples

## Context Budget and Information Density

The context window is a constrained shared resource. Document only domain-specific conventions, non-obvious operational constraints, concrete defaults, and exact tool interfaces.

```markdown
<!-- Functional: specifies tool and exact usage -->
## Extract PDF text
Use `pdfplumber`:
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```

<!-- Non-functional: generic background and tutorial prose -->
## Extract PDF text
PDF (Portable Document Format) files are a common format that contains text... there are
many libraries... pdfplumber is recommended because... first install it with pip...
```

## Instructional Tone and Phrasing

Write the body as imperative instructions: "**Extract** table schema. **Verify** page boundaries. **If** the document contains scanned images, **run** OCR." Avoid conversational commentary and personal pronouns.

## Instruction Specificity by Operational Fragility

Match instruction specificity to the operational risk and variability of the task:

- **Open Guidelines (High Freedom):** For exploratory tasks with multiple valid solutions (such as code review or design analysis), provide evaluation criteria and architectural constraints.
- **Parameterized Patterns (Medium Freedom):** For structured operations with variable inputs, provide standard schemas and adaptable script templates.
- **Exact Deterministic Commands (Low Freedom):** For state-mutating, destructive, or sequence-sensitive operations, provide exact commands and explicit argument constraints.

## Default Tool Selection

Specify a single primary tool and define explicit fallback conditions:

```markdown
Use `pdfplumber` for text extraction. When encountering scanned documents, use `pdf2image` and `pytesseract`.
```

Do not list unstructured alternatives without decision criteria.

## Consistent Domain Terminology

- Use one stable term per concept across the entire skill (such as consistently using `field` rather than alternating between `box`, `element`, or `control`).
- Do not write time-sensitive instructions. Place deprecated APIs or legacy migration notes in explicit collapsed blocks:

  ```markdown
  ## Current API
  Use the v2 endpoint: `api.example.com/v2/messages`

  <details><summary>Legacy v1 (deprecated)</summary>
  v1 endpoint `api.example.com/v1/messages` is obsolete.
  </details>
  ```

## Sequential Workflows

Structure multi-step operations as ordered, numbered procedures. For complex sequences, provide a structured checklist:

```markdown
## Form Processing Workflow
1. Analyze form fields (`analyze_form.py`)
2. Define field mappings (`fields.json`)
3. Validate mappings against schema (`validate_fields.py`)
4. Populate form data (`fill_form.py`)
5. Verify output artifact
```

## Verification Loops

Build validation steps directly into procedural workflows:

```markdown
1. Draft output artifact.
2. Validate against schema and style rules.
3. If errors are detected: record error messages, repair output, and re-validate.
4. Proceed only after validation passes.
```

## Templates and Concrete Examples

- **Templates:** Provide exact output schemas in `assets/` and specify whether adherence is strict or adaptable.
- **Concrete Input/Output Pairs:** Provide verified input/output examples to illustrate formatting rules and domain constraints.
