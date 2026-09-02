# Structure, Frontmatter, and Progressive Disclosure

*When to read this:* When defining skill directory layout, authoring frontmatter fields, optimizing discovery descriptions, organizing progressive disclosure, or specifying tool dependencies.

## Contents
- Directory Structure
- Frontmatter Specification
- Description Phrasing and Discovery Triggers
- Progressive Disclosure Hierarchy
- Tool Names and Dependencies

## Directory Structure

A skill directory name must match the frontmatter `name` attribute:

```text
skill-name/
├── SKILL.md          # Frontmatter and core procedural overview
├── references/       # Supplemental domain references and schemas
│   ├── topic-a.md
│   └── topic-b.md
├── scripts/          # Deterministic executables executed by the agent
│   └── validate.py
└── assets/           # Output templates and static fixtures copied by the agent
    └── report.template.md
```

Keep all subdirectories exactly one level deep.

## Frontmatter Specification

```yaml
---
name: processing-pdfs
description: >-
  Extract text and tables from PDF files, fill forms, and merge documents. Use when
  processing PDF documents, form templates, or tabular report extractions.
---
```

- **`name`:** 1–64 characters, lowercase alphanumeric and hyphens only (`processing-pdfs`).
- **`description`:** 1–1024 characters. Open with a third-person imperative or infinitive verb phrase (*"Design, implement, refactor, and test..."* or *"Debug live engine sessions..."*) rather than third-person singular indicative (*"Designs, implements..."*) or second-person directives (*"Use this skill when you want..."*). State what the skill executes and when to trigger it.

## Description Phrasing and Discovery Triggers

Skill discovery relies on the frontmatter description:

- **Effective Description:** `Analyze Excel spreadsheets, generate pivot tables, and create charts. Use when analyzing Excel workbooks, spreadsheet formulas, or .xlsx files.`
- **Ineffective Description:** `Helps with data processing and files.`

Discovery guidelines:
- Open with an imperative or infinitive verb phrase (`Analyze Excel spreadsheets...`, `Design and implement...`). Do not use third-person singular indicative (`Analyzes...`) or second-person directives (`Use this when you want...`).
- Include specific domain terms, file extensions, and tool names.
- Include explicit negative triggers when related skills have overlapping scope (such as specifying text extraction vs OCR).
- Avoid first-person or second-person phrasing.

## Progressive Disclosure Hierarchy

Organize skill content into three tiers:
1. **Frontmatter Metadata:** `name` and `description` (always indexed for skill discovery).
2. **`SKILL.md` Overview:** Procedural workflow, core invariants, and directory mapping (under 500 lines).
3. **References, Scripts, and Assets:** Detailed schemas, extended documentation, and executable code (loaded on demand).

### Reference Partitioning Criteria

Split content into `references/` files only when distinct tasks require disjoint subsets of the documentation. Do not split a document solely to reduce file length.

- **Extract to `references/` when:**
  - An agent needs one specialized part for a specific task and not the others (such as format-specific schemas, rare troubleshooting guides, or secondary platform targets).
  - Extended reference material (such as large output templates, static catalogs, or exhaustive translation matrices) would consume prompt tokens during routine tasks that only need procedural steps.
- **Keep Unified in `SKILL.md` when:**
  - All rules, invariants, and quality standards form an essential baseline required across every execution of the skill (such as core writing standards, code-style rules, or foundational architectural invariants).
  - Fragmenting essential rules would force the agent to execute unnecessary discovery round-trips or risk omitting critical baseline constraints.

### Progressive Disclosure Invariants

- **Direct Reference Links:** Link every reference file directly from `SKILL.md`. Do not create multi-hop reference chains.
- **Table of Contents:** Include a table of contents in any reference file exceeding 100 lines.
- **Explicit Read Triggers:** State the exact condition for loading each reference file (such as *"When encountering non-standard error codes, read [errors.md](errors.md)"*).
- **Semantic File Names:** Name files by domain concept (`form-validation.md`) rather than generic identifiers (`doc2.md`).

## Tool Names and Dependencies

- **Fully Qualified MCP Tool Names:** Always use `ServerName:tool_name` format (such as `GitHub:create_issue`).
- **Explicit Dependencies:** Document required runtime dependencies and installation commands in `SKILL.md`. Do not assume external binaries exist on `PATH`.
