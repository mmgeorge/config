# Pre-Deployment Checklist and Anti-Patterns

*When to read this:* Before finalizing a skill to verify quality invariants and eliminate non-functional patterns.

## Pre-Deployment Checklist

### Structure and Discovery
- [ ] `description` opens with an imperative/infinitive verb phrase, specifies exact triggers, and provides a "Use when..." clause.
- [ ] `name` is lowercase-hyphen format, ≤ 64 characters, and matches the parent directory.
- [ ] `SKILL.md` body is under 500 lines and structured as a navigable overview.
- [ ] Extended reference details are isolated in `references/` or `assets/` and linked directly from `SKILL.md`.
- [ ] Every internal link resolves to an existing file path.
- [ ] Reference files exceeding 100 lines contain a table of contents.
- [ ] All paths use forward slashes (`/`).

### Content and Phrasing
- [ ] Standard programming syntax and generic concepts are omitted to preserve context tokens.
- [ ] Single deterministic tool defaults are provided rather than unguided menus.
- [ ] Terminology is standardized with one term per domain concept.
- [ ] Time-sensitive phrases are replaced with explicit version numbers or collapsed deprecation notes.
- [ ] Multi-step operations are organized as ordered, numbered workflows.
- [ ] Concrete input/output examples accompany abstract schema definitions.

### Executable Scripts (if `scripts/` is included)
- [ ] Scripts handle internal errors and output actionable messages to `stderr`.
- [ ] Configuration parameters and timeouts are explicitly justified.
- [ ] Execution prerequisites and required package versions are documented.
- [ ] Execution vs reference inspection intent is specified.
- [ ] Plan-validate-execute workflows protect destructive or batch operations.

### Evaluation and Validation
- [ ] Baseline task evaluations exist and pass reliably.
- [ ] Trigger accuracy is validated against positive and negative test queries.
- [ ] Execution logic is verified across all targeted model tiers in clean sessions.

---

## Anti-Patterns to Remove

- **Vague Descriptions:** Phrasing such as `Helps with documents` or `Processes data` that lacks concrete trigger terms.
- **Monolithic `SKILL.md` Files:** Inline placement of large schemas, exhaustive API references, or extended output templates that consume context before being needed.
- **Chained Reference Dependencies:** Reference files reachable only through intermediary references (`SKILL.md -> a.md -> b.md` where `b.md` is unlinked in `SKILL.md`).
- **Unguided Tool Menus:** Listing multiple alternative libraries without explicit technical criteria for selection.
- **Generic Tutorial Text:** Explaining well-known formats or standard library fundamentals.
- **Unhandled Script Failures:** Scripts that exit with raw tracebacks rather than actionable error messages.
- **Unexplained Magic Constants:** Numeric timeouts, retry limits, or buffer sizes without documented rationale.
- **Artificial Reference Fragmentation:** Splitting essential baseline rules that must apply universally across every task into fragmented reference files, forcing unnecessary retrieval round-trips or risking missed baseline constraints.
- **Overlapping or Ambiguous Scope:** Skills that trigger simultaneously on unrelated tasks due to broad keyword matching.
