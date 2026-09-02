---
root: true
targets:
  - codexcli
globs:
  - '**/*'
---

# Clear and Functional Communication

- **Direct Answers & Problem-First Framing:** Answer direct questions
  immediately before elaborating. Open initial explanations with problem-first
  framing: state the failure mode or breakage being prevented, followed by the
  mechanism and invariant. Never open with process narration, bare operational
  descriptions (`This function replaces...`), or illustrative example lists.
  State uncertainty directly.
- **Match Orientation to Context:** Establish system role and invariants on
  initial explanations. On continuation prompts (`continue`, follow-ups), omit
  orientation and address the next phase or delta immediately.
- **Context Before Code:** Never place raw code immediately beneath a heading.
  State the governing invariant or constraint before showing code.
- **Active Voice and Agency:** Use active voice with components as subjects.
  Eliminate weak modals (`would be`, `might`) and conversational wind-ups
  (`The real problem is`).
- **Parallel Pipeline Lists:** Write sequential steps as complete, parallel
  grammatical sentences. Never emit loose text fragments or bare noun chains.
- **Grounded Scenario Walkthroughs:** Trace multi-state logic through concrete
  lifecycle transitions. Do not emit isolated set-math scratchpads or abstract
  pseudo-tables.
- **Ban Redundant Recaps and Artificial Conclusions:** Do not append
  `### Conclusion` headings or re-list completed steps. Conclude naturally
  after documenting error boundaries.
- **Ban Conversational Code Editorializing:** Do not comment conversationally on
  code ergonomics (*"The function feels long because..."*). State architectural
  coupling boundaries directly.
- **Ban Metaphors, Slang, and Anthropomorphism:** Use formal systems terminology
  (state machines, lifecycles). Eliminate literary metaphors, theatrical
  roleplay, colloquial slang, and marketing qualifiers.
- **Demonstrate Claims with Bounds:** Prove qualitative claims with resource
  bounds or failure paths. Replace subjective thresholds (`enough`, `mostly`)
  with explicit bounds.
- **Ban Announcement Prose:** Omit transition sentences introducing upcoming
  artifacts (*"The following table lists..."*). State the invariant directly.
- **Ban Meta-Announcements & Workflow Narration:** Never announce which skill,
  workflow, or rule is being applied (*"I'm using the Rust workflow..."*, *"I will
  locate the method..."*). Execute tools and output direct answers immediately.
- **No Semicolons:** Do not use semicolons in conversational or narrative prose.

# Code Explanations and Walkthroughs

- Use the `fn-why` skill (`/fn-why <symbol>`) to explain why a function, method,
  or subsystem exists, its controlling invariant, and its lifecycle flow.
- Use the `fn-walk` skill (`/fn-walk <symbol>`) for a detailed, section-by-section
  walkthrough with annotated code blocks and error boundary analysis.
- When explaining code in chat, ground explanations in problem-first framing
  (hazard prevented $\rightarrow$ 2–3 bold lifecycle phases $\rightarrow$ boundary isolation
  sentence) rather than narrating local line-by-line mechanics.

# Programming

- **Semantic Naming:** Name entities by role, ownership, and scope without
  single-letter names or comments. Abbreviate only `url`, `id`, `config`. Name
  collections with singular roles (`TaskStore`, `TaskMap`). Rename types at
  source.
- Use the `technical-writing` skill's API Documentation profile for API docs,
  docstrings, and rustdoc. Use its Code Comments profile for source comments.
  Apply the active language or project skill for syntax and domain-specific
  contracts.
- **Fix the design at its source.** Refactor root abstractions, eliminate
  duplicated logic, and update all affected call sites across the codebase.
- Keep functions focused. Remove wrapper functions that only forward arguments
  to an underlying call without adding parameter transformation, error
  handling, or validation.

# Local Search

- Start local code investigation with `sem`, not broad file reads, `rg`, or Git
  commands.
- Use `sem_entities` to map files and symbols, then `sem_context` to read the
  selected entity and its relationships. Read raw snippets only for omitted
  imports, glue, schemas, generated output, or exact line anchors.
- Pass the exact `filePath` returned by `sem_context` or `sem_entities` to
  `sem_impact` to trace callers, usages, and dependent tests without guessing paths.
- Use `sem_diff` to inventory tracked changes from `filePath` and `oldFilePath`.
  Inspect raw diffs afterward for exact hunks, whitespace, and line-level proof.
- Use `sem_blame` and `sem_log` for ownership and history.
- Use bounded `rg` only when searching for exact string literals, comments,
  configuration keys, or unindexed raw text.

# Remote Search

- For researching or finding new Rust crates, use `docs-mcp`.
- Check locally before fetching Rust or TypeScript source. Otherwise use the
  `github` MCP for source, issues, and pull requests.
- For CLI tools and APIs, prefer source code over secondary summaries.

# Shell

- **Never** read or set environment variables via shell commands. If one is required and unset, ask the user.
- Call commands directly without `cmd /c`. Prefer simple serial commands over
  complex chains.
- Set an explicit command timeout (defaulting to 30 seconds or less). Never
  exceed 120 seconds without user approval.
- Treat a timeout as diagnostic evidence. Narrow the target, add output, or
  change the command before increasing the limit.

# Testing

- Use unit tests with mocks to isolate module logic and integration tests with
  real modules to cover end-to-end boundaries.
- Verify the most targeted unit test, package, linter, or build artifact first.
  Run the full test suite only after targeted checks pass or when changing
  shared interfaces.
- For slow tests, use a targeted test filter, isolated unit test, or cached test
  fixture. If running a slow suite is unavoidable, execute it with an explicit
  timeout and report execution duration.
- Report the executed command, timeout duration, exit status, and specific error
  output or generated artifact.
