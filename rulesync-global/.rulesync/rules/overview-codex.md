---
root: true
targets:
  - codexcli
globs:
  - '**/*'
---

Keep communication direct, grounded, and concise.

- Always answer the immediate prompt directly. Do not include unsolicited
  background context, extra examples, or secondary elaboration before
  answering. The user will ask for expansion or clarification when needed.
- Apply the `technical-writing` skill to all architecture explanations, design
  reviews, implementation plans, code advice, and documentation. Omit it only
  for single-fact lookups, literal translations, syntax rewrites, or brief
  acknowledgments.
- Apply the skill's prose guidance to chat. Read a document profile only when
  the requested output matches that document form.
- Answer direct questions before elaborating. A question requests analysis, not
  implementation, unless the user also asks for a change.
- State uncertainty directly and distinguish verified facts, inferences,
  assumptions, and unresolved questions.
- Use concrete, established terminology. Do not invent unsupported frameworks,
  acronyms, or shorthand.
- Do not use semicolons in conversational or narrative prose.

# Programming

- Use semantic identifiers that state component role, ownership, and scope
  directly without requiring explanatory comments.
- Use the `technical-writing` skill's API Documentation profile for API docs,
  docstrings, and rustdoc. Use its Code Comments profile for source comments.
  Apply the active language or project skill for syntax and domain-specific
  contracts.
- **Fix the design at its source.** Refactor root abstractions, eliminate
  duplicated logic, and update all affected call sites across the codebase.
- **Use established patterns deliberately.** Make an implicit pattern explicit
  when it clarifies the design.
- **Preserve modular boundaries.** Keep concerns separate, interfaces narrow,
  and internals hidden.
- **Keep names semantic.** Never use single-letter names. Abbreviate only
  widely understood terms such as `url`, `id`, and `config`.
  - Name multi-value types with a singular collection role such as `TaskStore`,
    `TaskRegistry`, `TaskSet`, `TaskMap`, or `TaskList`.
  - Rename types at the source instead of adding aliases or compatibility
    wrappers.
- Keep functions focused. Remove wrapper functions that only forward arguments
  to an underlying call without adding parameter transformation, error
  handling, or validation.

# Local Search

- Start local code investigation with `sem`, not broad file reads, `rg`, or Git
  commands.
- Use `sem_entities` to map files and symbols, then `sem_context` to read the
  selected entity and its relationships. Read raw snippets only for omitted
  imports, glue, schemas, generated output, or exact line anchors.
- Use `sem_impact` for dependency and test effects. Use `sem_blame` and
  `sem_log` for ownership and history.
- Use bounded `rg` only when searching for exact string literals, comments,
  configuration keys, or unindexed raw text.
- Use `sem_diff` to inventory tracked changes from `filePath` and `oldFilePath`.
  Inspect raw diffs afterward for exact hunks, whitespace, and line-level proof.

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
