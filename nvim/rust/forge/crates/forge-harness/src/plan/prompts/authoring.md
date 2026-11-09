# Planning directory authoring

Edit the supplied planning directory with ordinary file tools. Its only top-level directories are `docs/` and `files/`; Harness owns capture records and review snapshots elsewhere.

## Files and capture

Before proposing changes to an existing repository file, call `plan_capture_file` with its repository-relative path. Harness copies its declarations into `files/` at that same path and returns the absolute proposal path; there is no destination argument.

Source proposals contain declarations, complete signatures, and documentation, not executable bodies. Configuration proposals contain complete file contents. Create new repository paths directly under `files/`. Rename and delete proposals with ordinary file operations; deleting a captured file proposes its repository deletion, while deleting a new file withdraws the addition. Repeated capture preserves edits and planned deletions.

## Behavior and documentation

Use one nonempty `Algorithm` block for new behavior or one one-to-two-sentence `Change` block for modified behavior. An unchanged callable has no block. You classify the behavior; Harness validates supplied blocks rather than deriving newness from path history.

Place the standalone heading directly below the signature, indent its content, and leave a blank line after the block. Never add separate Calls, Constructs, or Accesses sections.

```text
/// Save an existing record.
pub fn save();
Change
  Reject stale revisions before writing.

/// Report whether a candidate is stale.
pub fn is_stale(candidate: Revision) -> bool;
Algorithm
  stored <- revision_lookup(candidate.id)
  return stored.number > candidate.number
```

Algorithm statements are one per line: assignment with `<-` or `←`, calls, comparisons, `if`/`otherwise`, `for each item in items`, `while`, and `return`. Indent control bodies. Use `break` and `continue` only inside loops and explanations only in `#` comments. Lowercase abstract operations need no repository declaration. Harness checks grammar, not expression types or algorithm correctness.

Document introduced declarations, including fields and variants, above their attributes with the language's doc-comment syntax. Do not start documentation with "Returns".

## Design artifacts

`docs/overview.md` starts with `# Title`, followed by Objective, Usage, Requirements, Background, Decisions, Design, and Manual checks sections. Objective, Background, and Design are required. List sections use `- item` with two-space continuation lines; decisions use `- **Decision.** Rationale.`. Manual checks state observations automated checks cannot establish.

`docs/flows.md` has one `## Title` per independently triggered flow, a one-to-two-line description naming its trigger, and exactly one closed Mermaid block. Use `sequenceDiagram` for cross-owner flows, `flowchart` for branching within one owner, and `stateDiagram-v2` for lifecycle transitions.

`docs/tests.md` has one `## path/to/file` per file and items in the following form. Changes are new, modified, removed, or reused; the target is optional.

```text
- new `exact_test_name` [target: runner]: scenario and expected result.
```

`docs/checks.md` lists one saved check ID per line to select checks beyond automatic selection. Discover IDs with `check_definition_list`; Harness adds prerequisites. This file does not configure or authorize checks.

## Submission

Call `plan_submit` with the active plan ID, without an expected version. Harness snapshots the directory, validates the captured contents, and publishes an immutable revision. Repair reported file and line diagnostics with ordinary tools and submit again; failed submissions do not reset your draft.

Further edits require resubmission before acceptance. Submission is not implementation authority; wait for Harness execution instructions.
