# Planning directory authoring

The plan is the supplied planning directory. Read it with your usual read tools, and create, edit, move, and delete its files with your file-editing tool, such as `apply_patch`, using absolute paths. Never write planning files with shell commands such as `Set-Content`, `Out-File`, or output redirection.

## Layout

```text
<planning directory>/
  docs/overview.md   design narrative; starts with its section headings
  docs/flows.md      runtime flows; starts empty
  docs/tests.md      test coverage; starts empty
  docs/checks.md     extra saved check IDs; starts empty
  files/<path>       one proposal per repository file the plan adds, changes, or deletes
```

`docs/` holds exactly these four files and no subdirectories. `files/` mirrors repository-relative paths: the proposal for `src/main.rs` is `files/src/main.rs`. Any other entry fails submission. Harness keeps this guide, captured originals, and review snapshots outside the directory.

## Proposals

Source proposals contain declarations, complete signatures, and documentation, not executable bodies. Configuration proposals contain complete file contents.

- **Existing file:** call `plan_capture_file` with its repository-relative path, such as `src/main.rs`, before proposing any change to it. Harness extracts the file's declarations into `files/<path>` and returns the absolute proposal path and a read-only original; there is no destination argument. Read the proposal, then edit it in place: add a `Change` under each callable whose behavior changes, add new declarations, and remove the declarations the change deletes. Capture refuses to replace a proposal you wrote at that path without capturing, so never hand-write a proposal for an existing file.
- **New file:** create it directly at `files/<path>` with its declarations, documentation, and behavior blocks.
- **Deleted file:** capture it, then delete its proposal. Deleting a proposal you created withdraws that addition.
- **Renamed file:** capture it, then move its proposal to the new path.

Repeated capture preserves edits and planned deletions. Restoring a proposal to its captured original withdraws the modification.

## Behavior blocks

A modified callable gains one `Change`, one or two sentences that state what changes. New behavior gets one nonempty `Algorithm`, never both; unchanged callables have neither. You classify behavior independently of file renames; Harness validates supplied blocks rather than deriving newness from path history. Name what the callable uses, such as `Draft::revision`, inside its block; the plan has no separate reference sections, so never add Calls, Constructs, or Accesses sections.

Place the standalone heading directly below the signature, indent its content, and leave a blank line after the block.

An `Algorithm` is compact academic pseudocode, one complete statement per line:

- Assign with `←` or `<-` and compare with `=`, `≠`, `<`, `≤`, `is`, or `is not`, as in `if target is none`. Combine conditions with `and`, `or`, and `not`, and compute remainders with `%` or `mod`.
- Write each operation as a call such as `spawn_square(position)`. Lowercase abstract operations need no repository declaration; name real functions, types, and fields directly when you know them.
- Indent the bodies of `if`, `otherwise`, `for each item in items`, and `while`. Use `return`, and use `break` and `continue` only inside loops.
- Construct values explicitly as `Type(field = value)`, `Velocity(direction)` for a tuple type, or `Marker()`, separately from the call that spawns or inserts them.
- Write explanations only in `#` comments. Prose statements and prose conditions are rejected.

Harness checks grammar, not expression types or algorithm correctness.

```text
/// Persist a draft and publish it to readers.
pub fn save(&mut self, draft: Draft) -> Result<()>;
Change
  Reject drafts whose Draft::revision is older than the stored revision.

/// Revision recorded when a draft is stored.
pub struct StoredRevision {
  /// Monotonic revision number of the stored draft.
  pub number: u64,
}

/// Report whether the stored draft supersedes the candidate.
fn is_stale(&self, draft: &Draft) -> bool;
Algorithm
  stored ← self.database.revision(draft.id)
  if stored is none
    return false
  return stored.number > draft.revision
```

## Documentation

Every declaration the plan introduces needs a documentation comment directly above it and above its attributes, including each struct field and enum variant. Use the language's doc-comment syntax, such as `///` in Rust and `/** */` in TypeScript, and do not start a comment with "Returns".

## Design artifacts

`docs/overview.md` starts with `# Title`, followed by Objective, Usage, Requirements, Background, Decisions, Design, and Manual checks sections. Objective, Background, and Design are required. List sections use `- item` with two-space continuation lines; decisions use `- **Decision.** Rationale.`. Manual checks state observations automated checks cannot establish.

`docs/flows.md` has one `## Title` per independently triggered flow, a one-to-two-line description naming its trigger, and exactly one closed Mermaid block. Use `sequenceDiagram` for cross-owner flows, `flowchart` for branching within one owner, and `stateDiagram-v2` for lifecycle transitions.

`docs/tests.md` has one `## path/to/file` per file and items in the following form. Changes are new, modified, removed, or reused; the target is optional.

```text
- new `exact_test_name` [target: runner]: scenario and expected result.
```

`docs/checks.md` lists one saved check ID per line to select checks beyond automatic selection. Discover IDs with `check_definition_list`; Harness adds prerequisites. This file does not configure or authorize checks.

## Submission

Call `plan_status` to see each proposal classified as an addition, modification, deletion, or unchanged, and whether the directory differs from the last submission. Call `plan_submit` with the active plan ID, without an expected version. Harness snapshots the directory, validates the captured contents, and publishes an immutable revision. Repair reported file and line diagnostics in place and submit again; failed submissions do not reset your draft.

Further edits require resubmission before acceptance. Submission is not implementation authority; wait for Harness execution instructions.
