# Forge commands

These commands are registered by `nvim/lua/plugins/forge.lua`. This reference
describes the current local plugin configuration. The source registration defines
argument parsing and completion. Contributor documentation starts at
[Contributing to Forge](../../contributing/forge/README.md).

## Git review

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeStatus` | None | Open the current repository's Git review view |
| `:ForgeBranchDiff <revision>` | One branch or revision | Compare the working tree against that revision |
| `:ForgeBranchDiffFile <file> <revision>` | File, then branch or revision | Open that comparison for one file |
| `:ForgeFileRevision <file> <commit>` | File, then revision | Open a read-only historical file |
| `:ForgeDiffCompactPreview` | None | Preview the compacted working-tree diff |
| `:ForgeDiffCompactPreview!` | Bang | Preview the compacted staged diff |

For example, `:ForgeFileRevision README.md HEAD` opens the committed README.
Revision arguments use the shared revision completion source. File-specific
commands complete the file argument before the revision argument.

## Generated commit messages

Automatic About drafts and commit-message regeneration send a compact diff to the
selected model. A persistent notification shows an animated spinner, model name,
and elapsed seconds until generation completes. Readable provider reasoning summaries appear
when available. Failures preserve the existing draft, including edits made while waiting.

Commit generation uses a persistent, tool-free **Background session**. Codex and Copilot
default to `gpt-6-luna` with low thinking, independently of the active chat. `/config`
provides **Background model**, **Background thinking**, **Background auto compact**, and
**Background compact at tokens**. Choices persist across restarts. Gemini is not part
of this workflow. Other AI features retain their existing adapters.

The first actual generation creates the session. Normal commits reuse it. Switching
branches or rewriting history starts another conversation, with the old timeline retained.
Each request includes the last ten complete Git commit messages for terminology and style.

Background auto-compaction defaults to **128,000 tokens** using the model's default context
window. Ordinary Harness sessions default to off and have separate **Auto compact** and
**Compact at tokens** controls in `/config`. Compaction runs after completed turns, with no
idle timer. Missing provider usage cannot trigger compaction. Failed compaction is visible
and does not discard generated text.

`:ForgeDebugBackgroundTimeline [session_id]` opens the recorded Background conversation
without changing the active chat or sending model input. Use **Tab** to expand rows, **H**
to select an earlier conversation, **c** for its compaction configuration, and **q** to close.
Opening the command before any generation does not create a session.

```lua
require("forge").setup({
  background_generation = {
    codex = { model = "gpt-6-luna", reasoning_effort = "low" },
    copilot = { model = "gpt-6-luna", reasoning_effort = "low" },
  },
})
```

The context starts with changed files and added/deleted line counts.
Source excerpts retain function signatures. Modified functions include the first 20
changed body lines and two nearby unchanged lines. Added and removed functions retain
only their attributes and signatures. Test functions follow the same rule.
Changed old/new signature pairs appear together. Body lines
stay in original diff order, including inline
callbacks and nested functions. Test/spec files follow other files. Standalone imports
remain in the counts but are omitted from excerpts. Mixed import/code rows and runtime
import calls remain visible. Markdown and plain-text changes retain all
changed lines instead of applying the 20-line limit.

The constructed input is capped at 500,000 UTF-8 bytes. At that limit, further excerpts
stop, including signatures. Files without an included excerpt appear only in the file
list. Omitted ranges within an excerpt show `... N lines (+A -D)`.
Whitespace-only gaps have no marker. A single omitted line is shown directly, even
when the function body would otherwise be hidden, subject to the global byte limit.
Unsupported or malformed syntax uses ordinary diff excerpts rather than signature-aware excerpts.
License files and common dependency lockfiles keep their paths and change counts in the
file list but have no excerpts, including license files with Markdown or text extensions.

## Harness and permissions

A file-write request in Read mode offers **Switch to Write mode** or **Stay in Read mode**.
Switching resumes the task under the configured directory limits and permission rules.
Staying in Read mode rejects that request.

Long prompts and responses fold to a three-line preview. Prompts fold when sent, and the
previous response folds when a new exchange starts. Use the normal expansion command on
the caret or message body to show or hide the complete text.

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeHarness` | None | Open the Harness |
| `:ForgeHarnessNew` | None | Create a fresh Harness session |
| `:ForgeHarnessLog` | Optional `on`, `off`, or `open` | Control or open the active session's protocol trace |
| `:ForgePermissions` | None | Edit the Harness permissions document |

`ForgeHarnessLog` requires an open Harness session. Inside Harness, `/sessions`
opens durable session search and `/undo` opens checkpoint rollback.

The live status line shows the current phase, elapsed time, and interrupt key.
The latest thought appears below in italics, aligned with the phase label, and
wraps to the window width.
Codex and Copilot request concise reasoning summaries when the selected model supports them.
Claude and Antigravity retain provider-defined summary detail through their current ACP integrations.
Assistant commentary marked with an open circle is separate from the reasoning-summary setting.

`/config` includes an **LSP tools** toggle. It enables the read-only
[`lsp` tool](lsp-tools.md) for model-driven code exploration without
changing interactive plan navigation or starting language servers.

`/checks` opens project checks, and `/checks setup` configures their commands and
selection policies. See [Project checks](checks.md) for package-aware commands and
new-test selection.

PlanReview shows declaration changes with nearby context lines and enclosing
declaration boundaries, using the regular diff grouping rules. Nearby method
changes share their enclosing `impl` context. Distant changes appear in separate
groups without repeating that `impl` header. Explicit reference navigation can
reveal nearby context around a declaration outside the displayed changes.
Regular diffs and PlanReview render enclosing declarations as syntax-highlighted
source lines. An indented `...` marks omitted source between visible regions.
New functions and methods contain an `Algorithm` block in academic pseudocode.
Algorithm blocks highlight control keywords, assignment and comparison operators, literals,
and identifiers. Variables and named fields are white, while descriptive prose is comment gray.
Algorithm and Change headings use the same style as Calls, Constructs, and Accesses.
Wrapped rows preserve source indentation after the diff gutter. Constructors retain type highlighting.
Press `.` on an Algorithm parameter name to jump to that parameter in the function signature.
Press it on a call or member name to jump to its declaration. Receiver types take priority,
then Calls, Constructs, and Accesses resolve shortened names. Ambiguous matches report the
candidate names without moving the cursor. Jump-back returns to the original Algorithm token.
Write constructions explicitly as `Projectile(damage = 1)` or `CombatEntity()` for a marker.
Each `Constructs` entry must match an invocation in the Algorithm, using its exact qualified
name and named arguments. Keep construction separate from the operation that queues or inserts it.
Submission checks statement syntax and indentation, including branch and loop
structure, and reports the callable and failing line. Drafts can remain incomplete while edited.
Modified functions use `Change` for behavioral deltas in one or two succinct
sentences. Expand a function to read its behavior and reference sections.
Collapsed signatures omit the `[changed]` suffix.
Old and new function versions expand independently. Collapsing an old function
keeps added declarations outside its body visible.

Flow diagrams render as native buffer rows with branch rails, literal labels, and declaration-kind
symbol colors. Each flow starts with its trigger. `→` starts execution, `↓` continues it, `←` marks
inputs, and `┄▶` marks resulting state changes. Conditional branches contain actions, while results
cannot contain further steps. Independent events appear as separate flows.
Press `.` on a function, type, or qualified member name to jump to its definition.
Ambiguous names use the existing symbol picker. Diagram fences remain part of text exports,
and do not appear in PlanReview.

Inline-code names in plan prose use declaration-kind colors when the proposed
plan resolves them. Go to definition opens that proposed declaration, including
private declarations hidden by the Public filter. Ambiguous names open a picker.
Definition jumps reveal omitted declarations and open their enclosing folds in
PlanReview. Definitions outside the plan open in normal source buffers.
The `open` action uses the same definition navigation, or opens the source file from a file heading.
Declaration navigation never opens a popup.
While a planning response is running, follow-up instructions can revise and resubmit the plan
in that same response. A later edit requires a new submission before review. Submission never
authorizes implementation.
Save pending annotations before a definition jump that changes the projection.
`Constructs` lists directly instantiated types in green and factory calls with function highlighting.
Factories are identified by their owned named return type. `Calls` lists other invocations and
callbacks, while `Accesses` lists fields and named values. Type mentions in
signatures do not imply construction. References use the same definition navigation.
Type references retained under `Accesses` also use green type highlighting.
Test names use function highlighting and omit the leading `tests::` in the file
list. Navigation and test selection retain the full qualified name.
Names absent from the plan fall back to exact workspace symbol lookup through
running project language servers. Removed declarations do not fall back to old
source definitions.

Inside PlanReview, `oc` opens plan-specific check selection. Space toggles a
check, with `●` for enabled and `○` for disabled. Visual `j` deletes selected
planned tests or disables selected checks after confirmation.
Rust test-only modules and test functions are hidden from Changes in both Public and All views.
Their saved declarations remain intact, and planned cases remain in Tests.
The Tests section groups cases under New, Modified, Deleted, or Unchanged file headings.
File groups expand by default and toggle with the usual fold action. Test names and descriptions
use shared diff gutters and full-line backgrounds, including orange `~` rows for modified cases.
Select either a test name or its description to delete that planned case. Deleting the last
case removes its empty file group without deleting project source files.

## Build and diagnostics

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeBuild` | None | Explicitly build Forge and report the selected executable |
| `:ForgeStartupLog` | None | Open the startup diagnostic log |
| `:ForgeGitConfigCacheReset` | None | Clear the cached Git configuration location |
| `:ForgeDebugBackgroundTimeline [session_id]` | Optional saved Background session ID | Inspect generation history, provider status, failures, and compaction |
| `:ForgeDebugCommitPromptPreview [comparison]` | Optional `head`, `staged`, or `latest_commit` | Inspect the exact system instructions and compact commit context without calling a model |

`ForgeDebugCommitPromptPreview` defaults to `head`, including staged, unstaged,
and untracked changes. It uses the current Forge Status workspace when invoked
there, otherwise the current working directory. The read-only scratch buffer shows
the UTF-8 input size, system instructions, and user prompt. Press `q` to close it.
Long lines wrap at word boundaries without changing the prompt text.
Each invocation collects current repository state. Empty comparisons appear in
the buffer, and failed requests produce an error notification.
Renamed files appear once in the compact file list with their original path and
have no detailed content excerpt, including when the rename also contains edits.

A successful build does not replace an executable already loaded by a running
host. Restart Neovim to use the rebuilt host. Compilation errors appear as build
failure notifications. See [Building and testing](../../contributing/forge/building-and-testing.md)
for the contributor workflow.

GitHub PR and review views are opened by their integration, rather than by an
additional standalone Forge PR command.

PR creation checks the selected base and head for merge conflicts before preparing
the diff or requesting a description. Conflicts stop the workflow with "Merge
conflicts. Resolve them before creating the PR." The check leaves your branch,
index, and working files unchanged.
