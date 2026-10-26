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

ForgeStatus file rows, file-specific comparison labels, and single-file discard
confirmations show paths relative to the editor cwd in the window that opened the view.
Git actions continue to use native file identities and repository paths.

Unfolding an unloaded file keeps its heading visible until the first diff chunk has
syntax colors and hunk context. A syntax failure reports the error and still shows the diff.

Completed stage and unstage actions invalidate the native Git index cache before
settlement. Files retain their settled section even when the replacement index has
an older timestamp, without restarting Neovim.

For example, `:ForgeFileRevision README.md HEAD` opens the committed README.
Revision arguments use the shared revision completion source. File-specific
commands complete the file argument before the revision argument.

Opening a file from ForgeStatus uses a new tab by default, including historical
source for deleted files. Set `status_open_target = "buffer"` in
`require("forge").setup({ ... })` to open files in the Status window instead.
The default is `status_open_target = "tab"`.

## Generated commit messages

Automatic About drafts and commit-message regeneration send a compact diff to the
selected model without progress, waiting, or completion popups. Generation failures
produce error notifications and preserve the existing draft, including edits made while waiting.

Git LFS files are excluded from both the changed-file inventory and compact diff before
Forge reads their content. This applies to automatic drafts, staged regeneration, amend
generation, and pull-request descriptions, including untracked files and renamed LFS
paths. LFS-only changes produce no generation request.

Commit generation uses a persistent, tool-free **Background session**. Codex and Copilot
default to `gpt-6-luna` with low thinking, independently of the active chat. `/config`
provides the **Background** row in **Models** for model, thinking, and supported speed choices, and
its **Background** row in **Compaction** for automatic compaction and the token threshold. Choices persist across restarts. Gemini is not part
of this workflow. Other AI features retain their existing adapters.

Background **Speed** is independent of Main and applies to subsequent Background
requests. It offers `default` and the accelerated tiers advertised for the selected
model. Codex's model catalog determines whether `fast` or `ultrafast` is available.
Changing models resets an incompatible speed to `default`. The control is hidden
for providers without accelerated tiers. Invalid combinations are rejected before
provider submission, including settings supplied outside `/config`.

The first actual generation creates the session. Normal commits reuse it. Switching
branches or rewriting history starts another conversation, with the old timeline retained.
Each request includes the last ten complete Git commit messages for terminology and style.

Background auto-compaction defaults to **128,000 tokens** using the model's default context
window. Ordinary Harness sessions default to off. Their **Main** row in
**Compaction** combines the automatic toggle and token threshold in `/config`. Compaction runs after completed turns, with no
idle timer. Missing provider usage cannot trigger compaction. Failed compaction is visible
and does not discard generated text.

`:ForgeDebugBackgroundTimeline [session_id]` opens the recorded Background conversation
without changing the active chat or sending model input. Use **Tab** to expand rows, **H**
to select an earlier conversation, **c** for its compaction configuration, and **q** to close.
Opening the command before any generation does not create a session.

```lua
require("forge").setup({
  background_generation = {
    codex = { model = "gpt-6-luna", reasoning_effort = "low", service_tier = "fast" },
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

New interactive sessions start in Write mode. Reopening a session preserves its
saved permission mode. Use `/read`, `/write`, or `/yolo` to change the current mode.

Copilot can read and write its current session's notes directory without approval,
including in Read mode. Harness gets that directory from the native CLI, so the
location can differ across platforms and installations. The exception does not
include other sessions, Copilot configuration, or symlink targets outside that
directory. Explicit deny rules and protected Harness files still apply.

Harness automatically retries recognized network and model-capacity failures
before any tool or model output, up to five times with delays of 1, 5, 10, 30, and
60 seconds. The status shows the retry count and a countdown, such as
`Working · retry 2/5 · retrying in 5s`. Cancellation remains available while
waiting. If recovery fails, Forge shows the final provider error. With tracing
enabled, `/log` records every attempt and error. Authentication, quota, tool,
unknown, and potentially partially executed failures do not trigger this retry.

Failed prompt delivery preserves your input and attachments. Your next prompt
includes earlier input only when Forge knows it was never delivered. Your newest
instruction can amend or cancel that earlier context. If delivery is uncertain,
Forge checks provider history before sending again. When acceptance cannot be
confirmed, Forge keeps both prompts saved and displays the failure instead of
risking duplicate execution. This also applies to live steering.

A file-write request in Read mode offers **Switch to Write mode** or **Stay in Read mode**.
Switching resumes the task under the configured directory limits and permission rules.
Staying in Read mode rejects that request.

Long prompts and responses fold to a three-line preview. Prompts fold when sent, and the
two latest prior responses stay open during the current exchange. Submitting another prompt folds
the third-to-last response. Thinking and tool sections keep their usual folding behavior.
Use the normal expansion command on
the caret or message body to show or hide the complete text.

`Running` tool groups show only the latest call and its four-row output preview.
When a group changes to `Ran`, it folds automatically. Expanding that completed group
shows every call in the group, with individual output available on expansion.

Thoughts with activity start collapsed and preserve the assistant's text in their heading.
Expanding a thought shows its events first, then its tool summary, then its file changes,
one indentation level deeper. Thoughts without activity keep the `○` marker. Your explicit
expansion choice survives incoming activity. Tools and file writes accumulate into two entries,
even when they alternate. Each tool updates the tool group, and each completed write updates `Changed N files`.
Its file count, diff, and totals update in place while preserving expansion. Repeated writes
to the same file count once and show the original-to-final diff. A pending write shows
`Changing N files` or a changing-files suffix. A new thought, user input, or question interaction starts a new section.
Owned events stay inside the thought, including compaction. Late results stay with their
original operation. Failed writes retain their diagnostics inside the changes group.
Inconsistent edit reports retain their reported hunks with a diagnostic.
Each interaction also shows expandable **Changed files** and **Checks** summaries alongside
**Thought**. They update in place and remain visible when Thought is collapsed. Changed files
combines reported edits across thoughts and counts each file once. Checks combines recorded
command outcomes and elapsed times across completed runs. Individual file modifications and
check runs remain under their owning subthoughts, with the same expandable detail controls.
File checkpoints remain available through `/undo`.

A file created and deleted within one changes group does not appear in its net diff.
If no files remain changed, the summary shows `No net file changes`. Individual writes
remain available under their original thoughts.

`/fork [name]` opens a child timeline with a snapshot of the source's visible history
above its fork marker. New child messages append below that marker. Source updates do
not change the copied history, and pending questions, tasks, and agents do not resume
in the child. Inherited question and plan controls are inactive.

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeHarness` | None | Open the Harness |
| `:ForgeHarnessNew` | None | Create a fresh Harness session |
| `:ForgeHarnessLog` | Optional `on`, `off`, or `open` | Control or open the active session's protocol trace |
| `:ForgePermissions` | None | Edit the Harness permissions document |

`ForgeHarnessLog` requires an open Harness session. Inside Harness, `/sessions`
opens durable session search and `/undo` opens the interaction list directly.
After selecting an interaction, choose **Conversation only** to keep current files,
or **Conversation and files** to restore its checkpoint too. Conversation-only
rollback works without a file checkpoint. Confirming conversation rollback collects
active execution before changing history and leaves the task paused. An unfinished
restore requires recovery first.

The status area stays present between responses and shows **Idle** when no work is running.
It keeps **Working** visible while handing off to the next queued message.
Idle uses the same static icon as a question wait. At the live transcript tail, the status area
aligns with the bottom of the pane. Check activity, queued messages, and other current details
add rows below the phase label only while present. Scrolling through history remains available.
Queued follow-up inputs have a blank line above their heading, including when no steering inputs are present.
Each entry previews only its first line on one display row, with an ellipsis when
that line exceeds the available width. Editing opens the complete message in the
prompt and keeps the status and queue visible at the bottom of the transcript.
Live tool output occupies four rows, including blank padding.
The live status line shows the current phase, elapsed time, and interrupt key.
Thought headers retain tool totals and append failed-tool and changed-file counts
when nonzero. Repeated successful edits to one file count once.
Enable **Thoughts** in `/config` under **Display** to show the latest thought below it in
italics, aligned with the phase label on one line, with `...` when it exceeds the available width. The option
defaults to Off, persists across sessions, and applies without interrupting an
active response. Thoughts never appear as commentary in the timeline. Only the latest
readable reasoning summary appears in status when this option is On.
Timeline updates preserve the current cursor and reading position, including when the thought
preview changes. Active selections retain both endpoints and their selection type.
If selected text disappears, each endpoint moves to the nearest surviving position.
Submitting a prompt positions the transcript at its tail once.
Codex and Copilot request concise reasoning summaries when the selected model supports them.
Claude and Antigravity retain provider-defined summary detail through their current ACP integrations.
Assistant commentary marked with an open circle is separate from the reasoning-summary setting.

Explicit steering sends the current composer text to the active response while
preserving queued follow-ups. If the active turn cannot accept it, the text joins
the follow-up queue and Harness shows a warning.
Queued prompts and pending steering appear below the status in the upper timeline
pane. The lower prompt buffer contains only the draft being edited.
Queued prompts remain visible until accepted. If submission fails before acceptance,
the message and its attachments stay queued for inspection or resubmission. Messages
waiting for execution capacity do not interrupt the active response or checks.
The prompt inherits ordinary text-buffer editing and navigation mappings. Harness
adds submission, queueing, steering, cancellation, mode, and effort controls there.
Normal mode also supports session commands, including `oe` to reopen a pending
question or approval, model and artifact selection, help, and closing Harness.
In Insert mode, `Ctrl-J` inserts a newline at the cursor and preserves the draft.
Press `Up` on the first draft line to recall submitted prompts. Repeated `Up` and
`Down` browse history, and `Down` past the newest entry restores your unfinished
draft and attachments. Inside a multiline draft, the arrows move between lines.
`Alt-Up` and `Alt-Down` also browse history. Timeline row activation, folding,
and gutter selection remain in the upper pane.

Unknown or unfinished slash commands keep the draft and show a warning. A leading
path with a filename extension, such as `/workspace/screenshot.png`, submits and
queues as ordinary text. Extension detection also accepts spaces in a path on the
first line and attachments on later lines. Supported commands still take priority,
so `/plan inspect screenshot.png` starts planning rather than sending an ordinary prompt.

`M-s` edits the newest queued prompt in place and leaves the composer in Normal mode.
Repeating it moves backward through
the queue, and `M-t` moves forward while editing. Navigation preserves message order
and saved drafts. Earlier messages may execute, but the
message being edited and all later messages wait. `Ctrl-S` saves the edit in its
reserved position. Submitting an empty edit deletes that message. Finishing restores
any unqueued draft that was in the composer before editing began.
While editing, `Ctrl-Q` instead removes the selected message from the queue and keeps
its edited text and attachments in the normal prompt. It does not submit automatically,
so you can steer that text into the active response or submit it later. The upper bar
shows the configured unqueue and queue-navigation shortcuts during editing.

Paste an absolute file path into the Harness composer to attach the file. Terminal
drag-and-drop works when the terminal pastes the dropped paths. Quoted paths,
paths containing spaces, file URIs, and multiple dropped files are supported.
Normal-mode `p` and `P` also import pasted file paths. Ordinary text pastes keep
their existing behavior. Clipboard image pixels are not read.

An import keeps the path visible while copying the file asynchronously, then
replaces it with `[Image #1]`, `[PDF #2]`, or `[File #3]`. The marker retains a
separate typed file object. Copy or cut its complete label with your usual text
selection and editing commands, then paste it elsewhere in the draft. Full-line and visual
copies that include the complete label also retain its attachment state. Typing
the visible label creates ordinary text. Undo, prompt history, and queued-message
editing preserve complete references. Moving or deleting the original file after
import does not change the captured attachment.

Codex and image-capable ACP agents receive image inputs directly. Copilot receives
native file attachments. PDFs and other files sent to Codex or ACP are retained
file references for the agent to inspect with tools, rather than native document
inputs. Import errors keep the original path and report the failure. Finish pending
imports before submitting. Attempts during import retain the draft and show a notice.

`/config` includes an **LSP** toggle under **Tools**. It enables the read-only
[`lsp` tool](lsp-tools.md) for model-driven code exploration without
changing interactive plan navigation or starting language servers.

`/config` presents grouped rows for Models, Compaction, Checks, Planning, Execute, Permissions,
Tools, Display, Provider, and Debug. Use Up/Down to select a row,
Tab/Shift-Tab to select its fields, Left/Right to change values, Space to toggle
`●` enabled or `○` disabled, and Enter to edit a number or open a selector.
Planning has its own permission and Auto-approve controls. Execute has a Permission
choice of Write or YOLO, applied to new goals and accepted plan execution.
Tools contains Timeouts and separate Space toggles for Subagents and LSP. Display contains Thoughts, and Debug at the
end contains Logging. The explanation below the rows follows the focused field. Related settings share
rows, including model and thinking, automatic compaction and its threshold,
check shell and timing, access scope and extra directories, and both timeouts.
Check-editing permission is also available under **Checks → Setup** without
sending a prompt. Unsupported controls explain their restriction, and provider
setup and Windows sandbox options appear only where applicable.

Input timeouts run independently of the picker. An idle session has no periodic
timeout check. Changing a timeout affects future questions and approvals without
extending an existing deadline. If another execution prevents a clarification
from being submitted, Harness shows **Not submitted**, keeps the question open,
and restores the clarification text for retry.

`/checks` opens project checks, and `/checks setup` configures their commands and
selection policies through a guided conversation. `/checks setup on` and
`/checks setup off` toggle check-editing permission without sending a prompt.
The Harness winbar shows the current permission. See [Project checks](checks.md) for package-aware commands and
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
symbol colors. Each flow starts with its trigger. `→` starts execution, `↓` continues it, and
`┄▶` marks resulting state changes. Conditional branches contain actions, while results
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
Approving a plan and requesting changes both open an optional-comment popup. Press
`Enter` or `Ctrl-S` in normal or insert mode to submit, including an empty comment.
`Esc` or `Ctrl-C` cancels the decision. Approval comments remain with the accepted
revision and are included in execution context. A plan changed while its popup is
open must be reviewed again before the decision can be submitted.
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
| `:ForgeBuild` | None | Build the debug executable with optimized external dependencies |
| `:ForgeBuildRelease` | None | Explicitly build the release executable |
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

Forge launches the debug executable by default. `:ForgeBuildRelease` does not
switch the runtime profile. Set `vim.g.forge_build_profile = "release"` before
loading Forge to select the release executable explicitly. `:ForgeBuild` always
builds debug, including when the current runtime selects release.

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

PR descriptions use Harness with the same **Background model** and **Background thinking**
settings in `/config` as commits. Codex and Copilot both default to `gpt-6-luna` with
low thinking. PR descriptions and commits share one persistent, branch-aware Background
session and its compaction policy. Each request supplies its own output instructions.
Both appear in `:ForgeDebugBackgroundTimeline`. Provider failures stop PR creation and
appear in a notification.
