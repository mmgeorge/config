# Forge commands

These commands are registered by `nvim/lua/plugins/forge.lua`. This reference
describes the current local plugin configuration. The source registration defines
argument parsing and completion. Contributor documentation starts at
[Contributing to Forge](../../contributing/forge/README.md).

[Local text correction](text-correction.md) documents the optional local spelling service,
its eligible prose regions, and immediate Backspace rejection. It does not add a slash
command or a provider request.

## Harness inspection

Opening, creating, or resuming a Harness conversation starts its Codex or Copilot
provider session in the background. The editor remains usable while the provider
and MCP servers initialize. This sends no prompt and starts no model turn.
`/mcp` reports connection progress without requiring a first message. The first
prompt reuses the initialized provider thread. Startup failures appear as session
errors and remain available through `/mcp`.

Harness agents can inspect configuration with `harness_config` and recall previous
work through `harness_session_list`, `harness_session_search`,
`harness_session_exchange_list`, `harness_session_exchange`, `harness_session_tool`,
`harness_session_status`, and `harness_session_log`. These are model tools, not
editor commands. `/config` → **Tools** → **Session history** toggles all seven
history tools. Configuration inspection and the `harness-sessions` skill remain
available when history is off. Session-specific reads default to the current
session. Unfiltered listing/search considers all accessible sessions.

`:ForgeHistoryClear` stops its Forge host and attempts to delete every version
under `stdpath("data")/forge/harness/state/`. It removes unlocked versions completely:
sessions, Background conversations, transcripts, plans, captured files, journals,
and private provider runtimes. A live owner or locked database preserves that whole
version; the command reports its path and reason while clearing unlocked siblings.
It removes `state/` when empty. Settings, model preferences and pins, permissions,
repository files, and unsent composer text survive. Reopen `:ForgeHarness` afterward
to start a new session. Breaking state versions select a new directory automatically,
so old editors do not prevent new hosts from starting. The former unversioned
storage layout lies outside this command's deletion scope.

`harness_session_log` reads newest-first and returns up to 1,000 complete entries
per page by default. Its optional positive integer `limit` selects another page
size. Follow `next_cursor` with the same arguments to read older records; the
reader excludes later appends. The response byte budget does not split log pages
or large entries. The first request reads only its batch, not the entire retained log.

Provider cleanup errors show the retained original cause separately from cleanup
failures when available. Terminal protocol errors identify missing or invalid
inventory fields without exposing command text. A failed cleanup still blocks
provider restart until collection succeeds; these diagnostics do not authorize
replaying an uncertain prompt.

## Git review

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeStatus` | None | Open the current repository's Git review view |
| `:ForgeBranchDiff <revision>` | One branch or revision | Compare the working tree against that revision |
| `:ForgeBranchDiffFile <file> <revision>` | File, then branch or revision | Open that comparison for one file |
| `:ForgeFileRevision <file> <commit>` | File, then revision | Open a read-only historical file |
| `:ForgeDiffCompactPreview` | None | Preview the compacted working-tree diff |
| `:ForgeDiffCompactPreview!` | Bang | Preview the compacted staged diff |

Invoking `:ForgeStatus` from a floating picker or a window pinned to another buffer
reuses an existing non-floating Status window, or opens a new tab if none exists.
Forge leaves the picker or pinned buffer untouched.

ForgeStatus file rows, file-specific comparison labels, and single-file discard
confirmations show paths relative to the editor cwd in the window that opened the view.
Git actions continue to use native file identities and repository paths.

Unfolding an unloaded file keeps its heading visible until the first diff chunk has
syntax colors and hunk context. A syntax failure reports the error and still shows the diff.

Text LFS files show read-only line diffs without syntax highlighting or semantic
declaration grouping. Forge resolves pointers from local LFS storage and supports
UTF-8 content without NUL bytes; it does not download missing objects. Missing or
corrupt objects and unsupported content produce diagnostics. Stage, unstage, and
discard reject selections containing LFS files.

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

## Diff controls

Status, branch comparisons, PR and commit reviews, walkthrough review excerpts,
and saved timeline diffs start with **Full** presentation and **All** visibility.
The controls apply to the owning document across its windows.

| Key | Control | Values |
| --- | --- | --- |
| `ov` | Declaration visibility | All / Public |
| `ox` | Diff presentation | Full / Semantic |

Configure these keys through `keymaps.diff.toggle_visibility` and
`keymaps.diff.toggle_presentation`. Harness binds them in the transcript, leaving
the composer available for editing.

Semantic presentation shows declaration signatures. A `~` marker also identifies
body-only edits when the signature remains unchanged. A collapsed body shows its
change count, such as `pub fn update() { … 2 changes }`. Unfold the declaration to
read its source diff. PlanReview starts in Public visibility and supports Semantic
presentation only, so it binds `ov` without `ox`. Its unfolded declarations contain
the saved Change or Algorithm. Plans store no Calls, Constructs, or Accesses, so
unchanged functions show only their signatures.

Public visibility preserves unclassified source changes under an explicit fallback
label. Unsupported syntax or unavailable historical source cannot silently remove
those changes. Saved timeline diffs use captured checkpoint sources, and opening
their source view preserves that historical comparison after worktree edits.

## Generated commit messages

Automatic About drafts use only staged changes when any exist. With no staged
changes, they use unstaged tracked changes and untracked files, excluding ignored
files. A partially staged file contributes its staged contents only. Commit-message
generation sends a compact diff to the selected model without progress, waiting, or completion popups. Generation failures
produce error notifications and preserve the existing draft, including edits made while waiting.

Reopening ForgeStatus reuses its cached About draft without checking changes made
while Status was closed. Press `Ctrl-A` in Normal mode to regenerate About from
the current staged changes, or non-ignored worktree changes when nothing is staged.
It cancels any previous About generation and keeps waiting commit editors attached
to the replacement request. Changes detected while the same Status context remains
alive still trigger automatic regeneration.

For a new commit, the editor copies the existing About draft or waits for its in-progress
generation. It does not start another generation. About and the commit editor's first
line show an animated `Generating with <model>` status, including thinking and speed.
Failures and cancellation restore the previous subject. In the commit editor, `Ctrl-A` cancels the previous
generation request before regenerating from staged changes, or from the latest commit
when amending. Late results from the cancelled request cannot replace the new draft.
Entering Insert mode while generation is pending cancels that request and removes
the spinner, preserving the current draft for manual editing. `Ctrl-A` can start
generation again explicitly.

Git LFS files are excluded from both the changed-file inventory and compact diff before
Forge reads their content. This applies to automatic drafts, staged regeneration, amend
generation, and pull-request descriptions, including untracked files and renamed LFS
paths. LFS-only changes produce no generation request.

Commit generation uses a persistent, tool-free **Background session** whose provider is
chosen separately from the active chat, so a Claude Code chat can generate commits with
Codex and the reverse. The Background provider defaults to Codex. Codex and Copilot default
to `gpt-6-luna` with low thinking, and Claude Code defaults to `haiku` with low thinking.
`/config` provides a **Background model** section in **Models** for provider, model,
thinking, supported speed, automatic compaction, and token threshold choices. The model
list follows the selected provider. Choices persist across restarts. Gemini is not part
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
window. Ordinary Harness sessions default to off. Their **Main model** section in
**Models** includes the Compact toggle and token threshold in `/config`. Compaction runs after completed turns, with no
idle timer. Missing provider usage cannot trigger compaction. Failed compaction is visible
and does not discard generated text.

`:ForgeDebugBackgroundTimeline [session_id]` opens the recorded Background conversation
without changing the active chat or sending model input. Use **Tab** to expand rows, **H**
to select an earlier conversation, **c** for its compaction configuration, and **q** to close.
Opening the command before any generation does not create a session.

`background_provider` sets the Background provider until `/config` saves another choice.

```lua
require("forge").setup({
  background_provider = "codex",
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

`@` file completion shows paths relative to the Harness session's workspace.
Long menu labels use Snacks middle shortening, retaining the first directory and
the trailing path components that fit. Extremely long filenames can shorten too.
Matching and insertion always use the complete workspace-relative path.
Queries without `/` search both filenames and directories. Exact filenames and
stems rank first, followed by filename prefixes, exact directory components, and
weaker substring or fuzzy matches. A query such as `@rust/skill` favors a trailing
`rust/SKILL.md` match at any depth. A trailing `/` favors direct children of the
matching directory. Matching ignores case, with exact case breaking ties.
Recent file selections within the current editor and workspace, open buffers,
shorter paths, and alphabetical order break remaining ties. Bare `@` uses that
contextual ordering without a filename preference.

Ctrl-Q queues `/model <model> [effort]` without changing the active turn, even
when the command contains no follow-up prompt. The model change applies when its
entry reaches the front of the follow-up queue. Ctrl-S submits a settings-only
model change immediately through the configuration coordinator.

New interactive sessions start with full reads, workspace writes, and `ask` approval.
Reopening a session preserves its filesystem access and tool approval independently.
The winbar separates every field with a small gray `·`. One gray access/approval code
appends `dx` for Deny or `ax` for Allow to the filesystem abbreviation. Ask adds no suffix:
`rxwx` means full reads/writes with Ask, `rxwxdx` with Deny, and `rxwxax` with Allow.
Enabled sandbox status precedes the model.
Opening Harness waits for the session snapshot before showing permission and model
fields. New sessions reuse the last confirmed default model for that workspace and
provider while refreshing the catalog in the background. Without a cached model,
the field shows `resolving model` until the provider responds.
The model field reads `<speed icon> <model> <thinking>`: Fast uses a gray outlined
lightning glyph (`󱐌`), Ultrafast a filled glyph (`󱐋`) in YOLO's neon red, and Standard no icon.
Model, thinking, or speed changes waiting for existing work share one trailing `*`
after thinking. Idle and paused changes show no marker. The marker disappears when
all selected values are applied.
Only full reads, full writes, `allow`, and sandbox off add red `YOLO` text:
`YOLO · rxwxax`. Working, planning, revision, and review labels remain white
regardless of the selected policy.

| Access | Read scope | Write scope |
| --- | --- | --- |
| `r` | Workspace | Off |
| `rx` | Full | Off |
| `rw` | Workspace | Workspace |
| `rxw` | Full | Workspace |
| `rwx` | Workspace | Full |
| `rxwx` | Full | Full |

The suffix `x` means full filesystem scope for the preceding operation.

| Command | Effect |
| --- | --- |
| `/read full` | Toggle workspace/full reads, preserving writes and approval. |
| `/write` or Shift-Tab | Toggle writes between off and workspace, preserving reads and approval. |
| `/write full` | Toggle workspace/full writes, enabling full writes from off. |
| `/ask` | Apply saved rules and ask for remaining tool requests. |
| `/deny` | Apply saved rules and reject requests that still need approval. |
| `/allow` | Apply saved rules and approve remaining tool requests once. |
| `/yolo` | Toggle YOLO. Enter with full reads/writes, `allow`, and sandbox off. Exit with full reads/writes, `ask`, and sandbox enabled where supported. |

Disabling full writes discards that full selection. The next `/write` enables
workspace writes. Approval commands do not enable writes or change scope. Enabling
the sandbox removes the `YOLO` indicator. Bare `/read`, `/mode`, and the `all` forms are removed.

Saved denies, protected targets, malformed shell commands, and write-off remain
authoritative in every approval mode. An out-of-scope file request can receive an
explicit grant under `ask`, an automatic rejection under `deny`, or a once-only
approval under `allow`. Automatic approval never saves a rule or directory grant.

Rejecting a Copilot tool request sends the model an explicit reason and states
that the operation was not executed. The model can continue with permitted work
or explain the blocker, but must not retry or bypass the denial. Saved-policy
denials, approval-mode denials, timeouts, and user rejections retain distinct reasons.
In reject-all approval mode (`/deny`), rejection feedback tells the model to continue
autonomously using permitted tools and whitelisted commands without waiting for approval.
Filesystem access labels such as `rx` do not select the approval mode.
Cancel stops the response rather than requesting a continuation. If Copilot ends
after a rejection without a final answer or a Harness control handoff, Harness
reports the exchange as failed with the rejection reason instead of silently completing.
Approval expiry dismisses its popup automatically and sends timeout feedback,
without saving a deny rule. A final response after that denial settles the turn
without scheduling another timeout-driven continuation.

Copilot can read and write its current session's notes directory without approval,
when writes are enabled. Write-off also rejects session-note writes. Harness gets
that directory from the native CLI, so the
location can differ across platforms and installations. The exception does not
include other sessions, Copilot configuration, or symlink targets outside that
directory. Explicit deny rules and protected Harness files still apply.

Harness automatically retries recognized network and model-capacity failures
before any tool or model output, up to five times with delays of 1, 5, 10, 30, and
60 seconds. The status shows the retry count and a countdown, such as
`Working · retry 2/5 · retrying in 5s`. Cancellation remains available while
waiting. If recovery fails, Forge shows the final provider error. With tracing
set to Trace, Forge records every attempt and error. Authentication, quota, tool,
unknown, and potentially partially executed failures do not trigger this retry.

Failed prompt delivery preserves your input and attachments. Your next prompt
includes earlier input only when Forge knows it was never delivered. Your newest
instruction can amend or cancel that earlier context. If delivery is uncertain,
Forge checks provider history before sending again. When acceptance cannot be
confirmed, Forge keeps both prompts saved and displays the failure instead of
risking duplicate execution. This also applies to live steering.

A reported write while writing is off is rejected without a mode-switch popup.
Use `/write` explicitly to enable workspace writes. Plan, Execute, Goal, and task
resumption retain selected filesystem access.

Built-in Harness tools, including LSP tools, are allowed without a tool-approval
popup by default. Saved tool rules still apply. File access, write mode, and
protected-path checks remain enforced. External MCP and provider tools keep their
normal approval behavior.

File-write permission popups offer **Always allow writes in this workspace** for
paths inside the current workspace. The choice persists a workspace-specific rule,
not filesystem-wide access. Explicit denials and protected files remain restricted.
Directory grants for writes use the containing Git worktree root when available,
including linked worktrees and nested repositories. Non-Git paths use the nearest
existing directory. The popup shows the selected root before approval.
Permission popups, like every Forge picker, close with `q` or `Esc` without
approving or rejecting the tool; the request stays pending until you decide it.

Long prompts and responses fold to a three-line preview. Prompts fold when sent, and the
two latest prior responses stay open during the current exchange. Submitting another prompt folds
the third-to-last response. Thinking and tool sections keep their usual folding behavior.
Use the normal expansion command on
the caret or message body to show or hide the complete text.
Submitting another prompt preserves Markdown formatting in earlier responses while the new
exchange starts.

Copilot can finish processing after commentary without a separately labeled final
answer. In that case, Forge also displays the last commentary as a final response,
with the same text in both places. The original commentary keeps its tools.
An explicitly empty final answer does not produce a replacement response.

`Running` tool groups show only the latest call and its four-row output preview.
When a group changes to `Ran`, it folds automatically. Expanding that completed group
shows every call in the group, with individual output available on expansion.

Thoughts with activity start collapsed and preserve the assistant's text in their heading.
Expanding a thought shows its events first, then its tool summary, then its file changes,
one indentation level deeper. Thoughts without activity keep the `○` marker. Your explicit
expansion choice survives incoming activity. Tools and file writes accumulate into two entries,
even when they alternate. **Reported edits** counts operations and preserves their order,
individual patches, pending status, and failure diagnostics. Repeated writes to one file
remain separate operations. A new assistant thought or question branch starts a new section.
Steering stays inside the current commentary and divides its activity into ordered groups.
Subsequent tools and edits remain under that commentary until the assistant starts another
one. Question answers and clarifications still start separate branches.

Each exchange also displays **Workspace changes** and a separate **Checks** summary outside
its Thought fold. Workspace changes compares the saved starting and final checkpoints,
including captured formatter, command, child-agent, and external edits. Its file counts,
line counts, hunks, and raw-diff action all use that saved comparison. Opening a file
navigates to the live file, whose contents may have changed since capture.
The raw diff opens in a separate read-only tab when the transcript window is
protected. Press `q` in that diff to return to the transcript.

When the provider reports a checklist, the exchange shows it as **Provider-reported
checklist** after its activity. The heading counts completed entries and names the checklist.
Each entry shows `[ ]` pending, `[>]` in progress, `[x]` completed, or `[-]` superseded,
exactly as the provider reported it. This is the provider's own progress note: completing
every entry does not complete a plan, goal, or check, and an interrupted turn leaves
unfinished entries unchecked. Exchanges without a reported checklist show no section.

A file created and deleted within the captured interval is absent from the workspace result
but remains in Reported edits. An empty saved comparison produces no workspace-change row.
Missing checkpoint evidence displays **Workspace changes unavailable**, including non-Git
workspaces. An active exchange without a saved comparison displays a pending indication.
During recapture, a nonempty previous comparison remains visible as **previous capture**.
Failed recapture retains that comparison with the
failure reason. Checkpoints remain available through `/undo`.

Checkpoints capture text files and symlink targets. Sockets, pipes, devices, and
files containing NUL in their first 8 KiB are excluded from comparison and file
restoration. The binary check runs during the existing read, without a separate
file scan.

`/fork [name]` opens a child timeline with a snapshot of the source's visible history
above its fork marker. New child messages append below that marker. Source updates do
not change the copied history, and pending questions, tasks, and agents do not resume
in the child. Inherited question and plan controls are inactive.

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeHarness` | None | Open the Harness |
| `:ForgeHarnessNew` | None | Create a fresh Harness session |
| `:ForgeHarnessLog` | Optional logging level, `open`, or `clear` | Open the active session's log, select its recording threshold, or clear its files |
| `:ForgePermissions` | None | Edit the Harness permissions document |

`ForgeHarnessLog` requires an open Harness session. Check-worker and tool-export
cleanup diagnostics follow the same `/log` setting instead of printing to process
streams. Operation failures remain visible when logging is off.

Inside Harness, `/sessions`
opens durable session search and `/undo` opens the interaction list directly.
In session search, Enter with **Open: Current Tab** shows the chosen session in the current
tab, or focuses the tab that already shows it; `Tab` switches to **Open: New Tab**. `Ctrl-J`
deletes the selected saved session and closes every tab that shows it.
After selecting an interaction, choose **Conversation only** to keep current files,
or **Conversation and files** to restore its checkpoint too. Conversation-only
rollback works without a file checkpoint. Confirming conversation rollback collects
active execution before changing history and leaves the task paused. An unfinished
restore requires recovery first. Rollback rewinds the provider's own conversation, so it
requires a provider that supports native rewind (Codex, Copilot, and Claude). With
Antigravity, `/undo` reports that conversation rollback is unsupported and changes nothing.
Claude can rewind only to the end of an earlier interaction, not before the first one.

Live status occupies a non-focusable split immediately above the composer. It stays visible
while the transcript scrolls through history, and disappears when no work, pending
message, or local notice remains. The split keeps **Working** visible while handing
off to the next queued message. Check progress, queued messages, pending steering,
and local notices share the status split. Paused queues remain visible.
Normal window cycling skips status. Its viewport stays fixed at the first row,
including during mouse-wheel input. Use `oS` for a scrollable view of full status
content that exceeds the available screen space.
Delivered messages remain in conversation history.
Each entry previews only its first line on one display row, with an ellipsis when
that line exceeds the available width. Editing opens the complete message in the
prompt while live status remains above the composer.
Live tool output uses only the rows it needs, up to four preview rows, without
blank padding. Tools with no output reserve no preview space.
The latest active commentary stays unfolded to expose that preview, including
the initial "Considering initial steps" group. Earlier commentary folds when its
activity settles, and exchange completion collapses the activity. You can reopen
completed commentary explicitly.
Shell-backed tool headings show the underlying command rather than the shell tool
wrapper, both while running and after completion. Expanding the call retains its
full arguments. Native tools such as `rg` keep their structured call headings.
The live status line shows the current phase, elapsed time, and interrupt key.
Thought headers show successful/total tools and append changed-file counts when
nonzero. Repeated successful edits to one file count once. Individual tool errors
and compaction details remain available in the expanded timeline.
Enable **Thoughts** in `/config` under **Display** to show the latest thought below it in
italics, aligned with the phase label and wrapped to the available width. The option
defaults to Off, persists across sessions, and applies without interrupting an
active response. Thoughts never appear as commentary in the timeline. Only the latest
readable reasoning summary appears in status when this option is On.
Reasoning completion updates the existing summary without restarting Thinking,
even when its snapshot arrives after response text. Received-byte progress and
tool-argument streaming do not count as reasoning text. Copilot remains active
until its session becomes idle, including native background work.
While the prompt buffer is focused, new activity in the current live exchange
automatically scrolls the transcript to its end, exposing running tools and progress.
While the transcript is focused, updates preserve the cursor and reading position,
including when the thought preview changes. Active selections retain both endpoints and their selection type.
If selected text disappears, each endpoint moves to the nearest surviving position.
Submitting a prompt positions the transcript at its tail once.
When an exchange finishes, Harness reveals it if focus is outside that transcript.
If the prompt and response fit in the visible transcript window, Harness aligns
the exchange to the bottom. Otherwise, it places the prompt at the top so you can
read the response from its beginning.
This decision uses current focus, not previous scrolling or cursor movement.
Focused transcripts and folded responses keep their reading position.
Skipped reveals do not run later when focus changes.
Codex and Copilot request concise reasoning summaries when the selected model supports them.
Claude Code always streams summarized thinking. Antigravity retains provider-defined summary detail
through its ACP integration.
Assistant commentary marked with an open circle is separate from the reasoning-summary setting.
`oS` opens the complete current status in a readable view, including content taller than
the status split. Press `q` there to return. Configure or disable that binding through
`keymaps.harness.status_details`.

Explicit steering sends the current composer text to the active response while
preserving queued follow-ups. If the active turn ends before dispatch, the message
keeps its priority for the next eligible interaction. Harness never automatically
resends a message whose delivery is uncertain.
Consecutive messages sent to an active steering queue are handed off in order
without waiting for earlier acknowledgments. Each keeps its own delivery status;
explicitly queued follow-ups still wait for the active work to finish.
`Ctrl-C` only interrupts active work. It never submits or steers the composer,
including while editing a queued message. Drafts and queued follow-ups remain
unsent while the task is paused. With a child timeline selected, it interrupts
that child without sending the draft.
Interrupting a prompt before the model shows any reply, reasoning, or tool activity
removes that exchange and returns the prompt to the composer; the provider's echo of
your own prompt does not count as activity. After visible activity, the exchange stays
in history as cancelled.
When a turn fails in a way that leaves the provider's state unknown, such as a send
that ends without confirmation, Harness restarts the provider before it runs anything
else. Commands, including `Ctrl-C`, wait until the restart finishes. If the provider
cannot restart, the session shows an error, later commands fail immediately, and the
session must be reopened.
Pending steering appears above queued follow-ups in the status split. Explicitly
queued messages remain visible while execution is paused or idle. Direct submissions
enter exchange history. The lower prompt buffer contains only the draft being edited.
The prompt grows with visual wrapped rows up to its configured maximum height.
Longer drafts scroll through wrapped rows without inserting newlines into the text.
Queued prompts remain visible until accepted. If submission fails before acceptance,
the message and its attachments remain visible in red. `Alt-R` retries the first
failed message without changing the composer draft. For uncertain delivery, the
same shortcut checks provider acceptance before permitting a resend. Recovery entries
retained after a host restart require manual delivery confirmation and are not resent
by this shortcut. Explicitly queued messages do not interrupt the active response or checks.
The prompt inherits ordinary text-buffer editing and navigation mappings. Harness
adds submission, queueing, steering, cancellation, mode, and effort controls there.
Normal mode also supports session commands, including `oe` to reopen a pending
question or approval, `op` for artifacts, `og` for agents, model selection, help,
and closing Harness. These hotkeys also work with Read permissions.
In Insert mode, `Ctrl-J` inserts a newline at the cursor and preserves the draft.
Press `Up` on the first draft line to recall submitted prompts. Repeated `Up` and
`Down` browse history, and `Down` past the newest entry restores your unfinished
draft and attachments. Inside a multiline draft, the arrows move between lines.
`Alt-Up` and `Alt-Down` also browse history. Prompt navigation and `oa` or `Tab`
focus the timeline before navigating or toggling activity. Timeline row activation
and gutter selection remain in the upper pane.

In the timeline, press `W` to select whole lines from the left gutter, including
markers and indentation. Extend the selection, then press `<Space>l` to copy the
displayed lines and their gutters to the clipboard for pasting into a prompt.

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
Messages still being submitted cannot be edited until the host acknowledges them.
The same applies while a previous edit is removing the message from the queue.
If delivery wins the race with an edit reservation, submitting or navigating the
unavailable edit exits editing and keeps nonempty text as a normal draft. Submit
again to send that draft; recovery never sends it automatically.

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

`/config` includes an **LSP** toggle under **Tools**. It enables the
[LSP tools](lsp-tools.md) for code exploration and permission-checked symbol,
file, and directory renames. It also adds LSP guidance after the
Harness system prompt. Enabling the setting does not start language servers or
change interactive plan navigation.

`/config` opens five tabs: **Models**, **Workflow**, **Permissions**, **Tools**, and
**System**. The window occupies the bottom 80% of the host at full width.
Tab/Shift-Tab cycle through tabs; Up/Down select settings without wrapping.
Enter activates a setting, Space also toggles booleans, and Left/Right cycle choices.
Press `?` for the full key help. A fixed two-line area describes the selected setting.
Category and field selection survive resizing, updates, and returns from child editors.

**Models** contains Main, Ask, and Background model sections. Main and Background each
start with a **Provider** field that decides which models the section lists. Activating
Main's Provider switches the chat's provider by starting a new chat or resuming one, the
same flow as `/provider`. Background's Provider cycles with Left/Right among providers that
support Background sessions and leaves the chat unchanged. Main and Background
include their own Compact toggle and threshold: 64k, 128k, 256k, or 512k. Once the provider
reports the main model's context window, the Main threshold offers only values below it.
Ask lists the current session's models. A Background settings failure
disables only the Background rows.
Ask serves Plan Review questions created with `A` and defaults to the main planning
model. Turn on **Override** to select a separate model, thinking level, and speed.
Answers run in a forked session using the same provider and appear below their
review questions without entering the main timeline.

**Permissions** groups read/write scopes and extra directories under **Access**.
Its **Tools** subsection contains Mode (Ask, Deny, or Approve), Approval timeout,
and Tool / shell timeout.
Ask is the default; Mode changes the same current-session policy as `/ask`, `/deny`,
and `/allow`. The status suffixes are `dx` for Deny and `ax` for Approve; `rx`
is a read-access label. **Checks** contains Allow editing, also controlled by
`/checks edit on/off`.
The **Sandbox** subsection contains the sandbox toggle and Windows sandbox mode.
Their descriptions follow the selected value, explaining isolation, setup requirements,
and provider limits. Forge currently exposes native OS sandboxing only for Codex.
Windows modes explain Elevated setup, Unelevated restrictions, and MXC compatibility
with Elevated fallback. Both controls remain editable and are gray for non-Codex providers. Windows mode is also gray while Sandbox is off. Saved preferences remain available after restarting or switching to Codex; runtime access still follows provider capabilities. See the
[official Windows sandbox documentation](https://learn.chatgpt.com/docs/windows/windows-sandbox)
for platform requirements.
Extra-directory settings open a text buffer with one directory per line. Save with
`:write` or Ctrl-S and press `q` to return; `R` reloads the saved list. Blank lines
are ignored, duplicate paths collapse to one grant, and every nonblank line must
name an existing directory. Failed saves retain the draft. Read and write lists
are independent, and neither changes sandbox settings.
After saving a question, its answer block shows
`Generating response · <model> · Thinking: <effort>`. Leaving and returning to the
block preserves the pending status and the eventual answer. Closing PlanReview
while an answer is running keeps the review open and explains the pending operation.
If the buffer is forcibly closed, completed answers remain available after reopening.
Planning retains the session's current tool-approval mode, configured under
**Permissions → Tools → Mode**.
Automatic plan acceptance belongs to the active task and is controlled only by
`/plan accept on/off`; it has no global configuration setting.
New sessions start in Ask. Plan → Execute retains the current session and its
tool-approval mode unless plan acceptance explicitly selects a different mode.
Questions have no timeout. By default, the agent receives question IDs immediately
and can keep working while you answer. For an important decision that blocks
progress, the agent can request `wait: true`; the question appears as **Required**
and its tool call waits for the answers. Open questions with
`oe` or `/questions`; Tab and Shift-Tab switch questions. Enter submits one answer
immediately, and **Ask…** requests clarification without answering. Closing the
picker retains pending questions and drafts. Pending questions branch beneath the
status text, with previews capped at two display lines and the answer hotkey beside
interrupt. The picker retains the full text. Optional questions remain answerable
after work finishes or chat execution is cancelled; late answers do not restart the
cancelled work. The agent can retract a pending question with `question_withdraw`
and a reason. `H` in the picker includes answered history and allows explicit corrections.
**Tools** contains toggles for Subagents, LSP, and Session history.
Both timeout settings show one value, such as `10m`. Enter opens an
inline editor: Up/Down adjust the duration, Enter saves, and Esc cancels. Values
step through `15s`, `30s`, `45s`, `60s`, then whole minutes from `2m` to `60m`.
The endpoints do not wrap.
**System** contains Thoughts under Display and Logging under Debug.
Logging opens the same picker as `/log`: None, Error, Warn, Info, and Trace.
Info is the default and records major IPC and provider lifecycle operations plus
warnings and errors. Trace adds verbose diagnostics and Harness performance
logging. `/log none|error|warn|info|trace` sets a level directly; `/log open` opens
the current session's JSONL log, and `/log clear` clears it. Saved choices persist
across restarts and override the Lua `log_level` default.
Use `/checks edit on/off` to control check-authoring permission.
Unsupported controls explain their restriction, and Windows sandbox options appear
only where applicable. Forge has no sign-in flow: sign in through each provider's own
CLI, such as running `claude` and logging in. A signed-out provider reports that its CLI
is not signed in.

The model picker, configuration page, and winbar name each model by the
provider's display name, such as `Opus 5.5`, when the provider publishes one.
Claude Code advertises aliases such as `opus` whose display name carries the
concrete version, and `/model` still accepts and completes the alias. Models
without a display name keep their identifier. The winbar shows the identifier
until the provider session reports its model catalog. Before a Claude Code or
Antigravity session's first message, `/model` starts a short-lived adapter to
read the catalog, so the first listing takes a few seconds.

The model picker's **Context** value uses the provider's explicit context limit.
For the active model and tier, it uses the live session limit shown in the winbar.
Copilot tiers without an explicit numeric limit display their identifier, such as
`long_context`, until the provider reports their active capacity. Billing thresholds
and output allowances do not increase the displayed context size.

Tool / shell timeout defaults to **10 minutes** for each invocation. It covers
Harness tools, provider tools, shell commands, child-agent tools, and background
shell commands. Output and progress do not extend a deadline. Expiry records an
error and stops execution; it does not retry the command. Check commands use the
same policy and stop the remaining check run on expiry. Changing this setting
applies to subsequent provider turns and newly started check commands. A running
invocation retains its captured deadline.

Approval timeouts run independently of the picker. An idle session has no periodic
timeout check. Changing a timeout affects future approvals without
extending an existing deadline. Ordinary submitted messages update the display
immediately, then Harness starts, steers, or queues them according to the actual
execution state. Queue editing remains local while an asynchronous reservation
protects the message from delivery. Failed operations retain drafts and show their
diagnostic separately from task status and review actions.

Question-picker answers and clarifications use the same durable delivery path.
Saving them does not wait for provider execution. Clarification leaves the question
pending and runs a read-only discussion; required work resumes only after every
required question resolves and its answer has been delivered.

Queued messages and operation receipts persist in the session's input journal.
Restart recovery suspends unfinished delivery and reconciles provider evidence;
it does not automatically duplicate an uncertain send.

`/checks` opens project checks. `/checks edit on` and
`/checks edit off` toggle check-editing permission without sending a prompt.
Request configuration changes in chat after enabling editing. The `harness-checks`
skill supplies setup guidance. Bare `/checks edit` is not a command.
The Harness winbar appends `ce` to the permissions code while editing is enabled.
See [Project checks](checks.md) for
publication approval, declared inputs, and repository callbacks.

The winbar also shows the selected workflow and phase: `plan · draft`,
`plan · review`, `exec · implement`, `exec · check`, `exec · resolve`,
`exec · verify`, or `goal · work`. Pausing preserves that label, such as
`exec · check · paused`. Resuming Check runs Harness checks locally rather than
starting a model response. Plan acceptance keeps the same task identity as it
enters Execute, and planning never enters Verify.

Automatic continuation stops after five consecutive model turns without
observable progress or the configured total allowance, which defaults to 20 turns.
Reads and phase changes do not replenish the allowance. Explicit task resume
starts a new allowance while retaining the phase, accepted artifact, and findings.
After a host restart, unfinished tasks require explicit resume before work starts.
Accepting a paused plan with `oY` keeps the pause, so its tool-approval question stays
hidden. A notice asks for `/task resume`, which presents the question.

PlanReview shows declaration changes with nearby context lines and enclosing
declaration boundaries, using the regular diff grouping rules. Nearby method
changes share their enclosing `impl` context. Distant changes appear in separate
groups without repeating that `impl` header. Explicit reference navigation can
reveal nearby context around a declaration outside the displayed changes.
Regular diffs and PlanReview render enclosing declarations as syntax-highlighted
source lines. An indented `...` marks omitted source between visible regions.
New functions and methods contain an `Algorithm` block in academic pseudocode.
Algorithm blocks highlight control keywords, assignment and comparison operators, literals,
and identifiers. Plans contain no reference sections, so Algorithm calls, including
abstract operations such as `delta(time)`, read as plain identifiers. Variables and named fields are white. Only `#` comments use comment coloring.
Write operations as calls such as `spawn_square(...)`. Prose statements, prose expressions,
and adjacent operands without an operator are rejected at submission. Use comma-separated
tuples and lists, and keep each complete statement on one line.
Wrapped rows preserve source indentation after the diff gutter. Constructors retain type highlighting.
Press `.` on an Algorithm parameter name to jump to that parameter in the function signature.
Press it on a qualified name, a capitalized type or constructor, or a member of a typed
parameter to jump to its declaration. Ambiguous matches report the candidate names without
moving the cursor. Jump-back returns to the original Algorithm token.
Unqualified lowercase calls represent abstract operations and have no declaration
navigation target, even when a function of that name exists. They do not require
implementation functions, and a name that resolves to nothing is not an error.
Every declaration a plan introduces, including enum variants and struct fields, needs a
documentation comment (`///` in Rust, `/** */` in TypeScript) directly above it.
Assign with `←` or `<-`, compare with `=`, `≠`, `is`, or `is not` (as in `target is none`),
compute remainders with `%` or `mod`, and loop with
`for each item in items` or `for item in items`.
Each Algorithm and Change heading in PlanReview folds its block, and the caret before the heading
shows its state: `▾` open, `▸` closed. Every block starts open. Folding changes only the display.
Write constructions explicitly as `Projectile(damage = 1)`, `Velocity(direction)` for a tuple
struct or variant, or `CombatEntity()` for a marker. Keep construction separate from the
operation that queues or inserts it. Name the fields and types a function uses inside its
Algorithm or Change; plans have no Calls, Constructs, or Accesses sections to fill in.
Submission checks statement syntax and indentation, including branch and loop structure, and
reports every violation in one response, each with the callable, the Algorithm line, and the
rejected text. Drafts can remain incomplete while edited.
Modified functions use `Change` for behavioral deltas in one or two succinct
sentences. Expand a function to read its behavior block.
Collapsed signatures omit the `[changed]` suffix.
Old and new function versions expand independently. Collapsing an old function
keeps added declarations outside its body visible.

Flows appear as the agent wrote them in `plan:flows.md`: one heading per
independently triggered flow, a one-to-two-line description, and a fenced
Mermaid diagram. A `sequenceDiagram` shows a flow that crosses ownership
boundaries, a `flowchart` shows branching inside one owner, and a
`stateDiagram-v2` shows a lifecycle. PlanReview shows the Mermaid source as
text and does not link names inside it to declarations.

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
Accepting with inline comments asks the agent to revise the plan first. The revised
submission returns for review unless automatic acceptance is enabled for the task.
Each comment includes its source context and any question-and-answer thread it
belongs to. Question-only threads are not sent as change requests. Feedback and revision
work remain in the same exchange, labeled as requested changes rather than a new plan.
Every new Plan task starts with automatic acceptance off.
Use `/plan accept on` or `/plan accept off` at any time during a Plan or Execute task
to change automatic acceptance without interrupting the agent. The winbar shows
`Auto-accept: on/off`. Successful full submissions check this task setting; drafts and
failed submissions never authorize implementation. The setting survives submissions,
pauses, and restarts until changed or the task ends. Requesting changes leaves it
unchanged, as does accepting inline feedback. Turn it off before resubmission when
another review is required.
Save pending annotations before a definition jump that changes the projection.
Test names use function highlighting and omit the leading `tests::` in the file
list. Navigation and test selection retain the full qualified name.
Names absent from the plan fall back to exact workspace symbol lookup through
running project language servers. Removed declarations do not fall back to old
source definitions.

Inside PlanReview, `ov` toggles All/Public declaration visibility. Plan review
supports Semantic presentation only and does not bind `ox`. Configure visibility
through `keymaps.diff.toggle_visibility`.

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
| `:ForgeStartupLog [harness]` | Optional `harness` | Open host diagnostics, or the current editor's detailed Harness startup timings |
| `:ForgeGitConfigCacheReset` | None | Clear the cached Git configuration location |
| `:ForgeDebugBackgroundTimeline [session_id]` | Optional saved Background session ID | Inspect generation history, provider status, failures, and compaction |
| `:ForgeDebugCommitPromptPreview [comparison]` | Optional `status` (default), `head`, `staged`, or `latest_commit` | Inspect the exact system instructions and compact commit context without calling a model |

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

PR descriptions use Harness with the same Background provider, model, thinking, and
speed settings in `/config` as commits. PR descriptions and commits share one persistent, branch-aware Background
session and its compaction policy. Each request supplies its own output instructions.
Both appear in `:ForgeDebugBackgroundTimeline`. Provider failures stop PR creation and
appear in a notification.
