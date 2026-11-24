# Forge commands

These commands are registered by `nvim/lua/plugins/forge.lua`. This reference
describes the current local plugin configuration. The source registration defines
argument parsing and completion. Contributor documentation starts at
[Contributing to Forge](../../contributing/forge/README.md).

[Local text correction](text-correction.md) documents the optional local spelling service,
its eligible prose regions, and immediate Backspace rejection. It does not add a slash
command or a provider request.

## Instruction reads

Claude, Copilot, and Codex conversations show `Instruction · <path>` rows beside skill reads. Selecting a row opens its full source path. The first exchange includes confirmed startup instructions; later rows report native loads or successful explicit instruction-file reads. Conditional Copilot rules appear when their bodies load, and repeated reports of the same path within a turn produce one row. Codex's unreported internal reloads and Antigravity instruction tracking are not displayed.

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
history tools. Configuration inspection and the `forge-sessions` skill remain
available when history is off. Session-specific reads default to the current
session. Unfiltered listing/search considers all accessible sessions.
`harness_session_exchange_list` lists only the current conversation: exchanges an
`/undo` removed are left out, and a direct read or a search result marks such an
exchange `history: rolled_back`. A task's opening, such as `Goal started: …`, has
the role `harness` rather than `user`, because Harness wrote it.

`:ForgeHistoryClear` stops its Forge host and attempts to delete every version
under `stdpath("data")/forge/harness/state/`. It removes unlocked versions completely:
sessions, Background conversations, transcripts, plans, captured files, journals,
and private provider runtimes. A live owner or locked database preserves that whole
version; the command reports its path and reason while clearing unlocked siblings.
It removes `state/` when empty. Settings, model preferences and pins, permissions,
repository files, and unsent composer text survive. Reopen `:ForgeHarness` afterward
to start a new session. Breaking state versions select a new directory automatically,
so old editors do not prevent new hosts from starting. The one-time old-layout
cleanup described below also belongs to this command.

Antigravity uses the bundled ACP server in Antigravity CLI 1.5.13 or newer. Install the CLI on your search path and sign in with `agy acp --login`. The first start with the bundled adapter opens a fresh history for all providers and loads existing persistent preferences. Older hosts can finish with their original history version.

Remove any `harness.providers.antigravity.command` override that launches `agy_acp_server` to use the bundled default. If Neovim cannot find `agy` on its search path, set that override to the absolute path to `agy` followed by `acp` and `--process-per-session`, as separate command entries.

After steering an Antigravity response, separate replies may first stream together. When the turn settles, Forge separates them if the CLI's saved transcript confirms the boundary. If you stop the later reply, a confirmed earlier reply stays visible.

Antigravity's `/skills` and `/mcp` show the current resource selection. Press `e` or `d` to enable or disable the selected resource for future conversations in its global or workspace scope. Forge applies those defaults the next time it opens a native binding.

The first startup of a recognized retired layout asks you to run `:ForgeHistoryClear`. The command clears retired history after its owners release it. It keeps a compatible `settings.db`, permission rules, and subagent profiles in place. It moves incompatible settings databases and older provider selection and logging files to `retired-settings/` inside the Harness data directory for manual recovery. If the current settings database needs to be created, Forge starts with defaults for the archived choices. The next Harness start reports the cutover once, in one editor. Later clears preserve `retired-settings/` along with active settings. An unknown layout or corrupt canonical database reports an error.

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

`:ForgeStatus` opens in a new tab, preserving the invoking window and its buffer.
If its buffer is already displayed, the command focuses that tab.

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

Opening a file from ForgeStatus with `.`, `o`, or `<CR>` uses the Status window by
default, including historical source for deleted files. Set
`status_open_target = "tab"` in `require("forge").setup({ ... })` to open files in
a new tab. The default is `status_open_target = "buffer"`.

In **Ignored changes**, ForgeStatus labels untracked files `New` and tracked files
deleted from the worktree `Deleted`.

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

Commit generation uses a fresh temporary conversation through the selected provider.
It supplies no Harness tools and denies permission requests; native tools and hooks
are disabled where supported. It creates no saved Harness conversation and never
resumes a background thread. Each request includes the current diff and the last ten complete
Git commit messages as style examples.

`/config` and `:ForgeConfig` provide a **Commit model** section in **Models** with provider,
model, thinking, and speed choices, independent of Main, Ask, and Background. These settings
also supply one-shot PR descriptions. Existing Background preferences seed the initial
commit selection; saving a Commit choice stores it independently. Closing the last
consumer cancels unfinished commit generation.

The **Background model** settings and service remain available for future persistent
workflows. Commit messages and PR descriptions do not use them.

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

`Up` and `Down` move through suggestions while the completion menu is open.
With the menu closed, `Up` starts browsing prompt history from an empty composer.
Once browsing, `Up` moves to older prompts and `Down` moves toward the empty composer.
Any other key or leaving the composer ends browsing and keeps the selected prompt as a draft.

Type `$` anywhere in the composer to complete an enabled, user-invocable skill.
Forge sends a `$skill` at the start of the prompt as a structured skill invocation.
An inline `$skill` travels as prompt text.

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
model change immediately through the configuration coordinator. A `/model` that names the model, and
effort if given, the session already uses clears the composer and says that nothing changed.
`/model` names its model by id, or by the display name the winbar and the model picker show,
compared without regard to case, so `/model Haiku 5.5` selects `haiku`; a thinking level may
follow the name. Harness resolves the name against the provider's model catalog before it sends
anything, so a name that matches no model, or several, is refused with a notice listing the models
it could have meant, the command stays in the composer, and the winbar never shows the refused
selection.

New interactive sessions start with full reads, workspace writes, and `ask` approval.
Reopening a session preserves its filesystem access and tool approval independently.
The winbar separates every field with a small gray `·`. One gray access/approval code
appends `dx` for Deny or `ax` for Allow to the filesystem abbreviation. Ask adds no suffix:
`rxwx` means full reads/writes with Ask, `rxwxdx` with Deny, and `rxwxax` with Allow.
Enabled sandbox status precedes the model.
Opening Harness waits for the session snapshot before showing permission and model
fields. New sessions reuse the last confirmed default model for that workspace and
provider while refreshing the catalog in the background. Without a cached model,
the field shows `resolving model` until the provider responds. A model, thinking,
context, or speed change made in a session becomes the default of that session's
workspace and provider once Harness applies it; it never changes the defaults of
other workspaces. Editing the global defaults with `:ForgeConfig` outside a session
is newer than every earlier workspace choice, so the next new session in any
workspace starts from it until a session there confirms another choice.
The model field reads `<speed icon> <model> <thinking>`: Fast uses a gray outlined
lightning glyph (`󱐌`), Ultrafast a filled glyph (`󱐋`) in YOLO's neon red, and Standard no icon.
Thinking names the level the provider uses. A `default` selection sends no level, so
it reads as the provider's advertised default level, or as `default` when the provider
names none; the `/model` picker shows the same value for the current model and offers
`default` as a choice for such models.
Model, thinking, or speed changes waiting for existing work share one trailing `*`
after thinking, and the field shows the values they select. This covers a `/model`
entry queued with Ctrl-Q, from the moment it is queued until its hand-off applies it,
and a change made with the effort keys or a picker during a turn, which restarts the
turn on the new settings, until that restart applies it. A change made while the turn's
prompt is still being sent waits until the provider has started the turn, then restarts it
the same way. When children run or the
turn waits for them, the restart first stops the children, which end `Cancelled` as
after Ctrl-C, and the restarted turn may start children again. An idle or paused change shows
its value at once and no marker, though the host takes a second or two to check it
against the provider's model list before it applies. The marker disappears when all
selected values are applied, and a rejected change leaves the field with the applied
values.
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

`/ask`, `/deny`, and `/allow` take effect at once, even during a turn: they never stop or
restart the response. A request already waiting for approval is decided as a new one would
be: `/allow` approves it once, `/deny` rejects it with the reject-all reason, and `/ask`
leaves it waiting. Access changes still restart the turn.

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

Expand **An approval timed out** to inspect the original request, action details,
complete commands and reported tool arguments, and the automatic timeout outcome.
The entry distinguishes expiry from a user rejection.

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

Forge suppresses competing native question, agent, plan, and task tools where the
provider supports suppression. File, shell, and web tools remain available.
Antigravity retains its native task, question, messaging, and subagent tools. Its
Forge profile excludes the CLI scheduler. Codex's separate `send_message_to_user_async`
tool may still appear when its model catalog enables it.

File-write permission popups offer **Always allow writes in this workspace** for
paths inside the current workspace. The choice persists a workspace-specific rule,
not filesystem-wide access. Explicit denials and protected files remain restricted.
Directory grants for writes use the containing Git worktree root when available,
including linked worktrees and nested repositories. Non-Git paths use the nearest
existing directory. The popup shows the selected root before approval.
Permission popups, like every Forge picker, close with `q` or `Esc` without
approving or rejecting the tool; the request stays pending until you decide it.
A permission popup opens with no decision selected, so an Enter typed into the
composer just as the popup takes focus decides nothing; choose a decision with its
key or the arrow keys first. A permission or question popup never opens by itself while
the editor is in Insert, Replace, or Command-line mode, in any buffer: the request waits as if
you had closed it with `q`, a notice such as **Approval waiting · oe** says so until the popup opens or the request is
answered or cancelled, and the popup
opens when you leave that mode with the session in the current tab, or when you press `oe`.
Answering or closing a popup that waited this way returns you to the window and mode you were
in when the request arrived, so you continue typing in the composer where you stopped. Once you
submit the last answer, the question popup closes at once: keys typed right after the submit key
land in that window, not in the closing popup. A
question opens by itself, like a permission request, when focus is in the session's transcript
or composer. Every such popup ignores keys, except `q`, `Esc`, and `Ctrl-C`, for about a third
of a second after it opens and until you stop typing for that long, and its footer reads
**Waiting for typing to stop** meanwhile. When such a popup closes without your decision, as
on expiry, keys pressed in the next third of a second are ignored too, rather than landing in
the composer. Keys that a popup does not map do nothing in it, including keys you mapped
globally, such as a `Y` mapped to paste. The status row, the transcript, and a popup's
countdown keep updating while you sit at a `:` prompt. When another picker replaces a pending permission
popup, the popup returns once that picker closes.

Rust code fences in the Harness transcript display four-space indentation levels
as two spaces. Copying the code preserves its source indentation.

Long prompts and responses fold to a three-line preview. Prompts fold when sent, and the
two latest prior responses stay open during the current exchange. Submitting another prompt folds
the third-to-last response. Thinking and tool sections keep their usual folding behavior.
Use the normal expansion command on
the caret or message body to show or hide the complete text.
Submitting another prompt preserves Markdown formatting in earlier responses while the new
exchange starts.

An interrupted response disappears from the transcript when Forge records its interruption.
Its text remains in session history. Completed responses remain visible, including those
completed before the provider consumes a steering message.

Copilot can finish processing after commentary without a separately labeled final
answer. In that case, Forge also displays the last commentary as a final response,
with the same text in both places. The original commentary keeps its tools.
An explicitly empty final answer does not produce a replacement response.

`Tools · successful/total` groups stay folded while calls run. Only a denied count
appears in parentheses before the duration, when nonzero; failed, cancelled, and
interrupted calls lower the successful count without a count of their own. Expand a
group to see its calls, then expand an individual call to read its output. Each call's
bullet shows its outcome: green for a successful or running call, red for a failed one,
orange for a denied one, and gray for one that was cancelled or interrupted.
Completion folds the group again.
The group duration counts overlapping calls once and excludes gaps between calls.
Individual rows show each call's duration, so their sum can exceed the group duration.

Loaded skills appear as `≡ Skill · name`, aligned with commentary rather than nested beneath its tool group; opening a skill entry opens its instruction file.

The live check status shows a single selected test by name on a child row beneath its check. Multiple selected tests show a count; full paths and IDs remain available in check details.

Child agents appear in three places. Each spawn adds a `◇ Spawned name · provider · model` row to the thought that started it. While a child runs, the status area shows its row with elapsed time, name, model, tool summary, and newest reasoning summary; a child that reports no reasoning summary leaves that column empty. Once children finish, an **Agents** summary beside **Checks** lists each finished child with its outcome, tool counts, and token usage. Enter on a spawn row or a summary row opens that child's own transcript, also while running children keep changing the transcript.

A child's transcript names the child in its winbar: the child's own provider, model, and thinking level, then its name, since its profile can choose them independently of the session. A child's edits belong to the exchange that started it, which shows them under its own **Edits**, so a child's transcript shows no Edits row.

The agents picker (`og` or `/agents`) lists running children, then finished ones, each group in the order of the children's numbers (`-2` before `-10`). A child's duration counts the time its task ran, excluding time spent starting and waiting for your approval, so the picker, the status row, the summary row, and the child's heading show the same number, and the picker shows the status row's tool count. A child you ran again with a follow-up also shows the tool count of all its runs, as `… across 2 runs`. `Ctrl-X` interrupts the selected running child at once, even while an earlier command to a child is still waiting: its row reads `Cancelling` until the child has stopped. If the interrupt cannot be carried out, the row says why after `Interrupt failed:`, and a child that finished before the interrupt reached it reads `Finished before the interrupt reached it`. Either note describes the child as it was then, so it leaves the row when the child's state changes, for example when a follow-up starts it again, or after three seconds. `Ctrl-A` interrupts every running child the same way. A parent waiting for its children receives each child you cancelled as a cancelled result and continues, whether you cancelled one child or all of them, so it can report what it has or start new children; only a child the parent's model interrupted itself does not wake it. A message sent with `Ctrl-G` waits in the child's inbox until its next task, because a running child cannot take it into its current turn; while it waits, the child's status row ends with `1 message waiting`. Each exchange's Agents summary lists every child once, with its latest run in that exchange.

`/agents on` and `/agents off` take effect at once: the picker's title shows the new setting, and a model that tries to start a child after `/agents off` is refused. Changing the setting while children run stops nothing; the children finish and report to the parent, and the model's own tool list changes when its next turn starts.

`/agents spawn` lets you choose a profile, access mode, and task. Use `/agents spawn --read <profile> <text>` for an inspection or `--write` to require write access from the session and profile. The model can make the same choice with the `access` field in `agent_spawn`. Omit the choice to use the profile's default. `/agents edit` saves each profile's Access setting as `inherit`, `read`, or `write`.

Thoughts with activity start collapsed and preserve the assistant's text in their heading.
Tool rows and tool summaries show milliseconds below one second. From one second, thought and tool durations show completed whole seconds. From one minute, they show tenths of a minute, such as `2.5m` for 150 seconds. Longer durations use hours or days.
Expanding a thought shows its events first, then its tool summary, then its file changes,
one indentation level deeper. Thoughts without activity keep the `○` marker. Your explicit
expansion choice survives incoming activity. Tools and file writes accumulate into two entries,
even when they alternate. **Edits** counts operations and preserves their order,
individual patches, pending status, and failure diagnostics. Repeated writes to one file
remain separate operations. A new assistant thought or question branch starts a new section.
Steering stays inside the current commentary and divides its activity into ordered groups.
Subsequent tools and edits remain under that commentary until the assistant starts another
one. Question answers and clarifications still start separate branches.

Each exchange also displays **Edits** and a separate **Checks** summary outside
its Thought fold. The Checks summary stays folded until expanded. The Edits summary
compares the saved starting and final checkpoints,
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
but remains in Edits. An empty saved comparison produces no workspace-change row.
Missing checkpoint evidence displays **Edits unavailable**, including non-Git
workspaces. An exchange that was running when Forge stopped gets its comparison when
Harness next starts, so it shows the edits present at that start, including any made
while Forge was not running. An active exchange without a saved comparison displays a pending indication.
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
in the child. Inherited question and plan controls are inactive. `/fork` and `/btw`
require native fork support, which Antigravity lacks.

`/fork` works while the source is running a turn. The child then stops at the source's
last finished exchange, and its fork marker reads "Forked before the unfinished
exchange of" the source. A source still running its first turn has nothing finished to copy, so its
child starts empty. Two cases are refused with a message and create no session: the
source has child agents running, or the source is itself a fork that has not finished
its first turn. The fork's tab opens only once Harness has created the child, so a refused
`/fork` opens no tab: the message appears in the tab where you typed `/fork`, and the command
stays in the composer in the mode it had. A fork opens in the foreground when you are still
in that tab, and behind the tab you moved to otherwise. Until the provider has copied the
source conversation, the child's status reads **Preparing fork · copying the source
conversation**, and a message sent meanwhile waits under it as `[submitting]` and runs once the
copy finishes. If the provider fork fails, the child keeps its tab, and sending or
retrying a message in it attempts the fork again, showing the same status while it does.

`/btw <question>` opens an aside: a fork in a new tab whose agent is told that you are
asking about the session you came from. The question is sent to the aside at once, and
the origin session keeps running undisturbed. The aside's agent reads the origin's
history, including its running exchange, through the session history tools, which stay
on in an aside regardless of `/config`. It can also send the origin a message with its
`origin_message` tool, for example to pass on a finding. The message appears in the
origin's queue labeled with the aside's name and ID, and runs after the origin's current
turn; the agent can instead steer it into the running turn. The origin's agent receives
it as a message from the aside rather than as your own words, so it acts on the message
only where it agrees with what you asked in the origin, and the message cannot widen the
origin's permissions. An
aside is an ordinary session in every other respect: continue the conversation, give
it unrelated work, or resume it later from `/sessions`. A fork of an aside remains an
aside of the same origin. `/btw` follows the same rules as `/fork` for a running
source.

| Command | Arguments | Behavior |
| --- | --- | --- |
| `:ForgeHarness` | None | Focus the open Harness, or open it in a new tab |
| `:ForgeHarnessNew` | None | Create a fresh Harness session in a new tab |
| `:ForgeConfig` | None | Edit the current session from its panes, or global defaults elsewhere without opening a session |
| `:ForgeHarnessLog` | Optional logging level, `open`, or `clear` | Open the active session's log, select its recording threshold, or clear its files |
| `:ForgePermissions` | None | Edit the Harness permissions document |

`/clear` replaces the session in the current Harness tab after creation succeeds.
`/new` opens a fresh session in a new tab. Both leave the composer in Normal mode when you
submit them; a mode you enter while the host creates the session is kept. The new tab's
composer takes your keys at once, and a submit before the session exists keeps the draft
and reports that Harness is still initializing. A creation the host refuses closes the new
tab and reports the refusal in the tab where you typed the command.

`ForgeHarnessLog` requires an open Harness session. Check-worker and tool-export
cleanup diagnostics follow the same `/log` setting instead of printing to process
streams. Operation failures remain visible when logging is off.

Inside Harness, `/sessions` (or `/s`) opens durable session search in a picker
with a fixed height of 35% of the Harness area. `/undo` opens the interaction list directly.
Session search starts on **Workspace**; switch to **All** with `Tab`, the left/right
arrow keys, or `Ctrl-O`. Each tab retains its own search query, and **All** includes
the workspace path for each session.
Within each scope, **Active** sessions loaded in this Forge host appear first,
including sessions whose editor views are closed. Saved sessions that are not loaded
appear under **Inactive**. Searching keeps this section order. Each scope lists every
session it holds; a list taller than the picker scrolls with the highlighted row, and the
picker's bottom border then reads, for example, `Rows 1–7 of 16`, as every Forge picker
does when its rows do not fit. The picker opens with the
current session highlighted. Choosing a session that another tab already shows moves it into the
current tab and closes its former tab; the session you left stays under **Active**.
The transcript area previews the highlighted session in a layer drawn over the transcript,
below its winbar. The preview shows the session's saved text without the transcript's
rendering, and a session without exchanges says so; the rendered transcript replaces the
preview when you choose the session. The preview ends at the session's latest exchange, on the last row above
the picker. Highlighting the current session removes the preview and shows its own
transcript, and closing the picker leaves that transcript exactly as it was, including its
scroll position and expanded sections. A session that no editor owns, such as
one whose editor was killed mid-turn, previews its unfinished exchange as **Interrupted**
at its last recorded active time, as resuming it will show. A session open in another
editor previews its stored state, with a timer that does not advance.
In session search, Enter shows the chosen session in the current tab. The picker stays
open until that session has rendered, then the session replaces it in one step. Its composer takes
the mode yours had when you opened the picker, also when the session already had a tab and
`/sessions` or `/provider` **Resume session** switches to it. A session of
another provider opens through the same provider switch as `/provider`: Forge restarts
its host on that session's provider, so the switch requires that no Harness tab has
running, queued, or unsent work, and that no other tab is itself switching. A switch that
cannot start reports why, naming the tab and the work, such as "Cannot switch providers
yet: tab 2 (Fix tax rate) has a running turn. Finish or cancel it first.", or "has a turn
waiting for an approval" when that turn stopped at an approval request, and leaves the
picker open. While the switch runs, the status rows read **Switching Harness to** the
provider, and a submit there keeps its draft and says that the switch is still running. The
session you chose replaces the current one in the same tab once it has rendered and takes the
focus, unless you moved to another tab meanwhile. The
switch waits only for the session to open: the provider's model list loads afterwards, so
a provider that is slow to list its models does not fail or delay it. A switch that fails
returns the tab to its own session, and the tab's status rows keep saying why, for example
"Switching to Copilot CLI failed: … Harness still serves Codex CLI.", until the next switch. Unlike
`/provider`, opening a session this way leaves the saved default
provider unchanged, so the next editor start still opens the default provider's session.
Every other Harness tab then says in its status rows which provider the host now serves,
for example "Harness now serves Codex CLI. Submit or run :ForgeHarness to return to
Copilot CLI." Submitting from that tab, or running `:ForgeHarness` in it, switches the host
back to the tab's provider under the same rule, without changing the default; the tab
stays open and keeps its draft for you to submit. Every such submit or `:ForgeHarness`
either starts that switch or shows why it cannot start, also after an earlier attempt was
refused, and `:ForgeHarness` brings back the tab it runs in, even when the switch that
detached it finished while you were in that tab. Until then `/sessions` lists that tab's
session under **Inactive** marked `(open, disconnected)`.
A session of another worktree is history-only; open Harness from that worktree to
resume it. `Ctrl-J` deletes the selected saved session and closes every tab that shows it.
Its row leaves the list at once and the selection moves to the next row; if Harness
refuses the deletion, the row returns with the reason.
The `/undo` list uses up to 30% of the Harness area and opens on the newest exchange
at the bottom. Press Up to move toward earlier exchanges. **Time** shows the
timeline's Thought duration. **Edits** shows the changed file count, added lines,
and removed lines as `2 · +3 −3`. The three counts align across rows, with
green additions and red deletions. It leaves the cell blank for a confirmed empty comparison
and `—` when a completed comparison is unavailable. In the Main timeline,
the transcript follows the selection, highlights its opening row, and expands
folded responses. Closing
the list clears the highlight and restores the previous transcript position. Responses
opened while browsing stay expanded.
Selecting an exchange keeps it and every earlier exchange. Forge rolls back the exchanges
that follow and returns the first removed exchange's prompt to the composer when you wrote it.
The latest exchange is available for browsing. **Before first exchange** rolls back the
whole conversation when the provider supports it.

Choose **Conversation only** to keep current files or **Conversation and files** to restore
the checkpoint from before the first removed exchange. Conversation-only
rollback works without a file checkpoint. The list also includes the exchange of a
running turn or a paused goal. A `/undo` that cannot open its list, because Harness refused it or a rollback is
already in progress, leaves `/undo` in the composer. Confirming conversation rollback collects active execution before changing history and
leaves a task that was working paused, while a plan waiting in review stays in review; choosing **Conversation and files** collects it before showing the
files to restore, so the preview describes settled files, and both options say that they
stop the running turn. Choosing **Cancel** on that preview rolls nothing back, and its
description says that the turn was already stopped. From confirmation until the rollback finishes, the status reads
`Rolling back the conversation` (or `… and files`), and a paused queue keeps its `paused`
heading. A restore that stopped part-way shows `Restore interrupted` and requires recovery
through `/undo` before other rollbacks; a restore that is still running never shows it.
When the files were restored but the provider refused the conversation rewind, the notice
says so and names both ways out: **Continue interrupted restore** retries the conversation,
and **Restore pre-restore files** puts the files back.
Rollback rewinds the
provider's own conversation, so it requires a provider that supports native rewind (Codex,
Copilot, and Claude). With Antigravity, `/undo` reports that conversation rollback is
unsupported and changes nothing. Claude can rewind only to the end of an earlier
interaction, not before the first one, and not to a point before its context was compacted,
because the compaction replaced the conversation before it. An unavailable rollback
destination shows the provider's reason. A failed rollback stays in `:messages` after its
notice fades.
Rolling back an interaction inside a goal leaves the goal paused; rolling back the
interaction that started it, or an earlier one, removes the goal. The transcript drops every
row of the rolled-back interactions, including task rows such as `Goal completed` and
`Goal cleared`. Rolling back the interaction that started a plan also removes the plan from
the artifact count and the artifact picker, whatever phase it reached. A completed rollback returns the rolled-back prompt to the composer, a
`/goal <objective>` or `/plan <request>` command as typed, only when you have not typed there since confirming
it, and it does not move focus out of another window. Rolling back every interaction of a
session that took its name from its first prompt leaves the session unnamed again, so the tab
title no longer shows that prompt, and the next prompt you send names it again; a name you
chose with `/rename` stays.

Live status occupies a non-focusable split immediately above the composer. It stays visible
while the transcript scrolls through history, and disappears when no work, pending
message, or local notice remains. The split keeps **Working** visible while handing
off to the next queued message, and that message keeps its status row until its own
turn shows it in the conversation. A prompt sent while nothing is running goes straight
to the conversation and never appears as a queued message. If Harness cannot start it
within half a second, as during a stop or a provider restart, it appears in status,
marked `[submitting]` until Harness confirms it, and stays there until its turn starts.
Check progress, queued messages, pending steering,
and local notices share the status split. Paused queues remain visible.
Normal window cycling skips status. Its viewport stays fixed at the first row,
including during mouse-wheel input. Use `oS` for a scrollable view of full status
content that exceeds the available screen space.
Delivered messages remain in conversation history.
Each entry previews only its first line on one display row, with an ellipsis when
that line exceeds the available width. Editing opens the complete message in the
prompt while live status remains above the composer.
Live tool groups show a folded count heading until you expand them. Individual
tool output stays behind each tool's own expansion control.
The latest active commentary stays unfolded to expose that count, including
the initial "Considering initial steps" group. Earlier commentary folds when its
activity settles, and exchange completion collapses the activity. You can reopen
completed commentary explicitly.
Shell-backed tool headings show the underlying command rather than the shell tool
wrapper, both while running and after completion. Expanding the call retains its
full arguments. Native tools such as `rg` keep their structured call headings.
The live status line shows the current phase, elapsed time, and interrupt key.
Elapsed time counts active execution, not wall-clock time: the timer stops while an
approval or question popup waits for you and resumes when you answer, so the status line
and the exchange's final duration can read lower than the time since you submitted the
prompt. An exchange cut off because Forge itself stopped reports the active time at its
last recorded event.
When interruption begins, the execution timer stops and status changes to **Stopping**,
which stays unchanged until the stop settles. Running children stay listed beneath it until
each is cancelled. The status area shrinks as those rows disappear and shows at most 12 screen
lines. Open status details with `oS` to see the full list. A turn that runs while its task is
paused, such as a prompt sent to a session whose plan a restart paused, shows its own **Working**
status with its time and interrupt key. **Paused** appears only while nothing runs.
If cleanup outlives its response deadline, **Cleanup timed out - provider state uncertain**
replaces the spinner. Harness retains ownership until cleanup settles; the timeout does not
authorize another turn against uncertain provider state. Copilot force-stops its shared native
runtime if the interrupted prompt does not settle within ten seconds, which also interrupts
other sessions using that runtime.
Interrupting a turn also stops a Copilot shell command that the turn is still waiting on,
including the processes the command started, such as `nu -c "sleep 60sec"` under
Copilot's PowerShell wrapper, and the command leaves the background-terminal count.
Steering does not stop it. A command started in the
background, or one that outlasted its initial wait, keeps running across turns; stop
it with `ot`.
The background-terminal count in the status line follows provider updates, and Harness
re-reads it when a turn settles and when `ot` opens its list, so the count matches the
list `ot` shows.
Thought headers show successful/total tools and append changed-file counts when
nonzero. Repeated successful edits to one file count once. Individual tool errors
and compaction details remain available in the expanded timeline.
`/compact` requires manual compaction support, which Antigravity lacks. It asks the
provider to compact the conversation's context and adds one
`Context compacted` row with the context size before and after the compaction, as the
provider reports them; a provider that reports no counts gets no figures. Both figures measure
the whole context, so on a short conversation they can rise: a summary longer than the few
messages it replaces outweighs them. Sent during a turn,
it stops the turn; the status and the exchange heading then read `Compacting` and count the
compaction's own time while the provider works, and the heading keeps that word until the turn
runs again on the compacted context. The row sits inside that
exchange, above the output of the turn that ran again, and ends with `turn ran again`.
When the provider declines, for example because nothing is left to compact, the row reads
`Context not compacted` followed by the provider's own explanation, such as Copilot's
"Nothing to compact." or Claude Code's "Not enough messages to compact.", and the notice
repeats it. A turn that `/compact` stopped runs again whether the provider compacted, declined,
or failed, so the exchange never rests on `Paused` waiting for a resume that nothing sends.
It runs again with the input it was given, also after a model, thinking-level, or permission
change: a plan revision keeps your change request, saved comments, and edits, and a turn that
was fixing failed checks keeps those checks and their output.
A tool approval still waiting when a stop such as `/compact` ends the turn is cancelled, not
denied: the call reads as interrupted, and the turn that runs again may request it afresh.
Enable **Thoughts** in `/config` under **Display** to show the latest thought beside the
phase and its controls, after a `│` separator. The thought uses the remaining width,
with an ellipsis when it does not fit, and disappears when the controls need the whole
row. The option defaults to On, persists across sessions, and applies without interrupting an
active response. Thoughts never appear as commentary in the timeline. Only the latest
readable reasoning summary appears in status when this option is On.
Copilot supplies these summaries through its concise reasoning stream, independently of
the commentary shown in exchanges.
Reasoning completion updates the existing summary without restarting Thinking,
even when its snapshot arrives after response text. Received-byte progress and
tool-argument streaming do not count as reasoning text. Copilot remains active
until its session becomes idle, including native background work.

The live status heading shows how many checks have finished. When checks are the only
activity, the heading reads `Working` and shows the run's elapsed time. Each check
stays at the same indentation as other activity starts or finishes. Running checks
show progress and recent output, successful checks show the result and elapsed time,
and unsuccessful checks retain their reported cause. Pending questions follow the
checks, with their answer control beside the phase label.

Formatting checks show file counts only when the formatter supplies a total or reports
file outcomes. Otherwise, the running check shows its label and elapsed time.

While the prompt buffer is focused, new activity in the current live exchange
automatically scrolls the transcript to its end, exposing running tools and progress.
While the transcript is focused, updates preserve the cursor and reading position,
including when the thought preview changes. Active selections retain both endpoints and their selection type.
If selected text disappears, each endpoint moves to the nearest surviving position.
The reading position holds while the cursor is outside the transcript's bottom scroll
margin of three rows. A cursor on the last rows of the streaming exchange lies inside
that margin, so Neovim advances the view as rows arrive below it to keep the margin
visible; a cursor on an earlier line leaves the view where it is.
Submitting a prompt positions the transcript at its tail once.
When an exchange finishes, Harness reveals it if focus is outside that transcript.
If the prompt and response fit in the visible transcript window, Harness aligns
the exchange to the bottom. Otherwise, it places the prompt at the top so you can
read the response from its beginning.
This decision uses current focus, not previous scrolling or cursor movement.
An exchange that hands off to a queued message is not revealed, because that
message's turn takes its place at the end of the transcript a moment later.
Focused transcripts and folded responses keep their reading position, within the
same scroll margin.
Skipped reveals do not run later when focus changes. An exchange that finishes while
its tab is hidden is revealed when you show the tab again, by the same rules, unless
focus is then in its transcript.
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
While a tool approval is pending, every message you send, including one sent with
`Ctrl-S`, waits under "Queued follow-up inputs" until the approval is answered or
expires, on every provider; it is neither steered nor sent while the request is open.
An explicitly queued follow-up likewise waits while a question is pending. After an
approval expires, a waiting message steers into the automatic continuation turn when
the provider accepts steering, or runs after it.
A message sent while the parent waits for its children (`Waiting for agents`) waits
under "Queued follow-up inputs", because the parent has no running turn to steer. When
the children report, it steers into the turn that continues the parent with their
results, or, if that turn has already ended, runs once after it as a follow-up. Either
way, on every provider, the parent's report and the message both reach the transcript.
`Ctrl-C` only interrupts active work. It never submits or steers the composer,
including while editing a queued message. Interrupting active work pauses the
queue: drafts and queued follow-ups remain unsent, and the status split shows
`Queued follow-up inputs · paused · <M-r> resume`, with the configured retry key. Press
it to resume the queue in its order. Submitting a new prompt also resumes the queue;
that prompt runs first, ahead of the explicitly queued follow-ups and of any message the
interrupt returned to the queue. A message you sent before pressing `Ctrl-C`, even one
still being sent when the interrupt lands, stays in the paused queue and does not run
by itself. The heading keeps `paused` in every frame until the queue resumes, also while
the stop settles, while such a message still reads `[submitting]`, and while a `/compact`
or a rollback runs. `Ctrl-C` with
nothing running pauses nothing. With a child timeline selected, it interrupts
that child without sending the draft. A message or follow-up sent to a selected child
leaves the composer at once and returns to it only if the child rejects it.
Interrupting a prompt before the model shows any reply, reasoning, or tool activity
removes that exchange and returns the prompt to the composer; the provider's echo of
your own prompt does not count as activity. If the composer already holds newer text,
the prompt stays in the queue as an unsent message that you can retry or edit instead.
The prompt also survives leaving Neovim before the interrupt finishes: Harness keeps it
in the session's queue as an unsent message, and reopening the session shows it there
with the queue paused. After visible activity, the exchange stays in history as
cancelled.
Removing an exchange the provider has already accepted requires a provider that can
drop a turn from its own history. Codex and Copilot can. Claude Code and Antigravity
cannot, so with them the prompt returns only when the interrupt arrives before the
provider accepts it, which is a moment after you submit; once the provider has
accepted the prompt, the exchange stays in history as cancelled even when nothing was
shown.
The exchange that `/goal <objective>` or `/plan <request>` started is never removed this
way: interrupting its first turn before any reply keeps the exchange with its
`Goal started` or `Plan started` row and a `Cancelled` heading, and the task stays paused
until you resume it.
A task command such as `/goal clear`, `/goal pause`, or `/task clear` sent while a turn is
**Stopping** waits for the stop to finish and then applies to the task as the stop left it.
Cancellation remains pending until native work and host settlement finish. Repeated
`Ctrl-C` presses share the pending request. A stop that settles promptly shows only
**Stopping**. If it is still pending after ten seconds, or its response times out, cleanup
continues and a notice directly below the status line identifies the phase still running;
another cancel is not required. The response times out after thirty seconds. If the
provider still has not stopped twenty seconds after that, Harness force-ends the provider
process or connection for this session and the exchange settles as cancelled, so a stop
never needs an editor restart to leave **Stopping**. Work the provider was doing at that
moment has an unknown outcome. `Ctrl-C` with nothing running changes no status.
A reopened view restores that progress from the host snapshot.
When a turn fails in a way that leaves the provider's state unknown, such as a send
that ends without confirmation, Harness restarts the provider before it runs anything
else. Commands, including `Ctrl-C`, wait until the restart finishes. If the provider
cannot restart, the session shows an error, later commands fail immediately, and the
session must be reopened. Commands and prompts that were waiting for the restart fail
at that moment with the same error rather than when you leave Neovim. To reopen the
session, select it in `/sessions` and press `Enter`, even when it is the session you
are viewing. Harness stops the failed session, rebuilds its own provider connection,
and loads it again from storage; the transcript and the queue survive, and the queue
stays paused until you send or resume. Choosing a working session only focuses it.
A reopen cannot revive a provider process that Harness shares between sessions; when
that process itself cannot start, switch the provider with `/provider`, which restarts
Harness.
A prompt Harness refuses before it reaches the provider, such as a bare `/read` or a
typed answer while a question needs its own reply, stays in the queue marked failed with
the reason. No turn runs or fails and the provider is not restarted; edit, retry, or
remove the entry there.
An interrupt that Harness cannot confirm restarts the provider the same way and ends
the interrupted exchange as failed. A prompt interrupted before it was sent stays in
the queue marked failed, and Harness never resends it on its own; retry or edit it
there.
Pending steering appears above queued follow-ups in the status split, under
"Steering active turn". A steer the provider cannot take yet, because it is still
starting the turn, is listed there marked `[waiting]` until the provider takes it.
A steered message stays there after the provider accepts it, until the provider reports that the message joined the running turn; it then moves
into the transcript at that point, before the output that used it. If the exchange ends
without that report, for example after an interruption, the message appears in the
transcript as `Steering (not confirmed read)`. Harness never resends such a message,
because the provider did accept it. Once the exchange settles and its heading folds,
a steered message stays visible directly below that heading. A message still waiting
for the provider when you stop the turn moves to the paused queue, unsent. Explicitly
queued messages remain visible while execution is paused or idle. Direct submissions
enter exchange history. The lower prompt buffer contains only the draft being edited.
The prompt grows with visual wrapped rows up to its configured maximum height.
Longer drafts scroll through wrapped rows without inserting newlines into the text.
Queued prompts remain visible until accepted. A submitted prompt that the session
neither starts nor steers at once, because other work holds it, joins the queued
messages after half a second; one that starts or steers at once never appears there.
If Harness rejects a submission before accepting it, the prompt and its attachments
return to the composer with the reason; when the composer already holds newer text,
they remain visible in red with the reason instead. `Alt-R` retries the first
failed message without changing the composer draft, and resumes a paused queue so
the message is sent. A message Harness marked unsent, such as one interrupted before
the model replied or before provider dispatch, is retried this way, including after a
host restart. With no failed message, `Alt-R` resumes a paused queue. A failed
message pauses the queue, and a prompt you send after it runs as the next exchange
while the failed message stays, unsent, for `Alt-R`. Follow-ups queued behind a failed
message wait for it, and the heading then reads
`Queued follow-up inputs · paused · <M-r> retry`. For uncertain delivery, `Alt-R`
opens **Unconfirmed input**. Choose **Reconcile delivery** to check receipt and retry
only if the provider confirms it never received the message. Choose **Discard queue
entry** to let later prompts proceed. An unconfirmed editor
submission stays in the queue until the host confirms it or you discard it.
During checks, a direct message steers the active turn or starts a new one alongside the run.
Explicitly queued messages wait for checks to finish.
The prompt inherits ordinary text-buffer editing and navigation mappings. Harness
adds submission, queueing, steering, cancellation, mode, and effort controls there.
Normal mode also supports session commands, including `oe` to reopen a pending
question or approval, `op` for artifacts, `og` for agents, model selection, help,
and closing Harness. A question popup closed on any of its pages, by its close key or the
interrupt key, leaves the question pending with its answers, and `oe` reopens it; earlier
questions that were already answered or withdrawn do not stand in its way. The artifact list
omits cancelled plans; their transcript rows still
open them. Cancelling a plan also removes its editable planning directory, so a later plan
never reads the cancelled draft as current work. These hotkeys also work with Read permissions.
In Insert mode, `Ctrl-J` inserts a newline at the cursor and preserves the draft.
`Ctrl-C` pressed in Insert mode keeps the composer in Insert mode at the same cursor,
including when Neovim is busy and delivers the key as an interrupt rather than through
the mapping.
Closing a picker returns focus to the window and mode it was opened from. A chain of
pickers, such as `/provider` followed by **Resume session…**, returns to the mode the
first picker was opened from, and a provider switch keeps the composer in that mode.
Press `Up` on the first draft line to recall submitted prompts. Repeated `Up` and
`Down` browse history, and `Down` past the newest entry restores your unfinished
draft and attachments. Inside a multiline draft, the arrows move between lines.
`Alt-Up` and `Alt-Down` also browse history. Prompt navigation and `oa` or `Tab`
focus the timeline before navigating or toggling activity. Timeline row activation
and gutter selection remain in the upper pane.

In the timeline, press `W` to select whole lines from the left gutter, including
markers and indentation. Extend the selection, then press `<Space>l` to copy the
displayed lines and their gutters to the clipboard.

To ask about part of the timeline, select it in Visual mode and press `A`.
Harness appends the selection to your draft as a Markdown blockquote and enters
Insert mode a blank line below it, ready for your question. A selection started
with `W` includes the displayed gutters.

Unknown or unfinished slash commands keep the draft and show a warning. A leading
path with a filename extension, such as `/workspace/screenshot.png`, submits and
queues as ordinary text. Extension detection also accepts spaces in a path on the
first line and attachments on later lines. Supported commands still take priority,
so `/plan inspect screenshot.png` starts planning rather than sending an ordinary prompt.

`M-s` edits the newest queued prompt in place and leaves the composer in Normal mode.
Repeating it moves backward through
the queue, and `M-t` moves forward while editing. Navigation preserves message order
and saved drafts. Leaving a message unchanged releases it, so only messages you
changed and the one open in the composer stay held. Earlier messages may execute,
but the first held message and all later messages wait. `Ctrl-S` saves the edit in its
reserved position, and the queue row shows the saved text from that moment while Harness
confirms the edit. If Harness rejects the edit, the row returns to the text Harness still
holds, a notice gives the reason, and the next `M-s` reopens your edit. Submitting an
empty edit deletes that message. Finishing restores
any unqueued draft that was in the composer before editing began.
While editing, `Ctrl-Q` instead removes the selected message from the queue and keeps
its edited text and attachments in the normal prompt. It does not submit automatically,
so you can steer that text into the active response or submit it later. Because the
prompt now holds that message, a nonempty unqueued draft from before editing, with its
attachments, becomes the newest prompt-history entry, and a notice says so: press `Up`
to recall it, and `Down` to return to the unqueued message. The upper bar
shows the configured unqueue and queue-navigation shortcuts during editing.
Messages still being submitted cannot be edited until the host acknowledges them.
The same applies while a previous edit is removing the message from the queue.
If delivery wins the race with an edit reservation, editing ends as soon as Harness
reports the delivery. Unchanged text was already sent, so the composer returns to the
draft it held before editing. Text you already changed stays as a normal draft, with a
notice; submit it to send it as a new message. Recovery never sends it automatically.

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
Submit, queue, steer, and queue-editing keys first let any paste that preceded them in
the terminal input land, so a path pasted and submitted in one burst waits for its
import. If the draft changes in any other way before the key takes effect, such as by
keys typed right after it in the same burst, Harness sends nothing, keeps the draft,
and shows a notice. Switching to another Harness composer before the action runs
also leaves the drafts in place.

`/config` includes an **LSP** toggle under **Tools**. It enables the
[LSP tools](lsp-tools.md) for code exploration and permission-checked symbol,
file, and directory renames. It also adds LSP guidance after the
Harness system prompt. Enabling the setting does not start language servers or
change interactive plan navigation.

`/config` opens the current session's configuration. `:ForgeConfig` uses that same
session when invoked from its transcript or composer; elsewhere it edits global
defaults without creating or changing a session. New sessions inherit those defaults.
Changes made on the **Permissions** tab of a session's `/config` apply only to that
session, as `/write`, `/ask`, and the other permission commands do: access scopes, extra
directories, sandbox settings, Mode, and both timeouts. The tab shows the session's
effective values. A running session never follows a change another editor makes to the
defaults; only sessions started afterwards begin from it.

The picker opens five tabs: **Models**, **Workflow**, **Permissions**, **Tools**, and
**System**. The window occupies the bottom 80% of the host at full width.
Tab/Shift-Tab cycle through tabs; Up/Down select settings without wrapping.
Enter activates a setting, Space also toggles booleans, and Left/Right cycle choices.
Press `?` for the full key help. A fixed two-line area describes the selected setting.
Category and field selection survive resizing, updates, and returns from child editors.

**Models** contains Main, Ask, Commit, and Background model sections. Commit selects a
provider from the full provider list and offers Model, Thinking, and Speed controls. Its
choices are global and do not reconfigure the current chat. Speed choices follow the
selected provider and model; Main and Commit keep Speed visible when only the default
tier is available. Main and Background each start with a **Provider** field that decides
which models the section lists. Activating
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
review questions without entering the main timeline. Answer sessions never appear in
`/sessions` or **Resume session**, which list conversations newest created first.

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
block preserves the pending status and the eventual answer. Plan Review formats
Markdown and highlights fenced code in compact comments and answers. Closing PlanReview
while an answer, save, or review edit is running keeps the review open with the notice
"Plan review stays open until its running answers and edits finish."
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
and its tool call waits for the answers. Every question asked while a plan is drafted
or revised is Required: planning waits for your answer, and the status reads
`Waiting for your answer`. Quitting the editor or interrupting the turn ends the question
with it: the transcript reads `Question lost: <header>`, the plan is paused, and you
continue by typing the answer as a prompt and running `/task resume`. Cancelling the plan
with `/plan cancel` ends its questions with it and adds no such row. Open questions with
`oe` or `/questions`; Tab and Shift-Tab switch questions, including answered drafts.
Enter records a draft and advances to the next unanswered question. Confirming the
last pending question submits all answers together; ignoring an optional question
counts toward completing that set. Earlier answers remain editable until submission.
**Ask…** requests clarification immediately without answering. Closing the picker or
switching sessions retains each session's pending answers and text drafts. Pending questions branch beneath the
status text, with previews capped at two display lines and the answer hotkey beside
interrupt. The picker retains the full text. Optional questions remain answerable
after work finishes or chat execution is cancelled; late answers do not restart the
cancelled work. The agent can retract a pending question with `question_withdraw`
and a reason. `H` in the picker includes answered history and allows explicit corrections.
Submitted answers appear as a blue, collapsed **Answered N questions** transcript row.
When an answer starts a new exchange, that row opens the exchange before its Thought
fold. Answers delivered during existing work stay in that exchange. Queued or failed
question replies retain their category in a separate status section.
Adjacent answered entries share one row, including answers submitted separately.
Answers retain this presentation while being delivered to an active turn; they do
not appear briefly as steering messages.
Expand it to read the questions and answers. Pending questions retain their status display
and picker behavior.
**Tools** contains toggles for Subagents, LSP, and Session history.
**Subagent limit** sets the maximum number of Forge-managed subagents that can run
concurrently in one session. It defaults to **8**. Press Enter to edit the limit
as a positive integer. Additional work waits for a slot. Lowering the limit lets
active subagents keep their slots until they finish. Edits in a session apply to
that session, and the standalone configuration editor sets the global default.
Read-only and write-capable subagents can run concurrently in the same checkout.
Write-capable subagents share the same files and can overwrite one another's edits.
Both timeout settings show one value, such as `10m`. Enter opens an
inline editor: Up/Down adjust the duration, Enter saves, and Esc cancels. Values
step through `15s`, `30s`, `45s`, `60s`, then whole minutes from `2m` to `60m`.
The endpoints do not wrap.
**System** contains Thoughts under Display and Logging under Debug.
Logging opens the same picker as `/log`: None, Error, Warn, Info, and Trace.
Info is the default and records major IPC and provider lifecycle operations plus
warnings and errors. It also records rate-limited slow UI and queue-edit diagnostics
locally in `stdpath("cache")/forge/harness-perf.log`. Trace adds verbose diagnostics
and detailed Harness performance logging. `/log none|error|warn|info|trace` sets a level directly; `/log open` opens
the current session's JSONL log in Normal mode, with `N records lost` in its header when the host
failed to write records, and `/log clear` clears it. Saved choices persist
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
invocation retains its captured deadline. Time a tool spends waiting for your
approval does not count against this deadline: the deadline pauses while the
approval is pending and resumes with its remaining time afterwards. Approvals
that Harness tools raise for their own work, such as checks and plan capture,
do not pause it.

Approval timeouts run independently of the picker. An expired approval rejects
that one request, not your instruction: the agent receives the rejection and
continues the requested work in a follow-up turn without asking you to confirm.
The agent does not repeat the expired request on its own, but when you ask for the
operation again it requests it again for your approval.
An idle session has no periodic timeout check. Changing a timeout affects future approvals without
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
Request configuration changes in chat after enabling editing. The `forge-checks`
skill supplies setup guidance. Bare `/checks edit` is not a command.
The Harness winbar appends `ce` to the permissions code while editing is enabled.
See [Project checks](checks.md) for
publication approval, declared inputs, and repository callbacks.

The winbar also shows the selected workflow and phase: `plan · draft`,
`plan · review`, `exec · implement`, `exec · check`, `exec · resolve`,
`exec · verify`, or `goal · work`. Pausing preserves that label, such as
`exec · check · paused`. A planning turn may resubmit its plan before it ends,
so the label changes to `plan · review` only once the submitting turn settles. A
`/compact` while a plan waits in review runs no planning turn, so the label stays
`plan · review` while it compacts. A
revision whose acceptance is already decided, such as the one a saved comment
produces under automatic acceptance, goes straight from `plan · draft` to
`exec · implement`. Resuming Check runs Harness checks locally rather than
starting a model response. Plan acceptance keeps the same task identity as it
enters Execute, and planning never enters Verify.

Automatic continuation stops after five consecutive model turns without
observable progress or the configured total allowance, which defaults to 20 turns.
Reads and phase changes do not replenish the allowance. Explicit task resume
starts a new allowance while retaining the phase, accepted artifact, and findings.
A goal or execution that stops this way, or because the model reported that it cannot
continue, shows `· blocked` in the winbar and adds a transcript row that names the
cause, such as `Execution blocked: 5 consecutive turns changed no files`, followed by
`/task resume` and `/task clear`. The status split names the same cause while the task
stays blocked: `Blocked · 5 consecutive turns changed no files · /task resume · /task clear`.
After a host restart, unfinished tasks require explicit resume before work starts.
A review decision is an explicit resume: accepting a paused plan with `oY` or requesting
changes with `oN` resumes the task, so an execution question appears at once.
A revision that ends without resubmitting, after five turns without progress or at the
total allowance, returns the plan to review at its last submitted revision with a
`Plan revision stopped` row; the plan never fails, and the comments sent with the request
stay in the review.
A first draft that stops this way reads `Planning stopped · no progress limit reached`.
A prompt you type then reaches the model as conversation, and `/task resume` plans
again with it in the conversation.

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
Reference search (`or`) lists usages in declarations and source, not in Algorithm text.
Every declaration a plan introduces, including enum variants and struct fields, needs a
documentation comment (`///` in Rust, `/** */` in TypeScript) directly above it.
Assign with `←` or `<-`, compare with `=`, `≠`, `is`, or `is not` (as in `target is none`),
compute remainders with `%` or `mod`, and loop with
`for each item in items` or `for item in items`.
PlanReview starts with each file folded in Changes and Tests. Press `Tab` on a file heading to
load its complete changes or tests on first unfold. Changes shows a file when it has changed rows
visible in the current mode. Switch to All to review private declarations and ordinary imports.
Reopening the same plan in the same Neovim process restores each file's fold state,
including across revisions. Changes and Tests remember their file folds independently.
Under Changes, each file starts in **Unviewed Changes**. Press `S` on a file heading or any
line inside it to move the whole file to **Viewed Changes**. Press `U` to move it back.
In Visual mode, `S` and `U` move every file touched by the selected rows in one operation.
As in Forge Status, the cursor and scroll position stay in place when the file moves.
If the next file takes its row, you can press `S` again to mark that file viewed.
Comments and questions stay on their original lines. Reopening the same revision restores its viewed
files, including after restarting Neovim or opening that revision from history. A new
revision starts unviewed.
When a plan adds a behavior block to an existing function with the same signature, one
`~` signature row shows both line numbers. Expand the function to read the behavior.
A file heading such as `Modified src/app.rs +1 ~2 -5` counts added, modified, and
removed declarations across the whole file. A signature change and a planned behavior
change on the same declaration count once. Imports and comments contribute zero.
Both Public and All show only nonzero counts. A file with zero or unavailable
declaration counts shows just its status and path. Hunk headings count lines.
Function bodies start collapsed. Expand a body to see its Algorithm or Change blocks, which
start open. The caret on each block heading shows its state (`▾` open, `▸` closed).
Write constructions explicitly as `Projectile(damage = 1)`, `Velocity(direction)` for a tuple
struct or variant, or `CombatEntity()` for a marker. Keep construction separate from the
operation that queues or inserts it. Name the fields and types a function uses inside its
Algorithm or Change; plans have no Calls, Constructs, or Accesses sections to fill in.
New plans start with an empty `docs/overview.md`. Submission requires only non-whitespace text in the overview. The global `forge-planning` skill provides a suggested template.
Submission checks statement syntax and indentation, including branch and loop structure, and
reports every violation in one response, each with the callable, the Algorithm line, and the
rejected text. Drafts can remain incomplete while edited. If a captured repository file changed, the planning agent receives the refreshed baseline diff and can revise or resubmit the same draft. Forge reports body-only edits even when the declarations are identical. The next successful submission creates the review revision.
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
Press `A` to ask a question or `C` to add a comment on a plan line or selection.
Inside a comment card, the cursor moves through the question, comment, and answer text.
Markdown renders inside the card, including syntax highlighting for fenced examples
when the language parser is installed. Visual selections and yanks skip the card headings,
dividers, padding, and borders.
Existing threads start folded when you open the plan. Each summary shows its question
and comment counts. Press `Tab` on the summary to expand it, or anywhere in the thread
to fold it again. On blank lines inside a card, the cursor stops at the text column.
Pressing either key with a selection quotes it in the popup and places the cursor on a
blank line below it. A selection within a thread follows up on the message at the cursor.
The card stays boxed as you move through it. Plan Review scrolls to the bottom of its
thread and opens a multiline input aligned with the card's borders.
In Insert mode, `Enter` adds a line. `Ctrl-S` or `:write` saves feedback and submits
unanswered questions. In Normal mode, `Enter` keeps the draft in the review for a
later save. `Ctrl-C` cancels the input.
Approving a plan and requesting changes both open an optional-comment popup. Press
`Enter` or `Ctrl-S` in normal or insert mode to submit. Approval accepts an empty
comment. A change request needs an overall comment, a saved or unsaved inline comment, or
a review edit; an empty request is refused with that reason and the review stays open.
`Esc` or `Ctrl-C` cancels the decision. Approval comments remain with the accepted
revision and are included in execution context. A plan changed while its popup is
open must be reviewed again before the decision can be submitted.
Accepting with inline comments asks the agent to revise the plan first. The revised
submission returns for review unless automatic acceptance is enabled for the task.
Each comment includes its source context and any question-and-answer thread it
belongs to. Question-only threads are not sent as change requests. Revision work continues
the exchange that submitted the reviewed revision while that exchange is the latest one;
after a later discussion or reply, the revision starts its own exchange labeled
`Revision requested`, so a finished answer is never reopened. When no comment needs a
change, the agent resubmits the unchanged plan.
Review edits and saved comments travel with every message you send the planner while
they are pending, including a question about the plan, so the review always shows what
acceptance runs. Once a later revision replaces the document, edits and comments that
reached the planner retire; any that did not stay pending, comments keep their lines when
the new revision still has them, and a notice names the rest. Renaming a declaration
highlights every place it changes, including signatures, code blocks, test descriptions,
and planned exports, and asks for confirmation before applying; a name that already
exists in the scope is refused. Turning a check off and back on, or a planner that writes
Windows line endings, does not count as a change to the plan.
Every new Plan task starts with automatic acceptance off.
Forge gives automated plan resolution ten seconds. If it expires, the agent receives a Harness bug diagnostic asking it to stop and report the error. Your draft stays available. Time spent reviewing a submitted revision is outside this limit.
Use `/plan accept on` or `/plan accept off` at any time during a Plan or Execute task
to change automatic acceptance without interrupting the agent. The winbar appends `pa`
while it is on. Turning it on while a plan awaits review accepts that revision at once;
turning it off withdraws an acceptance still waiting for its execution answers, and the plan
returns to review. Turning it on again is a new acceptance that asks those questions again.
Successful full submissions check this task setting; drafts and
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

Inside PlanReview, `ol` shows the captured definition of the entity under the
cursor, and `os` shows the canonical plan document as JSON. Both open read-only
popups. Configure them through `keymaps.plan_review.entity_info` and
`keymaps.plan_review.schema`.

Inside PlanReview, `oc` opens plan-specific check selection. Space toggles a
check, with `●` for enabled and `○` for disabled. Visual `j` deletes selected
planned tests or disables selected checks after confirmation.
Under Changes, Rust tests follow normal declaration visibility. Use `ov` to see private
test functions and private `#[cfg(test)]` modules in All. Planned cases also appear in Tests.
The Tests section groups cases under New, Modified, Deleted, or Unchanged file headings.
Expand a file to see its test names and descriptions. These rows
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

PR descriptions use the same Commit provider, model, and thinking settings in `/config`
as commits. Each request uses a fresh temporary conversation with its own output
instructions and no saved Harness conversation. Provider failures stop PR creation and appear
in a notification.
