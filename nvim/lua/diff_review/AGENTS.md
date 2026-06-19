# DiffReview Plugin Notes

DiffReview is a standalone local Neovim plugin. Trouble must not own this
feature: do not place DiffReview code under `trouble/sources`, do not register a
`diff_review` Trouble mode, and do not route `:GitStatus` through Trouble.

The public commands are `:GitStatus`, `:GitBranchDiff <branch>` (read-only diff
of the working tree against a branch or revision),
`:GitBranchDiffFile <file> <branch>` (same, limited to one file), and
`:GitFileRevision <file> <commit>` (read-only buffer with the file's content at
a revision), registered by `nvim/lua/plugins/diff_review.lua`. The plugin
entrypoints are `require("diff_review").open()`,
`require("diff_review").open_branch_diff(branch, { file = ... })`, and
`require("diff_review").open_file_revision(file, rev)`.

Pressing open on a deleted (left-side) diff line opens a file revision buffer
(`GitFileRevision://<path>@<short sha>`, owned by `M._file_revision`) at the
diff's base revision — index for unstaged hunks, HEAD for staged hunks, the
branch for `:GitBranchDiff` views — where the old line number is exact. The
buffer name carries the short sha of the underlying commit (HEAD's sha for the
index, which is not a commit). The buffer is `nowrite` (not `nofile`, so
buffer pickers list it), persists when hidden, closes on `q`, and shows a red
winbar header while displayed. If the base content cannot be fetched, the
jump falls back to the working-tree file.

Tests must not trigger the `o` status key via `maparg().callback()` — the
user's mini.clue config registers `o` as a prefix trigger whose callback
blocks on a key query (hangs headless). Trigger open via `<CR>` instead.

In the PR view (`ogp`), the PR title and description are editable in place
(`M._pr_edit`): the buffer is `acwrite`, and 'modifiable' follows the cursor
— unlocked exactly while the cursor sits on the title line or inside the
description block (regions tracked with extmarks), locked everywhere else,
so every native editing command works in the regions. Unsynced fields show
an inline `*` before their label; `:w` clears the markers immediately and
syncs via `gh pr edit` through a sequential queue (`M._pr_edit.enqueue` —
not `status_enqueue_operation`, whose reconcile tail would rebuild the PR
buffer as a status view). Failures notify and restore the markers.
Re-renders are blocked while edits are unsynced.

Pressing `or` in the status or PR view starts PR review mode
(`M._review`, view_kind = "review", `M.open_review(pr, opts)`): the PR title,
an editable review summary, and the changed files split into Unviewed/Viewed
sections (files start expanded). This is the **normal batched review flow** —
comments are drafted locally, synced deliberately, and submitted together:
- `S`/`U` move the hunk/file under the cursor between the sections; on
  Unviewed/Viewed section headers they move the whole section.
- `C` on a changed (diff body) line creates an empty inline editable comment
  body and focuses it; on an existing comment, or the line immediately
  above/below it, it focuses that comment body. Comments render as real,
  navigable buffer lines
  emitted inline right below their anchor row by a renderer hook
  (`M._status.review_after_row`, consumed in `status_add_fancy_row`): a
  read-only rule header, raw editable full-width body rows, and a read-only
  rule footer. Header rules start directly with the author/date, use dashes only
  between the left label and the right-aligned line number, and do not have
  surrounding `--` frame markers. Size header/footer rules to the window text
  area so they never overflow or soft-wrap, and refresh those rule rows from
  shared display/resize handling (`WinResized`/`VimResized` plus
  `BufWinEnter`). The review window enables soft word wrap; do not hard-wrap,
  reflow, prefix, or draw box sides into editable body text.
- `J` deletes the draft comment under the cursor; `y`/`n` jump between
  comments.
- `b` on actual comment rows (header/body/footer) opens the GitHub comment
  anchor; adjacent diff rows keep their code-line anchors instead of borrowing
  the nearby comment.
- Unsynced comment creates/edits show `*` before the username in the comment
  header. `<C-s>` clears the marker immediately and syncs dirty comments to the
  pending GitHub review; failures restore the marker.
- `b` browses the PR changes tab; the submit key (`cc`) picks a verdict
  (`vim.ui.select` → APPROVE/COMMENT/REQUEST_CHANGES) and posts the summary,
  verdict, and every draft comment in ONE `gh api .../reviews` request
  (`gh.submit_pr_review_async`, `commit_id` = head SHA, `comments[]`); on
  success the drafts clear.

The review summary is edited in place with the same cursor-follows-modifiable
mechanism as `M._pr_edit`. Diff rows carry absolute paths, so comments keep
both `path` (repo-relative, for GitHub) and `abs_file` (for matching rendered
rows). Tests drive the comment body and verdict through the
`M._review.input_provider` / `M._review.verdict_provider` seams. The summary
region's end extmark uses right gravity (a left-gravity boundary mark gets
pulled into an edit of the adjacent line). Review-specific key defaults live in
`config.keymaps.review`, but the hint bar, `?` help, and installed mappings must
come through the shared status command specs in `init.lua`; pin only the compact
high-value subset in the winbar and keep the full command list in `?`. The
verdict chooser is a plain popup window (`M._review.pick_verdict`,
`c`/`a`/`r`/`q`), not `vim.ui.select`/snacks.

`bc` in the status view creates a branch (`M._create_branch`): it prompts for
a name via a centered popup (`M._prompt_branch_name`, not vim.ui.input/snacks;
prefilled with the branch prefix), then `git switch -c` and refreshes.
The prefix comes from per-repository config — `<repo root>/.diffreview.json`
(`{ "branch_prefix": "..." }`, read by `M._repo_config`, test seam
`set_reader`) — falling back to `config.branch_prefix` (default `matt9222/`).

init.lua is at Lua's hard limit of 200 local variables per chunk: new
file-scope helpers must hang off the module table (see `M._branch_diff` and
`M._review`) instead of being declared `local`. (luals's syntax check can
over-count this; the ground truth is whether real nvim loads the file.)

The diff gutter (old/new line numbers and the `+`/`-` sign) is rendered as
inline virtual text (`hunk_add_gutter`), not buffer text: buffer lines for
hunk rows contain only the code content, so visual selection, yank, and
search operate on the code alone. Tests asserting gutter content must read
the row's inline `virt_text` extmark (see `gutter_text` in
`tests/diff_review/hunk_boundary_context.lua`).

Diff rendering uses two layers:

- Raw hunks are the data/action model. Git diff/show commands should request
  zero-context hunks (`--unified=0`) so each raw hunk maps tightly to actual
  changed lines. Stage/unstage/discard, viewed/unviewed state, comments, jumps,
  and future line-level actions must use the raw hunk metadata and line mapping,
  not whatever rows happen to be visible after presentation merging.
- Virtual display hunks are the presentation model. `build_fancy_diff_rows()`
  builds render plans from raw hunks, injects bounded Tree-sitter-aware context,
  and merges adjacent or overlapping display windows so the user does not see
  duplicate `@@` headers or repeated context. A displayed hunk may therefore
  contain multiple raw hunks; carry them in `raw_hunks` and expand them again
  before any action that writes state or talks to git.

GitStatus has an additional status-level grouping step before each hunk is
handed to `build_fancy_diff_rows()`. That grouping must use the same effective
display-window idea: expand each raw hunk by the shared context padding limit and
merge hunks whose padded ranges touch. If status renders the raw hunks one at a
time, the row builder cannot merge across them and bridge context such as
`Self {` can be duplicated or split under separate headers. Keep
`tests/diff_review/status_virtual_hunk_merge.lua` covering this edge case.

The `@@ +N -N` header in GitStatus/DiffReview is a display summary for the
merged virtual group, not a patch header to feed back to git. If a test or
feature needs the original patch, assert against the raw hunk diff. If a test
needs the visible UI, assert that nearby zero-context changes render under one
header without duplicated semantic context.

## LuaLS Typing

Use LuaLS/EmmyLua comment annotations for plugin code. Public module functions,
test seams, and non-trivial tables must have explicit types so LSP jump-to-type,
completion, and diagnostics work.

Required patterns:

- Define table shapes with `---@class` and fields with `---@field`.
- Define narrow aliases with `---@alias` when a primitive table has semantic
  meaning, for example `---@alias DiffReviewGitCommand string[]`.
- Annotate public functions and injected callbacks with `---@param` and
  `---@return`.
- Type module tables with `---@type` when the module exposes state or a stable
  public API.
- Keep test mocks typed too. If a test injects a backend, mark it with
  `---@type DiffReviewGitBackend` or `---@type DiffReviewGhBackend`.
- Prefer exact union literals for known variants, for example
  `"section"|"file"|"hunk"`, instead of plain `string`.

Do not leave new public tables or callback seams as untyped `table` unless the
shape is genuinely arbitrary.

## Testing

Run the load check:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false); require('diff_review').setup(); require('diff_review.commit'); require('plugins.diff_review')" -c "qa!"
```

Run the command-open check:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -c "GitStatus" -c "lua assert(vim.bo.filetype == 'GitStatus', 'GitStatus buffer did not open')" -c "qa!"
```

Run the mocked integration test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/mock_backend.lua
```

Run the GitHub integration test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/github_integration.lua
```

Run the async stale-refresh test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/async_stale.lua
```

Run the real-git visual selection integration test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/visual_selection.lua
```

Run the walkthrough test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/walkthrough.lua
```

Run the branch-diff test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/branch_diff.lua
```

Run the file-revision test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/file_revision.lua
```

Run the PR-edit test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/pr_edit.lua
```

Run the PR-review test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/pr_review.lua
```

Run the branch-create test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/branch_create.lua
```

Run the whitespace check:

```text
git diff --check -- nvim/lua/diff_review nvim/lua/plugins/diff_review.lua nvim/lua/plugins/trouble.lua nvim/tests/diff_review
```

Headless Neovim may create `nvim.log`; remove it before finishing a task.

## Git Backend Seam

Tests may inject git behavior with:

```lua
require("diff_review").set_git_backend(backend)
require("diff_review").reset_git_backend()
```

The backend may implement:

- `systemlist_async(command, cb) -> nil`
- `system_async(command, input, cb) -> nil`
- `systemlist(command) -> output_lines, exit_code`
- `system(command, input) -> output_text, exit_code`
- `delete(path) -> exit_code`

Production code should prefer the async methods and route process errors through
notifications. Synchronous methods are only compatibility for tests or
non-interactive helper paths. Keep index-mutating git operations asynchronous
but sequential within a batch so tests and production avoid `.git/index.lock`
races.

## Walkthrough Seam

`diff_review.walkthrough` drives the `ow` review walkthrough from
`.walkthrough.json` at the repo root (schema: `walkthrough.schema.json` next to
the module; generated by the `walkthrough` agent skill). It never touches init
internals - init hands it a `DiffReviewWalkthroughHost` of closures via
`M._walkthrough_host(buf)`. Tests inject the JSON with:

```lua
require("diff_review.walkthrough").set_reader(function(path) return fixtures[path] end)
require("diff_review.walkthrough").reset_reader()
```

HEAD-sha staleness lookups ride the git backend seam (`rev-parse HEAD`).

## GitHub CLI Seam

Use `diff_review.gh` for all GitHub CLI integration. Do not call `gh` directly
from status, PRView, keymaps, renderers, or tests. The wrapper owns nonblocking
`vim.system` calls, JSON normalization, no-PR detection, URL opening, and the
`DiffReviewGhBackend` test seam. Tests that render status must mock this backend
so they never depend on a real `gh` installation, auth state, network, or
repository remote.

All GitHub/Git request consumers must notify request failures. Nonzero exits,
API errors, invalid JSON, missing required repo/PR context, and failed background
lookups should reach the user via `notify_error`, `vim.notify`, or the owning
notification helper with the underlying stderr/API/decode message. Do not treat
failed requests as empty comments, empty completion lists, unchanged status, or
generic loading state. Empty-but-successful results are fine, but they must be
distinguishable in code and tests from request failures.

DiffReview error handling should be loud by default. If a caught error affects
rendering, syncing, completion, cursor behavior, or user action handling, report
it with a notification rather than only logging it or silently falling back.

GitStatus, PR overview, PR review, issue, and notification buffers share GitHub
repo metadata and issue completion through `github.repo_cache` and
`github.issue_index`; keep that ownership centralized. Repo-scoped durable data
lives under `vim.fn.stdpath("data")/gitstatus/repos/<owner>/<repo>/`, not inside
DiffReview module state or ad hoc temp paths. The issue index specifically uses
`issues/issues.redb` plus `issues/open-snapshot.json`, populated by background
GraphQL sync and the Rust redb sidecar in `nvim/rust/github-issue-index`. `#`
completion must read the snapshot locally and must not run a live GitHub search
per keystroke. `:GithubIssueSync [all]` owns manual refresh, and
`:GithubDeleteRepoCache` owns repo cache cleanup.

Any status, picker, preview, header, or detail row that displays text starting
with a commit subject must use the shared conventional commit formatter. In this
module, route subject text through `M._status_conventional_commit_subject_segments(...)`
or `M._status_conventional_commit_type_end(...)` instead of hand-building a
plain string, so the conventional commit `<type>` prefix is colorized
consistently across Head/Merge/Push, PR, About, and commit-list rows.

## Shared Status UX Pattern

GitStatus' Unstaged/Staged model is the canonical UX pattern for all
DiffReview list-style buffers. When adding related surfaces (PR review
Unviewed/Viewed, branch diffs, walkthroughs, or future grouped review flows),
reuse the same concepts instead of inventing a separate interaction model:

- Group content into named sections that can be toggled with `<Tab>` via
  `status_toggle()`; section, file, and hunk folds should all flow through the
  shared fold state (`status_folded` / `set_status_folded`).
- Use movement actions like GitStatus stage/unstage: update the in-memory
  section model immediately, preserve cursor/fold intent, then sync/reconcile
  asynchronously. For review mode, `S`/`U` moving hunks or files between
  Unviewed/Viewed should feel like GitStatus `S`/`U` moving items between
  Unstaged/Staged. Header actions matter too: pressing `S`/`U` on a section
  header should apply to all actionable items in that section, matching
  GitStatus staged/unstaged header behavior.
- Keep the same command model for mappings, hint bars, and `?` help. The winbar
  should show only a compact pinned subset; the full command palette stays in
  `?`.

Status-buffer mappings must be defined from `config.defaults.keymaps.status` and
the central status command spec in `init.lua`. Review-specific defaults may live
under `config.defaults.keymaps.review`, but their command specs, visibility,
hint participation, and help text still belong in the shared command model. Do
not hardcode key text in the hint row, the `?` help popup, or tests. User config
can override or disable a mapping, and the actual keymaps plus displayed help
must stay in sync from that single source.

For status actions, do not replace an already-rendered status buffer with a
generic loading line. Apply an optimistic in-memory section update immediately,
enqueue the Git operation, and reconcile from Git after the operation queue is
idle. This keeps rapid actions such as stage-then-unstage ordered without UI
flashing.

Tree-sitter context lookup must use async `LanguageTree:parse(range, on_parse)`
with a cached fallback-render-then-upgrade flow. Do not call synchronous
`parser:parse()` from DiffReview renderers, keymaps, autocmds, or preview
refresh paths.

Do not register the whole `GitStatus` filetype as markdown to support PR/issue
description rendering. GitStatus is a mixed buffer: status rows, editable
markdown regions, and real diff/code rows live together. Whole-buffer markdown
registration lets markdown Tree-sitter syntax overlay diff rows with `Special`
or `@markup.*`, which can make Rust/Slang/etc syntax look missing even when
DiffReview's own syntax extmarks are present. Markdown rendering and otter code
completion must be explicit for the description/comment region or a true
markdown-like buffer, and syntax regressions should inspect the rendered cell
stack with `nvim__inspect_cell`, not just DiffReview extmarks.
