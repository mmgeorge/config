# DiffReview Plugin Notes

DiffReview is a standalone local Neovim plugin. Trouble must not own this
feature: do not place DiffReview code under `trouble/sources`, do not register a
`diff_review` Trouble mode, and do not route `:GitStatus` through Trouble.

The public commands are `:GitStatus`, `:GitBranchDiff <branch>` (read-only diff
of the working tree against a branch or revision), and
`:GitBranchDiffFile <file> <branch>` (same, limited to one file), registered by
`nvim/lua/plugins/diff_review.lua`. The plugin entrypoints are
`require("diff_review").open()` and
`require("diff_review").open_branch_diff(branch, { file = ... })`.

init.lua is at Lua's hard limit of 200 local variables per chunk: new
file-scope helpers must hang off the module table (see `M._branch_diff`)
instead of being declared `local`.

The diff gutter (old/new line numbers and the `+`/`-` sign) is rendered as
inline virtual text (`hunk_add_gutter`), not buffer text: buffer lines for
hunk rows contain only the code content, so visual selection, yank, and
search operate on the code alone. Tests asserting gutter content must read
the row's inline `virt_text` extmark (see `gutter_text` in
`tests/diff_review/hunk_boundary_context.lua`).

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

## Status Keymaps

Status-buffer mappings must be defined from `config.defaults.keymaps.status` and
the central status command spec in `init.lua`. Do not hardcode key text in the
hint row, the `?` help popup, or tests. User config can override or disable a
mapping, and the actual keymaps plus displayed help must stay in sync from that
single source.

For status actions, do not replace an already-rendered status buffer with a
generic loading line. Apply an optimistic in-memory section update immediately,
enqueue the Git operation, and reconcile from Git after the operation queue is
idle. This keeps rapid actions such as stage-then-unstage ordered without UI
flashing.

Tree-sitter context lookup must use async `LanguageTree:parse(range, on_parse)`
with a cached fallback-render-then-upgrade flow. Do not call synchronous
`parser:parse()` from DiffReview renderers, keymaps, autocmds, or preview
refresh paths.
