# DiffReview Plugin Notes

DiffReview is a standalone local Neovim plugin. Trouble must not own this
feature: do not place DiffReview code under `trouble/sources`, do not register a
`diff_review` Trouble mode, and do not route `:DiffReview` through Trouble.

The public command is `:DiffReview`, registered by `nvim/lua/plugins/diff_review.lua`.
The plugin entrypoint is `require("diff_review").open()`.

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
  `---@type DiffReviewGitBackend`.
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
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -c "DiffReview" -c "lua assert(vim.bo.filetype == 'DiffReviewStatus', 'DiffReviewStatus buffer did not open')" -c "qa!"
```

Run the mocked integration test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/mock_backend.lua
```

Run the async stale-refresh test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/async_stale.lua
```

Run the real-git visual selection integration test:

```text
nvim --headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)" -S nvim/tests/diff_review/visual_selection.lua
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

Tree-sitter context lookup must use async `LanguageTree:parse(range, on_parse)`
with a cached fallback-render-then-upgrade flow. Do not call synchronous
`parser:parse()` from DiffReview renderers, keymaps, autocmds, or preview
refresh paths.
