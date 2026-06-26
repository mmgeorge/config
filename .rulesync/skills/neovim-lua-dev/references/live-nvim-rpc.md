# Driving a Live Neovim over RPC

*When to read this:* whenever a question is answerable by the **running** editor — "what buffer/window/option/LSP/diagnostic is live right now?", "did my `diff_review` change actually take?" — instead of inferring from source. This is about inspecting and verifying against a real instance, not writing plugin code (see architecture.md and the sibling references for that).

When an agent runs **inside a Neovim terminal**, `$NVIM` points at the parent editor's socket — a Unix socket on macOS/Linux, a **named pipe** on Windows (`\\.\pipe\nvim.…`). That gives full msgpack-RPC access with no plugins or HTTP server. Use it to ground every claim about the live editor in fact.

## Prerequisite — confirm the socket

```bash
echo "$NVIM"          # bash
```
```powershell
$env:NVIM             # PowerShell (this repo's default shell)
```

Empty means you are **not** inside a Neovim terminal and cannot reach an editor. Do not fabricate output — say so and stop. Every command below assumes a non-empty `$NVIM`.

## The query pattern and the NVIM_APPNAME gotcha

With `NVIM_APPNAME` set, `nvim --server` prints a `Warning: Using NVIM_APPNAME=…` line to **stdout**, corrupting any parsed output (fatal for JSON). Always capture, then filter:

```bash
# shorthand used throughout: nvimx 'EXPR'
result=$(nvim --server "$NVIM" --remote-expr 'EXPR') \
  && echo "$result" | grep -v '^Warning: Using NVIM_APPNAME='
```
```powershell
# PowerShell equivalent
$result = nvim --server $env:NVIM --remote-expr 'EXPR'
$result | Where-Object { $_ -notmatch '^Warning: Using NVIM_APPNAME=' }
```

Use **command substitution** (`$(...)`), never a bare pipe into the warning filter — `$NVIM` may not expand inside some pipe contexts. Apply the warning filter on **every** parsed query; skip it only for `--remote-send`/`--remote` which return nothing.

## Evaluate expressions — `--remote-expr`

`--remote-expr` is read-only and is the **default** tool. Three escalating forms:

```bash
nvimx 'v:version'                                          # Vimscript expression
nvimx 'luaeval("vim.api.nvim_buf_get_name(0)")'           # a single Lua expression
# multi-statement Lua that must return a value → wrap in an IIFE:
nvimx 'luaeval("(function() return vim.api.nvim_win_get_number(0) end)()")'
# any non-scalar (tables, lists) → encode to JSON, then parse on your side:
nvimx 'luaeval("vim.json.encode(vim.api.nvim_list_bufs())")'
```

`luaeval` evaluates **one expression**. The moment you need a `local`, a loop, or two statements, wrap them in an IIFE `(function() … end)()`. Anything that is not a string/number must go through `vim.json.encode` or it comes back as an unusable Vimscript-coerced blob.

## Side effects without a return — `execute()`

```bash
nvimx 'execute("lua vim.notify(\"hi from the agent\")")'
nvimx 'execute("GitStatus")'        # run a diff_review user command
nvimx 'execute("LspRestart")'
```

`execute()` runs an Ex command and returns its captured output as a string, so it works through the read-only `--remote-expr` channel. Reach for it to **trigger** a `diff_review` command (`GitStatus`, `GitBranchDiff`, `GitFileRevision`, `GitDiffCompactPreview`) and then re-query state to verify the effect.

## Send keystrokes / open files — `--remote-send`, `--remote`, `--remote-tab`

```bash
nvim --server "$NVIM" --remote-send ':echo "hello"<CR>'   # simulated typing; no return, no filter
nvim --server "$NVIM" --remote      nvim/lua/diff_review/init.lua   # open in the instance
nvim --server "$NVIM" --remote-tab  a.lua b.lua                     # open each in a new tab
```

`--remote-send` simulates keystrokes (modes, mappings, and all) — powerful but stateful and easy to get wrong. Prefer `--remote-expr`/`execute()` for anything observable; reserve `--remote-send` for genuinely keyboard-only flows (e.g. exercising a `diff_review` keymap like `<leader>gd` end-to-end). `--remote`/`--remote-tab` open files in the user's session — only do so when asked.

## Handy queries

```bash
nvimx 'luaeval("vim.api.nvim_buf_get_name(0)")'                                  # current buffer path
nvimx 'luaeval("vim.fn.getcwd()")'                                              # cwd (diff_review keys git work off this)
nvimx 'luaeval("vim.json.encode(vim.api.nvim_win_get_cursor(0))")'             # cursor [row,col]
nvimx 'luaeval("vim.o.filetype")'                                              # a single option
nvimx 'luaeval("vim.json.encode(vim.tbl_map(function(c) return c.name end, vim.lsp.get_clients({bufnr=0})))")'  # attached LSP client names
nvimx 'luaeval("vim.json.encode(vim.diagnostic.get(0))")'                       # diagnostics for current buffer
```

diff_review-specific inspection (extmarks, namespaces, the rendered cell stack):

```bash
# all namespaces (find the diff_review.* decoration namespaces, e.g. diff_review.views.pr.review)
nvimx 'luaeval("vim.json.encode(vim.api.nvim_get_namespaces())")'
# every extmark on buffer 0 for one namespace id (substitute NS):
nvimx 'luaeval("vim.json.encode(vim.api.nvim_buf_get_extmarks(0, NS, 0, -1, {details=true}))")'
# what highlight actually WON on a cell — extmark presence alone does not prove the visible color:
nvimx 'luaeval("vim.json.encode(vim.api.nvim__inspect_cell(0, ROW, COL))")'
```

Use `nvim__inspect_cell` (not extmark listing) when debugging a `diff_review` highlight regression — markdown/`@markup.*`/`Special` groups can override a language-specific extmark, and only the cell stack shows the winner.

## Finding plugin source / docs via RPC (never open help in the editor)

Never `execute("help …")` — it opens a help split as a **side effect** instead of returning content. Resolve paths via RPC, then read them with `Glob`/`Grep`/`Read`:

```bash
nvimx 'luaeval("vim.fn.stdpath(\"data\")")'                                      # data dir; lazy plugins under <data>/lazy/
nvimx 'luaeval("vim.fn.stdpath(\"config\")")'                                    # config dir — this dotfiles checkout
nvimx 'luaeval("vim.fn.expand(\"$VIMRUNTIME\")")'                                # built-in runtime; docs under <runtime>/doc/
nvimx 'luaeval("vim.json.encode(vim.api.nvim_get_runtime_file(\"lua/diff_review/**\", true))")'  # active runtime files
# lazy.nvim knows EVERY plugin, loaded or not:
nvimx 'luaeval("require(\"lazy.core.config\").plugins[\"trouble.nvim\"].dir")'
nvimx 'luaeval("vim.json.encode(vim.fn.getcompletion(\"GitStatus\", \"command\"))")'  # command-name completion
```

`nvim_get_runtime_file` only sees **loaded** runtime paths — a lazy-loaded plugin that has not fired yet won't appear; use the `lazy.core.config` API for those. diff_review itself loads from `stdpath("config")/lua/diff_review` (a local plugin), so its source is in this repo, not under `lazy/`.

## Reloading a changed module and verifying live

After editing `diff_review` code, **prove** it against the running instance instead of asserting. Clear the `require` cache, re-require, re-run `setup`, trigger, then observe:

```bash
# drop the cached top module AND the submodule you changed (require caches each separately):
nvimx 'execute("lua package.loaded[\"diff_review\"] = nil; package.loaded[\"diff_review.views.status.state\"] = nil")'
nvimx 'execute("lua require(\"diff_review\").setup({})")'   # re-run setup if it registers state/highlights
nvimx 'execute("GitStatus")'                                # trigger the feature
nvimx 'luaeval("vim.json.encode(vim.api.nvim_win_get_cursor(0))")'   # observe the result
```

`package.loaded` caches **each** submodule by its require key, so clearing only `"diff_review"` leaves stale `diff_review.git.*` / `diff_review.views.*` tables behind. Clear every module you touched. If `setup()` registers view controllers or highlights (it calls `register_view_controllers()` and configures highlights), re-run it so the reload actually rewires.

## Stale LSP diagnostics after external edits

Editing files **outside** nvim leaves stale diagnostics in the live instance. To refresh: reload+write the buffer (`:edit! | write`), or `:LspRestart` and wait ~10s before re-querying. If diagnostics still look wrong, run the linter directly (`luacheck` against the project `.luacheckrc`) to confirm ground truth — do not trust a possibly-stale `vim.diagnostic.get`.

## Safety

- **Never** send `:q`, `:qa`, `:bd`, `:bw`, or any destructive command without explicit confirmation — the user may have unsaved work in this exact instance.
- **Never** mutate buffer contents over RPC (`nvim_buf_set_lines`, `--remote-send` edits, `:%d`) without asking first. This is the user's live editor, not a scratch harness.
- **Prefer `--remote-expr`** (read-only) over `--remote-send` (simulated typing) for anything you can observe instead of type.
- **Always** apply the `grep -v '^Warning: Using NVIM_APPNAME='` filter on parsed/JSON output.
- Treat module reloads (clearing `package.loaded`) as a mutation of editor state — fine for verifying your own changes, but mention it; do not silently re-`setup` plugins the user is actively using.
