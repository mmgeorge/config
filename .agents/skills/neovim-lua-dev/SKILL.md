---
name: neovim-lua-dev
description: >-
  Develop and debug Lua and Neovim plugin code, and drive the user's running
  Neovim over RPC. Use when writing or reviewing Lua / Neovim code (autocmds,
  keymaps, treesitter, floating windows, folds, user commands, plugin modules),
  when building Trouble.nvim sources or Snacks.nvim diff UIs, or when you need to
  inspect or execute things inside the live editor — buffers, windows, options,
  LSP clients, diagnostics, plugin source, help docs — via the $NVIM socket.
---

# Neovim & Lua Development

This skill covers two things:

1. **Writing and debugging Lua / Neovim plugin code** — the idioms below, plus
   the deep subsystem patterns in `references/plugin-patterns.md`.
2. **Driving the user's *running* Neovim over RPC** — to inspect editor state
   and verify changes against the real thing instead of guessing.

Reach for the RPC section whenever a question is answerable by the live editor
("what's attached to this buffer?", "did my change take?"). Reach for
`references/plugin-patterns.md` before touching Trouble sources, the Snacks diff
renderer, folds, floating-window previews, or git-from-inside-nvim — those have
non-obvious failure modes already mapped out there.

---

## Driving a live Neovim (RPC)

When an agent runs **inside a Neovim terminal**, `$NVIM` points at the parent
editor's socket (a Unix socket on macOS/Linux, a named pipe on Windows). That
gives full access to Neovim's msgpack-RPC API with no plugins or HTTP servers.

### Prerequisite — confirm the socket

```bash
echo "$NVIM"          # bash; PowerShell: $env:NVIM
```

Empty means you are **not** inside a Neovim terminal and cannot reach an editor.
Don't fabricate output — say so.

### The query pattern (and the NVIM_APPNAME gotcha)

With `NVIM_APPNAME` set, `nvim --server` prints a `Warning: Using NVIM_APPNAME=…`
line to **stdout**, corrupting parsed output (especially JSON). Always capture
then filter:

```bash
# shorthand used below: nvimx 'EXPR'
result=$(nvim --server "$NVIM" --remote-expr 'EXPR') \
  && echo "$result" | grep -v '^Warning: Using NVIM_APPNAME='
```

Use **command substitution** (`$(...)`), never a direct pipe — `$NVIM` may not
expand in pipe contexts.

### Evaluate expressions — `--remote-expr`

```bash
nvimx 'v:version'                                   # Vimscript expression
nvimx 'luaeval("vim.api.nvim_buf_get_name(0)")'     # Lua expression
# multi-statement Lua returning a value → wrap in an IIFE:
nvimx 'luaeval("(function() return vim.api.nvim_win_get_number(0) end)()")'
# complex data → encode JSON:
nvimx 'luaeval("vim.json.encode(vim.api.nvim_list_bufs())")'
```

### Side effects without a return — `execute()`

```bash
nvimx 'execute("lua vim.notify(\"hi from the agent\")")'
nvimx 'execute("Trouble diff_review")'   # run a user command
nvimx 'execute("LspRestart")'
```

`execute()` runs an Ex command and returns its output as a string.

### Send keystrokes / open files

```bash
nvim --server "$NVIM" --remote-send ':echo "hello"<CR>'   # no return, no filter
nvim --server "$NVIM" --remote file.lua                   # open in the instance
nvim --server "$NVIM" --remote-tab a.lua b.lua            # open in new tabs
```

### Handy queries

```bash
nvimx 'luaeval("vim.api.nvim_buf_get_name(0)")'                                   # current buffer path
nvimx 'luaeval("vim.fn.getcwd()")'                                               # cwd
nvimx 'luaeval("vim.json.encode(vim.api.nvim_win_get_cursor(0))")'               # cursor [row,col]
nvimx 'luaeval("vim.o.filetype")'                                                # an option
nvimx 'luaeval("vim.json.encode(vim.tbl_map(function(c) return c.name end, vim.lsp.get_clients({bufnr=0})))")'  # attached LSPs
nvimx 'luaeval("vim.json.encode(vim.diagnostic.get(0))")'                        # diagnostics for current buffer
```

### Finding plugin source / docs (don't open help in the editor)

Never `execute("help …")` — it opens help as a side effect instead of returning
content. Instead resolve paths via RPC, then use `Glob`/`Grep`/`Read`:

```bash
nvimx 'luaeval("vim.fn.stdpath(\"data\")")'                                      # data dir; lazy plugins under <data>/lazy/
nvimx 'luaeval("vim.fn.expand(\"$VIMRUNTIME\")")'                                # built-in runtime docs under <runtime>/doc/
nvimx 'luaeval("vim.json.encode(vim.api.nvim_get_runtime_file(\"lua/**/neotest*\", true))")'  # active runtime files only
# lazy.nvim knows ALL plugins, loaded or not:
nvimx 'luaeval("require(\"lazy.core.config\").plugins[\"neotest\"].dir")'
nvimx 'luaeval("vim.json.encode(vim.fn.getcompletion(\"MiniDiff\", \"help\"))")' # help-tag completion
```

`nvim_get_runtime_file` only sees **loaded** runtime paths — lazy-loaded plugins
that haven't fired won't appear; use the lazy.nvim API above for those.

### Stale LSP diagnostics after external edits

Editing files outside nvim leaves stale diagnostics. To refresh: reload+write
the buffer (`:edit! | write`), or `:LspRestart` and wait ~10s before re-querying.
If diagnostics still look wrong, run the linter directly (e.g. `luacheck`,
`golangci-lint`) to confirm ground truth.

### Safety

- **Never** send `:q`, `:qa`, `:bd`, or other destructive commands without
  explicit confirmation — the user may have unsaved work.
- **Never** modify buffer contents over RPC without asking.
- Prefer `--remote-expr` (read-only) over `--remote-send` (simulated typing).
- Always apply the `grep -v` warning filter on parsed output.

---

## Lua guidelines

- **`local` everywhere.** Global access is slower and pollutes the namespace;
  declare `local` at the top of each scope.
- **Tables are the one data structure** — arrays, maps, objects, modules,
  namespaces. OO via metatables + `__index`; `obj:method()` for methods needing
  `self`.
- **Prefer a single return.** When you need several values, return a table —
  multiple returns are silently truncated by callers and in table constructors.
- **Handle `nil` explicitly.** Lua can't tell a missing key from a key set to
  `nil`; use a sentinel or `rawget` when the distinction matters.
- **Modules return a table:** `local M = {}; … return M`. `require` caches in
  `package.loaded`, so later calls get the same table.
- **Errors:** `pcall`/`xpcall` for protected calls; return `nil, msg` from
  fallible functions and check the first value; never silently swallow — log at
  minimum.
- **LuaJIT hot paths (Neovim runs LuaJIT):** numeric `for` over `pairs()`, keep
  functions monomorphic, avoid creating closures in hot loops, cache `ffi.typeof`
  ctypes.

## Neovim plugin development

- **Layout:** `lua/<plugin>/init.lua` exposing a `setup(opts)` that merges user
  options into defaults with `vim.tbl_deep_extend("force", defaults, opts)`.
  Keep plugin state in a module-level table.
- **Events / keys / commands:** `vim.api.nvim_create_autocmd`,
  `vim.keymap.set(..., { desc = … })` (always set `desc`),
  `vim.api.nvim_create_user_command` (handle range/bang/complete).
- **Messages:** `vim.notify(msg, vim.log.levels.{INFO,WARN,ERROR})`.
- **Syntax-aware edits:** prefer `vim.treesitter` queries over regex.
- **Window options belong to windows:** use `vim.wo[win]`, not `vim.wo[0]` — the
  "current window" inside an autocmd is often not the one you mean.
- **`nvim_win_set_buf` vs `vim.cmd.edit`:** the former is low-level and fires no
  `BufRead`/`FileType` autocmds (LSP/gitsigns won't attach); use `:edit` when you
  need plugins to attach.
- **Tie per-display setup to the buffer, not one code path.** A buffer is
  (re)entered through many routes (window switch, `:b`, pickers, session
  restore); do setup in a `BufEnter`/`BufWinEnter` autocmd so it always
  re-applies. (See reference §"Common Bugs" #6.)

## Verifying changes in the running editor

After editing plugin code, prove it works against the live instance rather than
asserting it:

```bash
# reload a changed module (clear the require cache, then re-require / re-run setup)
nvimx 'execute("lua package.loaded[\"myplugin\"] = nil; require(\"myplugin\")")'
# trigger the feature and observe
nvimx 'execute("MyCommand")'
nvimx 'luaeval("vim.json.encode(vim.diagnostic.get(0))")'
```

For plugin tests, use the `plenary.nvim` busted harness (or `busted` for
standalone Lua); run `luacheck` against the project `.luacheckrc` before
declaring done.

---

## Deep reference

`references/plugin-patterns.md` — the consolidated, battle-tested knowledge for
building Neovim UI plugins, organised by subsystem:

- **Trouble.nvim v3 custom sources** — item/group structure, `ctx.item` vs
  `ctx.node` (and why `ctx.node.item` is only the group's *first* item), fold
  state keyed by `node.id`, main-window detection, refresh behaviour.
- **Snacks.nvim diff renderer** — using `format_hunk`, replacing highlight
  groups, gutter-width math, hunk line counting.
- **Window & buffer pitfalls**, **the fold system**, **highlight & virtual
  text**.
- **Git integration** — staging/discarding individual hunks, synthesising
  untracked-file diffs, and the headless **fake-editor bridge** for running
  `git commit` (with hooks) from inside nvim.
- **Common Bugs & How to Avoid Them** — a numbered catalog (shared table refs,
  fold-on-refresh, per-display setup, scratch-buffer name collisions, group
  actions hitting one file, etc.) each with the root cause and the fix.

Read the relevant section before working in that area — every entry exists
because the obvious approach failed.
