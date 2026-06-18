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
- **Use LuaLS annotations for type safety.** Prefer EmmyLua/LuaLS comments:
  `---@class` / `---@field` for table shapes, `---@alias` for semantic primitive
  tables, `---@param` / `---@return` for public and non-trivial local functions,
  and `---@type` for module state, callback tables, injected test seams, and
  mocks. Use literal unions for known variants such as
  `"section"|"file"|"hunk"`. Do not leave new public APIs or callback seams as
  untyped `table` unless their shape is genuinely arbitrary.
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
  fallible functions and check the first value. Never silently swallow errors.
  In editor-facing code, favor loud user-visible notifications (`vim.notify()` or
  the plugin notification helper) over quiet logs or silent fallbacks so failures
  are obvious and diagnosable.
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
- **Request errors:** every UI-facing request path must explicitly notify
  failures. This includes external CLIs (`git`, `gh`, `curl`, etc.), API/network
  calls, LSP-style requests, async metadata loads, completion sources, and
  background processes. Route nonzero exits, invalid JSON, missing required
  context, and stale-operation errors through `vim.notify()` or the plugin's
  notification module with the underlying stderr/API/decode message. Never
  silently collapse a failed request into an empty list, no-op, stale cache, or
  endless loading state; tests should cover the notification path.
- **Async processes:** prefer `vim.system({ ... }, { text = true, stdout = true,
  stderr = true }, cb)` for plugin Git/process work. Explicitly capture both
  streams on Windows; otherwise MSYS/Cygwin children can leak
  `dtable::stdio_init: couldn't make stderr distinct from stdout` during startup.
  Do not call `vim.fn.system()`, `vim.fn.systemlist()`, or
  `vim.system(...):wait()` from UI render paths, keymaps, autocmds, or other
  interactive code. Never use `os.execute("which ...")` to check for tools; use
  `vim.fn.executable("tool") == 1` so no shell process is spawned. In callbacks,
  schedule any editor API mutations with
  `vim.schedule()` / `vim.schedule_wrap()` and route process start failures,
  nonzero exits, and stale-operation errors through `vim.notify()` or the
  plugin's notification module.
- **External CLI wrappers:** put each external process family behind a small
  module wrapper with typed LuaLS records and a mockable backend seam. UI modules
  should call wrappers such as `plugin.github.current_pr_async()` instead of
  constructing CLI commands inline. The wrapper owns argument shape, JSON
  decoding/normalization, nonzero-exit classification, URL opening, and test
  injection.
- **Request schema validation:** before adding or changing fields for an
  external request, validate the response shape against the real API/CLI contract
  instead of inferring it from nearby code. For read-only requests, make one
  targeted real request through the approved transport for the project; when that
  is not safe or possible, use primary documentation, schema output, or source.
  Tests must cover the outbound request contract as well as the renderer path:
  assert command arguments, JSON field lists, request parameters, and unsupported
  fields so mocks cannot accept shapes the real API rejects.
- **Async refresh state:** long-running refreshes need a request id or equivalent
  cancellation token. Start a new id for each refresh and ignore callbacks from
  older ids so slow Git/process results cannot repaint stale data over newer UI.
- **Cursor-preserving UI rerenders:** separate passive refreshes from intentional
  action movement. For passive async rerenders (Git refresh, Tree-sitter
  enrichment, syntax enrichment), treat "no explicit target" as "preserve the
  user's current cursor." Snapshot the current target immediately before
  mutating buffer lines, not when the async request starts; the user may move
  while the backend is in flight. Prefer a stable item id plus raw cursor line
  fallback. For action-triggered renders, pass an explicit semantic target
  chosen by the action, such as the next hunk or destination section/file
  header.
- **Responsive async UI state:** keep an explicit UI model that can update
  immediately when the user acts, then trigger the backend process asynchronously
  and reconcile when it finishes. Do not replace an already-rendered status/list
  buffer with a generic loading line during action refreshes; only show a
  spinner/`...` state when there is no previous useful UI state to display.
- **Git index mutations:** keep index-writing commands (`git add`,
  `git restore --staged`, `git apply --cached`, checkout/restore fallbacks)
  asynchronous but sequential within a batch. Running multiple index writers in
  parallel can race on `.git/index.lock`. Reconcile status/list UI from Git only
  after the mutation queue is idle, and use a short debounce so rapid repeated
  keypresses can enqueue before a full backend refresh repaints an intermediate
  state.
- **Tree-sitter parsing:** use `LanguageTree:parse(range, on_parse)` when parse
  work is needed during rendering or cursor-driven UI. Render with a cheap
  fallback first, then update from the parse callback. Do not call
  `parser:parse()` synchronously from status renderers, previews, keymaps, or
  autocmds.
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
