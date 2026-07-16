---
name: neovim-lua-dev
description: >-
  Develop, structure, and debug Lua and Neovim plugin code — modular and
  performant by default — and drive the user's running Neovim over RPC. Use when
  writing or refactoring a Neovim plugin (module layout, setup/config, keymaps,
  autocmds, user commands, treesitter, folds, decoration providers, async
  git/process work), when building Trouble.nvim sources or Snacks.nvim diff UIs,
  or when inspecting live editor state via the $NVIM socket.
targets:
  - '*'
---
# Neovim & Lua Development

Two jobs: **write or refactor Lua / Neovim plugin code** that is modular and
performant *by default*, and **drive the user's running Neovim over RPC** to verify
against the real editor instead of guessing.

The sections below are the always-apply essentials. Every subsystem with non-obvious
failure modes has a one-topic file in `references/` — **read the matching reference
before you start that work** (the "Reference map" at the bottom says which, and when).
Those files exist because the obvious approach failed: this skill's source plugin
(`diff_review`) began as a 20,000-line `init.lua` monolith and took several rounds of
decomposition to become clean. The references are how to skip that pain.

---

## Lua essentials

- **`local` everywhere.** Globals are slower and leak; declare `local` at the top of
  each scope. Modules are `local M = {}; … return M` — `require` caches in
  `package.loaded`, so every caller shares one table.
- **Annotate with LuaLS.** `---@class`/`---@field` for table shapes, `---@alias` for
  semantic unions (`"section"|"file"|"hunk"`), `---@param`/`---@return` for public and
  non-trivial functions, `---@type` for module state, callback tables, and test seams.
  Don't leave new public APIs or seams as untyped `table`.
- **Tables are the only data structure** (arrays, maps, objects, modules). OO via
  metatables + `__index`; `obj:method()` when a method needs `self`.
- **Prefer a single return; return a table for several values** — multiple returns are
  silently truncated in constructors and by callers.
- **Handle `nil` explicitly.** Lua can't distinguish a missing key from one set to
  `nil`; use a sentinel (e.g. cache `false` for "checked, empty") when it matters.
- **Errors:** `pcall`/`xpcall` for protected calls; return `nil, msg` from fallible
  functions and check the first value. In editor-facing code, **fail loud** — a
  user-visible `vim.notify()` beats a silent fallback, stale UI, or log-only message.
- **LuaJIT hot paths:** numeric `for` over `pairs()`, keep functions monomorphic,
  don't create closures inside hot loops.

## Neovim plugin essentials

- **Structure (get it right up front, don't "split it later").** Layered package with a
  strict inward dependency direction: `views/ → render/ → git/ → infra/` (+ `shared/`,
  `integrations/`). `init.lua` is a **thin public-API facade** (`setup`/`get`/`open*`),
  **not** a re-export wall — internal modules `require()` each other **directly**
  (the gitsigns/telescope idiom). Shared mutable state lives in a `session.lua` that
  requires nothing. → **`references/architecture.md`** (and how to break genuine cycles
  with lazy in-function `require`).
- **Config:** `setup(opts)` does `M.config = config.setup(opts)`, which
  `vim.tbl_deep_extend("force", defaults, opts)`. Keymaps are **data-driven** (a command
  vocabulary + per-view tables), not hand-written `if view_kind` branches.
  → **`references/configuration.md`**.
- **Async:** use `vim.system({…}, { text = true, stdout = true, stderr = true }, cb)`
  for git/process work; **never** `vim.fn.system()`, `systemlist()`, or
  `vim.system():wait()` in render paths, keymaps, autocmds, or cursor handlers. Schedule
  editor mutations with `vim.schedule()`. Check tools with `vim.fn.executable("x") == 1`,
  never `os.execute("which x")`. → **`references/async-and-git.md`**.
- **Surface every request failure.** External CLI/API/LSP/completion/background calls
  must route nonzero exits, invalid JSON, missing context, and stale-operation errors
  through `vim.notify()` (or the plugin's notify module) with the real message. Keep
  "request failed" distinct from "succeeded with zero results"; never collapse a failure
  into an empty list, no-op, or endless spinner. Test the notification path.
- **Tree-sitter:** parse async with `LanguageTree:parse(range, on_parse)` — render a
  cheap fallback first, upgrade from the callback. Never parse synchronously in a
  renderer, preview, keymap, or autocmd. → **`references/async-and-git.md`**.
- **Big buffers:** keep all logical rows as **real buffer lines**; do **not** virtualize
  text (it breaks search/marks/undo/folds). Scope expensive highlights to the viewport
  with `nvim_set_decoration_provider` — postpone highlight application to the rows that
  become visible. → **`references/rendering-and-highlights.md`**.
- **Window options belong to windows:** `vim.wo[win]`, not `vim.wo[0]` — the "current
  window" inside an autocmd is often not the one you mean.
- **Focused TUI cursor hiding needs an explicit lifecycle.** A window-local
  `Cursor:Hidden` highlight does not reliably hide the terminal cursor in the focused
  window. Capture `vim.o.guicursor`, set an explicit hidden cursor while the modal owns
  focus, then restore an explicit visible fallback when the captured value was empty.
  Never leave the global option hidden after closing or entering an input child.
  → **`references/common-bugs.md`**.
- **`nvim_win_set_buf` fires no `BufRead`/`FileType` autocmds** (LSP/gitsigns won't
  attach); use `vim.cmd.edit` when you need plugins to attach.
- **Tie per-display setup to the buffer, not one code path** — a buffer is re-entered
  via many routes (window switch, `:b`, pickers, session restore). Do setup in a
  `BufEnter`/`BufWinEnter` autocmd so it always re-applies. (See `common-bugs.md` #6.)
- **Keymaps/commands:** always set `desc` on `vim.keymap.set`; handle range/bang/complete
  on `nvim_create_user_command`. Prefer `vim.treesitter` queries over regex for
  syntax-aware edits.

## Verify against the running editor

After editing plugin code, prove it works against the live instance rather than asserting
it — reload the module and trigger the feature over RPC:

```bash
# clear the require cache, re-require, run the command, inspect the result
nvim --server "$NVIM" --remote-expr 'execute("lua package.loaded[\"myplugin\"]=nil; require(\"myplugin\")")'
nvim --server "$NVIM" --remote-expr 'execute("MyCommand")'
```

The full RPC toolkit (the `$NVIM` socket, the `NVIM_APPNAME` stdout gotcha, query
patterns, safety rules) is in **`references/live-nvim-rpc.md`**. For plugin tests, use the
`plenary.nvim` busted harness and run `luacheck` against the project `.luacheckrc` before
declaring done. The `Undefined global vim` lua-ls warnings in a project without a
`.luarc.json` are a harness artifact, not a runtime issue — don't chase them.

---

## Reference map

Read the file that matches the work **before** you start it:

| Read this… | …when you are about to |
| --- | --- |
| **`references/architecture.md`** | Scaffold a new plugin, or lay out modules / requires / shared state / break a `require` cycle. (Read first — design the layers up front.) |
| **`references/configuration.md`** | Add a user option, add/rebind an in-buffer key, wire a command into a view, or write the lazy.nvim spec. |
| **`references/rendering-and-highlights.md`** | Render large diffs/lists, add per-row highlights or virtual text, use a decoration provider, or implement folding / lazy syntax in a buffer that can reach tens of thousands of lines. |
| **`references/async-and-git.md`** | Shell out to `git`/`gh`, parse a diff, stage/unstage hunks, run `git commit` from inside nvim, or do Tree-sitter work in a render/keymap/autocmd path. |
| **`references/trouble-and-snacks.md`** | Build a Trouble.nvim v3 custom source or use the Snacks.nvim diff renderer. |
| **`references/common-bugs.md`** | Scan the moment a symptom matches — preview won't edit, folds re-collapse on stage, cursor jumps back after an action, a modal cursor remains hidden or visible, a buffer half-renders after a window switch, or Windows reports a stdio error. |
| **`references/live-nvim-rpc.md`** | Inspect or verify against the **running** editor over the `$NVIM` socket (buffers, windows, options, LSP, diagnostics; "did my change take?"). |

Also reach for the **`terminal-tui-debugging`** skill when the rendered terminal grid
itself matters (cursor position, what a cell actually displays), and for project-specific
guidance see `nvim/lua/diff_review/architecture.md` and `AGENTS.md` in the plugin.
