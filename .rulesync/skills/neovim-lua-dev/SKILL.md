---
name: neovim-lua-dev
description: >-
  Develop, structure, and debug Lua and Neovim plugin code with modular architecture
  and asynchronous I/O, and drive running Neovim instances over RPC. Use when
  authoring or refactoring Neovim plugins (module layout, configuration schemas,
  keymaps, autocmds, user commands, Tree-sitter, folds, decoration providers,
  asynchronous process execution), integrating Trouble.nvim or Snacks.nvim diff
  renderers, or inspecting live editor state via the $NVIM socket.
targets:
  - '*'
---

# Neovim & Lua Development

This skill governs authoring and refactoring Neovim plugins in Lua and inspecting running Neovim instances over RPC.

Read the relevant reference file in `references/` before modifying complex subsystems.

---

## Lua Language Standards

- **Scope Management:** Declare `local` variables explicitly at the top of scopes. Return a module table (`local M = {} ... return M`) to leverage `package.loaded` caching across callers.
- **Type Annotations:** Use EmmyLua/LuaLS annotations: `---@class` and `---@field` for table structures, `---@alias` for domain unions, `---@param` and `---@return` for public APIs, and `---@type` for module state and callback seams. Do not expose public APIs as untyped `table`.
- **Object Modeling:** Use metatables and `__index` for object orientation. Use colon syntax (`obj:method()`) when operations require instance state (`self`).
- **Return Conventions:** Prefer a single return value. Return a table when returning multiple properties to prevent truncation during constructor evaluation.
- **Explicit Nil Handling:** Distinguish between uninitialized keys and values explicitly set to `nil`. Use sentinel boolean values (`false`) for cached empty states.
- **Error Handling:** Use `pcall` or `xpcall` for protected execution. Return `nil, err_msg` from fallible helper functions and evaluate the primary return value. Report user-facing failures via `vim.notify`.
- **LuaJIT Optimization:** Use numeric `for` loops in hot paths, maintain monomorphic function call sites, and avoid allocating closures inside loop iterations.

## Neovim Plugin Architecture

- **Layered Module Structure:** Enforce inward dependency flow: `views/ -> render/ -> git/ -> infra/`. Structure `init.lua` as a thin facade (`setup`, `open*`), allowing internal modules to require peer modules directly. Isolate shared mutable state in a dependency-free session module.
- **Configuration Management:** Structure `setup(opts)` to merge user options via `vim.tbl_deep_extend("force", defaults, opts)`. Drive keymaps through data-driven command specifications rather than branching on view types.
- **Asynchronous Process Execution:** Use `vim.system({ ... }, { text = true, stdout = true, stderr = true }, callback)` for external Git and CLI processes. Never invoke synchronous `vim.fn.system()` or `vim.system():wait()` in render loops, keymap handlers, autocmds, or cursor events. Schedule UI updates with `vim.schedule()`. Verify tool presence with `vim.fn.executable("cmd") == 1`.
- **Failure Surface Propagation:** Route nonzero process exits, invalid JSON payloads, missing context, and aborted operations through `vim.notify` with full error text. Distinguish failed requests from empty result sets in logic and test assertions.
- **Tree-sitter Parsing:** Parse syntax trees asynchronously via `LanguageTree:parse(range, on_parse)`. Render initial text before parsing completes and upgrade highlighting in the callback.
- **Buffer Rendering and Viewports:** Store logical lines as actual buffer text to preserve native search, marks, undo history, and folding. Apply syntax highlighting lazily to the active viewport using `nvim_set_decoration_provider`.
- **Window Option Scoping:** Target window options explicitly using `vim.wo[win_id]` rather than relying on current window defaults (`vim.wo[0]`).
- **Cursor Lifecycle in Focused TUIs:** Explicitly capture `vim.o.guicursor` when entering modal buffers, set a hidden cursor during focus, and restore the original cursor state on exit.
- **Buffer Initialization:** Use `vim.cmd.edit` when editor plugins (such as LSP or gitsigns) must attach, as `nvim_win_set_buf` does not trigger `BufRead` or `FileType` autocmds.
- **Display Re-entry:** Bind buffer display setup to `BufEnter` or `BufWinEnter` events so configuration re-applies across window switches and session restoration.

## Live Editor RPC Verification

Verify plugin behavior against running Neovim instances over the `$NVIM` socket:

```bash
# Reload module in running instance and execute test command
nvim --server "$NVIM" --remote-expr 'execute("lua package.loaded[\"myplugin\"]=nil; require(\"myplugin\")")'
nvim --server "$NVIM" --remote-expr 'execute("MyCommand")'
```

---

## Reference Map

| Reference File | Purpose |
| --- | --- |
| **`references/architecture.md`** | Module stratification, dependency layers, require patterns, and circular dependency resolution. |
| **`references/configuration.md`** | Configuration schemas, keymap tables, command integration, and lazy.nvim specifications. |
| **`references/rendering-and-highlights.md`** | Viewport decoration providers, large buffer rendering, syntax highlighting, and folding mechanics. |
| **`references/async-and-git.md`** | Asynchronous Git/CLI execution, diff parsing, hunk staging, and Tree-sitter integration. |
| **`references/trouble-and-snacks.md`** | Trouble.nvim v3 custom sources and Snacks.nvim diff rendering integration. |
| **`references/common-bugs.md`** | Defect catalog covering fold collapse, cursor jump regressions, modal cursor leakage, and partial buffer rendering. |
| **`references/live-nvim-rpc.md`** | `$NVIM` socket communication, RPC expressions, state queries, and safety rules. |

Use the **`terminal-tui-debugging`** skill when validating cursor positions or terminal cell grids.
