# Known Failure Modes and Remedies in Neovim Plugins

This reference catalogs recurring bugs, race conditions, rendering defects, and platform-specific failures encountered in Neovim plugin development alongside their architectural fixes.

## Failure Mode Directory

| Symptom | Diagnostic Category | Reference Item |
| --- | --- | --- |
| Preview updates skip or multiple items share position | Table Reference Sharing | 1 |
| Cannot type into preview buffer | Scratch Overlay vs Real Buffer | 2 |
| Preview closes immediately upon focus | Focus Loss in Preview Floating Window | 3 |
| Git diff appears stale after index mutation | Caching vs Asynchronous Execution | 4 |
| Staging one hunk collapses all open file nodes | Inappropriate Fold Reset on Refresh | 5 |
| Number column or stale state reappears after window switch | Call-Site vs Buffer-Scoped Lifecycle | 6 |
| Single-key mapping delays waiting for subsequent key | Keymap Collision (`nowait` omission) | 7 |
| Staged hunk jumps to end of buffer | Naive Concatenation of Diff Streams | 8 |
| `E95: Buffer with this name already exists` | Unhandled Scratch Buffer Name Collisions | 9 |
| Staged file from Untracked renders expanded | Category ID Mismatch in Fold Tracking | 10 |
| Cursor jumps unexpectedly after asynchronous completion | Stale Target Restoration | 11 |
| Rapid mutations flicker intermediate backend states | Uncoordinated Burst Refresh | 12 |
| `dtable::stdio_init: couldn't make stderr distinct...` on Windows | Implicit Standard Stream Descriptors | 13 |
| Modal leaks or fails to restore terminal cursor | Global `guicursor` Lifecycle Mishandling | 14 |

---

## Failure Modes and Solutions

### 1. Table Reference Sharing

- **Defect:** Two distinct UI items share the same mutable `pos` table reference. Renderers that check reference equality (`self.pos == item.pos`) skip updates for the second item.
- **Remedy:** Clone tables explicitly rather than aliasing references.

```lua
-- Bad: Shares table reference
file_item.pos = first_hunk.pos

-- Good: Creates a shallow copy
file_item.pos = { first_hunk.pos[1], first_hunk.pos[2] }
```

### 2. Scratch Overlay vs Real Buffer

- **Defect:** Attempting to direct user edits into a read-only scratch buffer or preview floating window.
- **Remedy:** Keep preview buffers strictly read-only. When the user requests an edit action, close the preview floating window and open the real file via `vim.cmd.edit()`.

### 3. Focus Loss in Preview Floating Window

- **Defect:** Focusing a preview window triggers `WinLeave` events on the source buffer, causing the preview window to close automatically.
- **Remedy:** Keep preview overlays non-focusable for browsing. Direct all interactive navigation through the main window.

### 4. Caching vs Asynchronous Execution

- **Defect:** Reading diff data immediately after `git apply` returns stale lines.
- **Remedy:** Ensure the reader re-executes Git commands rather than reading an in-memory cache populated before the mutation.

### 5. Inappropriate Fold Reset on Refresh

- **Defect:** Calling `view:fold_level({ level = 1 })` on every refresh resets manual fold expansions configured by the user.
- **Remedy:** Apply default fold levels once during initial buffer initialization (`first_render`). Preserve manual fold states across refreshes by keying fold models with stable node IDs.

### 6. Call-Site vs Buffer-Scoped Lifecycle

- **Defect:** Configuring window-local settings (such as hiding line numbers or setting buffer options) only within the primary display function. Switching back to the buffer via `<C-w>` or `:b` leaves the buffer unconfigured.
- **Remedy:** Attach buffer configuration to buffer-scoped autocommands (`BufEnter` and `BufWinEnter`) rather than specific invocation functions. Ensure configuration callbacks are idempotent.

```lua
vim.api.nvim_create_autocmd("BufEnter", {
  buffer = buf,
  callback = function()
    M._refresh_diff_buffer(buf, filename)
    M._hide_line_numbers(vim.api.nvim_get_current_win())
  end,
})
```

### 7. Keymap Collision (`nowait` Omission)

- **Defect:** Pressing a single-key shortcut (such as `S`) waits for timeout because another mapping shares the prefix.
- **Remedy:** Set `nowait = true` on single-key buffer keymaps.

### 8. Naive Concatenation of Diff Streams

- **Defect:** Appending staged hunks directly after unstaged hunks (`all_hunks = unstaged .. staged`) causes a newly staged hunk to jump to the bottom of the file view instead of remaining in line order.
- **Remedy:** Sort the combined collection of hunks by file line index (`pos`) before constructing the buffer model.

### 9. Unhandled Scratch Buffer Name Collisions

- **Defect:** Naming per-file scratch buffers with base names (`diff://filename.lua`) triggers `E95: Buffer with this name already exists` when multiple files share a name in different directories.
- **Remedy:** Wrap `nvim_buf_set_name` in `pcall` and append unique buffer ID suffixes upon collision.

```lua
local name = "diff://" .. vim.fn.fnamemodify(filename, ":t")
if not pcall(vim.api.nvim_buf_set_name, buf, name) then
  pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
end
```

### 10. Category ID Mismatch in Fold Tracking

- **Defect:** Staging an untracked file moves it from "Untracked Files" to "Tracked Changes" and renders it expanded, ignoring the default collapsed state.
- **Remedy:** Pre-seed the fold state for the target node ID before running the refresh pass rather than folding after rendering.

```lua
local destination_id = node.id:gsub("#Untracked Files#", "#" .. category .. "#")
                              :gsub("#Tracked Changes#", "#" .. category .. "#")
view.renderer._folded[destination_id] = true
```

### 11. Stale Target Restoration

- **Defect:** Background syntax highlighting or Tree-sitter callbacks complete after a stage/unstage action and reposition the cursor to an outdated line.
- **Remedy:** Enforce distinct cursor policies per render type:
  - **Passive Rerenders:** Capture the active item ID and line index immediately before applying line updates.
  - **Stage and Unstage:** Disable explicit cursor relocation and let Neovim maintain position relative to minimal line diffs.
  - **Discard:** Provide an explicit replacement target determined prior to deletion.

### 12. Uncoordinated Burst Refresh

- **Defect:** Rapidly pressing action keys (such as `S S`) causes UI flicker because the first completed Git command triggers an authoritative reload that overwrites newer optimistic layers.
- **Remedy:** Maintain an optimistic journal baseline and queue mutations through a FIFO. Collect snapshot updates only after a quiet window (120 ms) expires, and reconcile against the union of modified paths.

### 13. Implicit Standard Stream Descriptors

- **Defect:** Spawning child processes on Windows with implicit stdio produces `dtable::stdio_init: couldn't make stderr distinct from stdout, Win32 error 6`.
- **Remedy:** Provide explicit `stdin`, `stdout`, and `stderr` descriptors for all `vim.system` invocations. Use `vim.fn.executable(...)` rather than shell probes like `which`.

### 14. Global `guicursor` Lifecycle Mishandling

- **Defect:** Modal floating windows fail to hide the terminal cursor or leave the editor cursor invisible after closing.
- **Remedy:** Capture global `guicursor` settings before opening modals, set an explicit hidden cursor highlight while the modal has focus, and restore the original settings upon modal teardown.

```lua
local function set_modal_cursor_hidden(modal, hidden)
  local visible = modal.guicursor ~= "" and modal.guicursor or "a:block-Cursor"
  vim.o.guicursor = hidden and "n:block-ModalHiddenCursor" or visible
end
```
