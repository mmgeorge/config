# Common DiffReview Plugin Bugs (Already Hit)

*When to read this:* scan this checklist the moment a symptom matches — preview won't let you edit, folds re-collapse on stage, the cursor jumps back after an action, a buffer half-renders after a window switch, or Windows prints a stdio error on status open. Each entry is a real bug from `diff_review` with its fix.

For the surrounding subsystem patterns see trouble-and-snacks.md (Trouble sources, Snacks diff renderer), async-and-git.md (git integration, folds), and rendering-and-highlights.md. Read this file first to identify the bug by symptom, then jump to the named pattern section for context.

## Symptom index

| Symptom you see | Entry |
| --- | --- |
| Preview updates skip / two items "share" position | #1 |
| Can't type into the preview buffer | #2 |
| Preview closes the instant you focus it | #3 |
| `git diff` looks stale right after `git apply` | #4 |
| Staging one hunk re-collapses every open file | #5 |
| Number column / stale content reappears after window switch | #6 |
| Single-key map (`S`) waits for a second key | #7 |
| Staged hunk jumps to the bottom of the file diff | #8 |
| `E95: Buffer with this name already exists` | #9 |
| File staged from Untracked renders expanded | #10 |
| Cursor jumps back a moment after an action | #11 |
| Rapid `S S` flickers through an intermediate state | #12 |
| `dtable::stdio_init: couldn't make stderr distinct...` on Windows | #13 |
| Cursor stays hidden after a modal, or remains visible inside it | #14 |

## The 14 bugs

### 1. Shared Table References

**Bug**: Two items share the same `pos` table. Snacks preview uses reference
equality (`self.pos == item.pos`) to skip redundant updates, so the second item
never previews.

**Fix**: Copy the table, never alias it.

```lua
-- BAD: shares reference
file_item.pos = first_hunk.pos

-- GOOD: copy
file_item.pos = { first_hunk.pos[1], first_hunk.pos[2] }
```

### 2. Trouble Preview Overlay vs Real Buffer

**Bug**: Trying to edit in the preview overlay (a read-only scratch buffer).

**Fix**: Don't use `focus_preview`. Close/hide the picker and open the real
file with `vim.cmd.edit()`.

### 3. WinLeave Closing Preview

**Bug**: Focusing the preview window triggers `WinLeave` on Trouble, which
closes the preview immediately.

**Fix**: Don't try to focus the preview overlay. Either:

- Use the preview for read-only browsing only
- Set buffers on the main window directly (bypass the preview system)

### 4. Stale Data After vim.fn.system

**Bug**: Running `git apply` then immediately reading `git diff` returns
stale data.

**Fix**: `vim.fn.system` is synchronous, so git state IS updated. The
issue is usually that you're reading cached data instead of re-running git.

### 5. Re-applying Fold Level After Refresh Collapses Open Nodes

**Bug**: Calling `view:fold_level({ level = 1 })` after `view:refresh()` (to
"restore" folds) actually *destroys* the user's manual fold state — staging one
hunk re-collapses every file they had expanded.

**Fix**: Don't. The renderer persists `_folded` (keyed by stable `node.id`)
across a refresh, so expanded/collapsed state survives on its own. Set the fold
level only once, in `first_render`:

```lua
view.first_render:next(function()
  view:fold_level({ level = 1 })  -- initial default; never re-apply on refresh
end)
```

### 6. Per-Display State Tied to One Code Path, Not the Buffer

**Bug**: Whenever a buffer is shown it needs some setup — re-render stale
content, hide the window's `number` column (the Snacks renderer draws its own),
set window-local options, etc. It's tempting to do that setup in the *one*
function you wrote to display the buffer (e.g. `auto_preview`). But a buffer
gets (re-)entered and (re-)displayed through **many** paths you don't control:

- `auto_preview` / `nvim_win_set_buf` (your preview code)
- a window-switch keybind back from the real file (`<C-w>` motions)
- `:b`/`:buffer`, `:bnext`, the buffer picker, session restore
- another plugin or the user splitting/reopening the window

Set up via one path and every other path leaves the buffer half-configured —
e.g. the number column reappears after switching back from the real file.

**Fix**: Attach the setup to the **buffer**, not the call site. Use a
`BufEnter` (focus) and/or `BufWinEnter` (display) autocmd scoped to the buffer
so it re-applies no matter how you got there. Make the callback idempotent and
capture any to-be-restored state once:

```lua
vim.api.nvim_create_autocmd("BufEnter", {
  buffer = buf,
  callback = function()
    M._refresh_diff_buffer(buf, filename)            -- re-render stale content
    M._hide_line_numbers(vim.api.nvim_get_current_win()) -- re-hide number column
  end,
})
```

Caveat: `BufEnter` only fires when the buffer is **focused**. For a buffer
shown unfocused in another window (a preview), `BufEnter` won't fire there —
still call the setup directly at your display site (`auto_preview`) as well, so
both the focused and previewed cases are covered. The principle holds: don't
assume a single entry path.

### 7. nowait for Single-Key Mappings

**Bug**: Pressing `S` waits for a second keypress because another mapping
starts with `S` (e.g., `Sa`).

**Fix**: Add `nowait = true` to the keymap options.

### 8. Hunks Jumping on Stage (Unstaged-then-Staged Grouping)

**Bug**: `git diff` and `git diff --cached` are fetched separately, so naively
concatenating them (`all_hunks = unstaged ++ staged`) groups every unstaged
hunk before every staged hunk. Staging a hunk then moves it to the **end** of
the file's diff buffer instead of leaving it in place — the list visibly
re-sorts on every stage.

**Fix**: Order each file's hunks by line position (`pos`) before building the
per-file diff, so a staged hunk keeps its place and is merely folded (via the
`staged_flags` auto-fold). The combined diff is built in three places
(`M.get`, `refresh_open_diff_buffer`, `_update_file_diff_cache`) — all route
through the shared `order_file_hunks` helper so they can't drift apart.

### 9. Scratch Buffer Names Collide (E95)

**Bug**: Naming per-file scratch buffers by basename
(`nvim_buf_set_name(buf, "diff://" .. fnamemodify(f, ":t"))`) throws
`E95: Buffer with this name already exists` the moment two files share a
basename (`a/config.lua` and `b/config.lua`). It surfaces as an unhandled
promise rejection because the display runs inside Trouble's promise chain.

**Fix**: `pcall` the name and fall back to a unique suffix:

```lua
local name = "diff://" .. vim.fn.fnamemodify(filename, ":t")
if not pcall(vim.api.nvim_buf_set_name, buf, name) then
  pcall(vim.api.nvim_buf_set_name, buf, name .. "#" .. buf)
end
```

The buffer is keyed by full path internally, so the name is only cosmetic —
never let it crash the preview.

### 10. Fold State Lost When an Item Changes Group

**Bug**: A file staged from the "Untracked Files" category jumps to "Tracked
Changes" and renders **expanded**, even though every other file is collapsed.

**Cause**: Trouble keys fold state (`renderer._folded`) by `node.id`, and the id
encodes the **group path** — `…#Untracked Files#<file>` vs
`…#Tracked Changes#<file>`. Staging changes the file's category, so it becomes a
*different node* that never inherits the old folded state. (Bug #5 says fold
state survives a refresh — true, but only while the id is stable.)

**Fix**: **Pre-seed** the fold before the refresh, don't fold after. Folding
after the refresh (`view:fold` → `view:render`) is a *second* render on top of
the refresh's render — visible flicker, and the row math shifts under any cursor
target you computed. Instead, since the node id is just a string, rewrite its
category segment to the destination and seed the fold table directly, so the
refresh renders it collapsed in one pass:

```lua
-- direction depends on the action: stage -> "Tracked Changes",
-- unstage of a new file -> "Untracked Files"
local destination_id = node.id:gsub("#Untracked Files#", "#" .. category .. "#")
                              :gsub("#Tracked Changes#",  "#" .. category .. "#")
view.renderer._folded[destination_id] = true
```

Seed only the *destination* id, never the current one — a file that stays in its
category (unstaging a *modified*, not new, file) then keeps the fold state the
user chose. This is also how "untracked files are never expanded" survives a
stage→unstage round-trip. Do not pair that fold projection with an action-owned
cursor target. Apply the smallest buffer edit and let Neovim retain the cursor
without a second render or explicit move.

### 11. Async Context Rerenders Steal the Cursor

**Bug**: A stage or unstage appears immediately, then a later callback jumps the
cursor to an earlier hunk or the top of the buffer.

**Cause**: A delayed enrichment callback, such as Tree-sitter context or syntax
highlighting, rerenders the list and passes a stale item id as the cursor target.
The same jump occurs when stage or unstage routes its later authoritative sync
through generic cursor restoration. A full-buffer rewrite compounds the visible
movement even when only a few rows changed.

**Fix**: Give each render source an explicit cursor policy:

- **Passive async rerender:** no explicit target means "preserve wherever the
  user is now." Capture the stable item id plus raw cursor line immediately
  before mutating buffer lines. Do not capture when the async request starts;
  the user may move while Git, Tree-sitter, or syntax work is in flight.
- **Stage/unstage render:** never restore or explicitly move the cursor during
  optimistic, corrective, or failure-recovery renders. Let Neovim place it as
  the affected lines move.
- **Discard rerender:** pass an explicit semantic target chosen before deletion
  because the removed entry cannot preserve its identity.

Starting a stage or unstage must also invalidate any in-flight full status load
and pending enrichment generation. Otherwise a callback launched against the
old section model can finish after the mutation and reintroduce its stale target.

When correction changes the status text, apply `vim.diff` index hunks from
bottom to top instead of replacing the full buffer. This keeps unchanged rows
out of the mutation surface.

```lua
-- BAD: captures the target before async work, targets the completed item
local target_id, fallback_line = cursor_target(buf)
load_async(function(result)
  render_loaded(buf, result, target_id, fallback_line)
end)

-- GOOD: capture at render time, preserve wherever the user is now
load_async(function(result)
  local target_id, fallback_line = cursor_target(buf)
  render_loaded(buf, result, target_id, fallback_line)
end)
```

### 12. Rapid Actions Repaint Intermediate Backend State

**Bug**: Pressing an action key repeatedly, such as staging two hunks with `S S`,
briefly flickers through an intermediate state even though optimistic UI updates
work.

**Cause**: The first async Git mutation finishes and starts a full repository
refresh. That intermediate snapshot can describe only the completed prefix, so
it replaces newer optimistic state and writes every status row again.

**Fix**: Store a confirmed baseline plus ordered optimistic journal layers, and
render each new layer immediately. Serialize index mutations through one FIFO
per repository root. After the FIFO drains, wait for a 120 ms quiet window and
run exactly one path-scoped snapshot for the burst's affected paths:

- porcelain-v2 status with NUL records and all untracked files
- zero-context unstaged diff
- zero-context staged diff

When the snapshot semantically matches the projection, retire the resolved
layers without rendering the status buffer or writing an open diff buffer. On a
real mismatch, replace only those paths and render once. Replay later optimistic
layers over the confirmed snapshot so a sync can never erase a newer keypress.

If one three-command snapshot attempt fails, retry it once after 120 ms while the
projection stays visible. Do not notify or render between attempts. Mark
verification stale only after the retry fails too.

If one Git mutation fails, notify immediately and cancel the queued tasks from
that burst. Preserve the Git writes that already completed, then use one path
snapshot attempt, with the same single retry on read failure, and one forced
recovery render to show actual truth. A batch can
partially succeed, so pretending to roll back every completed action creates a
second lie instead of recovery.

### 13. Windows MSYS/Cygwin Stdio Leak On Status Startup

**Bug**: Opening a status buffer on Windows prints a message like
`dtable::stdio_init: couldn't make stderr distinct from stdout, Win32 error 6`.

**Cause**: A startup-time child process was spawned with implicit stdio handling
(`vim.system(..., { text = true })`). This often hides in adjacent metadata
loaders, not the row renderer itself.

**Fix**: Audit every subprocess started by the buffer open path, including
plugin-spec setup and executable probes. Replace `os.execute("which ...")` with
`vim.fn.executable(...)`; for real child processes use explicit capture
everywhere:

```lua
vim.system(command, {
  text = true,
  stdout = true,
  stderr = true,
  stdin = input,
  cwd = cwd,
}, callback)
```

Preserve combined `stdout` and `stderr` in wrapper `output` fields so
notifications keep the actionable error text. Add or update a regression test
that checks startup commands request `stdout == true` and `stderr == true`.

### 14. A Focused Modal Leaks Or Fails To Hide The Terminal Cursor

**Bug**: A navigation-only modal either shows a block cursor that should remain
invisible, or closing the modal leaves the cursor hidden in the editor.

**Cause**: Neovim's `Cursor` highlight controls the cell styling but does not
reliably hide the terminal's focused cursor. `guicursor` drives terminal cursor
shape and visibility globally. Setting it to a hidden blend group works while
the modal owns focus, but restoring an empty option can leave the terminal in
that last explicit hidden state because no visible cursor transition gets sent.

**Fix**: Treat cursor visibility as a paired modal lifecycle. Capture the global
configuration before opening, set an explicit hidden cursor while the modal
owns focus, and restore an explicit visible fallback when the captured value
was empty. Restore before focusing any editable child and on every close path.

```lua
local function set_modal_cursor_hidden(modal, hidden)
  local visible = modal.guicursor ~= "" and modal.guicursor or "a:block-Cursor"
  vim.o.guicursor = hidden and "n:block-ModalHiddenCursor" or visible
end
```

Keep the window-local `Cursor:ModalHiddenCursor` mapping as presentation state,
but do not rely on it as the terminal visibility mechanism. Verify the complete
sequence in a fresh PTY: cursor visible in the editor, hidden while modal focus
owns the terminal cell, then visible again after close and inside editable input.
