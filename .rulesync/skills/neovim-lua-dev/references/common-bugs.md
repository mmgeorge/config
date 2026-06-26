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

## The 13 bugs

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
local id = node.id:gsub("#Untracked Files#", "#" .. category .. "#")
                  :gsub("#Tracked Changes#",  "#" .. category .. "#")
view.renderer._folded[id] = true          -- before view:refresh()
```

Seed only the *destination* id, never the current one — a file that stays in its
category (unstaging a *modified*, not new, file) then keeps the fold state the
user chose. This is also how "untracked files are never expanded" survives a
stage→unstage round-trip.
For the cursor, capture the *next* file **before** the action (its row moves
once the staged file changes category), then after the single refresh find it by
identity — scan `view.renderer._locations` for the file-level group node
(`node.group.fields[1] == "filename"`, matching `node.item.filename`) and put
the cursor on its row. Identity beats `cursor + 1`: when staging creates a brand
new category header, `+1` lands on the header, not the next file.

### 11. Async Context Rerenders Steal the Cursor

**Bug**: A status/list action moves the cursor correctly, then a moment later it
jumps back to an earlier hunk or the top of the buffer.

**Cause**: A delayed enrichment callback, such as Tree-sitter context or syntax
highlighting, rerenders the list and passes the callback's item id as the
cursor target. If that callback belongs to a different hunk than the one the
user is now on, the late render steals the cursor. Renders with no explicit
target are also dangerous if the generic fallback is line 1.

**Fix**: Treat cursor restore as two different flows:

- **Passive async rerender:** no explicit target means "preserve wherever the
  user is now." Capture the stable item id plus raw cursor line immediately
  before mutating buffer lines. Do not capture when the async request starts;
  the user may move while Git, Tree-sitter, or syntax work is in flight.
- **Action rerender:** pass an explicit semantic target chosen by the action,
  such as the next hunk after stage/unstage/discard or the destination
  section/file header after a section-level action.

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

**Cause**: The first async git mutation finishes and immediately starts a full
backend reconcile before the second keypress has been processed or queued. That
backend snapshot is technically current for only the first mutation, so it
repaints over the optimistic UI until the second action renders.

**Fix**: Keep index mutations sequential, but debounce backend reconciliation
after the queue becomes idle. Enqueuing another mutation must cancel the pending
reconcile. While mutations are running or queued, suppress unrelated full status
loads and async enrichment rerenders; the final debounced reconcile refreshes
from Git once the burst has settled.

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
