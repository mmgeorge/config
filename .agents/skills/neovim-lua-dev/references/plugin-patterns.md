# Reference: Neovim Plugin Patterns (Trouble / Snacks / git UIs)

Reference material for the **neovim-lua-dev** skill. Hard-won knowledge from
building the DiffReview plugin (a custom Trouble source with a fancy diff
buffer): Snacks.nvim and Trouble.nvim internals, window/fold/highlight pitfalls,
git integration (including driving `git commit` from inside nvim), and a catalog
of concrete bugs with fixes. The patterns generalise to most Neovim UI plugins —
read the relevant section before touching that subsystem.

---

## Table of Contents

1. [Trouble.nvim v3 Custom Sources](#troublenvim-v3-custom-sources)
2. [Snacks.nvim Diff Renderer](#snacksnvim-diff-renderer)
3. [Window & Buffer Management Pitfalls](#window--buffer-management-pitfalls)
4. [Neovim Fold System](#neovim-fold-system)
5. [Git Integration Patterns](#git-integration-patterns)
6. [Highlight & Virtual Text](#highlight--virtual-text)
7. [Common Bugs & How to Avoid Them](#common-bugs--how-to-avoid-them)

---

## Trouble.nvim v3 Custom Sources

### Source File Location

Sources live in `lua/trouble/sources/<name>.lua` anywhere in the runtimepath.
Trouble auto-discovers them. The module must return a table with:

```lua
local M = {}
M.config = { modes = { ... } }  -- registers modes
M.setup = function() end        -- called once on load
M.get = function(cb, ctx) end   -- fetches items
return M
```

### Item Structure

Create items via `require("trouble.item").new()`:

```lua
Item.new({
  source = "my_source",
  filename = "/absolute/path/to/file.lua",  -- MUST be absolute
  pos = { line, col },                       -- 1-based line, 0-based col
  item = {
    -- Custom fields accessible as {item.field} in format strings
    check = "[ ]",
    category = "Tracked Changes",
  },
})
```

### Groups & Hierarchy

Groups create collapsible tree levels in the Trouble list:

```lua
groups = {
  { "item.category" },                                            -- level 1
  { "filename", format = "{file_icon} {basename} {item.stats}" }, -- level 2
},
```

Items are grouped by matching field values. The format string for a group uses
fields from the **first item** in that group.

### Actions & Context

Actions receive `(view, ctx)` where:

```lua
ctx.item  -- the trouble.Item at cursor (nil for group headers!)
ctx.node  -- the tree node at cursor (always set)
ctx.opts  -- action options
```

**Critical**: `ctx.item` is `nil` on group header rows. To distinguish
group headers from items, check `ctx.node.group ~= nil`:

```lua
local is_group = ctx.node and ctx.node.group ~= nil
if is_group then
  -- On a file/category group header
  -- Access the first item via ctx.node.item
else
  -- On an actual item row
end
```

**`ctx.node.item` is only the group's FIRST item.** On a top-level header like
"Tracked Changes" that's just the first file's first hunk — an action that reads
`ctx.node.item.filename` silently operates on one file instead of the whole
group. To act on everything under a header, walk the subtree to the file-level
nodes (those whose `node.group.fields[1] == "filename"`) and loop:

```lua
-- the node itself if it's a file group, else every file group beneath it
local function file_group_nodes(node)
  if node.group and node.group.fields and node.group.fields[1] == "filename" then
    return { node }
  end
  local out = {}
  local function walk(n)
    if n.group and n.group.fields and n.group.fields[1] == "filename" then
      out[#out + 1] = n
    else
      for _, c in ipairs(n.children or {}) do walk(c) end
    end
  end
  walk(node)
  return out
end
```

### Movement: next/prev vs Raw Cursor

Trouble's built-in `next`/`prev` actions **skip group headers** (they only
land on item rows). If you need the cursor to reach group headers (e.g., for
staging entire files), use raw cursor movement:

```lua
s = {
  action = function(self)
    local cursor = vim.api.nvim_win_get_cursor(self.win.win)
    if cursor[1] > 1 then
      vim.api.nvim_win_set_cursor(self.win.win, { cursor[1] - 1, cursor[2] })
    end
  end,
},
```

### Fold Level

Trouble hardcodes `foldlevel = 99` via `Window.FOLDS`, applied with
`vim.tbl_deep_extend("force", ...)` AFTER user config. You cannot override it
via `win.wo.foldlevel`. Instead, set fold level after the first render:

```lua
view.first_render:next(function()
  view:fold_level({ level = 1 })  -- 0 = all collapsed, 1 = first level expanded
end)
```

### Preview System (preview = "main")

When `preview.type = "main"`, Trouble creates a **floating overlay window**
on top of the main editor window. Key implications:

- `ctx.win` in preview functions is the overlay, NOT the main window
- `line_hl_group` extmarks are invisible under overlays (`hl_mode = "replace"`)
- Focusing the overlay triggers `WinLeave` on Trouble, which closes the preview
- `preview.scratch = false` uses real buffers (LSP attaches) but still overlays

**If you need the user to edit/interact with the preview content, DON'T use
the preview system.** Instead, set buffers on the main window directly and
manage the window yourself.

### Refreshing After Actions

`view:refresh()` re-runs `M.get()` and re-renders. But:

- `view:at()` returns stale data until render completes
- Cursor position may shift; use `vim.defer_fn` for post-refresh operations

```lua
view:refresh()
vim.defer_fn(function()
  -- Now view:at() has fresh data; move the cursor / update preview here
end, 50)
```

**Do NOT call `view:fold_level()` after a refresh.** The renderer keeps its
per-node fold state (`renderer._folded`, keyed by stable `node.id`) across a
refresh — `clear()` and `render()` never touch it, and the window's
`foldlevel = 99` is only applied once at mount. Calling `fold_level()` after a
refresh recomputes folds purely from depth, discarding whatever the user
manually expanded/collapsed (e.g. staging one hunk would re-collapse every open
file). Set the initial fold level **once** in the `first_render` callback; never
again.

### Main Window Detection

Trouble's `view:main()` finds the main editor window. It rejects windows
showing buffers with `buftype ~= ""`. If you set a scratch/nofile buffer on
the main window, `view:main()` will return the WRONG window (falls back to
current window, which might be Trouble itself).

**Solution**: Cache the main window ID on first call before setting any
scratch buffers:

```lua
function M.get_main_win(view)
  if not M._main_win or not vim.api.nvim_win_is_valid(M._main_win) then
    local main = view:main()
    if main then M._main_win = main.win end
  end
  return M._main_win
end
```

### Winbar for Key Hints

Use `vim.wo[view.win.win].winbar` for non-selectable header text:

```lua
vim.wo[view.win.win].winbar = " %#Comment#S%* stage | %#Comment#q%* close"
```

`%#HlGroup#text%*` applies highlight groups. `%=` right-aligns.

---

## Snacks.nvim Diff Renderer

### Using the Fancy Diff Renderer

```lua
local snacks_diff = require("snacks.picker.util.diff")
local H = Snacks.picker.highlight

local diff = snacks_diff.get_diff(diff_text)  -- parse diff string
local ret = snacks_diff.format(diff)          -- format into highlight lines
H.render(buf, ns, ret)                        -- render to buffer
```

### Customizing Rendered Output

`format_hunk` returns highlight lines per hunk. To skip file/hunk headers:

```lua
local opts = { hunk_header = false }
-- Build a context object that mimics Snacks' internal ctx
local ctx_base = setmetatable({ diff = diff, opts = opts }, { __index = function(_, k)
  if k == "extend" then
    return function(self2, t) return setmetatable(t, { __index = self2 }) end
  end
end })

for _, block in ipairs(diff.blocks) do
  local block_ctx = ctx_base:extend({ block = block })
  for _, hunk in ipairs(block.hunks) do
    local hunk_ctx = block_ctx:extend({ hunk = hunk })
    local lines = snacks_diff.format_hunk(hunk_ctx)
    vim.list_extend(ret, lines)
  end
end
```

### Replacing Highlight Groups

Snacks applies `SnacksDiffAdd`, `SnacksDiffDelete`, etc. to virtual text with
`hl_mode = "replace"`. To change colors, you must replace the highlight groups
**in the format output before rendering**, not via extmarks after:

```lua
local hl_replacements = {
  SnacksDiffAdd = "MyAddBg",
  SnacksDiffDelete = "MyDeleteBg",
}

local function replace_hl(h)
  if type(h) == "string" then return hl_replacements[h] or h end
  if type(h) == "table" then
    local new = {}
    for i, v in ipairs(h) do new[i] = replace_hl(v) end
    return new
  end
  return h
end

-- Walk each line's entries and replace hl groups
for _, line in ipairs(ret) do
  for _, entry in ipairs(line) do
    if type(entry[1]) == "string" and entry[2] then
      entry[2] = replace_hl(entry[2])
    end
    if entry.virt_text then
      for _, vt in ipairs(entry.virt_text) do
        if vt[2] then vt[2] = replace_hl(vt[2]) end
      end
    end
  end
end
```

**Why extmarks don't work**: Snacks renders code as virtual text overlays with
`hl_mode = "replace"`, which completely overrides any underlying `line_hl_group`
or `hl_group` extmarks. The ONLY way to change the appearance is to modify the
virtual text highlight groups themselves.

### Hunk Line Counting

Snacks' `parse_hunk` strips trailing empty lines from diff hunks. When
computing line ranges for a rendered hunk, you must match this behavior:

```lua
local code_lines_list = {}
local found_hunk_header = false
for diff_line in h.diff:gmatch("[^\n]+") do
  if found_hunk_header then
    code_lines_list[#code_lines_list + 1] = diff_line
  elseif diff_line:match("^@@") then
    found_hunk_header = true
  end
end
-- Strip trailing empty lines (matching Snacks parse_hunk behavior)
while #code_lines_list > 0 and code_lines_list[#code_lines_list]:match("^%s*$") do
  table.remove(code_lines_list)
end
local code_lines = #code_lines_list
```

---

## Window & Buffer Management Pitfalls

### vim.wo[0] vs vim.wo[win]

`vim.wo[0]` sets window options on the **current window**, not necessarily the
window you think. When autocmds fire (e.g., `BufWinEnter`), the current window
might be different from the window that triggered the event.

**Always use `vim.wo[win_id]`** with an explicit window ID:

```lua
-- BAD: might set options on the wrong window
vim.wo[0].foldmethod = "manual"

-- GOOD: explicit window
vim.wo[win].foldmethod = "manual"
```

### nvim_win_set_buf vs vim.cmd.edit

- `nvim_win_set_buf(win, buf)`: Low-level. Does NOT trigger `BufRead`,
  `BufEnter`, `FileType` autocmds. LSP, mini.diff, gitsigns won't attach.
- `vim.cmd.edit(path)`: Triggers all normal autocmds. Use when you need
  plugins to attach to the buffer.

For preview/temporary display, `nvim_win_set_buf` is fine. For "jump to real
file for editing", use `vim.cmd.edit`.

### Buffer Cleanup

When creating scratch buffers (`buftype=nofile`), set `bufhidden = "hide"`
(not `"wipe"`) if you plan to reuse them. `"wipe"` deletes the buffer when
it's hidden, which causes errors if anything references it later.

Clean up explicitly when done:

```lua
vim.api.nvim_buf_delete(buf, { force = true })
```

---

## Neovim Fold System

### Manual Folds on Non-Current Window

To create folds on a window you're not in, use `nvim_win_call`:

```lua
vim.api.nvim_win_call(win, function()
  vim.cmd("10,20fold")
end)
```

**Always save/restore view** to prevent scroll jumping:

```lua
vim.api.nvim_win_call(win, function()
  local view = vim.fn.winsaveview()
  vim.cmd("normal! zE")  -- delete all folds
  vim.cmd("10,20fold")
  vim.fn.winrestview(view)
end)
```

### Fold Settings Must Be On The Right Window

Folds require `foldmethod=manual` and `foldenable=true` on the **window**
(not buffer) showing the content. Set these with `vim.wo[win]` before
creating folds.

---

## Git Integration Patterns

### Async Tree-sitter in UI Plugins

Tree-sitter parsing can be expensive enough to jank status buffers and previews.
When parse work is needed during rendering, use `LanguageTree:parse(range,
on_parse)` instead of synchronous `parser:parse()`. Render a cheap fallback
first, cache a pending sentinel, and repaint only when the async parse callback
fills the cache.

```lua
local request_id = state.request_id
parser:parse({ row, 0, row + 1, 0 }, function(first, second)
  local trees = type(first) == "table" and first or second
  vim.schedule(function()
    if state.request_id ~= request_id then return end
    state.context_cache[key] = context_from_trees(trees) or false
    render()
  end)
end)
```

Avoid synchronous parsing in renderers, previews, keymaps, cursor handlers, and
autocmds. If the async context is optional, prefer showing the Git/parser
fallback immediately and upgrading the label after parsing completes.

### Async Git in UI Plugins

Use `vim.system({ ... }, { text = true }, on_exit)` for Git/process calls from
interactive plugin code. Avoid `vim.fn.system()`, `vim.fn.systemlist()`, and
`vim.system(...):wait()` in render paths, keymaps, autocmds, cursor handlers, or
anything that can run while the user is waiting for the editor.

Process callbacks must schedule editor mutations back onto the main loop:

```lua
vim.system({ "git", "-C", root, "status", "--short" }, { text = true }, function(result)
  vim.schedule(function()
    if result.code ~= 0 then
      vim.notify(result.stderr ~= "" and result.stderr or "git status failed", vim.log.levels.ERROR)
      return
    end
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(result.stdout, "\n", { plain = true }))
  end)
end)
```

Every async refresh should have a monotonically increasing request id:

```lua
state.request_id = (state.request_id or 0) + 1
local request_id = state.request_id
vim.system(cmd, { text = true }, function(result)
  vim.schedule(function()
    if state.request_id ~= request_id then return end
    render(result)
  end)
end)
```

This prevents slow callbacks from repainting stale data after a newer refresh
has already completed. Design responsive UIs around an explicit UI model that is
separate from backend state: user actions update the UI model immediately, then
an async process updates Git/backend state and reconciles the model when it
finishes. Render a cheap spinner or `...` loading state only for the first load
or when there is no useful previous model; during action-triggered refreshes,
keep the existing UI visible until async data is ready.

For status/list UIs, prefer optimistic in-memory state over a loading flash:

```lua
apply_optimistic_change(state.sections, action)
render(state.sections)
enqueue_git_operation(function(done)
  run_git_async(action, function(result)
    if result.code ~= 0 then notify_failure(result) end
    mark_needs_reconcile()
    done()
  end)
end)
```

If the user stages and immediately unstages before Git finishes, the second
action should operate on the optimistic state and the underlying Git commands
must run FIFO. Reconcile from Git once the queue is idle, not after each command
while later queued commands are still pending.

Run Git index mutations asynchronously but sequentially inside a batch. Parallel
`git add`, `git restore --staged`, `git checkout`, or `git apply --cached`
commands can race on `.git/index.lock`. Sequential callbacks keep the UI
responsive without corrupting the batch.

### Parsing Unified Diff

The `@@ -old,count +new,count @@ context` header:
- `+new` is the **1-based** line number in the new file
- This points to where **context lines start**, not the first change
- The first actual change is `new + (number of leading context lines)`

### Staging Individual Hunks

```lua
vim.system(
  { "git", "-C", cwd, "apply", "--cached", "--whitespace=nowarn", "-" },
  { text = true, stdin = patch_text .. "\n" },
  function(result)
    vim.schedule(function()
      if result.code ~= 0 then
        vim.notify(result.stderr ~= "" and result.stderr or "stage failed", vim.log.levels.ERROR)
      end
    end)
  end
)
```

- The patch must include the file header (`diff --git`, `---`, `+++`) AND
  the hunk (`@@` + lines)
- Use `--whitespace=nowarn` to avoid whitespace errors
- Use `-C cwd` to run from git root (paths in patch are relative to root)
- For unstaging: add `--reverse`

### Empty New Files

`git diff --cached` for empty new files produces a diff with `new file mode`
but NO `@@` hunk header. Your parser must handle this case by checking
`git diff --name-status` for files that have no hunks:

```lua
vim.system({ "git", "-C", cwd, "diff", "--cached", "--name-status" }, { text = true }, function(result)
  vim.schedule(function()
    if result.code ~= 0 then return end
    for _, line in ipairs(vim.split(result.stdout or "", "\n", { plain = true })) do
      local status, file = line:match("^(%S+)%s+(.+)$")
      if file and not files_with_hunks[file] then
        -- Handle file with no hunks (empty new file, binary, etc.)
      end
    end
  end
end)
```

### Untracked Files: Synthesize the Diff, Don't Ask Git

Untracked files have no git diff (`git diff`/`git diff --cached` ignore them).
Don't try to fetch one — build a synthetic "new file" patch straight from disk
and feed it through the normal render path so the file previews as one
all-additions hunk:

```
diff --git a/<relpath> b/<relpath>
new file mode 100644
--- /dev/null
+++ b/<relpath>
@@ -0,0 +1,<N> @@
+<line 1>
...
```

Read the contents with `vim.fn.readfile`, prefix each line with `+`, and skip
binary files (a NUL byte in any line) so you don't dump garbage.

**Perf trap that motivated this:** the per-file preview cache (`_file_diffs`)
is keyed by filename, and the "do I need to fetch?" guard was `not
_file_diffs[file]`. Tracked files get populated during `get()`, so they're a
cache hit. Untracked files were never populated, so **every cursor move** over
one re-ran two full-repo `git diff` calls — visibly laggy. Fix is two parts:
(1) build untracked diffs from disk (no git at all), and (2) cache a `false`
sentinel for "checked but empty/binary" and change the guard to
`_file_diffs[file] == nil`, so a negative result is remembered instead of
re-fetched on every move. Reserve `nil` for "invalidated, must re-fetch" (set on
`BufLeave` to a real file).

### Path Comparison on Windows

Windows uses `\` in paths, git uses `/`. Always normalize before comparing:

```lua
local norm = vim.fs.normalize(path)
```

### Driving `git commit` (the fake-editor bridge)

You can't run `git commit` with an interactive editor *inside* the running
nvim — git would block waiting for an editor it can't reach. The trick (lifted
from Neogit, see `diff_review_commit.lua`): point `GIT_EDITOR` at a **headless
child nvim** that RPCs back to the parent.

1. Run `git commit` via `vim.system({ "git", "commit" }, opts, on_exit)` with
   `env = { GIT_EDITOR = <headless cmd>, NVIM = vim.v.servername }`. Stream
   stdout/stderr callbacks into a console buffer shown in the **existing
   diff-preview window** above the Trouble pane (save its buffer + winbar first
   to restore later) — no new float. This is where pre-commit hook output shows
   up, *before* the editor opens.
2. `GIT_EDITOR` is `nvim --headless --clean … -c "lua require('…').client()"`.
   Built with `vim.fn.shellescape` per token (git parses it with a shell). The
   `--clean` child needs `set runtimepath^=<config root>` to find the module.
3. In the child, `client()` reads the `COMMIT_EDITMSG` path from `argv()`,
   `serverstart()`s its own address, and `sockconnect`s to `$NVIM` (the parent)
   to `rpcrequest("nvim_command", "lua …editor(target, child_addr)")`. Then it
   **returns and the child keeps running** — git stays blocked on it.
4. The parent's `editor()` **swaps that same window's buffer** to the message
   buffer (the diff window is reused throughout — console → message → back to
   preview). On submit it writes the file, `rpcnotify`s the child `qall` (exit
   0 → git commits); on abort it sends `cq` (exit non-zero → git aborts). A
   `BufWipeout` autocmd sends `cq` too, so closing the buffer any other way
   never hangs git.
5. The job's `on_exit` hands the window back: success/abort restore the saved
   buffer + winbar and refresh the list (so `auto_preview` repopulates it); a
   real failure (hook rejected, empty message) shows git's captured output in
   the window until `q`. Distinguish abort from failure with a flag — both exit
   non-zero, but only a failure should show the error. Keep the console buffer
   `bufhidden=hide` so it survives the swap and still has the output to show.

Because the commit borrows the preview window, the cursor-move `auto_preview`
would clobber it. Gate `auto_preview` on a module flag (`M.suspend_preview`) that
the commit flow sets while running and clears on exit.

Gotchas: set `NVIM` explicitly in the job env (don't rely on it being
inherited); wrap the child's `client()` so any error does `cq` rather than
leaving git hung; `$NVIM` only exists if the parent has a server
(`serverstart()` if `v:servername` is empty).

---

## Highlight & Virtual Text

### Background-Only Highlights

To set background color without affecting foreground (preserving syntax):

```lua
vim.api.nvim_set_hl(0, "MyAddBg", { bg = "#002200" })
```

Do NOT link to `DiffAdd` if it has both `fg` and `bg` — the `fg` will
override syntax highlighting.

### line_hl_group vs hl_group

- `line_hl_group`: Colors the entire screen line. But invisible under
  virtual text with `hl_mode = "replace"`.
- `hl_group` with `hl_eol = true`: Colors from col to end of text, then
  extends past EOL. Also invisible under `hl_mode = "replace"` overlays.

If the buffer uses Snacks' virtual text rendering, neither works. Modify
the highlight groups in the virtual text entries instead.

### Gutter Width in Diff Buffers

Snacks renders line numbers and diff prefixes as virtual text overlays.
The underlying buffer text has matching spaces as padding. The gutter width
depends on the max line number width and varies per hunk.

To compute gutter width for cursor column mapping:

```lua
-- Find the code in the diff line and locate it in the buffer line
local diff_code = diff_line:sub(2)  -- strip +/-/space prefix
local code_pos = buf_line:find(vim.pesc(diff_code:sub(1, 20)), 1, true)
local gutter_width = code_pos and (code_pos - 1) or 0
```

---

## Common Bugs & How to Avoid Them

### 1. Shared Table References

**Bug**: Two items share the same `pos` table. Snacks preview uses reference
equality (`self.pos == item.pos`) to skip redundant updates.

```lua
-- BAD: shares reference
file_item.pos = first_hunk.pos

-- GOOD: copy
file_item.pos = { first_hunk.pos[1], first_hunk.pos[2] }
```

### 2. Trouble Preview Overlay vs Real Buffer

**Bug**: Trying to edit in the preview overlay (read-only scratch buffer).

**Fix**: Don't use `focus_preview`. Close/hide the picker and open the real
file with `vim.cmd.edit()`.

### 3. WinLeave Closing Preview

**Bug**: Focusing the preview window triggers `WinLeave` on Trouble, which
closes the preview immediately.

**Fix**: Don't try to focus the preview overlay. Either:
- Use the preview for read-only browsing only
- Set buffers on the main window directly (bypass preview system)

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

Bad pattern:

```lua
local target_id, fallback_line = cursor_target(buf)
load_async(function(result)
  render_loaded(buf, result, target_id, fallback_line)
end)
```

Good pattern:

```lua
load_async(function(result)
  local target_id, fallback_line = cursor_target(buf)
  render_loaded(buf, result, target_id, fallback_line)
end)
```

Async callbacks should preserve the current target at render time, not target
the item whose async work just completed.

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
