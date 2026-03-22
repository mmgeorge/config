# Agent Guide: Neovim Plugin Development

This document captures hard-won knowledge from building the DiffReview plugin
(a custom Trouble source with a fancy diff buffer). It covers Snacks.nvim and
Trouble.nvim internals, common pitfalls, and patterns that actually work.

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

- It resets fold level to 99 (re-apply with `vim.defer_fn`)
- `view:at()` returns stale data until render completes
- Cursor position may shift; use `vim.defer_fn` for post-refresh operations

```lua
view:refresh()
vim.defer_fn(function()
  view:fold_level({ level = 1 })
  -- Now view:at() has fresh data
end, 50)
```

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

### Parsing Unified Diff

The `@@ -old,count +new,count @@ context` header:
- `+new` is the **1-based** line number in the new file
- This points to where **context lines start**, not the first change
- The first actual change is `new + (number of leading context lines)`

### Staging Individual Hunks

```lua
vim.fn.system(
  { "git", "-C", cwd, "apply", "--cached", "--whitespace=nowarn", "-" },
  patch_text .. "\n"  -- MUST end with newline
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
local name_status = vim.fn.systemlist({ "git", "-C", cwd, "diff", "--cached", "--name-status" })
for _, line in ipairs(name_status) do
  local status, file = line:match("^(%S+)%s+(.+)$")
  if file and not files_with_hunks[file] then
    -- Handle file with no hunks (empty new file, binary, etc.)
  end
end
```

### Path Comparison on Windows

Windows uses `\` in paths, git uses `/`. Always normalize before comparing:

```lua
local norm = vim.fs.normalize(path)
```

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

### 5. Fold Level Reset on Refresh

**Bug**: `view:refresh()` re-renders with Trouble's default `foldlevel=99`.

**Fix**: Re-apply fold level after refresh:
```lua
view:refresh()
vim.defer_fn(function()
  view:fold_level({ level = 1 })
end, 50)
```

### 6. BufEnter for Diff Buffer Refresh

**Bug**: Diff buffer shows stale content after editing the real file and
switching back.

**Fix**: Add `BufEnter` autocmd on the diff buffer:
```lua
vim.api.nvim_create_autocmd("BufEnter", {
  buffer = buf,
  callback = function()
    M._refresh_diff_buffer(buf, filename)
  end,
})
```

### 7. nowait for Single-Key Mappings

**Bug**: Pressing `S` waits for a second keypress because another mapping
starts with `S` (e.g., `Sa`).

**Fix**: Add `nowait = true` to the keymap options.
