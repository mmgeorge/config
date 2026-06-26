# Trouble.nvim v3 Sources & the Snacks Diff Renderer

*When to read this:* before building a Trouble.nvim v3 custom source (items,
groups, actions, folds, preview) or rendering a diff through the Snacks.nvim
fancy diff renderer. The rules below are the ones a model gets wrong from first
principles. For fold mechanics see rendering-and-highlights.md; for the
git/async side of a status UI see async-and-git.md; numbered concrete bugs live
in common-bugs.md.

> Grounding note: DiffReview grew out of a literal Trouble source and the Snacks
> diff renderer, then internalized both — the status view is now a `GitStatus`
> buffer and `render/diff_render.lua` emits its own `virt_text` rows. It still
> obeys every constraint below (winbar hints in `integrations/commit.lua` /
> `shared/keymaps.lua`; `hl_mode = "replace"` overlays in `diff_render.lua`).
> Treat this as the integration contract for wiring a *new* source/renderer on.

---

## Source file & module shape

Trouble v3 auto-discovers sources at `lua/trouble/sources/<name>.lua` anywhere on
the runtimepath. The module returns a table with exactly this shape:

```lua
local M = {}
M.config = { modes = { ... } }  -- registers named modes
M.setup = function() end         -- called once on load
M.get = function(cb, ctx) end    -- fetches items; call cb(items)
return M
```

Register the mode in `M.config.modes`, not imperatively elsewhere. `M.get` is
also what `view:refresh()` re-runs (see *Refreshing after actions*), so keep it
side-effect free apart from producing items.

## Items: `Item.new` with an absolute filename

Build every row with `require("trouble.item").new()`:

```lua
local Item = require("trouble.item")
Item.new({
  source = "my_source",
  filename = "/absolute/path/to/file.lua",  -- MUST be absolute
  pos = { line, col },                        -- 1-based line, 0-based col
  item = {                                    -- custom fields, see below
    check = "[ ]",
    category = "Tracked Changes",
  },
})
```

`filename` must be absolute — a relative path breaks preview, jump, and grouping
by file. On Windows normalize first (`vim.fs.normalize`) so git's `/` and the
OS's `\` compare equal. Custom fields go under `item = { ... }` and are then
addressable as `{item.field}` inside format strings.

Never share a mutable subtable between two items. Snacks' preview uses reference
equality on `pos` to skip redundant updates, so copy it:

```lua
file_item.pos = { first_hunk.pos[1], first_hunk.pos[2] }  -- not = first_hunk.pos
```

(See common-bugs.md #1.)

## Groups & hierarchy

`groups` turns flat items into a collapsible tree. Each entry is one level:

```lua
groups = {
  { "item.category" },                                            -- level 1
  { "filename", format = "{file_icon} {basename} {item.stats}" }, -- level 2
},
```

Items are bucketed by matching field values. **A group's `format` string renders
from the *first* item in that group** — this is the same trap that bites group
actions below, so don't put per-item data in a header format and expect it to
summarize the group.

## Actions: `ctx.item` vs `ctx.node` (and the first-item trap)

Actions receive `(view, ctx)`:

```lua
ctx.item  -- the trouble.Item at cursor — nil on group header rows!
ctx.node  -- the tree node at cursor — always set
ctx.opts  -- action options
```

`ctx.item` is `nil` on a group header. Distinguish header from item via the
node's `group` field:

```lua
local is_group = ctx.node and ctx.node.group ~= nil
if is_group then
  -- header row: ctx.node.item is the group's FIRST item only
else
  -- real item row: ctx.item is valid
end
```

**The trap:** `ctx.node.item` is **only the group's first item**. On a top-level
header like "Tracked Changes" that is just the first file's first hunk. An action
that reads `ctx.node.item.filename` silently operates on one file instead of the
whole group. To act on everything under a header, walk the subtree down to the
file-level nodes (`node.group.fields[1] == "filename"`) and loop:

```lua
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

## Movement: `next`/`prev` skip headers

Trouble's built-in `next`/`prev` actions land only on **item** rows — they skip
group headers. If an action needs the cursor to reach headers (e.g. to stage a
whole file/category from its header), drive the cursor raw instead:

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

## Fold level: the hardcoded `foldlevel = 99`

Trouble forces `foldlevel = 99` via `Window.FOLDS`, applied with
`vim.tbl_deep_extend("force", ...)` **after** your config — so setting
`win.wo.foldlevel` does nothing. Set the level once, in the `first_render`
promise:

```lua
view.first_render:next(function()
  view:fold_level({ level = 1 })  -- 0 = all collapsed, 1 = first level expanded
end)
```

Do this **once and never again.** The renderer keeps per-node fold state
(`renderer._folded`, keyed by stable `node.id`) across a refresh — `clear()` and
`render()` never touch it, and `foldlevel = 99` is applied only at mount.
Calling `fold_level()` after a `refresh()` recomputes folds purely from depth and
discards whatever the user manually expanded/collapsed (staging one hunk would
re-collapse every open file). See common-bugs.md #5 and #10 (fold state is keyed
by a `node.id` that encodes the group path, so a file that changes category
becomes a *different* node and loses its fold — pre-seed the destination id
before refresh rather than folding after).

## Preview = "main" is a floating overlay

With `preview.type = "main"`, Trouble draws a **floating overlay window** over
the main editor window. Consequences:

- `ctx.win` in preview functions is the **overlay**, not the main window.
- `line_hl_group` / `hl_group` extmarks are invisible under the overlay
  (`hl_mode = "replace"`).
- Focusing the overlay fires `WinLeave` on Trouble, which closes the preview.
- `preview.scratch = false` uses real buffers (LSP attaches) but still overlays.

**If the user must edit or interact with the previewed content, do not use the
preview system.** Set the buffer on the main window directly and manage the
window yourself (see rendering-and-highlights.md for main-window setup and
common-bugs.md #2/#3 for the overlay-vs-real-buffer and `WinLeave` failures).

## Refreshing after actions

`view:refresh()` re-runs `M.get()` and re-renders, but the new data is not
synchronously available:

```lua
view:refresh()
vim.defer_fn(function()
  -- only now does view:at() return fresh data; move cursor / update preview here
end, 50)
```

`view:at()` returns stale data until the render completes, and cursor rows shift
under you. Prefer identity-based cursor restore (scan `renderer._locations` for
the file node whose `node.item.filename` matches) over `cursor + 1`, which lands
on a freshly created header. Do **not** re-apply `fold_level()` here.

## Main-window detection (cache it)

`view:main()` finds the main editor window by rejecting any window whose buffer
has `buftype ~= ""`. The moment you put a scratch/`nofile` buffer on the main
window, `view:main()` returns the **wrong** window (it falls back to the current
window — possibly Trouble itself). Cache the id on first call, before you set any
scratch buffer:

```lua
function M.get_main_win(view)
  if not M._main_win or not vim.api.nvim_win_is_valid(M._main_win) then
    local main = view:main()
    if main then M._main_win = main.win end
  end
  return M._main_win
end
```

## Winbar key hints

Use the window's `winbar` for non-selectable header hints — it is not a list row,
so `next`/`prev` and actions never touch it. `%#HlGroup#text%*` switches highlight
group, `%*` resets, `%=` right-aligns. This is exactly how the current plugin
draws its sticky hint bar (`integrations/commit.lua`, `shared/keymaps.lua`):

```lua
vim.wo[win].winbar = " %#Comment#<C-c><C-c>%* commit   %#Comment#<C-q>%* abort "
```

Always address the explicit window (`vim.wo[win]`, not `vim.wo[0]`) — inside an
autocmd the "current" window is often not the one you mean.

---

## Snacks diff renderer: get_diff / format / format_hunk

The Snacks fancy diff lives at `require("snacks.picker.util.diff")`. The base
flow parses a unified-diff string and renders highlight lines into a buffer:

```lua
local snacks_diff = require("snacks.picker.util.diff")
local H = Snacks.picker.highlight

local diff = snacks_diff.get_diff(diff_text)  -- parse diff string -> diff model
local ret = snacks_diff.format(diff)          -- -> highlight lines
H.render(buf, ns, ret)                         -- render to buffer
```

To render hunk bodies **without** the file/hunk headers, drive `format_hunk`
directly. It takes a context object that mirrors Snacks' internal `ctx`; build
one with an `extend` that chains `__index` metatables down block → hunk:

```lua
local opts = { hunk_header = false }
local ctx_base = setmetatable({ diff = diff, opts = opts }, { __index = function(_, k)
  if k == "extend" then
    return function(self2, t) return setmetatable(t, { __index = self2 }) end
  end
end })

for _, block in ipairs(diff.blocks) do
  local block_ctx = ctx_base:extend({ block = block })
  for _, hunk in ipairs(block.hunks) do
    local hunk_ctx = block_ctx:extend({ hunk = hunk })
    vim.list_extend(ret, snacks_diff.format_hunk(hunk_ctx))
  end
end
```

## Replace highlight groups in the format output, not via extmarks

Snacks renders code as **virtual-text overlays** with `hl_mode = "replace"`
(`SnacksDiffAdd`, `SnacksDiffDelete`, ...). That overlay completely overrides any
underlying `line_hl_group` or `hl_group` extmark — so to recolor, you must
rewrite the highlight groups **inside the `format` output, before rendering**.
Extmarks layered on afterward are invisible. (The current `diff_render.lua`
relies on the same fact: it carries each row's background `hl` on every
`virt_text` chunk because a later `line_hl_group` would lose to the overlay.)

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

for _, line in ipairs(ret) do
  for _, entry in ipairs(line) do
    if type(entry[1]) == "string" and entry[2] then
      entry[2] = replace_hl(entry[2])      -- the chunk's hl group
    end
    if entry.virt_text then
      for _, vt in ipairs(entry.virt_text) do
        if vt[2] then vt[2] = replace_hl(vt[2]) end
      end
    end
  end
end
```

For background-only recoloring define the replacement with `bg` only
(`vim.api.nvim_set_hl(0, "MyAddBg", { bg = "#002200" })`) so syntax foreground
survives; see rendering-and-highlights.md.

## Hunk line counting (match Snacks' trailing-blank strip)

Snacks' `parse_hunk` **strips trailing empty lines** from a hunk. When you
compute the rendered line range of a hunk yourself (for cursor mapping, fold
boundaries, or line-level actions), replicate that strip or your ranges drift by
the number of trailing blanks:

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

The `@@ -old,count +new,count @@` header's `+new` is the **1-based** new-file
line where *context* starts, not the first change (first change is
`new + <leading context lines>`). Keep raw hunks separate from displayed hunks
and never feed a virtual display header back to git — see async-and-git.md.
