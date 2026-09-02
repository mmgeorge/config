# Trouble.nvim v3 Sources and Snacks Diff Renderer Integration

This reference documents Trouble.nvim v3 custom source contracts, hierarchy management, action contexts, and the Snacks.nvim diff rendering pipeline.

## Trouble v3 Source Module Structure

Trouble v3 discovers source modules located under `lua/trouble/sources/<name>.lua` along the runtimepath:

```lua
local M = {}

M.config = { modes = { ... } }
M.setup = function() end
M.get = function(cb, ctx) end

return M
```

- **`config.modes`**: Defines declarative modes and formatting options.
- **`setup()`**: Executes once when the source is loaded.
- **`get(cb, ctx)`**: Asynchronously produces items and invokes `cb(items)`.

## Constructing Items with `Item.new`

Instantiate list rows using `require("trouble.item").new()`:

```lua
local Item = require("trouble.item")

Item.new({
  source = "my_source",
  filename = vim.fs.normalize("/absolute/path/to/file.lua"),
  pos = { line, col },
  item = {
    check = "[ ]",
    category = "Tracked Changes",
  },
})
```

- **`filename`**: Must be an absolute, normalized path.
- **`pos`**: `{ line, col }` tuple with 1-based line and 0-based column. Clone the position table explicitly rather than sharing references across items.
- **`item`**: Custom field dictionary accessible in format templates as `{item.fieldName}`.

## Grouping and Hierarchy

Group flat item arrays into tree structures via the `groups` configuration:

```lua
groups = {
  { "item.category" },
  { "filename", format = "{file_icon} {basename} {item.stats}" },
}
```

Format templates assigned to a group header render attributes from the first child item in that group.

## Action Context and Header Handling

Action callbacks receive `(view, ctx)`:

- **`ctx.item`**: The active `Item` under the cursor (evaluates to `nil` on group header rows).
- **`ctx.node`**: The active tree node.

```lua
local is_group = ctx.node and ctx.node.group ~= nil

if is_group then
  -- Walk all child nodes under the group header
  local nodes = file_group_nodes(ctx.node)
else
  -- Operate on the individual item
  process_item(ctx.item)
end
```

To operate on all items under a category header, traverse the node's child tree recursively rather than indexing `ctx.node.item`.

## Fold Level Initialization

Set default fold levels once during initial render inside `first_render`:

```lua
view.first_render:next(function()
  view:fold_level({ level = 1 })
end)
```

Do not invoke `view:fold_level()` after subsequent `view:refresh()` calls, as it overrides manual fold adjustments made by the user.

## Main Window Caching

`view:main()` identifies the primary editor window by filtering out non-empty `buftype` windows. If scratch or preview buffers are placed into the main window, `view:main()` can incorrectly return the Trouble window. Cache the window ID on initial setup before mounting scratch buffers:

```lua
function M.get_main_win(view)
  if not M._main_win or not vim.api.nvim_win_is_valid(M._main_win) then
    local main = view:main()
    if main then
      M._main_win = main.win
    end
  end
  return M._main_win
end
```

## Winbar Keymap Hints

Display keymap hints in the window `winbar` using standard statusline formatting:

```lua
vim.wo[win].winbar = " %#Comment#<C-c><C-c>%* commit   %#Comment#<C-q>%* abort "
```

## Snacks Diff Renderer Operations

Render unified diff strings using `require("snacks.picker.util.diff")`:

```lua
local snacks_diff = require("snacks.picker.util.diff")
local H = Snacks.picker.highlight

local diff = snacks_diff.get_diff(diff_text)
local lines = snacks_diff.format(diff)
H.render(buf, ns, lines)
```

### Highlight Overlays in Formatted Diffs

Snacks formats diff text using inline virtual text overlays configured with `hl_mode = "replace"` (`SnacksDiffAdd`, `SnacksDiffDelete`). Because `replace` overlays take precedence over line extmarks, recolor diff elements by modifying the highlight group names directly within the formatted table before invoking `render`:

```lua
local hl_replacements = {
  SnacksDiffAdd = "MyAddBg",
  SnacksDiffDelete = "MyDeleteBg",
}

for _, line in ipairs(lines) do
  for _, entry in ipairs(line) do
    if type(entry[1]) == "string" and entry[2] then
      entry[2] = hl_replacements[entry[2]] or entry[2]
    end
  end
end
```

### Trailing Empty Line Normalization

The Snacks diff parser strips trailing empty lines from hunks. When calculating rendered line spans for custom cursor tracking or fold ranges, strip trailing empty lines to match the rendered geometry.
