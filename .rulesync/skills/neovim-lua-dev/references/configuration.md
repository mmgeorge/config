# Configuration and Keymap Architecture

This reference documents the configuration pipeline, option merge semantics, data-driven keymap installation, and package manager specifications for Neovim plugins.

## Configuration Lifecycle and Merging

Consolidate user configuration through a single merge step. Do not route raw option tables across submodules individually.

```lua
-- infra/config.lua
M.options = vim.deepcopy(M.defaults)

function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.defaults), opts or {})
  return M.options
end
```

In `init.lua`, `setup` coordinates subsystem initialization from the merged options:

```lua
function M.setup(opts)
  M.config = config.setup(opts)
  perf.configure_from_diff_review_options(M.config)
  highlights.setup()
  require("diff_review.views.status.state").register_view_controllers()
end
```

Internal modules access configuration via `require("diff_review.infra.config").options`. Avoid caching independent copies of option tables inside view modules.

## Keymap Organization by Interaction Vocabulary

Organize default keymaps under `config.defaults.keymaps` categorized by operational mode rather than per view buffer:

- **`keymaps.status`**: Status-family views (status, pull request, file diff, branch diff) sharing a common navigation vocabulary (`q` to close, `<Tab>` to toggle fold, `o`/`<CR>` to open, `R` to refresh, `S`/`U` to stage/unstage).
- **`keymaps.review`**: Dedicated review mode where keys represent distinct semantic actions (`S` to mark viewed, `U` to mark unviewed, `cc` to submit review).

```lua
-- infra/config.lua
keymaps = {
  status = {
    close = "q",
    refresh = "R",
    reply = "R",
    toggle = "<Tab>",
    collapse_parent = "N",
    stage = "S",
    unstage = "U",
    discard = "j",
    open = { "o", "<CR>", "." },
    commit = "cc",
    help = "?",
  },
  review = {
    viewed = "S",
    unviewed = "U",
    comment = "C",
    delete = "J",
    sync = "<C-s>",
    submit = "cc",
  },
}
```

Binding values accept a key string (`"R"`), a list of alternative keys (`{ "o", "<CR>", "." }`), or `false` to disable the command entirely.

## Deep Merge Behavior and Override Semantics

Each keymap group merges independently against defaults.

```lua
local function status_keymap_config()
  local options = M.config or config.options or config.defaults
  local keymaps = options.keymaps or config.defaults.keymaps
  return vim.tbl_deep_extend("force", vim.deepcopy(config.defaults.keymaps.status), keymaps.status or {})
end
```

`vim.tbl_deep_extend` merges lists by numeric index. When overriding a default list binding with a shorter list, supply an explicit string or a list of equal or greater length to ensure trailing default elements do not persist.

## Data-Driven Command Specifications

Define all commands as declarative data in `shared/command_specs.lua`. Separate display visibility from key configuration:

```lua
{
  id = "stage",
  label = "stage",
  desc = "Stage hunk, file, or selection",
  modes = { "n", "x" },
  visual = true,
  pinned = true,
  views = { status = true },
},
{
  id = "comment",
  label = "comment",
  desc = "Add an inline comment",
  modes = { "n", "x" },
  keymap = "review",
  visual = true,
  pinned = true,
  views = { pr = true, review = true },
},
{
  id = "open",
  label = "open",
  desc = "Open PR or jump to source file",
  modes = "n",
  pinned = true,
},
```

- **`views`**: Determines view visibility. Omitting the field makes the command active in all views.
- **`keymap`**: Specifies which keymap group provides the key binding (`status` or `review`).

## Data-Driven Keymap Installer

Avoid hardcoded conditional branches when binding keymaps. Assemble keymaps using static tables for common keys, per-view actions, and setup hooks:

```lua
local COMMON_KEYMAPS = {
  { id = "close", mode = "n", handler = close_view },
  { id = "toggle", mode = "n", handler = toggle_fold },
  { id = "collapse_parent", mode = "n", handler = collapse_parent },
  { id = "visual_line_with_gutter", mode = "n", handler = start_visual_line_gutter },
}

local VIEW_KEYMAPS = {
  status = {
    { id = "stage", mode = "n", handler = stage_under_cursor, desc = "Stage hunk or file" },
    { id = "stage", mode = "x", handler = stage_selection, desc = "Stage visual selection" },
    { id = "open", mode = "n", handler = status_open },
  },
  pr = {
    { id = "comment", mode = { "n", "x" }, handler = pr_add_comment },
  },
  diff = {
    { id = "refresh", mode = "n", handler = refresh_diff },
  },
}

local function setup_status_keymaps(buf)
  local view_kind = session.status and session.status.view_kind or "status"

  local function map(command_id, mode, callback, desc)
    local spec = status_command_specs_by_id[command_id]
    if spec and not status_command_visible(spec) then
      return
    end
    for _, key in ipairs(status_keys_for(command_id)) do
      vim.keymap.set(mode, key, callback, {
        buffer = buf,
        silent = true,
        nowait = true,
        desc = desc or (spec and spec.desc) or command_id,
      })
    end
  end

  for _, item in ipairs(COMMON_KEYMAPS) do
    map(item.id, item.mode, function() item.handler(buf) end, item.desc)
  end
  for _, item in ipairs(VIEW_KEYMAPS[view_kind] or {}) do
    map(item.id, item.mode, function() item.handler(buf) end, item.desc)
  end
end
```

## Plugin Specification for Lazy.nvim

Structure the lazy plugin specification to defer loading until command execution or key binding invocation:

```lua
return {
  "you/diff_review",
  dependencies = { "folke/snacks.nvim" },
  cmd = {
    "GitStatus",
    "GitBranchDiff",
    "GitBranchDiffFile",
    "GitFileRevision",
    "GitDiffCompactPreview",
  },
  keys = {
    { "<leader>gs", "<cmd>GitStatus<cr>", desc = "Git status review" },
    {
      "<leader>gd",
      function()
        require("diff_review").open_compact_preview()
      end,
      desc = "Compact diff preview",
    },
  },
  opts = {
    about_auto_generate = false,
    keymaps = {
      status = {
        discard = "X",
        open = { "<cr>", "o", "." },
        walkthrough = false,
      },
      review = {
        submit = "<C-CR>",
      },
    },
  },
  config = function(_, opts)
    require("diff_review").setup(opts)
  end,
}
```

Always assign descriptive `desc` attributes to lazy key bindings and buffer-local keymaps to ensure discovery in which-key and interactive help dialogs.
