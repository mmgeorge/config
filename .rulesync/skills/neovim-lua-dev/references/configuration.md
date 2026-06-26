# Configuration & Keymaps

*When to read this:* before adding a user-facing option, adding or rebinding an in-buffer key, wiring a new command into a view, or writing the lazy.nvim spec — anything touching `infra/config.lua`, `shared/command_specs.lua`, or `shared/keymaps.lua`.

Ground truth: `D:\config\nvim\lua\diff_review`. The config layer is deliberately tiny — one defaults table, one merge, and a data-driven keymap installer. Match its shape; do not grow a parallel options path.

## setup(opts): the entire plumbing is one line

User options flow through exactly one merge. Do not thread `opts` through init by hand.

```lua
-- infra/config.lua
M.options = vim.deepcopy(M.defaults)          -- live config at module load

function M.setup(opts)
  M.options = vim.tbl_deep_extend("force", vim.deepcopy(M.defaults), opts or {})
  return M.options
end
```

```lua
-- init.lua  M.setup is the only caller users invoke
function M.setup(opts)
  M.config = config.setup(opts)               -- merge + publish to config.options
  perf.configure_from_diff_review_options(M.config)
  highlights.setup()
  require("diff_review.views.status.state").register_view_controllers()
end
```

After `setup`, `config.options` is the source of truth and every reader pulls from it. The read idiom you will see in `shared/keymaps.lua` and `views/pr/review.lua` is:

```lua
local options = M.config or config.options or config.defaults
```

The per-module `M.config` here is a normally-nil test/override seam (nothing assigns it in the live tree), so reads fall through to `config.options` after `setup`, or `config.defaults` before it. Read config through `require("diff_review.infra.config").options`; do NOT cache a copy of `opts` in a view module.

Per-repo overrides (e.g. `branch_prefix`) come from `<repo>/.diffreview.json` via `git/repo_config.lua`, layered on top of `config.options` at use sites — separate from `setup` and out of scope here.

## Keymap model: two groups by MODE, not per view

Defaults live under `config.defaults.keymaps`, split into exactly two groups:

- `keymaps.status` — the status-family views: **status / pr / diff / branch-diff** share one vocabulary (`q` close, `<Tab>` toggle, `o`/`<CR>`/`.` open, `R` refresh, `?` help, `S`/`U`/`j` stage/unstage/discard, …).
- `keymaps.review` — review mode **redefines** keys (`S` = mark viewed, `U` = unviewed, `cc` = submit, `<C-s>` = sync). It earns its own group because its vocabulary genuinely diverges.

```lua
-- infra/config.lua (excerpt)
keymaps = {
  status = {
    close = "q", refresh = "R", toggle = "<Tab>", collapse_parent = "N",
    stage = "S", unstage = "U", discard = "j",
    open = { "o", "<CR>", "." }, commit = "cc", help = "?", -- …
  },
  review = {
    viewed = "S", unviewed = "U", comment = "C", delete = "J",
    sync = "<C-s>", submit = "cc", -- …
  },
}
```

A binding value is one of: a **key string** (`"R"`), a **list of keys** (`{ "o", "<CR>", "." }`), or **`false` to disable**. Unspecified keys keep their default.

## Override semantics: deep-merge per command

Each group merges independently over its defaults. From `shared/keymaps.lua`:

```lua
local function status_keymap_config()
  local options = M.config or config.options or config.defaults
  local keymaps = options.keymaps or config.defaults.keymaps
  return vim.tbl_deep_extend("force", vim.deepcopy(config.defaults.keymaps.status), keymaps.status or {})
end
```

`views/pr/review.lua` has the identical shape for the `review` group. So `opts.keymaps.status.discard = "X"` rebinds only `discard`; all other `status` keys keep defaults.

GOTCHA — list values merge by INDEX. `vim.tbl_deep_extend` recurses into list tables, so a *shorter* replacement list leaves trailing default keys bound. The only list default is `open = { "o", "<CR>", "." }`:

```lua
-- opts.keymaps.status.open = { "<cr>", "o" }
-- result: { "<cr>", "o", "." }   ← default index [3] "." LEAKS THROUGH
```

To fully replace, pass a **string** (single key cleanly overrides the whole table) or a list **no shorter** than the default. There is no per-index disable; `open = false` disables the command entirely.

## Command vocabulary is pure data

`shared/command_specs.lua` is the single registry. Each spec carries metadata, per-view **visibility**, and an optional keymap **group** — visibility is separate from key assignment.

```lua
{ id = "stage", label = "stage", desc = "Stage hunk/file/selection",
  modes = { "n", "x" }, visual = true, pinned = true, views = { status = true } },

{ id = "comment", label = "comment", desc = "Add or edit an inline comment",
  modes = { "n", "x" }, keymap = "review", visual = true, pinned = true,
  views = { pr = true, review = true } },           -- key comes from the REVIEW group

{ id = "open", label = "open", desc = "Open PR/about or jump to file", modes = "n", pinned = true },
```

Two fields drive wiring:

- `views` → visibility. Omit it for "visible everywhere" (`open`, `refresh`, `close`, `help`). Otherwise only the listed views show it:
  ```lua
  function status_command_visible_for_view(spec, view_kind)
    return not spec.views or spec.views[view_kind] == true
  end
  ```
- `keymap` → which config group supplies the key. It is **per-command, not per-view**: `comment`/`sync` render inside the `pr` view but read their keys from `keymaps.review`, while `toggle`/`open` in the same view read from `keymaps.status`. Resolution:
  ```lua
  local function status_keys_for(command_id)
    local spec = status_command_specs_by_id[command_id]
    local keymaps = spec and spec.keymap == "review"
      and review().keymap_config() or status_keymap_config()
    local key = keymaps[command_id]
    if key == false or key == nil then return {} end   -- disabled / unmapped
    if type(key) == "table" then return key end        -- list of keys
    return { key }                                      -- single key
  end
  ```

`M.hint_command_ids_by_view` in the same file orders the winbar hint bar per view; `pinned` gates which commands appear there.

## The data-driven per-view installer (the pattern to copy)

Do NOT write `if view_kind == "pr" then …` branches. Keep three flat tables and let visibility/keys/behavior come from their three sources:

```lua
-- COMMON: bound in EVERY status-family view
local COMMON_KEYMAPS = {
  { id = "close",            mode = "n", handler = close_view },
  { id = "toggle",           mode = "n", handler = toggle_fold },
  { id = "collapse_parent",  mode = "n", handler = collapse_parent },
  { id = "visual_line_with_gutter", mode = "n", handler = start_visual_line_gutter },
}

-- PER-VIEW behavior: one entry per binding {id, mode, handler, desc?}
local VIEW_KEYMAPS = {
  status = {
    { id = "stage", mode = "n", handler = stage_under_cursor, desc = "Stage hunk/file" },
    { id = "stage", mode = "x", handler = stage_selection,    desc = "Stage selection" },
    { id = "open",  mode = "n", handler = status_open },
    -- commit / push / pull / pr / branch_create / walkthrough / review / refresh / help …
  },
  pr     = { { id = "comment", mode = { "n", "x" }, handler = pr_add_comment }, --[[ … ]] },
  diff   = { { id = "refresh", mode = "n", handler = refresh_diff }, --[[ … ]] },
  review = { { id = "browse",  mode = { "n", "x" }, handler = review_browse }, --[[ … ]] },
}

-- NON-keymap per-view setup (autocmds, action-set registration, jump-back keys)
local VIEW_SETUP_HOOKS = {
  status = function(buf, _map) --[[ jump-back keys ]] install_cursor_prewarm(buf) end,
  review = function(buf, map)  review().setup_keymaps(buf) --[[ scheduled re-map ]] end,
}
```

The installer reads all three: visibility from `command_specs.views`, keys from the config groups, behavior from the table entry.

```lua
local function setup_status_keymaps(buf)
  local view_kind = session.status and session.status.view_kind or "status"
  local function map(command_id, mode, callback, desc)
    local spec = status_command_specs_by_id[command_id]
    if spec and not status_command_visible(spec) then return end   -- visibility gate
    for _, key in ipairs(status_keys_for(command_id)) do           -- keys from config
      vim.keymap.set(mode, key, callback, {
        buffer = buf, silent = true, nowait = true,
        desc = desc or (spec and spec.desc) or command_id,         -- always a desc
      })
    end
  end
  for _, e in ipairs(COMMON_KEYMAPS)               do map(e.id, e.mode, function() e.handler(buf) end, e.desc) end
  for _, e in ipairs(VIEW_KEYMAPS[view_kind] or {}) do map(e.id, e.mode, function() e.handler(buf) end, e.desc) end
  local hook = VIEW_SETUP_HOOKS[view_kind]; if hook then hook(buf, map) end
end
```

(Real `map` also rebinds `session.status` per-buffer and routes the `review` view through `review().register_command_map` instead of `vim.keymap.set`; the structure is unchanged.) Because visibility gates each call, listing a command in a view where the spec hides it is a harmless no-op.

**Adding a command to a view = two edits:** one entry in `VIEW_KEYMAPS[view]` (behavior) + one spec in `command_specs.lua` (id/label/desc/modes/`views`/optional `keymap`), plus a default key in `config.defaults.keymaps.<group>`. No branching, no per-view key table.

## Why mode-based groups beat per-view key tables

The status family is one family that **intentionally shares muscle memory**: `q`, `<Tab>`, `o`, `R`, `?` mean the same thing in status, pr, diff, and branch-diff. A per-view key table would duplicate those identical bindings four times and invite drift — rebind `open` in one view, forget the others. One `keymaps.status` group keeps all four in lockstep, and `command_specs.views` decides *where each appears* without duplicating *what key it is*.

Review mode is the exception that proves the rule: `S`/`cc` mean different actions there, so it gets its own `keymaps.review` group. Add a third group **only** when a view's vocabulary genuinely diverges — not to vary one or two keys.

Best practice: centralize all in-buffer keys under `opts.keymaps` (the gitsigns/telescope idiom). Never scatter default keys into view modules; a view module owns behavior, the config owns keys.

## Loading with lazy.nvim

`cmd=` lazy-loads on the user commands; `keys=` holds *your* launch bindings; `opts=` is config (including the in-buffer keymap overrides); `config=` runs `setup(opts)` and registers the `:Git*` commands (a packaged plugin would do that in `plugin/`).

```lua
return {
  "you/diff_review",                       -- or dir = vim.fn.stdpath("config") for a local plugin
  dependencies = { "folke/snacks.nvim" },
  cmd = { "GitStatus", "GitBranchDiff", "GitBranchDiffFile",
          "GitFileRevision", "GitDiffCompactPreview" },
  keys = {                                 -- keys that LAUNCH a view (always set desc)
    { "<leader>gs", "<cmd>GitStatus<cr>", desc = "Git status review" },
    { "<leader>gd", function() require("diff_review").open_compact_preview() end,
      desc = "Compact diff preview" },
  },
  opts = {
    about_auto_generate = false,           -- a top-level option (see infra/config.lua defaults)
    keymaps = {                            -- in-buffer ACTION keys, deep-merged per command
      status = {
        discard = "X",                     -- rebind one command
        open    = { "<cr>", "o", "." },    -- a list of keys (keep length ≥ default to drop none)
        walkthrough = false,               -- disable a binding
      },
      review = { submit = "<C-CR>" },      -- the review group, overridden independently
    },
  },
  config = function(_, opts)
    require("diff_review").setup(opts)
    -- register the :Git* user commands here (see lua/plugins/diff_review.lua)
  end,
}
```

Always set `desc` on both `keys=` entries and any custom `vim.keymap.set`; the in-buffer `?` help and which-key read it.
