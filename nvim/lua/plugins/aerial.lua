local kinds = {
  full = {
    "Class",
    "Constructor",
    "Enum",
    "Function",
    "Method",
    "Interface",
    "Module",
    "Namespace",
    "Package",
    "Object",
    "Struct",
    "Property",
    "EnumMember",
    "Variable",
    "Constant",
  },
  minimal = {
    "Class",
    "Constructor",
    "Enum",
    "Function",
    "Method",
    "Interface",
    "Module",
    "Namespace",
    "Package",
    "Object",
    "Struct",
  },
}

return {
  {
    "onsails/lspkind.nvim",
    opts = {}
  },
  {
    "stevearc/aerial.nvim",
    keys = {
      {
        "oo",
        function()
          local mode = "minimal"
          local level = 2

          require("aerial").snacks_picker({
            win = {
              input = {
                keys = {
                  ["<C-o>"] = { "symbol_toggle", mode = { "n", "i" } },
                  ["<C-u>"] = { "level_toggle", mode = { "n", "i" } },
                  ["<C-l>"] = { "symbol_fold", mode = { "n", "i" } },
                }
              }
            },
            actions = {
              symbol_fold = {
                name = "symbol_fold",
                desc = "Symbol Fold",
                action = function(self, item)
                  -- print(vim.inspect(item.item))
                  -- Iterate over items and set visible to false
                  -- for _, item in ipairs(self:items()) do
                  -- item.hidden = true
                  self:find() -- rerun transfrom
                end
              },
              symbol_toggle = {
                name = "symbol_toggle",
                desc = "Symbol Toggle",
                action = function(self, item)
                  if mode == "full" then
                    mode = "minimal"
                  elseif mode == "minimal" then
                    mode = "full"
                  end
                  self:find() -- rerun transfrom
                end
              },
              level_toggle = {
                name = "level_toggle",
                desc = "Level Toggle",
                action = function(self, item)
                  -- Toggle level
                  if level == 1 then
                    level = 2
                  elseif level == 2 then
                    level = 1
                  end
                  self:find() -- rerun transfrom
                end
              }
            },
            transform = function(item, context)
              -- Check if the item's kind is in the allowed kinds array
              local has_kind = vim.tbl_contains(kinds[mode], item.item.kind)
              return not item.hidden and has_kind and item.item.level < level
            end,
            format = function(item, picker)
              local config = require("aerial.config")
              local highlight = require("aerial.highlight")
              local collapsed = false

              ---@type aerial.Symbol
              local symbol = item.item
              local icon = config.get_icon(bufnr, symbol.kind, collapsed)
              local icon_hl = highlight.get_highlight(symbol, true, false) or "NONE"

              local ret = {} ---@type snacks.picker.Highlight[]
              vim.list_extend(ret, Snacks.picker.format.tree(item, picker))
              table.insert(ret, { icon .. " ", icon_hl })

              local text = item.text

              -- local function_info = item.item.function_info
              -- if function_info then
              --   local params = table.concat(function_info.parameter_types, ", ")
              --   local function_str = "(" .. params .. ")"
              --   if function_info.return_type then
              --     function_str = function_str .. " -> " .. function_info.return_type
              --   end
              --
              --   text = string.format("%-10s", text) .. " " .. function_str
              -- end

              Snacks.picker.highlight.format(item, text, ret)

              -- print(vim.inspect(ret))

              return ret
            end,

          })
        end
      }
    },
    opts = {
      -- Priority list of preferred backends for aerial.
      -- This can be a filetype map (see :help aerial-filetype-map)
      -- backends = { "treesitter", "lsp", "markdown", "asciidoc", "man" },
      backends = { "treesitter", "markdown", "asciidoc", "man" },

      layout = {
        max_width = { 40, 0.2 },
        width = nil,
        min_width = 10,
        win_opts = {},
        default_direction = "prefer_right",
        placement = "window",
        resize_to_content = true,
        preserve_equality = false,
      },

      -- Determines how the aerial window decides which buffer to display symbols for
      --   window - aerial window will display symbols for the buffer in the window from which it was opened
      --   global - aerial window will display symbols for the current window
      attach_mode = "window",

      -- List of enum values that configure when to auto-close the aerial window
      --   unfocus       - close aerial when you leave the original source window
      --   switch_buffer - close aerial when you change buffers in the source window
      --   unsupported   - close aerial when attaching to a buffer that has no symbol source
      close_automatic_events = {},

      -- Keymaps in aerial window. Can be any value that `vim.keymap.set` accepts OR a table of keymap
      -- options with a `callback` (e.g. { callback = function() ... end, desc = "", nowait = true })
      -- Additionally, if it is a string that matches "actions.<name>",
      -- it will use the mapping at require("aerial.actions").<name>
      -- Set to `false` to remove a keymap
      keymaps = {
        ["?"] = "actions.show_help",
        ["g?"] = "actions.show_help",
        ["<CR>"] = "actions.jump",
        ["<2-LeftMouse>"] = "actions.jump",
        ["<C-v>"] = "actions.jump_vsplit",
        ["<C-s>"] = "actions.jump_split",
        ["p"] = "actions.scroll",
        ["<C-j>"] = "actions.down_and_scroll",
        ["<C-k>"] = "actions.up_and_scroll",
        ["{"] = "actions.prev",
        ["}"] = "actions.next",
        ["[["] = "actions.prev_up",
        ["]]"] = "actions.next_up",
        ["q"] = "actions.close",
        ["o"] = "actions.tree_toggle",
        ["za"] = "actions.tree_toggle",
        ["O"] = "actions.tree_toggle_recursive",
        ["zA"] = "actions.tree_toggle_recursive",
        ["l"] = "actions.tree_open",
        ["zo"] = "actions.tree_open",
        ["L"] = "actions.tree_open_recursive",
        ["zO"] = "actions.tree_open_recursive",
        ["h"] = "actions.tree_close",
        ["zc"] = "actions.tree_close",
        ["H"] = "actions.tree_close_recursive",
        ["zC"] = "actions.tree_close_recursive",
        ["zr"] = "actions.tree_increase_fold_level",
        ["zR"] = "actions.tree_open_all",
        ["zm"] = "actions.tree_decrease_fold_level",
        ["zM"] = "actions.tree_close_all",
        ["zx"] = "actions.tree_sync_folds",
        ["zX"] = "actions.tree_sync_folds",
      },

      -- When true, don't load aerial until a command or function is called
      -- Defaults to true, unless `on_attach` is provided, then it defaults to false
      lazy_load = true,

      -- Disable aerial on files with this many lines
      disable_max_lines = 10000,

      -- Disable aerial on files this size or larger (in bytes)
      disable_max_size = 2000000, -- Default 2MB

      -- A list of all symbols to display. Set to false to display all symbols.
      -- This can be a filetype map (see :help aerial-filetype-map)
      -- To see all available values, see :help SymbolKind
      filter_kind = kinds.full,

      -- Determines line highlighting mode when multiple splits are visible.
      -- split_width   Each open window will have its cursor location marked in the
      --               aerial buffer. Each line will only be partially highlighted
      --               to indicate which window is at that location.
      -- full_width    Each open window will have its cursor location marked as a
      --               full-width highlight in the aerial buffer.
      -- last          Only the most-recently focused window will have its location
      --               marked in the aerial buffer.
      -- none          Do not show the cursor locations in the aerial window.
      highlight_mode = "split_width",

      -- Highlight the closest symbol if the cursor is not exactly on one.
      highlight_closest = true,

      -- Highlight the symbol in the source buffer when cursor is in the aerial win
      highlight_on_hover = false,

      -- When jumping to a symbol, highlight the line for this many ms.
      -- Set to false to disable
      highlight_on_jump = 300,

      -- Jump to symbol in source window when the cursor moves
      autojump = false,

      icons = {
        rust = {
          Class = "impl"
        }
      },

      -- Control which windows and buffers aerial should ignore.
      -- Aerial will not open when these are focused, and existing aerial windows will not be updated
      ignore = {
        unlisted_buffers = false,
        diff_windows = true,
        -- List of filetypes to ignore.
        filetypes = {},

        -- Ignored buftypes.
        -- Can be one of the following:
        -- false or nil - No buftypes are ignored.
        -- "special"    - All buffers other than normal, help and man page buffers are ignored.
        -- table        - A list of buftypes to ignore. See :help buftype for the
        --                possible values.
        -- function     - A function that returns true if the buffer should be
        --                ignored or false if it should not be ignored.
        --                Takes two arguments, `bufnr` and `buftype`.
        buftypes = "special",

        -- Ignored wintypes.
        -- Can be one of the following:
        -- false or nil - No wintypes are ignored.
        -- "special"    - All windows other than normal windows are ignored.
        -- table        - A list of wintypes to ignore. See :help win_gettype() for the
        --                possible values.
        -- function     - A function that returns true if the window should be
        --                ignored or false if it should not be ignored.
        --                Takes two arguments, `winid` and `wintype`.
        wintypes = "special",
      },

      -- Use symbol tree for folding. Set to true or false to enable/disable
      -- Set to "auto" to manage folds if your previous foldmethod was 'manual'
      -- This can be a filetype map (see :help aerial-filetype-map)
      manage_folds = false,

      -- When you fold code with za, zo, or zc, update the aerial tree as well.
      -- Only works when manage_folds = true
      link_folds_to_tree = false,

      -- Fold code when you open/collapse symbols in the tree.
      -- Only works when manage_folds = true
      link_tree_to_folds = true,

      -- Call this function when aerial attaches to a buffer.
      on_attach = function(bufnr) end,

      -- Call this function when aerial first sets symbols on a buffer.
      on_first_symbols = function(bufnr) end,

      open_automatic = false,

      -- Run this command after jumping to a symbol (false will disable)
      post_jump_cmd = "normal! zz",

      -- Invoked after each symbol is parsed, can be used to modify the parsed item,
      -- or to filter it by returning false.
      --
      -- bufnr: a neovim buffer number
      -- item: of type aerial.Symbol
      -- ctx: a record containing the following fields:
      --   * backend_name: treesitter, lsp, man...
      --   * lang: info about the language
      --   * symbols?: specific to the lsp backend
      --   * symbol?: specific to the lsp backend
      --   * syntax_tree?: specific to the treesitter backend
      --   * match?: specific to the treesitter backend, TS query matc        post_parse_symbol = function(bufnr, item, ctx) Check if node is a function type and not nil
      -- post_parse_symbol = function(bufnr, item, ctx)
      --   local node = ctx.match.symbol.node
      --
      --   local func_types = {
      --     "function_declaration",
      --     "method_definition",
      --     -- "function_item"
      --   }
      --
      --   if node and vim.tbl_contains(func_types, node:type()) then
      --     function get_annotation_type(node)
      --       local type = node:child(1)
      --       if type then
      --         return vim.treesitter.get_node_text(type, bufnr)
      --       end
      --     end
      --
      --     function get_parameter_type(node)
      --       local type_annotation = node:field("type")[1]
      --       if type_annotation then
      --         return get_annotation_type(type_annotation)
      --       end
      --     end
      --
      --     function get_parameter_types(node)
      --       local out = {}
      --       local params = node:field("parameters")[1]
      --       for param, _ in params:iter_children() do
      --         local type = get_parameter_type(param)
      --         if type then
      --           table.insert(out, type)
      --         end
      --       end
      --       return out
      --     end
      --
      --     local function_info = {}
      --     function_info.parameter_types = get_parameter_types(node)
      --     local ret = node:field("return_type")[1] -- 'return_type' is a common field name, check grammar
      --     if ret then
      --       function_info.return_type = get_annotation_type(ret)
      --     end
      --
      --     item.function_info = function_info
      --   end
      --
      --   return true
      -- end,


      -- Invoked after all symbols have been parsed and post-processed,
      -- allows to modify the symbol structure before final display
      --
      -- bufnr: a neovim buffer number
      -- items: a collection of aerial.Symbol items, organized in a tree,
      --        with 'parent' and 'children' fields
      -- ctx: a record containing the following fields:
      --   * backend_name: treesitter, lsp, man...
      --   * lang: info about the language
      --   * symbols?: specific to the lsp backend
      --   * syntax_tree?: specific to the treesitter backend
      -- post_add_all_symbols = function(bufnr, items, ctx)
      --   if items and type(items) == 'table' then
      --     for _, top_level_item in ipairs(items) do
      --       if type(top_level_item) == 'table' then
      --         local immediate_children = top_level_item.children
      --         if immediate_children and type(immediate_children) == 'table' then
      --           for _, child_item in ipairs(immediate_children) do
      --             if type(child_item) == 'table' then
      --               child_item.children = {}
      --             end
      --           end
      --         end
      --       end
      --     end
      --   end
      --
      --   return items
      -- end,


      -- When true, aerial will automatically close after jumping to a symbol
      close_on_select = false,

      -- The autocmds that trigger symbols update (not used for LSP backend)
      update_events = "TextChanged,InsertLeave",

      -- Show box drawing characters for the tree hierarchy
      show_guides = false,

      -- Set this function to override the highlight groups for certain symbols
      -- get_highlight = function(symbol, is_icon, is_collapsed)
      --   -- return "MyHighlight" .. symbol.kind
      -- end,

      lsp = {
        -- If true, fetch document symbols when LSP diagnostics update.
        diagnostics_trigger_update = false,

        -- Set to false to not update the symbols when there are LSP errors
        update_when_errors = true,

        -- How long to wait (in ms) after a buffer change before updating
        -- Only used when diagnostics_trigger_update = false
        update_delay = 300,

        -- Map of LSP client name to priority. Default value is 10.
        -- Clients with higher (larger) priority will be used before those with lower priority.
        -- Set to -1 to never use the client.
        priority = {
          -- pyright = 10,
        },
      },

      treesitter = {
        -- How long to wait (in ms) after a buffer change before updating
        update_delay = 300,
      },

      markdown = {
        -- How long to wait (in ms) after a buffer change before updating
        update_delay = 300,
      },

      asciidoc = {
        -- How long to wait (in ms) after a buffer change before updating
        update_delay = 300,
      },

      man = {
        -- How long to wait (in ms) after a buffer change before updating
        update_delay = 300,
      },
    }
  }
}
