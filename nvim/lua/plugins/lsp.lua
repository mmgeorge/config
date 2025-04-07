return {
  -- Lsp package manager
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end
  },

  -- Bridge mason.nvim & lspconfig
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        automatic_installation = true,
        ensure_installed = {
          "lua_ls",
          "ts_ls",
          "eslint",
          -- "rust_analyzer",
          "terraformls",
          -- "tailwind-language-server",
          -- "tailwindcss",
        }

      })
    end
  },

  -- Install mason tools automatically -- 
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    config = function()
      require("mason-tool-installer").setup({
        -- ensure_installed = { "stylua", "prettierd" },
        ensure_installed = { "prettierd" },
        auto_update = true,
        run_on_start = true,
        debounce_hours = 5, 
      })
    end
  },

  -- Setup servers via lspconfig --
  {
    "neovim/nvim-lspconfig",
    config = function()
      -- local lspconfig = require("lspconfig")
      -- lspconfig.lua_ls.setup({})
      -- lspconfig.rust_analyzer.setup({})
      -- lspconfig.tsserver.setup({})
      -- lspconfig.eslint.setup({})
    end
  },
  
  {
    "smjonas/inc-rename.nvim",
    config = function ()
      require("inc_rename").setup ({
        -- the name of the command
        cmd_name = "IncRename",
        -- the highlight group used for highlighting the identifier's new name
        hl_group = "Substitute",
        -- whether an empty new name should be previewed; if false the command preview will be cancelled instead
        preview_empty_name = false,
        -- whether to display a `Renamed m instances in n files` message after a rename operation
        show_message = true,
        -- whether to save the "IncRename" command in the commandline history (set to false to prevent issues with
        -- navigating to older entries that may arise due to the behavior of command preview)
        save_in_cmdline_history = true,
        -- the type of the external input buffer to use (the only supported value is currently "dressing")
        input_buffer_type = nil,
        -- callback to run after renaming, receives the result table (from LSP handler) as an argument
        post_hook = nil,
      })
    end
  },

  -- Log startup status --
  {
    "j-hui/fidget.nvim",
    opts = {
      -- options
    },
  },
   
  -- Better rename UI --
  -- {
  --   "filipdutescu/renamer.nvim",
  --   dependencies = { "nvim-lua/plenary.nvim" },
  --   keys = {
  --     {
  --     "<leader>f",
  --     function() require("renamer").rename() end,
  --     mode = { "n", "x" },
  --     desc = "Rename",
  --
  --     }
  --   },
  --   config = function ()
  --     require("renamer").setup({
  --       -- The popup title, shown if `border` is true
  --       title = 'Rename',
  --       -- The padding around the popup content
  --       padding = {
  --         top = 0,
  --         left = 0,
  --         bottom = 0,
  --         right = 0,
  --       },
  --       -- The minimum width of the popup
  --       min_width = 15,
  --       -- The maximum width of the popup
  --       max_width = 45,
  --       -- Whether or not to shown a border around the popup
  --       border = true,
  --       -- The characters which make up the border
  --       border_chars = { 'Ä', '³', 'Ä', '³', '?', '?', '?', '?' },
  --       -- Whether or not to highlight the current word references through LSP
  --       show_refs = true,
  --       -- Whether or not to add resulting changes to the quickfix list
  --       with_qf_list = true,
  --       -- Whether or not to enter the new name through the UI or Neovim's `input`
  --       -- prompt
  --       with_popup = true,
  --       -- The keymaps available while in the `renamer` buffer. The example below
  --       -- overrides the default values, but you can add others as well.
  --       mappings = {
  --         -- ['<c-i>'] = mappings_utils.set_cursor_to_start,
  --         -- ['<c-a>'] = mappings_utils.set_cursor_to_end,
  --         -- ['<c-e>'] = mappings_utils.set_cursor_to_word_end,
  --         -- ['<c-b>'] = mappings_utils.set_cursor_to_word_start,
  --         -- ['<c-c>'] = mappings_utils.clear_line,
  --         -- ['<c-u>'] = mappings_utils.undo,
  --         -- ['<c-r>'] = mappings_utils.redo,
  --       },
  --       -- Custom handler to be run after successfully renaming the word. Receives
  --       -- the LSP 'textDocument/rename' raw response as its parameter.
  --       handler = nil,
  --     })
  --   end
  -- },

  -- {
  --    "aznhe21/actions-preview.nvim",
  --    config = function()
  --       -- vim.keymap.set({ "v", "n" }, "olf", require("actions-preview").code_actions)
  --    end,
  -- },

  -- {
  --    'filipdutescu/renamer.nvim',
  --    branch = 'master',
  --    requires = { {'nvim-lua/plenary.nvim'} }
  -- },


  -- Very cool idea, but visual distracting. Make as a toggle?
  -- Could toggle, see: <https://github.com/ErichDonGubler/lsp_lines.nvim>
  -- {
  --    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  --    config = function()
  --       require("lsp_lines").setup()
  --    end,
  -- }
  
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    -- branch = "dev",
    keys = {
      -- {
      --   "<tab>",
      --   "<cmd>Trouble diagnostics toggle<cr>",
      --   desc = "Diagnostics (Trouble)",
      -- }
    },
    -- config = function ()
    --   -- Open Trouble qflist when qf list opens
    --   vim.api.nvim_create_autocmd("BufRead", {
    --     callback = function(ev)
    --       if vim.bo[ev.buf].buftype == "quickfix" then
    --         vim.schedule(function()
    --           vim.cmd([[cclose]])
    --           vim.cmd([[Trouble qflist open]])
    --         end)
    --       end
    --     end,
    --   }) 
    -- end,
    opts = {
      -- warn_no_results = false,
      -- open_no_results = true,
      auto_close = false, -- auto close when there are no items
      -- auto_open = true, -- auto open when there are items
      auto_preview = true, -- automatically open preview when on an item
      auto_refresh = true, -- auto refresh when open
      focus = true, -- Focus the window when opened
      restore = true, -- restores the last location in the list when opening
      follow = true, -- Follow the current item
      indent_guides = true, -- show indent guides
      max_items = 200, -- limit number of items that can be displayed per section
      multiline = false, -- render multi-line messages
      pinned = false, -- When pinned, the opened trouble window will be bound to the current buffer
      ---@type trouble.Window.opts
      win = {}, -- window options for the results window. Can be a split or a floating window.
      -- Window options for the preview window. Can be a split, floating window,
      -- or `main` to show the preview in the main editor window.
      ---@type trouble.Window.opts
      preview = { type = "main" },
      -- Throttle/Debounce settings. Should usually not be changed.
      ---@type table<string, number|{ms:number, debounce?:boolean}>
      throttle = {
        refresh = 20, -- fetches new data when needed
        update = 10, -- updates the window
        render = 10, -- renders the window
        follow = 10, -- follows the current item
        preview = { ms = 100, debounce = true }, -- shows the preview for the current item
      },
      -- Key mappings can be set to the name of a builtin action,
      -- or you can define your own custom action.
      ---@type table<string, string|trouble.Action>
      keys = {
        ["?"] = "help",
        r = "refresh",
        R = "toggle_refresh",
        q = "close",
        o = "jump_close",
        ["<esc>"] = "cancel",
        ["<cr>"] = "jump",
        ["<2-leftmouse>"] = "jump",
        -- ["<c-s>"] = "jump_split",
        -- ["<c-v>"] = "jump_vsplit",
        -- go down to next item (accepts count)
        -- j = "next",
        -- ["]]"] = "next",
        -- go up to prev item (accepts count)
        -- k = "prev",
        ["s"] = "prev",
        ["t"] = "next",
        -- ["[["] = "prev",
        -- i = "inspect",
        -- p = "preview",
        P = "toggle_preview",
        zo = "fold_open",
        zO = "fold_open_recursive",
        zc = "fold_close",
        zC = "fold_close_recursive",
        za = "fold_toggle",
        zA = "fold_toggle_recursive",
        zm = "fold_more",
        zM = "fold_close_all",
        zr = "fold_reduce",
        zR = "fold_open_all",
        zx = "fold_update",
        zX = "fold_update_all",
        zn = "fold_disable",
        zN = "fold_enable",
        zi = "fold_toggle_enable",
        ["F"] = { -- example of a custom action that toggles the active view filter
          action = function(view)
            view:filter({ buf = 0 }, { toggle = true })
          end,
          desc = "Toggle Current Buffer Filter",
        },
        ["f"] = { -- example of a custom action that toggles the severity
          action = function(view)
            local f = view:get_filter("severity")
            local severity = ((f and f.filter.severity or 0) + 1) % 5
            view:filter({ severity = severity }, {
              id = "severity",
              template = "{hl:Title}Filter:{hl} {severity}",
              del = severity == 0,
            })
          end,
          desc = "Toggle Severity Filter",
        },
      },
      ---@type table<string, trouble.Mode>
      modes = {
        symbols = {
          desc = "document symbols",
          mode = "lsp_document_symbols",
          focus = false,
          win = { position = "right" },
          filter = {
            -- remove Package since luals uses it for control flow structures
            ["not"] = { ft = "lua", kind = "Package" },
            any = {
              -- all symbol kinds for help / markdown files
              ft = { "help", "markdown" },
              -- default set of symbol kinds
              kind = {
                "Class",
                "Constructor",
                "Enum",
                "Field",
                "Function",
                "Interface",
                "Method",
                "Module",
                "Namespace",
                "Package",
                "Property",
                "Struct",
                "Trait",
              },
            },
          },
        },
      },
    } 
  }
}
