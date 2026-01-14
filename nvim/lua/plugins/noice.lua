return {
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = {
      -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
      "MunifTanjim/nui.nvim",
    },
    opts = {
      routes = {
        -- Redirect all messages as we display them in status line
        {
          filter = {
            event = "msg_show",
            max_height = 1,
            -- ["not"] = { find = { "treesitter" } }
          },
          opts = { skip = true },
        },
        {
          filter = {
            error = true,
          },
          opts = { skip = true },
        },
      },
      format = {
        -- default = { "{level} ", "{title} ", "{message}" }
        default = { "{message}" }
      },
      views = {

      },
      presets = {
        bottom_search = true, -- use a classic bottom cmdline for search
        command_palette = true,
        -- command_palette = {
        --   views = {
        --     cmdline_popup = {
        --       position = {
        --         row = "100%",
        --         col = "0%",
        --       },
        --       size = {
        --         min_width = 60,
        --         width = "auto",
        --         height = "auto",
        --       },
        --     },
        --   },
        -- },
        -- mand_palette = true,       -- position the cmdline and popupmenu together
        long_message_to_split = false, -- long messages will be sent to a split
        inc_rename = true,             -- enables an input dialog for inc-rename.nvim
        lsp_doc_border = true,         -- add a border to hover docs and signature help
        view_history = "messages",     -- view for :messages
      },
      popupmenu = {
        enabled = false,
      },
      messages = {
        -- We filter all messages (see above) and display them in lualine
        enabled = true,
      },
      notify = {
        enabled = false
      },
      lsp = {
        progress = {
          enabled = false
        },
        signature = {
          enabled = false,
        },
      }
    },
  }
}
