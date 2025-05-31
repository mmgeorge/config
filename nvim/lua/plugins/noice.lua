return {
  {
    "rcarriga/nvim-notify",
    opts = {
      background_colour = "NotifyBackground",
      fps = 30,
      icons = {
        DEBUG = "",
        ERROR = "",
        INFO = "",
        TRACE = "",
        WARN = ""
      },
      level = 2,
      minimum_width = 20,
      render = "compact",
      stages = "static",
      time_formats = {
        notification = "%T",
        notification_history = "%FT%T"
      },
      timeout = 6000,
      top_down = false
    },
  },
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = {
      -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
      "MunifTanjim/nui.nvim",
      -- OPTIONAL:
      --   `nvim-notify` is only needed, if you want to use the notification view.
      --   If not available, we use `mini` as the fallback
      "rcarriga/nvim-notify",
    },
    opts = {
      routes = {
        -- Redirect all messages as we display them in status line
        {
          filter = {
            event = "msg_show",
            max_height = 1,
            -- error = false,
            -- ["not"] = { find = { "treesitter" } }
          },
          -- filter = { event = "msg_show", kind = "search_count" },
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
        lsp_doc_border = false,        -- add a border to hover docs and signature help
      },
      popupmenu = {
        enabled = false,
      },
      messages = {
        -- We filter all messages (see above) and display them in lualine
        enabled = true,
        -- view = "mini",
        -- view_error = "mini",
        -- view_warn = "mini",
      },
      notify = {
        enabled = false
      },
      lsp = {
        progress = {
          enabled = false
        },
        signature = {
          enabled = false
        },
      }

    },
  }
}
