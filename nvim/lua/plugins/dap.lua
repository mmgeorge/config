return {
  {
    "mfussenegger/nvim-dap", 
    keys = {
      {
        "B",
        function ()
          require"dap".toggle_breakpoint() 
        end,
        mode = { "n", "x" },
        desc = "Set breakpoint",
      }, 
      {
        "<leader>ds",
        function ()
          local widgets = require"dap.ui.widgets"
          widgets.centered_float(widgets.scopes)
        end,
        mode = { "n", "x" },
        desc = "Browse Scopes",
      },
      {
        "<leader>dt",
        function ()
          local widgets = require"dap.ui.widgets"
          widgets.centered_float(widgets.threads)
        end,
        mode = { "n", "x" },
        desc = "Browse Threads",
      },
      {
        "<leader>df",
        function ()
          local widgets = require"dap.ui.widgets"
          widgets.centered_float(widgets.threads)
        end,
        mode = { "n", "x" },
        desc = "Browse Frames",
      },
      {
        "<leader>de",
        function ()
          local widgets = require"dap.ui.widgets"
          widgets.centered_float(widgets.expression)
        end,
        mode = { "n", "x" },
        desc = "Browse Expressions",
      },

    },
    config = function ()
      require("dap").defaults.fallback.terminal_win_cmd = '10split new'; 
    end
  },
  {
    "rcarriga/nvim-dap-ui",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
    },
    keys = {
      {
        "<leader>db",
        function ()
          require('dapui').float_element("breakpoints", {
            title = "Breakpoints",
            enter = true,
            height = 10, 
            width = 60, 
            position = "center"
          }) 
        end,
        mode = { "n", "x" },
        desc = "Browse breakpoints",
      }
    },
    config = function () 
      require("dapui").setup({
        controls = {
          element = "breakpoints",
          enabled = false,
          icons = {
            disconnect = "",
            pause = "",
            play = "",
            run_last = "",
            step_back = "",
            step_into = "",
            step_out = "",
            step_over = "",
            terminate = ""
          }
        },
        element_mappings = {},
        expand_lines = true,
        floating = {
          border = "single",
          mappings = {
            -- edit = "de",
            -- expand = { "<Tab>", "<2-LeftMouse>" },
            -- open = "<CR>",
            -- remove = "jj",
            -- repl = "dr",
            -- toggle = "dt",
            close = { "q", "<Esc>" }
          }
        },
        force_buffers = true,
        icons = {
          collapsed = "",
          current_frame = "",
          expanded = ""
        },
        layouts = { 
          {
            elements = { 
              {
                id = "watches",
                size = 0.25
              }, 
              {
                id = "stacks",
                size = 0.25
              }, 
              {
                id = "scopes",
                size = 0.25
              },
              {
                id = "breakpoints",
                size = 0.10
              }, 
            },
            position = "right",
            size = 35
          }, 
          {
            elements = { 
              {
                id = "repl",
                size = 0.5
              }, 
              {
                id = "console",
                size = 0.5
              }
            },
            position = "bottom",
            size = 20
          } 
        },
        mappings = {
          edit = "de",
          expand = {
            -- "<Tab>",
            "<2-LeftMouse>",
          },
          open = "<CR>",
          remove = "jj",
          repl = "dr",
          toggle = "<Tab>",
        },
        render = {
          indent = 1,
          max_value_lines = 100
        }
      } 
      )
    end,
  },
  {
    "theHamsta/nvim-dap-virtual-text",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function ()
      require("nvim-dap-virtual-text").setup({
        enabled = true,                        -- enable this plugin (the default)
        enabled_commands = false,              -- create commands DapVirtualTextEnable,         
        highlight_changed_variables = true,    -- highlight changed values with NvimDapVirtualTextChanged                                                  , else always NvimDapVirtualText
        highlight_new_as_changed = false,      -- highlight new variables in the same way as changed 
        show_stop_reason = true,               -- show stop reason when stopped for exceptions
        commented = false,                     -- prefix virtual text with comment string
        only_first_definition = true,          -- only show virtual text at first definition 
        all_references = false,                -- show virtual text on all all references of the var 
        clear_on_continue = false,             -- clear virtual text on "continue" 
        display_callback = function(variable, buf, stackframe, node, options)
          -- by default, strip out new line characters
          if options.virt_text_pos == 'inline' then
            return ' = ' .. variable.value:gsub("%s+", " ")
          else
            return variable.name .. ' = ' .. variable.value:gsub("%s+", " ")
          end
        end,
        -- position of virtual text, see `:h nvim_buf_set_extmark()`, default tries to inline the 
        -- virtual text. Use 'eol' to set to end of line
        -- virt_text_pos = vim.fn.has 'nvim-0.10' == 1 and 'inline' or 'eol',
        virt_text_pos = 'eol',
        -- experimental features:
        all_frames = false,                    -- show virtual text for all stack frames 
        virt_lines = false,                    -- show virtual lines instead of virtual text (flickers!)
        virt_text_win_col = nil                -- position the virtual text at a fixed window column 
        -- (starting from the first text column) ,
        -- e.g. 80 to position at column 80, 
        -- see `:h nvim_buf_set_extmark()`
      })
    end
  }
}
