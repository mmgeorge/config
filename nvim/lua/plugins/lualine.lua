return {
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons'
    },
    config = function()
      local colors = {
        red = "#ef3573",
        green = "#5bff94",
        yellow = "#ffe18e",
        gray = "#696969",
        white = "#e9e9e9",
        light_blue = "#3ec5ff",
        light_gray = "#b4b4b4",
        teal = "#9dffe7"
      }

      local black = "#000000"

      local function getColors(color)
        return {
          a = { bg = color, fg = black },
          b = { bg = black, fg = color },
          c = { bg = black, fg = color }
        }
      end

      local theme = {
        -- normal = getColors(colors.white),
        -- insert = getColors(colors.green),
        -- visual = getColors(colors.red),
        -- command = getColors(colors.yellow),
        -- replace = getColors(colors.white),
        -- terminal = getColors(colors.white),
        -- inactive = getColors(colors.gray),
        normal = getColors(colors.white),
        insert = getColors(colors.white),
        visual = getColors(colors.white),
        command = getColors(colors.white),
        replace = getColors(colors.white),
        terminal = getColors(colors.white),
        inactive = getColors(colors.gray),

      }

      require('lualine').setup({
        options = {
          icons_enabled = true,
          theme = theme,
          -- section_separators = { left = '', right = '' },
          -- section_separators = { left = '', right = '' },
          section_separators = '',
          component_separators = '',
          -- component_separators = { left = '', right = '' },
          -- component_separators = { left = '', right = '' },
          -- section_separators = { left = '', right = '' },
          -- component_separators = { left = '', right = ''},
          -- section_separators = { left = '', right = ''},
          disabled_filetypes = {
            winbar = {
              "snacks_picker_input",
              "snacks_picker_list",
              "snacks_layout_box",
              "NeogitStatus"
            },
            statusline = {
              -- "snacks_picker_input",
              -- "snacks_picker_list",
              -- "snacks_layout_box",
              -- "NeogitStatus"
            },
          },
          ignore_focus = {},
          always_divide_middle = false,
          always_show_tabline = false,
          globalstatus = true,
          refresh = {
            statusline = 50,
            tabline = 100,
            winbar = 100,
          }
        },
        -- inactive_winbar = {
        --   lualine_a = {
        --     {
        --       separator = { left = '', right = '' },
        --       right_padding = 2,
        --       'filename',
        --       file_status = true,    -- Displays file status (readonly status, modified status)
        --       newfile_status = true, -- Display new file status (new file means no write after created)
        --       path = 1,              -- 0: Just the filename
        --       -- 1: Relative path
        --       -- 2: Absolute path
        --       -- 3: Absolute path, with tilde as the home directory
        --       -- 4: Filename and parent dir, with tilde as the home directory
        --
        --       shorting_target = 40, -- Shortens path to leave 40 spaces in the window
        --       -- for other components. (terrible name, any suggestions?)
        --       symbols = {
        --         modified = '[+]',      -- Text to show when the file is modified.
        --         readonly = '[-]',      -- Text to show when the file is non-modifiable or readonly.
        --         unnamed = '[No Name]', -- Text to show for unnamed buffers.
        --         newfile = '[New]',     -- Text to show for newly created file before first write
        --       }
        --     }
        --   },
        --   lualine_b = {
        --   },
        --   lualine_c = {
        --   },
        --   lualine_x = {
        --     {
        --       "overseer",
        --       label = "",         -- Prefix for task counts
        --       colored = true,     -- Color the task icons and counts
        --       unique = false,     -- Unique-ify non-running task count by name
        --       name = nil,         -- List of task names to search for
        --       name_not = false,   -- When true, invert the name search
        --       status = nil,       -- List of task statuses to display
        --       status_not = false, -- When true, invert the status search
        --     },
        --     {
        --       'diagnostics',
        --       sections = { 'error', 'warn' }
        --     }
        --
        --     -- 'encoding',
        --     -- 'fileformat',
        --     -- 'filetype'
        --   },
        --   lualine_y = {},
        --   lualine_z = {
        --     {
        --       'filetype',
        --       separator = { left = '', right = '' },
        --       left_padding = 2,
        --     }
        --
        --     -- 'branch',
        --     -- 'diff',
        --   }
        -- },
        --
        extensions = {},
        sections = {
          -- lualine_a = {},
          lualine_a = {
          },
          lualine_b = {
            {
              'mode',
              cond = function()
                return vim.api.nvim_get_mode().mode ~= 'n'
              end
            },
            {
              function()
                local message = require("noice").api.status.message.get()
                local newline = string.find(message, "\n")

                if newline then
                  return ""
                end

                return message
                -- local message_len = #message
                -- -- Truncate message to 80 chars or before any newline
                -- if message then
                --   local newline_pos = string.find(message, "\n")
                --   local cutoff = 100
                --   if newline_pos and newline_pos - 1 < cutoff then
                --     cutoff = newline_pos - 1
                --   end
                --   if #message > cutoff then
                --     message = string.sub(message, 1, cutoff)
                --   end
                -- end
                --
                -- if message:sub(#message, #message) == ":" then
                --   message = message:sub(1, #message - 1)
                -- end
                --
                -- if #message ~= message_len then
                --   message = message .. "..."
                -- end
                --
                -- return message
              end,
              cond = require("noice").api.status.message.has,
            }
          },
          lualine_c = {
            -- 'branch',
            -- 'diff',
            {
              'diagnostics',
              sections = { 'error', 'warn' }
            }
          },
          lualine_x = {
            {
              "overseer",
              label = "",         -- Prefix for task counts
              colored = true,     -- Color the task icons and counts
              unique = false,     -- Unique-ify non-running task count by name
              name = nil,         -- List of task names to search for
              name_not = false,   -- When true, invert the name search
              status = nil,       -- List of task statuses to display
              status_not = false, -- When true, invert the status search
            },
            -- 'encoding',
            -- 'fileformat',
            'filetype'
          },
          lualine_y = { 'progress' },
          lualine_z = { 'location' }
        },
        -- sections = {
        --   lualine_a = {},
        --   lualine_b = {},
        --   lualine_c = {
        --   },
        --   lualine_x = {},
        --   lualine_y = {},
        --   lualine_z = {}
        -- },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {
          },
          lualine_x = {},
          lualine_y = {},
          lualine_z = {}
        },
        winbar = {},
        inactive_winbar = {}
      })
    end
  }
}
