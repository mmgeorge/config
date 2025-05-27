return {
  {
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'projekt0n/github-nvim-theme',
      'nvim-tree/nvim-web-devicons' },
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

      local black = "#0d1117"

      local function getColors(color)
        return {
          a = { bg = color, fg = black },
          b = { bg = black, fg = color },
          c = { bg = black, fg = color }
        }
      end

      local lualine_theme = require('github-theme.util.lualine')('github_dark_default')
      -- lualine_theme.normal = getColors(colors.white)
      -- lualine_theme.insert = getColors(colors.green)
      -- lualine_theme.visual = getColors(colors.red)
      -- lualine_theme.command = getColors(colors.yellow)
      -- lualine_theme.replace = getColors(colors.light_blue)
      -- lualine_theme.terminal = getColors(colors.teal)

      -- Disable color change -- looks laggy
      lualine_theme.normal = getColors(colors.white)
      lualine_theme.insert = getColors(colors.white)
      lualine_theme.visual = getColors(colors.white)
      lualine_theme.command = getColors(colors.white)
      lualine_theme.replace = getColors(colors.white)
      lualine_theme.terminal = getColors(colors.white)

      require('lualine').setup({
        theme = lualine_theme,
        options = {
          icons_enabled = true,
          theme = lualine_theme,
          component_separators = { left = '', right = '' },
          section_separators = { left = '', right = '' },
          -- component_separators = { left = '', right = ''},
          -- section_separators = { left = '', right = ''},
          disabled_filetypes = {
            winbar = {
              "snacks_picker_input",
              "snacks_picker_list",
              "snacks_layout_box",
              "NeogitStatus"
            },
          },
          ignore_focus = {},
          always_divide_middle = true,
          always_show_tabline = false,
          globalstatus = false,
          refresh = {
            statusline = 100,
            tabline = 100,
            winbar = 100,
          }
        },
        -- tabline = {
        --   lualine_a = {
        --     -- 'filetype'
        --
        --   },
        --   lualine_b = {
        --     {
        --       "tabs",
        --       max_tabs_length = 40,
        --       max_length = 80,
        --       mode = 1,
        --       path = 1,
        --       show_modified_status = false,
        --       -- cond = function()
        --       -- return #vim.fn.gettabinfo() > 1
        --       -- end,
        --       -- symbols = {
        --       -- }
        --     }
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
        --     -- 'branch',
        --     -- 'diff',
        --   }
        -- },
        winbar = {
          lualine_a = {
            -- 'filetype'

          },
          lualine_b = {
            {
              'filename',
              file_status = true,    -- Displays file status (readonly status, modified status)
              newfile_status = true, -- Display new file status (new file means no write after created)
              path = 1,              -- 0: Just the filename
              -- 1: Relative path
              -- 2: Absolute path
              -- 3: Absolute path, with tilde as the home directory
              -- 4: Filename and parent dir, with tilde as the home directory

              shorting_target = 40, -- Shortens path to leave 40 spaces in the window
              -- for other components. (terrible name, any suggestions?)
              symbols = {
                modified = '[+]',      -- Text to show when the file is modified.
                readonly = '[-]',      -- Text to show when the file is non-modifiable or readonly.
                unnamed = '[No Name]', -- Text to show for unnamed buffers.
                newfile = '[New]',     -- Text to show for newly created file before first write
              }
            }
          },
          lualine_c = {
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
            {
              'diagnostics',
              sections = { 'error', 'warn' }
            }

            -- 'encoding',
            -- 'fileformat',
            -- 'filetype'
          },
          lualine_y = {},
          lualine_z = {
            -- 'branch',
            -- 'diff',
          }
        },
        inactive_winbar = {
          lualine_a = {
            -- 'filetype'

          },
          lualine_b = {
            {
              'filename',
              file_status = true,    -- Displays file status (readonly status, modified status)
              newfile_status = true, -- Display new file status (new file means no write after created)
              path = 1,              -- 0: Just the filename
              -- 1: Relative path
              -- 2: Absolute path
              -- 3: Absolute path, with tilde as the home directory
              -- 4: Filename and parent dir, with tilde as the home directory

              shorting_target = 40, -- Shortens path to leave 40 spaces in the window
              -- for other components. (terrible name, any suggestions?)
              symbols = {
                modified = '[+]',      -- Text to show when the file is modified.
                readonly = '[-]',      -- Text to show when the file is non-modifiable or readonly.
                unnamed = '[No Name]', -- Text to show for unnamed buffers.
                newfile = '[New]',     -- Text to show for newly created file before first write
              }
            }
          },
          lualine_c = {
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
            {
              'diagnostics',
              sections = { 'error', 'warn' }
            }

            -- 'encoding',
            -- 'fileformat',
            -- 'filetype'
          },
          lualine_y = {},
          lualine_z = {
            -- 'branch',
            -- 'diff',
          }
        },

        extensions = {},
        -- sections = {
        --   lualine_a = {},
        --   lualine_b = {},
        --   lualine_c = {
        --     -- { 'filename', path = 1 }
        --   },
        --   lualine_x = {},
        --   lualine_y = {},
        --   lualine_z = {}
        -- },
        -- sections = {
        --   -- lualine_a = {'mode'},
        --   lualine_a = {
        --   },
        --   lualine_b = {},
        --   lualine_c = {
        --     'branch',
        --     -- 'diff',
        --     {
        --       'diagnostics',
        --       sections = { 'error', 'warn' }
        --     }
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
        --     -- 'encoding',
        --     -- 'fileformat',
        --     'filetype'
        --   },
        --   lualine_y = { 'progress' },
        --   lualine_z = { 'location' }
        -- },
        sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {
          },
          lualine_x = {},
          lualine_y = {},
          lualine_z = {}
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {
          },
          lualine_x = {},
          lualine_y = {},
          lualine_z = {}
        },
      })
    end
  }
}
