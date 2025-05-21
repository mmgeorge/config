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
            statusline = {},
            winbar = {},
          },
          ignore_focus = {},
          always_divide_middle = true,
          globalstatus = false,
          refresh = {
            statusline = 1000,
            tabline = 1000,
            winbar = 1000,
          }
        },
        sections = {
          -- lualine_a = {'mode'},
          lualine_a = {},
          lualine_c = {
            'branch',
            -- 'diff',
            {
              'diagnostics',
              sections = { 'error', 'warn' }
            }
          },
          lualine_b = {
            {
              'filename',
              file_status = true,     -- Displays file status (readonly status, modified status)
              newfile_status = false, -- Display new file status (new file means no write after created)
              path = 1,               -- 0: Just the filename
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
            },
            -- {
            --   'buffers',
            --   show_filename_only = false,      -- Shows shortened relative path when set to false.
            --   hide_filename_extension = false, -- Hide filename extension when set to true.
            --   show_modified_status = true,     -- Shows indicator when the buffer is modified.
            --
            --   mode = 4,                        -- 0: Shows buffer name
            --   -- 1: Shows buffer index
            --   -- 2: Shows buffer name + buffer index
            --   -- 3: Shows buffer number
            --   -- 4: Shows buffer name + buffer number
            --
            --   max_length = vim.o.columns * 2 / 3, -- Maximum width of buffers component,
            --   -- max_length = vim.o.columns * 2 / 3, -- Maximum width of buffers component,
            --   -- it can also be a function that returns
            --   -- the value of `max_length` dynamically.
            -- },
            --
            -- 'filename'
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
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {
            -- { 'filename', path = 1 }
          },
          lualine_x = { 'location' },
          lualine_y = {},
          lualine_z = {}
        },
        tabline = {},
        winbar = {},
        inactive_winbar = {},
        extensions = {}
      })
    end
  }
}
