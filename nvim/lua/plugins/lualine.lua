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
          component_separators = { left= '', right= ''}, 
          section_separators = { left= '', right= ''}, 
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
          lualine_b = {
            'branch', 
            -- 'diff', 
            { 
              'diagnostics',
              sections = { 'error', 'warn' }
            }
          },
          lualine_c = {
            -- 'filename'
          },
          lualine_x = {
            {
              "overseer",
              label = "", -- Prefix for task counts
              colored = true, -- Color the task icons and counts
              unique = false, -- Unique-ify non-running task count by name
              name = nil, -- List of task names to search for
              name_not = false, -- When true, invert the name search
              status = nil, -- List of task statuses to display
              status_not = false, -- When true, invert the status search
            },
            -- 'encoding', 
            -- 'fileformat', 
            'filetype'
          },
          lualine_y = {'progress'},
          lualine_z = {'location'}
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {'filename'},
          lualine_x = {'location'},
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
