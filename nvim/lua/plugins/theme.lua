return {
  'projekt0n/github-nvim-theme',
  lazy = false, -- make sure we load this during startup if it is your main colorscheme
  priority = 1000, -- make sure to load this before all the other start plugins
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
    -- local lualine_theme = require('lualine.themes.github')
    require('github-theme').setup({
      options = {
        -- Compiled file's destination location
        compile_path = vim.fn.stdpath('cache') .. '/github-theme',
        compile_file_suffix = '_compiled', -- Compiled file suffix
        hide_end_of_buffer = true, -- Hide the '~' character at the end of the buffer for a cleaner look
        hide_nc_statusline = true, -- Override the underline style for non-active statuslines
        transparent = false,       -- Disable setting background
        terminal_colors = true,    -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
        dim_inactive = false,      -- Non focused panes set to alternative background
        module_default = true,     -- Default enable value for modules
        styles = {                 -- Style to be applied to different syntax groups
          comments = 'NONE',       -- Value is any valid attr-list value `:help attr-list`
          functions = 'NONE',
          keywords = 'NONE',
          variables = 'NONE',
          conditionals = 'NONE',
          constants = 'NONE',
          numbers = 'NONE',
          operators = 'NONE',
          strings = 'NONE',
          types = 'NONE',
        },
        inverse = {                -- Inverse highlight for different types
          match_paren = false,
          visual = false,
          search = false,
        },
        darken = {                 -- Darken floating windows and sidebar-like windows
          floats = false,
          sidebars = {
            enable = true,
            list = {},             -- Apply dark background to specific windows
          },
        },
        modules = {                -- List of various plugins and additional options
          -- ...
        },
      },
      palettes = {},
      specs = {
        all = {
          syntax = {
            -- bracket     = spec.fg1,                             -- Brackets and Punctuation
            builtin0    = colors.yellow,                   -- Builtin variable
            builtin1    = colors.teal,                    -- Builtin type
            builtin2    = colors.yellow,                   -- Builtin const
            -- comment     = colors.gray,                    -- Comment
            conditional = colors.yellow,                    -- Conditional and loop
            const       = colors.white,                   -- Constants, imports and booleans
            dep         = colors.red,                     -- Deprecated
            field       = colors.white,                   -- Field
            func        = colors.red,                     -- Functions and Titles
            ident       = colors.red,                             -- Identifiers
            keyword     = colors.yellow,                    -- Keywords
            number      = colors.teal,                   -- Numbers
            operator    = colors.white,                   -- Operators
            -- param       = spec.fg1,                             -- Parameters
            preproc     = colors.yellow,                    -- PreProc
            regex       = colors.green,                     -- Regex
            statement   = colors.yellow,                    -- Statements
            string      = colors.light_blue,                     -- Strings
            type        = colors.green,                   -- Types
            tag         = colors.green,                  -- Tags
            variable    = colors.white                              -- Variables
          }
        }
      },
      groups = {},
    })

    vim.cmd('colorscheme github_dark_default')
    vim.cmd([[highlight Visual guibg=#303136]])
    -- vim.cmd([[highlight Visual guibg=#6e7681]])
    vim.cmd([[highlight DiagnosticUnderlineError cterm=undercurl ctermfg=red gui=undercurl guifg=red]])
    vim.cmd([[highlight DiagnosticUnderlineWarn cterm=undercurl ctermfg=yellow gui=undercurl guifg=yellow]])
    vim.api.nvim_set_hl(0, "@include", { fg = colors.yellow })
    vim.api.nvim_set_hl(0, "@conditional", { fg = colors.yellow })
    vim.api.nvim_set_hl(0, "@constructor", { fg = colors.red })
    vim.api.nvim_set_hl(0, "@method", { fg = colors.red })
    vim.api.nvim_set_hl(0, "@lsp.type.enumMember", { fg = colors.green })
    vim.api.nvim_set_hl(0, "@lsp.mod.declaration", { fg = colors.red })
    -- vim.api.nvim_set_hl(0, "@lsp.type.variable", { fg = colors.green })
    -- Dropbar theming
    -- See <https://github.com/Bekaboo/dropbar.nvim?tab=readme-ov-file#highlighting>
    -- vim.api.nvim_set_hl(0, "DropBarMenuNormalFloat", { fg = colors.white })
    -- vim.api.nvim_set_hl(0, "DropBarMenuCurrentContext", { fg = colors.white })
    vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.white })
    vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.white }) 
    vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.white }) 
    
    -- Change breadcrumb colors based on current mode
    -- vim.api.nvim_create_autocmd('ModeChanged', {
    --   callback = function()
    --     local new_mode = vim.v.event.new_mode
    --     if new_mode == "i" then 
    --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.green })
    --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.green }) 
    --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.green }) 
    --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.green }) 
    --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.green }) 
    --     elseif new_mode == "n" then
    --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.white })
    --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.white }) 
    --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.white }) 
    --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.white }) 
    --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.white }) 
    --     elseif new_mode == "c" then
    --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.yellow })
    --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.yellow }) 
    --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.yellow }) 
    --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.yellow }) 
    --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.yellow }) 
    --     elseif new_mode == "v" then
    --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.red })
    --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.red }) 
    --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.red }) 
    --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.red }) 
    --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.red }) 
    --     end
    --   end
    -- })
  end,
}
