local colors = {
  red = "#ef3573",
  -- light_red = "#c3d0e0",
  off_white = "#ccdbed",
  green = "#5bff94",
  yellow = "#ffe18e",
  gray = "#696969",
  light_gray = "#8a929c",
  white = "#e9e9e9",
  light_blue = "#3ec5ff",
  -- light_gray = "#b4b4b4",
  teal = "#9dffe7",
  black = "#000000"
}

return {
  {
    "slugbyte/lackluster.nvim",
    lazy = false,
    priority = 1000,
    setup = function()
      -- vim.cmd.colorscheme("lackluster")
    end,
    init = function()
      local lackluster = require("lackluster")
      lackluster.setup({
        tweak_color = {
          lack = colors.white,
          luster = colors.white,
          -- black = "#000000",
          -- gray1 = "#000000",
          -- gray2 = colors.white,
          -- gray3 = colors.white,
          -- gray4 = colors.white,
          gray5 = colors.light_gray,
          gray6 = colors.white,
          -- gray7 = colors.red,
          -- gray8 = colors.light_gray,
          -- gray9 = colors.red,
        },
        tweak_background = {
          normal = 'none',
          menu = 'none',  -- nvim_cmp, wildmenu ... (bad idea to transparent)
          popup = 'none', -- lazy, mason, whichkey ... (bad idea to transparent)
        },
        tweak_syntax = {
          string = colors.light_blue,
          string_escape = colors.light_blue,
          comment = colors.light_gray,
          builtin = colors.yellow, -- builtin modules and functions
          type = colors.green,
          keyword = colors.yellow,
          keyword_return = colors.yellow,
          keyword_exception = colors.yellow,
        },
        disable_plugin = {
          todo_comments = true,
          trouble = true,
          git_gutter = true,
          git_signs = true
        }
      })

      vim.cmd.colorscheme("lackluster")
      -- vim.cmd.colorscheme("lackluster-hack") -- my favorite
      -- vim.cmd.colorscheme("lackluster-mint")
      vim.api.nvim_set_hl(0, "@tag.tsx", { fg = colors.green })
      vim.api.nvim_set_hl(0, "@tag.attribute", { fg = colors.light_gray })
      vim.api.nvim_set_hl(0, "@comment", { fg = colors.light_gray })
      vim.api.nvim_set_hl(0, "@parameter", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@constant", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@variable", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@variable.member", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@variable.parameter", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@property", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@include", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@conditional", { fg = colors.yellow })
      -- vim.api.nvim_set_hl(0, "@constructor", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@lsp.type.enumMember", { fg = colors.green })
      vim.api.nvim_set_hl(0, "@lsp.mod.declaration", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@declaration", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@boolean", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@primitive", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@number", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@builtin", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@module.builtin", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@module", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@keyword", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@function.method.call", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@function.call", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@function", { fg = colors.red })
      -- vim.api.nvim_set_hl(0, "@constructor", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@decorator", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@function.macro.rust", { fg = colors.red })
      vim.api.nvim_set_hl(0, "Visual", { bg = "#303136" })
      vim.api.nvim_set_hl(0, "GitSignsAdd", { fg = colors.green })
      vim.api.nvim_set_hl(0, "GitSignsDelete", { fg = colors.red })
      vim.api.nvim_set_hl(0, "GitSignsChange", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "GitSignsChangedelete", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "NeogitDiffAdd", { fg = colors.green })
      vim.api.nvim_set_hl(0, "NeogitDiffDelete", { fg = colors.red })
      vim.api.nvim_set_hl(0, "NeogitDiffChange", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "NeogitDiffContext", { fg = colors.white })
      vim.api.nvim_set_hl(0, "NeogitDiffHeader", { fg = colors.black, bg = colors.light_gray })
      vim.api.nvim_set_hl(0, "NeogitHunkHeader", { fg = colors.black, bg = colors.light_gray })
      vim.api.nvim_set_hl(0, "NeogitActiveItem", { bg = "#303136" })
      vim.api.nvim_set_hl(0, "TabLine", { fg = colors.white })
      vim.api.nvim_set_hl(0, "TabLineSel", { fg = colors.black, bg = colors.white })
      vim.api.nvim_set_hl(0, "TabLineFill", { fg = colors.white })

      vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", {
        fg = "red",
        -- undercurl = true,
        underline = true,
      })
      vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", {
        fg = "yellow",
        -- undercurl = true,
        underline = true
      })

      -- vim.cmd([[highlight DiagnosticUnderlineError cterm=undercurl ctermfg=red gui=undercurl guifg=red]])
      -- vim.cmd([[highlight DiagnosticUnderlineWarn cterm=undercurl ctermfg=yellow gui=undercurl guifg=yellow]])
    end,
  }
  -- {
  --   'projekt0n/github-nvim-theme',
  --   lazy = false,    -- make sure we load this during startup if it is your main colorscheme
  --   priority = 1000, -- make sure to load this before all the other start plugins
  --   config = function()
  --     local colors = {
  --       red = "#ef3573",
  --       -- light_red = "#c3d0e0",
  --       off_white = "#ccdbed",
  --       green = "#5bff94",
  --       yellow = "#ffe18e",
  --       gray = "#696969",
  --       white = "#e9e9e9",
  --       light_blue = "#3ec5ff",
  --       light_gray = "#b4b4b4",
  --       teal = "#9dffe7"
  --     }
  --     -- local lualine_theme = require('lualine.themes.github')
  --     require('github-theme').setup({
  --       options = {
  --         -- Compiled file's destination location
  --         compile_path = vim.fn.stdpath('cache') .. '/github-theme',
  --         compile_file_suffix = '_compiled', -- Compiled file suffix
  --         hide_end_of_buffer = true,         -- Hide the '~' character at the end of the buffer for a cleaner look
  --         hide_nc_statusline = true,         -- Override the underline style for non-active statuslines
  --         transparent = true,                -- Disable setting background
  --         terminal_colors = true,            -- Set terminal colors (vim.g.terminal_color_*) used in `:terminal`
  --         styles = {                         -- Style to be applied to different syntax groups
  --           comments = 'NONE',               -- Value is any valid attr-list value `:help attr-list`
  --           functions = 'NONE',
  --           keywords = 'NONE',
  --           variables = 'NONE',
  --           conditionals = 'NONE',
  --           constants = 'NONE',
  --           numbers = 'NONE',
  --           operators = 'NONE',
  --           strings = 'NONE',
  --           types = 'NONE',
  --         },
  --         inverse = { -- Inverse highlight for different types
  --           match_paren = false,
  --           visual = false,
  --           search = false,
  --         },
  --         darken = { -- Darken floating windows and sidebar-like windows
  --           floats = false,
  --           sidebars = {
  --             enable = true,
  --             list = {}, -- Apply dark background to specific windows
  --           },
  --         },
  --         modules = { -- List of various plugins and additional options
  --           -- ...
  --         },
  --       },
  --       specs = {
  --         all = {
  --           syntax = {
  --             -- bracket     = spec.fg1,                             -- Brackets and Punctuation
  --             builtin0    = colors.yellow,     -- Builtin variable
  --             builtin1    = colors.teal,       -- Builtin type
  --             builtin2    = colors.teal,       -- Builtin const
  --             -- comment     = colors.gray,                    -- Comment
  --             conditional = colors.yellow,     -- Conditional and loop
  --             const       = colors.white,      -- Constants, imports and booleans
  --             dep         = colors.red,        -- Deprecated
  --             field       = colors.white,      -- Field
  --             -- func        = colors.red,                     -- Functions and Titles
  --             func        = colors.white,      -- Functions and Titles
  --             ident       = colors.red,        -- Identifiers
  --             keyword     = colors.yellow,     -- Keywords
  --             number      = colors.teal,       -- Numbers
  --             operator    = colors.white,      -- Operators
  --             -- param       = spec.fg1,                             -- Parameters
  --             preproc     = colors.yellow,     -- PreProc
  --             regex       = colors.green,      -- Regex
  --             statement   = colors.yellow,     -- Statements
  --             string      = colors.light_blue, -- Strings
  --             type        = colors.green,      -- Types
  --             tag         = colors.green,      -- Tags
  --             variable    = colors.white       -- Variables
  --           }
  --         }
  --       },
  --       groups = {},
  --     })
  --
  --     vim.cmd('colorscheme github_dark_high_contrast')
  --     -- vim.cmd([[highlight Normal guibg=#000000]])
  --     vim.cmd([[highlight Visual guibg=#303136]])
  --     -- vim.cmd([[highlight Visual guibg=#6e7681]])
  --     vim.cmd([[highlight DiagnosticUnderlineError cterm=undercurl ctermfg=red gui=undercurl guifg=red]])
  --     vim.cmd([[highlight DiagnosticUnderlineWarn cterm=undercurl ctermfg=yellow gui=undercurl guifg=yellow]])
  --     vim.api.nvim_set_hl(0, "@include", { fg = colors.yellow })
  --     vim.api.nvim_set_hl(0, "@conditional", { fg = colors.yellow })
  --     vim.api.nvim_set_hl(0, "@constructor", { fg = colors.red })
  --     vim.api.nvim_set_hl(0, "@lsp.type.enumMember", { fg = colors.green })
  --     vim.api.nvim_set_hl(0, "@lsp.mod.declaration", { fg = colors.red })
  --     vim.api.nvim_set_hl(0, "@declaration", { fg = colors.red })
  --     vim.api.nvim_set_hl(0, "@primitive", { fg = colors.teal })
  --     vim.api.nvim_set_hl(0, "@builtin", { fg = colors.yellow })
  --     vim.api.nvim_set_hl(0, "@function.method.call", { fg = colors.red })
  --     vim.api.nvim_set_hl(0, "@function.call", { fg = colors.red })
  --     vim.api.nvim_set_hl(0, "@decorator", { fg = colors.red })
  --     vim.api.nvim_set_hl(0, "@function.macro.rust", { fg = colors.red })
  --     -- vim.api.nvim_set_hl(0, "@function.macro.rust", { fg = colors.red })
  --     -- vim.api.nvim_set_hl(0, "@property.lua", { fg = colors.off_white })
  --     -- vim.api.nvim_set_hl(0, "@property", { fg = colors.off_white })
  --     -- vim.api.nvim_set_hl(0, "@variable.member", { fg = colors.off_white })
  --
  --     -- vim.api.nvim_set_hl(0, "@method", { fg = colors.red })
  --
  --     -- vim.api.nvim_set_hl(0, "@lsp.type.variable", { fg = colors.green })
  --     -- Dropbar theming
  --     -- See <https://github.com/Bekaboo/dropbar.nvim?tab=readme-ov-file#highlighting>
  --     -- vim.api.nvim_set_hl(0, "DropBarMenuNormalFloat", { fg = colors.white })
  --     -- vim.api.nvim_set_hl(0, "DropBarMenuCurrentContext", { fg = colors.white })
  --     vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.white })
  --     vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.white })
  --     vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.white })
  --
  --     -- Change breadcrumb colors based on current mode
  --     -- vim.api.nvim_create_autocmd('ModeChanged', {
  --     --   callback = function()
  --     --     local new_mode = vim.v.event.new_mode
  --     --     if new_mode == "i" then
  --     --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.green })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.green })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.green })
  --     --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.green })
  --     --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.green })
  --     --     elseif new_mode == "n" then
  --     --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.white })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.white })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.white })
  --     --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.white })
  --     --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.white })
  --     --     elseif new_mode == "c" then
  --     --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.yellow })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.yellow })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.yellow })
  --     --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.yellow })
  --     --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.yellow })
  --     --     elseif new_mode == "v" then
  --     --       vim.api.nvim_set_hl(0, "dropbariconkindfolder", { fg = colors.red })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfile", { fg = colors.red })
  --     --       vim.api.nvim_set_hl(0, "dropbarkindfolder", { fg = colors.red })
  --     --       vim.api.nvim_set_hl(0, "dropbariconuiseparator", { fg = colors.red })
  --     --       vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = colors.red })
  --     --     end
  --     --   end
  --     -- })
  --   end,
  -- }
}
