local colors = {
  red = "#ef3573",
  -- light_red = "#c3d0e0",
  off_white = "#ccdbed",
  green = "#5bff94",
  yellow = "#ffe18e",
  gray = "#696969",
  light_gray = "#8a929c",
  -- white = "#e9e9e9",
  white = "#ffffff",
  light_blue = "#3ec5ff",
  -- light_gray = "#b4b4b4",
  teal = "#9dffe7",
  dark_gray = "#151515",
  black = "#000000",
  visual = "#303136",
  error = "red"
}

return {
  {
    "slugbyte/lackluster.nvim",
    lazy = false,
    priority = 1000,
    -- setup = function()
    -- vim.cmd.colorscheme("lackluster")
    -- end,
    init = function()
      local lackluster = require("lackluster")
      lackluster.setup({
        tweak_color = {
          lack = colors.white,
          luster = colors.white,
          -- black = "#000000",
          gray1 = colors.gray,
          gray2 = colors.gray,
          gray3 = colors.gray,
          gray4 = colors.gray,

          gray5 = colors.light_gray, -- controls snacks text
          gray6 = colors.white,
          -- gray7 = colors.red,
          -- gray8 = colors.light_gray,
          -- gray9 = colors.red,
        },
        tweak_background = {
          normal = 'none',
          menu = colors.black,  -- nvim_cmp, wildmenu ... (bad idea to transparent)
          popup = colors.black, -- lazy, mason, whichkey ... (bad idea to transparent)
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
          git_signs = true,
          headlines = true,
          render_markdown = true,
          cmp = true,
          bufferline = true
        }
      })

      vim.cmd.colorscheme("lackluster")
      -- vim.cmd.colorscheme("lackluster-hack") -- my favorite
      -- vim.cmd.colorscheme("lackluster-mint")

      -- Semantic
      vim.api.nvim_set_hl(0, "@tag.attribute", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@comment", { fg = colors.light_gray })
      vim.api.nvim_set_hl(0, "@parameter", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@constant", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@variable", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@variable.member", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@variable.parameter", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@property", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@include", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@conditional", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@constructor", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@constructor.lua", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@lsp.type.enumMember", { fg = colors.green })
      vim.api.nvim_set_hl(0, "@lsp.mod.declaration", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@declaration", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@boolean", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@primitive", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@number", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@tag", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@tag.delimiter", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@tag.builtin.tsx", { fg = colors.green })
      vim.api.nvim_set_hl(0, "@tag.tsx", { fg = colors.green })
      vim.api.nvim_set_hl(0, "@type.builtin", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@number.float", { fg = colors.teal })
      vim.api.nvim_set_hl(0, "@builtin", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@variable.builtin", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@module.builtin", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@module", { fg = colors.white })
      vim.api.nvim_set_hl(0, "@keyword", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "@function.method.call", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@function.call", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@function", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@decorator", { fg = colors.red })
      vim.api.nvim_set_hl(0, "@function.macro.rust", { fg = colors.red })

      vim.api.nvim_set_hl(0, "SpellBad", { fg = colors.red, underline = true })

      -- StatusLine
      vim.api.nvim_set_hl(0, "StatusLineError", { fg = colors.error })

      -- Git
      vim.api.nvim_set_hl(0, "GitSignsAdd", { fg = colors.green })
      vim.api.nvim_set_hl(0, "GitSignsDelete", { fg = colors.red })
      vim.api.nvim_set_hl(0, "GitSignsChange", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "GitSignsChangedelete", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "MiniDiffSignAdd", { fg = colors.green })
      vim.api.nvim_set_hl(0, "MiniDiffSignDelete", { fg = colors.red })
      vim.api.nvim_set_hl(0, "MiniDiffSignChange", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "NeogitDiffAdd", { fg = colors.green })
      vim.api.nvim_set_hl(0, "NeogitDiffDelete", { fg = colors.red })
      vim.api.nvim_set_hl(0, "NeogitDiffChange", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "NeogitDiffContext", { fg = colors.white })
      vim.api.nvim_set_hl(0, "NeogitDiffHeader", { fg = colors.black, bg = colors.light_gray })
      vim.api.nvim_set_hl(0, "NeogitHunkHeader", { fg = colors.black, bg = colors.light_gray })
      vim.api.nvim_set_hl(0, "NeogitActiveItem", { bg = colors.visual })
      vim.api.nvim_set_hl(0, "NeogitBranch", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "NeogitBranchHead", { fg = colors.yellow })
      vim.api.nvim_set_hl(0, "NeogitRemote", { fg = colors.light_blue })
      vim.api.nvim_set_hl(0, "NeogitSectionHeader", { fg = colors.white })
      vim.api.nvim_set_hl(0, "NeogitPopupActionKey", { fg = colors.white })
      vim.api.nvim_set_hl(0, "BufferLineGroupSeparator", { bg = colors.black })
      vim.api.nvim_set_hl(0, "StatusLine", { bg = colors.black })
      vim.api.nvim_set_hl(0, "StatusLineTerm", { bg = colors.black })
      vim.api.nvim_set_hl(0, "MsgSeparator", { bg = colors.black })

      -- Backgrounds
      vim.api.nvim_set_hl(0, "BufferLineBackground", { bg = "none" })
      -- vim.api.nvim_set_hl(0, "RenderMarkdownInlineHighlight", { bg = colors.dark_gray })
      -- vim.api.nvim_set_hl(0, "RenderMarkdownCodeInline", { bg = colors.dark_gray })
      -- vim.api.nvim_set_hl(0, "RenderMarkdownCode", { bg = colors.dark_gray })
      -- vim.api.nvim_set_hl(0, "RenderMarkdownCodeBorder", { bg = colors.dark_gray })
      vim.api.nvim_set_hl(0, "RenderMarkdownInlineHighlight", { bg = colors.black })
      vim.api.nvim_set_hl(0, "RenderMarkdownCodeInline", { bg = colors.black })
      vim.api.nvim_set_hl(0, "RenderMarkdownCode", { bg = colors.black })
      vim.api.nvim_set_hl(0, "RenderMarkdownCodeBorder", { bg = colors.black })


      -- Tabline
      vim.api.nvim_set_hl(0, "TabLine", { fg = colors.white })
      vim.api.nvim_set_hl(0, "TabLineSel", { fg = colors.black, bg = colors.white })
      vim.api.nvim_set_hl(0, "TabLineFill", { fg = colors.white })

      -- Selection
      vim.api.nvim_set_hl(0, "SnacksPickerSearch", { bg = colors.visual })
      vim.api.nvim_set_hl(0, "SnacksPickerPickWinCurrent", { bg = colors.visual })
      -- vim.api.nvim_set_hl(0, "SnacksPickerPickWin", { bg = colors.visual })
      -- vim.api.nvim_set_hl(0, "MatchParen", { fg = colors.black, bg = colors.white })
      vim.api.nvim_set_hl(0, "MatchParen", { bg = colors.visual })
      vim.api.nvim_set_hl(0, "Visual", { bg = colors.visual })
      vim.api.nvim_set_hl(0, "Substitute", { bg = colors.visual })

      -- Dropbar
      -- vim.api.nvim_set_hl(0, "DropBarKindDefault", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarMenuHoveSymbol", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarMenuNormalFloat", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarKindFile", { fg = colors.white, bold = false })
      -- vim.api.nvim_set_hl(0, "DropBarKindFileNC", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarKindFolder", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarKindFolderNC", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarKindSection", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarIconKindFolder", { fg = colors.white })
      -- vim.api.nvim_set_hl(0, "DropBarMenuCurrentContext", { fg = colors.white })

      vim.api.nvim_set_hl(0, "DropBarFileName", { fg = colors.white, italic = true, bold = false })
      vim.api.nvim_set_hl(0, "DropBarPath", { fg = colors.white, bold = false })

      -- vim.api.nvim_create_autocmd('ModeChanged', {
      --   callback = function()
      --     local new_mode = vim.v.event.new_mode
      --     if new_mode == "i" then
      --       vim.api.nvim_set_hl(0, "DropBarFileName", { fg = colors.green, italic = true, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarPath", { fg = colors.green, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarIconKindFolder", { fg = colors.green })
      --     elseif new_mode == "n" then
      --       vim.api.nvim_set_hl(0, "DropBarFileName", { fg = colors.white, italic = true, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarPath", { fg = colors.white, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarIconKindFolder", { fg = colors.white })
      --     elseif new_mode == "c" then
      --       vim.api.nvim_set_hl(0, "DropBarFileName", { fg = colors.yellow, italic = true, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarPath", { fg = colors.yellow, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarIconKindFolder", { fg = colors.yellow })
      --     elseif new_mode == "v" then
      --       vim.api.nvim_set_hl(0, "DropBarFileName", { fg = colors.red, italic = true, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarPath", { fg = colors.red, bold = false })
      --       vim.api.nvim_set_hl(0, "DropBarIconKindFolder", { fg = colors.red })
      --     end
      --   end
      -- })
      --
      --
      --
      -- vim.api.nvim_set_hl(0, "DropBarKindFolderNC", { fg = colors.white })


      -- Diagnostics
      vim.api.nvim_set_hl(0, "DiagnosticUnnecessary", { fg = colors.light_gray, underline = true })

      vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", {
        fg = colors.error,
        underline = true,
        -- undercurl = true,
      })

      vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", {
        underline = true
        -- undercurl = true,
      })

      vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", {
        fg = "yellow",
        underline = true
        -- undercurl = true,
      })

      vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", {
        -- fg = "yellow",
        underline = true,
      })

      -- vim.cmd([[highlight DiagnosticUnderlineError cterm=undercurl ctermfg=red gui=undercurl guifg=red]])
      -- vim.cmd([[highlight DiagnosticUnderlineWarn cterm=undercurl ctermfg=yellow gui=undercurl guifg=yellow]])
    end,
  }
}
