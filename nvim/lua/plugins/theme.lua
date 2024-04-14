return {
   'projekt0n/github-nvim-theme',
   lazy = false, -- make sure we load this during startup if it is your main colorscheme
   priority = 1000, -- make sure to load this before all the other start plugins
   config = function()
      require('github-theme').setup({
            -- ...
                                   })

      vim.cmd([[highlight LspDiagnosticsError cterm=underline ctermfg=blue]])

      vim.cmd([[highlight LspDiagnosticsUnderlineError cterm=underline ctermfg=red gui=underline guifg=red]])
vim.cmd([[highlight LspDiagnosticsUnderlineErrorSign cterm=bold ctermfg=red gui=bold guifg=red]])
      -- vim.cmd('colorscheme github_dark_default')
      -- vim.cmd([[highlight LspDiagnosticsUnderlineError cterm=underline ctermfg=red gui=underline guifg=red]])
   end,
}
