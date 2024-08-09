-- vim.o.termguicolors = true
vim.o.expandtab = true;
vim.o.cindent = false
vim.o.autoindent = false
vim.o.smartindent = false
vim.o.tabstop = 2
vim.o.shiftwidth=2
vim.o.timeoutlen = 1500
vim.o.ttimeoutlen = 0
vim.o.guicursor= ""
vim.o.number=true
vim.o.laststatus=3
vim.opt.statuscolumn = "%l %=%s" -- %= means right align
vim.opt.signcolumn = 'yes'
vim.o.scrolloff = 0
vim.o.swapfile = false

vim.g.rust_recommended_style = '0' -- Otherwise will override indentation settings

-- Disable auto comment
vim.cmd('autocmd BufEnter * set formatoptions-=cro')
vim.cmd('autocmd BufEnter * setlocal formatoptions-=cro')
vim.cmd('autocmd FocusGained * checktime')
vim.cmd('autocmd CursorHold * checktime')



