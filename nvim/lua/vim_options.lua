-- vim.o.termguicolors = true
vim.o.expandtab = true;
vim.o.smartindent = true
vim.o.tabstop = 2
vim.o.shiftwidth=2
vim.o.timeoutlen = 1500
vim.o.ttimeoutlen = 100
vim.o.guicursor= ""
vim.o.number=true
vim.o.laststatus=3
vim.opt.statuscolumn = "%l %=%s" -- %= means right align
vim.opt.signcolumn = 'yes'
vim.o.scrolloff = 0

vim.g.rust_recommended_style = '0' -- Otherwise will override indentation settings

-- Disable auto comment
vim.cmd('autocmd BufEnter * set formatoptions-=cro')
vim.cmd('autocmd BufEnter * setlocal formatoptions-=cro')
