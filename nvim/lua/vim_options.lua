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
vim.o.splitbelow = true
vim.opt.spell = true
vim.opt.spelllang = { "en_us" }
vim.opt.spelloptions = "camel"
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.o.fileformat = "unix"
-- vim.opt.shellslash = true

vim.o.foldcolumn = '1' -- '0' is not bad
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true
-- vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
-- vim.opt.foldtext = "v:lua.vim.treesitter.foldtext()"
-- vim.opt.foldmethod = "expr"
-- vim.opt.nofoldenable = true
-- vim.cmd('set nofoldenable')


vim.g.rust_recommended_style = '0' -- Otherwise will override indentation settings

-- vim.cmd('autocmd BufEnter * set formatoptions+=roj')
-- vim.cmd('autocmd BufRead,BufNewFile * set fileformat=unix')

vim.api.nvim_create_autocmd({'BufRead','BufNewFile'}, {
  pattern = '*',  
  callback = function()
    if vim.bo.modifiable then
      -- r = Continue comment on enter
      -- o = Continue comment on o
      vim.bo.formatoptions = 'roj'
      vim.bo.fileformat = 'unix'
    end
  end
})

vim.api.nvim_create_autocmd({'BufEnter'}, {
  pattern = '*',  
  callback = function()
    if vim.bo.modifiable then
      -- r = Continue comment on enter
      -- o = Continue comment on o
      vim.bo.formatoptions = 'roj'
      
      vim.bo.fileformat = 'unix'
    end
  end
})

vim.api.nvim_create_autocmd({'FocusGained', 'CursorHold'}, {
  pattern = '*',  
  callback = function()
    vim.cmd('checktime')
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "slang",
  callback = function()
    vim.bo.commentstring = "// %s"
  end
})

-- Disable auto comment
-- vim.cmd('autocmd BufEnter * set formatoptions-=cro')
-- vim.cmd('autocmd BufEnter * setlocal formatoptions-=cro')
-- vim.cmd('autocmd FocusGained * checktime')
-- vim.cmd('autocmd CursorHold * checktime')

-- Slang support
vim.filetype.add({
  extension = {
    slang = "slang",
  },
})

