-- vim.o.termguicolors = true
vim.o.expandtab = true;
vim.o.cindent = false
vim.o.autoindent = false
vim.o.smartindent = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.timeoutlen = 1500
vim.o.ttimeoutlen = 0
vim.o.guicursor = ""
vim.o.number = true
vim.o.laststatus = 0
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
-- vim.o.fileformat = "unix"
-- vim.opt.shellslash = true

vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.o.foldcolumn = '1' -- '0' is not bad
vim.o.foldlevel = 99   -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true
-- vim.opt.foldmethod = "expr"
-- vim.opt.nofoldenable = true
-- vim.cmd('set nofoldenable')
vim.cmd('set mps +=<:>')           -- Add <> to matchpairs (% command)

vim.g.rust_recommended_style = '0' -- Otherwise will override indentation settings

-- vim.cmd('autocmd BufEnter * set formatoptions+=roj')
-- vim.cmd('autocmd BufRead,BufNewFile * set fileformat=unix')

function is_special()
  return vim.bo.buftype ~= '' or vim.bo.filetype == 'oil'
end

vim.api.nvim_create_autocmd({ 'BufRead', 'BufNewFile' }, {
  pattern = '*',
  callback = function()
    if not is_special() and vim.bo.modifiable then
      -- r = Continue comment o~n enter
      -- o = Continue comment on o
      vim.bo.formatoptions = 'roj'
      vim.bo.fileformat = 'unix'
    end
  end
})

vim.api.nvim_create_autocmd({ 'BufEnter' }, {
  pattern = '*',
  callback = function()
    if not is_special() and vim.bo.modifiable then
      -- r = Continue comment on enter
      -- o = Continue comment on o
      vim.bo.formatoptions = 'roj'

      vim.bo.fileformat = 'unix'
    end
  end
})

-- vim.api.nvim_create_autocmd({'FocusGained', 'CursorHold'}, {
--   pattern = '*',
--   callback = function()
--     if vim.bo.modifiable then
--       vim.cmd('checktime')
--     end
--   end
-- })

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
