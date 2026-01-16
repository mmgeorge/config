vim.g._ts_force_sync_parsing = true

-- Shadafile keeps history accross sessions
-- vim.opt.shadafile = "NONE"

-- "yes" means overwrite the file in place (preserves inode)
-- "auto", the default can have issues with some file watcher
-- implementations
vim.opt.backupcopy = "yes"

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
vim.o.laststatus = 3
vim.opt.statuscolumn = "%l %=%s" -- %= means right align
-- vim.opt.statuscolumn = "%=%l %s" -- %= means right align
vim.opt.signcolumn = 'yes'
vim.o.scrolloff = 0
vim.o.swapfile = false
vim.o.backup = false
vim.o.writebackup = false
vim.o.splitbelow = true
vim.opt.spell = false
vim.opt.spelllang = { "en_us" }
vim.opt.spelloptions = "camel"
vim.opt.ignorecase = true
vim.opt.smartcase = true
-- vim.o.fileformat = "unix"
-- vim.opt.shellslash = true
-- vim.opt.fillchars = {
--   stl = "-",
--   stlnc = "-",
-- }
vim.opt.numberwidth = 1

-- This sets it for everything -- ideally we just want rounded for diagnostics
-- vim.o.winborder = 'rounded'

vim.opt.foldcolumn = '1' -- '0' is not bad
vim.opt.foldlevel = 99   -- Using ufo provider need a large value, feel free to decrease the value
vim.opt.foldlevelstart = 99
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldenable = true

-- vim.o.foldmethod = "expr"
-- vim.o.foldexpr = "nvim_treesitter#foldexpr()"


-- vim.o.foldtext = 'v:lua.vim.treesitter.foldtext()'
-- vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"

-- vim.opt.foldmethod = "expr"
-- vim.opt.nofoldenable = true
-- vim.cmd('set nofoldenable')
-- vim.opt.showmatch = false
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

vim.api.nvim_create_autocmd({ 'FocusGained', 'CursorHold' }, {
  pattern = '*',
  callback = function()
    if vim.bo.modifiable then
      vim.cmd('checktime')
    end
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

vim.api.nvim_create_autocmd({ 'VimLeavePre' }, {
    group = vim.api.nvim_create_augroup('KillShada', { clear = true }),
    pattern = { '*' },
    callback = function()
        local status = 0
        for _, f in ipairs(vim.fn.globpath(vim.fn.stdpath('data') .. '/shada', '*tmp*', false, true)) do
            if vim.tbl_isempty(vim.fn.readfile(f)) then
                status = status + vim.fn.delete(f)
            end
        end
        if status ~= 0 then
            vim.notify('Could not delete empty temporary ShaDa files.', vim.log.levels.ERROR)
            vim.fn.getchar()
        end
    end,
    desc = "Delete empty temp ShaDa files"
})
