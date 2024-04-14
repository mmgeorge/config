
-- To load, modify .config/nvim/init.lua to point here, e.g, assuming we're at ~/config, then
-- dofile(vim.fn.expand("$HOME/config/nvim/init.lua"))

vim.g.mapleader=" "

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup(
  "plugins",
  {}
)

require("vim_options")
require("keymap"); 
