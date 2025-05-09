
-- To load, modify .config/nvim/init.lua to point here, e.g, assuming we're at ~/config, then
-- dofile(vim.fn.expand("$HOME/config/nvim/init.lua"))


-- Silence the specific position encoding message
local notify_original = vim.notify
vim.notify = function(msg, ...)
  if
    msg
    and (
      msg:match 'position_encoding param is required'
      or msg:match 'Defaulting to position encoding of the first client'
      or msg:match 'multiple different client offset_encodings'
    )
  then
    return
  end
  return notify_original(msg, ...)
end

vim.g.mapleader=" "
vim.g.maplocalleader=" "

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
  {
    change_detection = {
      notify = false
    }
  }
)

require("vim_options")
require("keymap"); 
require("diagnostics"); 
require("autosnippets"); 
