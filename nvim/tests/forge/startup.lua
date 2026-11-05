vim.loader.enable(false)

local ok, failure = xpcall(function()
  require("forge").setup()
  require("forge.integrations.commit")
  local plugin = require("plugins.forge")[1]
  package.loaded.lazy = { load = function() plugin.config(nil, {}) end }
  plugin.init()
  vim.cmd("ForgeStatus")
  assert(vim.wait(5000, function() return vim.bo.filetype == "ForgeStatus" end, 5),
    "ForgeStatus did not open its native buffer")
end, debug.traceback)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
