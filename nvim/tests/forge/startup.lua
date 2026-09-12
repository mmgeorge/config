vim.loader.enable(false)

local ok, failure = xpcall(function()
  require("forge").setup()
  require("forge.integrations.commit")
  local plugin = require("plugins.forge")
  if vim.fn.exists(":ForgeStatus") == 0 then plugin.config() end
  vim.cmd("ForgeStatus")
  assert(vim.bo.filetype == "ForgeStatus", "ForgeStatus did not open its native buffer")
end, debug.traceback)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
