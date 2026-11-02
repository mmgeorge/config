local keymaps = require("forge.shared.keymaps")

local M = {}

--- Observe an unmapped interrupt that Neovim delivered while it was busy.
---@param buffer integer
---@param is_current fun(): boolean
---@param cancel fun()
function M.attach(buffer, is_current, cancel)
  local namespace = vim.api.nvim_create_namespace("ForgeComposerInterrupt" .. buffer)
  local interrupt = vim.api.nvim_replace_termcodes("<C-c>", true, false, true)
  local pending = false
  local alive = true
  vim.on_key(function(key, typed)
    if key ~= interrupt or typed ~= interrupt or pending or not alive then return end
    if vim.api.nvim_get_current_buf() ~= buffer or not is_current() then return end
    local mode = vim.api.nvim_get_mode().mode
    if mode ~= "n" and mode ~= "i" then return end
    local configured = false
    for _, binding in ipairs(keymaps.view_keys_for("harness", "cancel")) do
      if vim.api.nvim_replace_termcodes(binding, true, false, true) == interrupt then configured = true break end
    end
    if not configured then return end
    pending = true
    vim.schedule(function()
      pending = false
      if alive and is_current() and vim.api.nvim_buf_is_valid(buffer) then cancel() end
    end)
  end, namespace)
  vim.api.nvim_create_autocmd("BufWipeout", {
    buffer = buffer,
    once = true,
    callback = function()
      alive = false
      vim.on_key(nil, namespace)
    end,
  })
end

return M
