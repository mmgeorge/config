--- Manages harness permissions document buffer and asynchronous RPC persistence.
---@class ForgePermissionsViewModule
local M = {}

local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local group = vim.api.nvim_create_augroup("ForgePermissions", { clear = true })

---@class ForgePermissionDocument
---@field path string
---@field source string

--- Reuses the permissions editor without replacing unsaved text.
---@param result ForgePermissionDocument Permission document descriptor from harness.
---@param window integer Window that requested the editor.
local function edit(result, window)
  local buffer = vim.fn.bufadd(result.path)
  vim.fn.bufload(buffer)
  vim.bo[buffer].buflisted = true
  vim.bo[buffer].buftype = "acwrite"
  vim.bo[buffer].bufhidden = "hide"
  vim.bo[buffer].swapfile = false
  if vim.bo[buffer].filetype ~= "json" then vim.bo[buffer].filetype = "json" end
  if not vim.bo[buffer].modified then
    vim.api.nvim_buf_set_lines(buffer, 0, -1, false, vim.split(result.source or "", "\n", { plain = true }))
    vim.bo[buffer].modified = false
  end
  vim.api.nvim_clear_autocmds({ group = group, buffer = buffer })
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    group = group,
    buffer = buffer,
    callback = function()
      local source = table.concat(vim.api.nvim_buf_get_lines(buffer, 0, -1, false), "\n") .. "\n"
      local submitted_tick = vim.api.nvim_buf_get_changedtick(buffer)
      client.request("permissions.save", { source = source }, function(_, request_error)
        if request_error then
          notifications.error(request_error, "ForgePermissions")
          return
        end
        if vim.api.nvim_buf_is_valid(buffer) and vim.api.nvim_buf_get_changedtick(buffer) == submitted_tick then
          vim.bo[buffer].modified = false
        end
        notifications.info("Harness permissions saved", "ForgePermissions")
      end)
    end,
  })
  vim.api.nvim_win_set_buf(window, buffer)
end

--- Connects to harness RPC client, requests permissions document, and opens editor buffer.
function M.open()
  local window = vim.api.nvim_get_current_win()
  local origin = vim.api.nvim_win_get_buf(window)
  local function current()
    return vim.api.nvim_win_is_valid(window) and vim.api.nvim_win_get_buf(window) == origin
  end
  client.start_harness(function(_, start_error)
    if start_error then
      notifications.error(start_error, "ForgePermissions")
      return
    end
    if not current() then return end
    client.request("permissions.open", {}, function(result, request_error)
      if request_error then
        notifications.error(request_error, "ForgePermissions")
        return
      end
      if current() then edit(result, window) end
    end)
  end)
end

return M
