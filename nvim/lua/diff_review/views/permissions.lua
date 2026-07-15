local M = {}

local client = require("diff_review.harness.client")
local notifications = require("diff_review.infra.notifications")

---@class DiffReviewPermissionDocument
---@field path string
---@field source string

---@param result DiffReviewPermissionDocument
local function edit(result)
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_name(buf, result.path)
  vim.bo[buf].buftype = "acwrite"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "json"
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, vim.split(result.source or "", "\n", { plain = true }))
  vim.bo[buf].modified = false
  vim.api.nvim_create_autocmd("BufWriteCmd", {
    buffer = buf,
    callback = function()
      local source = table.concat(vim.api.nvim_buf_get_lines(buf, 0, -1, false), "\n") .. "\n"
      client.request("permissions.save", { source = source }, function(_, request_error)
        if request_error then
          notifications.error(request_error, "Permissions")
          return
        end
        vim.bo[buf].modified = false
        notifications.info("Harness permissions saved", "Permissions")
      end)
    end,
  })
  vim.api.nvim_set_current_buf(buf)
end

function M.open()
  client.start(function(_, start_error)
    if start_error then
      notifications.error(start_error, "Permissions")
      return
    end
    client.request("permissions.open", {}, function(result, request_error)
      if request_error then
        notifications.error(request_error, "Permissions")
        return
      end
      edit(result)
    end)
  end)
end

return M
