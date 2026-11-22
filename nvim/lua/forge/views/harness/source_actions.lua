local notifications = require("forge.infra.notifications")

local M = {}
local severity_types = { error = "E", warning = "W", info = "I", hint = "I" }

---@class ForgeHarnessSourceAction
---@field path string Absolute or workspace-relative source file.
---@field line? number One-based line, when known.
---@field column? number One-based UTF-8 byte column, when known.

---@class ForgeHarnessQuickfixItem: ForgeHarnessSourceAction
---@field message string
---@field severity 'error'|'warning'|'info'|'hint'

---@class ForgeHarnessQuickfixAction
---@field title string
---@field items ForgeHarnessQuickfixItem[]

---@param workspace string
---@param path string
---@return string
local function source_path(workspace, path)
  local normalized = vim.fs.normalize(path, { expand_env = false })
  if vim.fs.abspath(normalized) == normalized then return normalized end
  return vim.fs.joinpath(workspace, path)
end

---Open a source location, clamping stale coordinates to the current file.
---@param workspace string
---@param action ForgeHarnessSourceAction
function M.open_file(workspace, action)
  local path = source_path(workspace, action.path)
  if vim.fn.filereadable(path) ~= 1 then
    notifications.warn("Source file is no longer available: " .. path, "ForgeHarness")
    return
  end
  local return_path = vim.api.nvim_buf_get_name(0)
  local opened, failure = pcall(vim.api.nvim_cmd, { cmd = "tabedit", args = { path } }, {})
  if not opened then notifications.error(failure, "ForgeHarness") return end
  if return_path ~= "" then
    vim.keymap.set("n", ",", function()
      local returned, return_failure = pcall(vim.api.nvim_cmd, { cmd = "tabprevious" }, {})
      if not returned then
        notifications.error(return_failure, "ForgeHarness")
        return
      end
      if vim.api.nvim_buf_get_name(0) ~= return_path then
        local reopened, reopen_failure = pcall(vim.api.nvim_cmd, { cmd = "tabedit", args = { return_path } }, {})
        if not reopened then notifications.error(reopen_failure, "ForgeHarness") end
      end
    end, { buffer = 0, desc = "Return to the Forge timeline" })
  end
  local line = math.max(1, math.min(type(action.line) == "number" and action.line or 1, vim.api.nvim_buf_line_count(0)))
  local text = vim.api.nvim_buf_get_lines(0, line - 1, line, false)[1] or ""
  local column = math.max(0, math.min(type(action.column) == "number" and action.column - 1 or 0, #text))
  while column > 0 and column < #text do
    local byte = text:byte(column + 1)
    if byte < 128 or byte >= 192 then break end
    column = column - 1
  end
  vim.api.nvim_win_set_cursor(0, { line, column })
end

---Load the selected result's located diagnostics into a new quickfix list.
---@param workspace string
---@param action ForgeHarnessQuickfixAction
function M.quickfix(workspace, action)
  local items = {}
  for _, item in ipairs(action.items) do
    items[#items + 1] = {
      filename = source_path(workspace, item.path),
      lnum = type(item.line) == "number" and item.line or 0,
      col = type(item.column) == "number" and item.column or 0,
      text = item.message,
      type = severity_types[item.severity],
    }
  end
  if #items == 0 then return end
  local updated, failure = pcall(vim.fn.setqflist, {}, " ", { title = action.title, items = items })
  if not updated or failure ~= 0 then
    notifications.error(updated and "Could not load check diagnostics into quickfix" or failure, "ForgeHarness")
    return
  end
  local opened, open_error = pcall(vim.api.nvim_cmd, { cmd = "copen" }, {})
  if not opened then notifications.error(open_error, "ForgeHarness") end
end

return M
