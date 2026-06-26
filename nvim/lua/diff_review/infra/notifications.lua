---@class DiffReviewNotificationsModule
---@field error fun(message: string, title?: string)
---@field format_failures fun(failures: DiffReviewGitFailure[]): string
---@field git_failures fun(title: string, failures: DiffReviewGitFailure[])
---@field debug fun(message: string, level?: integer, opts?: table)
local M = {}

local config = require("diff_review.infra.config")

---@param message string
---@param title? string
function M.error(message, title)
  vim.notify(message, vim.log.levels.ERROR, { title = title or "Diff Review" })
end

---@param failures DiffReviewGitFailure[]
---@return string
function M.format_failures(failures)
  local max_lines = 8
  local lines = {}
  for index, failure in ipairs(failures) do
    if index > max_lines then break end
    local label = failure.file or failure.path or ("#" .. index)
    local detail = failure.message or failure.output or failure.stderr or failure.stdout or ""
    if detail == "" and failure.code then
      detail = "exit code " .. tostring(failure.code)
    end
    detail = tostring(detail):gsub("\r", ""):gsub("\n+", " ")
    lines[#lines + 1] = ("  %s: %s"):format(label, detail)
  end
  if #failures > max_lines then
    lines[#lines + 1] = ("  ... %d more"):format(#failures - max_lines)
  end
  return table.concat(lines, "\n")
end

---@param title string
---@param failures DiffReviewGitFailure[]
function M.git_failures(title, failures)
  if #failures == 0 then return end
  local count = #failures
  local noun = count == 1 and "failure" or "failures"
  M.error(("%s: %d %s\n%s"):format(title, count, noun, M.format_failures(failures)))
end

---@param message string
---@param level? integer
---@param opts? table
function M.debug(message, level, opts)
  local options = config.options or config.defaults
  if not (options and options.debug_notifications == true) then return end
  vim.notify(message, level or vim.log.levels.INFO, opts)
end

return M
