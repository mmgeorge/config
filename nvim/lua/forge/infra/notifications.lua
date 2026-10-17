---@class ForgeNotificationsModule
---@field error fun(message: string, title?: string)
---@field warn fun(message: string, title?: string)
---@field info fun(message: string, title?: string)
---@field format_failures fun(failures: ForgeGitFailure[]): string
---@field git_failures fun(title: string, failures: ForgeGitFailure[])
---@field debug fun(message: string, level?: integer, opts?: table)
---@field progress fun(id: string, title: string, message: fun(elapsed_seconds: integer): string): ForgeProgressNotice
local M = {}

local config = require("forge.infra.config")

---@class ForgeProgressNotice
---@field finish fun(message?: string, level?: integer) Stop animation and optionally replace the notice. Repeated calls have no effect.

--- Keeps one notification visible until its owner finishes the operation.
---@param id string Notification identity, unique among active operations.
---@param title string
---@param message fun(elapsed_seconds: integer): string Current status, evaluated on each animation frame.
---@return ForgeProgressNotice notice The caller must finish this notice on every terminal path.
function M.progress(id, title, message)
  local started = vim.uv.hrtime()
  local active = true
  local timer = assert(vim.uv.new_timer(), "Cannot create progress notification timer")
  local frame_list = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
  local function render()
    if not active then return end
    local elapsed = math.floor((vim.uv.hrtime() - started) / 1e9)
    vim.notify(message(elapsed), vim.log.levels.INFO, {
      id = id, title = title, timeout = false,
      icon = frame_list[math.floor((vim.uv.hrtime() - started) / 120e6) % #frame_list + 1],
    })
  end
  local notice = {}
  function notice.finish(text, level)
    if not active then return end
    active = false
    timer:stop()
    timer:close()
    if text then
      vim.notify(text, level or vim.log.levels.INFO, {
        id = id, title = title, timeout = level == vim.log.levels.ERROR and 8000 or 3000,
        icon = level == vim.log.levels.ERROR and "✗" or "✓",
      })
    end
  end
  render()
  timer:start(120, 120, vim.schedule_wrap(render))
  return notice
end

---@param message string
---@param title? string
function M.error(message, title)
  vim.notify(message, vim.log.levels.ERROR, { title = title or "Diff Review" })
end

---@param message string
---@param title? string
function M.warn(message, title)
  vim.notify(message, vim.log.levels.WARN, { title = title or "Diff Review" })
end

---@param message string
---@param title? string
function M.info(message, title)
  vim.notify(message, vim.log.levels.INFO, { title = title or "Diff Review" })
end

---@param failures ForgeGitFailure[]
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
---@param failures ForgeGitFailure[]
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
