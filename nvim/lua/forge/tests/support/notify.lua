-- Captures notifications for the rest of the current case: `vim.notify` messages and, optionally,
-- Forge's `forge.infra.notifications` reporters. The capture restores both when the case ends.
-- Pass the capture as `errors` to `wait.lua` so a wait fails as soon as an error is reported.
--
--   local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
--   local errors = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
--   wait_until(function() return state.session end, "Harness did not start", { errors = errors })
--   errors:assert_none()
local stub = dofile("nvim/lua/forge/tests/support/stub.lua")

local notify = {}

---@class NotificationCapture
---@field [integer] { message: string, level: integer?, options: table? }
local Capture = {}
Capture.__index = Capture

---@return string[]
function Capture:messages()
  local messages = {}
  for _, entry in ipairs(self) do messages[#messages + 1] = entry.message end
  return messages
end

function Capture:clear()
  for index = #self, 1, -1 do self[index] = nil end
end

function Capture:assert_none()
  if #self > 0 then error("unexpected notifications:\n" .. table.concat(self:messages(), "\n"), 2) end
end

---@class NotificationCaptureOptions
---@field min_level? integer record only messages at or above this `vim.log.levels` value
---@field notifications? boolean also capture `forge.infra.notifications` `error`, `warn`, and `info`

---@param options? NotificationCaptureOptions
---@return NotificationCapture
function notify.capture(options)
  options = options or {}
  local capture = setmetatable({}, Capture)
  local function record(message, level, notify_options)
    if options.min_level == nil or (level or vim.log.levels.INFO) >= options.min_level then
      capture[#capture + 1] = { message = tostring(message), level = level, options = notify_options }
    end
  end
  stub(vim, "notify", record)
  if options.notifications then
    local notifications = require("forge.infra.notifications")
    for name, level in pairs({ error = vim.log.levels.ERROR, warn = vim.log.levels.WARN, info = vim.log.levels.INFO }) do
      if type(notifications[name]) == "function" then
        stub(notifications, name, function(message, notify_options) record(message, level, notify_options) end)
      end
    end
  end
  return capture
end

return notify
