-- Waits for a condition to become true while the event loop runs, and fails with `message` when it
-- does not. The timeout bounds a hung case; the wait returns as soon as the condition holds,
-- polling every 5 ms instead of `vim.wait`'s 200 ms default.
--
--   local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
--   wait_until(function() return state.ready end, "Harness state never became ready")
--   wait_until(function() return state.session end, "Harness did not start", { timeout_ms = 15000,
--     errors = errors, context = function() return vim.inspect(state.activity) end })
--
-- `errors` is a list that asynchronous code appends failures to, such as a notification capture;
-- the wait fails as soon as it is non-empty instead of running out its timeout. `context` adds
-- diagnostic text to the failure. A wait that checks that something does not happen within a
-- period measures that period on purpose; it calls `vim.wait` directly instead.
local default_timeout_ms = 1000
local poll_interval_ms = 5

---@class WaitOptions
---@field timeout_ms? integer
---@field errors? string[]|{ message: string }[]
---@field context? fun(): string

local function error_text(errors)
  local lines = {}
  for _, entry in ipairs(errors) do lines[#lines + 1] = type(entry) == "table" and tostring(entry.message) or tostring(entry) end
  return table.concat(lines, "\n")
end

---@param condition fun(): any
---@param message string the behavior that failed to happen
---@param options? integer|WaitOptions a timeout in milliseconds, or options
return function(condition, message, options)
  assert(type(message) == "string" and message ~= "", "wait_until requires a failure message")
  if type(options) == "number" then options = { timeout_ms = options } end
  options = options or {}
  local limit = options.timeout_ms or default_timeout_ms
  local errors = options.errors
  local satisfied = vim.wait(limit, function()
    return (errors ~= nil and #errors > 0) or condition()
  end, poll_interval_ms)
  local failure
  if errors ~= nil and #errors > 0 then
    failure = ("%s: errors were reported\n%s"):format(message, error_text(errors))
  elseif not satisfied then
    failure = ("%s (waited %d ms)"):format(message, limit)
  else
    return
  end
  if options.context then failure = failure .. "\n" .. tostring(options.context()) end
  error(failure, 2)
end
