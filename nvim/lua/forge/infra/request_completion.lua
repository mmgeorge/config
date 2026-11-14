local M = {}

---@class ForgeRequestCompletionOptions
---@field timeout_ms integer
---@field timeout_message string
---@field complete fun(result?: any, failure?: string, detail?: table)
---@field failed fun(failure: string)
---@field finally? fun()

---Settle once across response, timeout, and late response; always release caller ownership.
---@param options ForgeRequestCompletionOptions
---@return fun(result?: any, failure?: string, detail?: table)
function M.new(options)
  local completed = false
  local timer
  local function report(failure)
    local reported, report_error = pcall(options.failed, tostring(failure))
    if not reported then
      vim.api.nvim_echo({ { tostring(failure) .. "\n" .. tostring(report_error), "ErrorMsg" } }, true, { err = true })
    end
  end
  local function receive(result, failure, detail)
    if completed then return end
    completed = true
    local deadline = timer
    timer = nil
    local succeeded, completion_error = xpcall(function()
      if deadline and not deadline:is_closing() then deadline:stop() deadline:close() end
      options.complete(result, failure, detail)
    end, debug.traceback)
    -- Failure reporting is consumer code too: it must not prevent release of the request slot.
    local reported, report_error = true, nil
    if not succeeded then reported, report_error = pcall(report, completion_error) end
    local released, release_error = true, nil
    if options.finally then released, release_error = xpcall(options.finally, debug.traceback) end
    if not reported then error(report_error, 0) end
    if not released then report(release_error) end
  end
  timer = vim.defer_fn(function()
    -- defer_fn closes its own handle before invoking this callback.
    timer = nil
    receive(nil, options.timeout_message, { code = "outcome_unknown" })
  end, options.timeout_ms)
  return receive
end

return M
