local client = require("forge.client")
local M = {}

---@class ForgeTimeoutConfiguration
---@field state ForgeHarnessPresentationState
---@field session_id string
---@field generation integer
---@field pending table<string, table>
---@field completion_list fun(applied: boolean)[]
---@field in_flight boolean
---@field report_failure fun(message: string)

---@param configuration ForgeTimeoutConfiguration
---@return boolean
local function current(configuration)
  local state = configuration.state
  return state.input_timeout_configuration == configuration
    and state.session ~= nil and state.session.id == configuration.session_id
    and client.host_get_generation() == configuration.generation
end

---@param configuration ForgeTimeoutConfiguration
local function flush(configuration)
  if configuration.in_flight or next(configuration.pending) == nil then return end
  local submitted, completion_list = configuration.pending, configuration.completion_list
  configuration.pending, configuration.completion_list = {}, {}
  configuration.in_flight = true
  client.request({ session_id = configuration.session_id, method = "session.input.timeout.configure", params = submitted }, function(result, failure)
    local applied = current(configuration) and failure == nil
    if applied then
      for field in pairs(submitted) do
        if configuration.pending[field] == nil then configuration.state.session[field] = result[field] end
      end
    elseif current(configuration) then
      configuration.report_failure(failure)
    end
    for _, complete in ipairs(completion_list) do complete(applied) end
    configuration.in_flight = false
    if current(configuration) then
      flush(configuration)
    else
      local abandoned = configuration.completion_list
      configuration.pending, configuration.completion_list = {}, {}
      for _, complete in ipairs(abandoned) do complete(false) end
    end
  end)
end

--- Save the newest pending value for each timeout after the current request settles.
---@param state ForgeHarnessPresentationState
---@param values table<string, table>
---@param completed? fun(applied: boolean)
---@param report_failure fun(message: string)
function M.configure(state, values, completed, report_failure)
  local configuration = state.input_timeout_configuration
  if not configuration or not current(configuration) then
    configuration = { state = state, session_id = state.session.id, generation = client.host_get_generation(),
      pending = {}, completion_list = {}, in_flight = false, report_failure = report_failure }
    state.input_timeout_configuration = configuration
  end
  for field, value in pairs(values) do configuration.pending[field] = vim.deepcopy(value) end
  if completed then configuration.completion_list[#configuration.completion_list + 1] = completed end
  flush(configuration)
end

return M
