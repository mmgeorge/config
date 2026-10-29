local M = {}
local notifications = require("forge.infra.notifications")
local sequence = 0
local writer = tostring(vim.fn.getpid()) .. ":" .. tostring(vim.uv.hrtime())

local contract = require("forge.event_contract")

---@return string
function M.identity()
  sequence = sequence + 1
  return ("command:%s:%s:%s"):format(vim.fn.getpid(), vim.uv.hrtime(), sequence)
end

---Preserve one intent across transport uncertainty without replaying it in a new host.
---@param transport function
---@param session_id? string
---@param method string
---@param params? table
---@param callback function
function M.dispatch(transport, session_id, method, params, callback)
  if contract.method_policy(method).recovery ~= "session_command" then transport(session_id, method, params, callback) return end
  local client = require("forge.client")
  local state = require("forge.session").harness_by_id[session_id]
  local generation = client.host_get_generation()
  local request = vim.deepcopy(params or {})
  local identity = request._command and request._command.id or request.operation_id
    or (method == "session.input.apply" and request.id) or M.identity()
  request._command = request._command or { id = identity,
    runtime_epoch = state and state.runtime_epoch or nil }
  if method == "session.question.answer" or method == "session.question.skip" then
    request.operation_id = identity
    sequence = sequence + 1
    request.answer_order = request.answer_order or { writer, sequence }
  end
  local finished, recovering = false, false
  local receive
  local function complete(result, failure, detail)
    if finished then return end
    finished = true
    callback(result, failure, detail)
  end
  local function current()
    if client.host_get_generation() == generation then return true end
    complete(nil, "Host restarted before the command outcome was confirmed. The original intent was not replayed.",
      { code = "outcome_unknown", operation_id = identity })
    return false
  end
  local function reconcile()
    if finished or not current() then return end
    transport(session_id, "session.command.status.get", { id = identity }, function(result, failure)
      if finished or not current() then return end
      if not failure and result and result.state == "completed" then
        local response = result.response
        if response.error then complete(nil, response.error.message, response.error)
        else complete(response.result) end
      elseif not failure and result and result.state == "not_admitted" then
        transport(session_id, method, request, receive)
      elseif not failure and result and result.state == "outcome_unknown" then
        complete(nil, "Command stopped without a confirmed outcome. Inspect its effects before retrying.",
          { code = "outcome_unknown", operation_id = identity })
      else vim.defer_fn(reconcile, 500) end
    end)
  end
  receive = function(result, failure, detail)
    if finished or not current() then return end
    if failure and (not detail or detail.code == "outcome_unknown" or detail.code == "result_encoding_failed") then
      if not recovering then
        recovering = true
        notifications.error("Checking command outcome: " .. failure, "Harness command")
      end
      vim.defer_fn(reconcile, 250)
    else complete(result, failure, detail) end
  end
  transport(session_id, method, request, receive, function(sent_generation) generation = sent_generation end)
end

return M
