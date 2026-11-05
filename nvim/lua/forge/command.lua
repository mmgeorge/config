local M = {}
local sequence = 0
local writer = tostring(vim.fn.getpid()) .. ":" .. tostring(vim.uv.hrtime())
local contract = require("forge.event_contract")

---Create one immutable caller identity for response correlation and safe recovery.
---@return string
function M.identity()
  sequence = sequence + 1
  return ("command:%s:%s:%s"):format(vim.fn.getpid(), vim.uv.hrtime(), sequence)
end

---Await one command result; recover lost responses from completion events without polling.
---@param transport function
---@param session_id? string
---@param method string
---@param params? table
---@param callback function
---@param owned_command? boolean
function M.dispatch(transport, session_id, method, params, callback, owned_command)
  if not owned_command and contract.method_policy(method).recovery ~= "session_command" then
    transport(session_id, method, params, callback)
    return
  end
  local client = require("forge.client")
  local state = require("forge.session").harness_by_id[session_id]
  local generation = client.host_get_generation()
  local request = vim.deepcopy(params or {})
  local identity = request._command and request._command.id or request.operation_id
    or (type(request.command) == "table" and request.command.operation_id)
    or (method == "session.input.apply" and request.id) or M.identity()
  request._command = request._command or { id = identity, runtime_epoch = state and state.runtime_epoch or nil }
  if method == "session.question.answer" or method == "session.question.skip" then
    request.operation_id = identity
    sequence = sequence + 1
    request.answer_order = request.answer_order or { writer, sequence }
  end
  local phase, notice = "request", false
  local review_refreshed = false
  local unsubscribe, receive, reconcile
  local function complete(result, failure, detail)
    if phase == "complete" then return end
    phase = "complete"
    if unsubscribe then unsubscribe() unsubscribe = nil end
    callback(result, failure, detail)
  end
  local function current()
    if client.host_get_generation() == generation then return true end
    complete(nil, "Host restarted before the command result was confirmed. The original command was not replayed.",
      { code = "outcome_unknown", operation_id = identity })
    return false
  end
  reconcile = function()
    if phase == "complete" or phase == "reconciling" or not current() then return end
    phase, notice = "reconciling", false
    transport(session_id, "session.command.status.get", { id = identity }, function(result, failure)
      if phase == "complete" or not current() then return end
      if failure or not result then
        complete(nil, failure or "Command result could not be read", { code = "outcome_unknown", operation_id = identity })
      elseif result.state == "completed" then
        local response = result.response
        if response.error then complete(nil, response.error.message, response.error)
        elseif response.result and response.result.review_reload_required and request.review_document and not review_refreshed then
          review_refreshed = true
          phase = "refresh"
          transport(session_id, method, request, receive)
        else complete(response.result) end
      elseif result.state == "not_admitted" then
        phase = "request"
        transport(session_id, method, request, receive)
      elseif result.state == "outcome_unknown" then
        complete(nil, "Command stopped without a confirmed result. Inspect it before retrying.",
          { code = "outcome_unknown", operation_id = identity })
      else
        phase = "waiting"
        if notice then reconcile() end
      end
    end)
  end
  unsubscribe = client.session_events_subscribe(function(event, payload, owner)
    if phase == "complete" or owner and owner ~= session_id then return end
    if event == "host_stopped" then
      complete(nil, "Host stopped before the command result was confirmed", { code = "outcome_unknown", operation_id = identity })
    elseif event == "command_completed" and payload.id == identity then
      notice = true
      if phase == "waiting" then reconcile() end
    end
  end)
  receive = function(result, failure, detail)
    if phase == "complete" or not current() then return end
    if failure and (not detail or detail.code == "outcome_unknown" or detail.code == "command_observation_lost" or detail.code == "result_encoding_failed") then
      reconcile()
    else
      complete(result, failure, detail)
    end
  end
  transport(session_id, method, request, receive, function(sent_generation) generation = sent_generation end)
end

return M
