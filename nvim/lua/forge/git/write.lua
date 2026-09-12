local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")

---@class ForgeGitWriteResult
---@field ok boolean
---@field code integer
---@field stdout string
---@field stderr string
---@field output string
---@field operation_id string|integer|nil

---@param workspace string
---@param action table
---@param callback fun(result: ForgeGitWriteResult)
---@param progress? fun(text: string, stream: string)
---@return fun()
function M.execute(workspace, action, callback, progress)
  local stream = { stdout = { sequence = 0, text = "", pending = "" }, stderr = { sequence = 0, text = "", pending = "" } }
  local cancelled, settled, intent, operation_id, progress_failure = false, false, nil, nil, nil
  local function cancel()
    if settled then return end
    cancelled = true
    if intent or operation_id then
      client.request_host("repository.write", { operation = "cancel", intent = intent, operation_id = operation_id }, function(_, failure)
        if failure then notifications.error("Git cancellation failed: " .. failure) end
      end)
    end
  end
  local function finish(outcome, failure)
    if settled then return end
    settled = true
    local diagnostic, code, uncertain = {}, 0, false
    failure = failure or progress_failure
    if not failure and (type(outcome) ~= "table" or type(outcome.target) ~= "table" or #outcome.target == 0) then
      failure = "Git write returned no verifiable target receipt"
    end
    if failure then diagnostic[#diagnostic + 1] = failure code = 1 uncertain = true end
    if outcome and outcome.success == false then code = 1 end
    for _, target in ipairs(outcome and outcome.target or {}) do
      if target.completion ~= "completed" then code = target.exit_code or 1 end
      if target.completion == "outcome_unknown" then uncertain = true end
      if target.diagnostic then diagnostic[#diagnostic + 1] = target.diagnostic end
    end
    if outcome and outcome.settlement_diagnostic then diagnostic[#diagnostic + 1] = outcome.settlement_diagnostic end
    operation_id = outcome and (outcome.operation_id or outcome.operation) or operation_id
    for name, state in pairs(stream) do
      if progress and state.pending ~= "" then
        local rendered, render_error = pcall(progress, state.pending, name)
        if not rendered then
          diagnostic[#diagnostic + 1] = "Git progress presentation failed: " .. tostring(render_error)
          code = 1
        end
      end
    end
    local output = stream.stdout.text .. stream.stderr.text
    if #diagnostic > 0 then output = output .. (#output > 0 and "\n" or "") .. table.concat(diagnostic, "\n") end
    callback({ ok = code == 0, code = code, stdout = stream.stdout.text, stderr = stream.stderr.text, output = output, operation_id = operation_id })
    if operation_id and not uncertain then
      client.request_host("repository.write", { operation = "acknowledge", operation_id = operation_id }, function(_, acknowledgement_error)
        if acknowledgement_error then notifications.error("Git receipt acknowledgement failed: " .. acknowledgement_error) end
      end)
    end
  end
  client.request_host("repository.write", { operation = "prepare", workspace = workspace, action = action }, function(prepared, failure)
    if failure then finish(nil, failure) return end
    if type(prepared) ~= "table" or type(prepared.intent) ~= "string" or prepared.intent == "" then
      finish(nil, "Git write preparation returned no intent")
      return
    end
    intent = prepared.intent
    if cancelled then cancel() finish(nil, "Git write cancelled before submission") return end
    client.request_host("repository.write", { operation = "submit", intent = intent }, finish, function(chunk)
      if progress_failure then return end
      local received, receive_error = pcall(function()
        local state = stream[chunk.stream]
        if not state or chunk.sequence ~= state.sequence then error("Git progress stream sequence differs from admitted order") end
        state.sequence = state.sequence + 1
        local bytes = vim.base64.decode(chunk.bytes)
        if #state.text + #bytes > 64 * 1024 then error("Git progress exceeds retained stream limit") end
        state.text = state.text .. bytes
        local visible = bytes
        if state.last_carriage and visible:sub(1, 1) == "\n" then visible = visible:sub(2) end
        state.last_carriage = bytes:sub(-1) == "\r"
        state.pending = state.pending .. visible:gsub("\r\n", "\n"):gsub("\r", "\n")
        local last = state.pending:match("^.*()\n")
        if last then
          if progress then progress(state.pending:sub(1, last), chunk.stream) end
          state.pending = state.pending:sub(last + 1)
        end
      end)
      if not received then
        progress_failure = "Git progress failed: " .. tostring(receive_error)
        cancel()
      end
    end)
  end)
  return cancel
end

return M
