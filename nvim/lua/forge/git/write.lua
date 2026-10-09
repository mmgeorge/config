local M = {}
local client = require("forge.client")
local notifications = require("forge.infra.notifications")
local perf = require("forge.infra.perf")

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
---@param trace_id? string Correlates preparation and execution with the initiating action.
---@return fun()
function M.execute(workspace, action, callback, progress, trace_id)
  local started = perf.now()
  trace_id = trace_id or ("git-write:%s:%s"):format(vim.fn.getpid(), started)
  local submitted
  local function record(event, fields)
    perf.event("diff", "git.write." .. event, vim.tbl_extend("force", {
      request_id = trace_id, operation = action.kind, elapsed_ms = perf.elapsed_ms(started),
    }, fields or {}))
  end
  record("prepare.start")
  local stream = { stdout = { sequence = 0, text = "", pending = "" }, stderr = { sequence = 0, text = "", pending = "" } }
  local cancelled, settled, intent, operation_id, progress_failure = false, false, nil, nil, nil
  local function cancel()
    if settled then return end
    cancelled = true
    record("cancel")
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
    record("complete", { operation_id = operation_id, code = code, cancelled = cancelled,
      ms = submitted and perf.elapsed_ms(submitted),
      source_bytes = #stream.stdout.text, result_bytes = #stream.stderr.text })
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
      local acknowledgement_started = perf.now()
      record("acknowledge.start", { operation_id = operation_id })
      client.request_host("repository.write", { operation = "acknowledge", operation_id = operation_id }, function(_, acknowledgement_error)
        record("acknowledge.complete", { operation_id = operation_id, ms = perf.elapsed_ms(acknowledgement_started),
          status = acknowledgement_error and "failed" or "ok" })
        if acknowledgement_error then notifications.error("Git receipt acknowledgement failed: " .. acknowledgement_error) end
      end)
    end
  end
  client.request_host("repository.write", { operation = "prepare", workspace = workspace, action = action }, function(prepared, failure)
    record("prepare.complete", { ms = perf.elapsed_ms(started), status = failure and "failed" or "ok" })
    if failure then finish(nil, failure) return end
    if type(prepared) ~= "table" or type(prepared.intent) ~= "string" or prepared.intent == "" then
      finish(nil, "Git write preparation returned no intent")
      return
    end
    for _, phase in ipairs(vim.fn.sort(vim.tbl_keys(prepared.timing_us or {}))) do
      record("prepare.native." .. phase, { ms = prepared.timing_us[phase] / 1000 })
    end
    intent = prepared.intent
    if cancelled then cancel() finish(nil, "Git write cancelled before submission") return end
    submitted = perf.now()
    record("submit.start")
    client.request_host("repository.write", { operation = "submit", intent = intent }, finish, function(chunk)
      if progress_failure then return end
      local received, receive_error = pcall(function()
        local state = stream[chunk.stream]
        if not state or chunk.sequence ~= state.sequence then error("Git progress stream sequence differs from admitted order") end
        state.sequence = state.sequence + 1
        local bytes = vim.base64.decode(chunk.bytes)
        if state.sequence == 1 then record("progress.first", { source = chunk.stream, bytes = #bytes }) end
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
