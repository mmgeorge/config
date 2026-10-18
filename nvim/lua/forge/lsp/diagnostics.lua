local M = {}
local method = "textDocument/diagnostic"
local states = {}
local request_epochs = {}

local function key(buffer, identifier)
  return tostring(buffer) .. ":" .. (identifier or "")
end

local function invalidate(client_id)
  local state = states[client_id]
  if state then state.epoch = state.epoch + 1 end
end

local group = vim.api.nvim_create_augroup("ForgeLspDiagnostics", { clear = true })
vim.api.nvim_create_autocmd("LspNotify", { group = group, callback = function(event)
  local name = event.data.method
  if name == "textDocument/didChange" or name == "textDocument/didSave"
    or name == "workspace/didChangeWatchedFiles" or name == "workspace/didChangeConfiguration" then
    invalidate(event.data.client_id)
  end
end })
vim.api.nvim_create_autocmd("BufWritePost", { group = group, callback = function()
  for client_id in pairs(states) do invalidate(client_id) end
end })
vim.api.nvim_create_autocmd("LspRequest", { group = group, callback = function(event)
  local data = event.data
  if data.request.method ~= method then return end
  local state = states[data.client_id]
  if not state then return end
  local identity = tostring(data.client_id) .. ":" .. tostring(data.request_id)
  if data.request.type == "pending" then request_epochs[identity] = state.epoch
  else request_epochs[identity] = nil end
end })
vim.api.nvim_create_autocmd({ "BufWipeout", "LspDetach" }, { group = group, callback = function(event)
  for _, state in pairs(states) do
    for identity, report in pairs(state.reports) do
      if report.buffer == event.buf then state.reports[identity] = nil end
    end
  end
end })

local function remember(state, buffer, params, result, epoch, version)
  if type(result) ~= "table" or epoch ~= state.epoch or not vim.api.nvim_buf_is_valid(buffer) then return end
  if version and version ~= vim.lsp.util.buf_versions[buffer] then return end
  local identity = key(buffer, params.identifier)
  local previous = state.reports[identity]
  local items = result.kind == "unchanged" and previous and previous.items or result.items
  if not items then return end
  state.reports[identity] = { buffer = buffer, items = items, result_id = result.resultId,
    tick = vim.api.nvim_buf_get_changedtick(buffer), epoch = epoch }
end

local function observe(client)
  local state = states[client.id]
  if state and state.client == client then return state end
  state = { client = client, epoch = 0, reports = {}, pending = {} }
  states[client.id] = state
  client.handlers = client.handlers or {}
  local original = client.handlers[method] or vim.lsp.handlers[method]
  state.publish = original
  client.handlers[method] = function(failure, result, context, config)
    local epoch = request_epochs[tostring(client.id) .. ":" .. tostring(context.request_id)]
    if original then original(failure, result, context, config) end
    if not failure then remember(state, context.bufnr, context.params, result, epoch, context.version) end
  end
  local refresh = "workspace/diagnostic/refresh"
  local original_refresh = client.handlers[refresh] or vim.lsp.handlers[refresh]
  client.handlers[refresh] = function(...)
    invalidate(client.id)
    if original_refresh then return original_refresh(...) end
  end
  return state
end

---Reuse completed editor reports and share a pull among concurrent consumers.
---@param client vim.lsp.Client
---@param buffer integer
---@param params table
---@param receive fun(failure: table?, result: table?)
---@return boolean accepted
---@return fun() cancel
function M.request(client, buffer, params, receive)
  local state = observe(client)
  local provider = client.server_capabilities and client.server_capabilities.diagnosticProvider
  if type(provider) == "table" then params.identifier = provider.identifier end
  local identity = key(buffer, params.identifier)
  local tick = vim.api.nvim_buf_get_changedtick(buffer)
  local report = state.reports[identity]
  local waiter = { receive = receive }
  if report and report.epoch == state.epoch and report.tick == tick then
    vim.schedule(function()
      if waiter.receive then
        if report.epoch ~= state.epoch or report.tick ~= vim.api.nvim_buf_get_changedtick(buffer) then
          waiter.receive({ code = -32801, message = "diagnostic source changed" })
        else waiter.receive(nil, { items = report.items }) end
      end
    end)
    return true, function() waiter.receive = nil end
  end
  local pending_key = identity .. ":" .. tostring(state.epoch) .. ":" .. tostring(tick)
  local pending = state.pending[pending_key]
  if not pending then
    pending = { waiters = {}, epoch = state.epoch, tick = tick }
    state.pending[pending_key] = pending
  end
  pending.waiters[waiter] = true
  local function cancel()
    waiter.receive = nil
    pending.waiters[waiter] = nil
    if not next(pending.waiters) and state.pending[pending_key] == pending then
      state.pending[pending_key] = nil
      if pending.id then client:cancel_request(pending.id) end
    end
  end
  if pending.started then return true, cancel end
  pending.started = true
  params.previousResultId = report and report.result_id or nil
  local function complete(failure, result, context)
    if state.pending[pending_key] ~= pending then return end
    state.pending[pending_key] = nil
    if not failure and (not vim.api.nvim_buf_is_valid(buffer) or pending.epoch ~= state.epoch
      or pending.tick ~= vim.api.nvim_buf_get_changedtick(buffer)
      or context and context.version and context.version ~= vim.lsp.util.buf_versions[buffer]) then
      failure = { code = -32801, message = "diagnostic source changed" }
    end
    if not failure then
      if type(result) ~= "table" or (result.kind ~= "unchanged" and type(result.items) ~= "table") then
        failure = { code = -32603, message = "invalid diagnostic report" }
      elseif result.kind == "unchanged" and not report then
        failure = { code = -32603, message = "unchanged diagnostics without a previous report" }
      else
        if state.publish and context then
          local success, message = pcall(state.publish, nil, result, context)
          if not success then failure = { code = -32603, message = "publishing diagnostics failed: " .. tostring(message) } end
        end
        if not failure then
          remember(state, buffer, params, result, pending.epoch, context and context.version)
          local current = state.reports[identity]
          result = current and { items = current.items } or result
        end
      end
    end
    for consumer in pairs(pending.waiters) do
      if consumer.receive then consumer.receive(failure, result) end
    end
  end
  local accepted, request_id = client:request(method, params, complete, buffer)
  pending.id = request_id
  if not accepted then complete({ code = -32603, message = "diagnostic request rejected" }) end
  return true, cancel
end

return M
