local M = {}
local method = "textDocument/diagnostic"
local states = {}
local request_epochs = {}

---Complete each subscriber once, independently of the native request that answered it.
local function settle(state, pending_key, failure, result)
  local pending = state.pending[pending_key]
  if not pending then return end
  state.pending[pending_key] = nil
  for consumer in pairs(pending.waiters) do
    if consumer.receive then consumer.receive(failure, result) end
  end
end

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
  elseif data.request.type == "cancel" then request_epochs[identity] = nil
  else vim.schedule(function() request_epochs[identity] = nil end) end
  if data.request.type == "cancel" or data.request.type == "complete" then
    for pending_key, pending in pairs(state.pending) do
      if pending.buffer == data.request.bufnr then
        if pending.id == data.request_id then pending.id = nil end
        vim.schedule(function()
          if state.pending[pending_key] ~= pending then return end
          request_epochs[identity] = nil
          for _, request in pairs(state.client.requests or {}) do
            if request.type == "pending" and request.method == method and request.bufnr == pending.buffer then return end
          end
          settle(state, pending_key, { code = -32801, message = "diagnostic request ended without a current replacement report" })
        end)
      end
    end
  end
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
  if result.kind ~= "full" and result.kind ~= "unchanged" then return end
  local items = result.kind == "unchanged" and previous and previous.items or result.items
  if not items then return end
  state.reports[identity] = { buffer = buffer, items = items, result_id = result.resultId,
    tick = vim.api.nvim_buf_get_changedtick(buffer), epoch = epoch }
  for pending_key, pending in pairs(state.pending) do
    if pending.buffer == buffer and pending.identity == identity
      and pending.tick == vim.api.nvim_buf_get_changedtick(buffer) then
      settle(state, pending_key, nil, { items = items })
    end
  end
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
    local request_identity = tostring(client.id) .. ":" .. tostring(context.request_id)
    local epoch = request_epochs[request_identity]
    request_epochs[request_identity] = nil
    if original then
      local success, message = pcall(original, failure, result, context, config)
      if not success then failure = { code = -32603, message = "publishing diagnostics failed: " .. tostring(message) } end
    end
    if not failure then remember(state, context.bufnr, context.params, result, epoch, context.version)
    else
      for pending_key, pending in pairs(state.pending) do
        if pending.buffer == context.bufnr and not pending.id then settle(state, pending_key, failure) end
      end
    end
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
    pending = { waiters = {}, epoch = state.epoch, tick = tick, buffer = buffer, identity = identity }
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
    settle(state, pending_key, failure, result)
  end
  local accepted, request_id = client:request(method, params, complete, buffer)
  pending.id = request_id
  if not accepted then complete({ code = -32603, message = "diagnostic request rejected" }) end
  return true, cancel
end

---@class ForgeLspWorkspaceReport
---@field uri string
---@field kind string
---@field items lsp.Diagnostic[]
---@field encoding? string

---Merge every diagnostic namespace owned by the selected live client.
---@param client vim.lsp.Client
---@param refreshed? table<string, boolean> Documents replaced by a native workspace report.
---@return ForgeLspWorkspaceReport[]
function M.stored_reports(client, refreshed)
  local push_namespace = vim.lsp.diagnostic.get_namespace(client.id, false)
  local namespace_set = { [push_namespace] = true }
  local provider = client.server_capabilities and client.server_capabilities.diagnosticProvider
  if type(provider) == "table" then
    namespace_set[vim.lsp.diagnostic.get_namespace(client.id, true, provider.identifier)] = true
  end
  if client._provider_foreach then
    client:_provider_foreach(method, function(capability)
      namespace_set[vim.lsp.diagnostic.get_namespace(client.id, true, capability.identifier)] = true
    end)
  end
  local reports, seen = {}, {}
  for namespace in pairs(namespace_set) do
    for _, diagnostic in ipairs(vim.diagnostic.get(nil, { namespace = namespace })) do
      if vim.api.nvim_buf_is_valid(diagnostic.bufnr) then
        local path = vim.api.nvim_buf_get_name(diagnostic.bufnr)
        local identity = vim.json.encode({ path, diagnostic.lnum, diagnostic.col,
          diagnostic.end_lnum, diagnostic.end_col, diagnostic.severity, diagnostic.code, diagnostic.message })
        if path ~= "" and not seen[identity]
          and (namespace == push_namespace or not refreshed or not refreshed[vim.uri_from_fname(path)]) then
          seen[identity] = true
          local report = reports[path]
          if not report then
            report = { uri = vim.uri_from_fname(path), kind = "full", items = {}, encoding = "utf-8" }
            reports[path] = report
          end
          report.items[#report.items + 1] = {
            range = { start = { line = diagnostic.lnum, character = diagnostic.col },
              ["end"] = { line = diagnostic.end_lnum, character = diagnostic.end_col } },
            severity = diagnostic.severity, code = diagnostic.code, message = diagnostic.message,
          }
        end
      end
    end
  end
  return vim.tbl_values(reports)
end

---Read native workspace diagnostics and every stored push or pull report.
---@param client vim.lsp.Client
---@param receive fun(failure: table?, result: {items: ForgeLspWorkspaceReport[]}?, acquisition: "native"|"published")
---@return boolean accepted
---@return fun() cancel
function M.workspace(client, receive)
  local provider = client.server_capabilities and client.server_capabilities.diagnosticProvider
  if type(provider) == "table" and provider.workspaceDiagnostics == true then
    local accepted, request_id = client:request("workspace/diagnostic", {
      identifier = provider.identifier, previousResultIds = {},
    }, function(failure, result)
      if not failure and type(result) == "table" and type(result.items) == "table" then
        local refreshed = {}
        for _, report in ipairs(result.items) do
          if report.uri then refreshed[report.uri] = true end
        end
        vim.list_extend(result.items, M.stored_reports(client, refreshed))
      end
      receive(failure, result, "native")
    end)
    return accepted, function() if request_id then client:cancel_request(request_id) end end
  end
  if client:is_stopped() or vim.lsp.get_client_by_id(client.id) ~= client then
    receive({ message = "selected diagnostic publisher is no longer running" }, nil, "published")
    return true, function() end
  end
  if not vim.lsp.diagnostic or not vim.lsp.diagnostic.get_namespace or not vim.diagnostic.get then
    receive({ code = -32601, message = "published diagnostic acquisition is unsupported" }, nil, "published")
    return true, function() end
  end
  receive(nil, { items = M.stored_reports(client) }, "published")
  return true, function() end
end

return M
