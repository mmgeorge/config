local M = {}

local builder = require("diff_review.harness.builder")
local config = require("diff_review.infra.config")
local notifications = require("diff_review.infra.notifications")
local protocol = require("diff_review.harness.protocol")
local session = require("diff_review.session")

local launcher_for_test = nil
local shutdown_autocmd = false

---@class DiffReviewHarnessClient
---@field process table?
---@field generation integer
---@field next_id integer
---@field pending table<integer, { method: string, callback: fun(result?: any, error?: string) }>
---@field subscriber table<integer, fun(event: string, payload: any)>
---@field next_subscriber integer
---@field stdout_buffer string
---@field stderr string
---@field ready boolean
---@field starting boolean
---@field initialize_callback function[]
---@field snapshot table?

---@return DiffReviewHarnessClient
local function state()
  if session.harness.client then return session.harness.client end
  session.harness.client = {
    process = nil,
    generation = 0,
    next_id = 0,
    pending = {},
    subscriber = {},
    next_subscriber = 0,
    stdout_buffer = "",
    stderr = "",
    ready = false,
    starting = false,
    initialize_callback = {},
    snapshot = nil,
  }
  return session.harness.client
end

---@return DiffReviewHarnessBackendDescriptor
local function backend_descriptor()
  local harness_config = config.options.harness
  local backend = harness_config.backend
  return require("diff_review.harness.backends." .. backend).descriptor(harness_config)
end

---@param callback fun(result?: any, error?: string)
---@param result? any
---@param start_error? string
local function finish_start(callback, result, start_error)
  local client = state()
  client.starting = false
  callback(result, start_error)
  local callback_list = client.initialize_callback
  client.initialize_callback = {}
  for _, queued_callback in ipairs(callback_list) do queued_callback(result, start_error) end
end

local function ensure_shutdown_autocmd()
  if shutdown_autocmd then return end
  shutdown_autocmd = true
  local group = vim.api.nvim_create_augroup("DiffReviewHarnessClient", { clear = true })
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function() M.stop() end,
  })
end

---@param message table
local function dispatch_message(message)
  local client = state()
  if message.event then
    for _, subscriber in pairs(client.subscriber) do
      local ok, err = pcall(subscriber, message.event, message.payload)
      if not ok then notifications.error("Harness event subscriber failed: " .. tostring(err), "Harness") end
    end
    return
  end
  local pending = client.pending[message.id]
  if not pending then return end
  client.pending[message.id] = nil
  if message.error then
    pending.callback(nil, tostring(message.error.message or message.error.code or "Harness request failed"))
    if pending.method ~= "initialize" and pending.method ~= "state.get" then
      for _, subscriber in pairs(client.subscriber) do
        local ok, err = pcall(subscriber, "state_invalidated", { method = pending.method })
        if not ok then notifications.error("Harness event subscriber failed: " .. tostring(err), "Harness") end
      end
    end
  else
    pending.callback(message.result, nil)
  end
end

---@param chunk string?
---@param generation integer
local function consume_stdout(chunk, generation)
  local client = state()
  if generation ~= client.generation or not chunk then return end
  client.stdout_buffer = client.stdout_buffer .. chunk
  while true do
    local newline = client.stdout_buffer:find("\n", 1, true)
    if not newline then break end
    local line = client.stdout_buffer:sub(1, newline - 1)
    client.stdout_buffer = client.stdout_buffer:sub(newline + 1)
    if line ~= "" then
      local message, decode_error = protocol.decode_message(line)
      if message then
        vim.schedule(function()
          if generation == state().generation then dispatch_message(message) end
        end)
      else
        vim.schedule(function() notifications.error(decode_error or "Invalid Harness broker message", "Harness") end)
      end
    end
  end
end

---@param callback fun(result?: any, error?: string)
local function send_initialize(callback)
  local client = state()
  local harness_config = config.options.harness
  local descriptor = backend_descriptor()
  client.next_id = client.next_id + 1
  local id = client.next_id
  client.pending[id] = { method = "initialize", callback = function(result, request_error)
    if request_error then
      client.ready = false
      finish_start(callback, nil, request_error)
      return
    end
    client.ready = true
    client.snapshot = result
    session.harness.ready = true
    finish_start(callback, result, nil)
  end }
  local payload = protocol.encode_request(id, "initialize", {
    data_root = vim.fs.joinpath(vim.fn.stdpath("data"), "diff-review", "harness"),
    workspace = vim.fn.getcwd(),
    client_id = ("nvim-%s-%s"):format(vim.fn.getpid(), vim.uv.hrtime()),
    backend = { kind = descriptor.kind, command = descriptor.command },
    model = harness_config.model,
    effort = harness_config.effort,
    trust_profile = harness_config.trust_profile,
    trust_policy = harness_config.trust_profiles[harness_config.trust_profile],
    goal_max_turns = harness_config.goal_max_turns,
  })
  local ok, write_error = pcall(client.process.write, client.process, payload)
  if not ok then
    client.pending[id] = nil
    finish_start(callback, nil, "Failed to initialize Harness broker: " .. tostring(write_error))
  end
end

---@param binary string
---@param callback fun(result?: any, error?: string)
local function launch(binary, callback)
  local client = state()
  client.generation = client.generation + 1
  local generation = client.generation
  client.stdout_buffer = ""
  client.stderr = ""
  local command = { binary }
  local options = {
    text = true,
    stdin = true,
    stdout = function(_, data) consume_stdout(data, generation) end,
    stderr = function(_, data)
      if data and generation == state().generation then client.stderr = client.stderr .. data end
    end,
  }
  local on_exit = function(result)
    vim.schedule(function()
      local current = state()
      if generation ~= current.generation then return end
      current.process = nil
      current.ready = false
      current.snapshot = nil
      current.starting = false
      session.harness.ready = false
      if result.code ~= 0 then
        local message = vim.trim(current.stderr)
        notifications.error("Harness broker exited " .. tostring(result.code) .. (message ~= "" and (": " .. message) or ""), "Harness")
      end
      for id, pending in pairs(current.pending) do
        current.pending[id] = nil
        pending.callback(nil, "Harness broker stopped")
      end
    end)
  end
  local ok, process_or_error
  if launcher_for_test then
    ok, process_or_error = pcall(launcher_for_test, command, options, on_exit)
  else
    ok, process_or_error = pcall(vim.system, command, {
      text = true,
      stdin = true,
      stdout = function(_, data) consume_stdout(data, generation) end,
      stderr = function(_, data)
        if data and generation == state().generation then client.stderr = client.stderr .. data end
      end,
    }, on_exit)
  end
  if not ok then
    finish_start(callback, nil, "Failed to start Harness broker: " .. tostring(process_or_error))
    return
  end
  client.process = process_or_error
  ensure_shutdown_autocmd()
  send_initialize(callback)
end

---@param callback fun(result?: any, error?: string)
function M.start(callback)
  callback = callback or function() end
  local client = state()
  if client.ready and client.process then
    vim.schedule(function() callback(client.snapshot or {}, nil) end)
    return
  end
  if client.starting then
    client.initialize_callback[#client.initialize_callback + 1] = callback
    return
  end
  client.starting = true
  builder.ensure(function(build_result)
    if not build_result.ok or not build_result.path then
      finish_start(callback, nil, build_result.message or "Harness broker build failed")
      return
    end
    launch(build_result.path, callback)
  end)
end

---@param method string
---@param params? table
---@param callback? fun(result?: any, error?: string)
function M.request(method, params, callback)
  callback = callback or function() end
  M.start(function(_, start_error)
    if start_error then callback(nil, start_error) return end
    local client = state()
    if not client.process then callback(nil, "Harness broker is not running") return end
    client.next_id = client.next_id + 1
    local id = client.next_id
    client.pending[id] = { method = method, callback = callback }
    local ok, write_error = pcall(client.process.write, client.process, protocol.encode_request(id, method, params))
    if not ok then
      client.pending[id] = nil
      callback(nil, "Failed to write Harness request: " .. tostring(write_error))
    end
  end)
end

---@param callback fun(event: string, payload: any)
---@return fun()
function M.subscribe(callback)
  local client = state()
  client.next_subscriber = client.next_subscriber + 1
  local subscriber_id = client.next_subscriber
  client.subscriber[subscriber_id] = callback
  return function() client.subscriber[subscriber_id] = nil end
end

function M.stop()
  local client = state()
  local process = client.process
  client.generation = client.generation + 1
  if process then
    pcall(process.write, process, protocol.encode_request(client.next_id + 1, "shutdown", {}))
    vim.defer_fn(function() pcall(process.kill, process, 15) end, 250)
  end
  client.process = nil
  client.ready = false
  client.snapshot = nil
  client.starting = false
  session.harness.ready = false
  for id, pending in pairs(client.pending) do
    client.pending[id] = nil
    pending.callback(nil, "Harness broker stopped")
  end
  local callback_list = client.initialize_callback
  client.initialize_callback = {}
  for _, callback in ipairs(callback_list) do callback(nil, "Harness broker stopped") end
end

function M._set_launcher_for_test(launcher) launcher_for_test = launcher end
function M._reset_for_test()
  M.stop()
  session.harness.client = nil
  launcher_for_test = nil
  shutdown_autocmd = false
end

return M
