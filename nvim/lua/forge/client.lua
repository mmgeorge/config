local M = {}

local builder = require("forge.builder")
local config = require("forge.infra.config")
local notifications = require("forge.infra.notifications")
local protocol = require("forge.protocol")
local session = require("forge.session")
local receive = require("forge.receive")
local json_transfer = require("forge.json_transfer")

local launcher_for_test = nil
local shutdown_autocmd = false
local RESERVED_METHOD = {
  initialize = true,
  ["harness.initialize"] = true,
  ["plan.scope_deviation_review"] = true,
  ["turn.cancel"] = true,
  ["turn.restart"] = true,
  ["approval.resolve"] = true,
  shutdown = true,
}

---@alias ForgeCallback fun(result?: any, error?: string, error_detail?: table)
---@alias ForgeSubscriber fun(event: string, payload: any, session_id?: string)

---@class ForgeClient
---@field process table?
---@field generation integer
---@field launch_token integer
---@field next_id integer
---@field pending table<integer, { method: string, callback: ForgeCallback, progress?: fun(payload: table) }>
---@field transfer table<integer, ForgeJsonTransfer>
---@field transfer_bytes integer
---@field subscriber table<integer, ForgeSubscriber>
---@field next_subscriber integer
---@field receiver table?
---@field shutdown_timer uv_timer_t?
---@field shutdown_timer_token integer?
---@field taskkill_process table?
---@field stderr string
---@field ready boolean
---@field starting boolean
---@field draining boolean
---@field stop_reason string?
---@field start_callback ForgeCallback?
---@field start_started? integer
---@field initialize_callback ForgeCallback[]
---@field harness_initialize_callback ForgeCallback[]
---@field harness_ready boolean
---@field harness_starting boolean
---@field snapshot_by_id table<string, table>
---@field snapshot table?

---@type ForgeClient?
M._client = nil

---@return ForgeClient
local function state()
  if M._client then return M._client end
  M._client = {
    process = nil,
    generation = 0,
    launch_token = 0,
    next_id = 0,
    pending = {},
    transfer = {},
    transfer_bytes = 0,
    subscriber = {},
    document_subscriber = {},
    completed_status_operation = {},
    completed_status_order = {},
    next_subscriber = 0,
    receiver = nil,
    shutdown_timer = nil,
    shutdown_timer_token = nil,
    taskkill_process = nil,
    stderr = "",
    ready = false,
    starting = false,
    draining = false,
    initialize_callback = {},
    snapshot = nil,
    snapshot_by_id = {},
    harness_ready = false,
    harness_starting = false,
    harness_initialize_callback = {},
  }
  return M._client
end

local function protect_consumer(callback, context)
  return function(...)
    local succeeded, failure = pcall(callback, ...)
    if not succeeded then
      local diagnostic = "Forge " .. context .. " callback failed: " .. tostring(failure)
      require("forge.startup_log").write("host.callback.failed", { context = context, error = tostring(failure) })
      if not pcall(notifications.error, diagnostic, "Forge") then pcall(vim.api.nvim_err_writeln, diagnostic) end
    end
  end
end

---@param method string
---@param callback ForgeCallback
---@return integer?
local function admit_request(method, callback)
  local client = state()
  if client.draining then callback(nil, "Forge host is draining") return nil end
  local count, ordinary = 0, 0
  for _, pending in pairs(client.pending) do
    count = count + 1
    if not RESERVED_METHOD[pending.method] then ordinary = ordinary + 1 end
  end
  if count >= 64 or (not RESERVED_METHOD[method] and ordinary >= 60) or client.next_id >= 9007199254740991 then
    callback(nil, "Forge request admission is full")
    return nil
  end
  client.next_id = client.next_id + 1
  client.pending[client.next_id] = { method = method, callback = callback }
  return client.next_id
end

---@param id integer
---@param method string
---@param params table
---@param session_id? string
local function write_request(id, method, params, session_id)
  require("forge.startup_log").write("host.request.send", { id = id, method = method, operation = params and params.operation,
    document = params and params.document })
  local client = state()
  if not client.pending[id] then return end
  local ok, failure = pcall(function()
    local encoded = protocol.encode_request(id, method, params, session_id)
    assert(#encoded <= 512 * 1024, "Forge request exceeds frame limit")
    assert(client.process, "Forge host is not running")
    client.process:write(encoded)
  end)
  if not ok and client.pending[id] then
    local pending = client.pending[id]
    client.pending[id] = nil
    pending.callback(nil, "Failed to write Forge request: " .. tostring(failure))
  end
end

---@return ForgeHarnessBackendDescriptor
local function backend_descriptor()
  local harness_config = config.options.harness
  local backend = harness_config.backend
  return require("forge.harness.backends." .. backend).descriptor(harness_config)
end

---@param callback ForgeCallback
---@param result? any
---@param start_error? string
---@param error_detail? table
local function finish_start(callback, result, start_error, error_detail)
  local client = state()
  require("forge.startup_log").write("host.initialize.finished", { error = start_error,
    elapsed_ms = client.start_started and math.floor((vim.uv.hrtime() - client.start_started) / 1e6) or nil })
  client.start_started = nil
  client.starting = false
  client.start_callback = nil
  local callback_list = client.initialize_callback
  client.initialize_callback = {}
  callback(result, start_error, error_detail)
  for _, queued_callback in ipairs(callback_list) do queued_callback(result, start_error, error_detail) end
end

---@param callback ForgeCallback
---@param result? any
---@param start_error? string
---@param error_detail? table
local function finish_harness_start(callback, result, start_error, error_detail)
  local client = state()
  client.harness_starting = false
  local queued = client.harness_initialize_callback
  client.harness_initialize_callback = {}
  callback(result, start_error, error_detail)
  for _, waiting in ipairs(queued) do waiting(result, start_error, error_detail) end
end

local function ensure_shutdown_autocmd()
  if shutdown_autocmd then return end
  shutdown_autocmd = true
  local group = vim.api.nvim_create_augroup("ForgeClient", { clear = true })
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = function() M.stop() end,
  })
end

---@param client ForgeClient
---@param message table
---@return table? response
---@return string? failure
local function receive_result(client, message)
  local document = message.document
  local request_id = document and ("document:" .. document) or message.request_id
  local part = message.payload
  local transfer = client.transfer[request_id]
  if message.event == "result.part" or message.event == "document.part" then
    if not json_transfer.valid(part) then return nil, "invalid result part" end
    for key in pairs(part) do
      if key ~= "sequence" and key ~= "part_count" and key ~= "total_bytes" and key ~= "payload" then
        return nil, "unexpected result part field"
      end
    end
    if not transfer then
      local active = 0
      for _ in pairs(client.transfer) do active = active + 1 end
      if part.sequence ~= 0 or active >= 2 or client.transfer_bytes + part.total_bytes > 2 * json_transfer.MAX_BYTES then
        return nil, "result transfer admission or starting sequence is invalid"
      end
      transfer = json_transfer.new()
      transfer.started_at = vim.uv.hrtime()
      transfer.accept_us = 0
      client.transfer[request_id] = transfer
      client.transfer_bytes = client.transfer_bytes + part.total_bytes
    end
    local started_at = vim.uv.hrtime()
    local failure = json_transfer.accept(transfer, part)
    transfer.accept_us = transfer.accept_us + math.floor((vim.uv.hrtime() - started_at) / 1000)
    return nil, failure
  end
  if not transfer or type(part) ~= "table" or part.part_count ~= transfer.part_count or part.total_bytes ~= transfer.total_bytes then
    return nil, "result completion has no matching transfer"
  end
  for key in pairs(part) do
    if key ~= "part_count" and key ~= "total_bytes" then return nil, "unexpected result completion field" end
  end
  local assembly_started = vim.uv.hrtime()
  local encoded, failure = json_transfer.finish(transfer)
  local assembly_us = math.floor((vim.uv.hrtime() - assembly_started) / 1000)
  if not encoded then return nil, failure end
  client.transfer[request_id] = nil
  client.transfer_bytes = client.transfer_bytes - transfer.total_bytes
  local decode_started = vim.uv.hrtime()
  local decoded, response = pcall(vim.json.decode, encoded)
  require("forge.startup_log").write("host.result.decoded", { id = request_id, bytes = #encoded,
    parts = transfer.part_count, accept_us = transfer.accept_us, assembly_us = assembly_us,
    decode_us = math.floor((vim.uv.hrtime() - decode_started) / 1000),
    transfer_elapsed_us = math.floor((vim.uv.hrtime() - transfer.started_at) / 1000) })
  if document then
    if not decoded or type(response) ~= "table" or response.document ~= document or response.event ~= "status.update" or type(response.payload) ~= "table" then
      return nil, "invalid assembled document update"
    end
    return response, nil
  end
  if not decoded or type(response) ~= "table" or response.id ~= request_id
    or ((response.result ~= nil) == (response.error ~= nil)) then return nil, "invalid assembled result identity or JSON" end
  for key in pairs(response) do
    if key ~= "id" and key ~= "result" and key ~= "error" then return nil, "unexpected assembled response field" end
  end
  if response.error ~= nil and (type(response.error) ~= "table"
    or type(response.error.code) ~= "string" or type(response.error.message) ~= "string") then
    return nil, "invalid assembled response error"
  end
  return response, nil
end

---@param process table
---@param still_running fun(): boolean
---@param finished fun(): nil
local function force_collect_process(client, process, still_running, finished)
  if vim.fn.has("win32") ~= 1 then
    pcall(process.kill, process, 9)
    vim.defer_fn(function()
      if not still_running() then return end
      local collected, result = pcall(process.wait, process, 0)
      if not collected then
        notifications.error("Forge forced shutdown could not collect its process: " .. tostring(result), "Forge")
        return
      end
      finished(result)
    end, 1000)
    return
  end
  local pid = process.pid
  if type(pid) ~= "number" or pid < 1 or pid % 1 ~= 0 then
    notifications.error("Forge forced shutdown has no valid Windows process ID", "Forge")
    return
  end
  local taskkill_process
  taskkill_process = vim.system({ "taskkill", "/PID", tostring(pid), "/T", "/F" }, {
    text = true,
    stdout = true,
    stderr = true,
    timeout = 2000,
  }, function(result)
    vim.schedule(function()
      if client.taskkill_process == taskkill_process then client.taskkill_process = nil end
      if not still_running() then return end
      if result.code ~= 0 then
        notifications.error("Forge forced shutdown failed: " .. vim.trim(result.stderr or "taskkill failed"), "Forge")
        return
      end
      local collected, collection_failure = pcall(process.wait, process, 0)
      if not collected then
        notifications.error("Forge forced shutdown could not collect its process: " .. tostring(collection_failure), "Forge")
        return
      end
      finished(collection_failure)
    end)
  end)
  client.taskkill_process = taskkill_process
end

---@param client ForgeClient
---@param launch_token integer
local function cancel_shutdown_timer(client, launch_token)
  if client.shutdown_timer_token ~= launch_token then return end
  local timer = client.shutdown_timer
  client.shutdown_timer = nil
  client.shutdown_timer_token = nil
  if timer and not timer:is_closing() then
    timer:stop()
    timer:close()
  end
end

---@param message table
local function dispatch_message(message, timing)
  local client = state()
  if message.document ~= nil then
    if message.event == "document.part" or message.event == "document.complete" then
      local assembled, failure = receive_result(client, message)
      if failure then
        local key = "document:" .. message.document
        local transfer = client.transfer[key]
        if transfer then client.transfer_bytes = client.transfer_bytes - transfer.total_bytes end
        client.transfer[key] = nil
        message = { document = message.document, event = "status.resync", payload = { diagnostic = { failure } } }
      elseif assembled then message = assembled
      else return end
    end
    local update = message.payload
    if message.event == "status.update" and (update.phase == "settled" or update.phase == "failed") then
      local key = tostring(client.generation) .. ":" .. tostring(update.operation_id)
      if not client.completed_status_operation[key] then
        client.completed_status_operation[key] = true
        client.completed_status_order[#client.completed_status_order + 1] = key
        if #client.completed_status_order > 128 then client.completed_status_operation[table.remove(client.completed_status_order, 1)] = nil end
        for _, diagnostic in ipairs(update.diagnostic or {}) do notifications.error(diagnostic, "Forge status") end
      end
    elseif message.event == "status.resync" or (message.event == "status.update" and update.phase ~= "accepted") then
      for _, diagnostic in ipairs(update.diagnostic or {}) do notifications.error(diagnostic, "Forge status") end
    end
    for _, subscriber in pairs(client.document_subscriber) do
      if message.document == "" or message.document == subscriber.document then
        subscriber.callback(message.event, update, client.generation)
      end
    end
    return
  end
  local measured = client.pending[message.request_id or message.id]
  if measured and timing then
    measured.timing = measured.timing or { frames = 0, bytes = 0, decode_us = 0, maximum_queue_wait_us = 0,
      first_frame_us = math.floor(timing.received_at / 1000) }
    local aggregate = measured.timing
    aggregate.frames = aggregate.frames + 1
    aggregate.bytes = aggregate.bytes + timing.bytes
    aggregate.decode_us = aggregate.decode_us + timing.decode_us
    aggregate.maximum_queue_wait_us = math.max(aggregate.maximum_queue_wait_us, timing.queue_wait_us)
  end
  if message.request_id then
    local pending = client.pending[message.request_id]
    if pending and (message.event == "result.part" or message.event == "result.complete") then
      local result, failure = receive_result(client, message)
      if failure then error("Forge result transfer failed: " .. failure) end
      if not result then return end
      message = result
    else
      if pending and pending.progress and message.event == pending.method .. ".progress" then
        local ok, failure = pcall(pending.progress, message.payload)
        if not ok then
          local diagnostic = "Forge request progress callback failed: " .. tostring(failure)
          if not pcall(notifications.error, diagnostic, "Forge") then pcall(vim.api.nvim_err_writeln, diagnostic) end
        end
      end
      return
    end
  end
  if message.event then
    local event_snapshot = client.snapshot_by_id[message.session_id]
    if event_snapshot and (message.event == "plan_question" or message.event == "plan_question_updated") then
      event_snapshot.active_plan = vim.deepcopy(message.payload and message.payload.plan or nil)
    elseif event_snapshot and (message.event == "question" or message.event == "question_updated") then
      event_snapshot.active_elicitation = vim.deepcopy(message.payload)
    elseif event_snapshot and message.event == "question_answered" then
      event_snapshot.active_elicitation = nil
    end
    for _, subscriber in pairs(client.subscriber) do
      local ok, err = pcall(subscriber, message.event, message.payload, message.session_id)
      if not ok then notifications.error("Harness event subscriber failed: " .. tostring(err), "ForgeHarness") end
    end
    return
  end
  local pending = client.pending[message.id]
  if not pending then return end
  if pending.timing then
    require("forge.startup_log").write("host.response.received", { id = message.id, method = pending.method,
      transport = pending.timing })
  end
  local transfer = client.transfer[message.id]
  if transfer then
    if not message.error then error("Forge result transfer was replaced before completion") end
    client.transfer[message.id] = nil
    client.transfer_bytes = client.transfer_bytes - transfer.total_bytes
  end
  client.pending[message.id] = nil
  if message.error then
    pending.callback(
      nil,
      tostring(message.error.message or message.error.code or "Harness request failed"),
      message.error
    )
    if pending.method ~= "initialize"
      and pending.method ~= "harness.initialize"
      and pending.method ~= "repository.revisions"
      and pending.method:sub(1, 11) ~= "repository."
      and pending.method ~= "github.sync"
      and pending.method ~= "github.issues"
      and pending.method ~= "github.detail"
          and pending.method ~= "github.metadata"
          and pending.method ~= "github.pull_request"
          and pending.method ~= "github.comment"
      and pending.method:sub(1, 7) ~= "review."
      and pending.method ~= "state.get"
      and pending.method ~= "session.configure"
      and pending.method ~= "session.execution_mode"
    then
      for _, subscriber in pairs(client.subscriber) do
        local ok, err = pcall(subscriber, "state_invalidated", { method = pending.method })
        if not ok then notifications.error("Harness event subscriber failed: " .. tostring(err), "ForgeHarness") end
      end
    end
  else
    if pending.method == "state.get" then
      client.snapshot = message.result
      local result_session_id = message.result and message.result.session and message.result.session.id
      if result_session_id then client.snapshot_by_id[result_session_id] = message.result end
    elseif pending.method == "history.record" and client.snapshot then
      client.snapshot.prompt_history = vim.deepcopy(message.result or {})
    elseif (pending.method == "question.answer" or pending.method == "question.skip")
      and client.snapshot
    then
      client.snapshot = vim.deepcopy(message.result)
    end
    local result_session_id = message.result and message.result.session and message.result.session.id
    if result_session_id then client.snapshot_by_id[result_session_id] = message.result end
    pending.callback(message.result, nil)
  end
end

---@param chunk string?
---@param generation integer
local function consume_stdout(chunk, generation)
  local client = state()
  if generation ~= client.generation or not chunk or not client.receiver then return end
  receive.push(client.receiver, chunk)
end

---@param callback fun(result?: any, error?: string, error_detail?: table)
local function send_harness_initialize(callback, initialize_options)
  local client = state()
  local harness_config = config.options.harness
  local descriptor = backend_descriptor()
  local function initialized(result, request_error, error_detail)
    if request_error then
      client.harness_ready = false
      finish_harness_start(callback, nil, request_error, error_detail)
      return
    end
    client.snapshot = result
    local initialized_session_id = result and result.session and result.session.id
    if initialized_session_id then client.snapshot_by_id[initialized_session_id] = result end
    local plan_config = harness_config.plan or {}
    M.request_host("plan.scope_deviation_review", { policy = plan_config.scope_deviation_review or "auto" }, function(_, policy_error, policy_error_detail)
      if policy_error then
        client.harness_ready = false
        session.harness.ready = false
        finish_harness_start(callback, nil, policy_error, policy_error_detail)
        return
      end
      client.harness_ready = true
      session.harness.ready = true
      finish_harness_start(callback, result, nil)
    end)
  end
  local params = {
    data_root = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"),
    permission_file = vim.fs.joinpath(vim.fn.stdpath("config"), "forge", "permissions.json"),
    workspace = vim.fn.getcwd(),
    client_id = ("nvim-%s-%s"):format(vim.fn.getpid(), vim.uv.hrtime()),
    backend = { kind = descriptor.kind, command = descriptor.command },
    model = harness_config.model,
    effort = harness_config.effort,
    goal_max_turns = harness_config.goal_max_turns,
    lease_conflict_action = initialize_options and initialize_options.lease_conflict_action or nil,
    new_session_name = initialize_options and initialize_options.new_session_name or nil,
  }
  M.request_host("harness.initialize", params, initialized)

end

---@param callback ForgeCallback
local function send_initialize(callback)
  local client = state()
  client.next_id = client.next_id + 1
  local id = client.next_id
  client.pending[id] = { method = "initialize", callback = function(result, failure, detail)
    if failure then finish_start(callback, nil, failure, detail) return end
    if type(result) ~= "table" or result.protocol_version ~= protocol.VERSION then
      local mismatch = "Forge client and host wire versions differ. Rebuild Forge and restart the client."
      finish_start(callback, nil, mismatch)
      M.stop(mismatch)
      return
    end
    if type(result.git_config_cache) == "table" then
      require("forge.startup_log").write("host.git_config_cache", result.git_config_cache)
      if result.git_config_cache.warning then notifications.error(result.git_config_cache.warning, "Forge") end
    end
    client.ready = not client.draining
    finish_start(callback, { generation = client.generation }, nil)
  end }
  local ok, failure = pcall(client.process.write, client.process, protocol.encode_request(id, "initialize", {
    protocol_version = protocol.VERSION,
    recovery_directory = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "recovery", "github", "v1"),
    status_ignored_directory = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "status-ignored"),
    git_config_cache_path = require("forge.git_config_cache").path(),
  }))
  if not ok then
    client.pending[id] = nil
    finish_start(callback, nil, "Failed to initialize Forge host: " .. tostring(failure))
  end
end

---@param binary string
---@param callback fun(result?: any, error?: string, error_detail?: table)
---@param launch_token integer
---@param lease? RustSidecarLease
local function spawn(binary, callback, launch_token, lease)
  local client = state()
  if M._client ~= client or client.launch_token ~= launch_token or client.start_callback ~= callback then return end
  client.generation = client.generation + 1
  local generation = client.generation
  if client.receiver then receive.close(client.receiver) end
  client.stderr = ""
  local process_launcher = launcher_for_test or vim.system
  local command = { binary }
  local skip_line_stats = vim.g.forge_skip_line_stats == true
  if skip_line_stats then command[#command + 1] = "--diagnostic-skip-line-stats" end
  require("forge.startup_log").write("host.spawn", { binary = binary, generation = generation,
    line_stats_skipped = skip_line_stats })
  local process
  local options = {
    text = true,
    stdin = true,
    stdout = function(_, data) consume_stdout(data, generation) end,
    stderr = function(_, data)
      if data then require("forge.startup_log").write("host.stderr", { text = data:sub(-8192), generation = generation }) end
      if data and generation == state().generation then client.stderr = (client.stderr .. data:sub(-65536)):sub(-65536) end
    end,
  }
  local receiver
  local finish_exit = function(result)
    if lease then
      local released, failure = pcall(lease.release)
      if not released then notifications.error(tostring(failure), "Forge") end
      lease = nil
    end
    local current = state()
    if generation ~= current.generation or launch_token ~= current.launch_token then return end
    cancel_shutdown_timer(current, launch_token)
    current.process = nil
    if current.receiver == receiver then current.receiver = nil end
    current.ready = false
    current.harness_ready = false
    current.snapshot = nil
    current.starting = false
    current.draining = false
    session.harness.ready = false
    if result.code ~= 0 then
      local message = vim.trim(current.stderr)
      notifications.error("Harness broker exited " .. tostring(result.code) .. (message ~= "" and (": " .. message) or ""), "ForgeHarness")
    end
    local pending_list = current.pending
    current.pending = {}
    current.transfer = {}
    current.transfer_bytes = 0
    for id, pending in pairs(pending_list) do
      pending.callback(nil, (current.stop_reason or "Forge host stopped") .. ": unresolved request " .. id .. " (" .. pending.method .. ")", {
        code = "outcome_unknown", request_id = id, method = pending.method,
      })
    end
    current.stop_reason = nil
  end
  local consumed_bytes, consumed_frames = 0, 0
  receiver = receive.new({
    active = function() return generation == state().generation end,
    decode = protocol.decode_message,
    dispatch = dispatch_message,
    consumed = function(bytes, frames)
      consumed_bytes, consumed_frames = consumed_bytes + bytes, consumed_frames + frames
      if generation ~= state().generation or not client.process then return end
      client.process:write(protocol.encode_request(0, "transport.consumed", {
        bytes = consumed_bytes, frames = consumed_frames,
      }))
    end,
    failed = function(message)
      M.stop()
      if not pcall(notifications.error, message, "ForgeHarness") then pcall(vim.api.nvim_err_writeln, message) end
    end,
    finished = finish_exit,
  })
  client.receiver = receiver
  local on_exit = function(result)
    require("forge.startup_log").write("host.exit", { code = result.code, signal = result.signal, generation = generation })
    if M._client == client and generation == client.generation then
      if receiver.failure or receiver.closed then
        vim.schedule(function()
          if M._client == client and generation == client.generation then finish_exit(result) end
        end)
      else
        receive.finish(receiver, result)
      end
    elseif lease then
      vim.schedule(function()
        if not lease then return end
        local released, failure = pcall(lease.release)
        lease = nil
        if not released then notifications.error(tostring(failure), "Forge") end
      end)
    end
  end
  local spawn_started = vim.uv.hrtime()
  local ok, process_or_error = pcall(process_launcher, command, options, on_exit)
  if not ok then
    if lease then pcall(lease.release) end
    finish_start(callback, nil, "Failed to start Harness broker: " .. tostring(process_or_error))
    return
  end
  process = process_or_error
  client.process = process
  require("forge.startup_log").write("host.spawned", { pid = process.pid, generation = generation,
    elapsed_ms = math.floor((vim.uv.hrtime() - spawn_started) / 1e6) })
  ensure_shutdown_autocmd()
  send_initialize(callback)
end

---@param binary string
---@param callback fun(result?: any, error?: string, error_detail?: table)
---@param launch_token integer
local function launch(binary, callback, launch_token)
  if launcher_for_test then spawn(binary, callback, launch_token) return end
  local client = state()
  local lease_started = vim.uv.hrtime()
  builder.acquire(binary, function(lease, failure)
    require("forge.startup_log").write("host.lease.finished", { error = failure,
      elapsed_ms = math.floor((vim.uv.hrtime() - lease_started) / 1e6) })
    if M._client ~= client or client.launch_token ~= launch_token or client.start_callback ~= callback then
      if lease then
        local released, release_error = pcall(lease.release)
        if not released then notifications.error(tostring(release_error), "Forge") end
      end
      return
    end
    if not lease then finish_start(callback, nil, failure) return end
    spawn(lease.path, callback, launch_token, lease)
  end)
end

---@param callback fun(result?: any, error?: string, error_detail?: table)
local function start_process(callback)
  require("forge.startup_log").write("host.start")
  local host_started = vim.uv.hrtime()
  local client = state()
  client.start_started = host_started
  client.starting = true
  client.start_callback = callback
  client.launch_token = client.launch_token + 1
  local launch_token = client.launch_token
  builder.ensure(function(executable_result)
    require("forge.startup_log").write("host.binary.result", { ok = executable_result.ok, path = executable_result.path,
      error = executable_result.message, elapsed_ms = math.floor((vim.uv.hrtime() - host_started) / 1e6) })
    vim.schedule(function()
      if M._client ~= client or client.launch_token ~= launch_token or client.start_callback ~= callback then return end
      if not executable_result.ok or not executable_result.path then
        finish_start(callback, nil, executable_result.message or "Forge executable is unavailable")
        return
      end
      launch(executable_result.path, callback, launch_token)
    end)
  end)
end

---@param callback fun(result?: any, error?: string, error_detail?: table)
function M.start(callback)
  callback = protect_consumer(callback or function() end, "startup")
  local client = state()
  if client.draining then callback(nil, "Forge host is draining") return end
  if client.ready and client.process then
    local generation = client.generation
    vim.schedule(function()
      if generation ~= client.generation or not client.ready or not client.process then
        callback(nil, "Forge host generation changed during startup")
      else callback({ generation = generation }, nil) end
    end)
    return
  end
  if client.starting then
    if #client.initialize_callback >= 64 then callback(nil, "Forge startup admission is full") return end
    client.initialize_callback[#client.initialize_callback + 1] = callback
    return
  end
  start_process(callback)
end

---@param callback? ForgeCallback
---@param initialize_options? table
function M.start_harness(callback, initialize_options)
  callback = protect_consumer(callback or function() end, "Harness startup")
  local client = state()
  if client.harness_ready and client.process then
    local generation = client.generation
    vim.schedule(function()
      if generation ~= client.generation or not client.harness_ready or not client.process then
        callback(nil, "Forge host generation changed during Harness startup")
      else callback(client.snapshot or {}, nil) end
    end)
    return
  end
  if client.harness_starting then
    if #client.harness_initialize_callback >= 64 then callback(nil, "Harness startup admission is full") return end
    client.harness_initialize_callback[#client.harness_initialize_callback + 1] = callback
    return
  end
  client.harness_starting = true
  M.start(function(_, failure, detail)
    if failure then finish_harness_start(callback, nil, failure, detail) return end
    send_harness_initialize(callback, initialize_options)
  end)
end

---@param method string
---@param params table
---@param callback ForgeCallback
---@param progress? fun(payload: table)
function M.request_host(method, params, callback, progress)
  callback = protect_consumer(callback, method)
  local id = admit_request(method, callback)
  if not id then return end
  state().pending[id].progress = progress
  M.start(function(_, failure)
    local client = state()
    if not client.pending[id] then return end
    if failure then
      client.pending[id] = nil
      callback(nil, failure)
      return
    end
    local generation = client.generation
    vim.schedule(function()
      local current = state()
      if current.generation ~= generation or not current.pending[id] then return end
      write_request(id, method, params)
    end)
  end)
end

---@param action string
---@param conflict { session_id: string }
---@param callback? ForgeCallback
function M.resolve_lease_conflict(action, conflict, callback)
  callback = callback or function() end
  if state().harness_ready then
    if action == "new" then
      M.request("session.new", {}, callback)
    elseif action == "retry" then
      M.request("session.resume", { session_id = conflict.session_id }, callback)
    elseif action == "fork" then
      M.request("session.fork", { session_id = conflict.session_id }, callback)
    else callback(nil, "Unknown Harness lease recovery action") end
    return
  end
  local initialize_options = action == "retry" and nil or { lease_conflict_action = "new" }
  M.start_harness(function(result, start_error, error_detail)
    if start_error or action ~= "fork" then callback(result, start_error, error_detail) return end
    M.request("session.fork", { session_id = conflict.session_id }, callback)
  end, initialize_options)
end

---@param method string
---@param params? table
---@param callback? fun(result?: any, error?: string, error_detail?: table)
function M.request(method, params, callback)
  local active_session_id = session.harness.session and session.harness.session.id or nil
  M.request_for(active_session_id, method, params, callback)
end

---@param request_state ForgeHarnessPresentationState
---@param callback fun(result?: any, error?: string, error_detail?: table)
---@return fun(result?: any, error?: string, error_detail?: table)
local function scope_callback(request_state, callback)
  local deliver = protect_consumer(callback, "session request")
  return function(result, request_error, error_detail)
    local active_state = session.harness
    session.activate_harness(request_state)
    deliver(result, request_error, error_detail)
    if active_state ~= request_state then session.activate_harness(active_state) end
  end
end

---@param session_id? string
---@param name? string
---@param callback? fun(result?: any, error?: string, error_detail?: table)
function M.create_session(session_id, name, callback)
  callback = callback or function() end
  local client = state()
  if client.harness_ready then
    M.request_for(session_id, "session.new", { name = name }, callback)
    return
  end
  M.start_harness(scope_callback(session.harness, callback), { new_session_name = name or "" })
end

---@param session_id? string
---@param method string
---@param params? table
---@param callback? fun(result?: any, error?: string, error_detail?: table)
function M.request_for(session_id, method, params, callback)
  callback = callback or function() end
  local session_callback = scope_callback(session.harness, callback)
  local id = admit_request(method, session_callback)
  if not id then return end
  M.start_harness(function(_, start_error)
    local client = state()
    if not client.pending[id] then return end
    if start_error then
      client.pending[id] = nil
      session_callback(nil, start_error)
      return
    end
    write_request(id, method, params or {}, session_id)
  end)
end

---@param callback ForgeSubscriber
---@return fun()
function M.subscribe(callback)
  local client = state()
  client.next_subscriber = client.next_subscriber + 1
  local subscriber_id = client.next_subscriber
  client.subscriber[subscriber_id] = callback
  return function() client.subscriber[subscriber_id] = nil end
end

---@param document string
---@param callback fun(event: string, update: table, generation: integer)
---@return fun()
function M.subscribe_document(document, callback)
  local client = state()
  client.next_subscriber = client.next_subscriber + 1
  local subscriber_id = client.next_subscriber
  client.document_subscriber[subscriber_id] = { document = document, callback = protect_consumer(callback, "document event") }
  return function() client.document_subscriber[subscriber_id] = nil end
end

function M.stop(reason)
  reason = reason or "Forge host stopped"
  local client = state()
  local process = client.process
  if client.draining then return end
  client.stop_reason = reason
  client.ready = false
  client.harness_ready = false
  session.harness.ready = false
  if process then
    client.draining = true
    local generation = client.generation
    local launch_token = client.launch_token
    local function still_running()
      return M._client == client
        and client.generation == generation
        and client.launch_token == launch_token
        and client.process == process
    end
    local sent, failure = pcall(process.write, process, protocol.encode_request(0, "shutdown", {}))
    if not sent then notifications.error("Forge shutdown request failed: " .. tostring(failure), "Forge") end
    local shutdown_timer = vim.uv.new_timer()
    client.shutdown_timer = shutdown_timer
    client.shutdown_timer_token = launch_token
    shutdown_timer:start(3000, 0, vim.schedule_wrap(function()
      if client.shutdown_timer == shutdown_timer then
        client.shutdown_timer = nil
        client.shutdown_timer_token = nil
      end
      if not shutdown_timer:is_closing() then
        shutdown_timer:stop()
        shutdown_timer:close()
      end
      if not still_running() then return end
      notifications.error("Forge drain deadline exceeded. Unresolved requests retain unknown outcomes until process collection.", "Forge")
      pcall(process.kill, process, 15)
      force_collect_process(client, process, still_running, function(collected)
        if not still_running() then return end
        if collected and client.receiver then receive.finish(client.receiver, collected) end
      end)
    end))
    return
  end
  client.launch_token = client.launch_token + 1
  client.generation = client.generation + 1
  if client.receiver then receive.close(client.receiver) end
  client.process = nil
  client.ready = false
  client.harness_ready = false
  client.snapshot = nil
  client.starting = false
  session.harness.ready = false
  local pending_list = client.pending
  client.pending = {}
  client.transfer = {}
  client.transfer_bytes = 0
  for _, pending in pairs(pending_list) do pending.callback(nil, reason) end
  if client.start_callback then finish_start(client.start_callback, nil, reason) end
  local callback_list = client.initialize_callback
  client.initialize_callback = {}
  for _, callback in ipairs(callback_list) do callback(nil, reason) end
end

function M.host_generation()
  return state().generation
end


function M.host_accepting()
  local client = state()
  return client.process ~= nil and not client.draining
end

function M._set_launcher_for_test(launcher) launcher_for_test = launcher end
function M._reset_for_test()
  local client = M._client
  M.stop()
  if client then cancel_shutdown_timer(client, client.launch_token) end
  M._client = nil
  launcher_for_test = nil
  shutdown_autocmd = false
end

return M
