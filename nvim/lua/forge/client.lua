local M = {}

local builder = require("forge.builder")
local backend_preference = require("forge.harness.backend_preference")
local config = require("forge.infra.config")
local notifications = require("forge.infra.notifications")
local protocol = require("forge.protocol")
local contract = require("forge.event_contract")
local session = require("forge.session")
local receive = require("forge.receive")
local json_transfer = require("forge.json_transfer")
local perf = require("forge.infra.perf")

local launcher_for_test = nil
local shutdown_autocmd = false
local exiting = false
local host_drain_timeout_ms = 3000
local host_unix_collection_timeout_ms = 1000
local host_windows_collection_timeout_ms = 2000
local send_request

---@alias ForgeCallback fun(result?: any, error?: string, error_detail?: table)
---@alias ForgeSubscriber fun(event: string, payload: any, session_id?: string)

---@class ForgeBackgroundSelection
---@field model string
---@field reasoning_effort? string
---@field service_tier? 'default'|'fast'|'ultrafast'
---@field auto_compaction? {enabled: boolean, threshold_tokens: integer}

---@class ForgeTextGenerationConfig
---@field backend {kind: string, command: string[]}
---@field model string
---@field reasoning_effort? string
---@field service_tier? 'default'|'fast'|'ultrafast'
---@field auto_compaction? {enabled: boolean, threshold_tokens: integer}
---@field data_root string

---@class ForgePendingRequest
---@field method string
---@field callback ForgeCallback
---@field progress? fun(payload: table)
---@field session_id? string
---@field timing? table
---@field dispatched? boolean

---@class ForgeClient
---@field process table?
---@field generation integer
---@field launch_token integer
---@field next_id integer
---@field pending table<integer, ForgePendingRequest>
---@field transfer table<integer, ForgeJsonTransfer>
---@field transfer_bytes integer
---@field subscriber table<integer, ForgeSubscriber>
---@field document_subscriber table<integer, {document: string, callback: fun(event: string, update: table, generation: integer)}>
---@field completed_status_operation table<string, boolean>
---@field completed_status_order string[]
---@field next_subscriber integer
---@field receiver table?
---@field shutdown_timer uv.uv_timer_t?
---@field shutdown_timer_token integer?
---@field taskkill_process table?
---@field stderr string
---@field ready boolean
---@field starting boolean
---@field draining boolean
---@field stop_reason string?
---@field stop_callback (fun())[]
---@field start_callback ForgeCallback?
---@field start_started? integer
---@field initialize_callback ForgeCallback[]
---@field harness_initialize_callback ForgeCallback[]
---@field harness_ready boolean
---@field harness_starting boolean
---@field runtime_ready boolean
---@field runtime_starting boolean
---@field runtime_initialize_callback ForgeCallback[]
---@field snapshot_by_id table<string, table>
---@field snapshot table?

---@type ForgeClient?
M._client = nil

---@return ForgeClient
local function host_state()
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
    stop_callback = {},
    initialize_callback = {},
    snapshot = nil,
    snapshot_by_id = {},
    harness_ready = false,
    harness_starting = false,
    harness_initialize_callback = {},
    runtime_ready = false,
    runtime_starting = false,
    runtime_initialize_callback = {},
  }
  return M._client
end

local function protect_consumer(callback, context)
  return function(...)
    local succeeded, failure = pcall(callback, ...)
    if not succeeded then
      local diagnostic = "Forge " .. context .. " callback failed: " .. tostring(failure)
      require("forge.startup_log").write("host.callback.failed", { context = context, error = tostring(failure) })
      if not pcall(notifications.error, diagnostic, "Forge") then pcall(vim.api.nvim_echo, { { diagnostic, "ErrorMsg" } }, true, { err = true }) end
    end
  end
end

---@param method string
---@param callback ForgeCallback
---@return integer?
local function host_admit_request(method, callback)
  local client = host_state()
  if exiting then callback(nil, "Forge host is exiting", { code = "not_admitted" }) return nil end
  if client.draining then callback(nil, "Forge host is draining", { code = "not_admitted" }) return nil end
  local count, ordinary = 0, 0
  for _, pending in pairs(client.pending) do
    count = count + 1
    if contract.method_policy(pending.method).admission ~= "control" then ordinary = ordinary + 1 end
  end
  if count >= 64 or (contract.method_policy(method).admission ~= "control" and ordinary >= 60) or client.next_id >= 9007199254740991 then
    callback(nil, "Forge request admission is full", { code = "not_admitted" })
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
local function host_write_request(id, method, params, session_id)
  require("forge.startup_log").write("host.request.send", { id = id, method = method, operation = params and params.operation,
    document = params and params.document })
  local client = host_state()
  if not client.pending[id] then return end
  local ok, failure = pcall(function()
    local encoded = protocol.encode_request(id, method, params, session_id)
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
local function session_backend_descriptor()
  local harness_config = config.options.harness
  local backend = harness_config.backend
  local configured = harness_config.backends[backend]
  assert(configured, "Unknown Harness backend: " .. tostring(backend))
  return {
    kind = backend,
    command = vim.deepcopy(configured.command or {}),
    permission_note = "Forge owns approval rules. Provider capabilities describe isolation support.",
  }
end

---@param callback ForgeCallback
---@param result? any
---@param start_error? string
---@param error_detail? table
local function host_finish_start(callback, result, start_error, error_detail)
  local client = host_state()
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
local function session_finish_initialize(mode, callback, result, start_error, error_detail)
  local client = host_state()
  client[mode .. "_starting"] = false
  local queued = client[mode .. "_initialize_callback"]
  client[mode .. "_initialize_callback"] = {}
  callback(result, start_error, error_detail)
  for _, waiting in ipairs(queued) do waiting(result, start_error, error_detail) end
end

local function session_fail_initialization(client, failure)
  for _, mode in ipairs({ "runtime", "harness" }) do
    client[mode .. "_ready"] = false
    client[mode .. "_starting"] = false
    local waiting = client[mode .. "_initialize_callback"]
    client[mode .. "_initialize_callback"] = {}
    for _, callback in ipairs(waiting) do callback(nil, failure, { code = "outcome_unknown" }) end
  end
end

---Keep the event loop alive until normal shutdown collects the host, without admitting replacement work.
local function host_stop_for_exit()
  exiting = true
  local stopped = false
  M.host_stop(nil, function() stopped = true end)
  if stopped then return end
  local collection_timeout_ms = vim.fn.has("win32") == 1
    and host_windows_collection_timeout_ms or host_unix_collection_timeout_ms
  if not vim.wait(host_drain_timeout_ms + collection_timeout_ms, function() return stopped end) then
    notifications.error("Forge exit exceeded its drain and process collection deadlines; shutdown completion is unknown", "Forge")
  end
end

local function host_ensure_shutdown_autocmd()
  if shutdown_autocmd then return end
  shutdown_autocmd = true
  local group = vim.api.nvim_create_augroup("ForgeClient", { clear = true })
  vim.api.nvim_create_autocmd("VimLeavePre", {
    group = group,
    callback = host_stop_for_exit,
  })
end

---@param client ForgeClient
---@param message table
---@return table? response
---@return string? failure
local function host_receive_transfer(client, message)
  local document = message.document
  local session_id = message.session_id
  local envelope = message.payload
  if session_id and (type(envelope) ~= "table" or type(envelope.transfer_id) ~= "string") then
    return nil, "invalid event transfer identity"
  end
  local request_id = session_id and ("event:" .. session_id .. ":" .. envelope.transfer_id)
    or document and ("document:" .. document) or message.request_id
  local part = session_id and envelope.part or envelope
  local transfer = client.transfer[request_id]
  if message.event == "result.part" or message.event == "document.part" or message.event == "event.part" then
    if not json_transfer.valid(part) then return nil, "invalid result part" end
    for key in pairs(part) do
      if key ~= "sequence" and key ~= "part_count" and key ~= "total_bytes" and key ~= "payload" then
        return nil, "unexpected result part field"
      end
    end
    if not transfer then
      local active = 0
      for _ in pairs(client.transfer) do active = active + 1 end
      if part.sequence ~= 0 or active >= 2 then
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
  local response, decode_failure = protocol.decode_message(encoded)
  require("forge.startup_log").write("host.result.decoded", { id = request_id, bytes = #encoded,
    parts = transfer.part_count, accept_us = transfer.accept_us, assembly_us = assembly_us,
    decode_us = math.floor((vim.uv.hrtime() - decode_started) / 1000),
    transfer_elapsed_us = math.floor((vim.uv.hrtime() - transfer.started_at) / 1000) })
  if not response then return nil, decode_failure end
  if document then
    if not response or response.document ~= document or response.event ~= "status.update" then
      return nil, decode_failure or "invalid assembled document update"
    end
    return response, nil
  end
  if session_id then
    if not response or response.session_id ~= session_id
      or type(response.event) ~= "string" or response.event == "event.part" or response.event == "event.complete"
      or response.payload == nil then return nil, "invalid assembled session event" end
    return response, nil
  end
  if not response or response.id ~= request_id then return nil, decode_failure or "invalid assembled result identity" end
  return response, nil
end

---@param client ForgeClient
---@param process table
---@param still_running fun(): boolean
---@param finished fun(result: table)
local function host_collect_process(client, process, still_running, finished)
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
    end, host_unix_collection_timeout_ms)
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
    timeout = host_windows_collection_timeout_ms,
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
local function host_cancel_shutdown_timer(client, launch_token)
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
local function host_dispatch_message(message, timing)
  local client = host_state()
  if message.event == "event.part" or message.event == "event.complete" then
    local assembled, failure = host_receive_transfer(client, message)
    if failure then error("Forge event transfer failed: " .. failure) end
    if not assembled then return end
    message = assembled
  end
  if message.document ~= nil then
    if message.event == "document.part" or message.event == "document.complete" then
      local assembled, failure = host_receive_transfer(client, message)
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
        perf.trace("harness", "ui.document.event", { event_type = message.event,
          generation = client.generation }, function()
          subscriber.callback(message.event, update, client.generation)
        end)
      end
    end
    return
  end
  local measured = client.pending[message.request_id or message.id]
  if measured and timing then
    measured.timing = measured.timing or { frames = 0, bytes = 0, decode_us = 0, maximum_queue_wait_us = 0,
      first_frame_us = math.floor(timing.received_at / 1000) }
    local aggregate = assert(measured.timing)
    aggregate.frames = aggregate.frames + 1
    aggregate.bytes = aggregate.bytes + timing.bytes
    aggregate.decode_us = aggregate.decode_us + timing.decode_us
    aggregate.maximum_queue_wait_us = math.max(aggregate.maximum_queue_wait_us, timing.queue_wait_us)
  end
  if message.request_id then
    local pending = client.pending[message.request_id]
    if pending and (message.event == "result.part" or message.event == "result.complete") then
      local result, failure = host_receive_transfer(client, message)
      if failure then error("Forge result transfer failed: " .. failure) end
      if not result then return end
      message = result
    else
      if pending and pending.progress and message.event == pending.method .. ".progress" then
        local ok, failure = pcall(pending.progress, message.payload)
        if not ok then
          local diagnostic = "Forge request progress callback failed: " .. tostring(failure)
          if not pcall(notifications.error, diagnostic, "Forge") then pcall(vim.api.nvim_echo, { { diagnostic, "ErrorMsg" } }, true, { err = true }) end
        end
      end
      return
    end
  end
  if message.event == "lsp.request" or message.event == "lsp.cancel" then
    require("forge.lsp.bridge").handle(message.event, message.payload, message.session_id, client.generation)
    return
  end
  if message.event == "background_error" then
    notifications.error(message.payload and message.payload.error or "Background operation failed", "Forge Background")
    return
  end
  if message.event then
    local event_snapshot = client.snapshot_by_id[message.session_id]
    if event_snapshot and (message.event == "plan_question" or message.event == "plan_question_updated") then
      local plan = message.payload and message.payload.plan
        event_snapshot.active_plan = plan and vim.deepcopy(plan) or nil
    elseif event_snapshot and (message.event == "question" or message.event == "question_updated") then
      event_snapshot.active_elicitation = vim.deepcopy(message.payload)
    elseif event_snapshot and message.event == "question_answered" then
      event_snapshot.active_elicitation = nil
    end
    for _, subscriber in pairs(client.subscriber) do
      local ok, err = pcall(perf.trace, "harness", "ui.session.event", {
        event_type = message.event, session_id = message.session_id }, function()
        subscriber(message.event, message.payload, message.session_id)
      end)
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
  else
    if pending.method == "session.prompt.history.record" then
      local snapshot = pending.session_id and client.snapshot_by_id[pending.session_id]
      if snapshot then snapshot.prompt_history = vim.deepcopy(message.result or {}) end
    end
    local result_session_id = message.result and message.result.session and message.result.session.id
    if result_session_id then client.snapshot_by_id[result_session_id] = message.result end
    pending.callback(message.result, nil)
  end
end

---@param chunk string?
---@param generation integer
local function host_consume_stdout(chunk, generation)
  local client = host_state()
  if generation ~= client.generation or not chunk or not client.receiver then return end
  receive.push(client.receiver, chunk)
end

---@param callback fun(result?: any, error?: string, error_detail?: table)
local function session_send_initialize(callback, initialize_options, startup)
  local client = host_state()
  local mode = initialize_options and initialize_options.services_only and "runtime" or "harness"
  local harness_config = config.options.harness
  if not config.harness_backend_explicit and not session.harness.session then
    harness_config.backend = backend_preference.load(harness_config.backends, harness_config.backend)
  end
  local descriptor = session_backend_descriptor()
  local function initialized(result, request_error, error_detail)
    if startup then startup.mark("harness.initialize.received", { error = request_error,
      session_id = result and result.session and result.session.id,
      rust = result and result.startup_timing }) end
    if request_error then
      client[mode .. "_ready"] = false
      session_finish_initialize(mode, callback, nil, request_error, error_detail)
      return
    end
    client.runtime_ready = true
    if mode == "harness" then client.snapshot = result end
    local initialized_session_id = result and result.session and result.session.id
    if initialized_session_id then client.snapshot_by_id[initialized_session_id] = result end
    client[mode .. "_ready"] = true
    if mode == "harness" then session.harness.ready = true end
    send_request(nil, "host.trace.configure", { enabled = config.options.harness_logging == true, default_only = true }, function(status, failure)
      if startup then startup.mark("trace.configure.received", { error = failure }) end
      if failure then
        client[mode .. "_ready"] = false
        if mode == "harness" then session.harness.ready = false end
        session_finish_initialize(mode, callback, nil, failure)
        return
      end
      if mode == "harness" then require("forge.views.harness.settings").adopt(status) end
      session_finish_initialize(mode, callback, result, nil)
    end)
  end
  local backend_launch_by_provider = {}
  for provider, backend in pairs(harness_config.backends) do
    backend_launch_by_provider[provider] = { kind = provider, command = vim.deepcopy(backend.command or {}) }
  end
  local params = {
    data_root = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"),
    config_dir = harness_config.config_dir or vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(
      vim.uv.fs_realpath(debug.getinfo(1, "S").source:sub(2)) or debug.getinfo(1, "S").source:sub(2))))),
    permission_file = vim.fs.joinpath(vim.fn.stdpath("config"), "forge", "permissions.json"),
    workspace = vim.fn.getcwd(),
    client_id = ("nvim-%s-%s"):format(vim.fn.getpid(), vim.uv.hrtime()),
    backend = { kind = descriptor.kind, command = descriptor.command },
    backend_launch_by_provider = backend_launch_by_provider,
    model = harness_config.model,
    effort = harness_config.effort,
    goal_max_turns = harness_config.goal_max_turns,
    lease_conflict_action = initialize_options and initialize_options.lease_conflict_action or nil,
    session_id = initialize_options and initialize_options.session_id or nil,
    new_session_name = initialize_options and initialize_options.new_session_name or nil,
    services_only = mode == "runtime",
  }
  if startup then startup.mark("harness.initialize.requested") end
  send_request(nil, "session.initialize", params, initialized)

end

---@param callback ForgeCallback
local function host_send_initialize(callback)
  local client = host_state()
  client.next_id = client.next_id + 1
  local id = client.next_id
  client.pending[id] = { method = "host.initialize", callback = function(result, failure, detail)
    if failure then host_finish_start(callback, nil, failure, detail) return end
    if type(result) ~= "table" or result.protocol_version ~= protocol.VERSION then
      local mismatch = "Forge client and host wire versions differ. Rebuild Forge and restart the client."
      host_finish_start(callback, nil, mismatch)
      M.host_stop(mismatch)
      return
    end
    if type(result.git_config_cache) == "table" then
      require("forge.startup_log").write("host.git_config_cache", result.git_config_cache)
      if result.git_config_cache.warning then notifications.error(result.git_config_cache.warning, "Forge") end
    end
    client.ready = not client.draining
    host_finish_start(callback, { generation = client.generation }, nil)
  end }
  local ok, failure = pcall(client.process.write, client.process, protocol.encode_request(id, "host.initialize", {
    protocol_version = protocol.VERSION,
    recovery_directory = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "recovery", "github", "v1"),
    status_ignored_directory = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "status-ignored"),
    git_config_cache_path = require("forge.git_config_cache").path(),
  }))
  if not ok then
    client.pending[id] = nil
    host_finish_start(callback, nil, "Failed to initialize Forge host: " .. tostring(failure))
  end
end

---@param binary string
---@param callback fun(result?: any, error?: string, error_detail?: table)
---@param launch_token integer
---@param lease? RustSidecarLease
local function host_spawn(binary, callback, launch_token, lease)
  local client = host_state()
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
  local spawn_started
  local first_stdout = false
  local options = {
    text = true,
    stdin = true,
    stdout = function(_, data)
      if data and not first_stdout then
        first_stdout = true
        require("forge.startup_log").write("host.first_stdout", {
          generation = generation, elapsed_ms = (vim.uv.hrtime() - spawn_started) / 1e6, bytes = #data })
      end
      host_consume_stdout(data, generation)
    end,
    stderr = function(_, data)
      if data then require("forge.startup_log").write("host.stderr", { text = data:sub(-8192), generation = generation }) end
      if data and generation == host_state().generation then client.stderr = (client.stderr .. data:sub(-65536)):sub(-65536) end
    end,
  }
  local receiver
  local finish_exit = function(result)
    if lease then
      local released, failure = pcall(lease.release)
      if not released then notifications.error(tostring(failure), "Forge") end
      lease = nil
    end
    local current = host_state()
    if generation ~= current.generation or launch_token ~= current.launch_token then return end
    host_cancel_shutdown_timer(current, launch_token)
    current.process = nil
    if current.receiver == receiver then current.receiver = nil end
    current.ready = false
    current.harness_ready = false
    current.runtime_ready = false
    current.snapshot = nil
    current.starting = false
    current.draining = true
    session.harness.ready = false
    local stop_reason = current.stop_reason or ("Forge host stopped (exit " .. tostring(result.code) .. ")")
    for _, subscriber in pairs(current.subscriber) do
      local ok, failure = pcall(subscriber, "host_stopped", { message = stop_reason, expected = current.stop_reason ~= nil })
      if not ok then notifications.error("Harness stop handler failed: " .. tostring(failure), "ForgeHarness") end
    end
    if result.code ~= 0 and not current.stop_reason then
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
    session_fail_initialization(current, stop_reason)
    current.draining = false
    current.stop_reason = nil
    local stopped = current.stop_callback or {}
    current.stop_callback = {}
    for _, finished in ipairs(stopped) do finished() end
  end
  local consumed_bytes, consumed_frames = 0, 0
  receiver = receive.new({
    active = function() return generation == host_state().generation end,
    decode = protocol.decode_message,
    dispatch = host_dispatch_message,
    consumed = function(bytes, frames)
      consumed_bytes, consumed_frames = consumed_bytes + bytes, consumed_frames + frames
      if generation ~= host_state().generation or not client.process then return end
      client.process:write(protocol.encode_request(0, "host.transport.consumed", {
        bytes = consumed_bytes, frames = consumed_frames,
      }))
    end,
    failed = function(message)
      M.host_stop()
      if not pcall(notifications.error, message, "ForgeHarness") then pcall(vim.api.nvim_echo, { { message, "ErrorMsg" } }, true, { err = true }) end
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
  spawn_started = vim.uv.hrtime()
  local ok, process_or_error = pcall(process_launcher, command, options, on_exit)
  if not ok then
    if lease then pcall(lease.release) end
    host_finish_start(callback, nil, "Failed to start Harness broker: " .. tostring(process_or_error))
    return
  end
  process = process_or_error
  client.process = process
  require("forge.startup_log").write("host.spawned", { pid = process.pid, generation = generation,
    elapsed_ms = math.floor((vim.uv.hrtime() - spawn_started) / 1e6) })
  host_send_initialize(callback)
end

---@param binary string
---@param callback fun(result?: any, error?: string, error_detail?: table)
---@param launch_token integer
local function host_launch(binary, callback, launch_token)
  if launcher_for_test then host_spawn(binary, callback, launch_token) return end
  local client = host_state()
  local lease_started = vim.uv.hrtime()
  builder.acquire(binary, function(lease, failure)
    require("forge.startup_log").write("host.lease.finished", { error = failure,
      elapsed_ms = math.floor((vim.uv.hrtime() - lease_started) / 1e6),
      source = lease and lease.source_path, binary = lease and lease.path,
      cache_hit = lease and lease.cache_hit, cache_key = lease and lease.cache_key,
      file_bytes = lease and lease.file_bytes, copy_ms = lease and lease.copy_ms,
      pin_ms = lease and lease.pin_ms, lookup_ms = lease and lease.lookup_ms })
    if M._client ~= client or client.launch_token ~= launch_token or client.start_callback ~= callback then
      if lease then
        local released, release_error = pcall(lease.release)
        if not released then notifications.error(tostring(release_error), "Forge") end
      end
      return
    end
    if not lease then host_finish_start(callback, nil, failure) return end
    host_spawn(lease.path, callback, launch_token, lease)
  end)
end

---@param callback fun(result?: any, error?: string, error_detail?: table)
local function host_start_process(callback)
  require("forge.startup_log").write("host.start")
  local host_started = vim.uv.hrtime()
  local client = host_state()
  client.start_started = host_started
  client.starting = true
  client.start_callback = callback
  client.launch_token = client.launch_token + 1
  local launch_token = client.launch_token
  builder.ensure(function(executable_result)
    require("forge.startup_log").write("host.binary.result", { ok = executable_result.ok, path = executable_result.path,
      error = executable_result.message, elapsed_ms = math.floor((vim.uv.hrtime() - host_started) / 1e6) })
    local function launch_ready()
      if M._client ~= client or client.launch_token ~= launch_token or client.start_callback ~= callback then return end
      if not executable_result.ok or not executable_result.path then
        host_finish_start(callback, nil, executable_result.message or "Forge executable is unavailable")
        return
      end
      host_launch(executable_result.path, callback, launch_token)
    end
    if vim.in_fast_event() then vim.schedule(launch_ready) else launch_ready() end
  end)
end

---@param callback? fun(result?: any, error?: string, error_detail?: table)
function M.host_start(callback)
  callback = protect_consumer(callback or function() end, "startup")
  local client = host_state()
  if exiting then callback(nil, "Forge host is exiting", { code = "not_admitted" }) return end
  host_ensure_shutdown_autocmd()
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
  host_start_process(callback)
end

---@param callback? ForgeCallback
---@param initialize_options? table
---@param startup? ForgeHarnessStartupTrace
function M.session_initialize(callback, initialize_options, startup)
  callback = protect_consumer(callback or function() end, "Harness startup")
  local client = host_state()
  local mode = initialize_options and initialize_options.services_only and "runtime" or "harness"
  if client[mode .. "_ready"] and client.process then
    if mode == "harness" and initialize_options then
      if initialize_options.new_session_name ~= nil or initialize_options.lease_conflict_action == "new" then
        M.session_create({ name = initialize_options.new_session_name }, callback)
        return
      end
      local target = initialize_options.session_id
      if target then
        M.request({ method = "session.resume", session_id = target, params = { session_id = target } }, callback)
        return
      end
    end
    if startup then startup.mark("host.reused") end
    local generation = client.generation
    vim.schedule(function()
      if generation ~= client.generation or not client[mode .. "_ready"] or not client.process then
        callback(nil, "Forge host generation changed during Harness startup", { code = "not_admitted" })
      else callback(mode == "runtime" and { initialized = true } or client.snapshot or {}, nil) end
    end)
    return
  end
  if client[mode .. "_starting"] then
    if #client[mode .. "_initialize_callback"] >= 64 then callback(nil, "Harness startup admission is full") return end
    client[mode .. "_initialize_callback"][#client[mode .. "_initialize_callback"] + 1] = function(result, failure, detail)
      if failure or mode == "runtime" or not initialize_options then
        callback(result, failure, detail)
      else
        M.session_initialize(callback, initialize_options, startup)
      end
    end
    return
  end
  client[mode .. "_starting"] = true
  if startup then startup.mark("host.requested") end
  M.host_start(function(_, failure, detail)
    if startup then startup.mark("host.ready", { error = failure }) end
    if failure then session_finish_initialize(mode, callback, nil, failure, detail) return end
    session_send_initialize(callback, initialize_options, startup)
  end)
end

--- Resolve isolated generation launch settings without starting a provider.
---@param selection? ForgeBackgroundSelection
---@return ForgeTextGenerationConfig? generation
---@return string? failure
function M.generation_get_config(selection)
  local harness_config = config.options.harness
  local selected = session.harness.session
  local backend = selected and selected.backend or harness_config.backend
  if not selected and not config.harness_backend_explicit then
    backend = backend_preference.load(harness_config.backends, backend)
  end
  local configured = harness_config.backends[backend]
  if not configured then return nil, "Unknown Harness backend: " .. tostring(backend) end
  selection = selection or (config.options.background_generation or {})[backend]
  if selection and (type(selection) ~= "table" or type(selection.model) ~= "string" or vim.trim(selection.model) == "") then
    return nil, "Invalid Background-generation model for backend: " .. backend
  end
  ---@type ForgeTextGenerationConfig
  local generation = {
    backend = { kind = backend, command = vim.deepcopy(configured.command or {}) },
    model = selection and selection.model or (selected and selected.model or harness_config.model),
    reasoning_effort = selection and selection.reasoning_effort,
    service_tier = selection and selection.service_tier or "default",
    auto_compaction = selection and selection.auto_compaction,
    data_root = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"),
  }
  return generation
end

---@param params table
---@param callback ForgeCallback
---@param on_model? fun(model: string) Receives the configured generation model before request dispatch.
---@param selection? ForgeBackgroundSelection
---@return fun() cancel Cancels this generation without stopping the shared host.
function M.generation_request(params, callback, on_model, selection)
  local generation, failure = M.generation_get_config(selection)
  if not generation then callback(nil, failure) return function() end end
  if on_model then
    local effort = generation.reasoning_effort and (" · thinking " .. generation.reasoning_effort) or ""
    on_model(generation.model .. effort .. " (" .. generation.backend.kind .. ")")
  end
  local label = generation.model .. (generation.reasoning_effort and (" · thinking " .. generation.reasoning_effort) or "")
  local owner, finished = host_state(), false
  local request_id = M.request({ method = "repository.generation", params = vim.tbl_extend("force", params, { generation = generation }) }, function(...)
    finished = true
    callback(...)
  end, function(progress)
    if finished then return end
    local event = progress.event
    if not on_model or type(event) ~= "table" then return end
    if event.kind == "runtime_resolved" and event.data and event.data.model then
      label = event.data.model .. (generation.reasoning_effort and (" · thinking " .. generation.reasoning_effort) or "")
      on_model(label)
    elseif event.kind == "reasoning_summary" and type(event.text) == "string" then
      on_model(label .. "\n" .. event.text)
    elseif event.kind == "status" and event.data and type(event.data.reasoning_summary) == "string" then
      on_model(label .. "\n" .. event.data.reasoning_summary)
    end
  end)
  return function()
    if finished then return end
    finished = true
    if host_state() ~= owner or not request_id then return end
    local pending = owner.pending[request_id]
    if not pending then return end
    if not pending.dispatched then
      owner.pending[request_id] = nil
      callback(nil, "Background generation cancelled")
      return
    end
    M.request({ method = "repository.generation", params = { operation = "cancel", request_id = request_id } }, function(_, cancel_failure)
      if cancel_failure then notifications.error("Could not cancel Background generation: " .. cancel_failure) end
    end)
  end
end

---@param action string
---@param conflict { session_id: string }
---@param callback? ForgeCallback
function M.session_resolve_lease_conflict(action, conflict, callback)
  callback = callback or function() end
  if host_state().harness_ready then
    if action == "new" then
      M.request({ method = "session.new", params = {} }, callback)
    elseif action == "retry" then
      M.request({ method = "session.resume", session_id = conflict.session_id, params = { session_id = conflict.session_id } }, callback)
    elseif action == "fork" then
      M.request({ method = "session.fork", session_id = conflict.session_id, params = { session_id = conflict.session_id } }, callback)
    else callback(nil, "Unknown Harness lease recovery action") end
    return
  end
  local initialize_options = action ~= "retry" and { lease_conflict_action = "new" }
    or { session_id = conflict.session_id }
  M.session_initialize(function(result, start_error, error_detail)
    if start_error or action ~= "fork" then callback(result, start_error, error_detail) return end
    M.request({ method = "session.fork", session_id = conflict.session_id, params = { session_id = conflict.session_id } }, callback)
  end, initialize_options)
end

---@param request_state ForgeHarnessPresentationState
---@param callback fun(result?: any, error?: string, error_detail?: table)
---@return fun(result?: any, error?: string, error_detail?: table)
local function session_scope_callback(request_state, callback)
  local deliver = protect_consumer(callback, "session request")
  return function(result, request_error, error_detail)
    local active_state = session.harness
    session.activate_harness(request_state)
    deliver(result, request_error, error_detail)
    if active_state ~= request_state then session.activate_harness(active_state) end
  end
end

---@param options? {source_session_id?: string, name?: string}
---@param callback? fun(result?: any, error?: string, error_detail?: table)
function M.session_create(options, callback)
  options = options or {}
  M.request({ method = "session.new", params = {
    name = options.name, source_session_id = options.source_session_id,
  } }, callback)
end

---@param session_id? string
---@param method string
---@param params? table
---@param callback? fun(result?: any, error?: string, error_detail?: table)
send_request = function(session_id, method, params, callback, sending, progress)
  local owner = session_id and session.harness_by_id[session_id]
  callback = callback or function() end
  local receive = owner and session_scope_callback(owner, callback) or protect_consumer(callback, method)
  local id = host_admit_request(method, receive)
  if not id then return end
  host_state().pending[id].progress = progress
  host_state().pending[id].session_id = session_id
  local function ready(_, failure, detail)
    local client = host_state()
    if not client.pending[id] then return end
    if failure then
      client.pending[id] = nil
      receive(nil, failure, detail)
      return
    end
    local generation = client.generation
    vim.schedule(function()
      if host_state() ~= client or client.generation ~= generation or not client.pending[id] then return end
      if sending then sending(generation) end
      client.pending[id].dispatched = true
      host_write_request(id, method, params or {}, session_id)
    end)
  end
  if method == "session.initialize" or contract.method_policy(method).readiness == "host" then
    M.host_start(ready)
  else
    M.session_initialize(ready, { services_only = true })
  end
  return id
end

---@class ForgeRequestSpec
---@field method string Exact method from the shared contract.
---@field params? table Method-owned parameters.
---@field session_id? string Explicit target required by session operations.

---Dispatch one request while preserving its target and transport correlation.
---@param spec ForgeRequestSpec
---@param callback? ForgeCallback
---@param progress? fun(payload: table)
---@return integer? request_id Transport identity for requests without command recovery.
function M.request(spec, callback, progress)
  callback = callback or function() end
  local valid, policy = pcall(contract.validate_request, spec)
  if not valid then
    protect_consumer(callback, "request validation")(nil, tostring(policy), { code = "invalid_request" })
    return
  end
  local function transport(session_id, method, params, receive, sending)
    return send_request(session_id, method, params, receive, sending, progress)
  end
  if policy.recovery == "session_command" and spec.session_id then
    require("forge.command").dispatch(transport, spec.session_id, spec.method, spec.params, callback)
  else
    return transport(spec.session_id, spec.method, spec.params, callback)
  end
end

---@param callback ForgeSubscriber
---@return fun()
function M.session_events_subscribe(callback)
  local client = host_state()
  client.next_subscriber = client.next_subscriber + 1
  local subscriber_id = client.next_subscriber
  client.subscriber[subscriber_id] = callback
  return function() client.subscriber[subscriber_id] = nil end
end

---@param document string
---@param callback fun(event: string, update: table, generation: integer)
---@return fun()
function M.document_events_subscribe(document, callback)
  local client = host_state()
  client.next_subscriber = client.next_subscriber + 1
  local subscriber_id = client.next_subscriber
  client.document_subscriber[subscriber_id] = { document = document, callback = protect_consumer(callback, "document event") }
  return function() client.document_subscriber[subscriber_id] = nil end
end

---Drain admitted requests and notify completion after the host process is collected.
---@param reason string?
---@param on_stopped fun()?
function M.host_stop(reason, on_stopped)
  if package.loaded["forge.lsp.bridge"] then require("forge.lsp.bridge").close() end
  reason = reason or "Forge host stopped"
  local client = host_state()
  if on_stopped then
    client.stop_callback = client.stop_callback or {}
    client.stop_callback[#client.stop_callback + 1] = protect_consumer(on_stopped, "shutdown")
  end
  local process = client.process
  if client.draining then return end
  client.draining = true
  client.stop_reason = reason
  client.ready = false
  client.harness_ready = false
  client.runtime_ready = false
  session.harness.ready = false
  if process then
    local generation = client.generation
    local launch_token = client.launch_token
    local function still_running()
      return M._client == client
        and client.generation == generation
        and client.launch_token == launch_token
        and client.process == process
    end
    local sent, failure = pcall(process.write, process, protocol.encode_request(0, "host.shutdown", {}))
    if not sent then notifications.error("Forge shutdown request failed: " .. tostring(failure), "Forge") end
    local shutdown_timer = assert(vim.uv.new_timer(), "Unable to create Forge shutdown timer")
    client.shutdown_timer = shutdown_timer
    client.shutdown_timer_token = launch_token
    shutdown_timer:start(host_drain_timeout_ms, 0, vim.schedule_wrap(function()
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
      host_collect_process(client, process, still_running, function(collected)
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
  client.runtime_ready = false
  client.snapshot = nil
  client.starting = false
  session.harness.ready = false
  local pending_list = client.pending
  client.pending = {}
  client.transfer = {}
  client.transfer_bytes = 0
  for _, pending in pairs(pending_list) do pending.callback(nil, reason) end
  if client.start_callback then host_finish_start(client.start_callback, nil, reason) end
  local callback_list = client.initialize_callback
  client.initialize_callback = {}
  for _, callback in ipairs(callback_list) do callback(nil, reason) end
  session_fail_initialization(client, reason)
  client.draining = false
  local stopped = client.stop_callback or {}
  client.stop_callback = {}
  for _, finished in ipairs(stopped) do finished() end
end

function M.host_get_generation()
  return host_state().generation
end


function M.host_is_accepting()
  local client = host_state()
  return client.process ~= nil and not client.draining
end

function M._host_set_launcher_for_test(launcher) launcher_for_test = launcher end
function M._host_reset_for_test()
  local client = M._client
  M.host_stop()
  if client then host_cancel_shutdown_timer(client, client.launch_token) end
  M._client = nil
  launcher_for_test = nil
  shutdown_autocmd = false
  exiting = false
end

return M
