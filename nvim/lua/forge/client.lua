local M = {}

local builder = require("forge.builder")
local provider_preference = require("forge.harness.provider_preference")
local config = require("forge.infra.config")
local notifications = require("forge.infra.notifications")
local protocol = require("forge.protocol")
local contract = require("forge.event_contract")
local command_intent = require("forge.command_intent")
local session = require("forge.session")
local receive = require("forge.receive")
local json_transfer = require("forge.json_transfer")
local perf = require("forge.infra.perf")

local launcher_for_test = nil
local shutdown_autocmd = false
local exiting = false
local history_clearing = false
local host_drain_timeout_ms = 3000
local host_unix_collection_timeout_ms = 1000
local host_windows_collection_timeout_ms = 2000
local send_request
local pump_admission
local max_active_requests, reserved_control_requests = 256, 4

---@alias ForgeCallback fun(result?: any, error?: string, error_detail?: table)
---@alias ForgeSubscriber fun(event: string, payload: any, session_id?: string)

---@class ForgeBackgroundSelection
---@field provider? string Background provider; omitted means the active chat's provider.
---@field model string
---@field reasoning_effort? string
---@field service_tier? 'default'|'fast'|'ultrafast'
---@field auto_compaction? {enabled: boolean, threshold_tokens: integer}

---@class ForgeTextGenerationConfig
---@field provider {kind: string, command: string[]}
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
---@field admitted? boolean
---@field start? fun()

---@class ForgeProviderRegistryAdoption
---@field generation integer Host generation whose `provider.registry.list` reply produced the registry.
---@field registry ForgeHarnessProviderRegistry Descriptors exactly as the host returned them.
---@field provider_map table<string, ForgeHarnessProviderConfig> Map last written to `config.options`; another table there means `config.setup` replaced the options.

---@class ForgeClient
---@field process table?
---@field provider_registry? ForgeProviderRegistryAdoption Host registry retained for the client's lifetime.
---@field generation integer
---@field launch_token integer
---@field next_id integer
---@field pending table<integer, ForgePendingRequest>
---@field admission_queue integer[]
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
---@field publication_by_session table<string, {generation: integer, cursor: integer, seen: table<integer, boolean>}>

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
    admission_queue = {},
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
    publication_by_session = {},
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

local function admission_counts(client)
  local count, ordinary = 0, 0
  for _, pending in pairs(client.pending) do
    if pending.admitted ~= false then
      count = count + 1
      if contract.method_policy(pending.method).admission ~= "control" then ordinary = ordinary + 1 end
    end
  end
  return count, ordinary
end

local function release_request(client, id)
  local pending = client.pending[id]
  client.pending[id] = nil
  if #client.admission_queue > 0 then
    vim.schedule(function() if M._client == client then pump_admission(client) end end)
  end
  return pending
end

---@param method string
---@param callback ForgeCallback
---@return integer?
local function host_admit_request(method, callback)
  local client = host_state()
  if history_clearing then callback(nil, "Forge history is being cleared", { code = "not_admitted" }) return nil end
  if exiting then callback(nil, "Forge host is exiting", { code = "not_admitted" }) return nil end
  if client.draining then callback(nil, "Forge host is draining", { code = "not_admitted" }) return nil end
  if client.next_id >= 9007199254740991 then
    callback(nil, "Forge request identity space is exhausted", { code = "not_admitted" })
    return nil
  end
  client.next_id = client.next_id + 1
  client.pending[client.next_id] = { method = method, callback = callback, admitted = false }
  return client.next_id
end

local function queue_ready_request(client, id)
  local index = #client.admission_queue + 1
  if contract.method_policy(client.pending[id].method).admission == "control" then
    index = 1
    while index <= #client.admission_queue do
      local pending = client.pending[client.admission_queue[index]]
      if pending and contract.method_policy(pending.method).admission ~= "control" then break end
      index = index + 1
    end
  end
  table.insert(client.admission_queue, index, id)
  pump_admission(client)
end

pump_admission = function(client)
  if client.draining or exiting then return end
  local index = 1
  while index <= #client.admission_queue do
    local count, ordinary = admission_counts(client)
    if count >= max_active_requests then break end
    local id = client.admission_queue[index]
    local pending = client.pending[id]
    local control = pending and contract.method_policy(pending.method).admission == "control"
    if not pending then
      table.remove(client.admission_queue, index)
    elseif control or ordinary < max_active_requests - reserved_control_requests then
      table.remove(client.admission_queue, index)
      pending.admitted = true
      local start = pending.start
      pending.start = nil
      start()
      index = 1
    else index = index + 1 end
  end
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
    local pending = release_request(client, id)
    pending.callback(nil, "Failed to write Forge request: " .. tostring(failure))
  end
end

---@return ForgeHarnessProviderDescriptor
local function session_provider_descriptor()
  local harness_config = config.options.harness
  local provider = harness_config.provider
  local configured = M.provider_map()[provider]
  assert(configured, "Unknown Harness provider: " .. tostring(provider))
  return {
    kind = provider,
    command = vim.deepcopy(configured.command or {}),
    permission_note = "Forge owns approval rules. Provider capabilities describe isolation support.",
  }
end

--- Merge host descriptors with user provider configuration into `config.options`.
---@param registry ForgeHarnessProviderRegistry
---@return table<string, ForgeHarnessProviderConfig> provider_map The map now held by `config.options`.
local function adopt_provider_registry(registry)
  if type(registry) ~= "table" or type(registry.providers) ~= "table"
    or type(registry.default_provider) ~= "string" then
    error("Forge returned an invalid provider registry")
  end
  local previous = config.options.harness.providers or {}
  local providers = {}
  for _, descriptor in ipairs(registry.providers) do
    if type(descriptor.id) ~= "string" or type(descriptor.label) ~= "string"
      or type(descriptor.command) ~= "table" then
      error("Forge returned an invalid provider descriptor")
    end
    local configured = previous[descriptor.id] or {}
    providers[descriptor.id] = vim.tbl_deep_extend("force", {
      command = vim.deepcopy(descriptor.command),
      label = descriptor.label,
      detail = descriptor.detail,
      selectable = descriptor.selectable,
      capability = vim.deepcopy(descriptor.capability),
      background_generation = type(descriptor.background_generation) == "table"
        and vim.deepcopy(descriptor.background_generation) or nil,
    }, configured)
  end
  config.options.harness.providers = providers
  local background = {}
  for id, descriptor in pairs(providers) do
    if descriptor.background_generation or (config.options.background_generation or {})[id] then
      background[id] = vim.tbl_deep_extend("force",
        vim.deepcopy(descriptor.background_generation or {}),
        vim.deepcopy((config.options.background_generation or {})[id] or {}))
    end
  end
  config.options.background_generation = background
  if not providers[registry.default_provider] then error("Forge registry default is missing") end
  if config.harness_provider_explicit and not providers[config.options.harness.provider] then
    error("Unknown Harness provider: " .. tostring(config.options.harness.provider))
  end
  if not config.harness_provider_explicit and not session.harness.session then
    config.options.harness.provider = provider_preference.load(providers, registry.default_provider)
  end
  return providers
end

--- Re-derive the provider map from the retained registry when `config.setup` replaced the
--- options that held the adopted map; user configuration may have changed, the host has not.
---@param adoption ForgeProviderRegistryAdoption
---@return string? failure
local function synchronize_provider_registry(adoption)
  if config.options.harness.providers == adoption.provider_map then return nil end
  local ok, provider_map = pcall(adopt_provider_registry, adoption.registry)
  if not ok then return tostring(provider_map) end
  adoption.provider_map = provider_map
  return nil
end

---@param client ForgeClient
---@return ForgeProviderRegistryAdoption?
local function current_provider_registry(client)
  local adoption = client.provider_registry
  if adoption and adoption.generation == client.generation then return adoption end
  return nil
end

--- Resolve the effective provider map: host registry descriptors merged with user configuration.
--- Readers use this instead of `config.options.harness.providers`, which `config.setup` resets.
---@return table<string, ForgeHarnessProviderConfig>
function M.provider_map()
  local adoption = M._client and current_provider_registry(M._client)
  if adoption then synchronize_provider_registry(adoption) end
  return config.options.harness.providers or {}
end

---@param callback fun(failure?: string)
local function ensure_provider_registry(callback)
  local client = host_state()
  local adoption = current_provider_registry(client)
  if adoption then callback(synchronize_provider_registry(adoption)) return end
  send_request(nil, "provider.registry.list", {}, function(registry, failure)
    if failure then callback(failure) return end
    local ok, provider_map = pcall(adopt_provider_registry, registry)
    if not ok then callback(tostring(provider_map)) return end
    client.provider_registry = { generation = client.generation, registry = registry, provider_map = provider_map }
    callback(nil)
  end)
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

---Consume each retained publication once, including live events received ahead of a reconnect gap.
---@param client ForgeClient
---@param message table
---@return boolean
local function accept_publication(client, message)
  local publication = type(message.payload) == "table" and message.payload._publication
  if not publication then return true end
  local sequence = publication.sequence
  assert(type(message.session_id) == "string" and type(sequence) == "number"
    and sequence >= 1 and sequence <= 9007199254740991 and sequence == math.floor(sequence),
    "Invalid Forge publication identity")
  local retained = client.publication_by_session[message.session_id]
  if not retained or retained.generation ~= client.generation then
    retained = { generation = client.generation, cursor = sequence - 1, seen = {} }
    client.publication_by_session[message.session_id] = retained
  end
  if sequence <= retained.cursor or retained.seen[sequence] then return false end
  retained.seen[sequence] = true
  while retained.seen[retained.cursor + 1] do
    retained.cursor = retained.cursor + 1
    retained.seen[retained.cursor] = nil
  end
  return true
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
  if message.event and not accept_publication(client, message) then return end
  if message.event == "lsp.request" or message.event == "lsp.cancel" or message.event == "lsp.mutation" then
    require("forge.lsp.bridge").handle(message.event, message.payload, message.session_id, client.generation)
    return
  end
  if message.event == "background_error" then
    notifications.error(message.payload and message.payload.error or "Background operation failed", "Forge Background")
    return
  end
  if message.event then
    local event_snapshot = client.snapshot_by_id[message.session_id]
    if event_snapshot and message.event == "questions_changed" then
      local previous = event_snapshot.questions
      if not previous or (message.payload.revision or 0) >= (previous.revision or 0) then
        event_snapshot.questions = vim.deepcopy(message.payload)
      end
    elseif event_snapshot and (message.event == "plan_question" or message.event == "plan_question_updated") then
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
  release_request(client, message.id)
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

---Recover missing retained events through the live dispatch path before adopting the final snapshot.
---@param snapshot table
function M.replay_publication(snapshot)
  local page = snapshot.owner_publication
  if not page then return end
  local client = host_state()
  local session_id = snapshot.session.id
  local retained = client.publication_by_session[session_id]
  if not retained or retained.generation ~= client.generation then
    -- A newly attached view adopts current state without replaying historical one-shot effects.
    client.publication_by_session[session_id] = {
      generation = client.generation, cursor = page.high_water or page.through or page.after or 0, seen = {},
    }
    return
  end
  for _, entry in ipairs(page.events or {}) do
    assert(entry.event.session_id == session_id and entry.event.payload._publication.sequence == entry.sequence,
      "Forge publication page belongs to a different event")
    host_dispatch_message(entry.event)
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
  if not config.harness_provider_explicit and not session.harness.session then
    harness_config.provider = provider_preference.load(harness_config.providers, harness_config.provider)
  end
  local descriptor = session_provider_descriptor()
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
    send_request(nil, "host.trace.configure", { level = config.options.log_level, default_only = true }, function(status, failure)
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
  local provider_launch_by_provider = {}
  for kind, provider in pairs(harness_config.providers) do
    provider_launch_by_provider[kind] = { kind = kind, command = vim.deepcopy(provider.command or {}) }
  end
  local params = {
    data_root = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"),
    config_dir = harness_config.config_dir or vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(
      vim.uv.fs_realpath(debug.getinfo(1, "S").source:sub(2)) or debug.getinfo(1, "S").source:sub(2))))),
    permission_file = vim.fs.joinpath(vim.fn.stdpath("config"), "forge", "permissions.json"),
    workspace = vim.fn.getcwd(),
    client_id = ("nvim-%s-%s"):format(vim.fn.getpid(), vim.uv.hrtime()),
    provider = { kind = descriptor.kind, command = descriptor.command },
    provider_launch_by_provider = provider_launch_by_provider,
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
    current.admission_queue = {}
    current.transfer = {}
    current.transfer_bytes = 0
    for id, pending in pairs(pending_list) do
      pending.callback(nil, (current.stop_reason or "Forge host stopped") .. ": unresolved request " .. id .. " (" .. pending.method .. ")", {
        code = pending.dispatched and "outcome_unknown" or "not_admitted", request_id = id, method = pending.method,
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
    failed = function(message, exit_result)
      -- A receiver that fails after the process exited, such as on a frame cut off by the exit,
      -- finishes that exit directly. `host_stop` would drain a process that can no longer answer
      -- and wait on a forced collection that cannot succeed, leaving the client draining.
      if exit_result then finish_exit(exit_result) else M.host_stop() end
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
---@param control_admission? boolean Control requests and internal initializers bypass the ordinary
---  startup waiter limit, so queued ordinary requests can never lock out task control.
function M.host_start(callback, control_admission)
  callback = protect_consumer(callback or function() end, "startup")
  local client = host_state()
  if history_clearing then callback(nil, "Forge history is being cleared", { code = "not_admitted" }) return end
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
    if #client.initialize_callback >= 64 and not control_admission then
      callback(nil, "Forge startup admission is full", { code = "not_admitted" })
      return
    end
    client.initialize_callback[#client.initialize_callback + 1] = callback
    return
  end
  host_start_process(callback)
end

---@param callback? ForgeCallback
---@param initialize_options? table
---@param startup? ForgeHarnessStartupTrace
---@param control_admission? boolean Bypass the ordinary startup waiter limit for task control.
function M.session_initialize(callback, initialize_options, startup, control_admission)
  callback = protect_consumer(callback or function() end, "Harness startup")
  local client = host_state()
  local mode = initialize_options and initialize_options.services_only and "runtime" or "harness"
  if client[mode .. "_ready"] and client.process then
    local adoption = current_provider_registry(client)
    if not adoption or adoption.provider_map ~= config.options.harness.providers then
      ensure_provider_registry(function(failure)
        if failure then callback(nil, failure) else M.session_initialize(callback, initialize_options, startup) end
      end)
      return
    end
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
    if #client[mode .. "_initialize_callback"] >= 64 and not control_admission then
      callback(nil, "Harness startup admission is full", { code = "not_admitted" })
      return
    end
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
    ensure_provider_registry(function(registry_failure)
      if registry_failure then session_finish_initialize(mode, callback, nil, registry_failure)
      else session_send_initialize(callback, initialize_options, startup) end
    end)
  end, true)
end

--- Resolve isolated generation launch settings from the loaded provider registry.
---@param selection? ForgeBackgroundSelection
---@return ForgeTextGenerationConfig? generation
---@return string? failure
function M.generation_get_config(selection)
  local harness_config = config.options.harness
  local selected = session.harness.session
  local provider = selection and selection.provider or selected and selected.provider or harness_config.provider
  local configured = M.provider_map()[provider]
  if not configured then return nil, "Unknown Harness provider: " .. tostring(provider) end
  selection = selection or (config.options.background_generation or {})[provider]
  if selection and (type(selection) ~= "table" or type(selection.model) ~= "string" or vim.trim(selection.model) == "") then
    return nil, "Invalid Background-generation model for provider: " .. provider
  end
  ---@type ForgeTextGenerationConfig
  local generation = {
    provider = { kind = provider, command = vim.deepcopy(configured.command or {}) },
    model = selection and selection.model
      or (selected and selected.provider == provider and selected.model or harness_config.model),
    reasoning_effort = selection and selection.reasoning_effort,
    service_tier = selection and selection.service_tier or "default",
    auto_compaction = selection and selection.auto_compaction,
    data_root = vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"),
  }
  return generation
end

--- Load registry defaults before resolving Background settings, without opening a conversation.
---@param callback fun(generation?: ForgeTextGenerationConfig, failure?: string)
---@param selection? ForgeBackgroundSelection
function M.generation_config(callback, selection)
  M.host_start(function(_, startup_failure)
    if startup_failure then callback(nil, startup_failure) return end
    ensure_provider_registry(function(registry_failure)
      if registry_failure then callback(nil, registry_failure) return end
      callback(M.generation_get_config(selection))
    end)
  end)
end

---@param params table
---@param callback ForgeCallback
---@param on_model? fun(model: string) Receives the configured generation model before request dispatch.
---@param selection? ForgeBackgroundSelection
---@return fun() cancel Cancels this generation without stopping the shared host.
function M.generation_request(params, callback, on_model, selection)
  local owner, finished, request_id = host_state(), false, nil
  M.generation_config(function(generation, failure)
    if finished then return end
    if not generation then finished = true callback(nil, failure) return end
    local effort = generation.reasoning_effort and (" · thinking " .. generation.reasoning_effort) or ""
    local label = generation.model .. effort
    if on_model then on_model(label .. " (" .. generation.provider.kind .. ")") end
    request_id = M.request({ method = "repository.generation", params = vim.tbl_extend("force", params, { generation = generation }) }, function(...)
      finished = true
      callback(...)
    end, function(progress)
      if finished then return end
      local event = progress.event
      if not on_model or type(event) ~= "table" then return end
      if event.kind == "runtime_resolved" and event.data then
        label = (type(event.data.model) == "string" and event.data.model or generation.model) .. effort
        on_model(label)
      elseif event.kind == "reasoning_summary" and type(event.text) == "string" then
        on_model(label .. "\n" .. event.text)
      elseif event.kind == "status" and event.data and type(event.data.reasoning_summary) == "string" then
        on_model(label .. "\n" .. event.data.reasoning_summary)
      end
    end)
  end, selection)
  return function()
    if finished then return end
    finished = true
    if not request_id then callback(nil, "Background generation cancelled") return end
    if host_state() ~= owner then return end
    local pending = owner.pending[request_id]
    if not pending then return end
    if not pending.dispatched then
      release_request(owner, request_id)
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
    local selection = session.harness_selection()
    deliver(result, request_error, error_detail)
    if active_state ~= request_state and session.harness_selection() == selection then
      session.activate_harness(active_state)
    end
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
  local client = host_state()
  params = vim.deepcopy(params or {})
  client.pending[id].progress = progress
  client.pending[id].session_id = session_id
  local function ready(_, failure, detail)
    if M._client ~= client or not client.pending[id] then return end
    if failure then
      release_request(client, id)
      receive(nil, failure, detail)
      return
    end
    local generation = client.generation
    client.pending[id].start = function()
      vim.schedule(function()
        if M._client ~= client or client.generation ~= generation or not client.pending[id] then return end
        if sending then sending(generation) end
        if not client.pending[id] then return end
        client.pending[id].dispatched = true
        host_write_request(id, method, params, session_id)
      end)
    end
    queue_ready_request(client, id)
  end
  -- Readiness may issue prerequisite RPCs, so it must not reserve a transport slot.
  local control_admission = contract.method_policy(method).admission == "control"
  if method == "session.initialize" or contract.method_policy(method).readiness == "host" then
    M.host_start(ready, control_admission)
  else
    M.session_initialize(ready, { services_only = true }, nil, control_admission)
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
local function dispatch_request(spec, callback, progress, owned_command)
  callback = callback or function() end
  local valid, policy = pcall(contract.validate_request, spec)
  if not valid then
    protect_consumer(callback, "request validation")(nil, tostring(policy), { code = "invalid_request" })
    return
  end
  local function transport(session_id, method, params, receive, sending)
    return send_request(session_id, method, params, receive, sending, progress)
  end
  if (owned_command or policy.recovery == "session_command") and spec.session_id then
    require("forge.command").dispatch(transport, spec.session_id, spec.method, spec.params, callback, owned_command)
  else
    return transport(spec.session_id, spec.method, spec.params, callback)
  end
end

---Execute one caller command; completion reports its finite committed outcome, never the eventual answer.
---Legacy aliases pass their exact wire shape privately so existing UI response projections remain compatible.
---@param spec ForgeCommandSpec
---@param callback? ForgeCallback
---@param progress? fun(payload: table)
---@param legacy_spec? ForgeRequestSpec
---@return integer? request_id
function M.execute_command(spec, callback, progress, legacy_spec)
  callback = callback or function() end
  local valid, request = pcall(command_intent.to_request, spec)
  if not valid then
    protect_consumer(callback, "command validation")(nil, tostring(request), { code = "invalid_command" })
    return
  end
  return dispatch_request(legacy_spec or request, function(result, failure, detail)
    if not failure and result and result.state == "failed" then
      callback(nil, result.error or "Command failed", { code = result.error_code or "command_failed", operation_id = result.id })
    elseif not failure and result and result.state == "outcome_unknown" then
      callback(nil, result.error or "Command outcome is unconfirmed", { code = result.error_code or "outcome_unknown", operation_id = result.id })
    elseif failure and detail and detail.code == "command_outcome_uncommitted" then
      callback(nil, failure, vim.tbl_extend("force", detail, { code = "outcome_unknown", cause_code = detail.code }))
    else
      callback(result, failure, detail)
    end
  end, progress, true)
end

---Keep transport aliases compatible while every lifecycle caller enters execute_command.
---@param spec ForgeRequestSpec
---@param callback? ForgeCallback
---@param progress? fun(payload: table)
---@return integer? request_id
function M.request(spec, callback, progress)
  local intent = command_intent.from_request(spec)
  if intent then return M.execute_command(intent, callback, progress, spec) end
  return dispatch_request(spec, callback, progress)
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
  local waiting = {}
  for id, pending in pairs(client.pending) do
    if pending.admitted == false then waiting[#waiting + 1] = { id = id, request = pending } client.pending[id] = nil end
  end
  client.admission_queue = {}
  for _, entry in ipairs(waiting) do
    entry.request.callback(nil, reason, { code = "not_admitted", request_id = entry.id, method = entry.request.method })
  end
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
  client.admission_queue = {}
  client.transfer = {}
  client.transfer_bytes = 0
  for id, pending in pairs(pending_list) do
    pending.callback(nil, reason, { code = pending.dispatched and "outcome_unknown" or "not_admitted",
      request_id = id, method = pending.method })
  end
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

---Stop the host before clearing state versions, fencing requests until native cleanup settles.
---@param callback? fun(error?: string)
function M.history_clear(callback)
  if history_clearing then
    local failure = "Forge history is already being cleared"
    notifications.error(failure, "ForgeHistoryClear")
    if callback then callback(failure) end
    return
  end
  local executable = builder.binary_path()
  if vim.fn.executable(executable) ~= 1 then
    local failure = "Forge executable is missing. Run :ForgeBuild before clearing history."
    notifications.error(failure, "ForgeHistoryClear")
    if callback then callback(failure) end
    return
  end
  history_clearing = true
  local function finish(failure, current_cleared)
    history_clearing = false
    if current_cleared then
      local client = host_state()
      client.snapshot, client.snapshot_by_id, client.publication_by_session = nil, {}, {}
      local cleaned, cleanup_error = pcall(function()
        require("forge.views.harness.session_navigation").history_clear()
      end)
      if not cleaned then
        failure = (failure and (failure .. "\n") or "") .. "State cleared, but view cleanup failed: " .. tostring(cleanup_error)
      end
    end
    if failure then
      notifications.error("History clear failed: " .. failure, "ForgeHistoryClear")
    end
    if callback then callback(failure) end
  end
  M.host_stop("Forge history is being cleared", function()
    local started, failure = pcall(vim.system, {
      executable, "history-clear", "--data-root", vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"),
    }, { text = true, stdout = true, stderr = true }, function(result)
      vim.schedule(function()
        if vim.trim(result.stdout or "") == "" then
          finish(vim.trim(result.stderr or "") ~= "" and vim.trim(result.stderr)
            or ("Forge history-clear returned no report (exit " .. tostring(result.code) .. ")"))
          return
        end
        local decoded, report = pcall(vim.json.decode, result.stdout)
        if not decoded or type(report) ~= "table" or type(report.current_cleared) ~= "boolean"
          or type(report.retained) ~= "table" then
          finish("Forge history-clear returned an invalid report: " .. tostring(report))
          return
        end
        local retained = {}
        for _, entry in ipairs(report.retained) do
          if type(entry) ~= "table" or type(entry.path) ~= "string" or type(entry.message) ~= "string" then
            finish("Forge history-clear returned an invalid retained-version entry", report.current_cleared)
            return
          end
          retained[#retained + 1] = entry.path .. ": " .. entry.message
        end
        local failure = #retained > 0 and table.concat(retained, "\n") or nil
        if result.code ~= 0 and not failure then
          failure = vim.trim(result.stderr or "") ~= "" and vim.trim(result.stderr)
            or ("Forge history-clear exited " .. tostring(result.code))
        end
        finish(failure, report.current_cleared)
      end)
    end)
    if not started then finish(tostring(failure)) end
  end)
end

function M.host_get_generation()
  return host_state().generation
end


function M.host_is_accepting()
  local client = host_state()
  return client.process ~= nil and not client.draining
end

function M._host_set_launcher_for_test(launcher) launcher_for_test = launcher end
--- Treat the configured provider map as the current host generation's adopted registry, or
--- forget the adoption so the next request fetches `provider.registry.list`.
---@param adopted boolean
function M._provider_registry_adopt_for_test(adopted)
  local client = host_state()
  client.provider_registry = adopted and {
    generation = client.generation,
    registry = { default_provider = config.options.harness.provider or "", providers = {} },
    provider_map = config.options.harness.providers,
  } or nil
end

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
