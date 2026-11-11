local MiniTest = require("mini.test")
local isolation = dofile("nvim/lua/forge/tests/support/isolation.lua")
local wait_until = dofile("nvim/lua/forge/tests/support/wait.lua")
local buffer = dofile("nvim/lua/forge/tests/support/buffer.lua")
local notify = dofile("nvim/lua/forge/tests/support/notify.lua")
local eq = MiniTest.expect.equality

local T = MiniTest.new_set({
  hooks = isolation.hooks({
    -- Stop the mock host process, its shutdown timer, and queued work while the case's launcher
    -- and stubs are still installed.
    post_case = function()
      local client = package.loaded["forge.client"]
      if client then client._host_reset_for_test() end
    end,
  }),
})

T["admission_queue"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local writes, results, emit = {}, {}, nil
  builder.ensure = function(callback) callback({ ok = true, path = "admission-fixture" }) end
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
        elseif request.method == "host.shutdown" then
          vim.schedule(function() on_exit({ code = 0 }) end)
        elseif request.method ~= "host.transport.consumed" then
          writes[#writes + 1] = request
        end
      end,
      kill = function() error("admission fixture should shut down cleanly") end,
    }
  end)
  local function record(label)
    return function(result, failure, detail)
      assert(not results[label], "request completed twice: " .. label)
      results[label] = { result = result, failure = failure, detail = detail }
    end
  end
  local function start()
    local ready = false
    client.host_start(function(_, failure) assert(not failure) ready = true end)
    wait_until(function() return ready end, "admission fixture host did not start")
    client._client.runtime_ready = true
    client._provider_registry_adopt_for_test(true)
  end
  local function ordinary(label, params)
    return client.request({ method = "repository.status.document", params = params or { label = label } }, record(label))
  end
  local function control(label)
    return client.request({ method = "session.health.get", session_id = "admission", params = { label = label } }, record(label))
  end
  local function respond(request)
    emit({ id = request.id, result = { label = request.params.label } })
  end
  start()
  for index = 1, 252 do ordinary("ordinary-" .. index) end
  wait_until(function() return #writes == 252 end, "ordinary requests did not reach the host")
  local params = { label = "queued-one", nested = { retained = true } }
  ordinary("queued-one", params)
  params.label, params.nested.retained = "changed", false
  ordinary("queued-two")
  for index = 1, 4 do control("control-" .. index) end
  wait_until(function() return #writes == 256 end, "queued and control requests did not fill the transport")
  assert(not next(results), "capacity rejected a queued request")
  control("control-five")
  respond(writes[1])
  wait_until(function() return #writes == 257 end, "reserved control request was not dispatched after a reply")
  assert(writes[257].params.label == "control-five", "ordinary backlog blocked reserved control progress")
  respond(writes[2])
  wait_until(function() return #writes == 258 end, "first queued ordinary request was not dispatched")
  assert(writes[258].params.label == "queued-one" and writes[258].params.nested.retained,
    "queued request lost its original payload or FIFO identity")
  respond(writes[258])
  wait_until(function() return #writes == 259 end, "second queued ordinary request was not dispatched")
  eq(writes[259].params.label, "queued-two")
  respond(writes[259])
  wait_until(function() return results["queued-one"] and results["queued-two"] end, "queued replies did not complete")
  assert(results["queued-one"].result.label == "queued-one" and results["queued-two"].result.label == "queued-two",
    "queued replies lost callback association")
  ordinary("refill")
  wait_until(function() return #writes == 260 end, "refill request was not dispatched")
  client.generation_config = function(callback)
    callback({ model = "fixture", provider = { kind = "fixture" } })
  end
  local cancelled = 0
  local cancel = client.generation_request({ text = "queued background" }, function(_, error)
    assert(error == "Background generation cancelled") cancelled = cancelled + 1
  end)
  cancel()
  cancel()
  assert(cancelled == 1, "queued cancellation did not complete once")
  ordinary("shutdown-queued")
  assert(not results["shutdown-queued"], "saturation failed instead of waiting")
  client.host_stop("fixture shutdown")
  assert(results["shutdown-queued"] and results["shutdown-queued"].detail.code == "not_admitted",
    "shutdown did not reject undispatched queued work with typed evidence")
  wait_until(function() return not client._client.process end, "host process was not collected after shutdown")
  assert(#writes == 260, "cancelled or shutdown queue entries reached the host")
  assert(results["refill"].detail.code == "outcome_unknown", "dispatched request lost unknown-outcome evidence")
  start()
  ordinary("new-generation")
  wait_until(function() return #writes == 261 end, "new-generation request was not dispatched")
  assert(writes[261].params.label == "new-generation", "old queued work replayed on a new host")
  respond(writes[261])
  wait_until(function() return results["new-generation"] end, "new-generation reply did not complete")
  local offset = #writes
  for index = 1, 252 do ordinary("reentrant-ordinary-" .. index) end
  for index = 1, 4 do control("reentrant-active-" .. index) end
  wait_until(function() return #writes == offset + 256 end, "reentrant requests did not fill the transport")
  client._provider_registry_adopt_for_test(false)
  for index = 1, 5 do control("reentrant-queued-" .. index) end
  respond(writes[offset + 1])
  respond(writes[offset + 2])
  vim.wait(50, function() return false end, 1)
  local active = 0
  for _, pending in pairs(client._client.pending) do
    if pending.admitted ~= false then active = active + 1 end
  end
  assert(active <= 256, "recursive readiness admission exceeded the 256-request transport bound: " .. active)
  local registry_request
  for index = offset + 257, #writes do
    if writes[index].method == "provider.registry.list" then registry_request = writes[index] break end
  end
  assert(registry_request, "waiting controls starved their provider-registry prerequisite")
  emit({ id = registry_request.id, result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() })
  respond(writes[offset + 253])
  wait_until(function()
    for index = offset + 257, #writes do
      if writes[index].params.label == "reentrant-queued-1" then return true end
    end
  end, "waiting reentrant control was not dispatched")
  client.host_stop("reentrant fixture complete")
  wait_until(function() return not client._client.process end, "host process was not collected after the reentrant fixture")
  start()
  offset = #writes
  for index = 1, 252 do ordinary("startup-ordinary-" .. index) end
  for index = 1, 4 do control("startup-active-" .. index) end
  wait_until(function() return #writes == offset + 256 end, "startup requests did not fill the transport")
  client._client.runtime_ready = false
  client._provider_registry_adopt_for_test(false)
  control("startup-waiting")
  respond(writes[offset + 1])
  wait_until(function() return #writes == offset + 257 end, "runtime startup registry request was not dispatched")
  assert(writes[#writes].method == "provider.registry.list", "full transport starved runtime startup registry")
  emit({ id = writes[#writes].id, result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() })
  wait_until(function() return #writes == offset + 258 end, "runtime initialization request was not dispatched")
  assert(writes[#writes].method == "session.initialize", "full transport starved runtime initialization")
  emit({ id = writes[#writes].id, result = { initialized = true } })
  wait_until(function() return #writes == offset + 259 end, "trace configuration request was not dispatched")
  eq(writes[#writes].method, "host.trace.configure")
  emit({ id = writes[#writes].id, result = { level = "none" } })
  wait_until(function() return #writes == offset + 260 end, "initialized waiting request was not dispatched")
  assert(writes[#writes].params.label == "startup-waiting", "initialized request never reached transport")
  respond(writes[#writes])
  wait_until(function() return results["startup-waiting"] end, "startup-waiting reply did not complete")
end

T["callback_isolation"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local session = require("forge.session")
  local sent = {}
  local emit
  local initializing_harness = false
  local response_version = require("forge.protocol").VERSION
  builder.ensure = function(callback) callback({ ok = true, path = "callback-fixture" }) end
  local diagnostic = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        sent[request.method] = request.id
        if request.method == "host.initialize" then emit({ id = request.id, result = { protocol_version = response_version } }) end
        if request.method == "provider.registry.list" and initializing_harness then
          emit({ id = request.id, result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() })
        end
        if request.method == "session.initialize" then emit({ id = request.id, result = { session = { id = "fixture" } } }) end
        if request.method == "host.trace.configure" then emit({ id = request.id, result = { level = "none" } }) end
        if request.method == "host.shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
      end,
      kill = function() end,
    }
  end)

  local started = false
  client.host_start(function() error("startup UI fixture failure") end)
  client.host_start(function(_, start_error) assert(not start_error) started = true end)
  wait_until(function() return started end, "throwing startup consumer skipped another waiter")
  assert(client._client.ready and not client._client.draining)

  local completed = false
  client.request({ method = "provider.registry.list", params = {} }, function() error("reply UI fixture failure") end)
  client.request({ method = "repository.status.document", params = {} }, function(result) completed = result.ok end)
  wait_until(function() return sent["provider.registry.list"] and sent["repository.status.document"] end, "requests were not written")
  emit({ id = sent["provider.registry.list"], result = { document = "policy" } })
  emit({ id = sent["repository.status.document"], result = { ok = true } })
  wait_until(function() return completed end, "throwing response consumer skipped independent reply")
  assert(client._client.ready and client._client.process and not client._client.draining)
  assert(vim.tbl_isempty(client._client.pending))
  assert(#diagnostic == 2 and diagnostic[1].message:find("startup UI fixture failure", 1, true)
    and diagnostic[2].message:find("reply UI fixture failure", 1, true), "consumer failure diagnostics were lost")

  sent["provider.registry.list"] = nil
  client.request({ method = "provider.registry.list", params = {} }, function() error("error UI fixture failure") end)
  wait_until(function() return sent["provider.registry.list"] end, "error request was not written")
  emit({ id = sent["provider.registry.list"], error = { code = "fixture", message = "expected request error" } })
  wait_until(function() return #diagnostic == 3 end, "error callback exception was not reported")
  assert(client._client.ready and not client._client.draining)

  local harness_started = false
  initializing_harness = true
  client.session_initialize(function() error("Harness startup UI fixture failure") end)
  client.session_initialize(function(_, start_error) assert(not start_error) harness_started = true end)
  wait_until(function() return harness_started end, "throwing Harness startup consumer skipped another waiter")
  local requesting_state = session.new_harness_state()
  local viewing_state = session.new_harness_state()
  session.activate_harness(requesting_state)
  session.register_harness("fixture", requesting_state)
  client.request({ session_id = "fixture", method = "session.state.get", params = {} }, function()
    assert(session.harness == requesting_state)
    error("scoped UI fixture failure")
  end)
  session.activate_harness(viewing_state)
  wait_until(function() return sent["session.state.get"] end, "scoped request was not written")
  emit({ id = sent["session.state.get"], result = {} })
  wait_until(function() return #diagnostic == 5 end, "scoped consumer failure was not reported")
  assert(session.harness == viewing_state, "throwing scoped consumer changed active Harness state")
  assert(client._client.ready and not client._client.draining)

  client.request({ method = "github.issue.list", params = {} }, function() end)
  wait_until(function() return sent["github.issue.list"] end, "malformed transfer request was not written")
  emit({ request_id = sent["github.issue.list"], event = "result.complete", payload = { part_count = 1, total_bytes = 1 } })
  wait_until(function() return not client._client.ready end, "malformed protocol stopped being fatal")
  wait_until(function() return not client._client.process end, "malformed protocol host did not drain")
  response_version = 999
  local mismatch_reported = false
  client.host_start(function(_, start_error)
    assert(start_error:find("wire versions differ", 1, true))
    mismatch_reported = true
    error("mismatched startup UI fixture failure")
  end)
  wait_until(function() return mismatch_reported and not client._client.process end,
    "consumer failure prevented initialization mismatch shutdown")
end

T["executable_copy"] = function()
  local builder = require("forge.builder")
  local client = require("forge.client")
  ---@type fun(lease?: RustSidecarLease, failure?: string)?
  local complete_copy
  local released, spawned, completed = 0, 0, 0
  builder.ensure = function(callback) callback({ ok = true, path = "fixture" }) end
  builder.acquire = function(_, callback) complete_copy = callback end
  vim.system = function() spawned = spawned + 1 error("cancelled startup spawned a process") end
  client.host_start(function(_, start_error)
    assert(start_error, "cancelled startup succeeded")
    completed = completed + 1
  end)
  wait_until(function() return complete_copy ~= nil end, "host startup did not request the executable copy")
  client.host_stop()
  complete_copy({ path = "copy", release = function() released = released + 1 end })
  assert(released == 1 and spawned == 0 and completed == 1, "cancelled copy lost lifecycle ownership")
  client._host_reset_for_test()
  complete_copy = nil
  local copy_failure
  client.host_start(function(_, start_error) copy_failure = start_error end)
  wait_until(function() return complete_copy ~= nil end, "second host startup did not request the executable copy")
  complete_copy(nil, "copy denied")
  assert(copy_failure == "copy denied" and spawned == 0, "copy failure did not reach startup caller")
  client._host_reset_for_test()
  complete_copy = nil
  vim.system = function() error("fixture spawn failure") end
  local spawn_failure
  client.host_start(function(_, start_error) spawn_failure = start_error end)
  wait_until(function() return complete_copy ~= nil end, "third host startup did not request the executable copy")
  complete_copy({ path = "copy", release = function() released = released + 1 end })
  assert(spawn_failure and spawn_failure:find("fixture spawn failure", 1, true))
  assert(released == 2, "spawn failure retained executable copy")
end

T["exit_admission"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local launch_count, shutdown_count, restart_rejected, request_rejected = 0, 0, false, false
  builder.ensure = function(callback) callback({ ok = true, path = "exit-admission-fixture" }) end
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    launch_count = launch_count + 1
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = {
            protocol_version = require("forge.protocol").VERSION,
          } }) .. "\n")
        elseif request.method == "host.shutdown" then
          shutdown_count = shutdown_count + 1
          vim.schedule(function() on_exit({ code = 0 }) end)
        end
      end,
      kill = function() error("graceful fixture must not need forced collection") end,
    }
  end)

  local initialized = false
  client.host_start(function(_, problem) assert(not problem, problem) initialized = true end)
  wait_until(function() return initialized end, "host did not initialize")
  client.host_stop(nil, function()
    -- Completion runs inside the exit wait, after normal draining has cleared.
    client.host_start(function(result, problem, detail)
      assert(result == nil and problem == "Forge host is exiting")
      restart_rejected = detail.code == "not_admitted"
    end)
    client.request({ method = "repository.revision.list", params = {} }, function(result, problem, detail)
      assert(result == nil and problem == "Forge host is exiting")
      request_rejected = detail.code == "not_admitted"
    end)
  end)
  assert(client._client.process ~= nil, "ordinary runtime stop became synchronous")
  vim.api.nvim_exec_autocmds("VimLeavePre", { group = "ForgeClient" })
  assert(client._client.process == nil and not client._client.draining, "exit returned before collection")
  assert(restart_rejected and request_rejected, "exit callback admitted replacement work")
  assert(launch_count == 1 and shutdown_count == 1, "already-draining exit relaunched or repeated shutdown")
end

T["request_contract"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local protocol = require("forge.protocol")
  local session = require("forge.session")
  local requests, initialize_request, respond = {}, nil, nil
  builder.ensure = function(callback) callback({ ok = true, path = "request-contract" }) end
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, exited)
    respond = function(request, result)
      options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
    end
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        requests[#requests + 1] = request
        if request.method == "host.initialize" then
          respond(request, { protocol_version = protocol.VERSION })
        elseif request.method == "provider.registry.list" then
          respond(request, dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry())
        elseif request.method == "session.initialize" then
          if request.params.services_only then respond(request, { initialized = true })
          else initialize_request = request end
        elseif request.method == "host.trace.configure" then respond(request, { level = "none" })
        elseif request.method == "session.resume" then respond(request, { session = { id = request.session_id } })
        elseif request.method == "host.shutdown" then vim.schedule(function() exited({ code = 0 }) end) end
      end,
      kill = function() end,
    }
  end)

  local function request_log() return vim.inspect(requests) end
  local function last_request(method)
    for index = #requests, 1, -1 do
      if requests[index].method == method then return requests[index] end
    end
  end

  for _, specification in ipairs({
    {}, { method = "status" }, { method = "session.state.get" },
    { method = "session.state.get", session_id = " " },
    { method = "repository.revision.list", session_id = "unexpected" },
    { method = "repository.revision.list", params = { "array" } },
  }) do
    local rejected = false
    client.request(specification, function(result, message, detail)
      assert(result == nil and message and detail.code == "invalid_request")
      rejected = true
    end)
    assert(rejected and #requests == 0, "invalid request reached host startup")
  end

  local shared_complete = false
  client.request({ method = "permissions.document.open" }, function(result)
    shared_complete = result.path == "permissions.json"
  end)
  wait_until(function() return last_request("permissions.document.open") end, "shared request was not written",
    { context = request_log })
  assert(not initialize_request and not client._client.harness_ready, "shared request opened a session")
  respond(last_request("permissions.document.open"), { path = "permissions.json", source = "{}" })
  wait_until(function() return shared_complete end, "shared request did not complete", { context = request_log })

  local first_session, second_session
  client.session_initialize(function(result) first_session = result.session.id end, { session_id = "first" })
  client.session_initialize(function(result) second_session = result.session.id end, { session_id = "second" })
  wait_until(function() return initialize_request end, "session initialization was not written", { context = request_log })
  eq(initialize_request.params.session_id, "first")
  respond(initialize_request, { session = { id = "first" } })
  wait_until(function() return first_session and second_session end, "concurrent session startups did not complete",
    { context = request_log })
  assert(first_session == "first" and second_session == "second", "startup reused another caller's session")

  local origin = session.new_harness_state()
  local active = session.new_harness_state()
  session.register_harness("origin", origin)
  session.activate_harness(active)
  local tracing_complete = false
  client.request({ method = "host.trace.configure", session_id = "origin", params = { level = "trace" } }, function()
    assert(session.harness == origin, "trace callback lost its explicit session context")
    session.activate_harness(origin)
    tracing_complete = true
  end)
  wait_until(function() return tracing_complete end, "scoped trace request did not complete", { context = request_log })
  assert(session.harness == active, "temporary rendering activation stole the selected session")
  eq(last_request("host.trace.configure").session_id, "origin")
  local order = {}
  client.request({ method = "session.state.get", session_id = "origin" }, function(result)
    assert(session.harness == origin and result.session.id == "origin")
    order[#order + 1] = "session"
  end)
  client.request({ method = "repository.revision.list" }, function()
    assert(session.harness == active)
    order[#order + 1] = "repository"
  end)
  wait_until(function() return last_request("session.state.get") and last_request("repository.revision.list") end,
    "scoped and unscoped requests were not written", { context = request_log })
  respond(last_request("repository.revision.list"), {})
  respond(last_request("session.state.get"), { session = { id = "origin" } })
  wait_until(function() return #order == 2 end, "scoped and unscoped replies did not complete", { context = request_log })
  assert(order[1] == "repository" and order[2] == "session")
  assert(session.harness == active and vim.tbl_isempty(client._client.pending))
end

T["startup_cancellation"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local initialized, failed = false, 0
  builder.ensure = function(callback) callback({ ok = true, path = "startup-cancellation" }) end
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = {
            protocol_version = require("forge.protocol").VERSION,
          } }) .. "\n")
        elseif request.method == "provider.registry.list" then
          options.stdout(nil, vim.json.encode({ id = request.id,
            result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() }) .. "\n")
        elseif request.method == "session.initialize" then
          initialized = true
        elseif request.method == "host.shutdown" then
          vim.schedule(function() on_exit({ code = 0 }) end)
        end
      end,
      kill = function() end,
    }
  end)
  for _ = 1, 2 do
    client.session_initialize(function(result, message)
      assert(result == nil and message, "cancelled startup reported success")
      failed = failed + 1
    end, { services_only = true })
  end
  wait_until(function() return initialized end, "initialization was not sent")
  client.host_stop()
  wait_until(function() return client._client.process == nil end, "host did not stop")
  assert(failed == 2, "concurrent runtime startup lost a completion callback: " .. failed)
  assert(not client._client.runtime_starting and not client._client.harness_starting)
  assert(vim.tbl_isempty(client._client.runtime_initialize_callback))
  assert(vim.tbl_isempty(client._client.pending))
end

T["status_document_events"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local received = {}
  local emit
  client._host_reset_for_test()
  builder.ensure = function(callback) callback({ ok = true, path = "document-event-fixture" }) end
  local notices = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  client._host_set_launcher_for_test(function(_, options, on_exit)
    emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
    return { write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.initialize" then emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) end
      if request.method == "repository.write" then emit({ id = request.id, result = {} }) end
      if request.method == "host.shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
    end, kill = function() end }
  end)

  local ready = false
  client.host_start(function(_, failure) assert(not failure) ready = true end)
  wait_until(function() return ready end, "document event fixture host did not start")
  client.document_events_subscribe("other", function() error("document crossed ownership") end)
  local unsubscribe = client.document_events_subscribe("status", function(event, payload, generation)
    received[#received + 1] = { event = event, payload = payload, generation = generation }
  end)
  local update = { document = "status", event = "status.update", payload = { operation_id = 0, phase = "accepted", diagnostic = {}, content = string.rep("x", 600000) } }
  local encoded = vim.json.encode(update)
  local part_count = math.ceil(#encoded / (256 * 1024))
  for sequence = 0, part_count - 1 do
    emit({ document = "status", event = "document.part", payload = { sequence = sequence, part_count = part_count, total_bytes = #encoded,
      payload = encoded:sub(sequence * 256 * 1024 + 1, (sequence + 1) * 256 * 1024) } })
  end
  wait_until(function() return client._client.transfer["document:status"] ~= nil end, "document transfer did not start")
  assert(#received == 0, "partial update reached a document consumer")
  emit({ document = "status", event = "document.complete", payload = { part_count = part_count, total_bytes = #encoded } })
  wait_until(function() return #received == 1 end, "completed document update did not arrive")
  assert(received[1].payload.content == update.payload.content and received[1].generation == client._client.generation)
  eq(client._client.transfer_bytes, 0)
  update.payload = { operation_id = 0, phase = "settled", diagnostic = { "write rejected" } }
  emit(update)
  emit(update)
  wait_until(function() return #received == 3 end, "operation-zero updates did not arrive")
  assert(#notices == 1 and notices[1].message == "write rejected", "operation zero lost or duplicated its diagnostic")
  emit({ document = "status", event = "document.complete", payload = { part_count = 1, total_bytes = 1 } })
  wait_until(function() return #received == 4 end, "malformed completion did not resynchronize the document")
  assert(received[4].event == "status.resync" and #notices == 2 and client._client.ready)
  unsubscribe()
  emit(update)
  wait_until(function() return client._client.pending and next(client._client.pending) == nil end,
    "event after unsubscribe was not processed")
  assert(#received == 4, "closed document received another event")
end

T["client"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local notifications = require("forge.infra.notifications")
  local original_error, original_echo = notifications.error, vim.api.nvim_echo
  local launch_count = 0
  ---@type fun(result: table)?
  local finish_build

  local original_start_harness, original_request = client.session_initialize, client.request
  local recovery_options, recovery_method
  client.session_initialize = function(callback, options)
    recovery_options = options
    callback({ initialized = true })
  end
  client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

    recovery_method = method
    eq(params.session_id, "leased-session")
    callback({ forked = true })
  end
  for _, action in ipairs({ "retry", "new", "fork" }) do
    recovery_method = nil
    local recovered
    client.session_resolve_lease_conflict(action, { session_id = "leased-session" }, function(result, request_error)
      assert(not request_error)
      recovered = result
    end)
    assert(recovered)
    if action == "retry" then
      assert(recovery_options.session_id == "leased-session" and recovery_options.lease_conflict_action == nil,
        "retry replaced the leased session with a new session")
    else
      eq(recovery_options.lease_conflict_action, "new")
    end
    eq(recovery_method, (action == "fork" and "session.fork" or nil))
  end
  client.session_initialize, client.request = original_start_harness, original_request
  builder.ensure = function(done) finish_build = done end
  client._host_set_launcher_for_test(function() launch_count = launch_count + 1 error("stopped build launched a host") end)
  local completed, rejected = 0, 0
  for _ = 1, 70 do
    client.request({ method = "repository.revision.list", params = { workspace = "fixture" } }, function(_, request_error, detail)
      assert(request_error)
      completed = completed + 1
      if request_error:find("admission is full", 1, true) then
        assert(detail and detail.code == "not_admitted")
        rejected = rejected + 1
      end
    end)
  end
  -- While the build runs, the launching request plus 64 waiters fill startup admission; the rest
  -- fail as not admitted so callers may retry them safely.
  assert(rejected == 5 and completed == 5, vim.inspect({ rejected = rejected, completed = completed }))
  eq(vim.tbl_count(client._client.pending), 65)
  local control_completed = 0
  for _, method in ipairs({ "session.command.apply", "session.command.operation.get", "session.health.get" }) do
    client.request({ session_id = "admission", method = method, params = {} }, function(_, request_error)
      assert(request_error and not request_error:find("admission is full", 1, true))
      control_completed = control_completed + 1
    end)
  end
  assert(vim.tbl_count(client._client.pending) == 68, "ordinary requests consumed task control capacity")
  client.host_stop()
  eq(control_completed, 3)
  assert(completed == 70, "stopping the host failed or duplicated a queued request")
  assert(vim.tbl_isempty(client._client.pending))
  assert(finish_build)
  finish_build({ ok = true, path = "late-build" })
  assert(launch_count == 0, "a cancelled startup accepted a late build")

  client._host_reset_for_test()
  local write_order, transport_completed = {}, false
  builder.ensure = function(done) done({ ok = true, path = "transport-order-fixture" }) end
  client._host_set_launcher_for_test(function(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        write_order[#write_order + 1] = request.method
        if request.method == "host.initialize" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "repository.status.document" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = { opened = true } }) .. "\n")
        end
      end,
      kill = function() end,
    }
  end)
  client.request({ method = "repository.status.document", params = {} }, function(result, request_error)
    assert(not request_error and result.opened)
    transport_completed = true
  end)
  wait_until(function() return transport_completed end, "initial transport request did not complete")
  assert(table.concat(write_order, ",") == "host.initialize,host.transport.consumed,repository.status.document,host.transport.consumed",
    "first application request bypassed initialization response credit")
  client._host_reset_for_test()
  require("forge").setup({ harness = { provider = "mock" }, harness_logging = false })
  builder.ensure = function(done) done({ ok = true, path = "admission-fixture" }) end
  client._host_set_launcher_for_test(function(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.transport.consumed" or request.method == "host.shutdown" then return end
        if request.method == "github.issue.sync" then
          local messages = {
            { request_id = request.id, event = "github.issue.sync.progress", payload = { phase = "reading", fetched = 0 } },
            { request_id = request.id, event = "repository.status.document.progress", payload = {} },
            { id = request.id, result = { refreshed = true, fetched = 1, pages = 1 } },
            { request_id = request.id, event = "github.issue.sync.progress", payload = { phase = "late" } },
          }
          for _, message in ipairs(messages) do options.stdout(nil, vim.json.encode(message) .. "\n") end
          return
        end
        if request.method == "github.issue.list" then
          options.stdout(nil, vim.json.encode({ id = request.id, error = { code = "busy", message = "storage is busy" } }) .. "\n")
          return
        end
        if request.method == "github.comment" or request.method:match("^github%.review%.document%.") then
          options.stdout(nil, vim.json.encode({ id = request.id, error = { code = "review_error", message = "review operation failed" } }) .. "\n")
          return
        end
        local result = {}
        if request.method == "host.initialize" then result.protocol_version = require("forge.protocol").VERSION
        elseif request.method == "provider.registry.list" then result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry()
        elseif request.method == "session.initialize" then result.session = { id = "admission" } end
        options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
      end,
      kill = function() end,
    }
  end)
  local accepted = 0
  for _ = 1, 60 do
    client.request({ session_id = "admission", method = "session.state.get", params = {} }, function(_, request_error)
      assert(not request_error, request_error)
      accepted = accepted + 1
    end)
  end
  wait_until(function() return accepted == 60 end, "full ordinary admission prevented startup control traffic")
  assert(vim.tbl_isempty(client._client.pending))
  local events, progress, finished = 0, 0, false
  local unsubscribe = client.session_events_subscribe(function() events = events + 1 end)
  client.request({ method = "github.issue.sync", params = {} }, function(result, request_error)
    assert(not request_error, request_error)
    eq(result.fetched, 1)
    finished = true
  end, function(payload)
    eq(payload.phase, "reading")
    assert(not finished, "progress arrived after completion")
    progress = progress + 1
  end)
  wait_until(function() return finished end, "progress-reporting request did not complete")
  assert(progress == 1 and events == 0, "request progress escaped into Harness subscribers or survived completion")
  local failed = false
  client.request({ method = "github.issue.list", params = {} }, function(_, request_error)
    eq(request_error, "storage is busy")
    failed = true
  end)
  wait_until(function() return failed end, "storage-busy request did not fail")
  assert(events == 0, "host storage failure invalidated Harness state")
  for _, method in ipairs({ "github.comment", "github.review.document.open", "github.review.document.input.apply", "github.review.document.save", "github.review.document.reconcile", "github.review.document.close" }) do
    local completed = false
    client.request({ method = method, params = {} }, function(_, request_error)
      eq(request_error, "review operation failed")
      completed = true
    end)
    wait_until(function() return completed end, "review operation did not fail")
    assert(events == 0, "review failure invalidated Harness state")
  end
  local reported, completed_after_failure = 0, false
  notifications.error = function() error("notification failed") end
  vim.api.nvim_echo = function(chunks, history, options)
    assert(chunks[1][1]:find("progress callback failed", 1, true) and history and options.err)
    reported = reported + 1
  end
  client.request({ method = "github.issue.sync", params = {} }, function(result, request_error)
    assert(not request_error and result.fetched == 1)
    completed_after_failure = true
  end, function() error("progress callback failed") end)
  wait_until(function() return completed_after_failure end, "request with a throwing progress callback did not complete")
  assert(reported == 1 and vim.tbl_isempty(client._client.pending))
  notifications.error, vim.api.nvim_echo = original_error, original_echo
  unsubscribe()
  local protocol = require("forge.protocol")
  for _, message in ipairs({
    { request_id = 1.5, event = "github.issue.sync.progress", payload = {} },
    { request_id = 1, event = "github.issue.sync.progress", payload = {}, session_id = "foreign" },
    { request_id = 1, event = "github.issue.sync.progress", payload = {}, id = 1 },
  }) do
    local decoded, decode_failure = protocol.decode_message(vim.json.encode(message))
    assert(decoded == nil and decode_failure)
  end
  client._host_reset_for_test()
  local output, collected, held_id
  local credits, killed = 0, 0
  client._host_set_launcher_for_test(function(_, options, on_exit)
    output, collected = options.stdout, on_exit
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          eq(request.params.recovery_directory, vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "recovery", "github", "v1"))
          eq(request.params.status_ignored_directory, vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "status-ignored"))
        end
        if request.method == "host.initialize" then
          output(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "host.transport.consumed" then credits = credits + 1
        elseif request.method ~= "host.shutdown" then held_id = request.id end
      end,
      kill = function() killed = killed + 1 end,
    }
  end)
  local settled
  client.request({ method = "github.comment", params = {} }, function(result, request_error)
    assert(not request_error, request_error)
    settled = result
  end)
  wait_until(function() return held_id ~= nil end, "mutation request was not written")
  local generation = client._client.generation
  local stopped = false
  client.host_stop(nil, function()
    assert(client._client.process == nil and not client._client.draining,
      "shutdown completion ran before process collection")
    assert(settled and settled.posted, "shutdown completion discarded an admitted mutation")
    stopped = true
  end)
  assert(client._client.draining and client._client.generation == generation)
  assert(not stopped, "shutdown completion must wait for the host")
  assert(not settled and client._client.pending[held_id], "drain discarded an admitted mutation")
  local rejected_drain
  client.request({ session_id = "admission", method = "session.state.get", params = {} }, function(_, request_error) rejected_drain = request_error end)
  eq(rejected_drain, "Forge host is draining")
  local previous_credits = credits
  output(nil, vim.json.encode({ id = held_id, result = { posted = true } }) .. "\n")
  wait_until(function() return settled ~= nil end, "held mutation reply did not settle")
  assert(settled.posted and credits > previous_credits and killed == 0)
  collected({ code = 0 })
  wait_until(function() return not client._client.draining end, "draining did not finish after collection")
  assert(client._client.process == nil and vim.tbl_isempty(client._client.pending))
  assert(stopped, "shutdown completion must run after the host is collected")
end

T["result_transfer"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local notifications = require("forge.infra.notifications")
  local emit, request_id
  local calls, errors, events = 0, 0, 0
  local received

  ---@param method? string
  local function start(method)
    method = method or "github.issue.list"
    client._host_reset_for_test()
    calls, errors, events, received, request_id = 0, 0, 0, nil, nil
    builder.ensure = function(done) done({ ok = true, path = "result-fixture" }) end
    notifications.error = function() end
    client._host_set_launcher_for_test(function(_, options, on_exit)
      emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
      local exited = false
      return {
        write = function(_, encoded)
          local request = vim.json.decode(encoded)
          if request.method == "host.initialize" then
            emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
          elseif request.method == method then
            request_id = request.id
          elseif request.method == "host.shutdown" and not exited then
            exited = true
            vim.schedule(function() on_exit({ code = 0, signal = 0 }) end)
          end
        end,
        kill = function() end,
      }
    end)
    client.session_events_subscribe(function(event)
      if event ~= "host_stopped" then events = events + 1 end
    end)
    client.request({ method = method, params = {} }, function(result, failure)
      calls = calls + 1
      if failure then errors = errors + 1 else received = result end
    end)
    wait_until(function() return request_id ~= nil end, "fixture request was not written")
  end

  local function part(sequence, count, bytes, payload, identity)
    emit({ request_id = identity or request_id, event = "result.part", payload = {
      sequence = sequence, part_count = count, total_bytes = bytes, payload = payload,
    } })
  end

  local function complete(count, bytes)
    emit({ request_id = request_id, event = "result.complete", payload = { part_count = count, total_bytes = bytes } })
  end

  local function settled()
    wait_until(function() return calls == 1 end, "transfer request did not complete")
    assert(client._client.transfer_bytes == 0 and vim.tbl_isempty(client._client.transfer))
    assert(vim.tbl_isempty(client._client.pending))
    assert(events == 0, "result transfer escaped into Harness subscribers")
  end

  start("github.review.document.open")
  local body = string.rep('日本語 "quote" \\ newline\n', 900000)
  local encoded = vim.json.encode({ id = request_id, result = { body = body } })
  local chunk = 100000
  local count = math.ceil(#encoded / chunk)
  assert(#encoded > 16 * 1024 * 1024 and count > 256)
  for sequence = 0, count - 1 do
    part(sequence, count, #encoded, encoded:sub(sequence * chunk + 1, (sequence + 1) * chunk))
    wait_until(function()
      local transfer = client._client.transfer[request_id]
      return transfer and transfer.sequence == sequence + 1
    end, "result transfer part was not buffered")
  end
  wait_until(function()
    local transfer = client._client.transfer[request_id]
    return transfer and transfer.sequence == count
  end, "result transfer did not buffer every part")
  assert(calls == 0, "parts completed a request before its completion marker")
  complete(count, #encoded)
  settled()
  assert(errors == 0 and received.body == body)
  complete(count, #encoded)
  vim.wait(10, function() return false end, 1)
  assert(calls == 1, "late completion duplicated a callback")

  start()
  local nullable = vim.json.encode({ id = request_id, result = { optional = vim.NIL, list = { vim.NIL, "second" } } })
  part(0, 1, #nullable, nullable)
  complete(1, #nullable)
  settled()
  assert(errors == 0 and received.optional == nil and received.list[1] == vim.NIL)

  start()
  local empty = vim.json.encode({ id = request_id, result = vim.NIL })
  part(0, 1, #empty, empty)
  complete(1, #empty)
  settled()
  assert(errors == 0 and received == nil)

  local corrupt = {
    function() part(1, 2, 4, "{}") end,
    function() part(0, 2, 4, "{}"); part(0, 2, 4, "{}") end,
    function() part(0, 2, 4, "{}"); part(1, 2, 5, "{}") end,
    function() part(0, 2, 4, "{}"); complete(2, 4) end,
    function() part(0, 1, 1, "{}"); complete(1, 1) end,
    function() part(0, 1, 2, "xx"); complete(1, 2) end,
    function() complete(1, 2) end,
    function() part(0, 1, -1, "{}") end,
    function() part(0, 257, 256, "x") end,
    function()
      local foreign = vim.json.encode({ id = request_id + 1, result = {} })
      part(0, 1, #foreign, foreign)
      complete(1, #foreign)
    end,
    function()
      local invalid = vim.json.encode({ id = request_id, error = "invalid" })
      part(0, 1, #invalid, invalid)
      complete(1, #invalid)
    end,
    function()
      part(0, 2, 4, "{}")
      emit({ id = request_id, result = {} })
    end,
  }
  for _, send_corrupt in ipairs(corrupt) do
    start()
    send_corrupt()
    settled()
    assert(errors == 1 and received == nil, "corrupt transfer reached a success callback")
  end

  start()
  part(0, 2, 4, "{}")
  wait_until(function() return client._client.transfer_bytes == 4 end, "first transfer part was not buffered")
  client.host_stop()
  settled()
  eq(errors, 1)

  start()
  part(0, 2, 4, "{}")
  emit({ id = request_id, error = { code = "cancelled", message = "cancelled" } })
  settled()
  eq(errors, 1)

  start()
  local identity_list = { request_id }
  for _ = 1, 2 do
    local previous_id = request_id
    client.request({ method = "github.issue.list", params = {} }, function(_, request_error)
      assert(request_error)
      calls, errors = calls + 1, errors + 1
    end)
    wait_until(function() return request_id ~= previous_id end, "concurrent transfer request was not written")
    identity_list[#identity_list + 1] = request_id
  end
  for _, identity in ipairs(identity_list) do part(0, 2, 4, "{}", identity) end
  wait_until(function() return calls == 3 end, "stopped transfers did not fail every request")
  assert(errors == 3 and client._client.transfer_bytes == 0 and vim.tbl_isempty(client._client.transfer))
  assert(events == 0 and vim.tbl_isempty(client._client.pending))
end

T["receive"] = function()
  local builder = require("forge.builder")
  local client = require("forge.client")
  local session = require("forge.session")
  local event_count, result, failure
  local event_total = 4095

  require("forge").setup({ harness = { provider = "mock" }, harness_logging = false })
  builder.ensure = function(callback) callback({ ok = true, path = "injected-forge" }) end
  client._host_set_launcher_for_test(function(_, options, on_exit)
    local published, acknowledged_frames, acknowledged_bytes = {}, 0, 0
    local function emit(message)
      local encoded = vim.json.encode(message) .. "\n"
      published[#published + 1] = #encoded
      options.stdout(nil, encoded)
    end
    return {
      write = function(_, payload)
        local request = vim.json.decode(payload)
        if request.method == "host.transport.consumed" then
          assert(request.params.frames > acknowledged_frames and request.params.frames <= #published)
          for index = acknowledged_frames + 1, request.params.frames do
            acknowledged_bytes = acknowledged_bytes + published[index]
          end
          acknowledged_frames = request.params.frames
          assert(request.params.bytes == acknowledged_bytes, "client credit byte total differs")
          return
        end
        if request.method == "host.initialize" then
          emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
        elseif request.method == "provider.registry.list" then
          emit({ id = request.id, result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() })
        elseif request.method == "session.initialize" then
          emit({ id = request.id, result = { session = { id = "receive-test" } } })
        elseif request.method == "host.trace.configure" then
          emit({ id = request.id, result = { level = request.params.level } })
        elseif request.method == "session.exchange.list" then
          for sequence = 1, event_total do
            emit({ session_id = "receive-test", event = "exchange_updated", payload = { sequence = sequence } })
          end
          emit({ id = request.id, result = { complete = true } })
          options.stderr(nil, string.rep("x", 100000))
          eq(#client._client.stderr, 65536)
          on_exit({ code = 0 })
        elseif request.method ~= "host.shutdown" then error("unexpected method: " .. request.method) end
      end,
      kill = function() end,
    }
  end)
  local started = false
  client.session_initialize(function(_, start_error) assert(not start_error, start_error) started = true end)
  wait_until(function() return started end, "receive fixture Harness did not start")
  event_count = 0
  client.session_events_subscribe(function(event, payload)
    if event == "exchange_updated" then
      event_count = event_count + 1
      eq(payload.sequence, event_count)
    end
  end)
  client.request({ session_id = "receive-test", method = "session.exchange.list", params = {} }, function(value, request_error)
    result, failure = value, request_error
    assert(event_count == event_total, "response overtook earlier events")
  end)
  wait_until(function() return result ~= nil or failure ~= nil end, "exchange list request did not complete")
  assert(not failure, failure)
  assert(result.complete and event_count == event_total)
  wait_until(function() return client._client.process == nil end, "receive fixture host process was not collected")
  assert(not client._client.ready)
  assert(vim.tbl_isempty(client._client.pending))
end

T["transport_failure"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local emitted, exited, request_id
  local launch_count = 0
  builder.ensure = function(callback) callback({ ok = true, path = "transport-fixture" }) end
  local diagnostics = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    launch_count = launch_count + 1
    emitted, exited = options.stdout, on_exit
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          emitted(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "provider.registry.list" then
          local registry = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry()
          emitted(nil, vim.json.encode({ id = request.id, result = registry }) .. "\n")
        elseif request.method == "session.initialize" then
          emitted(nil, vim.json.encode({ id = request.id, result = { initialized = true } }) .. "\n")
        elseif request.method == "host.trace.configure" then
          emitted(nil, vim.json.encode({ id = request.id, result = { level = "none" } }) .. "\n")
        elseif request.method == "session.state.get" then request_id = request.id end
      end,
      kill = function() end,
    }
  end)

  local events, stopped, unresolved = {}, false, false
  client.session_events_subscribe(function(event, payload, session_id)
    if event == "host_stopped" then stopped = true return end
    events[#events + 1] = { event = event, payload = payload, session_id = session_id }
  end)
  client.request({ session_id = "session", method = "session.state.get", params = {} }, function(_, failure, detail)
    assert(stopped, "pending callback ran before the UI learned that the host stopped")
    assert(failure:find("unresolved request", 1, true) and detail.code == "outcome_unknown")
    client.request({ session_id = "session", method = "session.state.get", params = {} }, function(_, retry_error)
      assert(retry_error == "Forge host is draining", "failure callback restarted the host")
    end)
    unresolved = true
  end)
  wait_until(function() return request_id ~= nil end, "state request was not written")
  local text = ('tool output "\\\n'):rep(50000)
  local encoded = vim.json.encode({ session_id = "session", event = "exchange_updated", payload = { text = text } })
  local count = math.ceil(#encoded / 100000)
  for sequence = 0, count - 1 do
    emitted(nil, vim.json.encode({ session_id = "session", event = "event.part", payload = {
      transfer_id = "1", part = { sequence = sequence, part_count = count, total_bytes = #encoded,
        payload = encoded:sub(sequence * 100000 + 1, (sequence + 1) * 100000) },
    } }) .. "\n")
  end
  wait_until(function() return client._client.transfer_bytes == #encoded end, "event transfer parts were not buffered")
  assert(#events == 0, "incomplete event reached a subscriber")
  emitted(nil, vim.json.encode({ session_id = "session", event = "event.complete", payload = {
    transfer_id = "1", part = { part_count = count, total_bytes = #encoded },
  } }) .. "\n")
  wait_until(function() return #events == 1 end, "completed event transfer did not reach the subscriber")
  assert(events[1].session_id == "session" and events[1].event == "exchange_updated" and events[1].payload.text == text)
  assert(client._client.transfer_bytes == 0 and vim.tbl_isempty(client._client.transfer))
  exited({ code = 1 })
  wait_until(function() return unresolved end, "unresolved request callback did not run after host exit")
  assert(launch_count == 1 and not client._client.process and vim.tbl_isempty(client._client.pending))
  assert(#diagnostics == 1 and diagnostics[1].message:find("exited 1", 1, true))
end

-- A host that exits while a frame is half written must still finish its exit path: pending callbacks
-- fail, subscribers learn of the stop, and the next start launches a fresh host. The receiver used to
-- route the truncated frame through `host_stop`, which drained a process that had already exited and
-- waited on a forced collection that cannot succeed. The forced-collection collaborators fail the way
-- they do for a dead process on each platform, so the case fails if recovery depends on them.
local function exit_inside_frame_recovers(windows)
  local client = require("forge.client")
  local builder = require("forge.builder")
  local stub = dofile("nvim/lua/forge/tests/support/stub.lua")
  local original_has = stub(vim.fn, "has", function(feature)
    if feature == "win32" then return windows and 1 or 0 end
    return original_has(feature)
  end)
  local original_system = stub(vim, "system", function(command, options, on_exit)
    if command[1] ~= "taskkill" then return original_system(command, options, on_exit) end
    vim.schedule(function() on_exit({ code = 128, stdout = "", stderr = "ERROR: process not found" }) end)
    return { kill = function() end }
  end)
  local emitted, exited
  local launch_count = 0
  builder.ensure = function(callback) callback({ ok = true, path = "truncated-frame-fixture" }) end
  local diagnostics = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  client._host_reset_for_test()
  client._host_set_launcher_for_test(function(_, options, on_exit)
    launch_count = launch_count + 1
    emitted, exited = options.stdout, on_exit
    return {
      pid = 4242,
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          emitted(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        end
      end,
      kill = function() error("process already exited") end,
      wait = function() error("process already exited") end,
    }
  end)
  local function start()
    local ready = false
    client.host_start(function(_, failure) assert(not failure, failure) ready = true end)
    wait_until(function() return ready end, "fixture host did not start")
    client._client.runtime_ready = true
    client._provider_registry_adopt_for_test(true)
  end
  start()
  local stopped = false
  client.session_events_subscribe(function(event) if event == "host_stopped" then stopped = true end end)
  local unresolved
  client.request({ method = "repository.status.document", params = {} }, function(_, failure, detail)
    unresolved = { failure = failure, detail = detail }
  end)
  wait_until(function() return client._client.pending[next(client._client.pending)].dispatched end,
    "request was not written to the fixture host")
  emitted(nil, '{"id":')
  exited({ code = 0 })
  wait_until(function() return unresolved ~= nil end, "pending callback did not run after the host exited mid-frame")
  assert(unresolved.failure:find("unresolved request", 1, true), unresolved.failure)
  eq(unresolved.detail.code, "outcome_unknown")
  assert(stopped, "subscribers did not receive host_stopped")
  assert(not client._client.draining and not client._client.process, "client stayed draining after the host exited")
  assert(diagnostics[1] and diagnostics[1].message:find("ended inside a frame", 1, true), vim.inspect(diagnostics))
  start()
  eq(launch_count, 2)
end

T["exit_inside_frame_windows"] = function() exit_inside_frame_recovers(true) end

T["exit_inside_frame_unix"] = function() exit_inside_frame_recovers(false) end

T["history_clear"] = function()
  local client = require("forge.client")
  local builder = require("forge.builder")
  local navigation = require("forge.views.harness.session_navigation")
  local session = require("forge.session")
  local stopped, process_complete
  local launch_count = 0
  local state = session.new_harness_state()
  state.session = { id = "history-fixture" }
  state.transcript_buf = buffer.scratch({ "saved transcript" })
  state.composer_buf = buffer.scratch({ "unsent draft" })
  state.prompt_history = { "old prompt" }
  local registry = navigation._registry_for_test()
  registry[state.session.id] = state
  session.register_harness(state.session.id, state)
  session.select_harness(state)

  builder.binary_path = function() return vim.v.progpath end
  local notice_list = notify.capture({ min_level = vim.log.levels.ERROR, notifications = true })
  client.host_stop = function(_, callback) stopped = callback end
  vim.system = function(command, options, callback)
    assert(command[2] == "history-clear" and command[3] == "--data-root")
    eq(command[4], vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"))
    assert(options.stdout and options.stderr)
    launch_count = launch_count + 1
    process_complete = callback
    return {}
  end

  local completed, clear_error
  client.history_clear(function(error) completed, clear_error = true, error end)
  assert(launch_count == 0, "history deletion started before host collection")
  local request_error
  client.host_start(function(_, error) request_error = error end)
  assert(request_error and request_error:find("history is being cleared", 1, true))
  client.request({ method = "repository.revision.list", params = {} }, function(_, error)
    request_error = error
  end)
  assert(request_error and request_error:find("history is being cleared", 1, true))
  stopped()
  assert(launch_count == 1 and not completed)
  process_complete({ code = 1, stderr = "history is in use" })
  wait_until(function() return completed end, "failed history clear did not complete")
  eq(clear_error, "history is in use")
  assert(state.session.id == "history-fixture", "failed clearing discarded live view identities")
  assert(#notice_list == 1 and notice_list[1].message:find("history is in use", 1, true))

  completed, clear_error = false, nil
  client.history_clear(function(error) completed, clear_error = true, error end)
  stopped()
  process_complete({ code = 1, stderr = "Some versions remain", stdout = vim.json.encode({
    current_cleared = true, removed = { "state/current" },
    retained = { { path = "state/v8", message = "locked by another host" } },
  }) })
  wait_until(function() return completed end, "partial history clear did not complete")
  assert(clear_error == "state/v8: locked by another host" and #notice_list == 2)
  assert(state.session == nil and vim.tbl_isempty(session.harness_by_id))
  eq(#state.prompt_history, 0)
  assert(buffer.line(state.transcript_buf, 1):find("History cleared", 1, true))
  eq(buffer.line(state.composer_buf, 1), "unsent draft")
  assert(not session.harness.session)
  completed, clear_error = false, nil
  client.history_clear(function(error) completed, clear_error = true, error end)
  stopped()
  process_complete({ code = 0, stderr = "", stdout = vim.json.encode({
    current_cleared = true, removed = {}, retained = {},
  }) })
  wait_until(function() return completed end, "successful history clear did not complete")
  assert(not clear_error and #notice_list == 2)

  local specification = dofile("nvim/lua/plugins/forge.lua")[1]
  assert(vim.tbl_contains(specification.cmd, "ForgeHistoryClear"), "history command is not lazy-loadable")
  specification.config(nil, { about_auto_generate = false })
  local command_called = false
  client.history_clear = function() command_called = true end
  vim.cmd("ForgeHistoryClear")
  assert(command_called, "ForgeHistoryClear bypassed the shared history owner")
end

T["provider_registry_config_reload"] = function()
  -- Replacing config.options through a repeated setup must not discard the provider registry the
  -- running host already returned; provider lookups must keep resolving registry descriptors.
  local client = require("forge.client")
  local builder = require("forge.builder")
  local config = require("forge.infra.config")
  local preference = require("forge.harness.provider_preference")
  local session = require("forge.session")
  local registry_fixture = dofile("nvim/lua/forge/tests/support/provider_catalog.lua")
  local registry_request_count = 0

  builder.ensure = function(callback) callback({ ok = true, path = "registry-reload-fixture" }) end
  preference.load = function(_, fallback) return fallback end

  ---@param _ string[]
  ---@param options table
  ---@return table
  local function launch_fixture(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.transport.consumed" or request.method == "host.shutdown" then return end
        local result = {}
        if request.method == "host.initialize" then
          result.protocol_version = require("forge.protocol").VERSION
        elseif request.method == "provider.registry.list" then
          registry_request_count = registry_request_count + 1
          result = registry_fixture.registry()
        elseif request.method == "host.trace.configure" then
          result.level = "none"
        end
        options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
      end,
      kill = function() end,
    }
  end

  ---@return ForgeTextGenerationConfig?, string?
  local function resolve_generation()
    local settled, generation, failure = false, nil, nil
    client.generation_config(function(value, request_failure)
      settled, generation, failure = true, value, request_failure
    end)
    wait_until(function() return settled end, "generation configuration never settled")
    return generation, failure
  end

  client._host_reset_for_test()
  client._host_set_launcher_for_test(launch_fixture)
  config.setup({ harness_logging = false })
  session.activate_harness(session.new_harness_state())
  local generation, first_failure = resolve_generation()
  assert(generation and not first_failure, first_failure)
  eq(registry_request_count, 1)

  config.setup({ harness_logging = false })
  session.harness.session = { id = "reload-session", provider = "fixture_beta", model = "chat-model" }
  generation, first_failure = resolve_generation()
  assert(not first_failure, "replaced configuration lost the host registry: " .. tostring(first_failure))
  assert(generation and generation.provider.kind == "fixture_beta")
  assert(vim.deep_equal(generation.provider.command, { "fixture-provider", "fixture_beta" }),
    "generation lost the registry provider command")
  assert(registry_request_count == 1, "re-adopting a retained registry must not repeat the host request")
  assert(client.provider_map().fixture_gamma, "synchronous provider lookup lost the registry")

  config.setup({ harness_logging = false, harness = { providers = { fixture_beta = { label = "Configured Beta" } } } })
  assert(client.provider_map().fixture_beta.label == "Configured Beta",
    "user provider overrides must win over registry descriptors")
  assert(client.provider_map().fixture_beta.command[2] == "fixture_beta",
    "user provider overrides must merge with registry descriptors")
end

return T
