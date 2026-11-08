-- Unit tests for `forge.client` (client.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "admission_queue", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure, original_generation_config = builder.ensure, client.generation_config
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
local function await(predicate)
  assert(vim.wait(1000, predicate, 1), "admission fixture timed out")
end
local function record(label)
  return function(result, failure, detail)
    assert(not results[label], "request completed twice: " .. label)
    results[label] = { result = result, failure = failure, detail = detail }
  end
end
local function start()
  local ready = false
  client.host_start(function(_, failure) assert(not failure) ready = true end)
  await(function() return ready end)
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
local succeeded, failure = xpcall(function()
  start()
  for index = 1, 252 do ordinary("ordinary-" .. index) end
  await(function() return #writes == 252 end)
  local params = { label = "queued-one", nested = { retained = true } }
  ordinary("queued-one", params)
  params.label, params.nested.retained = "changed", false
  ordinary("queued-two")
  for index = 1, 4 do control("control-" .. index) end
  await(function() return #writes == 256 end)
  assert(not next(results), "capacity rejected a queued request")
  control("control-five")
  respond(writes[1])
  await(function() return #writes == 257 end)
  assert(writes[257].params.label == "control-five", "ordinary backlog blocked reserved control progress")
  respond(writes[2])
  await(function() return #writes == 258 end)
  assert(writes[258].params.label == "queued-one" and writes[258].params.nested.retained,
    "queued request lost its original payload or FIFO identity")
  respond(writes[258])
  await(function() return #writes == 259 end)
  assert(writes[259].params.label == "queued-two")
  respond(writes[259])
  await(function() return results["queued-one"] and results["queued-two"] end)
  assert(results["queued-one"].result.label == "queued-one" and results["queued-two"].result.label == "queued-two",
    "queued replies lost callback association")
  ordinary("refill")
  await(function() return #writes == 260 end)
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
  await(function() return not client._client.process end)
  assert(#writes == 260, "cancelled or shutdown queue entries reached the host")
  assert(results["refill"].detail.code == "outcome_unknown", "dispatched request lost unknown-outcome evidence")
  start()
  ordinary("new-generation")
  await(function() return #writes == 261 end)
  assert(writes[261].params.label == "new-generation", "old queued work replayed on a new host")
  respond(writes[261])
  await(function() return results["new-generation"] end)
  local offset = #writes
  for index = 1, 252 do ordinary("reentrant-ordinary-" .. index) end
  for index = 1, 4 do control("reentrant-active-" .. index) end
  await(function() return #writes == offset + 256 end)
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
  await(function()
    for index = offset + 257, #writes do
      if writes[index].params.label == "reentrant-queued-1" then return true end
    end
  end)
  client.host_stop("reentrant fixture complete")
  await(function() return not client._client.process end)
  start()
  offset = #writes
  for index = 1, 252 do ordinary("startup-ordinary-" .. index) end
  for index = 1, 4 do control("startup-active-" .. index) end
  await(function() return #writes == offset + 256 end)
  client._client.runtime_ready = false
  client._provider_registry_adopt_for_test(false)
  control("startup-waiting")
  respond(writes[offset + 1])
  await(function() return #writes == offset + 257 end)
  assert(writes[#writes].method == "provider.registry.list", "full transport starved runtime startup registry")
  emit({ id = writes[#writes].id, result = dofile("nvim/lua/forge/tests/support/provider_catalog.lua").registry() })
  await(function() return #writes == offset + 258 end)
  assert(writes[#writes].method == "session.initialize", "full transport starved runtime initialization")
  emit({ id = writes[#writes].id, result = { initialized = true } })
  await(function() return #writes == offset + 259 end)
  assert(writes[#writes].method == "host.trace.configure")
  emit({ id = writes[#writes].id, result = { enabled = false } })
  await(function() return #writes == offset + 260 end)
  assert(writes[#writes].params.label == "startup-waiting", "initialized request never reached transport")
  respond(writes[#writes])
  await(function() return results["startup-waiting"] end)
end, debug.traceback)
client._host_reset_for_test()
builder.ensure, client.generation_config = original_ensure, original_generation_config
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_admission_queue: passed")
vim.cmd("qa!")
  end },
  { name = "callback_isolation", run = function()
local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")
local original_ensure, original_error = builder.ensure, notifications.error
local diagnostic, sent = {}, {}
local emit
local initializing_harness = false
local response_version = require("forge.protocol").VERSION
builder.ensure = function(callback) callback({ ok = true, path = "callback-fixture" }) end
notifications.error = function(message) diagnostic[#diagnostic + 1] = tostring(message) end
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
      if request.method == "host.trace.configure" then emit({ id = request.id, result = { enabled = false } }) end
      if request.method == "host.shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
    end,
    kill = function() end,
  }
end)

local function await(predicate, message)
  assert(vim.wait(1000, predicate, 5), message)
end

local succeeded, failure = xpcall(function()
  local started = false
  client.host_start(function() error("startup UI fixture failure") end)
  client.host_start(function(_, start_error) assert(not start_error) started = true end)
  await(function() return started end, "throwing startup consumer skipped another waiter")
  assert(client._client.ready and not client._client.draining)

  local completed = false
  client.request({ method = "provider.registry.list", params = {} }, function() error("reply UI fixture failure") end)
  client.request({ method = "repository.status.document", params = {} }, function(result) completed = result.ok end)
  await(function() return sent["provider.registry.list"] and sent["repository.status.document"] end, "requests were not written")
  emit({ id = sent["provider.registry.list"], result = { document = "policy" } })
  emit({ id = sent["repository.status.document"], result = { ok = true } })
  await(function() return completed end, "throwing response consumer skipped independent reply")
  assert(client._client.ready and client._client.process and not client._client.draining)
  assert(vim.tbl_isempty(client._client.pending))
  assert(#diagnostic == 2 and diagnostic[1]:find("startup UI fixture failure", 1, true)
    and diagnostic[2]:find("reply UI fixture failure", 1, true), "consumer failure diagnostics were lost")

  sent["provider.registry.list"] = nil
  client.request({ method = "provider.registry.list", params = {} }, function() error("error UI fixture failure") end)
  await(function() return sent["provider.registry.list"] end, "error request was not written")
  emit({ id = sent["provider.registry.list"], error = { code = "fixture", message = "expected request error" } })
  await(function() return #diagnostic == 3 end, "error callback exception was not reported")
  assert(client._client.ready and not client._client.draining)

  local harness_started = false
  initializing_harness = true
  client.session_initialize(function() error("Harness startup UI fixture failure") end)
  client.session_initialize(function(_, start_error) assert(not start_error) harness_started = true end)
  await(function() return harness_started end, "throwing Harness startup consumer skipped another waiter")
  local requesting_state = session.new_harness_state()
  local viewing_state = session.new_harness_state()
  session.activate_harness(requesting_state)
  session.register_harness("fixture", requesting_state)
  client.request({ session_id = "fixture", method = "session.state.get", params = {} }, function()
    assert(session.harness == requesting_state)
    error("scoped UI fixture failure")
  end)
  session.activate_harness(viewing_state)
  await(function() return sent["session.state.get"] end, "scoped request was not written")
  emit({ id = sent["session.state.get"], result = {} })
  await(function() return #diagnostic == 5 end, "scoped consumer failure was not reported")
  assert(session.harness == viewing_state, "throwing scoped consumer changed active Harness state")
  assert(client._client.ready and not client._client.draining)

  client.request({ method = "github.issue.list", params = {} }, function() end)
  await(function() return sent["github.issue.list"] end, "malformed transfer request was not written")
  emit({ request_id = sent["github.issue.list"], event = "result.complete", payload = { part_count = 1, total_bytes = 1 } })
  await(function() return not client._client.ready end, "malformed protocol stopped being fatal")
  await(function() return not client._client.process end, "malformed protocol host did not drain")
  response_version = 999
  local mismatch_reported = false
  client.host_start(function(_, start_error)
    assert(start_error:find("wire versions differ", 1, true))
    mismatch_reported = true
    error("mismatched startup UI fixture failure")
  end)
  await(function() return mismatch_reported and not client._client.process end,
    "consumer failure prevented initialization mismatch shutdown")
end, debug.traceback)
client._host_reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client callback isolation passed")
vim.cmd("qa!")
  end },
  { name = "executable_copy", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local builder = require("forge.builder")
local client = require("forge.client")
local original_ensure, original_acquire, original_system = builder.ensure, builder.acquire, vim.system
---@type fun(lease?: RustSidecarLease, failure?: string)?
local complete_copy
local released, spawned, completed = 0, 0, 0
local succeeded, failure = xpcall(function()
  builder.ensure = function(callback) callback({ ok = true, path = "fixture" }) end
  builder.acquire = function(_, callback) complete_copy = callback end
  vim.system = function() spawned = spawned + 1 error("cancelled startup spawned a process") end
  client.host_start(function(_, start_error)
    assert(start_error, "cancelled startup succeeded")
    completed = completed + 1
  end)
  assert(vim.wait(1000, function() return complete_copy ~= nil end, 5))
  client.host_stop()
  complete_copy({ path = "copy", release = function() released = released + 1 end })
  assert(released == 1 and spawned == 0 and completed == 1, "cancelled copy lost lifecycle ownership")
  client._host_reset_for_test()
  complete_copy = nil
  local copy_failure
  client.host_start(function(_, start_error) copy_failure = start_error end)
  assert(vim.wait(1000, function() return complete_copy ~= nil end, 5))
  complete_copy(nil, "copy denied")
  assert(copy_failure == "copy denied" and spawned == 0, "copy failure did not reach startup caller")
  client._host_reset_for_test()
  complete_copy = nil
  vim.system = function() error("fixture spawn failure") end
  local spawn_failure
  client.host_start(function(_, start_error) spawn_failure = start_error end)
  assert(vim.wait(1000, function() return complete_copy ~= nil end, 5))
  complete_copy({ path = "copy", release = function() released = released + 1 end })
  assert(spawn_failure and spawn_failure:find("fixture spawn failure", 1, true))
  assert(released == 2, "spawn failure retained executable copy")
end, debug.traceback)
builder.ensure, builder.acquire, vim.system = original_ensure, original_acquire, original_system
client._host_reset_for_test()
if not succeeded then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("executable copy lifecycle passed")
vim.cmd("qa!")
  end },
  { name = "exit_admission", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
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

local success, failure = xpcall(function()
  local initialized = false
  client.host_start(function(_, problem) assert(not problem, problem) initialized = true end)
  assert(vim.wait(1000, function() return initialized end), "host did not initialize")
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
end, debug.traceback)
client._host_reset_for_test()
builder.ensure = original_ensure
if not success then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_exit_admission: passed")
vim.cmd("qa!")
  end },
  { name = "request_contract", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local protocol = require("forge.protocol")
local session = require("forge.session")
local original_ensure = builder.ensure
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
      elseif request.method == "host.trace.configure" then respond(request, { enabled = false })
      elseif request.method == "session.resume" then respond(request, { session = { id = request.session_id } })
      elseif request.method == "host.shutdown" then vim.schedule(function() exited({ code = 0 }) end) end
    end,
    kill = function() end,
  }
end)

local function await(predicate)
  assert(vim.wait(1000, predicate, 5), "request contract fixture timed out: " .. vim.inspect(requests))
end
local function last_request(method)
  for index = #requests, 1, -1 do
    if requests[index].method == method then return requests[index] end
  end
end

local succeeded, failure = xpcall(function()
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
  await(function() return last_request("permissions.document.open") end)
  assert(not initialize_request and not client._client.harness_ready, "shared request opened a session")
  respond(last_request("permissions.document.open"), { path = "permissions.json", source = "{}" })
  await(function() return shared_complete end)

  local first_session, second_session
  client.session_initialize(function(result) first_session = result.session.id end, { session_id = "first" })
  client.session_initialize(function(result) second_session = result.session.id end, { session_id = "second" })
  await(function() return initialize_request end)
  assert(initialize_request.params.session_id == "first")
  respond(initialize_request, { session = { id = "first" } })
  await(function() return first_session and second_session end)
  assert(first_session == "first" and second_session == "second", "startup reused another caller's session")

  local origin = session.new_harness_state()
  local active = session.new_harness_state()
  session.register_harness("origin", origin)
  session.activate_harness(active)
  local tracing_complete = false
  client.request({ method = "host.trace.configure", session_id = "origin", params = { enabled = true } }, function()
    assert(session.harness == origin, "trace callback lost its explicit session context")
    session.activate_harness(origin)
    tracing_complete = true
  end)
  await(function() return tracing_complete end)
  assert(session.harness == active, "temporary rendering activation stole the selected session")
  assert(last_request("host.trace.configure").session_id == "origin")
  local order = {}
  client.request({ method = "session.state.get", session_id = "origin" }, function(result)
    assert(session.harness == origin and result.session.id == "origin")
    order[#order + 1] = "session"
  end)
  client.request({ method = "repository.revision.list" }, function()
    assert(session.harness == active)
    order[#order + 1] = "repository"
  end)
  await(function() return last_request("session.state.get") and last_request("repository.revision.list") end)
  respond(last_request("repository.revision.list"), {})
  respond(last_request("session.state.get"), { session = { id = "origin" } })
  await(function() return #order == 2 end)
  assert(order[1] == "repository" and order[2] == "session")
  assert(session.harness == active and vim.tbl_isempty(client._client.pending))
end, debug.traceback)
client._host_reset_for_test()
builder.ensure = original_ensure
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_request_contract: passed")
vim.cmd("qa!")
  end },
  { name = "startup_cancellation", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
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
local success, failure = xpcall(function()
  for _ = 1, 2 do
    client.session_initialize(function(result, message)
      assert(result == nil and message, "cancelled startup reported success")
      failed = failed + 1
    end, { services_only = true })
  end
  assert(vim.wait(1000, function() return initialized end, 5), "initialization was not sent")
  client.host_stop()
  assert(vim.wait(1000, function() return client._client.process == nil end, 5), "host did not stop")
  assert(failed == 2, "concurrent runtime startup lost a completion callback: " .. failed)
  assert(not client._client.runtime_starting and not client._client.harness_starting)
  assert(vim.tbl_isempty(client._client.runtime_initialize_callback))
  assert(vim.tbl_isempty(client._client.pending))
end, debug.traceback)
client._host_reset_for_test()
builder.ensure = original_ensure
if not success then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client_startup_cancellation: passed")
vim.cmd("qa!")
  end },
  { name = "status_document_events", run = function()
vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local original_ensure, original_error = builder.ensure, notifications.error
local notices, received = {}, {}
local emit
client._host_reset_for_test()
builder.ensure = function(callback) callback({ ok = true, path = "document-event-fixture" }) end
notifications.error = function(message) notices[#notices + 1] = tostring(message) end
client._host_set_launcher_for_test(function(_, options, on_exit)
  emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
  return { write = function(_, encoded)
    local request = vim.json.decode(encoded)
    if request.method == "host.initialize" then emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) end
    if request.method == "repository.write" then emit({ id = request.id, result = {} }) end
    if request.method == "host.shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
  end, kill = function() end }
end)

---@param predicate fun(): boolean
local function await(predicate)
  assert(vim.wait(1000, predicate, 5), "document event did not arrive")
end

local ok, failure = xpcall(function()
  local ready = false
  client.host_start(function(_, failure) assert(not failure) ready = true end)
  await(function() return ready end)
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
  await(function() return client._client.transfer["document:status"] ~= nil end)
  assert(#received == 0, "partial update reached a document consumer")
  emit({ document = "status", event = "document.complete", payload = { part_count = part_count, total_bytes = #encoded } })
  await(function() return #received == 1 end)
  assert(received[1].payload.content == update.payload.content and received[1].generation == client._client.generation)
  assert(client._client.transfer_bytes == 0)
  update.payload = { operation_id = 0, phase = "settled", diagnostic = { "write rejected" } }
  emit(update)
  emit(update)
  await(function() return #received == 3 end)
  assert(#notices == 1 and notices[1] == "write rejected", "operation zero lost or duplicated its diagnostic")
  emit({ document = "status", event = "document.complete", payload = { part_count = 1, total_bytes = 1 } })
  await(function() return #received == 4 end)
  assert(received[4].event == "status.resync" and #notices == 2 and client._client.ready)
  unsubscribe()
  emit(update)
  await(function() return client._client.pending and next(client._client.pending) == nil end)
  assert(#received == 4, "closed document received another event")
end, debug.traceback)
client._host_reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not ok then error(failure) end
print("status document events: passed")
  end },
  { name = "client", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local notifications = require("forge.infra.notifications")
local original_error, original_echo = notifications.error, vim.api.nvim_echo
local launch_count = 0
---@type fun(result: table)?
local finish_build

local ok, failure = xpcall(function()
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
    assert(params.session_id == "leased-session")
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
      assert(recovery_options.lease_conflict_action == "new")
    end
    assert(recovery_method == (action == "fork" and "session.fork" or nil))
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
  assert(vim.tbl_count(client._client.pending) == 65)
  local control_completed = 0
  for _, method in ipairs({ "session.command.apply", "session.command.operation.get", "session.health.get" }) do
    client.request({ session_id = "admission", method = method, params = {} }, function(_, request_error)
      assert(request_error and not request_error:find("admission is full", 1, true))
      control_completed = control_completed + 1
    end)
  end
  assert(vim.tbl_count(client._client.pending) == 68, "ordinary requests consumed task control capacity")
  client.host_stop()
  assert(control_completed == 3)
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
  assert(vim.wait(1000, function() return transport_completed end, 1), "initial transport request did not complete")
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
  assert(vim.wait(1000, function() return accepted == 60 end, 1), "full ordinary admission prevented startup control traffic")
  assert(vim.tbl_isempty(client._client.pending))
  local events, progress, finished = 0, 0, false
  local unsubscribe = client.session_events_subscribe(function() events = events + 1 end)
  client.request({ method = "github.issue.sync", params = {} }, function(result, request_error)
    assert(not request_error, request_error)
    assert(result.fetched == 1)
    finished = true
  end, function(payload)
    assert(payload.phase == "reading")
    assert(not finished, "progress arrived after completion")
    progress = progress + 1
  end)
  assert(vim.wait(1000, function() return finished end, 1))
  assert(progress == 1 and events == 0, "request progress escaped into Harness subscribers or survived completion")
  local failed = false
  client.request({ method = "github.issue.list", params = {} }, function(_, request_error)
    assert(request_error == "storage is busy")
    failed = true
  end)
  assert(vim.wait(1000, function() return failed end, 1))
  assert(events == 0, "host storage failure invalidated Harness state")
  for _, method in ipairs({ "github.comment", "github.review.document.open", "github.review.document.input.apply", "github.review.document.save", "github.review.document.reconcile", "github.review.document.close" }) do
    local completed = false
    client.request({ method = method, params = {} }, function(_, request_error)
      assert(request_error == "review operation failed")
      completed = true
    end)
    assert(vim.wait(1000, function() return completed end, 1))
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
  assert(vim.wait(1000, function() return completed_after_failure end, 1))
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
          assert(request.params.recovery_directory == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "recovery", "github", "v1"))
          assert(request.params.status_ignored_directory == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "status-ignored"))
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
  assert(vim.wait(1000, function() return held_id ~= nil end, 1))
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
  assert(rejected_drain == "Forge host is draining")
  local previous_credits = credits
  output(nil, vim.json.encode({ id = held_id, result = { posted = true } }) .. "\n")
  assert(vim.wait(1000, function() return settled ~= nil end, 1))
  assert(settled.posted and credits > previous_credits and killed == 0)
  collected({ code = 0 })
  assert(vim.wait(1000, function() return not client._client.draining end, 1))
  assert(client._client.process == nil and vim.tbl_isempty(client._client.pending))
  assert(stopped, "shutdown completion must run after the host is collected")
end, debug.traceback)

builder.ensure = original_ensure
notifications.error, vim.api.nvim_echo = original_error, original_echo
client._host_reset_for_test()
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
  end },
  { name = "result_transfer", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local original_ensure, original_error = builder.ensure, notifications.error
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
  assert(vim.wait(1000, function() return request_id ~= nil end, 1))
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
  assert(vim.wait(1000, function() return calls == 1 end, 1))
  assert(client._client.transfer_bytes == 0 and vim.tbl_isempty(client._client.transfer))
  assert(vim.tbl_isempty(client._client.pending))
  assert(events == 0, "result transfer escaped into Harness subscribers")
end

local ok, failure = xpcall(function()
  start("github.review.document.open")
  local body = string.rep('日本語 "quote" \\ newline\n', 900000)
  local encoded = vim.json.encode({ id = request_id, result = { body = body } })
  local chunk = 100000
  local count = math.ceil(#encoded / chunk)
  assert(#encoded > 16 * 1024 * 1024 and count > 256)
  for sequence = 0, count - 1 do
    part(sequence, count, #encoded, encoded:sub(sequence * chunk + 1, (sequence + 1) * chunk))
    assert(vim.wait(1000, function()
      local transfer = client._client.transfer[request_id]
      return transfer and transfer.sequence == sequence + 1
    end, 1))
  end
  assert(vim.wait(1000, function()
    local transfer = client._client.transfer[request_id]
    return transfer and transfer.sequence == count
  end, 1))
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
  assert(vim.wait(1000, function() return client._client.transfer_bytes == 4 end, 1))
  client.host_stop()
  settled()
  assert(errors == 1)

  start()
  part(0, 2, 4, "{}")
  emit({ id = request_id, error = { code = "cancelled", message = "cancelled" } })
  settled()
  assert(errors == 1)

  start()
  local identity_list = { request_id }
  for _ = 1, 2 do
    local previous_id = request_id
    client.request({ method = "github.issue.list", params = {} }, function(_, request_error)
      assert(request_error)
      calls, errors = calls + 1, errors + 1
    end)
    assert(vim.wait(1000, function() return request_id ~= previous_id end, 1))
    identity_list[#identity_list + 1] = request_id
  end
  for _, identity in ipairs(identity_list) do part(0, 2, 4, "{}", identity) end
  assert(vim.wait(1000, function() return calls == 3 end, 1))
  assert(errors == 3 and client._client.transfer_bytes == 0 and vim.tbl_isempty(client._client.transfer))
  assert(events == 0 and vim.tbl_isempty(client._client.pending))
end, debug.traceback)

client._host_reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
  end },
  { name = "receive", run = function()
vim.loader.enable(false)
local builder = require("forge.builder")
local client = require("forge.client")
local session = require("forge.session")
local original_ensure = builder.ensure
local event_count, result, failure
local event_total = 4095

local ok, error_message = xpcall(function()
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
          emit({ id = request.id, result = { enabled = request.params.enabled } })
        elseif request.method == "session.exchange.list" then
          for sequence = 1, event_total do
            emit({ session_id = "receive-test", event = "exchange_updated", payload = { sequence = sequence } })
          end
          emit({ id = request.id, result = { complete = true } })
          options.stderr(nil, string.rep("x", 100000))
          assert(#client._client.stderr == 65536)
          on_exit({ code = 0 })
        elseif request.method ~= "host.shutdown" then error("unexpected method: " .. request.method) end
      end,
      kill = function() end,
    }
  end)
  local started = false
  client.session_initialize(function(_, start_error) assert(not start_error, start_error) started = true end)
  assert(vim.wait(1000, function() return started end, 1))
  event_count = 0
  client.session_events_subscribe(function(event, payload)
    if event == "exchange_updated" then
      event_count = event_count + 1
      assert(payload.sequence == event_count)
    end
  end)
  client.request({ session_id = "receive-test", method = "session.exchange.list", params = {} }, function(value, request_error)
    result, failure = value, request_error
    assert(event_count == event_total, "response overtook earlier events")
  end)
  assert(vim.wait(1000, function() return result ~= nil or failure ~= nil end, 1))
  assert(not failure, failure)
  assert(result.complete and event_count == event_total)
  assert(vim.wait(1000, function() return client._client.process == nil end, 1))
  assert(not client._client.ready)
  assert(vim.tbl_isempty(client._client.pending))
end, debug.traceback)

builder.ensure = original_ensure
client._host_reset_for_test()
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
  end },
  { name = "transport_failure", run = function()
vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local emitted, exited, request_id
local launch_count = 0
local diagnostics = {}
builder.ensure = function(callback) callback({ ok = true, path = "transport-fixture" }) end
notifications.error = function(message) diagnostics[#diagnostics + 1] = message end
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
        emitted(nil, vim.json.encode({ id = request.id, result = { enabled = false } }) .. "\n")
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
assert(vim.wait(1000, function() return request_id ~= nil end, 5))
local text = ('tool output "\\\n'):rep(50000)
local encoded = vim.json.encode({ session_id = "session", event = "exchange_updated", payload = { text = text } })
local count = math.ceil(#encoded / 100000)
for sequence = 0, count - 1 do
  emitted(nil, vim.json.encode({ session_id = "session", event = "event.part", payload = {
    transfer_id = "1", part = { sequence = sequence, part_count = count, total_bytes = #encoded,
      payload = encoded:sub(sequence * 100000 + 1, (sequence + 1) * 100000) },
  } }) .. "\n")
end
assert(vim.wait(1000, function() return client._client.transfer_bytes == #encoded end, 5))
assert(#events == 0, "incomplete event reached a subscriber")
emitted(nil, vim.json.encode({ session_id = "session", event = "event.complete", payload = {
  transfer_id = "1", part = { part_count = count, total_bytes = #encoded },
} }) .. "\n")
assert(vim.wait(1000, function() return #events == 1 end, 5))
assert(events[1].session_id == "session" and events[1].event == "exchange_updated" and events[1].payload.text == text)
assert(client._client.transfer_bytes == 0 and vim.tbl_isempty(client._client.transfer))
exited({ code = 1 })
assert(vim.wait(1000, function() return unresolved end, 5))
assert(launch_count == 1 and not client._client.process and vim.tbl_isempty(client._client.pending))
assert(#diagnostics == 1 and diagnostics[1]:find("exited 1", 1, true))
client._host_reset_for_test()
print("harness_transport_failure: passed")
vim.cmd("qa!")
  end },
  { name = "history_clear", run = function()
vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local navigation = require("forge.views.harness.session_navigation")
local session = require("forge.session")
local notifications = require("forge.infra.notifications")
local original_stop, original_system = client.host_stop, vim.system
local original_path, original_error = builder.binary_path, notifications.error
local stopped, process_complete
local launch_count, notice_list = 0, {}
local state = session.new_harness_state()
state.session = { id = "history-fixture" }
state.transcript_buf = vim.api.nvim_create_buf(false, true)
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.prompt_history = { "old prompt" }
vim.api.nvim_buf_set_lines(state.transcript_buf, 0, -1, false, { "saved transcript" })
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "unsent draft" })
local registry = navigation._registry_for_test()
registry[state.session.id] = state
session.register_harness(state.session.id, state)
session.select_harness(state)

local ok, failure = xpcall(function()
  builder.binary_path = function() return vim.v.progpath end
  notifications.error = function(message) notice_list[#notice_list + 1] = message end
  client.host_stop = function(_, callback) stopped = callback end
  vim.system = function(command, options, callback)
    assert(command[2] == "history-clear" and command[3] == "--data-root")
    assert(command[4] == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"))
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
  assert(vim.wait(1000, function() return completed end))
  assert(clear_error == "history is in use")
  assert(state.session.id == "history-fixture", "failed clearing discarded live view identities")
  assert(#notice_list == 1 and notice_list[1]:find("history is in use", 1, true))

  completed, clear_error = false, nil
  client.history_clear(function(error) completed, clear_error = true, error end)
  stopped()
  process_complete({ code = 1, stderr = "Some versions remain", stdout = vim.json.encode({
    current_cleared = true, removed = { "state/current" },
    retained = { { path = "state/v8", message = "locked by another host" } },
  }) })
  assert(vim.wait(1000, function() return completed end))
  assert(clear_error == "state/v8: locked by another host" and #notice_list == 2)
  assert(state.session == nil and vim.tbl_isempty(session.harness_by_id))
  assert(#state.prompt_history == 0)
  assert(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)[1]:find("History cleared", 1, true))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "unsent draft")
  assert(not session.harness.session)
  completed, clear_error = false, nil
  client.history_clear(function(error) completed, clear_error = true, error end)
  stopped()
  process_complete({ code = 0, stderr = "", stdout = vim.json.encode({
    current_cleared = true, removed = {}, retained = {},
  }) })
  assert(vim.wait(1000, function() return completed end))
  assert(not clear_error and #notice_list == 2)

  local specification = dofile("nvim/lua/plugins/forge.lua")[1]
  assert(vim.tbl_contains(specification.cmd, "ForgeHistoryClear"), "history command is not lazy-loadable")
  specification.config(nil, { about_auto_generate = false })
  local command_called = false
  local original_clear = client.history_clear
  client.history_clear = function() command_called = true end
  vim.cmd("ForgeHistoryClear")
  client.history_clear = original_clear
  assert(command_called, "ForgeHistoryClear bypassed the shared history owner")
end, debug.traceback)

client.host_stop, vim.system = original_stop, original_system
builder.binary_path, notifications.error = original_path, original_error
vim.api.nvim_buf_delete(state.transcript_buf, { force = true })
vim.api.nvim_buf_delete(state.composer_buf, { force = true })
assert(ok, failure)
print("Forge history clearing passed")
  end },
  { name = "provider_registry_config_reload", run = function()
vim.loader.enable(false)

-- Replacing config.options through a repeated setup must not discard the provider registry the
-- running host already returned; provider lookups must keep resolving registry descriptors.
local client = require("forge.client")
local builder = require("forge.builder")
local config = require("forge.infra.config")
local preference = require("forge.harness.provider_preference")
local session = require("forge.session")
local original_ensure, original_load = builder.ensure, preference.load
local original_options, original_explicit = config.options, config.harness_provider_explicit
local original_state = session.harness
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
        result.enabled = false
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
  assert(vim.wait(1000, function() return settled end, 5), "generation configuration never settled")
  return generation, failure
end

local succeeded, failure = xpcall(function()
  client._host_reset_for_test()
  client._host_set_launcher_for_test(launch_fixture)
  config.setup({ harness_logging = false })
  session.activate_harness(session.new_harness_state())
  local generation, first_failure = resolve_generation()
  assert(generation and not first_failure, first_failure)
  assert(registry_request_count == 1)

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
end, debug.traceback)

client._host_reset_for_test()
builder.ensure, preference.load = original_ensure, original_load
config.options, config.harness_provider_explicit = original_options, original_explicit
session.activate_harness(original_state)
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("provider registry survives configuration replacement passed")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
