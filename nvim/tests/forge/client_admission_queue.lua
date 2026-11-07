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
  emit({ id = registry_request.id, result = dofile("nvim/tests/forge/support/provider_catalog.lua").registry() })
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
  emit({ id = writes[#writes].id, result = dofile("nvim/tests/forge/support/provider_catalog.lua").registry() })
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
