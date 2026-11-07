vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local config = require("forge.infra.config")
config.setup({ harness = { provider = "mock" } })
builder.ensure = function(callback) callback({ ok = true, path = "generation-cancellation-fixture" }) end
local requests, respond, progress = {}, nil, nil
client._host_reset_for_test()
client._host_set_launcher_for_test(function(_, options, exited)
  respond = function(request, result)
    options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
  end
  progress = function(request, model)
    options.stdout(nil, vim.json.encode({ request_id = request.id, event = "repository.generation.progress",
      payload = { session_id = "isolated-generation", event = { kind = "runtime_resolved", data = { model = model } } } }) .. "\n")
  end
  return {
    write = function(_, encoded)
      ---@type {id: integer, method: string, params: table}
      local request = vim.json.decode(encoded)
      if request.method == "host.initialize" then
        respond(request, { protocol_version = require("forge.protocol").VERSION })
      elseif request.method == "provider.registry.list" then
        respond(request, dofile("nvim/tests/forge/support/provider_catalog.lua").registry())
      elseif request.method == "host.trace.configure" then respond(request, { enabled = false })
      elseif request.method == "repository.generation" then
        requests[#requests + 1] = request
        if request.params.operation == "cancel" then respond(request, { state = "none" }) end
      elseif request.method == "host.shutdown" then vim.schedule(function() exited({ code = 0 }) end) end
    end,
    kill = function() end,
  }
end)
local selection = { model = "mock-model" }
local arguments = { operation = "generate", workspace = "fixture", comparison = "staged" }
local cancelled
local cancel = client.generation_request(arguments, function(_, failure) cancelled = failure end, nil, selection)
cancel()
cancel()
assert(cancelled == "Background generation cancelled")
assert(vim.wait(1000, function() return client._client.ready end, 5))
assert(#requests == 0, "cancellation before dispatch sent a generation request")
local completed, labels = nil, {}
cancel = client.generation_request(arguments, function(result) completed = result end,
  function(label) labels[#labels + 1] = label end, selection)
assert(vim.wait(1000, function() return #requests == 1 end, 5))
local generation = requests[1]
assert(labels[1]:find("unconfirmed", 1, true), "configured model was presented as native confirmation")
assert(progress)(generation, "native-confirmed")
assert(vim.wait(1000, function() return labels[#labels] == "native-confirmed" end, 5))
assert(progress)(generation, vim.NIL)
assert(vim.wait(1000, function() return labels[#labels] == "mock-model (unconfirmed)" end, 5),
  "unknown native model did not clear isolated generation confirmation")
cancel()
cancel()
assert(vim.wait(1000, function() return #requests == 2 end, 5))
assert(requests[2].params.operation == "cancel")
assert(requests[2].params.request_id == generation.id)
respond(generation, { state = "none" })
assert(vim.wait(1000, function() return completed end, 5))
cancel()
assert(#requests == 2, "repeated cancellation sent another request")
client.host_stop()
assert(vim.wait(1000, function() return client._client.process == nil end, 5))
print("generation_request_cancel: queued and dispatched cancellation passed")
