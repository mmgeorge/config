vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local config = require("forge.infra.config")
config.setup({ harness = { backend = "mock" } })
builder.ensure = function(callback) callback({ ok = true, path = "generation-cancellation-fixture" }) end
local requests, respond = {}, nil
client._host_reset_for_test()
client._host_set_launcher_for_test(function(_, options, exited)
  respond = function(request, result)
    options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
  end
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "host.initialize" then
        respond(request, { protocol_version = require("forge.protocol").VERSION })
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
local completed
cancel = client.generation_request(arguments, function(result) completed = result end, nil, selection)
assert(vim.wait(1000, function() return #requests == 1 end, 5))
local generation = requests[1]
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
