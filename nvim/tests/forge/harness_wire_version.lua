vim.loader.enable(false)
local builder = require("forge.builder")
local client = require("forge.client")
local session = require("forge.session")
local original_ensure = builder.ensure

local ok, failure = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" }, harness_logging = false })
  builder.ensure = function(callback) callback({ ok = true, path = "injected-forge" }) end
  for _, version in ipairs({ 0, 2, "3", false }) do
    client._reset_for_test()
    local initialize_requests, shutdown_requests, method_list = 0, 0, {}
    client._set_launcher_for_test(function(_, options)
      return {
        write = function(_, payload)
          local request = vim.json.decode(payload)
          method_list[#method_list + 1] = request.method
          if request.method == "shutdown" then shutdown_requests = shutdown_requests + 1 return end
          if request.method == "transport.consumed" then return end
          initialize_requests = initialize_requests + 1
          assert(request.method == "initialize" and request.params.protocol_version == 3)
          local result = { session = { id = "mismatched" } }
          if version ~= false then result.protocol_version = version end
          options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
        end,
        kill = function() end,
      }
    end)
    local completed, received_error, completion_count = false, nil, 0
    client.start_harness(function(result, error)
      completion_count = completion_count + 1
      assert(not result)
      received_error = error
      completed = true
    end)
    assert(vim.wait(1000, function() return completed end, 1))
    assert(received_error:find("wire versions differ", 1, true))
    assert(completion_count == 1, "wire mismatch completed startup more than once")
    assert(initialize_requests == 1 and shutdown_requests == 1, "client admitted work after mismatched initialization: " .. vim.inspect(method_list))
    assert(not client._client.ready and client._client.draining)
    assert(vim.tbl_isempty(client._client.pending))
  end
end, debug.traceback)

builder.ensure = original_ensure
client._reset_for_test()
if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
