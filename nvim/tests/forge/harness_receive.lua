vim.loader.enable(false)
local builder = require("forge.builder")
local client = require("forge.client")
local session = require("forge.session")
local original_ensure = builder.ensure
local event_count, result, failure

local ok, error_message = xpcall(function()
  require("forge").setup({ harness = { backend = "mock" }, harness_logging = false })
  builder.ensure = function(callback) callback({ ok = true, path = "injected-forge" }) end
  client._set_launcher_for_test(function(_, options, on_exit)
    local published, acknowledged_frames, acknowledged_bytes = {}, 0, 0
    local function emit(message)
      local encoded = vim.json.encode(message) .. "\n"
      published[#published + 1] = #encoded
      options.stdout(nil, encoded)
    end
    return {
      write = function(_, payload)
        local request = vim.json.decode(payload)
        if request.method == "transport.consumed" then
          assert(request.params.frames > acknowledged_frames and request.params.frames <= #published)
          for index = acknowledged_frames + 1, request.params.frames do
            acknowledged_bytes = acknowledged_bytes + published[index]
          end
          acknowledged_frames = request.params.frames
          assert(request.params.bytes == acknowledged_bytes, "client credit byte total differs")
          return
        end
        if request.method == "initialize" then
          emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
        elseif request.method == "harness.initialize" then
          emit({ id = request.id, result = { session = { id = "receive-test" } } })
        elseif request.method == "plan.scope_deviation_review" then
          emit({ id = request.id, result = {} })
        elseif request.method == "receive.test" then
          for sequence = 1, 40 do
            emit({ session_id = "receive-test", event = "receive_test", payload = { sequence = sequence } })
          end
          emit({ id = request.id, result = { complete = true } })
          options.stderr(nil, string.rep("x", 100000))
          assert(#client._client.stderr == 65536)
          on_exit({ code = 0 })
        elseif request.method ~= "shutdown" then error("unexpected method: " .. request.method) end
      end,
      kill = function() end,
    }
  end)
  local started = false
  client.start_harness(function(_, start_error) assert(not start_error, start_error) started = true end)
  assert(vim.wait(1000, function() return started end, 1))
  event_count = 0
  client.subscribe(function(event, payload)
    if event == "receive_test" then
      event_count = event_count + 1
      assert(payload.sequence == event_count)
    end
  end)
  client.request_for("receive-test", "receive.test", {}, function(value, request_error)
    result, failure = value, request_error
    assert(event_count == 40, "response overtook earlier events")
  end)
  assert(vim.wait(1000, function() return result ~= nil or failure ~= nil end, 1))
  assert(not failure, failure)
  assert(result.complete and event_count == 40)
  assert(vim.wait(1000, function() return client._client.process == nil end, 1))
  assert(not client._client.ready)
  assert(vim.tbl_isempty(client._client.pending))
end, debug.traceback)

builder.ensure = original_ensure
client._reset_for_test()
if not ok then
  vim.api.nvim_err_writeln(error_message)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
