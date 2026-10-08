vim.opt.runtimepath:prepend("nvim")
local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local emitted, exited, request_id
local launch_count = 0
local diagnostics = {}
builder.ensure = function(callback) callback({ ok = true, path = "transport-fixture" }) end
notifications.error = function(message) diagnostics[#diagnostics + 1] = message end
client._reset_for_test()
client._set_launcher_for_test(function(_, options, on_exit)
  launch_count = launch_count + 1
  emitted, exited = options.stdout, on_exit
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      if request.method == "initialize" then
        emitted(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
      elseif request.method == "prompt.submit" then request_id = request.id end
    end,
    kill = function() end,
  }
end)

local events, stopped, unresolved = {}, false, false
client.subscribe(function(event, payload, session_id)
  if event == "host_stopped" then stopped = true return end
  events[#events + 1] = { event = event, payload = payload, session_id = session_id }
end)
client.request_host("prompt.submit", {}, function(_, failure, detail)
  assert(stopped, "pending callback ran before the UI learned that the host stopped")
  assert(failure:find("unresolved request", 1, true) and detail.code == "outcome_unknown")
  client.request_host("state.get", {}, function(_, retry_error)
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
client._reset_for_test()
print("harness_transport_failure: passed")
vim.cmd("qa!")
