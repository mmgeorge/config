vim.opt.runtimepath:prepend(vim.fn.getcwd() .. "/nvim")
local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local original_ensure, original_error = builder.ensure, notifications.error
local notices, received = {}, {}
local emit
client._reset_for_test()
builder.ensure = function(callback) callback({ ok = true, path = "document-event-fixture" }) end
notifications.error = function(message) notices[#notices + 1] = tostring(message) end
client._set_launcher_for_test(function(_, options, on_exit)
  emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
  return { write = function(_, encoded)
    local request = vim.json.decode(encoded)
    if request.method == "initialize" then emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) end
    if request.method == "repository.write" then emit({ id = request.id, result = {} }) end
    if request.method == "shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
  end, kill = function() end }
end)

---@param predicate fun(): boolean
local function await(predicate)
  assert(vim.wait(1000, predicate, 5), "document event did not arrive")
end

local ok, failure = xpcall(function()
  local ready = false
  client.start(function(_, failure) assert(not failure) ready = true end)
  await(function() return ready end)
  client.subscribe_document("other", function() error("document crossed ownership") end)
  local unsubscribe = client.subscribe_document("status", function(event, payload, generation)
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
client._reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not ok then error(failure) end
print("status document events: passed")
