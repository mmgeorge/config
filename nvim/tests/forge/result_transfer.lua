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
  method = method or "github.issues"
  client._reset_for_test()
  calls, errors, events, received, request_id = 0, 0, 0, nil, nil
  builder.ensure = function(done) done({ ok = true, path = "result-fixture" }) end
  notifications.error = function() end
  client._set_launcher_for_test(function(_, options, on_exit)
    emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
    local exited = false
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "initialize" then
          emit({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } })
        elseif request.method == method then
          request_id = request.id
        elseif request.method == "shutdown" and not exited then
          exited = true
          vim.schedule(function() on_exit({ code = 0, signal = 0 }) end)
        end
      end,
      kill = function() end,
    }
  end)
  client.subscribe(function() events = events + 1 end)
  client.request_host(method, {}, function(result, failure)
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
  start("review.open_pr")
  local body = string.rep('日本語 "quote" \\ newline\n', 30000)
  local encoded = vim.json.encode({ id = request_id, result = { body = body } })
  local chunk = 100000
  local count = math.ceil(#encoded / chunk)
  for sequence = 0, count - 1 do
    part(sequence, count, #encoded, encoded:sub(sequence * chunk + 1, (sequence + 1) * chunk))
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

  local corrupt = {
    function() part(1, 2, 4, "{}") end,
    function() part(0, 2, 4, "{}"); part(0, 2, 4, "{}") end,
    function() part(0, 2, 4, "{}"); part(1, 2, 5, "{}") end,
    function() part(0, 2, 4, "{}"); complete(2, 4) end,
    function() part(0, 1, 1, "{}"); complete(1, 1) end,
    function() part(0, 1, 2, "xx"); complete(1, 2) end,
    function() complete(1, 2) end,
    function() part(0, 1, 16 * 1024 * 1024 + 1, "{}") end,
    function() part(0, 257, 257, "x") end,
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
  client.stop()
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
    client.request_host("github.issues", {}, function(_, request_error)
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

client._reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
