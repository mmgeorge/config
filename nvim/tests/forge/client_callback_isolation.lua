local client = require("forge.client")
local builder = require("forge.builder")
local notifications = require("forge.infra.notifications")
local session = require("forge.session")
local original_ensure, original_error = builder.ensure, notifications.error
local diagnostic, sent = {}, {}
local emit
local response_version = require("forge.protocol").VERSION
builder.ensure = function(callback) callback({ ok = true, path = "callback-fixture" }) end
notifications.error = function(message) diagnostic[#diagnostic + 1] = tostring(message) end
client._reset_for_test()
client._set_launcher_for_test(function(_, options, on_exit)
  emit = function(message) options.stdout(nil, vim.json.encode(message) .. "\n") end
  return {
    write = function(_, encoded)
      local request = vim.json.decode(encoded)
      sent[request.method] = request.id
      if request.method == "initialize" then emit({ id = request.id, result = { protocol_version = response_version } }) end
      if request.method == "harness.initialize" then emit({ id = request.id, result = { session = { id = "fixture" } } }) end
      if request.method == "plan.scope_deviation_review" then emit({ id = request.id, result = {} }) end
      if request.method == "shutdown" then vim.schedule(function() on_exit({ code = 0 }) end) end
    end,
    kill = function() end,
  }
end)

local function await(predicate, message)
  assert(vim.wait(1000, predicate, 5), message)
end

local succeeded, failure = xpcall(function()
  local started = false
  client.start(function() error("startup UI fixture failure") end)
  client.start(function(_, start_error) assert(not start_error) started = true end)
  await(function() return started end, "throwing startup consumer skipped another waiter")
  assert(client._client.ready and not client._client.draining)

  local completed = false
  client.request_host("permissions.get", {}, function() error("reply UI fixture failure") end)
  client.request_host("status", {}, function(result) completed = result.ok end)
  await(function() return sent["permissions.get"] and sent.status end, "requests were not written")
  emit({ id = sent["permissions.get"], result = { document = "policy" } })
  emit({ id = sent.status, result = { ok = true } })
  await(function() return completed end, "throwing response consumer skipped independent reply")
  assert(client._client.ready and client._client.process and not client._client.draining)
  assert(vim.tbl_isempty(client._client.pending))
  assert(#diagnostic == 2 and diagnostic[1]:find("startup UI fixture failure", 1, true)
    and diagnostic[2]:find("reply UI fixture failure", 1, true), "consumer failure diagnostics were lost")

  client.request_host("permissions.error", {}, function() error("error UI fixture failure") end)
  await(function() return sent["permissions.error"] end, "error request was not written")
  emit({ id = sent["permissions.error"], error = { code = "fixture", message = "expected request error" } })
  await(function() return #diagnostic == 3 end, "error callback exception was not reported")
  assert(client._client.ready and not client._client.draining)

  local harness_started = false
  client.start_harness(function() error("Harness startup UI fixture failure") end)
  client.start_harness(function(_, start_error) assert(not start_error) harness_started = true end)
  await(function() return harness_started end, "throwing Harness startup consumer skipped another waiter")
  local requesting_state = session.new_harness_state()
  local viewing_state = session.new_harness_state()
  session.activate_harness(requesting_state)
  client.request_for("fixture", "session.fixture", {}, function()
    assert(session.harness == requesting_state)
    error("scoped UI fixture failure")
  end)
  session.activate_harness(viewing_state)
  await(function() return sent["session.fixture"] end, "scoped request was not written")
  emit({ id = sent["session.fixture"], result = {} })
  await(function() return #diagnostic == 5 end, "scoped consumer failure was not reported")
  assert(session.harness == viewing_state, "throwing scoped consumer changed active Harness state")
  assert(client._client.ready and not client._client.draining)

  client.request_host("github.issues", {}, function() end)
  await(function() return sent["github.issues"] end, "malformed transfer request was not written")
  emit({ request_id = sent["github.issues"], event = "result.complete", payload = { part_count = 1, total_bytes = 1 } })
  await(function() return not client._client.ready end, "malformed protocol stopped being fatal")
  await(function() return not client._client.process end, "malformed protocol host did not drain")
  response_version = 999
  local mismatch_reported = false
  client.start(function(_, start_error)
    assert(start_error:find("wire versions differ", 1, true))
    mismatch_reported = true
    error("mismatched startup UI fixture failure")
  end)
  await(function() return mismatch_reported and not client._client.process end,
    "consumer failure prevented initialization mismatch shutdown")
end, debug.traceback)
client._reset_for_test()
builder.ensure, notifications.error = original_ensure, original_error
if not succeeded then io.stderr:write(tostring(failure), "\n") vim.cmd("cquit 1") end
print("client callback isolation passed")
vim.cmd("qa!")
