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
  assert(vim.wait(1000, predicate, 5), "request contract fixture timed out")
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
    tracing_complete = true
  end)
  await(function() return tracing_complete end)
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
