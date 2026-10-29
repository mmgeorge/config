vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local notifications = require("forge.infra.notifications")
local original_error, original_echo = notifications.error, vim.api.nvim_echo
local launch_count = 0
---@type fun(result: table)?
local finish_build

local ok, failure = xpcall(function()
  local original_start_harness, original_request = client.session_initialize, client.request
  local recovery_options, recovery_method
  client.session_initialize = function(callback, options)
    recovery_options = options
    callback({ initialized = true })
  end
  client.request = function(request_specification, callback)
  local method = request_specification.method
  local params = request_specification.params or {}

    recovery_method = method
    assert(params.session_id == "leased-session")
    callback({ forked = true })
  end
  for _, action in ipairs({ "retry", "new", "fork" }) do
    recovery_method = nil
    local recovered
    client.session_resolve_lease_conflict(action, { session_id = "leased-session" }, function(result, request_error)
      assert(not request_error)
      recovered = result
    end)
    assert(recovered)
    if action == "retry" then
      assert(recovery_options.session_id == "leased-session" and recovery_options.lease_conflict_action == nil,
        "retry replaced the leased session with a new session")
    else
      assert(recovery_options.lease_conflict_action == "new")
    end
    assert(recovery_method == (action == "fork" and "session.fork" or nil))
  end
  client.session_initialize, client.request = original_start_harness, original_request
  builder.ensure = function(done) finish_build = done end
  client._host_set_launcher_for_test(function() launch_count = launch_count + 1 error("stopped build launched a host") end)
  local completed, rejected = 0, 0
  for _ = 1, 70 do
    client.request({ method = "repository.revision.list", params = { workspace = "fixture" } }, function(_, request_error, detail)
      assert(request_error)
      completed = completed + 1
      if request_error:find("admission is full", 1, true) then
        assert(detail and detail.code == "not_admitted")
        rejected = rejected + 1
      end
    end)
  end
  assert(rejected == 10 and completed == 10)
  assert(vim.tbl_count(client._client.pending) == 60)
  local control_completed = 0
  for _, method in ipairs({ "session.command.apply", "session.command.operation.get", "session.health.get" }) do
    client.request({ session_id = "admission", method = method, params = {} }, function(_, request_error)
      assert(request_error and not request_error:find("admission is full", 1, true))
      control_completed = control_completed + 1
    end)
  end
  assert(vim.tbl_count(client._client.pending) == 63, "ordinary requests consumed task control capacity")
  client.host_stop()
  assert(control_completed == 3)
  assert(completed == 70, "stopping the host failed or duplicated a queued request")
  assert(vim.tbl_isempty(client._client.pending))
  assert(finish_build)
  finish_build({ ok = true, path = "late-build" })
  assert(launch_count == 0, "a cancelled startup accepted a late build")

  client._host_reset_for_test()
  local write_order, transport_completed = {}, false
  builder.ensure = function(done) done({ ok = true, path = "transport-order-fixture" }) end
  client._host_set_launcher_for_test(function(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        write_order[#write_order + 1] = request.method
        if request.method == "host.initialize" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "repository.status.document" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = { opened = true } }) .. "\n")
        end
      end,
      kill = function() end,
    }
  end)
  client.request({ method = "repository.status.document", params = {} }, function(result, request_error)
    assert(not request_error and result.opened)
    transport_completed = true
  end)
  assert(vim.wait(1000, function() return transport_completed end, 1), "initial transport request did not complete")
  assert(table.concat(write_order, ",") == "host.initialize,host.transport.consumed,repository.status.document,host.transport.consumed",
    "first application request bypassed initialization response credit")
  client._host_reset_for_test()
  require("forge").setup({ harness = { backend = "mock" }, harness_logging = false })
  builder.ensure = function(done) done({ ok = true, path = "admission-fixture" }) end
  client._host_set_launcher_for_test(function(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.transport.consumed" or request.method == "host.shutdown" then return end
        if request.method == "github.issue.sync" then
          local messages = {
            { request_id = request.id, event = "github.issue.sync.progress", payload = { phase = "reading", fetched = 0 } },
            { request_id = request.id, event = "repository.status.document.progress", payload = {} },
            { id = request.id, result = { refreshed = true, fetched = 1, pages = 1 } },
            { request_id = request.id, event = "github.issue.sync.progress", payload = { phase = "late" } },
          }
          for _, message in ipairs(messages) do options.stdout(nil, vim.json.encode(message) .. "\n") end
          return
        end
        if request.method == "github.issue.list" then
          options.stdout(nil, vim.json.encode({ id = request.id, error = { code = "busy", message = "storage is busy" } }) .. "\n")
          return
        end
        if request.method == "github.comment" or request.method:match("^github%.review%.document%.") then
          options.stdout(nil, vim.json.encode({ id = request.id, error = { code = "review_error", message = "review operation failed" } }) .. "\n")
          return
        end
        local result = {}
        if request.method == "host.initialize" then result.protocol_version = require("forge.protocol").VERSION
        elseif request.method == "session.initialize" then result.session = { id = "admission" } end
        options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
      end,
      kill = function() end,
    }
  end)
  local accepted = 0
  for _ = 1, 60 do
    client.request({ session_id = "admission", method = "session.state.get", params = {} }, function(_, request_error)
      assert(not request_error, request_error)
      accepted = accepted + 1
    end)
  end
  assert(vim.wait(1000, function() return accepted == 60 end, 1), "full ordinary admission prevented startup control traffic")
  assert(vim.tbl_isempty(client._client.pending))
  local events, progress, finished = 0, 0, false
  local unsubscribe = client.session_events_subscribe(function() events = events + 1 end)
  client.request({ method = "github.issue.sync", params = {} }, function(result, request_error)
    assert(not request_error, request_error)
    assert(result.fetched == 1)
    finished = true
  end, function(payload)
    assert(payload.phase == "reading")
    assert(not finished, "progress arrived after completion")
    progress = progress + 1
  end)
  assert(vim.wait(1000, function() return finished end, 1))
  assert(progress == 1 and events == 0, "request progress escaped into Harness subscribers or survived completion")
  local failed = false
  client.request({ method = "github.issue.list", params = {} }, function(_, request_error)
    assert(request_error == "storage is busy")
    failed = true
  end)
  assert(vim.wait(1000, function() return failed end, 1))
  assert(events == 0, "host storage failure invalidated Harness state")
  for _, method in ipairs({ "github.comment", "github.review.document.open", "github.review.document.input.apply", "github.review.document.save", "github.review.document.reconcile", "github.review.document.close" }) do
    local completed = false
    client.request({ method = method, params = {} }, function(_, request_error)
      assert(request_error == "review operation failed")
      completed = true
    end)
    assert(vim.wait(1000, function() return completed end, 1))
    assert(events == 0, "review failure invalidated Harness state")
  end
  local reported, completed_after_failure = 0, false
  notifications.error = function() error("notification failed") end
  vim.api.nvim_echo = function(chunks, history, options)
    assert(chunks[1][1]:find("progress callback failed", 1, true) and history and options.err)
    reported = reported + 1
  end
  client.request({ method = "github.issue.sync", params = {} }, function(result, request_error)
    assert(not request_error and result.fetched == 1)
    completed_after_failure = true
  end, function() error("progress callback failed") end)
  assert(vim.wait(1000, function() return completed_after_failure end, 1))
  assert(reported == 1 and vim.tbl_isempty(client._client.pending))
  notifications.error, vim.api.nvim_echo = original_error, original_echo
  unsubscribe()
  local protocol = require("forge.protocol")
  for _, message in ipairs({
    { request_id = 1.5, event = "github.issue.sync.progress", payload = {} },
    { request_id = 1, event = "github.issue.sync.progress", payload = {}, session_id = "foreign" },
    { request_id = 1, event = "github.issue.sync.progress", payload = {}, id = 1 },
  }) do
    local decoded, decode_failure = protocol.decode_message(vim.json.encode(message))
    assert(decoded == nil and decode_failure)
  end
  client._host_reset_for_test()
  local output, collected, held_id
  local credits, killed = 0, 0
  client._host_set_launcher_for_test(function(_, options, on_exit)
    output, collected = options.stdout, on_exit
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "host.initialize" then
          assert(request.params.recovery_directory == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "recovery", "github", "v1"))
          assert(request.params.status_ignored_directory == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "status-ignored"))
        end
        if request.method == "host.initialize" then
          output(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "host.transport.consumed" then credits = credits + 1
        elseif request.method ~= "host.shutdown" then held_id = request.id end
      end,
      kill = function() killed = killed + 1 end,
    }
  end)
  local settled
  client.request({ method = "github.comment", params = {} }, function(result, request_error)
    assert(not request_error, request_error)
    settled = result
  end)
  assert(vim.wait(1000, function() return held_id ~= nil end, 1))
  local generation = client._client.generation
  local stopped = false
  client.host_stop(nil, function()
    assert(client._client.process == nil and not client._client.draining,
      "shutdown completion ran before process collection")
    assert(settled and settled.posted, "shutdown completion discarded an admitted mutation")
    stopped = true
  end)
  assert(client._client.draining and client._client.generation == generation)
  assert(not stopped, "shutdown completion must wait for the host")
  assert(not settled and client._client.pending[held_id], "drain discarded an admitted mutation")
  local rejected_drain
  client.request({ session_id = "admission", method = "session.state.get", params = {} }, function(_, request_error) rejected_drain = request_error end)
  assert(rejected_drain == "Forge host is draining")
  local previous_credits = credits
  output(nil, vim.json.encode({ id = held_id, result = { posted = true } }) .. "\n")
  assert(vim.wait(1000, function() return settled ~= nil end, 1))
  assert(settled.posted and credits > previous_credits and killed == 0)
  collected({ code = 0 })
  assert(vim.wait(1000, function() return not client._client.draining end, 1))
  assert(client._client.process == nil and vim.tbl_isempty(client._client.pending))
  assert(stopped, "shutdown completion must run after the host is collected")
end, debug.traceback)

builder.ensure = original_ensure
notifications.error, vim.api.nvim_echo = original_error, original_echo
client._host_reset_for_test()
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
