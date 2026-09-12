vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local original_ensure = builder.ensure
local notifications = require("forge.infra.notifications")
local original_error, original_write = notifications.error, vim.api.nvim_err_writeln
local launch_count = 0
---@type fun(result: table)?
local finish_build

local ok, failure = xpcall(function()
  builder.ensure = function(done) finish_build = done end
  client._set_launcher_for_test(function() launch_count = launch_count + 1 error("stopped build launched a host") end)
  local completed, rejected = 0, 0
  for _ = 1, 70 do
    client.request_host("repository.revisions", { workspace = "fixture" }, function(_, request_error)
      assert(request_error)
      completed = completed + 1
      if request_error:find("admission is full", 1, true) then rejected = rejected + 1 end
    end)
  end
  assert(rejected == 10 and completed == 10)
  assert(vim.tbl_count(client._client.pending) == 60)
  client.stop()
  assert(completed == 70, "stopping the host failed or duplicated a queued request")
  assert(vim.tbl_isempty(client._client.pending))
  assert(finish_build)
  finish_build({ ok = true, path = "late-build" })
  assert(launch_count == 0, "a cancelled startup accepted a late build")

  client._reset_for_test()
  local write_order, transport_completed = {}, false
  builder.ensure = function(done) done({ ok = true, path = "transport-order-fixture" }) end
  client._set_launcher_for_test(function(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        write_order[#write_order + 1] = request.method
        if request.method == "initialize" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "status" then
          options.stdout(nil, vim.json.encode({ id = request.id, result = { opened = true } }) .. "\n")
        end
      end,
      kill = function() end,
    }
  end)
  client.request_host("status", {}, function(result, request_error)
    assert(not request_error and result.opened)
    transport_completed = true
  end)
  assert(vim.wait(1000, function() return transport_completed end, 1), "initial transport request did not complete")
  assert(table.concat(write_order, ",") == "initialize,transport.consumed,status,transport.consumed",
    "first application request bypassed initialization response credit")
  client._reset_for_test()
  require("forge").setup({ harness = { backend = "mock" }, harness_logging = false })
  builder.ensure = function(done) done({ ok = true, path = "admission-fixture" }) end
  client._set_launcher_for_test(function(_, options)
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "transport.consumed" or request.method == "shutdown" then return end
        if request.method == "github.sync" then
          local messages = {
            { request_id = request.id, event = "github.sync.progress", payload = { phase = "reading", fetched = 0 } },
            { request_id = request.id, event = "unrelated.progress", payload = {} },
            { id = request.id, result = { refreshed = true, fetched = 1, pages = 1 } },
            { request_id = request.id, event = "github.sync.progress", payload = { phase = "late" } },
          }
          for _, message in ipairs(messages) do options.stdout(nil, vim.json.encode(message) .. "\n") end
          return
        end
        if request.method == "github.issues" then
          options.stdout(nil, vim.json.encode({ id = request.id, error = { code = "busy", message = "storage is busy" } }) .. "\n")
          return
        end
        if request.method == "github.comment" or request.method:sub(1, 7) == "review." then
          options.stdout(nil, vim.json.encode({ id = request.id, error = { code = "review_error", message = "review operation failed" } }) .. "\n")
          return
        end
        local result = {}
        if request.method == "initialize" then result.protocol_version = require("forge.protocol").VERSION
        elseif request.method == "harness.initialize" then result.session = { id = "admission" } end
        options.stdout(nil, vim.json.encode({ id = request.id, result = result }) .. "\n")
      end,
      kill = function() end,
    }
  end)
  local accepted = 0
  for _ = 1, 60 do
    client.request("state.get", {}, function(_, request_error)
      assert(not request_error, request_error)
      accepted = accepted + 1
    end)
  end
  assert(vim.wait(1000, function() return accepted == 60 end, 1), "full ordinary admission prevented startup control traffic")
  assert(vim.tbl_isempty(client._client.pending))
  local events, progress, finished = 0, 0, false
  local unsubscribe = client.subscribe(function() events = events + 1 end)
  client.request_host("github.sync", {}, function(result, request_error)
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
  client.request_host("github.issues", {}, function(_, request_error)
    assert(request_error == "storage is busy")
    failed = true
  end)
  assert(vim.wait(1000, function() return failed end, 1))
  assert(events == 0, "host storage failure invalidated Harness state")
  for _, method in ipairs({ "github.comment", "review.open_pr", "review.region_edit", "review.snapshot", "review.save", "review.reconcile", "review.close" }) do
    local completed = false
    client.request_host(method, {}, function(_, request_error)
      assert(request_error == "review operation failed")
      completed = true
    end)
    assert(vim.wait(1000, function() return completed end, 1))
    assert(events == 0, "review failure invalidated Harness state")
  end
  local reported, completed_after_failure = 0, false
  notifications.error = function() error("notification failed") end
  vim.api.nvim_err_writeln = function(message)
    assert(message:find("progress callback failed", 1, true))
    reported = reported + 1
  end
  client.request_host("github.sync", {}, function(result, request_error)
    assert(not request_error and result.fetched == 1)
    completed_after_failure = true
  end, function() error("progress callback failed") end)
  assert(vim.wait(1000, function() return completed_after_failure end, 1))
  assert(reported == 1 and vim.tbl_isempty(client._client.pending))
  notifications.error, vim.api.nvim_err_writeln = original_error, original_write
  unsubscribe()
  local protocol = require("forge.protocol")
  for _, message in ipairs({
    { request_id = 1.5, event = "github.sync.progress", payload = {} },
    { request_id = 1, event = "github.sync.progress", payload = {}, session_id = "foreign" },
    { request_id = 1, event = "github.sync.progress", payload = {}, id = 1 },
  }) do
    local decoded, decode_failure = protocol.decode_message(vim.json.encode(message))
    assert(decoded == nil and decode_failure)
  end
  client._reset_for_test()
  local output, collected, held_id
  local credits, killed = 0, 0
  client._set_launcher_for_test(function(_, options, on_exit)
    output, collected = options.stdout, on_exit
    return {
      write = function(_, encoded)
        local request = vim.json.decode(encoded)
        if request.method == "initialize" then
          assert(request.params.recovery_directory == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "recovery", "github", "v1"))
          assert(request.params.status_ignored_directory == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "status-ignored"))
        end
        if request.method == "initialize" then
          output(nil, vim.json.encode({ id = request.id, result = { protocol_version = require("forge.protocol").VERSION } }) .. "\n")
        elseif request.method == "transport.consumed" then credits = credits + 1
        elseif request.method ~= "shutdown" then held_id = request.id end
      end,
      kill = function() killed = killed + 1 end,
    }
  end)
  local settled
  client.request_host("github.comment", {}, function(result, request_error)
    assert(not request_error, request_error)
    settled = result
  end)
  assert(vim.wait(1000, function() return held_id ~= nil end, 1))
  local generation = client._client.generation
  client.stop()
  assert(client._client.draining and client._client.generation == generation)
  assert(not settled and client._client.pending[held_id], "drain discarded an admitted mutation")
  local rejected_drain
  client.request_host("state.get", {}, function(_, request_error) rejected_drain = request_error end)
  assert(rejected_drain == "Forge host is draining")
  local previous_credits = credits
  output(nil, vim.json.encode({ id = held_id, result = { posted = true } }) .. "\n")
  assert(vim.wait(1000, function() return settled ~= nil end, 1))
  assert(settled.posted and credits > previous_credits and killed == 0)
  collected({ code = 0 })
  assert(vim.wait(1000, function() return not client._client.draining end, 1))
  assert(client._client.process == nil and vim.tbl_isempty(client._client.pending))
end, debug.traceback)

builder.ensure = original_ensure
notifications.error, vim.api.nvim_err_writeln = original_error, original_write
client._reset_for_test()
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") else vim.cmd("qa!") end
