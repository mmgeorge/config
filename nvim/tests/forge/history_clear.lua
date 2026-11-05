vim.loader.enable(false)
local client = require("forge.client")
local builder = require("forge.builder")
local navigation = require("forge.views.harness.session_navigation")
local session = require("forge.session")
local notifications = require("forge.infra.notifications")
local original_stop, original_system = client.host_stop, vim.system
local original_path, original_error = builder.binary_path, notifications.error
local stopped, process_complete
local launch_count, notice_list = 0, {}
local state = session.new_harness_state()
state.session = { id = "history-fixture" }
state.transcript_buf = vim.api.nvim_create_buf(false, true)
state.composer_buf = vim.api.nvim_create_buf(false, true)
state.prompt_history = { "old prompt" }
vim.api.nvim_buf_set_lines(state.transcript_buf, 0, -1, false, { "saved transcript" })
vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "unsent draft" })
local registry = navigation._registry_for_test()
registry[state.session.id] = state
session.register_harness(state.session.id, state)
session.select_harness(state)

local ok, failure = xpcall(function()
  builder.binary_path = function() return vim.v.progpath end
  notifications.error = function(message) notice_list[#notice_list + 1] = message end
  client.host_stop = function(_, callback) stopped = callback end
  vim.system = function(command, options, callback)
    assert(command[2] == "history-clear" and command[3] == "--data-root")
    assert(command[4] == vim.fs.joinpath(vim.fn.stdpath("data"), "forge", "harness"))
    assert(options.stdout and options.stderr)
    launch_count = launch_count + 1
    process_complete = callback
    return {}
  end

  local completed, clear_error
  client.history_clear(function(error) completed, clear_error = true, error end)
  assert(launch_count == 0, "history deletion started before host collection")
  local request_error
  client.host_start(function(_, error) request_error = error end)
  assert(request_error and request_error:find("history is being cleared", 1, true))
  client.request({ method = "repository.revision.list", params = {} }, function(_, error)
    request_error = error
  end)
  assert(request_error and request_error:find("history is being cleared", 1, true))
  stopped()
  assert(launch_count == 1 and not completed)
  process_complete({ code = 1, stderr = "history is in use" })
  assert(vim.wait(1000, function() return completed end))
  assert(clear_error == "history is in use")
  assert(state.session.id == "history-fixture", "failed clearing discarded live view identities")
  assert(#notice_list == 1 and notice_list[1]:find("history is in use", 1, true))

  completed, clear_error = false, nil
  client.history_clear(function(error) completed, clear_error = true, error end)
  stopped()
  process_complete({ code = 1, stderr = "Some versions remain", stdout = vim.json.encode({
    current_cleared = true, removed = { "state/current" },
    retained = { { path = "state/v8", message = "locked by another host" } },
  }) })
  assert(vim.wait(1000, function() return completed end))
  assert(clear_error == "state/v8: locked by another host" and #notice_list == 2)
  assert(state.session == nil and vim.tbl_isempty(session.harness_by_id))
  assert(#state.prompt_history == 0)
  assert(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)[1]:find("History cleared", 1, true))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "unsent draft")
  assert(not session.harness.session)
  completed, clear_error = false, nil
  client.history_clear(function(error) completed, clear_error = true, error end)
  stopped()
  process_complete({ code = 0, stderr = "", stdout = vim.json.encode({
    current_cleared = true, removed = {}, retained = {},
  }) })
  assert(vim.wait(1000, function() return completed end))
  assert(not clear_error and #notice_list == 2)

  local specification = dofile("nvim/lua/plugins/forge.lua")[1]
  assert(vim.tbl_contains(specification.cmd, "ForgeHistoryClear"), "history command is not lazy-loadable")
  specification.config(nil, { about_auto_generate = false })
  local command_called = false
  local original_clear = client.history_clear
  client.history_clear = function() command_called = true end
  vim.cmd("ForgeHistoryClear")
  client.history_clear = original_clear
  assert(command_called, "ForgeHistoryClear bypassed the shared history owner")
end, debug.traceback)

client.host_stop, vim.system = original_stop, original_system
builder.binary_path, notifications.error = original_path, original_error
vim.api.nvim_buf_delete(state.transcript_buf, { force = true })
vim.api.nvim_buf_delete(state.composer_buf, { force = true })
assert(ok, failure)
print("Forge history clearing passed")
