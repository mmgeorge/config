vim.loader.enable(false)
local root = vim.fs.normalize(vim.fn.getcwd())
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
assert(vim.fn.mkdir(workspace, "p") == 1 and vim.fn.mkdir(data, "p") == 1)
local name = "forge" .. (vim.fn.has("win32") == 1 and ".exe" or "")
local executable = data .. "/" .. name
assert(vim.uv.fs_copyfile(require("forge.builder").binary_path(), executable))
local original_stdpath, original_notify = vim.fn.stdpath, vim.notify
vim.fn.stdpath = function(kind) return kind == "data" and data or original_stdpath(kind) end
local errors = {}
local cleaning = false
vim.notify = function(message, level)
  if level == vim.log.levels.ERROR then errors[#errors + 1] = tostring(message) end
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = executable }) end)
  return function() end
end }
local client = require("forge.client")
client._set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local success, failure = xpcall(function()
  vim.api.nvim_set_current_dir(workspace)
  require("forge").setup({ diff_logging = false, harness_logging = false, harness = { backend = "mock" } })
  require("forge.views.harness").open()
  assert(vim.wait(10000, function() return #errors > 0 or (state.presentation and state.presentation.ready) end, 10), "native Harness did not open")
  assert(#errors == 0, table.concat(errors, "\n"))
  require("forge.views.harness.settings").log(state.session.id, "on")
  assert(vim.wait(3000, function() return require("forge.infra.perf").enabled("harness") end, 10))
  local prompt = "literal ** prompt with native ownership"
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { prompt })
  controller.submit()
  assert(vim.wait(10000, function()
    local text = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
    return #errors > 0 or (not state.busy and text:find("Mock response:", 1, true) ~= nil)
  end, 10), "native Harness response did not settle")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "", "durable native admission did not clear composer")
  local summary_found = false
  for _, line in ipairs(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false)) do
    if line:match("^▸ Thought %d+s$") then summary_found = true end
  end
  assert(summary_found, "fresh host did not render the current metrics header")
  local log_status
  client.request("trace.status", {}, function(result, error)
    assert(not error, error)
    log_status = result
  end)
  assert(vim.wait(3000, function() return log_status ~= nil end, 10))
  assert(log_status.enabled and log_status.path:find(state.session.id, 1, true))
  local prompt_record
  for _, line in ipairs(vim.fn.readfile(log_status.path)) do
    if line:find(prompt, 1, true) then prompt_record = line break end
  end
  assert(prompt_record, "session trace lost prompt body")
  local cleared_status
  client.request("trace.session.clear", {}, function(result, error)
    assert(not error, error)
    cleared_status = result
  end)
  assert(vim.wait(3000, function() return cleared_status ~= nil end, 10))
  assert(cleared_status.enabled and cleared_status.path == log_status.path)
  assert(not vim.tbl_contains(vim.fn.readfile(log_status.path), prompt_record), "prior session trace survived clear")
  assert(vim.bo[state.composer_buf].modifiable and not vim.bo[state.transcript_buf].modifiable)
  local previous = state.transcript_win
  vim.api.nvim_set_current_win(previous)
  vim.cmd("vsplit")
  local secondary = vim.api.nvim_get_current_win()
  state.presentation.refresh_views()
  vim.api.nvim_win_close(previous, true)
  assert(vim.wait(3000, function() return state.presentation.views[previous] == nil and state.presentation.views[secondary] ~= nil end, 10))
  controller.render()
  assert(vim.wait(3000, function() return not state.presentation.syncing end, 10))
  assert(#errors == 0, table.concat(errors, "\n"))
  vim.api.nvim_buf_set_text(state.composer_buf, 0, 0, 0, 0, { "draft survives collected host" })
  local state_settled, state_error = false, nil
  client.request("state.get", {}, function(_, request_error)
    state_error = request_error
    state_settled = true
  end)
  assert(vim.wait(3000, function() return state_settled and not state.state_sync_pending end, 10),
    "Harness state did not settle before host collection")
  assert(not state_error, state_error)
  local old_generation = client.host_generation()
  client.stop()
  assert(vim.wait(5000, function() return client._client.process == nil end, 10), "old Harness host was not collected")
  local restarted = false
  client.start_harness(function(snapshot, restart_error)
    if restart_error then if not cleaning then errors[#errors + 1] = restart_error end return end
    controller.activate_snapshot(snapshot)
    restarted = true
  end)
  assert(vim.wait(10000, function() return #errors > 0 or (restarted and state.presentation and state.presentation.ready and state.presentation.host_generation ~= old_generation) end, 10))
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "draft survives collected host")
  assert(require("forge.infra.perf").enabled("harness"), "saved logging preference lost on restart")
  local initialized = vim.system({ "git", "init", workspace }, { text = true }):wait(10000)
  assert(initialized.code == 0, initialized.stderr)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, { "/plan build the feature" })
  controller.submit()
  assert(vim.wait(10000, function() return #errors > 0 or (not state.busy and state.active_plan) end, 10),
    "complete-text planning submission did not settle")
  assert(#errors == 0, table.concat(errors, "\n"))
  assert(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false)[1] == "", "planning admission did not clear the draft")
  require("forge.views.harness.workspace").release(state)
  assert(state.presentation.close())
end, debug.traceback)
cleaning = true
client.stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.api.nvim_set_current_dir(root)
vim.fn.stdpath, vim.notify = original_stdpath, original_notify
vim.fn.delete(workspace, "rf")
vim.fn.delete(data, "rf")
assert(success and collected, failure or "Harness host was not collected")
print("harness_host: passed")
