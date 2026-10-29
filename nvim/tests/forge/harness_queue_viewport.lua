vim.loader.enable(false)
vim.o.lines, vim.o.columns = 30, 80
local root = vim.fn.getcwd()
local workspace, data = vim.fn.tempname(), vim.fn.tempname()
vim.fn.mkdir(workspace, "p")
vim.fn.mkdir(data, "p")
local binary = vim.fn.fnamemodify(require("forge.builder").binary_path(), ":p")
local original_stdpath = vim.fn.stdpath
vim.fn.stdpath = function(kind)
  return (kind == "data" or kind == "config") and data or original_stdpath(kind)
end
package.loaded["forge.builder"] = { ensure = function(callback)
  vim.schedule(function() callback({ ok = true, path = binary }) end)
  return function() end
end }
local client = require("forge.client")
client._host_set_launcher_for_test(vim.system)
local state = require("forge.session").harness
local controller = require("forge.views.harness.controller")
local failures = {}
require("forge.infra.notifications").error = function(message) failures[#failures + 1] = message end
local function await(predicate, message)
  assert(vim.wait(5000, function() return #failures > 0 or predicate() end, 10), message .. ": " .. vim.inspect({
    ready = state.ready, session = state.session, initial_snapshot_pending = state.initial_snapshot_pending,
    presentation = state.presentation and { ready = state.presentation.ready, failure = state.presentation.failure,
      syncing = state.presentation.syncing, pending = state.presentation.pending },
    input = state.input, host = { ready = client._client.ready, runtime_ready = client._client.runtime_ready },
  }))
  assert(#failures == 0, table.concat(failures, "\n"))
end
local function settled()
  await(function()
    return state.input and #state.input.pending == 0 and not state.input.refreshing
      and state.presentation and not state.presentation.syncing and not state.presentation.pending
      and not state.presentation.staging_inflight and not state.presentation.applying
      and vim.deep_equal(state.presentation.staging, state.presentation.applied_staging)
  end, "queue presentation did not settle")
end
local function compose(text)
  vim.api.nvim_buf_set_lines(state.composer_buf, 0, -1, false, vim.split(text, "\n", { plain = true }))
end
local function text()
  return table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n")
end
local success, failure = xpcall(function()
  vim.fn.chdir(workspace)
  require("forge").setup({ harness_logging = false, harness = { backend = "mock",
    backends = { mock = { command = { "mock" }, selectable = true } } } })
  require("forge.views.harness").open()
  await(function() return state.ready and state.presentation and state.presentation.ready and state.input end, "startup")
  controller.cancel_turn()
  await(function() return not state.task_operation and not state.request_busy end, "pause")
  local original = "first queued line\nfirst hidden line"
  local selected = "second queued line\n" .. string.rep("complete draft line 界\n", 12) .. "last draft line"
  compose(original)
  controller.queue_submit()
  compose(selected)
  controller.queue_submit()
  settled()
  local transcript = table.concat(vim.api.nvim_buf_get_lines(state.transcript_buf, 0, -1, false), "\n")
  local status = table.concat(vim.api.nvim_buf_get_lines(state.presentation.status_view.buffer, 0, -1, false), "\n")
  assert(status:find("first queued line", 1, true) and status:find("second queued line", 1, true))
  assert(not status:find("first hidden line", 1, true) and not status:find("last draft line", 1, true))
  assert(not transcript:find("first queued line", 1, true) and not transcript:find("second queued line", 1, true))
  assert(state.presentation.status_view.window ~= nil, "paused queue did not keep the split visible")
  controller.edit_queued(-1)
  assert(text() == selected, "editing waited for acknowledgement or lost the complete draft")
  settled()
  assert(vim.api.nvim_get_current_win() == state.composer_win)
  controller.edit_queued(-1)
  assert(text() == original)
  settled()
  controller.edit_queued(1)
  settled()
  assert(text() == selected)
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 2 })
  vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! zt") end)
  local view = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  controller.refresh_winbar()
  settled()
  assert(vim.deep_equal(view, vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)),
    "passive queue refresh moved a history reader")
  controller.submit()
  settled()
  assert(not state.queue_edit and #state.queue == 2)
  controller.edit_queued(-1)
  controller.queue_submit()
  settled()
  assert(not state.queue_edit and #state.queue == 1 and text() == selected,
    "unqueue lost the complete message or removed another entry")
end, debug.traceback)
require("forge.views.harness.workspace").release(state)
if state.presentation then state.presentation.close() end
client.host_stop()
local collected = vim.wait(5000, function() return client._client.process == nil end, 10)
vim.fn.chdir(root)
vim.fn.stdpath = original_stdpath
assert(success and collected, failure or "fixture host did not stop")
print("harness_queue_viewport: passed")
