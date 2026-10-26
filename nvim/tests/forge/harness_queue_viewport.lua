vim.loader.enable(false)
vim.o.lines, vim.o.columns = 30, 80
local scene = dofile("nvim/tests/forge/support/check_groups_scene.lua").open()
local state = scene.state
local controller = require("forge.views.harness.controller")
local function settled()
  assert(vim.wait(5000, function()
    return not state.presentation.syncing and not state.presentation.pending
      and not state.presentation.staging_inflight and not state.presentation.applying
      and vim.deep_equal(state.presentation.staging, state.presentation.applied_staging)
  end, 10), "queue presentation did not settle")
end
local function assert_tail()
  local count = vim.api.nvim_buf_line_count(state.transcript_buf)
  local first, last = unpack(vim.api.nvim_win_call(state.transcript_win, function()
    return { vim.fn.line("w0"), vim.fn.line("w$") }
  end))
  local status = require("forge.views.harness.status_region").location(state.presentation.transcript) + 1
  assert(last == count and first <= status, "queue editing hid status or the queue tail")
  assert(vim.api.nvim_get_current_win() == state.composer_win, "queue editing stole composer focus")
end
local success, failure = xpcall(function()
  state.busy = true
  local original = "first queued line\nfirst hidden line"
  local selected = "second queued line\n" .. string.rep("complete draft line 界\n", 12) .. "last draft line"
  state.queue = { { text = original }, { text = selected } }
  controller.refresh_winbar()
  settled()
  local text = table.concat(scene.lines(), "\n")
  assert(text:find("first queued line", 1, true) and text:find("second queued line", 1, true))
  assert(not text:find("hidden line", 1, true) and not text:find("last draft line", 1, true), "queue leaked complete messages")
  scene.toggle("scene:checks")
  controller.edit_queued(-1)
  settled()
  assert_tail()
  assert(table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n") == selected,
    "editing lost the complete queued message")
  controller.edit_queued(-1)
  settled()
  assert_tail()
  assert(table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n") == original)
  controller.edit_queued(1)
  settled()
  assert_tail()
  vim.api.nvim_set_current_win(state.transcript_win)
  vim.api.nvim_win_set_cursor(state.transcript_win, { 1, 2 })
  vim.api.nvim_win_call(state.transcript_win, function() vim.cmd("normal! zt") end)
  local view = vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)
  controller.refresh_winbar()
  settled()
  assert(vim.deep_equal(view, vim.api.nvim_win_call(state.transcript_win, vim.fn.winsaveview)),
    "passive queue refresh moved a history reader")
  controller.edit_queued(-1)
  controller.submit()
  settled()
  assert_tail()
  assert(not state.queue_edit and #state.queue == 2, "saving changed the queue")
  controller.edit_queued(-1)
  controller.queue_submit()
  settled()
  assert_tail()
  assert(not state.queue_edit and #state.queue == 1, "unqueue did not remove only the edited message")
  assert(table.concat(vim.api.nvim_buf_get_lines(state.composer_buf, 0, -1, false), "\n") == selected,
    "unqueue lost the complete message")
end, debug.traceback)
scene.close()
assert(success, failure)
print("harness_queue_viewport: passed")
