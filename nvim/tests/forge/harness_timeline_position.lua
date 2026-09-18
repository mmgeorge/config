vim.loader.enable(false)
local client = require("forge.client")
local request_for, host_accepting = client.request_for, client.host_accepting
local pending = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  pending[#pending + 1] = { params = params, callback = callback }
end

local function take(operation)
  local request = table.remove(pending, 1)
  assert(request and request.params.operation == operation, "expected " .. operation)
  return request
end

local function snapshot(document, revision, count)
  local rows = {}
  for row = 1, count do rows[row] = "source row " .. row end
  return { document = document, revision = revision, block = {
    { id = "body", text = rows, metadata = { target = {}, decoration = {}, editable_region = {} } },
  } }
end

local success, failure = xpcall(function()
  local transcript = vim.api.nvim_create_buf(false, true)
  local composer = vim.api.nvim_create_buf(false, true)
  local window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(window, transcript)
  local owner = require("forge.views.harness.presentation").open({
    session_id = "position", transcript_buffer = transcript, composer_buffer = composer,
    transcript_window = window, is_alive = function() return true end,
    notice = function(message) error(message) end,
  }, function(value, message) assert(value and not message) end)
  local opening = take("open")
  opening.callback({ transcript = snapshot(opening.params.document, 0, 80),
    composer = snapshot(opening.params.composer, 0, 1) })
  local revision = 0
  local function sync(count)
    revision = revision + 1
    take("sync").callback({ snapshot = snapshot(opening.params.document, revision, count) })
  end
  vim.fn.winrestview({ lnum = 40, col = 4, topline = 30 })
  local parent = vim.fn.winsaveview()
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  vim.api.nvim_win_set_cursor(window, { 3, 2 })
  owner.select_agent(nil)
  take("select_agent").callback({})
  sync(80)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 40, 4 }), "parent cursor was lost")
  assert(vim.fn.winsaveview().topline == parent.topline, "parent viewport was lost")
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 3, 2 }), "child cursor was lost")

  -- A selection queued during refresh captures the outgoing view only after that refresh settles.
  owner.sync()
  owner.select_agent(nil)
  assert(#pending == 1, "timeline selection raced a refresh")
  sync(8)
  take("select_agent").callback({})
  sync(80)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 40, 4 }))
  assert(#pending == 0)
  owner.highlight()
  local highlighting = take("highlight")
  owner.highlight()
  assert(#pending == 0, "syntax analysis was started twice")
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 3, 2 }),
    "pending syntax blocked timeline selection")
  highlighting.callback({})
  sync(8)
  assert(#pending == 0)
  owner.close()
  take("close").callback({})
end, debug.traceback)
client.request_for, client.host_accepting = request_for, host_accepting
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_timeline_position: passed")
vim.cmd("qa!")
