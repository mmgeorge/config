vim.loader.enable(false)
local client = require("forge.client")
local request_for, host_accepting = client.request_for, client.host_accepting
local pending = {}
client.host_accepting = function() return true end
client.request_for = function(_, _, params, callback)
  if params.operation == "background_terminals" then callback({ supported = false }) return end
  pending[#pending + 1] = { params = params, callback = callback }
end

local function take(operation)
  assert(vim.wait(1000, function() return #pending > 0 end, 1), "missing " .. operation)
  local request = table.remove(pending, 1)
  assert(request and request.params.operation == operation, "expected " .. operation .. ", got " .. vim.inspect(request and request.params))
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
  assert(vim.wo[window].signcolumn == "yes:1" and vim.wo[window].statuscolumn:find("forge.nodes", 1, true)
    and not vim.wo[window].foldenable and vim.wo[window].foldcolumn == "0",
    "initial transcript must use the shared fold marker column")
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
  vim.fn.winrestview({ lnum = 45, col = 5, topline = 34 })
  parent = vim.fn.winsaveview()
  take("select_agent").callback({})
  vim.fn.winrestview({ lnum = 48, col = 6, topline = 36 })
  parent = vim.fn.winsaveview()
  sync(8)
  vim.api.nvim_win_set_cursor(window, { 3, 2 })
  owner.select_agent(nil)
  take("select_agent").callback({})
  sync(80)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 48, 6 }), "parent cursor was lost")
  assert(vim.fn.winsaveview().topline == parent.topline, "parent viewport was lost")
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 3, 2 }), "child cursor was lost")

  -- A selection queued during refresh captures the outgoing view only after that refresh settles.
  owner.sync()
  assert(vim.wait(1000, function() return #pending == 1 end, 1))
  owner.select_agent(nil)
  assert(#pending == 1, "timeline selection raced a refresh")
  sync(8)
  take("select_agent").callback({})
  sync(80)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 48, 6 }))
  assert(#pending == 0)
  owner.highlight()
  local highlighting = take("highlight")
  owner.highlight()
  assert(#pending == 0, "syntax analysis was started twice")
  owner.select_agent("child")
  assert(#pending == 0, "timeline selection bypassed an in-flight presentation request")
  highlighting.callback({})
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 3, 2 }),
    "timeline selection lost the saved child position")
  assert(#pending == 0)

  owner.select_agent(nil)
  take("select_agent").callback({})
  local prepared = take("sync")
  revision = revision + 1
  prepared.callback({ snapshot = snapshot(opening.params.document, revision, 6000) })
  assert(owner.transcript.update_pending, "large timeline did not yield during preparation")
  vim.api.nvim_win_set_cursor(window, { 5, 1 })
  assert(vim.wait(10000, function() return not owner.transcript.update_pending end, 1))
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 48, 6 }), "return did not restore parent history")
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 5, 1 }),
    "timeline switch lost movement made during asynchronous preparation")

  owner.select_agent(nil)
  take("select_agent").callback({})
  vim.api.nvim_win_set_cursor(window, { 6, 2 })
  take("sync").callback({ patch = {} })
  owner.select_agent("child")
  take("select_agent").callback({})
  sync(8)
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(window), { 6, 2 }),
    "unchanged timeline switch did not capture its outgoing view")
  owner.close()
  take("close").callback({})
end, debug.traceback)
client.request_for, client.host_accepting = request_for, host_accepting
if not success then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("harness_timeline_position: passed")
vim.cmd("qa!")
