local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
local workspace = vim.fn.getcwd()
vim.opt.runtimepath:prepend(workspace .. "/nvim")

local status = require("forge.status")
local startup_log = require("forge.startup_log")
local startup_log_path = vim.fn.tempname()
local original_startup_log_path = startup_log.path
startup_log.path = function() return startup_log_path end
local original_context = package.loaded["forge.views.status.status_context"]
local context_attachment = {}
package.loaded["forge.views.status.status_context"] = {
  attach = function(options)
    context_attachment[#context_attachment + 1] = options
    return { refresh = function() end, close = function() end }
  end,
  producer_handlers = function() return {} end,
}

local pending_open = {}
local closed_document = {}
status._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then
    pending_open[params.document] = function()
      callback(fixture.snapshot(params.document, { context = { workspace = workspace, branch = "main", recent = {}, issues = {}, branch_prefix = "" } }))
    end
  elseif params.operation == "close" then
    closed_document[params.document] = true
    callback({ closed = true })
  elseif params.operation == "close_view" or params.operation == "refresh" then
    callback(nil)
  else
    error("unexpected lifecycle request: " .. params.operation)
  end
end)

local function await(predicate, message)
  assert(vim.wait(1000, predicate, 5), message)
end

local succeeded, failure = xpcall(function()
  local source = vim.api.nvim_get_current_buf()
  local source_window = vim.api.nvim_get_current_win()
  vim.wo[source_window].number = true
  local ready = false
  local hidden = status.open({ workspace = workspace, on_ready = function() ready = true end })
  local loading_namespace = vim.api.nvim_create_namespace("ForgeStatusLoading")
  local function loading_text()
    local marks = vim.api.nvim_buf_get_extmarks(hidden.replica.buffer, loading_namespace, 0, -1, { details = true })
    assert(#marks == 1, "startup message was duplicated")
    return marks[1][4].virt_text[1][1]
  end
  assert(loading_text() == "Loading Forge status…")
  vim.api.nvim_win_set_buf(source_window, source)
  pending_open[hidden.document]()
  await(function() return ready end, "hidden opening did not complete initialization")
  local timing_event = {}
  for _, row in ipairs(vim.fn.readfile(startup_log_path)) do
    local record = vim.json.decode(row)
    timing_event[record.event] = record.fields
  end
  assert(timing_event["status.buffer.opened"], "buffer creation timing was not logged")
  assert(timing_event["status.loading.presented"], "loading presentation timing was not logged")
  assert(timing_event["status.buffer.prepared"], "buffer preparation timing was not logged")
  assert(timing_event["status.snapshot.applied"].files == 1, "snapshot application size was not logged")
  assert(timing_event["status.snapshot.applied"].rows == 8, "snapshot application row count was not logged")
  assert(timing_event["status.ready"].elapsed_ms >= 0, "ready timing was not logged")
  assert(hidden.replica.status == "Applied")
  assert(next(hidden.view) == nil, "hidden document attached an unrelated window")
  assert(vim.api.nvim_get_current_buf() == source and vim.wo[source_window].number)
  vim.api.nvim_win_set_buf(source_window, hidden.replica.buffer)
  status.demand(hidden)
  await(function() return hidden.view[source_window] ~= nil end, "redisplayed document did not attach its view")
  status.close(hidden)
  await(function() return closed_document[hidden.document] end, "hidden document close did not settle")

  vim.api.nvim_win_set_buf(source_window, source)
  ready = false
  local visible = status.open({ workspace = workspace, on_ready = function() ready = true end })
  vim.cmd("vsplit")
  local unrelated_window = vim.api.nvim_get_current_win()
  vim.api.nvim_win_set_buf(unrelated_window, source)
  vim.wo[unrelated_window].number = true
  pending_open[visible.document]()
  await(function() return ready and visible.view[source_window] ~= nil end,
    "background visible document did not initialize its displaying window")
  assert(visible.view[unrelated_window] == nil, "opening attached the current unrelated window")
  assert(vim.api.nvim_get_current_win() == unrelated_window and vim.api.nvim_get_current_buf() == source)
  assert(vim.wo[unrelated_window].number, "opening altered unrelated window presentation")
  status.close(visible)
  await(function() return closed_document[visible.document] end, "background close did not settle")
  assert(vim.api.nvim_win_is_valid(unrelated_window), "closing status removed the unrelated window")

  local attached_before_close = #context_attachment
  local closed = status.open({ workspace = workspace })
  status.close(closed)
  pending_open[closed.document]()
  await(function() return closed_document[closed.document] end, "pending close did not collect host document")
  assert(not closed.active and not vim.api.nvim_buf_is_valid(closed.replica.buffer))
  assert(#context_attachment == attached_before_close, "late response attached context to a closed document")
end, debug.traceback)

status._set_runner_for_test(nil)
startup_log.path = original_startup_log_path
vim.fn.delete(startup_log_path)
package.loaded["forge.views.status.status_context"] = original_context
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("status opening lifecycle passed")
vim.cmd("qa!")
