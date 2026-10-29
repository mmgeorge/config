local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
local workspace = vim.fn.getcwd()
vim.opt.runtimepath:prepend(workspace .. "/nvim")

local status = require("forge.status")
local startup_log = require("forge.startup_log")
local startup_log_path = vim.fn.tempname()
local original_startup_log_path = startup_log.path
startup_log.path = function() return startup_log_path end
local original_context = package.loaded["forge.views.status.status_context"]
local client = require("forge.client")
local original_start = client.host_start
client.host_start = function() end
local context_attachment = {}
local refocus_checks = 0
local context_refresh = {}
package.loaded["forge.views.status.status_context"] = {
  attach = function(options)
    context_attachment[#context_attachment + 1] = options
    return { refresh = function() context_refresh[options.document_id] = true end,
      refocus = function() refocus_checks = refocus_checks + 1 end, close = function() end }
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
  elseif params.operation == "snapshot" then
    callback(fixture.snapshot(params.document, { context = { workspace = workspace, branch = "main", recent = {}, issues = {}, branch_prefix = "" } }))
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
  local hidden = status.open({ workspace = workspace, bind = false, on_ready = function() ready = true end })
  local loading_namespace = vim.api.nvim_create_namespace("ForgeStatusLoading")
  assert(#vim.api.nvim_buf_get_extmarks(hidden.replica.buffer, loading_namespace, 0, -1, {}) == 0,
    "startup created a loading placeholder")
  assert(vim.api.nvim_get_current_buf() == source, "pending opening replaced usable content")
  pending_open[hidden.document]()
  await(function() return ready end, "hidden opening did not complete initialization")
  await(function()
    if vim.fn.filereadable(startup_log_path) == 0 then return false end
    for _, row in ipairs(vim.fn.readfile(startup_log_path)) do
      local ok, record = pcall(vim.json.decode, row)
      if ok and record.event == "status.ready" then return true end
    end
    return false
  end, "startup timing did not flush")
  local timing_event = {}
  for _, row in ipairs(vim.fn.readfile(startup_log_path)) do
    local record = vim.json.decode(row)
    timing_event[record.event] = record.fields
  end
  assert(timing_event["status.buffer.opened"], "buffer creation timing was not logged")
  assert(not timing_event["status.loading.presented"], "loading placeholder was presented")
  assert(timing_event["status.buffer.prepared"], "buffer preparation timing was not logged")
  assert(timing_event["status.snapshot.applied"].files == 1, "snapshot application size was not logged")
  assert(timing_event["status.snapshot.applied"].rows == 7, "snapshot application row count was not logged")
  assert(timing_event["status.ready"].elapsed_ms >= 0, "ready timing was not logged")
  assert(hidden.replica.status == "Applied")
  assert(next(hidden.view) == nil, "hidden document attached an unrelated window")
  assert(vim.api.nvim_get_current_buf() == source and vim.wo[source_window].number)
  vim.api.nvim_win_set_buf(source_window, hidden.replica.buffer)
  status.demand(hidden)
  await(function() return hidden.view[source_window] ~= nil end, "redisplayed document did not attach its view")
  local about_source = context_attachment[#context_attachment].get_about_source()
  assert(#about_source.file == 1 and about_source.file[1].generation == 1, "About source omitted visible file generation")
  vim.api.nvim_set_current_win(source_window)
  local before_refocus = refocus_checks
  vim.api.nvim_exec_autocmds("FocusGained", {})
  await(function() return refocus_checks > before_refocus end, "refocus did not check About after refresh")
  status.close(hidden)
  await(function() return closed_document[hidden.document] end, "hidden document close did not settle")

  vim.api.nvim_win_set_buf(source_window, source)
  ready = false
  local visible = status.open({ workspace = workspace, on_ready = function() ready = true end })
  assert(vim.api.nvim_get_current_buf() == source, "pending status replaced the current buffer")
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

  local closed_ready = status.open({ workspace = workspace, bind = false, on_ready = function(state)
    assert(not context_refresh[state.document], "background context delayed document readiness")
    status.close(state)
  end })
  pending_open[closed_ready.document]()
  await(function() return closed_document[closed_ready.document] end, "ready document close did not settle")
  assert(not context_refresh[closed_ready.document], "closed document started deferred background work")

  vim.api.nvim_win_set_buf(unrelated_window, source)
  vim.api.nvim_set_current_win(unrelated_window)
  local commands = require("forge.views.commands")
  local pending_command = commands.open()
  assert(commands.open() == pending_command, "repeated command opened a second pending document")
  assert(vim.api.nvim_get_current_buf() == source, "repeated command displayed an empty pending document")
  status.close(pending_command)
  pending_open[pending_command.document]()
  await(function() return closed_document[pending_command.document] end, "pending command close did not settle")
end, debug.traceback)

status._set_runner_for_test(nil)
client.host_start = original_start
startup_log.path = original_startup_log_path
vim.fn.delete(startup_log_path)
package.loaded["forge.views.status.status_context"] = original_context
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("status opening lifecycle passed")
vim.cmd("qa!")
