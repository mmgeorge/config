-- Unit tests for `forge.status` (status.lua). Each scenario row runs in its own
-- headless Neovim: `.forge/checks/lua-tests.nu` passes the row's name as `arg[1]`.
local scenarios = {
  { name = "refresh", run = function()
local fixture = dofile(vim.fn.getcwd() .. "/nvim/lua/forge/tests/support/status_fixture.lua")
local root = vim.fn.getcwd()
vim.opt.runtimepath:prepend(root .. "/nvim")
local status = require("forge.status")
local perf = require("forge.infra.perf")
local original_event = perf.event
local event_list = {}
perf.event = function(_, event, payload)
  if event:match("^status%.refresh%.") then event_list[#event_list + 1] = { event = event, payload = payload } end
end
local pending, refresh_count, completed = {}, 0, 0
local snapshot_count, snapshot_revision, snapshot_hook = 0, 2, nil
local function snapshot(document, revision, label)
  return fixture.snapshot(document, { revision = revision, path = label })
end
status._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then callback(snapshot(params.document, 0, "old counts"))
  elseif params.operation == "refresh" then refresh_count = refresh_count + 1; pending[#pending + 1] = callback
  elseif params.operation == "snapshot" then
    snapshot_count = snapshot_count + 1
    local value = snapshot(params.document, snapshot_revision, "new counts")
    if snapshot_hook then snapshot_hook(value, callback) else callback(value) end
  elseif params.operation == "close" then callback({ closed = true })
  elseif params.operation == "close_view" then callback(nil)
  else error("unexpected request: " .. params.operation) end
end)
local state = status.open({ workspace = root, bind = false })
assert(vim.wait(3000, function() return state.ready end))
local function finished(success) assert(success); completed = completed + 1 end
status.refresh(state, finished)
status.refresh(state, finished)
status.refresh(state, finished)
assert(refresh_count == 1, "refreshes did not coalesce")
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true)[4] == "Modified old counts")
table.remove(pending, 1)({ document = state.document, base = 0, next = 1 })
assert(vim.wait(3000, function() return refresh_count == 2 end))
assert(completed == 0, "superseded refresh completed callbacks")
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true)[4] == "Modified old counts", "stale result replaced counts")
table.remove(pending, 1)(nil)
assert(vim.wait(3000, function() return completed == 3 end))
assert(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true)[4] == "Modified new counts")
local expected = { "start", "request", "response", "superseded", "request", "response",
  "snapshot.request", "snapshot.response", "snapshot.applied", "complete" }
assert(#event_list == #expected, "refresh timing count differs")
for index, event in ipairs(event_list) do
  assert(event.event == "status.refresh." .. expected[index], "refresh timing order differs: " .. event.event)
  assert(event.payload.request_id == event_list[1].payload.request_id, "coalesced refresh lost timing correlation")
  assert(event.payload.session_id == state.document and event.payload.elapsed_ms >= 0)
end
vim.api.nvim_exec_autocmds("BufWritePost", { pattern = root .. "/source.rs", modeline = false })
assert(vim.wait(3000, function() return refresh_count == 3 end), "save did not invalidate counts")
local unchanged_tick = state.replica.changedtick
table.remove(pending, 1)(nil)
assert(vim.wait(3000, function() return not state.refresh_active end))
assert(state.replica.changedtick == unchanged_tick, "unchanged refresh redrew the buffer")
local render = require("forge.status_render")
local original_apply_snapshot = render.apply_snapshot
local notices = {}
local original_notify = vim.notify
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
snapshot_hook = function(value, callback)
  local record = fixture.file("new counts")
  record.section = "staged"
  status.apply_update(state, { document = state.document, operation_id = 1, phase = "accepted", body = {}, delta = {
    document = state.document, base = 2, next = 3, removed = {}, file = { record }, pending = {},
    section = { { kind = "unstaged", file = {} }, { kind = "staged", file = { 1 } } },
  } })
  callback(value)
end
state.refresh_recover = true
status.refresh(state, finished)
assert(refresh_count == 4)
table.remove(pending, 1)(nil)
assert(vim.wait(1000, function() return completed == 4 end, 5), "obsolete recovery retained its callback")
assert(refresh_count == 4 and snapshot_count == 2, "caught-up state triggered another Git refresh")
assert(not state.refresh_active and not state.refresh_recover)
assert(state.replica.inventory.revision == 3 and state.replica.inventory.section[1].kind == "staged")

snapshot_hook, snapshot_revision = nil, 4
local supersede = true
render.apply_snapshot = function(replica, value, callback)
  if supersede and callback then
    supersede = false
    callback({ kind = "Superseded" })
    return
  end
  return original_apply_snapshot(replica, value, callback)
end
state.refresh_recover = true
status.refresh(state, finished)
assert(refresh_count == 5)
table.remove(pending, 1)(nil)
assert(vim.wait(1000, function() return refresh_count == 6 end, 5), "missing state did not retain recovery")
assert(completed == 4 and state.refresh_active and state.refresh_recover)
table.remove(pending, 1)(nil)
assert(vim.wait(1000, function() return completed == 5 end, 5), "retained recovery did not settle")
assert(state.replica.inventory.revision == 4 and not state.refresh_recover)
assert(refresh_count == 6 and snapshot_count == 4, "retained recovery repeated after catching up")
snapshot_revision = 3
snapshot_hook = function(value, callback)
  snapshot_revision, snapshot_hook = 4, nil
  callback(value)
end
state.replica.status = "Desynchronized"
state.replica.recover()
assert(vim.wait(1000, function() return state.replica.status == "Applied" and not state.recovering end, 5),
  "outdated snapshot abandoned required desynchronization recovery")
assert(snapshot_count == 6, "desynchronization recovery did not request one replacement snapshot")
assert(#notices == 0, table.concat(notices, "\n"))
render.apply_snapshot, vim.notify = original_apply_snapshot, original_notify
status.close(state)
perf.event = original_event
print("status_refresh passed")
vim.cmd("qa!")
  end },
  { name = "body_recovery", run = function()
vim.loader.enable(false)
local fixture = dofile("nvim/lua/forge/tests/support/status_fixture.lua")
local status = require("forge.status")
local notices = {}
local original_notify = vim.notify
vim.notify = function(message) notices[#notices + 1] = tostring(message) end
local state
local demand_count, recovery_count = 0, 0
local snapshot_failure = false
local patch_base, recovery_revision = 1, 2
local ok, failure = xpcall(function()
  status._set_runner_for_test(function(_, params, callback)
    if params.operation == "open" or params.operation == "snapshot" then
      local snapshot = fixture.snapshot(params.document)
      if params.operation == "snapshot" then snapshot.file[1].generation = 2 end
      callback(snapshot)
    elseif params.operation == "demand" then
      demand_count = demand_count + 1
      local delivery = fixture.body(params.input.document, { "highlighted source" }, { more = demand_count <= 2 })
      if demand_count > 1 then
        delivery.generation = snapshot_failure and 3 or 2
        delivery.snapshot.document = "body:1:" .. delivery.generation
      end
      if demand_count > 2 then
        delivery.snapshot = nil
        delivery.patch = { document = "body:1:" .. delivery.generation, base = patch_base, next = patch_base + 1 }
      end
      callback(delivery)
    elseif params.operation == "body_snapshot" then
      recovery_count = recovery_count + 1
      if snapshot_failure then callback(nil, "body snapshot unavailable") return end
      assert(params.generation == 2)
      local delivery = fixture.body(params.document, { "highlighted source" })
      delivery.generation, delivery.snapshot.document = 2, "body:1:2"
      delivery.snapshot.revision = recovery_revision
      callback(delivery)
    elseif params.operation == "close" then callback({ closed = true })
    elseif params.operation == "close_view" then callback(nil)
    else error("unexpected status operation " .. params.operation) end
  end)
  state = status.open({ workspace = vim.fn.getcwd() })
  assert(vim.wait(1000, function() return next(state.view) ~= nil and not state.scheduled end, 5))
  vim.api.nvim_win_set_cursor(0, { 4, 0 })
  vim.fn.maparg("<Tab>", "n", false, true).callback()
  assert(vim.wait(1000, function()
    return state.done[1] and not state.pending and not state.request_active
  end, 5), "body recovery did not settle")
  assert(demand_count == 3 and recovery_count == 1, "missing body did not request one snapshot")
  assert(state.replica.status == "Applied" and state.replica.file[1].body.revision == 2)
  assert(not require("forge.nodes").closed(state.replica, "file:1"), "recovery closed the file")
  assert(table.concat(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false), "\n")
    :find("highlighted source", 1, true), "recovered source was not displayed")
  assert(#notices == 0, table.concat(notices, "\n"))
  patch_base, recovery_revision = 4, 5
  state.done = {}
  status.demand(state)
  assert(vim.wait(1000, function() return state.done[1] and not state.request_active end, 5),
    "revision-gap recovery did not settle")
  assert(recovery_count == 2 and state.replica.file[1].body.revision == 5,
    "retained body revision gap did not request a current snapshot")
  assert(state.replica.status == "Applied" and #notices == 0, table.concat(notices, "\n"))
  assert(not require("forge.nodes").closed(state.replica, "file:1"), "revision-gap recovery closed the file")
  snapshot_failure = true
  local snapshot = vim.deepcopy(state.replica.inventory)
  snapshot.file[1].generation = 3
  assert(require("forge.status_render").apply_snapshot(state.replica, snapshot).kind == "Applied")
  local previous = vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)
  state.done = {}
  status.demand(state)
  assert(vim.wait(1000, function() return #notices > 0 and not state.request_active end, 5))
  assert(#notices == 1 and notices[1] == "body snapshot unavailable", table.concat(notices, "\n"))
  assert(vim.deep_equal(previous, vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, false)),
    "failed recovery replaced the previous view")
  assert(not require("forge.nodes").closed(state.replica, "file:1"), "failed recovery closed the file")
end, debug.traceback)
if state then status.close(state) end
status._set_runner_for_test(nil)
vim.notify = original_notify
if not ok then vim.api.nvim_err_writeln(failure) vim.cmd("cquit 1") end
print("status body recovery: generation catch-up preserves expansion")
vim.cmd("qa!")
  end },
  { name = "native", run = function()
local fixture = dofile(vim.fn.getcwd() .. "/nvim/lua/forge/tests/support/status_fixture.lua")
local cwd = vim.fn.getcwd()
vim.opt.runtimepath:prepend(cwd .. "/nvim")
package.path = cwd .. "/nvim/lua/?.lua;" .. cwd .. "/nvim/lua/?/init.lua;" .. package.path

local status = require("forge.status")
local config = require("forge.infra.config")
local original_open_target = config.options.status_open_target
local source_path = vim.fn.tempname() .. " source.txt"
vim.fn.writefile({ "first", "second" }, source_path)
local requests, notices, deliveries = {}, {}, 0
local selected, selected_input, confirmation, demand_sequence
local original_notify = vim.notify
local confirm = require("forge.infra.confirm")
local original_confirm = confirm.open
vim.notify = function(message) notices[#notices + 1] = tostring(message) end

local ok, failure = xpcall(function()

local function metadata(target)
  return { target = { { id = target, range = { start = { row = 0, column = 0 }, ["end"] = { row = 1, column = 0 } } } }, decoration = {}, editable_region = {} }
end

status._set_runner_for_test(function(method, params, callback)
  assert(method == "repository.status.document")
  requests[#requests + 1] = params.operation
  if params.operation == "open" then
    callback(fixture.snapshot(params.document))
  elseif params.operation == "demand" then
    deliveries = deliveries + 1
    if deliveries == 3 then
      demand_sequence = params.input.sequence
      callback({ document = params.input.document, file = 1, generation = 1, state = { state = "ready" }, more = false })
      return
    end
    assert(deliveries <= 2, "completed visible demand must not repeat")
    assert(params.input.location.kind == "file" and params.input.location.id == 1)
    if deliveries == 1 then
      callback(fixture.body(params.input.document, { "row 1" }, { more = true, block = "body1", target = "hunk:1" }))
    else
      callback({ document = params.input.document, file = 1, generation = 1, more = false, state = { state = "ready" },
        syntax_diagnostic = "Syntax highlighting unavailable: parser allocation failed",
        patch = { document = "body:1:1", base = 1, next = 2, base_rows = 1, next_rows = 2, base_blocks = 1, next_blocks = 2,
          text_edit = { { start_row = 1, removed_rows = 0, text = { "row 2" } } },
          block_edit = { { start_block = 1, removed_blocks = 0, inserted = { "body2" } } },
          metadata_edit = { { block = "body2", row_count = 1, metadata = fixture.metadata("hunk:2", 1) } }, removed_block = {} } })
    end
  elseif params.operation == "open_target" then
    callback({ id = "source-" .. params.input.sequence, kind = "open_file",
      document = params.input.document, revision = params.input.revision,
      view = params.input.view, sequence = params.input.sequence,
      path = vim.fs.normalize(source_path), row = 1, column = 2 })
  elseif params.operation == "input" then
    selected = params.selection
    selected_input = params.input
    callback(nil, "selection captured for test")
  elseif params.operation == "refresh" then callback(nil)
  elseif params.operation == "close_view" then callback(nil)
  elseif params.operation == "close" then callback({ closed = true })
  else error("unexpected status request: " .. params.operation) end
end)

vim.wo.number, vim.wo.wrap, vim.wo.conceallevel, vim.wo.foldcolumn = true, false, 2, "1"
vim.wo.virtualedit = "all"
local state = status.open({ workspace = cwd, keymaps = { discard = false } })
assert(vim.wait(3000, function() return state.ready end))
assert(vim.wo.virtualedit == "", "Status permits virtual cursor columns")
vim.api.nvim_win_set_cursor(0, { 3, 0 })
vim.cmd("normal! 100|")
assert(vim.fn.virtcol(".") == #vim.api.nvim_get_current_line(), "Status cursor escaped the heading text")
vim.api.nvim_win_set_cursor(0, { 4, 0 })
assert(require("forge.nodes").toggle_heading(state.replica, vim.api.nvim_get_current_win()))
status.demand(state)
assert(vim.wait(3000, function() return deliveries == 2 and state.replica.row_count == 6 end, 5), "automatic demand continuation did not complete")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), { "main abc", "", "Unstaged changes (1):", "Modified source.txt", "row 1", "row 2" }))
assert(#notices == 1 and notices[1] == "Syntax highlighting unavailable: parser allocation failed",
  "Status failed to report its syntax failure while retaining the usable view: " .. table.concat(notices, "\n"))
assert(not vim.wo.number and vim.wo.wrap and vim.wo.conceallevel == 0 and vim.wo.foldcolumn == "0")
assert(vim.wo.statuscolumn == "", "Status retained a continuation-line margin")
assert(require("forge.width").capture(0).columns == vim.api.nvim_win_get_width(0),
  "Status projected rows one column narrower than the legacy view")
local ordinary = vim.api.nvim_create_buf(true, false)
vim.api.nvim_win_set_buf(0, ordinary)
assert(vim.wo.number and not vim.wo.wrap and vim.wo.conceallevel == 2 and vim.wo.foldcolumn == "1",
  "Status window options leaked into the next buffer: " .. vim.inspect({ number = vim.wo.number,
    wrap = vim.wo.wrap, conceallevel = vim.wo.conceallevel, foldcolumn = vim.wo.foldcolumn }))
vim.api.nvim_win_set_buf(0, state.replica.buffer)
status.demand(state)
assert(vim.wait(1000, function() return not vim.wo.number end))
assert(vim.wo.virtualedit == "", "Status reentry restored virtual cursor columns")
for _, mapping in ipairs(vim.api.nvim_buf_get_keymap(state.replica.buffer, "n")) do
  assert(mapping.lhs ~= "D", "disabled buffer keymap was installed")
end
status.demand(state)
vim.wait(30, function() return false end, 5)
assert(deliveries == 2)
vim.api.nvim_win_set_cursor(0, { 5, 0 })
vim.cmd("normal! Vj")
status.action(state, "stage", true)
assert(vim.wait(1000, function() return selected ~= nil end, 5))
assert(selected.target[1].block == "body1" and selected.target[1].position.row == 0)
assert(selected.target[2].block == "body2" and selected.target[2].position.row == 0)
assert(vim.wait(1000, function() return not state.request_active end, 5))
confirm.open = function(lines, callback)
  assert(vim.deep_equal(lines, { "Discard ALL changes to file?", "  source.txt" }))
  confirmation = callback
end
vim.api.nvim_win_set_cursor(0, { 4, 0 })
local captured_revision = state.replica.revision
status.action(state, "discard", false)
assert(confirmation, "discard did not request confirmation")
state.done = {}
status.demand(state)
assert(vim.wait(1000, function() return demand_sequence and not state.request_active end, 5))
vim.api.nvim_win_set_cursor(0, { 6, 0 })
selected_input = nil
confirmation()
assert(vim.wait(1000, function() return selected_input ~= nil end, 5))
assert(selected_input.sequence > demand_sequence, "confirmation reused a sequence overtaken by background demand")
assert(selected_input.location.kind == "file" and selected_input.location.id == 1, "confirmation retargeted the moved cursor")
assert(selected_input.revision == captured_revision, "confirmation replaced its captured revision")
confirm.open = original_confirm
local status_window = vim.api.nvim_get_current_win()
local status_tab = vim.api.nvim_get_current_tabpage()
assert(config.options.status_open_target == "tab", "file opening no longer defaults to a tab")
for _, target in ipairs({ "tab", "buffer" }) do
  config.options.status_open_target = target
  vim.api.nvim_set_current_win(status_window)
  vim.api.nvim_win_set_buf(status_window, state.replica.buffer)
  vim.api.nvim_win_set_cursor(status_window, { 4, 0 })
  local tab_count = #vim.api.nvim_list_tabpages()
  status.action(state, "open", false)
  assert(vim.wait(1000, function() return vim.api.nvim_buf_get_name(0) == vim.fs.normalize(source_path) end))
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(0), { 2, 2 }))
  assert(#vim.api.nvim_list_tabpages() == tab_count + (target == "tab" and 1 or 0))
  if target == "tab" then
    assert(vim.api.nvim_win_get_buf(status_window) == state.replica.buffer,
      "opening a file replaced the Status buffer")
    vim.cmd("tabclose")
    assert(vim.api.nvim_get_current_tabpage() == status_tab)
  end
end
config.options.status_open_target = original_open_target
vim.api.nvim_win_set_buf(status_window, state.replica.buffer)
status.close(state)
assert(not state.active and state.replica.status == "Closed")
assert(vim.wo.number and not vim.wo.wrap and vim.wo.conceallevel == 2 and vim.wo.foldcolumn == "1",
  "Status close did not restore window presentation: " .. vim.inspect({ number = vim.wo.number,
    wrap = vim.wo.wrap, conceallevel = vim.wo.conceallevel, foldcolumn = vim.wo.foldcolumn }))
assert(vim.wait(1000, function() return requests[#requests] == "close" end, 5))
local source_buffer = vim.api.nvim_get_current_buf()
vim.bo[source_buffer].buflisted = false
local failed_requests = {}
notices = {}
status._set_runner_for_test(function(_, params, callback)
  failed_requests[#failed_requests + 1] = params.operation
  if params.operation == "open" then callback(nil, "initial observation failed")
  elseif params.operation == "close" then callback(nil, "unknown status document")
  else error("failed opening sent a content request for an absent document") end
end)
local failed_state = status.open({ workspace = cwd })
assert(vim.wait(1000, function() return not failed_state.active end, 5), "failed opening left an active status view")
assert(not vim.api.nvim_buf_is_valid(failed_state.replica.buffer), "failed opening retained an unusable buffer")
assert(vim.api.nvim_get_current_buf() == source_buffer, "failed opening did not restore its source buffer")
assert(vim.wait(1000, function() return not failed_state.request_active end, 5), "failed opening did not finish cleanup")
assert(vim.deep_equal(failed_requests, { "open", "close" }), "failed opening did not attempt uncertain host cleanup")
assert(vim.deep_equal(notices, { "initial observation failed" }), "failed opening lost or duplicated its diagnostic")
status._set_runner_for_test(nil)
vim.notify = original_notify
print("native status demand continuation and cleanup passed")
end, debug.traceback)
vim.notify = original_notify
confirm.open = original_confirm
config.options.status_open_target = original_open_target
vim.fn.delete(source_path)
if not ok then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
vim.cmd("qa!")
  end },
  { name = "open_lifecycle", run = function()
local fixture = dofile(vim.fn.getcwd() .. "/nvim/lua/forge/tests/support/status_fixture.lua")
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
local original_set_buffer = vim.api.nvim_win_set_buf
local original_notify = vim.notify
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

  -- Exercise the public entry from a real pinned window, preserving its draft and ownership.
  local forge = require("forge")
  vim.api.nvim_set_current_win(unrelated_window)
  local pinned = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_name(pinned, "ForgeHarnessInput://status-entry-fixture")
  vim.api.nvim_buf_set_lines(pinned, 0, -1, false, { "Retain this Harness draft café 東京" })
  vim.api.nvim_win_set_buf(unrelated_window, pinned)
  vim.api.nvim_win_set_cursor(unrelated_window, { 1, 5 })
  vim.wo[unrelated_window].winfixbuf = true
  local pinned_cursor = vim.api.nvim_win_get_cursor(unrelated_window)
  local public = forge.open()
  assert(vim.api.nvim_get_current_win() == unrelated_window and vim.api.nvim_get_current_buf() == pinned,
    "pending public Status entry changed the pinned window")
  pending_open[public.document]()
  await(function() return public.ready and #vim.fn.win_findbuf(public.replica.buffer) == 1 end,
    "public Status entry did not mount beside the pinned feature")
  local status_window = vim.fn.win_findbuf(public.replica.buffer)[1]
  assert(status_window ~= unrelated_window and vim.api.nvim_get_current_win() == status_window)
  assert(vim.api.nvim_win_get_buf(unrelated_window) == pinned and vim.wo[unrelated_window].winfixbuf,
    "Status stole the pinned Harness input buffer or removed its pin")
  assert(vim.deep_equal(vim.api.nvim_win_get_cursor(unrelated_window), pinned_cursor)
    and vim.api.nvim_buf_get_lines(pinned, 0, -1, false)[1] == "Retain this Harness draft café 東京")
  vim.api.nvim_set_current_win(unrelated_window)
  local tabs = #vim.api.nvim_list_tabpages()
  assert(forge.open() == public and vim.api.nvim_get_current_win() == status_window,
    "cached Status entry could not focus its existing mount from a pinned input")
  assert(#vim.api.nvim_list_tabpages() == tabs, "cached Status entry created another tab")

  vim.api.nvim_win_set_buf(status_window, source)
  vim.api.nvim_set_current_win(unrelated_window)
  local picker_buffer = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(picker_buffer, 0, -1, false, { "Retain the picker query" })
  local picker_window = vim.api.nvim_open_win(picker_buffer, true, {
    relative = "editor", row = 1, col = 1, width = 40, height = 1, style = "minimal",
  })
  local picker_notices = {}
  vim.notify = function(message) picker_notices[#picker_notices + 1] = tostring(message) end
  assert(forge.open() == public, "cached Status failed from a one-row picker: " .. table.concat(picker_notices, "\n"))
  local mounted_window = vim.api.nvim_get_current_win()
  assert(mounted_window ~= picker_window and vim.api.nvim_win_get_config(mounted_window).relative == "",
    "cached Status mounted inside a transient picker")
  assert(vim.api.nvim_win_get_buf(picker_window) == picker_buffer and vim.wo[picker_window].winbar == "",
    "cached Status changed the picker buffer or presentation")
  assert(vim.api.nvim_buf_get_lines(picker_buffer, 0, -1, false)[1] == "Retain the picker query")
  local mounted_tabs = #vim.api.nvim_list_tabpages()
  vim.api.nvim_set_current_win(picker_window)
  assert(forge.open() == public and vim.api.nvim_get_current_win() == mounted_window,
    "picker entry did not reuse the existing normal Status window")
  assert(#vim.api.nvim_list_tabpages() == mounted_tabs, "picker entry duplicated the Status tab")
  assert(#picker_notices == 0, table.concat(picker_notices, "\n"))
  vim.notify = original_notify
  vim.api.nvim_win_close(picker_window, true)
  vim.api.nvim_buf_delete(picker_buffer, { force = true })
  assert(vim.api.nvim_win_get_buf(mounted_window) == public.replica.buffer,
    "picker cleanup removed the Status mount")
  status.close(public)
  await(function() return closed_document[public.document] end, "public pinned Status close did not settle")
  if vim.api.nvim_win_is_valid(mounted_window) then vim.api.nvim_win_close(mounted_window, true) end
  if vim.api.nvim_win_is_valid(status_window) then vim.api.nvim_win_close(status_window, true) end
  vim.api.nvim_set_current_win(unrelated_window)

  picker_buffer = vim.api.nvim_create_buf(false, true)
  picker_window = vim.api.nvim_open_win(picker_buffer, true, {
    relative = "editor", row = 1, col = 1, width = 40, height = 1, style = "minimal",
  })
  local fresh = forge.open()
  assert(vim.api.nvim_win_get_buf(picker_window) == picker_buffer, "pending Status replaced the picker")
  pending_open[fresh.document]()
  await(function() return fresh.ready end, "fresh Status did not open from a one-row picker")
  mounted_window = vim.api.nvim_get_current_win()
  assert(mounted_window ~= picker_window and vim.api.nvim_win_get_config(mounted_window).relative == "",
    "fresh Status mounted inside a transient picker")
  assert(vim.api.nvim_win_get_buf(picker_window) == picker_buffer and vim.wo[picker_window].winbar == "")
  vim.schedule(function()
    vim.api.nvim_win_close(picker_window, true)
    vim.api.nvim_buf_delete(picker_buffer, { force = true })
  end)
  await(function() return not vim.api.nvim_win_is_valid(picker_window) end, "picker teardown did not settle")
  assert(fresh.active and vim.api.nvim_win_get_buf(mounted_window) == fresh.replica.buffer,
    "deferred picker cleanup removed fresh Status")
  status.close(fresh)
  await(function() return closed_document[fresh.document] end, "fresh picker Status close did not settle")
  if vim.api.nvim_win_is_valid(mounted_window) then vim.api.nvim_win_close(mounted_window, true) end
  vim.api.nvim_set_current_win(unrelated_window)

  -- A failed asynchronous bind must release the cached owner and its newly created window.
  local failed = forge.open()
  local notices = {}
  vim.notify = function(message) notices[#notices + 1] = tostring(message) end
  vim.api.nvim_win_set_buf = function(window, buf)
    if buf == failed.replica.buffer then error("fixture Status bind failure") end
    return original_set_buffer(window, buf)
  end
  local before_failure_tabs = #vim.api.nvim_list_tabpages()
  pending_open[failed.document]()
  await(function() return not failed.active and closed_document[failed.document] end,
    "failed Status binding retained a partial active document")
  vim.api.nvim_win_set_buf = original_set_buffer
  vim.notify = original_notify
  assert(not vim.api.nvim_buf_is_valid(failed.replica.buffer) and not failed.ready)
  assert(#vim.api.nvim_list_tabpages() == before_failure_tabs, "failed binding leaked its new Status tab")
  assert(#notices == 1 and notices[1]:find("fixture Status bind failure", 1, true))
  assert(vim.api.nvim_win_get_buf(unrelated_window) == pinned and vim.wo[unrelated_window].winfixbuf)
  local recovered = forge.open()
  assert(recovered ~= failed, "public entry reused a failed partial Status owner")
  pending_open[recovered.document]()
  await(function() return recovered.ready end, "public Status entry did not recover after a failed bind")
  status.close(recovered)
  await(function() return closed_document[recovered.document] end, "recovered Status close did not settle")
  vim.wo[unrelated_window].winfixbuf = false
end, debug.traceback)

status._set_runner_for_test(nil)
client.host_start = original_start
vim.api.nvim_win_set_buf = original_set_buffer
vim.notify = original_notify
startup_log.path = original_startup_log_path
vim.fn.delete(startup_log_path)
package.loaded["forge.views.status.status_context"] = original_context
if not succeeded then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
print("status opening lifecycle passed")
vim.cmd("qa!")
  end },
}

local selected = arg[1]
for _, scenario in ipairs(scenarios) do
  if scenario.name == selected then return scenario.run() end
end
error(("unknown scenario %q"):format(tostring(selected)))
