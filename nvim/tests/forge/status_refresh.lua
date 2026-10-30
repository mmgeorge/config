local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
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
