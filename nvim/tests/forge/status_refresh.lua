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
local function snapshot(document, revision, label)
  return fixture.snapshot(document, { revision = revision, path = label })
end
status._set_runner_for_test(function(_, params, callback)
  if params.operation == "open" then callback(snapshot(params.document, 0, "old counts"))
  elseif params.operation == "refresh" then refresh_count = refresh_count + 1; pending[#pending + 1] = callback
  elseif params.operation == "snapshot" then callback(snapshot(params.document, 2, "new counts"))
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
table.remove(pending, 1)(nil)
assert(vim.wait(3000, function() return not state.refresh_active end))
status.close(state)
perf.event = original_event
print("status_refresh passed")
vim.cmd("qa!")
