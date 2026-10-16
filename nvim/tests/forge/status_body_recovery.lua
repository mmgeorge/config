vim.loader.enable(false)
local fixture = dofile("nvim/tests/forge/support/status_fixture.lua")
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
