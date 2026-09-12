local fixture = dofile(vim.fn.getcwd() .. "/nvim/tests/forge/support/status_fixture.lua")
local cwd = vim.fn.getcwd()
vim.opt.runtimepath:prepend(cwd .. "/nvim")
package.path = cwd .. "/nvim/lua/?.lua;" .. cwd .. "/nvim/lua/?/init.lua;" .. package.path

local status = require("forge.status")
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
  assert(method == "status")
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
        syntax_diagnostic = "Syntax highlighting unavailable: MemoryLimit",
        patch = { document = "body:1:1", base = 1, next = 2, base_rows = 1, next_rows = 2, base_blocks = 1, next_blocks = 2,
          text_edit = { { start_row = 1, removed_rows = 0, text = { "row 2" } } },
          block_edit = { { start_block = 1, removed_blocks = 0, inserted = { "body2" } } },
          metadata_edit = { { block = "body2", row_count = 1, metadata = fixture.metadata("hunk:2", 1) } }, removed_block = {} } })
    end
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
local state = status.open({ workspace = cwd, keymaps = { discard = false } })
assert(vim.wait(3000, function() return state.ready end))
vim.api.nvim_win_set_cursor(0, { 4, 0 })
vim.cmd("normal! za")
status.demand(state)
assert(vim.wait(3000, function() return deliveries == 2 and state.replica.row_count == 6 end, 5), "automatic demand continuation did not complete")
assert(vim.deep_equal(vim.api.nvim_buf_get_lines(state.replica.buffer, 0, -1, true), { "main abc", "", "Unstaged changes (1):", "Modified source.txt", "row 1", "row 2" }))
assert(#notices == 0, table.concat(notices, "\n"))
assert(not vim.wo.number and vim.wo.wrap and vim.wo.conceallevel == 0 and vim.wo.foldcolumn == "0")
assert(vim.wo.statuscolumn == "", "Status retained a continuation-line margin")
assert(require("forge.width").capture(0).columns == vim.api.nvim_win_get_width(0),
  "Status projected rows one column narrower than the legacy view")
local ordinary = vim.api.nvim_create_buf(true, false)
vim.api.nvim_win_set_buf(0, ordinary)
assert(vim.wo.number and not vim.wo.wrap and vim.wo.conceallevel == 2 and vim.wo.foldcolumn == "1", "Status window options leaked into the next buffer")
vim.api.nvim_win_set_buf(0, state.replica.buffer)
status.demand(state)
assert(vim.wait(1000, function() return not vim.wo.number end))
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
status.close(state)
assert(not state.active and state.replica.status == "Closed")
assert(vim.wo.number and not vim.wo.wrap and vim.wo.conceallevel == 2 and vim.wo.foldcolumn == "1", "Status close did not restore window presentation")
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
if not ok then
  io.stderr:write(tostring(failure), "\n")
  vim.cmd("cquit 1")
end
vim.cmd("qa!")
