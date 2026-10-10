vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.nodes")
local input = require("forge.input")
local block = {}
for index = 1, 3000 do
  local id = "history:" .. index
  local text = {}
  for row = 1, 10 do text[row] = "history " .. index .. " row " .. row end
  block[index] = { id = id, text = text, metadata = {
    target = {}, decoration = {}, editable_region = {},
    fold = { { id = "fold:" .. index, start = { row = 0, column = 0 },
      ["end"] = { block = id, position = { row = 10, column = 0 } }, closed = true } },
  } }
end
local replica = buffer.open("cooperative-snapshot", { preserve_view = true, editable = { send = function() return true end } })
vim.api.nvim_set_current_buf(replica.buffer)
local view = input.open(replica, vim.api.nvim_get_current_win())
local tick, result = 0, nil
local timer = vim.uv.new_timer()
timer:start(0, 1, vim.schedule_wrap(function() tick = tick + 1 end))
buffer.apply_async(replica, { document = replica.document, revision = 7, block = block },
  function() return true end, function(adopted) result = adopted end)
assert(replica.update_pending, "large snapshot finished without yielding")
assert(not input.capture(replica, view, "activate"), "partial snapshot accepted input")
assert(vim.wait(10000, function() return result ~= nil end, 1), "snapshot did not settle")
assert(result.kind == "Applied", result.diagnostic)
assert(tick > 20, "snapshot blocked the main loop")
assert(replica.revision == 7 and replica.row_count == 3000)
assert(vim.api.nvim_buf_get_lines(replica.buffer, 2999, 3000, true)[1] == "history 3000 row 1")
assert(folds.closed(replica, "fold:3000"), "scheduled projection lost the last node")
assert(not vim.bo[replica.buffer].modifiable)
assert(replica.update_timing.maximum_prepare_ms < 50, "snapshot preparation exceeded 50ms")
assert(replica.update_timing.maximum_commit_ms < 250, "atomic snapshot commit exceeded 250ms")
local snapshot_timing = vim.deepcopy(replica.update_timing)
local recovering
vim.api.nvim_win_set_cursor(view.window, { 1001, 0 })
local write_lines = vim.api.nvim_buf_set_lines
local recovery_writes = 0
vim.api.nvim_buf_set_lines = function(native, ...)
  local result = write_lines(native, ...)
  if native == replica.buffer then
    recovery_writes = recovery_writes + 1
  end
  return result
end
local recovery_block = { { id = "new-heading", text = { "New heading", "New context" },
  metadata = { target = {}, decoration = {}, editable_region = {}, fold = {} } } }
vim.list_extend(recovery_block, block)
buffer.apply_async(replica, { document = replica.document, revision = 8, block = recovery_block },
  function() return true end, function(adopted) recovering = adopted end)
vim.api.nvim_win_set_cursor(view.window, { 91, 0 })
assert(vim.wait(10000, function() return recovering ~= nil end, 1))
vim.api.nvim_buf_set_lines = write_lines
assert(recovering.kind == "Applied", recovering.diagnostic)
assert(vim.api.nvim_win_get_cursor(view.window)[1] == 93,
  "recovery lost the reader's latest cursor identity when rows were inserted above it")
assert(vim.api.nvim_get_current_line() == "history 91 row 1", "recovery selected a different history entry")
assert(replica.update_timing.maximum_prepare_ms < 50, "recovery preparation exceeded 50ms")
assert(recovery_writes == 1, "snapshot cleared or incrementally refilled the live buffer")
assert(folds.closed(replica, "fold:3000"))
local patch_result
local replacement = {}
for row = 1, 30000 do replacement[row] = "expanded output row " .. row end
buffer.apply_async(replica, {
  document = replica.document, base = 8, next = 9,
  base_rows = 30002, next_rows = 30000, base_blocks = 3001, next_blocks = 1,
  block_edit = { { start_block = 0, removed_blocks = 3001, inserted = { "expanded" } } },
  removed_block = vim.tbl_map(function(entry) return entry.id end, recovery_block),
  metadata_edit = { { block = "expanded", row_count = 30000, metadata = { target = {}, decoration = {}, editable_region = {}, fold = {} } } },
  text_edit = { { start_row = 0, removed_rows = 30002, text = replacement } },
}, function() return true end, function(adopted) patch_result = adopted end)
assert(vim.wait(10000, function() return patch_result ~= nil end, 1))
assert(patch_result.kind == "Applied", patch_result.diagnostic)
assert(replica.update_timing.maximum_prepare_ms < 50, "replacement preparation exceeded 50ms")
assert(replica.revision == 9 and replica.sequence:count() == 1)
assert(vim.api.nvim_buf_get_lines(replica.buffer, 29999, 30000, true)[1] == replacement[30000])
timer:stop()
timer:close()
input.close(view)
buffer.close(replica)

local cancelled = buffer.open("cancelled-snapshot")
local alive, cancellation = true, nil
buffer.apply_async(cancelled, { document = cancelled.document, revision = 1, block = block },
  function() return alive end, function(adopted) cancellation = adopted end)
alive = false
assert(vim.wait(1000, function() return cancellation ~= nil end, 1))
assert(cancellation.kind ~= "Applied", "cancelled work published a revision")
assert(cancelled.revision == nil and cancelled.status ~= "Applied")
buffer.close(cancelled)
local closing = buffer.open("closed-snapshot")
buffer.apply_async(closing, { document = closing.document, revision = 1, block = block },
  function() return true end, function() end)
buffer.close(closing)
assert(vim.wait(1000, function() return not closing.update_pending end, 1))
assert(closing.status == "Closed")
local interrupted = buffer.open("interrupted-snapshot")
local mutation_alive, interrupted_result = true, nil
local original_set_lines = vim.api.nvim_buf_set_lines
local writes = 0
vim.api.nvim_buf_set_lines = function(native, ...)
  local result = original_set_lines(native, ...)
  if native == interrupted.buffer then
    writes = writes + 1
    if writes == 1 then mutation_alive = false end
  end
  return result
end
buffer.apply_async(interrupted, { document = interrupted.document, revision = 1, block = block },
  function() return mutation_alive end, function(adopted) interrupted_result = adopted end)
assert(vim.wait(10000, function() return interrupted_result ~= nil end, 1))
vim.api.nvim_buf_set_lines = original_set_lines
assert(interrupted_result.kind == "Applied" and interrupted.revision == 1,
  "cancellation inside the commit exposed an unfinished publication")
assert(writes == 1, "atomic snapshot used more than one text replacement")
assert(not vim.bo[interrupted.buffer].modifiable)
buffer.close(interrupted)
print("cooperative_snapshot: passed, " .. tick .. " main-loop ticks; initial timing " .. vim.json.encode(snapshot_timing))
vim.cmd("qa!")
