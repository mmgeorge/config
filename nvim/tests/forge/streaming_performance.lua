vim.loader.enable(false)
local buffer = require("forge.buffer")
local folds = require("forge.nodes")
local session = buffer.open("streaming-performance", { source_projected = true, editable = { send = function() return true end } })
vim.api.nvim_set_current_buf(session.buffer)
local metadata = function() return { target = {}, decoration = {}, editable_region = {}, fold = {} } end
local block = {}
for index = 1, 3000 do
  local text = {}
  for row = 1, 10 do text[row] = "settled history " .. index .. " row " .. row end
  block[index] = { id = "history:" .. index, text = text, metadata = metadata() }
end
block[3001] = { id = "tool", text = { "tool heading", "first", "second", "third", "fourth", "…(1 hidden)" }, metadata = metadata() }
block[3001].metadata.fold = { { id = "tool-fold", start = { row = 0, column = 0 },
  ["end"] = { block = "tool", position = { row = 6, column = 0 } }, closed = false } }
assert(buffer.apply_snapshot(session, { document = session.document, revision = 0, block = block }).kind == "Applied")
folds.attach(session, vim.api.nvim_get_current_win())
local native = session.editable.native
local retained_history = session.sequence.node["history:1"]
local times, maximum_visits = {}, 0
for revision = 1, 100 do
  session.sequence.visits, native.shadow.sequence.visits = 0, 0
  local started = vim.uv.hrtime()
  local result = buffer.apply_patch(session, {
    document = session.document, base = revision - 1, next = revision,
    base_rows = 30006, next_rows = 30006, base_blocks = 3001, next_blocks = 3001,
    block_edit = {}, removed_block = {}, metadata_edit = {},
    text_edit = { { start_row = 30005, removed_rows = 1, text = { "…(" .. revision .. " hidden)" } } },
  })
  times[#times + 1] = (vim.uv.hrtime() - started) / 1e6
  assert(result.kind == "Applied", result.diagnostic)
  maximum_visits = math.max(maximum_visits, session.sequence.visits)
  assert(session.sequence.visits < 200, "streaming traversed retained history")
  assert(native.shadow.sequence.visits < 150, "streaming traversed the editable shadow")
  assert(session.sequence.node["history:1"] == retained_history, "streaming replaced retained history")
  assert(not folds.closed(session, "tool-fold"), "streaming closed the active tool")
end
table.sort(times)
local result = { rows = 30006, updates = #times, p95_ms = times[95], maximum_ms = times[100], maximum_sequence_visits = maximum_visits }
print(vim.json.encode(result))
assert(result.p95_ms < 16 and result.maximum_ms < 50, "streaming exceeded the interactive callback budget")
buffer.close(session)
vim.cmd("qa!")
