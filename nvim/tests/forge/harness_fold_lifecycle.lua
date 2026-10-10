vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")
local owner = buffer.open("harness-fold-lifecycle")
vim.api.nvim_set_current_buf(owner.buffer)
local function metadata(id, endpoint, rows)
  return { target = {}, decoration = {}, editable_region = {}, fold = id and {
    { id = id, start = { row = 0, column = 0 },
      ["end"] = { block = endpoint, position = { row = rows, column = 0 } }, closed = false },
  } or {} }
end
local function snapshot(revision, tool_rows)
  return { document = owner.document, revision = revision, block = {
    { id = "prompt", text = { "Accept plan" }, metadata = metadata() },
    { id = "summary", text = { "Plan implementation" }, metadata = metadata("exchange", "tools", #tool_rows) },
    { id = "thought", text = { "Inspecting the workspace" }, metadata = metadata() },
    { id = "tools", text = tool_rows, metadata = metadata("tools", "tools", #tool_rows) },
    { id = "status", text = { "Implementing" }, metadata = metadata() },
  } }
end
local tool_rows = { "Running 1 tool", "Get-Content src/lib.rs", "no output" }
assert(buffer.apply_snapshot(owner, snapshot(0, tool_rows)).kind == "Applied")
local first_window = vim.api.nvim_get_current_win()
folds.attach(owner, first_window)
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
folds.attach(owner, second_window)

local function check()
  local last = 3 + #tool_rows
  for _, window in ipairs({ first_window, second_window }) do
    vim.api.nvim_win_call(window, function()
      for row = 1, last + 1 do
        local expected = row >= 4 and row <= last and 2 or row >= 2 and row <= last and 1 or 0
        assert(vim.fn.foldlevel(row) == expected,
          ("revision %d, row %d: expected fold depth %d, got %d"):format(owner.revision, row, expected, vim.fn.foldlevel(row)))
      end
    end)
  end
  assert(vim.deep_equal(vim.api.nvim_buf_get_lines(owner.buffer, 3, last, true), tool_rows))
end

local function patch(text_edit, metadata_edit, next_rows)
  local result = buffer.apply_patch(owner, {
    document = owner.document, base = owner.revision, next = owner.revision + 1,
    base_rows = owner.row_count, next_rows = next_rows or owner.row_count,
    base_blocks = 5, next_blocks = 5, block_edit = {}, removed_block = {},
    text_edit = text_edit, metadata_edit = metadata_edit or {},
  })
  assert(result.kind == "Applied", vim.inspect(result))
  check()
end

check()
for step = 1, 12 do
  folds.set_open(owner, first_window, "exchange", step % 2 == 0)
  folds.set_open(owner, second_window, "tools", step % 3 == 0)
  tool_rows[#tool_rows] = step % 2 == 0 and "" or ("output " .. step .. " λ")
  patch({ { start_row = 2 + #tool_rows, removed_rows = 1, text = { tool_rows[#tool_rows] } } })
  patch({ { start_row = 1, removed_rows = 1, text = { "Plan implementation " .. step .. "s" } } })
end

for _, count in ipairs({ 5, 2, 4, 3 }) do
  local previous_count = #tool_rows
  tool_rows = { "Ran 1 tool" }
  for index = 2, count do tool_rows[index] = "output " .. index end
  patch({ { start_row = 3, removed_rows = previous_count, text = tool_rows } }, {
    { block = "tools", row_count = count, metadata = metadata("tools", "tools", count) },
    { block = "summary", row_count = 1, metadata = metadata("exchange", "tools", count) },
  }, 4 + count)
  folds.release(second_window)
  folds.attach(owner, second_window)
  check()
end
assert(buffer.apply_snapshot(owner, snapshot(owner.revision + 1, tool_rows)).kind == "Applied")
check()
buffer.close(owner)
print("harness fold lifecycle: streaming, open/close, grow/shrink, split reattach, and snapshot passed")
vim.cmd("qa!")
