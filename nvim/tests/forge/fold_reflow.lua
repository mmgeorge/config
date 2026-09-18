vim.loader.enable(false)
local buffer = require("forge.buffer")
local input = require("forge.input")
local replica = buffer.open("fold-reflow")
local function snapshot(extra)
  local function fold(id, endpoint, count)
    return { id = id, start = { row = 0, column = 0 },
      ["end"] = { block = endpoint, position = { row = count, column = 0 } }, closed = true }
  end
  local function block(id, text, folds)
    return { id = id, text = text, metadata = { target = {}, decoration = {}, editable_region = {}, fold = folds or {} } }
  end
  local blocks = {
    block("activity", extra and { "activity", "wrapped heading" } or { "activity" }, { fold("activity", "tail", 1) }),
    block("tool", { "tool" }, { fold("tool", "output", 2) }),
    block("output", { "output", "output tail" }),
    block("tail", { "activity tail" }),
    block("changes", { "changes" }, { fold("changes", "source", 2) }),
    block("file", extra and { "file", "wrapped path" } or { "file" }, { fold("file", "source", 2) }),
    block("source", { "source", "source tail" }),
  }
  local text, metadata = {}, {}
  for _, block in ipairs(blocks) do
    vim.list_extend(text, block.text)
    metadata[#metadata + 1] = { block = block.id, row_count = #block.text, metadata = block.metadata }
  end
  return blocks, text, metadata
end
local initial, initial_text = snapshot(false)
assert(buffer.apply_snapshot(replica, { document = replica.document, revision = 0, block = initial }).kind == "Applied")
vim.api.nvim_set_current_buf(replica.buffer)
local first_window = vim.api.nvim_get_current_win()
local first_view = input.open(replica, first_window)
vim.cmd("6foldopen")
vim.api.nvim_win_set_cursor(0, { 6, 0 })
vim.cmd.vsplit()
local second_window = vim.api.nvim_get_current_win()
local second_view = input.open(replica, second_window)
vim.cmd("1foldopen")
local base_rows = #initial_text
for revision = 1, 4 do
  local expanded = revision % 2 == 1
  local _, text, metadata = snapshot(expanded)
  local patch = { document = replica.document, base = revision - 1, next = revision,
    base_rows = base_rows, next_rows = #text, base_blocks = 7, next_blocks = 7,
    block_edit = {}, removed_block = {}, text_edit = { { start_row = 0, removed_rows = base_rows, text = text } },
    metadata_edit = metadata,
  }
  assert(buffer.apply_patch(replica, patch).kind == "Applied")
  vim.cmd.redraw()
  vim.wait(10)
  local changes = expanded and 7 or 6
  assert(vim.api.nvim_win_call(first_window, function() return vim.fn.foldclosed(1) end) == 1,
    "normal reflow opened the completed activity")
  assert(vim.api.nvim_win_call(first_window, function() return vim.fn.foldclosed(changes) end) == -1,
    "normal reflow closed the expanded change summary")
  assert(vim.api.nvim_win_call(second_window, function() return vim.fn.foldclosed(1) end) == -1,
    "normal reflow lost the second window's expanded activity")
  assert(vim.api.nvim_win_call(second_window, function() return vim.fn.foldclosed(changes) end) == changes,
    "normal reflow opened the second window's collapsed changes")
  base_rows = #text
end
input.close(first_view)
input.close(second_view)
buffer.close(replica)
print("fold_reflow: passed")
