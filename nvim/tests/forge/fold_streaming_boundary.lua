vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")

local function metadata(definition)
  return { target = {}, decoration = {}, editable_region = {}, fold = definition or {} }
end

local function fold(id, endpoint, count, closed)
  return { id = id, start = { row = 0, column = 0 },
    ["end"] = { block = endpoint, position = { row = count, column = 0 } }, closed = closed }
end

local owner = buffer.open("streaming-fold-boundary")
vim.api.nvim_set_current_buf(owner.buffer)
assert(buffer.apply_snapshot(owner, { document = owner.document, revision = 0, block = {
  { id = "prompt", text = { "prompt" }, metadata = metadata() },
  { id = "exchange", text = { "exchange" }, metadata = metadata({ fold("exchange", "end", 0, false) }) },
  { id = "thought", text = { "thought" }, metadata = metadata() },
  { id = "group", text = { "tools" }, metadata = metadata({ fold("group", "end", 0, false) }) },
  { id = "first-tool", text = { "first tool" }, metadata = metadata() },
  { id = "tool", text = { "current tool" }, metadata = metadata({ fold("tool", "end", 0, false) }) },
  { id = "output", text = {}, metadata = metadata() },
  { id = "end", text = {}, metadata = metadata() },
  { id = "sibling", text = { "sibling", "body" }, metadata = metadata({ fold("sibling", "sibling", 2, true) }) },
} }).kind == "Applied")
local first_window = vim.api.nvim_get_current_win()
folds.attach(owner, first_window)
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
folds.attach(owner, second_window)

local previous = 0
for _, count in ipairs({ 4, 7, 2, 0, 4 }) do
  local output = {}
  for row = 1, count do output[row] = "output " .. row end
  local result = buffer.apply_patch(owner, {
    document = owner.document, base = owner.revision, next = owner.revision + 1,
    base_rows = 8 + previous, next_rows = 8 + count, base_blocks = 9, next_blocks = 9,
    block_edit = {}, removed_block = {},
    text_edit = { { start_row = 6, removed_rows = previous, text = output } },
    metadata_edit = { { block = "output", row_count = count, metadata = metadata() } },
  })
  assert(result.kind == "Applied", vim.inspect(result))
  for _, window in ipairs({ first_window, second_window }) do
    vim.api.nvim_win_call(window, function()
      for row = 6, 6 + count do
        assert(vim.fn.foldlevel(row) == 3,
          ("streamed output escaped its enclosing folds at row %d: expected 3, got %d"):format(row, vim.fn.foldlevel(row)))
      end
      assert(vim.fn.foldclosed(7 + count) == 7 + count,
        "streaming output changed its sibling's closed state")
    end)
  end
  previous = count
end

buffer.close(owner)
print("streaming fold boundaries: nested append, resize, removal, and split views passed")
vim.cmd("qa!")
