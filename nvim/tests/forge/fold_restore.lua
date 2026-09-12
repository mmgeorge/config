vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")
local replica = buffer.open("fold-restore", {})
local function block(id, text, folded)
  return { id = id, text = text, metadata = { target = {}, decoration = {}, editable_region = {},
    fold = folded and { { id = id, start = { row = 0, column = 0 },
      ["end"] = { block = id, position = { row = #text, column = 0 } }, closed = true } } or {} } }
end
local function snapshot(revision, first)
  return { document = replica.document, revision = revision, block = {
    first, block("gap", { "" }), block("retained", { "retained", "second", "third" }, true),
  } }
end
assert(buffer.apply_snapshot(replica, snapshot(0, block("shrinking", { "first", "second", "third" }, true))).kind == "Applied")
vim.api.nvim_win_set_buf(0, replica.buffer)
folds.attach(replica, vim.api.nvim_get_current_win())
assert(vim.fn.foldclosed(1) == 1 and vim.fn.foldclosed(5) == 5)
local first_window = vim.api.nvim_get_current_win()
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
folds.attach(replica, second_window)
vim.cmd("5foldopen")
local captured = folds.capture(replica)
assert(buffer.apply_snapshot(replica, snapshot(1, block("shrinking", { "first" }, true))).kind == "Applied")
folds.restore(replica, captured)
assert(vim.api.nvim_win_call(first_window, function() return vim.fn.foldclosed(3) end) == 3, "surviving fold lost its closed state")
assert(vim.fn.foldclosed(3) == -1, "surviving fold lost its open state in the second window")
assert(replica.status == "Applied")
captured = folds.capture(replica)
replica.editable.suspended = true
vim.wo.foldexpr = vim.wo.foldexpr
assert(vim.fn.foldlevel(3) == 0, "fixture still has a native fold")
folds.restore(replica, captured)
replica.editable.suspended = false
vim.wo.foldexpr = vim.wo.foldexpr
folds.restore(replica, captured)
assert(vim.api.nvim_win_call(first_window, function() return vim.fn.foldclosed(3) end) == 3, "resumed fold lost its closed state")
assert(vim.fn.foldclosed(3) == -1, "resumed fold lost its open state")
assert(buffer.apply_snapshot(replica, snapshot(2, block("replacement", { "replacement without a fold" }))).kind == "Applied")
folds.restore(replica, captured)
assert(vim.fn.foldlevel(1) == 0, "removed fold survived in Neovim's fold cache")
assert(vim.api.nvim_win_call(first_window, function() return vim.fn.foldclosed(3) end) == 3)
assert(vim.fn.foldclosed(3) == -1)
folds.detach(replica)
local nested = buffer.open("nested-fold-return", {})
local nested_block = block("nested", { "parent", "child", "child body", "child tail", "sibling", "sibling body", "parent tail", "outside" })
nested_block.metadata.fold = {
  { id = "parent", start = { row = 0, column = 0 }, ["end"] = { block = "nested", position = { row = 7, column = 0 } }, closed = true },
  { id = "child", start = { row = 1, column = 0 }, ["end"] = { block = "nested", position = { row = 4, column = 0 } }, closed = false },
  { id = "sibling", start = { row = 4, column = 0 }, ["end"] = { block = "nested", position = { row = 6, column = 0 } }, closed = true },
}
assert(buffer.apply_snapshot(nested, { document = nested.document, revision = 0, block = { nested_block } }).kind == "Applied")
vim.api.nvim_win_set_buf(second_window, nested.buffer)
folds.attach(nested, second_window)
vim.cmd("1foldopen")
assert(vim.fn.foldclosed(2) == -1 and vim.fn.foldclosed(5) == 5)
vim.cmd("1foldclose")
folds.release(second_window)
folds.attach(nested, second_window)
vim.cmd("1foldopen")
assert(vim.fn.foldclosed(2) == -1, "returning to a closed parent closed its previously open child")
assert(vim.fn.foldclosed(5) == 5, "returning to a closed parent opened its previously closed child")
for revision = 1, 3 do
  vim.cmd("1foldclose")
  local view = vim.fn.winsaveview()
  local nested_state = folds.capture(nested)
  assert(nested_state[second_window].parent == true)
  assert(nested_state[second_window].child == false)
  assert(nested_state[second_window].sibling == true)
  assert(vim.fn.foldclosed(1) == 1, "capture opened the parent")
  assert(vim.deep_equal(view, vim.fn.winsaveview()), "capture moved the view")
  assert(buffer.apply_snapshot(nested, { document = nested.document, revision = revision,
    block = { block("prefix", { "inserted " .. revision }), nested_block } }).kind == "Applied")
  folds.restore(nested, nested_state)
  assert(vim.fn.foldclosed(2) == 2 and vim.fn.foldclosedend(2) == 8, "refresh lost the parent range")
  vim.cmd("2foldopen")
  assert(vim.fn.foldclosed(3) == -1, "refresh closed an open child")
  assert(vim.fn.foldclosed(6) == 6, "refresh opened a closed sibling")
  if revision < 3 then
    local shifted_state = folds.capture(nested)
    assert(buffer.apply_snapshot(nested, { document = nested.document, revision = revision,
      block = { nested_block } }).kind == "Applied")
    folds.restore(nested, shifted_state)
  end
end
folds.detach(nested)
print("fold restoration across shrinking ranges passed")
vim.cmd("qa!")
