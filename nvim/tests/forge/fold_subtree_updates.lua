vim.opt.runtimepath:prepend("nvim")
local buffer = require("forge.buffer")
local folds = require("forge.folds")

local function fold(id, endpoint, first, finish, closed)
  return { id = id, start = { row = first, column = 0 },
    ["end"] = { block = endpoint, position = { row = finish, column = 0 } }, closed = closed }
end

local function metadata(definitions)
  return { target = {}, decoration = {}, editable_region = {}, fold = definitions }
end

local function fixture(identity, definitions)
  local owner = buffer.open(identity)
  vim.api.nvim_set_current_buf(owner.buffer)
  assert(buffer.apply_snapshot(owner, { document = identity, revision = 0, block = {
    { id = "body", text = { "heading", "inner", "outer", "tail" }, metadata = metadata(definitions) },
    { id = "sibling", text = { "sibling", "body" },
      metadata = metadata({ fold("sibling", "sibling", 0, 2, true) }) },
  } }).kind == "Applied")
  folds.attach(owner, vim.api.nvim_get_current_win())
  return owner
end

local function update(owner, definitions)
  local result = buffer.apply_patch(owner, {
    document = owner.document, base = owner.revision, next = owner.revision + 1,
    base_rows = 6, next_rows = 6, base_blocks = 2, next_blocks = 2,
    block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = "body", row_count = 4, metadata = metadata(definitions) } },
  })
  assert(result.kind == "Applied", vim.inspect(result))
end

local function check(owner, window, expected)
  vim.api.nvim_win_call(window, function()
    for row, depth in ipairs(expected) do
      assert(vim.fn.foldlevel(row) == depth,
        ("%s revision %d row %d: expected %d native levels, got %d"):format(
          owner.document, owner.revision, row, depth, vim.fn.foldlevel(row)))
    end
    assert(vim.fn.foldclosed(5) == 5 and vim.fn.foldclosedend(5) == 6,
      "updating a subtree changed the unrelated sibling fold")
  end)
end

local owner = fixture("coincident-fold-start", {
  fold("parent", "body", 0, 4, false), fold("child", "body", 0, 2, false),
})
local first_window = vim.api.nvim_get_current_win()
vim.cmd("vsplit")
local second_window = vim.api.nvim_get_current_win()
folds.attach(owner, second_window)
for revision = 1, 12 do
  update(owner, { fold("parent", "body", 0, 4, revision % 2 == 0),
    fold("child", "body", 0, 2, revision % 3 == 0) })
  check(owner, first_window, { 2, 2, 1, 1, 1, 1 })
  check(owner, second_window, { 2, 2, 1, 1, 1, 1 })
end
update(owner, { fold("parent", "body", 0, 4, false) })
check(owner, first_window, { 1, 1, 1, 1, 1, 1 })
check(owner, second_window, { 1, 1, 1, 1, 1, 1 })
buffer.close(owner)

local equal = fixture("coincident-fold-range", {
  fold("parent", "body", 0, 4, false), fold("child", "body", 0, 4, false),
})
local equal_window = vim.api.nvim_get_current_win()
for revision = 1, 6 do
  update(equal, { fold("parent", "body", 0, 4, revision % 2 == 0),
    fold("child", "body", 0, 4, false) })
  check(equal, equal_window, { 2, 2, 2, 2, 1, 1 })
end
buffer.close(equal)

local small = fixture("single-line-fold", { fold("small", "body", 0, 1, false) })
local small_window = vim.api.nvim_get_current_win()
for revision = 1, 6 do
  update(small, { fold("small", "body", 0, 1, revision % 2 == 0) })
  check(small, small_window, { 1, 0, 0, 0, 1, 1 })
  assert(vim.wo.foldminlines == 1, "fold deletion changed the window's minimum fold size")
end
buffer.close(small)

local nested = buffer.open("retained-parent")
vim.api.nvim_set_current_buf(nested.buffer)
assert(buffer.apply_snapshot(nested, { document = nested.document, revision = 0, block = {
  { id = "header", text = { "parent" }, metadata = metadata({ fold("parent", "body", 0, 2, true) }) },
  { id = "body", text = { "child", "body" }, metadata = metadata({ fold("child", "body", 0, 2, false) }) },
} }).kind == "Applied")
folds.attach(nested, vim.api.nvim_get_current_win())
for revision = 1, 6 do
  assert(buffer.apply_patch(nested, {
    document = nested.document, base = nested.revision, next = nested.revision + 1,
    base_rows = 3, next_rows = 3, base_blocks = 2, next_blocks = 2,
    block_edit = {}, removed_block = {}, text_edit = {},
    metadata_edit = { { block = "body", row_count = 2,
      metadata = metadata({ fold("child", "body", 0, 2, revision % 2 == 0) }) } },
  }).kind == "Applied")
  assert(vim.fn.foldlevel(1) == 1 and vim.fn.foldlevel(2) == 2,
    ("child update duplicated or removed its parent: revision %d, levels %d/%d"):format(revision, vim.fn.foldlevel(1), vim.fn.foldlevel(2)))
  assert(vim.fn.foldclosed(1) == 1, "child update opened the retained parent")
end
buffer.close(nested)
print("fold subtree updates: coincident starts, equal ranges, single-line folds, splits, and retained folds passed")
vim.cmd("qa!")
