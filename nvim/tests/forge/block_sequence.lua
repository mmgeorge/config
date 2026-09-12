vim.loader.enable(false)
local Sequence = require("forge.block_sequence")

local function entry(id, count)
  return { id = id, entry = { row_count = count, metadata = {} } }
end

local sequence = Sequence.new()
local reference = {}
local function verify()
  assert(sequence:count() == #reference)
  local start = 0
  for index, item in ipairs(reference) do
    assert(sequence:at(index - 1).id == item.id)
    local actual_index, actual_start = sequence:position(item.id)
    assert(actual_index == index - 1 and actual_start == start)
    for row = start, start + item.entry.row_count - 1 do
      assert(sequence:locate(row).id == item.id)
    end
    start = start + item.entry.row_count
  end
  assert(sequence:rows() == start)
  assert(sequence:locate(start) == nil)
  local function check(node, parent)
    if not node then return 0 end
    assert(node.parent == parent)
    local left, right = check(node.left, node), check(node.right, node)
    assert(math.abs(left - right) <= 1, "unbalanced sequence")
    assert(node.height == math.max(left, right) + 1)
    return node.height
  end
  check(sequence.root, nil)
end

local ok, failure = xpcall(function()
  math.randomseed(4701)
  for iteration = 1, 500 do
    local position = math.random(0, #reference)
    local removed = math.random(0, math.min(4, #reference - position))
    local inserted = {}
    for offset = 1, math.random(0, 4) do
      inserted[offset] = entry(iteration .. ":" .. offset, math.random(0, 4))
    end
    sequence:begin()
    sequence:splice(position, removed, inserted)
    sequence:rollback()
    verify()
    sequence:splice(position, removed, inserted)
    for _ = 1, removed do table.remove(reference, position + 1) end
    for offset, item in ipairs(inserted) do table.insert(reference, position + offset, item) end
    verify()
  end
  sequence = Sequence.new()
  local batch = {}
  for index = 1, 10000 do batch[index] = entry(tostring(index), 1) end
  sequence:splice(0, 0, batch)
  sequence.visits = 0
  sequence:begin()
  sequence:update("5000", { row_count = 2, metadata = {} })
  local _, start = sequence:position("9999")
  assert(start == 9999)
  assert(sequence.visits < 64, "small update traversed unrelated blocks")
  sequence:rollback()
  assert(sequence:rows() == 10000)
end, debug.traceback)

if not ok then
  vim.api.nvim_err_writeln(failure)
  vim.cmd("cquit 1")
else
  vim.cmd("qa!")
end
