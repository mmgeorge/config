---@class ForgeSequenceEntry
---@field row_count integer
---@field metadata table

---@class ForgeSequenceNode
---@field id string
---@field entry ForgeSequenceEntry
---@field left? ForgeSequenceNode
---@field right? ForgeSequenceNode
---@field parent? ForgeSequenceNode
---@field count integer
---@field rows integer
---@field height integer
---@field fold_sum integer
---@field fold_delta? integer
---@field boundary? table<string, {row: integer, delta: integer}>

---@class ForgeBlockSequence
---@field root? ForgeSequenceNode
---@field node table<string, ForgeSequenceNode>
---@field undo? {owner: table, key: string, value: any}[]
---@field visits integer
local Sequence = {}
Sequence.__index = Sequence

local function count(node) return node and node.count or 0 end
local function height(node) return node and node.height or 0 end
local function rows(node) return node and node.rows or 0 end
local function folds(node) return node and node.fold_sum or 0 end

local function assign(sequence, owner, key, value)
  if owner[key] == value then return end
  if sequence.undo then
    sequence.undo[#sequence.undo + 1] = { owner = owner, key = key, value = owner[key] }
  end
  owner[key] = value
end

local function refresh(sequence, node)
  sequence.visits = sequence.visits + 1
  assign(sequence, node, "count", count(node.left) + count(node.right) + 1)
  assign(sequence, node, "rows", rows(node.left) + rows(node.right) + node.entry.row_count)
  assign(sequence, node, "height", math.max(height(node.left), height(node.right)) + 1)
  assign(sequence, node, "fold_sum", folds(node.left) + folds(node.right) + (node.fold_delta or 0))
end

local function child(sequence, parent, side, node)
  assign(sequence, parent, side, node)
  if node then assign(sequence, node, "parent", parent) end
end

local function rotate(sequence, node, side, opposite)
  local replacement, parent = node[side], node.parent
  child(sequence, node, side, replacement[opposite])
  child(sequence, replacement, opposite, node)
  assign(sequence, replacement, "parent", parent)
  refresh(sequence, node)
  refresh(sequence, replacement)
  return replacement
end

local function balance(sequence, node)
  refresh(sequence, node)
  local difference = height(node.left) - height(node.right)
  if difference > 1 then
    if height(node.left.left) < height(node.left.right) then
      child(sequence, node, "left", rotate(sequence, node.left, "right", "left"))
    end
    return rotate(sequence, node, "left", "right")
  elseif difference < -1 then
    if height(node.right.right) < height(node.right.left) then
      child(sequence, node, "right", rotate(sequence, node.right, "left", "right"))
    end
    return rotate(sequence, node, "right", "left")
  end
  return node
end

local function insert(sequence, node, position, inserted)
  if not node then return inserted end
  local left_count = count(node.left)
  if position <= left_count then
    child(sequence, node, "left", insert(sequence, node.left, position, inserted))
  else
    child(sequence, node, "right", insert(sequence, node.right, position - left_count - 1, inserted))
  end
  return balance(sequence, node)
end

local function remove(sequence, node, position)
  local left_count = count(node.left)
  if position < left_count then
    child(sequence, node, "left", remove(sequence, node.left, position))
  elseif position > left_count then
    child(sequence, node, "right", remove(sequence, node.right, position - left_count - 1))
  else
    assign(sequence, sequence.node, node.id, nil)
    if not node.left or not node.right then
      local replacement = node.left or node.right
      if replacement then assign(sequence, replacement, "parent", node.parent) end
      return replacement
    end
    local successor = node.right
    while successor.left do successor = successor.left end
    local successor_id, successor_entry = successor.id, successor.entry
    local successor_boundary, successor_delta = successor.boundary, successor.fold_delta
    child(sequence, node, "right", remove(sequence, node.right, 0))
    assign(sequence, node, "id", successor_id)
    assign(sequence, node, "entry", successor_entry)
    assign(sequence, node, "boundary", successor_boundary)
    assign(sequence, node, "fold_delta", successor_delta)
    assign(sequence, sequence.node, successor_id, node)
  end
  return balance(sequence, node)
end

---@return ForgeBlockSequence
function Sequence.new()
  return setmetatable({ node = {}, visits = 0 }, Sequence)
end

---@param entries {id: string, entry: ForgeSequenceEntry, boundary?: table}[]
---@param checkpoint? fun() Cooperative preparation boundary called after each 64 nodes.
---@return ForgeBlockSequence
function Sequence.from(entries, checkpoint)
  local sequence = Sequence.new()
  local visited = 0
  local function build(first, last, parent)
    if first > last then return nil end
    local middle = math.floor((first + last) / 2)
    local value = entries[middle]
    visited = visited + 1
    if checkpoint and visited % 64 == 0 then checkpoint() end
    assert(not sequence.node[value.id], "duplicate block identity")
    local node = { id = value.id, entry = value.entry, parent = parent, boundary = value.boundary, fold_delta = 0 }
    for _, boundary in pairs(value.boundary or {}) do node.fold_delta = node.fold_delta + boundary.delta end
    sequence.node[value.id] = node
    node.left = build(first, middle - 1, node)
    node.right = build(middle + 1, last, node)
    refresh(sequence, node)
    return node
  end
  sequence.root = build(1, #entries)
  return sequence
end

---@return integer
function Sequence:count() return count(self.root) end

---@return integer
function Sequence:rows() return rows(self.root) end

---@param position integer Zero-based block position.
---@return ForgeSequenceNode
function Sequence:at(position)
  assert(position >= 0 and position < self:count(), "block position outside sequence")
  local node = self.root
  while node do
    self.visits = self.visits + 1
    local left_count = count(node.left)
    if position == left_count then return node end
    if position < left_count then
      node = node.left
    else
      position, node = position - left_count - 1, node.right
    end
  end
  error("block sequence is inconsistent")
end

---@param id string
---@return integer index
---@return integer start_row
function Sequence:position(id)
  local node = assert(self.node[id], "unknown block identity")
  local index, start_row = count(node.left), rows(node.left)
  local parent = node.parent
  while parent do
    self.visits = self.visits + 1
    if parent.right == node then
      index = index + count(parent.left) + 1
      start_row = start_row + rows(parent.left) + parent.entry.row_count
    end
    node = parent
    parent = node.parent
  end
  return index, start_row
end

---@param row integer Zero-based physical row.
---@return ForgeSequenceNode?
function Sequence:locate(row)
  if row < 0 or row >= self:rows() then return nil end
  local node = self.root
  while node do
    self.visits = self.visits + 1
    local preceding = rows(node.left)
    if row < preceding then
      node = node.left
    elseif row < preceding + node.entry.row_count then
      return node
    else
      row, node = row - preceding - node.entry.row_count, node.right
    end
  end
end

---@param position integer
---@param removed integer
---@param inserted {id: string, entry: ForgeSequenceEntry}[]
function Sequence:splice(position, removed, inserted)
  assert(position >= 0 and removed >= 0 and position + removed <= self:count(), "invalid block splice")
  for _ = 1, removed do
    assign(self, self, "root", remove(self, self.root, position))
    if self.root then assign(self, self.root, "parent", nil) end
  end
  for offset, entry in ipairs(inserted) do
    assert(not self.node[entry.id], "duplicate block identity")
    local node = { id = entry.id, entry = entry.entry, count = 1, rows = entry.entry.row_count, height = 1, fold_sum = 0 }
    assign(self, self.node, entry.id, node)
    assign(self, self, "root", insert(self, self.root, position + offset - 1, node))
    assign(self, self.root, "parent", nil)
  end
end

---@param id string
---@param entry ForgeSequenceEntry
function Sequence:update(id, entry)
  local node = assert(self.node[id], "unknown block identity")
  assign(self, node, "entry", entry)
  while node do
    refresh(self, node)
    node = node.parent
  end
end

---@param id string
---@param key string
---@param row integer?
---@param delta integer?
function Sequence:fold_boundary(id, key, row, delta)
  local node = self.node[id]
  if not node then return end
  if not node.boundary then assign(self, node, "boundary", {}) end
  local previous = node.boundary[key]
  assign(self, node, "fold_delta", (node.fold_delta or 0) - (previous and previous.delta or 0) + (delta or 0))
  assign(self, node.boundary, key, row and { row = row, delta = delta } or nil)
  while node do
    refresh(self, node)
    node = node.parent
  end
end

---@param row integer
---@return integer level
---@return boolean starts
function Sequence:fold_level(row)
  local node, level = self.root, 0
  while node do
    local preceding = rows(node.left)
    if row < preceding then
      node = node.left
    elseif row >= preceding + node.entry.row_count then
      level = level + folds(node.left) + (node.fold_delta or 0)
      row, node = row - preceding - node.entry.row_count, node.right
    else
      level = level + folds(node.left)
      local starts = false
      for _, boundary in pairs(node.boundary or {}) do
        if boundary.row <= row - preceding then level = level + boundary.delta end
        if boundary.row == row - preceding and boundary.delta > 0 then starts = true end
      end
      return level, starts
    end
  end
  return 0, false
end

function Sequence:begin()
  assert(not self.undo, "block transaction already active")
  self.undo = {}
end

function Sequence:rollback()
  local undo = assert(self.undo, "no block transaction")
  self.undo = nil
  for index = #undo, 1, -1 do
    local change = undo[index]
    change.owner[change.key] = change.value
  end
end

return Sequence
