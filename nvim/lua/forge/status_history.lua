local M = {}

---@class ForgeStatusCommitComparison
---@field document string
---@field oid string
---@field snapshot? table
---@field failure? string
---@field prefix string
---@field unsubscribe? fun()
---@field recovering? boolean

---@param document string
---@param oid string
---@return ForgeStatusCommitComparison
function M.new(document, oid)
  return { document = document .. ":commit:" .. oid, oid = oid, prefix = "commit:" .. oid .. ":" }
end

---@param owner ForgeStatusCommitComparison
---@param id integer
---@return string
function M.file_key(owner, id)
  return owner.prefix .. string.format("%.0f", id)
end

---@param session ForgeStatusReplica
---@param document string
---@return ForgeStatusCommitComparison?
function M.owner(session, document)
  for _, owner in pairs(session.commit) do
    if owner.document == document then return owner end
  end
end

local function metadata(owner, value)
  for _, target in ipairs(value.target or {}) do target.id = owner.prefix .. target.id end
  for _, fold in ipairs(value.fold or {}) do
    fold.id = owner.prefix .. fold.id
    fold["end"].block = owner.prefix .. fold["end"].block
  end
end

---@param session ForgeStatusReplica
---@param delivery table
---@return table
function M.delivery(session, delivery)
  if delivery.document == session.document then return delivery end
  local owner = M.owner(session, delivery.document)
  if not owner or not owner.snapshot then return delivery end
  local result = vim.deepcopy(delivery)
  result.document, result.file = session.document, M.file_key(owner, delivery.file)
  local body_document = "body:" .. result.file .. ":" .. string.format("%.0f", delivery.generation)
  if type(result.snapshot) == "table" then
    assert(result.snapshot.document == string.format("body:%.0f:%.0f", delivery.file, delivery.generation), "commit body identity differs")
    result.snapshot.document = body_document
    for _, block in ipairs(result.snapshot.block) do
      block.id = owner.prefix .. block.id
      metadata(owner, block.metadata)
    end
  end
  if type(result.patch) == "table" then
    local patch = result.patch
    assert(patch.document == string.format("body:%.0f:%.0f", delivery.file, delivery.generation), "commit patch identity differs")
    patch.document = body_document
    for _, edit in ipairs(patch.block_edit) do
      for index, id in ipairs(edit.inserted) do edit.inserted[index] = owner.prefix .. id end
    end
    for _, edit in ipairs(patch.metadata_edit) do
      edit.block = owner.prefix .. edit.block
      metadata(owner, edit.metadata)
    end
    for index, id in ipairs(patch.removed_block) do patch.removed_block[index] = owner.prefix .. id end
  end
  return result
end

---@param session ForgeStatusReplica
---@param captured ForgeStatusInput
---@return ForgeStatusInput
function M.input(session, captured)
  local location = captured.location
  local model = session.file[location.kind == "file" and location.id or location.file]
  if not model or not model.owner then return captured end
  local owner = model.owner
  local result = vim.deepcopy(captured)
  result.document, result.revision = owner.document, owner.snapshot.revision
  location = result.location
  if location.kind == "file" then location.id = model.record.id else
    location.file = model.record.id
    location.block = location.block:sub(#owner.prefix + 1)
    if location.target then location.target = location.target:sub(#owner.prefix + 1) end
  end
  return result
end

---@param session ForgeStatusReplica
---@param effect table
---@return table
function M.effect(session, effect)
  if effect.document == session.document then return effect end
  local owner = M.owner(session, effect.document)
  if not owner or not owner.snapshot or effect.revision ~= owner.snapshot.revision then return effect end
  ---@type table
  local result = vim.deepcopy(effect)
  result.document, result.revision = session.document, session.revision
  if result.block then result.block = owner.prefix .. result.block end
  local location = result.location
  if location and location.kind == "file" then
    result.location = vim.tbl_extend("force", location, { id = M.file_key(owner, location.id) })
  elseif location and location.kind == "body" then
    result.location = vim.tbl_extend("force", location, { file = M.file_key(owner, location.file),
      block = owner.prefix .. location.block, target = location.target and owner.prefix .. location.target or nil })
  end
  return result
end

return M
