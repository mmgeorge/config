local previous = {}

local function encode(value)
  if type(value) ~= "table" then return value end
  if value.review then value = vim.tbl_extend("force", value, { review = encode(value.review) }) end
  if not value.snapshot or not value.source_row then return value end
  value = vim.deepcopy(value)
  local snapshot = value.snapshot
  local prior = previous[snapshot.document]
  local source_edit = { upsert = {}, remove = {} }
  local retained = {}
  local block_by_id = {}
  for _, block in ipairs(snapshot.block) do block_by_id[block.id] = block end
  for _, row in ipairs(value.source_row) do
    local id = row.block .. ":" .. row.position.row
    retained[id] = true
    source_edit.upsert[#source_edit.upsert + 1] = { id = id, source_line = row.source_line, test = row.test, check = row.check,
      test_change = row.test_change, test_file = row.test_file, test_summary = row.test_summary }
    if type(row.target) == "string" then
      local block = assert(block_by_id[row.block])
      block.metadata.target = block.metadata.target or {}
      if not vim.iter(block.metadata.target):any(function(target) return target.id == row.target end) then
        block.metadata.target[#block.metadata.target + 1] = { id = row.target,
          range = { start = { row = row.position.row, column = 0 }, ["end"] = { row = row.position.row + 1, column = 0 } } }
      end
    end
  end
  if prior then
    snapshot.revision = math.max(snapshot.revision, prior.revision + 1)
    for _, block in ipairs(prior.block) do
      for position in ipairs(block.text) do
        local id = block.id .. ":" .. (position - 1)
        if not retained[id] then source_edit.remove[#source_edit.remove + 1] = id end
      end
    end
    local old_rows, new_rows, metadata, removed, order, ids = {}, {}, {}, {}, {}, {}
    for _, block in ipairs(snapshot.block) do
      vim.list_extend(new_rows, block.text)
      ids[block.id] = true
      order[#order + 1] = block.id
      metadata[#metadata + 1] = { block = block.id, row_count = #block.text, metadata = block.metadata }
    end
    for _, block in ipairs(prior.block) do
      vim.list_extend(old_rows, block.text)
      if not ids[block.id] then removed[#removed + 1] = block.id end
    end
    value.patch = { document = snapshot.document, base = prior.revision, next = snapshot.revision,
      base_rows = #old_rows, next_rows = #new_rows, base_blocks = #prior.block, next_blocks = #snapshot.block,
      text_edit = { { start_row = 0, removed_rows = #old_rows, text = new_rows } }, metadata_edit = metadata,
      removed_block = removed, block_edit = { { start_block = 0, removed_blocks = #prior.block, inserted = order } } }
    value.snapshot = nil
  end
  previous[snapshot.document] = snapshot
  value.source_row, value.source_edit = nil, source_edit
  return value
end

return function(callback)
  return function(value, failure) callback(encode(value), failure) end
end
