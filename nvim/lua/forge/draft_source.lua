local M = {}

---@class ForgeDraftSourceIdentity: ForgeDraftSourceRow
---@field block string
---@field position {row: integer, column: integer}
---@field canonical_row? integer
---@field target? string
---@field metadata? table

---@param replica table
---@param comments ForgeDraftCommentState
---@param source ForgeDraftSourceIdentity[]
function M.attach(replica, comments, source)
  local tick, generation
  local by_row, by_canonical, position = {}, {}, {}
  local last_canonical = 0
  for index, original in ipairs(source) do
    last_canonical = math.max(last_canonical, original.canonical_row or index - 1)
  end
  replica.prepare_source = function()
    local current_tick = vim.api.nvim_buf_get_changedtick(comments.buf)
    if tick == current_tick and generation == comments.generation then return end
    local source_index = {}
    for _, mark in ipairs(comments.source_mark) do source_index[mark.mark] = mark.source_index end
    by_row, by_canonical, position = {}, {}, {}
    for _, mark in ipairs(vim.api.nvim_buf_get_extmarks(comments.buf, comments.namespace, 0, -1, {})) do
      local index = source_index[mark[1]]
      if index then
        local original = source[index]
        local canonical = original.canonical_row or index - 1
        by_row[mark[2]], by_canonical[canonical] = original, mark[2]
        position[#position + 1] = { row = mark[2], canonical = canonical, source = original }
      end
    end
    tick, generation = current_tick, comments.generation
  end
  local function preceding(row)
    replica.prepare_source()
    local low, high, selected = 1, #position, nil
    while low <= high do
      local middle = math.floor((low + high) / 2)
      if position[middle].row <= row then selected, low = position[middle], middle + 1
      else high = middle - 1 end
    end
    return selected
  end
  replica.decoration_location = function(row)
    replica.prepare_source()
    return by_row[row]
  end
  replica.locate = function(row, column)
    local selected = preceding(row)
    if not selected then return nil end
    local original = selected.source
    return {
      block = original.block,
      position = { row = original.position.row, column = math.min(column, #original.text) },
      target = original.target ~= vim.NIL and original.target or nil,
    }
  end
  replica.physical_row = function(row)
    replica.prepare_source()
    if row > last_canonical then return vim.api.nvim_buf_line_count(comments.buf) end
    if by_canonical[row] then return by_canonical[row] end
    local low, high, following = 1, #position, nil
    while low <= high do
      local middle = math.floor((low + high) / 2)
      if position[middle].canonical > row then following, high = position[middle].row, middle - 1
      else low = middle + 1 end
    end
    return following or vim.api.nvim_buf_line_count(comments.buf)
  end
end

return M
