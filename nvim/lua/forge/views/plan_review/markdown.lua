local M = {}

--- Map Markdown source runs to physical ranges that skip inserted annotations.
---@param source table[] Native source rows carrying Markdown metadata.
---@param projection table Current review projection with physical source records.
---@return ForgeMarkdownRange[] ranges Half-open Markdown ranges excluding headings and annotations.
function M.ranges(source, projection)
  local ranges, previous_source_index = {}, nil
  for _, record in ipairs(projection.source_record_list) do
    local row = source[record.source_index]
    if row.metadata and row.metadata.markdown then
      local previous = ranges[#ranges]
      local continues = previous_source_index == record.source_index - 1
      if previous and continues and previous.after0 == record.row then
        previous.after0 = record.row + 1
      else
        ranges[#ranges + 1] = { first0 = record.row, after0 = record.row + 1, continues_region = continues }
      end
      previous_source_index = record.source_index
    end
  end
  return ranges
end

return M
