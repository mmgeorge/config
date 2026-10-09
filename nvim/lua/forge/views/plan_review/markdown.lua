local M = {}

--- Locates section prose after review annotations have shifted the source rows.
---@param source table[] Native source rows carrying Markdown metadata.
---@param projection table Current review projection with physical source records.
---@return table[] ranges Half-open Markdown regions excluding headings and annotations.
function M.ranges(source, projection)
  local ranges = {}
  for _, record in ipairs(projection.source_record_list) do
    local row = source[record.source_index]
    if row.metadata and row.metadata.markdown then
      local previous = ranges[#ranges]
      if previous and previous.after0 == record.row then
        previous.after0 = record.row + 1
      else
        ranges[#ranges + 1] = { first0 = record.row, after0 = record.row + 1 }
      end
    end
  end
  return ranges
end

return M
