local M = {}

--- Keep interrupted source runs together and give each annotation body its own Markdown region.
---@param source table[] Native source rows carrying Markdown metadata.
---@param projection table Current review projection with physical source records.
---@return ForgeMarkdownRange[] ranges Half-open Markdown ranges for source and annotation bodies.
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
  for _, region in ipairs(projection.markdown_regions or {}) do
    local first, after = region[1][1], region[#region][3]
    ranges[#ranges + 1] = { first0 = first, after0 = after, annotation = true,
      frame = projection.line_meta_list[first + 1].comment_frame }
  end
  return ranges
end

return M
