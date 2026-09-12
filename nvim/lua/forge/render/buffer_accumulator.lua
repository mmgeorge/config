--- Accumulates rendered diff-tree rows for native presentation owners.
---@class ForgeBufferAccumulatorModule
local M = {}

local function add_line(state, text, entry)
  state.lines[#state.lines + 1] = text
  local line = #state.lines
  if entry then state.entries[line] = entry end
  return line
end

function M.highlighted_text_segments(text, highlights)
  text = tostring(text or "")
  local ordered = vim.deepcopy(highlights or {})
  table.sort(ordered, function(left, right) return (left.start_col or 0) < (right.start_col or 0) end)
  local segments, cursor = {}, 0
  for _, highlight in ipairs(ordered) do
    local first = math.max(cursor, math.min(#text, tonumber(highlight.start_col) or 0))
    local last = math.max(first, math.min(#text, tonumber(highlight.end_col) or first))
    if first > cursor then segments[#segments + 1] = { text:sub(cursor + 1, first) } end
    if last > first then segments[#segments + 1] = { text:sub(first + 1, last), highlight.hl_group } end
    cursor = last
  end
  if cursor < #text then segments[#segments + 1] = { text:sub(cursor + 1) } end
  if #segments == 0 then segments[1] = { text } end
  return segments
end

function M.add_segment_line(state, segments, entry)
  local parts, column = {}, 0
  local line = #state.lines + 1
  for _, segment in ipairs(segments) do
    local text = segment[1] or ""
    parts[#parts + 1] = text
    if segment[2] and text ~= "" then
      state.highlights[#state.highlights + 1] = { line = line, start_col = column, end_col = column + #text, hl_group = segment[2] }
    end
    column = column + #text
  end
  return add_line(state, table.concat(parts), entry)
end

function M.add_fancy_row(state, row, entry, indent)
  indent = indent or 0
  local parts = indent > 0 and { string.rep(" ", indent) } or {}
  local column = indent
  local diff_line, diff_lines
  for _, chunk in ipairs(row) do
    if chunk.meta and chunk.meta.diff then diff_line = chunk.meta.diff end
    if chunk.meta and chunk.meta.diff_lines then diff_lines = chunk.meta.diff_lines end
    if type(chunk[1]) == "string" then
      local text = chunk[1]
      parts[#parts + 1] = text
      if chunk[2] and text ~= "" then
        state.highlights[#state.highlights + 1] = { line = #state.lines + 1, start_col = column, end_col = column + #text, hl_group = chunk[2] }
      end
      column = column + #text
    elseif chunk.virt_text then
      local opts = vim.deepcopy(chunk)
      opts.col = nil
      state.extmarks[#state.extmarks + 1] = { line = #state.lines + 1, col = (chunk.col or 0) + indent, opts = opts }
    end
  end
  if diff_line and entry then
    entry = vim.tbl_extend("force", entry, { diff_line = diff_line, diff_lines = diff_lines })
  end
  return add_line(state, table.concat(parts), entry)
end

return M
