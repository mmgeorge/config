---@class DiffReviewTextLineSpan
---@field start_offset integer
---@field end_offset integer
---@field newline_offset integer?

---@class DiffReviewTextSnapshot
---@field text string
---@field line_span DiffReviewTextLineSpan[]
---@field line_count integer
---@field byte_count integer

---@class DiffReviewTextSnapshotModule
local M = {}

--- Strips the trailing carriage return byte offset from a line end if present.
---@param text string Source string.
---@param end_offset integer Byte offset pointing to the end of a line.
---@return integer offset Adjusted end offset excluding `\r`.
function M.line_end_without_cr(text, end_offset)
  if end_offset >= 1 and text:sub(end_offset, end_offset) == "\r" then return end_offset - 1 end
  return end_offset
end

--- Creates an immutable, byte-indexed text snapshot from a file content string.
--- Computes line span byte intervals to allow zero-copy slice and lookup operations.
---@param text string? Raw file content string.
---@return DiffReviewTextSnapshot snapshot Indexed text snapshot structure.
function M.from_text(text)
  text = text or ""
  local span = {}
  local byte_count = #text
  local start_offset = 1

  while start_offset <= byte_count do
    local newline_offset = text:find("\n", start_offset, true)
    if newline_offset then
      span[#span + 1] = {
        start_offset = start_offset,
        end_offset = M.line_end_without_cr(text, newline_offset - 1),
        newline_offset = newline_offset,
      }
      start_offset = newline_offset + 1
    else
      span[#span + 1] = {
        start_offset = start_offset,
        end_offset = byte_count,
      }
      break
    end
  end

  return {
    text = text,
    line_span = span,
    line_count = #span,
    byte_count = byte_count,
  }
end

--- Extracts line text from a snapshot by one-based line number without allocations when empty.
---@param snapshot DiffReviewTextSnapshot Target text snapshot.
---@param line_number integer One-based line index.
---@return string? text Line content string, or nil if out of bounds.
function M.line_text(snapshot, line_number)
  local span = snapshot and snapshot.line_span and snapshot.line_span[line_number] or nil
  if not span then return nil end
  if span.end_offset < span.start_offset then return "" end
  return snapshot.text:sub(span.start_offset, span.end_offset)
end

--- Extracts an array of line strings for a range of line numbers.
--- Clamps line bounds to the snapshot limits.
---@param snapshot DiffReviewTextSnapshot Target text snapshot.
---@param first_line integer One-based starting line number.
---@param last_line integer One-based ending line number.
---@return string[] lines Array of line text strings.
function M.line_slice(snapshot, first_line, last_line)
  local line = {}
  if not snapshot then return line end
  first_line = math.max(1, tonumber(first_line) or 1)
  last_line = math.min(snapshot.line_count, tonumber(last_line) or snapshot.line_count)
  for line_number = first_line, last_line do
    line[#line + 1] = M.line_text(snapshot, line_number) or ""
  end
  return line
end

--- Retrieves byte offset span boundaries for a given one-based line number.
---@param snapshot DiffReviewTextSnapshot Target text snapshot.
---@param line_number integer One-based line index.
---@return DiffReviewTextLineSpan? span Span record containing byte offsets, or nil.
function M.line_span(snapshot, line_number)
  if not (snapshot and snapshot.line_span) then return nil end
  return snapshot.line_span[line_number]
end

return M
