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

---@param text string
---@param end_offset integer
---@return integer
function M.line_end_without_cr(text, end_offset)
  if end_offset >= 1 and text:sub(end_offset, end_offset) == "\r" then return end_offset - 1 end
  return end_offset
end

---Build a byte-indexed text snapshot from a file body so diff rows can store offsets instead of copied lines.
---@param text string?
---@return DiffReviewTextSnapshot
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

---@param snapshot DiffReviewTextSnapshot
---@param line_number integer
---@return string?
function M.line_text(snapshot, line_number)
  local span = snapshot and snapshot.line_span and snapshot.line_span[line_number] or nil
  if not span then return nil end
  if span.end_offset < span.start_offset then return "" end
  return snapshot.text:sub(span.start_offset, span.end_offset)
end

---@param snapshot DiffReviewTextSnapshot
---@param first_line integer
---@param last_line integer
---@return string[]
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

---@param snapshot DiffReviewTextSnapshot
---@param line_number integer
---@return DiffReviewTextLineSpan?
function M.line_span(snapshot, line_number)
  if not (snapshot and snapshot.line_span) then return nil end
  return snapshot.line_span[line_number]
end

return M
