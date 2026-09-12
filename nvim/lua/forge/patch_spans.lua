local M = {}

function M.new(count)
  return count == 0 and {} or { { start = 0, count = count } }
end

local function part(piece, start, count)
  if piece.start then return { start = piece.start + start, count = count } end
  local value = {}
  for index = start + 1, start + count do value[#value + 1] = piece.value[index] end
  return { value = value, count = count }
end

function M.splice(pieces, start, removed, value, replacement)
  replacement = replacement or (#value > 0 and { {value=value,count=#value} } or {})
  local result, position, inserted = {}, 0, false
  local finish = start + removed
  for _, piece in ipairs(pieces) do
    local after = position + piece.count
    if position < start and after > start then result[#result + 1] = part(piece, 0, start - position)
    elseif after <= start then result[#result + 1] = piece end
    if not inserted and after >= start then
      for _, piece in ipairs(replacement) do result[#result + 1] = piece end
      inserted = true
    end
    if position >= finish and position >= start then result[#result + 1] = piece
    elseif after > finish and after > start then result[#result + 1] = part(piece, finish - position, after - finish) end
    position = after
  end
  assert(start <= position and finish <= position, "composed patch span exceeds source")
  if not inserted then for _, piece in ipairs(replacement) do result[#result + 1] = piece end end
  return result
end

function M.at(pieces, row, read)
  local position = 0
  for _, piece in ipairs(pieces) do
    if row < position + piece.count then
      return piece.start and read(piece.start + row - position) or piece.value[row - position + 1]
    end
    position = position + piece.count
  end
  error("composed row is unavailable")
end

function M.edits(pieces, total)
  local edit, cursor, value = {}, 0, {}
  local function flush(finish)
    if finish > cursor or #value > 0 then edit[#edit + 1] = {start=cursor,removed=finish-cursor,value=value} end
    value = {}
  end
  for _, piece in ipairs(pieces) do
    if piece.start then
      assert(piece.start >= cursor, "composed source spans are reordered")
      flush(piece.start)
      cursor = piece.start + piece.count
    else
      for _, item in ipairs(piece.value) do value[#value + 1] = item end
    end
  end
  flush(total)
  local descending = {}
  for index = #edit, 1, -1 do descending[#descending+1] = edit[index] end
  return descending
end

return M
