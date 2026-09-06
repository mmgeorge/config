local module = {}

---@class MarkdownMathBlock
---@field start_row integer Zero-based opening row.
---@field end_row integer Exclusive row after the closing delimiter.
---@field indent string Leading whitespace of the opening delimiter.
---@field input string TeX content joined with spaces.

---@param line string
---@param fence string?
---@return string? fence
---@return boolean literal
local function fence_state(line, fence)
  local marker = line:match("^%s*(```+)") or line:match("^%s*(~~~+)")
  if fence then
    if marker and marker:sub(1, 1) == fence:sub(1, 1) and #marker >= #fence
      and line:match("^%s*" .. marker .. "%s*$") then
      return nil, true
    end
    return fence, true
  end
  if marker then return marker, true end
  return nil, false
end

---Find complete display blocks outside fenced code without changing source lines.
---@param line_list string[]
---@return MarkdownMathBlock[]
function module.find(line_list)
  local block_list = {}
  local fence
  local opening
  local closing
  local input_list = {}
  for line_index, line in ipairs(line_list) do
    local literal
    fence, literal = fence_state(line, fence)
    if not literal then
      local trimmed = vim.trim(line)
      if opening then
        if trimmed == closing then
          block_list[#block_list + 1] = {
            start_row = opening - 1,
            end_row = line_index,
            indent = line_list[opening]:match("^%s*") or "",
            input = table.concat(input_list, " "),
          }
          opening = nil
          input_list = {}
        elseif trimmed ~= "" then
          input_list[#input_list + 1] = trimmed
        end
      elseif trimmed == "$$" or trimmed == "\\[" then
        opening = line_index
        closing = trimmed == "$$" and "$$" or "\\]"
      end
    elseif opening then
      opening = nil
      input_list = {}
    end
  end
  return block_list
end

---Normalize complete display blocks and inline delimiters, preserving fenced code and incomplete blocks.
---@param text string
---@return string
function module.normalize(text)
  local line_list = vim.split(text, "\n", { plain = true })
  local block_list = module.find(line_list)
  for block_index = #block_list, 1, -1 do
    local block = block_list[block_index]
    line_list[block.start_row + 1] = block.indent .. "$$ " .. block.input .. " $$"
    for line_index = block.end_row, block.start_row + 2, -1 do
      table.remove(line_list, line_index)
    end
  end
  local fence
  for line_index, line in ipairs(line_list) do
    local literal
    fence, literal = fence_state(line, fence)
    if not literal then
      line_list[line_index] = line:gsub("\\%((.-)\\%)", function(input) return "$" .. input .. "$" end)
    end
  end
  return table.concat(line_list, "\n")
end

return module
