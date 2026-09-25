local M = {}

---@class ForgeContentLayout
---@field indent integer
---@field source_indent? integer
---@field marker? {text: string, capture: string}

---@param layout ForgeContentLayout
---@param first boolean
---@return table[]
function M.prefix(layout, first)
  local width = math.max(0, layout.indent - 2 - (layout.source_indent or 0))
  local marker = first and layout.marker or nil
  if marker == vim.NIL then marker = nil end
  if marker and width > 0 then
    local marker_width = vim.fn.strdisplaywidth(marker.text)
    local gap = math.max(0, 2 - marker_width)
    return { { string.rep(" ", math.max(0, width - marker_width - gap)), "Normal" },
      { marker.text .. string.rep(" ", gap), marker.capture } }
  end
  return width > 0 and { { string.rep(" ", width), "Normal" } } or {}
end

---@param buffer integer
---@param namespace integer
---@param start_row integer
---@param count integer
---@param layout ForgeContentLayout
---@return integer[]
function M.install(buffer, namespace, start_row, count, layout)
  local marks = {}
  for row = 0, count - 1 do
    local options = { priority = 200, right_gravity = true, strict = true }
    local marker = row == 0 and layout.marker or nil
    if marker == vim.NIL then marker = nil end
    if marker and layout.indent == 2 then
      options.sign_text, options.sign_hl_group = marker.text, marker.capture
    end
    local prefix = M.prefix(layout, false)
    if #prefix > 0 then
      options.virt_text, options.virt_text_pos, options.hl_mode = prefix, "inline", "combine"
      options.virt_text_repeat_linebreak = true
    end
    if options.sign_text or options.virt_text then
      marks[#marks + 1] = vim.api.nvim_buf_set_extmark(buffer, namespace, start_row + row, 0, options)
    end
    if marker and layout.indent > 2 then
      marks[#marks + 1] = vim.api.nvim_buf_set_extmark(buffer, namespace, start_row + row, 0, {
        virt_text = { { marker.text, marker.capture } }, virt_text_win_col = layout.indent - 4,
        priority = 201, right_gravity = true, strict = true,
      })
    end
  end
  return marks
end

return M
