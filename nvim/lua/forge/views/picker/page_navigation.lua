--- Measures and composes category navigation without owning picker interaction or settings.
local PageNavigation = {}

--- Geometry shared by page wrapping, navigation composition, and editor overlays.
---@class ForgePickerNavigationLayout
---@field mode "none"|"tabs" Available navigation presentation.
---@field width integer Total popup width in display cells.
---@field content_column integer Zero-based display column of page content.
---@field content_width integer Width allocated to page content.
---@field header_height integer Rows reserved above content for tabs.
---@field minimum_height integer Minimum height needed for visible category context.

--- Clip a label at character boundaries and reserve one cell for the omission mark.
local function clipped_label(label, width)
  if vim.fn.strdisplaywidth(label) <= width then return label end
  local count = vim.fn.strchars(label)
  repeat count = count - 1 until count == 0 or vim.fn.strdisplaywidth(vim.fn.strcharpart(label, 0, count)) <= width - 1
  return vim.fn.strcharpart(label, 0, count) .. "…"
end

local function page_label(page, index)
  return page.title or page.id or tostring(index)
end

--- Reserve tab rows without taking columns from page content.
---@param page_list {id?: string, title?: string}[]
---@param presentation? "tabs"
---@param width integer
---@param height integer
---@return ForgePickerNavigationLayout
function PageNavigation.measure(page_list, presentation, width, height)
  local active = presentation == "tabs" and #page_list > 1
  return {
    mode = active and "tabs" or "none",
    width = width, content_column = 0, content_width = width,
    header_height = active and 2 or 0,
    minimum_height = math.min(height, active and 4 or 1),
  }
end

--- Shift rendered row anchors while leaving logical viewport and scroll offsets untouched.
local function shift_rows(frame, amount)
  for _, name in ipairs({ "option_range", "primary_range", "child_range", "content_range" }) do
    for _, range in pairs(frame[name] or {}) do
      range.first, range.last = range.first + amount, range.last + amount
    end
  end
  for index, line in ipairs(frame.section_line or {}) do frame.section_line[index] = line + amount end
  for _, span in ipairs(frame.content_span_list or {}) do span.line = span.line + amount end
  for _, name in ipairs({ "search_start", "input_start", "footer_line", "content_hint_line" }) do
    if frame[name] then frame[name] = frame[name] + amount end
  end
end

--- Keep a contiguous range of complete tabs around the active page.
local function tab_strip(page_list, page_index, width)
  local labels = {}
  for index, page in ipairs(page_list) do
    local label = page_label(page, index)
    labels[index] = index == page_index and ("[" .. label .. "]") or label
  end
  local function strip(first, last)
    return (first > 1 and "‹ " or "") .. table.concat(labels, "  ", first, last)
      .. (last < #labels and " ›" or "")
  end
  local first, last = 1, #labels
  while vim.fn.strdisplaywidth(strip(first, last)) > width and first < last do
    if page_index - first > last - page_index then first = first + 1
    else last = last - 1 end
  end
  local leading, trailing = first > 1 and "‹ " or "", last < #labels and " ›" or ""
  if first == last then
    labels[first] = "[" .. clipped_label(page_label(page_list[first], first),
      math.max(1, width - vim.fn.strdisplaywidth(leading .. trailing) - 2)) .. "]"
  end
  local prefix = leading
  for index = first, page_index - 1 do prefix = prefix .. labels[index] .. "  " end
  return { text = strip(first, last), first = #prefix, last = #prefix + #labels[page_index] }
end

--- Add category context to an existing viewport without mutating its source frame.
---@param frame ForgePickerFrame
---@param geometry ForgePickerNavigationLayout
---@param page_list {id?: string, title?: string}[]
---@param page_index integer
---@return ForgePickerFrame
function PageNavigation.compose(frame, geometry, page_list, page_index)
  if geometry.mode == "none" then return frame end
  local composed = vim.deepcopy(frame)
  composed.content_column = 0
  composed.content_width = geometry.content_width
  composed.body_start_by_line = {}
  composed.content_span_list = composed.content_span_list or {}
  shift_rows(composed, geometry.header_height)
  local strip = tab_strip(page_list, page_index, math.max(1, geometry.width - 4))
  table.insert(composed.lines, 1, "")
  table.insert(composed.lines, 1, "  " .. strip.text)
  for line = 1, #composed.lines do composed.body_start_by_line[line] = 0 end
  composed.content_span_list[#composed.content_span_list + 1] = {
    line = 1, first = 2, last = #composed.lines[1], group = "ForgePickerHint",
  }
  composed.content_span_list[#composed.content_span_list + 1] = {
    line = 1, first = strip.first + 2, last = strip.last + 2, group = "ForgePickerSelected", priority = 250,
  }
  return composed
end

return PageNavigation
