local PickerLayout = {}

local function wrap(text, width, prefix, continuation)
  local result = {}
  local current = prefix
  continuation = continuation or string.rep(" ", vim.fn.strdisplaywidth(prefix))
  for word in tostring(text or ""):gmatch("%S+") do
    local separator = current == prefix and "" or " "
    if vim.fn.strdisplaywidth(current .. separator .. word) > width and current ~= prefix then
      result[#result + 1] = current
      current = continuation .. word
    else
      current = current .. separator .. word
    end
  end
  result[#result + 1] = current
  return result
end

local function append(target, source)
  for _, line in ipairs(source) do target[#target + 1] = line end
end

---@param page table
---@param selected_index integer
---@param width integer
---@param options? { input_visible?: boolean, footer?: string }
---@return table
function PickerLayout.build(page, selected_index, width, options)
  options = options or {}
  local lines = {}
  local option_range = {}
  local primary_range = {}
  local child_range = {}
  local section_line = {}
  local content_range = {}
  local usable_width = math.max(20, width - 4)
  if page.subtitle and page.subtitle ~= "" then
    append(lines, wrap(page.subtitle, usable_width, "  "))
    lines[#lines + 1] = ""
  end
  local search_start = nil
  if page.search then
    search_start = #lines + 1
    lines[#lines + 1] = ""
    lines[#lines + 1] = ""
  end
  for _, content in ipairs(page.content_list or {}) do
    local first = #lines + 1
    append(lines, wrap(content.text, usable_width, "  "))
    content_range[#content_range + 1] = { first = first, last = #lines, group = content.group }
  end
  if #(page.content_list or {}) > 0 and #page.option_list > 0 then lines[#lines + 1] = "" end

  local column_width = {}
  for column, heading in ipairs(page.column_headers or {}) do
    column_width[column] = vim.fn.strdisplaywidth(heading)
  end
  local prefix_width = 5
  for _, option in ipairs(page.option_list) do
    prefix_width = math.max(prefix_width, 2 + vim.fn.strdisplaywidth(option.key or " ") + 2)
    for column, value in ipairs(option.columns or { option.label or "", option.detail or "" }) do
      column_width[column] = math.max(column_width[column] or 0, vim.fn.strdisplaywidth(value))
    end
  end
  local total = prefix_width + math.max(0, #column_width - 1) * 2
  for _, size in ipairs(column_width) do total = total + size end
  while total > usable_width do
    local widest = nil
    for column, size in ipairs(column_width) do
      if size > 3 and (not widest or size > column_width[widest]) then widest = column end
    end
    if not widest then break end
    column_width[widest] = column_width[widest] - 1
    total = total - 1
  end
  local function column_row(values, prefix)
    local cells = {}
    for column, value in ipairs(values) do
      local size = column_width[column]
      if vim.fn.strdisplaywidth(value) > size then
        local count = vim.fn.strchars(value)
        repeat count = count - 1 until count == 0 or vim.fn.strdisplaywidth(vim.fn.strcharpart(value, 0, count)) <= size - 1
        value = vim.fn.strcharpart(value, 0, count) .. "…"
      end
      cells[column] = value .. string.rep(" ", math.max(0, size - vim.fn.strdisplaywidth(value)))
    end
    return prefix .. table.concat(cells, "  "):gsub("%s+$", "")
  end
  if page.column_headers then
    lines[#lines + 1] = column_row(page.column_headers, string.rep(" ", prefix_width))
    section_line[#section_line + 1] = #lines
  end
  local previous_section = nil
  for index, option in ipairs(page.option_list) do
    if option.section and option.section ~= previous_section then
      if #lines > 0 and lines[#lines] ~= "" then lines[#lines + 1] = "" end
      section_line[#section_line + 1] = #lines + 1
      lines[#lines + 1] = "  " .. option.section
      previous_section = option.section
    end
    local key = option.key and (option.key .. "  ") or "   "
    local prefix = "  " .. key
    prefix = prefix .. string.rep(" ", prefix_width - vim.fn.strdisplaywidth(prefix))
    local first = #lines + 1
    lines[#lines + 1] = column_row(option.columns or { option.label or "", option.detail or "" }, prefix)
    local primary_last = #lines
    for _, child in ipairs(option.child_line_list or {}) do
      append(lines, wrap(child, usable_width, "    ", "      "))
    end
    option_range[index] = { first = first, last = #lines }
    primary_range[index] = { first = first, last = primary_last }
    if primary_last < #lines then child_range[index] = { first = primary_last + 1, last = #lines } end
  end
  if #page.option_list == 0 then append(lines, wrap(page.empty_text or "No matching options.", usable_width, "  ")) end

  local input_start = nil
  if options.input_visible then
    lines[#lines + 1] = ""
    input_start = #lines + 1
    local reserve = math.max(3, page.input_height or 3)
    for _ = 1, reserve do lines[#lines + 1] = "" end
  end
  lines[#lines + 1] = ""
  local footer_line = #lines + 1
  lines[#lines + 1] = "  " .. (options.footer or page.footer or "↑↓ select  Enter confirm  q close")
  return {
    lines = lines,
    option_range = option_range,
    primary_range = primary_range,
    child_range = child_range,
    section_line = section_line,
    content_range = content_range,
    selected_index = selected_index,
    search_start = search_start,
    input_start = input_start,
    footer_line = footer_line,
  }
end

---@param window_list integer[]
---@return table
function PickerLayout.host_bounds(window_list)
  local top, left = math.huge, math.huge
  local bottom, right = 0, 0
  for _, win in ipairs(window_list) do
    if vim.api.nvim_win_is_valid(win) then
      local position = vim.api.nvim_win_get_position(win)
      top = math.min(top, position[1])
      left = math.min(left, position[2])
      bottom = math.max(bottom, position[1] + vim.api.nvim_win_get_height(win))
      right = math.max(right, position[2] + vim.api.nvim_win_get_width(win))
    end
  end
  assert(top < math.huge, "picker requires a valid host window")
  return { top = top, left = left, bottom = bottom, right = right, width = right - left, height = bottom - top }
end

return PickerLayout
