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
---@param options? { input_visible?: boolean }
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

  local label_width = 0
  for _, option in ipairs(page.option_list) do
    label_width = math.max(label_width, vim.fn.strdisplaywidth(option.label or ""))
  end
  label_width = math.min(label_width, math.max(12, math.floor(usable_width * 0.42)))
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
    local detail = option.detail or ""
    local label = option.label or ""
    local first = #lines + 1
    local two_column_width = vim.fn.strdisplaywidth(prefix) + label_width + 2 + vim.fn.strdisplaywidth(detail)
    if detail ~= "" and two_column_width <= usable_width then
      lines[#lines + 1] = prefix .. label .. string.rep(" ", label_width - vim.fn.strdisplaywidth(label) + 2) .. detail
    else
      append(lines, wrap(label, usable_width, prefix))
      if detail ~= "" then append(lines, wrap(detail, usable_width, "      ")) end
    end
    local primary_last = #lines
    for _, child in ipairs(option.child_line_list or {}) do
      append(lines, wrap(child, usable_width, "    ", "      "))
    end
    option_range[index] = { first = first, last = #lines }
    primary_range[index] = { first = first, last = primary_last }
    if primary_last < #lines then child_range[index] = { first = primary_last + 1, last = #lines } end
  end
  if #page.option_list == 0 then lines[#lines + 1] = "  " .. (page.empty_text or "No matching options.") end

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
