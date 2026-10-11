local PickerLayout = {}

---@class ForgePickerColumnSpan
---@field first integer Zero-based byte offset in the rendered row.
---@field last integer Exclusive byte offset in the rendered row.
---@field group string
---@field priority? integer

---@class ForgePickerContent
---@field text string
---@field group? string
---@field preformatted? boolean
---@field spans? ForgePickerColumnSpan[]
---@field focus_offset? integer Zero-based byte offset initially kept visible.

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

local function wrap_preformatted(text, width)
  local result, segment_list = {}, {}
  local source_offset = 0
  local function append_segment(source, prefix, offset)
    result[#result + 1] = prefix .. source
    segment_list[#segment_list + 1] = { line = #result, first = offset, last = offset + #source, prefix = #prefix }
  end
  for _, source in ipairs(vim.split(text, "\n", { plain = true })) do
    local line_size, consumed = #source, 0
    local indent = source:match("^%s*") or ""
    local prefix = "  "
    while vim.fn.strdisplaywidth(prefix .. source) > width do
      local count = 0
      while count < vim.fn.strchars(source)
        and vim.fn.strdisplaywidth(prefix .. vim.fn.strcharpart(source, 0, count + 1)) <= width do
        count = count + 1
      end
      count = math.max(1, count)
      local segment = vim.fn.strcharpart(source, 0, count)
      append_segment(segment, prefix, source_offset + consumed)
      consumed = consumed + #segment
      source = vim.fn.strcharpart(source, count)
      prefix = "  " .. indent .. "  "
      if vim.fn.strdisplaywidth(prefix) >= width - 1 then prefix = "    " end
    end
    append_segment(source, prefix, source_offset + consumed)
    source_offset = source_offset + line_size + 1
  end
  return { lines = result, segment_list = segment_list }
end

---@param page { option_list: table[], show_item_counter?: boolean, highlight_selected_line?: boolean, highlight_selected_text?: boolean, [string]: any }
---@param selected_index integer
---@param width integer
---@param options? { input_visible?: boolean, search_visible?: boolean, footer?: string }
---@return table
function PickerLayout.build(page, selected_index, width, options)
  options = options or {}
  local lines = {}
  local option_range = {}
  local primary_range = {}
  local child_range = {}
  local section_line = {}
  local content_range = {}
  local content_span_list, content_focus_line = {}, nil
  local usable_width = math.max(20, width - 4)
  if page.subtitle and page.subtitle ~= "" then
    append(lines, wrap(page.subtitle, usable_width, "  "))
    lines[#lines + 1] = ""
  end
  local search_start = nil
  if page.search and options.search_visible ~= false then
    search_start = #lines + 1
    lines[#lines + 1] = ""
    lines[#lines + 1] = ""
  end
  local content_start = #lines + 1
  for _, content in ipairs(page.content_list or {}) do
    local first = #lines + 1
    if content.preformatted then
      local wrapped = wrap_preformatted(content.text, usable_width)
      append(lines, wrapped.lines)
      for _, segment in ipairs(wrapped.segment_list) do
        local row = first + segment.line - 1
        if content.focus_offset and not content_focus_line
          and content.focus_offset >= segment.first and content.focus_offset < segment.last then
          content_focus_line = row
        end
        for _, span in ipairs(content.spans or {}) do
          local start, final = math.max(span.first, segment.first), math.min(span.last, segment.last)
          if start < final then
            content_span_list[#content_span_list + 1] = {
              line = row, first = segment.prefix + start - segment.first,
              last = segment.prefix + final - segment.first, group = span.group, priority = span.priority,
            }
          end
        end
      end
    else
      append(lines, wrap(content.text, usable_width, "  "))
    end
    content_range[#content_range + 1] = { first = first, last = #lines, group = content.group }
  end
  local content_end = #lines
  if #(page.content_list or {}) > 0 and #page.option_list > 0 then lines[#lines + 1] = "" end

  local column_width = {}
  for column, heading in ipairs(page.column_headers or {}) do
    column_width[column] = vim.fn.strdisplaywidth(heading)
  end
  local prefix_width = 2
  for _, option in ipairs(page.option_list) do
    if option.key then prefix_width = math.max(prefix_width, 4 + vim.fn.strdisplaywidth(option.key)) end
    for column, value in ipairs(option.columns or { option.label or "", option.detail or "" }) do
      column_width[column] = math.max(column_width[column] or 0, vim.fn.strdisplaywidth(value))
    end
  end
  local total = prefix_width + math.max(0, #column_width - 1) * 2
  for _, size in ipairs(column_width) do total = total + size end
  if page.flexible_column and column_width[page.flexible_column] then
    local reduction = math.min(math.max(0, total - usable_width), math.max(0, column_width[page.flexible_column] - 3))
    column_width[page.flexible_column] = column_width[page.flexible_column] - reduction
    total = total - reduction
  end
  while total > usable_width do
    local widest = nil
    for column, size in ipairs(column_width) do
      if size > 3 and (not widest or size > column_width[widest]) then widest = column end
    end
    if not widest then break end
    column_width[widest] = column_width[widest] - 1
    total = total - 1
  end
  ---@param values string[]
  ---@param prefix string
  ---@param column_segments? table<integer, table[]> Ordered text and highlight tuples for each column.
  ---@param column_spans? table<integer, ForgePickerColumnSpan[]> Overlapping source captures for each column.
  ---@return { text: string, spans: ForgePickerColumnSpan[] }
  local function column_row(values, prefix, column_segments, column_spans)
    local cells = {}
    local spans = {}
    local offset = #prefix
    for column, value in ipairs(values) do
      local size = column_width[column]
      local retained = #value
      if vim.fn.strdisplaywidth(value) > size then
        local count = vim.fn.strchars(value)
        repeat count = count - 1 until count == 0 or vim.fn.strdisplaywidth(vim.fn.strcharpart(value, 0, count)) <= size - 1
        value = vim.fn.strcharpart(value, 0, count)
        retained = #value
        value = value .. "…"
      end
      local segment_offset = 0
      for _, segment in ipairs(column_segments and column_segments[column] or {}) do
        local final = math.min(retained, segment_offset + #segment[1])
        if segment[2] and segment_offset < final then
          spans[#spans + 1] = { first = offset + segment_offset, last = offset + final, group = segment[2] }
        end
        segment_offset = segment_offset + #segment[1]
      end
      for _, span in ipairs(column_spans and column_spans[column] or {}) do
        local final = math.min(retained, span.last)
        if span.first < final then
          spans[#spans + 1] = { first = offset + span.first, last = offset + final, group = span.group, priority = span.priority }
        end
      end
      cells[column] = value .. string.rep(" ", math.max(0, size - vim.fn.strdisplaywidth(value)))
      offset = offset + #cells[column] + 2
    end
    return { text = prefix .. table.concat(cells, "  "):gsub("%s+$", ""), spans = spans }
  end
  if page.column_headers then
    lines[#lines + 1] = column_row(page.column_headers, string.rep(" ", prefix_width)).text
    content_range[#content_range + 1] = { first = #lines, last = #lines, group = "ForgePickerHint" }
  end
  local header_height = #lines
  local previous_section = nil
  for index, option in ipairs(page.option_list) do
    if option.section and option.section ~= previous_section then
      if #lines > 0 and lines[#lines] ~= "" then lines[#lines + 1] = "" end
      section_line[#section_line + 1] = #lines + 1
      lines[#lines + 1] = "  " .. option.section
      previous_section = option.section
    end
    local key = option.key and (option.key .. "  ") or ""
    local prefix = "  " .. key
    prefix = prefix .. string.rep(" ", prefix_width - vim.fn.strdisplaywidth(prefix))
    local first = #lines + 1
    local row = column_row(option.columns or { option.label or "", option.detail or "" }, prefix, option.column_segments, option.column_spans)
    if option.wrap_label then
      append(lines, wrap(option.label, usable_width, prefix, string.rep(" ", prefix_width)))
    else
      lines[#lines + 1] = row.text
    end
    local primary_last = #lines
    for _, child in ipairs(option.child_line_list or {}) do
      append(lines, wrap(child, usable_width, "    ", "      "))
    end
    option_range[index] = { first = first, last = #lines }
    primary_range[index] = { first = first, last = primary_last, spans = row.spans, key_end = option.key and (2 + #option.key) }
    if primary_last < #lines then child_range[index] = { first = primary_last + 1, last = #lines } end
  end
  if #page.option_list == 0 then append(lines, wrap(page.empty_text or "No matching options.", usable_width, "  ")) end

  local body_end = #lines
  local input_start = nil
  if options.input_visible then
    lines[#lines + 1] = ""
    input_start = #lines + 1
    local reserve = math.max(3, page.input_height or 3)
    for _ = 1, reserve do lines[#lines + 1] = "" end
  end
  local footer_line = #lines + 1
  local footer = "  " .. (options.footer or page.footer or "↑↓ select  Enter confirm  q close")
  if page.show_item_counter then
    local count = #page.option_list
    local counter = ("%d of %d"):format(count == 0 and 0 or selected_index, count)
    local footer_width = math.max(0, width - vim.fn.strdisplaywidth(counter) - 4)
    local character_count = vim.fn.strchars(footer)
    while vim.fn.strdisplaywidth(footer) > footer_width do
      character_count = character_count - 1
      footer = vim.fn.strcharpart(footer, 0, character_count)
    end
    footer = footer .. string.rep(" ", width - 2 - vim.fn.strdisplaywidth(footer) - vim.fn.strdisplaywidth(counter)) .. counter
  end
  lines[#lines + 1] = footer
  return {
    lines = lines,
    header_height = header_height,
    body_end = body_end,
    option_range = option_range,
    primary_range = primary_range,
    child_range = child_range,
    section_line = section_line,
    content_range = content_range,
    content_span_list = content_span_list,
    content_focus_line = content_focus_line,
    scroll_range = page.scroll_content and content_end >= content_start
      and { first = content_start, last = content_end } or nil,
    selected_index = selected_index,
    highlight_selected_line = page.highlight_selected_line == true,
    highlight_selected_text = page.highlight_selected_text ~= false,
    search_start = search_start,
    input_start = input_start,
    footer_line = footer_line,
  }
end

---@param frame table
---@param height integer
---@param previous_top? integer
---@param content_offset? integer
---@return table
function PickerLayout.viewport(frame, height, previous_top, content_offset)
  local header_height = frame.header_height
  local suffix_height = #frame.lines - frame.body_end
  local visible_header_height = header_height
  local content_first, content_last, content_hint
  local scroll = frame.scroll_range
  if scroll then
    local content_height = scroll.last - scroll.first + 1
    local fixed_height = header_height - content_height
    local option_height = math.min(7, frame.body_end - header_height)
    local content_capacity = math.max(1, height - fixed_height - suffix_height - option_height)
    if content_height > content_capacity then
      content_capacity = math.max(1, content_capacity - 1)
      local initial_offset = frame.content_focus_line and frame.content_focus_line - scroll.first - math.floor(content_capacity / 2) or 0
      content_first = scroll.first + math.max(0, math.min(content_offset or initial_offset, content_height - content_capacity))
      content_last = content_first + content_capacity - 1
      visible_header_height = fixed_height + content_capacity + 1
      content_hint = ("  Lines %d–%d of %d · PgUp/PgDn scroll"):format(
        content_first - scroll.first + 1, content_last - scroll.first + 1, content_height)
    end
  end
  local capacity = math.max(1, height - visible_header_height - suffix_height)
  local top = math.max(header_height + 1, previous_top or header_height + 1)
  local selected = frame.option_range[frame.selected_index]
  if selected then
    if selected.first < top then top = selected.first end
    if selected.first >= top + capacity then top = selected.first - capacity + 1 end
  end
  top = math.max(header_height + 1, math.min(top, frame.body_end - capacity + 1))
  local last = math.min(frame.body_end, top + capacity - 1)
  local projected = vim.deepcopy(frame)
  projected.lines = {}
  local row_map = {}
  for row, line in ipairs(frame.lines) do
    local hidden_content = content_first and row >= scroll.first and row <= scroll.last
      and (row < content_first or row > content_last)
    if not hidden_content and (row <= header_height or (row >= top and row <= last) or row > frame.body_end) then
      projected.lines[#projected.lines + 1] = line
      row_map[row] = #projected.lines
    end
    if content_hint and row == scroll.last then
      projected.lines[#projected.lines + 1] = content_hint
      projected.content_hint_line = #projected.lines
    end
  end
  for _, field in ipairs({ "option_range", "primary_range", "child_range", "content_range" }) do
    projected[field] = {}
    for index, range in pairs(frame[field]) do
      local first, final
      for row = range.first, range.last do
        if row_map[row] then first = first or row_map[row] final = row_map[row] end
      end
      if first then
        local mapped = vim.deepcopy(range)
        mapped.first, mapped.last = first, final
        projected[field][index] = mapped
      end
    end
  end
  if projected.content_hint_line then
    projected.content_range[#frame.content_range + 1] = {
      first = projected.content_hint_line, last = projected.content_hint_line, group = "ForgePickerHint",
    }
  end
  projected.section_line = {}
  projected.content_span_list = {}
  for _, span in ipairs(frame.content_span_list or {}) do
    if row_map[span.line] then
      local visible = vim.tbl_extend("force", span, { line = row_map[span.line] })
      projected.content_span_list[#projected.content_span_list + 1] = visible
    end
  end
  for _, row in ipairs(frame.section_line) do
    if row_map[row] then projected.section_line[#projected.section_line + 1] = row_map[row] end
  end
  for _, field in ipairs({ "search_start", "input_start", "footer_line" }) do
    projected[field] = row_map[frame[field]]
  end
  projected.viewport_top = top
  projected.content_offset = content_first and content_first - scroll.first or 0
  projected.content_page_size = content_first and content_last - content_first + 1 or 1
  return projected
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
