local M = {}

---@class ForgeContentLayout
---@field indent integer
---@field source_indent? integer
---@field marker? {text: string, capture: string}

---@class ForgePrefixInsertion
---@field position {row: integer, column: integer}
---@field chunk {text: string, capture: string}[]
---@field priority integer
---@field width integer

---@class ForgeRowLayout
---@field column integer
---@field width integer
---@field insertion ForgePrefixInsertion[]
---@field continuation {column: integer, text: string, capture: string}[]

---@class ForgePrefixSign
---@field row integer
---@field column integer
---@field text string
---@field capture string
---@field priority integer

---@class ForgeLayoutMetadata
---@field layout? ForgeContentLayout
---@field gutter? table[]
---@field source_overlay? table[]

---@class ForgeLayoutEntry
---@field row_count integer
---@field text? string[]
---@field metadata ForgeLayoutMetadata
---@field row_layout table<integer, ForgeRowLayout>
---@field prefix_sign ForgePrefixSign[]

---Prepare source-preserving insertions once for rendering, navigation, and copying.
---@param entry ForgeLayoutEntry
---@param read_row? fun(row: integer): string Zero-based row within the block.
function M.prepare(entry, read_row)
  ---@type table<integer, ForgeRowLayout>
  entry.row_layout = {}
  ---@type ForgePrefixSign[]
  entry.prefix_sign = {}
  local function append(row, column, chunks, priority)
    local row_layout = entry.row_layout[row]
    if not row_layout then
      row_layout = { column = column, width = 0, insertion = {}, continuation = {} }
      entry.row_layout[row] = row_layout
    end
    local insertion = row_layout.insertion[#row_layout.insertion]
    if not insertion or insertion.position.column ~= column then
      insertion = { position = { row = row, column = column }, chunk = {}, priority = priority, width = 0 }
      row_layout.insertion[#row_layout.insertion + 1] = insertion
    end
    insertion.priority = math.max(insertion.priority, priority)
    for _, chunk in ipairs(chunks) do insertion.chunk[#insertion.chunk + 1] = chunk end
  end
  local layout = entry.metadata.layout
  if layout then
    local width = math.max(0, layout.indent - 2 - (layout.source_indent or 0))
    if width > 0 then
      local chunks = { { text = string.rep(" ", width), capture = "Normal" } }
      for row = 0, entry.row_count - 1 do append(row, 0, chunks, 200) end
    end
  end
  local ordered, order = {}, {}
  for index, gutter in ipairs(entry.metadata.gutter or {}) do
    ordered[index], order[gutter] = gutter, index
  end
  table.sort(ordered, function(left, right)
    if left.position.row ~= right.position.row then return left.position.row < right.position.row end
    if left.position.column ~= right.position.column then return left.position.column < right.position.column end
    if left.priority ~= right.priority then return left.priority < right.priority end
    return order[left] < order[right]
  end)
  for _, gutter in ipairs(ordered) do
    local chunks = gutter.chunk
    if gutter.placement == "sign" then
      local text = {}
      for _, chunk in ipairs(chunks) do text[#text + 1] = chunk.text end
      local label = table.concat(text)
      local marker = vim.trim(label)
      if marker == "" then
        chunks = {}
      elseif vim.fn.strdisplaywidth(marker) <= 2 then
        entry.prefix_sign[#entry.prefix_sign + 1] = { row = gutter.position.row, column = gutter.position.column,
          text = marker, capture = chunks[1].capture, priority = gutter.priority }
        local indent = label:match("^ +")
        chunks = indent and { { text = indent, capture = "Normal" } } or {}
      end
    end
    if #chunks > 0 then append(gutter.position.row, gutter.position.column, chunks, gutter.priority) end
  end
  for row, row_layout in pairs(entry.row_layout) do
    local preceding_width = 0
    for _, insertion in ipairs(row_layout.insertion) do
      local start = preceding_width
      for _, chunk in ipairs(insertion.chunk) do
        if chunk.text:find("\t", 1, true) then
          local text = read_row and read_row(row) or assert(entry.text and entry.text[row + 1], "tabbed prefix requires source row")
          start = start + vim.fn.strdisplaywidth(text:sub(1, insertion.position.column))
          break
        end
      end
      local width = 0
      for _, chunk in ipairs(insertion.chunk) do
        if (chunk.text == "+" or chunk.text == "~" or chunk.text == "-")
          and (chunk.capture == "ForgeAddLineNr" or chunk.capture == "ForgeModifyLineNr" or chunk.capture == "ForgeDeleteLineNr") then
          row_layout.continuation[#row_layout.continuation + 1] = {
            column = start + width, text = chunk.text, capture = chunk.capture,
          }
        end
        width = width + vim.fn.strdisplaywidth(chunk.text, start + width)
      end
      insertion.width = width
      preceding_width = preceding_width + width
    end
    local first = row_layout.insertion[1]
    row_layout.column, row_layout.width = first.position.column, first.width
  end
end

---Measure one row's leading presentation columns.
---@param entry ForgeLayoutEntry
---@param row integer
---@return integer
function M.leading_width(entry, row)
  local width = 0
  local layout = entry.row_layout and entry.row_layout[row]
  for _, insertion in ipairs(layout and layout.insertion or {}) do
    if insertion.position.column == 0 then width = width + insertion.width end
  end
  return width
end

---Resolve the displayed marker from the current expansion state.
---@param entry ForgeLayoutEntry
---@return {text: string, capture: string}?
function M.marker(entry)
  local layout = entry.metadata.layout
  local marker = layout and layout.marker
  if not marker or marker == vim.NIL then return nil end
  if marker.text ~= "▸" then return marker end
  local display = require("forge.nodes").display(entry)
  return {
    text = (display.display == "heading" or display.kind == "message" and display.display == "preview") and "▸" or "▾",
    capture = marker.capture,
  }
end

---@param buffer integer
---@param namespace integer
---@param start_row integer
---@param entry ForgeLayoutEntry
---@param physical_row? fun(row: integer): integer? Maps visible source rows into an annotated buffer.
---@param materialized? boolean Leading presentation columns already exist as display whitespace.
---@return integer[]
function M.install(buffer, namespace, start_row, entry, physical_row, materialized)
  local marks = {}
  for row, row_layout in pairs(entry.row_layout) do
    local target_row = physical_row and physical_row(start_row + row) or nil
    if not physical_row then target_row = start_row + row end
    if target_row ~= nil then
      for _, insertion in ipairs(row_layout.insertion) do
        local chunks = {}
        for _, chunk in ipairs(insertion.chunk) do chunks[#chunks + 1] = { chunk.text, chunk.capture } end
        local leading = materialized and M.leading_width(entry, row) or 0
        local column = insertion.position.column == 0 and 0 or insertion.position.column + leading
        marks[#marks + 1] = vim.api.nvim_buf_set_extmark(buffer, namespace,
          target_row, column, {
          virt_text = chunks, virt_text_pos = materialized and column == 0 and "overlay" or "inline", hl_mode = "combine", priority = insertion.priority,
          right_gravity = true, strict = true,
        })
      end
    end
  end
  for _, sign in ipairs(entry.prefix_sign) do
    local target_row = physical_row and physical_row(start_row + sign.row) or nil
    if not physical_row then target_row = start_row + sign.row end
    if target_row ~= nil then
      marks[#marks + 1] = vim.api.nvim_buf_set_extmark(buffer, namespace,
        target_row, sign.column, {
        sign_text = sign.text, sign_hl_group = sign.capture, priority = sign.priority, right_gravity = true, strict = true,
      })
    end
  end
  local layout = entry.metadata.layout
  local marker = layout and layout.marker
  if layout and marker and marker ~= vim.NIL and marker.text ~= "▸" then
    local options = { priority = 201, right_gravity = true, strict = true }
    if layout.indent == 2 then
      options.sign_text, options.sign_hl_group = marker.text, marker.capture
    elseif layout.indent > 2 then
      options.virt_text, options.virt_text_win_col = { { marker.text, marker.capture } }, layout.indent - 4
    end
    local target_row = physical_row and physical_row(start_row) or nil
    if not physical_row then target_row = start_row end
    if target_row ~= nil and (options.sign_text or options.virt_text) then
      marks[#marks + 1] = vim.api.nvim_buf_set_extmark(buffer, namespace,
        target_row, 0, options)
    end
  end
  return marks
end

function M.draw_fold(window, buffer, namespace, row, layout, state)
  local marker = layout and layout.marker
  if not marker or marker == vim.NIL or marker.text ~= "▸" or layout.indent == 2 then return end
  local icon = state and (state.display == "heading" or state.kind == "message" and state.display == "preview") and "▸" or "▾"
  vim.api.nvim_buf_set_extmark(buffer, namespace, row, 0, {
    ephemeral = true, priority = 201,
    virt_text = { { icon, marker.capture } }, virt_text_win_col = layout.indent - 4,
  })
end

return M
