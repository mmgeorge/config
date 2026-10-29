local M = {}
local gutter = require("forge.gutter")
local sessions = setmetatable({}, { __mode = "v" })
local namespace = vim.api.nvim_create_namespace("forge.visible.decorations")
local code_info = {}

---Identify background captures that fill the display row beyond its text.
---@param capture string
---@return boolean
function M.full_width(capture)
  return capture == "ForgeAddBg" or capture == "ForgeDeleteBg" or capture == "ForgeModifyBg" or capture == "RenderMarkdownCode"
    or capture == "ForgeReviewCommentBox" or capture == "ForgeReviewCommentBoxHeader"
end

---@class ForgeProjectedDecoration
---@field column integer
---@field options vim.api.keyset.set_extmark

---Project an intersecting source span onto one physical row, retaining full-row backgrounds.
---@param decoration {range: {start: {row: integer, column: integer}, ["end"]: {row: integer, column: integer}}, capture: string, priority: integer}
---@param relative integer Source row within the block.
---@param row integer Physical buffer row.
---@param length? integer Current source byte length, when edited text needs clipping.
---@param prefix? integer Materialized presentation columns before the source text.
---@return ForgeProjectedDecoration? # Nil when the source row does not intersect the span or clipping removes it.
function M.project_row(decoration, relative, row, length, prefix)
  local first, last = decoration.range.start, decoration.range["end"]
  if relative < first.row or relative > last.row or (relative == last.row and last.column == 0) then return nil end
  local column = relative == first.row and first.column or 0
  local full_width = M.full_width(decoration.capture)
  local continues = last.row > relative
  local finish = continues and (length or 0) or last.column
  if length then finish = math.min(finish, length) end
  if not continues and finish <= column then return nil end
  return {
    column = full_width and 0 or column + (prefix or 0),
    options = {
      end_row = full_width and row or continues and row + 1 or row,
      end_col = (continues or full_width) and 0 or finish + (prefix or 0),
      hl_group = decoration.capture, hl_eol = full_width, priority = decoration.priority,
      line_hl_group = full_width and decoration.capture or nil,
    },
  }
end

local function overlay_chunks(overlay)
  local language = overlay.capture:match("^RenderMarkdownCodeInfo:(.+)$")
  if not language then return { { overlay.text, overlay.capture } } end
  if not code_info[language] then
    local icon, capture = "", "RenderMarkdownCodeInfo"
    local loaded, devicons = pcall(require, "nvim-web-devicons")
    if loaded then icon, capture = devicons.get_icon_by_filetype(language, { default = true }) end
    code_info[language] = { icon or "", capture or "RenderMarkdownCodeInfo" }
  end
  return { { code_info[language][1] .. " ", code_info[language][2] },
    { overlay.text, code_info[language][2] } }
end

local function build(values, first, last)
  if first > last then return nil end
  local middle = math.floor((first + last) / 2)
  local span = values[middle]
  local left, right = build(values, first, middle - 1), build(values, middle + 1, last)
  local finish = span.range["end"]
  local end_row = finish.row - (finish.column == 0 and 1 or 0)
  return { span = span, left = left, right = right,
    finish = math.max(end_row, left and left.finish or -1, right and right.finish or -1) }
end

local function emit(tree, buffer, row, relative, prefix, indent, continuation)
  if not tree or tree.finish < relative then return end
  emit(tree.left, buffer, row, relative, prefix, indent, continuation)
  local span = tree.span
  if span.range.start.row > relative then return end
  local projected = M.project_row(span, relative, row, nil, prefix)
  if projected then
    projected.options.ephemeral = true
    if projected.options.hl_eol and indent > 0 then
      local chunks, column = {}, 0
      for _, marker in ipairs(continuation or {}) do
        if marker.column < indent then
          chunks[#chunks + 1] = { string.rep(" ", marker.column - column), span.capture }
          chunks[#chunks + 1] = { marker.text, { span.capture, marker.capture } }
          column = marker.column + #marker.text
        end
      end
      chunks[#chunks + 1] = { string.rep(" ", indent - column), span.capture }
      projected.options.virt_text = chunks
      projected.options.virt_text_win_col = 0
      projected.options.virt_text_repeat_linebreak = true
      projected.options.hl_mode = "combine"
    end
    vim.api.nvim_buf_set_extmark(buffer, namespace, row, projected.column, projected.options)
  end
  emit(tree.right, buffer, row, relative, prefix, indent, continuation)
end

vim.api.nvim_set_decoration_provider(namespace, {
  on_win = function(_, _, buffer)
    local session = sessions[buffer]
    if session and session.prepare_source then session.prepare_source() end
    return session and session.status == "Applied" and not session.applying or false
  end,
  on_line = function(_, window, buffer, row)
    local session = sessions[buffer]
    if not session or session.status ~= "Applied" or session.applying then return end
    if session.draw_header and session.draw_header(row, namespace) then return end
    local location = session.decoration_location and session.decoration_location(row)
      or not session.decoration_location and require("forge.buffer").locate(session, row, 0)
    local node = location and session.sequence.node[location.block]
    if not location or not node then return end
    local relative = location.position.row
    local prefix = location.display_indent or 0
    local row_layout = node.entry.row_layout[relative]
    local continuation = row_layout and row_layout.continuation
    local indent = 0
    if vim.wo[window].breakindent then
      local text = vim.api.nvim_buf_get_lines(buffer, row, row + 1, false)[1] or ""
      local available = vim.api.nvim_win_get_width(window) - vim.fn.getwininfo(window)[1].textoff
      local minimum = tonumber(vim.wo[window].breakindentopt:match("min:(%d+)")) or 20
      indent = math.min(vim.fn.strdisplaywidth(text:match("^%s*") or ""), math.max(0, available - minimum))
    end
    if relative == 0 then
      require("forge.content_layout").draw_fold(window, buffer, namespace, row,
        node.entry.metadata.layout, require("forge.nodes").display(node.entry))
    end
    if indent > 0 then emit(node.entry.row_background, buffer, row, relative, prefix, indent, continuation) end
    emit(node.entry.visible_decoration, buffer, row, relative, prefix, indent, continuation)
    local source_overlay = node.entry.source_overlay_row[relative]
    if source_overlay or node.entry.source_highlight then
      local mode = vim.api.nvim_get_mode().mode:sub(1, 1):lower()
      if mode == "\22" then mode = "v" end
      local cursor = vim.api.nvim_win_get_cursor(window)[1]
      local selected = cursor == row + 1
      if mode == "v" and window == vim.api.nvim_get_current_win() then
        local anchor = vim.fn.line("v")
        selected = row + 1 >= math.min(cursor, anchor) and row + 1 <= math.max(cursor, anchor)
      end
      local reveal = vim.wo[window].conceallevel == 0
        or (selected and not vim.wo[window].concealcursor:find(mode, 1, true))
      if not reveal then emit(node.entry.source_highlight, buffer, row, relative, prefix, indent) end
      for _, overlay in ipairs(source_overlay or {}) do
        local right_aligned = overlay.capture == "ForgeRightAlignedOwner"
        if not reveal or right_aligned then
          if right_aligned and overlay.range.start.column < overlay.range["end"].column then
            vim.api.nvim_buf_set_extmark(buffer, namespace, row, overlay.range.start.column + prefix, {
              virt_text = { { string.rep(" ", vim.fn.strdisplaywidth(overlay.text)), "Normal" } },
              virt_text_pos = "overlay", hl_mode = "replace",
              priority = overlay.priority + 1, ephemeral = true,
            })
          end
          local options = {
            virt_text = right_aligned and { { overlay.text, "ForgePlanMetadata" } } or overlay_chunks(overlay),
            virt_text_pos = right_aligned and "right_align" or "overlay",
            hl_mode = "combine", priority = overlay.priority, ephemeral = true,
          }
          vim.api.nvim_buf_set_extmark(buffer, namespace, row, overlay.range.start.column + prefix, options)
        end
      end
    end
    gutter.highlight(session, window, row, namespace)
  end,
})

---@class ForgeDecoratedEntry: ForgeLayoutEntry
---@field source_overlay_row table<integer, table[]>
---@field row_background? table
---@field visible_decoration? table
---@field source_highlight? table

---@param entry ForgeDecoratedEntry
---@param read_row? fun(row: integer): string
function M.prepare(entry, read_row)
  entry.source_overlay_row = {}
  for _, overlay in ipairs(entry.metadata.source_overlay or {}) do
    local row = overlay.range.start.row
    entry.source_overlay_row[row] = entry.source_overlay_row[row] or {}
    entry.source_overlay_row[row][#entry.source_overlay_row[row] + 1] = overlay
  end
  require("forge.content_layout").prepare(entry, read_row)
  local background = {}
  for _, span in ipairs(entry.metadata.decoration or {}) do
    if M.full_width(span.capture) then background[#background + 1] = span end
  end
  table.sort(background, function(left, right) return left.range.start.row < right.range.start.row end)
  entry.row_background = build(background, 1, #background)
  for _, kind in ipairs({ "visible_decoration", "source_highlight" }) do
    local values = vim.list_extend({}, entry.metadata[kind] or {})
    local order = {}
    for index, span in ipairs(values) do order[span] = index end
    table.sort(values, function(left, right)
      if left.range.start.row ~= right.range.start.row then return left.range.start.row < right.range.start.row end
      return order[left] < order[right]
    end)
    entry[kind] = build(values, 1, #values)
  end
end

function M.attach(session)
  sessions[session.buffer] = session
  -- Highlight metadata can change without invalidating Neovim's cached screen rows.
  vim.api.nvim__redraw({ buf = session.buffer, valid = false, flush = false })
end

function M.detach(session)
  sessions[session.buffer] = nil
end

return M
