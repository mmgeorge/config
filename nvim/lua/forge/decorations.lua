local M = {}
local gutter = require("forge.gutter")
local sessions = setmetatable({}, { __mode = "v" })
local namespace = vim.api.nvim_create_namespace("forge.visible.decorations")
local code_info = {}

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

local function emit(tree, buffer, row, relative)
  if not tree or tree.finish < relative then return end
  emit(tree.left, buffer, row, relative)
  local span = tree.span
  if span.range.start.row > relative then return end
  local finish = span.range["end"]
  if finish.row > relative or (finish.row == relative and finish.column > 0) then
    vim.api.nvim_buf_set_extmark(buffer, namespace, row, span.range.start.row == relative and span.range.start.column or 0, {
      end_row = finish.row > relative and row + 1 or row,
      end_col = finish.row > relative and 0 or finish.column,
      hl_group = span.capture, hl_eol = span.capture == "RenderMarkdownCode",
      priority = span.priority, ephemeral = true,
    })
  end
  emit(tree.right, buffer, row, relative)
end

vim.api.nvim_set_decoration_provider(namespace, {
  on_win = function(_, _, buffer)
    local session = sessions[buffer]
    return session and session.status == "Applied" and not session.applying and not session.editable.suspended or false
  end,
  on_line = function(_, window, buffer, row)
    local session = sessions[buffer]
    if not session or session.status ~= "Applied" or session.applying or session.editable.suspended then return end
    if session.draw_header and session.draw_header(row, namespace) then return end
    local node = session.sequence:locate(row)
    if not node then return end
    local _, start = session.sequence:position(node.id)
    emit(node.entry.visible_decoration, buffer, row, row - start)
    local source_overlay = node.entry.source_overlay_row[row - start]
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
      if not reveal then emit(node.entry.source_highlight, buffer, row, row - start) end
      for _, overlay in ipairs(source_overlay or {}) do
        local right_aligned = overlay.capture == "ForgeRightAlignedOwner"
        if not reveal or right_aligned then
          if right_aligned then
            vim.api.nvim_buf_set_extmark(buffer, namespace, row, overlay.range.start.column, {
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
          vim.api.nvim_buf_set_extmark(buffer, namespace, row, overlay.range.start.column, options)
        end
      end
    end
    gutter.highlight(session, window, row, namespace)
  end,
})

function M.prepare(entry)
  entry.source_overlay_row = {}
  for _, overlay in ipairs(entry.metadata.source_overlay or {}) do
    local row = overlay.range.start.row
    entry.source_overlay_row[row] = entry.source_overlay_row[row] or {}
    entry.source_overlay_row[row][#entry.source_overlay_row[row] + 1] = overlay
  end
  entry.gutter_row = {}
  for _, gutter in ipairs(entry.metadata.gutter or {}) do
    local row = gutter.position.row
    entry.gutter_row[row] = entry.gutter_row[row] or {}
    entry.gutter_row[row][#entry.gutter_row[row] + 1] = gutter
  end
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
end

function M.detach(session)
  sessions[session.buffer] = nil
end

return M
