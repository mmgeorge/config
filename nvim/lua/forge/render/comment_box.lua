--- Builds view-independent compact comment-box rows for every diff-with-comments surface.
--- Status-buffer ownership lives in views.status.comment_box_rows.

local display_text = require("forge.render.display_text")

--- Box content the renderer reads; the full descriptor adds identity/anchor for dispatch.
---@class ForgeCommentBoxContent
---@field heading string preformatted heading text drawn in the box top border
---@field body_lines string[] newline-split body, never empty
---@field stale_note string? optional dim note appended below the body
---@field replies { heading: string, body_lines: string[] }[]? read-only reply sub-rows
---@field continuation boolean? Share the preceding message's bottom border as an internal divider.
---@field markdown boolean? Preserve fenced examples and describe Markdown content spans.

---@class ForgeCommentDescriptor : ForgeCommentBoxContent
---@field id string? stable identity; drives focus dispatch (required for editable surfaces)
---@field anchor { file: string, side: string, line: integer }? required for editable surfaces
---@field readonly boolean? when true this comment never morphs to editable rows
---@field source any opaque back-pointer (the comment/step table)
---@field index integer? source's 1-based index (editable path needs it)
---@field reply_draft ForgePrReplyDraft? pending reply rendered as the final merged block

---@class ForgeCommentBoxStyle
---@field border string border highlight group
---@field heading string heading highlight group
---@field body string body highlight group
---@field stale string staleness-note highlight group

---@class ForgeCommentBoxModule
local M = {}

---@class ForgeCommentBoxLines : table[]
---@field reply_start_row? integer
---@field reply_start_list? integer[]
---@field markdown_rows table<integer, { first_col: integer, end_col: integer, starts_body: boolean }>

M.box_max_inner_width = 84

---@type ForgeCommentBoxStyle
M.default_style = {
  border = "FloatBorder",
  heading = "ForgeWalkthroughItemTitle",
  body = "ForgeWalkthroughComment",
  stale = "ForgeWalkthroughStale",
}

--- Truncate display text to width while preserving a visible truncation marker.
---@param value string
---@param width integer
---@return string
local function truncate_display(value, width)
  value = tostring(value or "")
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(value) <= width then return value end
  if width == 1 then return "…" end
  local split = display_text.take_prefix(value, width - 1)
  return split.prefix .. "…"
end

--- Wrap prose while preserving the authored lines and spacing in fenced examples.
---@param lines string[]
---@param width integer
---@param markdown boolean?
---@return string[]
function M.wrap_body(lines, width, markdown)
  if not markdown then return display_text.wrap(table.concat(lines, "\n"), width) end
  if #lines == 0 then return { "" } end
  local wrapped, fence = {}, nil
  for _, line in ipairs(lines) do
    local marker, rest = line:match("^ ? ? ?(```+)(.*)$")
    if not marker then marker, rest = line:match("^ ? ? ?(~~~+)(.*)$") end
    if fence then
      wrapped[#wrapped + 1] = line
      if marker and marker:sub(1, 1) == fence:sub(1, 1) and #marker >= #fence and vim.trim(rest) == "" then
        fence = nil
      end
    elseif marker and (marker:sub(1, 1) == "~" or not rest:find("`", 1, true)) then
      wrapped[#wrapped + 1] = line
      fence = marker
    elseif vim.fn.strdisplaywidth(line, 4) <= width then
      wrapped[#wrapped + 1] = line
    else
      vim.list_extend(wrapped, display_text.wrap(line, width))
    end
  end
  return wrapped
end

--- Builds styled row segment lists representing a framed comment box.
--- Wraps content text to the computed inner column budget.
---@param desc ForgeCommentBoxContent Comment heading, body, and reply metadata.
---@param win_width integer Total available window column width.
---@param style ForgeCommentBoxStyle? Optional custom highlight group styling.
---@return ForgeCommentBoxLines box_lines Rendered rows and reply boundaries.
function M.build_box_lines(desc, win_width, style)
  style = style or M.default_style
  local pad = "  "
  local body_prefix = pad .. "│ "
  local body_column = desc.markdown and vim.fn.strdisplaywidth(body_prefix) or 0
  local inner_width = math.max(4, math.min(M.box_max_inner_width, (tonumber(win_width) or 80) - 8))
  local content_width = math.max(1, inner_width - 2)

  ---@type { text: string, hl: string, divider?: boolean, markdown?: boolean, starts_body?: boolean }[]
  local content = {}
  local function append_body(lines)
    for index, line in ipairs(M.wrap_body(lines, content_width, desc.markdown)) do
      content[#content + 1] = { text = line, hl = style.body, markdown = desc.markdown, starts_body = index == 1 }
      if desc.markdown then inner_width = math.max(inner_width, vim.fn.strdisplaywidth(line, body_column) + 2) end
    end
  end
  append_body(desc.body_lines or {})
  local reply_content_start
  local reply_start_list = {}
  for _, reply in ipairs(desc.replies or {}) do
    reply_content_start = reply_content_start or #content + 1
    reply_start_list[#reply_start_list + 1] = #content + 1
    if reply.heading and reply.heading ~= "" then
      content[#content + 1] = {
        text = " " .. truncate_display(vim.trim(reply.heading), math.max(1, inner_width - 5)) .. " ",
        hl = style.heading,
        divider = true,
      }
    end
    append_body(reply.body_lines or {})
  end
  if desc.stale_note then
    content[#content + 1] = { text = "", hl = style.body }
    for _, line in ipairs(display_text.wrap(desc.stale_note, content_width)) do
      content[#content + 1] = { text = line, hl = style.stale }
    end
  end

  local heading = truncate_display(tostring(desc.heading or ""), math.max(1, inner_width - 3))
  local heading_width = vim.fn.strdisplaywidth(heading)

  local box_lines = {}
  box_lines.markdown_rows = {}
  box_lines[#box_lines + 1] = {
    { pad .. (desc.continuation and "├─" or "╭─"), style.border },
    { heading, style.heading },
    { ("─"):rep(math.max(inner_width - heading_width - 2, 0)), style.border },
    { desc.continuation and "─┤" or "─╮", style.border },
  }
  for _, row in ipairs(content) do
    if row.divider then
      local row_width = vim.fn.strdisplaywidth(row.text)
      box_lines[#box_lines + 1] = {
        { pad .. "├─", style.border },
        { row.text, row.hl },
        { ("─"):rep(math.max(inner_width - row_width - 2, 0)), style.border },
        { "─┤", style.border },
      }
    else
      local row_width = vim.fn.strdisplaywidth(row.text, body_column)
      local fill = (" "):rep(math.max(inner_width - row_width - 2, 0))
      if row.markdown then
        box_lines.markdown_rows[#box_lines + 1] = {
          first_col = #body_prefix, end_col = #body_prefix + #row.text, starts_body = row.starts_body,
        }
      end
      box_lines[#box_lines + 1] = {
        { body_prefix, style.border },
        { row.text .. fill, row.hl },
        { " │", style.border },
      }
    end
  end
  box_lines[#box_lines + 1] = { { pad .. "╰" .. ("─"):rep(inner_width) .. "╯", style.border } }
  box_lines.reply_start_row = reply_content_start
  box_lines.reply_start_list = reply_start_list
  return box_lines
end

--- Builds stacked compact comment box rows for multiple descriptors anchored to the same diff line.
---@param items { desc: ForgeCommentDescriptor, style: ForgeCommentBoxStyle? }[] Array of comment descriptors and optional styles.
---@param win_width integer Total available window column width.
---@return table[] box_lines Combined array of row segment arrays.
function M.build_box_group_lines(items, win_width)
  local box_lines = {}
  for _, item in ipairs(items or {}) do
    vim.list_extend(box_lines, M.build_box_lines(item.desc, win_width, item.style))
  end
  return box_lines
end

return M
