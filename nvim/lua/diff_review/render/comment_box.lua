--- Builds view-independent compact comment-box rows for every diff-with-comments surface.
--- Status-buffer ownership lives in views.status.comment_box_rows.

--- Box content the renderer reads; the full descriptor adds identity/anchor for dispatch.
---@class DiffReviewCommentBoxContent
---@field heading string preformatted heading text drawn in the box top border
---@field body_lines string[] newline-split body, never empty
---@field stale_note string? optional dim note appended below the body
---@field replies { heading: string, body_lines: string[] }[]? read-only reply sub-rows

---@class DiffReviewCommentDescriptor : DiffReviewCommentBoxContent
---@field id string? stable identity; drives focus dispatch (required for editable surfaces)
---@field anchor { file: string, side: string, line: integer }? required for editable surfaces
---@field readonly boolean? when true this comment never morphs to editable rows
---@field source any opaque back-pointer (the comment/step table)
---@field index integer? source's 1-based index (editable path needs it)
---@field reply_draft DiffReviewPrReplyDraft? pending reply rendered as the final merged block

---@class DiffReviewCommentBoxStyle
---@field border string border highlight group
---@field heading string heading highlight group
---@field body string body highlight group
---@field stale string staleness-note highlight group

---@class DiffReviewCommentBoxModule
local M = {}

M.box_max_inner_width = 84

---@type DiffReviewCommentBoxStyle
M.default_style = {
  border = "FloatBorder",
  heading = "DiffReviewWalkthroughItemTitle",
  body = "DiffReviewWalkthroughComment",
  stale = "DiffReviewWalkthroughStale",
}

--- Split the largest display-width prefix from text without breaking a character.
---@param value string
---@param width integer
---@return { prefix: string, remainder: string }
local function take_display_prefix(value, width)
  if width <= 0 or value == "" then return { prefix = "", remainder = value } end
  local prefix = ""
  local character_count = vim.fn.strchars(value)
  for character_index = 0, character_count - 1 do
    local character = vim.fn.strcharpart(value, character_index, 1)
    if vim.fn.strdisplaywidth(prefix .. character) > width then
      return { prefix = prefix, remainder = vim.fn.strcharpart(value, character_index) }
    end
    prefix = prefix .. character
  end
  return { prefix = prefix, remainder = "" }
end

--- Truncate display text to width while preserving a visible truncation marker.
---@param value string
---@param width integer
---@return string
local function truncate_display(value, width)
  value = tostring(value or "")
  if width <= 0 then return "" end
  if vim.fn.strdisplaywidth(value) <= width then return value end
  if width == 1 then return "…" end
  local split = take_display_prefix(value, width - 1)
  return split.prefix .. "…"
end

--- Wrap text to a display width, preserving blank lines and splitting overlong words.
---@param text string
---@param width integer
---@return string[]
local function wrap_text(text, width)
  width = math.max(1, width)
  local wrapped = {}
  for _, paragraph in ipairs(vim.split(tostring(text or ""), "\n", { plain = true })) do
    if vim.trim(paragraph) == "" then
      wrapped[#wrapped + 1] = ""
    else
      local line = ""
      for word in paragraph:gmatch("%S+") do
        if line ~= "" and vim.fn.strdisplaywidth(line .. " " .. word) <= width then
          line = line .. " " .. word
        else
          if line ~= "" then
            wrapped[#wrapped + 1] = line
            line = ""
          end
          while vim.fn.strdisplaywidth(word) > width do
            local split = take_display_prefix(word, width)
            if split.prefix == "" then break end
            wrapped[#wrapped + 1] = split.prefix
            word = split.remainder
          end
          line = word
        end
      end
      if line ~= "" then wrapped[#wrapped + 1] = line end
    end
  end
  if #wrapped == 0 then wrapped[1] = "" end
  return wrapped
end

M.wrap_text = wrap_text

--- Build segmented display rows for a compact comment box.
---@param desc DiffReviewCommentBoxContent
---@param win_width integer
---@param style DiffReviewCommentBoxStyle?
---@return table[] box_lines
function M.build_box_lines(desc, win_width, style)
  style = style or M.default_style
  local inner_width = math.max(4, math.min(M.box_max_inner_width, (tonumber(win_width) or 80) - 8))
  local content_width = math.max(1, inner_width - 2)

  ---@type { text: string, hl: string, divider?: boolean }[]
  local content = {}
  for _, line in ipairs(wrap_text(table.concat(desc.body_lines or {}, "\n"), content_width)) do
    content[#content + 1] = { text = line, hl = style.body }
  end
  for _, reply in ipairs(desc.replies or {}) do
    if reply.heading and reply.heading ~= "" then
      content[#content + 1] = {
        text = " " .. truncate_display(vim.trim(reply.heading), math.max(1, inner_width - 5)) .. " ",
        hl = style.heading,
        divider = true,
      }
    end
    for _, line in ipairs(wrap_text(table.concat(reply.body_lines or {}, "\n"), content_width)) do
      content[#content + 1] = { text = line, hl = style.body }
    end
  end
  if desc.stale_note then
    content[#content + 1] = { text = "", hl = style.body }
    for _, line in ipairs(wrap_text(desc.stale_note, content_width)) do
      content[#content + 1] = { text = line, hl = style.stale }
    end
  end

  local heading = truncate_display(tostring(desc.heading or ""), math.max(1, inner_width - 3))
  local heading_width = vim.fn.strdisplaywidth(heading)

  local pad = "  "
  local box_lines = {}
  box_lines[#box_lines + 1] = {
    { pad .. "╭─", style.border },
    { heading, style.heading },
    { ("─"):rep(math.max(inner_width - heading_width - 2, 0)), style.border },
    { "─╮", style.border },
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
      local fill = (" "):rep(math.max(inner_width - vim.fn.strdisplaywidth(row.text) - 2, 0))
      box_lines[#box_lines + 1] = {
        { pad .. "│ ", style.border },
        { row.text .. fill, row.hl },
        { " │", style.border },
      }
    end
  end
  box_lines[#box_lines + 1] = { { pad .. "╰" .. ("─"):rep(inner_width) .. "╯", style.border } }
  return box_lines
end

--- Build stacked compact boxes at one diff anchor in stable descriptor order.
---@param items { desc: DiffReviewCommentDescriptor, style: DiffReviewCommentBoxStyle? }[]
---@param win_width integer
---@return table[] box_lines
function M.build_box_group_lines(items, win_width)
  local box_lines = {}
  for _, item in ipairs(items or {}) do
    vim.list_extend(box_lines, M.build_box_lines(item.desc, win_width, item.style))
  end
  return box_lines
end

return M
