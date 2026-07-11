--- Shared comment rendering for every diff-with-comments surface (PR review, PR status,
--- PR overview, walkthrough). Interactive comments render by default as compact real buffer
--- rows so normal cursor movement can enter them. The shared dispatcher
--- (section_builder.emit_anchored_comments) morphs the focused comment or PR reply draft
--- into full-width editable rows. Readonly surfaces (walkthrough) reuse the same box-line
--- builder through virtual lines.

local status_buffer = require("diff_review.views.status.status_buffer")

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

--- Resolve the narrowest displayed text width so one box layout fits every window.
---@param buf integer?
---@return integer
local function display_columns(buf)
  local width
  if buf and vim.api.nvim_buf_is_valid(buf) then
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      if vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_buf(win) == buf then
        local wininfo = vim.fn.getwininfo(win)[1]
        local textoff = tonumber(wininfo and wininfo.textoff) or 0
        local text_width = math.max(1, vim.api.nvim_win_get_width(win) - textoff)
        width = width and math.min(width, text_width) or text_width
      end
    end
  end
  return width or tonumber(vim.o.columns) or 80
end

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

--- Emit a compact comment box as real status-buffer rows after its diff anchor.
--- Real rows let normal cursor movement enter the box and trigger inline editing.
---@param state table
---@param desc DiffReviewCommentDescriptor
---@param anchor_line integer 1-based buffer line of the diff row
---@param style DiffReviewCommentBoxStyle?
function M.render_box(state, desc, anchor_line, style)
  if not (state and anchor_line and anchor_line > 0) then return end
  state.comment_box_record_by_anchor = state.comment_box_record_by_anchor or {}
  local record = state.comment_box_record_by_anchor[anchor_line]
  if not record then
    record = { anchor_line = anchor_line, items = {} }
    state.comment_box_record_by_anchor[anchor_line] = record
  end
  local width = display_columns(state.buf)
  local item = {
    desc = desc,
    style = style,
    start_line = #(state.lines or {}) + 1,
  }
  record.items[#record.items + 1] = item
  state.comment_box_render_width = width

  local anchor_entry = state.entries and state.entries[anchor_line] or nil
  local occurrence_id = tostring(anchor_entry and anchor_entry.id or anchor_line)
  local box_rows = M.build_box_lines(desc, width, style)
  for box_row_index, segments in ipairs(box_rows) do
    local entry = {
      id = ("comment-box:%s:%s:%d:%d"):format(
        occurrence_id,
        tostring(desc.id or #record.items),
        #record.items,
        box_row_index
      ),
      kind = "comment_box",
      comment_box = desc,
      comment_box_source = desc.source,
      comment_box_index = desc.index,
      comment_box_anchor_line = anchor_line,
      comment_box_anchor_entry_id = anchor_entry and anchor_entry.id or nil,
      comment_box_boundary = box_row_index == 1 and "header"
        or (box_row_index == #box_rows and "footer" or "body"),
    }
    item.end_line = status_buffer.add_segment_line(state, segments, entry)
  end
  record.start_line = record.start_line or item.start_line
  record.end_line = item.end_line
end

--- Report whether real comment boxes need a layout render after a width change.
---@param state table
---@return boolean layout_changed
function M.refresh_buffer(state)
  local buf = state and state.buf or nil
  if not (buf and vim.api.nvim_buf_is_valid(buf)) then return false end
  if not next(state.comment_box_record_by_anchor or {}) then return false end
  local width = display_columns(buf)
  if state.comment_box_render_width == width then return false end
  state.comment_box_render_width = width
  return true
end

return M
