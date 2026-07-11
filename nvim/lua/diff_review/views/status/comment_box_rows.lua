--- Owns compact comment boxes as real status-buffer rows so every comment provider shares
--- cursor navigation, resize behavior, identity, and edit promotion boundaries.

local layout = require("diff_review.render.comment_box")
local status_buffer = require("diff_review.views.status.status_buffer")

local M = {}

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

--- Emit a compact comment box as real status-buffer rows after its diff anchor.
---@param state table
---@param desc DiffReviewCommentDescriptor
---@param anchor_line integer
---@param style DiffReviewCommentBoxStyle?
function M.render_box(state, desc, anchor_line, style)
  if not (state and anchor_line and anchor_line > 0) then return end
  state.comment_box_record_by_anchor = state.comment_box_record_by_anchor or {}
  state.comment_box_record_by_id = state.comment_box_record_by_id or {}
  local record = state.comment_box_record_by_anchor[anchor_line]
  if not record then
    record = { anchor_line = anchor_line, items = {} }
    state.comment_box_record_by_anchor[anchor_line] = record
  end
  local width = display_columns(state.buf)
  local item = {
    desc = desc,
    style = style,
    anchor_line = anchor_line,
    start_line = #(state.lines or {}) + 1,
  }
  record.items[#record.items + 1] = item
  state.comment_box_render_width = width

  local anchor_entry = state.entries and state.entries[anchor_line] or nil
  local occurrence_id = tostring(anchor_entry and anchor_entry.id or anchor_line)
  local box_rows = layout.build_box_lines(desc, width, style)
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
  if desc.id then state.comment_box_record_by_id[desc.id] = item end
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
