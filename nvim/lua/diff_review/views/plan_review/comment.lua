local M = {}

local comment_box = require("diff_review.render.comment_box")
local comment_editor = require("diff_review.render.comment_editor")
local notifications = require("diff_review.infra.notifications")

local namespace = vim.api.nvim_create_namespace("DiffReviewPlanReviewComment")

---@class DiffReviewPlanAnnotation
---@field id integer
---@field source_line integer
---@field end_source_line integer
---@field body string
---@field focused boolean?

---@class DiffReviewPlanCommentRange
---@field annotation DiffReviewPlanAnnotation
---@field compact boolean
---@field first_row integer
---@field last_row integer
---@field header_mark integer?
---@field footer_mark integer?

---@class DiffReviewPlanCommentState
---@field buf integer
---@field win integer
---@field source_lines string[]
---@field source_provider? fun(width: integer): DiffReviewPlanSourceRow[]
---@field annotation_list DiffReviewPlanAnnotation[]
---@field source_mark { mark: integer, source_line: integer }[]
---@field range_list DiffReviewPlanCommentRange[]
---@field group integer
---@field next_id integer
---@field rendering boolean
---@field before_render? fun(buf: integer, win: integer?)
---@field after_render? fun(buf: integer, win: integer?, projection: DiffReviewPlanCommentProjection)

---@class DiffReviewPlanSourceRow
---@field id string
---@field text string
---@field source_line integer
---@field segments? table[]
---@field fold_id? string
---@field default_folded? boolean
---@field ancestor_ids? string[]

---@class DiffReviewPlanCommentOptions
---@field source_provider? fun(width: integer): DiffReviewPlanSourceRow[]
---@field before_render? fun(buf: integer, win: integer?)
---@field after_render? fun(buf: integer, win: integer?, projection: DiffReviewPlanCommentProjection)

---@type table<integer, DiffReviewPlanCommentState>
local state_by_buf = {}

---@param state DiffReviewPlanCommentState
---@return integer?
local function displayed_window(state)
  if state.win > 0 and vim.api.nvim_win_is_valid(state.win) and vim.api.nvim_win_get_buf(state.win) == state.buf then
    return state.win
  end
  local win = vim.fn.bufwinid(state.buf)
  if win and win > 0 and vim.api.nvim_win_is_valid(win) then
    state.win = win
    return win
  end
  return nil
end

---@param state DiffReviewPlanCommentState
---@param modifiable boolean
local function set_modifiable(state, modifiable)
  if vim.api.nvim_buf_is_valid(state.buf) then vim.bo[state.buf].modifiable = modifiable end
end

---@param state DiffReviewPlanCommentState
---@param mark integer?
---@return integer?
local function mark_row(state, mark)
  if not mark then return nil end
  local position = vim.api.nvim_buf_get_extmark_by_id(state.buf, namespace, mark, {})
  return #position > 0 and position[1] or nil
end

---@param state DiffReviewPlanCommentState
---@param range DiffReviewPlanCommentRange
---@return integer?, integer?
local function full_range_rows(state, range)
  return mark_row(state, range.header_mark), mark_row(state, range.footer_mark)
end

---@param state DiffReviewPlanCommentState
---@param range DiffReviewPlanCommentRange
local function sync_range_body(state, range)
  if range.compact then return end
  local header_row, footer_row = full_range_rows(state, range)
  if not header_row or not footer_row or footer_row <= header_row then return end
  local body_lines = vim.api.nvim_buf_get_lines(state.buf, header_row + 1, footer_row, false)
  range.annotation.body = comment_editor.normalize_body_text(table.concat(body_lines, "\n"))
end

---@param state DiffReviewPlanCommentState
local function sync_focused_body(state)
  for _, range in ipairs(state.range_list) do
    if range.annotation.focused then sync_range_body(state, range) end
  end
end

---@param state DiffReviewPlanCommentState
---@param row integer
---@return DiffReviewPlanAnnotation?, DiffReviewPlanCommentRange?
local function annotation_at_row(state, row)
  for _, range in ipairs(state.range_list) do
    if range.compact then
      if row >= range.first_row and row <= range.last_row then return range.annotation, range end
    else
      local header_row, footer_row = full_range_rows(state, range)
      if header_row and footer_row and row >= header_row and row <= footer_row then
        return range.annotation, range
      end
    end
  end
  return nil, nil
end

---@param state DiffReviewPlanCommentState
---@param row integer
---@return integer?
local function source_line_at_row(state, row)
  for _, record in ipairs(state.source_mark) do
    if mark_row(state, record.mark) == row then return record.source_line end
  end
  return nil
end

---@param state DiffReviewPlanCommentState
---@param first_row integer
---@param last_row integer
---@return integer?, integer?
local function source_range_at_rows(state, first_row, last_row)
  local start_source_line = nil
  local end_source_line = nil
  for row = math.min(first_row, last_row), math.max(first_row, last_row) do
    local source_line = source_line_at_row(state, row)
    if source_line then
      start_source_line = math.min(start_source_line or source_line, source_line)
      end_source_line = math.max(end_source_line or source_line, source_line)
    end
  end
  return start_source_line, end_source_line
end

---@param annotation DiffReviewPlanAnnotation
---@return string
local function annotation_line_label(annotation)
  if annotation.source_line == annotation.end_source_line then
    return "line " .. tostring(annotation.source_line)
  end
  return ("lines %d-%d"):format(annotation.source_line, annotation.end_source_line)
end

---@param segmented_line string[][]
---@return string, { start_col: integer, end_col: integer, hl: string }[]
local function flatten_segmented_line(segmented_line)
  local text = ""
  local highlight_list = {}
  for _, segment in ipairs(segmented_line) do
    local segment_text = segment[1]
    local start_col = #text
    text = text .. segment_text
    if segment[2] and segment_text ~= "" then
      highlight_list[#highlight_list + 1] = {
        start_col = start_col,
        end_col = #text,
        hl = segment[2],
      }
    end
  end
  return text, highlight_list
end

---@param state DiffReviewPlanCommentState
---@param annotation_id integer
---@return DiffReviewPlanAnnotation?
local function find_annotation(state, annotation_id)
  for _, annotation in ipairs(state.annotation_list) do
    if annotation.id == annotation_id then return annotation end
  end
  return nil
end

---@param state DiffReviewPlanCommentState
---@param annotation DiffReviewPlanAnnotation
local function remove_annotation(state, annotation)
  for index, candidate in ipairs(state.annotation_list) do
    if candidate == annotation then
      table.remove(state.annotation_list, index)
      return
    end
  end
end

---@class DiffReviewPlanCommentRenderTarget
---@field annotation_id integer?
---@field source_line integer?

---@class DiffReviewPlanCommentProjection
---@field line_list string[]
---@field source_row_by_line table<integer, integer>
---@field range_list DiffReviewPlanCommentRange[]
---@field compact_highlight_list { row: integer, start_col: integer, end_col: integer, hl: string }[]
---@field source_highlight_list { row: integer, start_col: integer, end_col: integer, hl: string }[]
---@field source_record_list { row: integer, source_line: integer }[]
---@field line_meta_list table[]

---@param state DiffReviewPlanCommentState
---@param width integer
---@return DiffReviewPlanCommentProjection
local function build_projection(state, width)
  local projection = {
    line_list = {},
    source_row_by_line = {},
    range_list = {},
    compact_highlight_list = {},
    source_highlight_list = {},
    source_record_list = {},
    line_meta_list = {},
  }
  local annotation_by_source = {}
  local source_count = math.max(1, #state.source_lines)
  for _, annotation in ipairs(state.annotation_list) do
    annotation.source_line = math.max(1, math.min(tonumber(annotation.source_line) or 1, source_count))
    annotation.end_source_line =
      math.max(annotation.source_line, math.min(tonumber(annotation.end_source_line) or annotation.source_line, source_count))
    annotation_by_source[annotation.end_source_line] = annotation_by_source[annotation.end_source_line] or {}
    table.insert(annotation_by_source[annotation.end_source_line], annotation)
  end

  local source_row_list = {}
  if state.source_provider then
    source_row_list = state.source_provider(width) or {}
  else
    for source_line = 1, source_count do
      source_row_list[#source_row_list + 1] = {
        id = ("plan:source:%d"):format(source_line),
        text = state.source_lines[source_line] or "",
        source_line = source_line,
        ancestor_ids = {},
      }
    end
  end

  for source_index, source_row in ipairs(source_row_list) do
    local source_line = math.max(1, math.min(tonumber(source_row.source_line) or 1, source_count))
    local row = #projection.line_list
    if projection.source_row_by_line[source_line] == nil then projection.source_row_by_line[source_line] = row end
    projection.line_list[#projection.line_list + 1] = source_row.text or ""
    projection.line_meta_list[#projection.line_list] = vim.deepcopy(source_row)
    projection.source_record_list[#projection.source_record_list + 1] = {
      row = row,
      source_line = source_line,
    }
    local text_offset = 0
    for _, segment in ipairs(source_row.segments or {}) do
      local segment_text = segment[1] or ""
      if segment[2] and segment_text ~= "" then
        projection.source_highlight_list[#projection.source_highlight_list + 1] = {
          row = row,
          start_col = text_offset,
          end_col = text_offset + #segment_text,
          hl = segment[2],
        }
      end
      text_offset = text_offset + #segment_text
    end
    local next_source_row = source_row_list[source_index + 1]
    local anchors_annotation = not next_source_row
      or tonumber(next_source_row.source_line) ~= source_line
    if anchors_annotation then
      for _, annotation in ipairs(annotation_by_source[source_line] or {}) do
      local first_row = #projection.line_list
      if annotation.focused then
        projection.line_list[#projection.line_list + 1] = comment_editor.rule_line(
          " Plan comment ",
          " " .. annotation_line_label(annotation) .. " ",
          width
        )
        projection.line_meta_list[#projection.line_list] = {
          ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
        }
        for _, body_line in ipairs(comment_editor.body_lines(annotation.body)) do
          projection.line_list[#projection.line_list + 1] = body_line
          projection.line_meta_list[#projection.line_list] = {
            ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
          }
        end
        projection.line_list[#projection.line_list + 1] = comment_editor.footer_line(width)
        projection.line_meta_list[#projection.line_list] = {
          ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
        }
      else
        local descriptor = {
          id = annotation.id,
          anchor = { line = source_line },
          heading = " Plan comment • " .. annotation_line_label(annotation) .. " ",
          body_lines = comment_editor.body_lines(annotation.body),
          readonly = false,
        }
        for _, segmented_line in ipairs(comment_box.build_box_lines(descriptor, width + 1)) do
          local text, highlight_list = flatten_segmented_line(segmented_line)
          local row = #projection.line_list
          projection.line_list[#projection.line_list + 1] = text
          projection.line_meta_list[#projection.line_list] = {
            ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
          }
          for _, highlight in ipairs(highlight_list) do
            highlight.row = row
            projection.compact_highlight_list[#projection.compact_highlight_list + 1] = highlight
          end
        end
      end
      projection.range_list[#projection.range_list + 1] = {
        annotation = annotation,
        compact = not annotation.focused,
        first_row = first_row,
        last_row = #projection.line_list - 1,
      }
      end
    end
  end
  return projection
end

---@param state DiffReviewPlanCommentState
---@param projection DiffReviewPlanCommentProjection
local function apply_projection(state, projection)
  vim.api.nvim_buf_set_lines(state.buf, 0, -1, false, projection.line_list)
  state.source_mark = {}
  state.range_list = projection.range_list
  for _, record in ipairs(projection.source_record_list) do
    state.source_mark[#state.source_mark + 1] = {
      source_line = record.source_line,
      mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, record.row, 0, {
        right_gravity = false,
      }),
    }
  end
  for _, highlight in ipairs(projection.source_highlight_list) do
    vim.api.nvim_buf_set_extmark(state.buf, namespace, highlight.row, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl,
    })
  end
  for _, highlight in ipairs(projection.compact_highlight_list) do
    vim.api.nvim_buf_set_extmark(state.buf, namespace, highlight.row, highlight.start_col, {
      end_col = highlight.end_col,
      hl_group = highlight.hl,
    })
  end
  for _, range in ipairs(projection.range_list) do
    if not range.compact then
      range.header_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.first_row, 0, {
        right_gravity = false,
        line_hl_group = "DiffReviewReviewCommentBoxHeader",
      })
      range.footer_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.last_row, 0, {
        right_gravity = true,
        line_hl_group = "DiffReviewReviewCommentBoxHeader",
      })
      for row = range.first_row + 1, range.last_row - 1 do
        vim.api.nvim_buf_set_extmark(state.buf, namespace, row, 0, {
          line_hl_group = "DiffReviewReviewCommentBox",
        })
      end
    end
  end
  vim.bo[state.buf].modified = false
end

---@param state DiffReviewPlanCommentState
---@param projection DiffReviewPlanCommentProjection
---@param target DiffReviewPlanCommentRenderTarget?
---@return integer?
local function target_row(state, projection, target)
  if target and target.annotation_id then
    local annotation = find_annotation(state, target.annotation_id)
    for _, range in ipairs(projection.range_list) do
      if range.annotation == annotation then
        return annotation.focused and range.first_row + 1 or range.first_row
      end
    end
  end
  return target and target.source_line and projection.source_row_by_line[target.source_line] or nil
end

---@param state DiffReviewPlanCommentState
---@param win integer?
local function sync_cursor_modifiable(state, win)
  local cursor_row = win and vim.api.nvim_win_get_cursor(win)[1] - 1 or -1
  local _, cursor_range = annotation_at_row(state, cursor_row)
  local header_row, footer_row = nil, nil
  if cursor_range and not cursor_range.compact then
    header_row, footer_row = full_range_rows(state, cursor_range)
  end
  set_modifiable(state, header_row ~= nil and cursor_row > header_row and cursor_row < footer_row)
end

---@param state DiffReviewPlanCommentState
---@param target DiffReviewPlanCommentRenderTarget?
local function render(state, target)
  if not vim.api.nvim_buf_is_valid(state.buf) then return end
  state.rendering = true
  set_modifiable(state, true)
  local win = displayed_window(state)
  if state.before_render then state.before_render(state.buf, win) end
  vim.api.nvim_buf_clear_namespace(state.buf, namespace, 0, -1)

  local width = comment_editor.display_width(win, state.buf)
  local projection = build_projection(state, width)
  apply_projection(state, projection)
  if state.after_render then state.after_render(state.buf, win, projection) end
  local requested_row = target_row(state, projection, target)
  if win and requested_row then vim.api.nvim_win_set_cursor(win, { requested_row + 1, 0 }) end
  sync_cursor_modifiable(state, win)
  state.rendering = false
end

---@param state DiffReviewPlanCommentState
local function handle_cursor_moved(state)
  if state.rendering or not vim.api.nvim_buf_is_valid(state.buf) then return end
  local win = displayed_window(state)
  if not win then return end
  sync_focused_body(state)
  local row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local annotation, range = annotation_at_row(state, row)
  if annotation then
    if not annotation.focused then
      for _, candidate in ipairs(state.annotation_list) do candidate.focused = candidate == annotation end
      render(state, { annotation_id = annotation.id })
      return
    end
    local header_row, footer_row = full_range_rows(state, range)
    set_modifiable(state, header_row ~= nil and row > header_row and row < footer_row)
    return
  end

  local source_line = source_line_at_row(state, row)
  local focused_annotation = nil
  for _, candidate in ipairs(state.annotation_list) do
    if candidate.focused then
      focused_annotation = candidate
      candidate.focused = false
    end
  end
  if focused_annotation then
    if vim.trim(focused_annotation.body) == "" then remove_annotation(state, focused_annotation) end
    render(state, { source_line = source_line or focused_annotation.end_source_line })
  else
    set_modifiable(state, false)
  end
end

---@param state DiffReviewPlanCommentState
local function handle_resize(state)
  if state.rendering or not displayed_window(state) then return end
  sync_focused_body(state)
  local row = vim.api.nvim_win_get_cursor(state.win)[1] - 1
  local annotation = annotation_at_row(state, row)
  local source_line = source_line_at_row(state, row)
  render(state, annotation and { annotation_id = annotation.id } or { source_line = source_line })
end

---@param state DiffReviewPlanCommentState
local function install_autocmd(state)
  local group = vim.api.nvim_create_augroup("DiffReviewPlanReviewComment" .. tostring(state.buf), { clear = true })
  state.group = group
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
    group = group,
    buffer = state.buf,
    callback = function() handle_cursor_moved(state) end,
  })
  vim.api.nvim_create_autocmd({ "TextChanged", "TextChangedI" }, {
    group = group,
    buffer = state.buf,
    callback = function()
      if not state.rendering then sync_focused_body(state) end
    end,
  })
  vim.api.nvim_create_autocmd({ "WinResized", "VimResized" }, {
    group = group,
    callback = function() handle_resize(state) end,
  })
  vim.api.nvim_create_autocmd("BufWipeout", {
    group = group,
    buffer = state.buf,
    callback = function()
      state_by_buf[state.buf] = nil
      pcall(vim.api.nvim_del_augroup_by_id, group)
    end,
  })
end

---@param buf integer
---@param win integer
---@param source_lines string[]
---@param annotation_list DiffReviewPlanAnnotation[]
---@param opts? DiffReviewPlanCommentOptions
---@return DiffReviewPlanCommentState
function M.attach(buf, win, source_lines, annotation_list, opts)
  opts = opts or {}
  local previous = state_by_buf[buf]
  if previous then pcall(vim.api.nvim_del_augroup_by_id, previous.group) end
  local next_id = 1
  for _, annotation in ipairs(annotation_list) do
    annotation.focused = false
    next_id = math.max(next_id, (tonumber(annotation.id) or 0) + 1)
  end
  local state = {
    buf = buf,
    win = win,
    source_lines = vim.deepcopy(source_lines),
    annotation_list = annotation_list,
    source_mark = {},
    range_list = {},
    group = 0,
    next_id = next_id,
    rendering = false,
    source_provider = opts.source_provider,
    before_render = opts.before_render,
    after_render = opts.after_render,
  }
  state_by_buf[buf] = state
  install_autocmd(state)
  render(state)
  return state
end

---@param buf integer
function M.add_at_cursor(buf)
  local state = state_by_buf[buf]
  if not state then return end
  local win = displayed_window(state)
  if not win then return end
  local mode = vim.fn.mode(1)
  local cursor_row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local first_row = cursor_row
  local last_row = cursor_row
  if mode == "v" or mode == "V" or mode == "\22" then
    first_row = vim.fn.getpos("v")[2] - 1
  end
  local source_line, end_source_line = source_range_at_rows(state, first_row, last_row)
  if not source_line or not end_source_line then
    notifications.error("Selected PlanReview rows have no semantic plan lines", "PlanReview")
    return
  end
  for _, annotation in ipairs(state.annotation_list) do annotation.focused = false end
  local annotation = {
    id = state.next_id,
    source_line = source_line,
    end_source_line = end_source_line,
    body = "",
    focused = true,
  }
  state.next_id = state.next_id + 1
  state.annotation_list[#state.annotation_list + 1] = annotation
  render(state, { annotation_id = annotation.id })
  vim.schedule(function()
    local win = displayed_window(state)
    if win and vim.api.nvim_get_current_win() == win then vim.cmd("startinsert") end
  end)
end

---@param buf integer
---@return integer?
function M.source_line_at_cursor(buf)
  local state = state_by_buf[buf]
  if not state then return nil end
  local win = displayed_window(state)
  if not win then return nil end
  local row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local annotation = annotation_at_row(state, row)
  return annotation and annotation.source_line or source_line_at_row(state, row)
end

---@param buf integer
---@param source_line integer
---@return integer?
function M.display_line_for_source_line(buf, source_line)
  local state = state_by_buf[buf]
  if not state then return nil end
  for _, record in ipairs(state.source_mark) do
    if record.source_line == source_line then
      local row = mark_row(state, record.mark)
      if row then return row + 1 end
    end
  end
  return nil
end

---@param buf integer
---@return table[]
function M.serialize(buf)
  local state = state_by_buf[buf]
  if not state then return {} end
  sync_focused_body(state)
  local result = {}
  for _, annotation in ipairs(state.annotation_list) do
    if vim.trim(annotation.body) ~= "" then
      result[#result + 1] = {
        start_line = annotation.source_line,
        end_line = annotation.end_source_line,
        body = annotation.body,
      }
    end
  end
  return result
end

---@param buf integer
function M.lock(buf)
  local state = state_by_buf[buf]
  if not state then return end
  sync_focused_body(state)
  local source_line = M.source_line_at_cursor(buf)
  for _, annotation in ipairs(state.annotation_list) do annotation.focused = false end
  render(state, { source_line = source_line })
  set_modifiable(state, false)
end

---@param buf integer
---@param restore_source boolean?
function M.detach(buf, restore_source)
  local state = state_by_buf[buf]
  if not state then return end
  sync_focused_body(state)
  for _, annotation in ipairs(state.annotation_list) do annotation.focused = false end
  pcall(vim.api.nvim_del_augroup_by_id, state.group)
  if vim.api.nvim_buf_is_valid(buf) then
    set_modifiable(state, true)
    vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
    if restore_source ~= false then vim.api.nvim_buf_set_lines(buf, 0, -1, false, state.source_lines) end
    vim.bo[buf].modified = false
    set_modifiable(state, false)
  end
  state_by_buf[buf] = nil
end

return M
