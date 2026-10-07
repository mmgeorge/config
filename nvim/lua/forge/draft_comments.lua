local M = {}

local comment_box = require("forge.render.comment_box")
local comment_editor = require("forge.render.comment_editor")
local notifications = require("forge.infra.notifications")
local editable = require("forge.editable")

local namespace = vim.api.nvim_create_namespace("ForgeDraftComments")

---@class ForgeDraftComment
---@field id integer|string
---@field parent_id string? Earlier editable message in the same conversation.
---@field source_line integer
---@field end_source_line integer
---@field body string
---@field focused boolean?
---@field new boolean?
---@field readonly boolean?
---@field kind string? Opaque workflow tag retained in explicit captures.
---@field heading string? Per-comment heading overrides the view default.
---@field replies {heading: string, body_lines: string[]}[]?
---@field replies_body string? Restricts replies to the parent body they answer.

---@class ForgeDraftCommentCapture
---@field id string
---@field source {start_line: integer, end_line: integer, body: string}
---@field kind string?

---@class ForgeDraftCommentRange
---@field reply_index integer?
---@field reply_start_list integer[]?
---@field annotation ForgeDraftComment
---@field compact boolean
---@field first_row integer
---@field last_row integer
---@field header_mark integer?
---@field footer_mark integer?
---@field reply_mark integer?
---@field reply_start_row integer?
---@field readonly boolean?

---@class ForgeDraftCommentState
---@field buf integer
---@field win integer
---@field source_lines string[]
---@field source_provider? fun(width: integer): ForgeDraftSourceRow[]
---@field annotation_list ForgeDraftComment[]
---@field source_mark { mark: integer, source_line: integer }[]
---@field range_list ForgeDraftCommentRange[]
---@field group integer
---@field next_id integer
---@field rendering boolean
---@field generation integer?
---@field namespace integer
---@field readonly boolean
---@field heading string
---@field source_label fun(annotation: ForgeDraftComment): string
---@field editable_source? fun(row: integer): boolean
---@field baseline ForgeDraftCommentCapture[]
---@field before_render? fun(buf: integer, win: integer?)
---@field after_render? fun(buf: integer, win: integer?, projection: ForgeDraftCommentProjection)
---@field guard? table
---@field guard_source boolean

---@class ForgeDraftSourceRow
---@field id string
---@field text string
---@field source_line integer
---@field segments? table[]
---@field fold_id? string
---@field default_folded? boolean
---@field ancestor_ids? string[]
---@field annotation_anchor? boolean

---@class ForgeDraftCommentOptions
---@field readonly? boolean
---@field heading? string
---@field source_label? fun(annotation: ForgeDraftComment): string
---@field editable_source? fun(row: integer): boolean
---@field source_provider? fun(width: integer): ForgeDraftSourceRow[]
---@field before_render? fun(buf: integer, win: integer?)
---@field after_render? fun(buf: integer, win: integer?, projection: ForgeDraftCommentProjection)
---@field guard_source? boolean

---@type table<integer, ForgeDraftCommentState>
local state_by_buf = {}

---@param state ForgeDraftCommentState
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

---@param state ForgeDraftCommentState
---@param modifiable boolean
local function set_modifiable(state, modifiable)
  if vim.api.nvim_buf_is_valid(state.buf) then vim.bo[state.buf].modifiable = modifiable end
end

---@param state ForgeDraftCommentState
---@param mark integer?
---@return integer?
local function mark_row(state, mark)
  if not mark then return nil end
  local position = vim.api.nvim_buf_get_extmark_by_id(state.buf, namespace, mark, {})
  return #position > 0 and position[1] or nil
end

---@param state ForgeDraftCommentState
---@param range ForgeDraftCommentRange
---@return integer?, integer?
local function full_range_rows(state, range)
  return mark_row(state, range.header_mark), mark_row(state, range.footer_mark)
end

---@param state ForgeDraftCommentState
---@param range ForgeDraftCommentRange
local function sync_range_body(state, range)
  if range.compact or range.readonly then return end
  local header_row, footer_row = full_range_rows(state, range)
  if not header_row or not footer_row or footer_row <= header_row then return end
  local body_lines = vim.api.nvim_buf_get_lines(state.buf, header_row + 1, footer_row, false)
  range.annotation.body = table.concat(body_lines, "\n")
end

---@param state ForgeDraftCommentState
local function sync_focused_body(state)
  if state.guard and state.guard.native and state.guard.native.rejecting then return end
  for _, range in ipairs(state.range_list) do
    if range.annotation.focused then sync_range_body(state, range) end
  end
end

---@param state ForgeDraftCommentState
---@param row integer
---@return ForgeDraftComment?, ForgeDraftCommentRange?
---@return boolean? readonly_reply
local function annotation_at_row(state, row)
  for index = #state.range_list, 1, -1 do
    local range = state.range_list[index]
    local header_row, footer_row = full_range_rows(state, range)
    if header_row and footer_row and row >= header_row and row <= footer_row then
      local reply_row = range.reply_mark and mark_row(state, range.reply_mark)
      return range.annotation, range, range.readonly or reply_row and row >= reply_row or false
    end
  end
  return nil, nil
end

---@param annotation ForgeDraftComment
---@return ForgeDraftCommentCapture
local function capture_annotation(annotation)
  return { id = tostring(annotation.id), kind = annotation.kind, parent_id = annotation.parent_id, source = {
    start_line = annotation.source_line, end_line = annotation.end_source_line, body = annotation.body } }
end

---@param state ForgeDraftCommentState
---@param row integer
---@return integer?
local function source_line_at_row(state, row, annotation_anchor)
  for _, record in ipairs(state.source_mark) do
    if mark_row(state, record.mark) == row
      and (not annotation_anchor or record.annotation_anchor ~= false) then return record.source_line end
  end
  return nil
end

---@param state ForgeDraftCommentState
---@param first_row integer
---@param last_row integer
---@return integer?, integer?
local function source_range_at_rows(state, first_row, last_row)
  local start_source_line = nil
  local end_source_line = nil
  for row = math.min(first_row, last_row), math.max(first_row, last_row) do
    local source_line = source_line_at_row(state, row, true)
    if source_line then
      start_source_line = math.min(start_source_line or source_line, source_line)
      end_source_line = math.max(end_source_line or source_line, source_line)
    end
  end
  return start_source_line, end_source_line
end

---@param annotation ForgeDraftComment
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

---@param state ForgeDraftCommentState
---@param annotation_id integer
---@return ForgeDraftComment?
local function find_annotation(state, annotation_id)
  for _, annotation in ipairs(state.annotation_list) do
    if tostring(annotation.id) == tostring(annotation_id) then return annotation end
  end
  return nil
end

--- Resolve the shared focus owner for a conversation without changing source identity.
local function thread_root(state, annotation)
  for _ = 1, #state.annotation_list do
    local parent = annotation.parent_id and find_annotation(state, annotation.parent_id)
    if not parent then return annotation end
    annotation = parent
  end
  error("cyclic comment thread")
end

local function focus_thread(state, annotation)
  local root = thread_root(state, annotation)
  for _, candidate in ipairs(state.annotation_list) do
    candidate.focused = thread_root(state, candidate) == root
  end
end

---@param state ForgeDraftCommentState
---@param annotation ForgeDraftComment
local function remove_annotation(state, annotation)
  for _, candidate in ipairs(state.annotation_list) do
    if candidate.parent_id == tostring(annotation.id) then
      notifications.error("Remove follow-ups before deleting their parent", "Forge comments")
      return
    end
  end
  for index, candidate in ipairs(state.annotation_list) do
    if candidate == annotation then
      table.remove(state.annotation_list, index)
      return
    end
  end
end

---@class ForgeDraftCommentRenderTarget
---@field annotation_id integer?
---@field source_line integer?

---@class ForgeDraftCommentProjection
---@field line_list string[]
---@field source_row_by_line table<integer, integer>
---@field range_list ForgeDraftCommentRange[]
---@field compact_highlight_list { row: integer, start_col: integer, end_col: integer, hl: string }[]
---@field source_highlight_list { row: integer, start_col: integer, end_col: integer, hl: string }[]
---@field source_record_list { row: integer, source_line: integer }[]
---@field line_meta_list table[]

---@param state ForgeDraftCommentState
---@param width integer
---@return ForgeDraftCommentProjection
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
  local source_row_list = {}
  if state.source_provider then
    source_row_list = state.source_provider(width) or {}
    for _, source_row in ipairs(source_row_list) do
      source_count = math.max(source_count, tonumber(source_row.source_line) or 1)
    end
  else
    for source_line = 1, source_count do
      source_row_list[#source_row_list + 1] = {
        id = ("source:%d"):format(source_line),
        text = state.source_lines[source_line] or "",
        source_line = source_line,
        ancestor_ids = {},
      }
    end
  end
  local annotation_row_by_source = {}
  for source_index, source_row in ipairs(source_row_list) do
    if source_row.annotation_anchor ~= false then
      annotation_row_by_source[tonumber(source_row.source_line) or source_index] = source_index
    end
  end
  local ordered = {}
  for _, root in ipairs(state.annotation_list) do
    if not root.parent_id then
      for _, candidate in ipairs(state.annotation_list) do
        if thread_root(state, candidate) == root then ordered[#ordered + 1] = candidate end
      end
    end
  end
  for _, annotation in ipairs(ordered) do
    annotation.source_line = math.max(1, math.min(tonumber(annotation.source_line) or 1, source_count))
    annotation.end_source_line =
      math.max(annotation.source_line, math.min(tonumber(annotation.end_source_line) or annotation.source_line, source_count))
    annotation_by_source[annotation.end_source_line] = annotation_by_source[annotation.end_source_line] or {}
    table.insert(annotation_by_source[annotation.end_source_line], annotation)
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
      source_index = source_index,
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
    local anchors_annotation = annotation_row_by_source[source_line] == source_index
    if anchors_annotation then
      for _, annotation in ipairs(annotation_by_source[source_line] or {}) do
      local first_row = #projection.line_list
      local heading = annotation.heading or state.heading
      local replies = annotation.replies
      if annotation.replies_body and annotation.replies_body ~= annotation.body then replies = nil end
      local reply_start_row, reply_start_list
      local previous = projection.range_list[#projection.range_list]
      local shared_divider = annotation.parent_id and previous
        and previous.compact == not annotation.focused
        and thread_root(state, previous.annotation) == thread_root(state, annotation)
      if shared_divider then first_row = #projection.line_list - 1 end
      if annotation.focused then
        local heading_row = shared_divider and #projection.line_list or #projection.line_list + 1
        projection.line_list[heading_row] = comment_editor.rule_line(
          " " .. heading .. " ",
          " " .. state.source_label(annotation) .. " ",
          width
        )
        projection.line_meta_list[heading_row] = {
          ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
        }
        for _, body_line in ipairs(vim.split(annotation.body, "\n", { plain = true })) do
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
          heading = " " .. heading .. " • " .. state.source_label(annotation) .. " ",
          body_lines = vim.split(annotation.body, "\n", { plain = true }),
          readonly = annotation.readonly == true,
          replies = replies,
          continuation = shared_divider,
        }
        local box_lines = comment_box.build_box_lines(descriptor, width + 1)
        reply_start_row = box_lines.reply_start_row and first_row + box_lines.reply_start_row
        reply_start_list = {}
        for _, reply_row in ipairs(box_lines.reply_start_list or {}) do
          reply_start_list[#reply_start_list + 1] = first_row + reply_row
        end
        if shared_divider then
          for index = #projection.compact_highlight_list, 1, -1 do
            if projection.compact_highlight_list[index].row == first_row then
              table.remove(projection.compact_highlight_list, index)
            end
          end
        end
        for index, segmented_line in ipairs(box_lines) do
          local text, highlight_list = flatten_segmented_line(segmented_line)
          local row = shared_divider and index == 1 and first_row or #projection.line_list
          projection.line_list[row + 1] = text
          projection.line_meta_list[row + 1] = {
            ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
          }
          for _, highlight in ipairs(highlight_list) do
            projection.compact_highlight_list[#projection.compact_highlight_list + 1] = {
              row = row,
              start_col = highlight.start_col,
              end_col = highlight.end_col,
              hl = highlight.hl,
            }
          end
        end
      end
      projection.range_list[#projection.range_list + 1] = {
        annotation = annotation,
        compact = not annotation.focused,
        first_row = first_row,
        last_row = #projection.line_list - 1,
        reply_start_row = reply_start_row,
        reply_start_list = reply_start_list,
      }
      if annotation.focused then
        for reply_index, reply in ipairs(replies or {}) do
          local first_reply_row = #projection.line_list - 1
          projection.line_list[#projection.line_list] = comment_editor.rule_line(" " .. reply.heading .. " ", "", width)
          local reply_lines = comment_box.wrap_text(table.concat(reply.body_lines or {}, "\n"), width)
          reply_lines[#reply_lines + 1] = comment_editor.footer_line(width)
          for _, line in ipairs(reply_lines) do
            projection.line_list[#projection.line_list + 1] = line
            projection.line_meta_list[#projection.line_list] = { ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}) }
          end
          projection.range_list[#projection.range_list + 1] = { annotation = annotation, compact = false,
            readonly = true, reply_index = reply_index, first_row = first_reply_row, last_row = #projection.line_list - 1 }
        end
      end
      end
    end
  end
  return projection
end

---@param state ForgeDraftCommentState
---@param projection ForgeDraftCommentProjection
local function apply_projection(state, projection)
  local previous = vim.api.nvim_buf_get_lines(state.buf, 0, -1, false)
  local change_list = vim.diff(table.concat(previous, "\n") .. "\n",
    table.concat(projection.line_list, "\n") .. "\n", { result_type = "indices", algorithm = "histogram" })
  for index = #change_list, 1, -1 do
    local change = change_list[index]
    local start = change[2] == 0 and change[1] or change[1] - 1
    local replacement = {}
    for row = change[3], change[3] + change[4] - 1 do replacement[#replacement + 1] = projection.line_list[row] end
    vim.api.nvim_buf_set_lines(state.buf, start, start + change[2], false, replacement)
  end
  state.source_mark = {}
  state.range_list = projection.range_list
  for _, record in ipairs(projection.source_record_list) do
    state.source_mark[#state.source_mark + 1] = {
      source_line = record.source_line,
      source_index = record.source_index,
      annotation_anchor = projection.line_meta_list[record.row + 1].annotation_anchor,
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
    range.header_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.first_row, 0, { right_gravity = false })
    range.footer_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.last_row, 0, { right_gravity = true })
    if range.reply_start_row then range.reply_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.reply_start_row, 0, { right_gravity = false }) end
    if not range.compact then
      range.header_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.first_row, 0, {
        right_gravity = false,
        line_hl_group = "ForgeReviewCommentBoxHeader",
      })
      range.footer_mark = vim.api.nvim_buf_set_extmark(state.buf, namespace, range.last_row, 0, {
        right_gravity = true,
        line_hl_group = "ForgeReviewCommentBoxHeader",
      })
      for row = range.first_row + 1, range.last_row - 1 do
        vim.api.nvim_buf_set_extmark(state.buf, namespace, row, 0, {
          line_hl_group = "ForgeReviewCommentBox",
        })
      end
    end
  end
  local captured = {}
  for _, annotation in ipairs(state.annotation_list) do
    captured[#captured + 1] = capture_annotation(annotation)
  end
  vim.bo[state.buf].modified = not vim.deep_equal(captured, state.baseline)
  state.generation = (state.generation or 0) + 1
end

---@param state ForgeDraftCommentState
---@param projection ForgeDraftCommentProjection
---@param target ForgeDraftCommentRenderTarget?
---@return integer?
local function target_row(state, projection, target)
  if target and target.annotation_id then
    local annotation = find_annotation(state, target.annotation_id)
    if annotation then
      for _, range in ipairs(projection.range_list) do
        if range.annotation == annotation and range.reply_index == target.reply_index then
          return annotation.focused and range.first_row + 1 or range.first_row
        end
      end
    end
  end
  return target and target.source_line and projection.source_row_by_line[target.source_line] or nil
end

---@param state ForgeDraftCommentState
---@param win integer?
local function sync_cursor_modifiable(state, win)
  local cursor_row = win and vim.api.nvim_win_get_cursor(win)[1] - 1 or -1
  local _, cursor_range, reply = annotation_at_row(state, cursor_row)
  local header_row, footer_row = nil, nil
  if cursor_range and not cursor_range.compact then
    header_row, footer_row = full_range_rows(state, cursor_range)
  end
  local allowed = not state.readonly and cursor_range and not reply and not cursor_range.annotation.readonly
    and header_row ~= nil and cursor_row > header_row and cursor_row < footer_row
  if state.editable_source then allowed = allowed or state.editable_source(cursor_row) end
  set_modifiable(state, allowed == true)
end

---@param state ForgeDraftCommentState
---@param target ForgeDraftCommentRenderTarget?
local function render(state, target)
  if not vim.api.nvim_buf_is_valid(state.buf) then return end
  state.rendering = true
  if state.guard and state.guard.native then editable.applying(state.guard, true) end
  set_modifiable(state, true)
  local win = displayed_window(state)
  if state.before_render then state.before_render(state.buf, win) end
  vim.api.nvim_buf_clear_namespace(state.buf, namespace, 0, -1)
  local width = comment_editor.display_width(win, state.buf)
  local projection = build_projection(state, width)
  apply_projection(state, projection)
  if state.guard_source then
    state.guard = state.guard or editable.new("plan-comment:" .. state.buf)
    local anchor = {}
    for _, range in ipairs(projection.range_list) do
      if not range.compact and not range.readonly and not range.annotation.readonly then
        local region = tostring(range.annotation.id)
        if not state.guard.region[region] then editable.register(state.guard, region, 0) end
        anchor[region] = { start = { row = range.first_row + 1, column = 0 },
          finish = { row = range.last_row, column = 0 } }
      end
    end
    for region in pairs(state.guard.region) do
      if not anchor[region] then state.guard.region[region] = nil end
    end
    if state.guard.native then editable.reanchor(state.guard, anchor)
    else editable.attach(state.guard, state.buf, anchor, {
      notice = function(message) notifications.error(message, "Forge comments") end,
      restored = function()
        if state.guard.native and state.guard.native.rejecting then render(state) end
      end,
    }) end
  end
  if state.after_render then state.after_render(state.buf, win, projection) end
  local requested_row = target_row(state, projection, target)
  if win and requested_row then vim.api.nvim_win_set_cursor(win, { requested_row + 1, 0 }) end
  sync_cursor_modifiable(state, win)
  state.rendering = false
  if state.guard and state.guard.native then editable.applying(state.guard, false) end
end

---@param state ForgeDraftCommentState
local function handle_cursor_moved(state)
  if state.rendering or not vim.api.nvim_buf_is_valid(state.buf)
    or state.guard and state.guard.native and state.guard.native.rejecting then return end
  if state.readonly then set_modifiable(state, false) return end
  local win = displayed_window(state)
  if not win then return end
  sync_focused_body(state)
  local row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local annotation, range, reply = annotation_at_row(state, row)
  if annotation then
    if annotation.readonly then sync_cursor_modifiable(state, win) return end
    if not annotation.focused then
      local reply_index = range.reply_index
      if reply then
        for index, reply_row in ipairs(range.reply_start_list or {}) do
          if row >= reply_row then reply_index = index end
        end
      end
      focus_thread(state, annotation)
      render(state, { annotation_id = annotation.id, reply_index = reply_index })
      return
    end
    if reply then sync_cursor_modifiable(state, win) return end
    if range then
      local header_row, footer_row = full_range_rows(state, range)
      set_modifiable(state, header_row ~= nil and row > header_row and row < footer_row)
    end
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
    if focused_annotation.new and vim.trim(focused_annotation.body) == "" then remove_annotation(state, focused_annotation) end
    render(state, { source_line = source_line or focused_annotation.end_source_line })
  else
    sync_cursor_modifiable(state, win)
  end
end

---@param state ForgeDraftCommentState
local function handle_resize(state)
  if state.rendering or not displayed_window(state) then return end
  sync_focused_body(state)
  local row = vim.api.nvim_win_get_cursor(state.win)[1] - 1
  local annotation = annotation_at_row(state, row)
  local source_line = source_line_at_row(state, row)
  render(state, annotation and { annotation_id = annotation.id } or { source_line = source_line })
end

---@param state ForgeDraftCommentState
local function install_autocmd(state)
  local group = vim.api.nvim_create_augroup("ForgeDraftComment" .. tostring(state.buf), { clear = true })
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
      if not state.rendering then
        sync_focused_body(state)
        vim.bo[state.buf].modified = not vim.deep_equal(M.capture(state.buf), state.baseline)
      end
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
---@param annotation_list ForgeDraftComment[]
---@param opts? ForgeDraftCommentOptions
---@return ForgeDraftCommentState
function M.attach(buf, win, source_lines, annotation_list, opts)
  opts = opts or {}
  local previous = state_by_buf[buf]
  if previous then
    pcall(vim.api.nvim_del_augroup_by_id, previous.group)
    if previous.guard then editable.detach(previous.guard) end
  end
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
    namespace = namespace,
    readonly = opts.readonly == true,
    heading = opts.heading or "Plan comment",
    source_label = opts.source_label or annotation_line_label,
    editable_source = opts.editable_source,
    baseline = vim.deepcopy(opts.baseline or {}),
    source_provider = opts.source_provider,
    before_render = opts.before_render,
    after_render = opts.after_render,
    guard_source = opts.guard_source == true,
  }
  state_by_buf[buf] = state
  for _, annotation in ipairs(opts.baseline and {} or annotation_list) do
    state.baseline[#state.baseline + 1] = capture_annotation(annotation)
  end
  install_autocmd(state)
  render(state)
  return state
end

---@param buf integer
---@return ForgeDraftCommentCapture[]
function M.capture(buf)
  local state = assert(state_by_buf[buf], "comment view is not attached")
  sync_focused_body(state)
  local captured = {}
  for _, annotation in ipairs(state.annotation_list) do
    captured[#captured + 1] = capture_annotation(annotation)
  end
  return captured
end

---@param buf integer
---@param captured ForgeDraftCommentCapture[]
function M.saved(buf, captured)
  local state = assert(state_by_buf[buf], "comment view is not attached")
  state.baseline = vim.deepcopy(captured)
  for _, annotation in ipairs(state.annotation_list) do
    for _, saved in ipairs(captured) do
      if tostring(annotation.id) == saved.id then annotation.new = nil break end
    end
  end
  vim.bo[buf].modified = not vim.deep_equal(M.capture(buf), state.baseline)
  if state.guard and state.guard.native then state.guard.native.modified = vim.bo[buf].modified end
end

---@param buf integer
function M.delete_at_cursor(buf)
  local state = state_by_buf[buf]
  if not state or state.readonly then return end
  sync_focused_body(state)
  local row = vim.api.nvim_win_get_cursor(state.win)[1] - 1
  local annotation, _, reply = annotation_at_row(state, row)
  if not annotation or annotation.readonly or reply then return end
  local source_line = annotation.end_source_line
  remove_annotation(state, annotation)
  render(state, { source_line = source_line })
end

---@param buf integer
---@param id integer|string
---@return boolean
function M.focus(buf, id)
  local state = state_by_buf[buf]
  local annotation = state and find_annotation(state, id)
  if not annotation or state.readonly or annotation.readonly then return false end
  sync_focused_body(state)
  focus_thread(state, annotation)
  render(state, { annotation_id = id })
  return true
end

---@param buf integer
---@param annotation ForgeDraftComment
---@param start_insert? boolean
---@return ForgeDraftComment?
function M.add(buf, annotation, start_insert)
  local state = state_by_buf[buf]
  if not state or state.readonly then return nil end
  assert(annotation.id ~= nil and not find_annotation(state, annotation.id), "comment identity is already present")
  assert(type(annotation.body) == "string", "comment body is required")
  sync_focused_body(state)
  local win = displayed_window(state)
  if not win then return nil end
  for _, candidate in ipairs(state.annotation_list) do candidate.focused = false end
  annotation.focused, annotation.new = true, true
  state.annotation_list[#state.annotation_list + 1] = annotation
  focus_thread(state, annotation)
  render(state, { annotation_id = annotation.id })
  if start_insert ~= false then
    vim.schedule(function()
      local displayed = displayed_window(state)
      if displayed and vim.api.nvim_get_current_win() == displayed then vim.cmd("startinsert") end
    end)
  end
  return annotation
end

---@param buf integer
---@param start_insert? boolean
---@param properties? {kind?: string, heading?: string}
function M.add_at_cursor(buf, start_insert, properties)
  local state = state_by_buf[buf]
  if not state or state.readonly then return end
  sync_focused_body(state)
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
  local parent = first_row == last_row and annotation_at_row(state, cursor_row) or nil
  if parent then source_line, end_source_line = parent.source_line, parent.end_source_line end
  if not source_line or not end_source_line then
    notifications.error("Selected rows have no source identity", "Forge comments")
    return
  end
  local annotation = {
    id = state.next_id,
    source_line = source_line,
    end_source_line = end_source_line,
    body = "",
    focused = true,
    new = true,
    kind = properties and properties.kind,
    parent_id = parent and tostring(parent.id) or nil,
    heading = properties and properties.heading,
  }
  state.next_id = state.next_id + 1
  M.add(buf, annotation, start_insert)
end

--- Update thread presentation in one render without replacing local draft bodies.
---@param buf integer
---@param updates {id: integer|string, heading?: string, replies?: {heading: string, body_lines: string[]}[], replies_body?: string}[]
function M.update(buf, updates)
  local state = state_by_buf[buf]
  if not state then return end
  sync_focused_body(state)
  local win = displayed_window(state)
  local cursor_namespace = vim.api.nvim_create_namespace("ForgeDraftCommentCursor")
  local cursor = win and vim.api.nvim_win_get_cursor(win)
  local cursor_mark = cursor and vim.api.nvim_buf_set_extmark(buf, cursor_namespace, cursor[1] - 1, cursor[2], { right_gravity = false })
  for _, update in ipairs(updates) do
    local annotation = find_annotation(state, update.id)
    if annotation then
      annotation.heading = update.heading or annotation.heading
      annotation.replies = update.replies
      annotation.replies_body = update.replies_body
    end
  end
  render(state)
  if cursor_mark and win and vim.api.nvim_win_is_valid(win) then
    local position = vim.api.nvim_buf_get_extmark_by_id(buf, cursor_namespace, cursor_mark, {})
    if #position == 2 then vim.api.nvim_win_set_cursor(win, { position[1] + 1, position[2] }) end
    vim.api.nvim_buf_del_extmark(buf, cursor_namespace, cursor_mark)
    sync_cursor_modifiable(state, win)
  end
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
  if state.guard then editable.detach(state.guard) end
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
