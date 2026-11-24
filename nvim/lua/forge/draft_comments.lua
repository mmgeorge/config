local M = {}

local comment_box = require("forge.render.comment_box")
local comment_editor = require("forge.render.comment_editor")
local notifications = require("forge.infra.notifications")
local editable = require("forge.editable")

local namespace = vim.api.nvim_create_namespace("ForgeDraftComments")
local blank_cursor_keys = vim.api.nvim_create_namespace("ForgeDraftCommentCursorInput")

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
---@field parent_id string? Parent message when the captured draft is a follow-up.
---@field source {start_line: integer, end_line: integer, body: string}
---@field kind string?

---@class ForgeDraftCommentRange
---@field folded? boolean Single summary row for the entire thread.
---@field inner_width? integer Interior width of a virtually framed card.
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
---@field source_mark { mark: integer, source_line: integer, source_index: integer, annotation_anchor?: boolean }[]
---@field range_list ForgeDraftCommentRange[]
---@field group integer
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
---@field reposition_input? fun()
---@field folded_thread table<string, boolean>
---@field blank_cursor? {window: integer, virtualedit: string}
---@field popup_input boolean
---@field markdown boolean

---@class ForgeDraftSourceRow
---@field id string
---@field text string
---@field display_indent? integer Presentation whitespace excluded from canonical source columns.
---@field source_line integer
---@field segments? table[]
---@field fold_id? string
---@field default_folded? boolean
---@field ancestor_ids? string[]
---@field annotation_anchor? boolean
---@field hidden? boolean

---@class ForgeDraftCommentOptions
---@field baseline? ForgeDraftCommentCapture[]
---@field focused_id? integer|string Retains the focused annotation during a source update.
---@field readonly? boolean
---@field heading? string
---@field source_label? fun(annotation: ForgeDraftComment): string
---@field editable_source? fun(row: integer): boolean
---@field source_provider? fun(width: integer): ForgeDraftSourceRow[]
---@field before_render? fun(buf: integer, win: integer?)
---@field after_render? fun(buf: integer, win: integer?, projection: ForgeDraftCommentProjection)
---@field guard_source? boolean
---@field folded_thread? table<string, boolean> Local fold choices keyed by root message ID.
---@field popup_input? boolean Keep annotations boxed during navigation and composition.
---@field markdown? boolean Render Markdown in compact bodies and read-only replies.

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
---@param annotation_id integer|string
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
  state.folded_thread[tostring(root.id)] = nil
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
---@field annotation_id integer|string|nil
---@field reply_index integer?
---@field source_line integer?

---@class ForgeDraftCommentProjection
---@field line_list string[]
---@field source_row_by_line table<integer, integer>
---@field range_list ForgeDraftCommentRange[]
---@field frame_mark {row: integer, col: integer, options: vim.api.keyset.set_extmark}[]
---@field compact_highlight_list { row: integer, start_col: integer, end_col: integer, hl: string, border_column?: integer }[]
---@field source_highlight_list { row: integer, start_col: integer, end_col: integer, hl: string }[]
---@field source_record_list { row: integer, source_line: integer, source_index: integer }[]
---@field line_meta_list table[]
---@field markdown_regions Range4[][]

--- Include only body bytes and newlines in each annotation's Markdown parse.
---@param projection ForgeDraftCommentProjection
---@param row integer
---@param first_col integer
---@param end_col integer
---@param starts_body boolean
local function add_markdown_row(projection, row, first_col, end_col, starts_body)
  local regions = projection.markdown_regions
  if starts_body then regions[#regions + 1] = {} end
  local region = regions[#regions]
  local line_length = #projection.line_list[row + 1]
  if end_col == line_length then
    region[#region + 1] = { row, first_col, row + 1, 0 }
  else
    if first_col < end_col then region[#region + 1] = { row, first_col, row, end_col } end
    region[#region + 1] = { row, line_length, row + 1, 0 }
  end
end

--- Keep only the body in the buffer. Virtual lines draw headings and the footer,
--- and virtual text draws the side borders outside the native selection.
---@param projection ForgeDraftCommentProjection
---@param box ForgeCommentBoxLines
---@param ancestors string[]
---@param continuation boolean?
---@return integer[] reply_rows
local function append_card(projection, box, ancestors, continuation)
  local marks = projection.frame_mark
  if continuation then table.remove(marks) end -- Replace the previous footer with the shared heading.
  local reply_rows = {}
  local headings = {}
  local first_body_row = #projection.line_list
  local frame = { width = box.inner_width - 2, indent = vim.fn.strdisplaywidth(box[2][1][1]) }
  for index, segments in ipairs(box) do
    local span = box.markdown_rows[index]
    if span then
      local row = #projection.line_list
      local body = segments[2][1]:sub(1, span.end_col - span.first_col)
      projection.line_list[row + 1] = body
      projection.line_meta_list[row + 1] = { ancestor_ids = ancestors, comment_frame = frame }
      add_markdown_row(projection, row, 0, #body, span.starts_body)
      if #headings > 0 then
        marks[#marks + 1] = { row = row, col = 0, options = { virt_lines = headings, virt_lines_above = true } }
        headings = {}
      end
      if span.starts_body and row > first_body_row then reply_rows[#reply_rows + 1] = row end
      marks[#marks + 1] = { row = row, col = 0, options = { virt_text = { segments[1] },
        virt_text_pos = "inline", right_gravity = false } }
      if #body > 0 then
        marks[#marks + 1] = { row = row, col = 0, options = { end_col = #body, hl_group = segments[2][2],
          priority = vim.hl.priorities.syntax - 1 } }
      end
      marks[#marks + 1] = { row = row, col = #body, options = { virt_text = { { "│", segments[3][2] } },
        virt_text_win_col = 3 + box.inner_width } }
    elseif index < #box then
      headings[#headings + 1] = segments
    else
      marks[#marks + 1] = { row = #projection.line_list - 1, col = 0, options = { virt_lines = { segments } } }
    end
  end
  return reply_rows
end

---@param state ForgeDraftCommentState
---@param width integer
---@return ForgeDraftCommentProjection
local function build_projection(state, width)
  local projection = {
    line_list = {},
    source_row_by_line = {},
    range_list = {},
    compact_highlight_list = {},
    frame_mark = {},
    source_highlight_list = {},
    source_record_list = {},
    line_meta_list = {},
    markdown_regions = {},
  }
  local annotation_by_source = {}
  local source_count = math.max(1, #state.source_lines)
  local source_row_list = {}
  if state.source_provider then
    source_row_list = state.source_provider(width) or {}
    -- Projected sources can omit a folded file, including the final annotated rows.
    for _, annotation in ipairs(state.annotation_list) do
      source_count = math.max(source_count, tonumber(annotation.end_source_line) or 1)
    end
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
  local thread = {}
  for _, annotation in ipairs(state.annotation_list) do
    local root = thread_root(state, annotation)
    thread[root] = thread[root] or {}
    table.insert(thread[root], annotation)
  end
  local ordered, summary = {}, {}
  for _, root in ipairs(state.annotation_list) do
    if not root.parent_id then
      local messages = thread[root]
      if state.folded_thread[tostring(root.id)] then
        local questions = 0
        for _, message in ipairs(messages) do
          if message.kind == "question" then questions = questions + 1 end
        end
        local comments = #messages - questions
        summary[root] = ("  ▸ %d question%s · %d comment%s"):format(questions,
          questions == 1 and "" or "s", comments, comments == 1 and "" or "s")
        ordered[#ordered + 1] = root
      else
        vim.list_extend(ordered, messages)
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
    if not source_row.hidden then
    local source_line = math.max(1, math.min(tonumber(source_row.source_line) or 1, source_count))
    local row = #projection.line_list
    if projection.source_row_by_line[source_line] == nil then projection.source_row_by_line[source_line] = row end
    projection.line_list[#projection.line_list + 1] = string.rep(" ", source_row.display_indent or 0) .. (source_row.display_text or source_row.text or "")
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
      local reply_start_row, reply_start_list, inner_width
      local previous = projection.range_list[#projection.range_list]
      local shared_divider = annotation.parent_id and previous
        and previous.compact == not annotation.focused
        and thread_root(state, previous.annotation) == thread_root(state, annotation)
      local virtual_frame = state.popup_input and not annotation.focused
      if shared_divider and not virtual_frame then first_row = #projection.line_list - 1 end
      if summary[annotation] then
        projection.line_list[first_row + 1] = ""
        projection.line_meta_list[first_row + 1] = { ancestor_ids = source_row.ancestor_ids or {} }
        projection.frame_mark[#projection.frame_mark + 1] = { row = first_row, col = 0,
          options = { virt_text = { { summary[annotation], comment_box.default_style.heading } }, virt_text_pos = "overlay" } }
        inner_width = comment_box.inner_width(width + 1)
      elseif annotation.focused then
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
          markdown = state.markdown,
        }
        local box_lines = comment_box.build_box_lines(descriptor, width + 1)
        if virtual_frame then
          inner_width = box_lines.inner_width
          reply_start_list = append_card(projection, box_lines, source_row.ancestor_ids or {}, shared_divider)
          reply_start_row = reply_start_list[1]
        else
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
            local markdown = box_lines.markdown_rows[index]
            if markdown then add_markdown_row(projection, row, markdown.first_col, markdown.end_col, markdown.starts_body) end
            projection.line_meta_list[row + 1] = {
              ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}),
            }
            for highlight_index, highlight in ipairs(highlight_list) do
              local border_column
              if markdown and highlight_index == #highlight_list and vim.fn.strdisplaywidth(text) <= width then
                border_column = vim.fn.strdisplaywidth(text:sub(1, highlight.start_col))
              end
              projection.compact_highlight_list[#projection.compact_highlight_list + 1] = {
                row = row,
                start_col = highlight.start_col,
                end_col = highlight.end_col,
                hl = highlight.hl,
                border_column = border_column,
              }
            end
          end
        end
      end
      projection.range_list[#projection.range_list + 1] = {
        annotation = annotation,
        compact = not annotation.focused,
        inner_width = inner_width,
        folded = summary[annotation] ~= nil,
        first_row = first_row,
        last_row = #projection.line_list - 1,
        reply_start_row = reply_start_row,
        reply_start_list = reply_start_list,
      }
      if annotation.focused then
        for reply_index, reply in ipairs(replies or {}) do
          local first_reply_row = #projection.line_list - 1
          projection.line_list[#projection.line_list] = comment_editor.rule_line(" " .. reply.heading .. " ", "", width)
          local reply_lines = comment_box.wrap_body(reply.body_lines or {}, width, state.markdown)
          for index, line in ipairs(reply_lines) do
            projection.line_list[#projection.line_list + 1] = line
            projection.line_meta_list[#projection.line_list] = { ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}) }
            if state.markdown then add_markdown_row(projection, #projection.line_list - 1, 0, #line, index == 1) end
          end
          projection.line_list[#projection.line_list + 1] = comment_editor.footer_line(width)
          projection.line_meta_list[#projection.line_list] = { ancestor_ids = vim.deepcopy(source_row.ancestor_ids or {}) }
          projection.range_list[#projection.range_list + 1] = { annotation = annotation, compact = false,
            readonly = true, reply_index = reply_index, first_row = first_reply_row, last_row = #projection.line_list - 1 }
        end
      end
      end
    end
  end
  end
  return projection
end

---@param state ForgeDraftCommentState
---@param projection ForgeDraftCommentProjection
---@param mark_phase? fun(field: string)
local function apply_projection(state, projection, mark_phase)
  local previous = vim.api.nvim_buf_get_lines(state.buf, 0, -1, false)
  local change_list = assert(vim.text.diff(table.concat(previous, "\n") .. "\n",
    table.concat(projection.line_list, "\n") .. "\n", { result_type = "indices", algorithm = "histogram" }))
  if mark_phase then mark_phase("text_diff_ms") end
  for index = #change_list, 1, -1 do
    local change = change_list[index]
    local start = change[2] == 0 and change[1] or change[1] - 1
    local replacement = {}
    for row = change[3], change[3] + change[4] - 1 do replacement[#replacement + 1] = projection.line_list[row] end
    vim.api.nvim_buf_set_lines(state.buf, start, start + change[2], false, replacement)
  end
  if mark_phase then mark_phase("text_ms") end
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
  for _, mark in ipairs(projection.frame_mark) do
    vim.api.nvim_buf_set_extmark(state.buf, namespace, mark.row, mark.col, mark.options)
  end
  for _, highlight in ipairs(projection.compact_highlight_list) do
    local options = {
      end_col = highlight.end_col,
      hl_group = highlight.hl,
    }
    if highlight.border_column then
      -- Concealed Markdown delimiters change the display width of the body.
      options.conceal = ""
      options.virt_text_win_col = highlight.border_column
      options.virt_text = {
        { projection.line_list[highlight.row + 1]:sub(highlight.start_col + 1, highlight.end_col), highlight.hl },
      }
    end
    vim.api.nvim_buf_set_extmark(state.buf, namespace, highlight.row, highlight.start_col, options)
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
local function release_blank_cursor(state)
  local owner = state.blank_cursor
  if not owner then return end
  state.blank_cursor = nil
  vim.on_key(nil, blank_cursor_keys)
  if vim.api.nvim_win_is_valid(owner.window) then
    local position = owner.window == vim.api.nvim_get_current_win() and vim.fn.getcurpos()
    vim.wo[owner.window].virtualedit = owner.virtualedit
    if position then vim.fn.cursor({ position[2], position[3], 0, position[5] }) end
  end
end

--- On an empty buffer line Neovim draws the cursor before inline virtual text.
--- Give that line a single virtual cursor position at the start of the card body.
---@param state ForgeDraftCommentState
local function align_blank_cursor(state)
  if not state.popup_input or vim.api.nvim_get_current_buf() ~= state.buf then return end
  local window = vim.api.nvim_get_current_win()
  local mode = vim.api.nvim_get_mode().mode
  if (mode == "n" or mode == "v" or mode == "V" or mode == "\22") and vim.api.nvim_get_current_line() == "" then
    local position = vim.fn.getcurpos()
    local start = { position[2] - 1, 0 }
    local marks = vim.api.nvim_buf_get_extmarks(state.buf, namespace, start, start, { type = "virt_text", details = true })
    for _, mark in ipairs(marks) do
      if mark[4].virt_text_pos == "inline" then
        local column = vim.fn.strdisplaywidth(mark[4].virt_text[1][1])
        if not state.blank_cursor then
          state.blank_cursor = { window = window, virtualedit = vim.wo[window].virtualedit }
          -- Native motions and operators must consume the real EOL, including when a
          -- single blank line is yanked. Reapply the display offset when input settles.
          vim.on_key(function() release_blank_cursor(state) end, blank_cursor_keys)
        end
        if vim.wo[window].virtualedit ~= "all" then vim.wo[window].virtualedit = "all" end
        if position[4] ~= column then vim.fn.cursor({ position[2], 1, column, position[5] }) end
        return
      end
    end
  end
  release_blank_cursor(state)
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
---@param mark_phase? fun(field: string)
local function render(state, target, mark_phase)
  if not vim.api.nvim_buf_is_valid(state.buf) then return end
  state.rendering = true
  if state.guard and state.guard.native then editable.applying(state.guard, true) end
  set_modifiable(state, true)
  local win = displayed_window(state)
  if state.before_render then state.before_render(state.buf, win) end
  vim.api.nvim_buf_clear_namespace(state.buf, namespace, 0, -1)
  local width = comment_editor.display_width(win, state.buf)
  local projection = build_projection(state, width)
  if mark_phase then mark_phase("comment_layout_ms") end
  apply_projection(state, projection, mark_phase)
  if mark_phase then mark_phase("comment_buffer_ms") end
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
  if mark_phase then mark_phase("comment_guard_ms") end
  if state.after_render then state.after_render(state.buf, win, projection) end
  local requested_row = target_row(state, projection, target)
  if win and requested_row then vim.api.nvim_win_set_cursor(win, { requested_row + 1, 0 }) end
  sync_cursor_modifiable(state, win)
  align_blank_cursor(state)
  state.rendering = false
  if state.reposition_input then state.reposition_input() end
  if state.guard and state.guard.native then editable.applying(state.guard, false) end
end

---@param state ForgeDraftCommentState
local function handle_cursor_moved(state)
  if state.rendering or not vim.api.nvim_buf_is_valid(state.buf)
    or state.guard and state.guard.native and state.guard.native.rejecting then return end
  align_blank_cursor(state)
  if state.readonly then set_modifiable(state, false) return end
  local win = displayed_window(state)
  if not win then return end
  sync_focused_body(state)
  local row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local annotation, range, reply = annotation_at_row(state, row)
  if annotation and range then
    if annotation.readonly or state.popup_input and range.compact then
      sync_cursor_modifiable(state, win)
      return
    end
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
  if state.popup_input then
    vim.api.nvim_create_autocmd({ "BufLeave", "WinLeave" }, { group = group, buffer = state.buf,
      callback = function() release_blank_cursor(state) end })
    vim.api.nvim_create_autocmd({ "BufEnter", "WinEnter", "ModeChanged", "SafeState" }, { group = group, buffer = state.buf,
      callback = function() align_blank_cursor(state) end })
  end
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
      release_blank_cursor(state)
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
---@param mark_phase? fun(field: string) Time the initial render only.
function M.attach(buf, win, source_lines, annotation_list, opts, mark_phase)
  opts = opts or {}
  local previous = state_by_buf[buf]
  if previous then
    release_blank_cursor(previous)
    pcall(vim.api.nvim_del_augroup_by_id, previous.group)
    if previous.guard then editable.detach(previous.guard) end
  end
  for _, annotation in ipairs(annotation_list) do
    annotation.focused = opts.focused_id ~= nil and tostring(annotation.id) == tostring(opts.focused_id)
  end
  local state = {
    buf = buf,
    win = win,
    source_lines = vim.deepcopy(source_lines),
    annotation_list = annotation_list,
    source_mark = {},
    range_list = {},
    group = 0,
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
    markdown = opts.markdown == true,
    popup_input = opts.popup_input == true,
    folded_thread = opts.folded_thread or {},
    guard_source = opts.guard_source == true,
  }
  state_by_buf[buf] = state
  for _, annotation in ipairs(opts.baseline and {} or annotation_list) do
    state.baseline[#state.baseline + 1] = capture_annotation(annotation)
  end
  install_autocmd(state)
  render(state, nil, mark_phase)
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

--- Remove every comment that holds no text, so a save stores nothing for it and deletes a saved
--- comment that was edited to empty. A comment that another one follows up stays, because
--- removing it would orphan its thread.
---@param buf integer
function M.discard_empty(buf)
  local state = state_by_buf[buf]
  if not state or state.readonly then return end
  sync_focused_body(state)
  local followed = {}
  for _, annotation in ipairs(state.annotation_list) do
    if annotation.parent_id then followed[tostring(annotation.parent_id)] = true end
  end
  local discarded
  for index = #state.annotation_list, 1, -1 do
    local annotation = state.annotation_list[index]
    if vim.trim(annotation.body) == "" and not annotation.readonly and not followed[tostring(annotation.id)] then
      discarded = annotation
      table.remove(state.annotation_list, index)
    end
  end
  if not discarded then return end
  local win = displayed_window(state)
  if win and win == vim.api.nvim_get_current_win() and vim.api.nvim_get_mode().mode:sub(1, 1) == "i" then
    vim.cmd("stopinsert")
  end
  render(state, { source_line = discarded.end_source_line })
end

---@param buf integer
---@param captured ForgeDraftCommentCapture[]
function M.saved(buf, captured)
  local state = assert(state_by_buf[buf], "comment view is not attached")
  state.baseline = vim.deepcopy(captured)
  sync_focused_body(state)
  local collapsed
  for _, annotation in ipairs(state.annotation_list) do
    for _, saved in ipairs(captured) do
      if tostring(annotation.id) == saved.id then
        annotation.new = nil
        -- Collapsing is the save's visible acknowledgement. A body typed into after the save was
        -- sent is still being edited, so it stays open.
        if annotation.focused and capture_annotation(annotation).source.body == saved.source.body then
          annotation.focused, collapsed = false, annotation
        end
        break
      end
    end
  end
  if collapsed then
    local win = displayed_window(state)
    if win and win == vim.api.nvim_get_current_win() and vim.api.nvim_get_mode().mode:sub(1, 1) == "i" then
      vim.cmd("stopinsert")
    end
    -- The cursor returns to the source line the comment annotates, as when it leaves the comment.
    -- A cursor left inside the collapsed rows would focus the comment again on the editor's next
    -- CursorMoved and reopen it as the editor, unlike every other saved comment.
    render(state, { source_line = collapsed.end_source_line })
  end
  vim.bo[buf].modified = not vim.deep_equal(M.capture(buf), state.baseline)
  if state.guard and state.guard.native then state.guard.native.modified = vim.bo[buf].modified end
end

---@param annotations ForgeDraftComment[]
---@return table<string, boolean>
function M.folded_roots(annotations)
  local folded = {}
  for _, annotation in ipairs(annotations) do
    if not annotation.parent_id then folded[tostring(annotation.id)] = true end
  end
  return folded
end

--- A retained review can reopen while its answer request or unsaved feedback still owns the buffer.
---@param buf integer
function M.fold_all_threads(buf)
  local state = state_by_buf[buf]
  if not state then return end
  local win = displayed_window(state)
  local row = win and vim.api.nvim_win_get_cursor(win)[1] - 1
  local annotation = row and annotation_at_row(state, row)
  local source = row and source_line_at_row(state, row)
  local folded = M.folded_roots(state.annotation_list)
  if vim.deep_equal(state.folded_thread, folded) then return end
  sync_focused_body(state)
  for _, message in ipairs(state.annotation_list) do message.focused = false end
  for id in pairs(state.folded_thread) do state.folded_thread[id] = nil end
  for id in pairs(folded) do state.folded_thread[id] = true end
  render(state, { annotation_id = annotation and thread_root(state, annotation).id, source_line = source })
end

--- Toggle the entire conversation under the cursor, including its read-only answers.
---@param buf integer
---@param win integer
---@return boolean handled
function M.toggle_thread_fold(buf, win)
  local state = state_by_buf[buf]
  if not state or not state.popup_input then return false end
  local annotation = annotation_at_row(state, vim.api.nvim_win_get_cursor(win)[1] - 1)
  if not annotation then return false end
  sync_focused_body(state)
  local root = thread_root(state, annotation)
  local id = tostring(root.id)
  state.folded_thread[id] = not state.folded_thread[id] or nil
  for _, message in ipairs(state.annotation_list) do
    if message.focused and thread_root(state, message) == root then message.focused = false end
  end
  state.win = win
  render(state, { annotation_id = root.id })
  return true
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

---@param state ForgeDraftCommentState
local function start_insert_later(state)
  vim.schedule(function()
    local displayed = displayed_window(state)
    if displayed and vim.api.nvim_get_current_win() == displayed then vim.cmd("startinsert") end
  end)
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
  annotation.focused, annotation.new = not state.popup_input, true
  state.annotation_list[#state.annotation_list + 1] = annotation
  if state.popup_input then state.folded_thread[tostring(thread_root(state, annotation).id)] = nil
  else focus_thread(state, annotation) end
  render(state, { annotation_id = annotation.id })
  if not state.popup_input and start_insert ~= false then start_insert_later(state) end
  return annotation
end

--- Open `annotation`'s own body for editing, leaving the cursor where it is when it already sits
--- inside that body.
---@param state ForgeDraftCommentState
---@param annotation ForgeDraftComment
---@param win integer
local function edit_annotation(state, annotation, win)
  local cursor_row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local _, range, reply = annotation_at_row(state, cursor_row)
  local header_row, footer_row
  if range and not range.compact and not reply then header_row, footer_row = full_range_rows(state, range) end
  if annotation.focused and header_row and cursor_row > header_row and cursor_row < footer_row then
    sync_cursor_modifiable(state, win)
    return
  end
  focus_thread(state, annotation)
  render(state, { annotation_id = annotation.id })
end

local function comment_target(state, win)
  local mode = vim.fn.mode(1)
  local cursor_row = vim.api.nvim_win_get_cursor(win)[1] - 1
  local first_row = cursor_row
  local last_row = cursor_row
  if mode == "v" or mode == "V" or mode == "\22" then
    first_row = vim.fn.getpos("v")[2] - 1
  end
  local bottom = math.max(first_row, last_row)
  local parent, range, on_answer = annotation_at_row(state, cursor_row)
  local anchor = parent
  if first_row ~= last_row then anchor = state.popup_input and annotation_at_row(state, first_row) or nil end
  if parent and anchor and (parent == anchor or thread_root(state, parent) == thread_root(state, anchor)) then
    return parent.source_line, parent.end_source_line, parent, range, on_answer, bottom
  end
  local first, last = source_range_at_rows(state, first_row, last_row)
  if state.popup_input then
    for _, endpoint in pairs({ anchor = anchor, cursor = parent }) do
      first = math.min(first or endpoint.source_line, endpoint.source_line)
      last = math.max(last or endpoint.end_source_line, endpoint.end_source_line)
    end
  end
  return first, last, nil, nil, nil, bottom
end

--- Add a comment on the selected rows; a refusal notifies and returns nil without other effects.
---
--- On an answer row the comment follows up that answer in its thread. Inside a comment's own
--- rows the key edits that comment and returns it rather than stacking a second one beside it.
---@param buf integer
---@param start_insert? boolean
---@param properties? {kind?: string, heading?: string}
---@return ForgeDraftComment?
function M.add_at_cursor(buf, start_insert, properties)
  local state = state_by_buf[buf]
  if not state or state.readonly then return nil end
  sync_focused_body(state)
  local win = displayed_window(state)
  if not win then return nil end
  local source_line, end_source_line, parent, _, on_answer = comment_target(state, win)
  if parent and not on_answer then
    if parent.readonly then
      notifications.error("This comment cannot be edited", "Forge comments")
      return nil
    end
    edit_annotation(state, parent, win)
    if start_insert ~= false then start_insert_later(state) end
    return parent
  end
  if not source_line or not end_source_line then
    notifications.error("Selected rows have no source identity", "Forge comments")
    return nil
  end
  local annotation = {
    id = vim.text.hexencode(assert(vim.uv.random(16))),
    source_line = source_line,
    end_source_line = end_source_line,
    body = "",
    focused = true,
    new = true,
    kind = properties and properties.kind,
    parent_id = parent and tostring(parent.id) or nil,
    heading = properties and properties.heading,
  }
  return M.add(buf, annotation, start_insert)
end

---@class ForgeCommentInputTarget
---@field window integer
---@field heading string
---@field state ForgeDraftCommentState
---@field placement fun(): integer?, integer?, integer?, integer? Anchor row, column offset, interior width, and row offset.
---@field append fun(body: string, kind?: string, heading?: string): boolean

--- Capture the selected source or message before moving the review viewport. A reply
--- belongs to the selected message and appears after the last message in its thread.
---@param buf integer
---@return ForgeCommentInputTarget?
function M.input_target(buf)
  local state = state_by_buf[buf]
  local win = state and displayed_window(state)
  if not state or not win or state.readonly then return end
  release_blank_cursor(state)
  local first, last, parent, _, _, row = comment_target(state, win)
  if not first or not last then
    notifications.error("Selected rows have no source identity", "Forge comments")
    return
  end
  local root = parent and thread_root(state, parent)
  local function placement()
    if state_by_buf[buf] ~= state or not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then return end
    local usable = comment_editor.display_width(win, buf) + 1
    if root then
      for index = #state.range_list, 1, -1 do
        local range = state.range_list[index]
        if thread_root(state, range.annotation) == root then
          local footer = mark_row(state, range.footer_mark)
          if not footer then return end
          if range.folded then return footer, 2, math.max(1, math.min(range.inner_width, usable - 4)), 1 end
          if range.inner_width then
            local body_height = vim.api.nvim_win_text_height(win, { start_row = footer, start_vcol = 0, end_row = footer }).all
            return footer, 2, math.max(1, math.min(range.inner_width, usable - 4)), body_height + 1
          end
          local line = vim.api.nvim_buf_get_lines(buf, footer, footer + 1, false)[1]
          local indent = #line:match("^ *")
          local width = vim.api.nvim_win_call(win, function() return vim.fn.strdisplaywidth(line) end) - indent - 2
          return footer, indent, math.max(1, math.min(width, usable - indent - 2))
        end
      end
      return
    end
    return row, 2, math.max(1, math.min(comment_box.inner_width(usable), usable - 4))
  end
  local function append(body, kind, heading)
    if state_by_buf[buf] ~= state or not displayed_window(state) or parent and not find_annotation(state, parent.id) then
      notifications.error("The plan changed. Copy your draft and reopen it on the current plan.", "Forge comments")
      return false
    end
    local annotation = { id = vim.text.hexencode(assert(vim.uv.random(16))), body = body, source_line = first, end_source_line = last,
      kind = kind, heading = heading, parent_id = parent and tostring(parent.id) or nil }
    M.add(buf, annotation, false)
    return true
  end
  return { window = win, heading = state.heading, state = state, placement = placement, append = append }
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
    if #position == 2 then
      local row = math.min(position[1], vim.api.nvim_buf_line_count(buf) - 1)
      local text = vim.api.nvim_buf_get_lines(buf, row, row + 1, false)[1] or ""
      vim.api.nvim_win_set_cursor(win, { row + 1, math.min(position[2], #text) })
    end
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
  release_blank_cursor(state)
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
