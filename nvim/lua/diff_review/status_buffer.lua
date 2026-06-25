--- Accumulates rendered lines, highlights, extmarks, and fold state into an explicit
--- DiffReviewStatusState, so every view builds its buffer through one state-passing seam
--- instead of mutating a render singleton.
---
--- Holds no state of its own: each function takes the target state, letting callers render
--- into per-buffer states (status, PR, review, diff) without cross-talk.
---@class DiffReviewStatusBufferModule
local M = {}

---@param state DiffReviewStatusState
---@param line integer
---@param start_col integer
---@param end_col integer
---@param hl_group string
---@param priority integer?
function M.add_highlight(state, line, start_col, end_col, hl_group, priority)
  state.highlights[#state.highlights + 1] = {
    line = line,
    start_col = start_col,
    end_col = end_col,
    hl_group = hl_group,
    priority = priority,
  }
end

---@param state DiffReviewStatusState
---@param line integer
---@param col integer
---@param opts table
function M.add_extmark(state, line, col, opts)
  state.extmarks[#state.extmarks + 1] = {
    line = line,
    col = col,
    opts = opts,
  }
end

---@param state DiffReviewStatusState
---@param text string
---@param entry table?
---@param line_hl_group string?
---@return integer line
function M.add_line(state, text, entry, line_hl_group)
  state.lines[#state.lines + 1] = text
  local line = #state.lines
  if entry then state.entries[line] = entry end
  if line_hl_group then
    state.line_highlights[#state.line_highlights + 1] = {
      line = line,
      hl_group = line_hl_group,
    }
  end
  return line
end

--- Split a segmented head line into plain text and per-segment highlight spans.
---@param segments table[]
---@return string text
---@return table[] segment_highlights
function M.segment_line_parts(segments)
  local parts = {}
  local col = 0
  local segment_highlights = {}
  for _, segment in ipairs(segments) do
    local text = segment[1] or ""
    parts[#parts + 1] = text
    if segment[2] and text ~= "" then
      segment_highlights[#segment_highlights + 1] = {
        start_col = col,
        end_col = col + #text,
        hl_group = segment[2],
      }
    end
    col = col + #text
  end
  return table.concat(parts), segment_highlights
end

---@param state DiffReviewStatusState
---@param segments table[]
---@param entry table?
---@return integer line
function M.add_segment_line(state, segments, entry)
  local text, segment_highlights = M.segment_line_parts(segments)
  local line = M.add_line(state, text, entry)
  for _, highlight in ipairs(segment_highlights) do
    M.add_highlight(state, line, highlight.start_col, highlight.end_col, highlight.hl_group)
  end
  return line
end

---@param state DiffReviewStatusState
---@param key string
---@param default boolean?
---@return boolean?
function M.folded(state, key, default)
  state = state or {}
  state.folds = state.folds or {}
  local folded = state.folds[key]
  if folded == nil then return default end
  return folded
end

---@param state DiffReviewStatusState
---@param key string
---@param folded boolean
function M.set_folded(state, key, folded)
  state.folds = state.folds or {}
  if state.folds[key] == folded then return end
  state.folds[key] = folded
  state.fold_revision = (state.fold_revision or 0) + 1
end

--- Render one fancy-diff row into `state`: plain text plus its highlight, extmark, and
--- per-line span decoration, routing diff-body decoration into the ephemeral span store
--- and keeping the gutter as a persistent inline extmark.
--- Emit review draft comments below the row when `state.review_after_row` is set.
---@param state DiffReviewStatusState
---@param row table
---@param entry table?
---@param indent integer?
---@return integer line
function M.add_fancy_row(state, row, entry, indent)
  local diff_review = require("diff_review")
  indent = indent or 0
  local text_parts = {}
  if indent > 0 then
    text_parts[#text_parts + 1] = string.rep(" ", indent)
  end

  local col = indent
  local row_highlights = {}
  local row_extmarks = {}
  local diff_line = nil
  local diff_lines = nil
  for _, chunk in ipairs(row) do
    if chunk.meta and chunk.meta.diff then
      diff_line = chunk.meta.diff
    end
    if chunk.meta and chunk.meta.diff_lines then
      diff_lines = chunk.meta.diff_lines
    end
    if type(chunk[1]) == "string" then
      local text = chunk[1]
      if text ~= "" then
        text_parts[#text_parts + 1] = text
        if chunk[2] then
          row_highlights[#row_highlights + 1] = {
            start_col = col,
            end_col = col + #text,
            hl_group = chunk[2],
          }
        end
        col = col + #text
      end
    elseif chunk.virt_text then
      local opts = {}
      for key, value in pairs(chunk) do
        if key ~= "col" then
          opts[key] = value
        end
      end
      row_extmarks[#row_extmarks + 1] = {
        col = (chunk.col or 0) + indent,
        opts = opts,
      }
    end
  end

  local line_text = table.concat(text_parts)
  local content_length = nil
  if row.diff_review_bg_hl then
    line_text, content_length = diff_review._diff_pad_highlighted_line(line_text, state.buf)
  end

  local line_entry = entry
  if row.diff_review_boundary or row.diff_review_context_padding then
    line_entry = nil
  end
  if diff_line and entry and (row.diff_review_boundary or row.diff_review_context_padding) then
    line_entry = {
      kind = "context_line",
      file = entry.file,
      diff_line = diff_line,
    }
    if diff_lines then line_entry.diff_lines = diff_lines end
  elseif diff_line and entry then
    local diff_entry = { diff_line = diff_line }
    if diff_lines then diff_entry.diff_lines = diff_lines end
    local inline_jump_spans = {}
    for _, inline_highlight in ipairs(row.diff_review_inline_highlights or {}) do
      inline_jump_spans[#inline_jump_spans + 1] = {
        start_col = indent + inline_highlight.start_col,
        end_col = indent + inline_highlight.end_col,
        hl_group = inline_highlight.hl_group,
        kind = inline_highlight.kind,
      }
    end
    if #inline_jump_spans > 0 then diff_entry.inline_jump_spans = inline_jump_spans end
    line_entry = vim.tbl_extend("force", entry, diff_entry)
  end
  local line = M.add_line(state, line_text, line_entry)
  -- Route diff-body decoration into the per-line span store instead of persistent
  -- extmarks, so the decoration provider emits it ephemerally for visible rows only.
  -- Chrome rows (commits/sections/headers) keep using the persistent accumulators.
  local spans = nil
  if row.diff_review_bg_hl then
    diff_review._diff_line_content_lengths = diff_review._diff_line_content_lengths or {}
    diff_review._diff_line_content_lengths[state.buf] = diff_review._diff_line_content_lengths[state.buf] or {}
    diff_review._diff_line_content_lengths[state.buf][line] = content_length or #line_text
    spans = spans or { highlights = {} }
    spans.bg = { end_col = #line_text, hl_group = row.diff_review_bg_hl, priority = 60 }
  end
  if row.diff_review_boundary then
    state.boundary_lines = state.boundary_lines or {}
    state.boundary_lines[line] = true
  end
  for _, highlight in ipairs(row_highlights) do
    spans = spans or { highlights = {} }
    spans.highlights[#spans.highlights + 1] = {
      start_col = highlight.start_col,
      end_col = highlight.end_col,
      hl_group = highlight.hl_group,
      priority = 90,
    }
  end
  for _, inline_highlight in ipairs(row.diff_review_inline_highlights or {}) do
    spans = spans or { highlights = {} }
    spans.highlights[#spans.highlights + 1] = {
      start_col = indent + inline_highlight.start_col,
      end_col = indent + inline_highlight.end_col,
      hl_group = inline_highlight.hl_group,
      priority = inline_highlight.priority or 110,
    }
  end
  -- The gutter is inline virt_text (it shifts where the code starts), so it must stay
  -- a persistent extmark: layout is fixed by the time the provider's on_line runs, so
  -- ephemeral inline virt_text would never render. Only non-layout decoration (syntax,
  -- background, intraline) moves to the provider.
  for _, extmark in ipairs(row_extmarks) do
    M.add_extmark(state, line, extmark.col, extmark.opts)
  end
  if spans then
    state.diff_row_spans = state.diff_row_spans or {}
    state.diff_row_spans[line] = spans
  end
  -- Review view: emit any draft comments anchored on this diff row as real
  -- (navigable, editable) lines right below it.
  if state.review_after_row and diff_line and entry and not (row.diff_review_boundary or row.diff_review_context_padding) then
    local emitted = {}
    local targets = diff_lines or { diff_line }
    for _, target_line in ipairs(targets) do
      local key = ("%s\0%s\0%s"):format(tostring(target_line.file), tostring(target_line.side), tostring(target_line.line))
      if not emitted[key] then
        emitted[key] = true
        state.review_after_row(target_line, indent)
      end
    end
  end
  return line
end

return M
