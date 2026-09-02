local M = {}

local diff_render = require("diff_review.render.diff_render")
local git_data = require("diff_review.git.git_data")
local status_buffer = require("diff_review.views.status.status_buffer")

--- Constructs structured diff row records from raw unified diff text.
---@param diff_text string Unified diff text to render.
---@param hunk_staged boolean[] Staged flags array corresponding to each hunk.
---@param filename? string Optional file path for syntax detection.
---@param context_callback_key? function Optional callback key for Tree-sitter context updates.
---@param on_context_update? function Optional callback invoked when syntax context changes.
---@param options? table Optional render options.
---@return table[] rows Array of rendered diff row structures.
function M.build_rows(diff_text, hunk_staged, filename, context_callback_key, on_context_update, options)
  return diff_render.build_fancy_diff_rows(
    diff_text,
    hunk_staged,
    filename,
    context_callback_key,
    on_context_update,
    options
  )
end

--- Builds formatted text and highlight segments for a file summary header row.
---@param file table Status file record.
---@param indent? integer Number of leading whitespace columns.
---@return table[] segment_list Array of text and highlight group pairs.
function M.file_segment_list(file, indent)
  local resolved_indent = indent or 0
  local stats, stat_segment_list = git_data._status_file_stat_text_and_segments(file)
  local change_label, change_group = git_data._status_file_change_label(file)
  local padded_change = change_label .. string.rep(" ", math.max(0, #"Modified" - #change_label))
  local segment_list = {}
  if resolved_indent > 0 then segment_list[#segment_list + 1] = { string.rep(" ", resolved_indent) } end
  segment_list[#segment_list + 1] = { change_label, change_group }
  local label_padding = padded_change:sub(#change_label + 1)
  if label_padding ~= "" then segment_list[#segment_list + 1] = { label_padding } end
  segment_list[#segment_list + 1] = { " " }
  segment_list[#segment_list + 1] = { file.relpath, "DiffReviewStatusPath" }
  segment_list[#segment_list + 1] = { " " }
  for _, segment in ipairs(status_buffer.highlighted_text_segments(stats, stat_segment_list)) do
    segment_list[#segment_list + 1] = segment
  end
  return segment_list
end

--- Emits a file summary header line into the status render state.
---@param state table Target status view state.
---@param file table Status file record.
---@param entry table Navigation entry record for the file header.
---@param indent? integer Leading indentation column count.
---@return integer line One-based line index where row was inserted.
---@return table[] segment_list Highlight segments rendered on the line.
function M.append_file_header(state, file, entry, indent)
  local segment_list = M.file_segment_list(file, indent)
  return status_buffer.add_segment_line(state, segment_list, entry), segment_list
end

--- Appends formatted diff rows to the status render state, tracking first and last lines.
---@param state table Target status view state.
---@param row_list table[] Array of diff row records to append.
---@param entry table|fun(row: table): table? Static entry record or resolver function per row.
---@param indent? integer Leading indentation column count.
---@param include? fun(row: table): boolean Filter predicate determining whether row is appended.
---@return table result Summary table containing `first_line`, `last_line`, and optional `fold_text`.
function M.append_rows(state, row_list, entry, indent, include)
  local result = { first_line = #state.lines + 1, last_line = #state.lines, fold_text = nil }
  for _, row in ipairs(row_list or {}) do
    if not include or include(row) then
      ---@type table?
      local row_entry = nil
      if type(entry) == "function" then row_entry = entry(row) else row_entry = entry end
      local line = status_buffer.add_fancy_row(state, row, row_entry, indent)
      result.last_line = line
      if row.diff_review_hunk_header then result.fold_text = state.lines[line] end
    end
  end
  return result
end

return M
