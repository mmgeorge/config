local M = {}

local diff_render = require("diff_review.render.diff_render")
local git_data = require("diff_review.git.git_data")
local status_buffer = require("diff_review.views.status.status_buffer")

---@param diff_text string
---@param hunk_staged boolean[]
---@param filename? string
---@param context_callback_key? function
---@param on_context_update? function
---@param options? table
---@return table[]
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

---@param file table
---@param indent? integer
---@return table[]
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

---@param state table
---@param file table
---@param entry table
---@param indent? integer
---@return integer line
---@return table[] segment_list
function M.append_file_header(state, file, entry, indent)
  local segment_list = M.file_segment_list(file, indent)
  return status_buffer.add_segment_line(state, segment_list, entry), segment_list
end

---@param state table
---@param row_list table[]
---@param entry table|fun(row: table): table?
---@param indent? integer
---@param include? fun(row: table): boolean
---@return table result
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
