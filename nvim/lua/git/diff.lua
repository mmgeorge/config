local M = {}

local default_compact_options = {
  max_hunks = 30,
  max_changed_lines = 800,
  min_hunk_changed_lines = 8,
  hunk_head_lines = 12,
}

local function copy_list(list)
  local out = {}
  for i, value in ipairs(list) do
    out[i] = value
  end
  return out
end

local function iter_lines(text)
  text = tostring(text or "")
  local index = 1
  local length = #text
  return function()
    if index > length then return nil end
    local next_newline = text:find("\n", index, true)
    if next_newline then
      local line = text:sub(index, next_newline - 1)
      index = next_newline + 1
      return line
    end
    local line = text:sub(index)
    index = length + 1
    return line
  end
end

local function iter_line_ranges(text)
  local index = 1
  local length = #text
  return function()
    if index > length then return nil end
    local start_index = index
    local next_newline = text:find("\n", index, true)
    if next_newline then
      index = next_newline + 1
      return start_index, next_newline - 1
    end
    index = length + 1
    return start_index, length
  end
end

local function starts_with_at(text, start_index, prefix)
  return text:find(prefix, start_index, true) == start_index
end

local function new_metrics()
  return {
    hunks = 0,
    added = 0,
    removed = 0,
    changed = 0,
  }
end

local function compact_header_lines(options, metrics)
  return {
    ("Compact diff: %d hunks, +%d -%d changed lines"):format(metrics.hunks, metrics.added, metrics.removed),
    ("Showing first %d lines from hunks with at least %d changed lines."):format(
      options.hunk_head_lines,
      options.min_hunk_changed_lines
    ),
  }
end

local function finish_compact_hunk(metrics, candidates, hunk, min_hunk_changed_lines)
  if not hunk then return nil end
  local hunk_changed = hunk.added + hunk.removed
  metrics.hunks = metrics.hunks + 1
  metrics.added = metrics.added + hunk.added
  metrics.removed = metrics.removed + hunk.removed
  metrics.changed = metrics.changed + hunk_changed
  if hunk_changed >= min_hunk_changed_lines then
    candidates[#candidates + 1] = hunk
  end
  return nil
end

local function add_compact_footer(out, options, metrics, candidates, shown_hunks, skipped_changed)
  if shown_hunks == 0 then
    out[#out + 1] = ""
    out[#out + 1] = ("No hunks have at least %d changed lines."):format(options.min_hunk_changed_lines)
  end

  local skipped_hunks = metrics.hunks - #candidates
  if skipped_hunks > 0 then
    out[#out + 1] = ""
    out[#out + 1] = ("Skipped %d small hunks (%d changed lines total)."):format(skipped_hunks, skipped_changed)
  end
end

local function append_text_range(out, text, start_index, end_index)
  if start_index and end_index then
    out[#out + 1] = text:sub(start_index, end_index)
  end
end

local function compact_text(diff, options)
  local metrics = new_metrics()
  local candidates = {}
  local file_header_start = nil
  local file_header_end = nil
  local current_hunk = nil

  for line_start, line_end in iter_line_ranges(diff) do
    if starts_with_at(diff, line_start, "diff --git ") then
      current_hunk = finish_compact_hunk(metrics, candidates, current_hunk, options.min_hunk_changed_lines)
      file_header_start = line_start
      file_header_end = line_end
    elseif starts_with_at(diff, line_start, "@@") then
      current_hunk = finish_compact_hunk(metrics, candidates, current_hunk, options.min_hunk_changed_lines)
      current_hunk = {
        file_header_start = file_header_start,
        file_header_end = file_header_end,
        header_start = line_start,
        header_end = line_end,
        head_start = nil,
        head_end = nil,
        head_count = 0,
        line_count = 0,
        added = 0,
        removed = 0,
      }
    elseif current_hunk then
      current_hunk.line_count = current_hunk.line_count + 1
      if current_hunk.head_count < options.hunk_head_lines then
        current_hunk.head_start = current_hunk.head_start or line_start
        current_hunk.head_end = line_end
        current_hunk.head_count = current_hunk.head_count + 1
      end
      local first = diff:byte(line_start)
      if first == 43 then
        current_hunk.added = current_hunk.added + 1
      elseif first == 45 then
        current_hunk.removed = current_hunk.removed + 1
      end
    elseif file_header_start then
      file_header_end = line_end
    end
  end
  finish_compact_hunk(metrics, candidates, current_hunk, options.min_hunk_changed_lines)

  if metrics.hunks <= options.max_hunks and metrics.changed <= options.max_changed_lines then
    return diff, false, metrics
  end

  local out = compact_header_lines(options, metrics)
  local shown_hunks = 0
  local skipped_changed = metrics.changed

  for _, hunk in ipairs(candidates) do
    local hunk_changed = hunk.added + hunk.removed
    skipped_changed = skipped_changed - hunk_changed
    shown_hunks = shown_hunks + 1
    out[#out + 1] = ""
    append_text_range(out, diff, hunk.file_header_start, hunk.file_header_end)
    append_text_range(out, diff, hunk.header_start, hunk.header_end)
    append_text_range(out, diff, hunk.head_start, hunk.head_end)
    if hunk.line_count > hunk.head_count then
      out[#out + 1] = ("... omitted %d lines from this hunk ..."):format(hunk.line_count - hunk.head_count)
    end
  end

  add_compact_footer(out, options, metrics, candidates, shown_hunks, skipped_changed)
  return table.concat(out, "\n"), true, metrics
end

local function append_line_range(out, lines, start_index, end_index)
  if not start_index or not end_index then return end
  for i = start_index, end_index do
    out[#out + 1] = lines[i]
  end
end

local function compact_line_array(lines, options)
  local metrics = new_metrics()
  local candidates = {}
  local file_header_start = nil
  local file_header_end = nil
  local current_hunk = nil

  for i = 1, #lines do
    local line = lines[i]
    if line:sub(1, 11) == "diff --git " then
      current_hunk = finish_compact_hunk(metrics, candidates, current_hunk, options.min_hunk_changed_lines)
      file_header_start = i
      file_header_end = i
    elseif line:sub(1, 2) == "@@" then
      current_hunk = finish_compact_hunk(metrics, candidates, current_hunk, options.min_hunk_changed_lines)
      current_hunk = {
        file_header_start = file_header_start,
        file_header_end = file_header_end,
        header_index = i,
        head_start = nil,
        head_end = nil,
        head_count = 0,
        line_count = 0,
        added = 0,
        removed = 0,
      }
    elseif current_hunk then
      current_hunk.line_count = current_hunk.line_count + 1
      if current_hunk.head_count < options.hunk_head_lines then
        current_hunk.head_start = current_hunk.head_start or i
        current_hunk.head_end = i
        current_hunk.head_count = current_hunk.head_count + 1
      end
      local first = line:byte(1)
      if first == 43 then
        current_hunk.added = current_hunk.added + 1
      elseif first == 45 then
        current_hunk.removed = current_hunk.removed + 1
      end
    elseif file_header_start then
      file_header_end = i
    end
  end
  finish_compact_hunk(metrics, candidates, current_hunk, options.min_hunk_changed_lines)

  if metrics.hunks <= options.max_hunks and metrics.changed <= options.max_changed_lines then
    return table.concat(lines, "\n"), false, metrics
  end

  local out = compact_header_lines(options, metrics)
  local shown_hunks = 0
  local skipped_changed = metrics.changed

  for _, hunk in ipairs(candidates) do
    local hunk_changed = hunk.added + hunk.removed
    skipped_changed = skipped_changed - hunk_changed
    shown_hunks = shown_hunks + 1
    out[#out + 1] = ""
    append_line_range(out, lines, hunk.file_header_start, hunk.file_header_end)
    out[#out + 1] = lines[hunk.header_index]
    append_line_range(out, lines, hunk.head_start, hunk.head_end)
    if hunk.line_count > hunk.head_count then
      out[#out + 1] = ("... omitted %d lines from this hunk ..."):format(hunk.line_count - hunk.head_count)
    end
  end

  add_compact_footer(out, options, metrics, candidates, shown_hunks, skipped_changed)
  return table.concat(out, "\n"), true, metrics
end

---@param diff string
---@return table[]
function M.parse_hunks(diff)
  local hunks = {}
  local file_header = {}
  local current_file = nil
  local current_hunk = nil

  local function flush_hunk()
    if current_hunk then
      table.insert(hunks, current_hunk)
      current_hunk = nil
    end
  end

  for line in iter_lines(diff) do
    if line:sub(1, 11) == "diff --git " then
      flush_hunk()
      file_header = { line }
      local _, new_file = line:match("^diff %-%-git a/(.-) b/(.+)$")
      current_file = new_file
    elseif line:sub(1, 2) == "@@" then
      flush_hunk()
      current_hunk = {
        file = current_file,
        file_header = copy_list(file_header),
        header = line,
        lines = {},
        added = 0,
        removed = 0,
      }
    elseif current_hunk then
      table.insert(current_hunk.lines, line)
      local first = line:sub(1, 1)
      if first == "+" then
        current_hunk.added = current_hunk.added + 1
      elseif first == "-" then
        current_hunk.removed = current_hunk.removed + 1
      end
    elseif #file_header > 0 then
      table.insert(file_header, line)
    end
  end
  flush_hunk()

  return hunks
end

---@param hunks table[]
---@return table
function M.summarize_hunks(hunks)
  local added, removed = 0, 0
  for _, hunk in ipairs(hunks) do
    added = added + hunk.added
    removed = removed + hunk.removed
  end

  return {
    hunks = #hunks,
    added = added,
    removed = removed,
    changed = added + removed,
  }
end

---@param diff string
---@param options? table
---@return string text
---@return boolean compacted
---@return table metrics
function M.compact(diff, options)
  options = vim.tbl_extend("force", default_compact_options, options or {})
  return compact_text(tostring(diff or ""), options)
end

---@param lines string[]
---@param options? table
---@return string text
---@return boolean compacted
---@return table metrics
function M.compact_lines(lines, options)
  options = vim.tbl_extend("force", default_compact_options, options or {})
  return compact_line_array(lines or {}, options)
end

return M
