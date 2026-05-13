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

---@param diff string
---@return table[]
function M.parse_hunks(diff)
  local hunks = {}
  local file_header = {}
  local current_file = nil
  local current_hunk = nil

  local function flush_hunk()
    if current_hunk then
      for _, line in ipairs(current_hunk.lines) do
        local first = line:sub(1, 1)
        if first == "+" then
          current_hunk.added = current_hunk.added + 1
        elseif first == "-" then
          current_hunk.removed = current_hunk.removed + 1
        end
      end
      table.insert(hunks, current_hunk)
      current_hunk = nil
    end
  end

  for line in diff:gmatch("[^\n]+") do
    if line:match("^diff %-%-git ") then
      flush_hunk()
      file_header = { line }
      local _, new_file = line:match("^diff %-%-git a/(.-) b/(.+)$")
      current_file = new_file
    elseif line:match("^@@") then
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

  local hunks = M.parse_hunks(diff)
  local metrics = M.summarize_hunks(hunks)

  if metrics.hunks <= options.max_hunks and metrics.changed <= options.max_changed_lines then
    return diff, false, metrics
  end

  local lines = {
    ("Compact diff: %d hunks, +%d -%d changed lines"):format(metrics.hunks, metrics.added, metrics.removed),
    ("Showing first %d lines from hunks with at least %d changed lines."):format(
      options.hunk_head_lines,
      options.min_hunk_changed_lines
    ),
  }

  local shown_hunks = 0
  local skipped_hunks = 0
  local skipped_changed = 0

  for _, hunk in ipairs(hunks) do
    local hunk_changed = hunk.added + hunk.removed
    if hunk_changed < options.min_hunk_changed_lines then
      skipped_hunks = skipped_hunks + 1
      skipped_changed = skipped_changed + hunk_changed
    else
      shown_hunks = shown_hunks + 1
      table.insert(lines, "")
      vim.list_extend(lines, hunk.file_header)
      table.insert(lines, hunk.header)
      local shown_lines = math.min(options.hunk_head_lines, #hunk.lines)
      for i = 1, shown_lines do
        table.insert(lines, hunk.lines[i])
      end
      if #hunk.lines > shown_lines then
        table.insert(lines, ("... omitted %d lines from this hunk ..."):format(#hunk.lines - shown_lines))
      end
    end
  end

  if shown_hunks == 0 then
    table.insert(lines, "")
    table.insert(lines, ("No hunks have at least %d changed lines."):format(options.min_hunk_changed_lines))
  end
  if skipped_hunks > 0 then
    table.insert(lines, "")
    table.insert(lines, ("Skipped %d small hunks (%d changed lines total)."):format(skipped_hunks, skipped_changed))
  end

  return table.concat(lines, "\n"), true, metrics
end

return M
