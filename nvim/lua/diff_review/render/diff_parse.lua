--- Parses unified diff text into structured blocks, hunks, and gutter-annotated lines.
---
--- Pure parsing for the status view, the standalone diff buffer, and the render engine.
--- Holds no state, so every consumer requires it directly instead of bouncing through init.
local M = {}

-- Resolve diff_render lazily so diff_render can require this module at load time without a cycle.
local diff_render
local function default_gutter()
  diff_render = diff_render or require("diff_review.render.diff_render")
  return diff_render.default_hunk_gutter_spec()
end

---@class DiffReviewParsedHunkLine
---@field prefix string
---@field code string
---@field old_line? integer
---@field new_line? integer
---@field position? integer

---@class DiffReviewParsedHunk
---@field header string
---@field old_start integer
---@field old_count integer
---@field new_start integer
---@field new_count integer
---@field context string
---@field diff string[]
---@field body string[]
---@field lines DiffReviewParsedHunkLine[]
---@field added integer
---@field removed integer
---@field gutter DiffReviewGutterSpec

---@class DiffReviewParsedBlock
---@field file string
---@field old_file? string
---@field new_file? string
---@field hunks DiffReviewParsedHunk[]

--- Parses a unified diff range descriptor (`"start,count"`) into start line and line count.
---@param range string Range string from a hunk header (e.g. `"12,5"` or `"42"`).
---@return integer start One-based starting line number.
---@return integer count Number of lines in the range.
local function parse_hunk_range(range)
  local start_text, count_text = range:match("^(%d+),?(%d*)$")
  local start = tonumber(start_text) or 0
  local count = count_text ~= "" and tonumber(count_text) or 1
  return start, count or 1
end

--- Parses a unified diff hunk header (`@@ -old,count +new,count @@ context`).
---@param header string Raw diff hunk header line.
---@return integer old_start Starting line in old revision.
---@return integer old_count Line count in old revision.
---@return integer new_start Starting line in new revision.
---@return integer new_count Line count in new revision.
---@return string context Trailing hunk function/scope context text.
local function parse_hunk_header(header)
  local old_range, new_range, context = header:match("^@@ %-(%d+,?%d*) %+(%d+,?%d*) @@%s?(.*)$")
  local old_start, old_count = parse_hunk_range(old_range or "0,0")
  local new_start, new_count = parse_hunk_range(new_range or "0,0")
  return old_start, old_count, new_start, new_count, context or ""
end

--- Strips standard git diff `"a/"` and `"b/"` path prefixes.
--- Preserves `"/dev/null"`.
---@param path string Raw path string from diff headers.
---@return string path Normalized repository relative path.
local function diff_path_without_prefix(path)
  if path == "/dev/null" then return path end
  return (path:gsub("^[ab]/", ""))
end

--- Parses unified diff text into structured file blocks and hunk models.
---@param diff_text string Unified diff text to parse.
---@return DiffReviewParsedBlock[] blocks Array of parsed file blocks containing hunks.
local function parse_unified_diff(diff_text)
  local blocks = {} ---@type DiffReviewParsedBlock[]
  local current_block = nil ---@type DiffReviewParsedBlock?
  local current_hunk = nil ---@type DiffReviewParsedHunk?

  for _, line in ipairs(vim.split(diff_text or "", "\n", { plain = true })) do
    local left_path, right_path = line:match("^diff %-%-git a/(.-) b/(.+)$")
    if left_path or right_path then
      current_block = {
        file = right_path or left_path or "",
        old_file = left_path,
        new_file = right_path,
        hunks = {},
      }
      blocks[#blocks + 1] = current_block
      current_hunk = nil
    elseif line:match("^%-%-%- ") then
      local path = line:match("^%-%-%-%s+(.+)$")
      if current_block then current_block.old_file = path and diff_path_without_prefix(path) or nil end
    elseif line:match("^%+%+%+ ") then
      local path = line:match("^%+%+%+%s+(.+)$")
      if current_block then current_block.new_file = path and diff_path_without_prefix(path) or nil end
      if path and path ~= "/dev/null" then
        if not current_block then
          current_block = { file = diff_path_without_prefix(path), hunks = {} }
          blocks[#blocks + 1] = current_block
        else
          current_block.file = diff_path_without_prefix(path)
        end
      end
    elseif line:match("^@@ ") then
      local old_start, old_count, new_start, new_count, context = parse_hunk_header(line)
      current_hunk = {
        header = line,
        old_start = old_start,
        old_count = old_count,
        new_start = new_start,
        new_count = new_count,
        context = context,
        diff = { line },
        body = {},
        lines = {},
        added = 0,
        removed = 0,
        gutter = default_gutter(),
      }
      if not current_block then
        current_block = { file = "", hunks = {} }
        blocks[#blocks + 1] = current_block
      end
      current_block.hunks[#current_block.hunks + 1] = current_hunk
    elseif current_hunk then
      current_hunk.diff[#current_hunk.diff + 1] = line
      current_hunk.body[#current_hunk.body + 1] = line
    end
  end

  for _, block in ipairs(blocks) do
    for _, hunk in ipairs(block.hunks) do
      M.parse_hunk_body(hunk)
    end
  end

  return blocks
end

--- Calculates the minimum gutter character width needed to display a line number.
---@param value? integer Maximum line number to display.
---@return integer width Gutter column character count (at least 3).
local function line_number_width(value)
  return math.max(3, #tostring(value or 0))
end

--- Parses hunk body lines into structured lines with old and new line numbers.
--- Computes gutter width metrics based on maximum line numbers.
---@param hunk DiffReviewParsedHunk Target parsed hunk structure.
---@param opts? { preserve_trailing_blank?: boolean } Optional parsing options.
---@return DiffReviewParsedHunk hunk Mutated hunk with populated `lines` and `gutter`.
local function parse_hunk_body(hunk, opts)
  opts = opts or {}
  if not opts.preserve_trailing_blank then
    while #hunk.body > 0 and hunk.body[#hunk.body]:match("^%s*$") do
      hunk.body[#hunk.body] = nil
    end
  end

  local old_line = hunk.old_start
  local new_line = hunk.new_start
  local max_old = math.max(hunk.old_start, hunk.old_start + math.max(hunk.old_count - 1, 0))
  local max_new = math.max(hunk.new_start, hunk.new_start + math.max(hunk.new_count - 1, 0))

  for position, diff_line in ipairs(hunk.body) do
    local prefix = diff_line:sub(1, 1)
    local code = diff_line:sub(2)
    if prefix == " " then
      hunk.lines[#hunk.lines + 1] = { prefix = prefix, code = code, old_line = old_line, new_line = new_line, position = position }
      max_old = math.max(max_old, old_line)
      max_new = math.max(max_new, new_line)
      old_line = old_line + 1
      new_line = new_line + 1
    elseif prefix == "-" then
      hunk.lines[#hunk.lines + 1] = { prefix = prefix, code = code, old_line = old_line, position = position }
      hunk.removed = hunk.removed + 1
      max_old = math.max(max_old, old_line)
      old_line = old_line + 1
    elseif prefix == "+" then
      hunk.lines[#hunk.lines + 1] = { prefix = prefix, code = code, new_line = new_line, position = position }
      hunk.added = hunk.added + 1
      max_new = math.max(max_new, new_line)
      new_line = new_line + 1
    end
  end

  hunk.gutter = {
    old_width = line_number_width(max_old),
    new_width = line_number_width(max_new),
    width = line_number_width(max_old) + 2 + line_number_width(max_new) + 2 + 1 + 1,
  }
  return hunk
end

--- Resolves the one-based line number of the first modified or added line in a hunk.
--- Falls back to the hunk's new starting line if only deletions or context exist.
---@param hunk DiffReviewParsedHunk Target parsed hunk.
---@return integer line One-based line number in working tree file.
local function hunk_first_changed_current_line(hunk)
  local current_line = hunk.new_start
  for _, parsed_line in ipairs(hunk.lines) do
    if parsed_line.prefix == " " and parsed_line.new_line then
      current_line = parsed_line.new_line + 1
    elseif parsed_line.prefix == "+" and parsed_line.new_line then
      return parsed_line.new_line
    elseif parsed_line.prefix == "-" then
      return current_line
    end
  end
  return hunk.new_start
end

M.parse_hunk_range = parse_hunk_range
M.parse_hunk_header = parse_hunk_header
M.diff_path_without_prefix = diff_path_without_prefix
M.parse_unified_diff = parse_unified_diff
M.line_number_width = line_number_width
M.parse_hunk_body = parse_hunk_body
M.hunk_first_changed_current_line = hunk_first_changed_current_line

return M
