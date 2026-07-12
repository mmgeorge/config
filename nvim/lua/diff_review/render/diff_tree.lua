local M = {}

local diff_parse = require("diff_review.render.diff_parse")
local diff_component = require("diff_review.render.diff_component")
local source = require("diff_review.render.source")

local function file_status(block)
  if block.old_file == "/dev/null" then return "added" end
  if block.new_file == "/dev/null" then return "deleted" end
  return "modified"
end

local function append_file_header(result, block, file_path, added, removed, indent, expand_key)
  local file = {
    relpath = file_path,
    filename = file_path,
    status = file_status(block),
    added = added,
    removed = removed,
  }
  diff_component.append_file_header(result, file, {
    kind = "file",
    file_path = file_path,
    expand_key = expand_key,
  }, indent)
  return #result.lines
end

---@class DiffReviewRenderedTree
---@field lines string[]
---@field highlights table[]
---@field extmarks table[]
---@field folds table[]
---@field rows table<integer, table>
---@field file_count integer
---@field added integer
---@field removed integer

---@param diff_text string?
---@param options? { indent?: integer, key_prefix?: string, interaction?: table, cwd?: string, on_update?: function, expanded?: table<string, boolean> }
---@return DiffReviewRenderedTree
function M.build(diff_text, options)
  options = options or {}
  local indent = options.indent or 0
  local expanded = options.expanded
  if expanded == nil then expanded = setmetatable({}, { __index = function() return true end }) end
  local row_map = {}
  local result = {
    lines = {},
    entries = row_map,
    rows = row_map,
    highlights = {},
    line_highlights = {},
    extmarks = {},
    diff_row_spans = {},
    boundary_lines = {},
    folds = {},
    file_count = 0, added = 0, removed = 0,
  }
  for block_index, block in ipairs(diff_parse.parse_unified_diff(diff_text or "")) do
    local file_path = source.normalize_path(block.file)
    local file_added = 0
    local file_removed = 0
    for _, raw_hunk in ipairs(block.hunks or {}) do
      local parsed = diff_parse.parse_hunk_body(raw_hunk, { preserve_trailing_blank = true })
      file_added = file_added + (parsed.added or 0)
      file_removed = file_removed + (parsed.removed or 0)
    end
    result.file_count = result.file_count + 1
    result.added = result.added + file_added
    result.removed = result.removed + file_removed
    local file_key = ("%s:file:%d"):format(options.key_prefix or "diff", block_index)
    local file_line = append_file_header(result, block, file_path, file_added, file_removed, indent, file_key)
    result.rows[file_line].interaction = options.interaction
    if #(block.hunks or {}) == 0 and expanded[file_key] then
      local empty_line = #result.lines + 1
      result.lines[empty_line] = string.rep(" ", indent + 2) .. "No textual diff"
      result.rows[empty_line] = {
        kind = "empty_diff",
        file_path = file_path,
        interaction = options.interaction,
      }
      result.highlights[#result.highlights + 1] = {
        line = empty_line,
        first = 0,
        last = -1,
        group = "Comment",
      }
    end
    if expanded[file_key] then
      for hunk_index, raw_hunk in ipairs(block.hunks or {}) do
        local hunk_diff = table.concat(raw_hunk.diff or {}, "\n")
        local filename = file_path
        if options.cwd then filename = vim.fs.joinpath(options.cwd, filename) end
        local row_key = ("%s:file:%d:hunk:%d"):format(options.key_prefix or "diff", block_index, hunk_index)
        local row_list = diff_component.build_rows(
          hunk_diff,
          { false },
          filename,
          function(line) return ("%s:%d"):format(row_key, line) end,
          options.on_update,
          {
            compact_replacements = true,
            syntax_source = "diff",
            syntax_diff_text = hunk_diff,
            fallback_syntax_diff_text = hunk_diff,
          }
        )
        diff_component.append_rows(result, row_list, function(row)
          return {
            kind = row.diff_review_hunk_header and "hunk" or "diff",
            file_path = file_path,
            interaction = options.interaction,
            expand_key = row.diff_review_hunk_header and row_key or nil,
          }
        end, indent, function(row)
          if row.diff_review_hunk_header then return true end
          return expanded[row_key] == true
        end)
      end
    end
  end
  return result
end

return M
