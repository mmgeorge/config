local M = {}

local diff_parse = require("diff_review.render.diff_parse")
local source = require("diff_review.render.source")

---@class DiffReviewInteractionRender
---@field lines string[]
---@field row table<integer, table>
---@field fold { first: integer, last: integer, folded: boolean }[]
---@field highlight { line: integer, group: string }[]
---@field comment { line: integer, value: table }[]

---@param comment_list table[]
---@param file_path string
---@param old_line integer?
---@param new_line integer?
---@return table[]
local function comment_at_line(comment_list, file_path, old_line, new_line)
  local result = {}
  for _, comment in ipairs(comment_list or {}) do
    local comment_path = source.normalize_path(comment.file_path or "")
    local matches_line = comment.new_line ~= nil and comment.new_line == new_line
      or comment.new_line == nil and comment.old_line == old_line
    if comment_path == file_path and matches_line then result[#result + 1] = comment end
  end
  return result
end

---@param interactions table[]
---@return DiffReviewInteractionRender
function M.build(interactions)
  local result = { lines = {}, row = {}, fold = {}, highlight = {}, comment = {} }
  for interaction_index, interaction in ipairs(interactions or {}) do
    if #result.lines > 0 then result.lines[#result.lines + 1] = "" end
    local interaction_first = #result.lines + 1
    local state = tostring(interaction.state or "unknown"):gsub("_", " ")
    result.lines[#result.lines + 1] = ("▾ Interaction %d  [%s]"):format(interaction.ordinal or interaction_index, state)
    result.row[#result.lines] = { kind = "interaction", interaction = interaction }
    result.highlight[#result.highlight + 1] = { line = #result.lines, group = "DiffReviewStatusSection" }
    result.lines[#result.lines + 1] = "  " .. tostring(interaction.prompt or ""):gsub("\n", " ")
    result.row[#result.lines] = { kind = "prompt", interaction = interaction }
    result.highlight[#result.highlight + 1] = { line = #result.lines, group = "DiffReviewStatusHint" }

    local block_list = diff_parse.parse_unified_diff(interaction.diff_text or "")
    if #block_list == 0 then
      result.lines[#result.lines + 1] = "  No workspace changes"
      result.row[#result.lines] = { kind = "empty", interaction = interaction }
      result.highlight[#result.highlight + 1] = { line = #result.lines, group = "Comment" }
    else
      for _, block in ipairs(block_list) do
        local current_file = source.normalize_path(block.file)
        local file_first = #result.lines + 1
        result.lines[#result.lines + 1] = "  ▾ " .. current_file
        result.row[#result.lines] = { kind = "file", interaction = interaction, file_path = current_file }
        result.highlight[#result.highlight + 1] = { line = #result.lines, group = "DiffReviewStatusPath" }
        for _, raw_hunk in ipairs(block.hunks or {}) do
          local hunk = diff_parse.parse_hunk_body(raw_hunk, { preserve_trailing_blank = true })
          result.lines[#result.lines + 1] = "    " .. hunk.header
          result.row[#result.lines] = { kind = "hunk", interaction = interaction, file_path = current_file }
          result.highlight[#result.highlight + 1] = { line = #result.lines, group = "DiffReviewHunkHeader" }
          for _, parsed_line in ipairs(hunk.lines) do
            local metadata = {
              kind = "diff",
              interaction = interaction,
              file_path = current_file,
              old_line = parsed_line.old_line,
              new_line = parsed_line.new_line,
            }
            result.lines[#result.lines + 1] = "    " .. parsed_line.prefix .. parsed_line.code
            result.row[#result.lines] = metadata
            if parsed_line.prefix == "+" then
              result.highlight[#result.highlight + 1] = { line = #result.lines, group = "DiffAdd" }
            elseif parsed_line.prefix == "-" then
              result.highlight[#result.highlight + 1] = { line = #result.lines, group = "DiffDelete" }
            end
            for _, comment in ipairs(comment_at_line(interaction.comment, current_file, parsed_line.old_line, parsed_line.new_line)) do
              result.comment[#result.comment + 1] = { line = #result.lines, value = comment }
            end
          end
        end
        if #result.lines >= file_first then
          result.fold[#result.fold + 1] = { first = file_first, last = #result.lines, folded = false }
        end
      end
    end
    result.fold[#result.fold + 1] = {
      first = interaction_first,
      last = #result.lines,
      folded = interaction_index < #interactions,
    }
  end
  if #result.lines == 0 then result.lines = { "No interactions in this session." } end
  return result
end

return M
