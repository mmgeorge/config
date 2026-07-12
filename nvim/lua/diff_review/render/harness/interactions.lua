local M = {}

local diff_tree = require("diff_review.render.diff_tree")
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
  local result = { lines = {}, row = {}, fold = {}, highlight = {}, extmark = {}, comment = {} }
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

    local tree = diff_tree.build(interaction.diff_text, {
      indent = 2,
      key_prefix = "interaction:" .. tostring(interaction.id or interaction_index),
      interaction = interaction,
    })
    if tree.file_count == 0 then
      result.lines[#result.lines + 1] = "  No workspace changes"
      result.row[#result.lines] = { kind = "empty", interaction = interaction }
      result.highlight[#result.highlight + 1] = { line = #result.lines, group = "Comment" }
    else
      local offset = #result.lines
      for _, line in ipairs(tree.lines) do result.lines[#result.lines + 1] = line end
      for line, row in pairs(tree.rows) do
        local target_line = offset + line
        result.row[target_line] = row
        if row.kind == "diff" then
          for _, comment in ipairs(comment_at_line(
            interaction.comment,
            row.file_path,
            row.old_line,
            row.new_line
          )) do
            result.comment[#result.comment + 1] = { line = target_line, value = comment }
          end
        end
      end
      for _, highlight in ipairs(tree.highlights) do
        local shifted = vim.deepcopy(highlight)
        shifted.line = shifted.line + offset
        result.highlight[#result.highlight + 1] = shifted
      end
      for _, extmark in ipairs(tree.extmarks or {}) do
        local shifted = vim.deepcopy(extmark)
        shifted.line = shifted.line + offset
        if shifted.options and shifted.options.end_row then shifted.options.end_row = shifted.options.end_row + offset end
        result.extmark[#result.extmark + 1] = shifted
      end
      for _, fold in ipairs(tree.folds) do
        result.fold[#result.fold + 1] = {
          first = fold.first + offset,
          last = fold.last + offset,
          folded = fold.folded,
        }
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
