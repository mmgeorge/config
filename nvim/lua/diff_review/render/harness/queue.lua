local M = {}

---@param text string
---@param width integer
---@return string
local function preview(text, width)
  local compact = vim.trim(text:gsub("%s+", " "))
  if vim.fn.strdisplaywidth(compact) <= width then return compact end
  return vim.fn.strcharpart(compact, 0, math.max(1, width - 1)) .. "…"
end

---@param queue string[]
---@param width integer
---@return table[], integer
function M.build(queue, width)
  if #queue == 0 then return {}, 0 end
  local line_list = {
    { { "• Queued follow-up inputs", "DiffReviewStatusLabel" } },
  }
  local preview_width = math.max(10, width - 2)
  for _, text in ipairs(queue) do
    line_list[#line_list + 1] = { { "└ " .. preview(text, preview_width), "DiffReviewHarnessOutput" } }
  end
  return line_list, #line_list
end

return M
