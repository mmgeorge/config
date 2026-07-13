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
function M.build(queue, width, pending_steer)
  pending_steer = pending_steer or {}
  if #queue == 0 and #pending_steer == 0 then return {}, 0 end
  local line_list = {}
  local preview_width = math.max(10, width - 2)
  if #pending_steer > 0 then
    line_list[#line_list + 1] = { { "• Steering active turn", "DiffReviewStatusLabel" } }
    for _, entry in ipairs(pending_steer) do
      line_list[#line_list + 1] = {
        { "└ " .. preview(entry.text or "", preview_width), "DiffReviewHarnessOutput" },
      }
    end
  end
  if #queue > 0 then
    line_list[#line_list + 1] = { { "• Queued follow-up inputs", "DiffReviewStatusLabel" } }
  end
  for _, text in ipairs(queue) do
    line_list[#line_list + 1] = { { "└ " .. preview(text, preview_width), "DiffReviewHarnessOutput" } }
  end
  return line_list, #line_list
end

return M
