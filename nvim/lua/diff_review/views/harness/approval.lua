local M = {}

local choice_flow = require("diff_review.views.harness.choice_flow")

---@class DiffReviewApprovalRequest
---@field id string
---@field title string
---@field detail string
---@field reason? string
---@field choice_list { id: string, label: string }[]

---@class DiffReviewApprovalHost
---@field transcript_win integer
---@field resolve fun(id: string, choice: string, callback: fun(resolved: boolean))
---@field closed function

---@param request DiffReviewApprovalRequest
---@return DiffReviewChoiceFlowOption[]
local function option_list(request)
  local key_list = { "n", "e", "i", "l", "u", "y", "o" }
  local result = {}
  for index, choice in ipairs(request.choice_list or {}) do
    result[#result + 1] = {
      key = key_list[index],
      value = choice.id,
      label = choice.label,
    }
  end
  return result
end

---@param request DiffReviewApprovalRequest
---@param host DiffReviewApprovalHost
function M.open(request, host)
  choice_flow.open({
    parent_win = host.transcript_win,
    title = "Approval requested",
    filetype = "DiffReviewHarnessApproval",
    width = 72,
    page_list = {
      {
        prompt = request.title,
        detail = request.reason and (request.detail .. "\nReason: " .. request.reason) or request.detail,
        option_list = option_list(request),
        footer = "  ↑↓ navigate  Enter select  q close",
      },
    },
    on_select = function(option, _, close)
      host.resolve(request.id, option.value, function(resolved)
        if resolved then close() end
      end)
    end,
    on_close = host.closed,
  })
end

function M.is_open()
  return choice_flow.is_open()
end

function M.close()
  choice_flow.close()
end

return M
