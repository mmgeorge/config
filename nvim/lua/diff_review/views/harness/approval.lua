local Approval = {}

local config = require("diff_review.infra.config")
local picker = require("diff_review.views.picker")

local function option_list(request)
  local result = {}
  for index, choice in ipairs(request.choice_list or {}) do
    result[#result + 1] = {
      key = config.options.picker.choice_keys[index],
      value = choice.id,
      label = choice.label,
    }
  end
  return result
end

---@param request table
---@param host table
function Approval.open(request, host)
  picker.open({
    owner = "approval",
    host = {
      window_list = host.window_list or { host.transcript_win },
      control_win = host.control_win or host.transcript_win,
    },
    page_list = {
      {
        id = request.id,
        title = "Approval requested",
        subtitle = request.title,
        content_list = {
          { text = request.detail or "", group = "DiffReviewPickerText" },
          request.reason and { text = "Reason: " .. request.reason, group = "DiffReviewPickerText" } or nil,
        },
        option_list = option_list(request),
        footer = "↑↓ select  Enter confirm  q close",
      },
    },
    on_confirm = function(result)
      host.resolve(request.id, result.option.value, function(resolved)
        if resolved then picker.close(false) end
      end)
      return false
    end,
    on_close = host.closed,
  })
end

function Approval.is_open()
  return picker.is_open("approval")
end

function Approval.close()
  picker.close()
end

return Approval
