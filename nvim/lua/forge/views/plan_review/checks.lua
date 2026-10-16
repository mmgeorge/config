local M = {}
local picker = require("forge.views.picker")

---@class ForgePlanCheckOption
---@field id string
---@field name string
---@field stage string
---@field enabled boolean
---@field available boolean
---@field reason? string

---Opens the reviewed plan's check choices without editing project defaults.
---@param review table
---@param update fun(enabled: table<string, boolean>, receive: fun(applied: boolean))
function M.open(review, update)
  local owner = "forge-plan-checks:" .. review.owner.document
  local busy, closed = false, false
  local editable = not review.plan.historical_revision and review.plan.state == "awaiting_review"
  local build_spec
  local function toggle(context)
    local check = context.option and context.option.value
    if busy or closed or not editable or not check or not check.available or review.owner.closed then return end
    busy = true
    picker.update(build_spec())
    update({ [check.id] = not check.enabled }, function()
      busy = false
      if not closed and picker.is_open(owner) and not review.owner.closed then picker.update(build_spec()) end
    end)
  end
  build_spec = function()
    local options = {}
    for _, check in ipairs(review.owner.checks or {}) do
      local state = check.enabled and "Enabled" or "Disabled"
      options[#options + 1] = {
        id = check.id, value = check, label = check.name,
        columns = { (check.enabled and "● " or "○ ") .. check.name, check.stage, state },
        detail = type(check.reason) == "string" and check.reason or nil,
        disabled_reason = not check.available and "Disabled in project settings" or nil,
      }
    end
    return {
      owner = owner, host = { window_list = { review.win }, control_win = review.win },
      page_list = {{ id = "checks", title = "Plan checks", column_headers = { "Check", "Stage", "State" },
        option_list = options, empty_text = "This plan has no configured checks.",
        subtitle = busy and "Saving plan checks…" or "Changes apply to this plan only.",
        footer = editable and "↑↓ select  Space enable/disable  q close" or "Read-only plan  q close" }},
      action_list = {{ id = "toggle", key = "<Space>", desc = "Enable or disable this plan check", callback = toggle }},
      on_confirm = function(result) toggle(result) return false end,
      on_cancel = function() closed = true end,
      on_close = function() closed = true end,
    }
  end
  picker.open(build_spec())
end

return M
