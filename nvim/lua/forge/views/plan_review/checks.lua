local M = {}
local picker = require("forge.views.picker")
local field = require("forge.views.picker.field")
local actions = require("forge.shared.command_specs").plan_check_actions

---@class ForgePlanCheckOption
---@field id string
---@field name string
---@field stage string
---@field enabled boolean
---@field available boolean
---@field reason? string
---@field test_scope? 'planned'|'whole_suite'

---Opens the reviewed plan's check choices without editing project defaults.
---@param review table
---@param update fun(changes: {enabled?: table<string, boolean>, test_scopes?: table<string, string>}, receive: fun(applied: boolean))
function M.open(review, update)
  local owner = "forge-plan-checks:" .. review.owner.document
  local busy, closed = false, false
  local editable = not review.plan.historical_revision and review.plan.state == "awaiting_review"
  local build_spec
  local function change(context, scope)
    local check = context.option and context.option.value
    if busy or closed or not editable or not check or review.owner.closed
      or review.plan.state ~= "awaiting_review" or review.plan.historical_revision then return end
    if scope and check.stage ~= "test" then return end
    if not scope and not check.available then return end
    local changes = scope and { test_scopes = { [check.id] = check.test_scope == "whole_suite" and "planned" or "whole_suite" } }
      or { enabled = { [check.id] = not check.enabled } }
    busy = true
    picker.update(build_spec())
    update(changes, function()
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
        columns = { (check.enabled and "● " or "○ ") .. check.name, check.stage, state,
          check.stage == "test" and field.render(check.test_scope == "whole_suite" and "Whole suite" or "Planned", editable) or "—" },
        detail = type(check.reason) == "string" and check.reason or nil,
        disabled_reason = not check.available and "Disabled in project settings" or nil,
      }
    end
    return {
      owner = owner, host = { window_list = { review.win }, control_win = review.win },
      page_list = {{ id = "checks", title = "Plan checks", column_headers = { "Check", "Stage", "State", "Tests" },
        option_list = options, empty_text = "This plan has no configured checks.",
        subtitle = busy and "Saving plan checks…" or "Changes apply to this plan only.",
        footer = editable and "↑↓ select  Space enable/disable  ←→ tests  ? help  q close" or "Read-only plan  ? help  q close" }},
      action_list = {
        { id = "toggle", key = "<Space>", desc = actions.toggle, callback = function(context) change(context, false) end },
        { id = "previous_scope", command = "previous_value", desc = actions.previous_scope, callback = function(context) change(context, true) end },
        { id = "next_scope", command = "next_value", desc = actions.next_scope, callback = function(context) change(context, true) end },
      },
      on_confirm = function(result) change(result, false) return false end,
      on_cancel = function() closed = true end,
      on_close = function() closed = true end,
    }
  end
  picker.open(build_spec())
end

return M
