local M = {}

local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local entity_info = require("diff_review.views.plan_review.entity_info")

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@param callback fun(entity: DiffReviewPlanEntity, new_name: string)
function M.prompt(model, buf, win, callback)
  local entity = entity_info.entity_at_cursor(model, buf, win)
  if not entity then return end
  if entity.action ~= "add" then
    notifications.error("Only newly added plan entities can be renamed", "PlanReview")
    return
  end
  popup_window.input({
    prompt = "Rename entity: ",
    default = entity.name,
  }, function(value)
    if value == nil then return end
    local new_name = vim.trim(value)
    if new_name == "" then
      notifications.error("Entity name cannot be empty", "PlanReview")
      return
    end
    if new_name == entity.name then return end
    callback(entity, new_name)
  end)
end

return M
