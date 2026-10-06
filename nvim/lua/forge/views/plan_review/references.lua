local M = {}
local picker = require("forge.views.picker")
local notifications = require("forge.infra.notifications")

---@class ForgePlanReference
---@field id string
---@field path string
---@field owner string
---@field name string
---@field kind string
---@field line integer

--- Open snapshot-bound plan usages through the shared searchable picker.
---@param review table
---@param references ForgePlanReference[]
---@param captured table
---@param select fun(id: string)
function M.open(review, references, captured, select)
  local options = {}
  for _, reference in ipairs(references) do
    options[#options + 1] = {
      id = reference.id,
      value = reference,
      label = ("%s:%d  %s  [%s]  %s"):format(reference.path, reference.line,
        reference.owner ~= "" and reference.owner or reference.name, reference.kind, reference.name),
    }
  end
  picker.open({
    id = "plan_references",
    title = "Plan references",
    host = { control_win = review.win, window_list = { review.win } },
    page_list = { { id = "references", title = "References", option_list = options,
      selection_mode = "single", search = { start_in_normal = true } } },
    on_confirm = function(result)
      if not review.owner.is_current(captured) then
        notifications.warn("Plan references changed while the picker was open", "ForgePlanReview")
        return
      end
      local id = result.option.value.id
      vim.schedule(function()
        if review.owner.is_current(captured) and vim.api.nvim_win_is_valid(review.win) then
          vim.api.nvim_set_current_win(review.win)
          select(id)
        end
      end)
    end,
  })
end

return M
