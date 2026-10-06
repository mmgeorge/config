local M = {}

local notifications = require("forge.infra.notifications")
local popup_window = require("forge.infra.popup_window")
local entity_info = require("forge.views.plan_review.entity_info")

---@param model ForgePlanTaskModel?
---@param buf integer
---@param win integer
---@param callback fun(entity: ForgePlanEntity, new_name: string)
function M.prompt(model, buf, win, callback)
  local entity = entity_info.entity_at_cursor(model, buf, win)
  if not entity then return end
  if entity.action ~= "add" then
    notifications.error("Only newly added plan entities can be renamed", "ForgePlanReview")
    return
  end
  popup_window.input({
    prompt = "Rename entity: ",
    default = entity.name,
  }, function(value)
    if value == nil then return end
    local new_name = vim.trim(value)
    if new_name == "" then
      notifications.error("Entity name cannot be empty", "ForgePlanReview")
      return
    end
    if new_name == entity.name then return end
    callback(entity, new_name)
  end)
end

---@class ForgePlanRenamePreview
---@field block string
---@field column integer
---@field length integer

---@class ForgePlanRenameSelection
---@field symbol string
---@field name string
---@field expected_version integer
---@field preview ForgePlanRenamePreview[]

--- Preview snapshot-bound rename edits and commit only an unchanged review selection.
---@param review table
---@param selection ForgePlanRenameSelection
---@param captured table
---@param commit fun(name: string)
function M.proposed(review, selection, captured, commit)
  local namespace = vim.api.nvim_create_namespace("ForgePlanRename")
  local conceallevel, concealcursor = vim.wo[review.win].conceallevel, vim.wo[review.win].concealcursor
  vim.wo[review.win].conceallevel, vim.wo[review.win].concealcursor = 2, "nvic"
  local function clear()
    if vim.api.nvim_buf_is_valid(review.buf) then vim.api.nvim_buf_clear_namespace(review.buf, namespace, 0, -1) end
  end
  popup_window.incremental_input({ title = "Rename symbol", default = selection.name, on_change = function(value)
    clear()
    if value == "" or value:find("[\r\n]") or not review.owner.is_current(captured) then return end
    for _, edit in ipairs(selection.preview) do
      local _, row = review.owner.replica.sequence:position(edit.block)
      if row then
        row = require("forge.buffer").physical_row(review.owner.replica, row)
        vim.api.nvim_buf_set_extmark(review.buf, namespace, row, edit.column, {
          end_col = edit.column + edit.length, hl_group = "Substitute",
          conceal = "", virt_text = { { value, "Substitute" } }, virt_text_pos = "inline", priority = 250,
        })
      end
    end
  end }, function(value)
    clear()
    if vim.api.nvim_win_is_valid(review.win) then
      vim.wo[review.win].conceallevel, vim.wo[review.win].concealcursor = conceallevel, concealcursor
    end
    if not value or value == selection.name then return end
    if not review.owner.is_current(captured) then
      notifications.error("Plan changed while the rename popup was open", "ForgePlanReview") return
    end
    commit(vim.trim(value))
  end)
end

return M
