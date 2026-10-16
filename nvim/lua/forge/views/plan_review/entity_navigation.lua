local comment_view = require("forge.draft_comments")
local entity_info = require("forge.views.plan_review.entity_info")
local notifications = require("forge.infra.notifications")

local M = {}

---@class ForgePlanNavigationOptions
---@field source_line integer
---@field workspace_root string

---@param model ForgePlanTaskModel
---@param entity ForgePlanEntity
---@param buf integer
---@param win integer
local function jump_to_declaration(model, entity, buf, win)
  local source_line = model:entity_declaration_source_line(entity)
  local display_line = source_line and comment_view.display_line_for_source_line(buf, source_line) or nil
  if not display_line then
    notifications.info("This plan entity has no UML declaration", "ForgePlanReview")
    return
  end
  local target_text = vim.api.nvim_buf_get_lines(buf, display_line - 1, display_line, false)[1] or ""
  local target_col = math.max(0, (target_text:find(entity.name, 1, true) or 1) - 1)
  vim.api.nvim_win_call(win, function()
    vim.cmd(("normal! %dG"):format(display_line))
    vim.api.nvim_win_set_cursor(win, { display_line, target_col })
  end)
end

---@param target ForgePlanWorkspaceTarget
---@param workspace_root string
---@param win integer
local function open_workspace_source(target, workspace_root, win)
  local source_path = vim.fs.joinpath(workspace_root, target.path)
  local opened, open_error = pcall(vim.api.nvim_win_call, win, function()
    vim.cmd("edit " .. vim.fn.fnameescape(source_path))
    local source_buf = vim.api.nvim_win_get_buf(win)
    local line_count = vim.api.nvim_buf_line_count(source_buf)
    if target.line < 1 or target.line > line_count then
      error(("Workspace entity `%s` points to line %d in a %d-line file")
        :format(target.name, target.line, line_count))
    end
    local source_line = vim.api.nvim_buf_get_lines(source_buf, target.line - 1, target.line, false)[1] or ""
    local target_col = math.max(0, (source_line:find(target.name, 1, true) or 1) - 1)
    vim.api.nvim_win_set_cursor(win, { target.line, target_col })
  end)
  if not opened then
    notifications.error(tostring(open_error), "PlanReview workspace source")
  end
end

---@param model ForgePlanTaskModel?
---@param buf integer
---@param win integer
---@param options ForgePlanNavigationOptions
---@return boolean handled
function M.jump(model, buf, win, options)
  if not model then
    notifications.info("Plan entity navigation is unavailable", "ForgePlanReview")
    return false
  end
  local opening_cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, opening_cursor[1] - 1, opening_cursor[1], false)[1] or ""
  local workspace_target = model:workspace_target_at_position(
    options.source_line,
    line,
    opening_cursor[2]
  )
  if workspace_target then
    open_workspace_source(workspace_target, options.workspace_root, win)
    return true
  end
  local entity = entity_info.entity_at_cursor(model, buf, win, false)
  if entity then
    jump_to_declaration(model, entity, buf, win)
    return true
  end
  notifications.info("This plan reference has no captured source definition", "ForgePlanReview")
  return false
end

return M
