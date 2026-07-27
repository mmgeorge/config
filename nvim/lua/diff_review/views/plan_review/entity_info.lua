local M = {}

local display_text = require("diff_review.render.display_text")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local comment_view = require("diff_review.views.plan_review.comment")

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@return DiffReviewPlanEntity?
function M.entity_at_cursor(model, buf, win)
  if not model then
    notifications.info("Plan entity information is unavailable", "PlanReview")
    return nil
  end
  local cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, cursor[1] - 1, cursor[1], false)[1] or ""
  local entity = model:entity_at_position(line, cursor[2])
  if not entity then notifications.info("Cursor is not on a plan entity", "PlanReview") end
  return entity
end

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
function M.show(model, buf, win)
  local entity = M.entity_at_cursor(model, buf, win)
  if not entity then return end
  local description = vim.trim(tostring(entity.description or ""))
  if description == "" then
    notifications.info("This plan entity has no description", "PlanReview")
    return
  end
  local maximum_width = 40
  local line_list = display_text.wrap(description, maximum_width)
  local popup_buf, popup_win = popup_window.open({
    relative = "cursor",
    width = maximum_width,
    height = #line_list,
    enter = false,
    focusable = true,
    filetype = "markdown",
    title = "",
    border = "none",
  })
  vim.api.nvim_buf_set_lines(popup_buf, 0, -1, false, line_list)
  vim.bo[popup_buf].modifiable = false
  local group = vim.api.nvim_create_augroup("DiffReviewPlanEntityInfo" .. tostring(popup_buf), { clear = true })
  vim.api.nvim_create_autocmd({ "BufHidden", "CursorMoved", "CursorMovedI", "InsertCharPre" }, {
    group = group,
    buffer = buf,
    once = true,
    callback = function() popup_window.close(popup_win, false) end,
    desc = "Close PlanReview entity information after leaving its cursor position",
  })
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, function() popup_window.close(popup_win) end, {
      buffer = popup_buf,
      silent = true,
      nowait = true,
      desc = "Close PlanReview entity information",
    })
  end
end

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
function M.jump(model, buf, win)
  local entity = M.entity_at_cursor(model, buf, win)
  if not entity or not model then return end
  local source_line = model:entity_declaration_source_line(entity)
  local display_line = source_line and comment_view.display_line_for_source_line(buf, source_line) or nil
  if not display_line then
    notifications.info("This plan entity has no UML declaration", "PlanReview")
    return
  end
  local target_text = vim.api.nvim_buf_get_lines(buf, display_line - 1, display_line, false)[1] or ""
  local target_col = math.max(0, (target_text:find(entity.name, 1, true) or 1) - 1)
  vim.api.nvim_win_call(win, function()
    vim.cmd(("normal! %dG"):format(display_line))
    vim.api.nvim_win_set_cursor(win, { display_line, target_col })
  end)
end

return M
