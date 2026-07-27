local comment_view = require("diff_review.views.plan_review.comment")
local entity_info = require("diff_review.views.plan_review.entity_info")
local notifications = require("diff_review.infra.notifications")

local M = {}
local request_generation = 0

---@class DiffReviewRustdocNavigationOptions
---@field source_line integer
---@field plan_id string
---@field expected_version integer
---@field workspace_root string
---@field request fun(params: table, callback: fun(result: table?, request_error: string?))

---@param model DiffReviewPlanTaskModel
---@param entity DiffReviewPlanEntity
---@param buf integer
---@param win integer
local function jump_to_declaration(model, entity, buf, win)
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

---@param result table
---@param win integer
local function open_source(result, win)
  if type(result.path) ~= "string" or result.path == ""
      or type(result.line) ~= "number" or result.line < 1
      or type(result.column) ~= "number" or result.column < 1 then
    notifications.error("Harness returned an invalid Rust source location", "PlanReview Rust source")
    return
  end
  local opened, open_error = pcall(vim.api.nvim_win_call, win, function()
    vim.cmd("edit " .. vim.fn.fnameescape(result.path))
    local source_buf = vim.api.nvim_win_get_buf(win)
    vim.bo[source_buf].readonly = true
    vim.bo[source_buf].modifiable = false
    vim.api.nvim_win_set_cursor(win, { result.line, result.column - 1 })
  end)
  if not opened then
    notifications.error(tostring(open_error), "PlanReview Rust source")
  end
end

---@param target DiffReviewPlanWorkspaceTarget
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

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@param options DiffReviewRustdocNavigationOptions
---@return boolean handled
function M.jump(model, buf, win, options)
  if not model then
    notifications.info("Plan entity navigation is unavailable", "PlanReview")
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
  local target = model:rustdoc_target_at_position(options.source_line, line, opening_cursor[2])
  if not target then
    notifications.info("Cursor is not on a plan entity or external Rust symbol", "PlanReview")
    return false
  end
  request_generation = request_generation + 1
  local generation = request_generation
  options.request(vim.tbl_extend("force", target, {
    plan_id = options.plan_id,
    expected_version = options.expected_version,
  }), function(result, request_error)
    vim.schedule(function()
      if generation ~= request_generation then return end
      if not vim.api.nvim_buf_is_valid(buf)
          or not vim.api.nvim_win_is_valid(win)
          or vim.api.nvim_win_get_buf(win) ~= buf
          or not vim.deep_equal(vim.api.nvim_win_get_cursor(win), opening_cursor) then
        return
      end
      if request_error then
        notifications.error(request_error, "PlanReview Rust source")
        return
      end
      if type(result) ~= "table" then
        notifications.error("Harness returned no Rust source location", "PlanReview Rust source")
        return
      end
      open_source(result, win)
    end)
  end)
  return true
end

return M
