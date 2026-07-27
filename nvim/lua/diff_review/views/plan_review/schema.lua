local M = {}

local notifications = require("diff_review.infra.notifications")

---@param plan table
---@return string
local function canonical_plan_path(plan)
  return vim.fs.joinpath(vim.fs.dirname(plan.working_path), "working.json")
end

---@param review DiffReviewPlanReviewSession
---@param schema_buf integer
local function return_to_review(review, schema_buf)
  if not vim.api.nvim_buf_is_valid(review.buf) then
    notifications.error("The originating PlanReview buffer is no longer available", "PlanReviewSchema")
    return
  end
  local win = vim.api.nvim_get_current_win()
  if not vim.api.nvim_win_is_valid(win) then
    notifications.error("The PlanReviewSchema window is no longer available", "PlanReviewSchema")
    return
  end
  vim.api.nvim_win_set_buf(win, review.buf)
  if vim.api.nvim_buf_is_valid(schema_buf) then vim.api.nvim_buf_delete(schema_buf, { force = true }) end
end

---@param plan table
---@param review DiffReviewPlanReviewSession
function M.open(plan, review)
  local path = canonical_plan_path(plan)
  local read_ok, line_list = pcall(vim.fn.readfile, path)
  if not read_ok then
    notifications.error("Failed to read canonical plan JSON: " .. tostring(line_list), "PlanReviewSchema")
    return
  end
  if not (review.win and vim.api.nvim_win_is_valid(review.win)) then
    notifications.error("The PlanReview window is no longer available", "PlanReviewSchema")
    return
  end

  local buf = vim.api.nvim_create_buf(false, true)
  local name = "PlanReviewSchema://" .. tostring(plan.id)
  if not pcall(vim.api.nvim_buf_set_name, buf, name) then
    vim.api.nvim_buf_set_name(buf, name .. "#" .. tostring(buf))
  end
  vim.bo[buf].modifiable = true
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, line_list)
  vim.bo[buf].buftype = "nofile"
  vim.bo[buf].bufhidden = "wipe"
  vim.bo[buf].swapfile = false
  vim.bo[buf].filetype = "json"
  vim.bo[buf].modifiable = false
  vim.bo[buf].readonly = true
  vim.keymap.set("n", "q", function() return_to_review(review, buf) end, {
    buffer = buf,
    silent = true,
    nowait = true,
    desc = "Return to PlanReview",
  })

  vim.api.nvim_set_current_win(review.win)
  vim.api.nvim_win_set_buf(review.win, buf)
end

return M
