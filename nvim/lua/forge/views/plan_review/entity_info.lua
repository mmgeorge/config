local display_text = require("forge.render.display_text")
local notifications = require("forge.infra.notifications")
local popup_window = require("forge.infra.popup_window")

local M = {}
local generation_request = 0

---@param popup_buf integer
---@param popup_win integer
---@param source_buf integer
---@param source_win integer
local function attach_close_lifecycle(popup_buf, popup_win, source_buf, source_win)
  local group = vim.api.nvim_create_augroup("ForgePlanEntityInfo" .. tostring(popup_buf), { clear = true })
  local opening_cursor = vim.api.nvim_win_get_cursor(source_win)
  local function close_popup()
    generation_request = generation_request + 1
    popup_window.close(popup_win, false)
    pcall(vim.api.nvim_del_augroup_by_id, group)
  end
  vim.api.nvim_create_autocmd({ "BufHidden", "InsertCharPre" }, {
    group = group,
    buffer = source_buf,
    once = true,
    callback = close_popup,
    desc = "Close PlanReview entity information after leaving its cursor position",
  })
  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
    group = group,
    buffer = source_buf,
    callback = function()
      local cursor = vim.api.nvim_win_is_valid(source_win) and vim.api.nvim_win_get_cursor(source_win) or nil
      if cursor and vim.deep_equal(cursor, opening_cursor) then return end
      close_popup()
    end,
    desc = "Close PlanReview entity information after moving from its opening cursor position",
  })
  for _, key in ipairs({ "q", "<Esc>" }) do
    vim.keymap.set("n", key, function()
      close_popup()
    end, {
      buffer = popup_buf,
      silent = true,
      nowait = true,
      desc = "Close PlanReview entity information",
    })
  end
end

---@param model ForgePlanTaskModel?
---@param buf integer
---@param win integer
---@param notify_missing? boolean
---@return ForgePlanEntity?
function M.entity_at_cursor(model, buf, win, notify_missing)
  local should_notify = notify_missing ~= false
  if not model then
    if should_notify then notifications.info("Plan entity information is unavailable", "ForgePlanReview") end
    return nil
  end
  local cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, cursor[1] - 1, cursor[1], false)[1] or ""
  local entity = model:entity_at_position(line, cursor[2])
  if not entity and should_notify then notifications.info("Cursor is not on a plan entity", "ForgePlanReview") end
  return entity
end

---@param model ForgePlanTaskModel?
---@param buf integer
---@param win integer
---@param options ForgePlanDescriptionOptions
---@return boolean handled
function M.show(model, buf, win, options)
  if not model then return false end
  local cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, cursor[1] - 1, cursor[1], false)[1] or ""
  local item = model:described_item_at_position(options.source_line, line, cursor[2])
  if not item then return false end
  local description = vim.trim(tostring(item.description or ""))
  if description == "" then
    notifications.info("This plan item has no description", "ForgePlanReview")
    return true
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
  attach_close_lifecycle(popup_buf, popup_win, buf, win)
  return true
end

---@class ForgePlanDescriptionOptions
---@field source_line integer

---@param model ForgePlanTaskModel?
---@param buf integer
---@param win integer
---@param options ForgePlanDescriptionOptions
---@return boolean handled
function M.show_context(model, buf, win, options)
  if M.show(model, buf, win, options) then return true end
  notifications.info("External references are unchecked during planning", "ForgePlanReview")
  return false
end

return M
