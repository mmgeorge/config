local display_text = require("diff_review.render.display_text")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")

local M = {}
local request_generation = 0

---@param popup_buf integer
---@param popup_win integer
---@param source_buf integer
---@param source_win integer
local function attach_close_lifecycle(popup_buf, popup_win, source_buf, source_win)
  local group = vim.api.nvim_create_augroup("DiffReviewPlanEntityInfo" .. tostring(popup_buf), { clear = true })
  local opening_cursor = vim.api.nvim_win_get_cursor(source_win)
  local function close_popup()
    request_generation = request_generation + 1
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
  vim.defer_fn(function()
    if not vim.api.nvim_buf_is_valid(popup_buf) or not vim.api.nvim_win_is_valid(popup_win) then return end
    if not vim.api.nvim_win_is_valid(source_win) or vim.api.nvim_win_get_buf(source_win) ~= source_buf then
      close_popup()
      return
    end
    local anchor = vim.api.nvim_win_get_cursor(source_win)
    if anchor[1] ~= opening_cursor[1] then
      close_popup()
      return
    end
    pcall(vim.api.nvim_create_autocmd, { "CursorMoved", "CursorMovedI" }, {
      group = group,
      buffer = source_buf,
      callback = function()
        local cursor = vim.api.nvim_win_is_valid(source_win) and vim.api.nvim_win_get_cursor(source_win) or nil
        if cursor and cursor[1] == anchor[1] and cursor[2] == anchor[2] then return end
        close_popup()
      end,
      desc = "Close PlanReview entity information after moving from its settled cursor position",
    })
  end, 50)
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

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@param notify_missing? boolean
---@return DiffReviewPlanEntity?
function M.entity_at_cursor(model, buf, win, notify_missing)
  local should_notify = notify_missing ~= false
  if not model then
    if should_notify then notifications.info("Plan entity information is unavailable", "PlanReview") end
    return nil
  end
  local cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, cursor[1] - 1, cursor[1], false)[1] or ""
  local entity = model:entity_at_position(line, cursor[2])
  if not entity and should_notify then notifications.info("Cursor is not on a plan entity", "PlanReview") end
  return entity
end

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@param options DiffReviewPlanDescriptionOptions
---@return boolean handled
function M.show(model, buf, win, options)
  if not model then return false end
  local cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, cursor[1] - 1, cursor[1], false)[1] or ""
  local item = model:described_item_at_position(options.source_line, line, cursor[2])
  if not item then return false end
  local description = vim.trim(tostring(item.description or ""))
  if description == "" then
    notifications.info("This plan item has no description", "PlanReview")
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

---@class DiffReviewPlanDescriptionOptions
---@field source_line integer

---@class DiffReviewRustdocInfoOptions: DiffReviewPlanDescriptionOptions
---@field plan_id string
---@field expected_version integer
---@field request fun(params: table, callback: fun(result: table?, request_error: string?))

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@param options DiffReviewRustdocInfoOptions
---@return boolean handled
function M.show_rustdoc(model, buf, win, options)
  if not model then return false end
  local opening_cursor = vim.api.nvim_win_get_cursor(win)
  local line = vim.api.nvim_buf_get_lines(buf, opening_cursor[1] - 1, opening_cursor[1], false)[1] or ""
  local target = model:rustdoc_target_at_position(options.source_line, line, opening_cursor[2])
  if not target then return false end
  request_generation = request_generation + 1
  local generation = request_generation
  local pending_group = vim.api.nvim_create_augroup(
    ("DiffReviewPlanRustdocPending%d_%d"):format(buf, generation),
    { clear = true }
  )
  local settled = false
  local presented = false
  local result_lines = nil
  local function cleanup_pending()
    pcall(vim.api.nvim_del_augroup_by_id, pending_group)
  end
  local function cancel_pending()
    if generation == request_generation then request_generation = request_generation + 1 end
    cleanup_pending()
  end
  local function present()
    if presented or not settled or not result_lines or generation ~= request_generation then return end
    presented = true
    cleanup_pending()
    local popup_buf, popup_win = popup_window.open({
      relative = "cursor",
      width = 40,
      height = math.max(1, math.min(#result_lines, 20)),
      enter = false,
      focusable = true,
      filetype = "markdown",
      title = "",
      border = "none",
    })
    vim.api.nvim_buf_set_lines(popup_buf, 0, -1, false, result_lines)
    vim.bo[popup_buf].modifiable = false
    vim.wo[popup_win].wrap = true
    attach_close_lifecycle(popup_buf, popup_win, buf, win)
  end
  vim.api.nvim_create_autocmd({ "BufHidden", "InsertCharPre" }, {
    group = pending_group,
    buffer = buf,
    once = true,
    callback = cancel_pending,
    desc = "Cancel pending PlanReview Rust documentation",
  })
  vim.defer_fn(function()
    if generation ~= request_generation then
      cleanup_pending()
      return
    end
    if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= buf then
      cancel_pending()
      return
    end
    local anchor = vim.api.nvim_win_get_cursor(win)
    if anchor[1] ~= opening_cursor[1] then
      cancel_pending()
      return
    end
    vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
      group = pending_group,
      buffer = buf,
      callback = function()
        local cursor = vim.api.nvim_win_is_valid(win) and vim.api.nvim_win_get_cursor(win) or nil
        if cursor and cursor[1] == anchor[1] and cursor[2] == anchor[2] then return end
        cancel_pending()
      end,
      desc = "Cancel pending PlanReview Rust documentation after cursor movement",
    })
    settled = true
    present()
  end, 50)
  options.request(vim.tbl_extend("force", target, {
    plan_id = options.plan_id,
    expected_version = options.expected_version,
  }), function(result, request_error)
    if generation ~= request_generation then
      cleanup_pending()
      return
    end
    if request_error then
      cancel_pending()
      notifications.error(request_error, "PlanReview Rustdoc")
      return
    end
    if type(result) ~= "table" or type(result.signature) ~= "string" then
      cancel_pending()
      notifications.error("Harness returned invalid Rust documentation", "PlanReview Rustdoc")
      return
    end
    result_lines = { "```rust", result.signature, "```" }
    if type(result.docs) == "string" and vim.trim(result.docs) ~= "" then
      result_lines[#result_lines + 1] = ""
      vim.list_extend(result_lines, vim.split(result.docs, "\n", { plain = true }))
    end
    present()
  end)
  return true
end

---@param model DiffReviewPlanTaskModel?
---@param buf integer
---@param win integer
---@param options DiffReviewRustdocInfoOptions
---@return boolean handled
function M.show_context(model, buf, win, options)
  if M.show(model, buf, win, options) then return true end
  if M.show_rustdoc(model, buf, win, options) then return true end
  notifications.info("Cursor is not on a plan entity or documented Rust symbol", "PlanReview")
  return false
end

return M
