local M = {}

local client = require("diff_review.harness.client")
local command_set = require("diff_review.shared.view_command_set")
local keymaps = require("diff_review.shared.keymaps")
local notifications = require("diff_review.infra.notifications")
local popup_window = require("diff_review.infra.popup_window")
local session = require("diff_review.session")

local namespace = vim.api.nvim_create_namespace("DiffReviewPlanReview")

---@param plan table
---@return table[]
local function annotation_list(plan)
  local revision_key = plan.id .. ":" .. tostring(plan.model_revision or 0)
  session.harness.plan_annotations[revision_key] = session.harness.plan_annotations[revision_key] or {}
  return session.harness.plan_annotations[revision_key]
end

---@param buf integer
---@param annotation table
local function render_annotation(buf, annotation)
  local extmark = vim.api.nvim_buf_get_extmark_by_id(buf, namespace, annotation.mark, {})
  if #extmark == 0 then return end
  vim.api.nvim_buf_set_extmark(buf, namespace, extmark[1], 0, {
    id = annotation.mark,
    virt_lines = {
      { { "  ╭─ Plan comment " .. string.rep("─", 20), "DiffReviewWalkthroughItemTitle" } },
      { { "  │ " .. annotation.body, "DiffReviewWalkthroughComment" } },
      { { "  ╰" .. string.rep("─", 35), "FloatBorder" } },
    },
    virt_lines_above = false,
  })
end

---@param plan table
---@param buf integer
local function add_comment(plan, buf)
  local line = vim.api.nvim_win_get_cursor(0)[1] - 1
  popup_window.input({ prompt = "Plan comment: " }, function(body)
    if not body or vim.trim(body) == "" or not vim.api.nvim_buf_is_valid(buf) then return end
    local mark = vim.api.nvim_buf_set_extmark(buf, namespace, line, 0, {
      right_gravity = false,
    })
    local annotation = { mark = mark, body = vim.trim(body) }
    annotation_list(plan)[#annotation_list(plan) + 1] = annotation
    render_annotation(buf, annotation)
  end)
end

---@param plan table
---@param buf integer
---@return table[]
local function serialized_annotation(plan, buf)
  local result = {}
  for _, annotation in ipairs(annotation_list(plan)) do
    local position = vim.api.nvim_buf_get_extmark_by_id(buf, namespace, annotation.mark, {})
    if #position > 0 then result[#result + 1] = { line = position[1] + 1, body = annotation.body } end
  end
  return result
end

---@param plan table
local function open_source(plan)
  local index_path = vim.fs.joinpath(vim.fs.dirname(plan.working_path), "working.index.json")
  local ok, source = pcall(vim.fn.readfile, index_path)
  if not ok then
    notifications.error("Failed to read plan navigation index", "PlanReview")
    return
  end
  local decoded_ok, index = pcall(vim.json.decode, table.concat(source, "\n"))
  if not decoded_ok then
    notifications.error("Failed to decode plan navigation index", "PlanReview")
    return
  end
  local line = vim.api.nvim_win_get_cursor(0)[1]
  local candidate = nil
  for _, target in ipairs(index.target or {}) do
    if target.path and line >= target.first_line and line <= target.last_line then
      if not candidate or (target.last_line - target.first_line) < (candidate.last_line - candidate.first_line) then
        candidate = target
      end
    end
  end
  if not candidate then
    notifications.info("This plan line has no source boundary", "PlanReview")
    return
  end
  vim.cmd("edit " .. vim.fn.fnameescape(vim.fs.joinpath(vim.fn.getcwd(), candidate.path)))
end

---@param buf integer
---@param locked boolean
local function set_locked(buf, locked)
  if vim.api.nvim_buf_is_valid(buf) then vim.bo[buf].modifiable = not locked end
end

---@param tab integer
local function close_tab(tab)
  if not vim.api.nvim_tabpage_is_valid(tab) then return end
  if vim.fn.tabpagenr("$") > 1 then
    vim.api.nvim_set_current_tabpage(tab)
    vim.cmd("tabclose")
  elseif vim.api.nvim_get_current_tabpage() == tab then
    vim.cmd("enew")
  end
end

---@param plan table
---@param buf integer
local function accept_with_context(plan, buf, tab, fresh_context)
  set_locked(buf, true)
  local previous_goal = session.harness.goal
  session.harness.busy = true
  session.harness.goal = { objective = "Complete the plan", state = "active" }
  require("diff_review.views.harness.controller").refresh_winbar()
  client.request("plan.accept", {
    plan_id = plan.id,
    digest = plan.review_digest,
    fresh_context = fresh_context,
  }, function(result, request_error)
    session.harness.busy = false
    if request_error then
      session.harness.goal = previous_goal
      set_locked(buf, true)
      notifications.error(request_error, "PlanReview")
      require("diff_review.views.harness.controller").refresh_winbar()
      return
    end
    if result then
      session.harness.session = result.session or session.harness.session
      session.harness.capability = result.capability or session.harness.capability
    end
    notifications.info("Plan accepted. Goal: Complete the plan", "Harness")
    require("diff_review.views.harness.controller").render()
    close_tab(tab)
  end)
end

---@param plan table
---@param buf integer
local function accept(plan, buf, tab)
  vim.ui.select({
    { label = "Continue context", fresh_context = false },
    { label = "Fresh context", fresh_context = true },
  }, {
    prompt = "Approve plan",
    format_item = function(item) return item.label end,
  }, function(choice)
    if choice then accept_with_context(plan, buf, tab, choice.fresh_context) end
  end)
end

---@param plan table
---@param buf integer
local function request_changes(plan, buf)
  local annotation = serialized_annotation(plan, buf)
  popup_window.input({ prompt = "Overall plan review comment (optional): " }, function(comment)
    if comment == nil then return end
    set_locked(buf, true)
    session.harness.busy = true
    require("diff_review.views.harness.controller").refresh_winbar()
    client.request("plan.request_changes", {
      plan_id = plan.id,
      annotations = annotation,
      comment = vim.trim(comment),
    }, function(_, request_error)
      session.harness.busy = false
      if request_error then
        notifications.error(request_error, "PlanReview")
        require("diff_review.views.harness.controller").refresh_winbar()
        return
      end
      if vim.api.nvim_buf_is_valid(buf) then
        vim.api.nvim_buf_call(buf, function() vim.cmd("silent edit!") end)
        vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
      end
      local revision_key = plan.id .. ":" .. tostring(plan.model_revision or 0)
      session.harness.plan_annotations[revision_key] = nil
      notifications.info("Plan revision requested", "PlanReview")
      require("diff_review.views.harness.controller").render()
    end)
  end)
end

---@param plan table
---@param buf integer
---@return DiffReviewViewCommandSet
local function commands(plan, buf, tab)
  local set = command_set.new()
  command_set.register(set, "open", function() open_source(plan) end)
  command_set.register(set, "comment", function() add_comment(plan, buf) end)
  command_set.register(set, "accept", function() accept(plan, buf, tab) end)
  command_set.register(set, "request_changes", function() request_changes(plan, buf) end)
  command_set.register(set, "close", function() close_tab(tab) end)
  command_set.register(set, "help", function() keymaps.show_view_help("plan_review", set, "PlanReview") end)
  return set
end

---@param plan table
function M.open(plan)
  assert(type(plan) == "table" and type(plan.working_path) == "string", "PlanReview requires a physical plan path")
  local existing_buf = vim.fn.bufnr(plan.working_path)
  local existing_win = existing_buf >= 0 and vim.fn.win_findbuf(existing_buf)[1] or nil
  if existing_win and vim.api.nvim_win_is_valid(existing_win) then
    vim.api.nvim_set_current_tabpage(vim.api.nvim_win_get_tabpage(existing_win))
    vim.api.nvim_set_current_win(existing_win)
    vim.cmd("silent edit! " .. vim.fn.fnameescape(plan.working_path))
  else
    vim.cmd("tabnew")
    vim.cmd("edit " .. vim.fn.fnameescape(plan.working_path))
  end
  local buf = vim.api.nvim_get_current_buf()
  local win = vim.api.nvim_get_current_win()
  local tab = vim.api.nvim_get_current_tabpage()
  vim.bo[buf].filetype = "markdown"
  vim.bo[buf].bufhidden = "hide"
  vim.bo[buf].swapfile = false
  vim.bo[buf].modifiable = false
  vim.api.nvim_buf_clear_namespace(buf, namespace, 0, -1)
  session.harness.plan_review = { plan = plan, buf = buf, win = win, tab = tab }
  local set = commands(plan, buf, tab)
  keymaps.setup_view_keymaps(buf, "plan_review", set)
  keymaps.apply_view_winbar(win, "PlanReview", "plan_review", set, "Awaiting review • read-only projection • C adds comments")
  for _, annotation in ipairs(annotation_list(plan)) do render_annotation(buf, annotation) end
end

return M
